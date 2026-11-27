;;; nvemacs.el --- Notational Velocity style note search  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 T. Charles Yun

;; Author: T. Charles Yun
;; Assisted-by: LLM/Claude Code (2026)
;; Version: 0.0.6
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, text, notes, Notational Velocity
;; URL: https://github.com/tcharlesyun/nvemacs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Search-as-you-type over folders of plain-text notes (Org, Markdown,
;; text), in the manner of Notational Velocity, backed by a SQLite FTS5
;; trigram index.
;;
;; Start with M-x nvemacs.  The first start asks which folders hold your
;; notes and keeps them in settings.eld, in `nvemacs-data-directory';
;; M-x nvemacs-index-folder-add and nvemacs-index-folder-remove change
;; them later.  A `nvemacs-watched-directories' set in the init file is
;; used instead, and those two commands then say to change it there.
;; nvEmacs never writes the init file.
;;
;; README.txt (install and start), USAGE.txt (searching), COMMANDS.txt
;; and SETTINGS.txt at https://github.com/tcharlesyun/nvemacs describe
;; the rest.

;;; Code:

(require 'cl-lib)
(require 'ucs-normalize)
(require 'time-date)

;;;; Folding
;;
;; Indexed text and queries pass through the same fold:
;;   1. NFC — only when the text holds decomposed Hangul jamo or
;;      compatibility Hanja, where it changes the result (for Latin accents,
;;      step 3 already gives the NFC-then-strip result: e + U+0301 -> e);
;;   2. case fold with Emacs `downcase' (ß stays ß);
;;   3. accents removed and full-width ASCII mapped to ASCII, by a
;;      translation table built once (both done in C by `translate-region');
;;   4. any combining mark left over is deleted.
;; The folded text lives only in the index; note files are never modified.

(defconst nvemacs--combining-mark-regexp "\\c^"
  "Non-spacing combining marks (Unicode Mn), deleted by the fold.
Emacs's `^' character category matches Mn and not Mc or Hangul (checked
2026-11-18).  A hand-written list of mark blocks missed Hebrew points.")

(defconst nvemacs--needs-nfc-regexp
  "[\u1100-\u11FF\uA960-\uA97F\uD7B0-\uD7FF\uF900-\uFAFF]"
  "Text containing these (conjoining Hangul jamo, compatibility Hanja)
must be NFC-normalised before folding.")

(defconst nvemacs--fold-ranges
  '((#x00C0 . #x024F)    ; Latin-1 Supplement, Latin Extended-A/B
    (#x0370 . #x03FF)    ; Greek
    (#x0400 . #x04FF)    ; Cyrillic
    (#x1E00 . #x1FFF))   ; Latin Extended Additional, Greek Extended
  "Ranges whose precomposed letters are mapped to their base letter.")

(defvar nvemacs--fold-table nil
  "Translation table: accented letter -> lowercase base letter;
full-width ASCII -> ASCII.  Built on first use.")

(defun nvemacs--build-fold-table ()
  "Build `nvemacs--fold-table'."
  (let ((tbl (make-char-table 'translation-table)))
    (dolist (range nvemacs--fold-ranges)
      (cl-loop for c from (car range) to (cdr range) do
               (let* ((d (ucs-normalize-NFD-string (string c)))
                      (base (cl-remove-if
                             (lambda (ch)
                               (eq (get-char-code-property ch 'general-category) 'Mn))
                             d)))
                 (when (and (= (length base) 1) (/= (aref base 0) c))
                   (aset tbl c (downcase (aref base 0)))))))
    (cl-loop for c from #xFF01 to #xFF5E do
             (aset tbl c (downcase (- c #xFEE0))))
    tbl))

(defun nvemacs--fold-buffer ()
  "Fold the whole current buffer in place (see the Folding section)."
  (unless nvemacs--fold-table
    (setq nvemacs--fold-table (nvemacs--build-fold-table)))
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (when (re-search-forward nvemacs--needs-nfc-regexp nil t)
      (ucs-normalize-NFC-region (point-min) (point-max)))
    (downcase-region (point-min) (point-max))
    (translate-region (point-min) (point-max) nvemacs--fold-table)
    (goto-char (point-min))
    (while (re-search-forward nvemacs--combining-mark-regexp nil t)
      (replace-match "" t t))))

(defun nvemacs--collapse-whitespace ()
  "Make every whitespace run in the buffer one space.
The two C primitives keep this cheap on a full rebuild."
  (dolist (c '(?\n ?\t ?\r ?\f))
    (subst-char-in-region (point-min) (point-max) c ?\s t))
  (goto-char (point-min))
  (while (search-forward "  " nil t)
    (let ((second (point)))
      (skip-chars-forward " ")
      (delete-region (1- second) (point)))))

(defun nvemacs--fold (string)
  "Return STRING folded for indexing or querying."
  (with-temp-buffer
    (insert string)
    (nvemacs--fold-buffer)
    (buffer-string)))

;;;; Settings

(defgroup nvemacs nil
  "Notational Velocity style note search."
  :group 'applications)

(defcustom nvemacs-watched-directories nil
  "Folders whose notes are indexed, with their subfolders.
Leave this unset: nvEmacs asks for the folders at its first start and
keeps them in settings.eld, and \[nvemacs-index-folder-add] and
\[nvemacs-index-folder-remove] change them.  Set here, or with Customize,
this list wins, and those commands say to change it here."
  :type '(repeat directory)
  :package-version '(nvemacs . "0.0.5"))

(defcustom nvemacs-watched-directories-recursive t
  "Non-nil means descend into subdirectories of watched directories."
  :type 'boolean
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-ignore-dir-regexp "\\`\\."
  "Subdirectories whose name matches this are skipped (default: hidden)."
  :type 'regexp
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-import-extensions '("org" "md" "txt")
  "Which files are notes: only these extensions are read and indexed."
  :type '(repeat string)
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-import-title-source 'auto
  "Where a note's title comes from.
`auto': title in the file, else filename, else first line.
`filename': always the filename.  `first-line': always the first line."
  :type '(choice (const auto) (const filename) (const first-line))
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-title-match-width 60
  "Display columns of a title that count as a TITLE match.
It exists to limit title matches: a paragraph-long \"title\" would make
most of its words title matches, which rank first. The title is cut at a
word boundary within this width (Notational Velocity cuts at 60 the same
way); the rest still matches as body text, and the notelist shows the cut
part in bold. 0 = the whole title. Changing it rebuilds only the title
tables, from the titles stored in the index (no note is read)."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-import-max-size (* 10 1024 1024)
  "Notes larger than this many bytes are skipped."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-filename-timestamp-regexp
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}h[0-9]\\{2\\}m-"
  "Leading timestamp removed from filenames used as titles."
  :type 'regexp
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-data-directory (locate-user-emacs-file "nvemacs/")
  "Directory where nvEmacs keeps the files it writes.
These are the index, the instrumentation logs, state.eld (remembered
values, such as the notelist tags width) and dates.eld (the created
dates of undated notes).  Your preferences are not here: they live in
your init file or `custom-file'."
  :type 'directory
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-index-file nil
  "The index database; nil = index.sqlite in `nvemacs-data-directory'."
  :type '(choice (const :tag "In the data directory" nil) file)
  :package-version '(nvemacs . "0.0.2"))

(defun nvemacs--index-path ()
  "Return the path of the index file.
That is `nvemacs-index-file', or index.sqlite in `nvemacs-data-directory'."
  (or nvemacs-index-file (expand-file-name "index.sqlite" nvemacs-data-directory)))

;;;; Import: title and indexed text

(defconst nvemacs--cjk-regexp
  "[\u1100-\u11FF\u3040-\u30FF\u3130-\u318F\u31F0-\u31FF\u3400-\u4DBF\u4E00-\u9FFF\uAC00-\uD7A3\uF900-\uFAFF\uFF66-\uFF9F\U00020000-\U0002FFFF]"
  "Han, Hangul and kana: scripts with common 1–2 character words.
A note or query containing one is routed to the _other tables.")

(defconst nvemacs--org-heading-regexp
  "^\\*+[ \t]+\\(.*?\\)\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"
  "An Org heading; group 1 is its text without stars and tags.
Per-note code shares regexp strings where it can: Emacs caches only ~20
compiled regexps, and import code using more distinct ones per note made
every `insert-file-contents' recompile its own (rebuild 3.7 s -> 5 s,
measured 2026-11-19).")

(defconst nvemacs--title-store-max 300
  "Characters of a title kept in the index for display.")

(defconst nvemacs--preview-max 300
  "Characters of body preview kept in the index per note.
This is more than any window shows.")

(defun nvemacs--store-title (title)
  "Return TITLE trimmed and cut to `nvemacs--title-store-max' characters."
  (let ((s (string-trim title)))
    (if (> (length s) nvemacs--title-store-max) (substring s 0 nvemacs--title-store-max) s)))

(defun nvemacs--match-title (title)
  "Split TITLE into (MATCH . REST).
MATCH is TITLE cut to `nvemacs-title-match-width' display columns at a
word boundary (the whole title when that width is 0 or the title fits);
REST is what follows, trimmed.  MATCH is what counts as a title match
and what the notelist shows in bold."
  (let ((w nvemacs-title-match-width))
    (if (or (<= w 0) (<= (length title) (/ w 2)) (<= (string-width title) w))
        (cons title "")
      (let* ((cut (truncate-string-to-width (substring title 0 (min w (length title))) w))
             ;; A cut that already ends a word (a space follows) is kept.
             (at-word (memq (aref title (length cut)) '(?\s ?\t)))
             (sp (and (not at-word) (string-match-p "[ \t][^ \t]*\\'" cut)))
             (match (string-trim-right (if (and sp (> sp 0)) (substring cut 0 sp) cut))))
        (cons match (string-trim (substring title (length match))))))))

(defconst nvemacs--date-regexp
  (concat "\\`[ \t\"'<[]*\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
          "\\(?:\\(?:[T-]\\|[ \t]+\\(?:[[:alpha:]]+\\.?[ \t]+\\)?\\)"
          "\\([0-9]\\{1,2\\}\\)[:h]\\([0-9]\\{2\\}\\)"
          "\\(?::\\([0-9]\\{2\\}\\)\\)?"
          "\\(Z\\|[-+][0-9]\\{2\\}\\(?::?[0-9]\\{2\\}\\)?\\)?\\)?")
  "A date, an optional time and an optional offset, at the start of a string.
Groups: 1 year, 2 month, 3 day, 4 hour, 5 minute, 6 second, 7 offset.")

(defun nvemacs--zone-seconds (zone)
  "Return ZONE, `Z', `+13:00' or `-0800', in seconds east of UTC, or nil."
  (if (equal zone "Z")
      0
    (let* ((digits (string-replace ":" "" (substring zone 1)))
           (hh (string-to-number (substring digits 0 2)))
           (mm (if (> (length digits) 2) (string-to-number (substring digits 2)) 0)))
      (when (and (<= hh 14) (<= mm 59))
        (* (if (eq (aref zone 0) ?-) -1 1) (+ (* hh 3600) (* mm 60)))))))

(defun nvemacs--date-parts (s)
  "Return the date at the start of S as (WALL . OFFSET), or nil.
WALL is the clock time as written, in seconds as if it were UTC, so it
reads the same in every time zone.  OFFSET is the zone the text gives,
in seconds east of UTC, or nil for local time.  Accepts
2026-11-23-10:30:45, 2026-11-19-19h40m, 2024-05-01, 2024-05-01 10:30,
2024-05-01T10:30:45-08:00, Org timestamps <2024-05-01 Wed 10:30> and
\[2024-05-01 Wed], and quotes around any of them.  An Org timestamp has
no offset: its `10:30-11:30' is a time range."
  (when s
    (let ((case-fold-search nil))
      (when (string-match nvemacs--date-regexp s)
        (let* ((num (lambda (g) (if (match-beginning g) (string-to-number (match-string g s)) 0)))
               (y (funcall num 1)) (mo (funcall num 2)) (d (funcall num 3))
               (h (funcall num 4)) (mi (funcall num 5)) (sec (funcall num 6))
               (org (and (> (match-beginning 1) 0)
                         (memq (aref s (1- (match-beginning 1))) '(?< ?\[))))
               (end (match-end 7))
               (zone (and end (not org)
                          (or (= end (length s))
                              (not (string-match-p "[[:alnum:]]" (substring s end (1+ end)))))
                          (match-string 7 s))))
          (when (and (<= 1 mo 12) (<= 1 d (date-days-in-month y mo))
                     (<= 0 h 23) (<= 0 mi 59) (<= 0 sec 59))
            (cons (time-convert (encode-time (list sec mi h d mo y nil nil 0)) 'integer)
                  (and zone (nvemacs--zone-seconds zone)))))))))

(defun nvemacs--date-moment (parts)
  "Return PARTS, (WALL . OFFSET), as seconds since 1970.
A date with no offset is read in the current time zone."
  (if (cdr parts)
      (- (car parts) (cdr parts))
    (pcase-let ((`(,s ,mi ,h ,d ,mo ,y) (decode-time (car parts) 0)))
      (time-convert (encode-time (list s mi h d mo y nil -1 nil)) 'integer))))

(defun nvemacs--parse-date (s)
  "Seconds since 1970 of the date at the start of S, or nil.
See `nvemacs--date-parts' for the forms it reads."
  (let ((parts (nvemacs--date-parts s)))
    (and parts (nvemacs--date-moment parts))))

(defun nvemacs--filename-date-parts (path)
  "Return the date at the start of PATH's filename as (WALL . OFFSET), or nil.
The filename must start with a timestamp such as 2024-05-01 or
2024-05-01-10h00m."
  (let ((base (file-name-nondirectory path)))
    (and (> (length base) 9) (<= ?0 (aref base 0) ?9)   ; no regexp for most names
         (string-match-p (concat "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                                 "\\(?:-[0-9]\\{2\\}h[0-9]\\{2\\}m\\)?\\(?:[-_ .]\\|\\'\\)")
                         base)
         (nvemacs--date-parts base))))

(defun nvemacs--filename-date (path)
  "Return the created date, in seconds, from the start of PATH's filename."
  (let ((parts (nvemacs--filename-date-parts path)))
    (and parts (nvemacs--date-moment parts))))

(defun nvemacs--norm-line (s)
  "S without heading markers, case, punctuation, `_' or extra spaces."
  (string-trim (downcase (replace-regexp-in-string
                          "[_[:space:][:punct:]]+" " "
                          (replace-regexp-in-string "\\`[ \t]*\\(?:\\*+\\|#+\\)[ \t]+" "" s)))))

(defun nvemacs--delete-line ()
  "Delete the current line and its newline."
  (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))

(defun nvemacs--same-line-p (line title)
  "Return non-nil if LINE says the same as TITLE.
They are the same if they are equal ignoring case, or else (only when
their lengths are close) equal ignoring heading markers, punctuation,
`_' and spaces.  The cheap test first keeps regexps out of the common
case."
  (let ((line (string-trim line)))
    (or (string-equal-ignore-case line title)
        (and (< (abs (- (length line) (length title))) 12)
             (equal (nvemacs--norm-line line) (nvemacs--norm-line title))))))

(defun nvemacs--preview (ext source title)
  "Return the cleaned note text in the current buffer as one preview line.
The line the title came from (SOURCE) is removed, and so is a first
line that repeats TITLE (whole line only).  Org heading stars and tags,
or Markdown heading markers, are dropped, as EXT (\"org\" or \"md\")
says; whitespace is collapsed, and the result is cut to
`nvemacs--preview-max'.  The buffer is not changed.  Uses few distinct
regexps: see `nvemacs--org-heading-regexp'."
  ;; 2 KB is plenty for 300 characters of preview.
  (let ((text (buffer-substring-no-properties (point-min) (min (point-max) (+ (point-min) 2048))))
        (org (equal ext "org")) (md (equal ext "md")) (case-fold-search t))
    ;; One reused work buffer: a new temp buffer per note cost more.
    (with-current-buffer (get-buffer-create " *nvemacs-preview*")
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (pcase source
        ('heading (when (re-search-forward (if org "^\\*+[ \t]" "^#[ \t]") nil t) (nvemacs--delete-line)))
        ('first-line (skip-chars-forward " \t\n") (nvemacs--delete-line)))
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (when (and (not (eobp)) (not (string-empty-p title))
                 (nvemacs--same-line-p (buffer-substring (point) (line-end-position)) title))
        (nvemacs--delete-line))
      (goto-char (point-min))
      (cond (org (while (re-search-forward nvemacs--org-heading-regexp nil t) ; stars and tags
                   (replace-match "\\1" t)))
            (md (while (re-search-forward "^#+[ \t]+" nil t) (replace-match ""))))
      ;; Collapse whitespace runs to one space, up to the preview length.
      (goto-char (point-min))
      (skip-chars-forward " \t\n\r\f")
      (delete-region (point-min) (point))
      (while (and (< (point) (+ (point-min) nvemacs--preview-max)) (not (eobp)))
        (skip-chars-forward "^ \t\n\r\f")
        (let ((p (point)))
          (skip-chars-forward " \t\n\r\f")
          (when (> (point) p)
            (delete-region p (point))
            (insert " "))))
      (string-trim-right (buffer-substring-no-properties
                          (point-min) (min (point-max) (+ (point-min) nvemacs--preview-max)))))))

(defun nvemacs--filename-base (path)
  "Return PATH's filename without directory, extension and timestamp."
  (replace-regexp-in-string nvemacs-filename-timestamp-regexp ""
                            (file-name-sans-extension (file-name-nondirectory path))))

(defun nvemacs--filename-title (path)
  "Return the title from PATH's filename.
Return nil if the filename is only a date stamp: nothing is left after
the timestamp, or only digits and separators (`2024-05-01',
`2024-05-01-10h00m')."
  (let ((base (string-trim (replace-regexp-in-string "_+" " " (nvemacs--filename-base path)))))
    (unless (string-match-p "\\`[0-9hmHM .:_-]*\\'" base) base)))

(defun nvemacs--filename-spelling (path)
  "Return PATH's filename as it is spelled, when it has a `_'.
A title made from the filename shows each `_' as a space, so a search
for the filename as typed (`covington_drive' for
1827_Covington_Drive.org) finds the note only if this spelling is
indexed too.  It is the filename without directory, extension and
timestamp, with no `_' at either end; nil when it has no `_' or is
only a date stamp."
  (when (nvemacs--filename-title path)
    (let ((s (string-trim (nvemacs--filename-base path) "[_ \t]+" "[_ \t]+")))
      (and (string-search "_" s) s))))

(defun nvemacs--org-heading ()
  "Text of the first Org heading, without stars and tags."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward nvemacs--org-heading-regexp nil t)
      (let ((h (match-string-no-properties 1)))
        (unless (string-empty-p h) h)))))

(defun nvemacs--md-heading ()
  "First level-1 ATX heading outside code fences, without a closing #."
  (save-excursion
    (goto-char (point-min))
    (let ((fence nil) (found nil))
      (while (and (not found) (not (eobp)))
        (cond ((looking-at "[ \t]*\\(```\\|~~~\\)") (setq fence (not fence)))
              ((and (not fence)
                    (looking-at "#[ \t]+\\(.*?\\)\\(?:[ \t]+#+\\)?[ \t]*$"))
               (setq found (match-string-no-properties 1))))
        (forward-line 1))
      (and found (not (string-empty-p found)) found))))

;;;; Tags: reading
;;
;; A tag is (TAG . KIND), KIND `file' or `section'. File tags live in the
;; note for Org (`#+filetags:') and Markdown (front matter `tags:'), and
;; in a sister file NAME.EXT.tags beside any other note. Section tags are
;; Org heading tags. Every form is read; writing keeps the form found.

(defcustom nvemacs-tags-in-file-extensions '("org" "md")
  "Extensions of notes whose file tags are written inside the note.
Any other note keeps them in a sister file beside it: foo.txt ->
foo.txt.tags."
  :type '(repeat string)
  :package-version '(nvemacs . "0.0.2"))

(defconst nvemacs--tag-chars "[:alnum:]_@#%"
  "Characters an Org tag may contain (no `-', no space).")

(defun nvemacs--underscore (string keep)
  "Return STRING NFC-normalised, with other characters replaced by `_'.
Every run of characters outside KEEP, the body of a regexp bracket
expression, becomes one `_'.  Shared by filenames and tags."
  (replace-regexp-in-string (concat "[^" keep "]+") "_" (ucs-normalize-NFC-string string)))

(defun nvemacs--tag-normalize (tag)
  "Return TAG reduced to Org's tag characters, or nil if none are left.
Its case is kept, any other character becomes `_', and `_' is trimmed
from both ends."
  (let ((s (replace-regexp-in-string "\\`_+\\|_+\\'" ""
                                     (nvemacs--underscore (or tag "") nvemacs--tag-chars))))
    (unless (string-empty-p s) s)))

(defun nvemacs--tags-clean (pairs)
  "Return PAIRS, ((TAG . KIND) ...), normalised and without duplicates.
Empty tags are dropped, and each (tag, kind) is kept once, compared
ignoring case; the first spelling wins."
  (let ((seen nil) (out nil))
    (dolist (p pairs)
      (let ((tag (nvemacs--tag-normalize (car p))))
        (when tag
          (let ((key (cons (nvemacs--fold tag) (cdr p))))
            (unless (member key seen)
              (push key seen)
              (push (cons tag (cdr p)) out))))))
    (nreverse out)))

(defun nvemacs--tags-in-file-p (path)
  "Return non-nil if note PATH keeps its file tags inside the note.
See `nvemacs-tags-in-file-extensions'."
  (member (downcase (or (file-name-extension path) "")) nvemacs-tags-in-file-extensions))

(defun nvemacs--sister (path)
  "Return the sister tags file of note PATH: PATH with \".tags\" added."
  (concat path ".tags"))

(defun nvemacs--sister-mtime (path)
  "Rowid-unit mtime of PATH's sister tag file; 0 if none or not used."
  (let ((a (and (not (nvemacs--tags-in-file-p path))
                (file-attributes (nvemacs--sister path)))))
    (if a (nvemacs--mtime-us a) 0)))

(defconst nvemacs--org-block-regexp
  "[ \t]*#\\+begin_\\(src\\|example\\|export\\|comment\\)\\(?:[ \t]\\|$\\)"
  "The first line of an Org block whose lines are not keywords or comments.
Group 1 is the block's type.  Use it with `case-fold-search' on.")

(defun nvemacs--org-drawer-end ()
  "With point on a `:PROPERTIES:' line, return the start of the line after `:END:'.
Return nil if the drawer is not closed."
  (save-excursion
    (forward-line 1)
    (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
      (min (point-max) (1+ (point))))))

(defun nvemacs--org-header-end ()
  "Return where the header of the Org text in the current buffer ends.
The header is the block of lines up to the first blank line, which may
hold only spaces.  Blank lines before it are skipped, a `:PROPERTIES:'
drawer counts as one unit, and a heading or a code block also ends it.
Title, Created, Date and file tags count only here."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\r\n")
    (beginning-of-line)
    (let ((case-fold-search t) (done nil))
      (while (and (not done) (not (eobp)))
        (cond
         ((or (looking-at "[ \t\r]*$") (looking-at "\\*+[ \t]")
              (looking-at nvemacs--org-block-regexp))
          (setq done t))
         ((looking-at "[ \t]*:PROPERTIES:[ \t]*$")
          (goto-char (or (nvemacs--org-drawer-end) (line-beginning-position 2))))
         (t (forward-line 1))))
      (point))))

(defun nvemacs--org-tags-in-buffer ()
  "Return the tags of the Org text in the current buffer.
These are the `#+filetags:' lines of the header (any case; several
combine), as (TAG . file), and the heading tags, as
\(TAG . section).  A commented-out `# +filetags:' does not count."
  (save-excursion
    (let ((case-fold-search t) (limit (nvemacs--org-header-end)) (out nil))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+filetags:[ \t]*\\(.*\\)$" limit t)
        (dolist (tg (split-string (match-string-no-properties 1) "[: \t]+" t))
          (push (cons tg 'file) out)))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+[ \t]+.*?[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" nil t)
        (dolist (tg (split-string (match-string-no-properties 1) ":" t))
          (push (cons tg 'section) out)))
      (nreverse out))))

(defun nvemacs--md-front-matter ()
  "Front matter at the start of the buffer as (TYPE BEG END IBEG IEND):
TYPE `yaml' (---), `toml' (+++) or `json' ({ on its own line); BEG..END
the whole block up to the end of its closing line; IBEG..IEND its inside
\(IEND = start of closing line)."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "{[ \t]*\n")
        (let* ((ibeg (match-end 0))
               (end (ignore-errors (json-parse-buffer) (point))))
          (when end (list 'json (point-min) end ibeg (1- end))))
      (let ((type (cond ((looking-at "---[ \t]*\n") 'yaml)
                        ((looking-at "\\+\\+\\+[ \t]*\n") 'toml))))
        (when type
          (let ((ibeg (match-end 0)))
            (goto-char ibeg)
            (when (re-search-forward (if (eq type 'yaml) "^---[ \t]*$" "^\\+\\+\\+[ \t]*$") nil t)
              (list type (point-min) (match-end 0) ibeg (match-beginning 0)))))))))

(defun nvemacs--md-unquote (s)
  "Return S trimmed and without one pair of matching outer quotes."
  (let ((s (string-trim s)))
    (if (string-match "\\`\\([\"']\\)\\(.*\\)\\1\\'" s) (match-string 2 s) s)))

(defun nvemacs--md-list-items (s)
  "Items of S, the inside of a flow list: `a, \"b c\"' -> (\"a\" \"b c\")."
  (delete "" (mapcar #'nvemacs--md-unquote (split-string s ","))))

(defun nvemacs--md-tags-entry (fm)
  "Return the `tags' entry of front matter FM, or nil.
FM is as `nvemacs--md-front-matter' returns it.  The entry is a plist:
:tags (strings), :form (`flow', `block', `comma', `space', `toml' or `json'),
:beg and :end (the text to replace) and :indent (block form)."
  (save-excursion
    (let ((case-fold-search t) (iend (nth 4 fm)))
      (goto-char (nth 3 fm))
      (if (memq (car fm) '(toml json))
          (when (re-search-forward (if (eq (car fm) 'toml)
                                       "^tags[ \t]*=[ \t]*\\[\\(.*\\)\\][ \t]*$"
                                     "\"tags\"[ \t\n]*:[ \t\n]*\\[\\([^]]*\\)\\]")
                                   iend t)
            ;; Positions first: `split-string' in the items call resets match data.
            (let ((beg (match-beginning 0)) (end (match-end 0)))
              (list :tags (nvemacs--md-list-items (match-string-no-properties 1)) :form (car fm)
                    :beg beg :end end)))
        (when (re-search-forward "^tags:[ \t]*\\(.*?\\)[ \t]*$" iend t)
          (let ((v (match-string-no-properties 1))
                (beg (match-beginning 0))
                (end (match-end 0)))
            (cond
             ((string-prefix-p "[" v)
              (list :tags (nvemacs--md-list-items (replace-regexp-in-string "\\`\\[\\|\\]\\'" "" v))
                    :form 'flow :beg beg :end end))
             ((string-empty-p v)
              (let ((items nil) (indent nil))
                (forward-line 1)
                (while (and (< (point) iend) (looking-at "\\([ \t]*\\)-[ \t]+\\(.*?\\)[ \t]*$"))
                  (setq indent (or indent (match-string-no-properties 1)))
                  (push (nvemacs--md-unquote (match-string-no-properties 2)) items)
                  (setq end (match-end 0))
                  (forward-line 1))
                (list :tags (nreverse items) :form 'block :beg beg :end end
                      :indent (or indent "  "))))
             (t
              (let* ((s (nvemacs--md-unquote v)) (comma (string-match-p "," s)))
                (list :tags (split-string s (if comma "[ \t]*,[ \t]*" "[ \t]+") t)
                      :form (if comma 'comma 'space) :beg beg :end end))))))))))

(defun nvemacs--md-tags-in-buffer ()
  "Return the front-matter tags of the Markdown text in this buffer.
Each tag is (TAG . file)."
  (let ((fm (nvemacs--md-front-matter)))
    (mapcar (lambda (tg) (cons tg 'file))
            (plist-get (and fm (nvemacs--md-tags-entry fm)) :tags))))

;;;; Note fields: keys, values and header comments
;;
;; Every key in a note is captured, whatever its name.  A field is
;; (KEY VALUE KIND POS): KIND is `header', `body', or `comment' for a header
;; comment, which has no KEY; POS is where it is in the file.  Plain search
;; never sees them; `key:' reads the names and `meta:' the values and header
;; comments.  The design is in the project's note_fields notes.

(defcustom nvemacs-search-comments nil
  "Non-nil means comments in a note's body are part of plain search.
Comments follow the file type: in Org a `#' line and a comment block, in
Markdown `<!-- -->'; text files have none.  The comments of an Org header
stay out either way.  \[nvemacs-index-comments] turns this on or off,
rebuilds the index and remembers the choice, which then wins over this
value.  Set here, a change takes effect when the index is rebuilt, which
nvEmacs offers at its next start."
  :type 'boolean
  :package-version '(nvemacs . "0.0.4"))

(defvar nvemacs--comments-held nil
  "(VALUE) of the comment setting the index holds, while the user keeps it.
Set when the user declines the rebuild, so that notes indexed later
agree with the rest of the index.")

(defun nvemacs--comments-on ()
  "Non-nil if comments are indexed with the body."
  (if nvemacs--comments-held (car nvemacs--comments-held) nvemacs-search-comments))

(defun nvemacs--strip-regions (regions &optional filler)
  "Delete REGIONS, ((BEG . END) ...), from the buffer, putting FILLER in each."
  (dolist (r (sort (copy-sequence regions) (lambda (a b) (> (car a) (car b)))))
    (delete-region (car r) (cdr r))
    (when filler (goto-char (car r)) (insert filler))))

(defun nvemacs--date-problems (titles created dates has-keys)
  "Return the header problems of a note and its date, as (PROBLEMS . PARTS).
TITLES, CREATED and DATES are the non-blank values, ((VALUE . POS) ...),
in file order.  HAS-KEYS means the note has a header with keys.  A
problem is (KIND LINE DETAIL).  PARTS is the note's date: the first
readable Created, then the first readable Date."
  (let* ((line (lambda (pos) (and pos (line-number-at-pos pos))))
         (parts (or (cl-some (lambda (v) (nvemacs--date-parts (car v))) created)
                    (cl-some (lambda (v) (nvemacs--date-parts (car v))) dates)))
         (dated (append created dates))
         (out nil))
    (dolist (c `((several-title . ,titles) (several-created . ,created) (several-date . ,dates)))
      (when (cddr c)
        (push (list (car c) (funcall line (cdr (nth 2 c))) nil) out)))
    (cond ((and (null dated) has-keys) (push (list 'missing-date nil nil) out))
          ((and dated (null parts))
           (push (list 'unreadable-date (funcall line (cdar dated)) (caar dated)) out)))
    (cons (nreverse out) parts)))

(defun nvemacs--org-scan ()
  "Read the keys and comments of the Org text in the current buffer.
Return a plist: :fields, the note's fields; :strip, the regions that are
not body text; :comments, the body's comments; :title and :date, from the
header; :problems.  The buffer is not changed.  One regexp finds the few
lines that matter, so a long note costs little (see
`nvemacs--org-heading-regexp')."
  (let ((case-fold-search t)
        (header-end (nvemacs--org-header-end))
        fields strip comments titles created dates outside has-keys)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\r\n")
      (beginning-of-line)
      ;; A bare mode line, such as `; -*-Org-*-'.
      (when (and (< (point) header-end) (looking-at "[^#\n]*-\\*-.*-\\*-.*$"))
        (push (list nil (string-trim (match-string-no-properties 0)) "comment" (point)) fields)
        (push (cons (point) (line-beginning-position 2)) strip))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\(?:#\\|:PROPERTIES:\\)" nil t)
        (beginning-of-line)
        (let* ((bol (point)) (next (line-beginning-position 2)) (in-header (< bol header-end)))
          (cond
           ((looking-at nvemacs--org-block-regexp)
            (let* ((type (downcase (match-string 1)))
                   (end (save-excursion
                          (forward-line 1)
                          (and (re-search-forward (concat "^[ \t]*#\\+end_" type "[ \t]*$") nil t)
                               (min (point-max) (1+ (point)))))))
              (when (and end (equal type "comment")) (push (cons bol end) comments))
              (goto-char (or end next))))
           ((looking-at "[ \t]*#\\+\\([^ \t\n:]+\\):[ \t]*\\(.*\\)$")
            (let* ((key (match-string-no-properties 1))
                   (val (string-trim (match-string-no-properties 2)))
                   (up (upcase key)))
              (push (cons bol next) strip)
              (unless (equal up "END")
                (push (list key val (if in-header "header" "body") bol) fields)
                (cond
                 ((not (member up '("TITLE" "CREATED" "DATE" "FILETAGS"))))
                 ((not in-header) (push (list up bol) outside))
                 ((string-empty-p val))
                 ((equal up "TITLE") (push (cons val bol) titles))
                 ((equal up "CREATED") (push (cons val bol) created))
                 ((equal up "DATE") (push (cons val bol) dates)))
                (when in-header (setq has-keys t))))
            (goto-char next))
           ((looking-at "[ \t]*#\\(?:[ \t]+\\|$\\)")
            (if in-header
                (let ((text (string-trim (buffer-substring-no-properties (match-end 0) (max (match-end 0) (1- next))))))
                  (push (cons bol next) strip)
                  (unless (string-empty-p text)
                    (push (list nil text "comment" bol) fields)))
              (push (cons bol next) comments))
            (goto-char next))
           ((looking-at "[ \t]*:PROPERTIES:[ \t]*$")
            (let ((end (nvemacs--org-drawer-end)))
              (forward-line 1)
              (when end
                (while (< (point) end)
                  (when (looking-at "[ \t]*:\\([^ \t\n:]+\\):[ \t]*\\(.*\\)$")
                    (let ((key (match-string-no-properties 1))
                          (val (string-trim (match-string-no-properties 2))))
                      (unless (string-equal-ignore-case key "END")
                        (push (list key val (if in-header "header" "body") (point)) fields)
                        (when in-header
                          (setq has-keys t)
                          (when (and (string-equal-ignore-case key "CREATED")
                                     (not (string-empty-p val)))
                            (push (cons val (point)) created))))))
                  (forward-line 1))
                (push (cons bol end) strip))))
           (t (goto-char next)))))
      ;; A #+Created: comes before a drawer's :CREATED:, as written or not.
      (let* ((titles (nreverse titles))
             (created (sort (nreverse created)
                            (lambda (a b)
                              (and (save-excursion (goto-char (cdr a)) (looking-at "[ \t]*#"))
                                   (save-excursion (goto-char (cdr b)) (looking-at "[ \t]*:"))))))
             (checked (nvemacs--date-problems titles created (nreverse dates) has-keys))
             (late (mapcar (lambda (o)
                             (list (intern (concat (downcase (car o)) "-outside"))
                                   (line-number-at-pos (cadr o)) nil))
                           (nreverse outside))))
        (list :fields (nreverse fields) :strip strip :comments comments
              :title (caar titles) :date (cdr checked)
              :problems (append late (car checked)))))))

(defun nvemacs--value-scalar (s)
  "Return front-matter value S without quotes or a trailing comment."
  (let ((s (string-trim s)))
    (cond ((string-match "\\`\\([\"']\\)\\(.*?\\)\\1\\(?:[ \t]+#.*\\)?\\'" s) (match-string 2 s))
          ((string-match "[ \t]+#.*\\'" s) (substring s 0 (match-beginning 0)))
          (t s))))

(defun nvemacs--yaml-fields (beg end)
  "Return the fields of the YAML between BEG and END.
This is a limited reader for the common shapes: nested keys become
`author.name', each list item is a value, and block strings are joined.
A line it cannot read stays as a value of the key above it, so its text
can still be found."
  (save-excursion
    (goto-char beg)
    (let (out stack last)
      (while (< (point) end)
        (let ((pos (point)))
          (cond
           ((looking-at "[ \t]*$"))
           ((looking-at "[ \t]*#[ \t]*\\(.*\\)$")
            (push (list nil (string-trim (match-string-no-properties 1)) "comment" pos) out))
           ((looking-at (concat "\\([ \t]*\\)\\(-[ \t]+\\)?"
                                "\\(\"[^\"\n]+\"\\|'[^'\n]+'\\|[^ \t\n:#\"'-][^:\n]*?\\)"
                                "[ \t]*:\\(?:[ \t]+\\(.*\\)\\)?$"))
            (let* ((i (+ (length (match-string 1)) (length (or (match-string 2) ""))))
                   (key (nvemacs--md-unquote (match-string-no-properties 3)))
                   (raw (string-trim (or (match-string-no-properties 4) ""))))
              (while (and stack (>= (caar stack) i)) (pop stack))
              (let ((full (if stack (concat (cdar stack) "." key) key)))
                (setq last full)
                (cond
                 ((string-empty-p raw)
                  (push (cons i full) stack)
                  (push (list full "" "header" pos) out))
                 ((string-match-p "\\`[|>][-+0-9]*\\'" raw)
                  (let (lines)
                    (forward-line 1)
                    (while (and (< (point) end)
                                (or (looking-at "[ \t]*$")
                                    (progn (looking-at "[ \t]*")
                                           (> (- (match-end 0) (match-beginning 0)) i))))
                      (push (string-trim (buffer-substring-no-properties (point) (line-end-position))) lines)
                      (forward-line 1))
                    (forward-line -1)
                    (push (list full (string-join (delete "" (nreverse lines)) " ") "header" pos) out)))
                 ((string-match "\\`\\[\\(.*\\)\\]\\'" raw)
                  (dolist (it (nvemacs--md-list-items (match-string 1 raw)))
                    (push (list full it "header" pos) out)))
                 (t (push (list full (nvemacs--value-scalar raw) "header" pos) out))))))
           ((looking-at "\\([ \t]*\\)-[ \t]+\\(.*\\)$")
            (let ((i (length (match-string 1)))
                  (val (nvemacs--value-scalar (match-string-no-properties 2))))
              (while (and stack (> (caar stack) i)) (pop stack))
              (let ((key (cdar stack)))
                (push (list key val (if key "header" "comment") pos) out))))
           (t (push (list last (string-trim (buffer-substring-no-properties (point) (line-end-position)))
                          (if last "header" "comment") pos)
                    out))))
        (forward-line 1))
      ;; A key that only holds other keys is not a value of its own.
      (let ((all out))
        (nreverse
         (cl-remove-if
          (lambda (r)
            (and (car r) (string-empty-p (cadr r))
                 (cl-some (lambda (o)
                            (and (car o)
                                 (or (string-prefix-p (concat (car r) ".") (car o))
                                     (and (equal (car o) (car r))
                                          (not (string-empty-p (cadr o)))))))
                          all)))
          out))))))

(defun nvemacs--toml-fields (beg end)
  "Return the fields of the TOML between BEG and END.
A limited reader: `[table]' prefixes its keys, and each array item is a
value.  A line it cannot read stays as a value without a key."
  (save-excursion
    (goto-char beg)
    (let (out prefix)
      (while (< (point) end)
        (let ((pos (point)))
          (cond
           ((looking-at "[ \t]*$"))
           ((looking-at "[ \t]*#[ \t]*\\(.*\\)$")
            (push (list nil (string-trim (match-string-no-properties 1)) "comment" pos) out))
           ((looking-at "[ \t]*\\[\\[?[ \t]*\\([^]\n]+?\\)[ \t]*\\]\\]?[ \t]*$")
            (setq prefix (match-string-no-properties 1)))
           ((looking-at "[ \t]*\\(\"[^\"\n]+\"\\|'[^'\n]+'\\|[^ \t\n=#]+\\)[ \t]*=[ \t]*\\(.*\\)$")
            (let* ((key (nvemacs--md-unquote (match-string-no-properties 1)))
                   (raw (string-trim (match-string-no-properties 2)))
                   (full (if prefix (concat prefix "." key) key)))
              (when (and (string-prefix-p "[" raw) (not (string-search "]" raw)))
                (while (and (not (string-search "]" raw)) (zerop (forward-line 1)) (< (point) end))
                  (setq raw (concat raw " " (string-trim (buffer-substring-no-properties
                                                          (point) (line-end-position)))))))
              (if (string-match "\\`\\[\\(.*\\)\\]" raw)
                  (dolist (it (nvemacs--md-list-items (match-string 1 raw)))
                    (push (list full it "header" pos) out))
                (push (list full (nvemacs--value-scalar raw) "header" pos) out))))
           (t (push (list nil (string-trim (buffer-substring-no-properties (point) (line-end-position)))
                          "comment" pos)
                    out))))
        (forward-line 1))
      (nreverse out))))

(defun nvemacs--json-fields (beg)
  "Return the fields of the JSON object at BEG, flattened like YAML."
  (save-excursion
    (goto-char beg)
    (let ((out nil))
      (cl-labels ((walk (key v)
                    (cond
                     ((vectorp v) (mapc (lambda (x) (walk key x)) v))
                     ((consp v)
                      (dolist (kv v)
                        (let ((name (symbol-name (car kv))))
                          (walk (if key (concat key "." name) name) (cdr kv)))))
                     (t (push (list key
                                    (cond ((stringp v) (string-trim v))
                                          ((numberp v) (number-to-string v))
                                          ((eq v t) "true")
                                          ((eq v :false) "false")
                                          (t ""))
                                    "header" nil)
                              out)))))
        (walk nil (json-parse-buffer :object-type 'alist :null-object :null :false-object :false)))
      (nreverse out))))

(defun nvemacs--md-scan (fm)
  "Read front matter FM.  Return a plist of :fields, :title, :date, :problems."
  (let* ((fields (pcase (car fm)
                   ('json (nvemacs--json-fields (nth 1 fm)))
                   ('toml (nvemacs--toml-fields (nth 3 fm) (nth 4 fm)))
                   (_ (nvemacs--yaml-fields (nth 3 fm) (nth 4 fm)))))
         (values-of (lambda (name)
                      (cl-loop for f in fields
                               when (and (car f) (string-equal-ignore-case (car f) name)
                                         (not (string-empty-p (nth 1 f))))
                               collect (cons (nth 1 f) (nth 3 f)))))
         (titles (funcall values-of "title"))
         (checked (nvemacs--date-problems titles (funcall values-of "created")
                                          (funcall values-of "date")
                                          (and (cl-some #'car fields) t))))
    (list :fields fields :title (caar titles) :date (cdr checked) :problems (car checked))))

(defun nvemacs--md-comments ()
  "Return the regions of `<!-- -->' comments in the Markdown buffer.
Those inside a code fence are code, and an unclosed comment is text."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "<!--" nil t)
      (goto-char (point-min))
      (let ((fence nil) (out nil) (stop nil))
        (while (and (not stop)
                    (re-search-forward "^[ \t]*\\(?:```\\|~~~\\)\\|<!--" nil t))
          (cond
           ((not (equal (match-string 0) "<!--"))
            (setq fence (not fence))
            (forward-line 1))
           (fence nil)
           (t (let ((beg (match-beginning 0)))
                (if (search-forward "-->" nil t)
                    (push (cons beg (point)) out)
                  (setq stop t))))))
        out))))

(defun nvemacs--sister-tags (path)
  "Return the file tags in PATH's sister file.
The file holds one tag per line; lines starting with `# ' are comments,
and blank lines are ignored."
  (let ((f (nvemacs--sister path)) (out nil))
    (when (file-readable-p f)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents f))
        (dolist (l (split-string (buffer-string) "\n"))
          (let ((l (string-trim l)))
            (unless (or (string-empty-p l) (string= l "#") (string-prefix-p "# " l))
              (push (cons l 'file) out))))))
    (nreverse out)))

(defun nvemacs--tags-read (path)
  "Tags of note PATH read from disk now: ((TAG . KIND) ...), clean."
  (nvemacs--tags-clean
   (if (nvemacs--tags-in-file-p path)
       (with-temp-buffer
         (let ((coding-system-for-read 'utf-8)) (insert-file-contents path))
         (if (equal (downcase (file-name-extension path)) "org")
             (nvemacs--org-tags-in-buffer)
           (nvemacs--md-tags-in-buffer)))
     (nvemacs--sister-tags path))))

(defun nvemacs--import-buffer (path)
  "Return the index record of the note in the current buffer.
PATH is the file it was read from.  The record is (TITLE FOLDED-MATCH
FOLDED-TEXT OTHER-P TAGS PREVIEW CREATED CREATED-PARTS FIELDS PROBLEMS):
TITLE, the full title for display (up to `nvemacs--title-store-max');
FOLDED-MATCH, its match part (`nvemacs--match-title'), folded, for the
title tables;
FOLDED-TEXT, the note's text, folded, each whitespace run one space so a
quoted phrase matches across line breaks;
OTHER-P, non-nil if the text holds CJK characters (the `other' tables);
TAGS, the tags found in the text (Org, Markdown), ((TAG . KIND) ...);
PREVIEW, the body as one line (`nvemacs--preview');
CREATED, the declared created date in seconds, or nil;
CREATED-PARTS, that date as written, (WALL . OFFSET): `nvemacs--date-parts';
FIELDS, the note's keys and header comments (see `nvemacs--org-scan');
PROBLEMS, what is wrong with its header, ((KIND LINE DETAIL) ...).
The buffer is modified."
  (let* ((case-fold-search t)
         (ext (downcase (or (file-name-extension path) "")))
         (in-file-title nil)
         (heading nil)
         (tags nil)
         (created nil)
         (fields nil)
         (problems nil))
    ;; Per-extension filters, built in for Org and Markdown.
    (goto-char (point-min))
    (cond
     ((equal ext "org")
      (setq tags (nvemacs--org-tags-in-buffer))   ; before keyword lines go
      (let ((scan (nvemacs--org-scan)))
        (setq in-file-title (plist-get scan :title)
              created (plist-get scan :date)
              fields (plist-get scan :fields)
              problems (plist-get scan :problems)
              heading (nvemacs--org-heading))
        (nvemacs--strip-regions (append (plist-get scan :strip)
                                        (unless (nvemacs--comments-on)
                                          (plist-get scan :comments))))))
     ((equal ext "md")
      (let ((fm (nvemacs--md-front-matter)))
        (when fm
          (setq tags (nvemacs--md-tags-in-buffer))
          (let ((scan (nvemacs--md-scan fm)))
            (setq in-file-title (plist-get scan :title)
                  created (plist-get scan :date)
                  fields (plist-get scan :fields)
                  problems (plist-get scan :problems)))
          (delete-region (nth 1 fm) (nth 2 fm))))
      (unless (nvemacs--comments-on)
        (nvemacs--strip-regions (nvemacs--md-comments) " "))
      (setq heading (nvemacs--md-heading))))
    (setq created (or created (nvemacs--filename-date-parts path)))
    (let* ((first-line (save-excursion
                         (goto-char (point-min))
                         (skip-chars-forward " \t\n")
                         (buffer-substring (point) (line-end-position))))
           (fname (nvemacs--filename-title path))
           (in-file (and in-file-title (not (string-empty-p in-file-title)) in-file-title))
           ;; Cascade: explicit title data, then the
           ;; filename; the first heading only when the filename is just a
           ;; date stamp; then the first line. (TITLE . WHERE-IT-CAME-FROM)
           (pick (pcase nvemacs-import-title-source
                   ('filename (cond (fname (cons fname 'filename))
                                    (heading (cons heading 'heading))
                                    (t (cons first-line 'first-line))))
                   ('first-line (cons first-line 'first-line))
                   (_ (cond (in-file (cons in-file (if (equal ext "org") 'org-keyword 'front-matter)))
                            (fname (cons fname 'filename))
                            (heading (cons heading 'heading))
                            (t (cons first-line 'first-line))))))
           (title (nvemacs--store-title (or (car pick) "")))
           (preview (nvemacs--preview ext (cdr pick) title)))
      ;; A title that is not part of the note's text (filename, keyword,
      ;; front matter) joins the body text, so its words beyond the match
      ;; part still match as body.
      (when (and (memq (cdr pick) '(filename front-matter org-keyword))
                 (not (string-empty-p title)))
        (goto-char (point-max))
        (insert "\n" title "\n"))
      (when (and in-file (not (equal in-file title)))   ; the title setting chose another
        (goto-char (point-max))
        (insert "\n" in-file "\n"))
      (let ((spelling (nvemacs--filename-spelling path)))
        (when spelling
          (goto-char (point-max))
          (insert "\n" spelling "\n")))
      (nvemacs--fold-buffer)
      (nvemacs--collapse-whitespace)
      (let ((text (buffer-string)))
        (list title (nvemacs--fold (car (nvemacs--match-title title))) text
              (and (string-match-p nvemacs--cjk-regexp text) t)
              (nvemacs--tags-clean tags) preview
              (and created (nvemacs--date-moment created)) created fields problems)))))

(defvar nvemacs--rowid-units-per-second 1
  "Resolution of rowid = modification time.
Seconds, not microseconds: the FTS5
index stores rowid DELTAS per trigram, and µs timestamps made every delta a
5-byte varint.  Measured on 10 MB (4,377 notes): µs 32.8 MB, ms 27.5 MB,
s 23.1 MB; no collisions in any case.  Same-second saves are ordered by the
+1 nudge.")

(defun nvemacs--mtime-us (attrs)
  "Return the modification time in file ATTRS as an integer rowid.
The unit is 1/`nvemacs--rowid-units-per-second' s since 1970."
  (car (time-convert (file-attribute-modification-time attrs)
                     nvemacs--rowid-units-per-second)))

;;;; Index database

(defvar nvemacs--db nil "Open index database, or nil.")

(defconst nvemacs--tables '("body" "title" "body_other" "title_other"))

;; Everything nvEmacs writes is readable by the user only: the index holds
;; the text of every note. New directories are 0700, files 0600.

(defun nvemacs--private-directory (dir)
  "Create DIR, readable by the user only, unless it exists; return DIR.
An existing directory keeps its modes: it may be one the user chose."
  (unless (file-directory-p dir)
    (with-file-modes #o700 (make-directory dir t)))
  dir)

(defun nvemacs--write-file (file &optional modes)
  "Write the current buffer to FILE through a temporary file.
The temporary file is then renamed to FILE, so FILE is never
half-written.  The text is written as UTF-8 unless
`coding-system-for-write' says otherwise.  FILE gets MODES, or by
default is readable by the user only (`make-temp-file' creates 0600)."
  (let ((tmp (make-temp-file (expand-file-name (concat "." (file-name-nondirectory file) "-")
                                               (file-name-directory file))))
        (coding-system-for-write (or coding-system-for-write 'utf-8-unix)))
    (condition-case err
        (progn (write-region nil nil tmp nil 'silent)
               (when modes (set-file-modes tmp modes))
               (rename-file tmp file t))
      (error (ignore-errors (delete-file tmp))
             (signal (car err) (cdr err))))))

(defun nvemacs--command-text (command expectation solution)
  "Return an error's text: COMMAND EXPECTATION.  SOLUTION.
It says what was tried, what that needs and what to do next."
  (format "%s %s.  %s." command expectation solution))

(defun nvemacs--command-error (command expectation solution)
  "Signal a `user-error' that reads COMMAND EXPECTATION.  SOLUTION.
A short path in it goes to *Messages* in full first."
  (let* ((text (nvemacs--command-text command expectation solution))
         (full (nvemacs--full-paths text)))
    (unless (equal full text)
      (let ((inhibit-message t)) (message "%s" full)))
    (user-error "%s" (substring-no-properties text))))

(defun nvemacs--command-message (command expectation solution)
  "Show COMMAND EXPECTATION.  SOLUTION, without stopping anything.
The echo area gets a short path as it is, and *Messages* gets it in full."
  (let* ((text (nvemacs--command-text command expectation solution))
         (full (nvemacs--full-paths text)))
    (if (equal full text)
        (message "%s" full)
      (let ((inhibit-message t)) (message "%s" full))
      (let ((message-log-max nil)) (message "%s" (substring-no-properties text))))))

(defconst nvemacs--path-tail-length 20
  "How many characters of a file's folder a message shows, from its end.")

(defun nvemacs--short-path (path)
  "Return PATH for a message: its filename, then the end of its folder.
A full path can push the rest of the message out of the echo area.  The
text remembers PATH, for `nvemacs--full-paths'.  A bare name stays as it is."
  (let ((folder (file-name-directory path)))
    (propertize
     (if (null folder)
         path
       (format "%s <%s>" (file-name-nondirectory path)
               (if (<= (length folder) nvemacs--path-tail-length)
                   folder
                 (concat "..." (substring folder (- nvemacs--path-tail-length))))))
     'nvemacs-full-path path)))

(defun nvemacs--full-paths (text)
  "Return TEXT with each `nvemacs--short-path' replaced by its full path."
  (let ((pos 0) (parts nil))
    (while (< pos (length text))
      (let ((next (or (next-single-property-change pos 'nvemacs-full-path text)
                      (length text))))
        (push (or (get-text-property pos 'nvemacs-full-path text)
                  (substring-no-properties text pos next))
              parts)
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun nvemacs--error-reason (err)
  "Return the text of ERR without the file names a file error ends with."
  (if (and (memq 'file-error (get (car err) 'error-conditions))
           (stringp (nth 2 err)) (nth 3 err))
      (nth 2 err)                       ; ("Opening output file" REASON FILE)
    (error-message-string err)))

(defconst nvemacs--sqlite-missing
  (concat "Emacs lacks sqlite support that nvEmacs requires.  "
          "Refer to your Emacs documentation to determine how to obtain sqlite support.  "
          "See https://www.gnu.org/software/emacs/manual/html_node/elisp/Database.html")
  "The message shown when this Emacs was built without SQLite.")

(defun nvemacs--require-sqlite ()
  "Signal a `user-error' unless this Emacs has built-in SQLite support.
Without it there is no index, and nvEmacs cannot run."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error "%s" nvemacs--sqlite-missing)))

(defun nvemacs--db ()
  "Return the open index database, creating the schema if needed."
  (unless (and nvemacs--db (sqlitep nvemacs--db))
    (nvemacs--require-sqlite)
    (nvemacs--private-directory (file-name-directory (nvemacs--index-path)))
    (setq nvemacs--db (with-file-modes #o700 (sqlite-open (nvemacs--index-path))))
    (dolist (tbl nvemacs--tables)
      (sqlite-execute
       nvemacs--db
       (format "CREATE VIRTUAL TABLE IF NOT EXISTS %s USING fts5(x, tokenize='trigram case_sensitive 0', detail=none)" tbl)))
    (sqlite-execute nvemacs--db
                    "CREATE TABLE IF NOT EXISTS meta (id INTEGER PRIMARY KEY, path TEXT UNIQUE, size INTEGER, mtime INTEGER, title TEXT, other INTEGER, tmtime INTEGER, preview TEXT, created INTEGER, cguess INTEGER, added INTEGER, title_key TEXT, tags_key TEXT, cwall INTEGER, coff INTEGER)")
    ;; Tags: FOLDED is the tag through `nvemacs--fold' (search is
    ;; case-insensitive); TAG keeps the spelling for display; KIND file|section.
    (sqlite-execute nvemacs--db
                    "CREATE TABLE IF NOT EXISTS tags (id INTEGER, tag TEXT, folded TEXT, kind TEXT)")
    (sqlite-execute nvemacs--db "CREATE INDEX IF NOT EXISTS tags_folded ON tags(folded)")
    (sqlite-execute nvemacs--db "CREATE INDEX IF NOT EXISTS tags_id ON tags(id)")
    ;; Fields: KEY as spelled, FKEY and FOLDED for search.  A header comment
    ;; has no key.  KIND is header, body or comment.
    (sqlite-execute nvemacs--db
                    "CREATE TABLE IF NOT EXISTS fields (id INTEGER, key TEXT, fkey TEXT, value TEXT, folded TEXT, kind TEXT, pos INTEGER)")
    (sqlite-execute nvemacs--db "CREATE INDEX IF NOT EXISTS fields_fkey ON fields(fkey)")
    (sqlite-execute nvemacs--db "CREATE INDEX IF NOT EXISTS fields_id ON fields(id)")
    (sqlite-execute nvemacs--db
                    "CREATE TABLE IF NOT EXISTS problems (id INTEGER, kind TEXT, line INTEGER, detail TEXT)")
    (sqlite-execute nvemacs--db "CREATE INDEX IF NOT EXISTS problems_id ON problems(id)")
    (sqlite-execute nvemacs--db
                    "CREATE TABLE IF NOT EXISTS info (key TEXT PRIMARY KEY, value)")
    (sqlite-execute nvemacs--db
                    "INSERT OR IGNORE INTO info VALUES ('generation', 0)")
    ;; An index an earlier version left readable by others is closed off.
    ;; SQLite gives its journal files the index's modes.
    (let ((modes (file-modes (nvemacs--index-path))))
      (when (and modes (/= 0 (logand modes #o077)))
        (set-file-modes (nvemacs--index-path) #o600))))
  nvemacs--db)

(defun nvemacs--bump-generation (db)
  "Count one more change to the index DB.
A list or cache drawn for an older generation is stale."
  (sqlite-execute db "UPDATE info SET value = value + 1 WHERE key = 'generation'"))

(defun nvemacs--delete-id (db id)
  "Remove note ID from every table of DB."
  (dolist (tbl nvemacs--tables)
    (sqlite-execute db (format "DELETE FROM %s WHERE rowid = ?" tbl) (list id)))
  (sqlite-execute db "DELETE FROM tags WHERE id = ?" (list id))
  (sqlite-execute db "DELETE FROM fields WHERE id = ?" (list id))
  (sqlite-execute db "DELETE FROM problems WHERE id = ?" (list id))
  (sqlite-execute db "DELETE FROM meta WHERE id = ?" (list id)))

;; Raw bytes left by decoding invalid UTF-8 (Emacs keeps them as the
;; chars #x3FFF80-#x3FFFFF).
(defconst nvemacs--raw-byte-regexp (rx (any (#x3fff80 . #x3fffff))))

(define-error 'nvemacs-skip "nvEmacs: note skipped")

(defvar nvemacs--skipped nil
  "Notes the last rebuild or reconcile could not index: ((PATH . REASON) ...).")

(defun nvemacs--read-note (path &optional attrs)
  "Read the note at PATH and return its index record.
The record is as `nvemacs--import-buffer' returns it.  ATTRS, if given,
are PATH's file attributes, already read.  Signal `nvemacs-skip' with a
reason when the note must not be indexed: too large, unreadable, binary
or not UTF-8."
  (let ((size (file-attribute-size (or attrs (file-attributes path)))))
    (when (and size (> size nvemacs-import-max-size))
      (signal 'nvemacs-skip (list (format "larger than %.1f MB"
                                          (/ nvemacs-import-max-size 1048576.0))))))
  (with-temp-buffer
    (condition-case err
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents path))
      (file-error (signal 'nvemacs-skip (list (error-message-string err)))))
    (goto-char (point-min))
    (when (search-forward (string 0) nil t)
      (signal 'nvemacs-skip (list "binary (contains NUL bytes)")))
    (goto-char (point-min))
    (when (re-search-forward nvemacs--raw-byte-regexp nil t)
      (signal 'nvemacs-skip (list "not UTF-8")))
    (goto-char (point-min))
    (when (eq (char-after) #xFEFF) (delete-char 1))   ; byte-order mark
    (let ((rec (nvemacs--import-buffer path)))
      (unless (nvemacs--tags-in-file-p path)
        (setf (nth 4 rec) (nvemacs--tags-clean (nvemacs--sister-tags path))))
      rec)))

(defun nvemacs--insert-fields (db id fields)
  "Store FIELDS of note ID in DB.
They are folded in one pass and written in one statement: a typical
header has a dozen keys, and one of each per note would slow a rebuild."
  (when fields
    (let* ((clean (lambda (s) (subst-char-in-string ?\n ?\s (or s ""))))
           (folded (split-string
                    (nvemacs--fold
                     (mapconcat (lambda (f) (concat (funcall clean (car f)) "\n"
                                                    (funcall clean (nth 1 f))))
                                fields "\n"))
                    "\n"))
           (args nil))
      (dolist (f fields)
        (let ((fkey (pop folded)) (fval (pop folded)))
          (setq args (nconc args (list id (car f) (and (car f) fkey)
                                       (nth 1 f) fval (nth 2 f) (nth 3 f))))))
      (sqlite-execute db (concat "INSERT INTO fields VALUES "
                                 (mapconcat (lambda (_) "(?,?,?,?,?,?,?)") fields ","))
                      args))))

(defun nvemacs--insert-note (db path attrs rec)
  "Insert record REC of PATH (file ATTRS) into DB under rowid = mtime.
Every note gets an Added date (`nvemacs--added-date').  Its Created date
is the one it declares, or else Added, marked as a guess.  The folded
title and tags are stored as sort keys: Emacs's SQLite API has no custom
collation."
  (let ((id (nvemacs--mtime-us attrs)))
    (while (sqlite-select db "SELECT 1 FROM meta WHERE id = ?" (list id))
      (setq id (1+ id)))                          ; collision: nudge by 1
    (pcase-let* ((`(,title ,ftitle ,ftext ,other ,tags ,preview ,declared ,parts ,fields ,problems) rec)
                 (added (nvemacs--added-date path attrs))
                 (folded (mapcar (lambda (tk) (nvemacs--fold (car tk))) tags)))
      (sqlite-execute db "INSERT INTO meta VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
                      (list id path (file-attribute-size attrs)
                            (nvemacs--mtime-us attrs) title (if other 1 0)
                            (nvemacs--sister-mtime path)
                            preview (or declared added) (if declared 0 1)
                            added (nvemacs--fold (or title ""))
                            (nvemacs--tags-key folded) (car parts) (cdr parts)))
      (cl-loop for tk in tags for f in folded
               do (sqlite-execute db "INSERT INTO tags VALUES (?,?,?,?)"
                                  (list id (car tk) f (symbol-name (cdr tk)))))
      (nvemacs--insert-fields db id fields)
      (nvemacs--dir-note-added db path)
      (dolist (pb problems)
        (sqlite-execute db "INSERT INTO problems VALUES (?,?,?,?)"
                        (list id (symbol-name (car pb)) (nth 1 pb) (nth 2 pb))))
      (sqlite-execute db "INSERT INTO body(rowid, x) VALUES (?,?)" (list id ftext))
      (sqlite-execute db "INSERT INTO title(rowid, x) VALUES (?,?)" (list id ftitle))
      (when other
        (sqlite-execute db "INSERT INTO body_other(rowid, x) VALUES (?,?)" (list id ftext))
        (sqlite-execute db "INSERT INTO title_other(rowid, x) VALUES (?,?)" (list id ftitle))))
    id))

;;;; Added dates: the earliest date seen for a file
;;
;; Every note has an Added date: the earlier of the file's creation and
;; modified dates when nvEmacs first finds it. A file found days late keeps
;; its own older date, an edit never moves it, a restore with an older
;; mtime moves it earlier. A note that declares no created date also shows
;; it as its Created date, marked `~'. These dates cannot be rebuilt from
;; the notes, so they live in their own plain-text file, not in the index:
;; deleting the index stays a safe fix, and the file can sit in Dropbox for
;; several machines (keys are relative to the watched directory). The index
;; copies the dates into `meta' for display and sorting.

(defcustom nvemacs-dates-file nil
  "File of the Added dates of notes.
nil means dates.eld in `nvemacs-data-directory'.  The file is plain
text and may be shared between machines (e.g. through Dropbox)."
  :type '(choice (const nil) file)
  :package-version '(nvemacs . "0.0.2"))

(defvar nvemacs--dates nil
  "Hash table of Added dates: KEY -> earliest date seen for the file.
Times are in seconds.  nil until read from `nvemacs--dates-file'.")

(defvar nvemacs--births nil
  "During a full build, PATH -> creation time, read for all notes at once.
nil otherwise: a note seen for the first time is then asked about alone.")

(defconst nvemacs--birth-batch 500 "Paths given to one run of `stat'.")

(defun nvemacs--birth-program ()
  "Return (PROGRAM ARGS...) printing \"SECONDS PATH\", or nil if there is none.
The full path on macOS and BSD: a GNU `stat' earlier in PATH reads `-f'
as something else."
  (pcase system-type
    ((or 'darwin 'berkeley-unix)
     (and (file-executable-p "/usr/bin/stat") '("/usr/bin/stat" "-f" "%B %N")))
    ('gnu/linux
     (let ((p (executable-find "stat"))) (and p (list p "-c" "%W %n"))))))

(defun nvemacs--birth-times (paths)
  "Return a hash table PATH -> creation time in seconds, for PATHS.
Emacs cannot read this date, so `stat' is run.  A file it reports no
date for is left out.  Each answer carries its path, because `stat'
prints nothing for a file that is gone."
  (let ((h (make-hash-table :test #'equal))
        (program (nvemacs--birth-program))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (default-directory "/"))
    (when program
      (while paths
        (let ((chunk (seq-take paths nvemacs--birth-batch)))
          (setq paths (nthcdr nvemacs--birth-batch paths))
          (with-temp-buffer
            (ignore-errors
              (apply #'call-process (car program) nil '(t nil) nil (append (cdr program) chunk)))
            (goto-char (point-min))
            (while (re-search-forward "^\\([0-9]+\\) \\(.+\\)$" nil t)
              (let ((secs (string-to-number (match-string 1))))
                (when (> secs 0) (puthash (match-string 2) secs h))))))))
    h))

(defun nvemacs--added-date (path attrs)
  "Record and return the Added date of note PATH, with file ATTRS.
The creation date is asked for only when the note is not in the dates
file yet: saves can reset it, and it costs a process."
  (let* ((mtime (time-convert (file-attribute-modification-time attrs) 'integer))
         (birth (cond (nvemacs--births (gethash path nvemacs--births))
                      ((gethash (nvemacs--dates-key path) (nvemacs--dates-table)) nil)
                      (t (gethash path (nvemacs--birth-times (list path)))))))
    (nvemacs--dates-seen path (if (and birth (< birth mtime)) birth mtime))))

(defun nvemacs--tags-key (folded)
  "Return the sort key of a note's FOLDED tags.
They are alphabetized and de-duplicated: the order they were entered in
means nothing.  The separator sorts below every character, so a note
tagged art and zoo comes before one tagged artist."
  (mapconcat #'identity (sort (delete-dups (copy-sequence folded)) #'string<) "\x01"))

(defvar nvemacs--dates-dirty nil "Non-nil when `nvemacs--dates' has unsaved changes.")

(defun nvemacs--dates-file ()
  "Return the path of the dates file.
That is `nvemacs-dates-file', or dates.eld in `nvemacs-data-directory'."
  (or nvemacs-dates-file (expand-file-name "dates.eld" nvemacs-data-directory)))

(defun nvemacs--dates-read (table)
  "Merge the entries of the dates file into TABLE and return TABLE.
Where both have a key, the earlier date is kept."
  (let ((f (nvemacs--dates-file)))
    (when (file-readable-p f)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents f))
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((e (read (current-buffer))))
                (when (and (consp e) (stringp (car e)) (integerp (cadr e)))
                  (let ((old (gethash (car e) table)))
                    (when (or (null old) (< (cadr e) old)) (puthash (car e) (cadr e) table))))))
          (end-of-file nil)
          (invalid-read-syntax
           (nvemacs--command-message
            "The Added-dates file"
            (format "requires valid entries, and %s is damaged at line %d"
                    (nvemacs--short-path f) (line-number-at-pos))
            "Repair or restore that file"))))))
  table)

(defun nvemacs--dates-table ()
  "Return the dates hash table, reading the dates file the first time."
  (or nvemacs--dates
      (setq nvemacs--dates (nvemacs--dates-read (make-hash-table :test #'equal)))))

(defvar nvemacs--watched-roots nil
  "Cache of (WATCHED-DIRECTORIES . their truenames).
Calling `file-truename' per note doubled the rebuild time (measured
2026-11-19).")

(defun nvemacs--watched-roots ()
  "The watched directories resolved through symlinks, as directory names."
  (unless (equal (car nvemacs--watched-roots) nvemacs-watched-directories)
    (setq nvemacs--watched-roots
          (cons nvemacs-watched-directories
                (mapcar (lambda (d) (file-name-as-directory (file-truename (expand-file-name d))))
                        nvemacs-watched-directories))))
  (cdr nvemacs--watched-roots))

(defun nvemacs--dates-key (path)
  "Return the dates-file key of PATH.
The key is PATH relative to its watched directory, prefixed with that
directory's name (\"notes/journal/a.txt\"), so it is the same on
machines whose home directories differ."
  (or (cl-loop for dir in (nvemacs--watched-roots)
               when (string-prefix-p dir path)
               return (concat (file-name-nondirectory (directory-file-name dir)) "/"
                              (substring path (length dir))))
      path))

(defun nvemacs--dates-seen (path secs)
  "Record date SECS for note PATH; return the earliest seen."
  (let* ((table (nvemacs--dates-table))
         (key (nvemacs--dates-key path))
         (old (gethash key table)))
    (if (and old (<= old secs))
        old
      (puthash key secs table)
      (setq nvemacs--dates-dirty t)
      secs)))

(defun nvemacs--dates-flush ()
  "Write the dates file if it changed.
The table is first merged with what is on disk (another machine may
have added entries), then written through a temporary file and a
rename."
  (when (and nvemacs--dates nvemacs--dates-dirty)
    (let* ((f (nvemacs--dates-file))
           (entries nil))
      (nvemacs--private-directory (file-name-directory f))
      (nvemacs--dates-read nvemacs--dates)
      (maphash (lambda (k v) (push (cons k v) entries)) nvemacs--dates)
      (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
      (with-temp-buffer
        (insert ";; nvEmacs Added dates: the earliest creation or modified date nvEmacs has\n"
                ";; seen for each note (seconds since 1970). Paths are relative to their\n"
                ";; watched directory. Written by nvEmacs. Keep this file: unlike the index\n"
                ";; it cannot be rebuilt from the notes.\n")
        (let ((print-escape-newlines t))
          (dolist (e entries) (prin1 (list (car e) (cdr e)) (current-buffer)) (insert "\n")))
        (nvemacs--write-file f))
      (setq nvemacs--dates-dirty nil))))

(defun nvemacs--index-file (db path attrs)
  "Read note PATH, with file ATTRS, and index it in DB.
Signals `nvemacs-skip' before writing anything."
  (nvemacs--insert-note db path attrs (nvemacs--read-note path attrs)))

(defmacro nvemacs--with-transaction (db &rest body)
  "Run BODY in a transaction on DB: commit if it finishes, else roll back."
  (declare (indent 1))
  (let ((ok (make-symbol "ok")) (d (make-symbol "db")))
    `(let ((,d ,db) (,ok nil))
       (sqlite-transaction ,d)
       (unwind-protect (prog1 (progn ,@body) (setq ,ok t))
         (if ,ok (sqlite-commit ,d) (sqlite-rollback ,d))))))

(defun nvemacs--skip-summary ()
  "Return a message suffix counting skipped notes, or \"\" if none."
  (if nvemacs--skipped
      (format ", %d skipped (M-x nvemacs-index-skipped)" (length nvemacs--skipped))
    ""))

(defun nvemacs-index-skipped ()
  "List the notes the last rebuild or reconcile could not index, and why."
  (interactive)
  (with-current-buffer (get-buffer-create "*nvEmacs skipped*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%d note(s) not indexed:\n\n" (length nvemacs--skipped)))
      (dolist (s nvemacs--skipped) (insert (format "%s\n    %s\n" (car s) (cdr s)))))
    (special-mode)
    (pop-to-buffer (current-buffer))))

;;;; Index version

(defconst nvemacs--index-version 9
  "Format version of the index.
Bump it whenever the schema, folding, import filters or rowid unit
change: `nvemacs' rebuilds an index with another version, because a
stale one gives wrong results without any error.
1 = prototype M1 (no stamp); 2 = skips, BOM, Org/Markdown title rules;
3 = filename before heading, heading only for date-stamp filenames;
4 = paths stored with directories resolved through symlinks;
5 = tags table, meta.tmtime (sister tag file mtime);
6 = full title (display) + match part in the title tables, the full title
    in the body when it is not in the text, meta.preview, meta.created and
    meta.cguess, info.match_width;
7 = a filename with `_' also in the body as spelled
    (`nvemacs--filename-spelling');
8 = meta.added, and the sort keys meta.title_key and meta.tags_key;
9 = meta.cwall and meta.coff, the Created date as written; the fields and
    problems tables; whitespace runs in the body indexed as one space;
    keys and comments out of the body.")

(defun nvemacs--index-current-p ()
  "Non-nil if the index was built with `nvemacs--index-version'."
  (equal (caar (sqlite-select (nvemacs--db) "SELECT value FROM info WHERE key = 'version'"))
         nvemacs--index-version))

(defun nvemacs--canonical-path (path)
  "Return PATH with its directory resolved through symlinks.
Paths are compared after realpath.  The note file itself is not
resolved.  Emacs already decodes macOS filenames to NFC."
  (let ((path (expand-file-name path)))
    (expand-file-name (file-name-nondirectory path)
                      (file-truename (file-name-directory path)))))

(defvar nvemacs--listed-dirs nil
  "Directories walked by the last `nvemacs--note-files' (for watches).")

(defvar nvemacs--listed-sisters (make-hash-table :test #'equal)
  "Sister tag files seen by the last `nvemacs--note-files': NOTE-PATH -> mtime.")

(defun nvemacs--note-ext-regexp ()
  "Return a regexp matching file names with a note extension.
The extensions are `nvemacs-import-extensions'."
  (concat "\\.\\(" (regexp-opt nvemacs-import-extensions) "\\)\\'"))

(defun nvemacs--walk-roots ()
  "The watched directories that exist, resolved through symlinks."
  (cl-loop for d in nvemacs-watched-directories
           for dd = (file-name-as-directory (file-truename (expand-file-name d)))
           when (file-directory-p dd) collect dd))

(defun nvemacs--list-dir (dir ext-re)
  "One directory of the walk, as (NOTES SUBDIRS SISTERS): NOTES (PATH .
ATTRS) of the notes in DIR (names matching EXT-RE), SUBDIRS the
subdirectories to walk too, and
SISTERS (NOTE-PATH . MTIME-US) of its `.tags' sister files."
  (let (notes subdirs sisters)
    (dolist (entry (directory-files-and-attributes dir t nil t))
      (let ((name (file-name-nondirectory (car entry)))
            (attrs (cdr entry)))
        (cond
         ((member name '("." "..")))
         ((eq (file-attribute-type attrs) t)
          (when (and nvemacs-watched-directories-recursive
                     (not (string-match-p nvemacs-ignore-dir-regexp name)))
            (push (file-name-as-directory (car entry)) subdirs)))
         ((and (null (file-attribute-type attrs)) (string-match-p ext-re name))
          (unless (nvemacs--malformed-path-p (car entry))
            (push entry notes)))
         ((and (null (file-attribute-type attrs)) (string-suffix-p ".tags" name))
          (push (cons (substring (car entry) 0 -5) (nvemacs--mtime-us attrs)) sisters)))))
    (list notes (nreverse subdirs) sisters)))

(defun nvemacs--note-files ()
  "Alist (PATH . ATTRS) of every note in the watched directories.
Also sets `nvemacs--listed-dirs' and `nvemacs--listed-sisters'."
  (let ((ext-re (nvemacs--note-ext-regexp))
        (todo (nvemacs--walk-roots))
        (dirs nil) (out nil))
    (clrhash nvemacs--listed-sisters)
    (while todo
      (let* ((d (pop todo)) (r (nvemacs--list-dir d ext-re)))
        (push d dirs)
        (setq out (nconc (nth 0 r) out)
              todo (append (nth 1 r) todo))
        (dolist (sis (nth 2 r)) (puthash (car sis) (cdr sis) nvemacs--listed-sisters))))
    (setq nvemacs--listed-dirs dirs)
    out))

(defun nvemacs-index-rebuild ()
  "Delete the index and build it from every note."
  (interactive)
  (nvemacs--comments-restore)
  (when nvemacs--db (sqlite-close nvemacs--db) (setq nvemacs--db nil))
  (when (file-exists-p (nvemacs--index-path)) (delete-file (nvemacs--index-path)))
  (let* ((db (nvemacs--db))
         (files (nvemacs--note-files))
         (nvemacs--births (nvemacs--birth-times (mapcar #'car files)))
         (n 0))
    (setq nvemacs--skipped nil
          nvemacs--comments-held nil)
    (nvemacs--with-transaction db
      (dolist (f files)
        (condition-case err
            (progn (nvemacs--index-file db (car f) (cdr f))
                   (when (zerop (% (cl-incf n) 1000))
                     (sqlite-commit db) (sqlite-transaction db)))
          (nvemacs-skip (push (cons (car f) (cadr err)) nvemacs--skipped))))
      (sqlite-execute db "INSERT OR REPLACE INTO info VALUES ('version', ?)"
                      (list nvemacs--index-version))
      (sqlite-execute db "INSERT OR REPLACE INTO info VALUES ('match_width', ?)"
                      (list nvemacs-title-match-width))
      (sqlite-execute db "INSERT OR REPLACE INTO info VALUES ('search_comments', ?)"
                      (list (if nvemacs-search-comments 1 0)))
      (sqlite-execute db "INSERT OR REPLACE INTO info VALUES ('rebuilt', ?)"
                      (list (format-time-string "%F %R")))
      (nvemacs--bump-generation db))
    (nvemacs--dates-flush)
    (dolist (tbl nvemacs--tables)
      (sqlite-execute db (format "INSERT INTO %s(%s) VALUES('optimize')" tbl tbl)))
    ;; Reclaim the pages 'optimize' frees: without it the file was 3.3x the
    ;; text at 100 MB instead of ~1.6x (measured 2026-11-18).
    (sqlite-execute db "VACUUM")
    (nvemacs--malformed-save)
    (message "nvEmacs: indexed %d notes%s" n (nvemacs--skip-summary))
    n))

(defvar nvemacs--plan-batch 1000 "Index entries a plan reads per step.")
(defvar nvemacs--plan-files-per-step 500 "Files a plan compares per step.")

(cl-defstruct (nvemacs--pl (:constructor nvemacs--pl-make) (:copier nil))
  "A reconcile plan in progress: stages `known' (read the index's sizes
and times, a batch per step), `list' (one directory per step), `compare'
\(a batch of files per step), then `done' with ITEMS."
  (stage 'known) (after nil) (known (make-hash-table :test #'equal))
  (ext-re nil) (todo nil) (dirs nil) (files nil)
  (sisters (make-hash-table :test #'equal))
  (listed (make-hash-table :test #'equal))
  (items nil))

(defun nvemacs--plan-new ()
  "Return a new reconcile plan (see `nvemacs--pl')."
  (nvemacs--pl-make :ext-re (nvemacs--note-ext-regexp)))

(defun nvemacs--plan-step (db p)
  "Do one bounded step of plan P against the index DB.
Return non-nil when P is complete; its items are then in
`nvemacs--pl-items'.  The folder comparison used to be one 50 ms piece;
now no step blocks typing for long."
  (pcase (nvemacs--pl-stage p)
    ('known
     (let ((rows (if (nvemacs--pl-after p)
                     (sqlite-select db "SELECT id, path, size, mtime, tmtime FROM meta WHERE id < ? ORDER BY id DESC LIMIT ?"
                                    (list (nvemacs--pl-after p) nvemacs--plan-batch))
                   (sqlite-select db "SELECT id, path, size, mtime, tmtime FROM meta ORDER BY id DESC LIMIT ?"
                                  (list nvemacs--plan-batch)))))
       (dolist (r rows) (puthash (nth 1 r) (nthcdr 2 r) (nvemacs--pl-known p)))
       (if (< (length rows) nvemacs--plan-batch)
           (setf (nvemacs--pl-stage p) 'list
                 (nvemacs--pl-todo p) (nvemacs--walk-roots))
         (setf (nvemacs--pl-after p) (car (car (last rows)))))))
    ('list
     (let ((d (pop (nvemacs--pl-todo p))))
       (when d
         (let ((r (nvemacs--list-dir d (nvemacs--pl-ext-re p))))
           (push d (nvemacs--pl-dirs p))
           (setf (nvemacs--pl-files p) (nconc (nth 0 r) (nvemacs--pl-files p))
                 (nvemacs--pl-todo p) (append (nth 1 r) (nvemacs--pl-todo p)))
           (dolist (sis (nth 2 r)) (puthash (car sis) (cdr sis) (nvemacs--pl-sisters p))))))
     (unless (nvemacs--pl-todo p)          ; the listing is complete: publish it
       (setq nvemacs--listed-dirs (nvemacs--pl-dirs p)
             nvemacs--listed-sisters (nvemacs--pl-sisters p))
       (setf (nvemacs--pl-stage p) 'compare)))
    ('compare
     (let ((known (nvemacs--pl-known p)) (listed (nvemacs--pl-listed p)) (n 0))
       (while (and (nvemacs--pl-files p) (< n nvemacs--plan-files-per-step))
         (let* ((f (pop (nvemacs--pl-files p)))
                (path (car f)) (attrs (cdr f)) (k (gethash path known)))
           (puthash path t listed)
           (when (or (null k)
                     (/= (nth 0 k) (file-attribute-size attrs))
                     (/= (nth 1 k) (nvemacs--mtime-us attrs))
                     ;; a sister tag file changes without touching the note
                     (/= (or (nth 2 k) 0)
                         (if (nvemacs--tags-in-file-p path) 0
                           (gethash path nvemacs--listed-sisters 0))))
             (push (cons path t) (nvemacs--pl-items p)))
           (remhash path known))
         (cl-incf n))
       (unless (nvemacs--pl-files p)
         (setq nvemacs--skipped (cl-remove-if-not (lambda (s) (gethash (car s) listed))
                                                  nvemacs--skipped))
         (maphash (lambda (path _k) (push (cons path nil) (nvemacs--pl-items p))) known)
         (setf (nvemacs--pl-stage p) 'done)))))
  (eq (nvemacs--pl-stage p) 'done))

(defun nvemacs--sync-plan (db)
  "Compare the watched directories with `meta' in DB, by metadata only.
Return the paths whose entry may be out of date, as (PATH . LISTED):
LISTED non-nil for a note on disk (new or changed), nil for an entry
whose note is gone (or no longer a watched note).  Previously skipped
notes are retried; skips for notes no longer listed are dropped.
All at once; background reconciles take the same steps in slices."
  (let ((p (nvemacs--plan-new)))
    (while (not (nvemacs--plan-step db p)))
    (nvemacs--pl-items p)))

(defvar nvemacs--sync-changes nil
  "(OLD-ID . NEW-ID) of index entries the running background run changed;
OLD-ID nil for a new note, NEW-ID nil for a deleted one.")

(defun nvemacs--sync-item (db item)
  "Bring the entry in index DB of ITEM, (PATH . LISTED), up to date.
The file's CURRENT state is read (an item may wait in the queue while
the file changes again).  Return `new', `changed', `deleted', `skipped'
or nil (no change).  A changed note that cannot be read keeps its old
entry."
  (let* ((path (car item))
         (k (car (sqlite-select db "SELECT id, size, mtime, tmtime FROM meta WHERE path = ?" (list path))))
         (attrs (and (cdr item) (file-attributes path))))
    (cond
     ((null attrs)
      (setq nvemacs--skipped (assoc-delete-all path nvemacs--skipped))
      (when k
        (nvemacs--delete-id db (nth 0 k))
        (push (cons (nth 0 k) nil) nvemacs--sync-changes)
        'deleted))
     ((and k (= (nth 1 k) (file-attribute-size attrs))
           (= (nth 2 k) (nvemacs--mtime-us attrs))
           (= (or (nth 3 k) 0) (nvemacs--sister-mtime path)))
      nil)
     (t
      (condition-case err
          (let ((rec (nvemacs--read-note path attrs)))   ; read BEFORE deleting
            (when k (nvemacs--delete-id db (nth 0 k)))
            (push (cons (car k) (nvemacs--insert-note db path attrs rec)) nvemacs--sync-changes)
            (setq nvemacs--skipped (assoc-delete-all path nvemacs--skipped))
            (if k 'changed 'new))
        (nvemacs-skip
         (setf (alist-get path nvemacs--skipped nil nil #'equal) (cadr err))
         'skipped))))))

(defun nvemacs-index-reconcile ()
  "Bring the index up to date with the watched directories, now.
The directories are compared with the index by metadata only, and what
changed is indexed again.  Return (NEW CHANGED DELETED), the counts.
Background syncs use the same steps in idle slices."
  (interactive)
  (let ((db (nvemacs--db)) (new 0) (changed 0) (deleted 0)
        (nvemacs--sync-changes nil))        ; callers search again themselves
    (nvemacs--with-transaction db
      (dolist (item (nvemacs--sync-plan db))
        (pcase (nvemacs--sync-item db item)
          ('new (cl-incf new)) ('changed (cl-incf changed)) ('deleted (cl-incf deleted))))
      (when (> (+ new changed deleted) 0) (nvemacs--bump-generation db)))
    (nvemacs--dates-flush)
    (when (> (+ new changed deleted) 0) (nvemacs--malformed-save))
    (list new changed deleted)))

(defun nvemacs--ensure-match-width ()
  "Make the title tables hold titles cut at `nvemacs-title-match-width'.
When the index was built with another width, rewrite them from the full
titles in `meta' — no note is read; only titles longer than the narrower
of the two widths can differ (a title is at most 2 columns per char)."
  (let* ((db (nvemacs--db))
         (old (caar (sqlite-select db "SELECT value FROM info WHERE key = 'match_width'")))
         (new nvemacs-title-match-width))
    (unless (equal old new)
      (let* ((eff (lambda (w) (if (and (integerp w) (> w 0)) w most-positive-fixnum)))
             (lo (if (integerp old) (min (funcall eff old) (funcall eff new)) 0))
             (rows (sqlite-select db "SELECT id, title, other FROM meta WHERE length(title) * 2 > ?"
                                  (list lo))))
        (nvemacs--with-transaction db
          (dolist (r rows)
            (let ((f (nvemacs--fold (car (nvemacs--match-title (or (nth 1 r) ""))))))
              (dolist (tbl (if (eql (nth 2 r) 1) '("title" "title_other") '("title")))
                (sqlite-execute db (format "DELETE FROM %s WHERE rowid = ?" tbl) (list (car r)))
                (sqlite-execute db (format "INSERT INTO %s(rowid, x) VALUES (?,?)" tbl) (list (car r) f)))))
          (sqlite-execute db "INSERT OR REPLACE INTO info VALUES ('match_width', ?)" (list new))
          (when rows (nvemacs--bump-generation db)))))))

(defun nvemacs--comments-indexed-p ()
  "Non-nil if the index was built with comments in plain search."
  (eql 1 (caar (sqlite-select (nvemacs--db)
                              "SELECT value FROM info WHERE key = 'search_comments'"))))

(defun nvemacs--comments-restore ()
  "Apply the choice `nvemacs-index-comments' kept in settings.eld, if any."
  (pcase (nvemacs--setting 'search-comments)
    ('on (setq nvemacs-search-comments t))
    ('off (setq nvemacs-search-comments nil))))

(defun nvemacs--comments-check ()
  "Offer a rebuild if `nvemacs-search-comments' differs from the index.
Comments are in the index or not, so only a rebuild can change that.
Without one the index keeps what it holds, and notes indexed later
follow it, so a search never sees half of them each way."
  (let ((indexed (nvemacs--comments-indexed-p)))
    (cond
     ((eq indexed (and nvemacs-search-comments t)) (setq nvemacs--comments-held nil))
     ((and (not noninteractive)
           (y-or-n-p (format "Comment search is now %s.  Rebuild the index to apply it? "
                             (if nvemacs-search-comments "on" "off"))))
      (nvemacs-index-rebuild))
     (t
      (setq nvemacs--comments-held (list indexed))
      (message "nvEmacs: comments stay %s until M-x nvemacs-index-rebuild"
               (if indexed "searched" "unsearched"))))))

;;;; Background sync and auto-save
;; Defined with the search window below.
(defvar nvemacs--buffer-name)
(defvar nvemacs--query-start)
(defvar nvemacs--rows-start)
(require 'filenotify)
;;
;; Triggers only prompt a reconcile; reconcile compares metadata, so any
;; trigger catches every change (NV's rule). Measured 2026-11-18 (macOS,
;; kqueue): file-notify reports creates, deletes, atomic saves (tmp + mv)
;; and new subdirectories, but NOT writes in place (`echo >> note') and not
;; files inside subdirectories (one watch per directory). Hence a second
;; trigger: a frame regaining focus (the user was editing elsewhere).

(defcustom nvemacs-reconcile-on-startup t
  "Non-nil: reconcile the index in the background when nvEmacs starts."
  :type 'boolean
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-sync-delay 1.0
  "Seconds to wait after a change event before reconciling.
Events in between are coalesced (Notational Velocity uses a 1.0 s
FSEvents latency).  The reconcile then waits until Emacs has been idle
`nvemacs--sync-idle' seconds."
  :type 'number
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-sync-on-focus t
  "Non-nil means reconcile when an Emacs frame regains focus.
This is needed because file-notify (kqueue) does not report notes
rewritten in place."
  :type 'boolean
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-sync-slice-ms 10
  "Longest stretch of background work, in milliseconds.
Background work compares the folders with the index and indexes changed
notes; after this long, Emacs attends to typing again.  A key pressed
meanwhile waits at most this long."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-auto-save-interval 3.0
  "Idle seconds before nvEmacs copies your edits to a temp file.
This covers the notes nvEmacs opened; a note you already had open when
nvEmacs showed it is yours to save.  The note's own file is written
when you save it, when the cursor leaves the note, and when nvEmacs
exits, and only if its text really changed.  A changed value takes
effect at the next pause.  nil means no temp files and no writing on
leaving or at exit; closing a changed note, or Emacs, still offers to
save it."
  :type '(choice (const :tag "Never" nil) number)
  :package-version '(nvemacs . "0.0.2"))

(defconst nvemacs--sync-idle 0.5
  "Idle seconds before background work starts or continues.
So background work never runs while the user types.")

(defvar nvemacs--watches nil "Alist (DIRECTORY . file-notify descriptor).")
(defvar nvemacs--sync-timer nil "Pending coalesced reconcile.")
(defvar nvemacs--sync-queue nil "Items (PATH . LISTED) waiting to be indexed.")
(defvar nvemacs--work-timer nil "Pending background indexing slice.")
(defvar nvemacs--auto-save-timer nil)
(defvar nvemacs--auto-save-armed nil "The interval `nvemacs--auto-save-timer' was armed with.")
(defvar nvemacs--pending-timer nil "Copies edits to temp files during nonstop typing.")
(defconst nvemacs--pending-max-seconds 30
  "Longest time edits go without a temp-file copy.
The idle timer never fires while the user keeps typing.")
(defvar nvemacs--plan nil "The reconcile plan being worked through (`nvemacs--plan-new'), or nil.")

;;;; Instrumentation
;;
;; Three logs, one JSON record per line, each capped at
;; `nvemacs-log-max-bytes' by dropping its OLDEST records. One file per
;; kind, so frequent records can never push out rare ones:
;;   startup.jsonl  each start of nvEmacs: time to index, first row, first screen
;;   session.jsonl  each end of nvEmacs: duration, searches, rows, memory, GC
;;   sync.jsonl     the startup reconcile, and later runs that changed something

(defcustom nvemacs-log-directory nil
  "Where instrumentation logs go; nil = \"nvemacs-log/\" in `nvemacs-data-directory'."
  :type '(choice (const :tag "Beside the index" nil) directory)
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-log-max-bytes 10240
  "Largest size of each instrumentation log; the oldest records are dropped."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(define-minor-mode nvemacs-instrumentation
  "Log startup, session and sync measurements to `nvemacs-log-directory'.
On by default; it must be on before `nvemacs' to
measure a start.  Reports: \\[nvemacs-report-all]."
  :global t :init-value t :group 'nvemacs
  (when (called-interactively-p 'interactive)
    (message "nvEmacs instrumentation %s%s" (if nvemacs-instrumentation "on" "off")
             (if nvemacs-instrumentation (concat ", logging to " (nvemacs--log-directory)) ""))))

(defvar nvemacs--shown)
(defvar nvemacs--started-in-process nil
  "Non-nil once `nvemacs' has built its list in this Emacs process.")
(defvar nvemacs--startup nil "Plist of the start being measured.")
(defvar nvemacs--session nil "Plist of the running nvEmacs session (kept even when not logging).")
(defvar nvemacs--sync-run nil "Plist of the reconcile run in progress.")
(defvar nvemacs--sync-trigger nil "Why the next reconcile runs: `startup', or nil (an event).")
(defvar nvemacs--last-sync nil "Plist of the last finished reconcile run.")
(defvar nvemacs--last-search nil "Plist describing the last search.")

(defun nvemacs--log-directory ()
  "Return the directory of the instrumentation logs.
That is `nvemacs-log-directory', or nvemacs-log/ in
`nvemacs-data-directory'."
  (file-name-as-directory
   (or nvemacs-log-directory
       (expand-file-name "nvemacs-log/" nvemacs-data-directory))))

(defun nvemacs--ms (since)
  "Milliseconds from float time SINCE to now, one decimal."
  (/ (round (* 10000 (- (float-time) since))) 10.0))

(defun nvemacs--rss-mb ()
  "Return the resident size of this Emacs in MB, or nil if unknown."
  (let ((kb (alist-get 'rss (process-attributes (emacs-pid)))))
    (and kb (/ (round (/ kb 102.4)) 10.0))))

(defun nvemacs--json (v)
  "V as a JSON value: nil -> false, other non-numbers kept."
  (cond ((null v) :false) ((symbolp v) (if (eq v t) t (symbol-name v))) (t v)))

(defun nvemacs--log (kind record)
  "Append RECORD (alist) to log KIND as one JSON line led by the time.
Keep the file within `nvemacs-log-max-bytes' by dropping its oldest lines.
Written to a temporary file and renamed, so a log is never half-written.
A logging failure is reported and never stops nvEmacs."
  (condition-case err
      (let* ((dir (nvemacs--log-directory))
             (file (expand-file-name (concat kind ".jsonl") dir))
             (line (json-serialize
                    (cons (cons 'time (format-time-string "%FT%T%z"))
                          (mapcar (lambda (c) (cons (car c) (nvemacs--json (cdr c)))) record)))))
        (nvemacs--private-directory dir)
        (with-temp-buffer
          (when (file-exists-p file)
            (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file)))
          (goto-char (point-max))
          (insert line "\n")
          (goto-char (point-min))
          (while (and (> (1- (position-bytes (point-max))) nvemacs-log-max-bytes)
                      (save-excursion (forward-line 1) (< (point) (point-max))))
            (delete-region (point) (progn (forward-line 1) (point))))
          (nvemacs--write-file file)))
    (error
     (nvemacs--command-message
      (format "The %s log" kind)
      (format "requires a writable data folder, and %s could not be written: %s"
              (nvemacs--short-path
               (expand-file-name (concat kind ".jsonl") (nvemacs--log-directory)))
              (nvemacs--error-reason err))
      "Check the permissions of nvemacs-data-directory"))))

(defun nvemacs--note-first-row ()
  "Called after rows are inserted: time the first row of a measured start."
  (when (and nvemacs--startup nvemacs--shown (not (plist-get nvemacs--startup :first-row)))
    (setq nvemacs--startup (plist-put nvemacs--startup :first-row
                                      (nvemacs--ms (plist-get nvemacs--startup :t0)))))
  (when nvemacs--session
    (setq nvemacs--session (plist-put nvemacs--session :max-rows
                                      (max (or (plist-get nvemacs--session :max-rows) 0)
                                           (length nvemacs--shown))))))

(defun nvemacs--note-count ()
  "Return the number of notes in the index."
  (or (caar (sqlite-select (nvemacs--db) "SELECT count(*) FROM meta")) 0))

(defun nvemacs--startup-finish (rows want)
  "Log the measured start: ROWS on the first screen, WANT rows fit."
  (let ((s nvemacs--startup))
    (setq nvemacs--startup nil)
    (redisplay t)                       ; time the screen as drawn, not as inserted
    (nvemacs--log
     "startup"
     `((cold . ,(plist-get s :cold))
       (rebuilt . ,(plist-get s :rebuilt))
       (index_ms . ,(plist-get s :index))
       (first_row_ms . ,(or (plist-get s :first-row) :null))
       (screen_ms . ,(nvemacs--ms (plist-get s :t0)))
       (screen_rows . ,rows)
       (window_rows . ,want)
       (notes . ,(nvemacs--note-count))
       (index_kb . ,(/ (or (file-attribute-size (file-attributes (nvemacs--index-path))) 0) 1024))
       (rss_mb . ,(or (nvemacs--rss-mb) :null))
       (emacs_up_s . ,(/ (round (* 100 (float-time (time-subtract (plist-get s :call-time)
                                                                   before-init-time))))
                         100.0))
       (emacs . ,emacs-version)
       (gui . ,(display-graphic-p))))))

(defun nvemacs--on-kill-emacs ()
  "Emacs is exiting: remember the notelist widths and log the session.
Not from a batch Emacs: tests and scripts let-bind the data directory,
and at exit only the global default (~/.emacs.d/nvemacs/) is left."
  (unless noninteractive
    (nvemacs--history-end 'quit)
    (when (get-buffer nvemacs--buffer-name)
      (nvemacs--widths-save)
      (nvemacs--homepage-save))
    (dolist (b (buffer-list))
      (with-current-buffer b (nvemacs--trash-if-untouched)))
    (nvemacs--session-end)))

(defun nvemacs--session-begin ()
  "Start measuring a session: searches, rows and garbage collection."
  (setq nvemacs--session (list :t0 (float-time) :searches 0 :max-rows 0
                               :gcs gcs-done :gc-time gc-elapsed)))

(defun nvemacs--session-end ()
  "Log the session that is ending (*nvEmacs* killed, or Emacs exiting)."
  (let ((s nvemacs--session))
    (setq nvemacs--session nil)
    (when (and s nvemacs-instrumentation)
      (nvemacs--log
       "session"
       `((duration_s . ,(/ (round (* 10 (- (float-time) (plist-get s :t0)))) 10.0))
         (searches . ,(plist-get s :searches))
         (max_rows . ,(plist-get s :max-rows))
         (notes . ,(if nvemacs--db (nvemacs--note-count) :null))
         (rss_mb . ,(or (nvemacs--rss-mb) :null))
         (gcs . ,(- gcs-done (plist-get s :gcs)))
         (gc_s . ,(/ (round (* 1000 (- gc-elapsed (plist-get s :gc-time)))) 1000.0)))))))

(defun nvemacs--sync-run-add (result)
  "Count RESULT (`new', `changed', `deleted', `skipped', ...) in the run."
  (when (and nvemacs--sync-run (memq result '(new changed deleted skipped)))
    (setq nvemacs--sync-run (plist-put nvemacs--sync-run result
                                       (1+ (plist-get nvemacs--sync-run result))))))

(defun nvemacs--sync-run-finish ()
  "Finish the measured sync run: the queue is empty.
Keep the run for `nvemacs-report-sync', and log it if it was the
startup run or changed something (focus syncs that find nothing would
fill the log)."
  (let ((r nvemacs--sync-run))
    (setq nvemacs--sync-run nil)
    (when r
      (setq r (plist-put r :wall-ms (nvemacs--ms (plist-get r :t0))))
      (setq nvemacs--last-sync (plist-put r :when (format-time-string "%F %T")))
      (when (eq (plist-get r :trigger) 'startup)
        (nvemacs--malformed-save)
        (nvemacs--homepage-checked r))
      (when (and nvemacs-instrumentation
                 (or (eq (plist-get r :trigger) 'startup) (> (plist-get r :items) 0)))
        (nvemacs--log
         "sync"
         `((trigger . ,(plist-get r :trigger))
           (plan_ms . ,(plist-get r :plan-ms))
           (items . ,(plist-get r :items))
           (new . ,(plist-get r 'new)) (changed . ,(plist-get r 'changed))
           (deleted . ,(plist-get r 'deleted)) (skipped . ,(plist-get r 'skipped))
           (slices . ,(plist-get r :slices))
           (work_ms . ,(plist-get r :work-ms))
           (wall_ms . ,(plist-get r :wall-ms))
           (dirs . ,(length nvemacs--listed-dirs))
           (notes . ,(nvemacs--note-count))))))))

(defun nvemacs--timer-pending-p (timer)
  "Return non-nil if TIMER is set and has not run yet."
  (and timer (or (memq timer timer-list) (memq timer timer-idle-list))))

(defun nvemacs--sync-next (fn)
  "Return a timer that runs FN once Emacs has been idle long enough.
FN runs soon if Emacs has already been idle `nvemacs--sync-idle'
seconds, else once it has been: background work waits while the user
types."
  (let ((idle (current-idle-time)))
    (if (and idle (>= (float-time idle) nvemacs--sync-idle) (not (input-pending-p)))
        (run-with-timer 0.01 nil fn)
      (run-with-idle-timer nvemacs--sync-idle nil fn))))

(defun nvemacs--sync-soon (&optional delay)
  "Reconcile after DELAY seconds, then only once Emacs is idle.
DELAY defaults to `nvemacs-sync-delay'.  A call while one is pending is
absorbed by it."
  (unless (nvemacs--timer-pending-p nvemacs--sync-timer)
    (setq nvemacs--sync-timer
          (run-with-timer (or delay nvemacs-sync-delay) nil
                          (lambda ()
                            (setq nvemacs--sync-timer (nvemacs--sync-next #'nvemacs--sync-start)))))))

(defun nvemacs--sync-start ()
  "Start a reconcile: make a fresh plan and work through it in slices.
The queue is worked through with it.  A plan in progress is replaced:
it may have listed a directory before the change that prompted this
one."
  (setq nvemacs--sync-timer nil
        nvemacs--plan (nvemacs--plan-new))
  (unless nvemacs--sync-run                 ; instrumentation: joins a run still draining
    (setq nvemacs--sync-run
          (list :t0 (float-time) :trigger (or nvemacs--sync-trigger 'event)
                :plan-ms nil :items 0
                'new 0 'changed 0 'deleted 0 'skipped 0 :slices 0 :work-ms 0.0)))
  (setq nvemacs--sync-trigger nil)
  (when nvemacs--work-timer (cancel-timer nvemacs--work-timer))
  (nvemacs--sync-work))

(defun nvemacs--plan-finish ()
  "The plan is complete: queue its items, watch every directory it listed."
  (let ((items (nvemacs--pl-items nvemacs--plan))
        (queued (make-hash-table :test #'equal))
        (added nil))
    (setq nvemacs--plan nil)
    (dolist (it nvemacs--sync-queue) (puthash (car it) t queued))
    (dolist (it items)
      (unless (gethash (car it) queued)
        (puthash (car it) t queued)
        (push it added)))
    (setq nvemacs--sync-queue (nconc nvemacs--sync-queue (nreverse added)))
    (when nvemacs--sync-run
      (setq nvemacs--sync-run
            (plist-put (plist-put nvemacs--sync-run :items
                                  (+ (length added) (plist-get nvemacs--sync-run :items)))
                       :plan-ms (or (plist-get nvemacs--sync-run :plan-ms)
                                    (nvemacs--ms (plist-get nvemacs--sync-run :t0))))))
    (nvemacs--update-watches)))

(defun nvemacs--sync-work ()
  "Do one slice of background work.
Plan steps come first, then queued items, for up to
`nvemacs-sync-slice-ms' (at least one step), stopping early when input
is pending.  While work remains, reschedule once Emacs is idle; at the
end, show the notes that changed in the list."
  (setq nvemacs--work-timer nil)
  (when (or nvemacs--plan nvemacs--sync-queue)
    (let* ((db (nvemacs--db))
           (t0 (float-time))
           (deadline (+ t0 (/ nvemacs-sync-slice-ms 1000.0)))
           (changed nil)
           (first t))
      (nvemacs--with-transaction db
        (while (and (or nvemacs--plan nvemacs--sync-queue)
                    (or first (and (< (float-time) deadline) (not (input-pending-p)))))
          (setq first nil)
          (if nvemacs--plan
              (when (nvemacs--plan-step db nvemacs--plan) (nvemacs--plan-finish))
            (let ((result (nvemacs--sync-item db (pop nvemacs--sync-queue))))
              (nvemacs--sync-run-add result)
              (when (memq result '(new changed deleted))
                (setq changed t)))))
        (when changed (nvemacs--bump-generation db)))
      (when changed (nvemacs--dates-flush))
      (when nvemacs--sync-run
        (setq nvemacs--sync-run
              (plist-put (plist-put nvemacs--sync-run :slices
                                    (1+ (plist-get nvemacs--sync-run :slices)))
                         :work-ms (+ (plist-get nvemacs--sync-run :work-ms) (nvemacs--ms t0)))))))
  (if (or nvemacs--plan nvemacs--sync-queue)
      (setq nvemacs--work-timer (nvemacs--sync-next #'nvemacs--sync-work))
    (nvemacs--sync-run-finish)
    (when nvemacs--sync-changes
      (let ((changes (nreverse nvemacs--sync-changes)))
        (setq nvemacs--sync-changes nil)
        (nvemacs--list-update changes)))))

(defun nvemacs--indexed-as-is-p (path)
  "Non-nil if PATH is a note whose index entry matches its size and time."
  (and nvemacs--db
       (member (downcase (or (file-name-extension path) "")) nvemacs-import-extensions)
       (let ((attrs (file-attributes path))
             (k (car (sqlite-select nvemacs--db "SELECT size, mtime FROM meta WHERE path = ?"
                                    (list (nvemacs--canonical-path path))))))
         (and attrs k
              (= (nth 0 k) (file-attribute-size attrs))
              (= (nth 1 k) (nvemacs--mtime-us attrs))))))

(defun nvemacs--watch-event (event)
  "Reconcile soon after file-notify EVENT on a note or a directory.
The event's details are not trusted to say what changed (Notational
Velocity's rule).  One exception: an event for a note whose size and
time already match the index, as after nvEmacs's own save, which
re-indexed it (without this, every auto-save was followed by a full
rescan)."
  (let ((file (nth 2 event)))
    (when (and (or (null (file-name-extension file))
                   (member (downcase (file-name-extension file))
                           (cons "tags" nvemacs-import-extensions)))
               (not (nvemacs--indexed-as-is-p file)))
      (nvemacs--sync-soon))))

(defun nvemacs--update-watches ()
  "Watch every directory the last listing walked.
Watches of directories that are gone are dropped.  kqueue watches are
not recursive, hence one watch per directory."
  (require 'filenotify)
  (let ((want nvemacs--listed-dirs))
    (dolist (w nvemacs--watches)
      (unless (and (member (car w) want) (file-notify-valid-p (cdr w)))
        (ignore-errors (file-notify-rm-watch (cdr w)))
        (setq nvemacs--watches (delq w nvemacs--watches))))
    (dolist (d want)
      (unless (assoc d nvemacs--watches)
        (condition-case nil
            (push (cons d (file-notify-add-watch d '(change) #'nvemacs--watch-event))
                  nvemacs--watches)
          (error nil))))))                 ; no watch: focus and C-c C-g still sync

(defun nvemacs--focus-changed ()
  "`after-focus-change-function': a frame regained focus — sync."
  (when (and nvemacs-sync-on-focus nvemacs--db
             (cl-some (lambda (f) (eq (frame-focus-state f) t)) (frame-list)))
    (nvemacs--sync-soon 0.1)))

(defun nvemacs--auto-save-arm ()
  "(Re)start the auto-save timers at `nvemacs-auto-save-interval'."
  (dolist (tm (list nvemacs--auto-save-timer nvemacs--pending-timer))
    (when tm (cancel-timer tm)))
  (setq nvemacs--auto-save-armed nvemacs-auto-save-interval
        nvemacs--auto-save-timer
        (and nvemacs-auto-save-interval
             (run-with-idle-timer nvemacs-auto-save-interval t #'nvemacs--auto-save))
        nvemacs--pending-timer
        (and nvemacs-auto-save-interval
             (run-with-timer nvemacs--pending-max-seconds nvemacs--pending-max-seconds
                             #'nvemacs--pending-save))))

(defun nvemacs--auto-save ()
  "Copy edits to their temp files and write notes the cursor has left.
Run by the idle timer.  A changed `nvemacs-auto-save-interval' takes
effect here."
  (unless (eql nvemacs--auto-save-armed nvemacs-auto-save-interval)
    (nvemacs--auto-save-arm))
  (when nvemacs-auto-save-interval
    (nvemacs--pending-save)
    (nvemacs--check-left)))

(defun nvemacs--sync-enable ()
  "Start watches, the focus trigger and auto-save."
  (add-hook 'post-command-hook #'nvemacs--check-left)
  (add-hook 'kill-emacs-query-functions #'nvemacs--quit-query 90)
  ;; Advice on two core commands, on purpose.  Before Emacs asks "Save
  ;; file ...?", every note whose typing was deleted by hand must lose its
  ;; modified flag, or Emacs asks about a note that has not changed.  No
  ;; hook runs at that moment (checked in files.el, Emacs 31.1):
  ;;  - `save-some-buffers-functions' runs AFTER the questions, and
  ;;    `save-buffers-kill-emacs' consults it only when no buffer needs
  ;;    saving;
  ;;  - `kill-emacs-query-functions' runs after `save-some-buffers';
  ;;  - `save-some-buffers-default-predicate' is the user's option, and
  ;;    `save-buffers-kill-emacs' ignores it;
  ;;  - `buffer-save-without-query' would save real edits without asking.
  ;; So the advice stays, kept small: `:before' only, it ignores the
  ;; arguments, its errors are demoted to messages, it is added here and
  ;; removed in `nvemacs--sync-disable' at exit, and it touches only the
  ;; notes waiting in `nvemacs--waiting'.  (2026-11-25 review: kept.)
  (advice-add 'save-some-buffers :before #'nvemacs--settle-unchanged)
  (advice-add 'save-buffers-kill-emacs :before #'nvemacs--settle-unchanged)
  (add-function :after after-focus-change-function #'nvemacs--focus-changed)
  (unless (and nvemacs--auto-save-timer (eql nvemacs--auto-save-armed nvemacs-auto-save-interval))
    (nvemacs--auto-save-arm))
  (add-hook 'pre-redisplay-functions #'nvemacs--pin-sync)
  (nvemacs--note-files)                     ; sets `nvemacs--listed-dirs'
  (nvemacs--update-watches))

(defun nvemacs--sync-disable ()
  "Stop watches, timers, the focus trigger and auto-save."
  (remove-hook 'pre-redisplay-functions #'nvemacs--pin-sync)
  (walk-windows (lambda (w) (when (nvemacs--pinned-p w)
                              (set-window-parameter w 'header-line-format nil)))
                'nomini t)
  (dolist (w nvemacs--watches) (ignore-errors (file-notify-rm-watch (cdr w))))
  (setq nvemacs--watches nil nvemacs--sync-queue nil nvemacs--plan nil nvemacs--sync-changes nil
        nvemacs--sync-run nil)            ; the next start must not join it
  (dolist (tm (list nvemacs--sync-timer nvemacs--work-timer nvemacs--auto-save-timer
                    nvemacs--pending-timer))
    (when tm (cancel-timer tm)))
  (setq nvemacs--sync-timer nil nvemacs--work-timer nil nvemacs--pending-timer nil
        nvemacs--auto-save-timer nil nvemacs--auto-save-armed nil)
  (remove-hook 'post-command-hook #'nvemacs--check-left)
  (remove-hook 'kill-emacs-query-functions #'nvemacs--quit-query)
  (advice-remove 'save-some-buffers #'nvemacs--settle-unchanged)
  (advice-remove 'save-buffers-kill-emacs #'nvemacs--settle-unchanged)
  (remove-function after-focus-change-function #'nvemacs--focus-changed))

;;;; Search core, no UI

(defconst nvemacs--or-mark "\uFDD0"
  "Stands in, through the fold, for a capital OR typed as a word.
The fold lowercases, so OR and or look the same after it.  U+FDD0 is a
Unicode noncharacter, which typed text never holds.")

(defconst nvemacs--and-mark "\uFDD1"
  "Stands in for a capital AND typed as a word: see `nvemacs--or-mark'.")

(defun nvemacs--mark-operators (query)
  "Return QUERY with each capital OR and AND typed as a word marked.
A word here ends at whitespace or an end of QUERY, or has `(' before it
or `)' after it.  The tokenizer decides whether a mark is an operator;
where it is not, the mark turns back into the word."
  (let ((case-fold-search nil) (pos 0) (parts nil))
    (while (string-match "\\(?:\\`\\|[ \t\n(]\\)\\(OR\\|AND\\)\\(?:[ \t\n)]\\|\\'\\)" query pos)
      (push (substring query pos (match-beginning 1)) parts)
      (push (if (equal (match-string 1 query) "OR") nvemacs--or-mark nvemacs--and-mark) parts)
      (setq pos (match-end 1)))
    (push (substring query pos) parts)
    (apply #'concat (nreverse parts))))

(defun nvemacs--unmark (text)
  "Return TEXT with the operator marks turned back into the words or, and."
  (string-replace nvemacs--and-mark "and" (string-replace nvemacs--or-mark "or" text)))

(defun nvemacs--query-tokens (query &optional plain)
  "Fold QUERY and split it into tokens.
With PLAIN, every token is a word: parentheses and OR are text.
A word is (TEXT . QUOTE-AT).  Spaces separate words, except inside
double quotes, where each run is one space.  QUOTE-AT is where in TEXT
the quoted part starts, or nil.  A quote opens only at the start of a
token, after a `-', or after a colon, so 12\" is plain text.  An
unclosed quote runs to the end, so results narrow while a phrase is
being typed.

Outside quotes, parentheses group.  `(' at the start of a token is the
symbol `open', and after a lone `-' it is `neg-open'.  `)' is `close'
when a group is open and only more `)' follow it in the token; any
other parenthesis is text, so f(x) and :) stay words, and (f(x)) is
the word f(x) in a group.  In a group, a capital OR is `or' and a
capital AND is `and'.  An unclosed group runs to the end."
  (let* ((s (nvemacs--fold (if plain query (nvemacs--mark-operators query))))
         (n (length s)) (i 0)
         (tok nil) (at nil) (in nil) (out nil) (depth 0) (inner 0))
    (cl-flet ((finish ()
                (when tok
                  (let ((text (apply #'string (reverse tok))))
                    (push (cond ((and (> depth 0) (null at) (equal text nvemacs--or-mark)) 'or)
                                ((and (> depth 0) (null at) (equal text nvemacs--and-mark)) 'and)
                                (t (cons (nvemacs--unmark text) at)))
                          out)))
                (setq tok nil at nil inner 0)))
      (while (< i n)
        (let ((c (aref s i)))
          (cond
           ((and (eq c ?\") in) (setq in nil))
           ((and (eq c ?\") (not at)
                 (or (null tok) (eq (car tok) ?:) (equal tok '(?-))))
            (setq in t at (length tok)))
           ((memq c '(?\s ?\t ?\n))
            (if in
                (unless (eq (car tok) ?\s) (push ?\s tok))
              (finish)))
           ((or in plain) (push c tok))
           ((and (eq c ?\() (or (null tok) (equal tok '(?-))))
            (push (if tok 'neg-open 'open) out)
            (setq tok nil depth (1+ depth)))
           ((eq c ?\() (push c tok) (cl-incf inner))
           ((and (eq c ?\)) (> inner 0)) (push c tok) (cl-decf inner))
           ((and (eq c ?\)) (> depth 0)
                 (let ((j (1+ i)))
                   (while (and (< j n) (eq (aref s j) ?\))) (cl-incf j))
                   (or (= j n) (memq (aref s j) '(?\s ?\t ?\n)))))
            (finish)
            (push 'close out)
            (cl-decf depth))
           (t (push c tok))))
        (cl-incf i))
      (finish))
    (nreverse out)))

(defun nvemacs--route (terms)
  "Return (TITLE-TABLE . BODY-TABLE) for TERMS."
  (if (cl-some (lambda (term) (string-match-p nvemacs--cjk-regexp term)) terms)
      '("title_other" . "body_other")
    '("title" . "body")))

(defun nvemacs--term-where (terms)
  "Return (SQL . ARGS): column x holds every one of TERMS, literally.
Each term gets the operator that matches it exactly and still lets the
trigram index narrow the rows (timings: the test
`nvemacs-search-term-operator'):
- no `%' or `_': LIKE, where `*', `?' and `[' are plain;
- `%' or `_' but none of `*', `?', `[': GLOB, where `%' and `_' are plain;
- both kinds: LIKE narrows (reading `%' and `_' as wildcards) and
  instr() makes the match exact.  An escaped GLOB (`[[]') would make
  the index check every row.
Text and terms are both folded, so GLOB's case sensitivity changes
nothing."
  (let (sql args)
    (dolist (term terms)
      (cond ((not (string-match-p "[%_]" term))
             (push "x LIKE ?" sql)
             (push (concat "%" term "%") args))
            ((not (string-match-p "[*?[]" term))
             (push "x GLOB ?" sql)
             (push (concat "*" term "*") args))
            (t
             (push "(x LIKE ? AND instr(x, ?) > 0)" sql)
             (push (concat "%" term "%") args)
             (push term args))))
    (cons (mapconcat #'identity (nreverse sql) " AND ") (nreverse args))))

(defun nvemacs--leaf-table (word side)
  "Return the table to find WORD in on SIDE, `title' or `body'."
  (let ((cjk (string-match-p nvemacs--cjk-regexp word)))
    (if (eq side 'title)
        (if cjk "title_other" "title")
      (if cjk "body_other" "body"))))

(defun nvemacs--node-sql (node base side)
  "Return (SQL . ARGS), true of the row `rowid' when it matches NODE.
BASE is the table searched, its words checked on its column x; `:row' a
filter checked note by note; `:list' a filter read as an id list.  A
word goes on BASE's column when that is right and fast: the same side,
and not a CJK word on a table without the CJK notes' index (`서울 OR
korea' must read 서울 in the CJK table and korea in the main one).
Otherwise each row looks up its own note.  A minus checks the whole
note, as `-whale' does, so its words are on the body SIDE."
  (pcase node
    (`(word ,w . ,_)
     (let ((leaf (nvemacs--leaf-table w side)))
       (cond
        ((and (stringp base) (or (equal leaf base) (equal (concat leaf "_other") base)))
         (nvemacs--term-where (list w)))
        ((eq base :list)
         (let ((m (nvemacs--term-where (list w))))
           (cons (format "rowid IN (SELECT rowid FROM %s WHERE %s)" leaf (car m)) (cdr m))))
        (t
         ;; Text is folded, so `instr' is an exact match.
         (cons (format "EXISTS (SELECT 1 FROM (SELECT rowid AS note, x AS doc FROM %s) WHERE note = rowid AND instr(doc, ?) > 0)"
                       leaf)
               (list w))))))
    (`(filter ,item) (nvemacs--filter-cond item nil (not (eq base :list))))
    (`(not ,k)
     (let ((s (nvemacs--node-sql k base 'body)))
       (cons (format "NOT (%s)" (car s)) (cdr s))))
    (`(,op . ,kids)
     (let ((parts (mapcar (lambda (k) (nvemacs--node-sql k base side)) kids)))
       (cons (concat "(" (mapconcat #'car parts (if (eq op 'or) " OR " " AND ")) ")")
             (mapcan (lambda (p) (copy-sequence (cdr p))) parts))))))

(defun nvemacs--text-match (terms side)
  "Return (TABLE WHERE . ARGS): the rows of TABLE whose note has TERMS.
SIDE is `title' or `body'.  TERMS are strings, as for
`nvemacs--term-where', then the group nodes of `nvemacs--parse-query'.
Without groups this is exactly `nvemacs--route' and
`nvemacs--term-where', so a search without parentheses costs what it did.
With groups, TABLE is the CJK one only when every match must hold a CJK
word."
  (let ((groups (cl-remove-if #'stringp terms)))
    (if (null groups)
        (let ((route (nvemacs--route terms)))
          (cons (if (eq side 'title) (car route) (cdr route)) (nvemacs--term-where terms)))
      (let* ((words (cl-remove-if-not #'stringp terms))
             (other (or (cl-some (lambda (w) (string-match-p nvemacs--cjk-regexp w)) words)
                        (cl-some #'nvemacs--node-cjk-p groups)))
             (table (if (eq side 'title)
                        (if other "title_other" "title")
                      (if other "body_other" "body")))
             (parts (append (and words (list (nvemacs--term-where words)))
                            (mapcar (lambda (g) (nvemacs--node-sql g table side)) groups))))
        (cons table (cons (mapconcat #'car parts " AND ")
                          (mapcan (lambda (p) (copy-sequence (cdr p))) parts)))))))

(defconst nvemacs--date-min -62135596800 "An open start of a date range: year 1.")
(defconst nvemacs--date-max 253402300799 "An open end of a date range: year 9999.")

(defun nvemacs--wall (y mo d h mi sec)
  "Return year Y, month MO, day D at H:MI:SEC in seconds, as if UTC."
  (time-convert (encode-time (list sec mi h d mo y nil nil 0)) 'integer))

(defun nvemacs--date-anchor (s)
  "Read S, a date in a search, as (LO HI PRECISION), or nil.
A partial date means its whole span: 2026, 2026-11, 2026-11-23.  With a
time, it means that minute or second.  LO and HI are clock times in
seconds (`nvemacs--date-parts').  PRECISION is `year', `month' or `day'."
  (let ((case-fold-search t))
    (cond
     ((string-match "\\`\\([0-9]\\{4\\}\\)\\'" s)
      (let ((y (string-to-number (match-string 1 s))))
        (list (nvemacs--wall y 1 1 0 0 0) (nvemacs--wall y 12 31 23 59 59) 'year)))
     ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)\\'" s)
      (let ((y (string-to-number (match-string 1 s)))
            (mo (string-to-number (match-string 2 s))))
        (when (<= 1 mo 12)
          (list (nvemacs--wall y mo 1 0 0 0)
                (nvemacs--wall y mo (date-days-in-month y mo) 23 59 59) 'month))))
     ((string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" s)
      (let ((p (nvemacs--date-parts s)))
        (when p (list (car p) (+ (car p) 86399) 'day))))
     ((string-match-p (concat "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[-t ]"
                              "[0-9]\\{1,2\\}:[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\)?\\'")
                      s)
      (let ((p (nvemacs--date-parts (upcase s))))
        (when p
          (list (car p) (+ (car p) (if (= (length (split-string s ":")) 3) 0 59)) 'day)))))))

(defun nvemacs--wall-add (wall n unit end-of-month)
  "Return clock time WALL moved by N of UNIT: ?d, ?w, ?m or ?y.
A month step stops at the end of a shorter month.  END-OF-MONTH keeps
the result on the last day, for the end of a whole month or year."
  (if (memq unit '(?d ?w))
      (+ wall (* n (if (eq unit ?w) 7 1) 86400))
    (pcase-let* ((`(,sec ,mi ,h ,d ,mo ,y) (decode-time wall 0))
                 (total (+ (* y 12) (1- mo) (* n (if (eq unit ?y) 12 1))))
                 (ny (floor total 12))
                 (nm (1+ (mod total 12)))
                 (last (date-days-in-month ny nm)))
      (nvemacs--wall ny nm (if end-of-month last (min d last)) h mi sec))))

(defun nvemacs--date-range (v)
  "Read V, the text after `created:' and the like, as (LO . HI), or nil.
Both ends are included and clock times; nil for an end means open.
Forms: 2026-11, A..B, >=A, >A, <=A, <A, -30d (the last 30 days), A~1m
\(either side of A), A>1m (forward from A) and A<1m (back from A).  A
number with no unit takes A's precision: years, months or days."
  (cond
   ((string-match "\\`-\\([0-9]+\\)\\([dwmy]\\)\\'" v)
    (let ((n (string-to-number (match-string 1 v)))
          (unit (aref (match-string 2 v) 0))
          (today (nvemacs--date-anchor (format-time-string "%F"))))
      (cons (nvemacs--wall-add (car today) (- n) unit nil) (nth 1 today))))
   ((string-match "\\`\\(.+\\)\\.\\.\\(.+\\)\\'" v)
    ;; The strings first: reading a date resets the match data.
    (let* ((from (match-string 1 v)) (to (match-string 2 v))
           (a (nvemacs--date-anchor from))
           (b (nvemacs--date-anchor to)))
      (when (and a b)
        (cons (min (car a) (car b)) (max (nth 1 a) (nth 1 b))))))
   ((string-match "\\`\\(>=\\|<=\\|>\\|<\\)\\(.+\\)\\'" v)
    (let* ((op (match-string 1 v)) (a (nvemacs--date-anchor (match-string 2 v))))
      (when a
        (pcase op
          (">=" (cons (car a) nil))
          (">" (cons (1+ (nth 1 a)) nil))
          ("<=" (cons nil (nth 1 a)))
          (_ (cons nil (1- (car a))))))))
   ((string-match "\\`\\(.+?\\)\\([~<>]\\)\\([0-9]+\\)\\([dwmy]?\\)\\'" v)
    (let* ((from (match-string 1 v))
           (op (aref (match-string 2 v) 0))
           (n (string-to-number (match-string 3 v)))
           (given (match-string 4 v))
           (a (nvemacs--date-anchor from)))
      (when a
        (let* ((unit (if (string-empty-p given)
                         (pcase (nth 2 a) ('year ?y) ('month ?m) (_ ?d))
                       (aref given 0)))
               (whole (and (memq unit '(?m ?y)) (memq (nth 2 a) '(year month)) t)))
          (cons (if (eq op ?>) (car a) (nvemacs--wall-add (car a) (- n) unit nil))
                (if (eq op ?<) (nth 1 a) (nvemacs--wall-add (nth 1 a) n unit whole)))))))
   (t (let ((a (nvemacs--date-anchor v)))
        (and a (cons (car a) (nth 1 a)))))))

(defvar nvemacs--query-bad nil
  "The first date search of the last parsed query that could not be read.")

(defun nvemacs--query-item (kind v)
  "Return the filter for prefix KIND and value V, or nil while V is empty.
A tag is a string.  Any other filter is (KIND TEXT A B): TEXT as typed;
A and B the value or the two ends of a date range.  (bad TEXT) is a
date that cannot be read."
  (let ((text (format "%s:%s" kind v)))
    (pcase kind
      ('tag
       ;; Not `nvemacs--tag-normalize': a trailing `_' being typed
       ;; (`tag:alpha_') must narrow the list, not be trimmed.
       (let ((tg (replace-regexp-in-string
                  "\\`_+" "" (nvemacs--underscore v nvemacs--tag-chars))))
         (when (string-match-p "[^_]" tg) (nvemacs--fold tg))))
      ((or 'key 'meta 'dir) (unless (string-empty-p v) (list kind text v)))
      (_ (unless (string-empty-p v)
           (let ((r (nvemacs--date-range v)))
             (if r (list kind text (car r) (cdr r)) (list 'bad text))))))))

(defvar nvemacs--query-syntax nil
  "What the last parsed search held besides its terms, a plist.
:grouped   non-nil if it has parentheses
:empty     non-nil if a group in it is empty
:rejected  nil, or why it cannot run: `mixed' (OR and AND in one group)
           or `unclear' (a minus before an AND group, -(a b))
:warning   nil, or why it runs with a warning: `negative-or' (a minus
           inside OR, which matches almost every note) or `and-word'
           (AND typed in a group, searched as the word)")

(defun nvemacs--token-node (tk)
  "Return token TK, (TEXT . QUOTE-AT), as a search node, or nil.
\(word TEXT QUOTED) finds TEXT, (filter ITEM) is a prefix filter (see
`nvemacs--query-item') and (not NODE) leaves either out.  nil is a
token still being typed: a bare prefix or a lone `-'."
  (let ((tok (car tk)) (at (cdr tk)))
    (if (and (string-match "\\`\\(-?\\)\\(tag\\|key\\|meta\\|dir\\|created\\|added\\|modified\\):\\(.*\\)\\'" tok)
             (or (null at) (>= at (1+ (match-end 2)))))
        (let* ((neg (equal (match-string 1 tok) "-"))
               (kind (intern (match-string 2 tok)))
               (item (nvemacs--query-item kind (match-string 3 tok))))
          (when item
            (when (and (eq (car-safe item) 'bad) (not nvemacs--query-bad))
              (setq nvemacs--query-bad (nth 1 item)))
            (if neg (list 'not (list 'filter item)) (list 'filter item))))
      ;; `-whale' and `-"blue whale"' leave notes out.  In quotes the
      ;; minus is text, and so it is before anything but a letter, digit
      ;; or quote: --verbose, ->.  A lone `-' is still being typed.
      (cond
       ((or (not (string-prefix-p "-" tok)) (eql at 0)) (list 'word tok nil))
       ((= (length tok) 1) nil)
       ((or (eql at 1) (string-match-p "\\`-[[:alnum:]]" tok))
        (list 'not (list 'word (substring tok 1) (and at t))))
       (t (list 'word tok nil))))))

(defun nvemacs--query-tree (tokens)
  "Read TOKENS (see `nvemacs--query-tokens') as (and NODE ...).
A group is all OR or all AND: one with both is rejected, as is a minus
before an AND group, since either could be read two ways.  The notes go
in `nvemacs--query-syntax'."
  (let ((syntax nil))
    (cl-labels
        ((note (key value) (unless (plist-get syntax key) (setq syntax (plist-put syntax key value))))
         (read-items (in-group)
           ;; Return the segments between ORs: lists of nodes.
           (let ((segs nil) (seg nil) (done nil))
             (while (and tokens (not done))
               (let ((tk (pop tokens)))
                 (pcase tk
                   ('close (setq done t))
                   ('or (push (nreverse seg) segs) (setq seg nil))
                   ('and (note :warning 'and-word) (push (list 'word "and" nil) seg))
                   ((or 'open 'neg-open)
                    (let ((g (read-group (eq tk 'neg-open)))) (when g (push g seg))))
                   (_ (let ((nd (nvemacs--token-node tk))) (when nd (push nd seg)))))))
             (ignore in-group)
             (nreverse (cons (nreverse seg) segs))))
         (read-group (neg)
           (setq syntax (plist-put syntax :grouped t))
           (let ((segs (delq nil (read-items t))))
             (cond
              ((null segs) (note :empty t) nil)
              (t
               (when (and (cdr segs) (cl-some #'cdr segs)) (note :rejected 'mixed))
               (when (and neg (null (cdr segs)) (cdr (car segs))) (note :rejected 'unclear))
               (let ((node (if (cdr segs) (cons 'or (mapcar #'car segs)) (cons 'and (car segs)))))
                 (if neg (list 'not node) node)))))))
      (let ((top (cons 'and (apply #'append (read-items nil)))))
        (setq nvemacs--query-syntax syntax)
        top))))

(defun nvemacs--node-simplify (node)
  "Return NODE in its simplest form, or nil if nothing is left.
Empty groups go, a group of one is its member, a group inside one of
the same kind merges into it, and a double minus cancels."
  (pcase node
    (`(,(and op (or 'and 'or)) . ,kids)
     (let ((out nil))
       (dolist (k kids)
         (let ((s (nvemacs--node-simplify k)))
           (cond ((null s))
                 ((eq (car s) op) (setq out (append (reverse (cdr s)) out)))
                 (t (push s out)))))
       (setq out (nreverse out))
       (if (cdr out) (cons op out) (car out))))
    (`(not ,k)
     (let ((s (nvemacs--node-simplify k)))
       (cond ((null s) nil)
             ((eq (car s) 'not) (cadr s))
             (t (list 'not s)))))
    (_ node)))

(defun nvemacs--node-negative-p (node)
  "Non-nil if NODE matches notes by what they lack: almost every note."
  (pcase (car node)
    ('not t)
    ('and (cl-every #'nvemacs--node-negative-p (cdr node)))
    ('or (cl-some #'nvemacs--node-negative-p (cdr node)))))

(defun nvemacs--node-warn-negative (node)
  "Non-nil if NODE has an OR with a negative member, outside any minus."
  (pcase (car node)
    ('or (or (cl-some #'nvemacs--node-negative-p (cdr node))
             (cl-some #'nvemacs--node-warn-negative (cdr node))))
    ('and (cl-some #'nvemacs--node-warn-negative (cdr node)))))

(defun nvemacs--node-has-word-p (node)
  "Non-nil if NODE can match by a word, not only by filters or a minus."
  (pcase (car node)
    ('word t)
    ((or 'and 'or) (cl-some #'nvemacs--node-has-word-p (cdr node)))))

(defun nvemacs--node-cjk-p (node)
  "Non-nil if every note NODE matches holds a CJK word: see `nvemacs--route'."
  (pcase (car node)
    ('word (string-match-p nvemacs--cjk-regexp (nth 1 node)))
    ('and (cl-some #'nvemacs--node-cjk-p (cdr node)))
    ('or (cl-every #'nvemacs--node-cjk-p (cdr node)))))

(defun nvemacs--node-text (node)
  "Return NODE as it could be typed, for the status line."
  (pcase node
    (`(word ,w ,quoted) (if (or quoted (string-search " " w)) (format "\"%s\"" w) w))
    (`(filter ,item) (nvemacs--filter-text item))
    (`(not ,k) (concat "-" (nvemacs--node-text k)))
    (`(,op . ,kids)
     (concat "(" (mapconcat #'nvemacs--node-text kids (if (eq op 'or) " OR " " ")) ")"))))

(defun nvemacs--parse-query (query)
  "Split QUERY into (TERMS INCLUDE EXCLUDE).
TERMS are the text terms, a quoted phrase being one term.  INCLUDE and
EXCLUDE are the filters of the prefixes `tag:', `key:', `meta:', `dir:',
`created:', `added:' and `modified:', and of the same with `-' (see
`nvemacs--query-item').  EXCLUDE also holds negated words, (word TEXT
WORD).  A bare prefix or a lone `-' is ignored while it is being typed.
Any other colon, and anything quoted, is plain text.

A group that is still a group after simplifying, such as (a OR b), is
a node (see `nvemacs--token-node').  With a word to find anywhere in the
search, it goes in TERMS after the strings; otherwise it goes in INCLUDE
as (group TEXT NODE).  A search without parentheses parses as it did
before groups existed.  `nvemacs--query-syntax' gets the notes on
groups, and `nvemacs--query-bad' the first date that cannot be read."
  (setq nvemacs--query-bad nil)
  (let* ((tree (nvemacs--node-simplify (nvemacs--query-tree (nvemacs--query-tokens query))))
         (items (if (eq (car tree) 'and) (cdr tree) (and tree (list tree))))
         (terms nil) (inc nil) (exc nil) (groups nil))
    (cl-labels ((place (nd)
                  (pcase nd
                    (`(word ,w ,_) (push w terms))
                    (`(filter ,item) (push item inc))
                    (`(not (word ,w ,quoted)) (push (list 'word (if quoted (format "\"%s\"" w) w) w) exc))
                    (`(not (filter ,item)) (push item exc))
                    ;; -(a OR b) is -a -b.
                    (`(not (or . ,kids))
                     (dolist (k kids) (place (nvemacs--node-simplify (list 'not k)))))
                    (_ (push nd groups)))))
      (mapc #'place items))
    (when (and (null (plist-get nvemacs--query-syntax :warning))
               (cl-some #'nvemacs--node-warn-negative groups))
      (setq nvemacs--query-syntax (plist-put nvemacs--query-syntax :warning 'negative-or)))
    (setq groups (nreverse groups) terms (nreverse terms))
    (if (or terms (cl-some #'nvemacs--node-has-word-p groups))
        (list (append terms groups) (nreverse inc) (nreverse exc))
      (list terms
            (append (nreverse inc)
                    (mapcar (lambda (g) (list 'group (nvemacs--node-text g) g)) groups))
            (nreverse exc)))))

(defun nvemacs--filter-text (item)
  "Return search filter ITEM as the user typed it."
  (if (stringp item) (concat "tag:" item) (nth 1 item)))

(defvar nvemacs--dir-cache nil
  "(DB . FOLDERS) for `dir:'.  See `nvemacs--dir-folders'.")

(defun nvemacs--dir-folders ()
  "Return every folder holding a note, as (FOLDER FOLDED START).
FOLDER is as stored, ending in `/'; FOLDED is it folded for matching.
START is where in FOLDED the watched folder's own name begins, so a
partial name can be kept from matching a folder above it.  Reading every
path takes ~95 ms on 42,460 notes, so the list is kept for this index
and read again only when a note lands in a new folder
\(`nvemacs--dir-note-added'), not on every save.  A folder left empty
matches no note, so it can stay."
  (let ((db (nvemacs--db)))
    (unless (eq (car nvemacs--dir-cache) db)
      (let ((roots (sort (mapcar (lambda (r) (cons r (length (nvemacs--fold
                                                                (file-name-directory
                                                                 (directory-file-name r))))))
                                 (nvemacs--walk-roots))
                         (lambda (a b) (> (length (car a)) (length (car b))))))
            ;; In Lisp: SQLite's rtrim(path, ...) to drop filenames was 2.4x
            ;; slower (213 ms against 89 ms on 42,460 notes, 2026-11-24).
            (folders (let ((seen (make-hash-table :test #'equal)) (out nil))
                       (dolist (r (sqlite-select db "SELECT path FROM meta") (nreverse out))
                         (let ((f (file-name-directory (car r))))
                           (unless (gethash f seen) (puthash f t seen) (push f out)))))))
        (setq nvemacs--dir-cache
              (cons db (cl-mapcar
                                  (lambda (f folded)
                                    (list f folded
                                          (or (cdr (cl-find-if (lambda (r) (string-prefix-p (car r) f))
                                                               roots))
                                              (length folded))))
                                  folders
                                  ;; One fold for all: a temp buffer per folder costs more.
                                  (and folders
                                       (split-string (nvemacs--fold (mapconcat #'identity folders "\n"))
                                                     "\n")))))))
    (cdr nvemacs--dir-cache)))

(defun nvemacs--dir-note-added (db path)
  "Note PATH was indexed in DB: forget the folder list if PATH's folder is new."
  (when (and (eq (car nvemacs--dir-cache) db)
             (not (assoc (file-name-directory path) (cdr nvemacs--dir-cache))))
    (setq nvemacs--dir-cache nil)))

(defun nvemacs--dir-match-p (v folded start)
  "Non-nil if `dir:V' matches a folder, FOLDED, with its watched name at START.
A trailing `/' means whole names, which may be anywhere in the path.
Otherwise the last part of V starts a folder name at or inside the
watched folder, and any parts before it are the folders right above."
  (let ((s (concat "/" v)))
    (if (string-suffix-p "/" v)
        (string-search s folded)
      (let ((last (1+ (cl-position ?/ s :from-end t))) (from 0) (found nil) p)
        (while (and (not found) (setq p (string-search s folded from)))
          (when (>= (+ p last) start) (setq found t))
          (setq from (1+ p)))
        found))))

(defun nvemacs--dir-anchor (v)
  "Return `dir:V' as a folded full path when V starts with / or ~, else nil.
The folders in it are resolved through symlinks, as stored paths are."
  (when (string-match-p "\\`[/~]" v)
    (let* ((whole (or (string-suffix-p "/" v) (member v '("~"))))
           (full (expand-file-name v))
           (dir (if whole (file-name-as-directory full) (file-name-directory full)))
           (base (if whole "" (file-name-nondirectory full))))
      (nvemacs--fold (concat (file-name-as-directory
                              (if (file-directory-p dir) (file-truename dir) dir))
                             base)))))

(defun nvemacs--dir-condition (v)
  "Return (CONDITION . ARGS) on `path' for the notes `dir:V' matches.
Each matching folder is a range of paths, which covers its subfolders."
  (let* ((anchor (nvemacs--dir-anchor v))
         (hits (sort (cl-loop for (f folded start) in (nvemacs--dir-folders)
                              when (if anchor
                                       (string-prefix-p anchor folded)
                                     (nvemacs--dir-match-p v folded start))
                              collect f)
                     #'string<))
         (top nil))
    ;; A folder inside one already listed adds nothing.
    (dolist (f hits) (unless (and top (string-prefix-p (car top) f)) (push f top)))
    (if (null top)
        (cons "0" nil)
      (cons (mapconcat (lambda (_) "(path >= ? AND path <= ?)") top " OR ")
            (mapcan (lambda (f) (list f (concat f (string #x10FFFF)))) (nreverse top))))))

(defun nvemacs--filter-parts (item)
  "Return (TABLE COLUMNS CONDITION . ARGS) for filter ITEM, not a tag."
  (pcase item
    (`(dir ,_ ,v)
     (let ((c (nvemacs--dir-condition v)))
       `("meta" "path" ,(concat "(" (car c) ")") ,@(cdr c))))
    (`(key ,_ ,v)
     ;; The highest character ends every name starting with V.
     `("fields" "fkey" "fkey >= ? AND fkey <= ?" ,v ,(concat v (string #x10FFFF))))
    (`(meta ,_ ,v) `("fields" "folded" "instr(folded, ?) > 0" ,v))
    (`(,kind ,_ ,lo ,hi)
     (let ((mlo (if lo (nvemacs--date-moment (cons lo nil)) nvemacs--date-min))
           (mhi (if hi (nvemacs--date-moment (cons hi nil)) nvemacs--date-max))
           (wlo (or lo nvemacs--date-min))
           (whi (or hi nvemacs--date-max)))
       (pcase kind
         ;; As written when the note gives a date; a guess is a real moment.
         ('created
          `("meta" "created, cwall"
            "CASE WHEN cwall IS NULL THEN created BETWEEN ? AND ? ELSE cwall BETWEEN ? AND ? END"
            ,mlo ,mhi ,wlo ,whi))
         ('added `("meta" "added" "added BETWEEN ? AND ?" ,mlo ,mhi))
         (_ `("meta" "mtime" "mtime BETWEEN ? AND ?" ,mlo ,mhi)))))))

(defun nvemacs--filter-cond (item neg per-row)
  "Return (CONDITION . ARGS) true of `rowid' when filter ITEM matches it.
NEG non-nil leaves those notes out instead.  PER-ROW: see
`nvemacs--tag-filter'.  A date that cannot be read matches nothing; a
group (group TEXT NODE) is its node."
  (cond
   ((stringp item)
    (cons (format "rowid %s (SELECT id FROM tags WHERE substr(folded, 1, ?) = ?)"
                  (if neg "NOT IN" "IN"))
          (list (length item) item)))
   ((eq (car item) 'bad) (cons (if neg "1" "0") nil))
   ((eq (car item) 'group)
    (let ((s (nvemacs--node-sql (nth 2 item) (if per-row :row :list) 'body)))
      (if neg (cons (format "NOT (%s)" (car s)) (cdr s)) s)))
   ((eq (car item) 'word)
    ;; The body holds the full title too.  Text is folded, so `instr' is
    ;; an exact match.
    (let ((table (cdr (nvemacs--route (list (nth 2 item))))))
      (if per-row
          (cons (format "NOT EXISTS (SELECT 1 FROM (SELECT rowid AS note, x AS doc FROM %s) WHERE note = rowid AND instr(doc, ?) > 0)"
                        table)
                (list (nth 2 item)))
        (let ((match (nvemacs--term-where (list (nth 2 item)))))
          (cons (format "rowid NOT IN (SELECT rowid FROM %s WHERE %s)" table (car match))
                (cdr match))))))
   (t
    (pcase-let ((`(,table ,cols ,cond . ,vals) (nvemacs--filter-parts item)))
      (cons (if per-row
                (format "%sEXISTS (SELECT 1 FROM (SELECT id AS note, %s FROM %s) WHERE note = rowid AND %s)"
                        (if neg "NOT " "") cols table cond)
              (format "rowid %s (SELECT id FROM %s WHERE %s)" (if neg "NOT IN" "IN") table cond))
            vals)))))

(defun nvemacs--tag-filter (inc exc &optional per-row)
  "Return (FRAGMENT . ARGS), SQL that restricts `rowid' by the filters.
The notes kept match every filter of INC and none of EXC.  Tags and keys
match by the start of the name, so the list narrows while one is typed.
A date that cannot be read matches nothing.  FRAGMENT is empty or begins
with \" AND\".

PER-ROW non-nil, for a search with words or a check of one note: each
row looks up its own note, since words read the notes in slices and an
id list would be rebuilt for each (`whale meta:x' took 3.4 s on 42,460
notes, measured 2026-11-23).  Otherwise one id list is read: a rare
`key:' stays at milliseconds where a lookup per note took 470 ms.  The
inner SELECT renames `id' so that `rowid' is the outer query's row."
  (if (cl-some (lambda (i) (eq (car-safe i) 'bad)) (append inc exc))
      (cons " AND 0" nil)
    (let ((frag "") (args nil))
      (dolist (group `((nil . ,inc) (t . ,exc)))
        (dolist (item (cdr group))
          (let ((c (nvemacs--filter-cond item (car group) per-row)))
            (setq frag (concat frag " AND " (car c))
                  args (append args (cdr c))))))
      (cons frag args))))

(defun nvemacs--search-all (query)
  "Every rowid matching QUERY, newest first.  Reference path for tests."
  (pcase-let* ((`(,terms ,inc ,exc) (nvemacs--parse-query query))
               (filter (nvemacs--tag-filter inc exc terms)))
    (cond
     (terms
      (pcase-let ((`(,table ,where . ,args) (nvemacs--text-match terms 'body)))
        (mapcar #'car
                (sqlite-select (nvemacs--db)
                               (format "SELECT rowid FROM %s WHERE %s%s ORDER BY rowid DESC"
                                       table where (car filter))
                               (append args (cdr filter))))))
     ((or inc exc)
      (mapcar #'car (sqlite-select (nvemacs--db)
                                   (format "SELECT id FROM meta WHERE 1%s ORDER BY id DESC" (car filter))
                                   (cdr filter)))))))

;;;; Single-note update on save

(defun nvemacs--watched-note-p (path)
  "Non-nil if PATH is a note inside a watched directory."
  (and path
       (not (nvemacs--malformed-path-p path))
       (member (downcase (or (file-name-extension path) "")) nvemacs-import-extensions)
       (cl-some (lambda (d) (file-in-directory-p path (expand-file-name d)))
                nvemacs-watched-directories)))

(defun nvemacs--reindex (path &optional was)
  "Read note PATH again now and replace its index entry.
This happens always, even when size and mtime look unchanged: a tag
edit can keep both.  Only the note's row in the list changes: it moves
to the top (redrawing every listed row after each auto-save stalled
typing).  WAS is the path the note had, if it has just been moved."
  (when nvemacs--db
    (let* ((db nvemacs--db)
           (path (nvemacs--canonical-path path))
           (attrs (file-attributes path))
           (old (caar (sqlite-select db "SELECT id FROM meta WHERE path = ?"
                                     (list (if was (nvemacs--canonical-path was) path)))))
           (new nil))
      (condition-case err
          (let ((rec (nvemacs--read-note path attrs)))
            (nvemacs--with-transaction db
              (when old (nvemacs--delete-id db old))
              (setq new (nvemacs--insert-note db path attrs rec))
              (nvemacs--bump-generation db))
            (nvemacs--dates-flush)
            (setq nvemacs--skipped (assoc-delete-all path nvemacs--skipped))
            (nvemacs--list-update (list (cons old new)) t))
        (nvemacs-skip
         (push (cons path (cadr err)) nvemacs--skipped)
         (when was (nvemacs--forget was))
         (nvemacs--command-message
          "The search index"
          (format "requires a readable note, and %s was saved but not indexed: %s"
                  (nvemacs--short-path path)
                  (string-replace path (file-name-nondirectory path) (cadr err)))
          "Fix the note, or see M-x nvemacs-index-skipped"))))))

(defun nvemacs--forget (path)
  "PATH is gone (moved to the Trash by nvEmacs): drop its index entry and row."
  (when nvemacs--db
    (let ((old (caar (sqlite-select nvemacs--db "SELECT id FROM meta WHERE path = ?"
                                    (list (nvemacs--canonical-path path))))))
      (when old
        (nvemacs--with-transaction nvemacs--db
          (nvemacs--delete-id nvemacs--db old)
          (nvemacs--bump-generation nvemacs--db))
        (nvemacs--list-update (list (cons old nil)) t)))))

;;;; Search window

(defcustom nvemacs-search-delay 0.1
  "Idle seconds after the last keystroke before a search runs."
  :type 'number
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-grow-delay 0.5
  "Idle seconds between steps that lengthen the notelist.
Each time Emacs has been idle this many more seconds, the list grows by
one screenful, until every match is listed.  It also grows at once when
the cursor nears its last row.  nil means the list stays one screenful."
  :type '(choice (number :tag "Seconds") (const :tag "Never" nil))
  :package-version '(nvemacs . "0.0.2"))

(defconst nvemacs--buffer-name "*nvEmacs*")
(defconst nvemacs--prompt "Search: ")
(defconst nvemacs--slice-fast-ms 2.0 "Double the slice after a call faster than this.")
(defconst nvemacs--slice-slow-ms 8.0 "Halve the slice after a call slower than this.")
(defconst nvemacs--slice-start-fraction 1000 "First slice = rowid range / this.")

(defvar-local nvemacs--query-start nil "Marker: start of the query text.")
(defvar-local nvemacs--rows-start nil "Marker: start of the result rows.")
(defvar-local nvemacs--timer nil "Pending debounce timer.")
(defvar-local nvemacs--shown nil "Rowids shown, in display order.")
(defvar-local nvemacs--shown-set nil
  "Hash table of the rowids in `nvemacs--shown'.
It answers \"already shown?\" without walking the list (that walk grew
with the square of the rows: 47 ms of a 4,783-row redraw, measured
2026-11-20).")
(defvar-local nvemacs--status "" "Status text, shown in the mode line.")
(defvar-local nvemacs--status-base ""
  "The status text without the note naming a hidden sort column.")
(defvar-local nvemacs--rendered-size nil
  "(WIDTH . HEIGHT) of the window when rows were last drawn.")
(defvar-local nvemacs--selected-title nil
  "Rowid whose title the search line shows in place of the typed search.
nil when the search line shows what the user typed.")
(defvar-local nvemacs--typed nil
  "The typed search, kept while the search line shows a selected title.")
(defvar nvemacs--inhibit-search nil
  "Non-nil while nvEmacs itself rewrites the search line (no new search).")
(defvar-local nvemacs--selected-id nil
  "Rowid of the selected note, or nil.
Kept apart from point, which Emacs drags along when a window scrolls.")
(defvar-local nvemacs--view-free nil
  "Non-nil while the notelist's view has left the selection.
Point is then only where Emacs keeps it on screen, and means nothing.")
(defvar nvemacs--command-here nil
  "Non-nil while a command that started in the notelist runs.")

(defcustom nvemacs-show-selected-title t
  "Non-nil means the search line shows the title of the selected row.
Moving onto a result row shows that note's title on the search line, as
Notational Velocity does (AppController.m:1559-1571).  Typing replaces
the title with a new search; DEL, or moving back to the search line,
restores the search you typed."
  :type 'boolean
  :package-version '(nvemacs . "0.0.2"))

(defface nvemacs-selected-title '((t :inherit region))
  "Face of a selected note's title shown on the search line.")

(defvar-keymap nvemacs-mode-map
  :doc "Keymap of the nvEmacs search buffer."
  "RET" #'nvemacs-note-open
  "C-o" #'nvemacs-note-open-other-window
  "C-c C-c" #'nvemacs-searchbar-clear
  "M-p" #'nvemacs-searchbar-history-previous
  "M-n" #'nvemacs-searchbar-history-next
  "C-c C-q" #'nvemacs-tags-edit                   ; as in Org
  "C-c C-]" #'nvemacs-quit
  "C-c C-g" #'nvemacs-index-refresh
  "C-c C-v" #'nvemacs-layout-cycle
  "C-c C-l" #'nvemacs-notelist-refresh
  "C-c C-n" #'nvemacs-note-new
  "C-c C-r" #'nvemacs-note-rename
  "C-c C-w" #'nvemacs-note-move                   ; as Org's refile
  "C-c C-x C-a" #'nvemacs-note-move-outside       ; as Org's archive
  "C-c ^" #'nvemacs-notelist-sort-column          ; as Org's sort
  "C-c C-x C-c" #'nvemacs-notelist-column-toggle  ; as Org's columns
  "<tab-line> <mouse-1>" #'nvemacs--heading-click
  "<header-line> <mouse-1>" #'nvemacs--pinned-click
  ;; Mouse: nothing happens on the press; the release selects.  See
  ;; `nvemacs-notelist-mouse-select'.
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'nvemacs-notelist-mouse-select
  "<double-mouse-1>" #'nvemacs-notelist-mouse-select
  "<triple-mouse-1>" #'nvemacs-notelist-mouse-select
  "<drag-mouse-1>" #'nvemacs-notelist-mouse-select
  "TAB" #'nvemacs-notelist-next
  "<backtab>" #'nvemacs-notelist-previous
  ;; Search line: movement and basic query edits only.  Other editing
  ;; commands are refused, remapped by command so every key bound to them
  ;; is covered.  Result rows are read-only text anyway.
  "<remap> <delete-char>" #'nvemacs--refuse
  "<remap> <delete-forward-char>" #'nvemacs--refuse
  "<remap> <kill-line>" #'nvemacs--refuse
  "<remap> <kill-region>" #'nvemacs--refuse
  "<remap> <kill-word>" #'nvemacs--refuse
  "<remap> <transpose-chars>" #'nvemacs--refuse
  "<remap> <transpose-words>" #'nvemacs--refuse
  "<remap> <upcase-word>" #'nvemacs--refuse
  "<remap> <downcase-word>" #'nvemacs--refuse
  "<remap> <capitalize-word>" #'nvemacs--refuse
  "<remap> <undo>" #'nvemacs--refuse
  "<remap> <undo-only>" #'nvemacs--refuse
  "<remap> <undo-redo>" #'nvemacs--refuse)

(defun nvemacs-notelist-mouse-select (event)
  "Select the row clicked in EVENT, or put point on the search line.
Bound to the button RELEASE, with the press doing nothing.  With Emacs's
default press handler (`mouse-drag-region') the selected title replaced
the search line between press and release; if that line changes height
\(Hangul/Hanja fallback fonts are taller), every row shifts, and the
release lands on another row or starts a drag region (Hangul
rows could not be re-selected by mouse).  A click in the empty space
below the last row selects the list's window and changes nothing else."
  (interactive "e")
  (let ((pos (event-end event)))
    (if (nvemacs--below-rows-p pos)
        (progn (select-window (posn-window pos))
               (setq this-command 'ignore)) ; not a selection: see `nvemacs--selection-update'
      (mouse-set-point event)))
  (deactivate-mark))

(defun nvemacs--below-rows-p (position)
  "Return non-nil if the click POSITION is in the empty space below the list.
Emacs reports such a click at the end of the buffer, exactly as it does
one past the text of the last row; only its height on screen tells them
apart.  That is compared in pixels, since rows with Hangul are taller."
  (let ((win (posn-window position)))
    (and (windowp win)
         (eql (posn-point position) (with-current-buffer (window-buffer win) (point-max)))
         (let ((end (posn-at-point (posn-point position) win)))
           (and end
                (>= (cdr (posn-x-y position))
                    (+ (cdr (posn-x-y end))
                       (with-selected-window win
                         (save-excursion (goto-char (point-max))
                                         (line-pixel-height))))))))))

(defun nvemacs--refuse ()
  "Refuse an editing command in the search buffer."
  (interactive)
  (message "%s" (nvemacs--command-text
                 (or this-original-command this-command "This command")
                 "does not work on the search line"
                 "Type, or use DEL, M-DEL, C-y or C-c C-c, to edit the search")))

(define-derived-mode nvemacs-mode fundamental-mode "nvEmacs"
  "Notational Velocity style search over watched notes.
\\{nvemacs-mode-map}"
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t)                 ; no undo of redraws
  ;; The tab line holds the column headings (it never scrolls), so the header
  ;; line under it can pin the search line.  It looks like a header line, and
  ;; `global-tab-line-mode' must not take it.  The status is in the mode line.
  (setq-local tab-line-exclude t)
  (setq-local tab-line-format '(:eval (nvemacs--headings)))
  (setq-local face-remapping-alist (cons '(tab-line header-line) face-remapping-alist))
  (add-hook 'window-scroll-functions #'nvemacs--pin-scrolled nil t)
  (setq-local mode-line-format
              '("%e" mode-line-front-space mode-line-buffer-identification "  "
                (:eval nvemacs--status)))
  (add-hook 'pre-command-hook #'nvemacs--pre-command nil t)
  (add-hook 'after-change-functions #'nvemacs--after-change nil t)
  (add-hook 'window-size-change-functions #'nvemacs--window-resized nil t)
  (add-hook 'post-command-hook #'nvemacs--post-command nil t)
  (add-hook 'kill-buffer-hook #'nvemacs--search-killed nil t)
  (add-hook 'window-selection-change-functions #'nvemacs--selection-changed nil t)
  (require 'hl-line)
  (setq-local hl-line-range-function #'nvemacs--highlight-range)
  (hl-line-mode 1))

;;;; The selected note and the view
;;
;; Emacs keeps point on screen, so a scrolled window drags point along. In
;; most Emacs lists the line at point IS the selection, and scrolling changes
;; it. Here it must not: someone reading a note can scroll
;; the list to look at other titles. So the notelist holds two things:
;;   the selection  `nvemacs--selected-id', which keys and clicks move;
;;   the view       the window's scroll position, which the scroll bar, wheel
;;                  and trackpad move.
;; While they agree, point is on the selected row. Once the view has left it
;; (`nvemacs--view-free'), point is wherever Emacs put it and means nothing:
;; the cursor is hidden, the highlight stays on the selected row, and every
;; command asks `nvemacs--selection', never point.

(defvar hl-line-mode)
(defvar hl-line-range-function)
(declare-function hl-line-highlight "hl-line" ())
(defvar nvemacs--rest)                  ; defined with the notelist columns

(defconst nvemacs--typing-commands
  '(self-insert-command yank delete-backward-char backward-kill-word org-self-insert-command)
  "Commands that edit the search line from anywhere in the list.")

(defun nvemacs--selection ()
  "Return the id of the selected note, or nil.
While the list's window is selected and its view is on the selection,
that is the row at point.  Otherwise point may have been dragged by a
scroll, and the remembered note counts."
  (if (or nvemacs--view-free (not (eq (window-buffer) (current-buffer))))
      nvemacs--selected-id
    (and (not (nvemacs--in-query-p)) (nvemacs--row-id))))

(defun nvemacs--on-search-line-p ()
  "Return non-nil if nothing is selected: commands act from the search line."
  (if (or nvemacs--view-free (not (eq (window-buffer) (current-buffer))))
      (null nvemacs--selected-id)
    (nvemacs--in-query-p)))

(defun nvemacs--view-free-set (free)
  "Record whether the view has left the selection (FREE); if so, hide the cursor."
  (setq nvemacs--view-free free)
  (if free (setq-local cursor-type nil) (kill-local-variable 'cursor-type)))

(defun nvemacs--view-command-p (command)
  "Return non-nil if COMMAND moves only the view: scroll bar, wheel or trackpad."
  (and (symbolp command)
       (string-match-p "\\`\\(?:scroll-bar-\\|mwheel-\\|mac-mwheel-\\|pixel-scroll-\\)"
                       (symbol-name command))))

(defun nvemacs--motion-command-p (command)
  "Return non-nil if COMMAND is a keyboard move.
Emacs marks its movement commands with `^' in their interactive spec;
that covers `next-line', `previous-line', `scroll-up-command' and
`scroll-down-command', whichever keys they are on."
  (or (memq command '(nvemacs-notelist-next nvemacs-notelist-previous
                      isearch-forward isearch-backward
                      isearch-forward-regexp isearch-backward-regexp))
      (and (symbolp command) (fboundp command)
           (let ((spec (cadr (interactive-form command))))
             (and (stringp spec) (string-prefix-p "^" spec))))))

(defun nvemacs--fully-visible-p (pos window)
  "Return non-nil if POS is on a line WINDOW shows in full.
Moving point to a partly shown line would make Emacs scroll."
  (let ((v (pos-visible-in-window-p pos window t)))
    (and v (= (length v) 2))))

(defun nvemacs--view-settle ()
  "Keep the selection after the view moved.
If the selected row, or with none the search line, is on screen, the
cursor goes back to it.  Otherwise the view is free of the selection."
  (let* ((id nvemacs--selected-id)
         (home (if id (nvemacs--row-pos id) (marker-position nvemacs--query-start)))
         (there (if id (eql (nvemacs--row-id) id) (nvemacs--in-query-p))))
    (cond (there (nvemacs--view-free-set nil))
          ((and home (nvemacs--fully-visible-p home (selected-window)))
           (goto-char home)
           (unless id (end-of-line))
           (nvemacs--view-free-set nil))
          (t (nvemacs--view-free-set t)))))

(defun nvemacs--view-return ()
  "Before a key moves the selection, go to the selected row and show it.
With nothing selected there is nowhere to return to: the key acts on
the rows on screen."
  (let ((id nvemacs--selected-id))
    (nvemacs--view-free-set nil)
    (when id
      (nvemacs--show-id id)
      (when (and (eql (nvemacs--row-id) id) (eq (window-buffer) (current-buffer)))
        (set-window-start (selected-window)
                          (save-excursion (forward-line (- (/ (window-body-height) 2)))
                                          (point)))))))

(defun nvemacs--selection-update ()
  "Work out what the command just run did to the selection.
A click selects.  A view move, or the cursor arriving from another
window, leaves it alone.  Any other command that ran here moves it to
the row at point."
  (cond
   ((eq this-command 'nvemacs-notelist-mouse-select)
    (nvemacs--view-free-set nil)
    (setq nvemacs--selected-id (and (not (nvemacs--in-query-p)) (nvemacs--row-id))))
   ((or nvemacs--view-free (nvemacs--view-command-p this-command)
        (and (not nvemacs--command-here) (not (nvemacs--in-query-p))
             (not (nvemacs--motion-command-p this-command))))
    (nvemacs--view-settle))
   (t (setq nvemacs--selected-id (and (not (nvemacs--in-query-p)) (nvemacs--row-id)))))
  ;; `hl-line-mode' runs before this hook.
  (when hl-line-mode (hl-line-highlight)))

(defun nvemacs--selection-restore (id free starts)
  "Put the selection ID back after the rows were drawn again.
With FREE the view returns to its first lines, STARTS: ((WINDOW . LINE) ...)."
  (let ((listed (and id (or (and nvemacs--shown-set (gethash id nvemacs--shown-set))
                            (memq id nvemacs--rest)))))
    (cond
     (free
      (setq nvemacs--selected-id (and listed id))
      (pcase-dolist (`(,w . ,line) starts)
        (let ((pos (save-excursion (goto-char (point-min)) (forward-line (1- line)) (point))))
          (set-window-start w pos)
          (set-window-point w pos))))
     (id
      (nvemacs--show-id id)
      (if (eql (nvemacs--row-id) id)
          (progn (setq nvemacs--selected-id id)
                 (dolist (w (get-buffer-window-list (current-buffer) nil t))
                   (set-window-point w (point))))
        (setq nvemacs--selected-id nil)
        (goto-char nvemacs--query-start)
        (end-of-line))))))

(defun nvemacs--highlight-range ()
  "Return the span `hl-line-mode' marks: the selected row.
While the view is free of the selection that row may be off screen,
and with nothing selected nothing is marked."
  (if nvemacs--view-free
      (let ((p (and nvemacs--selected-id (nvemacs--row-pos nvemacs--selected-id))))
        (and p (cons p (save-excursion (goto-char p) (line-beginning-position 2)))))
    (cons (line-beginning-position) (line-beginning-position 2))))

;;;; The pinned search line
;;
;; The search line is the buffer's first line, so it scrolls with the rows.
;; While a window has scrolled it off, that window's header line shows a
;; read-only copy in the same place on screen.  The real line stays the one
;; that is edited: typing moves point there, and the list comes back to it.
;; It is a window parameter because two frames can be scrolled differently.

(defconst nvemacs--pin-format '(:eval (nvemacs--pinned-search))
  "A window's header line while its search line is scrolled off.")

(defun nvemacs--pinned-search ()
  "Return the search line as header-line text.
It keeps the line's faces, over the normal text's rather than the
header line's, and fills the row."
  (let ((text (string-replace
               "%" "%%"
               (buffer-substring (point-min)
                                 (save-excursion (goto-char nvemacs--query-start)
                                                 (line-end-position))))))
    (add-face-text-property 0 (length text) 'default t text)
    (concat text (propertize " " 'face 'default 'display '(space :align-to right)))))

(defun nvemacs--pinned-p (win)
  "Return non-nil if WIN's header line holds the pinned search line."
  (eq (window-parameter win 'header-line-format) nvemacs--pin-format))

(defun nvemacs--pin-wanted-p (win &optional start)
  "Return non-nil if WIN shows the list from START, past its search line.
START defaults to the window's start."
  (let ((list (get-buffer nvemacs--buffer-name)))
    (and (eq (window-buffer win) list)
         (let ((rows (buffer-local-value 'nvemacs--rows-start list)))
           (and rows (>= (or start (window-start win)) rows))))))

(defun nvemacs--pin-sync (win)
  "Show or hide the pinned search line in WIN, as its scroll position needs.
A `pre-redisplay-functions' entry while nvEmacs runs.  It is global so
that the copy also leaves a window that stops showing the list."
  (let ((want (nvemacs--pin-wanted-p win)))
    (unless (eq (and want t) (nvemacs--pinned-p win))
      (set-window-parameter win 'header-line-format (and want nvemacs--pin-format)))))

(defun nvemacs--pin-refresh (win)
  "Bring WIN's pinned search line up to date and draw it."
  (when (window-live-p win)
    (nvemacs--pin-sync win)
    (force-window-update win)))

(defun nvemacs--pin-scrolled (win start)
  "Follow a new START that redisplay chose for WIN.
A buffer-local `window-scroll-functions' entry.  This runs too late to
change the window being drawn, so the change waits for a timer."
  (unless (eq (and (nvemacs--pin-wanted-p win start) t) (nvemacs--pinned-p win))
    (run-at-time 0 nil #'nvemacs--pin-refresh win)))

(defun nvemacs--pinned-click (event)
  "Go to the search line after a click on its pinned copy in EVENT."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (nvemacs--focus-query))

(defvar nvemacs--away nil "Non-nil once the cursor left the list for another window.")

(defun nvemacs--selection-changed (win)
  "Rescan the folders when the cursor comes back to the list.
This is a buffer-local `window-selection-change-functions' of
*nvEmacs*; WIN is its window, just selected or deselected.  Coming back
to the list from another window (a note, F8, a click) rescans the
folders at the next idle moment: another program may have rewritten a
note in place while Emacs kept focus, which no folder event reports.  A
trip to the minibuffer does not count."
  (if (eq win (selected-window))
      (when nvemacs--away
        (setq nvemacs--away nil)
        (nvemacs--sync-soon 0))
    (unless (minibufferp (window-buffer (selected-window)))
      (setq nvemacs--away t))))

(defun nvemacs--query ()
  "Current query text."
  (buffer-substring-no-properties nvemacs--query-start
                                  (save-excursion (goto-char nvemacs--query-start)
                                                  (line-end-position))))

(defun nvemacs--in-query-p (&optional pos)
  "Return non-nil if POS, by default point, is on the search line."
  (let ((pos (or pos (point))))
    (and nvemacs--query-start           ; nil while the buffer is being set up
         (>= pos nvemacs--query-start)
         (<= pos (save-excursion (goto-char nvemacs--query-start) (line-end-position))))))

(defun nvemacs--search-text ()
  "The search to run: the typed search, even while a title is shown."
  (if nvemacs--selected-title nvemacs--typed (nvemacs--query)))

(defun nvemacs--replace-query (text)
  "Replace the search line's text with TEXT without starting a search."
  (let ((inhibit-read-only t) (nvemacs--inhibit-search t))
    (save-excursion
      (goto-char nvemacs--query-start)
      (delete-region (point) (line-end-position))
      (insert text))))

(defun nvemacs--restore-typed ()
  "Put the typed search back on the search line."
  (when nvemacs--selected-title
    (setq nvemacs--selected-title nil)
    (nvemacs--replace-query (or nvemacs--typed ""))))

(defun nvemacs--show-selected-title ()
  "Show the title of the row at point on the search line.
Run after each command.  When point is not on a row, restore the typed
search (Notational Velocity's behaviour)."
  (let ((id (nvemacs--selection)))
    (cond
     ((and id (not (eql id nvemacs--selected-title)))
      (unless nvemacs--selected-title (setq nvemacs--typed (nvemacs--query)))
      (nvemacs--put-selected-title id))
     ((null id) (nvemacs--restore-typed)))))

(defun nvemacs--put-selected-title (id)
  "Show note ID's title on the search line in place of the typed search.
Only the match (bold) part: a paragraph-long title would fill the search
line."
  (let ((title (caar (sqlite-select (nvemacs--db) "SELECT title FROM meta WHERE id = ?" (list id)))))
    (nvemacs--replace-query (propertize (car (nvemacs--match-title (or title "")))
                                        'face 'nvemacs-selected-title))
    (setq nvemacs--selected-title id)))

(defun nvemacs--pre-command ()
  "Make typing anywhere in the list edit the search line.
Run before each command.  If a query-editing command runs while point
is on the result rows, move point to the end of the query.  While a
selected title is shown, typing or yanking replaces it (a new search);
DEL and M-DEL restore the typed search and do nothing else.  With the
view scrolled away from the selection, a keyboard move first returns to
it (see `nvemacs--view-return')."
  (setq nvemacs--command-here t)
  (when nvemacs--view-free
    (cond ((memq this-command nvemacs--typing-commands)
           (setq nvemacs--selected-id nil)
           (nvemacs--view-free-set nil))
          ((nvemacs--motion-command-p this-command) (nvemacs--view-return))))
  (when (memq this-command nvemacs--typing-commands)
    (unless (nvemacs--in-query-p)
      (goto-char nvemacs--query-start)
      (end-of-line))
    (when nvemacs--selected-title
      (if (memq this-command '(delete-backward-char backward-kill-word))
          (progn (nvemacs--restore-typed)
                 (goto-char nvemacs--query-start) (end-of-line)
                 (setq this-command #'ignore))
        ;; Typing over a shown title is a new search.
        (nvemacs--history-end 'new-search)
        (setq nvemacs--selected-title nil nvemacs--typed nil)
        (nvemacs--replace-query "")
        (goto-char nvemacs--query-start)))))

(defun nvemacs--schedule-search ()
  "Run the search after `nvemacs-search-delay', restarting any pending one."
  (when nvemacs--timer (cancel-timer nvemacs--timer))
  (let ((buf (current-buffer)))
    (setq nvemacs--timer
          (run-with-timer nvemacs-search-delay nil
                          (lambda () (when (buffer-live-p buf)
                                       (with-current-buffer buf (nvemacs--search))))))))

(defun nvemacs--after-change (beg end len)
  "Restart the debounce timer when the query changes.
An `after-change-functions' entry: BEG and END are the new text, and LEN
the length of the text it replaced.  The search history sees each edit."
  (when (and (not nvemacs--inhibit-search) (nvemacs--in-query-p beg))
    (nvemacs--history-edit (< (- end beg) len))
    ;; A new search selects nothing: blank the pane at once, so a search
    ;; with no matches never sits beside an unrelated note.
    (nvemacs--clear-pane)
    (nvemacs--schedule-search)))

(defun nvemacs--window-size (&optional win)
  "Return (WIDTH . HEIGHT), the body size of WIN in columns and lines.
WIN defaults to the window showing the current buffer.  Return nil if
there is no such window.  The pinned search line's row counts as body:
it comes and goes with scrolling, which is not a new window size."
  (let ((win (or win (get-buffer-window (current-buffer)))))
    (and win (cons (window-body-width win)
                   (+ (window-body-height win) (if (nvemacs--pinned-p win) 1 0))))))

(defun nvemacs--window-resized (win)
  "Search again when the nvEmacs window WIN has a new size.
This is a buffer-local `window-size-change-functions'.  When the
window's size differs from the size the rows were drawn for, the rows
are laid out again for the new width and the screen fills the new
height (maximizing the frame must refresh the list)."
  (with-current-buffer (window-buffer win)
    (unless (equal (nvemacs--window-size win) nvemacs--rendered-size)
      (run-at-time 0 nil (lambda () (with-selected-frame (window-frame win)
                                      (nvemacs--maybe-relayout))))
      (nvemacs--schedule-search))))

(defun nvemacs--setup-buffer ()
  "Create the prompt, query line and empty row area."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; `field' makes C-a stop after the prompt, as in the minibuffer.
    (insert (propertize nvemacs--prompt 'read-only t 'front-sticky t
                        'rear-nonsticky t 'face 'minibuffer-prompt
                        'field 'nvemacs-prompt))
    (setq nvemacs--query-start (point-marker))
    ;; The newline ending the query line is read-only but NOT front-sticky,
    ;; so text can be typed in front of it.
    (insert (propertize "\n" 'read-only t))
    (setq nvemacs--rows-start (point-marker))
    (set-marker-insertion-type nvemacs--rows-start nil)
    (goto-char nvemacs--query-start)))

(defun nvemacs--screen-rows ()
  "Rows that fit in the window below the query line."
  (let ((win (get-buffer-window (current-buffer))))
    (max 5 (- (if win (window-body-height win) 40) 2))))

(defun nvemacs--fit (string width)
  "Return STRING cut to WIDTH columns.
It is first cut to 4 x WIDTH characters, before `string-width' runs: in
a GUI, `string-width' is about 20 times slower than in batch."
  (let ((s (if (> (length string) (* 4 width)) (substring string 0 (* 4 width)) string)))
    (truncate-string-to-width s width nil nil "…")))

;;;; Notelist row and columns
;;
;; Columns in the order of `nvemacs-notelist-columns', 2 columns apart:
;; dates have fixed widths, TAGS is the measured column, and the title cell
;; (bold title, then a grey one-line preview, as in Notational Velocity)
;; takes the rest. The tags width is measured when the notelist is first
;; drawn (unless remembered) and again only by `nvemacs-notelist-refresh' —
;; never by searches or growth (widths changing under the reader are
;; disorienting).

(defcustom nvemacs-notelist-columns '(title tags modified)
  "Notelist columns at launch, and their order.
A list of symbols, or a string of comma-separated names, such as
\"modified, tags, title, added\".  Names: title, tags, created, added,
modified.  The title is always shown.  Created is the date a note
declares, or else its Added date marked `~'.  Added is the earlier of
the file's creation and modified dates when nvEmacs first found it.
`nvemacs-notelist-column-toggle' changes the columns for the session."
  :type '(choice (repeat (choice (const title) (const tags) (const created)
                                 (const added) (const modified)))
                 string)
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-preview t
  "Non-nil means show each note's body as one grey line after its title.
This is Notational Velocity's \"show note body previews\"."
  :type 'boolean
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-title-min-width 20
  "Narrowest title column; narrower windows drop columns instead."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-tags-min-width 20
  "Narrowest tags column; with less room the tags column is hidden."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-tags-max-width 40
  "Widest tags column a measurement may give; longer tags end in ….
nil: no limit."
  :type '(choice (const :tag "No limit" nil) integer)
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-remember-widths 'last
  "How the notelist tags width is kept between sessions.
`last': saved on exit and on \\[nvemacs-notelist-refresh] into state.eld,
  used at the next start; `never': nothing saved, each start measures its
  first screen; `user': a fixed width only, from `nvemacs-notelist-tags-width'
  or `nvemacs-notelist-widths-set', else measured each time.  With a fixed
  width, a refresh lasts for the session only."
  :type '(choice (const last) (const never) (const user))
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-tags-width nil
  "A fixed tags column width.  Set here, it wins over every other width.
Leave it unset to have \\[nvemacs-notelist-widths-set] keep a width in
settings.eld instead."
  :type '(choice (const :tag "Measure" nil) integer)
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-state-file nil
  "Remembered values (state.eld); nil = in `nvemacs-data-directory'."
  :type '(choice (const nil) file)
  :package-version '(nvemacs . "0.0.2"))

(defface nvemacs-notelist-title '((t :weight bold))
  "The title's match part in the notelist (what counts as a title match).")

(defface nvemacs-notelist-preview '((t :inherit shadow))
  "The rest of a long title, the body preview and (untitled) in the notelist.")

(defface nvemacs-tags '((t :inherit shadow))
  "Face of the tags column in the nvEmacs list.")

(defvar nvemacs--tags-width nil
  "Tags column width in use (one for all frames); nil until measured.")

(defvar-local nvemacs--layout nil
  "Columns of the last draw: ((COLUMN START WIDTH) ...).")

(defconst nvemacs--column-names '(title tags created added modified)
  "Every column, in the standard order.")

(defvar nvemacs--columns-warned nil
  "The `nvemacs-notelist-columns' value last warned about (warn once).")

(defvar nvemacs--columns-now nil
  "The columns the toggles set for this session; nil: the preference.")

(defvar nvemacs--sort (cons 'modified 'desc)
  "The notelist's sort: (COLUMN . DIRECTION), DIRECTION `asc' or `desc'.")

(defvar-local nvemacs--sort-keys nil
  "Hash table ID -> sort key of every listed note, once its order is known.
nil while the list is read newest first, a screenful at a time.")

(defvar-local nvemacs--rest nil
  "Ids of the listed notes not drawn yet, in list order.
Used when `nvemacs--sort-keys' is set.")

(defvar-local nvemacs--list-stale nil
  "Non-nil while a stopped sort pass waits to run again.
The rows on screen then belong to the previous search, so none are added.")

(defun nvemacs--notelist-column-list ()
  "The columns to draw: this session's toggles, or the preference."
  (or nvemacs--columns-now (nvemacs--notelist-column-preference)))

(defun nvemacs--notelist-column-preference ()
  "`nvemacs-notelist-columns' as a list of known symbols, title included."
  (let* ((v nvemacs-notelist-columns)
         (names (mapcar (lambda (n) (intern (downcase (string-trim (format "%s" n)))))
                        (if (stringp v) (split-string v "," t "[ \t]+") v)))
         (out nil) (problems nil))
    (dolist (n names)
      (cond ((memq n out))
            ((memq n nvemacs--column-names) (push n out))
            (t (push (format "unknown column `%s' ignored" n) problems))))
    (setq out (nreverse out))
    (unless (memq 'title out)
      (push "no title column; it is added first" problems)
      (push 'title out))
    (when (and problems (not (equal v nvemacs--columns-warned)))
      (setq nvemacs--columns-warned v)
      (display-warning 'nvemacs (format "nvemacs-notelist-columns: %s"
                                        (mapconcat #'identity (nreverse problems) "; "))))
    out))

(defun nvemacs--notelist-layout (width tags cols)
  "Columns COLS laid out in a row WIDTH columns wide with tags TAGS wide:
\((COLUMN START WIDTH) ...).  Dates are fixed (modified and added 10,
created 11 for its `~'); the title takes the rest.  A title narrower than
`nvemacs-notelist-title-min-width' first shrinks tags toward their
minimum, then drops tags, created, added, modified in that order."
  (let* ((cs cols)
         (fixed (lambda (c) (pcase c ((or 'modified 'added) 10) ('created 11) ('tags tags) (_ 0))))
         (title-w (lambda () (- width (apply #'+ (mapcar fixed (remq 'title cs)))
                                (* 2 (1- (length cs))))))
         (tmin nvemacs-notelist-title-min-width))
    (when (and (memq 'tags cs) (< (funcall title-w) tmin))
      (setq tags (max nvemacs-notelist-tags-min-width (- tags (- tmin (funcall title-w))))))
    (dolist (c '(tags created added modified))
      (when (and (memq c cs) (< (funcall title-w) tmin))
        (setq cs (remq c cs))))
    (let ((x 0) (out nil))
      (dolist (c cs)
        (let ((w (if (eq c 'title) (max 1 (funcall title-w)) (funcall fixed c))))
          (push (list c x w) out)
          (setq x (+ x w 2))))
      (nreverse out))))

(defun nvemacs--tags-text (id)
  "The tags of note ID as the tags column shows them: file tags first."
  (mapconcat #'car (sqlite-select (nvemacs--db)
                                  "SELECT tag FROM tags WHERE id = ? ORDER BY kind = 'section', rowid"
                                  (list id))
             " "))

(defun nvemacs--measure-tags (ids)
  "Widest tags of notes IDS in display columns, within [tags-min, tags-max]."
  (let ((w (apply #'max 0 (mapcar (lambda (id) (string-width (nvemacs--fit (nvemacs--tags-text id) 200)))
                                  ids))))
    (max nvemacs-notelist-tags-min-width
         (if nvemacs-notelist-tags-max-width (min w nvemacs-notelist-tags-max-width) w))))

(defun nvemacs--title-cell (title preview width)
  "Return the title column's text for a note.
TITLE's match part is in bold, and the rest of a long title is in grey
after a space; then come a grey \" — \" and PREVIEW (Notational Velocity:
NSString_CustomTruncation.m attributedSingleLinePreviewFromBodyText).
The text is cut to WIDTH columns with ….  A note without a title shows
a grey (untitled)."
  (let* ((parts (nvemacs--match-title (or title "")))
         (match (car parts))
         (rest (cdr parts))
         (head (if (string-empty-p match)
                   (propertize "(untitled)" 'face 'nvemacs-notelist-preview)
                 (concat (propertize match 'face 'nvemacs-notelist-title)
                         (if (string-empty-p rest) ""
                           (propertize (concat " " rest) 'face 'nvemacs-notelist-preview))))))
    (nvemacs--fit (if (and nvemacs-notelist-preview preview (not (string-empty-p preview)))
                      (concat head (propertize (concat " — " preview) 'face 'nvemacs-notelist-preview))
                    head)
                  width)))

(defun nvemacs--date-text (secs)
  "Return SECS, a time in seconds, as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (seconds-to-time secs)))

(defun nvemacs--cell (entry _id row)
  "Return the text of column ENTRY for a note whose data is ROW.
ENTRY is (COLUMN START WIDTH), and ROW is (TITLE MTIME PREVIEW CREATED
CGUESS TAGS-TEXT ADDED CWALL).  A Created date shows as written (CWALL),
so it reads the same in any time zone.  Every cell after the first
starts at its column by :align-to (pixels: fallback CJK fonts draw
Hangul narrower than the 2 columns `string-width' counts)."
  (pcase-let ((`(,col ,start ,w) entry))
    (concat (if (zerop start) "" (propertize " " 'display `(space :align-to ,start)))
            (pcase col
              ('title (nvemacs--title-cell (nth 0 row) (nth 2 row) w))
              ('tags (propertize (nvemacs--fit (or (nth 5 row) "") w) 'face 'nvemacs-tags))
              ('modified (nvemacs--date-text (/ (float (nth 1 row)) nvemacs--rowid-units-per-second)))
              ('created (if (nth 3 row)
                            (concat (if (eql (nth 4 row) 1) "~" " ")
                                    (if (nth 7 row)
                                        (format-time-string "%Y-%m-%d" (nth 7 row) t)
                                      (nvemacs--date-text (nth 3 row))))
                          ""))
              ('added (if (nth 6 row) (nvemacs--date-text (nth 6 row)) ""))))))

(defun nvemacs--sort-triangle ()
  "▲ for A to Z or oldest first, ▼ for Z to A or newest first."
  (if (eq (cdr nvemacs--sort) 'asc) "▲" "▼"))

(defun nvemacs--heading (column)
  "Return the heading of COLUMN, with a triangle if the list is sorted by it."
  (propertize (concat (pcase column
                        ('title "Title") ('tags "Tags") ('created " Created")
                        ('added "Added") ('modified "Modified"))
                      (if (eq column (car nvemacs--sort))
                          (concat " " (nvemacs--sort-triangle))
                        ""))
              'nvemacs-column column
              'mouse-face 'highlight
              'help-echo "mouse-1: sort by this column; again to reverse"))

(defun nvemacs--headings ()
  "Return the tab line: the column headings.
They sit at the column positions of the last draw, with a triangle on
the sort column."
  (mapconcat (lambda (e)
               (concat (propertize " " 'display `(space :align-to ,(nth 1 e)))
                       (nvemacs--heading (car e))))
             nvemacs--layout ""))

(defun nvemacs--state-file ()
  "Return the path of the state file.
That is `nvemacs-state-file', or state.eld in `nvemacs-data-directory'."
  (or nvemacs-state-file (expand-file-name "state.eld" nvemacs-data-directory)))

(defun nvemacs--state-read ()
  "The alist in state.eld, or nil (missing or unreadable)."
  (let ((f (nvemacs--state-file)))
    (when (file-readable-p f)
      (ignore-errors
        (with-temp-buffer (insert-file-contents f) (read (current-buffer)))))))

(defun nvemacs--state-write (alist)
  "Write ALIST to state.eld (temp + rename)."
  (let ((f (nvemacs--state-file)))
    (nvemacs--private-directory (file-name-directory f))
    (with-temp-buffer
      (insert ";; nvEmacs remembered values (written by nvEmacs; safe to delete).\n")
      (prin1 alist (current-buffer))
      (insert "\n")
      (nvemacs--write-file f))))

(defcustom nvemacs-settings-file nil
  "Choices made in nvEmacs (settings.eld); nil = in `nvemacs-data-directory'.
Unlike state.eld, keep this file: it holds the watched folders."
  :type '(choice (const nil) file)
  :package-version '(nvemacs . "0.0.5"))

(defun nvemacs--settings-file ()
  "Return the path of settings.eld.
nvEmacs keeps the choices made in it there, never in the init file."
  (or nvemacs-settings-file (expand-file-name "settings.eld" nvemacs-data-directory)))

(defvar nvemacs--settings-read-only nil
  "Non-nil when settings.eld could not be read and the user chose to go on.
For the rest of the session nvEmacs reads no settings from it and writes
none to it, so the damaged file cannot be replaced.")

(defun nvemacs--settings-parse (f)
  "Read settings file F: (ok . ALIST), or (damaged . REASON).
A file holding only comments has nothing kept yet.  Anything other than
one list of (KEY . VALUE) pairs is damaged."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table) ; `forward-comment' skips ;; lines
    (condition-case err
        (progn
          (insert-file-contents f)
          (goto-char (point-min))
          (forward-comment (buffer-size))
          (if (eobp)
              (cons 'ok nil)
            (let ((v (read (current-buffer))))
              (forward-comment (buffer-size))
              (cond
               ((not (eobp))
                (cons 'damaged (format "more text follows the settings, at line %d"
                                       (line-number-at-pos))))
               ((not (and (proper-list-p v)
                          (cl-every (lambda (e) (and (consp e) (symbolp (car e)))) v)))
                (cons 'damaged "it does not hold a list of settings"))
               (t (cons 'ok v))))))
      (error (cons 'damaged (format "%s, at line %d" (error-message-string err)
                                    (line-number-at-pos)))))))

(defun nvemacs--settings-damaged-name (f)
  "A new name beside F for a damaged settings file: settings-damaged-STAMP.eld."
  (let* ((dir (file-name-directory f))
         (stem (concat "settings-damaged-" (format-time-string "%Y-%m-%d-%Hh%Mm")))
         (name (expand-file-name (concat stem ".eld") dir))
         (n 1))
    (while (file-exists-p name)
      (setq n (1+ n) name (expand-file-name (format "%s-%d.eld" stem n) dir)))
    name))

(defun nvemacs--settings-damaged (f reason)
  "Ask what to do with settings file F, unreadable because of REASON.
Return nil: no settings come from it.  Moving it aside keeps its text
under a new name and lets a new F be written.  Going on makes the
session read-only for settings (`nvemacs--settings-read-only').
Quitting signals a `user-error' and leaves F alone.  A batch run cannot
be asked, so it goes on read-only."
  (let* ((new (nvemacs--settings-damaged-name f))
         (key (if noninteractive
                  ?l
                (car (read-multiple-choice
                      (format "Settings file %s cannot be read (%s).  It holds your watched folders.  Move it to %s and start a new one, load without settings, or quit"
                              (abbreviate-file-name f) reason (file-name-nondirectory new))
                      `((?m "move" ,(format "Move it to %s and start a new settings.eld"
                                            (file-name-nondirectory new)))
                        (?l "load without settings"
                            "Go on without settings; nothing is written to settings.eld this session")
                        (?q "quit" "Stop, and leave the file alone")))))))
    (pcase key
      (?m (rename-file f new)
          (message "nvEmacs: moved the unreadable settings.eld to %s; a new one starts now"
                   (abbreviate-file-name new))
          nil)
      (?l (setq nvemacs--settings-read-only t)
          (message "nvEmacs: %s cannot be read (%s); this session reads and writes no settings"
                   (abbreviate-file-name f) reason)
          nil)
      (_ (nvemacs--command-error
          (or this-command 'nvemacs)
          (format "requires a readable settings.eld, and %s could not be read (%s), so it is left as it is"
                  (abbreviate-file-name f) reason)
          "Fix or move that file, then re-run the command")))))

(defun nvemacs--settings-read ()
  "The alist in settings.eld, or nil when nothing is kept.
A file that exists but cannot be read is never treated as empty, which
would replace it at the next change: `nvemacs--settings-damaged' asks."
  (let ((f (nvemacs--settings-file)))
    (cond
     (nvemacs--settings-read-only nil)
     ((not (file-exists-p f)) nil)
     (t (let ((r (nvemacs--settings-parse f)))
          (if (eq (car r) 'ok)
              (cdr r)
            (nvemacs--settings-damaged f (cdr r))))))))

(defconst nvemacs--settings-moved '(search-comments search-history)
  "Keys that versions before settings.eld kept in state.eld.
Only these are looked for there: state.eld's `tags-width' is the width
remembered from the last session, not a width the user chose.")

(defun nvemacs--setting (key)
  "The value kept for KEY in settings.eld.
A key in `nvemacs--settings-moved' is also looked for in state.eld."
  (let ((kept (assq key (nvemacs--settings-read))))
    (cond (kept (cdr kept))
          (nvemacs--settings-read-only nil)
          ((memq key nvemacs--settings-moved) (alist-get key (nvemacs--state-read))))))

(defun nvemacs--setting-put (key value)
  "Keep VALUE for KEY in settings.eld; nil removes KEY.
An old copy of a moved key in state.eld is removed, so it cannot come back."
  (let* ((f (nvemacs--settings-file))
         (rest (assq-delete-all key (copy-alist (nvemacs--settings-read))))
         (all (if value (cons (cons key value) rest) rest)))
    (when nvemacs--settings-read-only
      (nvemacs--command-error
       (or this-command 'nvemacs)
       (format "requires a readable settings.eld, and %s could not be read, so this session changes no settings"
               (abbreviate-file-name f))
       "Fix or move that file, then restart nvEmacs"))
    (nvemacs--private-directory (file-name-directory f))
    (with-temp-buffer
      (insert ";; nvEmacs settings: the choices made in nvEmacs (written by nvEmacs).\n"
              ";; Keep this file: it holds your watched folders.\n")
      (let ((print-escape-newlines t)) (prin1 all (current-buffer)))
      (insert "\n")
      (nvemacs--write-file f)))
  (when (memq key nvemacs--settings-moved)
    (let ((st (nvemacs--state-read)))
      (when (assq key st) (nvemacs--state-write (assq-delete-all key st))))))

(defun nvemacs--width-fixed ()
  "The fixed tags width, or nil.
A width set in the init file wins, then one chosen with
`nvemacs-notelist-widths-set'."
  (or (and (integerp nvemacs-notelist-tags-width) (> nvemacs-notelist-tags-width 0)
           nvemacs-notelist-tags-width)
      (let ((w (nvemacs--setting 'tags-width))) (and (integerp w) (> w 0) w))))

(defun nvemacs--widths-start ()
  "Return the tags width for a new notelist.
That is a fixed width, else one per `nvemacs-notelist-remember-widths'."
  (setq nvemacs--tags-width
        (or (nvemacs--width-fixed)
            (and (eq nvemacs-notelist-remember-widths 'last)
                 (let ((w (alist-get 'tags-width (nvemacs--state-read))))
                   (and (integerp w) (> w 0) w))))))

(defun nvemacs--widths-save ()
  "Remember the tags width when the mode is `last'."
  (when (and (eq nvemacs-notelist-remember-widths 'last) (integerp nvemacs--tags-width))
    (nvemacs--state-write (cons (cons 'tags-width nvemacs--tags-width)
                                (assq-delete-all 'tags-width (nvemacs--state-read))))))

(defun nvemacs--notelist-redraw ()
  "Draw the shown rows again with the current widths.
The selected row and the first visible line are kept (the list never
jumps to the first row)."
  (let* ((ids (copy-sequence nvemacs--shown))
         (wins (get-buffer-window-list (current-buffer) nil t))
         (pt (cons (line-number-at-pos) (current-column)))
         (starts (mapcar (lambda (w) (cons w (cons (line-number-at-pos (window-start w))
                                                    (line-number-at-pos (window-point w)))))
                         wins)))
    (let ((nvemacs--inhibit-search t))
      (nvemacs--clear-rows)
      (nvemacs--insert-rows ids))
    (cl-flet ((at (line) (save-excursion (goto-char (point-min)) (forward-line (1- line)) (point))))
      (goto-char (at (car pt))) (move-to-column (cdr pt))
      (dolist (s starts)
        (set-window-start (car s) (at (cadr s)) t)
        (set-window-point (car s) (if (eq (car s) (selected-window)) (point) (at (cddr s))))))))

(defun nvemacs-notelist-refresh ()
  "Fit the tags column to every row shown so far and redraw in place.
Saved for the next start when `nvemacs-notelist-remember-widths' is
`last'.  With a fixed width (`nvemacs-notelist-widths-set'), it lasts for
this session only."
  (interactive)
  (with-current-buffer nvemacs--buffer-name
    (setq nvemacs--tags-width (nvemacs--measure-tags nvemacs--shown))
    (nvemacs--notelist-redraw)
    (cond
     ((or (nvemacs--width-fixed) (eq nvemacs-notelist-remember-widths 'user))
      (message "nvEmacs: tags column %d for this session; not saved (M-x nvemacs-notelist-widths-set to keep)"
               nvemacs--tags-width))
     ((eq nvemacs-notelist-remember-widths 'last)
      (nvemacs--widths-save)
      (message "nvEmacs: tags column %d (saved)" nvemacs--tags-width))
     (t (message "nvEmacs: tags column %d (not saved)" nvemacs--tags-width)))))


(defun nvemacs-notelist-widths-set (width)
  "Set the notelist tags column to WIDTH columns, and keep it.
The width is kept in settings.eld in `nvemacs-data-directory', never in
the init file.  An empty answer (WIDTH nil) measures the column again
and stops keeping a width.  A width set in the init file wins: change it
there."
  (interactive
   (let ((s (string-trim (read-string "Notelist tags column width (empty: measure it): "
                                      (and nvemacs--tags-width (number-to-string nvemacs--tags-width))))))
     (list (and (not (string-empty-p s)) (string-to-number s)))))
  (when (integerp nvemacs-notelist-tags-width)
    (nvemacs--command-error
     'nvemacs-notelist-widths-set
     (format "requires a width nvEmacs keeps, and nvemacs-notelist-tags-width is %d in your init file or custom-file"
             nvemacs-notelist-tags-width)
     "Change it there, or delete that setting so nvEmacs can keep the width"))
  (when (and width (not (and (integerp width) (> width 0))))
    (nvemacs--command-error 'nvemacs-notelist-widths-set
                            "requires a whole number of columns above 0"
                            "Re-run the command with a number, or empty to measure"))
  (nvemacs--setting-put 'tags-width width)
  (setq nvemacs--tags-width width)
  (when (get-buffer nvemacs--buffer-name)
    (with-current-buffer nvemacs--buffer-name
      (if width (nvemacs--notelist-redraw) (nvemacs-notelist-refresh))))
  (if width
      (message "nvEmacs: tags column %d, kept in settings.eld" width)
    (message "nvEmacs: tags column measured again")))

(defun nvemacs--layout-now ()
  "The column layout rows drawn now in the current buffer's window get."
  (let ((win (get-buffer-window (current-buffer))))
    (nvemacs--notelist-layout (max 20 (- (if win (window-body-width win) 100) 1))
                              (or nvemacs--tags-width nvemacs-notelist-tags-min-width)
                              (nvemacs--notelist-column-list))))

(defun nvemacs--insert-rows (ids &optional max)
  "Append rows for IDS, skipping ones already shown, until MAX rows are shown."
  (let* ((inhibit-read-only t)
         (db (nvemacs--db))
         ;; First draw of a notelist: measure its first screen's tags.
         (_ (unless nvemacs--tags-width
              (when ids
                (setq nvemacs--tags-width (nvemacs--measure-tags (seq-take ids (or max 200)))))))
         (layout (nvemacs--layout-now)))
    (setq nvemacs--layout layout)
    (unless nvemacs--shown-set (setq nvemacs--shown-set (make-hash-table)))
    (let ((room (if max (- max (hash-table-count nvemacs--shown-set)) most-positive-fixnum))
          (seen (make-hash-table))
          (todo nil) (added nil))
      ;; The rows to draw: not shown yet, each once, at most ROOM.
      (while (and ids (> room 0))
        (let ((id (pop ids)))
          (unless (or (gethash id nvemacs--shown-set) (gethash id seen))
            (puthash id t seen) (push id todo) (cl-decf room))))
      (setq todo (nreverse todo))
      (let ((data (nvemacs--row-data db todo (assq 'tags layout))))
        (when todo
          (save-excursion
            (nvemacs--rows-open)
            (goto-char (point-max))
            (dolist (id todo)
              (let ((row (gethash id data)))
                (when row
                  (puthash id t nvemacs--shown-set)
                  (push id added)
                  (insert (nvemacs--row-text id row layout)))))
            (nvemacs--rows-close))))
      (setq nvemacs--shown (nconc nvemacs--shown (nreverse added))))
    (nvemacs--note-first-row)))

(defun nvemacs--row-data (db ids tags)
  "Return a hash table of the row data of IDS, read from index DB.
Each entry is ID -> (TITLE MTIME PREVIEW CREATED CGUESS TAGS-TEXT ADDED CWALL),
read a batch at a time (two queries per row cost ~65 ms of a 4,783-row
redraw, measured 2026-11-20).  With TAGS nil, no tags are read.  A
note's tags are listed file tags first, as `nvemacs--tags-text' does,
or alphabetized while the list is sorted by tags, so a row's place
makes sense."
  (let ((h (make-hash-table :size (max 1 (length ids))))
        ;; Only a list ordered by tags: the kept copy is always newest first.
        (tag-order (if (and nvemacs--sort-keys (eq (car nvemacs--sort) 'tags))
                       "folded, rowid"
                     "kind = 'section', rowid")))
    (while ids
      (let* ((chunk (seq-take ids 500))
             (marks (mapconcat (lambda (_) "?") chunk ",")))
        (setq ids (nthcdr 500 ids))
        (dolist (r (sqlite-select db (format "SELECT id, title, mtime, preview, created, cguess, added, cwall FROM meta WHERE id IN (%s)" marks)
                                  chunk))
          (pcase-let ((`(,id ,title ,mtime ,preview ,created ,cguess ,added ,cwall) r))
            (puthash id (list title mtime preview created cguess nil added cwall) h)))
        (when tags
          (dolist (r (sqlite-select db (format "SELECT id, tag FROM tags WHERE id IN (%s) ORDER BY id, %s" marks tag-order)
                                    chunk))
            (let ((row (gethash (car r) h)))
              (when row
                (setf (nth 5 row) (if (nth 5 row) (concat (nth 5 row) " " (cadr r)) (cadr r)))))))))
    h))

(defconst nvemacs--row-fill (propertize " " 'display '(space :align-to (- right (1))))
  "The end of every row: it carries the highlight to the window's edge.
A newline does that for the other rows, but the last row has none.  It
stops a pixel short so the end of the row stays on screen.")

(defun nvemacs--row-text (id row layout)
  "The list line of note ID: its cells for LAYOUT, from ROW.
ROW is as `nvemacs--row-data' returns it."
  (propertize (concat (mapconcat (lambda (e) (nvemacs--cell e id row)) layout "")
                      nvemacs--row-fill "\n")
              'read-only t 'nvemacs-id id))

(defun nvemacs--clear-rows ()
  "Delete every result row from the current list buffer."
  (let ((inhibit-read-only t))
    (delete-region nvemacs--rows-start (point-max))
    (setq nvemacs--shown nil nvemacs--shown-set nil)))

;; The list ends at its last row: a newline there would be a blank line the
;; cursor and the mouse could land on, with no note selected.
;; Every row is drawn with its newline, so code that adds or removes rows opens
;; the end of the list first and closes it after.

(defun nvemacs--rows-open ()
  "Give the last row its newline back, before rows are added or removed."
  (when (and (> (point-max) nvemacs--rows-start)
             (not (eq (char-before (point-max)) ?\n)))
    (let ((inhibit-read-only t)
          (id (get-text-property (1- (point-max)) 'nvemacs-id)))
      (save-excursion
        (goto-char (point-max))
        (insert (propertize "\n" 'read-only t 'nvemacs-id id))))))

(defun nvemacs--rows-close ()
  "Remove the newline after the last row."
  (when (and (> (point-max) nvemacs--rows-start)
             (eq (char-before (point-max)) ?\n))
    (let ((inhibit-read-only t))
      (delete-region (1- (point-max)) (point-max)))))

(defun nvemacs--row-at (pos)
  "Return the id of the note whose row holds POS, or nil."
  (and (>= pos nvemacs--rows-start)
       (get-text-property (save-excursion (goto-char pos) (line-beginning-position))
                          'nvemacs-id)))

;;;; Updating rows in place
;;
;; A save, a tag edit or a background reconcile changes a few index
;; entries; the list shows them by moving or replacing those rows only.
;; Redrawing every listed row (up to all notes, ~365 ms for 4,783) after
;; each 1 s auto-save is what stalled typing.

(defun nvemacs--row-pos (id)
  "Start of the row of note ID in the current list buffer, or nil."
  (and nvemacs--shown-set (gethash id nvemacs--shown-set)
       (text-property-any nvemacs--rows-start (point-max) 'nvemacs-id id)))

(defun nvemacs--shown-insert (new before)
  "Put NEW in `nvemacs--shown' just before BEFORE (nil: at the end)."
  (puthash new t nvemacs--shown-set)
  (cond ((or (null before) (null nvemacs--shown))
         (setq nvemacs--shown (nconc nvemacs--shown (list new))))
        ((eql (car nvemacs--shown) before) (push new nvemacs--shown))
        (t (let ((cell nvemacs--shown))
             (while (and (cdr cell) (not (eql (cadr cell) before))) (setq cell (cdr cell)))
             (setcdr cell (cons new (cdr cell)))))))

;; A typed search in the default sort lists title matches, then the other
;; matches, each group newest first.

(defconst nvemacs--title-rank (ash 1 60)
  "Added to a title match's sort key, to put it above every other match.")

(defun nvemacs--typed-key (id titles)
  "Return note ID's sort key in a typed search in the default sort.
TITLES is a hash table holding the ids of the title matches."
  (if (gethash id titles) (+ id nvemacs--title-rank) id))

(defun nvemacs--title-matches (db ids)
  "Return a hash table of those IDS whose title matches the typed search.
DB is the index."
  (let ((terms (car (nvemacs--parse-query (nvemacs--list-query))))
        (found (make-hash-table)))
    (when terms
      (pcase-let ((`(,table . ,match) (nvemacs--text-match terms 'title)))
        (while ids
          (let* ((chunk (seq-take ids 500))
                 (marks (mapconcat (lambda (_) "?") chunk ",")))
            (setq ids (nthcdr 500 ids))
            (dolist (r (sqlite-select db (format "SELECT rowid FROM %s WHERE rowid IN (%s) AND %s"
                                                 table marks (car match))
                                      (append chunk (cdr match))))
              (puthash (car r) t found))))))
    found))

(defun nvemacs--place-in-default-sort (db new newest)
  "Return the listed row that note NEW goes above, or nil for the end.
DB is the index.  A NEWEST list is in date order; a typed search keeps
title matches above the others."
  (if newest
      (cl-find-if (lambda (id) (< id new)) nvemacs--shown)
    (let* ((titles (nvemacs--title-matches db (cons new nvemacs--shown)))
           (key (nvemacs--typed-key new titles)))
      (cl-find-if (lambda (id) (< (nvemacs--typed-key id titles) key)) nvemacs--shown))))

(defun nvemacs--rows-apply (changes newest filter)
  "Show index CHANGES, ((OLD-ID . NEW-ID) ...), in the current list.
OLD's row is taken out and NEW's row is drawn at its place by date,
normally the top of its group (a word search lists title matches
first).  A word search shows what it found: a NEW it did not
list waits for the next search, and a listed note stays listed even if
it no longer matches.  In a NEWEST list (one without words, narrowed by
tag FILTER, see `nvemacs--tag-filter') a NEW the filter excludes goes,
and one not listed and older than every listed row waits for growth.
Return the list of (OLD . NEW) whose row moved or went."
  (let ((db (nvemacs--db)) (inhibit-read-only t) (moved nil))
    (unless nvemacs--shown-set (setq nvemacs--shown-set (make-hash-table)))
    (nvemacs--rows-open)
    (dolist (c changes)
      (let* ((old (car c)) (new (cdr c))
             (pos (and old (nvemacs--row-pos old)))
             (waiting (and old (memq old nvemacs--rest) t))
             (before nil) (draw nil))
        (when pos
          (delete-region pos (save-excursion (goto-char pos) (forward-line 1) (point)))
          (remhash old nvemacs--shown-set)
          (setq nvemacs--shown (delq old nvemacs--shown)))
        (when waiting (setq nvemacs--rest (delq old nvemacs--rest)))
        (when (and old nvemacs--sort-keys) (remhash old nvemacs--sort-keys))
        (when (and new (not (gethash new nvemacs--shown-set)))
          (cond
           ((if newest
                (not (caar (sqlite-select db (format "SELECT 1 FROM meta WHERE id = ?%s" (car filter))
                                          (cons new (cdr filter)))))
              (not (or pos waiting))))
           (nvemacs--sort-keys
            (pcase-let ((`(,at . ,now) (nvemacs--sort-place db new pos)))
              (setq before at draw now)))
           ((null nvemacs--shown) (setq draw t))
           ;; A listed note stays listed (at the end if nothing listed is
           ;; older); one not listed and older than every row waits for growth.
           (t (setq before (nvemacs--place-in-default-sort db new newest)
                    draw (or before pos)))))
        (when draw
          (let ((row (gethash new (nvemacs--row-data db (list new) (assq 'tags nvemacs--layout)))))
            (when (and row nvemacs--layout)
              (save-excursion
                (goto-char (or (and before (nvemacs--row-pos before)) (point-max)))
                (insert (nvemacs--row-text new row nvemacs--layout)))
              (nvemacs--shown-insert new before))))
        (when (or pos draw) (push c moved))))
    (nvemacs--rows-close)
    (nreverse moved)))

(defun nvemacs--list-update (changes &optional patch-kept)
  "Show CHANGES ((OLD-ID . NEW-ID) ...) in *nvEmacs* without redrawing it:
see `nvemacs--rows-apply'.  Point, and windows showing the list, stay on
the note they were on; the search line follows a selected note's title.
PATCH-KEPT non-nil (a save or tag edit made in Emacs): the kept list is
patched the same way; otherwise it is let go."
  (nvemacs--kept-update changes patch-kept)
  (let ((buf (get-buffer nvemacs--buffer-name)))
    (when (and buf changes)
      (with-current-buffer buf
        (pcase-let* ((`(,terms ,inc ,exc) (nvemacs--parse-query (nvemacs--list-query)))
                     (wins (get-buffer-window-list buf nil t))
                     (here (nvemacs--row-at (point)))
                     (win-ids (mapcar (lambda (w) (cons w (nvemacs--row-at (window-point w)))) wins))
                     (moved (nvemacs--rows-apply changes (null terms) (nvemacs--tag-filter inc exc t)))
                     (follow (lambda (id) (let ((m (assq id moved))) (if m (cdr m) id))))
                     (go (lambda (id) (and id (nvemacs--row-pos (funcall follow id))))))
          (when nvemacs--selected-id    ; a save gives the note a new id
            (setq nvemacs--selected-id (funcall follow nvemacs--selected-id)))
          (let ((p (funcall go here))) (when p (goto-char p)))
          (dolist (w win-ids)
            (let ((p (funcall go (cdr w))))
              (when p (set-window-point (car w) (if (eq (car w) (selected-window)) (point) p)))))
          (when nvemacs--selected-title
            (let ((now (funcall follow nvemacs--selected-title)))
              (cond ((null now) (nvemacs--restore-typed))
                    ((not (eql now nvemacs--selected-title))
                     (nvemacs--put-selected-title now)))))
          (when (and nvemacs--view-free hl-line-mode) (hl-line-highlight))
          (cond ((null terms) (nvemacs--landing-status inc exc))
                (nvemacs--sort-keys (nvemacs--list-status terms inc exc))))))))

(defun nvemacs--set-status (fmt &rest args)
  "Set the list's status text to (format FMT ARGS...) and show it."
  (setq nvemacs--status-base (apply #'format fmt args))
  (nvemacs--status-show))

(defun nvemacs--status-show ()
  "Show the status text, naming the sort when its column is not on screen."
  (setq nvemacs--status
        (concat nvemacs--status-base
                (if nvemacs--query-bad
                    (format " · %s requires a date such as 2026-11-23" nvemacs--query-bad)
                  "")
                (if (assq (car nvemacs--sort) nvemacs--layout)
                    ""
                  (format " · sorted by %s %s" (car nvemacs--sort) (nvemacs--sort-triangle)))))
  (force-mode-line-update))

(defvar nvemacs--list-complete nil
  "Non-nil once a list with no words shows every note it can.")

(defvar-local nvemacs--list-text nil
  "The search the list was last drawn for, as typed.
A rejected search leaves the list, and this, as they were.")

(defvar-local nvemacs--syntax-said nil
  "The search whose Boolean message was shown last.
The list redraws for many reasons; the message shows once per search.")

(defvar nvemacs--usage-section nil
  "The USAGE.txt section the last Boolean message pointed to, or nil.
`nvemacs-usage' opens there.  A search that runs without one clears it.")

(defconst nvemacs--usage-pointer "See \"Boolean search\" in USAGE.txt (M-x nvemacs-usage)"
  "Where the Boolean messages send the user.")

(defun nvemacs--query-check (query)
  "Return the `nvemacs--query-syntax' notes of QUERY.
The parse state of the search being shown is left as it was."
  (let ((nvemacs--query-bad nil) (nvemacs--query-syntax nil))
    (nvemacs--parse-query query)
    nvemacs--query-syntax))

(defun nvemacs--list-query ()
  "The search the list shows: the typed one, unless it was rejected."
  (let ((text (nvemacs--search-text)))
    (if (plist-get (nvemacs--query-check text) :rejected)
        (or nvemacs--list-text "")
      text)))

(defun nvemacs--boolean-say (kind)
  "Say in the echo area why a search with groups did not do what was typed.
KIND is a `nvemacs--query-syntax' value, `empty' or `no-note'.  The
messages do not quote the search, which can be longer than the echo
area; they state the rule, and every one points to USAGE.txt, where
`nvemacs-usage' then opens."
  (setq nvemacs--usage-section "Boolean search")
  (let ((message-log-max nil))         ; the echo area only
    (message "%s"
             (pcase kind
               ((or 'mixed 'unclear)
                (format "OR and AND in one group need explicit groups, as in (a (b OR c) -d).  %s."
                        nvemacs--usage-pointer))
               ('negative-or
                (format "A minus inside OR, as in (a OR -b), matches almost every note.  Results are shown; %s."
                        (concat (downcase (substring nvemacs--usage-pointer 0 1))
                                (substring nvemacs--usage-pointer 1))))
               ('and-word
                (format "Boolean AND is the space between words, so AND is searched as a word.  %s."
                        nvemacs--usage-pointer))
               ('empty
                (format "The search has an empty group.  Type what goes in it, or delete the parentheses.  %s."
                        nvemacs--usage-pointer))
               ('no-note
                (format "Return does not create a note from a search with parentheses.  C-c C-n creates one titled with the search.  %s."
                        nvemacs--usage-pointer))))))

(defun nvemacs--search ()
  "Run the current query in the current sort; the selection survives it.
A search rejected for its groups does not run: the list stays as it
was, and the echo area says why, once for each search.  One that runs
with a warning says so too."
  (let* ((text (nvemacs--search-text))
         (syntax (nvemacs--query-check text))
         (kind (or (plist-get syntax :rejected) (plist-get syntax :warning))))
    (cond ((null kind) (setq nvemacs--syntax-said nil nvemacs--usage-section nil))
          ((not (equal text nvemacs--syntax-said))
           (setq nvemacs--syntax-said text)
           (nvemacs--boolean-say kind)))
    (unless (plist-get syntax :rejected)
      (setq nvemacs--list-complete nil nvemacs--list-text text)
      (let* ((id (nvemacs--selection))
             (free nvemacs--view-free)
             (starts (and free (mapcar (lambda (w) (cons w (line-number-at-pos (window-start w))))
                                       (get-buffer-window-list (current-buffer) nil t)))))
        (if (equal nvemacs--sort '(modified . desc))
            (progn (setq nvemacs--sort-keys nil nvemacs--rest nil nvemacs--list-stale nil)
                   (nvemacs--search-newest))
          (nvemacs--search-sorted))
        (nvemacs--selection-restore id free starts)))))

(defun nvemacs--search-newest ()
  "Run the current query, drawing the matching rows as they arrive.
Titles are searched first, then bodies in adaptive rowid slices.  The
search stops when the screen is full, the range is exhausted, or a key
is pressed."
  (setq nvemacs--rendered-size (nvemacs--window-size))
  (nvemacs--ensure-match-width)            ; the setting may have changed
  (let* ((db (nvemacs--db))
         (query (nvemacs--list-query))
         (parsed (nvemacs--parse-query query))
         (terms (nth 0 parsed))
         (inc (nth 1 parsed))
         (exc (nth 2 parsed))
         (filter (nvemacs--tag-filter inc exc terms))
         (want (nvemacs--screen-rows))
         (before (length nvemacs--shown))
         (t0 (float-time))
         (first-ms nil)
         (first-row (lambda () (when (and nvemacs--shown (not first-ms))
                                 (setq first-ms (nvemacs--ms t0)))))
         (record (lambda (route outcome)
                   (setq nvemacs--last-search
                         (list :query query :terms (length terms) :route route
                               :rows (length nvemacs--shown) :want want
                               :first-row-ms first-ms :ms (nvemacs--ms t0)
                               :outcome outcome :when (format-time-string "%F %T"))))))
    (when nvemacs--session
      (setq nvemacs--session (plist-put nvemacs--session :searches
                                        (1+ (plist-get nvemacs--session :searches)))))
    (nvemacs--clear-rows)
    (if (null terms)
        (progn
          ;; Redraw at least as many rows as were shown: a refresh must not
          ;; drop rows the idle growth added, or a selected row below the
          ;; first screen is lost (see `nvemacs--grow').
          (let ((kept (and (null inc) (null exc) (nvemacs--restore-kept))))
            (unless kept
              (nvemacs--insert-rows
               (mapcar #'car (sqlite-select db (format "SELECT id FROM meta WHERE 1%s ORDER BY id DESC LIMIT ?"
                                                       (car filter))
                                            (append (cdr filter) (list (max want before)))))))
            (funcall first-row)
            (when (< (length nvemacs--shown) (max want before)) (setq nvemacs--list-complete t))
            (nvemacs--landing-status inc exc)
            (funcall record (if (or inc exc) "tags" "newest") (if kept "kept list" "listed"))))
      (let* ((tmatch (nvemacs--text-match terms 'title))
             (bmatch (nvemacs--text-match terms 'body))
             (route (cons (car tmatch) (car bmatch)))
             (twhere (concat (cadr tmatch) (car filter)))
             (targs (append (cddr tmatch) (cdr filter)))
             (where (concat (cadr bmatch) (car filter)))
             (args (append (cddr bmatch) (cdr filter)))
             (top (or (caar (sqlite-select db "SELECT max(id) FROM meta")) 0))
             (bottom (or (caar (sqlite-select db "SELECT min(id) FROM meta")) 0))
             (span (max 1 (- top bottom)))
             (r (1+ top))
             (d (max 1 (/ span nvemacs--slice-start-fraction)))
             (stopped nil)
             ;; Title phase: small table, newest first.
             (titles (mapcar #'car (sqlite-select db (format "SELECT rowid FROM %s WHERE %s ORDER BY rowid DESC LIMIT ?"
                                                             (car route) twhere)
                                                  (append targs (list want))))))
        (nvemacs--insert-rows titles)
        (funcall first-row)
        (redisplay)
        ;; Body phase: adaptive slices.
        (while (and (> r bottom) (< (length nvemacs--shown) want) (not stopped))
          (let* ((lo (max bottom (- r d)))
                 (c0 (float-time))
                 (ids (mapcar #'car
                              ;; No LIMIT: rows already shown by the title phase
                              ;; would count against it and cut the slice short,
                              ;; skipping matches (a check once showed 19 of
                              ;; the 20 matches for 서울). Slices are small by design.
                              (sqlite-select db (format "SELECT rowid FROM %s WHERE rowid < ? AND rowid >= ? AND %s ORDER BY rowid DESC"
                                                        (cdr route) where)
                                             (append (list r lo) args))))
                 (ms (* 1000 (- (float-time) c0))))
            (setq r lo)
            (when (= lo bottom) (setq r (1- bottom)))
            (setq d (cond ((< ms nvemacs--slice-fast-ms) (* d 2))
                          ((> ms nvemacs--slice-slow-ms) (max 1 (/ d 2)))
                          (t d)))
            (when ids
              (nvemacs--insert-rows ids want)
              (funcall first-row)
              (nvemacs--set-status " %d shown · searching %d%% · %s"
                                   (length nvemacs--shown)
                                   (/ (* 100 (- top r)) span) (cdr route))
              (redisplay))
            (when (input-pending-p) (setq stopped t))))
        ;; Every match is on screen: growth has nothing left to read.
        (when (and (not stopped) (< (length nvemacs--shown) want))
          (let ((rank (make-hash-table)))
            (dolist (id titles) (puthash id t rank))
            (setq nvemacs--sort-keys (make-hash-table))
            (dolist (id nvemacs--shown)
              (puthash id (nvemacs--typed-key id rank) nvemacs--sort-keys))))
        ;; A key stopped it before any row: growth needs a row to follow, and
        ;; a cursor key would not search again.
        (when (and stopped (null nvemacs--shown)) (nvemacs--schedule-search))
        (nvemacs--set-status " %d shown%s · %s · %.0f ms"
                             (length nvemacs--shown)
                             (cond (stopped " · interrupted")
                                   ((>= (length nvemacs--shown) want) " · screen full")
                                   (t " · all found"))
                             (cdr route) (* 1000 (- (float-time) t0)))
        (funcall record (cdr route)
                 (cond (stopped "interrupted")
                       ((>= (length nvemacs--shown) want) "screen full")
                       (t "all found")))))))

;;;; The kept list
;;
;; Once growth has listed every note (empty search, no tag filter), a copy
;; of the rows is kept in a hidden buffer. Clearing a later search puts the
;; copy back -- every row at once, about the cost of one screen (6.6 ms for
;; 4,783 rows in batch) -- instead of growing the list again over a minute
;; or more of pauses. The copy is used only while it is right: the index
;; generation, the column layout, the title match width and the preview
;; setting must be those it was drawn for. A save or tag edit made in Emacs
;; patches it like the visible list; any other index change lets it go.

(defconst nvemacs--kept-name " *nvEmacs kept list*")
(defvar nvemacs--kept-key nil "What the kept list was drawn for (`nvemacs--list-key').")

(defun nvemacs--generation ()
  "Return the index's generation: its count of changes."
  (caar (sqlite-select (nvemacs--db) "SELECT value FROM info WHERE key = 'generation'")))

(defun nvemacs--list-key (layout)
  "Return what a list drawn now with LAYOUT depends on.
A kept list is valid only while this key stays the same."
  (list (nvemacs--generation) layout nvemacs-title-match-width nvemacs-notelist-preview))

(defun nvemacs--kept-valid-p (layout)
  "Return non-nil if the kept list still matches LAYOUT and the index."
  (and nvemacs--kept-key (get-buffer nvemacs--kept-name)
       (equal nvemacs--kept-key (nvemacs--list-key layout))))

(defun nvemacs--kept-drop ()
  "Forget the kept list and kill its buffer."
  (setq nvemacs--kept-key nil)
  (let ((b (get-buffer nvemacs--kept-name))) (when b (kill-buffer b))))

(defun nvemacs--keep-list ()
  "Keep a copy of the rows of the current list buffer.
Called when every note is listed, newest first.  Nothing is copied if
the kept copy is already this list."
  (unless (nvemacs--kept-valid-p nvemacs--layout)
    (let ((src (current-buffer)) (start (marker-position nvemacs--rows-start)) (end (point-max))
          (shown nvemacs--shown) (layout nvemacs--layout))
      (with-current-buffer (get-buffer-create nvemacs--kept-name)
        (setq buffer-undo-list t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-buffer-substring src start end))
        (setq-local nvemacs--rows-start (copy-marker (point-min)))
        (setq-local nvemacs--layout layout)
        (setq-local nvemacs--shown (copy-sequence shown))
        (setq-local nvemacs--shown-set (let ((h (make-hash-table :size (max 1 (length shown)))))
                                         (dolist (id shown) (puthash id t h))
                                         h)))
      (setq nvemacs--kept-key (nvemacs--list-key layout)))))

(defun nvemacs--restore-kept ()
  "Put the kept list in the current list buffer, if it still fits.
It fits if it matches the layout rows would be drawn with now.  Return
non-nil if it was put back."
  (let ((layout (nvemacs--layout-now)))
    (if (not (nvemacs--kept-valid-p layout))
        (progn (when nvemacs--kept-key (nvemacs--kept-drop)) nil)
      (let ((kept (get-buffer nvemacs--kept-name)) (inhibit-read-only t))
        (nvemacs--clear-rows)
        (save-excursion (goto-char (point-max)) (insert-buffer-substring kept))
        (setq nvemacs--layout layout
              nvemacs--shown (copy-sequence (buffer-local-value 'nvemacs--shown kept))
              nvemacs--shown-set (copy-hash-table (buffer-local-value 'nvemacs--shown-set kept)))
        (nvemacs--note-first-row)
        t))))

(defun nvemacs--kept-update (changes patch)
  "Update the kept list for index CHANGES, ((OLD-ID . NEW-ID) ...).
With PATCH, and the kept list right until this change (one generation
behind), patch it like the visible list; otherwise let it go."
  (let ((kept (get-buffer nvemacs--kept-name)))
    (when kept
      (if (and patch nvemacs--kept-key
               (eql (car nvemacs--kept-key) (1- (nvemacs--generation))))
          (progn
            (with-current-buffer kept (nvemacs--rows-apply changes t (list "")))
            (setcar nvemacs--kept-key (nvemacs--generation)))
        (nvemacs--kept-drop)))))

;;;; Newest-notes list: header count and idle growth

(defvar nvemacs--grow-timer nil
  "Repeating idle timer: fires once per idle period, after `nvemacs-grow-delay'.")
(defvar nvemacs--grow-chain nil
  "One-shot idle timer for the next step within the same idle period.")

(defun nvemacs--group-digits (n)
  "N with thousands separators: 4726 -> \"4,726\"."
  (let ((s (number-to-string n)))
    (while (string-match "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\(?:,\\|\\'\\)" s)
      (setq s (replace-match "\\1,\\2" t nil s 0)))
    s))

(defun nvemacs--landing-status (&optional inc exc)
  "Return the header line of a list without text terms.
It reads N of TOTAL newest notes, or of the notes matching the search
filters INC and EXC."
  (let ((filter (nvemacs--tag-filter inc exc)))
    (nvemacs--set-status
     (if (or inc exc)
         (concat " %s of %s notes with "
                 ;; A filter is user text, and this is a format string.
                 (string-replace
                  "%" "%%"
                  (mapconcat #'identity
                             (append (mapcar #'nvemacs--filter-text inc)
                                     (mapcar (lambda (v) (concat "-" (nvemacs--filter-text v))) exc))
                             " "))
                 " displayed%s")
       (if (equal nvemacs--sort '(modified . desc))
           " %s of %s Newest notes displayed · type to search%s"
         " %s of %s notes displayed · type to search%s"))
     (nvemacs--group-digits (length nvemacs--shown))
     (nvemacs--group-digits
      (or (caar (sqlite-select (nvemacs--db)
                               (format "SELECT count(*) FROM meta WHERE 1%s" (car filter))
                               (cdr filter)))
          0))
     (if nvemacs--skipped
         (format " · %d not indexed" (length nvemacs--skipped)) ""))))

(defun nvemacs--grow ()
  "Append the next screenful of notes below the list.
Return the number of rows added, or nil when there is nothing to do
\(every note listed, or a key was pressed while matches were read)."
  (pcase-let ((`(,terms ,inc ,exc) (nvemacs--parse-query (nvemacs--list-query))))
    (cond
     ((or (null nvemacs--shown) nvemacs--list-stale) nil)
     ;; A typed search in the default sort: its first growth finds the rest.
     ((and terms (null nvemacs--sort-keys)
           (not (nvemacs--read-typed-rest terms inc exc)))
      nil)
     (nvemacs--sort-keys
      (let ((added (nvemacs--rest-draw (nvemacs--screen-rows))))
        (when (> added 0)
          (nvemacs--list-status terms inc exc)
          added)))
     (t                                 ; newest notes, or a tag-only list
      (let ((before (length nvemacs--shown))
            (filter (nvemacs--tag-filter inc exc)))
        (nvemacs--insert-rows
         (mapcar #'car (sqlite-select
                        (nvemacs--db)
                        (format "SELECT id FROM meta WHERE id < ?%s ORDER BY id DESC LIMIT ?" (car filter))
                        (append (list (car (last nvemacs--shown))) (cdr filter)
                                (list (nvemacs--screen-rows))))))
        ;; Rows actually added, so the idle chain can never spin on rows
        ;; that were fetched but already shown.
        (let ((added (- (length nvemacs--shown) before)))
          (if (> added 0)
              (progn (nvemacs--landing-status inc exc) added)
            (unless (or inc exc) (nvemacs--keep-list))   ; every note is listed
            (setq nvemacs--list-complete t)
            nil)))))))

(defconst nvemacs--grow-margin 3
  "Rows from the end of the list at which the next screenful loads at once.")

(defun nvemacs--grow-near-bottom ()
  "Load more rows now if the cursor is close to the last row.
Waiting for idle time would stop the cursor at the end of what is drawn.
`nvemacs-grow-delay' nil keeps the list at one screenful."
  (when (and nvemacs-grow-delay nvemacs--shown (not (nvemacs--in-query-p))
             (save-excursion (forward-line nvemacs--grow-margin) (eobp)))
    (nvemacs--grow)))

(defun nvemacs--grow-tick ()
  "Grow the list by one step while the cursor is in it.
Run from an idle timer.  The list does not grow beside a note being
typed in: that gave each save more rows to redraw.  If it grew, arm the
next step for `nvemacs-grow-delay' more seconds of the same idle
period; an idle timer fires only once per idle period, hence the
chain."
  (when nvemacs--grow-chain (cancel-timer nvemacs--grow-chain))
  (let ((buf (get-buffer nvemacs--buffer-name)))
    (when (and nvemacs-grow-delay buf (eq (window-buffer (selected-window)) buf)
               (with-current-buffer buf (nvemacs--grow)))
      (setq nvemacs--grow-chain
            (run-with-idle-timer (time-add (or (current-idle-time) 0) nvemacs-grow-delay)
                                 nil #'nvemacs--grow-tick)))))

(defun nvemacs--grow-enable ()
  "Start the idle timer that grows the list of newest notes."
  (unless (or nvemacs--grow-timer (null nvemacs-grow-delay))
    (setq nvemacs--grow-timer (run-with-idle-timer nvemacs-grow-delay t #'nvemacs--grow-tick))))

(defun nvemacs--grow-disable ()
  "Stop the idle timers that grow the list."
  (dolist (tm (list nvemacs--grow-timer nvemacs--grow-chain)) (when tm (cancel-timer tm)))
  (setq nvemacs--grow-timer nil nvemacs--grow-chain nil))

;;;; Sorting the notelist
;;
;; Modified, newest first, is the order rows are stored in, so that list
;; fills as it is read. In any other order a row cannot be trusted until
;; every match is known: refreshing while reading changed the screen up to
;; 160 times and was final only after 91-100% of the matches (measured
;; 2026-11-22). So all matches are found and ordered once (4-119 ms for 35
;; to 36,694 matches), the first screen is drawn, and later rows are only
;; added below. The read goes in slices, so a key stops it with the old
;; list still up.

(defconst nvemacs--column-keys
  '((?a . title) (?b . tags) (?c . created) (?d . added) (?e . modified))
  "The letters of the sort and column menus.")

(defun nvemacs--sort-sql (column)
  "Return the `meta' field that orders COLUMN."
  (pcase column
    ('title "title_key") ('tags "tags_key") ('added "added")
    ;; The true moment when the note gives a zone.  A date without one is
    ;; local wherever the user is.
    ('created (format "coalesce(cwall - coff, cwall - %d, created)" (car (current-time-zone))))
    (_ "id")))

(defun nvemacs--match-pairs (terms filter column &optional titles)
  "Return every note matching TERMS and tag FILTER, as ((ID KEY) ...).
KEY is the note's sort key for COLUMN, and the list is newest first.
With TITLES, only notes whose title matches.  Text searches read in
adaptive slices; if a key is pressed before the last one, return
`interrupted'."
  (let ((db (nvemacs--db)) (key (nvemacs--sort-sql column)))
    (if (null terms)
        (sqlite-select db (format "SELECT id, %s FROM meta WHERE 1%s ORDER BY id DESC" key (car filter))
                       (cdr filter))
      (let* ((tm (nvemacs--text-match terms (if titles 'title 'body)))
             (table (car tm))
             (where (concat (cadr tm) (car filter)))
             (args (append (cddr tm) (cdr filter)))
             (top (or (caar (sqlite-select db "SELECT max(id) FROM meta")) 0))
             (bottom (or (caar (sqlite-select db "SELECT min(id) FROM meta")) 0))
             (span (max 1 (- top bottom)))
             (r (1+ top))
             (d (max 1 (/ span nvemacs--slice-start-fraction)))
             (t0 (float-time))
             (chunks nil) (stopped nil))
        (while (and (> r bottom) (not stopped))
          (let* ((lo (max bottom (- r d)))
                 (c0 (float-time))
                 (rows (sqlite-select
                        db (format "SELECT id, %s FROM meta WHERE id IN (SELECT rowid FROM %s WHERE rowid < ? AND rowid >= ? AND %s) ORDER BY id DESC"
                                   key table where)
                        (append (list r lo) args)))
                 (ms (* 1000 (- (float-time) c0))))
            (setq r (if (= lo bottom) (1- bottom) lo))
            (setq d (cond ((< ms nvemacs--slice-fast-ms) (* d 2))
                          ((> ms nvemacs--slice-slow-ms) (max 1 (/ d 2)))
                          (t d)))
            (when rows (push rows chunks))
            (when (> r bottom)
              (if (input-pending-p)
                  (setq stopped t)
                (when (> (- (float-time) t0) 0.1)     ; only a slow pass says so
                  (nvemacs--set-status " sorting… %d%%" (/ (* 100 (- top r)) span))
                  (redisplay))))))
        (if stopped 'interrupted (apply #'nconc (nreverse chunks)))))))

(defun nvemacs--sort-order (pairs)
  "Return the ids of PAIRS, ((ID KEY) ...) newest first, in the current sort.
Blank keys go last in both directions.  The sort is stable, so ties stay
newest first."
  (let ((asc (eq (cdr nvemacs--sort) 'asc)) (filled nil) (blank nil))
    (dolist (p pairs)
      (if (member (cadr p) '(nil "")) (push (car p) blank) (push p filled)))
    (setq filled (nreverse filled))
    (nconc (mapcar #'car
                   (sort filled
                         (if (stringp (cadr (car filled)))
                             (if asc
                                 (lambda (a b) (string< (cadr a) (cadr b)))
                               (lambda (a b) (string< (cadr b) (cadr a))))
                           (if asc
                               (lambda (a b) (< (cadr a) (cadr b)))
                             (lambda (a b) (> (cadr a) (cadr b)))))))
           (nreverse blank))))

(defun nvemacs--sort-before-p (a b)
  "Return non-nil if the note with id A sorts before the one with id B."
  (let ((ka (gethash a nvemacs--sort-keys)) (kb (gethash b nvemacs--sort-keys)))
    (cond ((member kb '(nil "")) (or (not (member ka '(nil ""))) (> a b)))
          ((member ka '(nil "")) nil)
          ((equal ka kb) (> a b))
          ((stringp ka) (if (eq (cdr nvemacs--sort) 'asc) (string< ka kb) (string< kb ka)))
          ((eq (cdr nvemacs--sort) 'asc) (< ka kb))
          (t (> ka kb)))))

(defun nvemacs--rest-draw (n)
  "Draw the next N rows of the ordered list; return how many were added."
  (let ((before (length nvemacs--shown))
        (next (seq-take nvemacs--rest n)))
    (setq nvemacs--rest (nthcdr n nvemacs--rest))
    (nvemacs--insert-rows next)
    (- (length nvemacs--shown) before)))

(defun nvemacs--rest-insert (id)
  "Put ID at its place among the rows not drawn yet."
  (if (or (null nvemacs--rest) (nvemacs--sort-before-p id (car nvemacs--rest)))
      (push id nvemacs--rest)
    (let ((cell nvemacs--rest))
      (while (and (cdr cell) (not (nvemacs--sort-before-p id (cadr cell))))
        (setq cell (cdr cell)))
      (setcdr cell (cons id (cdr cell))))))

(defun nvemacs--sort-place (db new was-drawn)
  "Find where note NEW, read from DB, goes in an ordered list.
Return (BEFORE . DRAW): BEFORE is the row to draw it above (nil: the
end), DRAW non-nil to draw it.  A note that belongs below the rows drawn
so far waits with them, unless WAS-DRAWN: then the rows down to it are
drawn, so the cursor keeps it."
  (puthash new (if (equal nvemacs--sort '(modified . desc)) ; here, always a typed search
                   (nvemacs--typed-key new (nvemacs--title-matches db (list new)))
                 (caar (sqlite-select db (format "SELECT %s FROM meta WHERE id = ?"
                                                 (nvemacs--sort-sql (car nvemacs--sort)))
                                      (list new))))
           nvemacs--sort-keys)
  (let ((before (cl-find-if (lambda (id) (nvemacs--sort-before-p new id)) nvemacs--shown)))
    (cond (before (cons before t))
          ((null nvemacs--rest) (cons nil t))
          (t (nvemacs--rest-insert new)
             (when was-drawn (nvemacs--rest-draw (1+ (cl-position new nvemacs--rest))))
             (cons nil nil)))))

(defun nvemacs--read-typed-rest (terms inc exc)
  "Order the matches of a typed search in the default sort, for growth.
TERMS, INC and EXC are the parsed search.  The rows already drawn stay;
the others follow them, title matches and then the rest, each newest
first.  Return non-nil unless a key stopped the read."
  (let* ((status nvemacs--status-base)
         (filter (nvemacs--tag-filter inc exc t))
         (titles (nvemacs--match-pairs terms filter 'modified t))
         (pairs (if (eq titles 'interrupted)
                    titles
                  (nvemacs--match-pairs terms filter 'modified))))
    (if (eq pairs 'interrupted)
        (progn (nvemacs--set-status "%s" status) nil)
      (let ((keys (make-hash-table :size (max 1 (+ (length titles) (length pairs)))))
            (rank (make-hash-table :size (max 1 (length titles)))))
        (dolist (p titles) (puthash (car p) t rank))
        (dolist (p (append titles pairs))
          (puthash (car p) (nvemacs--typed-key (car p) rank) keys))
        (dolist (id nvemacs--shown) (puthash id (nvemacs--typed-key id rank) keys))
        (setq nvemacs--sort-keys keys
              nvemacs--rest
              (nconc (cl-loop for p in titles
                              unless (gethash (car p) nvemacs--shown-set)
                              collect (car p))
                     (cl-loop for p in pairs
                              unless (or (gethash (car p) rank)
                                         (gethash (car p) nvemacs--shown-set))
                              collect (car p))))
        (nvemacs--set-status "%s" status)
        t))))

(defun nvemacs--list-status (terms inc exc)
  "Show how much of an ordered list is drawn.
TERMS, INC and EXC are the parsed search."
  (if terms
      (nvemacs--set-status " %s of %s matches displayed"
                           (nvemacs--group-digits (length nvemacs--shown))
                           (nvemacs--group-digits (+ (length nvemacs--shown)
                                                     (length nvemacs--rest))))
    (nvemacs--landing-status inc exc)))

(defun nvemacs--search-sorted ()
  "Run the current query in a column sort.
Every match is found and ordered before the list changes.  A key
pressed meanwhile leaves the old list up, and the search runs again."
  (setq nvemacs--rendered-size (nvemacs--window-size))
  (nvemacs--ensure-match-width)
  (when nvemacs--session
    (setq nvemacs--session (plist-put nvemacs--session :searches
                                      (1+ (plist-get nvemacs--session :searches)))))
  (pcase-let* ((query (nvemacs--list-query))
               (`(,terms ,inc ,exc) (nvemacs--parse-query query))
               (want (nvemacs--screen-rows))
               (before (length nvemacs--shown))
               (t0 (float-time))
               (pairs (nvemacs--match-pairs terms (nvemacs--tag-filter inc exc terms)
                                            (car nvemacs--sort)))
               (stopped (eq pairs 'interrupted)))
    (if stopped
        (progn
          (setq nvemacs--list-stale t)
          (nvemacs--set-status " %d shown · interrupted" before)
          (nvemacs--schedule-search))
      (let ((keys (make-hash-table :size (max 1 (length pairs)))))
        (dolist (p pairs) (puthash (car p) (cadr p) keys))
        (nvemacs--clear-rows)
        (setq nvemacs--list-stale nil
              nvemacs--sort-keys keys
              nvemacs--rest (nvemacs--sort-order pairs))
        (nvemacs--rest-draw (max want before))
        (nvemacs--list-status terms inc exc)))
    (setq nvemacs--last-search
          (list :query query :terms (length terms) :route "sorted"
                :rows (length nvemacs--shown) :want want
                :first-row-ms (nvemacs--ms t0) :ms (nvemacs--ms t0)
                :outcome (if stopped "interrupted" "listed")
                :when (format-time-string "%F %T")))))

(defun nvemacs--show-id (id)
  "Draw rows until note ID is listed, and put point on it if it is."
  (cl-flet ((drawn () (and nvemacs--shown-set (gethash id nvemacs--shown-set))))
    (while (and (not (drawn)) (not nvemacs--sort-keys) (nvemacs--grow)))
    (unless (drawn)
      (let ((i (cl-position id nvemacs--rest)))
        (when i (nvemacs--rest-draw (1+ i)))))
    (let ((p (nvemacs--row-pos id))) (when p (goto-char p)))))

(defun nvemacs--sort-apply ()
  "Redraw the list in the current sort and go to the selected note.
A new order makes a scrolled view meaningless.  A window in another
frame stays on the note it was on."
  (let* ((id (nvemacs--selection))
         (others (cl-loop for w in (get-buffer-window-list (current-buffer) nil t)
                          unless (eq (window-frame w) (selected-frame))
                          collect (cons w (nvemacs--row-at (window-point w))))))
    (nvemacs--view-free-set nil)
    (setq nvemacs--selected-id id)
    (if id
        (nvemacs--show-id id)
      (goto-char nvemacs--query-start)
      (end-of-line))
    (nvemacs--search)
    (dolist (w others)
      (when (cdr w)
        (save-excursion (nvemacs--show-id (cdr w)))
        (let ((p (nvemacs--row-pos (cdr w)))) (when p (set-window-point (car w) p)))))
    (force-mode-line-update t)))

(defun nvemacs--notelist-check (command)
  "Signal an error from COMMAND unless the notelist exists."
  (unless (get-buffer nvemacs--buffer-name)
    (nvemacs--command-error command "requires the nvEmacs notelist"
                            "Run M-x nvemacs and then re-run the command")))

(defun nvemacs--sort-start ()
  "Set the sort remembered in state.eld, or the default."
  (let ((s (alist-get 'sort (nvemacs--state-read))))
    (setq nvemacs--sort
          (if (and (consp s) (memq (car s) nvemacs--column-names) (memq (cdr s) '(asc desc)))
              s
            (cons 'modified 'desc)))))

(defun nvemacs--sort-set (sort)
  "Make SORT, (COLUMN . DIRECTION), the list's sort, and remember it."
  (setq nvemacs--sort sort)
  (nvemacs--state-write (cons (cons 'sort sort) (assq-delete-all 'sort (nvemacs--state-read))))
  (with-current-buffer nvemacs--buffer-name (nvemacs--sort-apply)))

(defun nvemacs--sort-by (column command)
  "Sort by COLUMN for COMMAND; if the list already is, reverse it.
A new column starts A to Z (title, tags) or newest first (dates)."
  (nvemacs--notelist-check command)
  (nvemacs--sort-set
   (cons column (cond ((not (eq column (car nvemacs--sort)))
                       (if (memq column '(title tags)) 'asc 'desc))
                      ((eq (cdr nvemacs--sort) 'asc) 'desc)
                      (t 'asc)))))

(defun nvemacs--heading-at (event)
  "Return the column whose heading mouse EVENT is on, or nil.
The heading's text says, when the event carries it.  A real first click
can arrive without it, so the click's position is the fallback."
  (let* ((pos (event-start event))
         (where (posn-string pos)))
    (or (and where (get-text-property (cdr where) 'nvemacs-column (car where)))
        (let ((x (car (posn-col-row pos))))
          (with-current-buffer (window-buffer (posn-window pos))
            (car (cl-find-if (lambda (e) (and (>= x (nth 1 e)) (< x (+ (nth 1 e) (nth 2 e) 2))))
                             nvemacs--layout)))))))

(defun nvemacs--heading-click (event)
  "Sort by the column heading clicked in EVENT; click again to reverse."
  (interactive "e")
  (let ((column (nvemacs--heading-at event)))
    (when column (nvemacs--sort-by column 'nvemacs-notelist-sort-column))))

(defun nvemacs-notelist-sort ()
  "Reverse the notelist's current sort."
  (interactive)
  (nvemacs--sort-by (car nvemacs--sort) 'nvemacs-notelist-sort))

(defun nvemacs-notelist-sort-title ()
  "Sort the notelist by title.  Run it again to reverse the order."
  (interactive)
  (nvemacs--sort-by 'title 'nvemacs-notelist-sort-title))

(defun nvemacs-notelist-sort-tags ()
  "Sort the notelist by tags.  Run it again to reverse the order.
Each note's tags are alphabetized and compared one at a time."
  (interactive)
  (nvemacs--sort-by 'tags 'nvemacs-notelist-sort-tags))

(defun nvemacs-notelist-sort-created ()
  "Sort the notelist by Created date.  Run it again to reverse the order."
  (interactive)
  (nvemacs--sort-by 'created 'nvemacs-notelist-sort-created))

(defun nvemacs-notelist-sort-added ()
  "Sort the notelist by Added date.  Run it again to reverse the order."
  (interactive)
  (nvemacs--sort-by 'added 'nvemacs-notelist-sort-added))

(defun nvemacs-notelist-sort-modified ()
  "Sort the notelist by Modified date.  Run it again to reverse the order."
  (interactive)
  (nvemacs--sort-by 'modified 'nvemacs-notelist-sort-modified))

(defun nvemacs--sort-menu-text ()
  "Return the prompt of the sort menu, with a triangle on the sorted column."
  (concat "Sort by: "
          (mapconcat (lambda (k)
                       (format "%c) %s%s" (car k) (cdr k)
                               (if (eq (cdr k) (car nvemacs--sort))
                                   (concat " " (nvemacs--sort-triangle))
                                 "")))
                     nvemacs--column-keys "  ")
          "  ^) reverse "))

(defun nvemacs-notelist-sort-column ()
  "Ask for a column and sort the notelist by it.
One key chooses.  Choosing the sorted column, or ^, reverses the order."
  (interactive)
  (nvemacs--notelist-check 'nvemacs-notelist-sort-column)
  (let ((key (read-char-choice (nvemacs--sort-menu-text)
                               (cons ?^ (mapcar #'car nvemacs--column-keys)))))
    (message nil)                       ; Emacs leaves the question and answer up
    (if (eq key ?^)
        (nvemacs-notelist-sort)
      (nvemacs--sort-by (alist-get key nvemacs--column-keys) 'nvemacs-notelist-sort-column))))

;;;; Showing and hiding columns

(defun nvemacs--column-order ()
  "Return every column in the order it is drawn.
That is the preference, with the columns it leaves out at their place in
the standard order."
  (let ((order (nvemacs--notelist-column-preference)))
    (dolist (c nvemacs--column-names)
      (unless (memq c order)
        (let* ((later (cdr (memq c nvemacs--column-names)))
               (at (or (cl-position-if (lambda (x) (memq x later)) order) (length order))))
          (setq order (append (seq-take order at) (list c) (nthcdr at order))))))
    order))

(defun nvemacs--columns-redraw ()
  "Draw the list again with the current columns."
  (with-current-buffer nvemacs--buffer-name
    (nvemacs--notelist-redraw)
    (nvemacs--status-show)
    (force-mode-line-update t)))

(defun nvemacs--column-toggle (column command)
  "Show or hide COLUMN for this session, for COMMAND."
  (nvemacs--notelist-check command)
  (let ((now (nvemacs--notelist-column-list)))
    (setq nvemacs--columns-now
          (if (memq column now)
              (remq column now)
            (cl-remove-if-not (lambda (c) (or (eq c column) (memq c now)))
                              (nvemacs--column-order)))))
  (nvemacs--columns-redraw))

(defun nvemacs-notelist-column-tags-toggle ()
  "Show or hide the Tags column for this session."
  (interactive)
  (nvemacs--column-toggle 'tags 'nvemacs-notelist-column-tags-toggle))

(defun nvemacs-notelist-column-created-toggle ()
  "Show or hide the Created column for this session."
  (interactive)
  (nvemacs--column-toggle 'created 'nvemacs-notelist-column-created-toggle))

(defun nvemacs-notelist-column-added-toggle ()
  "Show or hide the Added column for this session."
  (interactive)
  (nvemacs--column-toggle 'added 'nvemacs-notelist-column-added-toggle))

(defun nvemacs-notelist-column-modified-toggle ()
  "Show or hide the Modified column for this session."
  (interactive)
  (nvemacs--column-toggle 'modified 'nvemacs-notelist-column-modified-toggle))

(defun nvemacs--column-menu-text ()
  "Return the prompt of the column menu, with a check on each shown column."
  (let ((now (nvemacs--notelist-column-list)))
    (concat "Columns: "
            (mapconcat (lambda (k)
                         (format "%c) %s%s" (car k) (cdr k) (if (memq (cdr k) now) " ✓" "")))
                       (cdr nvemacs--column-keys) "  ")
            "   RET) done ")))

(defun nvemacs-notelist-column-toggle ()
  "Show or hide notelist columns from a menu, for this session.
Each key toggles a column at once.  RET ends the menu, and \\[keyboard-quit]
puts back the columns it started with.  The title is always shown.
`nvemacs-notelist-columns' sets the columns at launch."
  (interactive)
  (nvemacs--notelist-check 'nvemacs-notelist-column-toggle)
  (let ((start nvemacs--columns-now)
        (keys (cdr nvemacs--column-keys))
        (key nil))
    (condition-case nil
        (progn
          (while (not (eq ?\r (setq key (read-char-choice (nvemacs--column-menu-text)
                                                          (cons ?\r (mapcar #'car keys))))))
            (nvemacs--column-toggle (alist-get key keys) 'nvemacs-notelist-column-toggle))
          (message nil))                ; Emacs leaves the question and answer up
      (quit (setq nvemacs--columns-now start)
            (nvemacs--columns-redraw)
            (signal 'quit nil)))))

(defun nvemacs--row-id ()
  "Return the rowid of the note on the current line, or nil."
  (get-text-property (line-beginning-position) 'nvemacs-id))

(defun nvemacs-notelist-next ()
  "Move to the next result row."
  (interactive)
  (if (nvemacs--in-query-p) (goto-char nvemacs--rows-start) (forward-line 1)))

(defun nvemacs-notelist-previous ()
  "Move to the previous result row, or back to the query."
  (interactive)
  (forward-line -1)
  (when (< (point) nvemacs--rows-start) (goto-char nvemacs--query-start) (end-of-line)))

(defun nvemacs-searchbar-clear ()
  "Clear the query.
The search history sees the deleted text as a search cleared."
  (interactive)
  (setq nvemacs--selected-title nil nvemacs--typed nil nvemacs--selected-id nil)
  (nvemacs--view-free-set nil)
  (let ((inhibit-read-only t))
    (delete-region nvemacs--query-start
                   (save-excursion (goto-char nvemacs--query-start) (line-end-position))))
  (goto-char nvemacs--query-start))

(defun nvemacs--target-path ()
  "Path of the selected note, or of the first row when none is selected."
  (let ((id (if (nvemacs--on-search-line-p) (car nvemacs--shown) (nvemacs--selection))))
    (and id (caar (sqlite-select (nvemacs--db) "SELECT path FROM meta WHERE id = ?" (list id))))))

;;;; Layout
;;
;; After `nvemacs' the frame belongs to nvEmacs. It shows one of three
;; arrangements: `side' (list left, note pane right), `stacked' (list top,
;; note pane below) or `list' (list only; a note then takes the whole frame).
;; State lives in frame parameters, so each frame is independent:
;;   nvemacs-active    non-nil while nvEmacs owns the frame
;;   nvemacs-l0        window configuration from before `nvemacs'
;;   nvemacs-override  nil (auto) or a forced arrangement (C-c C-v)
;;   nvemacs-temp      non-nil: `stacked' opened by C-o from `list'
;;   nvemacs-current   arrangement currently applied
;;   nvemacs-l1        (CONFIG . NOTE) saved when a note took the frame

(defcustom nvemacs-preview-delay 0.2
  "Idle seconds on a row before its note is previewed in the note pane."
  :type 'number
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-min-width 50
  "Narrowest useful result list, in columns (side-by-side arrangement)."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notecontent-min-width 80
  "Narrowest useful note pane, in columns (`fill-column' + 10)."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notelist-min-lines 10
  "Shortest useful result list, in lines (stacked arrangement)."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-notecontent-min-lines 20
  "Shortest useful note pane, in lines (stacked arrangement)."
  :type 'integer
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-other-window-action 'pane
  "What `nvemacs-note-open-other-window' does.
\\<nvemacs-mode-map>In the notelist that is \\[nvemacs-note-open-other-window].
`pane' opens the note in the note pane and moves the cursor there (in
the list-only arrangement it opens a temporary stacked pane; with a
prefix argument the cursor stays in the list).  `open' acts like
\\[nvemacs-note-open]."
  :type '(choice (const pane) (const open))
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-return-layout 'rebuild
  "What `nvemacs-searchbar-return' does after the frame is rearranged.
`rebuild' restores the nvEmacs arrangement of windows; `respect' leaves
your windows as they are and only shows the list."
  :type '(choice (const rebuild) (const respect))
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-searchbar-return-shows 'typed
  "What the search line shows after `nvemacs-searchbar-return'.
`typed' shows the search you typed.  With nothing typed, the note pane
clears too, so an empty search line never sits beside a note.
`typed-or-title' shows the search you typed.  With nothing typed, the
note stays selected and the search line shows its title.
`title' keeps the note selected and shows its title, even over a typed
search, as Notational Velocity does.
While a title is shown, typing starts a new search and DEL brings back
the search you typed."
  :type '(choice (const typed) (const typed-or-title) (const title))
  :package-version '(nvemacs . "0.0.4"))

(defconst nvemacs--placeholder-name "*nvEmacs: no note*")
(defconst nvemacs--homepage-name "*nvEmacs home*")
(defconst nvemacs--history-name "*nvEmacs history*" "The search history view.")
(defconst nvemacs--history-entry-name "*nvEmacs history entry*"
  "The note pane's view of one history entry.")
(defvar-local nvemacs--previewed nil "Path last shown in the note pane.")
(defvar-local nvemacs--preview-timer nil)
(defvar-local nvemacs--owned nil
  "Non-nil in a note buffer that nvEmacs created (it was not open before).")
(put 'nvemacs--owned 'permanent-local t)    ; survives a major-mode change
(defvar nvemacs--sweeping nil "Non-nil while `nvemacs--sweep' kills buffers.")

;; A preview visits the note before the user has chosen it: the cursor
;; only rested on its row. So a buffer a preview creates sets just the
;; note's SAFE local variables, without asking; the rest, and Emacs's
;; question about them, wait until the cursor enters the note.
(defvar-local nvemacs--locals-held nil
  "Non-nil in a note a preview visited with only its safe local variables.
`nvemacs--locals-release' sets the rest once the cursor is in the note.")

(defun nvemacs--locals-release ()
  "Set the local variables a preview held back, now the cursor is in the note.
Run from the note's `post-command-hook', once: it removes itself.  The
variables are set as when Emacs visits a file, asking about any that
are not known to be safe (see `enable-local-variables')."
  (when (and nvemacs--locals-held (eq (window-buffer) (current-buffer)))
    (setq nvemacs--locals-held nil)
    (remove-hook 'post-command-hook #'nvemacs--locals-release t)
    (with-local-quit                    ; C-g at the question sets nothing more
      (with-demoted-errors "nvEmacs: local variables: %S"
        (hack-local-variables 'no-mode)
        (when font-lock-mode (font-lock-flush))))))

(defun nvemacs--visit (path &optional preview)
  "Return a buffer visiting PATH, marked as owned if nvEmacs created it.
Only an owned buffer has its edits held in a temp file and re-indexes
its note when saved (see `nvemacs--note-attach').
With PREVIEW, a buffer created here sets only the note's safe local
variables; the others wait until the cursor enters the note."
  (unless (eql nvemacs--auto-save-armed nvemacs-auto-save-interval)
    (nvemacs--auto-save-arm))               ; the setting changed while no timer ran
  (let* ((existing (find-buffer-visiting path))
         ;; nil (set none) and :safe (never ask) need nothing held back.
         (hold (and preview (not existing) enable-local-variables
                    (not (eq enable-local-variables :safe))))
         (buf (or existing
                  (let ((enable-local-variables (if hold :safe enable-local-variables)))
                    (find-file-noselect path)))))
    (with-current-buffer buf
      (unless existing (setq nvemacs--owned t))
      (when nvemacs--owned (nvemacs--note-attach))
      (when hold
        (setq nvemacs--locals-held t)
        (add-hook 'post-command-hook #'nvemacs--locals-release nil t))
      (nvemacs-note-mode 1))
    buf))

(defun nvemacs--sweep ()
  "Kill the hidden, unmodified note buffers nvEmacs created.
Hidden means shown in no window (previewing ten notes must not leave
ten buffers open).  Buffers the user had open before are never
killed.  A note edited since it was opened stays until nvEmacs exits,
so its undo survives moving between notes; a new note back as it
started goes (it is not kept)."
  (let ((nvemacs--sweeping t))
    (dolist (b (buffer-list))
      (when (and (buffer-local-value 'nvemacs--owned b)
                 (not (buffer-modified-p b))
                 (not (get-buffer-window b t))
                 (or (not (buffer-local-value 'nvemacs--edited b))
                     (with-current-buffer b (nvemacs--untouched-p))))
        (kill-buffer b)))))

(defun nvemacs--fparam (p &optional frame)
  "Return frame parameter P of FRAME, by default the selected frame."
  (frame-parameter (or frame (selected-frame)) p))
(defun nvemacs--set-fparam (p v &optional frame)
  "Set frame parameter P of FRAME, by default the selected frame, to V."
  (set-frame-parameter (or frame (selected-frame)) p v))

(defcustom nvemacs-default-arrangement 'stacked
  "Arrangement nvEmacs uses when the frame is big enough for it.
`stacked' puts the search line over the list over the note; `side' puts
the note to the right of the list.  If the frame does not fit this
arrangement, the other one is tried, then the list alone.
\\<nvemacs-mode-map>\\[nvemacs-layout-cycle] overrides it per frame."
  :type '(choice (const stacked) (const side))
  :package-version '(nvemacs . "0.0.2"))

(defun nvemacs--auto-arrangement (&optional frame)
  "Return the arrangement that fits FRAME: `side', `stacked' or `list'.
`nvemacs-default-arrangement' comes first, then the other one, and
`list' when neither fits."
  (let* ((w (frame-width frame)) (h (frame-height frame))
         (fits `((side . ,(>= w (+ nvemacs-notelist-min-width nvemacs-notecontent-min-width)))
                 (stacked . ,(>= h (+ nvemacs-notelist-min-lines nvemacs-notecontent-min-lines)))))
         (other (if (eq nvemacs-default-arrangement 'side) 'stacked 'side)))
    (cond ((alist-get nvemacs-default-arrangement fits) nvemacs-default-arrangement)
          ((alist-get other fits) other)
          (t 'list))))

(defun nvemacs--arrangement (&optional frame)
  "Return the arrangement of FRAME, forced or automatic.
A forced one comes from `nvemacs-layout-cycle'.  The temporary pane of
`nvemacs-note-open-other-window' turns `list' into `stacked'."
  (let ((base (or (nvemacs--fparam 'nvemacs-override frame) (nvemacs--auto-arrangement frame))))
    (if (and (eq base 'list) (nvemacs--fparam 'nvemacs-temp frame)) 'stacked base)))

(defun nvemacs--list-window (&optional frame)
  "Return the window of FRAME that shows the list, or nil."
  (get-buffer-window nvemacs--buffer-name (or frame (selected-frame))))

(defun nvemacs--note-window (&optional frame)
  "Return the note pane of FRAME, or nil."
  (cl-find-if (lambda (w) (window-parameter w 'nvemacs-note))
              (window-list (or frame (selected-frame)) 'nomini)))

(defun nvemacs--placeholder ()
  "The \"No note selected\" buffer, drawn centered in its windows."
  (let ((buf (get-buffer nvemacs--placeholder-name)))
    (unless buf
      (setq buf (get-buffer-create nvemacs--placeholder-name))
      (with-current-buffer buf
        (setq-local cursor-type nil mode-line-format nil buffer-read-only t)
        (add-hook 'window-size-change-functions
                  (lambda (_w) (nvemacs--draw-placeholder)) nil t)))
    buf))

(defun nvemacs--draw-placeholder ()
  "Center \"No note selected\" vertically and horizontally."
  (let ((buf (get-buffer nvemacs--placeholder-name)))
    (when buf
      (with-current-buffer buf
        (let* ((win (get-buffer-window buf t))
               (lines (if win (/ (window-body-height win) 2) 10))
               (msg "No note selected")
               (inhibit-read-only t))
          (erase-buffer)
          (insert (make-string (max 0 (1- lines)) ?\n)
                  (propertize " " 'display `(space :align-to (- center ,(/ (length msg) 2))))
                  (propertize msg 'face 'shadow)))))))

(defun nvemacs--setup-note-window (win buffer)
  "Make WIN the note pane and show BUFFER in it.
With BUFFER nil, show the home page if it is up in WIN's frame, else
the \"No note selected\" placeholder."
  (set-window-parameter win 'nvemacs-note t)
  (let ((shown (or buffer (nvemacs--homepage-up-buffer (window-frame win)))))
    (set-window-buffer win (or shown (nvemacs--placeholder)))
    (unless shown (nvemacs--draw-placeholder))))

(defun nvemacs--apply-layout (&optional frame)
  "Lay out FRAME in its arrangement, keeping the note shown in the pane."
  (let* ((frame (or frame (selected-frame)))
         (arr (nvemacs--arrangement frame))
         (nw (nvemacs--note-window frame))
         (note (and nw (let ((b (window-buffer nw)))
                         (unless (equal (buffer-name b) nvemacs--placeholder-name) b)))))
    (with-selected-frame frame
      (let ((lw (or (nvemacs--list-window frame) (frame-first-window frame))))
        (select-window lw)
        (delete-other-windows lw)
        (set-window-parameter lw 'nvemacs-note nil)
        (set-window-buffer lw nvemacs--buffer-name)
        (pcase arr
          ('side (nvemacs--setup-note-window
                  (split-window lw (max nvemacs-notelist-min-width (/ (frame-width) 3)) 'right) note))
          ('stacked (nvemacs--setup-note-window
                     (split-window lw (max (+ nvemacs-notelist-min-lines 2) (/ (frame-height) 3)) 'below) note)))
        (nvemacs--set-fparam 'nvemacs-current arr frame)
        (select-window lw))))
  (nvemacs--sweep))

(defun nvemacs--show-in-pane (path &optional select)
  "Show the note at PATH in the note pane; SELECT moves the cursor there.
Without SELECT this is a preview (see `nvemacs--visit').  The note it
replaces is closed if nvEmacs opened it and it is unmodified."
  (let ((nw (nvemacs--note-window)))
    (when (and nw path)
      (nvemacs--set-fparam 'nvemacs-homepage-up nil)
      (let ((buf (nvemacs--visit path (not select))))
        (set-window-buffer nw buf)
        (with-current-buffer nvemacs--buffer-name (setq nvemacs--previewed path))
        (when select (select-window nw))
        (nvemacs--sweep)))))

(defvar-local nvemacs--on-row nil "Non-nil if point was on a result row after the last command.")

(defun nvemacs--clear-pane ()
  "Show \"No note selected\" in the note pane (nothing is selected).
The home page stays while it is up: only selecting or opening a note
ends it."
  (let ((nw (nvemacs--note-window)))
    (when (and nw (not (nvemacs--fparam 'nvemacs-homepage-up))
               (not (equal (buffer-name (window-buffer nw)) nvemacs--placeholder-name)))
      (nvemacs--setup-note-window nw nil)
      (with-current-buffer nvemacs--buffer-name (setq nvemacs--previewed nil))
      (nvemacs--sweep))))

(defun nvemacs--preview-now ()
  "Show the selected note in the note pane.
The pane follows the SELECTED ROW: with none, nothing is previewed (no
note is selected at start), and scrolling the list changes nothing."
  (let ((nw (nvemacs--note-window)))
    (when (and nw (nvemacs--selection))
      (let ((path (nvemacs--target-path)))
        ;; Compare with what the pane shows now, not with the last path
        ;; previewed: the sweep may have closed it.
        (unless (or (null path) (eq (window-buffer nw) (find-buffer-visiting path)))
          (nvemacs--show-in-pane path))))))

(defun nvemacs--schedule-preview ()
  "Preview the selected row in the note pane after `nvemacs-preview-delay'."
  (when (and (nvemacs--note-window) (eq major-mode 'nvemacs-mode))
    (when nvemacs--preview-timer (cancel-timer nvemacs--preview-timer))
    (let ((buf (current-buffer)))
      (setq nvemacs--preview-timer
            (run-with-idle-timer
             nvemacs-preview-delay nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf (nvemacs--preview-now)))))))))

(defun nvemacs--post-command ()
  "Selected title on the search line, then live preview.
Moving from a row onto the search line deselects and blanks the pane,
except when returning from a note with `nvemacs-searchbar-return': that
note stays shown."
  ;; With no rows, the empty line under the search line is not a place to be.
  (when (and nvemacs--query-start (null nvemacs--shown) (not (nvemacs--in-query-p)))
    (goto-char nvemacs--query-start)
    (end-of-line))
  ;; The end of the last row is at the window's right edge, where Emacs would
  ;; scroll the list sideways.  M-> and C-n past the end land there.
  (when (and nvemacs--shown (eobp)) (goto-char (line-beginning-position)))
  (nvemacs--selection-update)
  (let ((on-row (not (nvemacs--on-search-line-p))))
    ;; On a real row: a click on the prompt selects nothing.
    (when (and on-row (nvemacs--selection)) (nvemacs--set-fparam 'nvemacs-homepage-up nil))
    (when (and nvemacs--on-row (not on-row) (not (eq this-command 'nvemacs-searchbar-return)))
      (nvemacs--clear-pane))
    (setq nvemacs--on-row on-row))
  (nvemacs--grow-near-bottom)
  (when nvemacs-show-selected-title (nvemacs--show-selected-title))
  (nvemacs--schedule-preview))

;; TODO: `C-c C-]' breaks the Emacs key-binding conventions here.
;;
;; Context: `nvemacs-note-mode' is a MINOR mode, on in every note nvEmacs
;; opens, whatever the note's major mode (Org, Markdown, text).  The Elisp
;; manual, "Key Binding Conventions", reserves `C-c' followed by a control
;; character or a digit for MAJOR modes, and gives minor modes `C-c'
;; followed by punctuation other than {, }, <, >, : and ;.  A minor mode's
;; keymap takes precedence over the major mode's, so this binding hides
;; whatever the note's major mode puts on `C-c C-]'.  `C-c )' above is
;; within the conventions.
;;
;; Background: `C-c C-]' was chosen because Org leaves it free, so the
;; same key exits nvEmacs from the list and from an Org note.  Markdown
;; mode may bind it (believed, not checked: 2026-11-25 review of
;; public-v0.0.5), and any other major mode could.  Options: a punctuation
;; key such as `C-c ]', binding only where the major mode leaves the key
;; free, or leaving exit to the list and C-x C-c.  The search buffer's map
;; (`nvemacs-mode-map') is a major-mode map, so `C-c C-]' is fine there.
(defvar-keymap nvemacs-note-mode-map
  :doc "Keys in the notes nvEmacs shows."
  "C-c )" #'nvemacs-searchbar-return
  "C-c C-]" #'nvemacs-quit)                     ; Org leaves it free; see TODO

(define-minor-mode nvemacs-note-mode
  "Buffer-local mode in the notes nvEmacs shows.
\\<nvemacs-note-mode-map>\\[nvemacs-searchbar-return] returns to the list, \\[nvemacs-quit] exits nvEmacs.
nvEmacs turns the mode off in every buffer when it exits."
  :lighter " nv"
  (if nvemacs-note-mode
      (add-hook 'kill-buffer-hook #'nvemacs--note-killed nil t)
    (remove-hook 'kill-buffer-hook #'nvemacs--note-killed t)))

(defun nvemacs-note-open ()
  "Open the selected note, with the cursor in it.
The note opens in the note pane or, in the list-only arrangement, takes
over the frame.  On an empty search line, start an empty note; when
nothing matches, create a note named after the search.  A search with
parentheses names no note, and a rejected one or one with an empty group
opens nothing: the echo area says why."
  (interactive)
  (let* ((path (nvemacs--target-path))
         (typed (and (nvemacs--on-search-line-p) (not nvemacs--selected-title)))
         (syntax (and typed (nvemacs--query-check (nvemacs--search-text)))))
    (cond
     ((and typed (string-empty-p (string-trim (nvemacs--query))))
      (nvemacs-note-new ""))
     ;; With parentheses, the user's rules (2026-11-27): a rejected search
     ;; opens nothing, though the last list is still up; nor does an empty
     ;; group; and no note is named after the search.  C-c C-n still is.
     ((plist-get syntax :rejected) (nvemacs--boolean-say (plist-get syntax :rejected)))
     ((plist-get syntax :empty) (nvemacs--boolean-say 'empty))
     ((and (null path) (null nvemacs--shown) (plist-get syntax :grouped))
      (nvemacs--boolean-say 'no-note))
     ((and (null path) (null nvemacs--shown))
      (nvemacs-note-new (string-trim (nvemacs--search-text))))
     ((null path)
      (message "%s" (nvemacs--command-text 'nvemacs-note-open "requires a note's row"
                                           "Move to a row and then re-run the command")))
     (t (nvemacs--open-path path)))))

(defun nvemacs--open-path (path)
  "Show PATH in the note pane with the cursor in it, or take the frame."
  (nvemacs--history-end 'opened)
  (if (nvemacs--note-window)
      (nvemacs--show-in-pane path t)
    (nvemacs--set-fparam 'nvemacs-homepage-up nil)
    (let ((buf (nvemacs--visit path)))
      (nvemacs--set-fparam 'nvemacs-l1 (cons (current-window-configuration) buf))
      (delete-other-windows)
      (switch-to-buffer buf))))

(defun nvemacs-note-open-other-window (&optional stay)
  "Open the selected note in the note pane and move the cursor there.
With a prefix argument STAY, the cursor stays in the list.  In the
list-only arrangement, open a temporary stacked pane, which collapses
again on `nvemacs-searchbar-return' or when the note is killed.  With
`nvemacs-other-window-action' set to `open', act like
`nvemacs-note-open'."
  (interactive "P")
  (let ((path (nvemacs--target-path)))
    (cond
     ((eq nvemacs-other-window-action 'open) (nvemacs-note-open))
     ((not path)
      (message "%s" (nvemacs--command-text 'nvemacs-note-open-other-window
                                           "requires a note to open, and none is listed"
                                           "Change the search and then re-run the command")))
     (t
      (nvemacs--history-end 'opened)
      (unless (nvemacs--note-window)
        (nvemacs--set-fparam 'nvemacs-temp t)
        (nvemacs--apply-layout))
      (nvemacs--show-in-pane path (not stay))))))

(defun nvemacs--restore-l1-p ()
  "Non-nil if the frame still shows only the note that took it."
  (let ((l1 (nvemacs--fparam 'nvemacs-l1)))
    (and l1 (one-window-p t) (eq (window-buffer) (cdr l1)))))

(defun nvemacs--layout-intact-p ()
  "Non-nil if the frame shows exactly the nvEmacs arrangement."
  (let ((lw (nvemacs--list-window)) (nw (nvemacs--note-window))
        (n (length (window-list nil 'nomini))))
    (and lw (if (eq (nvemacs--arrangement) 'list) (= n 1) (and nw (= n 2))))))

(defun nvemacs--focus-query ()
  "Move the cursor to the end of the search line."
  (let ((lw (nvemacs--list-window)))
    (when lw
      (select-window lw)
      (with-current-buffer nvemacs--buffer-name
        (setq nvemacs--selected-id nil)
        (nvemacs--view-free-set nil)
        (goto-char nvemacs--query-start) (end-of-line)))))

(defun nvemacs--return-typed-empty-p ()
  "Return non-nil if no search is typed, even with a title on the search line."
  (with-current-buffer nvemacs--buffer-name
    (string-empty-p (string-trim (or (nvemacs--search-text) "")))))

(defun nvemacs--return-title-p ()
  "Return non-nil if a return should keep the note and show its title."
  (and (get-buffer nvemacs--buffer-name)
       (pcase nvemacs-searchbar-return-shows
         ('title t)
         ('typed-or-title (nvemacs--return-typed-empty-p)))))

(defun nvemacs--path-id (file)
  "Return the index id of the note FILE, or nil."
  (caar (sqlite-select (nvemacs--db) "SELECT id FROM meta WHERE path = ?"
                       (list (nvemacs--canonical-path file)))))

(defvar nvemacs--waiting)               ; defined with the pending edits

(defun nvemacs--return-note ()
  "Return the id of the note a return comes back from, or nil.
From the list that is the selected note; from elsewhere, the note in the
pane or the one that took the frame.  A new note has no id until it is
written, so its waiting edits are written here rather than after the
return."
  (or (and (eq (current-buffer) (get-buffer nvemacs--buffer-name)) (nvemacs--selection))
      (let* ((nw (nvemacs--note-window))
             (l1 (nvemacs--fparam 'nvemacs-l1))
             (b (cond (nw (window-buffer nw)) (l1 (cdr l1))))
             (file (and (buffer-live-p b) (buffer-file-name b))))
        (when file
          (or (nvemacs--path-id file)
              (when (and nvemacs-auto-save-interval (memq b nvemacs--waiting)
                         (not (buffer-local-value 'nvemacs--held b)))
                (nvemacs--commit b)
                (nvemacs--path-id file)))))))

(defun nvemacs--return-finish (id)
  "Finish a return as `nvemacs-searchbar-return-shows' says.
The cursor is on the search line.  ID is the note to go back to, if any:
on its row the search line shows its title, as on any selected row."
  (when (nvemacs--list-window)
    (with-current-buffer nvemacs--buffer-name
      (cond
       (id (nvemacs--show-id id)
           (when (eql (nvemacs--row-id) id) (setq nvemacs--selected-id id)))
       ((and (eq nvemacs-searchbar-return-shows 'typed) (nvemacs--return-typed-empty-p))
        (nvemacs--clear-pane))))))

;;;###autoload
(defun nvemacs-searchbar-return ()
  "Return to nvEmacs from anywhere.
Suggested global key: F8; `C-c )' in notes nvEmacs opened.
`nvemacs-searchbar-return-shows' sets what the search line shows then."
  (interactive)
  (let* ((l1 (nvemacs--restore-l1-p))
         (active (or l1 (nvemacs--fparam 'nvemacs-active)))
         (id (and active (nvemacs--return-title-p) (nvemacs--return-note))))
    (cond
     (l1
      (set-window-configuration (car (nvemacs--fparam 'nvemacs-l1)))
      (nvemacs--set-fparam 'nvemacs-l1 nil)
      (nvemacs--sweep)
      (nvemacs--focus-query))
     (active
      (when (nvemacs--fparam 'nvemacs-temp)        ; C-o's temporary pane collapses
        (nvemacs--set-fparam 'nvemacs-temp nil)
        (nvemacs--apply-layout))
      (if (eq nvemacs-return-layout 'rebuild)
          (unless (nvemacs--layout-intact-p) (nvemacs--apply-layout))
        (unless (nvemacs--list-window) (switch-to-buffer nvemacs--buffer-name)))
      (nvemacs--focus-query))
     (t (nvemacs)))
    (when active (nvemacs--return-finish id))))

(defun nvemacs--note-killed ()
  "Tidy up the frame after a note nvEmacs opened is killed.
In the full-frame case, restore the list; otherwise put \"No note
selected\" in the note pane.  A temporary pane of
`nvemacs-note-open-other-window' collapses."
  (let ((buf (current-buffer)) (frame (selected-frame)))
    (cond
     (nvemacs--sweeping nil)              ; nvEmacs closing it: no relayout
     ((nvemacs--restore-l1-p)
      (let ((config (car (nvemacs--fparam 'nvemacs-l1))))
        (nvemacs--set-fparam 'nvemacs-l1 nil)
        (run-at-time 0 nil (lambda () (set-window-configuration config) (nvemacs--focus-query)))))
     ((cl-some (lambda (w) (and (window-parameter w 'nvemacs-note) (eq (window-buffer w) buf)))
               (window-list frame 'nomini))
      (run-at-time 0 nil
                   (lambda ()
                     (with-selected-frame frame
                       (if (nvemacs--fparam 'nvemacs-temp)
                           (progn (nvemacs--set-fparam 'nvemacs-temp nil) (nvemacs--apply-layout))
                         (let ((nw (nvemacs--note-window)))
                           (when nw (nvemacs--setup-note-window nw nil))))
                       (when (get-buffer nvemacs--buffer-name)   ; it may be gone by now
                         (with-current-buffer nvemacs--buffer-name (setq nvemacs--previewed nil)))
                       (nvemacs--focus-query))))))))

(defun nvemacs-layout-cycle ()
  "Force an arrangement for this frame, the next in a cycle.
The cycle is auto, side, stacked, list, then auto again.  The forced
arrangement is reset when nvEmacs quits."
  (interactive)
  (let* ((order '(nil side stacked list))
         (next (or (cadr (memq (nvemacs--fparam 'nvemacs-override) order)) nil)))
    (nvemacs--set-fparam 'nvemacs-override next)
    (nvemacs--set-fparam 'nvemacs-temp nil)
    (nvemacs--apply-layout)
    (message "nvEmacs layout: %s" (if next next (format "auto (%s)" (nvemacs--auto-arrangement))))))

(defun nvemacs--maybe-relayout ()
  "After a frame resize: re-arrange if the auto arrangement changed."
  (when (and (nvemacs--fparam 'nvemacs-active)
             (not (nvemacs--fparam 'nvemacs-l1))
             (not (eq (nvemacs--arrangement) (nvemacs--fparam 'nvemacs-current))))
    (nvemacs--apply-layout)))

(defun nvemacs-index-refresh ()
  "Reconcile the index with the watched directories and search again."
  (interactive)
  (let ((r (nvemacs-index-reconcile)))
    (with-current-buffer nvemacs--buffer-name (nvemacs--search))
    (message "nvEmacs: %d new, %d changed, %d deleted%s" (nth 0 r) (nth 1 r) (nth 2 r)
             (nvemacs--skip-summary))))

(defun nvemacs-index-comments ()
  "Turn comment search on or off, and say which it now is.
Comments are in the index or not, so this asks, then rebuilds the index.
The choice is kept in settings.eld and survives a restart.  Answering no
changes nothing."
  (interactive)
  (nvemacs--require-sqlite)
  (nvemacs--comments-restore)
  (let* ((on (nvemacs--comments-indexed-p))
         (new (not on))
         (word (lambda (v) (if v "on" "off"))))
    (if (not (y-or-n-p (format "Turn comment search %s and rebuild the index? "
                               (funcall word new))))
        (message "nvEmacs: comment search stays %s" (funcall word on))
      (let ((saved (nvemacs--setting 'search-comments))
            (before nvemacs-search-comments)
            (done nil))
        (nvemacs--setting-put 'search-comments (if new 'on 'off))
        (unwind-protect
            (progn (nvemacs-index-rebuild) (setq done t))
          ;; A rebuild that stopped must not leave a choice it did not apply.
          (unless done
            (nvemacs--setting-put 'search-comments saved)
            (setq nvemacs-search-comments before))))
      ;; Previews change with comments, and a rebuild restarts the generation.
      (nvemacs--kept-drop)
      (let ((buf (get-buffer nvemacs--buffer-name)))
        (when buf (with-current-buffer buf (nvemacs--search))))
      (message "nvEmacs: comment search is %s" (funcall word new)))))

(defun nvemacs--leave-frame (&optional frame)
  "Clear FRAME's nvEmacs state and restore its layout from before.
That is the layout before `nvemacs' took the frame.  Return non-nil if
there was a layout to restore."
  (let* ((frame (or frame (selected-frame)))
         (l0 (nvemacs--fparam 'nvemacs-l0 frame)))
    (dolist (p '(nvemacs-active nvemacs-l0 nvemacs-override nvemacs-temp nvemacs-current nvemacs-l1
                 nvemacs-homepage-up))
      (nvemacs--set-fparam p nil frame))
    (when l0 (set-window-configuration l0) t)))

(defun nvemacs-quit ()
  "Exit nvEmacs and restore this frame's layout from before `nvemacs'.
The notes nvEmacs opened are written if their text changed, and closed
unless a window still shows them.  Its folder watches,
timers and save hook stop.
\\[nvemacs-searchbar-return] or \\[nvemacs] starts it again.
While another frame still shows nvEmacs, only this frame is left and
nvEmacs keeps running there."
  (interactive)
  (nvemacs--widths-save)
  (unless (nvemacs--leave-frame) (quit-window))
  (unless (cl-some (lambda (f) (frame-parameter f 'nvemacs-active)) (frame-list))
    (when (get-buffer nvemacs--buffer-name) (kill-buffer nvemacs--buffer-name)))
  (nvemacs--sweep))

(defun nvemacs--release-notes ()
  "Detach nvEmacs from every buffer, as it exits.
Turn off `nvemacs-note-mode' and remove nvEmacs's hooks, so a note left
open behaves as if nvEmacs had never shown it."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (nvemacs--note-detach)
      (when nvemacs-note-mode (nvemacs-note-mode -1)))))

(defun nvemacs--search-killed ()
  "Shut nvEmacs down, as *nvEmacs* is being killed.
This is its `kill-buffer-hook', run by `nvemacs-quit' in the last
nvEmacs frame and by any other kill.  Write the changed notes nvEmacs
opened (exiting is leaving them), stop the background work, and detach
nvEmacs from every note buffer.  Then leave every frame nvEmacs owns,
and close the placeholder and the unmodified notes nvEmacs opened
\(killing nvEmacs must not leave every previewed note open)."
  (dolist (tm (list nvemacs--timer nvemacs--preview-timer)) (when tm (cancel-timer tm)))
  (nvemacs--history-end 'quit)
  (nvemacs--grow-disable)
  (nvemacs--widths-save)
  (nvemacs--session-end)
  ;; Helper lists show nvEmacs state; stale once it is gone. The report stays: it may be kept to compare.
  (dolist (f (frame-list)) (set-frame-parameter f 'nvemacs-homepage-up nil))
  (dolist (b (list "*nvEmacs tags*" "*nvEmacs skipped*" "*nvEmacs keys*" nvemacs--homepage-name
                  nvemacs--history-name nvemacs--history-entry-name))
    (when (get-buffer b) (kill-buffer b)))
  (nvemacs--kept-drop)
  (when nvemacs-auto-save-interval (nvemacs--commit-all))
  (nvemacs--homepage-save)              ; after the last notes are written
  (nvemacs--sync-disable)
  (nvemacs--release-notes)
  (let ((frames (cl-remove-if-not (lambda (f) (frame-parameter f 'nvemacs-active)) (frame-list))))
    ;; After the kill completes: restoring layouts inside the hook would
    ;; race with `kill-buffer' replacing *nvEmacs* in its windows. Not if
    ;; nvEmacs was started again meanwhile: that would tear down the new
    ;; session's windows (found 2026-11-20: a test's cleanup timer fired
    ;; inside the next test).
    (run-at-time 0 nil
                 (lambda ()
                   (unless (get-buffer nvemacs--buffer-name)
                     (dolist (f frames)
                       (when (frame-live-p f) (with-selected-frame f (nvemacs--leave-frame f))))
                     (let ((p (get-buffer nvemacs--placeholder-name))) (when p (kill-buffer p)))
                     (nvemacs--sweep))))))

;;;; Creating notes

(defcustom nvemacs-new-note-directory nil
  "Directory where new notes go.
nil means the first of `nvemacs-watched-directories'.  It must be
inside a watched directory, or nvEmacs could not find the note again."
  :type '(choice (const nil) directory)
  :package-version '(nvemacs . "0.0.2"))

(defconst nvemacs--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory nvemacs.el was loaded from.")

(defcustom nvemacs-template-directory
  (expand-file-name "templates/" nvemacs--directory)
  "Directory of the template files for new notes.
The default is the templates shipped with nvEmacs (note.org, note.md,
note.txt).  Keep your own outside the watched directories, or in a
hidden subdirectory, so they are not indexed as notes."
  :type 'directory
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-new-note-extension "org"
  "Extension of blank new notes.
A blank note is one whose naming style names no template file."
  :type 'string
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-new-note-naming-styles
  '((diary :filename "{time:%Y-%m-%d-%Hh%Mm}-{title}" :template "note.org")
    (topic :filename "{title}" :template "note.org"))
  "Naming styles: NAME :filename PATTERN [:directory D] [:template F].
The template file (in `nvemacs-template-directory') gives the new note its
text and extension; a style without :template starts a blank note with
`nvemacs-new-note-extension'.  Placeholders: {title}, {time:FORMAT};
templates also {path} and {author} (the variable `user-full-name')."
  :type '(alist :key-type symbol :value-type plist)
  :package-version '(nvemacs . "0.0.2"))

(defcustom nvemacs-new-note-naming-style-default 'diary
  "Style used by \\<nvemacs-mode-map>\\[nvemacs-note-open] and \\[nvemacs-note-new]."
  :type 'symbol
  :package-version '(nvemacs . "0.0.2"))

(defconst nvemacs--reserved-names
  '("CON" "PRN" "AUX" "NUL" "COM1" "COM2" "COM3" "COM4" "COM5" "COM6" "COM7" "COM8" "COM9"
    "LPT1" "LPT2" "LPT3" "LPT4" "LPT5" "LPT6" "LPT7" "LPT8" "LPT9")
  "Windows reserved device names: get a trailing `_'.")

(defun nvemacs--clock () "Now; one reading per new note." (current-time))

(defun nvemacs--title-to-filename (title)
  "Return TITLE as a filename part.
The title is NFC-normalised; letters and digits of any script and `-'
are kept, with their case; everything else becomes `_'; runs are
collapsed; `_' and `-' are trimmed at both ends; there is never a
leading dot; reserved names get a `_'."
  (let* ((s (nvemacs--underscore title "[:alnum:]-"))
         (s (replace-regexp-in-string "\\`[-_.]+\\|[-_]+\\'" "" s)))
    (if (member (upcase s) nvemacs--reserved-names) (concat s "_") s)))

(defun nvemacs--expand (template title time path)
  "Fill the placeholders in TEMPLATE and return the result.
{title} becomes TITLE, {path} PATH, {author} the variable
`user-full-name', and {time:FORMAT} TIME formatted with FORMAT."
  (let ((re "{\\(title\\|path\\|author\\|time:\\([^}]*\\)\\)}"))
    (replace-regexp-in-string
     re (lambda (m)
          (save-match-data
            (string-match re m)
            (let ((key (match-string 1 m)))
              (cond ((equal key "title") title)
                    ((equal key "path") (or path ""))
                    ((equal key "author") (or user-full-name ""))
                    (t (format-time-string (match-string 2 m) time))))))
     template t t)))

(defun nvemacs--style-filename (pattern title time ext)
  "Return the filename for TITLE made by naming-style PATTERN.
The filename has no directory and no extension.  TIME fills the time
fields; an empty TITLE leaves a bare time stamp.  With `.EXT' added,
it is at most 255 bytes, cut at a character boundary."
  (let* ((name (nvemacs--expand pattern (nvemacs--title-to-filename title) time nil))
         (name (replace-regexp-in-string "[-_]+\\'" "" name))
         (max (- 255 1 (string-bytes ext))))
    (while (> (string-bytes name) max) (setq name (substring name 0 -1)))
    (replace-regexp-in-string "[-_]+\\'" "" name)))

(defun nvemacs--style-directory (style)
  "Return the directory where notes of naming STYLE go.
That is the style's :directory, relative to
`nvemacs-new-note-directory' or else the first watched directory."
  (let ((base (file-name-as-directory
               (expand-file-name (or nvemacs-new-note-directory
                                     (car nvemacs-watched-directories)))))
        (d (plist-get style :directory)))
    (file-name-as-directory (if d (expand-file-name d base) base))))

(defun nvemacs--existing-file (dir name)
  "Path of NAME in DIR compared case-insensitively (APFS), or nil."
  (let ((hit (and (file-directory-p dir)
                  (cl-find name (directory-files dir) :test #'string-equal-ignore-case))))
    (and hit (expand-file-name hit dir))))

(defvar-local nvemacs--new-note-start nil
  "In a note `nvemacs-note-new' created in this session: its text then.")
(put 'nvemacs--new-note-start 'permanent-local t)   ; survives a major-mode change

(defvar-local nvemacs--new-note-file nil
  "The file that `nvemacs-note-new' created this note as.
Set in the note's buffer, this session.  Only this file may go to the
Trash.")
(put 'nvemacs--new-note-file 'permanent-local t)

(defun nvemacs--buffer-text ()
  "Return the whole text of the current buffer, ignoring narrowing."
  (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))

(defun nvemacs--untouched-p ()
  "Non-nil in a note `nvemacs-note-new' created that reads as it started:
nothing typed, or the typing undone, and still visiting the file it was
created as (a buffer pointed at another file never sends that file to
the Trash)."
  (and nvemacs--new-note-start buffer-file-name
       (equal buffer-file-name nvemacs--new-note-file)
       (string= nvemacs--new-note-start (nvemacs--buffer-text))))

(defun nvemacs--move-to-trash (path)
  "Move PATH to the system Trash (macOS: Finder's; it can be put back)."
  (let ((delete-by-moving-to-trash t)) (move-file-to-trash path)))

(defun nvemacs--untouched-query ()
  "Let an untouched new note close without asking.
Run from `kill-buffer-query-functions': there is nothing to keep, so
there is no \"modified; kill anyway?\" question."
  (when (nvemacs--untouched-p) (set-buffer-modified-p nil))
  t)

(defun nvemacs--trash-if-untouched ()
  "Move a new note that is closed as it started to the Trash.
Run from `kill-buffer-hook'.  The file goes to the Trash if auto-save
wrote it, and leaves the index: a note created and then left empty, or
typed in and undone, is not kept."
  (when (and (nvemacs--untouched-p) (file-exists-p buffer-file-name))
    (let ((path buffer-file-name))
      (nvemacs--move-to-trash path)
      (nvemacs--forget path))))

;; Buffer-local in the new note, and kept when its major mode changes.
(put 'nvemacs--untouched-query 'permanent-local-hook t)
(put 'nvemacs--trash-if-untouched 'permanent-local-hook t)

(defun nvemacs-note-new (title)
  "Create a note titled TITLE in the default naming style and open it.
It opens as with `nvemacs-note-open'.  If the file already exists, it
is opened instead.  The note is only a buffer until it is first typed
into: nothing is written for a note abandoned empty (auto-save writes
it once modified), and one closed as it started, its typing undone,
goes to the Trash."
  (interactive (list (string-trim (with-current-buffer nvemacs--buffer-name
                                    (nvemacs--search-text)))))
  (nvemacs--history-end 'opened)
  (let* ((style (alist-get nvemacs-new-note-naming-style-default nvemacs-new-note-naming-styles))
         (time (nvemacs--clock))
         (tname (plist-get style :template))
         (ext (if tname (file-name-extension tname) nvemacs-new-note-extension))
         (dir (nvemacs--style-directory style))
         (name (concat (nvemacs--style-filename (plist-get style :filename) title time ext) "." ext))
         (existing (nvemacs--existing-file dir name))
         (path (or existing (expand-file-name name dir))))
    (unless (nvemacs--watched-note-p path)
      (nvemacs--command-error
       'nvemacs-note-new
       (format "requires a note type in a watched folder, and %s is not"
               (nvemacs--short-path path))
       "Check nvemacs-new-note-directory and the naming style, and then re-run the command"))
    (let ((missing nil))
      (unless existing
        ;; No :template: a blank note. A template file that cannot be read:
        ;; a blank note too, and a message (a built-in Org
        ;; text would be wrong in a .md note).
        (let* ((tfile (and tname nvemacs-template-directory
                           (expand-file-name tname nvemacs-template-directory)))
               (text (cond ((null tname) "")
                           ((and tfile (file-readable-p tfile))
                            (with-temp-buffer (insert-file-contents tfile) (buffer-string)))
                           (t (setq missing (or tfile tname)) "")))
               (buf (nvemacs--visit path)))
          (with-current-buffer buf
            (when (= (buffer-size) 0)
              (insert (nvemacs--expand text title time path))
              (normal-mode)                   ; major mode and #+STARTUP from the text
              (nvemacs-note-mode 1)
              (set-buffer-modified-p nil)
              (setq nvemacs--new-note-start (nvemacs--buffer-text)
                    nvemacs--new-note-file buffer-file-name)
              (nvemacs--not-an-edit)
              (add-hook 'kill-buffer-query-functions #'nvemacs--untouched-query nil t)
              (add-hook 'kill-buffer-hook #'nvemacs--trash-if-untouched nil t)
              (goto-char (point-max))))))
      (nvemacs--open-path path)
      (when missing
        (nvemacs--command-message
         'nvemacs-note-new
         (format "requires the template %s, which was not found, so the note starts blank"
                 (nvemacs--short-path missing))
         "Set nvemacs-template-directory to the folder that holds it")))))

;;;; Pending edits: a temp file until a save or leaving the note
;;
;; Auto-save used to write the note itself, so an accidental edit, undone,
;; left the note with today's date and moved it up the list.  Now edits
;; wait in a temp file under the data directory.  The note is written by a
;; save, by the cursor leaving it, or by exiting nvEmacs, and only if its
;; text changed.

(defconst nvemacs--discarded-days 7
  "Days a discarded temp file stays in discarded/ before it is deleted.")

(defvar nvemacs--waiting nil "Note buffers with edits not yet written to their files.")
(defvar nvemacs--committing nil "Non-nil while nvEmacs writes a note (not a save by hand).")

(defvar-local nvemacs--attached nil "Non-nil in a note whose edits nvEmacs is looking after.")
(defvar-local nvemacs--edited nil
  "Non-nil in a note edited since it was opened; it stays open until exit.")
(defvar-local nvemacs--base nil
  "The note before its first edit, (TEXT . MTIME); MTIME is nil for a new file.
A write that brings the text back to TEXT also brings the date back.")
(defvar-local nvemacs--pending-file nil "This note's temp file, or nil.")
(defvar-local nvemacs--pending-tick nil "`buffer-chars-modified-tick' at the last temp write.")
(defvar-local nvemacs--held nil "Non-nil while this note's edits cannot be written.")
(defvar-local nvemacs--held-told nil "The reason last shown for holding this note.")
(defvar-local nvemacs--tags-stay nil
  "Non-nil after a tag edit from the list: written when the cursor leaves the row.")
(defvar-local nvemacs--sister-held nil
  "(TAGS) waiting to go to this note's sister file, or nil.")
(defvar-local nvemacs--save-skipped nil
  "Non-nil during a save that found nothing to write to the file.")
(dolist (v '(nvemacs--attached nvemacs--edited nvemacs--base nvemacs--pending-file
             nvemacs--pending-tick nvemacs--held nvemacs--held-told nvemacs--tags-stay
             nvemacs--sister-held nvemacs--save-skipped))
  (put v 'permanent-local t))           ; they survive a major-mode change

(defconst nvemacs--note-hooks
  '((first-change-hook . nvemacs--first-change)
    (write-contents-functions . nvemacs--save-unchanged)
    (after-save-hook . nvemacs--after-save)
    (kill-buffer-query-functions . nvemacs--kill-query)
    (kill-buffer-hook . nvemacs--note-closing)
    (before-revert-hook . nvemacs--before-revert)
    (after-revert-hook . nvemacs--after-revert))
  "The buffer-local hooks of a note nvEmacs opened.")
(dolist (h nvemacs--note-hooks) (put (cdr h) 'permanent-local-hook t))
(put 'nvemacs--pending-delete 'permanent-local-hook t)

(defun nvemacs--note-attach ()
  "Start looking after this note's edits.
Emacs's own auto-save goes off here while nvEmacs's temp file does its
job: with edits waiting it would put a #note# file in the notes folder,
where a sync service would pick it up."
  (setq nvemacs--attached t)
  (pcase-dolist (`(,hook . ,fn) nvemacs--note-hooks) (add-hook hook fn nil t))
  (nvemacs--emacs-auto-save-off))

(defun nvemacs--emacs-auto-save-off ()
  "Turn off Emacs's auto-save here, if nvEmacs's temp files are in use."
  (when (and nvemacs-auto-save-interval buffer-auto-save-file-name)
    (auto-save-mode -1)))

(defun nvemacs--note-detach ()
  "Stop looking after this note's edits, as nvEmacs exits.
A temp file still here holds edits that could not be written.  It stays
until the note is saved, or for recovery at the next start."
  (when nvemacs--attached
    (pcase-dolist (`(,hook . ,fn) nvemacs--note-hooks) (remove-hook hook fn t))
    (setq nvemacs--attached nil nvemacs--edited nil nvemacs--base nil nvemacs--held nil
          nvemacs--held-told nil nvemacs--tags-stay nil
          nvemacs--waiting (delq (current-buffer) nvemacs--waiting))
    (when (and auto-save-default buffer-file-name) (auto-save-mode 1))
    (when nvemacs--pending-file
      (add-hook 'after-save-hook #'nvemacs--pending-delete nil t))))

(defun nvemacs--first-change ()
  "Remember this note as it was; run by `first-change-hook'.
An edit in a note ends the search in progress, as opening it would."
  (nvemacs--history-end 'opened)
  (when (and nvemacs-auto-save-interval buffer-file-name
             ;; Renamed in Emacs 31; nvEmacs supports 29 and up.
             (not (symbol-value (if (boundp 'revert-buffer-in-progress)
                                    'revert-buffer-in-progress
                                  'revert-buffer-in-progress-p))))
    (setq nvemacs--edited t)
    (unless nvemacs--base
      (let ((m (visited-file-modtime)))
        (setq nvemacs--base (cons (nvemacs--buffer-text) (unless (memql m '(0 -1)) m)))))
    (cl-pushnew (current-buffer) nvemacs--waiting)))

(defun nvemacs--not-an-edit ()
  "Forget the change just made: it was a new note's template."
  (setq nvemacs--base nil nvemacs--edited nil
        nvemacs--waiting (delq (current-buffer) nvemacs--waiting)))

(defun nvemacs--pending-dir ()
  "Return the directory of the temp files."
  (expand-file-name "pending/" nvemacs-data-directory))

(defun nvemacs--discarded-dir ()
  "Return the directory of discarded temp files."
  (expand-file-name "discarded/" nvemacs-data-directory))

(defun nvemacs--pending-claim (path)
  "Return the temp file for note PATH, named from its full path.
One left by a crash is the only copy of those edits: it is renamed, and
stays listed by `nvemacs-edits-recover'."
  (let* ((dir (nvemacs--private-directory (nvemacs--pending-dir)))
         (stem (expand-file-name (secure-hash 'sha1 (encode-coding-string path 'utf-8)) dir))
         (file (concat stem ".eld")))
    (when (file-exists-p file)
      (rename-file file (concat (make-temp-name (concat stem "-")) ".eld")))
    file))

(defun nvemacs--pending-write ()
  "Copy this note's edits to its temp file.
The first line is a printed plist, the rest the text."
  (let* ((path (nvemacs--canonical-path buffer-file-name))
         (file (or nvemacs--pending-file (nvemacs--pending-claim path)))
         (m (visited-file-modtime))
         (tick (buffer-chars-modified-tick))
         (text (nvemacs--buffer-text))
         (meta (list :path path
                     :file-mtime (unless (memql m '(0 -1)) (time-convert m 'list))
                     :saved (format-time-string "%F %T")
                     :edited (and (buffer-modified-p) t)
                     :sister nvemacs--sister-held)))
    (with-temp-buffer
      (let ((print-escape-newlines t) (print-length nil) (print-level nil))
        (prin1 meta (current-buffer)))
      (insert "\n" text)
      (let ((coding-system-for-write 'utf-8-emacs-unix))
        (nvemacs--write-file file)))
    (setq nvemacs--pending-file file nvemacs--pending-tick tick)))

(defun nvemacs--pending-read (file)
  "Return temp FILE as a plist: its header, :file and :text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-emacs-unix)) (insert-file-contents file))
    (goto-char (point-min))
    (let ((meta (read (current-buffer))))
      (unless (stringp (plist-get meta :path)) (error "Not a temp file: %s" file))
      (forward-line 1)
      (append (list :file file :text (buffer-substring-no-properties (point) (point-max)))
              meta))))

(defun nvemacs--pending-delete ()
  "Delete this note's temp file: nothing in it needs keeping."
  (when nvemacs--pending-file
    (ignore-errors (delete-file nvemacs--pending-file))
    (setq nvemacs--pending-file nil nvemacs--pending-tick nil)))

(defun nvemacs--discard-file (file path)
  "Move temp FILE, of note PATH, to discarded/; its 7 days start now."
  (let* ((base (file-name-base path))
         (dest (expand-file-name
                (format "%s-%s-%s.eld" (format-time-string "%Y-%m-%d-%Hh%Mm%S")
                        (substring base 0 (min 40 (length base)))
                        (substring (file-name-base file) 0 8))
                (nvemacs--private-directory (nvemacs--discarded-dir)))))
    (rename-file file dest t)
    (set-file-times dest)))

(defun nvemacs--discard-edits ()
  "Move this note's edits, as they stand now, to discarded/."
  (nvemacs--pending-write)
  (nvemacs--discard-file nvemacs--pending-file (nvemacs--canonical-path buffer-file-name))
  (setq nvemacs--pending-file nil nvemacs--pending-tick nil))

(defun nvemacs--discarded-prune ()
  "Delete files in discarded/ older than `nvemacs--discarded-days'."
  (let ((dir (nvemacs--discarded-dir))
        (limit (time-subtract nil (* nvemacs--discarded-days 86400))))
    (when (file-directory-p dir)
      (dolist (f (directory-files dir t "\\.eld\\'"))
        (when (time-less-p (file-attribute-modification-time (file-attributes f)) limit)
          (ignore-errors (delete-file f)))))))

(defun nvemacs--same-as-file-p ()
  "Non-nil if this buffer's text is exactly what its file holds.
Emacs's modified flag cannot say: it stays set when typing is deleted
by hand rather than undone."
  (let ((file buffer-file-name) (coding buffer-file-coding-system) (buf (current-buffer)))
    (and file (file-regular-p file)
         (with-temp-buffer
           (let ((coding-system-for-read coding)) (insert-file-contents file))
           (let ((tmp (current-buffer)))
             (with-current-buffer buf
               (save-restriction
                 (widen)
                 (let ((case-fold-search nil))
                   (and (= (buffer-size) (buffer-size tmp))
                        (zerop (compare-buffer-substrings buf nil nil tmp nil nil)))))))))))

(defun nvemacs--pending-save ()
  "Copy the edits of the notes nvEmacs opened to their temp files.
The notes themselves are not touched.  Edits that no longer change the
note end here: the buffer is marked unmodified and the temp file goes."
  (dolist (b (buffer-list))
    (when (and (buffer-local-value 'nvemacs--attached b)
               (buffer-file-name b)
               (or (buffer-modified-p b) (buffer-local-value 'nvemacs--sister-held b)))
      (with-current-buffer b
        (with-demoted-errors "nvEmacs: writing a temp file: %S"
          (cl-pushnew b nvemacs--waiting)
          (setq nvemacs--held nil)      ; look again at the next check
          (unless (and nvemacs--pending-file
                       (eql nvemacs--pending-tick (buffer-chars-modified-tick)))
            (unless (nvemacs--settle)
              (nvemacs--pending-write))))))))

(defun nvemacs--settle ()
  "If this note has nothing to write, mark it unmodified; return non-nil if so."
  (when (and (buffer-modified-p) (verify-visited-file-modtime)
             (or (and (nvemacs--untouched-p) (not (file-exists-p buffer-file-name)))
                 (nvemacs--same-as-file-p)))
    (set-buffer-modified-p nil)
    (if nvemacs--sister-held (nvemacs--pending-write) (nvemacs--pending-delete))
    t))

(defun nvemacs--settle-unchanged (&rest _)
  "Before Emacs asks which buffers to save, clear notes with nothing to write.
Typing deleted by hand keeps the modified flag until the next idle check,
and Emacs would ask about a note that has not changed.  This is `:before'
advice on `save-some-buffers' and `save-buffers-kill-emacs' while nvEmacs
runs."
  (dolist (b (copy-sequence nvemacs--waiting))
    (when (buffer-live-p b)
      (with-current-buffer b
        (with-demoted-errors "nvEmacs: checking a note: %S"
          (when buffer-file-name (nvemacs--settle)))))))

(defun nvemacs--save-unchanged ()
  "Skip a save that would not change the file, keeping its date.
Run from `write-contents-functions'.  A new note has no file yet, so
saving it still creates it."
  (when (and (not nvemacs--committing) (buffer-modified-p) (nvemacs--same-as-file-p))
    (set-buffer-modified-p nil)
    (setq nvemacs--save-skipped t)
    (message "(No changes need to be saved)")
    t))

(defun nvemacs--hold (reason)
  "Keep this note's edits unwritten because of REASON, and say so once."
  (nvemacs--pending-write)
  (setq nvemacs--held t)
  (unless (equal reason nvemacs--held-told)
    (setq nvemacs--held-told reason)
    (message "nvEmacs: %s: %s.  Your edits are kept: C-x C-s to write them, M-x revert-buffer to discard them"
             (file-name-nondirectory buffer-file-name) reason)))

(defun nvemacs--sister-flush ()
  "Write this note's held sister-file tags, if they differ from the file."
  (let ((tags (car nvemacs--sister-held)) (path buffer-file-name))
    (setq nvemacs--sister-held nil)
    (unless (equal tags (mapcar #'car (nvemacs--tags-clean (nvemacs--sister-tags path))))
      (nvemacs--sister-write path tags)
      (when nvemacs--db (nvemacs--reindex path)))))

(defun nvemacs--commit (b)
  "Write note buffer B's edits to its file, if they change it.
Nothing is overwritten: a file changed or deleted elsewhere, or one
that cannot be written, leaves the edits in the buffer and temp file."
  (with-current-buffer b
    (let ((file buffer-file-name) (hold nil))
      (setq nvemacs--tags-stay nil)
      (when file
        (when nvemacs--sister-held (nvemacs--sister-flush))
        (when (buffer-modified-p)
          (cond
           ((and (nvemacs--untouched-p) (not (file-exists-p file)))
            (set-buffer-modified-p nil))
           ((not (verify-visited-file-modtime b))
            (setq hold (if (file-exists-p file) "changed on disk" "deleted from disk")))
           ((and (file-exists-p file) (not (file-writable-p file)))
            (setq hold "not writable"))
           ((nvemacs--same-as-file-p) (set-buffer-modified-p nil))
           (t (condition-case err
                  (let ((nvemacs--committing t) (save-silently t)) (save-buffer))
                (error (setq hold (error-message-string err))))))))
      (if hold
          (nvemacs--hold hold)
        (nvemacs--pending-delete)
        (setq nvemacs--held nil nvemacs--held-told nil
              nvemacs--waiting (delq b nvemacs--waiting))))))

(defun nvemacs--commit-all ()
  "Write the edits of every note nvEmacs opened, as it exits."
  (dolist (b (buffer-list))
    (when (buffer-local-value 'nvemacs--attached b)
      (with-demoted-errors "nvEmacs: writing a note: %S" (nvemacs--commit b)))))

(defun nvemacs--cursor-buffer ()
  "Return the buffer the user is working in.
During a minibuffer prompt, that is the buffer the prompt came from."
  (let ((w (selected-window)))
    (window-buffer (if (window-minibuffer-p w) (or (minibuffer-selected-window) w) w))))

(defun nvemacs--on-row-p (b)
  "Non-nil if the cursor is in the list, on the row of note buffer B."
  (let ((list (get-buffer nvemacs--buffer-name)))
    (and list (eq (nvemacs--cursor-buffer) list)
         (with-current-buffer list
           (let ((path (nvemacs--tags-target)))
             (and path (eq (find-buffer-visiting path) b)))))))

(defun nvemacs--check-left ()
  "Write the notes the cursor has left; run after every command.
A signal here would make Emacs drop this function from
`post-command-hook', so errors are only shown."
  ;; Runs after the list's own hook.  A command reading the minibuffer has
  ;; not finished yet.
  (when (zerop (minibuffer-depth)) (setq nvemacs--command-here nil))
  ;; A new note saved by hand is kept, even with nothing to write: Emacs
  ;; runs no save hook then, so the command itself is the only sign.
  (when (and nvemacs--new-note-start (eq this-command 'save-buffer))
    (setq nvemacs--new-note-start nil))
  (when (and nvemacs--waiting nvemacs-auto-save-interval)
    (with-demoted-errors "nvEmacs: writing a note: %S"
      (let ((here (nvemacs--cursor-buffer)))
        (dolist (b (copy-sequence nvemacs--waiting))
          (cond
           ((not (buffer-live-p b)) (setq nvemacs--waiting (delq b nvemacs--waiting)))
           ((eq b here) (with-current-buffer b (setq nvemacs--tags-stay nil)))
           ((buffer-local-value 'nvemacs--held b))   ; looked at again when idle
           ((and (buffer-local-value 'nvemacs--tags-stay b) (nvemacs--on-row-p b)))
           (t (nvemacs--commit b))))))))

(defun nvemacs--restore-date ()
  "Give the file its old date if the text just written is the old text."
  (when (and (cdr nvemacs--base)
             (string= (car nvemacs--base) (nvemacs--buffer-text)))
    (set-file-times buffer-file-name (cdr nvemacs--base))
    (set-visited-file-modtime)))

(defun nvemacs--after-save ()
  "Finish a write of this note and re-index it.
This is a buffer-local `after-save-hook' of the notes nvEmacs opened
itself (see `nvemacs--visit'); saving any other buffer runs nothing of
nvEmacs.  A note saved elsewhere reaches the index at the next rescan,
as a change made outside Emacs does.  A save by hand that wrote is
final: the note's old date does not come back, and a new note is kept.
One that found nothing to write (`nvemacs--save-unchanged') changes
neither."
  (let ((skipped nvemacs--save-skipped))
    (setq nvemacs--save-skipped nil)
    (cond (skipped nil)
          (nvemacs--committing (nvemacs--restore-date))
          (t (setq nvemacs--base nil nvemacs--new-note-start nil)))
    (when nvemacs--sister-held (nvemacs--sister-flush))
    (nvemacs--pending-delete)
    (setq nvemacs--held nil nvemacs--held-told nil nvemacs--tags-stay nil
          nvemacs--waiting (delq (current-buffer) nvemacs--waiting))
    (when (and (not skipped) nvemacs--db (nvemacs--watched-note-p buffer-file-name))
      (nvemacs--reindex buffer-file-name))))

(defun nvemacs--kill-query ()
  "Spare a note that has not really changed Emacs's kill question.
Run from `kill-buffer-query-functions'."
  (when (and (buffer-modified-p) (nvemacs--same-as-file-p))
    (set-buffer-modified-p nil))
  t)

(defun nvemacs--note-closing ()
  "A note is being killed: settle its edits.
Run from `kill-buffer-hook'.  Still modified here means the user chose
to kill it without saving, a discard.  Tags alone raise no question, so
closing the note writes them, as leaving it would."
  (with-demoted-errors "nvEmacs: closing a note: %S"
    (setq nvemacs--waiting (delq (current-buffer) nvemacs--waiting))
    (cond
     ((and buffer-file-name nvemacs-auto-save-interval (buffer-modified-p)
           (not (nvemacs--untouched-p)) (not (nvemacs--same-as-file-p)))
      (nvemacs--discard-edits))
     (t (when (and buffer-file-name nvemacs--sister-held) (nvemacs--sister-flush))
        (nvemacs--pending-delete)))))

(defun nvemacs--before-revert ()
  "`revert-buffer' discards this note's edits: keep a copy in discarded/."
  (with-demoted-errors "nvEmacs: discarding edits: %S"
    (if (or nvemacs--sister-held
            (and (buffer-modified-p) (not (nvemacs--same-as-file-p))))
        (nvemacs--discard-edits)
      (nvemacs--pending-delete)))
  (setq nvemacs--sister-held nil))

(defun nvemacs--after-revert ()
  "The note is what its file holds again: start afresh."
  (setq nvemacs--base nil nvemacs--edited nil nvemacs--held nil nvemacs--held-told nil
        nvemacs--tags-stay nil nvemacs--waiting (delq (current-buffer) nvemacs--waiting))
  (unless nvemacs-note-mode (nvemacs-note-mode 1)))  ; reverting resets the modes

(defun nvemacs--quit-query ()
  "Emacs is exiting, and the user has answered its save questions.
A note still modified was answered no, a discard.  Run from
`kill-emacs-query-functions', so a crash or shutdown, which never gets
here, keeps its temp files for recovery.  If exiting is cancelled after
this, the next auto-save writes a new temp file."
  (dolist (b (buffer-list))
    (when (buffer-local-value 'nvemacs--attached b)
      (with-current-buffer b
        (with-demoted-errors "nvEmacs: discarding edits: %S"
          (when (and buffer-file-name nvemacs--sister-held (not (buffer-modified-p)))
            (nvemacs--sister-flush))
          (if (and buffer-file-name (buffer-modified-p)
                   (not (nvemacs--untouched-p)) (not (nvemacs--same-as-file-p)))
              (nvemacs--discard-edits)
            (nvemacs--pending-delete))))))
  t)

;;;;; Recovery

(defun nvemacs--pending-records ()
  "Return the temp files no open note is using, as plists, oldest first.
These are edits left by a crash or forced quit.  An unreadable file is
skipped, and left where it is."
  (let ((dir (nvemacs--pending-dir))
        (live (delq nil (mapcar (lambda (b) (buffer-local-value 'nvemacs--pending-file b))
                                (buffer-list)))))
    (when (file-directory-p dir)
      (delq nil (mapcar (lambda (f) (unless (member f live)
                                      (ignore-errors (nvemacs--pending-read f))))
                        (directory-files dir t "\\`[^.].*\\.eld\\'"))))))

(defun nvemacs--recover-notice ()
  "Say so at startup if a crash left unsaved edits."
  (let ((n (length (nvemacs--pending-records))))
    (when (> n 0)
      (message "nvEmacs: %d note%s unsaved edits: M-x nvemacs-edits-recover to review them"
               n (if (= n 1) " has" "s have")))))

(defun nvemacs--recover-status (rec)
  "Describe the note of temp-file record REC as it is on disk now."
  (let* ((a (file-attributes (plist-get rec :path)))
         (m (plist-get rec :file-mtime)))
    (cond ((null a) (if m "note is gone" "new note, never written"))
          ((and m (time-equal-p m (file-attribute-modification-time a))) "note unchanged since")
          (t "NOTE CHANGED SINCE"))))

(defvar-keymap nvemacs-edits-mode-map
  :doc "Keys of the *nvEmacs recover* list."
  "RET" #'nvemacs-edits-use
  "u" #'nvemacs-edits-use
  "d" #'nvemacs-edits-diff
  "x" #'nvemacs-edits-discard
  "g" #'nvemacs-edits-recover)

(define-derived-mode nvemacs-edits-mode special-mode "nvEmacs recover"
  "List of notes with unsaved edits left by a crash or forced quit.
\\{nvemacs-edits-mode-map}")

(defun nvemacs--recover-draw ()
  "Draw the list of unsaved edits in the current buffer."
  (let ((inhibit-read-only t) (recs (nvemacs--pending-records)))
    (erase-buffer)
    (if (null recs)
        (insert "No unsaved edits to recover.\n")
      (insert "Unsaved edits left by a crash or forced quit.\n"
              "d: compare with the note   RET: use them   x: discard them   g: refresh   q: quit\n\n")
      (dolist (rec recs)
        (insert (propertize (format "%s  %-24s %s\n" (or (plist-get rec :saved) "?")
                                    (nvemacs--recover-status rec) (plist-get rec :path))
                            'nvemacs-edits-recover rec))))
    (goto-char (point-min))
    (let ((p (next-single-property-change (point-min) 'nvemacs-edits-recover)))
      (when p (goto-char p)))))

(defun nvemacs-edits-recover ()
  "List notes with unsaved edits left by a crash or forced quit.
Each can be compared with its note, used or discarded.  Nothing is
written to a note until you choose.  The list opens in the note pane,
so it does not cover the notelist."
  (interactive)
  (let ((buf (get-buffer-create "*nvEmacs recover*"))
        (pane (and (get-buffer nvemacs--buffer-name) (nvemacs--note-window))))
    (with-current-buffer buf
      (nvemacs-edits-mode)
      (nvemacs--recover-draw))
    (if pane
        (progn (set-window-buffer pane buf) (select-window pane))
      (pop-to-buffer buf))))

(defun nvemacs--recover-at-point (command)
  "Return the temp-file record on this line, for COMMAND."
  (or (get-text-property (line-beginning-position) 'nvemacs-edits-recover)
      (nvemacs--command-error command "requires a line that lists a note"
                              "Move to one and then re-run the command")))

(defun nvemacs-edits-use ()
  "Put the edits on this line back into their note, for you to review.
The note's file is written as usual: when the cursor leaves the note."
  (interactive)
  (let* ((rec (nvemacs--recover-at-point 'nvemacs-edits-use)) (path (plist-get rec :path)))
    (unless (get-buffer nvemacs--buffer-name)
      (nvemacs--command-error 'nvemacs-edits-use "requires that nvEmacs is running"
                              "Run M-x nvemacs and then re-run the command"))
    ;; The file names where it would write: never outside the notes.
    (unless (nvemacs--watched-note-p path)
      (nvemacs--command-error
       'nvemacs-edits-use (format "requires a note in a watched folder, and %s is not"
                                  (nvemacs--short-path path))
       "Add its folder to nvemacs-watched-directories and then re-run the command"))
    (let ((buf (nvemacs--visit path)))
      (with-current-buffer buf
        (when (buffer-modified-p)
          (nvemacs--command-error
           'nvemacs-edits-use
           (format "requires that %s has no unsaved edits" (file-name-nondirectory path))
           "Save or revert them and then re-run the command"))
        (when (plist-get rec :edited)
          (let ((inhibit-read-only t))
            (save-restriction (widen) (erase-buffer) (insert (plist-get rec :text)))))
        (setq nvemacs--sister-held (plist-get rec :sister)
              nvemacs--edited t
              nvemacs--pending-file (plist-get rec :file)
              nvemacs--pending-tick nil)
        (cl-pushnew buf nvemacs--waiting))
      (nvemacs--recover-draw)
      (nvemacs--open-path path)
      (message "nvEmacs: Edits restored; written when you leave the note (M-x revert-buffer discards them)"))))

(declare-function diff-no-select "diff" (old new &optional switches no-async buf))

(defun nvemacs-edits-diff ()
  "Show how the edits on this line differ from their note."
  (interactive)
  (require 'diff)
  (let* ((rec (nvemacs--recover-at-point 'nvemacs-edits-diff))
         (path (plist-get rec :path))
         (old (if (file-exists-p path) path (generate-new-buffer " *nvEmacs recover: none*")))
         (new (generate-new-buffer " *nvEmacs recover: edits*"))
         ;; The copies diff compares hold note text: not in the system temp.
         (temporary-file-directory (nvemacs--private-directory (nvemacs--pending-dir))))
    (unwind-protect
        (progn
          (with-current-buffer new (insert (plist-get rec :text)))
          (display-buffer (diff-no-select old new nil t (get-buffer-create "*nvEmacs recover diff*"))))
      (kill-buffer new)
      (when (bufferp old) (kill-buffer old)))))

(defun nvemacs-edits-discard ()
  "Discard the edits on this line: they move to discarded/ for 7 days."
  (interactive)
  (let ((rec (nvemacs--recover-at-point 'nvemacs-edits-discard)))
    (nvemacs--discard-file (plist-get rec :file) (plist-get rec :path))
    (nvemacs-edits-recover)
    (message "nvEmacs: Discarded (kept for %d days in %s)"
             nvemacs--discarded-days (nvemacs--discarded-dir))))

;;;; Tags: writing and commands

(defun nvemacs--org-set-filetags (tags)
  "Make TAGS the `#+filetags:' of the Org text in the current buffer.
Every existing line stays where it is: a tag that remains stays on its
line, a removed tag comes off the line holding it, and a new tag goes on
the first line.  A line whose tags do not change is left exactly as it
is.  With no line, one goes between `#+Weather:' and `#+Notes:', else at
the end of the leading keyword block, else at the top after a -*- line.
No tags leaves an empty line."
  (let* ((case-fold-search t)
         (limit (copy-marker (nvemacs--org-header-end)))
         (fold (lambda (tg) (nvemacs--fold (or (nvemacs--tag-normalize tg) tg))))
         (left (copy-sequence tags))
         (lines nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+filetags:\\([ \t]*\\)\\(.*\\)$" limit t)
        (push (list (copy-marker (match-beginning 1)) (copy-marker (match-end 2) t)
                    (match-string-no-properties 1)
                    (split-string (match-string-no-properties 2) "[: \t]+" t))
              lines))
      (setq lines (nreverse lines))
      (if (null lines)
          (let ((value (if tags (concat ":" (mapconcat #'identity tags ":") ":") "")))
            (goto-char (nvemacs--org-filetags-place limit))
            (unless (bolp) (insert "\n"))
            (insert "#+filetags: " value "\n"))
        (let ((kept (mapcar
                     (lambda (l)
                       (cl-loop for tok in (nth 3 l)
                                for tg = (cl-find (funcall fold tok) left
                                                  :key fold :test #'equal)
                                when tg collect tg and do (setq left (remq tg left))))
                     lines)))
          ;; One line takes TAGS as given; with several, new tags join the first.
          (if (cdr lines)
              (setcar kept (append (car kept) left))
            (setq kept (list tags)))
          (cl-mapc
           (lambda (l new)
             (unless (equal new (nth 3 l))
               (goto-char (nth 0 l))
               (delete-region (nth 0 l) (nth 1 l))
               (insert (if (string-empty-p (nth 2 l)) " " (nth 2 l))
                       (if new (concat ":" (mapconcat #'identity new ":") ":") ""))))
           lines kept)))
      (dolist (l lines) (set-marker (nth 0 l) nil) (set-marker (nth 1 l) nil)))
    (set-marker limit nil)))

(defun nvemacs--org-filetags-place (limit)
  "Where a new `#+filetags:' line goes (see `nvemacs--org-set-filetags').
Search no further than LIMIT, the end of the header."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward "^#\\+weather:.*$" limit t) (forward-line 1) (point))
       ((progn (goto-char (point-min)) (re-search-forward "^#\\+notes:" limit t))
        (line-beginning-position))
       (t
        ;; After any leading blank lines: above them, the new line would
        ;; become a header of its own and cut off the real one.
        (goto-char (point-min))
        (skip-chars-forward " \t\r\n")
        (beginning-of-line)
        (when (looking-at ".*-\\*-.*-\\*-") (forward-line 1))
        (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
          (goto-char (or (nvemacs--org-drawer-end) (point))))
        (while (and (< (point) limit) (looking-at "[ \t]*#"))
          (forward-line 1))
        (point))))))

(defun nvemacs--yaml-item (tag)
  "Return TAG as a YAML scalar.
It is quoted when its first character is YAML syntax (`#' would start
a comment)."
  (if (string-match-p "\\`[@#%`!&*|>'\"]" tag) (format "\"%s\"" tag) tag))

(defun nvemacs--md-set-tags (tags)
  "Set the front-matter tags of the Markdown text in this buffer to TAGS.
The form found is kept (flow, block, comma or space string, TOML, JSON);
a missing key is added in that format.  No front matter is created."
  (let* ((fm (nvemacs--md-front-matter))
         (e (and fm (nvemacs--md-tags-entry fm)))
         (flow (concat "[" (mapconcat #'nvemacs--yaml-item tags ", ") "]"))
         (toml (concat "[" (mapconcat (lambda (tg) (format "\"%s\"" tg)) tags ", ") "]")))
    (save-excursion
      (cond
       ((null fm)
        (when tags (goto-char (point-min)) (insert "---\ntags: " flow "\n---\n")))
       ((and (null e) (eq (car fm) 'json))
        (when tags
          (let ((empty (string-blank-p (buffer-substring (nth 3 fm) (nth 4 fm)))))
            (goto-char (nth 3 fm))
            (insert "  \"tags\": " toml (if empty "\n" ",\n")))))
       ((null e)
        (when tags
          (goto-char (nth 4 fm))
          (insert (if (eq (car fm) 'toml) (concat "tags = " toml) (concat "tags: " flow)) "\n")))
       (t
        (delete-region (plist-get e :beg) (plist-get e :end))
        (goto-char (plist-get e :beg))
        (insert
         (pcase (plist-get e :form)
           ('toml (concat "tags = " toml))
           ('json (concat "\"tags\": " toml))
           ((guard (null tags)) "tags: []")
           ('flow (concat "tags: " flow))
           ('comma (concat "tags: " (mapconcat #'nvemacs--yaml-item tags ", ")))
           ('space (concat "tags: " (mapconcat #'nvemacs--yaml-item tags " ")))
           ('block (concat "tags:" (mapconcat (lambda (tg) (concat "\n" (plist-get e :indent) "- "
                                                                    (nvemacs--yaml-item tg)))
                                              tags ""))))))))))

(defun nvemacs--sister-write (path tags)
  "Write TAGS to PATH's sister file, or remove that file if TAGS is nil.
The file is headed by a comment naming the note and is written through
a temporary file and a rename.  It gets the note's permissions."
  (let ((f (nvemacs--sister path)))
    (if (null tags)
        (when (file-exists-p f) (delete-file f))
      (with-temp-buffer
        (insert (format "# nvEmacs metadata: the tags of \"%s\", the file with this name minus \".tags\".\n"
                        (file-name-nondirectory path))
                "# One tag per line. Lines starting with \"# \" are comments.\n")
        (dolist (tg tags) (insert tg "\n"))
        (nvemacs--write-file f (file-modes path))))))

(defun nvemacs--tags-in-buffer (path)
  "Return the clean tags of the current buffer, which holds note PATH."
  (nvemacs--tags-clean
   (if (equal (downcase (file-name-extension path)) "org")
       (nvemacs--org-tags-in-buffer)
     (nvemacs--md-tags-in-buffer))))

(defun nvemacs--tags-now (path)
  "Tags of note PATH as they stand, with edits not yet written."
  (let ((buf (find-buffer-visiting path)))
    (cond
     ((and buf (nvemacs--tags-in-file-p path))
      (with-current-buffer buf (save-restriction (widen) (nvemacs--tags-in-buffer path))))
     ((and buf (buffer-local-value 'nvemacs--sister-held buf))
      (mapcar (lambda (tg) (cons tg 'file))
              (car (buffer-local-value 'nvemacs--sister-held buf))))
     (t (nvemacs--tags-read path)))))

(defun nvemacs--set-text (new)
  "Make the buffer's text NEW, changing only the part that differs.
Point and the window's scroll stay put outside that part."
  (save-restriction
    (widen)
    (let* ((old (buffer-substring-no-properties (point-min) (point-max)))
           (n (min (length old) (length new)))
           (c (compare-strings old 0 n new 0 n))
           (pre (if (eq c t) n (1- (abs c))))
           (suf 0))
      (while (and (< suf (- n pre))
                  (eq (aref old (- (length old) suf 1)) (aref new (- (length new) suf 1))))
        (setq suf (1+ suf)))
      (save-excursion
        (goto-char (+ (point-min) pre))
        (delete-char (- (length old) pre suf))
        (insert (substring new pre (- (length new) suf)))))))

(defun nvemacs--tags-settle (path setter)
  "Put back this note's exact text if only its tag lines differ from it.
Removing the last tag leaves an empty `#+filetags:' or `tags: []', so
adding a tag and deleting it would count as a change.  Two texts are
the same apart from their tag lines if they have the same tags, and
SETTER, the writer for PATH's format, makes them equal."
  (when nvemacs--base
    (let ((norm (lambda (text)
                  (with-temp-buffer
                    (insert text)
                    (let ((tags (nvemacs--tags-in-buffer path)))
                      (funcall setter '("nvemacs-settle"))
                      (cons tags (buffer-string)))))))
      (when (equal (funcall norm (nvemacs--buffer-text))
                   (funcall norm (car nvemacs--base)))
        (nvemacs--set-text (car nvemacs--base))))))

(defun nvemacs--tags-stage (path tags)
  "Make TAGS, a list of normalised strings, the file tags of note PATH.
This is an edit like typing, inline or in a sister file: it is written
when the cursor leaves the note, or, made from the list, leaves its
row.  In a note you already had open it goes into your buffer for you
to save; a sister file is not in that buffer, so it is written now."
  (let* ((buf (nvemacs--visit path t))
         (from-list (eq (nvemacs--cursor-buffer) (get-buffer nvemacs--buffer-name)))
         (hold (and (buffer-local-value 'nvemacs--attached buf) nvemacs-auto-save-interval)))
    (with-current-buffer buf
      (if (nvemacs--tags-in-file-p path)
          (let ((setter (if (equal (downcase (file-name-extension path)) "org")
                            #'nvemacs--org-set-filetags #'nvemacs--md-set-tags)))
            (unless (verify-visited-file-modtime buf)
              (nvemacs--command-error
               'nvemacs-tags-edit
               (format "requires that %s match its file, and the file changed on disk"
                       (file-name-nondirectory path))
               "Run M-x revert-buffer in the note and then re-run the command"))
            (unless (equal tags (mapcar #'car (cl-remove-if-not
                                               (lambda (tk) (eq (cdr tk) 'file))
                                               (nvemacs--tags-now path))))
              (save-restriction (widen) (funcall setter tags))
              (when hold (nvemacs--tags-settle path setter))))
        (if hold
            (setq nvemacs--sister-held (list tags))
          (nvemacs--sister-write path tags)
          (when nvemacs--db (nvemacs--reindex path))))
      (when hold
        (setq nvemacs--edited t nvemacs--tags-stay from-list)
        (cl-pushnew buf nvemacs--waiting)
        (nvemacs--pending-write)))))

(defun nvemacs--tags-target ()
  "Return the note a tag command acts on.
That is the selected note in *nvEmacs*, or the note visited in the
current buffer."
  (cond
   ((derived-mode-p 'nvemacs-mode)
    (let ((id (nvemacs--selection)))
      (and id (caar (sqlite-select (nvemacs--db) "SELECT path FROM meta WHERE id = ?" (list id))))))
   ((and buffer-file-name (nvemacs--watched-note-p buffer-file-name))
    (nvemacs--canonical-path buffer-file-name))))

(defun nvemacs--all-tags ()
  "Every tag in the index once (case-insensitively), most used first."
  (mapcar #'car (sqlite-select (nvemacs--db)
                               "SELECT min(tag) FROM tags GROUP BY folded ORDER BY count(*) DESC, 1")))

(defun nvemacs-tags-edit (&optional path)
  "Edit the file tags of the selected note, or of PATH.
The minibuffer starts with the current tags; separate tags with commas;
completion offers every tag.  Spaces and punctuation become `_'.  This
is an edit of the note: it is written, and reaches the index, when the
cursor leaves the note or its row."
  (interactive)
  (let* ((path (or path (nvemacs--selected-note 'nvemacs-tags-edit)))
         (current (mapcar #'car (cl-remove-if-not (lambda (tk) (eq (cdr tk) 'file))
                                                  (nvemacs--tags-now path))))
         (answer (completing-read-multiple
                  (format "Tags of %s (comma-separated): " (file-name-nondirectory path))
                  (nvemacs--all-tags) nil nil
                  (and current (mapconcat #'identity current ", "))))
         (tags (mapcar #'car (nvemacs--tags-clean (mapcar (lambda (a) (cons a 'file)) answer)))))
    (nvemacs--tags-stage path tags)
    (message "nvEmacs: %s tags: %s" (file-name-nondirectory path)
             (if tags (mapconcat #'identity tags " ") "none"))
    tags))

(defun nvemacs-tags-list ()
  "List every tag with how many notes carry it (as a file or section tag)."
  (interactive)
  (let ((rows (sqlite-select
               (nvemacs--db)
               "SELECT min(tag), count(DISTINCT id), sum(kind = 'file'), sum(kind = 'section') FROM tags GROUP BY folded ORDER BY 2 DESC, 1")))
    (with-current-buffer (get-buffer-create "*nvEmacs tags*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-24s %5s  %s\n" "Tag" "Notes" "as file / section tag"))
        (dolist (r rows)
          (insert (format "%-24s %5d  %d / %d\n" (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r))))
        (insert (format "\n%d tag(s). Search with tag:NAME or -tag:NAME.\n" (length rows))))
      (special-mode)
      (setq-local revert-buffer-function (lambda (&rest _) (nvemacs-tags-list)))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Search history
;;
;; Each search is recorded once, when it ends.  A search starts at the first
;; edit after the last one ended.  Adding text extends it.  Deleting within
;; a second of the last edit fixes a typo; deleting after a pause ends it
;; and starts a new one.  It also ends when a note is opened or edited, the
;; search line is cleared, or nvEmacs or Emacs exits.  The design is in the
;; project's search_history notes.

(defcustom nvemacs-searchbar-history t
  "Non-nil means searches are recorded in `nvemacs-searchbar-history-file'.
On by default while the feature is tested.
\\[nvemacs-searchbar-history] turns it on or off and keeps the
choice in settings.eld, where it wins over this value.  While it is
off, the file is deleted."
  :type 'boolean
  :package-version '(nvemacs . "0.0.5"))

(defcustom nvemacs-searchbar-history-size 100
  "How many distinct searches the history keeps.  The oldest go first."
  :type 'integer
  :package-version '(nvemacs . "0.0.5"))

(defcustom nvemacs-searchbar-history-file nil
  "File of the search history.
nil means history.jsonl in `nvemacs-data-directory'."
  :type '(choice (const nil) file)
  :package-version '(nvemacs . "0.0.5"))

(defface nvemacs-history-gutter
  '((((background dark)) :background "#2f5d7c")
    (t :background "#a9cce3"))
  "The bar down the left of the history view, so it is not taken for the notelist.")

(defconst nvemacs--history-pause 1.0
  "Seconds after the last edit after which deleting starts a new search.
Sooner, a deletion fixes a typo.")

(defvar nvemacs--history-pending nil
  "The search in progress: (QUERY START LAST-EDIT), times in seconds.")

(defvar nvemacs--history-recall nil
  "While stepping through past searches: (INDEX SAVED ENTRIES TYPED).
INDEX is -1 on the text typed before; SAVED the search then in progress;
ENTRIES the history, newest first, read once so it does not reorder.")

(defun nvemacs--history-now ()
  "The time now, in seconds."
  (float-time))

(defun nvemacs--history-file ()
  "Return the path of the history file."
  (or nvemacs-searchbar-history-file (expand-file-name "history.jsonl" nvemacs-data-directory)))

(defun nvemacs--history-on-p ()
  "Non-nil if searches are recorded."
  nvemacs-searchbar-history)

(defun nvemacs--history-size (&optional quiet)
  "Return the history's limit.
An unusable value gives 100, and says so unless QUIET."
  (let ((n nvemacs-searchbar-history-size))
    (if (and (integerp n) (> n 0))
        n
      (unless quiet
        (nvemacs--command-message
         'nvemacs-searchbar-history
         (format "requires nvemacs-searchbar-history-size to be 1 or more, and it is %S" n)
         "Set it to a whole number of searches; 100 is used until then"))
      100)))

(defun nvemacs--history-key (query)
  "QUERY as compared for repeats: trimmed, spaces collapsed, folded."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " (nvemacs--fold query))))

(defun nvemacs--history-read ()
  "Return the history's entries, oldest first, each an alist.
The first line describes the file; a line that is not an entry is passed over."
  (let ((f (nvemacs--history-file)) (out nil))
    (when (file-readable-p f)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents f))
        (dolist (line (split-string (buffer-string) "\n" t))
          (let ((e (ignore-errors (json-parse-string line :object-type 'alist
                                                     :false-object :false :null-object nil))))
            (when (and (consp e) (stringp (alist-get 'query e)))
              (push e out))))))
    (nreverse out)))

(defvar nvemacs-version)

(defun nvemacs--history-write (entries)
  "Write ENTRIES, oldest first, as the history file: readable by its owner only."
  (let ((f (nvemacs--history-file)))
    (nvemacs--private-directory (file-name-directory f))
    (with-temp-buffer
      (insert (json-serialize `((about . "nvEmacs search history")
                                (version . ,(or nvemacs-version "unknown"))
                                (format . 1)))
              "\n")
      (dolist (e entries) (insert (json-serialize e) "\n"))
      (nvemacs--write-file f))))

(defun nvemacs--history-complete-p ()
  "Non-nil if the list shows every match of its search."
  (or nvemacs--list-complete
      (and nvemacs--sort-keys (null nvemacs--rest) (not nvemacs--list-stale))))

(defun nvemacs--history-record (pending how)
  "Write search PENDING, ended HOW, to the history.
A search rejected for its groups is not written: it never ran.
A search already there moves to the end, its count one higher.  A write
that fails says so; the search itself is not affected."
  (when (and pending (nvemacs--history-on-p)
             ;; It never ran.
             (not (plist-get (nvemacs--query-check (car pending)) :rejected)))
    (condition-case err
        (let* ((query (car pending))
               (key (nvemacs--history-key query))
               (entries (nvemacs--history-read))
               (old (cl-find-if (lambda (e) (equal (nvemacs--history-key (alist-get 'query e)) key))
                                entries))
               (when (format-time-string "%FT%T%:z" (nth 1 pending)))
               (buf (get-buffer nvemacs--buffer-name))
               (shown (if buf (with-current-buffer buf (length nvemacs--shown)) 0))
               (all (and buf (with-current-buffer buf (nvemacs--history-complete-p))))
               (entry `((query . ,query) (when . ,when)
                        (first . ,(or (alist-get 'first old) when))
                        (count . ,(1+ (or (alist-get 'count old) 0)))
                        (ended . ,(symbol-name how))
                        (shown . ,shown) (all . ,(if all t :false))
                        (notes . ,(or (ignore-errors (nvemacs--note-count)) 0))))
               (size (nvemacs--history-size)))
          (setq entries (append (remq old entries) (list entry)))
          (when (> (length entries) size) (setq entries (last entries size)))
          (nvemacs--history-write entries))
      (error
       (nvemacs--command-message
        "The search history"
        (format "requires a file it can write, and %s could not be written: %s"
                (nvemacs--short-path (nvemacs--history-file)) (nvemacs--error-reason err))
        "Check that folder, or set nvemacs-searchbar-history-file")))))

(defun nvemacs--history-end (how)
  "End the search in progress, recorded as ended HOW.
HOW is `opened', `new-search', `cleared' or `quit'.  A search left for
one recalled from the history is recorded as replaced by a new search."
  (let ((r nvemacs--history-recall) (p nvemacs--history-pending))
    (setq nvemacs--history-recall nil nvemacs--history-pending nil)
    (when (and r (>= (car r) 0)) (nvemacs--history-record (nth 1 r) 'new-search))
    (nvemacs--history-record p how)))

(defun nvemacs--history-begin (query)
  "Make QUERY the search in progress, starting now."
  (let ((now (nvemacs--history-now)))
    (setq nvemacs--history-pending (list query now now))))

(defun nvemacs--history-edit (deleting)
  "The search line was edited; DELETING non-nil if text was removed.
Extend the search in progress, end it, or start one."
  (when (nvemacs--history-on-p)
    (let ((r nvemacs--history-recall))
      ;; Editing a recalled search keeps it: the one left for it ends now.
      (when r
        (setq nvemacs--history-recall nil)
        (when (>= (car r) 0) (nvemacs--history-record (nth 1 r) 'new-search))))
    (let ((q (string-trim (nvemacs--search-text)))
          (now (nvemacs--history-now))
          (p nvemacs--history-pending))
      (cond
       ((string-empty-p q) (when p (nvemacs--history-end 'cleared)))
       ((null p) (nvemacs--history-begin q))
       ((and deleting (>= (- now (nth 2 p)) nvemacs--history-pause))
        (nvemacs--history-end 'new-search)
        (nvemacs--history-begin q))
       (t (setcar p q) (setcar (cddr p) now))))))

(defun nvemacs--history-delete-file ()
  "Delete the history file, if there is one."
  (let ((f (nvemacs--history-file)))
    (when (file-exists-p f) (delete-file f))))

(defun nvemacs--history-start-session ()
  "Apply the kept choice; with history off, delete any file left."
  (pcase (nvemacs--setting 'search-history)
    ('on (setq nvemacs-searchbar-history t))
    ('off (setq nvemacs-searchbar-history nil)))
  (unless (nvemacs--history-on-p)
    (ignore-errors (nvemacs--history-delete-file))))

(defun nvemacs-searchbar-history ()
  "Turn search history on or off, and say which it now is.
Turning it off asks first, then deletes the history file.  The choice is
kept in settings.eld and survives a restart."
  (interactive)
  (pcase (nvemacs--setting 'search-history)
    ('on (setq nvemacs-searchbar-history t))
    ('off (setq nvemacs-searchbar-history nil)))
  (let ((save (lambda (v) (nvemacs--setting-put 'search-history v))))
    (if (not (nvemacs--history-on-p))
        (progn (setq nvemacs-searchbar-history t)
               (funcall save 'on)
               (message "nvEmacs: search history is on"))
      (let ((n (length (nvemacs--history-read))))
        (if (not (y-or-n-p (format "Turn search history off and delete its %d search%s? "
                                   n (if (= n 1) "" "es"))))
            (message "nvEmacs: search history stays on")
          (setq nvemacs-searchbar-history nil
                nvemacs--history-pending nil nvemacs--history-recall nil)
          (funcall save 'off)
          (nvemacs--history-delete-file)
          (dolist (b (list nvemacs--history-name nvemacs--history-entry-name))
            (when (get-buffer b) (kill-buffer b)))
          (message "nvEmacs: search history is off"))))))

;;; Recall with M-p and M-n

(defun nvemacs--history-recall-step (delta command)
  "Show the search DELTA steps older in the history, for COMMAND.
Stepping records nothing: the search shown is recorded when it ends."
  (unless (and (derived-mode-p 'nvemacs-mode) (nvemacs--on-search-line-p))
    (nvemacs--command-error command "works only on the search line"
                            "Move to the search line and then re-run the command"))
  (unless nvemacs--history-recall
    (setq nvemacs--history-recall
          (list -1 nvemacs--history-pending (reverse (nvemacs--history-read)) (nvemacs--query))))
  (let* ((r nvemacs--history-recall)
         (entries (nth 2 r))
         (i (+ (car r) delta)))
    (cond
     ((null entries) (message "nvEmacs: no searches in the history yet"))
     ((>= i (length entries)) (message "nvEmacs: no older search"))
     ((< i -1) (message "nvEmacs: no newer search"))
     (t
      (setcar r i)
      (if (< i 0)
          (progn (nvemacs--replace-query (nth 3 r))
                 (setq nvemacs--history-pending (nth 1 r)))
        (let ((q (alist-get 'query (nth i entries))))
          (nvemacs--replace-query q)
          (nvemacs--history-begin q)))
      (goto-char nvemacs--query-start)
      (end-of-line)
      (nvemacs--clear-pane)
      (nvemacs--schedule-search)))))

(defun nvemacs-searchbar-history-previous ()
  "Put the previous search from the history on the search line.
\\<nvemacs-mode-map>Only on the search line: elsewhere \\[nvemacs-searchbar-history-previous]
keeps its usual meaning."
  (interactive)
  (nvemacs--history-recall-step 1 'nvemacs-searchbar-history-previous))

(defun nvemacs-searchbar-history-next ()
  "Put the next search from the history on the search line.
After the newest, put back the text typed before stepping back."
  (interactive)
  (nvemacs--history-recall-step -1 'nvemacs-searchbar-history-next))

;;; The history view

(defvar-keymap nvemacs--history-row-map
  :doc "Keys on a row of the history view."
  "d" #'nvemacs--history-delete
  "q" #'nvemacs--history-leave)

(defvar-keymap nvemacs-searchbar-history-mode-map
  :doc "Keys of the history view."
  "RET" #'nvemacs--history-run
  "C-c C-c" #'nvemacs--history-leave
  "TAB" #'forward-line)

(defvar-local nvemacs--history-filter-start nil "Where the history's filter text starts.")
(defvar-local nvemacs--history-rows-start nil "Where the history's rows start.")

(define-derived-mode nvemacs-searchbar-history-mode fundamental-mode "nvEmacs-History"
  "Past searches, newest first.  Type to filter them; RET runs one.
\\{nvemacs-searchbar-history-mode-map}"
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t)
  ;; A colored bar down the left, so this is never taken for the notelist.
  (setq-local line-prefix (propertize "  " 'face 'nvemacs-history-gutter))
  (setq-local wrap-prefix line-prefix)
  (add-hook 'after-change-functions #'nvemacs--history-filtered nil t)
  (add-hook 'pre-command-hook #'nvemacs--history-pre-command nil t)
  (add-hook 'post-command-hook #'nvemacs--history-show-entry nil t))

(defun nvemacs--history-buffer ()
  "Return the history view, made if needed."
  (or (get-buffer nvemacs--history-name)
      (with-current-buffer (get-buffer-create nvemacs--history-name)
        (nvemacs-searchbar-history-mode)
        (let ((inhibit-read-only t))
          (insert (propertize "History: " 'read-only t 'front-sticky t 'rear-nonsticky t
                              'field 'prompt 'face 'minibuffer-prompt))
          (setq nvemacs--history-filter-start (point-marker))
          (insert (propertize "\n" 'read-only t))
          (setq nvemacs--history-rows-start (point-marker)))
        (current-buffer))))

(defun nvemacs--history-filter-text ()
  "The text typed on the history view's filter line."
  (buffer-substring-no-properties nvemacs--history-filter-start
                                  (save-excursion (goto-char nvemacs--history-filter-start)
                                                  (line-end-position))))

(defun nvemacs--history-matcher (text)
  "Return a predicate on a folded query, for filter TEXT.
Words, quoted phrases and -word work as in search, as plain text: a
prefix such as dir: is matched as written, and so are parentheses and OR."
  (let (want skip)
    (dolist (tk (nvemacs--query-tokens text t))
      (let ((tok (car tk)) (at (cdr tk)))
        (if (and (string-prefix-p "-" tok) (not (eql at 0)) (> (length tok) 1)
                 (or (eql at 1) (string-match-p "\\`-[[:alnum:]]" tok)))
            (push (substring tok 1) skip)
          (unless (equal tok "-") (push tok want)))))
    (lambda (q)
      (and (cl-every (lambda (w) (string-search w q)) want)
           (not (cl-some (lambda (w) (string-search w q)) skip))))))

(defun nvemacs--history-row (e)
  "Return the history view's line for entry E."
  (let ((when (or (alist-get 'when e) ""))
        (count (or (alist-get 'count e) 1)))
    (propertize
     (format "%-16s  %-4s  %-10s  %-5s  %s\n"
             (string-replace "T" " " (substring when 0 (min 16 (length when))))
             (if (> count 1) (format "×%d" count) "")
             (or (alist-get 'ended e) "")
             (format "%s%s" (or (alist-get 'shown e) 0) (if (eq (alist-get 'all e) t) "" "+"))
             (alist-get 'query e))
     'read-only t 'nvemacs-history e 'keymap nvemacs--history-row-map)))

(defun nvemacs--history-render ()
  "Draw the rows that match the filter, newest first."
  (let* ((entries (reverse (nvemacs--history-read)))
         (match (nvemacs--history-matcher (nvemacs--history-filter-text)))
         (inhibit-read-only t)
         (shown 0))
    (save-excursion
      (delete-region nvemacs--history-rows-start (point-max))
      (goto-char (point-max))
      (dolist (e entries)
        (when (funcall match (nvemacs--history-key (alist-get 'query e)))
          (setq shown (1+ shown))
          (insert (nvemacs--history-row e)))))
    (setq header-line-format
          (if (nvemacs--history-on-p)
              (format " nvEmacs history · %d/%d searches%s · q returns"
                      (length entries) (nvemacs--history-size t)
                      (if (= shown (length entries)) "" (format " · %d shown" shown)))
            " nvEmacs history · off: M-x nvemacs-searchbar-history turns it on · q returns"))))

(defun nvemacs--history-filtered (beg _end _len)
  "Draw the rows again when the filter line is edited at BEG."
  (when (and nvemacs--history-rows-start (< beg nvemacs--history-rows-start))
    (nvemacs--history-render)))

(defun nvemacs--history-pre-command ()
  "Make typing on a row edit the filter line, as in the notelist."
  (when (and (memq this-command nvemacs--typing-commands)
             (>= (point) nvemacs--history-rows-start))
    (goto-char nvemacs--history-filter-start)
    (end-of-line)))

(defun nvemacs--history-entry-at ()
  "The entry on the line at point, or nil."
  (get-text-property (line-beginning-position) 'nvemacs-history))

(defun nvemacs--history-show-entry ()
  "Show the entry at point in the note pane: no search is run."
  (let ((e (nvemacs--history-entry-at)) (nw (nvemacs--note-window)))
    (when (and e nw)
      (with-current-buffer (get-buffer-create nvemacs--history-entry-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Search:      %s\n\nLast run:    %s\nFirst run:   %s\nTimes run:   %s\nEnded:       %s\nRows shown:  %s%s\nNotes then:  %s\n"
                          (alist-get 'query e) (alist-get 'when e) (alist-get 'first e)
                          (alist-get 'count e) (alist-get 'ended e) (alist-get 'shown e)
                          (if (eq (alist-get 'all e) t) ", every match"
                            ", with more matches not yet listed")
                          (alist-get 'notes e))))
        (setq buffer-read-only t))
      (set-window-buffer nw nvemacs--history-entry-name))))

(defun nvemacs-searchbar-history-list ()
  "Show past searches in place of the notelist, newest first.
Type to filter them, RET runs one, d deletes one, q returns."
  (interactive)
  (let ((buf (nvemacs--history-buffer)) (lw (nvemacs--list-window)))
    (if lw (progn (set-window-buffer lw buf) (select-window lw)) (pop-to-buffer-same-window buf))
    (with-current-buffer buf
      (nvemacs--history-render)
      (goto-char nvemacs--history-filter-start)
      (end-of-line))))

(defun nvemacs--history-leave ()
  "Hide the history view and return to the notelist."
  (interactive)
  (if (get-buffer nvemacs--buffer-name)
      (progn (set-window-buffer (selected-window) nvemacs--buffer-name)
             (nvemacs--clear-pane)
             (nvemacs--focus-query))
    (quit-window)))

(defun nvemacs--history-run ()
  "Run the search on this row, or the newest shown from the filter line."
  (interactive)
  (let ((e (or (nvemacs--history-entry-at)
               (and (< (point) nvemacs--history-rows-start)
                    (get-text-property nvemacs--history-rows-start 'nvemacs-history)))))
    (when e
      (nvemacs--history-leave)
      (with-current-buffer nvemacs--buffer-name
        (let ((q (alist-get 'query e)))
          (nvemacs--history-end 'new-search)
          (setq nvemacs--selected-title nil nvemacs--typed nil nvemacs--selected-id nil)
          (nvemacs--view-free-set nil)
          (nvemacs--replace-query q)
          (nvemacs--history-begin q)
          (goto-char nvemacs--query-start)
          (end-of-line)
          (nvemacs--search))))))

(defun nvemacs--history-delete ()
  "Delete the search on this row from the history."
  (interactive)
  (let ((e (nvemacs--history-entry-at)))
    (when e
      (let ((key (nvemacs--history-key (alist-get 'query e)))
            (line (line-number-at-pos)))
        (nvemacs--history-write
         (cl-remove-if (lambda (x) (equal (nvemacs--history-key (alist-get 'query x)) key))
                       (nvemacs--history-read)))
        (nvemacs--history-render)
        (goto-char (point-min))
        (forward-line (1- line))
        (message "nvEmacs: deleted from the search history")))))

;;;; Malformed notes: the report file
;;
;; Title, Created, Date and file tags have a meaning, so nvEmacs can say
;; when one is misplaced or unreadable.  Any other key is arbitrary: a typo
;; such as `#+Crated:' is just a key of that name.  The report is built from
;; the index and never touches a note.

(defconst nvemacs--malformed-sections
  '(("title-outside" "Title outside the header"
     "A =#+Title:= below the header does not set the note's title."
     "Move it into the header, the block of lines before the first blank line.")
    ("created-outside" "Created outside the header"
     "A =#+Created:= below the header does not set the note's date."
     "Move it into the header, the block of lines before the first blank line.")
    ("date-outside" "Date outside the header"
     "A =#+Date:= below the header does not set the note's date."
     "Move it into the header, the block of lines before the first blank line.")
    ("filetags-outside" "File tags outside the header"
     "A =#+filetags:= below the header does not tag the note."
     "Move it into the header, the block of lines before the first blank line.")
    ("several-title" "More than one Title"
     "The header has several titles, and nvEmacs uses the first."
     "Keep one.")
    ("several-created" "More than one Created"
     "The header has several Created values, and nvEmacs uses the first it can read."
     "Keep one.")
    ("several-date" "More than one Date"
     "The header has several Date values, and nvEmacs uses the first it can read."
     "Keep one.")
    ("unreadable-date" "No readable date"
     "The note has a Created or Date, but none can be read as a date, so =created:= searches use a guess."
     "Write it as YYYY-MM-DD, optionally with a time: 2026-11-23-10:30:45.")
    ("missing-date" "Missing a date"
     "The note has a header but no Created or Date, so =created:= searches use a guess."
     "Add =#+Created: 2026-11-23-10:30:45= to the header, or =created:= to the front matter."))
  "The report's sections: (KIND HEADING WHAT-IS-WRONG HOW-TO-FIX).")

(defun nvemacs--malformed-file ()
  "Return the path of the report of malformed notes."
  (expand-file-name "malformed.org" nvemacs-data-directory))

(defun nvemacs--malformed-path-p (path)
  "Non-nil if PATH is the report itself, which is never indexed as a note."
  (and (equal (file-name-nondirectory path) "malformed.org")
       (equal (file-truename path) (file-truename (nvemacs--malformed-file)))))

(defun nvemacs--malformed-counts ()
  "Return (MALFORMED . MISSING), how many notes have each kind of problem."
  (let ((row (car (sqlite-select
                   (nvemacs--db)
                   "SELECT count(DISTINCT CASE WHEN kind <> 'missing-date' THEN id END), count(DISTINCT CASE WHEN kind = 'missing-date' THEN id END) FROM problems"))))
    (cons (car row) (cadr row))))

(defun nvemacs--malformed-summary ()
  "Return the report's counts for reading."
  (let ((c (nvemacs--malformed-counts)))
    (format "%s malformed, %s missing a date"
            (nvemacs--group-digits (car c)) (nvemacs--group-digits (cdr c)))))

(defun nvemacs--malformed-entry (path line detail)
  "Return the report's line for note PATH, at LINE, with DETAIL."
  (concat "- "
          (if (string-match-p "[][\n]" path)
              path
            (format "[[file:%s%s][%s]]" path (if line (format "::%d" line) "") path))
          (if line (format " (line %d)" line) "")
          (if detail (format ": %s" (string-replace "\n" " " detail)) "")
          "\n"))

(defun nvemacs--malformed-text ()
  "Return the text of the report, from the index."
  (let ((db (nvemacs--db)))
    (with-temp-buffer
      (insert "#+Title: nvEmacs malformed notes\n#+STARTUP: showall\n\n"
              "nvEmacs writes this file from its index.  It never edits your notes, and\n"
              "changes made here are overwritten.\n\n"
              (nvemacs--malformed-summary) ".\n")
      (let ((any nil))
        (pcase-dolist (`(,kind ,heading ,what ,fix) nvemacs--malformed-sections)
          (let ((rows (sqlite-select
                       db "SELECT m.path, p.line, p.detail FROM problems p JOIN meta m ON m.id = p.id WHERE p.kind = ? ORDER BY m.path, p.line"
                       (list kind))))
            (when rows
              (setq any t)
              (insert (format "\n* %s (%d)\n\n%s\nTo fix: %s\n\n" heading (length rows) what fix))
              (dolist (r rows) (insert (apply #'nvemacs--malformed-entry r))))))
        (unless any (insert "\nNothing to report.\n")))
      (buffer-string))))

(defun nvemacs--malformed-write ()
  "Write the report if its text changed; return the file."
  (let ((file (nvemacs--malformed-file)) (text (nvemacs--malformed-text)))
    (unless (and (file-readable-p file)
                 (equal text (with-temp-buffer
                               (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
                               (buffer-string))))
      (nvemacs--private-directory (file-name-directory file))
      (with-temp-buffer (insert text) (nvemacs--write-file file)))
    file))

(defun nvemacs--malformed-save ()
  "Write the report.  An error here never stops a rebuild, scan or exit."
  (when nvemacs--db
    (with-demoted-errors "nvEmacs: writing malformed.org: %S"
      (nvemacs--malformed-write))))

(defun nvemacs-index-malformed ()
  "Write and show malformed.org: notes whose header needs attention.
The report lists them by problem, with how to fix each.  nvEmacs never
edits those notes."
  (interactive)
  (let* ((file (nvemacs--malformed-write))
         (buf (find-buffer-visiting file)))
    (if buf
        (with-current-buffer buf (revert-buffer t t t))
      (setq buf (find-file-noselect file)))
    (with-current-buffer buf (setq buffer-read-only t))
    (pop-to-buffer buf)
    file))

;;;; Watched folders
;;
;; nvEmacs keeps the watched folders in settings.eld and asks for them at
;; its first start.  A list set in the init file wins instead, and the
;; commands here then say to change it there: two places setting it, each
;; winning some of the time, could not be explained.

(defconst nvemacs--large-folder 20000
  "Files in a folder being added at which nvEmacs warns that it is large.")

(defvar nvemacs--folders-from-init nil
  "Non-nil when the watched folders were set in the init file.")
(defvar nvemacs--folders-kept nil
  "The list nvEmacs set from settings.eld, to tell it from an init-file value.")

(defun nvemacs--folders-load ()
  "Use the watched folders kept in settings.eld, unless the init file set some."
  (if (and nvemacs-watched-directories
           (not (equal nvemacs-watched-directories nvemacs--folders-kept)))
      (setq nvemacs--folders-from-init t)
    (setq nvemacs--folders-from-init nil
          nvemacs-watched-directories (nvemacs--setting 'watched-folders)
          nvemacs--folders-kept nvemacs-watched-directories)))

(defun nvemacs--folders-check-own (command)
  "Refuse COMMAND when the watched folders were set in the init file."
  (when nvemacs--folders-from-init
    (nvemacs--command-error
     command
     (concat "requires watched folders that nvEmacs keeps, and nvemacs-watched-directories "
             "is set in your init file (by setq, use-package :custom or Customize)")
     "Change the folders there, or delete that setting and restart Emacs so nvEmacs can keep them")))

(defun nvemacs--folders-set (list)
  "Make LIST the watched folders and keep it in settings.eld."
  (setq nvemacs-watched-directories list nvemacs--folders-kept list)
  (nvemacs--setting-put 'watched-folders list))

(defun nvemacs--folders-applied ()
  "Bring the index, and a running nvEmacs, up to date with the folders."
  (when (or nvemacs--db (file-exists-p (nvemacs--index-path)))
    (nvemacs-index-reconcile))
  (when (get-buffer nvemacs--buffer-name)
    (nvemacs--note-files)
    (nvemacs--update-watches)
    (with-current-buffer nvemacs--buffer-name (nvemacs--search))))

(defun nvemacs--count-files (dir limit)
  "Count the files in DIR and its subfolders, stopping at LIMIT.
Only names are read: a name with an extension is taken as a file without
asking the disk, which costs about 8 µs a name (437,264 in 3.4 s,
measured 2026-11-24).  Hidden folders are skipped, as when indexing."
  (let ((todo (list dir)) (n 0))
    (catch 'full
      (while todo
        (let ((d (pop todo)))
          (dolist (name (ignore-errors (directory-files d nil nil t)))
            (cond ((member name '("." "..")))
                  ((string-match-p nvemacs-ignore-dir-regexp name))
                  ((string-match-p "\\.[^.]+\\'" name) (setq n (1+ n)))
                  ((file-directory-p (expand-file-name name d))
                   (when nvemacs-watched-directories-recursive
                     (push (expand-file-name name d) todo)))
                  (t (setq n (1+ n))))
            (when (>= n limit) (throw 'full n))))))
    n))

(defun nvemacs--folder-add-1 (dir command)
  "Add DIR to the watched folders for COMMAND, checking it with the user.
Return non-nil if it was added."
  (let* ((dir (file-name-as-directory (abbreviate-file-name (expand-file-name dir))))
         (true (lambda (d) (file-name-as-directory (file-truename (expand-file-name d)))))
         (said-no (lambda () (message "nvEmacs: %s not added" dir) nil)))
    (if (and (not (file-directory-p dir))
             (not (y-or-n-p (format "%s does not exist.  Create it (n cancels, in case of a typo)? " dir))))
        (funcall said-no)
      (unless (file-directory-p dir) (make-directory dir t))
      (let* ((td (funcall true dir))
             (covering (cl-find-if (lambda (w) (string-prefix-p (funcall true w) td))
                                   nvemacs-watched-directories))
             (inside (cl-remove-if-not (lambda (w) (string-prefix-p td (funcall true w)))
                                       nvemacs-watched-directories)))
        (cond
         (covering
          (nvemacs--command-error
           command
           (if (equal (funcall true covering) td)
               (format "requires a folder not watched yet, and %s is already watched" dir)
             (format "requires a folder not watched yet, and %s is inside the watched folder %s"
                     dir covering))
           "Choose another folder"))
         ((and inside
               (not (y-or-n-p (format "%s holds the watched folder%s %s.  Watch %s instead? "
                                      dir (if (cdr inside) "s" "") (string-join inside ", ") dir))))
          (funcall said-no))
         ((and (>= (nvemacs--count-files td nvemacs--large-folder) nvemacs--large-folder)
               (not (y-or-n-p (format "%s is a large folder (%s files or more), and indexing it may take a while.  Watch it anyway? "
                                      dir (nvemacs--group-digits nvemacs--large-folder)))))
          (funcall said-no))
         (t (nvemacs--folders-set
             (append (cl-remove-if (lambda (w) (member w inside)) nvemacs-watched-directories)
                     (list dir)))
            t))))))

(defun nvemacs--folders-ask-p ()
  "Non-nil when a start should ask for the watched folders.
Only with none set, in an interactive session, and with settings.eld
writable: read-only, the answer could not be kept, and the folders are
in the file that could not be read."
  (and (null nvemacs-watched-directories)
       (not noninteractive)
       (not nvemacs--settings-read-only)))

(defun nvemacs--folders-ask ()
  "Ask for the watched folders, as at a first start with none.
\\[keyboard-quit] stops asking; nvEmacs then starts with none, and its
home page says how to add them."
  (condition-case nil
      (let ((more t))
        (while more
          (condition-case err
              (nvemacs--folder-add-1 (read-directory-name "Folder of your notes: " nil nil nil) 'nvemacs)
            (user-error (message "%s" (error-message-string err)) (sit-for 2)))
          (setq more (y-or-n-p "Add another folder? "))))
    (quit nil)))

(defun nvemacs-index-folder-add (dir)
  "Watch folder DIR and its subfolders, and index its notes.
The list is kept in settings.eld.  A folder that does not exist can be
created; one holding 20,000 files or more asks first."
  (interactive
   (progn (nvemacs--folders-load)
          (nvemacs--folders-check-own 'nvemacs-index-folder-add)
          (list (read-directory-name "Watch folder: "))))
  (nvemacs--folders-load)
  (nvemacs--folders-check-own 'nvemacs-index-folder-add)
  (when (nvemacs--folder-add-1 dir 'nvemacs-index-folder-add)
    (nvemacs--folders-applied)
    (message "nvEmacs: now watching %s" (file-name-as-directory (abbreviate-file-name (expand-file-name dir))))))

(defun nvemacs-index-folder-remove (dir)
  "Stop watching folder DIR.  Its notes leave the index; its files stay.
Their Added dates are kept, so watching the folder again restores them."
  (interactive
   (progn (nvemacs--folders-load)
          (nvemacs--folders-check-own 'nvemacs-index-folder-remove)
          (list (completing-read "Stop watching folder: " nvemacs-watched-directories nil t))))
  (nvemacs--folders-load)
  (nvemacs--folders-check-own 'nvemacs-index-folder-remove)
  (unless (member dir nvemacs-watched-directories)
    (nvemacs--command-error 'nvemacs-index-folder-remove
                            (format "requires a watched folder, and %s is not one" dir)
                            "Re-run the command and choose from the list"))
  (nvemacs--folders-set (remove dir nvemacs-watched-directories))
  (nvemacs--folders-applied)
  (message "nvEmacs: no longer watching %s; its files are untouched" dir))

;;;; Keys

(defun nvemacs-keys-list ()
  "List every key in the notes with how many notes use it, and where."
  (interactive)
  (let ((rows (sqlite-select
               (nvemacs--db)
               "SELECT min(key), count(DISTINCT id), sum(kind = 'header'), sum(kind = 'body') FROM fields WHERE fkey IS NOT NULL GROUP BY fkey ORDER BY 2 DESC, 1")))
    (with-current-buffer (get-buffer-create "*nvEmacs keys*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-24s %5s  %s\n" "Key" "Notes" "uses in header / body"))
        (dolist (r rows)
          (insert (format "%-24s %5d  %d / %d\n" (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r))))
        (insert (format "\n%d key(s). Search with key:NAME or -key:NAME, and values with meta:WORD.\n"
                        (length rows))))
      (special-mode)
      (setq-local revert-buffer-function (lambda (&rest _) (nvemacs-keys-list)))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Reports: M-x nvemacs-report-all and nvemacs-report-*
;;
;; Each section is a function returning rows (LABEL . VALUE); VALUE is a
;; number, a string, or a list of strings shown as indented lines. The
;; report buffer is a `special-mode' buffer; g recomputes it.

(defun nvemacs--report-query (sql)
  "Return the first value of SQL on the open index, or nil.
A report never opens or creates the index itself."
  (and nvemacs--db (sqlitep nvemacs--db)
       (ignore-errors (caar (sqlite-select nvemacs--db sql)))))

(defun nvemacs--report-memory ()
  "Return the rows of the Memory report.
Each row is (LABEL . VALUE), as `nvemacs--report-render' shows them."
  (let* ((buf (get-buffer nvemacs--buffer-name))
         (owned (cl-remove-if-not (lambda (b) (buffer-local-value 'nvemacs--owned b))
                                  (buffer-list)))
         (heap (let ((sum 0))
                 (dolist (e (garbage-collect) sum)
                   (when (and (consp e) (numberp (nth 1 e)) (numberp (nth 2 e)))
                     (cl-incf sum (* (nth 1 e) (nth 2 e)))))))
         (cache (nvemacs--report-query "PRAGMA cache_size"))
         (page (nvemacs--report-query "PRAGMA page_size")))
    `(("Emacs process RSS (MB)" . ,(nvemacs--rss-mb))
      ;; `garbage-collect' counts only objects made after start: the
      ;; preloaded (dumped) Lisp is not included. macOS virtual size is
      ;; mostly reserved address space and says nothing, so it is omitted.
      ("Lisp objects made since start (MB)" . ,(/ (round (/ heap 104857.6)) 10.0))
      ("Garbage collections / time (s)" . ,(format "%d / %.2f" gcs-done gc-elapsed))
      ("SQLite page cache limit (KiB)"
       . ,(cond ((null cache) "index not open")
                ((< cache 0) (- cache))
                (t (/ (* cache page) 1024))))
      ("SQLite page size (bytes)" . ,(or page "index not open"))
      ("SQLite memory-mapped I/O (bytes)" . ,(or (nvemacs--report-query "PRAGMA mmap_size") "index not open"))
      ("Index file (bytes)" . ,(or (file-attribute-size (file-attributes (nvemacs--index-path))) "none"))
      ("Index pages" . ,(or (nvemacs--report-query "PRAGMA page_count") "index not open"))
      ("Rows listed in *nvEmacs*" . ,(if buf (length (buffer-local-value 'nvemacs--shown buf)) "not started"))
      ("*nvEmacs* buffer (bytes)" . ,(if buf (with-current-buffer buf (1- (position-bytes (point-max)))) "not started"))
      ("Most rows listed this session" . ,(or (plist-get nvemacs--session :max-rows) "no session"))
      ("Note buffers nvEmacs opened" . ,(length owned))
      ("  their text (bytes)" . ,(apply #'+ (mapcar #'buffer-size owned)))
      ("Not shown" . ("SQLite's own memory use cannot be read from Elisp: the cache LIMIT is shown."
                      "Index pages also sit in the macOS file cache, which belongs to the system.")))))

(defun nvemacs--orphan-tag-files ()
  "Return the `.tags' sister files, in the listed folders, with no note."
  (cl-loop for d in nvemacs--listed-dirs
           when (file-directory-p d)    ; it may be gone since the last walk
           nconc (cl-remove-if (lambda (f) (file-exists-p (substring f 0 -5)))
                               (directory-files d t "\\.tags\\'"))))

(defun nvemacs--report-index ()
  "Return the rows of the Index report.
Each row is (LABEL . VALUE), as `nvemacs--report-render' shows them."
  (let ((tables (and nvemacs--db
                     (ignore-errors
                       (sqlite-select nvemacs--db
                                      "SELECT name, sum(pgsize) FROM dbstat GROUP BY name ORDER BY 2 DESC")))))
    `(("Data directory" . ,nvemacs-data-directory)
      ("Index file" . ,(nvemacs--index-path))
      ("Index version (stored / current)"
       . ,(format "%s / %s" (or (nvemacs--report-query "SELECT value FROM info WHERE key = 'version'") "?")
                  nvemacs--index-version))
      ("Generation" . ,(or (nvemacs--report-query "SELECT value FROM info WHERE key = 'generation'") "?"))
      ("Notes" . ,(or (nvemacs--report-query "SELECT count(*) FROM meta") "index not open"))
      ("  with Hangul/Hanja (other tables)" . ,(or (nvemacs--report-query "SELECT count(*) FROM meta WHERE other") "?"))
      ("Watched directories" . ,(or nvemacs-watched-directories '("none")))
      ("Note extensions" . ,(mapconcat #'identity nvemacs-import-extensions " "))
      ("Not indexed (M-x nvemacs-index-skipped)" . ,(length nvemacs--skipped))
      ("Tag files without a note"
       . ,(or (nvemacs--orphan-tag-files) '("none")))
      ("Table sizes (bytes)"
       . ,(if tables
              (mapcar (lambda (r) (format "%-28s %s" (car r) (nvemacs--group-digits (cadr r)))) tables)
            '("not available (index not open, or SQLite built without dbstat)"))))))

(defun nvemacs--report-sync ()
  "Return the rows of the Sync report.
Each row is (LABEL . VALUE), as `nvemacs--report-render' shows them."
  (let ((r nvemacs--last-sync))
    `(("Watches (directories)" . ,(length nvemacs--watches))
      ("Directories listed" . ,(length nvemacs--listed-dirs))
      ("Queue (items waiting)" . ,(length nvemacs--sync-queue))
      ("Run in progress" . ,(if nvemacs--sync-run "yes" "no"))
      ("Sync on focus" . ,(if nvemacs-sync-on-focus "yes" "no"))
      ("Auto-save interval (s)" . ,(or nvemacs-auto-save-interval "off"))
      ("Last run"
       . ,(if r
              (list (format "%s, trigger %s" (plist-get r :when) (plist-get r :trigger))
                    (format "plan %.1f ms, %d item(s): %d new, %d changed, %d deleted, %d skipped"
                            (plist-get r :plan-ms) (plist-get r :items) (plist-get r 'new)
                            (plist-get r 'changed) (plist-get r 'deleted) (plist-get r 'skipped))
                    (format "%d slice(s), work %.1f ms, wall %.1f ms"
                            (plist-get r :slices) (plist-get r :work-ms) (plist-get r :wall-ms)))
            '("none yet"))))))

(defun nvemacs--report-search ()
  "Return the rows of the Search report.
Each row is (LABEL . VALUE), as `nvemacs--report-render' shows them."
  (let ((s nvemacs--last-search))
    `(("Searches this session" . ,(or (plist-get nvemacs--session :searches) "no session"))
      ("Last search"
       . ,(if s
              (list (format "%s: %S (%d term(s)), table %s"
                            (plist-get s :when) (plist-get s :query) (plist-get s :terms) (plist-get s :route))
                    (format "%d row(s) of %d wanted, %s" (plist-get s :rows) (plist-get s :want)
                            (plist-get s :outcome))
                    (format "first row %s ms, done %.1f ms"
                            (or (plist-get s :first-row-ms) "-") (plist-get s :ms)))
            '("none yet")))
      ("Search delay (s)" . ,nvemacs-search-delay)
      ("Grow delay (s)" . ,(or nvemacs-grow-delay "off")))))

(defun nvemacs--report-log ()
  "Return the rows of the Instrumentation log report.
Each row is (LABEL . VALUE), as `nvemacs--report-render' shows them."
  (let ((dir (nvemacs--log-directory)))
    `(("Instrumentation" . ,(if nvemacs-instrumentation "on" "off (M-x nvemacs-instrumentation)"))
      ("Log directory" . ,dir)
      ("Cap per file (bytes)" . ,nvemacs-log-max-bytes)
      ,@(mapcar
         (lambda (kind)
           (let* ((f (expand-file-name (concat kind ".jsonl") dir))
                  (lines (and (file-exists-p f)
                              (with-temp-buffer
                                (insert-file-contents f)
                                (split-string (buffer-string) "\n" t)))))
             (cons (format "%s.jsonl" kind)
                   (if lines
                       (cons (format "%d record(s), %s bytes; newest:" (length lines)
                                     (nvemacs--group-digits
                                      (file-attribute-size (file-attributes f))))
                             (last lines 3))
                     '("none")))))
         '("startup" "session" "sync")))))

(defconst nvemacs--report-sections
  '(("Memory" . nvemacs--report-memory)
    ("Index" . nvemacs--report-index)
    ("Sync" . nvemacs--report-sync)
    ("Search" . nvemacs--report-search)
    ("Instrumentation log" . nvemacs--report-log)))

(defun nvemacs--report-render (titles)
  "Fill *nvEmacs report* with the sections named TITLES and show it."
  (with-current-buffer (get-buffer-create "*nvEmacs report*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (title titles)
        (let* ((rows (funcall (cdr (assoc title nvemacs--report-sections))))
               (w (apply #'max 10 (mapcar (lambda (r) (string-width (car r))) rows))))
          (insert title "\n" (make-string (string-width title) ?-) "\n")
          (dolist (r rows)
            (let ((v (cdr r)))
              (if (and (listp v) v)
                  (progn (insert (car r) ":\n")
                         (dolist (l v) (insert "    " l "\n")))
                (insert (string-pad (car r) w) "  "
                        (cond ((integerp v) (nvemacs--group-digits v))
                              ((null v) "-")
                              (t (format "%s" v)))
                        "\n"))))
          (insert "\n"))))
    (special-mode)
    (setq-local revert-buffer-function (lambda (&rest _) (nvemacs--report-render titles)))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun nvemacs-report-all ()
  "Show every nvEmacs report: memory, index, sync, search, instrumentation."
  (interactive)
  (nvemacs--report-render (mapcar #'car nvemacs--report-sections)))

(defun nvemacs-report-memory ()
  "Show what nvEmacs holds in memory.
That is the process size, the Lisp heap, the SQLite page cache limit,
the rows listed and the note buffers opened."
  (interactive)
  (nvemacs--report-render '("Memory")))

(defun nvemacs-report-index ()
  "Show the index: file, version, note count, table sizes, skipped notes."
  (interactive)
  (nvemacs--report-render '("Index")))

(defun nvemacs-report-sync ()
  "Show sync state: watches, queue, and the last reconcile's numbers."
  (interactive)
  (nvemacs--report-render '("Sync")))

(defun nvemacs-report-search ()
  "Show the last search's numbers: rows, first-row and total time."
  (interactive)
  (nvemacs--report-render '("Search")))

(defun nvemacs-report-log ()
  "Show the instrumentation log files and their newest records."
  (interactive)
  (nvemacs--report-render '("Instrumentation log")))

;;;; Renaming, moving and deleting notes
;;
;; A note is more than its file: its sister tags file, open buffer, waiting
;; edits, created date and index entry go with it.  None of these commands
;; changes a note's text or its date.

(defun nvemacs--selected-note (command)
  "Return the path of the selected note, or signal an error from COMMAND."
  (or (nvemacs--tags-target)
      (nvemacs--command-error command "requires that a note be selected"
                              "Move to its row and then re-run the command")))

(defun nvemacs--note-target (command)
  "Return the path of the note COMMAND acts on, which must be on disk."
  (let ((path (nvemacs--selected-note command)))
    (unless (file-exists-p path)
      (nvemacs--command-error
       command
       (format "requires a note on disk, and %s has not been written yet"
               (file-name-nondirectory path))
       "Save it and then re-run the command"))
    path))

(defun nvemacs--check-destination (command old new)
  "Signal an error from COMMAND unless OLD can become NEW safely.
Nothing may be overwritten.
Names are compared without case: APFS treats them as the same file, so
a change of case alone is allowed.  A `.tags' file already at NEW would
silently become the note's tags."
  (let ((dir (file-name-directory new))
        (name (file-name-nondirectory new))
        (buf (find-buffer-visiting new)))
    (let ((clash (nvemacs--existing-file dir name)))
      (when (and clash (not (file-equal-p clash old)))
        (nvemacs--command-error
         command
         (format "requires a name not in use, and %s already exists"
                 (file-name-nondirectory clash))
         "Rename one of the two notes and then re-run the command")))
    (let ((clash (nvemacs--existing-file dir (concat name ".tags"))))
      (when (and clash (not (file-equal-p clash (nvemacs--sister old))))
        (nvemacs--command-error
         command
         (format "requires that no tags file has that name, and %s does"
                 (file-name-nondirectory clash))
         "Move or delete that tags file and then re-run the command")))
    (when (and buf (not (eq buf (find-buffer-visiting old))))
      (nvemacs--command-error
       command
       (format "requires that no other buffer visits %s, and %s does" name (buffer-name buf))
       "Close that buffer and then re-run the command"))))

(defun nvemacs--note-settle-edits (command path)
  "Write the edits waiting for note PATH, or signal an error from COMMAND.
A buffer the user had open before nvEmacs is theirs to save."
  (let ((b (find-buffer-visiting path)))
    (when b
      (when (buffer-local-value 'nvemacs--attached b) (nvemacs--commit b))
      (when (or (buffer-modified-p b) (buffer-local-value 'nvemacs--sister-held b))
        (nvemacs--command-error
         command
         (format "requires that the edits to %s be written first, and they are not"
                 (file-name-nondirectory path))
         "Save or revert the note and then re-run the command")))))

(defun nvemacs--note-files-move (old new)
  "Move note OLD and its sister file to NEW: both, or neither."
  (let ((sister (nvemacs--sister old)))
    (rename-file old new)
    (when (file-exists-p sister)
      (condition-case err
          (rename-file sister (nvemacs--sister new))
        (error (rename-file new old)
               (signal (car err) (cdr err)))))))

(defun nvemacs--note-relocate (command old new)
  "For COMMAND, move note OLD to NEW inside the watched folders.
Its edits are written first.  Its sister file, buffer, created date and
index entry follow.  The date is kept, so its row keeps its place."
  (nvemacs--note-settle-edits command old)
  (let ((buf (find-buffer-visiting old))
        (created (gethash (nvemacs--dates-key old) (nvemacs--dates-table))))
    (nvemacs--note-files-move old new)
    (when buf
      (with-current-buffer buf
        (set-visited-file-name new t t)
        (set-visited-file-modtime)
        ;; Visiting a new name turns Emacs's auto-save back on, and a new
        ;; extension can change the major mode, which ends minor modes.
        (when nvemacs--attached (nvemacs--emacs-auto-save-off))
        (unless nvemacs-note-mode (nvemacs-note-mode 1))))
    (when created (nvemacs--dates-seen new created))
    (nvemacs--reindex new old)))

(defun nvemacs--note-let-go (buf &optional new)
  "Note buffer BUF's file has left nvEmacs, for path NEW or the Trash.
Note panes stop showing it.  A buffer nvEmacs opened is closed; one the
user had open stays, visiting NEW if there is one."
  (when buf
    (dolist (w (get-buffer-window-list buf nil t))
      (when (window-parameter w 'nvemacs-note) (nvemacs--setup-note-window w nil)))
    (when (get-buffer nvemacs--buffer-name)
      (with-current-buffer nvemacs--buffer-name (setq nvemacs--previewed nil)))
    (if (buffer-local-value 'nvemacs--owned buf)
        (kill-buffer buf)
      (with-current-buffer buf
        (when new
          (set-visited-file-name new t t)
          (set-visited-file-modtime))
        (nvemacs-note-mode -1)))
    (when (window-parameter (selected-window) 'nvemacs-note) (nvemacs--focus-query))))

(defun nvemacs--rename-path (old name)
  "Return the path note OLD gets when renamed to NAME, or signal.
A NAME with no extension keeps the note's.  Any other extension must be
one of `nvemacs-import-extensions', or the note would drop out of search."
  (let ((name (string-trim name)))
    (when (or (string-empty-p name) (string-prefix-p "." name) (string-search "/" name))
      (nvemacs--command-error
       'nvemacs-note-rename
       "requires a name that is not empty, does not start with a dot and has no /"
       "Re-run the command with another name"))
    (unless (file-name-extension name)
      (setq name (concat name "." (file-name-extension old))))
    (unless (member (downcase (file-name-extension name)) nvemacs-import-extensions)
      (nvemacs--command-error
       'nvemacs-note-rename
       (format "requires a supported extension, and .%s is not one" (file-name-extension name))
       (format "Re-run the command with a name ending in %s"
               (mapconcat (lambda (e) (concat "." e)) nvemacs-import-extensions ", "))))
    (expand-file-name name (file-name-directory old))))

(defun nvemacs--tags-lost (old new)
  "Return the tags of note OLD that nvEmacs stops reading if it becomes NEW.
Org, Markdown and plain text keep tags in different places."
  (let ((place (lambda (p) (if (nvemacs--tags-in-file-p p)
                               (downcase (file-name-extension p))
                             'sister))))
    (unless (equal (funcall place old) (funcall place new))
      (delete-dups (mapcar #'car (nvemacs--tags-now old))))))

(defun nvemacs--extension-change-ok-p (old new)
  "Non-nil if OLD and NEW share an extension, or the user accepts the change."
  (let ((from (file-name-extension old)) (to (file-name-extension new)))
    (or (string-equal-ignore-case from to)
        (let ((lost (nvemacs--tags-lost old new)))
          (y-or-n-p (format "Change the extension from .%s to .%s%s? " from to
                            (if lost
                                (format " (tags no longer read: %s)" (string-join lost ", "))
                              "")))))))

(defun nvemacs-note-rename (name)
  "Rename the selected note's file to NAME, in the same folder.
Only the filename changes: the text, including any title in it, and the
date stay.  A NAME with no extension keeps the note's.  A new extension
must be a supported one, and is confirmed first.  The note's tags file
and open buffer follow it, and edits waiting for it are written first."
  (interactive
   (let ((file (file-name-nondirectory (nvemacs--note-target 'nvemacs-note-rename))))
     (list (read-string (format "Rename %s to: " file) file))))
  (let* ((old (nvemacs--note-target 'nvemacs-note-rename))
         (new (nvemacs--rename-path old name)))
    (if (string= old new)
        (message "nvEmacs: the name is unchanged")
      (nvemacs--check-destination 'nvemacs-note-rename old new)
      (if (nvemacs--extension-change-ok-p old new)
          (progn (nvemacs--note-relocate 'nvemacs-note-rename old new)
                 (message "nvEmacs: renamed to %s" (file-name-nondirectory new)))
        (message "nvEmacs: not renamed")))))

(defun nvemacs--move-folders (path)
  "Return the watched folders note PATH could move to, for reading."
  (sort (cl-loop for d in nvemacs--listed-dirs
                 unless (or (equal d (file-name-directory path)) (not (file-directory-p d)))
                 collect (abbreviate-file-name d))
        #'string<))

(defun nvemacs--folder-path (dir)
  "Return DIR resolved through symlinks, as a directory name."
  (file-name-as-directory (file-truename (expand-file-name dir))))

(defun nvemacs-note-move (dir)
  "Move the selected note to DIR, another watched folder.
Its text and date stay, and its tags file and open buffer follow it."
  (interactive
   (let* ((path (nvemacs--note-target 'nvemacs-note-move))
          (folders (or (nvemacs--move-folders path)
                       (nvemacs--command-error
                        'nvemacs-note-move "requires another watched folder, and there is none"
                        "Add one to nvemacs-watched-directories and then re-run the command"))))
     (list (completing-read (format "Move %s to folder: " (file-name-nondirectory path))
                            folders nil t))))
  (let* ((old (nvemacs--note-target 'nvemacs-note-move))
         (dest (nvemacs--folder-path dir))
         (new (expand-file-name (file-name-nondirectory old) dest)))
    (unless (member dest nvemacs--listed-dirs)
      (nvemacs--command-error
       'nvemacs-note-move (format "requires a watched folder, and %s is not one" dir)
       "Use M-x nvemacs-note-move-outside to move the note there"))
    (when (equal dest (file-name-directory old))
      (nvemacs--command-error
       'nvemacs-note-move (format "requires a different folder, and the note is already in %s" dir)
       "Re-run the command and choose another folder"))
    (nvemacs--check-destination 'nvemacs-note-move old new)
    (nvemacs--note-relocate 'nvemacs-note-move old new)
    (message "nvEmacs: moved %s to %s" (file-name-nondirectory new) (abbreviate-file-name dest))))

(defun nvemacs-note-move-outside (dir)
  "Move the selected note to DIR, a folder nvEmacs does not watch.
The note leaves the index, so this asks first.  Edits waiting for it
are written, its tags file goes with it, and a buffer nvEmacs opened
for it is closed."
  (interactive
   (list (read-directory-name
          (format "Move %s out of nvEmacs, to folder: "
                  (file-name-nondirectory (nvemacs--note-target 'nvemacs-note-move-outside)))
          nil nil t)))
  (let* ((old (nvemacs--note-target 'nvemacs-note-move-outside))
         (name (file-name-nondirectory old))
         (dest (nvemacs--folder-path dir))
         (new (expand-file-name name dest)))
    (when (member dest nvemacs--listed-dirs)
      (nvemacs--command-error
       'nvemacs-note-move-outside
       (format "requires a folder nvEmacs does not watch, and %s is watched" dir)
       "Use M-x nvemacs-note-move to move the note there"))
    (unless (file-directory-p dest)
      (nvemacs--command-error
       'nvemacs-note-move-outside (format "requires an existing folder, and %s does not exist" dir)
       "Create it and then re-run the command"))
    (nvemacs--check-destination 'nvemacs-note-move-outside old new)
    (if (not (y-or-n-p (format "Stop finding %s in nvEmacs and move it to %s? "
                               name (abbreviate-file-name dest))))
        (message "nvEmacs: not moved")
      (nvemacs--note-settle-edits 'nvemacs-note-move-outside old)
      (let ((buf (find-buffer-visiting old)))
        (nvemacs--note-files-move old new)
        (nvemacs--forget old)
        (nvemacs--note-let-go buf new))
      (message "nvEmacs: moved %s to %s" name (abbreviate-file-name dest)))))

(defun nvemacs-note-delete ()
  "Move the selected note and its tags file to the system Trash.
This asks first.  Unsaved edits are kept in discarded/ for 7 days, and
the note goes to the Trash as it is on disk.  A note with unsaved edits
in a buffer you had open before nvEmacs must be saved or reverted first."
  (interactive)
  (let* ((path (nvemacs--note-target 'nvemacs-note-delete))
         (name (file-name-nondirectory path))
         (buf (find-buffer-visiting path))
         (owned (and buf (buffer-local-value 'nvemacs--owned buf)))
         (edits (and buf (with-current-buffer buf
                           (or nvemacs--sister-held
                               (and (buffer-modified-p) (not (nvemacs--same-as-file-p))))))))
    (when (and edits (not owned))
      (nvemacs--command-error
       'nvemacs-note-delete
       (format "requires that %s has no unsaved edits in a buffer you opened yourself" name)
       "Save or revert it and then re-run the command"))
    (if (not (y-or-n-p (format "%sMove %s to the Trash? "
                               (if edits
                                   (format "Its unsaved edits are kept in discarded/ for %d days.  "
                                           nvemacs--discarded-days)
                                 "")
                               name)))
        (message "nvEmacs: not deleted")
      (let ((sister (nvemacs--sister path)))
        (nvemacs--move-to-trash path)
        (when (file-exists-p sister)
          (with-demoted-errors "nvEmacs: moving the tags file to the Trash: %S"
            (nvemacs--move-to-trash sister))))
      (when owned
        (with-current-buffer buf
          (when edits (nvemacs--discard-edits))
          ;; Closing a note writes waiting tags: they would bring back
          ;; the tags file that just went to the Trash.
          (setq nvemacs--sister-held nil)
          (set-buffer-modified-p nil)))
      (nvemacs--forget path)
      (nvemacs--note-let-go buf)
      (message "nvEmacs: moved %s to the Trash" name))))

;;;; Home page
;;
;; A plain Org or Markdown file in the data directory, built from a template
;; when nvEmacs exits, or at the first launch of a new version, and shown in
;; the note pane at launch.  Org pages are
;; rendered by Org, loaded only after the list is on screen (about 200 ms).
;; Markdown pages use a small viewer: markdown-mode is not part of Emacs.
;; What a crash could make stale (edits to recover) is worked out at launch
;; and shown above the file, never stored in it.

(defconst nvemacs-version (package-get-version) "The version of nvEmacs.")

(defun nvemacs-version ()
  "Show the version of nvEmacs that is running."
  (interactive)
  (message "nvEmacs %s" (or nvemacs-version "unknown")))

(defcustom nvemacs-homepage-file nil
  "File of the live home page.
nil means homepage.EXT in `nvemacs-data-directory', with the extension
of `nvemacs-homepage-template'.  nvEmacs overwrites this file: to change
the page, change the template.  Keep it out of the watched directories,
or it is indexed as a note."
  :type '(choice (const :tag "In the data directory" nil) file)
  :package-version '(nvemacs . "0.0.3"))

(defcustom nvemacs-homepage-template "homepage.org"
  "Template of the home page, a file in `nvemacs-template-directory'.
nvEmacs ships homepage.org and homepage.md.  A name not found there is
looked up among the shipped templates."
  :type 'string
  :package-version '(nvemacs . "0.0.3"))

(defcustom nvemacs-homepage-show-at-start t
  "Non-nil means show the home page when nvEmacs starts.
\\[nvemacs-homepage-show] shows it at any time."
  :type 'boolean
  :package-version '(nvemacs . "0.0.3"))

(defface nvemacs-homepage-heading '((t :inherit bold))
  "Face of the headings on a Markdown home page.")

(defface nvemacs-homepage-code '((t :inherit font-lock-constant-face))
  "Face of `code' on a Markdown home page.")

(defconst nvemacs--homepage-history-size 20 "Sessions of timings to keep.")
(defconst nvemacs--homepage-recent-notes 8 "Recently edited notes to list.")

(defvar nvemacs--homepage-run nil
  "This session's timings and startup check, a plist, or nil.")

(defvar-keymap nvemacs-homepage-mode-map
  :doc "Keys of the home page."
  "RET" #'nvemacs-homepage-open-link
  "q" #'nvemacs-searchbar-return
  "g" #'nvemacs-homepage-show)

(defconst nvemacs--default-keymaps
  `(("list" . ,(copy-keymap nvemacs-mode-map))
    ("note" . ,(copy-keymap nvemacs-note-mode-map))
    ("home page" . ,(copy-keymap nvemacs-homepage-mode-map)))
  "Where nvEmacs's keys work and the keymaps as it ships them.
They are copied before any user change.")

(defun nvemacs--homepage-file ()
  "Return the path of the live home page."
  (or nvemacs-homepage-file
      (expand-file-name (concat "homepage."
                                (or (file-name-extension nvemacs-homepage-template) "org"))
                        nvemacs-data-directory)))

(defun nvemacs--homepage-md-p ()
  "Non-nil if the home page template is Markdown."
  (member (downcase (or (file-name-extension nvemacs-homepage-template) ""))
          '("md" "markdown")))

(defun nvemacs--homepage-template-file ()
  "Return the home page template: the user's if any, else the shipped one."
  (cl-find-if #'file-readable-p
              (list (expand-file-name nvemacs-homepage-template nvemacs-template-directory)
                    (expand-file-name nvemacs-homepage-template
                                      (expand-file-name "templates/" nvemacs--directory)))))

(defun nvemacs--homepage-history-file ()
  "Return the file of the recent sessions' timings."
  (expand-file-name "session-times.eld" nvemacs-data-directory))

(defun nvemacs--homepage-history ()
  "Return the recent sessions' timings, newest first; nil if unreadable."
  (let ((f (nvemacs--homepage-history-file)))
    (when (file-readable-p f)
      (ignore-errors
        (cl-remove-if-not (lambda (e) (numberp (plist-get e :ready-ms)))
                          (with-temp-buffer
                            (insert-file-contents f)
                            (read (current-buffer))))))))

(defun nvemacs--homepage-history-add (entry)
  "Add ENTRY, a plist of timings, keeping the newest sessions."
  (let* ((all (cons entry (nvemacs--homepage-history)))
         (kept (cl-subseq all 0 (min (length all) nvemacs--homepage-history-size)))
         (f (nvemacs--homepage-history-file)))
    (nvemacs--private-directory (file-name-directory f))
    (with-temp-buffer
      (insert ";; nvEmacs timings of recent sessions (safe to delete).\n")
      (prin1 kept (current-buffer))
      (insert "\n")
      (nvemacs--write-file f))))

(defun nvemacs--homepage-ms (ms)
  "Return the duration MS, in milliseconds, for reading."
  (if (< ms 1000) (format "%d ms" (round ms)) (format "%.2f s" (/ ms 1000.0))))

(defun nvemacs--homepage-times (key)
  "Describe the timing KEY over the recent sessions."
  (let ((vals (delq nil (mapcar (lambda (e) (plist-get e key)) (nvemacs--homepage-history)))))
    (if (null vals)
        "not measured yet"
      (format "%s (min %s, average %s, max %s; last %d session%s)"
              (nvemacs--homepage-ms (car vals))
              (nvemacs--homepage-ms (apply #'min vals))
              (nvemacs--homepage-ms (/ (apply #'+ vals) (float (length vals))))
              (nvemacs--homepage-ms (apply #'max vals))
              (length vals) (if (= 1 (length vals)) "" "s")))))

(defun nvemacs--homepage-checked (run)
  "The startup check RUN finished: the index is ready.
Index ready is the index step plus this check, not the deliberately
paced growth of the list."
  (let ((h nvemacs--homepage-run))
    (when h
      (let ((ready (+ (plist-get h :index-ms) (plist-get run :wall-ms))))
        (setq nvemacs--homepage-run
              (append (list :checked t :new (plist-get run 'new)
                            :changed (plist-get run 'changed)
                            :deleted (plist-get run 'deleted))
                      h))
        (with-demoted-errors "nvEmacs: home page: %S"
          (nvemacs--homepage-history-add
           (list :when (format-time-string "%F %T")
                 :list-ms (plist-get h :list-ms) :ready-ms ready))
          (when (get-buffer-window nvemacs--homepage-name t)
            (nvemacs--homepage-render)))))))

(defun nvemacs--homepage-link (path title md)
  "Return a link to note PATH labelled TITLE, in Markdown if MD.
Brackets would break a link: a title gets parentheses instead, and a
path with brackets is listed without a link."
  (let ((label (string-replace "]" ")" (string-replace "[" "(" title))))
    (cond ((string-match-p "[][<>\n]" path) label)
          (md (format "[%s](<file:%s>)" label path))
          (t (format "[[file:%s][%s]]" path label)))))

(defun nvemacs--homepage-recent (md)
  "Return the recently edited notes as a list, newest first, in Markdown if MD."
  (mapconcat
   (lambda (r)
     (pcase-let ((`(,path ,title ,mtime) r))
       (format "- %s (%s)"
               (nvemacs--homepage-link
                path (if (string-empty-p (or title "")) (file-name-base path) title) md)
               (format-time-string "%F" mtime))))
   (sqlite-select (nvemacs--db) "SELECT path, title, mtime FROM meta ORDER BY id DESC LIMIT ?"
                  (list nvemacs--homepage-recent-notes))
   "\n"))

(defun nvemacs--homepage-table (header rows md)
  "Return a table of HEADER and ROWS, lists of strings, in Markdown if MD."
  (let* ((clean (lambda (r) (mapcar (lambda (c) (string-replace "|" "¦" c)) r)))
         (all (mapcar clean (cons header rows)))
         (widths (cl-loop for i below (length header)
                          collect (apply #'max (mapcar (lambda (r) (string-width (nth i r))) all))))
         (line (lambda (r)
                 (concat "| "
                         (mapconcat #'identity
                                    (cl-mapcar (lambda (c w)
                                                 (concat c (make-string (- w (string-width c)) ?\s)))
                                               r widths)
                                    " | ")
                         " |")))
         (rule (if md
                   (concat "| " (mapconcat (lambda (w) (make-string w ?-)) widths " | ") " |")
                 (concat "|" (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "+") "|"))))
    (mapconcat #'identity
               (append (list (funcall line (car all)) rule) (mapcar line (cdr all)))
               "\n")))

(defun nvemacs--key-names (cmd maps)
  "Return the keys of command CMD in keymaps MAPS, for reading.
A remapped command shows the key the user presses."
  (let ((names nil))
    (dolist (k (where-is-internal cmd maps nil t t))
      (cond
       ((memq (aref k 0) '(menu-bar tool-bar)))
       ((eq (aref k 0) 'remap)
        (dolist (g (where-is-internal (aref k 1) (current-global-map)))
          (unless (memq (aref g 0) '(menu-bar tool-bar))
            (push (key-description g) names))))
       ;; A click is also bound as its drag and double forms: one is enough.
       ((string-match-p "\\`<\\(?:drag\\|double\\|triple\\)-mouse" (key-description k)))
       (t (push (key-description k) names))))
    (if names (mapconcat #'identity (delete-dups (nreverse names)) ", ") "-")))

(defun nvemacs--homepage-key-rows (cmd)
  "Return the key table's rows for command CMD, one per place its keys work.
A command with no key gets one row: it is run by name, with \\[execute-extended-command]."
  (let ((rows nil))
    (pcase-dolist (`(,where . ,map)
                   `(("list" . ,nvemacs-mode-map)
                     ("note" . ,nvemacs-note-mode-map)
                     ("home page" . ,nvemacs-homepage-mode-map)
                     ("everywhere" . ,(current-global-map))))
      (let* ((shipped (cdr (assoc where nvemacs--default-keymaps)))
             (default (if shipped (nvemacs--key-names cmd (list shipped)) "-"))
             (current (nvemacs--key-names cmd (list map))))
        (unless (and (equal default "-") (equal current "-"))
          (push (list (symbol-name cmd) where default current) rows))))
    (or (nreverse rows) (list (list (symbol-name cmd) "M-x" "-" "-")))))

(defun nvemacs--homepage-commands (md)
  "Return a table of every command, where its keys work and which they are.
The table is in Markdown if MD."
  (let ((cmds nil))
    (mapatoms (lambda (s)
                (let ((n (symbol-name s)))
                  (when (and (commandp s)
                             (or (eq s 'nvemacs) (string-prefix-p "nvemacs-" n))
                             (not (string-prefix-p "nvemacs--" n))
                             (not (string-suffix-p "-mode" n)))
                    (push s cmds)))))
    (nvemacs--homepage-table
     '("Command" "Where" "Default" "Current")
     (mapcan #'nvemacs--homepage-key-rows (sort cmds #'string<))
     md)))

(defun nvemacs--homepage-values (md)
  "Return the alist (PLACEHOLDER . TEXT) of the home page, in Markdown if MD."
  (let* ((db (nvemacs--db))
         (size (lambda (n) (file-size-human-readable (or n 0) (quote iec) " ")))
         (index (nvemacs--index-path)))
    `(("written" . ,(format-time-string "%F %R"))
      ("version" . ,(or nvemacs-version "unknown"))
      ("emacs-version" . ,emacs-version)
      ("sqlite-version" . ,(caar (sqlite-select db "SELECT sqlite_version()")))
      ("notes" . ,(nvemacs--group-digits (nvemacs--note-count)))
      ("notes-size" . ,(funcall size (caar (sqlite-select db "SELECT sum(size) FROM meta"))))
      ("notes-by-type"
       . ,(mapconcat
           (lambda (ext)
             (let ((r (car (sqlite-select
                            db "SELECT count(*), sum(size) FROM meta WHERE path LIKE ?"
                            (list (concat "%." ext))))))
               (format "- %s: %s (%s)" ext (nvemacs--group-digits (car r))
                       (funcall size (cadr r)))))
           nvemacs-import-extensions "\n"))
      ("folders" . ,(mapconcat (lambda (d) (concat "- " d)) nvemacs-watched-directories "\n"))
      ("index-file" . ,index)
      ("index-size" . ,(funcall size (file-attribute-size (file-attributes index))))
      ("last-rebuild"
       . ,(or (caar (sqlite-select db "SELECT value FROM info WHERE key = 'rebuilt'"))
              "not recorded"))
      ("list-time" . ,(nvemacs--homepage-times :list-ms))
      ("index-ready" . ,(nvemacs--homepage-times :ready-ms))
      ("malformed-file" . ,(nvemacs--malformed-file))
      ("malformed-count" . ,(nvemacs--malformed-summary))
      ("history-state" . ,(if (nvemacs--history-on-p) "on" "off"))
      ("history-count" . ,(format "%d/%d" (if (nvemacs--history-on-p) (length (nvemacs--history-read)) 0)
                                  (nvemacs--history-size t)))
      ("history-file" . ,(if (nvemacs--history-on-p) (nvemacs--history-file) "none"))
      ("recent" . ,(nvemacs--homepage-recent md))
      ("commands" . ,(nvemacs--homepage-commands md)))))

(defun nvemacs--homepage-expand (template values)
  "Fill the {placeholders} of TEMPLATE from the alist VALUES.
The values go in as written, and an unknown placeholder is left alone."
  (replace-regexp-in-string
   "{\\([a-z-]+\\)}"
   (lambda (m) (let ((v (assoc (substring m 1 -1) values))) (if v (cdr v) m)))
   template t t))

(defun nvemacs--homepage-write ()
  "Write the home page file from its template; return the file, or nil.
The version writing it is remembered, so a later version can tell the
saved page is not its own."
  (let ((tpl (nvemacs--homepage-template-file)) (file (nvemacs--homepage-file)))
    (when tpl
      (nvemacs--malformed-save)         ; the page links to it
      (let ((text (nvemacs--homepage-expand
                   (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8)) (insert-file-contents tpl))
                     (buffer-string))
                   (nvemacs--homepage-values (nvemacs--homepage-md-p)))))
        (nvemacs--private-directory (file-name-directory file))
        (with-temp-buffer (insert text) (nvemacs--write-file file))
        (let ((state (nvemacs--state-read)))
          (unless (equal (alist-get 'homepage-version state) nvemacs-version)
            (nvemacs--state-write (cons (cons 'homepage-version nvemacs-version)
                                        (assq-delete-all 'homepage-version state)))))
        file))))

(defun nvemacs--homepage-save ()
  "Write the home page if the index is open.  An error never stops an exit."
  (when nvemacs--db
    (with-demoted-errors "nvEmacs: writing the home page: %S"
      (nvemacs--homepage-write))))

(defun nvemacs--homepage-text ()
  "Return the home page's text, first writing it if it needs it.
That is when it is missing, older than its template, or saved by another
version of nvEmacs."
  (let ((file (nvemacs--homepage-file)) (tpl (nvemacs--homepage-template-file)))
    (when (and tpl (or (not (file-exists-p file))
                       (file-newer-than-file-p tpl file)
                       (not (equal (alist-get 'homepage-version (nvemacs--state-read))
                                   nvemacs-version))))
      (nvemacs--homepage-write))
    (when (file-readable-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
        (buffer-string)))))

(defun nvemacs--homepage-live ()
  "Return the part of the page that must be current, shown above the file.
A crash skips the exit write, so none of this can be stored in it."
  (let* ((h (if (nvemacs--homepage-md-p) "# " "* "))
         (run nvemacs--homepage-run)
         (plural (lambda (n one many) (format "%d %s" n (if (= n 1) one many))))
         (pending (length (nvemacs--pending-records)))
         (orphans (if (plist-get run :checked) (length (nvemacs--orphan-tag-files)) 0))
         (malformed (car (nvemacs--malformed-counts)))
         (attention
          (append
           (when (> pending 0)
             (list (concat (funcall plural pending "note has" "notes have")
                           " unsaved edits: M-x nvemacs-edits-recover")))
           (when nvemacs--skipped
             (list (concat (funcall plural (length nvemacs--skipped)
                                    "note could not be indexed" "notes could not be indexed")
                           ": M-x nvemacs-index-skipped")))
           (when (> malformed 0)
             (list (concat (funcall plural malformed "note is malformed" "notes are malformed")
                           ": M-x nvemacs-index-malformed")))
           (mapcar (lambda (d) (concat "Watched folder not found: " d))
                   (cl-remove-if #'file-directory-p nvemacs-watched-directories))
           (when (> orphans 0)
             (list (concat (funcall plural orphans "tag file has no note" "tag files have no note")
                           ": M-x nvemacs-report-index"))))))
    (concat
     (when (or (null nvemacs-watched-directories) (zerop (nvemacs--note-count)))
       (concat h "Getting started\n\n"
               "- Choose your notes folders: M-x nvemacs-index-folder-add\n\n"))
     (when attention
       (concat h "Needs attention\n\n"
               (mapconcat (lambda (a) (concat "- " a)) attention "\n") "\n\n"))
     (when (plist-get run :checked)
       (let ((n (plist-get run :new)) (c (plist-get run :changed)) (d (plist-get run :deleted)))
         (concat h "Since last session\n\n"
                 (if (zerop (+ n c d))
                     "- No notes changed"
                   (format "- %d added, %d changed, %d deleted" n c d))
                 "\n\n"))))))

(defun nvemacs--homepage-open-path (path)
  "Open note PATH from the home page.
The page is a plain file, so a path outside the notes is refused."
  (cond
   ((nvemacs--malformed-path-p path) (nvemacs-index-malformed))
   ((and (file-regular-p path) (nvemacs--watched-note-p path))
    (nvemacs--open-path path))
   (t
    (nvemacs--command-message
     'nvemacs-homepage-open-link
     (format "requires a note in a watched folder, and %s is not" (nvemacs--short-path path))
     "Press g to refresh the page and then try another link"))))

(defun nvemacs--homepage-link-path ()
  "Return the path of the Org note link at point, or nil."
  (let ((pos (point)) (found nil))
    (save-excursion
      (beginning-of-line)
      (while (and (not found)
                  (re-search-forward "\\[\\[file:\\([^]\n]+\\)\\]\\[[^]\n]*\\]\\]"
                                     (line-end-position) t))
        (when (and (<= (match-beginning 0) pos) (< pos (match-end 0)))
          (setq found (match-string-no-properties 1)))))
    found))

(defun nvemacs--homepage-org-follow ()
  "Open the note link at point; return non-nil if there is one.
An `org-open-at-point-functions' entry: left to Org, a click would open
the note in some other window instead of nvEmacs's pane."
  (let ((path (nvemacs--homepage-link-path)))
    (when path
      (nvemacs--homepage-open-path path)
      t)))

(defun nvemacs-homepage-open-link ()
  "Open the note whose title is at point on the home page."
  (interactive)
  (unless (nvemacs--homepage-org-follow)
    (message "%s" (nvemacs--command-text 'nvemacs-homepage-open-link
                                         "requires a note's title at the cursor"
                                         "Move to a title and then re-run the command"))))

(define-minor-mode nvemacs-homepage-mode
  "The keys of the nvEmacs home page, which is read-only.
RET on a note's title opens it.
\\{nvemacs-homepage-mode-map}"
  :lighter " home")

(define-derived-mode nvemacs-homepage-md-mode special-mode "nvEmacs home"
  "A home page from a Markdown template, with its markup hidden."
  (setq-local truncate-lines t))

(defun nvemacs--homepage-md-view ()
  "Show this buffer's Markdown with the markup hidden and links as buttons."
  (unless (derived-mode-p 'nvemacs-homepage-md-mode) (nvemacs-homepage-md-mode))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(#+ \\).*$" nil t)
      (add-face-text-property (match-beginning 0) (match-end 0) 'nvemacs-homepage-heading)
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t))
    (goto-char (point-min))
    (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
      (add-face-text-property (match-beginning 1) (match-end 1) 'nvemacs-homepage-code)
      (put-text-property (match-beginning 0) (match-beginning 1) 'invisible t)
      (put-text-property (match-end 1) (match-end 0) 'invisible t))
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]\n]*\\)\\](<file:\\([^>\n]+\\)>)" nil t)
      (let ((title (match-string 1)) (path (match-string 2)))
        (replace-match "" t t)
        (insert-text-button title 'follow-link t 'nvemacs-path path
                            'action (lambda (b) (nvemacs--homepage-open-path
                                                 (button-get b 'nvemacs-path))))))))

(defvar org-startup-folded)
(defvar org-hide-emphasis-markers)
(defvar org-hide-leading-stars)
(defvar org-open-at-point-functions)
(declare-function org-mode "org" ())

(defun nvemacs--homepage-org-view ()
  "Show this buffer's Org text rendered by Org, with the markup hidden.
The whole page shows however the user's Org starts, and nothing on it
can set local variables."
  (require 'org)
  (unless (derived-mode-p 'org-mode)
    (let ((enable-local-variables nil)
          (org-startup-folded 'showeverything)
          (org-hide-leading-stars t))
      (org-mode))
    (setq-local org-hide-leading-stars t org-hide-emphasis-markers t)
    (add-hook 'org-open-at-point-functions #'nvemacs--homepage-org-follow nil t))
  (font-lock-flush))

(defun nvemacs--homepage-render ()
  "Draw the home page buffer and return it, or nil with no page to show."
  (let ((text (nvemacs--homepage-text)) (md (nvemacs--homepage-md-p)))
    (when text
      ;; Org takes about 200 ms to load: keep the list on screen meanwhile.
      (unless (or md (featurep 'org)) (redisplay))
      (with-current-buffer (get-buffer-create nvemacs--homepage-name)
        (let ((inhibit-read-only t) (pos (point))
              (starts (mapcar (lambda (w) (cons w (window-start w)))
                              (get-buffer-window-list (current-buffer) nil t))))
          (erase-buffer)
          (insert (nvemacs--homepage-live) text)
          (if md (nvemacs--homepage-md-view) (nvemacs--homepage-org-view))
          (nvemacs-homepage-mode 1)
          (setq buffer-read-only t)
          (goto-char (min pos (point-max)))
          (dolist (w starts) (set-window-start (car w) (min (cdr w) (point-max)))))
        (current-buffer)))))

(defun nvemacs--homepage-up-buffer (frame)
  "Return the home page buffer if the page is up in FRAME, else nil.
The page is up from when it is shown until a note is selected or
opened.  A resize can take the pane away and bring it back: the frame
parameter is what remembers the page meanwhile."
  (when (nvemacs--fparam 'nvemacs-homepage-up frame)
    (or (get-buffer nvemacs--homepage-name)
        (with-demoted-errors "nvEmacs: showing the home page: %S"
          (nvemacs--homepage-render)))))

(defun nvemacs--homepage-display ()
  "Put the home page up, from its top, in the note pane if there is one.
With no room for a pane, it appears when the frame grows."
  (nvemacs--set-fparam 'nvemacs-homepage-up t)
  (let ((nw (nvemacs--note-window)))
    (when nw
      (with-demoted-errors "nvEmacs: showing the home page: %S"
        (let ((buf (nvemacs--homepage-render)))
          (if (not buf)
              (message "%s" (nvemacs--command-text
                             "The home page"
                             (format "requires the template %s, which was not found"
                                     nvemacs-homepage-template)
                             "Set nvemacs-homepage-template to a template that exists"))
            (set-window-buffer nw buf)
            (set-window-start nw (with-current-buffer buf (point-min)))
            (set-window-point nw (with-current-buffer buf (point-min)))))))))

(defun nvemacs-homepage-show ()
  "Show the home page, brought up to date, in the notecontent area.
The cursor goes to the search line.  Selecting a note's row replaces
the page."
  (interactive)
  (nvemacs-searchbar-return)
  (unless (nvemacs--note-window)        ; list-only: the temporary pane of C-o
    (nvemacs--set-fparam 'nvemacs-temp t)
    (nvemacs--apply-layout)
    (nvemacs--focus-query))
  (nvemacs--homepage-save)
  (nvemacs--homepage-display))

;;;; How to search: USAGE.txt

(defconst nvemacs--usage-buffer-name "*nvEmacs usage*")

(defun nvemacs--usage-file ()
  "USAGE.txt, beside nvemacs.el."
  (expand-file-name "USAGE.txt" nvemacs--directory))

(defun nvemacs-usage ()
  "Show USAGE.txt, how to search, read-only in the notecontent area.
After a message about a search with groups, it opens at \"Boolean
search\"; otherwise at the top.  The cursor stays on the search line,
so the search can be fixed while the page is read: \[scroll-other-window]
scrolls it.  Selecting a note's row replaces the page."
  (interactive)
  (let ((file (nvemacs--usage-file)) (section nvemacs--usage-section))
    (unless (file-readable-p file)
      (nvemacs--command-error
       'nvemacs-usage
       (format "requires USAGE.txt beside nvemacs.el, and %s was not found"
               (nvemacs--short-path file))
       "Reinstall nvEmacs, or read USAGE.txt at https://github.com/tcharlesyun/nvemacs"))
    (let ((buf (get-buffer-create nvemacs--usage-buffer-name)) (pos nil))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (setq buffer-read-only t)
          (goto-char (point-min))
          (setq pos (or (and section
                             (re-search-forward (concat "^" (regexp-quote section) "\n-+$") nil t)
                             (line-beginning-position 0))
                        (point-min)))))
      (nvemacs-searchbar-return)
      (unless (nvemacs--note-window)    ; list-only: the temporary pane of C-o
        (nvemacs--set-fparam 'nvemacs-temp t)
        (nvemacs--apply-layout)
        (nvemacs--focus-query))
      (let ((nw (nvemacs--note-window)))
        (when nw
          (set-window-buffer nw buf)
          (set-window-start nw pos)
          (set-window-point nw pos))))))

;;;###autoload
(defun nvemacs ()
  "Start nvEmacs in this frame: take it over and show the newest notes.
Each frame keeps its own arrangement; the search buffer is shared.
With the mode variable `nvemacs-instrumentation' on, a start that builds
the list is measured and logged (startup.jsonl)."
  (interactive)
  (nvemacs--require-sqlite)              ; before anything is created
  (nvemacs--comments-restore)            ; before an index may be built
  (nvemacs--history-start-session)
  (nvemacs--folders-load)
  (when (nvemacs--folders-ask-p)
    (nvemacs--folders-ask))
  (let* ((t0 (float-time))
         (call-time (current-time))
         (rebuilt (unless (and (file-exists-p (nvemacs--index-path)) (nvemacs--index-current-p))
                    (message "nvEmacs: building the index…")
                    (nvemacs-index-rebuild)
                    t))
         (index-ms (progn (nvemacs--ensure-match-width) (nvemacs--comments-check) (nvemacs--ms t0)))
         (new (not (get-buffer nvemacs--buffer-name))))
    (nvemacs--sync-enable)
    (nvemacs--grow-enable)
    (add-hook 'kill-emacs-hook #'nvemacs--on-kill-emacs)
    (unless (nvemacs--fparam 'nvemacs-active)
      (nvemacs--set-fparam 'nvemacs-l0 (current-window-configuration))
      (nvemacs--set-fparam 'nvemacs-active t))
    (get-buffer-create nvemacs--buffer-name)
    (when new
      (setq nvemacs--away nil)
      (with-current-buffer nvemacs--buffer-name
        (nvemacs-mode)
        (nvemacs--setup-buffer)))
    (nvemacs--apply-layout)
    (when new
      (nvemacs--widths-start)
      (nvemacs--sort-start)
      (setq nvemacs--columns-now nil)   ; toggles last for one session
      (when nvemacs-instrumentation
        (setq nvemacs--startup (list :t0 t0 :call-time call-time :rebuilt rebuilt
                                     :index index-ms :cold (not nvemacs--started-in-process))))
      (nvemacs--session-begin)
      ;; The list is usable at once with the last-known index; the
      ;; reconcile runs in idle slices and refreshes it.
      (with-current-buffer nvemacs--buffer-name (nvemacs--search))
      (setq nvemacs--homepage-run (list :index-ms index-ms
                                        :list-ms (- (nvemacs--ms t0) index-ms)))
      (setq nvemacs--started-in-process t)
      (when nvemacs-reconcile-on-startup
        (setq nvemacs--sync-trigger 'startup)
        (nvemacs--sync-soon 0))
      (nvemacs--discarded-prune))
    (nvemacs--focus-query)
    (when nvemacs--startup
      (with-current-buffer nvemacs--buffer-name
        (nvemacs--startup-finish (length nvemacs--shown) (nvemacs--screen-rows))))
    ;; Last: rendering may load Org, and the list is timed and shown first.
    (when new
      (when nvemacs-homepage-show-at-start (nvemacs--homepage-display))
      (nvemacs--recover-notice))))

(provide 'nvemacs)
;;; nvemacs.el ends here
