;;; nvemacs.el --- Notational Velocity style note search  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 T. Charles Yun

;; Author: T. Charles Yun
;; Assisted-by: LLM/Claude Code (2026)
;; Version: 0.0.3
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

;; Search-as-you-type over a folder of plain-text notes (Org, Markdown,
;; text), in the manner of Notational Velocity, backed by a SQLite FTS5
;; trigram index.  Set `nvemacs-watched-directories' and start with
;; M-x nvemacs.  README.txt, COMMANDS.txt and SETTINGS.txt at
;; https://github.com/tcharlesyun/nvemacs describe the keys, commands
;; and settings.

;;; Code:

(require 'cl-lib)
(require 'ucs-normalize)

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

(defcustom nvemacs-watched-directories '("~/notes/")
  "Directories whose notes are indexed."
  :type '(repeat directory))

(defcustom nvemacs-watched-directories-recursive t
  "Non-nil means descend into subdirectories of watched directories."
  :type 'boolean)

(defcustom nvemacs-ignore-dir-regexp "\\`\\."
  "Subdirectories whose name matches this are skipped (default: hidden)."
  :type 'regexp)

(defcustom nvemacs-import-extensions '("org" "md" "txt")
  "Which files are notes: only these extensions are read and indexed."
  :type '(repeat string))

(defcustom nvemacs-import-title-source 'auto
  "Where a note's title comes from.
`auto': title in the file, else filename, else first line.
`filename': always the filename.  `first-line': always the first line."
  :type '(choice (const auto) (const filename) (const first-line)))

(defcustom nvemacs-title-match-width 60
  "Display columns of a title that count as a TITLE match.
It exists to limit title matches: a paragraph-long \"title\" would make
most of its words title matches, which rank first. The title is cut at a
word boundary within this width (Notational Velocity cuts at 60 the same
way); the rest still matches as body text, and the notelist shows the cut
part in bold. 0 = the whole title. Changing it rebuilds only the title
tables, from the titles stored in the index (no note is read)."
  :type 'integer)

(defcustom nvemacs-import-max-size (* 10 1024 1024)
  "Notes larger than this many bytes are skipped."
  :type 'integer)

(defcustom nvemacs-filename-timestamp-regexp
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}h[0-9]\\{2\\}m-"
  "Leading timestamp removed from filenames used as titles."
  :type 'regexp)

(defcustom nvemacs-data-directory (locate-user-emacs-file "nvemacs/")
  "Directory where nvEmacs keeps the files it writes.
These are the index, the instrumentation logs, state.eld (remembered
values, such as the notelist tags width) and dates.eld (the created
dates of undated notes).  Your preferences are not here: they live in
your init file or `custom-file'."
  :type 'directory)

(defcustom nvemacs-index-file nil
  "The index database; nil = index.sqlite in `nvemacs-data-directory'."
  :type '(choice (const :tag "In the data directory" nil) file))

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

(defun nvemacs--parse-date (s)
  "Seconds since 1970 (local time) of the date at the start of S, or nil.
Accepts 2026-11-19-19h40m, 2024-05-01, 2024-05-01 10:30,
2024-05-01T10:30, Org timestamps <2024-05-01 Wed 10:30> and [2024-05-01
Wed], and quotes around any of them."
  (when (and s (string-match
                (concat "\\`[ \t\"'<[]*\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
                        "\\(?:\\(?:[T-]\\|[ \t]+\\(?:[[:alpha:]]+\\.?[ \t]+\\)?\\)"
                        "\\([0-9]\\{1,2\\}\\)[:h]\\([0-9]\\{2\\}\\)\\)?")
                s))
    (let ((y (string-to-number (match-string 1 s)))
          (mo (string-to-number (match-string 2 s)))
          (d (string-to-number (match-string 3 s)))
          (h (if (match-beginning 4) (string-to-number (match-string 4 s)) 0))
          (mi (if (match-beginning 5) (string-to-number (match-string 5 s)) 0)))
      (when (and (<= 1 mo 12) (<= 1 d 31) (<= 0 h 23) (<= 0 mi 59))
        (time-convert (encode-time (list 0 mi h d mo y nil -1 nil)) 'integer)))))

(defun nvemacs--filename-date (path)
  "Return the created date, in seconds, from the start of PATH's filename.
The filename must start with a timestamp such as 2024-05-01 or
2024-05-01-10h00m.  Return nil if it does not."
  (let ((base (file-name-nondirectory path)))
    (and (> (length base) 9) (<= ?0 (aref base 0) ?9)   ; no regexp for most names
         (string-match-p (concat "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                                 "\\(?:-[0-9]\\{2\\}h[0-9]\\{2\\}m\\)?\\(?:[-_ .]\\|\\'\\)")
                         base)
         (nvemacs--parse-date base))))

(defun nvemacs--org-created (limit)
  "Return the declared created date of the Org text in the current buffer.
That is a #+created: keyword or a :CREATED: property (file-level or in
the first heading), then #+date:.  Keywords count only before LIMIT,
the first heading.  Values that cannot be parsed are passed over."
  (let ((case-fold-search t) (created nil) (date nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not created) (re-search-forward "^#\\+\\(created\\|date\\):[ \t]*\\(.*\\)$" limit t))
        (let* ((key (match-string 1))                ; before parse-date's string-match
               (v (nvemacs--parse-date (match-string 2))))
          (if (string-equal-ignore-case key "created")
              (setq created v)
            (setq date (or date v)))))
      (or created
          (progn
            (goto-char (point-min))
            (let ((second (save-excursion
                            (goto-char limit)
                            (if (and (< limit (point-max)) (zerop (forward-line 1))
                                     (re-search-forward "^\\*+[ \t]" nil t))
                                (match-beginning 0)
                              (point-max))))
                  (found nil))
              (while (and (not found) (re-search-forward "^[ \t]*:CREATED:[ \t]*\\(.*\\)$" second t))
                (setq found (nvemacs--parse-date (match-string 1))))
              found))
          date))))

(defun nvemacs--front-matter-created (inside toml)
  "Declared created date in front matter text INSIDE: created, then date.
TOML non-nil means TOML lines (key = value), else YAML (key: value)."
  (let ((case-fold-search t))
    (cl-some (lambda (key)
               (and (string-match (format (if toml "^%s[ \t]*=[ \t]*\\(.*\\)$" "^%s:[ \t]*\\(.*\\)$") key)
                                  inside)
                    (nvemacs--parse-date (match-string 1 inside))))
             '("created" "date"))))

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
        ('org-keyword (when (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t) (nvemacs--delete-line)))
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
  :type '(repeat string))

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

(defun nvemacs--org-first-heading ()
  "Return the position of the first Org heading, or `point-max' if none."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\*+[ \t]" nil t) (match-beginning 0) (point-max))))

(defun nvemacs--org-tags-in-buffer ()
  "Return the tags of the Org text in the current buffer.
These are the `#+filetags:' lines before the first heading (any case;
several combine), as (TAG . file), and the heading tags, as
\(TAG . section).  A commented-out `# +filetags:' does not count."
  (save-excursion
    (let ((case-fold-search t) (limit (nvemacs--org-first-heading)) (out nil))
      (goto-char (point-min))
      (while (re-search-forward "^#\\+filetags:[ \t]*\\(.*\\)$" limit t)
        (dolist (tg (split-string (match-string-no-properties 1) "[: \t]+" t))
          (push (cons tg 'file) out)))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+[ \t]+.*?[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" nil t)
        (dolist (tg (split-string (match-string-no-properties 1) ":" t))
          (push (cons tg 'section) out)))
      (nreverse out))))

(defun nvemacs--md-front-matter ()
  "Front matter at the start of the buffer as (TYPE BEG END IBEG IEND):
TYPE `yaml' (---) or `toml' (+++); BEG..END the whole block up to the end
of its closing line; IBEG..IEND its inside (IEND = start of closing line)."
  (save-excursion
    (goto-char (point-min))
    (let ((type (cond ((looking-at "---[ \t]*\n") 'yaml)
                      ((looking-at "\\+\\+\\+[ \t]*\n") 'toml))))
      (when type
        (let ((ibeg (match-end 0)))
          (goto-char ibeg)
          (when (re-search-forward (if (eq type 'yaml) "^---[ \t]*$" "^\\+\\+\\+[ \t]*$") nil t)
            (list type (point-min) (match-end 0) ibeg (match-beginning 0))))))))

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
:tags (strings), :form (`flow', `block', `comma', `space' or `toml'),
:beg and :end (the text to replace) and :indent (block form)."
  (save-excursion
    (let ((case-fold-search t) (iend (nth 4 fm)))
      (goto-char (nth 3 fm))
      (if (eq (car fm) 'toml)
          (when (re-search-forward "^tags[ \t]*=[ \t]*\\[\\(.*\\)\\][ \t]*$" iend t)
            ;; Positions first: `split-string' in the items call resets match data.
            (let ((beg (match-beginning 0)) (end (match-end 0)))
              (list :tags (nvemacs--md-list-items (match-string-no-properties 1)) :form 'toml
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
FOLDED-TEXT OTHER-P TAGS PREVIEW CREATED):
TITLE, the full title for display (up to `nvemacs--title-store-max');
FOLDED-MATCH, its match part (`nvemacs--match-title'), folded, for the
title tables;
FOLDED-TEXT, the note's text, folded;
OTHER-P, non-nil if the text holds CJK characters (the `other' tables);
TAGS, the tags found in the text (Org, Markdown), ((TAG . KIND) ...);
PREVIEW, the body as one line (`nvemacs--preview');
CREATED, the declared created date in seconds, or nil.
The buffer is modified."
  (let* ((case-fold-search t)
         (ext (downcase (or (file-name-extension path) "")))
         (in-file-title nil)
         (heading nil)
         (tags nil)
         (created nil))
    ;; Per-extension filters, built in for Org and Markdown.
    (goto-char (point-min))
    (cond
     ((equal ext "org")
      (setq tags (nvemacs--org-tags-in-buffer))   ; before keyword lines go
      ;; #+title: counts only before the first heading (not one quoted in a
      ;; block further down).
      (let ((first-heading (save-excursion
                             (if (re-search-forward "^\\*+[ \t]" nil t) (match-beginning 0) (point-max)))))
        (when (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" first-heading t)
          (setq in-file-title (string-trim (match-string 1))))
        (setq created (nvemacs--org-created first-heading)))
      (setq heading (nvemacs--org-heading))
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      ;; `#+KEY:' lines other than the title, and commented-out keywords
      ;; `# +KEY:' (deliberate comments, not indexed).
      ;; Ordinary `# comment' lines are the user's text and stay. An EMPTY
      ;; `#+Title:' (new untitled note from a template) goes too: it is no
      ;; title, and kept it became the "first line" title (bug 2026-11-19).
      (while (re-search-forward "^#\\(?: \\)?\\+\\([A-Za-z_]+\\):\\(.*\\)$" nil t)
        (unless (and (string-equal-ignore-case (match-string 1) "title")
                     (not (string-prefix-p "# " (match-string 0)))
                     (not (string-blank-p (match-string 2))))
          (replace-match ""))))
     ((equal ext "md")
      (let ((fm (nvemacs--md-front-matter)))
        (when fm
          (setq tags (nvemacs--md-tags-in-buffer))
          (let ((inside (buffer-substring (nth 3 fm) (nth 4 fm)))
                (toml (eq (car fm) 'toml)))
            (when (string-match (if toml "^title[ \t]*=[ \t]*\\(.*\\)$" "^title:[ \t]*\\(.*\\)$")
                                inside)
              (setq in-file-title (string-trim (match-string 1 inside) "[\"' \t]+" "[\"' \t]+")))
            (setq created (nvemacs--front-matter-created inside toml)))
          (delete-region (nth 1 fm) (nth 2 fm))))
      (setq heading (nvemacs--md-heading))))
    (setq created (or created (nvemacs--filename-date path)))
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
      ;; A title that is not part of the note's text (filename, front
      ;; matter) joins the body text, so its words beyond the match part
      ;; still match as body.
      (when (and (memq (cdr pick) '(filename front-matter)) (not (string-empty-p title)))
        (goto-char (point-max))
        (insert "\n" title "\n"))
      (let ((spelling (nvemacs--filename-spelling path)))
        (when spelling
          (goto-char (point-max))
          (insert "\n" spelling "\n")))
      (nvemacs--fold-buffer)
      (let ((text (buffer-string)))
        (list title (nvemacs--fold (car (nvemacs--match-title title))) text
              (and (string-match-p nvemacs--cjk-regexp text) t)
              (nvemacs--tags-clean tags) preview created)))))

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
  "Signal a `user-error' that reads COMMAND EXPECTATION.  SOLUTION."
  (user-error "%s" (nvemacs--command-text command expectation solution)))

(defconst nvemacs--path-tail-length 20
  "How many characters of a path a background error shows, from its end.")

(defun nvemacs--short-path (path)
  "Return PATH for an error message: its filename, then the end of the path.
A full path can push the rest of the message out of the echo area."
  (format "%s <%s>" (file-name-nondirectory path)
          (if (<= (length path) nvemacs--path-tail-length)
              path
            (concat "..." (substring path (- nvemacs--path-tail-length))))))

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
                    "CREATE TABLE IF NOT EXISTS meta (id INTEGER PRIMARY KEY, path TEXT UNIQUE, size INTEGER, mtime INTEGER, title TEXT, other INTEGER, tmtime INTEGER, preview TEXT, created INTEGER, cguess INTEGER, added INTEGER, title_key TEXT, tags_key TEXT)")
    ;; Tags: FOLDED is the tag through `nvemacs--fold' (search is
    ;; case-insensitive); TAG keeps the spelling for display; KIND file|section.
    (sqlite-execute nvemacs--db
                    "CREATE TABLE IF NOT EXISTS tags (id INTEGER, tag TEXT, folded TEXT, kind TEXT)")
    (sqlite-execute nvemacs--db "CREATE INDEX IF NOT EXISTS tags_folded ON tags(folded)")
    (sqlite-execute nvemacs--db "CREATE INDEX IF NOT EXISTS tags_id ON tags(id)")
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

(defun nvemacs--insert-note (db path attrs rec)
  "Insert record REC of PATH (file ATTRS) into DB under rowid = mtime.
Every note gets an Added date (`nvemacs--added-date').  Its Created date
is the one it declares, or else Added, marked as a guess.  The folded
title and tags are stored as sort keys: Emacs's SQLite API has no custom
collation."
  (let ((id (nvemacs--mtime-us attrs)))
    (while (sqlite-select db "SELECT 1 FROM meta WHERE id = ?" (list id))
      (setq id (1+ id)))                          ; collision: nudge by 1
    (pcase-let* ((`(,title ,ftitle ,ftext ,other ,tags ,preview ,declared) rec)
                 (added (nvemacs--added-date path attrs))
                 (folded (mapcar (lambda (tk) (nvemacs--fold (car tk))) tags)))
      (sqlite-execute db "INSERT INTO meta VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
                      (list id path (file-attribute-size attrs)
                            (nvemacs--mtime-us attrs) title (if other 1 0)
                            (nvemacs--sister-mtime path)
                            preview (or declared added) (if declared 0 1)
                            added (nvemacs--fold (or title ""))
                            (nvemacs--tags-key folded)))
      (cl-loop for tk in tags for f in folded
               do (sqlite-execute db "INSERT INTO tags VALUES (?,?,?,?)"
                                  (list id (car tk) f (symbol-name (cdr tk)))))
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
  :type '(choice (const nil) file))

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
           (message "%s" (nvemacs--command-text
                          "The Added-dates file"
                          (format "requires valid entries, and %s is damaged at line %d"
                                  (nvemacs--short-path f) (line-number-at-pos))
                          "Repair or restore that file")))))))
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

(defconst nvemacs--index-version 8
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
8 = meta.added, and the sort keys meta.title_key and meta.tags_key.")

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
          (push entry notes))
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
  (when nvemacs--db (sqlite-close nvemacs--db) (setq nvemacs--db nil))
  (when (file-exists-p (nvemacs--index-path)) (delete-file (nvemacs--index-path)))
  (let* ((db (nvemacs--db))
         (files (nvemacs--note-files))
         (nvemacs--births (nvemacs--birth-times (mapcar #'car files)))
         (n 0))
    (setq nvemacs--skipped nil)
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
      (sqlite-execute db "INSERT OR REPLACE INTO info VALUES ('rebuilt', ?)"
                      (list (format-time-string "%F %R")))
      (nvemacs--bump-generation db))
    (nvemacs--dates-flush)
    (dolist (tbl nvemacs--tables)
      (sqlite-execute db (format "INSERT INTO %s(%s) VALUES('optimize')" tbl tbl)))
    ;; Reclaim the pages 'optimize' frees: without it the file was 3.3x the
    ;; text at 100 MB instead of ~1.6x (measured 2026-11-18).
    (sqlite-execute db "VACUUM")
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
  :type 'boolean)

(defcustom nvemacs-sync-delay 1.0
  "Seconds to wait after a change event before reconciling.
Events in between are coalesced (Notational Velocity uses a 1.0 s
FSEvents latency).  The reconcile then waits until Emacs has been idle
`nvemacs--sync-idle' seconds."
  :type 'number)

(defcustom nvemacs-sync-on-focus t
  "Non-nil means reconcile when an Emacs frame regains focus.
This is needed because file-notify (kqueue) does not report notes
rewritten in place."
  :type 'boolean)

(defcustom nvemacs-sync-slice-ms 10
  "Longest stretch of background work, in milliseconds.
Background work compares the folders with the index and indexes changed
notes; after this long, Emacs attends to typing again.  A key pressed
meanwhile waits at most this long."
  :type 'integer)

(defcustom nvemacs-auto-save-interval 3.0
  "Idle seconds before nvEmacs copies your edits to a temp file.
This covers the notes nvEmacs opened; a note you already had open when
nvEmacs showed it is yours to save.  The note's own file is written
when you save it, when the cursor leaves the note, and when nvEmacs
exits, and only if its text really changed.  A changed value takes
effect at the next pause.  nil means no temp files and no writing on
leaving or at exit; closing a changed note, or Emacs, still offers to
save it."
  :type '(choice (const :tag "Never" nil) number))

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
  :type '(choice (const :tag "Beside the index" nil) directory))

(defcustom nvemacs-log-max-bytes 10240
  "Largest size of each instrumentation log; the oldest records are dropped."
  :type 'integer)

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
     (message "%s" (nvemacs--command-text
                    (format "The %s log" kind)
                    (format "requires a writable data folder, and %s could not be written: %s"
                            (nvemacs--short-path
                             (expand-file-name (concat kind ".jsonl") (nvemacs--log-directory)))
                            (nvemacs--error-reason err))
                    "Check the permissions of nvemacs-data-directory")))))

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
      (when (eq (plist-get r :trigger) 'startup) (nvemacs--homepage-checked r))
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
  ;; Emacs has no hook before it asks which buffers to save.
  (advice-add 'save-some-buffers :before #'nvemacs--settle-unchanged)
  (advice-add 'save-buffers-kill-emacs :before #'nvemacs--settle-unchanged)
  (add-function :after after-focus-change-function #'nvemacs--focus-changed)
  (unless (and nvemacs--auto-save-timer (eql nvemacs--auto-save-armed nvemacs-auto-save-interval))
    (nvemacs--auto-save-arm))
  (nvemacs--note-files)                     ; sets `nvemacs--listed-dirs'
  (nvemacs--update-watches))

(defun nvemacs--sync-disable ()
  "Stop watches, timers, the focus trigger and auto-save."
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

(defun nvemacs--query-terms (query)
  "Fold QUERY and split it into terms."
  (split-string (nvemacs--fold query) "[ \t\n]+" t))

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

(defun nvemacs--parse-query (query)
  "Split QUERY into (TERMS INCLUDE EXCLUDE).
TERMS are the text terms; INCLUDE and EXCLUDE are the tags of `tag:x'
and `-tag:x', normalised and folded.  A bare `tag:' is ignored while it
is being typed."
  (let (terms inc exc)
    (dolist (tok (nvemacs--query-terms query))
      (if (string-match "\\`\\(-?\\)tag:\\(.*\\)\\'" tok)
          (let* ((neg (equal (match-string 1 tok) "-"))
                 ;; Not `nvemacs--tag-normalize': a trailing `_' being typed
                 ;; (`tag:alpha_') must narrow the list, not be trimmed.
                 (v (replace-regexp-in-string
                     "\\`_+" "" (nvemacs--underscore (match-string 2 tok) nvemacs--tag-chars))))
            (when (string-match-p "[^_]" v)
              (if neg (push (nvemacs--fold v) exc) (push (nvemacs--fold v) inc))))
        (push tok terms)))
    (list (nreverse terms) (nreverse inc) (nreverse exc))))

(defun nvemacs--tag-filter (inc exc)
  "Return (FRAGMENT . ARGS), SQL that restricts `rowid' by tags.
The notes kept have a tag that STARTS WITH each of INC and none
starting with any of EXC (a prefix, so the list narrows while a tag is
typed).  FRAGMENT is empty or begins with \" AND\"."
  (let ((frag "") (args nil))
    (dolist (v inc)
      (setq frag (concat frag " AND rowid IN (SELECT id FROM tags WHERE substr(folded, 1, ?) = ?)")
            args (append args (list (length v) v))))
    (dolist (v exc)
      (setq frag (concat frag " AND rowid NOT IN (SELECT id FROM tags WHERE substr(folded, 1, ?) = ?)")
            args (append args (list (length v) v))))
    (cons frag args)))

(defun nvemacs--search-all (query)
  "Every rowid matching QUERY, newest first.  Reference path for tests."
  (pcase-let* ((`(,terms ,inc ,exc) (nvemacs--parse-query query))
               (filter (nvemacs--tag-filter inc exc)))
    (cond
     (terms
      (let ((match (nvemacs--term-where terms)))
        (mapcar #'car
                (sqlite-select (nvemacs--db)
                               (format "SELECT rowid FROM %s WHERE %s%s ORDER BY rowid DESC"
                                       (cdr (nvemacs--route terms)) (car match) (car filter))
                               (append (cdr match) (cdr filter))))))
     ((or inc exc)
      (mapcar #'car (sqlite-select (nvemacs--db)
                                   (format "SELECT id FROM meta WHERE 1%s ORDER BY id DESC" (car filter))
                                   (cdr filter)))))))

;;;; Single-note update on save

(defun nvemacs--watched-note-p (path)
  "Non-nil if PATH is a note inside a watched directory."
  (and path
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
         (message "%s" (nvemacs--command-text
                        "The search index"
                        (format "requires a readable note, and %s was saved but not indexed: %s"
                                (nvemacs--short-path path)
                                (string-replace path (file-name-nondirectory path) (cadr err)))
                        "Fix the note, or see M-x nvemacs-index-skipped")))))))

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
  :type 'number)

(defcustom nvemacs-grow-delay 0.5
  "Idle seconds between steps that lengthen the notelist.
Each time Emacs has been idle this many more seconds, the list grows by
one screenful, until every match is listed.  It also grows at once when
the cursor nears its last row.  nil means the list stays one screenful."
  :type '(choice (number :tag "Seconds") (const :tag "Never" nil)))

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
  :type 'boolean)

(defface nvemacs-selected-title '((t :inherit region))
  "Face of a selected note's title shown on the search line.")

(defvar nvemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nvemacs-note-open)
    (define-key map (kbd "C-o") #'nvemacs-note-open-other-window)
    (define-key map (kbd "C-c C-c") #'nvemacs-searchbar-clear)
    (define-key map (kbd "C-c C-q") #'nvemacs-tags-edit) ; as in Org
    (define-key map (kbd "C-c C-]") #'nvemacs-quit)
    (define-key map (kbd "C-c C-g") #'nvemacs-index-refresh)
    (define-key map (kbd "C-c C-v") #'nvemacs-layout-cycle)
    (define-key map (kbd "C-c C-l") #'nvemacs-notelist-refresh)
    (define-key map (kbd "C-c C-n") #'nvemacs-note-new)
    (define-key map (kbd "C-c C-r") #'nvemacs-note-rename)
    (define-key map (kbd "C-c C-w") #'nvemacs-note-move)         ; as Org's refile
    (define-key map (kbd "C-c C-x C-a") #'nvemacs-note-move-outside) ; as Org's archive
    (define-key map (kbd "C-c ^") #'nvemacs-notelist-sort-column)    ; as Org's sort
    (define-key map (kbd "C-c C-x C-c") #'nvemacs-notelist-column-toggle) ; as Org's columns
    (define-key map [header-line mouse-1] #'nvemacs--heading-click)
    ;; Mouse: nothing happens on the press; the release selects. See
    ;; `nvemacs-notelist-mouse-select'.
    (define-key map [down-mouse-1] #'ignore)
    (dolist (k '([mouse-1] [double-mouse-1] [triple-mouse-1] [drag-mouse-1]))
      (define-key map k #'nvemacs-notelist-mouse-select))
    (define-key map (kbd "TAB") #'nvemacs-notelist-next)
    (define-key map (kbd "<backtab>") #'nvemacs-notelist-previous)
    ;; Search line: movement and basic query edits only. Other editing
    ;; commands are refused, remapped by command so every key bound to them
    ;; is covered. Result rows are read-only text anyway.
    (dolist (cmd '(delete-char delete-forward-char kill-line kill-region kill-word
                   transpose-chars transpose-words upcase-word downcase-word
                   capitalize-word undo undo-only undo-redo))
      (define-key map (vector 'remap cmd) #'nvemacs--refuse))
    map)
  "Keymap of the nvEmacs search buffer.")

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
  ;; The header line holds the column headings
  ;; (it never scrolls); the status moved to the mode line.
  (setq-local header-line-format '(:eval (nvemacs--headings)))
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

(defun nvemacs--after-change (beg _end _len)
  "Restart the debounce timer when the query changes.
An `after-change-functions' entry: BEG is where the change starts."
  (when (and (not nvemacs--inhibit-search) (nvemacs--in-query-p beg))
    ;; A new search selects nothing: blank the pane at once, so a search
    ;; with no matches never sits beside an unrelated note.
    (nvemacs--clear-pane)
    (nvemacs--schedule-search)))

(defun nvemacs--window-size (&optional win)
  "Return (WIDTH . HEIGHT), the body size of WIN in columns and lines.
WIN defaults to the window showing the current buffer.  Return nil if
there is no such window."
  (let ((win (or win (get-buffer-window (current-buffer)))))
    (and win (cons (window-body-width win) (window-body-height win)))))

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
                 string))

(defcustom nvemacs-notelist-preview t
  "Non-nil means show each note's body as one grey line after its title.
This is Notational Velocity's \"show note body previews\"."
  :type 'boolean)

(defcustom nvemacs-notelist-title-min-width 20
  "Narrowest title column; narrower windows drop columns instead."
  :type 'integer)

(defcustom nvemacs-notelist-tags-min-width 20
  "Narrowest tags column; with less room the tags column is hidden."
  :type 'integer)

(defcustom nvemacs-notelist-tags-max-width 40
  "Widest tags column a measurement may give; longer tags end in ….
nil: no limit."
  :type '(choice (const :tag "No limit" nil) integer))

(defcustom nvemacs-notelist-remember-widths 'last
  "How the notelist tags width is kept between sessions.
`last': saved on exit and on \\[nvemacs-notelist-refresh] into state.eld,
  used at the next start; `never': nothing saved, each start measures its
  first screen; `user': always `nvemacs-notelist-tags-width' (set it with
  `nvemacs-notelist-widths-set'); a refresh then lasts for the session only."
  :type '(choice (const last) (const never) (const user)))

(defcustom nvemacs-notelist-tags-width nil
  "Tags column width used when `nvemacs-notelist-remember-widths' is `user'."
  :type '(choice (const :tag "Measure" nil) integer))

(defcustom nvemacs-state-file nil
  "Remembered values (state.eld); nil = in `nvemacs-data-directory'."
  :type '(choice (const nil) file))

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
CGUESS TAGS-TEXT ADDED).  Every cell after the first starts at its column by
:align-to (pixels: fallback CJK fonts draw Hangul narrower than the 2
columns `string-width' counts)."
  (pcase-let ((`(,col ,start ,w) entry))
    (concat (if (zerop start) "" (propertize " " 'display `(space :align-to ,start)))
            (pcase col
              ('title (nvemacs--title-cell (nth 0 row) (nth 2 row) w))
              ('tags (propertize (nvemacs--fit (or (nth 5 row) "") w) 'face 'nvemacs-tags))
              ('modified (nvemacs--date-text (/ (float (nth 1 row)) nvemacs--rowid-units-per-second)))
              ('created (if (nth 3 row)
                            (concat (if (eql (nth 4 row) 1) "~" " ") (nvemacs--date-text (nth 3 row)))
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
  "Return the header line: the column headings.
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

(defun nvemacs--widths-start ()
  "Tags width for a new notelist, per `nvemacs-notelist-remember-widths'."
  (setq nvemacs--tags-width
        (pcase nvemacs-notelist-remember-widths
          ('last (let ((w (alist-get 'tags-width (nvemacs--state-read))))
                   (and (integerp w) (> w 0) w)))
          ('user nvemacs-notelist-tags-width)
          (_ nil))))

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
`last'; with `user' it lasts for this session only."
  (interactive)
  (with-current-buffer nvemacs--buffer-name
    (setq nvemacs--tags-width (nvemacs--measure-tags nvemacs--shown))
    (nvemacs--notelist-redraw)
    (pcase nvemacs-notelist-remember-widths
      ('last (nvemacs--widths-save)
             (message "nvEmacs: tags column %d (saved)" nvemacs--tags-width))
      ('user (message "nvEmacs: tags column %d for this session; not saved (M-x nvemacs-notelist-widths-set to keep)"
                      nvemacs--tags-width))
      (_ (message "nvEmacs: tags column %d (not saved)" nvemacs--tags-width)))))

(declare-function custom-file "cus-edit" (&optional no-error))

(defun nvemacs-notelist-widths-set (width)
  "Set the notelist tags column to WIDTH columns, and save that.
The width is saved in your Customize file, with
`nvemacs-notelist-remember-widths' set to `user', and applied at once."
  (interactive (list (read-number "Notelist tags column width: " (or nvemacs--tags-width 30))))
  (require 'cus-edit)
  (let ((saving (custom-file t)))      ; nil under emacs -q: Customize cannot save
    (customize-save-variable 'nvemacs-notelist-tags-width width)
    (customize-save-variable 'nvemacs-notelist-remember-widths 'user)
    (setq nvemacs--tags-width width)
    (when (get-buffer nvemacs--buffer-name)
      (with-current-buffer nvemacs--buffer-name (nvemacs--notelist-redraw)))
    (message (if saving "nvEmacs: tags column %d, saved as your setting"
               "nvEmacs: tags column %d for this session; NOT saved (Emacs started with -q, no init file)")
             width)))

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
Each entry is ID -> (TITLE MTIME PREVIEW CREATED CGUESS TAGS-TEXT ADDED),
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
        (dolist (r (sqlite-select db (format "SELECT id, title, mtime, preview, created, cguess, added FROM meta WHERE id IN (%s)" marks)
                                  chunk))
          (pcase-let ((`(,id ,title ,mtime ,preview ,created ,cguess ,added) r))
            (puthash id (list title mtime preview created cguess nil added) h)))
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
  (let ((terms (car (nvemacs--parse-query (nvemacs--search-text))))
        (found (make-hash-table)))
    (when terms
      (let ((table (car (nvemacs--route terms)))
            (match (nvemacs--term-where terms)))
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
        (pcase-let* ((`(,terms ,inc ,exc) (nvemacs--parse-query (nvemacs--search-text)))
                     (wins (get-buffer-window-list buf nil t))
                     (here (nvemacs--row-at (point)))
                     (win-ids (mapcar (lambda (w) (cons w (nvemacs--row-at (window-point w)))) wins))
                     (moved (nvemacs--rows-apply changes (null terms) (nvemacs--tag-filter inc exc)))
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
                (if (assq (car nvemacs--sort) nvemacs--layout)
                    ""
                  (format " · sorted by %s %s" (car nvemacs--sort) (nvemacs--sort-triangle)))))
  (force-mode-line-update))

(defun nvemacs--search ()
  "Run the current query in the current sort; the selection survives it."
  (let* ((id (nvemacs--selection))
         (free nvemacs--view-free)
         (starts (and free (mapcar (lambda (w) (cons w (line-number-at-pos (window-start w))))
                                   (get-buffer-window-list (current-buffer) nil t)))))
    (if (equal nvemacs--sort '(modified . desc))
        (progn (setq nvemacs--sort-keys nil nvemacs--rest nil nvemacs--list-stale nil)
               (nvemacs--search-newest))
      (nvemacs--search-sorted))
    (nvemacs--selection-restore id free starts)))

(defun nvemacs--search-newest ()
  "Run the current query, drawing the matching rows as they arrive.
Titles are searched first, then bodies in adaptive rowid slices.  The
search stops when the screen is full, the range is exhausted, or a key
is pressed."
  (setq nvemacs--rendered-size (nvemacs--window-size))
  (nvemacs--ensure-match-width)            ; the setting may have changed
  (let* ((db (nvemacs--db))
         (query (nvemacs--search-text))
         (parsed (nvemacs--parse-query query))
         (terms (nth 0 parsed))
         (inc (nth 1 parsed))
         (exc (nth 2 parsed))
         (filter (nvemacs--tag-filter inc exc))
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
            (nvemacs--landing-status inc exc)
            (funcall record (if (or inc exc) "tags" "newest") (if kept "kept list" "listed"))))
      (let* ((route (nvemacs--route terms))
             (match (nvemacs--term-where terms))
             (where (concat (car match) (car filter)))
             (args (append (cdr match) (cdr filter)))
             (top (or (caar (sqlite-select db "SELECT max(id) FROM meta")) 0))
             (bottom (or (caar (sqlite-select db "SELECT min(id) FROM meta")) 0))
             (span (max 1 (- top bottom)))
             (r (1+ top))
             (d (max 1 (/ span nvemacs--slice-start-fraction)))
             (stopped nil)
             ;; Title phase: small table, newest first.
             (titles (mapcar #'car (sqlite-select db (format "SELECT rowid FROM %s WHERE %s ORDER BY rowid DESC LIMIT ?"
                                                             (car route) where)
                                                  (append args (list want))))))
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
It reads N of TOTAL newest notes, or of the notes matching the tag
filters INC and EXC."
  (let ((filter (nvemacs--tag-filter inc exc)))
    (nvemacs--set-status
     (if (or inc exc)
         (concat " %s of %s notes with "
                 (mapconcat #'identity (append (mapcar (lambda (v) (concat "tag:" v)) inc)
                                               (mapcar (lambda (v) (concat "-tag:" v)) exc))
                            " ")
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
  (pcase-let ((`(,terms ,inc ,exc) (nvemacs--parse-query (nvemacs--search-text))))
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
    ('title "title_key") ('tags "tags_key") ('created "created") ('added "added") (_ "id")))

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
      (let* ((route (nvemacs--route terms))
             (table (if titles (car route) (cdr route)))
             (match (nvemacs--term-where terms))
             (where (concat (car match) (car filter)))
             (args (append (cdr match) (cdr filter)))
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
         (filter (nvemacs--tag-filter inc exc))
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
  (pcase-let* ((query (nvemacs--search-text))
               (`(,terms ,inc ,exc) (nvemacs--parse-query query))
               (want (nvemacs--screen-rows))
               (before (length nvemacs--shown))
               (t0 (float-time))
               (pairs (nvemacs--match-pairs terms (nvemacs--tag-filter inc exc)
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
  "Clear the query."
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
  :type 'number)

(defcustom nvemacs-notelist-min-width 50
  "Narrowest useful result list, in columns (side-by-side arrangement)."
  :type 'integer)

(defcustom nvemacs-notecontent-min-width 80
  "Narrowest useful note pane, in columns (`fill-column' + 10)."
  :type 'integer)

(defcustom nvemacs-notelist-min-lines 10
  "Shortest useful result list, in lines (stacked arrangement)."
  :type 'integer)

(defcustom nvemacs-notecontent-min-lines 20
  "Shortest useful note pane, in lines (stacked arrangement)."
  :type 'integer)

(defcustom nvemacs-other-window-action 'pane
  "What `nvemacs-note-open-other-window' does.
\\<nvemacs-mode-map>In the notelist that is \\[nvemacs-note-open-other-window].
`pane' opens the note in the note pane and moves the cursor there (in
the list-only arrangement it opens a temporary stacked pane; with a
prefix argument the cursor stays in the list).  `open' acts like
\\[nvemacs-note-open]."
  :type '(choice (const pane) (const open)))

(defcustom nvemacs-return-layout 'rebuild
  "What `nvemacs-searchbar-return' does after the frame is rearranged.
`rebuild' restores the nvEmacs arrangement of windows; `respect' leaves
your windows as they are and only shows the list."
  :type '(choice (const rebuild) (const respect)))

(defconst nvemacs--placeholder-name "*nvEmacs: no note*")
(defconst nvemacs--homepage-name "*nvEmacs home*")
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
  :type '(choice (const stacked) (const side)))

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

(define-minor-mode nvemacs-note-mode
  "Buffer-local mode in the notes nvEmacs shows.
\\<nvemacs-note-mode-map>\\[nvemacs-searchbar-return] returns to the list, \\[nvemacs-quit] exits nvEmacs.
nvEmacs turns the mode off in every buffer when it exits."
  :lighter " nv"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c )") #'nvemacs-searchbar-return)
            (define-key map (kbd "C-c C-]") #'nvemacs-quit) ; Org leaves it free
            map)
  (if nvemacs-note-mode
      (add-hook 'kill-buffer-hook #'nvemacs--note-killed nil t)
    (remove-hook 'kill-buffer-hook #'nvemacs--note-killed t)))

(defun nvemacs-note-open ()
  "Open the selected note, with the cursor in it.
The note opens in the note pane or, in the list-only arrangement, takes
over the frame.  On an empty search line, start an empty note; when
nothing matches, create a note named after the search."
  (interactive)
  (let ((path (nvemacs--target-path)))
    (cond
     ((and (nvemacs--on-search-line-p) (not nvemacs--selected-title)
           (string-empty-p (string-trim (nvemacs--query))))
      (nvemacs-note-new ""))
     ((and (null path) (null nvemacs--shown))
      (nvemacs-note-new (string-trim (nvemacs--search-text))))
     ((null path)
      (message "%s" (nvemacs--command-text 'nvemacs-note-open "requires a note's row"
                                           "Move to a row and then re-run the command")))
     (t (nvemacs--open-path path)))))

(defun nvemacs--open-path (path)
  "Show PATH in the note pane with the cursor in it, or take the frame."
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

(defun nvemacs-searchbar-return ()
  "Return to nvEmacs from anywhere.
Suggested global key: F8; `C-c )' in notes nvEmacs opened."
  (interactive)
  (cond
   ((nvemacs--restore-l1-p)
    (set-window-configuration (car (nvemacs--fparam 'nvemacs-l1)))
    (nvemacs--set-fparam 'nvemacs-l1 nil)
    (nvemacs--sweep)
    (nvemacs--focus-query))
   ((nvemacs--fparam 'nvemacs-active)
    (when (nvemacs--fparam 'nvemacs-temp)        ; C-o's temporary pane collapses
      (nvemacs--set-fparam 'nvemacs-temp nil)
      (nvemacs--apply-layout))
    (if (eq nvemacs-return-layout 'rebuild)
        (unless (nvemacs--layout-intact-p) (nvemacs--apply-layout))
      (unless (nvemacs--list-window) (switch-to-buffer nvemacs--buffer-name)))
    (nvemacs--focus-query))
   (t (nvemacs))))

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
  (nvemacs--grow-disable)
  (nvemacs--widths-save)
  (nvemacs--session-end)
  ;; Helper lists show nvEmacs state; stale once it is gone. The report stays: it may be kept to compare.
  (dolist (f (frame-list)) (set-frame-parameter f 'nvemacs-homepage-up nil))
  (dolist (b (list "*nvEmacs tags*" "*nvEmacs skipped*" nvemacs--homepage-name))
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
  :type '(choice (const nil) directory))

(defconst nvemacs--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory nvemacs.el was loaded from.")

(defcustom nvemacs-template-directory
  (expand-file-name "templates/" nvemacs--directory)
  "Directory of the template files for new notes.
The default is the templates shipped with nvEmacs (note.org, note.md,
note.txt).  Keep your own outside the watched directories, or in a
hidden subdirectory, so they are not indexed as notes."
  :type 'directory)

(defcustom nvemacs-new-note-extension "org"
  "Extension of blank new notes.
A blank note is one whose naming style names no template file."
  :type 'string)

(defcustom nvemacs-new-note-naming-styles
  '((diary :filename "{time:%Y-%m-%d-%Hh%Mm}-{title}" :template "note.org")
    (topic :filename "{title}" :template "note.org"))
  "Naming styles: NAME :filename PATTERN [:directory D] [:template F].
The template file (in `nvemacs-template-directory') gives the new note its
text and extension; a style without :template starts a blank note with
`nvemacs-new-note-extension'.  Placeholders: {title}, {time:FORMAT};
templates also {path} and {author} (the variable `user-full-name')."
  :type '(alist :key-type symbol :value-type plist))

(defcustom nvemacs-new-note-naming-style-default 'diary
  "Style used by \\<nvemacs-mode-map>\\[nvemacs-note-open] and \\[nvemacs-note-new]."
  :type 'symbol)

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
       (format "requires a note type in a watched folder, and %s is not" path)
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
        (message "%s" (nvemacs--command-text
                       'nvemacs-note-new
                       (format "requires the template %s, which was not found, so the note starts blank"
                               missing)
                       "Set nvemacs-template-directory to the folder that holds it"))))))

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
  "Remember this note as it was; run by `first-change-hook'."
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
       'nvemacs-edits-use (format "requires a note in a watched folder, and %s is not" path)
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
An existing line keeps its keyword spelling; further lines merge into it.
Otherwise the line goes between `#+Weather:' and `#+Notes:',
else at the end of the leading keyword block, else at the top
after a -*- line.  No tags leaves an empty line."
  (let* ((case-fold-search t)
         (value (if tags (concat ":" (mapconcat #'identity tags ":") ":") ""))
         (limit (copy-marker (nvemacs--org-first-heading))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+filetags:\\([ \t]*\\)" limit t)
          (let ((ws (if (string-empty-p (match-string 1)) " " (match-string 1))))
            (delete-region (match-beginning 1) (line-end-position))
            (insert ws value)
            (forward-line 1)
            (while (re-search-forward "^#\\+filetags:.*\n?" limit t)
              (replace-match "")))
        (goto-char (nvemacs--org-filetags-place limit))
        (unless (bolp) (insert "\n"))
        (insert "#+filetags: " value "\n")))
    (set-marker limit nil)))

(defun nvemacs--org-filetags-place (limit)
  "Where a new `#+filetags:' line goes (see `nvemacs--org-set-filetags').
Search no further than LIMIT."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward "^#\\+weather:.*$" limit t) (forward-line 1) (point))
       ((progn (goto-char (point-min)) (re-search-forward "^#\\+notes:" limit t))
        (line-beginning-position))
       (t
        (goto-char (point-min))
        (when (looking-at ".*-\\*-.*-\\*-") (forward-line 1))
        (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" limit t) (forward-line 1)))
        (while (and (< (point) limit) (looking-at "#\\(?: \\)?\\+[A-Za-z_]+:"))
          (forward-line 1))
        (point))))))

(defun nvemacs--yaml-item (tag)
  "Return TAG as a YAML scalar.
It is quoted when its first character is YAML syntax (`#' would start
a comment)."
  (if (string-match-p "\\`[@#%`!&*|>'\"]" tag) (format "\"%s\"" tag) tag))

(defun nvemacs--md-set-tags (tags)
  "Set the front-matter tags of the Markdown text in this buffer to TAGS.
The form found is kept (flow, block, comma or space string, TOML); a
missing key is added as `tags: [a, b]'.  No front matter is created."
  (let* ((fm (nvemacs--md-front-matter))
         (e (and fm (nvemacs--md-tags-entry fm)))
         (flow (concat "[" (mapconcat #'nvemacs--yaml-item tags ", ") "]"))
         (toml (concat "[" (mapconcat (lambda (tg) (format "\"%s\"" tg)) tags ", ") "]")))
    (save-excursion
      (cond
       ((null fm)
        (when tags (goto-char (point-min)) (insert "---\ntags: " flow "\n---\n")))
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

;;;###autoload
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
;; when nvEmacs exits and shown in the note pane at launch.  Org pages are
;; rendered by Org, loaded only after the list is on screen (about 200 ms).
;; Markdown pages use a small viewer: markdown-mode is not part of Emacs.
;; What a crash could make stale (edits to recover) is worked out at launch
;; and shown above the file, never stored in it.

(defconst nvemacs-version (package-get-version) "The version of nvEmacs.")

(defcustom nvemacs-homepage-file nil
  "File of the live home page.
nil means homepage.EXT in `nvemacs-data-directory', with the extension
of `nvemacs-homepage-template'.  nvEmacs overwrites this file: to change
the page, change the template.  Keep it out of the watched directories,
or it is indexed as a note."
  :type '(choice (const :tag "In the data directory" nil) file))

(defcustom nvemacs-homepage-template "homepage.org"
  "Template of the home page, a file in `nvemacs-template-directory'.
nvEmacs ships homepage.org and homepage.md.  A name not found there is
looked up among the shipped templates."
  :type 'string)

(defcustom nvemacs-homepage-show-at-start t
  "Non-nil means show the home page when nvEmacs starts.
\\[nvemacs-homepage-show] shows it at any time."
  :type 'boolean)

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
  "Write the home page file from its template; return the file, or nil."
  (let ((tpl (nvemacs--homepage-template-file)) (file (nvemacs--homepage-file)))
    (when tpl
      (let ((text (nvemacs--homepage-expand
                   (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8)) (insert-file-contents tpl))
                     (buffer-string))
                   (nvemacs--homepage-values (nvemacs--homepage-md-p)))))
        (nvemacs--private-directory (file-name-directory file))
        (with-temp-buffer (insert text) (nvemacs--write-file file))
        file))))

(defun nvemacs--homepage-save ()
  "Write the home page if the index is open.  An error never stops an exit."
  (when nvemacs--db
    (with-demoted-errors "nvEmacs: writing the home page: %S"
      (nvemacs--homepage-write))))

(defun nvemacs--homepage-text ()
  "Return the home page's text, first writing it if missing or out of date."
  (let ((file (nvemacs--homepage-file)) (tpl (nvemacs--homepage-template-file)))
    (when (and tpl (or (not (file-exists-p file)) (file-newer-than-file-p tpl file)))
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
         (attention
          (append
           (when (> pending 0)
             (list (concat (funcall plural pending "note has" "notes have")
                           " unsaved edits: M-x nvemacs-edits-recover")))
           (when nvemacs--skipped
             (list (concat (funcall plural (length nvemacs--skipped)
                                    "note could not be indexed" "notes could not be indexed")
                           ": M-x nvemacs-index-skipped")))
           (mapcar (lambda (d) (concat "Watched folder not found: " d))
                   (cl-remove-if #'file-directory-p nvemacs-watched-directories))
           (when (> orphans 0)
             (list (concat (funcall plural orphans "tag file has no note" "tag files have no note")
                           ": M-x nvemacs-report-index"))))))
    (concat
     (when (or (null nvemacs-watched-directories) (zerop (nvemacs--note-count)))
       (concat h "Getting started\n\n"
               "- Point nvEmacs at your notes: M-x customize-variable RET"
               " nvemacs-watched-directories\n\n"))
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
  (if (and (file-regular-p path) (nvemacs--watched-note-p path))
      (nvemacs--open-path path)
    (message "%s" (nvemacs--command-text
                   'nvemacs-homepage-open-link
                   (format "requires a note in a watched folder, and %s is not" path)
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

(defun nvemacs ()
  "Start nvEmacs in this frame: take it over and show the newest notes.
Each frame keeps its own arrangement; the search buffer is shared.
With `nvemacs-instrumentation' on, a start that builds the list is
measured and logged (startup.jsonl)."
  (interactive)
  (nvemacs--require-sqlite)              ; before anything is created
  (let* ((t0 (float-time))
         (call-time (current-time))
         (rebuilt (unless (and (file-exists-p (nvemacs--index-path)) (nvemacs--index-current-p))
                    (message "nvEmacs: building the index…")
                    (nvemacs-index-rebuild)
                    t))
         (index-ms (progn (nvemacs--ensure-match-width) (nvemacs--ms t0)))
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
