nvEmacs 0.0.2
=============

Notational Velocity-style note search for Emacs.

Type in one line; the notes that match appear underneath as you type,
newest first; the selected note shows beside or below the list. Press RET
to open it, or, when nothing matches, to create a note named after what
you typed. Notes are ordinary files (Org, Markdown, plain text) in folders
you choose. nvEmacs is pure Emacs Lisp; the search index is a SQLite FTS5
trigram database that Emacs opens directly.

Version 0.0.2 (2026-11-21) is the first public version.

  COMMANDS.txt         keys and M-x commands
  SETTINGS.txt         every setting, with its default
  LICENSE.txt          the GNU General Public License, version 3
  ACKNOWLEDGEMENTS.txt the projects nvEmacs builds on
  LLM.txt              how nvEmacs was made


Requirements
------------

- Emacs 29.1 or later, built with SQLite support: (sqlite-available-p)
  must return t. Without it, M-x nvemacs stops with a message saying so.
  SQLite must have the FTS5 trigram tokenizer (3.34+).
- Developed and tested on GNU Emacs 31.1 (macOS), SQLite 3.54.1. Earlier
  Emacs versions are not tested yet.
- Nothing else: no external programs, packages or servers.


Installing
----------

nvEmacs is one file, nvemacs.el, with the templates/ folder beside it.

Emacs 30 or later, straight from GitHub (use-package fetches it):

    (use-package nvemacs
      :vc (:url "https://github.com/tcharlesyun/nvemacs")
      :custom (nvemacs-watched-directories '("~/notes/"))
      :bind ("<f8>" . nvemacs-searchbar-return))     ; suggested

Emacs 29: M-x package-vc-install RET https://github.com/tcharlesyun/nvemacs
RET, then in your init file:

    (require 'nvemacs)
    (setq nvemacs-watched-directories '("~/notes/"))
    (global-set-key [f8] #'nvemacs-searchbar-return)   ; suggested

By hand: download or clone the repository, and use the lines above with

    (add-to-list 'load-path "/path/to/nvemacs")

before (require 'nvemacs).

Loading. Only M-x nvemacs loads nvEmacs by itself. Before any other
nvEmacs command is bound to a key, nvEmacs must be loaded: (require
'nvemacs) does it, and use-package's :bind arranges it. Without either,
F8 pressed before the first M-x nvemacs only says "Symbol's function
definition is void: nvemacs-searchbar-return".

Compiling. use-package :vc and package-vc-install compile nvEmacs when
they install it. Installed by hand, compile it once, and again after each
update -- uncompiled, the list redraws about a fifth slower:

    M-x byte-compile-file RET /path/to/nvemacs/nvemacs.el RET

Emacs then also compiles it to native code in the background, if it was
built with native compilation. (setq load-prefer-newer t) in your init
file keeps Emacs from loading an old nvemacs.elc after an update.


Getting started
---------------

M-x nvemacs. The first start builds the index (a few seconds per few
thousand notes); later starts open it at once and bring it up to date in
the background.

To try nvEmacs without touching your own notes, point it at a folder of
copies, and keep its files apart from a later real setup:

    (setq nvemacs-watched-directories '("~/nvemacs-try/notes/")
          nvemacs-data-directory "~/nvemacs-try/data/")

"What nvEmacs writes, and where" below lists everything it writes.


The screen
----------

nvEmacs uses Notational Velocity's three areas:

  searchbar     the top line of *nvEmacs*: what you type
  notelist      the rows below it: the matching notes
  notecontent   the note pane: the selected note, read and edited in
                place (beside or below the list, chosen by frame size;
                C-c C-v cycles the arrangement)

Each notelist row shows the title (its first 60 columns in bold), the
rest of a long title and then the note's text as one grey line, then
tags and dates in columns. The columns and their order are a setting;
the header line names them, and the mode line shows the status ("20 of
4,782 Newest notes displayed", "12 shown · screen full · 6 ms").

With an empty search the list shows the newest notes and, while the
cursor is in the notelist, grows by a screenful each half second you pause,
until every note is listed. Once complete it is kept: clearing a later
search puts every row back at once.

Resting the cursor on a row shows that note in the pane (a preview). A
preview sets only the note's file-local variables (a "Local Variables:"
block or a -*- line) that Emacs knows to be safe, and asks nothing. The
others are set, with Emacs's usual question, when the cursor moves into
the note.

C-c C-q exits nvEmacs. The notes it opened are written if you changed
them and then closed, the frame gets back its earlier windows, and
nvEmacs stops watching your folders. F8 (or M-x nvemacs) starts it
again. To keep it running behind other work, switch buffers as usual
(C-x b). A note you already had open when nvEmacs showed it stays open,
and saving it is left to you; nvEmacs picks up the change the next time
you return to the list.


How edits are saved
-------------------

An accidental edit that you undo leaves a note exactly as it was: same
text, same date, same place in the list.

- While you edit a note nvEmacs opened, your edits are copied to a
  private temp file (after 3 s idle, and at least every 30 s). The note's
  own file is not touched.
- C-x C-s writes the note, always, even if you undid your edits.
- Leaving the note writes it, if its text changed: moving the cursor to
  another window or buffer, or exiting nvEmacs. A prompt such as M-x, or
  switching to another application, is not leaving.
- If you come back and undo back to the original text, the original date
  returns too, unless you saved with C-x C-s in between.
- Notes you edited stay open until nvEmacs exits, so undo keeps working
  as you move between notes.
- C-x k and quitting Emacs ask about unsaved edits as usual. Answering
  that you don't want them, or M-x revert-buffer, discards them. A
  discarded edit is kept for 7 days in discarded/ in the data directory.
- If the note was changed or deleted elsewhere while you were editing,
  nvEmacs does not overwrite it. It keeps your edits and tells you: C-x
  C-s writes them, M-x revert-buffer discards them.
- After a crash or forced quit, the next start tells you if edits were
  left behind. M-x nvemacs-edits-recover lists them; for each you can
  compare it with the note (d), use it (RET) or discard it (x).


Searching
---------

- Type words: a note matches when it contains every word, anywhere, as
  part of a word ("whal" finds "whale"). Case and accents are ignored
  ("eclair" finds "Éclair"). Korean, Chinese and Japanese work, including
  one-character words. Apart from case and accents, each character
  matches only itself: my_note finds "my_note", not "my-note"; 100%
  finds "100%", not "1000".
- Title matches come first, then the rest, newest first.
- tag:work lists notes with a tag that starts with "work" (so the list
  narrows as you type the tag); -tag:old leaves out notes with a tag
  starting with "old". They combine with words and with each other.
- The search runs 0.1 s after you stop typing and stops as soon as the
  screen is full, so the first rows appear in milliseconds.


Notes, titles and dates
-----------------------

- Notes are files with the extensions in nvemacs-import-extensions (org,
  md, txt) under the watched folders and their subfolders. Hidden
  folders are skipped. Notes over 10 MB, binary files and files that are
  not UTF-8 are skipped and listed by M-x nvemacs-index-skipped.
- A note's title is the first of: #+title: (Org) or title: (Markdown
  front matter); the filename (a leading yyyy-mm-dd-HHhMMm- stamp
  removed, _ shown as spaces); the first heading, when the filename is
  only a date; the first line.
- A filename with _ can be searched as spelled, whatever the title:
  covington_drive and "covington drive" both find
  1827_Covington_Drive.org.
- Only the first 60 columns of a title count as a title match
  (nvemacs-title-match-width); the rest matches as ordinary text.
- The created date comes from the note: #+created:, a :CREATED:
  property, #+date: (Org); created: or date: (Markdown); a date at the
  start of the filename. A note with none gets the earliest modified time
  nvEmacs has seen for it, shown as ~2026-11-02. Show the column with
  (setq nvemacs-notelist-columns "title, tags, created, modified").
- New notes are named yyyy-mm-dd-HHhMMm-Title.org by default and start
  from a template, or blank: see "New notes and templates" below.


New notes and templates
-----------------------

A new note is made by RET when nothing matches (titled with what you
typed), RET on an empty search line (untitled), or C-c C-n. nvEmacs opens
it in the note pane; nothing is written to disk until you save it or
leave it with something typed. A new note you close as it started --
nothing typed, or the typing undone -- is not kept. If it had already
been written by leaving it, it is moved to the system Trash (it can be
put back from there) and leaves the notelist. A new note you saved with
C-x C-s is always kept. Only the file nvEmacs created for that note is
ever moved.

How the note is named and what it starts with is set by a naming style.
The default:

    (setq nvemacs-new-note-naming-styles
          '((diary :filename "{time:%Y-%m-%d-%Hh%Mm}-{title}" :template "note.org")
            (topic :filename "{title}" :template "note.org"))
          nvemacs-new-note-naming-style-default 'diary)

- :filename is the file name without extension; the title has spaces and
  punctuation turned into _. With no title it is just the time stamp.
- :template names a file in nvemacs-template-directory. The new note
  starts as a copy of it, with the placeholders filled in, and takes its
  extension (note.md makes .md notes).
- Leave :template out and the note starts as a blank buffer, with the
  extension nvemacs-new-note-extension ("org" by default).
- Optional :directory puts the note in a subfolder of the new-note folder.

nvEmacs uses only the default style for now (a picker is planned).

Shipped templates

nvemacs-template-directory defaults to the templates/ folder that comes
with nvEmacs, which holds three templates.

templates/note.org (the default):

    #+emacs:    <!-- ; -*-Org-*- -->
    #+Title:    {title}
    #+Author:   {author}
    #+Created:  {time:%Y-%m-%d-%Hh%Mm}
    #+STARTUP:  hidestars
    #+STARTUP:  odd
    #+OPTIONS:  padline:no
    #+OPTIONS:  toc:1
    #+filename: {path}
    #+Weather:
    #+filetags:
    #+Notes:    add a hidden note here.

templates/note.md (the same fields as Markdown front matter; the
STARTUP and OPTIONS lines are Org settings with no Markdown equivalent):

    ---
    title: {title}
    author: {author}
    created: {time:%Y-%m-%d-%Hh%Mm}
    filename: {path}
    weather:
    tags: []
    notes: add a hidden note here.
    ---

templates/note.txt is one line, "type here". It is a known-good
template: if your own template seems broken, point a style at note.txt
to check that note creation itself works.

Placeholders, filled when the note is made:

  {title}         the title (the search text; empty for an untitled note)
  {author}        your name, from the Emacs variable user-full-name
  {time:FORMAT}   the time, formatted by format-time-string; see
                  https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
  {path}          the new note's full file name

One clock reading fills both the file name and {time:...}, so they agree.

What nvEmacs reads back from these notes: the title (#+Title:, title:),
the created date (#+Created:, created:) and the tags (#+filetags:,
tags: [a, b]). The other #+ lines and the whole front matter are not
indexed, so "add a hidden note here" is never a search hit.

Making your own templates

1. Make a folder for them, outside your watched folders or in a hidden
   subfolder of one (.templates/), so templates are never listed as
   notes. Copy any of the shipped templates there to start from.
2. Write each template as an ordinary note in its target format, with
   placeholders where values go. The file extension decides the note
   type, and must be one of nvemacs-import-extensions (org, md, txt by
   default) so the new note is indexed.
3. Point nvEmacs at the folder and name the template in a style:

       (setq nvemacs-template-directory "~/notes/.templates/"
             nvemacs-new-note-naming-styles
             '((diary :filename "{time:%Y-%m-%d-%Hh%Mm}-{title}" :template "journal.md")
               (plain :filename "{title}")))     ; a blank note

What goes inside a template is defined by its format, not by nvEmacs:

- Org: in-buffer settings (#+TITLE, #+STARTUP, #+OPTIONS, #+FILETAGS,
  ...) are described in the Org manual, https://orgmode.org/manual/ —
  see "In-buffer Settings" and "Export Settings".
- Markdown: front matter is defined by the tool that reads it (static
  site generators, Pandoc, Obsidian, ...); see its manual, for example
  Pandoc's YAML metadata blocks,
  https://pandoc.org/MANUAL.html#extension-yaml_metadata_block.
- Any other format: see that format's own documentation.

If a style names a template that cannot be found, the note starts blank
and nvEmacs says which template it could not find.


Tags
----

- Org: #+filetags: :a:b: and heading tags. Markdown: tags: [a, b] in the
  front matter (other YAML/TOML forms are read too). Other notes: a
  sister file, foo.txt.tags, one tag per line ("# " starts a comment).
- Tags keep their spelling and are matched without regard to case;
  spaces and punctuation become _.
- M-x nvemacs-tags-edit edits the selected note's tags in the form the
  note already uses; M-x nvemacs-tags-list lists every tag with a count.
- A tag edit is an edit of the note: it is written when you leave the
  note or, from the list, when the cursor moves off its row. Adding a
  tag and deleting it again leaves the note as it was.


What nvEmacs writes, and where
------------------------------

Into nvemacs-data-directory (default nvemacs/ in your Emacs directory,
usually ~/.emacs.d/nvemacs/). nvEmacs creates it readable by you only,
and every file in it too: the index holds the text of every note.

  index.sqlite    the search index. A cache: delete it any time and the
                  next start rebuilds it from the notes.
  dates.eld       created dates of notes that declare none. Plain text.
                  Keep it: it cannot be rebuilt. It may live elsewhere
                  (e.g. Dropbox) via nvemacs-dates-file.
  state.eld       remembered values (the tags column width).
  pending/        temp files holding the text of edits not yet written
                  to their notes. Each is removed once its note is
                  written.
  discarded/      edits you chose to discard, kept for 7 days.
  nvemacs-log/    small measurement logs (startup, sessions, syncs), each
                  capped at 10 KB: counts and timings only, never note
                  text, searches or file names. M-x nvemacs-report-all
                  shows them; M-x nvemacs-instrumentation turns them off.

Your notes are written only when you save one, when you leave a note
nvEmacs opened after changing its text or tags, and when nvEmacs exits
(see "How edits are saved"). A new note closed untouched is moved to the
system Trash, never deleted outright. While a note has unwritten edits,
Emacs keeps its usual .#name lock link beside it. Preferences live in
your init file or custom-file, never in the data directory.


Not in 0.0.2
------------

- Sorting other than newest first; clicking a heading to sort.
- Renaming, moving and deleting notes from nvEmacs; the naming-style
  picker.
- One search per frame (all frames share the one *nvEmacs* buffer).
- Section-only tag search, renaming a tag across notes.


License
-------

Copyright (C) 2026 T. Charles Yun

nvEmacs is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version. It is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See LICENSE.txt for
the full license.

No code from Notational Velocity (GPLv3) or Deft (BSD 3-clause) is
included; both inspired the design. See ACKNOWLEDGEMENTS.txt.
