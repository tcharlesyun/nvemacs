nvEmacs
=======

Notational Velocity-style note search for Emacs.

Type in one line; the notes that match appear underneath as you type,
newest first; the selected note shows beside or below the list.  Press RET
to open it, or, when nothing matches, to create a note named after what
you typed.  Notes are ordinary files (Org, Markdown, plain text) in folders
you choose.  nvEmacs is pure Emacs Lisp; the search index is a SQLite FTS5
trigram database that Emacs opens directly.

M-x nvemacs-version shows the version you are running.

  USAGE.txt            how to search, with examples
  COMMANDS.txt         keys and M-x commands
  SETTINGS.txt         every setting, with its default
  LICENSE.txt          the GNU General Public License, version 3
  ACKNOWLEDGEMENTS.txt the projects nvEmacs builds on
  LLM.txt              how nvEmacs was made


Requirements
------------

- Emacs 29.1 or later, built with SQLite support: (sqlite-available-p)
  must return t.  Without it, M-x nvemacs stops with a message saying so.
  SQLite must have the FTS5 trigram tokenizer (3.34+).
- Developed and tested on GNU Emacs 31.1 (macOS), SQLite 3.54.1.  Earlier
  Emacs versions are not tested yet.
- No other packages or servers.  To read a file's creation date for the
  Added column, nvEmacs runs the system's stat program on macOS, BSD and
  Linux.  Without it, Added uses the file's modified date.


Installing
----------

nvEmacs is one file, nvemacs.el, with the templates/ folder beside it.

Emacs 30 or later: put the following in your init file
(~/.emacs.d/init.el).  The first time Emacs starts with it, use-package
downloads nvEmacs from GitHub and installs it in ~/.emacs.d/elpa/.  Later
starts use that copy and do not go online.

    (use-package nvemacs
      :vc (:url "https://github.com/tcharlesyun/nvemacs")
      :bind ("<f8>" . nvemacs-searchbar-return))     ; suggested

Emacs 29: M-x package-vc-install RET https://github.com/tcharlesyun/nvemacs
RET, then in your init file:

    (require 'nvemacs)
    (global-set-key [f8] #'nvemacs-searchbar-return)   ; suggested

By hand: download or clone the repository, and use the lines above with

    (add-to-list 'load-path "/path/to/nvemacs")

before (require 'nvemacs).

Watched folders.  The first time you run M-x nvemacs, it asks which
folders hold your notes and keeps them in settings.eld, in its data folder.
M-x nvemacs-index-folder-add and nvemacs-index-folder-remove change them
later.  You may instead set nvemacs-watched-directories in your init file:
that list is then used, and the two commands say to change it there.

Loading.  Only M-x nvemacs loads nvEmacs by itself.  Before any other
nvEmacs command is bound to a key, nvEmacs must be loaded: (require
'nvemacs) does it, and use-package's :bind arranges it.  Without either,
F8 pressed before the first M-x nvemacs only says "Symbol's function
definition is void: nvemacs-searchbar-return".

Compiling.  use-package :vc and package-vc-install compile nvEmacs when
they install it.  Installed by hand, compile it once, and again after each
update -- uncompiled, the list redraws about a fifth slower:

    M-x byte-compile-file RET /path/to/nvemacs/nvemacs.el RET

Emacs then also compiles it to native code in the background, if it was
built with native compilation.  (setq load-prefer-newer t) in your init
file keeps Emacs from loading an old nvemacs.elc after an update.


Getting started
---------------

M-x nvemacs.  The first start asks for your notes folders, then builds
the index (a few seconds per few thousand notes); later starts open it at
once and bring it up to date in the background.

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
tags and dates in columns.  The columns and their order are a setting;
the row above the search line names them, and the mode line shows the
status ("20 of 4,782 Newest notes displayed", "12 shown · screen full ·
6 ms").

With an empty search the list shows the newest notes and, while the
cursor is in the notelist, grows by a screenful each half second you pause,
until every note is listed.  Once complete it is kept: clearing a later
search puts every row back at once.

Resting the cursor on a row shows that note in the pane (a preview).  A
preview sets only the note's file-local variables (a "Local Variables:"
block or a -*- line) that Emacs knows to be safe, and asks nothing.  The
others are set, with Emacs's usual question, when the cursor moves into
the note.

C-c C-] exits nvEmacs, from the list or a note.  The notes it opened are
written if you changed them and then closed, the frame gets back its earlier
windows, and nvEmacs stops watching your folders.  F8 (or M-x nvemacs) starts
it again.  To keep it running behind other work, switch buffers as usual
(C-x b).  A note you already had open when nvEmacs showed it stays open,
and saving it is left to you; nvEmacs picks up the change the next time
you return to the list.


How edits are saved
-------------------

An accidental edit that you undo leaves a note exactly as it was: same
text, same date, same place in the list.

- While you edit a note nvEmacs opened, your edits are copied to a
  private temp file (after 3 s idle, and at least every 30 s).  The note's
  own file is not touched.
- C-x C-s writes the note if its text changed.  If you undid your edits,
  there is nothing to write, and the file and its date stay as they were.
- Leaving the note writes it, if its text changed: moving the cursor to
  another window or buffer, or exiting nvEmacs.  A prompt such as M-x, or
  switching to another application, is not leaving.
- If you come back and undo back to the original text, the original date
  returns too, unless you saved with C-x C-s in between.
- Notes you edited stay open until nvEmacs exits, so undo keeps working
  as you move between notes.
- C-x k and quitting Emacs ask about unsaved edits as usual.  Answering
  that you don't want them, or M-x revert-buffer, discards them.  A
  discarded edit is kept for 7 days in discarded/ in the data directory.
- If the note was changed or deleted elsewhere while you were editing,
  nvEmacs does not overwrite it.  It keeps your edits and tells you: C-x
  C-s writes them, M-x revert-buffer discards them.
- After a crash or forced quit, the next start tells you if edits were
  left behind.  M-x nvemacs-edits-recover lists them; for each you can
  compare it with the note (d), use it (RET) or discard it (x).


Renaming, moving and deleting notes
-----------------------------------

With the cursor on a note's row:

- C-c C-r renames its file.  The text and date stay, so its place in the
  list does too.  A name with no extension keeps the current one.  A new
  extension must be org, md or txt, and nvEmacs asks first.  It also says
  if the note's tags would stop being read, because each type keeps tags
  in a different place.  Renaming back brings them back.
- C-c C-w moves it to another watched folder.
- C-c C-x C-a moves it to a folder nvEmacs does not watch.  It asks first,
  because the note will no longer be found.
- M-x nvemacs-note-delete moves it to the system Trash, after asking.
  Unsaved edits are kept in discarded/ for 7 days.

The note's .tags file goes with it, and edits waiting for it are written
first.  A note with unsaved edits in a buffer you opened yourself must be
saved or reverted first.


The home page
-------------

When nvEmacs starts, the notecontent area shows a home page: your notes
and watched folders, recently edited notes, the index's size and timings,
versions, search tips, and every command with its default and current
keys.  Anything that needs attention, such as unsaved edits to recover, is
at the top.  RET on a note's title opens it.

The page stays while you type a search, resize the window or move it to
another screen.  Selecting or opening a note replaces it; after that the
area says "No note selected".  M-x nvemacs-homepage-show brings it back,
up to date.

The page is a plain Org file, rendered by Org and rewritten when nvEmacs
exits or first starts at a new version, so change the template rather than
the page: copy templates/homepage.org to your nvemacs-template-directory
and edit it.  For Markdown, use homepage.md:

    (setq nvemacs-homepage-template "homepage.md")


Searching
---------

- Type words: a note matches when it contains every word, anywhere, as
  part of a word ("whal" finds "whale").  Case and accents are ignored
  ("eclair" finds "Éclair").  Korean, Chinese and Japanese work, including
  one-character words.  Apart from case and accents, each character
  matches only itself: my_note finds "my_note", not "my-note"; 100%
  finds "100%", not "1000".
- In the default sort, title matches come first, then the other matches,
  each group newest first.
- tag:work lists notes with a tag that starts with "work" (so the list
  narrows as you type the tag); -tag:old leaves out notes with a tag
  starting with "old".  They combine with words and with each other.
- The search runs 0.1 s after you stop typing, so the first rows appear
  in milliseconds.  More rows load when Emacs is idle, and at once when
  the cursor nears the last row.


Sorting and columns
-------------------

C-c ^ asks for a column, with one key:

    Sort by: a) title  b) tags  c) created  d) added  e) modified ▼  ^) reverse

Choosing the sorted column again, or ^, reverses it.  So does clicking its
heading, or running its command again (M-x nvemacs-notelist-sort-title,
-tags, -created, -added, -modified).  The sorted column's heading shows ▲
for A to Z or oldest first, ▼ for Z to A or newest first.  Title and Tags
start at ▲, the dates at ▼.  The sort is remembered between sessions.

- Blank values stay last in both directions; ties put the newest first.
- Each note's tags are alphabetized and compared one at a time, ignoring
  case.  While sorted by Tags, rows show them alphabetized; the note does
  not change.
- The cursor stays on the selected note, and a saved note moves to its
  sorted place.

C-c C-x C-c shows or hides columns for this session: each key toggles one,
RET ends, C-g puts them back.

    Columns: b) tags ✓  c) created  d) added  e) modified ✓   RET) done

The columns at launch come from nvemacs-notelist-columns, by default
title, tags, modified.  The title is always shown.  You can still sort on a
hidden column, and the mode line names it.


The selected note and the view
------------------------------

This differs from Emacs's default.  In most Emacs lists, the line with
the cursor is the selected item, and Emacs always keeps the cursor on
screen, so scrolling a list changes what is selected.

nvEmacs splits the notelist into two things:

- The selection is the note you chose.  It is highlighted, its title is
  on the search line, and it is the note in the note pane.
- The view is the part of the list on screen.

What moves which:

    Scroll bar, mouse wheel, trackpad    the view only
    Up, Down, C-n, C-p, TAB, S-TAB       the selection
    C-v, M-v                             the selection
    Clicking a row                       the selection
    Typing                               starts a new search, which
                                         selects nothing

So you can keep a note open and scroll the list to read other titles.

When the selected row is off screen:

- No row is highlighted and the cursor is hidden, so nothing on screen
  looks selected.
- RET, C-o, rename, move, delete and tags still act on the selected
  note, even if it is not visible in the buffer.
- C-x o into the list leaves the list where you scrolled it.
- A keyboard move starts from the selected note: Down selects the row
  below it, and the list jumps there.
- Scrolling back to the selected row puts the cursor on it again.
- Sorting returns the list to the selected note.

With no note selected, scrolling selects nothing, and a keyboard move
starts from the rows on screen.

The search line stays on screen while you scroll.  Once the list moves
past it, a read-only copy holds its place.  Typing, or clicking the copy,
goes to the real search line and brings the list back to the top.


Notes, titles and dates
-----------------------

- Notes are files with the extensions in nvemacs-import-extensions (org,
  md, txt) under the watched folders and their subfolders.  Hidden
  folders are skipped.  Notes over 10 MB, binary files and files that are
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
- The Created date comes from the note: #+created:, a :CREATED:
  property, #+date: (Org); created: or date: (Markdown); a date at the
  start of the filename.  A note with none shows its Added date with a ~,
  as ~2026-11-02.
- The Added date is the earlier of the file's creation and modified dates
  when nvEmacs first finds it.  It never moves later, and it follows the
  note through renames and moves.  Show these columns at launch with
  (setq nvemacs-notelist-columns "title, tags, created, added, modified").
- New notes are named yyyy-mm-dd-HHhMMm-Title.org by default and start
  from a template, or blank: see "New notes and templates" below.


New notes and templates
-----------------------

A new note is made by RET when nothing matches (titled with what you
typed), RET on an empty search line (untitled), or C-c C-n.  nvEmacs opens
it in the note pane; nothing is written to disk until you save it or
leave it with something typed.  A new note you close as it started --
nothing typed, or the typing undone -- is not kept.  If it had already
been written by leaving it, it is moved to the system Trash (it can be
put back from there) and leaves the notelist.  A new note you saved with
C-x C-s is always kept.  Only the file nvEmacs created for that note is
ever moved this way.

How the note is named and what it starts with is set by a naming style.
The default:

    (setq nvemacs-new-note-naming-styles
          '((diary :filename "{time:%Y-%m-%d-%Hh%Mm}-{title}" :template "note.org")
            (topic :filename "{title}" :template "note.org"))
          nvemacs-new-note-naming-style-default 'diary)

- :filename is the file name without extension; the title has spaces and
  punctuation turned into _.  With no title it is just the time stamp.
- :template names a file in nvemacs-template-directory.  The new note
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

templates/note.txt is one line, "type here".  It is a known-good
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
tags: [a, b]).  The other #+ lines and the whole front matter are not
indexed, so "add a hidden note here" is never a search hit.

Making your own templates

1.  Make a folder for them, outside your watched folders or in a hidden
   subfolder of one (.templates/), so templates are never listed as
   notes.  Copy any of the shipped templates there to start from.
2.  Write each template as an ordinary note in its target format, with
   placeholders where values go.  The file extension decides the note
   type, and must be one of nvemacs-import-extensions (org, md, txt by
   default) so the new note is indexed.
3.  Point nvEmacs at the folder and name the template in a style:

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

- Org: #+filetags: :a:b: and heading tags.  Markdown: tags: [a, b] in the
  front matter (other YAML/TOML forms are read too).  Other notes: a
  sister file, foo.txt.tags, one tag per line ("# " starts a comment).
- Tags keep their spelling and are matched without regard to case;
  spaces and punctuation become _.
- C-c C-q in the list (or M-x nvemacs-tags-edit) edits the selected
  note's tags in the form the note already uses; M-x nvemacs-tags-list
  lists every tag with a count.
- A tag edit is an edit of the note: it is written when you leave the
  note or, from the list, when the cursor moves off its row.  Adding a
  tag and deleting it again leaves the note as it was.


What nvEmacs writes, and where
------------------------------

Into nvemacs-data-directory (default nvemacs/ in your Emacs directory,
usually ~/.emacs.d/nvemacs/).  nvEmacs creates it readable by you only,
and every file in it too: the index holds the text of every note.

  index.sqlite    the search index.  A cache: delete it any time and the
                  next start rebuilds it from the notes.
  dates.eld       the Added date of every note.  Plain text.
                  Keep it: it cannot be rebuilt.  It may live elsewhere
                  (e.g. Dropbox) via nvemacs-dates-file.
  settings.eld    the choices you make in nvEmacs: the watched folders,
                  a tags column width you set, comment search and search
                  history on or off.  Keep it.
  state.eld       remembered values (the tags column width from the last
                  session, the sort).  Safe to delete.
  history.jsonl   your recent searches, while search history is on.
  malformed.org   the notes whose header needs attention.
  pending/        temp files holding the text of edits not yet written
                  to their notes.  Each is removed once its note is
                  written.
  discarded/      edits you chose to discard, kept for 7 days.
  homepage.org    the home page, rewritten at exit and after an upgrade.
                  It lists note titles and paths.  Move it with
                  nvemacs-homepage-file.
  session-times.eld  index and notelist timings of the last 20 sessions.
  nvemacs-log/    small measurement logs (startup, sessions, syncs), each
                  capped at 10 KB: counts and timings only, never note
                  text, searches or file names.  M-x nvemacs-report-all
                  shows them; M-x nvemacs-instrumentation turns them off.

Your notes are written only when you save one, when you leave a note
nvEmacs opened after changing its text or tags, and when nvEmacs exits
(see "How edits are saved").  A new note closed untouched is moved to the
system Trash, never deleted outright.  While a note has unwritten edits,
Emacs keeps its usual .#name lock link beside it.  nvEmacs never writes
your init file or custom-file: a setting there wins over the choices
nvEmacs keeps in settings.eld.


Not built yet
-------------

- The naming-style picker.
- One search per frame (all frames share the one *nvEmacs* buffer).
- Section-only tag search, renaming a tag across notes.


License
-------

Copyright (C) 2026 T. Charles Yun

nvEmacs is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.  It is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LICENSE.txt for
the full license.

No code from Notational Velocity (GPLv3) or Deft (BSD 3-clause) is
included; both inspired the design.  See ACKNOWLEDGEMENTS.txt.
