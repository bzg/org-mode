;;; org-index.el --- A personal index for org and beyond

;; Copyright (C) 2011-2015 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; Version: 3.2.0
;; Keywords: outlines index

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;;  Help to navigate org.  Mark and find your favorite org-headings easily:
;;  Create and update an index table of references and links.  This table is
;;  sorted by usage count, so that the builtin incremental occur presents
;;  often used entries first.
;;
;;  References are essentially small numbers (e.g. "R237" or "-455-"), as
;;  created by this package; they are well suited to be used outside of
;;  org (e.g. within folder names).  Links are normal org-mode links.
;;
;;
;; Setup:
;;
;;  - Add these lines to your .emacs (modify appropriately):
;;
;;    (require 'org-index)
;;    (org-index-default-keybindings)   ; optional
;;
;;  - Restart your Emacs to make these lines effective.
;;
;;  - Invoke `org-index', which will assist in creating your index
;;    table.  The variable org-index-id will be persisted within your
;;    customization file (typically .emacs).
;;
;;
;; Further reading:
;;
;;  See the documentation of `org-index', which can also be read
;;  by invoking `org-index' and choosing the help-command.
;;
;;
;; Updates:
;;
;;  The latest tested version of this file can always be found at:
;;
;;    http://orgmode.org/w/org-mode.git?p=org-mode.git;a=blob_plain;f=contrib/lisp/org-index.el;hb=HEAD

;;; Change Log:

;;   [2015-01-31 Sa] Version 3.2.0:
;;   - Complete sorting of index only occurs in idle-timer
;;   - Command "add" now updates index, if node is already present
;;   - New command "maintain"  with some subcommands
;;
;;   [2015-01-20 Mo] Version 3.1.1:
;;   - Bugfix for delete within occur
;;
;;   [2015-01-19 Mo] Version 3.1.0:
;;   - Rewrote command "occur" with overlays in an indirect buffer
;;   - Removed function `org-index-copy-references-from-heading-to-property'
;;   - introduced variable org-index-version
;;
;;   [2014-12-14 Su] Version 3.0.2:
;;   - Bugfixes in occur mode
;;   - New function `org-index-copy-references-from-heading-to-property'
;;
;;   [2014-12-10 We] Version 3.0.1:
;;   - Bugfixes related with assistant
;;   - Fix for editing of category
;;
;;   [2014-12-07 Sa] Version 3.0.0:
;;   - New commands "add" and "delete" to easily add and remove
;;     the current node to or from your index.
;;   - New command "example" to create an example index.
;;   - Moved flags to a list within the same node as the index table;
;;     this breaks compatibility to prior versions of the package.
;;   - Several new flags that are explained within index node.
;;   - Removed commands "reuse", "missing", "put", "goto",
;;     "update", "link", "fill", "unhighlight"
;;   - New function `org-index-default-keybindings'
;;
;;   [2014-04-26 Sa] Version 2.4.3:
;;   - Some Bug fixes and enhancements for occur-command
;;   - Fixes for assistant to create index table
;;
;;   [2014-02-01 Sa] Version 2.4.2:
;;   - Follow mode in occur-buffer
;;   - Reorder for x-columns
;;
;;   [2014-01-02 Th] Version 2.4.0:
;;   - New command "put" to store a nodes reference in a property
;;   - New functions org-index-new-line and org-index-get-line
;;     offer access to org-index from other lisp programs
;;   - New flag p, new columns x1,x2 and x3
;;   - Major Code refactoring
;;   - Regression tests with ert
;;   - Lots of bug fixes
;;
;;   [2013-10-04 Fr] Version 2.3.2:
;;   - Bug fix: index-table created by assistant is found after
;;     restart of emacs instead of invoking assistant again
;;
;;   [2013-07-20 Sa] Version 2.3.0:
;;    - Renamed from "org-favtable" to "org-index"
;;    - Added an assistant to set up the index table
;;    - occur is now incremental, searching as you type
;;    - simplified the documentation and help-system
;;    - Saving keystrokes, as "+g237" is now valid input
;;    - Many bug fixes
;;
;;   [2013-02-28 Th] Version 2.2.0:
;;    - Allowed shortcuts like "h237" for command "head" with argument "237"
;;    - Integrated with org-mark-ring-goto
;;
;;   [2013-01-25 Fr] Version 2.1.0:
;;    - Added full support for links
;;    - New commands "statistics"
;;    - Renamed the package from "org-reftable" to "org-favtable"
;;    - Additional columns are required (e.g. "link"). Error messages will
;;      guide you
;;
;;   [2012-12-07 Fr] Version 2.0.0:
;;    - The format of the table of favorites has changed ! You need to bring
;;      your existing table into the new format by hand (which however is
;;      easy and explained below)
;;    - Reference table can be sorted after usage count or date of last access
;;    - Ask user explicitly, which command to invoke
;;    - Renamed the package from "org-refer-by-number" to "org-reftable"
;;
;;   [2012-09-22 Sa] Version 1.5.0:
;;    - New command "sort" to sort a buffer or region by reference number
;;    - New commands "highlight" and "unhighlight" to mark references
;;
;;   [2012-07-13 Fr] Version 1.4.0:
;;    - New command "head" to find a headline with a reference number
;;
;;   [2012-04-28 Sa] Version 1.3.0:
;;    - New commands occur and multi-occur
;;    - All commands can now be invoked explicitly
;;    - New documentation
;;    - Many bugfixes
;;
;;   [2011-12-10 Sa] Version 1.2.0:
;;    - Fixed a bug, which lead to a loss of newly created reference numbers
;;    - Introduced single and double prefix arguments
;;    - Started this Change Log

;;; Code:

(require 'org-table)
(require 'cl)

(defcustom org-index-id nil
  "Id of the Org-mode node, which contains the index table."
  :group 'org
  :group 'org-index)

;; Version of this package
(defvar org-index-version "3.2.0" "Version of `org-index', format is major.minor.bugfix, where \"major\" is a change in index-table and \"minor\" are new features.")

;; Variables to hold the configuration of the index table
(defvar org-index--maxref nil "Maximum number from reference table (e.g. \"153\").")
(defvar org-index--head nil "Any header before number (e.g. \"R\").")
(defvar org-index--tail nil "Tail after number (e.g. \"}\" or \")\".")
(defvar org-index--numcols nil "Number of columns in index table.")
(defvar org-index--ref-regex nil "Regular expression to match a reference.")
(defvar org-index--ref-format nil "Format, that can print a reference.")
(defvar org-index--columns nil "Columns of index-table.")
(defvar org-index--special-columns nil "Columns with flags, that may appear only once.")
(defvar org-index--flagged-columns nil "Columns with flags, that may appear multiple times.")
(defvar org-index--buffer nil "Buffer of index table.")
(defvar org-index--point nil "Position at start of headline of index table.")
(defvar org-index--below-hline nil "Position of first cell in first line below hline.")
(defvar org-index--headings nil "Headlines of index-table as a string.")
(defvar org-index-map nil "Keymap for shortcuts for some commands of `org-index'.  Can be activated and filled by org-index-default-keybings.")

;; Variables to hold context and state
(defvar org-index--last-action nil "Last action performed by `org-index'.")
(defvar org-index--text-to-yank nil "Text, that can be yanked after call (mostly a reference).")
(defvar org-index--last-ref nil "Last reference created or visited.")
(defvar org-index--point-before nil "Point in buffer with index table.")
(defvar org-index--point-saved nil "Saved point if we want to return.")
(defvar org-index--category-before nil "Category of node before.")
(defvar org-index--silent nil "Set to t, if user should not be queried.")
(defvar org-index--active-region nil "Active region, initially.  I.e. what has been marked.")
(defvar org-index--below-cursor nil "Word below cursor.")
(defvar org-index--within-node nil "True, if we are within node of the index table.")
(defvar org-index--active-window-index nil "Active window with index table (if any).")
(defvar org-index--message-text nil "Text that was issued as an explanation; helpful for regression tests.")
(defvar org-index--occur-help-text nil "Text for help in occur buffer.")
(defvar org-index--occur-help-overlay nil "Overlay for help in occur buffer.")
(defvar org-index--occur-stack nil "Stack with overlays for hiding lines.")
(defvar org-index--occur-tail-overlay nil "Overlay to cover invisible lines.")
(defvar org-index--last-sort nil "Last column, the index has been sorted after.")
(defvar org-index--sort-timer nil "Timer to sort index in correct order.")

;; static information for this program package
(defconst org-index--commands '(occur add delete head enter leave ref help example sort multi-occur highlight maintain) "List of commands available.")
(defconst org-index--required-flags '(sort) "Flags that are required.")
(defconst org-index--single-flags '(sort point-on-add yank-after-add get-category-on-add get-heading-on-add shift-ref-and-date-on-add) "Flags, that may only appear once; these can appear as special-columns.")
(defconst org-index--multiple-flags '(edit-on-add) "Flags, that might appear multiple times.")
(defconst org-index--all-flags (append org-index--single-flags org-index--multiple-flags) "All flags.")
(defconst org-index--required-headings '(ref link created last-accessed count) "All required headings.")
(defconst org-index--valid-headings (append org-index--required-headings '(keywords category)) "All valid headings.")
(defconst org-index--occur-buffer-name "*org-index-occur*" "Name of occur buffer.")
(defconst org-index--sample-flags
"
  - columns-and-flags :: associate columns of index table with flags
    - ref
      - yank-after-add
    - category
      - get-category-on-add
      - edit-on-add
    - keywords
      - get-heading-on-add
      - edit-on-add
      - point-on-add
    - count
      - sort
    - last-accessed
    - created
    - link
    - all-columns-explained :: All columns of the index table and their meaning.
      - ref :: The reference number; will be generated automatically.
      - link :: link to the node, that this line represents
      - created :: When has this entry been created ?
      - last-accessed :: When has this entry been accessed last ?
      - count :: How many times has this entry been picked ?
      - keywords :: (optional) Suggested column to keep a list of keywords,
        which may match your input during occur.
      - category :: (optional) Suggested column to get category of node.
      - Any name starting with a dot (`.') :: No predefined meaning,
        depends on its flags.
    - all-flags-explained :: All flags, that can be associated with columns.
      - sort :: Sort whole table according to this column.
      - yank-after-add :: This column will be yanked after picking this line during
        occur.
      - edit-on-add :: This field will be presented for editing, when adding
        a new line to your index.
      - point-on-add :: Point will land here, when adding a new line, e.g. with
        command ref.
      - get-category-on-add :: This column will receive the nodes category
        during command add.
      - get-heading-on-add :: This column will receive the nodes heading
        during add.
      - shift-ref-and-date-on-add :: Remove leading reference and timestamp on add."
"A sample string of flags.")


(defun org-index (&optional command search)
  "Mark and find your favorite things and org-locations easily:
Create and update an index table of references and links.  When
searching, frequently used entries appear at the top and entering
some keywords narrows down to matching entries only, so that the
right one can be spotted easily.

References are essentially small numbers (e.g. \"R237\" or \"-455-\"),
as created by this package; they are well suited to be used
outside of org.  Links are normal `org-mode' links.

This is version 3.2.0 of org-index.el.

The function `org-index' operates on a dedicated table, the index
table, which lives within its own Org-mode node.  The table and
its containing node will be created, when you first invoke
`org-index'.  The node will have some comments, describing the
columns of the index table and their associated flags.  The index
table is found through the id of the containing node, which is
stored within the variable `org-index-id'.


The function `org-index' is the only interactive function of this
package and its main entry point; once invoked it asks, which of
it subcommands to execute:

  occur: Incremental search, that shows matching lines from the
    index table, updated after every keystroke.  You may enter a
    list of words seperated by space or comma (`,'), to select
    lines that contain all of the given words.

  add: Add the current node to your index, so that it can be
    found through the subcommand \"occur\".

  delete: Delete the current node from your index.

  head: Ask for a reference number and search for an entry, which
    either has this reference contained in its heading or within
    its property org-index-ref.

  enter: Enter index table and maybe go to a specific reference.

  leave: Leave the index table and return to your previous location.

  ref: Create a new reference.

  help: Show this text.

  example: Create a temporary index, that will not be saved, but
    may serve as an example.

  sort: Sort lines in region or buffer by contained reference
    or index by count, reference or last access.

  multi-occur: Apply Emacs standard `multi-occur' operation on all
    `org-mode' buffers to search for the given reference.

  highlight: Highlight or unhiglight references in active region or buffer.

  maintain: Offers some choices to check, update or fix your index.

If you invoke `org-index' for the first time, an assistant will be
invoked, that helps you to create your own, commented index.

Use `org-index-default-keybindings' to establish convenient
keyboard shortcuts.

Optional argument COMMAND is a symbol naming the command to execute; 
SEARCH specifies search string for commands that need one."

  (interactive "P")

  (let ((org-index--silent nil)   ; t, if user can be asked
        prefix-arg                ; prefix arg
        link                      ; link of starting node, if required
        guarded-search            ; with guard against additional digits
        search-ref                ; search, if search is a reference
        search-link               ; search, if search is a link
        sort-what                 ; sort what ?
        kill-new-text             ; text that will be appended to kill ring
        initial-ref-or-link       ; initial position in index table
        message-text)          ; text that will be issued as an explanation


    ;;
    ;; Initialize and parse
    ;;

    ;; creates index table, if necessary
    (org-index--verify-id)

    ;; Get configuration of index table
    (org-index--parse-table)

    ;; store context information
    (org-index--retrieve-context)


    ;;
    ;; Arrange for proper sorting of index
    ;;

    ;; lets assume, that it has been sorted this way (we try hard to make sure)
    (unless org-index--last-sort (setq org-index--last-sort (org-index--special-column 'sort)))
    ;; rearrange for index beeing sorted into default sort order after 300 secs of idle time
    (unless org-index--sort-timer
      (setq org-index--sort-timer
            (run-with-idle-timer 300 t (lambda ()
                                        (save-excursion
                                          (org-index--verify-id)
                                          (org-index--parse-table)
                                          (set-buffer org-index--buffer)
                                          (message "%s" (org-index--do-sort-index (org-index--special-column 'sort))))
                                        (org-index--special-column 'sort)))))
    ;; at least when saving index, it should again be sorted correctly
    (with-current-buffer org-index--buffer
      (add-hook 'before-save-hook (lambda ()
                                    (save-excursion
                                      (org-index--verify-id)
                                      (org-index--parse-table)
                                      (set-buffer org-index--buffer)
                                      (org-index--do-sort-index (org-index--special-column 'sort)))) nil t))


    ;;
    ;; Find out, what we are supposed to do
    ;;

    (when (equal command '(4))
      (setq prefix-arg command)
      (setq command nil))

    (if command
        (unless (memq command org-index--commands)
          (error "Unknown command '%s' passed as argument, valid choices are any of these symbols: %s"
                 command (mapconcat 'symbol-name org-index--commands ",")))
      (setq command (intern (org-completing-read
                             "Please choose: "
                             (mapcar 'symbol-name org-index--commands)
                             nil nil))))

    ;;
    ;; Get more information, if required by some commands
    ;;

    ;; These actions need a search string:
    (when (memq command '(enter head multi-occur))
      ;; Maybe we've got a search string from the arguments
      (setq search (org-index--get-or-read-search search command))

      (when search
        (when (string-match org-index--ref-regex search)
          (setq search-ref search)
          (setq guarded-search (org-index--make-guarded-search search)))
        (when (string-match "^[a-fA-F0-9]\\{8\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{12\\}$" search)
          (setq search-link search))))

    ;; Command sort needs to know, what to sort for in advance
    (when (eq command 'sort)
      (if org-index--silent (error "Cannot ask for details, because silence is required"))
      (setq sort-what (intern (org-completing-read "You may sort:\n  - index  : your index table by various columns\n  - region : the active region by contained reference\n  - buffer : the whole current buffer\nPlease choose what to sort: " (list "index" "region" "buffer") nil t))))

    
    ;;
    ;; Check for invalid combinations of arguments; try to be helpful
    ;;

    (when (and (eq command 'head)
               (not search-ref)
               (not search-link))
      (error "Can do 'head' only for a reference or link (not '%s'), try 'occur' to search for text" search))


    ;;
    ;; Enter table
    ;;

    ;; Get link if required before moving in
    (if (eq command 'add)
        (setq link (org-id-get-create)))

    ;; Save initial ref or link for later return
    (if (and org-index--within-node
             (org-at-table-p))
        (setq initial-ref-or-link
              (or (org-index--get-or-set-field 'ref)
                  (org-index--get-or-set-field 'link))))

    ;; These commands enter index table only temporarily, but need to start in index
    (when (memq command '(occur multi-occur example))

      (set-buffer org-index--buffer)
      (goto-char org-index--point))

    ;; These commands will leave user in index table after they are finished
    (when (or (memq command '(enter ref maintain))
              (and (eq command 'sort)
                   (eq sort-what 'index)))

      ;; Support orgmode-standard of going back (buffer and position)
      (org-mark-ring-push)

      (pop-to-buffer-same-window org-index--buffer)
      (goto-char org-index--point)
      (org-index--unfold-buffer)

      ;; Remember position for leave
      (if org-index--point-before
          (setq org-index--point-saved org-index--point-before)))

    ;; prepare to return to initial position in index table
    (when initial-ref-or-link
      (while (and (org-at-table-p)
                  (not (or
                        (string= initial-ref-or-link (org-index--get-or-set-field 'ref))
                        (string= initial-ref-or-link (org-index--get-or-set-field 'link)))))
        (forward-line))
      ;; did not find ref, go back to top
      (if (not (org-at-table-p)) (goto-char org-index--point)))


    ;;
    ;; Actually do, what is requested
    ;;

    (cond


     ((eq command 'help)

      ;; bring up help-buffer for this function
      (describe-function 'org-index))


     ((eq command 'multi-occur)

      (org-index--update-line search-ref)

      ;; Construct list of all org-buffers
      (let (buff org-buffers)
        (dolist (buff (buffer-list))
          (set-buffer buff)
          (if (string= major-mode "org-mode")
              (setq org-buffers (cons buff org-buffers))))

        ;; Do multi-occur
        (multi-occur org-buffers guarded-search)

        ;; Present results
        (if (get-buffer "*Occur*")
            (progn
              (setq message-text (format "multi-occur for '%s'" search))
              (other-window 1)
              (toggle-truncate-lines 1))
          (setq message-text (format "Did not find '%s'" search)))))


     ((eq command 'add)

      (let ((r (org-index--do-add-or-update link)))
        (setq message-text (car r))
        (setq kill-new-text (cdr r))))


     ((eq command 'delete)

      (setq message-text (org-index--do-delete)))


     ((eq command 'head)

      (let (link)
        (if (and org-index--within-node
                 (org-at-table-p))
            (setq link (org-index--get-or-set-field 'link)))

        (setq message-text (org-index--do-head search-ref (or link search-link)))))


     ((eq command 'leave)

      (setq kill-new-text org-index--text-to-yank)
      (setq org-index--text-to-yank nil)

      ;; If "leave" has been called two times in succession, make
      ;; org-mark-ring-goto believe it has been called two times too
      (if (eq org-index--last-action 'leave)
          (let ((this-command nil) (last-command nil))
            (org-mark-ring-goto 1))
        (org-mark-ring-goto))

      ;; Return to saved position in index buffer
      (when (and org-index--point-saved
                 (eq (marker-buffer org-index--point-saved)
                     org-index--buffer))
        ;; buffer displayed in window need to set point there first
        (if (eq (window-buffer org-index--active-window-index)
                org-index--buffer)
            (set-window-point org-index--active-window-index (marker-position org-index--point-saved)))
        ;; set position in buffer in any case and second
        (with-current-buffer org-index--buffer
          (goto-char org-index--point-saved)))
      (setq org-index--point-saved nil))


     ((eq command 'enter)

      (goto-char org-index--below-hline)
      
      (if search
          ;; Go downward in table to requested reference
          (progn
            (setq message-text (cdr (org-index--find-in-index search search-link)))
            (org-index--update-line))
        ;; simply go into table
        (setq message-text "At index table"))

      (recenter))


     ((eq command 'occur)

      (org-index--do-occur))


     ((eq command 'ref)

      (let (new)

        ;; add a new row
        (setq new (org-index--create-new-line t))

        ;; fill special columns with standard values
        (org-table-goto-column (org-index--column-num 'ref))
        (insert new)
        (setq org-index--last-ref new)

        (org-index--align)

        ;; goto point-field or first empty one or first field
        (if (org-index--special-column 'point-on-add)
            (org-table-goto-column (org-index--column-num (org-index--special-column 'point-on-add)))
          (unless (catch 'empty
                    (dotimes (col org-index--numcols)
                      (org-table-goto-column (+ col 1))
                      (if (string= (org-trim (org-table-get-field)) "")
                          (throw 'empty t))))
            ;; none found, goto first
            (org-table-goto-column 1)))

        (if org-index--active-region (setq kill-new-text org-index--active-region))
        (setq message-text (format "Adding a new row with ref '%s'" new))))


     ((eq command 'sort)

      (let (sort)

        (cond
         ((eq sort-what 'index)
          (setq sort
                (intern
                 (org-icompleting-read
                  "Please choose column to reorder index table once: "
                  (list "ref" "count" "created" "last-accessed")
                  nil t nil nil (symbol-name (org-index--special-column 'sort)))))

          (org-index--do-sort-index sort)

          (message "Your index has temporarily been sorted by %s, will be sorted by %s after some idle time."
                   (symbol-name sort)
                   (org-index--special-column 'sort)))

         ((memq sort-what '(region buffer))
          (org-index--do-sort-lines sort-what)
          (message "Sorted %s by contained references." sort-what)))))


     ((eq command 'highlight)

      (let ((where "buffer"))
        (save-excursion
          (save-restriction
            (when (and transient-mark-mode
                       mark-active)
              (narrow-to-region (region-beginning) (region-end))
              (setq where "region"))

            (if prefix-arg
                (progn
                  (unhighlight-regexp org-index--ref-regex)
                  (setq message-text (format "Removed highlights for references in %s" where)))
              (highlight-regexp org-index--ref-regex 'isearch)
              (setq message-text (format "Highlighted references in %s" where)))))))


     ((eq command 'maintain)
      (let ((check-what))
        (setq check-what (intern (org-completing-read "These checks and fixes are available:\n  - links      : add links to all entries of your index table\n  - statistics : compute statistics about index table\nPlease choose: " (list "links" "statistics") nil t nil nil "statistics")))
        (message nil)
        
        (cond
         ((eq check-what 'links)
          (setq message-text (org-index--complete-links)))

         ((eq check-what 'statistics)
          (setq message-text (org-index--do-statistics))))))


     ((eq command 'example)

      (if (y-or-n-p "This assistand will help you to create a temporary index with detailed comments.\nDo you want to proceed ? ")
          (org-index--create-index t)))


     (t (error "This is a bug: unmatched case '%s'" command)))


    ;; remember what we have done for next time
    (setq org-index--last-action command)

    ;; tell, what we have done and what can be yanked
    (if kill-new-text (setq kill-new-text
                            (substring-no-properties kill-new-text)))
    (if (string= kill-new-text "") (setq kill-new-text nil))
    (let ((m (concat
              message-text
              (if (and message-text kill-new-text)
                  " and r"
                (if kill-new-text "R" ""))
              (if kill-new-text (format "eady to yank '%s'" kill-new-text) ""))))
      (unless (string= m "")
        (message m)
        (setq org-index--message-text m)))
    (if kill-new-text (kill-new kill-new-text))))


(defun org-index-default-keybindings ()
  "Set default keybindings for `org-index'.

Establish the common prefix key `C-c i' Which is followed by the
first letter of a subcommand, so that `C-c i a' invokes the
subcommand \"add\". Subcommands available are occur, add, delete,
head, enter, leave and ref. As a special case `C-c i i' invokes
`org-index' to let you choose."
  (interactive)
  (define-prefix-command 'org-index-map)
  (global-set-key (kbd "C-c i") 'org-index-map)
  (define-key org-index-map (kbd "i") (lambda () (interactive) (message nil) (org-index)))
  (define-key org-index-map (kbd "o") (lambda () (interactive) (message nil) (org-index 'occur)))
  (define-key org-index-map (kbd "a") (lambda () (interactive) (message nil) (org-index 'add)))
  (define-key org-index-map (kbd "d") (lambda () (interactive) (message nil) (org-index 'delete)))
  (define-key org-index-map (kbd "h") (lambda () (interactive) (message nil) (org-index 'head)))
  (define-key org-index-map (kbd "e") (lambda () (interactive) (message nil) (org-index 'enter)))
  (define-key org-index-map (kbd "l") (lambda () (interactive) (message nil) (org-index 'leave)))
  (define-key org-index-map (kbd "r") (lambda () (interactive) (message nil) (org-index 'ref))))


(defun org-index-new-line (&rest keys-values)
  "Create a new line within the index table, returning its reference.

The function takes a varying number of argument pairs; each pair
is a symbol for an existing column heading followed by its value.

Example:

  (org-index-new-line 'ref t 'link \"7f480c3e\")

Passing \"'ref t\" will make the function create a new reference
within the new line.

Optional argument KEYS-VALUES specifies content of new line."

  (org-index--verify-id)
  (org-index--parse-table)

  (car (org-index--do-new-line keys-values)))


(defun org-index--do-new-line (&rest keys-values)
  "Do the work for `org-index-new-line'.
Optional argument KEYS-VALUES specifies content of new line."

  (let ((org-index--silent t))

    (save-excursion
      (org-index--retrieve-context)
      (with-current-buffer org-index--buffer
        (goto-char org-index--point)

        ;; check arguments early; they might come from a lisp-user
        (let ((kvs keys-values)
              k v)
          (while kvs
            (setq k (car kvs))
            (setq v (cadr kvs))
            (if (eq k 'ref)
                (unless (memq v '(t nil))
                  (error "Column 'ref' accepts only \"t\" or \"nil\""))
              (if (or (not (symbolp k))
                      (and (symbolp v) (not (eq v t)) (not (eq v nil))))
                  (error "Arguments must be alternation of key and value")))
            (unless (org-index--column-num k)
              (error "Unknown column or column not defined in table: '%s'" (symbol-name k)))
            (setq kvs (cddr kvs))))

        (if (and (not (plist-get keys-values 'ref))
                 (not (stringp (plist-get keys-values 'link))))
            (error "Need a link when not creating a ref"))

        (let (ref yank)
          ;; create new line
          (setq ref (org-index--create-new-line (plist-get keys-values 'ref)))
          (plist-put keys-values 'ref (or ref ""))

          ;; fill columns
          (let ((kvs keys-values)
                k v n)
            (while kvs
              (setq k (car kvs))
              (setq v (cadr kvs))
              (org-table-goto-column (org-index--column-num k))
              (insert (org-trim v))
              (setq kvs (cddr kvs))))

          ;; get column to yank
          (setq yank (org-index--get-or-set-field (org-index--special-column 'yank-after-add)))

          (cons ref yank))))))


(defun org-index-get-line (type value)
  "Retrieve an existing line within the index table by ref or link.
Return its contents as a property list.

The function `plist-get' may be used to retrieve specific elements
from the result.

Example:

  (plist-get (org-index-get-line 'ref \"R12\") 'count)

retrieves the value of the count-column for reference number 12.

Argument TYPE is a symbol, either ref or link,
argument VALUE specifies the value to search for."
  ;; check arguments
  (unless (memq type '(ref link))
    (error "Argument  can only be 'ref' or 'link'"))

  (org-index--verify-id)
  (org-index--parse-table)

  (org-index--get-or-delete-line 'get type value))


(defun org-index--get-or-delete-line (command type value)
  "Get or delete (according to COMMAND) a line by TYPE and VALUE."
  (let ((org-index--silent t)
        found)

    (save-excursion
      (org-index--retrieve-context)
      (with-current-buffer org-index--buffer

        (goto-char org-index--below-hline)
        (while (and (not found)
                    (org-at-table-p))
          (when (string= (org-index--get-or-set-field type)
                         value)
            ;; found matching line
            (if (eq command 'get)
                ;; get its fields
                (mapc (lambda (x)
                        (if (and (numberp (cdr x))
                                 (> (cdr x) 0))
                            (setq found (cons (car x) (cons (or (org-index--get-or-set-field (car x)) "") found)))
                          )) (reverse org-index--columns))
              ;; or delete it
              (let ((start (point)))
                (beginning-of-line)
                (forward-line)
                (delete-region start (point)))))
          (forward-line))))
    found))


(defun org-index--get-or-read-search (search command)
  "Get SEARCH string, maybe read from user; respect COMMAND that will be executed."

  (let (search-from-table
        search-from-cursor)

    (unless search
      ;; Search string can come from several sources:
      ;; From link or ref columns of table
      (when (and org-index--within-node
                 (org-at-table-p))
        (setq search-from-table (or (org-index--get-or-set-field 'link)
                                    (org-index--get-or-set-field 'ref))))

      ;; From string below cursor
      (when (and (not org-index--within-node)
                 org-index--below-cursor
                 (string-match (concat "\\(" org-index--ref-regex "\\)")
                               org-index--below-cursor))
        (setq search-from-cursor (match-string 1 org-index--below-cursor)))

      ;; Depending on requested action, get search from one of the sources above
      (cond ((eq command 'enter)
             (setq search search-from-cursor))
            ((memq command '(head occur))
             (setq search (or search-from-table search-from-cursor)))))


    ;; From occur-buffer into index ?
    (unless search
      (if (and (string= (buffer-name) org-index--occur-buffer-name)
               (org-at-table-p))
          (setq search (org-index--get-or-set-field 'ref))))

    
    ;; If we still do not have a search string, ask user explicitly
    (unless search

      (if org-index--silent (error "Need to specify search, if silence is required"))

      (if (eq command 'enter)
          ;; accept single char commands or switch to reading a sequence of digits
          (let (char prompt)

            ;; start with short prompt but give more help on next iteration
            (setq prompt "Please specify, where to go in index (0-9.,space,backspace,return or ? for help): ")

            ;; read one character
            (while (not (memq char (append (number-sequence ?0 ?9) (list ?\d ?\b ?\r ?\j ?\s))))
              (setq char (read-char prompt))
              (setq prompt "Go to index table and specific position. Digits specify a reference number to got to, `.' or <space> go to top of index, <backspace> or <delete> to last line created and <return> to index line of current node. Please choose: "))

            (if (memq char (number-sequence ?0 ?9))
                ;; read rest of digits
                (setq search (read-from-minibuffer "Search reference number: " (char-to-string char)))
              ;; decode single chars
              (if (memq char '(?\r ?\n))  (setq search (org-id-get)))
              (if (memq char '(?. ?\s)) (setq search nil))
              (if (memq char '(?\d ?\b)) (setq search (number-to-string org-index--maxref)))))

          (setq search (read-from-minibuffer "Search reference number: "))))

    ;; Clean up and examine search string
    (when search
      (setq search (org-trim search))
      (if (string= search "") (setq search nil))
      (when search
        (if (string-match "^[0-9]+$" search)
            (setq search (concat org-index--head search org-index--tail)))))

    search))


(defun org-index--verify-id ()
  "Check, that we have a valid id."

  ;; Check id
  (unless org-index-id
    (org-index--create-missing-index "Variable org-index-id is not set, so probably no index table has been created yet."))

  ;; Find node
  (let (marker)
    (setq marker (org-id-find org-index-id 'marker))
    (unless marker (org-index--create-missing-index "Cannot find the node with id \"%s\" (as specified by variable org-index-id)." org-index-id))
    ; Try again with new node
    (setq marker (org-id-find org-index-id 'marker))
    (unless marker (error "Could not create node"))
    (setq org-index--buffer (marker-buffer marker)
          org-index--point (marker-position marker))
    (move-marker marker nil)))


(defun org-index--retrieve-context ()
  "Collect context information before starting with command."

  ;; Get the content of the active region or the word under cursor
  (setq org-index--active-region
        (if (and transient-mark-mode mark-active)
            (buffer-substring (region-beginning) (region-end))
          nil))
  (setq org-index--below-cursor (thing-at-point 'symbol))

  ;; get category of current node
  (setq org-index--category-before
        (save-excursion ; workaround: org-get-category does not give category when at end of buffer
          (beginning-of-line)
          (org-get-category)))

  ;; Find out, if we are within index table or not
  (setq org-index--within-node (string= (org-id-get) org-index-id))

  ;; Check and remember, if active window contains buffer with index table
  (if (eq (window-buffer) org-index--buffer)
      (setq org-index--active-window-index (selected-window)))

  ;; get current position in index-buffer
  (with-current-buffer org-index--buffer
    (setq org-index--point-before
          (if (string= (org-id-get) org-index-id)
              nil
            (point-marker)))))


(defun org-index--parse-table ()
  "Parse content of index table."

  (let (ref-field
        link-field
        initial-point
        end-of-headings
        start-of-headings)

    (with-current-buffer org-index--buffer

      (setq org-index--maxref 0)
      (setq initial-point (point))

      (org-index--go-below-hline)
      (beginning-of-line)

      ;; get headings to display during occur
      (setq end-of-headings (point))
      (while (org-at-table-p) (forward-line -1))
      (forward-line)
      (setq start-of-headings (point))
      (setq org-index--headings (buffer-substring start-of-headings end-of-headings))

      ;; count columns
      (org-table-goto-column 100)
      (setq org-index--numcols (- (org-table-current-column) 1))

      ;; go to top of table
      (while (org-at-table-p)
        (forward-line -1))
      (forward-line)

      ;; parse line of headings
      (org-index--parse-headings)
      (forward-line 2)

      ;; parse list of flags
      (goto-char org-index--point)
      (org-index--parse-flags)

      (org-index--go-below-hline)
      (setq org-index--below-hline (point-marker))

      ;; Retrieve any decorations around the number within the first nonempty ref-field
      (goto-char org-index--below-hline)
      (while (and (org-at-table-p)
                  (not (setq ref-field (org-index--get-or-set-field 'ref))))
        (forward-line))

      ;; Some Checking
      (unless ref-field
        (org-index--report-index-error "Reference column is empty"))

      (unless (string-match "^\\([^0-9]*\\)\\([0-9]+\\)\\([^0-9]*\\)$" ref-field)
        (org-index--report-index-error
         "First reference in index table ('%s') does not contain a number" ref-field))

      ;; These are the decorations used within the first ref of index
      (setq org-index--head (match-string 1 ref-field))
      (setq org-index--tail (match-string 3 ref-field))
      (setq org-index--ref-regex (concat (regexp-quote org-index--head)
                                         "\\([0-9]+\\)"
                                         (regexp-quote org-index--tail)))
      (setq org-index--ref-format (concat org-index--head "%d" org-index--tail))

      ;; Go through table to find maximum number and do some checking
      (let ((ref 0))

        (while (org-at-table-p)

          (setq ref-field (org-index--get-or-set-field 'ref))
          (setq link-field (org-index--get-or-set-field 'link))

          (when (and (not ref-field)
                     (not link-field))
            (kill-whole-line)
            (message "Removing line from index-table with both ref and link empty"))

          (if ref-field
              (if (string-match org-index--ref-regex ref-field)
                  ;; grab number
                  (setq ref (string-to-number (match-string 1 ref-field)))
                (kill-whole-line)
                (message "Removing line from index-table whose ref does not contain a number")))

          ;; check, if higher ref
          (if (> ref org-index--maxref) (setq org-index--maxref ref))

          (forward-line 1)))

      ;; go back to initial position
      (goto-char initial-point))))


(defun org-index--do-sort-index (sort)
  "Sort index table according to SORT."

  (let ((is-modified (buffer-modified-p))
        top
        bottom
        ref-field
        count-field)

    (unless buffer-read-only

      (message "Sorting table for %s ..." (symbol-name sort))
      (undo-boundary)

      (let ((message-log-max nil)) ; we have just issued a message, dont need those of sort-subr

        ;; get boundaries of table
        (goto-char org-index--below-hline)
        (forward-line 0)
        (setq top (point))
        (while (org-at-table-p) (forward-line))

        ;; kill all empty rows at bottom
        (while (progn
                 (forward-line -1)
                 (org-table-goto-column 1)
                 (and
                  (not (org-index--get-or-set-field 'ref))
                  (not (org-index--get-or-set-field 'link))))
          (org-table-kill-row))
        (forward-line 1)
        (setq bottom (point))

        ;; sort lines
        (save-restriction
          (narrow-to-region top bottom)
          (goto-char top)
          (sort-subr t
                     'forward-line
                     'end-of-line
                     (lambda () (org-index--get-sort-key sort t))
                     nil
                     'string<)
          (goto-char (point-min))

          (org-table-align)
          
          ;; restore modification state
          (set-buffer-modified-p is-modified)))

        (setq org-index--last-sort sort))))


(defun org-index--do-sort-lines (what)
  "Sort lines in WHAT according to contained reference."
  (save-restriction
    (cond
     ((eq what 'region)
      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (error "No active region, cannot sort")))
     ((eq what 'buffer)
      (unless (y-or-n-p "Sort whole current buffer ? ")
        (error "Canceled"))
      (narrow-to-region (point-min) (point-max))))

    (goto-char (point-min))
    (undo-boundary)
    (sort-subr nil 'forward-line 'end-of-line
               (lambda ()
                 (if (looking-at (concat ".*"
                                         (org-index--make-guarded-search org-index--ref-regex 'dont-quote)))
                     (string-to-number (match-string 1))
                   0)))))


(defun org-index--go-below-hline ()
  "Move below hline in index-table."

  (let ((count 0)
        (errstring (format "index table within node %s" org-index-id)))

    (goto-char org-index--point)

    ;; go to heading of node
    (while (not (org-at-heading-p)) (forward-line -1))
    (forward-line 1)

    ;; go to first table, but make sure we do not get into another node
    (while (and (not (org-at-table-p))
                (not (org-at-heading-p))
                (not (eobp)))
      (forward-line))

    ;; check, if there really is a table
    (unless (org-at-table-p)
      (org-index--create-missing-index "Cannot find %s." errstring))

    ;; go just after hline
    (while (and (not (org-at-table-hline-p))
                (org-at-table-p))
      (forward-line))
    (forward-line)

    ;; and check
    (unless (org-at-table-p)
      (org-index--report-index-error "Cannot find a hline within %s" errstring))

    (org-table-goto-column 1)))


(defun org-index--align ()
  "Align index table without setting its buffer modified."
  (let ((is-modified (buffer-modified-p)))
    (unless buffer-read-only (org-table-align))
    (set-buffer-modified-p is-modified)
    (org-index--go-below-hline)
    (setq org-index--below-hline (point-marker))))


(defun org-index--parse-headings ()
  "Parse headings of index table."

  (let (field        ;; field content
        field-symbol ;; and as a symbol
        found)

    (setq org-index--columns nil)

    ;; For each column
    (dotimes (col org-index--numcols)

      (setq field (substring-no-properties (downcase (org-trim (org-table-get-field (+ col 1))))))

      (if (string= field "")
          (error "Heading of column cannot be empty"))
      (if (and (not (string= (substring field 0 1) "."))
               (not (member (intern field) org-index--valid-headings)))
          (error "Column name '%s' is not a valid heading (custom headings may start with a dot (e.g. '.foo')" field))

      (setq field-symbol (intern field))

      ;; check if heading has already appeared
      (if (assoc field-symbol org-index--columns)
          (org-index--report-index-error
           "'%s' appears two times as column heading" (downcase field))
        ;; add it to list at front, reverse later
        (setq org-index--columns (cons (cons field-symbol (+ col 1)) org-index--columns)))))

  (setq org-index--columns (reverse org-index--columns))

  ;; check if all necessary headings have appeared
  (mapc (lambda (head)
          (unless (cdr (assoc head org-index--columns))
            (org-index--report-index-error "No column has heading '%s'" head)))
        org-index--required-headings))


(defun org-index--parse-flags ()
  "Parse list of flags in index table."

  (let (parent parent-is-comment child)

    ;; reset configuration variables
    (setq org-index--special-columns nil)
    (setq org-index--flagged-columns nil)

    (org-index--goto-list "columns-and-flags" t)
    (forward-line 1)

    ;; outer loop over columns
    (while (and (setq parent (org-index--parse-list-item))
                parent
                (> (cdr (assoc :indent parent)) 0))

      (setq parent-is-comment (member (cdr (assoc :text parent)) '("all-columns-explained" "all-flags-explained")))

      ;; check, that we have a valid heading
      (unless (or parent-is-comment
                   (assoc (cdr (assoc :sym parent)) org-index--columns))
        (org-index--report-index-error "'%s' appears within flags, but not as a index column.? " (cdr (assoc :text parent))))

      ;; inner loop over children
      (while (and (forward-line 1)
                  (setq child (org-index--parse-list-item))
                  child
                  (> (cdr (assoc :indent child))
                     (cdr (assoc :indent parent))))

        (unless parent-is-comment
          ;; check, that we have a valid flag
          (unless (memq (cdr (assoc :sym child)) org-index--all-flags)
            (org-index--report-index-error "'%s' is not a valid flag" (cdr (assoc :text child))))

          ;; process flag with respect to current index-column
          (if (memq (cdr (assoc :sym child)) org-index--single-flags)
              ;; Check, that none of org-index--single-flags appears twice
              (if (assoc (cdr (assoc :sym child)) org-index--special-columns)
                  (org-index--report-index-error
                   "More than one column is marked with flag '%s'" (cdr (assoc :text child)))
                ;; add it to list
                (setq org-index--special-columns (cons (cons (cdr (assoc :sym child)) (cdr (assoc :sym parent)))
                                                       org-index--special-columns))))

          ;; all flags are stored in org-index--flagged-columns
          (let ((l (assoc (cdr (assoc :sym child)) org-index--flagged-columns))) ;; list of flag and columns, that carry this flag
            (unless l
              ;; no list of columns with this flag is present, create one
              (setq org-index--flagged-columns
                    (cons (cons (cdr (assoc :sym child)) nil)
                          org-index--flagged-columns))
              (setq l (car org-index--flagged-columns)))
            ;; prepend this column to list of columns with this flag
            (setcdr l (cons (cdr (assoc :sym parent)) (cdr l)))))))

    ;; check, that all needed flags have been specified
    (mapc (lambda (x)
            (unless (assoc x org-index--special-columns)
              (org-index--report-index-error "Required flag '%s' does not appear" (substring (symbol-name x) 1))))
          org-index--required-flags)))


(defun org-index--goto-list (name &optional required non-top)
  "Goto list NAME (maybe NON-TOP Level) in index node, err if REQUIRED list is not present."
  (goto-char org-index--point)

  ;; go to heading of node
  (while (not (org-at-heading-p)) (forward-line -1))
  (forward-line 1)

  ;; go to named list
  (while (and (not (let ((item (org-index--parse-list-item)))
                     (if item
                         (and (or non-top (= (cdr (assoc :indent item)) 0)) ;; accept only toplevel ?
                              (string= (cdr (assoc :text item)) name)) ;; with requested name
                       nil)))
              (not (org-at-table-p))
              (not (org-at-heading-p))
              (not (eobp)))
    (forward-line 1))

  (if (org-at-item-p)
      t
    (if required
        (org-index--report-index-error "Could not find required list '%s'" name)
      nil)))


(defun org-index--parse-list-item ()
  "Parse a list item into an assoc array (indent, checkbox, text, value)."

  ;; matche full list-item, maybe with checkbox and double-colon
  (if (looking-at org-list-full-item-re)

      ;; retrieve interesting parts of list item from match data
      (let (indent checkbox text value next-line)

        (setq indent
              (- (save-excursion (goto-char (match-beginning 1)) (current-column)) ; first column
                 (save-match-data (org-current-level)) ; indent-level
                 1))
        (setq checkbox (match-string 3))
        (setq text (match-string 4))
        (set (if text 'value 'text) (buffer-substring (match-end 0) (line-end-position))) ; regexp did not capture this

        ;; peek ahead, if item continues on next line
        (forward-line 1)
        (if (looking-at org-list-full-item-re)
            (forward-line -1) ; already at next item; go back
          (setq next-line (buffer-substring (line-beginning-position) (line-end-position))))
        
        ;; clean up strings
        (mapc (lambda (x)
                (if (stringp (symbol-value x))
                    (set x (org-trim (substring-no-properties (symbol-value x))))))
              '(text value next-line))

        (if next-line (setq text (concat text " " next-line))) ; append next line if
        
        (list (cons :indent indent) (cons :text text) (cons :value value) (cons :sym (intern text))))
    nil))


(defun org-index--create-missing-index (&rest reasons)
  "Create a new empty index table with detailed explanation.  Argument REASONS explains why."

  (org-index--ask-before-create-index "Cannot find your index table: "
                                      "new permanent" "."
                                      reasons)
  (org-index--create-index))



(defun org-index--report-index-error (&rest reasons)
  "Report an error (explained by REASONS) with the existing index and offer to create a valid one to compare with."

  (org-index--ask-before-create-index "The existing index contains this error: "
                                      "temporary" ", to compare with."
                                      reasons)
    (org-index--create-index t t))



(defun org-index--ask-before-create-index (explanation type for-what reasons)
                                                  ; checkdoc-params: (explanation type for-what reasons)
  "Ask the user before creating an index or throw error.  Arguments specify bits of issued message."
  (let (reason prompt)

    (setq reason (apply 'format reasons))
    (if org-index--silent (error (concat explanation reason))) ; cannot proceed without querying the user

    (setq prompt (concat explanation reason "\n\n"
                         "However, this assistant can help you to create a "
                         type " index with detailed comments" for-what "\n\n"
                         "Do you want to proceed ?"))

    (unless (let ((max-mini-window-height 1.0))
              (y-or-n-p prompt))
      (error explanation reason))))



(defun org-index--create-index (&optional temporary compare)
  "Create a new empty index table with detailed explanation.
specify flag TEMPORARY for th new table temporary, maybe COMPARE it with existing index."
  (let (buffer
        title
        firstref
        id)

    (if temporary
        (let ((file-name (concat temporary-file-directory "org-index--example-index.org"))
              (buffer-name "*org-index-example-index*"))
          (setq buffer (get-buffer-create buffer-name))
          (with-current-buffer buffer
            ;; but it needs a file for its index to be found
            (unless (string= (buffer-file-name) file-name)
              (set-visited-file-name file-name))
            (rename-buffer buffer-name) ; name is change by line above

            (erase-buffer)
            (org-mode)))

      (setq buffer (get-buffer (org-completing-read "Please choose the buffer, where the new node for the index table should be created; the new node will be inserted at its end.\n\nBuffer: " (mapcar 'buffer-name (org-buffer-list)) nil nil))))

    (setq title (read-from-minibuffer "Please enter the title of the index node: "))

    (while (progn
             (setq firstref (read-from-minibuffer "Please enter your first reference-number. This is a number preceeded by some non-digit chars and optionally followed by some more non-digit chars, e.g. 'R1', '-1-' or '#1#' (and your initial number does not need to be '1'). The format of your reference-numbers only needs to make sense for yourself, so that you can spot it easily in your texts or write it on a piece of paper; it should however not already appear to frequently within your existing notes, to avoid too many false hits when searching.\n\nPlease choose: "))
             (let (desc)
               (when (string-match "[[:blank:]]" firstref)
                 (setq desc "Contains whitespace"))
               (when (string-match "[[:cntrl:]]" firstref)
                 (setq desc "Contains control characters"))
               (unless (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)
                 ;; firstref not okay, report details
                 (setq desc
                       (cond ((string= firstref "") "is empty")
                             ((not (string-match "^[^0-9]+" firstref)) "starts with a digit")
                             ((not (string-match "^[^0-9]+[0-9]+" firstref)) "does not contain a number")
                             ((not (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)) "contains more than one sequence of digits")

                             )))
               (if desc
                   (progn
                     (read-from-minibuffer (format "Your input '%s' does not meet the requirements because it %s.\nPlease hit RET and try again: " firstref desc))
                     t)
                 nil))))

    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "* %s %s\n" firstref title))
      (if temporary
          (insert "
  Below you find your temporary index table, which will not last longer
  than your current emacs session.
")
        (insert "
  Below you find your initial index table, which will grow over time.
"))
      (insert "
  You may start using it by adding some lines. Just move to
  another heading, invoke `org-index' and choose the command
  \"add\".  After adding a few nodes, try the command \"occur\"
  to search among them.

  To gain further insight you may invoke the subcommand \"help\", or
  read the description of `org-index'.

  Within the index table below, dhe sequence of columns does not
  matter. You may reorder them any way you like. Columns are
  found by their heading. You may also add your own columns.

  Following these explanations there is the item-list
  `columns-and-flags', which influences the behaviour of
  `org-index'. See the explanations which are part of this list.

  This node needs not be a top level node; its name is completely
  at your choice; it is found through its ID only.

  Remark: These lines of explanation can be removed at any time.
")

      (setq id (org-id-get-create))
      (insert (format "
%s


  | ref |  category | keywords | count | last-accessed | created | link |
  |     |           |          |       |               |         | <4>  |
  |-----+-----------+----------+-------+---------------+---------+------|
  | %s  |           | %s       |       |               | %s      | %s   |

"
                      org-index--sample-flags
                      firstref
                      "This node"
                      (with-temp-buffer (org-insert-time-stamp nil nil t))
                      id))

      ;; make sure, that node can be found
      (org-id-add-location id (buffer-file-name))
      (setq buffer-save-without-query t)
      (basic-save-buffer)

      (while (not (org-at-table-p)) (forward-line -1))
      (unless buffer-read-only (org-table-align))
      (while (not (org-at-heading-p)) (forward-line -1))

      ;; read back some info about new index
      (let ((org-index-id id))
	(org-index--verify-id))

      ;; present results to user
      (if temporary
          (progn
            ;; Present existing and temporary index together
            (when compare
              (pop-to-buffer-same-window org-index--buffer)
              (goto-char org-index--point)
              (org-index--unfold-buffer)
              (delete-other-windows)
              (select-window (split-window-vertically)))
            ;; show new index
            (pop-to-buffer-same-window buffer)
            (org-id-goto id)
            (org-index--unfold-buffer)
            (if compare
                (error "Please compare your existing index (upper window) and a temporary new one (lower window) to fix your index")
              (message "This is your new temporary index.")))
        (progn
          ;; Only show the new index
          (pop-to-buffer-same-window buffer)
          (delete-other-windows)
          (org-id-goto id)
          (org-index--unfold-buffer)
          (setq org-index-id id)
          (if (y-or-n-p "This is your new index table.  It is already set for this Emacs session, so you may try it out.  Do you want to save its id to make it available for future Emacs sessions too ? ")
              (progn
                (customize-save-variable 'org-index-id id)
                (error "Saved org-index-id '%s' to %s" id (or custom-file
							      user-init-file)))
            (let (sq)
              (setq sq (format "(setq org-index-id \"%s\")" id))
              (kill-new sq)
              (error "Did not make the id of this new index permanent; you may want to put\n\n   %s\n\ninto your own initialization; it is copied already, just yank it" sq))))))))


(defun org-index--unfold-buffer ()
  "Helper function to unfold buffer."
  (org-show-context)
  (org-show-subtree)
  (recenter 1)
  (save-excursion
    (org-back-to-heading)
    (forward-line) ;; on property drawer
    (org-cycle)
    (org-index--goto-list "columns-and-flags")
    (org-cycle)))


(defun org-index--update-line (&optional ref-or-link)
  "Update columns count and last-accessed in line REF-OR-LINK."

  (let ((newcount 0)
        initial)

    (with-current-buffer org-index--buffer
      (unless buffer-read-only

        ;; search reference or link, if given (or assume, that we are already positioned right)
        (when ref-or-link
          (setq initial (point))
          (goto-char org-index--below-hline)
          (while (and (org-at-table-p)
                      (not (or (string= ref-or-link (org-index--get-or-set-field 'ref))
                               (string= ref-or-link (org-index--get-or-set-field 'link)))))
            (forward-line)))

        (if (not (org-at-table-p))
            (error "Did not find reference or link '%s'" ref-or-link)
          (org-index--update-current-line))

        (if initial (goto-char initial))))))


(defun org-index--update-current-line ()
  "Update current lines columns count and last-accessed."
  (let (newcount (count-field (org-index--get-or-set-field 'count)))

    ;; update count field only if number or empty
    (when (or (not count-field)
              (string-match "^[0-9]+$" count-field))
      (setq newcount (+ 1 (string-to-number (or count-field "0"))))
      (org-index--get-or-set-field 'count
                            (number-to-string newcount)))

    ;; update timestamp
    (org-table-goto-column (org-index--column-num 'last-accessed))
    (org-table-blank-field)
    (org-insert-time-stamp nil t t)

    ;; move line according to new content
    (org-index--promote-current-line)))


(defun org-index--promote-current-line ()
  "Move current line up in table according to changed sort fields."
  (let (begin end key
        (to-skip 0))

    (forward-line 0) ; stay at beginning of line

    (setq key (org-index--get-sort-key))
    (setq begin (point))
    (setq end (line-beginning-position 2))

    (forward-line -1)
    (while (and (org-at-table-p)
                (not (org-at-table-hline-p))
                (string< (org-index--get-sort-key) key))

      (incf to-skip)
      (forward-line -1))
    (forward-line 1)

    ;; insert line at new position
    (when (> to-skip 0)
      (insert (delete-and-extract-region begin end))
      (forward-line -1))))


(defun org-index--get-sort-key (&optional sort with-ref)
  "Get value for sorting from column SORT, optional WITH-REF."
  (let (ref
        ref-field
        key)

    (unless sort (setq sort org-index--last-sort)) ; use default value

    (when (or with-ref
              (eq sort 'ref))
      ;; get reference with leading zeroes, so it can be
      ;; sorted as text
      (setq ref-field (org-index--get-or-set-field 'ref))
      (string-match org-index--ref-regex ref-field)
      (setq ref (format
                 "%06d"
                 (string-to-number
                  (or (match-string 1 ref-field)
                      "0")))))

    (setq key
          (cond
           ((eq sort 'count)
            (format "%08d" (string-to-number (or (org-index--get-or-set-field 'count) ""))))
           ((eq sort 'ref)
            ref)
           ((eq sort 'last-accessed)
            (org-index--get-or-set-field sort))
           ((eq sort 'created)
            (org-index--get-or-set-field sort))
           (t (error "This is a bug: unmatched case '%s'" sort))))

    (if with-ref (setq key (concat key ref)))

    key))


(defun org-index--get-or-set-field (key &optional value)
  "Retrieve field KEY from index table or set it to VALUE."
  (let (field)
    (save-excursion
      (setq field (org-trim (org-table-get-field (cdr (assoc key org-index--columns)) value)))
      (if (string= field "") (setq field nil))

      (org-no-properties field))))


(defun org-index--column-num (key)
  "Return number of column KEY."
  (if (numberp key)
      key
    (cdr (assoc key org-index--columns))))


(defun org-index--special-column (key)
  "Return column (not a number) for special column KEY."
  (cdr (assoc key org-index--special-columns)))


(defun org-index--flag-p (flag column)
  "Check if COLUMN has FLAG set."
  (unless (memq flag org-index--all-flags)
    (error (format "Internal error: unknown flag %s" (symbol-name flag))))
  (memq column (assoc flag org-index--flagged-columns)))


(defun org-index--make-guarded-search (ref &optional dont-quote)
  "Make robust search string from REF; DONT-QUOTE it, if requested."
  (concat "\\_<" (if dont-quote ref (regexp-quote ref)) "\\_>"))


(defun org-index--do-statistics ()
  "Compute statistics about index table."
  (let ((total 0)
        ref-field
        ref
        min
        max
        message)


    ;; go through table and remove all refs, that we see
    (goto-char org-index--below-hline)
    (while (org-at-table-p)

      ;; get ref-field and number
      (setq ref-field (org-index--get-or-set-field 'ref))
      (if (and ref-field
               (string-match org-index--ref-regex ref-field))
          (setq ref (string-to-number (match-string 1 ref-field))))

      ;; record min and max
      (if (or (not min) (< ref min)) (setq min ref))
      (if (or (not max) (> ref max)) (setq max ref))

      ;; count
      (setq total (1+ total))

      (forward-line))

    (setq message (format "Found %d references from %s to %s."
                               total
                               (format org-index--ref-format min)
                               (format org-index--ref-format max)))

    (goto-char org-index--below-hline)
    message))


(defun org-index--do-add-or-update (link)
  "For current node (with id LINK): add a new line to index table."

  (let ((args (list 'ref t 'link link))
        content ref yank)

    (unless (org-at-heading-p)
      (error "Not at headline"))

    (setq ref (org-entry-get (point) "org-index-ref"))

    ;; some fields want to be edited
    (dolist (col-num org-index--columns)

      (setq content "")

      ;; copy heading ?
      (if (org-index--flag-p 'get-heading-on-add (car col-num))
          (setq content (nth 4 (org-heading-components))))

      ;; copy category ?
      (if (org-index--flag-p 'get-category-on-add (car col-num))
          (setq content org-index--category-before))

      ;; Shift ref and timestamp ?
      (if (org-index--flag-p 'shift-ref-and-date-on-add (car col-num))
          (dotimes (i 2)
            (if (or (string-match (concat "^\\s-*" org-index--ref-regex) content)
                    (string-match (concat org-ts-regexp-both) content))
                (setq content (substring content (match-end 0))))))
      
      (if (and (not ref) ; do not edit if heading has already been added
               (org-index--flag-p 'edit-on-add (car col-num)))
          (setq content (read-from-minibuffer
                         (format "Edit text for column '%s': " (symbol-name (car col-num)))
                         content)))

      (if (not (string= content ""))
          (setq args (append (list (car col-num) content) args))))

    (if ref
        ;; already have a ref, find it in index and update fields
        (let ((kvs args)
              found-and-message)

          (with-current-buffer org-index--buffer
            (save-excursion

              (goto-char org-index--below-hline)
              (setq found-and-message (org-index--find-in-index ref))
              (unless (car found-and-message) (error (cdr found-and-message)))
              (setq yank (org-index--get-or-set-field (org-index--special-column 'yank-after-add)))
              ;; put collected info into existing table row
              (while kvs
                (unless (eq (car kvs) 'ref)
                  (org-index--get-or-set-field (car kvs) (org-trim (cadr kvs))))
                (setq kvs (cddr kvs)))))
          
          (cons (format "Updated index line %s" ref)
                yank))
      

      ;; no ref here, create new line in index
      (let (ref-and-yank)
        
        ;; new line in index table
        (setq ref-and-yank (apply 'org-index--do-new-line args))

        ;; insert reference
        (org-entry-put (point) "org-index-ref" (car ref-and-yank))

        (cons (format "Added index line %s" (car ref-and-yank)) (cdr ref-and-yank))))))


(defun org-index--do-delete ()
  "Perform command delete."

  (unless (org-at-heading-p)
    (error "Not at headline"))

  (let ((ref (org-entry-get (point) "org-index-ref")))

    ;; delete from index table
    (org-index--get-or-delete-line 'delete 'ref ref)

    ;; delete from property
    (org-entry-delete (point) "org-index-ref")

    ;; maybe delete from heading
    (save-excursion
      (end-of-line)
      (let ((end (point)))
        (beginning-of-line)
        (when (search-forward ref end t)
          (delete-char (- (length ref)))
          (just-one-space))))

    (format "Deleted index line %s" ref)))


(defun org-index--find-in-index (search &optional search-link)
  "Find index line with ref or link SEARCH (decided by SEARCH-LINK); return boolean and message."
  (let ((initial (point))
        found text)

    (forward-line -1)
    (while (and (not found)
                (forward-line)
                (org-at-table-p))
      (save-excursion
        (setq found
              (string= search
                       (org-index--get-or-set-field
                        (if search-link 'link 'ref))))))

    (if found
        (progn
          (setq text (format "Found index line '%s'" search))
          (org-table-goto-column (org-index--column-num 'ref))
          (if (looking-back " ") (backward-char))
          ;; remember string to copy
          (setq org-index--text-to-yank
                (org-trim (org-table-get-field (org-index--column-num 'copy)))))
      (setq text (format "Did not find index line '%s'" search))
      (goto-char initial)
      (forward-line))
    (cons found text)))


(defun org-index--do-head (ref link &optional other no-message)
  "Perform command head: Find node with REF or LINK and present it; if OTHER in separate window; if NO-MESSAGE, do not prepare or return message."

  (if ref (setq org-index--last-ref ref))
  (let (message marker)

    ;; Prefer link if available
    (if link
        (setq marker (org-id-find link t))
      (setq marker
            (catch 'found
              (message (format "Scanning headlines for '%s' ..." ref))
              (org-map-entries
               (lambda ()
                 (when (string= ref (org-entry-get (point) "org-index-ref"))
                   (throw 'found (point-marker))))
               nil 'agenda)
              nil)))

    (if no-message
        ;; return t or nil and set buffer
        (if marker
            (progn
              (set-buffer (marker-buffer marker))
              (goto-char marker)
              t)
          nil)
      ;; return message
      (if marker
          (progn
            (org-index--update-line (or link ref))
            (if link
                (setq message (format "Followed link %s to %s" link (or (org-entry-get (point) "org-index-ref") "unknown ref")))
              (setq message (format "Found headline %s" ref)))
            (let (cb)
              (if other
                  (progn
                    (setq cb (current-buffer))
                    (pop-to-buffer (marker-buffer marker)))
                (pop-to-buffer-same-window (marker-buffer marker)))
              
              (goto-char marker)
              (org-reveal t)
              (org-show-entry)
              (recenter)))
        (if link
            (setq message (format "Did not find link '%s'" link))
          (setq message (format "Did not find headline '%s'." ref))))
      message)))


(defun org-index--do-occur ()
  "Perform command occur."
  (let ((word "") ; last word to search for growing and shrinking on keystrokes
        (prompt "Search for: ")
        (lines-wanted (window-body-height))
        (lines-found 0)                      ; number of lines found
        words                                ; list words that should match
        occur-buffer
        begin ; position of first line
        narrow                         ; start of narrowed buffer
        help-text                      ; cons with help text short and long
        key-help                       ; for keys with special function
        search-text                    ; description of text to search for
        done                           ; true, if loop is done
        in-c-backspace                 ; true, while processing C-backspace
        show-headings                  ; true, if headings should be shown
        help-overlay                   ; Overlay with help text
        last-point                     ; Last position before end of search
        key                            ; input from user
        key-sequence)                  ; as a sequence

    
    ;; make and show buffer
    (if (get-buffer org-index--occur-buffer-name)
        (kill-buffer org-index--occur-buffer-name))
    (setq occur-buffer (make-indirect-buffer org-index--buffer org-index--occur-buffer-name))
    (pop-to-buffer-same-window occur-buffer)
    ;; avoid modifying direct buffer
    (setq buffer-read-only t)
    (toggle-truncate-lines 1)
    (setq font-lock-keywords-case-fold-search t)
    (setq case-fold-search t)

    ;; reset stack and overlays
    (setq org-index--occur-stack nil)
    (setq org-index--occur-tail-overlay nil)
    
    ;; narrow to table rows and one line before
    (goto-char (marker-position org-index--below-hline))
    (forward-line 0)
    (setq begin (point))
    (forward-line -1)
    (setq narrow (point))
    (while (org-at-table-p)
      (forward-line))
    (narrow-to-region narrow (point))
    (goto-char (point-min))
    (forward-line)

    ;; initialize help text
    (setq help-text (cons
                     "Incremental occur; `?' toggles help and headlines.\n"
                     (concat
                      (org-index--wrap
                       (concat
                        "Normal keys add to search word; <space> or <comma> start additional word; <backspace> erases last char, <C-backspace> last word; <return> jumps to heading, <tab> jumps to heading in other window; all other keys end search.\n"))
                      org-index--headings)))
    
    ;; insert overlays for help text and to cover unsearched lines
    (setq help-overlay (make-overlay (point-min) begin))
    (overlay-put help-overlay 'display (car help-text))
    (overlay-put help-overlay 'face 'org-agenda-dimmed-todo-face)
    (setq org-index--occur-tail-overlay (make-overlay (point-max) (point-max)))
    (overlay-put org-index--occur-tail-overlay 'invisible t)

    (while (not done)

      (if in-c-backspace
          (setq key "<backspace>")
        (setq search-text (mapconcat 'identity (reverse (cons word words)) ","))
        ;; read key
        (setq key-sequence
              (vector (read-key
                       (format "%s%s%s"
                               prompt
                               search-text
                               (if (string= search-text "") "" " ")))))
        (setq key (key-description key-sequence)))

      (cond


       ((string= key "<C-backspace>")
        (setq in-c-backspace t))


       ((member key (list "<backspace>" "DEL"))   ; erase last char

        (if (= (length word) 0)

            ;; nothing more to delete from current word; try next
            (progn
              (setq word (car words))
              (setq words (cdr words))
              (setq in-c-backspace nil))

          ;; unhighlight longer match
          (unhighlight-regexp (regexp-quote word))

          ;; some chars are left; shorten word
          (setq word (substring word 0 -1))
          (when (= (length word) 0) ; when nothing left, use next word from list
            (setq word (car words))
            (setq words (cdr words))
            (setq in-c-backspace nil))

          ;; free top list of overlays and remove list
          (setq lines-found (or (org-index--unhide) lines-wanted))
          (move-overlay org-index--occur-tail-overlay
                        (if org-index--occur-stack (cdr (assoc :end-of-visible (car org-index--occur-stack)))
                          (point-max))
                        (point-max))
        
                
          ;; highlight shorter word
          (unless (= (length word) 0)
            (highlight-regexp (regexp-quote word) 'isearch))

          ;; make sure, point is still visible
          (goto-char begin)))


       ((member key (list "SPC" ",")) ; space or comma: enter an additional search word

        ;; push current word and clear, no need to change display
        (setq words (cons word words))
        (setq word ""))


       ((string= key "?") ; question mark: toggle display of headlines and help
        (setq help-text (cons (cdr help-text) (car help-text)))
        (overlay-put help-overlay 'display (car help-text)))

       ((and (= (length key) 1)
             (aref printable-chars (elt key 0))) ; any printable char: add to current search word

        ;; unhighlight short word
        (unless (= (length word) 0)
          (unhighlight-regexp (regexp-quote word)))

        ;; add to word
        (setq word (concat word key))
                
        ;; make overlays to hide lines, that do not match longer word any more
        (goto-char begin)
        (setq lines-found (org-index--hide-with-overlays (cons word words) lines-wanted))
        (move-overlay org-index--occur-tail-overlay
                      (if org-index--occur-stack (cdr (assoc :end-of-visible (car org-index--occur-stack)))
                        (point-max))
                      (point-max))
        
        (goto-char begin)
                
        ;; highlight longer word
        (highlight-regexp (regexp-quote word) 'isearch)

        ;; make sure, point is on a visible line
        (line-move -1 t)
        (line-move 1 t))

       ;; anything else terminates loop
       (t (setq done t))))

    ;; put back input event, that caused the loop to end
    (unless (string= key "C-g")
      (setq unread-command-events (listify-key-sequence key-sequence))
      (message key))
    
    ;; postprocessing
    (setq last-point (point))
    
    ;; For performance reasons do not show matching lines for rest of table. So not code here.
    
    ;; make permanent copy
    ;; copy visible lines
    (let ((lines-collected 0)
          keymap line all-lines)

      (setq cursor-type t)
      (goto-char begin)

      ;; collect all visible lines
      (while (and (not (eobp))
                  (< lines-collected lines-wanted))
        ;; skip over invisible lines
        (while (and (invisible-p (point))
                    (not (eobp)))
          (goto-char (1+ (overlay-end (car (overlays-at (point)))))))
        (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (unless (string= line "")
          (incf lines-collected)
          (setq all-lines (cons (concat line
                                        "\n")
                                all-lines)))
        (forward-line 1))
        
      (kill-buffer org-index--occur-buffer-name) ; cannot keep this buffer; might become stale soon

      ;; create new buffer
      (setq occur-buffer (get-buffer-create org-index--occur-buffer-name))
      (pop-to-buffer-same-window occur-buffer)
      (insert "\n")

      ;; prepare help text
      (setq org-index--occur-help-overlay (make-overlay (point-min) (point-max)))
      (setq org-index--occur-help-text
            (cons
             (org-index--wrap
              (concat "Search is done; `?' toggles help and headlines.\n"))
             (concat
              (org-index--wrap (format (concat "Search is done. "
                                               (if (< lines-collected lines-wanted)
                                                   " Showing all %d matches for "
                                                 " Showing one window of matches for ")
                                               "\"" search-text
                                               "\". <return> jumps to heading, <tab> jumps to heading in other window, <S-return> to matching line in index.\n" )
                                       (length all-lines)))
              org-index--headings)))
      
      (overlay-put org-index--occur-help-overlay 'display (car org-index--occur-help-text))
      (overlay-put org-index--occur-help-overlay 'face 'org-agenda-dimmed-todo-face)


      ;; insert into new buffer
      (save-excursion
        (apply 'insert (reverse all-lines))
        (if (= lines-collected lines-wanted)
            (insert "\n(more lines omitted)\n")))
      
      (org-mode)
      (setq truncate-lines t)
      (font-lock-fontify-buffer)

      ;; highlight words
      (setq case-fold-search t)
      (setq font-lock-keywords-case-fold-search t)
      (mapc (lambda (w) (unless (or (not w) (string= w "")) (highlight-regexp (regexp-quote w) 'isearch)))
            (cons word words))

      (setq buffer-read-only t)

      ;; install keyboard-shortcuts
      (setq keymap (make-sparse-keymap))
      (set-keymap-parent keymap org-mode-map)

      (mapc (lambda (x) (define-key keymap (kbd x)
                     (lambda () (interactive)
                       (message "%s" (org-index--occur-to-head)))))
            (list "<return>" "RET"))

      (define-key keymap (kbd "<tab>")
        (lambda () (interactive)
          (message (org-index--occur-to-head t))))
      
      (define-key keymap (kbd "<S-return>")
        (lambda () (interactive)
          (org-index 'enter)))
      
      (define-key keymap (kbd "?")
        (lambda () (interactive)
          (setq-local org-index--occur-help-text (cons (cdr org-index--occur-help-text) (car org-index--occur-help-text)))
          (overlay-put org-index--occur-help-overlay 'display (car org-index--occur-help-text))))
    
      (use-local-map keymap))))


(defun org-index--wrap (text)
  "Wrap TEXT at fill column."
  (with-temp-buffer
    (insert text)
    (fill-region (point-min) (point-max) nil t)
    (buffer-string)))


(defun org-index--occur-to-head (&optional other)
  "Helper for `org-index--occur', find heading with ref or link; if OTHER, in other window."
  (org-index--do-head (org-index--get-or-set-field 'ref)
                      (org-index--get-or-set-field 'link)
                      other))


(defun org-index--hide-with-overlays (words lines-wanted)
  "Hide text that is currently visible and does not match WORDS by creating overlays; leave LINES-WANTED lines visible."
  (let ((symbol (intern (format "org-index-%d" (length org-index--occur-stack))))
        (lines-found 0)
        (end-of-visible (point))
        overlay overlays start matched)

    ;; main loop
    (while (and (not (eobp))
                (< lines-found lines-wanted))

      ;; skip invisible lines
      (while (and (not (eobp))
                  (and
                   (invisible-p (point))
                   (< (point) (overlay-start org-index--occur-tail-overlay))))
        (goto-char (overlay-end (car (overlays-at (point))))))

      ;; find stretch of lines, that are currently visible but should be invisible now
      (setq matched nil)
      (setq start (point))
      (while (and (not (eobp))
                  (not
                   (and
                    (invisible-p (point))
                    (< (point) (overlay-start org-index--occur-tail-overlay))))
                  (not (and (org-index--test-words words)
                            (setq matched t)))) ; for its side effect
        (forward-line 1))

      ;; create overlay to hide this stretch
      (when (< start (point))           ; avoid creating an empty overlay
        (setq overlay (make-overlay start (point)))
        (overlay-put overlay 'invisible symbol)
        (setq overlays (cons overlay overlays)))

      ;; skip and count line, that matched
      (when matched
        (forward-line 1)
        (setq end-of-visible (point))
        (incf lines-found)))
    
    ;; put new list on top of stack
    (setq org-index--occur-stack
          (cons (list (cons :symbol symbol)
                      (cons :overlays overlays)
                      (cons :end-of-visible end-of-visible)
                      (cons :lines lines-found))
                org-index--occur-stack))

    ;; make lines invisible
    (add-to-invisibility-spec symbol)

    lines-found))


(defun org-index--unhide ()
  "Unhide text that does has been hidden by `org-index--hide-with-overlays'."
  (when org-index--occur-stack
    ;; make text visible again
    (remove-from-invisibility-spec (cdr (assoc :symbol (car org-index--occur-stack))))
    ;; delete overlays
    (mapc (lambda (y)
            (delete-overlay y))
          (cdr (assoc :overlays (car org-index--occur-stack))))
          ;; remove from stack
    (setq org-index--occur-stack (cdr org-index--occur-stack))
    ;; return number of lines, that are now visible
    (if org-index--occur-stack (cdr (assoc :lines (car org-index--occur-stack))))))


(defun org-index--test-words (words)
  "Test current line for match against WORDS."
  (let (line)
    (setq line (downcase (buffer-substring (line-beginning-position) (line-beginning-position 2))))
    (catch 'not-found
      (dolist (w words)
        (or (search w line)
            (throw 'not-found nil)))
      t)))


(defun org-index--create-new-line (create-ref)
  "Do the common work for `org-index-new-line' and `org-index'.  CREATE-REF asks for new reference."

  (let (new)

    (when create-ref

      ;; construct new reference
      (unless new
        (setq new (format "%s%d%s" org-index--head (1+ org-index--maxref) org-index--tail)))

      ;; remember for org-mark-ring-goto
      (setq org-index--text-to-yank new))

    ;; insert ref or link as last or first line, depending on sort-column
    (goto-char org-index--below-hline)
    (if (eq (org-index--special-column 'sort) 'count)
        (progn
          (while (org-at-table-p)
            (forward-line))
          (forward-line -1)
          (org-table-insert-row t))
      (org-table-insert-row))

    ;; insert some of the standard values
    (org-table-goto-column (org-index--column-num 'created))
    (org-insert-time-stamp nil nil t)
    (org-table-goto-column (org-index--column-num 'count))
    (insert "1")

    new))


(defun org-index--complete-links ()
  "Add links into index table."

  (goto-char org-index--below-hline)

  (let ((links-added 0)
        (ref-only 0)
        ref link)
    (while (org-at-table-p)
      (unless (org-index--get-or-set-field 'link)
        (setq ref (org-index--get-or-set-field 'ref))
        (unless ref (error "This line contains neither reference nor link"))
        (save-excursion
          (if (not (org-index--do-head ref nil nil t))
              (incf ref-only)
            (setq link (org-id-get-create))
            (incf links-added)))
        (org-index--get-or-set-field 'link link))
      (forward-line 1))
    (goto-char org-index--below-hline)
    (format "Added %d links and found %d references without corresponding node." links-added ref-only)))


(defadvice org-mark-ring-goto (after org-index--advice-text-to-yank activate)
  "Make text from `org-index' available for yank."
  (when org-index--text-to-yank
      (kill-new org-index--text-to-yank)
      (message (format "Ready to yank '%s'" org-index--text-to-yank))
      (setq org-index--text-to-yank nil)))


(provide 'org-index)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; lexical-binding: t
;; End:

;;; org-index.el ends here
