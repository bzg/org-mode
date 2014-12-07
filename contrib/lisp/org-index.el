;;; org-index.el --- A personal index for org and beyond

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; Version: 3.0
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
;;  Mark and find your favorite things and org-locations easily:
;;  Create and update an index table of references and links.  When
;;  searching, frequently used entries appear at the top and entering
;;  some keywords narrows down to matching entries only, so that the
;;  right one can be spotted easily.
;;
;;  References are essentially small numbers (e.g. "R237" or "-455-"),
;;  as created by this package; they are well suited to be used
;;  outside of org.  Links are normal org-mode links.
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
(defvar org-index--occur-follow-mode nil "True, if follow mode in occur-buffer is on.")
(defvar org-index--message-text nil "Text that was issued as an explanation; helpful for regression tests.")


;; static information for this program package
(defconst org-index--commands '(occur add delete head enter leave ref help example reorder sort multi-occur highlight statistics) "List of commands available.")
(defconst org-index--required-flags '(sort) "Flags that are required.")
(defconst org-index--single-flags '(sort point-on-add yank-after-add) "Flags, that may only appear once; these can appear as special-columns.")
(defconst org-index--multiple-flags '(edit-on-add get-category-on-add get-heading-on-add) "Flags, that might appear multiple times.")
(defconst org-index--all-flags (append org-index--single-flags org-index--multiple-flags) "All flags.")
(defconst org-index--valid-headings '(ref link created last-accessed count keywords) "All valid headings.")
(defconst org-index--required-headings org-index--valid-headings "All required headings.")
(defconst org-index--sample-flags
"
  - columns-and-flags :: associate columns of index table with flags
    - ref
      - yank-after-add
    - .category
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
    - all-columns-explained :: All columns of the index table and their meaning
      - ref :: The reference number; will be generated automatically
      - link :: link to the node, that this line represents
      - created :: When has this entry been created ?
      - last-accessed :: When has this entry been accessed last ?
      - count :: How many times has this entry been picked ?
      - keywords :: List of keywords, which may match your input during occur
    - all-flags-explained :: All flags, that can be associated with columns
      - sort :: Sort whole table after this column
      - yank-after-add :: Let this column be yanked after picking this line
      - edit-on-add :: Edit this line when adding a new one
      - point-on-add :: Point will land here, when adding a new line
      - get-category-on-add :: This column will receive the nodes category during add
      - get-heading-on-add :: This column will receive the nodes heading during add"
"A sample string of flags.")


(defun org-index (&optional command)
  "Mark and find your favorite things and org-locations easily:
Create and update an index table of references and links.  When
searching, frequently used entries appear at the top and entering
some keywords narrows down to matching entries only, so that the
right one can be spotted easily.

References are essentially small numbers (e.g. \"R237\" or \"-455-\"),
as created by this package; they are well suited to be used
outside of org.  Links are normal `org-mode' links.


This is version 3.0 of org-index.el .


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
    list of words seperated by space or comma (\",\"), to select
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

  reorder: Temporarily reorder the index table, e.g. by count,
    reference or last access.

  sort: Sort a set of lines (either from the active region or the
    whole buffer) by references found in each line.

  multi-occur: Apply Emacs standard `multi-occur' operation on all
    `org-mode' buffers to search for the given reference.

  highlight: Highlight or unhiglight references in active region or buffer.

  statistics : Show some statistics (e.g. minimum and maximum
    reference) about index table.

If you invoke `org-index' for the first time, an assistant will be
invoked, that helps you to create your own, commented index.

Use `org-index-default-keybindings' to establish convenient
keyboard shortcuts.

Optional argument COMMAND is a symbol naming the command to execute."

  (interactive "P")

  (let ((org-index--silent nil)      ; t, if user can be asked
        prefix-arg                   ; prefix arg
        link-id                      ; link of starting node, if required
        search                       ; what to search for
        guarded-search               ; with guard against additional digits
        search-ref                   ; search, if search is a reference
        search-link                  ; search, if search is a link
        reorder-once        ; column to use for single time sorting
        kill-new-text       ; text that will be appended to kill ring
        initial-ref-or-link ; initial position in index table
        message-text)       ; text that will be issued as an explanation


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
    ;; Find out, what we are supposed to do
    ;;

    (when (equal command '(4))
      (setq prefix-arg command)
      (setq command nil))

    (if command
        (unless (memq command org-index--commands)
          (error "Unknown command '%s' passed as argument, valid choices are any of these symbols: %s"
                 command (mapconcat 'symbol-name org-index--commands ",")))
      (let ((r (org-index--read-command))) ; query user if not from argument
        (setq command (car r))
        (setq reorder-once (cdr r))))


    ;;
    ;; Get search, if required
    ;;

    ;; These actions need a search string:
    (when (memq command '(enter head))
      ;; Maybe we've got a search string from the arguments
      (setq search (org-index--get-or-read-search search command))

      (when search
        (when (string-match org-index--ref-regex search)
          (setq search-ref search)
          (setq guarded-search (org-index--make-guarded-search search)))
        (when (string-match "^[a-fA-F0-9]\\{8\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{12\\}$" search)
          (setq search-link search))))


    ;;
    ;; Check for invalid combinations of arguments; try to be helpful
    ;;

    (when (and (eq command 'head)
               (not search-ref)
               (not search-link))
      (error "Can do 'head' only for a reference or link (not '%s'), try 'occur' to search for text" search))


    ;;
    ;; Sort and enter table
    ;;

    ;; Get link if required before moving in
    (if (eq command 'add)
        (setq link-id (org-id-get-create)))

    ;; Save initial ref or link for later return
    (if (and org-index--within-node
             (org-at-table-p))
        (setq initial-ref-or-link
              (or (org-index--get-field 'ref)
                  (org-index--get-field 'link))))

    ;; These commands enter index table only temporarily
    (when (memq command '(occur multi-occur statistics example))

      (set-buffer org-index--buffer)
      (goto-char org-index--point)

      ;; Sort and align
      (org-index--sort reorder-once)
      (org-index--align))

    ;; These commands will leave user in index table after they are finished
    (when (memq command '(enter ref))

      ;; Support orgmode-standard of going back (buffer and position)
      (org-mark-ring-push)

      (org-pop-to-buffer-same-window org-index--buffer)
      (goto-char org-index--point)
      (org-index--unfold-buffer)

      ;; Sort and align
      (org-index--sort reorder-once)
      (org-index--align)

      ;; Remember position for leave
      (if org-index--point-before
          (setq org-index--point-saved org-index--point-before)))

    ;; prepare to return to initial position in index table
    (when initial-ref-or-link
      (while (and (org-at-table-p)
                  (not (or
                        (string= initial-ref-or-link (org-index--get-field 'ref))
                        (string= initial-ref-or-link (org-index--get-field 'link)))))
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

      ;; Position point in index buffer on reference to search for
      (goto-char org-index--below-hline)
      (let (found (initial (point)))
        (while (and (not found)
                    (forward-line)
                    (org-at-table-p))
          (save-excursion
            (setq found (string= search
                                 (org-index--get-field 'ref)))))
        (if found
            (org-index--update-line nil)
          (goto-char initial)))

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

      (setq kill-new-text (org-index--do-add link-id)))


     ((eq command 'delete)

      (org-index--do-delete))


     ((eq command 'head)

      (let (link)
        (if (and org-index--within-node
                 (org-at-table-p))
            (setq link (org-index--get-field 'link))))

      (setq message-text (org-index--do-head search-ref search-link)))


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
      (when org-index--point-saved
        ;; buffer displayed in window need to set point there first
        (if (eq (window-buffer org-index--active-window-index)
                org-index--buffer)
            (set-window-point org-index--active-window-index (marker-position org-index--point-saved)))
        ;; set position in buffer in any case and second
        (with-current-buffer org-index--buffer
          (goto-char org-index--point-saved)))
      (setq org-index--point-saved nil))


     ((eq command 'enter)

      ;; Go downward in table to requested reference
      (goto-char org-index--below-hline)
      (if search
          (let (found (initial (point)))
            (while (and (not found)
                        (forward-line)
                        (org-at-table-p))
              (save-excursion
                (setq found
                      (string= search
                               (org-index--get-field
                                (if search-link 'link 'ref))))))
            (if found
                (progn
                  (setq message-text (format "Found '%s'" search))
                  (org-index--update-line nil)
                  (org-table-goto-column (org-index--column-num 'ref))
                  (if (looking-back " ") (backward-char))
                  ;; remember string to copy
                  (setq org-index--text-to-yank
                        (org-trim (org-table-get-field (org-index--column-num 'copy)))))
              (setq message-text (format "Did not find '%s'" search))
              (goto-char initial)
              (forward-line)
              (setq command 'missed)))

        ;; simply go into table
        (setq message-text "At index table"))

      (recenter))


     ((eq command 'occur)

      (org-index--do-occur))


     ((eq command 'ref)

      (let (new)

        ;; add a new row
        (setq new (org-index--create-new-line (eq command 'ref)))

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

      ;; sort lines according to contained reference
      (let (begin end where)
        (catch 'aborted
          ;; either active region or whole buffer
          (if (and transient-mark-mode
                   mark-active)
              ;; sort only region
              (progn
                (setq begin (region-beginning))
                (setq end (region-end))
                (setq where "region"))
            ;; sort whole buffer
            (setq begin (point-min))
            (setq end (point-max))
            (setq where "whole buffer")
            ;; make sure
            (unless (y-or-n-p "Sort whole buffer? ")
              (setq message-text "Sort aborted")
              (throw 'aborted nil)))

          (save-excursion
            (save-restriction
              (goto-char (point-min))
              (narrow-to-region begin end)
              (sort-subr nil 'forward-line 'end-of-line
                         (lambda ()
                           (if (looking-at (concat ".*"
                                                   (org-index--make-guarded-search org-index--ref-regex 'dont-quote)))
                               (string-to-number (match-string 1))
                             0))))
            (highlight-regexp org-index--ref-regex 'isearch)
            (setq message-text (format "Sorted %s from character %d to %d, %d lines"
                                       where begin end
                                       (count-lines begin end)))))))



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


     ((eq command 'statistics)

      (setq message-text (org-index--do-statistics)))


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

Establish the common prefix key `C-c i' which is followed by the
first letter of a subcommand, so that `C-c i a' invokes the
subcommand \"add\". Subcommands available are occur, add, delete,
head, enter, leave and ref. As a special case `C-c i i' invokes
`org-index' to let you choose."
  (define-prefix-command 'org-index-map)
  (global-set-key (kbd "C-c i") 'org-index-map)
  (define-key org-index-map (kbd "i") (lambda () (interactive) (org-index)))
  (define-key org-index-map (kbd "o") (lambda () (interactive) (org-index 'occur)))
  (define-key org-index-map (kbd "a") (lambda () (interactive) (org-index 'add)))
  (define-key org-index-map (kbd "d") (lambda () (interactive) (org-index 'delete)))
  (define-key org-index-map (kbd "h") (lambda () (interactive) (org-index 'head)))
  (define-key org-index-map (kbd "e") (lambda () (interactive) (org-index 'enter)))
  (define-key org-index-map (kbd "l") (lambda () (interactive) (org-index 'leave)))
  (define-key org-index-map (kbd "r") (lambda () (interactive) (org-index 'ref))))


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

        ;; check arguments early
        (let ((kvs keys-values)
              k v)
          (while kvs
            (setq k (car kvs))
            (setq v (cadr kvs))
            (if (eq k 'ref)
                (unless (memq v '(t nil))
                  (error "Column 'ref' accepts only t or nil"))
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
              (setq n (org-index--column-num k))
              (org-table-goto-column n)
              (insert v)
              (setq kvs (cddr kvs))))

          ;; get column to yank
          (setq yank (org-trim (org-table-get-field (org-index--column-num (org-index--special-column 'yank-after-add)))))

          (org-index--sort)
          (cons ref yank))))))


(defun org-index-get-line (type value)
  "Retrieve an existing line within the index table by ref or link.
Return its contents as a property list.

The function `plist-get' may be used to retrieve specific elements
from the result.

Example:

  (plist-get (org-index-get-line 'ref \"12\") 'count)

retrieves the value of the count-column for reference 12.

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
        (goto-char org-index--point)

        (goto-char org-index--below-hline)
        (while (and (not found)
                    (org-at-table-p))
          (when (string= (org-index--get-field type)
                         value)
            ;; found matching line
            (if (eq command 'get)
                ;; get its fields
                (mapc (lambda (x)
                        (if (and (numberp (cdr x))
                                 (> (cdr x) 0))
                            (setq found (cons (car x) (cons (or (org-index--get-field (car x)) "") found)))
                          )) (reverse org-index--columns))
              ;; or delete it
              (let ((start (point)))
                (beginning-of-line)
                (forward-line)
                (delete-region start (point)))))
          (forward-line))))
    found))


(defun org-index--read-command ()
  "Find out, what we are supposed to do."

  (let (reorder-once    ; Column to use for single time sorting
        command
        input)

    ;; Ask user, what to do
    (while (progn

             (setq input
                   (org-completing-read
                    "Please choose: "
                    (mapcar 'symbol-name org-index--commands)
                    nil nil))

             ;; convert to symbol
             (setq command (intern input))

             ;; ask for reorder in loop, because we may have to ask for command right again
             (if (eq command 'reorder)
                 (setq reorder-once
                       (intern
                        (org-icompleting-read
                         "Please choose column to reorder index table once: "
                         (list "ref" "count" "created" "last-accessed")
                         nil t))))

             ;; maybe ask initial question again
             (eq command 'reorder)))
    (cons command reorder-once)))


(defun org-index--get-or-read-search (search command)
  "Get SEARCH string, maybe read from user; respect COMMAND that will be executed."

  (let (search-from-table
        search-from-cursor)

    (unless search
      ;; Search string can come from several sources:
      ;; From link or ref columns of table
      (when (and org-index--within-node
                 (org-at-table-p))
        (setq search-from-table (or (org-index--get-field 'link)
                                    (org-index--get-field 'ref))))

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


    ;; If we still do not have a search string, ask user explicitly
    (unless search

      (if org-index--silent (error "Need to specify search, if silence is required"))

      (unless (eq command 'occur)

        (setq search (read-from-minibuffer
                      (cond ((eq command 'head)
                             "Text or reference number to search for: ")
                            ((eq command 'enter)
                             "Reference number to search for (or <empty> for id of current node, `l' for last ref created, `t' for top of index table): "))))))

    ;; Check for special case
    (when (eq command 'enter)
      (if (string= search "")  (setq search (org-id-get)))
      (if (string= search "t") (setq search nil))
      (if (string= search "l") (setq search (number-to-string org-index--maxref))))

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
                  (not (setq ref-field (org-index--get-field 'ref))))
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

          (setq ref-field (org-index--get-field 'ref))
          (setq link-field (org-index--get-field 'link))

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


(defun org-index--sort (&optional sort-column)
  "Sort index table maybe according to SORT-COLUMN."

  (unless sort-column (setq sort-column (org-index--special-column 'sort)))

  (let ((is-modified (buffer-modified-p))
        top
        bottom
        ref-field
        count-field)

    (unless buffer-read-only

      ;; get boundaries of table
      (goto-char org-index--below-hline)
      (forward-line 0)
      (setq top (point))
      (while (org-at-table-p) (forward-line))

      ;; Kill all empty rows at bottom
      (while (progn
               (forward-line -1)
               (org-table-goto-column 1)
               (and
                (not (org-index--get-field 'ref))
                (not (org-index--get-field 'link))))
        (org-table-kill-row))
      (forward-line 1)
      (setq bottom (point))

      (save-restriction
        (narrow-to-region top bottom)
        (goto-char top)
        (sort-subr t
                   'forward-line
                   'end-of-line
                   (lambda ()
                     (let (ref
                           (ref-field (or (org-index--get-field 'ref) ""))
                           (count-field (or (org-index--get-field 'count) "")))

                       ;; get reference with leading zeroes, so it can be
                       ;; sorted as text
                       (string-match org-index--ref-regex ref-field)
                       (setq ref (format
                                  "%06d"
                                  (string-to-number
                                   (or (match-string 1 ref-field)
                                       "0"))))

                       ;; Construct different sort-keys according to
                       ;; requested sort column
                       (cond

                        ((eq sort-column 'count)
                         (concat (format
                                  "%08d"
                                  (string-to-number (or (org-index--get-field 'count)
                                                        "")))
                                 ref))

                        ((eq sort-column 'ref)
                         ref)

                        ((eq sort-column 'last-accessed)
                         (concat (org-index--get-field sort-column)
                                 " "
                                 ref))

                        (t (error "This is a bug: unmatched case '%s'" sort-column)))))

                   nil 'string<))

      ;; restore modification state
      (set-buffer-modified-p is-modified))))


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
          (error "Column name cannot be empty"))
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
        (org-index--report-index-error "'%s' is not a valid column" (cdr (assoc :text parent))))

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

  (if (looking-at org-list-full-item-re)

      ;; retrieve interesting parts of list item from match data
      (let (alist indent checkbox text value)

        (setq indent (save-excursion
                       (goto-char (match-beginning 1))
                       (current-column)))
        (decf indent (+ (save-match-data (org-current-level)) 1))
        (add-to-list 'alist (cons :indent indent))

        (setq checkbox (match-string 3))

        (setq text (match-string 4))
        (setq value (buffer-substring
                     (match-end 0)
                     (save-excursion (end-of-line) (point))))

        (when (not text)
          (setq text value)
          (setq value nil))

        (add-to-list 'alist (cons :text text))
        (add-to-list 'alist (cons :value value))

        (add-to-list 'alist (cons :sym (intern text)))

        ;; clean up strings
        (mapc (lambda (x) (if (stringp (cdr x)) (setf (cdr x) (org-trim (substring-no-properties (cdr x)))))) alist)

        alist)

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


  | ref | .category | keywords | count | last-accessed | created | link |
  |     |           |          |       |               |         | <4>  |
  |-----+-----------+----------+-------+---------------+---------+------|
  | %s  |           | %s       |       |               | %s      | %s   |

"
                      org-index--sample-flags
                      firstref
                      "This node"
                      (with-temp-buffer (org-insert-time-stamp nil nil t))
                      id))

      (save-excursion(org-index--goto-list "columns-and-flags")
                              )
      ;; make sure, that node can be found
      (org-id-add-location id (buffer-file-name))
      (setq buffer-save-without-query t)
      (basic-save-buffer)

      (while (not (org-at-table-p)) (forward-line -1))
      (unless buffer-read-only (org-table-align))
      (while (not (org-at-heading-p)) (forward-line -1))

      ;; present results to user
      (if temporary
          (progn
            ;; Present existing and temporary index together
            (when compare
              (org-pop-to-buffer-same-window org-index--buffer)
              (goto-char org-index--point)
              (org-index--unfold-buffer)
              (delete-other-windows)
              (select-window (split-window-vertically)))
            ;; show new index
            (org-pop-to-buffer-same-window buffer)
            (org-id-goto id)
            (org-index--unfold-buffer)
            (if compare
                (error "Please compare your existing index (upper window) and a temporary new one (lower window) to fix your index")
              (message "This is your new temporary index.")))
        (progn
          ;; Only show the new index
          (org-pop-to-buffer-same-window buffer)
          (delete-other-windows)
          (org-id-goto id)
          (org-index--unfold-buffer)
          (setq org-index-id id)
          (if (y-or-n-p "This is your new index table.  It is already set for this Emacs session, so you may try it out.  Do you want to save its id to make it available for future Emacs sessions too ? ")
              (progn
                (customize-save-variable 'org-index-id id)
                (error "Saved org-index-id '%s' to %s" id custom-file))
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


(defun org-index--update-line (ref-or-link)
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
                      (not (or (string= ref-or-link (org-index--get-field 'ref))
                               (string= ref-or-link (org-index--get-field 'link)))))
            (forward-line)))

        (if (not (org-at-table-p))
            (error "Did not find reference or link '%s'" ref-or-link)
          (org-index--update-current-line))

        (if initial (goto-char initial))))))


(defun org-index--update-current-line ()
  "Update current lines columns count and last-accessed."
  (let (newcount (count-field (org-index--get-field 'count)))

    ;; update count field only if number or empty
    (when (or (not count-field)
              (string-match "^[0-9]+$" count-field))
      (setq newcount (+ 1 (string-to-number (or count-field "0"))))
      (org-index--get-field 'count
                            (number-to-string newcount)))

    ;; update timestamp
    (org-table-goto-column (org-index--column-num 'last-accessed))
    (org-table-blank-field)
    (org-insert-time-stamp nil t t)))


(defun org-index--get-field (key &optional value)
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
      (setq ref-field (org-index--get-field 'ref))
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

    message))


(defun org-index--do-add (link-id)
  "For current node (with id LINK-ID): add a new line to index table."

  (let ((args (list 'ref t 'link link-id))
        ref-and-yank content)

    (unless (org-at-heading-p)
      (error "Not at headline"))

    ;; some fields want to be edited
    (dolist (col-num org-index--columns)

      (setq content "")

      ;; copy heading ?
      (if (org-index--flag-p 'get-heading-on-add (car col-num))
          (setq content (nth 4 (org-heading-components))))

      ;; copy category ?
      (if (org-index--flag-p 'get-category-on-add (car col-num))
          (setq content org-index--category-before))

      (if (org-index--flag-p 'edit-on-add (car col-num))
          (read-from-minibuffer
           (format "Edit text for column '%s': " (symbol-name (car col-num)))
           content))

      (if (not (string= content ""))
          (setq args (append (list (car col-num) content) args))))

    ;; new line in index table
    (setq ref-and-yank (apply 'org-index--do-new-line args))

    ;; insert reference
    (org-entry-put (point) "org-index-ref" (car ref-and-yank))

    (cdr ref-and-yank)))


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
          (just-one-space))))))


(defun org-index--do-head (ref link &optional other)
  "Perform command head: Find node with REF or LINK and present it; if OTHER in separate window."

  (if ref (setq org-index--last-ref ref))

  (let (message)
    ;; Use link if available
    (if link
        (progn
          (org-index--update-line link)
          (org-id-goto link)
          (org-reveal)
          (setq message "Followed link"))

      (message (format "Scanning headlines for '%s' ..." ref))
      (org-index--update-line ref)
      (let ((search (concat ".*" (org-index--make-guarded-search ref)))
            (org-trust-scanner-tags t)
            buffer point)
        (if (catch 'found
              (progn
                ;; loop over all headlines, stop on first match
                (org-map-entries
                 (lambda ()
                   (when (or (looking-at search)
                             (eq ref (org-entry-get (point) "org-index-ref")))
                     ;; If this is not an inlinetask ...
                     (when (< (org-element-property :level (org-element-at-point))
                              org-inlinetask-min-level)
                       ;; ... remember location and bail out
                       (setq buffer (current-buffer))
                       (setq point (point))
                       (throw 'found t))))
                 nil 'agenda)
                nil))

            (progn
              (setq message (format "Found '%s'" (or ref link)))
              (if other
                  (progn
                    (pop-to-buffer buffer)
                    (goto-char point)
                    (org-reveal t)
                    (recenter)
                    (pop-to-buffer "*org-index-occur*"))
                (org-pop-to-buffer-same-window buffer)
                (goto-char point)
                (org-reveal t)
                (recenter)))
          (setq message (format "Did not find '%s'" (or ref link))))))
    message))


(defun org-index--do-occur ()
  "Perform command occur."
  (let ((occur-buffer-name "*org-index-occur*")
        (word "") ; last word to search for growing and shrinking on keystrokes
        (prompt "Search for: ")
        (hint "")
        (key-help "<up>, <down> move. <return> finds node, <tab> finds in other window.\n")
        words                      ; list of other words that must match too
        occur-buffer
        lines-to-show              ; number of lines to show in window
        start-of-lines             ; position, where lines begin
        start-of-help              ; start of displayed help (if any)
        left-off-at                ; stack of last positions in index table
        after-inserted             ; in occur-buffer
        at-end                     ; in occur-buffer
        lines-visible              ; in occur-buffer
        below-hline-bol            ; below-hline and at bol
        exit-gracefully            ; true if normal exit
        in-c-backspace             ; true while processing C-backspace
        show-headings              ; true, if headings should be shown
        fun-on-ret                 ; function to be executed, if return is pressed
        fun-on-s-ret               ; shift
        fun-on-m-ret               ; shift
        fun-on-tab                 ; function to be executed, if tab is pressed
        ret from to key)

    ;; clear buffer
    (if (get-buffer "*org-index-occur*")
        (kill-buffer occur-buffer-name))
    (setq occur-buffer (get-buffer-create "*org-index-occur*"))

    ;; install keyboard-shortcuts within occur-buffer
    (with-current-buffer occur-buffer
      (let ((keymap (make-sparse-keymap)))

        (set-keymap-parent keymap org-mode-map)
        (setq fun-on-ret (lambda () (interactive) (org-index--occur-find-heading nil)))
        (define-key keymap [return] fun-on-ret)
        (setq fun-on-tab (lambda () (interactive)
                           (org-index--occur-find-heading t)
                           (setq org-index--occur-follow-mode (not org-index--occur-follow-mode))))
        (define-key keymap [tab] fun-on-tab)
        (define-key keymap [(control ?i)] fun-on-tab)
        (define-key keymap [up] (lambda () (interactive)
                                          (forward-line -1)
                                          (if org-index--occur-follow-mode (org-index--occur-find-heading t))))
        (define-key keymap [down] (lambda () (interactive)
                                            (forward-line 1)
                                            (if org-index--occur-follow-mode (org-index--occur-find-heading t))))
        (use-local-map keymap)))

    (with-current-buffer org-index--buffer
      (let ((initial (point)))
        (goto-char org-index--below-hline)
        (forward-line 0)
        (setq below-hline-bol (point))
        (goto-char initial)))

    (org-pop-to-buffer-same-window occur-buffer)
    (toggle-truncate-lines 1)

    (unwind-protect          ; to reset cursor-shape even in case of errors
        (progn

          ;; fill in header
          (erase-buffer)
          (insert (concat "Incremental search, showing one window of matches. '?' toggles help.\n\n"))
          (setq start-of-lines (point-marker))
          (setq start-of-help start-of-lines)
          (setq cursor-type 'hollow)

          ;; get window size of occur-buffer as number of lines to be searched
          (setq lines-to-show (+ (- (window-body-height) (line-number-at-pos)) 1))

          ;; fill initially
          (setq ret (org-index--get-matching-lines nil lines-to-show below-hline-bol))
          (when (nth 0 ret)
            (insert (nth 1 ret))
            (setq left-off-at (cons (nth 0 ret) nil))
            (setq after-inserted (cons (point) nil)))

          ;; read keys
          (while
              (progn
                (goto-char start-of-lines)
                (setq lines-visible 0)

                (if in-c-backspace
                    (setq key 'backspace)
                  (let ((search-text (mapconcat 'identity (reverse (cons word words)) ",")))
                    (setq key (read-key
                               (format "%s%s%s%s"
                                       prompt
                                       search-text
                                       (if (string= search-text "") "" " ")
                                       hint))))
                  (setq hint "")
                  (setq exit-gracefully (member key (list 'up 'down 'left 'right 'RET ?\C-g ?\C-m
                                                          'C-return 'S-return ?\C-i 'TAB))))


                (not exit-gracefully))

            (cond

             ((eq key 'C-backspace)

              (setq in-c-backspace t))

             ((member key (list 'backspace 'deletechar ?\C-?))           ; erase last char

              (if (= (length word) 0)

                  ;; nothing more to delete from current word; try next
                  (progn
                    (setq word (car words))
                    (setq words (cdr words))
                    (setq in-c-backspace nil))

                ;; unhighlight longer match
                (let ((case-fold-search t))
                  (unhighlight-regexp (regexp-quote word)))

                ;; some chars are left; shorten word
                (setq word (substring word 0 -1))
                (when (= (length word) 0) ; when nothing left, use next word from list
                  (setq word (car words))
                  (setq words (cdr words))
                  (setq in-c-backspace nil))

                ;; remove everything, that has been added for char just deleted
                (when (cdr after-inserted)
                  (setq after-inserted (cdr after-inserted))
                  (goto-char (car after-inserted))
                  (delete-region (point) (point-max)))

                ;; back up last position in index table too
                (when (cdr left-off-at)
                  (setq left-off-at (cdr left-off-at)))

                ;; go through buffer and check, if any invisible line should now be shown
                (goto-char start-of-lines)
                (while (< (point) (point-max))
                  (if (outline-invisible-p)
                      (progn
                        (setq from (line-beginning-position)
                              to (line-beginning-position 2))

                        ;; check for matches
                        (when (org-index--test-words (cons word words) (buffer-substring from to))
                          (when (<= lines-visible lines-to-show) ; show, if more lines required
                            (outline-flag-region from to nil)
                            (incf lines-visible))))

                    ;; already visible, just count
                    (incf lines-visible))

                  (forward-line 1))

                ;; highlight shorter word
                (unless (= (length word) 0)
                  (let ((case-fold-search t))
                    (highlight-regexp (regexp-quote word) 'isearch)))))


             ((member key (list ?\s ?,)) ; space or comma: enter an additional search word

              ;; push current word and clear, no need to change display
              (setq words (cons word words))
              (setq word ""))


             ((eq key ??)    ; question mark: toggle display of headlines and help
              (setq show-headings (not show-headings))
              (goto-char start-of-lines)
              (if show-headings
                  (progn
                    (forward-line -1)
;                    (kill-line)
                    (setq start-of-help (point-marker))
                    (insert "Normal keys add to search word, SPACE or COMMA start new word, BACKSPACE and C-BACKSPACE erase char or word. Every other key ends search. ")
                    (insert key-help)
                    (goto-char start-of-help)
                    (fill-paragraph)
                    (goto-char start-of-lines)
                    (insert org-index--headings))
                (delete-region start-of-help start-of-lines)
                (insert "\n\n"))
              (setq start-of-lines (point-marker)))


             ((and (integerp key)
                   (aref printable-chars key)) ; any printable char: add to current search word

              ;; unhighlight short word
              (unless (= (length word) 0)
                (let ((case-fold-search t))
                  (unhighlight-regexp (regexp-quote word))))

              ;; add to word
              (setq word (concat word (char-to-string key)))

              ;; hide lines, that do not match longer word any more
              (while (< (point) (point-max))
                (unless (outline-invisible-p)
                  (setq from (line-beginning-position)
                        to (line-beginning-position 2))

                  ;; check for matches
                  (if (org-index--test-words (list word) (buffer-substring from to))
                      (incf lines-visible)            ; count as visible
                    (outline-flag-region from to t))) ; hide

                (forward-line 1))

              ;; duplicate top of stacks; eventually overwritten below
              (setq left-off-at (cons (car left-off-at) left-off-at))
              (setq after-inserted (cons (car after-inserted) after-inserted))

              ;; get new lines from index table
              (when (< lines-visible lines-to-show)
                (setq ret (org-index--get-matching-lines (cons word words)
                                                         (- lines-to-show lines-visible)
                                                         (car left-off-at)))

                (when (nth 0 ret)
                  (insert (nth 1 ret))
                  (setq at-end (nth 2 ret))
                  (setcar left-off-at (nth 0 ret))
                  (setcar after-inserted (point))))

              ;; highlight longer word
              (let ((case-fold-search t))
                (highlight-regexp (regexp-quote word) 'isearch)))


             (t                                   ; non-printable chars
              (setq hint (format "(cannot search for key '%s', use %s to quit)"
                                 (if (symbolp key)
                                     key
                                     (key-description (char-to-string key)))
                                 (substitute-command-keys "\\[keyboard-quit]"))))))

          ;; search is done collect and brush up results
          ;; remove any lines, that are still invisible
          (goto-char start-of-lines)
          (while (< (point) (point-max))
            (if (outline-invisible-p)
                (delete-region (line-beginning-position) (line-beginning-position 2))
              (forward-line 1))))

      ;; postprocessing even for non graceful exit
      (setq cursor-type t)
      ;; replace previous heading
      (let ((numlines (count-lines (point) start-of-lines)))
        (goto-char start-of-lines)
        (delete-region (point-min) (point))
        (insert (format  (concat (if exit-gracefully "Search is done;" "Search aborted;")
                                 (if at-end
                                     " showing all %d matches. "
                                   " showing one window of matches. ")
                                 key-help)
                         numlines))
        (insert "\n")
        (setq start-of-lines (point-marker))
        (goto-char (point-min))
        (fill-paragraph)
        (goto-char start-of-lines)
        (if show-headings (insert "\n\n" org-index--headings)))
      (forward-line))

    ;; perform action according to last char
    (forward-line -1)
    (cond

     ((member key (list 'RET ?\C-m))
      (funcall fun-on-ret))

     ((member key (list 'TAB ?\C-i))
      (funcall fun-on-tab))

     ((eq key 'up)
      (forward-line -1))

     ((eq key 'down)
      (forward-line 1)))))


(defun org-index--occur-find-heading (x)
  "Helper for keymap of occur: find heading, if X in other window."
  (interactive)
  (save-excursion
    (let ((ref (org-index--get-field 'ref))
          (link (org-index--get-field 'link)))
      (message (org-index--do-head ref link x)))))


(defun org-index--create-new-line (create-ref)
  "Do the common work for `org-index-new-line' and `org-index'.  CREATE-REF asks for new reference."

  (let (new)

    (when create-ref

      ;; construct new reference
      (unless new
        (setq new (format "%s%d%s" org-index--head (1+ org-index--maxref) org-index--tail)))

      ;; remember for org-mark-ring-goto
      (setq org-index--text-to-yank new))

    ;; insert ref or link as very first row
    (goto-char org-index--below-hline)
    (org-table-insert-row)

    ;; insert some of the standard values
    (org-table-goto-column (org-index--column-num 'created))
    (org-insert-time-stamp nil nil t)
    (org-table-goto-column (org-index--column-num 'count))
    (insert "1")

    new))


(defun org-index--get-matching-lines (words numlines start-from)
  "Helper for occur: search for WORDS and get NUMLINES lines from index table, starting at START-FROM."
  (let ((numfound 0)
        pos
        initial line lines at-end)

    (with-current-buffer org-index--buffer

      ;; remember initial pos and start at requested
      (setq initial (point))
      (goto-char start-from)

      ;; loop over buffer until we have found enough lines
      (while (and (or (< numfound numlines)
                      (= numlines 0))
                  (org-at-table-p))

        ;; check each word
        (setq line (buffer-substring (line-beginning-position) (line-beginning-position 2)))
        (when (org-index--test-words words line)
          (setq lines (concat lines line))
          (incf numfound))
        (forward-line 1)
        (setq pos (point)))

      (setq at-end (not (org-at-table-p)))

      ;; return to initial position
      (goto-char initial))

    (unless lines (setq lines ""))
    (list pos lines at-end)))


(defun org-index--test-words (words line)
  "Test LINE for match against WORDS."
  (let ((found-all t))
    (setq line (downcase line))
    (catch 'not-found
      (dolist (w words)
        (or (search w line)
            (throw 'not-found nil)))
      t)))


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
;; End:

;;; org-index.el ends here
