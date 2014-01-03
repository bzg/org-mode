;;; org-index.el --- A personal index for org and beyond

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; Keywords: outlines, hypermedia, matching
;; Requires: org
;; Version: 2.4.0

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
;;  Mark and find your favorite org-locations and other points of interest
;;  easily; create and update a lookup table of references and links. When
;;  searching, frequently used entries appear at the the top and entering
;;  some keywords narrows down to matching entries only, so that the
;;  right one can be spotted easily.
;;
;;  References are essentially small numbers (e.g. "R237" or "-455-"),
;;  which are created by this package; they are well suited to be used
;;  outside org. Links are normal org-mode links.
;;
;;
;; Setup:
;;
;;  - Add these lines to your .emacs:
;;
;;    ;; use the real path from your org-installation
;;    (add-to-list 'load-path "~/path/to/orgdir/contrib/lisp" t) 
;;    (require 'org-index)
;;
;;  - Restart your emacs to make these lines effective
;;
;;  - Invoke `org-index', which will assist in creating your index
;;    table. The variable org-index-id will be persisted within your
;;    customization file (typically .emacs).
;;
;;
;; Further reading:
;;
;;  See the documentation of `org-index', which can also be read
;;  by invoking `org-index' and and choosing the help-command.
;;
;;  For more documentation and working examples, see:
;;
;;    http://orgmode.org/worg/org-contrib/org-index.html
;;
;;
;; Updates:
;;
;;  The latest tested version of this file can always be found at:
;;
;;    http://orgmode.org/w/org-mode.git?p=org-mode.git;a=blob;f=contrib/lisp/org-index.el;hb=HEAD

;;; Change Log:

;;   [2014-01-02 Th] Version 2.4.0:
;;   - New command "put" to store a nodes reference in a property
;;   - New functions org-index-new-line and org-index-get-line 
;;     offer access to org-index from other lisp programs
;;   - New flags p,x1,x2 and x3
;;   - Major Code refactoring
;;   - Regression tests with ert
;;   - Lots of bugfixes
;;
;;   [2013-10-04 Fr] Version 2.3.2:
;;   - Bugfix: index-table created by assistant is found after 
;;     restart of emacs instead of invoking assistent again
;;
;;   [2013-07-20 Sa] Version 2.3.0:
;;    - Renamed from "org-favtable" to "org-index"
;;    - Added an assistent to set up the index table
;;    - occur is now incremental, searching as you type
;;    - simplified the documentation and help-system
;;    - Saving keystrokes, as "+g237" is now valid input
;;    - Many bugfixes
;;
;;   [2013-02-28 Th] Version 2.2.0:
;;    - Allowed shortcuts like "h237" for command "head" with argument "237"
;;    - Integrated with org-mark-ring-goto
;;
;;   [2013-01-25 Fr] Version 2.1.0:
;;    - Added full support for links
;;    - New commands "missing" and "statistics"
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
(defvar org-index--maxref)   ; Maximum number from reference table (e.g. "153")
(defvar org-index--head)     ; Any header before number (e.g. "R")
(defvar org-index--tail)     ; Tail after number (e.g. "}" or "")
(defvar org-index--numcols)  ; Number of columns in index table
(defvar org-index--ref-regex)      ; Regular expression to match a reference
(defvar org-index--has-reuse nil)  ; True, if table contains a line for reuse
(defvar org-index--ref-format)     ; Format, that can print a reference
(defvar org-index--columns nil)    ; Columns of index-table
(defvar org-index--special-columns nil)   ; Columns with flags
(defvar org-index--buffer)         ; Buffer of index table
(defvar org-index--point)          ; Position at start of headline of index table
(defvar org-index--below-hline)    ; Position of first cell in first line below hline
(defvar org-index--headings) ; Headlines of index-table as a string

;; Variables to hold context and state
(defvar org-index--last-action nil)         ; Last action performed by org-index
(defvar org-index--text-to-yank nil)        ; Text, that can be yanked after call (mostly a reference)
(defvar org-index--last-ref)       ; Last reference created or visited
(defvar org-index--point-before nil)        ; Point in buffer with index table
(defvar org-index--silent nil)     ; t, if user should not be queried
(defvar org-index--preferred-command)       ; command, that is presented first
(defvar org-index--active-region)  ; Active region, initially. I.e. what has been marked
(defvar org-index--below-cursor)   ; Word below cursor
(defvar org-index--within-node)    ; True, if we are within node of the index table
(defvar org-index--active-window-index nil) ; Active window with index table (if any)

(setq org-index--commands '(occur head ref link leave put enter goto help + reorder fill sort update highlight unhighlight missing statistics)) ; list of commands available

(defun org-index (&optional ARG)
  "Mark and find your favorite things and org-locations easily:
Create and update a lookup table of references and links. Often
used entries bubble to the top; entering some keywords narrows
down to matching entries only, so that the right one can be
spotted easily.

References are essentially small numbers (e.g. \"R237\" or \"-455-\"),
which are created by this package; they are well suited to be used
outside of org. Links are normal org-mode links.

This is version 2.4.0 of org-index.

The function `org-index' operates on a dedicated table, the index
table, which lives within its own Org-mode node. The table and
its node will be created, when you first invoke org-index.

Each line in the index table contains:

 - A reference (e.g. \"R237\")

 - An optional link to another location in org

 - A number, counting, how often each reference has been
   used. This number is updated automatically and the table can
   be sorted after it, so that most frequently used references
   appear at the top of the table and can be spotted easily.

 - The creation date of the line

 - Date and time of last access. This column can alternatively be
   used to sort the table.

 - A column for your own comments

The index table is found through the id of the containing
node; this id is stored within the variable `org-index-id'.


The function `org-index' is the only interactive function of this
package and its main entry point; it offers several commands to
create, find and look up line within the index table.

Commands known:

  occur: Incremental search, that shows matching lines from the
    index table, updated after every keystroke. You may enter a
    list of words seperated by space or comma (\",\"), to select
    lines that contain all of the given words.

    If you supply a number (e.g. \"237\"): Apply emacs standard
    multi-occur operation on all org-mode buffers to search for
    this specific reference.

    You may also read the note at the end of this help on saving
    the keystroke RET with this frequent default command.

  head: If invoked outside the index table, ask for a reference
    number and search for an entry, which either has this
    reference contained in its heading or within its property
    org-index-ref. If invoked from within the index table dont
    ask; rather use the reference or link from the current line.

  ref: Create a new reference, copy any previously selected text.
    If already within index table, fill in ref-column.

  link: Create a new line in index table with a link to the
    current node.  Do not populate the ref column; this can later
    be populated by calling the \"fill\" command from within the
    index table.

  leave: Leave the index table. If the last command has been
    \"ref\", the new reference is copied and ready to yank.  This
    \"org-mark-ring-goto\" and can be called several times in
    succession. If you invoke org-index with a prefix argument,
    this command \"leave\" is executed without further questions.

  put: Put the reference, that was created last, as the value of
    property org-index-ref into the current node. That way it can
    be found by a later call to \"head\". 

  enter: Just enter the node with the index table.

  goto: Enter index table and go to a specific reference.

  help: Show this text.

  +: Show all commands including the less frequently used ones
    given below. If \"+\" is followd by enough letters of such a
    command (e.g. \"+fi\"), then this command (e.g. \"fill\") is
    invoked directly.

  reorder: Temporarily reorder the index table, e.g. by count,
    reference or last access.

  fill: If either ref or link is missing in current line of index
    table, fill in the missing value.

  sort: Sort a set of lines (either from the active region or the
    whole buffer) by references found in each line.

  update: For the given reference, update the line in the
    index table, i.e. increment its count.

  highlight: Highlight references in active region or buffer.

  unhighlight: Remove those highlights.

  missing : Search for missing reference numbers (which do not
    appear in the reference table). If requested, add additional
    lines for them, so that the command \"ref\" is able to reuse
    them.

  statistics : Show some statistics (e.g. minimum and maximum
    reference) about index table.



Two ways to save keystrokes:

When prompting for a command, org-index puts the most likely
one (e.g. \"occur\" or \"ref\") in front of the list, so that
you may just type RET.

If this first command in the list of commands needs additional
input (like e.g. \"occur\"), you may supply this input right
away, although you are still beeing prompted for the command. So,
to do an occur for the string \"foo\", you can just enter \"foo\"
RET, without even typing \"occur\".


Another way to save keystrokes applies if you want to choose a
command, that requrires a reference number and would normally
prompt for it: In that case you may just enter enough characters
from your command, so that it appears first in the list of
matches; then immediately enter the number of the reference you
are searching for. So the input \"h237\" would execute the
command \"head\" for reference \"237\".

"

  (interactive "P")

  (let ((org-index--silent nil)      ; t, if user can be asked
        link-id                      ; link of starting node, if required
        what                         ; what to do
        search                       ; what to search for
        guarded-search               ; with guard against additional digits
        search-ref                   ; search, if search is a reference
        search-link                  ; search, if search is a link
        what-adjusted                ; true, if we had to adjust what
        what-input          ; Input on what question (need not necessary be "what")
        reorder-once        ; column to use for single time sorting
        kill-new-text       ; text that will be appended to kill ring
        message-text        ; text that will be issued as an explanation
        initial-ref-or-link ; initial position in index table
        )


    ;;
    ;; Initialize and parse
    ;;

    ;; creates index table, if necessary
    (org-index--verify-id) 

    ;; store context information
    (org-index--retrieve-context)

    ;; Get configuration of index table
    (org-index--parse-table)


    ;;
    ;; Find out, what we are supposed to do
    ;;

    (if ARG
        (if (equal ARG '(4))
            (setq what 'leave)
          (if (and (symbolp ARG)
                   (memq ARG org-index--commands))
              (setq what ARG)
            (error "Unknown command '%s' passed as argument, valid choices are a prefix argument or any of these symbols: %s" 
                   ARG (mapconcat 'symbol-name org-index--commands ","))))
      
      (let ((r (org-index--read-what what))) ; query user if not from argument
        (setq what (nth 0 r))
        (setq what-input (nth 1 r))
        (setq reorder-once (nth 2 r))))


    ;;
    ;; Get search, if required
    ;;

    ;; These actions need a search string:
    (when (memq what '(goto occur head update))
      ;; Maybe we've got a search string from the arguments
      (setq search (org-index--get-or-read-search search what what-input))
    
      (when search
        (when (string-match org-index--ref-regex search) 
          (setq search-ref search)
          (setq guarded-search (org-index--make-guarded-search search)))
        (when (string-match "^[a-fA-F0-9]\\{8\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{12\\}$" search)
          (setq search-link search))))

    
    ;;
    ;; Do some sanity checking before really starting
    ;;
    
    ;; Correct requested action, if nothing to search
    (when (and (not search)
               (memq what '(search head)))
      (setq what 'enter)
      (setq what-adjusted t))
    
    ;; For a proper reference as input, we do multi-occur
    (if (and (eq what 'occur) search-ref)
        (setq what 'multi-occur))
    
    ;; Check for invalid combinations of arguments; try to be helpful
    (when (and (memq what '(head goto))
               (not search-ref)
               (not search-link))
      (error "Can do '%s' only for a reference or link (not '%s'), try 'occur' to search for text" what search))

    
    ;;
    ;; Sort and enter table
    ;;

    ;; Get link if required before moving in
    (if (eq what 'link)
        (let ((org-id-link-to-org-use-id t))
          (setq link-id (org-id-get-create))))

    ;; Save initial ref or link for later return
    (if (and org-index--within-node
             (org-at-table-p))
        (setq initial-ref-or-link 
              (or (org-index--get-field :ref)
                  (org-index--get-field :link))))

    ;; These commands enter index table only temporarily
    (when (memq what '(occur multi-occur statistics))

      (set-buffer org-index--buffer)
      (goto-char org-index--point)
      
      ;; Sort and align
      (org-index--sort reorder-once)      
      (org-index--align))

    ;; These commands will leave user in index table after they are finished
    (when (memq what '(enter ref link goto missing))

      ;; Support orgmode-standard of going back (buffer and position)
      (org-mark-ring-push)

      (org-pop-to-buffer-same-window org-index--buffer)
      (goto-char org-index--point)
      (show-subtree)
      (org-show-context)

      ;; Sort and align
      (org-index--sort reorder-once)
      (org-index--align))
    
    ;; Return to initial position
    (when initial-ref-or-link
      (while (and (org-at-table-p)
                  (not (or
                        (string= initial-ref-or-link (org-index--get-field :ref))
                        (string= initial-ref-or-link (org-index--get-field :link)))))
        (forward-line))
      ;; did not find ref, go back to top
      (if (not (org-at-table-p)) (goto-char org-index--point)))
    
    
    ;;
    ;; Actually do, what is requested
    ;;

    (cond


     ((eq what 'help)
      
      ;; bring up help-buffer for this function
      (describe-function 'org-index))


     ((eq what 'multi-occur) 
      
      ;; Position point in index buffer on reference to search for
      (goto-char org-index--below-hline)
      (let (found (initial (point)))
        (while (and (not found)
                    (forward-line)
                    (org-at-table-p))
          (save-excursion 
            (setq found (string= search 
                                 (org-index--get-field :ref)))))
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


     ((eq what 'head) 

      (let (link)
        (if (and org-index--within-node
                 (org-at-table-p))
            (setq link (org-index--get-field :link))))
      
      (setq message-text (org-index--do-head search-ref search-link)))


     ((eq what 'leave)

      (setq kill-new-text org-index--text-to-yank)
      (setq org-index--text-to-yank nil)
      
      ;; If "leave" has been called two times in succession, make
      ;; org-mark-ring-goto believe it has been called two times too
      (if (eq org-index--last-action 'leave) 
          (let ((this-command nil) (last-command nil))
            (org-mark-ring-goto 1))
        (org-mark-ring-goto))
      
      ;; Return to saved position in index buffer
      (when org-index--point-before
        ;; buffer displayed in window need to set point there first
        (if (eq (window-buffer org-index--active-window-index)
                org-index--buffer)
            (set-window-point org-index--active-window-index org-index--point-before))
        ;; set position in buffer in any case and second
        (with-current-buffer org-index--buffer
          (goto-char org-index--point-before)))     
      (setq org-index--point-before nil))


     ((eq what 'goto)

      ;; Go downward in table to requested reference
      (let (found (initial (point)))
        (goto-char org-index--below-hline)
        (while (and (not found)
                    (forward-line)
                    (org-at-table-p))
          (save-excursion 
            (setq found 
                  (string= search 
                           (org-index--get-field 
                            (if search-link :link :ref))))))
        (if found
            (progn
              (setq message-text (format "Found '%s'" search))
              (org-index--update-line nil)
              (org-table-goto-column (org-index--column-num :ref))
              (if (looking-back " ") (backward-char))
              ;; remember string to copy
              (setq org-index--text-to-yank 
                    (org-trim (org-table-get-field (org-index--column-num :copy)))))
          (setq message-text (format "Did not find '%s'" search))
          (goto-char initial)
          (forward-line)
          (setq what 'missed))))


     ((eq what 'occur)

      (org-index--do-occur what-input))


     ((memq what '(ref link))

      (let (new)

        ;; add a new row (or reuse existing one)
        (setq new (org-index--do-new-line (eq what 'ref)))

        ;; fill special columns with standard values
        (when (eq what 'ref)
          (org-table-goto-column (org-index--column-num :ref))
          (insert new)
          (setq org-index--last-ref new))
        (when (eq what 'link)
          (org-table-goto-column (org-index--column-num :link))
          (insert link-id))

        (org-index--align)
        
        ;; goto point-field or copy-field or first empty one or first field
        (if (org-index--special-column :point)
            (org-table-goto-column (org-index--column-num (org-index--special-column :point)))
          (if (org-index--special-column :copy)
              (org-table-goto-column (org-index--column-num (org-index--special-column :copy)))
            (unless (catch 'empty
                      (dotimes (col org-index--numcols)
                        (org-table-goto-column (+ col 1))
                        (if (string= (org-trim (org-table-get-field)) "")
                            (throw 'empty t))))
              ;; none found, goto first
              (org-table-goto-column 1))))
      
        (if org-index--active-region (setq kill-new-text org-index--active-region))
        (if (eq what 'ref)
            (setq message-text (format "Adding a new row with ref '%s'" new))
          (setq message-text (format "Adding a new row linked to '%s'" link-id)))))


     ((eq what 'put)

      ;; put latest reference into property

      
      (if org-index--last-ref
          (progn
            (org-entry-put (point) "org-index-ref" org-index--last-ref)
            (message "Reference '%s' has been stored in property org-index-ref" org-index--last-ref))
        (setq org-index--last-ref
              (read-from-minibuffer "Reference to be stored in this node: "))
        (unless org-index--last-ref
          (message "No reference has been given."))
        ))

     
     ((eq what 'enter)

      ;; simply go into table
      (goto-char org-index--below-hline)
      (show-subtree)
      (recenter)
      (if what-adjusted
          (setq message-text "Nothing to search for; at index table")
        (setq message-text "At index table")))

     
     ((eq what 'fill)

      ;; check, if within index table
      (unless (and org-index--within-node
                   (org-at-table-p))
        (error "Not within index table"))

      ;; applies to missing refs and missing links alike
      (let ((ref (org-index--get-field :ref))
            (link (org-index--get-field :link)))

        (if (and (not ref)
                 (not link))
            ;; have already checked this during parse, check here anyway
            (error "Columns ref and link are both empty in this line"))

        ;; fill in new ref
        (if (not ref)
            (progn 
              (setq kill-new-text (format "%s%d%s" org-index--head (1+ org-index--maxref) org-index--tail))
              (org-index--get-field :ref kill-new-text)
              ;; remember for org-mark-ring-goto
              (setq org-index--text-to-yank kill-new-text)
              (org-id-goto link)
              (setq message-text "Filled field of index table with new reference"))

          ;; fill in new link
          (if (not link)
              (progn
                (setq guarded-search (org-index--make-guarded-search ref))
                (message (format "Scanning headlines for '%s' ..." ref))
                (let ((search (concat ".*" guarded-search)) 
                      link)
                  (if (catch 'found
                        (org-map-entries 
                         (lambda () 
                           (when (looking-at search)
                             (setq link (org-id-get-create))
                             (throw 'found t)))   
                         nil 'agenda)
                        nil)

                      (progn
                        (org-index--get-field :link link)
                        (setq message-text "Inserted link"))

                    (setq message-text (format "Did not find reference '%s'" ref)))))
            
            ;; nothing is missing
            (setq message-text "Columns ref and link are already filled; nothing to do")))))
     

     ((eq what 'sort)

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
            (unless (y-or-n-p "Sort whole buffer ")
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
     

     ((eq what 'update)

      ;; simply update line in index table
      (save-excursion
        (let ((ref-or-link (if search-link "link" "reference")))
          (beginning-of-line)
          (if (org-index--update-line search)
              (setq message-text (format "Updated %s '%s'" ref-or-link search))
            (setq message-text (format "Did not find %s '%s'" ref-or-link search))))))


     ((memq what '(highlight unhighlight))

      (let ((where "buffer"))
        (save-excursion
          (save-restriction
            (when (and transient-mark-mode
                       mark-active)
              (narrow-to-region (region-beginning) (region-end))
              (setq where "region"))

            (if (eq what 'highlight)
                (progn
                  (highlight-regexp org-index--ref-regex 'isearch)
                  (setq message-text (format "Highlighted references in %s" where)))
              (unhighlight-regexp org-index--ref-regex)
              (setq message-text (format "Removed highlights for references in %s" where)))))))


     ((memq what '(missing statistics))

      (setq message-text (org-index--do-statistics what)))
     
     
     (t (error "This is a bug: unmatched case '%s'" what)))


    ;; remember what we have done for next time
    (setq org-index--last-action what)
    
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
      (unless (string= m "") (message m)))
    (if kill-new-text (kill-new kill-new-text))))


(defun org-index-new-line (&rest keys-values)
  "Create a new line within the index table, returning its reference.

The function takes a varying number of arguments pairs; each pair
is a symbol for an existing column heading followed by its value.
their values.

Example:

  (org-index-new-line :ref t :x1 \"foo\" :link \"7f480c3e\")

Passing \":ref t\" will make the function create a new reference within the new line.

"

  (let ((org-index--silent t))

    (save-excursion
      (org-index--retrieve-context)
      (with-current-buffer org-index--buffer
        (goto-char org-index--point)
        (org-index--parse-table)

        ;; check arguments early
        (let ((kvs keys-values) 
              k v)
          (while kvs
            (setq k (car kvs))
            (setq v (cadr kvs))
            (if (eq k :ref)
                (unless (memq v '(t nil))
                  (error "Argument :ref accepts only t or nil"))
              (if (or (not (symbolp k)) 
                      (symbolp v))
                  (error "Arguments must be alternation of key and value")))
            (unless (> (org-index--column-num k) 0)
              (error "Unknown column or column not defined in table: '%s'" (symbol-name k)))
            (setq kvs (cddr kvs))))

        (if (and (not (plist-get keys-values :ref))
                 (not (stringp (plist-get keys-values :link))))
            (error "Need a link when not creating a ref"))

        (let (new)
          ;; create new line
          (setq new (org-index--do-new-line (plist-get keys-values :ref)))
          (plist-put keys-values :ref (or new ""))

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
          
          (org-index--sort)
          new)))))


(defun org-index-get-line (what value)
  "Retrieve an existing line within the index table by ref or
link and return its contents as a property list. 

The function `plist-get' may be used to retrieve specific values.

Example:

  (plist-get (org-index-get-line \"12\") :count) 

retrieves the value of the count-column for reference 12.

"
  (interactive)
  (let ((org-index--silent t)
        found)

    ;; check arguments
    (unless (memq what '(:ref :link))
      (error "Argument what can only be :ref or :link"))

    (save-excursion
      (org-index--retrieve-context)
      (with-current-buffer org-index--buffer
        (goto-char org-index--point)
        (org-index--parse-table)

        (goto-char org-index--below-hline)
        (while (and (not found) 
                    (org-at-table-p))
          (when (string= (org-index--get-field what)
                         value)
            (mapc (lambda (x) 
                    (if (and (numberp (cdr x))
                             (> (cdr x) 0))
                        (setq found (cons (car x) (cons (or (org-index--get-field (car x)) "") found)))
                      )) (reverse org-index--columns)))
          (forward-line))
        found))))


(defun org-index--read-what (what)
  "Find out, what we are supposed to do"

  (let (commands        ; currently active set of selectable commands
        trailing-digits ; any digits, that are are appended to what-input
        reorder-once    ; Column to use for single time sorting
        what-input)     ; Input on what question (need not necessary be "what")
        
    ;; Set preferred action, that will be the default choice
    (setq org-index--preferred-command
          (if org-index--within-node
              (if (memq org-index--last-action '(ref link))
                  'leave
                'goto)
            (if org-index--active-region
                'ref
              (if (and org-index--below-cursor (string-match org-index--ref-regex org-index--below-cursor))
                  'occur
                nil))))
  
    ;; Ask user, what to do
    (if what
        (setq what-input (symbol-name what))
      ;; subset of most common commands for initial selection, ie. up to first plus
      (setq commands (copy-list org-index--commands))
      (let ((c commands)) 
        (while (and c (not (eq (car c) '+)))
          (setq c (cdr c)))
        (setcdr c nil))
      
      (while (let (completions starts-with-plus is-only-plus)

               (setq what-input
                     (org-completing-read 
                      "Please choose: " 
                      (mapcar 'symbol-name 
                              ;; Construct unique list of commands with
                              ;; preferred one at front
                              (delq nil (delete-dups 
                                         (append 
                                          (list org-index--preferred-command)
                                          (copy-list commands)))))
                      nil nil))

               ;; if input ends in digits, save them away and do completions on head of input
               ;; this allows input like "h224" to be accepted
               (when (string-match "^\\([^0-9]+\\)\\([0-9]+\\)\\s *$" what-input)
                 ;; remember digits
                 (setq trailing-digits (string-to-number (match-string 2 what-input)))
                 ;; and use non-digits-part to find match
                 (setq what-input (match-string 1 what-input)))

               ;; if input starts with "+", any command (not only some) may follow
               ;; this allows input like "+sort" to be accepted
               (when (and (> (length what-input) 0)
                          (string= (substring what-input 0 1) "+"))
                 ;; make all commands available for selection
                 (setq commands (copy-list org-index--commands))
                 (setq what-input (substring what-input 1))
                 (setq starts-with-plus (> (length what-input) 0))
                 (setq is-only-plus (not starts-with-plus)))

               ;; get list of possible completions for what-input; i.e.
               ;; all commands, that start with what-input                   
               (setq completions (delq nil (mapcar 
                                            (lambda (x) 
                                              (let ((where (search what-input (symbol-name x))))
                                                (if (and where 
                                                         (= where 0))
                                                    x
                                                  nil))) commands)))

               ;; if input starts with "+" and not just "+"
               (when starts-with-plus
                 ;; use first completion, if unambigously
                 (if (= (length completions) 1)
                     (setq what-input (symbol-name (car completions)))
                   (if completions
                       (error "Input \"+%s\" matches multiple commands: %s" 
                              what-input 
                              (mapconcat 'symbol-name completions ", "))
                     (error "Input \"+%s\" matches no commands" what-input))))

               ;; if input ends in digits, use first completion, even if ambigous
               ;; this allows input like "h224" to be accepted
               (when (and trailing-digits completions)
                 ;; use first match as input, even if ambigously
                 (setq org-index--preferred-command (first completions))
                 (setq what-input (number-to-string trailing-digits)))

               ;; convert to symbol
               (setq what (intern what-input))
               (if is-only-plus (setq what '+))
             
               ;; user is not required to input one of the commands; if
               ;; not, take the first one and use the original input for
               ;; next question
               (if (memq what commands)
                   ;; input matched one element of list, dont need original
                   ;; input any more
                   (setq what-input nil)
                 ;; what-input will be used for next question, use first
                 ;; command for what
                 (setq what (or org-index--preferred-command
                                (first commands)))
                 ;; remove any trailing dot, that user might have added to
                 ;; disambiguate his input
                 (if (and (> (length what-input) 0) 
                          (equal (substring what-input -1) "."))
                     ;; but do this only, if dot was really necessary to
                     ;; disambiguate
                     (let ((shortened-what-input (substring what-input 0 -1)))
                       (unless (test-completion shortened-what-input 
                                                (mapcar 'symbol-name 
                                                        commands))
                         (setq what-input shortened-what-input)))))
             
               ;; ask for reorder in loop, because we have to ask for
               ;; what right again
               (if (eq what 'reorder)
                   (setq reorder-once
                         (intern
                          (concat ":"
                                  (org-icompleting-read 
                                   "Please choose column to reorder index table once: " 
                                   (mapcar 'symbol-name '(ref count accessed))
                                   nil t)))))
             
               ;; maybe ask initial question again
               (memq what '(reorder +)))))
    (list what what-input reorder-once)))


(defun org-index--get-or-read-search (search what what-input)
  "Get search string, maybe read from user"

  (let (search-from-table
        search-from-cursor)
            
    (unless search
      ;; Search string can come from several sources:
      ;; From link or ref columns of table
      (when org-index--within-node
        (setq search-from-table (or (org-index--get-field :link)
                                    (org-index--get-field :ref))))      
            
      ;; From string below cursor
      (when (and (not org-index--within-node)
                 org-index--below-cursor
                 (string-match (concat "\\(" org-index--ref-regex "\\)") 
                               org-index--below-cursor))
        (setq search-from-cursor (match-string 1 org-index--below-cursor)))
            
      ;; Depending on requested action, get search from one of the sources above
      (cond ((eq what 'goto)
             (setq search (or what-input search-from-cursor)))
            ((memq what '(head occur))
             (setq search (or what-input search-from-table search-from-cursor)))))


    ;; If we still do not have a search string, ask user explicitly
    (unless search
      
      (if org-index--silent (error "Need to specify search, if silence is required"))

      (unless (eq what 'occur)
            
        (if what-input 
            (setq search what-input)
          (setq search (read-from-minibuffer
                        (cond ((eq what 'head)
                               "Text or reference number to search for: ")
                              ((eq what 'goto)
                               "Reference number to search for, or enter \".\" for id of current node: ")
                              ((eq what 'update)
                               "Reference number to update: ")))))

        (if (string-match "^\\s *[0-9]+\\s *$" search)
            (setq search (format "%s%s%s" org-index--head search org-index--tail)))))
   

    ;; Clean up and examine search string
    (when search 
      (setq search (org-trim search))
      (if (string= search "") (setq search nil))
      (when search
        (if (string-match "^[0-9]+$" search)
            (setq search (concat org-index--head search org-index--tail)))))
    
    ;; Check for special case
    (when (and (memq what '(head goto))
               (string= search "."))
      (setq search (org-id-get)))
    
    search))


(defun org-index--verify-id ()

  ;; Check id
  (unless org-index-id
    (setq org-index-id (org-index--create-new-index 
                        t
                        (format "No index table has been created yet." org-index-id))))

  ;; Find node
  (let (marker)      
    (setq marker (org-id-find org-index-id 'marker))
    (unless marker (setq org-index-id (org-index--create-new-index 
                                       t
                                       (format "Cannot find node with id \"%s\"" org-index-id))))
    ; Try again with new node
    (setq marker (org-id-find org-index-id 'marker)) 
    (unless marker (error "Could not create node"))
    (setq org-index--buffer (marker-buffer marker) 
          org-index--point (marker-position marker))
    (move-marker marker nil)))


(defun org-index--retrieve-context ()

  ;; Get the content of the active region or the word under cursor
  (setq org-index--active-region 
        (if (and transient-mark-mode mark-active)
            (buffer-substring (region-beginning) (region-end))
          nil))
  (setq org-index--below-cursor (thing-at-point 'symbol))
  
  ;; Find out, if we are within favable or not
  (setq org-index--within-node (string= (org-id-get) org-index-id))
  
  ;; Check and remember, if active window contains buffer with index table
  (if (eq (window-buffer) org-index--buffer)
      (setq org-index--active-window-index (selected-window)))

  ;; get current position in index-buffer
  (with-current-buffer org-index--buffer
    (unless (string= (org-id-get) org-index-id)
      (unless org-index--point-before 
        (setq org-index--point-before (point))))))


(defun org-index--parse-table ()

  (let (ref-field
        link-field
        initial-point
        end-of-heading)

    (with-current-buffer org-index--buffer

      (setq org-index--maxref 0)
      (setq initial-point (point))
      (org-index--go-below-hline)
      (setq org-index--below-hline (point))
      (beginning-of-line)
      (setq end-of-heading (point))
      (while (org-at-table-p) (forward-line -1))
      (forward-line)
      (setq org-index--headings (buffer-substring (point) end-of-heading))
      (goto-char org-index--below-hline)
      
      
      ;; count columns
      (org-table-goto-column 100)
      (setq org-index--numcols (- (org-table-current-column) 1))
      
      ;; get contents of columns
      (forward-line -2)
      (unless (org-at-table-p)
        (org-index--create-new-index 
         nil
         "Index table starts with a hline"))

      ;; check for optional line consisting solely of width specifications
      (beginning-of-line)
      (if (looking-at "\\s *|\\(\\(\\s *|\\)\\|\\(\\s *<[0-9]+>\\s *|\\)\\)+\\s *$")
          (forward-line -1))
      (org-table-goto-column 1)

      (org-index--parse-headings)
      
      ;; Go beyond end of table
      (while (org-at-table-p) (forward-line 1))
      
      ;; Retrieve any decorations around the number within the first nonempty ref-field
      (goto-char org-index--below-hline)
      (while (and (org-at-table-p)
                  (not (setq ref-field (org-index--get-field :ref))))
        (forward-line))

      ;; Some Checking
      (unless ref-field
        (org-index--create-new-index 
         nil
         "Reference column is empty"))
      
      (unless (string-match "^\\([^0-9]*\\)\\([0-9]+\\)\\([^0-9]*\\)$" ref-field)
        (org-index--create-new-index 
         nil 
         (format "First reference in index table ('%s') does not contain a number" ref-field)))
      

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

          (setq ref-field (org-index--get-field :ref))
          (setq link-field (org-index--get-field :link))

          (when (and (not ref-field)
                     (not link-field))
            (org-pop-to-buffer-same-window org-index--buffer)
            (org-reveal)
            (error "Columns ref and link are both empty in this line"))

          (if ref-field
              (if (string-match org-index--ref-regex ref-field)
                  ;; grab number
                  (setq ref (string-to-number (match-string 1 ref-field)))
                (org-pop-to-buffer-same-window org-index--buffer)
                (org-reveal)
                (error "Column ref does not contain a number")))

          ;; check, if higher ref
          (if (> ref org-index--maxref) (setq org-index--maxref ref))

          ;; check if ref is ment for reuse
          (if (string= (org-index--get-field :count) ":reuse:")
              (setq org-index--has-reuse t))

          (forward-line 1)))
      
      ;; go back to initial position
      (goto-char initial-point))))


(defun org-index--sort (&optional sort-column)

  (unless sort-column (setq sort-column (org-index--special-column :sort)))

  (let (top 
        bottom
        ref-field
        count-field
        count-special)

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
                (not (org-index--get-field :ref))
                (not (org-index--get-field :link))))
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
                           (ref-field (or (org-index--get-field :ref) ""))
                           (count-field (or (org-index--get-field :count) ""))
                           (count-special 0))

                       ;; get reference with leading zeroes, so it can be
                       ;; sorted as text
                       (string-match org-index--ref-regex ref-field)
                       (setq ref (format 
                                  "%06d" 
                                  (string-to-number 
                                   (or (match-string 1 ref-field)
                                       "0"))))

                       ;; find out, if special token in count-column
                       (setq count-special (format "%d" 
                                                   (- 2
                                                      (length (member count-field '(":missing:" ":reuse:"))))))
                       
                       ;; Construct different sort-keys according to
                       ;; requested sort column; prepend count-special to
                       ;; sort special entries at bottom of table, append ref
                       ;; as a secondary sort key
                       (cond 

                        ((eq sort-column :count)
                         (concat count-special
                                 (format 
                                  "%08d" 
                                  (string-to-number (or (org-index--get-field :count)
                                                        ""))) 
                                 ref))
                        
                        ((eq sort-column :accessed)
                         (concat count-special
                                 (org-index--get-field :accessed) 
                                 " " 
                                 ref))
                        
                        ((eq sort-column :ref)
                         (concat count-special
                                 ref))
                        
                        (t (error "This is a bug: unmatched case '%s'" sort-column)))))
                   
                   nil 'string<))
      
      ;; sorting has moved point below hline
      (org-index--go-below-hline)
      (setq org-index--below-hline (point)))))


(defun org-index--go-below-hline ()

  (goto-char org-index--point)
  ;; go to heading of node
  (while (not (org-at-heading-p)) (forward-line -1))
  (forward-line 1)
  ;; go to table within node, but make sure we do not get into another node
  (while (and (not (org-at-heading-p))
              (not (org-at-table-p))
              (not (eq (point) (point-max)))) 
    (forward-line 1))
  
  ;; check, if there really is a table
  (unless (org-at-table-p)
    (org-index--create-new-index 
     t
     (format "Cannot find index table within node %s" org-index-id)))

  ;; go to first hline
  (while (and (not (org-at-table-hline-p))
              (org-at-table-p))
    (forward-line 1))
  
  ;; and check
  (unless (org-at-table-hline-p)
    (org-index--create-new-index 
     nil 
     "Cannot find hline within index table"))      

  (forward-line 1)
  (org-table-goto-column 1))


(defun org-index--align ()
  (unless buffer-read-only (org-table-align))
  (org-index--go-below-hline)
  (setq org-index--below-hline (point)))


(defun org-index--parse-headings ()

  ;; Associate names of special columns with column-numbers
  (setq org-index--columns (copy-tree '((:ref . 0) (:link . 0) (:first . 0) (:last . 0)
                                        (:count . 0) (:x1 . 0) (:x2 . 0) (:x3 . 0))))

  ;; Associate names of special columns with names of columns 
  (setq org-index--special-columns (copy-tree '((:sort . nil) (:copy . nil) (:point . nil))))

  ;; For each column
  (dotimes (col org-index--numcols)
    (let* (field-flags  ;; raw heading, consisting of file name and maybe
           ;; flags (seperated by ";")
           field        ;; field name only
           field-symbol ;; and as a symbol
           flags        ;; flags from field-flags
           found)

      ;; parse field-flags into field and flags
      (setq field-flags (org-trim (org-table-get-field (+ col 1))))
      (if (string-match "^\\([^;]*\\);\\([a-z]+\\)$" field-flags)
          (progn 
            (setq field (downcase (or (match-string 1 field-flags) "")))
            ;; get flags as list of characters
            (setq flags (mapcar 'string-to-char 
                                (split-string 
                                 (downcase (match-string 2 field-flags)) 
                                 "" t))))
        ;; no flags
        (setq field field-flags))

      (unless (string= field "") (setq field-symbol (intern (concat ":" (downcase field)))))
      ;; aliases for backward compatability
      (if (eq field-symbol :last-accessed) (setq field-symbol :last)) 
      (if (eq field-symbol :created) (setq field-symbol :first)) 

      (if (and field-symbol 
               (not (assoc field-symbol org-index--columns)))
          (error "Column %s is not a valid heading" (symbol-name field-symbol)))

      ;; Check, that no flags appear twice
      (mapc (lambda (x)
              (when (memq (car x) flags)
                (if (cdr (assoc (cdr x) org-index--columns))
                    (org-index--create-new-index 
                     nil
                     (format "More than one heading is marked with flag '%c'" (car x))))))
            '((?s . sort)
              (?c . copy)))
        
      ;; Process flags
      (if (memq ?s flags)
          (setcdr (assoc :sort org-index--special-columns) (or field-symbol (+ col 1))))
      (if (memq ?c flags)
          (setcdr (assoc :copy org-index--special-columns) (or field-symbol (+ col 1))))
      (if (memq ?p flags)
          (setcdr (assoc :point org-index--special-columns) (or field-symbol (+ col 1))))
        
      ;; Store columns in alist
      (setq found (assoc field-symbol org-index--columns))
      (when found
        (if (> (cdr found) 0) 
            (org-index--create-new-index 
             nil
             (format "'%s' appears two times as column heading" (downcase field))))
        (setcdr found (+ col 1)))))

  ;; check if all necessary informations have been specified
  (mapc (lambda (col) 
          (unless (> (cdr (assoc col org-index--columns)) 0)
            (org-index--create-new-index 
             nil
             (format "column '%s' has not been set" col))))
        (list :ref :link :count :first :last))

  ;; use count as a default sort-column
  (unless (cdr (assoc :sort org-index--special-columns))
    (setcdr (assoc :sort org-index--special-columns) :count)))


(defun org-index--create-new-index (create-new-index reason)
  "Create a new empty index table with detailed explanation."
  (let (prompt buffer-name title firstref id)

    ;; cannot proceed without querying user
    (if org-index--silent (error "No valid index: %s" reason))

    (setq prompt
          (if create-new-index
              (concat "There is this problem with the existing index table:\n\n   " reason "\n\nThis assistant will guide you to create a new one.\n\nDo you want to proceed ?")        
            (concat "The existing index table contains this error:\n\n   " reason "\n\nYou need to correct this error manually before trying again. However, this assistant will help you to create an new initial index table with detailed comments, so that you may fix the errors in your existing table more easily.\n\nDo you want to proceed ?")))
    (unless (y-or-n-p prompt) 
      (error "Cannot proceed without a valid index table: %s" reason))
  
    (setq buffer-name (org-completing-read "Please choose the buffer, where the new node for the index table should be created; the new node will be inserted at its end.\n\nBuffer: " (mapcar 'buffer-name (org-buffer-list)) nil nil))

    (setq title (read-from-minibuffer "Please enter the title of the index node: "))

    (while (progn
             (setq firstref (read-from-minibuffer "Please enter your first reference-number. This is a number preceeded by some non-digit chars and optionally followed by some more non-digit chars, e.g. 'R1', '-1-' or '#1#' (and your initial number does not need to be '1'). The format of your reference-numbers only needs to make sense for yourself, so that you can spot it easily in your texts or write it on a piece of paper; it should however not already appear to frequently within your existing notes, to avoid too many false hits when searching.\n\nPlease choose: "))
             (let (desc)
               (unless (equal '(95 119) (sort (delete-dups (mapcar (lambda (x) (char-syntax x)) (concat "-1" firstref))) '<))
                 (setq desc "Contains other characters than those allowed in symbols"))
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
                     (read-from-minibuffer (format "Your input '%s' does not meet the requirements because it %s. Please hit RET and try again" firstref desc))
                     t)
                 nil))))

    (with-current-buffer buffer-name
      (goto-char (point-max))
      (insert (format "\n\n* %s %s\n" firstref title))
      (insert "\n\n  Below you find your initial index table, which will grow over time.\n"
              "  Following that your may read its detailed explanation, which will help you,\n"
              "  to adjust org-index to your needs. This however is optional reading and not\n" 
              "  required to start using org-index.\n")

      (setq id (org-id-get-create))
      (insert (format "

  |     |      |       |         |      | comment |
  | ref | link | first | count;s | last | ;c      |
  |     | <4>  |       |         |      |         |
  |-----+------+-------+---------+------+---------|
  | %s  | %s   | %s    |         |      | %s      |

" 
                      firstref
                      id
                      (with-temp-buffer (org-insert-time-stamp nil nil t))
                      "This node"))


      (insert "

  Detailed explanation:


  The index table above has three lines of headings above the first
  hline:

  - The first one is ignored by org-index, and you can use it to
    give meaningful names to columns. In the table above only one
    column has a name (\"comment\"). This line is optional.

  - The second line is the most important one, because it
    contains the configuration information for org-index; please
    read further below for its format.

  - The third line is again optional; it may only specify the
    widths of the individual columns (e.g. <4>).

  The columns get their meaning by the second line of headings;
  specifically by one of the keywords (e.g. \"ref\") or a flag
  seperated by a semicolon (e.g. \";s\").



  The keywords and flags are:


  - ref: This contains the reference, which consists of a decorated
    number, which is incremented for each new line. References are
    meant to be used in org-mode headlines or outside of org,
    e.g. within folder names.

  - link: org-mode link pointing to the matching location within org.

  - first: When has this line been first accessed (i.e. created) ?

  - count: How many times has this line been accessed ? The
    trailing flag \"s\" makes the table beeing sorted after this
    column this column, so that often used entries appear at the
    top of the table.

  - last: When has this line been accessed last ?

  - The last column above has no keyword, only the flag \"c\",
    which makes its content beeing copied under certain
    conditions. It is typically used for comments.

  The sequence of columns does not matter. You may reorder them any
  way you like. Columns are found by their name, which appears in
  the second line of headings.

  You can add further columns or even remove the last column. All
  other columns are required.


  Finally: This node needs not be a top level node; its name is
  completely at you choice; it is found through its ID only.

")


      (while (not (org-at-table-p)) (forward-line -1))
      (unless buffer-read-only (org-table-align))
      (while (not (org-at-heading-p)) (forward-line -1))
      
      ;; present results to user
      (if create-new-index
          (progn 
            ;; Only show the new index
            (org-pop-to-buffer-same-window buffer-name)
            (delete-other-windows)
            (org-id-goto id)
            (org-show-context)    
            (show-subtree)
            (recenter 1)
            (if (y-or-n-p "This is your new index table; Do you want to save its id to make it permanent ? ")
                (progn
                  (customize-save-variable 'org-index-id id)
                  (message "Saved org-index-id '%s' to %s" org-index-id custom-file))
              (let (sq)
                (setq sq (format "(setq org-index-id \"%s\")" org-index-id))
                (kill-new sq)
                (message "Did not make the id of the new index permamanent; you may want to put\n\n   %s\n\ninto your own initialization; it is copied already, just yank it." sq))
              id))          
        ;; we had an error with the existing index table, so present old
        ;; and new one together
        ;; show existing index
        (org-pop-to-buffer-same-window org-index--buffer)
        (goto-char org-index--point)
        (org-show-context)
        (show-subtree)
        (recenter 1)
        (delete-other-windows)
        ;; show new index
        (select-window (split-window-vertically))
        (org-pop-to-buffer-same-window buffer-name)
        (org-id-goto id)
        (org-show-context)    
        (show-subtree)
        (recenter 1)
        (error "Please compare your existing index (upper window) and a temporary new one (lower window) to correct the previous error (\"%s\"); the explanations following the new index table should help." reason)))))


(defun org-index--update-line (ref-or-link)

  (let (initial
        found
        count-field)

    (with-current-buffer org-index--buffer
      (unless buffer-read-only

        ;; search reference or link, if given (or assume, that we are already positioned right)
        (when ref-or-link
          (setq initial (point))
          (goto-char org-index--below-hline)
          (while (and (org-at-table-p)
                      (not (or (string= ref-or-link (org-index--get-field :ref))
                               (string= ref-or-link (org-index--get-field :link)))))
            (forward-line)))
        
        (if (not (org-at-table-p))
            (error "Did not find reference or link '%s'" ref-or-link)
          (setq count-field (org-index--get-field :count))
          
          ;; update count field only if number or empty; leave :missing: and :reuse: as is
          (if (or (not count-field)
                  (string-match "^[0-9]+$" count-field))
              (org-index--get-field :count
                                    (number-to-string 
                                     (+ 1 (string-to-number (or count-field "0"))))))
          
          ;; update timestamp
          (org-table-goto-column (org-index--column-num :last))
          (org-table-blank-field)
          (org-insert-time-stamp nil t t)
          
          (setq found t))
      
        (if initial (goto-char initial))
        
        found))))


(defun org-index--get-field (key &optional value)
  (let (field)
    (setq field (org-trim (org-table-get-field (cdr (assoc key org-index--columns)) value)))
    (if (string= field "") (setq field nil))
    
    (org-no-properties field)))


(defun org-index--column-num (key)
  (if (numberp key) 
      key
    (cdr (assoc key org-index--columns))))


(defun org-index--special-column (key)
  (cdr (assoc key org-index--special-columns)))


(defun org-index--make-guarded-search (ref &optional dont-quote)
  (concat "\\_<" (if dont-quote ref (regexp-quote ref)) "\\_>"))


(defun org-index--do-statistics (what)
  (let ((total 0)
        missing 
        ref-field
        ref
        min
        max
        message-text)

          
    ;; start with list of all references
    (setq missing (mapcar (lambda (x) (format "%s%d%s" org-index--head x org-index--tail)) 
                          (number-sequence 1 org-index--maxref)))

    ;; go through table and remove all refs, that we see
    (goto-char org-index--below-hline)
    (while (org-at-table-p)

      ;; get ref-field and number
      (setq ref-field (org-index--get-field :ref))
      (if (and ref-field 
               (string-match org-index--ref-regex ref-field))
          (setq ref (string-to-number (match-string 1 ref-field))))

      ;; remove existing refs from list
      (if ref-field (setq missing (delete ref-field missing)))

      ;; record min and max            
      (if (or (not min) (< ref min)) (setq min ref))
      (if (or (not max) (> ref max)) (setq max ref))

      ;; count
      (setq total (1+ total))
      
      (forward-line))

    ;; insert them, if requested
    (forward-line -1)
    (if (eq what 'statistics)
        
        (setq message-text (format "Found %d references from %s to %s. %d references below highest do not appear in table. "
                                   total 
                                   (format org-index--ref-format min)   
                                   (format org-index--ref-format max)
                                   (length missing)))

      (if (y-or-n-p (format "Found %d missing references; do you wish to append them to the index table" 
                            (length missing)))
          (let (type)
            (setq type (org-icompleting-read 
                        "Insert new lines for reuse by command \"new\" or just as missing ? " '("reuse" "missing")))
            (mapc (lambda (x) 
                    (let (org-table-may-need-update) (org-table-insert-row t))
                    (org-index--get-field :ref x)
                    (org-index--get-field :count (format ":%s:" type)))
                  missing)
            (org-index--align)

            (setq message-text (format "Inserted %d new lines for missing refernces" (length missing))))
        (setq message-text (format "%d missing references." (length missing)))))
      message-text))


(defun org-index--do-head (ref link)
    
  (if ref (setq org-index--last-ref ref))
    
  (let (message-text)
    ;; Use link if available
    (if link
        (progn 
          (org-index--update-line link)
          (org-id-goto link)
          (org-reveal)
          (if (eq (current-buffer) org-index--buffer)
              (setq org-index--point-before nil))
          (setq message-text "Followed link"))
      
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
              (if (eq buffer org-index--buffer)
                  (setq org-index--point-before nil))
              (setq message-text (format "Found '%s'" (or ref link)))
              (org-pop-to-buffer-same-window buffer)
              (goto-char point)
              (org-reveal))
          (setq message-text (format "Did not find '%s'" (or ref link))))))
    message-text))


(defun org-index--do-occur (initial-search)
  (let ((occur-buffer-name "*org-index-occur*")
        (word "") ; last word to search for growing and shrinking on keystrokes
        (prompt "Search for: ")
        (hint "")
        words                     ; list of other words that must match too
        occur-buffer 
        lines-to-show              ; number of lines to show in window
        start-of-lines             ; position, where lines begin
        start-of-help              ; start of displayed help (if any)
        left-off-at                ; stack of last positions in index table
        after-inserted             ; in occur-buffer
        lines-visible              ; in occur-buffer
        below-hline-bol            ; below-hline and at bol
        exit-gracefully            ; true if normal exit
        in-c-backspace             ; true while processing C-backspace
        show-headings              ; true, if headings should be shown
        fun-on-ret                 ; function to be executed, if return has been pressed
        ret from to key)
        
    ;; clear buffer
    (if (get-buffer "*org-index-occur*")
        (kill-buffer occur-buffer-name))
    (setq occur-buffer (get-buffer-create "*org-index-occur*"))

    ;; install keyboard-shortcuts within occur-buffer
    (with-current-buffer occur-buffer
      (let ((keymap (make-sparse-keymap)))

        (set-keymap-parent keymap org-mode-map)
        (setq fun-on-ret (lambda () (interactive) 
                           (let ((ref (org-index--get-field :ref)) 
                                 (link (org-index--get-field :link)))
                             (message (org-index--do-head ref link)))))
        
        (define-key keymap (kbd "RET") fun-on-ret)
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
          (insert (concat "Incremental search, showing one window of matches. TAB toggles help.\n\n"))
          (setq start-of-lines (point))
          (setq start-of-help start-of-lines)
          (setq cursor-type 'hollow)
          
          ;; get window size of occur-buffer as number of lines to be searched
          (setq lines-to-show (+ (- (window-body-height) (line-number-at-pos)) 1))
          

          ;; fill initially
          (setq ret (org-index--get-matching-lines nil lines-to-show below-hline-bol))
          (when (car ret)
            (insert (cdr ret))
            (setq left-off-at (cons (car ret) nil))
            (setq after-inserted (cons (point) nil)))

          ;; read keys
          (while 
              (progn
                (goto-char start-of-lines)
                (setq lines-visible 0)
                
                ;; use initial-search (if present) to simulate keyboard input
                (if (and initial-search 
                         (> (length initial-search) 0))
                    (progn 
                      (setq key (string-to-char (substring initial-search 0 1)))
                      (if (length initial-search) 
                          (setq initial-search (substring initial-search 1))))
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
                    (setq exit-gracefully (member key (list 'up 'down 'left 'right 'RET ?\C-g ?\C-m)))))
                
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


             ((member key (list 'TAB ?\C-i))    ; tab: toggle display of headlines
              (setq show-headings (not show-headings))
              (goto-char start-of-lines)
              (if show-headings
                  (progn
                    (forward-line -1)
                    (kill-line)
                    (setq start-of-help (point))
                    (if (display-graphic-p)
                        (insert "<backspace> and <c-backspace> erase, cursor keys move. RET finds node, C-RET all matches.\nComma seperates words, any other key adds to search word.\n\n")
                      (insert "BACKSPACE to erase,  to finish. Then cursor keys and RET to find node.\n\n"))
                    (insert org-index--headings))
                (delete-region start-of-help start-of-lines)
                (insert "\n"))
              (setq start-of-lines (point)))


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

                (when (car ret)
                  (insert (cdr ret))
                  (setcar left-off-at (car ret))
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
              (forward-line 1)))

          ;; get all the rest
          (when (eq key 'C-return)
            (message "Getting all matches ...")
            (setq ret (org-index--get-matching-lines (cons word words) 0 (car left-off-at)))
            (message "done.")
            (insert (cdr ret))))
      
      ;; postprocessing even for non graceful exit
      (setq cursor-type t)
      ;; replace previous heading
      (let ((numlines (count-lines (point) start-of-lines)))
        (goto-char start-of-lines)
        (delete-region (point-min) (point))
        (insert (format  (concat (if exit-gracefully "Search is done;" "Search aborted;")
                                 (if (eq key 'C-return) 
                                     " showing all %d matches." 
                                   " showing only some matches.")
                                 " Use cursor keys to move, press RET to find node.\n\n")
                         numlines))
        (if show-headings (insert "\n\n" org-index--headings)))
      (forward-line))
    
    (setq buffer-read-only t)

    ;; perform action according to last char
    (forward-line -1)
    (cond 

     ((member key (list 'RET ?\C-m))
      (funcall fun-on-ret))

     ((eq key 'up)
      (forward-line -1))

     ((eq key 'down)
      (forward-line 1))

     ((eq key 'left)
      (forward-char -1))

     ((eq key 'right)
      (forward-char 1)))))


(defun org-index--do-new-line (create-ref)
  "Do the common work for org-index-new-line and org-index"

  (let (new)

    (when create-ref
      ;; go through table to find first entry to be reused
      (when org-index--has-reuse
        (goto-char org-index--below-hline)
        ;; go through table
        (while (and (org-at-table-p)
                    (not new))
          (when (string= 
                 (org-index--get-field :count)
                 ":reuse:")
            (setq new (org-index--get-field :ref))
            (if new (org-table-kill-row)))
          (forward-line)))
      
      ;; no ref to reuse; construct new reference
      (unless new 
        (setq new (format "%s%d%s" org-index--head (1+ org-index--maxref) org-index--tail)))

      ;; remember for org-mark-ring-goto
      (setq org-index--text-to-yank new))
    
    ;; insert ref or link as very first row
    (goto-char org-index--below-hline)
    (org-table-insert-row)

    ;; insert some of the standard values
    (org-table-goto-column (org-index--column-num :first))
    (org-insert-time-stamp nil nil t)
    (org-table-goto-column (org-index--column-num :count))
    (insert "1")

    new))


(defun org-index--get-matching-lines (words numlines start-from)
  (let ((numfound 0)
        pos
        initial line lines)
    
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

      ;; return to initial position
      (goto-char initial))

    (unless lines (setq lines ""))
    (cons pos lines)))


(defun org-index--test-words (words line)
  (let ((found-all t))
    (setq line (downcase line))
    (catch 'not-found
        (dolist (w words)
          (or (search w line)
              (throw 'not-found nil)))
        t)))


(defun org-index--dump-variables ()
  "Dump variables of org-index; mostly for debugging"
  (interactive)
  "Dump all variables of org-index for debugging"
 (let ((buff (get-buffer-create "*org-index-dump-variables*"))
       (maxlen 0)
       vars name value)
       
   (with-current-buffer buff 
     (erase-buffer)
     (mapatoms (lambda (s) (when (and (boundp s)
                                    (string-prefix-p "org-index-" (symbol-name s)))
                             
                             (setq name (symbol-name s))
                             (setq value (symbol-value s))
                             (setq vars (cons (cons name value) vars))
                             (if (> (length name) maxlen)
                                 (setq maxlen (length name))))))
     (setq vars (sort vars (lambda (x y) (string< (car x) (car y)))))
     (mapc (lambda (x) (insert (format (format "%%-%ds: %%s\n" (+ maxlen 1)) (car x) (cdr x)))) 
           vars)
     (pop-to-buffer buff))))


(defadvice org-mark-ring-goto (after org-index--advice-text-to-yank activate)
  "Make text from org-index available for yank."
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

