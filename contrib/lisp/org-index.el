;;; org-index.el --- A personal index for org and beyond

;; Copyright (C) 2011-2013 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@ferntreffer.de>
;; Keywords: hypermedia, matching
;; Requires: org
;; Download: http://orgmode.org/worg/code/elisp/org-index.el
;; Version: 2.3.2

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
;;  some keywords narrows down to matching entries only; so the right one
;;  can be spotted easily.
;;
;;  References are essentially small numbers (e.g. "R237" or "-455-"),
;;  which are created by this package; they are well suited to be used
;;  outside org. Links are normal org-mode links.
;;
;; Setup:
;;
;;  - Add these lines to your .emacs:
;;
;;    (require 'org-index)
;;
;;    ;; Optionally assign a key. Pick your own.
;;    (global-set-key (kbd "C-+") 'org-index)
;;
;;  - Invoke `org-index', which will assist you to create your 
;;    index table.
;;
;;  - Do not forget to restart emacs to make these lines effective.
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

;;; Change Log:

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

;;   [2012-09-22 Sa] Version 1.5.0:
;;    - New command "sort" to sort a buffer or region by reference number
;;    - New commands "highlight" and "unhighlight" to mark references

;;   [2012-07-13 Fr] Version 1.4.0:
;;    - New command "head" to find a headline with a reference number

;;   [2012-04-28 Sa] Version 1.3.0:
;;    - New commands occur and multi-occur
;;    - All commands can now be invoked explicitly
;;    - New documentation
;;    - Many bugfixes

;;   [2011-12-10 Sa] Version 1.2.0:
;;    - Fixed a bug, which lead to a loss of newly created reference numbers
;;    - Introduced single and double prefix arguments
;;    - Started this Change Log

;;; Code:

(require 'org-table)
(require 'cl)

(defvar org-index--preferred-command nil)

(defvar org-index--commands 
  '(occur head ref link leave enter goto help + reorder fill sort update highlight unhighlight missing statistics)
  "List of commands known to org-index.")

(defvar org-index--commands-some '(occur head ref link leave enter goto help +))  


(defvar org-index--columns nil)

(defcustom org-index-id nil 
  "Id of the Org-mode node, which contains the index table."
  :group 'org
  :group 'org-index)


(defvar org-index--text-to-yank nil)
(defvar org-index--last-action nil)
(defvar org-index--ref-regex nil)
(defvar org-index--ref-format nil)
(defvar org-index--buffer nil "buffer of index table")
(defvar org-index--point nil "position at start of headline of index table")
(defvar org-index--below-hline nil "position of first cell in first line below hline") 
(defvar org-index--point-before nil "point in buffer with index table")     


(defun org-index (&optional ARG)
  "Mark and find your favorite things and org-locations easily:
Create and update a lookup table of references and links. Often
used entries bubble to the top; entering some keywords narrows
down to matching entries only, so that the right one can be
spotted easily.

References are essentially small numbers (e.g. \"R237\" or \"-455-\"),
which are created by this package; they are well suited to be used
outside of org. Links are normal org-mode links.

This is version 2.3.2 of org-index.

The function `org-index' operates on a dedicated table, the index
table, which lives within its own Org-mode node. The table and
its node will be created, when you first invoke org-index.

Each line in the index table contains:

 - A reference

 - A link

 - A number; counting, how often each reference has been
   used. This number is updated automatically and the table can
   be sorted after it, so that most frequently used references
   appear at the top of the table and can be spotted easily.

 - The creation date of the line.

 - Date and time of last access. This column can alternatively be
   used to sort the table.

 - A column for your own comments, which allows lines to be selected by
   keywords.

The index table is found through the id of the containing
node; this id is stored within `org-index-id'.


The function `org-index' is the only interactive function of this
package and its sole entry point; it offers several commands to
create, find and look up these favorites (references and links).

Commands known:

  occur: Incremental search, that after each keystroke shows
    matching lines from index table. You may enter a list of words
    seperated by comma (\",\"), to select lines that contain all
    of the given words.

    If you supply a number (e.g. \"237\"): Apply emacs standard
    multi-occur operation on all org-mode buffers to search for
    this specific reference.

    You may also read the note at the end of this help on saving
    the keystroke RET with this frequent default command.

  head: If invoked outside the index table, ask for a
    reference number and search for a heading containing it. If
    invoked within index table dont ask; rather use the reference or
    link from the current line.

  ref: Create a new reference, copy any previously selected text.
    If already within index table, fill in ref-column.

  link: Create a new line in index table with a link to the
    current node.  Do not populate the ref column; this can later
    be populated by calling the \"fill\" command from within the
    index table.

  leave: Leave the index table. If the last command has
    been \"ref\", the new reference is copied and ready to yank.
    This \"org-mark-ring-goto\" and can be called several times
    in succession. If you invoke org-index with a prefix argument,
    this command \"leave\" is executed without further questions.

  enter: Just enter the node with the index table.

  goto: Search for a specific reference within the index table.

  help: Show this text.

  +: Show all commands including the less frequently used ones
    given below. If \"+\" is followd by enough letters of such a
    command (e.g. \"+fi\"), then this command is invoked
    directly.

  reorder: Temporarily reorder the index table, e.g. by
    count, reference or last access.

  fill: If either ref or link is missing, fill it.

  sort: Sort a set of lines (either the active region or the
    whole buffer) by the references found in each line.

  update: For the given reference, update the line in the
    index table.

  highlight: Highlight references in region or buffer.

  unhighlight: Remove highlights.

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

If this command needs additional input (like e.g. \"occur\"), you
may supply this input right away, although you are still beeing
prompted for the command. So, to do an occur for the string
\"foo\", you can just enter \"foo\" RET, without even typing
\"occur\".


Another way to save keystrokes applies if you want to choose a
command, that requrires a reference number (and would normally
prompt for it): In that case you may just enter enough characters
from your command, so that it appears first in the list of
matches; then immediately enter the number of the reference you
are searching for. So the input \"h237\" would execute the
command \"head\" for reference \"237\" right away.

"

  (interactive "P")

  (org-index-1 (if (equal ARG '(4)) 'leave nil) )
)


(defun org-index-1 (&optional what search search-is-link) 
"Do the actual worg for org-index; its optional arguments are:

         search : string to search for
           what : symbol of the command to invoke
 search-is-link : t, if argument search is actually a link

An example would be:

 (org-index \"237\" 'head)   ;; find heading with ref 237
"
  (let (within-node        ; True, if we are within node of the index table
        active-window-index    ; active window with index table (if any)
        below-cursor              ; word below cursor
        active-region             ; active region (if any)
        link-id                   ; link of starting node, if required
        guarded-search            ; with guard against additional digits
        search-is-ref             ; true, if search is a reference
        commands                ; currently active set of selectable commands
        what-adjusted           ; True, if we had to adjust what
        what-input    ; Input on what question (need not necessary be "what")
        trailing-digits ; any digits, that are are appended to what-input
        reorder-once  ; Column to use for single time sorting
        parts         ; Parts of a typical reference number (which
                      ; need not be a plain number); these are:
        head               ; Any header before number (e.g. "R")
        maxref             ; Maximum number from reference table (e.g. "153")
        tail               ; Tail after number (e.g. "}" or "")
        ref-regex          ; Regular expression to match a reference
        has-reuse          ; True, if table contains a line for reuse
        numcols            ; Number of columns in index table
        kill-new-text      ; Text that will be appended to kill ring
        message-text       ; Text that will be issued as an explanation,
                           ; what we have done
        initial-ref-or-link      ; Initial position in index table
        )

    ;;
    ;; Examine current buffer and location, before turning to index table
    ;;
    
    (unless (boundp 'org-index-id)
      (setq org-index-id nil)
      (org-index--create-new-index 
       t
       (format "No index table has been created yet." org-index-id)))

    ;; Bail out, if new index has been created
    (catch 'created-new-index

      ;; Get the content of the active region or the word under cursor
      (if (and transient-mark-mode
               mark-active)
          (setq active-region (buffer-substring (region-beginning) (region-end))))
      (setq below-cursor (thing-at-point 'symbol))


      ;; Find out, if we are within favable or not
      (setq within-node (string= (org-id-get) org-index-id))


      ;;
      ;; Get decoration of references and highest reference from index table
      ;;


      ;; Save initial ref or link
      (if (and within-node
               (org-at-table-p))
          (setq initial-ref-or-link 
                (or (org-index--get-field 'ref)
                    (org-index--get-field 'link))))

      ;; Find node
      (let ((marker (org-id-find org-index-id 'marker)) initial)      
        (if marker 
            (progn
              (setq org-index--buffer (marker-buffer marker) 
                    org-index--point (marker-position marker))
              (move-marker marker nil))
          (org-index--create-new-index 
           t
           (format "Cannot find node with id \"%s\"" org-index-id))))
      
      ;; Check and remember, if active window contains buffer with index table
      (if (eq (window-buffer) org-index--buffer)
          (setq active-window-index (selected-window)))

      ;; Get configuration of index table; catch errors
      (let ((error-message
             (catch 'content-error

               (with-current-buffer org-index--buffer
                 (unless org-index--point-before 
                   (setq org-index--point-before (point)))

                 (unless (string= (org-id-get) org-index-id)
                   (goto-char org-index--point))

                 ;; parse table while still within buffer
                 (setq parts (org-index--parse-and-adjust-table))
                 
                 ;; go back
                 (goto-char org-index--point-before)

                 nil))))

        (when error-message 
          (org-pop-to-buffer-same-window org-index--buffer)
          (org-reveal)
          (error error-message)))

      ;; Give names to parts of configuration
      (setq head (nth 0 parts))
      (setq maxref (nth 1 parts))
      (setq tail (nth 2 parts))
      (setq numcols (nth 3 parts))
      (setq ref-regex (nth 4 parts))
      (setq has-reuse (nth 5 parts))
      (setq org-index--ref-regex ref-regex)
      (setq org-index--ref-format (concat head "%d" tail))

      ;;
      ;; Find out, what we are supposed to do
      ;;

      ;; Set preferred action, that will be the default choice
      (setq org-index--preferred-command
            (if within-node
                (if (memq org-index--last-action '(ref link))
                    'leave
                  'goto)
              (if active-region
                  'ref
                (if (and below-cursor (string-match ref-regex below-cursor))
                    'occur
                  nil))))
      
      ;; Ask user, what to do
      (unless what
        (setq commands (copy-list org-index--commands-some))
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
                 (when (string= (substring what-input 0 1) "+")
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
                            (org-icompleting-read 
                             "Please choose column to reorder index table once: " 
                             (mapcar 'symbol-name '(ref count last-accessed))
                             nil t))))
                 
                 ;; maybe ask initial question again
                 (memq what '(reorder +)))))


      ;;
      ;; Get search, if required
      ;;

      ;; These actions need a search string:
      (when (memq what '(goto occur head update))

        ;; Maybe we've got a search string from the arguments
        (unless search
          (let (search-from-table
                search-from-cursor)
            
            ;; Search string can come from several sources:
            ;; From link or ref columns of table
            (when within-node
              (setq search-from-table (org-index--get-field 'link))
              (if search-from-table 
                  (setq search-is-link t)
                (setq search-from-table (org-index--get-field 'ref))))      
            
            ;; From string below cursor
            (when (and (not within-node)
                       below-cursor
                       (string-match (concat "\\(" ref-regex "\\)") 
                                     below-cursor))
              (setq search-from-cursor (match-string 1 below-cursor)))
            
            ;; Depending on requested action, get search from one of the sources above
            (cond ((eq what 'goto)
                   (setq search (or what-input search-from-cursor)))
                  ((memq what '(head occur))
                   (setq search (or what-input search-from-table search-from-cursor))))))


        ;; If we still do not have a search string, ask user explicitly
        (unless search
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
                (setq search (format "%s%s%s" head (org-trim search) tail))))))
      
      ;; Clean up and examine search string
      (when search 
        (setq search (org-trim search))
        (if (string= search "") (setq search nil))
        (when search
          (if (string-match "^[0-9]+$" search)
              (setq search (concat head search tail)))
          (setq search-is-ref (string-match ref-regex search))))
      
      ;; Check for special case
      (when (and (memq what '(head goto))
                 (string= search "."))
        (setq search (org-id-get))
        (setq search-is-link t))
      
      (when search-is-ref
        (setq guarded-search (org-index--make-guarded-search search)))
      
      ;;
      ;; Do some sanity checking before really starting
      ;;
      
      ;; Correct requested action, if nothing to search
      (when (and (not search)
                 (memq what '(search head)))
        (setq what 'enter)
        (setq what-adjusted t))
      
      ;; For a proper reference as input, we do multi-occur
      (if (and search
               (string-match ref-regex search)
               (eq what 'occur))
          (setq what 'multi-occur))
      
      ;; Check for invalid combinations of arguments; try to be helpful
      (when (and (memq what '(head goto))
                 (not search-is-link)
                 (not search-is-ref))
        (error "Can do '%s' only for a reference or link (not '%s'), try 'occur' to search for text" what search))

      
      ;;
      ;; Prepare
      ;;

      ;; Get link if required before moving in
      (if (eq what 'link)
          (let ((org-id-link-to-org-use-id t))
            (setq link-id (org-id-get-create))))

      ;; Move into table, if outside

      ;; These commands enter index table only temporarily
      (when (memq what '(occur multi-occur statistics))

        ;; Switch to index table
        (set-buffer org-index--buffer)
        (goto-char org-index--point)

        ;; sort index table
        (org-index--sort-table reorder-once))

      ;; These commands will leave user in index table after they are finished
      (when (memq what '(enter ref link goto missing))

        ;; Support orgmode-standard of going back (buffer and position)
        (org-mark-ring-push)

        ;; Switch to index table
        (org-pop-to-buffer-same-window org-index--buffer)
        (goto-char org-index--point)
        (show-subtree)
        (org-show-context)
        (setq org-index--point-before nil) ;; dont want to go back

        ;; sort index table
        (org-index--sort-table reorder-once))

      ;; Goto back to initial ref, because reformatting of table above might
      ;; have moved point
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


       ((eq what 'help)
        
        ;; bring up help-buffer for this function
        (describe-function 'org-index))


       ((eq what 'multi-occur) 
        
        ;; Conveniently position cursor on number to search for
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
          (if (get-buffer "*Occur*")
              (progn 
                (setq message-text (format "multi-occur for '%s'" search))
                (other-window 1)
                (toggle-truncate-lines 1))
            (setq message-text (format "Did not find '%s'" search)))))


       ((eq what 'head)

        (let (link)
          ;; link either from table or passed in as argument
          
          ;; try to get link
          (if search-is-link 
              (setq link (org-trim search))
            (if (and within-node
                     (org-at-table-p))
                (setq link (org-index--get-field 'link))))

          ;; use link if available
          (if (and link
                   (not (string= link "")))
              (progn 
                (org-index--update-line search)
                (org-id-goto link)
                (org-reveal)
                (if (eq (current-buffer) org-index--buffer)
                    (setq org-index--point-before nil))
                (setq message-text "Followed link"))

            (message (format "Scanning headlines for '%s' ..." search))
            (org-index--update-line search)
            (let (buffer point)
              (if (catch 'found
                    (progn
                      ;; loop over all headlines, stop on first match
                      (org-map-entries 
                       (lambda () 
                         (when (looking-at (concat ".*" guarded-search))
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
                    (setq message-text (format "Found '%s'" search))
                    (org-pop-to-buffer-same-window buffer)
                    (goto-char point)
                    (org-reveal))
                (setq message-text (format "Did not find '%s'" search)))))))


       ((eq what 'leave)

        (setq kill-new-text org-index--text-to-yank)
        (setq org-index--text-to-yank nil)
        
        ;; If "leave" has been called two times in succession, make
        ;; org-mark-ring-goto believe it has been called two times too
        (if (eq org-index--last-action 'leave) 
            (let ((this-command nil) (last-command nil))
              (org-mark-ring-goto 1))
          (org-mark-ring-goto)))


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
                              (if search-is-link 'link 'ref))))))
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
            (setq what 'missed))))


       ((eq what 'occur)

        (org-index--do-occur what-input))


       ((memq what '(ref link))

        ;; add a new row (or reuse existing one)
        (let (new)

          (when (eq what 'ref)
            ;; go through table to find first entry to be reused
            (when has-reuse
              (goto-char org-index--below-hline)
              ;; go through table
              (while (and (org-at-table-p)
                          (not new))
                (when (string= 
                       (org-index--get-field 'count)
                       ":reuse:")
                  (setq new (org-index--get-field 'ref))
                  (if new (org-table-kill-row)))
                (forward-line)))
            
            ;; no ref to reuse; construct new reference
            (unless new 
              (setq new (format "%s%d%s" head (1+ maxref) tail)))

            ;; remember for org-mark-ring-goto
            (setq org-index--text-to-yank new))
          
          ;; insert ref or link as very first row
          (goto-char org-index--below-hline)
          (org-table-insert-row)
          
          ;; fill special columns with standard values
          (when (eq what 'ref)
            (org-table-goto-column (org-index--column-num 'ref))
            (insert new))
          (when (eq what 'link)
            (org-table-goto-column (org-index--column-num 'link))
            (insert link-id))
          (org-table-goto-column (org-index--column-num 'created))
          (org-insert-time-stamp nil nil t)
          (org-table-goto-column (org-index--column-num 'count))
          (insert "1")

          ;; goto copy-field or first empty one
          (if (org-index--column-num 'copy)
              (org-table-goto-column (org-index--column-num 'copy))
            (unless (catch 'empty
                      (dotimes (col numcols)
                        (org-table-goto-column (+ col 1))
                        (if (string= (org-trim (org-table-get-field)) "")
                            (throw 'empty t))))
              ;; none found, goto first
              (org-table-goto-column 1)))

          (org-table-align)
          (if active-region (setq kill-new-text active-region))
          (if (eq what 'ref)
              (setq message-text (format "Adding a new row with ref '%s'" new))
            (setq message-text (format "Adding a new row linked to '%s'" link-id)))))


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
        (unless (and within-node
                     (org-at-table-p))
          (error "Not within index table"))

        ;; applies to missing refs and missing links alike
        (let ((ref (org-index--get-field 'ref))
              (link (org-index--get-field 'link)))

          (if (and (not ref)
                   (not link))
              ;; have already checked this during parse, check here anyway
              (error "Columns ref and link are both empty in this line"))

          ;; fill in new ref
          (if (not ref)
              (progn 
                (setq kill-new-text (format "%s%d%s" head (1+ maxref) tail))
                (org-index--get-field 'ref kill-new-text)
                ;; remember for org-mark-ring-goto
                (setq org-index--text-to-yank kill-new-text)
                (org-id-goto link)
                (setq message-text "Filled field of index table with new reference"))

            ;; fill in new link
            (if (not link)
                (progn
                  (setq guarded-search (org-index--make-guarded-search ref))
                  (message (format "Scanning headlines for '%s' ..." ref))
                  (let (link)
                    (if (catch 'found
                          (org-map-entries 
                           (lambda () 
                             (when (looking-at (concat ".*" guarded-search))
                               (setq link (org-id-get-create))
                               (throw 'found t)))   
                           nil 'agenda)
                          nil)

                        (progn
                          (org-index--get-field 'link link)
                          (setq message-text "Inserted link"))

                      (setq message-text (format "Did not find reference '%s'" ref)))))
              
              ;; nothing is missing
              (setq message-text "Columns 'ref' and 'link' are already filled; nothing to do")))))
       

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
                                                     (org-index--make-guarded-search ref-regex 'dont-quote)))
                                 (string-to-number (match-string 1))
                               0))))
              (highlight-regexp ref-regex 'isearch)
              (setq message-text (format "Sorted %s from character %d to %d, %d lines" 
                                         where begin end
                                         (count-lines begin end)))))))
       

       ((eq what 'update)

        ;; simply update line in index table
        (save-excursion
          (let ((ref-or-link (if search-is-link "link" "reference")))
            (beginning-of-line)
            (if (org-index--update-line search)
                (setq message-text (format "Updated %s '%s'" ref-or-link search))
              (setq message-text (format "Did not find %s '%s'" ref-or-link search))))))


       ((eq what 'parse)
        ;; Just parse the index table, which is already done, so nothing to do
        )


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
                    (highlight-regexp ref-regex 'isearch)
                    (setq message-text (format "Highlighted references in %s" where)))
                (unhighlight-regexp ref-regex)
                (setq message-text (format "Removed highlights for references in %s" where)))))))


       ((memq what '(missing statistics))

        (goto-char org-index--below-hline)
        (let (missing 
              ref-field
              ref
              min
              max 
              (total 0))

          ;; start with list of all references
          (setq missing (mapcar (lambda (x) (format "%s%d%s" head x tail)) 
                                (number-sequence 1 maxref)))

          ;; go through table and remove all refs, that we see
          (while (and (forward-line)
                      (org-at-table-p))

            ;; get ref-field and number
            (setq ref-field (org-index--get-field 'ref))
            (if (and ref-field 
                     (string-match ref-regex ref-field))
                (setq ref (string-to-number (match-string 1 ref-field))))

            ;; remove existing refs from list
            (if ref-field (setq missing (delete ref-field missing)))

            ;; record min and max            
            (if (or (not min) (< ref min)) (setq min ref))
            (if (or (not max) (> ref max)) (setq max ref))

            ;; count
            (setq total (1+ total)))

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
                          (org-index--get-field 'ref x)
                          (org-index--get-field 'count (format ":%s:" type)))
                        missing)
                  (org-table-align)
                  (setq message-text (format "Inserted %d new lines for missing refernces" (length missing))))
              (setq message-text (format "%d missing references." (length missing)))))))
       
       
       (t (error "This is a bug: unmatched case '%s'" what)))


      ;; restore point in buffer or window with index table
      (if org-index--point-before
          ;; buffer displayed in window need to set point there first
          (if (eq (window-buffer active-window-index)
                  org-index--buffer)
              (set-window-point active-window-index org-index--point-before)
            ;; set position in buffer in any case and second
            (with-current-buffer org-index--buffer
              (goto-char org-index--point-before)
              (setq org-index--point-before nil))))


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
      (if kill-new-text (kill-new kill-new-text)))))



(defun org-index--parse-and-adjust-table ()

  (let ((maxref 0)
        top
        bottom
        ref-field
        link-field
        parts
        numcols
        head
        tail
        ref-regex
        has-reuse
        initial-point)

    (setq initial-point (point))
    (org-index--go-below-hline)
    (setq org-index--below-hline (point))
    (setq top (point))
    
    ;; count columns
    (org-table-goto-column 100)
    (setq numcols (- (org-table-current-column) 1))
    
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

    (setq org-index--columns (org-index--parse-headings numcols))
    
    ;; Go beyond end of table
    (while (org-at-table-p) (forward-line 1))
    
    ;; Kill all empty rows at bottom
    (while (progn
             (forward-line -1)
             (org-table-goto-column 1)
             (and
              (not (org-index--get-field 'ref))
              (not (org-index--get-field 'link))))
      (org-table-kill-row))
    (forward-line)
    (setq bottom (point))
    (forward-line -1)
    
    ;; Retrieve any decorations around the number within the first nonempty ref-field
    (goto-char top)
    (while (and (org-at-table-p)
                (not (setq ref-field (org-index--get-field 'ref))))
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
    (setq head (match-string 1 ref-field))
    (setq tail (match-string 3 ref-field))
    (setq ref-regex (concat (regexp-quote head)
                            "\\([0-9]+\\)" 
                            (regexp-quote tail)))

    ;; Go through table to find maximum number and do some checking
    (let ((ref 0))

      (while (org-at-table-p) 

        (setq ref-field (org-index--get-field 'ref))
        (setq link-field (org-index--get-field 'link))

        (if (and (not ref-field)
                 (not link-field))
            (throw 'content-error "Columns ref and link are both empty in this line"))

        (if ref-field
            (if (string-match ref-regex ref-field)
                ;; grab number
                (setq ref (string-to-number (match-string 1 ref-field)))
              (throw 'content-error "Column ref does not contain a number")))

        ;; check, if higher ref
        (if (> ref maxref) (setq maxref ref))

        ;; check if ref is ment for reuse
        (if (string= (org-index--get-field 'count) ":reuse:")
            (setq has-reuse 1))

        (forward-line 1)))
    
    ;; sort used to be here
    
    (setq parts (list head maxref tail numcols ref-regex has-reuse))
        
    ;; go back to top of table
    (goto-char top)

    parts))



(defun org-index--sort-table (sort-column)

  (unless sort-column (setq sort-column (org-index--column-num 'sort)))

  (let (top 
        bottom
        ref-field
        count-field
        count-special)


    ;; get boundaries of table
    (goto-char org-index--below-hline)
    (forward-line 0)
    (setq top (point))
    (while (org-at-table-p) (forward-line))
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
                         (count-field (or (org-index--get-field 'count) ""))
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

                      ((eq sort-column 'count)
                       (concat count-special
                               (format 
                                "%08d" 
                                (string-to-number (or (org-index--get-field 'count)
                                                      ""))) 
                               ref))
                    
                      ((eq sort-column 'last-accessed)
                       (concat count-special
                               (org-index--get-field 'last-accessed) 
                               " " 
                               ref))
                    
                      ((eq sort-column 'ref)
                       (concat count-special
                               ref))
                    
                      (t (error "This is a bug: unmatched case '%s'" sort-column)))))
               
                 nil 'string<)))
    
  ;; align table
  (org-table-align)) 


(defun org-index--go-below-hline ()

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



(defun org-index--parse-headings (numcols)

  (let (columns)

    ;; Associate names of special columns with column-numbers
    (setq columns (copy-tree '((ref . 0) (link . 0) (created . 0) (last-accessed . 0) 
                               (count . 0) (sort . nil) (copy . nil))))

    ;; For each column
    (dotimes (col numcols)
      (let* (field-flags ;; raw heading, consisting of file name and maybe
                         ;; flags (seperated by ";")
             field       ;; field name only
             field-symbol ;; and as a symbol
             flags       ;; flags from field-flags
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

        (unless (string= field "") (setq field-symbol (intern (downcase field))))

        ;; Check, that no flags appear twice
        (mapc (lambda (x)
                (when (memq (car x) flags)
                  (if (cdr (assoc (cdr x) columns))
                      (org-index--create-new-index 
                       nil
                       (format "More than one heading is marked with flag '%c'" (car x))))))
              '((?s . sort)
                (?c . copy)))
        
        ;; Process flags
        (if (memq ?s flags)
            (setcdr (assoc 'sort columns) field-symbol))
        (if (memq ?c flags)
            (setcdr (assoc 'copy columns) (+ col 1)))
        
        ;; Store columns in alist
        (setq found (assoc field-symbol columns))
        (when found
          (if (> (cdr found) 0) 
              (org-index--create-new-index 
               nil
               (format "'%s' appears two times as column heading" (downcase field))))
          (setcdr found (+ col 1)))))

    ;; check if all necessary informations have been specified
    (mapc (lambda (col) 
            (unless (> (cdr (assoc col columns)) 0)
              (org-index--create-new-index 
               nil
               (format "column '%s' has not been set" col))))
          '(ref link count created last-accessed))

    ;; use ref as a default sort-column
    (unless (cdr (assoc 'sort columns))
      (setcdr (assoc 'sort columns) 'ref))
    columns))



(defun org-index--create-new-index (create-new-index reason)
  "Create a new empty index table with detailed explanation."
  (let (prompt buffer-name title firstref id)

    (setq prompt
          (if create-new-index
              (concat "There is this problem with the existing index table:\n\n   " reason "\n\nThis assistant will guide you to create a new one.\n\nDo you want to proceed ?")        
            (concat "The existing index table contains this error:\n\n   " reason "\n\nYou need to correct this error manually before proceeding. However, this assistant will help you to create an new initial index table with detailed comments, so that you may fix the errors in your existing table more easily.\n\nDo you want to proceed ?")))
    
    (unless (y-or-n-p prompt) 
      (message "Cannot proceed without a valid index table: %s" reason)
      ;; show existing index
      (when (and org-index--buffer
                 org-index--point)
        (org-pop-to-buffer-same-window org-index--buffer)
        (goto-char org-index--point)
        (org-show-context)
        (show-subtree)
        (recenter 1)
        (delete-other-windows))
      (throw 'created-new-index nil))
  
    (setq buffer-name (org-completing-read "Please choose the buffer, where the new node for the index table should be created; the new node will be inserted at its end.\n\nBuffer: " (mapcar 'buffer-name (org-buffer-list)) nil nil))

    (setq title (read-from-minibuffer "Please enter the title of the index node: "))

    (while (progn
             (setq firstref (read-from-minibuffer "Please enter your first reference-number. This is a number preceeded by some non-digit chars and optionally followed by some more non-digit chars, e.g. 'R1', '-1-' or '#1#' (and your initial number does not need to be '1'). The format of your reference-numbers only needs to make sense for yourself, so that you can spot it easily in your texts or write it on a piece of paper; it should however not already appear to frequently within your existing notes, to avoid too many false hits when searching.\n\nPlease choose: "))
             (if (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)
                 nil
               (let (desc)
                 ;; firstref not okay, report details
                 (setq desc
                       (cond ((string= firstref "") "is empty") 
                             ((not (string-match "^[^0-9]+" firstref)) "starts with a digit")
                             ((not (string-match "^[^0-9]+[0-9]+" firstref)) "does not contain a number")
                             ((not (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)) "contains more than one sequence of digits")))
                 (read-from-minibuffer (format "Your input '%s' does not meet the requirements because it %s. Please hit RET and try again " firstref desc)))
               t)))

    (with-current-buffer buffer-name
      (goto-char (point-max))
      (insert (format "\n\n* %s %s\n" firstref title))
      (insert "\n\n  Below you find your initial index table, which will grow over time.\n"
              "  Following that your may read its detailed explanation, which will help you,\n"
              "  to adopt org-index to your needs. This however is optional reading and not\n" 
              "  required to start using org-index.\n\n")

      (setq id (org-id-get-create))
      (insert (format "

  |     |      |         |         |               | comment |
  | ref | link | created | count;s | last-accessed | ;c      |
  |     | <4>  |         |         |               |         |
  |-----+------+---------+---------+---------------+---------|
  | %s  | %s   | %s      |         |               | %s      |

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
    meant to be used in org-mode headlines or outside of org´,
    e.g. within folder names.

  - link: org-mode link pointing to the matching location within org.

  - created: When has this line been created ?

  - count: How many times has this line accessed ? The trailing
    flag \"s\" makes the table beeing sorted after
    this column, so that often used entries appear at the top of
    the table.

  - last-accessed: When has this line ben accessed

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
      (org-table-align)
      (while (not (org-at-heading-p)) (forward-line -1))
      
      ;; present results to user
      (if (and (not create-new-index)
               org-index--buffer
               org-index--point)
          
          ;; we had an error with the existing table, so present old and new one
          (progn
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
            (message "Please compare your existing index (upper window) and a temporary new one (lower window) to correct the previous error (\"%s\"); the explanations following the new index table should help." reason))

        ;; Only show the new index
        (org-pop-to-buffer-same-window buffer-name)
        (delete-other-windows)
        (org-id-goto id)
        (org-show-context)    
        (show-subtree)
        (recenter 1)
        (setq org-index-id id)
        (if (y-or-n-p "This is your new index table; Do you want to save its id to make it permanent ? ")
            (progn
              (customize-save-variable 'org-index-id id)
              (message "Saved org-index-id '%s' to %s" org-index-id custom-file))
          (let (sq)
            (setq sq (format "(setq org-index-id \"%s\")" org-index-id))
            (kill-new sq)
            (message "Did not make the id of the new index permamanent; you may want to put\n\n   %s\n\ninto your own initialization; it is copied already, just yank it." sq)))))
    ;; cannot handle this situation in higher code, but do not want to finish with an error
    (throw 'created-new-index nil)))




(defun org-index--update-line (ref-or-link)

  (let (initial
        found
        count-field)

    (with-current-buffer org-index--buffer
      
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
        (setq count-field (org-index--get-field 'count))

        ;; update count field only if number or empty; leave :missing: and :reuse: as is
        (if (or (not count-field)
                (string-match "^[0-9]+$" count-field))
            (org-index--get-field 'count
                                    (number-to-string 
                                     (+ 1 (string-to-number (or count-field "0"))))))

        ;; update timestamp
        (org-table-goto-column (org-index--column-num 'last-accessed))
        (org-table-blank-field)
        (org-insert-time-stamp nil t t)

        (setq found t))
      
      (if initial (goto-char initial))
      
      found)))



(defun org-index--get-field (key &optional value)
  (let (field)
    (setq field (org-trim (org-table-get-field (cdr (assoc key org-index--columns)) value)))
    (if (string= field "") (setq field nil))
    
    field))


(defun org-index--column-num (key)
  (cdr (assoc key org-index--columns)))


(defun org-index--make-guarded-search (ref &optional dont-quote)
  (concat "\\b" (if dont-quote ref (regexp-quote ref)) "\\b"))


(defun org-index-get-ref-regex-format ()
  "return cons-cell with regular expression and format for references"
  (unless org-index--ref-regex
    (org-index-1 'parse))
  (cons (org-index--make-guarded-search org-index--ref-regex 'dont-quote) org-index--ref-format))
  

(defun org-index--do-occur (initial-search)
  (let (
        (occur-buffer-name "*org-index-occur*")
        (word "") ; last word to search for growing and shrinking on keystrokes
        (prompt "Search for: ")
        words     ; list of other words that must match too
        occur-buffer 
        lines-to-show                 ; number of lines to show in window
        start-of-lines                ; position, where lines begin
        left-off-at                   ; stack of last positions in index table
        after-inserted                ; in occur-buffer
        lines-visible                 ; in occur-buffer
        below-hline-bol               ; below-hline and at bol
        exit-gracefully               ; true if normal exit
        in-c-backspace                ; true while processing C-backspace
        ret from to key)
        
    ;; clear buffer
    (if (get-buffer "*org-index-occur*")
        (kill-buffer occur-buffer-name))
    (setq occur-buffer (get-buffer-create "*org-index-occur*"))

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
          (insert (concat "Incremental search, showing one window of matches.\n"
                          "Use DEL and C-DEL to erase, cursor keys to move, RET to find heading.\n\n"))
          (setq start-of-lines (point))
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
                    (setq key (read-event
                               (format "%s %s" 
                                       prompt 
                                       (mapconcat 'identity (reverse (cons word words)) ","))))

                    (setq exit-gracefully (memq key (list 'return 'up 'down 'left 'right)))))
                
                (not exit-gracefully))
            
            (cond 

             ((eq key 'C-backspace)

              (setq in-c-backspace t))

             ((eq key 'backspace)              ; erase last char

              (if (= (length word) 0)

                  ;; nothing more to delete
                  (setq in-c-backspace nil)

                ;; unhighlight longer match
                (let ((case-fold-search t))
                (unhighlight-regexp (regexp-quote word)))

                ;; chars left shorten word
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


             ((eq key ?,)         ; comma: enter an additional search word

              ;; push current word and clear, no need to change display
              (setq words (cons word words))
              (setq word ""))


             ((and (characterp key)
                   (aref printable-chars key)) ; any other char: add to current search word


              ;; unhighlight short word
              (unless (= (length word) 0)
                (let ((case-fold-search t))
                  (unhighlight-regexp (regexp-quote word))))

              ;; add to word
              (setq word (concat word (downcase (string key))))

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
                (highlight-regexp (regexp-quote word) 'isearch)))))
          
          ;; search is done collect and brush up results
          ;; remove any lines, that are still invisible
          (goto-char start-of-lines)
          (while (< (point) (point-max))
            (if (outline-invisible-p)
                (delete-region (line-beginning-position) (line-beginning-position 2))
              (forward-line 1)))

          ;; get all the rest
          (message "Getting all matches ...")
          (setq ret (org-index--get-matching-lines (cons word words) 0 (car left-off-at)))
          (message "done.")
          (insert (cdr ret)))
      
      ;; postprocessing even for non graceful exit
      (setq cursor-type t)
      ;; replace previous heading
      (let ((numlines (count-lines (point) start-of-lines)))
        (goto-char start-of-lines)
        (forward-line -1)
        (delete-region (point-min) (point))
        (insert (format  (concat (if exit-gracefully 
                                     "Search is done; showing all %d matches.\n"
                                   "Search aborted; showing only some matches.\n")
                                 "Use cursor keys to move, press RET to find heading.\n")
                         numlines)))
      (forward-line))

    ;; install keyboard-shortcuts within occur-buffer
    (let ((keymap (make-sparse-keymap))
          fun-on-ret)
      (set-keymap-parent keymap text-mode-map)

      (setq fun-on-ret (lambda () (interactive) 
                         (let ((ref (org-index--get-field 'ref)) 
                               (link (org-index--get-field 'link)))
                           (org-index-1 'head
                                         (or link ref) ;; prefer link
                                         (if link t nil)))))      

      (define-key keymap (kbd "RET") fun-on-ret)
      (use-local-map keymap)
    
      ;; perform action according to last char
      (cond 
       ((eq key 'return)
        (funcall fun-on-ret))

       ((eq key 'up)
         (forward-line -1))

       ((eq key 'down)
         (forward-line 1))

       ((eq key 'left)
         (forward-char -1))

       ((eq key 'right)
         (forward-char 1))))))


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
