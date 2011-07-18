;;; org-velocity.el --- something like Notational Velocity for Org.

;; Copyright (C) 2010, 2011 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <paulmrodriguez@gmail.com>
;; Created: 2010-05-05
;; Version: 2.4

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;; Org-Velocity.el is an interface for Org inspired by the minimalist
;; notetaking program Notational Velocity. The idea is to let you
;; amass and access brief notes on many subjects with minimal fuss.
;; Each note is an entry in an ordinary Org file.

;; Org-Velocity can be used in two ways: when called outside Org, to
;; store and access notes in a designated bucket file; or, when called
;; inside Org, as a method for navigating any Org file. (Setting the
;; option `org-velocity-always-use-bucket' disables navigation inside
;; Org files by default, although you can still force this behavior by
;; calling `org-velocity-read' with an argument.)

;; Org-Velocity prompts for search terms in the minibuffer. A list of
;; headings of entries whose text matches your search is updated as
;; you type; you can end the search and visit an entry at any time by
;; clicking on its heading.

;; RET displays the results. If there are no matches, Org-Velocity
;; offers to create a new entry with your search string as its
;; heading. If there are matches, it displays a list of results where
;; the heading of each matching entry is hinted with a number or
;; letter; clicking a result, or typing the matching hint, opens the
;; entry for editing in an indirect buffer. 0 forces a new entry; RET
;; reopens the search for editing.

;; You can customize every step in this process, including the search
;; method, completion for search terms, and templates for creating new
;; entries; M-x customize-group RET org-velocity RET to see all the
;; options.

;; Thanks to Richard Riley, Carsten Dominik, Bastien Guerry, and Jeff
;; Horn for their suggestions.

;;; Usage:
;; (require 'org-velocity)
;; (setq org-velocity-bucket (expand-file-name "bucket.org" org-directory))
;; (global-set-key (kbd "C-c v") 'org-velocity-read)

;;; Code:
(require 'org)
(require 'button)
(require 'electric)
(require 'dabbrev)
(eval-when-compile (require 'cl))

(defgroup org-velocity nil
  "Notational Velocity-style interface for Org."
  :tag "Org-Velocity"
  :group 'outlines
  :group 'hypermedia
  :group 'org)

(defcustom org-velocity-bucket ""
  "Where is the bucket file?"
  :group 'org-velocity
  :type 'file)

(defcustom org-velocity-search-is-incremental t
  "Show results incrementally when possible?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-exit-on-match nil
  "When searching incrementally, exit on a single match?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-force-new nil
  "Should exiting the minibuffer with C-j force a new entry?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-max-depth nil
  "Ignore headings deeper than this."
  :group 'org-velocity
  :type '(choice
          (const :tag "No maximum depth" nil)
          (integer :tag "Set maximum depth"))
  :safe (lambda (v) (or (null v) (wholenump v))))

(defcustom org-velocity-use-search-ring t
  "Push search to `search-ring' when visiting an entry?

This means that C-s C-s will take you directly to the first
instance of the search string."
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-always-use-bucket nil
  "Use bucket file even when called from an Org buffer?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-use-completion nil
  "Use completion?

Notwithstanding the value of this option, calling
`dabbrev-expand' always completes against the text of the bucket
file."
  :group 'org-velocity
  :type '(choice
          (const :tag "Do not use completion" nil)
          (const :tag "Use completion" t))
  :safe 'booleanp)

(defcustom org-velocity-edit-indirectly t
  "Edit entries in an indirect buffer or just visit the file?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-search-method 'phrase
  "Match on whole phrase, any word, or all words?"
  :group 'org-velocity
  :type '(choice
	  (const :tag "Match whole phrase" phrase)
	  (const :tag "Match any word" any)
	  (const :tag "Match all words" all)
          (const :tag "Match a regular expression" regexp))
  :safe (lambda (v) (memq v '(phrase any all regexp))))

(defcustom org-velocity-create-method 'capture
  "Prefer `org-capture', `org-remember', or neither?"
  :group 'org-velocity
  :type '(choice
	  (const :tag "Prefer capture > remember > default." capture)
	  (const :tag "Prefer remember > default." remember)
	  (const :tag "Edit in buffer." buffer))
  :safe (lambda (v) (memq v '(capture remember buffer))))

(defcustom org-velocity-remember-templates
  '(("Velocity entry"
     ?v
     "* %:search\n\n%i%?"
     nil
     bottom))
  "Use these templates with `org-remember'.
Meanwhile `org-default-notes-file' is bound to `org-velocity-use-file'.
The keyword :search inserts the current search.
See the documentation for `org-remember-templates'."
  :group 'org-velocity
  :type (or (get 'org-remember-templates 'custom-type) 'list))

(defcustom org-velocity-capture-templates
  '(("v"
     "Velocity entry"
     entry
     (file "")
     "* %:search\n\n%i%?"))
  "Use these template with `org-capture'.
Meanwhile `org-default-notes-file' is bound to `org-velocity-use-file'.
The keyword :search inserts the current search.
See the documentation for `org-capture-templates'."
  :group 'org-velocity
  :type (or (get 'org-capture-templates 'custom-type) 'list))

(defstruct (org-velocity-heading
	    (:constructor org-velocity-make-heading
                          (&aux (components (org-heading-components))))
	    (:type list))
  (marker (point-marker))
  (name (nth 4 components))
  (level (nth 0 components)))

(defconst org-velocity-index
  (eval-when-compile
    (nconc (number-sequence 49 57) 	;numbers
           (number-sequence 97 122)	;lowercase letters
           (number-sequence 65 90)))	;uppercase letters
  "List of chars for indexing results.")

(defconst org-velocity-display-buffer-name "*Velocity headings*")

(defvar org-velocity-search nil
  "Variable to bind to current search.")

(defsubst org-velocity-buffer-file-name (&optional buffer)
  "Return the name of the file BUFFER saves to.
Same as function `buffer-file-name' unless BUFFER is an indirect
buffer or a minibuffer.  In the former case, return the file name
of the base buffer; in the latter, return the file name of
`minibuffer-selected-window' (or its base buffer)."
  (let ((buffer (if (minibufferp buffer)
                    (window-buffer (minibuffer-selected-window))
                  buffer)))
    (buffer-file-name
     (or (buffer-base-buffer buffer)
         buffer))))

(defun org-velocity-minibuffer-contents ()
  "Return the contents of the minibuffer when it is active."
  (if (active-minibuffer-window)
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (minibuffer-contents))))

(defun org-velocity-use-file ()
  "Return the proper file for Org-Velocity to search.
If `org-velocity-always-use-bucket' is t, use bucket file; complain
if missing.  Otherwise if this is an Org file, use it."
  (or
   ;; Use the target in in remember buffers.
   (if (and (boundp 'org-remember-mode) org-remember-mode)
       org-default-notes-file)
   (let ((org-velocity-bucket
          (and org-velocity-bucket (expand-file-name org-velocity-bucket)))
         (buffer (if (org-velocity-buffer-file-name)
                     ;; Use the target in capture buffers.
                     (org-find-base-buffer-visiting (org-velocity-buffer-file-name)))))
     (if org-velocity-always-use-bucket
         (or org-velocity-bucket (error "Bucket required but not defined"))
       (if (and (eq (buffer-local-value 'major-mode (or buffer (current-buffer)))
                    'org-mode)
		(org-velocity-buffer-file-name))
	   (org-velocity-buffer-file-name)
	 (or org-velocity-bucket
	     (error "No bucket and not an Org file")))))))

(defsubst org-velocity-display-buffer ()
  "Return the proper buffer for Org-Velocity to display in."
  (get-buffer-create org-velocity-display-buffer-name))

(defsubst org-velocity-bucket-buffer ()
  "Return proper buffer for bucket operations."
  (find-file-noselect (org-velocity-use-file)))

(defun org-velocity-nearest-heading (position)
  "Return last heading at POSITION.
If there is no last heading, return nil."
  (save-excursion
    (goto-char position)
    ;; If we are before the first heading we could still be at the
    ;; first heading.
    (unless (and (org-before-first-heading-p)
                 (not (org-at-heading-p)))
      (org-back-to-heading t)
      (let ((heading (org-velocity-make-heading)))
        (if org-velocity-max-depth
            (if (<= (org-velocity-heading-level heading)
                    org-velocity-max-depth)
                heading)
          heading)))))

(defun org-velocity-make-button-action (heading)
  "Return a form to visit HEADING."
  `(lambda (button)
     (run-hooks 'mouse-leave-buffer-hook) ;turn off temporary modes
     (if org-velocity-use-search-ring
         (add-to-history 'search-ring ,org-velocity-search search-ring-max))
     (if org-velocity-edit-indirectly
         (org-velocity-edit-entry ',heading)
       (progn
         (message "%s" ,(org-velocity-heading-name heading))
         (org-pop-to-buffer-same-window (marker-buffer
                            ,(org-velocity-heading-marker heading)))
         (goto-char (marker-position
                     ,(org-velocity-heading-marker heading)))))))

(defun org-velocity-make-indirect-buffer (heading)
  "Make or switch to an indirect buffer visiting HEADING."
  (let* ((bucket (marker-buffer (org-velocity-heading-marker heading)))
         (name (org-velocity-heading-name heading))
         (existing (get-buffer name)))
    (if (and existing (buffer-base-buffer existing)
             (equal (buffer-base-buffer existing) bucket))
        existing
      (make-indirect-buffer
       bucket
       (generate-new-buffer-name (org-velocity-heading-name heading))))))

(defun org-velocity-edit-entry (heading)
  "Edit entry at HEADING in an indirect buffer."
  (let ((buffer (org-velocity-make-indirect-buffer heading)))
    (with-current-buffer buffer
      (let ((org-inhibit-startup t))
	(org-mode))
      (goto-char (marker-position (org-velocity-heading-marker heading)))
      (narrow-to-region (point)
			(save-excursion
			  (org-end-of-subtree t)
			  (point)))
      (goto-char (point-min))
      (add-hook 'org-ctrl-c-ctrl-c-hook 'org-velocity-dismiss nil t))
    (pop-to-buffer buffer)
    (set (make-local-variable 'header-line-format)
	 (format "%s Use C-c C-c to finish."
		 (abbreviate-file-name
		  (buffer-file-name
		   (marker-buffer
		    (org-velocity-heading-marker heading))))))))

(defun org-velocity-dismiss ()
  "Save current entry and close indirect buffer."
  (progn
    (save-buffer)
    (kill-buffer)))

(defun org-velocity-buttonize-no-hints (heading)
  "Insert HEADING as a text button with no hints."
  (let ((action (org-velocity-make-button-action heading)))
    (insert-text-button
     (org-velocity-heading-name heading)
     'action action))
  (newline))

(defun org-velocity-buttonize (heading)
  "Insert HEADING as a text button with an hint."
  (insert (format "#%c " (nth (1- (line-number-at-pos))
			      org-velocity-index)))
  (org-velocity-buttonize-no-hints heading))

(defun org-velocity-remember ()
  "Use `org-remember' to record a note."
  (let ((org-remember-templates
	 org-velocity-remember-templates))
    (call-interactively 'org-remember)
    (when org-remember-mode
      (set (make-local-variable 'remember-buffer)
           (rename-buffer org-velocity-search t)))))

(defun org-velocity-capture ()
  "Use `org-capture' to record a note."
  (let ((org-capture-templates
	 org-velocity-capture-templates))
    (when (fboundp 'org-capture) ;; quiet compiler
      (call-interactively 'org-capture)
      (if org-capture-mode (rename-buffer org-velocity-search t)))))

(defun org-velocity-insert-heading (&optional heading)
  "Add a new heading named HEADING and go to it."
  (let ((heading (or heading org-velocity-search)))
    (pop-to-buffer (org-velocity-bucket-buffer))
    (goto-char (point-max))
    (let ((inhibit-quit t))
      (newline)
      (org-insert-heading t t) (insert heading)
      (newline)
      (goto-char (point-max)))))

(defun org-velocity-generic-search (search)
  "Return entries containing SEARCH."
  (save-excursion
    (loop initially (goto-char (point-min))
          while (re-search-forward search (point-max) t)
          if (org-velocity-nearest-heading (match-beginning 0))
          collect it
          do (outline-next-heading))))

(defsubst org-velocity-phrase-search (search)
  "Return entries containing SEARCH as a phrase."
  (org-velocity-generic-search (regexp-quote search)))

(defsubst org-velocity-any-search (search)
  "Return entries containing any word in SEARCH."
  (org-velocity-generic-search (regexp-opt (split-string search))))

(defsubst org-velocity-regexp-search (search)
  (condition-case lossage
      (org-velocity-generic-search search)
    (invalid-regexp (minibuffer-message "%s" lossage))))

(defun org-velocity-all-search (search)
  "Return entries containing all words in SEARCH."
  (save-excursion
    (let ((keywords (mapcar 'regexp-quote (split-string search))))
      (delq nil
            (org-map-entries
             (lambda ()
               ;; Only search the subtree once.
               (setq org-map-continue-from
                     (save-excursion (org-end-of-subtree t) (point)))
               (if (loop for word in keywords
                         always (save-excursion
                                  (re-search-forward
                                   word org-map-continue-from t)))
                   (org-velocity-nearest-heading (point)))))))))

(defun org-velocity-present (headings &optional no-hints search)
  "Buttonize HEADINGS in `org-velocity-display-buffer'.
If NO-HINTS is non-nil, display entries without indices.
SEARCH binds `org-velocity-search'."
  (and (listp headings) (delete-dups headings))
  (let ((cdr (nthcdr
	      (1- (length org-velocity-index))
	      headings)))
    (and (consp cdr) (setcdr cdr nil)))
  (let ((org-velocity-search search))
    (with-current-buffer (org-velocity-display-buffer)
      (mapc
       (if no-hints 'org-velocity-buttonize-no-hints
         'org-velocity-buttonize)
       headings)
      (goto-char (point-min)))))

(defun org-velocity-create-1 ()
  "Create a new heading.
The possible methods are `org-velocity-capture',
`org-velocity-remember', or `org-velocity-create', in
that order.  Which is preferred is determined by
`org-velocity-create-method'."
  (funcall
   (ecase org-velocity-create-method
     (capture (or (and (featurep 'org-capture) 'org-velocity-capture)
		  (and (featurep 'org-remember) 'org-velocity-remember)
		  'org-velocity-insert-heading))
     (remember (or (and (featurep 'org-remember) 'org-velocity-remember)
		   'org-velocity-insert-heading))
     (buffer 'org-velocity-insert-heading))))

(defun org-velocity-store-link ()
  "Function for `org-store-link-functions'."
  (if org-velocity-search
      (org-store-link-props
       :search org-velocity-search)))

(add-hook 'org-store-link-functions 'org-velocity-store-link)

(defun org-velocity-create (search &optional ask)
  "Create new heading named SEARCH.
If ASK is non-nil, ask first."
  (when (or (null ask) (y-or-n-p "No match found, create? "))
    (let ((org-velocity-search search)
	  (org-default-notes-file (org-velocity-use-file))
	  ;; save a stored link
	  org-store-link-plist)
      (org-velocity-create-1))
    search))

(defun org-velocity-get-matches (search)
  "Return matches for SEARCH in current bucket.
Use method specified by `org-velocity-search-method'."
  (when (and search (not (string-equal "" search)))
    (with-current-buffer (org-velocity-bucket-buffer)
      ;; Fold case if the search string is lowercase.
      (let ((case-fold-search (equal search (downcase search))))
        (case org-velocity-search-method
          ('phrase (org-velocity-phrase-search search))
          ('any    (org-velocity-any-search search))
          ('all    (org-velocity-all-search search))
          ('regexp (org-velocity-regexp-search search)))))))

(defun org-velocity-engine (search)
  "Display a list of headings where SEARCH occurs."
  (with-current-buffer (org-velocity-display-buffer)
    (erase-buffer)
    (setq cursor-type nil))
  (unless (or
	   (not (stringp search))
	   (string-equal "" search))	;exit on empty string
    (case
        (if (and org-velocity-force-new (eq last-command-event ?\C-j))
            'force
          (with-current-buffer (org-velocity-bucket-buffer)
            (save-excursion
              (let ((matches (org-velocity-get-matches search)))
                (org-velocity-present matches nil search)
                (cond ((zerop (length matches)) 'new)
                      ((= (length matches) 1)   'follow)
                      ((> (length matches) 1)   'prompt))))))
      ('prompt (progn
		 (Electric-pop-up-window (org-velocity-display-buffer))
		 (let ((hint (org-velocity-electric-follow-hint)))
		   (if hint
		       (case hint
			 (edit (org-velocity-read nil search))
                         (force (org-velocity-create search))
			 (otherwise (org-velocity-activate-button hint)))))))
      ('new (unless (org-velocity-create search t)
	      (org-velocity-read nil search)))
      ('force (org-velocity-create search))
      ('follow (if (y-or-n-p "One match, follow? ")
		   (progn
		     (set-buffer (org-velocity-display-buffer))
		     (goto-char (point-min))
		     (button-activate (next-button (point))))
		 (org-velocity-read nil search))))))

(defun org-velocity-position (item list)
  "Return first position of ITEM in LIST."
  (loop for elt in list
	for i from 0
	if (equal elt item)
	return i))

(defun org-velocity-activate-button (char)
  "Go to button on line number associated with CHAR in `org-velocity-index'."
  (goto-char (point-min))
  (forward-line (org-velocity-position char org-velocity-index))
  (goto-char
   (button-start
    (next-button (point))))
  (message "%s" (button-label (button-at (point))))
  (button-activate (button-at (point))))

(defun org-velocity-electric-undefined ()
  "Complain about an undefined key."
  (interactive)
  (message "%s"
	   (substitute-command-keys
	    "\\[org-velocity-electric-new] for new entry, \\[org-velocity-electric-edit] to edit search, \\[scroll-up] to scroll."))
  (sit-for 4))

(defun org-velocity-electric-follow (ev)
  "Follow a hint indexed by keyboard event EV."
  (interactive (list last-command-event))
  (if (not (> (org-velocity-position ev org-velocity-index)
              (1- (count-lines (point-min) (point-max)))))
      (throw 'org-velocity-select ev)
    (call-interactively 'org-velocity-electric-undefined)))

(defun org-velocity-electric-click (ev)
  "Follow hint indexed by a mouse event EV."
  (interactive "e")
  (throw 'org-velocity-select
	 (nth (1- (count-lines
		   (point-min)
		   (posn-point (event-start ev))))
	      org-velocity-index)))

(defun org-velocity-electric-edit ()
  "Edit the search string."
  (interactive)
  (throw 'org-velocity-select 'edit))

(defun org-velocity-electric-new ()
  "Force a new entry."
  (interactive)
  (throw 'org-velocity-select 'force))

(defvar org-velocity-electric-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'org-velocity-electric-undefined)
    (loop for c in org-velocity-index
	  do (define-key map (char-to-string c) 'org-velocity-electric-follow))
    (define-key map "0" 'org-velocity-electric-new)
    (define-key map [tab] 'scroll-up)
    (define-key map [return] 'org-velocity-electric-edit)
    (define-key map [mouse-1] 'org-velocity-electric-click)
    (define-key map [mouse-2] 'org-velocity-electric-click)
    (define-key map [escape escape escape] 'keyboard-quit)
    (define-key map "\C-h" 'help-command)
    map))

(defun org-velocity-electric-follow-hint ()
  "Read index of button electrically."
  (with-current-buffer (org-velocity-display-buffer)
    (use-local-map org-velocity-electric-map)
    (catch 'org-velocity-select
      (Electric-command-loop 'org-velocity-select
			     "Follow: "))))

(defvar org-velocity-incremental-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'org-velocity-click-for-incremental)
    (define-key map [mouse-2] 'org-velocity-click-for-incremental)
    map))

(defun org-velocity-click-for-incremental ()
  "Jump out of search and select hint clicked on."
  (interactive)
  (let ((ev last-command-event))
    (org-velocity-activate-button
     (nth (- (count-lines
              (point-min)
              (posn-point (event-start ev))) 2)
          org-velocity-index)))
  (throw 'click (current-buffer)))

(defun org-velocity-displaying-completions-p ()
  "Is there a *Completions* buffer showing?"
  (get-window-with-predicate
   (lambda (w)
     (eq (buffer-local-value 'major-mode (window-buffer w))
         'completion-list-mode))))

(defun org-velocity-display-for-incremental ()
  "Display results of search without hinting."
  (when (and (sit-for idle-update-delay)
             (not (org-velocity-displaying-completions-p)))
    (let* ((search (org-velocity-minibuffer-contents))
           (matches (org-velocity-get-matches search)))
      (if (zerop (length matches))
          (progn
            (when (get-buffer-window (org-velocity-display-buffer))
              (delete-window
               (get-buffer-window (org-velocity-display-buffer)))
              (select-window (active-minibuffer-window)))
            (unless (string-equal search "")
              (minibuffer-message "No match; RET to create")))
        (if (and org-velocity-exit-on-match
                 (= (length matches) 1))
            (throw 'click search))
        (with-current-buffer (org-velocity-display-buffer)
          (use-local-map org-velocity-incremental-keymap)
          (erase-buffer)
          (setq cursor-type nil))
        (with-current-buffer (org-velocity-bucket-buffer)
          (org-velocity-present matches t search))
        (display-buffer (org-velocity-display-buffer))))))

(defun org-velocity-dabbrev-completion-list (abbrev)
  "Return all dabbrev completions for ABBREV."
  ;; This is based on `dabbrev-completion'.
  (dabbrev--reset-global-variables)
  (setq dabbrev--last-abbrev abbrev)
  (dabbrev--find-all-expansions abbrev case-fold-search))

(defun org-velocity-read-with-completion (prompt)
  "Completing read with PROMPT."
  (let ((minibuffer-local-completion-map
         minibuffer-local-filename-completion-map)
        (completion-no-auto-exit t)
        (crm-separator " "))
    (funcall
     (case org-velocity-search-method
       (phrase 'completing-read)
       (any 'completing-read-multiple)
       (all 'completing-read-multiple))
     prompt
     (completion-table-dynamic
      'org-velocity-dabbrev-completion-list))))

(defun org-velocity-read-string (prompt &optional initial-input)
  "Read string with PROMPT followed by INITIAL-INPUT."
  ;; The use of initial inputs to the minibuffer is deprecated (see
  ;; `read-from-minibuffer'), but in this case it is the user-friendly
  ;; thing to do.
  (minibuffer-with-setup-hook
      (lexical-let ((initial-input initial-input))
        (lambda ()
          (and initial-input (insert initial-input))
          (goto-char (point-max))))
    (if (eq org-velocity-search-method 'regexp)
	(read-regexp prompt)
      (if (and org-velocity-use-completion
	       ;; map-entries complains for nonexistent files
	       (file-exists-p (org-velocity-use-file)))
	  (org-velocity-read-with-completion prompt)
	(read-string prompt)))))

(defun org-velocity-read-incrementally (prompt)
  "Read string with PROMPT and display results incrementally."
  (let ((res
         (unwind-protect
             (catch 'click
               (add-hook 'post-command-hook
                         'org-velocity-display-for-incremental)
               (if (eq org-velocity-search-method 'regexp)
		   (read-regexp prompt)
		 (if (and org-velocity-use-completion
			  (file-exists-p (org-velocity-use-file)))
		     (org-velocity-read-with-completion prompt)
		   (read-string prompt))))
           (remove-hook 'post-command-hook
                        'org-velocity-display-for-incremental))))
    (if (bufferp res) (org-pop-to-buffer-same-window res) res)))

(defun org-velocity-read (arg &optional search)
  "Read a search string SEARCH for Org-Velocity interface.
This means that a buffer will display all headings where SEARCH
occurs, where one can be selected by a mouse click or by typing
its index.  If SEARCH does not occur, then a new heading may be
created named SEARCH.

If `org-velocity-bucket' is defined and
`org-velocity-always-use-bucket' is non-nil, then the bucket file
will be used; otherwise, this will work when called in any Org
file.  Calling with ARG forces current file."
  (interactive "P")
  (let ((org-velocity-always-use-bucket
	 (if arg nil org-velocity-always-use-bucket)))
    ;; complain if inappropriate
    (assert (org-velocity-use-file))
    (unwind-protect
        (let ((dabbrev-search-these-buffers-only
               (list (org-velocity-bucket-buffer))))
          (org-velocity-engine
           (if org-velocity-search-is-incremental
               (org-velocity-read-incrementally "Velocity search: ")
             (org-velocity-read-string "Velocity search: " search))))
      (progn
	(kill-buffer (org-velocity-display-buffer))
	(delete-other-windows)))))

(provide 'org-velocity)
;;; org-velocity.el ends here
