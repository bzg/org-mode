;;; org-mode.el --- Org major mode                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library contains Org major mode decalration.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'outline)
(require 'org-element)
(require 'ol)
(require 'org-tags)
(require 'org-property)
(require 'org-table)
(require 'org-priority)
(require 'org-font-lock)
(require 'org-macro)
(require 'org-fill)
(require 'org-comment)
(require 'org-pcomplete)
(require 'org-dnd)

(defvar org-enforce-todo-dependencies)
(defvar org-enforce-todo-checkbox-dependencies)
(declare-function org-num-mode "org-num")
(declare-function org-indent-mode "org-indent")
(declare-function org-beamer-mode "ox-beamer")
(declare-function org-agenda-files "org")
(declare-function org-latex-preview "org-latex-preview")

(defcustom org-mode-hook nil
  "Mode hook for Org mode, run after the mode was turned on."
  :group 'org
  :type 'hook)

(defcustom org-insert-mode-line-in-empty-file nil
  "Non-nil means insert the first line setting Org mode in empty files.
When the function `org-mode' is called interactively in an empty file, this
normally means that the file name does not automatically trigger Org mode.
To ensure that the file will always be in Org mode in the future, a
line enforcing Org mode will be inserted into the buffer, if this option
has been set."
  :group 'org-startup
  :type 'boolean)

(defcustom org-ellipsis nil
  "The ellipsis to use in the Org mode outline.

When nil, just use the standard three dots.  When a non-empty string,
use that string instead.

The change affects only Org mode (which will then use its own display table).
Changing this requires executing `\\[org-mode]' in a buffer to become
effective.  It cannot be set as a local variable."
  :group 'org-startup
  :type '(choice (const :tag "Default" nil)
		 (string :tag "String" :value "...#")))

(defvar org-inhibit-startup nil)        ; Dynamically-scoped param.
(defvar org-inhibit-startup-visibility-stuff nil) ; Dynamically-scoped param.

(defgroup org-startup nil
  "Startup options Org uses when first visiting a file."
  :tag "Org Startup"
  :group 'org)

(defcustom org-startup-folded 'showeverything
  "Initial folding state of headings when entering Org mode.

Allowed values are:

symbol `nofold'
  Do not fold headings.

symbol `fold'
  Fold everything, leaving only top-level headings visible.

symbol `content'
  Leave all the headings and sub-headings visible, but hide their
  text.  This is an equivalent of table of contents.

symbol `show2levels', `show3levels', `show4levels', `show5levels'
  Show headings up to Nth level.

symbol `showeverything' (default)
  Start Org mode in fully unfolded state.  Unlike all other allowed
  values, this value prevents drawers, blocks, and archived subtrees
  from being folded even when `org-cycle-hide-block-startup',
  `org-cycle-open-archived-trees', or `org-cycle-hide-drawer-startup'
  are non-nil.  Per-subtree visibility settings (see manual node
  `(org)Initial visibility)') are also ignored.

This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: fold              (or `overview', this is equivalent)
   #+STARTUP: nofold            (or `showall', this is equivalent)
   #+STARTUP: content
   #+STARTUP: show<n>levels (<n> = 2..5)
   #+STARTUP: showeverything

Set `org-agenda-inhibit-startup' to a non-nil value if you want
to ignore this option when Org opens agenda files for the first
time."
  :group 'org-startup
  :package-version '(Org . "9.4")
  :type '(choice
	  (const :tag "nofold: show all" nofold)
	  (const :tag "fold: overview" fold)
	  (const :tag "fold: show two levels" show2levels)
	  (const :tag "fold: show three levels" show3levels)
	  (const :tag "fold: show four levels" show4evels)
	  (const :tag "fold: show five levels" show5levels)
	  (const :tag "content: all headlines" content)
	  (const :tag "show everything, even drawers" showeverything)))

(defcustom org-startup-truncated t
  "Non-nil means entering Org mode will set `truncate-lines'.
This is useful since some lines containing links can be very long and
uninteresting.  Also tables look terrible when wrapped.

The variable `org-startup-truncated' enables you to configure
truncation for Org mode different to the other modes that use the
variable `truncate-lines' and as a shortcut instead of putting
the variable `truncate-lines' into the `org-mode-hook'.  If one
wants to configure truncation for Org mode not statically but
dynamically e.g. in a hook like `ediff-prepare-buffer-hook' then
the variable `truncate-lines' has to be used because in such a
case it is too late to set the variable `org-startup-truncated'."
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-indented nil
  "Non-nil means turn on `org-indent-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: indent
   #+STARTUP: noindent"
  :group 'org-structure
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Globally (slow on startup in large files)" t)))

(defcustom org-startup-numerated nil
  "Non-nil means turn on `org-num-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: num
   #+STARTUP: nonum"
  :group 'org-structure
  :package-version '(Org . "9.4")
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Globally" t)))

(defcustom org-startup-with-beamer-mode nil
  "Non-nil means turn on `org-beamer-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: beamer"
  :group 'org-startup
  :version "24.1"
  :type 'boolean)

(defcustom org-startup-align-all-tables nil
  "Non-nil means align all tables when visiting a file.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: align
   #+STARTUP: noalign"
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-shrink-all-tables nil
  "Non-nil means shrink all table columns with a width cookie.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: shrink"
  :group 'org-startup
  :type 'boolean
  :version "27.1"
  :package-version '(Org . "9.2")
  :safe #'booleanp)

(defvaralias 'org-startup-with-inline-images
  'org-startup-with-link-previews
  "Non-nil means show inline images when loading a new Org file.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: inlineimages
   #+STARTUP: noinlineimages")

(defcustom org-startup-with-link-previews nil
  "Non-nil means show link previews when loading a new Org file.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: linkpreviews
   #+STARTUP: nolinkpreviews"
  :group 'org-startup
  :version "29.4"
  :type 'boolean)

(defcustom org-startup-with-latex-preview nil
  "Non-nil means preview LaTeX fragments when loading a new Org file.

This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: latexpreview
   #+STARTUP: nolatexpreview"
  :group 'org-startup
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defvar org-mode-syntax-table
  (let ((st (make-syntax-table outline-mode-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?~ "_" st)
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    st)
  "Standard syntax table for Org mode buffers.")

(defvar org-display-table nil
  "The display table for Org mode, in case `org-ellipsis' is non-nil.")

;;; Variables for pre-computed regular expressions, all buffer local

(defvar-local org-current-tag-alist nil
  "Alist of all tag groups in current buffer.
This variable takes into consideration `org-tag-alist',
`org-tag-persistent-alist' and TAGS keywords in the buffer.")

(defvar-local org-not-done-keywords nil)
(defvar-local org-tag-groups-alist nil)

(defvar-local org-todo-regexp nil
  "Matches any of the TODO state keywords.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-not-done-heading-regexp nil
  "Matches a TODO headline that is not done.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-complex-heading-regexp nil
  "Matches a headline and puts everything into groups:

group 1: Stars
group 2: The TODO keyword, maybe
group 3: Priority cookie
group 4: True headline
group 5: Tags

Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-complex-heading-regexp-format nil
  "`format' string to make regexp to match an exact headline.
This regexp will match the headline of any node which has the
exact headline text that is put into the format, but may have any
TODO state, priority, tags, statistics cookies (at the beginning
or end of the headline title), or COMMENT keyword.")

(defvar-local org-todo-line-tags-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Also put tags into group 4 if tags are present.")

(defconst org-heading-keyword-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "`format' string for a regexp matching a headline with some keyword.
This regexp will match the headline of any node which has the
exact keyword that is put into the format.  The keyword isn't in
any group by default, but the stars and the body are.")

(defconst org-heading-keyword-maybe-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)?\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "`format' string for a regexp matching a headline, possibly with some keyword.
This regexp can match any headline with the specified keyword, or
without a keyword.  The keyword isn't in any group by default,
but the stars and the body are.")

(defvar org-agenda-file-menu-enabled t
  "When non-nil, refresh Agenda files in Org menu when loading Org.")

(defun org-file-menu-entry (file)
  (vector file (list 'find-file file) t))

(defun org-install-agenda-files-menu ()
  "Install agenda file menu."
  (let ((bl (buffer-list)))
    (save-excursion
      (while bl
	(set-buffer (pop bl))
	(when (derived-mode-p 'org-mode) (setq bl nil)))
      (when (derived-mode-p 'org-mode)
	(easy-menu-change
	 '("Org") "File List for Agenda"
	 (append
	  (list
	   ["Edit File List" (org-edit-agenda-file-list) t]
	   ["Add/Move Current File to Front" org-agenda-file-to-front t]
	   ["Remove Current File" org-remove-file t]
	   ["Cycle Through Files" org-cycle-agenda-files t]
	   ["Occur in Files" org-occur-in-agenda-files t]
	   "--")
	  (mapcar #'org-file-menu-entry
		  ;; Prevent initialization from failing.
		  (ignore-errors (org-agenda-files t)))))))))

(defconst org-startup-options
  '(("fold" org-startup-folded fold)
    ("overview" org-startup-folded overview)
    ("nofold" org-startup-folded nofold)
    ("showall" org-startup-folded showall)
    ("show2levels" org-startup-folded show2levels)
    ("show3levels" org-startup-folded show3levels)
    ("show4levels" org-startup-folded show4levels)
    ("show5levels" org-startup-folded show5levels)
    ("showeverything" org-startup-folded showeverything)
    ("content" org-startup-folded content)
    ("indent" org-startup-indented t)
    ("noindent" org-startup-indented nil)
    ("num" org-startup-numerated t)
    ("nonum" org-startup-numerated nil)
    ("hidestars" org-hide-leading-stars t)
    ("showstars" org-hide-leading-stars nil)
    ("odd" org-odd-levels-only t)
    ("oddeven" org-odd-levels-only nil)
    ("align" org-startup-align-all-tables t)
    ("noalign" org-startup-align-all-tables nil)
    ("shrink" org-startup-shrink-all-tables t)
    ("descriptivelinks" org-link-descriptive t)
    ("literallinks" org-link-descriptive nil)
    ("inlineimages" org-startup-with-link-previews t)
    ("noinlineimages" org-startup-with-link-previews nil)
    ("linkpreviews" org-startup-with-link-previews t)
    ("nolinkpreviews" org-startup-with-link-previews nil)
    ("latexpreview" org-startup-with-latex-preview t)
    ("nolatexpreview" org-startup-with-latex-preview nil)
    ("customtime" org-display-custom-times t)
    ("logdone" org-log-done time)
    ("lognotedone" org-log-done note)
    ("nologdone" org-log-done nil)
    ("lognoteclock-out" org-log-note-clock-out t)
    ("nolognoteclock-out" org-log-note-clock-out nil)
    ("logrepeat" org-log-repeat state)
    ("lognoterepeat" org-log-repeat note)
    ("logdrawer" org-log-into-drawer t)
    ("nologdrawer" org-log-into-drawer nil)
    ("logstatesreversed" org-log-states-order-reversed t)
    ("nologstatesreversed" org-log-states-order-reversed nil)
    ("nologrepeat" org-log-repeat nil)
    ("logreschedule" org-log-reschedule time)
    ("lognotereschedule" org-log-reschedule note)
    ("nologreschedule" org-log-reschedule nil)
    ("logredeadline" org-log-redeadline time)
    ("lognoteredeadline" org-log-redeadline note)
    ("nologredeadline" org-log-redeadline nil)
    ("logrefile" org-log-refile time)
    ("lognoterefile" org-log-refile note)
    ("nologrefile" org-log-refile nil)
    ("fninline" org-footnote-define-inline t)
    ("nofninline" org-footnote-define-inline nil)
    ("fnlocal" org-footnote-section nil)
    ("fnauto" org-footnote-auto-label t)
    ("fnprompt" org-footnote-auto-label nil)
    ("fnconfirm" org-footnote-auto-label confirm)
    ("fnplain" org-footnote-auto-label plain)
    ("fnadjust" org-footnote-auto-adjust t)
    ("nofnadjust" org-footnote-auto-adjust nil)
    ("fnanon" org-footnote-auto-label anonymous)
    ("constcgs" constants-unit-system cgs)
    ("constSI" constants-unit-system SI)
    ("noptag" org-tag-persistent-alist nil)
    ("hideblocks" org-hide-block-startup t)
    ("nohideblocks" org-hide-block-startup nil)
    ("hidedrawers" org-hide-drawer-startup t)
    ("nohidedrawers" org-hide-drawer-startup nil)
    ("beamer" org-startup-with-beamer-mode t)
    ("entitiespretty" org-pretty-entities t)
    ("entitiesplain" org-pretty-entities nil))
  "Variable associated with STARTUP options for Org.
Each element is a list of three items: the startup options (as written
in the #+STARTUP line), the corresponding variable, and the value to set
this variable to if the option is found.  An optional fourth element PUSH
means to push this value onto the list in the variable.")

(defun org-set-regexps-and-options (&optional tags-only)
  "Precompute regular expressions used in the current buffer.
When optional argument TAGS-ONLY is non-nil, only compute tags
related expressions."
  ;; FIXME: The buffer-local values here should eventually become
  ;; unused in the rest of the code, making use of org-data object
  ;; instead.
  (when (derived-mode-p 'org-mode)
    (let ((org-data (org-element-org-data)))
      ;; Startup options.  Get this early since it does change
      ;; behavior for other options (e.g., tags).
      (let ((startup (cl-mapcan #'split-string (org-element-property :STARTUP org-data))))
	(dolist (option startup)
	  (pcase (assoc-string option org-startup-options t)
	    (`(,_ ,variable ,value t)
	     (unless (listp (symbol-value variable))
	       (set (make-local-variable variable) nil))
	     (add-to-list variable value))
	    (`(,_ ,variable ,value . ,_)
	     (set (make-local-variable variable) value))
	    (_ nil))))
      ;; FIXME: This variable is assigned for backwards-compatibility
      ;; and should probably be removed in future.
      (with-suppressed-warnings ((obsolete org-file-tags))
        (setq-local org-file-tags
		    (mapcar #'org-add-prop-inherited
                            (org-element-property :tags org-data))))
      ;; Used for tag completion.
      (setq org-current-tag-alist
	    (org--tag-add-to-alist
	     org-tag-persistent-alist
	     (let ((tags (org-element-property :TAGS org-data)))
	       (if tags
		   (org-tag-string-to-alist
		    (mapconcat #'identity tags "\n"))
		 org-tag-alist))))
      ;; Used for tag completion.
      (setq org-tag-groups-alist
	    (org-tag-alist-to-groups org-current-tag-alist))
      (unless tags-only
        ;; FIXME: `org-keyword-properties' is set for backwards
        ;; compatibility.  org-data element properties should be used
        ;; instead.
	(let ((properties nil))
	  (dolist (value (org-element-property :PROPERTY org-data))
	    (when (string-match "\\(\\S-+\\)[ \t]+\\(.*\\)" value)
	      (setq properties (org--update-property-plist
				(match-string-no-properties 1 value)
				(match-string-no-properties 2 value)
				properties))))
	  (setq-local org-keyword-properties properties))
	;; Archive location.
        ;; FIXME: The users should prefer `org-get-archive-location'
        ;; instead of relying on `org-set-regexps-and-options' to
        ;; parse top-level keyword.
	(let ((archive (org-element-property :ARCHIVE org-data)))
	  (when archive (setq-local org-archive-location archive)))
	;; Category.
	(let ((category (org-element-property :CATEGORY org-data)))
	  (when category
	    (setq-local org-category (intern category))
	    (setq-local org-keyword-properties
			(org--update-property-plist
			 "CATEGORY" category org-keyword-properties))))
	;; Columns.
	(let ((column (org-element-property :COLUMNS org-data)))
	  (when column (setq-local org-columns-default-format column)))
	;; Constants.
	(let ((store nil))
	  (dolist (pair (cl-mapcan #'split-string
				   (org-element-property :CONSTANTS org-data)))
	    (when (string-match "^\\([a-zA-Z0][_a-zA-Z0-9]*\\)=\\(.*\\)" pair)
	      (let* ((name (match-string 1 pair))
		     (value (match-string 2 pair))
		     (old (assoc name store)))
		(if old (setcdr old value)
		  (push (cons name value) store)))))
	  (setq org-table-formula-constants-local store))
	;; Link abbreviations.
        (with-suppressed-warnings ((obsolete org-link-abbrev-alist-local))
          (setq org-link-abbrev-alist-local
                (org-element-property :link-abbrevs org-data)))
	;; Priorities.
	(let ((value (org-element-property :PRIORITIES org-data)))
	  (pcase (and value (split-string value))
	    (`(,high ,low ,default . ,_)
	     (setq-local org-priority-highest (org-priority-to-value high))
	     (setq-local org-priority-lowest (org-priority-to-value low))
	     (setq-local org-priority-default (org-priority-to-value default)))))
	;; Scripts.
	(let ((value (org-element-property :OPTIONS org-data)))
	  (dolist (option value)
	    (when (string-match "\\^:\\(t\\|nil\\|{}\\)" option)
	      (setq-local org-use-sub-superscripts
			  (read (match-string 1 option))))))
	;; TODO keywords.
	(setq-local org-todo-kwd-alist nil)
	(setq-local org-todo-key-alist nil)
	(setq-local org-todo-key-trigger nil)
	(setq-local org-todo-keywords-1 nil)
	(setq-local org-done-keywords nil)
	(setq-local org-todo-heads nil)
	(setq-local org-todo-sets nil)
	(setq-local org-todo-log-states nil)
	(let ((sequences (org-element-property :todo-keyword-settings org-data)))
          ;; org-todo-keywords-1
          (setq org-todo-keywords-1 (org-element-property :todo-keywords org-data))
          ;; org-done-keywords
          (setq org-done-keywords (org-element-property :done-keywords org-data))
          ;; org-todo-heads
          ;; org-todo-sets
          ;; org-todo-log-states
          ;; org-todo-key-alist
          ;; org-todo-kwd-alist
          (dolist (sequence sequences)
            ;; SEQUENCE = (TYPE ((KWD1 . OPT1) (KWD2 . OPT2) ...) ((DONE-KWD1 . OPT1) ...))
            (push (caar (nth 1 sequence)) org-todo-heads)
            (push (mapcar #'car (nth 1 sequence)) org-todo-sets)
            (let (keys
                  (kwd-tail (list (car sequence) ;; TYPE
                                  (car org-todo-heads) ;; First keyword
                                  (car (car (nth 2 sequence))) ;; First done keyword
                                  (car (org-last (nth 2 sequence))) ;; Last done keyword
                                  )))
              (dolist (pair (nth 1 sequence))
                (pcase pair
                  (`(,(and (pred stringp) name) .
                     ,setting)
                   (when (stringp setting)
                     (when-let* ((state-setting (org-extract-log-state-settings (concat name "(" setting ")"))))
                       (push state-setting org-todo-log-states))
                     (push (cons name
                                 (if (string-match "^[^!@/]" setting)
                                     (string-to-char (match-string 0 setting))
                                   nil))
                           keys))
                   (push (cons name kwd-tail) org-todo-kwd-alist))))
              (when keys
                (setq keys (nconc (list (list :startgroup))
                                  (nreverse keys)
                                  (list (list :endgroup))))
                (setq org-todo-key-alist (nconc org-todo-key-alist keys)))))
          (setq org-todo-heads (nreverse org-todo-heads)
                org-todo-sets (nreverse org-todo-sets)
                org-todo-kwd-alist (nreverse org-todo-kwd-alist)
                ;; org-todo-key-trigger
                org-todo-key-trigger (delq nil (mapcar #'cdr org-todo-key-alist))
                org-todo-key-alist (org-assign-fast-keys org-todo-key-alist)))
        
	;; Compute the regular expressions and other local variables.
	;; Using `org-outline-regexp-bol' would complicate them much,
	;; because of the fixed white space at the end of that string.

        ;; FIXME: When is `org-done-keywords' nil?
	(unless org-done-keywords
	  (setq org-done-keywords
		(and org-todo-keywords-1 (last org-todo-keywords-1))))
	(setq org-not-done-keywords
	      (org-delete-all org-done-keywords
			      (copy-sequence org-todo-keywords-1))
	      org-todo-regexp (org-element-property :todo-regexp org-data)
	      org-not-done-regexp (regexp-opt org-not-done-keywords t)
	      org-not-done-heading-regexp
	      (format org-heading-keyword-regexp-format org-not-done-regexp)
	      org-todo-line-regexp
	      (format org-heading-keyword-maybe-regexp-format org-todo-regexp)
	      org-complex-heading-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      (format "\\(?: +\\(%s\\(?:%s\\)%s\\)\\)?"
                              (regexp-quote org-priority-prefix)
                              org-priority-value-regexp
                              (regexp-quote org-priority-suffix))
		      "\\(?: +\\(.*?\\)\\)??"
                      org-tag--group-optional-re)
	      org-complex-heading-regexp-format
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      (format "\\(?: +\\(%s\\(?:%s\\)%s\\)\\)?"
                              (regexp-quote org-priority-prefix)
                              org-priority-value-regexp
                              (regexp-quote org-priority-suffix))
		      "\\(?: +"
                      ;; Headline might be commented
                      "\\(?:" org-comment-string " +\\)?"
		      ;; Stats cookies can be stuck to body.
		      "\\(?:\\[[0-9%%/]+\\] *\\)*"
		      "\\(%s\\)"
		      "\\(?: *\\[[0-9%%/]+\\]\\)*"
		      "\\)"
                      ;; Shield % inside as they will break `format'.
		      (replace-regexp-in-string "%" "%%" org-tag--group-optional-re))
	      org-todo-line-tags-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(.*?\\)\\)??"
                      org-tag--group-optional-re))
	(org-compute-latex-and-related-regexp)))))

(defun org--tag-add-to-alist (alist1 alist2)
  "Merge tags from ALIST1 into ALIST2.

Duplicates tags outside a group are removed.  Keywords and order
are preserved.

The function assumes ALIST1 and ALIST2 are proper tag alists.
See `org-tag-alist' for their structure."
  (cond
   ((null alist2) alist1)
   ((null alist1) alist2)
   (t
    (let ((to-add nil)
	  (group-flag nil))
      (dolist (tag-pair alist1)
	(pcase tag-pair
	  (`(,(or :startgrouptag :startgroup))
	   (setq group-flag t)
	   (push tag-pair to-add))
	  (`(,(or :endgrouptag :endgroup))
	   (setq group-flag nil)
	   (push tag-pair to-add))
	  (`(,(or :grouptags :newline))
	   (push tag-pair to-add))
	  (`(,tag . ,_)
	   ;; Remove duplicates from ALIST1, unless they are in
	   ;; a group.  Indeed, it makes sense to have a tag appear in
	   ;; multiple groups.
	   (when (or group-flag (not (assoc tag alist2)))
	     (push tag-pair to-add)))
	  (_ (error "Invalid association in tag alist: %S" tag-pair))))
      ;; Preserve order of ALIST1.
      (append (nreverse to-add) alist2)))))

;; FIXME: This is very specialized.  Should it be made internal? Or
;; removed?
(defun org-extract-log-state-settings (x)
  "Extract the log state setting from a TODO keyword string.
This will extract info from a string like \"WAIT(w@/!)\"."
  (when (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?\\([!@]\\)?\\(?:/\\([!@]\\)\\)?)\\)?$" x)
    (let ((kw (match-string 1 x))
	  (log1 (and (match-end 3) (match-string 3 x)))
	  (log2 (and (match-end 4) (match-string 4 x))))
      (and (or log1 log2)
	   (list kw
		 (and log1 (if (equal log1 "!") 'time 'note))
		 (and log2 (if (equal log2 "!") 'time 'note)))))))

(defun org-assign-fast-keys (alist)
  "Assign fast keys to a keyword-key ALIST.
Respect keys that are already there."
  (let (new e (alt ?0))
    (while (setq e (pop alist))
      (if (or (memq (car e) '(:newline :grouptags :endgroup :startgroup))
	      (cdr e)) ;; Key already assigned.
	  (push e new)
	(let ((clist (string-to-list (downcase (car e))))
	      (used (append new alist)))
	  (when (= (car clist) ?@)
	    (pop clist))
	  (while (and clist (rassoc (car clist) used))
	    (pop clist))
	  (unless clist
	    (while (rassoc alt used)
	      (cl-incf alt)))
	  (push (cons (car e) (or (car clist) alt)) new))))
    (nreverse new)))

(defun org-find-invisible-foreground ()
  (let ((candidates (remove
		     "unspecified-bg"
		     (nconc
		      (list (face-background 'default)
			    (face-background 'org-default))
		      (mapcar
		       (lambda (alist)
			 (when (boundp alist)
			   (cdr (assq 'background-color (symbol-value alist)))))
		       '(default-frame-alist initial-frame-alist window-system-default-frame-alist))
		      (list (face-foreground 'org-hide))))))
    (car (remove nil candidates))))

(defun org--link-at-point ()
  "`thing-at-point' provider function."
  (org-element-property :raw-link (org-element-context)))

(defun org--bounds-of-link-at-point ()
  "`bounds-of-thing-at-point' provider function."
  (let ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (cons (org-element-begin context)
            (org-element-end context)))))

(define-derived-mode org-mode outline-mode "Org"
  "Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}"
  (setq-local org-mode-loading t)
  ;; Force tab width - indentation is significant in lists, so we need
  ;; to make sure that it is consistent across configurations.
  (org--set-tab-width)
  ;; Really force it, even if dir-locals or file-locals set it - we
  ;; need tab-width = 8 as a part of Org syntax.
  (add-hook 'hack-local-variables-hook
            #'org--set-tab-width 90 'local)
  ;; In Emacs <30, editorconfig-mode uses advices, so we cannot rely
  ;; upon `hack-local-variables-hook' to run after editorconfig
  ;; tab-width settings are applied.
  (add-hook 'editorconfig-after-apply-functions
            #'org--set-tab-width 90 'local)
  (org-load-modules-maybe)
  (when org-agenda-file-menu-enabled
    (org-install-agenda-files-menu))
  (setq-local outline-regexp org-outline-regexp)
  (setq-local outline-level 'org-outline-level)
  ;; Initialize cache.
  (org-element-cache-reset)
  (when (and org-element-cache-persistent
             org-element-use-cache)
    (org-persist-load
     `((elisp org-element--cache) (version ,org-element-cache-version))
     (current-buffer)
     'match-hash :read-related t))
  (org-set-regexps-and-options)
  (add-to-invisibility-spec '(org-link))
  (add-to-invisibility-spec '(org-emphasis))
  (add-to-invisibility-spec '(org-raise))
  (org-fold-initialize (or (and (stringp org-ellipsis) (not (equal "" org-ellipsis)) org-ellipsis)
                           "..."))
  (make-local-variable 'org-link-descriptive)
  (when (eq org-fold-core-style 'overlays) (add-to-invisibility-spec '(org-hide-block . t)))
  (when (and (stringp org-ellipsis) (not (equal "" org-ellipsis)))
    (unless org-display-table
      (setq org-display-table (make-display-table)))
    (set-display-table-slot
     org-display-table 4
     (vconcat (mapcar (lambda (c) (make-glyph-code c 'org-ellipsis))
		      org-ellipsis)))
    (setq buffer-display-table org-display-table))
  (org-set-font-lock-defaults)
  (when (and org-tag-faces (not org-tags-special-faces-re))
    ;; tag faces set outside customize.... force initialization.
    (org-set-tag-faces 'org-tag-faces org-tag-faces))
  ;; Calc embedded
  (setq-local calc-embedded-open-mode "# ")
  ;; Set syntax table.  Ensure that buffer-local changes to the syntax
  ;; table do not affect other Org buffers.
  (set-syntax-table (make-syntax-table org-mode-syntax-table))
  (setq-local font-lock-unfontify-region-function 'org-unfontify-region)
  ;; Activate before-change-function
  (setq-local org-table-may-need-update t)
  (add-hook 'before-change-functions 'org-before-change-function nil 'local)
  ;; Check for running clock before killing a buffer
  (add-hook 'kill-buffer-hook 'org-check-running-clock nil 'local)
  ;; Check for invisible edits.
  (org-fold--advice-edit-commands)
  ;; Initialize macros templates.
  (org-macro-initialize-templates)
  ;; Initialize radio targets.
  (org-update-radio-target-regexp)
  ;; Indentation.
  (setq-local indent-line-function #'org-indent-line)
  (setq-local indent-region-function #'org-indent-region)
  (setq-local electric-indent-inhibit t)
  ;; Filling and auto-filling.
  (org-setup-filling)
  ;; Comments.
  (org-setup-comments-handling)
  ;; Obey the syntax-table text property when navigating text (used in
  ;; source blocks).
  (setq-local parse-sexp-lookup-properties t)
  ;; Beginning/end of defun
  (setq-local beginning-of-defun-function
              (lambda ()
                (condition-case _
                    (org-backward-element)
                  ;; Already at the beginning.
                  (user-error t))))
  (setq-local end-of-defun-function
	      (lambda ()
                (condition-case _
		    (if (not (org-at-heading-p))
		        (org-forward-element)
		      (org-forward-element)
		      (forward-char -1))
                  ;; At the last element. Move to eob.
                  (user-error
                   (goto-char (point-max))))))
  ;; Next error for sparse trees
  (setq-local next-error-function 'org-occur-next-match)
  ;; Make commit log messages from Org documents easier.
  (setq-local add-log-current-defun-function #'org-add-log-current-headline)
  ;; Make sure dependence stuff works reliably, even for users who set it
  ;; too late :-(
  (if org-enforce-todo-dependencies
      (add-hook 'org-blocker-hook
		'org-block-todo-from-children-or-siblings-or-parent)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-children-or-siblings-or-parent))
  (if org-enforce-todo-checkbox-dependencies
      (add-hook 'org-blocker-hook
		'org-block-todo-from-checkboxes)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-checkboxes))

  ;; Align options lines
  (setq-local
   align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^[ \t]*#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode)))))

  ;; Setup the pcomplete hooks
  (setq-local pcomplete-command-completion-function #'org-pcomplete-initial)
  (setq-local pcomplete-command-name-function #'org-command-at-point)
  (setq-local pcomplete-default-completion-function #'ignore)
  (setq-local pcomplete-parse-arguments-function #'org-parse-arguments)
  (setq-local pcomplete-termination-string "")
  (add-hook 'completion-at-point-functions
            #'pcomplete-completions-at-point nil t)
  (setq-local buffer-face-mode-face 'org-default)

  ;; `thing-at-point' support
  (when (boundp 'thing-at-point-provider-alist)
    (setq-local thing-at-point-provider-alist
                (cons '(url . org--link-at-point)
                      thing-at-point-provider-alist)))
  (when (boundp 'forward-thing-provider-alist)
    (setq-local forward-thing-provider-alist
                (cons '(url . org-next-link)
                      forward-thing-provider-alist))
    (setq-local forward-thing-provider-alist
                (cons '(sentence . org--forward-sentence-thing-at-point-provider)
                      forward-thing-provider-alist)))
  (when (boundp 'bounds-of-thing-at-point-provider-alist)
    (setq-local bounds-of-thing-at-point-provider-alist
                (cons '(url . org--bounds-of-link-at-point)
                      bounds-of-thing-at-point-provider-alist)))

  ;; If empty file that did not turn on Org mode automatically, make
  ;; it to.
  (when (and org-insert-mode-line-in-empty-file
	     (called-interactively-p 'any)
	     (= (point-min) (point-max)))
    (insert "#    -*- mode: org -*-\n\n"))
  (unless org-inhibit-startup
    (when (or org-startup-align-all-tables org-startup-shrink-all-tables)
      (org-table-map-tables
       (cond ((and org-startup-align-all-tables
		   org-startup-shrink-all-tables)
	      (lambda () (org-table-align) (org-table-shrink)))
	     (org-startup-align-all-tables #'org-table-align)
	     (t #'org-table-shrink))
       t 'org))
    ;; Suppress modification hooks to speed up the startup.
    ;; However, do it only when text properties/overlays, but not
    ;; buffer text are actually modified.  We still need to track text
    ;; modifications to make cache updates work reliably.
    (org-unmodified
     (when org-startup-with-beamer-mode (org-beamer-mode))
     (when org-startup-with-inline-images (org-link-preview '(16)))
     (when org-startup-with-latex-preview (org-latex-preview 'buffer))
     (unless org-inhibit-startup-visibility-stuff (org-cycle-set-startup-visibility))
     (when org-startup-truncated (setq truncate-lines t))
     (when org-startup-numerated (require 'org-num) (org-num-mode 1))
     (when org-startup-indented (require 'org-indent) (org-indent-mode 1))))

  ;; Add a custom keymap for `visual-line-mode' so that activating
  ;; this minor mode does not override Org's keybindings.
  ;; FIXME: Probably `visual-line-mode' should take care of this.
  (let ((oldmap (cdr (assoc 'visual-line-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap [remap move-beginning-of-line] nil)
    (define-key newmap [remap move-end-of-line] nil)
    (define-key newmap [remap kill-line] nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(visual-line-mode . ,newmap) minor-mode-overriding-map-alist))

  ;; Activate `org-table-header-line-mode'
  (when org-table-header-line-p
    (org-table-header-line-mode 1))
  ;; Try to set `org-hide' face correctly.
  (let ((foreground (org-find-invisible-foreground)))
    (when foreground
      (set-face-foreground 'org-hide foreground)))
  ;; Set face extension as requested.
  (org--set-faces-extend '(org-block-begin-line org-block-end-line)
                         org-fontify-whole-block-delimiter-line)
  (org--set-faces-extend org-level-faces org-fontify-whole-heading-line)
  (setq-local org-mode-loading nil)

  ;; `yank-media' handler and DND support.
  (org-setup-yank-dnd-handlers)
  ;; Remove folds when changing major mode
  (add-hook 'change-major-mode-hook
            #'org-fold-show-all 'append 'local))

(provide 'org-mode)

;;; org-mode.el ends here
