;;; org-src.el --- Source code examples in Org
;;
;; Copyright (C) 2004-2014 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg@gnu.org>
;;         Dan Davison <davison at stats dot ox dot ac dot uk>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with source code examples in Org-mode.

;;; Code:

(require 'org-macs)
(require 'org-compat)
(require 'ob-keys)
(require 'ob-comint)
(eval-when-compile
  (require 'cl))

(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-switch-to-buffer-other-window "org" (&rest args))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))
(declare-function org-base-buffer "org" (buffer))

(defcustom org-edit-src-turn-on-auto-save nil
  "Non-nil means turn `auto-save-mode' on when editing a source block.
This will save the content of the source code editing buffer into
a newly created file, not the base buffer for this source block.

If you want to regularly save the base buffer instead of the source
code editing buffer, see `org-edit-src-auto-save-idle-delay' instead."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-edit-src-auto-save-idle-delay 0
  "Delay before saving a source code buffer back into its base buffer.
When a positive integer N, save after N seconds of idle time.
When 0 (the default), don't auto-save.

If you want to save the source code buffer itself, don't use this.
Check `org-edit-src-turn-on-auto-save' instead."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-coderef-label-format "(ref:%s)"
  "The default coderef format.
This format string will be used to search for coderef labels in literal
examples (EXAMPLE and SRC blocks).  The format can be overwritten in
an individual literal example with the -l option, like

#+BEGIN_SRC pascal +n -r -l \"((%s))\"
...
#+END_SRC

If you want to use this for HTML export, make sure that the format does
not introduce special font-locking, and avoid the HTML special
characters `<', `>', and `&'.  The reason for this restriction is that
the labels are searched for only after htmlize has done its job."
  :group 'org-edit-structure ; FIXME this is not in the right group
  :type 'string)

(defcustom org-edit-fixed-width-region-mode 'artist-mode
  "The mode that should be used to edit fixed-width regions.
These are the regions where each line starts with a colon."
  :group 'org-edit-structure
  :type '(choice
	  (const artist-mode)
	  (const picture-mode)
	  (const fundamental-mode)
	  (function :tag "Other (specify)")))

(defcustom org-src-preserve-indentation nil
  "If non-nil preserve leading whitespace characters on export.
If non-nil leading whitespace characters in source code blocks
are preserved on export, and when switching between the org
buffer and the language mode edit buffer.

When this variable is nil, after editing with \\[org-edit-src-code],
the minimum (across-lines) number of leading whitespace characters
are removed from all lines, and the code block is uniformly indented
according to the value of `org-edit-src-content-indentation'."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-edit-src-content-indentation 2
  "Indentation for the content of a source code block.
This should be the number of spaces added to the indentation of the #+begin
line in order to compute the indentation of the block content after
editing it with \\[org-edit-src-code].  Has no effect if
`org-src-preserve-indentation' is non-nil."
  :group 'org-edit-structure
  :type 'integer)

(defcustom org-edit-src-persistent-message t
  "Non-nil means show persistent exit help message while editing src examples.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-src-ask-before-returning-to-edit-buffer t
  "Non-nil means ask before switching to an existing edit buffer.
If nil, when `org-edit-src-code' is used on a block that already
has an active edit buffer, it will switch to that edit buffer
immediately; otherwise it will ask whether you want to return to
the existing edit buffer."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-src-window-setup 'reorganize-frame
  "How the source code edit buffer should be displayed.
Possible values for this option are:

current-window    Show edit buffer in the current window, keeping all other
                  windows.
other-window      Use `switch-to-buffer-other-window' to display edit buffer.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the edit buffer.  When exiting the edit buffer,
                  return to one window.
other-frame       Use `switch-to-buffer-other-frame' to display edit buffer.
                  Also, when exiting the edit buffer, kill that frame."
  :group 'org-edit-structure
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-window)
	  (const reorganize-frame)))

(defvar org-src-mode-hook nil
  "Hook  run after Org switched a source code snippet to its Emacs mode.
This hook will run

- when editing a source code snippet with \"C-c '\".
- When formatting a source code snippet for export with htmlize.

You may want to use this hook for example to turn off `outline-minor-mode'
or similar things which you want to have when editing a source code file,
but which mess up the display of a snippet in Org exported files.")

(defcustom org-src-lang-modes
  '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
    ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
    ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
    ("screen" . shell-script) ("shell" . sh) ("bash" . sh))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the string that should
be inserted as the name of the major mode.  For many languages this is
simple, but for language where this is not the case, this variable
provides a way to simplify things on the user side.
For example, there is no ocaml-mode in Emacs, but the mode to use is
`tuareg-mode'."
  :group 'org-edit-structure
  :type '(repeat
	  (cons
	   (string "Language name")
	   (symbol "Major mode"))))

(defcustom org-src-tab-acts-natively nil
  "If non-nil, the effect of TAB in a code block is as if it were
issued in the language major mode buffer."
  :type 'boolean
  :version "24.1"
  :group 'org-babel)



;;; Internal functions and variables

(defvar org-src--allow-write-back-p t)
(defvar org-src--beg-marker nil)
(defvar org-src--block-indentation nil)
(defvar org-src--code-timer nil)
(defvar org-src--end-marker nil)
(defvar org-src--from-org-mode nil)
(defvar org-src--overlay nil)
(defvar org-src--saved-temp-window-config nil)
(defvar org-src--type nil)
(defvar org-src--babel-info nil)

(defun org-src--construct-edit-buffer-name (org-buffer-name lang)
  "Construct the buffer name for a source editing buffer."
  (concat "*Org Src " org-buffer-name "[ " lang " ]*"))

(defun org-src--edit-buffer (beg end)
  "Return buffer editing area between BEG and END.
Return nil if there is no such buffer."
  (catch 'exit
    (dolist (b (buffer-list))
      (with-current-buffer b
	(and (org-src-edit-buffer-p)
	     (eq beg org-src--beg-marker)
	     (eq end org-src--end-marker)
	     (throw 'exit b))))))

(defun org-src--source-buffer ()
  "Return source buffer edited by current buffer."
  (unless (org-src-edit-buffer-p) (error "Not in a source buffer"))
  (or (marker-buffer org-src--beg-marker)
      (error "No source buffer available for current editing session")))

(defun org-src--get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (intern
   (concat
    (let ((l (or (cdr (assoc lang org-src-lang-modes)) lang)))
      (if (symbolp l) (symbol-name l) l))
    "-mode")))

(defun org-src--coordinates (pos beg end)
  "Return coordinates of POS relatively to BEG and END.
POS, BEG and END are buffer positions.  Return value is either
a cons cell (LINE . COLUMN) or symbol `end'.  See also
`org-src--goto-coordinates'."
  (if (>= pos end) 'end
    (org-with-wide-buffer
     (goto-char (max beg pos))
     (cons (count-lines beg (line-beginning-position))
	   ;; Column is relative to the end of line to avoid problems of
	   ;; comma escaping or colons appended in front of the line.
	   (- (current-column)
	      (progn (end-of-line) (current-column)))))))

(defun org-src--goto-coordinates (coord beg end)
  "Move to coordinates COORD relatively to BEG and END.
COORD are coordinates, as returned by `org-src--coordinates',
which see.  BEG and END are buffer positions."
  (goto-char
   (if (eq coord 'end) (max (1- end) beg)
     ;; If BEG happens to be located outside of the narrowed part of
     ;; the buffer, widen it first.
     (org-with-wide-buffer
      (goto-char beg)
      (forward-line (car coord))
      (end-of-line)
      (org-move-to-column (max (+ (current-column) (cdr coord)) 0))
      (point)))))

(defun org-src--element-contents-area (element)
  "Return contents boundaries of ELEMENT.
Return value is a pair (BEG . END) where BEG and END are buffer
positions."
  (let ((blockp (memq (org-element-type element)
		      '(example-block export-block src-block))))
    (cons (org-with-wide-buffer
	   (goto-char (org-element-property :post-affiliated element))
	   (if blockp (line-beginning-position 2) (point)))
	  (org-with-wide-buffer
	   (goto-char (org-element-property :end element))
	   (skip-chars-backward " \r\t\n")
	   (line-beginning-position (if blockp 1 2))))))

(defun org-src--make-source-overlay (beg end edit-buffer)
  "Create overlay between BEG and END positions and return it.
EDIT-BUFFER is the buffer currently editing area between BEG and
END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'edit-buffer edit-buffer)
    (overlay-put overlay 'help-echo
		 "Click with mouse-1 to switch to buffer editing this segment")
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'keymap
		 (let ((map (make-sparse-keymap)))
		   (define-key map [mouse-1] 'org-edit-src-continue)
		   map))
    (overlay-put overlay :read-only "Leave me alone")
    overlay))

(defun org-src--on-element-p (element)
  "Non-nil when point is on ELEMENT."
  (and (>= (point) (org-element-property :begin element))
       (<= (point)
	   (org-with-wide-buffer
	    (goto-char (org-element-property :end element))
	    (skip-chars-backward " \r\t\n")
	    (line-end-position)))))

(defun org-src--contents-for-write-back ()
  "Return buffer contents in a format appropriate for write back.
Assume point is in the corresponding edit buffer."
  (let ((indentation (+ (or org-src--block-indentation 0)
			(if (memq org-src--type '(example-block src-block))
			    org-edit-src-content-indentation
			  0)))
	(preserve-indentation org-src-preserve-indentation)
	(contents (org-with-wide-buffer (buffer-string)))
	(fixed-width-p (eq org-src--type 'fixed-width)))
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (if fixed-width-p
	  (progn
	    (untabify (point-min) (point-max))
	    (let ((prefix (concat (and (not preserve-indentation)
				       (make-string indentation ?\s))
				  ": ")))
	      (while (not (eobp)) (insert prefix) (forward-line))))
	(unless preserve-indentation (untabify (point-min) (point-max)))
	(org-escape-code-in-region (point-min) (point-max))
	(unless (or preserve-indentation (zerop indentation))
	  (let ((ind (make-string indentation ?\s)))
	    (while (not (eobp))
	      (when (org-looking-at-p "[ \t]*\\S-") (insert ind))
	      (forward-line)))))
      (buffer-string))))

(defun org-src--edit-element (element name &optional major write-back contents)
  "Edit ELEMENT contents in a dedicated buffer.

MAJOR is the major mode used in the edit buffer.  A nil value is
equivalent to `fundamental-mode'.  When WRITE-BACK is non-nil,
assume contents will replace original region.  When CONTENTS is
non-nil, display them in the edit buffer.  Otherwise, assume they
are located in property `:value'.

Leave point in edit buffer."
  (setq org-src--saved-temp-window-config (current-window-configuration))
  (let* ((area (org-src--element-contents-area element))
	 (beg (copy-marker (car area)))
	 (end (copy-marker (cdr area) t))
	 (old-edit-buffer (org-src--edit-buffer beg end)))
    (if (and old-edit-buffer
	     (or (not org-src-ask-before-returning-to-edit-buffer)
		 (y-or-n-p "Return to existing edit buffer ([n] will revert changes)? ")))
	;; Move to existing buffer.
	(org-src-switch-to-buffer old-edit-buffer 'return)
      ;; Discard old edit buffer.
      (when old-edit-buffer
	(with-current-buffer old-edit-buffer
	  (when (boundp 'org-src--overlay)
	    (delete-overlay org-src--overlay)))
	(kill-buffer old-edit-buffer))
      (let* ((org-mode-p (derived-mode-p 'org-mode))
	     (type (org-element-type element))
	     (ind (org-with-wide-buffer
		   (goto-char (org-element-property :begin element))
		   (org-get-indentation)))
	     (preserve-ind
	      (and (memq type '(example-block src-block))
		   (or (org-element-property :preserve-indent element)
		       org-src-preserve-indentation)))
	     ;; Store relative positions of mark (if any) and point
	     ;; within the edited area.
	     (point-coordinates (org-src--coordinates (point) beg end))
	     (mark-coordinates (and (org-region-active-p)
				    (let ((m (mark)))
				      (and (>= m beg) (>= end m)
					   (org-src--coordinates m beg end)))))
	     ;; Generate a new edit buffer.
	     (buffer (generate-new-buffer name))
	     ;; Add an overlay on top of source.
	     (overlay (org-src--make-source-overlay beg end buffer)))
	;; Switch to edit buffer.
	(org-src-switch-to-buffer buffer 'edit)
	;; Insert contents.
	(insert (or contents (org-element-property :value element)))
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(unless preserve-ind (org-do-remove-indentation))
	(set-buffer-modified-p nil)
	(setq buffer-file-name nil)
	;; Start major mode.
	(if (not major) (fundamental-mode)
	  (let ((org-inhibit-startup t))
	    (condition-case e (funcall major)
	      (error (message "Language mode `%s' fails with: %S"
			      major (nth 1 e))))))
	;; Transmit buffer-local variables for exit function.  It must
	;; be done after initializing major mode, as this operation
	;; may reset them otherwise.
	(org-set-local 'org-src--from-org-mode org-mode-p)
	(org-set-local 'org-src--beg-marker beg)
	(org-set-local 'org-src--end-marker end)
	(org-set-local 'org-src--type type)
	(org-set-local 'org-src--block-indentation ind)
	(org-set-local 'org-src-preserve-indentation preserve-ind)
	(org-set-local 'org-src--overlay overlay)
	(org-set-local 'org-src--allow-write-back-p write-back)
	;; Start minor mode.
	(org-src-mode)
	(when org-edit-src-persistent-message
	  (org-set-local
	   'header-line-format
	   (substitute-command-keys
	    (if write-back
		"Edit, then exit with \\[org-edit-src-exit] or abort with \
\\[org-edit-src-abort]"
	      "Exit with \\[org-edit-src-exit] or abort with \
\\[org-edit-src-abort]"))))
	;; Possibly start `auto-save-mode'.
	(when org-edit-src-turn-on-auto-save
	  (setq buffer-auto-save-file-name
		(concat (make-temp-name "org-src-")
			(format-time-string "-%Y-%d-%m")
			".txt")))
	;; Move mark and point in edit buffer to the corresponding
	;; location.
	(when mark-coordinates
	  (org-src--goto-coordinates mark-coordinates (point-min) (point-max))
	  (push-mark (point) 'no-message t)
	  (setq deactivate-mark nil))
	(org-src--goto-coordinates point-coordinates (point-min) (point-max)))
      ;; Install idle auto save feature, if necessary.
      (or org-src--code-timer
	  (zerop org-edit-src-auto-save-idle-delay)
	  (setq org-src--code-timer
		(run-with-idle-timer
		 org-edit-src-auto-save-idle-delay t
		 (lambda ()
		   (cond
		    ((org-src-edit-buffer-p)
		     (when (buffer-modified-p) (org-edit-src-save)))
		    ((not (org-some (lambda (b) (org-src-edit-buffer-p b))
				    (buffer-list)))
		     (cancel-timer org-src--code-timer)
		     (setq org-src--code-timer nil))))))))))



;;; Fontification of source blocks

(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
This function is called by emacs automatic fontification, as long
as `org-src-fontify-natively' is non-nil."
  (let ((lang-mode (org-src--get-lang-mode lang)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
	    (modified (buffer-modified-p))
	    (org-buffer (current-buffer)) pos next)
	(remove-text-properties start end '(face nil))
	(with-current-buffer
	    (get-buffer-create
	     (concat " org-src-fontification:" (symbol-name lang-mode)))
	  (delete-region (point-min) (point-max))
	  (insert string " ") ;; so there's a final property change
	  (unless (eq major-mode lang-mode) (funcall lang-mode))
	  (font-lock-fontify-buffer)
	  (setq pos (point-min))
	  (while (setq next (next-single-property-change pos 'face))
	    (put-text-property
	     (+ start (1- pos)) (1- (+ start next)) 'face
	     (get-text-property pos 'face) org-buffer)
	    (setq pos next)))
	(add-text-properties
	 start end
	 '(font-lock-fontified t fontified t font-lock-multiline t))
	(set-buffer-modified-p modified)))))



;;; Escape contents

(defun org-escape-code-in-region (beg end)
  "Escape lines between BEG and END.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^[ \t]*,?\\(\\*\\|#\\+\\)" end t)
      (replace-match ",\\1" nil nil nil 1))))

(defun org-escape-code-in-string (s)
  "Escape lines in string S.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (replace-regexp-in-string "^[ \t]*,?\\(\\*\\|#\\+\\)" ",\\1" s nil nil 1))

(defun org-unescape-code-in-region (beg end)
  "Un-escape lines between BEG and END.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^[ \t]*,?\\(,\\)\\(?:\\*\\|#\\+\\)" end t)
      (replace-match "" nil nil nil 1))))

(defun org-unescape-code-in-string (s)
  "Un-escape lines in string S.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (replace-regexp-in-string
   "^[ \t]*,?\\(,\\)\\(?:\\*\\|#\\+\\)" "" s nil nil 1))



;;; Org src minor mode

(defvar org-src-mode-map (make-sparse-keymap))
(define-key org-src-mode-map "\C-c'" 'org-edit-src-exit)
(define-key org-src-mode-map "\C-c\C-k" 'org-edit-src-abort)
(define-key org-src-mode-map "\C-x\C-s" 'org-edit-src-save)

(define-minor-mode org-src-mode
  "Minor mode for language major mode buffers generated by org.
This minor mode is turned on in two situations:
- when editing a source code snippet with \"C-c '\".
- When formatting a source code snippet for export with htmlize.
There is a mode hook, and keybindings for `org-edit-src-exit' and
`org-edit-src-save'")

(defun org-src-mode-configure-edit-buffer ()
  (when (org-bound-and-true-p org-src--from-org-mode)
    (org-add-hook 'kill-buffer-hook
		  (lambda () (delete-overlay org-src--overlay)) nil 'local)
    (if (org-bound-and-true-p org-src--allow-write-back-p)
	(progn
	  (setq buffer-offer-save t)
	  (setq buffer-file-name
		(concat (buffer-file-name (marker-buffer org-src--beg-marker))
			"[" (buffer-name) "]"))
	  (if (featurep 'xemacs)
	      (progn
		(make-variable-buffer-local 'write-contents-hooks) ; needed only for 21.4
		(setq write-contents-hooks '(org-edit-src-save)))
	    (setq write-contents-functions '(org-edit-src-save))))
      (setq buffer-read-only t))))

(org-add-hook 'org-src-mode-hook #'org-src-mode-configure-edit-buffer)



;;; Babel related functions

(defun org-src-associate-babel-session (info)
  "Associate edit buffer with comint session."
  (interactive)
  (let ((session (cdr (assoc :session (nth 2 info)))))
    (and session (not (string= session "none"))
	 (org-babel-comint-buffer-livep session)
	 (let ((f (intern (format "org-babel-%s-associate-session"
                                  (nth 0 info)))))
           (and (fboundp f) (funcall f session))))))

(defun org-src-babel-configure-edit-buffer ()
  (when org-src--babel-info
    (org-src-associate-babel-session org-src--babel-info)))

(org-add-hook 'org-src-mode-hook #'org-src-babel-configure-edit-buffer)

(defmacro org-src-do-at-code-block (&rest body)
  "Execute a command from an edit buffer in the Org mode buffer."
  `(let ((beg-marker org-src--beg-marker))
     (when beg-marker
       (with-current-buffer (marker-buffer beg-marker)
	 (goto-char beg-marker)
	 ,@body))))
(def-edebug-spec org-src-do-at-code-block (body))

(defun org-src-do-key-sequence-at-code-block (&optional key)
  "Execute key sequence at code block in the source Org buffer.
The command bound to KEY in the Org-babel key map is executed
remotely with point temporarily at the start of the code block in
the Org buffer.

This command is not bound to a key by default, to avoid conflicts
with language major mode bindings.  To bind it to C-c @ in all
language major modes, you could use

  (add-hook 'org-src-mode-hook
            (lambda () (define-key org-src-mode-map \"\\C-c@\"
                    'org-src-do-key-sequence-at-code-block)))

In that case, for example, C-c @ t issued in code edit buffers
would tangle the current Org code block, C-c @ e would execute
the block and C-c @ h would display the other available
Org-babel commands."
  (interactive "kOrg-babel key: ")
  (if (equal key (kbd "C-g")) (keyboard-quit)
    (org-edit-src-save)
    (org-src-do-at-code-block
     (call-interactively (lookup-key org-babel-map key)))))



;;; Public functions

(defun org-src-edit-buffer-p (&optional buffer)
  "Non-nil when current buffer is a source editing buffer.
If BUFFER is non-nil, test it instead."
  (let ((buffer (org-base-buffer (or buffer (current-buffer)))))
    (and (buffer-live-p buffer)
	 (local-variable-p 'org-src--beg-marker buffer)
	 (local-variable-p 'org-src--end-marker buffer))))

(defun org-src-switch-to-buffer (buffer context)
  (case org-src-window-setup
    (current-window (org-pop-to-buffer-same-window buffer))
    (other-window
     (switch-to-buffer-other-window buffer))
    (other-frame
     (case context
       (exit
	(let ((frame (selected-frame)))
	  (switch-to-buffer-other-frame buffer)
	  (delete-frame frame)))
       (save
	(kill-buffer (current-buffer))
	(org-pop-to-buffer-same-window buffer))
       (t (switch-to-buffer-other-frame buffer))))
    (reorganize-frame
     (when (eq context 'edit) (delete-other-windows))
     (org-switch-to-buffer-other-window buffer)
     (when (eq context 'exit) (delete-other-windows)))
    (switch-invisibly (set-buffer buffer))
    (t
     (message "Invalid value %s for `org-src-window-setup'"
	      org-src-window-setup)
     (org-pop-to-buffer-same-window buffer))))

(defun org-edit-table.el ()
  "Edit \"table.el\" table at point.

A new buffer is created and the table is copied into it.  Then
the table is recognized with `table-recognize'.  When done
editing, exit with \\[org-edit-src-exit].  The edited text will
then replace the area in the Org mode buffer.

Throw an error when not at such a table."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'table)
		 (eq (org-element-property :type element) 'table.el)
		 (org-src--on-element-p element))
      (user-error "Not in a table.el table"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "Table")
     #'text-mode t)
    (when (org-bound-and-true-p flyspell-mode) (flyspell-mode -1))
    (table-recognize)
    t))

(defun org-edit-export-block ()
  "Edit export block at point.

A new buffer is created and the block is copied into it, and the
buffer is switched into an appropriate major mode.  See also
`org-src-lang-modes'.  When done, exit with
\\[org-edit-src-exit].  The edited text will then replace the
area in the Org mode buffer.

Throw an error when not at an export block."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'export-block)
		 (org-src--on-element-p element))
      (user-error "Not in an export block"))
    (let* ((type (downcase (org-element-property :type element)))
	   (mode (org-src--get-lang-mode type)))
      (unless (functionp mode) (error "No such language mode: %s" mode))
      (org-src--edit-element
       element (org-src--construct-edit-buffer-name (buffer-name) type) mode t))
    t))

(defun org-edit-src-code (&optional code edit-buffer-name)
  "Edit the source or example block at point.

The code is copied to a separate buffer and the appropriate mode
is turned on.  When done, exit with \\[org-edit-src-exit].  This
will remove the original code in the Org buffer, and replace it
with the edited version.  See `org-src-window-setup' to configure
the display of windows containing the Org buffer and the code
buffer.

When optional argument CODE is a string, edit it in a dedicated
buffer instead.

When optional argument EDIT-BUFFER-NAME is non-nil, use it as the
name of the sub-editing buffer."
  (interactive)
  (let* ((element (org-element-at-point))
	 (type (org-element-type element)))
    (unless (and (memq type '(example-block src-block))
		 (org-src--on-element-p element))
      (user-error "Not in a source or example block"))
    (let* ((lang
	    (if (eq type 'src-block) (org-element-property :language element)
	      "example"))
	   (lang-f (and (eq type 'src-block) (org-src--get-lang-mode lang)))
	   (babel-info (and (eq type 'src-block)
			    (org-babel-get-src-block-info 'light))))
      (when (and (eq type 'src-block) (not (functionp lang-f)))
	(error "No such language mode: %s" lang-f))
      (org-src--edit-element
       element
       (or edit-buffer-name
	   (org-src--construct-edit-buffer-name (buffer-name) lang))
       lang-f (null code) (and code (org-unescape-code-in-string code)))
      ;; Finalize buffer.
      (org-set-local 'org-coderef-label-format
		     (or (org-element-property :label-fmt element)
			 org-coderef-label-format))
      (when (eq type 'src-block)
	(org-set-local 'org-src--babel-info babel-info)
	(let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
	  (when (fboundp edit-prep-func)
	    (funcall edit-prep-func babel-info))))
      t)))

(defun org-edit-fixed-width-region ()
  "Edit the fixed-width ASCII drawing at point.

This must be a region where each line starts with a colon
followed by a space or a newline character.

A new buffer is created and the fixed-width region is copied into
it, and the buffer is switched into the major mode defined in
`org-edit-fixed-width-region-mode', which see.  When done, exit
with \\[org-edit-src-exit].  The edited text will then replace
the area in the Org mode buffer."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'fixed-width)
		 (org-src--on-element-p element))
      (user-error "Not in a fixed-width area"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "Fixed Width")
     org-edit-fixed-width-region-mode
     t)
    ;; Return success.
    t))

(defun org-edit-src-abort ()
  "Abort editing of the src code and return to the Org buffer."
  (interactive)
  (let (org-src--allow-write-back-p) (org-edit-src-exit)))

(defun org-edit-src-continue (e)
  "Unconditionally return to buffer editing area under point.
Throw an error if there is no such buffer."
  (interactive "e")
  (mouse-set-point e)
  (let ((buf (get-char-property (point) 'edit-buffer)))
    (if buf (org-src-switch-to-buffer buf 'continue)
      (user-error "No sub-editing buffer for area at point"))))

(defun org-edit-src-save ()
  "Save parent buffer with current state source-code buffer."
  (interactive)
  (unless (org-src-edit-buffer-p) (user-error "Not in a sub-editing buffer"))
  (set-buffer-modified-p nil)
  (let ((edited-code (org-src--contents-for-write-back))
	(beg org-src--beg-marker)
	(end org-src--end-marker)
	(overlay org-src--overlay))
    (with-current-buffer (org-src--source-buffer)
      (undo-boundary)
      (delete-region beg end)
      (when (org-string-nw-p edited-code) (insert edited-code))
      (unless (bolp) (insert "\n"))
      (move-overlay overlay beg (point))
      (save-buffer))))

(defun org-edit-src-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  (unless (org-src-edit-buffer-p) (error "Not in a sub-editing buffer"))
  (let* ((allow-write-back-p org-src--allow-write-back-p)
	 (beg org-src--beg-marker)
	 (end org-src--end-marker)
	 (coordinates (org-src--coordinates (point) 1 (point-max)))
	 (code (and allow-write-back-p (org-src--contents-for-write-back))))
    (set-buffer-modified-p nil)
    ;; Switch to source buffer.  Kill sub-editing buffer.
    (let ((edit-buffer (current-buffer)))
      (org-src-switch-to-buffer (marker-buffer beg) 'exit)
      (kill-buffer edit-buffer))
    ;; Insert modified code.  Ensure it ends with a newline character.
    (when (and allow-write-back-p
	       (not (equal (buffer-substring-no-properties beg end) code)))
      (undo-boundary)
      (goto-char beg)
      (delete-region beg end)
      (when (org-string-nw-p code)
	(insert code)
	(unless (bolp) (insert "\n"))))
    ;; If we are to return to source buffer, put point at an
    ;; appropriate location.  In particular, if block is hidden, move
    ;; to the beginning of the block opening line.
    (goto-char beg)
    (cond
     ;; Block is hidden; move at start of block.
     ((org-some (lambda (o) (eq (overlay-get o 'invisible) 'org-hide-block))
		(overlays-at (point)))
      (beginning-of-line 0))
     (allow-write-back-p (org-src--goto-coordinates coordinates beg end)))
    ;; Clean up left-over markers and restore window configuration.
    (set-marker beg nil)
    (set-marker end nil)
    (when org-src--saved-temp-window-config
      (set-window-configuration org-src--saved-temp-window-config)
      (setq org-src--saved-temp-window-config nil))))


(provide 'org-src)

;;; org-src.el ends here
