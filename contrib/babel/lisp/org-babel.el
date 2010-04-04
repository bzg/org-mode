;;; org-babel.el --- facilitating communication between programming languages and people

;; Copyright (C) 2009 Eric Schulte, Dan Davison

;; Author: Eric Schulte, Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See org-babel.org in the parent directory for more information

;;; Code:
(require 'org)

(defun org-babel-execute-src-block-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-execute-src-block current-prefix-arg info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-execute-src-block-maybe)

(defadvice org-edit-special (around org-babel-prep-session-for-edit activate)
  "Prepare the current source block's session according to it's
header arguments before editing in an org-src buffer.  This
function is called when `org-edit-special' is called with a
prefix argument from inside of a source-code block."
  (when current-prefix-arg
    (let* ((info (org-babel-get-src-block-info))
           (lang (first info))
           (params (third info))
           (session (cdr (assoc :session params))))
      (when (and info session) ;; if we are in a source-code block which has a session
        (funcall (intern (concat "org-babel-prep-session:" lang)) session params))))
  ad-do-it)

(defadvice org-open-at-point (around org-babel-open-at-point activate)
  "If `point' is on a source code block, then open that block's
results with `org-babel-open-src-block-results', otherwise defer
to `org-open-at-point'."
  (interactive "P")
  (or (call-interactively #'org-babel-open-src-block-result) ad-do-it))

(defun org-babel-load-in-session-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-load-in-session'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-load-in-session current-prefix-arg info) t) nil)))

(add-hook 'org-metaup-hook 'org-babel-load-in-session-maybe)

(defun org-babel-pop-to-session-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-pop-to-session'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-pop-to-session current-prefix-arg info) t) nil)))

(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

(defconst org-babel-header-arg-names
  '(cache cmdline colnames dir exports file noweb results session tangle var)
  "Common header arguments used by org-babel.  Note that
individual languages may define their own language specific
header arguments as well.")

(defvar org-babel-default-header-args
  '((:session . "none") (:results . "replace") (:exports . "code") (:cache . "no") (:noweb . "no"))
  "Default arguments to use when evaluating a source block.")

(defvar org-babel-default-inline-header-args
  '((:session . "none") (:results . "silent") (:exports . "results"))
  "Default arguments to use when evaluating an inline source block.")

(defvar org-babel-src-block-regexp nil
  "Regexp used to test when inside of a org-babel src-block")

(defvar org-babel-inline-src-block-regexp nil
  "Regexp used to test when on an inline org-babel src-block")

(defvar org-babel-result-regexp
  "^[ \t]*#\\+res\\(ults\\|name\\)\\(\\[\\([[:alnum:]]+\\)\\]\\)?\\:"
  "Regular expression used to match result lines.  If the
results are associated with a hash key then the hash will be
saved in the second match data.")

(defvar org-babel-source-name-regexp
  "^[ \t]*#\\+\\(srcname\\|source\\|function\\):[ \t]*"
  "Regular expression used to match a source name line.")

(defvar org-babel-min-lines-for-block-output 10
  "If number of lines of output is equal to or exceeds this
value, the output is placed in a #+begin_example...#+end_example
block. Otherwise the output is marked as literal by inserting
colons at the starts of the lines. This variable only takes
effect if the :results output option is in effect.")

(defvar org-babel-noweb-error-langs nil
  "List of language for which errors should be raised when the
source code block satisfying a noweb reference in this language
can not be resolved.")

(defvar org-babel-hash-show 4
  "Number of initial characters to show of a hidden results hash.")

(defvar org-babel-after-execute-hook nil
  "Hook for functions to be called after `org-babel-execute-src-block'")
(defun org-babel-named-src-block-regexp-for-name (name)
  "Regexp used to match named src block."
  (concat org-babel-source-name-regexp (regexp-quote name) "[ \t\n]*"
	  (substring org-babel-src-block-regexp 1)))

(defun org-babel-set-interpreters (var value)
  (set-default var value)
  (setq org-babel-src-block-regexp
	(concat "^[ \t]*#\\+begin_src[ \t]+\\("       ;; (1)   lang
		(mapconcat 'regexp-quote value "\\|")
		"\\)[ \t]*"
		"\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)" ;; (2)   switches
		"\\([^\n]*\\)\n"                      ;; (3)   header arguments
                "\\([^\000]+?\n\\)[ \t]*#\\+end_src"));; (4)   body
  (setq org-babel-inline-src-block-regexp
	(concat "[ \f\t\n\r\v]\\(src_"                ;; (1)   replacement target
		"\\("                                 ;; (2)   lang
		(mapconcat 'regexp-quote value "\\|")
		"\\)"
                "\\(\\|\\[\\(.*\\)\\]\\)"             ;; (3,4) (unused, headers)
                "{\\([^\f\n\r\v]+\\)}"                ;; (5)   body
		"\\)")))

(defun org-babel-add-interpreter (interpreter)
  "Add INTERPRETER to `org-babel-interpreters' and update
`org-babel-src-block-regexp' appropriately."
  (unless (member interpreter org-babel-interpreters)
    (setq org-babel-interpreters
          (sort (cons interpreter org-babel-interpreters)
		(lambda (left right)
		  (> (length left) (length right)))))
    (org-babel-set-interpreters 'org-babel-interpreters org-babel-interpreters)))

(defcustom org-babel-interpreters '()
  "Interpreters allows for evaluation tags.
This is a list of program names (as strings) that can evaluate code and
insert the output into an Org-mode buffer.  Valid choices are

R          Evaluate R code
emacs-lisp Evaluate Emacs Lisp code and display the result
sh         Pass command to the shell and display the result
perl       The perl interpreter
python     The python interpreter
ruby       The ruby interpreter

The source block regexp `org-babel-src-block-regexp' is updated
when a new interpreter is added to this list through the
customize interface.  To add interpreters to this variable from
lisp code use the `org-babel-add-interpreter' function."
  :group 'org-babel
  :set 'org-babel-set-interpreters
  :type '(set :greedy t
              (const "R")
	      (const "emacs-lisp")
              (const "sh")
	      (const "perl")
	      (const "python")
	      (const "ruby")))

;;; functions
(defun org-babel-execute-src-block (&optional arg info params)
  "Execute the current source code block, and insert the results
into the buffer.  Source code execution and the collection and
formatting of results can be controlled through a variety of
header arguments.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive)
  ;; (message "supplied params=%S" params) ;; debugging
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (first info))
	 (params (setf (third info)
                       (sort (org-babel-merge-params (third info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
                                                   (symbol-name (car el2)))))))
         (new-hash (if (and (cdr (assoc :cache params))
                            (string= "yes" (cdr (assoc :cache params)))) (org-babel-sha1-hash info)))
         (old-hash (org-babel-result-hash info))
         (body (setf (second info)
		     (if (and (cdr (assoc :noweb params))
                              (string= "yes" (cdr (assoc :noweb params))))
			 (org-babel-expand-noweb-references info) (second info))))

         (result-params (split-string (or (cdr (assoc :results params)) "")))
         (result-type (cond ((member "output" result-params) 'output)
			    ((member "value" result-params) 'value)
			    (t 'value)))
         (cmd (intern (concat "org-babel-execute:" lang)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (call-process-region-original
	  (if (boundp 'call-process-region-original) call-process-region-original
	    (symbol-function 'call-process-region)))
         result)
    (unwind-protect
        (flet ((call-process-region (&rest args)
                                    (apply 'org-babel-tramp-handle-call-process-region args)))
          (unless (member lang org-babel-interpreters)
            (error "Language is not in `org-babel-interpreters': %s" lang))
          (if (and (not arg) new-hash (equal new-hash old-hash))
              (save-excursion ;; return cached result
                (goto-char (org-babel-where-is-src-block-result nil info))
                (move-end-of-line 1) (forward-char 1)
                (setq result (org-babel-read-result))
                (message (replace-regexp-in-string "%" "%%" (format "%S" result))) result)
            (setq result (funcall cmd body params))
            (if (eq result-type 'value)
                (setq result (if (and (or (member "vector" result-params)
                                          (member "table" result-params))
                                      (not (listp result)))
                                 (list (list result))
                               result)))
            (org-babel-insert-result result result-params info new-hash)
            (run-hooks 'org-babel-after-execute-hook)
            result))
      (setq call-process-region 'call-process-region-original))))

(defun org-babel-load-in-session (&optional arg info)
  "Load the body of the current source-code block.  Evaluate the
header arguments for the source block before entering the
session.  After loading the body this pops open the session."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (params (third info))
         (session (cdr (assoc :session params))))
    (unless (member lang org-babel-interpreters)
      (error "Language is not in `org-babel-interpreters': %s" lang))
    ;; if called with a prefix argument, then process header arguments
    (pop-to-buffer (funcall (intern (concat "org-babel-load-session:" lang)) session body params))
    (move-end-of-line 1)))

(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current source-code block.
If called with a prefix argument then evaluate the header arguments
for the source block before entering the session. Copy the body
of the source block to the kill ring."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (params (third info))
         (session (cdr (assoc :session params)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory)))
    (unless (member lang org-babel-interpreters)
      (error "Language is not in `org-babel-interpreters': %s" lang))
    ;; copy body to the kill ring
    (with-temp-buffer (insert (org-babel-trim body)) (copy-region-as-kill (point-min) (point-max)))
    ;; if called with a prefix argument, then process header arguments
    (if arg (funcall (intern (concat "org-babel-prep-session:" lang)) session params))
    ;; just to the session using pop-to-buffer
    (pop-to-buffer (funcall (intern (format "org-babel-%s-initiate-session" lang)) session params))
    (move-end-of-line 1)))

(defalias 'org-babel-pop-to-session 'org-babel-switch-to-session)

(defun org-babel-open-src-block-result (&optional re-run)
  "If `point' is on a src block then open the results of the
source code block, otherwise return nil.  With optional prefix
argument RE-RUN the source-code block is evaluated even if
results already exist."
  (interactive "P")
  (when (org-babel-get-src-block-info)
    (save-excursion
      ;; go to the results, if there aren't any then run the block
      (goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
                     (progn (org-babel-execute-src-block)
                            (org-babel-where-is-src-block-result))))
      (move-end-of-line 1) (forward-char 1)
      ;; open the results
      (if (looking-at org-bracket-link-regexp)
          ;; file results
          (org-open-at-point)
        (let ((results (org-babel-read-result)))
          (flet ((echo-res (result)
                           (if (stringp result) result (format "%S" result))))
            (pop-to-buffer (get-buffer-create "org-babel-results"))
            (delete-region (point-min) (point-max))
            (if (listp results)
                ;; table result
                (insert (orgtbl-to-generic results '(:sep "\t" :fmt echo-res)))
              ;; scalar result
              (insert (echo-res results))))))
      t)))

(defun org-babel-execute-buffer (&optional arg)
  "Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (let ((pos-end (match-end 0)))
	(goto-char (match-beginning 0))
	(org-babel-execute-src-block arg)
	(goto-char pos-end)))))

(defun org-babel-execute-subtree (&optional arg)
  "Call `org-babel-execute-src-block' on every source block in
the current subtree."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (org-babel-execute-buffer)
    (widen)))

(defun org-babel-get-src-block-info (&optional header-vars-only)
  "Get information of the current source block.
Returns a list
 (language body header-arguments-alist switches name function-args).
Unless HEADER-VARS-ONLY is non-nil, any variable
references provided in 'function call style' (i.e. in a
parenthesised argument list following the src block name) are
added to the header-arguments-alist."
  (let ((case-fold-search t) head info args)
    (if (setq head (org-babel-where-is-src-block-head))
        (save-excursion
	  (goto-char head)
	  (setq info (org-babel-parse-src-block-match))
	  (forward-line -1)
	  (when (looking-at (concat org-babel-source-name-regexp
                                    "\\([^ ()\f\t\n\r\v]+\\)\\(\(\\(.*\\)\)\\|\\)"))
	    (setq info (append info (list (org-babel-clean-text-properties (match-string 2)))))
	    ;; Note that e.g. "name()" and "name( )" result in ((:var . "")).
	    ;; We maintain that behaviour, and the resulting non-nil sixth
	    ;; element is relied upon in org-babel-exp-code to detect a functional-style
	    ;; block in those cases. However, "name" without any
	    ;; parentheses would result in the same thing, so we
	    ;; explicitly avoid that.
	    (if (setq args (match-string 4))
		(setq info (append info (list (mapcar (lambda (ref) (cons :var ref))
						      (org-babel-ref-split-args args))))))
	    (unless header-vars-only
	      (setf (third info)
		    (org-babel-merge-params (sixth info) (third info)))))
	  info)
      (if (save-excursion ;; inline source block
            (re-search-backward "[ \f\t\n\r\v]" nil t)
            (looking-at org-babel-inline-src-block-regexp))
          (org-babel-parse-inline-src-block-match)
        nil)))) ;; indicate that no source block was found

(defun org-babel-sha1-hash (&optional info)
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (hash (sha1 (format "%s-%s" (mapconcat (lambda (arg) (format "%S" arg))
                                                (third info) ":")
                             (second info)))))
    (when (interactive-p) (message hash))
    hash))

(defun org-babel-result-hash (&optional info)
  (org-babel-where-is-src-block-result nil info)
  (org-babel-clean-text-properties (match-string 3)))

(defun org-babel-hide-hash ()
  "Hide the hash in the current results line.  Only the initial
`org-babel-hash-show' characters of the hash will remain
visible."
  (org-add-to-invisibility-spec '(org-babel-hide-hash . t))
  (save-excursion
    (when (and (re-search-forward org-babel-result-regexp nil t)
               (match-string 3))
      (let* ((start (match-beginning 3))
             (hide-start (+ org-babel-hash-show start))
             (end (match-end 3))
             (hash (match-string 3))
             ov1 ov2)
        (setq ov1 (org-make-overlay start hide-start))
        (setq ov2 (org-make-overlay hide-start end))
        (org-overlay-put ov2 'invisible 'org-babel-hide-hash)
        (org-overlay-put ov1 'babel-hash hash)))))

(defun org-babel-hide-all-hashes ()
  "Hide the hash in the current buffer.  Only the initial
`org-babel-hash-show' characters of each hash will remain
visible.  This function should be called as part of the
`org-mode-hook'."
  (save-excursion
    (while (re-search-forward org-babel-result-regexp nil t)
      (goto-char (match-beginning 0))
      (org-babel-hide-hash)
      (goto-char (match-end 0)))))
(add-hook 'org-mode-hook 'org-babel-hide-all-hashes)

(defun org-babel-hash-at-point (&optional point)
  "Return the value of the hash at `point'.  The hash is also
added as the last element of the kill ring.  This can be called
with C-c C-c."
  (interactive)
  (let ((hash (car (delq nil (mapcar
                               (lambda (ol) (org-overlay-get ol 'babel-hash))
                              (org-overlays-at (or point (point))))))))
    (when hash (kill-new hash) (message hash))))
(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-hash-at-point)

(defun org-babel-result-hide-spec ()
  (org-add-to-invisibility-spec '(org-babel-hide-result . t)))
(add-hook 'org-mode-hook 'org-babel-result-hide-spec)

(defvar org-babel-hide-result-overlays nil
  "Overlays hiding results.")

(defun org-babel-result-hide-all ()
  "Fold all results in the current buffer."
  (interactive)
  (org-babel-show-result-all)
  (save-excursion
    (while (re-search-forward org-babel-result-regexp nil t)
      (save-excursion (goto-char (match-beginning 0))
                      (org-babel-hide-result-toggle-maybe)))))

(defun org-babel-show-result-all ()
  "Unfold all results in the current buffer."
  (mapc 'org-delete-overlay org-babel-hide-result-overlays)
  (setq org-babel-hide-result-overlays nil))

(defun org-babel-hide-result-toggle-maybe ()
  "Toggle visibility of result at point."
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at org-babel-result-regexp))
        (progn (org-babel-hide-result-toggle)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(defun org-babel-hide-result-toggle (&optional force)
  "Toggle the visibility of the current result."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-babel-result-regexp nil t)
        (let ((start (progn (beginning-of-line 2) (- (point) 1)))
              (end (progn (goto-char (- (org-babel-result-end) 1)) (point)))
              ov)
          (if (memq t (mapcar (lambda (overlay)
                                (eq (org-overlay-get overlay 'invisible)
				    'org-babel-hide-result))
                              (org-overlays-at start)))
              (if (or (not force) (eq force 'off))
                  (mapc (lambda (ov)
                          (when (member ov org-babel-hide-result-overlays)
                            (setq org-babel-hide-result-overlays
                                  (delq ov org-babel-hide-result-overlays)))
                          (when (eq (org-overlay-get ov 'invisible)
                                    'org-babel-hide-result)
                            (org-delete-overlay ov)))
                        (org-overlays-at start)))
            (setq ov (org-make-overlay start end))
            (org-overlay-put ov 'invisible 'org-babel-hide-result)
            ;; make the block accessible to isearch
            (org-overlay-put
             ov 'isearch-open-invisible
             (lambda (ov)
               (when (member ov org-babel-hide-result-overlays)
                 (setq org-babel-hide-result-overlays
                       (delq ov org-babel-hide-result-overlays)))
               (when (eq (org-overlay-get ov 'invisible)
                         'org-babel-hide-result)
                 (org-delete-overlay ov))))
            (push ov org-babel-hide-result-overlays)))
      (error "Not looking at a result line"))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-tab-first-hook 'org-babel-hide-result-toggle-maybe)
;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (org-add-hook 'change-major-mode-hook
				   'org-babel-show-result-all 'append 'local)))

(defmacro org-babel-map-source-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE."
  (declare (indent 1))
  `(let ((visited-p (get-buffer (file-name-nondirectory ,file))))
     (save-window-excursion
       (find-file ,file) (goto-char (point-min))
       (while (re-search-forward org-babel-src-block-regexp nil t)
         (goto-char (match-beginning 0))
         (save-match-data ,@body)
         (goto-char (match-end 0))))
     (unless visited-p (kill-buffer (file-name-nondirectory file)))))

(defun org-babel-params-from-properties ()
  "Return an association list of any source block params which
may be specified in the properties of the current outline entry."
  (save-match-data
    (delq nil
          (mapcar
           (lambda (header-arg)
             (let ((val (or (condition-case nil
                                (org-entry-get (point) header-arg t)
                              (error nil))
                            (cdr (assoc header-arg org-file-properties)))))
               (when val
                 ;; (message "prop %s=%s" header-arg val) ;; debugging
                 (cons (intern (concat ":" header-arg)) val))))
           (mapcar 'symbol-name org-babel-header-arg-names)))))

(defun org-babel-parse-src-block-match ()
  (let* ((lang (org-babel-clean-text-properties (match-string 1)))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang)))
	 (switches (match-string 2))
         (body (org-babel-clean-text-properties (match-string 4)))
	 (preserve-indentation (or org-src-preserve-indentation
				   (string-match "-i\\>" switches))))
    (list lang
          ;; get src block body removing properties, protective commas, and indentation
          (with-temp-buffer
            (save-match-data
              (insert (org-babel-strip-protective-commas body))
	      (unless preserve-indentation (org-do-remove-indentation))
              (buffer-string)))
	  (org-babel-merge-params
	   org-babel-default-header-args
           (org-babel-params-from-properties)
	   (if (boundp lang-headers) (eval lang-headers) nil)
	   (org-babel-parse-header-arguments (org-babel-clean-text-properties (or (match-string 3) ""))))
	  switches)))

(defun org-babel-parse-inline-src-block-match ()
  (let* ((lang (org-babel-clean-text-properties (match-string 2)))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang))))
    (list lang
          (org-babel-strip-protective-commas (org-babel-clean-text-properties (match-string 5)))
          (org-babel-merge-params
           org-babel-default-inline-header-args
           (org-babel-params-from-properties)
           (if (boundp lang-headers) (eval lang-headers) nil)
           (org-babel-parse-header-arguments (org-babel-clean-text-properties (or (match-string 4) "")))))))

(defun org-babel-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (if (> (length arg-string) 0)
      (delq nil
	    (mapcar
	     (lambda (arg)
	       (if (string-match "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)" arg)
		   (cons (intern (concat ":" (match-string 1 arg)))
                         (let ((raw (org-babel-chomp (match-string 2 arg))))
                           (if (org-babel-number-p raw) raw (eval (org-babel-read raw)))))
		 (cons (intern (concat ":" arg)) nil)))
	     (split-string (concat " " arg-string) "[ \f\t\n\r\v]+:" t)))))

(defun org-babel-process-params (params)
  "Parse params and resolve references.

Return a list (session vars result-params result-type)."
  (let* ((session (cdr (assoc :session params)))
	 (vars (org-babel-ref-variables params))
	 (result-params (split-string (or (cdr (assoc :results params)) "")))
	 (result-type (cond ((member "output" result-params) 'output)
			    ((member "value" result-params) 'value)
			    (t 'value))))
    (list session vars result-params result-type)))

(defun org-babel-where-is-src-block-head ()
  "Return the point at the beginning of the current source
block.  Specifically at the beginning of the #+BEGIN_SRC line.
If the point is not on a source block then return nil."
  (let ((initial (point)) top bottom)
    (or
     (save-excursion ;; on a source name line
       (beginning-of-line 1)
       (and (looking-at org-babel-source-name-regexp) (forward-line 1)
            (looking-at org-babel-src-block-regexp)
            (point)))
     (save-excursion ;; on a #+begin_src line
       (beginning-of-line 1)
       (and (looking-at org-babel-src-block-regexp)
            (point)))
     (save-excursion ;; inside a src block
       (and
        (re-search-backward "^[ \t]*#\\+begin_src" nil t) (setq top (point))
        (re-search-forward "^[ \t]*#\\+end_src" nil t) (setq bottom (point))
        (< top initial) (< initial bottom)
        (goto-char top) (move-beginning-of-line 1)
        (looking-at org-babel-src-block-regexp)
        (point))))))

(defun org-babel-goto-named-source-block (&optional name)
  "Go to a named source-code block."
  (interactive "ssource-block name: ")
  (let ((point (org-babel-find-named-block name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-show-context))
      (message "source-code block '%s' not found in this buffer" name))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by source
NAME, or nil if no such block exists. Set match data according to
org-babel-named-src-block-regexp."
  (save-excursion
    (let ((case-fold-search t)
	  (regexp (org-babel-named-src-block-regexp-for-name name)) msg)
      (goto-char (point-min))
      (when (or (re-search-forward regexp nil t)
                (re-search-backward regexp nil t))
        (match-beginning 0)))))

(defun org-babel-find-named-result (name)
  "Return the location of the result named NAME in the current
buffer or nil if no such result exists."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat org-babel-result-regexp "[ \t]" (regexp-quote name) "[ \t\n\f\v\r]") nil t)
      (move-beginning-of-line 0) (point))))

(defun org-babel-where-is-src-block-result (&optional insert info hash)
  "Return the point at the beginning of the result of the current
source block.  Specifically at the beginning of the results line.
If no result exists for this block then create a results line
following the source block."
  (save-excursion
    (let* ((on-lob-line (progn (beginning-of-line 1)
			       (looking-at org-babel-lob-one-liner-regexp)))
	   (name (if on-lob-line (first (org-babel-lob-get-info))
		   (fifth (or info (org-babel-get-src-block-info)))))
	   (head (unless on-lob-line (org-babel-where-is-src-block-head))) end)
      (when head (goto-char head))
      (or (and name (org-babel-find-named-result name))
          (and (or on-lob-line (re-search-forward "^[ \t]*#\\+end_src" nil t))
               (progn (move-end-of-line 1)
		      (if (eobp) (insert "\n") (forward-char 1))
		      (setq end (point))
                      (or (and (not name)
			       (progn ;; unnamed results line already exists
				 (re-search-forward "[^ \f\t\n\r\v]" nil t)
				 (move-beginning-of-line 1)
                                 (looking-at (concat org-babel-result-regexp "\n"))))
			  ;; or (with optional insert) back up and make one ourselves
                          (when insert
                            (goto-char end)
			    (if (looking-at "[\n\r]") (forward-char 1) (insert "\n"))
                            (insert (concat "#+results" (if hash (concat "["hash"]"))
                                            ":"(if name (concat " " name)) "\n"))
                            (move-beginning-of-line 0)
                            (if hash (org-babel-hide-hash)) t)))
               (point))))))

(defun org-babel-read-result ()
  "Read the result at `point' into emacs-lisp."
  (let ((case-fold-search t) result-string)
    (cond
     ((org-at-table-p) (org-babel-read-table))
     ((looking-at org-bracket-link-regexp) (org-babel-read-link))
     ((looking-at org-block-regexp) (org-babel-trim (match-string 4)))
     ((looking-at ": ")
      (setq result-string
	    (org-babel-trim
	     (mapconcat (lambda (line) (if (and (> (length line) 1)
						(string= ": " (substring line 0 2)))
					   (substring line 2)
					 line))
			(split-string
			 (buffer-substring (point) (org-babel-result-end)) "[\r\n]+")
			"\n")))
      (or (org-babel-number-p result-string) result-string))
     ((looking-at org-babel-result-regexp)
      (save-excursion (forward-line 1) (org-babel-read-result))))))

(defun org-babel-read-table ()
  "Read the table at `point' into emacs-lisp."
  (mapcar (lambda (row)
            (if (and (symbolp row) (equal row 'hline)) row
              (mapcar #'org-babel-read row)))
          (org-table-to-lisp)))

(defun org-babel-read-link ()
  "Read the link at `point' into emacs-lisp.  If the path of the
link is a file path it is expanded using `expand-file-name'."
  (let* ((case-fold-search t)
         (raw (and (looking-at org-bracket-link-regexp)
                   (org-babel-clean-text-properties (match-string 1))))
         (type (and (string-match org-link-types-re raw)
                    (match-string 1 raw))))
    (cond
     ((not type) (expand-file-name raw))
     ((string= type "file")
      (and (string-match "file\\(.*\\):\\(.+\\)" raw)
           (expand-file-name (match-string 2 raw))))
     (t raw))))

(defun org-babel-insert-result (result &optional result-params info hash)
  "Insert RESULT into the current buffer after the end of the
current source block.  With optional argument RESULT-PARAMS
controls insertion of results in the org-mode file.
RESULT-PARAMS can take the following values...

replace - (default option) insert results after the source block
          replacing any previously inserted results

silent -- no results are inserted

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org-mode file syntax

raw ----- results are added directly to the org-mode file.  This
          is a good option if you code block will output org-mode
          formatted text.

org ----- this is the same as the 'raw' option

html ---- results are added inside of a #+BEGIN_HTML block.  This
          is a good option if you code block will output html
          formatted text.

latex --- results are added inside of a #+BEGIN_LATEX block.
          This is a good option if you code block will output
          latex formatted text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a #+BEGIN_SRC block with the source-code
          language set appropriately."
  (if (stringp result)
      (progn
        (setq result (org-babel-clean-text-properties result))
        (when (member "file" result-params)
          (setq result (org-babel-result-to-file result))))
    (unless (listp result) (setq result (format "%S" result))))
  (if (and result-params (member "replace" result-params)
           (not (member "silent" result-params)))
      (org-babel-remove-result info))
  (if (= (length result) 0)
      (if (member "value" result-params)
	  (message "No result returned by source block")
	(message "Source block produced no output"))
    (if (and result-params (member "silent" result-params))
        (progn (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
               result)
      (when (and (stringp result) ;; ensure results end in a newline
                 (not (or (string-equal (substring result -1) "\n")
                          (string-equal (substring result -1) "\r"))))
        (setq result (concat result "\n")))
      (save-excursion
	(let ((existing-result (org-babel-where-is-src-block-result t info hash))
	      (results-switches (cdr (assoc :results_switches (third info)))))
	  (when existing-result (goto-char existing-result) (forward-line 1))
	  (setq results-switches
                (if results-switches (concat " " results-switches) ""))
	  (cond
	   ;; assume the result is a table if it's not a string
	   ((not (stringp result))
	    (insert (concat (orgtbl-to-orgtbl
			     (if (and (listp (car result))
                                      (listp (cdr (car result))))
				 result (list result))
			     '(:fmt (lambda (cell) (format "%s" cell)))) "\n"))
	    (forward-line -1) (org-cycle))
	   ((member "file" result-params)
	    (insert result))
	   ((member "html" result-params)
	    (insert (format "#+BEGIN_HTML%s\n%s#+END_HTML\n" results-switches result)))
	   ((member "latex" result-params)
	    (insert (format "#+BEGIN_LaTeX%s\n%s#+END_LaTeX\n" results-switches result)))
	   ((member "code" result-params)
	    (insert (format "#+BEGIN_SRC %s%s\n%s#+END_SRC\n" lang results-switches result)))
	   ((or (member "raw" result-params) (member "org" result-params))
	    (save-excursion (insert result)) (if (org-at-table-p) (org-cycle)))
	   (t
	    (org-babel-examplize-region
             (point) (progn (insert result) (point)) results-switches)))))
      (message "finished"))))

(defun org-babel-result-to-org-string (result)
  "Return RESULT as a string in org-mode format.  This function
relies on `org-babel-insert-result'."
  (with-temp-buffer (org-babel-insert-result result) (buffer-string)))

(defun org-babel-remove-result (&optional info)
  "Remove the result of the current source block."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result nil info)) start)
    (when location
      (save-excursion
        (goto-char location) (setq start (point)) (forward-line 1)
        (delete-region start (org-babel-result-end))))))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results"
  (save-excursion
    (if (org-at-table-p)
        (progn (goto-char (org-table-end)) (point))
      (let ((case-fold-search t))
        (cond
         ((looking-at "#\\+begin_latex")
          (search-forward "#+end_latex" nil t)
          (forward-line 1))
         ((looking-at "#\\+begin_html")
          (search-forward "#+end_html" nil t)
          (forward-line 1))
         ((looking-at "#\\+begin_example")
          (search-forward "#+end_example" nil t)
          (forward-line 1))
         ((looking-at "#\\+begin_src")
          (search-forward "#+end_src" nil t)
          (forward-line 1))
         (t (progn (while (looking-at "\\(: \\|\\[\\[\\)")
                     (forward-line 1))))))
      (point))))

(defun org-babel-result-to-file (result)
  "Convert RESULT into an `org-mode' link.  If the
`default-directory' is different from the containing file's
directory then expand relative links."
  (format
   "[[file:%s]]"
   (if (and default-directory
            buffer-file-name
            (not (string= (expand-file-name default-directory)
                          (expand-file-name (file-name-directory buffer-file-name)))))
       (expand-file-name result default-directory)
     result)))

(defun org-babel-examplize-region (beg end &optional results-switches)
  "Comment out region using the ': ' org example quote."
  (interactive "*r")
  (let ((size (abs (- (line-number-at-pos end)
		      (line-number-at-pos beg)))))
    (save-excursion
      (cond ((= size 0)
	     (error "This should be impossible: a newline was appended to result if missing"))
	    ((< size org-babel-min-lines-for-block-output)
	     (goto-char beg)
	     (dotimes (n size)
	       (move-beginning-of-line 1) (insert ": ") (forward-line 1)))
	    (t
	     (goto-char beg)
	     (insert (if results-switches
                         (format "#+begin_example%s\n" results-switches)
                       "#+begin_example\n"))
	     (forward-char (- end beg))
	     (insert "#+end_example\n"))))))

(defun org-babel-merge-params (&rest plists)
  "Combine all parameter association lists in PLISTS.  Later
elements of PLISTS override the values of previous element.  This
takes into account some special considerations for certain
parameters when merging lists."
  (let ((results-exclusive-groups
	 '(("file" "vector" "table" "scalar" "raw" "org" "html" "latex" "code" "pp")
	   ("replace" "silent")
	   ("output" "value")))
	(exports-exclusive-groups
	 '(("code" "results" "both" "none")))
	params results exports tangle noweb cache vars var ref shebang comments)
    (flet ((e-merge (exclusive-groups &rest result-params)
                    ;; maintain exclusivity of mutually exclusive parameters
                    (let (output)
                      (mapc (lambda (new-params)
                              (mapc (lambda (new-param)
                                      (mapc (lambda (exclusive-group)
                                              (when (member new-param exclusive-group)
                                                (mapcar (lambda (excluded-param)
                                                          (setq output (delete excluded-param output)))
                                                        exclusive-group)))
                                            exclusive-groups)
                                      (setq output (org-uniquify (cons new-param output))))
                                    new-params))
                            result-params)
                      output)))
      (mapc (lambda (plist)
              (mapc (lambda (pair)
                      (case (car pair)
                        (:var
                         ;; we want only one specification per variable
                         (when (string-match "^\\([^= \f\t\n\r\v]+\\)[ \t]*=[ \t]*\\([^\f\n\r\v]+\\)$" (cdr pair))
                           ;; TODO: When is this not true?
                           (setq var (intern (match-string 1 (cdr pair)))
                                 ref (match-string 2 (cdr pair))
                                 vars (cons (cons var ref) (assq-delete-all var vars)))))
                        (:results
                         (setq results
			       (e-merge results-exclusive-groups results (split-string (cdr pair)))))
			(:file
			 (when (cdr pair)
			   (setq results (e-merge results-exclusive-groups results '("file")))
			   (unless (or (member "both" exports)
                                       (member "none" exports)
                                       (member "code" exports))
			     (setq exports (e-merge exports-exclusive-groups exports '("results"))))
			   (setq params (cons pair (assq-delete-all (car pair) params)))))
                        (:exports
                         (setq exports (e-merge exports-exclusive-groups
                                                exports (split-string (cdr pair)))))
                        (:tangle ;; take the latest -- always overwrite
                         (setq tangle (or (list (cdr pair)) tangle)))
                        (:noweb
                         (setq noweb (e-merge '(("yes" "no"))
                                              noweb (split-string (or (cdr pair) "")))))
                        (:cache
                         (setq cache (e-merge '(("yes" "no"))
                                              cache (split-string (or (cdr pair) "")))))
                        (:shebang ;; take the latest -- always overwrite
                         (setq shebang (or (list (cdr pair)) shebang)))
                        (:comments
                         (setq comments (e-merge '(("yes" "no"))
                                                 comments (split-string (or (cdr pair) "")))))
                        (t ;; replace: this covers e.g. :session
                         (setq params (cons pair (assq-delete-all (car pair) params))))))
                    plist))
            plists))
    (setq vars (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) vars))
    (while vars (setq params (cons (cons :var (pop vars)) params)))
    (cons (cons :comments (mapconcat 'identity comments " "))
          (cons (cons :shebang (mapconcat 'identity shebang " "))
                (cons (cons :cache (mapconcat 'identity cache " "))
                      (cons (cons :noweb (mapconcat 'identity noweb " "))
                            (cons (cons :tangle (mapconcat 'identity tangle " "))
                                  (cons (cons :exports (mapconcat 'identity exports " "))
                                        (cons (cons :results (mapconcat 'identity results " "))
                                              params)))))))))

(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "This function expands Noweb style references in the body of
the current source-code block.  For example the following
reference would be replaced with the body of the source-code
block named 'example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named 'example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
         (info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (new-body "") index source-name evaluate prefix)
    (flet ((nb-add (text)
                   (setq new-body (concat new-body text))))
      (with-temp-buffer
        (insert body) (goto-char (point-min))
        (setq index (point))
        (while (and (re-search-forward "<<\\(.+?\\)>>" nil t))
          (save-match-data (setf source-name (match-string 1)))
          (save-match-data (setq evaluate (string-match "\(.*\)" source-name)))
          (save-match-data
            (setq prefix (buffer-substring (match-beginning 0)
                                           (save-excursion
                                             (move-beginning-of-line 1) (point)))))
          ;; add interval to new-body (removing noweb reference)
          (goto-char (match-beginning 0))
          (nb-add (buffer-substring index (point)))
          (goto-char (match-end 0))
          (setq index (point))
          (nb-add (save-excursion
                    (set-buffer parent-buffer)
                    (mapconcat ;; interpose `prefix' between every line
                     #'identity
                     (split-string
                      (if evaluate
                          (let ((raw (org-babel-ref-resolve-reference
                                      source-name nil)))
                            (if (stringp raw) raw (format "%S" raw)))
                        (let ((point (org-babel-find-named-block source-name)))
                          (if point
                              (save-excursion
                                (goto-char point)
                                (org-babel-trim (org-babel-expand-noweb-references
                                                 (org-babel-get-src-block-info))))
                            ;; optionally raise an error if named
                            ;; source-block doesn't exist
                            (if (member lang org-babel-noweb-error-langs)
                                (error
                                 "<<%s>> could not be resolved (see `org-babel-noweb-error-langs')"
                                 source-name)
                              "")))) "[\n\r]") (concat "\n" prefix)))))
        (nb-add (buffer-substring index (point-max)))))
    new-body))

(defun org-babel-error-notify (exit-code stderr)
  (message (format "Shell command exited with code %d" exit-code))
  (let ((buf (get-buffer-create "*Org-Babel Error Output*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (save-excursion (insert stderr)))
    (display-buffer buf)))

(defun org-babel-clean-text-properties (text)
  "Strip all properties from text return."
  (set-text-properties 0 (length text) nil text) text)

(defun org-babel-strip-protective-commas (body)
  "Strip protective commas from bodies of source blocks."
  (replace-regexp-in-string "^,#" "#" body))

(defun org-babel-read (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if cell looks like lisp (meaning it starts with a
\"(\" or a \"'\") then read it as lisp, otherwise return it
unmodified as a string.

This is taken almost directly from `org-read-prop'."
  (if (and (stringp cell) (not (equal cell "")))
      (or (org-babel-number-p cell)
          (if (or (equal "(" (substring cell 0 1))
                  (equal "'" (substring cell 0 1)))
              (eval (read cell))
            (progn (set-text-properties 0 (length cell) nil cell) cell)))
    cell))

(defun org-babel-number-p (string)
  "Return t if STRING represents a number"
  (if (and (string-match "^-?[[:digit:]]*\\.?[[:digit:]]*$" string)
           (= (match-end 0) (length string)))
      (string-to-number string)))

(defun org-babel-import-elisp-from-file (file-name)
  "Read the results located at FILE-NAME into an elisp table.  If
the table is trivial, then return it as a scalar."
  (let (result)
    (save-window-excursion
      (with-temp-buffer
	(condition-case nil
	    (progn
	      (org-table-import file-name nil)
	      (delete-file file-name)
	      (setq result (mapcar (lambda (row)
				     (mapcar #'org-babel-string-read row))
				   (org-table-to-lisp))))
	  (error nil)))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
	  (if (consp (car result))
	      (if (null (cdr (car result)))
		  (caar result)
		result)
	    (car result))
	result))))

(defun org-babel-string-read (cell)
  "Strip nested \"s from around strings."
  (org-babel-read (or (and (stringp cell)
                           (string-match "\\\"\\(.+\\)\\\"" cell)
                           (match-string 1 cell))
                      cell)))

(defun org-babel-reverse-string (string)
  (apply 'string (reverse (string-to-list string))))

(defun org-babel-chomp (string &optional regexp)
  "Remove any trailing space or carriage returns characters from
STRING.  Default regexp used is \"[ \f\t\n\r\v]\" but can be
overwritten by specifying a regexp as a second argument."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (while (and (> (length string) 0) (string-match regexp (substring string -1)))
      (setq string (substring string 0 -1)))
    string))

(defun org-babel-trim (string &optional regexp)
  "Like `org-babel-chomp' only it runs on both the front and back of the string"
  (org-babel-chomp (org-babel-reverse-string
                    (org-babel-chomp (org-babel-reverse-string string) regexp)) regexp))

(defun org-babel-tramp-handle-call-process-region
  (start end program &optional delete buffer display &rest args)
  "Use tramp to handle call-process-region.
Fixes a bug in `tramp-handle-call-process-region'."
  (if (and (featurep 'tramp) (file-remote-p default-directory))
      (let ((tmpfile (tramp-compat-make-temp-file "")))
	(write-region start end tmpfile)
	(when delete (delete-region start end))
	(unwind-protect
	    ;;	(apply 'call-process program tmpfile buffer display args) ;; bug in tramp
	    (apply 'process-file program tmpfile buffer display args)
	  (delete-file tmpfile)))
    ;; call-process-region-original is the original emacs definition. It
    ;; is in scope from the let binding in org-babel-execute-src-block
    (apply call-process-region-original start end program delete buffer display args)))

(defun org-babel-maybe-remote-file (file)
  (if (file-remote-p default-directory)
      (let* ((vec (tramp-dissect-file-name default-directory))
             (user (tramp-file-name-user vec))
             (host (tramp-file-name-host vec)))
        (concat "/" user (when user "@") host ":" file))
    file))

(defun org-babel-shell-command-on-region (start end command
				      &optional output-buffer replace
				      error-buffer display-error-buffer)
  "Execute string COMMAND in inferior shell with region as input.

Fixes bugs in the emacs 23.1.1 version of `shell-command-on-region'

Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.  Return the exit code of
COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded in the same coding system that will be used to save the file,
`buffer-file-coding-system'.  If the output is going to replace the region,
then it is decoded from that same coding system.

The noninteractive arguments are START, END, COMMAND,
OUTPUT-BUFFER, REPLACE, ERROR-BUFFER, and DISPLAY-ERROR-BUFFER.
Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.  Otherwise
it is displayed in the buffer `*Shell Command Output*'.  The output
is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional fourth argument OUTPUT-BUFFER is non-nil,
that says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in the current buffer.
In either case, the output is inserted after point (leaving mark after it).

If REPLACE, the optional fifth argument, is non-nil, that means insert
the output in place of text from START to END, putting point and mark
around it.

If optional sixth argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
If DISPLAY-ERROR-BUFFER is non-nil, display the error buffer if there
were any errors.  (This is always t, interactively.)
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."
  (interactive (let (string)
		 (unless (mark)
		   (error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg
		       shell-command-default-error-buffer
		       t)))
  (let ((error-file
	 (if error-buffer
	     (make-temp-file
	      (expand-file-name "scor"
				(or small-temporary-file-directory
				    temporary-file-directory)))
	   nil))
	exit-status)
    (if (or replace
	    (and output-buffer
		 (not (or (bufferp output-buffer) (stringp output-buffer)))))
	;; Replace specified region with output from command.
	(let ((swap (and replace (< start end))))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (goto-char start)
	  (and replace (push-mark (point) 'nomsg))
	  (setq exit-status
		(call-process-region start end shell-file-name t
				     (if error-file
					 (list output-buffer error-file)
				       t)
				     nil shell-command-switch command))
	  ;; It is rude to delete a buffer which the command is not using.
	  ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
	  ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
	  ;; 	 (kill-buffer shell-buffer)))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (and replace swap (exchange-point-and-mark)))
      ;; No prefix argument: put the output in a temp buffer,
      ;; replacing its entire contents.
      (let ((buffer (get-buffer-create
		     (or output-buffer "*Shell Command Output*"))))
	(unwind-protect
	    (if (eq buffer (current-buffer))
		;; If the input is the same buffer as the output,
		;; delete everything but the specified region,
		;; then replace that region with the output.
		(progn (setq buffer-read-only nil)
		       (delete-region (max start end) (point-max))
		       (delete-region (point-min) (min start end))
		       (setq exit-status
			     (call-process-region (point-min) (point-max)
						  shell-file-name t
						  (if error-file
						      (list t error-file)
						    t)
						  nil shell-command-switch
						  command)))
	      ;; Clear the output buffer, then run the command with
	      ;; output there.
	      (let ((directory default-directory))
		(save-excursion
		  (set-buffer buffer)
		  (setq buffer-read-only nil)
		  (if (not output-buffer)
		      (setq default-directory directory))
		  (erase-buffer)))
	      (setq exit-status
		    (call-process-region start end shell-file-name nil
					 (if error-file
					     (list buffer error-file)
					   buffer)
					 nil shell-command-switch command)))
	  ;; Report the output.
	  (with-current-buffer buffer
	    (setq mode-line-process
		  (cond ((null exit-status)
			 " - Error")
			((stringp exit-status)
			 (format " - Signal [%s]" exit-status))
			((not (equal 0 exit-status))
			 (format " - Exit [%d]" exit-status)))))
	  (if (with-current-buffer buffer (> (point-max) (point-min)))
	      ;; There's some output, display it
	      (display-message-or-buffer buffer)
	    ;; No output; error?
	    (let ((output
		   (if (and error-file
			    (< 0 (nth 7 (file-attributes error-file))))
		       "some error output"
		     "no output")))
	      (cond ((null exit-status)
		     (message "(Shell command failed with error)"))
		    ((equal 0 exit-status)
		     (message "(Shell command succeeded with %s)"
			      output))
		    ((stringp exit-status)
		     (message "(Shell command killed by signal %s)"
			      exit-status))
		    (t
		     (message "(Shell command failed with code %d and %s)"
			      exit-status output))))
	    ;; Don't kill: there might be useful info in the undo-log.
	    ;; (kill-buffer buffer)
	    ))))

    (when (and error-file (file-exists-p error-file))
      (if (< 0 (nth 7 (file-attributes error-file)))
	  (with-current-buffer (get-buffer-create error-buffer)
	    (let ((pos-from-end (- (point-max) (point))))
	      (or (bobp)
		  (insert "\f\n"))
	      ;; Do no formatting while reading error file,
	      ;; because that can run a shell command, and we
	      ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      ;; Put point after the inserted errors.
	      (goto-char (- (point-max) pos-from-end)))
	    (and display-error-buffer
		 (display-buffer (current-buffer)))))
      (delete-file error-file))
    exit-status))


(provide 'org-babel)
;;; org-babel.el ends here
