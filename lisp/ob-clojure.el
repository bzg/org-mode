;;; ob-clojure.el --- Babel Functions for Clojure    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: Joel Boehland, Eric Schulte, Oleh Krehel, Frederick Giasson
;;
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
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

;; Support for evaluating clojure code

;; Requirements:

;; - clojure (at least 1.2.0)
;; - clojure-mode
;; - inf-clojure, cider or SLIME

;; For clojure-mode, see https://github.com/clojure-emacs/clojure-mode
;; For cider, see https://github.com/clojure-emacs/cider
;; For inf-clojure, see https://github.com/clojure-emacs/cider

;; For SLIME, the best way to install these components is by following
;; the directions as set out by Phil Hagelberg (Technomancy) on the
;; web page: http://technomancy.us/126

;;; Code:
(require 'cl-lib)
(require 'ob)
(require 'org-macs)

(declare-function cider-jack-in "ext:cider" (&optional prompt-project cljs-too))
(declare-function cider-current-connection "ext:cider-client" (&optional type))
(declare-function cider-current-ns "ext:cider-client" ())
(declare-function cider-repls "ext:cider-connection" (&optional type ensure))
(declare-function nrepl--merge "ext:nrepl-client" (dict1 dict2))
(declare-function nrepl-dict-get "ext:nrepl-client" (dict key))
(declare-function nrepl-dict-put "ext:nrepl-client" (dict key value))
(declare-function nrepl-request:eval "ext:nrepl-client" (input callback connection &optional ns line column additional-params tooling))
(declare-function nrepl-sync-request:eval "ext:nrepl-client" (input connection &optional ns tooling))
(declare-function slime-eval "ext:slime" (sexp &optional package))
(declare-function inf-clojure "ext:inf-clojure" (cmd))
(declare-function inf-clojure-eval-string "ext:inf-clojure" (code))

(defvar nrepl-sync-request-timeout)
(defvar cider-buffer-ns)
(defvar sesman-system)
(defvar cider-version)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
(add-to-list 'org-babel-tangle-lang-exts '("clojurescript" . "cljs"))

(defvar org-babel-default-header-args:clojure '())
(defvar org-babel-header-args:clojure '((ns . :any)
					(package . :any)))

(defvar org-babel-default-header-args:clojurescript '())
(defvar org-babel-header-args:clojurescript '((package . :any)))

(defcustom org-babel-clojure-backend nil
  "Backend used to evaluate Clojure code blocks."
  :group 'org-babel
  :type '(choice
	  (const :tag "inf-clojure" inf-clojure)
	  (const :tag "cider" cider)
	  (const :tag "slime" slime)
	  (const :tag "Not configured yet" nil)))

(defcustom org-babel-clojure-sync-nrepl-timeout 10
  "Timeout value, in seconds, of a Clojure sync call.
If the value is nil, timeout is disabled."
  :group 'org-babel
  :type 'integer
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe #'wholenump)

(defcustom org-babel-clojure-default-ns "user"
  "Default Clojure namespace for source block when finding ns failed."
  :type 'string
  :group 'org-babel)

(defun org-babel-clojure-cider-current-ns ()
  "Like `cider-current-ns' except `cider-find-ns'."
  (or cider-buffer-ns
      (let ((repl-buf (cider-current-connection)))
	(and repl-buf (buffer-local-value 'cider-buffer-ns repl-buf)))
      org-babel-clojure-default-ns))

(defun org-babel-expand-body:clojure (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel--get-vars params))
	 (ns (or (cdr (assq :ns params))
		 (org-babel-clojure-cider-current-ns)))
	 (result-params (cdr (assq :result-params params)))
	 (print-level nil)
	 (print-length nil)
	 ;; Remove comments, they break (let [...] ...) bindings
	 (body (replace-regexp-in-string "^[ 	]*;+.*$" "" body))
	 (body (org-trim
		(concat
		 ;; Source block specified namespace :ns.
		 (and (cdr (assq :ns params)) (format "(ns %s)\n" ns))
		 ;; Variables binding.
		 (if (null vars) (org-trim body)
		   (format "(let [%s]\n%s)"
			   (mapconcat
			    (lambda (var)
			      (format "%S %S" (car var) (cdr var)))
			    vars
			    "\n      ")
			   body))))))
    (if (or (member "code" result-params)
	    (member "pp" result-params))
	(format "(clojure.pprint/pprint (do %s))" body)
      body)))

(defvar ob-clojure-inf-clojure-tmp-output nil)
(defun ob-clojure-inf-clojure-output (s)
  "Store a trimmed version of S in a variable and return S."
  (sit-for .1) ; necessary to get the output ordered
  (let ((s0 (org-trim s)))
    (push s0 ob-clojure-inf-clojure-tmp-output))
  s)

(defvar inf-clojure-buffer)
(defvar inf-clojure-comint-prompt-regexp)
(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (unless org-babel-clojure-backend
    (user-error "You need to customize org-babel-clojure-backend"))
  (let* ((expanded (org-babel-expand-body:clojure body params))
	 (response (list 'dict))
	 (result-params (cdr (assq :result-params params)))
	 result)
    (cond
     ((eq org-babel-clojure-backend 'inf-clojure)
      (condition-case nil (require 'inf-clojure)
	(user-error "inf-clojure not available"))
      (add-hook 'comint-preoutput-filter-functions
		#'ob-clojure-inf-clojure-output)
      (setq ob-clojure-inf-clojure-tmp-output nil)
      (unless (and inf-clojure-buffer
		   (buffer-live-p (get-buffer inf-clojure-buffer)))
	(setq comint-prompt-regexp inf-clojure-comint-prompt-regexp)
	(save-window-excursion (call-interactively #'inf-clojure))
	(sit-for 5)
	(setq ob-clojure-inf-clojure-tmp-output nil))
      (inf-clojure-eval-string expanded)
      (sit-for .3)
      (remove-hook 'comint-preoutput-filter-functions
		   #'ob-clojure-inf-clojure-output)
      (setq result
	    (mapconcat
	     (lambda (s)
	       ;; FIXME: don't hardcode "user=>" below
	       (unless (or (string-match-p "user=>" s)) s))
	     (delete "" ob-clojure-inf-clojure-tmp-output)
	     "\n")))
     ((eq org-babel-clojure-backend 'cider)
      (condition-case nil (require 'cider)
	(user-error "cider not available"))
      (let ((connection (cider-current-connection (cdr (assq :target params)))))
	(unless connection
	  (message "Starting cider default session")
	  (org-babel-clojure-initiate-session "default"))
	(setq response
	      (let ((nrepl-sync-request-timeout
		     org-babel-clojure-sync-nrepl-timeout))
		(nrepl-sync-request:eval expanded connection)))
	(setq result
	      (concat
	       (nrepl-dict-get response
			       (if (or (member "output" result-params)
				       (member "pp" result-params))
				   "out"
				 "value"))
	       (nrepl-dict-get response "ex")
	       (nrepl-dict-get response "root-ex")
	       (nrepl-dict-get response "err")))))
     ((eq org-babel-clojure-backend 'slime)
      (require 'slime)
      (with-temp-buffer
	(insert expanded)
	(setq result
	      (slime-eval
	       `(swank:eval-and-grab-output
		 ,(buffer-substring-no-properties (point-min) (point-max)))
	       (cdr (assq :package params)))))))
    (org-babel-result-cond result-params
      result
      (condition-case nil (org-babel-script-escape result)
	(error result)))))

(defun org-babel-execute:clojurescript (body params)
  "Execute a block of ClojureScript code with Babel."
  (org-babel-execute:clojure body (cons '(:target . "cljs") params)))

(defun org-babel-edit-prep:clojure (babel-info)
  "Set `org-edit-special' src block by injecting `cider-buffer-ns' as namespace."
  (let ((namespace (cdr (assq :ns (nth 2 babel-info)))))
    (when (and (eq org-babel-clojure-backend 'cider) namespace)
      (setq-local cider-buffer-ns namespace))))

(defun org-babel-clojure-initiate-session (session &optional _params)
  "Initiate a session named SESSION according to PARAMS."
  (when (not (string= session "none"))
    (save-window-excursion
      (cond
       ;; Session is already initiated
       ((org-babel-comint-buffer-livep session) nil)
       ;; Initiate a session with CIDER jack-in
       ((eq org-babel-clojure-backend 'cider)
	(condition-case nil (require 'cider)
	  (user-error "cider not available"))
	(let ((session-buffer
	       (if (version< cider-version "0.18.0")
		   ;; Older CIDER (without sesman) still need to use
		   ;; old way.
		   (cider-jack-in nil) ; jack-in without project
		 ;; New CIDER (with sesman to manage sessions).
		 (unless (cider-repls)
		   (let ((sesman-system 'CIDER))
		     (call-interactively 'sesman-link-with-directory)))
		 (current-buffer))))
          (when (org-babel-comint-buffer-livep session-buffer)
            (sit-for .3)
	    session-buffer)))
       ((eq org-babel-clojure-backend 'inf-clojure)
	(user-error "Session evaluation with inf-clojure is not supported"))
       ((eq org-babel-clojure-backend 'slime)
        (user-error "Session evaluation with SLIME is not supported"))
       (t (user-error "Session initiate failed"))))))

(defun org-babel-prep-session:clojure (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let ((session (org-babel-clojure-initiate-session session))
        (var-lines (org-babel-variable-assignments:clojure params)))
    (when session
      (org-babel-comint-in-buffer session
	(dolist (var var-lines)
	  (insert var)
	  (comint-send-input nil t)
	  (org-babel-comint-wait-for-output session)
	  (sit-for .1)
	  (goto-char (point-max)))))
    session))

(defun org-babel-clojure-var-to-clojure (var)
  "Convert src block's VAR to Clojure variable."
  (cond
   ((listp var)
    (replace-regexp-in-string "(" "'(" var))
   ((stringp var)
    ;; Wrap Babel passed-in header argument value with quotes in Clojure.
    (format "\"%s\"" var))
   (t
    (format "%S" var))))

(defun org-babel-variable-assignments:clojure (params)
  "Return a list of Clojure statements assigning the block's variables in PARAMS."
  (mapcar
   (lambda (pair)
     (format "(def %s %s)"
             (car pair)
             (org-babel-clojure-var-to-clojure (cdr pair))))
   (org-babel--get-vars params)))

(provide 'ob-clojure)

;;; ob-clojure.el ends here
