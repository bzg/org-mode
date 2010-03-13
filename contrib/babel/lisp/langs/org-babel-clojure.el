;;; org-babel-clojure.el --- org-babel functions for clojure evaluation

;; Copyright (C) 2009 Joel Boehland

;; Author: Joel Boehland
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

;;; Org-Babel support for evaluating clojure code

;;; Requirements:

;;; A working clojure install. This also implies a working java executable
;;; clojure-mode
;;; slime
;;; swank-clojure

;;; By far, the best way to install these components is by following
;;; the directions as set out by Phil Hagelberg (Technomancy) on the
;;; web page: http://technomancy.us/126

;;; Code:
(require 'org-babel)
(require 'cl)
(require 'slime)
(require 'swank-clojure)


(org-babel-add-interpreter "clojure")

(add-to-list 'org-babel-tangle-langs '("clojure" "clj"))

(defvar org-babel-clojure-wrapper-method
  "
(defn spit
  [f content]
  (with-open [#^java.io.PrintWriter w
                 (java.io.PrintWriter.
                   (java.io.BufferedWriter.
                     (java.io.OutputStreamWriter.
                       (java.io.FileOutputStream.
                         (java.io.File. f)))))]
      (.print w content)))

(defn main
  []
  %s)

(spit \"%s\" (str (main)))") ;;" <-- syntax highlighting is messed without this double quote

;;taken mostly from clojure-test-mode.el
(defun org-babel-clojure-clojure-slime-eval (string &optional handler)
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (or handler #'identity)))

(defun org-babel-clojure-slime-eval-sync (string)
  (slime-eval `(swank:eval-and-grab-output ,string)))

;;taken from swank-clojure.el
(defun org-babel-clojure-babel-clojure-cmd ()
  "Create the command to start clojure according to current settings."
  (if (and (not swank-clojure-binary) (not swank-clojure-classpath))
      (error "You must specifiy either a `swank-clojure-binary' or a `swank-clojure-jar-path'")
    (if swank-clojure-binary
        (if (listp swank-clojure-binary)
            swank-clojure-binary
          (list swank-clojure-binary))
      (delete-if
       'null
       (append
        (list swank-clojure-java-path)
        swank-clojure-extra-vm-args
        (list
         (when swank-clojure-library-paths
           (concat "-Djava.library.path="
                   (swank-clojure-concat-paths swank-clojure-library-paths)))
         "-classpath"
         (swank-clojure-concat-paths
          (append
           swank-clojure-classpath
           swank-clojure-extra-classpaths))
         "clojure.main"))))))

(defun org-babel-clojure-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (string-match "^\\[.+\\]$" results)
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               ", " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(defun org-babel-clojure-var-to-clojure (var)
  "Convert an elisp var into a string of clojure source code
specifying a var of the same value."
  (if (listp var)
      (format "'%s" var)
    (format "%S" var)))

(defun org-babel-clojure-build-full-form (body vars)
  "Construct a clojure let form with vars as the let vars"
  (let ((vars-forms (mapconcat ;; define any variables
                      (lambda (pair)
                        (format "%s %s" (car pair) (org-babel-clojure-var-to-clojure (cdr pair))))
                      vars "\n      ")))
    (format "(let [%s]\n  %s)" vars-forms (org-babel-trim body))))

(defun org-babel-prep-session:clojure (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session-buf (org-babel-clojure-initiate-session session))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar ;; define any top level session variables
                     (lambda (pair)
                       (format "(def %s %s)\n" (car pair)
                               (org-babel-clojure-var-to-clojure (cdr pair))))
                     vars)))
    session-buf))

(defun org-babel-load-session:clojure (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:clojure session params)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (org-babel-chomp body)))
      buffer)))

(defvar org-babel-clojure-buffers '())
(defvar org-babel-clojure-pending-sessions '())

(defun org-babel-clojure-session-buffer (session)
  (cdr (assoc session org-babel-clojure-buffers)))

(defun org-babel-clojure-initiate-session-by-key (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion
    (let* ((session (if session
                        (if (stringp session) (intern session)
                          session)
                        :default))
           (clojure-buffer (org-babel-clojure-session-buffer session)))
      (unless (and clojure-buffer (buffer-live-p clojure-buffer))
        (setq org-babel-clojure-buffers (assq-delete-all session org-babel-clojure-buffers))
        (push session org-babel-clojure-pending-sessions)
        (slime)
        ;; we are waiting to finish setting up which will be done in
        ;; org-babel-clojure-session-connected-hook below.
        (let ((timeout 9))
          (while (and (not (org-babel-clojure-session-buffer session))
                      (< 0 timeout))
            (message "Waiting for clojure repl for session: %s ... %i" session timeout)
            (sit-for 1)
            (decf timeout)))
        (setq org-babel-clojure-pending-sessions
              (remove session org-babel-clojure-pending-sessions))
        (unless (org-babel-clojure-session-buffer session)
          (error "Couldn't create slime clojure process"))
        (setq clojure-buffer (org-babel-clojure-session-buffer session)))
      session)))

(defun org-babel-clojure-initiate-session (&optional session params)
  "Return the slime-clojure repl buffer bound to this session
or nil if \"none\" is specified"
  (unless (and (stringp session) (string= session "none"))
    (org-babel-clojure-session-buffer (org-babel-clojure-initiate-session-by-key session))))

(defun org-babel-clojure-session-connected-hook ()
  "Finish setting up the bindings of org-babel session to a slime-clojure repl"
  (let ((pending-session (pop org-babel-clojure-pending-sessions)))
    (when pending-session
      (save-excursion
        (switch-to-buffer (slime-output-buffer))
        (rename-buffer (if (stringp pending-session) pending-session (symbol-name pending-session)))
        (org-babel-clojure-bind-session-to-repl-buffer pending-session (slime-output-buffer))))))

(add-hook 'slime-connected-hook 'org-babel-clojure-session-connected-hook)

(defun org-babel-clojure-bind-session-to-repl-buffer (session repl-buffer)
  (when (stringp session) (setq session (intern session)))
  (setq org-babel-clojure-buffers
        (cons (cons session repl-buffer)
              (assq-delete-all session org-babel-clojure-buffers))))

(defun org-babel-clojure-repl-buffer-pred ()
  "Predicate used to test whether the passed in buffer is an active slime-clojure repl buffer"
  (and (buffer-live-p (current-buffer)) (eq major-mode 'slime-repl-mode)))

(defun org-babel-clojure-bind-session-to-repl (session)
  (interactive "sEnter session name: ")
  (let ((repl-bufs (slime-filter-buffers 'org-babel-clojure-repl-buffer-pred)))
    (unless repl-bufs (error "No existing slime-clojure repl buffers exist"))
    (let ((repl-buf (read-buffer "Choose slime-clojure repl: " repl-bufs t)))
      (org-babel-clojure-bind-session-to-repl-buffer session repl-buf))))

(defun org-babel-clojure-evaluate-external-process (buffer body &optional result-type)
  "Evaluate the body in an external process."
  (save-excursion
    (let ((cmd (format "%s -" (mapconcat #'identity (org-babel-clojure-babel-clojure-cmd) " "))))
      (case result-type
	(output
	 (with-temp-buffer
	   (insert body)
	   (org-babel-shell-command-on-region cmd (point-min) (point-max) 'current-buffer 'replace)
	   (buffer-string)))
	(value
	 (let* ((tmp-file (make-temp-file "org-babel-clojure-results-")) exit-code
		(stderr
		 (with-temp-buffer
		   (insert
		    (format org-babel-clojure-wrapper-method body tmp-file tmp-file))
		   (setq exit-code
			 (org-babel-shell-command-on-region (point-min) (point-max) cmd nil 'replace (current-buffer)))
		   (buffer-string))))
	   (if (> exit-code 0) (org-babel-error-notify exit-code stderr))
	   (org-babel-clojure-table-or-string
	    (with-temp-buffer
	      (insert-file-contents (org-babel-maybe-remote-file tmp-file)) (buffer-string)))))))))

(defun org-babel-clojure-evaluate-session (buffer body &optional result-type)
  "Evaluate the body in the context of a clojure session"
  (let ((raw nil)
        (results nil))
    (save-window-excursion
      (set-buffer buffer)
      (setq raw (org-babel-clojure-slime-eval-sync body))
      (setq results (reverse (mapcar #'org-babel-trim raw)))
      (case result-type
        (output (mapconcat #'identity (reverse (cdr results)) "\n"))
        (value (org-babel-clojure-table-or-string (car results)))))))

(defun org-babel-clojure-evaluate (buffer body &optional result-type)
  "Pass BODY to the Clojure process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (if session
      (org-babel-clojure-evaluate-session buffer body result-type)
    (org-babel-clojure-evaluate-external-process buffer body result-type)))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with org-babel."
  (let* ((processed-params (org-babel-process-params params))
         (vars (second processed-params))
         (body (org-babel-clojure-build-full-form body vars))
         (session (org-babel-clojure-initiate-session (first processed-params))))
    (org-babel-clojure-evaluate session body (fourth processed-params))))

(provide 'org-babel-clojure)
