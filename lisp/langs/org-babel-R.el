;;; org-babel-R.el --- org-babel functions for R code evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, R, statistics
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

;; Org-Babel support for evaluating R code

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "R")

(defun org-babel-execute:R (body params)
  "Execute a block of R code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing R source code block...")
  (save-window-excursion
    (let* ((vars (org-babel-ref-variables params))
           (full-body (concat
                       (mapconcat ;; define any variables
                        (lambda (pair)
                          (org-babel-R-assign-elisp (car pair) (cdr pair)))
                        vars "\n") "\n" body "\n"))
           (result-params (split-string (or (cdr (assoc :results params)) "")))
           (result-type (cond ((member "output" result-params) 'output)
                              ((member "value" result-params) 'value)
                              (t 'value)))
           (session (org-babel-R-initiate-session (cdr (assoc :session params))))
           results)
      ;; ;;; debugging statements
      ;; (message (format "result-type=%S" result-type))
      ;; (message (format "body=%S" body))
      ;; (message (format "session=%S" session))
      ;; (message (format "result-params=%S" result-params))
      ;; evaluate body and convert the results to ruby
      (setq results (org-babel-R-evaluate session full-body result-type))
      (setq results (if (member "scalar" result-params)
                        results
                      (let ((tmp-file (make-temp-file "org-babel-R")))
                        (with-temp-file tmp-file (insert results))
                        (org-babel-import-elisp-from-file tmp-file))))
      (if (and (member "vector" result-params) (not (listp results)))
          (list (list results))
        results))))

(defun org-babel-prep-session:R (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-R-initiate-session session))
         (vars (org-babel-ref-variables params)))
    (mapc (lambda (pair) (org-babel-R-assign-elisp session (car pair) (cdr pair))) vars)))

;; helper functions

(defun org-babel-R-quote-tsv-field (s)
  "Quote field S for export to R."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-R-assign-elisp (name value)
  "Read the elisp VALUE into a variable named NAME."
  (if (listp value)
      (let ((transition-file (make-temp-file "org-babel-R-import")))
        ;; ensure VALUE has an orgtbl structure (depth of at least 2)
        (unless (listp (car value)) (setq value (list value)))
        (with-temp-file transition-file
          (insert (orgtbl-to-tsv value '(:fmt org-babel-R-quote-tsv-field)))
          (insert "\n"))
        (format "%s <- read.table(\"%s\", header=FALSE, sep=\"\\t\", as.is=TRUE)"
                name transition-file))
    (format "%s <- %s" name (org-babel-R-quote-tsv-field value))))

(defun org-babel-R-initiate-session (session)
  "If there is not a current R process then create one."
  (unless (string= session "none")
    (setq session (or session "*R*"))
    (if (org-babel-comint-buffer-livep session)
        session
      (save-window-excursion (R) (rename-buffer (if (bufferp session) (buffer-name session)
                                                  (if (stringp session) session (buffer-name)))) (current-buffer)))))

(defvar org-babel-R-eoe-indicator "'org_babel_R_eoe'")
(defvar org-babel-R-eoe-output "[1] \"org_babel_R_eoe\"")
(defvar org-babel-R-wrapper-method "main <- function ()\n{\n%s\n}
write.table(main(), file=\"%s\", sep=\"\\t\", na=\"nil\",row.names=FALSE, col.names=FALSE, quote=FALSE)")

(defun org-babel-R-evaluate (buffer body result-type)
  "Pass BODY to the R process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY."
  (if (not session)
      ;; external process evaluation
      (let ((in-tmp-file (make-temp-file "R-in-functional-results"))
            (out-tmp-file (make-temp-file "R-out-functional-results")))
        (case result-type
          (output
           (with-temp-file in-tmp-file (insert body))
           ;; (message "R --slave --no-save < '%s' > '%s'" in-tmp-file out-tmp-file)
           (shell-command-to-string (format "R --slave --no-save < '%s' > '%s'" in-tmp-file out-tmp-file)))
          (value
           (with-temp-file in-tmp-file
             (insert (format org-babel-R-wrapper-method body out-tmp-file)))
           ;; (message "R --no-save < '%s'" in-tmp-file)
           (shell-command (format "R --no-save < '%s'" in-tmp-file))))
        (with-temp-buffer (insert-file-contents out-tmp-file) (buffer-string)))
    ;; comint session evaluation
    (org-babel-comint-in-buffer buffer
      (let* ((tmp-file (make-temp-file "org-babel-R"))
             (last-value-eval
              (format "write.table(.Last.value, file=\"%s\", sep=\"\\t\", na=\"nil\",row.names=FALSE, col.names=FALSE, quote=FALSE)"
                      tmp-file))
             (full-body (mapconcat #'org-babel-chomp (list body last-value-eval org-babel-R-eoe-indicator) "\n"))
             (raw (org-babel-comint-with-output buffer org-babel-R-eoe-output nil
                    (insert full-body) (inferior-ess-send-input)))
             (results (let ((broke nil))
                        (delete nil (mapcar (lambda (el)
                                              (if (or broke
                                                      (and (string-match (regexp-quote org-babel-R-eoe-output) el) (setq broke t)))
                                                  nil
                                                (if (= (length el) 0)
                                                    nil
                                                  (if (string-match comint-prompt-regexp el)
                                                      (substring el (match-end 0))
                                                    el))))
                                            (mapcar #'org-babel-trim raw))))))
        (case result-type
          (output (org-babel-trim (mapconcat #'identity results "\n")))
          (value (org-babel-trim (with-temp-buffer (insert-file-contents tmp-file) (buffer-string))))
          (t (reverse results)))))))

(provide 'org-babel-R)
;;; org-babel-R.el ends here
