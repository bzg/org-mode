;;; org-babel-gnuplot.el --- org-babel functions for gnuplot evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
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

;; Org-Babel support for evaluating gnuplot source code.
;;
;; This differs from most standard languages in that
;;
;; 1) we are generally only going to return results of type "file"
;;
;; 2) we are adding the "file" and "cmdline" header arguments

;;; Code:
(require 'org-babel)
(require 'gnuplot)

(org-babel-add-interpreter "gnuplot")

(add-to-list 'org-babel-tangle-langs '("gnuplot" "gnuplot"))

(defvar org-babel-default-header-args:gnuplot '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a gnuplot source block.")

(defvar org-babel-gnuplot-timestamp-fmt nil)

(defun org-babel-execute:gnuplot (body params)
  "Execute a block of Gnuplot code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Gnuplot source code block")
  (let* ((vars (org-babel-ref-variables params))
         (result-params (split-string (or (cdr (assoc :results params)) "")))
         (out-file (cdr (assoc :file params)))
         (cmdline (cdr (assoc :cmdline params)))
         (in-file (make-temp-file "org-babel-ditaa"))
	 (title (plist-get params :title))
         (lines (plist-get params :line))
	 (sets (plist-get params :set))         
	 (x-labels (plist-get params :xlabels))
	 (y-labels (plist-get params :ylabels))
	 (time-ind (plist-get params :timeind)))
    ;; insert variables into code body
    (mapc
     (lambda (pair)
       (message "resolving %S" pair) ;; debugging
       (setq body
             (replace-regexp-in-string
              (regexp-quote (format "%s" (car pair)))
              (if (listp (cdr pair))
                  (org-babel-gnuplot-table-to-data
                   (cdr pair) (make-temp-file "org-babel-gnuplot") params)
                (cdr pair)) body)))
     vars)
    ;; append header argument settings to body
    (when title (add-to-script (format "set title '%s'" title))) ;; title
    (when lines (mapc (lambda (el) (add-to-script el)) lines)) ;; line
    (when sets ;; set
      (mapc (lambda (el) (add-to-script (format "set %s" el))) sets))
    (when x-labels ;; x labels (xtics)
      (add-to-script
       (format "set xtics (%s)"
               (mapconcat (lambda (pair)
                            (format "\"%s\" %d" (cdr pair) (car pair)))
                          x-labels ", "))))
    (when y-labels ;; y labels (ytics)
      (add-to-script
       (format "set ytics (%s)"
               (mapconcat (lambda (pair)
                            (format "\"%s\" %d" (cdr pair) (car pair)))
                          y-labels ", "))))
    (when time-ind ;; timestamp index
      (add-to-script "set xdata time")
      (add-to-script (concat "set timefmt \""
                             (or timefmt ;; timefmt passed to gnuplot
                                 "%Y-%m-%d-%H:%M:%S") "\"")))
    ;; evaluate the code body with gnuplot
    (with-temp-buffer
      (insert (concat body "\n"))
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot))
    out-file))

(defun org-babel-prep-session:gnuplot (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-gnuplot-initiate-session session))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar ;; define any variables
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-ruby-var-to-ruby (cdr pair))))
                     vars)))
    ;; (message "vars=%S" vars) ;; debugging
    (org-babel-comint-in-buffer session
      (sit-for .5) (goto-char (point-max))
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)
              (sit-for .1) (goto-char (point-max))) var-lines))))

(defun org-babel-gnuplot-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (let ((session-buffer (save-window-excursion (run-ruby nil session) (current-buffer))))
      (if (org-babel-comint-buffer-livep session-buffer)
          session-buffer
        (sit-for .5)
        (org-babel-ruby-initiate-session session)))))

(defun org-babel-gnuplot-quote-timestamp-field (s)
  "Convert field S from timestamp to Unix time and export to gnuplot."
  (format-time-string org-babel-gnuplot-timestamp-fmt (org-time-string-to-time s)))

(defun org-babel-gnuplot-quote-tsv-field (s)
  "Quote field S for export to gnuplot."
  (unless (stringp s)
    (setq s (format "%s" s)))
  (if (string-match org-table-number-regexp s) s
    (if (string-match org-ts-regexp3 s)
	(org-babel-gnuplot-quote-timestamp-field s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\""))))

(defun org-babel-gnuplot-table-to-data (table data-file params)
  "Export TABLE to DATA-FILE in a format readable by gnuplot.
Pass PARAMS through to `orgtbl-to-generic' when exporting TABLE."
  (with-temp-file data-file
    (message "table = %S" table)
    (make-local-variable 'org-babel-gnuplot-timestamp-fmt)
    (setq org-babel-gnuplot-timestamp-fmt (or
                                           (plist-get params :timefmt)
                                           "%Y-%m-%d-%H:%M:%S"))
    (insert (orgtbl-to-generic
	     table
	     (org-combine-plists
	      '(:sep "\t" :fmt org-babel-gnuplot-quote-tsv-field)
	      params))))
  data-file)

(provide 'org-babel-gnuplot)
;;; org-babel-gnuplot.el ends here
