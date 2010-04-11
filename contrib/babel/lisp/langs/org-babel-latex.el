;;; org-babel-latex.el --- org-babel functions for latex "evaluation"

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

;; Org-Babel support for evaluating LaTeX source code.
;;
;; Currently on evaluation this returns raw LaTeX code, unless a :file
;; header argument is given in which case small png or pdf files will
;; be created directly form the latex source code.

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "latex")

(add-to-list 'org-babel-tangle-langs '("latex" "tex"))

(defvar org-babel-default-header-args:latex
  '((:results . "latex") (:exports . "results"))
  "Default arguments to use when evaluating a latex source block.")

(defun org-babel-execute:latex (body params)
  "Execute a block of Latex code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Latex source code block")
  (mapc (lambda (pair) ;; replace variables
          (setq body
                (replace-regexp-in-string
                 (regexp-quote (format "%S" (car pair)))
                 (if (stringp (cdr pair))
                     (cdr pair) (format "%S" (cdr pair)))
                 body))) (second (org-babel-process-params params)))
  (if (cdr (assoc :file params))
      (let ((out-file (cdr (assoc :file params)))
            (tex-file (make-temp-file "org-babel-latex" nil ".tex"))
            (pdfheight (cdr (assoc :pdfheight params)))
            (pdfwidth (cdr (assoc :pdfwidth params)))
            (in-buffer (not (string= "no" (cdr (assoc :buffer params)))))
            (org-export-latex-packages-alist
             (append (cdr (assoc :packages params))
                     org-export-latex-packages-alist)))
        (cond
         ((string-match "\\.png$" out-file)
          (org-create-formula-image
           body out-file org-format-latex-options in-buffer))
         ((string-match "\\.pdf$" out-file)
          (org-babel-latex-body-to-tex-file tex-file body pdfheight pdfwidth)
          (when (file-exists-p out-file) (delete-file out-file))
          (rename-file (org-babel-latex-tex-to-pdf tex-file) out-file))
         ((string-match "\\.\\([^\\.]+\\)$" out-file)
          (error
           (message "can not create %s files, please specify a .png or .pdf file"
                    (match-string 1 out-file)))))
        out-file)
    body))

(defun org-babel-latex-body-to-tex-file (tex-file body &optional height width)
  "Extracted from `org-create-formula-image' in org.el."
  (with-temp-file tex-file
    (insert (org-splice-latex-header
	       org-format-latex-header
	       (remove-if
                (lambda (el) (and (listp el) (string= "hyperref" (cadr el))))
                org-export-latex-default-packages-alist)
	       org-export-latex-packages-alist
	       org-format-latex-header-extra)
            (if height (concat "\n" (format "\\pdfpageheight %s" height)) "")
            (if width (concat "\n" (format "\\pdfpagewidth %s" width)) "")
            (if org-format-latex-header-extra
                (concat "\n" org-format-latex-header-extra)
              "")
            "\n\\begin{document}\n" body "\n\\end{document}\n")))

(defun org-babel-latex-tex-to-pdf (tex-file)
  "Extracted from `org-export-as-pdf' in org-latex.el."
  (let* ((wconfig (current-window-configuration))
         (default-directory (file-name-directory tex-file))
         (base (file-name-sans-extension tex-file))
         (pdffile (concat base ".pdf"))
         (cmds org-latex-to-pdf-process)
         (outbuf (get-buffer-create "*Org PDF LaTeX Output*"))
         cmd)
    (if (and cmds (symbolp cmds))
        (funcall cmds tex-file)
      (while cmds
        (setq cmd (pop cmds))
        (while (string-match "%b" cmd)
          (setq cmd (replace-match
                     (save-match-data
                       (shell-quote-argument base))
                     t t cmd)))
        (while (string-match "%s" cmd)
          (setq cmd (replace-match
                     (save-match-data
                       (shell-quote-argument tex-file))
                     t t cmd)))
        (shell-command cmd outbuf outbuf)))
    (if (not (file-exists-p pdffile))
        (error "PDF file was not produced from %s" tex-file)
      (set-window-configuration wconfig)
      (when org-export-pdf-remove-logfiles
        (dolist (ext org-export-pdf-logfiles)
          (setq tex-file (concat base "." ext))
          (and (file-exists-p tex-file) (delete-file tex-file))))
      pdffile)))

(defun org-babel-prep-session:latex (session params)
  (error "Latex does not support sessions"))

(provide 'org-babel-latex)
;;; org-babel-latex.el ends here
