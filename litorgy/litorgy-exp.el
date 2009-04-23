;;; litorgy-exp.el --- Exportation of litorgy source blocks

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

;; for more information see the comments in litorgy.el

;;; Code:
(add-to-list 'org-export-blocks '(src litorgy-exp-src-blocks))

(defun litorgy-exp-src-blocks (body &rest headers)
  "Process src block for export.  Depending on the 'export'
headers argument in replace the source code block with...

both ---- the default, display the code and the results

code ---- display the code inside the block but do not process

results - process the block and replace it with the results of
          execution

none ----- do not display either code or results upon export"
  (interactive)
  (unless headers (error "litorgy can't process a source block without knowing the source code"))
  (message "litorgy processing...")
  (let* ((lang (car headers))
         (params (litorgy-parse-header-arguments (mapconcat #'identity (cdr headers) " ")))
         (export (cdr (assoc :exports params))))
    (case (intern (or export "both"))
          ('none "")
          ('code (litorgy-exp-code body lang params))
          ('results (litorgy-exp-results body lang params))
          ('both (concat (litorgy-exp-code body lang params)
                     "\n\n"
                     (litorgy-exp-results body lang params))))))

(defun litorgy-exp-code (body lang params)
  (format "#+BEGIN_SRC %s\n%s%s\n#+END_SRC" lang body
          (if (string-match "\n$" body) "" "\n")))

(defun litorgy-exp-results (body lang params)
  (let* ((cmd (intern (concat "litorgy-execute:" lang)))
         (result (funcall cmd body params))
         (result-as-org (litorgy-result-to-org-string result)))
    (if (stringp result)
        (format "#+BEGIN_EXAMPLE\n%s%s\n#+END_EXAMPLE" result
                (if (string-match "\n$" body) "" "\n"))
      result-as-org)))

(provide 'litorgy-exp)
;;; litorgy-exp.el ends here
