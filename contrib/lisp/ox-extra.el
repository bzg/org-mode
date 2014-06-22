;;; ox-extra.el --- Convenience functions for org export

;; Copyright (C) 2014  Aaron Ecay

;; Author: Aaron Ecay <aaronecay@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains some convenience functions for org export, which
;; are not part of org's core.  Call `ox-extras-activate' passing a
;; list of symbols naming extras, which will be installed globally in
;; your org session.

;; Currently available extras:

;; - `latex-header-blocks' -- allow the use of latex blocks, the
;; contents of which which will be interpreted as #+latex_header lines
;; for export.  These blocks should be tagged with #+header: :header
;; yes.  For example:
;; #+header: :header yes
;; #+begin_latex
;;   ...
;; #+end_latex

;; TODO:
;; - add a function to org-mode-hook that looks for a ox-extras local
;;   variable and activates the specified extras buffer-locally
;; - allow specification of desired extras to be activated via
;;   customize

;;; Code:

(require 'ox)
(eval-when-compile (require 'cl))

(defun org-latex-header-blocks-filter (backend)
  (when (org-export-derived-backend-p backend 'latex)
    (let ((positions
	   (org-element-map (org-element-parse-buffer 'greater-element nil) 'export-block
	     (lambda (block)
	       (when (and (string= (org-element-property :type block) "LATEX")
			  (string= (org-export-read-attribute
				    :header block :header)
				   "yes"))
		 (list (org-element-property :begin block)
		       (org-element-property :end block)
		       (org-element-property :post-affiliated block)))))))
      (mapc (lambda (pos)
	      (goto-char (nth 2 pos))
	      (destructuring-bind
		  (beg end &rest ignore)
		  (org-edit-src-find-region-and-lang)
		(let ((contents-lines (split-string
				       (buffer-substring-no-properties beg end)
				       "\n")))
		  (delete-region (nth 0 pos) (nth 1 pos))
		  (dolist (line contents-lines)
		    (insert (concat "#+latex_header: "
				    (replace-regexp-in-string "\\` *" "" line)
				    "\n"))))))
	    ;; go in reverse, to avoid wrecking the numeric positions
	    ;; earlier in the file
	    (reverse positions)))))

(defconst ox-extras
  '((latex-header-blocks org-latex-header-blocks-filter org-export-before-parsing-hook))
  "A list of org export extras that can be enabled.

Should be a list of items of the form (NAME FN HOOK).  NAME is a
symbol, which can be passed to `ox-extras-activate'.  FN is a
function which will be added to HOOK.")

(defun ox-extras-activate (extras)
  "Activate certain org export extras.

EXTRAS should be a list of extras (defined in `ox-extras') which
should be activated."
  (dolist (extra extras)
    (let* ((lst (assq extra ox-extras))
	   (fn (nth 1 lst))
	   (hook (nth 2 lst)))
      (when (and fn hook)
	(add-hook hook fn)))))

(defun ox-extras-deactivate (extras)
  "Deactivate certain org export extras.

This function is the opposite of `ox-extras-activate'.  EXTRAS
should be a list of extras (defined in `ox-extras') which should
be activated."
  (dolist (extra extras)
    (let* ((lst (assq extra ox-extras))
	   (fn (nth 1 lst))
	   (hook (nth 2 lst)))
      (when (and fn hook)
	(remove-hook hook fn)))))

(provide 'ox-extra)
;;; ox-extra.el ends here
