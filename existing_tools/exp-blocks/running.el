(load "org-exp-blocks.el")

(defun org-exp-preprocess ()
  "This is a simple helper to preprocess a file without actually
exporting to html or latex.  Useful for debugging."
  (interactive)
  (let ((htmlp t))
    (org-export-blocks-preprocess)))

