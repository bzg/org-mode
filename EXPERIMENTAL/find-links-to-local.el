(defun org-find-links ()
  (let* ((file (buffer-file-name))
	 (tname (file-truename file)))
