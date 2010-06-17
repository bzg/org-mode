;;; ob-init.el --- working with code blocks in org-mode
;; add the langs/ directory to the load path
(add-to-list 'load-path (expand-file-name
			 "langs"
			 (file-name-directory (or (buffer-file-name)
						  load-file-name))))
(require 'ob)
(require 'ob-table)
(require 'ob-lob)
(require 'ob-ref)
(require 'ob-exp)
(require 'ob-tangle)
(require 'ob-comint)
(require 'ob-keys)
(require 'ob-sh)
(require 'ob-emacs-lisp)

(provide 'ob-init)
;;; ob-init.el ends here