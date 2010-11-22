;;;; org-test.el --- Tests for Org-mode

;; Copyright (c) 2010 Sebastian Rose, Eric Schulte
;; Authors:
;;     Sebastian Rose, Hannover, Germany, sebastian_rose gmx de
;;     Eric Schulte, Santa Fe, New Mexico, USA, schulte.eric gmail com

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Interactive testing for Org mode.

;; The heart of all this is the commands `org-test-current-defun'.  If
;; called while in a `defun' all ert tests with names matching the
;; name of the function are run.

;;; Prerequisites:

;; ERT and jump.el are both included as git submodules, install with
;;   $ git submodule init
;;   $ git submodule update


;;;; Code:
(let* ((org-test-dir (expand-file-name
		      (file-name-directory
		       (or load-file-name buffer-file-name))))
       (load-path (cons
		   (expand-file-name "ert" org-test-dir)
		   (cons
		    (expand-file-name "jump" org-test-dir)
		    load-path))))
  (require 'ert-batch)
  (require 'ert)
  (require 'ert-exp)
  (require 'ert-exp-t)
  (require 'ert-run)
  (require 'ert-ui)
  (require 'jump)
  (require 'which-func)
  (require 'org))

(defconst org-test-default-test-file-name "tests.el"
  "For each defun a separate file with tests may be defined.
tests.el is the fallback or default if you like.")

(defconst org-test-default-directory-name "testing"
  "Basename or the directory where the tests live.
org-test searches this directory up the directory tree.")

(defconst org-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-base-dir
  (expand-file-name ".." org-test-dir))

(defconst org-test-example-dir
  (expand-file-name "examples" org-test-dir))

(defconst org-test-file
  (expand-file-name "normal.org" org-test-example-dir))

(defconst org-test-no-heading-file
  (expand-file-name "no-heading.org" org-test-example-dir))

(defconst org-test-link-in-heading-file
  (expand-file-name "link-in-heading.org" org-test-dir))


;;; Functions for writing tests
(defun org-test-buffer (&optional file)
  "TODO:  Setup and return a buffer to work with.
If file is non-nil insert it's contents in there.")

(defun org-test-compare-with-file (&optional file)
  "TODO:  Compare the contents of the test buffer with FILE.
If file is not given, search for a file named after the test
currently executed.")

(defmacro org-test-at-id (id &rest body)
  "Run body after placing the point in the headline identified by ID."
  (declare (indent 1))
  `(let* ((id-location (org-id-find ,id))
	  (id-file (car id-location))
	  (visited-p (get-file-buffer id-file))
	  to-be-removed)
     (save-window-excursion
       (save-match-data
	 (org-id-goto ,id)
	 (setq to-be-removed (current-buffer))
	 (condition-case nil
	     (progn
	       (org-show-subtree)
	       (org-show-block-all))
	   (error nil))
	 (save-restriction ,@body)))
     (unless visited-p
       (kill-buffer to-be-removed))))

(defmacro org-test-in-example-file (file &rest body)
  "Execute body in the Org-mode example file."
  (declare (indent 1))
  `(let* ((my-file (or ,file org-test-file))
	  (visited-p (get-file-buffer my-file))
	  to-be-removed)
     (save-window-excursion
       (save-match-data
	 (find-file my-file)
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (condition-case nil
	     (progn
	       (outline-next-visible-heading 1)
	       (org-show-subtree)
	       (org-show-block-all))
	   (error nil))
	 (save-restriction ,@body)))
     (unless visited-p
       (kill-buffer to-be-removed))))

(defmacro org-test-at-marker (file marker &rest body)
  "Run body after placing the point at MARKER in FILE.
Note the uuidgen command-line command can be useful for
generating unique markers for insertion as anchors into org
files."
  (declare (indent 2))
  `(org-test-in-example-file ,file
     (goto-char (point-min))
     (re-search-forward (regexp-quote ,marker))
     ,@body))


;;; Navigation Functions
(defjump org-test-jump
  (("lisp/\\1.el" . "testing/lisp/test-\\1.el")
   ("lisp/\\1.el" . "testing/lisp/\\1.el/test.*.el")
   ("contrib/lisp/\\1.el" . "testing/contrib/lisp/test-\\1.el")
   ("contrib/lisp/\\1.el" . "testing/contrib/lisp/\\1.el/test.*.el")
   ("testing/lisp/test-\\1.el" . "lisp/\\1.el")
   ("testing/lisp/\\1.el" . "lisp/\\1.el/test.*.el")
   ("testing/contrib/lisp/test-\\1.el" . "contrib/lisp/\\1.el")
   ("testing/contrib/lisp/test-\\1.el" . "contrib/lisp/\\1.el/test.*.el"))
  (concat org-base-dir "/")
  "Jump between org-mode files and their tests."
  (lambda (path)
    (let* ((full-path (expand-file-name path org-base-dir))
	  (file-name (file-name-nondirectory path))
	  (name (file-name-sans-extension file-name)))
      (find-file full-path)
      (insert
       ";;; " file-name "\n\n"
       ";; Copyright (c) " (nth 5 (decode-time (current-time)))
       " " user-full-name "\n"
       ";; Authors: " user-full-name "\n\n"
       ";; Released under the GNU General Public License version 3\n"
       ";; see: http://www.gnu.org/licenses/gpl-3.0.html\n\n"
       ";;;; Comments:\n\n"
       ";; Template test file for Org-mode tests\n\n"
       "\n"
       ";;; Code:\n"
       "(let ((load-path (cons (expand-file-name\n"
       "			\"..\" (file-name-directory\n"
       "			      (or load-file-name buffer-file-name)))\n"
       "		       load-path)))\n"
       "  (require 'org-test)\n"
       "  (require 'org-test-ob-consts))\n\n"
       "\n"
       ";;; Tests\n"
       "(ert-deftest " name "/example-test ()\n"
       "  \"Just an example to get you started.\"\n"
       "  (should t)\n"
       "  (should-not nil)\n"
       "  (should-error (error \"errr...\")))\n\n\n"
       "(provide '" name ")\n\n"
       ";;; " file-name " ends here\n") full-path))
  (lambda () ((lambda (res) (if (listp res) (car res) res)) (which-function))))

(define-key emacs-lisp-mode-map "\M-\C-j" 'org-test-jump)


;;; Load and Run tests
(defun org-test-load ()
  "Load up the org-mode test suite."
  (interactive)
  (flet ((rload (base)
		(mapc
		 (lambda (path)
		   (if (file-directory-p path) (rload path) (load-file path)))
		 (directory-files base 'full
				  "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.el"))))
    (rload (expand-file-name "lisp" org-test-dir))
    (rload (expand-file-name "lisp"
			     (expand-file-name "contrib" org-test-dir)))))

(defun org-test-current-defun ()
  "Test the current function."
  (interactive)
  (ert (which-function)))

(defun org-test-current-file ()
  "Run all tests for current file."
  (interactive)
  (ert (concat "test-"
	       (file-name-sans-extension
		(file-name-nondirectory (buffer-file-name)))
	       "/")))

(defun org-test-run-all-tests ()
  "Run all defined tests matching \"\\(org\\|ob\\)\".
Load all test files first."
  (interactive)
  (org-test-load)
  (ert "\\(org\\|ob\\)"))

(provide 'org-test)

;;; org-test.el ends here
