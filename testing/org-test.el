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

(defconst org-test-example-file-name
  (expand-file-name "example-file.org" org-test-dir))


;;; Functions for writing tests

(defun org-test-buffer (&optional file)
  "TODO:  Setup and return a buffer to work with.
If file is non-nil insert it's contents in there.")

(defun org-test-compare-with-file (&optional file)
  "TODO:  Compare the contents of the test buffer with FILE.
If file is not given, search for a file named after the test
currently executed.")

(defmacro in-org-example-file (&rest body)
  "Execute body in the Org-mode example file."
  (declare (indent 0))
  `(let ((visited-p (get-file-buffer org-test-example-file-name))
	 to-be-removed)
     (save-window-excursion
       (save-match-data
	 (find-file org-test-example-file-name)
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (outline-next-visible-heading 1)
	 (org-show-subtree)
	 (org-show-block-all)
	 ,@body))
     (unless visited-p
       (kill-buffer to-be-removed))))


;;; Navigation Functions
(defjump 'org-test-jump
  '(("lisp/\\1.el" . "testing/lisp/test-\\1.el")
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
    (let ((full-path (expand-file-name path org-base-dir))
	  (name (file-name-nondirectory path)))
      (find-file full-path)
      (insert
       ";;; " name "\n\n"
       ";; Copyright (c) 2010 " user-full-name "\n"
       ";; Authors: " user-full-name "\n\n"
       ";; Released under the GNU General Public License version 3\n"
       ";; see: http://www.gnu.org/licenses/gpl-3.0.html\n\n"
       ";;;; Comments:\n\n"
       ";; Template test file for Org-mode tests\n\n"
       "\n"
       ";;; Code:\n"
       "(require 'org-test)\n\n"
       "\n"
       ";;; Tests\n"
       "(ert-deftest " name "/example-test ()\n"
       "  \"Just an example to get you started.\"\n"
       "  (should t)\n"
       "  (should-not nil)\n"
       "  (should-error (error \"errr...\")))") full-path))
  (lambda () (car (which-function))))


;;; Load and Run tests
(defun org-load-tests ()
  "Load up the org-mode test suite."
  (interactive)
  (mapc (lambda (file) (load-file file))
	(directory-files (expand-file-name "lisp" org-test-dir)
			 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.el")))

(defun org-test-current-defun ()
  "Test the current function."
  (interactive)
  (ert (car (which-function))))

(defun org-test-run-all-tests ()
  "Run all defined tests matching \"^org\".
Load all test files first."
  (interactive)
  (org-load-tests)
  (ert "^org"))

(provide 'org-test)

;;; org-test.el ends here
