;;;; org-test.el --- Tests for Org-mode

;; Copyright (c) 2010 Sebastian Rose, Hannover, Germany
;; Authors:
;;     Sebastian Rose, Hannover, Germany, sebastian_rose gmx de

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Interactive testing for Org mode.

;; The heart of all this is the commands
;; `org-test-test-current-defun'.  If called while in an emacs-lisp
;; file, org-test first searches for a directory testing/tests/NAME/,
;; where name is the basename of the lisp file you're in.  This
;; directory is then searched for a file named like the defun the
;; point is in.  If that failes, a file named 'tests.el' is searched
;; in this directory.  The file found is loaded and
;; `org-test-run-tests' is called with the prefix "^NAME-OF-DEFUN".

;; The second usefull function is `org-test-test-buffer-file'.  This
;; function searches the same way as `org-test-test-current-defun'
;; does, but only for the tests.el file.  All tests in that file with
;; the prefix "^BUFFER-FILE-NAME" with the ".el" suffix stripped are
;; executed.

;;; Prerequisites:

;; You'll need to download and install ERT to use this stuff.  You can
;; get ERT like this:
;;        sh$  git clone http://github.com/ohler/ert.git



;;;; Code:

(require 'ert-batch)
(require 'ert)
(require 'ert-exp)
(require 'ert-exp-t)
(require 'ert-run)
(require 'ert-ui)

(require 'org)



(defconst org-test-default-test-file-name "tests.el"
  "For each defun a separate file with tests may be defined.
tests.el is the fallback or default if you like.")

(defconst org-test-default-directory-name "testing"
  "Basename or the directory where the tests live.
org-test searches this directory up the directory tree.")



;;; Find tests

(defun org-test-test-directory-for-file (file)
  "Search up the directory tree for a directory
called like `org-test-default-directory-name'.
If that directory is not found, ask the user.

Return the name of the directory that should contain tests for
FILE regardless of it's existence.

If the directory `org-test-default-directory-name' cannot be
found up the directory tree, return nil."
  (let* ((file (file-truename
		(or file buffer-file-name)))
	 (orig
	  (file-name-directory
	   (expand-file-name (or file buffer-file-name))))
	 (parent orig)
	 (child "")
	 base)
    (catch 'dir
      (progn
	(while (not (string= parent child))
	  (let ((td (file-name-as-directory
		     (concat parent
			     org-test-default-directory-name))))
	    (when (file-directory-p td)
	      (setq base parent)
	      (throw 'dir parent))
	    (setq child parent)
	    (setq parent (file-name-as-directory
			  (file-truename (concat parent ".."))))))
	(throw 'dir nil)))

    (if base
	;; For now, rely on the fact, that if base exists, the rest of
	;; the directory setup is as expected, too.
	(progn
	  (file-name-as-directory
	   (concat
	    (file-name-as-directory
	     (file-truename
	      (concat
	       (file-name-as-directory
		(concat base org-test-default-directory-name))
	       (file-relative-name orig base))))
	    (file-name-nondirectory file))))
      ;; TODO:
      ;; it's up to the user to find the directory for the file he's
      ;; testing...
      ;; (setq base (read-directory-name
      ;;	  "Testdirectory: " orig orig t))
      nil)))

(defun org-test-test-file-name-for-file (directory file)
  "Return the name of the file that should contain the tests for FILE.
FILE might be a path or a base filename.
Return nil if no file tests for FILE exists."
  ;; TODO: fall back on a list of all *.el files in this directory.
  (let ((tf (concat directory
		    org-test-default-test-file-name)))
    (if (file-exists-p tf)
	tf
      nil)))

(defun org-test-test-file-name-for-defun (directory fun &optional file)
  "Return the name of the file that might or might not contain tests
for defun FUN (a string) defined FILE.  Return nil if no file with
special tests for FUN exists."
  (let* ((funsym (intern fun))
         (file (or file
                   (find-lisp-object-file-name
                    (intern fun)
                    (symbol-function (intern fun)))))
         (tf (concat directory fun ".el")))
    (if (file-exists-p tf)
	tf
      nil)))



;;; TODO: Test buffers and control files

(defun org-test-buffer (&optional file)
  "TODO:  Setup and return a buffer to work with.
If file is non-nil insert it's contents in there.")

(defun org-test-compare-with-file (&optional file)
  "TODO:  Compare the contents of the test buffer with FILE.
If file is not given, search for a file named after the test
currently executed.")



;;; Run tests

(defun org-test-run-tests (&optional selector)
  "Run all tests matched by SELECTOR.
SELECTOR defaults to \"^org\".
See the docstring of `ert-select-tests' for valid selectors.
Tests are run inside
 (let ((deactivate-mark nil))
    (save-excursion
      (save-match-data
    ...)))."
  (interactive)
  (let ((select (or selector "^org"))
	(deactivate-mark nil))
    (save-excursion
      (save-match-data
	  (ert select)))))

(defun org-test-run-all-tests ()
  "Run all defined tests matching \"^org\".
Unlike `org-test-run-tests', load all test files first.
Uses `org-test-run-tests' to run the actual tests."
  (interactive)
  (let* ((org-dir
	  (file-name-directory
	   (find-lisp-object-file-name 'org-mode 'function)))
	 (org-files
	  (directory-files org-dir nil "\\.el")))
    (message "Loading all tests....")
    (mapc
     (lambda (f)
       (let* ((dir (org-test-test-directory-for-file f)))
	 (when (and dir (file-directory-p dir))
	   (let ((tfs (directory-files dir t "\\.el")))
	     (mapc (lambda (tf)
		     (load-file tf))
		   tfs)))))
     org-files)
  (org-test-run-tests)))



;;; Commands:

(defun org-test-test-current-defun ()
  "Execute all tests for function at point if tests exist."
  (interactive)
  (ert-delete-all-tests)
  (save-excursion
    (save-match-data
      (end-of-line)
      (beginning-of-defun)
      (when (looking-at "(defun[[:space:]]+\\([^([:space:]]*\\)[[:space:]]*(")
        (let* ((fun (match-string-no-properties 1))
	       (dir (org-test-test-directory-for-file buffer-file-name))
               (tf (or (org-test-test-file-name-for-defun
			dir fun buffer-file-name)
		       (org-test-test-file-name-for-file dir buffer-file-name))))
          (if tf
	      (progn
		(load-file tf)
		(org-test-run-tests
		 (concat "^" fun)))
            (error "No test files found for \"%s\"" fun)))))))

(defun org-test-test-buffer-file (&optional only)
  "Run all tests for current `buffer-file-name' if tests exist.
If ONLY is non-nil, use the `org-test-default-test-file-name'
file only."
  (interactive "P")
  (ert-delete-all-tests)
  (let* ((pref
	  (concat
	   "^"
	   (file-name-sans-extension
	    (file-name-nondirectory buffer-file-name))))
	 (dir (org-test-test-directory-for-file buffer-file-name))
	 (tfs (if only
		  (list
		   (org-test-test-file-name-for-file
		    dir buffer-file-name))
		(directory-files dir t "\\.el$"))))
    (if (car tfs)
	(mapc
	 (lambda (tf)
	   (load-file tf)
	   (org-test-run-tests pref))
	 tfs)
      (error "No %s found for \"%s\""
	     (if only
		 (format "file \"%s\"" org-test-default-test-file-name)
	       "test files")
	     buffer-file-name))))

(defun org-test-edit-buffer-file-tests ()
  "Open the `org-test-default-test-file-name' file for editing.
If the file (and parent directories) do not yet exist,
create them."
  (interactive)
  (save-match-data
    ;; Check, if editing an emacs-lisp file
    (unless
	(string-match "\\.el$" buffer-file-name)
      (error "Not an emacs lisp file: %s" buffer-file-name)))

  (let ((dir (org-test-test-directory-for-file
	      buffer-file-name)))
    (unless dir
      (error "Directory %s not found. Sorry."
	     org-test-default-directory-name))

    (let* ((tf     (concat dir org-test-default-test-file-name))
	  (exists  (file-exists-p tf))
	  (rel     (file-relative-name buffer-file-name dir))
	  (tprefix (file-name-nondirectory
		    (file-name-sans-extension buffer-file-name))))
      (unless (file-directory-p dir)	; FIXME: Ask?
	(make-directory dir t))
      (find-file tf)
      (unless exists
	(insert
	 ";;; " org-test-default-test-file-name " --- Tests for "
	 (replace-regexp-in-string "^\\(?:\\.+/\\)+" "" rel)
	 "\n\n"
	 "\n"
	 ";;; Code:\n"
	 "(require 'org-test)\n"
	 "(unless (fboundp 'org-test-run-all-tests)\n"
	 "  (error \"%s\" \"org-test.el not loaded.  Giving up.\"))\n"
	 "\n"
	 "\n"
	 ";;; Tests\n"
	 "(ert-deftest " tprefix "/example-test ()\n"
	 "  \"Just an example to get you started.\"\n"
	 "  (should t)\n"
	 "  (should-not nil)\n"
	 "  (should-error (error \"errr...\")))\n")))))



(provide 'org-test)
;;; org-test.el ends here
