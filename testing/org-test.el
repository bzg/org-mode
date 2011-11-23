;;;; org-test.el --- Tests for Org-mode

;; Copyright (c) 2010 Sebastian Rose, Eric Schulte
;; Authors:
;;     Sebastian Rose, Hannover, Germany, sebastian_rose gmx de
;;     Eric Schulte, Santa Fe, New Mexico, USA, schulte.eric gmail com
;;     David Maus, Brunswick, Germany, dmaus ictsoc de

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;; Definition of `special-mode' copied from Emacs23's simple.el to be
;; provide a testing environment for Emacs22.

;;;; Comments:

;; Interactive testing for Org mode.

;; The heart of all this is the commands `org-test-current-defun'.  If
;; called while in a `defun' all ert tests with names matching the
;; name of the function are run.

;;; Test Development
;; For test development purposes a number of navigation and test
;; function construction routines are available as a git submodule
;; (jump.el)
;; Install with...
;; $ git submodule init
;; $ git submodule update


;;;; Code:
(let* ((org-test-dir (expand-file-name
		      (file-name-directory
		       (or load-file-name buffer-file-name))))
       (org-lisp-dir (expand-file-name
		      (concat org-test-dir "../lisp"))))

  (unless (featurep 'org)
    (setq load-path (cons org-lisp-dir load-path))
    (require 'org)
    (require 'org-id)
     (org-babel-do-load-languages
     'org-babel-load-languages '((sh . t) (org . t))))

  (let* ((load-path (cons
		     org-test-dir
		     (cons
		      (expand-file-name "jump" org-test-dir)
		      load-path))))
    (require 'cl)
    (when (= emacs-major-version 22)
      (defvar special-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)
	  (define-key map "q" 'quit-window)
	  (define-key map " " 'scroll-up)
	  (define-key map "\C-?" 'scroll-down)
	  (define-key map "?" 'describe-mode)
	  (define-key map "h" 'describe-mode)
	  (define-key map ">" 'end-of-buffer)
	  (define-key map "<" 'beginning-of-buffer)
	  (define-key map "g" 'revert-buffer)
	  (define-key map "z" 'kill-this-buffer)
	  map))

      (put 'special-mode 'mode-class 'special)
      (define-derived-mode special-mode nil "Special"
	"Parent major mode from which special major modes should inherit."
	(setq buffer-read-only t)))
    (require 'ert)
    (require 'ert-x)
    (when (file-exists-p
	   (expand-file-name "jump/jump.el" org-test-dir))
      (require 'jump)
      (require 'which-func))))

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
(put 'missing-test-dependency
     'error-conditions
     '(error missing-test-dependency))

(defun org-test-for-executable (exe)
  "Throw an error if EXE is not available.
This can be used at the top of code-block-language specific test
files to avoid loading the file on systems without the
executable."
  (unless (reduce
	   (lambda (acc dir)
	     (or acc (file-exists-p (expand-file-name exe dir))))
	   exec-path :initial-value nil)
    (signal 'missing-test-dependency (list exe))))

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
	 (unless (eq major-mode 'org-mode)
	   (org-mode))
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
(def-edebug-spec org-test-at-marker (form form body))

(defmacro org-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org-mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1))
  (let ((inside-text (if (stringp text) text (eval text))))
    `(with-temp-buffer
       (org-mode)
       ,(let ((point (string-match (regexp-quote "<point>") inside-text)))
	  (if point
	      `(progn (insert `(replace-match "" nil nil inside-text))
		      (goto-char ,(match-beginning 0)))
	    `(progn (insert ,inside-text)
		    (goto-char (point-min)))))
       ,@body)))
(def-edebug-spec org-test-with-temp-text (form body))

(defmacro org-test-with-temp-text-in-file (text &rest body)
  "Run body in a temporary file buffer with Org-mode as the active mode."
  (declare (indent 1))
  (let ((file (make-temp-file "org-test"))
	(inside-text (if (stringp text) text (eval text)))
	(results (gensym)))
    `(let ((kill-buffer-query-functions nil) ,results)
       (with-temp-file ,file (insert ,inside-text))
       (find-file ,file)
       (org-mode)
       (setq ,results ,@body)
       (save-buffer) (kill-buffer)
       (delete-file ,file)
       ,results)))
(def-edebug-spec org-test-with-temp-text-in-file (form body))


;;; Navigation Functions
(when (featurep 'jump)
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
    (lambda () ((lambda (res) (if (listp res) (car res) res)) (which-function)))))

(define-key emacs-lisp-mode-map "\M-\C-j" 'org-test-jump)


;;; Miscellaneous helper functions
(defun org-test-strip-text-props (s)
  "Return S without any text properties."
  (let ((noprop (copy-sequence s)))
    (set-text-properties 0 (length noprop) nil noprop)
    noprop))


(defun org-test-string-exact-match (regex string &optional start)
  "case sensative string-match"
  (let ((case-fold-search nil)
        (case-replace nil))
    (if(and (equal regex "")
	    (not(equal string "")))
        nil
      (if (equal 0 (string-match regex string start))
          t
        nil))))

;;; Load and Run tests
(defun org-test-load ()
  "Load up the org-mode test suite."
  (interactive)
  (flet ((rld (base)
	      ;; Recursively load all files, if files throw errors
	      ;; then silently ignore the error and continue to the
	      ;; next file.  This allows files to error out if
	      ;; required executables aren't available.
	      (mapc
	       (lambda (path)
		 (if (file-directory-p path)
		     (rld path)
		   (condition-case err
		       (when (string-match "^[A-Za-z].*\\.el$"
					 (file-name-nondirectory path))
			 (load-file path))
		     (missing-test-dependency
		      (let ((name (intern
				   (concat "org-missing-dependency/"
					   (file-name-nondirectory
					    (file-name-sans-extension path))))))
			(eval `(ert-deftest ,name ()
				 :expected-result :failed (should nil))))))))
	       (directory-files base 'full
				"^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.el$"))))
    (rld (expand-file-name "lisp" org-test-dir))
    (rld (expand-file-name "lisp" (expand-file-name "contrib" org-test-dir)))))

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

(defun org-test-touch-all-examples ()
  (dolist (file (directory-files
		 org-test-example-dir 'full
		 "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$"))
    (find-file file)))

(defun org-test-update-id-locations ()
  (org-id-update-id-locations
   (directory-files
    org-test-example-dir 'full
    "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))

(defun org-test-run-batch-tests ()
  "Run all defined tests matching \"\\(org\\|ob\\)\".
Load all test files first."
  (interactive)
  (let ((org-id-track-globally t)
	(org-id-locations-file
	 (convert-standard-filename
	  (expand-file-name
	   "testing/.test-org-id-locations"
	   org-base-dir))))
    (org-test-touch-all-examples)
    (org-test-update-id-locations)
    (org-test-load)
    (ert-run-tests-batch-and-exit "\\(org\\|ob\\)")))

(defun org-test-run-all-tests ()
  "Run all defined tests matching \"\\(org\\|ob\\)\".
Load all test files first."
  (interactive)
  (org-test-touch-all-examples)
  (org-test-load)
  (ert "\\(org\\|ob\\)"))

(provide 'org-test)

;;; org-test.el ends here
