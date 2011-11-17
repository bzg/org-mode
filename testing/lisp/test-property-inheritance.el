;;; test-ob-R.el --- tests for ob-R.el

;; Copyright (c) 2011 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))

(defmacro test-org-in-property-buffer (&rest body)
  `(with-temp-buffer
     (insert-file-contents (expand-file-name "property-inheritance.org"
					     org-test-example-dir))
     (org-mode)
     ,@body))

(ert-deftest test-org-property-accumulation-top-use ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 1)
   (should (equal 3 (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-top-val ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 2)
   (should (string= "foo=1 bar=2" (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-overwrite-use ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 3)
   (should (= 7 (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-overwrite-val ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 4)
   (should (string= "foo=7" (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-append-use ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 5)
   (should (= 6 (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-append-val ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 6)
   (should (string= "foo=1 bar=2 baz=3" (org-babel-execute-src-block)))))

(provide 'test-ob-R)

;;; test-ob-R.el ends here
