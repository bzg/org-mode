;;; test-ob.el --- tests for ob.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for Org-mode tests


;;; Code:
(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))

;;; ob-get-src-block-info
(ert-deftest test-org-babel/get-src-block-info-language ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "emacs-lisp" (nth 0 info))))))

(ert-deftest test-org-babel/get-src-block-info-body ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string-match (regexp-quote org-test-file-ob-anchor)
			    (nth 1 info))))))

(ert-deftest test-org-babel/get-src-block-info-tangle ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "no" (cdr (assoc :tangle (nth 2 info))))))))

(ert-deftest test-org-babel/elisp-in-header-arguments ()
  "Test execution of elisp forms in header arguments."
  ;; at the babel.org:elisp-forms-in-header-arguments header
  (org-test-at-id "22d67284-bf14-4cdc-8319-f4bd876829d7"
    (org-babel-next-src-block)
    (let ((info (org-babel-get-src-block-info)))
      (should (= 4 (org-babel-execute-src-block))))))

(ert-deftest test-org-babel/simple-named-code-block ()
  "Test that simple named code blocks can be evaluated."
  (org-test-at-id "0d82b52d-1bb9-4916-816b-2c67c8108dbb"
    (org-babel-next-src-block 1)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/simple-variable-resolution ()
  "Test that simple variable resolution is working."
  (org-test-at-id "f68821bc-7f49-4389-85b5-914791ee3718"
    (org-babel-next-src-block 2)
    (should (= 4 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/multi-line-header-arguments ()
  "Test that multi-line header arguments and can be read."
  (org-test-at-id "b77c8857-6c76-4ea9-8a61-ddc2648d96c4"
    (org-babel-next-src-block)
    (let ((results (org-babel-execute-src-block)))
      (should (equal 'a (cadr (assoc 1 results))))
      (should (equal 'd (cadr (assoc 4 results)))))))

(ert-deftest test-org-babel/sha1-hash ()
  (org-test-at-id "f68821bc-7f49-4389-85b5-914791ee3718"
    (org-babel-next-src-block 2)
    (should (string= "7374bf4f8a18dfcb6f365f93d15f1a0ef42db745"
		     (org-babel-sha1-hash)))))

(ert-deftest test-org-babel/parse-header-args ()
  (org-test-at-id "7eb0dc6e-1c53-4275-88b3-b22f3113b9c3"
    (org-babel-next-src-block)
    (let* ((info (org-babel-get-src-block-info))
	   (params (nth 2 info)))
      (message "%S" params)
      (should (equal "example-lang" (nth 0 info)))
      (should (string= "the body" (org-babel-trim (nth 1 info))))
      (should-not (member '(:session\ \ \ \ ) params))
      (should (equal '(:session) (assoc :session params)))
      (should (equal '(:result-type . output) (assoc :result-type params)))
      (should (equal '(num . 9) (cdr (assoc :var params)))))))

(provide 'test-ob)

;;; test-ob ends here
