;;; test-ob.el --- tests for ob.el
(require 'org-test)

(defmacro test-ob-in-code-block (marker &rest body)
  (declare (indent 1))
  `(in-org-example-file
     (goto-char (point-min))
     (re-search-forward (regexp-quote ,marker))
     ,@body))

(ert-deftest test-org-babel-get-src-block-info-language ()
  (test-ob-in-code-block "94839181-184f-4ff4-a72f-94214df6f5ba"
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "emacs-lisp" (nth 0 info))))))

(ert-deftest test-org-babel-get-src-block-info-body ()
  (test-ob-in-code-block "94839181-184f-4ff4-a72f-94214df6f5ba"
    (let ((info (org-babel-get-src-block-info)))
      (should (string-match (regexp-quote "94839181-184f-4ff4-a72f-94214df6f5ba")
			    (nth 1 info))))))

(ert-deftest test-org-babel-get-src-block-info-tangle ()
  (test-ob-in-code-block "94839181-184f-4ff4-a72f-94214df6f5ba"
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "no" (cdr (assoc :tangle (nth 2 info))))))))

(provide 'test-ob)

;;; test-ob ends here