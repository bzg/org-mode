(ert-deftest test-org-exp/stripping-commas ()
  "Test the stripping of commas from within blocks during export."
  (org-test-at-id "76d3a083-67fa-4506-a41d-837cc48158b5"
    ;; don't strip internal commas
    (org-narrow-to-subtree)
    (should (string-match
	     ", 2"
	     (org-export-as-ascii nil nil nil 'string)))))
