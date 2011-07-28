(require 'ob-awk)

(ert-deftest ob-awk/input-none ()
  "Test with no input file"
  (org-test-at-id "9e998b2a-3581-43fe-b26d-07d3c507b86a"
    (org-babel-next-src-block)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-awk/input-src-block ()
  "Test a code block as an input"
  (org-test-at-id "9e998b2a-3581-43fe-b26d-07d3c507b86a"
    (org-babel-next-src-block 2)
    (should (= 43 (org-babel-execute-src-block)))))

(ert-deftest ob-awk/input-src-block ()
  "Test a code block as an input"
  (org-test-at-id "9e998b2a-3581-43fe-b26d-07d3c507b86a"
    (org-babel-next-src-block 3)
    (should (= 150 (org-babel-execute-src-block)))))
