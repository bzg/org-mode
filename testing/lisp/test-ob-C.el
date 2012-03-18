(require 'ob-C)
 
(ert-deftest ob-C/assert ()
  (should t))

(ert-deftest ob-C/simple-program ()
  "Hello world program."
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-C/integer-var ()
  "Test of an integer variable."
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 2)
    (should (= 12 (org-babel-execute-src-block)))))

(ert-deftest ob-C/two-integer-var ()
  "Test of two input variables"
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 3)
    (should (= 22 (org-babel-execute-src-block)))))

(ert-deftest ob-C/string-var ()
  "Test of a string input variable"
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 4)
    (should (equal "word 4" (org-babel-execute-src-block)))))

(ert-deftest ob-C/preprocessor ()
  "Test of a string variable"
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 5)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-C/table ()
  "Test of a table output"
  :expected-result :failed
  (org-test-at-id "2df1ab83-3fa3-462a-a1f3-3aef6044a874"
    (org-babel-next-src-block)
    (should (equal '((1) (2)) (org-babel-execute-src-block)))))

;;; test-ob-C.el ends here
 
