;;; test-ob-sh.el

;; Copyright (c) 2010-2012 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;; Template test file for Org-mode tests

;;; Code:
(org-test-for-executable "sh")
(unless (featurep 'ob-sh)
  (signal 'missing-test-dependency "Support for Sh code blocks"))

(ert-deftest test-ob-sh/dont-insert-spaces-on-expanded-bodies ()
  "Expanded shell bodies should not start with a blank line
unless the body of the tangled block does."
  (should-not (string-match "^[\n\r][\t ]*[\n\r]"
			    (org-babel-expand-body:generic "echo 2" '())))
  (should (string-match "^[\n\r][\t ]*[\n\r]"
			(org-babel-expand-body:generic "\n\necho 2" '()))))

(ert-deftest test-ob-sh/dont-error-on-empty-results ()
  "Was throwing an elisp error when shell blocks threw errors and
returned empty results."
  (should (null (org-babel-execute:sh "ls NoSuchFileOrDirectory.txt" nil))))

(ert-deftest test-ob-sh/session ()
  "This also tests `org-babel-comint-with-output' in
ob-comint.el, which was not previously tested."
  (let ((res (org-babel-execute:sh "echo 1; echo 2" '((:session . "yes")))))
    (should res)
    (should (listp res))))

(provide 'test-ob-sh)

;;; test-ob-sh.el ends here
