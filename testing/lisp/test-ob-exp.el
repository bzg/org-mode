;;; test-ob-exp.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for Org-mode tests


;;; Code:
(require 'org-test)


;;; Tests
(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers ()
  "Testing export without any headlines in the org-mode file."
  
  (org-test-in-example-file org-test-no-header-example-file-name
    ;; export the file to html
    (org-export-as-html nil)
    ;; should create a .html file
    (should (file-exists-p (concat
			    (file-name-sans-extension
			     org-test-no-header-example-file-name)
			    ".html")))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p
		 (concat org-test-no-header-example-file-name "::")))))

(provide 'test-ob-exp)

;;; test-ob-exp.el ends here
