;;; test-ob.el --- tests for ob.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for Org-mode tests


;;; Code:
(require 'org-test)

(ert-deftest test-org-babel-get-src-block-info-language ()
  (org-test-at-marker nil test-org-code-block-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "emacs-lisp" (nth 0 info))))))

(ert-deftest test-org-babel-get-src-block-info-body ()
  (org-test-at-marker nil test-org-code-block-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string-match (regexp-quote test-org-code-block-anchor)
			    (nth 1 info))))))

(ert-deftest test-org-babel-get-src-block-info-tangle ()
  (org-test-at-marker nil test-org-code-block-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "no" (cdr (assoc :tangle (nth 2 info))))))))


(provide 'test-ob)

;;; test-ob ends here