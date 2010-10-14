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
(ert-deftest test-org-babel-get-src-block-info-language ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "emacs-lisp" (nth 0 info))))))

(ert-deftest test-org-babel-get-src-block-info-body ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string-match (regexp-quote org-test-file-ob-anchor)
			    (nth 1 info))))))

(ert-deftest test-org-babel-get-src-block-info-tangle ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "no" (cdr (assoc :tangle (nth 2 info))))))))

;;; elisp forms in header arguments
(ert-deftest test-org-babel-elisp-in-header-arguments ()
  ;; at the babel.org:elisp-forms-in-header-arguments header
  (org-test-at-id "22d67284-bf14-4cdc-8319-f4bd876829d7"
    (org-babel-next-src-block)
    (let ((info (org-babel-get-src-block-info)))
      (should (= 4 (org-babel-execute-src-block))))))

(provide 'test-ob)

;;; test-ob ends here