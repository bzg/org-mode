;;; test-ob-table.el

;; Copyright (c) ߚ Eric Schulte
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


;;; Tests

;; TODO Test Broken (wrong-type-argument number-or-marker-p "2.0")
;; (ert-deftest test-ob-table/sbe ()
;;   "Test that `sbe' can be used to call code blocks from inside tables."
;;   (org-test-at-id "6d2ff4ce-4489-4e2a-9c65-e3f71f77d975"
;;     (should (= 2 (sbe take-sqrt (n "4"))))))

(provide 'test-ob-table)

;;; test-ob-table.el ends here
