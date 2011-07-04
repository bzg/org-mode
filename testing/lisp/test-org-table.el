;;; test-org-table.el

;; Copyright (c) ߛ David Maus
;; Authors: David Maus

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
(ert-deftest test-org-table/org-table-convert-refs-to-an/1 ()
  "Simple reference @1$1."
  (should
   (string= "A1" (org-table-convert-refs-to-an "@1$1"))))

;; TODO: Test broken
;; (ert-deftest test-org-table/org-table-convert-refs-to-an/2 ()
;;   "Self reference @1$1."
;;   (should
;;    (string= "A1 = $0" (org-table-convert-refs-to-an "@1$1 = $0"))))

(ert-deftest test-org-table/org-table-convert-refs-to-an/3 ()
  "Remote reference."
  (should
   (string= "C& = remote(FOO, @@#B&)" (org-table-convert-refs-to-an "$3 = remote(FOO, @@#$2)"))))

(ert-deftest test-org-table/org-table-convert-refs-to-rc/1 ()
  "Simple reference @1$1."
  (should
   (string= "@1$1" (org-table-convert-refs-to-rc "A1"))))

(ert-deftest test-org-table/org-table-convert-refs-to-rc/2 ()
  "Self reference $0."
  (should
   (string= "@1$1 = $0" (org-table-convert-refs-to-rc "A1 = $0"))))

;; TODO: Test Broken
;; (ert-deftest test-org-table/org-table-convert-refs-to-rc/3 ()
;;   "Remote reference."
;;   (should
;;    (string= "$3 = remote(FOO, @@#$2)" (org-table-convert-refs-to-rc "C& = remote(FOO, @@#B&)"))))

(ert-deftest test-org-table/simple-formula ()
  (org-test-at-id "563523f7-3f3e-49c9-9622-9216cc9a5d95"
    (re-search-forward (regexp-quote "#+tblname: simple-formula") nil t)
    (forward-line 1)
    (should (org-at-table-p))
    (should (org-table-recalculate 'all))
    (should (string= "10" (first (nth 5 (org-table-to-lisp)))))))

(provide 'test-org-table)

;;; test-org-table.el ends here
