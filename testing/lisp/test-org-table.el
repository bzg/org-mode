;;; test-org-table.el --- tests for org-table.el

;; Copyright (c)  David Maus
;; Authors: David Maus

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Comments:

;; Template test file for Org-mode tests

;;; Code:
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
  (org-test-with-temp-text-in-file "

* simple formula
  :PROPERTIES:
  :ID:       563523f7-3f3e-49c9-9622-9216cc9a5d95
  :END:

#+tblname: simple-formula
|  1 |
|  2 |
|  3 |
|  4 |
|----|
|    |
  #+TBLFM: $1=vsum(@1..@-1)
"
    (progn    
      (re-search-forward (regexp-quote "#+tblname: simple-formula") nil t)
      (forward-line 1)
      (should (org-at-table-p))
      (should (org-table-recalculate 'all))
      (should (string= "10" (first (nth 5 (org-table-to-lisp))))))))

(provide 'test-org-table)

;;; test-org-table.el ends here
