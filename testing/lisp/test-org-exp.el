;;; test-org-exp.el --- tests for org-exp.el

;; Copyright (c) 2010-2013 Eric Schulte
;; Authors: Eric Schulte

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

;;; Code:
(ert-deftest test-org-exp/stripping-commas ()
  "Test the stripping of commas from within blocks during export."
  (org-test-at-id "76d3a083-67fa-4506-a41d-837cc48158b5"
    ;; don't strip internal commas
    (org-narrow-to-subtree)
    (should (string-match
             ", 2"
             (org-export-as-ascii nil nil 'string)))))

(provide 'test-org-exp)
