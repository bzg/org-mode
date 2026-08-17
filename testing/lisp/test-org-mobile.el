;;; test-org-mobile.el --- Tests for org-mobile.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Derek Chen-Becker

;; Authors: Derek Chen-Becker <oss@chen-becker.org>

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for Org Mobile library.

;;; Code:

(require 'org-test "../testing/org-test")

(require 'org-mobile)

(ert-deftest test-org-mobile/org-mobile-edit ()
  "Test `org-mobile-edit' functionality."
  (should
   (equal "* [#42] H"
          (let ((org-priority-highest 40)
                (org-priority-lowest 50))
            (org-test-with-temp-text "* [#48] H"
              (org-mobile-edit 'priority "48" "42")
              (buffer-string))))))

(provide 'test-org-mobile)
;;; test-org-mobile.el ends here
