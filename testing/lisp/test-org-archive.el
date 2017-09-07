;;; test-org-archive.el --- Test for Org Archive     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jay Kamat

;; Author: Jay Kamat <jaygkamat@gmail.com>

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


(ert-deftest test-org-element/archive-update-status-cookie ()
  "Test archiving properly updating status cookies."
  ;; Test org-archive-subtree with two children.
  (should
   (equal
    "Top [0%]"
    (org-test-with-temp-text-in-file
	"* Top [%]\n** DONE One\n** TODO Two"
      (forward-line)
      (org-archive-subtree)
      (forward-line -1)
      (org-element-property :title (org-element-at-point)))))
  ;; Test org-archive-subtree with one child.
  (should
   (equal
    "Top [100%]"
    (org-test-with-temp-text-in-file "* Top [%]\n** TODO Two"
      (forward-line)
      (org-archive-subtree)
      (forward-line -1)
      (org-element-property :title (org-element-at-point)))))
  ;; Test org-archive-to-archive-sibling with two children.
  (should
   (equal
    "Top [100%]"
    (org-test-with-temp-text "* Top [%]\n<point>** TODO One\n** DONE Two"
      (org-archive-to-archive-sibling)
      (forward-line -1)
      (org-element-property :title (org-element-at-point)))))
  ;; Test org-archive-to-archive-sibling with two children.
  (should
   (equal
    "Top [0%]"
    (org-test-with-temp-text "* Top [%]\n<point>** DONE Two"
      (org-archive-to-archive-sibling)
      (forward-line -1)
      (org-element-property :title (org-element-at-point))))))


(provide 'test-org-archive)
;;; test-org-archive.el ends here
