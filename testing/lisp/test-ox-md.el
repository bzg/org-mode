;;; test-ox-md.el --- Tests from ox-md.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>

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

;;; Code:

(require 'ox-md)

(ert-deftest ox-md/footnotes-level ()
  "Test `org-md-toplevel-hlevel' being honored by footnote section."
  (org-test-with-temp-text "
** level 1
   Post starts here. [fn:1]
*** level2
    lorem ipsum
** Footnotes
[fn:1] a footnote 
"
    (let ((org-md-toplevel-hlevel 4)
          (export-buffer "*Test MD Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'md export-buffer)
      (with-current-buffer export-buffer
        (goto-char (point-min))
        (should (search-forward "#### Footnotes"))))))

(ert-deftest ox-md/headline-style ()
  "Test `org-md-headline-style' being honored."
  (dolist (org-md-headline-style '(atx setext mixed))
    (let ((export-buffer "*Test MD Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-test-with-temp-text "#+options: toc:nil h:10
* level 1
** level 2
*** level 3
**** level 4
***** level 5
****** level 6
******* level 7
"
        (org-export-to-buffer 'md export-buffer)
        (with-current-buffer export-buffer
          (goto-char (point-min))
          (pcase org-md-headline-style
            (`atx
             (should (search-forward "# level 1"))
             (should (search-forward "## level 2"))
             (should (search-forward "### level 3"))
             (should (search-forward "#### level 4"))
             (should (search-forward "##### level 5"))
             (should (search-forward "###### level 6"))
             (should (search-forward "1.  level 7")))
            (`setext
             (should (search-forward "level 1\n======="))
             (should (search-forward "level 2\n------"))
             (should (search-forward "1.  level 3"))
             (should (search-forward "1.  level 4"))
             (should (search-forward "1.  level 5"))
             (should (search-forward "1.  level 6"))
             (should (search-forward "1.  level 7")))
            (`mixed
             (should (search-forward "level 1\n======="))
             (should (search-forward "level 2\n------"))
             (should (search-forward "### level 3"))
             (should (search-forward "#### level 4"))
             (should (search-forward "##### level 5"))
             (should (search-forward "###### level 6"))
             (should (search-forward "1.  level 7")))))))))

(ert-deftest ox-md/item ()
  "Test `org-md-item'."
  ;; Align items at column 4.
  ;; Columns >=100 not aligned.
  (org-test-with-temp-text
      (mapconcat
       #'identity
       (cl-loop for n from 1 to 105
                collect (format "%d. item" n))
       "\n")
    (let ((export-buffer "*Test MD Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'md export-buffer)
      (with-current-buffer export-buffer
        (goto-char (point-min))
        (should (search-forward "1.  item"))
        (should (search-forward "10. item"))
        (should (search-forward "101. item"))))))

(provide 'test-ox-md)
;;; test-ox-md.el ends here
