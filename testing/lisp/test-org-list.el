;;; test-org-list.el --- Tests for org-list.el

;; Copyright (C) 2012  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

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

(ert-deftest test-org-list/list-ending ()
  "Test if lists end at the right place."
  ;; With two blank lines.
  (org-test-with-temp-text "- item\n\n\n  Text"
    (goto-line 4)
    (should-not (org-in-item-p)))
  ;; With text less indented than top items.
  (org-test-with-temp-text "- item\nText"
    (goto-line 2)
    (should-not (org-in-item-p)))
  ;; Though, blank lines and text indentation is ignored in blocks.
  (org-test-with-temp-text
      "- item\n  #+begin_quote\n\n\nText at column 0\n  #+end_quote\n Text"
    (goto-line 7)
    (should (org-in-item-p))))

(ert-deftest test-org-list/list-navigation ()
  "Test list navigation specifications."
  (org-test-with-temp-text "
- item A
- item B


- item 1
  - item 1.1
  - item 1.2
  - item 1.3
- item 2


- item X
- item Y"
    (let ((org-list-use-circular-motion nil))
      ;; 1. Test `org-next-item'.
      ;;
      ;; 1.1. Should return an error if at last item in
      ;;      a list/sub-list, unless `org-list-use-circular-motion'
      ;;      is non-nil.
      (goto-line 9)
      (should-error (org-next-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-next-item) t)))
      (goto-line 14)
      (should-error (org-next-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-next-item) t)))
      ;; 1.2. Should jump over sub-lists.
      (goto-line 6)
      (org-next-item)
      (should (looking-at "- item 2"))
      ;; 1.3. Shouldn't move to another list.
      (goto-line 3)
      (should-error (org-next-item))
      (should-not (looking-at "- item 1"))
      ;; 1.4. Should move to the list/sub-list first item when
      ;;     `org-list-use-circular-motion' is non-nil.
      (let ((org-list-use-circular-motion t))
	(goto-line 10)
	(org-next-item)
	(should (looking-at "- item 1"))
	(goto-line 9)
	(org-next-item)
	(should (looking-at "  - item 1.1")))
      ;; 2. Test `org-previous-item'.
      ;;
      ;; 2.1. Should return an error if at first item in
      ;;      a list/sub-list, unless `org-list-use-circular-motion is
      ;;      non-nil.
      (goto-line 7)
      (should-error (org-previous-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-previous-item) t)))
      (goto-line 13)
      (should-error (org-previous-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-previous-item) t)))
      ;; 2.2. Should ignore sub-lists.
      (goto-line 10)
      (org-previous-item)
      (should (looking-at "- item 1"))
      ;; 2.3. Shouldn't move to another list.
      (goto-line 6)
      (should-error (org-previous-item))
      (should-not (looking-at "- item B"))
      ;; 2.4. Should move to the list/sub-list last item when
      ;;      `org-list-use-circular-motion' is non-nil.
      (let ((org-list-use-circular-motion t))
	(goto-line 6)
	(org-previous-item)
	(should (looking-at "- item 2"))
	(goto-line 7)
	(org-previous-item)
	(should (looking-at "  - item 1.3"))))))


(provide 'test-org-list)
;;; test-org-list.el ends here
