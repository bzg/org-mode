;;; test-org-element.el --- Tests for org-element.el

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

;;; Commentary:

;;; Code:

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts)
  (require 'org-element))



;;; Tests:


;;;; Headlines

(ert-deftest test-org-element/headline-quote-keyword ()
  "Test QUOTE keyword recognition."
  ;; Reference test.
  (org-test-with-temp-text "* Headline"
    (let ((org-quote-string "QUOTE"))
      (should-not (org-element-property :quotedp (org-element-at-point)))))
  ;; Standard position.
  (org-test-with-temp-text "* QUOTE Headline"
    (let ((org-quote-string "QUOTE"))
      (let ((headline (org-element-at-point)))
	(should (org-element-property :quotedp headline))
	;; Test removal from raw value.
	(should (equal (org-element-property :raw-value headline) "Headline"))))
    ;; Case sensitivity.
    (let ((org-quote-string "Quote"))
      (should-not (org-element-property :quotedp (org-element-at-point)))))
  ;; With another keyword.
  (org-test-with-temp-text "* TODO QUOTE Headline"
    (let ((org-quote-string "QUOTE")
	  (org-todo-keywords '((sequence "TODO" "DONE"))))
      (should (org-element-property :quotedp (org-element-at-point))))))

(ert-deftest test-org-element/headline-comment-keyword ()
  "Test COMMENT keyword recognition."
  ;; Reference test.
  (org-test-with-temp-text "* Headline"
    (let ((org-comment-string "COMMENT"))
      (should-not (org-element-property :commentedp (org-element-at-point)))))
  ;; Standard position.
  (org-test-with-temp-text "* COMMENT Headline"
    (let ((org-comment-string "COMMENT"))
      (let ((headline (org-element-at-point)))
	(should (org-element-property :commentedp headline))
	;; Test removal from raw value.
	(should (equal (org-element-property :raw-value headline) "Headline"))))
    ;; Case sensitivity.
    (let ((org-comment-string "Comment"))
      (should-not (org-element-property :commentedp (org-element-at-point)))))
  ;; With another keyword.
  (org-test-with-temp-text "* TODO COMMENT Headline"
    (let ((org-comment-string "COMMENT")
	  (org-todo-keywords '((sequence "TODO" "DONE"))))
      (should (org-element-property :commentedp (org-element-at-point))))))

(ert-deftest test-org-element/headline-archive-tag ()
  "Test ARCHIVE tag recognition."
  ;; Reference test.
  (org-test-with-temp-text "* Headline"
    (let ((org-archive-tag "ARCHIVE"))
      (should-not (org-element-property :archivedp (org-element-at-point)))))
  ;; Single tag.
  (org-test-with-temp-text "* Headline :ARCHIVE:"
    (let ((org-archive-tag "ARCHIVE"))
      (let ((headline (org-element-at-point)))
	(should (org-element-property :archivedp headline))
	;; Test tag removal.
	(should-not (org-element-property :tags headline))))
    (let ((org-archive-tag "Archive"))
      (should-not (org-element-property :archivedp (org-element-at-point)))))
  ;; Multiple tags.
  (org-test-with-temp-text "* Headline :test:ARCHIVE:"
    (let ((org-archive-tag "ARCHIVE"))
      (let ((headline (org-element-at-point)))
	(should (org-element-property :archivedp headline))
	;; Test tag removal.
	(should (equal (org-element-property :tags headline) ":test:"))))))


(provide 'test-org-element)
;;; test-org-element.el ends here
