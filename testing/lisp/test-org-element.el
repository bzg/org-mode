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

(unless (featurep 'org-element)
  (signal 'missing-test-dependency "org-element"))



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



;;;; Example-blocks and Src-blocks

(ert-deftest test-org-element/block-switches ()
  "Test `example-block' and `src-block' switches parsing."
  (let ((org-coderef-label-format "(ref:%s)"))
    ;; 1. Test "-i" switch.
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should-not (org-element-property :preserve-indent element))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should (org-element-property :preserve-indent element))))
    (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should-not (org-element-property :preserve-indent element))))
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -i\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should (org-element-property :preserve-indent element))))
    ;; 2. "-n -r -k" combination should number lines, retain labels but
    ;;    not use them in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -n -r -k\nText.\N#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (org-element-property :retain-labels element)
		     (not (org-element-property :use-labels element))))))
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -n -r -k\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (org-element-property :retain-labels element)
		     (not (org-element-property :use-labels element))))))
    ;; 3. "-n -r" combination should number-lines remove labels and not
    ;;    use them in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -n -r\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (not (org-element-property :retain-labels element))
		     (not (org-element-property :use-labels element))))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -n -r\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (not (org-element-property :retain-labels element))
		     (not (org-element-property :use-labels element))))))
    ;; 4. "-n" or "+n" should number lines, retain labels and use them
    ;;    in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -n\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (org-element-property :retain-labels element)
		     (org-element-property :use-labels element)))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -n\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (org-element-property :retain-labels element)
		     (org-element-property :use-labels element)))))
    (org-test-with-temp-text "#+BEGIN_EXAMPLE +n\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (org-element-property :retain-labels element)
		     (org-element-property :use-labels element)))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp +n\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should (and (org-element-property :number-lines element)
		     (org-element-property :retain-labels element)
		     (org-element-property :use-labels element)))))
    ;; 5. No switch should not number lines, but retain labels and use
    ;;    them in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should (and (not (org-element-property :number-lines element))
		     (org-element-property :retain-labels element)
		     (org-element-property :use-labels element)))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should (and (not (org-element-property :number-lines element))
		     (org-element-property :retain-labels element)
		     (org-element-property :use-labels element)))))
    ;; 6. "-r" switch only: do not number lines, remove labels, and
    ;;    don't use labels in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -r\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should (and (not (org-element-property :number-lines element))
		     (not (org-element-property :retain-labels element))
		     (not (org-element-property :use-labels element))))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -r\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should (and (not (org-element-property :number-lines element))
		     (not (org-element-property :retain-labels element))
		     (not (org-element-property :use-labels element))))))
    ;; 7. Recognize coderefs with user-defined syntax.
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -l \"[ref:%s]\"\nText [ref:text]\n#+END_EXAMPLE"
      (let ((element (org-element-current-element)))
	(should
	 (equal (org-element-property :label-fmt element) "[ref:%s]"))))
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -l \"[ref:%s]\"\n(+ 1 1) [ref:text]\n#+END_SRC"
      (let ((element (org-element-current-element)))
	(should
	 (equal (org-element-property :label-fmt element) "[ref:%s]"))))))



;;; Navigation tools.

(ert-deftest test-org-element/forward-element ()
  "Test `org-element-forward' specifications."
  ;; 1. At EOB: should error.
  (org-test-with-temp-text "Some text\n"
    (goto-char (point-max))
    (should-error (org-element-forward)))
  ;; 2. Standard move: expected to ignore blank lines.
  (org-test-with-temp-text "First paragraph.\n\n\nSecond paragraph."
    (org-element-forward)
    (should (looking-at "Second paragraph.")))
  ;; 3. Headline tests.
  (org-test-with-temp-text "
* Head 1
** Head 1.1
*** Head 1.1.1
** Head 1.2"
    ;; 3.1. At an headline beginning: move to next headline at the
    ;;      same level.
    (goto-line 3)
    (org-element-forward)
    (should (looking-at "** Head 1.2"))
    ;; 3.2. At an headline beginning: move to parent headline if no
    ;;      headline at the same level.
    (goto-line 3)
    (org-element-forward)
    (should (looking-at "** Head 1.2")))
  ;; 4. Greater element tests.
  (org-test-with-temp-text
      "#+BEGIN_CENTER\nInside.\n#+END_CENTER\n\nOutside."
    ;; 4.1. At a greater element: expected to skip contents.
    (org-element-forward)
    (should (looking-at "Outside."))
    ;; 4.2. At the end of greater element contents: expected to skip
    ;;      to the end of the greater element.
    (goto-line 2)
    (org-element-forward)
    (should (looking-at "Outside.")))
  ;; 5. List tests.
  (org-test-with-temp-text "
- item1

  - sub1

  - sub2

  - sub3

  Inner paragraph.

- item2

Outside."
    ;; 5.1. At list top point: expected to move to the element after
    ;;      the list.
    (goto-line 2)
    (org-element-forward)
    (should (looking-at "Outside."))
    ;; 5.2. Special case: at the first line of a sub-list, but not at
    ;;      beginning of line, move to next item.
    (goto-line 2)
    (forward-char)
    (org-element-forward)
    (should (looking-at "- item2"))
    (goto-line 4)
    (forward-char)
    (org-element-forward)
    (should (looking-at "  - sub2"))
    ;; 5.3 At sub-list beginning: expected to move after the sub-list.
    (goto-line 4)
    (org-element-forward)
    (should (looking-at "  Inner paragraph."))
    ;; 5.4. At sub-list end: expected to move outside the sub-list.
    (goto-line 8)
    (org-element-forward)
    (should (looking-at "  Inner paragraph."))
    ;; 5.5. At an item: expected to move to next item, if any.
    (goto-line 6)
    (org-element-forward)
    (should (looking-at "  - sub3"))))

(ert-deftest test-org-element/backward-element ()
  "Test `org-element-backward' specifications."
  ;; 1. At BOB (modulo some white spaces): should error.
  (org-test-with-temp-text "    \nParagraph."
    (org-skip-whitespace)
    (should-error (org-element-backward)))
  ;; 2. Not at the beginning of an element: move at its beginning.
  (org-test-with-temp-text "Paragraph1.\n\nParagraph2."
    (goto-line 3)
    (end-of-line)
    (org-element-backward)
    (should (looking-at "Paragraph2.")))
  ;; 3. Headline tests.
  (org-test-with-temp-text "
* Head 1
** Head 1.1
*** Head 1.1.1
** Head 1.2"
    ;; 3.1. At an headline beginning: move to previous headline at the
    ;;      same level.
    (goto-line 5)
    (org-element-backward)
    (should (looking-at "** Head 1.1"))
    ;; 3.2. At an headline beginning: move to parent headline if no
    ;;      headline at the same level.
    (goto-line 3)
    (org-element-backward)
    (should (looking-at "* Head 1"))
    ;; 3.3. At the first top-level headline: should error.
    (goto-line 2)
    (should-error (org-element-backward)))
  ;; 4. At beginning of first element inside a greater element:
  ;;    expected to move to greater element's beginning.
  (org-test-with-temp-text "Before.\n#+BEGIN_CENTER\nInside.\n#+END_CENTER."
    (goto-line 3)
    (org-element-backward)
    (should (looking-at "#\\+BEGIN_CENTER")))
  ;; 5. List tests.
  (org-test-with-temp-text "
- item1

  - sub1

  - sub2

  - sub3

  Inner paragraph.

- item2


Outside."
    ;; 5.1. At beginning of sub-list: expected to move to the
    ;;      paragraph before it.
    (goto-line 4)
    (org-element-backward)
    (should (looking-at "item1"))
    ;; 5.2. At an item in a list: expected to move at previous item.
    (goto-line 8)
    (org-element-backward)
    (should (looking-at "  - sub2"))
    (goto-line 12)
    (org-element-backward)
    (should (looking-at "- item1"))
    ;; 5.3. At end of list/sub-list: expected to move to list/sub-list
    ;;      beginning.
    (goto-line 10)
    (org-element-backward)
    (should (looking-at "  - sub1"))
    (goto-line 15)
    (org-element-backward)
    (should (looking-at "- item1"))
    ;; 5.4. At blank-lines before list end: expected to move to top
    ;; item.
    (goto-line 14)
    (org-element-backward)
    (should (looking-at "- item1"))))

(ert-deftest test-org-element/up-element ()
  "Test `org-element-up' specifications."
  ;; 1. At BOB or with no surrounding element: should error.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-element-up)))
  (org-test-with-temp-text "* Head1\n* Head2"
    (goto-line 2)
    (should-error (org-element-up)))
  (org-test-with-temp-text "Paragraph1.\n\nParagraph2."
    (goto-line 3)
    (should-error (org-element-up)))
  ;; 2. At an headline: move to parent headline.
  (org-test-with-temp-text "* Head1\n** Sub-Head1\n** Sub-Head2"
    (goto-line 3)
    (org-element-up)
    (should (looking-at "\\* Head1")))
  ;; 3. Inside a greater element: move to greater element beginning.
  (org-test-with-temp-text
      "Before.\n#+BEGIN_CENTER\nParagraph1\nParagraph2\n#+END_CENTER\n"
    (goto-line 3)
    (org-element-up)
    (should (looking-at "#\\+BEGIN_CENTER")))
  ;; 4. List tests.
  (org-test-with-temp-text "* Top
- item1

  - sub1

  - sub2

    Paragraph within sub2.

- item2"
    ;; 4.1. Within an item: move to the item beginning.
    (goto-line 8)
    (org-element-up)
    (should (looking-at "  - sub2"))
    ;; 4.2. At an item in a sub-list: move to parent item.
    (goto-line 4)
    (org-element-up)
    (should (looking-at "- item1"))
    ;; 4.3. At an item in top list: move to beginning of whole list.
    (goto-line 10)
    (org-element-up)
    (should (looking-at "- item1"))
    ;; 4.4. Special case.  At very top point: should move to parent of
    ;;      list.
    (goto-line 2)
    (org-element-up)
    (should (looking-at "\\* Top"))))

(ert-deftest test-org-element/down-element ()
  "Test `org-element-down' specifications."
  ;; 1. Error when the element hasn't got a recursive type.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-element-down)))
  ;; 2. When at a plain-list, move to first item.
  (org-test-with-temp-text "- Item 1\n  - Item 1.1\n  - Item 2.2"
    (goto-line 2)
    (org-element-down)
    (should (looking-at " - Item 1.1")))
  ;; 3. Otherwise, move inside the greater element.
  (org-test-with-temp-text "#+BEGIN_CENTER\nParagraph.\n#+END_CENTER"
    (org-element-down)
    (should (looking-at "Paragraph"))))

(ert-deftest test-org-element/drag-backward ()
  "Test `org-element-drag-backward' specifications."
  ;; 1. Error when trying to move first element of buffer.
  (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
    (should-error (org-element-drag-backward)))
  ;; 2. Error when trying to swap nested elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nTest.\n#+END_CENTER"
    (forward-line)
    (should-error (org-element-drag-backward)))
  ;; 3. Error when trying to swap an headline element and
  ;;    a non-headline element.
  (org-test-with-temp-text "Test.\n* Head 1"
    (forward-line)
    (should-error (org-element-drag-backward)))
  ;; 4. Otherwise, swap elements, preserving column and blank lines
  ;;    between elements.
  (org-test-with-temp-text "Para1\n\n\nParagraph 2\n\nPara3"
    (search-forward "graph")
    (org-element-drag-backward)
    (should (equal (buffer-string) "Paragraph 2\n\n\nPara1\n\nPara3"))
    (should (looking-at " 2"))))

(ert-deftest test-org-element/drag-forward ()
  "Test `org-element-drag-forward' specifications."
  ;; 1. Error when trying to move first element of buffer.
  (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
    (goto-line 3)
    (should-error (org-element-drag-forward)))
  ;; 2. Error when trying to swap nested elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nTest.\n#+END_CENTER"
    (forward-line)
    (should-error (org-element-drag-forward)))
  ;; 3. Error when trying to swap a non-headline element and an
  ;;    headline.
  (org-test-with-temp-text "Test.\n* Head 1"
    (should-error (org-element-drag-forward)))
  ;; 4. Otherwise, swap elements, preserving column and blank lines
  ;;    between elements.
  (org-test-with-temp-text "Paragraph 1\n\n\nPara2\n\nPara3"
    (search-forward "graph")
    (org-element-drag-forward)
    (should (equal (buffer-string) "Para2\n\n\nParagraph 1\n\nPara3"))
    (should (looking-at " 1"))))


(provide 'test-org-element)
;;; test-org-element.el ends here
