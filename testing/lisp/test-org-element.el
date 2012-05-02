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

(defun org-test-parse-and-interpret (text)
  "Parse TEXT as Org syntax and interpret it.
Return interpreted string."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (org-element-interpret-data (org-element-parse-buffer))))



;;; Test Parsers

;;;; Comments

(ert-deftest test-org-element/comment-parser ()
  "Test `comment' parsing."
  ;; Regular comment.
  (should
   (equal
    (org-test-with-temp-text "# Comment"
      (org-element-map (org-element-parse-buffer) 'comment 'identity nil t))
    '(comment (:begin 1 :end 10 :value "Comment\n" :post-blank 0))))
  ;; Inline comment.
  (should
   (equal
    (org-test-with-temp-text "#+ Comment"
      (org-element-map (org-element-parse-buffer) 'comment 'identity nil t))
    '(comment (:begin 1 :end 11 :value "Comment\n" :post-blank 0))))
  ;; Preserve indentation.
  (should
   (equal
    (org-test-with-temp-text "#+ No blank\n#+  One blank"
      (org-element-map (org-element-parse-buffer) 'comment 'identity nil t))
    '(comment (:begin 1 :end 26 :value "No blank\n One blank\n" :post-blank 0))))
  ;; Comment with blank lines.
  (should
   (equal
    (org-test-with-temp-text "#+ First part\n#+ \n#+\n#+ Second part"
      (org-element-map (org-element-parse-buffer) 'comment 'identity nil t))
    '(comment
      (:begin 1 :end 36 :value "First part\n\n\nSecond part\n" :post-blank 0)))))


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
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -n -r -k\nText.\n#+END_EXAMPLE"
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


;;;; Export snippets

(ert-deftest test-org-element/export-snippet-parser ()
  "Test export-snippet parsing."
  (should
   (equal
    (org-test-with-temp-text "<back-end@contents>"
      (org-element-map
       (org-element-parse-buffer) 'export-snippet 'identity nil t))
    '(export-snippet
      (:back-end "back-end"
		 :value "contents" :begin 1 :end 20 :post-blank 0)))))


;;;; Fixed width

(ert-deftest test-org-element/fixed-width ()
  "Test fixed-width area parsing."
  ;; Preserve indentation.
  (should
   (equal
    (org-test-with-temp-text ": no blank\n:  one blank"
      (org-element-map (org-element-parse-buffer) 'fixed-width 'identity nil t))
    '(fixed-width
      (:begin 1 :end 24 :value "no blank\n one blank\n" :post-blank 0))))
  ;; Fixed-width with empty lines.
  (should
   (equal
    (org-test-with-temp-text ": first part\n:\n: \n: second part"
      (org-element-map (org-element-parse-buffer) 'fixed-width 'identity nil t))
    '(fixed-width
      (:begin 1 :end 32 :value "first part\n\n\nsecond part\n" :post-blank 0))))
  ;; Parse indented fixed-width markers.
  (should
   (equal
    (org-test-with-temp-text "Text\n  : no blank\n  :  one blank"
      (org-element-map (org-element-parse-buffer) 'fixed-width 'identity nil t))
    '(fixed-width
      (:begin 6 :end 33 :value "no blank\n one blank\n" :post-blank 0))))
  ;; Distinguish fixed-width areas within a list and outside of it.
  (should
   (= 2
      (length
       (org-test-with-temp-text "
- Item
  : fixed-width inside
: fixed-width outside"
	 (org-element-map
	  (org-element-parse-buffer) 'fixed-width 'identity))))))


;;;; Footnotes references

(ert-deftest test-org-element/footnote-reference-parser ()
  "Test footnote-reference parsing."
  ;; 1. Parse a standard reference.
  (org-test-with-temp-text "[fn:label]"
    (should (equal (org-element-footnote-reference-parser)
		   '(footnote-reference
		     (:label "fn:label" :type standard :inline-definition nil
			     :begin 1 :end 11 :post-blank 0)))))
  ;; 2. Parse a normalized reference.
  (org-test-with-temp-text "[1]"
    (should (equal (org-element-footnote-reference-parser)
		   '(footnote-reference
		     (:label "1" :type standard :inline-definition nil
			     :begin 1 :end 4 :post-blank 0)))))
  ;; 3. Parse an inline reference.
  (org-test-with-temp-text "[fn:test:def]"
    (should (equal (org-element-footnote-reference-parser)
		   '(footnote-reference
		     (:label "fn:test" :type inline :inline-definition ("def")
			     :begin 1 :end 14 :post-blank 0)))))
  ;; 4. Parse an anonymous reference.
  (org-test-with-temp-text "[fn::def]"
    (should (equal (org-element-footnote-reference-parser)
		   '(footnote-reference
		     (:label nil :type inline :inline-definition ("def")
			     :begin 1 :end 10 :post-blank 0)))))
  ;; 5. Parse nested footnotes.
  (org-test-with-temp-text "[fn::def [fn:label]]"
    (should
     (equal
      (org-element-footnote-reference-parser)
      '(footnote-reference
	(:label nil :type inline
		:inline-definition
		("def "
		 (footnote-reference
		  (:label "fn:label" :type standard :inline-definition nil
			  :begin 5 :end 15 :post-blank 0)))
		:begin 1 :end 21 :post-blank 0)))))
  ;; 6. Parse adjacent footnotes.
  (org-test-with-temp-text "[fn:label1][fn:label2]"
    (should
     (equal
      (org-element-footnote-reference-parser)
      '(footnote-reference
	(:label "fn:label1" :type standard :inline-definition nil :begin 1
		:end 12 :post-blank 0)))))
  ;; 7. Only properly closed footnotes are recognized as such.
  (org-test-with-temp-text "Text [fn:label"
    (should-not
     (org-element-map
      (org-element-parse-buffer) 'footnote-reference 'identity))))


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
	(should (equal (org-element-property :tags headline) '("test")))))))


;;;; Links

(ert-deftest test-org-element/link-parser ()
  "Test link parsing."
  ;; 1. Radio target.
  (should
   (equal (org-test-with-temp-text "A radio link"
	    (org-element-map
	     (let ((org-target-link-regexp "radio")) (org-element-parse-buffer))
	     'link 'identity nil t))
	  '(link (:type "radio" :path "radio" :raw-link "radio" :begin 3 :end 9
			:contents-begin nil :contents-end nil :post-blank 1))))
  ;; 2. Standard link.
  ;;
  ;; 2.1. With description.
  (should
   (equal (org-test-with-temp-text "[[http://orgmode.org][Orgmode.org]]"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "http" :path "//orgmode.org"
			:raw-link "http://orgmode.org" :begin 1 :end 36
			:contents-begin 23 :contents-end 34 :post-blank 0)
		 "Orgmode.org")))
  ;; 2.2. Without description.
  (should
   (equal (org-test-with-temp-text "[[http://orgmode.org]]"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "http" :path "//orgmode.org"
			:raw-link "http://orgmode.org" :begin 1 :end 23
			:contents-begin nil :contents-end nil :post-blank 0))))
  ;; 2.3. With expansion.
  (should
   (equal (org-test-with-temp-text "[[Org:worg]]"
	    (let ((org-link-abbrev-alist '(("Org" . "http://orgmode.org/"))))
	      (org-element-map
	       (org-element-parse-buffer) 'link 'identity nil t)))
	  '(link (:type "http" :path "//orgmode.org/worg" :raw-link "Org:worg"
			:begin 1 :end 13 :contents-begin nil :contents-end nil
			:post-blank 0))))
  ;; 2.4. With translation.
  (should
   (equal (org-test-with-temp-text "[[http://orgmode.org]]"
	    (flet ((link-translate (type path) (cons type "127.0.0.1")))
	      (let ((org-link-translation-function 'link-translate))
		(org-element-map
		 (org-element-parse-buffer) 'link 'identity nil t))))
	  '(link (:type "http" :path "127.0.0.1" :raw-link "http://orgmode.org"
			:begin 1 :end 23 :contents-begin nil :contents-end nil
			:post-blank 0))))
  ;; 2.5. Id link.
  (should
   (equal (org-test-with-temp-text "[[id:aaaa]]"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "id" :path "aaaa" :raw-link "id:aaaa" :begin 1 :end 12
			:contents-begin nil :contents-end nil :post-blank 0))))
  ;; 2.6. Custom-id link.
  (should
   (equal (org-test-with-temp-text "[[#some-id]]"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "custom-id" :path "some-id" :raw-link "#some-id"
			:begin 1 :end 13 :contents-begin nil :contents-end nil
			:post-blank 0))))
  ;; 2.7 Coderef link.
  (should
   (equal (org-test-with-temp-text "[[(reference)]]"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "coderef" :path "reference" :raw-link "(reference)"
			:begin 1 :end 16 :contents-begin nil :contents-end nil
			:post-blank 0))))
  ;; 2.8 Fuzzy link.
  (should
   (equal (org-test-with-temp-text "[[target-or-title]]"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "fuzzy" :path "target-or-title"
			:raw-link "target-or-title" :begin 1 :end 20
			:contents-begin nil :contents-end nil :post-blank 0))))
  ;; 3. Plain link.
  (should
   (equal (org-test-with-temp-text "A link: http://orgmode.org"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "http" :path "//orgmode.org"
			:raw-link "http://orgmode.org" :begin 9 :end 27
			:contents-begin nil :contents-end nil :post-blank 0))))
  ;; 4. Angular link.
  (should
   (equal (org-test-with-temp-text "A link: <http://orgmode.org>"
	    (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
	  '(link (:type "http" :path "//orgmode.org"
			:raw-link "http://orgmode.org" :begin 9 :end 29
			:contents-begin nil :contents-end nil :post-blank 0)))))

;;;; Verse blocks

(ert-deftest test-org-element/verse-block-parser ()
  "Test verse block parsing."
  ;; Standard test.
  (org-test-with-temp-text "#+BEGIN_VERSE\nVerse block\n#+END_VERSE"
    (should
     (equal
      (org-element-map (org-element-parse-buffer) 'verse-block 'identity nil t)
      '(verse-block
	(:begin 1 :end 38 :contents-begin 15 :contents-end 27 :hiddenp nil
		:post-blank 0)
	"Verse block\n"))))
  ;; Ignore case.
  (org-test-with-temp-text "#+begin_verse\nVerse block\n#+end_verse"
    (should
     (equal
      (org-element-map (org-element-parse-buffer) 'verse-block 'identity nil t)
      '(verse-block
	(:begin 1 :end 38 :contents-begin 15 :contents-end 27 :hiddenp nil
		:post-blank 0)
	"Verse block\n"))))
  ;; Parse folding.
  (org-test-with-temp-text "#+BEGIN_VERSE\nVerse block\n#+END_VERSE"
    (org-hide-block-all)
    (should
     (equal
      (org-element-map (org-element-parse-buffer) 'verse-block 'identity nil t)
      '(verse-block
	(:begin 1 :end 38 :contents-begin 15 :contents-end 27
		:hiddenp org-hide-block :post-blank 0)
	"Verse block\n"))))
  ;; Parse objects in verse blocks.
  (org-test-with-temp-text "#+BEGIN_VERSE\nVerse \\alpha\n#+END_VERSE"
    (should (org-element-map (org-element-parse-buffer) 'entity 'identity))))



;;; Test Interpreters.

(ert-deftest test-org-element/interpret-affiliated-keywords ()
  "Test if affiliated keywords are correctly interpreted."
  ;; Interpret simple keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:name "para") "Paragraph")))
    "#+NAME: para\nParagraph\n"))
  ;; Interpret multiple keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:attr_ascii ("line1" "line2")) "Paragraph")))
    "#+ATTR_ASCII: line1\n#+ATTR_ASCII: line2\nParagraph\n"))
  ;; Interpret parsed keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:caption ("caption")) "Paragraph")))
    "#+CAPTION: caption\nParagraph\n"))
  ;; Interpret dual keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:caption (("long") "short")) "Paragraph")))
    "#+CAPTION[short]: long\nParagraph\n")))

(ert-deftest test-org-element/center-block-interpreter ()
  "Test center block interpreter."
  (should
   (equal (org-test-parse-and-interpret "#+BEGIN_CENTER\nTest\n#+END_CENTER")
	  "#+BEGIN_CENTER\nTest\n#+END_CENTER\n")))

(ert-deftest test-org-element/drawer-interpreter ()
  "Test drawer interpreter."
  (should
   (equal (let ((org-drawers '("TEST")))
	    (org-test-parse-and-interpret ":TEST:\nTest\n:END:"))
	  ":TEST:\nTest\n:END:\n")))

(ert-deftest test-org-element/dynamic-block-interpreter ()
  "Test dynamic block interpreter."
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN: myblock :parameter value1\nTest\n#+END:")
	  "#+BEGIN: myblock :parameter value1\nTest\n#+END:\n")))

(ert-deftest test-org-element/footnote-definition-interpreter ()
  "Test footnote definition interpreter."
  (should (equal (org-test-parse-and-interpret "[fn:1] Test") "[fn:1] Test\n")))

(ert-deftest test-org-element/headline-interpreter ()
  "Test headline and section interpreters."
  ;; 1. Standard test.
  (should (equal (org-test-parse-and-interpret "* Headline") "* Headline\n"))
  ;; 2. With TODO keywords.
  (should
   (equal (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	    (org-test-parse-and-interpret "* TODO Headline"))
	  "* TODO Headline\n"))
  ;; 3. With tags...
  ;;
  ;; 3.1. ... and a positive `org-tags-column' value.
  (should
   (equal (let ((org-tags-column 20))
	    (org-test-parse-and-interpret "* Headline :tag:"))
	  "* Headline          :tag:\n"))
  ;; 3.2. ... and a negative `org-tags-column' value.
  (should
   (equal (let ((org-tags-column -20))
	    (org-test-parse-and-interpret "* Headline :tag:"))
	  "* Headline     :tag:\n"))
  ;; 3.3. ... and a null `org-tags-column' value.
  (should
   (equal (let ((org-tags-column 0))
	    (org-test-parse-and-interpret "* Headline     :tag:"))
	  "* Headline :tag:\n"))
  ;; 4. With priority cookie.
  (should
   (equal (org-test-parse-and-interpret "* [#B] Headline")
	  "* [#B] Headline\n"))
  ;; 5. With comment keyword.
  (should
   (equal (let ((org-comment-string "COMMENT"))
	    (org-test-parse-and-interpret "* COMMENT Headline"))
	  "* COMMENT Headline\n"))
  ;; 6. With quote section.
  (should
   (equal (let ((org-quote-string "QUOTE"))
	    (org-test-parse-and-interpret "* QUOTE Headline"))
	  "* QUOTE Headline\n"))
  ;; 7. Keep same number of blank lines before body.
  (should
   (equal (org-test-parse-and-interpret
	   "* Headline\n\n\nText after two blank lines.")
	  "* Headline\n\n\nText after two blank lines.\n")))

(ert-deftest test-org-element/inlinetask-interpreter ()
  "Test inlinetask interpretation."
  (when (featurep 'org-inlinetask)
    (let ((org-inlinetask-min-level 15))
      ;; 1. Regular inlinetask.
     (should (equal (org-test-parse-and-interpret
		     "*************** Task\nTest\n*************** END")
		    "*************** Task\nTest\n*************** END\n"))
     ;; 2. Degenerate inlinetask.
     (should (equal (org-test-parse-and-interpret "*************** Task")
		    "*************** Task\n"))
     ;; 3. Prefer degenerate form when there are no contents.
     (should (equal (org-test-parse-and-interpret
		     "*************** Task\n*************** END")
		    "*************** Task\n"))
     ;; 4. With TODO keywords.
     (should
      (equal (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	       (org-test-parse-and-interpret "*************** TODO Task"))
	     "*************** TODO Task\n"))
     ;; 5. With tags...
     ;;
     ;; 5.1. ... and a positive `org-tags-column' value.
     (should
      (equal (let ((org-tags-column 30))
	       (org-test-parse-and-interpret "*************** Task :tag:"))
	     "*************** Task          :tag:\n"))
     ;; 5.2. ... and a negative `org-tags-column' value.
     (should
      (equal (let ((org-tags-column -30))
	       (org-test-parse-and-interpret "*************** Task :tag:"))
	     "*************** Task     :tag:\n"))
     ;; 5.3. ... and a null `org-tags-column' value.
     (should
      (equal (let ((org-tags-column 0))
	       (org-test-parse-and-interpret "*************** Task     :tag:"))
	     "*************** Task :tag:\n"))
     ;; 6. With priority cookie.
     (should
      (equal (org-test-parse-and-interpret "*************** [#B] Task")
	     "*************** [#B] Task\n")))))

(ert-deftest test-org-element/plain-list-interpreter ()
  "Test plain-list and item interpreters."
  ;; 1. Unordered list.
  (should (equal (org-test-parse-and-interpret "- item 1") "- item 1\n"))
  ;; 2. Description list.
  (should
   (equal (org-test-parse-and-interpret "- tag :: desc") "- tag :: desc\n"))
  ;; 3. Ordered list.
  (should
   (equal (let ((org-plain-list-ordered-item-terminator t))
	    (org-test-parse-and-interpret "1. Item"))
	  "1. Item\n"))
  ;; 4. Ordered list with counter.
  (should
   (equal (let ((org-plain-list-ordered-item-terminator t))
	    (org-test-parse-and-interpret "1. [@5] Item"))
	  "5. [@5] Item\n"))
  ;; 5. List with check-boxes.
  (should
   (equal (org-test-parse-and-interpret
	   "- [-] Item 1\n  - [X] Item 2\n  - [ ] Item 3")
	  "- [-] Item 1\n  - [X] Item 2\n  - [ ] Item 3\n")))

(ert-deftest test-org-element/quote-block-interpreter ()
  "Test quote block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_QUOTE\nTest\n#+END_QUOTE")
		 "#+BEGIN_QUOTE\nTest\n#+END_QUOTE\n")))

(ert-deftest test-org-element/special-block-interpreter ()
  "Test special block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_SPECIAL\nTest\n#+END_SPECIAL")
		 "#+BEGIN_SPECIAL\nTest\n#+END_SPECIAL\n")))

(ert-deftest test-org-element/babel-call-interpreter ()
  "Test babel call interpreter."
  ;; 1. Without argument.
  (should (equal (org-test-parse-and-interpret "#+CALL: test()")
		 "#+CALL: test()\n"))
  ;; 2. With argument.
  (should (equal (org-test-parse-and-interpret "#+CALL: test(x=2)")
		 "#+CALL: test(x=2)\n"))
  ;; 3. With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "#+CALL: test[:results output]()[:results html]")
		 "#+CALL: test[:results output]()[:results html]\n")))

(ert-deftest test-org-element/clock-interpreter ()
  "Test clock interpreter."
  ;; Running clock.
  (should
   (equal (let ((org-clock-string "CLOCK:"))
	    (org-test-parse-and-interpret "CLOCK: [2012-01-01 sun. 00:01]"))
	  "CLOCK: [2012-01-01 sun. 00:01]\n"))
  ;; Closed clock.
  (should
   (equal
    (let ((org-clock-string "CLOCK:"))
      (org-test-parse-and-interpret "
CLOCK: [2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02] =>  0:01"))
    "CLOCK: [2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02] =>  0:01\n")))

(ert-deftest test-org-element/comment-interpreter ()
  "Test comment interpreter."
  ;; Regular comment.
  (should (equal (org-test-parse-and-interpret "#Comment") "#+ Comment\n"))
  ;; Inline comment.
  (should (equal (org-test-parse-and-interpret "  #+ Comment")
		 "#+ Comment\n"))
  ;; Preserve indentation.
  (should (equal (org-test-parse-and-interpret "  #+ No blank\n#+  One blank")
		 "#+ No blank\n#+  One blank\n")))

(ert-deftest test-org-element/comment-block-interpreter ()
  "Test comment block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_COMMENT\nTest\n#+END_COMMENT")
		 "#+BEGIN_COMMENT\nTest\n#+END_COMMENT\n")))

(ert-deftest test-org-element/example-block-interpreter ()
  "Test example block interpreter."
  ;; Without switches.
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE")
		 "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE\n"))
  ;; With switches.
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN_EXAMPLE -n -k\n(+ 1 1)\n#+END_EXAMPLE")
	  "#+BEGIN_EXAMPLE -n -k\n(+ 1 1)\n#+END_EXAMPLE\n")))

(ert-deftest test-org-element/export-block-interpreter ()
  "Test export block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_HTML\nTest\n#+END_HTML")
		 "#+BEGIN_HTML\nTest\n#+END_HTML\n")))

(ert-deftest test-org-element/fixed-width-interpreter ()
  "Test fixed width interpreter."
  ;; Standard test.
  (should (equal (org-test-parse-and-interpret ": Test") ": Test\n"))
  ;; Preserve indentation.
  (should (equal (org-test-parse-and-interpret ":  2 blanks\n: 1 blank")
		 ":  2 blanks\n: 1 blank\n")))

(ert-deftest test-org-element/horizontal-rule-interpreter ()
  "Test horizontal rule interpreter."
  (should (equal (org-test-parse-and-interpret "-------") "-----\n")))

(ert-deftest test-org-element/keyword-interpreter ()
  "Test keyword interpreter."
  (should (equal (org-test-parse-and-interpret "#+KEYWORD: value")
		 "#+KEYWORD: value\n")))

(ert-deftest test-org-element/latex-environment-interpreter ()
  "Test latex environment interpreter."
  (should (equal (org-test-parse-and-interpret
		  "\\begin{equation}\n1+1=2\n\\end{equation}")
		 "\\begin{equation}\n1+1=2\n\\end{equation}\n")))

(ert-deftest test-org-element/planning-interpreter ()
  "Test planning interpreter."
  (let ((org-closed-string "CLOSED:")
	(org-deadline-string "DEADLINE:")
	(org-scheduled-string "SCHEDULED:"))
    (should
     (equal
      (org-test-parse-and-interpret
       "* Headline
CLOSED: <2012-01-01> DEADLINE: <2012-01-01> SCHEDULED: <2012-01-01>")
      "* Headline
CLOSED: <2012-01-01> DEADLINE: <2012-01-01> SCHEDULED: <2012-01-01>\n"))))

(ert-deftest test-org-element/property-drawer-interpreter ()
  "Test property drawer interpreter."
  (should (equal (let ((org-property-format "%-10s %s"))
		   (org-test-parse-and-interpret
		    ":PROPERTIES:\n:prop: value\n:END:"))
		 ":PROPERTIES:\n:prop:     value\n:END:\n")))

(ert-deftest test-org-element/src-block-interpreter ()
  "Test src block interpreter."
  ;; With arguments.
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN_SRC emacs-lisp :results silent\n(+ 1 1)\n#+END_SRC")
	  "#+BEGIN_SRC emacs-lisp :results silent\n(+ 1 1)\n#+END_SRC\n"))
  ;; With switches.
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN_SRC emacs-lisp -n -k\n(+ 1 1)\n#+END_SRC")
	  "#+BEGIN_SRC emacs-lisp -n -k\n(+ 1 1)\n#+END_SRC\n")))

(ert-deftest test-org-element/table-interpreter ()
  "Test table, table-row and table-cell interpreters."
  ;; 1. Simple table.
  (should (equal (org-test-parse-and-interpret "| a | b |\n| c | d |")
		 "| a | b |\n| c | d |\n"))
  ;; 2. Table with horizontal rules.
  (should (equal (org-test-parse-and-interpret
		  "| a | b |\n|---+---|\n| c | d |")
		 "| a | b |\n|---+---|\n| c | d |\n"))
  ;; 3. Table with meta-data.
  (should (equal (org-test-parse-and-interpret "| / | < | > |\n| * | 1 | 2 |")
		 "| / | < | > |\n| * | 1 | 2 |\n")))

(ert-deftest test-org-element/verse-block-interpreter ()
  "Test verse block interpretation."
  (should
   (equal (org-test-parse-and-interpret "#+BEGIN_VERSE\nTest\n#+END_VERSE")
	  "#+BEGIN_VERSE\nTest\n#+END_VERSE\n")))

(ert-deftest test-org-element/bold-interpreter ()
  "Test bold interpreter."
  (should (equal (org-test-parse-and-interpret "*text*") "*text*\n")))

(ert-deftest test-org-element/code-interpreter ()
  "Test code interpreter."
  (should (equal (org-test-parse-and-interpret "~text~") "~text~\n")))

(ert-deftest test-org-element/entity-interpreter ()
  "Test entity interpreter."
  ;; 1. Without brackets.
  (should
   (equal (org-test-parse-and-interpret "\\alpha text") "\\alpha text\n"))
  ;; 2. With brackets.
  (should
   (equal (org-test-parse-and-interpret "\\alpha{}text") "\\alpha{}text\n")))

(ert-deftest test-org-element/export-snippet-interpreter ()
  "Test export snippet interpreter."
  (should (equal (org-test-parse-and-interpret "<back-end@contents>")
		 "<back-end@contents>\n")))

(ert-deftest test-org-element/footnote-reference-interpreter ()
  "Test footnote reference interpreter."
  ;; 1. Regular reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:1]") "Text[fn:1]\n"))
  ;; 2. Normalized reference.
  (should (equal (org-test-parse-and-interpret "Text[1]") "Text[1]\n"))
  ;; 3. Named reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:label]")
		 "Text[fn:label]\n"))
  ;; 4. Inline reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:label:def]")
		 "Text[fn:label:def]\n"))
  ;; 5. Anonymous reference.
  (should (equal (org-test-parse-and-interpret "Text[fn::def]")
		 "Text[fn::def]\n")))

(ert-deftest test-org-element/inline-babel-call-interpreter ()
  "Test inline babel call interpreter."
  ;; 1. Without arguments.
  (should (equal (org-test-parse-and-interpret "call_test()") "call_test()\n"))
  ;; 2. With arguments.
  (should (equal (org-test-parse-and-interpret "call_test(x=2)")
		 "call_test(x=2)\n"))
  ;; 3. With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "call_test[:results output]()[:results html]")
		 "call_test[:results output]()[:results html]\n")))

(ert-deftest test-org-element/inline-src-block-interpreter ()
  "Test inline src block interpreter."
  ;; 1. Without header argument.
  (should (equal (org-test-parse-and-interpret "src_emacs-lisp{(+ 1 1)}")
		 "src_emacs-lisp{(+ 1 1)}\n"))
  ;; 2. With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "src_emacs-lisp[:results silent]{(+ 1 1)}")
		 "src_emacs-lisp[:results silent]{(+ 1 1)}\n")))

(ert-deftest test-org-element/italic-interpreter ()
  "Test italic interpreter."
  (should (equal (org-test-parse-and-interpret "/text/") "/text/\n")))

(ert-deftest test-org-element/latex-fragment-interpreter ()
  "Test latex fragment interpreter."
  (let ((org-latex-regexps
	 '(("begin" "^[ 	]*\\(\\\\begin{\\([a-zA-Z0-9\\*]+\\)[^ ]+?\\\\end{\\2}\\)" 1 t)
	   ("$1" "\\([^$]\\|^\\)\\(\\$[^ 	\n,;.$]\\$\\)\\([- 	.,?;:'\") ]\\|$\\)" 2 nil)
	   ("$" "\\([^$]\\|^\\)\\(\\(\\$\\([^ 	\n,;.$][^$\n]*?\\(\n[^$\n]*?\\)\\{0,2\\}[^ 	\n,.$]\\)\\$\\)\\)\\([- 	.,?;:'\") ]\\|$\\)" 2 nil)
	   ("\\(" "\\\\([^ ]*?\\\\)" 0 nil)
	   ("\\[" "\\\\\\[[^ ]*?\\\\\\]" 0 nil)
	   ("$$" "\\$\\$[^ ]*?\\$\\$" 0 nil))))
    (should (equal (org-test-parse-and-interpret "\\command{}")
		   "\\command{}\n"))
    (should (equal (org-test-parse-and-interpret "$x$") "$x$\n"))
    (should (equal (org-test-parse-and-interpret "$x+y$") "$x+y$\n"))
    (should (equal (org-test-parse-and-interpret "$$x+y$$") "$$x+y$$\n"))
    (should (equal (org-test-parse-and-interpret "\\(x+y\\)") "\\(x+y\\)\n"))
    (should (equal (org-test-parse-and-interpret "\\[x+y\\]") "\\[x+y\\]\n"))))

(ert-deftest test-org-element/line-break-interpreter ()
  "Test line break interpreter."
  (should (equal (org-test-parse-and-interpret "First line \\\\ \nSecond line")
		 "First line \\\\\nSecond line\n")))

(ert-deftest test-org-element/link-interpreter ()
  "Test link interpreter."
  ;; 1. Links targeted from a radio target.
  (should (equal (let ((org-target-link-regexp "radio-target"))
		   (org-test-parse-and-interpret "a radio-target"))
		 "a radio-target\n"))
  ;; 2. Regular links.
  ;;
  ;; 2.1. Without description.
  (should (equal (org-test-parse-and-interpret "[[http://orgmode.org]]")
		 "[[http://orgmode.org]]\n"))
  ;; 2.2. With a description.
  (should (equal (org-test-parse-and-interpret
		  "[[http://orgmode.org][Org mode]]")
		 "[[http://orgmode.org][Org mode]]\n"))
  ;; 2.3. Id links.
  (should (equal (org-test-parse-and-interpret "[[id:aaaa]]") "[[id:aaaa]]\n"))
  ;; 2.4. Custom-id links.
  (should (equal (org-test-parse-and-interpret "[[#id]]") "[[#id]]\n"))
  ;; 2.5 Code-ref links.
  (should (equal (org-test-parse-and-interpret "[[(ref)]]") "[[(ref)]]\n"))
  ;; 3. Normalize plain links.
  (should (equal (org-test-parse-and-interpret "http://orgmode.org")
		 "[[http://orgmode.org]]\n"))
  ;; 4. Normalize angular links.
  (should (equal (org-test-parse-and-interpret "<http://orgmode.org>")
		 "[[http://orgmode.org]]\n")))

(ert-deftest test-org-element/macro-interpreter ()
  "Test macro interpreter."
  ;; 1. Without argument.
  (should (equal (org-test-parse-and-interpret "{{{test}}}") "{{{test}}}\n"))
  ;; 2. With arguments.
  (should (equal (org-test-parse-and-interpret "{{{test(arg1,arg2)}}}")
		 "{{{test(arg1,arg2)}}}\n")))

(ert-deftest test-org-element/radio-target-interpreter ()
  "Test radio target interpreter."
  (should (equal (org-test-parse-and-interpret "<<<some text>>>")
		 "<<<some text>>>\n")))

(ert-deftest test-org-element/statistics-cookie-interpreter ()
  "Test statistics cookie interpreter."
  ;; 1. Without percent
  (should (equal (org-test-parse-and-interpret "[0/1]") "[0/1]\n"))
  ;; 2. With percent.
  (should (equal (org-test-parse-and-interpret "[66%]") "[66%]\n")))

(ert-deftest test-org-element/strike-through-interpreter ()
  "Test strike through interpreter."
  (should (equal (org-test-parse-and-interpret "+target+") "+target+\n")))

(ert-deftest test-org-element/subscript-interpreter ()
  "Test subscript interpreter."
  ;; 1. Without brackets.
  (should (equal (org-test-parse-and-interpret "a_b") "a_b\n"))
  ;; 2. With brackets.
  (should (equal (org-test-parse-and-interpret "a_{b}") "a_{b}\n")))

(ert-deftest test-org-element/superscript-interpreter ()
  "Test superscript interpreter."
  ;; 1. Without brackets.
  (should (equal (org-test-parse-and-interpret "a^b") "a^b\n"))
  ;; 2. With brackets.
  (should (equal (org-test-parse-and-interpret "a^{b}") "a^{b}\n")))

(ert-deftest test-org-element/target-interpreter ()
  "Test target interpreter."
  (should (equal (org-test-parse-and-interpret "<<target>>") "<<target>>\n")))

(ert-deftest test-org-element/underline-interpreter ()
  "Test underline interpreter."
  (should (equal (org-test-parse-and-interpret "_text_") "_text_\n")))

(ert-deftest test-org-element/verbatim-interpreter ()
  "Test verbatim interpreter."
  (should (equal (org-test-parse-and-interpret "=text=") "=text=\n")))



;;; Test Granularity

(ert-deftest test-org-element/granularity ()
  "Test granularity impact on buffer parsing."
  (org-test-with-temp-text "
* Head 1
** Head 2
#+BEGIN_CENTER
Centered paragraph.
#+END_CENTER
Paragraph \\alpha."
    ;; 1.1. Granularity set to `headline' should parse every headline
    ;;      in buffer, and only them.
    (let ((tree (org-element-parse-buffer 'headline)))
      (should (= 2 (length (org-element-map tree 'headline 'identity))))
      (should-not (org-element-map tree 'paragraph 'identity)))
    ;; 1.2. Granularity set to `greater-element' should not enter
    ;;      greater elements excepted headlines and sections.
    (let ((tree (org-element-parse-buffer 'greater-element)))
      (should (= 1 (length (org-element-map tree 'center-block 'identity))))
      (should (= 1 (length (org-element-map tree 'paragraph 'identity))))
      (should-not (org-element-map tree 'entity 'identity)))
    ;; 1.3. Granularity set to `element' should enter every
    ;;      greater-element.
    (let ((tree (org-element-parse-buffer 'element)))
      (should (= 2 (length (org-element-map tree 'paragraph 'identity))))
      (should-not (org-element-map tree 'entity 'identity)))
    ;; 1.4. Granularity set to `object' can see everything.
    (let ((tree (org-element-parse-buffer 'object)))
      (should (= 1 (length (org-element-map tree 'entity 'identity)))))))

(ert-deftest test-org-element/secondary-string-parsing ()
  "Test if granularity correctly toggles secondary strings parsing."
  ;; 1. With a granularity bigger than `object', no secondary string
  ;;    should be parsed.
  ;;
  ;; 1.1. Test with `headline' type.
  (org-test-with-temp-text "* Headline"
    (let ((headline
	   (org-element-map (org-element-parse-buffer 'headline) 'headline
			    'identity
			    nil
			    'first-match)))
      (should (stringp (org-element-property :title headline)))))
  ;; 1.2. Test with `item' type.
  (org-test-with-temp-text "* Headline\n- tag :: item"
    (let ((item (org-element-map (org-element-parse-buffer 'element)
				 'item
				 'identity
				 nil
				 'first-match)))
      (should (stringp (org-element-property :tag item)))))
  ;; 1.3. Test with `inlinetask' type, if avalaible.
  (when (featurep 'org-inlinetask)
    (let ((org-inlinetask-min-level 15))
      (org-test-with-temp-text "*************** Inlinetask"
	(let ((inlinetask (org-element-map (org-element-parse-buffer 'element)
					   'inlinetask
					   'identity
					   nil
					   'first-match)))
	  (should (stringp (org-element-property :title inlinetask)))))))
  ;; 2. With a default granularity, secondary strings should be
  ;;    parsed.
  (org-test-with-temp-text "* Headline"
    (let ((headline
	   (org-element-map (org-element-parse-buffer) 'headline
			    'identity
			    nil
			    'first-match)))
      (should (listp (org-element-property :title headline)))))
  ;; 3. `org-element-at-point' should never parse a secondary string.
  (org-test-with-temp-text "* Headline"
    (should (stringp (org-element-property :title (org-element-at-point))))))



;;; Test Normalize Contents

(ert-deftest test-org-element/normalize-contents ()
  "Test `org-element-normalize-contents' specifications."
  ;; 1. Remove maximum common indentation from element's contents.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n   Three spaces"))
    '(paragraph nil "Two spaces\n Three spaces")))
  ;; 2. Ignore objects within contents when computing maximum common
  ;;    indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil " One " (emphasis nil "space") "\n  Two spaces"))
    '(paragraph nil "One " (emphasis nil "space") "\n Two spaces")))
  ;; 3. Ignore blank lines.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n\n \n  Two spaces"))
    '(paragraph nil "Two spaces\n\n \nTwo spaces")))
  ;; 4. Recursively enter objects in order to compute common
  ;;    indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces " (bold nil " and\n One space")))
    '(paragraph nil " Two spaces " (bold nil " and\nOne space"))))
  ;; 5. When optional argument is provided, ignore first line
  ;;    indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "No space\n  Two spaces\n   Three spaces") t)
    '(paragraph nil "No space\nTwo spaces\n Three spaces"))))



;;; Test Navigation Tools.

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
  (org-test-with-temp-text "#+NAME: list\n- Item 1"
    (org-element-down)
    (should (looking-at " Item 1")))
  ;; 3. When at a table, move to first row
  (org-test-with-temp-text "#+NAME: table\n| a | b |"
    (org-element-down)
    (should (looking-at " a | b |")))
  ;; 4. Otherwise, move inside the greater element.
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
