;;; test-org-export.el --- Tests for org-export.el

;; Copyright (C) 2012  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments



;;; Code:

(unless (featurep 'org-export)
  (signal 'missing-test-dependency "org-export"))



;;; Tests

(defmacro org-test-with-backend (backend &rest body)
  "Execute body with an export back-end defined.

BACKEND is the name, as a string, of the back-end.  BODY is the
body to execute.  The defined back-end simply returns parsed data
as Org syntax."
  (declare (debug (form body)) (indent 1))
  `(flet ,(let (transcoders)
	    (dolist (type (append org-element-all-elements
				  org-element-all-objects)
			  transcoders)
	      (push `(,(intern (format "org-%s-%s" backend type))
		      (obj contents info)
		      (,(intern (format "org-element-%s-interpreter" type))
		       obj contents))
		    transcoders)))
     ,@body))

(defmacro org-test-with-parsed-data (data &rest body)
  "Execute body with parsed data available.

DATA is a string containing the data to be parsed.  BODY is the
body to execute.  Parse tree is available under the `tree'
variable, and communication channel under `info'.

This function calls `org-export-collect-tree-properties'.  As
such, `:ignore-list' (for `org-element-map') and
`:parse-tree' (for `org-export-get-genealogy') properties are
already filled in `info'."
  (declare (debug (form body)) (indent 1))
  `(org-test-with-temp-text ,data
     (let* ((tree (org-element-parse-buffer))
	    (info (org-export-collect-tree-properties tree nil)))
       ,@body)))

(ert-deftest test-org-export/parse-option-keyword ()
  "Test reading all standard #+OPTIONS: items."
  (should
   (equal
    (org-export-parse-option-keyword
     "H:1 num:t \\n:t timestamp:t arch:t author:t creator:t d:t email:t
 *:t e:t ::t f:t pri:t -:t ^:t toc:t |:t tags:t tasks:t <:t todo:t")
    '(:headline-levels
      1 :preserve-breaks t :section-numbers t :time-stamp-file t
      :with-archived-trees t :with-author t :with-creator t :with-drawers t
      :with-email t :with-emphasize t :with-entities t :with-fixed-width t
      :with-footnotes t :with-priority t :with-special-strings t
      :with-sub-superscript t :with-toc t :with-tables t :with-tags t
      :with-tasks t :with-timestamps t :with-todo-keywords t)))
  ;; Test some special values.
  (should
   (equal
    (org-export-parse-option-keyword
     "arch:headline creator:comment d:(\"TEST\")
 ^:{} toc:1 tags:not-in-toc tasks:todo num:2 <:active")
    '( :section-numbers
       2
       :with-archived-trees headline :with-creator comment
       :with-drawers ("TEST") :with-sub-superscript {} :with-toc 1
       :with-tags not-in-toc :with-tasks todo :with-timestamps active))))

(ert-deftest test-org-export/get-inbuffer-options ()
  "Test reading all standard export keywords."
  (should
   (equal
    (org-test-with-temp-text "#+AUTHOR: Me, Myself and I
#+CREATOR: Idem
#+DATE: Today
#+DESCRIPTION: Testing
#+DESCRIPTION: with two lines
#+EMAIL: some@email.org
#+EXPORT_EXCLUDE_TAGS: noexport invisible
#+KEYWORDS: test
#+LANGUAGE: en
#+EXPORT_SELECT_TAGS: export
#+TITLE: Some title
#+TITLE: with spaces"
      (org-export-get-inbuffer-options))
    '(:author
      ("Me, Myself and I") :creator "Idem" :date "Today"
      :description "Testing\nwith two lines" :email "some@email.org"
      :exclude-tags ("noexport" "invisible") :keywords "test" :language "en"
      :select-tags ("export") :title ("Some title with spaces")))))

(ert-deftest test-org-export/define-macro ()
  "Try defining various Org macro using in-buffer #+MACRO: keyword."
  ;; Parsed macro.
  (should (equal (org-test-with-temp-text "#+MACRO: one 1"
		   (org-export-get-inbuffer-options))
		 '(:macro-one ("1"))))
  ;; Evaled macro.
  (should (equal (org-test-with-temp-text "#+MACRO: two (eval (+ 1 1))"
		   (org-export-get-inbuffer-options))
		 '(:macro-two "(eval (+ 1 1))")))
  ;; Incomplete macro.
  (should-not (org-test-with-temp-text "#+MACRO: three"
		(org-export-get-inbuffer-options)))
  ;; Macro with newline character.
  (should (equal (org-test-with-temp-text "#+MACRO: four a\\nb"
		   (org-export-get-inbuffer-options))
		 '(:macro-four ("a\nb"))))
  ;; Macro with protected newline character.
  (should (equal (org-test-with-temp-text "#+MACRO: five a\\\\nb"
		   (org-export-get-inbuffer-options))
		 '(:macro-five ("a\\nb"))))
  ;; Recursive macro.
  (org-test-with-temp-text "#+MACRO: six 6\n#+MACRO: seven 1 + {{{six}}}"
    (should
     (equal
      (org-export-get-inbuffer-options)
      '(:macro-six
	("6")
	:macro-seven
	("1 + " (macro (:key "six" :value "{{{six}}}" :args nil :begin 5 :end 14
			     :post-blank 0))))))))

(ert-deftest test-org-export/handle-options ()
  "Test if export options have an impact on output."
  ;; Test exclude tags.
  (org-test-with-temp-text "* Head1 :noexport:"
    (org-test-with-backend "test"
      (should
       (equal (org-export-as 'test nil nil nil '(:exclude-tags ("noexport")))
	      ""))))
  ;; Test include tags.
  (org-test-with-temp-text "
* Head1
** Sub-Head1.1 :export:
*** Sub-Head1.1.1
* Head2"
    (org-test-with-backend "test"
      (should
       (string-match
	"\\* Head1\n\\*\\* Sub-Head1.1[ \t]+:export:\n\\*\\*\\* Sub-Head1.1.1\n"
	(org-export-as 'test nil nil nil '(:select-tags ("export")))))))
  ;; Test mixing include tags and exclude tags.
  (org-test-with-temp-text "
* Head1 :export:
** Sub-Head1 :noexport:
** Sub-Head2
* Head2 :noexport:
** Sub-Head1 :export:"
    (org-test-with-backend "test"
      (should
       (string-match
	"\\* Head1[ \t]+:export:\n\\*\\* Sub-Head2\n"
	(org-export-as
	 'test nil nil nil
	 '(:select-tags ("export") :exclude-tags ("noexport")))))))
  ;; Ignore tasks.
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (org-test-with-temp-text "* TODO Head1"
      (org-test-with-backend "test"
	(should (equal (org-export-as 'test nil nil nil '(:with-tasks nil))
		       "")))))
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (org-test-with-temp-text "* TODO Head1"
      (org-test-with-backend "test"
	(should (equal (org-export-as 'test nil nil nil '(:with-tasks t))
		       "* TODO Head1\n")))))
  ;; Archived tree.
  (org-test-with-temp-text "* Head1 :archive:"
    (let ((org-archive-tag "archive"))
      (org-test-with-backend "test"
	(should
	 (equal (org-export-as 'test nil nil nil '(:with-archived-trees nil))
		"")))))
  (org-test-with-temp-text "* Head1 :archive:\nbody\n** Sub-head 2"
    (let ((org-archive-tag "archive"))
      (org-test-with-backend "test"
	(should
	 (string-match
	  "\\* Head1[ \t]+:archive:"
	  (org-export-as 'test nil nil nil
			 '(:with-archived-trees headline)))))))
  (org-test-with-temp-text "* Head1 :archive:"
    (let ((org-archive-tag "archive"))
      (org-test-with-backend "test"
	(should
	 (string-match
	  "\\`\\* Head1[ \t]+:archive:\n\\'"
	  (org-export-as 'test nil nil nil '(:with-archived-trees t)))))))
  ;; Drawers.
  (let ((org-drawers '("TEST")))
    (org-test-with-temp-text ":TEST:\ncontents\n:END:"
      (org-test-with-backend "test"
	(should (equal (org-export-as 'test nil nil nil '(:with-drawers nil))
		       ""))
	(should (equal (org-export-as 'test nil nil nil '(:with-drawers t))
		       ":TEST:\ncontents\n:END:\n")))))
  (let ((org-drawers '("FOO" "BAR")))
    (org-test-with-temp-text ":FOO:\nkeep\n:END:\n:BAR:\nremove\n:END:"
      (org-test-with-backend "test"
	(should
	 (equal (org-export-as 'test nil nil nil '(:with-drawers ("FOO")))
		":FOO:\nkeep\n:END:\n")))))
  ;; Timestamps.
  (org-test-with-temp-text "[2012-04-29 sun. 10:45]<2012-04-29 sun. 10:45>"
    (org-test-with-backend "test"
      (should
       (equal (org-export-as 'test nil nil nil '(:with-timestamps t))
	      "[2012-04-29 sun. 10:45]<2012-04-29 sun. 10:45>\n"))
      (should
       (equal (org-export-as 'test nil nil nil '(:with-timestamps nil)) ""))
      (should
       (equal (org-export-as 'test nil nil nil '(:with-timestamps active))
	      "<2012-04-29 sun. 10:45>\n"))
      (should
       (equal (org-export-as 'test nil nil nil '(:with-timestamps inactive))
	      "[2012-04-29 sun. 10:45]\n"))))
  ;; Clocks.
  (let ((org-clock-string "CLOCK:"))
    (org-test-with-temp-text "CLOCK: [2012-04-29 sun. 10:45]"
      (org-test-with-backend "test"
	(should
	 (equal (org-export-as 'test nil nil nil '(:with-clocks t))
		"CLOCK: [2012-04-29 sun. 10:45]\n"))
	(should
	 (equal (org-export-as 'test nil nil nil '(:with-clocks nil)) "")))))
  ;; Plannings.
  (let ((org-closed-string "CLOSED:"))
    (org-test-with-temp-text "CLOSED: [2012-04-29 sun. 10:45]"
      (org-test-with-backend "test"
	(should
	 (equal (org-export-as 'test nil nil nil '(:with-plannings t))
		"CLOSED: [2012-04-29 sun. 10:45]\n"))
	(should
	 (equal (org-export-as 'test nil nil nil '(:with-plannings nil))
		""))))))

(ert-deftest test-org-export/comment-tree ()
  "Test if export process ignores commented trees."
  (let ((org-comment-string "COMMENT"))
    (org-test-with-temp-text "* COMMENT Head1"
      (org-test-with-backend "test"
	(should (equal (org-export-as 'test) ""))))))

(ert-deftest test-org-export/export-scope ()
  "Test all export scopes."
  (org-test-with-temp-text "
* Head1
** Head2
text
*** Head3"
    (org-test-with-backend "test"
      ;; Subtree.
      (forward-line 3)
      (should (equal (org-export-as 'test 'subtree) "text\n*** Head3\n"))
      ;; Visible.
      (goto-char (point-min))
      (forward-line)
      (org-cycle)
      (should (equal (org-export-as 'test nil 'visible) "* Head1\n"))
      ;; Body only.
      (flet ((org-test-template (body info) (format "BEGIN\n%sEND" body)))
	(should (equal (org-export-as 'test nil nil 'body-only)
		       "* Head1\n** Head2\ntext\n*** Head3\n"))
	(should (equal (org-export-as 'test)
		       "BEGIN\n* Head1\n** Head2\ntext\n*** Head3\nEND")))
      ;; Region.
      (goto-char (point-min))
      (forward-line 3)
      (transient-mark-mode 1)
      (push-mark (point) t t)
      (goto-char (point-at-eol))
      (should (equal (org-export-as 'test) "text\n"))))
  ;; Subtree with a code block calling another block outside.
  (org-test-with-temp-text "
* Head1
#+BEGIN_SRC emacs-lisp :noweb yes :exports results
<<test>>
#+END_SRC
* Head2
#+NAME: test
#+BEGIN_SRC emacs-lisp
\(+ 1 2)
#+END_SRC"
    (org-test-with-backend "test"
      (forward-line 1)
      (should (equal (org-export-as 'test 'subtree) ": 3\n")))))

(ert-deftest test-org-export/export-snippet ()
  "Test export snippets transcoding."
  (org-test-with-temp-text "<test@A><t@B>"
    (org-test-with-backend "test"
      (flet ((org-test-export-snippet
	      (snippet contents info)
	      (when (eq (org-export-snippet-backend snippet) 'test)
		(org-element-property :value snippet))))
	(let ((org-export-snippet-translation-alist nil))
	  (should (equal (org-export-as 'test) "A\n")))
	(let ((org-export-snippet-translation-alist '(("t" . "test"))))
	  (should (equal (org-export-as 'test) "AB\n")))))))

(ert-deftest test-org-export/expand-include ()
  "Test file inclusion in an Org buffer."
  ;; Full insertion with recursive inclusion.
  (org-test-with-temp-text
      (format "#+INCLUDE: \"%s/examples/include.org\"" org-test-dir)
    (org-export-expand-include-keyword)
    (should (equal (buffer-string)
		   "Small Org file with an include keyword.

#+BEGIN_SRC emacs-lisp :exports results\n(+ 2 1)\n#+END_SRC

Success!

* Heading
body\n")))
  ;; Localized insertion.
  (org-test-with-temp-text
      (format "#+INCLUDE: \"%s/examples/include.org\" :lines \"1-2\""
	      org-test-dir)
    (org-export-expand-include-keyword)
    (should (equal (buffer-string)
		   "Small Org file with an include keyword.\n")))
  ;; Insertion with constraints on headlines level.
  (org-test-with-temp-text
      (format
       "* Top heading\n#+INCLUDE: \"%s/examples/include.org\" :lines \"9-\""
       org-test-dir)
    (org-export-expand-include-keyword)
    (should (equal (buffer-string) "* Top heading\n** Heading\nbody\n")))
  ;; Inclusion within an example block.
  (org-test-with-temp-text
      (format "#+INCLUDE: \"%s/examples/include.org\" :lines \"1-2\" example"
	      org-test-dir)
    (org-export-expand-include-keyword)
    (should
     (equal
      (buffer-string)
      "#+BEGIN_EXAMPLE\nSmall Org file with an include keyword.\n#+END_EXAMPLE\n")))
  ;; Inclusion within a src-block.
  (org-test-with-temp-text
      (format
       "#+INCLUDE: \"%s/examples/include.org\" :lines \"4-5\" src emacs-lisp"
       org-test-dir)
    (org-export-expand-include-keyword)
    (should (equal (buffer-string)
		   "#+BEGIN_SRC emacs-lisp\n(+ 2 1)\n#+END_SRC\n"))))

(ert-deftest test-org-export/user-ignore-list ()
  "Test if `:ignore-list' accepts user input."
  (org-test-with-backend "test"
    (flet ((skip-note-head
	    (data backend info)
	    ;; Ignore headlines with the word "note" in their title.
	    (org-element-map
	     data 'headline
	     (lambda (headline)
	       (when (string-match "\\<note\\>"
				   (org-element-property :raw-value headline))
		 (org-export-ignore-element headline info)))
	     info)
	    data))
      ;; Install function in parse tree filters.
      (let ((org-export-filter-parse-tree-functions '(skip-note-head)))
	(org-test-with-temp-text "* Head1\n* Head2 (note)\n"
	  (should (equal (org-export-as 'test) "* Head1\n")))))))

(ert-deftest test-org-export/before-parsing-hook ()
  "Test `org-export-before-parsing-hook'."
  (org-test-with-backend "test"
    (org-test-with-temp-text "* Headline 1\nBody 1\n* Headline 2\nBody 2"
      (let ((org-export-before-parsing-hook
	     '((lambda ()
		 (org-map-entries
		  (lambda ()
		    (delete-region (point) (progn (forward-line) (point)))))))))
	(should (equal (org-export-as 'test) "Body 1\nBody 2\n"))))))



;;; Footnotes

(ert-deftest test-org-export/footnotes ()
  "Test footnotes specifications."
  (let ((org-footnote-section nil))
    ;; 1. Read every type of footnote.
    (org-test-with-temp-text
	"Text[fn:1] [1] [fn:label:C] [fn::D]\n\n[fn:1] A\n\n[1] B"
      (let* ((tree (org-element-parse-buffer))
	     (info (org-export-store-footnote-definitions
		    `(:parse-tree ,tree :with-footnotes t))))
	(should
	 (equal
	  '((1 . "A") (2 . "B") (3 . "C") (4 . "D"))
	  (org-element-map
	   tree 'footnote-reference
	   (lambda (ref)
	     (let ((def (org-export-get-footnote-definition ref info)))
	       (cons (org-export-get-footnote-number ref info)
		     (if (eq (org-element-property :type ref) 'inline) (car def)
		       (car (org-element-contents
			     (car (org-element-contents def))))))))
	   info)))))
    ;; 2. Test nested footnotes order.
    (org-test-with-temp-text
	"Text[fn:1:A[fn:2]] [fn:3].\n\n[fn:2] B [fn:3] [fn::D].\n\n[fn:3] C."
      (let* ((tree (org-element-parse-buffer))
	     (info (org-export-store-footnote-definitions
		    `(:parse-tree ,tree :with-footnotes t))))
	(should
	 (equal
	  '((1 . "fn:1") (2 . "fn:2") (3 . "fn:3") (4))
	  (org-element-map
	   tree 'footnote-reference
	   (lambda (ref)
	     (when (org-export-footnote-first-reference-p ref info)
	       (cons (org-export-get-footnote-number ref info)
		     (org-element-property :label ref))))
	   info)))))
    ;; 3. Test nested footnote in invisible definitions.
    (org-test-with-temp-text "Text[1]\n\n[1] B [2]\n\n[2] C."
      ;; Hide definitions.
      (narrow-to-region (point) (point-at-eol))
      (let* ((tree (org-element-parse-buffer))
	     (info (org-export-store-footnote-definitions
		    `(:parse-tree ,tree :with-footnotes t))))
	;; Both footnotes should be seen.
	(should
	 (= (length (org-export-collect-footnote-definitions tree info)) 2))))
    ;; 4. Test footnotes definitions collection.
    (org-test-with-temp-text "Text[fn:1:A[fn:2]] [fn:3].

\[fn:2] B [fn:3] [fn::D].

\[fn:3] C."
      (let* ((tree (org-element-parse-buffer))
	     (info (org-export-store-footnote-definitions
		    `(:parse-tree ,tree :with-footnotes t))))
	(should (= (length (org-export-collect-footnote-definitions tree info))
		   4))))
    ;; 5. Test export of footnotes defined outside parsing scope.
    (org-test-with-temp-text "[fn:1] Out of scope
* Title
Paragraph[fn:1]"
      (org-test-with-backend "test"
	(flet ((org-test-footnote-reference
		(fn-ref contents info)
		(org-element-interpret-data
		 (org-export-get-footnote-definition fn-ref info))))
	  (forward-line)
	  (should (equal "ParagraphOut of scope\n"
			 (org-export-as 'test 'subtree))))))))



;;; Links

(ert-deftest test-org-export/fuzzy-links ()
  "Test fuzzy link export specifications."
  ;; 1. Links to invisible (keyword) targets should be ignored.
  (org-test-with-parsed-data
      "Paragraph.\n#+TARGET: Test\n[[Test]]"
    (should-not
     (org-element-map
      tree 'link
      (lambda (link)
	(org-export-get-ordinal
	 (org-export-resolve-fuzzy-link link info) info)) info)))
  ;; 2. Link to an headline should return headline's number.
  (org-test-with-parsed-data
      "Paragraph.\n* Head1\n* Head2\n* Head3\n[[Head2]]"
    (should
     ;; Note: Headline's number is in fact a list of numbers.
     (equal '(2)
	    (org-element-map
	     tree 'link
	     (lambda (link)
	       (org-export-get-ordinal
		(org-export-resolve-fuzzy-link link info) info)) info t))))
  ;; 3. Link to a target in an item should return item's number.
  (org-test-with-parsed-data
      "- Item1\n  - Item11\n  - <<test>>Item12\n- Item2\n\n\n[[test]]"
    (should
     ;; Note: Item's number is in fact a list of numbers.
     (equal '(1 2)
	    (org-element-map
	     tree 'link
	     (lambda (link)
	       (org-export-get-ordinal
		(org-export-resolve-fuzzy-link link info) info)) info t))))
  ;; 4. Link to a target in a footnote should return footnote's
  ;;    number.
  (org-test-with-parsed-data "
Paragraph[1][2][fn:lbl3:C<<target>>][[test]][[target]]\n[1] A\n\n[2] <<test>>B"
    (should
     (equal '(2 3)
	    (org-element-map
	     tree 'link
	     (lambda (link)
	       (org-export-get-ordinal
		(org-export-resolve-fuzzy-link link info) info)) info))))
  ;; 5. Link to a named element should return sequence number of that
  ;;    element.
  (org-test-with-parsed-data
      "#+NAME: tbl1\n|1|2|\n#+NAME: tbl2\n|3|4|\n#+NAME: tbl3\n|5|6|\n[[tbl2]]"
    (should
     (= 2
	(org-element-map
	 tree 'link
	 (lambda (link)
	   (org-export-get-ordinal
	    (org-export-resolve-fuzzy-link link info) info)) info t))))
  ;; 6. Link to a target not within an item, a table, a footnote
  ;;    reference or definition should return section number.
  (org-test-with-parsed-data
      "* Head1\n* Head2\nParagraph<<target>>\n* Head3\n[[target]]"
    (should
     (equal '(2)
	    (org-element-map
	     tree 'link
	     (lambda (link)
	       (org-export-get-ordinal
		(org-export-resolve-fuzzy-link link info) info)) info t)))))

(defun test-org-export/resolve-coderef ()
  "Test `org-export-resolve-coderef' specifications."
  (let ((org-coderef-label-format "(ref:%s)"))
    ;; 1. A link to a "-n -k -r" block returns line number.
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -n -k -r\nText (ref:coderef)\n#+END_EXAMPLE"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (= (org-export-resolve-coderef "coderef" `(:parse-tree ,tree)) 1))))
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -n -k -r\n(+ 1 1) (ref:coderef)\n#+END_SRC"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (= (org-export-resolve-coderef "coderef" `(:parse-tree ,tree)) 1))))
    ;; 2. A link to a "-n -r" block returns line number.
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -n -r\nText (ref:coderef)\n#+END_EXAMPLE"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (= (org-export-resolve-coderef "coderef" `(:parse-tree ,tree)) 1))))
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -n -r\n(+ 1 1) (ref:coderef)\n#+END_SRC"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (= (org-export-resolve-coderef "coderef" `(:parse-tree ,tree)) 1))))
    ;; 3. A link to a "-n" block returns coderef.
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -n\n(+ 1 1) (ref:coderef)\n#+END_SRC"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (equal (org-export-resolve-coderef "coderef" `(:parse-tree ,tree))
		"coderef"))))
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -n\nText (ref:coderef)\n#+END_EXAMPLE"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (equal (org-export-resolve-coderef "coderef" `(:parse-tree ,tree))
		"coderef"))))
    ;; 4. A link to a "-r" block returns line number.
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -r\n(+ 1 1) (ref:coderef)\n#+END_SRC"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (= (org-export-resolve-coderef "coderef" `(:parse-tree ,tree)) 1))))
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -r\nText (ref:coderef)\n#+END_EXAMPLE"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (= (org-export-resolve-coderef "coderef" `(:parse-tree ,tree)) 1))))
    ;; 5. A link to a block without a switch returns coderef.
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp\n(+ 1 1) (ref:coderef)\n#+END_SRC"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (equal (org-export-resolve-coderef "coderef" `(:parse-tree ,tree))
		"coderef"))))
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE\nText (ref:coderef)\n#+END_EXAMPLE"
      (let ((tree (org-element-parse-buffer)))
	(should
	 (equal (org-export-resolve-coderef "coderef" `(:parse-tree ,tree))
		"coderef"))))
    ;; 6. Correctly handle continued line numbers.  A "+n" switch
    ;;    should resume numbering from previous block with numbered
    ;;    lines, ignoring blocks not numbering lines in the process.
    ;;    A "-n" switch resets count.
    (org-test-with-temp-text "
#+BEGIN_EXAMPLE -n
Text.
#+END_EXAMPLE

#+BEGIN_SRC emacs-lisp
\(- 1 1)
#+END_SRC

#+BEGIN_SRC emacs-lisp +n -r
\(+ 1 1) (ref:addition)
#+END_SRC

#+BEGIN_EXAMPLE -n -r
Another text. (ref:text)
#+END_EXAMPLE"
      (let* ((tree (org-element-parse-buffer))
	     (info `(:parse-tree ,tree)))
	(should (= (org-export-resolve-coderef "addition" info) 2))
	(should (= (org-export-resolve-coderef "text" info) 1))))
    ;; 7. Recognize coderef with user-specified syntax.
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -l \"[ref:%s]\"\nText. [ref:text]\n#+END_EXAMPLE"
      (let ((tree (org-element-parse-buffer)))
	(should (equal (org-export-resolve-coderef "text" `(:parse-tree ,tree))
		       "text"))))))



;;; Src-block and example-block

(ert-deftest test-org-export/unravel-code ()
  "Test `org-export-unravel-code' function."
  (let ((org-coderef-label-format "(ref:%s)"))
    ;; 1. Code without reference.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE\n(+ 1 1)\n#+END_EXAMPLE"
      (should (equal (org-export-unravel-code (org-element-current-element))
		     '("(+ 1 1)\n"))))
    ;; 2. Code with reference.
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE\n(+ 1 1) (ref:test)\n#+END_EXAMPLE"
      (should (equal (org-export-unravel-code (org-element-current-element))
		     '("(+ 1 1)\n" (1 . "test")))))
    ;; 3. Code with user-defined reference.
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -l \"[ref:%s]\"\n(+ 1 1) [ref:test]\n#+END_EXAMPLE"
      (should (equal (org-export-unravel-code (org-element-current-element))
		     '("(+ 1 1)\n" (1 . "test")))))
    ;; 4. Code references keys are relative to the current block.
    (org-test-with-temp-text "
#+BEGIN_EXAMPLE -n
\(+ 1 1)
#+END_EXAMPLE
#+BEGIN_EXAMPLE +n
\(+ 2 2)
\(+ 3 3) (ref:one)
#+END_EXAMPLE"
      (goto-line 5)
      (should (equal (org-export-unravel-code (org-element-current-element))
		     '("(+ 2 2)\n(+ 3 3)\n" (2 . "one")))))
    ;; 5. Free up comma-protected lines.
    ;;
    ;; 5.1. In an Org source block, every line is protected.
    (org-test-with-temp-text
	"#+BEGIN_SRC org\n,* Test\n,# comment\n,Text\n#+END_SRC"
      (should (equal (org-export-unravel-code (org-element-current-element))
		     '("* Test\n# comment\nText\n"))))
    ;; 5.2. In other blocks, only headlines, comments and keywords are
    ;;      protected.
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE\n,* Headline\n, * Not headline\n,Keep\n#+END_EXAMPLE"
      (should (equal (org-export-unravel-code (org-element-current-element))
		     '("* Headline\n, * Not headline\n,Keep\n"))))))



;;; Tables

(ert-deftest test-org-export/special-column ()
  "Test if the table's special column is properly recognized."
  ;; 1. First column is special if it contains only a special marking
  ;;    characters or empty cells.
  (org-test-with-temp-text "
| ! | 1 |
|   | 2 |"
    (should
     (org-export-table-has-special-column-p
      (org-element-map
       (org-element-parse-buffer) 'table 'identity nil 'first-match))))
  ;; 2. If the column contains anything else, it isn't special.
  (org-test-with-temp-text "
| ! | 1 |
| b | 2 |"
    (should-not
     (org-export-table-has-special-column-p
      (org-element-map
       (org-element-parse-buffer) 'table 'identity nil 'first-match))))
  ;; 3. Special marking characters are "#", "^", "*", "_", "/", "$"
  ;;    and "!".
  (org-test-with-temp-text "
| # | 1 |
| ^ | 2 |
| * | 3 |
| _ | 4 |
| / | 5 |
| $ | 6 |
| ! | 7 |"
    (should
     (org-export-table-has-special-column-p
      (org-element-map
       (org-element-parse-buffer) 'table 'identity nil 'first-match))))
  ;; 4. A first column with only empty cells isn't considered as
  ;;    special.
  (org-test-with-temp-text "
|   | 1 |
|   | 2 |"
    (should-not
     (org-export-table-has-special-column-p
      (org-element-map
       (org-element-parse-buffer) 'table 'identity nil 'first-match)))))

(ert-deftest test-org-export/table-row-is-special-p ()
  "Test `org-export-table-row-is-special-p' specifications."
  ;; 1. A row is special if it has a special marking character in the
  ;;    special column.
  (org-test-with-parsed-data "| ! | 1 |"
    (should
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 2. A row is special when its first field is "/"
  (org-test-with-parsed-data "
| / | 1 |
| a | b |"
    (should
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 3. A row only containing alignment cookies is also considered as
  ;;    special.
  (org-test-with-parsed-data "| <5> |   | <l> | <l22> |"
    (should
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 4. Everything else isn't considered as special.
  (org-test-with-parsed-data "| \alpha |   | c |"
    (should-not
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 5. Table's rules are never considered as special rows.
  (org-test-with-parsed-data "|---+---|"
    (should-not
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info))))

(ert-deftest test-org-export/has-header-p ()
  "Test `org-export-table-has-header-p' specifications."
  ;; 1. With an header.
  (org-test-with-parsed-data "
| a | b |
|---+---|
| c | d |"
    (should
     (org-export-table-has-header-p
      (org-element-map tree 'table 'identity info 'first-match)
      info)))
  ;; 2. Without an header.
  (org-test-with-parsed-data "
| a | b |
| c | d |"
    (should-not
     (org-export-table-has-header-p
      (org-element-map tree 'table 'identity info 'first-match)
      info)))
  ;; 3. Don't get fooled with starting and ending rules.
  (org-test-with-parsed-data "
|---+---|
| a | b |
| c | d |
|---+---|"
    (should-not
     (org-export-table-has-header-p
      (org-element-map tree 'table 'identity info 'first-match)
      info))))

(ert-deftest test-org-export/table-row-group ()
  "Test `org-export-table-row-group' specifications."
  ;; 1. A rule creates a new group.
  (org-test-with-parsed-data "
| a | b |
|---+---|
| 1 | 2 |"
    (should
     (equal
      '(1 nil 2)
      (mapcar (lambda (row) (org-export-table-row-group row info))
	      (org-element-map tree 'table-row 'identity)))))
  ;; 2. Special rows are ignored in count.
  (org-test-with-parsed-data "
| / | < | > |
|---|---+---|
|   | 1 | 2 |"
    (should
     (equal
      '(nil nil 1)
      (mapcar (lambda (row) (org-export-table-row-group row info))
	      (org-element-map tree 'table-row 'identity)))))
  ;; 3. Double rules also are ignored in count.
  (org-test-with-parsed-data "
| a | b |
|---+---|
|---+---|
| 1 | 2 |"
    (should
     (equal
      '(1 nil nil 2)
      (mapcar (lambda (row) (org-export-table-row-group row info))
	      (org-element-map tree 'table-row 'identity))))))

(ert-deftest test-org-export/table-cell-width ()
  "Test `org-export-table-cell-width' specifications."
  ;; 1. Width is primarily determined by width cookies.  If no cookie
  ;;    is found, cell's width is nil.
  (org-test-with-parsed-data "
| / | <l> | <6> | <l7> |
|   |  a  |  b  |  c   |"
    (should
     (equal
      '(nil 6 7)
      (mapcar (lambda (cell) (org-export-table-cell-width cell info))
	      (org-element-map tree 'table-cell 'identity info)))))
  ;; 2. The last width cookie has precedence.
  (org-test-with-parsed-data "
| <6> |
| <7> |
|  a  |"
    (should
     (equal
      '(7)
      (mapcar (lambda (cell) (org-export-table-cell-width cell info))
	      (org-element-map tree 'table-cell 'identity info)))))
  ;; 3. Valid width cookies must have a specific row.
  (org-test-with-parsed-data "| <6> | cell |"
    (should
     (equal
      '(nil nil)
      (mapcar (lambda (cell) (org-export-table-cell-width cell info))
	      (org-element-map tree 'table-cell 'identity))))))

(ert-deftest test-org-export/table-cell-alignment ()
  "Test `org-export-table-cell-alignment' specifications."
  (let ((org-table-number-fraction 0.5)
	(org-table-number-regexp "^[0-9]+$"))
    ;; 1. Alignment is primarily determined by alignment cookies.
    (org-test-with-temp-text "| <l> | <c> | <r> |"
      (let* ((tree (org-element-parse-buffer))
	     (info `(:parse-tree ,tree)))
	(should
	 (equal
	  '(left center right)
	  (mapcar (lambda (cell) (org-export-table-cell-alignment cell info))
		  (org-element-map tree 'table-cell 'identity))))))
    ;; 2. The last alignment cookie has precedence.
    (org-test-with-temp-text "
| <l8> |
| cell |
| <r9> |"
      (let* ((tree (org-element-parse-buffer))
	     (info `(:parse-tree ,tree)))
	(should
	 (equal
	  '(right right right)
	  (mapcar (lambda (cell) (org-export-table-cell-alignment cell info))
		  (org-element-map tree 'table-cell 'identity))))))
    ;; 3. If there's no cookie, cell's contents determine alignment.
    ;;    A column mostly made of cells containing numbers will align
    ;;    its cells to the right.
    (org-test-with-temp-text "
| 123       |
| some text |
| 12345     |"
      (let* ((tree (org-element-parse-buffer))
	     (info `(:parse-tree ,tree)))
	(should
	 (equal
	  '(right right right)
	  (mapcar (lambda (cell)
		    (org-export-table-cell-alignment cell info))
		  (org-element-map tree 'table-cell 'identity))))))
    ;; 5. Otherwise, they will be aligned to the left.
    (org-test-with-temp-text "
| text      |
| some text |
| \alpha    |"
      (let* ((tree (org-element-parse-buffer))
	     (info `(:parse-tree ,tree)))
	(should
	 (equal
	  '(left left left)
	  (mapcar (lambda (cell)
		    (org-export-table-cell-alignment cell info))
		  (org-element-map tree 'table-cell 'identity))))))))

(ert-deftest test-org-export/table-cell-borders ()
  "Test `org-export-table-cell-borders' specifications."
  ;; 1. Recognize various column groups indicators.
  (org-test-with-parsed-data "| / | < | > | <> |"
    (should
     (equal
      '((right bottom top) (left bottom top) (right bottom top)
	(right left bottom top))
      (mapcar (lambda (cell)
		(org-export-table-cell-borders cell info))
	      (org-element-map tree 'table-cell 'identity)))))
  ;; 2. Accept shortcuts to define column groups.
  (org-test-with-parsed-data "| / | < | < |"
    (should
     (equal
      '((right bottom top) (right left bottom top) (left bottom top))
      (mapcar (lambda (cell)
		(org-export-table-cell-borders cell info))
	      (org-element-map tree 'table-cell 'identity)))))
  ;; 3. A valid column groups row must start with a "/".
  (org-test-with-parsed-data "
|   | < |
| a | b |"
    (should
     (equal '((top) (top) (bottom) (bottom))
	    (mapcar (lambda (cell)
		      (org-export-table-cell-borders cell info))
		    (org-element-map tree 'table-cell 'identity)))))
  ;; 4. Take table rules into consideration.
  (org-test-with-parsed-data "
| 1 |
|---|
| 2 |"
    (should
     (equal '((below top) (bottom above))
	    (mapcar (lambda (cell)
		      (org-export-table-cell-borders cell info))
		    (org-element-map tree 'table-cell 'identity)))))
  ;; 5. Top and (resp. bottom) rules induce both `top' and `above'
  ;;    (resp. `bottom' and `below') borders.  Any special row is
  ;;    ignored.
  (org-test-with-parsed-data "
|---+----|
| / |    |
|   |  1 |
|---+----|"
    (should
     (equal '((bottom below top above))
	    (last
	     (mapcar (lambda (cell)
		       (org-export-table-cell-borders cell info))
		     (org-element-map tree 'table-cell 'identity)))))))

(ert-deftest test-org-export/table-dimensions ()
  "Test `org-export-table-dimensions' specifications."
  ;; 1. Standard test.
  (org-test-with-parsed-data "
| 1 | 2 | 3 |
| 4 | 5 | 6 |"
    (should
     (equal '(2 . 3)
	    (org-export-table-dimensions
	     (org-element-map tree 'table 'identity info 'first-match) info))))
  ;; 2. Ignore horizontal rules and special columns.
  (org-test-with-parsed-data "
| / | < | > |
| 1 | 2 | 3 |
|---+---+---|
| 4 | 5 | 6 |"
    (should
     (equal '(2 . 3)
	    (org-export-table-dimensions
	     (org-element-map tree 'table 'identity info 'first-match) info)))))

(ert-deftest test-org-export/table-cell-address ()
  "Test `org-export-table-cell-address' specifications."
  ;; 1. Standard test: index is 0-based.
  (org-test-with-parsed-data "| a | b |"
    (should
     (equal '((0 . 0) (0 . 1))
	    (org-element-map
	     tree 'table-cell
	     (lambda (cell) (org-export-table-cell-address cell info))
	     info))))
  ;; 2. Special column isn't counted, nor are special rows.
  (org-test-with-parsed-data "
| / | <> |
|   | c  |"
    (should
     (equal '(0 . 0)
	    (org-export-table-cell-address
	     (car (last (org-element-map tree 'table-cell 'identity info)))
	     info))))
  ;; 3. Tables rules do not count either.
  (org-test-with-parsed-data "
| a |
|---|
| b |
|---|
| c |"
    (should
     (equal '(2 . 0)
	    (org-export-table-cell-address
	     (car (last (org-element-map tree 'table-cell 'identity info)))
	     info))))
  ;; 4. Return nil for special cells.
  (org-test-with-parsed-data "| / | a |"
    (should-not
     (org-export-table-cell-address
      (org-element-map tree 'table-cell 'identity nil 'first-match)
      info))))

(ert-deftest test-org-export/get-table-cell-at ()
  "Test `org-export-get-table-cell-at' specifications."
  ;; 1. Address ignores special columns, special rows and rules.
  (org-test-with-parsed-data "
| / | <> |
|   | a  |
|---+----|
|   | b  |"
    (should
     (equal '("b")
	    (org-element-contents
	     (org-export-get-table-cell-at
	      '(1 . 0)
	      (org-element-map tree 'table 'identity info 'first-match)
	      info)))))
  ;; 2. Return value for a non-existent address is nil.
  (org-test-with-parsed-data "| a |"
    (should-not
     (org-export-get-table-cell-at
      '(2 . 2)
      (org-element-map tree 'table 'identity info 'first-match)
      info)))
  (org-test-with-parsed-data "| / |"
    (should-not
     (org-export-get-table-cell-at
      '(0 . 0)
      (org-element-map tree 'table 'identity info 'first-match)
      info))))

(ert-deftest test-org-export/table-cell-starts-colgroup-p ()
  "Test `org-export-table-cell-starts-colgroup-p' specifications."
  ;; 1. A cell at a beginning of a row always starts a column group.
  (org-test-with-parsed-data "| a |"
    (should
     (org-export-table-cell-starts-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 2. Special column should be ignored when determining the
  ;;    beginning of the row.
  (org-test-with-parsed-data "
| / |   |
|   | a |"
    (should
     (org-export-table-cell-starts-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 2. Explicit column groups.
  (org-test-with-parsed-data "
| / |   | < |
| a | b | c |"
    (should
     (equal
      '(yes no yes)
      (org-element-map
       tree 'table-cell
       (lambda (cell)
	 (if (org-export-table-cell-starts-colgroup-p cell info) 'yes 'no))
       info)))))

(ert-deftest test-org-export/table-cell-ends-colgroup-p ()
  "Test `org-export-table-cell-ends-colgroup-p' specifications."
  ;; 1. A cell at the end of a row always ends a column group.
  (org-test-with-parsed-data "| a |"
    (should
     (org-export-table-cell-ends-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 2. Special column should be ignored when determining the
  ;;    beginning of the row.
  (org-test-with-parsed-data "
| / |   |
|   | a |"
    (should
     (org-export-table-cell-ends-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 3. Explicit column groups.
  (org-test-with-parsed-data "
| / | < |   |
| a | b | c |"
    (should
     (equal
      '(yes no yes)
      (org-element-map
       tree 'table-cell
       (lambda (cell)
	 (if (org-export-table-cell-ends-colgroup-p cell info) 'yes 'no))
       info)))))

(ert-deftest test-org-export/table-row-starts-rowgroup-p ()
  "Test `org-export-table-row-starts-rowgroup-p' specifications."
  ;; 1. A row at the beginning of a table always starts a row group.
  ;;    So does a row following a table rule.
  (org-test-with-parsed-data "
| a |
|---|
| b |"
    (should
     (equal
      '(yes no yes)
      (org-element-map
       tree 'table-row
       (lambda (row)
	 (if (org-export-table-row-starts-rowgroup-p row info) 'yes 'no))
       info))))
  ;; 2. Special rows should be ignored when determining the beginning
  ;;    of the row.
  (org-test-with-parsed-data "
| / | < |
|   | a |
|---+---|
| / | < |
|   | b |"
    (should
     (equal
      '(yes no yes)
      (org-element-map
       tree 'table-row
       (lambda (row)
	 (if (org-export-table-row-starts-rowgroup-p row info) 'yes 'no))
       info)))))

(ert-deftest test-org-export/table-row-ends-rowgroup-p ()
  "Test `org-export-table-row-ends-rowgroup-p' specifications."
  ;; 1. A row at the end of a table always ends a row group.  So does
  ;;    a row preceding a table rule.
  (org-test-with-parsed-data "
| a |
|---|
| b |"
    (should
     (equal
      '(yes no yes)
      (org-element-map
       tree 'table-row
       (lambda (row)
	 (if (org-export-table-row-ends-rowgroup-p row info) 'yes 'no))
       info))))
  ;; 2. Special rows should be ignored when determining the beginning
  ;;    of the row.
  (org-test-with-parsed-data "
|   | a |
| / | < |
|---+---|
|   | b |
| / | < |"
    (should
     (equal
      '(yes no yes)
      (org-element-map
       tree 'table-row
       (lambda (row)
	 (if (org-export-table-row-ends-rowgroup-p row info) 'yes 'no))
       info)))))

(ert-deftest test-org-export/table-row-starts-header-p ()
  "Test `org-export-table-row-starts-header-p' specifications."
  ;; 1. Only the row starting the first row group starts the table
  ;;    header.
  (org-test-with-parsed-data "
| a |
| b |
|---|
| c |"
    (should
     (equal
      '(yes no no no)
      (org-element-map
       tree 'table-row
       (lambda (row)
	 (if (org-export-table-row-starts-header-p row info) 'yes 'no))
       info))))
  ;; 2. A row cannot start an header if there's no header in the
  ;;    table.
  (org-test-with-parsed-data "
| a |
|---|"
    (should-not
     (org-export-table-row-starts-header-p
      (org-element-map tree 'table-row 'identity info 'first-match)
      info))))

(ert-deftest test-org-export/table-row-ends-header-p ()
  "Test `org-export-table-row-ends-header-p' specifications."
  ;; 1. Only the row starting the first row group starts the table
  ;;    header.
  (org-test-with-parsed-data "
| a |
| b |
|---|
| c |"
    (should
     (equal
      '(no yes no no)
      (org-element-map
       tree 'table-row
       (lambda (row)
	 (if (org-export-table-row-ends-header-p row info) 'yes 'no))
       info))))
  ;; 2. A row cannot start an header if there's no header in the
  ;;    table.
  (org-test-with-parsed-data "
| a |
|---|"
    (should-not
     (org-export-table-row-ends-header-p
      (org-element-map tree 'table-row 'identity info 'first-match)
      info))))


(provide 'test-org-export)
;;; test-org-export.el end here
