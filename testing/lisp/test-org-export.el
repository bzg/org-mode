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
 ^:{} toc:1 tags:not-in-toc tasks:todo num:2")
    '( :section-numbers
       2
       :with-archived-trees headline :with-creator comment
       :with-drawers ("TEST") :with-sub-superscript {} :with-toc 1
       :with-tags not-in-toc :with-tasks todo))))

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
      "Me, Myself and I" :creator "Idem" :date "Today"
      :description "Testing\nwith two lines" :email "some@email.org"
      :exclude-tags ("noexport" "invisible") :keywords "test" :language "en"
      :select-tags ("export") :title "Some title with spaces"))))

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
		       "")))))
  (let ((org-drawers '("TEST")))
    (org-test-with-temp-text ":TEST:\ncontents\n:END:"
      (org-test-with-backend "test"
	(should (equal (org-export-as 'test nil nil nil '(:with-drawers t))
		       ":TEST:\ncontents\n:END:\n"))))))

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
      (mark-paragraph)
      (should (equal (org-export-as 'test) "text\n")))))

(ert-deftest test-org-export/export-snippet ()
  "Test export snippets transcoding."
  (org-test-with-temp-text "@test{A}@t{B}"
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

(ert-deftest test-org-export/footnotes ()
  "Test footnotes specifications."
  (let ((org-footnote-section nil))
    ;; 1. Read every type of footnote.
    (org-test-with-temp-text
	"Text[fn:1] [1] [fn:label:C] [fn::D]\n\n[fn:1] A\n\n[1] B"
      (let* ((tree (org-element-parse-buffer))
	     (info (org-combine-plists
		    (org-export-initial-options) '(:with-footnotes t))))
	(setq info (org-combine-plists
		    info (org-export-collect-tree-properties tree info 'test)))
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
	     (info (org-combine-plists
		    (org-export-initial-options) '(:with-footnotes t))))
	(setq info (org-combine-plists
		    info (org-export-collect-tree-properties tree info 'test)))
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
	     (info (org-combine-plists
		    (org-export-initial-options) '(:with-footnotes t))))
	(setq info (org-combine-plists
		    info (org-export-collect-tree-properties tree info 'test)))
	;; Both footnotes should be seen.
	(should
	 (= (length (org-export-collect-footnote-definitions tree info)) 2))))
    ;; 4. Test footnotes definitions collection.
    (org-test-with-temp-text "Text[fn:1:A[fn:2]] [fn:3].

\[fn:2] B [fn:3] [fn::D].

\[fn:3] C."
      (let ((tree (org-element-parse-buffer))
	    (info (org-combine-plists
		   (org-export-initial-options) '(:with-footnotes t))))
	(setq info (org-combine-plists
		    info (org-export-collect-tree-properties tree info 'test)))
	(should (= (length (org-export-collect-footnote-definitions tree info))
		   4))))))

(ert-deftest test-org-export/fuzzy-links ()
  "Test fuzz link export specifications."
  ;; 1. Links to invisible (keyword) targets should be ignored.
  (org-test-with-temp-text
      "Paragraph.\n#+TARGET: Test\n[[Test]]"
    (let* ((tree (org-element-parse-buffer))
	   (info (org-combine-plists (org-export-initial-options))))
      (setq info (org-combine-plists
		  info (org-export-collect-tree-properties tree info 'test)))
      (should-not
       (org-element-map
	tree 'link
	(lambda (link)
	  (org-export-get-ordinal
	   (org-export-resolve-fuzzy-link link info) info)) info))))
  ;; 2. Link to an headline should return headline's number.
  (org-test-with-temp-text
      "Paragraph.\n* Head1\n* Head2\n* Head3\n[[Head2]]"
    (let* ((tree (org-element-parse-buffer))
	   (info (org-combine-plists (org-export-initial-options))))
      (setq info (org-combine-plists
		  info (org-export-collect-tree-properties tree info 'test)))
      (should
       ;; Note: Headline's number is in fact a list of numbers.
       (equal '(2)
	      (org-element-map
	       tree 'link
	       (lambda (link)
		 (org-export-get-ordinal
		  (org-export-resolve-fuzzy-link link info) info)) info t)))))
  ;; 3. Link to a target in an item should return item's number.
  (org-test-with-temp-text
      "- Item1\n  - Item11\n  - <<test>>Item12\n- Item2\n\n\n[[test]]"
    (let* ((tree (org-element-parse-buffer))
	   (info (org-combine-plists (org-export-initial-options))))
      (setq info (org-combine-plists
		  info (org-export-collect-tree-properties tree info 'test)))
      (should
       ;; Note: Item's number is in fact a list of numbers.
       (equal '(1 2)
	      (org-element-map
	       tree 'link
	       (lambda (link)
		 (org-export-get-ordinal
		  (org-export-resolve-fuzzy-link link info) info)) info t)))))
  ;; 4. Link to a target in a footnote should return footnote's
  ;;    number.
  (org-test-with-temp-text
      "Paragraph[1][2][fn:lbl3:C<<target>>][[test]][[target]]\n[1] A\n\n[2] <<test>>B"
    (let* ((tree (org-element-parse-buffer))
	   (info (org-combine-plists (org-export-initial-options))))
      (setq info (org-combine-plists
		  info (org-export-collect-tree-properties tree info 'test)))
      (should
       (equal '(2 3)
	      (org-element-map
	       tree 'link
	       (lambda (link)
		 (org-export-get-ordinal
		  (org-export-resolve-fuzzy-link link info) info)) info)))))
  ;; 5. Link to a named element should return sequence number of that
  ;;    element.
  (org-test-with-temp-text
      "#+NAME: tbl1\n|1|2|\n#+NAME: tbl2\n|3|4|\n#+NAME: tbl3\n|5|6|\n[[tbl2]]"
    (let* ((tree (org-element-parse-buffer))
	   (info (org-combine-plists (org-export-initial-options))))
      (setq info (org-combine-plists
		  info (org-export-collect-tree-properties tree info 'test)))
      (should
       (= 2
	  (org-element-map
	   tree 'link
	   (lambda (link)
	     (org-export-get-ordinal
	      (org-export-resolve-fuzzy-link link info) info)) info t)))))
  ;; 6. Link to a target not within an item, a table, a footnote
  ;;    reference or definition should return section number.
  (org-test-with-temp-text
      "* Head1\n* Head2\nParagraph<<target>>\n* Head3\n[[target]]"
    (let* ((tree (org-element-parse-buffer))
	   (info (org-combine-plists (org-export-initial-options))))
      (setq info (org-combine-plists
		  info (org-export-collect-tree-properties tree info 'test)))
      (should
       (equal '(2)
	      (org-element-map
	       tree 'link
	       (lambda (link)
		 (org-export-get-ordinal
		  (org-export-resolve-fuzzy-link link info) info)) info t))))))
