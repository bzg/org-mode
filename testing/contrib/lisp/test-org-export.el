;;; test-org-export.el --- Tests for org-export.el

;; Copyright (C) 2012  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments



;;; Code:
(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts)
  (require 'org-export))



;;; Tests

(require 'org-test)
(require 'org-export)

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
 ^:{} toc:1 tags:not-in-toc tasks:todo")
    '(:with-archived-trees
      headline :with-creator comment :with-drawers ("TEST")
      :with-sub-superscript {} :with-toc 1 :with-tags not-in-toc
      :with-tasks todo))))

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
  (org-test-with-temp-text "* Head1\n* Head2 :export:"
    (org-test-with-backend "test"
      (should
       (string-match
	"\\* Head2[ \t]+:export:\n"
	(org-export-as 'test nil nil nil
		       '(:select-tags ("export") :with-tags nil))))))
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
