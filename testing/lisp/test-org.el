;;; test-org.el

;; Copyright (c) ߚ David Maus
;; Authors: David Maus

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for Org-mode tests

;;; Code:
(ert-deftest test-org/org-link-escape-ascii-character ()
  "Escape an ascii character."
  (should
   (string=
    "%5B"
    (org-link-escape "["))))

(ert-deftest test-org/org-link-escape-ascii-ctrl-character ()
  "Escape an ascii control character."
  (should
   (string=
    "%09"
    (org-link-escape "\t"))))

(ert-deftest test-org/org-link-escape-multibyte-character ()
  "Escape an unicode multibyte character."
  (should
   (string=
    "%E2%82%AC"
    (org-link-escape "€"))))

(ert-deftest test-org/org-link-escape-custom-table ()
  "Escape string with custom character table."
  (should
   (string=
    "Foo%3A%42ar%0A"
    (org-link-escape "Foo:Bar\n" '(?\: ?\B)))))

(ert-deftest test-org/org-link-escape-custom-table-merge ()
  "Escape string with custom table merged with default table."
  (should
   (string=
    "%5BF%6F%6F%3A%42ar%0A%5D"
    (org-link-escape "[Foo:Bar\n]" '(?\: ?\B ?\o) t))))

(ert-deftest test-org/org-link-unescape-ascii-character ()
  "Unescape an ascii character."
  (should
   (string=
    "["
    (org-link-unescape "%5B"))))

(ert-deftest test-org/org-link-unescape-ascii-ctrl-character ()
  "Unescpae an ascii control character."
  (should
   (string=
    "\n"
    (org-link-unescape "%0A"))))

(ert-deftest test-org/org-link-unescape-multibyte-character ()
  "Unescape unicode multibyte character."
  (should
   (string=
    "€"
    (org-link-unescape "%E2%82%AC"))))

(ert-deftest test-org/org-link-unescape-ascii-extended-char ()
  "Unescape old style percent escaped character."
  (should
   (string=
    "àâçèéêîôùû"
        (decode-coding-string (org-link-unescape "%E0%E2%E7%E8%E9%EA%EE%F4%F9%FB") 'latin-1))))

(ert-deftest test-org/org-link-escape-url-with-escaped-char ()
  "Escape and unscape a URL that includes an escaped char.
http://article.gmane.org/gmane.emacs.orgmode/21459/"
  (should
   (string=
    "http://some.host.com/form?&id=blah%2Bblah25"
    (org-link-unescape (org-link-escape "http://some.host.com/form?&id=blah%2Bblah25")))))

(ert-deftest test-org/accumulated-properties-in-drawers ()
  "Ensure properties accumulate in subtree drawers."
  (org-test-at-id "75282ba2-f77a-4309-a970-e87c149fe125"
    (org-babel-next-src-block)
    (should (equal '(2 1) (org-babel-execute-src-block)))))



;;; Links

;;;; Fuzzy links

;; Fuzzy links [[text]] encompass links to a target (<<text>>), to
;; a target keyword (aka an invisible target: #+TARGET: text), to
;; a named element (#+name: text) and to headlines (* Text).

(ert-deftest test-org/fuzzy-links ()
  "Test fuzzy links specifications."
  ;; 1. Fuzzy link goes in priority to a matching target.
  (org-test-with-temp-text
      "#+TARGET: Test\n#+NAME: Test\n|a|b|\n<<Test>>\n* Test\n[[Test]]"
    (goto-line 6)
    (org-open-at-point)
    (should (looking-at "<<Test>>")))
  ;; 2. Fuzzy link should then go to a matching target keyword.
  (org-test-with-temp-text
      "#+NAME: Test\n|a|b|\n#+TARGET: Test\n* Test\n[[Test]]"
    (goto-line 5)
    (org-open-at-point)
    (should (looking-at "#\\+TARGET: Test")))
  ;; 3. Then fuzzy link points to an element with a given name.
  (org-test-with-temp-text "Test\n#+NAME: Test\n|a|b|\n* Test\n[[Test]]"
    (goto-line 5)
    (org-open-at-point)
    (should (looking-at "#\\+NAME: Test")))
  ;; 4. A target still lead to a matching headline otherwise.
  (org-test-with-temp-text "* Head1\n* Head2\n*Head3\n[[Head2]]"
    (goto-line 4)
    (org-open-at-point)
    (should (looking-at "\\* Head2")))
  ;; 5. With a leading star in link, enforce heading match.
  (org-test-with-temp-text "#+TARGET: Test\n* Test\n<<Test>>\n[[*Test]]"
    (goto-line 4)
    (org-open-at-point)
    (should (looking-at "\\* Test"))))



;;; Filling

(ert-deftest test-org/fill-paragraph ()
  "Test `org-fill-paragraph' specifications."
  ;; At an Org table, align it.
  (org-test-with-temp-text "|a|"
    (org-fill-paragraph)
    (should (equal (buffer-string) "| a |\n")))
  ;; At a paragraph, preserve line breaks.
  (org-test-with-temp-text "some \\\\\nlong\ntext"
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string) "some \\\\\nlong text"))))
  ;; At a verse block, fill paragraph at point, also preserving line
  ;; breaks.  Though, do nothing when point is at the block
  ;; boundaries.
  (org-test-with-temp-text "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"
    (forward-line)
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string)
		     "#+BEGIN_VERSE\nSome \\\\\nlong text\n#+END_VERSE"))))
  (org-test-with-temp-text "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string)
		     "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"))))
  ;; Fill contents of `comment-block' elements.
  (should
   (equal
    (org-test-with-temp-text "#+BEGIN_COMMENT\nSome\ntext\n#+END_COMMENT"
      (let ((fill-column 20))
	(forward-line)
	(org-fill-paragraph)
	(buffer-string)))
    "#+BEGIN_COMMENT\nSome text\n#+END_COMMENT"))
  ;; Fill `comment' elements, indented or not.
  (should
   (equal "# A B"
	  (org-test-with-temp-text "# A\n#B"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  (should
   (equal "  #+ A B"
	  (org-test-with-temp-text "  #+ A\n  #+ B"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Do nothing at affiliated keywords.
  (org-test-with-temp-text "#+NAME: para\nSome\ntext."
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string) "#+NAME: para\nSome\ntext.")))))

(ert-deftest test-org/auto-fill-function ()
  "Test auto-filling features."
  ;; Auto fill paragraph.
  (should
   (equal "12345\n7890"
	  (org-test-with-temp-text "12345 7890"
	    (let ((fill-column 5))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Auto fill first paragraph in an item.
  (should
   (equal "- 12345\n  7890"
	  (org-test-with-temp-text "- 12345 7890"
	    (let ((fill-column 7))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Auto fill comments, indented or not.
  (should
   (equal "# 12345\n# 7890"
	  (org-test-with-temp-text "# 12345 7890"
	    (let ((fill-column 7))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  (should
   (equal "  #+ 12345\n  #+ 7890"
	  (org-test-with-temp-text "  #+ 12345 7890"
	    (let ((fill-column 10))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Verse and comment block: auto fill contents.
  (should
   (equal "#+BEGIN_VERSE\n12345\n7890\n#+END_VERSE"
	  (org-test-with-temp-text "#+BEGIN_VERSE\n12345 7890\n#+END_VERSE"
	    (let ((fill-column 5))
	      (forward-line)
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  (should
   (equal "#+BEGIN_COMMENT\n12345\n7890\n#+END_COMMENT"
	  (org-test-with-temp-text "#+BEGIN_COMMENT\n12345 7890\n#+END_COMMENT"
	    (let ((fill-column 5))
	      (forward-line)
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill if a new item could be created.
  (should-not
   (equal "12345\n- 90"
	  (org-test-with-temp-text "12345 - 90"
	    (let ((fill-column 5))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill if a line break could be introduced.
  (should-not
   (equal "123\\\\\n7890"
	  (org-test-with-temp-text "123\\\\ 7890"
	    (let ((fill-column 6))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill affiliated keywords.
  (should-not
   (equal "#+ATTR_LATEX: ABC\nDEFGHIJKL"
	  (org-test-with-temp-text "#+ATTR_LATEX: ABC DEFGHIJKL"
	    (let ((fill-column 20))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string))))))


(provide 'test-org)

;;; test-org.el ends here
