;;; test-org.el --- tests for org.el  -*- lexical-binding: t; -*-

;; Authors: Ihor Radchenko

;; This file is not part of GNU Emacs.

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

;; Org folding tests.

;;; Code:

(eval-and-compile (require 'cl-lib))



(ert-deftest test-org-fold/hide-drawer-toggle ()
  "Test `org-fold-hide-drawer-toggle' specifications."
  ;; Error when not at a drawer.
  (should-error
   (org-test-with-temp-text ":fake-drawer:\ncontents"
     (org-fold-hide-drawer-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  (should-error
   (org-test-with-temp-text
       "#+begin_example\n<point>:D:\nc\n:END:\n#+end_example"
     (org-fold-hide-drawer-toggle t)))
  ;; Hide drawer.
  (should
   (org-test-with-temp-text ":drawer:\ncontents\n:end:"
     (org-fold-show-all)
     (org-fold-hide-drawer-toggle)
     (get-char-property (line-end-position) 'invisible)))
  ;; Show drawer unconditionally when optional argument is `off'.
  (should-not
   (org-test-with-temp-text ":drawer:\ncontents\n:end:"
     (org-fold-hide-drawer-toggle)
     (org-fold-hide-drawer-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  ;; Hide drawer unconditionally when optional argument is non-nil.
  (should
   (org-test-with-temp-text ":drawer:\ncontents\n:end:"
     (org-fold-hide-drawer-toggle t)
     (get-char-property (line-end-position) 'invisible)))
  ;; Do not hide drawer when called from final blank lines.
  (should-not
   (org-test-with-temp-text ":drawer:\ncontents\n:end:\n\n<point>"
     (org-fold-show-all)
     (org-fold-hide-drawer-toggle)
     (goto-char (point-min))
     (get-char-property (line-end-position) 'invisible)))
  ;; Don't leave point in an invisible part of the buffer when hiding
  ;; a drawer away.
  (should-not
   (org-test-with-temp-text ":drawer:\ncontents\n<point>:end:"
     (org-fold-hide-drawer-toggle)
     (get-char-property (point) 'invisible))))

(ert-deftest test-org/hide-block-toggle ()
  "Test `org-fold-hide-block-toggle' specifications."
  ;; Error when not at a block.
  (should-error
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents"
     (org-fold-hide-block-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  ;; Hide block.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\ncontents\n#+END_CENTER"
     (org-fold-hide-block-toggle)
     (get-char-property (line-end-position) 'invisible)))
  (should
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\ncontents\n#+END_EXAMPLE"
     (org-fold-hide-block-toggle)
     (get-char-property (line-end-position) 'invisible)))
  ;; Show block unconditionally when optional argument is `off'.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-fold-hide-block-toggle)
     (org-fold-hide-block-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-fold-hide-block-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  ;; Hide block unconditionally when optional argument is non-nil.
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-fold-hide-block-toggle t)
     (get-char-property (line-end-position) 'invisible)))
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-fold-hide-block-toggle)
     (org-fold-hide-block-toggle t)
     (get-char-property (line-end-position) 'invisible)))
  ;; Do not hide block when called from final blank lines.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE\n\n<point>"
     (org-fold-hide-block-toggle)
     (goto-char (point-min))
     (get-char-property (line-end-position) 'invisible)))
  ;; Don't leave point in an invisible part of the buffer when hiding
  ;; a block away.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n<point>#+END_QUOTE"
     (org-fold-hide-block-toggle)
     (get-char-property (point) 'invisible))))

(ert-deftest test-org-fold/hide-block-toggle-maybe ()
  "Test `org-fold-hide-block-toggle' specifications."
  (should
   (org-test-with-temp-text "#+BEGIN: dynamic\nContents\n#+END:"
     (org-hide-block-toggle)))
  (should-error
   (org-test-with-temp-text "Paragraph" (org-hide-block-toggle))))

(ert-deftest test-org-fold/org-fold-hide-entry ()
  "Test `org-fold-hide-entry' specifications."
  ;; Do nothing on empty heading with children.
  (should-not
   (org-test-with-temp-text
       "* H<point>EADING
** subheading1
** subheading2
"
     (org-fold-hide-entry)
     (org-invisible-p (line-end-position))))
  ;; Text inside entry.  Hide it.
  (should
   (org-test-with-temp-text
       "* H<point>EADING
Some text here
** subheading1
** subheading2
"
     (org-fold-hide-entry)
     (org-invisible-p (line-end-position))))
  ;; Heading at EOB.  Do nothing.
  (should-not
   (org-test-with-temp-text
       "* H<point>EADING"
     (org-fold-hide-entry)
     (org-invisible-p (line-end-position)))))

(ert-deftest test-org-fold/show-set-visibility ()
  "Test `org-fold-show-set-visibility' specifications."
  ;; Do not throw an error before first heading.
  (should
   (org-test-with-temp-text "Preamble\n* Headline"
     (org-fold-show-set-visibility 'tree)
     t))
  ;; Test all visibility spans, both on headline and in entry.
  (let ((list-visible-lines
	 (lambda (state headerp)
	   (org-test-with-temp-text "* Grandmother  (0)
** Uncle              (1)
*** Heir              (2)
** Father             (3)
   Ancestor text      (4)
*** Sister            (5)
    Sibling text      (6)
*** Self              (7)
    Match	      (8)
**** First born	      (9)
     Child text	      (10)
**** The other child  (11)
*** Brother	      (12)
** Aunt               (13)
"
	     (org-cycle t)
	     (search-forward (if headerp "Self" "Match"))
	     (org-fold-show-set-visibility state)
	     (goto-char (point-min))
	     (let (result (line 0))
	       (while (not (eobp))
		 (unless (org-invisible-p2) (push line result))
		 (cl-incf line)
		 (forward-line))
	       (nreverse result))))))
    (should (equal '(0 7) (funcall list-visible-lines 'minimal t)))
    (should (equal '(0 7 8) (funcall list-visible-lines 'minimal nil)))
    (should (equal '(0 7 8 9) (funcall list-visible-lines 'local t)))
    (should (equal '(0 7 8 9) (funcall list-visible-lines 'local nil)))
    (should (equal '(0 3 7) (funcall list-visible-lines 'ancestors t)))
    (should (equal '(0 3 7 8) (funcall list-visible-lines 'ancestors nil)))
    (should (equal '(0 3 7 8 9 10 11)
                   (funcall list-visible-lines 'ancestors-full t)))
    (should (equal '(0 3 7 8 9 10 11)
                   (funcall list-visible-lines 'ancestors-full nil)))
    (should (equal '(0 3 5 7 12) (funcall list-visible-lines 'lineage t)))
    (should (equal '(0 3 5 7 8 9 12) (funcall list-visible-lines 'lineage nil)))
    (should (equal '(0 1 3 5 7 12 13) (funcall list-visible-lines 'tree t)))
    (should (equal '(0 1 3 5 7 8 9 11 12 13)
		   (funcall list-visible-lines 'tree nil)))
    (should (equal '(0 1 3 4 5 7 12 13)
		   (funcall list-visible-lines 'canonical t)))
    (should (equal '(0 1 3 4 5 7 8 9 11 12 13)
		   (funcall list-visible-lines 'canonical nil))))
  ;; When point is hidden in a drawer or a block, make sure to make it
  ;; visible.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\nText\n#+END_QUOTE"
     (org-fold-hide-block-toggle)
     (search-forward "Text")
     (org-fold-show-set-visibility 'minimal)
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text ":DRAWER:\nText\n:END:"
     (org-fold-hide-drawer-toggle)
     (search-forward "Text")
     (org-fold-show-set-visibility 'minimal)
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text
       "#+BEGIN_QUOTE\n<point>:DRAWER:\nText\n:END:\n#+END_QUOTE"
     (org-fold-hide-drawer-toggle)
     (forward-line -1)
     (org-fold-hide-block-toggle)
     (search-forward "Text")
     (org-fold-show-set-visibility 'minimal)
     (org-invisible-p2))))

(ert-deftest test-org-fold/copy-visible ()
  "Test `org-copy-visible' specifications."
  ;;`org-unfontify-region', which is wired up to
  ;; `font-lock-unfontify-region-function', removes the invisible text
  ;; property, among other things.
  (cl-letf (((symbol-function 'org-unfontify-region) #'ignore))
    (should
     (equal "Foo"
	    (org-test-with-temp-text "Foo"
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))
    ;; Skip invisible characters by text property.
    (should
     (equal "Foo"
	    (org-test-with-temp-text #("F<hidden>oo" 1 9 (invisible t))
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))
    ;; Skip invisible characters by overlay.
    (should
     (equal "Foo"
	    (org-test-with-temp-text "F<hidden>oo"
	      (let ((o (make-overlay 2 10)))
	        (overlay-put o 'invisible t))
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))
    ;; Handle invisible characters at the beginning and the end of the
    ;; buffer.
    (should
     (equal "Foo"
	    (org-test-with-temp-text #("<hidden>Foo" 0 8 (invisible t))
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))
    (should
     (equal "Foo"
	    (org-test-with-temp-text #("Foo<hidden>" 3 11 (invisible t))
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))
    ;; Handle multiple visible parts.
    (should
     (equal "abc"
	    (org-test-with-temp-text
	        #("aXbXc" 1 2 (invisible t) 3 4 (invisible t))
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))
    ;; Handle adjacent invisible parts.
    (should
     (equal "ab"
	    (org-test-with-temp-text
	        #("aXXb" 1 2 (invisible t) 2 3 (invisible org-link))
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))
    ;; Copies text based on what's actually visible, as defined by
    ;; `buffer-invisibility-spec'.
    (should
     (equal "aYb"
	    (org-test-with-temp-text
	        #("aXYb"
                  1 2 (invisible t)
                  2 3 (invisible org-test-copy-visible))
	      (let ((kill-ring nil))
	        (org-copy-visible (point-min) (point-max))
	        (current-kill 0 t)))))))

(ert-deftest test-org-fold/set-visibility-according-to-property ()
  "Test `org-set-visibility-according-to-property' specifications."
  ;; "folded" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: folded
:END:
** <point>b"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  (org-test-with-temp-text
      "<point>
#+STARTUP: overview
* A
** AA
** AB
*** ABA
:PROPERTIES:
:VISIBILITY: folded
:END:
**** ABAA
**** ABAB
**** ABAC
** AC
* B
"
    (org-set-regexps-and-options)
    (org-cycle-set-startup-visibility)
    (search-forward "A")
    (should-not (invisible-p (point)))
    (search-forward "AB")
    (should (invisible-p (point)))
    (search-forward "ABA")
    (should (invisible-p (point)))
    (search-forward "ABAB")
    (should (invisible-p (point)))
    (search-forward "AC")
    (should (invisible-p (point)))
    (search-forward "B")
    (should-not (invisible-p (point))))
  ;; "children" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: children
:END:
** b
<point>Contents
** c"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: children
:END:
** b
Contents
*** <point>c"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  ;; "content" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: content
:END:
** b
<point>Contents
*** c"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: content
:END:
** b
Contents
*** <point>c"
     (org-set-visibility-according-to-property)
     (not (invisible-p (point)))))
  ;; "showall" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: showall
:END:
** b
<point>Contents
*** c"
     (org-set-visibility-according-to-property)
     (not (invisible-p (point)))))
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: showall
:END:
** b
Contents
*** <point>c"
     (org-set-visibility-according-to-property)
     (not (invisible-p (point)))))
  ;; When VISIBILITY properties are nested, ignore inner ones.
  (should
   (org-test-with-temp-text
       "
* A
:PROPERTIES:
:VISIBILITY: folded
:END:
** <point>B
:PROPERTIES:
:VISIBILITY: folded
:END:"
     (org-set-visibility-according-to-property)
     (invisible-p (point)))))

(ert-deftest test-org-fold/visibility-show-branches ()
  "Test visibility of inline archived subtrees."
  (org-test-with-temp-text
      "* Foo<point>
** Bar :ARCHIVE:
*** Baz
"
    (org-kill-note-or-show-branches)
    (should (org-invisible-p (- (point-max) 2)))))

(ert-deftest test-org-fold/org-cycle-narrowed-subtree ()
  "Test cycling in narrowed buffer."
  (org-test-with-temp-text
      "* Heading 1<point>
** Child 1.1
** Child 1.2
some text
*** Sub-child 1.2.1
* Heading 2"
    (org-overview)
    (org-narrow-to-subtree)
    (org-cycle)
    (re-search-forward "Sub-child")
    (should (org-invisible-p))))

(ert-deftest test-org-fold/org-fold-reveal-broken-structure ()
  "Test unfolding broken elements."
  (let ((org-fold-core-style 'text-properties))
    (org-test-with-temp-text
        "<point>* Heading 1
Text here"
      (org-overview)
      (re-search-forward "Text")
      (should (org-invisible-p))
      (goto-char 1)
      (org-delete-char 1)
      (re-search-forward "Text")
      (should-not (org-invisible-p)))
    (org-test-with-temp-text
        "* Heading 1
<point>:PROPERTIES:
:ID: something
:END:
Text here"
      (org-cycle)
      (org-fold-hide-drawer-all)
      (re-search-forward "ID")
      (should (org-invisible-p))
      (re-search-backward ":PROPERTIES:")
      (delete-char 1)
      (re-search-forward "ID")
      (should-not (org-invisible-p)))
    (org-test-with-temp-text
        "* Heading 1
<point>:PROPERTIES:
:ID: something
:END:
Text here"
      (org-cycle)
      (org-fold-hide-drawer-all)
      (re-search-forward "ID")
      (should (org-invisible-p))
      (re-search-forward ":END:")
      (delete-char -1)
      (re-search-backward "ID")
      (should-not (org-invisible-p)))
    (org-test-with-temp-text
        "* Heading 1
<point>#+begin_src emacs-lisp
(+ 1 2)
#+end_src
Text here"
      (org-cycle)
      (org-fold-hide-drawer-all)
      (re-search-forward "end")
      (should (org-invisible-p))
      (delete-char -1)
      (re-search-backward "2")
      (should-not (org-invisible-p)))))

(ert-deftest test-org-fold/re-hide-edits-inside-fold ()
  "Test edits inside folded regions."
  (org-test-with-temp-text
      "<point>* Heading 1
Text here"
    (org-overview)
    (org-set-property "TEST" "1")
    (re-search-forward "TEST")
    (should (org-invisible-p)))
  (org-test-with-temp-text
      "* Heading 1<point>
Text here"
    (org-overview)
    (insert " and extra heading text")
    (re-search-backward "heading")
    (should-not (org-invisible-p)))
  (org-test-with-temp-text
      "* Heading 1
Text<point> here"
    (org-overview)
    (insert " and extra text")
    (re-search-backward "extra")
    (should (org-invisible-p))))


(defmacro test-org-fold-with-default-template (&rest body)
  "Run `org-test-with-temp-text' using default folded template."
  (declare (indent 0))
  `(let ((org-link-descriptive t))
     (org-test-with-temp-text
         "#+STARTUP: showeverything
* <point>Folded heading
Folded Paragraph inside heading.
* Unfolded heading
:FOLDED-DRAWER:
Folded Paragraph inside drawer.
:END:
Unfolded Paragraph.
#+begin_src emacs-lisp
(message \"Folded block\")
#+end_src
[[hiddenlink][link]]
"
       (org-cycle)
       (search-forward "FOLDED-DRAWER")
       (org-hide-drawer-toggle t)
       (search-forward "begin_src")
       (org-hide-block-toggle t)
       (goto-char 1)
       ,@body)))

(ert-deftest test-org-fold/org-catch-invisible-edits ()
  "Test invisible edits handling."
  ;; Disable delay in `org-fold-check-before-invisible-edit'.
  (cl-letf (((symbol-function 'sit-for) #'ignore))
    (dolist (org-fold-core-style '(text-properties overlays))
      (dolist (org-fold-catch-invisible-edits
               '(nil error smart show show-and-error))
        (dolist (kind '(insert delete-backward delete nil))
          (message "Testing invisible edits: %S:%S:%S"
                   org-fold-core-style
                   org-fold-catch-invisible-edits
                   kind)
          ;; Edits outside invisible.
          (test-org-fold-with-default-template
            (search-forward "Unfolded Paragraph")
            (message "Outside invisible")
            (org-fold-check-before-invisible-edit kind)
            (should-not (org-invisible-p)))
          ;; Edits inside invisible region.
          (test-org-fold-with-default-template
            (dolist (txt '("Folded Paragraph inside heading"
                           "Folded Paragraph inside drawer"
                           "Folded block"))
              (search-forward txt)
              (message "Inside invisible %S" txt)
              (pcase org-fold-catch-invisible-edits
                (`nil
                 (org-fold-check-before-invisible-edit kind)
                 (should (org-invisible-p)))
                (`show
                 (org-fold-check-before-invisible-edit kind)
                 (should-not (org-invisible-p)))
                ((or `smart `show-and-error)
                 (should-error (org-fold-check-before-invisible-edit kind))
                 (should-not (org-invisible-p)))
                (`error
                 (should-error (org-fold-check-before-invisible-edit kind))
                 (should (org-invisible-p)))))
            (search-forward "hiddenlink")
            (message "Inside hidden link")
            (org-fold-check-before-invisible-edit kind)
            (should (org-invisible-p)))
          ;; Edits at the left border.
          (test-org-fold-with-default-template
            (dolist (txt '("Folded heading"
                           ":FOLDED-DRAWER:"
                           "#+begin_src emacs-lisp"))
              (search-forward txt)
              (message "Left of folded %S" txt)
              (pcase org-fold-catch-invisible-edits
                (`nil
                 (org-fold-check-before-invisible-edit kind)
                 (should (org-invisible-p (1+ (point)))))
                (`show
                 (org-fold-check-before-invisible-edit kind)
                 (should-not (org-invisible-p (1+ (point)))))
                (`smart
                 (if (memq kind '(insert delete-backward))
                     (org-fold-check-before-invisible-edit kind)
                   (should-error (org-fold-check-before-invisible-edit kind)))
                 (should-not (org-invisible-p (1+ (point)))))
                (`show-and-error
                 (should-error (org-fold-check-before-invisible-edit kind))
                 (should-not (org-invisible-p (1+ (point)))))
                (`error
                 (should-error (org-fold-check-before-invisible-edit kind))
                 (should (org-invisible-p (1+ (point)))))))
            (search-forward "hiddenlink")
            (search-forward "lin")
            (message "Left border of ]] in link")
            (org-fold-check-before-invisible-edit kind)
            (should (org-invisible-p (1+ (point)))))
          ;; Edits at the right border.
          (test-org-fold-with-default-template
            (dolist (txt '("Folded Paragraph inside heading."
                           ":END:"
                           "#+end_src"))
              (search-forward txt)
              (message "After %S" txt)
              (pcase org-fold-catch-invisible-edits
                (`nil
                 (org-fold-check-before-invisible-edit kind)
                 (should (org-invisible-p (1- (point)))))
                (`show
                 (org-fold-check-before-invisible-edit kind)
                 (should-not (org-invisible-p (1- (point)))))
                ((or `smart `show-and-error)
                 (should-error (org-fold-check-before-invisible-edit kind))
                 (should-not (org-invisible-p (1- (point)))))
                (`error
                 (should-error (org-fold-check-before-invisible-edit kind))
                 (should (org-invisible-p (1- (point)))))))
            (search-forward "hiddenlink")
            (search-forward "link]]")
            (message "Right border of ]] in link")
            (org-fold-check-before-invisible-edit kind)
            (should (org-invisible-p (1- (point))))))))))

(ert-deftest test-org-fold/org-fold-display-inline-images ()
  "Test inline images displaying when cycling."
  (let* ((org-cycle-inline-images-display t)
         (images-dir (expand-file-name "examples/images/" org-test-dir))
         (org-logo-image (expand-file-name "Org mode logo mono-color.png" images-dir)))
    ;; `org-cycle' -(state)-> `'children' display child inline images.
    ;; TODO:
    
    ;; `org-cycle' -(state)-> `'subtree' display subtrees inline images.
    ;; TODO:
    
    ;; `org-cycle' -(state)-> `'folded' remove inline image overlays.
    (org-test-with-temp-text
        (format "<point>* Heading 1
[[file:%s]]
** Subheading 1
[[file:%s]]
** Subheading 2
[[file:%s]]" org-logo-image org-logo-image org-logo-image)
      (org-overview)
      (org-show-subtree)
      (org-fold-subtree t)
      (run-hook-with-args 'org-cycle-hook 'folded)
      (should (null org-inline-image-overlays))
      (should (null (overlays-in (point-min) (point-max))))
      (org-show-subtree)
      (should-not org-inline-image-overlays)
      (should-not (overlays-in (point-min) (point-max))))))

(provide 'test-org-fold)

;;; test-org-fold.el ends here
