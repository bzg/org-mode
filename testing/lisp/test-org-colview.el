;;; test-org-colview.el --- Tests for org-colview.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;;; Column view

(require 'cl-lib)

(ert-deftest test-org-colview/columns-scope ()
  "Test `org-columns' scope."
  ;; Before the first headline, view all document.
  (should
   (equal
    '("H1" "H2" "H3")
    (org-test-with-temp-text "Top\n* H1\n** H2\n* H3"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  ;; When :COLUMNS: is set up in the hierarchy, view tree starting
  ;; there.
  (should
   (equal
    '(nil "H2" "H3" nil)
    (org-test-with-temp-text
	"* H1\n** H2\n:PROPERTIES:\n:COLUMNS: %ITEM\n:END:\n*** <point>H3\n* H4"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  ;; Otherwise, view tree starting at the current headline.
  (should
   (equal
    '(nil "H2" "H3" nil)
    (org-test-with-temp-text "Top\n* H1\n** <point>H2\n*** H3\n* H4"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  ;; With a non-nil prefix argument, always view all document.
  (should
   (equal
    '("H1" "H2" "H3" "H4")
    (org-test-with-temp-text
	"* H1\n** H2\n:PROPERTIES:\n:COLUMNS: %ITEM\n:END:\n*** <point>H3\n* H4"
      (let ((org-columns-default-format "%ITEM")) (org-columns t))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  (should
   (equal
    '("H1" "H2" "H3" "H4")
    (org-test-with-temp-text "Top\n* H1\n** <point>H2\n*** H3\n* H4"
      (let ((org-columns-default-format "%ITEM")) (org-columns t))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value)))))))

(ert-deftest test-org-colview/columns-width ()
  "Test `org-columns' column widths."
  ;; When a width is specified in the format, use it.
  (should
   (= 9
      (org-test-with-temp-text "* H"
	(let ((org-columns-default-format "%9ITEM")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; Otherwise, use the width of the largest value in the column.
  (should
   (= 2
      (org-test-with-temp-text
	  "* H\n:PROPERTIES:\n:P: X\n:END:\n** H2\n:PROPERTIES:\n:P: XX\n:END:"
	(let ((org-columns-default-format "%P")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; If the title is wider than the widest value, use title width
  ;; instead.
  (should
   (= 4
      (org-test-with-temp-text "* H"
	(let ((org-columns-default-format "%ITEM")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; Special case: stars do count for ITEM.
  (should
   (= 6
      (org-test-with-temp-text "* Head"
	(let ((org-columns-default-format "%ITEM")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; Special case: width takes into account link narrowing in ITEM.
  (should
   (equal
    '("* [123]" . 7)
    (org-test-with-temp-text "* [[http://orgmode.org][123]]"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (cons (get-char-property (point) 'org-columns-value-modified)
	    (aref org-columns-current-maxwidths 0)))))
  ;; When a value is too wide for the current column, add ellipses.
  ;; Take into consideration length of `org-columns-ellipses'.
  (should
   (equal "123.. |"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:P: 123456\n:END:"
	    (let ((org-columns-default-format "%5P")
		  (org-columns-ellipses ".."))
	      (org-columns))
	    (org-trim (get-char-property (point) 'display)))))
  (should
   (equal "1234… |"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:P: 123456\n:END:"
	    (let ((org-columns-default-format "%5P")
		  (org-columns-ellipses "…"))
	      (org-columns))
	    (org-trim (get-char-property (point) 'display))))))

(ert-deftest test-org-colview/columns-summary ()
  "Test `org-columns' summary types."
  ;; {+} and {+;format} add numbers.
  (should
   (equal
    "3"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{+}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "3.0"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{+;%.1f}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {$} is a shortcut for {+;%.2f}.
  (should
   (equal
    "3.60"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1.50
:END:
** S1
:PROPERTIES:
:A: 2.10
:END:"
      (let ((org-columns-default-format "%A{$}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {:} sums times.  Plain numbers are hours.
  (should
   (equal
    "4:10"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1:30
:END:
** S1
:PROPERTIES:
:A: 2:40
:END:"
      (let ((org-columns-default-format "%A{:}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "3:30"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1:30
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{:}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {X}, {X/} and {X%} indicate checkbox status.
  (should
   (equal
    "[ ]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [ ]
:END:"
      (let ((org-columns-default-format "%A{X}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[-]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[X]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [X]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[1/2]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X/}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[50%]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X%}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {min} is the smallest number in column, {max} the largest one.
  ;; {mean} is the arithmetic mean of numbers in column.
  (should
   (equal
    "42"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{min}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "99"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{max}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "51.0"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 60
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{mean}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {:min}, {:max} and {:mean} apply to time values.
  (should
   (equal
    "1:20"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:min}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "4:40"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:max}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "3:00"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:mean}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {@min}, {@max} and {@mean} apply to ages.
  (should
   (equal
    (let ((org-columns--time (float-time (current-time))))
      (org-columns--summary-min-age (list "<2014-03-04 Tue>") nil))
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: <2012-03-29 Thu>
:END:
** S1
:PROPERTIES:
:A: <2014-03-04 Tue>
:END:"
      (let ((org-columns-default-format "%A{@min}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    (let ((org-columns--time (float-time (current-time))))
      (org-columns--summary-max-age (list "<2012-03-29 Thu>") nil))
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: <2012-03-29 Thu>
:END:
** S1
:PROPERTIES:
:A: <2014-03-04 Tue>
:END:"
      (let ((org-columns-default-format "%A{@max}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    (let ((org-columns--time (float-time (current-time))))
      (org-columns--summary-mean-age
       (list "<2012-03-29 Thu>" "<2014-03-04 Tue>") nil))
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: <2012-03-29 Thu>
:END:
** S1
:PROPERTIES:
:A: <2014-03-04 Tue>
:END:"
      (let ((org-columns-default-format "%A{@mean}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; If a time value is expressed as a duration, return a duration.
  ;; If any of them follows H:MM:SS pattern, use it too.
  (should
   (equal
    "1d 4:20"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 3d 3h
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:}")
	    (org-time-clocksum-use-fractional nil)
	    (org-time-clocksum-format
	     '(:days "%dd " :hours "%d" :minutes ":%02d")))
	(org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "6:00:10"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40:10
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; @min, @max and @mean also accept regular duration in
  ;; a "?d ?h ?m ?s" format.
  (should
   (equal
    "1d 10h 0m 0s"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1d 10h 0m 0s
:END:
** S1
:PROPERTIES:
:A: 5d 3h 0m 0s
:END:"
      (let ((org-columns-default-format "%A{@min}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {est+} gives a low-high estimate using mean and standard
  ;; deviation.
  (should
   (equal
    "3-17"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 0-10
:END:
** S1
:PROPERTIES:
:A: 0-10
:END:"
      (let ((org-columns-default-format "%A{est+}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; When using {est+} summary, a single number is understood as
  ;; a degenerate range.
  (should
   (equal
    "4-4"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4
:END:
"
      (let ((org-columns-default-format "%A{est+}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; Allow custom summary types.
  (should
   (equal
    "1|2"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-summary-types
	     '(("custom" . (lambda (s _) (mapconcat #'identity s "|")))))
	    (org-columns-default-format "%A{custom}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; Allow multiple summary types applied to the same property.
  (should
   (equal
    '("42" "99")
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{min} %A{max}")) (org-columns))
      (list (get-char-property (point) 'org-columns-value-modified)
	    (get-char-property (1+ (point)) 'org-columns-value-modified)))))
  ;; Allow mixing both summarized and non-summarized columns for
  ;; a property.  However, the first column takes precedence and
  ;; updates the value.
  (should
   (equal
    '("1000" "42")
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1000
:END:
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A %A{min}")) (org-columns))
      (list (get-char-property (point) 'org-columns-value-modified)
	    (get-char-property (1+ (point)) 'org-columns-value-modified)))))
  (should
   (equal
    '("42" "42")
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1000
:END:
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{min} %A")) (org-columns))
      (list (get-char-property (point) 'org-columns-value-modified)
	    (get-char-property (1+ (point)) 'org-columns-value-modified))))))

(ert-deftest test-org-colview/columns-new ()
  "Test `org-columns-new' specifications."
  ;; Insert new column at the left of the current one.
  (should
   (equal '("FOO" "ITEM")
	  (org-test-with-temp-text "* H"
	    (let ((org-columns-default-format "%ITEM")) (org-columns))
	    (org-columns-new "FOO")
	    (list (get-char-property (point) 'org-columns-key)
		  (get-char-property (1+ (point)) 'org-columns-key)))))
  (should
   (equal '("ITEM" "FOO" "BAR")
	  (org-test-with-temp-text "* H"
	    (let ((org-columns-default-format "%ITEM %BAR")) (org-columns))
	    (forward-char)
	    (org-columns-new "FOO")
	    (list (get-char-property (1- (point)) 'org-columns-key)
		  (get-char-property (point) 'org-columns-key)
		  (get-char-property (1+ (point)) 'org-columns-key)))))
  ;; Update #+COLUMNS keyword if needed.
  (should
   (equal "#+COLUMNS: %FOO %ITEM"
	  (org-test-with-temp-text "#+COLUMNS: %ITEM\n<point>* H"
	    (let ((org-columns-default-format "%ITEM")) (org-columns))
	    (org-columns-new "FOO")
	    (goto-char (point-min))
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "#+COLUMNS: %ITEM %FOO %BAR"
	  (org-test-with-temp-text "#+COLUMNS: %ITEM %BAR\n<point>* H"
	    (let ((org-columns-default-format "%ITEM %BAR")) (org-columns))
	    (forward-char)
	    (org-columns-new "FOO")
	    (goto-char (point-min))
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Mind case when updating #+COLUMNS.
  (should
   (equal "#+COLUMNS: %ITEM %Foo %BAR"
	  (org-test-with-temp-text "#+COLUMNS: %ITEM %BAR\n<point>* H"
	    (let ((org-columns-default-format "%ITEM %BAR")) (org-columns))
	    (forward-char)
	    (org-columns-new "Foo")
	    (goto-char (point-min))
	    (buffer-substring-no-properties (point) (line-end-position))))))

(ert-deftest test-org-colview/columns-update ()
  "Test `org-columns-update' specifications."
  ;; Update display.
  (should
   (equal
    "12    |"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
"
      (let ((org-columns-default-format "%5A")) (org-columns))
      (search-forward "1")
      (insert "2")
      (org-columns-update "A")
      (get-char-property (point-min) 'display))))
  ;; Update is case-insensitive.
  (should
   (equal
    "12    |"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
"
      (let ((org-columns-default-format "%5A")) (org-columns))
      (search-forward "1")
      (insert "2")
      (org-columns-update "a")
      (get-char-property (point-min) 'display))))
  ;; Update stored values.
  (should
   (equal
    '("12" "12")
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
"
      (let ((org-columns-default-format "%5A")) (org-columns))
      (search-forward "1")
      (insert "2")
      (org-columns-update "A")
      (list (get-char-property (point-min) 'org-columns-value)
	    (get-char-property (point-min) 'org-columns-value-modified)))))
  ;; When multiple columns are using the same property, value is
  ;; updated according to the specifications of the first one.
  (should
   (equal
    "2"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
** S
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{min} %A")) (org-columns))
      (org-columns-update "A")
      (org-entry-get nil "A"))))
  (should
   (equal
    "1"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
** S
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A %A{min}")) (org-columns))
      (org-columns-update "A")
      (org-entry-get nil "A"))))
  ;; Ensure modifications propagate in upper levels even when multiple
  ;; summary types apply to the same property.
  (should
   (equal
    '("1" "22")
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S2
:PROPERTIES:
:A: <point>2
:END:"
      (save-excursion
	(goto-char (point-min))
	(let ((org-columns-default-format "%A{min} %A{max}")) (org-columns)))
      (insert "2")
      (org-columns-update "A")
      (list (get-char-property 1 'org-columns-value)
	    (get-char-property 2 'org-columns-value-modified)))))
  ;; Ensure additional processing is done (e.g., ellipses, special
  ;; keywords fontification...).
  (should
   (equal
    "ve.. |"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: text
:END:
"
      (let ((org-columns-default-format "%4A")
	    (org-columns-ellipses ".."))
	(org-columns))
      (search-forward ":A: ")
      (insert "very long ")
      (org-columns-update "A")
      (get-char-property (point-min) 'display))))
  ;; Values obtained from inline tasks are at the same level as those
  ;; obtained from children of the current node.
  (when (featurep 'org-inlinetask)
    (should
     (equal
      "2"
      (org-test-with-temp-text
	  "* H
*************** Inline task
:PROPERTIES:
:A: 2
:END:
*************** END
** Children
:PROPERTIES:
:A: 3
:END:
"
	(let ((org-columns-default-format "%A{min}")
	      (org-columns-ellipses "..")
	      (org-inlinetask-min-level 15))
	  (org-columns))
	(get-char-property (point-min) 'org-columns-value)))))
  ;; Handle `org-columns-modify-value-for-display-function', even with
  ;; multiple titles for the same property.
  (should
   (equal '("foo" "bar")
	  (org-test-with-temp-text "* H"
	    (let ((org-columns-default-format "%ITEM %ITEM(Name)")
		  (org-columns-modify-value-for-display-function
		   (lambda (title value)
		     (pcase title ("ITEM" "foo") ("Name" "bar") (_ "baz")))))
	      (org-columns))
	    (list (get-char-property 1 'org-columns-value-modified)
		  (get-char-property 2 'org-columns-value-modified))))))



;;; Dynamic block

(ert-deftest test-org-colview/dblock ()
  "Test the column view table."
  (should
   (equal
    "#+BEGIN: columnview
| ITEM |
|------|
| H    |
#+END:"
    (org-test-with-temp-text
        "* H\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  (should
   (equal
    "#+BEGIN: columnview
| ITEM | A |
|------+---|
| H    | 1 |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Properties are case insensitive.
  (should
   (equal
    "#+BEGIN: columnview
| a |
|---|
| 1 |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%a")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Test titles given to columns.
  (should
   (equal
    "#+BEGIN: columnview
| Name | Prop |
|------+------|
| H    |    1 |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM(Name) %A(Prop)"))
        (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Test `:id' parameter
  (should
   (equal
    "#+BEGIN: columnview :id local
| ITEM |
|------|
| H1   |
| H1.1 |
#+END:
"
    (org-test-with-temp-text
        "* H1\n<point>#+BEGIN: columnview :id local\n#+END:\n** H1.1\n* H2"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :id global
| ITEM |
|------|
| H1   |
| H1.1 |
| H2   |
#+END:
"
    (org-test-with-temp-text
        "\n* H1\n<point>#+BEGIN: columnview :id global\n#+END:\n** H1.1\n* H2"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:hlines' parameter.
  (should
   (equal
    "#+BEGIN: columnview :hlines t :id global
| ITEM |
|------|
| H    |
|------|
| H2   |
|------|
| H2.1 |
#+END:\n"
    (org-test-with-temp-text
        "
* H
<point>#+BEGIN: columnview :hlines t :id global
#+END:
* H2
** H2.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :hlines 1 :id global
| ITEM |
|------|
| H    |
|------|
| H2   |
| H2.1 |
#+END:\n"
    (org-test-with-temp-text
        "
* H
<point>#+BEGIN: columnview :hlines 1 :id global
#+END:
* H2
** H2.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:indent' parameter.
  (should
   (equal
    "#+BEGIN: columnview :indent t
| ITEM     |
|----------|
| H1       |
| \\_  H1.1 |
#+END:
"
    (org-test-with-temp-text
        "* H1\n<point>#+BEGIN: columnview :indent t\n#+END:\n** H1.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :indent t
| Prop | Name     |
|------+----------|
|      | H1       |
|      | \\_  H1.1 |
#+END:
"
    (org-test-with-temp-text
        "* H1\n<point>#+BEGIN: columnview :indent t\n#+END:\n** H1.1"
      (let ((org-columns-default-format "%A(Prop) %ITEM(Name)"))
        (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:vlines' parameter.
  (should
   (equal
    "#+BEGIN: columnview :vlines t
|   | ITEM | A  |
|---+------+----|
|   | H    | 1  |
| / | <>   | <> |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview :vlines t\n#+END:"
      (let ((org-columns-default-format "%ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Test `:skip-empty-rows' parameter.
  (should
   (equal
    "#+BEGIN: columnview :skip-empty-rows t
| ITEM | A |
|------+---|
| H1.1 | 1 |
#+END:
"
    (org-test-with-temp-text
        "
* H1
<point>#+BEGIN: columnview :skip-empty-rows t
#+END:
** H1.1
:PROPERTIES:
:A: 1
:END:"
      (let ((org-columns-default-format "%ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:format' parameter.
  (should
   (equal
    "#+BEGIN: columnview :format \"%ITEM(Name)\"
| Name |
|------|
| H    |
#+END:"
    (org-test-with-temp-text
        "* H\n<point>#+BEGIN: columnview :format \"%ITEM(Name)\"\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Test `:width' parameter
  (should
   (equal
    "#+BEGIN: columnview :width t
| ITEM       | A |
|------------+---|
| H          |   |
| <10>       |   |
#+END:"
    (org-test-with-temp-text
        "* H\n<point>#+BEGIN: columnview :width t\n#+END:"
      (let ((org-columns-default-format "%10ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; When inserting ITEM values, make sure to clean sensitive
  ;; contents, like unique targets or forbidden inline src-blocks.
  (should
   (equal
    "#+BEGIN: columnview
| ITEM |
|------|
| H 1  |
#+END:"
    (org-test-with-temp-text
        "* H <<target>> 1\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  (should
   (equal
    "#+BEGIN: columnview
| ITEM |
|------|
| H 1  |
#+END:"
    (org-test-with-temp-text
        "* H src_emacs-lisp{(+ 1 1)} 1\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max))))))

(provide 'test-org-colview)
;;; test-org-colview.el ends here
