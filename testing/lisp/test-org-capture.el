;;; test-org-capture.el --- Tests for org-capture.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017  Nicolas Goaziou

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

;;; Commentary:

;; Unit tests for Org Capture library.

;;; Code:

(require 'org-capture)

(ert-deftest test-org-capture/fill-template ()
  "Test `org-capture-fill-template' specifications."

  ;; When working on these tests consider to also change
  ;; `test-org-feed/fill-template'.

  ;; %(sexp) placeholder.
  (should
   (equal "success!\n"
	  (org-capture-fill-template "%(concat \"success\" \"!\")")))
  ;; It is possible to include other place holders in %(sexp).  In
  ;; that case properly escape \ and " characters.
  (should
   (equal "Nested string \"\\\"\\\"\"\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%(concat \"%i\")"
				       "Nested string \"\\\"\\\"\""))))
  ;; %<...> placeholder.
  (should
   (equal (concat (format-time-string "%Y") "\n")
	  (org-capture-fill-template "%<%Y>")))
  ;; %t and %T placeholders.
  (should
   (equal (concat (format-time-string (org-time-stamp-format nil nil)) "\n")
	  (org-capture-fill-template "%t")))
  (should
   (equal (concat (format-time-string (org-time-stamp-format t nil)) "\n")
	  (org-capture-fill-template "%T")))
  ;; %u and %U placeholders.
  (should
   (equal
    (concat (format-time-string (org-time-stamp-format nil t)) "\n")
    (org-capture-fill-template "%u")))
  (should
   (equal
    (concat (format-time-string (org-time-stamp-format t t)) "\n")
    (org-capture-fill-template "%U")))
  ;; %i placeholder.  Make sure sexp placeholders are not expanded
  ;; when they are inserted through this one.
  (should
   (equal "success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%i" "success!"))))
  (should
   (equal "%(concat \"no \" \"evaluation\")\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template
	     "%i" "%(concat \"no \" \"evaluation\")"))))
  ;; When %i contents span over multiple line, repeat initial leading
  ;; characters over each line.
  (should
   (equal "> line 1\n> line 2\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "> %i" "line 1\nline 2"))))
  ;; Test %-escaping with \ character.
  (should
   (equal "%i\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\%i" "success!"))))
  (should
   (equal "\\success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\\\%i" "success!"))))
  (should
   (equal "\\%i\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\\\\\%i" "success!"))))
  ;; More than one placeholder in the same template.
  (should
   (equal "success! success! success! success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%i %i %i %i" "success!"))))
  ;; %(sexp) placeholder with an input containing the traps %, " and )
  ;; all at once which is complicated to parse.
  (should
   (equal "5 % Less (See Item \"3)\" Somewhere)\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template
	     "%(capitalize \"%i\")"
	     "5 % less (see item \"3)\" somewhere)")))))

(ert-deftest test-org-capture/refile ()
  "Test `org-capture-refile' specifications."
  ;; When refiling, make sure the headline being refiled is the one
  ;; being captured.  In particular, empty lines after the entry may
  ;; be removed, and we don't want to shift onto the next heading.
  (should
   (string-prefix-p
    "** H1"
    (org-test-with-temp-text-in-file "* A\n* B\n"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Todo" entry (file+headline ,file "A") "** H1 %?"))))
	(org-capture nil "t")
	(insert "\n")
	(cl-letf (((symbol-function 'org-refile)
		   (lambda ()
		     (interactive)
		     (throw :return
			    (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))))))
	  (catch :return (org-capture-refile)))))))
  ;; When the entry is refiled, `:jump-to-captured' moves point to the
  ;; refile location, not the initial capture target.
  (should
   (org-test-with-temp-text-in-file "* Refile target"
     (let ((file1 (buffer-file-name)))
       (org-test-with-temp-text-in-file "* A"
	 (let* ((file2 (buffer-file-name))
		(org-capture-templates
		 `(("t" "Todo" entry (file+headline ,file2 "A")
		    "** H1 %?" :jump-to-captured t))))
	   (org-capture nil "t")
	   (cl-letf (((symbol-function 'org-refile-get-location)
		      (lambda (&rest args)
			(list (file-name-nondirectory file1) file1 nil nil))))
	     (org-capture-refile)
	     (list file1 file2 (buffer-file-name)))))))))


(provide 'test-org-capture)
;;; test-org-capture.el ends here
