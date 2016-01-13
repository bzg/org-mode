;;; test-org-capture.el --- Tests for org-capture.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nicolas Goaziou

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
   (equal
    "5 % Less (See Item \"3)\" Somewhere)\n"
    (let ((org-store-link-plist nil))
      (org-capture-fill-template
       "%(capitalize \"%i\")"
       "5 % less (see item \"3)\" somewhere)")))))




(provide 'test-org-capture)
;;; test-org-capture.el ends here
