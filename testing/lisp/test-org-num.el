;;; test-org-num.el --- Tests for Org Num library    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;;; Code:

(require 'org-num)

(ert-deftest test-org-num/face ()
  "Test `org-num-face' parameter."
  (should
   (equal
    '(foo)
    (org-test-with-temp-text "* H1"
      (let ((org-num-face 'foo)) (org-num-mode 1))
      (mapcar (lambda (o)
		(get-text-property 0 'face (overlay-get o 'after-string)))
	      (overlays-in (point-min) (point-max)))))))

(ert-deftest test-org-num/format-function ()
  "Test `org-num-format-function' parameter."
  (should
   (equal '("foo" "foo")
          (org-test-with-temp-text "* H1\n** H2"
            (let ((org-num-format-function (lambda (_) "foo")))
              (org-num-mode 1))
            (mapcar (lambda (o) (overlay-get o 'after-string))
                    (overlays-in (point-min) (point-max))))))
  ;; Preserve face, when set.
  (should
   (equal-including-properties
    '(#("foo" 0 3 (face bar)))
    (org-test-with-temp-text "* H1"
      (let ((org-num-format-function
             (lambda (_) (org-add-props "foo" nil 'face 'bar))))
        (org-num-mode 1))
      (mapcar (lambda (o) (overlay-get o 'after-string))
              (overlays-in (point-min) (point-max))))))
  ;; Set face override `org-num-face'.
  (should
   (equal-including-properties
    '(#("foo" 0 3 (face bar)))
    (org-test-with-temp-text "* H1"
      (let ((org-num-face 'baz)
            (org-num-format-function
             (lambda (_) (org-add-props "foo" nil 'face 'bar))))
        (org-num-mode 1))
      (mapcar (lambda (o) (overlay-get o 'after-string))
              (overlays-in (point-min) (point-max)))))))

(ert-deftest test-org-num/max-level ()
  "Test `org-num-max-level' option."
  (should
   (equal (sort '("1.1 " "1 ") #'string-lessp)
          (org-test-with-temp-text "* H1\n** H2\n*** H3"
            (let ((org-num-max-level 2)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp)))))

(ert-deftest test-org-num/skip-numbering ()
  "Test various skip numbering parameters."
  ;; Skip commented headlines.
  (should
   (equal (sort '(nil "1 ") #'string-lessp)
          (org-test-with-temp-text "* H1\n* COMMENT H2"
            (let ((org-num-skip-commented t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("2 " "1 ") #'string-lessp)
          (org-test-with-temp-text "* H1\n* COMMENT H2"
            (let ((org-num-skip-commented nil)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Skip commented sub-trees.
  (should
   (equal (sort '(nil nil) #'string-lessp)
          (org-test-with-temp-text "* COMMENT H1\n** H2"
            (let ((org-num-skip-commented t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Skip footnotes sections.
  (should
   (equal (sort '(nil "1 ") #'string-lessp)
          (org-test-with-temp-text "* H1\n* FN"
            (let ((org-num-skip-footnotes t)
                  (org-footnote-section "FN"))
              (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("2 " "1 ") #'string-lessp)
          (org-test-with-temp-text "* H1\n* FN"
            (let ((org-num-skip-footnotes nil)
                  (org-footnote-section "FN"))
              (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Skip tags, recursively.
  (should
   (equal (sort '(nil "1 ") #'string-lessp)
          (org-test-with-temp-text "* H1\n* H2 :foo:"
            (let ((org-num-skip-tags '("foo"))) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '(nil nil) #'string-lessp)
          (org-test-with-temp-text "* H1 :foo:\n** H2"
            (let ((org-num-skip-tags '("foo"))) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Skip unnumbered sections.
  (should
   (equal (sort '(nil "1 ") #'string-lessp)
          (org-test-with-temp-text
              "* H1\n* H2\n:PROPERTIES:\n:UNNUMBERED: t\n:END:"
            (let ((org-num-skip-unnumbered t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("2 " "1 ") #'string-lessp)
          (org-test-with-temp-text
              "* H1\n* H2\n:PROPERTIES:\n:UNNUMBERED: t\n:END:"
            (let ((org-num-skip-unnumbered nil)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("2 " "1 ") #'string-lessp)
          (org-test-with-temp-text
              "* H1\n* H2\n:PROPERTIES:\n:UNNUMBERED: nil\n:END:"
            (let ((org-num-skip-unnumbered t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Skip unnumbered sub-trees.
  (should
   (equal (sort '(nil nil) #'string-lessp)
          (org-test-with-temp-text
              "* H1\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n** H2"
            (let ((org-num-skip-unnumbered t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Do not choke on empty headlines.
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text "* "
            (let ((org-num-skip-commented t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text "* "
            (let ((org-num-skip-unnumbered t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text "* "
            (let ((org-num-skip-footnotes t)) (org-num-mode 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp)))))

(ert-deftest test-org-num/update ()
  "Test numbering update after a buffer modification."
  ;; Headlines created at BEG.
  (should
   (equal "1 "
          (org-test-with-temp-text "X* H"
            (org-num-mode 1)
            (delete-char 1)
            (overlay-get (car (overlays-at (line-beginning-position)))
                         'after-string))))
  (should
   (equal "1 "
          (org-test-with-temp-text "*<point>\n H"
            (org-num-mode 1)
            (delete-char 1)
            (overlay-get (car (overlays-at (line-beginning-position)))
                         'after-string))))
  (should
   (equal "1 "
          (org-test-with-temp-text "*<point>bold*"
            (org-num-mode 1)
            (insert " ")
            (overlay-get (car (overlays-at (line-beginning-position)))
                         'after-string))))
  ;; Headlines created at END.
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text "X<point> H"
            (org-num-mode 1)
            (insert "\n*")
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text "X<point>* H"
            (org-num-mode 1)
            (insert "\n")
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Headlines created between BEG and END.
  (should
   (equal (sort '("1.1 " "1 ") #'string-lessp)
          (org-test-with-temp-text ""
            (org-num-mode 1)
            (insert "\n* H\n** H2")
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Change level of a headline.
  (should
   (equal (sort '("0.1 ") #'string-lessp)
          (org-test-with-temp-text "* H"
            (org-num-mode 1)
            (insert "*")
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text "*<point>* H"
            (org-num-mode 1)
            (delete-char 1)
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Alter skip state.
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text "* H :fo<point>o:"
            (let ((org-num-skip-tags '("foo")))
              (org-num-mode 1)
              (delete-char 1))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  (should
   (equal (sort '(nil) #'string-lessp)
          (org-test-with-temp-text "* H :fo<point>:"
            (let ((org-num-skip-tags '("foo")))
              (org-num-mode 1)
              (insert "o"))
            (sort
             (mapcar (lambda (o) (overlay-get o 'after-string))
                     (overlays-in (point-min) (point-max)))
             #'string-lessp))))
  ;; Invalidate an overlay and insert new headlines.
  (should
   (equal (sort '("1.2 " "1.1 " "1 ") #'string-lessp)
          (org-test-with-temp-text
              "* H\n:PROPERTIES:\n:UNNUMBE<point>RED: t\n:END:"
            (let ((org-num-skip-unnumbered t))
              (org-num-mode 1)
              (insert "\n** H2\n** H3\n")
              (sort
               (mapcar (lambda (o) (overlay-get o 'after-string))
                       (overlays-in (point-min) (point-max)))
               #'string-lessp)))))
  ;; Invalidate two overlays: current headline and next one.
  (should
   (equal (sort '("1 ") #'string-lessp)
          (org-test-with-temp-text
              "* H\n:PROPERTIES:\n:UNNUMBE<point>RED: t\n:END:\n** H2"
            (let ((org-num-skip-unnumbered t))
              (org-num-mode 1)
              (delete-region (point) (line-beginning-position 3))
              (sort
               (mapcar (lambda (o) (overlay-get o 'after-string))
                       (overlays-in (point-min) (point-max)))
               #'string-lessp))))))

(provide 'test-org-num)
;;; org-test-num.el ends here
