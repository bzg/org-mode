;;; test-ol.el --- Tests for Org Links library       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nicolas Goaziou

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:


;;; (Un)Escape links

(ert-deftest test-ol/escape-ascii-character ()
  "Escape an ascii character."
  (should
   (string=
    "%5B"
    (org-link-escape "["))))

(ert-deftest test-ol/escape-ascii-ctrl-character ()
  "Escape an ascii control character."
  (should
   (string=
    "%09"
    (org-link-escape "\t"))))

(ert-deftest test-ol/escape-multibyte-character ()
  "Escape an unicode multibyte character."
  (should
   (string=
    "%E2%82%AC"
    (org-link-escape "€"))))

(ert-deftest test-ol/escape-custom-table ()
  "Escape string with custom character table."
  (should
   (string=
    "Foo%3A%42ar%0A"
    (org-link-escape "Foo:Bar\n" '(?\: ?\B)))))

(ert-deftest test-ol/escape-custom-table-merge ()
  "Escape string with custom table merged with default table."
  (should
   (string=
    "%5BF%6F%6F%3A%42ar%0A%5D"
    (org-link-escape "[Foo:Bar\n]" '(?\: ?\B ?\o) t))))

(ert-deftest test-ol/unescape-ascii-character ()
  "Unescape an ascii character."
  (should
   (string=
    "["
    (org-link-unescape "%5B"))))

(ert-deftest test-ol/unescape-ascii-ctrl-character ()
  "Unescpae an ascii control character."
  (should
   (string=
    "\n"
    (org-link-unescape "%0A"))))

(ert-deftest test-ol/unescape-multibyte-character ()
  "Unescape unicode multibyte character."
  (should
   (string=
    "€"
    (org-link-unescape "%E2%82%AC"))))

(ert-deftest test-ol/unescape-ascii-extended-char ()
  "Unescape old style percent escaped character."
  (should
   (string=
    "àâçèéêîôùû"
        (decode-coding-string
	 (org-link-unescape "%E0%E2%E7%E8%E9%EA%EE%F4%F9%FB") 'latin-1))))

(ert-deftest test-ol/escape-url-with-escaped-char ()
  "Escape and unescape a URL that includes an escaped char.
http://article.gmane.org/gmane.emacs.orgmode/21459/"
  (should
   (string=
    "http://some.host.com/form?&id=blah%2Bblah25"
    (org-link-unescape
     (org-link-escape "http://some.host.com/form?&id=blah%2Bblah25")))))


;;; Store links

(ert-deftest test-ol/store-link ()
  "Test `org-store-link' specifications."
  ;; On a headline, link to that headline.  Use heading as the
  ;; description of the link.
  (should
   (let (org-store-link-props org-stored-links)
     (org-test-with-temp-text-in-file "* H1"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::*H1][H1]]" file)
		(org-store-link nil))))))
  ;; On a headline, remove any link from description.
  (should
   (let (org-store-link-props org-stored-links)
     (org-test-with-temp-text-in-file "* [[#l][d]]"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::*%s][d]]"
			file
			(org-link-escape "[[#l][d]]"))
		(org-store-link nil))))))
  (should
   (let (org-store-link-props org-stored-links)
     (org-test-with-temp-text-in-file "* [[l]]"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::*%s][l]]" file (org-link-escape "[[l]]"))
		(org-store-link nil))))))
  (should
   (let (org-store-link-props org-stored-links)
     (org-test-with-temp-text-in-file "* [[l1][d1]] [[l2][d2]]"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::*%s][d1 d2]]"
			file
			(org-link-escape "[[l1][d1]] [[l2][d2]]"))
		(org-store-link nil))))))
  ;; On a named element, link to that element.
  (should
   (let (org-store-link-props org-stored-links)
     (org-test-with-temp-text-in-file "#+NAME: foo\nParagraph"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::foo][foo]]" file)
		(org-store-link nil))))))
  ;; Store link to Org buffer, with context.
  (should
   (let ((org-stored-links nil)
	 (org-id-link-to-org-use-id nil)
	 (org-context-in-file-links t))
     (org-test-with-temp-text-in-file "* h1"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::*h1][h1]]" file)
		(org-store-link nil))))))
  ;; Store link to Org buffer, without context.
  (should
   (let ((org-stored-links nil)
	 (org-id-link-to-org-use-id nil)
	 (org-context-in-file-links nil))
     (org-test-with-temp-text-in-file "* h1"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s][file:%s]]" file file)
		(org-store-link nil))))))
  ;; C-u prefix reverses `org-context-in-file-links' in Org buffer.
  (should
   (let ((org-stored-links nil)
	 (org-id-link-to-org-use-id nil)
	 (org-context-in-file-links nil))
     (org-test-with-temp-text-in-file "* h1"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::*h1][h1]]" file)
		(org-store-link '(4)))))))
  ;; A C-u C-u does *not* reverse `org-context-in-file-links' in Org
  ;; buffer.
  (should
   (let ((org-stored-links nil)
	 (org-id-link-to-org-use-id nil)
	 (org-context-in-file-links nil))
     (org-test-with-temp-text-in-file "* h1"
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s][file:%s]]" file file)
		(org-store-link '(16)))))))
  ;; Store file link to non-Org buffer, with context.
  (should
   (let ((org-stored-links nil)
	 (org-context-in-file-links t))
     (org-test-with-temp-text-in-file "one\n<point>two"
       (fundamental-mode)
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::one]]" file)
		(org-store-link nil))))))
  ;; Store file link to non-Org buffer, without context.
  (should
   (let ((org-stored-links nil)
	 (org-context-in-file-links nil))
     (org-test-with-temp-text-in-file "one\n<point>two"
       (fundamental-mode)
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s][file:%s]]" file file)
	 	(org-store-link nil))))))
  ;; C-u prefix reverses `org-context-in-file-links' in non-Org
  ;; buffer.
  (should
   (let ((org-stored-links nil)
	 (org-context-in-file-links nil))
     (org-test-with-temp-text-in-file "one\n<point>two"
       (fundamental-mode)
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s::one]]" file)
		(org-store-link '(4)))))))
  ;; A C-u C-u does *not* reverse `org-context-in-file-links' in
  ;; non-Org buffer.
  (should
   (let ((org-stored-links nil)
	 (org-context-in-file-links nil))
     (org-test-with-temp-text-in-file "one\n<point>two"
       (fundamental-mode)
       (let ((file (buffer-file-name)))
	 (equal (format "[[file:%s][file:%s]]" file file)
	 	(org-store-link '(16))))))))


;;; Radio Targets

(ert-deftest test-ol/update-radio-target-regexp ()
  "Test `org-update-radio-target-regexp' specifications."
  ;; Properly update cache with no previous radio target regexp.
  (should
   (eq 'link
       (org-test-with-temp-text "radio\n\nParagraph\n\nradio"
	 (save-excursion (goto-char (point-max)) (org-element-context))
	 (insert "<<<")
	 (search-forward "o")
	 (insert ">>>")
	 (org-update-radio-target-regexp)
	 (goto-char (point-max))
	 (org-element-type (org-element-context)))))
  ;; Properly update cache with previous radio target regexp.
  (should
   (eq 'link
       (org-test-with-temp-text "radio\n\nParagraph\n\nradio"
	 (save-excursion (goto-char (point-max)) (org-element-context))
	 (insert "<<<")
	 (search-forward "o")
	 (insert ">>>")
	 (org-update-radio-target-regexp)
	 (search-backward "r")
	 (delete-char 5)
	 (insert "new")
	 (org-update-radio-target-regexp)
	 (goto-char (point-max))
	 (delete-region (line-beginning-position) (point))
	 (insert "new")
	 (org-element-type (org-element-context))))))


;;; Navigation

(ert-deftest test-ol/next-link ()
  "Test `org-next-link' specifications."
  ;; Move to any type of link.
  (should
   (equal "[[link]]"
	  (org-test-with-temp-text "foo [[link]]"
	    (org-next-link)
	    (buffer-substring (point) (line-end-position)))))
  (should
   (equal "http://link"
	  (org-test-with-temp-text "foo http://link"
	    (org-next-link)
	    (buffer-substring (point) (line-end-position)))))
  (should
   (equal "<http://link>"
	  (org-test-with-temp-text "foo <http://link>"
	    (org-next-link)
	    (buffer-substring (point) (line-end-position)))))
  ;; Ignore link at point.
  (should
   (equal "[[link2]]"
	  (org-test-with-temp-text "[[link1]] [[link2]]"
	    (org-next-link)
	    (buffer-substring (point) (line-end-position)))))
  ;; Ignore fake links.
  (should
   (equal "[[truelink]]"
	  (org-test-with-temp-text "foo\n: [[link]]\n[[truelink]]"
	    (org-next-link)
	    (buffer-substring (point) (line-end-position)))))
  ;; Do not move point when there is no link.
  (should
   (org-test-with-temp-text "foo bar"
     (org-next-link)
     (bobp)))
  ;; Wrap around after a failed search.
  (should
   (equal "[[link]]"
	  (org-test-with-temp-text "[[link]]\n<point>foo"
	    (org-next-link)
	    (let* ((this-command 'org-next-link)
		   (last-command this-command))
	      (org-next-link))
	    (buffer-substring (point) (line-end-position))))))

(ert-deftest test-ol/previous-link ()
  "Test `org-previous-link' specifications."
  ;; Move to any type of link.
  (should
   (equal "[[link]]"
	  (org-test-with-temp-text "[[link]]\nfoo<point>"
	    (org-previous-link)
	    (buffer-substring (point) (line-end-position)))))
  (should
   (equal "http://link"
	  (org-test-with-temp-text "http://link\nfoo<point>"
	    (org-previous-link)
	    (buffer-substring (point) (line-end-position)))))
  (should
   (equal "<http://link>"
	  (org-test-with-temp-text "<http://link>\nfoo<point>"
	    (org-previous-link)
	    (buffer-substring (point) (line-end-position)))))
  ;; Ignore link at point.
  (should
   (equal "[[link1]]"
	  (org-test-with-temp-text "[[link1]]\n[[link2<point>]]"
	    (org-previous-link)
	    (buffer-substring (point) (line-end-position)))))
  ;; Ignore fake links.
  (should
   (equal "[[truelink]]"
	  (org-test-with-temp-text "[[truelink]]\n: [[link]]\n<point>"
	    (org-previous-link)
	    (buffer-substring (point) (line-end-position)))))
  ;; Do not move point when there is no link.
  (should
   (org-test-with-temp-text "foo bar<point>"
     (org-previous-link)
     (eobp)))
  ;; Wrap around after a failed search.
  (should
   (equal "[[link]]"
	  (org-test-with-temp-text "foo\n[[link]]"
	    (org-previous-link)
	    (let* ((this-command 'org-previous-link)
		   (last-command this-command))
	      (org-previous-link))
	    (buffer-substring (point) (line-end-position))))))

(provide 'test-ol)
;;; test-ol.el ends here
