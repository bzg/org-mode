;;; test-org-archive.el --- Test for Org Archive     -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019  Jay Kamat

;; Author: Jay Kamat <jaygkamat@gmail.com>

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


;;; Commentary:
;;

;;; Code:

(require 'org-test "../testing/org-test")

(require 'org-archive)

(ert-deftest test-org-archive/update-status-cookie ()
  "Test archiving properly updating status cookies."
  ;; Test org-archive-subtree with two children.
  (should
   (equal
    "Top [0%]"
    (org-test-with-temp-text-in-file
	"* Top [%]\n** DONE One\n** TODO Two"
      (forward-line)
      (org-archive-subtree)
      (forward-line -1)
      (org-element-property :raw-value (org-element-at-point)))))
  ;; Test org-archive-subtree with one child.
  (should
   (equal
    "Top [100%]"
    (org-test-with-temp-text-in-file "* Top [%]\n** TODO Two"
      (forward-line)
      (org-archive-subtree)
      (forward-line -1)
      (org-element-property :raw-value (org-element-at-point)))))
  ;; Test org-archive-to-archive-sibling with two children.
  (should
   (equal
    "Top [100%]"
    (org-test-with-temp-text "* Top [%]\n<point>** TODO One\n** DONE Two"
      (org-archive-to-archive-sibling)
      (forward-line -1)
      (org-element-property :raw-value (org-element-at-point)))))
  ;; Test org-archive-to-archive-sibling with two children.
  (should
   (equal
    "Top [0%]"
    (org-test-with-temp-text "* Top [%]\n<point>** DONE Two"
      (org-archive-to-archive-sibling)
      (forward-line -1)
      (org-element-property :raw-value (org-element-at-point))))))

(ert-deftest test-org-archive/datetree ()
  "Test `org-archive-subtree' with a datetree target."
  (org-test-at-time "<2020-07-05 Sun>"
    ;; Test in buffer target with no additional subheadings...
    (should
     (string-match-p
      (regexp-quote (format-time-string "*** 2020-07-05 %A\n**** a"))
      (org-test-with-temp-text-in-file "* a\n"
	(let ((org-archive-location "::datetree/"))
	  (org-archive-subtree)
	  (buffer-string)))))
    ;; ... and with `org-odd-levels-only' non-nil.
    (should
     (string-match-p
      (regexp-quote (format-time-string "***** 2020-07-05 %A\n******* a"))
      (org-test-with-temp-text-in-file "* a\n"
	(let ((org-archive-location "::datetree/")
	      (org-odd-levels-only t))
	  (org-archive-subtree)
	  (buffer-string)))))
    ;; Test in buffer target with an additional subheading...
    (should
     (string-match-p
      (regexp-quote (format-time-string "*** 2020-07-05 %A\n**** a\n***** b"))
      (org-test-with-temp-text-in-file "* b\n"
	(let ((org-archive-location "::datetree/* a"))
	  (org-archive-subtree)
	  (buffer-string)))))
    ;; ... and with `org-odd-levels-only' non-nil.
    (should
     (string-match-p
      (regexp-quote (format-time-string "***** 2020-07-05 %A\n******* a\n********* b"))
      (org-test-with-temp-text-in-file "* b\n"
	(let ((org-archive-location "::datetree/* a")
	      (org-odd-levels-only t))
	  (org-archive-subtree)
	  (buffer-string)))))))

(ert-deftest test-org-archive/context ()
  "Test that `org-archive-subtree' creates context info.
Context info is controlled by `org-archive-save-context-info'."
  (let ((org-archive-location "::* Archived Tasks"))
    (org-test-at-time "<2020-07-05 Sun>"
      (org-test-with-temp-text-in-file "* a\n"
        (should
         (string-equal
          (concat
           "* Archived Tasks

** a
:PROPERTIES:
:ARCHIVE_TIME: 2020-07-05 " (org-test-get-day-name "Sun") " 00:00\n"
           ":ARCHIVE_FILE: " buffer-file-name "\n"
           ":ARCHIVE_CATEGORY: " (file-name-nondirectory buffer-file-name) "\n"
           ":END:")
          (progn
            (org-archive-subtree)
            (string-trim (buffer-string))))))
      (org-test-with-temp-text-in-file "* a\n** b"
        (should
         (string-equal
          (concat "* Archived Tasks

** a
:PROPERTIES:
:ARCHIVE_TIME: 2020-07-05 " (org-test-get-day-name "Sun") " 00:00\n"
                  ":ARCHIVE_FILE: " buffer-file-name "\n"
                  ":ARCHIVE_CATEGORY: " (file-name-nondirectory buffer-file-name) "\n"
                  ":END:
*** b")
          (progn
            (org-archive-subtree)
            (string-trim (buffer-string))))))
      (org-test-with-temp-text-in-file "* a\n<point>** TODO b"
        (should
         (string-equal
          (concat
           "* a
* Archived Tasks

** TODO b
:PROPERTIES:
:ARCHIVE_TIME: 2020-07-05 " (org-test-get-day-name "Sun") " 00:00\n"
           ":ARCHIVE_FILE: " buffer-file-name "\n"
           ":ARCHIVE_OLPATH: a\n"
           ":ARCHIVE_CATEGORY: " (file-name-nondirectory buffer-file-name) "\n"
           ":ARCHIVE_TODO: TODO\n"
           ":END:")
          (progn
            (org-archive-subtree)
            (string-trim (buffer-string))))))
      (org-test-with-temp-text-in-file "* a\\q [/] slashes\n<point>** b"
        (should
         (string-equal
          (concat
           "* a\\q [/] slashes
* Archived Tasks

** b
:PROPERTIES:
:ARCHIVE_TIME: 2020-07-05 " (org-test-get-day-name "Sun") " 00:00\n"
           ":ARCHIVE_FILE: " buffer-file-name "\n"
           ":ARCHIVE_OLPATH: a\\q [/] slashes\n"
           ":ARCHIVE_CATEGORY: " (file-name-nondirectory buffer-file-name) "\n"

           ":END:")
          (progn
            (org-archive-subtree)
            (string-trim (buffer-string)))))))
    (let ((org-tags-column -10))
      (dolist (org-archive-save-context-info '((ltags) (itags) (ltags itags)))
        (org-test-with-temp-text-in-file "* a  :top:\n<point>** b  :b_tag:\n"
          (should
           (string-equal
            (concat "* a  :top:
* Archived Tasks

** b :top:b_tag:
:PROPERTIES:\n"
                    (if (memq 'ltags org-archive-save-context-info)
                        ":ARCHIVE_LTAGS: b_tag\n"
                      "")
                    (if (memq 'itags org-archive-save-context-info)
                        ":ARCHIVE_ITAGS: top\n"
                      "")
                    ":END:")
            (progn
              (org-archive-subtree)
              (string-trim (buffer-string))))))))))

(ert-deftest test-org-archive/to-archive-sibling ()
  "Test `org-archive-to-archive-sibling' specifications."
  ;; Archive sibling before or after archive heading.
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* H\n* Archive :ARCHIVE:\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* Archive :ARCHIVE:\n<point>* H\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  ;; When there is no sibling archive heading, create it.
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* H\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-tags-column 1))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  ;; Ignore non-sibling archive headings.
  (should
   (equal "* Archive :ARCHIVE:\n* Top\n** Archive :ARCHIVE:\n*** H\n"
	  (org-test-with-temp-text "* Archive :ARCHIVE:\n* Top\n<point>** H\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-tags-column 0))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 5))))))
  ;; When archiving a heading, leave point on next heading.
  (should
   (equal "* H2"
	  (org-test-with-temp-text "* H1\n* H2\n* Archive :ARCHIVE:\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (buffer-substring-no-properties (point) (line-end-position))))))
  (should
   (equal "* H2"
	  (org-test-with-temp-text "* Archive :ARCHIVE:\n<point>* H1\n* H2\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (buffer-substring-no-properties (point) (line-end-position))))))
  ;; If `org-archive-reversed-order' is nil, archive as the last
  ;; child.  Otherwise, archive as the first one.
  (should
   (equal "* Archive :ARCHIVE:\n** A\n"
	  (org-test-with-temp-text "* H\n* Archive :ARCHIVE:\n** A\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-archive-reversed-order nil))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* H\n* Archive :ARCHIVE:\n** A\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-archive-reversed-order t))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3)))))))

(provide 'test-org-archive)
;;; test-org-archive.el ends here
