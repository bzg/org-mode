;;; test-org-info.el --- Tests for "org-info.el"     -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019  Nicolas Goaziou

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

(ert-deftest test-org-info/export ()
  "Test `org-info-export' specifications."
  ;; Export to HTML.  Without node, refer to "Top".
  (should
   (equal (org-info-export "filename#node" nil 'html)
	  "<a href=\"filename.html#node\">filename#node</a>"))
  (should
   (equal (org-info-export "filename" nil 'html)
	  "<a href=\"filename.html#Top\">filename</a>"))
  ;; Directory index. Top anchor actually should not be added,
  ;; but it should be rather rare case to add special code path.
  (should
   (equal (org-info-export "dir" nil 'html)
	  "<a href=\"https://www.gnu.org/manual/manual.html#Top\">dir</a>"))
  ;; When exporting to HTML, ensure node names are expanded according
  ;; to (info "(texinfo) HTML Xref Node Name Expansion").
  (should
   (equal "_005f"
	  (let ((name (org-info-export "#_" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  (should
   (equal "_002d"
	  (let ((name (org-info-export "#-" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  (should
   (equal "A-node"
	  (let ((name (org-info-export "#A node" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  (should
   (equal "A-node-_002d_002d_002d-with-_005f_0027_0025"
	  (let ((name (org-info-export "#A  node --- with _'%" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  ;; Export to Texinfo.  Without a node name, refer to "Top".
  (should
   (equal (org-info-export "filename" nil 'texinfo)
	  "@ref{Top,,,filename,}"))
  (should
   (equal (org-info-export "filename#node" nil 'texinfo)
	  "@ref{node,,,filename,}"))
  ;; "Top" is preserved, "::" as node separator.
  (should
   (equal "@ref{Top,,,emacs,}"
          (org-info-export "emacs::Top" nil 'texinfo)))

  ;; Description.
  (should
   (equal "@ref{Top,Emacs,,emacs,}"
          (org-info-export "emacs" "Emacs" 'texinfo)))
  (should
   (equal "@ref{Destructuring with pcase Patterns,pcase-let,,emacs,}"
          (org-info-export "emacs#Destructuring with pcase Patterns"
                           "pcase-let" 'texinfo))))

(ert-deftest test-org-info/link-file-node ()
  "Test parse info links by `org-info--link-file-node'."
  (should (equal '("success" . "Hash Separator")
                 (org-info--link-file-node "success#Hash Separator")))
  ;; Other separators.
  (should (equal '("success" . "Single Colon Separator")
                 (org-info--link-file-node "success:Single Colon Separator")))
  (should (equal '("success" . "Double Colon Separator")
                 (org-info--link-file-node "success::Double Colon Separator")))
  (should (equal '("success" . "Hash Colon Separator")
                 (org-info--link-file-node "success#:Hash Colon Separator")))
  ;; Partial specification.
  (should (equal '("nodeless" . "Top")
                 (org-info--link-file-node "nodeless")))
  (should (equal '("dir" . "Top")
                 (org-info--link-file-node "")))
  (should (equal '("dir" . "Top")
                 (org-info--link-file-node nil)))
  ;; Feel free to change behavior of underspecified links,
  ;; the case is added to check that it does not signal some error.
  (should (equal '("dir" . "broken")
                 (org-info--link-file-node "#broken")))
  ;; Trailing separator.
  (should (equal '("trailing-hash" . "Top")
                 (org-info--link-file-node "trailing-hash#")))
  (should (equal '("trailing-single-colon" . "Top")
                 (org-info--link-file-node "trailing-single-colon:")))
  (should (equal '("trailing-double-colon" . "Top")
                 (org-info--link-file-node "trailing-double-colon::")))
  (should (equal '("trailing-hash-colon" . "Top")
                 (org-info--link-file-node "trailing-hash-colon#:")))
  ;; Trim spaces.
  (should (equal '("trim" . "Spaces")
                 (org-info--link-file-node " trim # Spaces \t"))))

(ert-deftest test-org-info/description-as-command ()
  "Test `org-info-description-as-command'."
  (let ((cases
         '(("info file" "info:file")
           ("info strip-top-hash" "info:strip-top-hash#Top")
           ("info strip-top-single-colon" "info:strip-top-single-colon:Top")
           ("info strip-top-double-colon" "info:strip-top-double-colon::Top")
           ("info \"(pass) Hash\"" "info:pass#Hash")
           ("info \"(pass) Double Colon\"" "info:pass:: Double Colon")
           ("info \"(info) Advanced\"" "info:info:Advanced")
           ("info \"(dir)\"" "info:")
           ;; It actually works as "(dir) Top", test that no errors is signalled.
           ("info \"(dir) Invalid\"" "info::Invalid")
           (nil "http://orgmode.org/index.html#Not-info-link"))))
    (dolist (expectation-input cases)
      (let ((expectation (car expectation-input))
            (input (cadr expectation-input)))
        (should (equal
                 expectation
                 (org-info-description-as-command input nil))))))
  (let ((cases
         '(("Override link" "info:ignored#Link" "Override link")
           ("Fallback description" "http://not.info/link" "Fallback description")
           ("Link is nil" nil "Link is nil"))))
        (dolist (expectation-input-desc cases)
      (let ((expectation (car expectation-input-desc))
            (input (cadr expectation-input-desc))
            (desc (nth 2 expectation-input-desc)))
        (should (equal
                 expectation
                 (org-info-description-as-command input desc)))))))

(provide 'test-org-info)
;;; test-org-info.el ends here
