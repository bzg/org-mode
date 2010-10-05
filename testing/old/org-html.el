;;;_ org-html/tests.el --- Tests for org-html

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ , Commentary:

;; 


;;;_ , Requires

(require 'org-html)

;;;_. Body
;;;_ , org-id testhelp
;;This would go into org-id/testhelp.el if there were such a file
(defconst org-id:thd:usual-id-locations 
   (org-id-alist-to-hash
      '(
	  ("file1" "id-1-in-file1")
	  ("file1" "id-2-in-file1")
	  ("file2" "id-1-in-file2")))
   
   "A stable id-locations table for testing purposes" )
;;;_  . Validation
(emt:deftest-3 org-id:thd:usual-id-locations
   (nil
      (progn
	 (emt:doc "Operation: Look up one of the keys we inserted.")
	 (emt:doc "Response: It has the value we gave it.")
	 (assert
	    (equal
	       (gethash "id-1-in-file1" org-id:thd:usual-id-locations)
	       "file1")))))

;;;_ , Config
;;;_  . org-html:thd:isolation
(defconst org-html:thd:isolation
   ;;Can't hope to capture all the org configuration any time soon,
   ;;but let's set it up to some degree.

   ;;It is generally better for this to remain constant than to try to
   ;;sync this with new versions.  To change it is effectively to
   ;;write new tests.
   '(let
       (
	  (org-export-html-inline-image-extensions
	     '("png" "jpeg" "jpg" "gif"))
	  (org-html-cvt-link-fn                    nil)
	  (org-export-first-hook                   nil)
	  (org-par-open                            t)
	  (org-url-encoding-use-url-hexify         nil)

	  ;;To control the org-id lookups
	  (org-id-locations
	     org-id:thd:usual-id-locations)

          ;;To control `org-default-export-plist'
	  (org-export-inbuffer-options-extra       nil)
						   
	  ;;To control `org-infile-export-plist'.  Set up for minimal
	  ;;export so we can more easily handle examples.  When
	  ;;specific behavior is to be tested, locally bind the
	  ;;controlling variable(s), don't change them here.
	  (org-export-html-link-up                 "")
	  (org-export-html-link-home               "")
	  (org-export-default-language             "en")
	  (org-export-page-keywords                "")
	  (org-export-page-description             "")
	  (org-display-custom-times                nil)
	  (org-export-headline-levels              100)
	  (org-export-with-section-numbers         nil)
	  (org-export-section-number-format '((("1" ".")) . ""))
	  (org-export-with-toc                     nil)
	  (org-export-preserve-breaks              nil)
	  (org-export-with-archived-trees          nil)
	  (org-export-with-emphasize               nil)
	  (org-export-with-sub-superscripts        nil)
	  (org-export-with-special-strings         nil)
	  (org-export-with-footnotes               nil)
	  (org-export-with-drawers                 nil)
	  (org-export-with-tags                    nil)
	  (org-export-with-todo-keywords           nil)
	  (org-export-with-priority                nil)
	  (org-export-with-TeX-macros              nil)
	  (org-export-with-LaTeX-fragments         nil)
	  (org-export-latex-listings               nil)
	  (org-export-skip-text-before-1st-heading nil)
	  (org-export-with-fixed-width             nil)
	  (org-export-with-timestamps              nil)
	  (org-export-author-info                  nil)
	  (org-export-email-info                   nil)
	  (org-export-creator-info                 nil)
	  (org-export-time-stamp-file              nil)
	  (org-export-with-tables                  nil)
	  (org-export-highlight-first-table-line   nil)
	  (org-export-html-style-include-default   nil)
	  (org-export-html-style-include-scripts   nil)
	  (org-export-html-style                   "")
	  (org-export-html-style-extra             "")
	  (org-agenda-export-html-style            "")
	  (org-export-html-link-org-files-as-html  nil)
	  (org-export-html-inline-images           nil)
	  (org-export-html-extension               "html")
	  (org-export-html-xml-declaration 
	     '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>") 
		 ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))
	  (org-export-html-table-tag 
	     "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">")
	  (org-export-html-expand                nil)
	  (org-export-html-with-timestamp        nil)
	  (org-export-publishing-directory       nil)
	  (org-export-html-preamble              nil)
	  (org-export-html-postamble             nil)
	  (org-export-html-auto-preamble         nil)
	  (org-export-html-auto-postamble        nil)
	  (user-full-name                        "Emtest user")
	  (user-mail-address                     "emtest-user@localhost.localdomain")
	  (org-export-select-tags                '("export"))
	  (org-export-exclude-tags               '("noexport"))
	  (org-export-latex-image-default-option nil)

	  (org-export-plist-vars
	     '(  
		 (:link-up                 nil         org-export-html-link-up)
		 (:link-home               nil         org-export-html-link-home)
		 (:language                nil         org-export-default-language)
		 (:keywords                nil         org-export-page-keywords)
		 (:description             nil         org-export-page-description)
		 (:customtime              nil         org-display-custom-times)
		 (:headline-levels         "H"         org-export-headline-levels)
		 (:section-numbers         "num"       org-export-with-section-numbers)
		 (:section-number-format   nil         org-export-section-number-format)
		 (:table-of-contents       "toc"       org-export-with-toc)
		 (:preserve-breaks         "\\n"       org-export-preserve-breaks)
		 (:archived-trees          nil         org-export-with-archived-trees)
		 (:emphasize               "*"         org-export-with-emphasize)
		 (:sub-superscript         "^"         org-export-with-sub-superscripts)
		 (:special-strings         "-"         org-export-with-special-strings)
		 (:footnotes               "f"         org-export-with-footnotes)
		 (:drawers                 "d"         org-export-with-drawers)
		 (:tags                    "tags"      org-export-with-tags)
		 (:todo-keywords           "todo"      org-export-with-todo-keywords)
		 (:priority                "pri"       org-export-with-priority)
		 (:TeX-macros              "TeX"       org-export-with-TeX-macros)
		 (:LaTeX-fragments         "LaTeX"     org-export-with-LaTeX-fragments)
		 (:latex-listings          nil         org-export-latex-listings)
		 (:skip-before-1st-heading "skip"      org-export-skip-text-before-1st-heading)
		 (:fixed-width             ":"         org-export-with-fixed-width)
		 (:timestamps              "<"         org-export-with-timestamps)
		 (:author-info             "author"    org-export-author-info)
		 (:email-info              "email"     org-export-email-info)
		 (:creator-info            "creator"   org-export-creator-info)
		 (:time-stamp-file         "timestamp" org-export-time-stamp-file)
		 (:tables                  "|"         org-export-with-tables)
		 (:table-auto-headline     nil         org-export-highlight-first-table-line)
		 (:style-include-default   nil         org-export-html-style-include-default)
		 (:style-include-scripts   nil         org-export-html-style-include-scripts)
		 (:style                   nil         org-export-html-style)
		 (:style-extra             nil         org-export-html-style-extra)
		 (:agenda-style            nil         org-agenda-export-html-style)
		 (:convert-org-links       nil         org-export-html-link-org-files-as-html)
		 (:inline-images           nil         org-export-html-inline-images)
		 (:html-extension          nil         org-export-html-extension)
		 (:xml-declaration         nil         org-export-html-xml-declaration)
		 (:html-table-tag          nil         org-export-html-table-tag)
		 (:expand-quoted-html      "@"         org-export-html-expand)
		 (:timestamp               nil         org-export-html-with-timestamp)
		 (:publishing-directory    nil         org-export-publishing-directory)
		 (:preamble                nil         org-export-html-preamble)
		 (:postamble               nil         org-export-html-postamble)
		 (:auto-preamble           nil         org-export-html-auto-preamble)
		 (:auto-postamble          nil         org-export-html-auto-postamble)
		 (:author                  nil         user-full-name)
		 (:email                   nil         user-mail-address)
		 (:select-tags             nil         org-export-select-tags)
		 (:exclude-tags            nil         org-export-exclude-tags)
		 (:latex-image-options     nil         org-export-latex-image-default-option)))
	  ))
   "Isolation let-form for org-html tests.

Isolation let-forms are intended to be included by
`:surrounders'.  They provide a known configuration and keep
tests from altering the outside state." )

;;;_ , Examples
(defconst org-html:thd:examples
   (emt:eg:define+ ;;xmp:tqu804919ze0
      ((project org)
	 (library html)
	 (subsection link-examples))
      (group
	 ((name only-path))
	 ;;No src-text because this arglist wouldn't be generated by
	 ;;org-export-as-html, though it might be created by custom link
	 ;;types.
	 (item ((type arglist))
	    '("" "foo" nil "desc" nil nil))
	 (item ((type link-text))
	    "<a href=\"foo\">desc</a>"))
      (group
	 ((name only-fragment))
	 ;;No src-text, same reason
	 (item ((type arglist))
	    '("" "" "bar" "desc" nil nil))
	 (item ((type link-text))
	    "<a href=\"#bar\">desc</a>"))
      (group
	 ((name all-3-parts))
	 (item ((type src-text))
	    "[[http:foo#bar][desc]]")
	 (item ((type arglist))
	    '("http" "foo" "bar" "desc" nil nil))
	 (item ((type link-text))
	    "<a href=\"http:foo#bar\">desc</a>"))

      ;;Filename has to be absolute to trigger substitution.
      (group
	 ((name subst-in-filename))
	 (item ((type src-text))
	    "[[file:/foo/unfoo/.././baz][desc]]")
	 (item ((type arglist))
	    '("file" "/foo/unfoo/.././baz" "" "desc" nil nil))
	 (item ((type link-text))
	    "<a href=\"file:/foo/baz\">desc</a>"))

      (group
	 ((name type=file))
	 (item ((type src-text))
	    "[[file:foo.txt][desc]]")
	 (item ((type arglist))
	    '("file" "foo.txt" "" "desc" nil nil))
	 (item ((type link-text))
	    "<a href=\"file:foo.txt\">desc</a>"))

      ;;We control what location id finds by controlling
      ;;`org-id-locations' in `org-html:thd:isolation'
      (group
	 ((name type=id))
	 (item ((type src-text))
	    "[[id:id-1-in-file1][desc]]")
	 (item ((type arglist))
	    '("" "file1" "id-1-in-file1" "desc" nil nil))
	 (item ((type link-text))
	    "<a href=\"file1#id-1-in-file1\">desc</a>"))
      (group
	 ((name type=ftp))
	 (item ((type src-text))
	    "[[ftp:foo.com][desc]]")
	 (item ((type arglist))
	    '("ftp" "foo.com" "" "desc" nil nil))
	 (item ((type link-text))
	    "<a href=\"ftp:foo.com\">desc</a>"))

      ;;Punt coderef, internal logic is too hairy, would have to control
      ;;`org-export-get-coderef-format'.

      ;;Punt custom links, would have to make a controlled
      ;;`org-link-protocols', which means identifying and binding every
      ;;variable that `org-add-link-type' alters, then binding
      ;;`org-link-protocols' to empty list, then calling
      ;;`org-add-link-type' (possibly for id as well)

      (group
	 ((name convertable))
	 (item ((type src-text))
	    "[[file:foo.org][desc]]")
	 (item ((type arglist))
	    '("file" "foo.org" nil "desc" nil nil))
	 (group
	    ((type link-text))
	    (item ((subname old-conversion))
	       "<a href=\"http:foo.html\">desc</a>")
	    (item ((subname new-conversion))
	       "<a href=\"xform:transformed-foo.org\">desc</a>")
	    (item ((subname no-conversion))
	       "<a href=\"file:foo.org\">desc</a>")))
      ))

;;;_ , Helpers

(defun org-html:th:cvt-fn (opt-plist type path)
   "Trivial URL transformer"
   (declare (ignored opt-plist))
   (list
      "xform"
      (concat "transformed-" path)))

(defun org-html:th:check-link-matches (expected)
   "Build a link text and check it against expected text.
Sensitive to emt:eg narrowing."
   
   (assert
      (equal
	 (apply #'org-html-make-link
	     '(:html-extension "html")
	    (emt:eg (type arglist)))
	 expected)
      t))


;;;_ , org-html-make-link
(emt:deftest-3 
   ((of 'org-html-make-link)
      (:surrounders
	 (list
	    org-html:thd:isolation
	     '(emt:eg:with org-html:thd:examples
		((project org)
		   (library html)
		   (subsection link-examples))))))
   
   (nil
      (emt:eg:map name name
	 (unless
	    (eq name 'convertable)  
	    (emt:doc "Proves: Example arglist gives the expected result.")
	    (org-html:th:check-link-matches
	       (emt:eg (type link-text))))))
   
   (nil
      (emt:eg:narrow ((name convertable))
	 (let
	    (
	       (org-export-html-link-org-files-as-html t)
	       (org-html-cvt-link-fn nil))
	    (emt:doc "Proves: Old org->html conversion works.")
	    (org-html:th:check-link-matches
		  (emt:eg (type link-text) (subname old-conversion))))))
   
   (nil
      (emt:eg:narrow ((name convertable))
	 (let
	    (
	       (org-export-html-link-org-files-as-html nil)
	       (org-html-cvt-link-fn #'org-html:th:cvt-fn))
	    (emt:doc "Proves: New file->url conversion works.")
	    (org-html:th:check-link-matches
	       (emt:eg (type link-text) (subname new-conversion))))))
   
   (nil
      (emt:eg:narrow ((name convertable))
	 (let
	    (
	       (org-export-html-link-org-files-as-html t)
	       (org-html-cvt-link-fn #'org-html:th:cvt-fn))
	    (emt:doc "Proves: New conversion has precedence over old.")
	    (org-html:th:check-link-matches
	       (emt:eg (type link-text) (subname new-conversion))))))
   


   ;;Add tests for making images - but it's nearly direct.

   )
;;;_ , Helpers
(defun org-html:th:build-source (type path fragment &optional desc
				   descp attr may-inline-p)
   ""
   (declare (ignored descp attr may-inline-p))
   (concat
      "[["type":"
      (org-link-escape path)
      (if fragment
	 (cond
	    ((string= type "file")(concat "::" fragment))
	    ((string= type "http")(concat "#" fragment))))
      "]["desc"]]"))
;;;_  . org-html:th:strip-whitepadding
(defun org-html:th:strip-whitepadding (str)
   ""
   
   (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
	 (replace-match ""))
      (goto-char (point-min))
      (while (search-forward "<p>" nil t)
	 (replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</p>" nil t)
	 (replace-match ""))
      (buffer-string)))
;;;_   , Examples
(defconst org-html:stripwhite:thd:examples
   (emt:eg:define+ ;;xmp:khpjmfi0aze0
      ((project org)(library html)
	 (subsection org-html:th:strip-whitepadding)
	 (type string)
	 (role before))
      (item ((name 0)) 
	 "\na\nb")
      (item ((name 1)) 
	 "\n<p>ab")
      (item ((name 2)) 
	 "\n</p>a\nb")))

;;;_   , Tests
(emt:deftest-3 org-html:th:strip-whitepadding
   (nil
      (emt:eg:with org-html:stripwhite:thd:examples
	 ((project org)(library html)
	    (subsection org-html:th:strip-whitepadding)) 
	 (emt:eg:map name name
	    (emt:doc 
	       "Check: The stripped string matches what's expected.")
	    (assert
	       (string=
		  (org-html:th:strip-whitepadding (emt:eg))
		  "ab"))))))

;;;_ , org-export-as-html

(emt:deftest-3 
   ((of 'org-export-as-html)
      (:surrounders
	 (list
	    org-html:thd:isolation
	    ;;Re-use the link examples.
	    '(emt:eg:with org-html:thd:examples
		((project org)(library html))))))
   (nil
      (emt:eg:narrow ((subsection link-examples)) 
	 (emt:eg:map name name
	    (when
	       (and
		  (not (eq name 'convertable))
		  ;;Dormant for id because it wants to find filename
		  ;;relative to `org-current-export-file', but for
		  ;;buffer export there is none.
		  (not (eq name 'type=id))  
		  (emt:eg:boundp '(type src-text)))
	       (emt:doc 
		  "Situation: the only thing in the buffer is that link")
	       (with-buffer-containing-object
 		  (:string
		     (emt:eg (type src-text)))
		  (org-mode)

		  ;;This calculation has to be done outside the assert
		  ;;or it will be done twice.
		  (emt:doc "Operation: export the buffer as HTML.")
		  (let
		     ((result
			 (org-html:th:strip-whitepadding
			    (org-export-region-as-html
			       (point-min)
			       (point-max)
			       t
			       'string))))
		     (emt:doc 
			"Proves: Example arglist gives the expected result.")
		     (assert
			(string=
			   result
			   (emt:eg (type link-text)))
			t)))))))
   ;;Could also use testpoints to test that we feed the link-builder
   ;;functions as expected.


   ;;Could also make example files and convert them.
   )



;;;_. Footers
;;;_ , Provides

(provide 'org-html/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org-html/tests.el ends here
