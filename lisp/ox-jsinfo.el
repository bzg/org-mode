;;; ox-jsinfo.el --- Org HTML Export Extension for org-info.js

;; Copyright (C) 2004-2013 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements the support for Sebastian Rose's JavaScript
;; org-info.js to display an Org mode file exported to HTML in an
;; Info-like way, or using folding similar to the outline structure of
;; Org mode itself.

;; Documentation for using this module is in the Org manual.  The
;; script itself is documented by Sebastian Rose in a file distributed
;; with the script.  FIXME: Accurate pointers!

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-html)

(add-to-list 'org-export-filter-options-functions 'org-infojs-install-script)


(defgroup org-export-infojs nil
  "Options specific for using org-info.js in HTML export."
  :tag "Org Export HTML INFOJS"
  :group 'org-export-html)

(defcustom org-export-html-use-infojs 'when-configured
  "Non-nil when Sebastian Rose's Java Script org-info.js should be active.
This option can be nil or t to never or always use the script.
It can also be the symbol `when-configured', meaning that the
script will be linked into the export file if and only if there
is a \"#+INFOJS_OPT:\" line in the buffer.  See also the variable
`org-infojs-options'."
  :group 'org-export-html
  :group 'org-export-infojs
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When configured in buffer" when-configured)
	  (const :tag "Always" t)))

(defconst org-infojs-opts-table
  '((path PATH "http://orgmode.org/org-info.js")
    (view VIEW "info")
    (toc TOC :with-toc)
    (ftoc FIXED_TOC "0")
    (tdepth TOC_DEPTH "max")
    (sdepth SECTION_DEPTH "max")
    (mouse MOUSE_HINT "underline")
    (buttons VIEW_BUTTONS "0")
    (ltoc LOCAL_TOC "1")
    (up LINK_UP :html-link-up)
    (home LINK_HOME :html-link-home))
  "JavaScript options, long form for script, default values.")

(defvar org-infojs-options)
(when (and (boundp 'org-infojs-options)
	   (assq 'runs org-infojs-options))
  (setq org-infojs-options (delq (assq 'runs org-infojs-options)
				 org-infojs-options)))

(defcustom org-infojs-options
  (mapcar (lambda (x) (cons (car x) (nth 2 x))) org-infojs-opts-table)
  "Options settings for the INFOJS JavaScript.
Each of the options must have an entry in `org-export-html/infojs-opts-table'.
The value can either be a string that will be passed to the script, or
a property.  This property is then assumed to be a property that is defined
by the Export/Publishing setup of Org.
The `sdepth' and `tdepth' parameters can also be set to \"max\", which
means to use the maximum value consistent with other options."
  :group 'org-export-infojs
  :type
  `(set :greedy t :inline t
	,@(mapcar
	   (lambda (x)
	     (list 'cons (list 'const (car x))
		   '(choice
		     (symbol :tag "Publishing/Export property")
		     (string :tag "Value"))))
	   org-infojs-opts-table)))

(defcustom org-infojs-template
  "<script type=\"text/javascript\" src=\"%SCRIPT_PATH\">
/**
 *
 * @source: %SCRIPT_PATH
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in %SCRIPT_PATH.
 *
 * Copyright (C) 2012-2013  Sebastian Rose
 *
 *
 * The JavaScript code in this tag is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in %SCRIPT_PATH.
 *
 */
</script>

<script type=\"text/javascript\">

/*
@licstart  The following is the entire license notice for the
JavaScript code in this tag.

Copyright (C) 2012-2013 Free Software Foundation, Inc.

The JavaScript code in this tag is free software: you can
redistribute it and/or modify it under the terms of the GNU
General Public License (GNU GPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option)
any later version.  The code is distributed WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

As additional permission under GNU GPL version 3 section 7, you
may distribute non-source (e.g., minimized or compacted) forms of
that code without the copy of the GNU GPL normally required by
section 4, provided you include this license notice and a URL
through which recipients can access the Corresponding Source.


@licend  The above is the entire license notice
for the JavaScript code in this tag.
*/

<!--/*--><![CDATA[/*><!--*/
%MANAGER_OPTIONS
org_html_manager.setup();  // activate after the parameters are set
/*]]>*///-->
</script>"
  "The template for the export style additions when org-info.js is used.
Option settings will replace the %MANAGER-OPTIONS cookie."
  :group 'org-infojs
  :type 'string)

(defun org-infojs-install-script (exp-plist backend)
  "Install script in export options when appropriate.
EXP-PLIST is a plist containing export options.  BACKEND is the
export back-end currently used."
  (unless (or (not (org-export-derived-backend-p backend 'html))
	      (not org-export-html-use-infojs)
	      (and (eq org-export-html-use-infojs 'when-configured)
		   (or (not (plist-get exp-plist :infojs-opt))
		       (string-match "\\<view:nil\\>"
				     (plist-get exp-plist :infojs-opt)))))
    (let* ((template org-infojs-template)
	   (ptoc (plist-get exp-plist :with-toc))
	   (hlevels (plist-get exp-plist :headline-levels))
	   (sdepth hlevels)
	   (tdepth (if (integerp ptoc) (min ptoc hlevels) hlevels))
	   (options (plist-get exp-plist :infojs-opt))
	   (table org-infojs-opts-table)
	   style)
      (dolist (entry table)
	(let* ((opt (car entry))
	       (var (nth 1 entry))
	       ;; Compute default values for script option OPT from
	       ;; `org-infojs-options' variable.
	       (default
		 (let ((default (cdr (assq opt org-infojs-options))))
		   (if (and (symbolp default) (not (memq default '(t nil))))
		       (plist-get exp-plist default)
		     default)))
	       ;; Value set through INFOJS_OPT keyword has precedence
	       ;; over the default one.
	       (val (if (and options
			     (string-match (format "\\<%s:\\(\\S-+\\)" opt)
					   options))
			(match-string 1 options)
		      default)))
	  (case opt
	    (path (setq template
			(replace-regexp-in-string
			 "%SCRIPT_PATH" val template t t)))
	    (sdepth (when (integerp (read val))
		      (setq sdepth (min (read val) sdepth))))
	    (tdepth (when (integerp (read val))
		      (setq tdepth (min (read val) tdepth))))
	    (otherwise (setq val
			     (cond
			      ((or (eq val t) (equal val "t")) "1")
			      ((or (eq val nil) (equal val "nil")) "0")
			      ((stringp val) val)
			      (t (format "%s" val))))
		       (push (cons var val) style)))))
      ;; Now we set the depth of the *generated* TOC to SDEPTH,
      ;; because the toc will actually determine the splitting.  How
      ;; much of the toc will actually be displayed is governed by the
      ;; TDEPTH option.
      (setq exp-plist (plist-put exp-plist :with-toc sdepth))
      ;; The table of contents should not show more sections than we
      ;; generate.
      (setq tdepth (min tdepth sdepth))
      (push (cons "TOC_DEPTH" tdepth) style)
      ;; Build style string.
      (setq style (mapconcat
		   (lambda (x) (format "org_html_manager.set(\"%s\", \"%s\");"
				  (car x)
				  (cdr x)))
		   style "\n"))
      (when (and style (> (length style) 0))
	(and (string-match "%MANAGER_OPTIONS" template)
	     (setq style (replace-match style t t template))
	     (setq exp-plist
		   (plist-put
		    exp-plist :html-style-extra
		    (concat (or (plist-get exp-plist :html-style-extra) "")
			    "\n"
			    style)))))
      ;; This script absolutely needs the table of contents, so we
      ;; change that setting.
      (unless (plist-get exp-plist :with-toc)
	(setq exp-plist (plist-put exp-plist :with-toc t)))
      ;; Return the modified property list.
      exp-plist)))



(provide 'ox-infojs)
(provide 'ox-jsinfo)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-jsinfo.el ends here
