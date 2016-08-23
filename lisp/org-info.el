;;; org-info.el --- Support for Links to Info Nodes -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2016 Free Software Foundation, Inc.

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

;; This file implements links to Info nodes from within Org-mode.
;; Org mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

;; Declare external functions and variables

(declare-function Info-find-node "info"
                  (filename nodename &optional no-going-back strict-case))
(defvar Info-current-file)
(defvar Info-current-node)

;; Install the link type
(org-link-set-parameters "info"
			 :follow #'org-info-open
			 :export #'org-info-export
			 :store #'org-info-store-link)

;; Implementation
(defun org-info-store-link ()
  "Store a link to an Info file and node."
  (when (eq major-mode 'Info-mode)
    (let ((link (concat "info:"
			(file-name-nondirectory Info-current-file)
			"#" Info-current-node))
	  (desc (concat (file-name-nondirectory Info-current-file)
			"#" Info-current-node)))
      (org-store-link-props :type "info" :file Info-current-file
			    :node Info-current-node
			    :link link :desc desc)
      link)))

(defun org-info-open (path)
  "Follow an Info file and node link specified by PATH."
  (org-info-follow-link path))


(defun org-info-follow-link (name)
  "Follow an Info file and node link specified by NAME."
  (if (or (string-match "\\(.*\\)[#:]:?\\(.*\\)" name)
          (string-match "\\(.*\\)" name))
      (let ((filename (match-string 1 name))
	    (nodename-or-index (or (match-string 2 name) "Top")))
	(require 'info)
	;; If nodename-or-index is invalid node name, then look it up
	;; in the index.
	(condition-case nil
	    (Info-find-node filename nodename-or-index)
	  (user-error (Info-find-node filename "Top")
		      (condition-case nil
			  (Info-index nodename-or-index)
			(user-error "Could not find '%s' node or index entry"
				    nodename-or-index)))))
    (user-error "Could not open: %s" name)))

(defconst org-info-emacs-documents
  '("ada-mode" "auth" "autotype" "bovine" "calc" "ccmode" "cl" "dbus" "dired-x"
    "ebrowse" "ede" "ediff" "edt" "efaq-w32" "efaq" "eieio" "eintr" "elisp"
    "emacs-gnutls" "emacs-mime" "emacs" "epa" "erc" "ert" "eshell" "eudc" "eww"
    "flymake" "forms" "gnus" "htmlfontify" "idlwave" "ido" "info" "mairix-el"
    "message" "mh-e" "newsticker" "nxml-mode" "octave-mode" "org" "pcl-cvs"
    "pgg" "rcirc" "reftex" "remember" "sasl" "sc" "semantic" "ses" "sieve"
    "smtpmail" "speedbar" "srecode" "todo-mode" "tramp" "url" "vip" "viper"
    "widget" "wisent" "woman")
  "List of emacs documents available.
Taken from <http://www.gnu.org/software/emacs/manual/html_mono/.>")

(defconst org-info-other-documents
  '(("libc" . "http://www.gnu.org/software/libc/manual/html_mono/libc.html")
    ("make" . "http://www.gnu.org/software/make/manual/make.html"))
  "Alist of documents generated from texinfo source.

When converting info links to html, links to any one of these manuals are
converted to use these URL's.")

(defun org-info-map-html-url (filename)
  "Given info FILENAME, either return it (plus '.html' suffix added) or convert
it to URL pointing to the official page on internet, e.g., use gnu.org for all
emacs related documents. See `org-info-official-gnu-document' and
`org-info-other-documents' for details."
  (if (member filename org-info-emacs-documents)
      (format "http://www.gnu.org/software/emacs/manual/html_mono/%s.html"
              filename)
    (let ((url (cdr (assoc filename org-info-other-documents))))
      (or url (concat filename ".html")))))

(defun org-info-export (path desc format)
  "Export an info link.
See `org-link-parameters' for details about PATH, DESC and FORMAT."
  (when (eq format 'html)
    (or (string-match "\\(.*\\)[#:]:?\\(.*\\)" path)
	(string-match "\\(.*\\)" path))
    (let ((filename (match-string 1 path))
	  (node (or (match-string 2 path) "Top")))
      (format "<a href=\"%s#%s\">%s</a>"
	      (org-info-map-html-url filename)
	      (replace-regexp-in-string " " "-" node)
	      (or desc path)))))

(provide 'org-info)

;;; org-info.el ends here
