;;; org-bbdb.el --- Support for links to BBDB entries from within Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 1.0
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to BBDB database entries from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

;; Declare external functions and variables

(declare-function bbdb "ext:bbdb-com" (string elidep))
(declare-function bbdb-company "ext:bbdb-com" (string elidep))
(declare-function bbdb-current-record "ext:bbdb-com"
		  (&optional planning-on-modifying))
(declare-function bbdb-name "ext:bbdb-com" (string elidep))
(declare-function bbdb-record-getprop "ext:bbdb" (record property))
(declare-function bbdb-record-name "ext:bbdb" (record))

;; Install the link type
(org-add-link-type "bbdb" 'org-bbdb-open 'org-bbdb-export)
(add-hook 'org-store-link-functions 'org-bbdb-store-link)

;; Implementation
(defun org-bbdb-store-link ()
  "Store a link to a BBDB database entry."
  (when (eq major-mode 'bbdb-mode)
    ;; This is BBDB, we make this link!
    (let* ((name (bbdb-record-name (bbdb-current-record)))
	   (company (bbdb-record-getprop (bbdb-current-record) 'company))
	   (link (org-make-link "bbdb:" name)))
      (org-store-link-props :type "bbdb" :name name :company company
			    :link link :description name)
      link)))

(defun org-bbdb-export (path desc format)
  "Create the export version of a BBDB link specified by PATH or DESC.
If exporting to either HTML or LaTeX FORMAT the link will be
italicised, in all other cases it is left unchanged."
  "Create the exprt verison of a bbdb link."
  (cond
   ((eq format 'html) (format "<i>%s</i>" (or desc path)))
   ((eq format 'latex) (format "\\textit{%s}" (or desc path)))
   (t (or desc path))))

(defun org-bbdb-open (name)
  "Follow a BBDB link to NAME."
  (require 'bbdb)
  (let ((inhibit-redisplay (not debug-on-error))
	(bbdb-electric-p nil))
    (catch 'exit
      ;; Exact match on name
      (bbdb-name (concat "\\`" name "\\'") nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Exact match on name
      (bbdb-company (concat "\\`" name "\\'") nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Partial match on name
      (bbdb-name name nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Partial match on company
      (bbdb-company name nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; General match including network address and notes
      (bbdb name nil)
      (when (= 0 (buffer-size (get-buffer "*BBDB*")))
	(delete-window (get-buffer-window "*BBDB*"))
	(error "No matching BBDB record")))))

(provide 'org-bbdb)

;;; org-bbdb.el ends here
