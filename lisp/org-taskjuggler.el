;;; org-taskjuggler.el --- TaskJuggler exporter for org-mode
;;
;; Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-taskjuggler.el
;; Version: 6.34trans
;; Author: Christian Egli
;; Maintainer: Christian Egli
;; Keywords: org, taskjuggler, project planning
;; Description: Converts an org-mode buffer into a taskjuggler project plan
;; URL:

;; This file is part of GNU Emacs.

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

;; Commentary:
;;
;; This library implements a TaskJuggler exporter for org-mode.
;;
;; The interactive functions are similar to those of the HTML and LaTeX
;; exporters:
;;
;; M-x `org-export-as-taskjuggler'
;; M-x `org-export-as-taskjuggler-and-open'
;; M-x `org-export-as-taskjuggler-batch'
;; M-x `org-export-as-taskjuggler-to-buffer'
;; M-x `org-export-region-as-taskjuggler'
;; M-x `org-replace-region-by-taskjuggler'
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)
(require 'org-exp)

;;; Variables:

(declare-function org-id-find-id-file "org-id" (id))

;;; User variables:

(defgroup org-export-taskjuggler nil
  "Options for exporting Org-mode files to TaskJuggler."
  :tag "Org Export TaskJuggler"
  :group 'org-export)

(defcustom org-export-taskjuggler-extension ".tjp"
  "Extension of TaskJuggler files."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-export-taskjuggler-project-tag "project"
  "."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-export-taskjuggler-resource-tag "resource"
  "."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-export-taskjuggler-default-project-version "1.0"
  "."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-export-taskjuggler-default-project-duration 365
  "."
  :group 'org-export-taskjuggler
  :type 'integer)

;;; Hooks

(defvar org-export-taskjuggler-final-hook nil
  "Hook run at the end of TaskJuggler export, in the new buffer.")

;;; Autoload functions:

;;;###autoload
(defun org-export-as-taskjuggler ()
  "Export the current buffer as a TaskJuggler file."
  (interactive)

  (message "Exporting...")
  (let* ((tasks
	  (org-map-entries '(org-taskjuggler-components) 
			   org-export-taskjuggler-project-tag nil 'archive 'comment))
	 (resources
	  (org-map-entries '(org-taskjuggler-components) 
			   org-export-taskjuggler-resource-tag nil 'archive 'comment))
	 (filename (expand-file-name
		    (concat
		     (file-name-sans-extension
		      (file-name-nondirectory buffer-file-name))
		     org-export-taskjuggler-extension)))
	 (buffer (find-file-noselect filename))
	 (old-level 0)
	 (current-id 0)
	 task resource)
    ;; add a default resource
    (unless resources
      (setq resources 
	    `((("ID" . ,(user-login-name)) 
	       ("headline" . ,user-full-name) 
	       ("level" . 1)))))
    ;; add a default allocation if none was given
    (unless (assoc "allocate" (car tasks))
      (let ((task (car tasks)))
	(setcar tasks (push (cons "allocate" (user-login-name)) task))))
    ;; add a default start date to the first task if none was given
    (unless (assoc "start" (car tasks))
      (let ((task (car tasks))
	    (time-string (format-time-string "%Y-%m-%d")))
	(setcar tasks (push (cons "start" time-string) task))))
    ;; add a default end date to the first task if none was given
    (unless (assoc "end" (car tasks))
      (let* ((task (car tasks))
	    (now (current-time))
	    (duration 
	     (days-to-time org-export-taskjuggler-default-project-duration))
	    (time-string 
	     (format-time-string "%Y-%m-%d" (time-add now duration))))
	(setcar tasks (push (cons "end" time-string) task))))
    ;; add a default version if none was given
    (unless (assoc "version" (car tasks))
      (let ((task (car tasks))
	    (version org-export-taskjuggler-default-project-version))
	(setcar tasks (push (cons "version" version) task))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-taskjuggler-open-project (car tasks))
      (dolist (resource resources nil)
	(let ((level (cdr (assoc "level" resource))))
	  (org-taskjuggler-close-maybe level)
	  (org-taskjuggler-open-resource resource)
	  (setq old-level level)))
      (org-taskjuggler-close-maybe 1)
      (setq old-level 0)
      (dolist (task tasks nil)
	(let ((level (cdr (assoc "level" task))))
	  (org-taskjuggler-close-maybe level)
	  (org-taskjuggler-open-task task)
	  (setq old-level level)))
      (org-taskjuggler-close-maybe 1))))

(defun org-taskjuggler-components ()
  ""
  (let* ((props (org-entry-properties))
	 (components (org-heading-components))
	 (level (car components))
	 (headline (nth 4 components)))
    (push (cons "level" level) props)
    (push (cons "headline" headline) props)))

(defun org-taskjuggler-clean-id (id)
  (and id (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" id)))

(defun org-taskjuggler-open-project (project)
  (let ((id (org-taskjuggler-clean-id (cdr (assoc "ID" project))))
	(headline (cdr (assoc "headline" project)))
	(version (cdr (assoc "version" project)))
	(start (cdr (assoc "start" project)))
	(end (cdr (assoc "end" project))))
    (insert 
     (concat 
      "project " 
      (or id "FIXME") 
      " \"" headline "\" \"" version "\" " start " - " end " {\n " "}\n"))))

(defun org-taskjuggler-open-resource (resource)
  (let ((id (org-taskjuggler-clean-id (cdr (assoc "ID" resource))))
	(headline (cdr (assoc "headline" resource))))
    (insert 
     (concat "resource " id " \"" headline "\" {\n "))))

(defun org-taskjuggler-clean-effort (effort)
  (cond 
   ((null effort) effort)
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)" effort) 
    (concat (match-string 1 effort) "." (match-string 2 effort) "h"))
   ((string-match "\\([0-9]+\\).\\([0-9]+\\)" effort) (concat effort "d"))
   (t (error "Not a valid effort (%s)" effort))))

(defun org-taskjuggler-open-task (task)
  (let ((id (org-taskjuggler-clean-id (cdr (assoc "ID" task))))
	(headline (cdr (assoc "headline" task)))
	(effort (org-taskjuggler-clean-effort(cdr (assoc org-effort-property task))))
	(depends (cdr (assoc "depends" task)))
	(allocate (cdr (assoc "allocate" task)))
	(account (cdr (assoc "account" task)))
	(start (cdr (assoc "start" task)))
	(complete (cdr (assoc "complete" task)))
	(note (cdr (assoc "note" task)))
	(priority (cdr (assoc "priority" task))))
    (insert
     (concat 
      "task " 
      (or id (concat "id" (number-to-string (setq current-id (1+ current-id))))) 
      " \"" headline "\" {" 
      (and effort (concat "\n effort " effort))
      (and depends (concat "\n depends " depends))
      (and allocate (concat "\n purge allocations\n allocate " allocate))
      (and account (concat "\n account " account))
      (and start (concat "\n start " start))
      (and complete (concat "\n complete " complete))
      (and note (concat "\n note " note))
      (and priority (concat "\n priority " priority))
      "\n"))))

(defun org-taskjuggler-close-maybe (level)
  (while (> old-level level) 
    (insert "}\n")
    (setq old-level (1- old-level)))
  (when (= old-level level)
    (insert "}\n")))


(provide 'org-taskjuggler)

;; arch-tag: a24a127c-d365-4c2a-9e9b-f7dcb0ebfdc3
;;; org-taskjuggler.el ends here
