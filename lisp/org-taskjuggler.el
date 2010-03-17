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
;;
;;; TODO:
;;    * derive completeness info from TODO state
;;    * Handle explicit dependencies such as BLOCKER and depends attribute
;;    * Code cleanup
;;    * Add documentation
;;    * Try using plists instead of alists
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)
(require 'org-exp)

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

(defcustom org-export-taskjuggler-default-project-duration 180
  "."
  :group 'org-export-taskjuggler
  :type 'integer)

(defcustom org-export-taskjuggler-default-reports 
  '("taskreport \"Gantt Chart\" {
  headline \"Project Gantt Chart\"
  columns hierarchindex, name, start, end, effort, duration, completed, chart
  timeformat \"%a %Y-%m-%d\"
  loadunit days
}"
"resourcereport \"Resource Graph\" {
  headline \"Resource Allocation Graph\"
  columns no, name, rate, utilization, freeload, chart
  loadunit days
  hidetask 1
}")
  ""
  :group 'org-export-taskjuggler
  :type '(repeat (string :tag "Report")))

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
	  (org-taskjuggler-resolve-dependencies
	   (org-taskjuggler-assign-task-ids 
	    (org-map-entries '(org-taskjuggler-components) 
			     org-export-taskjuggler-project-tag nil 'archive 'comment))))
	 (resources
	  (org-taskjuggler-assign-resource-ids
	   (org-map-entries '(org-taskjuggler-components) 
			    org-export-taskjuggler-resource-tag nil 'archive 'comment)))
	 (filename (expand-file-name
		    (concat
		     (file-name-sans-extension
		      (file-name-nondirectory buffer-file-name))
		     org-export-taskjuggler-extension)))
	 (buffer (find-file-noselect filename))
	 (old-level 0)
	 task resource)
    ;; add a default resource
    (unless resources
      (setq resources 
	    `((("ID" . ,(user-login-name)) 
	       ("headline" . ,user-full-name) 
	       ("level" . 1)))))
    ;; add a default allocation to the first task if none was given
    (unless (assoc "allocate" (car tasks))
      (let ((task (car tasks))
	    (resource-id (cdr (assoc "ID" (car resources)))))
	(setcar tasks (push (cons "allocate" resource-id) task))))
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
      (dolist (resource resources)
	(let ((level (cdr (assoc "level" resource))))
	  (org-taskjuggler-close-maybe level)
	  (org-taskjuggler-open-resource resource)
	  (setq old-level level)))
      (org-taskjuggler-close-maybe 1)
      (setq old-level 0)
      (dolist (task tasks)
	(let ((level (cdr (assoc "level" task))))
	  (org-taskjuggler-close-maybe level)
	  (org-taskjuggler-open-task task)
	  (setq old-level level)))
      (org-taskjuggler-close-maybe 1)
      (org-taskjuggler-insert-reports)
      (save-buffer)
      (or (org-export-push-to-kill-ring "TaskJuggler")
	  (message "Exporting... done"))
      (current-buffer))))

;;;###autoload
(defun org-export-as-taskjuggler-and-open ()
  "Export the current buffer as a TaskJuggler file and open it with the TaskJuggler GUI."
  (interactive)
  (let ((file-name (buffer-file-name (org-export-as-taskjuggler)))
	(command "TaskJugglerUI"))
    (start-process-shell-command command nil command file-name)))

(defun org-taskjuggler-parent-is-ordered-p ()
  (save-excursion
    (and (org-up-heading-safe) (org-entry-get (point) "ORDERED"))))

(defun org-taskjuggler-components ()
  (let* ((props (org-entry-properties))
	 (components (org-heading-components))
	 (level (car components))
	 (headline (nth 4 components))
	 (parent-ordered (org-taskjuggler-parent-is-ordered-p)))
    (push (cons "level" level) props)
    (push (cons "headline" headline) props)
    (push (cons "parent-ordered" parent-ordered) props)))

(defun org-taskjuggler-assign-task-ids (tasks)
  (let ((previous-level 0)
	unique-ids
	path
	task resolved-tasks tmp)
    (dolist (task tasks resolved-tasks)
      (let ((level (cdr (assoc "level" task))))
	(cond
	 ((< previous-level level) 
	  (setq unique-id (org-taskjuggler-get-unique-id task (car unique-ids)))
	  (dotimes (tmp (- level previous-level))
	    (push (list unique-id) unique-ids)
	    (push unique-id path)))
	 ((= previous-level level) 
	  (setq unique-id (org-taskjuggler-get-unique-id task (car unique-ids)))
	  (push unique-id (car unique-ids)))
	 ((> previous-level level) 
	  (dotimes (tmp (- previous-level level))
	    (pop unique-ids)
	    (pop path))
	  (setq unique-id (org-taskjuggler-get-unique-id task (car unique-ids)))
	  (push unique-id (car unique-ids))))
	(push (cons "unique-id" unique-id) task)
	(push (cons "path" (mapconcat 'identity (reverse path) ".")) task)
	(setq previous-level level)
	(setq resolved-tasks (append resolved-tasks (list task)))))))

(defun org-taskjuggler-assign-resource-ids (resources)
  (let (unique-ids
	unique-id
	resource resolved-resources)
    (dolist (resource resources resolved-resources)
      (setq unique-id (org-taskjuggler-get-unique-id resource unique-ids))
      (push unique-id unique-ids)
      (push (cons "unique-id" unique-id) resource)
      (setq resolved-resources (append resolved-resources (list resource))))))

(defun org-taskjuggler-resolve-dependencies (tasks)
  (let ((previous-level 0)
	siblings
	task resolved-tasks)
    (dolist (task tasks resolved-tasks)
      (let ((level (cdr (assoc "level" task)))
	    (depends (cdr (assoc "depends" task)))
	    (parent-ordered (cdr (assoc "parent-ordered" task)))
	    previous-sibling)
	(cond
	 ((< previous-level level) 
	  (dotimes (tmp (- level previous-level))
	    (push task siblings)))
	 ((= previous-level level)
	  (setq previous-sibling (car siblings))
	  (setcar siblings task))
	 ((> previous-level level) 
	  (dotimes (tmp (- previous-level level))
	    (pop siblings))
	  (setq previous-sibling (car siblings))
	  (setcar siblings task)))
	(when (and previous-sibling parent-ordered)
	  (push 
	   (cons "depends" 
		 (format "!%s" (cdr (assoc "unique-id" previous-sibling)))) task))
	(setq previous-level level)
	(setq resolved-tasks (append resolved-tasks (list task)))))))

(defun org-taskjuggler-get-unique-id (task unique-ids)
  (let* ((headline (cdr (assoc "headline" task)))
	 (parts (split-string headline))
	 (id (downcase (pop parts))))
    ; try to add more parts of the headline to make it unique
    (while (member id unique-ids)
      (setq id (concat id "_" (downcase (pop parts)))))
    ; if its still not unique add "_"
    (while (member id unique-ids)
      (setq id (concat id "_")))
    (org-taskjuggler-clean-id id)))
	
(defun org-taskjuggler-clean-id (id)
  (and id (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" id)))

(defun org-taskjuggler-open-project (project)
  (let ((unique-id (cdr (assoc "unique-id" project)))
	(headline (cdr (assoc "headline" project)))
	(version (cdr (assoc "version" project)))
	(start (cdr (assoc "start" project)))
	(end (cdr (assoc "end" project))))
    (insert 
     (concat 
      "project " unique-id
      " \"" headline "\" \"" version "\" " start " - " end " {\n }\n"))))

(defun org-taskjuggler-open-resource (resource)
  (let ((id (org-taskjuggler-clean-id (cdr (assoc "ID" resource))))
	(unique-id (org-taskjuggler-clean-id (cdr (assoc "unique-id" resource))))
	(headline (cdr (assoc "headline" resource)))
	(limits (cdr (assoc "limits" resource)))
	(vacation (cdr (assoc "vacation" resource))))
    (insert 
     (concat "resource " (or id unique-id) " \"" headline "\" {\n "
	     (and limits (concat "\n limits { " limits " }\n"))
	     (and vacation (concat "\n vacation " vacation "\n"))))))

(defun org-taskjuggler-clean-effort (effort)
  (cond 
   ((null effort) effort)
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)" effort) 
    (concat (match-string 1 effort) "." (match-string 2 effort) "h"))
   ((string-match "\\([0-9]+\\).\\([0-9]+\\)" effort) (concat effort "d"))
   (t (error "Not a valid effort (%s)" effort))))

(defun org-taskjuggler-open-task (task)
  (let ((unique-id (cdr (assoc "unique-id" task)))
	(headline (cdr (assoc "headline" task)))
	(effort (org-taskjuggler-clean-effort (cdr (assoc org-effort-property task))))
	(depends (cdr (assoc "depends" task)))
	(allocate (cdr (assoc "allocate" task)))
	(account (cdr (assoc "account" task)))
	(start (cdr (assoc "start" task)))
	(complete (cdr (assoc "complete" task)))
	(note (cdr (assoc "note" task)))
	(priority (cdr (assoc "priority" task)))
	(parent-ordered (cdr (assoc "parent-ordered" task)))
	(previous-sibling (cdr (assoc "previous-sibling" task))))
    (insert
     (concat 
      "task " unique-id " \"" headline "\" {" 
      (and effort (concat "\n effort " effort))
      (if (and parent-ordered previous-sibling)
	  (concat "\n depends " previous-sibling)
	(and depends (concat "\n depends " depends)))
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

(defun org-taskjuggler-insert-reports ()
  (let (report)
    (dolist (report org-export-taskjuggler-default-reports)
      (insert report "\n"))))

(provide 'org-taskjuggler)

;;; org-taskjuggler.el ends here
