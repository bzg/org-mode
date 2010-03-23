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
  "Tag, property or todo used to find the tree containing all
the tasks for the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-export-taskjuggler-resource-tag "resource"
  "Tag, property or todo used to find the tree containing all the
resources for the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-export-taskjuggler-default-project-version "1.0"
  "Default version string for the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-export-taskjuggler-default-project-duration 180
  "Default project duration if no start and end date have been defined
in the root node of the task tree, i.e. the tree that has been marked
with `org-export-taskjuggler-project-tag'"
  :group 'org-export-taskjuggler
  :type 'integer)

(defcustom org-export-taskjuggler-default-reports 
  '("taskreport \"Gantt Chart\" {
  headline \"Project Gantt Chart\"
  columns hierarchindex, name, start, end, effort, duration, completed, chart
  timeformat \"%Y-%m-%d\"
  hideresource 1
  loadunit days
}"
"resourcereport \"Resource Graph\" {
  headline \"Resource Allocation Graph\"
  columns no, name, utilization, freeload, chart
  loadunit days
  sorttasks startup
  hidetask ~isleaf()
}")
  "Default reports for the project."
  :group 'org-export-taskjuggler
  :type '(repeat (string :tag "Report")))

(defcustom org-export-taskjuggler-default-global-properties 
  "shift s40 \"Part time shift\" {
  workinghours wed, thu, fri off
}
"
  "Default global properties for the project. Here you typically
define global properties such as shifts, accounts, rates,
vacation, macros and flags. Any property that is allowed within
the TaskJuggler file can be inserted. You could for example
include another TaskJuggler file. 

The global properties are inserted after the project declaration
but before any resource and task declarations."
  :group 'org-export-taskjuggler
  :type '(string :tag "Preamble"))

;;; Hooks

(defvar org-export-taskjuggler-final-hook nil
  "Hook run at the end of TaskJuggler export, in the new buffer.")

;;; Autoload functions:

;;;###autoload
(defun org-export-as-taskjuggler ()
  "Export parts of the current buffer as a TaskJuggler file.
The exporter looks for a tree with tag, property or todo that
matches `org-export-taskjuggler-project-tag' and takes this as
the tasks for this project. The first node of this tree defines
the project properties such as project name and project period.
If there is a tree with tag, property or todo that matches
`org-export-taskjuggler-resource-tag' this three is taken as
resources for the project. If no resources are specified, a
default resource is created and allocated to the project. Also
the taskjuggler project will be created with default reports as
defined in `org-export-taskjuggler-default-reports'."
  (interactive)

  (message "Exporting...")
  (setq-default org-done-keywords org-done-keywords)
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
      (insert org-export-taskjuggler-default-global-properties)
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
  "Return true if the parent of the current node has a property
\"ORDERED\". Return nil otherwise."
  (save-excursion
    (and (org-up-heading-safe) (org-entry-get (point) "ORDERED"))))

(defun org-taskjuggler-components ()
  "Return an alist containing all the pertinent information for
the current node such as the headline, the level, todo state
information, all the properties, etc."
  (let* ((props (org-entry-properties))
	 (components (org-heading-components))
	 (level (car components))
	 (headline (nth 4 components))
	 (parent-ordered (org-taskjuggler-parent-is-ordered-p)))
    (push (cons "level" level) props)
    (push (cons "headline" headline) props)
    (push (cons "parent-ordered" parent-ordered) props)))

(defun org-taskjuggler-assign-task-ids (tasks)
  "Given a list of tasks return the same list assigning a unique id
and the full path to each task. Taskjuggler takes hierarchical ids.
For that reason we have to make ids locally unique and we have to keep
a path to the current task."
  (let ((previous-level 0)
	unique-ids unique-id
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
	  (push unique-id (car unique-ids))
	  (setcar path unique-id))
	 ((> previous-level level) 
	  (dotimes (tmp (- previous-level level))
	    (pop unique-ids)
	    (pop path))
	  (setq unique-id (org-taskjuggler-get-unique-id task (car unique-ids)))
	  (push unique-id (car unique-ids))
	  (setcar path unique-id)))
	(push (cons "unique-id" unique-id) task)
	(push (cons "path" (mapconcat 'identity (reverse path) ".")) task)
	(setq previous-level level)
	(setq resolved-tasks (append resolved-tasks (list task)))))))

(defun org-taskjuggler-assign-resource-ids (resources &optional unique-ids)
  "Given a list of resources return the same list, assigning a
unique id to each resource."
  (cond
   ((null resources) nil)
   (t 
    (let* ((resource (car resources))
	   (unique-id (org-taskjuggler-get-unique-id resource unique-ids)))
      (push (cons "unique-id" unique-id) resource)
      (cons resource (org-taskjuggler-assign-resource-ids (cdr resources) 
							  (cons unique-id unique-ids)))))))

(defun org-taskjuggler-resolve-dependencies (tasks)
  (let ((previous-level 0)
	siblings
	task resolved-tasks)
    (dolist (task tasks resolved-tasks)
      (let* ((level (cdr (assoc "level" task)))
	     (depends (cdr (assoc "depends" task)))
	     (parent-ordered (cdr (assoc "parent-ordered" task)))
	     (blocker (cdr (assoc "BLOCKER" task)))
	     (blocked-on-previous (and blocker (string-match "previous-sibling" blocker)))
	     (dependencies
	      (org-taskjuggler-resolve-explicit-dependencies
	       (append 
		(and depends (split-string depends "[, \f\t\n\r\v]+" t))
		(and blocker (split-string blocker "[, \f\t\n\r\v]+" t))) tasks))
	      previous-sibling)
	; update previous sibling info
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
	; insert a dependency on previous sibling if the parent is
	; ordered or if the tasks has a BLOCKER attribute with value "previous-sibling"
	(when (or (and previous-sibling parent-ordered) blocked-on-previous)
	  (push (format "!%s" (cdr (assoc "unique-id" previous-sibling))) dependencies))
	; store dependency information
	(when dependencies 
	  (push (cons "depends" (mapconcat 'identity dependencies ", ")) task))
	(setq previous-level level)
	(setq resolved-tasks (append resolved-tasks (list task)))))))

(defun org-taskjuggler-resolve-explicit-dependencies (dependencies tasks)
  (let (path)
    (cond 
     ((null dependencies) nil)
     ; ignore previous sibling dependencies
     ((equal (car dependencies) "previous-sibling")
      (org-taskjuggler-resolve-explicit-dependencies (cdr dependencies) tasks))
     ; if the id is found in another task use its path
     ((setq path (org-taskjuggler-find-task-with-id (car dependencies) tasks))
      (cons path (org-taskjuggler-resolve-explicit-dependencies (cdr dependencies) tasks)))
     ; silently ignore all other dependencies
     (t (org-taskjuggler-resolve-explicit-dependencies (cdr dependencies) tasks)))))

(defun org-taskjuggler-find-task-with-id (id tasks)
  "Find ID in tasks. If found return the path of task. Otherwise return nil."
  (let ((task-id (cdr (assoc "ID" (car tasks))))
	(path (cdr (assoc "path" (car tasks)))))
    (cond 
     ((null tasks) nil)
     ((equal task-id id) path)
     (t (org-taskjuggler-find-task-with-id id (cdr tasks))))))

(defun org-taskjuggler-get-unique-id (item unique-ids)
  "Return a unique id for an ITEM which can be a task or a resource.
The id is derived from the headline and made unique against
UNIQUE-IDS. If the first part of the headline is not unique try to add
more parts of the headline or finally add more underscore characters (\"_\")."
  (let* ((headline (cdr (assoc "headline" item)))
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
  "Clean and return ID to make it acceptable for taskjuggler."
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
	(attributes '(limits vacation shift booking efficiency journalentry rate)))
    (insert 
     (concat 
      "resource " (or id unique-id) " \"" headline "\" {\n "
      (mapconcat
       'identity
       (remq
	nil 
	(mapcar
	 (lambda (attribute)
	   (let ((value (cdr (assoc (symbol-name attribute) resource))))
	     (and value 
		  (if (equal attribute 'limits)
		      (format "%s { %s }" (symbol-name attribute) value)
		    (format "%s %s" (symbol-name attribute) value)))))
	 attributes)) "\n") "\n"))))

(defun org-taskjuggler-clean-effort (effort)
  "Translate effort strings into a format acceptable to taskjuggler,
i.e. REAL UNIT. If the effort string is something like 5:00 it
will be assumed to be hours and will be translated into 5.0h.
Otherwise if it contains something like 3.0 it is assumed to be
days and will be translated into 3.0d. Other formats that taskjuggler
supports (like weeks, months and years) are currently not supported."
  (cond 
   ((null effort) effort)
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)" effort) 
    ; FIXME: translate the minutes to a decimal
    (concat (match-string 1 effort) "." (match-string 2 effort) "h"))
   ((string-match "\\([0-9]+\\).\\([0-9]+\\)" effort) (concat effort "d"))
   (t (error "Not a valid effort (%s)" effort))))

(defun org-taskjuggler-open-task (task)
  (let* ((unique-id (cdr (assoc "unique-id" task)))
	(headline (cdr (assoc "headline" task)))
	(effort (org-taskjuggler-clean-effort (cdr (assoc org-effort-property task))))
	(depends (cdr (assoc "depends" task)))
	(allocate (cdr (assoc "allocate" task)))
	(account (cdr (assoc "account" task)))
	(start (cdr (assoc "start" task)))
	(state (cdr (assoc "TODO" task)))
	(complete (or (and (member state org-done-keywords) "100") 
		      (cdr (assoc "complete" task))))
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
