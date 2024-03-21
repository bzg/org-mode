;;; org-dblock.el --- Org dynamic block support                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements Org mode commands and function to define
;; and update dynamic blocks.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)

;;;; Dynamic blocks

(defun org-find-dblock (name)
  "Find the first dynamic block with name NAME in the buffer.
If not found, stay at current position and return nil."
  (let ((case-fold-search t) pos)
    (save-excursion
      (goto-char (point-min))
      (setq pos (and (re-search-forward
		      (concat "^[ \t]*#\\+\\(?:BEGIN\\|begin\\):[ \t]+" name "\\>") nil t)
		     (match-beginning 0))))
    (when pos (goto-char pos))
    pos))

(defun org-create-dblock (plist)
  "Create a dynamic block section, with parameters taken from PLIST.
PLIST must contain a :name entry which is used as the name of the block."
  (when (string-match "\\S-" (buffer-substring (line-beginning-position)
                                              (line-end-position)))
    (end-of-line 1)
    (newline))
  (let ((col (current-column))
	(name (plist-get plist :name)))
    (insert "#+BEGIN: " name)
    (while plist
      (if (eq (car plist) :name)
	  (setq plist (cddr plist))
	(insert " " (prin1-to-string (pop plist)))))
    (insert "\n\n" (make-string col ?\ ) "#+END:\n")
    (forward-line -3)))

(defun org-prepare-dblock ()
  "Prepare dynamic block for refresh.
This empties the block, puts the cursor at the insert position and returns
the property list including an extra property :name with the block name."
  (unless (looking-at org-dblock-start-re)
    (user-error "Not at a dynamic block"))
  (let* ((begdel (1+ (match-end 0)))
	 (name (org-no-properties (match-string 1)))
	 (params (append (list :name name)
			 (read (concat "(" (match-string 3) ")")))))
    (save-excursion
      (forward-line 0)
      (skip-chars-forward " \t")
      (setq params (plist-put params :indentation-column (current-column))))
    (unless (re-search-forward org-dblock-end-re nil t)
      (error "Dynamic block not terminated"))
    (setq params
	  (append params
		  (list :content (buffer-substring
				  begdel (match-beginning 0)))))
    (delete-region begdel (match-beginning 0))
    (goto-char begdel)
    (open-line 1)
    params))

(defun org-map-dblocks (&optional command)
  "Apply COMMAND to all dynamic blocks in the current buffer.
If COMMAND is not given, use `org-update-dblock'."
  (let ((cmd (or command 'org-update-dblock)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-dblock-start-re nil t)
	(goto-char (match-beginning 0))
        (save-excursion
          (condition-case-unless-debug nil
              (funcall cmd)
            (error (message "Error during update of dynamic block"))))
	(unless (re-search-forward org-dblock-end-re nil t)
	  (error "Dynamic block not terminated"))))))

(defvar org-dynamic-block-alist nil
  "Alist defining all the Org dynamic blocks.

The key is the dynamic block type name, as a string.  The value
is the function used to insert the dynamic block.

Use `org-dynamic-block-define' to populate it.")

(defun org-dynamic-block-function (type)
  "Return function associated to a given dynamic block type.
TYPE is the dynamic block type, as a string."
  (cdr (assoc type org-dynamic-block-alist)))

(defun org-dynamic-block-types ()
  "List all defined dynamic block types."
  (mapcar #'car org-dynamic-block-alist))

;;;###org-autoload
(defun org-dynamic-block-define (type func)
  "Define dynamic block TYPE with FUNC.
TYPE is a string.  FUNC is the function creating the dynamic
block of such type.  FUNC must be able to accept zero arguments."
  (pcase (assoc type org-dynamic-block-alist)
    (`nil (push (cons type func) org-dynamic-block-alist))
    (def (setcdr def func))))

(defun org-dynamic-block-insert-dblock (type &optional interactive-p)
  "Insert a dynamic block of type TYPE.
When used interactively, select the dynamic block types among
defined types, per `org-dynamic-block-define'.  If INTERACTIVE-P
is non-nil, call the dynamic block function interactively."
  (interactive (list (completing-read "Dynamic block: "
				      (org-dynamic-block-types))
		     t)
               org-mode)
  (pcase (org-dynamic-block-function type)
    (`nil (error "No such dynamic block: %S" type))
    ((and f (pred functionp))
     (if (and interactive-p (commandp f)) (call-interactively f) (funcall f)))
    (_ (error "Invalid function for dynamic block %S" type))))

(defun org-dblock-update (&optional arg)
  "User command for updating dynamic blocks.
Update the dynamic block at point.  With prefix ARG, update all dynamic
blocks in the buffer."
  (interactive "P" org-mode)
  (if arg
      (org-update-all-dblocks)
    (or (looking-at org-dblock-start-re)
	(org-beginning-of-dblock))
    (org-update-dblock)))

(defun org-update-dblock ()
  "Update the dynamic block at point.
This means to empty the block, parse for parameters and then call
the correct writing function."
  (interactive nil org-mode)
  (save-excursion
    (let* ((win (selected-window))
	   (pos (point))
	   (line (org-current-line))
	   (params
            ;; Called for side effect.
            (org-prepare-dblock))
	   (name (plist-get params :name))
	   (indent (plist-get params :indentation-column))
	   (cmd (intern (concat "org-dblock-write:" name))))
      (message "Updating dynamic block `%s' at line %d..." name line)
      (funcall cmd params)
      (message "Updating dynamic block `%s' at line %d...done" name line)
      (goto-char pos)
      (when (and indent (> indent 0))
	(setq indent (make-string indent ?\ ))
	(save-excursion
	  (select-window win)
	  (org-beginning-of-dblock)
	  (forward-line 1)
	  (while (not (looking-at org-dblock-end-re))
	    (insert indent)
	    (forward-line 1))
	  (when (looking-at org-dblock-end-re)
	    (and (looking-at "[ \t]+")
		 (replace-match ""))
	    (insert indent)))))))

(defun org-beginning-of-dblock ()
  "Find the beginning of the dynamic block at point.
Error if there is no such block at point."
  (let ((pos (point))
	beg)
    (end-of-line 1)
    (if (and (re-search-backward org-dblock-start-re nil t)
	     (setq beg (match-beginning 0))
	     (re-search-forward org-dblock-end-re nil t)
	     (> (match-end 0) pos))
	(goto-char beg)
      (goto-char pos)
      (error "Not in a dynamic block"))))

(defun org-update-all-dblocks ()
  "Update all dynamic blocks in the buffer.
This function can be used in a hook."
  (interactive nil org-mode)
  (when (derived-mode-p 'org-mode)
    (org-map-dblocks 'org-update-dblock)))

(provide 'org-dblock)

;;; org-dblock.el ends here
