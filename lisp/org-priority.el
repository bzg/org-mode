;;; org-priority.el --- Org priority API                      -*- lexical-binding: t; -*-

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

;; This library implements Org mode headline priorities.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-move)

(defvar org-todo-line-regexp)

(defgroup org-priorities nil
  "Priorities in Org mode."
  :tag "Org Priorities"
  :group 'org-todo)

(defvaralias 'org-enable-priority-commands 'org-priority-enable-commands)
(defcustom org-priority-enable-commands t
  "Non-nil means priority commands are active.
When nil, these commands will be disabled, so that you never accidentally
set a priority."
  :group 'org-priorities
  :type 'boolean)

(defvaralias 'org-default-priority 'org-priority-default)
(defcustom org-priority-default ?B
  "The default priority of TODO items.
This is the priority an item gets if no explicit priority is given.
When starting to cycle on an empty priority the first step in the cycle
depends on `org-priority-start-cycle-with-default'.  The resulting first
step priority must not exceed the range from `org-priority-highest' to
`org-priority-lowest' which means that `org-priority-default' has to be
in this range exclusive or inclusive to the range boundaries.  Else the
first step refuses to set the default and the second will fall back on
\(depending on the command used) the highest or lowest priority."
  :group 'org-priorities
  :package-version '(Org . "9.8")
  :type '( restricted-sexp :tag "Number 0-64 or uppercase character A-Z"
           :match-alternatives ((lambda (val) (org-priority-valid-value-p val t))))
  :safe #'integerp)

(defcustom org-priority-start-cycle-with-default t
  "Non-nil means start with default priority when starting to cycle.
When this is nil, the first step in the cycle will be (depending on the
command used) one higher or lower than the default priority.
See also `org-priority-default'."
  :group 'org-priorities
  :type 'boolean)

(defvaralias 'org-get-priority-function 'org-priority-get-priority-function)
(defcustom org-priority-get-priority-function nil
  "Function to extract the priority from a string.
The string is normally the headline.  If this is nil, Org
computes the priority from the priority cookie like [#A] in the
headline.  It returns an integer, increasing by 1000 for each
priority level.

The user can set a different function here, which should take a
string as an argument and return the numeric priority."
  :group 'org-priorities
  :version "24.1"
  :type '(choice
	  (const nil)
	  (function)))

(defun org-priority-up ()
  "Increase the priority of the current item."
  (interactive nil org-mode)
  (org-priority 'up))

(defun org-priority-down ()
  "Decrease the priority of the current item."
  (interactive nil org-mode)
  (org-priority 'down))

(defun org-priority (&optional action)
  "Change the priority of an item.

When called interactively with a `\\[universal-argument]' prefix,
show the priority in the minibuffer instead of changing it.

When called programmatically, ACTION can be `set', `up', `down', `remove', an
uppercase alphabetic character A through Z, or an integer 0 through 64,
inclusive.  If a lower-case character is passed as ACTION or entered via
interactive prompt, it will automatically be converted to uppercase."
  (interactive "P" org-mode)
  (if (equal action '(4))
      (org-priority-show)
    (unless org-priority-enable-commands
      (user-error "Priority commands are disabled"))
    ;; If action was not provided, default to "set", which will prompt the user
    (setq action (or action 'set))
    (let ((is-numeric-priority (org-priority-number-p org-priority-lowest))
	  current-value new-value new-value-string has-existing-cookie remove)
      (save-excursion
	(org-back-to-heading t)
	(when (looking-at org-priority-regexp)
	  (let ((ms (match-string 2)))
	    (setq current-value (org-priority-to-value ms)
		  has-existing-cookie t)))
	(cond
	 ((eq action 'remove)
	  (setq remove t new-value ?\s))
         ;; set and a value are treated similarly, but only if the
         ;; value is a valid priority
	 ((or (eq action 'set)
	      (org-priority-valid-value-p action))
	  (if (not (eq action 'set))
	      (setq new-value action)
	    (setq
	     new-value
             (let* ((msg (format "Priority %s-%s, SPC to remove: "
                                 (org-priority-to-string org-priority-highest)
                                 (org-priority-to-string org-priority-lowest)))
                    ;; Read input as a string if the current high/low range could
                    ;; be two digits. Otherwise, we can just read a single
                    ;; character and save the user from pressing <enter>
                    (s (save-excursion ; prompt may change point position
                         (if (and is-numeric-priority
                                  (>= org-priority-lowest 10))
                             (read-string msg)
                           (char-to-string (read-char-exclusive msg))))))
               ;; Space is a special value indicating removal. For numeric
               ;; priorities, parse the numeric value or ensure an upper case
               ;; character (and convert back to a character for validation)
               (if (string-equal s " ")
                   ?\s
                 (if is-numeric-priority
                     (progn
                       ;; Validate the input string to ensure it's a valid value because
                       ;; string-to-number returns zero for a non-numeric string, which could
                       ;; be a valid priority
                       (unless (string-match-p org-priority-value-regexp s)
                         (user-error "Priority must be a number between `%s' and `%s'"
		                     (org-priority-to-string org-priority-highest)
                                     (org-priority-to-string org-priority-lowest)))
                       (string-to-number s))
                   ;; Validation of non-numerics is handled next
                   (string-to-char (upcase s)))))))
          ;; After reading interactive input, set removal flag if needed and
          ;; perform validation on the new value
	  (cond
           ((equal new-value ?\s) (setq remove t))
	   ((not (org-priority-valid-value-p new-value))
	    (user-error "Priority must be between `%s' and `%s'"
		        (org-priority-to-string org-priority-highest)
                        (org-priority-to-string org-priority-lowest)))))
	 ((eq action 'up)
	  (setq new-value (if has-existing-cookie
			      (1- current-value)  ; normal cycling
		            ;; last priority was empty
		            (if (eq last-command this-command)
			        org-priority-lowest  ; wrap around empty to lowest
			      ;; default
			      (if org-priority-start-cycle-with-default
			          org-priority-default
			        (1- org-priority-default))))))
	 ((eq action 'down)
	  (setq new-value (if has-existing-cookie
			      (1+ current-value)  ; normal cycling
		            ;; last priority was empty
		            (if (eq last-command this-command)
			        org-priority-highest  ; wrap around empty to highest
			      ;; default
			      (if org-priority-start-cycle-with-default
			          org-priority-default
			        (1+ org-priority-default))))))
	 (t (user-error (concat "Invalid action: `%s'.  Action must be one of "
                                "`up', `down', `set', `remove' or a value "
                                "between `%s' and `%s'")
                        action
                        (org-priority-to-string org-priority-highest)
                        (org-priority-to-string org-priority-lowest))))
        ;; Check against the current high/low range if we need to wrap
	(when (not (org-priority-valid-value-p new-value))
	  (if (and (memq action '(up down))
		   (not has-existing-cookie) (not (eq last-command this-command)))
	      ;; `new-value' is from default priority
	      (error
	       "The default can not be set, see `org-priority-default' why")
	    ;; normal cycling: `new-value' is beyond highest/lowest priority
	    ;; and is wrapped around to the empty priority
	    (setq remove t)))
        (setq new-value-string
              (if remove
                  "removed"
                (org-priority-to-string new-value)))
	(if has-existing-cookie
	    (if remove
		(replace-match "" t t nil 1)
	      (replace-match new-value-string t t nil 2))
	  (if remove
	      (user-error "No priority cookie found in line")
	    (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	    (if (match-end 2)
		(progn
		  (goto-char (match-end 2))
		  (insert " " org-priority-prefix new-value-string org-priority-suffix))
	      (goto-char (match-beginning 3))
	      (insert org-priority-prefix new-value-string org-priority-suffix " "))))
        ;; In org-tags.el
	(when (bound-and-true-p org-auto-align-tags)
          (declare-function org-align-tags "org-tags.el" (&optional all))
          (org-align-tags)))
      (if remove
	  (message "Priority removed")
	(message "Priority of current item set to %s" new-value-string)))))

(defalias 'org-show-priority 'org-priority-show)
(defun org-priority-show ()
  "Show the priority of the current item as number.
Return the priority value."
  (interactive nil org-mode org-agenda-mode)
  (let ((pri (if (eq major-mode 'org-agenda-mode)
		 (org-get-at-bol 'priority)
               (let ((heading (org-element-at-point)))
                 (when (org-element-type-p heading 'headline)
                   (org-get-priority (org-element-property :raw-value (org-element-at-point))))))))
    (message "Priority is %d" (if pri pri -1000))))

(defun org-get-priority (s)
  "Find priority cookie and return priority.
S is a string against which you can match `org-priority-regexp'.
If `org-priority-get-priority-function' is set to a custom
function, use it.  Otherwise process S and output the priority
value, an integer."
  (save-match-data
    (if (functionp org-priority-get-priority-function)
	(funcall org-priority-get-priority-function s)
      (if (not (string-match org-priority-regexp s))
	  (* 1000 (- org-priority-lowest org-priority-default))
	(* 1000 (- org-priority-lowest
		   (org-priority-to-value (match-string 2 s))))))))

(provide 'org-priority)

;;; org-priority.el ends here
