;; Then I don't really thing you would have to be able to customize 
;; this, as there are only very few operations for which this makes 
;; sense:

;; A****  Archive
;; T**** Mark TODO
;; D**** Mark DONE
;; N**** Cycle TODO to the next state

;; Can't really think of anything else.


;; I prefer configurable, because then people can use numbers. This is
;; the idea that the editor may have limited UI. I'm using a j2me based
;; editor called JPE at the moment:
;; http://my-communicator.com/s80/software/applications.php?fldAuto=556&faq=2

;; But other people may be using something like this: 
;; http://www.getjar.com/products/3960/TextEditor

;; Or this which i'm currently playing with: 
;; http://www.bermin.net/index.html

;; As for other things, it depends on what you want emacs to be able to
;; do with an externally changed org mode file. For me this is about
;; using org mode in an intelligent way with my mobile phone/pda. I can
;; imagine wanting to write functions like:

;; * move this huge piece of text and tables down a level
;; <* move this huge piece of text and tables up a level
;; M* ask to recategorise this heading when i open org mode
;; +* remind me about this when i open org mode so i can brain dump on it
;;    in a real editor.
;; D* ask me to schedule this as an event when i open org mode.
;; O* open my mail client to send an email to this email address i just got
;; C* search bbdb for the contact details of the phone no on this line.
;; c* search ldap for the contact details of this name
;; B* open a web browser to this link i wanted to check out when i got back to my machine
;; R* remind me to look at TheseSearchTags headings when i get back to my machine.


(defcustom org-fastup-action-alist
  '((?A org-archive t)
    (?T (org-todo 1) nil)
    (?D (org-todo (length org-todo-keywords)) nil)
    (?N org-todo nil)
    (?< org-promote-subtree t)
    (?> org-demote-subtree t)
    (?M org-set-tags nil)
    (?S org-schedule t))
  "List of fastupdate actions.
Each entry in this list is a list of 3 items:

- A character representing the fastupdate action
- A function or form to be executed, with cursor at beginning of headline
- A flag indicating if execution of this action should normally be confirmed."
  :group 'org-fastup
  :type '(repeat
	  (list  :value (?a  nil t)
	   (character :tag "Prefix char")
	   (choice
	    (const :tag "Archive this subtree" org-archive)
	    (const :tag "Make TODO" (org-todo 1))
	    (const :tag "Mark DONE" (org-todo (length org-todo-keywords)))
	    (const :tag "Cycle TODO" org-todo)
	    (const :tag "Promote subtree" org-promote-subtree)
	    (const :tag "Demote subtree" org-demote-subtree)
	    (const :tag "Set Tags" org-set-tags)
	    (const :tag "Schedule" org-schedule)
	    (const :tag "Set Deadline" org-schedule)
	    (sexp))
	    (boolean :tag "Confirm"))))

(defun org-fastup-check-buffer ()
  "Check for and execute fastupdate actions.
This first checks if there are any fastupdate actions in the buffer.
If yes, the user is asked for a processing mode, with three possibilities
with respect to confirming actions:

Always    confirm each action before executing it
Never     execute all actions without prior confirmation
Maybe     get only confirmation for actions that have been configured
          as requiring confirmation in `org-fastup-action-alist'.

The command will then walk through the buffer, stop at each eaction
and do the right thing there."
  (interactive)
  (show-all)  ; make everything visible
  (let ((start (point-min))
	;; FIXME: should I limit the regexp to match existing actions?
	;; I think not, to catch typos
	(re "^\\([-a-zA-Z0-9!@#$%^&+?<>]\\)\\*+")
	s action confirm)
    (if (not (save-excursion
	       (goto-char (point-min))
	       (re-search-forward re nil t)))
	(if (interactive-p) (message "No fastupdate actions in this buffer"))
      (goto-char start)
      (message "Fastupdate:  Confirm actions [A]lways [Maybe] [N]ever, or [Q]uit?")
      (setq reaction (read-char-exclusive))
      (cond
       ((memq reaction '(?q ?Q)) (error "Abort"))
       ((memq reaction '(?a ?A)) (setq cf 'always))
       ((memq reaction '(?m ?M)) (setq cf 'maybe))
       ((memq reaction '(?n ?N)) (setq cf 'never)))
      (while (re-search-forward re nil t)
	(goto-char (setq start (match-beginning 0)))
	(setq s (match-string 1)
	      entry (assoc (string-to-char s) org-fastup-action-alist)
	      action (nth 1 entry)
	      confirm (nth 2 entry))
	(cond
	 ((null action)
	  (if (y-or-n-p "Unknown action.  Remove fastupdate character? ")
	      (delete-region start (1+ start))
	    (goto-char (1+ start))))
	 ((or (equal cf 'never)
	      (and (eq cf 'maybe) (not confirm))
	      (y-or-n-p (format "execute action [%s] " s)))
	  (delete-region start (1+ start))
	  ;; FIXME: wrap the following into condition-case and 
	  ;; handle any errors in some way.
	  (if (symbolp action) (funcall action) (eval action))
	  ;; FIXME: remove the sit-for
	  (sit-for 2))
	 (t
	  (if (y-or-n-p "Action denied. Remove fastupdate character? ")
	      ;; Remove the character, without action.
	      (delete-region start (1+ start))
	    ;; Just leave the character in and skip this location
	    (goto-char (1+ start)))))))))




