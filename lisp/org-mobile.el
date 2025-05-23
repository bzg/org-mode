;;; org-mobile.el --- Code for Asymmetric Sync With a Mobile Device -*- lexical-binding: t; -*-
;; Copyright (C) 2009-2025 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains the code to interact with a mobile application,
;; such as Richard Moreland's iPhone application MobileOrg, or the
;; Android version by Matthew Jones.  This code is documented in
;; Appendix B of the Org manual.  The code is not specific for the
;; iPhone and Android - any external viewer/flagging/editing
;; application that uses the same conventions could be used.

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'ol)

;;; Code:

(defgroup org-mobile nil
  "Options concerning support for a viewer/editor on a mobile device."
  :tag "Org Mobile"
  :group 'org)

(defcustom org-mobile-files '(org-agenda-files)
  "Files to be staged for the mobile application.

This is basically a list of files and directories.  Files will be staged
directly.  Directories will be search for files with the extension \".org\".
In addition to this, the list may also contain the following symbols:

`org-agenda-files'
     This means include the complete, unrestricted list of files given in
     the variable `org-agenda-files'.

`org-agenda-text-search-extra-files'
     Include the files given in the variable
     `org-agenda-text-search-extra-files'."
  :group 'org-mobile
  :type '(list :greedy t
	       (option (const :tag "org-agenda-files" org-agenda-files))
	       (option (const :tag "org-agenda-text-search-extra-files"
			      org-agenda-text-search-extra-files))
	       (repeat :inline t :tag "Additional files"
		       (file))))

(defcustom org-mobile-files-exclude-regexp ""
  "A regexp to exclude files from `org-mobile-files'."
  :group 'org-mobile
  :version "24.1"
  :type 'regexp)

(defcustom org-mobile-directory ""
  "The WebDAV directory where the interaction with the mobile takes place."
  :group 'org-mobile
  :type 'directory)

(defcustom org-mobile-allpriorities "A B C"
  "Default set of priority cookies for the index file."
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string
  :group 'org-mobile)

(defcustom org-mobile-use-encryption nil
  "Non-nil means keep only encrypted files on the WebDAV server.

Encryption uses AES-256, with a password given in
`org-mobile-encryption-password'.  When nil, plain files are kept
on the server.

Turning on encryption requires setting the same password in the
mobile application.  Before turning this on, check if the mobile
application does support it."
  :group 'org-mobile
  :version "24.1"
  :type 'boolean)

(defcustom org-mobile-encryption-tempfile "~/orgtmpcrypt"
  "File that is being used as a temporary file for encryption.
This must be local file on your local machine (not on the WebDAV server).
You might want to put this file into a directory where only you have access."
  :group 'org-mobile
  :version "24.1"
  :type 'directory)

(defcustom org-mobile-encryption-password ""
  "Password for encrypting files uploaded to the server.

This is a single password which is used for AES-256 encryption.  The same
password must also be set in the mobile application.  All Org files,
including \"mobileorg.org\" will be encrypted using this password.

SECURITY CONSIDERATIONS:

Note that, when Org runs the encryption commands, the password could
be visible briefly on your system with the `ps' command.  So this method is
only intended to keep the files secure on the server, not on your own machine.

Also, if you set this variable in an init file (.emacs or .emacs.d/init.el
or custom.el...) and if that file is stored in a way so that other can read
it, this also limits the security of this approach.  You can also leave
this variable empty - Org will then ask for the password once per Emacs
session."
  :group 'org-mobile
  :version "24.1"
  :type '(string :tag "Password"))

(defvar org-mobile-encryption-password-session nil)

(defun org-mobile-encryption-password ()
  (or (org-string-nw-p org-mobile-encryption-password)
      (org-string-nw-p org-mobile-encryption-password-session)
      (setq org-mobile-encryption-password-session
	    (read-passwd "Password for mobile application: " t))))

(defcustom org-mobile-inbox-for-pull "~/org/from-mobile.org"
  "The file where captured notes and flags will be appended to.
During the execution of `org-mobile-pull', the file
`org-mobile-capture-file' is emptied as soon as its contents have
been appended to the file given here.  This file should be in
`org-directory', and not in the staging area or on the web server."
  :group 'org-mobile
  :type 'file)

(defconst org-mobile-capture-file "mobileorg.org"
  "The capture file where the mobile stores captured notes and flags.
This must not be changed, because the mobile application assumes this name.")

(defcustom org-mobile-index-file "index.org"
  "Index file with links to all Org files.
It should be loaded by the mobile application.  The file name is
relative to `org-mobile-directory'.  The \"Address\" field in the
mobile application setup should point to this file."
  :group 'org-mobile
  :type 'file)

(defcustom org-mobile-agendas 'all
  "The agendas that should be pushed to the mobile application.

Allowed values:

`default'  the weekly agenda and the global TODO list
`custom'   all custom agendas defined by the user
`all'      the custom agendas and the default ones
`list'     a list of selection key(s) as string."
  :group 'org-mobile
  :version "24.1"
  :type '(choice
	  (const :tag "Default Agendas" default)
	  (const :tag "Custom Agendas" custom)
	  (const :tag "Default and Custom Agendas" all)
	  (repeat :tag "Selected"
		  (string :tag "Selection Keys"))))

(defcustom org-mobile-force-id-on-agenda-items t
  "Non-nil means make all agenda items carry an ID."
  :group 'org-mobile
  :type 'boolean)

(defcustom org-mobile-force-mobile-change nil
  "Non-nil means force the change made on the mobile device.
So even if there have been changes to the computer version of the entry,
force the new value set on the mobile.
When nil, mark the entry from the mobile with an error message.
Instead of nil or t, this variable can also be a list of symbols, indicating
the editing types for which the mobile version should always dominate."
  :group 'org-mobile
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (set :greedy t :tag "Specify"
	       (const todo)
	       (const tags)
	       (const priority)
	       (const heading)
	       (const body))))

(defcustom org-mobile-checksum-binary (or (executable-find "shasum")
					  (executable-find "sha1sum")
					  (executable-find "md5sum")
					  (executable-find "md5"))
  "Executable used for computing checksums of agenda files."
  :group 'org-mobile
  :type 'string)

(defvar org-mobile-pre-push-hook nil
  "Hook run before running `org-mobile-push'.
This could be used to clean up `org-mobile-directory', for example to
remove files that used to be included in the agenda but no longer are.
The presence of such files would not really be a problem, but after time
they may accumulate.")

(defvar org-mobile-post-push-hook nil
  "Hook run after running `org-mobile-push'.
If Emacs does not have direct write access to the WebDAV directory used
by the mobile device, this hook should be used to copy all files from the
local staging directory `org-mobile-directory' to the WebDAV directory,
for example using `rsync' or `scp'.")

(defvar org-mobile-pre-pull-hook nil
  "Hook run before executing `org-mobile-pull'.
If Emacs does not have direct write access to the WebDAV directory used
by the mobile device, this hook should be used to copy the capture file
`mobileorg.org' from the WebDAV location to the local staging
directory `org-mobile-directory'.")

(defvar org-mobile-post-pull-hook nil
  "Hook run after running `org-mobile-pull', only if new items were found.
If Emacs does not have direct write access to the WebDAV directory used
by the mobile device, this hook should be used to copy the emptied
capture file `mobileorg.org' back to the WebDAV directory, for example
using `rsync' or `scp'.")

(defconst org-mobile-action-alist '(("edit" . org-mobile-edit))
  "Alist with flags and actions for mobile sync.

When flagging an entry, the mobile application creates entries
that look like

  * F(action:data)  [[id:entry-id][entry title]]

This alist defines that the ACTION in the parentheses of F()
should mean, i.e. what action should be taken.  The :data part in
the parenthesis is optional.  If present, the string after the
colon will be passed to the action function as the first argument
variable.

The car of each elements of the alist is an actions string.  The
cdr is a function that is called with the cursor on the headline
of that entry.  It should accept three arguments, the :data part,
the old and new values for the entry.")

(defvar org-mobile-last-flagged-files nil
  "List of files containing entries flagged in the latest pull.")

(defvar org-mobile-files-alist nil)
(defvar org-mobile-checksum-files nil)

;; Add org mobile commands to the main org menu
(easy-menu-add-item
 org-org-menu
 nil
 '("MobileOrg"
   ["Push Files and Views" org-mobile-push t]
   ["Get Captured and Flagged" org-mobile-pull t]
   ["Find FLAGGED Tasks" (org-agenda nil "?") :active t :keys "\\[org-agenda] ?"]
   "--"
   ["Setup" (customize-group 'org-mobile) t]))

(defun org-mobile-prepare-file-lists ()
  (setq org-mobile-files-alist (org-mobile-files-alist))
  (setq org-mobile-checksum-files nil))

(defun org-mobile-files-alist ()
  "Expand the list in `org-mobile-files' to a list of existing files.
Also exclude files matching `org-mobile-files-exclude-regexp'."
  (let* ((include-archives
	  (and (member 'org-agenda-text-search-extra-files org-mobile-files)
	       (member 'agenda-archives	org-agenda-text-search-extra-files)
	       t))
	 (files
	  (apply 'append
		 (mapcar
		  (lambda (f)
		    (cond
		     ((eq f 'org-agenda-files)
		      (org-agenda-files	t include-archives))
		     ((eq f 'org-agenda-text-search-extra-files)
		      (delq 'agenda-archives
			    (copy-sequence
			     org-agenda-text-search-extra-files)))
		     ((and (stringp f) (file-directory-p f))
		      (directory-files f 'full "\\.org\\'"))
		     ((and (stringp f) (file-exists-p f))
		      (list f))
		     (t nil)))
		  org-mobile-files)))
	 (files (delq
		 nil
		 (mapcar (lambda (f)
			   (unless (and (not (string= org-mobile-files-exclude-regexp ""))
					(string-match org-mobile-files-exclude-regexp f))
			     (identity f)))
			 files)))
	 (orgdir-uname (file-name-as-directory (file-truename org-directory)))
	 (orgdir-re (concat "\\`" (regexp-quote orgdir-uname)))
	 uname seen rtn file link-name)
    ;; Make the files unique, and determine the name under which they will
    ;; be listed.
    (while (setq file (pop files))
      (if (not (file-name-absolute-p file))
	  (setq file (expand-file-name file org-directory)))
      (setq uname (file-truename file))
      (unless (member uname seen)
	(push uname seen)
	(if (string-match orgdir-re uname)
	    (setq link-name (substring uname (match-end 0)))
	  (setq link-name (file-name-nondirectory uname)))
	(push (cons file link-name) rtn)))
    (nreverse rtn)))

;;;###autoload
(defun org-mobile-push ()
  "Push the current state of Org affairs to the target directory.
This will create the index file, copy all agenda files there, and also
create all custom agenda views, for upload to the mobile phone."
  (interactive)
  (let ((org-agenda-buffer-name "*SUMO*")
	(org-agenda-tag-filter org-agenda-tag-filter)
	(org-agenda-redo-command org-agenda-redo-command))
    ;; Offer to save agenda-related buffers before pushing, preventing
    ;; "Non-existent agenda file" prompt for lock files (see #19448).
    (let ((agenda-buffers (org-buffer-list 'agenda)))
      (save-some-buffers nil
			 (lambda () (memq (current-buffer) agenda-buffers))))
    (save-excursion
      (save-restriction
	(save-window-excursion
	  (run-hooks 'org-mobile-pre-push-hook)
	  (org-mobile-check-setup)
	  (org-mobile-prepare-file-lists)
	  (message "Creating agendas...")
	  (let ((inhibit-redisplay t)
		(org-agenda-files (mapcar 'car org-mobile-files-alist)))
	    (org-mobile-create-sumo-agenda))
	  (message "Creating agendas...done")
	  (org-save-all-org-buffers) ; to save any IDs created by this process
	  (message "Copying files...")
	  (org-mobile-copy-agenda-files)
	  (message "Writing index file...")
	  (org-mobile-create-index-file)
	  (message "Writing checksums...")
	  (org-mobile-write-checksums)
	  (run-hooks 'org-mobile-post-push-hook)))))
  (org-agenda-maybe-redo)
  (message "Files for mobile viewer staged"))

(defvar org-mobile-before-process-capture-hook nil
  "Hook that is run after content was moved to `org-mobile-inbox-for-pull'.
The inbox file is visited by the current buffer, and the buffer is
narrowed to the newly captured data.")

;;;###autoload
(defun org-mobile-pull ()
  "Pull the contents of `org-mobile-capture-file' and integrate them.
Apply all flagged actions, flag entries to be flagged and then call an
agenda view showing the flagged items."
  (interactive)
  (org-mobile-check-setup)
  (run-hooks 'org-mobile-pre-pull-hook)
  (let ((insertion-marker (org-mobile-move-capture)))
    (if (not (markerp insertion-marker))
	(message "No new items")
      (org-with-point-at insertion-marker
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (run-hooks 'org-mobile-before-process-capture-hook)))
      (org-with-point-at insertion-marker
	(org-mobile-apply (point) (point-max)))
      (move-marker insertion-marker nil)
      (run-hooks 'org-mobile-post-pull-hook)
      (when org-mobile-last-flagged-files
	;; Make an agenda view of flagged entries, but only in the files
	;; where stuff has been added.
	(put 'org-agenda-files 'org-restrict org-mobile-last-flagged-files)
	(let ((org-agenda-keep-restricted-file-list t))
	  (org-agenda nil "?"))))))

(defun org-mobile-check-setup ()
  "Check if `org-mobile-directory' has been set up."
  (org-mobile-cleanup-encryption-tempfile)
  (unless (and org-directory
	       (stringp org-directory)
	       (string-match "\\S-" org-directory)
	       (file-exists-p org-directory)
	       (file-directory-p org-directory))
    (error
     "Please set `org-directory' to the directory where your org files live"))
  (unless (and org-mobile-directory
	       (stringp org-mobile-directory)
	       (string-match "\\S-" org-mobile-directory)
	       (file-exists-p org-mobile-directory)
	       (file-directory-p org-mobile-directory))
    (error
     "Variable `org-mobile-directory' must point to an existing directory"))
  (unless (and org-mobile-inbox-for-pull
	       (stringp org-mobile-inbox-for-pull)
	       (string-match "\\S-" org-mobile-inbox-for-pull)
	       (file-exists-p
		(file-name-directory org-mobile-inbox-for-pull)))
    (error
     "Variable `org-mobile-inbox-for-pull' must point to a file in an existing directory"))
  (unless (and org-mobile-checksum-binary
	       (string-match "\\S-" org-mobile-checksum-binary))
    (error "No executable found to compute checksums"))
  (when org-mobile-use-encryption
    (unless (string-match "\\S-" (org-mobile-encryption-password))
      (error
       "To use encryption, you must set `org-mobile-encryption-password'"))
    (unless (file-writable-p org-mobile-encryption-tempfile)
      (error "Cannot write to encryption tempfile %s"
	     org-mobile-encryption-tempfile))
    (unless (executable-find "openssl")
      (error "OpenSSL is needed to encrypt files"))))

(defun org-mobile-create-index-file ()
  "Write the index file in the WebDAV directory."
  (let ((files-alist (sort (copy-sequence org-mobile-files-alist)
			   (lambda (a b) (string< (cdr a) (cdr b)))))
	(def-todo (default-value 'org-todo-keywords))
	(def-tags org-tag-alist)
	(target-file (expand-file-name org-mobile-index-file
				       org-mobile-directory))
	todo-kwds done-kwds tags)
    (when (stringp (car def-todo))
      (setq def-todo (list (cons 'sequence def-todo))))
    (org-agenda-prepare-buffers (mapcar 'car files-alist))
    (setq done-kwds (org-uniquify org-done-keywords-for-agenda))
    (setq todo-kwds (org-delete-all
		     done-kwds
		     (org-uniquify org-todo-keywords-for-agenda)))
    (setq tags (mapcar 'car (org-global-tags-completion-table
			     (mapcar 'car files-alist))))
    (with-temp-file (if org-mobile-use-encryption org-mobile-encryption-tempfile
		      target-file)
      (insert "#+READONLY\n")
      (dolist (entry def-todo)
	(let ((kwds (mapcar (lambda (x)
			      (if (string-match "(" x)
				  (substring x 0 (match-beginning 0))
				x))
			    (cdr entry))))
	  (insert "#+TODO: " (mapconcat #'identity kwds " ") "\n")
	  (let* ((dwds (or (member "|" kwds) (last kwds)))
		 (twds (org-delete-all dwds kwds)))
	    (setq todo-kwds (org-delete-all twds todo-kwds))
	    (setq done-kwds (org-delete-all dwds done-kwds)))))
      (when (or todo-kwds done-kwds)
	(insert "#+TODO: " (mapconcat 'identity todo-kwds " ") " | "
		(mapconcat 'identity done-kwds " ") "\n"))
      (setq def-tags (split-string (org-tag-alist-to-string def-tags t)))
      (setq tags (org-delete-all def-tags tags))
      (setq tags (sort tags (lambda (a b) (string< (downcase a) (downcase b)))))
      (setq tags (append def-tags tags nil))
      (insert "#+TAGS: " (mapconcat 'identity tags " ") "\n")
      (insert "#+ALLPRIORITIES: " org-mobile-allpriorities "\n")
      (when (file-exists-p (expand-file-name
			    "agendas.org" org-mobile-directory))
	(insert "* [[file:agendas.org][Agenda Views]]\n"))
      (pcase-dolist (`(,_ . ,link-name) files-alist)
	(insert (format "* [[file:%s][%s]]\n" link-name link-name)))
      (push (cons org-mobile-index-file (md5 (buffer-string)))
	    org-mobile-checksum-files))
    (when org-mobile-use-encryption
      (org-mobile-encrypt-and-move org-mobile-encryption-tempfile
				   target-file)
      (org-mobile-cleanup-encryption-tempfile))))

(defun org-mobile-copy-agenda-files ()
  "Copy all agenda files to the stage or WebDAV directory."
  (let ((files-alist org-mobile-files-alist)
	file buf entry link-name target-path target-dir check)
    (while (setq entry (pop files-alist))
      (setq file (car entry) link-name (cdr entry))
      (when (file-exists-p file)
	(setq target-path (expand-file-name link-name org-mobile-directory)
	      target-dir (file-name-directory target-path))
	(unless (file-directory-p target-dir)
	  (make-directory target-dir 'parents))
	(if org-mobile-use-encryption
	    (org-mobile-encrypt-and-move file target-path)
	  (copy-file file target-path 'ok-if-already-exists))
	(setq check (shell-command-to-string
		     (concat (shell-quote-argument org-mobile-checksum-binary)
			     " "
			     (shell-quote-argument (expand-file-name file)))))
	(when (string-match "[[:xdigit:]]\\{30,40\\}" check)
	  (push (cons link-name (match-string 0 check))
		org-mobile-checksum-files))))

    (setq file (expand-file-name org-mobile-capture-file
				 org-mobile-directory))
    (save-excursion
      (setq buf (find-file file))
      (when (and (= (point-min) (point-max)))
	(insert "\n")
	(save-buffer)
	(when org-mobile-use-encryption
	  (write-file org-mobile-encryption-tempfile)
	  (org-mobile-encrypt-and-move org-mobile-encryption-tempfile file)))
      (push (cons org-mobile-capture-file (md5 (buffer-string)))
	    org-mobile-checksum-files))
    (org-mobile-cleanup-encryption-tempfile)
    (kill-buffer buf)))

(defun org-mobile-write-checksums ()
  "Create checksums for all files in `org-mobile-directory'.
The table of checksums is written to the file mobile-checksums."
  (let ((sumfile (expand-file-name "checksums.dat" org-mobile-directory))
	(files org-mobile-checksum-files)
	entry file sum)
    (with-temp-file sumfile
      (set-buffer-file-coding-system 'undecided-unix nil)
      (while (setq entry (pop files))
	(setq file (car entry) sum (cdr entry))
	(insert (format "%s  %s\n" sum file))))))

(defun org-mobile-sumo-agenda-command ()
  "Return an agenda custom command that comprises all custom commands."
  (let ((custom-list
	 ;; normalize different versions
	 (delq nil
	       (mapcar
		(lambda (x)
		  (cond ((stringp (cdr x)) nil)
			((stringp (nth 1 x)) x)
			((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
			(t (cons (car x) (cons "" (cdr x))))))
		org-agenda-custom-commands)))
	(default-list '(("a" "Agenda" agenda) ("t" "All TODO" alltodo)))
	thelist	atitle new e key desc type match settings cmds gkey gdesc gsettings cnt)
    (cond
     ((eq org-mobile-agendas 'custom)
      (setq thelist custom-list))
     ((eq org-mobile-agendas 'default)
      (setq thelist default-list))
     ((eq org-mobile-agendas 'all)
      (setq thelist custom-list)
      (unless (assoc "t" thelist) (push '("t" "ALL TODO" alltodo) thelist))
      (unless (assoc "a" thelist) (push '("a" "Agenda" agenda) thelist)))
     ((listp org-mobile-agendas)
      (setq thelist (append custom-list default-list))
      (setq thelist (delq nil (mapcar (lambda (k) (assoc k thelist))
				      org-mobile-agendas)))))
    (while (setq e (pop thelist))
      (cond
       ((stringp (cdr e))
	;; this is a description entry - skip it
	)
       ((eq (nth 2 e) 'search)
	;; Search view is interactive, skip
	)
       ((memq (nth 2 e) '(todo-tree tags-tree occur-tree))
	;; These are trees, not really agenda commands
	)
       ((and (memq (nth 2 e) '(todo tags tags-todo))
	     (or (null (nth 3 e))
		 (not (string-match "\\S-" (nth 3 e)))))
	;; These would be interactive because the match string is empty
	)
       ((memq (nth 2 e) '(agenda alltodo todo tags tags-todo))
	;; a normal command
	(setq key (car e) desc (nth 1 e) type (nth 2 e) match (nth 3 e)
	      settings (nth 4 e))
	(setq settings
	      (cons (list 'org-agenda-title-append
			  (concat "<after>KEYS=" key " TITLE: "
				  (if (and (stringp desc) (> (length desc) 0))
				      desc (symbol-name type))
				  "</after>"))
		    settings))
	(push (list type match settings) new))
       ((or (functionp (nth 2 e)) (symbolp (nth 2 e)))
	;; A user-defined function, which can do anything, so simply
	;; ignore it.
	)
       (t
	;; a block agenda
	(setq gkey (car e) gdesc (nth 1 e) gsettings (nth 3 e) cmds (nth 2 e))
	(setq cnt 0)
	(while (setq e (pop cmds))
	  (setq type (car e) match (nth 1 e) settings (nth 2 e))
	  (setq atitle (if (string= "" gdesc) match gdesc))
	  (setq settings (append gsettings settings))
	  (setq settings
		(cons (list 'org-agenda-title-append
			    (concat "<after>KEYS=" gkey "#" (number-to-string
							     (setq cnt (1+ cnt)))
				    " TITLE: " atitle "</after>"))
		      settings))
	  (push (list type match settings) new)))))
    (and new (list "X" "SUMO" (reverse new)
		   '((org-agenda-compact-blocks nil))))))

(defvar org-mobile-creating-agendas nil)
(defun org-mobile-write-agenda-for-mobile (file)
  (let ((all (buffer-string)) in-date id pl prefix line app short m sexp)
    (with-temp-file file
      (org-mode)
      (insert "#+READONLY\n")
      (insert all)
      (goto-char (point-min))
      (while (not (eobp))
	(cond
	 ((looking-at "[ \t]*$")) ; keep empty lines
	 ((looking-at "=+$")
	  ;; remove underlining
          (delete-region (point) (line-end-position)))
	 ((get-text-property (point) 'org-agenda-structural-header)
	  (setq in-date nil)
	  (setq app (get-text-property (point) 'org-agenda-title-append))
	  (setq short (get-text-property (point) 'short-heading))
	  (when (and short (looking-at ".+"))
	    (replace-match short nil t)
	    (forward-line 0))
	  (when app
	    (end-of-line 1)
	    (insert app)
	    (forward-line 0))
	  (insert "* "))
	 ((get-text-property (point) 'org-agenda-date-header)
	  (setq in-date t)
	  (insert "** "))
	 ((setq m (or (get-text-property (point) 'org-hd-marker)
		      (get-text-property (point) 'org-marker)))
	  (setq sexp (member (get-text-property (point) 'type)
			     '("diary" "sexp")))
          (if (setq pl (text-property-any (point) (line-end-position) 'org-heading t))
	      (progn
		(setq prefix (org-trim (buffer-substring
					(point) pl))
		      line (org-trim (buffer-substring
				      pl
                                      (line-end-position))))
                (delete-region (line-beginning-position) (line-end-position))
		(insert line "<before>" prefix "</before>")
		(forward-line 0))
	    (and (looking-at "[ \t]+") (replace-match "")))
	  (insert (if in-date "***  " "**  "))
	  (end-of-line 1)
	  (insert "\n")
	  (unless sexp
	    (insert (org-agenda-get-some-entry-text
		     m 10 "   " 'planning)
		    "\n")
	    (when (setq id
			(if (bound-and-true-p
			     org-mobile-force-id-on-agenda-items)
			    (org-id-get m 'create)
			  (or (org-entry-get m "ID")
			      (org-mobile-get-outline-path-link m))))
	      (insert "   :PROPERTIES:\n   :ORIGINAL_ID: " id
		      "\n   :END:\n")))))
	(forward-line 1))
      (push (cons "agendas.org" (md5 (buffer-string)))
	    org-mobile-checksum-files))
    (message "Agenda written to Org file %s" file)))

(defun org-mobile-get-outline-path-link (pom)
  (org-with-point-at pom
    (concat "olp:"
	    (org-mobile-escape-olp (file-name-nondirectory buffer-file-name))
	    ":"
	    (mapconcat 'org-mobile-escape-olp
		       (org-get-outline-path)
		       "/")
	    "/"
	    (org-mobile-escape-olp (nth 4 (org-heading-components))))))

(defun org-mobile-escape-olp (s)
  (org-link-encode s '(?: ?/)))

(defun org-mobile-create-sumo-agenda ()
  "Create a file that contains all custom agenda views."
  (interactive)
  (let* ((file (expand-file-name "agendas.org"
				 org-mobile-directory))
	 (file1 (if org-mobile-use-encryption
		    org-mobile-encryption-tempfile
		  file))
	 (sumo (org-mobile-sumo-agenda-command))
	 (org-agenda-custom-commands
	  (list (append sumo (list (list file1)))))
	 (org-mobile-creating-agendas t))
    (unless (file-writable-p file1)
      (error "Cannot write to file %s" file1))
    (when sumo
      (org-store-agenda-views))
    (when org-mobile-use-encryption
      (org-mobile-encrypt-and-move file1 file)
      (delete-file file1)
      (org-mobile-cleanup-encryption-tempfile))))

(defun org-mobile-encrypt-and-move (infile outfile)
  "Encrypt INFILE locally to INFILE_enc, then move it to OUTFILE.
We do this in two steps so that remote paths will work, even if the
encryption program does not understand them."
  (let ((encfile (concat infile "_enc")))
    (org-mobile-encrypt-file infile encfile)
    (when outfile
      (copy-file encfile outfile 'ok-if-already-exists)
      (delete-file encfile))))

(defun org-mobile-encrypt-file (infile outfile)
  "Encrypt INFILE to OUTFILE, using `org-mobile-encryption-password'."
  (shell-command
   (format "openssl enc -md md5 -aes-256-cbc -salt -pass %s -in %s -out %s"
	   (shell-quote-argument (concat "pass:"
					 (org-mobile-encryption-password)))
	   (shell-quote-argument (expand-file-name infile))
	   (shell-quote-argument (expand-file-name outfile)))))

(defun org-mobile-decrypt-file (infile outfile)
  "Decrypt INFILE to OUTFILE, using `org-mobile-encryption-password'."
  (shell-command
   (format "openssl enc -md md5 -d -aes-256-cbc -salt -pass %s -in %s -out %s"
	   (shell-quote-argument (concat "pass:"
					 (org-mobile-encryption-password)))
	   (shell-quote-argument (expand-file-name infile))
	   (shell-quote-argument (expand-file-name outfile)))))

(defun org-mobile-cleanup-encryption-tempfile ()
  "Remove the encryption tempfile if it exists."
  (and (stringp org-mobile-encryption-tempfile)
       (file-exists-p org-mobile-encryption-tempfile)
       (delete-file org-mobile-encryption-tempfile)))

(defun org-mobile-move-capture ()
  "Move the contents of the capture file to the inbox file.
Return a marker to the location where the new content has been added.
If nothing new has been added, return nil."
  (interactive)
  (let* ((encfile nil)
	 (capture-file (expand-file-name org-mobile-capture-file
					 org-mobile-directory))
	 (inbox-buffer (find-file-noselect org-mobile-inbox-for-pull))
	 (capture-buffer
	  (if (not org-mobile-use-encryption)
	      (find-file-noselect capture-file)
	    (org-mobile-cleanup-encryption-tempfile)
	    (setq encfile (concat org-mobile-encryption-tempfile "_enc"))
	    (copy-file capture-file encfile)
	    (org-mobile-decrypt-file encfile org-mobile-encryption-tempfile)
	    (find-file-noselect org-mobile-encryption-tempfile)))
	 (insertion-point (make-marker))
	 not-empty content)
    (with-current-buffer capture-buffer
      (setq content (buffer-string))
      (setq not-empty (string-match "\\S-" content))
      (when not-empty
	(set-buffer inbox-buffer)
	(widen)
	(goto-char (point-max))
	(or (bolp) (newline))
	(move-marker insertion-point
		     (prog1 (point) (insert content)))
	(save-buffer)
	(set-buffer capture-buffer)
	(erase-buffer)
	(save-buffer)
	(org-mobile-update-checksum-for-capture-file (buffer-string))))
    (kill-buffer capture-buffer)
    (when org-mobile-use-encryption
      (org-mobile-encrypt-and-move org-mobile-encryption-tempfile
				   capture-file)
      (org-mobile-cleanup-encryption-tempfile))
    (if not-empty insertion-point)))

(defun org-mobile-update-checksum-for-capture-file (buffer-string)
  "Find the checksum line and modify it to match BUFFER-STRING."
  (let* ((file (expand-file-name "checksums.dat" org-mobile-directory))
	 (buffer (find-file-noselect file)))
    (when buffer
      (with-current-buffer buffer
	(when (re-search-forward (concat "\\([[:xdigit:]]\\{30,\\}\\).*?"
					 (regexp-quote org-mobile-capture-file)
					 "[ \t]*$") nil t)
	  (goto-char (match-beginning 1))
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert (md5 buffer-string))
	  (save-buffer)))
      (kill-buffer buffer))))

(defun org-mobile-apply (&optional beg end)
  "Apply all change requests in the current buffer.
If BEG and END are given, only do this in that region."
  (interactive)
  (require 'org-archive)
  (setq org-mobile-last-flagged-files nil)
  (setq beg (or beg (point-min)) end (or end (point-max)))

  ;; Remove all Note IDs
  (goto-char beg)
  (while (re-search-forward "^\\*\\* Note ID: [-0-9A-F]+[ \t]*\n" end t)
    (replace-match ""))

  ;; Find all the referenced entries, without making any changes yet
  (let ((marker (make-marker))
	(bos-marker (make-marker))
	(end (move-marker (make-marker) end))
	(cnt-new 0)
	(cnt-edit 0)
	(cnt-flag 0)
	(cnt-error 0)
	buf-list
	org-mobile-error)

    ;; Count the new captures
    (goto-char beg)
    (while (re-search-forward "^\\* \\(.*\\)" end t)
      (and (>= (- (match-end 1) (match-beginning 1)) 2)
	   (not (equal (downcase (substring (match-string 1) 0 2)) "f("))
	   (cl-incf cnt-new)))

    ;; Find and apply the edits
    (goto-char beg)
    (while (re-search-forward
	    "^\\*+[ \t]+F(\\([^():\n]*\\)\\(:\\([^()\n]*\\)\\)?)[ \t]+\\[\\[\\(\\(id\\|olp\\):\\([^]\n]+\\)\\)" end t)
      (catch 'next
	(let* ((action (match-string 1))
	       (data (and (match-end 3) (match-string 3)))
	       (id-pos (condition-case msg
			   (org-mobile-locate-entry (match-string 4))
			 (error (nth 1 msg))))
	       (bos (line-beginning-position))
	       (eos (save-excursion (org-end-of-subtree t t)))
	       (cmd (if (equal action "")
			(let ((note (buffer-substring-no-properties
				     (line-beginning-position 2) eos)))
			  (lambda (_data _old _new)
			    (cl-incf cnt-flag)
			    (org-toggle-tag "FLAGGED" 'on)
			    (org-entry-put
			     nil "THEFLAGGINGNOTE"
			     (replace-regexp-in-string "\n" "\\\\n" note))))
		      (cl-incf cnt-edit)
		      (cdr (assoc action org-mobile-action-alist))))
	       ;; Do not take notes interactively.
	       (org-inhibit-logging 'note)
	       old new)

	  (goto-char bos)
	  (when (and (markerp id-pos)
		     (not (member (marker-buffer id-pos) buf-list)))
	    (org-mobile-timestamp-buffer (marker-buffer id-pos))
	    (push (marker-buffer id-pos) buf-list))
	  (unless (markerp id-pos)
            (goto-char (+ 2 (line-beginning-position)))
	    (if (stringp id-pos)
		(insert id-pos " ")
	      (insert "BAD REFERENCE "))
	    (cl-incf cnt-error)
	    (throw 'next t))
	  (unless cmd
	    (insert "BAD FLAG ")
	    (cl-incf cnt-error)
	    (throw 'next t))
	  (move-marker bos-marker (point))
	  (if (re-search-forward "^\\** Old value[ \t]*$" eos t)
	      (setq old (buffer-substring
			 (1+ (match-end 0))
			 (progn (outline-next-heading) (point)))))
	  (if (re-search-forward "^\\** New value[ \t]*$" eos t)
	      (setq new (buffer-substring
			 (1+ (match-end 0))
			 (progn (outline-next-heading)
				(if (eobp) (org-back-over-empty-lines))
				(point)))))
	  (setq old (org-string-nw-p old))
	  (setq new (org-string-nw-p new))
	  (unless (equal data "body")
	    (setq new (and new (org-trim new)))
	    (setq old (and old (org-trim old))))
	  (goto-char (+ 2 bos-marker))
	  ;; Remember this place so that we can return
	  (move-marker marker (point))
	  (setq org-mobile-error nil)
	  (condition-case msg
	      (org-with-point-at id-pos
		(funcall cmd data old new)
		(unless (member data '("delete" "archive" "archive-sibling"
				       "addheading"))
		  (when (member "FLAGGED" (org-get-tags nil t))
		    (add-to-list 'org-mobile-last-flagged-files
				 (buffer-file-name)))))
	    (error (setq org-mobile-error msg)))
	  (when org-mobile-error
	    (pop-to-buffer-same-window (marker-buffer marker))
	    (goto-char marker)
	    (cl-incf cnt-error)
	    (insert (if (stringp (nth 1 org-mobile-error))
			(nth 1 org-mobile-error)
		      "EXECUTION FAILED")
		    " ")
	    (throw 'next t))
	  ;; If we get here, the action has been applied successfully
	  ;; So remove the entry
	  (goto-char bos-marker)
	  (delete-region (point) (org-end-of-subtree t t)))))
    (save-buffer)
    (move-marker marker nil)
    (move-marker end nil)
    (message "%d new, %d edits, %d flags, %d errors"
	     cnt-new cnt-edit cnt-flag cnt-error)
    (sit-for 1)))

(defun org-mobile-timestamp-buffer (buf)
  "Time stamp buffer BUF, just to make sure its checksum will change."
  (with-current-buffer buf
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(if (re-search-forward
	     "^\\([ \t]*\\)#\\+LAST_MOBILE_CHANGE:.*\n?" nil t)
	    (progn
              (goto-char (match-end 1))
	      (delete-region (point) (match-end 0)))
          (if (looking-at ".*?-\\*-.*-\\*-")
              (forward-line 1)))
	(insert "#+LAST_MOBILE_CHANGE: "
		(format-time-string "%Y-%m-%d %T") "\n")))))

(defun org-mobile-smart-read ()
  "Parse the entry at point for shortcuts and expand them.
These shortcuts are meant for fast and easy typing on the limited
keyboards of a mobile device.  Below we show a list of the shortcuts
currently implemented.

The entry is expected to contain an inactive time stamp indicating when
the entry was created.  When setting dates and
times (for example for deadlines), the time strings are interpreted
relative to that creation date.
Abbreviations are expected to take up entire lines, just because it is so
easy to type RET on a mobile device.  Abbreviations start with one or two
letters, followed immediately by a dot and then additional information.
Generally the entire shortcut line is removed after action have been taken.
Time stamps will be constructed using `org-read-date'.  So for example a
line \"dd. 2tue\" will set a deadline on the second Tuesday after the
creation date.

Here are the shortcuts currently implemented:

dd. string             set deadline
ss. string             set scheduling
tt. string             set time tamp, here.
ti. string             set inactive time

tg. tag1 tag2 tag3     set all these tags, change case where necessary
td. kwd                set this todo keyword, change case where necessary

FIXME: Hmmm, not sure if we can make his work against the
auto-correction feature.  Needs a bit more thinking.  So this function
is currently a noop.")

(defun org-mobile-locate-entry (link)
  (if (string-match "\\`id:\\(.*\\)$" link)
      (org-id-find (match-string 1 link) 'marker)
    (if (not (string-match "\\`olp:\\(.*?\\):\\(.*\\)$" link))
					; not found with path, but maybe it is to be inserted
					; in top level of the file?
	(if (not (string-match "\\`olp:\\(.*?\\)$" link))
	    nil
	  (let ((file (match-string 1 link)))
	    (setq file (org-link-decode file))
	    (setq file (expand-file-name file org-directory))
	    (save-excursion
	      (find-file file)
	      (goto-char (point-max))
	      (newline)
	      (goto-char (point-max))
	      (point-marker))))
      (let ((file (match-string 1 link))
	    (path (match-string 2 link)))
	(setq file (org-link-decode file))
	(setq file (expand-file-name file org-directory))
	(setq path (mapcar #'org-link-decode
			   (org-split-string path "/")))
	(org-find-olp (cons file path))))))

(defun org-mobile-edit (what old new)
  "Edit item WHAT in the current entry by replacing OLD with NEW.
WHAT can be \"heading\", \"todo\", \"tags\", \"priority\", or \"body\".
The edit only takes place if the current value is equal (except for
white space) the OLD.  If this is so, OLD will be replace by NEW
and the command will return t.  If something goes wrong, a string will
be returned that indicates what went wrong."
  (let (current old1 new1 level)
    (if (stringp what) (setq what (intern what)))

    (cond

     ((memq what '(todo todostate))
      (setq current (org-get-todo-state))
      (cond
       ((equal new "DONEARCHIVE")
	(org-todo 'done)
	(org-archive-subtree-default))
       ((equal new current) t)		; nothing needs to be done
       ((or (equal current old)
	    (eq org-mobile-force-mobile-change t)
	    (memq 'todo org-mobile-force-mobile-change))
	(org-todo (or new 'none)) t)
       (t (error "State before change was expected as \"%s\", but is \"%s\""
		 old current))))

     ((eq what 'tags)
      (setq current (org-get-tags nil t)
	    new1 (and new (org-split-string new ":+"))
	    old1 (and old (org-split-string old ":+")))
      (cond
       ((org-mobile-tags-same-p current new1) t) ; no change needed
       ((or (org-mobile-tags-same-p current old1)
	    (eq org-mobile-force-mobile-change t)
	    (memq 'tags org-mobile-force-mobile-change))
	(org-set-tags new1) t)
       (t (error "Tags before change were expected as \"%s\", but are \"%s\""
		 (or old "") (or current "")))))

     ((eq what 'priority)
      (let ((case-fold-search nil))
	(when (looking-at org-complex-heading-regexp)
	  (let ((current (and (match-end 3) (substring (match-string 3) 2 3))))
	    (cond
	     ((equal current new) t)	;no action required
	     ((or (equal current old)
		  (eq org-mobile-force-mobile-change t)
		  (memq 'tags org-mobile-force-mobile-change))
	      (org-priority (and new (string-to-char new))))
	     (t (error "Priority was expected to be %s, but is %s"
		       old current)))))))

     ((eq what 'heading)
      (let ((case-fold-search nil))
	(when (looking-at org-complex-heading-regexp)
	  (let ((current (match-string 4)))
	    (cond
	     ((equal current new) t)	;no action required
	     ((or (equal current old)
		  (eq org-mobile-force-mobile-change t)
		  (memq 'heading org-mobile-force-mobile-change))
	      (goto-char (match-beginning 4))
	      (insert new)
	      (delete-region (point) (+ (point) (length current)))
	      (when org-auto-align-tags (org-align-tags)))
	     (t
	      (error
	       "Heading changed in the mobile device and on the computer")))))))

     ((eq what 'addheading)
      (if (org-at-heading-p)	; if false we are in top-level of file
	  (progn
	    ;; Workaround a `org-insert-heading-respect-content' bug
	    ;; which prevents correct insertion when point is invisible
	    (org-fold-show-subtree)
	    (end-of-line 1)
	    (org-insert-heading-respect-content t)
	    (org-demote))
	(forward-line 0)
	(insert "* "))
      (insert new))

     ((eq what 'refile)
      (org-copy-subtree)
      (org-with-point-at (org-mobile-locate-entry new)
	(if (org-at-heading-p)	; if false we are in top-level of file
	    (progn
	      (setq level (org-get-valid-level (funcall outline-level) 1))
	      (org-end-of-subtree t t)
	      (org-paste-subtree level))
	  (org-paste-subtree 1)))
      (org-cut-subtree))

     ((eq what 'delete)
      (org-cut-subtree))

     ((eq what 'archive)
      (org-archive-subtree))

     ((eq what 'archive-sibling)
      (org-archive-to-archive-sibling))

     ((eq what 'body)
      (setq current (buffer-substring (min (1+ (line-end-position)) (point-max))
				      (save-excursion (outline-next-heading)
						      (point))))
      (if (not (string-match "\\S-" current)) (setq current nil))
      (cond
       ((org-mobile-bodies-same-p current new) t) ; no action necessary
       ((or (org-mobile-bodies-same-p current old)
	    (eq org-mobile-force-mobile-change t)
	    (memq 'body org-mobile-force-mobile-change))
	(save-excursion
	  (end-of-line 1)
	  (insert "\n" new)
	  (or (bolp) (insert "\n"))
	  (delete-region (point) (progn (org-back-to-heading t)
					(outline-next-heading)
					(point))))
	t)
       (t (error
	   "Body was changed in the mobile device and on the computer")))))))

(defun org-mobile-tags-same-p (list1 list2)
  "Are the two tag lists the same?"
  (not (or (org-delete-all list1 list2)
	   (org-delete-all list2 list1))))

(defun org-mobile-bodies-same-p (a b)
  "Compare if A and B are visually equal strings.
We first remove leading and trailing white space from the entire strings.
Then we split the strings into lines and remove leading/trailing whitespace
from each line.  Then we compare.
A and B must be strings or nil."
  (cond
   ((and (not a) (not b)) t)
   ((or (not a) (not b)) nil)
   (t (setq a (org-trim a) b (org-trim b))
      (setq a (mapconcat 'identity (org-split-string a "[ \t]*\n[ \t]*") "\n"))
      (setq b (mapconcat 'identity (org-split-string b "[ \t]*\n[ \t]*") "\n"))
      (equal a b))))

(provide 'org-mobile)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-mobile.el ends here
