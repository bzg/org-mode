;;; org-id.el --- Global identifier for Org-mode entries
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.01
;;
;; This file is not yet part of GNU Emacs.
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

;; This file implements globally unique identifiers for Org-mode entries.
;; Identifiers are stored in the entry as an :ID: property.  This file
;; provides functions to create and retrieve such identifiers.

;; It provides the following API:
;;
;;
;; Higer-level-functions
;;
;; org-id-create
;;        Create an ID for the entry at point if it does not yet have one.
;;        Returns the ID (old or new).  This function can be used
;;        interactively, with prefix argument the creation of a new ID is
;;        forced, even if there was an old one.
;;
;; org-id-get
;;        Get the ID property of an entry.  Using appropriate arguments
;;        to the function, it can also create the ID for this entry.
;;
;; org-id-goto
;;        Command to go to a specific ID, this command can be used
;;        interactively.
;;
;; org-id-get-with-outline-path-completion
;;        Retrieve the ID of an entry, using outline path completion.
;;        This function can work for multiple files.
;;
;; org-id-get-with-outline-drilling
;;        Retrieve the ID of an entry, using outline path completion.
;;        This function only works for the current file.
;;
;; org-id-find
;;        Find the location of an entry with specific id.
;;
;; TODO:
;; - get/create id at current entry, safe in kill or so.

(require 'org)


;;; Customization

(defgroup org-id nil
  "Options concerning global entry identifiers in Org-mode."
  :tag "Org ID"
  :group 'org)

(defcustom org-id-structure (list "Org" (user-login-name) t 4)
  "Components of globaly unique id's created by Org-mode.
An Org-mode ID has 4 components:

prefix       A prefix to identify the ID type (default \"Org\".
creator      The creator of the ID, defaults to the users login name.
incremental  An incremental number, specific for the creator.
random       A random sequence of characters, to make sure ID created
             in distributed development will still be unique.
             This may be a short random sequence, or an md5 sum created
             based on the current time, the current computer, and the user."
  :group 'org-id
  :type '(list
	  (string :tag "Prefix")
	  (string :tag "Creator")
	  (boolean :tag "Incremental")
	  (choice :tag "Random part"
		  (const :tag "No random part" nil)
		  (integer :tag "N characters")
		  (const :tag "MD5 digest" md5))))

(defcustom org-id-tracking-file "~/.org-id"
  "The file for remembering the last ID number generated, for each type."
  :group 'org-id
  :type 'file)

(defvar org-id-values nil
  "Association list of ID types+creator with largest index used so far.")

(defcustom org-id-locations-file "~/.org-id-locations"
  "The file for remembering the last ID number generated."
  :group 'org-id
  :type 'file)

(defvar org-id-locations nil
  "List of files with ID's in those files.")

(defcustom org-id-extra-files 'org-agenda-multi-occur-extra-files
  "Files to be searched for ID's, besides the agenda files."
  :group 'org-id
  :type
  '(choice
    (symbol :tag "Variable")
    (repeat :tag "List of files"
	    (file))))


;;; The API functions

(defun org-id-create (&optional force)
  "Create an ID for the current entry and return it.
If the entry already has an ID, just return it.
With optional argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when force
    (org-entry-put (point) "ID" nil))
  (org-id-get (point) 'create))
  
(defun org-id-copy ()
  "Copy the ID of the entry at point to the kill ring.
Create an ID if necessary."
  (interactive)
  (kill-new (org-id-get nil 'create)))  

(defun org-id-get (&optional pom create type nrandom)
  "Get the ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
However, when CREATE is non nil, create an ID if none is present already.
TYPE and NRANDOM will be passed through to `org-id-new'.
In any case, the ID of the entry is returned."
  (let ((id (org-entry-get pom "ID")))
    (cond
     ((and id (stringp id) (string-match "\\S-" id))
      id)
     (create
      (setq id (org-id-new type nrandom))
      (org-entry-put pom "ID" id)
      (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
      id)
     (t nil))))

(defun org-id-get-with-outline-path-completion (&optional targets)
  "Use outline-path-completion to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define the elegible
headlines.  When omitted, all headlines in all agenda files are
elegible.
It returns the ID of the entry.  If necessary, the ID is created."
  (let* ((org-refile-targets (or targets '((nil . (:maxlevel . 10)))))
	 (org-refile-use-outline-path 
	  (if (caar org-refile-targets) 'file t))
	 (spos (org-refile-get-location "Entry: "))
	 (pom (and spos (move-marker (make-marker) (nth 3 spos) 
				     (get-file-buffer (nth 1 spos))))))
    (org-id-get pom 'create)
    (move-marker pom nil)))

(defun org-id-get-with-outline-drilling (&optional targets)
  "Use an outline-cycling interface to retrieve the ID of an entry.
This only finds entries in the current buffer, using `org-get-location'.
It returns the ID of the entry.  If necessary, the ID is created."
  (let* ((spos (org-get-location (current-buffer) org-goto-help))
	 (pom (and spos (move-marker (make-marker) (car spos)))))
    (org-id-get pom 'create)
    (move-marker pom nil)))

(defun org-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer."
  (interactive)
  (let ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (switch-to-buffer (marker-buffer m))
    (goto-char m)
    (org-show-context)))    

(defun org-id-find (id &optional markerp)
  "Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a markerp."
  (let ((file (org-id-find-id-file id))
	org-agenda-new-buffers where)
    (when file
      (setq where (org-id-find-id-in-file id file markerp)))
    (unless where
      (org-id-update-id-locations)
      (setq file (org-id-find-id-file id))
      (when file
	(setq where (org-id-find-id-in-file id file markerp))))
    where))

;;; Internal functions

;; Creating new ids

(defun org-id-new (&optional type creator n xrandom)
  "Create a new globally unique ID.
The ID is a string with four colon-separated parts:

1. The type or prefix, given by the argument TYPE, or by the first element

2. The creator, given by the argument CREATOR, or by the second element
   of `org-id-structure' (default is the user's login name).
3. An incremental number linked to the ID type.  This is a number that
   runs for each ID type from 1 up, each time a new ID is created.
   Org-mode keeps track of these numbers in the file `org-id-tracking-file',
   so if you only work on a single computer or synchronize this file,
   this is enough as a unique identifier.  If you work with other people,
   or on different computers, the uniqueness of this number is not certain.
   A specific value for N can be forces by passing it into the function.
4. An extra string guaranteeing the uniqueness of the ID.
   This is either a random string of XRANDOM characters if XRANDOM is an
   integer.  If XRANDOM is the symbol `md5', the extra string is a MD5 digest
   of a string consisting of ID info, the current time, and a random number.
   If you are sure the sequence number (component 3) is unique in your
   setting, the random part can be omitted from the ID.

So a typical ID could look like \"Org:dominik:105:2HtZ\"."
  (unless n (org-id-load))
  (let* ((type (or type (car org-id-structure)))
	 (creator (or creator (nth 1 org-id-structure)))
	 (n-p n)
	 (key (concat type ":" creator))
	 (ass (and (not n-p) (assoc key org-id-values)))
	 (n (or n (1+ (or (cdr ass) 0))))
	 (xrandom (or xrandom (nth 3 org-id-structure)))
	 (random
	  (cond
	   ((not xrandom) nil)
	   ((eq xrandom 'none) nil)
	   ((integerp xrandom) (org-id-random-string xrandom))
	   ((eq xrandom 'md5) (org-id-md5 type creator n))
	   (t nil))))
    (unless n-p
      (if ass
	  (setcdr ass n)
	(push (cons key n) org-id-values))
      (org-id-save))
    (concat type
	    ":" creator
	    ":" (number-to-string n)
	    (if random (concat ":" random)))))

(defun org-id-random-string (n)
  "Return a string of N random characters."
  (let ((rtn "") x)
    (while (>= (setq n (1- n)) 0)
      (setq x (random 62))
      (setq rtn (concat rtn (cond
			     ((< x 10) (char-to-string (+ ?0 x)))
			     ((< x 36) (char-to-string (+ ?A x -10)))
			     ((< x 62) (char-to-string (+ ?a x -36)))
			     (t (error "xxx"))))))
    rtn))

(defun org-id-md5 (type creator n)
  "Return the md5 digest of a string.
This function concatenates ID info with random stuff like the time
and then computes the md5 digest.  The result should be unique."
  (md5 (concat type ":" creator ":" (number-to-string (or n 0)) ":"
	       (system-name) ":"
	       (prin1-to-string (current-time)) ":"
	       (number-to-string (random)))))

;; Storing id indices

(defun org-id-save ()
  "Save `org-id-values' in `org-id-tracking-file'."
  (with-temp-file org-id-tracking-file
    (print org-id-values (current-buffer))))

(defun org-id-load ()
  "Read the data from `org-id-tracking-file'."
  (setq org-id-values nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally org-id-tracking-file)
          (goto-char (point-min))
          (setq org-id-values (read (current-buffer))))
      (error
       (message "Could not read org-id-values from %s. Setting it to nil."
                org-id-tracking-file)))))

;; Storing ID locations (files)

(defun org-id-update-id-locations ()
  "Scan relevant files for ID's.
Store the relation between files and corresponding ID's."
  (interactive)
  (let ((files (append (org-agenda-files)
		       (if (symbolp org-id-extra-files)
			   (symbol-value org-id-extra-files)
			 org-id-extra-files)))
	org-agenda-new-buffers
	file ids reg found id)
    (while (setq file (pop files))
      (setq ids nil)
      (with-current-buffer (org-get-agenda-file-buffer file)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$"
				      nil t)
	      (setq id (org-match-string-no-properties 1))
	      (if (member id found)
		  (error "Duplicate ID \"%s\"" id))
	      (push id found)
	      (push id ids))
	    (push (cons file ids) reg)))))
    (org-release-buffers org-agenda-new-buffers)
    (setq org-agenda-new-buffers nil)
    (setq org-id-locations reg)
    (org-id-locations-save)))

(defun org-id-locations-save ()
  "Save `org-id-locations' in `org-id-locations-file'."
  (with-temp-file org-id-locations-file
    (print org-id-locations (current-buffer))))

(defun org-id-locations-load ()
  "Read the data from `org-id-locations-file'."
  (setq org-id-locations nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally org-id-locations-file)
          (goto-char (point-min))
          (setq org-id-locations (read (current-buffer))))
      (error
       (message "Could not read org-id-values from %s. Setting it to nil."
                org-id-locations-file)))))

(defun org-id-add-location (id file)
  "Add the ID with location FILE to the database of ID loations."
  (unless org-id-locations (org-id-locations-load))
  (catch 'exit
    (let ((locs org-id-locations) list)
      (while (setq list (pop locs))
	(when (equal (file-truename file) (file-truename (car list)))
	  (setcdr list (cons id (cdr list)))
	  (throw 'exit t)))
      (push (list file id) org-id-locations))
    (org-id-locations-save)))

;; Finding entries with specified id

(defun org-id-find-id-file (id)
  "Query the id database for the file in which this ID is located."
  (unless org-id-locations (org-id-locations-load))
  (catch 'found
    (mapc (lambda (x) (if (member id (cdr x))
			  (throw 'found (car x))))
	  org-id-locations)
    nil))

(defun org-id-find-id-in-file (id file &optional markerp)
  "Return the position of the entry ID in FILE.
If that files does not exist, or if it does not contain this ID,
return nil.
The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a marker."
  (let (org-agenda-new-buffers m buf pos)
    (cond
     ((not file) nil)
     ((not (file-exists-p file)) nil)
     (t (with-current-buffer (setq buf (org-get-agenda-file-buffer file))
	  (setq pos (org-find-entry-with-id id))
	  (when pos
	    (if markerp
		(move-marker (make-marker) pos buf)
	      (cons file pos))))))))


(provide 'org-id)

;;; org-id.el ends here

