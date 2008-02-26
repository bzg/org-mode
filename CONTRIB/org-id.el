;;; org-id.el --- Global identifier for Org-mode entries
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 5.22a+
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

;; This file implements globally unique identifiers for Org-mode entries.
;; Identifiers are tored in the entry as an :ID: property.  This file
;; provides functions to create and retrieve such identifies.

;; It provides the following API:

;; org-id-get
;;        Get the ID property of an entry.  Using appropriate arguments
;;        to the function, it can also create the id for this entry.
;;
;; FIXME: more to describe
;; TODO:
;; get/create id at current entry, safe in kill or so.

(require 'org)

(defgroup org-id nil
  "Options concerning global entry identifiers in Org-mode."
  :tag "Org ID"
  :group 'org)

(defcustom org-id-tracking-file "~/.org-id"
  "The file for remembering the last ID number generated."
  :group 'org-id
  :type 'file)

(defcustom org-id-prefix (user-login-name)
  "The string prefix of global id's created by a user.
When working with other people, make sure everyone has their own
ID prefix, in order to guarantee that id's created by differnt people
will always be distinct."
  :group 'org-id
  :type 'string)

(defcustom org-id-random-length 4
  "Non-nil means, insert a random part into id's.
This will be a random alpha-numeric string with as many characters
as given by this option."
  :group 'org-id
  :type 'integer)

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

(defvar org-id-values nil
  "Association list of id keywords with largest index used.")

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

(defun org-id-new (&optional type nrandom)
  "Create a new globally unique id.
The id is a string with two or three colon-separated parts:

1. The type or prefix, given by the argument TYPE, or the value
   of `org-id-prefix' (which defaults to the user name).
2. A hopefully unique number.  This is a number that runs for each ID type
   from 1 up, each time a new ID is created.  Org-mode keeps track
   of these numbers in the file `org-id-tracking-file',  so if you
   only work on a single computer or synchronize this file, this is enough
   as a unique identifier.  If you work with other people, or on different
   computers, the uniqueness of this number is not certain.  In this case
   you should use a value larger than 0 for NRADNOM (which defaults
   to `org-id-random-length').
3. A random string with NRANDOM or `org-id-random-length' characters.
   If that length is 0, the random part will be omitted from the ID.

So a typical ID could look like \"dominik:105:2HtZ\"."
  (org-id-load)
  (let* ((type (or type org-id-prefix))
	 (ass (assoc type org-id-values))
	 (n (1+ (or (cdr ass) 0)))
	 (nrandom (or nrandom org-id-random-length))
	 (random (org-id-random-string nrandom)))
    (if ass
	(setcdr ass n)
      (push (cons type n) org-id-values))
    (org-id-save)
    (concat type ":"
	    (number-to-string n)
	    (if (> nrandom 0) (concat ":" random)))))

(defun org-id-get (&optional pom create type nrandom)
  "Get the ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
However, when CREATE is non nil, create an ID if none is present already.
TYPE and NRANDOM will be passed through to `org-id-new'.
In any case, the ID of the entry is returned."
  (or (org-entry-get pom "ID")
      (and create
	   (let ((id (org-id-new type nrandom)))
	     (org-entry-put pom "ID" id)
	     id))))

(defun org-id-get-with-outline-path-completion (&optional targets)
  "Use outline-path-completion to retrieve the id of an entry.
TARGETS may be a setting for `org-refile-targets' to define the elegible
headlines.  When omitted, all headlines in all agenda files are
elegible.
It returns the id of the entry.  If necessary, the id is created."
  (let* ((org-refile-targets (or targets '((nil . (:maxlevel . 10)))))
	 (org-refile-use-outline-path 
	  (if (caar org-refile-targets) 'file t))
	 (spos (org-refile-get-location "Entry: "))
	 (pom (and spos (move-marker (make-marker) (nth 3 spos) 
				     (get-file-buffer (nth 1 spos))))))
    (org-id-get pom 'create)
    (move-marker pom nil)))

(defun org-id-get-with-outline-drilling (&optional targets)
  "Use an outline-cycling interface to retrieve the id of an entry.
This only finds entries in the current buffer, using `org-get-location'.
It returns the id of the entry.  If necessary, the id is created."
  (let* ((spos (org-get-location (current-buffer) org-goto-help))
	 (pom (and spos (move-marker (make-marker) (car spos)))))
    (org-id-get pom 'create)
    (move-marker pom nil)))

(defvar org-id-locations nil
  "Association list of id's with files.")

(defcustom org-id-extra-files 'org-agenda-multi-occur-extra-files
  "Files to be searched for ID's, besides the agenda files."
  :group 'org-id
  :type
  '(choice
    (symbol :tag "Variable")
    (repeat :tag "List of files"
	    (file))))

(defcustom org-id-locations-file "~/.org-id-locations"
  "The file for remembering the last ID number generated."
  :group 'org-id
  :type 'file)


(defun org-id-update-id-locations ()
  "FIXME"
  (let ((files (append (org-agenda-files)
		       (if (symbolp org-id-extra-files)
			   (symbol-value org-id-extra-files)
			 org-id-extra-files)))
	org-agenda-new-buffers
	file ids reg)
    (while (setq file (pop files))
      (setq ids nil)
      (with-current-buffer (org-get-agenda-file-buffer file)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$"
				      nil t)
	      (push (match-string 1) ids))
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
  "Add the ID with location FILE to the database of id loations."
  (catch 'exit
    (let ((locs org-id-locations) list)
      (while (setq list (pop locs))
	(when (equal (file-truename file) (file-truename (car list)))
	  (setcdr list (cons id list))
	  (throw 'exit t)))
      (push (list file id) org-id-locations))
    (org-id-locations-save)))

(defun org-id-find-id-file (id)
  "Query the id database for the file in which this ID is located."
  (unless org-id-locations (org-id-locations-load))
  (catch 'found
    (mapc (lambda (x) (if (member id (cdr x))
			  (throw 'found (car x))))
	  org-id-locations)))

(defun org-id-find-id-in-file (id file &optional markerp)
  "Return the position of the entry ID in FILE.
If that files does not exist, or if it does not contain this ID,
return nil.
The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a marker."
  (let (org-agenda-new-buffers m buf)
    (cond
     ((not file) nil)
     ((not (file-exists-p file)) nil)
     (t (with-current-buffer (setq buf (org-get-agenda-file-buffer file))
	  (setq pos (org-find-entry-with-id id))
	  (when pos
	    (cons file pos (move-marker (make-marker) pos buf))))))))

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

(defun org-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer."
  (interactive))
    

(provide 'org-id)

;;; org-id.el ends here

