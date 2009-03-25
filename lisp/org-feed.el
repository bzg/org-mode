;;; org-feed.el --- Add RSS feed items to Org files
;;
;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.24trans
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;;  This library allows to create entries in an Org-mode file from
;;  RSS feeds.
;;
;;  Selecting feeds and target locations
;;  -----------------------------------
;;
;;  This module is configured through a single variable, `org-feed-alist'.
;;  Here is an example, using a notes/tasks feed from reQall.com.
;;
;;    (setq org-feed-alist
;;          '(("ReQall"
;;             "http://www.reqall.com/user/feeds/rss/a1b2c3....."
;;             "~/org/feeds.org" "ReQall Entries")
;;
;;  With this setup, the command `M-x org-feed-update-all' will
;;  collect new entries in the feed at the given URL and create
;;  entries as subheadings under the "ReQall Entries" heading in the
;;  file "~/org.feeds.org". 
;;  In addition to these standard arguments, additional keyword-value
;;  pairs are possible.  For example, here we deselect entries with
;;  a description containing "Reqall is typing" using the `:filter'
;;  argument:
;;
;;    (setq org-feed-alist
;;          '(("ReQall"
;;             "http://www.reqall.com/user/feeds/rss/a1b2c3....."
;;             "~/org/feeds.org" "ReQall Entries"
;;             :filter my-reqall-filter)))
;;
;;     (defun my-reqall-filter (e)
;;       (if (string-match "Reqall is typing" (plist-get e :description))
;;           nil
;;         e)
;;
;;  A `:template' entry in the alist would override the template
;;  in `org-feed-default-template' for the construction of the outline
;;  node to be inserted.  You may also write your own function to format
;;  the entry and specify it using the `:formatter' keyword.
;;
;;  Keeping track of previously added entries
;;  -----------------------------------------
;;
;;  Since Org allows you to delete, archive, or move outline nodes,
;;  org-feed.el needs to keep track of which feed items have been added
;;  before, so that they will not be added again.  For this, org-feed.el
;;  stores information in a special drawer, FEEDSTATUS, under the heading
;;  that received the input of the feed.  For this reason, each feed must
;;  have its own headline in an Org file.  You should add FEEDSTATUS
;;  to your list of drawers in the files that receive feed input:
;;
;;    #+DRAWERS: PROPERTIES LOGBOOK FEEDSTATUS
;;
;;  Acknowledgements
;;  ----------------
;;
;;  org-feed.el is based on ideas by Brad Bozarth who implemented a
;;  similar mechanism using shell and awk scripts, and who in this
;;  way made me for the first time look into an RSS feed, showing
;;  how simple this really was.  Because I wanted to include a
;;  solution into Org with as few dependencies as possible, I
;;  reimplemented his ideas in Emacs Lisp.

;;; Code:

(require 'org)

(declare-function url-retrieve-synchronously "url" (url))

(defgroup org-feed  nil
  "Options concerning RSS feeds as inputs for Org files."
  :tag "Org ID"
  :group 'org)

;;;###autoload
(defcustom org-feed-alist nil
  "Alist specifying RSS feeds that should create inputs for Org.
Each entry in this list specified an RSS feed tat should be queried
to create inbox items in Org.  Each entry is a list with the following items:

name         a custom name for this feed
URL          the Feed URL
file         the target Org file where entries should be listed
headline     the headline under which entries should be listed

Additional argumetns can be given using keyword-value pairs:

:filter filter-function
             A function to filter entries before Org nodes are
             created from them.

:template template-string
             The template to create an Org node from a feed item.
             For more control, use the `:formatter'.

:formatter formatter-function
             A function to filter entries before Org nodes are
             created from them.

The filter function gets as a argument a property list describing the item.
That list has a property for each field, for example `:title' for the
`<title>' field and `:pubDate' for the publication date.  In addition,
it contains the following properties:

`:item-full-text'   the full text in the <item> tag
`:guid-permalink'   t when the guid property is a permalink

The filter function should do only one thing:  decide whether this entry
is worth being added now to the Org file.  The filter does not need to worry
if the entry was added in the past, just decide if this is a junk entry,
or something useful.  Entries with a given GUID will be added only once,
the first time they pass the filter.

Entries will be turned onto Org nodes acccording to a template.  If no
template is given here, `org-feed-default-template' is used.  See the
docstring of that variable for information on the template syntax.  If
creating the node requires more logic than a template can provide, define a
:formatter function that will take an entry and return the formatted Org
node as a string."
  :group 'org-feed
  :type '(repeat
	  (list :value ("" "http://" "" "")
	   (string :tag "Name")
	   (string :tag "Feed URL")
	   (file :tag "File for inbox")
	   (string :tag "Headline for inbox")
	   (repeat :inline t
		   (choice
		    (list :inline t :tag "Template"
			  (const :template) (string :tag "Template"))
		    (list :inline t :tag "Filter"
			  (const :filter) (symbol :tag "Filter Function"))
		    (list :inline t :tag "Formatter"
			  (const :filter) (symbol :tag "Formatter Function"))
		    )))))

(defcustom org-feed-default-template "\n* %h\n  %U\n  %description\n  %a\n"
  "Template for the Org node created from RSS feed items.
This is just the default, each feed can specify its own.
Any fields from the feed item can be interpolated into the template with
%name, for example %title, %description, %pubDate etc.  In addition, the
following special escapes are valid as well:

%h      the title, or the first line of the description
%t      the date as a stamp, either from <pubDate> (if present), or
        the current date.
%T      date and time
%u,%U   like %t,%T, but inactive time stamps
%a      A link, from <guid> if that is a permalink, else from <link>"
  :group 'org-feed
  :type '(string :tag "Template"))

(defcustom org-feed-save-after-adding t
  "Non-nil means, save buffer after adding new feed items."
  :group 'org-feed
  :type 'boolean)

(defcustom org-feed-retrieve-method 'url-retrieve-synchronously
  "The method to be used to retrieve a feed URL.
This can be `curl' or `wget' to call these external programs, or it can be
an Emacs Lisp function that will return a buffer containing the content
of the file pointed to by the URL."
  :group 'org-feed
  :type '(choice
	  (const :tag "Internally with url.el" url-retrieve-synchronously)
	  (const :tag "Externally with curl" curl)
	  (const :tag "Externally with wget" wget)
	  (function :tag "Function")))

 (defcustom org-feed-before-adding-hook nil
  "Hook that is run before adding new feed items to a file.
You might want to commit the file in its current state to version control,
for example."
  :group 'org-feed
  :type 'hook)

(defcustom org-feed-after-adding-hook nil
  "Hook that is run after new items have been added to a file.
Depending on `org-feed-save-after-adding', the buffer will already
have been saved."
  :group 'org-feed
  :type 'hook)

(defvar org-feed-buffer "*Org feed*"
  "The buffer used to retrieve a feed.")

;;;###autoload
(defun org-feed-update-all ()
  "Get inbox items from all feeds in `org-feed-alist'."
  (interactive)
  (let ((nfeeds (length org-feed-alist))
	(nnew (apply '+  (mapcar 'org-feed-update org-feed-alist))))
    (message "%s from %d %s"
	     (cond ((= nnew 0) "No new entries")
		   ((= nnew 1) "1 new entry")
		   (t (format "%d new entries" nnew)))
	     nfeeds
	     (if (= nfeeds 1) "feed" "feeds"))))

;;;###autoload
(defun org-feed-update (feed)
  "Get inbox items from FEED.
FEED can be a string with an association in `org-feed-alist', or
it can be a list structured like an entry in `org-feed-alist'."
  (interactive (list (org-completing-read "Feed name: " org-feed-alist)))
  (if (stringp feed) (setq feed (assoc feed org-feed-alist)))
  (unless feed
    (error "No such feed in `org-feed-alist"))
  (catch 'exit
    (let ((name (car feed))
	  (url (nth 1 feed))
	  (file (nth 2 feed))
	  (headline (nth 3 feed))
	  (filter (nth 1 (memq :filter feed)))
	  (formatter (nth 1 (memq :formatter feed)))
	  (template (or (nth 1 (memq :template feed))
			org-feed-default-template))
	  feed-buffer inbox-pos
	  entries old-status status new e guid)
      (setq feed-buffer (org-feed-get-feed url))
      (unless (and feed-buffer (bufferp feed-buffer))
	(error "Cannot get feed %s" name))
      (setq entries (org-feed-parse-feed feed-buffer))
      (ignore-errors (kill-buffer feed-buffer))
      (save-excursion
	(save-window-excursion
	  (setq inbox-pos (org-feed-goto-inbox-internal file headline))
	  (setq old-status (org-feed-read-previous-status inbox-pos))
	  ;; Add the "added" status to the appropriate entries
	  (setq entries (mapcar (lambda (e)
				  (setq e (plist-put e :added
						     (nth 1 (assoc 
							     (plist-get e :guid)
							     old-status)))))
				entries))
	  ;; Find out which entries are new
	  (setq new (delq nil (mapcar (lambda (e)
					(if (plist-get e :added) nil e))
				      entries)))
	  ;; Parse the entries fully
	  (setq new (mapcar 'org-feed-parse-entry new))
	  ;; Run the filter
	  (when filter
	    (setq new (delq nil (mapcar filter new))))
	  (when (not new)
	    (message "No new items in feed %s" name)
	    (throw 'exit 0))
	  ;; Format the new entries into an alist with GUIDs in the car
	  (setq new (mapcar
		     (lambda (e)
		       (list (plist-get e :guid)
			     (org-feed-format-entry e template formatter)))
		     new))
		
	  ;; Construct the new status
	  (setq status
		(mapcar
		 (lambda (e)
		   (setq guid (plist-get e :guid))
		   (list guid (if (assoc guid new) t (plist-get e :added))))
		 entries))
	  ;; Insert the new items
	  (org-feed-add-items inbox-pos new)

	  ;; Write the new status
	  (org-feed-write-status inbox-pos status)
	  
	  ;; Normalize the visibility of the inbox tree
	  (goto-char inbox-pos)
	  (hide-subtree)
	  (show-children)
	  (org-cycle-hide-drawers 'children)
	  (when org-feed-save-after-adding (save-buffer))
	  (message "Added %d new item%s from feed %s to file %s, heading %s"
		   (length new) (if (> (length new) 1) "s" "")
		   name
		   (file-name-nondirectory file) headline)
	  (run-hooks 'org-feed-after-adding-hook)
	  (length new))))))

;;;###autoload
(defun org-feed-goto-inbox (feed)
  "Go to the inbox that captures the feed named FEED."
  (interactive
   (list (if (= (length org-feed-alist) 1)
	     (car org-feed-alist)
	   (org-completing-read "Feed name: " org-feed-alist))))
  (if (stringp feed) (setq feed (assoc feed org-feed-alist)))
  (unless feed
    (error "No such feed in `org-feed-alist"))
  (org-feed-goto-inbox-internal (nth 2 feed) (nth 3 feed)))

(defun org-feed-goto-inbox-internal (file heading)
  "Find or create HEADING in FILE.
Switch to that buffer, and return the position of that headline."
  (find-file file)
  (widen)
  (goto-char (point-min))
  (if (re-search-forward
       (concat "^\\*+[ \t]+" heading "[ \t]*\\(:.*?:[ \t]*\\)?$")
       nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))
      (insert "\n\n* " heading "\n\n")
      (org-back-to-heading t))
  (point))

(defun org-feed-read-previous-status (pos)
  "Get the alist of old GUIDs from the entry at POS.
This will find the FEEDSTATUS drawer and extract the alist."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (if (re-search-forward
	   "^[ \t]*:FEEDSTATUS:[ \t]*\n\\([^\000]*?\\)\n[ \t]*:END:"
	   end t)
	  (read (match-string 1))
	nil))))

(defun org-feed-write-status (pos status)
  "Write the feed status to the FEEDSTATUS drawer."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion (org-end-of-subtree t t)))
	  guid)
      (if (re-search-forward "^[ \t]*:FEEDSTATUS:[ \t]*\n" end t)
	  (progn
	    (goto-char (match-end 0))
	    (delete-region (point)
			   (save-excursion
			     (and (re-search-forward "^[ \t]*:END:" nil t)
				  (match-beginning 0)))))
	(outline-next-heading)
	(insert "  :FEEDSTATUS:\n  :END:\n")
	(beginning-of-line 0))
      (insert (pp-to-string status)))))

(defun org-feed-add-items (pos entries)
  "Add the formatted items to the headline as POS."
  (let (entry level)
    (save-excursion
      (goto-char pos)
      (unless (looking-at org-complex-heading-regexp)
	(error "Wrong position"))
      (setq level (org-get-valid-level (length (match-string 1)) 1))
      (org-end-of-subtree t t)
      (skip-chars-backward " \t\n")
      (beginning-of-line 2)
      (setq pos (point))
      (while (setq entry (pop entries))
	(insert "\n")
	(org-paste-subtree level (nth 1 entry)))
      (org-mark-ring-push pos))))

(defun org-feed-format-entry (entry template formatter)
  "Format ENTRY so that it can be inserted into an Org file.
ENTRY is a property list.  This function adds a `:formatted-for-org' property
and returns the full property list.
If that property is already present, nothing changes."
  (if formatter
      (funcall formatter entry)
    (let (dlines fmt tmp indent time
		 v-h v-t v-T v-u v-U v-a)
      (setq dlines (org-split-string (or (plist-get entry :description) "???")
				     "\n")
	    v-h (or (plist-get entry :title) (car dlines) "???")
	    time (or (if (plist-get entry :pubDate)
			 (org-read-date t t (plist-get entry :pubDate)))
		     (current-time))
	    v-t (format-time-string (org-time-stamp-format nil nil) time)
	    v-T (format-time-string (org-time-stamp-format t   nil) time)
	    v-u (format-time-string (org-time-stamp-format nil t)   time)
	    v-U (format-time-string (org-time-stamp-format t   t)   time)
	    v-a (if (setq tmp (or (and (plist-get entry :guid-permalink)
				       (plist-get entry :guid))
				  (plist-get entry :link)))
		    (concat "[[" tmp "]]\n")
		  ""))
      (with-temp-buffer
	(insert template)
	(goto-char (point-min))
	(while (re-search-forward "%\\([a-zA-Z]+\\)" nil t)
	  (setq name (match-string 1))
	  (cond
	   ((member name '("h" "t" "T" "u" "U" "a"))
	    (replace-match (symbol-value (intern (concat "v-" name))) t t))
	   ((setq tmp (plist-get entry (intern (concat ":" name))))
	    (save-excursion
	      (save-match-data
		(beginning-of-line 1)
		(when (looking-at (concat "^\\([ \t]*\\)%" name "[ \t]*$"))
		  (setq tmp (org-feed-make-indented-block
			     tmp (org-get-indentation))))))
	    (replace-match tmp t t))))
	(buffer-string)))))

(defun org-feed-make-indented-block (s n)
  "Add indentaton of N spaces to a multiline string S."
  (if (not (string-match "\n" s))
      s
    (mapconcat 'identity
	       (org-split-string s "\n")
	       (concat "\n" (make-string n ?\ )))))

(defun org-feed-get-feed (url)
  "Get the RSS feed file at URL and return the buffer."
  (cond
   ((eq org-feed-retrieve-method 'url-retrieve-synchronously)
    (url-retrieve-synchronously url))
   ((eq org-feed-retrieve-method 'curl)
    (ignore-errors (kill-buffer org-feed-buffer))
    (call-process "curl" nil org-feed-buffer nil url)
    org-feed-buffer)
   ((eq org-feed-retrieve-method 'wget)
    (ignore-errors (kill-buffer org-feed-buffer))
    (call-process "curl" nil org-feed-buffer nil "-q" "-O" "-" url)
    org-feed-buffer)
   ((functionp org-feed-retrieve-method)
    (funcall org-feed-retrieve-method url))))

(defun org-feed-parse-feed (buffer)
  "Parse BUFFER for RS feed entries.
Returns a list of entries, with each entry a property list,
containing the properties `:guid' and `:item-full-text'."
  (let (entries beg end item guid entry)
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "<item>" nil t)
	(setq beg (point)
	      end (and (re-search-forward "</item>" nil t)
		       (match-beginning 0)))
	(setq item (buffer-substring beg end)
	      guid (if (string-match "<guid\\>.*?>\\(.*?\\)</guid>" item)
		       (org-match-string-no-properties 1 item)))
	(setq entry (list :guid guid :item-full-text item))
	(push entry entries)
	(widen)
	(goto-char end))
      (nreverse entries))))

(defun org-feed-parse-entry (entry)
  "Parse the `:item-full-text' field for xml tags and create new properties."
  (with-temp-buffer
    (insert (plist-get entry :item-full-text))
    (goto-char (point-min))
    (while (re-search-forward "<\\([a-zA-Z]+\\>\\).*?>\\([^\000]*?\\)</\\1>"
			      nil t)
      (setq entry (plist-put entry
			     (intern (concat ":" (match-string 1)))
			     (match-string 2))))
    (goto-char (point-min))
    (unless (re-search-forward "isPermaLink[ \t]*=[ \t]*\"false\"" nil t)
      (setq entry (plist-put entry :guid-permalink t))))
  entry)

(provide 'org-feed)

;;; org-feed.el ends here

;; arch-tag: 0929b557-9bc4-47f4-9633-30a12dbb5ae2


;1. parse all items
;2. filter with user filter
;3. Remove GUIDs that we have already *added* before
;4. Format, using user or built-in formatter
;5. add new items
;6. Store the guids from step 2, after the filtering
;   This means that the feed could go back, have the entry
;   pass the filter, and then it will be added.;

					;Each item will be added once, when it first passes the filter.