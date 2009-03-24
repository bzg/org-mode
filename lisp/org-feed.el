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
 
;; This library allows to create entries in an Org-mode file from
;; RSS feeds.  
;; 
;; Selecting feeds and target locations
;; -----------------------------------
;; 
;; This module is configured through a single variable, `org-feed-alist'.
;; Here is an example, using a notes/tasks feed from reQall.com.
;; 
;;   (setq org-feed-alist
;;         '(("ReQall"
;;            "http://www.reqall.com/user/feeds/rss/a1b2c3....."
;;            "~/org/feeds.org" "ReQall Entries" nil)
;; 
;; With this setup, the command `M-x org-feed-update-all' will
;; collect new entries in the feed at the given URL and create
;; entries as subheading under the "ReQall Entries" heading in the
;; file "~/org.feeds.org".  The final entry in this list can be
;; a filter function to further process the parsed information.  For
;; example, here we turn entries with "<category>Task</category>"
;; into TODO entries by adding the keyword to the title:
;; 
;;   (setq org-feed-alist
;;         '(("ReQall"
;;            "http://www.reqall.com/user/feeds/rss/a1b2c3....."
;;            "~/org/feeds.org" "ReQall Entries"
;;            my-raquall-filter)))
;;
;;   (defun my-requall-filter (e)
;;     (when (equal (plist-get e :category) "Task")
;;       (setq e (plist-put e :title
;;                         (concat "TODO " (plist-get e :title)))))
;;     e)
;; 
;; The filter function may also decide that certain feed items
;; should be ignored, by returning nil instead of the entry.
;; 
;; See the docstring of `org-feed-alist' for more details.
;; 
;; Keeping track of old GUIDs
;; --------------------------
;; 
;; Since Org allows you to delete, archive, or move outline nodes,
;; org-feed needs to keep track of all GUIDs in the feed it has
;; already processed.  It does so by listing them in a special
;; drawer, FEEDGUIDS, under the heading that received the input of
;; te feed.  You should add FEEDGUIDS to your list of drawers
;; in the files that receive feed input:
;; 
;;   #+DRAWERS: PROPERTIES LOGBOOK FEEDGUIDS
;; 
;; Acknowledgements
;; ----------------
;; 
;; It is based on ideas by Brad Bozarth who implemented it using
;; shell and awk scripts, and who in this way made me for the first
;; time look into an RSS feed, showing me how simple this really
;; was.

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
filter       a filter function to modify the property list before
             an Org entry is created from it.

The filter function gets as a argument a property list describing the item.
That list has a property for each field, for example `:title' for the
`<title>' field and `:pubDate' for the publication date.  In addition,
it contains the following properties:

`:item-full-text'   the full text in the <item> tag.
`:guid-permalink'   t when the guid property is a permalink

The filter function can modify the existing fields before an item
is constructed from the `:title', `:pubDate', `:link', `:guid', and
`:description' fields.  For more control, the filter can construct
the Org item itself, by adding a `:formatted-for-org' property that
specifies the complete outline node that should be added.

If the filter returns nil for some entries, these will be marked as seen
but *not* inserted into the inbox."
  :group 'org-feed
  :type '(repeat
	  (list :value ("" "http://" "" "" nil)
	   (string :tag "Name")
	   (string :tag "Feed URL")
	   (file :tag "File for inbox")
	   (string :tag "Headline for inbox")
	   (symbol :tag "Filter Function"))))

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

(defcustom org-feed-after-adding-hook nil
  "Hook that is run after new items have been added to a file.
Depending on `org-feed-save-after-adding', the buffer will already
have been saved."
  :group 'org-feed
  :type 'hook)


(defvar org-feed-buffer "*Org feed*"
  "The buffer used to retrieve a feed.")

(defun org-feed-goto-inbox (file heading)
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

(defun org-feed-get-old-guids (pos)
  "Get the list of old GUIDs from the entry at POS.
This will find the FEEDGUIDS drawer and extract the IDs."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (if (re-search-forward
	   "^[ \t]*:FEEDGUIDS:[ \t]*\n\\([^\000]*?\\)\n[ \t]*:END:"
	   end t)
	  (org-split-string (org-trim (org-match-string-no-properties 1))
			    "[ \t]*\n[ \t]*")
	nil))))

(defun org-feed-add-guids (pos &rest entries)
  "Add GUIDs to the headline at POS."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion (org-end-of-subtree t t)))
	  guid)
      (if (re-search-forward "^[ \t]*:FEEDGUIDS:[ \t]*\n" end t)
	  (goto-char (match-end 0))
	(outline-next-heading)
	(insert "  :FEEDGUIDS:\n  :END:\n")
	(beginning-of-line 0))
      (while entries
	(when (setq guid (plist-get (pop entries) :guid))
	  (insert "  " guid "\n"))))))

(defun org-feed-add-items (pos &rest entries)
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
	(org-paste-subtree level (plist-get entry :formatted-for-org) 'yank))
      (org-mark-ring-push pos))))

(defun org-feed-format (entry)
  "Format ENTRY so that it can be inserted into an Org file.
ENTRY is a property list.  This function adds a `:formatted-for-org' property
and returns the full property list.
If that property is already present, nothing changes."
  (unless (or (not entry)                            ; not an entry at all
	      (plist-get entry :formatted-for-org))  ; already formatted
    (let (lines fmt tmp indent)
      (setq lines (org-split-string (or (plist-get entry :description) "???")
				    "\n")
	    indent "  ")
      (setq fmt
	    (concat
	     "* " (or (plist-get entry :title) (car lines)) "\n"
	     (if (setq tmp (plist-get entry :pubDate))
		 (concat
		  "  ["
		  (substring
		   (format-time-string (cdr org-time-stamp-formats)
				       (org-read-date t t tmp))
		   1 -1)
		  "]\n"))
	     (concat "  :PROPERTIES:\n  :FEED-GUID: "
		     (plist-get entry :guid)
		     "\n  :END:\n")
	     (mapconcat (lambda (x) (concat indent x)) lines "\n") "\n"
	     (if (setq tmp (or (and (plist-get entry :guid-permalink)
				    (plist-get entry :guid))
			       (plist-get entry :link)))
		 (concat "  [[" tmp "]]\n")))
	    entry (plist-put entry :formatted-for-org fmt))))
      entry)

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

;;;###autoload
(defun org-feed-update (feed)
  "Get inbox items from FEED.
FEED can be a string with an association in `org-feed-alist', or
it can be a list structured like an entry in `org-feed-alist'."
  (interactive (list (org-completing-read "Feed name: " org-feed-alist)))
  (if (stringp feed) (setq feed (assoc feed org-feed-alist)))
  (unless feed
    (error "No such feed in `org-feed-alist"))
  (let ((feed-name (car feed))
	(feed-url (nth 1 feed))
	(feed-file (nth 2 feed))
	(feed-headline (nth 3 feed))
	(feed-formatter (nth 4 feed))
	feed-buffer feed-pos
	entries old-guids new new-selected e)
    (setq feed-buffer (org-feed-get-feed feed-url))
    (unless (and feed-buffer (bufferp feed-buffer))
      (error "Cannot get feed %s" feed-name))
    (setq entries (org-feed-parse-feed feed-buffer))
    (ignore-errors (kill-buffer feed-buffer))
    (save-excursion
      (save-window-excursion
	(setq feed-pos (org-feed-goto-inbox feed-file feed-headline))
	(setq old-guids (org-feed-get-old-guids feed-pos))
	(while (setq e (pop entries))
	  (unless (member (plist-get e :guid) old-guids)
	    (push (org-feed-parse-entry e) new)))
	(if (not new)
	    (message "No new items in feed %s" feed-name)
	  ;; Format the new entries
	  (setq new-selected new)
	  (when feed-formatter
	    (setq new-selected (mapcar feed-formatter new-selected)))
	  (setq new-selected (mapcar 'org-feed-format new-selected))
	  (setq new-selected (delq nil new-selected))
	  ;; Insert them
	  (apply 'org-feed-add-items feed-pos new-selected)
	  (apply 'org-feed-add-guids feed-pos new)
	  (goto-char feed-pos)
	  (show-children)
	  (when org-feed-save-after-adding
	    (save-buffer))
	  (message "Added %d new item%s from feed %s to file %s, heading %s"
		   (length new) (if (> (length new) 1) "s" "")
		   feed-name
		   (file-name-nondirectory feed-file) feed-headline)
	  (run-hooks 'org-feed-after-adding-hook))))))

;;;###autoload
(defun org-feed-update-all ()
  "Get inbox items from all feeds in `org-feed-alist'."
  (interactive)
  (mapc 'org-feed-update org-feed-alist))

(provide 'org-feed)

;;; org-feed.el ends here

;; arch-tag: 0929b557-9bc4-47f4-9633-30a12dbb5ae2


