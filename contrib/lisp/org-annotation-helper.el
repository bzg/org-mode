;;; org-annotation-helper.el --- start remember from a web browser
;;
;; Author: bzg AT altern DOT org
;; Author: dmg AT uvic   DOT org
;;
;; Keywords: org remember
;; 
;; Version 0.3,  May 19, 2008
;;
;;; Commentary:
;;
;; [bzg:] This is an adapted version of the planner-mode extension the
;; was first posted by Geert Kloosterman <g.j.kloosterman@gmail.com> on
;; the Planner mailing list.  
;;
;; [dmg:] I have updated and extended the function to allow for
;; handling of the selection (it is now available as a region, so it
;; can be used in a template using %i )
;;
;;
;; We want to be able to pass a URL and document title directly from a
;; web browser to Emacs.
;;
;; We define a remember:// url handler in the browser and use a shell
;; script to handle the protocol.  This script passes the information
;; to a running Emacs process (using emacsclient/gnuclient).  We use 
;; bookmarklets to create the remember:// urls dynamicly.
;;
;; The protocol types currently recognized are:
;; 
;; remember://     start `remember' with the url and title filled in
;; annotation://   similar to `planner-annotation-as-kill' (org?)
;;
;; The urls used internally will have the following form:
;;
;;   remember://<the web page url>::remember::<the title>::remember::<selection>
;;
;; The title will be url-hex-encoded. 
;;
;; The bookmarklets:
;;
;;----------------------------------------------------------------------
;; javascript:location.href='remember://' + location.href + \ 
;;   '::remember::' + escape(document.title) + '::remember::' + escape(window.getSelection())
;;----------------------------------------------------------------------
;; javascript:location.href='annotation://' + location.href + '::remember::' +\
;;     escape(document.title) ;; 
;;----------------------------------------------------------------------
;;
;; The handler
;;
;;----------------------------------------------------------------------
;; #!/bin/sh
;; # org-annotation-helper -- pass a remember-url to emacs
;; #
;; # Author: Geert Kloosterman <g.j.kloosterman@gmail.com>
;; # Date: Sat Nov 19 22:33:18 2005
;; 
;; if [ -z "$1" ]; then
;;     echo "$0: Error: no arguments given!" 1>&2
;;     exit 1
;; fi
;; 
;; # To test uncomment following line
;; #echo $1 >> /tmp/remember.out
;; 
;; emacsclient --eval "(progn (bzg/org-annotation-helper \"$1\" ) nil)"
;;----------------------------------------------------------------------
;; 
;;  To install:
;; 
;; Step 0: Install this module
;;
;;  *  Install this script and require it in your .emacs (or wherever you
;;    want to do it)
;;
;;    (require 'org-annotation-helper)
;;
;;
;; Step 1: Install the remember script
;; 
;;  * Save the handler as a script, and make sure it is executable, i.e.
;;     remember
;;  * Try it:  
;;     Make sure emacs is running and you have started its server mode (server-start)
;;     Run this command from the command line
;;      remember 'remember://http%3A//orgmode.org/::remember::Org-Mode%20Homepage::remember::Notes'
;;     Emacs should now show a remember window with a URL to remember.org
;;
;;
;; Step 2: add two bookmarklets
;;
;; For firefox:
;;
;;  * Right click on the bookmarks area of Firefox. 
;;  * Select new bookmark.
;;  * In location fill the javascript code above (the bookmarklet)
;;  * Make sure "Load this bookmark in the sidebar is deselected
;;
;;  Try it. You should have now a url that starts with "remember://"
;;  and your browser will not know what do to with it.
;;
;; Step 3: Add the handler for the "remember://" URI
;;
;; Firefox
;;
;; To add a protocol handler (eg: remember://) in Firefox, take the
;; following steps:
;;
;; - type in "about:config" in the location bar
;; - right click, select New --> String
;; - the name should be "network.protocol-handler.app.remember" 
;; - the value should be the executable, eg. "org-annotation-helper".
;;   At least under Linux this does not need to be the full path to 
;;   the executable.
;;
;; See http://kb.mozillazine.org/Register_protocol for more details.
;;
;; Opera
;;
;; In Opera add the protocol in the Preferences->Advanced->Programs
;; dialog.
;;
;;   Step 4: Configure a template
;;    I personally  use the following template for this mode
;;
;; 	   (?w "* %u %c \n\n%i" "~/working/trunk/org/bookmarks.org" "Web links")
;;  
;;    %c will be replaced with the hyperlink to the page, displaying the title of the page 
;;    %i will be replaced with the selected text from the browser
;;       By default the new remember notes are placed in the
;;    bookmarks.org file under the "Web links" section, but it can be
;;    easily overriden with C-u C-c C-c
;;
;; Step 5:
;;    Enjoy

(require 'url)

(autoload 'url-unhex-string "url")

(defun bzg/org-annotation-helper (info)
(interactive)
  "Process an externally passed remember:// style url.

URLSTRING consists of a protocol part and a url and title,
separated by ::remember::

The protocol types currently recognized are:

remember://     start `remember' with the url and title
annotation://   similar to `org-annotation-as-kill'."
  (let ((remember-annotation-functions nil))
    ;; The `parse-url' functions break on the embedded url,
    ;; since our format is fixed we'll split the url ourselves.
    (if (string-match  "^\\([^:]*\\):\\(/*\\)\\(.*\\)" info)
      (let* ((b (get-buffer-create "*org-ann*"))
	     (proto (match-string 1 info))
	     (url_title_region (match-string 3 info))
	     (splitparts (split-string url_title_region "::remember::"))
	     (url (url-unhex-string (car splitparts)))
	     (type (if (string-match "^\\([a-z]+\\):" url) 
		       (match-string 1 url)))
	     (title (cadr splitparts))
	     (region (url-unhex-string (caddr splitparts)))
	     orglink)
        (setq title (if (> (length title) 0) (url-unhex-string title)))
        (setq orglink (org-make-link-string url title))
	(org-store-link-props :type type
			      :link url
			      :region region
			      :description title)
	(setq org-stored-links
	      (cons (list url title) org-stored-links))
	;; FIXME can't access %a in the template -- how to set annotation? 
	(raise-frame)
        (cond ((equal proto "remember")
	       (kill-new orglink)
	       (set-buffer b)
	       (set-mark (point))
	       (insert region)
               (org-remember nil ?w))
              ((equal proto "annotation")
               (message "Copied '%s' to the kill-ring." orglink)
               (kill-new orglink))
              (t (error "unrecognized org-helper protocol"))))
      (error "could not parse argument")))
)


(provide 'org-annotation-helper)
