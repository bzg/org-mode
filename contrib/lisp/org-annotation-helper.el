;;; org-annotation-helper.el --- start remember from a web browser
;;
;; Author: bzg AT altern DOT org
;; Author: dmg AT uvic   DOT org
;;
;; Keywords: org remember
;; 
;; Version 0.4, Feb 24, 2009
;;   - Patch by David Moffat to force activation of region
;;
;; Version 0.3b, Sept 18, 2008
;;   - Added one entry to FAQ
;;
;; Version 0.3a, June 3, 2008
;;   - org-fied the FAQ, and fixed typos/grammar.  Big thanks to Nick Dokos.
;;   - Added a new file: README
;;
;; Version 0.3,  May 23, 2008
;; 
;;  - Simplified call to org-remember by using %w template (thanks to Carsten 
;       Dominik)
;;  - Improved documentation (thanks to Nick Dokos and John Rakestraw for this)
;;  - Created  a standalone file for the org-annotation-helper script
;;  - Added org-annotation-helper.html with a FAQ and quick links to the bookmarkets
;;
;; Version 0.2,  May 18, 2008
;;
;;; Commentary:
;;
;; [bzg:] This is an adapted version of the planner-mode extension the
;; was first posted by Geert Kloosterman <g.j.kloosterman@gmail.com> on
;; the Planner mailing list.  
;;
;; [dmg:] I have updated and extended the function to allow for
;; handling of the selection (it is now available as a region, so it
;; can be used in a template using %:region )
;;
;;
;; We want to be able to pass a URL and document title directly from a
;; web browser to Emacs.
;;
;; The idea is to educate the browser about two new protocols "remember"
;; and "annotation", so that when it is asked to handle URLs with these
;; protocols, it will execute a shell script and pass some information
;; to it (the URL of the page it's on, the title and, in the case of the
;; remember protocol, the current selection on that page, if any.)
;;
;; The handlers are invoked using bookmarklets (fake bookmarks whose
;; "location" is some javascript code: when the bookmarklet is
;; activated, the javascript code is executed.) The bookmarklets
;; create remember:// or annotation:// URLs dynamically.
;;
;; When a handler is invoked, it executes a shell script to handle the
;; protocol and passes it the gathered information in a standard
;; format.  The script, in turn, passes the information to a running
;; Emacs process (using emacsclient/gnuclient), calling the function
;; bzg/org-annotate-helper, which is what this file provides.
;;
;; The protocol types currently recognized and the corresponding actions
;; of bzg/org-annotate-helper are:
;; 
;; remember://     assuming you have set up an appropriate remember
;;                 template (see below), start `remember' with the
;;                 url, title and current selection filled in
;;
;; annotation://   make an org link out of the url and the title and
;;                 glom it onto the kill ring, from where it can be
;;                 retrieved either with a yank or through
;;                 org-insert-link.
;;
;; The urls used internally have the following form:
;;
;;   remember://<the web page url>::remember::<the title>::remember::<selection>
;;
;; or
;;
;;   annotation://<the web page url>::remember::<the title>
;;
;; The url, title and selection (if present) will be url-hex-encoded. 
;;
;;
;;======================================================================
;;
;; Here are the pieces you'll need, to set up the bits outside emacs.
;;
;;
;; The javascript code for the two bookmarklets:
;;
;;----------------------------------------------------------------------
;; javascript:location.href='remember://' + location.href + '::remember::' + escape(document.title) + '::remember::' + escape(window.getSelection())
;;----------------------------------------------------------------------
;; javascript:location.href='annotation://' + location.href + '::remember::' + escape(document.title)
;;----------------------------------------------------------------------
;;
;;
;; The shell script that the handlers execute:
;;
;;----------------------------------------------------------------------
;; #!/bin/sh
;; # org-annotation-helper -- pass a {remember,annotation}-url to emacs
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
;;
;;======================================================================
;;
;; Installation.
;; 
;; Step 0: Install this module.
;;
;;  *  Install this file and require it in your .emacs:
;;
;;    (require 'org-annotation-helper)
;;
;;  * Add the following line to your .emacs if it's not there already:
;;
;;    (server-start)
;;
;;
;; Step 1: Install the org-annotation-helper shell script and add
;;         a remember template.
;; 
;;  * Save the shell script in a file in some directory in your $PATH,
;;    and make sure it is executable. In the following, it is assumed
;;    that the file name is "org-annotation-helper".
;;
;;  * Add a ?w `remember template' to org-remember-templates. You can
;;    start by using this template:
;;
;; 	   (?w "* %u %c \n\n%:region" "~/working/trunk/org/bookmarks.org" "Web links")
;;
;;    See section 9.2 of the Org manual for information about
;;    `remember templates'.
;;
;;    "%u" will be replaced by a timestamp, "%c" will be replaced with
;;    the link to the page and labelled with the title of the page, and
;;    "%:region" will be replaced with the selected text from the
;;    browser. By default, the new remember notes are placed in the
;;    bookmarks.org file under the "Web links" section, but that can be
;;    easily overriden with C-u C-c C-c.
;;
;;  * Try the setup so far:
;;
;;     Make sure emacs is running and you have started its server
;;     mode: ``M-x server-start<RET>'' should do it.
;;     
;;     Run this command from the command line:
;;
;;       org-annotation-helper 'remember://http%3A//orgmode.org/::remember::Org-Mode%20Homepage::remember::Notes'
;;
;;     Assuming you used the template above, you should be looking at
;;     a *Remember* buffer that looks like this (minor variations are
;;     possible because of local customizations - the last three lines
;;     of the output are the important one and should be identical):

;;         ## Filing location: Select interactively, default, or last used:
;;         ##     C-u C-c C-c  to select file and header location interactively.
;;         ##         C-c C-c  "~/working/trunk/org/bookmarks.org" -> "* Web links"
;;         ## C-u C-u C-c C-c  "~/working/trunk/org/bookmarks.org" -> "* Web links"
;;         ## To switch templates, use `C-c r'.  To abort use `C-c C-k'.
;;
;;         * [2008-05-21 Wed] [[http://orgmode.org/][Org-Mode Homepage]] 
;;
;;         Notes

;;     Assuming that everything is OK, the script and emacs side of the setup are done.
;;    
;;
;; Step 2: Browser set-up - add two bookmarklets.
;; 
;;  Note: see the file org-annotation-helper-faq.html for a simpler way to add
;;        these bookmarklets
;;
;;  [Firefox specific]
;;
;;  * Create a new bookmark, e.g by selecting Bookmarks/Organize bookmarks... and clicking
;;    on "New Bookmark". In the "Properties" pop-up, give it a unique name.
;;  * In the "Location" field,  fill in the first line of javascript code above.
;;  * Make sure "Load this bookmark in the sidebar" is deselected and click "OK".
;;
;;  * Lather, rinse, repeat for the second line of javascript code.
;;
;;  Try the two bookmarklets. You should get error pop-ups about
;;  unknown protocols "remember" or "annotation", because your browser
;;  will not know what do to with them yet (but see the Firefox 3
;;  section below, if you are running that browser).
;;
;;  You can also look at org-annotation-helper.html for a simple way to add both.
;;
;; Step 3: Browser set-up - add the protocol handlers for the
;;                          "remember://" and "annotation://" URIs.
;;
;;  [Firefox]
;;
;;  To add a protocol handler (eg: remember://) in Firefox, take the
;;  following steps:
;;
;;  * For Firefox 2, type "about:config" in the location bar, right
;;    click to get the pop-up menu, select New --> String, and in the
;;    name field, enter "network.protocol-handler.app.remember".
;;
;;    In Firefox 3, when you first click on the button associated with
;;    the bookmarklet, you should get a pop-up asking if you'd like to
;;    associate the bookmarklet with a particular file. Use the
;;    file-select process to navigate to the org-annotation-helper
;;    script and select it.  You can still edit the about:config
;;    list directly as in Firefox 2.
;;
;;  * the value should be the name of the file containing the shell
;;    script, e.g. in Step 1, we called it "org-annotation-helper".
;;    At least under Linux this does not need to be the full path to
;;    the script.
;;
;;
;;  * Lather, rinse, repeat for the annotation protocol. The string to
;;    add is, in this case, "network.protocol-handler.app.annotation",
;;    and the script is the same as
;;
;;  You should have two new entries like this:
;;  
;;   network.protocol-handler.app.annotation user set string <path>
;;   network.protocol-handler.app.remember   user set string <path>
;;
;;   where <path> is the location where org-annotation-helper is
;;   for example, in my case it is /home/dmg/bin/org-annotation-helper

;;  See http://kb.mozillazine.org/Register_protocol for more details.
;;
;;  [Opera]
;;
;;  In Opera add the protocol in the Preferences->Advanced->Programs
;;  dialog.
;;
;;
;; Step 4: At this point, activating the bookmarklets should invoke
;;    the shell script, which will invoke the
;;    bzg/org-annotation-helper function (below), which will do one of
;;    two things: for a remember:// URL, it will bring up a *Remember*
;;    buffer; for an annotation:// URL, it will squirrel away a link
;;    that you can use with C-c C-l.
;;
;;
;; Debugging notes: if there are problems, it might be useful to run
;; the shell script from the command line (see Step 1 above); it might
;; also be useful to uncomment the "echo" line in the shell
;; script. That will dump the script's argument in /tmp/remember.out,
;; so you can figure out if the browser is passing the right stuff to
;; the script. If it does pass the right stuff, then the emacs side
;; probably has a problem; if not, then the browser side is the likely
;; suspect.

(require 'url)

(autoload 'url-unhex-string "url")

(defun bzg/org-annotation-helper (info)
  "Process an externally passed remember:// style url.

URLSTRING consists of a protocol part and a url and title,
separated by ::remember::

The protocol types currently recognized are:

remember://     start `remember' with the url, title and selection (if any).
annotation://   squirrel away a link of the form [[url][title]] that can
                be used later with \\[org-insert-link]."
  (interactive)
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
               (exchange-point-and-mark t) ;; activate region.. not always on by default
               (org-remember nil ?w)
               (kill-buffer b)       
               )
              ((equal proto "annotation")
               (message "Copied '%s' to the kill-ring." orglink)
               (kill-new orglink))
              (t (error "unrecognized org-helper protocol"))))
      (error "could not parse argument")))
)


(provide 'org-annotation-helper)

