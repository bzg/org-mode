;;; org-browser-url.el --- Bookmark from a browser into org links

;; Author: Ross Patterson <me@rpatterson.net>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; Once a bookmarklet and a app handler are setup in your browser,
;; the functions here support using the bookmarklet to add links to
;; the org links ring.
;;
;; Much of this is taken from or modeled after
;; org-annotation-helper.el
;;
;; Installation and Activation
;; ---------------------------
;;
;; Step 1: Install this library on your path and enable it in your
;;         $HOME/.emacs:
;;
;;    (require 'org-browser-url)
;;
;; Step 2: Install the handler script
;;
;;  * Save the following script to an appropriate place and make sure
;;    it is executable:
;;
;;    #!/bin/sh
;;    # org-browser-url-store - Store URLs in the org links ring
;;    emacsclient --eval "(let ((org-browser-url-args \"$*\"))\
;;        (call-interactively 'org-store-link))"
;;
;;  * Make sure emacs is running with server mode started:
;;
;;    (server-start)
;;
;;  * Test the script from the command line
;;
;;    $ org-browser-url-store \
;;      'org-browser-url-store:///Link%20Description/http://foo.com'
;;
;;  * Insert the link in an org-mode buffer with C-c C-l
;;
;; Step 3: Add the handler to your browser
;;
;;  * For Firefox:
;;    - type in "about:config" in the location bar
;;    - right click, select "New", then "String"
;;    - enter the name:
;;      "network.protocol-handler.app.org-browser-url-store"
;;    - leave the value blank
;;
;;    See http://kb.mozillazine.org/Register_protocol for more details.
;;
;;  * For Opera add the protocol in the
;;    Preferences->Advanced->Programs dialog.
;;
;; Step 4: Add bookmarklet
;;
;;  * Create a new bookmark with the following location:
;;
;;    javascript:location.href='org-browser-url-store:///'+\
;;    escape(document.title)+'/'+location.href
;;
;;    When you first use the bookmarklet, Firefox will prompt you for
;;    the script.  Point it to the full path of the script.

;;; Code:

(require 'org)
(require 'url)

(defun org-browser-url-store-link ()
  "Store a browser URL as an org link from the bookmarklet"
  (if (boundp 'org-browser-url-args)
      (let* ((stored (url-generic-parse-url org-browser-url-args))
             (path (split-string (aref stored 6) "/"))
             (parsed (url-generic-parse-url
                      (mapconcat 'identity (cddr path) "/")))
             (type (aref parsed 1))
             (link (aset parsed 7 (aref stored 7)))
             (link (url-recreate-url parsed))
             (description (url-unhex-string (nth 1 path))))
        (org-store-link-props
         :type type :link link :description description))))

(add-hook 'org-store-link-functions 'org-browser-url-store-link)

(provide 'org-browser-url)