;;; org-mew.el --- Support for links to messages in Mew
;;
;; Copyright 2008 Bastien Guerry
;;
;; Emacs Lisp Archive Entry
;; Filename: org-mew.el
;; Version: 0.1
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: org, mail, Mew
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; Mew is an Emacs mailer written by Kazu Yamamoto:  http://www.mew.org
;; 
;; This Org add-on provides a way to link to Mew messages.
;;
;; PUT this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-mew)
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)

(org-add-link-type "mew" 'org-mew-open)

(add-hook 'org-store-link-functions 'org-mew-store-link)

(defun org-mew-open (mew-link)
  "Visit the message MSG-NUMBER in FOLDER."
  (when (string-match "\\(+.*\\)+\\+\\([0-9]+\\)" mew-link)
    (let ((folder (match-string 1 mew-link))
	  (msg-num (match-string 2 mew-link)))
      (mew-summary-visit-folder folder)
      (when (mew-summary-search-msg msg-num)
	(if mew-summary-goto-line-then-display
	    (mew-summary-display))))))

(defun org-mew-store-link ()
  "Store a link to a Mew message."
  (when (mew-summary-p)
    (let ((folder (mew-summary-folder-name))
	  (number (mew-summary-message-number))
	  (subject (mew-summary-get-subject)))
      (org-store-link-props
       :type "mew"
       :link (concat "mew:" folder "+" number)
       :description subject))))

(provide 'org-mew)

;;;  User Options, Variables


;;; org-mew.el ends here
