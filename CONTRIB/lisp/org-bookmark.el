;;; org-bookmark.el - Support for links to bookmark
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Tokuya Kameshima <kames AT fa2.so-net.ne.jp>
;; Version: 1.0
;; Keywords: outlines, hypermedia, calendar, wp
;;
;; This file is not part of GNU Emacs.
;;
;; Emacs is free software; you can redistribute it and/or modify
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

(require 'org)
(require 'bookmark)

(org-add-link-type "bookmark" 'org-bookmark-open)
(add-hook 'org-store-link-functions 'org-bookmark-store-link)

(defun org-bookmark-open (bookmark)
  "Visit the bookmark BOOKMARK."
  (bookmark-jump bookmark))

(defun org-bookmark-store-link ()
  "Store a link to the current line's bookmark in Emacs bookmark list window."
  (if (eq major-mode 'bookmark-bmenu-mode)
      (let ((bookmark (bookmark-bmenu-bookmark)))
	(if bookmark
	    (org-store-link-props :link (org-make-link "bookmark:" bookmark)
				  :description bookmark)))))

(provide 'org-bookmark)

;;; org-bookmark.el ends here
