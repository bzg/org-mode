;;; org-eww.el --- Storing link in eww-mode for Org-mode

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Marco Wahl <marcowahlsoft>a<gmailcom>
;; Keywords: link, eww
;; Homepage: http://orgmode.org
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; When this module is active `org-store-link' (often on key C-c l) in
;; a eww buffer stores a link to the current url of the eww buffer.

;; `org-eww-store-link' below is almost the same as
;; `org-w3m-store-link' of the org-w3m module.

;; Hint: There are further features in module org-w3m which might be
;; interesting for org-eww also.


;;; Code:

(require 'org)

(add-hook 'org-store-link-functions 'org-eww-store-link)
(defun org-eww-store-link ()
  "Store a link to the url of a eww buffer."
  (when (eq major-mode 'eww-mode)
    (org-store-link-props
     :type "eww"
     :link eww-current-url
     :url (url-view-url t)
     :description (or eww-current-title eww-current-url))))


(provide 'org-eww)

;;; org-eww.el ends here
