;;; org-contacts-wl.el --- Org-contacts support for Wanderlust

;; Copyright (C) 2011 Michael Markert <markert.michael@googlemail.com>

;; Author: Michael Markert <markert.michael@googlemail.com>
;;
;; This file is NOT part of GNU Emacs.
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
;;; Code:

(require 'std11)
(require 'elmo)
(require 'wl-address)
(require 'wl-summary)

(defun wl-get-from-header-content ()
  (save-excursion
    (set-buffer (org-capture-get :original-buffer))
    (cond
     ((eq major-mode 'wl-summary-mode) (when wl-summary-buffer-elmo-folder
                                         (elmo-message-field
                                          wl-summary-buffer-elmo-folder
                                          (wl-summary-message-number)
                                          'from)))
     ((eq major-mode 'mime-view-mode) (std11-narrow-to-header)
                                      (prog1
                                          (std11-fetch-field "From")
                                        (widen))))))

(defun org-contacts-template-wl-name (&optional return-value)
  (let ((from (wl-get-from-header-content)))
    (or (and from (wl-address-header-extract-realname from))
       return-value
       "%^{Name}")))

(defun org-contacts-template-wl-email (&optional return-value)
  (let ((from (wl-get-from-header-content)))
    (or (and from (wl-address-header-extract-address from))
       return-value
       (concat "%^{" org-contacts-email-property "}p"))))

(provide 'org-contacts-wl)
