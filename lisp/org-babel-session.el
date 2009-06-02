;;; org-babel-session.el --- session management for org-babel

;; Copyright (C) 2009 Eric Schulte, Dan Davison

;; Author: Eric Schulte, Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel evaluates code in the context of consistent sessions.
;; This file will hold functions for interacting with these sections.
;;
;; For more information see org-babel.org in the top level directory.

;;; Code:
(require 'org-babel)

(defcustom org-babel-session-defaults nil
  "An a-list associating each org-babel interpreter with a
default session buffer."
  :group 'org-babel
  :type 'alist)



(provide 'org-babel-session)
;;; org-babel-session.el ends here
