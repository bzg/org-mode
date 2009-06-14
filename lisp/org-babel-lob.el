;;; org-babel-lob.el --- The Library of Babel: off-the-shelf functions for data analysis and plotting using org-babel

;; Copyright (C) 2009 Dan Davison, Eric Schulte

;; Author: Dan Davison, Eric Schulte
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

;; See org-babel.org in the parent directory for more information

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "babel")

(setq org-babel-library-of-babel
      (progn (set-buffer
	      (find-file-noselect "../library-of-babel.org"))
	     (org-babel-get-all-src-block-infos)))

(defun org-babel-execute:babel (body params)
  "Execute a library-of-babel block.

  These blocks do not have their own body. Instead they use
  a :srcname header argument to reference a different source
  block, whose body they use. Source blocks in the library of
  babel should use a standard naming scheme for the variable
  containing the input data for analysis / plotting. E.g. if that
  variable is always called __data__ then one of these bodyless
  babel blocks will call a library of babel block using :var
  __data__=<some reference>. The header args from a babel block
  are appended to the header args from the target block.

  This function is called by `org-babel-execute-src-block'."
  (message "executing babel source code block...")
  (save-window-excursion
    (let* ((srcname (cdr (assoc :srcname params)))
	   (info (or (save-excursion
		       (goto-char (org-babel-find-named-block srcname))
		       (org-babel-get-src-block-info))
		     (gethash srcname org-babel-library-of-babel))))
      (org-babel-execute-src-block nil info params))))

(provide 'org-babel-lob)
