;;; litorgy-reference.el --- litorgical functions for referencing external data

;; Copyright (C) 2009 Eric Schulte, Dan Davison, Austin F. Frank

;; Author: Eric Schulte, Dan Davison, Austin F. Frank
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

;; Functions for referencing data from the header arguments of a
;; litorgical block.  The syntax of such a reference should be
;;
;;   #+VAR: variable-name file resource-id:name
;;
;; - variable-name :: the name of the variable to which the value
;;                    will be assigned
;;                    
;; - file :: path to the file containing the resource, or omitted if
;;           resource is in the current file
;;
;; - resource-id :: the id or name of the resource, or 'previous' to
;;                  grab the previous table, or 'next' to grab the
;;                  next table
;;
;; So an example of a simple src block referencing table data in the
;; same file would be
;;
;; #+var: table previous
;; #+begin_src emacs-lisp
;; (message table)
;; #+end_src
;;

;;; Code:
(require 'litorgy)

(provide 'litorgy-reference)
;;; litorgy-reference.el ends here
