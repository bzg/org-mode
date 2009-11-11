;;; org-babel-sql.el --- org-babel functions for sql evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
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

;; Org-Babel support for evaluating sql source code.
;;
;; SQL is somewhat unique in that there are many different engines for
;; the evaluation of sql (Mysql, PostgreSQL, etc...), so much of this
;; file will have to be implemented engine by engine.
;;
;; Also SQL evaluation generally takes place inside of a database.
;;
;; For now lets just allow a generic ':cmdline' header argument.
;;
;; TODO:
;;
;; - support for sessions
;; - add more useful header arguments (user, passwd, database, etc...)
;; - support for more engines (currently only supports mysql)
;; - what's a reasonable way to drop table data into SQL?
;; 

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "sql")

(add-to-list 'org-babel-tangle-langs '("sql" "sql"))

(defun org-babel-execute:sql (body params)
  "Execute a block of Sql code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Sql source code block")
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
         (cmdline (cdr (assoc :cmdline params)))
         (engine (cdr (assoc :engine params)))
         (in-file (make-temp-file "org-babel-sql-in"))
         (out-file (or (cdr (assoc :out-file params))
                       (make-temp-file "org-babel-sql-out")))
         (command (case (intern engine)
                    ('mysql (format "mysql %s -e \"source %s\" > %s"
                                    (or cmdline "") in-file out-file))
                    (t (error "no support for the %s sql engine")))))
    (with-temp-file in-file (insert body))
    (message command)
    (shell-command command)
    (with-temp-buffer
      (org-table-import out-file nil)
      (org-table-to-lisp))))

(defun org-babel-prep-session:sql (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "sql sessions not yet implemented"))

(provide 'org-babel-sql)
;;; org-babel-sql.el ends here
