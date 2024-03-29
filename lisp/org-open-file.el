;;; org-open-file.el --- Org open file command                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements `org-open-file' command.

;;; Code:

(require 'org-macs)
(org-assert-version)

(defvar org-wait nil)

(defvar org-link-frame-setup)
(declare-function org-fold-reveal "org-fold")
(declare-function org-link-search "ol")
(declare-function org-link-frame-setup-function "ol" (link-type))

(declare-function mailcap-parse-mailcaps "mailcap" (&optional path force))
(declare-function mailcap-extension-to-mime "mailcap" (extn))
(declare-function mailcap-mime-info
		  "mailcap" (string &optional request no-decode))


(declare-function org-mark-ring-push "org")

(defconst org-file-apps-gnu
  '((remote . emacs)
    (system . mailcap)
    (t . mailcap))
  "Default file applications on a UNIX or GNU/Linux system.
See `org-file-apps'.")

(defconst org-file-apps-macos
  '((remote . emacs)
    (system . "open %s")
    ("ps.gz"  . "gv %s")
    ("eps.gz" . "gv %s")
    ("dvi"    . "xdvi %s")
    ("fig"    . "xfig %s")
    (t . "open %s"))
  "Default file applications on a macOS system.
The system \"open\" is known as a default, but we use X11 applications
for some files for which the OS does not have a good default.
See `org-file-apps'.")

(defconst org-file-apps-windowsnt
  (list '(remote . emacs)
	(cons 'system (lambda (file _path)
			(with-no-warnings (w32-shell-execute "open" file))))
	(cons t (lambda (file _path)
		  (with-no-warnings (w32-shell-execute "open" file)))))
  "Default file applications on a Windows NT system.
The system \"open\" is used for most files.
See `org-file-apps'.")

(defcustom org-file-apps
  '((auto-mode . emacs)
    (directory . emacs)
    ("\\.mm\\'" . default)
    ("\\.x?html?\\'" . default)
    ("\\.pdf\\'" . default))
  "Applications for opening `file:path' items in a document.

\\<org-mode-map>
Org mode uses system defaults for different file types, but you
can use this variable to set the application for a given file
extension.  The entries in this list are cons cells where the car
identifies files and the cdr the corresponding command.

Possible values for the file identifier are:

 \"string\"    A string as a file identifier can be interpreted in different
               ways, depending on its contents:

               - Alphanumeric characters only:
                 Match links with this file extension.
                 Example: (\"pdf\" . \"evince %s\")
                          to open PDFs with evince.

               - Regular expression: Match links where the
                 filename matches the regexp.  If you want to
                 use groups here, use shy groups.

                 Example: (\"\\\\.x?html\\\\\\='\" . \"firefox %s\")
                          (\"\\\\(?:xhtml\\\\|html\\\\)\\\\\\='\" . \"firefox %s\")
                          to open *.html and *.xhtml with firefox.

               - Regular expression which contains (non-shy) groups:
                 Match links where the whole link, including \"::\", and
                 anything after that, matches the regexp.
                 In a custom command string, %1, %2, etc. are replaced with
                 the parts of the link that were matched by the groups.
                 For backwards compatibility, if a command string is given
                 that does not use any of the group matches, this case is
                 handled identically to the second one (i.e. match against
                 file name only).
                 In a custom function, you can access the group matches with
                 (match-string n link).

                 Example: (\"\\\\.pdf::\\\\([0-9]+\\\\)\\\\\\='\" . \
\"evince -p %1 %s\")
                     to open [[file:document.pdf::5]] with evince at page 5.

                 Likely, you will need more entries: without page
                 number; with search pattern; with
                 cross-reference anchor; some combination of
                 options.  Consider simple pattern here and a
                 Lisp function to determine command line
                 arguments instead.  Passing an argument list to
                 `call-process' or `make-process' directly avoids
                 treating some character in peculiar file names
                 as shell specials that prompt parts of said file
                 names to be executed as subcommands.

 `directory'   Matches a directory
 `remote'      Matches a remote file, accessible through tramp.
               Remote files most likely should be visited through Emacs
               because external applications cannot handle such paths.
`auto-mode'    Matches files that are matched by any entry in `auto-mode-alist',
               so all files Emacs knows how to handle.  Using this with
               command `emacs' will open most files in Emacs.  Beware that this
               will also open html files inside Emacs, unless you add
               (\"html\" . default) to the list as well.
 `system'      The system command to open files, like `open' on Windows
               and macOS, and mailcap under GNU/Linux.  This is the command
               that will be selected if you call `org-open-at-point' with a
               double prefix argument (`\\[universal-argument] \
\\[universal-argument] \\[org-open-at-point]').
 t             Default for files not matched by any of the other options.

Possible values for the command are:

 `emacs'       The file will be visited by the current Emacs process.
 `default'     Use the default application for this file type, which is the
               association for t in the list, most likely in the system-specific
               part.  This can be used to overrule an unwanted setting in the
               system-specific variable.
 `system'      Use the system command for opening files, like \"open\".
               This command is specified by the entry whose car is `system'.
               Most likely, the system-specific version of this variable
               does define this command, but you can overrule/replace it
               here.
`mailcap'      Use command specified in the mailcaps
               (also see `mailcap-user-mime-data')..
 string        A command to be executed by a shell; %s will be replaced
               by the path to the file.
 function      A Lisp function, which will be called with two arguments:
               the file path and the original link string, without the
               \"file:\" prefix.

For more examples, see the system specific constants
`org-file-apps-macos'
`org-file-apps-windowsnt'
`org-file-apps-gnu'."
  :group 'org
  :package-version '(Org . "9.4")
  :type '(repeat
	  (cons (choice :value ""
			(string :tag "Extension")
			(const :tag "System command to open files" system)
			(const :tag "Default for unrecognized files" t)
			(const :tag "Remote file" remote)
			(const :tag "Links to a directory" directory)
			(const :tag "Any files that have Emacs modes"
			       auto-mode))
		(choice :value ""
			(const :tag "Visit with Emacs" emacs)
			(const :tag "Use default" default)
			(const :tag "Use the system command" system)
			(string :tag "Command")
			(function :tag "Function")))))

(defcustom org-open-non-existing-files nil
  "Non-nil means `org-open-file' opens non-existing files.

When nil, an error is thrown.

This variable applies only to external applications because they
might choke on non-existing files.  If the link is to a file that
will be opened in Emacs, the variable is ignored."
  :group 'org
  :type 'boolean
  :safe #'booleanp)

(defcustom org-open-directory-means-index-dot-org nil
  "When non-nil a link to a directory really means to \"index.org\".
When nil, following a directory link runs Dired or opens
a finder/explorer window on that directory."
  :group 'org
  :type 'boolean
  :safe #'booleanp)

(defun org--file-default-apps ()
  "Return the default applications for this operating system."
  (pcase system-type
    (`darwin org-file-apps-macos)
    (`windows-nt org-file-apps-windowsnt)
    (_ org-file-apps-gnu)))

(defun org--file-apps-entry-locator-p (entry)
  "Non-nil if ENTRY should be matched against the link by `org-open-file'.

It assumes that is the case when the entry uses a regular
expression which has at least one grouping construct and the
action is either a Lisp form or a command string containing
\"%1\", i.e., using at least one subexpression match as
a parameter."
  (pcase entry
    (`(,selector . ,action)
     (and (stringp selector)
	  (> (regexp-opt-depth selector) 0)
	  (or (and (stringp action)
		   (string-match "%[0-9]" action))
	      (functionp action))))
    (_ nil)))

(defun org--file-apps-regexp-alist (list &optional add-auto-mode)
  "Convert extensions to regular expressions in the cars of LIST.

Also, weed out any non-string entries, because the return value
is used only for regexp matching.

When ADD-AUTO-MODE is non-nil, make all matches in `auto-mode-alist'
point to the symbol `emacs', indicating that the file should be
opened in Emacs."
  (append
   (delq nil
	 (mapcar (lambda (x)
		   (unless (not (stringp (car x)))
		     (if (string-match "\\W" (car x))
			 x
		       (cons (concat "\\." (car x) "\\'") (cdr x)))))
		 list))
   (when add-auto-mode
     (mapcar (lambda (x) (cons (car x) 'emacs)) auto-mode-alist))))

(defun org--open-file-format-command
    (mailcap-command file link match-data)
  "Format MAILCAP-COMMAND to launch viewer for the FILE.

MAILCAP-COMMAND may be an entry from the `org-file-apps' list or viewer
field from mailcap file loaded to `mailcap-mime-data'.  See \"RFC
1524.  A User Agent Configuration Mechanism For Multimedia Mail Format
Information\" (URL `https://www.rfc-editor.org/rfc/rfc1524.html') for
details, man page `mailcap(5)' for brief summary, and Info node
`(emacs-mime) mailcap' for specific related to Emacs.  Only a part of
mailcap specification is supported.

The following substitutions are interpolated in the MAILCAP-COMMAND
string:

- \"%s\" to FILE name passed through
  `convert-standard-filename', so it must be absolute path.

- \"%1\" to \"%9\" groups from MATCH-DATA found in the LINK string by
  the regular expression in the key part of the `org-file-apps' entry.
  (performed by caller).  Not recommended, consider a lisp function
  instead of a shell command.  For example, the following link in an
  Org file

       <file:///usr/share/doc/bash/bashref.pdf::#Redirections::allocate a file>

   may be handled by an `org-file-apps' entry like

       (\"\\\\.pdf\\\\(?:\\\\.[gx]z\\\\|\\\\.bz2\\\\)?::\\\\(#[^:]+\\\\)::\\\\(.+\\\\)\\\\\\='\"
        . \"okular --find %2 %s%1\")

Use backslash \"\\\" to quote percent \"%\" or any other character
including backslash itself.

In addition, each argument is passed through `shell-quote-argument',
so quotes around substitutions should not be used.  For compliance
with mailcap files shipped e.g. in Debian GNU/Linux, single or double
quotes around substitutions are stripped.  It deviates from mailcap
specification that requires file name to be safe for shell and for the
application."
  (let ((spec (list (cons ?s  (convert-standard-filename file))))
        (ngroups (min 9 (- (/ (length match-data) 2) 1))))
    (when (> ngroups 0)
      (set-match-data match-data)
      (dolist (i (number-sequence 1 ngroups))
        (push (cons (+ ?0 i) (match-string-no-properties i link)) spec)))
    (replace-regexp-in-string
     (rx (or (and "\\" (or (group anything) string-end))
             (and (optional (group (any "'\"")))
                  "%"
                  (or (group anything) string-end)
                  (optional (group (backref 2))))))
     (lambda (fmt)
       (let* ((backslash (match-string-no-properties 1 fmt))
              (key (match-string 3 fmt))
              (value (and key (alist-get (string-to-char key) spec))))
         (cond
          (backslash)
          (value (let ((quot (match-string 2 fmt))
                       (subst (shell-quote-argument value)))
                   ;; Remove quotes around the file name - we use
                   ;; `shell-quote-argument'.
                   (if (match-string 4 fmt)
                       subst
                     (concat quot subst))))
          (t (error "Invalid format `%s'" fmt)))))
     mailcap-command nil 'literal)))

;;;###autoload
(defun org-open-file (path &optional in-emacs line search)
  "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.

If no application is found, Emacs simply visits the file.

With optional prefix argument IN-EMACS, Emacs will visit the file.
With a double \\[universal-argument] \\[universal-argument] \
prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file.

Optional LINE specifies a line to go to, optional SEARCH a string
to search for.  If LINE or SEARCH is given, the file will be
opened in Emacs, unless an entry from `org-file-apps' that makes
use of groups in a regexp matches.

If you want to change the way frames are used when following a
link, please customize `org-link-frame-setup'.

If the file does not exist, throw an error."
  (let* ((file (if (equal path "") buffer-file-name
		 (substitute-in-file-name (expand-file-name path))))
	 (file-apps (append org-file-apps (org--file-default-apps)))
	 (apps (cl-remove-if #'org--file-apps-entry-locator-p file-apps))
	 (apps-locator (cl-remove-if-not #'org--file-apps-entry-locator-p
                                         file-apps))
	 (remp (and (assq 'remote apps) (file-remote-p file)))
	 (dirp (unless remp (file-directory-p file)))
	 (file (if (and dirp org-open-directory-means-index-dot-org)
		   (concat (file-name-as-directory file) "index.org")
		 file))
	 (a-m-a-p (assq 'auto-mode apps))
	 (dfile (downcase file))
	 ;; Reconstruct the original link from the PATH, LINE and
	 ;; SEARCH args.
	 (link (cond (line (concat file "::" (number-to-string line)))
		     (search (concat file "::" search))
		     (t file)))
	 (ext
	  (and (string-match "\\`.*?\\.\\([a-zA-Z0-9]+\\(\\.gz\\)?\\)\\'" dfile)
	       (match-string 1 dfile)))
	 (save-position-maybe
	  (let ((old-buffer (current-buffer))
		(old-pos (point))
		(old-mode major-mode))
	    (lambda ()
	      (and (derived-mode-p 'org-mode)
		   (eq old-mode 'org-mode)
		   (or (not (eq old-buffer (current-buffer)))
		       (not (eq old-pos (point))))
		   (org-mark-ring-push old-pos old-buffer)))))
	 cmd link-match-data)
    (cond
     ((member in-emacs '((16) system))
      (setq cmd (cdr (assq 'system apps))))
     (in-emacs (setq cmd 'emacs))
     (t
      (setq cmd (or (and remp (cdr (assq 'remote apps)))
		    (and dirp (cdr (assq 'directory apps)))
		    ;; First, try matching against apps-locator if we
		    ;; get a match here, store the match data for
		    ;; later.
		    (let* ((case-fold-search t)
                           (match (assoc-default link apps-locator
                                                 'string-match)))
		      (if match
			  (progn (setq link-match-data (match-data))
				 match)
			(progn (setq in-emacs (or in-emacs line search))
			       nil))) ; if we have no match in apps-locator,
					; always open the file in emacs if line or search
					; is given (for backwards compatibility)
		    (assoc-default dfile
				   (org--file-apps-regexp-alist apps a-m-a-p)
				   'string-match)
		    (cdr (assoc ext apps))
		    (cdr (assq t apps))))))
    (when (eq cmd 'system)
      (setq cmd (cdr (assq 'system apps))))
    (when (eq cmd 'default)
      (setq cmd (cdr (assoc t apps))))
    (when (eq cmd 'mailcap)
      (require 'mailcap)
      (mailcap-parse-mailcaps)
      (let* ((mime-type (mailcap-extension-to-mime (or ext "")))
	     (command (mailcap-mime-info mime-type)))
	(if (stringp command)
	    (setq cmd command)
	  (setq cmd 'emacs))))
    (when (and (not (eq cmd 'emacs)) ; Emacs has no problems with non-ex files
	       (not (file-exists-p file))
	       (not org-open-non-existing-files))
      (user-error "No such file: %s" file))
    (cond
     ((org-string-nw-p cmd)
      (setq cmd (org--open-file-format-command cmd file link link-match-data))

      (save-window-excursion
	(message "Running %s...done" cmd)
        ;; Handlers such as "gio open" and kde-open5 start viewer in background
        ;; and exit immediately.  Use pipe connection type instead of pty to
        ;; avoid killing children processes with SIGHUP when temporary terminal
        ;; session is finished.
        ;;
        ;; TODO: Once minimum Emacs version is 25.1 or above, consider using
        ;; the `make-process' invocation from 5db61eb0f929 to get more helpful
        ;; error messages.
        (let ((process-connection-type nil))
	  (start-process-shell-command cmd nil cmd))
	(and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))))
     ((or (stringp cmd)
	  (eq cmd 'emacs))
      (funcall (org-link-frame-setup-function 'file) file)
      (widen)
      (cond (line (org-goto-line line)
		  (when (derived-mode-p 'org-mode) (org-fold-reveal)))
	    (search (condition-case err
			(org-link-search search)
		      ;; Save position before error-ing out so user
		      ;; can easily move back to the original buffer.
		      (error (funcall save-position-maybe)
			     (error "%s" (error-message-string err)))))))
     ((functionp cmd)
      (save-match-data
	(set-match-data link-match-data)
	(condition-case nil
	    (funcall cmd file link)
	  ;; FIXME: Remove this check when most default installations
	  ;; of Emacs have at least Org 9.0.
	  ((debug wrong-number-of-arguments wrong-type-argument
		  invalid-function)
	   (user-error "Please see Org News for version 9.0 about \
`org-file-apps'--Lisp error: %S" cmd)))))
     ((consp cmd)
      ;; FIXME: Remove this check when most default installations of
      ;; Emacs have at least Org 9.0.  Heads-up instead of silently
      ;; fall back to `org-link-frame-setup' for an old usage of
      ;; `org-file-apps' with sexp instead of a function for `cmd'.
      (user-error "Please see Org News for version 9.0 about \
`org-file-apps'--Error: Deprecated usage of %S" cmd))
     (t (funcall (org-link-frame-setup-function 'file) file)))
    (funcall save-position-maybe)))

(provide 'org-open-file)

;;; org-open-file.el ends here
