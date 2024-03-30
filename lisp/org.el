;;; org.el --- Outline-based notes management and organizer -*- lexical-binding: t; -*-

;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2004-2026 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Maintainer: Ihor Radchenko <yantar92@posteo.net>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;; Package-Requires: ((emacs "28.2") (compat "31"))

;; Version: 10.0-pre

;; This file is part of GNU Emacs.
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Org is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org mode develops organizational tasks around NOTES files that
;; contain information about projects as plain text.  Org mode is
;; implemented on top of outline-mode, which makes it possible to keep
;; the content of large files well structured.  Visibility cycling and
;; structure editing help to work with the tree.  Tables are easily
;; created with a built-in table editor.  Org mode supports ToDo
;; items, deadlines, time stamps, and scheduling.  It dynamically
;; compiles entries into an agenda that utilizes and smoothly
;; integrates much of the Emacs calendar and diary.  Plain text
;; URL-like links connect to websites, emails, Usenet messages, BBDB
;; entries, and any files related to the projects.  For printing and
;; sharing of notes, an Org file can be exported as a structured ASCII
;; file, as HTML, or (todo and agenda items only) as an iCalendar
;; file.  It can also serve as a publishing tool for a set of linked
;; webpages.
;;
;; Installation and Activation
;; ---------------------------
;; See the corresponding sections in the manual at
;;
;;   https://orgmode.org/org.html#Installation
;;
;; Documentation
;; -------------
;; The documentation of Org mode can be found in the TeXInfo file.  The
;; distribution also contains a PDF version of it.  At the Org mode website,
;; you can read the same text online as HTML.  There is also an excellent
;; reference card made by Philip Rooke.  This card can be found in the
;; doc/ directory.
;;
;; A list of recent changes can be found at
;; https://orgmode.org/Changes.html
;;
;;; Code:

(defvar org-inlinetask-min-level)
(defvar org-footnote-definition-re)
(defvar org-comment-regexp)
(declare-function org-collect-keywords "org-element" (keywords &optional unique directory))
(declare-function org-element-org-data "org-element" ())

;;;; Require other packages

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'map)

(eval-when-compile (require 'gnus-sum))

(require 'calendar)
(require 'find-func)
(require 'format-spec)
(require 'thingatpt)

(condition-case err
    (load (concat (file-name-directory load-file-name)
		  "org-loaddefs")
	  nil t nil t)
  (error
   (message "WARNING: org-loaddefs.el file could not be loaded from where org.el is loaded - %S" err)
   (sit-for 3)
   (message "You need to run \"make\" or \"make autoloads\" from Org lisp directory")
   (sit-for 3)))

(require 'org-macs)
(require 'org-compat)
(require 'org-keys)
(require 'ol)
(require 'oc)
(require 'org-table)
(require 'org-fold)
(require 'org-element)
(require 'org-dnd)
(require 'org-cdlatex)
(require 'org-indent-static)
(require 'org-fill)
(require 'org-comment)
(require 'org-time)
(require 'org-element-timestamp)
(require 'org-read-date)
(require 'org-timestamp)
(require 'org-tags)
(require 'org-font-lock)
(require 'org-dblock)
(require 'org-property)
(require 'org-priority)
(require 'org-log-note)
(require 'org-mode)
(require 'org-agenda-files)
(require 'org-map)
(require 'org-sparse-tree)
(require 'org-open-file)
(require 'org-mark-ring)

(require 'org-cycle)
(defalias 'org-global-cycle #'org-cycle-global)
(defalias 'org-overview #'org-cycle-overview)
(defalias 'org-content #'org-cycle-content)
(defalias 'org-reveal #'org-fold-reveal)
(defalias 'org-force-cycle-archived #'org-cycle-force-archived)

(defvar org-property-re)

(defvar org-heading-regexp "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Matches a headline, putting stars and text into groups.
Stars are put in group 1 and the trimmed body in group 2.")

(declare-function calendar-check-holidays "holidays" (date))
(declare-function Info-goto-node "info" (nodename &optional fork strict-case))
(declare-function isearch-no-upper-case-p "isearch" (string regexp-flag))
(declare-function feature-file "loadhist" (feature))
(declare-function org-add-archive-files "org-archive" (files))
(declare-function org-agenda-entry-get-agenda-timestamp "org-agenda" (pom))
(declare-function org-agenda-todo-yesterday "org-agenda" (&optional arg))
(declare-function org-agenda-list "org-agenda" (&optional arg start-day span with-hour))
(declare-function org-agenda-redo "org-agenda" (&optional all))
(declare-function org-archive-subtree "org-archive" (&optional find-done))
(declare-function org-archive-subtree-default "org-archive" ())
(declare-function org-archive-to-archive-sibling "org-archive" ())
(declare-function org-attach "org-attach" ())
(declare-function org-attach-dir "org-attach"
		  (&optional create-if-not-exists-p no-fs-check))
(declare-function org-babel-do-in-edit-buffer "ob-core" (&rest body) t)
(declare-function org-babel-tangle-file "ob-tangle" (file &optional target-file lang))
(declare-function org-beamer-mode "ox-beamer" (&optional prefix) t)
(declare-function org-clock-auto-clockout "org-clock" ())
(declare-function org-clock-cancel "org-clock" ())
(declare-function org-clock-display "org-clock" (&optional arg))
(declare-function org-clock-get-last-clock-out-time "org-clock" ())
(declare-function org-clock-goto "org-clock" (&optional select))
(declare-function org-clock-in "org-clock" (&optional select start-time))
(declare-function org-clock-out "org-clock" (&optional switch-to-state fail-quietly at-time))
(declare-function org-clock-out-if-current "org-clock" ())
(declare-function org-clock-remove-overlays "org-clock" (&optional beg end noremove))
(declare-function org-clock-report "org-clock" (&optional arg))
(declare-function org-clock-sum "org-clock" (&optional tstart tend headline-filter propname))
(declare-function org-clock-sum-current-item "org-clock" (&optional tstart))
(declare-function org-clock-timestamps-down "org-clock" (&optional n))
(declare-function org-clock-timestamps-up "org-clock" (&optional n))
(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-clocktable-shift "org-clock" (dir n))
(declare-function org-columns-quit "org-colview" ())
(declare-function org-columns-insert-dblock "org-colview" ())
(declare-function org-duration-from-minutes "org-duration" (minutes &optional fmt canonical))
(declare-function org-duration-to-minutes "org-duration" (duration &optional canonical))
(declare-function org-export-dispatch "ox" (&optional arg))
(declare-function org-export-get-backend "ox" (name))
(declare-function org-export-get-environment "ox" (&optional backend subtreep ext-plist))
(declare-function org-feed-goto-inbox "org-feed" (feed))
(declare-function org-feed-update-all "org-feed" ())
(declare-function org-goto "org-goto" (&optional alternative-interface))
(declare-function org-id-get-create "org-id" (&optional force))
(declare-function org-inlinetask-outline-regexp "org-inlinetask" ())
(declare-function org-inlinetask-demote "org-inlinetask" ())
(declare-function org-inlinetask-promote "org-inlinetask" ())
(declare-function org-latex-make-preamble "ox-latex" (info &optional template snippet?))
(declare-function org-num-mode "org-num" (&optional arg))
(declare-function org-plot/gnuplot "org-plot" (&optional params))
(declare-function org-persist-load "org-persist")
(declare-function org-tags-view "org-agenda" (&optional todo-only match))
(declare-function org-timer "org-timer" (&optional restart no-insert))
(declare-function org-timer-item "org-timer" (&optional arg))
(declare-function org-timer-pause-or-continue "org-timer" (&optional stop))
(declare-function org-timer-start "org-timer" (&optional offset))
(declare-function org-toggle-archive-tag "org-archive" (&optional find-done))
(declare-function org-latex-preview "org-latex-preview" (&optional mode))

(defvar org-agenda-buffer-name)
(defvar org-element-paragraph-separate)
(defvar org-element-cache-map-continue-from)
(defvar org-element--timestamp-regexp)
(defvar org-indent-indentation-per-level)
(defvar org-radio-target-regexp)
(defvar org-target-link-regexp)
(defvar org-target-regexp)
(defvar org-id-overriding-file-name)
(defvar org-edit-src-content-indentation)

;; load languages based on value of `org-babel-load-languages'
(defvar org-babel-load-languages)

(defvar crm-separator)  ; dynamically scoped param

;;;###autoload
(defun org-babel-do-load-languages (sym value)
  "Load the languages defined in `org-babel-load-languages'."
  (set-default-toplevel-value sym value)
  (dolist (pair org-babel-load-languages)
    (let ((active (cdr pair)) (lang (symbol-name (car pair))))
      (if active
	  (require (intern (concat "ob-" lang)))
	(fmakunbound
	 (intern (concat "org-babel-execute:" lang)))
	(fmakunbound
	 (intern (concat "org-babel-expand-body:" lang)))))))


;;;###autoload
(defun org-babel-load-file (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let ((tangled-file (concat (file-name-sans-extension file) ".el")))
    ;; Tangle only if the Elisp file is older than the Org file.
    ;; Catch the case when the .el file exists while the .org file is missing.
    (unless (file-exists-p file)
      (error "File to tangle does not exist: %s" file))
    (when (file-newer-than-file-p file tangled-file)
      (org-babel-tangle-file file
                             tangled-file
                             (rx string-start
                                 (or "emacs-lisp" "elisp")
                                 string-end))
      ;; Make sure that tangled file modification time is
      ;; updated even when `org-babel-tangle-file' does not make changes.
      ;; This avoids re-tangling changed FILE where the changes did
      ;; not affect the tangled code.
      (when (file-exists-p tangled-file)
        (set-file-times tangled-file)))
    (if compile
	(progn
	  (byte-compile-file tangled-file)
	  (load-file (byte-compile-dest-file tangled-file))
	  (message "Compiled and loaded %s" tangled-file))
      (load-file tangled-file)
      (message "Loaded %s" tangled-file))))

(defcustom org-babel-load-languages '((emacs-lisp . t))
  "Languages which can be evaluated in Org buffers.
\\<org-mode-map>
This list can be used to load support for any of the available
languages with babel support (see info node `(org) Languages').  Each
language will depend on a different set of system executables and/or
Emacs modes.

When a language is \"loaded\", code blocks in that language can
be evaluated with `org-babel-execute-src-block', which is bound
by default to \\[org-ctrl-c-ctrl-c].

The `org-babel-no-eval-on-ctrl-c-ctrl-c' option can be set to
remove code block evaluation from \\[org-ctrl-c-ctrl-c].  By
default, only Emacs Lisp is loaded, since it has no specific
requirement."
  :group 'org-babel
  :set 'org-babel-do-load-languages
  :package-version '(Org . "9.6")
  :type '(alist :tag "Babel Languages"
		:key-type
		(choice
		 (const :tag "Awk" awk)
		 (const :tag "C, D, C++, and cpp" C)
		 (const :tag "R" R)
                 (const :tag "Calc" calc)
		 (const :tag "Clojure and ClojureScript" clojure)
		 (const :tag "CSS" css)
		 (const :tag "Ditaa" ditaa)
		 (const :tag "Dot" dot)
                 (const :tag "Emacs Lisp" emacs-lisp)
                 (const :tag "Eshell" eshell)
		 (const :tag "Forth" forth)
		 (const :tag "Fortran" fortran)
		 (const :tag "GnuPlot" gnuplot)
		 (const :tag "Groovy" groovy)
		 (const :tag "Haskell" haskell)
                 (const :tag "Java" java)
		 (const :tag "JavaScript" js)
                 (const :tag "Julia" julia)
                 (const :tag "LaTeX" latex)
                 (const :tag "LilyPond" lilypond)
		 (const :tag "Lisp" lisp)
                 (const :tag "Lua" lua)
		 (const :tag "Makefile" makefile)
		 (const :tag "Maxima" maxima)
                 (const :tag "OCaml" ocaml)
		 (const :tag "Octave and MatLab" octave)
		 (const :tag "Org" org)
		 (const :tag "Perl" perl)
                 (const :tag "Processing" processing)
		 (const :tag "PlantUML" plantuml)
		 (const :tag "Python" python)
		 (const :tag "Ruby" ruby)
		 (const :tag "Sass" sass)
		 (const :tag "Scheme" scheme)
		 (const :tag "Screen" screen)
                 (const :tag "Sed" sed)
		 (const :tag "Shell Script" shell)
                 (const :tag "Sql" sql)
		 (const :tag "Sqlite" sqlite))
		:value-type (boolean :tag "Activate" :value t)))

;;;; Customization variables
(defcustom org-clone-delete-id nil
  "Remove ID property of clones of a subtree.
When non-nil, clones of a subtree don't inherit the ID property.
Otherwise they inherit the ID property with a new unique
identifier."
  :type 'boolean
  :version "24.1"
  :group 'org-id)

;;; Version
(org-check-version)

;;;###autoload
(defun org-version (&optional here full message)
  "Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
  (let ((org-dir (ignore-errors (org-find-library-dir "org")))
        (save-load-suffixes load-suffixes)
	(load-suffixes (list ".el"))
	(org-install-dir
	 (ignore-errors (org-find-library-dir "org-loaddefs"))))
    (unless (and (fboundp 'org-release) (fboundp 'org-git-version))
      (org-load-noerror-mustsuffix (concat org-dir "org-version")))
    (let* ((load-suffixes save-load-suffixes)
	   (release (org-release))
	   (git-version (org-git-version))
	   (version (format "Org mode version %s (%s @ %s)"
			    release
			    git-version
			    (if org-install-dir
				(if (string= org-dir org-install-dir)
				    org-install-dir
				  (concat "mixed installation! "
					  org-install-dir
					  " and "
					  org-dir))
			      "org-loaddefs.el can not be found!")))
	   (version1 (if full version release)))
      (when here (insert version1))
      (when message (message "%s" version1))
      version1)))

(defconst org-version (org-version))


;;; Syntax Constants
;;;; Comments

;;;; Keyword
(defconst org-keyword-regexp "^[ \t]*#\\+\\(\\S-+?\\):[ \t]*\\(.*\\)$"
  "Regular expression for keyword-lines.")

;;;; Drawer

(defconst org-drawer-regexp
  ;; FIXME: Duplicate of `org-element-drawer-re'.
  (rx line-start (0+ (any ?\s ?\t))
      ":" (group (1+ (any ?- ?_ word))) ":"
      (0+ (any ?\s ?\t)) line-end)
  "Matches first or last line of a hidden block.
Group 1 contains drawer's name or \"END\".")

(defconst org-clock-drawer-start-re "^[ \t]*:CLOCK:[ \t]*$"
  "Regular expression matching the first line of a clock drawer.")

(defconst org-clock-drawer-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the last line of a clock drawer.")

(defconst org-logbook-drawer-re
  (rx (seq bol (0+ (any "\t ")) ":LOGBOOK:" (0+ (any "\t ")) "\n"
	   (*? (0+ nonl) "\n")
	   (0+ (any "\t ")) ":END:" (0+ (any "\t ")) eol))
  "Matches an entire LOGBOOK drawer.")

(defconst org-clock-drawer-re
  (concat "\\(" org-clock-drawer-start-re "\\)[^z-a]*?\\("
	  org-clock-drawer-end-re "\\)\n?")
  "Matches an entire clock drawer.")

;;;; Headline

(defconst org-archive-tag "ARCHIVE"
  "The tag that marks a subtree as archived.
An archived subtree does not open during visibility cycling, and does
not contribute to the agenda listings.")


;;; The custom variables

(defgroup org nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'outlines
  :group 'calendar)

(defcustom org-load-hook nil
  "Hook that is run after org.el has been loaded."
  :group 'org
  :type 'hook)

(make-obsolete-variable
 'org-load-hook
 "use `with-eval-after-load' instead." "9.5")

(defvar org-modules)  ; defined below
(defvar org-modules-loaded nil
  "Have the modules been loaded already?")

;;;###autoload
(defun org-load-modules-maybe (&optional force)
  "Load all extensions listed in `org-modules'."
  (when (or force (not org-modules-loaded))
    (dolist (ext org-modules)
      (condition-case-unless-debug nil (require ext)
	(error (message "Problems while trying to load feature `%s'" ext))))
    (setq org-modules-loaded t)))

(defun org-set-modules (var value)
  "Set VAR to VALUE and call `org-load-modules-maybe' with the force flag."
  (set-default-toplevel-value var value)
  (when (featurep 'org)
    (org-load-modules-maybe 'force)
    (org-element-cache-reset 'all)))

(defcustom org-modules '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
  "Modules that should always be loaded together with org.el.

If a description starts with <C>, the file is not part of Emacs and Org mode,
so loading it will require that you have properly installed org-contrib
package from NonGNU Emacs Lisp Package Archive
https://elpa.nongnu.org/nongnu/org-contrib.html

You can also use this system to load external packages (i.e. neither Org
core modules, nor org-contrib modules).  Just add symbols
to the end of the list.  If the package is called org-xyz.el, then you need
to add the symbol `xyz', and the package must have a call to:

   (provide \\='org-xyz)

For export specific modules, see also `org-export-backends'."
  :group 'org
  :set 'org-set-modules
  :package-version '(Org . "9.5")
  :type
  '(set :greedy t
	(const :tag "   bbdb:              Links to BBDB entries" ol-bbdb)
	(const :tag "   bibtex:            Links to BibTeX entries" ol-bibtex)
	(const :tag "   crypt:             Encryption of subtrees" org-crypt)
	(const :tag "   ctags:             Access to Emacs tags with links" org-ctags)
	(const :tag "   docview:           Links to Docview buffers" ol-docview)
        (const :tag "   doi:               Links to DOI references" ol-doi)
	(const :tag "   eww:               Store link to URL of Eww" ol-eww)
	(const :tag "   gnus:              Links to GNUS folders/messages" ol-gnus)
	(const :tag "   habit:             Track your consistency with habits" org-habit)
	(const :tag "   id:                Global IDs for identifying entries" org-id)
	(const :tag "   info:              Links to Info nodes" ol-info)
	(const :tag "   inlinetask:        Tasks independent of outline hierarchy" org-inlinetask)
	(const :tag "   irc:               Links to IRC/ERC chat sessions" ol-irc)
	(const :tag "   mhe:               Links to MHE folders/messages" ol-mhe)
	(const :tag "   mouse:             Additional mouse support" org-mouse)
	(const :tag "   protocol:          Intercept calls from emacsclient" org-protocol)
	(const :tag "   rmail:             Links to RMAIL folders/messages" ol-rmail)
	(const :tag "   tempo:             Fast completion for structures" org-tempo)
	(const :tag "   w3m:               Special cut/paste from w3m to Org mode." ol-w3m)
	(const :tag "   eshell:            Links to working directories in Eshell" ol-eshell)

	(const :tag "C  annotate-file:     Annotate a file with Org syntax" org-annotate-file)
	(const :tag "C  bookmark:          Links to bookmarks" ol-bookmark)
	(const :tag "C  checklist:         Extra functions for checklists in repeated tasks" org-checklist)
	(const :tag "C  choose:            Use TODO keywords to mark decisions states" org-choose)
	(const :tag "C  collector:         Collect properties into tables" org-collector)
	(const :tag "C  depend:            TODO dependencies for Org mode\n\t\t\t(PARTIALLY OBSOLETE, see built-in dependency support))" org-depend)
	(const :tag "C  elisp-symbol:      Links to emacs-lisp symbols" ol-elisp-symbol)
	(const :tag "C  eval-light:        Evaluate inbuffer-code on demand" org-eval-light)
	(const :tag "C  eval:              Include command output as text" org-eval)
	(const :tag "C  expiry:            Expiry mechanism for Org entries" org-expiry)
	(const :tag "C  git-link:          Links to specific file version" ol-git-link)
	(const :tag "C  interactive-query: Interactive modification of tags query\n\t\t\t(PARTIALLY OBSOLETE, see secondary filtering)" org-interactive-query)
        (const :tag "C  invoice:           Help manage client invoices in Org mode" org-invoice)
	(const :tag "C  learn:             SuperMemo's incremental learning algorithm" org-learn)
	(const :tag "C  mac-iCal:          Imports events from iCal.app to the Emacs diary" org-mac-iCal)
	(const :tag "C  mac-link:          Grab links and url from various mac Applications" org-mac-link)
	(const :tag "C  mairix:            Hook mairix search into Org for different MUAs" org-mairix)
	(const :tag "C  man:               Links to man pages in Org mode" ol-man)
	(const :tag "C  mew:               Links to Mew folders/messages" ol-mew)
	(const :tag "C  notify:            Notifications for Org mode" org-notify)
	(const :tag "C  notmuch:           Provide Org links to notmuch searches or messages" ol-notmuch)
	(const :tag "C  panel:             Simple routines for us with bad memory" org-panel)
	(const :tag "C  registry:          A registry for Org links" org-registry)
	(const :tag "C  screen:            Visit screen sessions through links" org-screen)
	(const :tag "C  screenshot:        Take and manage screenshots in Org files" org-screenshot)
	(const :tag "C  secretary:         Team management with Org" org-secretary)
	(const :tag "C  sqlinsert:         Convert Org tables to SQL insertions" orgtbl-sqlinsert)
	(const :tag "C  toc:               Table of contents for Org buffer" org-toc)
	(const :tag "C  track:             Keep up with Org mode development" org-track)
	(const :tag "C  velocity           Something like Notational Velocity for Org" org-velocity)
	(const :tag "C  vm:                Links to VM folders/messages" ol-vm)
	(const :tag "C  wikinodes:         CamelCase wiki-like links" org-wikinodes)
	(const :tag "C  wl:                Links to Wanderlust folders/messages" ol-wl)
	(repeat :tag "External packages" :inline t (symbol :tag "Package"))))

(defvar org-export-registered-backends) ; From ox.el.
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))
(declare-function org-export-backend-name "ox" (backend) t)
(defcustom org-export-backends '(ascii html icalendar latex odt)
  "List of export backends that should be always available.

If a description starts with <C>, the file is not part of Emacs and Org mode,
so loading it will require that you have properly installed org-contrib
package from NonGNU Emacs Lisp Package Archive
https://elpa.nongnu.org/nongnu/org-contrib.html

Unlike to `org-modules', libraries in this list will not be
loaded along with Org, but only once the export framework is
needed.

This variable needs to be set before org.el is loaded.  If you
need to make a change while Emacs is running, use the customize
interface or run the following code, where VAL stands for the new
value of the variable, after updating it:

  (progn
    (setq org-export-registered-backends
          (cl-remove-if-not
           (lambda (backend)
             (let ((name (org-export-backend-name backend)))
               (or (memq name val)
                   (catch \\='parentp
                     (dolist (b val)
                       (and (org-export-derived-backend-p b name)
                            (throw \\='parentp t)))))))
           org-export-registered-backends))
    (let ((new-list (mapcar #\\='org-export-backend-name
                            org-export-registered-backends)))
      (dolist (backend val)
        (cond
         ((not (load (format \"ox-%s\" backend) t t))
          (message \"Problems while trying to load export backend \\=`%s\\='\"
                   backend))
         ((not (memq backend new-list)) (push backend new-list))))
      (set-default \\='org-export-backends new-list)))

Adding a backend to this list will also pull the backend it
depends on, if any."
  :group 'org
  :group 'org-export
  :version "26.1"
  :package-version '(Org . "9.0")
  :initialize 'custom-initialize-set
  :set (lambda (var val)
	 (if (not (featurep 'ox)) (set-default-toplevel-value var val)
	   ;; Any backend not required anymore (not present in VAL and not
	   ;; a parent of any backend in the new value) is removed from the
	   ;; list of registered backends.
	   (setq org-export-registered-backends
		 (cl-remove-if-not
		  (lambda (backend)
		    (let ((name (org-export-backend-name backend)))
		      (or (memq name val)
			  (catch 'parentp
			    (dolist (b val)
			      (and (org-export-derived-backend-p b name)
				   (throw 'parentp t)))))))
		  org-export-registered-backends))
	   ;; Now build NEW-LIST of both new backends and required
	   ;; parents.
	   (let ((new-list (mapcar #'org-export-backend-name
				   org-export-registered-backends)))
	     (dolist (backend val)
	       (cond
		((not (load (format "ox-%s" backend) t t))
		 (message "Problems while trying to load export backend `%s'"
			  backend))
		((not (memq backend new-list)) (push backend new-list))))
	     ;; Set VAR to that list with fixed dependencies.
	     (set-default-toplevel-value var new-list))))
  :type '(set :greedy t
	      (const :tag "   ascii       Export buffer to ASCII format" ascii)
	      (const :tag "   beamer      Export buffer to Beamer presentation" beamer)
	      (const :tag "   html        Export buffer to HTML format" html)
	      (const :tag "   icalendar   Export buffer to iCalendar format" icalendar)
	      (const :tag "   latex       Export buffer to LaTeX format" latex)
	      (const :tag "   man         Export buffer to MAN format" man)
	      (const :tag "   md          Export buffer to Markdown format" md)
	      (const :tag "   odt         Export buffer to ODT format" odt)
	      (const :tag "   org         Export buffer to Org format" org)
	      (const :tag "   texinfo     Export buffer to Texinfo format" texinfo)
	      (const :tag "C  confluence  Export buffer to Confluence Wiki format" confluence)
	      (const :tag "C  deck        Export buffer to deck.js presentations" deck)
	      (const :tag "C  freemind    Export buffer to Freemind mindmap format" freemind)
	      (const :tag "C  groff       Export buffer to Groff format" groff)
	      (const :tag "C  koma-letter Export buffer to KOMA Scrlttrl2 format" koma-letter)
	      (const :tag "C  RSS 2.0     Export buffer to RSS 2.0 format" rss)
	      (const :tag "C  s5          Export buffer to s5 presentations" s5)
	      (const :tag "C  taskjuggler Export buffer to TaskJuggler format" taskjuggler)))

(defun org-load-export-backends (&optional force)
  "Load all the export backends from `org-export-backends'.
When FORCE, load backends even when ox is loaded."
  (unless (and (featurep 'ox) (not force))
    (require 'ox)
    (dolist (backend org-export-backends)
      (condition-case-unless-debug nil (require (intern (format "ox-%s" backend)))
        (error (message "Problems while trying to load export backend `%s'"
		        backend))))))

(eval-after-load 'ox '(org-load-export-backends 'force))

(defcustom org-support-shift-select nil
  "Non-nil means make shift-cursor commands select text when possible.
\\<org-mode-map>
In Emacs 23, when `shift-select-mode' is on, shifted cursor keys
start selecting a region, or enlarge regions started in this way.
In Org mode, in special contexts, these same keys are used for
other purposes, important enough to compete with shift selection.
Org tries to balance these needs by supporting `shift-select-mode'
outside these special contexts, under control of this variable.

The default of this variable is nil, to avoid confusing behavior.  Shifted
cursor keys will then execute Org commands in the following contexts:
- on a headline, changing TODO state (left/right) and priority (up/down)
- on a time stamp, changing the time
- in a plain list item, changing the bullet type
- in a property definition line, switching between allowed values
- in the BEGIN line of a clock table (changing the time block).
- in a table, moving the cell in the specified direction.
Outside these contexts, the commands will throw an error.

When this variable is t and the cursor is not in a special
context, Org mode will support shift-selection for making and
enlarging regions.  To make this more effective, the bullet
cycling will no longer happen anywhere in an item line, but only
if the cursor is exactly on the bullet.

If you set this variable to the symbol `except-timestamps', then the
keys will not be special in headlines, property lines, item lines, and
table cells, to make shift selection work there as well.  If this is
what you want, you can use the following alternative commands:
`\\[org-todo]' and `\\[org-priority]' \
to change TODO state and priority,
`\\[universal-argument] \\[universal-argument] \\[org-todo]' \
can be used to switch TODO sets,
`\\[org-ctrl-c-minus]' to cycle item bullet types,
and properties can be edited by hand or in column view.

However, when the cursor is on a timestamp, shift-cursor commands will
still edit the timestamp - this is just too good to give up.  Set the
value to symbol `everywhere' to disable shift-cursor commands on
timestamps as well."
  :group 'org
  :package-version '("Org" . "10.0")
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When outside special context" t)
          ;; This used to be symbol `always', but it is confusing
          ;; when we also have `everywhere', so renamed, keeping
          ;; old symbol support in the code for backwards compatibility.
	  (const :tag "Everywhere except timestamps" except-timestamps)
          (const :tag "Everywhere" everywhere)))

(defcustom org-loop-over-headlines-in-active-region t
  "Shall some commands act upon headlines in the active region?

When set to t, some commands will be performed in all headlines
within the active region.

When set to `start-level', some commands will be performed in all
headlines within the active region, provided that these headlines
are of the same level than the first one.

When set to a string, those commands will be performed on the
matching headlines within the active region.  Such string must be
a tags/property/todo match as it is used in the agenda tags view.

The list of commands is: `org-schedule', `org-deadline',
`org-todo', `org-set-tags-command', `org-archive-subtree',
`org-archive-set-tag', `org-toggle-archive-tag' and
`org-archive-to-archive-sibling'.  The archiving commands skip
already archived entries.

See `org-agenda-loop-over-headlines-in-active-region' for the
equivalent option for agenda views."
  :type '(choice (const :tag "Don't loop" nil)
		 (const :tag "All headlines in active region" t)
		 (const :tag "In active region, headlines at the same level than the first one" start-level)
		 (string :tag "Tags/Property/Todo matcher"))
  :package-version '(Org . "9.4")
  :group 'org-todo
  :group 'org-archive)

(defcustom org-edit-keep-region
  '((org-metaleft . t)
    (org-metaright . t)
    (org-metaup . t)
    (org-metadown . t))
  "Shall some Org editing commands keep region active?

This variable can be nil, t, or an a list of entries like
  (COMMAND-NAME . KEEP-REGION-P)"
  :type '(choice
          (const :tag "Keep region for all commands" t)
          (const :tag "Never keep region" nil)
          (alist
           :key-type
           (choice (const org-metaleft)
                   (const org-metaright)
                   (const org-metaup)
                   (const org-metadown))
           :value-type
           (choice (const :tag "Keep region" t)
                   (const :tag "Deactivate region" nil))))
  :package-version '(Org . "9.8")
  :group 'org-edit-structure
  :safe #'always)

(defun org--deactivate-mark ()
  "Return non-nil when `this-command' should deactivate mark upon completion.
Honor `org-edit-keep-region'.  Return nil by default, when
`this-command' has no setting in `org-edit-keep-region'."
  (pcase org-edit-keep-region
    (`t nil)
    (`nil t)
    (_ (not (alist-get this-command org-edit-keep-region nil)))))

(unless (boundp 'untrusted-content)
  (defvar untrusted-content nil))
(defvar untrusted-content) ; defined in files.el since Emacs 29.3
(defvar org--latex-preview-when-risky nil
  "If non-nil, enable LaTeX preview in Org buffers from unsafe source.

Some specially designed LaTeX code may generate huge pdf or log files
that may exhaust disk space.

This variable controls how to handle LaTeX preview when rendering LaTeX
fragments that originate from incoming email messages.  It has no effect
when Org mode is unable to determine the origin of the Org buffer.

An Org buffer is considered to be from unsafe source when the
variable `untrusted-content' has a non-nil value in the buffer.

If this variable is non-nil, LaTeX previews are rendered unconditionally.

This variable may be renamed or changed in the future.")

(defgroup org-keywords nil
  "Keywords in Org mode."
  :tag "Org Keywords"
  :group 'org)

(defcustom org-closed-keep-when-no-todo nil
  "Remove CLOSED: timestamp when switching back to a non-todo state?"
  :group 'org-todo
  :group 'org-keywords
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defgroup org-structure nil
  "Options concerning the general structure of Org files."
  :tag "Org Structure"
  :group 'org)

(defcustom org-indirect-buffer-display 'other-window
  "How should indirect tree buffers be displayed?

This applies to indirect buffers created with the commands
`org-tree-to-indirect-buffer' and `org-agenda-tree-to-indirect-buffer'.

Valid values are:
current-window   Display in the current window
other-window     Just display in another window.
dedicated-frame  Create one new frame, and reuse it each time.
new-frame        Make a new frame each time.  Note that in this case
                 previously-made indirect buffers are kept, and you need to
                 kill these buffers yourself."
  :group 'org-structure
  :group 'org-agenda-windows
  :type '(choice
	  (const :tag "In current window" current-window)
	  (const :tag "In current frame, other window" other-window)
	  (const :tag "Each time a new frame" new-frame)
	  (const :tag "One dedicated frame" dedicated-frame)))

(defcustom org-bookmark-names-plist
  '(:last-capture "org-capture-last-stored"
		  :last-refile "org-refile-last-stored"
		  :last-capture-marker "org-capture-last-stored-marker")
  "Names for bookmarks automatically set by some Org commands.
This can provide strings as names for a number of bookmarks Org sets
automatically.  The following keys are currently implemented:
  :last-capture
  :last-capture-marker
  :last-refile
When a key does not show up in the property list, the corresponding bookmark
is not set."
  :group 'org-structure
  :type 'plist)

(defgroup org-edit-structure nil
  "Options concerning structure editing in Org mode."
  :tag "Org Edit Structure"
  :group 'org-structure)

(defvar org-odd-levels-only)

(defcustom org-special-ctrl-k nil
  "Non-nil means that \\<org-mode-map>\\[org-kill-line] \
will behave specially in headlines.

When nil, \\[org-kill-line] will call the default `kill-line' command.
Otherwise, the following will happen when point is in a headline:

- At the beginning of a headline, kill the entire line.
- In the middle of the headline text, kill the text up to the tags.
- After the headline text and before the tags, kill all the tags."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-ctrl-k-protect-subtree nil
  "Non-nil means, do not delete a hidden subtree with `C-k'.
When set to the symbol `error', simply throw an error when `C-k' is
used to kill (part-of) a headline that has hidden text behind it.
Any other non-nil value will result in a query to the user, if it is
OK to kill that hidden subtree.  When nil, kill without remorse."
  :group 'org-edit-structure
  :version "24.1"
  :type '(choice
	  (const :tag "Do not protect hidden subtrees" nil)
	  (const :tag "Protect hidden subtrees with a security query" t)
	  (const :tag "Never kill a hidden subtree with C-k" error)))

(defcustom org-special-ctrl-o t
  "Non-nil means, make `open-line' (\\[open-line]) insert a row in tables."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-yank-folded-subtrees t
  "Non-nil means when yanking subtrees, fold them.
If the kill is a single subtree, or a sequence of subtrees, i.e. if
it starts with a heading and all other headings in it are either children
or siblings, then fold all the subtrees.  However, do this only if no
text after the yank would be swallowed into a folded tree by this action."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-yank-adjusted-subtrees nil
  "Non-nil means when yanking subtrees, adjust the level.
With this setting, `org-paste-subtree' is used to insert the subtree, see
this function for details."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-M-RET-may-split-line '((default . t))
  "Non-nil means M-RET will split the line at the cursor position.
When nil, it will go to the end of the line before making a
new line.
You may also set this option in a different way for different
contexts.  Valid contexts are:

headline  when creating a new headline
item      when creating a new item
table     in a table field
default   the value to be used for all contexts not explicitly
          customized"
  :group 'org-structure
  :group 'org-table
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
			   (const headline)
			   (const item)
			   (const table)
			   (const default))
		   (boolean)))))

(defcustom org-insert-heading-respect-content nil
  "Non-nil means insert new headings after the current subtree.
\\<org-mode-map>
When nil, the new heading is created directly after the current line.
The commands `\\[org-insert-heading-respect-content]' and \
`\\[org-insert-todo-heading-respect-content]' turn this variable on
for the duration of the command."
  :group 'org-structure
  :type 'boolean)

(defcustom org-blank-before-new-entry '((heading . auto)
					(plain-list-item . auto))
  "Should `org-insert-heading' leave a blank line before new heading/item?
The value is an alist, with `heading' and `plain-list-item' as CAR,
and a boolean flag as CDR.  The cdr may also be the symbol `auto', in
which case Org will look at the surrounding headings/items and try to
make an intelligent decision whether to insert a blank line or not."
  :group 'org-edit-structure
  :type '(list
	  (cons (const heading)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))
	  (cons (const plain-list-item)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))))

(defcustom org-insert-heading-hook nil
  "Hook being run after inserting a new heading."
  :group 'org-edit-structure
  :type 'hook)

(defcustom org-self-insert-cluster-for-undo nil
  "Non-nil means cluster self-insert commands for undo when possible.
If this is set, then, like in the Emacs command loop, 20 consecutive
characters will be undone together.
This is configurable, because there is some impact on typing performance."
  :group 'org-table
  :type 'boolean)

(defgroup org-todo nil
  "Options concerning TODO items in Org mode."
  :tag "Org TODO"
  :group 'org)

(defgroup org-progress nil
  "Options concerning Progress logging in Org mode."
  :tag "Org Progress"
  :group 'org-time)

(defvar org-todo-interpretation-widgets
  '((:tag "Sequence (cycling hits every state)" sequence)
    (:tag "Type     (cycling directly to DONE)" type))
  "The available interpretation symbols for customizing `org-todo-keywords'.
Interested libraries should add to this list.")

(defcustom org-todo-keywords '((sequence "TODO" "DONE"))
  "List of TODO entry keyword sequences and their interpretation.
\\<org-mode-map>This is a list of sequences.

Each sequence starts with a symbol, either `sequence' or `type',
indicating if the keywords should be interpreted as a sequence of
action steps, or as different types of TODO items.  The first
keywords are states requiring action - these states will select a headline
for inclusion into the global TODO list Org produces.  If one of the
\"keywords\" is the vertical bar, \"|\", the remaining keywords
signify that no further action is necessary.  If \"|\" is not found,
the last keyword is treated as the only DONE state of the sequence.

The command `\\[org-todo]' cycles an entry through these states, and one
additional state where no keyword is present.  For details about this
cycling, see the manual.

TODO keywords and interpretation can also be set on a per-file basis with
the special #+SEQ_TODO and #+TYP_TODO lines.

Each keyword can optionally specify a character for fast state selection
\(in combination with the variable `org-use-fast-todo-selection')
and specifiers for state change logging, using the same syntax that
is used in the \"#+TODO:\" lines.  For example, \"WAIT(w)\" says that
the WAIT state can be selected with the \"w\" key.  \"WAIT(w!)\"
indicates to record a time stamp each time this state is selected.

Each keyword may also specify if a timestamp or a note should be
recorded when entering or leaving the state, by adding additional
characters in the parenthesis after the keyword.  This looks like this:
\"WAIT(w@/!)\".  \"@\" means to add a note (with time), \"!\" means to
record only the time of the state change.  With X and Y being either
\"@\" or \"!\", \"X/Y\" means use X when entering the state, and use
Y when leaving the state if and only if the *target* state does not
define X.  You may omit any of the fast-selection key or X or /Y,
so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.

For backward compatibility, this variable may also be just a list
of keywords.  In this case the interpretation (sequence or type) will be
taken from the (otherwise obsolete) variable `org-todo-interpretation'."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice
	  (repeat :tag "Old syntax, just keywords"
		  (string :tag "Keyword"))
	  (repeat :tag "New syntax"
		  (cons
		   (choice
		    :tag "Interpretation"
		    ;;Quick and dirty way to see
                    ;;`org-todo-interpretation'.  This takes the
		    ;;place of item arguments
		    :convert-widget
		    (lambda (widget)
		      (widget-put widget
				  :args (mapcar
					 (lambda (x)
					   (widget-convert
					    (cons 'const x)))
					 org-todo-interpretation-widgets))
		      widget))
		   (repeat
		    (string :tag "Keyword"))))))

(defvar-local org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")
(defvar org-todo-keywords-for-agenda nil)
(defvar org-done-keywords-for-agenda nil)
(defvar org-todo-keyword-alist-for-agenda nil)
(defvar org-tag-alist-for-agenda nil
  "Alist of all tags from all agenda files.")
(defvar org-tag-groups-alist-for-agenda nil
  "Alist of all groups tags from all current agenda files.")
(defvar org-agenda-contributing-files nil)

(defvar-local org-done-keywords nil)
(defvar-local org-todo-heads nil)
(defvar-local org-todo-sets nil)
(defvar-local org-todo-log-states nil)
(defvar-local org-todo-kwd-alist nil)
(defvar-local org-todo-key-alist nil)
(defvar-local org-todo-key-trigger nil)

(defcustom org-use-fast-todo-selection 'auto
  "\\<org-mode-map>\
Non-nil means use the fast todo selection scheme with `\\[org-todo]'.
This variable describes if and under what circumstances the cycling
mechanism for TODO keywords will be replaced by a single-key, direct
selection scheme, where the choices are displayed in a little window.

When nil, fast selection is never used.  This means that the command
will always switch to the next state.

When it is the symbol `auto', fast selection is whenever selection
keys have been defined.

`expert' is like `auto', but no special window with the keyword
will be shown, choices will only be listed in the prompt.

In all cases, the special interface is only used if access keys have
actually been assigned by the user, i.e. if keywords in the configuration
are followed by a letter in parenthesis, like TODO(t)."
  :group 'org-todo
  :set (lambda (var val)
	 (cond
	  ((eq var t) (set-default-toplevel-value var 'auto))
	  ((eq var 'prefix) (set-default-toplevel-value var nil))
	  (t (set-default-toplevel-value var val))))
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Automatically, when key letter have been defined" auto)
	  (const :tag "Automatically, but don't show the selection window" expert)))

(defcustom org-provide-todo-statistics t
  "Non-nil means update todo statistics after insert and toggle.
`all-headlines' means update todo statistics by including headlines
with no TODO keyword as well, counting them as not done.
A list of TODO keywords means the same, but skip keywords that are
not in this list.
When set to a list of two lists, the first list contains keywords
to consider as TODO keywords, the second list contains keywords
to consider as DONE keywords.

When this is set, todo statistics is updated in the parent of the
current entry each time a todo state is changed."
  :group 'org-todo
  :type '(choice
	  (const :tag "Yes, only for TODO entries" t)
	  (const :tag "Yes, including all entries" all-headlines)
	  (repeat :tag "Yes, for TODOs in this list"
		  (string :tag "TODO keyword"))
	  (list :tag "Yes, for TODOs and DONEs in these lists"
		(repeat (string :tag "TODO keyword"))
		(repeat (string :tag "DONE keyword")))
	  (other :tag "No TODO statistics" nil)))

(defcustom org-todo-children-only-statistics t
  "Non-nil means TODO statistics covers just direct children.
When nil, all entries in the subtree are considered.
This has only an effect if `org-provide-todo-statistics' is set.
To set this to nil for only a single subtree, use a COOKIE_DATA
property and include the word \"recursive\" into the value."
  :group 'org-todo
  :package-version '(Org . "10.0")
  :type 'boolean)

(defcustom org-after-todo-state-change-hook nil
  "Hook which is run after the state of a TODO item was changed.
The new state (a string with a TODO keyword, or nil) is available in the
Lisp variable `org-state'."
  :group 'org-todo
  :type 'hook)

(defvar org-blocker-hook nil
  "Hook for functions that are allowed to block a state change.

Functions in this hook should not modify the buffer.
Each function gets as its single argument a property list,
see `org-trigger-hook' for more information about this list.

If any of the functions in this hook returns nil, the state change
is blocked.")

(defvar org-trigger-hook nil
  "Hook for functions that are triggered by a state change.

Each function gets as its single argument a property list with at
least the following elements:

 (:type type-of-change :position pos-at-entry-start
  :from old-state :to new-state)

Depending on the type, more properties may be present.

This mechanism is currently implemented for:

TODO state changes
------------------
:type  todo-state-change
:from  previous state (keyword as a string), or nil, or a symbol
       `todo' or `done', to indicate the general type of state.
:to    new state, like in :from")

(defcustom org-enforce-todo-dependencies nil
  "Non-nil means undone TODO entries will block switching the parent to DONE.
Also, if a parent has an :ORDERED: property, switching an entry to DONE will
be blocked if any prior sibling is not yet done.
Finally, if the parent is blocked because of ordered siblings of its own,
the child will also be blocked."
  :set (lambda (var val)
	 (set-default-toplevel-value var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-children-or-siblings-or-parent)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-children-or-siblings-or-parent)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-enforce-todo-checkbox-dependencies nil
  "Non-nil means unchecked boxes will block switching the parent to DONE.
When this is nil, checkboxes have no influence on switching TODO states.
When non-nil, you first need to check off all check boxes before the TODO
entry can be switched to DONE.
This variable needs to be set before org.el is loaded, and you need to
restart Emacs after a change to make the change effective.  The only way
to change it while Emacs is running is through the customize interface."
  :set (lambda (var val)
	 (set-default-toplevel-value var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-checkboxes)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-checkboxes)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-treat-insert-todo-heading-as-state-change nil
  "Non-nil means inserting a TODO heading is treated as state change.
So when the command `\\[org-insert-todo-heading]' is used, state change
logging will apply if appropriate.  When nil, the new TODO item will
be inserted directly, and no logging will take place."
  :group 'org-todo
  :type 'boolean)

(defcustom org-treat-S-cursor-todo-selection-as-state-change t
  "Non-nil means switching TODO states with S-cursor counts as state change.
This is the default behavior.  However, setting this to nil allows a
convenient way to select a TODO state and bypass any logging associated
with that."
  :group 'org-todo
  :type 'boolean)

(defcustom org-todo-state-tags-triggers nil
  "Tag changes that should be triggered by TODO state changes.
This is a list.  Each entry is

  (state-change (tag . flag) .......)

State-change can be a string with a state, and empty string to indicate the
state that has no TODO keyword, or it can be one of the symbols `todo'
or `done', meaning any not-done or done state, respectively."
  :group 'org-todo
  :group 'org-tags
  :type '(repeat
	  (cons (choice :tag "When changing to"
			(const :tag "Not-done state" todo)
			(const :tag "Done state" done)
			(string :tag "State"))
		(repeat
		 (cons :tag "Tag action"
		       (string :tag "Tag")
		       (choice (const :tag "Add" t) (const :tag "Remove" nil)))))))

(defcustom org-log-done nil
  "Information to record when a task moves to the DONE state.

Possible values are:

nil     Don't add anything, just change the keyword
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologdone
   #+STARTUP: logdone
   #+STARTUP: lognotedone

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record CLOSED timestamp" time)
	  (const :tag "Record CLOSED timestamp with note." note)))

;; Normalize old uses of org-log-done.
(cond
 ((eq org-log-done t) (setq org-log-done 'time))
 ((and (listp org-log-done) (memq 'done org-log-done))
  (setq org-log-done 'note)))

(defcustom org-log-reschedule nil
  "Information to record when the scheduling date of a task is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologreschedule
   #+STARTUP: logreschedule
   #+STARTUP: lognotereschedule

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords.

This variable has an effect when calling `org-schedule' or
`org-agenda-schedule' only."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note" note)))

(defcustom org-log-redeadline nil
  "Information to record when the deadline date of a task is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologredeadline
   #+STARTUP: logredeadline
   #+STARTUP: lognoteredeadline

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords.

This variable has an effect when calling `org-deadline' or
`org-agenda-deadline' only."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-log-note-clock-out nil
  "Non-nil means record a note when clocking out of an item.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: lognoteclock-out
   #+STARTUP: nolognoteclock-out"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-done-with-time t
  "Non-nil means the CLOSED time stamp will contain date and time.
When nil, only the date will be recorded."
  :group 'org-progress
  :type 'boolean)

(defcustom org-todo-repeat-to-state nil
  "The TODO state to which a repeater should return the repeating task.
By default this is the first task of a TODO sequence or the
previous state of a TYPE_TODO set.  But you can specify to use
the previous state in a TODO sequence or a string.

Alternatively, you can set the :REPEAT_TO_STATE: property of the
entry, which has precedence over this option."
  :group 'org-todo
  :version "24.1"
  :type '(choice (const :tag "Use the previous TODO state" t)
		 (const :tag "Use the head of the TODO sequence" nil)
		 (string :tag "Use a specific TODO state")))

(defcustom org-log-repeat 'time
  "Non-nil means record moving through the DONE state when triggering repeat.
An auto-repeating task is immediately switched back to TODO when
marked DONE.  If you are not logging state changes (by adding \"@\"
or \"!\" to the TODO keyword definition), or set `org-log-done' to
record a closing note, there will be no record of the task moving
through DONE.  This variable forces taking a note anyway.

nil     Don't force a record
time    Record a time stamp
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologrepeat
   #+STARTUP: logrepeat
   #+STARTUP: lognoterepeat

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Don't force a record" nil)
	  (const :tag "Force recording the DONE state" time)
	  (const :tag "Force recording a note with the DONE state" note)))

(defcustom org-todo-repeat-hook nil
  "Hook that is run after a task has been repeated."
  :package-version '(Org . "9.2")
  :group 'org-todo
  :type 'hook)

(defgroup org-time nil
  "Options concerning time stamps and deadlines in Org mode."
  :tag "Org Time"
  :group 'org)

(defcustom org-deadline-warning-days 14
  "Number of days before expiration during which a deadline becomes active.
This variable governs the display in sparse trees and in the agenda.
When 0 or negative, it means use this number (the absolute value of it)
even if a deadline has a different individual lead time specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :type 'integer)

(defcustom org-scheduled-delay-days 0
  "Number of days before a scheduled item becomes active.
This variable governs the display in sparse trees and in the agenda.
The default value (i.e. 0) means: don't delay scheduled item.
When negative, it means use this number (the absolute value of it)
even if a scheduled item has a different individual delay time
specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-agenda-jump-prefer-future 'org-read-date-prefer-future
  "Should the agenda jump command prefer the future for incomplete dates?
The default is to do the same as configured in `org-read-date-prefer-future'.
But you can also set a deviating value here.
This may t or nil, or the symbol `org-read-date-prefer-future'."
  :group 'org-agenda
  :group 'org-time
  :version "24.1"
  :type '(choice
	  (const :tag "Use org-read-date-prefer-future"
		 org-read-date-prefer-future)
	  (const :tag "Never" nil)
	  (const :tag "Always" t)))

(defcustom org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS"
  "The default column format, if no other format has been defined.
This variable can be set on the per-file basis by inserting a line

#+COLUMNS: %25ITEM ....."
  :group 'org-properties
  :type 'string)

(defcustom org-columns-default-format-for-agenda nil
  "The default column format in an agenda buffer.
This will be used for column view in the agenda unless a format has
been set by adding `org-overriding-columns-format' to the local
settings list of a custom agenda view.  When nil, the columns format
for the first item in the agenda list will be used, or as a fall-back,
`org-columns-default-format'."
  :group 'org-properties
  :type '(choice
	  (const :tag "No default" nil)
	  (string :tag "Format string")))

(defcustom org-columns-ellipses ".."
  "The ellipses to be used when a field in column view is truncated.
When this is the empty string, as many characters as possible are shown,
but then there will be no visual indication that the field has been truncated.
When this is a string of length N, the last N characters of a truncated
field are replaced by this string.  If the column is narrower than the
ellipses string, only part of the ellipses string will be shown."
  :group 'org-properties
  :type 'string)

(defgroup org-agenda nil
  "Options concerning agenda views in Org mode."
  :tag "Org Agenda"
  :group 'org)

(defvar-local org-category nil
  "Variable used by Org files to set a category for agenda display.
There are multiple ways to set the category.  One way is to set
it in the document property drawer.  For example:

:PROPERTIES:
:CATEGORY: Elisp
:END:

Other ways to define it is as an Emacs file variable, for example

#   -*- mode: org; org-category: \"Elisp\"

or for the file to contain a special line:

#+CATEGORY: Elisp

If the file does not specify a category, then file's base name
is used instead.")
(put 'org-category 'safe-local-variable (lambda (x) (or (symbolp x) (stringp x))))

(defvaralias 'org-agenda-multi-occur-extra-files
  'org-agenda-text-search-extra-files)

(defcustom org-agenda-text-search-extra-files nil
  "List of extra files to be searched by text search commands.
These files will be searched in addition to the agenda files by the
commands `org-search-view' (`\\[org-agenda] s') \
and `org-occur-in-agenda-files'.
Note that these files will only be searched for text search commands,
not for the other agenda views like todo lists, tag searches or the weekly
agenda.  This variable is intended to list notes and possibly archive files
that should also be searched by these two commands.
In fact, if the first element in the list is the symbol `agenda-archives',
then all archive files of all agenda files will be added to the search
scope."
  :group 'org-agenda
  :type '(set :greedy t
	      (const :tag "Agenda Archives" agenda-archives)
	      (repeat :inline t (file))))

(defun org-set-packages-alist (var val)
  "Set the packages alist and make sure it has 3 elements per entry."
  (set-default-toplevel-value var (mapcar (lambda (x)
		                            (if (and (consp x) (= (length x) 2))
			                        (list (car x) (nth 1 x) t)
		                              x))
		                          val)))

(defun org-get-packages-alist (var)
  "Get the packages alist and make sure it has 3 elements per entry."
  (mapcar (lambda (x)
	    (if (and (consp x) (= (length x) 2))
		(list (car x) (nth 1 x) t)
	      x))
	  (default-value var)))

(defcustom org-latex-default-packages-alist
  '(;; amsmath before fontspec for lualatex and xetex
    (""     "amsmath"   t ("lualatex" "xelatex"))
    ;; fontspec ASAP for lualatex and xetex
    (""     "fontspec"  t ("lualatex" "xelatex"))
    ;; inputenc and fontenc are for pdflatex only
    ("AUTO" "inputenc"  t ("pdflatex"))
    ("T1"   "fontenc"   t ("pdflatex"))
    (""     "graphicx"  t)
    (""     "longtable" nil)
    (""     "wrapfig"   nil)
    (""     "rotating"  nil)
    ("normalem" "ulem"  t)
    ;; amsmath and amssymb after inputenc/fontenc for pdflatex
    (""     "amsmath"   t ("pdflatex"))
    (""     "amssymb"   t ("pdflatex"))
    (""     "capt-of"   nil)
    (""     "hyperref"  nil))
  "Alist of default packages to be inserted in the header.

Change this only if one of the packages here causes an
incompatibility with another package you are using.

The packages in this list are needed by one part or another of
Org mode to function properly:

- fontspec: for font and character selection in lualatex and xetex
- inputenc, fontenc:  for basic font and character selection
  in pdflatex
- graphicx: for including images
- longtable: For multipage tables
- wrapfig: for figure placement
- rotating: for sideways figures and tables
- ulem: for underline and strike-through
- amsmath: for subscript and superscript and math environments
- amssymb: for various symbols used for interpreting the entities
  in `org-entities'.  You can skip some of this package if you don't
  use any of the symbols.
- capt-of: for captions outside of floats
- hyperref: for cross references

Therefore you should not modify this variable unless you know
what you are doing.  The one reason to change it anyway is that
you might be loading some other package that conflicts with one
of the default packages.  Each element is either a cell or
a string.

A cell is of the format

  (\"options\" \"package\" SNIPPET-FLAG COMPILERS)

If SNIPPET-FLAG is non-nil, the package also needs to be included
when compiling LaTeX snippets into images for inclusion into
non-LaTeX output.

COMPILERS is a list of compilers that should include the package,
see `org-latex-compiler'.  If the document compiler is not in the
list, and the list is non-nil, the package will not be inserted
in the final document.

A string will be inserted as-is in the header of the document."
  :group 'org-latex
  :group 'org-export-latex
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :safe #'listp
  :package-version '(Org . "9.7")
  :type '(repeat
	  (choice
	   (list :tag "options/package pair"
		 (string :tag "options")
		 (string :tag "package")
		 (boolean :tag "Snippet")
		 (choice
		  (const :tag "For all compilers" nil)
		  (repeat :tag "Allowed compiler" string)))
	   (string :tag "A line of LaTeX"))))

(defcustom org-latex-packages-alist nil
  "Alist of packages to be inserted in every LaTeX header.

These will be inserted after `org-latex-default-packages-alist'.
Each element is either a cell or a string.

A cell is of the format:

    (\"options\" \"package\" SNIPPET-FLAG COMPILERS)

SNIPPET-FLAG, when non-nil, indicates that this package is also
needed when turning LaTeX snippets into images for inclusion into
non-LaTeX output.

COMPILERS is a list of compilers that should include the package,
see `org-latex-compiler'.  If the document compiler is not in the
list, and the list is non-nil, the package will not be inserted
in the final document.

A string will be inserted as-is in the header of the document.

Make sure that you only list packages here which:

  - you want in every file;
  - do not conflict with the setup in `org-latex-preview-preamble';
  - do not conflict with the default packages in
    `org-latex-default-packages-alist'."
  :group 'org-latex
  :group 'org-export-latex
  :safe #'listp
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :type
  '(repeat
    (choice
     (list :tag "options/package pair"
           (string :tag "options")
           (string :tag "package")
           (boolean :tag "snippet")
           (choice
            (const :tag "All compilers include this package" nil)
            (repeat :tag "Only include from these compilers" string)))
     (string :tag "A line of LaTeX"))))

;;; Functions and variables from their packages
;;  Declared here to avoid compiler warnings
(defvar mark-active)

;; Various packages
(declare-function calc-eval "calc" (str &optional separator &rest args))
(declare-function calendar-forward-day "cal-move" (arg))
(declare-function calendar-goto-date "cal-move" (date))
(declare-function calendar-goto-today "cal-move" ())
(declare-function calendar-iso-from-absolute "cal-iso" (date))
(declare-function calendar-iso-to-absolute "cal-iso" (date))
(declare-function org-agenda-change-all-lines
		  "org-agenda"
		  (newhead hdmarker &optional fixface just-this))
(declare-function org-agenda-copy-local-variable "org-agenda" (var))
(declare-function org-agenda-format-item
		  "org-agenda"
		  (extra txt &optional level category tags dotime
			 remove-re habitp))
(declare-function org-agenda-new-marker "org-agenda" (&optional pos))
(declare-function org-agenda-save-markers-for-cut-and-paste
		  "org-agenda"
		  (beg end))
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-skip "org-agenda" (&optional element))
(declare-function org-attach-reveal "org-attach" ())
(declare-function org-attach-reveal-in-emacs "org-attach" ())
(declare-function org-indent-mode "org-indent" (&optional arg))
(declare-function org-inlinetask-goto-end "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function parse-time-string "parse-time" (string &optional form))

(defvar align-mode-rules-list)
(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(defvar calc-embedded-open-mode)
(defvar org-agenda-tags-todo-honor-ignore-options)
(defvar remember-data-file)

(declare-function org-clock-save-markers-for-cut-and-paste "org-clock" (beg end))
(declare-function org-clock-update-mode-line "org-clock" (&optional refresh))

(defvar org-clock-start-time)
(defvar org-clock-marker (make-marker)
  "Marker recording the last clock-in.")
(defvar org-clock-hd-marker (make-marker)
  "Marker recording the last clock-in, but the headline position.")
(defvar org-clock-heading ""
  "The heading of the current clock entry.")
(defun org-clocking-buffer ()
  "Return the buffer where the clock is currently running.
Return nil if no clock is running."
  (marker-buffer org-clock-marker))
(defalias 'org-clock-is-active #'org-clocking-buffer)

(defun org-check-running-clock ()
  "Check if the current buffer contains the running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	     (y-or-n-p (format "Clock-out in buffer %s before killing it? "
			       (buffer-name))))
    (org-clock-out)
    (when (y-or-n-p "Save changed buffer?")
      (save-buffer))))

(defun org-clocktable-try-shift (dir n)
  "Check if this line starts a clock table, if yes, shift the time block."
  (when (org-match-line "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>")
    (org-clocktable-shift dir n)))

;;;###autoload
(defun org-clock-persistence-insinuate ()
  "Set up hooks for clock persistence."
  (require 'org-clock)
  (add-hook 'org-mode-hook 'org-clock-load)
  (add-hook 'kill-emacs-hook 'org-clock-save))

(defun org-clock-auto-clockout-insinuate ()
  "Set up hook for auto clocking out when Emacs is idle.
See `org-clock-auto-clockout-timer'.

This function is meant to be added to the user configuration."
  (require 'org-clock)
  (add-hook 'org-clock-in-hook #'org-clock-auto-clockout t))

(defgroup org-archive nil
  "Options concerning archiving in Org mode."
  :tag "Org Archive"
  :group 'org-structure)

(defcustom org-archive-location "%s_archive::"
  "The location where subtrees should be archived.

The value of this variable is a string, consisting of two parts,
separated by a double-colon.  The first part is a filename and
the second part is a headline.

When the filename is omitted, archiving happens in the same file.
%s in the filename will be replaced by the current file
name (without the directory part).  Archiving to a different file
is useful to keep archived entries from contributing to the
Org Agenda.

The archived entries will be filed as subtrees of the specified
headline.  When the headline is omitted, the subtrees are simply
filed away at the end of the file, as top-level entries.  Also in
the heading you can use %s to represent the file name, this can be
useful when using the same archive for a number of different files.

Here are a few examples:
\"%s_archive::\"
	If the current file is Projects.org, archive in file
	Projects.org_archive, as top-level trees.  This is the default.

\"::* Archived Tasks\"
	Archive in the current file, under the top-level headline
	\"* Archived Tasks\".

\"~/org/archive.org::\"
	Archive in file ~/org/archive.org (absolute path), as top-level trees.

\"~/org/archive.org::* From %s\"
	Archive in file ~/org/archive.org (absolute path), under headlines
        \"From FILENAME\" where file name is the current file name.

\"~/org/datetree.org::datetree/* Finished Tasks\"
        The \"datetree/\" string is special, signifying to archive
        items to the datetree.  Items are placed in either the CLOSED
        date of the item, or the current date if there is no CLOSED date.
        The heading will be a subentry to the current date.  There doesn't
        need to be a heading, but there always needs to be a slash after
        datetree.  For example, to store archived items directly in the
        datetree, use \"~/org/datetree.org::datetree/\".

\"basement::** Finished Tasks\"
	Archive in file ./basement (relative path), as level 3 trees
	below the level 2 heading \"** Finished Tasks\".

You may define it locally by setting an ARCHIVE property.  If
such a property is found in the file or in an entry, and anywhere
up the hierarchy, it will be used.

You can also set it for the whole file using the keyword-syntax:

#+ARCHIVE: basement::** Finished Tasks"
  :group 'org-archive
  :type 'string)

(defcustom org-agenda-skip-archived-trees t
  "Non-nil means the agenda will skip any items located in archived trees.
An archived tree is a tree marked with the tag ARCHIVE.  The use of this
variable is no longer recommended, you should leave it at the value t.
Instead, use the key `v' to cycle the archives-mode in the agenda."
  :group 'org-archive
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-columns-skip-archived-trees t
  "Non-nil means ignore archived trees when creating column view."
  :group 'org-archive
  :group 'org-properties
  :type 'boolean)

(define-obsolete-function-alias 'org-advertized-archive-subtree
  #'org-archive-subtree "9.8")

;; Declare Column View Code

(declare-function org-columns-get-format-and-top-level "org-colview" ())
(declare-function org-columns-compute "org-colview" (property))

;; Declare ID code

(declare-function org-id-locations-load "org-id")
(declare-function org-id-locations-save "org-id")
(defvar org-id-track-globally)

;;; Variables for pre-computed regular expressions, all buffer local

(defconst org-plain-time-of-day-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\(--?"
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\)?")
  "Regular expression to match a plain time or time range.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
1  the first time, range or not
8  the second time, if it is a range.")

(defconst org-plain-time-extension-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\+\\([0-9]+\\)\\(:\\([0-5][0-9]\\)\\)?")
  "Regular expression to match a time range like 13:30+2:10 = 13:30-15:40.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
7  hours of duration
9  minutes of duration")

(defconst org-stamp-time-of-day-regexp
  (concat
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +\\sw+ +\\)"
   "\\([012][0-9]:[0-5][0-9]\\)\\(-\\([012][0-9]:[0-5][0-9]\\)\\)?[^\n\r>]*?>"
   "\\(--?"
   "<\\1\\([012][0-9]:[0-5][0-9]\\)>\\)?")
  "Regular expression to match a timestamp time or time range.
After a match, the following groups carry important information:
0  the full match
1  date plus weekday, for back referencing to make sure
     both times are on the same day
2  the first time, range or not
4  the second time, if it is a range.")

(defun org-remove-keyword-keys (list)
  "Remove a pair of parenthesis at the end of each string in LIST."
  (mapcar (lambda (x)
	    (if (string-match "(.*)$" x)
		(substring x 0 (match-beginning 0))
	      x))
	  list))

;;; Some variables used in various places

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")
(defvar org-selected-window nil
  "Used in various places to store a window configuration.")
(defvar org-finish-function nil
  "Function to be called when \\`C-c C-c' is used.
This is for getting out of special buffers like capture.")
(defvar org-last-state)

;; Defined somewhere in this file, but used before definition.
(defvar org-entities)     ;; defined in org-entities.el
(defvar org-struct-menu)
(defvar org-org-menu)
(defvar org-tbl-menu)

;;;; Define the Org mode

(defun org-before-change-function (_beg _end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))
(defvar org-mode-map)
(defvar org-agenda-keep-modes nil)      ; Dynamically-scoped param.
(defvar org-inhibit-logging nil)        ; Dynamically-scoped param.
(defvar org-inhibit-blocking nil)       ; Dynamically-scoped param.

(defvar bidi-paragraph-direction)
(defvar buffer-face-mode-face)

(require 'outline)

;; Other stuff we need.
(when (< emacs-major-version 28)  ; preloaded in Emacs 28
  (require 'easymenu))

(require 'org-entities)
(require 'org-faces)
(require 'org-list)
(require 'org-pcomplete)
(require 'org-src)
(require 'org-footnote)
(require 'org-macro)

;; babel
(require 'ob)

(defvar org-element-cache-version); Defined in org-element.el
(defvar org-element-cache-persistent); Defined in org-element.el
(defvar org-element-use-cache); Defined in org-element.el
(defvar org-mode-loading nil
  "Non-nil during Org mode initialization.")

(defvar org-mode-tags-syntax-table
  (let ((st (make-syntax-table org-mode-syntax-table)))
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table including \"@\" and \"_\" as word constituents.")

;;;###autoload
;; Update `customize-package-emacs-version-alist'
(add-to-list 'customize-package-emacs-version-alist
	     '(Org ("8.0" . "24.4")
		   ("8.1" . "24.4")
		   ("8.2" . "24.4")
		   ("8.2.7" . "24.4")
		   ("8.3" . "26.1")
		   ("9.0" . "26.1")
		   ("9.1" . "26.1")
		   ("9.2" . "27.1")
		   ("9.3" . "27.1")
		   ("9.4" . "27.2")
		   ("9.5" . "28.1")
		   ("9.6" . "29.1")
                   ("9.7" . "30.1")
                   ("9.8" . "31.1")))

(defvar org-mode-transpose-word-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (dolist (c org-emphasis-alist st)
      (modify-syntax-entry (string-to-char (car c)) "w p" st))))

(when (fboundp 'abbrev-table-put)
  (abbrev-table-put org-mode-abbrev-table
		    :parents (list text-mode-abbrev-table)))

;;;; Font-Lock stuff, including the activators

(defconst org-match-substring-with-braces-regexp
  (concat
   "\\(\\S-\\)\\([_^]\\)"
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)")
  "The regular expression matching a sub- or superscript, forcing braces.")


(defun org-emphasize (&optional char)
  "Insert or change an emphasis, i.e. a font like bold or italic.
If there is an active region, change that region to a new emphasis.
If there is no region, just insert the marker characters and position
the cursor between them.
CHAR should be the marker character.  If it is a space, it means to
remove the emphasis of the selected region.
If CHAR is not given (for example in an interactive call) it will be
prompted for."
  (interactive nil org-mode)
  (let ((erc org-emphasis-regexp-components)
	(string "") beg end move s)
    (if (org-region-active-p)
	(setq beg (region-beginning)
	      end (region-end)
	      string (buffer-substring beg end))
      (setq move t))

    (unless char
      (message "Emphasis marker or tag: [%s]"
	       (mapconcat #'car org-emphasis-alist ""))
      (setq char (read-char-exclusive)))
    (if (equal char ?\s)
	(setq s ""
	      move nil)
      (unless (assoc (char-to-string char) org-emphasis-alist)
	(user-error "No such emphasis marker: \"%c\"" char))
      (setq s (char-to-string char)))
    (while (and (> (length string) 1)
		(equal (substring string 0 1) (substring string -1))
		(assoc (substring string 0 1) org-emphasis-alist))
      (setq string (substring string 1 -1)))
    (setq string (concat s string s))
    (when beg (delete-region beg end))
    (unless (or (bolp)
		(string-match (concat "[" (nth 0 erc) "\n]")
			      (char-to-string (char-before (point)))))
      (insert " "))
    (unless (or (eobp)
		(string-match (concat "[" (nth 1 erc) "\n]")
			      (char-to-string (char-after (point)))))
      (insert " ") (backward-char 1))
    (insert string)
    (and move (backward-char 1))))

(defcustom org-allow-promoting-top-level-subtree nil
  "When non-nil, allow promoting a top level subtree.
The leading star of the top level headline will be replaced
by a #."
  :type 'boolean
  :version "24.1"
  :group 'org-appearance)

;; FIXME: This function is unused.
(defsubst org-entry-beginning-position ()
  "Return the beginning position of the current entry."
  (save-excursion (org-back-to-heading t) (point)))

(defsubst org-entry-end-position ()
  "Return the end position of the current entry."
  (save-excursion (outline-next-heading) (point)))

(defun org-subtree-end-visible-p ()
  "Is the end of the current subtree visible?"
  (pos-visible-in-window-p
   (save-excursion (org-end-of-subtree t) (point))))

(defun org-first-headline-recenter ()
  "Move cursor to the first headline and recenter the headline."
  (let ((window (get-buffer-window)))
    (when window
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
	(set-window-start window (line-beginning-position))))))


;;; Indirect buffer display of subtrees

(defvar org-indirect-dedicated-frame nil
  "This is the frame being used for indirect tree display.")
(defvar org-last-indirect-buffer nil)

(defun org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.

With a numerical prefix ARG, go up to this level and then take that tree.
If ARG is negative, go up that many levels.

If `org-indirect-buffer-display' is not `new-frame', the command removes the
indirect buffer previously made with this command, to avoid proliferation of
indirect buffers.  However, when you call the command with a \
`\\[universal-argument]' prefix, or
when `org-indirect-buffer-display' is `new-frame', the last buffer is kept
so that you can work with several indirect buffers at the same time.  If
`org-indirect-buffer-display' is `dedicated-frame', the \
`\\[universal-argument]' prefix also
requests that a new frame be made for the new buffer, so that the dedicated
frame is not changed."
  (interactive "P" org-mode)
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
	beg end level heading ibuf
        (last-indirect-window
         (and org-last-indirect-buffer
              (get-buffer-window org-last-indirect-buffer))))
    (save-excursion
      (org-back-to-heading t)
      (when (numberp arg)
	(setq level (org-outline-level))
	(when (< arg 0) (setq arg (+ level arg)))
	(while (> (setq level (org-outline-level)) arg)
	  (org-up-heading-safe)))
      (setq beg (point)
	    heading (org-get-heading 'no-tags))
      (org-end-of-subtree t t)
      (when (and (not (eobp)) (org-at-heading-p)) (backward-char 1))
      (setq end (point)))
    (when (and (buffer-live-p org-last-indirect-buffer)
	       (not (eq org-indirect-buffer-display 'new-frame))
	       (not arg))
      (kill-buffer org-last-indirect-buffer))
    (setq ibuf (org-get-indirect-buffer cbuf heading)
	  org-last-indirect-buffer ibuf)
    (cond
     ((or (eq org-indirect-buffer-display 'new-frame)
	  (and arg (eq org-indirect-buffer-display 'dedicated-frame)))
      (select-frame (make-frame))
      (pop-to-buffer ibuf '(org-display-buffer-full-frame))
      (org-set-frame-title heading))
     ((eq org-indirect-buffer-display 'dedicated-frame)
      (raise-frame
       (select-frame (or (and org-indirect-dedicated-frame
			      (frame-live-p org-indirect-dedicated-frame)
			      org-indirect-dedicated-frame)
			 (setq org-indirect-dedicated-frame (make-frame)))))
      (pop-to-buffer ibuf '(org-display-buffer-full-frame))
      (org-set-frame-title (concat "Indirect: " heading)))
     ((eq org-indirect-buffer-display 'current-window)
      (pop-to-buffer-same-window ibuf))
     ((eq org-indirect-buffer-display 'other-window)
      (pop-to-buffer
       ibuf
       `(org-display-buffer-in-window (window . ,last-indirect-window)
                                      (same-frame . t))))
     (t (error "Invalid value")))
    (narrow-to-region beg end)
    (org-fold-show-all '(headings drawers blocks))
    (goto-char pos)
    (run-hook-with-args 'org-cycle-hook 'all)
    (and (window-live-p cwin) (select-window cwin))))

(cl-defun org-get-indirect-buffer (&optional (buffer (current-buffer)) heading)
  "Return an indirect buffer based on BUFFER.
If HEADING, append it to the name of the new buffer."
  (let* ((base-buffer (or (buffer-base-buffer buffer) buffer))
         (buffer-name (generate-new-buffer-name
                       (format "%s%s"
                               (buffer-name base-buffer)
                               (if heading
                                   (concat "::" heading)
                                 ""))))
         (indirect-buffer (make-indirect-buffer base-buffer buffer-name 'clone)))
    ;; Decouple folding state.  We need to do it manually since
    ;; `make-indirect-buffer' does not run
    ;; `clone-indirect-buffer-hook'.
    (org-fold-core-decouple-indirect-buffer-folds)
    indirect-buffer))

(defun org-set-frame-title (title)
  "Set the title of the current frame to the string TITLE."
  (modify-frame-parameters (selected-frame) (list (cons 'name title))))

;;;; Structure editing

;;; Inserting headlines

(defun org--blank-before-heading-p (&optional parent)
  "Non-nil when an empty line should precede a new heading here.
When optional argument PARENT is non-nil, consider parent
headline instead of current one."
  (pcase (assq 'heading org-blank-before-new-entry)
    (`(heading . auto)
     (save-excursion
       (org-with-limited-levels
        (unless (and (org-before-first-heading-p)
                     (not (outline-next-heading)))
          (org-back-to-heading t)
          (when parent (org-up-heading-safe))
          (cond ((not (bobp))
                 (org-previous-line-empty-p))
		((outline-next-heading)
		 (org-previous-line-empty-p))
		;; Ignore trailing spaces on last buffer line.
		((progn (skip-chars-backward " \t") (bolp))
		 (org-previous-line-empty-p))
		(t nil))))))
    (`(heading . ,value) value)
    (_ nil)))

(defun org-insert-heading (&optional arg invisible-ok level)
  "Insert a new heading or an item with the same depth at point.

If point is at the beginning of a heading, insert a new heading
or a new headline above the current one.  When at the beginning
of a regular line of text, turn it into a heading.

If point is in the middle of a line, split it and create a new
headline with the text in the current line after point (see
`org-M-RET-may-split-line' on how to modify this behavior).  As
a special case, on a headline, splitting can only happen on the
title itself.  E.g., this excludes breaking stars or tags.

With a `\\[universal-argument]' prefix, set \
`org-insert-heading-respect-content' to
a non-nil value for the duration of the command.  This forces the
insertion of a heading after the current subtree, independently
on the location of point.

With a `\\[universal-argument] \\[universal-argument]' prefix, \
insert the heading at the end of the tree
above the current heading.  For example, if point is within a
2nd-level heading, then it will insert a 2nd-level heading at
the end of the 1st-level parent subtree.

When INVISIBLE-OK is set, stop at invisible headlines when going
back.  This is important for non-interactive uses of the
command.

When optional argument LEVEL is a number, insert a heading at
that level.  For backwards compatibility, when LEVEL is non-nil
but not a number, insert a level-1 heading."
  (interactive "P" org-mode)
  (let* ((blank? (org--blank-before-heading-p (equal arg '(16))))
         (current-level (org-current-level))
         (num-stars (or
                     ;; Backwards compat: if LEVEL non-nil, level is 1
                     (and level (if (wholenump level) level 1))
                     current-level
                     ;; This `1' is for when before first headline
                     1))
         (stars (make-string num-stars ?*))
         (maybe-add-blank-after
          (lambda (blank?)
            "Add a blank line before next heading when BLANK? is non-nil.
Assume that point is on the inserted heading."
            (save-excursion
              (end-of-line)
              (unless (eobp)
                (forward-char)
                (when (and blank? (org-at-heading-p))
                  (insert "\n")))))))
    (cond
     ((or org-insert-heading-respect-content
	  (member arg '((4) (16)))
	  (and (not invisible-ok)
	       (invisible-p (max (1- (point)) (point-min)))))
      ;; Position point at the location of insertion.  Make sure we
      ;; end up on a visible headline if INVISIBLE-OK is nil.
      (org-preserve-local-variables
       (org-with-limited-levels
        (if (not current-level) (outline-next-heading) ;before first headline
          (org-back-to-heading invisible-ok)
          (when (equal arg '(16)) (org-up-heading-safe))
          (org-end-of-subtree invisible-ok 'to-heading)))
       ;; At `point-max', if the file does not have ending newline,
       ;; create one, so that we are not appending stars at non-empty
       ;; line.
       (unless (bolp) (insert "\n"))
       (when (and blank? (save-excursion
                           (backward-char)
                           (org-before-first-heading-p)))
         (insert "\n")
         (backward-char))
       (when (and (not current-level) (not (eobp)) (not (bobp)))
         (when (org-at-heading-p) (insert "\n"))
         (backward-char))
       (unless (and blank? (org-previous-line-empty-p))
         (org-N-empty-lines-before-current (if blank? 1 0)))
       (insert stars " " "\n")
       ;; Move point after stars.
       (backward-char)
       ;; Retain blank lines before next heading.
       (funcall maybe-add-blank-after blank?)
       ;; When INVISIBLE-OK is non-nil, ensure newly created headline
       ;; is visible.
       (unless invisible-ok
         (if (eq org-fold-core-style 'text-properties)
             (cond
              ((org-fold-folded-p
                (max (point-min)
                     (1- (line-beginning-position))))
               (org-fold-region (line-end-position 0) (line-end-position) nil))
              (t nil))
           (pcase (get-char-property-and-overlay (point) 'invisible)
             (`(outline . ,o)
              (move-overlay o (overlay-start o) (line-end-position 0)))
             (_ nil))))))
     ;; At a headline...
     ((org-at-heading-p)
      (cond ((bolp)
	     (when blank? (save-excursion (insert "\n")))
	     (save-excursion (insert stars " \n"))
	     (unless (and blank? (org-previous-line-empty-p))
	       (org-N-empty-lines-before-current (if blank? 1 0)))
	     (end-of-line))
	    ((and (org-get-alist-option org-M-RET-may-split-line 'headline)
		  (org-match-line org-complex-heading-regexp)
		  (org-pos-in-match-range (point) 4))
	     ;; Grab the text that should moved to the new headline.
	     ;; Preserve tags.
	     (let ((split (delete-and-extract-region (point) (match-end 4))))
	       (if (looking-at "[ \t]*$") (replace-match "")
		 (when org-auto-align-tags (org-align-tags)))
	       (end-of-line)
	       (when blank? (insert "\n"))
	       (insert "\n" stars " ")
               ;; Retain blank lines before next heading.
               (funcall maybe-add-blank-after blank?)
	       (when (org-string-nw-p split) (insert split))))
	    (t
	     (end-of-line)
	     (when blank? (insert "\n"))
	     (insert "\n" stars " ")
             ;; Retain blank lines before next heading.
             (funcall maybe-add-blank-after blank?))))
     ;; On regular text, turn line into a headline or split, if
     ;; appropriate.
     ((bolp)
      (insert stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0)))
      ;; Retain blank lines before next heading.
      (funcall maybe-add-blank-after blank?))
     (t
      (unless (org-get-alist-option org-M-RET-may-split-line 'headline)
        (end-of-line))
      (insert "\n" stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0)))
      ;; Retain blank lines before next heading.
      (funcall maybe-add-blank-after blank?))))
  (run-hooks 'org-insert-heading-hook))

(defun org-N-empty-lines-before-current (n)
  "Make the number of empty lines before current exactly N.
So this will delete or add empty lines."
  (let ((column (current-column)))
    (forward-line 0)
    (unless (bobp)
      (let ((start (save-excursion
		     (skip-chars-backward " \r\t\n")
		     (line-end-position))))
	(delete-region start (line-end-position 0))))
    (insert (make-string n ?\n))
    (move-to-column column)))

(defun org-get-heading (&optional no-tags no-todo no-priority no-comment)
  "Return the heading of the current entry, without the stars.
When NO-TAGS is non-nil, don't include tags.
When NO-TODO is non-nil, don't include TODO keywords.
When NO-PRIORITY is non-nil, don't include priority cookie.
When NO-COMMENT is non-nil, don't include COMMENT string.
Return nil before first heading."
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((case-fold-search nil))
	(looking-at org-complex-heading-regexp)
        ;; When using `org-fold-core--optimize-for-huge-buffers',
        ;; returned text will be invisible.  Clear it up.
        (save-match-data
          (org-fold-core-remove-optimization (match-beginning 0) (match-end 0)))
        (let ((todo (and (not no-todo) (match-string 2)))
	      (priority (and (not no-priority) (match-string 3)))
	      (headline (pcase (match-string 4)
			  (`nil "")
			  ((and (guard no-comment) h)
			   (replace-regexp-in-string
			    (eval-when-compile
			      (format "\\`%s[ \t]+" org-comment-string))
			    "" h))
			  (h h)))
	      (tags (and (not no-tags) (match-string 5))))
          ;; Restore cleared optimization.
          (org-fold-core-update-optimization (match-beginning 0) (match-end 0))
	  (mapconcat #'identity
		     (delq nil (list todo priority headline tags))
		     " "))))))

(defun org-heading-components ()
  "Return the components of the current heading.
This is a list with the following elements:
- the level as an integer
- the reduced level, different if `org-odd-levels-only' is set.
- the TODO keyword, or nil
- the priority value, like ?A or 42, or nil if no priority is given
- the headline text itself, or the tags string if no headline text
- the tags string, or nil."
  (save-excursion
    (org-back-to-heading t)
    (when (let (case-fold-search) (looking-at org-complex-heading-regexp))
      (org-fold-core-remove-optimization (match-beginning 0) (match-end 0))
      (prog1
          (list (length (match-string 1))
	        (org-reduced-level (length (match-string 1)))
	        (match-string-no-properties 2)
	        (and (match-end 3) (org-priority-to-value (substring (match-string 3) 2 -1)))
	        (match-string-no-properties 4)
	        (match-string-no-properties 5))
        (org-fold-core-update-optimization (match-beginning 0) (match-end 0))))))

(defun org-get-entry ()
  "Get the entry text, after heading, entire subtree."
  (save-excursion
    (org-back-to-heading t)
    (filter-buffer-substring (line-beginning-position 2) (org-end-of-subtree t))))

(defun org-edit-headline (&optional heading)
  "Edit the current headline.
Set it to HEADING when provided."
  (interactive nil org-mode)
  (let ((beg (point-min))
	(end (point-max)))
    (org-with-wide-buffer
     (org-back-to-heading t)
     (let ((case-fold-search nil))
       (when (looking-at org-complex-heading-regexp)
	 (let* ((old (match-string-no-properties 4))
		(new
		 (save-match-data
		   (org-trim
		    (or heading
			(save-restriction
			  (when (and (<= beg (point))
				     (<= (line-end-position) end))
			    (narrow-to-region beg end))
			  (read-string "Edit: " old)))))))
	   (unless (equal old new)
	     (if old (replace-match new t t nil 4)
	       (goto-char (or (match-end 3) (match-end 2) (match-end 1)))
	       (insert " " new))
	     (when org-auto-align-tags (org-align-tags))
	     (when (looking-at "[ \t]*$") (replace-match "")))))))))

(defun org-insert-heading-after-current ()
  "Insert a new heading with same level as current, after current subtree."
  (interactive nil org-mode)
  (org-back-to-heading)
  (org-insert-heading)
  (org-move-subtree-down)
  (end-of-line 1))

(defun org-insert-heading-respect-content (&optional invisible-ok)
  "Insert heading with `org-insert-heading-respect-content' set to t."
  (interactive nil org-mode)
  (org-insert-heading '(4) invisible-ok))

(defun org-insert-todo-heading-respect-content (&optional arg)
  "Call `org-insert-todo-heading', inserting after current subtree.
ARG is passed to `org-insert-todo-heading'.
This command temporarily sets `org-insert-heading-respect-content' to t."
  (interactive "P" org-mode)
  (let ((org-insert-heading-respect-content t))
    (org-insert-todo-heading arg t)))

(defun org-insert-todo-heading (arg &optional force-heading)
  "Insert a new heading with the same level and TODO state as current heading.

If the heading has no TODO state, or if the state is DONE, use
the first state (TODO by default).  Also with `\\[universal-argument]'
prefix, force first state.  With a `\\[universal-argument]
\\[universal-argument]' prefix, force inserting at the end of the
parent subtree.

When called at a plain list item, insert a new item with an
unchecked check box."
  (interactive "P" org-mode)
  (when (or force-heading (not (org-insert-item 'checkbox)))
    (org-insert-heading (or (and (equal arg '(16)) '(16))
			    force-heading))
    (save-excursion
      (org-forward-heading-same-level -1)
      (let ((case-fold-search nil)) (looking-at org-todo-line-regexp)))
    (let* ((new-mark-x
	    (if (or (equal arg '(4))
		    (not (match-beginning 2))
		    (member (match-string 2) org-done-keywords))
		(car org-todo-keywords-1)
	      (match-string 2)))
	   (new-mark
	    (or
	     (run-hook-with-args-until-success
	      'org-todo-get-default-hook new-mark-x nil)
	     new-mark-x)))
      (forward-line 0)
      (and (looking-at org-outline-regexp) (goto-char (match-end 0))
	   (if org-treat-insert-todo-heading-as-state-change
	       (org-todo new-mark)
	     (insert new-mark " "))))
    (when org-provide-todo-statistics
      (org-update-parent-todo-statistics))))

(defun org-insert-subheading (arg)
  "Insert a new subheading and demote it.
Works for outline headings and for plain lists alike.
The prefix argument ARG is passed to `org-insert-heading'.
Unlike `org-insert-heading', when point is at the beginning of a
heading, still insert the new sub-heading below."
  (interactive "P" org-mode)
  (when (and (bolp) (not (eobp)) (not (eolp))) (forward-char))
  (org-insert-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

(defun org-insert-todo-subheading (arg)
  "Insert a new subheading with TODO keyword or checkbox and demote it.
Works for outline headings and for plain lists alike.
The prefix argument ARG is passed to `org-insert-todo-heading'."
  (interactive "P" org-mode)
  (org-insert-todo-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

;;; Promotion and Demotion

(defvar org-after-demote-entry-hook nil
  "Hook run after an entry has been demoted.
The cursor will be at the beginning of the entry.
When a subtree is being demoted, the hook will be called for each node.")

(defvar org-after-promote-entry-hook nil
  "Hook run after an entry has been promoted.
The cursor will be at the beginning of the entry.
When a subtree is being promoted, the hook will be called for each node.")

(defun org-promote-subtree ()
  "Promote the entire subtree.
See also `org-promote'."
  (interactive nil org-mode)
  (save-excursion
    (org-back-to-heading t)
    (org-combine-change-calls (point) (save-excursion (org-end-of-subtree t))
      (org-with-limited-levels (org-map-tree 'org-promote))))
  (org-fix-position-after-promote))

(defun org-demote-subtree ()
  "Demote the entire subtree.
See `org-demote' and `org-promote'."
  (interactive nil org-mode)
  (save-excursion
    (org-back-to-heading t)
    (org-combine-change-calls (point) (save-excursion (org-end-of-subtree t))
      (org-with-limited-levels (org-map-tree 'org-demote))))
  (org-fix-position-after-promote))

(defun org-do-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all
headings in the region."
  (interactive nil org-mode)
  (save-excursion
    (if (org-region-active-p)
        (progn
          (org-map-region 'org-promote (region-beginning) (region-end))
          (setq deactivate-mark (org--deactivate-mark)))
      (org-promote)))
  (org-fix-position-after-promote))

(defun org-do-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all
headings in the region."
  (interactive nil org-mode)
  (save-excursion
    (if (org-region-active-p)
        (progn
          (org-map-region 'org-demote (region-beginning) (region-end))
          (setq deactivate-mark (org--deactivate-mark)))
      (org-demote)))
  (org-fix-position-after-promote))

(defun org-fix-position-after-promote ()
  "Fix cursor position and indentation after demoting/promoting."
  (let ((pos (point)))
    (when (save-excursion
	    (forward-line 0)
	    (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	    (or (eq pos (match-end 1)) (eq pos (match-end 2))))
      (cond ((eobp) (insert " "))
	    ((eolp) (insert " "))
	    ((equal (char-after) ?\s) (forward-char 1))))))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the current line.
Returns 0 for the first headline in the buffer, and nil if before the
first headline."
  (and (org-current-level)
       (or (and (/= (line-beginning-position) (point-min))
		(save-excursion (forward-line -1) (org-current-level)))
	   0)))

(defun org-level-increment ()
  "Return the number of stars that will be added or removed at a
time to headlines when structure editing, based on the value of
`org-odd-levels-only'."
  (if org-odd-levels-only 2 1))

(defun org-get-valid-level (level &optional change)
  "Rectify a level change under the influence of `org-odd-levels-only'.
LEVEL is a current level, CHANGE is by how much the level should
be modified.  Even if CHANGE is nil, LEVEL may be returned
modified because even level numbers will become the next higher
odd number.  Returns values greater than 0."
  (if org-odd-levels-only
      (cond ((or (not change) (= 0 change)) (1+ (* 2 (/ level 2))))
	    ((> change 0) (1+ (* 2 (/ (+ (1- level) (* 2 change)) 2))))
	    ((< change 0) (max 1 (1+ (* 2 (/ (+ level (* 2 change)) 2))))))
    (max 1 (+ level (or change 0)))))

(defun org-promote ()
  "Promote the current heading higher up the tree."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((after-change-functions (remq 'flyspell-after-change-function
					after-change-functions))
	  (level (save-match-data (funcall outline-level)))
	  (up-head (concat (make-string (org-get-valid-level level -1) ?*) " "))
	  (diff (abs (- level (length up-head) -1))))
     (cond
      ((and (= level 1) org-allow-promoting-top-level-subtree)
       (replace-match "# " nil t))
      ((= level 1)
       (user-error "Cannot promote to level 0.  UNDO to recover if necessary"))
      (t (replace-match (apply #'propertize up-head (text-properties-at (match-beginning 0))) t)))
     (unless (= level 1)
       (when org-auto-align-tags (org-align-tags))
       (when org-adapt-indentation (org-fixup-indentation (- diff))))
     (run-hooks 'org-after-promote-entry-hook))))

(defun org-demote ()
  "Demote the current heading lower down the tree."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((after-change-functions (remq 'flyspell-after-change-function
					after-change-functions))
	  (level (save-match-data (funcall outline-level)))
	  (down-head (concat (make-string (org-get-valid-level level 1) ?*) " "))
	  (diff (abs (- level (length down-head) -1))))
     (org-fold-core-ignore-fragility-checks
       (replace-match (apply #'propertize down-head (text-properties-at (match-beginning 0))) t)
       (when org-auto-align-tags (org-align-tags))
       (when org-adapt-indentation (org-fixup-indentation diff)))
     (run-hooks 'org-after-demote-entry-hook))))

(defun org-cycle-level ()
  "Cycle the level of an empty headline through possible states.
This goes first to child, then to parent, level, then up the hierarchy.
After top level, it switches back to sibling level."
  (interactive nil org-mode)
  (let ((org-adapt-indentation nil))
    (when (and (org-point-at-end-of-empty-headline)
               (not (and (featurep 'org-inlinetask)
                       (org-inlinetask-in-task-p))))
      (setq this-command 'org-cycle-level) ; Only needed for caching
      (let ((cur-level (org-current-level))
            (prev-level (org-get-previous-line-level)))
        (cond
         ;; If first headline in file, promote to top-level.
         ((= prev-level 0)
          (cl-loop repeat (/ (- cur-level 1) (org-level-increment))
		   do (org-do-promote)))
         ;; If same level as prev, demote one.
         ((= prev-level cur-level)
          (org-do-demote))
         ;; If parent is top-level, promote to top level if not already.
         ((= prev-level 1)
          (cl-loop repeat (/ (- cur-level 1) (org-level-increment))
		   do (org-do-promote)))
         ;; If top-level, return to prev-level.
         ((= cur-level 1)
          (cl-loop repeat (/ (- prev-level 1) (org-level-increment))
		   do (org-do-demote)))
         ;; If less than prev-level, promote one.
         ((< cur-level prev-level)
          (org-do-promote))
         ;; If deeper than prev-level, promote until higher than
         ;; prev-level.
         ((> cur-level prev-level)
          (cl-loop repeat (+ 1 (/ (- cur-level prev-level) (org-level-increment)))
		   do (org-do-promote))))
        t))))

(defvar org-property-drawer-re)

(defun org-fixup-indentation (diff)
  "Change the indentation in the current entry by DIFF.

DIFF is an integer.  Indentation is done according to the
following rules:

  - Planning information and property drawers are always indented
    according to the new level of the headline;

  - Footnote definitions and their contents are ignored;

  - Inlinetasks' boundaries are not shifted;

  - Empty lines are ignored;

  - Other lines' indentation are shifted by DIFF columns, unless
    it would introduce a structural change in the document, in
    which case no shifting is done at all.

Assume point is at a heading or an inlinetask beginning."
  (org-with-wide-buffer
   (narrow-to-region (line-beginning-position)
		     (save-excursion
		       (if (org-with-limited-levels (org-at-heading-p))
			   (org-with-limited-levels (outline-next-heading))
			 (org-inlinetask-goto-end))
		       (point)))
   (forward-line)
   ;; Indent properly planning info and property drawer.
   (when (looking-at-p org-planning-line-re)
     (org-indent-line)
     (forward-line))
   (when (looking-at org-property-drawer-re)
     (goto-char (match-end 0))
     (forward-line)
     (org-indent-region (match-beginning 0) (match-end 0)))
   (when (looking-at org-logbook-drawer-re)
     (let ((end-marker  (move-marker (make-marker) (match-end 0)))
	   (col (+ (current-indentation) diff)))
       (when (wholenump col)
	 (while (< (point) end-marker)
           (if (natnump diff)
	       (insert (make-string diff 32))
             (delete-char (abs diff)))
	   (forward-line)))))
   (catch 'no-shift
     (when (or (zerop diff) (not (eq org-adapt-indentation t)))
       (throw 'no-shift nil))
     ;; If DIFF is negative, first check if a shift is possible at all
     ;; (e.g., it doesn't break structure).  This can only happen if
     ;; some contents are not properly indented.
     (let ((case-fold-search t))
       (when (< diff 0)
	 (let ((diff (- diff))
	       (forbidden-re (concat org-outline-regexp
				     "\\|"
				     (substring org-footnote-definition-re 1))))
	   (save-excursion
	     (while (not (eobp))
	       (cond
		((looking-at-p "[ \t]*$") (forward-line))
		((and (looking-at-p org-footnote-definition-re)
		      (let ((e (org-element-at-point)))
			(and (org-element-type-p e 'footnote-definition)
			     (goto-char (org-element-end e))))))
		((looking-at-p org-outline-regexp) (forward-line))
		;; Give up if shifting would move before column 0 or
		;; if it would introduce a headline or a footnote
		;; definition.
		(t
		 (skip-chars-forward " \t")
		 (let ((ind (current-column)))
		   (when (or (< ind diff)
			     (and (= ind diff) (looking-at-p forbidden-re)))
		     (throw 'no-shift nil)))
		 ;; Ignore contents of example blocks and source
		 ;; blocks if their indentation is meant to be
		 ;; preserved.  Jump to block's closing line.
		 (forward-line 0)
		 (or (and (looking-at-p "[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\)")
			  (let ((e (org-element-at-point)))
			    (and (org-src-preserve-indentation-p e)
			         (goto-char (org-element-end e))
			         (progn (skip-chars-backward " \r\t\n")
				        (forward-line 0)
				        t))))
		     (forward-line))))))))
       ;; Shift lines but footnote definitions, inlinetasks boundaries
       ;; by DIFF.  Also skip contents of source or example blocks
       ;; when indentation is meant to be preserved.
       (while (not (eobp))
	 (cond
	  ((and (looking-at-p org-footnote-definition-re)
		(let ((e (org-element-at-point)))
		  (and (org-element-type-p e 'footnote-definition)
		       (goto-char (org-element-end e))))))
	  ((looking-at-p org-outline-regexp) (forward-line))
	  ((looking-at-p "[ \t]*$") (forward-line))
	  (t
	   (indent-line-to (+ (current-indentation) diff))
	   (forward-line 0)
	   (or (and (looking-at-p "[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\)")
		    (let ((e (org-element-at-point)))
		      (and (org-src-preserve-indentation-p e)
			   (goto-char (org-element-end e))
			   (progn (skip-chars-backward " \r\t\n")
				  (forward-line 0)
				  t))))
	       (forward-line)))))))))

(defun org-convert-to-odd-levels ()
  "Convert an Org file with all levels allowed to one with odd levels.
This will leave level 1 alone, convert level 2 to level 3, level 3 to
level 5 etc."
  (interactive nil org-mode)
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd? ")
    (let ((outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (- (length (match-string 0)) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-demote))
	  (end-of-line 1))))))

(defun org-convert-to-oddeven-levels ()
  "Convert an Org file with only odd levels to one with odd/even levels.
This promotes level 3 to level 2, level 5 to level 3 etc.  If the
file contains a section with an even level, conversion would
destroy the structure of the file.  An error is signaled in this
case."
  (interactive nil org-mode)
  (goto-char (point-min))
  ;; First check if there are no even levels
  (when (re-search-forward "^\\(\\*\\*\\)+ " nil t)
    (org-fold-show-set-visibility 'canonical)
    (error "Not all levels are odd in this file.  Conversion not possible"))
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd-even? ")
    (let ((outline-regexp org-outline-regexp)
	  (outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (/ (1- (length (match-string 0))) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-promote))
	  (end-of-line 1))))))

(defun org-tr-level (n)
  "Make N odd if required."
  (if org-odd-levels-only (1+ (/ n 2)) n))

;;; Vertical tree motion, cutting and pasting of subtrees

(defun org-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p" org-mode)
  (org-move-subtree-down (- (prefix-numeric-value arg))))

(defun org-clean-visibility-after-subtree-move ()
  "Fix visibility issues after moving a subtree."
  ;; First, find a reasonable region to look at:
  ;; Start two siblings above, end three below
  (let* ((beg (save-excursion
		(and (org-get-previous-sibling)
		     (org-get-previous-sibling))
		(point)))
	 (end (save-excursion
		(and (org-get-next-sibling)
		     (org-get-next-sibling)
		     (org-get-next-sibling))
		(if (org-at-heading-p)
		    (line-end-position)
		  (point))))
	 (level (looking-at "\\*+"))
	 (re (when level (concat "^" (regexp-quote (match-string 0)) " "))))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(when re
	  ;; Properly fold already folded siblings
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (when (and (not (org-invisible-p))
		       (org-invisible-p (line-end-position)))
	      (org-fold-heading nil))))
	(org-cycle-hide-drawers 'all)
	(org-cycle-show-empty-lines 'overview)))))

(defun org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p" org-mode)
  (setq arg (prefix-numeric-value arg))
  (org-preserve-local-variables
   (let ((movfunc (if (> arg 0) 'org-get-next-sibling
		    'org-get-previous-sibling))
	 (ins-point (make-marker))
	 (cnt (abs arg))
	 (col (current-column))
	 beg end txt folded)
     ;; Select the tree
     (org-back-to-heading)
     (setq beg (point))
     (save-match-data
       (save-excursion (outline-end-of-heading)
		       (setq folded (org-invisible-p)))
       (progn (org-end-of-subtree nil t)
	      (unless (eobp) (backward-char))))
     (outline-next-heading)
     (setq end (point))
     (goto-char beg)
     ;; Find insertion point, with error handling
     (while (> cnt 0)
       (unless (and (funcall movfunc) (looking-at org-outline-regexp))
	 (goto-char beg)
	 (user-error "Cannot move past superior level or buffer limit"))
       (setq cnt (1- cnt)))
     (when (> arg 0)
       ;; Moving forward - still need to move over subtree
       (org-end-of-subtree t t)
       (save-excursion
	 (org-back-over-empty-lines)
	 (or (bolp) (newline))))
     (move-marker ins-point (point))
     (setq txt (buffer-substring beg end))
     (org-save-markers-in-region beg end)
     (delete-region beg end)
     (when (eq org-fold-core-style 'overlays) (org-remove-empty-overlays-at beg))
     (unless (= beg (point-min)) (org-fold-region (1- beg) beg nil 'outline))
     (unless (bobp) (org-fold-region (1- (point)) (point) nil 'outline))
     (and (not (bolp)) (looking-at "\n") (forward-char 1))
     (let ((bbb (point)))
       (insert-before-markers txt)
       (org-reinstall-markers-in-region bbb)
       (move-marker ins-point bbb))
     (or (bolp) (insert "\n"))
     (goto-char ins-point)
     (org-skip-whitespace)
     (move-marker ins-point nil)
     (if folded
	 (org-fold-subtree t)
       (org-fold-show-entry 'hide-drawers)
       (org-fold-show-children))
     (org-clean-visibility-after-subtree-move)
     ;; move back to the initial column we were at
     (move-to-column col))))

(defvar org-subtree-clip ""
  "Clipboard for cut and paste of subtrees.
This is actually only a copy of the kill, because we use the normal kill
ring.  We need it to check if the kill was created by `org-copy-subtree'.")

(defvar org-subtree-clip-folded nil
  "Was the last copied subtree folded?
This is used to fold the tree back after pasting.")

(defun org-cut-subtree (&optional n)
  "Cut the current subtree into the clipboard.
With prefix arg N, cut this many sequential subtrees.
This is a short-hand for marking the subtree and then cutting it."
  (interactive "p" org-mode)
  (org-copy-subtree n 'cut))

(defun org-copy-subtree (&optional n cut force-store-markers nosubtrees)
  "Copy the current subtree into the clipboard.
With prefix arg N, copy this many sequential subtrees.
This is a short-hand for marking the subtree and then copying it.
If CUT is non-nil, actually cut the subtree.
If FORCE-STORE-MARKERS is non-nil, store the relative locations
of some markers in the region, even if CUT is non-nil.  This is
useful if the caller implements cut-and-paste as copy-then-paste-then-cut."
  (interactive "p" org-mode)
  (org-preserve-local-variables
   (let (beg end folded subtree-text (beg0 (point)))
     (if (called-interactively-p 'any)
	 (org-back-to-heading nil)    ; take what looks like a subtree
       (org-back-to-heading t))	      ; take what is really there
     ;; Do not consider inlinetasks as a subtree.
     (when (org-element-type-p (org-element-at-point) 'inlinetask)
       (org-up-element))
     (setq beg (point))
     (skip-chars-forward " \t\r\n")
     (save-match-data
       (if nosubtrees
	   (outline-next-heading)
	 (save-excursion (outline-end-of-heading)
			 (setq folded (org-invisible-p)))
	 (ignore-errors (org-forward-heading-same-level (1- n) t))
	 (org-end-of-subtree t t)))
     ;; Include the end of an inlinetask
     (when (and (featurep 'org-inlinetask)
		(looking-at-p (concat (org-inlinetask-outline-regexp)
				      "END[ \t]*$")))
       (end-of-line))
     (setq end (point))
     (goto-char beg0)
     (when (> end beg)
       (setq org-subtree-clip-folded folded)
       (when (or cut force-store-markers)
	 (org-save-markers-in-region beg end))
       (setq subtree-text (buffer-substring-no-properties beg end))
       (if cut (kill-region beg end) (copy-region-as-kill beg end))
       (setq org-subtree-clip subtree-text)
       (message "%s: Subtree(s) with %d characters"
		(if cut "Cut" "Copied")
		(length org-subtree-clip))
       subtree-text))))

(defun org-paste-subtree (&optional level tree for-yank remove)
  "Paste the clipboard as a subtree, with modification of headline level.

The entire subtree is promoted or demoted in order to match a new headline
level.

If the cursor is at the beginning of a headline, the same level as
that headline is used to paste the tree before current headline.

With `\\[universal-argument]' prefix, force inserting at the same level
as current headline, after subtree at point.

With `\\[universal-argument]' `\\[universal-argument]' prefix, force
inserting as a child headline, as the first child.

If not, the new level is derived from the *visible* headings
before and after the insertion point, and taken to be the inferior headline
level of the two.  So if the previous visible heading is level 3 and the
next is level 4 (or vice versa), level 4 will be used for insertion.
This makes sure that the subtree remains an independent subtree and does
not swallow low level entries.

You can also force a different level, either by using a numeric prefix
argument, or by inserting the heading marker by hand.  For example, if the
cursor is after \"*****\", then the tree will be shifted to level 5.

If optional TREE is given, use this text instead of the kill ring.

When FOR-YANK is set, this is called by `org-yank'.  In this case, do not
move back over whitespace before inserting, and move point to the end of
the inserted text when done.

When REMOVE is non-nil, remove the subtree from the clipboard."
  (interactive "P" org-mode)
  (setq tree (or tree (current-kill 0)))
  (unless (org-kill-is-subtree-p tree)
    (user-error
     (substitute-command-keys
      "The kill is not a (set of) tree(s).  Use `\\[yank]' to yank anyway")))
  (org-with-limited-levels
   (org-fold-core-ignore-fragility-checks
     (let* ((visp (not (org-invisible-p)))
	    (txt tree)
	    (old-level (if (string-match org-outline-regexp-bol txt)
			   (- (match-end 0) (match-beginning 0) 1)
		         -1))
            level-indicator?
	    (force-level
	     (cond
	      ;; When point is after the stars in an otherwise empty
	      ;; headline, use the number of stars as the forced level.
	      ((and (or (not level) (member level '((4) (16))))
                    (org-match-line "^\\*+[ \t]*$")
		    (not (eq ?* (char-after))))
	       (setq level-indicator? (org-outline-level)))
              ((equal level '(4)) (org-outline-level))
              ((equal level '(16)) nil) ; handle later
	      (level (prefix-numeric-value level))
	      ((looking-at-p org-outline-regexp-bol) (org-outline-level))))
	    (previous-level
	     (save-excursion
	       (unless (org-at-heading-p) (org-previous-visible-heading 1))
	       (if (org-at-heading-p) (org-outline-level) 1)))
	    (next-level
	     (save-excursion
	       (org-next-visible-heading 1)
	       (if (org-at-heading-p) (org-outline-level) 1)))
	    (new-level (or force-level
                           (max
                            ;; C-u C-u forces child.
                            (if (equal level '(16)) (1+ previous-level) 0)
                            previous-level
                            next-level)))
	    (shift (if (or (= old-level -1)
			   (= new-level -1)
			   (= old-level new-level))
		       0
		     (- new-level old-level)))
	    (delta (if (> shift 0) -1 1))
	    (func (if (> shift 0) #'org-demote #'org-promote))
	    (org-odd-levels-only nil)
	    beg end newend)
       ;; Remove the forced level indicator.
       (when level-indicator?
         (delete-region (line-beginning-position) (line-beginning-position 2)))
       ;; Paste before the next visible heading or at end of buffer,
       ;; unless point is at the beginning of a headline.
       (unless (and (bolp) (org-at-heading-p) (not (member level '((4) (16)))))
         (when (equal level '(4)) (org-end-of-subtree t))
         (org-next-visible-heading 1)
         (unless (bolp) (insert "\n")))
       (setq beg (point))
       ;; Avoid re-parsing cache elements when i.e. level 1 heading
       ;; is inserted and then promoted.
       (org-combine-change-calls beg beg
         (when (fboundp 'org-id-paste-tracker) (org-id-paste-tracker txt))
         (insert txt)
         (unless (string-suffix-p "\n" txt) (insert "\n"))
         (setq newend (point))
         (org-reinstall-markers-in-region beg)
         (setq end (point))
         (goto-char beg)
         (skip-chars-forward " \t\n\r")
         (setq beg (point))
         (when (and (org-invisible-p) visp)
           (save-excursion (org-fold-heading nil)))
         ;; Shift if necessary.
         (unless (= shift 0)
           (save-restriction
	     (narrow-to-region beg end)
	     (while (not (= shift 0))
	       (org-map-region func (point-min) (point-max))
	       (setq shift (+ delta shift)))
	     (goto-char (point-min))
	     (setq newend (point-max)))))
       (when (or for-yank (called-interactively-p 'interactive))
         (message "Clipboard pasted as level %d subtree" new-level))
       (when (and (not for-yank) ; in this case, org-yank will decide about folding
		  (equal org-subtree-clip tree)
		  org-subtree-clip-folded)
         ;; The tree was folded before it was killed/copied
         (org-fold-subtree t))
       (when for-yank (goto-char newend))
       (when remove (pop kill-ring))))))

(defun org-kill-is-subtree-p (&optional txt)
  "Check if the current kill is an outline subtree, or a set of trees.
Returns nil if kill does not start with a headline, or if the first
headline level is not the largest headline level in the tree.
So this will actually accept several entries of equal levels as well,
which is OK for `org-paste-subtree'.
If optional TXT is given, check this string instead of the current kill."
  (let* ((kill (or txt (ignore-errors (current-kill 0))))
	 (re (org-get-limited-outline-regexp))
	 (^re (concat "^" re))
	 (start-level (and kill
			   (string-match
			    (concat "\\`\\([ \t\n\r]*?\n\\)?\\(" re "\\)")
			    kill)
			   (- (match-end 2) (match-beginning 2) 1)))
	 (start (1+ (or (match-beginning 2) -1))))
    (if (not start-level)
	(progn
	  nil)  ;; does not even start with a heading
      (catch 'exit
	(while (setq start (string-match ^re kill (1+ start)))
	  (when (< (- (match-end 0) (match-beginning 0) 1) start-level)
	    (throw 'exit nil)))
	t))))

(defvar org-markers-to-move nil
  "Markers that should be moved with a cut-and-paste operation.
Those markers are stored together with their positions relative to
the start of the region.")

(defvar org-log-note-marker) ; defined later
(defun org-save-markers-in-region (beg end)
  "Check markers in region.
If these markers are between BEG and END, record their position relative
to BEG, so that after moving the block of text, we can put the markers back
into place.
This function gets called just before an entry or tree gets cut from the
buffer.  After re-insertion, `org-reinstall-markers-in-region' must be
called immediately, to move the markers with the entries."
  (setq org-markers-to-move nil)
  (org-check-and-save-marker org-log-note-marker beg end)
  (when (featurep 'org-clock)
    (org-clock-save-markers-for-cut-and-paste beg end))
  (when (featurep 'org-agenda)
    (org-agenda-save-markers-for-cut-and-paste beg end)))

(defun org-check-and-save-marker (marker beg end)
  "Check if MARKER is between BEG and END.
If yes, remember the marker and the distance to BEG."
  (when (and (marker-buffer marker)
	     (or (equal (marker-buffer marker) (current-buffer))
                 (equal (marker-buffer marker) (buffer-base-buffer (current-buffer))))
	     (>= marker beg) (< marker end))
    (push (cons marker (- marker beg)) org-markers-to-move)))

(defun org-reinstall-markers-in-region (beg)
  "Move all remembered markers to their position relative to BEG."
  (dolist (x org-markers-to-move)
    (move-marker (car x) (+ beg (cdr x))))
  (setq org-markers-to-move nil))

(defun org-narrow-to-subtree (&optional element)
  "Narrow buffer to the current subtree.
Use the command `\\[widen]' to see the whole buffer again.
With optional argument ELEMENT narrow to subtree around ELEMENT."
  (interactive nil org-mode)
  (let* ((heading
          (org-element-lineage
           (or element (org-element-at-point))
           'headline 'with-self))
         (begin (org-element-begin heading))
         (end (org-element-end heading)))
    (if (and heading end
             ;; Preserve historical behavior throwing an error when
             ;; current heading starts before active narrowing.
             (<= (point-min) begin))
        (narrow-to-region
         begin
         ;; Preserve historical behavior not extending the active
         ;; narrowing when the subtree extends beyond it.
         (min (point-max)
              (if (= end (point-max))
                  end (1- end))))
      (signal 'outline-before-first-heading nil))))

(defun org-toggle-narrow-to-subtree ()
  "Narrow to the subtree at point or widen a narrowed buffer.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive nil org-mode)
  (if (buffer-narrowed-p)
      (progn (widen) (message "Buffer widen"))
    (org-narrow-to-subtree)
    (message "Buffer narrowed to current subtree")))

(defun org-narrow-to-block ()
  "Narrow buffer to the current block.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive nil org-mode)
  (let* ((case-fold-search t)
         (element (org-element-at-point)))
    (if (string-match-p "block" (symbol-name (org-element-type element)))
        (org-narrow-to-element)
      (user-error "Not in a block"))))

(defun org-clone-subtree-with-time-shift (n &optional shift)
  "Clone the task (subtree) at point N times.
The clones will be inserted as siblings.

In interactive use, the user will be prompted for the number of
clones to be produced.  If the entry has a timestamp, the user
will also be prompted for a time shift, which may be a repeater
as used in time stamps, for example `+3d'.  To disable this,
you can call the function with a universal prefix argument.

When a valid repeater is given and the entry contains any time
stamps, the clones will become a sequence in time, with time
stamps in the subtree shifted for each clone produced.  If SHIFT
is nil or the empty string, time stamps will be left alone.  The
ID property of the original subtree is removed.

In each clone, all the CLOCK entries will be removed.  This
prevents Org from considering that the clocked times overlap.

If the original subtree did contain time stamps with a repeater,
the following will happen:
- the repeater will be removed in each clone
- an additional clone will be produced, with the current, unshifted
  date(s) in the entry.
- the original entry will be placed *after* all the clones, with
  repeater intact.
- the start days in the repeater in the original entry will be shifted
  to past the last clone.
In this way you can spell out a number of instances of a repeating task,
and still retain the repeater to cover future instances of the task.

As described above, N+1 clones are produced when the original
subtree has a repeater.  Setting N to 0, then, can be used to
remove the repeater from a subtree and create a shifted clone
with the original repeater."
  (interactive "nNumber of clones to produce: " org-mode)
  (unless (wholenump n) (user-error "Invalid number of replications %s" n))
  (when (org-before-first-heading-p) (user-error "No subtree to clone"))
  (let* ((beg (save-excursion (org-back-to-heading t) (point)))
	 (end-of-tree (save-excursion (org-end-of-subtree t t) (point)))
	 (shift
	  (or shift
	      (if (and (not (equal current-prefix-arg '(4)))
		       (save-excursion
			 (goto-char beg)
			 (re-search-forward org-ts-regexp-both end-of-tree t)))
		  (read-from-minibuffer
		   "Date shift per clone (e.g. +1w, empty to copy unchanged): ")
		"")))			;No time shift
	 (doshift
	  (and (org-string-nw-p shift)
	       (or (string-match "\\`[ \t]*\\([+-]?[0-9]+\\)\\([hdwmy]\\)[ \t]*\\'"
				 shift)
		   (user-error "Invalid shift specification %s" shift)))))
    (goto-char end-of-tree)
    (unless (bolp) (insert "\n"))
    (let* ((end (point))
	   (template (buffer-substring beg end))
	   (shift-n (and doshift (string-to-number (match-string 1 shift))))
	   (shift-what (pcase (and doshift (match-string 2 shift))
			 (`nil nil)
			 ("h" 'hour)
			 ("d" 'day)
			 ("w" (setq shift-n (* 7 shift-n)) 'day)
			 ("m" 'month)
			 ("y" 'year)
			 (_ (error "Unsupported time unit"))))
	   (nmin 1)
	   (nmax n)
	   (n-no-remove -1)
	   (org-id-overriding-file-name (buffer-file-name (buffer-base-buffer)))
	   (idprop (org-entry-get beg "ID")))
      (when (and doshift
		 (string-match-p "<[^<>\n]+ [.+]?\\+[0-9]+[hdwmy][^<>\n]*>"
				 template))
	(delete-region beg end)
	(setq end beg)
	(setq nmin 0)
	(setq nmax (1+ nmax))
	(setq n-no-remove nmax))
      (goto-char end)
      (cl-loop for n from nmin to nmax do
	       (insert
		;; Prepare clone.
		(with-temp-buffer
		  (insert template)
		  (org-mode)
		  (goto-char (point-min))
		  (org-fold-show-subtree)
		  (and idprop (if org-clone-delete-id
				  (org-entry-delete nil "ID")
				(org-id-get-create t)))
		  (unless (= n 0)
		    (while (re-search-forward org-clock-line-re nil t)
		      (delete-region (line-beginning-position)
				     (line-beginning-position 2)))
		    (goto-char (point-min))
		    (while (re-search-forward org-drawer-regexp nil t)
		      (org-remove-empty-drawer-at (point))))
		  (goto-char (point-min))
		  (when doshift
		    (while (re-search-forward org-ts-regexp-both nil t)
		      (org-timestamp-change (* n shift-n) shift-what))
		    (unless (= n n-no-remove)
		      (goto-char (point-min))
		      (while (re-search-forward org-ts-regexp nil t)
			(save-excursion
			  (goto-char (match-beginning 0))
			  (when (looking-at "<[^<>\n]+\\( +[.+]?\\+[0-9]+[hdwmy]\\)")
			    (delete-region (match-beginning 1) (match-end 1)))))))
		  (buffer-string)))))
    (goto-char beg)))

;;; Outline path

(defvar org-outline-path-cache nil
  "Alist between buffer positions and outline paths.
It value is an alist (POSITION . PATH) where POSITION is the
buffer position at the beginning of an entry and PATH is a list
of strings describing the outline path for that entry, in reverse
order.")

(defun org--get-outline-path-1 (&optional use-cache)
  "Return outline path to current headline.

Outline path is a list of strings, in reverse order.  When
optional argument USE-CACHE is non-nil, make use of a cache.  See
`org-get-outline-path' for details.

Assume buffer is widened and point is on a headline."
  (or (and use-cache (cdr (assq (point) org-outline-path-cache)))
      (let ((p (point))
	    (heading (let ((case-fold-search nil))
		       (looking-at org-complex-heading-regexp)
		       (if (not (match-end 4)) ""
			 ;; Remove statistics cookies.
			 (org-trim
			  (org-link-display-format
			   (replace-regexp-in-string
			    "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
			    (match-string-no-properties 4))))))))
        (when (org-element-property :commentedp (org-element-at-point))
          (setq heading (replace-regexp-in-string (format "^%s[ \t]*" org-comment-string) "" heading)))
	(if (org-up-heading-safe)
	    (let ((path (cons heading (org--get-outline-path-1 use-cache))))
	      (when use-cache
		(push (cons p path) org-outline-path-cache))
	      path)
	  ;; This is a new root node.  Since we assume we are moving
	  ;; forward, we can drop previous cache so as to limit number
	  ;; of associations there.
	  (let ((path (list heading)))
	    (when use-cache (setq org-outline-path-cache (list (cons p path))))
	    path)))))

(defun org-get-outline-path (&optional with-self use-cache)
  "Return the outline path to the current entry.

An outline path is a list of ancestors for current headline, as
a list of strings.  Statistics cookies are removed and links are
replaced with their description, if any, or their path otherwise.

When optional argument WITH-SELF is non-nil, the path also
includes the current headline.

When optional argument USE-CACHE is non-nil, cache outline paths
between calls to this function so as to avoid backtracking.  This
argument is useful when planning to find more than one outline
path in the same document.  In that case, there are two
conditions to satisfy:
  - `org-outline-path-cache' is set to nil before starting the
    process;
  - outline paths are computed by increasing buffer positions."
  (org-with-wide-buffer
   (and (or (and with-self (org-back-to-heading t))
	    (org-up-heading-safe))
	(reverse (org--get-outline-path-1 use-cache)))))

(defun org-format-outline-path (path &optional width prefix separator)
  "Format the outline path PATH for display.
WIDTH is the maximum number of characters that is available.
PREFIX is a prefix to be included in the returned string,
such as the file name.
SEPARATOR is inserted between the different parts of the path,
the default is \"/\"."
  (setq width (or width 79))
  (setq path (delq nil path))
  (unless (> width 0)
    (user-error "Argument `width' must be positive"))
  (setq separator (or separator "/"))
  (let* ((org-odd-levels-only nil)
	 (fpath (concat
		 prefix (and prefix path separator)
		 (mapconcat
		  (lambda (s) (replace-regexp-in-string "[ \t]+\\'" "" s))
		  (cl-loop for head in path
			   for n from 0
			   collect (org-add-props
				       head nil 'face
				       (nth (% n org-n-level-faces) org-level-faces)))
		  separator))))
    (when (> (length fpath) width)
      (if (< width 7)
	  ;; It's unlikely that `width' will be this small, but don't
	  ;; waste characters by adding ".." if it is.
	  (setq fpath (substring fpath 0 width))
	(setf (substring fpath (- width 2)) "..")))
    fpath))

(defun org-get-title (&optional buffer-or-file)
  "Collect title from the provided `org-mode' BUFFER-OR-FILE.

Returns nil if there are no #+TITLE property."
  (let ((buffer (cond ((bufferp buffer-or-file) buffer-or-file)
                      ((stringp buffer-or-file) (find-file-noselect
                                                 buffer-or-file))
                      (t (current-buffer)))))
    (with-current-buffer buffer
      (org-macro-initialize-templates)
      (let ((title (assoc-default "title" org-macro-templates)))
        (unless (string= "" title)
          title)))))

(defun org-display-outline-path (&optional file-or-title current separator just-return-string)
  "Display the current outline path in the echo area.

If FILE-OR-TITLE is `title', prepend outline with file title.  If
it is non-nil or title is not present in document, prepend
outline path with the file name.
If CURRENT is non-nil, append the current heading to the output.
SEPARATOR is passed through to `org-format-outline-path'.  It separates
the different parts of the path and defaults to \"/\".
If JUST-RETURN-STRING is non-nil, return a string, don't display a message."
  (interactive "P" org-mode)
  (let* (case-fold-search
	 (bfn (buffer-file-name (buffer-base-buffer)))
         (prefix (when (eq file-or-title 'title)
                   (or (org-get-title) (file-name-nondirectory bfn))))
	 (path (and (derived-mode-p 'org-mode) (org-get-outline-path)))
	 res)
    (when current (setq path (append path
				     (save-excursion
				       (org-back-to-heading-or-point-min t)
				       (when (looking-at org-complex-heading-regexp)
					 (list (match-string 4)))))))
    (setq res
	  (org-format-outline-path
	   path
	   (1- (frame-width))
	   prefix
	   separator))
    (add-face-text-property 0 (length res)
			    `(:height ,(face-attribute 'default :height))
			    nil res)
    (if just-return-string
	res
      (org-unlogged-message "%s" res))))

;;; Outline Sorting

(defun org-sort (&optional with-case)
  "Call `org-sort-entries', `org-table-sort-lines' or `org-sort-list'.
Optional argument WITH-CASE means sort case-sensitively."
  (interactive "P" org-mode)
  (org-call-with-arg
   (cond ((org-at-table-p) #'org-table-sort-lines)
	 ((org-at-item-p) #'org-sort-list)
	 (t #'org-sort-entries))
   with-case))

(defun org-sort-remove-invisible (s)
  "Remove emphasis markers and any invisible property from string S.
Assume S may contain only objects."
  ;; org-element-interpret-data clears any text property, including
  ;; invisible part.
  (org-element-interpret-data
   (let ((tree (org-element-parse-secondary-string
                s (org-element-restriction 'paragraph))))
     (org-element-map tree '(bold code italic link strike-through underline verbatim)
       (lambda (o)
         (pcase (org-element-type o)
           ;; Terminal object.  Replace it with its value.
           ((or `code `verbatim)
            (let ((new (org-element-property :value o)))
              (org-element-insert-before new o)
              (org-element-put-property
               new :post-blank (org-element-post-blank o))))
           ;; Non-terminal objects.  Splice contents.
           (type
            (let ((contents
                   (or (org-element-contents o)
                       (and (eq type 'link)
                            (list (org-element-property :raw-link o)))))
                  (c nil))
              (while contents
                (setq c (pop contents))
                (org-element-insert-before c o))
              (org-element-put-property
               c :post-blank (org-element-post-blank o)))))
         (org-element-extract o)))
     ;; Return modified tree.
     tree)))

(defvar org-after-sorting-entries-or-items-hook nil
  "Hook that is run after a bunch of entries or items have been sorted.
When children are sorted, the cursor is in the parent line when this
hook gets called.  When a region or a plain list is sorted, the cursor
will be in the first entry of the sorted region/list.")

(defun org-sort-entries
    (&optional with-case sorting-type getkey-func compare-func property
	       interactive?)
  "Sort entries on a certain level of an outline tree.
If there is an active region, the entries in the region are sorted.
Else, if the cursor is before the first entry, sort the top-level items.
Else, the children of the entry at point are sorted.

Sorting can be alphabetically, numerically, by date/time as given by
a time stamp, by a property, by priority order, or by a custom function.

The command prompts for the sorting type unless it has been given to the
function through the SORTING-TYPE argument, which needs to be a character,
\(?n ?N ?a ?A ?t ?T ?s ?S ?d ?D ?p ?P ?o ?O ?r ?R ?f ?F ?k ?K).  Here is
the precise meaning of each character:

a   Alphabetically, ignoring the TODO keyword and the priority, if any.
c   By creation time, which is assumed to be the first inactive time stamp
    at the beginning of a line.
d   By deadline date/time.
k   By clocking time.
n   Numerically, by converting the beginning of the entry/item to a number.
o   By order of TODO keywords.
p   By priority according to the cookie.
r   By the value of a property.
s   By scheduled date/time.
t   By date/time, either the first active time stamp in the entry, or, if
    none exist, by the first inactive one.

Capital letters will reverse the sort order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies a function to be
called with point at the beginning of the record.  It must return a
value that is compatible with COMPARE-FUNC, the function used to
compare entries.

Comparing entries ignores case by default.  However, with an optional argument
WITH-CASE, the sorting considers case as well.

Sorting is done against the visible part of the headlines, it ignores hidden
links.

When sorting is done, call `org-after-sorting-entries-or-items-hook'.

A non-nil value for INTERACTIVE? is used to signal that this
function is being called interactively."
  (interactive (list current-prefix-arg nil nil nil nil t) org-mode)
  (let ((case-func (if with-case 'identity 'downcase))
        start beg end stars re re2
        txt what tmp)
    ;; Find beginning and end of region to sort
    (cond
     ((org-region-active-p)
      (setq start (region-beginning)
            end (region-end))
      ;; we will sort the region
      ;; Limit the region to full headings.
      (goto-char start)
      ;; Move to beginning of heading.
      ;; If we are inside heading, move to next.
      ;; If we are on heading, move to its begin position.
      (if (org-at-heading-p)
          (forward-line 0)
        (outline-next-heading))
      (setq start (point))
      ;; Extend region end beyond the last subtree.
      (goto-char end)
      (org-end-of-subtree nil t)
      (setq end (point)
            what "region")
      (goto-char start))
     ((or (org-at-heading-p)
          (ignore-errors (progn (org-back-to-heading (not interactive?)) t)))
      ;; we will sort the children of the current headline
      (org-back-to-heading (not interactive?))
      (setq start (point)
	    end (progn (org-end-of-subtree t t)
		       (or (bolp) (insert "\n"))
		       (when (>= (org-back-over-empty-lines) 1)
			 (forward-line 1))
		       (point))
	    what "children")
      (goto-char start)
      (org-fold-show-subtree)
      (outline-next-heading))
     (t
      ;; we will sort the top-level entries in this file
      (goto-char (point-min))
      (or (org-at-heading-p) (outline-next-heading))
      (setq start (point))
      (goto-char (point-max))
      (forward-line 0)
      (when (looking-at ".*?\\S-")
	;; File ends in a non-white line
	(end-of-line 1)
	(insert "\n"))
      (setq end (point-max))
      (setq what "top-level")
      (goto-char start)
      (org-fold-show-all '(headings drawers blocks))))

    (setq beg (point))
    (when (>= beg end) (goto-char start) (user-error "Nothing to sort"))

    (looking-at "\\(\\*+\\)")
    (setq stars (match-string 1)
	  re (concat "^" (regexp-quote stars) " +")
	  re2 (concat "^" (regexp-quote (substring stars 0 -1)) "[ \t\n]")
	  txt (buffer-substring beg end))
    (unless (equal (substring txt -1) "\n") (setq txt (concat txt "\n")))
    (when (and (not (equal stars "*")) (string-match re2 txt))
      (user-error "Region to sort contains a level above the first entry"))

    (unless sorting-type
      (message
       "Sort %s: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder  [f]unc
               [t]ime [s]cheduled  [d]eadline  [c]reated  cloc[k]ing
               A/N/P/R/O/F/T/S/D/C/K means reversed:"
       what)
      (setq sorting-type (read-char-exclusive)))

    (unless getkey-func
      (and (= (downcase sorting-type) ?f)
	   (setq getkey-func
		 (or (and interactive?
			  (org-read-function
			   "Function for extracting keys: "))
		     (error "Missing key extractor")))))

    (and (= (downcase sorting-type) ?r)
	 (not property)
	 (setq property
	       (completing-read "Property: "
				(mapcar #'list (org-buffer-property-keys t))
				nil t)))

    (when (member sorting-type '(?k ?K)) (org-clock-sum))
    (message "Sorting entries...")

    (save-restriction
      (narrow-to-region start end)
      ;; No trailing newline - add one to avoid
      ;; * heading
      ;; text* another heading
      (save-excursion
        (goto-char end)
        (unless (bolp) (insert "\n")))
      (let ((restore-clock?
	     ;; The clock marker is lost when using `sort-subr'; mark
	     ;; the clock with temporary `:org-clock-marker-backup'
	     ;; text property.
	     (when (and (eq (org-clocking-buffer) (current-buffer))
			(<= start (marker-position org-clock-marker))
			(>= end (marker-position org-clock-marker)))
	       (with-silent-modifications
		 (put-text-property (1- org-clock-marker) org-clock-marker
				    :org-clock-marker-backup t))
	       t))
	    (dcst (downcase sorting-type))
	    (case-fold-search nil)
	    (now (current-time)))
        (org-preserve-local-variables
	 (sort-subr
	  (/= dcst sorting-type)
	  ;; This function moves to the beginning character of the
	  ;; "record" to be sorted.
	  (lambda nil
	    (if (re-search-forward re nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max))))
	  ;; This function moves to the last character of the "record" being
	  ;; sorted.
	  (lambda nil
	    (save-match-data
	      (condition-case nil
		  (outline-forward-same-level 1)
		(error
		 (goto-char (point-max))))))
	  ;; This function returns the value that gets sorted against.
	  (lambda ()
	    (cond
	     ((= dcst ?n)
	      (string-to-number
	       (org-sort-remove-invisible (org-get-heading t t t t))))
	     ((= dcst ?a)
	      (funcall case-func
		       (org-sort-remove-invisible (org-get-heading t t t t))))
	     ((= dcst ?k)
	      (or (get-text-property (point) :org-clock-minutes) 0))
	     ((= dcst ?t)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (or (re-search-forward org-ts-regexp end t)
			(re-search-forward org-ts-regexp-both end t))
		    (org-time-string-to-seconds (match-string 0))
		  (float-time now))))
	     ((= dcst ?c)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward
		     (concat "^[ \t]*\\[" org-ts-regexp1 "\\]")
		     end t)
		    (org-time-string-to-seconds (match-string 0))
		  (float-time now))))
	     ((= dcst ?s)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward org-scheduled-time-regexp end t)
		    (org-time-string-to-seconds (match-string 1))
		  (float-time now))))
	     ((= dcst ?d)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward org-deadline-time-regexp end t)
		    (org-time-string-to-seconds (match-string 1))
		  (float-time now))))
	     ((= dcst ?p)
              (if (re-search-forward org-priority-regexp (line-end-position) t)
                  (org-priority-to-value (match-string 2))
		org-priority-default))
	     ((= dcst ?r)
	      (or (org-entry-get nil property) ""))
	     ((= dcst ?o)
	      (when (looking-at org-complex-heading-regexp)
		(let* ((m (match-string 2))
		       (s (if (member m org-done-keywords) '- '+)))
		  (- 99 (funcall s (length (member m org-todo-keywords-1)))))))
	     ((= dcst ?f)
	      (if getkey-func
		  (progn
		    (setq tmp (funcall getkey-func))
		    (when (stringp tmp) (setq tmp (funcall case-func tmp)))
		    tmp)
		(error "Invalid key function `%s'" getkey-func)))
	     (t (error "Invalid sorting type `%c'" sorting-type))))
	  nil
	  (cond
	   ((= dcst ?a) #'org-string<)
	   ((= dcst ?f)
	    (or compare-func
		(and interactive?
		     (org-read-function
		      (concat "Function for comparing keys "
			      "(empty for default `sort-subr' predicate): ")
		      'allow-empty))))
	   ((member dcst '(?p ?t ?s ?d ?c ?k)) '<))))
	(org-cycle-hide-drawers 'all)
	(when restore-clock?
	  (move-marker org-clock-marker
		       (1+ (next-single-property-change
			    start :org-clock-marker-backup)))
	  (remove-text-properties (1- org-clock-marker) org-clock-marker
				  '(:org-clock-marker-backup t)))))
    (run-hooks 'org-after-sorting-entries-or-items-hook)
    (message "Sorting entries...done")))

(defun org-contextualize-keys (alist contexts)
  "Return valid elements in ALIST depending on CONTEXTS.

`org-agenda-custom-commands' or `org-capture-templates' are the
values used for ALIST, and `org-agenda-custom-commands-contexts'
or `org-capture-templates-contexts' are the associated contexts
definitions."
  (let ((contexts
	 ;; normalize contexts
	 (mapcar
	  (lambda(c) (cond ((listp (cadr c))
			    (list (car c) (car c) (nth 1 c)))
			   ((string= "" (cadr c))
			    (list (car c) (car c) (nth 2 c)))
			   (t c)))
          contexts))
	(a alist) r s)
    ;; loop over all commands or templates
    (dolist (c a)
      (let (vrules repl)
	(cond
	 ((not (assoc (car c) contexts))
	  (push c r))
	 ((and (assoc (car c) contexts)
	       (setq vrules (org-contextualize-validate-key
			     (car c) contexts)))
	  (mapc (lambda (vr)
		  (unless (equal (car vr) (cadr vr))
		    (setq repl vr)))
                vrules)
	  (if (not repl) (push c r)
	    (push (cadr repl) s)
	    (push
	     (cons (car c)
		   (cdr (or (assoc (cadr repl) alist)
			    (error "Undefined key `%s' as contextual replacement for `%s'"
				   (cadr repl) (car c)))))
	     r))))))
    ;; Return limited ALIST, possibly with keys modified, and deduplicated
    (delq
     nil
     (delete-dups
      (mapcar (lambda (x)
		(let ((tpl (car x)))
		  (unless (delq
			   nil
			   (mapcar (lambda (y)
				     (equal y tpl))
				   s))
                    x)))
	      (reverse r))))))

(defun org-contextualize-validate-key (key contexts)
  "Check CONTEXTS for agenda or capture KEY."
  (let (res)
    (dolist (r contexts)
      (dolist (rr (car (last r)))
	(when
	    (and (equal key (car r))
		 (if (functionp rr) (funcall rr)
		   (or (and (eq (car rr) 'in-file)
			    (buffer-file-name)
			    (string-match (cdr rr) (buffer-file-name)))
		       (and (eq (car rr) 'in-mode)
			    (string-match (cdr rr) (symbol-name major-mode)))
		       (and (eq (car rr) 'in-buffer)
			    (string-match (cdr rr) (buffer-name)))
		       (when (and (eq (car rr) 'not-in-file)
				  (buffer-file-name))
			 (not (string-match (cdr rr) (buffer-file-name))))
		       (when (eq (car rr) 'not-in-mode)
			 (not (string-match (cdr rr) (symbol-name major-mode))))
		       (when (eq (car rr) 'not-in-buffer)
			 (not (string-match (cdr rr) (buffer-name)))))))
	  (push r res))))
    (delete-dups (delq nil res))))

;; Defined to provide a value for defcustom, since there is no
;; string-collate-greaterp in Emacs.
(defun org-string-collate-greaterp (s1 s2)
  "Return non-nil if S1 is greater than S2 in collation order."
  (not (string-collate-lessp s1 s2)))

;;;###autoload
(defun org-run-like-in-org-mode (cmd)
  "Run a command, pretending that the current buffer is in Org mode.
This will temporarily bind local variables that are typically bound in
Org mode to the values they have in Org mode, and then interactively
call CMD."
  (org-load-modules-maybe)
  (let (vars vals)
    (dolist (var (org-get-local-variables))
      (when (or (not (boundp (car var)))
		(eq (symbol-value (car var))
		    (default-value (car var))))
	(push (car var) vars)
	(push (cadr var) vals)))
    (cl-progv vars vals
      (call-interactively cmd))))

(defun org-get-archive-location (&optional epom)
  "Get archive location applying to EPOM or point.
EPOM is an element, marker, or buffer position."
  (or (org-entry-get epom "ARCHIVE" 'inherit)
      org-archive-location))

(defun org-get-category (&optional pos _)
  "Get the category applying to position POS.
Return \"???\" when no category is set.

This function may modify the match data."
  ;; Sync cache.
  (cond
   ((org-entry-get-with-inheritance
     "CATEGORY" nil (or pos (point))))
   ((null org-category)
    (when (org-with-base-buffer nil
            buffer-file-name)
      (file-name-sans-extension
       (file-name-nondirectory
        (org-with-base-buffer nil
          buffer-file-name)))))
   ((symbolp org-category) (symbol-name org-category))
   ((stringp org-category) org-category)
   (t "???")))

;;; Refresh properties

(defun org-refresh-properties (dprop tprop)
  "Refresh buffer text properties.
DPROP is the drawer property and TPROP is either the
corresponding text property to set, or an alist with each element
being a text property (as a symbol) and a function to apply to
the value of the drawer property."
  (let* ((case-fold-search t)
	 (inhibit-read-only t)
	 (inherit? (org-property-inherit-p dprop))
	 (property-re (org-re-property (concat (regexp-quote dprop) "\\+?") t))
	 (global-or-keyword (and inherit?
				 (org--property-global-or-keyword-value dprop nil))))
    (with-silent-modifications
      (org-with-point-at 1
	;; Set global and keyword based values to the whole buffer.
	(when global-or-keyword
	  (put-text-property (point-min) (point-max) tprop global-or-keyword))
	;; Set values based on property-drawers throughout the document.
	(while (re-search-forward property-re nil t)
	  (when (org-at-property-p)
	    (org-refresh-property tprop (org-entry-get (point) dprop) inherit?))
	  (outline-next-heading))))))

(defun org-refresh-property (tprop p &optional inherit)
  "Refresh the buffer text property TPROP from the drawer property P.

The refresh happens only for the current entry, or the whole
sub-tree if optional argument INHERIT is non-nil.

If point is before first headline, the function applies to the
part before the first headline.  In that particular case, when
optional argument INHERIT is non-nil, it refreshes properties for
the whole buffer."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (let ((start (point))
	  (end (save-excursion
		 (cond ((and inherit (org-before-first-heading-p))
			(point-max))
		       (inherit
			(org-end-of-subtree t t))
		       ((outline-next-heading))
		       ((point-max))))))
      (with-silent-modifications
	(if (symbolp tprop)
	    ;; TPROP is a text property symbol.
	    (put-text-property start end tprop p)
	  ;; TPROP is an alist with (property . function) elements.
	  (pcase-dolist (`(,prop . ,f) tprop)
	    (put-text-property start end prop (funcall f p))))))))

(defun org-refresh-category-properties ()
  "Refresh category text properties in the buffer."
  (let ((case-fold-search t)
	(inhibit-read-only t)
	(default-category
	 (cond ((null org-category)
		(if buffer-file-name
		    (file-name-sans-extension
		     (file-name-nondirectory buffer-file-name))
		  "???"))
	       ((symbolp org-category) (symbol-name org-category))
	       (t org-category))))
    (let ((category (catch 'buffer-category
                      (org-with-wide-buffer
	               (goto-char (point-max))
	               (while (re-search-backward "^[ \t]*#\\+CATEGORY:" (point-min) t)
	                 (let ((element (org-element-at-point-no-context)))
	                   (when (org-element-type-p element 'keyword)
		             (throw 'buffer-category
		                    (org-element-property :value element))))))
	              default-category)))
      (with-silent-modifications
        (org-with-wide-buffer
         ;; Set buffer-wide property from keyword.  Search last #+CATEGORY
         ;; keyword.  If none is found, fall-back to `org-category' or
         ;; buffer file name, or set it by the document property drawer.
         (put-text-property (point-min) (point-max)
                            'org-category category)
         ;; Set categories from the document property drawer or
         ;; property drawers in the outline.  If category is found in
         ;; the property drawer for the whole buffer that value
         ;; overrides the keyword-based value set above.
         (goto-char (point-min))
         (let ((regexp (org-re-property "CATEGORY")))
           (while (re-search-forward regexp nil t)
             (let ((value (match-string-no-properties 3)))
               (when (org-at-property-p)
                 (put-text-property
                  (save-excursion (org-back-to-heading-or-point-min t))
                  (save-excursion (if (org-before-first-heading-p)
                                      (point-max)
                                    (org-end-of-subtree t t)))
                  'org-category
                  value))))))))))

(defun org-refresh-stats-properties ()
  "Refresh stats text properties in the buffer."
  (with-silent-modifications
    (org-with-point-at 1
      (let ((regexp (concat org-outline-regexp-bol
			    ".*\\[\\([0-9]*\\)\\(?:%\\|/\\([0-9]*\\)\\)\\]")))
	(while (re-search-forward regexp nil t)
	  (let* ((numerator (string-to-number (match-string 1)))
		 (denominator (and (match-end 2)
				   (string-to-number (match-string 2))))
		 (stats (cond ((not denominator) numerator) ;percent
			      ((= denominator 0) 0)
			      (t (/ (* numerator 100) denominator)))))
	    (put-text-property (line-beginning-position)
                               (progn (org-end-of-subtree t t) (point))
			       'org-stats stats)))))))

(defun org-refresh-effort-properties ()
  "Refresh effort properties."
  (org-refresh-properties
   org-effort-property
   '((effort . identity)
     (effort-minutes . org-duration-to-minutes))))

(defun org-find-file-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e" org-mode)
  (mouse-set-point ev)
  (org-open-at-point 'in-emacs))

(defun org-open-at-mouse (ev)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e" org-mode)
  (mouse-set-point ev)
  (when (eq major-mode 'org-agenda-mode)
    ;; FIXME: `org-link-abbrev-alist-local' is obsolete
    ;; but we cannot replace it with the Org parser.
    (org-agenda-copy-local-variable 'org-link-abbrev-alist-local))
  ;; FIXME: This feature is actually unreliable - if we are in non-Org
  ;; buffer and the link happens to be inside what Org parser
  ;; recognizes as verbarim (for exampe, src block),
  ;; `org-open-at-point' will do nothing.
  ;; We might have used `org-open-at-point-global' instead, but it is
  ;; not exactly the same. For example, it will have no way to open
  ;; link abbreviations. So, suppressing parser complains about
  ;; non-Org buffer to keep the feature working at least to the extent
  ;; it did before.
  (require 'warnings) ; Emacs <30
  (defvar warning-suppress-types) ; warnings.el
  (let ((warning-suppress-types
         (cons '(org-element org-element-parser)
               warning-suppress-types)))
    ;; FIXME: Suppress warning in Emacs <30
    ;; (ignore warning-suppress-types)
    (org-open-at-point)))

(defvar org-window-config-before-follow-link nil
  "The window configuration before following a link.
This is saved in case the need arises to restore it.")

;;;###autoload
(defun org-open-at-point-global (&optional arg)
  "Follow a link or a timestamp like Org mode does.
Pass ARG to `org-link-open-to-string'.
Also follow links and emails as seen by `thing-at-point'.
This command can be called in any mode to follow an external
link or a timestamp that has Org mode syntax.  Its behavior
is undefined when called on internal links like fuzzy links.
Raise a user error when there is nothing to follow."
  (interactive "P")
  (let ((tap-url (thing-at-point 'url))
	(tap-email (thing-at-point 'email)))
    (cond ((org-in-regexp
            org-link-any-re
            (let ((origin (point)))
              (max
               (save-excursion
                 (backward-paragraph)
                 (count-lines (point) origin))
               (save-excursion
                 (forward-paragraph)
                 (count-lines origin (point))))))
	   (org-link-open-from-string (match-string-no-properties 0) arg))
	  ((or (org-in-regexp org-ts-regexp-both nil t)
	       (org-in-regexp org-tsr-regexp-both nil t))
	   (org-follow-timestamp-link))
	  (tap-url (org-link-open-from-string tap-url arg))
	  (tap-email (org-link-open-from-string
		      (concat "mailto:" tap-email)
                      arg))
	  (t (user-error "No link found")))))

(defvar org-open-at-point-functions nil
  "Hook that is run when following a link at point.

Functions in this hook must return t if they identify and follow
a link at point.  If they don't find anything interesting at point,
they must return nil.")

(defun org-open-at-point (&optional arg)
  "Open thing at point.
The thing can be a link, citation, timestamp, footnote, src-block or
tags.

When point is on a link, follow it.  Normally, files will be opened by
an appropriate application (see `org-file-apps').  If the optional prefix
argument ARG is non-nil, Emacs will visit the file.  With a double
prefix argument, try to open outside of Emacs, in the application the
system uses for this file type.

When point is on a timestamp, open the agenda at the day
specified.

When point is a footnote definition, move to the first reference
found.  If it is on a reference, move to the associated
definition.

When point is on a src-block or inline src-block, open its result.

When point is on a citation, follow it.

When point is on a headline, display a list of every link in the
entry, so it is possible to pick one, or all, of them.  If point
is on a tag, call `org-tags-view' instead.

On top of syntactically correct links, this function also tries
to open links and timestamps in comments, node properties, and
keywords if point is on something looking like a timestamp or
a link."
  (interactive "P" org-mode)
  (org-load-modules-maybe)
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
    (let* ((context
	    ;; Only consider supported types, even if they are not the
	    ;; closest one.
	    (org-element-lineage
	     (org-element-context)
	     '(citation citation-reference clock comment comment-block
                        footnote-definition footnote-reference headline
                        inline-src-block inlinetask keyword link node-property
                        planning src-block timestamp)
	     t))
	   (type (org-element-type context))
	   (value (org-element-property :value context)))
      (cond
       ((not type) (user-error "No link found"))
       ;; No valid link at point.  For convenience, look if something
       ;; looks like a link under point in some specific places.
       ((memq type '(comment comment-block node-property keyword))
	(call-interactively #'org-open-at-point-global))
       ;; On a headline or an inlinetask, but not on a timestamp,
       ;; a link, a footnote reference or a citation.
       ((memq type '(headline inlinetask))
	(org-match-line org-complex-heading-regexp)
	(let ((tags-beg (match-beginning 5))
	      (tags-end (match-end 5)))
	  (if (and tags-beg (>= (point) tags-beg) (< (point) tags-end))
	      ;; On tags.
	      (org-tags-view
	       arg
	       (save-excursion
		 (let* ((beg-tag (or (search-backward ":" tags-beg 'at-limit) (point)))
			(end-tag (search-forward ":" tags-end nil 2)))
		   (buffer-substring (1+ beg-tag) (1- end-tag)))))
	    ;; Not on tags.
	    (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
	      (`(nil . ,_)
	       (require 'org-attach)
	       (when (org-attach-dir)
		 (message "Opening attachment")
		 (if (equal arg '(4))
		     (org-attach-reveal-in-emacs)
		   (org-attach-reveal))))
	      (`(,links . ,links-end)
               (let ((link-marker (make-marker))
                     (last-moved-marker (point-marker)))
	         (dolist (link (if (stringp links) (list links) links))
		   (search-forward link nil links-end)
		   (goto-char (match-beginning 0))
                   (move-marker link-marker (point))
                   (save-excursion
		     (org-open-at-point arg)
                     (unless (equal (point-marker) link-marker)
                       (move-marker last-moved-marker (point-marker)))))
                 ;; If any of the links moved point in current buffer,
                 ;; move to the point corresponding to such latest link.
                 ;; Otherwise, restore the original point position.
                 (goto-char last-moved-marker)))))))
       ;; On a footnote reference or at definition's label.
       ((or (eq type 'footnote-reference)
	    (and (eq type 'footnote-definition)
		 (save-excursion
		   ;; Do not validate action when point is on the
		   ;; spaces right after the footnote label, in order
		   ;; to be on par with behavior on links.
		   (skip-chars-forward " \t")
		   (let ((begin
			  (org-element-contents-begin context)))
		     (if begin (< (point) begin)
		       (= (org-element-post-affiliated context)
			  (line-beginning-position)))))))
	(org-footnote-action))
       ;; On a planning line.  Check if we are really on a timestamp.
       ((and (eq type 'planning)
	     (org-in-regexp org-ts-regexp-both nil t))
	(org-follow-timestamp-link))
       ;; On a clock line, make sure point is on the timestamp
       ;; before opening it.
       ((and (eq type 'clock)
	     value
	     (>= (point) (org-element-begin value))
	     (<= (point) (org-element-end value)))
	(org-follow-timestamp-link))
       ((eq type 'src-block) (org-babel-open-src-block-result))
       ;; Do nothing on white spaces after an object.
       ((>= (point)
	    (save-excursion
	      (goto-char (org-element-end context))
	      (skip-chars-backward " \t")
	      (point)))
	(user-error "No link found"))
       ((eq type 'inline-src-block) (org-babel-open-src-block-result))
       ((eq type 'timestamp) (org-follow-timestamp-link))
       ((eq type 'link) (org-link-open context arg))
       ((memq type '(citation citation-reference)) (org-cite-follow context arg))
       (t (user-error "No link found")))))
  (run-hook-with-args 'org-follow-link-hook))

;;;###autoload
(defun org-offer-links-in-entry (buffer marker &optional nth zero)
  "Offer links in the current entry and return the selected link.
If there is only one link, return it.
If NTH is an integer, return the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, return it."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char marker)
     (let ((cnt ?0)
	   have-zero end links link c)
       (when (and (stringp zero) (string-match org-link-bracket-re zero))
	 (push (match-string 0 zero) links)
	 (setq cnt (1- cnt) have-zero t))
       (save-excursion
	 (org-back-to-heading t)
	 (setq end (save-excursion (outline-next-heading) (point)))
	 (while (re-search-forward org-link-any-re end t)
           ;; Only consider valid links or links openable via
           ;; `org-open-at-point'.
           (when (org-element-type-p
                  (save-match-data (org-element-context))
                  '(link comment comment-block node-property keyword))
	     (push (match-string 0) links)))
	 (setq links (org-uniquify (reverse links))))
       (cond
	((null links)
	 (message "No links"))
	((equal (length links) 1)
	 (setq link (car links)))
	((and (integerp nth) (>= (length links) (if have-zero (1+ nth) nth)))
	 (setq link (nth (if have-zero nth (1- nth)) links)))
	(t				; we have to select a link
	 (save-excursion
	   (save-window-excursion
             ;; We have no direct control over how
             ;; `with-output-to-temp-buffer' displays the buffer.  Try
             ;; to gain more space, making sure that only the Org
             ;; buffer and the *Select link* buffer are displayed for
             ;; the duration of selection.
	     (ignore-errors (delete-other-windows))
	     (with-output-to-temp-buffer "*Select Link*"
	       (dolist (l links)
		 (cond
		  ((not (string-match org-link-bracket-re l))
		   (princ (format "[%c]  %s\n" (cl-incf cnt)
				  (org-unbracket-string "<" ">" l))))
		  ((match-end 2)
		   (princ (format "[%c]  %s (%s)\n" (cl-incf cnt)
				  (match-string 2 l) (match-string 1 l))))
		  (t (princ (format "[%c]  %s\n" (cl-incf cnt)
				    (match-string 1 l)))))))
	     (org-fit-window-to-buffer (get-buffer-window "*Select Link*"))
	     (message "Select link to open, RET to open all:")
             (unwind-protect (setq c (read-char-exclusive))
               (and (get-buffer-window "*Select Link*" t)
                    (quit-window 'kill (get-buffer-window "*Select Link*" t)))
	       (and (get-buffer "*Select Link*") (kill-buffer "*Select Link*")))))
	 (when (equal c ?q) (user-error "Abort"))
	 (if (equal c ?\C-m)
	     (setq link links)
	   (setq nth (- c ?0))
	   (when have-zero (setq nth (1+ nth)))
	   (unless (and (integerp nth) (>= (length links) nth))
	     (user-error "Invalid link selection"))
	   (setq link (nth (1- nth) links)))))
       (cons link end)))))

(defun org--forward-sentence-thing-at-point-provider (&optional backward)
  "`forward-thing' provider function."
  (if backward
      (org-backward-sentence)
    (org-forward-sentence)))

;;; File search

(defun org-do-occur (regexp &optional cleanup)
  "Call the Emacs command `occur'.
If CLEANUP is non-nil, remove the printout of the regular expression
in the *Occur* buffer.  This is useful if the regex is long and not useful
to read."
  (occur regexp)
  (when cleanup
    (let ((cwin (selected-window)) win beg end)
      (when (setq win (get-buffer-window "*Occur*"))
	(select-window win))
      (goto-char (point-min))
      (when (re-search-forward "match[a-z]+" nil t)
	(setq beg (match-end 0))
	(when (re-search-forward "^[ \t]*[0-9]+" nil t)
	  (setq end (1- (match-beginning 0)))))
      (and beg end (let ((inhibit-read-only t)) (delete-region beg end)))
      (goto-char (point-min))
      (select-window cwin))))


;;; Following specific links

(defvar org-agenda-buffer-tmp-name)
(defvar org-agenda-start-on-weekday)
(defvar org-agenda-buffer-name)
(defun org-follow-timestamp-link ()
  "Open an agenda view for the timestamp date/range at point."
  (require 'org-agenda)
  ;; Avoid changing the global value.
  (let ((org-agenda-buffer-name org-agenda-buffer-name))
    (cond
     ((org-at-date-range-p t)
      (let ((org-agenda-start-on-weekday)
	    (t1 (match-string 1))
	    (t2 (match-string 2)) tt1 tt2)
	(setq tt1 (time-to-days (org-time-string-to-time t1))
	      tt2 (time-to-days (org-time-string-to-time t2)))
	(let ((org-agenda-buffer-tmp-name
	       (format "*Org Agenda(a:%s)"
		       (concat (substring t1 0 10) "--" (substring t2 0 10)))))
	  (org-agenda-list nil tt1 (1+ (- tt2 tt1))))))
     ((org-at-timestamp-p 'lax)
      (let ((org-agenda-buffer-tmp-name
	     (format "*Org Agenda(a:%s)" (substring (match-string 1) 0 10))))
	(org-agenda-list nil (time-to-days (org-time-string-to-time
					  (substring (match-string 1) 0 10)))
			 1)))
     (t (error "This should not happen")))))


;;;; Refiling

(declare-function org-string-nw-p "org-macs" (s))
;;;; Completion

(declare-function org-export-backend-options "ox" (cl-x) t)
(defun org-get-export-keywords ()
  "Return a list of all currently understood export keywords.
Export keywords include options, block names, attributes and
keywords relative to each registered export backend."
  (let (keywords)
    (dolist (backend
	     (bound-and-true-p org-export-registered-backends)
	     (delq nil keywords))
      ;; Backend name (for keywords, like #+LATEX:)
      (push (upcase (symbol-name (org-export-backend-name backend))) keywords)
      ;; Backend attributes, like #+ATTR_LATEX:
      (push (format "ATTR_%s" (upcase (symbol-name (org-export-backend-name backend)))) keywords)
      (dolist (option-entry (org-export-backend-options backend))
	;; Backend options.
	(push (nth 1 option-entry) keywords)))))

(defconst org-options-keywords
  '("ARCHIVE:" "AUTHOR:" "BIBLIOGRAPHY:" "BIND:" "CATEGORY:" "CITE_EXPORT:"
    "COLUMNS:" "CREATOR:" "DATE:" "DESCRIPTION:" "DRAWERS:" "EMAIL:"
    "EXCLUDE_TAGS:" "FILETAGS:" "INCLUDE:" "INDEX:" "KEYWORDS:" "LANGUAGE:"
    "MACRO:" "OPTIONS:" "PROPERTY:" "PRINT_BIBLIOGRAPHY:" "PRIORITIES:"
    "SELECT_TAGS:" "SEQ_TODO:" "SETUPFILE:" "STARTUP:" "TAGS:" "TITLE:" "TODO:"
    "TYP_TODO:" "SELECT_TAGS:" "EXCLUDE_TAGS:" "EXPORT_FILE_NAME:"))

(defcustom org-structure-template-alist
  '(("a" . "export ascii")
    ("c" . "center")
    ("C" . "comment")
    ("e" . "example")
    ("E" . "export")
    ("h" . "export html")
    ("l" . "export latex")
    ("q" . "quote")
    ("s" . "src")
    ("v" . "verse"))
  "An alist of keys and block types.
`org-insert-structure-template' will display a menu with this list of
templates to choose from.  The block type is inserted, with
\"#+begin_\" and \"#+end_\" added automatically.  If the block type
consists of just uppercase letters, \"#+BEGIN_\" and \"#+END_\" are
added instead.

The menu keys are defined by the car of each entry in this alist.
If two entries have the keys \"a\" and \"aa\" respectively, the
former will be inserted by typing \"a TAB/RET/SPC\" and the
latter will be inserted by typing \"aa\".  If an entry with the
key \"aab\" is later added, it can be inserted by typing \"ab\".

If loaded, Org Tempo also uses `org-structure-template-alist'.  A
block can be inserted by pressing TAB after the string \"<KEY\"."
  :group 'org-edit-structure
  :type '(repeat
	  (cons (string :tag "Key")
		(string :tag "Template")))
  :package-version '(Org . "9.6"))

(defun org--insert-structure-template-mks ()
  "Present `org-structure-template-alist' with `org-mks'.

Menus are added if keys require more than one keystroke.  Tabs
are added to single key entries when more than one stroke is
needed.  Keys longer than two characters are reduced to two
characters."
  (org--check-org-structure-template-alist 'org-structure-template-alist)
  (let* (case-fold-search
	 (templates (append org-structure-template-alist
			    '(("\t" . "Press TAB, RET or SPC to write block name"))))
         (keys (mapcar #'car templates))
         (start-letters
	  (delete-dups (mapcar (lambda (key) (substring key 0 1)) keys)))
	 ;; Sort each element of `org-structure-template-alist' into
	 ;; sublists according to the first letter.
         (superlist
	  (mapcar (lambda (letter)
                    (list letter
			  (cl-remove-if-not
			   (apply-partially #'string-match-p (concat "^" letter))
			   templates :key #'car)))
		  start-letters)))
    (org-mks
     (apply #'append
	    ;; Make an `org-mks' table.  If only one element is
	    ;; present in a sublist, make it part of the top-menu,
	    ;; otherwise make a submenu according to the starting
	    ;; letter and populate it.
	    (mapcar (lambda (sublist)
		      (if (eq 1 (length (cadr sublist)))
                          (mapcar (lambda (elm)
				    (list (substring (car elm) 0 1)
                                          (cdr elm) ""))
                                  (cadr sublist))
			;; Create submenu.
                        (let* ((topkey (car sublist))
			       (elms (cadr sublist))
			       (keys (mapcar #'car elms))
			       (long (> (length elms) 3)))
                          (append
			   (list
			    ;; Make a description of the submenu.
			    (list topkey
				  (concat
				   (mapconcat #'cdr
					      (cl-subseq elms 0 (if long 3 (length elms)))
					      ", ")
                                   (when long ", ..."))))
			   ;; List of entries in submenu.
			   (cl-mapcar #'list
				      (org--insert-structure-template-unique-keys keys)
				      (mapcar #'cdr elms)
				      (make-list (length elms) ""))))))
		    superlist))
     "Select a key\n============"
     "Key: ")))

(defun org--insert-structure-template-unique-keys (keys)
  "Make a list of unique, two characters long elements from KEYS.

Elements of length one have a tab appended.  Elements of length
two are kept as is.  Longer elements are truncated to length two.

If an element cannot be made unique, an error is raised."
  (let ((ordered-keys (cl-sort (copy-sequence keys) #'< :key #'length))
	menu-keys)
    (dolist (key ordered-keys)
      (let ((potential-key
	     (cl-case (length key)
	       (1 (concat key "\t"))
	       (2 key)
	       (otherwise
		(cl-find-if-not (lambda (k) (assoc k menu-keys))
				(mapcar (apply-partially #'concat (substring  key 0 1))
					(split-string (substring key 1) "" t)))))))
	(if (or (not potential-key) (assoc potential-key menu-keys))
            (user-error "Could not make unique key for `%s'" key)
	  (push (cons potential-key key) menu-keys))))
    (mapcar #'car
	    (cl-sort menu-keys #'<
		     :key (lambda (elm) (cl-position (cdr elm) keys))))))

(defalias 'org-insert-block-template #'org-insert-structure-template)
(defun org-insert-structure-template (type)
  "Insert a block structure of the type #+begin_foo/#+end_foo.
Select a block from `org-structure-template-alist' then type
either RET, TAB or SPC to write the block type.  With an active
region, wrap the region in the block.  Otherwise, insert an empty
block.

When foo is written as FOO, upcase the #+BEGIN/END as well."
  (interactive
   (list (pcase (org--insert-structure-template-mks)
	   (`("\t" . ,_)
            (let ((type (read-string "Structure type: ")))
              (when (string-empty-p type) (user-error "Empty structure type"))
              type))
	   (`(,_ ,choice . ,_) choice))))
  (when (or (not (stringp type)) (string-empty-p type))
    (error "Invalid structure type: %S" type))
  (let* ((case-fold-search t) ; Make sure that matches are case-insensitive.
         (region? (use-region-p))
	 (region-start (and region? (region-beginning)))
	 (region-end (and region? (copy-marker (region-end))))
	 (extended? (string-match-p "\\`\\(src\\|export\\)\\'" type))
	 (verbatim? (string-match-p
		     (concat "\\`" (regexp-opt '("example" "export"
                                                 "src" "comment")))
		     type))
         (upcase? (string= (car (split-string type))
                           (upcase (car (split-string type))))))
    (when region? (goto-char region-start))
    (let ((column (current-indentation)))
      (if (save-excursion (skip-chars-backward " \t") (bolp))
	  (forward-line 0)
	(insert "\n"))
      (save-excursion
	(indent-to column)
	(insert (format "#+%s_%s%s\n" (if upcase? "BEGIN" "begin") type (if extended? " " "")))
	(when region?
	  (when verbatim? (org-escape-code-in-region (point) region-end))
	  (goto-char region-end)
	  ;; Ignore empty lines at the end of the region.
	  (skip-chars-backward " \r\t\n")
	  (unless (eolp) (insert "\n") (forward-line -1))
	  (end-of-line))
	(unless (bolp) (insert "\n"))
	(indent-to column)
	(insert (format "#+%s_%s" (if upcase? "END" "end") (car (split-string type))))
	(if (looking-at "[ \t]*$") (replace-match "")
	  (insert "\n"))
	(when (and (eobp) (not (bolp))) (insert "\n")))
      (if extended? (end-of-line)
	(forward-line)
	(skip-chars-forward " \t")))))


;;;; TODO, DEADLINE, Comments

(defvar org-last-todo-state-is-todo nil
  "This is non-nil when the last TODO state change led to a TODO state.
If the last change removed the TODO tag or switched to DONE, then
this is nil.")

(defvar org-todo-setup-filter-hook nil
  "Hook for functions that pre-filter todo specs.
Each function takes a todo spec and returns either nil or the spec
transformed into canonical form." )

(defvar org-todo-get-default-hook nil
  "Hook for functions that get a default item for todo.
Each function takes arguments (NEW-MARK OLD-MARK) and returns either
nil or a string to be used for the todo mark." )

(defvar org-agenda-headline-snapshot-before-repeat)

(defun org-todo-yesterday (&optional arg)
  "Like `org-todo' but the time of change will be 23:59 of yesterday."
  (interactive "P" org-mode org-agenda-mode)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo-yesterday arg)
    (let* ((org-use-effective-time t)
	   (hour (decoded-time-hour (decode-time (org-current-time))))
	   (org-extend-today-until (1+ hour)))
      (org-todo arg))))

(defvar org-block-entry-blocking ""
  "First entry preventing the TODO state change.")

(defalias 'org-cancel-repeater #'org-cancel-repeaters)
(defun org-cancel-repeaters ()
  "Cancel all the repeaters in entry by setting their numeric value to zero."
  (interactive nil org-mode)
  (save-excursion
    (org-back-to-heading t)
    (let ((bound1 (point))
	  (bound0 (save-excursion (outline-next-heading) (point))))
      (while (re-search-forward
	      (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
		      org-deadline-time-regexp "\\)\\|\\("
		      org-ts-regexp "\\)")
	      bound0 t)
        (when (save-excursion
	        (re-search-backward "[ \t]+\\(?:[.+]\\)?\\+\\([0-9]+\\)[hdwmy]"
			            bound1 t))
	  (replace-match "0" t nil nil 1))))))

(defvar org-state)
;; FIXME: We should refactor this and similar dynamically scoped blocker flags.
(defvar org-blocked-by-checkboxes nil) ; dynamically scoped
(defun org-todo (&optional arg)
  "Change the TODO state of an item.

The state of an item is given by a keyword at the start of the heading,
like
     *** TODO Write paper
     *** DONE Call mom

The different keywords are specified in the variable `org-todo-keywords'.
By default the available states are \"TODO\" and \"DONE\".  So, for this
example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.
You can set up single-character keys to fast-select the new state.  See the
`org-todo-keywords' and `org-use-fast-todo-selection' for details.

With `\\[universal-argument]' prefix ARG, force logging the state change \
and take a
logging note.
With a `\\[universal-argument] \\[universal-argument]' prefix, switch to the \
next set of TODO \
keywords (nextset).
Another way to achieve this is `S-C-<right>'.
With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix, circumvent any state blocking.
With numeric prefix arg, switch to the Nth state.

With a numeric prefix arg of 0, inhibit note taking for the change.
With a numeric prefix arg of -1, cancel repeater to allow marking as DONE.

When called through Elisp, arg is also interpreted in the following way:
`none'        -> empty state
\"\"            -> switch to empty state
`done'        -> switch to DONE
`nextset'     -> switch to the next set of keywords
`previousset' -> switch to the previous set of keywords
\"WAITING\"     -> switch to the specified keyword, but only if it
                 really is a member of `org-todo-keywords'."
  (interactive "P" org-mode)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 (lambda () (org-todo arg))
	 nil cl
	 (when (org-invisible-p) (org-end-of-subtree nil t))))
    (when (equal arg '(16)) (setq arg 'nextset))
    (when (equal (prefix-numeric-value arg) -1) (org-cancel-repeaters) (setq arg nil))
    (when (< (prefix-numeric-value arg) -1) (user-error "Prefix argument %d not supported" arg))
    (let ((org-blocker-hook org-blocker-hook)
	  commentp
	  case-fold-search)
      (when (equal arg '(64))
	(setq arg nil org-blocker-hook nil))
      (when (and org-blocker-hook
		 (or org-inhibit-blocking
		     (org-entry-get nil "NOBLOCKING")))
	(setq org-blocker-hook nil))
      (save-excursion
	(catch 'exit
	  (org-back-to-heading t)
	  (when (org-in-commented-heading-p t)
	    (org-toggle-comment)
	    (setq commentp t))
	  (when (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
	  (or (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
	      (looking-at "\\(?: *\\|[ \t]*$\\)"))
	  (let* ((match-data (match-data))
		 (startpos (copy-marker (line-beginning-position)))
		 (force-log (and  (equal arg '(4)) (prog1 t (setq arg nil))))
		 (logging (save-match-data (org-entry-get nil "LOGGING" t t)))
		 (org-log-done org-log-done)
		 (org-log-repeat org-log-repeat)
		 (org-todo-log-states org-todo-log-states)
		 (org-inhibit-logging
		  (if (equal arg 0)
		      (progn (setq arg nil) 'note) org-inhibit-logging))
		 (this (match-string 1))
		 (hl-pos (match-beginning 0))
		 (head (org-get-todo-sequence-head this))
		 (ass (assoc head org-todo-kwd-alist))
		 (interpret (nth 1 ass))
		 (done-word (nth 3 ass))
		 (final-done-word (nth 4 ass))
		 (org-last-state (or this ""))
		 (completion-ignore-case t)
		 (member (member this org-todo-keywords-1))
		 (tail (cdr member))
		 (org-state (cond
			     ((eq arg 'right)
			      ;; Next state
			      (if this
			          (if tail (car tail) nil)
			        (car org-todo-keywords-1)))
			     ((eq arg 'left)
			      ;; Previous state
			      (unless (equal member org-todo-keywords-1)
			        (if this
				    (nth (- (length org-todo-keywords-1)
					    (length tail) 2)
				         org-todo-keywords-1)
			          (org-last org-todo-keywords-1))))
			     (arg
			      ;; User or caller requests a specific state.
			      (cond
			       ((equal arg "") nil)
			       ((eq arg 'none) nil)
			       ((eq arg 'done) (or done-word (car org-done-keywords)))
			       ((eq arg 'nextset)
			        (or (car (cdr (member head org-todo-heads)))
				    (car org-todo-heads)))
			       ((eq arg 'previousset)
			        (let ((org-todo-heads (reverse org-todo-heads)))
			          (or (car (cdr (member head org-todo-heads)))
				      (car org-todo-heads))))
			       ((car (member arg org-todo-keywords-1)))
			       ((stringp arg)
			        (user-error "State `%s' not valid in this file" arg))
			       ((nth (1- (prefix-numeric-value arg))
				     org-todo-keywords-1))))
			     ((and org-todo-key-trigger org-use-fast-todo-selection)
			      ;; Use fast selection.
			      (org-fast-todo-selection this))
			     ((null member) (or head (car org-todo-keywords-1)))
			     ((equal this final-done-word) nil) ;-> make empty
			     ((null tail) nil) ;-> first entry
			     ((memq interpret '(type priority))
			      (if (eq this-command last-command)
			          (car tail)
			        (if (> (length tail) 0)
				    (or done-word (car org-done-keywords))
			          nil)))
			     (t
			      (car tail))))
		 (org-state (or
			     (run-hook-with-args-until-success
			      'org-todo-get-default-hook org-state org-last-state)
			     org-state))
		 (next (if (org-string-nw-p org-state) (concat " " org-state " ") " "))
		 (change-plist (list :type 'todo-state-change :from this :to org-state
				     :position startpos))
		 dolog now-done-p)
	    (when org-blocker-hook
	      (let (org-blocked-by-checkboxes block-reason)
		(setq org-last-todo-state-is-todo
		      (not (member this org-done-keywords)))
		(unless (save-excursion
			  (save-match-data
			    (org-with-wide-buffer
			     (run-hook-with-args-until-failure
			      'org-blocker-hook change-plist))))
		  (setq block-reason (if org-blocked-by-checkboxes
					 "contained checkboxes"
				       (format "\"%s\"" org-block-entry-blocking)))
		  (if (called-interactively-p 'interactive)
		      (user-error "TODO state change from %s to %s blocked (by %s)"
				  this org-state block-reason)
		    ;; Fail silently.
		    (message "TODO state change from %s to %s blocked (by %s)"
			     this org-state block-reason)
		    (throw 'exit nil)))))
	    (store-match-data match-data)
            (org-fold-core-ignore-modifications
              (goto-char (match-beginning 0))
              (replace-match "")
              ;; We need to use `insert-before-markers-and-inherit'
              ;; because: (1) We want to preserve the folding state
              ;; text properties; (2) We do not want to make point
              ;; move before new todo state when inserting a new todo
              ;; into an empty heading.  In (2), the above
              ;; `save-excursion' is relying on markers saved before.
              (insert-before-markers-and-inherit next)
              (unless (org-invisible-p (line-beginning-position))
                (org-fold-region (line-beginning-position)
                                 (line-end-position)
                                 nil 'outline)))
	    (cond ((and org-state (equal this org-state))
		   (message "TODO state was already %s" (org-trim next)))
		  ((not (pos-visible-in-window-p hl-pos))
		   (message "TODO state changed to %s" (org-trim next))))
	    (unless head
	      (setq head (org-get-todo-sequence-head org-state)
		    ass (assoc head org-todo-kwd-alist)
		    interpret (nth 1 ass)
		    done-word (nth 3 ass)
		    final-done-word (nth 4 ass)))
	    (when (memq arg '(nextset previousset))
	      (message "Keyword-Set %d/%d: %s"
		       (- (length org-todo-sets) -1
			  (length (memq (assoc org-state org-todo-sets) org-todo-sets)))
		       (length org-todo-sets)
		       (mapconcat #'identity (assoc org-state org-todo-sets) " ")))
	    (setq org-last-todo-state-is-todo
		  (not (member org-state org-done-keywords)))
	    (setq now-done-p (and (member org-state org-done-keywords)
				  (not (member this org-done-keywords))))
	    (and logging (org-local-logging logging))
	    (when (or (and (or org-todo-log-states org-log-done)
			   (not (eq org-inhibit-logging t))
			   (not (memq arg '(nextset previousset))))
		      force-log)
	      ;; We need to look at recording a time and note.
	      (setq dolog (or (if force-log 'note)
			      (nth 1 (assoc org-state org-todo-log-states))
			      (nth 2 (assoc this org-todo-log-states))))
	      (when (and (eq dolog 'note) (eq org-inhibit-logging 'note))
		(setq dolog 'time))
	      (when (or (and (not org-state) (not org-closed-keep-when-no-todo))
			(and org-state
			     (member org-state org-not-done-keywords)
			     (not (member this org-not-done-keywords))))
		;; This is now a todo state and was not one before
		;; If there was a CLOSED time stamp, get rid of it.
		(org-add-planning-info nil nil 'closed))
	      (when (and now-done-p org-log-done)
		;; It is now done, and it was not done before.
		(org-add-planning-info 'closed (org-current-effective-time))
		(when (and (not dolog) (eq 'note org-log-done))
		  (org-add-log-setup 'done org-state this 'note)))
	      (when (and org-state dolog)
		;; This is a non-nil state, and we need to log it.
		(org-add-log-setup 'state org-state this dolog)))
	    ;; Fixup tag positioning.
	    (org-todo-trigger-tag-changes org-state)
	    (when org-auto-align-tags (org-align-tags))
	    (when org-provide-todo-statistics
	      (org-update-parent-todo-statistics))
	    (when (bound-and-true-p org-clock-out-when-done)
	      (org-clock-out-if-current))
            (save-excursion
	      (run-hooks 'org-after-todo-state-change-hook))
	    (when (and arg (not (member org-state org-done-keywords)))
	      (setq head (org-get-todo-sequence-head org-state)))
            (put-text-property (line-beginning-position)
                               (line-end-position) 'org-todo-head head)
	    ;; Do we need to trigger a repeat?
	    (when now-done-p
	      (when (boundp 'org-agenda-headline-snapshot-before-repeat)
		;; This is for the agenda, take a snapshot of the headline.
		(save-match-data
		  (setq org-agenda-headline-snapshot-before-repeat
			(org-get-heading))))
              (when force-log
                (setq org-log-repeat dolog))
	      (org-auto-repeat-maybe org-state))
	    ;; Fixup cursor location if close to the keyword.
	    (when (and (outline-on-heading-p)
		       (not (bolp))
		       (save-excursion
                         (forward-line 0)
			 (looking-at org-todo-line-regexp))
		       (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
	      (goto-char (or (match-end 2) (match-end 1)))
	      (and (looking-at " ")
		   (not (looking-at " *:"))
		   (just-one-space)))
	    (when org-trigger-hook
	      (save-excursion
		(run-hook-with-args 'org-trigger-hook change-plist)))
	    (when commentp (org-toggle-comment))))))))

(defun org-block-todo-from-children-or-siblings-or-parent (change-plist)
  "Block turning an entry into a TODO, using the hierarchy.
This checks whether the current task should be blocked from state
changes.  Such blocking occurs when:

  1. The task has children which are not all in a completed state.

  2. A task has a parent with the property :ORDERED:, and there
     are siblings prior to the current task with incomplete
     status.

  3. The parent of the task is blocked because it has siblings that should
     be done first, or is child of a block grandparent TODO entry."

  (if (not org-enforce-todo-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has children, and any are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((this-level (funcall outline-level)))
	  (outline-next-heading)
	  (let ((child-level (funcall outline-level)))
	    (while (and (not (eobp))
			(> child-level this-level))
	      ;; this todo has children, check whether they are all
	      ;; completed
	      (when (and (not (org-entry-is-done-p))
			 (org-entry-is-todo-p))
		(setq org-block-entry-blocking (org-get-heading))
		(throw 'dont-block nil))
	      (outline-next-heading)
	      (setq child-level (funcall outline-level))))))
      ;; Otherwise, if the task's parent has the :ORDERED: property, and
      ;; any previous siblings are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let* ((pos (point))
	       (parent-pos (and (org-up-heading-safe) (point)))
	       (case-fold-search nil))
	  (unless parent-pos (throw 'dont-block t)) ; no parent
	  (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		     (forward-line 1)
		     (re-search-forward org-not-done-heading-regexp pos t))
	    (setq org-block-entry-blocking (match-string 0))
	    (throw 'dont-block nil))  ; block, there is an older sibling not done.
	  ;; Search further up the hierarchy, to see if an ancestor is blocked
	  (while t
	    (goto-char parent-pos)
	    (unless (looking-at org-not-done-heading-regexp)
	      (throw 'dont-block t))	; do not block, parent is not a TODO
	    (setq pos (point))
	    (setq parent-pos (and (org-up-heading-safe) (point)))
	    (unless parent-pos (throw 'dont-block t)) ; no parent
	    (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		       (forward-line 1)
		       (re-search-forward org-not-done-heading-regexp pos t)
		       (setq org-block-entry-blocking (org-get-heading)))
	      (throw 'dont-block nil)))))))) ; block, older sibling not done.

(defcustom org-track-ordered-property-with-tag nil
  "Should the ORDERED property also be shown as a tag?
The ORDERED property decides if an entry should require subtasks to be
completed in sequence.  Since a property is not very visible, setting
this option means that toggling the ORDERED property with the command
`org-toggle-ordered-property' will also toggle a tag ORDERED.  That tag is
not relevant for the behavior, but it makes things more visible.

Note that toggling the tag with tags commands will not change the property
and therefore not influence behavior!

This can be t, meaning the tag ORDERED should be used.  It can also be a
string to select a different tag for this task."
  :group 'org-todo
  :type '(choice
	  (const :tag "No tracking" nil)
	  (const :tag "Track with ORDERED tag" t)
	  (string :tag "Use other tag")))

(defun org-toggle-ordered-property ()
  "Toggle the ORDERED property of the current entry.
For better visibility, you can track the value of this property with a tag.
See variable `org-track-ordered-property-with-tag'."
  (interactive nil org-mode)
  (let* ((t1 org-track-ordered-property-with-tag)
	 (tag (and t1 (if (stringp t1) t1 "ORDERED"))))
    (save-excursion
      (org-back-to-heading)
      (if (org-entry-get nil "ORDERED")
	  (progn
	    (org-delete-property "ORDERED")
	    (and tag (org-toggle-tag tag 'off))
	    (message "Subtasks can be completed in arbitrary order"))
	(org-entry-put nil "ORDERED" "t")
	(and tag (org-toggle-tag tag 'on))
	(message "Subtasks must be completed in sequence")))))

(defun org-block-todo-from-checkboxes (change-plist)
  "Block turning an entry into a TODO, using checkboxes.
This checks whether the current task should be blocked from state
changes because there are unchecked boxes in this entry."
  (if (not org-enforce-todo-checkbox-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has checkboxes that are not checked, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((beg (point)) end)
	  (outline-next-heading)
	  (setq end (point))
	  (goto-char beg)
	  (when (org-list-search-forward
		 (concat (org-item-beginning-re)
			 "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
			 "\\[[- ]\\]")
		 end t)
	    (when (boundp 'org-blocked-by-checkboxes)
	      (setq org-blocked-by-checkboxes t))
	    (throw 'dont-block nil))))
      t))) ; do not block

(defun org-entry-blocked-p ()
  "Non-nil if entry at point is blocked."
  (and (not (org-entry-get nil "NOBLOCKING"))
       (member (org-entry-get nil "TODO") org-not-done-keywords)
       (not (run-hook-with-args-until-failure
	     'org-blocker-hook
	     (list :type 'todo-state-change
		   :position (point)
		   :from 'todo
		   :to 'done)))))

(defun org-update-statistics-cookies (all)
  "Update the statistics cookie, either from TODO or from checkboxes.
This should be called with the cursor in a line with a statistics
cookie.  When called with a \\[universal-argument] prefix, update all
statistics cookies in the accessible portion of the buffer, i.e.,
respect narrowing."
  (interactive "P" org-mode)
  (if all
      (progn
	(org-update-checkbox-count
	 (if (buffer-narrowed-p) 'narrow 'all))
	(org-map-region 'org-update-parent-todo-statistics
                        (point-min) (point-max)))
    (if (not (org-at-heading-p))
	(org-update-checkbox-count)
      (let ((pos (point-marker))
	    end l1 l2)
	(ignore-errors (org-back-to-heading t))
	(if (not (org-at-heading-p))
	    (org-update-checkbox-count)
	  (setq l1 (org-outline-level))
	  (setq end
                (save-excursion
		  (outline-next-heading)
		  (when (org-at-heading-p) (setq l2 (org-outline-level)))
		  (point)))
	  (if (and (save-excursion
		     (re-search-forward
		      "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) \\[[- X]\\]" end t))
	           (not (save-excursion
                        (re-search-forward
			 ":COOKIE_DATA:.*\\<todo\\>" end t))))
	      (org-update-checkbox-count)
	    (if (and l2 (> l2 l1))
		(progn
		  (goto-char end)
		  (org-update-parent-todo-statistics))
	      (goto-char pos)
	      (forward-line 0)
	      (while (re-search-forward
		      "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)"
                      (line-end-position) t)
		(replace-match (if (match-end 2) "[100%]" "[0/0]") t t)))))
	(goto-char pos)
	(move-marker pos nil)))))

(defvar org-entry-property-inherited-from) ;; defined below
(defun org-update-parent-todo-statistics ()
  "Update any statistics cookie in the parent of the current headline.
When `org-todo-children-only-statistics' is nil, statistics will cover
the entire subtree and this will travel up the hierarchy and update
statistics everywhere."
  (let* ((prop (save-excursion
                 (org-up-heading-safe)
		 (org-entry-get nil "COOKIE_DATA" 'inherit)))
	 (recursive (or (not org-todo-children-only-statistics)
			(and prop (string-match "\\<recursive\\>" prop))))
	 (lim (or (and prop (marker-position org-entry-property-inherited-from))
		  0))
	 (first t)
	 (box-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	 level ltoggle l1 new ndel
	 (cnt-all 0) (cnt-done 0) is-percent kwd
	 checkbox-beg cookie-present)
    (catch 'exit
      (save-excursion
	(forward-line 0)
	(setq ltoggle (funcall outline-level))
	;; Three situations are to consider:

	;; 1. if `org-todo-children-only-statistics' is nil, repeat up
	;;    to the top-level ancestor on the headline;

	;; 2. If parent has "recursive" property, repeat up to the
	;;    headline setting that property, taking inheritance into
	;;    account;

	;; 3. Else, move up to direct parent and proceed only once.
	(while (and (setq level (org-up-heading-safe))
		    (or recursive first)
		    (>= (point) lim))
	  (setq first nil cookie-present nil)
	  (unless (and level
		       (not (string-match
			   "\\<checkbox\\>"
			   (downcase (or (org-entry-get nil "COOKIE_DATA")
					 "")))))
	    (throw 'exit nil))
          (while (re-search-forward box-re (line-end-position) t)
	    (setq cnt-all 0 cnt-done 0 cookie-present t)
	    (setq is-percent (match-end 2) checkbox-beg (match-beginning 0))
            (when (org-element-type-p
                   (save-excursion
                     (goto-char checkbox-beg)
                     (save-match-data (org-element-context)))
                   '(statistics-cookie
                     ;; Special case - statistics cookie inside properties.
                     keyword))
	      (save-match-data
	        (unless (outline-next-heading) (throw 'exit nil))
	        (while (and (let ((case-fold-search nil))
                              (looking-at org-complex-heading-regexp))
                            (> (setq l1 (length (match-string 1))) level))
                  (setq kwd (and (or recursive (= l1 ltoggle))
                                 (match-string 2)))
                  (if (or (eq org-provide-todo-statistics 'all-headlines)
                          (and (eq org-provide-todo-statistics t)
			       (or (member kwd org-done-keywords)))
                          (and (listp org-provide-todo-statistics)
			       (stringp (car org-provide-todo-statistics))
                               (or (member kwd org-provide-todo-statistics)
				   (member kwd org-done-keywords)))
			  (and (listp org-provide-todo-statistics)
			       (listp (car org-provide-todo-statistics))
			       (or (member kwd (car org-provide-todo-statistics))
				   (and (member kwd org-done-keywords)
				        (member kwd (cadr org-provide-todo-statistics))))))
                      (setq cnt-all (1+ cnt-all))
		    (and (eq org-provide-todo-statistics t)
		         kwd
		         (setq cnt-all (1+ cnt-all))))
		  (when (or (and (member org-provide-todo-statistics '(t all-headlines))
			         (member kwd org-done-keywords))
			    (and (listp org-provide-todo-statistics)
			         (listp (car org-provide-todo-statistics))
			         (member kwd org-done-keywords)
			         (member kwd (cadr org-provide-todo-statistics)))
			    (and (listp org-provide-todo-statistics)
			         (stringp (car org-provide-todo-statistics))
			         (member kwd org-done-keywords)))
		    (setq cnt-done (1+ cnt-done)))
                  (outline-next-heading)))
	      (setq new
                    (if is-percent
                        (org-format-percent-cookie cnt-done cnt-all)
		      (format "[%d/%d]" cnt-done cnt-all))
                    ndel (- (match-end 0) checkbox-beg))
              (goto-char (match-end 0))
              (unless (string-equal new (buffer-substring checkbox-beg (match-end 0)))
	        (goto-char checkbox-beg)
	        (insert new)
	        (delete-region (point) (+ (point) ndel))
	        (when org-auto-align-tags (org-fix-tags-on-the-fly)))))
	  (when cookie-present
	    (run-hook-with-args 'org-after-todo-statistics-hook
				cnt-done (- cnt-all cnt-done))))))
    (run-hooks 'org-todo-statistics-hook)))

(defvar org-after-todo-statistics-hook nil
  "Hook that is called after a TODO statistics cookie has been updated.
Each function is called with two arguments: the number of not-done entries
and the number of done entries.

For example, the following function, when added to this hook, will switch
an entry to DONE when all children are done, and back to TODO when new
entries are set to a TODO status.  Note that this hook is only called
when there is a statistics cookie in the headline!

 (defun org-summary-todo (n-done n-not-done)
   \"Switch entry to DONE when all subentries are done, to TODO otherwise.\"
   (let (org-log-done org-todo-log-states)   ; turn off logging
     (org-todo (if (= n-not-done 0) \"DONE\" \"TODO\"))))")

(defvar org-todo-statistics-hook nil
  "Hook that is run whenever Org thinks TODO statistics should be updated.
This hook runs even if there is no statistics cookie present, in which case
`org-after-todo-statistics-hook' would not run.")

(defun org-todo-trigger-tag-changes (state)
  "Apply the changes defined in `org-todo-state-tags-triggers'."
  (let ((l org-todo-state-tags-triggers)
	changes)
    (when (or (not state) (equal state ""))
      (setq changes (append changes (cdr (assoc "" l)))))
    (when (and (stringp state) (> (length state) 0))
      (setq changes (append changes (cdr (assoc state l)))))
    (when (member state org-not-done-keywords)
      (setq changes (append changes (cdr (assq 'todo l)))))
    (when (member state org-done-keywords)
      (setq changes (append changes (cdr (assq 'done l)))))
    (dolist (c changes)
      (org-toggle-tag (car c) (if (cdr c) 'on 'off)))))

(defun org-local-logging (value)
  "Get logging settings from a property VALUE."
  ;; Directly set the variables, they are already local.
  (setq org-log-done nil
        org-log-repeat nil
        org-todo-log-states nil)
  (dolist (w (split-string value))
    (let (a)
      (cond
       ((setq a (assoc-string w org-startup-options t))
        (and (member (nth 1 a) '(org-log-done org-log-repeat))
             (set (nth 1 a) (nth 2 a))))
       ((setq a (org-extract-log-state-settings w))
        (and (member (car a) org-todo-keywords-1)
             (push a org-todo-log-states)))))))

(defun org-get-todo-sequence-head (kwd)
  "Return the head of the TODO sequence to which KWD belongs.
If KWD is not set, check if there is a text property remembering the
right sequence."
  (let (p)
    (cond
     ((not kwd)
      (or (get-text-property (line-beginning-position) 'org-todo-head)
	  (progn
            (setq p (next-single-property-change (line-beginning-position)
                                                 'org-todo-head
                                                 nil (line-end-position)))
	    (get-text-property p 'org-todo-head))))
     ((not (member kwd org-todo-keywords-1))
      (car org-todo-keywords-1))
     (t (nth 2 (assoc kwd org-todo-kwd-alist))))))

(defun org-fast-todo-selection (&optional current-todo-keyword)
  "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur.

When CURRENT-TODO-KEYWORD is given and selection letters are not
unique globally, prefer a state in the current todo keyword sequence
where CURRENT-TODO-KEYWORD belongs over on in another sequence."
  (let* ((todo-alist org-todo-key-alist) ; copy from the original Org buffer.
         (todo-alist-tail todo-alist)
         ;; TODO keyword sequence that takes priority in case if there is binding collision.
	 (preferred-sequence-head (org-get-todo-sequence-head current-todo-keyword))
         in-preferred-sequence preferred-todo-alist
	 (done-keywords org-done-keywords) ;; needed for the faces when calling `org-get-todo-face'.
	 (expert-interface (equal org-use-fast-todo-selection 'expert))
	 (prompt "") ; Additional expert prompt, listing todo keyword bindings.
         ;; Max width occupied by a single todo record in the completion buffer.
         (field-width
          (+ 3 ; keep space for "[c]" binding.
             1 ; ensure that there is at least one space between adjacent todo fields.
             3 ; FIXME: likely coped from `org-fast-tag-selection'
             ;; The longest todo keyword.
             (apply #'max (mapcar
			  (lambda (x)
			    (if (stringp (car x)) (string-width (car x)) 0))
			  org-todo-key-alist))))
         field-number ; current todo keyword column in the completion buffer.
         todo-binding-spec todo-keyword todo-char input-char)
    ;; Display todo selection dialog, read the user input, and return.
    (save-excursion
      (save-window-excursion
        ;; Select todo keyword list buffer, and display it unless EXPERT-INTERFACE.
	(if expert-interface
	    (set-buffer (get-buffer-create " *Org todo*"))
          (pop-to-buffer
           (get-buffer-create (get-buffer-create " *Org todo*"))
           '(org-display-buffer-split (direction . down))))
        ;; Fill text in *Org todo* buffer.
	(erase-buffer)
        ;; Copy `org-done-keywords' from the original Org buffer to be
        ;; used by `org-get-todo-face'.
	(setq-local org-done-keywords done-keywords)
        ;; Show todo keyword sequences and bindings in a grid.
        ;; Each todo keyword in the grid occupies FIELD-WIDTH characters.
        ;; The keywords are filled up to `window-width'.
	(setq field-number 0)
	(while (setq todo-binding-spec (pop todo-alist-tail))
	  (pcase todo-binding-spec
            ;; Group keywords as { KWD1 KWD2 ... }
	    (`(:startgroup)
	     (unless (= field-number 0)
	       (setq field-number 0)
	       (insert "\n"))
	     (setq prompt (concat prompt "{"))
	     (insert "{ "))
	    (`(:endgroup)
	     (setq field-number 0
                   ;; End of a group.  Reset flag indicating preferred keyword sequence.
                   in-preferred-sequence nil)
	     (setq prompt (concat prompt "}"))
	     (insert "}\n"))
	    (`(:newline)
	     (unless (= field-number 0)
	       (insert "\n")
	       (setq field-number 0)
	       (setq todo-binding-spec (car todo-alist-tail))
	       (while (equal (car todo-alist-tail) '(:newline))
		 (insert "\n")
		 (pop todo-alist-tail))))
	    (_
	     (setq todo-keyword (car todo-binding-spec)
                   todo-char (cdr todo-binding-spec))
             ;; For the first keyword in a preferred sequence, set flag.
	     (if (equal todo-keyword preferred-sequence-head)
                 (setq in-preferred-sequence t))
             ;; Store the preferred todo keyword sequence.
	     (when in-preferred-sequence (push todo-binding-spec preferred-todo-alist))
             ;; Assign face to the todo keyword.
	     (setq todo-keyword
                   (org-add-props
                       todo-keyword nil
                     'face (org-get-todo-face todo-keyword)))
	     (when (= field-number 0) (insert "  "))
	     (setq prompt (concat prompt "[" (char-to-string todo-char) "] " todo-keyword " "))
	     (insert "[" todo-char "] " todo-keyword
                     ;; Fill spaces up to FIELD-WIDTH.
                     (make-string
		      (- field-width 4 (length todo-keyword)) ?\ ))
             ;; Last column in the row.
	     (when (and (= (setq field-number (1+ field-number))
                           (/ (- (window-width) 4) field-width))
		        ;; Avoid lines with just a closing delimiter.
		        (not (equal (car todo-alist-tail) '(:endgroup))))
	       (insert "\n")
	       (setq field-number 0)))))
	(insert "\n")
	(goto-char (point-min))
	(unless expert-interface (org-fit-window-to-buffer))
	(message (concat "[a-z..]:Set [SPC]:clear"
			 (if expert-interface (concat "\n" prompt) "")))
        ;; Read the todo keyword input and exit.
	(setq input-char
              (let ((inhibit-quit t)) ; intercept C-g.
                (read-char-exclusive)))
        ;; Restore the original keyword order.  Previously, it was reversed using `push'.
	(setq preferred-todo-alist (nreverse preferred-todo-alist))
	(cond
	 ((equal input-char ?\s) nil)
         ((or (= input-char ?\C-g)
	      (and (= input-char ?q) (not (rassoc input-char todo-alist))))
          (signal 'quit nil))
	 ((setq todo-binding-spec (or
                                   ;; Prefer bindings from todo sequence containing CURRENT-TODO-KEYWORD.
                                   (rassoc input-char preferred-todo-alist)
                                   (rassoc input-char todo-alist))
	        todo-keyword (car todo-binding-spec))
	  todo-keyword)
         (t (signal 'quit nil)))))))

(defun org-entry-is-todo-p ()
  (member (org-get-todo-state) org-not-done-keywords))

(defun org-entry-is-done-p ()
  (member (org-get-todo-state) org-done-keywords))

(defun org-get-todo-state ()
  "Return the TODO keyword of the current subtree."
  (save-excursion
    (org-back-to-heading t)
    (and (let ((case-fold-search nil))
           (looking-at org-todo-line-regexp))
	 (match-end 2)
	 (match-string 2))))

(defun org-at-date-range-p (&optional inactive-ok)
  "Non-nil if point is inside a date range.

When optional argument INACTIVE-OK is non-nil, also consider
inactive time ranges.

When this function returns a non-nil value, match data is set
according to `org-tr-regexp-both' or `org-tr-regexp', depending
on INACTIVE-OK."
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^[<\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<[\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

(defun org-get-repeat (&optional timestamp)
  "Check if there is a timestamp with repeater in this entry.

Return the repeater, as a string, or nil.  Also return nil when
this function is called before first heading.

When optional argument TIMESTAMP is a string, extract the
repeater from there instead."
  (save-match-data
    (cond
     (timestamp
      (and (string-match org-repeat-re timestamp)
	   (match-string-no-properties 1 timestamp)))
     ((org-before-first-heading-p) nil)
     (t
      (save-excursion
	(org-back-to-heading t)
	(let ((end (org-entry-end-position)))
	  (catch :repeat
	    (while (re-search-forward org-repeat-re end t)
	      (when (save-match-data (org-at-timestamp-p 'agenda))
		(throw :repeat (match-string-no-properties 1)))))))))))

(defvar org-last-changed-timestamp)
(defvar org-last-inserted-timestamp)
(defvar org-log-post-message)
(defvar org-log-note-purpose)
(defvar org-log-note-how nil)
(defvar org-log-note-extra)
(defvar org-log-setup nil)
(defun org-auto-repeat-maybe (done-word)
  "Check if the current headline contains a repeated timestamp.

If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.

This function is run automatically after each state change to a DONE state."
  (let* ((repeat (org-get-repeat))
	 (aa (assoc org-last-state org-todo-kwd-alist))
	 (interpret (nth 1 aa))
	 (head (nth 2 aa))
	 (msg "Entry repeats: ")
	 (org-log-done nil)
	 (org-todo-log-states nil)
	 (end (copy-marker (org-entry-end-position))))
    (when (and repeat (not (= 0 (string-to-number (substring repeat 1)))))
      (when (eq org-log-repeat t) (setq org-log-repeat 'state))
      (let ((to-state
             (or (org-entry-get nil "REPEAT_TO_STATE" 'selective)
		 (and (stringp org-todo-repeat-to-state)
		      org-todo-repeat-to-state)
		 (and org-todo-repeat-to-state org-last-state))))
	(org-todo (cond ((and to-state (member to-state org-todo-keywords-1))
			 to-state)
			((eq interpret 'type) org-last-state)
			(head)
			(t 'none))))
      (org-back-to-heading t)
      (org-add-planning-info nil nil 'closed)
      ;; When `org-log-repeat' is non-nil or entry contains
      ;; a clock, set LAST_REPEAT property.
      (when (or org-log-repeat
		(catch :clock
		  (save-excursion
		    (while (re-search-forward org-clock-line-re end t)
		      (when (org-at-clock-log-p) (throw :clock t))))))
	(org-entry-put nil "LAST_REPEAT" (format-time-string
					(org-time-stamp-format t t)
                                        (org-current-effective-time))))
      (when org-log-repeat
	(if org-log-setup
	    ;; We are already setup for some record.
	    (when (eq org-log-repeat 'note)
	      ;; Make sure we take a note, not only a time stamp.
	      (setq org-log-note-how 'note))
	  ;; Set up for taking a record.
	  (org-add-log-setup 'state
			     (or done-word (car org-done-keywords))
			     org-last-state
			     org-log-repeat)))
      ;; Timestamps without a repeater are usually skipped.  However,
      ;; a SCHEDULED timestamp without one is removed, as they are no
      ;; longer relevant.
      (save-excursion
	(let* ((headline (org-element-lineage
                          (org-element-at-point)
                          (list 'headline 'inlinetask) t))
               (scheduled (org-element-property
                           :scheduled
                           headline))
               (repeater-unit (org-element-property
                               :repeater-unit
                               scheduled))
               (repeater-value (org-element-property
                                :repeater-value
                                scheduled))
               (repeater-type (org-element-property
                               :repeater-type
                               scheduled))
               (has-valid-repeater (and repeater-unit
                                        repeater-value
                                        repeater-type)))
	  (when (and scheduled (not has-valid-repeater))
	    (org-remove-timestamp-with-keyword org-scheduled-string 'planning))))
      ;; Update every timestamp with a repeater in the entry.
      (let ((planning-re (regexp-opt
			  (list org-scheduled-string org-deadline-string))))
	(while (re-search-forward org-repeat-re end t)
	  (let* ((timestamp (save-match-data
                              (save-excursion
                                (goto-char (match-beginning 0))
                                (org-element-timestamp-parser))))
                 (repeater-type (org-element-property
                                 :repeater-type
                                 timestamp))
                 (repeater-unit (org-element-property
                                 :repeater-unit
                                 timestamp))
                 (repeater-value (org-element-property
                                  :repeater-value
                                  timestamp))
                 (has-start-time (and (org-element-property
                                       :hour-start
                                       timestamp)
                                      (org-element-property
                                       :minute-start
                                       timestamp)))
                 (time (org-timestamp-to-time timestamp))
                 (time-in-seconds (float-time time))
		 (type (if (not (org-at-planning-p)) "Plain:"
			 (save-excursion
			   (re-search-backward
			    planning-re (line-beginning-position) t)
			   (match-string 0)))))
	    (when (and (org-at-timestamp-p 'agenda)
		       repeater-unit
                       repeater-value)
	      (when (equal repeater-unit `week)
                (setq repeater-value (* repeater-value 7)
                      repeater-unit 'day))
	      (when (and (equal repeater-unit `hour)
			 (not has-start-time))
		(user-error
		 "Cannot repeat in %d hour(s) because no hour has been set"
		 repeater-value))
	      ;; Preparation, see if we need to modify the start
	      ;; date for the change.
	      (cond
	       ((equal `restart repeater-type)
		;; Shift starting date to today, or now if
		;; repeater is by hours.
		(if (equal repeater-unit `hour)
		    (org-timestamp-change
		     (floor (- (- time-in-seconds (float-time))) 60) 'minute)
		  (org-timestamp-change
		   (- (org-today) (time-to-days time)) 'day)))
	       ((equal `catch-up repeater-type)
		(let ((nshiftmax 10)
		      (nshift 0))
		  (while (or (= nshift 0)
			     (if (equal repeater-unit `hour)
				 (not (time-less-p nil time))
			       (>= (org-today)
				  (time-to-days time))))
		    (when (= nshiftmax (cl-incf nshift))
		      (or (y-or-n-p
			   (format "%d repeater intervals were not \
enough to shift date past today.  Continue? "
				   nshift))
			  (user-error "Abort")))
		    (org-timestamp-change repeater-value repeater-unit)
		    (org-in-regexp org-ts-regexp3)
		    (setq timestamp (save-match-data
                                      (save-excursion
                                        (goto-char (match-beginning 0))
                                        (org-element-timestamp-parser))))
		    (setq time
			  (save-match-data
			    (org-timestamp-to-time timestamp)))))
		(org-timestamp-change (- repeater-value) repeater-unit)
		;; Rematch, so that we have everything in place
		;; for the real shift.
		(org-in-regexp org-ts-regexp3)))
	      (save-excursion
		(org-timestamp-change repeater-value repeater-unit nil t))
	      (setq msg
		    (concat msg type " " org-last-changed-timestamp " "))))))
      (save-excursion
        (run-hooks 'org-todo-repeat-hook))
      (setq org-log-post-message msg)
      (message msg))))

(defun org--deadline-or-schedule (arg type time)
  "Insert DEADLINE or SCHEDULE information in current entry.
TYPE is either `deadline' or `scheduled'.  See `org-deadline' or
`org-schedule' for information about ARG and TIME arguments."
  (org-fold-core-ignore-modifications
    (let* ((deadline? (eq type 'deadline))
	   (keyword (if deadline? org-deadline-string org-scheduled-string))
	   (log (if deadline? org-log-redeadline org-log-reschedule))
	   (old-date (org-entry-get nil (if deadline? "DEADLINE" "SCHEDULED")))
	   (old-date-time (and old-date (org-time-string-to-time old-date)))
	   ;; Save repeater cookie from either TIME or current scheduled
	   ;; time stamp.  We are going to insert it back at the end of
	   ;; the process.
	   (repeater (or (and (org-string-nw-p time)
			      ;; We use `org-ts-regexp-both' because we
			      ;; need to tell the difference between a
			      ;; real repeater and a time delta, e.g.
			      ;; "+2d".
                              (string-match-p org-ts-regexp-both time)
                              (string-match "\\([.+-]+[0-9]+[hdwmy]\
\\(?:[/ ][-+]?[0-9]+[hdwmy]\\)?\\)"
					    time)
			      (match-string 1 time))
		         (and (org-string-nw-p old-date)
			      (string-match "\\([.+-]+[0-9]+[hdwmy]\
\\(?:[/ ][-+]?[0-9]+[hdwmy]\\)?\\)"
					    old-date)
			      (match-string 1 old-date)))))
      (pcase arg
        (`(4)
         (if (not old-date)
	     (message (if deadline? "Entry had no deadline to remove"
		        "Entry was not scheduled"))
	   (when (and old-date log)
	     (org-add-log-setup (if deadline? 'deldeadline 'delschedule)
			        nil old-date log))
	   (org-remove-timestamp-with-keyword keyword 'planning)
	   (message (if deadline? "Entry no longer has a deadline."
		      "Entry is no longer scheduled."))))
        (`(16)
         (save-excursion
	   (org-back-to-heading t)
	   (let ((regexp (if deadline? org-deadline-time-regexp
			   org-scheduled-time-regexp)))
	     (if (not (re-search-forward regexp (line-end-position 2) t))
	         (user-error (if deadline? "No deadline information to update"
			       "No scheduled information to update"))
	       (let* ((rpl0 (match-string 1))
		      (rpl (replace-regexp-in-string " -[0-9]+[hdwmy]" "" rpl0))
		      (msg (if deadline? "Warn starting from" "Delay until")))
	         (replace-match
		  (concat keyword
			  " <" rpl
			  (format " -%dd"
				  (abs (- (time-to-days
					   (save-match-data
					     (org-read-date
					      nil t nil msg old-date-time)))
					  (time-to-days old-date-time))))
			  ">") t t))))))
        (_
         (org-add-planning-info type time 'closed)
         (when (and old-date
		    log
		    (not (equal old-date org-last-inserted-timestamp)))
	   (org-add-log-setup (if deadline? 'redeadline 'reschedule)
			      org-last-inserted-timestamp
			      old-date
			      log))
         (when repeater
	   (save-excursion
	     (org-back-to-heading t)
	     (when (re-search-forward
		    (concat keyword " " org-last-inserted-timestamp)
		    (line-end-position 2)
		    t)
	       (goto-char (1- (match-end 0)))
	       (insert-and-inherit " " repeater)
	       (setq org-last-inserted-timestamp
		     (concat (substring org-last-inserted-timestamp 0 -1)
			     " " repeater
			     (substring org-last-inserted-timestamp -1))))))
         (message (if deadline? "Deadline on %s" "Scheduled to %s")
		  org-last-inserted-timestamp))))))

(defun org-deadline (arg &optional time)
  "Insert a \"DEADLINE:\" string with a timestamp to make a deadline.

When called interactively, this command pops up the Emacs calendar to let
the user select a date.

With one universal prefix argument, remove any deadline from the item.
With two universal prefix arguments, prompt for a warning delay.
With argument TIME, set the deadline at the corresponding date.  TIME
can either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P" org-mode)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (org-map-entries
       (lambda () (org--deadline-or-schedule arg 'deadline time))
       nil
       (if (eq org-loop-over-headlines-in-active-region 'start-level)
	   'region-start-level
	 'region)
       (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))
    (org--deadline-or-schedule arg 'deadline time)))

(defun org-schedule (arg &optional time)
  "Insert a \"SCHEDULED:\" string with a timestamp to schedule an item.

When called interactively, this command pops up the Emacs calendar to let
the user select a date.

With one universal prefix argument, remove any scheduling date from the item.
With two universal prefix arguments, prompt for a delay cookie.
With argument TIME, scheduled at the corresponding date.  TIME can
either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P" org-mode)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (org-map-entries
       (lambda () (org--deadline-or-schedule arg 'scheduled time))
       nil
       (if (eq org-loop-over-headlines-in-active-region 'start-level)
	   'region-start-level
	 'region)
       (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))
    (org--deadline-or-schedule arg 'scheduled time)))

(defun org-get-scheduled-time (pom &optional inherit)
  "Get the scheduled time as a time tuple, of a format suitable
for calling org-schedule with, or if there is no scheduling,
returns nil."
  (let ((time (org-entry-get pom "SCHEDULED" inherit)))
    (when time
      (org-time-string-to-time time))))

(defun org-get-deadline-time (pom &optional inherit)
  "Get the deadline as a time tuple, of a format suitable for
calling org-deadline with, or if there is no scheduling, returns
nil."
  (let ((time (org-entry-get pom "DEADLINE" inherit)))
    (when time
      (org-time-string-to-time time))))

(defun org-remove-timestamp-with-keyword (keyword &optional planning)
  "Remove all time stamps with KEYWORD in the current entry.
When PLANNING is non-nil, only remove on planning line."
  (let ((re (concat "\\<" (regexp-quote keyword) " +<[^>\n]+>[ \t]*"))
	beg)
    (save-excursion
      (org-back-to-heading t)
      (if planning
          (progn
            (forward-line)
            (when (looking-at-p org-planning-line-re)
              (setq beg (point))
              (forward-line)))
        (setq beg (point))
        (outline-next-heading))
      (while (re-search-backward re beg t)
	(replace-match "")
        (if (and (string-match "\\S-" (buffer-substring (line-beginning-position) (point)))
		 (equal (char-before) ?\ ))
	    (delete-char -1)
	  (when (string-match "^[ \t]*$" (buffer-substring
                                          (line-beginning-position) (line-end-position)))
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position))))))))))

(defvar org-time-was-given) ; dynamically scoped parameter
(defvar org-end-time-was-given) ; dynamically scoped parameter

(defun org-add-planning-info (what &optional time &rest remove)
  "Insert new timestamp with keyword in the planning line.
WHAT indicates what kind of time stamp to add.  It is a symbol
among `closed', `deadline', `scheduled' and nil.  TIME indicates
the time to use.  If none is given, the user is prompted for
a date.  REMOVE indicates what kind of entries to remove.  An old
WHAT entry will also be removed."
  (org-fold-core-ignore-modifications
    (let (org-time-was-given org-end-time-was-given default-time default-input)
      (when (and (memq what '(scheduled deadline))
	         (or (not time)
		     (and (stringp time)
			  (string-match "^[-+]+[0-9]" time))))
        ;; Try to get a default date/time from existing timestamp
        (save-excursion
	  (org-back-to-heading t)
	  (let ((end (save-excursion (outline-next-heading) (point))) ts)
	    (when (re-search-forward (if (eq what 'scheduled)
				         org-scheduled-time-regexp
				       org-deadline-time-regexp)
				     end t)
              (setq ts (match-string 1))
              (when (org-element-type-p (org-element-at-point) 'planning)
	        (setq default-time (org-time-string-to-time ts)
		      default-input (and ts (org-get-compact-tod ts))))))))
      (when what
        (setq time
	      (if (stringp time)
		  ;; This is a string (relative or absolute), set
		  ;; proper date.
		  (org-encode-time
		   (org-read-date-analyze
		    time default-time (decode-time default-time)))
	        ;; If necessary, get the time from the user
	        (or time (org-read-date nil 'to-time nil
				        (cl-case what
				          (deadline "DEADLINE")
				          (scheduled "SCHEDULED")
				          (otherwise nil))
				        default-time default-input)))))
      (org-with-wide-buffer
       (org-back-to-heading t)
       (let ((planning? (save-excursion
			  (forward-line)
			  (looking-at-p org-planning-line-re))))
         (cond
	  (planning?
	   (forward-line)
	   ;; Move to current indentation.
	   (skip-chars-forward " \t")
	   ;; Check if we have to remove something.
	   (dolist (type (if what (cons what remove) remove))
	     (save-excursion
	       (when (re-search-forward
		      (cl-case type
		        (closed org-closed-time-regexp)
		        (deadline org-deadline-time-regexp)
		        (scheduled org-scheduled-time-regexp)
		        (otherwise (error "Invalid planning type: %s" type)))
		      (line-end-position)
		      t)
	         ;; Delete until next keyword or end of line.
	         (delete-region
		  (match-beginning 0)
		  (if (re-search-forward org-keyword-time-not-clock-regexp
				         (line-end-position)
				         t)
		      (match-beginning 0)
		    (line-end-position))))))
	   ;; If there is nothing more to add and no more keyword is
	   ;; left, remove the line completely.
	   (if (and (looking-at-p "[ \t]*$") (not what))
	       (delete-region (line-end-position 0)
			      (line-end-position))
	     ;; If we removed last keyword, do not leave trailing white
	     ;; space at the end of line.
	     (let ((p (point)))
	       (save-excursion
	         (end-of-line)
	         (unless (= (skip-chars-backward " \t" p) 0)
		   (delete-region (point) (line-end-position)))))))
	  (what
	   (end-of-line)
	   (insert-and-inherit "\n")
	   (when org-adapt-indentation
	     (indent-to-column (1+ (org-outline-level)))))
	  (t nil)))
       (when what
         ;; Insert planning keyword.
         (insert-and-inherit (cl-case what
		               (closed org-closed-string)
		               (deadline org-deadline-string)
		               (scheduled org-scheduled-string)
		               (otherwise (error "Invalid planning type: %s" what)))
	                     " ")
         ;; Insert associated timestamp.
         (let ((ts (org-insert-timestamp
		    time
		    (or org-time-was-given
		        (and (eq what 'closed) org-log-done-with-time))
		    (eq what 'closed)
		    nil nil (list org-end-time-was-given))))
	   (unless (eolp) (insert " "))
	   ts))))))

(defun org-remove-empty-drawer-at (pos)
  "Remove an empty drawer at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (org-with-wide-buffer
     (goto-char pos)
     (let ((drawer (org-element-at-point)))
       (when (and (org-element-type-p drawer '(drawer property-drawer))
		  (not (org-element-contents-begin drawer)))
	 (delete-region (org-element-begin drawer)
			(progn (goto-char (org-element-end drawer))
			       (skip-chars-backward " \r\t\n")
			       (forward-line)
			       (point))))))))

;;;; Tags
(defalias 'org-tags-sparse-tree 'org-match-sparse-tree)

(defun org-insert-drawer (&optional arg drawer)
  "Insert a drawer at point.

When optional argument ARG is non-nil, insert a property drawer.

Optional argument DRAWER, when non-nil, is a string representing
drawer's name.  Otherwise, the user is prompted for a name.

If a region is active, insert the drawer around that region
instead.

Point is left between drawer's boundaries."
  (interactive "P" org-mode)
  (let* ((drawer (if arg "PROPERTIES"
		   (or drawer (read-from-minibuffer "Drawer: ")))))
    (cond
     ;; With C-u, fall back on `org-insert-property-drawer'
     (arg
      (org-insert-property-drawer)
      (org-back-to-heading-or-point-min t)
      ;; Move inside.
      (re-search-forward org-property-end-re)
      (forward-line 0)
      (unless (org-element-contents-begin (org-element-at-point))
        ;; Empty drawer.
        (insert "\n")
        (forward-char -1))
      (org-reveal))
     ;; Check validity of suggested drawer's name.
     ((not (string-match-p org-drawer-regexp (format ":%s:" drawer)))
      (user-error "Invalid drawer name"))
     ;; With an active region, insert a drawer at point.
     ((not (org-region-active-p))
      (progn
	(unless (bolp) (insert "\n"))
	(insert (format ":%s:\n\n:END:\n" drawer))
	(forward-line -2)))
     ;; Otherwise, insert the drawer at point
     (t
      (let ((rbeg (region-beginning))
	    (rend (copy-marker (region-end))))
	(unwind-protect
	    (progn
	      (goto-char rbeg)
	      (forward-line 0)
	      (when (save-excursion
		      (re-search-forward org-outline-regexp-bol rend t))
		(user-error "Drawers cannot contain headlines"))
	      ;; Position point at the beginning of the first
	      ;; non-blank line in region.  Insert drawer's opening
	      ;; there, then indent it.
	      (org-skip-whitespace)
	      (forward-line 0)
	      (insert ":" drawer ":\n")
	      (forward-line -1)
	      (indent-for-tab-command)
	      ;; Move point to the beginning of the first blank line
	      ;; after the last non-blank line in region.  Insert
	      ;; drawer's closing, then indent it.
	      (goto-char rend)
	      (skip-chars-backward " \r\t\n")
	      (insert "\n:END:")
	      (deactivate-mark t)
	      (indent-for-tab-command)
	      (unless (eolp) (insert "\n"))
              ;; Leave point inside drawer boundaries.
              (search-backward ":END:")
              (forward-char -1))
	  ;; Clear marker, whatever the outcome of insertion is.
	  (set-marker rend nil)))))))

(defun org-find-olp (path &optional this-buffer)
  "Return a marker pointing to the entry at outline path OLP.
If anything goes wrong, throw an error, and if you need to do
something based on this error, you can catch it with
`condition-case'.

If THIS-BUFFER is set, the outline path does not contain a file,
only headings."
  (let* ((file (if this-buffer buffer-file-name (pop path)))
	 (buffer (if this-buffer (current-buffer) (find-file-noselect file)))
	 (level 1)
	 (lmin 1)
	 (lmax 1)
	 end found flevel)
    (unless buffer (error "File not found :%s" file))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
	(error "Buffer %s needs to be in Org mode" buffer))
      (org-with-wide-buffer
       (goto-char (point-min))
       (dolist (heading path)
	 (let ((re (format org-complex-heading-regexp-format
			   (regexp-quote heading)))
	       (cnt 0))
	   (while (re-search-forward re end t)
	     (setq level (- (match-end 1) (match-beginning 1)))
	     (when (and (>= level lmin) (<= level lmax))
	       (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
	   (when (= cnt 0)
	     (error "Heading not found on level %d: %s" lmax heading))
	   (when (> cnt 1)
	     (error "Heading not unique on level %d: %s" lmax heading))
	   (goto-char found)
	   (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
	   (setq end (save-excursion (org-end-of-subtree t t)))))
       (when (org-at-heading-p)
	 (point-marker))))))

(defun org-find-exact-headline-in-buffer (heading &optional buffer pos-only)
  "Find node HEADING in BUFFER.
Return a marker to the heading if it was found, or nil if not.
If POS-ONLY is set, return just the position instead of a marker.

The heading text must match exact, but it may have a TODO keyword,
a priority cookie and tags in the standard locations."
  (with-current-buffer (or buffer (current-buffer))
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (case-fold-search)
       (when (re-search-forward
	      (format org-complex-heading-regexp-format
		      (regexp-quote heading)) nil t)
	 (if pos-only
	     (match-beginning 0)
	   (move-marker (make-marker) (match-beginning 0))))))))

(defun org-find-exact-heading-in-directory (heading &optional dir)
  "Find Org node headline HEADING in all \".org\" files in directory DIR.
When the target headline is found, return a marker to this location."
  (let ((files (directory-files (or dir default-directory)
				t "\\`[^.#].*\\.org\\'"))
	visiting m buffer)
    (catch 'found
      (dolist (file files)
        (message "trying %s" file)
        (setq visiting (org-find-base-buffer-visiting file))
        (setq buffer (or visiting (find-file-noselect file)))
        (setq m (org-find-exact-headline-in-buffer
                 heading buffer))
        (when (and (not m) (not visiting)) (kill-buffer buffer))
        (and m (throw 'found m))))))

(defun org-find-entry-with-id (ident)
  "Locate the entry that contains the ID property with exact value IDENT.
IDENT can be a string, a symbol or a number, this function will search for
the string representation of it.
Return the position where this entry starts, or nil if there is no such entry."
  (interactive "sID: " org-mode)
  (let ((id (cond
	     ((stringp ident) ident)
	     ((symbolp ident) (symbol-name ident))
	     ((numberp ident) (number-to-string ident))
	     (t (error "IDENT %s must be a string, symbol or number" ident)))))
    (org-with-wide-buffer (org-find-property "ID" id))))

;;;; Timestamps

(defun org-fix-decoded-time (time)
  "Set 0 instead of nil for the first 6 elements of time.
Don't touch the rest."
  (let ((n 0))
    (mapcar (lambda (x) (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))

(defalias 'org-time-stamp-to-now #'org-timestamp-to-now)
(defun org-timestamp-to-now (timestamp-string &optional seconds)
  "Difference between TIMESTAMP-STRING and now in days.
If SECONDS is non-nil, return the difference in seconds."
  (let ((fdiff (if seconds #'float-time #'time-to-days)))
    (- (funcall fdiff (org-time-string-to-time timestamp-string))
       (funcall fdiff nil))))

(defun org-deadline-close-p (timestamp-string &optional ndays)
  "Is the time in TIMESTAMP-STRING close to the current date?"
  (setq ndays (or ndays (org-get-wdays timestamp-string)))
  (and (<= (org-timestamp-to-now timestamp-string) ndays)
       (not (org-entry-is-done-p))))

(defun org-get-wdays (ts &optional delay zero-delay)
  "Get the deadline lead time appropriate for timestring TS.
When DELAY is non-nil, get the delay time for scheduled items
instead of the deadline lead time.  When ZERO-DELAY is non-nil
and `org-scheduled-delay-days' is 0, enforce 0 as the delay,
don't try to find the delay cookie in the scheduled timestamp."
  (let ((tv (if delay org-scheduled-delay-days
	      org-deadline-warning-days)))
    (cond
     ((or (and delay (< tv 0))
	  (and delay zero-delay (<= tv 0))
	  (and (not delay) (<= tv 0)))
      ;; Enforce this value no matter what
      (- tv))
     ((string-match "-\\([0-9]+\\)\\([hdwmy]\\)\\(\\'\\|[ >]\\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25)
			      ("h" . 0.041667)))))))
     ;; go for the default.
     (t tv))))

(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P" org-mode)
  (or
   (org-clock-update-time-maybe)
   (save-excursion
     (unless (org-at-date-range-p t)
       (goto-char (line-beginning-position))
       (re-search-forward org-tr-regexp-both (line-end-position) t))
     (unless (org-at-date-range-p t)
       (user-error "Not at a timestamp range, and none found in current line")))
   (let* ((ts1 (match-string 1))
	  (ts2 (match-string 2))
	  (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	  (match-end (match-end 0))
	  (time1 (org-time-string-to-time ts1))
	  (time2 (org-time-string-to-time ts2))
	  (diff (abs (float-time (time-subtract time2 time1))))
	  (negative (time-less-p time2 time1))
	  ;; (ys (floor (* 365 24 60 60)))
	  (ds (* 24 60 60))
	  (hs (* 60 60))
	  (fy "%dy %dd %02d:%02d")
	  (fy1 "%dy %dd")
	  (fd "%dd %02d:%02d")
	  (fd1 "%dd")
	  (fh "%02d:%02d")
	  y d h m align)
     (if havetime
	 (setq ; y (floor diff ys)  diff (mod diff ys)
	  y 0
	  d (floor diff ds)  diff (mod diff ds)
	  h (floor diff hs)  diff (mod diff hs)
	  m (floor diff 60))
       (setq ; y (floor diff ys)  diff (mod diff ys)
	y 0
	d (round diff ds)
	h 0 m 0))
     (if (not to-buffer)
	 (message "%s" (org-make-tdiff-string y d h m))
       (if (org-at-table-p)
	   (progn
	     (goto-char match-end)
	     (setq align t)
	     (and (looking-at " *|") (goto-char (match-end 0))))
	 (goto-char match-end))
       (when (looking-at
	      "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
	 (replace-match ""))
       (when negative (insert " -"))
       (if (> y 0) (insert " " (format (if havetime fy fy1) y d h m))
	 (if (> d 0) (insert " " (format (if havetime fd fd1) d h m))
	   (insert " " (format fh h m))))
       (when align (org-table-align))
       (message "Time difference inserted")))))

(defvar org-agenda-current-date)
(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org mode."
  (require 'holidays)
  (let ((hl (calendar-check-holidays org-agenda-current-date)))
    (and hl (mapconcat #'identity hl "; "))))

(defun org-diary-to-ical-string (frombuf)
  "Get iCalendar entries from diary entries in buffer FROMBUF.
This uses the icalendar.el library."
  (let* ((tmpdir temporary-file-directory)
	 (tmpfile (make-temp-name
		   (expand-file-name "orgics" tmpdir)))
	 buf rtn b e)
    (unwind-protect
        (with-current-buffer frombuf
          (if (fboundp 'diary-icalendar-export-region)
              (diary-icalendar-export-region (point-min) (point-max) tmpfile)
            ;; Emacs < 31
            (with-suppressed-warnings ((obsolete icalendar-export-region))
              (icalendar-export-region (point-min) (point-max) tmpfile)))
          (setq buf (find-buffer-visiting tmpfile))
          (set-buffer buf)
          (goto-char (point-min))
          (when (re-search-forward "^BEGIN:VEVENT" nil t)
	    (setq b (match-beginning 0)))
          (goto-char (point-max))
          (when (re-search-backward "^END:VEVENT" nil t)
	    (setq e (match-end 0)))
          (setq rtn (if (and b e) (concat (buffer-substring b e) "\n") "")))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf))
      (delete-file tmpfile))
    rtn))

(defun org-recenter-calendar (d)
  "If the calendar is visible, recenter it to date D."
  (let ((cwin (get-buffer-window calendar-buffer t)))
    (when cwin
      (let ((calendar-move-hook nil))
	(with-selected-window cwin
	  (calendar-goto-date
	   (if (listp d) d (calendar-gregorian-from-absolute d))))))))

(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used to force the current date."
  (interactive "P")
  (let ((calendar-move-hook nil)
	(calendar-view-holidays-initially-flag nil)
	(calendar-view-diary-initially-flag nil)
	diff)
    (when (or (org-at-timestamp-p 'lax)
	      (org-match-line (concat ".*" org-ts-regexp)))
      (let ((d1 (time-to-days nil))
	    (d2 (time-to-days (org-time-string-to-time (match-string 1)))))
	(setq diff (- d2 d1))))
    (calendar)
    (calendar-goto-today)
    (when (and diff (not arg)) (calendar-forward-day diff))))

(defun org-get-date-from-calendar ()
  "Return a list (month day year) of date at point in calendar."
  (with-current-buffer calendar-buffer
    (save-match-data
      (calendar-cursor-to-date))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive nil org-mode)
  (if (org-at-timestamp-p 'lax)
      (org-timestamp-change 0 'calendar)
    (let ((cal-date (org-get-date-from-calendar)))
      (org-insert-timestamp
       (org-encode-time
        0 0 0
        (calendar-extract-day cal-date)
        (calendar-extract-month cal-date)
        (calendar-extract-year cal-date))))))

(defcustom org-image-actual-width t
  "When non-nil, use the actual width of images when inlining them.

When set to a number, use imagemagick (when available) to set the
image's width to this value.

When set to a number in a list, try to get the width from any
#+ATTR.* keyword if it matches a width specification like

  #+ATTR_HTML: :width 300px

and fall back on that number if none is found.

When set to nil, first try to get the width from #+ATTR_ORG.  If
that is not found, use the first #+ATTR_xxx :width specification.
If that is also not found, fall back on the original image width.

Finally, Org mode is quite flexible in the width specifications it
supports and intelligently interprets width specifications for other
backends when rendering an image in an org buffer.  This behavior is
described presently.

1. A floating point value between 0 and 2 is interpreted as the
   percentage of the text area that should be taken up by the image.
2. A number followed by a percent sign is divided by 100 and then
   interpreted as a floating point value.
3. If a number is followed by other text, extract the number and
   discard the remaining text.  That number is then interpreted as a
   floating-point value.  For example,

   #+ATTR_LATEX: :width 0.7\\linewidth

   would be interpreted as 70% of the text width.
4. If t is provided the original image width is used.  This is useful
   when you want to specify a width for a backend, but still want to
   use the original image width in the org buffer.

This requires Emacs >= 24.1, built with imagemagick support."
  :group 'org-appearance
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Use the image width" t)
	  (integer :tag "Use a number of pixels")
	  (list :tag "Use #+ATTR* or a number of pixels" (integer))
	  (const :tag "Use #+ATTR* or don't resize" nil)))

(defcustom org-agenda-inhibit-startup nil
  "Inhibit startup when preparing agenda buffers.
When this variable is t, the initialization of the Org agenda
buffers is inhibited: e.g. the visibility state is not set, the
tables are not re-aligned, etc."
  :type 'boolean
  :version "24.3"
  :group 'org-agenda)

(defcustom org-agenda-ignore-properties nil
  "Avoid updating text properties when building the agenda.
Properties are used to prepare buffers for effort estimates,
appointments, statistics and subtree-local categories.
If you don't use these in the agenda, you can add them to this
list and agenda building will be a bit faster.
The value is a list, with symbol `stats'."
  :type '(set :greedy t
	      (const stats))
  :package-version '(Org . "9.7")
  :group 'org-agenda)

(defun org-agenda-prepare-buffers (files)
  "Create buffers for all agenda files, protect archived trees and comments."
  (interactive)
  (let ((inhibit-read-only t)
	(org-inhibit-startup org-agenda-inhibit-startup)
        ;; Do not refresh list of agenda files in the menu when
        ;; opening every new file.
        (org-agenda-file-menu-enabled nil))
    (setq org-tag-alist-for-agenda nil
	  org-tag-groups-alist-for-agenda nil)
    (dolist (file files)
      (catch 'nextfile
        (with-current-buffer
            (if (bufferp file)
                file
              (org-check-agenda-file file)
              (org-get-agenda-file-buffer file))
          (org-with-wide-buffer
	   (org-set-regexps-and-options 'tags-only)
	   (or (memq 'stats org-agenda-ignore-properties)
	       (org-refresh-stats-properties))
           (dolist (el org-todo-keywords-1)
             (unless (member el org-todo-keywords-for-agenda)
               (push el org-todo-keywords-for-agenda)))
           (dolist (el org-done-keywords)
             (unless (member el org-done-keywords-for-agenda)
               (push el org-done-keywords-for-agenda)))
	   (setq org-todo-keyword-alist-for-agenda
                 (org--tag-add-to-alist
		  org-todo-key-alist
                  org-todo-keyword-alist-for-agenda))
	   (setq org-tag-alist-for-agenda
		 (org--tag-add-to-alist
		  org-current-tag-alist
                  org-tag-alist-for-agenda))
	   ;; Merge current file's tag groups into global
	   ;; `org-tag-groups-alist-for-agenda'.
	   (when org-group-tags
	     (dolist (alist org-tag-groups-alist)
	       (let ((old (assoc (car alist) org-tag-groups-alist-for-agenda)))
		 (if old
		     (setcdr old (org-uniquify (append (cdr old) (cdr alist))))
		   (push alist org-tag-groups-alist-for-agenda)))))))))
    ;; Refresh the menu once after loading all the agenda buffers.
    (when org-agenda-file-menu-enabled
      (org-install-agenda-files-menu))))

;;;; LaTeX fragments

(defun org-splice-latex-header (tpl def-pkg pkg snippets-p &optional extra)
  "Fill a LaTeX header template TPL.
In the template, the following place holders will be recognized:

 [DEFAULT-PACKAGES]      \\usepackage statements for DEF-PKG
 [NO-DEFAULT-PACKAGES]   do not include DEF-PKG
 [PACKAGES]              \\usepackage statements for PKG
 [NO-PACKAGES]           do not include PKG
 [EXTRA]                 the string EXTRA
 [NO-EXTRA]              do not include EXTRA

For backward compatibility, if both the positive and the negative place
holder is missing, the positive one (without the \"NO-\") will be
assumed to be present at the end of the template.
DEF-PKG and PKG are assumed to be alists of options/packagename lists.
EXTRA is a string.

Note that setting `org-latex-default-packages-alist' to nil is equivalent
to [NO-DEFAULT-PACKAGES] and setting `org-latex-packages-alist' to nil is
equivalent to [NO-PACKAGES].

SNIPPETS-P indicates if this is run to create snippet images for HTML."
  (let (rpl (end ""))
    (if (string-match "^[ \t]*\\[\\(NO-\\)?DEFAULT-PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not def-pkg))
		      "" (org-latex-packages-to-string def-pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (when def-pkg (setq end (org-latex-packages-to-string def-pkg snippets-p))))

    (if (string-match "\\[\\(NO-\\)?PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not pkg))
		      "" (org-latex-packages-to-string pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (when pkg (setq end
		      (concat end "\n"
			      (org-latex-packages-to-string pkg snippets-p)))))

    (if (string-match "\\[\\(NO-\\)?EXTRA\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not extra))
		      "" (concat extra "\n"))
	      tpl (replace-match rpl t t tpl))
      (when (and extra (string-match "\\S-" extra))
	(setq end (concat end "\n" extra))))

    (if (string-match "\\S-" end)
	(concat tpl "\n" end)
      tpl)))

(defun org-latex-packages-to-string (pkg &optional snippets-p newline)
  "Turn an alist of packages into a string with the \\usepackage macros."
  (setq pkg (mapconcat (lambda(p)
			 (cond
			  ((stringp p) p)
			  ((and snippets-p (>= (length p) 3) (not (nth 2 p)))
			   (format "%% Package %s omitted" (cadr p)))
			  ((equal "" (car p))
			   (format "\\usepackage{%s}" (cadr p)))
			  (t
			   (format "\\usepackage[%s]{%s}"
				   (car p) (cadr p)))))
		       pkg
		       "\n"))
  (if newline (concat pkg "\n") pkg))


(defvar org-self-insert-command-undo-counter 0)
(defvar org-speed-command nil)

(defun org--speed-command-p ()
  "Return non-nil when current command is a speed command.
Set `org-speed-command' to the appropriate command as a side effect."
  (and org-use-speed-commands
       (let ((kv (this-command-keys-vector)))
	 (setq org-speed-command
	       (run-hook-with-args-until-success
		'org-speed-command-hook
		(make-string 1 (aref kv (1- (length kv)))))))))

(defun org-self-insert-command (N)
  "Like `self-insert-command', use `overwrite-mode' for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p" org-mode)
  (cond
   ((org--speed-command-p)
    (cond
     ((commandp org-speed-command)
      (setq this-command org-speed-command)
      (call-interactively org-speed-command))
     ((functionp org-speed-command)
      (funcall org-speed-command))
     ((consp org-speed-command)
      (eval org-speed-command t))
     (t (let (org-use-speed-commands)
	  (call-interactively #'org-self-insert-command)))))
   ((and
     (= N 1)
     (not (org-region-active-p))
     (org-at-table-p)
     (progn
       ;; Check if we blank the field, and if that triggers align.
       (and (featurep 'org-table)
	    org-table-auto-blank-field
	    (memq last-command
		  '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
	    (if (or (eq (char-after) ?\s) (looking-at "[^|\n]*  |"))
		;; Got extra space, this field does not determine
		;; column width.
		(let (org-table-may-need-update) (org-table-blank-field))
	      ;; No extra space, this field may determine column
	      ;; width.
	      (org-table-blank-field)))
       t)
     (looking-at "[^|\n]*  |"))
    ;; There is room for insertion without re-aligning the table.
    ;; Interactively, point should never be inside invisible regions
    (org-fold-core-suppress-folding-fix
      (self-insert-command N))
    (org-table-with-shrunk-field
     (save-excursion
       (skip-chars-forward "^|")
       ;; Do not delete last space, which is
       ;; `org-table-separator-space', but the regular space before
       ;; it.
       (delete-region (- (point) 2) (1- (point))))))
   (t
    (setq org-table-may-need-update t)
    ;; Interactively, point should never be inside invisible regions
    (org-fold-core-suppress-folding-fix
      (self-insert-command N)
      (when org-auto-align-tags (org-fix-tags-on-the-fly)))
    (when org-self-insert-cluster-for-undo
      (if (not (eq last-command 'org-self-insert-command))
	  (setq org-self-insert-command-undo-counter 1)
	(if (>= org-self-insert-command-undo-counter 20)
	    (setq org-self-insert-command-undo-counter 1)
	  (and (> org-self-insert-command-undo-counter 0)
	       buffer-undo-list (listp buffer-undo-list)
	       (not (cadr buffer-undo-list)) ; remove nil entry
	       (setcdr buffer-undo-list (cddr buffer-undo-list)))
	  (setq org-self-insert-command-undo-counter
		(1+ org-self-insert-command-undo-counter))))))))

(defun org-delete-backward-char (N)
  "Like `delete-backward-char', insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p" org-mode)
  (save-match-data
    (if (and (= N 1)
	     (not overwrite-mode)
	     (not (org-region-active-p))
	     (not (eq (char-before) ?|))
	     (save-excursion (skip-chars-backward " \t") (not (bolp)))
	     (looking-at-p ".*?|")
	     (org-at-table-p))
	(progn (forward-char -1) (org-delete-char 1))
      (funcall-interactively #'backward-delete-char N)
      (when org-auto-align-tags (org-fix-tags-on-the-fly)))))

(defun org-delete-char (N)
  "Like `delete-char', but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p" org-mode)
  (save-match-data
    (cond
     ((or (/= N 1)
	  (eq (char-after) ?|)
	  (save-excursion (skip-chars-backward " \t") (bolp))
	  (not (org-at-table-p)))
      (delete-char N)
      (when org-auto-align-tags (org-fix-tags-on-the-fly)))
     ((looking-at ".\\(.*?\\)|")
      (let* ((update? org-table-may-need-update)
	     (noalign (looking-at-p ".*?  |")))
	(delete-char 1)
	(org-table-with-shrunk-field
	 (save-excursion
	   ;; Last space is `org-table-separator-space', so insert
	   ;; a regular one before it instead.
	   (goto-char (- (match-end 0) 2))
	   (insert " ")))
	;; If there were two spaces at the end, this field does not
	;; determine the width of the column.
	(when noalign (setq org-table-may-need-update update?))))
     (t
      (delete-char N)))))

;; Make `delete-selection-mode' work with Org mode and Orgtbl mode
(put 'org-self-insert-command 'delete-selection
     (lambda ()
       (unless (org--speed-command-p)
         (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions)))))
(put 'orgtbl-self-insert-command 'delete-selection
     (lambda ()
       (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions))))
(put 'org-delete-char 'delete-selection 'supersede)
(put 'org-delete-backward-char 'delete-selection 'supersede)
(put 'org-yank 'delete-selection 'yank)
(put 'org-return 'delete-selection t)

;; Make `flyspell-mode' delay after some commands
(put 'org-self-insert-command 'flyspell-delayed t)
(put 'orgtbl-self-insert-command 'flyspell-delayed t)
(put 'org-delete-char 'flyspell-delayed t)
(put 'org-delete-backward-char 'flyspell-delayed t)

;; Make pabbrev-mode expand after Org mode commands
(put 'org-self-insert-command 'pabbrev-expand-after-command t)
(put 'orgtbl-self-insert-command 'pabbrev-expand-after-command t)

(defun org-transpose-words ()
  "Transpose words for Org.
This uses the `org-mode-transpose-word-syntax-table' syntax
table, which interprets characters in `org-emphasis-alist' as
word constituents."
  (interactive nil org-mode)
  (org-with-syntax-table org-mode-transpose-word-syntax-table
    (call-interactively #'transpose-words)))

(defvar org-ctrl-c-ctrl-c-hook nil
  "Hook for functions attaching themselves to \\`C-c C-c'.

This can be used to add additional functionality to the \\`C-c C-c'
key which executes context-dependent commands.  This hook is run
before any other test, while `org-ctrl-c-ctrl-c-final-hook' is
run after the last test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-ctrl-c-ctrl-c-final-hook nil
  "Hook for functions attaching themselves to \\`C-c C-c'.

This can be used to add additional functionality to the \\`C-c C-c'
key which executes context-dependent commands.  This hook is run
after any other test, while `org-ctrl-c-ctrl-c-hook' is run
before the first test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-tab-after-check-for-table-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that the cursor is not in a
table, but before checking if the cursor is in a headline or if global cycling
should be done.
If any function in this hook returns t, not other actions like visibility
cycling will be done.")

(defvar org-tab-after-check-for-cycling-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that not table field motion and
not visibility should be done because of current context.  This is probably
the place where a package like yasnippets can hook in.")

(defvar org-tab-before-tab-emulation-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after every other options for TAB have been exhausted, but
before indentation and \t insertion takes place.")

(defvar org-metaleft-hook nil
  "Hook for functions attaching themselves to `M-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaleft-final-hook nil
  "Hook for functions attaching themselves to `M-left'.
This one runs after all options have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaright-hook nil
  "Hook for functions attaching themselves to `M-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaright-final-hook nil
  "Hook for functions attaching themselves to `M-right'.
This one runs after all options have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaup-hook nil
  "Hook for functions attaching themselves to `M-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaup-final-hook nil
  "Hook for functions attaching themselves to `M-up'.
This one runs after all other options except
`org-drag-element-backward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metadown-hook nil
  "Hook for functions attaching themselves to `M-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metadown-final-hook nil
  "Hook for functions attaching themselves to `M-down'.
This one runs after all other options except
`org-drag-element-forward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaleft-hook nil
  "Hook for functions attaching themselves to `M-S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaleft-final-hook nil
  "Hook for functions attaching themselves to `M-S-left'.
This one runs after all other options have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaright-hook nil
  "Hook for functions attaching themselves to `M-S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaright-final-hook nil
  "Hook for functions attaching themselves to `M-S-right'.
This one runs after all other options have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaup-hook nil
  "Hook for functions attaching themselves to `M-S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaup-final-hook nil
  "Hook for functions attaching themselves to `M-S-up'.
This one runs after all other options except
`org-drag-line-backward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetadown-hook nil
  "Hook for functions attaching themselves to `M-S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetadown-final-hook nil
  "Hook for functions attaching themselves to `M-S-down'.
This one runs after all other options except
`org-drag-line-forward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metareturn-hook nil
  "Hook for functions attaching themselves to `M-RET'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-hook nil
  "Hook for functions attaching themselves to `S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-final-hook nil
  "Hook for functions attaching themselves to `S-up'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-hook nil
  "Hook for functions attaching themselves to `S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-final-hook nil
  "Hook for functions attaching themselves to `S-down'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-hook nil
  "Hook for functions attaching themselves to `S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-final-hook nil
  "Hook for functions attaching themselves to `S-left'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-hook nil
  "Hook for functions attaching themselves to `S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-final-hook nil
  "Hook for functions attaching themselves to `S-right'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")

(defun org-modifier-cursor-error ()
  "Throw an error, a modified cursor command was applied in wrong context."
  (user-error "This command is active in special context like tables, headlines or items"))

(defun org-shiftselect-error ()
  "Throw an error because Shift-Cursor command was applied in wrong context."
  (if (and (boundp 'shift-select-mode) shift-select-mode)
      (user-error "To use shift-selection with Org mode, customize `org-support-shift-select'")
    (user-error "This command works only in special context like headlines or timestamps")))

(defun org-call-for-shift-select (cmd)
  (let ((this-command-keys-shift-translated t))
    (call-interactively cmd)))

(defun org-shifttab (&optional arg)
  "Global visibility cycling or move to previous table field.
Call `org-table-previous-field' within a table.
When ARG is nil, cycle globally through visibility states.
When ARG is a numeric prefix, show contents of this level."
  (interactive "P" org-mode)
  (cond
   ((org-at-table-p) (call-interactively #'org-table-previous-field))
   ((integerp arg)
    (let ((arg2 (if org-odd-levels-only (1- (* 2 arg)) arg)))
      (message "Content view to level: %d" arg)
      (org-cycle-content (prefix-numeric-value arg2))
      (org-cycle-show-empty-lines t)
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))
   (t (call-interactively #'org-cycle-global))))

(defun org-shiftmetaleft ()
  "Promote subtree or delete table column.
Calls `org-promote-subtree', `org-outdent-item-tree', or
`org-table-delete-column', depending on context.  See the
individual commands for more information.

This function runs the functions in `org-shiftmetaleft-hook' one
by one as a first step, and exits immediately if a function from
the hook returns non-nil.  In the absence of a specific context,
the function also runs `org-shiftmetaleft-final-hook' using the
same logic."
  (interactive nil org-mode)
  (cond
   ((and (eq system-type 'darwin)
         (or (memq org-support-shift-select '(always except-timestamps everywhere))
             (and org-support-shift-select (org-region-active-p))))
    (org-call-for-shift-select 'backward-char))
   ((run-hook-with-args-until-success 'org-shiftmetaleft-hook))
   ((org-at-table-p) (call-interactively #'org-table-delete-column))
   ((org-at-heading-p) (call-interactively #'org-promote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively #'org-outdent-item-tree))
   ((run-hook-with-args-until-success 'org-shiftmetaleft-final-hook))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaright ()
  "Demote subtree or insert table column.
Calls `org-demote-subtree', `org-indent-item-tree', or
`org-table-insert-column', depending on context.  See the
individual commands for more information.

This function runs the functions in `org-shiftmetaright-hook' one
by one as a first step, and exits immediately if a function from
the hook returns non-nil.  In the absence of a specific context,
the function also runs `org-shiftmetaright-final-hook' using the
same logic."
  (interactive nil org-mode)
  (cond
   ((and (eq system-type 'darwin)
         (or (memq org-support-shift-select '(always except-timestamps everywhere))
             (and org-support-shift-select (org-region-active-p))))
    (org-call-for-shift-select 'forward-char))
   ((run-hook-with-args-until-success 'org-shiftmetaright-hook))
   ((org-at-table-p) (call-interactively #'org-table-insert-column))
   ((org-at-heading-p) (call-interactively #'org-demote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively #'org-indent-item-tree))
   ((run-hook-with-args-until-success 'org-shiftmetaright-final-hook))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaup (&optional _arg)
  "Drag the line at point up.
In a table, kill the current row.
On a clock timestamp, update the value of the timestamp like `S-<up>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point up.

This function runs the functions in `org-shiftmetaup-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function also runs `org-shiftmetaup-final-hook' using the same
logic."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaup-hook))
   ((org-at-table-p) (call-interactively #'org-table-kill-row))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
			   (call-interactively #'org-timestamp-up)))
   ((run-hook-with-args-until-success 'org-shiftmetaup-final-hook))
   (t (call-interactively #'org-drag-line-backward))))

(defun org-shiftmetadown (&optional _arg)
  "Drag the line at point down.
In a table, insert an empty row at the current line.
On a clock timestamp, update the value of the timestamp like `S-<down>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point down.

This function runs the functions in `org-shiftmetadown-hook' one
by one as a first step, and exits immediately if a function from
the hook returns non-nil.  In the absence of a specific context,
the function also runs `org-shiftmetadown-final-hook' using the
same logic."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetadown-hook))
   ((org-at-table-p) (call-interactively #'org-table-insert-row))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
			   (call-interactively #'org-timestamp-down)))
   ((run-hook-with-args-until-success 'org-shiftmetadown-final-hook))
   (t (call-interactively #'org-drag-line-forward))))

(defsubst org-hidden-tree-error ()
  (user-error
   "Hidden subtree, open with TAB or use subtree command M-S-<left>/<right>"))

(defun org-metaleft (&optional _arg)
  "Promote heading, list item at point or move table column left.

Calls `org-do-promote', `org-outdent-item' or `org-table-move-column',
depending on context.  With no specific context, calls the Emacs
default `backward-word'.  See the individual commands for more
information.

This function runs the functions in `org-metaleft-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metaleft-final-hook' using the same logic."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-metaleft-hook))
   ((org-at-table-p) (org-call-with-arg #'org-table-move-column 'left))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively #'org-do-promote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively #'org-inlinetask-promote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively #'org-outdent-item))
   ((run-hook-with-args-until-success 'org-metaleft-final-hook))
   (t (call-interactively #'backward-word))))

(defun org-metaright (&optional _arg)
  "Demote heading, list item at point or move table column right.

In front of a drawer or a block keyword, indent it correctly.

Calls `org-do-demote', `org-indent-item', `org-table-move-column',
`org-indent-drawer' or `org-indent-block' depending on context.
With no specific context, calls the Emacs default `forward-word'.
See the individual commands for more information.

This function runs the functions in `org-metaright-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metaright-final-hook' using the same logic."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-metaright-hook))
   ((org-at-table-p) (call-interactively #'org-table-move-column))
   ((org-at-drawer-p) (call-interactively #'org-indent-drawer))
   ((org-at-block-p) (call-interactively #'org-indent-block))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively #'org-do-demote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively #'org-inlinetask-demote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively #'org-indent-item))
   ((run-hook-with-args-until-success 'org-metaright-final-hook))
   (t (call-interactively #'forward-word))))

(defun org-check-for-hidden (what)
  "Check if there are hidden headlines/items in the current visual line.
WHAT can be either `headlines' or `items'.  If the current line is
an outline or item heading and it has a folded subtree below it,
this function returns t, nil otherwise."
  (let ((re (cond
	     ((eq what 'headlines) org-outline-regexp-bol)
	     ((eq what 'items) (org-item-beginning-re))
	     (t (error "This should not happen"))))
	beg end)
    (save-excursion
      (catch 'exit
	(unless (org-region-active-p)
          (setq beg (line-beginning-position))
	  (forward-line 1)
	  (while (and (not (eobp)) ;; this is like `next-line'
		      (org-invisible-p (1- (point))))
	    (forward-line 1))
	  (setq end (point))
	  (goto-char beg)
          (goto-char (line-end-position))
	  (setq end (max end (point)))
	  (while (re-search-forward re end t)
	    (when (org-invisible-p (match-beginning 0))
	      (throw 'exit t))))
	nil))))

(defun org-metaup (&optional _arg)
  "Move subtree up or move table row up.
Calls `org-move-subtree-up' or `org-table-move-row' or
`org-move-item-up', depending on context.  Everywhere else, move
backward the element at point.  See the individual commands for
more information.

This function runs the functions in `org-metaup-hook' one by one
as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metaup-final-hook' using the same logic."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-metaup-hook))
   ((and (org-region-active-p)
         (org-with-limited-levels
          (save-excursion
            (goto-char (region-beginning))
            (org-at-heading-p))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (let ((beg (region-beginning))
          (end (region-end))
          (region-extended nil))
      (save-excursion
        ;; Go a little earlier because `org-move-subtree-down' will
        ;; insert before markers and we may overshoot in some cases.
        (goto-char (max beg (1- end)))
        (setq end (point-marker))
        (goto-char beg)
        (let ((level (org-current-level)))
          (when (or (and (> level 1) (re-search-forward (format "^\\*\\{1,%s\\} " (1- level)) end t))
                    ;; Search previous subtree.
                    (progn
                      (goto-char beg)
                      (forward-line 0)
                      (not (re-search-backward (format "^\\*\\{%s\\} " level) nil t))))
            (user-error "Cannot move past superior level or buffer limit"))
          ;; Drag first subtree above below the selected.
          (while (< (point) end)
            (call-interactively #'org-move-subtree-down)
            (setq deactivate-mark (org--deactivate-mark)))
          ;; When `org-move-subtree-down' inserts before markers, the
          ;; region boundaries will extend to the moved
          ;; heading. Prevent this.
          (when (<= (point) (region-end))
            (setq region-extended t))))
      (when region-extended
        (if (= (region-beginning) (point))
            (set-mark (1+ end))
          (goto-char (1+ end))))))
   ((org-region-active-p)
    (let* ((a (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
           (b (save-excursion
                (goto-char (region-end))
                (if (bolp) (1- (point)) (line-end-position))))
           (c (save-excursion
                (goto-char a)
                (move-beginning-of-line 0)
                (point)))
           (d (save-excursion
                (goto-char a)
                (move-end-of-line 0)
                (point)))
           (deactivate-mark nil)
           (swap? (< (point) (mark))))
      (transpose-regions a b c d)
      (set-mark c)
      (goto-char (+ c (- b a)))
      (when swap? (exchange-point-and-mark))))
   ((org-at-table-p) (org-call-with-arg #'org-table-move-row 'up))
   ((and (featurep 'org-inlinetask)
         (org-inlinetask-in-task-p))
    (org-drag-element-backward))
   ((org-at-heading-p) (call-interactively #'org-move-subtree-up))
   ((org-at-item-p) (call-interactively #'org-move-item-up))
   ((run-hook-with-args-until-success 'org-metaup-final-hook))
   (t (org-drag-element-backward))))

(defun org-metadown (&optional _arg)
  "Move subtree down or move table row down.
Calls `org-move-subtree-down' or `org-table-move-row' or
`org-move-item-down', depending on context.  Everywhere else,
move forward the element at point.  See the individual commands
for more information.

This function runs the functions in `org-metadown-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metadown-final-hook' using the same logic."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-metadown-hook))
   ((and (org-region-active-p)
         (org-with-limited-levels
          (save-excursion
            (goto-char (region-beginning))
            (org-at-heading-p))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char beg)
        (setq beg (point-marker))
        (let ((level (org-current-level)))
          (when (or (and (> level 1) (re-search-forward (format "^\\*\\{1,%s\\} " (1- level)) end t))
                    ;; Search next subtree.
                    (progn
                      (goto-char end)
                      (not (re-search-forward (format "^\\*\\{%s\\} " level) nil t))))
            (user-error "Cannot move past superior level or buffer limit"))
          ;; Drag first subtree below above the selected.
          (while (> (point) beg)
            (call-interactively #'org-move-subtree-up)
            (setq deactivate-mark (org--deactivate-mark)))))))
   ((org-region-active-p)
    (let* ((a (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
	   (b (save-excursion
                (goto-char (region-end))
                (if (bolp) (1- (point)) (line-end-position))))
	   (c (save-excursion
                (goto-char b)
                (move-beginning-of-line (if (bolp) 1 2))
                (point)))
	   (d (save-excursion
                (goto-char b)
                (move-end-of-line (if (bolp) 1 2))
                (point)))
           (deactivate-mark nil)
           (swap? (< (point) (mark))))
      (transpose-regions a b c d)
      (set-mark (+ 1 a (- d c)))
      (goto-char (+ 1 a (- d c) (- b a)))
      (when swap? (exchange-point-and-mark))))
   ((org-at-table-p) (call-interactively #'org-table-move-row))
   ((and (featurep 'org-inlinetask)
         (org-inlinetask-in-task-p))
    (org-drag-element-forward))
   ((org-at-heading-p) (call-interactively #'org-move-subtree-down))
   ((org-at-item-p) (call-interactively #'org-move-item-down))
   ((run-hook-with-args-until-success 'org-metadown-final-hook))
   (t (org-drag-element-forward))))

(defun org-shiftup (&optional arg)
  "Act on current element according to context.
Call `org-timestamp-up' or `org-priority-up', or
`org-previous-item', or `org-table-move-cell-up'.  See the
individual commands for more information.

This function runs the functions in `org-shiftup-hook' one by one
as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function also runs `org-shiftup-final-hook' using the same logic.

If none of the previous steps succeed and
`org-support-shift-select' is non-nil, the function runs
`shift-select-mode' associated command.  See that variable for
more information."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-shiftup-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'previous-line))
   ((and (not (eq org-support-shift-select 'everywhere))
         (org-at-timestamp-p 'lax))
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-down 'org-timestamp-up)))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 org-priority-enable-commands
	 (org-at-heading-p))
    (call-interactively #'org-priority-up))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively #'org-previous-item))
   ((org-clocktable-try-shift 'up arg))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-table-p))
    (org-table-move-cell-up))
   ((run-hook-with-args-until-success 'org-shiftup-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'previous-line))
   (t (org-shiftselect-error))))

(defun org-shiftdown (&optional arg)
  "Act on current element according to context.
Call `org-timestamp-down' or `org-priority-down', or
`org-next-item', or `org-table-move-cell-down'.  See the
individual commands for more information.

This function runs the functions in `org-shiftdown-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function also runs `org-shiftdown-final-hook' using the same
logic.

If none of the previous steps succeed and
`org-support-shift-select' is non-nil, the function runs
`shift-select-mode' associated command.  See that variable for
more information."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-shiftdown-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'next-line))
   ((and (not (eq org-support-shift-select 'everywhere))
         (org-at-timestamp-p 'lax))
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-up 'org-timestamp-down)))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 org-priority-enable-commands
	 (org-at-heading-p))
    (call-interactively #'org-priority-down))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively #'org-next-item))
   ((org-clocktable-try-shift 'down arg))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-table-p))
    (org-table-move-cell-down))
   ((run-hook-with-args-until-success 'org-shiftdown-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'next-line))
   (t (org-shiftselect-error))))

(defun org-shiftright (&optional arg)
  "Act on the current element according to context.
This does one of the following:

- switch a timestamp at point one day into the future
- on a headline, switch to the next TODO keyword
- on an item, switch entire list to the next bullet type
- on a property line, switch to the next allowed value
- on a clocktable definition line, move time block into the future
- in a table, move a single cell right

This function runs the functions in `org-shiftright-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-shiftright-final-hook' using the same logic.

If none of the above succeeds and `org-support-shift-select' is
non-nil, runs `shift-select-mode' specific command.  See that
variable for more information."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-shiftright-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-char))
   ((and (not (eq org-support-shift-select 'everywhere))
         (org-at-timestamp-p 'lax))
    (call-interactively #'org-timestamp-up-day))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg #'org-todo 'right)))
   ((or (and org-support-shift-select
	     (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg #'org-cycle-list-bullet nil))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-property-p))
    (call-interactively #'org-property-next-allowed-value))
   ((org-clocktable-try-shift 'right arg))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-table-p))
    (org-table-move-cell-right))
   ((run-hook-with-args-until-success 'org-shiftright-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-char))
   (t (org-shiftselect-error))))

(defun org-shiftleft (&optional arg)
  "Act on current element according to context.
This does one of the following:

- switch a timestamp at point one day into the past
- on a headline, switch to the previous TODO keyword.
- on an item, switch entire list to the previous bullet type
- on a property line, switch to the previous allowed value
- on a clocktable definition line, move time block into the past
- in a table, move a single cell left

This function runs the functions in `org-shiftleft-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-shiftleft-final-hook' using the same logic.

If none of the above succeeds and `org-support-shift-select' is
non-nil, runs `shift-select-mode' specific command.  See that
variable for more information."
  (interactive "P" org-mode)
  (cond
   ((run-hook-with-args-until-success 'org-shiftleft-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-char))
   ((and (not (eq org-support-shift-select 'everywhere))
         (org-at-timestamp-p 'lax))
    (call-interactively #'org-timestamp-down-day))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg #'org-todo 'left)))
   ((or (and org-support-shift-select
	     (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg #'org-cycle-list-bullet 'previous))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-property-p))
    (call-interactively #'org-property-previous-allowed-value))
   ((org-clocktable-try-shift 'left arg))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-table-p))
    (org-table-move-cell-left))
   ((run-hook-with-args-until-success 'org-shiftleft-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-char))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolright ()
  "Switch to next TODO set."
  (interactive nil org-mode)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-word))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-heading-p))
    (org-call-with-arg #'org-todo 'nextset))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolleft ()
  "Switch to previous TODO set."
  (interactive nil org-mode)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-word))
   ((and (not (memq org-support-shift-select '(always except-timestamps everywhere)))
	 (org-at-heading-p))
    (org-call-with-arg #'org-todo 'previousset))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolup (&optional n)
  "Change timestamps synchronously up in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P" org-mode)
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let ((org-support-shift-select
             (if (eq org-support-shift-select 'everywhere)
                 org-support-shift-select
               nil)))
	(org-clock-timestamps-up n))
    (user-error "Not at a clock log")))

(defun org-shiftcontroldown (&optional n)
  "Change timestamps synchronously down in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P" org-mode)
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let ((org-support-shift-select
             (if (eq org-support-shift-select 'everywhere)
                 org-support-shift-select
               nil)))
	(org-clock-timestamps-down n))
    (user-error "Not at a clock log")))

(defun org-increase-number-at-point (&optional inc)
  "Increment the number at point.
With an optional prefix numeric argument INC, increment using
this numeric value."
  (interactive "p" org-mode)
  (if (not (number-at-point))
      (user-error "Not on a number")
    (unless inc (setq inc 1))
    (let ((pos (point))
	  (beg (skip-chars-backward "-+^/*0-9eE."))
	  (end (skip-chars-forward "-+^/*0-9eE.")) nap)
      (setq nap (buffer-substring-no-properties
		 (+ pos beg) (+ pos beg end)))
      (delete-region (+ pos beg) (+ pos beg end))
      (insert (calc-eval (concat (number-to-string inc) "+" nap))))
    (when (org-at-table-p)
      (org-table-align)
      (org-table-end-of-field 1))))

(defun org-decrease-number-at-point (&optional inc)
  "Decrement the number at point.
With an optional prefix numeric argument INC, decrement using
this numeric value."
  (interactive "p" org-mode)
  (org-increase-number-at-point (- (or inc 1))))

(defun org-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-heading'."
  (interactive nil org-mode)
  (cond
   ((org-at-table-p) (call-interactively #'org-table-hline-and-move))
   (t (call-interactively #'org-insert-heading))))

(defun org-copy-visible (beg end)
  "Copy the visible parts of the region."
  (interactive "r" org-mode)
  (let ((result ""))
    (while (/= beg end)
      (while (org-invisible-p beg)
	(setq beg (org-fold-next-visibility-change beg end)))
      (let ((next (org-fold-next-visibility-change beg end)))
	(setq result (concat result (buffer-substring beg next)))
	(setq beg next)))
    ;; Prevent Emacs from adding full selected text to `kill-ring'
    ;; when `select-enable-primary' is non-nil.  This special value of
    ;; `deactivate-mark' only works since Emacs 29.
    (setq deactivate-mark 'dont-save)
    (kill-new result)
    (message "Visible strings have been copied to the kill ring.")))

(defun org-copy-special ()
  "Copy region in table or copy current subtree.
Calls `org-table-copy-region' or `org-copy-subtree', depending on
context.  See the individual commands for more information."
  (interactive nil org-mode)
  (call-interactively
   (if (org-at-table-p) #'org-table-copy-region #'org-copy-subtree)))

(defun org-cut-special ()
  "Cut region in table or cut current subtree.
Calls `org-table-cut-region' or `org-cut-subtree', depending on
context.  See the individual commands for more information."
  (interactive nil org-mode)
  (call-interactively
   (if (org-at-table-p) #'org-table-cut-region #'org-cut-subtree)))

(defun org-paste-special (arg)
  "Paste rectangular region into table, or paste subtree relative to level.
Calls `org-table-paste-rectangle' or `org-paste-subtree', depending on context.
See the individual commands for more information."
  (interactive "P" org-mode)
  (if (org-at-table-p)
      (org-table-paste-rectangle)
    (org-paste-subtree arg)))

(defun org-edit-special (&optional arg)
  "Call a special editor for the element at point.
When at a table, call the formula editor with `org-table-edit-formulas'.
When at table.el table, edit it in dedicated buffer.
When in a source code block, call `org-edit-src-code'; with prefix
  argument, switch to session buffer.
When in an example block, call `org-edit-src-code'.
When in an inline code block, call `org-edit-inline-src-code'.
When in a fixed-width region, call `org-edit-fixed-width-region'.
When in an export block, call `org-edit-export-block'.
When in a comment block, call `org-edit-comment-block'.
When in a LaTeX environment, call `org-edit-latex-environment'.
When at an INCLUDE, SETUPFILE or BIBLIOGRAPHY keyword, visit the included file.
When at a footnote reference, call `org-edit-footnote-reference'.
When at a planning line call, `org-deadline' and/or `org-schedule'.
When at an active timestamp, call `org-timestamp'.
When at an inactive timestamp, call `org-timestamp-inactive'.
On a link, call `ffap' to visit the link at point.
Otherwise, return a user error."
  (interactive "P" org-mode)
  (let ((element (org-element-at-point)))
    (barf-if-buffer-read-only)
    (pcase (org-element-type element)
      (`src-block
       (if (not arg) (org-edit-src-code)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info))
		(params (nth 2 info))
		(session (cdr (assq :session params))))
	   (if (not session) (org-edit-src-code)
	     ;; At a source block with a session and function called
	     ;; with an ARG: switch to the buffer related to the
	     ;; inferior process.
	     (switch-to-buffer
	      (funcall (intern (concat "org-babel-prep-session:" lang))
		       session params))))))
      (`keyword
       (unless (member (org-element-property :key element)
		       '("BIBLIOGRAPHY" "INCLUDE" "SETUPFILE"))
	 (user-error "No special environment to edit here"))
       (let ((value (org-element-property :value element)))
	 (unless (org-string-nw-p value) (user-error "No file to edit"))
	 (let ((file (and (string-match "\\`\"\\(.*?\\)\"\\|\\S-+" value)
			  (or (match-string 1 value)
			      (match-string 0 value)))))
	   (when (org-url-p file)
	     (user-error "Files located with a URL cannot be edited"))
	   (org-link-open-from-string
	    (format "[[%s]]" (expand-file-name file))))))
      (`table
       (if (eq (org-element-property :type element) 'table.el)
           (org-edit-table.el)
         (call-interactively #'org-table-edit-formulas)))
      ;; Only Org tables contain `table-row' type elements.
      (`table-row (call-interactively #'org-table-edit-formulas))
      (`example-block (org-edit-src-code))
      (`export-block (org-edit-export-block))
      (`comment-block (org-edit-comment-block))
      (`fixed-width (org-edit-fixed-width-region))
      (`latex-environment (org-edit-latex-environment))
      (`planning
       (let ((proplist (cadr element)))
         (mapc #'call-interactively
               (remq nil
                     (list
                      (when (plist-get proplist :deadline) #'org-deadline)
                      (when (plist-get proplist :scheduled) #'org-schedule))))))
      (_
       ;; No notable element at point.  Though, we may be at a link or
       ;; a footnote reference, which are objects.  Thus, scan deeper.
       (let ((context (org-element-context element)))
	 (pcase (org-element-type context)
	   (`footnote-reference (org-edit-footnote-reference))
	   (`inline-src-block (org-edit-inline-src-code))
	   (`latex-fragment (org-edit-latex-fragment))
	   (`timestamp (if (eq 'inactive (org-element-property :type context))
			   (call-interactively #'org-timestamp-inactive)
			 (call-interactively #'org-timestamp)))
	   (`link (call-interactively #'ffap))
	   (_ (user-error "No special environment to edit here"))))))))

(defun org-ctrl-c-ctrl-c (&optional arg)
  "Set tags in headline, or update according to changed information at point.

This command does many different things, depending on context:

- If column view is active, in agenda or org buffers, quit it.

- If there are highlights, remove them.

- If a function in `org-ctrl-c-ctrl-c-hook' recognizes this location,
  this is what we do.

- If the cursor is on a statistics cookie, update it.

- If the cursor is in a headline, in an agenda or an org buffer,
  prompt for tags and insert them into the current line, aligned
  to `org-tags-column'.  When called with prefix arg, realign all
  tags in the current buffer.

- If the cursor is in one of the special #+KEYWORD lines, this
  triggers scanning the buffer for these lines and updating the
  information.

- If the cursor is inside a table, realign the table.  This command
  works even if the automatic table editor has been turned off.

- If the cursor is on a #+TBLFM line, re-apply the formulas to
  the entire table.

- If the cursor is at a footnote reference or definition, jump to
  the corresponding definition or references, respectively.

- If the cursor is a the beginning of a dynamic block, update it.

- If the current buffer is a capture buffer, close note and file it.

- If the cursor is on a <<<target>>>, update radio targets and
  corresponding links in this buffer.

- If the cursor is on a numbered item in a plain list, renumber the
  ordered list.

- If the cursor is on a checkbox, toggle it.

- If the cursor is on a code block, evaluate it.  The variable
  `org-confirm-babel-evaluate' can be used to control prompting
  before code block evaluation, by default every code block
  evaluation requires confirmation.  Code block evaluation can be
  inhibited by setting `org-babel-no-eval-on-ctrl-c-ctrl-c'."
  (interactive "P" org-mode)
  (cond
   ((bound-and-true-p org-columns-overlays) (org-columns-quit))
   ((or (bound-and-true-p org-clock-overlays) org-occur-highlights)
    (when (boundp 'org-clock-overlays) (org-clock-remove-overlays))
    (org-remove-occur-highlights)
    (message "Temporary highlights/overlays removed from current buffer"))
   ((and (local-variable-p 'org-finish-function)
	 (fboundp org-finish-function))
    (funcall org-finish-function))
   ((org-babel-hash-at-point))
   ((run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-hook))
   (t
    (let* ((context
	    (org-element-lineage
	     (org-element-context)
	     ;; Limit to supported contexts.
	     '(babel-call clock dynamic-block footnote-definition
			  footnote-reference inline-babel-call inline-src-block
			  inlinetask item keyword node-property paragraph
			  plain-list planning property-drawer radio-target
			  src-block statistics-cookie table table-cell table-row
			  timestamp)
	     t))
	   (radio-list-p (org-at-radio-list-p))
	   (type (org-element-type context)))
      ;; For convenience: at the first line of a paragraph on the same
      ;; line as an item, apply function on that item instead.
      (when (eq type 'paragraph)
	(let ((parent (org-element-parent context)))
	  (when (and (org-element-type-p parent 'item)
		     (= (line-beginning-position)
			(org-element-begin parent)))
	    (setq context parent)
	    (setq type 'item))))
      ;; Act according to type of element or object at point.
      ;;
      ;; Do nothing on a blank line, except if it is contained in
      ;; a source block.  Hence, we first check if point is in such
      ;; a block and then if it is at a blank line.
      (pcase type
	((or `inline-src-block `src-block)
	 (unless org-babel-no-eval-on-ctrl-c-ctrl-c
	   (org-babel-eval-wipe-error-buffer)
	   (org-babel-execute-src-block
	    current-prefix-arg (org-babel-get-src-block-info nil context))))
	((guard (org-match-line "[ \t]*$"))
	 (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
	     (user-error
	      (substitute-command-keys
	       "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here"))))
	((or `babel-call `inline-babel-call)
	 (let ((info (org-babel-lob-get-info context)))
	   (when info (org-babel-execute-src-block nil info nil type))))
	(`clock
         (if (org-at-timestamp-p 'lax)
             ;; Update the timestamp as well.  `org-timestamp-change'
             ;; will call `org-clock-update-time-maybe'.
             (org-timestamp-change 0 'day)
           (org-clock-update-time-maybe)))
	(`dynamic-block
	 (save-excursion
	   (goto-char (org-element-post-affiliated context))
	   (org-update-dblock)))
	(`footnote-definition
	 (goto-char (org-element-post-affiliated context))
	 (call-interactively #'org-footnote-action))
	(`footnote-reference (call-interactively #'org-footnote-action))
	((or `headline `inlinetask)
	 (save-excursion (goto-char (org-element-begin context))
			 (call-interactively #'org-set-tags-command)))
	(`item
	 ;; At an item: `C-u C-u' sets checkbox to "[-]"
	 ;; unconditionally, whereas `C-u' will toggle its presence.
	 ;; Without a universal argument, if the item has a checkbox,
	 ;; toggle it.  Otherwise repair the list.
	 (if (or radio-list-p
		 (and (boundp org-list-checkbox-radio-mode)
		      org-list-checkbox-radio-mode))
	     (org-toggle-radio-button arg)
	   (let* ((box (org-element-property :checkbox context))
		  (struct (org-element-property :structure context))
                  ;; Avoid modifying cached structure by side effect.
                  (struct (copy-tree struct))
		  (old-struct (copy-tree struct))
		  (parents (org-list-parents-alist struct))
		  (prevs (org-list-prevs-alist struct))
		  (orderedp (org-not-nil (org-entry-get nil "ORDERED"))))
	     (org-list-set-checkbox
	      (org-element-begin context) struct
	      (cond ((equal arg '(16)) "[-]")
		    ((and (not box) (equal arg '(4))) "[ ]")
		    ((or (not box) (equal arg '(4))) nil)
		    ((eq box 'on) "[ ]")
		    (t "[X]")))
	     ;; Mimic `org-list-write-struct' but with grabbing a return
	     ;; value from `org-list-struct-fix-box'.
	     (org-list-struct-fix-ind struct parents 2)
	     (org-list-struct-fix-item-end struct)
	     (org-list-struct-fix-bul struct prevs)
	     (org-list-struct-fix-ind struct parents)
	     (let ((block-item
		    (org-list-struct-fix-box struct parents prevs orderedp)))
	       (if (and box (equal struct old-struct))
		   (if (equal arg '(16))
		       (message "Checkboxes already reset")
		     (user-error "Cannot toggle this checkbox: %s"
				 (if (eq box 'on)
				     "all subitems checked"
				   "unchecked subitems")))
		 (org-list-struct-apply-struct struct old-struct)
		 (org-update-checkbox-count-maybe))
	       (when block-item
		 (message "Checkboxes were removed due to empty box at line %d"
			  (org-current-line block-item)))))))
	(`plain-list
	 ;; At a plain list, with a double C-u argument, set
	 ;; checkboxes of each item to "[-]", whereas a single one
	 ;; will toggle their presence according to the state of the
	 ;; first item in the list.  Without an argument, repair the
	 ;; list.
	 (if (or radio-list-p
		 (and (boundp org-list-checkbox-radio-mode)
		      org-list-checkbox-radio-mode))
	     (org-toggle-radio-button arg)
	   (let* ((begin (org-element-contents-begin context))
		  (struct (org-element-property :structure context))
                  ;; Avoid modifying cached structure by side effect.
                  (struct (copy-tree struct))
		  (old-struct (copy-tree struct))
		  (first-box (save-excursion
			       (goto-char begin)
			       (looking-at org-list-full-item-re)
			       (match-string-no-properties 3)))
		  (new-box (cond ((equal arg '(16)) "[-]")
				 ((equal arg '(4)) (unless first-box "[ ]"))
				 ((equal first-box "[X]") "[ ]")
				 (t "[X]"))))
	     (cond
	      (arg
	       (dolist (pos
			(org-list-get-all-items
			 begin struct (org-list-prevs-alist struct)))
		 (org-list-set-checkbox pos struct new-box)))
	      ((and first-box (eq (point) begin))
	       ;; For convenience, when point is at bol on the first
	       ;; item of the list and no argument is provided, simply
	       ;; toggle checkbox of that item, if any.
	       (org-list-set-checkbox begin struct new-box)))
	     (when (equal
		    (org-list-write-struct
		     struct (org-list-parents-alist struct) old-struct)
		    old-struct)
	       (message "Cannot update this checkbox"))
	     (org-update-checkbox-count-maybe))))
	(`keyword
	 (let ((org-inhibit-startup-visibility-stuff t)
	       (org-startup-align-all-tables nil))
	   (when (boundp 'org-table-coordinate-overlays)
	     (mapc #'delete-overlay org-table-coordinate-overlays)
	     (setq org-table-coordinate-overlays nil))
	   (org-save-outline-visibility 'use-markers (org-mode-restart)))
	 (message "Local setup has been refreshed"))
	((or `property-drawer `node-property)
	 (call-interactively #'org-property-action))
	(`radio-target
	 (call-interactively #'org-update-radio-target-regexp))
	(`statistics-cookie
	 (call-interactively #'org-update-statistics-cookies))
	((or `table `table-cell `table-row)
	 ;; At a table, generate a plot if on the #+plot line,
         ;; recalculate every field and align it otherwise.  Also
	 ;; send the table if necessary.
         (cond
          ((and (org-match-line "[ \t]*#\\+plot:")
                (< (point) (org-element-post-affiliated context)))
           (org-plot/gnuplot))
          ;; If the table has a `table.el' type, just give up.
          ((eq (org-element-property :type context) 'table.el)
           (message "%s" (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables")))
          ;; At a table row or cell, maybe recalculate line but always
	  ;; align table.
          ((or (eq type 'table)
               ;; Check if point is at a TBLFM line.
               (and (eq type 'table-row)
                    (= (point) (org-element-end context))))
           (save-excursion
             (if (org-at-TBLFM-p)
                 (org-table-calc-current-TBLFM)
               (goto-char (org-element-contents-begin context))
               (org-call-with-arg #'org-table-recalculate (or arg t))
               (orgtbl-send-table 'maybe))))
          (t
           (org-table-maybe-eval-formula)
           (cond (arg (call-interactively #'org-table-recalculate))
                 ((org-table-maybe-recalculate-line))
                 (t (org-table-align))))))
	((or `timestamp (and `planning (guard (org-at-timestamp-p 'lax))))
	 (org-timestamp-change 0 'day))
	((and `nil (guard (org-at-heading-p)))
	 ;; When point is on an unsupported object type, we can miss
	 ;; the fact that it also is at a heading.  Handle it here.
	 (call-interactively #'org-set-tags-command))
	((guard
	  (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)))
	(_
	 (user-error
	  (substitute-command-keys
	   "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here"))))))))

(defun org-mode-restart ()
  "Restart `org-mode'."
  (interactive)
  (let ((indent-status (bound-and-true-p org-indent-mode)))
    (funcall major-mode)
    (hack-local-variables)
    (when (and indent-status (not (bound-and-true-p org-indent-mode)))
      (org-indent-mode -1))
    (org-reset-file-cache))
  (message "%s restarted" major-mode))

(defun org-kill-note-or-show-branches ()
  "Abort storing current note, or show just branches."
  (interactive)
  (cond (org-finish-function
	 (let ((org-note-abort t)) (funcall org-finish-function)))
	((org-before-first-heading-p)
	 (org-fold-show-branches-buffer)
	 (org-fold-hide-archived-subtrees (point-min) (point-max)))
	(t
	 (let ((beg (progn (org-back-to-heading) (point)))
	       (end (save-excursion (org-end-of-subtree t t) (point))))
	   (org-fold-hide-subtree)
	   (org-fold-show-branches)
	   (org-fold-hide-archived-subtrees beg end)))))

(defun org-delete-indentation (&optional arg beg end)
  "Join current line to previous and fix whitespace at join.

If previous line is a headline add to headline title.  Otherwise
the function calls `delete-indentation'.

If there is a region (BEG END), then join the lines in that region.

With a non-nil prefix ARG, join the line with the following one,
ignoring region."
  (interactive
   (cons current-prefix-arg
         (when (and (not current-prefix-arg) (use-region-p))
           (list (region-beginning) (region-end))))
   org-mode)
  (unless (and beg end)
    ;; No region selected or BEG/END arguments not passed.
    (setq beg (line-beginning-position (if arg 1 0))
          end (line-end-position (if arg 2 1))))
  (if (save-excursion
        (goto-char beg)
        (forward-line 0)
        (and (< (line-end-position) end)
             (let ((case-fold-search nil))
	       (looking-at org-complex-heading-regexp))))
      ;; At headline.
      (let ((tags-column (when (match-beginning 5)
			   (save-excursion (goto-char (match-beginning 5))
					   (current-column))))
	    string)
        (goto-char beg)
        ;; Join all but headline.
        (save-excursion
          (save-match-data
            (if (version<= "27" emacs-version)
                (delete-indentation nil (line-beginning-position 2) end)
              ;; FIXME: Emacs 26.  `delete-indentation' does not yet
              ;; accept BEG/END arguments.
              (save-restriction
                (narrow-to-region beg end)
                (goto-char beg)
                (forward-line 2)
                (while (< (point) (point-max))
                  (delete-indentation)
                  (forward-line 1))))))
        (setq string (org-trim (delete-and-extract-region (line-end-position) (line-end-position 2))))
	(goto-char (or (match-end 4)
		       (match-beginning 5)
		       (match-end 0)))
	(skip-chars-backward " \t")
	(save-excursion (insert " " string))
	;; Adjust alignment of tags.
	(cond
	 ((not tags-column))		;no tags
	 (org-auto-align-tags (org-align-tags))
	 (t (org--align-tags-here tags-column)))) ;preserve tags column
    (if (version<= "27" emacs-version)
        (funcall-interactively #'delete-indentation arg beg end)
      ;; FIXME: Emacs 26.  `delete-indentation' does not yet
      ;; accept BEG/END arguments.
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (forward-line 1)
        (while (< (point) (point-max))
          (delete-indentation)
          (forward-line 1))))))

(defun org-open-line (n)
  "Insert a new row in tables, call `open-line' elsewhere.
If `org-special-ctrl-o' is nil, just call `open-line' everywhere.
As a special case, when a document starts with a table, allow
calling `open-line' on the very first character."
  (interactive "*p" org-mode)
  (if (and org-special-ctrl-o (/= (point) 1) (org-at-table-p))
      (org-table-insert-row)
    (open-line n)))

(defun org--newline (indent arg interactive)
  "Call `newline-and-indent' or just `newline'.
If INDENT is non-nil, call `newline-and-indent' with ARG to
indent unconditionally; otherwise, call `newline' with ARG and
INTERACTIVE, which can trigger indentation if
`electric-indent-mode' is enabled."
  (if indent
      (org-newline-and-indent arg)
    (newline arg interactive)))

(defun org-return (&optional indent arg interactive)
  "Goto next table row or insert a newline.

Calls `org-table-next-row' or `newline', depending on context.

When optional INDENT argument is non-nil, call
`newline-and-indent' with ARG, otherwise call `newline' with ARG
and INTERACTIVE.

When `org-return-follows-link' is non-nil and point is on
a timestamp, a link or a citation, call `org-open-at-point'.
However, it will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
  (interactive "i\nP\np" org-mode)
  (let* ((context (if org-return-follows-link (org-element-context)
		    (org-element-at-point)))
         (element-type (org-element-type context)))
    (cond
     ;; In a table, call `org-table-next-row'.  However, before first
     ;; column or after last one, split the table.
     ((or (and (eq 'table element-type)
	       (not (eq 'table.el (org-element-property :type context)))
	       (>= (point) (org-element-contents-begin context))
	       (< (point) (org-element-contents-end context)))
	  (org-element-lineage context '(table-row table-cell) t))
      (if (or (looking-at-p "[ \t]*$")
	      (save-excursion (skip-chars-backward " \t") (bolp)))
	  (insert "\n")
	(org-table-justify-field-maybe)
	(call-interactively #'org-table-next-row)))
     ;; On a link, a timestamp or a citation, call `org-open-at-point'
     ;; if `org-return-follows-link' allows it.  Tolerate fuzzy
     ;; locations, e.g., in a comment, as `org-open-at-point'.
     ((and org-return-follows-link
	   (or
            (let ((context
                   (org-element-lineage
                    context
                    '(citation citation-reference link)
                    'include-self)))
              (and context
                   ;; Ensure point is not on the white spaces after
                   ;; the link.
                   (let ((origin (point)))
                     (org-with-point-at (org-element-end context)
                       (skip-chars-backward " \t")
                       (> (point) origin)))))
            (org-in-regexp org-ts-regexp-both nil t)
            (org-in-regexp org-tsr-regexp-both nil  t)
            (org-in-regexp org-link-any-re nil t)))
      (call-interactively #'org-open-at-point))
     ;; Insert newline in heading, but preserve tags.
     ((and (not (bolp))
	   (let ((case-fold-search nil))
	     (org-match-line org-complex-heading-regexp)))
      ;; At headline.  Split line.  However, if point is on keyword,
      ;; priority cookie or tags, do not break any of them: add
      ;; a newline after the headline instead.
      (let ((tags-column (and (match-beginning 5)
			      (save-excursion (goto-char (match-beginning 5))
					      (current-column))))
	    (string
	     (when (and (match-end 4) (org-point-in-group (point) 4))
	       (delete-and-extract-region (point) (match-end 4)))))
	;; Adjust tag alignment.
	(cond
	 ((not (and tags-column string)))
	 (org-auto-align-tags (org-align-tags))
	 (t (org--align-tags-here tags-column))) ;preserve tags column
	(end-of-line)
	(org-fold-show-entry 'hide-drawers)
	(org--newline indent arg interactive)
	(when string (save-excursion (insert (org-trim string))))))
     ;; In a list, make sure indenting keeps trailing text within.
     ((and (not (eolp))
	   (org-element-lineage context 'item))
      (let ((trailing-data
	     (delete-and-extract-region (point) (line-end-position))))
	(org--newline indent arg interactive)
	(save-excursion (insert trailing-data))))
     (t
      ;; Do not auto-fill when point is in an Org property drawer.
      (let ((auto-fill-function (and (not (org-at-property-p))
				     auto-fill-function)))
	(org--newline indent arg interactive))))))

(defun org-return-and-maybe-indent ()
  "Goto next table row, or insert a newline, maybe indented.
Call `org-table-next-row' or `org-return', depending on context.
See the individual commands for more information.

When inserting a newline, if `org-adapt-indentation' is t:
indent the line if `electric-indent-mode' is disabled, don't
indent it if it is enabled."
  (interactive nil org-mode)
  (org-return (not electric-indent-mode)))

(defun org-ctrl-c-tab (&optional arg)
  "Toggle columns width in a table, or show children.
Call `org-table-toggle-column-width' if point is in a table.
Otherwise provide a compact view of the children.  ARG is the
level to hide."
  (interactive "p" org-mode)
  (cond
   ((org-at-table-p)
    (call-interactively #'org-table-toggle-column-width))
   ((org-before-first-heading-p)
    (save-excursion
      (org-fold-flag-above-first-heading)
      (org-fold-hide-sublevels (or arg 1))))
   (t
    (org-fold-hide-subtree)
    (org-fold-show-children arg))))

(defun org-ctrl-c-star ()
  "Compute table, or change heading status of lines.
Calls `org-table-recalculate' or `org-toggle-heading',
depending on context."
  (interactive nil org-mode)
  (cond
   ((org-at-table-p)
    (call-interactively #'org-table-recalculate))
   (t
    ;; Convert all lines in region to list items
    (call-interactively #'org-toggle-heading))))

(defun org-ctrl-c-minus ()
  "Insert separator line in table or modify bullet status of line.
Also turns a plain line or a region of lines into list items.
Calls `org-table-insert-hline', `org-toggle-item', or
`org-cycle-list-bullet', depending on context."
  (interactive nil org-mode)
  (cond
   ((org-at-table-p)
    (call-interactively #'org-table-insert-hline))
   ((org-region-active-p)
    (call-interactively #'org-toggle-item))
   ((org-in-item-p)
    (call-interactively #'org-cycle-list-bullet))
   (t
    (call-interactively #'org-toggle-item))))

(defun org-toggle-heading (&optional nstars)
  "Convert headings to normal text, or items or text to headings.
If there is no active region, only convert the current line.

With a `\\[universal-argument]' prefix, convert the whole list at
point into heading.

In a region:

- If the first non blank line is a headline, remove the stars
  from all headlines in the region.

- If it is a normal line, turn each and every normal line (i.e.,
  not an heading or an item) in the region into headings.  If you
  want to convert only the first line of this region, use one
  universal prefix argument.

- If it is a plain list item, turn all plain list items into headings.
  The checkboxes are converted to appropriate TODO or DONE keywords
  (using `car' or `org-done-keywords' and `org-not-done-keywords' when
  available).

When converting a line into a heading, the number of stars is chosen
such that the lines become children of the current entry.  However,
when a numeric prefix argument is given, its value determines the
number of stars to add."
  (interactive "P" org-mode)
  (let ((skip-blanks
	 ;; Return beginning of first non-blank line, starting from
	 ;; line at POS.
	 (lambda (pos)
	   (save-excursion
	     (goto-char pos)
	     (while (org-at-comment-p) (forward-line))
	     (skip-chars-forward " \r\t\n")
             (line-beginning-position))))
	beg end toggled)
    ;; Determine boundaries of changes.  If a universal prefix has
    ;; been given, put the list in a region.  If region ends at a bol,
    ;; do not consider the last line to be in the region.

    (when (and current-prefix-arg (org-at-item-p))
      (when (listp current-prefix-arg) (setq current-prefix-arg 1))
      (org-mark-element))

    (if (org-region-active-p)
	(setq beg (funcall skip-blanks (region-beginning))
	      end (copy-marker (save-excursion
				 (goto-char (region-end))
                                 (if (bolp) (point) (line-end-position)))))
      (setq beg (funcall skip-blanks (line-beginning-position))
            end (copy-marker (line-end-position))))
    ;; Ensure inline tasks don't count as headings.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (cond
	;; Case 1. Started at an heading: de-star headings.
	((org-at-heading-p)
	 (while (< (point) end)
	   (when (org-at-heading-p)
	     (looking-at org-outline-regexp) (replace-match "")
	     (setq toggled t))
	   (forward-line)))
	;; Case 2. Started at an item: change items into headlines.
	;;         One star will be added by `org-list-to-subtree'.
	((org-at-item-p)
	 (while (< (point) end)
	   (when (org-at-item-p)
	     ;; Pay attention to cases when region ends before list.
	     (let* ((struct (org-list-struct))
		    (list-end
		     (min (org-list-get-bottom-point struct) (1+ end))))
	       (save-restriction
		 (narrow-to-region (point) list-end)
		 (insert (org-list-to-subtree
			  (org-list-to-lisp t)
			  (pcase (org-current-level)
			    (`nil 1)
			    (l (1+ (org-reduced-level l))))
                          ;; Keywords to replace checkboxes.
                          (list
                           ;; [X]
                           :cbon (concat (or (car org-done-keywords) "DONE") " ")
                           ;; [ ]
                           :cboff (concat (or (car org-not-done-keywords) "TODO") " ")
                           ;; [-]
                           :cbtrans (concat (or (car org-not-done-keywords) "TODO") " ")))
			 "\n")))
	     (setq toggled t))
	   (forward-line)))
	;; Case 3. Started at normal text: make every line an heading,
	;;         skipping headlines and items.
	(t (let* ((stars
		   (make-string
		    (if (numberp nstars) nstars (or (org-current-level) 0)) ?*))
		  (add-stars
		   (cond (nstars "")	; stars from prefix only
			 ((equal stars "") "*")	; before first heading
			 (org-odd-levels-only "**") ; inside heading, odd
			 (t "*")))	; inside heading, oddeven
		  (rpl (concat stars add-stars " "))
		  (lend (when (listp nstars) (save-excursion (end-of-line) (point)))))
	     (while (< (point) (if (equal nstars '(4)) lend end))
	       (when (and (not (or (org-at-heading-p) (org-at-item-p) (org-at-comment-p)))
			  (looking-at "\\([ \t]*\\)\\(\\S-\\)"))
		 (replace-match (concat rpl (match-string 2))) (setq toggled t))
	       (forward-line)))))))
    (unless toggled (message "Cannot toggle heading from here"))))

(defun org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-heading'."
  (interactive "P" org-mode)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				((org-at-table-p) #'org-table-wrap-region)
				((org-in-item-p) #'org-insert-item)
				(t #'org-insert-heading)))))

;;; Menu entries
(defsubst org-in-subtree-not-table-p ()
  "Are we in a subtree and not in a table?"
  (and (not (org-before-first-heading-p))
       (not (org-at-table-p))))

;; Define the Org mode menus
(easy-menu-define org-org-menu org-mode-map "Org menu."
  `("Org"
    ("Show/Hide"
     ["Cycle Visibility" org-cycle :active (or (bobp) (outline-on-heading-p))]
     ["Cycle Global Visibility" org-shifttab :active (not (org-at-table-p))]
     ["Sparse Tree..." org-sparse-tree t]
     ["Reveal Context" org-fold-reveal t]
     ["Show All" org-fold-show-all t]
     "--"
     ["Subtree to Indirect Buffer" org-tree-to-indirect-buffer t])
    "--"
    ["New Heading" org-insert-heading t]
    ("Navigate Headings"
     ["Up" outline-up-heading t]
     ["Next" outline-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t]
     "--"
     ["Jump" org-goto t])
    ("Edit Structure"
     ["Move Subtree Up" org-metaup (org-at-heading-p)]
     ["Move Subtree Down" org-metadown (org-at-heading-p)]
     "--"
     ["Copy Subtree" org-copy-special (org-in-subtree-not-table-p)]
     ["Cut Subtree" org-cut-special (org-in-subtree-not-table-p)]
     ["Paste Subtree"  org-paste-special (not (org-at-table-p))]
     "--"
     ["Clone Subtree, Shift Time" org-clone-subtree-with-time-shift t]
     "--"
     ["Copy Visible Text" org-copy-visible t]
     "--"
     ["Promote Heading" org-metaleft (org-in-subtree-not-table-p)]
     ["Promote Subtree" org-shiftmetaleft (org-in-subtree-not-table-p)]
     ["Demote Heading" org-metaright (org-in-subtree-not-table-p)]
     ["Demote Subtree" org-shiftmetaright (org-in-subtree-not-table-p)]
     "--"
     ["Sort Region/Children" org-sort t]
     "--"
     ["Convert to Odd Levels" org-convert-to-odd-levels t]
     ["Convert to Odd/Even Levels" org-convert-to-oddeven-levels t])
    ("Editing"
     ["Emphasis..." org-emphasize t]
     ["Add Block Structure" org-insert-structure-template t]
     ["Edit Source Example" org-edit-special t]
     "--"
     ["Footnote New/Jump" org-footnote-action t]
     ["Footnote Extra" (org-footnote-action t) :active t :keys "C-u C-c C-x f"])
    ("Archive"
     ["Archive (Default Method)" org-archive-subtree-default (org-in-subtree-not-table-p)]
     "--"
     ["Move Subtree to Archive File" org-archive-subtree (org-in-subtree-not-table-p)]
     ["Toggle ARCHIVE Tag" org-toggle-archive-tag (org-in-subtree-not-table-p)]
     ["Move Subtree to Archive Sibling" org-archive-to-archive-sibling (org-in-subtree-not-table-p)])
    "--"
    ("Hyperlinks"
     ["Store Link (Global)" org-store-link t]
     ["Find Existing Link to Here" org-occur-link-in-agenda-files t]
     ["Insert Link" org-insert-link t]
     ["Follow Link" org-open-at-point t]
     "--"
     ["Next Link" org-next-link t]
     ["Previous Link" org-previous-link t]
     "--"
     ["Descriptive Links"
      org-toggle-link-display
      :style radio
      :selected org-link-descriptive
      ]
     ["Literal Links"
      org-toggle-link-display
      :style radio
      :selected (not org-link-descriptive)])
    "--"
    ("TODO Lists"
     ["TODO/DONE/-" org-todo t]
     ("Select Keyword"
      ["Next Keyword" org-shiftright (org-at-heading-p)]
      ["Previous Keyword" org-shiftleft (org-at-heading-p)]
      ["Complete Keyword" pcomplete (assq :todo-keyword (org-context))]
      ["Next Keyword Set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))]
      ["Previous Keyword Set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))])
     ["Show TODO Tree" org-show-todo-tree :active t :keys "C-c / t"]
     ["Global TODO List" org-todo-list :active t :keys "\\[org-agenda] t"]
     "--"
     ["Enforce Dependencies" (customize-variable 'org-enforce-todo-dependencies)
      :selected org-enforce-todo-dependencies :style toggle :active t]
     "Settings for Tree at Point"
     "--"
     ["Enforce TODO Completion Order"
      org-toggle-ordered-property
      :style toggle
      :selected (org-entry-get nil "ORDERED")
      :active org-enforce-todo-dependencies
      :keys "C-c C-x o"]
     "--"
     ["Set Priority" org-priority t]
     ["Priority Up" org-shiftup t]
     ["Priority Down" org-shiftdown t]
     "--"
     ["Get News From All Feeds" org-feed-update-all t]
     ["Go to Inbox of Feed..." org-feed-goto-inbox t]
     ["Customize Feeds" (customize-variable 'org-feed-alist) t])
    ("Tags and Properties"
     ["Set Tags" org-set-tags-command (not (org-before-first-heading-p))]
     ["Change Tag in Region" org-change-tag-in-region (org-region-active-p)]
     "--"
     ["Set Property" org-set-property (not (org-before-first-heading-p))]
     ["Column View of Properties" org-columns t]
     ["Insert Column View DBlock" org-columns-insert-dblock t])
    ("Dates and Scheduling"
     ["Timestamp" org-timestamp (not (org-before-first-heading-p))]
     ["Timestamp (Inactive)" org-timestamp-inactive (not (org-before-first-heading-p))]
     ("Change Date"
      ["1 Day Later" org-shiftright (org-at-timestamp-p 'lax)]
      ["1 Day Earlier" org-shiftleft (org-at-timestamp-p 'lax)]
      ["1 ... Later" org-shiftup (org-at-timestamp-p 'lax)]
      ["1 ... Earlier" org-shiftdown (org-at-timestamp-p 'lax)])
     ["Compute Time Range" org-evaluate-time-range t]
     ["Schedule Item" org-schedule (not (org-before-first-heading-p))]
     ["Deadline" org-deadline (not (org-before-first-heading-p))]
     "--"
     ["Custom Time Format" org-toggle-timestamp-overlays
      :style radio :selected org-display-custom-times]
     "--"
     ["Go to Calendar" org-goto-calendar t]
     ["Date from Calendar" org-date-from-calendar t]
     "--"
     ["Start/Restart Timer" org-timer-start t]
     ["Pause/Continue Timer" org-timer-pause-or-continue t]
     ["Stop Timer" org-timer-pause-or-continue :active t :keys "C-u C-c C-x ,"]
     ["Insert Timer String" org-timer t]
     ["Insert Timer Item" org-timer-item t])
    ("Logging work"
     ["Clock In" org-clock-in :active t :keys "C-c C-x C-i"]
     ["Switch Task" ,(lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"]
     ["Clock Out" org-clock-out t]
     ["Clock Cancel" org-clock-cancel t]
     "--"
     ["Mark as Default Task" org-clock-mark-default-task t]
     ["Clock In, Mark as Default" ,(lambda () (interactive) (org-clock-in '(16))) :active t :keys "C-u C-u C-c C-x C-i"]
     ["Go to Running Clock" org-clock-goto t]
     "--"
     ["Display Times" org-clock-display t]
     ["Create Clock Table" org-clock-report t]
     "--"
     ["Record DONE Time"
      (progn (setq org-log-done (not org-log-done))
	     (message "Switching to %s will %s record a timestamp"
		      (car org-done-keywords)
		      (if org-log-done "automatically" "not")))
      :style toggle :selected org-log-done])
    "--"
    ["Agenda Command..." org-agenda t]
    ["Set Restriction Lock" org-agenda-set-restriction-lock t]
    ("File List for Agenda")
    ("Special Views Current File"
     ["TODO Tree"  org-show-todo-tree t]
     ["Check Deadlines" org-check-deadlines t]
     ["Tags/Property Tree" org-match-sparse-tree t])
    "--"
    ["Export/Publish..." org-export-dispatch t]
    ("LaTeX"
     ["Org CDLaTeX mode" org-cdlatex-mode :active (require 'cdlatex nil t)
      :style toggle :selected org-cdlatex-mode]
     ["Insert Environment" cdlatex-environment (fboundp 'cdlatex-environment)]
     ["Insert Math Symbol" cdlatex-math-symbol (fboundp 'cdlatex-math-symbol)]
     ["Modify Math Symbol" org-cdlatex-math-modify
      (org-inside-LaTeX-fragment-p)]
     ["Insert Citation" org-reftex-citation t])
    "--"
    ("Documentation"
     ["Show Version" org-version t]
     ["Show Org Manual" org-info t]
     ["Browse Org News" org-browse-news t])
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Expand This Menu" org-create-customize-menu t])
    ["Send Bug Report" org-submit-bug-report t]
    "--"
    ("Restart/Reload"
     ["Restart Org in Current Buffer" org-mode-restart t]
     "--"
     ["Reload Org" org-reload t]
     ["Reload Org Uncompiled" (org-reload t) :active t :keys "C-u C-c C-x !"])))

(easy-menu-define org-tbl-menu org-mode-map "Org Table menu."
  '("Table"
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p)]
    ["Next Field" org-cycle (org-at-table-p)]
    ["Previous Field" org-shifttab (org-at-table-p)]
    ["Next Row" org-return (org-at-table-p)]
    "--"
    ["Blank Field" org-table-blank-field (org-at-table-p)]
    ["Edit Field" org-table-edit-field (org-at-table-p)]
    ["Copy Field from Above" org-table-copy-down (org-at-table-p)]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft (org-at-table-p)]
     ["Move Column Right" org-metaright (org-at-table-p)]
     ["Delete Column" org-shiftmetaleft (org-at-table-p)]
     ["Insert Column" org-shiftmetaright (org-at-table-p)]
     ["Shrink Column" org-table-toggle-column-width (org-at-table-p)])
    ("Row"
     ["Move Row Up" org-metaup (org-at-table-p)]
     ["Move Row Down" org-metadown (org-at-table-p)]
     ["Delete Row" org-shiftmetaup (org-at-table-p)]
     ["Insert Row" org-shiftmetadown (org-at-table-p)]
     ["Sort Lines in Region" org-table-sort-lines (org-at-table-p)]
     "--"
     ["Insert Horizontal Line" org-ctrl-c-minus (org-at-table-p)])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special (org-at-table-p)]
     ["Cut Rectangle" org-cut-special (org-at-table-p)]
     ["Paste Rectangle" org-paste-special (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region (org-at-table-p)])
    "--"
    ("Calculate"
     ["Set Column Formula" org-table-eval-formula (org-at-table-p)]
     ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
     ["Edit Formulas" org-edit-special (org-at-table-p)]
     "--"
     ["Recalculate Line" org-table-recalculate (org-at-table-p)]
     ["Recalculate All" ,(lambda () (interactive) (org-table-recalculate '(4))) :active (org-at-table-p) :keys "C-u C-c *"]
     ["Iterate All" ,(lambda () (interactive) (org-table-recalculate '(16))) :active (org-at-table-p) :keys "C-u C-u C-c *"]
     "--"
     ["Toggle Recalculate Mark" org-table-rotate-recalc-marks (org-at-table-p)]
     "--"
     ["Sum Column/Rectangle" org-table-sum
      (or (org-at-table-p) (org-region-active-p))]
     ["Which Column?" org-table-current-column (org-at-table-p)])
    ["Debug Formulas"
     org-table-toggle-formula-debugger
     :style toggle :selected (bound-and-true-p org-table-formula-debug)]
    ["Show Column/Row Numbers"
     org-table-toggle-coordinate-overlays
     :style toggle
     :selected (bound-and-true-p org-table-overlay-coordinates)]
    "--"
    ["Create" org-table-create (not (org-at-table-p))]
    ["Convert Region" org-table-convert-region (not (org-at-table-p 'any))]
    ["Import from File" org-table-import (not (org-at-table-p))]
    ["Export to File" org-table-export (org-at-table-p)]
    "--"
    ["Create/Convert 'table.el' Table" org-table-create-with-table.el t]
    "--"
    ("Plot"
     ["ASCII Plot" orgtbl-ascii-plot :active (org-at-table-p) :keys "C-c \" a"]
     ["Gnuplot Plot" org-plot/gnuplot :active (org-at-table-p) :keys "C-c \" g"])))

(defun org-info (&optional node)
  "Read documentation for Org in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org)%s" (or node ""))))

(defun org-browse-news ()
  "Browse the news for the latest major release."
  (interactive)
  (browse-url "https://orgmode.org/Changes.html"))

;;;###autoload
(defun org-submit-bug-report ()
  "Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "emacs-orgmode@gnu.org"
     (org-version nil 'full)
     (let (list)
       (save-window-excursion
	 (pop-to-buffer
          (get-buffer-create "*Warn about privacy*")
          '(org-display-buffer-full-frame))
	 (erase-buffer)
	 (insert "You are about to submit a bug report to the Org mailing list.

If your report is about Org installation, please read this section:
https://orgmode.org/org.html#Installation

Please read https://orgmode.org/org.html#Feedback on how to make
a good report, it will help Org contributors fixing your problem.

Search https://lists.gnu.org/archive/html/emacs-orgmode/ to see
if the issue you are about to raise has already been dealt with.

We also would like to add your full Org and Outline configuration
to the bug report.  It will help us debugging the issue.

*HOWEVER*, some variables you have customized may contain private
information.  The names of customers, colleagues, or friends, might
appear in the form of file names, tags, todo states or search strings.
If you answer \"yes\" to the prompt, you might want to check and remove
such private information before sending the email.")
	 (add-text-properties (point-min) (point-max) '(face org-warning))
         (when (yes-or-no-p "Include your Org configuration and Org warning log?")
	   (mapatoms
	    (lambda (v)
	      (and (boundp v)
		   (string-match "\\`\\(org-\\|outline-\\)" (symbol-name v))
		   (or (and (symbol-value v)
			    (string-match "\\(-hook\\|-function\\)\\'" (symbol-name v)))
                       (eq v 'org--warnings)
		       (and
			(get v 'custom-type) (get v 'standard-value)
			(not (equal (symbol-value v)
			            (eval (car (get v 'standard-value)) t)))))
		   (push v list)))))
	 (kill-buffer (get-buffer "*Warn about privacy*"))
	 list))
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.  You don't know how to make a good report?  See

     https://orgmode.org/manual/Feedback.html#Feedback

Your bug report will be posted to the Org mailing list.
------------------------------------------------------------------------")
    (save-excursion
      (when (re-search-backward "^\\(Subject: \\)Org mode version \\(.*?\\);[ \t]*\\(.*\\)" nil t)
	(replace-match "\\1[BUG] \\3 [\\2]")))))

;;;###autoload
(defun org-submit-feature-request ()
  "Submit a feature request to Org mode.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (let ((reporter-prompt-for-summary-p "Feature request subject: "))
    (reporter-submit-bug-report
     "emacs-orgmode@gnu.org"
     nil nil)
    (save-excursion
      (when (re-search-backward "^\\(Subject: \\)[ \t]*\\(.*\\)" nil t)
	(replace-match "\\1[FR] \\2")))))

;;;###autoload
(defun org-submit-patch ()
  "Submit a patch for Org via mail.

Don't hesitate to submit unfinished patches and do not try too hard
to follow every possible rule listed in
https://orgmode.org/worg/org-contribute.html.  Just send what you have
and we will help you along the way.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (let ((reporter-prompt-for-summary-p "Patch subject: "))
    (reporter-submit-bug-report
     "emacs-orgmode@gnu.org"
     nil nil nil nil
     "Please put your patch as email attachment and briefly describe its overall purpose.
You may take a look at

https://orgmode.org/worg/org-contribute.html

but you do not need to stress too much over following all the rules.
We will guide you along.

Your patch will be posted to the Org mailing list.
------------------------------------------------------------------------")
    (save-excursion
      (when (re-search-backward "^\\(Subject: \\)[ \t]*\\(.*\\)" nil t)
	(replace-match "\\1[PATCH] \\2")))))

;;;; Documentation

(defun org-require-autoloaded-modules ()
  (interactive)
  (mapc #'require
	'(org-agenda org-archive org-attach org-clock org-colview org-id
		     org-table org-timer)))

;;;###autoload
(defun org-reload (&optional uncompiled)
  "Reload all Org Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (interactive "P")
  (require 'loadhist)
  (let* ((org-dir     (org-find-library-dir "org"))
	 (contrib-dir (or (org-find-library-dir "org-contribdir") org-dir))
	 (feature-re "^\\(org\\|ob\\|ox\\|ol\\|oc\\)\\(-.*\\)?")
	 (remove-re (format "\\`%s\\'"
			    (regexp-opt '("org" "org-loaddefs" "org-version"))))
	 (feats (delete-dups
		 (mapcar #'file-name-sans-extension
			 (mapcar #'file-name-nondirectory
				 (delq nil
				       (mapcar #'feature-file
					       features))))))
	 (lfeat (append
		 (sort
		  (setq feats
			(delq nil (mapcar
				   (lambda (f)
				     (if (and (string-match feature-re f)
					      (not (string-match remove-re f)))
					 f nil))
				   feats)))
		  'string-lessp)
		 (list "org-version" "org")))
	 (load-suffixes (if uncompiled (reverse load-suffixes) load-suffixes))
	 load-uncore load-misses)
    (setq load-misses
	  (delq t
		(mapcar (lambda (f)
			  (or (org-load-noerror-mustsuffix (concat org-dir f))
			      (and (string= org-dir contrib-dir)
				   (org-load-noerror-mustsuffix (concat contrib-dir f)))
			      (and (org-load-noerror-mustsuffix (concat (org-find-library-dir f) f))
				   (push f load-uncore)
				   t)
			      f))
			lfeat)))
    (when load-uncore
      (message "The following feature%s found in load-path, please check if that's correct:\n%s"
	       (if (> (length load-uncore) 1) "s were" " was")
               (reverse load-uncore)))
    (if load-misses
	(message "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
		 (if (> (length load-misses) 1) "s" "") load-misses (org-version nil 'full))
      (message "Successfully reloaded Org\n%s" (org-version nil 'full)))))

;;;###autoload
(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (customize-browse 'org))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org mode, insert it into the menu."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (easy-menu-change
   '("Org") "Customize"
   `(["Browse Org group" org-customize t]
     "--"
     ,(customize-menu-create 'org)
     ["Set" Custom-set t]
     ["Save" Custom-save t]
     ["Reset to Current" Custom-reset-current t]
     ["Reset to Saved" Custom-reset-saved t]
     ["Reset to Standard Settings" Custom-reset-standard t]))
  (message "\"Org\"-menu now contains full customization menu"))

;;;; Miscellaneous stuff

;;; Generally useful functions

(defun org-quote-csv-field (s)
  "Quote field for inclusion in CSV material."
  (if (string-match "[\",]" s)
      (concat "\"" (mapconcat #'identity (split-string s "\"") "\"\"") "\"")
    s))

(defun org-force-self-insert (N)
  "Needed to enforce self-insert under remapping."
  (interactive "p" org-mode)
  (self-insert-command N))

(defun org-quote-vert (s)
  "Replace \"|\" with \"\\vert\"."
  (while (string-match "|" s)
    (setq s (replace-match "\\vert" t t s)))
  s)

(defun org-context ()
  "Return a list of contexts of the current cursor position.
If several contexts apply, all are returned.
Each context entry is a list with a symbol naming the context, and
two positions indicating start and end of the context.  Possible
contexts are:

:headline         anywhere in a headline
:headline-stars   on the leading stars in a headline
:todo-keyword     on a TODO keyword (including DONE) in a headline
:tags             on the TAGS in a headline
:priority         on the priority cookie in a headline
:item             on the first line of a plain list item
:item-bullet      on the bullet/number of a plain list item
:checkbox         on the checkbox in a plain list item
:table            in an Org table
:table-special    on a special filed in a table
:table-table      in a table.el table
:clocktable       in a clocktable
:src-block        in a source block
:link             on a hyperlink
:keyword          on a keyword: SCHEDULED, DEADLINE, CLOSE, COMMENT.
:latex-fragment   on a LaTeX fragment
:latex-preview    on a LaTeX fragment with overlaid preview image

This function expects the position to be visible because it uses font-lock
faces as a help to recognize the following contexts: :table-special, :link,
and :keyword."
  (let* ((f (get-text-property (point) 'face))
	 (faces (if (listp f) f (list f)))
	 (case-fold-search t)
	 (p (point)) clist o)
    ;; First the large context
    (cond
     ((org-at-heading-p)
      (push (list :headline (line-beginning-position)
                  (line-end-position))
            clist)
      (when (progn
	      (forward-line 0)
	      (looking-at org-todo-line-tags-regexp))
	(push (org-point-in-group p 1 :headline-stars) clist)
	(push (org-point-in-group p 2 :todo-keyword) clist)
	(push (org-point-in-group p 4 :tags) clist))
      (goto-char p)
      (skip-chars-backward "^[\n\r \t") (or (bobp) (backward-char 1))
      (when (looking-at (org-make-priority-regexp t))
	(push (org-point-in-group p 0 :priority) clist)))

     ((org-at-item-p)
      (push (org-point-in-group p 2 :item-bullet) clist)
      (push (list :item (line-beginning-position)
		  (save-excursion (org-end-of-item) (point)))
	    clist)
      (and (org-at-item-checkbox-p)
	   (push (org-point-in-group p 0 :checkbox) clist)))

     ((org-at-table-p)
      (push (list :table (org-table-begin) (org-table-end)) clist)
      (when (memq 'org-formula faces)
	(push (list :table-special
		    (previous-single-property-change p 'face)
		    (next-single-property-change p 'face))
	      clist)))
     ((org-at-table-p 'any)
      (push (list :table-table) clist)))
    (goto-char p)

    (let ((case-fold-search t))
      ;; New the "medium" contexts: clocktables, source blocks
      (cond ((org-in-clocktable-p)
	     (push (list :clocktable
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN: clocktable\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN: clocktable\\)" nil t))
			      (match-beginning 1))
			 (and (re-search-forward "[ \t]*#\\+END:?" nil t)
			      (match-end 0)))
		   clist))
	    ((org-in-src-block-p)
	     (push (list :src-block
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN_SRC\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN_SRC\\)" nil t))
			      (match-beginning 1))
			 (and (search-forward "#+END_SRC" nil t)
			      (match-beginning 0)))
		   clist))))
    (goto-char p)

    ;; Now the small context
    (cond
     ((org-at-timestamp-p)
      (push (org-point-in-group p 0 :timestamp) clist))
     ((memq 'org-link faces)
      (push (list :link
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face))
	    clist))
     ((memq 'org-special-keyword faces)
      (push (list :keyword
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face))
	    clist))
     ((setq o (cl-some
	       (lambda (o)
		 (and (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
		      o))
	       (overlays-at (point))))
      (push (list :latex-fragment
		  (overlay-start o) (overlay-end o))
	    clist)
      (push (list :latex-preview
		  (overlay-start o) (overlay-end o))
	    clist))
     ((org-inside-LaTeX-fragment-p)
      ;; FIXME: positions wrong.
      (push (list :latex-fragment (point) (point)) clist)))

    (setq clist (nreverse (delq nil clist)))
    clist))

(defun org-between-regexps-p (start-re end-re &optional lim-up lim-down)
  "Non-nil when point is between matches of START-RE and END-RE.

Also return a non-nil value when point is on one of the matches.

Optional arguments LIM-UP and LIM-DOWN bound the search; they are
buffer positions.  Default values are the positions of headlines
surrounding the point.

The functions returns a cons cell whose car (resp. cdr) is the
position before START-RE (resp. after END-RE)."
  (save-match-data
    (let ((pos (point))
	  (limit-up (or lim-up (save-excursion (outline-previous-heading))))
	  (limit-down (or lim-down (save-excursion (outline-next-heading))))
	  beg end)
      (save-excursion
	;; Point is on a block when on START-RE or if START-RE can be
	;; found before it...
	(and (or (org-in-regexp start-re)
		 (re-search-backward start-re limit-up t))
	     (setq beg (match-beginning 0))
	     ;; ... and END-RE after it...
	     (goto-char (match-end 0))
	     (re-search-forward end-re limit-down t)
	     (> (setq end (match-end 0)) pos)
	     ;; ... without another START-RE in-between.
	     (goto-char (match-beginning 0))
	     (not (re-search-backward start-re (1+ beg) t))
	     ;; Return value.
	     (cons beg end))))))

;; Defined in org-agenda.el
(defvar org-agenda-restrict)
(defvar org-agenda-restrict-begin)
(defvar org-agenda-restrict-end)
(defun org-occur-in-agenda-files (regexp &optional _nlines)
  "Call `multi-occur' with buffers for all agenda files."
  (interactive "sOrg-files matching: ")
  (let* ((files (org-agenda-files))
	 (tnames (mapcar #'file-truename files))
	 (extra org-agenda-text-search-extra-files)
         (narrows nil))
    (when (and (eq (car extra) 'agenda-archives)
               (not org-agenda-restrict))
      (setq extra (cdr extra))
      (setq files (org-add-archive-files files)))
    (unless org-agenda-restrict
      (dolist (f extra)
        (unless (member (file-truename f) tnames)
	  (unless (member f files) (setq files (append files (list f))))
	  (setq tnames (append tnames (list (file-truename f)))))))
    (multi-occur
     (mapcar (lambda (x)
	       (with-current-buffer
		   ;; FIXME: Why not just (find-file-noselect x)?
		   ;; Is it to avoid the "revert buffer" prompt?
		   (or (get-file-buffer x) (find-file-noselect x))
                 (if (eq (current-buffer) org-agenda-restrict)
		     (progn
                       ;; Save the narrowing state.
                       (push (list (current-buffer) (point-min) (point-max))
                             narrows)
                       (widen)
                       (narrow-to-region org-agenda-restrict-begin
				         org-agenda-restrict-end))
		   (widen))
		 (current-buffer)))
	     files)
     regexp)
    ;; Restore the narrowing.
    (dolist (narrow narrows)
      (with-current-buffer (car narrow)
        (widen)
        (narrow-to-region (nth 1 narrow) (nth 2 narrow))))))

(add-hook 'occur-mode-find-occurrence-hook
	  (lambda () (when (derived-mode-p 'org-mode) (org-fold-reveal))))

(defun org-occur-link-in-agenda-files ()
  "Create a link and search for it in the agendas.
The link is not stored in `org-stored-links', it is just created
for the search purpose."
  (interactive)
  (let ((link (condition-case nil
		  (org-store-link nil)
		(error "Unable to create a link to here"))))
    (org-occur-in-agenda-files (regexp-quote link))))

(defun org-back-over-empty-lines ()
  "Move backwards over whitespace, to the beginning of the first empty line.
Returns the number of empty lines passed."
  (let ((pos (point)))
    (if (cdr (assq 'heading org-blank-before-new-entry))
	(skip-chars-backward " \t\n\r")
      (unless (eobp)
	(forward-line -1)))
    (forward-line 1)
    (goto-char (min (point) pos))
    (count-lines (point) pos)))

;;; TODO: Only called once, from ox-odt which should probably use
;;; org-export-inline-image-p or something.
(defun org-file-image-p (file)
  "Return non-nil if FILE is an image."
  (save-match-data
    (string-match (image-file-name-regexp) file)))

(defun org-get-cursor-date (&optional with-time)
  "Return the date at cursor in as a time.
This works in the calendar and in the agenda, anywhere else it just
returns the current time.
If WITH-TIME is non-nil, returns the time of the event at point (in
the agenda) or the current time of the day; otherwise returns the
earliest time on the cursor date that Org treats as that date
(bearing in mind `org-extend-today-until')."
  (let (date day defd tp hod mod)
    (when with-time
      (setq tp (get-text-property (point) 'time))
      (when (and tp (string-match "\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)" tp))
	(setq hod (string-to-number (match-string 1 tp))
	      mod (string-to-number (match-string 2 tp))))
      (or tp (let ((now (decode-time)))
	       (setq hod (decoded-time-hour now)
		     mod (decoded-time-minute now)))))
    (cond
     ((eq major-mode 'calendar-mode)
      (setq date (calendar-cursor-to-date)
	    defd (org-encode-time 0 (or mod 0) (or hod org-extend-today-until)
                                  (calendar-extract-day date)
                                  (calendar-extract-month date)
                                  (calendar-extract-year date))))
     ((eq major-mode 'org-agenda-mode)
      (setq day (get-text-property (point) 'day))
      (when day
	(setq date (calendar-gregorian-from-absolute day)
	      defd (org-encode-time 0 (or mod 0) (or hod org-extend-today-until)
                                    (calendar-extract-day date)
                                    (calendar-extract-month date)
                                    (calendar-extract-year date))))))
    (or defd (current-time))))

(defun org-mark-subtree (&optional up)
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at
the end.  If a numeric prefix UP is given, move up into the
hierarchy of headlines by UP levels before marking the subtree."
  (interactive "P" org-mode)
  (org-with-limited-levels
   (cond ((org-at-heading-p) (forward-line 0))
	 ((org-before-first-heading-p) (user-error "Not in a subtree"))
	 (t (outline-previous-visible-heading 1))))
  (when up (while (and (> up 0) (org-up-heading-safe)) (cl-decf up)))
  (if (called-interactively-p 'any)
      (call-interactively #'org-mark-element)
    (org-mark-element)))

;;; Fixed Width Areas

(defun org-toggle-fixed-width ()
  "Toggle fixed-width markup.

Add or remove fixed-width markup on current line, whenever it
makes sense.  Return an error otherwise.

If a region is active and if it contains only fixed-width areas
or blank lines, remove all fixed-width markup in it.  If the
region contains anything else, convert all non-fixed-width lines
to fixed-width ones.

Blank lines at the end of the region are ignored unless the
region only contains such lines."
  (interactive nil org-mode)
  (if (not (org-region-active-p))
      ;; No region:
      ;;
      ;; Remove fixed width marker only in a fixed-with element.
      ;;
      ;; Add fixed width maker in paragraphs, in blank lines after
      ;; elements or at the beginning of a headline or an inlinetask,
      ;; and before any one-line elements (e.g., a clock).
      (progn
        (forward-line 0)
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (cond
           ((and (eq type 'fixed-width)
                 (looking-at "[ \t]*\\(:\\(?: \\|$\\)\\)"))
            (replace-match
	     "" nil nil nil (if (= (line-end-position) (match-end 0)) 0 1)))
           ((and (memq type '(babel-call clock comment diary-sexp headline
					 horizontal-rule keyword paragraph
					 planning))
		 (<= (org-element-post-affiliated element) (point)))
            (skip-chars-forward " \t")
            (insert ": "))
           ((and (looking-at-p "[ \t]*$")
                 (or (eq type 'inlinetask)
                     (save-excursion
                       (skip-chars-forward " \r\t\n")
                       (<= (org-element-end element) (point)))))
            (delete-region (point) (line-end-position))
            (org-indent-line)
            (insert ": "))
           (t (user-error "Cannot insert a fixed-width line here")))))
    ;; Region active.
    (let* ((begin (save-excursion
                    (goto-char (region-beginning))
                    (line-beginning-position)))
           (end (copy-marker
                 (save-excursion
                   (goto-char (region-end))
                   (unless (eolp) (forward-line 0))
                   (if (save-excursion (re-search-backward "\\S-" begin t))
                       (progn (skip-chars-backward " \r\t\n") (point))
                     (point)))))
           (all-fixed-width-p
            (catch 'not-all-p
              (save-excursion
                (goto-char begin)
                (skip-chars-forward " \r\t\n")
                (when (eobp) (throw 'not-all-p nil))
                (while (< (point) end)
                  (let ((element (org-element-at-point)))
                    (if (org-element-type-p element 'fixed-width)
                        (goto-char (org-element-end element))
                      (throw 'not-all-p nil))))
                t))))
      (if all-fixed-width-p
          (save-excursion
            (goto-char begin)
            (while (< (point) end)
              (when (looking-at "[ \t]*\\(:\\(?: \\|$\\)\\)")
                (replace-match
                 "" nil nil nil
                 (if (= (line-end-position) (match-end 0)) 0 1)))
              (forward-line)))
        (let ((min-ind (point-max)))
          ;; Find minimum indentation across all lines.
          (save-excursion
            (goto-char begin)
            (if (not (save-excursion (re-search-forward "\\S-" end t)))
                (setq min-ind 0)
              (catch 'zerop
                (while (< (point) end)
                  (unless (looking-at-p "[ \t]*$")
                    (let ((ind (org-current-text-indentation)))
                      (setq min-ind (min min-ind ind))
                      (when (zerop ind) (throw 'zerop t))))
                  (forward-line)))))
          ;; Loop over all lines and add fixed-width markup everywhere
          ;; but in fixed-width lines.
          (save-excursion
            (goto-char begin)
            (while (< (point) end)
              (cond
               ((org-at-heading-p)
                (insert ": ")
                (forward-line)
                (while (and (< (point) end) (looking-at-p "[ \t]*$"))
                  (insert ":")
                  (forward-line)))
               ((looking-at-p "[ \t]*:\\( \\|$\\)")
                (let* ((element (org-element-at-point))
                       (element-end (org-element-end element)))
                  (if (org-element-type-p element 'fixed-width)
                      (progn (goto-char element-end)
                             (skip-chars-backward " \r\t\n")
                             (forward-line))
                    (let ((limit (min end element-end)))
                      (while (< (point) limit)
                        (org-move-to-column min-ind t)
                        (insert ": ")
                        (forward-line))))))
               (t
                (org-move-to-column min-ind t)
                (insert ": ")
                (forward-line)))))))
      (set-marker end nil))))


;;; Other stuff

(defvar reftex-docstruct-symbol)
(defvar org--rds)

(defun org-reftex-citation ()
  "Use `reftex-citation' to insert a citation into the buffer.
This looks for a line like

#+BIBLIOGRAPHY: foo plain option:-d

and derives from it that foo.bib is the bibliography file relevant
for this document.  It then installs the necessary environment for RefTeX
to work in this buffer and calls `reftex-citation'  to insert a citation
into the buffer.

Export of such citations to both LaTeX and HTML is handled by the contributed
package ox-bibtex by Taru Karttunen."
  (interactive nil org-mode)
  (let ((reftex-docstruct-symbol 'org--rds)
	org--rds bib)
    (org-with-wide-buffer
     (let ((case-fold-search t)
	   (re "^[ \t]*#\\+BIBLIOGRAPHY:[ \t]+\\([^ \t\n]+\\)"))
       (if (not (save-excursion
		(or (re-search-forward re nil t)
		    (re-search-backward re nil t))))
	   (user-error "No bibliography defined in file")
	 (setq bib (concat (match-string 1) ".bib")
	       org--rds (list (list 'bib bib))))))
    (call-interactively #'reftex-citation)))

;;;; Functions extending outline functionality

(require 'org-move)

(defun org-kill-line (&optional _arg)
  "Kill line, to tags or end of line.

The behavior of this command depends on the user options
`org-special-ctrl-k' and `org-ctrl-k-protect-subtree' (which
see)."
  (interactive nil org-mode)
  (cond
   ((or (not org-special-ctrl-k)
	(bolp)
	(not (org-at-heading-p)))
    (when (and (org-invisible-p (line-end-position))
	       org-ctrl-k-protect-subtree
	       (or (eq org-ctrl-k-protect-subtree 'error)
		   (not (y-or-n-p "Kill hidden subtree along with headline? "))))
      (user-error
       (substitute-command-keys
	"`\\[org-kill-line]' aborted as it would kill a hidden subtree")))
    (call-interactively
     (if (bound-and-true-p visual-line-mode) #'kill-visual-line #'kill-line)))
   ((org-match-line org-tag-line-re)
    (let ((end (save-excursion
		 (goto-char (match-beginning 1))
		 (skip-chars-backward " \t")
		 (point))))
      (if (<= end (point))		;on tags part
	  (kill-region (point) (line-end-position))
	(kill-region (point) end)))
    ;; Only align tags when we are still on a heading:
    (if (and (org-at-heading-p) org-auto-align-tags) (org-align-tags)))
   (t (kill-region (point) (line-end-position)))))

(defun org-yank (&optional arg)
  "Yank.  If the kill is a subtree, treat it specially.
This command will look at the current kill and check if is a single
subtree, or a series of subtrees[1].  If it passes the test, and if the
cursor is at the beginning of a line or after the stars of a currently
empty headline, then the yank is handled specially.  How exactly depends
on the value of the following variables.

`org-yank-folded-subtrees'
    By default, this variable is non-nil, which results in
    subtree(s) being folded after insertion, except if doing so
    would swallow text after the yanked text.

`org-yank-adjusted-subtrees'
    When non-nil (the default value is nil), the subtree will be
    promoted or demoted in order to fit into the local outline tree
    structure, which means that the level will be adjusted so that it
    becomes the smaller one of the two *visible* surrounding headings.

Any prefix to this command will cause `yank' to be called directly with
no special treatment.  In particular, a simple `\\[universal-argument]' prefix \
will just
plainly yank the text as it is.

\[1] The test checks if the first non-white line is a heading
    and if there are no other headings with fewer stars."
  (interactive "P" org-mode)
  (org-yank-generic 'yank arg))

(defun org-yank-generic (command arg)
  "Perform some yank-like command.

This function implements the behavior described in the `org-yank'
documentation.  However, it has been generalized to work for any
interactive command with similar behavior."

  ;; pretend to be command COMMAND
  (setq this-command command)

  (if arg
      (call-interactively command)

    (let ((subtreep ; is kill a subtree, and the yank position appropriate?
	   (and (org-kill-is-subtree-p)
		(or (bolp)
		    (and (looking-at "[ \t]*$")
			 (string-match
			  "\\`\\*+\\'"
                          (buffer-substring (line-beginning-position) (point)))))))
	  swallowp)
      (cond
       ((and subtreep org-yank-folded-subtrees)
	(let ((beg (point))
	      end)
	  (if (and subtreep org-yank-adjusted-subtrees)
	      (org-paste-subtree nil nil 'for-yank)
	    (call-interactively command))

	  (setq end (point))
	  (goto-char beg)
	  (when (and (bolp) subtreep
		     (not (setq swallowp
			      (org-yank-folding-would-swallow-text beg end))))
	    (org-with-limited-levels
	     (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	     (while (and (< (point) end) (looking-at org-outline-regexp))
	       (org-fold-subtree t)
	       (org-cycle-show-empty-lines 'folded)
	       (condition-case nil
		   (outline-forward-same-level 1)
		 (error (goto-char end))))))
	  (when swallowp
	    (message
	     "Inserted text not folded because that would swallow text"))

	  (goto-char end)
	  (skip-chars-forward " \t\n\r")
	  (forward-line 0)
	  (push-mark beg 'nomsg)))
       ((and subtreep org-yank-adjusted-subtrees)
        (let ((beg (line-beginning-position)))
	  (org-paste-subtree nil nil 'for-yank)
	  (push-mark beg 'nomsg)))
       (t
	(call-interactively command))))))

(defun org-yank-folding-would-swallow-text (beg end)
  "Would `hide-subtree' at BEG swallow any text after END?"
  (let (level)
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (when (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	 (setq level (org-outline-level)))
       (goto-char end)
       (skip-chars-forward " \t\r\n\v\f")
       (not (or (eobp)
	      (and (bolp) (looking-at-p org-outline-regexp)
		   (<= (org-outline-level) level))))))))

(defun org-drag-element-backward ()
  "Move backward element at point."
  (interactive nil org-mode)
  (let ((elem (or (org-element-at-point)
		  (user-error "No element at point"))))
    (if (org-element-type-p elem 'headline)
	;; Preserve point when moving a whole tree, even if point was
	;; on blank lines below the headline.
	(let ((offset (skip-chars-backward " \t\n")))
	  (unwind-protect (org-move-subtree-up)
	    (forward-char (- offset))))
      (let ((prev-elem
	     (save-excursion
	       (goto-char (org-element-begin elem))
	       (skip-chars-backward " \r\t\n")
	       (unless (bobp)
		 (let* ((beg (org-element-begin elem))
			(prev (org-element-at-point))
			(up prev))
		   (while (and (setq up (org-element-parent up))
			       (<= (org-element-end up) beg))
		     (setq prev up))
		   prev)))))
	;; Error out if no previous element or previous element is
	;; a parent of the current one.
	(if (or (not prev-elem) (org-element-nested-p elem prev-elem))
	    (user-error "Cannot drag element backward")
	  (let ((pos (point)))
	    (org-element-swap-A-B prev-elem elem)
	    (goto-char (+ (org-element-begin prev-elem)
			  (- pos (org-element-begin elem))))))))))

(defun org-drag-element-forward ()
  "Move forward element at point."
  (interactive nil org-mode)
  (let* ((pos (point))
	 (elem (or (org-element-at-point)
		   (user-error "No element at point"))))
    (when (= (point-max) (org-element-end elem))
      (user-error "Cannot drag element forward"))
    (goto-char (org-element-end elem))
    (let ((next-elem (org-element-at-point)))
      (when (or (org-element-nested-p elem next-elem)
		(and (org-element-type-p next-elem 'headline)
		     (not (org-element-type-p elem 'headline))))
	(goto-char pos)
	(user-error "Cannot drag element forward"))
      ;; Compute new position of point: it's shifted by NEXT-ELEM
      ;; body's length (without final blanks) and by the length of
      ;; blanks between ELEM and NEXT-ELEM.
      (let ((size-next (- (save-excursion
			    (goto-char (org-element-end next-elem))
			    (skip-chars-backward " \r\t\n")
			    (forward-line)
			    ;; Small correction if buffer doesn't end
			    ;; with a newline character.
			    (if (and (eolp) (not (bolp))) (1+ (point)) (point)))
			  (org-element-begin next-elem)))
	    (size-blank (- (org-element-end elem)
			   (save-excursion
			     (goto-char (org-element-end elem))
			     (skip-chars-backward " \r\t\n")
			     (forward-line)
			     (point)))))
	(org-element-swap-A-B elem next-elem)
	(goto-char (+ pos size-next size-blank))))))

(defun org-drag-line-forward (arg)
  "Drag the line at point ARG lines forward."
  (interactive "p")
  (dotimes (_ (abs arg))
    (let ((c (current-column)))
      (if (< 0 arg)
	  (progn
	    (forward-line 1)
	    (transpose-lines 1)
	    (forward-line -1))
	(transpose-lines 1)
	(forward-line -2))
      (org-move-to-column c))))

(defun org-drag-line-backward (arg)
  "Drag the line at point ARG lines backward."
  (interactive "p")
  (org-drag-line-forward (- arg)))

(defun org-mark-element ()
  "Put point at beginning of this element, mark at end.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next element after the
ones already marked."
  (interactive)
  (let (deactivate-mark)
    (if (and (called-interactively-p 'any)
	     (or (and (eq last-command this-command) (mark t))
		 (and transient-mark-mode mark-active)))
	(set-mark
	 (save-excursion
	   (goto-char (mark))
	   (goto-char (org-element-end (org-element-at-point)))
	   (point)))
      (let ((element (org-element-at-point)))
	(end-of-line)
	(push-mark (min (point-max) (org-element-end element)) t t)
	(goto-char (org-element-begin element))))))

(defun org-narrow-to-element ()
  "Narrow buffer to current element.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive nil org-mode)
  (let ((elem (org-element-at-point)))
    (cond
     ((eq (car elem) 'headline)
      (narrow-to-region
       (org-element-begin elem)
       (org-element-end elem)))
     ((memq (car elem) org-element-greater-elements)
      (narrow-to-region
       (org-element-contents-begin elem)
       (org-element-contents-end elem)))
     (t
      (narrow-to-region
       (org-element-begin elem)
       (org-element-end elem))))))

(defun org-transpose-element ()
  "Transpose current and previous elements, keeping blank lines between.
Point is moved after both elements."
  (interactive nil org-mode)
  (org-skip-whitespace)
  (let ((end (org-element-end (org-element-at-point))))
    (org-drag-element-backward)
    (goto-char end)))



;;; Conveniently switch to Info nodes

(defun org-info-find-node (&optional nodename)
  "Find Info documentation NODENAME or Org documentation according context.
Started from `gnus-info-find-node'."
  (interactive)
  (Info-goto-node
   (or nodename
       (let ((default-org-info-node "(org) Top"))
         (cond
          ((eq 'org-agenda-mode major-mode) "(org) Agenda Views")
          ((eq 'org-mode major-mode)
           (let* ((context (org-element-at-point))
                  (element-info-nodes ; compare to `org-element-all-elements'.
                   `((babel-call . "(org) Evaluating Code Blocks")
                     (center-block . "(org) Paragraphs")
                     (clock . ,default-org-info-node)
                     (comment . "(org) Comment Lines")
                     (comment-block . "(org) Comment Lines")
                     (diary-sexp . ,default-org-info-node)
                     (drawer . "(org) Drawers")
                     (dynamic-block . "(org) Dynamic Blocks")
                     (example-block . "(org) Literal Examples")
                     (export-block . "(org) ASCII/Latin-1/UTF-8 export")
                     (fixed-width . ,default-org-info-node)
                     (footnote-definition . "(org) Creating Footnotes")
                     (headline . "(org) Document Structure")
                     (horizontal-rule . "(org) Built-in Table Editor")
                     (inlinetask . ,default-org-info-node)
                     (item . "(org) Plain Lists")
                     (keyword . "(org) Per-file keywords")
                     (latex-environment . "(org) LaTeX Export")
                     (node-property . "(org) Properties and Columns")
                     (paragraph . "(org) Paragraphs")
                     (plain-list . "(org) Plain Lists")
                     (planning . "(org) Deadlines and Scheduling")
                     (property-drawer . "(org) Properties and Columns")
                     (quote-block . "(org) Paragraphs")
                     (section . ,default-org-info-node)
                     (special-block . ,default-org-info-node)
                     (src-block . "(org) Working with Source Code")
                     (table . "(org) Tables")
                     (table-row . "(org) Tables")
                     (verse-block . "(org) Paragraphs"))))
             (or (cdr (assoc (car context) element-info-nodes))
                 default-org-info-node)))
          (t default-org-info-node))))))



(provide 'org)

(run-hooks 'org-load-hook)

;;; org.el ends here
