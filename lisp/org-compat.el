;;; org-compat.el --- Compatibility Code for Older Emacsen -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2026 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains code needed for compatibility with older
;; versions of GNU Emacs and integration with other packages.

;;; Code:


(require 'cl-lib)
(require 'seq)
(require 'org-macs)

(require 'compat)

(eval-when-compile (require 'subr-x))  ; Emacs < 28

(org-assert-version)

(declare-function org-agenda-diary-entry "org-agenda")
(declare-function org-agenda-maybe-redo "org-agenda" ())
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-remove-restriction-lock "org-agenda" (&optional noupdate))
(declare-function org-calendar-goto-agenda "org-agenda" ())
(declare-function org-align-tags "org" (&optional all))
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-at-table.el-p "org-table" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-at-point-no-context "org-element" (&optional pom))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element-ast" (blob &optional types with-self))
(declare-function org-element-type "org-element-ast" (node &optional anonymous))
(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-begin "org-element" (node))
(declare-function org-element-end "org-element" (node))
(declare-function org-element-contents-begin "org-element" (node))
(declare-function org-element-contents-end "org-element" (node))
(declare-function org-element-post-affiliated "org-element" (node))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading element))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-fold-hide-block-toggle "org-fold" (&optional force no-error element))
(declare-function org-link-display-format "ol" (s))
(declare-function org-link-set-parameters "ol" (type &rest rest))
(declare-function org-make-tag-string "org" (tags))
(declare-function org-next-visible-heading "org" (arg))
(declare-function org-reduced-level "org" (l))
(declare-function org-return "org" (&optional indent arg interactive))
(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function outline-next-heading "outline" ())
(declare-function speedbar-line-directory "speedbar" (&optional depth))
(declare-function table--at-cell-p "table" (position &optional object at-column))
(declare-function ob-clojure-eval-with-cmd "ob-clojure" (cmd expanded))
(declare-function org-fold-hide-sublevels "org-fold" (levels))
(declare-function org-fold-hide-subtree "org-fold" ())
(declare-function org-fold-region "org-fold" (from to flag &optional spec))
(declare-function org-fold-show-all "org-fold" (&optional types))
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function decoded-time-add "time-date" (time delta))

(defvar calendar-mode-map)
(defvar org-complex-heading-regexp)
(defvar org-agenda-diary-file)
(defvar org-agenda-overriding-restriction)
(defvar org-agenda-restriction-lock-overlay)
(defvar org-table-any-border-regexp)
(defvar org-table-dataline-regexp)
(defvar org-table-tab-recognizes-table.el)
(defvar org-table1-hline-regexp)
(defvar org-fold-core-style)

;; Obsolete compatibility wrappers used before inclusion of compat.el.

(define-obsolete-function-alias 'org-file-has-changed-p
  'file-has-changed-p "10.0")
(define-obsolete-function-alias 'org-string-equal-ignore-case
  'string-equal-ignore-case "10.0")
(define-obsolete-function-alias 'org-file-name-concat
  'file-name-concat "10.0")
(define-obsolete-function-alias 'org-directory-empty-p
  'directory-empty-p "10.0")
(define-obsolete-function-alias 'org-string-clean-whitespace
  'string-clean-whitespace "10.0")
(define-obsolete-function-alias 'org-format-prompt
  'format-prompt "10.0")
(define-obsolete-function-alias 'org-xor
  'xor "10.0")
(define-obsolete-function-alias 'org-string-distance
  'string-distance "10.0")
(define-obsolete-function-alias 'org-buffer-hash
  'buffer-hash "10.0")
(define-obsolete-function-alias 'org-list-of-strings-p
  'list-of-strings-p "10.0")
(define-obsolete-function-alias 'org-assoc-delete-all
  'assoc-delete-all "10.0")
(define-obsolete-function-alias 'org--flatten-tree
  'flatten-tree "10.0")
(define-obsolete-function-alias 'org-completion-table-with-metadata
  'completion-table-with-metadata "10.0")
(define-obsolete-function-alias 'org-babel-local-file-name
  'file-local-name "10.0")


;;; Emacs < 31 compatibility

;; Broken for negative months before emacs commit bc33b70b280
(if (version< "31" emacs-version)
    (defalias 'org-decoded-time-add #'decoded-time-add)
  (defun org-decoded-time-add (time delta)
    "See doctring for `decoded-time-add'."
    (if (decoded-time-month delta)
        (let ((time (copy-sequence time))
              (delta (copy-sequence delta)))
          (let ((new (+ (1- (decoded-time-month time)) (decoded-time-month delta))))
            (setf (decoded-time-month time) (1+ (mod new 12)))
            (cl-incf (decoded-time-year time) (floor new 12)))
          (setf (decoded-time-month delta) nil)
          (decoded-time-add time delta))
      (decoded-time-add time delta))))


;;; Emacs < 29 compatibility

(if (fboundp 'display-buffer-full-frame)
    (defalias 'org-display-buffer-full-frame #'display-buffer-full-frame)
  (defun org-display-buffer-full-frame (buffer alist)
    "Display BUFFER in the current frame, taking the entire frame.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
    (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                            (display-buffer-same-window buffer alist)
                            (display-buffer-pop-up-window buffer alist)
                            (display-buffer-use-some-window buffer alist))))
      (delete-other-windows window)
      window)))

(defun org-buffer-text-pixel-width ()
  "Return pixel width of text in current buffer.
This function uses `buffer-text-pixel-size', when available, and falls
back to `window-text-pixel-size' otherwise."
  (if (fboundp 'buffer-text-pixel-size)
      (car (buffer-text-pixel-size nil nil t))
    (if (get-buffer-window (current-buffer))
        ;; FIXME: 10000 because `most-positive-fixnum' ain't working
        ;; (tests failing) and this call will be removed after we drop
        ;; Emacs 28 support anyway.
        (car (window-text-pixel-size
              nil (point-min) (point-max) 10000))
      (let ((dedicatedp (window-dedicated-p))
            (oldbuffer (window-buffer)))
        (unwind-protect
            (progn
              ;; Do not throw error in dedicated windows.
              (set-window-dedicated-p nil nil)
              (set-window-buffer nil (current-buffer))
              (car (window-text-pixel-size
                    nil (point-min) (point-max) 10000)))
          (set-window-buffer nil oldbuffer)
          (set-window-dedicated-p nil dedicatedp))))))


(if (fboundp 'with-undo-amalgamate)
    (defalias 'org-with-undo-amalgamate 'with-undo-amalgamate)
  ;; Copied from Emacs source.
  (defmacro org-with-undo-amalgamate (&rest body)
    "Like `progn' but perform BODY with amalgamated undo barriers.

This allows multiple operations to be undone in a single step.
When undo is disabled this behaves like `progn'."
    (declare (indent 0) (debug t))
    (let ((handle (make-symbol "--change-group-handle--")))
      `(let ((,handle (prepare-change-group))
             ;; Don't truncate any undo data in the middle of this,
             ;; otherwise Emacs might truncate part of the resulting
             ;; undo step: we want to mimic the behavior we'd get if the
             ;; undo-boundaries were never added in the first place.
             (undo-outer-limit nil)
             (undo-limit most-positive-fixnum)
             (undo-strong-limit most-positive-fixnum))
         (unwind-protect
             (progn
               (activate-change-group ,handle)
               ,@body)
           (progn
             (accept-change-group ,handle)
             (undo-amalgamate-change-group ,handle)))))))


;;; Emacs < 28.1 compatibility

(if (= 2 (cdr (subr-arity (symbol-function 'get-buffer-create))))
    ;; Emacs >27.
    (defalias 'org-get-buffer-create #'get-buffer-create)
  (defun org-get-buffer-create (buffer-or-name &optional _)
    "Call `get-buffer-create' with BUFFER-OR-NAME argument.
Ignore optional argument."
    (get-buffer-create buffer-or-name)))


;;; Emacs < 27.1 compatibility

(if (version< emacs-version "29")
    ;; A stub when `combine-change-calls' was not yet there or had
    ;; critical bugs (see Emacs bug#60467).
    (defmacro org-combine-change-calls (_beg _end &rest body)
      (declare (debug (form form def-body)) (indent 2))
      `(progn ,@body))
  (defalias 'org-combine-change-calls 'combine-change-calls))

(with-no-warnings ; `replace-buffer-contents' is obsolete in Emacs 31
  (cond
   ((version< emacs-version "27.1")
    (defsubst org-replace-buffer-contents (source &optional _max-secs _max-costs)
      (replace-buffer-contents source)))
   ((version< emacs-version "31")
    (defalias 'org-replace-buffer-contents #'replace-buffer-contents))
   (t
    (defsubst org-replace-buffer-contents (source &optional max-secs max-costs)
      (replace-region-contents (point-min) (point-max) source max-secs max-costs)))))

(unless (fboundp 'pcomplete-uniquify-list)
  ;; The misspelled variant was made obsolete in Emacs 27.1
  (defalias 'pcomplete-uniquify-list 'pcomplete-uniqify-list))

(if (fboundp 'time-convert)
    (progn
      (defsubst org-time-convert-to-integer (time)
	(time-convert time 'integer))
      (defsubst org-time-convert-to-list (time)
	(time-convert time 'list)))
  (defun org-time-convert-to-integer (time)
    (floor (float-time time)))
  (defun org-time-convert-to-list (time)
    (seconds-to-time (float-time time))))

;; `newline-and-indent' did not take a numeric argument before 27.1.
(if (version< emacs-version "27")
    (defsubst org-newline-and-indent (&optional _arg)
      (newline-and-indent))
  (defalias 'org-newline-and-indent #'newline-and-indent))

(defun org--set-faces-extend (faces extend-p)
  "Set the :extend attribute of FACES to EXTEND-P.

This is a no-op for Emacs versions lower than 27, since face
extension beyond end of line was not controllable."
  (when (fboundp 'set-face-extend)
    (mapc (lambda (f) (set-face-extend f extend-p)) faces)))

(define-obsolete-function-alias 'org-babel-edit-distance 'string-distance
  "9.5")

(unless (fboundp 'with-connection-local-variables)
  ;; Added in Emacs 27: commit:21f54feee8, 2019-03-09.
  ;; Redefining it using the old function `with-connection-local-profiles'.
  (defmacro with-connection-local-variables (&rest body)
    "Apply connection-local variables according to `default-directory'.
Execute BODY, and unwind connection-local variables."
    (declare (debug t))
    `(with-connection-local-profiles (connection-local-get-profiles nil)
       ,@body)))


;;; Emacs < 26.1 compatibility

(if (fboundp 'line-number-display-width)
    (defalias 'org-line-number-display-width 'line-number-display-width)
  (defun org-line-number-display-width (&rest _) 0))


;;; Obsolete aliases (remove them after the next major release).

;;;; XEmacs compatibility, now removed.
(define-obsolete-function-alias 'org-activate-mark 'activate-mark "9.0")
(define-obsolete-function-alias 'org-add-hook 'add-hook "9.0")
(define-obsolete-function-alias 'org-bound-and-true-p 'bound-and-true-p "9.0")
(define-obsolete-function-alias 'org-decompose-region 'decompose-region "9.0")
(define-obsolete-function-alias 'org-defvaralias 'defvaralias "9.0")
(define-obsolete-function-alias 'org-detach-overlay 'delete-overlay "9.0")
(define-obsolete-function-alias 'org-file-equal-p 'file-equal-p "9.0")
(define-obsolete-function-alias 'org-float-time 'float-time "9.0")
(define-obsolete-function-alias 'org-indent-line-to 'indent-line-to "9.0")
(define-obsolete-function-alias 'org-indent-to-column 'indent-to-column "9.0")
(define-obsolete-function-alias 'org-looking-at-p 'looking-at-p "9.0")
(define-obsolete-function-alias 'org-looking-back 'looking-back "9.0")
(define-obsolete-function-alias 'org-match-string-no-properties 'match-string-no-properties "9.0")
(define-obsolete-function-alias 'org-propertize 'propertize "9.0")
(define-obsolete-function-alias 'org-select-frame-set-input-focus 'select-frame-set-input-focus "9.0")
(define-obsolete-function-alias 'org-file-remote-p 'file-remote-p "9.2")

(define-obsolete-function-alias 'org-show-context 'org-fold-show-context "9.6")
(define-obsolete-function-alias 'org-show-entry 'org-fold-show-entry "9.6")
(define-obsolete-function-alias 'org-show-children 'org-fold-show-children "9.6")


(defmacro org-re (s)
  "Replace posix classes in regular expression S."
  (declare (debug (form))
           (obsolete "you can safely remove it." "9.0"))
  s)

;;;; Functions from cl-lib that Org used to have its own implementation of.
(define-obsolete-function-alias 'org-count 'cl-count "9.0")
(define-obsolete-function-alias 'org-every 'cl-every "9.0")
(define-obsolete-function-alias 'org-find-if 'cl-find-if "9.0")
(define-obsolete-function-alias 'org-reduce 'cl-reduce "9.0")
(define-obsolete-function-alias 'org-remove-if 'cl-remove-if "9.0")
(define-obsolete-function-alias 'org-remove-if-not 'cl-remove-if-not "9.0")
(define-obsolete-function-alias 'org-some 'cl-some "9.0")
(define-obsolete-function-alias 'org-floor* 'cl-floor "9.0")

(defun org-sublist (list start end)
  "Return a section of LIST, from START to END.
Counting starts at 1."
  (cl-subseq list (1- start) end))
(make-obsolete 'org-sublist
               "use cl-subseq (note the 0-based counting)."
               "9.0")

;;;; Functions available since Emacs 25.1
(define-obsolete-function-alias 'org-string-collate-lessp 'string-collate-lessp "9.6")
(define-obsolete-function-alias 'org-decode-time 'decode-time "9.6")
(define-obsolete-function-alias 'org-format-time-string 'format-time-string "9.6")
(define-obsolete-function-alias 'org-time-add 'time-add "9.6")
(define-obsolete-function-alias 'org-time-subtract 'time-subtract "9.6")
(define-obsolete-function-alias 'org-time-since 'time-since "9.6")
(define-obsolete-function-alias 'org-time-less-p 'time-less-p "9.6")

;;;; Functions available since Emacs 24.3
(define-obsolete-function-alias 'org-buffer-narrowed-p 'buffer-narrowed-p "9.0")
(define-obsolete-function-alias 'org-called-interactively-p 'called-interactively-p "9.0")
(define-obsolete-function-alias 'org-char-to-string 'char-to-string "9.0")
(define-obsolete-function-alias 'org-delete-directory 'delete-directory "9.0")
(define-obsolete-function-alias 'org-format-seconds 'format-seconds "9.0")
(define-obsolete-function-alias 'org-link-escape-browser 'url-encode-url "9.0")
(define-obsolete-function-alias 'org-no-warnings 'with-no-warnings "9.0")
(define-obsolete-function-alias 'org-number-sequence 'number-sequence "9.0")
(define-obsolete-function-alias 'org-pop-to-buffer-same-window 'pop-to-buffer-same-window "9.0")
(define-obsolete-function-alias 'org-string-match-p 'string-match-p "9.0")

;;;; Functions and variables from previous releases now obsolete.
(define-obsolete-variable-alias 'org-checkbox-hierarchical-statistics
  'org-checkbox-children-only-statistics "Org 10.0")
(define-obsolete-variable-alias 'org-hierarchical-todo-statistics
  'org-todo-children-only-statistics "Org 10.0")
(define-obsolete-variable-alias 'org-edit-src-content-indentation
  'org-src-content-indentation "Org 9.8")
(define-obsolete-variable-alias 'org-export-ignored-local-variables
  'org-element-ignored-local-variables "Org 9.7")
(define-obsolete-function-alias 'org-habit-get-priority
  'org-habit-get-urgency "Org 9.7")
(define-obsolete-function-alias 'org-timestamp-format
  'org-format-timestamp "Org 9.6")
(define-obsolete-variable-alias 'org-export-before-processing-hook
  'org-export-before-processing-functions "Org 9.6")
(define-obsolete-variable-alias 'org-export-before-parsing-hook
  'org-export-before-parsing-functions "Org 9.6")
(define-obsolete-function-alias 'org-element-remove-indentation
  'org-remove-indentation "9.0")
(define-obsolete-variable-alias 'org-latex-listings
  'org-latex-src-block-backend "9.6")
(define-obsolete-function-alias 'org-table-p 'org-at-table-p "9.0")
(define-obsolete-function-alias 'org-on-heading-p 'org-at-heading-p "9.0")
(define-obsolete-function-alias 'org-at-regexp-p 'org-in-regexp "8.3")
(define-obsolete-function-alias 'org-image-file-name-regexp
  'image-file-name-regexp "9.0")
(define-obsolete-function-alias 'org-completing-read-no-i
  'completing-read "9.0")
(define-obsolete-function-alias 'org-icompleting-read
  'completing-read "9.0")
(define-obsolete-function-alias 'org-iread-file-name 'read-file-name "9.0")
(define-obsolete-function-alias 'org-days-to-time
  'org-timestamp-to-now "8.2")
(define-obsolete-variable-alias 'org-agenda-ignore-drawer-properties
  'org-agenda-ignore-properties "9.0")
(define-obsolete-function-alias 'org-preview-latex-fragment
  'org-toggle-latex-fragment "8.3")
(define-obsolete-function-alias 'org-export-get-genealogy
  'org-element-lineage "9.0")
(define-obsolete-variable-alias 'org-latex-with-hyperref
  'org-latex-hyperref-template "9.0")
(define-obsolete-variable-alias 'hfy-optimisations 'hfy-optimizations "9.0")
(define-obsolete-variable-alias 'org-export-htmlized-org-css-url
  'org-org-htmlized-css-url "8.2")
(define-obsolete-function-alias 'org-list-parse-list 'org-list-to-lisp "9.0")
(define-obsolete-function-alias 'org-agenda-todayp
  'org-agenda-today-p "9.0")
(define-obsolete-function-alias 'org-babel-examplize-region
  'org-babel-examplify-region "9.0")
(define-obsolete-variable-alias 'org-babel-capitalize-example-region-markers
  'org-babel-uppercase-example-markers "9.1")

(define-obsolete-function-alias 'org-babel-trim 'org-trim "9.0")
(define-obsolete-variable-alias 'org-html-style 'org-html-head "24.4")
(define-obsolete-function-alias 'org-insert-columns-dblock
  'org-columns-insert-dblock "9.0")
(define-obsolete-variable-alias 'org-export-babel-evaluate
  'org-export-use-babel "9.1")
(define-obsolete-function-alias 'org-activate-bracket-links
  'org-activate-links "9.0")
(define-obsolete-function-alias 'org-activate-plain-links 'ignore "9.0")
(define-obsolete-function-alias 'org-activate-angle-links 'ignore "9.0")
(define-obsolete-function-alias 'org-remove-double-quotes 'org-strip-quotes "9.0")
(define-obsolete-function-alias 'org-get-indentation
  'current-indentation "9.2")
(define-obsolete-function-alias 'org-capture-member 'org-capture-get "9.2")
(define-obsolete-function-alias 'org-remove-from-invisibility-spec
  'remove-from-invisibility-spec "9.2")

(define-obsolete-variable-alias 'org-effort-durations 'org-duration-units
  "9.2")

(define-obsolete-function-alias 'org-toggle-latex-fragment 'org-latex-preview
  "9.3")

(define-obsolete-function-alias 'org-hide-archived-subtrees
  'org-fold-hide-archived-subtrees "9.6")

(define-obsolete-function-alias 'org-flag-region
  'org-fold-region "9.6")

(define-obsolete-function-alias 'org-flag-subtree
  'org-fold-subtree "9.6")

(define-obsolete-function-alias 'org-hide-entry
  'org-fold-hide-entry "9.6")

(define-obsolete-function-alias 'org-show-subtree
  'org-fold-show-subtree "9.6")

(define-obsolete-function-alias 'org--hide-wrapper-toggle
  'org-fold--hide-wrapper-toggle "9.6")

(define-obsolete-function-alias 'org-hide-block-toggle
  'org-fold-hide-block-toggle "9.6")

(define-obsolete-function-alias 'org-hide-drawer-toggle
  'org-fold-hide-drawer-toggle "9.6")

(define-obsolete-function-alias 'org--hide-drawers
  'org-fold--hide-drawers "9.6")

(define-obsolete-function-alias 'org-hide-block-all
  'org-fold-hide-block-all "9.6")

(define-obsolete-function-alias 'org-hide-drawer-all
  'org-fold-hide-drawer-all "9.6")

(define-obsolete-function-alias 'org-show-all
  'org-fold-show-all "9.6")

(define-obsolete-function-alias 'org-set-startup-visibility
  'org-cycle-set-startup-visibility "9.6")

(define-obsolete-function-alias 'org-show-set-visibility
  'org-fold-show-set-visibility "9.6")

(define-obsolete-function-alias 'org-check-before-invisible-edit
  'org-fold-check-before-invisible-edit "9.6")

(define-obsolete-function-alias 'org-flag-above-first-heading
  'org-fold-flag-above-first-heading "9.6")

(define-obsolete-function-alias 'org-show-branches-buffer
  'org-fold-show-branches-buffer "9.6")

(define-obsolete-function-alias 'org-show-siblings
  'org-fold-show-siblings "9.6")

(define-obsolete-function-alias 'org-show-hidden-entry
  'org-fold-show-hidden-entry "9.6")

(define-obsolete-function-alias 'org-flag-heading
  'org-fold-heading "9.6")

(define-obsolete-function-alias 'org-set-startup-visibility
  'org-cycle-set-startup-visibility "9.6")

(define-obsolete-function-alias 'org-set-visibility-according-to-property
  'org-cycle-set-visibility-according-to-property "9.6")

(define-obsolete-variable-alias 'org-scroll-position-to-restore
  'org-cycle-scroll-position-to-restore "9.6")
(define-obsolete-function-alias 'org-optimize-window-after-visibility-change
  'org-cycle-optimize-window-after-visibility-change "9.6")

(define-obsolete-function-alias 'org-force-cycle-archived
  'org-cycle-force-archived "9.6")

(define-obsolete-variable-alias 'org-attach-directory
  'org-attach-id-dir "9.3")
(make-obsolete 'org-attach-store-link "No longer used" "9.4")
(make-obsolete 'org-attach-expand-link "No longer used" "9.4")

(define-obsolete-function-alias 'org-file-url-p 'org-url-p "9.5")

(define-obsolete-variable-alias 'org-show-context-detail
  'org-fold-show-context-detail "9.6")

(define-obsolete-variable-alias 'org-catch-invisible-edits
  'org-fold-catch-invisible-edits "9.6")

(define-obsolete-variable-alias 'org-reveal-start-hook
  'org-fold-reveal-start-hook "9.6")
(define-obsolete-function-alias 'org-file-url-p 'org-url-p "9.6")
(define-obsolete-variable-alias 'org-plantuml-executable-args 'org-plantuml-args
  "Org 9.6")

(defvar org-cached-props nil)
(defvar org-use-property-inheritance)
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-entry-properties "org" (&optional epom which))
(defun org-cached-entry-get (pom property)
  (if (or (eq t org-use-property-inheritance)
	  (and (stringp org-use-property-inheritance)
	       (let ((case-fold-search t))
		 (string-match-p org-use-property-inheritance property)))
	  (and (listp org-use-property-inheritance)
	       (member-ignore-case property org-use-property-inheritance)))
      ;; Caching is not possible, check it directly.
      (org-entry-get pom property 'inherit)
    ;; Get all properties, so we can do complicated checks easily.
    (cdr (assoc-string property
		       (or org-cached-props
			   (setq org-cached-props (org-entry-properties pom)))
		       t))))

(make-obsolete 'org-cached-entry-get
               "Performs badly.  Instead use `org-entry-get' with the argument INHERIT set to `selective'"
               "9.7")

(defconst org-latex-line-break-safe "\\\\[0pt]"
  "Linebreak protecting the following [...].

Without \"[0pt]\" it would be interpreted as an optional argument to
the \\\\.

This constant, for example, makes the below code not err:

\\begin{tabular}{c|c}
    [t] & s\\\\[0pt]
    [I] & A\\\\[0pt]
    [m] & kg
\\end{tabular}")
(make-obsolete 'org-latex-line-break-safe
               "should not be used - it is not safe in all the scenarios."
               "9.7")

(defun org-in-fixed-width-region-p ()
  "Non-nil if point in a fixed-width region."
  (save-match-data
    (org-element-type-p (org-element-at-point) 'fixed-width)))
(make-obsolete 'org-in-fixed-width-region-p
               "use `org-element' library"
               "9.0")

(define-obsolete-variable-alias
  'org-format-latex-options 'org-latex-preview-appearance-options
  "10.0"
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  the foreground color for images embedded in Emacs, e.g. \"Black\".
             `default' means use the foreground of the default face.
             `auto' means use the foreground from the text face.
:background  the background color, or \"Transparent\".
             `default' means use the background of the default face.
             `auto' means use the background from the text face.
:scale       a scaling factor for the size of the images, to get more pixels

Support for the following keys is obsolete, use
`org-html-latex-image-options' instead:

:html-foreground, :html-background, :html-scale
the same numbers for HTML export.

Support for the following key is obsolete, use
`org-highlight-latex-matchers' instead:
:matchers    a list indicating which matchers should be used to
             find LaTeX fragments.  Valid members of this list are:
             \"begin\" find environments
             \"$1\"    find single characters surrounded by $.$
             \"$\"     find math expressions surrounded by $...$
             \"$$\"    find math expressions surrounded by $$....$$
             \"\\(\"    find math expressions surrounded by \\(...\\)
             \"\\=\\[\"    find math expressions surrounded by \\=\\[...\\]")

(make-obsolete-variable
 'org-format-latex-signal-error "no longer used" "10.0")

(defcustom org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}"
  "The document header used for processing LaTeX fragments.
It is imperative that this header make sure that no page number
appears on the page.  The package defined in the variables
`org-latex-default-packages-alist' and `org-latex-packages-alist'
will either replace the placeholder \"[PACKAGES]\" in this
header, or they will be appended."
  :group 'org-latex
  :type 'string)

(make-obsolete-variable
 'org-format-latex-header
 "To set the preamble for generating LaTeX preview images, use
`org-latex-preview-preamble' instead.

To generate images from LaTeX fragments programmatically use
`org-latex-preview-create-images' instead of `org-format-latex' or
`org-create-formula-image'."
 "10.0")

(define-obsolete-variable-alias
  'org-format-latex-header 'org-latex-preview-preamble "10.0")
(define-obsolete-variable-alias
  'org-preview-latex-default-process 'org-latex-preview-process-default "10.0")

(defcustom org-preview-latex-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvipng -D %D -T tight -o %O %f")
     :transparent-image-converter
     ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
    (dvisvgm
     :programs ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
    (xelatex
     :programs ("xelatex" "dvisvgm")
     :description "xdv > svg"
     :message "you need to install the programs: xelatex and dvisvgm."
     :image-input-type "xdv"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Definitions of external processes for LaTeX previewing.
Org mode can use some external commands to generate TeX snippet's images for
previewing or inserting into HTML files, e.g., \"dvipng\".  This variable tells
`org-create-formula-image' how to call them.

The value is an alist with the pattern (NAME . PROPERTIES).  NAME is a symbol.
PROPERTIES accepts the following attributes:

  :programs           list of strings, required programs.
  :description        string, describe the process.
  :message            string, message it when required programs cannot be found.
  :image-input-type   string, input file type of image converter (e.g., \"dvi\").
  :image-output-type  string, output file type of image converter (e.g., \"png\").
  :image-size-adjust  cons of numbers, the car element is used to adjust LaTeX
                      image size showed in buffer and the cdr element is for
                      HTML file.  This option is only useful for process
                      developers, users should use variable
                      `org-format-latex-options' instead.
  :post-clean         list of strings, files matched are to be cleaned up once
                      the image is generated.  When nil, the files with \".dvi\",
                      \".xdv\", \".pdf\", \".tex\", \".aux\", \".log\", \".svg\",
                      \".png\", \".jpg\", \".jpeg\" or \".out\" extension will
                      be cleaned up.
  :latex-header       list of strings, the LaTeX header of the snippet file.
                      When nil, the fallback value is used instead, which is
                      controlled by `org-format-latex-header',
                      `org-latex-default-packages-alist' and
                      `org-latex-packages-alist', which see.
  :latex-compiler list of LaTeX commands, as strings or a function.
                      Each of them is given to the shell.
                      Place-holders \"%t\", \"%b\" and \"%o\" are
                      replaced with values defined below.
                      When a function, that function should accept the
                      file name as its single argument.
  :image-converter list of image converter commands strings or a
                      function.  Each of them is given to the shell
                      and supports any of the following place-holders
                      defined below.
                      When a function, that function should accept the
                      file name as its single argument.

If set, :transparent-image-converter is used instead of :image-converter to
convert an image when the background color is nil or \"Transparent\".

Place-holders used by `:image-converter' and `:latex-compiler':

  %f    input file name
  %b    base name of input file
  %o    base directory of input file
  %O    absolute output file name

Place-holders only used by `:image-converter':

  %D    dpi, which is used to adjust image size by some processing commands.
  %S    the image size scale ratio, which is used to adjust image size by some
        processing commands."
  :group 'org-latex
  :package-version '(Org . "9.8")
  :type '(alist :tag "LaTeX to image backends"
		:value-type (plist)))

(make-obsolete-variable
 'org-preview-latex-process-alist 'org-latex-preview-process-alist
 "10.0")

(define-obsolete-function-alias
  'org-clear-latex-preview 'org-latex-preview-clear-overlays "10.0")
(make-obsolete
 'org-place-formula-image "no longer used" "10.0")
;;;###autoload (autoload 'org-latex-preview--format-color "org-latex-preview")
(define-obsolete-function-alias
  'org-latex-color-format 'org-latex-preview--format-color "10.0")
;;;###autoload (autoload 'org-latex-preview--attr-color "org-latex-preview")
(define-obsolete-function-alias
  'org-latex-color 'org-latex-preview--attr-color "10.0")
;; MathML related functions from org-latex-preview.el
(define-obsolete-variable-alias
  'org-latex-to-mathml-jar-file 'org-mathml-converter-jar-file "10.0")
(define-obsolete-variable-alias
  'org-latex-to-mathml-convert-command 'org-mathml-convert-command "10.0")
(define-obsolete-function-alias
  'org-format-latex-mathml-available-p 'org-mathml-converter-available-p "10.0")
(define-obsolete-function-alias
  'org-create-math-formula 'org-mathml-convert-latex "10.0")
(define-obsolete-function-alias
  'org-latex-mathml-directory 'org-mathml-export-directory "10.0")

;; FIXME: Unused; obsoleted; to be removed.
(defun org-format-latex-as-mathml (latex-frag latex-frag-type
                                              prefix &optional dir)
  (let* ((absprefix (expand-file-name prefix dir))
         (print-length nil) (print-level nil)
         (formula-id (concat
                      "formula-"
                      (sha1
                       (prin1-to-string
                        (list latex-frag
                              org-latex-to-mathml-convert-command)))))
         (formula-cache (format "%s-%s.mathml" absprefix formula-id))
         (formula-cache-dir (file-name-directory formula-cache)))
    (unless (file-directory-p formula-cache-dir)
      (make-directory formula-cache-dir t))
    (unless (file-exists-p formula-cache)
      (org-mathml-convert-latex latex-frag formula-cache))
    (if (file-exists-p formula-cache)
        ;; Successful conversion.  Return the link to MathML file.
        (org-add-props
            (format  "[[file:%s]]" (file-relative-name formula-cache dir))
            (list 'org-latex-src (replace-regexp-in-string "\"" "" latex-frag)
                  'org-latex-src-embed-type (if latex-frag-type
                                                'paragraph 'character)))
      ;; Failed conversion.  Return the LaTeX fragment verbatim
      latex-frag)))
(make-obsolete #'org-format-latex-as-mathml "to be removed" "10.0")

;; FIXME: Unused; obsoleted; to be removed.
(defun org-dvipng-color (attr)
  "Return a RGB color specification for dvipng."
  (org-dvipng-color-format (face-attribute 'default attr nil)))

;; FIXME: Unused; obsoleted; to be removed.
(defun org-dvipng-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value for dvipng."
  (apply #'format "rgb %s %s %s"
         (mapcar 'org-normalize-color
                 (color-values color-name))))
(make-obsolete
 'org-dvipng-color "to be removed" "10.0")
(make-obsolete
 'org-dvipng-color-format "to be removed" "10.0")

;; FIXME: Unused; obsoleted; to be removed.
(defun org-normalize-color (value)
  "Return string to be used as color value for an RGB component."
  (format "%g" (/ value 65535.0)))
(make-obsolete 'org-normalize-color "to be removed" "9.7")

;; FIXME: Unused; obsoleted; to be removed.
(defcustom org-preview-latex-image-directory "ltximg/"
  "Path to store latex preview images.
A relative path here creates many directories relative to the
processed Org files paths.  An absolute path puts all preview
images at the same place."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

(make-obsolete-variable
 'org-preview-latex-image-directory
 "Use `org-latex-preview-cache' instead."
 "10.0")

;; FIXME: Unused; obsoleted; to be removed.
(defvar org-latex-default-packages-alist)
(defvar org-latex-packages-alist)
(defvar org-html-latex-image-options)
(declare-function org-format-latex-as-html "ox-html")
(declare-function org-mathml-converter-available-p "ox-mathml")

(defun org-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type)
  "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

Some of the options can be changed using the variable
`org-format-latex-options', which see."
  (when (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (unless (eq processing-type 'verbatim)
    (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
	   (cnt 0)
	   checkdir-flag)
      (goto-char (or beg (point-min)))
      ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
      (when (and overlays (memq processing-type '(dvipng imagemagick)))
	(overlay-recenter (or end (point-max))))
      (while (re-search-forward math-regexp end t)
	(unless (and overlays
		     (eq (get-char-property (point) 'org-overlay-type)
			 'org-latex-overlay))
	  (let* ((context (org-element-context))
		 (type (org-element-type context)))
	    (when (memq type '(latex-environment latex-fragment))
	      (let ((block-type (eq type 'latex-environment))
		    (value (org-element-property :value context))
		    (beg (org-element-property :begin context))
		    (end (save-excursion
			   (goto-char (org-element-property :end context))
			   (skip-chars-backward " \r\t\n")
			   (point))))
		(cond
		 ((eq processing-type 'mathjax)
		  ;; Prepare for MathJax processing.
		  (if (not (string-match "\\`\\$\\$?" value))
		      (goto-char end)
		    (delete-region beg end)
		    (if (string= (match-string 0 value) "$$")
			(insert "\\[" (substring value 2 -2) "\\]")
		      (insert "\\(" (substring value 1 -1) "\\)"))))
		 ((eq processing-type 'html)
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-html value)))
		 ((assq processing-type org-preview-latex-process-alist)
		  ;; Process to an image.
		  (cl-incf cnt)
		  (goto-char beg)
		  (let* ((processing-info
			  (cdr (assq processing-type org-preview-latex-process-alist)))
			 (face (face-at-point))
			 ;; Get the colors from the face at point.
			 (fg
			  (let ((color (plist-get org-format-latex-options
						  :foreground)))
                            (if forbuffer
                                (cond
                                 ((eq color 'auto)
                                  (face-attribute face :foreground nil 'default))
                                 ((eq color 'default)
                                  (face-attribute 'default :foreground nil))
                                 (t color))
                              color)))
			 (bg
			  (let ((color (plist-get org-format-latex-options
						  :background)))
                            (if forbuffer
                                (cond
                                 ((eq color 'auto)
                                  (face-attribute face :background nil 'default))
                                 ((eq color 'default)
                                  (face-attribute 'default :background nil))
                                 (t color))
                              color)))
			 (hash (sha1 (prin1-to-string
				      (list org-format-latex-header
					    org-latex-default-packages-alist
					    org-latex-packages-alist
					    org-format-latex-options
					    forbuffer value fg bg))))
			 (imagetype (or (plist-get processing-info :image-output-type) "png"))
			 (absprefix (expand-file-name prefix dir))
			 (linkfile (format "%s_%s.%s" prefix hash imagetype))
			 (movefile (format "%s_%s.%s" absprefix hash imagetype))
			 (sep (and block-type "\n\n"))
			 (link (concat sep "[[file:" linkfile "]]" sep))
			 (html-options
                          (and (not forbuffer)
                               (require 'ox-html)
                               `(,@(and-let* ((foreground
                                               (plist-get org-html-latex-image-options
                                                          :foreground)))
                                     (list :html-foreground foreground))
                                 ,@(and-let* ((background
                                               (plist-get org-html-latex-image-options
                                                          :background)))
                                     (list :html-background background))
                                 ,@(and-let* ((scale (plist-get org-html-latex-image-options
                                                                :scale)))
                                     (list :html-scale scale)))))
                         (options
			  (org-combine-plists
			   html-options
                           org-format-latex-options
			   `(:foreground ,fg :background ,bg))))
		    (when msg (message msg cnt))
		    (unless checkdir-flag ; Ensure the directory exists.
		      (setq checkdir-flag t)
		      (let ((todir (file-name-directory absprefix)))
			(unless (file-directory-p todir)
			  (make-directory todir t))))
		    (unless (file-exists-p movefile)
		      (org-create-formula-image
		       value movefile options forbuffer processing-type))
                    (with-no-warnings
                      (org-place-formula-image link block-type beg end value overlays movefile imagetype))))
		 ((eq processing-type 'mathml)
		  (require 'ox-mathml)
                  ;; Process to MathML.
		  (unless (org-mathml-converter-available-p)
		    (user-error "LaTeX to MathML converter not configured"))
		  (cl-incf cnt)
		  (when msg (message msg cnt))
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (with-no-warnings
                            (org-format-latex-as-mathml
			     value block-type prefix dir))))
		 (t
		  (error "Unknown conversion process %s for LaTeX fragments"
			 processing-type)))))))))))

;; FIXME: Unused; obsoleted; to be removed.
(defun org-place-formula-image (link block-type beg end value overlays movefile imagetype)
  "Place an overlay from BEG to END showing MOVEFILE.
The overlay will be above BEG if OVERLAYS is non-nil."
  (if overlays
      (progn
        (dolist (o (overlays-in beg end))
          (when (eq (overlay-get o 'org-overlay-type)
                    'org-latex-overlay)
            (delete-overlay o)))
        (let ((ov (make-overlay beg end))
              (imagetype (or (intern imagetype) 'png)))
          (overlay-put ov 'org-overlay-type 'org-latex-overlay)
          (overlay-put ov 'evaporate t)
          (overlay-put ov
                       'modification-hooks
                       (list (lambda (o _flag _beg _end &optional _l)
                               (delete-overlay o))))
          (overlay-put ov
                       'display
                       (list 'image :type imagetype :file movefile :ascent 'center)))
        (goto-char end))
    (delete-region beg end)
    (insert
     (org-add-props link
         (list 'org-latex-src
               (replace-regexp-in-string "\"" "" value)
               'org-latex-src-embed-type
               (if block-type 'paragraph 'character))))))



;;;###autoload (autoload 'org-latex-preview-compiler-command-map "org-latex-preview")

(defvar org-latex-preview-compiler-command-map)
(defvar org-latex-precompile)
(defvar org-latex-compiler)
(declare-function org-latex-preview--get-display-dpi "org-latex-preview")
(declare-function org-latex-preview--attr-color "org-latex-preview")
(declare-function org-latex-preview--format-color "org-latex-preview")
(declare-function org-mathml-convert-latex "ox-mathml")
(declare-function org-latex-make-preamble "ox-latex")
(declare-function org-export-get-environment "ox")
(declare-function org-export-get-backend "ox")
(declare-function org-export-with-buffer-copy "ox")

;; FIXME: Unused; obsoleted; to be removed.
(defun org-create-formula-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
  (require 'org-latex-preview)
  (require 'ox-latex)
  (let* ((processing-type (or processing-type
			      org-preview-latex-default-process))
	 (processing-info
	  (cdr (assq processing-type org-preview-latex-process-alist)))
	 (programs (plist-get processing-info :programs))
	 (error-message (or (plist-get processing-info :message) ""))
	 (image-input-type (plist-get processing-info :image-input-type))
	 (image-output-type (plist-get processing-info :image-output-type))
	 (post-clean (or (plist-get processing-info :post-clean)
			 '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
			   ".svg" ".png" ".jpg" ".jpeg" ".out")))
	 (latex-header
	  (let ((org-latex-precompile nil))
            (or (plist-get processing-info :latex-header)
	        (org-latex-make-preamble
	         (org-export-get-environment (org-export-get-backend 'latex))
	         org-format-latex-header
	         'snippet))))
	 (latex-compiler (plist-get processing-info :latex-compiler))
         (org-tex-compiler (cdr (assoc org-latex-compiler
                                       org-latex-preview-compiler-command-map)))
	 (tmpdir temporary-file-directory)
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (image-size-adjust (or (plist-get processing-info :image-size-adjust)
				'(1.0 . 1.0)))
	 (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
		   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
	 (dpi (* scale (if (and buffer (display-graphic-p))
                           (org-latex-preview--get-display-dpi)
                         140.0)))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent"))
	 (image-converter
          (mapcar (lambda (s) (replace-regexp-in-string "%B.*?\\." "%B." s))
                  (ensure-list
                   (or (and (string= bg "Transparent")
                            (plist-get processing-info :transparent-image-converter))
                       (plist-get processing-info :image-converter)))))
         (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
	 (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (memq fg '(default auto))
	(setq fg (org-latex-preview--attr-color :foreground))
      (setq fg (org-latex-preview--format-color fg)))
    (setq bg (cond
	      ((memq bg '(default auto)) (org-latex-preview--attr-color :background))
	      ((string= bg "Transparent") nil)
	      (t (org-latex-preview--format-color bg))))
    ;; Remove TeX \par at end of snippet to avoid trailing space.
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
	      "\\definecolor{fg}{rgb}{" fg "}%\n"
	      (if bg
		  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
			  "\n\\pagecolor{bg}%\n")
		"")
	      "\n{\\color{fg}\n"
	      string
	      "\n}\n"
	      "\n\\end{document}\n"))
    (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
			    processing-type))
           (spec `((?D . ,(shell-quote-argument (format "%s" dpi)))
	           (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))
                   (?B . ,(shell-quote-argument texfilebase))
                   (?l . ,org-tex-compiler)
                   (?L . ,(car (split-string org-tex-compiler)))))
	   (image-input-file
	    (org-compile-file
	     texfile latex-compiler image-input-type err-msg log-buf spec))
	   (image-output-file
	    (org-compile-file
	     image-input-file image-converter image-output-type err-msg log-buf spec)))
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
	(when (file-exists-p (concat texfilebase e))
	  (delete-file (concat texfilebase e))))
      image-output-file)))

;; FIXME: Unused; obsoleted; to be removed.
(defun org-html-format-latex (latex-frag processing-type info)
  "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It can
be `mathjax', `verbatim', `html', nil, t or symbols in
`org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
`imagemagick'.  See `org-html-with-latex' for more information.
INFO is a plist containing export properties."
  (let ((cache-relpath "") (cache-dir ""))
    (unless (or (eq processing-type 'mathjax)
                (eq processing-type 'html))
      (let ((bfn (or (buffer-file-name)
		     (make-temp-name
		      (expand-file-name "latex" temporary-file-directory))))
	    (latex-header
	     (let ((header (plist-get info :latex-header)))
	       (and header
		    (concat (mapconcat
			     (lambda (line) (concat "#+LATEX_HEADER: " line))
			     (org-split-string header "\n")
			     "\n")
			    "\n")))))
	(setq cache-relpath
	      (concat (file-name-as-directory org-preview-latex-image-directory)
		      (file-name-sans-extension
		       (file-name-nondirectory bfn)))
	      cache-dir (file-name-directory
                         (or (plist-get info :output-file) bfn)))
	;; Re-create LaTeX environment from original buffer in
	;; temporary buffer so that dvipng/imagemagick can properly
	;; turn the fragment into an image.
	(setq latex-frag (concat latex-header latex-frag))))
    (org-export-with-buffer-copy
     :to-buffer (get-buffer-create " *Org HTML Export LaTeX*")
     :drop-visibility t :drop-narrowing t :drop-contents t
     (erase-buffer)
     (insert latex-frag)
     (org-format-latex cache-relpath nil nil cache-dir nil
		       "Creating LaTeX Image..." nil processing-type)
     (buffer-string))))

(make-obsolete #'org-format-latex "to be removed" "10.0")
(make-obsolete #'org-create-formula-image "to be removed" "10.0")
(make-obsolete #'org-html-format-latex "to be removed" "10.0")

(define-obsolete-function-alias 'org--math-always-on
  'org--math-p "9.7")

(defmacro org-no-popups (&rest body)
  "Suppress popup windows and evaluate BODY."
  `(let (pop-up-frames pop-up-windows)
     ,@body))
(make-obsolete 'org-no-popups "no longer used" "9.7")

(defun org-switch-to-buffer-other-window (&rest args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer."
  (let (pop-up-frames pop-up-windows)
    (apply #'switch-to-buffer-other-window args)))
  (make-obsolete 'org-switch-to-buffer-other-window "no longer used" "9.7")

(make-obsolete 'org-refresh-category-properties "no longer used" "9.7")
(make-obsolete 'org-refresh-effort-properties "no longer used" "9.7")

(defun org-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports
it, just inherit the face.  If INHERITS is not given and SPECS
is, use SPECS to define the face."
  (declare (indent 1))
  (if (facep inherits)
      (list (list t :inherit inherits))
    specs))
(make-obsolete 'org-compatible-face "you can remove it." "9.0")

(defun org-add-link-type (type &optional follow export)
  "Add a new TYPE link.
FOLLOW and EXPORT are two functions.

FOLLOW should take the link path as the single argument and do whatever
is necessary to follow the link, for example find a file or display
a mail message.

EXPORT should format the link path for export to one of the export formats.
It should be a function accepting three arguments:

  path    the path of the link, the text after the prefix (like \"http:\")
  desc    the description of the link, if any
  format  the export format, a symbol like `html' or `latex' or `ascii'.

The function may use the FORMAT information to return different values
depending on the format.  The return value will be put literally into
the exported file.  If the return value is nil, this means Org should
do what it normally does with links which do not have EXPORT defined.

Org mode has a built-in default for exporting links.  If you are happy with
this default, there is no need to define an export function for the link
type.  For a simple example of an export function, see `org-bbdb.el'.

If TYPE already exists, update it with the arguments.
See `org-link-parameters' for documentation on the other parameters."
  (org-link-set-parameters type :follow follow :export export)
  (message "Created %s link." type))

(make-obsolete 'org-add-link-type "use `org-link-set-parameters' instead." "9.0")

;;;; Functions unused in Org core.
(defun org-table-recognize-table.el ()
  "If there is a table.el table nearby, recognize it and move into it."
  (when (org-at-table.el-p)
    (forward-line 0)
    (unless (or (looking-at org-table-dataline-regexp)
                (not (looking-at org-table1-hline-regexp)))
      (forward-line)
      (when (looking-at org-table-any-border-regexp)
        (forward-line -2)))
    (if (re-search-forward "|" (org-table-end t) t)
        (progn
          (require 'table)
          (if (table--at-cell-p (point)) t
            (message "recognizing table.el table...")
            (table-recognize-table)
            (message "recognizing table.el table...done")))
      (error "This should not happen"))))

;; Not used since commit 6d1e3082, Feb 2010.
(make-obsolete 'org-table-recognize-table.el
               "please notify Org mailing list if you use this function."
               "9.0")

(defmacro org-preserve-lc (&rest body)
  (declare (debug (body))
	   (obsolete "please notify Org mailing list if you use this function."
		     "9.2"))
  (org-with-gensyms (line col)
    `(let ((,line (org-current-line))
	   (,col (current-column)))
       (unwind-protect
	   (progn ,@body)
	 (org-goto-line ,line)
	 (org-move-to-column ,col)))))

(defun org-version-check (version &rest _)
  "Non-nil if VERSION is lower (older) than `emacs-version'."
  (declare (obsolete "use `version<' or `fboundp' instead."
		     "9.2"))
  (version< version emacs-version))

(defun org-remove-angle-brackets (s)
  (org-unbracket-string "<" ">" s))
(make-obsolete 'org-remove-angle-brackets 'org-unbracket-string "9.0")

(defcustom org-capture-bookmark t
  "When non-nil, add bookmark pointing at the last stored position when capturing."
  :group 'org-capture
  :version "24.3"
  :type 'boolean)
(make-obsolete-variable
 'org-capture-bookmark
 "use `org-bookmark-names-plist' instead."
 "9.7")

(defcustom org-publish-sitemap-file-entry-format "%t"
  "Format string for site-map file entry.
You could use brackets to delimit on what part the link will be.

%t is the title.
%a is the author.
%d is the date."
  :group 'org-export-publish
  :type 'string)
(make-obsolete-variable
 'org-publish-sitemap-file-entry-format
 "set `:sitemap-format-entry' in `org-publish-project-alist' instead."
 "9.1")

(defvar org-agenda-skip-regexp)
(defun org-agenda-skip-entry-when-regexp-matches ()
  "Check if the current entry contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of this entry, causing agenda commands
to skip the entry but continuing the search in the subtree.  This is a
function that can be put into `org-agenda-skip-function' for the duration
of a command."
  (declare (obsolete "use `org-agenda-skip-if' instead." "9.1"))
  (let ((end (save-excursion (org-end-of-subtree t)))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip end)))

(defun org-agenda-skip-subtree-when-regexp-matches ()
  "Check if the current subtree contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of this tree, causing agenda commands
to skip this subtree.  This is a function that can be put into
`org-agenda-skip-function' for the duration of a command."
  (declare (obsolete "use `org-agenda-skip-if' instead." "9.1"))
  (let ((end (save-excursion (org-end-of-subtree t)))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip end)))

(defun org-agenda-skip-entry-when-regexp-matches-in-subtree ()
  "Check if the current subtree contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of the current entry (NOT the tree),
causing agenda commands to skip the entry but continuing the search in
the subtree.  This is a function that can be put into
`org-agenda-skip-function' for the duration of a command.  An important
use of this function is for the stuck project list."
  (declare (obsolete "use `org-agenda-skip-if' instead." "9.1"))
  (let ((end (save-excursion (org-end-of-subtree t)))
	(entry-end (save-excursion (outline-next-heading) (1- (point))))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip entry-end)))

(define-obsolete-function-alias 'org-minutes-to-clocksum-string
  'org-duration-from-minutes "9.1")

(define-obsolete-function-alias 'org-hh:mm-string-to-minutes
  'org-duration-to-minutes "9.1")

(define-obsolete-function-alias 'org-duration-string-to-minutes
  'org-duration-to-minutes "9.1")

(make-obsolete-variable 'org-time-clocksum-format
                        "set `org-duration-format' instead." "9.1")

(make-obsolete-variable 'org-time-clocksum-use-fractional
                        "set `org-duration-format' instead." "9.1")

(make-obsolete-variable 'org-time-clocksum-fractional-format
                        "set `org-duration-format' instead." "9.1")

(make-obsolete-variable 'org-time-clocksum-use-effort-durations
                        "set `org-duration-units' instead." "9.1")

(define-obsolete-function-alias 'org-babel-number-p
  'org-babel--string-to-number "9.0")

(define-obsolete-variable-alias 'org-usenet-links-prefer-google
  'org-gnus-prefer-web-links "9.1")

(define-obsolete-variable-alias 'org-texinfo-def-table-markup
  'org-texinfo-table-default-markup "9.1")

(define-obsolete-variable-alias 'org-agenda-overriding-columns-format
  'org-overriding-columns-format "9.2.2")

(define-obsolete-variable-alias 'org-doi-server-url
  'org-link-doi-server-url "9.3")

(define-obsolete-variable-alias 'org-email-link-description-format
  'org-link-email-description-format "9.3")

(define-obsolete-variable-alias 'org-make-link-description-function
  'org-link-make-description-function "9.3")

(define-obsolete-variable-alias 'org-from-is-user-regexp
  'org-link-from-user-regexp "9.3")

(define-obsolete-variable-alias 'org-descriptive-links
  'org-link-descriptive "9.3")

(define-obsolete-variable-alias 'org-context-in-file-links
  'org-link-context-for-files "9.3")

(define-obsolete-variable-alias 'org-keep-stored-link-after-insertion
  'org-link-keep-stored-after-insertion "9.3")

(define-obsolete-variable-alias 'org-display-internal-link-with-indirect-buffer
  'org-link-use-indirect-buffer-for-internals "9.3")

(define-obsolete-variable-alias 'org-confirm-shell-link-function
  'org-link-shell-confirm-function "9.3")

(define-obsolete-variable-alias 'org-confirm-shell-link-not-regexp
  'org-link-shell-skip-confirm-regexp "9.3")

(define-obsolete-variable-alias 'org-confirm-elisp-link-function
  'org-link-elisp-confirm-function "9.3")

(define-obsolete-variable-alias 'org-confirm-elisp-link-not-regexp
  'org-link-elisp-skip-confirm-regexp "9.3")

(define-obsolete-function-alias 'org-file-complete-link
  'org-link-complete-file "9.3")

(define-obsolete-function-alias 'org-email-link-description
  'org-link-email-description "9.3")

(define-obsolete-function-alias 'org-make-link-string
  'org-link-make-string "9.3")

(define-obsolete-function-alias 'org-store-link-props
  'org-link-store-props "9.3")

(define-obsolete-function-alias 'org-add-link-props
  'org-link-add-props "9.3")

(define-obsolete-function-alias 'org-make-org-heading-search-string
  'org-link-heading-search-string "9.3")

(define-obsolete-function-alias 'org-make-link-regexps
  'org-link-make-regexps "9.3")

(define-obsolete-function-alias 'org-property-global-value
  'org-property-global-or-keyword-value "9.3")

(make-obsolete-variable 'org-file-properties 'org-keyword-properties "9.3")

(define-obsolete-variable-alias 'org-angle-link-re
  'org-link-angle-re "9.3")

(define-obsolete-variable-alias 'org-plain-link-re
  'org-link-plain-re "9.3")

(define-obsolete-variable-alias 'org-bracket-link-regexp
  'org-link-bracket-re "9.3")

(define-obsolete-variable-alias 'org-bracket-link-analytic-regexp
  'org-link-bracket-re "9.3")

(define-obsolete-variable-alias 'org-any-link-re
  'org-link-any-re "9.3")

(define-obsolete-function-alias 'org-open-link-from-string
  'org-link-open-from-string "9.3")

(define-obsolete-function-alias 'org-add-angle-brackets
  'org-link-add-angle-brackets "9.3")

(declare-function org-link-preview--remove-overlay "ol"
                  (ov after beg end &optional len))
(declare-function org-link-preview--get-overlays "ol" (&optional beg end))
(declare-function org-link-preview-clear "ol" (&optional beg end))
(declare-function org-link-preview--remove-overlay "ol"
                  (ov after beg end &optional len))
(declare-function org-attach-expand "org-attach" (file))
(declare-function org-display-inline-image--width "ol" (link))
(declare-function org-image--align "ol" (link))
(declare-function org--create-inline-image "ol" (file width))

(define-obsolete-function-alias 'org-display-inline-remove-overlay
  'org-link-preview--remove-overlay "9.8")
(define-obsolete-function-alias 'org--inline-image-overlays
  'org-link-preview--get-overlays "9.8")
(define-obsolete-function-alias 'org-remove-inline-images
  'org-link-preview-clear "9.8")
(define-obsolete-function-alias 'org-redisplay-inline-images
  'org-link-preview-refresh "9.8")
(define-obsolete-variable-alias 'org-inline-image-overlays
  'org-link-preview-overlays "9.8")
(defvar org-link-preview-overlays)
(defvar org-link-abbrev-alist-local)
(defvar org-link-abbrev-alist)
(defvar org-link-angle-re)
(defvar org-link-plain-re)

(make-obsolete 'org-display-inline-images
               'org-link-preview-region "9.8")
;; FIXME: Unused; obsoleted; to be removed
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" or
     \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-link-preview-clear beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
	(let* ((case-fold-search t)
	       (file-extension-re (image-file-name-regexp))
	       (link-abbrevs (mapcar #'car
				     (append org-link-abbrev-alist-local
					     org-link-abbrev-alist)))
	       ;; Check absolute, relative file names and explicit
	       ;; "file:" links.  Also check link abbreviations since
	       ;; some might expand to "file" links.
	       (file-types-re
		(format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
			(if (not link-abbrevs) ""
			  (concat "\\|" (regexp-opt link-abbrevs))))))
	  (while (re-search-forward file-types-re end t)
	    (let* ((link (org-element-lineage
			  (save-match-data (org-element-context))
			  'link t))
                   (linktype (org-element-property :type link))
		   (inner-start (match-beginning 1))
		   (path
		    (cond
		     ;; No link at point; no inline image.
		     ((not link) nil)
		     ;; File link without a description.  Also handle
		     ;; INCLUDE-LINKED here since it should have
		     ;; precedence over the next case.  I.e., if link
		     ;; contains filenames in both the path and the
		     ;; description, prioritize the path only when
		     ;; INCLUDE-LINKED is non-nil.
		     ((or (not (org-element-contents-begin link))
			  include-linked)
		      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
			   (org-element-property :path link)))
		     ;; Link with a description.  Check if description
		     ;; is a filename.  Even if Org doesn't have syntax
		     ;; for those -- clickable image -- constructs, fake
		     ;; them, as in `org-export-insert-image-links'.
		     ((not inner-start) nil)
		     (t
		      (org-with-point-at inner-start
			(and (looking-at
			      (if (char-equal ?< (char-after inner-start))
				  org-link-angle-re
				org-link-plain-re))
			     ;; File name must fill the whole
			     ;; description.
			     (= (org-element-contents-end link)
				(match-end 0))
			     (progn
                               (setq linktype (match-string 1))
                               (match-string 2))))))))
	      (when (and path (string-match-p file-extension-re path))
		(let ((file (if (equal "attachment" linktype)
				(progn
                                  (require 'org-attach)
				  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
                  ;; Expand environment variables.
                  (when file (setq file (substitute-in-file-name file)))
		  (when (and file (file-exists-p file))
		    (let ((width (org-display-inline-image--width link))
			  (align (org-image--align link))
                          (old (get-char-property-and-overlay
				(org-element-begin link)
				'org-image-overlay)))
		      (if (and (car-safe old) refresh)
                          (image-flush (overlay-get (cdr old) 'display))
			(let ((image (org--create-inline-image file width)))
			  (when image
			    (let ((ov (make-overlay
				       (org-element-begin link)
				       (progn
					 (goto-char
					  (org-element-end link))
					 (unless (eolp) (skip-chars-backward " \t"))
					 (point)))))
                              ;; See bug#59902.  We cannot rely
                              ;; on Emacs to update image if the file
                              ;; has changed.
                              (image-flush image)
			      (overlay-put ov 'display image)
			      (overlay-put ov 'face 'default)
			      (overlay-put ov 'org-image-overlay t)
			      (overlay-put
			       ov 'modification-hooks
			       (list 'org-link-preview--remove-overlay))
			      (when (boundp 'image-map)
				(overlay-put ov 'keymap image-map))
                              (when align
                                (overlay-put
                                 ov 'before-string
                                 (propertize
                                  " " 'face 'default
                                  'display
                                  (pcase align
                                    ("center" `(space :align-to (- center (0.5 . ,image))))
                                    ("right"  `(space :align-to (- right ,image)))))))
			      (push ov org-inline-image-overlays))))))))))))))))

(make-obsolete 'org-toggle-inline-images
               'org-link-preview "9.8")
(declare-function org-link-preview-region "ol")
;; FIXME: Unused; obsoleted; to be removed
(defun org-toggle-inline-images (&optional include-linked beg end)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive "P")
  (if (org-link-preview--get-overlays beg end)
      (progn
        (org-link-preview-clear beg end)
        (when (called-interactively-p 'interactive)
	  (message "Inline image display turned off")))
    (org-link-preview-region include-linked nil beg end)
    (when (called-interactively-p 'interactive)
      (let ((new (org-link-preview--get-overlays beg end)))
        (message (if new
		     (format "%d images displayed inline"
			     (length new))
		   "No images to display inline"))))))


;; The function was made obsolete by commit 65399674d5 of 2013-02-22.
;; This make-obsolete call was added 2016-09-01.
(make-obsolete 'org-capture-import-remember-templates
	       "use the `org-capture-templates' variable instead."
	       "9.0")

(defun org-show-block-all ()
  "Unfold all blocks in the current buffer."
  (interactive)
  (org-fold-show-all '(blocks)))

(make-obsolete 'org-show-block-all
	       "use `org-fold-show-all' instead."
	       "9.2")

(define-obsolete-function-alias 'org-get-tags-at 'org-get-tags "9.2")

(defun org-get-local-tags ()
  "Get a list of tags defined in the current headline."
  (declare (obsolete "use `org-get-tags' instead." "9.2"))
  (org-get-tags nil 'local))

(defun org-get-local-tags-at (&optional pos)
  "Get a list of tags defined in the current headline."
  (declare (obsolete "use `org-get-tags' instead." "9.2"))
  (org-get-tags pos 'local))

(defun org-get-tags-string ()
  "Get the TAGS string in the current headline."
  (declare (obsolete "use `org-make-tag-string' instead." "9.2"))
  (org-make-tag-string (org-get-tags nil t)))

(define-obsolete-function-alias 'org-set-tags-to 'org-set-tags "9.2")

(defun org-align-all-tags ()
  "Align the tags in all headings."
  (declare (obsolete "use `org-align-tags' instead." "9.2"))
  (org-align-tags t))

(define-obsolete-function-alias
  'org-at-property-block-p 'org-at-property-drawer-p "9.4")

(defun org-flag-drawer (flag &optional element beg end)
  "When FLAG is non-nil, hide the drawer we are at.
Otherwise make it visible.

When optional argument ELEMENT is a parsed drawer, as returned by
`org-element-at-point', hide or show that drawer instead.

When buffer positions BEG and END are provided, hide or show that
region as a drawer without further ado."
  (declare (obsolete "use `org-fold-hide-drawer-toggle' instead." "9.4"))
  (if (and beg end) (org-fold-region beg end flag 'drawer)
    (let ((drawer
	   (or element
	       (and (save-excursion
		      (forward-line 0)
		      (looking-at-p "^[ \t]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ \t]*$"))
		    (org-element-at-point)))))
      (when (org-element-type-p drawer '(drawer property-drawer))
	(let ((post (org-element-post-affiliated drawer)))
	  (org-fold-region
	   (save-excursion (goto-char post) (line-end-position))
	   (save-excursion (goto-char (org-element-end drawer))
			   (skip-chars-backward " \t\n")
			   (line-end-position))
	   flag 'drawer)
	  ;; When the drawer is hidden away, make sure point lies in
	  ;; a visible part of the buffer.
	  (when (invisible-p (max (1- (point)) (point-min)))
	    (goto-char post)))))))

(defun org-hide-block-toggle-maybe ()
  "Toggle visibility of block at point.
Unlike to `org-fold-hide-block-toggle', this function does not throw
an error.  Return a non-nil value when toggling is successful."
  (declare (obsolete "use `org-fold-hide-block-toggle' instead." "9.4"))
  (interactive)
  (org-fold-hide-block-toggle nil t))

(defun org-hide-block-toggle-all ()
  "Toggle the visibility of all blocks in the current buffer."
  (declare (obsolete "please notify Org mailing list if you use this function."
		     "9.4"))
  (let ((start (point-min))
        (end (point-max)))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
		  (re-search-forward "^[ \t]*#\\+begin_?\
\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?\n\\([^z-a]+?\\)#\\+end_?\\1[ \t]*$" end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (org-fold-hide-block-toggle)))))))

(defun org-return-indent ()
  "Goto next table row or insert a newline and indent.
Calls `org-table-next-row' or `newline-and-indent', depending on
context.  See the individual commands for more information."
  (declare (obsolete "use `org-return' with INDENT set to t instead."
		     "9.4"))
  (interactive)
  (org-return t))

(defmacro org-with-silent-modifications (&rest body)
  (declare (obsolete "use `with-silent-modifications' instead." "9.2")
	   (debug (body)))
  `(with-silent-modifications ,@body))

(define-obsolete-function-alias 'org-babel-strip-quotes
  'org-strip-quotes "9.2")

(define-obsolete-variable-alias 'org-sort-agenda-notime-is-late
  'org-agenda-sort-notime-is-late "9.4")

(define-obsolete-variable-alias 'org-sort-agenda-noeffort-is-high
  'org-agenda-sort-noeffort-is-high "9.4")

(defconst org-maybe-keyword-time-regexp
  (concat "\\(\\<\\(\\(?:CLO\\(?:CK\\|SED\\)\\|DEADLINE\\|SCHEDULED\\):\\)\\)?"
	  " *\\([[<][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]\r\n>]*[]>]"
	  "\\|"
	  "<%%([^\r\n>]*>\\)")
  "Matches a timestamp, possibly preceded by a keyword.")
(make-obsolete-variable
 'org-maybe-keyword-time-regexp
 "use `org-planning-line-re', followed by `org-ts-regexp-both' instead."
 "9.4")

(define-obsolete-function-alias 'org-copy 'org-refile-copy "9.4")

(define-obsolete-function-alias 'org-get-last-sibling 'org-get-previous-sibling "9.4")

(define-obsolete-function-alias 'org-publish-cache-ctime-of-src
  'org-publish-cache-mtime-of-src "9.6")

(define-obsolete-function-alias 'org-truely-invisible-p
  'org-truly-invisible-p "9.6"
  "Compatibility alias for legacy misspelling of `org-truly-invisible-p'.")


(defconst org-latex-babel-language-alist
  '(("af" . "afrikaans")
    ("bg" . "bulgarian")
    ("ca" . "catalan")
    ("cs" . "czech")
    ("cy" . "welsh")
    ("da" . "danish")
    ("de" . "germanb")
    ("de-at" . "naustrian")
    ("de-de" . "ngerman")
    ("el" . "greek")
    ("en" . "english")
    ("en-au" . "australian")
    ("en-ca" . "canadian")
    ("en-gb" . "british")
    ("en-ie" . "irish")
    ("en-nz" . "newzealand")
    ("en-us" . "american")
    ("es" . "spanish")
    ("et" . "estonian")
    ("eu" . "basque")
    ("fi" . "finnish")
    ("fr" . "french")
    ("fr-ca" . "canadien")
    ("gl" . "galician")
    ("hr" . "croatian")
    ("hu" . "hungarian")
    ("id" . "indonesian")
    ("is" . "icelandic")
    ("it" . "italian")
    ("la" . "latin")
    ("ms" . "malay")
    ("nl" . "dutch")
    ("nb" . "norsk")
    ("nn" . "nynorsk")
    ("no" . "norsk")
    ("pl" . "polish")
    ("pt" . "portuguese")
    ("pt-br" . "brazilian")
    ("ro" . "romanian")
    ("ru" . "russian")
    ("sa" . "sanskrit")
    ("sb" . "uppersorbian")
    ("sk" . "slovak")
    ("sl" . "slovene")
    ("sq" . "albanian")
    ("sr" . "serbian")
    ("sv" . "swedish")
    ("ta" . "tamil")
    ("tr" . "turkish")
    ("uk" . "ukrainian"))
  "Alist between language code and corresponding Babel option.")

(defconst org-latex-polyglossia-language-alist
  '(("am" "amharic")
    ("ar" "arabic")
    ("ast" "asturian")
    ("bg" "bulgarian")
    ("bn" "bengali")
    ("bo" "tibetan")
    ("br" "breton")
    ("ca" "catalan")
    ("cop" "coptic")
    ("cs" "czech")
    ("cy" "welsh")
    ("da" "danish")
    ("de" "german" "german")
    ("de-at" "german" "austrian")
    ("de-de" "german" "german")
    ("dsb" "lsorbian")
    ("dv" "divehi")
    ("el" "greek")
    ("en" "english" "usmax")
    ("en-au" "english" "australian")
    ("en-gb" "english" "uk")
    ("en-nz" "english" "newzealand")
    ("en-us" "english" "usmax")
    ("eo" "esperanto")
    ("es" "spanish")
    ("et" "estonian")
    ("eu" "basque")
    ("fa" "farsi")
    ("fi" "finnish")
    ("fr" "french")
    ("fu" "friulan")
    ("ga" "irish")
    ("gd" "scottish")
    ("gl" "galician")
    ("he" "hebrew")
    ("hi" "hindi")
    ("hr" "croatian")
    ("hsb" "usorbian")
    ("hu" "magyar")
    ("hy" "armenian")
    ("ia" "interlingua")
    ("id" "bahasai")
    ("is" "icelandic")
    ("it" "italian")
    ("kn" "kannada")
    ("la" "latin" "modern")
    ("la-classic" "latin" "classic")
    ("la-medieval" "latin" "medieval")
    ("la-modern" "latin" "modern")
    ("lo" "lao")
    ("lt" "lithuanian")
    ("lv" "latvian")
    ("ml" "malayalam")
    ("mr" "maranthi")
    ("nb" "norsk")
    ("nko" "nko")
    ("nl" "dutch")
    ("nn" "nynorsk")
    ("no" "norsk")
    ("oc" "occitan")
    ("pl" "polish")
    ("pms" "piedmontese")
    ("pt" "portuges")
    ("pt-br" "brazilian")
    ("rm" "romansh")
    ("ro" "romanian")
    ("ru" "russian")
    ("sa" "sanskrit")
    ("se" "samin")
    ("sk" "slovak")
    ("sl" "slovenian")
    ("sq" "albanian")
    ("sr" "serbian")
    ("sv" "swedish")
    ("syr" "syriac")
    ("ta" "tamil")
    ("te" "telugu")
    ("th" "thai")
    ("tk" "turkmen")
    ("tr" "turkish")
    ("uk" "ukrainian")
    ("ur" "urdu")
    ("vi" "vietnamese"))
  "Alist between language code and corresponding Polyglossia option.")

(make-obsolete-variable 'org-latex-babel-language-alist
                        "set `org-latex-language-alist' instead." "9.6")

(make-obsolete-variable 'org-latex-polyglossia-language-alist
                        "set `org-latex-language-alist' instead." "9.6")

(defconst org-babel-python-mode 'python
  "Python mode for use in running python interactively.")

(make-obsolete-variable
 'org-babel-python-mode
 "Only the built-in Python mode is supported in ob-python now."
 "9.7")

(define-obsolete-function-alias 'ob-clojure-eval-with-babashka
  #'ob-clojure-eval-with-cmd "9.7")

(define-obsolete-function-alias 'org-export-get-parent
  'org-element-parent "9.7")
(define-obsolete-function-alias 'org-export-get-parent-element
  'org-element-parent-element "9.7")

(define-obsolete-function-alias 'org-print-speed-command
  'org--print-speed-command "9.7"
  "Internal function.  Subject of unannounced changes.")

;;;; Obsolete link types

(eval-after-load 'ol
  '(progn
     (org-link-set-parameters "file+emacs") ;since Org 9.0
     (org-link-set-parameters "file+sys"))) ;since Org 9.0





;;; Miscellaneous functions

(defun org-get-x-clipboard (value)
  "Get the value of the X or Windows clipboard."
  (cond ((and (eq window-system 'x)
              (fboundp 'gui-get-selection)) ;Silence byte-compiler.
         (org-no-properties
          (ignore-errors
            (or (gui-get-selection value 'UTF8_STRING)
                (gui-get-selection value 'COMPOUND_TEXT)
                (gui-get-selection value 'STRING)
                (gui-get-selection value 'TEXT)))))
        ((and (eq window-system 'w32) (fboundp 'w32-get-clipboard-data))
         (w32-get-clipboard-data))))


;;; Region compatibility

(defvar org-ignore-region nil
  "Non-nil means temporarily disable the active region.")

(defun org-region-active-p ()
  "Non-nil when the region active.
Unlike to `use-region-p', this function also checks
`org-ignore-region'."
  (and (not org-ignore-region) (use-region-p)))

(defun org-cursor-to-region-beginning ()
  (when (and (org-region-active-p)
             (> (point) (region-beginning)))
    (exchange-point-and-mark)))


;;; Invisibility compatibility

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (when (consp buffer-invisibility-spec)
    (member arg buffer-invisibility-spec)))

(defun org-move-to-column (column &optional force _buffer)
  "Move to column COLUMN.
Pass COLUMN and FORCE to `move-to-column'."
  (let ((buffer-invisibility-spec
         (if (listp buffer-invisibility-spec)
             (remove '(org-filtered) buffer-invisibility-spec)
           buffer-invisibility-spec)))
    (move-to-column column force)))

(defmacro org-find-library-dir (library)
  `(file-name-directory (or (locate-library ,library) "")))

(defun org-count-lines (s)
  "How many lines in string S?"
  (let ((start 0) (n 1))
    (while (string-match "\n" s start)
      (setq start (match-end 0) n (1+ n)))
    (when (and (> (length s) 0) (= (aref s (1- (length s))) ?\n))
      (setq n (1- n)))
    n))

(defun org-kill-new (string &rest args)
  (remove-text-properties 0 (length string) '(line-prefix t wrap-prefix t)
                          string)
  (apply 'kill-new string args))

;;;###autoload
(defmacro org-check-version ()
  "Try very hard to provide sensible version strings."
  (let* ((org-dir        (org-find-library-dir "org"))
         (org-version.el (concat org-dir "org-version.el"))
         (org-fixup.el   (concat org-dir "../mk/org-fixup.el")))
    (if (require 'org-version org-version.el 'noerror)
        '(progn
           (autoload 'org-release     "org-version.el")
           (autoload 'org-git-version "org-version.el"))
      (if (require 'org-fixup org-fixup.el 'noerror)
          '(org-fixup)
        ;; provide fallback definitions and complain
        (warn "Could not define org version correctly.  Check installation!")
        '(progn
           (defun org-release () "N/A")
           (defun org-git-version () "N/A !!check installation!!"))))))

(define-obsolete-function-alias 'org-define-error #'define-error "9.6")
(define-obsolete-function-alias 'org-without-partial-completion 'progn "9.6")


;;; Integration with and fixes for other packages

(defgroup org-imenu-and-speedbar nil
  "Options concerning imenu and speedbar in Org mode."
  :tag "Org Imenu and Speedbar"
  :group 'org-structure)

(defcustom org-imenu-depth 2
  "The maximum level for Imenu access to Org headlines.
This also applied for speedbar access."
  :type 'integer)

;;;; Imenu

(defvar-local org-imenu-markers nil
  "All markers currently used by Imenu.")

(defun org-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re (concat "^" (org-get-limited-outline-regexp)))
	  (subs (make-vector (1+ org-imenu-depth) nil))
	  (last-level 0))
     (while (re-search-backward re nil t)
       (let ((level (org-reduced-level (funcall outline-level)))
	     (headline (org-no-properties
			(org-link-display-format (org-get-heading t t t t)))))
	 (when (and (<= level org-imenu-depth) (org-string-nw-p headline))
	   (let* ((m (point-marker))
		  (item (propertize headline 'org-imenu-marker m 'org-imenu t)))
	     (push m org-imenu-markers)
             (push (cons item m) (aref subs level))
             (unless (>= level last-level)
	       (push (cons item
			   (cl-mapcan #'identity (cl-subseq subs (1+ level))))
		     (aref subs level))
	       (cl-loop for i from (1+ level) to org-imenu-depth
			do (aset subs i nil)))
	     (setq last-level level)))))
     (aref subs 1))))

(eval-after-load 'imenu
  '(progn
     (add-hook 'imenu-after-jump-hook
	       (lambda ()
		 (when (derived-mode-p 'org-mode)
		   (org-fold-show-context 'org-goto))))
     (add-hook 'org-mode-hook
	       (lambda ()
		 (setq imenu-create-index-function 'org-imenu-get-tree)))))

;;;; Speedbar

(defvar org-speedbar-restriction-lock-overlay (make-overlay 1 1)
  "Overlay marking the agenda restriction line in speedbar.")
(overlay-put org-speedbar-restriction-lock-overlay
	     'face 'org-agenda-restriction-lock)
(overlay-put org-speedbar-restriction-lock-overlay
	     'help-echo "Agendas are currently limited to this item.")
(delete-overlay org-speedbar-restriction-lock-overlay)

(defun org-speedbar-set-agenda-restriction ()
  "Restrict future agenda commands to the location at point in speedbar.
If there is already a restriction lock at the location, remove it.

To get rid of the restriction, use `\\[org-agenda-remove-restriction-lock]'."
  (interactive)
  (require 'org-agenda)
  (let (p m tp np dir txt)
    (cond
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'org-imenu t))
      (setq m (get-text-property p 'org-imenu-marker))
      (with-current-buffer (marker-buffer m)
	(goto-char m)
	(if (and org-agenda-overriding-restriction
		 (member org-agenda-restriction-lock-overlay
			 (overlays-at (point))))
	    (org-agenda-remove-restriction-lock 'noupdate)
	  (org-agenda-set-restriction-lock 'subtree))))
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'speedbar-function 'speedbar-find-file))
      (setq tp (previous-single-property-change
		(1+ p) 'speedbar-function)
	    np (next-single-property-change
		tp 'speedbar-function)
	    dir (speedbar-line-directory)
	    txt (buffer-substring-no-properties (or tp (point-min))
						(or np (point-max))))
      (with-current-buffer (find-file-noselect
			    (let ((default-directory dir))
			      (expand-file-name txt)))
	(unless (derived-mode-p 'org-mode)
	  (user-error "Cannot restrict to non-Org mode file"))
	(org-agenda-set-restriction-lock 'file)))
     (t (user-error "Don't know how to restrict Org mode agenda")))
    (move-overlay org-speedbar-restriction-lock-overlay
                  (line-beginning-position) (line-end-position))
    (setq current-prefix-arg nil)
    (org-agenda-maybe-redo)))

(defvar speedbar-file-key-map)
(declare-function speedbar-add-supported-extension "speedbar" (extension))
(eval-after-load 'speedbar
  '(progn
     (speedbar-add-supported-extension ".org")
     (define-key speedbar-file-key-map "<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map "\C-c\C-x<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map ">" 'org-agenda-remove-restriction-lock)
     (define-key speedbar-file-key-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
     (add-hook 'speedbar-visiting-tag-hook
	       (lambda () (and (derived-mode-p 'org-mode) (org-fold-show-context 'org-goto))))))

;;;; Add Log

(defun org-add-log-current-headline ()
  "Return current headline or nil.
This function ignores inlinetasks.  It is meant to be used as
`add-log-current-defun-function' value."
  (org-with-limited-levels (org-get-heading t t t t)))

;;;; Flyspell

(defun org--flyspell-object-check-p (element)
  "Non-nil when Flyspell can check object at point.
ELEMENT is the element at point."
  (let ((object (save-excursion
		  (when (looking-at-p "\\>") (backward-char))
		  (org-element-context element))))
    (cl-case (org-element-type object)
      ((citation citation-reference code entity export-snippet inline-babel-call
	         inline-src-block line-break latex-fragment macro
	         statistics-cookie target timestamp verbatim)
       nil)
      (link
       ;; Only check link description
       (when-let* ((cbeg (org-element-contents-begin object))
                   (cend (org-element-contents-end object)))
         (<= cbeg (point) cend)))
      (footnote-reference
       ;; Only in inline footnotes, within the definition.
       (and (eq (org-element-property :type object) 'inline)
            (<= (org-element-contents-begin object)
               (point)
               (org-element-contents-end object))))
      (otherwise t))))

(defun org-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate'."
  (if (org-at-heading-p)
      ;; At a headline or an inlinetask, check title only.
      (and (save-excursion (forward-line 0)
			   (and (let ((case-fold-search t))
				  (not (looking-at-p "\\*+ END[ \t]*$")))
				(let ((case-fold-search nil))
				  (looking-at org-complex-heading-regexp))))
	   (match-beginning 4)
	   (>= (point) (match-beginning 4))
	   (or (not (match-beginning 5))
	       (< (point) (match-beginning 5)))
           ;; Ignore checks in code, verbatim and others.
           (org--flyspell-object-check-p (org-element-at-point-no-context)))
    (let* ((element (org-element-at-point-no-context))
	   (post-affiliated (org-element-post-affiliated element)))
      (cond
       ;; Ignore checks in all affiliated keywords but captions.
       ((< (point) post-affiliated)
	(and (save-excursion
	       (forward-line 0)
	       (let ((case-fold-search t)) (looking-at "[ \t]*#\\+CAPTION:")))
	     (> (point) (match-end 0))
	     (org--flyspell-object-check-p element)))
       (t
	(cl-case (org-element-type element)
	  ((comment quote-section) t)
	  (comment-block
	   ;; Allow checks between block markers, not on them.
	   (and (> (line-beginning-position) post-affiliated)
		(save-excursion
		  (end-of-line)
		  (skip-chars-forward " \r\t\n")
		  (< (point) (org-element-end element)))))
	  ;; Arbitrary list of keywords where checks are meaningful.
	  ;; Make sure point is on the value part of the element.
	  (keyword
	   (and (member (org-element-property :key element)
			'("DESCRIPTION" "TITLE"))
		(save-excursion
		  (search-backward ":" (line-beginning-position) t))))
	  ;; Check is globally allowed in paragraphs verse blocks and
	  ;; table rows (after affiliated keywords) but some objects
	  ;; must not be affected.
	  ((paragraph table-row verse-block)
	   (let ((cbeg (org-element-contents-begin element))
		 (cend (org-element-contents-end element)))
	     (and cbeg (>= (point) cbeg) (< (point) cend)
		  (org--flyspell-object-check-p element))))))))))
(put 'org-mode 'flyspell-mode-predicate 'org-mode-flyspell-verify)

(defun org-remove-flyspell-overlays-in (beg end)
  "Remove flyspell overlays in region."
  (and (bound-and-true-p flyspell-mode)
       (fboundp 'flyspell-delete-region-overlays)
       (flyspell-delete-region-overlays beg end)))

(defvar flyspell-delayed-commands)
(eval-after-load 'flyspell
  '(add-to-list 'flyspell-delayed-commands 'org-self-insert-command))

;;;; Bookmark

(defun org-bookmark-jump-unhide (&rest _)
  "Unhide the current position, to show the bookmark location."
  (and (derived-mode-p 'org-mode)
       (or (org-invisible-p)
	   (save-excursion (goto-char (max (point-min) (1- (point))))
			   (org-invisible-p)))
       (org-fold-show-context 'bookmark-jump)))

;; Make `bookmark-jump' shows the jump location if it was hidden.
(add-hook 'bookmark-after-jump-hook #'org-bookmark-jump-unhide)

;;;; Calendar

(defcustom org-calendar-to-agenda-key 'default
  "Key to be installed in `calendar-mode-map' for switching to the agenda.

The command `org-calendar-goto-agenda' will be bound to this key.

When set to `default', bind the function to `c', but only if it is
available in the Calendar keymap.  This is the default choice because
`c' can then be used to switch back and forth between agenda and calendar.

When nil, `org-calendar-goto-agenda' is not bound to any key."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Bind to `c' if available" default)
	  (key-sequence :tag "Other binding")
	  (const :tag "No binding" nil))
  :safe (lambda (v) (or (symbolp v) (stringp v)))
  :package-version '(Org . "9.2"))

(defcustom org-calendar-insert-diary-entry-key [?i]
  "The key to be installed in `calendar-mode-map' for adding diary entries.
This option is irrelevant until `org-agenda-diary-file' has been configured
to point to an Org file.  When that is the case, the command
`org-agenda-diary-entry' will be bound to the key given here, by default
`i'.  In the calendar, `i' normally adds entries to `diary-file'.  So
if you want to continue doing this, you need to change this to a different
key."
  :group 'org-agenda
  :type 'sexp)

(defun org--setup-calendar-bindings ()
  "Bind Org functions in Calendar keymap."
  (pcase org-calendar-to-agenda-key
    (`nil nil)
    ((and key (pred stringp))
     (local-set-key (kbd key) #'org-calendar-goto-agenda))
    ((guard (not (lookup-key calendar-mode-map "c")))
     (local-set-key "c" #'org-calendar-goto-agenda))
    (_ nil))
  (when (and (boundp 'org-agenda-diary-file)
	     (not (eq org-agenda-diary-file 'diary-file)))
    (local-set-key org-calendar-insert-diary-entry-key
		   #'org-agenda-diary-entry)))

(eval-after-load 'calendar
  '(add-hook 'calendar-mode-hook #'org--setup-calendar-bindings))

;;;; Saveplace

;; Make sure saveplace shows the location if it was hidden
(advice-add 'save-place-find-file-hook :after #'org-bookmark-jump-unhide)

;;;; Ecb

;; Make sure ecb shows the location if it was hidden
(advice-add 'ecb-method-clicked :after #'org--ecb-show-context)
(defun org--ecb-show-context (&rest _)
  "Make hierarchy visible when jumping into location from ECB tree buffer."
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context)))

;;;; Simple

(defun org-mark-jump-unhide (&rest _)
  "Make the point visible with `org-fold-show-context' after jumping to the mark."
  (when (and (derived-mode-p 'org-mode)
	     (org-invisible-p))
    (org-fold-show-context 'mark-goto)))

(advice-add 'pop-to-mark-command :after #'org-mark-jump-unhide)

(advice-add 'exchange-point-and-mark :after #'org-mark-jump-unhide)
(advice-add 'pop-global-mark :after #'org-mark-jump-unhide)

;;;; Session

;; Make "session.el" ignore our circular variable.
(defvar session-globals-exclude)
(eval-after-load 'session
  '(add-to-list 'session-globals-exclude 'org-mark-ring))

;;;; outline-mode

;; Folding in outline-mode is not compatible with org-mode folding
;; anymore. Working around to avoid breakage of external packages
;; assuming the compatibility.
(define-advice outline-flag-region (:around (oldfun from to flag &rest extra) fix-for-org-fold)
  "Run `org-fold-region' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-fold-region (max from (point-min)) (min to (point-max)) flag 'headline)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun from to flag extra)))

(define-advice outline-next-visible-heading (:around (oldfun arg &rest extra) fix-for-org-fold)
  "Run `org-next-visible-heading' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-next-visible-heading arg)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun arg extra)))

(define-advice outline-back-to-heading (:around (oldfun &optional invisible-ok &rest extra) fix-for-org-fold)
  "Run `org-back-to-heading' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (progn
        (forward-line 0)
        (or (org-at-heading-p (not invisible-ok))
            (let (found)
	      (save-excursion
	        (while (not found)
	          (or (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				          nil t)
                      (signal 'outline-before-first-heading nil))
	          (setq found (and (or invisible-ok (not (org-invisible-p)))
			           (point)))))
	      (goto-char found)
	      found)))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun invisible-ok extra)))

(define-advice outline-on-heading-p (:around (oldfun &optional invisible-ok &rest extra) fix-for-org-fold)
  "Run `org-at-heading-p' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-at-heading-p (not invisible-ok))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun invisible-ok extra)))

(define-advice outline-hide-sublevels (:around (oldfun levels &rest extra) fix-for-org-fold)
  "Run `org-fold-hide-sublevels' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-fold-hide-sublevels levels)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun levels extra)))

(define-advice outline-toggle-children (:around (oldfun &rest extra) fix-for-org-fold)
  "Run `org-fold-hide-sublevels' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading)
        (if (not (org-invisible-p (line-end-position)))
            (org-fold-hide-subtree)
          (org-fold-show-children)
          (org-fold-show-entry 'hide-drawers)))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun extra)))

;; TODO: outline-headers-as-kill

;;;; Speed commands

(make-obsolete-variable 'org-speed-commands-user
                        "configure `org-speed-commands' instead." "9.5")
(provide 'org-compat)

;;;; yank-media
;; Emacs 29's pgtk port has a bug where it might fail to return the
;; right TARGET.  Install a workaround for Emacs <=29 since the fix
;; went to Emacs 30.  See bug#72254.
;; Org bug report link: https://list.orgmode.org/orgmode/87ed7kttoa.fsf@k-7.ch
;; This should be removed once we drop Emacs 29 support.
(when (and (fboundp 'pgtk-get-selection-internal)
           (<= emacs-major-version 29))
  ;; Only define the method if it hasn't been previously defined.
  (unless (cl-find-method 'gui-backend-get-selection nil
                          '((eql 'CLIPBOARD) (eql 'TARGETS)
                            ((&context . window-system) eql 'pgtk)))
    (cl-defmethod gui-backend-get-selection ((selection-symbol (eql 'CLIPBOARD))
                                             (target-type (eql 'TARGETS))
                                             &context (window-system pgtk))
      (let ((sel (pgtk-get-selection-internal selection-symbol target-type)))
        (if (vectorp sel)
            sel
          (vector sel))))))

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-compat.el ends here
