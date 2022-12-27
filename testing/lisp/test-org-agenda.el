;;; test-org-agenda.el --- Tests for org-agenda.el -*- lexical-binding: t ; -*-

;; Copyright (C) 2017, 2019 Marco Wahl

;; Author: Marco Wahl <marcowahlsoft@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for Org Agenda.

;;; Code:

(require 'org-test "../testing/org-test")
(require 'org-agenda)
(eval-and-compile (require 'cl-lib))


;; General auxiliaries

(defun org-test-agenda--agenda-buffers ()
  "Return agenda buffers in a list."
  (cl-remove-if-not (lambda (x)
		      (with-current-buffer x
			(eq major-mode 'org-agenda-mode)))
		    (buffer-list)))

(defun org-test-agenda--kill-all-agendas ()
  "Kill all agenda buffers."
  (mapc #'kill-buffer
	(org-test-agenda--agenda-buffers)))

(defmacro org-test-agenda-with-agenda (text &rest body)
  (declare (indent 1))
  `(org-test-with-temp-text-in-file ,text
     (let ((org-agenda-files `(,buffer-file-name)))
       ,@body
       (org-test-agenda--kill-all-agendas))))


;; Test the Agenda

(ert-deftest test-org-agenda/empty ()
  "Empty agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
        org-agenda-files)
    (org-agenda-list)
    (set-buffer org-agenda-buffer-name)
    (should (= 2 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/one-line ()
  "One informative line in the agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    ;; NOTE: Be aware that `org-agenda-list' may or may not display
    ;; past scheduled items depending whether the date is today
    ;; `org-today' or not.
    (org-agenda-list nil  "<2017-03-10 Fri>")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/scheduled-non-todo ()
  "One informative line in the agenda from scheduled non-todo-keyword-item."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    ;; NOTE: Be aware that `org-agenda-list' may or may not display
    ;; past scheduled items depending whether the date is today
    ;; `org-today' or not.
    (org-agenda-list nil "<2017-07-19 Wed>")
    (set-buffer org-agenda-buffer-name)
    (should
     (progn (goto-line 3)
	    (looking-at " *agenda-file:Scheduled: *test agenda"))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/non-scheduled-re-matces ()
  "Make sure that scheduled-looking elements do not appear in agenda.
See https://list.orgmode.org/20220101200103.GB29829@itccanarias.org/T/#t."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    ;; NOTE: Be aware that `org-agenda-list' may or may not display
    ;; past scheduled items depending whether the date is today
    ;; `org-today' or not.
    (org-agenda-list nil "<2022-01-03 Mon>")
    (set-buffer org-agenda-buffer-name)
    (should (= 2 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/org-search-view ()
  "Test `org-search-view' specifications."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  ;; Search a string.
  (let ((org-agenda-files `(,(expand-file-name "examples/agenda-search.org"
					       org-test-dir))))
    (org-search-view nil "foo")
    (set-buffer org-agenda-buffer-name)
    (should (= 4 (count-lines (point-min) (point-max)))))
  ;; Search past inlinetask.
  (let ((org-agenda-files `(,(expand-file-name "examples/agenda-search.org"
					       org-test-dir))))
    (org-search-view nil "bar")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max)))))
  ;; Search inside inlinetask.
  (let ((org-agenda-files `(,(expand-file-name "examples/agenda-search.org"
					       org-test-dir))))
    (org-search-view nil "text inside inlinetask")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/property-timestamp ()
  "Match timestamps inside property drawer.
See https://list.orgmode.org/06d301d83d9e$f8b44340$ea1cc9c0$@tomdavey.com"
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    ;; NOTE: Be aware that `org-agenda-list' may or may not display
    ;; past scheduled items depending whether the date is today
    ;; `org-today' or not.
    (org-agenda-list nil "<2022-03-22 Tue>")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max))))
    ;; NOTE: Be aware that `org-agenda-list' may or may not display
    ;; past scheduled items depending whether the date is today
    ;; `org-today' or not.
    (org-agenda-list nil "<2022-03-25 Fri>")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/set-priority ()
  "One informative line in the agenda. Check that org-agenda-priority updates the agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    ;; NOTE: Be aware that `org-agenda-list' may or may not display
    ;; past scheduled items depending whether the date is today
    ;; `org-today' or not.
    (org-agenda-list nil "<2017-07-19 Wed>")
    (set-buffer org-agenda-buffer-name)
    (should
     (progn (goto-line 3)
	    (org-agenda-priority ?B)
	    (looking-at-p " *agenda-file:Scheduled: *\\[#B\\] test agenda"))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/sticky-agenda-name ()
  "Agenda buffer name after having created one sticky agenda buffer."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(buf (get-buffer org-agenda-buffer-name))
        org-agenda-files)
    (when buf (kill-buffer buf))
    (dolist (fn '(org-agenda-list org-todo-list))
      (org-test-with-temp-text "<2017-03-17 Fri>"
			       (org-follow-timestamp-link)) ;creates a sticky agenda
      (org-test-agenda--kill-all-agendas)
      (funcall fn)
      (should (= 1 (length (org-test-agenda--agenda-buffers))))
      (should (string= "*Org Agenda*"
		       (buffer-name (car (org-test-agenda--agenda-buffers)))))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/sticky-agenda-name-after-reload ()
  "Agenda buffer name of sticky agenda after reload."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (org-toggle-sticky-agenda)
  (let (org-agenda-files)
    (org-agenda-list)
    (let* ((agenda-buffer-name
	    (progn
	      (cl-assert (= 1 (length (org-test-agenda--agenda-buffers))))
	      (buffer-name (car (org-test-agenda--agenda-buffers))))))
      (set-buffer agenda-buffer-name)
      (org-agenda-redo)
      (should (= 1 (length (org-test-agenda--agenda-buffers))))
      (should (string= agenda-buffer-name
                       (buffer-name (car (org-test-agenda--agenda-buffers)))))))
  (org-toggle-sticky-agenda)
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/sticky-agenda-filter-preset ()
  "Update sticky agenda buffers properly with preset of filters."
  (unless org-agenda-sticky
    (org-toggle-sticky-agenda))
  (org-test-agenda-with-agenda "* TODO Foo"
    (org-set-property "CATEGORY" "foo")
    (let ((org-agenda-custom-commands
           '(("f" "foo: multi-command"
	      ((tags-todo "+CATEGORY=\"foo\"")
               (alltodo ""))
              ((org-agenda-category-filter-preset '("+foo"))))
             ("b" "bar: multi-command"
	      ((tags-todo "+CATEGORY=\"bar\"")
               (alltodo ""))
              ((org-agenda-category-filter-preset '("+bar"))))
             ("f1" "foo: single-command"
	      tags-todo "+CATEGORY=\"foo\""
              ((org-agenda-category-filter-preset '("+foo"))))
             ("b1" "bar: single-command"
	      tags-todo "+CATEGORY=\"bar\""
              ((org-agenda-category-filter-preset '("+bar"))))
             ("f2" "foo: single-command"
	      alltodo "" ((org-agenda-category-filter-preset '("+foo"))))
             ("b2" "bar: single-command"
	      alltodo "" ((org-agenda-category-filter-preset '("+bar")))))))
      (org-agenda nil "f")
      (org-agenda nil "b")
      (set-buffer "*Org Agenda(f)*")
      (org-agenda-redo)
      (goto-char (point-min))
      (should (not (invisible-p (1- (search-forward "TODO Foo")))))
      (org-test-agenda--kill-all-agendas)
      (org-agenda nil "f1")
      (org-agenda nil "b1")
      (set-buffer "*Org Agenda(f1:+CATEGORY=\"foo\")*")
      (org-agenda-redo)
      (goto-char (point-min))
      (should (not (invisible-p (1- (search-forward "TODO Foo")))))
      (org-test-agenda--kill-all-agendas)
      (org-agenda nil "f2")
      (org-agenda nil "b2")
      (set-buffer "*Org Agenda(f2)*")
      (org-agenda-redo)
      (goto-char (point-min))
      (should (not (invisible-p (1- (search-forward "TODO Foo")))))))
  (org-toggle-sticky-agenda))

(ert-deftest test-org-agenda/goto-date ()
  "Test `org-agenda-goto-date'."
  (unwind-protect
      (should
       (equal
        (time-to-days (org-time-string-to-time "2019-12-30"))
        (let ((org-agenda-files nil))
          (org-agenda-list nil nil 'day)
          (org-agenda-goto-date "2019-12-30")
          (get-text-property (point) 'day))))
    (org-test-agenda--kill-all-agendas)))

(ert-deftest test-org-agenda/file-restriction ()
  "Test file restriction for org agenda."
  (org-test-with-temp-text-in-file "* TODO Foo"
    (org-agenda-set-restriction-lock t)
    (org-agenda nil "t")
    (should (search-forward "Foo"))
    (should (org-agenda-files))
    (should-not (org-agenda-files t))
    (org-agenda-remove-restriction-lock)
    (goto-char (point-min))
    (should-not (search-forward "Foo" nil t))
    (should-not (org-agenda-files)))
  (org-test-with-temp-text-in-file "* TODO Bar"
    (org-agenda nil "t" 'buffer)
    (should (search-forward "Bar"))
    (should (org-agenda-files))
    (should-not (org-agenda-files t))
    (org-agenda-remove-restriction-lock)
    (goto-char (point-min))
    (should-not (search-forward "Bar" nil t))
    (should-not (org-agenda-files)))
  (org-test-with-temp-text-in-file "* TODO Bar"
    (org-agenda nil "t" 'buffer)
    (org-agenda nil "t")
    (should-not (search-forward "Bar" nil t))
    (should-not (org-agenda-files)))
  (org-test-agenda--kill-all-agendas))


;; agenda redo

(require 'face-remap)

(ert-deftest test-org-agenda/rescale ()
  "Text scale survives `org-agenda-redo'."
  (org-test-agenda--kill-all-agendas)
  (unwind-protect
      (let ((org-agenda-span 'day)
         org-agenda-files)
     (org-agenda-list)
     (set-buffer org-agenda-buffer-name)
     (text-scale-mode)
     (text-scale-set 11)
     (cl-assert (and (boundp text-scale-mode) text-scale-mode))
     (org-agenda-redo)
     (should text-scale-mode)
     (should (= 11 text-scale-mode-amount)))
   (org-test-agenda--kill-all-agendas)))

(ert-deftest test-org-agenda/redo-setting ()
  "Command settings survives `org-agenda-redo'."
  (org-test-agenda--kill-all-agendas)
  (let ((org-agenda-custom-commands
         '(("t" "TODOs" alltodo ""
            ((org-agenda-overriding-header "Test"))))))
    (org-agenda nil "t")
    (org-agenda-redo)
    (org-agenda-redo)
    (goto-char (point-min))
    (should (looking-at-p "Test")))
  (org-test-agenda--kill-all-agendas))


(ert-deftest test-org-agenda/diary-inclusion ()
  "Diary inclusion happens."
  (org-test-agenda--kill-all-agendas)
  (let ((diary-file (expand-file-name "examples/diary-file" org-test-dir))
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir)))
	(diary-date-forms '((month "[-/]" day "[^-/0-9]")
			    (year "[-/]" month "[-/]" day "[^0-9]")
			    (monthname " *" day "[^-0-9]")
			    (year " *" monthname " *" day "[^0-9]")
			    (dayname "\\W")))
	(org-agenda-span 'day)
	(org-agenda-include-diary t))
    ;; NOTE: Be aware that `org-agenda-list' may or may not display
    ;; past scheduled items depending whether the date is today
    ;; `org-today' or not.
    (org-agenda-list nil "<2019-01-08>")
    (should (search-forward "f0bcf0cd8bad93c1451bb6e1b2aaedef5cce7cbb" nil t))
    (org-test-agenda--kill-all-agendas)))

;; agenda bulk actions

(ert-deftest test-org-agenda/bulk ()
  "Bulk actions are applied to marked items."
  (org-test-agenda-with-agenda "* TODO a\n* TODO b"
    (org-todo-list)
    (org-agenda-bulk-mark-all)
    (cl-letf (((symbol-function 'read-char-exclusive)
               (lambda () ?t))
              ((symbol-function 'completing-read)
               (lambda (&rest _rest) "DONE")))
      (org-agenda-bulk-action))
    (org-agenda-previous-item 99)
    (should (looking-at ".*DONE a"))
    (org-agenda-next-item 1)
    (should (looking-at ".*DONE b"))))

(ert-deftest test-org-agenda/bulk-custom ()
  "Custom bulk actions are applied to all marked items."
  (org-test-agenda-with-agenda "* TODO a\n* TODO b"
    (org-todo-list)
    (org-agenda-bulk-mark-all)

    ;; Mock read functions
    (let* ((f-call-cnt 0)
           (org-agenda-bulk-custom-functions
           `((?P ,(lambda () (setq f-call-cnt (1+ f-call-cnt)))))))
      (cl-letf* (((symbol-function 'read-char-exclusive)
                  (lambda () ?P)))
        (org-agenda-bulk-action)
        (should (= f-call-cnt 2))))))

(ert-deftest test-org-agenda/bulk-custom-arg-func ()
  "Argument collection functions can be specified for custom bulk
functions."
  (org-test-agenda-with-agenda "* TODO a\n* TODO b"
    (org-todo-list)
    (org-agenda-bulk-mark-all)
    (let* ((f-called-cnt 0)
           (arg-f-call-cnt 0)
           (f-called-args nil)
           (org-agenda-bulk-custom-functions
            `((?P
               ;; Custom bulk function
               ,(lambda (&rest args)
                  (message "test")
                  (setq f-called-cnt (1+ f-called-cnt)

                        f-called-args args))
               ;; Argument collection function
               ,(lambda ()
                  (setq arg-f-call-cnt (1+ arg-f-call-cnt))
                  '(1 2 3))))))
      (cl-letf (((symbol-function 'read-char-exclusive)
                 (lambda () ?P)))
        (org-agenda-bulk-action))
      (should (= f-called-cnt 2))
      (should (= arg-f-call-cnt 1))
      (should (equal f-called-args '(1 2 3))))))



(provide 'test-org-agenda)

;;; test-org-agenda.el ends here
