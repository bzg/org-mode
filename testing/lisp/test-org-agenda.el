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

(ert-deftest test-org-agenda/time-grid ()
  "Test time grid settings."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  ;; Default time grid.
  (org-test-at-time "2024-01-17 8:00"
    (let ((org-agenda-span 'day)
	  (org-agenda-files `(,(expand-file-name "examples/agenda-file2.org"
					         org-test-dir))))
      ;; NOTE: Be aware that `org-agenda-list' may or may not display
      ;; past scheduled items depending whether the date is today
      ;; `org-today' or not.
      (org-agenda-list nil  "<2024-01-17 Fri>")
      (set-buffer org-agenda-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "8:00...... now - - - - - - - - - - - - - - - - - - - - - - - - -")))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "agenda-file2: 9:30-10:00 Scheduled:  TODO one")))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "agenda-file2:10:00-12:30 Scheduled:  TODO two")))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "10:00...... ----------------")))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "agenda-file2:13:00-15:00 Scheduled:  TODO three")))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "agenda-file2:19:00...... Scheduled:  TODO four"))))
    (org-test-agenda--kill-all-agendas))
  ;; Custom time grid strings
  (org-test-at-time "2024-01-17 8:00"
    (let ((org-agenda-span 'day)
	  (org-agenda-files `(,(expand-file-name "examples/agenda-file2.org"
					         org-test-dir)))
          (org-agenda-time-grid '((daily today require-timed)
                                  (800 1000 1200 1400 1600 1800 2000)
			          "..." "^^^^^^^^^^^^^^" )))
      ;; NOTE: Be aware that `org-agenda-list' may or may not display
      ;; past scheduled items depending whether the date is today
      ;; `org-today' or not.
      (org-agenda-list nil  "<2024-01-17 Fri>")
      (set-buffer org-agenda-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "10:00...    ^^^^^^^^^^^^^^"))))
    (org-test-agenda--kill-all-agendas))
  ;; Time grid remove-match
  (org-test-at-time "2024-01-17 8:00"
    (let ((org-agenda-span 'day)
	  (org-agenda-files `(,(expand-file-name "examples/agenda-file2.org"
					         org-test-dir)))
          (org-agenda-time-grid '((today remove-match)
			          (800 1000 1200 1400 1600 1800 2000)
			          "......" "----------------" )))
      ;; NOTE: Be aware that `org-agenda-list' may or may not display
      ;; past scheduled items depending whether the date is today
      ;; `org-today' or not.
      (org-agenda-list nil  "<2024-01-17 Fri>")
      (set-buffer org-agenda-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "agenda-file2: 9:30-10:00 Scheduled:  TODO one")))
      (save-excursion
        (goto-char (point-min))
        (should-not (search-forward "10:00...... ----------------" nil t))))
    (org-test-agenda--kill-all-agendas))
  ;; Time grid with `org-agenda-default-appointment-duration'
  (org-test-at-time "2024-01-17 8:00"
    (let ((org-agenda-span 'day)
	  (org-agenda-files `(,(expand-file-name "examples/agenda-file2.org"
					         org-test-dir)))
          (org-agenda-time-grid '((today remove-match)
			          (800 1000 1200 1400 1600 1800 2000)
			          "......" "----------------" ))
          (org-agenda-default-appointment-duration 60))
      ;; NOTE: Be aware that `org-agenda-list' may or may not display
      ;; past scheduled items depending whether the date is today
      ;; `org-today' or not.
      (org-agenda-list nil  "<2024-01-17 Fri>")
      (set-buffer org-agenda-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "agenda-file2:19:00-20:00 Scheduled:  TODO four")))
      ;; Bug https://list.orgmode.org/orgmode/20211119135325.7f3f85a9@hsu-hh.de/
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "14:00...... ----------------"))))
    (org-test-agenda--kill-all-agendas)))

(ert-deftest test-org-agenda/todo-selector ()
  "Test selecting keywords in `org-todo-list'."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-todo-keywords
         '((sequence "[ ]" "[X]")
           (sequence "TODO" "NEXT" "|" "DONE"))))
    (org-test-agenda-with-agenda
     "
* [ ] Unchecked and will appear in agenda
* NEXT NEXT will appear in agenda
* [X] Checked and will not appear in agenda
* TODO Todo and will appear in agenda
* DONE Done and will not appear in agenda
"
     ;; All todo keywords.
     (org-todo-list)
     (set-buffer org-agenda-buffer-name)
     (should
      (progn
        "all todo"
        (goto-char (point-min))
        (search-forward "[ ] Unchecked and will appear in agenda" nil t)))
     (should
      (progn
        "all todo"
        (goto-char (point-min))
        (search-forward "NEXT NEXT will appear in agenda" nil t)))
     (should
      (progn
        "all todo"
        (goto-char (point-min))
        (search-forward "TODO Todo and will appear in agenda" nil t)))
     
     ;; All todo keywords, including not done.
     (org-todo-list "*")
     (should
      (progn
        "all keywords"
        (goto-char (point-min))
        (search-forward "[ ] Unchecked and will appear in agenda" nil t)))
     (should
      (progn
        "all keywords"
        (goto-char (point-min))
        (search-forward "[X] Checked and will not appear in agenda" nil t)))
     (should
      (progn
        "all keywords"
        (goto-char (point-min))
        (search-forward "DONE Done and will not appear in agenda" nil t)))
     (should
      (progn
        "all keywords"
        (goto-char (point-min))
        (search-forward "NEXT NEXT will appear in agenda" nil t)))
     (should
      (progn
        "all keywords"
        (goto-char (point-min))
        (search-forward "TODO Todo and will appear in agenda" nil t)))
     ;; Just [ ] regexp-like entry.
     (org-todo-list "[ ]")
     (should
      (progn
        "[ ] keyword"
        (goto-char (point-min))
        (search-forward "[ ] Unchecked and will appear in agenda" nil t)))
     (should-not
      (progn
        "[ ] keyword"
        (goto-char (point-min))
        (search-forward "NEXT NEXT will appear in agenda" nil t)))
     (should-not
      (progn
        "[ ] keyword"
        (goto-char (point-min))
        (search-forward "TODO Todo and will appear in agenda" nil t)))
     ;; Two keywords.
     (org-todo-list "NEXT|TODO")
     (should-not
      (progn
        "NEXT|TODO"
        (goto-char (point-min))
        (search-forward "[ ] Unchecked and will appear in agenda" nil t)))
     (should
      (progn
        "NEXT|TODO"
        (goto-char (point-min))
        (search-forward "NEXT NEXT will appear in agenda" nil t)))
     (should
      (progn
        "NEXT|TODO"
        (goto-char (point-min))
        (search-forward "TODO Todo and will appear in agenda" nil t))))))

(ert-deftest test-org-agenda/scheduled-non-todo ()
  "One informative line in the agenda from scheduled non-todo-keyword-item."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (dolist (org-element-use-cache '(t nil))
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
    (org-test-agenda--kill-all-agendas)))

(ert-deftest test-org-agenda/non-scheduled-re-matches ()
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

(ert-deftest test-org-agenda/skip-if ()
  "Test `org-agenda-skip-if'."
  (dolist (options '((scheduled) (notscheduled)
                     (deadline) (notdeadline)
                     (timestamp) (nottimestamp)
                     (regexp "hello") (notregexp "hello")
                     ;; TODO: Test for specific TODO keywords
                     (todo ("*")) (nottodo ("*"))))
    (should
     (equal
      (if (memq (car options) '(notscheduled notdeadline nottimestamp regexp nottodo))
          8
        nil)
      (org-test-with-temp-text
          "* hello"
        (org-agenda-skip-if nil options))))
    (should
     (equal
      (if (memq (car options) '(scheduled notdeadline timestamp regexp nottodo))
          36
        nil)
      (org-test-with-temp-text
          "* hello
SCHEDULED: <2023-07-15 Sat>"
        (org-agenda-skip-if nil options))))
    (should
     (equal
      (if (memq (car options) '(notscheduled deadline timestamp regexp nottodo))
          35
        nil)
      (org-test-with-temp-text
          "* hello
DEADLINE: <2023-07-15 Sat>"
        (org-agenda-skip-if nil options))))
    (should
     (equal
      (if (memq (car options) '(notscheduled notdeadline timestamp regexp nottodo))
          25
        nil)
      (org-test-with-temp-text
          "* hello
<2023-07-15 Sat>"
        (org-agenda-skip-if nil options))))
    (should
     (equal
      (if (memq (car options) '(notscheduled notdeadline nottimestamp notregexp nottodo))
          10
        nil)
      (org-test-with-temp-text
          "* goodbye"
        (org-agenda-skip-if nil options))))
    (should
     (equal
      (if (memq (car options) '(notscheduled notdeadline nottimestamp notregexp todo))
          26
        nil)
      (org-test-with-temp-text
          "* TODO write better tests"
        (org-agenda-skip-if nil options))))))

(ert-deftest test-org-agenda/timestamp-ignore-todo-item ()
  "Test if `org-agenda' ignores a todo item with a timestamp.

Based on the following variables:
`org-agenda-todo-ignore-deadlines',
`org-agenda-todo-ignore-scheduled', and
`org-agenda-todo-ignore-timestamp'."
  ;; TODO: test `org-agenda-todo-ignore-with-date'.
  ;; Maybe test having multiple variables set.
  (let ((org-agenda-custom-commands
         '(("f" "no fluff" todo ""
            ((org-agenda-todo-keyword-format "")
             (org-agenda-overriding-header "")
             (org-agenda-prefix-format "")))))
        (org-deadline-warning-days 1)
        (expected-return
         (lambda (timestamp value)
           (cl-case timestamp
             (past            (memq value '(all near past -1 -2 -3)))
             (yesteryesterday (memq value '(all near past -1 -2)))
             (yesterday       (memq value '(all near past -1)))
             (today           (memq value '(all near past 0)))
             (tomorrow        (memq value '(all near future 0 1)))
             (tomorroworrow   (memq value '(all far  future 0 1 2)))
             (future          (memq value '(all far  future 0 1 2 3))))))
        ;; Lexically bind the variables we're changing
        org-agenda-todo-ignore-deadlines
        org-agenda-todo-ignore-scheduled
        org-agenda-todo-ignore-timestamp)
    (org-test-at-time "2023-01-15"
      (dolist (variable '(org-agenda-todo-ignore-deadlines
                          org-agenda-todo-ignore-scheduled
                          org-agenda-todo-ignore-timestamp))
        (dolist (type '(timestamp scheduled deadline))
          ;; nil is last so it resets the variable for the next one
          (dolist (value `(past future all 3 2 1 0 -1 -2 -3
                                ,@(when (eq type 'deadline) '(near far nil))))
            (dolist (timestamp '((past .            "<2023-01-01>")
                                 (yesteryesterday . "<2023-01-13>")
                                 (yesterday .       "<2023-01-14>")
                                 (today .           "<2023-01-15>")
                                 (tomorrow .        "<2023-01-16>")
                                 (tomorroworrow .   "<2023-01-17>")
                                 (future .          "<2023-01-31>")))
              ;; Uncomment to debug failure
              ;; (message "Type: %S, Variable: %S, Value: %S, Time: %S" type variable value (car timestamp))
              (set variable value)
              (org-test-agenda-with-agenda
                  (cl-case type
                    (timestamp (concat "* TODO hello\n" (cdr timestamp)))
                    (scheduled (concat "* TODO hello
SCHEDULED: " (cdr timestamp)))
                    (deadline (concat "* TODO hello
DEADLINE: " (cdr timestamp))))
                (should
                 (string-equal
                  (or (and (funcall expected-return (car timestamp) value)
                           (cl-case variable
                             (org-agenda-todo-ignore-deadlines
                              (eq type 'deadline))
                             (org-agenda-todo-ignore-scheduled
                              (eq type 'scheduled))
                             (org-agenda-todo-ignore-timestamp
                              (eq type 'timestamp)))
                           "")
                      "hello\n")
                  (progn
                    (org-agenda nil "f")
                    (buffer-string))))))))))))

(ert-deftest test-org-agenda/skip-scheduled-repeats-after-deadline ()
  "Test `org-agenda-skip-scheduled-repeats-after-deadline'."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (dolist (org-agenda-skip-scheduled-repeats-after-deadline '(nil t))
    (org-test-at-time "2024-01-01 8:00"
      (org-test-with-temp-text-in-file "
* TODO Do me every day until Jan, 5th (inclusive)
SCHEDULED: <2024-01-03 Wed +1d> DEADLINE: <2024-01-05 Fri>
"
        (let ((org-agenda-span 'week)
	      (org-agenda-files `(,(buffer-file-name))))
          ;; NOTE: Be aware that `org-agenda-list' may or may not display
          ;; past scheduled items depending whether the date is today
          ;; `org-today' or not.
          (org-agenda-list nil  "<2024-01-01 Mon>")
          (set-buffer org-agenda-buffer-name)
          (if org-agenda-skip-scheduled-repeats-after-deadline
              (should
               ;; Not displayed after deadline.
               (string-match-p
                "Week-agenda (W01):
Monday      1 January 2024 W01
  [^:]+:In   4 d.:  TODO Do me every day until Jan, 5th (inclusive)
Tuesday     2 January 2024
Wednesday   3 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)
Thursday    4 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)
Friday      5 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)
  [^:]+:Deadline:   TODO Do me every day until Jan, 5th (inclusive)
Saturday    6 January 2024
Sunday      7 January 2024"
                (buffer-string)))
            (should
             ;; Displayed after deadline.
             (string-match-p
              "Week-agenda (W01):
Monday      1 January 2024 W01
  [^:]+:In   4 d.:  TODO Do me every day until Jan, 5th (inclusive)
Tuesday     2 January 2024
Wednesday   3 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)
Thursday    4 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)
Friday      5 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)
  [^:]+:Deadline:   TODO Do me every day until Jan, 5th (inclusive)
Saturday    6 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)
Sunday      7 January 2024
  [^:]+:Scheduled:  TODO Do me every day until Jan, 5th (inclusive)"
              (buffer-string))))))
      (org-test-agenda--kill-all-agendas))))

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

(ert-deftest test-org-agenda/skip-deadline-prewarning-if-scheduled ()
  "Test `org-agenda-skip-deadline-prewarning-if-scheduled'."
  (org-test-at-time
      "2024-01-15"
    (let ((org-agenda-skip-deadline-prewarning-if-scheduled t))
      (org-test-agenda-with-agenda
       "* TODO foo\nDEADLINE: <2024-01-20 Sat> SCHEDULED: <2024-01-19 Fri>"
       (org-agenda-list nil nil 1)
       (should-not (search-forward "In " nil t))))
    (let ((org-agenda-skip-deadline-prewarning-if-scheduled 10))
      (org-test-agenda-with-agenda
       "* TODO foo\nDEADLINE: <2024-01-20 Sat> SCHEDULED: <2024-01-19 Fri>"
       (org-agenda-list nil nil 1)
       (should (search-forward "In " nil t))))
    ;; Custom prewarning cookie "-3d", so there should be no warning anyway.
    (let ((org-agenda-skip-deadline-prewarning-if-scheduled 10))
      (org-test-agenda-with-agenda
       "* TODO foo\nDEADLINE: <2024-01-20 Sat -3d> SCHEDULED: <2024-01-19 Fri>"
       (org-agenda-list nil nil 1)
       (should-not (search-forward "In " nil t))))
    (let ((org-agenda-skip-deadline-prewarning-if-scheduled 3))
      (org-test-agenda-with-agenda
       "* TODO foo\nDEADLINE: <2024-01-20 Sat> SCHEDULED: <2024-01-19 Fri>"
       (org-agenda-list nil nil 1)
       (should-not (search-forward "In " nil t))))
    (let ((org-agenda-skip-deadline-prewarning-if-scheduled nil))
      (org-test-agenda-with-agenda
       "* TODO foo\nDEADLINE: <2024-01-20 Sat> SCHEDULED: <2024-01-19 Fri>"
       (org-agenda-list nil nil 1)
       (should (search-forward "In " nil t))))
    (let ((org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled))
      (org-test-agenda-with-agenda
       "* TODO foo\nDEADLINE: <2024-01-20 Sat> SCHEDULED: <2024-01-16 Tue>"
       (org-agenda-list nil nil 1)
       (should-not (search-forward "In " nil t))))
    (let ((org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled))
      (org-test-agenda-with-agenda
       "* TODO foo\nDEADLINE: <2024-01-20 Sat> SCHEDULED: <2024-01-15 Mon>"
       (org-agenda-list nil nil 1)
       (should (search-forward "In " nil t))))))

(ert-deftest test-org-agenda/diary-timestamp ()
  "Test diary timestamp handling."
  (org-test-at-time
      "2024-01-15"
    (org-test-agenda-with-agenda
        "* TODO foo\n<%%(diary-date 01 15 2024)>"
      (org-agenda-list nil nil 1)
      (should (search-forward "foo" nil t)))
    (org-test-agenda-with-agenda
        "* TODO foo\n<%%(diary-date 02 15 2024)>"
      (org-agenda-list nil nil 1)
      (should-not (search-forward "foo" nil t)))
    ;; Test time and time ranges in diary timestamps.
    (org-test-agenda-with-agenda
        "* TODO foo\n<%%(diary-date 01 15 2024) 12:00>"
      (org-agenda-list nil nil 1)
      (should (search-forward "12:00" nil t)))
    (org-test-agenda-with-agenda
        "* TODO foo\n<%%(diary-date 01 15 2024) 12:00-14:00>"
      (org-agenda-list nil nil 1)
      (should (search-forward "12:00-14:00" nil t)))))


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
