;;; test-org-habit.el --- Tests for org-habit.el -*- lexical-binding: t ; -*-

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

;; Unit tests for Org Habits.

;;; Code:

(require 'org-test "../testing/org-test")
(require 'org-agenda)
(require 'org-habit)
(require 'org-inlinetask)
(eval-when-compile
  (require 'test-org-agenda "../testing/lisp/test-org-agenda"))
(eval-when-compile
  (require 'test-org "../testing/lisp/test-org"))


;; Tests

(defvar org-test-habit-no-fluff-agenda
  '(("f" "no fluff" agenda ""
     ((org-agenda-overriding-header "")
      (org-agenda-format-date "")
      (org-agenda-span 'day)
      (org-agenda-show-all-dates nil)
      (org-agenda-todo-keyword-format "")
      (org-agenda-prefix-format "")))))

(defun org-test-habit-agenda-string (repeater-type-string
                                     repeater-deadline?
                                     &optional
                                     inlinetask-p)
  "Return an org habit test string.
REPEATER-TYPE-STRING is used as the repeater type (ex.  \".+\").
When REPEATER-DEADLINE? is non-nil, add a repeater deadline.
Order is determined by `org-log-states-order-reversed'.
When INLINETASK-P is non-nil, make the habit an inlinetask, otherwise
make it a normal heading."
  (concat
   (if inlinetask-p
       (make-string (1- org-inlinetask-min-level) ?*)
     "")
   "* TODO Shave
SCHEDULED: <2009-10-17 Sat " repeater-type-string "2d"
   (if repeater-deadline?
       "/4d"
     "")
   ">
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2009-10-19 Mon 00:36]
:END:
"

   (if org-log-states-order-reversed
       "- State \"DONE\"       from \"TODO\"       [2009-10-15 Thu]
- State \"DONE\"       from \"TODO\"       [2009-10-12 Mon]
- CLOSING NOTE [2009-10-10 Sat] \\
  this style occurs when `org-log-done' is `note'.
- State \"DONE\"       from \"TODO\"       [2009-10-04 Sun]
- State \"DONE\"       from \"TODO\"       [2009-10-02 Fri]
- State \"DONE\"       from \"TODO\"       [2009-09-29 Tue]
- State \"DONE\"       from \"TODO\"       [2009-09-25 Fri]
- State \"DONE\"       from \"TODO\"       [2009-09-19 Sat]
- State \"DONE\"       from \"TODO\"       [2009-09-16 Wed]
- State \"DONE\"       from \"TODO\"       [2009-09-12 Sat]"

     "- State \"DONE\"       from \"TODO\"       [2009-09-12 Sat]
- State \"DONE\"       from \"TODO\"       [2009-09-16 Wed]
- State \"DONE\"       from \"TODO\"       [2009-09-19 Sat]
- State \"DONE\"       from \"TODO\"       [2009-09-25 Fri]
- State \"DONE\"       from \"TODO\"       [2009-09-29 Tue]
- State \"DONE\"       from \"TODO\"       [2009-10-02 Fri]
- State \"DONE\"       from \"TODO\"       [2009-10-04 Sun]
- CLOSING NOTE [2009-10-10 Sat] \\
  this style occurs when `org-log-done' is `note'.
- State \"DONE\"       from \"TODO\"       [2009-10-12 Mon]
- State \"DONE\"       from \"TODO\"       [2009-10-15 Thu]")
   (if inlinetask-p
       (concat
        "\n" (make-string org-inlinetask-min-level ?*) " end")
     "")))

(defmacro org-test-habit (&rest body)
  "Run BODY multiple times for testing habits.
Add agenda from `org-test-habit-no-fluff-agenda' to
`org-agenda-custom-commands'.

Use habit data from `org-test-habit-agenda-string' both with and without
a repeater deadline and the log data reversed and not-reversed."
  (declare (indent 0))
  `(let ((org-agenda-custom-commands
          org-test-habit-no-fluff-agenda))
     (dolist (org-log-states-order-reversed '(t nil))
       (dolist (repeater-deadline? '(nil t))
         (dolist (repeater-type-string '(".+" "+" "++"))
           (dolist (inlinetask-p '(nil t))
             (org-test-agenda-with-agenda
              (org-test-habit-agenda-string repeater-type-string
                                            repeater-deadline?
                                            inlinetask-p)
              ,@body)))))))

(ert-deftest test-org-habit/simple-habit ()
  "Test the agenda view for a simple habit."
  ;; Avoid DST when TZ="Europe/Istanbul".  See `test-org-habit/dst'.
  (org-test-with-timezone "UTC0"
    (org-test-at-time "2009-10-22"
      (let ((org-agenda-custom-commands
             org-test-habit-no-fluff-agenda)
            (org-habit-graph-column 5))
        (org-test-agenda-with-agenda
            "* TODO habit
SCHEDULED: <2009-10-21 Sat ++2d>
:PROPERTIES:
:STYLE:    habit
:END:
- State \"DONE\"       from \"TODO\"       [2009-10-19 Sun]
- State \"DONE\"       from \"TODO\"       [2009-10-17 Sun]"
          (should
           (string-equal
            "\nhabit                * *  !       \n"
            (progn
              (org-agenda nil "f")
              (buffer-string)))))))))

(ert-deftest test-org-habit/simple-habit/week ()
  "Test the agenda view for a simple habit."
  (org-test-with-timezone "UTC0"
    (org-test-at-time "2026-06-16"
      (let ((org-agenda-custom-commands
             org-test-habit-no-fluff-agenda)
            (org-habit-graph-column 5))
        (org-test-agenda-with-agenda
            "* TODO habit
SCHEDULED: <2026-06-16 Tue +1w>
:PROPERTIES:
:STYLE:    habit
:END:
- State \"DONE\"       from \"TODO\"       [2026-06-16 Tue]
- State \"DONE\"       from \"TODO\"       [2026-06-09 Tue]
- State \"DONE\"       from \"TODO\"       [2026-06-02 Tue]
- State \"DONE\"       from \"TODO\"       [2026-05-26 Tue]
- State \"DONE\"       from \"TODO\"       [2026-05-12 Tue]
- State \"DONE\"       from \"TODO\"       [2026-05-05 Tue]
- State \"DONE\"       from \"TODO\"       [2026-04-28 Tue]
- State \"DONE\"       from \"TODO\"       [2026-04-21 Tue]
- State \"DONE\"       from \"TODO\"       [2026-04-14 Tue]
- State \"DONE\"       from \"TODO\"       [2026-04-07 Tue]
- State \"DONE\"       from \"TODO\"       [2026-03-31 Tue]"
          (should
           (string-equal
            "\nhabit*      *      *      *       \n"
            (progn
              (org-agenda nil "f")
              (buffer-string)))))))))

(ert-deftest test-org-habit/simple-habit/month ()
  "Test the agenda view for a simple habit."
  ;; Avoid DST when TZ="Europe/Istanbul".  See `test-org-habit/dst'.
  (org-test-with-timezone "UTC0"
    (org-test-at-time "2026-06-17"
      (let ((org-agenda-custom-commands
             org-test-habit-no-fluff-agenda)
            (org-habit-graph-column 5))
        (org-test-agenda-with-agenda
         "* TODO habit
SCHEDULED: <2026-06-17 Wed .+1m/3m>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2026-05-17 Mon 00:36]
:END:
- State \"DONE\"       from \"TODO\"       [2026-05-17 Sun]
- State \"DONE\"       from \"TODO\"       [2026-04-16 Thu]
- State \"DONE\"       from \"TODO\"       [2026-03-17 Tue]
- State \"DONE\"       from \"TODO\"       [2026-02-14 Sat]
- State \"DONE\"       from \"TODO\"       [2025-12-15 Mon]
- State \"DONE\"       from \"TODO\"       [2025-11-15 Sat]
- State \"DONE\"       from \"TODO\"       [2025-09-15 Mon]
- State \"DONE\"       from \"TODO\"       [2025-08-16 Sat]
- State \"DONE\"       from \"TODO\"       [2025-07-16 Wed]"
          (should
           (string-equal
            "\nhabit                     !       \n"
            (progn
              (org-agenda nil "f")
              (buffer-string)))))))))

(ert-deftest test-org-habit/org-extend-today-until ()
  "Test habit graph with `org-extend-today-until' set."
  :expected-result :failed
  (org-test-at-time "2009-10-20"
    (let ((org-agenda-custom-commands
           org-test-habit-no-fluff-agenda)
          (org-habit-preceding-days 5)
          (org-habit-following-days 5)
          (org-habit-graph-column 5)
          (org-habit-show-all-today t))
      (dolist (org-extend-today-until '(0 1 2))
        (org-test-agenda-with-agenda
            "* TODO habit
SCHEDULED: <2009-10-20 Sat ++1d>
:PROPERTIES:
:STYLE:    habit
:END:
- State \"DONE\"       from \"TODO\"       [2009-10-19 Sun 00:20]
- State \"DONE\"       from \"TODO\"       [2009-10-17 Sun 01:20]"
          (should
           (string-equal
            (cl-case org-extend-today-until
              (0 "\nhabit  * *!     \n")
              ;; Current behavior is:
              ;; (t "\nhabit   * *     \n")
              (1 "\nhabit  ** !   \n")
              (2 "\nhabit * * !   \n"))
            (progn
              (org-agenda nil "f")
              (buffer-string)))))))))

(ert-deftest test-org-habit/dst ()
  "Test the habit graph traversing a daylight savings time transition."
  :expected-result :failed
  (org-test-with-timezone "America/New_York"
    ;; DST transition (spring forward) [2009-03-08 01:59] -> [2009-03-08 03:00]
    (org-test-at-time "2009-03-05"
      (let ((org-agenda-custom-commands
             org-test-habit-no-fluff-agenda)
            (org-habit-preceding-days 5)
            (org-habit-following-days 5)
            (org-habit-graph-column 5))
        (org-test-agenda-with-agenda
            "* TODO habit
SCHEDULED: <2009-03-05 Sat ++1d>
:PROPERTIES:
:STYLE:    habit
:END:
- State \"DONE\"       from \"TODO\"       [2009-03-04 Wed]
- State \"DONE\"       from \"TODO\"       [2009-03-02 Mon]"
          (should
           (string-equal
            "\nhabit  * *!     \n"
            (progn
              (org-agenda nil "f")
              (buffer-string)))))))
    ;; DST transition (fall back) [2009-11-01 01:59] -> [2009-11-01 01:00]
    (org-test-at-time "2009-10-28"
      (let ((org-agenda-custom-commands
             org-test-habit-no-fluff-agenda)
            (org-habit-preceding-days 5)
            (org-habit-following-days 5)
            (org-habit-graph-column 5))
        (org-test-agenda-with-agenda
            "* TODO habit
SCHEDULED: <2009-10-28 Sat ++1d>
:PROPERTIES:
:STYLE:    habit
:END:
- State \"DONE\"       from \"TODO\"       [2009-10-27 Sun]
- State \"DONE\"       from \"TODO\"       [2009-10-25 Sun]"
          (should
           (string-equal
            ;; we lost a day in the transition! actual result:
            ;; "\nhabit  * *!    \n"
            "\nhabit  * *!     \n"
            (progn
              (org-agenda nil "f")
              (buffer-string)))))))))

(ert-deftest test-org-habit/habit ()
  "Test the agenda view for a habit."
  (org-test-at-time "2009-10-17"
    (org-test-habit
     (should
      (string-equal
       "\nShave                                      *  * *     * *  * !       \n"
       (progn
         (org-agenda nil "f")
         (buffer-string)))))))

(ert-deftest test-org-habit/graph-column ()
  "Test how modifying `org-habit-graph-column' affects habits in the agenda."
  (org-test-at-time "2009-10-17"
    (org-test-habit
     (dolist (org-habit-graph-column '(0 1 2 3 10 20 40 100))
       (should
        (string-equal
         (cl-case org-habit-graph-column
           (0 "\n   *  * *     * *  * !       \n")
           (1 "\nS   *  * *     * *  * !       \n")
           (2 "\nSh   *  * *     * *  * !       \n")
           (3 "\nSha   *  * *     * *  * !       \n")
           ((10 20 40 100) (concat "\nShave"
                                   (make-string (- org-habit-graph-column 2) 32)
                                   "*  * *     * *  * !       \n"))
           (t (cl-assert nil nil "Missing case!")))
         (progn
           (org-agenda nil "f")
           (buffer-string))))))))

(ert-deftest test-org-habit/preceding-days ()
  "Test how modifying `org-habit-preceding-days' affects habits in the agenda."
  (org-test-at-time "2009-10-17"
    (org-test-habit
     (dolist (org-habit-preceding-days '(0 1 2 3 10 20 40 100))
       (should
        (string-equal
         (cl-case org-habit-preceding-days
           (0 " !       \n")
           (1 "  !       \n")
           (2 " * !       \n")
           (3 "  * !       \n")
           (10 "    * *  * !       \n")
           (20 "   *  * *     * *  * !       \n")
           ((40 100) (concat (make-string (- org-habit-preceding-days 34) 32)
                             "*   *  *     *   *  * *     * *  * !       \n"))
           (t (cl-assert nil nil "Missing case!")))
         (progn
           (org-agenda nil "f")
           (buffer-substring (+ 1 org-habit-graph-column) (point-max)))))))))

(ert-deftest test-org-habit/following-days ()
  "Test how modifying `org-habit-following-days' affects habits in the agenda."
  ;; Avoid DST when TZ="America/New_York".  See `test-org-habit/dst'.
  (org-test-with-timezone "UTC0"
    (org-test-at-time "2009-10-17"
      (org-test-habit
       (dolist (org-habit-following-days '(0 1 2 3 10 20 40 100))
         (should
          (string-equal
           (cl-case org-habit-following-days
             (0   "    *  * *     * *  *  \n")
             ((1 2 3 10 20 40 100)
              (concat "    *  * *     * *  * !"
                      (make-string org-habit-following-days 32)
                      "\n"))
             (t (cl-assert nil nil "Missing case!")))
           (progn
             (org-agenda nil "f")
             (buffer-substring (+ 1 org-habit-graph-column) (point-max))))))))))

(ert-deftest test-org-habit/show-habits ()
  "Test displaying habits in the agenda at various points in time.
Also test modifying the variables `org-habit-show-habits',
`org-habit-show-habits-only-for-today', and `org-habit-show-all-today'."
  ;; Avoid DST when TZ="Europe/Istanbul".  See `test-org-habit/dst'.
  (org-test-with-timezone
   "UTC0"
   (org-test-habit
    (dolist (org-habit-show-habits '(nil t))
      (dolist (org-habit-show-habits-only-for-today '(nil t))
        (dolist (org-habit-show-all-today '(nil t))
          (dolist (test-data
                   '((2009-10-15 . " *   *  * *     * *  *       \n")
                     (2009-10-16 . "*   *  * *     * *  *!       \n")
                     (2009-10-17 . "   *  * *     * *  * !       \n")
                     (2009-10-18 . "  *  * *     * *  *  !       \n")
                     (2009-10-19 . " *  * *     * *  *   !       \n")
                     (2009-10-20 . "*  * *     * *  *    !       \n")
                     (2009-10-21 . "  * *     * *  *     !       \n")
                     (2009-10-22 . " * *     * *  *      !       \n")))
            (pcase-let ((`(,test-time . ,expected-output-string) test-data))
              (org-test-at-time (symbol-name test-time)
                (should
                 (string-equal
                  (if org-habit-show-habits
                      (cl-case test-time
                        ((2009-10-15 2009-10-16)
                         (if org-habit-show-all-today
                             expected-output-string
                           ""))
                        ((2009-10-17 2009-10-18 2009-10-19 2009-10-20 2009-10-21 2009-10-22)
                         expected-output-string)
                        (t (cl-assert nil t "Missing case for: %S!" (symbol-name test-time))))
                    "")
                  (progn
                    (org-agenda nil "f")
                    (let ((result (buffer-string)))
                      (if (string-empty-p result)
                          result
                        (substring result (+ 1 org-habit-graph-column))))))))))))))))

(ert-deftest test-org-habit/toggle-display-in-agenda ()
  "Test the agenda view for a habit."
  (let ((org-agenda-custom-commands
         '(("f" "no fluff" agenda ""
            ;; This differs from `org-test-habit-no-fluff-agenda' by
            ;; adding this header.  Without this we have cases where
            ;; the agenda buffer is completly empty and that causes
            ;; funny things to happen
            ((org-agenda-overriding-header "h")
             (org-agenda-format-date "")
             (org-agenda-span 'day)
             (org-agenda-show-all-dates nil)
             (org-agenda-todo-keyword-format "")
             (org-agenda-prefix-format "")))))
        (org-habit-graph-column 7)
        (org-habit-following-days 1)
        (org-habit-preceding-days 5))
    ;; (test-time . expected-string)
    (dolist (test-data '(("2009-10-15" . "h\n\nShave  * *  * \n")
                         ("2009-10-17" . "h\n\nShave  *  * ! \n")))
      (org-test-at-time (car test-data)
        (org-test-agenda-with-agenda
            (org-test-habit-agenda-string "++" nil)
          (org-agenda nil "f")
          (should
           (string-equal
            (if (string-equal (car test-data) "2009-10-17")
                (cdr test-data)
              "h\n")
            (buffer-string)))
          (org-habit-toggle-display-in-agenda nil)
          (should
           (string-equal
            "h\n"
            (buffer-string)))
          (org-habit-toggle-display-in-agenda nil)
          (should
           (string-equal
            (if (string-equal (car test-data) "2009-10-17")
                (cdr test-data)
              "h\n")
            (buffer-string)))
          (org-habit-toggle-display-in-agenda t)
          (should
           (string-equal
            (cdr test-data)
            (buffer-string))))))))

;;; Bad habits

(ert-deftest test-org-habit/bad-habit-no-repeater ()
  "Test a habit without a repeater."
  (org-test-agenda-with-agenda
      "* TODO no repeater
SCHEDULED: <2009-10-17 Sat>
:PROPERTIES:
:STYLE:    habit
:END:"
    (should-error
     (org-agenda nil "a"))))

(ert-deftest test-org-habit/bad-habit-bad-repeater ()
  "Test a habit with a bad repeater."
  (dolist (repeater '("7d" "12w" "3m" ".+7" "+7" "++2"))
    (org-test-agenda-with-agenda
        (concat "* TODO bad repeater
SCHEDULED: <2026-01-21 Wed " repeater ">
:PROPERTIES:
:STYLE:    habit
:END:")
      (should-error
       (org-agenda nil "a")))))

(ert-deftest test-org-habit/bad-habit-short-repeater ()
  "Test a habit with a period of less then 1 day."
  (dolist (repeater '("+0d" "+0w" "+1h" "+23h"))
    (org-test-agenda-with-agenda
        (concat
         "* TODO repeat period less then 1 day
SCHEDULED: <2009-10-17 Sat " repeater ">
:PROPERTIES:
:STYLE:    habit
:END:")
      (should-error
       (org-agenda nil "a")))))

(ert-deftest test-org-habit/bad-habit-no-scheduled ()
  "Test a habit that is not scheduled."
  (org-test-agenda-with-agenda
      "* TODO no scheduled <2009-10-17 Sat +1d>
:PROPERTIES:
:STYLE:    habit
:END:"
    (should-error
     (org-agenda nil "a")))

  (org-test-agenda-with-agenda
      "* TODO no scheduled <2026-06-17 Wed 11:59 +1d>
:PROPERTIES:
:STYLE:    habit
:END:"
    (should-error
     (org-agenda nil "a"))))

(ert-deftest test-org-habit/bad-habit-deadline-less-scheduled ()
  "Test a habit where the deadline is less then or equal to the scheduled."
  (dolist (deadline '("1d" "2d"))
    (org-test-agenda-with-agenda
        (concat
         "* TODO deadline < or = to scheduled
SCHEDULED: <2009-10-17 Sat +2d/" deadline ">
:PROPERTIES:
:STYLE:    habit
:END:")
      (should-error
       (org-agenda nil "a")))))


(provide 'test-org-habit)

;;; test-org-habit.el ends here
