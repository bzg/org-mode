(defun sacha/org-show-load ()
  "Show my unscheduled time and free time for the day."
  (interactive)
  (let ((time (sacha/org-calculate-free-time
               ;; today
               (calendar-gregorian-from-absolute (time-to-days (current-time)))
               ;; now
               (let* ((now (decode-time))
                      (cur-hour (nth 2 now))
                      (cur-min (nth 1 now)))
                 (+ (* cur-hour 60) cur-min))
               ;; until the last time in my time grid
               (let ((last (car (last (elt org-agenda-time-grid 2)))))
                 (+ (* (/ last 100) 60) (% last 100))))))
    (message "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
            (/ (car time) (* .01 (cdr time)))
            (car time)
            (cdr time)
            (- (cdr time) (car time)))))

(defun sacha/org-agenda-load (match)
  "Can be included in `org-agenda-custom-commands'."
  (let ((inhibit-read-only t)
        (time (sacha/org-calculate-free-time
               ;; today
               (calendar-gregorian-from-absolute org-starting-day)
               ;; now if today, else start of day
               (if (= org-starting-day
                      (time-to-days (current-time)))
                   (let* ((now (decode-time))
                          (cur-hour (nth 2 now))
                          (cur-min (nth 1 now)))
                     (+ (* cur-hour 60) cur-min))
                 (let ((start (car (elt org-agenda-time-grid 2))))
                   (+ (* (/ start 100) 60) (% start 100))))
                 ;; until the last time in my time grid
               (let ((last (car (last (elt org-agenda-time-grid 2)))))
                 (+ (* (/ last 100) 60) (% last 100))))))
    (goto-char (point-max))
    (insert (format
             "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
             (/ (car time) (* .01 (cdr time)))
             (car time)
             (cdr time)
             (- (cdr time) (car time))))))

(defun sacha/org-calculate-free-time (date start-time end-of-day)
  "Return a cons cell of the form (TASK-TIME . FREE-TIME) for DATE, given START-TIME and END-OF-DAY.
DATE is a list of the form (MONTH DAY YEAR).
START-TIME and END-OF-DAY are the number of minutes past midnight."
  (save-window-excursion
  (let ((files org-agenda-files)
        (total-unscheduled 0)
        (total-gap 0)
        file
        rtn
        rtnall
        entry
        (last-timestamp start-time)
        scheduled-entries)
    (while (setq file (car files))
      (catch 'nextfile
        (org-check-agenda-file file)
        (setq rtn (org-agenda-get-day-entries file date :scheduled :timestamp))
        (setq rtnall (append rtnall rtn)))
      (setq files (cdr files)))
    ;; For each item on the list
    (while (setq entry (car rtnall))
      (let ((time (get-text-property 1 'time entry)))
        (cond
         ((and time (string-match "\\([^-]+\\)-\\([^-]+\\)" time))
          (setq scheduled-entries (cons (cons
                                         (save-match-data (appt-convert-time (match-string 1 time)))
                                         (save-match-data (appt-convert-time (match-string 2 time))))
                                        scheduled-entries)))
         ((and time
               (string-match "\\([^-]+\\)\\.+" time)
               (string-match "^[A-Z]+ \\(\\[#[A-Z]\\]\\)? \\([0-9]+\\)" (get-text-property 1 'txt entry)))
          (setq scheduled-entries
                (let ((start (and (string-match "\\([^-]+\\)\\.+" time)
                                 (appt-convert-time (match-string 1 time)))))
                  (cons (cons start
                              (and (string-match "^[A-Z]+ \\(\\[#[A-Z]\\]\\)? \\([0-9]+\\) " (get-text-property 1 'txt entry))
                                   (+ start (string-to-number (match-string 2 (get-text-property 1 'txt entry))))))
                        scheduled-entries))))
         ((string-match "^[A-Z]+ \\([0-9]+\\)" (get-text-property 1 'txt entry))
          (setq total-unscheduled (+ (string-to-number
                                      (match-string 1 (get-text-property 1 'txt entry)))
                                     total-unscheduled)))))
      (setq rtnall (cdr rtnall)))
    ;; Sort the scheduled entries by time
    (setq scheduled-entries (sort scheduled-entries (lambda (a b) (< (car a) (car b)))))

    (while scheduled-entries
      (let ((start (car (car scheduled-entries)))
            (end (cdr (car scheduled-entries))))
      (cond
       ;; are we in the middle of this timeslot?
       ((and (>= last-timestamp start)
             (< = last-timestamp end))
        ;; move timestamp later, no change to time
        (setq last-timestamp end))
       ;; are we completely before this timeslot?
       ((< last-timestamp start)
        ;; add gap to total, skip to the end
        (setq total-gap (+ (- start last-timestamp) total-gap))
        (setq last-timestamp end)))
      (setq scheduled-entries (cdr scheduled-entries))))
    (if (< last-timestamp end-of-day)
        (setq total-gap (+ (- end-of-day last-timestamp) total-gap)))
    (cons total-unscheduled total-gap))))
