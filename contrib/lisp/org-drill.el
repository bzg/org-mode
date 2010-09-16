;;; org-drill.el - Self-testing with org-learn
;;;
;;; Author: Paul Sexton <eeeickythump@gmail.com>
;;; Version: 1.0
;;; Repository at http://bitbucket.org/eeeickythump/org-drill/
;;;
;;;
;;; Synopsis
;;; ========
;;;
;;; Uses the spaced repetition algorithm in `org-learn' to conduct interactive
;;; "drill sessions", where the material to be remembered is presented to the
;;; student in random order. The student rates his or her recall of each item,
;;; and this information is fed back to `org-learn' to schedule the item for
;;; later revision.
;;;
;;; Each drill session can be restricted to topics in the current buffer
;;; (default), one or several files, all agenda files, or a subtree. A single
;;; topic can also be drilled.
;;;
;;; Different "card types" can be defined, which present their information to
;;; the student in different ways.
;;;
;;; See the file README.org for more detailed documentation.


(eval-when-compile (require 'cl))
(eval-when-compile (require 'hi-lock))
(require 'org)
(require 'org-learn)


(defgroup org-drill nil
  "Options concerning interactive drill sessions in Org mode (org-drill)."
  :tag "Org-Drill"
  :group 'org-link)



(defcustom org-drill-question-tag 
  "drill"
  "Tag which topics must possess in order to be identified as review topics
by `org-drill'."
  :group 'org-drill
  :type 'string)



(defcustom org-drill-maximum-items-per-session
  30
  "Each drill session will present at most this many topics for review.
Nil means unlimited."
  :group 'org-drill
  :type '(choice integer (const nil)))



(defcustom org-drill-maximum-duration
  20
  "Maximum duration of a drill session, in minutes.
Nil means unlimited."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-failure-quality
  2
  "If the quality of recall for an item is this number or lower,
it is regarded as an unambiguous failure, and the repetition
interval for the card is reset to 0 days.  By default this is
2. For Mnemosyne-like behaviour, set it to 1.  Other values are
not really sensible."
  :group 'org-drill
  :type '(choice (const 2) (const 1)))


(defcustom org-drill-leech-failure-threshold
  15
  "If an item is forgotten more than this many times, it is tagged
as a 'leech' item."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-leech-method
  'skip
  "How should 'leech items' be handled during drill sessions?
Possible values:
- nil :: Leech items are treated the same as normal items.
- skip :: Leech items are not included in drill sessions.
- warn :: Leech items are still included in drill sessions,
  but a warning message is printed when each leech item is
  presented."
  :group 'org-drill
  :type '(choice (const 'warn) (const 'skip) (const nil)))


(defface org-drill-visible-cloze-face
  '((t (:foreground "dark slate blue")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defcustom org-drill-use-visible-cloze-face-p
  nil
  "Use a special face to highlight cloze-deleted text in org mode
buffers?"
  :group 'org-drill
  :type 'boolean)


(defface org-drill-hidden-cloze-face
  '((t (:foreground "deep sky blue" :background "blue")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(setplist 'org-drill-cloze-overlay-defaults
          '(display "[...]"
                    face org-drill-hidden-cloze-face
                    window t))


(defvar org-drill-cloze-regexp
  ;; ver 1   "[^][]\\(\\[[^][][^]]*\\]\\)"
  ;; ver 2   "\\(\\[.*?\\]\\|^[^[[:cntrl:]]*?\\]\\|\\[.*?$\\)"
  "\\(\\[.*?\\]\\|\\[.*?[[:cntrl:]]+.*?\\]\\)")


(defcustom org-drill-card-type-alist
  '((nil . org-drill-present-simple-card)
    ("simple" . org-drill-present-simple-card)
    ("twosided" . org-drill-present-two-sided-card)
    ("multisided" . org-drill-present-multi-sided-card)
    ("spanish_verb" . org-drill-present-spanish-verb))
  "Alist associating card types with presentation functions. Each entry in the
alist takes the form (CARDTYPE . FUNCTION), where CARDTYPE is a string
or nil, and FUNCTION is a function which takes no arguments and returns a
boolean value."
  :group 'org-drill
  :type '(alist :key-type (choice string (const nil)) :value-type function))


(defcustom org-drill-spaced-repetition-algorithm
  'sm5
  "Which SuperMemo spaced repetition algorithm to use for scheduling items.
Available choices are SM2 and SM5."
  :group 'org-drill
  :type '(choice (const 'sm2) (const 'sm5)))

(defcustom org-drill-add-random-noise-to-intervals-p
  nil
  "If true, the number of days until an item's next repetition
will vary slightly from the interval calculated by the SM2
algorithm. The variation is very small when the interval is
small, and scales up with the interval. The code for calculating
random noise is adapted from Mnemosyne."
  :group 'org-drill
  :type 'boolean)


(defvar *org-drill-done-entry-count* 0)
(defvar *org-drill-pending-entry-count* 0)
(defvar *org-drill-session-qualities* nil)
(defvar *org-drill-start-time* 0)


(defun shuffle-list (list)
  "Randomly permute the elements of LIST (all permutations equally likely)."
  ;; Adapted from 'shuffle-vector' in cookie1.el
  (let ((i 0)
	j
	temp
	(len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setf (nth i list) (nth j list))
      (setf (nth j list) temp)
      (setq i (1+ i))))
  list)
    


(defun org-drill-entry-p ()
  "Is the current entry a 'drill item'?"
  (or (assoc "LEARN_DATA" (org-entry-properties nil))
      (member org-drill-question-tag (org-get-local-tags))))


(defun org-part-of-drill-entry-p ()
  "Is the current entry either the main heading of a 'drill item',
or a subheading within a drill item?"
  (or (org-drill-entry-p)
      ;; Does this heading INHERIT the drill tag
      (member org-drill-question-tag (org-get-tags-at))))


(defun org-drill-entry-leech-p ()
  "Is the current entry a 'leech item'?"
  (and (org-drill-entry-p)
       (member "leech" (org-get-local-tags))))


(defun org-drill-entry-due-p ()
  (let ((item-time (org-get-scheduled-time (point))))
    (and (org-drill-entry-p)
         (or (not (eql 'skip org-drill-leech-method))
             (not (org-drill-entry-leech-p)))
         (or (null item-time)
             (not (minusp               ; scheduled for today/in future
                   (- (time-to-days (current-time))
                      (time-to-days item-time))))))))


(defun org-drill-entry-new-p ()
  (let ((item-time (org-get-scheduled-time (point))))
    (and (org-drill-entry-p)
         (null item-time))))



(defun org-drill-entry-last-quality ()
  (let ((quality (cdr (assoc "DRILL_LAST_QUALITY" (org-entry-properties nil)))))
    (if quality
        (string-to-number quality)
      nil)))


;;; SM2 Algorithm =============================================================


(defun determine-next-interval-sm2 (last-interval n ef quality of-matrix)
  "Arguments:
- LAST-INTERVAL -- the number of days since the item was last reviewed.
- N -- the number of times the item has been successfully reviewed
- EF -- the 'easiness factor'
- QUALITY -- 0 to 5
- OF-MATRIX -- a matrix of values, used by SM5 but not by SM2.

Returns a list: (INTERVAL N EF OFMATRIX), where:
- INTERVAL is the number of days until the item should next be reviewed
- N is incremented by 1.
- EF is modified based on the recall quality for the item.
- OF-MATRIX is not modified."
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (if (<= quality org-drill-failure-quality)
      ;; When an item is failed, its interval is reset to 0,
      ;; but its EF is unchanged
      (list -1 1 ef of-matrix)
    ;; else:
    (let* ((next-ef (modify-e-factor ef quality))
           (interval
            (cond
             ((<= n 1) 1)
             ((= n 2)
              (cond
               (org-drill-add-random-noise-to-intervals-p
                (case quality
                  (5 6)
                  (4 4)
                  (3 3)
                  (2 1)
                  (t -1)))
               (t 6)))
             (t (ceiling (* last-interval next-ef))))))
      (list (round
             (if org-drill-add-random-noise-to-intervals-p
                 (+ last-interval (* (- interval last-interval)
                                     (org-drill-random-dispersal-factor)))
               interval))
            (1+ n) next-ef of-matrix))))


;;; SM5 Algorithm =============================================================

;;; From http://www.supermemo.com/english/ol/sm5.htm
(defun org-drill-random-dispersal-factor ()
  (let ((a 0.047)
        (b 0.092)
        (p (- (random* 1.0) 0.5)))
    (flet ((sign (n)
                 (cond ((zerop n) 0)
                       ((plusp n) 1)
                       (t -1))))
      (/ (+ 100 (* (* (/ -1 b) (log (- 1 (* (/ b a ) (abs p)))))
                   (sign p)))
         100))))
      

(defun inter-repetition-interval-sm5 (last-interval n ef &optional of-matrix)
  (let ((of (get-optimal-factor n ef of-matrix)))
    (if (= 1 n)
	of
      (* of last-interval))))


(defun determine-next-interval-sm5 (last-interval n ef quality of-matrix)
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (let ((next-ef (modify-e-factor ef quality))
        (interval nil))
    (setq of-matrix
          (set-optimal-factor n next-ef of-matrix
                              (modify-of (get-optimal-factor n ef of-matrix)
                                         quality org-learn-fraction))
          ef next-ef)
    
    (cond
     ;; "Failed" -- reset repetitions to 0, 
     ((<= quality org-drill-failure-quality)
      (list -1 1 ef of-matrix))      ; Not clear if OF matrix is supposed to be
                                     ; preserved
     ;; For a zero-based quality of 4 or 5, don't repeat
     ((and (>= quality 4)
           (not org-learn-always-reschedule))
      (list 0 (1+ n) ef of-matrix))     ; 0 interval = unschedule
     (t
      (setq interval (inter-repetition-interval-sm5
                      last-interval n ef of-matrix))
      (if org-drill-add-random-noise-to-intervals-p
          (setq interval (+ last-interval
                            (* (- interval last-interval)
                               (org-drill-random-dispersal-factor)))))
      (list (round interval) (1+ n) ef of-matrix)))))


;;; Essentially copied from `org-learn.el', but modified to
;;; optionally call the SM2 function above.
(defun org-drill-smart-reschedule (quality)
  (interactive "nHow well did you remember the information (on a scale of 0-5)? ")
  (let* ((learn-str (org-entry-get (point) "LEARN_DATA"))
	 (learn-data (or (and learn-str
			      (read learn-str))
			 (copy-list initial-repetition-state)))
	 closed-dates)
    (setq learn-data
          (case org-drill-spaced-repetition-algorithm
            (sm5 (determine-next-interval-sm5 (nth 0 learn-data)
                                              (nth 1 learn-data)
                                              (nth 2 learn-data)
                                              quality
                                              (nth 3 learn-data)))
            (sm2 (determine-next-interval-sm2 (nth 0 learn-data)
                                              (nth 1 learn-data)
                                              (nth 2 learn-data)
                                              quality
                                              (nth 3 learn-data)))))
    (org-entry-put (point) "LEARN_DATA" (prin1-to-string learn-data))
    (cond
     ((= 0 (nth 0 learn-data))
      (org-schedule t))
     (t
      (org-schedule nil (time-add (current-time)
				  (days-to-time (nth 0 learn-data))))))))


(defun org-drill-reschedule ()
  "Returns quality rating (0-5), or nil if the user quit."
  (let ((ch nil))
    (while (not (memq ch '(?q ?0 ?1 ?2 ?3 ?4 ?5)))
      (setq ch (read-char
                (if (eq ch ??)
                    "0-2 Means you have forgotten the item.
3-5 Means you have remembered the item.
 
0 - Completely forgot. 
1 - Even after seeing the answer, it still took a bit to sink in. 
2 - After seeing the answer, you remembered it. 
3 - It took you awhile, but you finally remembered.
4 - After a little bit of thought you remembered.
5 - You remembered the item really easily.

How well did you do? (0-5, ?=help, q=quit)"
                  "How well did you do? (0-5, ?=help, q=quit)"))))
    (cond
     ((and (>= ch ?0) (<= ch ?5))
      (let ((quality (- ch ?0))
            (failures (cdr (assoc "DRILL_FAILURE_COUNT" (org-entry-properties nil)))))
        (save-excursion
          (org-drill-smart-reschedule quality))
        (push quality *org-drill-session-qualities*)
        (cond
         ((<= quality org-drill-failure-quality)
          (when org-drill-leech-failure-threshold
            (setq failures (if failures (string-to-number failures) 0))
            (org-set-property "DRILL_FAILURE_COUNT"
                              (format "%d" (1+ failures)))
            (if (> (1+ failures) org-drill-leech-failure-threshold)
                (org-toggle-tag "leech" 'on)))))
        (org-set-property "DRILL_LAST_QUALITY" (format "%d" quality))
        quality))
     (t
      nil))))


(defun org-drill-hide-all-subheadings-except (heading-list)
  "Returns a list containing the position of each immediate subheading of
the current topic."
  (let ((drill-entry-level (org-current-level))
        (drill-sections nil)
        (drill-heading nil))
    (org-show-subtree)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) (1+ drill-entry-level))
           (setq drill-heading (org-get-heading t))
           (unless (member drill-heading heading-list)
             (hide-subtree))
           (push (point) drill-sections)))
       "" 'tree))
    (reverse drill-sections)))



(defun org-drill-presentation-prompt (&rest fmt-and-args)
  (let ((ch nil)
        (prompt
         (if fmt-and-args
             (apply 'format
                    (first fmt-and-args)
                    (rest fmt-and-args))
           "Press any key to see the answer, 'e' to edit, 'q' to quit.")))
    (setq prompt
          (format "(%d) %s" *org-drill-pending-entry-count* prompt))
    (if (and (eql 'warn org-drill-leech-method)
             (org-drill-entry-leech-p))
        (setq prompt (concat "!!! LEECH ITEM !!!
You seem to be having a lot of trouble memorising this item.
Consider reformulating the item to make it easier to remember.\n" prompt)))
    (setq ch (read-char prompt))
    (case ch
      (?q nil)
      (?e 'edit)
      (otherwise t))))


(defun org-drill-hide-clozed-text ()
  (let ((ovl nil))
    (save-excursion
      (while (re-search-forward org-drill-cloze-regexp nil t)
        (setf ovl (make-overlay (match-beginning 0) (match-end 0)))
        (overlay-put ovl 'category
                     'org-drill-cloze-overlay-defaults)
        (when (find ?| (match-string 0))
          (overlay-put ovl
                       'display
                       (format "[...%s]"
                               (substring-no-properties
                                (match-string 0)
                                (1+ (position ?| (match-string 0)))
                                (1- (length (match-string 0)))))))))))


(defun org-drill-unhide-clozed-text ()
  (save-excursion
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql 'org-drill-cloze-overlay-defaults (overlay-get ovl 'category))
        (delete-overlay ovl)))))



;;; Presentation functions ====================================================

;; Each of these is called with point on topic heading.  Each needs to show the
;; topic in the form of a 'question' or with some information 'hidden', as
;; appropriate for the card type. The user should then be prompted to press a
;; key. The function should then reveal either the 'answer' or the entire
;; topic, and should return t if the user chose to see the answer and rate their
;; recall, nil if they chose to quit.

(defun org-drill-present-simple-card ()
  (org-drill-hide-all-subheadings-except nil)
  (prog1 (org-drill-presentation-prompt)
    (org-show-subtree)))


(defun org-drill-present-two-sided-card ()
  (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
    (when drill-sections
      (save-excursion
        (goto-char (nth (random (min 2 (length drill-sections))) drill-sections))
        (org-show-subtree)))
    (prog1
        (org-drill-presentation-prompt)
      (org-show-subtree))))



(defun org-drill-present-multi-sided-card ()
  (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
    (when drill-sections
      (save-excursion
        (goto-char (nth (random (length drill-sections)) drill-sections))
        (org-show-subtree)))
    (prog1
        (org-drill-presentation-prompt)
      (org-show-subtree))))



(defun org-drill-present-spanish-verb ()
  (case (random 6)
    (0
     (org-drill-hide-all-subheadings-except '("Infinitive"))
     (prog1
         (org-drill-presentation-prompt
          "Translate this Spanish verb, and conjugate it for the *present* tense.")
       (org-drill-hide-all-subheadings-except '("English" "Present Tense"
                                                "Notes"))))
    (1
     (org-drill-hide-all-subheadings-except '("English"))
     (prog1
         (org-drill-presentation-prompt
          "For the *present* tense, conjugate the Spanish translation of this English verb.")
       (org-drill-hide-all-subheadings-except '("Infinitive" "Present Tense"
                                                "Notes"))))
    (2
     (org-drill-hide-all-subheadings-except '("Infinitive"))
     (prog1
         (org-drill-presentation-prompt
          "Translate this Spanish verb, and conjugate it for the *past* tense.")
       (org-drill-hide-all-subheadings-except '("English" "Past Tense"
                                                "Notes"))))
    (3
     (org-drill-hide-all-subheadings-except '("English"))
     (prog1
         (org-drill-presentation-prompt
          "For the *past* tense, conjugate the Spanish translation of this English verb.")
       (org-drill-hide-all-subheadings-except '("Infinitive" "Past Tense"
                                                "Notes"))))
    (4
     (org-drill-hide-all-subheadings-except '("Infinitive"))
     (prog1
         (org-drill-presentation-prompt
          "Translate this Spanish verb, and conjugate it for the *future perfect* tense.")
       (org-drill-hide-all-subheadings-except '("English" "Future Perfect Tense"
                                                "Notes"))))
    (5
     (org-drill-hide-all-subheadings-except '("English"))
     (prog1
         (org-drill-presentation-prompt
          "For the *future perfect* tense, conjugate the Spanish translation of this English verb.")
       (org-drill-hide-all-subheadings-except '("Infinitive" "Future Perfect Tense"
                                                "Notes"))))))
    



(defun org-drill-entry ()
  "Present the current topic for interactive review, as in `org-drill'.
Review will occur regardless of whether the topic is due for review or whether
it meets the definition of a 'review topic' used by `org-drill'.

Returns a quality rating from 0 to 5, or nil if the user quit, or the symbol
EDIT if the user chose to exit the drill and edit the current item.

See `org-drill' for more details."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let ((card-type (cdr (assoc "DRILL_CARD_TYPE" (org-entry-properties nil))))
        (cont nil))
    (save-restriction
      (org-narrow-to-subtree) 
      (org-show-subtree)
      (org-cycle-hide-drawers 'all)
      
      (let ((presentation-fn (cdr (assoc card-type org-drill-card-type-alist))))
        (cond
         (presentation-fn
          (org-drill-hide-clozed-text)
          ;;(highlight-regexp org-drill-cloze-regexp
          ;;                  'org-drill-hidden-cloze-face)
          (unwind-protect
              (progn
                (setq cont (funcall presentation-fn)))
            (org-drill-unhide-clozed-text))
          ;;(unhighlight-regexp org-drill-cloze-regexp)
          )
         (t
          (error "Unknown card type: '%s'" card-type))))
      
      (cond
       ((not cont)
        (message "Quit")
        nil)
       ((eql cont 'edit)
        'edit)
       (t
        (save-excursion
          (org-drill-reschedule)))))))


(defun org-drill-entries (entries)
  "Returns nil, t, or a list of markers representing entries that were
'failed' and need to be presented again before the session ends."
  (let ((again-entries nil)
        (*org-drill-done-entry-count* 0)
        (*org-drill-pending-entry-count* (length entries)))
    (if (and org-drill-maximum-items-per-session
             (> (length entries)
                org-drill-maximum-items-per-session))
        (setq entries (subseq entries 0
                              org-drill-maximum-items-per-session)))
    (block org-drill-entries
      (dolist (m entries)
        (save-restriction
          (switch-to-buffer (marker-buffer m))
          (goto-char (marker-position m))
          (setq result (org-drill-entry))
          (cond
           ((null result)
            (message "Quit")
            (return-from org-drill-entries nil))
           ((eql result 'edit)
            (setq end-pos (point-marker))
            (return-from org-drill-entries nil))
           (t
            (cond
             ((< result 3)
              (push m again-entries))
             (t
              (decf *org-drill-pending-entry-count*)
              (incf *org-drill-done-entry-count*)))
            (when (and org-drill-maximum-duration
                       (> (- (float-time (current-time)) *org-drill-start-time*)
                          (* org-drill-maximum-duration 60)))
              (message "This drill session has reached its maximum duration.")
              (return-from org-drill-entries nil))))))
      (or again-entries
          t))))


(defun org-drill-final-report ()
  (read-char
(format
 "%d items reviewed, %d items awaiting review
Session duration %s

Recall of reviewed items:
 Excellent (5):     %3d%%
 Good (4):          %3d%%
 Hard (3):          %3d%%
 Near miss (2):     %3d%%
 Failure (1):       %3d%%
 Total failure (0): %3d%%

Session finished. Press a key to continue..." 
 *org-drill-done-entry-count*
 *org-drill-pending-entry-count*
 (format-seconds "%h:%.2m:%.2s"
                 (- (float-time (current-time)) *org-drill-start-time*))
 (round (* 100 (count 5 *org-drill-session-qualities*))
        (max 1 (length *org-drill-session-qualities*)))
 (round (* 100 (count 4 *org-drill-session-qualities*))
        (max 1 (length *org-drill-session-qualities*)))
 (round (* 100 (count 3 *org-drill-session-qualities*))
        (max 1 (length *org-drill-session-qualities*)))
 (round (* 100 (count 2 *org-drill-session-qualities*))
        (max 1 (length *org-drill-session-qualities*)))
 (round (* 100 (count 1 *org-drill-session-qualities*))
        (max 1 (length *org-drill-session-qualities*)))
 (round (* 100 (count 0 *org-drill-session-qualities*))
        (max 1 (length *org-drill-session-qualities*)))
 )))



(defun org-drill (&optional scope)
  "Begin an interactive 'drill session'. The user is asked to
review a series of topics (headers). Each topic is initially
presented as a 'question', often with part of the topic content
hidden. The user attempts to recall the hidden information or
answer the question, then presses a key to reveal the answer. The
user then rates his or her recall or performance on that
topic. This rating information is used to reschedule the topic
for future review using the `org-learn' library.

Org-drill proceeds by:

- Finding all topics (headings) in SCOPE which have either been
  used and rescheduled by org-learn before (i.e. the LEARN_DATA
  property is set), or which have a tag that matches
  `org-drill-question-tag'.

- All matching topics which are either unscheduled, or are
  scheduled for the current date or a date in the past, are
  considered to be candidates for the drill session.

- If `org-drill-maximum-items-per-session' is set, a random
  subset of these topics is presented. Otherwise, all of the
  eligible topics will be presented.

SCOPE determines the scope in which to search for
questions.  It is passed to `org-map-entries', and can be any of:

nil     The current buffer, respecting the restriction if any.
        This is the default.
tree    The subtree started with the entry at point
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
 (file1 file2 ...)
        If this is a list, all files in the list will be scanned."

  (interactive)
  (let ((entries nil)
        (failed-entries nil)
        (new-entries nil)
        (old-entries nil)
        (result nil)
        (results nil)
        (end-pos nil))
    (block org-drill
      (setq *org-drill-session-qualities* nil)
      (setq *org-drill-start-time* (float-time (current-time)))
      (save-excursion
        (org-map-entries
         (lambda () (when (org-drill-entry-due-p)
                 (cond
                  ((org-drill-entry-new-p)
                   (push (point-marker) new-entries))
                  ((<= (org-drill-entry-last-quality)
                       org-drill-failure-quality)
                   (push (point-marker) failed-entries))
                  (t
                   (push (point-marker) old-entries)))))
         "" scope)
        ;; Failed first, then random mix of old + new
        (setq entries (append (shuffle-list failed-entries)
                              (shuffle-list (append old-entries
                                                    new-entries))))
        (cond
         ((null entries)
          (message "I did not find any pending drill items."))
         (t
          (let ((again t))
            (while again
              (when (listp again)
                (setq entries (shuffle-list again)))
              (setq again (org-drill-entries entries))
              (cond
               ((null again)
                (return-from org-drill nil))
               ((eql t again)
                (setq again nil))))
            (message "Drill session finished!")
            )))))
    (cond
     (end-pos
      (switch-to-buffer (marker-buffer end-pos))
      (goto-char (marker-position end-pos))
      (message "Edit topic."))
     (t
      (org-drill-final-report)))))



(add-hook 'org-mode-hook
          (lambda ()
            (if org-drill-use-visible-cloze-face-p
                (font-lock-add-keywords
                 'org-mode
                 `((,org-drill-cloze-regexp
                    (0 'org-drill-visible-cloze-face nil)))
                 t)))) 


(provide 'org-drill)
