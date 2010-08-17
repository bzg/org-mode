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
;;;
;;; Installation
;;; ============
;;;
;;; Put the following in your .emacs:
;;;
;;; (add-to-list 'load-path "/path/to/org-drill/")
;;; (require 'org-drill)
;;;
;;;
;;; Writing the questions
;;; =====================
;;;
;;; See the file "spanish.org" for an example set of material.
;;;
;;; Tag all items you want to be asked about with a tag that matches
;;; `org-drill-question-tag'. This is :drill: by default.
;;;
;;; You don't need to schedule the topics initially.  However org-drill *will*
;;; recognise items that have been scheduled previously with `org-learn'.
;;;
;;; Within each question, the answer can be included in the following ways:
;;; 
;;; - Question in the main body text, answer in subtopics. This is the
;;;   default. All subtopics will be shown collapsed, while the text under
;;;   the main heading will stay visible.
;;;
;;; - Each subtopic contains a piece of information related to the topic. ONE
;;;   of these will revealed at random, and the others hidden. To define a
;;;   topic of this type, give the topic a property `DRILL_CARD_TYPE' with
;;;   value `multisided'.
;;;
;;; - Cloze deletion -- any pieces of text in the body of the card that are
;;;   surrounded with [SINGLE square brackets] will be hidden when the card is
;;;   presented to the user, and revealed once they press a key. Cloze deletion
;;;   is automatically applied to all topics.
;;; 
;;; - No explicit answer -- the user judges whether they recalled the
;;;   fact adequately.
;;;
;;; - Other methods of your own devising, provided you write a function to
;;;   handle selective display of the topic. See the function
;;;   `org-drill-present-spanish-verb', which handles topics of type "spanish_verb",
;;;   for an example.
;;;
;;;
;;; Running the drill session
;;; =========================
;;;
;;; Start a drill session with `M-x org-drill'. This will include all eligible
;;; topics in the current buffer. `org-drill' can also be targeted at a particular
;;; subtree or particular files or sets of files; see the documentation of 
;;; the function `org-drill' for details.
;;;
;;; During the drill session, you will be presented with each item, then asked
;;; to rate your recall of it by pressing a key between 0 and 5. At any time you
;;; can press 'q' to finish the drill early (your progress will be saved), or
;;; 'e' to finish the drill and jump to the current topic for editing.
;;;
;;; 
;;; TODO
;;; ====
;;;
;;; - encourage org-learn to reschedule "4" and "5" items.
;;; - nicer "cloze face" which does not hide the space preceding the cloze,
;;;   and behaves more nicely across line breaks
;;; - hide drawers.
;;; - org-drill-question-tag should use a tag match string, rather than a
;;;   single tag
;;; - when finished, display a message showing how many items reviewed,
;;;   how many still pending, numbers in each recall category

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



(defface org-drill-hidden-cloze-face
  '((t (:foreground "blue" :background "blue")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defvar org-drill-cloze-regexp
  "[^][]\\(\\[[^][][^]]*\\]\\)")


(defcustom org-drill-card-type-alist
  '((nil . org-drill-present-simple-card)
    ("simple" . org-drill-present-simple-card)
    ("multisided" . org-drill-present-multi-sided-card)
    ("spanish_verb" . org-drill-present-spanish-verb))
  "Alist associating card types with presentation functions. Each entry in the
alist takes the form (CARDTYPE . FUNCTION), where CARDTYPE is a string
or nil, and FUNCTION is a function which takes no arguments and returns a
boolean value."
  :group 'org-drill
  :type '(alist :key-type (choice string (const nil)) :value-type function))



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
    


(defun org-drill-entry-due-p ()
  (let ((item-time (org-get-scheduled-time (point))))
    (and (or (assoc "LEARN_DATA" (org-entry-properties nil))
             (member org-drill-question-tag (org-get-local-tags)))
         (or (null item-time)
             (not (minusp               ; scheduled for today/in
                                        ; future
                   (- (time-to-days (current-time))
                      (time-to-days item-time))))))))



(defun org-drill-reschedule ()
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
      (save-excursion
        (org-smart-reschedule (- ch 48)))
      ch)
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
  (let ((ch (read-char (if fmt-and-args
                           (apply 'format
                                  (first fmt-and-args)
                                  (rest fmt-and-args))
                         "Press any key to see the answer, 'e' to edit, 'q' to quit."))))
    (case ch
      (?q nil)
      (?e 'edit)
      (otherwise t))))


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

See `org-drill' for more details."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let ((card-type (cdr (assoc "DRILL_CARD_TYPE" (org-entry-properties nil))))
        (cont nil))
    (save-restriction
      (org-narrow-to-subtree) 
      (org-show-subtree)
      (org-cycle-hide-drawers 'overview)
      
      (let ((presentation-fn (cdr (assoc card-type org-drill-card-type-alist))))
        (cond
         (presentation-fn
          (highlight-regexp org-drill-cloze-regexp
                            'org-drill-hidden-cloze-face)
          (setq cont (funcall presentation-fn))
          (unhighlight-regexp org-drill-cloze-regexp))
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
        (result nil)
        (results nil)
        (end-pos nil))
    (block org-drill
      (save-excursion
        (org-map-entries
         (lambda () (if (org-drill-entry-due-p)
                   (push (point-marker) entries)))
         "" scope)
        (cond
         ((null entries)
          (message "I did not find any pending drill items."))
         (t
          (let ((start-time (float-time (current-time))))
            (dolist (m (if (and org-drill-maximum-items-per-session
                                (> (length entries)
                                   org-drill-maximum-items-per-session))
                           (subseq (shuffle-list entries) 0
                                   org-drill-maximum-items-per-session)
                         (shuffle-list entries)))
              (save-restriction
                (switch-to-buffer (marker-buffer m))
                (goto-char (marker-position m))
                (setq result (org-drill-entry))
                (cond
                 ((null result)
                  (message "Quit")
                  (return-from org-drill nil))
                 ((eql result 'edit)
                  (setq end-pos (point-marker))
                  (return-from org-drill nil))
                 ((and org-drill-maximum-duration
                       (> (- (float-time (current-time)) start-time)
                          (* org-drill-maximum-duration 60)))
                  (message "This drill session has reached its maximum duration.")
                  (return-from org-drill nil)))))
            (message "Drill session finished!")
            )))))
    (when end-pos
      (switch-to-buffer (marker-buffer end-pos))
      (goto-char (marker-position end-pos))
      (message "Edit topic."))))



(provide 'org-drill)
