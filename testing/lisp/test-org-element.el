;;; test-org-element.el --- Tests for org-element.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2015, 2019  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

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

;;; Code:

(eval-and-compile (require 'cl-lib))

(require 'org-element)
(require 'org)
(require 'org-inlinetask)

(defun org-test-parse-and-interpret (text)
  "Parse TEXT as Org syntax and interpret it.
Return interpreted string."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (org-element-interpret-data (org-element-parse-buffer))))


;;; Test getters.

(ert-deftest test-org-element/type ()
  "Test `org-element-type' specifications."
  (should (eq 'plain-text (org-element-type "string")))
  (should-not (org-element-type nil))
  (should-not (org-element-type 1))
  (should (eq 'dummy (org-element-type '(dummy))))
  (should (eq 'dummy (org-element-type '(dummy nil 'foo))))
  (should (eq 'dummy (org-element-type '(dummy (:a a :b b) 'foo))))
  ;; anonymous node.
  (should-not (org-element-type '((dummy))))
  (should (eq 'anonymous (org-element-type '((dummy)) t)))
  (should (eq 'anonymous (org-element-type '("string") t)))
  (should-not (org-element-type '(1 2) t)))

(ert-deftest test-org-element/type-p ()
  "Test `org-element-type-p' specifications."
  (should (org-element-type-p '(foo) 'foo))
  (should (org-element-type-p '(foo) '(foo)))
  (should (org-element-type-p '(foo) '(foo bar)))
  (should-not (org-element-type-p '(foo) 'bar))
  (should-not (org-element-type-p '(foo) '(bar baz)))
  (should (org-element-type-p "string" 'plain-text))
  (should (org-element-type-p '((foo)) 'anonymous)))

(ert-deftest test-org-element/org-element-property-raw ()
  "Test `org-element-property-raw' specifications."
  ;; No properties.
  (dolist (element `( nil
                      (headline nil)
                      (headline nil (headline))
                      "string"))
    (should-not (org-element-property-raw :begin element))
    (should (eq 'default (org-element-property-raw :begin element 'default)))
    (should-not (org-element-property-raw :begin1 element))
    (should (eq 'default (org-element-property-raw :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should-not (org-element-property-raw prop element))
      (should (eq 'default (org-element-property-raw prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-raw prop element))
      (should (eq 'default (org-element-property-raw prop element 'default)))))
  ;; Only non-standard properties.
  (dolist (element `((headline (:begin1 1))
                     (headline (:begin1 1) (headline))
                     ,(propertize "string" :begin1 1)))
    (should-not (org-element-property-raw :begin element))
    (should (eq 'default (org-element-property-raw :begin element 'default)))
    (should (= 1 (org-element-property-raw :begin1 element)))
    (should (= 1 (org-element-property-raw :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should-not (org-element-property-raw prop element))
      (should (eq 'default (org-element-property-raw prop element 'default))))
    (dolist (prop '(:begin1))
      (should (= 1 (org-element-property-raw prop element)))
      (should (= 1 (org-element-property-raw prop element 'default)))))
  ;; Only standard properties.
  (dolist (element `((headline (:standard-properties ,(make-vector 10 'test)))
                     (headline (:standard-properties ,(make-vector 10 'test)) (headline))))
    (should (eq 'test (org-element-property-raw :begin element)))
    (should (eq 'test (org-element-property-raw :begin element 'default)))
    (should-not (org-element-property-raw :begin1 element))
    (should (eq 'default (org-element-property-raw :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (eq 'test (org-element-property-raw prop element)))
      (should (eq 'test (org-element-property-raw prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-raw prop element))
      (should (eq 'default (org-element-property-raw prop element 'default)))))
  ;; Standard properties in the plist.
  (dolist (element `((headline (:begin 1))
                     (headline (:begin 1) (headline))
                     ,(propertize "string" :begin 1)))
    (should (= 1 (org-element-property-raw :begin element)))
    (should (= 1 (org-element-property-raw :begin element 'default)))
    (should-not (org-element-property-raw :begin1 element))
    (should (eq 'default (org-element-property-raw :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (= 1 (org-element-property-raw prop element)))
      (should (= 1 (org-element-property-raw prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-raw prop element))
      (should (eq 'default (org-element-property-raw prop element 'default)))))
  ;; Standard properties mixed in the plist and standard array.
  (dolist (element `((headline (:standard-properties ,(make-vector 10 'test) :begin 1))
                     (headline (:begin 1 :standard-properties ,(make-vector 10 'test)))
                     (headline (:standard-properties ,(make-vector 10 'test) :begin 1) (headline))))
    (should (eq 'test (org-element-property-raw :begin element)))
    (should (eq 'test (org-element-property-raw :begin element 'default)))
    (should-not (org-element-property-raw :begin1 element))
    (should (eq 'default (org-element-property-raw :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (eq 'test (org-element-property-raw prop element)))
      (should (eq 'test (org-element-property-raw prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-raw prop element))
      (should (eq 'default (org-element-property-raw prop element 'default)))))
  ;; General case.
  (dolist (element `((headline (:standard-properties ,(make-vector 10 'test) :begin1 1))
                     (headline (:begin1 1 :standard-properties ,(make-vector 10 'test)))
                     (headline (:standard-properties ,(make-vector 10 'test) :begin1 1) (headline))))
    (should (eq 'test (org-element-property-raw :begin element)))
    (should (eq 'test (org-element-property-raw :begin element 'default)))
    (should (= 1 (org-element-property-raw :begin1 element)))
    (should (= 1 (org-element-property-raw :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (eq 'test (org-element-property-raw prop element)))
      (should (eq 'test (org-element-property-raw prop element 'default))))
    (dolist (prop '(:begin1))
      (should (= 1 (org-element-property-raw prop element)))
      (should (= 1 (org-element-property-raw prop element 'default))))))

(ert-deftest test-org-element/property ()
  "Test resolving deferred properties."
  ;; Resolve `:deferred' property.
  (let ((el (org-element-create
             'dummy
             `(:deferred
               ,(org-element-deferred-create
                 t (lambda (el) (org-element-put-property el :foo 'bar) nil))))))
    (should (eq 'bar (org-element-property :foo el)))
    (should-not (org-element-property :foo2 el)))
  ;; Deferred value.
  (let ((el (org-element-create
             'dummy
             `(:foo
               ,(org-element-deferred-create
                 nil (lambda (_) 'bar))))))
    (should (eq 'bar (org-element-property :foo el))))
  ;; Auto-undefer.
  (let ((el (org-element-create
             'dummy
             `(:foo
               ,(org-element-deferred-create
                 t (lambda (_) 'bar))))))
    (should (eq 'bar (org-element-property :foo el)))
    (should (eq 'bar (org-element-property-raw :foo el))))
  ;; Force undefer.
  (let ((el (org-element-create
             'dummy
             `(:foo
               ,(org-element-deferred-create
                 nil (lambda (_) 'bar))))))
    (should (eq 'bar (org-element-property :foo el)))
    (should-not (eq 'bar (org-element-property-raw :foo el)))
    (should (eq 'bar (org-element-property :foo el nil 'force)))
    (should (eq 'bar (org-element-property-raw :foo el))))
  ;; Test deferred alias.
  (let ((el (org-element-create
             'dummy
             `( :foo 1
                :bar
                ,(org-element-deferred-create-alias :foo)))))
    (should (equal 1 (org-element-property :foo el)))
    (should (equal 1 (org-element-property :bar el))))
  ;; Test deferred list.
  (let ((el (org-element-create
             'dummy
             `(:foo
               ,(org-element-deferred-create-list
                 (list 1 2 (org-element-deferred-create nil (lambda (_) 3))))))))
    (should (equal '(1 2 3) (org-element-property :foo el))))
  ;; Test deferred property with side effects.
  (let ((el (org-element-create
             'dummy
             `(:foo
               ,(org-element-deferred-create
                 nil (lambda (el)
                     (org-element-put-property el :foo 1)
                     (throw :org-element-deferred-retry nil)))))))
    (should (eq 1 (org-element-property :foo el))))
  ;; Test recursive undefer.
  (let ((el (org-element-create
             'dummy
             `(:foo
               ,(org-element-deferred-create
                 nil (lambda (el)
                     (org-element-deferred-create
                      nil (lambda (_) 1))))))))
    (should (eq 1 (org-element-property :foo el)))))

(ert-deftest test-org-element/property-2 ()
  "Test `org-element-property-2' specifications."
  (let ((el (org-element-create 'dummy '(:foo bar))))
    (should (eq (org-element-property :foo el)
                (org-element-property-2 el :foo)))))

(ert-deftest test-org-element/parent ()
  "Test `org-element-parent' specifications."
  (let ((el (org-element-create 'dummy '(:parent bar))))
    (should (eq (org-element-property :parent el)
                (org-element-parent el)))))

(ert-deftest test-org-element/properties-resolve ()
  "Test `org-element-properties-resolve' specifications."
  (let ((el (org-element-create
             'dummy
             `( :foo ,(org-element-deferred-create t (lambda (_) 1))
                :bar ,(org-element-deferred-create nil (lambda (_) 2))
                :deferred
                ,(org-element-deferred-create
                  nil (lambda (el)
                      (org-element-put-property el :baz 3)))))))
    ;; Resolve conditionally.
    (setq el (org-element-properties-resolve el))
    (should (eq 1 (org-element-property-raw :foo el)))
    (should-not (eq 2 (org-element-property-raw :bar el)))
    (should (eq 2 (org-element-property :bar el)))
    (should (eq 3 (org-element-property-raw :baz el)))
    ;; Resolve unconditionally.
    (setq el (org-element-properties-resolve el 'force))
    (should (eq 2 (org-element-property-raw :bar el)))))

(ert-deftest test-org-element/secondary-p ()
  "Test `org-element-secondary-p' specifications."
  ;; In a secondary string, return property name.
  (should
   (eq :title
       (org-test-with-temp-text "* Headline *object*"
	 (org-element-map (org-element-parse-buffer) 'bold
	   (lambda (object) (org-element-secondary-p object))
	   nil t))))
  (should
   (eq :foo
       (org-element-secondary-p
        (let* ((el (org-element-create
                    'dummy '(:secondary (:foo))))
               (child (org-element-create "string" `(:parent ,el))))
          (org-element-put-property
           el :foo (list child))
          child))))
  ;; Outside a secondary string, return nil.
  (should-not
   (org-test-with-temp-text "Paragraph *object*"
     (org-element-map (org-element-parse-buffer) 'bold
       (lambda (object) (org-element-type (org-element-secondary-p object)))
       nil t)))
  (should-not
   (eq :foo
       (org-element-secondary-p
        (let* ((el (org-element-create
                    'dummy '(:secondary (:foo))))
               (child (org-element-create "string" `(:parent ,el))))
          (org-element-put-property
           el :bar (list child))
          child)))))

(ert-deftest test-org-element/class ()
  "Test `org-element-class' specifications."
  ;; Regular tests.
  (should (eq 'element (org-element-class '(paragraph nil) nil)))
  (should (eq 'object (org-element-class '(target nil) nil)))
  ;; Special types.
  (should (eq 'element (org-element-class '(org-data nil) nil)))
  (should (eq 'object (org-element-class "text" nil)))
  (should (eq 'object (org-element-class '("secondary " "string") nil)))
  ;; Pseudo elements.
  (should (eq 'element (org-element-class '(foo nil) nil)))
  (should (eq 'element (org-element-class '(foo nil) '(center-block nil))))
  (should (eq 'element (org-element-class '(foo nil) '(org-data nil))))
  ;; Pseudo objects.
  (should (eq 'object (org-element-class '(foo nil) '(bold nil))))
  (should (eq 'object (org-element-class '(foo nil) '(paragraph nil))))
  (should (eq 'object (org-element-class '(foo nil) '("secondary"))))
  (should
   (eq 'object
       (let* ((datum '(foo nil))
	      (headline `(headline (:title (,datum) :secondary (:title)))))
	 (org-element-put-property datum :parent headline)
	 (org-element-class datum)))))


;;; Test `org-element-map' and `org-element-properties-map'

(ert-deftest test-org-element/map ()
  "Test `org-element-map'."
  ;; Can map to `plain-text' objects.
  (should
   (= 2
      (org-test-with-temp-text "Some text \alpha
#+BEGIN_CENTER
Some other text
#+END_CENTER"
	(let ((count 0))
	  (org-element-map
	      (org-element-parse-buffer) 'plain-text
	    (lambda (s) (when (string-match "text" s) (cl-incf count))))
	  count))))
  ;; Applies to secondary strings
  (should
   (org-element-map '("some " (bold nil "bold") "text") 'bold 'identity))
  ;; Enter secondary strings before entering contents.
  (should
   (equal
    "alpha"
    (org-element-property
     :name
     (org-test-with-temp-text "* Some \\alpha headline\n\\beta entity."
       (org-element-map (org-element-parse-buffer) 'entity 'identity nil t)))))
  ;; Apply NO-RECURSION argument.
  (should-not
   (org-test-with-temp-text "#+BEGIN_CENTER\n\\alpha\n#+END_CENTER"
     (org-element-map
         (org-element-parse-buffer) 'entity 'identity nil nil 'center-block)))
  ;; Use WITH-AFFILIATED argument.
  (should
   (equal
    '("1" "a" "2" "b")
    (org-test-with-temp-text "#+CAPTION[a]: 1\n#+CAPTION[b]: 2\nParagraph"
      (org-element-map
          (org-element-at-point) 'plain-text 'identity nil nil nil t)))))

(ert-deftest test-org-element/ast-map ()
  "Test `org-element-ast-map' specifications."
  ;; TYPES = t
  (should
   (equal
    '(plain-text plain-text bold)
    (org-element-ast-map
        (org-element-create 'anonymous nil "a" "b" (org-element-create 'bold))
        t #'org-element-type)))
  ;; IGNORE
  (should
   (equal
    '(plain-text plain-text)
    (let ((bold (org-element-create 'bold)))
      (org-element-ast-map
          (org-element-create 'anonymous nil "a" "b" bold)
          t #'org-element-type (list bold)))))
  ;; FUN as a list form
  (org-test-with-temp-text "* H1\n* H2"
    (should
     (equal
      '("H1" "H2")
      (org-element-map
          (org-element-parse-buffer)
          t '(org-element-property :raw-value node)))))
  ;; Extra secondary properties.
  (should
   (equal
    '(bold bold)
    (org-element-ast-map
        (org-element-create
         'dummy
         `(:foo ,(org-element-create 'bold))
         (org-element-create 'bold))
        'bold #'org-element-type
        nil nil nil '(:foo))))
  (should-not
   (equal
    '(bold bold)
    (org-element-ast-map
        (org-element-create
         'dummy
         `(:foo ,(org-element-create 'bold))
         (org-element-create 'bold))
        'bold #'org-element-type)))
  ;; No secondary.
  (should-not
   (equal
    '(bold bold)
    (org-element-ast-map
        (org-element-create
         'dummy
         `(:secondary (:foo) :foo ,(org-element-create 'bold))
         (org-element-create 'bold))
        'bold #'org-element-type
        nil nil nil nil 'no-secondary)))
  (should
   (equal
    '(bold bold)
    (org-element-ast-map
        (org-element-create
         'dummy
         `(:secondary (:foo) :foo ,(org-element-create 'bold))
         (org-element-create 'bold))
        'bold #'org-element-type)))
  ;; Deferred values.
  (should
   (equal
    '(dummy bold)
    (org-element-ast-map
        (org-element-create
         'dummy
         `(:secondary (:foo) :foo ,(org-element-deferred-create nil (lambda (_) "a")))
         (org-element-create 'bold))
        t #'org-element-type
        nil nil nil nil nil 'no-undefer)))
  (should
   (equal
    '(dummy plain-text bold)
    (org-element-ast-map
        (org-element-create
         'dummy
         `(:secondary (:foo) :foo ,(org-element-deferred-create nil (lambda (_) "a")))
         (org-element-create 'bold))
        t #'org-element-type))))

(ert-deftest test-org-element/properties-mapc ()
  "Test `org-element-properties-mapc' specifications."
  (let ((el (org-element-create
             'dummy
             `( :foo ,(org-element-deferred-create t (lambda (_) 1))
                :bar 2))))
    (should
     (catch :found
       (org-element-properties-mapc
        (lambda (_ val _)
          (when (org-element-deferred-p val)
            (throw :found t)))
        el)))
    (should
     (catch :found
       (org-element-properties-mapc
        (lambda (prop val _)
          (when (and (eq prop :foo) (eq 1 val))
            (throw :found t)))
        el 'undefer)))))

(ert-deftest test-org-element/properties-map ()
  "Test `org-element-properties-map' specifications."
  ;; Check resolving deferred properties.
  (let ((el (org-element-create
             'dummy
             `( :foo ,(org-element-deferred-create t (lambda (_) 1))
                :bar 2))))
    (should
     (equal '(2)
            (cdr
             (org-element-properties-map
              (lambda (_ val _) val) el))))
    (should-not
     (equal '(1 2)
            (org-element-properties-map
             (lambda (_ val _) val) el)))
    (should
     (equal '(1 2)
            (org-element-properties-map
             (lambda (_ val _) val) el 'undefer))))
  ;; Check functions with different arity.
  (let ((el (org-element-create 'dummy '(:foo 1 :bar 2 :baz 3))))
    (should
     ;; Single argument.
     (equal '(1 2 3)
            (org-element-properties-map #'identity el)))
    ;; Two arguments.
    (should
     (equal '(1 2 nil)
            (org-element-properties-map
             (lambda (prop val) (unless (eq prop :baz) val)) el)))
    ;; Three arguments.
    (should
     (equal '(1 2 4)
            (org-element-properties-map
             (lambda (prop val node)
               (if (eq prop :baz)
                   (1+ (org-element-property-raw :baz node))
                 val))
             el)))))


;;; Test Setters

(ert-deftest test-org-element/org-element-create ()
  "Test `org-element-create' specifications."
  (should
   (pcase (org-element-create 'foo '(:a 1 :b 2))
     (`(foo (:standard-properties ,_ :a 1 :b 2)) t)))
  (should
   (pcase (org-element-create 'foo '(:begin 10))
     (`(foo (:standard-properties ,vec))
      (= 10 (aref vec (org-element--property-idx :begin))))))
  ;; Strings
  (should (equal "foo" (org-element-create "foo")))
  (should (equal "foo" (org-element-create 'plain-text nil "foo")))
  (should (get-text-property 0 :a (org-element-create 'plain-text '(:a 1) "foo")))
  (should (get-text-property 0 :begin (org-element-create 'plain-text '(:begin 1) "foo")))
  ;; Children
  (let ((children '("a" "b" (org-element-create 'foo))))
    (should (equal (cddr (apply #'org-element-create 'bar nil children))
                   children)))
  (let ((children nil))
    (should (equal (cddr (org-element-create 'bar nil children))
                   children))
    (should (equal (cddr (apply #'org-element-create 'bar nil children))
                   children)))
  (let ((children '("foo" nil "bar")))
    (should (equal (cddr (apply #'org-element-create 'bar nil children))
                   (delq nil children)))))

(ert-deftest test-org-element/put-property ()
  "Test `org-element-put-property' specifications."
  ;; Standard test.
  (org-test-with-temp-text "* Headline\n *a*"
    (let ((tree (org-element-parse-buffer)))
      (org-element-put-property
       (org-element-map tree 'bold 'identity nil t) :test 1)
      (should (org-element-property
	       :test (org-element-map tree 'bold 'identity nil t)))))
  ;; Put property on a string.
  (should
   (org-element-property :test (org-element-put-property "Paragraph" :test t)))
  ;; No properties.
  (let ((element (list 'heading nil))
        vec)
    (setq vec (make-vector (length org-element--standard-properties) nil))
    (aset vec 0 1)
    (should
     (equal
      (list 'heading (list :standard-properties vec))
      (org-element-put-property element :begin 1))))
  (let ((element (list 'heading nil)))
    (should
     (equal
      (list 'heading (list :begin1 1))
      (org-element-put-property element :begin1 1))))
  ;; Standard properties.
  (let ((element (list 'heading (list :standard-properties (make-vector (length org-element--standard-properties) 'foo)))))
    (should
     (= 1
        (org-element-property-raw :begin (org-element-put-property element :begin 1)))))
  ;; Adding standard properties when other standard properties are defined manually in the plist.
  (let ((element (list 'heading (list :begin 1 :end 20 :foo 'foo))))
    (should
     (= 2
        (org-element-property-raw :begin (org-element-put-property element :begin 2))))
    ;; Check setter.
    (cl-incf (org-element-property-raw :begin element))
    (should
     (= 3 (org-element-property-raw :begin element)))
    (should
     (= 20
        (org-element-property-raw :end element)))
    (should
     (eq 'foo
         (org-element-property-raw :foo element)))))

(ert-deftest test-org-element/put-property-2 ()
  "Test `org-element-put-property-2' specifications."
  (should
   (equal (org-element-put-property (org-element-create 'foo) :test 'value)
          (org-element-put-property-2 :test 'value  (org-element-create 'foo)))))

(ert-deftest test-org-element/set-contents ()
  "Test `org-element-set-contents' specifications."
  ;; Accept multiple entries.
  (should
   (equal '("b" (italic nil "a"))
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-set-contents
	       (org-element-map tree 'bold 'identity nil t) "b" '(italic nil "a"))
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t))))))
  ;; Accept atoms and elements.
  (should
   (equal '("b")
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-set-contents
	       (org-element-map tree 'bold 'identity nil t) "b")
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t))))))
  (should
   (equal '((italic nil "b"))
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-set-contents
	       (org-element-map tree 'bold 'identity nil t) '(italic nil "b"))
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t))))))
  ;; Allow nil contents.
  (should-not
   (org-test-with-temp-text "* Headline\n *a*"
     (let ((tree (org-element-parse-buffer)))
       (org-element-set-contents (org-element-map tree 'bold 'identity nil t))
       (org-element-contents (org-element-map tree 'bold 'identity nil t)))))
  ;; Set contents of anonymous elements.
  (should
   (equal '#1=((b (:parent #1#)))
          (let ((element '#1=((a (:parent #1#)) (b (:parent #1#)))))
            (org-element-set-contents element `(b (:parent ,element)))
            element))))

(ert-deftest test-org-element/adopt-elements ()
  "Test `org-element-adopt' specifications."
  ;; Adopt an element.
  (should
   (equal '(plain-text italic)
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-adopt
	          (org-element-map tree 'bold 'identity nil t) '(italic nil "a"))
	      (mapcar (lambda (blob) (org-element-type blob))
		      (org-element-contents
		       (org-element-map tree 'bold 'identity nil t)))))))
  ;; Adopt a string.
  (should
   (equal '("a" "b")
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-adopt
	          (org-element-map tree 'bold 'identity nil t) "b")
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t)))))))

(ert-deftest test-org-element/extract-element ()
  "Test `org-element-extract' specifications."
  ;; Extract a greater element.
  (should
   (eq 'org-data
       (org-test-with-temp-text "* Headline"
	 (let* ((tree (org-element-parse-buffer))
		(element (org-element-map tree 'headline 'identity nil t)))
	   (org-element-extract element)
	   (org-element-type tree)))))
  ;; Extract an element.
  (should-not
   (org-element-map
       (org-test-with-temp-text "Paragraph"
	 (let* ((tree (org-element-parse-buffer))
		(element (org-element-map tree 'paragraph 'identity nil t)))
	   (org-element-extract element)
	   tree))
       'paragraph
     'identity))
  ;; Extract an object, even in a secondary string.
  (should-not
   (org-element-map
       (org-test-with-temp-text "*bold*"
	 (let* ((tree (org-element-parse-buffer))
		(element (org-element-map tree 'bold 'identity nil t)))
	   (org-element-extract element)
	   tree))
       'bold
     'identity))
  (should-not
   (org-element-map
       (org-test-with-temp-text "* Headline *bold*"
	 (let* ((tree (org-element-parse-buffer))
		(element (org-element-map tree 'bold 'identity nil t)))
	   (org-element-extract element)
	   tree))
       'bold
     'identity))
  ;; Return value doesn't have any :parent set.
  (should-not
   (org-element-property
    :parent
    (org-test-with-temp-text "* Headline\n  Paragraph with *bold* text."
      (let* ((tree (org-element-parse-buffer))
	     (element (org-element-map tree 'bold 'identity nil t)))
	(org-element-extract element))))))

(ert-deftest test-org-element/insert-before ()
  "Test `org-element-insert-before' specifications."
  ;; Standard test.
  (should
   (equal
    '(italic entity bold)
    (org-test-with-temp-text "/some/ *paragraph*"
      (let* ((tree (org-element-parse-buffer))
	     (_paragraph (org-element-map tree 'paragraph #'identity nil t))
	     (bold (org-element-map tree 'bold 'identity nil t)))
	(org-element-insert-before '(entity (:name "\\alpha")) bold)
	(org-element-map tree '(bold entity italic) #'org-element-type nil)))))
  ;; Insert an object in a secondary string.
  (should
   (equal
    '(entity italic)
    (org-test-with-temp-text "* /A/\n  Paragraph."
      (let* ((tree (org-element-parse-buffer))
	     (headline (org-element-map tree 'headline 'identity nil t))
	     (italic (org-element-map tree 'italic 'identity nil t)))
	(org-element-insert-before '(entity (:name "\\alpha")) italic)
	(org-element-map (org-element-property :title headline) '(entity italic)
	  #'org-element-type))))))

(ert-deftest test-org-element/set ()
  "Test `org-element-set' specifications."
  ;; Check if new element is inserted.
  (should
   (org-test-with-temp-text "* Headline\n*a*"
     (let* ((tree (org-element-parse-buffer))
	    (bold (org-element-map tree 'bold 'identity nil t)))
       (org-element-set bold '(italic nil "b"))
       (org-element-map tree 'italic 'identity))))
  ;; Check if old element is removed.
  (should-not
   (org-test-with-temp-text "* Headline\n*a*"
     (let* ((tree (org-element-parse-buffer))
	    (bold (org-element-map tree 'bold 'identity nil t)))
       (org-element-set bold '(italic nil "b"))
       (org-element-map tree 'bold 'identity))))
  ;; Check if :parent property is correctly set.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "* Headline\n*a*"
	 (let* ((tree (org-element-parse-buffer))
		(bold (org-element-map tree 'bold 'identity nil t)))
	   (org-element-set bold '(italic nil "b"))
	   (org-element-type
	    (org-element-property
	     :parent (org-element-map tree 'italic 'identity nil t)))))))
  ;; Allow to replace strings with elements.
  (should
   (equal '("b")
	  (org-test-with-temp-text "* Headline"
	    (let* ((tree (org-element-parse-buffer))
		   (text (org-element-map tree 'plain-text 'identity nil t)))
	      (org-element-set text (list 'bold nil "b"))
	      (org-element-map tree 'plain-text 'identity)))))
  ;; Allow to replace elements with strings.
  (should
   (equal "a"
	  (org-test-with-temp-text "* =verbatim="
	    (let* ((tree (org-element-parse-buffer))
		   (verb (org-element-map tree 'verbatim 'identity nil t)))
	      (org-element-set verb "a")
	      (org-element-map tree 'plain-text 'identity nil t)))))
  ;; Allow to replace strings with strings.
  (should
   (equal "b"
	  (org-test-with-temp-text "a"
	    (let* ((tree (org-element-parse-buffer))
		   (text (org-element-map tree 'plain-text 'identity nil t)))
	      (org-element-set text "b")
	      (org-element-map tree 'plain-text 'identity nil t)))))
  ;; Replace string inside anonymous element with another string.
  (let* ((parent (org-element-create 'anonymous nil "test"))
	 (str (car (org-element-contents parent))))
    (let ((return (org-element-set str "repl"))
          (new (car (org-element-contents parent))))
      ;; Return the modified value.
      (should (eq return new))
      (should (equal new "repl"))
      (should (eq (org-element-parent new) parent))))
  ;; KEEP-PROPS
  (should
   (org-element-property
    :foo
    (org-element-set
     (org-element-create 'dummy '(:foo bar))
     (org-element-create 'dummy '(:foo2 bar2))
     '(:foo)))))

(ert-deftest test-org-element/copy ()
  "Test `org-element-copy' specifications."
  ;; Preserve type.
  (should (eq 'bold
	      (org-test-with-temp-text "*bold*"
		(org-element-type (org-element-copy (org-element-context))))))
  (should (eq 'plain-text
	      (org-test-with-temp-text "*bold*"
		(org-element-type
		 (org-element-map (org-element-parse-buffer) 'plain-text
		   #'org-element-copy nil t)))))
  ;; Preserve properties except `:parent'.
  (should (= 7
	     (org-test-with-temp-text "*bold*"
	       (org-element-property
		:end (org-element-copy (org-element-context))))))
  (should-not
   (org-test-with-temp-text "*bold*"
     (org-element-property
      :parent (org-element-copy (org-element-context)))))
  (should-not
   (org-test-with-temp-text "*bold*"
     (org-element-property
      :parent
      (org-element-map (org-element-parse-buffer) 'plain-text
	#'org-element-copy nil t))))
  ;; Copying nil returns nil.
  (should-not (org-element-copy nil))
  ;; Return a copy secondary strings.
  (should (equal '("text") (org-element-copy '("text"))))
  (should-not (eq '("text") (org-element-copy '("text"))))
  ;; Do not alter the source.
  (org-test-with-temp-text "*bold*"
    (let* ((source (org-element-context))
           (copy (org-element-copy source)))
      (should-not (org-element-parent copy))
      (should (org-element-parent source)))))



;;; Test Parsers

;;;; Affiliated Keywords

(ert-deftest test-org-element/affiliated-keywords-parser ()
  "Test affiliated keywords parsing."
  ;; Read simple keywords.
  (should
   (equal "para"
	  (org-element-property
	   :name
	   (org-test-with-temp-text "#+NAME: para\nParagraph"
	     (org-element-at-point)))))
  (should
   (= 1
      (org-element-property
       :begin
       (org-test-with-temp-text "#+NAME: para\nParagraph"
	 (org-element-at-point)))))
  ;; Parse multiple keywords.
  (should
   (equal
    '("line1" "line2")
    (org-element-property
     :attr_ascii
     (org-test-with-temp-text
	 "#+ATTR_ASCII: line1\n#+ATTR_ASCII: line2\nParagraph"
       (org-element-at-point)))))
  ;; Parse "parsed" keywords, unless granularity prevents it.
  (should
   (equal
    '(("caption"))
    (org-test-with-temp-text "#+CAPTION: caption\nParagraph"
      (car (org-element-property :caption (org-element-at-point))))))
  (should
   (org-test-with-temp-text "#+CAPTION: *caption*\nParagraph"
     (org-element-map (org-element-map (org-element-parse-buffer)
			  'paragraph
			(lambda (e) (org-element-property :caption e)) nil t)
	 'bold
       #'org-element-type nil t)))
  (should-not
   (org-test-with-temp-text "#+CAPTION: *caption*\nParagraph"
     (org-element-map (org-element-map (org-element-parse-buffer 'element)
			  'paragraph
			(lambda (e) (org-element-property :caption e)) nil t)
	 'bold
       #'org-element-type nil t)))
  ;; Parse dual keywords.
  (should
   (equal
    '((("long") "short"))
    (org-test-with-temp-text "#+CAPTION[short]: long\nParagraph"
      (org-element-property :caption (org-element-at-point)))))
  ;; Allow multiple caption keywords.
  (should
   (equal
    '((("l1") "s1") (("l2") "s2"))
    (org-test-with-temp-text "#+CAPTION[s1]: l1\n#+CAPTION[s2]: l2\nParagraph"
      (org-element-property :caption (org-element-at-point)))))
  (should
   (equal
    '((nil "s1") (("l1")))
    (org-test-with-temp-text "#+CAPTION[s1]:\n#+CAPTION: l1\nParagraph"
      (org-element-property :caption (org-element-at-point)))))
  ;; Corner case: orphaned keyword at the end of an element.
  (should
   (eq 'keyword
       (org-test-with-temp-text "- item\n  #+name: name\nSome paragraph"
	 (progn (search-forward "name")
		(org-element-type (org-element-at-point))))))
  (should-not
   (org-test-with-temp-text "- item\n  #+name: name\nSome paragraph"
     (progn (search-forward "Some")
	    (org-element-property :name (org-element-at-point)))))
  ;; Corner case: orphaned keyword before comment.
  ;; Comments cannot have affiliated keywords.
  (should-not
   (org-test-with-temp-text "#+name: foo\n# bar"
     (progn (search-forward "bar")
	    (org-element-property :name (org-element-at-point)))))
  ;; Headlines cannot have affiliated keywords.
  (should
   (org-test-with-temp-text "<point>#+name: foo\n* Heading"
     (org-element-type-p (org-element-at-point) 'keyword)))
  ;; Clocks cannot have affiliated keywords.
  (should
   (org-test-with-temp-text "<point>#+name: foo
CLOCK: [2023-10-13 Fri 14:40]--[2023-10-13 Fri 14:51] =>  0:11"
     (org-element-type-p (org-element-at-point) 'keyword)))
  ;; Inlinetasks cannot have affiliated keywords.
  (should
   (let ((org-inlinetask-min-level 4))
     (org-test-with-temp-text "<point>#+name: foo
**** Inlinetask"
       (org-element-type-p (org-element-at-point) 'keyword)))))


;;;; Babel Call

(ert-deftest test-org-element/babel-call-parser ()
  "Test `babel-call' parsing."
  ;; Standard test.
  (should
   (eq 'babel-call
       (org-test-with-temp-text "#+CALL: test()"
	 (org-element-type (org-element-at-point)))))
  ;; Ignore case.
  (should
   (eq 'babel-call
       (org-test-with-temp-text "#+call: test()"
	 (org-element-type (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+CALL: test()\n "
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  ;; Parse call name.
  (should
   (equal "test"
	  (org-test-with-temp-text "#+CALL: test()"
	    (org-element-property :call (org-element-at-point)))))
  ;; Parse inside header.  It may contain paired square brackets.
  (should
   (equal ":results output"
	  (org-test-with-temp-text "#+CALL: test[:results output]()"
	    (org-element-property :inside-header (org-element-at-point)))))
  (should
   (equal ":results output, a=table[1:2], b=2"
	  (org-test-with-temp-text
	      "#+CALL: test[:results output, a=table[1:2], b=2]()"
	    (org-element-property :inside-header (org-element-at-point)))))
  ;; Parse arguments, which can be nested.  However, stop at paired
  ;; parenthesis, even when, e.g.,end header contains some.
  (should
   (equal "n=4"
	  (org-test-with-temp-text "#+CALL: test(n=4)"
	    (org-element-property :arguments (org-element-at-point)))))
  (should
   (equal "test()"
	  (org-test-with-temp-text "#+CALL: test(test())"
	    (org-element-property :arguments (org-element-at-point)))))
  (should
   (equal "a=1"
	  (org-test-with-temp-text "#+CALL: test(a=1) :post another-call()"
	    (org-element-property :arguments (org-element-at-point)))))
  ;; Parse end header.
  (should
   (equal ":results html"
	  (org-test-with-temp-text "#+CALL: test() :results html"
	    (org-element-property :end-header (org-element-at-point))))))


;;;; Bold

(ert-deftest test-org-element/bold-parser ()
  "Test `bold' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "*bold*"
     (org-element-map (org-element-parse-buffer) 'bold #'identity nil t)))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (org-test-with-temp-text "*first line\nsecond line*"
       (org-element-map (org-element-parse-buffer) 'bold #'identity nil t)))
    '("first line\nsecond line"))))


;;;; Center Block

(ert-deftest test-org-element/center-block-parser ()
  "Test `center-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nText\n#+END_CENTER"
     (org-element-map (org-element-parse-buffer) 'center-block 'identity)))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "#+begin_center\nText\n#+end_center"
     (org-element-map (org-element-parse-buffer) 'center-block 'identity)))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_CENTER"
     (org-element-map (org-element-parse-buffer) 'center-block
       'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nC\n#+END_CENTER\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Citation

(ert-deftest test-org-element/citation-parser ()
  "Test `citation' parser"
  ;; Parse citations.  They must contain at least a bare key.
  (should
   (eq 'citation
       (org-test-with-temp-text "[cite:@key]"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'citation
       (org-test-with-temp-text "[cite:text]"
	 (org-element-type (org-element-context)))))
  ;; Citation may contain a style.
  (should
   (eq 'citation
       (org-test-with-temp-text "[cite/style:@key]"
	 (org-element-type (org-element-context)))))
  (should
   (equal "style"
	  (org-test-with-temp-text "[cite/style:@key]"
	    (org-element-property :style (org-element-context)))))
  ;; Handle multi citations separated with semi-columns.
  (should
   (eq 'citation
       (org-test-with-temp-text "[cite:@a;@b;@c]"
	 (org-element-type (org-element-context)))))
  (should
   (equal '("a" "b" "c")
	  (org-test-with-temp-text "[cite:@a;@b;@c]"
	    (org-element-map (org-element-parse-buffer) 'citation-reference
	      (lambda (r) (org-element-property :key r))))))
  (should
   (eq 'citation
       (org-test-with-temp-text "[cite:@a;-@b]"
	 (org-element-type (org-element-context)))))
  (should
   (equal '("a" "b")
	  (org-test-with-temp-text "[cite:@a;-@b]"
	    (org-element-map (org-element-parse-buffer) 'citation-reference
	      (lambda (r) (org-element-property :key r))))))
  ;; Multi citations accept `:prefix' and `:suffix' properties.
  (should
   (equal '("common-prefix")
	  (org-test-with-temp-text "[cite:common-prefix;@a]"
	    (org-element-property :prefix (org-element-context)))))
  (should
   (equal '("common-suffix")
	  (org-test-with-temp-text "[cite:@a;common-suffix]"
	    (org-element-property :suffix (org-element-context)))))
  ;; White spaces right after "cite" tags are ignored. So are white
  ;; spaces at the end of the citation.
  (should
   (equal '("common-prefix ")
	  (org-test-with-temp-text "[cite: common-prefix ;@a]"
	    (org-element-property :prefix (org-element-context)))))
  (should
   (equal '(" common-suffix")
	  (org-test-with-temp-text "[cite: @a; common-suffix ]"
	    (org-element-property :suffix (org-element-context)))))
  ;; Allow citations in a table cell.
  (should
   (eq 'citation
       (org-test-with-temp-text "| <point>[cite:@key] |"
	 (org-element-type (org-element-context))))))


;;;; Citation Reference

(ert-deftest test-org-element/citation-reference-parser ()
  "Test `citation' reference parser."
  ;; Parse bare keys.
  (should
   (eq 'citation-reference
       (org-test-with-temp-text "[cite:<point>@key]"
	 (org-element-type (org-element-context)))))
  ;; Bare keys can contain any word character, and some punctuation,
  ;; but not semicolon, square brackets, and space.
  (should
   (equal "_key"
	  (org-test-with-temp-text "[cite:@_k<point>ey]"
	    (org-element-property :key (org-element-context)))))
  (should
   (eq 'citation-reference
       (org-test-with-temp-text "[cite:<point>@a]"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'citation-reference
       (org-test-with-temp-text "[cite:<point>@ö]"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'citation-reference
       (org-test-with-temp-text "[cite:<point>@_]"
	 (org-element-type (org-element-context)))))
  (should
   (equal "a:.#$%&-+?<>~/1"
	  (org-test-with-temp-text "[cite:<point>@a:.#$%&-+?<>~/1]"
	    (org-element-property :key (org-element-context)))))
  (should-not
   (eq 'citation-reference
       (org-test-with-temp-text "[cite:<point>@;]"
	 (org-element-type (org-element-context)))))
  (should-not
   (equal "key"
	  (org-test-with-temp-text "[cite:<point>@[]]"
	    (org-element-property :key (org-element-context)))))
  ;; References in citations accept optional `:prefix' and `:suffix'
  ;; properties.
  (should
   (equal '("pre ")
	  (org-test-with-temp-text "[cite:pre <point>@key]"
	    (org-element-property :prefix (org-element-context)))))
  (should
   (equal '(" post")
	  (org-test-with-temp-text "[cite:<point>@key post]"
	    (org-element-property :suffix (org-element-context)))))
  ;; White spaces between "cite" tag and prefix are ignored.
  (should
   (equal '("pre ")
	  (org-test-with-temp-text "[cite: pre <point>@key]"
	    (org-element-property :prefix (org-element-context)))))
  ;; Semicolons do not belong to prefix or suffix.
  (should
   (equal '("pre ")
	  (org-test-with-temp-text "[cite:@key1;pre <point>@key2]"
	    (org-element-property :prefix (org-element-context)))))
  (should
   (equal '(" post")
	  (org-test-with-temp-text "[cite:@key1 <point>post;@key2]"
	    (org-element-property :suffix (org-element-context)))))
  (should
   (equal '("pre ")
	  (org-test-with-temp-text "[cite:global prefix;pre<point> @key1]"
	    (org-element-property :prefix (org-element-context)))))
  (should
   (equal '(" post")
	  (org-test-with-temp-text "[cite:@key1 <point>post; global suffix]"
	    (org-element-property :suffix (org-element-context))))))

;;;; Clock

(ert-deftest test-org-element/clock-parser ()
  "Test `clock' parser."
  ;; Running clock.
  (let ((clock (org-test-with-temp-text "CLOCK: [2012-01-01 sun. 00:01]"
		 (org-element-at-point))))
    (should (eq (org-element-property :status clock) 'running))
    (should
     (equal (org-element-property :raw-value
				  (org-element-property :value clock))
	    "[2012-01-01 sun. 00:01]"))
    (should-not (org-element-property :duration clock)))
  ;; clock string should not be case-sensitive.
  (let ((clock (org-test-with-temp-text "Clock: [2012-01-01 sun. 00:01]"
		 (org-element-at-point))))
    (should (eq (org-element-property :status clock) 'running))
    (should
     (equal (org-element-property :raw-value
				  (org-element-property :value clock))
	    "[2012-01-01 sun. 00:01]"))
    (should-not (org-element-property :duration clock)))
  ;; Closed clock.
  (let ((clock
	 (org-test-with-temp-text
	     "CLOCK: [2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02] =>  0:01"
	   (org-element-at-point))))
    (should (eq (org-element-property :status clock) 'closed))
    (should (equal (org-element-property :raw-value
					 (org-element-property :value clock))
		   "[2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02]"))
    (should (equal (org-element-property :duration clock) "0:01")))
  ;; Closed clock without timestamp.
  (let ((clock
	 (org-test-with-temp-text
	     "CLOCK: =>  0:11"
	   (org-element-at-point))))
    (should (eq (org-element-property :status clock) 'closed))
    (should-not (org-element-property :value clock))
    (should (equal (org-element-property :duration clock) "0:11"))))


;;;; Code

(ert-deftest test-org-element/code-parser ()
  "Test `code' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "~code~"
     (org-element-map (org-element-parse-buffer) 'code #'identity)))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-property
     :value
     (org-test-with-temp-text "~first line\nsecond line~"
       (org-element-map
	   (org-element-parse-buffer) 'code #'identity nil t)))
    "first line\nsecond line")))


;;;; Comment

(ert-deftest test-org-element/comment-parser ()
  "Test `comment' parser."
  ;; Regular comment.
  (should (eq 'comment
	      (org-test-with-temp-text "# Comment"
		(org-element-type (org-element-at-point)))))
  ;; Inline comment.
  (should (eq 'comment
	      (org-test-with-temp-text "  # Comment"
		(org-element-type (org-element-at-point)))))
  ;; Preserve indentation.
  (should
   (equal "No blank\n One blank"
	  (org-element-property
	   :value
	   (org-test-with-temp-text "# No blank\n#  One blank"
	     (org-element-at-point)))))
  ;; Comment with blank lines.
  (should
   (equal "First part\n\n\nSecond part"
	  (org-element-property
	   :value
	   (org-test-with-temp-text "# First part\n# \n#\n# Second part"
	     (org-element-at-point)))))
  ;; Do not mix comments and keywords.
  (should
   (eq 1
       (org-test-with-temp-text "#+keyword: value\n# comment\n#+keyword: value"
	 (length (org-element-map (org-element-parse-buffer) 'comment
		   #'identity)))))
  (should
   (equal "comment"
	  (org-test-with-temp-text
	      "#+key: value\n<point># comment\n#+key: value"
	    (org-element-property :value (org-element-at-point)))))
  ;; Correctly handle non-empty blank lines at the end of buffer.
  (should
   (org-test-with-temp-text "# A\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Comment Block

(ert-deftest test-org-element/comment-block-parser ()
  "Test `comment-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+BEGIN_COMMENT\nText\n#+END_COMMENT"
     (org-element-map (org-element-parse-buffer) 'comment-block 'identity)))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "#+begin_comment\nText\n#+end_comment"
     (org-element-map (org-element-parse-buffer) 'comment-block 'identity)))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_COMMENT"
     (org-element-map (org-element-parse-buffer) 'comment-block
       'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_COMMENT\nC\n#+END_COMMENT\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Diary Sexp

(ert-deftest test-org-element/diary-sexp-parser ()
  "Test `diary-sexp' parser."
  ;; Standard test.
  (should
   (eq 'diary-sexp
       (org-test-with-temp-text
	   "%%(org-anniversary 1956  5 14)(2) Arthur Dent is %d years old"
	 (org-element-type (org-element-at-point)))))
  ;; Diary sexp must live at beginning of line
  (should-not
   (eq 'diary-sexp
       (org-test-with-temp-text " %%(org-bbdb-anniversaries)"
	 (org-element-type (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "%%(org-bbdb-anniversaries)\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Drawer

(ert-deftest test-org-element/drawer-parser ()
  "Test `drawer' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text ":TEST:\nText\n:END:"
     (org-element-map (org-element-parse-buffer) 'drawer 'identity)))
  ;; Ignore incomplete drawer.
  (should-not
   (org-test-with-temp-text ":TEST:"
     (org-element-map (org-element-parse-buffer) 'drawer 'identity nil t)))
  (should-not
   (org-test-with-temp-text ":END:"
     (org-element-map (org-element-parse-buffer) 'drawer 'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text ":TEST:\nC\n:END:\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Dynamic Block

(ert-deftest test-org-element/dynamic-block-parser ()
  "Test `dynamic-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text
       "#+BEGIN: myblock :param1 val1 :param2 val2\nText\n#+END:"
     (org-element-map (org-element-parse-buffer) 'dynamic-block 'identity)))
  ;; Ignore case.
  (should
   (org-test-with-temp-text
       "#+begin: myblock :param1 val1 :param2 val2\nText\n#+end:"
     (org-element-map (org-element-parse-buffer) 'dynamic-block 'identity)))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN: myblock :param1 val1 :param2 val2"
     (org-element-map (org-element-parse-buffer) 'dynamic-block
       'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN: myblock :param val1\nC\n#+END:\n  "
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  ;; Block name is mandatory.
  (should-not
   (org-test-with-temp-text "#+BEGIN:\n\n#+END:\n"
     (org-element-type-p (org-element-at-point) 'dynamic-block))))


;;;; Entity

(ert-deftest test-org-element/entity-parser ()
  "Test `entity' parser."
  ;; Without brackets.
  (should
   (org-test-with-temp-text "\\sin"
     (org-element-map (org-element-parse-buffer) 'entity 'identity)))
  ;; Special case: space-based entities.
  (should
   (equal
    "_   "
    ;; Space after entity must be a part of its name.
    (org-test-with-temp-text "\\_   Foo"
      (org-element-property
       :name
       (car (org-element-map (org-element-parse-buffer) 'entity 'identity))))))
  (should-not
   ;; {} is not a part of whitespace entity name.
   (org-test-with-temp-text "\\_   {}Foo"
     (org-element-property
      :bracketsp
      (car (org-element-map (org-element-parse-buffer) 'entity 'identity)))))
  ;; With brackets.
  (should
   (org-element-property
    :use-brackets-p
    (org-test-with-temp-text "\\alpha{}text"
      (org-element-map (org-element-parse-buffer) 'entity 'identity nil t))))
  ;; User-defined entity.
  (should
   (equal
    (org-element-property
     :name
     (let ((org-entities-user
	    '(("test" "test" nil "test" "test" "test" "test"))))
       (org-test-with-temp-text "\\test"
	 (org-element-map (org-element-parse-buffer) 'entity 'identity nil t))))
    "test"))
  ;; Special case: entity at the end of a container.
  (should
   (eq 'entity
       (org-test-with-temp-text "*\\alpha \\beta*"
	 (search-forward "be")
	 (org-element-type (org-element-context))))))


;;;; Example Block

(ert-deftest test-org-element/example-block-parser ()
  "Test `example-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText\n#+END_EXAMPLE"
     (org-element-map (org-element-parse-buffer) 'example-block 'identity)))
  ;; Ignore incomplete block.
  (should-not
   (eq 'example-block
       (org-test-with-temp-text "#+BEGIN_EXAMPLE"
	 (org-element-type (org-element-at-point)))))
  ;; Properly un-escape code.
  (should
   (equal "* Headline\n #+keyword:\nText\n"
	  (org-test-with-temp-text
	      "#+BEGIN_EXAMPLE\n,* Headline\n ,#+keyword:\nText\n#+END_EXAMPLE"
	    (org-element-property :value (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\nC\n#+END_EXAMPLE\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))

(ert-deftest test-org-element/block-switches ()
  "Test `example-block' and `src-block' switches parsing."
  (let ((org-coderef-label-format "(ref:%s)"))
    ;; 1. Test "-i" switch.
    (should-not
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
       (org-element-property :preserve-indent (org-element-at-point))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n#+END_SRC"
       (org-element-property :preserve-indent (org-element-at-point))))
    (should-not
     (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText.\n#+END_EXAMPLE"
       (org-element-property :preserve-indent (org-element-at-point))))
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE -i\nText.\n#+END_EXAMPLE"
       (org-element-property :preserve-indent (org-element-at-point))))
    ;; 2. "-n -r -k" combination should number lines, retain labels but
    ;;    not use them in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -n -r -k\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-at-point)))
	(should (org-element-property :number-lines element))
	(should (org-element-property :retain-labels element))
	(should-not (org-element-property :use-labels element))))
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -n -r -k\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-at-point)))
	(should (org-element-property :number-lines element))
	(should (org-element-property :retain-labels element))
	(should-not (org-element-property :use-labels element))))
    ;; 3. "-n -r" combination should number-lines remove labels and not
    ;;    use them in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -n -r\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-at-point)))
	(should (org-element-property :number-lines element))
	(should-not (org-element-property :retain-labels element))
	(should-not (org-element-property :use-labels element))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -n -r\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-at-point)))
	(should (org-element-property :number-lines element))
	(should-not (org-element-property :retain-labels element))
	(should-not (org-element-property :use-labels element))))
    ;; 4. "-n" or "+n" should number lines, retain labels and use them
    ;;    in coderefs.
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE -n\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -n\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE +n\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp +n\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    ;; 5. No switch should not number lines, but retain labels and use
    ;;    them in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-at-point)))
	(should (not (org-element-property :number-lines element)))
	(should (org-element-property :retain-labels element))
	(should (org-element-property :use-labels element))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-at-point)))
	(should (not (org-element-property :number-lines element)))
	(should (org-element-property :retain-labels element))
	(should (org-element-property :use-labels element))))
    ;; 6. "-r" switch only: do not number lines, remove labels, and
    ;;    don't use labels in coderefs.
    (org-test-with-temp-text "#+BEGIN_EXAMPLE -r\nText.\n#+END_EXAMPLE"
      (let ((element (org-element-at-point)))
	(should (not (org-element-property :number-lines element)))
	(should (not (org-element-property :retain-labels element)))
	(should (not (org-element-property :use-labels element)))))
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -r\n(+ 1 1)\n#+END_SRC"
      (let ((element (org-element-at-point)))
	(should (not (org-element-property :number-lines element)))
	(should (not (org-element-property :retain-labels element)))
	(should (not (org-element-property :use-labels element)))))
    ;; 7. Recognize coderefs with user-defined syntax.
    (should
     (equal
      "[ref:%s]"
      (org-test-with-temp-text
	  "#+BEGIN_EXAMPLE -l \"[ref:%s]\"\nText [ref:text]\n#+END_EXAMPLE"
	(org-element-property :label-fmt (org-element-at-point)))))
    (should
     (equal
      "[ref:%s]"
      (org-test-with-temp-text
	  "#+BEGIN_SRC emacs-lisp -l \"[ref:%s]\"\n(+ 1 1) [ref:text]\n#+END_SRC"
	(org-element-property :label-fmt (org-element-at-point)))))))


;;;; Export Block

(ert-deftest test-org-element/export-block-parser ()
  "Test `export-block' parser."
  ;; Standard test.
  (should
   (eq 'export-block
       (org-test-with-temp-text "#+BEGIN_EXPORT LATEX\nText\n#+END_EXPORT"
	 (org-element-type (org-element-at-point)))))
  (should
   (equal "LATEX"
	  (org-test-with-temp-text "#+BEGIN_EXPORT LATEX\nText\n#+END_EXPORT"
	    (org-element-property :type (org-element-at-point)))))
  ;; Ignore case.
  (should
   (eq 'export-block
       (org-test-with-temp-text "#+begin_export latex\nText\n#+end_export"
	 (org-element-type (org-element-at-point)))))
  ;; Ignore incomplete block.
  (should-not
   (eq 'export-block
       (org-test-with-temp-text "#+BEGIN_EXPORT"
	 (org-element-type (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_EXPORT latex\nC\n#+END_EXPORT\n "
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  ;; Un-escape commas in `:value'.
  (should
   (equal "* H\n"
	  (org-test-with-temp-text "#+BEGIN_EXPORT org\n,* H\n#+END_EXPORT\n "
	    (org-element-property :value (org-element-at-point))))))


;;;; Export Snippet

(ert-deftest test-org-element/export-snippet-parser ()
  "Test `export-snippet' parser."
  (should
   (equal
    '("backend" . "contents")
    (org-test-with-temp-text "@@backend:contents@@"
      (org-element-map
       (org-element-parse-buffer) 'export-snippet
       (lambda (snippet) (cons (org-element-property :back-end snippet)
			  (org-element-property :value snippet)))
       nil t)))))


;;;; Fixed Width

(ert-deftest test-org-element/fixed-width-parser ()
  "Test fixed-width area parsing."
  ;; Preserve indentation.
  (should
   (equal "no blank\n one blank"
	  (org-test-with-temp-text ": no blank\n:  one blank"
	    (org-element-property :value (org-element-at-point)))))
  ;; Fixed-width with empty lines.
  (should
   (equal "first part\n\n\nsecond part"
	  (org-test-with-temp-text ": first part\n:\n: \n: second part"
	    (org-element-property :value (org-element-at-point)))))
  ;; Parse indented fixed-width markers.
  (should
   (eq 'fixed-width
       (org-test-with-temp-text "Text\n<point>  : no blank\n  :  one blank"
	 (org-element-type (org-element-at-point)))))
  ;; Distinguish fixed-width areas within a list and outside of it.
  (should
   (org-test-with-temp-text "
- Item
  : fixed-width inside<point>
: fixed-width outside"
     (= (org-element-property :end (org-element-at-point))
	(line-beginning-position 2))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text ": A\n "
     (= (org-element-property :end (org-element-at-point))
	(point-max))))
  ;; Correctly parse post-blank
  (should
   (org-test-with-temp-text ": A\n "
     (= (org-element-property :post-blank (org-element-at-point))
	1)))
  (should
   (org-test-with-temp-text ": A\nB"
     (= (org-element-property :post-blank (org-element-at-point))
	0))))


;;;; Footnote Definition

(ert-deftest test-org-element/footnote-definition-parser ()
  "Test `footnote-definition' parser."
  (should
   (org-test-with-temp-text "[fn:1] Definition"
     (eq (org-element-type (org-element-at-point)) 'footnote-definition)))
  ;; Footnote with more contents.
  (should
   (= 29 (org-test-with-temp-text "[fn:1] Definition\n\n| a | b |"
	   (org-element-property :end (org-element-at-point)))))
  ;; Test difference between :contents-end and :end property
  (should
   (< (org-test-with-temp-text "[fn:1] Definition\n\n\n"
	(org-element-property :contents-end (org-element-at-point)))
      (org-test-with-temp-text "[fn:1] Definition\n\n\n"
	(org-element-property :end (org-element-at-point)))))
  ;; Footnote starting with special syntax.
  (should-not
   (org-test-with-temp-text "[fn:1] <point>- no item"
     (eq (org-element-type (org-element-at-point)) 'item)))
  ;; Correctly handle footnote starting with an empty line.
  (should
   (= 9
      (org-test-with-temp-text "[fn:1]\n\n  Body"
	(org-element-property :contents-begin (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "[fn:1] Definition\n "
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  ;; Footnote with attributes.
  (should
   (= 1
      (org-test-with-temp-text "#+attr_latex: :offset 0in\n[fn:1] A footnote."
	(length
	 (org-element-map (org-element-parse-buffer) 'footnote-definition
	   #'identity)))))
  (should
   (org-test-with-temp-text "[fn:1] 1\n\n#+attr_latex: :offset 0in\n[fn:2] 2"
     (goto-char (org-element-property :end (org-element-at-point)))
     (looking-at "#")))
  ;; An empty footnote has no contents.
  (should-not
   (org-test-with-temp-text "[fn:1]\n\n"
     (let ((footnote (org-element-at-point)))
       (or (org-element-property :contents-begin footnote)
	   (org-element-property :contents-end footnote)))))
  ;; Parse `:pre-blank'.
  (should
   (= 0
      (org-test-with-temp-text "[fn:1] A"
	(org-element-property :pre-blank (org-element-at-point)))))
  (should
   (= 1
      (org-test-with-temp-text "[fn:1]\nA"
	(org-element-property :pre-blank (org-element-at-point)))))
  (should
   (= 2
      (org-test-with-temp-text "[fn:1]\n\nA"
	(org-element-property :pre-blank (org-element-at-point))))))


;;;; Footnotes Reference.

(ert-deftest test-org-element/footnote-reference-parser ()
  "Test `footnote-reference' parser."
  ;; Parse a standard reference.
  (should
   (org-test-with-temp-text "Text[fn:label]"
     (org-element-map
	 (org-element-parse-buffer) 'footnote-reference 'identity)))
  ;; Parse an inline reference.
  (should
   (org-test-with-temp-text "Text[fn:test:def]"
     (org-element-map
	 (org-element-parse-buffer) 'footnote-reference 'identity)))
  ;; Parse an anonymous reference.
  (should
   (org-test-with-temp-text "Text[fn::def]"
     (org-element-map
	 (org-element-parse-buffer) 'footnote-reference 'identity)))
  ;; Parse inline references with syntax loaded characters.
  (should
   (eq 'footnote-reference
       (org-test-with-temp-text "Text[fn<point>::(def]"
         (org-element-type (org-element-context)))))
  (should
   (eq 'footnote-reference
       (org-test-with-temp-text "Text[fn<point>::\"def]"
         (org-element-type (org-element-context)))))
  ;; Parse nested footnotes.
  (should
   (= 2
      (length
       (org-test-with-temp-text "Text[fn::def [fn:label]]"
	 (org-element-map
	     (org-element-parse-buffer) 'footnote-reference 'identity)))))
  ;; Parse adjacent footnotes.
  (should
   (org-test-with-temp-text "Text[fn:label1][fn:label2]"
     (= 2
	(length
	 (org-element-map
	     (org-element-parse-buffer) 'footnote-reference 'identity)))))
  ;; Only properly closed footnotes are recognized as such.
  (should-not
   (org-test-with-temp-text "Text[fn:label"
     (org-element-map
	 (org-element-parse-buffer) 'footnote-reference 'identity))))


;;;; Headline

(ert-deftest test-org-element/headline-todo-keyword ()
  "Test todo keyword recognition."
  ;; Reference test.
  (org-test-with-temp-text "* TODO Headline"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (should (org-element-property :todo-keyword (org-element-at-point)))))
  ;; Todo keyword is prefix of headlines first word.
  (org-test-with-temp-text "* TODOHeadline"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (should-not (org-element-property :todo-keyword (org-element-at-point)))))
  (org-test-with-temp-text "* TODO"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (should (org-element-property :todo-keyword (org-element-at-point)))))
  (org-test-with-temp-text "* :tag:"
    (should (member "tag" (org-element-property :tags (org-element-at-point)))))
  (org-test-with-temp-text "* COMMENT"
    (should (org-element-property :commentedp (org-element-at-point))))
  (org-test-with-temp-text "* COMMENT title"
    (should (equal "title" (org-element-property :raw-value (org-element-at-point)))))
  (org-test-with-temp-text "* COMMENT:tag:"
    (should-not (org-element-property :commentedp (org-element-at-point)))))

(ert-deftest test-org-element/headline-comment-keyword ()
  "Test COMMENT keyword recognition."
  ;; Reference test.
  (org-test-with-temp-text "* Headline"
    (should-not (org-element-property :commentedp (org-element-at-point))))
  ;; Standard position.
  (org-test-with-temp-text "* COMMENT Headline"
    (let ((headline (org-element-at-point)))
      (should (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline) "Headline"))))
  ;; Case sensitivity.
  (org-test-with-temp-text "* Comment Headline"
    (let ((headline (org-element-at-point)))
      (should-not (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline)
		     "Comment Headline"))))
  ;; With another keyword.
  (org-test-with-temp-text "* TODO COMMENT Headline"
    (let* ((org-todo-keywords '((sequence "TODO" "DONE")))
	   (headline (org-element-at-point)))
      (should (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline) "Headline"))))
  ;; With the keyword only.
  (org-test-with-temp-text "* COMMENT"
    (let* ((headline (org-element-at-point)))
      (should (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline) "")))))

(ert-deftest test-org-element/headline-archive-tag ()
  "Test ARCHIVE tag recognition."
  ;; Reference test.
  (should-not
   (org-test-with-temp-text "* Headline"
     (org-element-property :archivedp (org-element-at-point))))
  ;; Single tag.
  (org-test-with-temp-text "* Headline :ARCHIVE:"
    (let ((headline (org-element-at-point)))
      (should (org-element-property :archivedp headline))))
  ;; Multiple tags.
  (org-test-with-temp-text "* Headline :test:ARCHIVE:"
    (let ((headline (org-element-at-point)))
      (should (org-element-property :archivedp headline))))
  ;; Tag is case-sensitive.
  (should-not
   (org-test-with-temp-text "* Headline :Archive:"
     (org-element-property :archivedp (org-element-at-point)))))

(ert-deftest test-org-element/headline-properties ()
  "Test properties from property drawer."
  ;; All properties from property drawer have their symbol upper
  ;; cased.
  (should
   (org-test-with-temp-text "* Headline\n:PROPERTIES:\n:foo: bar\n:END:"
     (org-element-property :FOO (org-element-at-point))))
  (should-not
   (org-test-with-temp-text "* Headline\n:PROPERTIES:\n:foo: bar\n:END:"
     (org-element-property :foo (org-element-at-point))))
  ;; Also parse properties associated in inlinetasks.
  (when (featurep 'org-inlinetask)
    (should
     (org-test-with-temp-text "*************** Inlinetask
:PROPERTIES:
:foo: bar
:END:
*************** END"
       (org-element-property :FOO (org-element-at-point)))))
  ;; Do not find property drawer in a verbatim area.
  (should-not
   (org-test-with-temp-text
       "* Headline
#+BEGIN_EXAMPLE
:PROPERTIES:
:foo: bar
:END:
#+END_EXAMPLE"
     (org-element-property :FOO (org-element-at-point))))
  ;; Do not use properties from a drawer associated to an inlinetask.
  (when (featurep 'org-inlinetask)
    (should-not
     (org-test-with-temp-text
	 "* Headline
*************** Inlinetask
:PROPERTIES:
:foo: bar
:END:
*************** END"
       (org-element-property
	:FOO (let ((org-inlinetask-min-level 15)) (org-element-at-point))))))
  ;; Do not find incomplete drawers.
  (should-not
   (org-test-with-temp-text "* Headline\n:PROPERTIES:\n:foo: bar"
     (org-element-property :FOO (org-element-at-point)))))


;;;; Horizontal Rule

(ert-deftest test-org-element/horizontal-rule-parser ()
  "Test `horizontal-rule' parser."
  ;; Standard.
  (should
   (org-test-with-temp-text "-----"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; Indented.
  (should
   (org-test-with-temp-text "  -----"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; Hyphen must be alone on the line.
  (should-not
   (org-test-with-temp-text "-----wrong"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; 4 hyphens is too small.
  (should-not
   (org-test-with-temp-text "----"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "-----\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Inline Babel Call

(ert-deftest test-org-element/inline-babel-call-parser ()
  "Test `inline-babel-call' parser."
  ;; Standard test.
  (should
   (eq 'inline-babel-call
       (org-test-with-temp-text "call_test()"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'inline-babel-call
       (org-test-with-temp-text "call_test[:results output](x=2)[:results html]"
	 (org-element-type (org-element-context)))))
  ;; Parse call name.
  (should
   (equal
    "test"
    (org-test-with-temp-text "call_test[:results output](x=2)[:results html]"
      (org-element-property :call (org-element-context)))))
  ;; Parse inside header.
  (should
   (equal
    ":results output"
    (org-test-with-temp-text "call_test[:results output](x=2)[:results html]"
      (org-element-property :inside-header (org-element-context)))))
  ;; Parse arguments.
  (should
   (equal
    "x=2"
    (org-test-with-temp-text "call_test[:results output](x=2)[:results html]"
      (org-element-property :arguments (org-element-context)))))
  ;; Parse end header.
  (should
   (equal
    ":results html"
    (org-test-with-temp-text "call_test[:results output](x=2)[:results html]"
      (org-element-property :end-header (org-element-context)))))
  ;; Handle multi-line babel calls.
  (should
   (eq 'inline-babel-call
       (org-test-with-temp-text
	   "call_test[:results\noutput](x=2)[:results html]"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'inline-babel-call
       (org-test-with-temp-text
	   "call_test[:results output](x=2\ny=3)[:results html]"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'inline-babel-call
       (org-test-with-temp-text
	   "call_test[:results output](x=2)[:results\nhtml]"
	 (org-element-type (org-element-context)))))
  ;; Parse parameters containing round brackets.
  (should
   (eq 'inline-babel-call
       (org-test-with-temp-text "call_test[:var x='(1)](x=2)"
	 (org-element-type (org-element-context))))))


;;;; Inline Src Block

(ert-deftest test-org-element/inline-src-block-parser ()
  "Test `inline-src-block' parser."
  (should
   (org-test-with-temp-text "src_emacs-lisp{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  ;; With switches.
  (should
   (org-test-with-temp-text "src_emacs-lisp[:foo bar]{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  (should
   (org-test-with-temp-text "src_emacs-lisp[ :foo bar]{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  ;; Empty switches.
  (should
   (org-test-with-temp-text "src_emacs-lisp[]{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  ;; Invalid syntax.
  (should-not
   (org-test-with-temp-text "src_emacs-lisp[]foo{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  (should-not
   (org-test-with-temp-text "foosrc_emacs-lisp[]{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  ;; Invalid language name
  (should-not
   (org-test-with-temp-text "src_emacs-\tlisp{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  ;; Test parsing at the beginning of an item.
  (should
   (org-test-with-temp-text "- src_emacs-lisp{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  ;; Test parsing multi-line source blocks.
  (should
   (eq 'inline-src-block
       (org-test-with-temp-text "src_emacs-lisp{(+ 1\n  1)}"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'inline-src-block
       (org-test-with-temp-text "src_emacs-lisp[\n:foo bar]{(+ 1 1)}"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'inline-src-block
       (org-test-with-temp-text "src_emacs-lisp[:foo\nbar]{(+ 1 1)}"
	 (org-element-type (org-element-context)))))
  ;; Besides curly brackets, ignore any other bracket type.
  (should
   (equal "[foo"
	  (org-test-with-temp-text "src_emacs-lisp{[foo}"
	    (org-element-property :value (org-element-context)))))
  (should
   (equal "foo]"
	  (org-test-with-temp-text "src_emacs-lisp{foo]}"
	    (org-element-property :value (org-element-context)))))
  (should
   (equal "(foo"
	  (org-test-with-temp-text "src_emacs-lisp{(foo}"
	    (org-element-property :value (org-element-context)))))
  (should
   (equal "foo)"
	  (org-test-with-temp-text "src_emacs-lisp{foo)}"
	    (org-element-property :value (org-element-context)))))
  ;; Parse parameters containing square brackets.
  (should
   (eq 'inline-src-block
       (org-test-with-temp-text "src_emacs-lisp[:var table=t[1,1]]{(+ 1 1)}"
	 (org-element-type (org-element-context))))))


;;;; Inlinetask

(ert-deftest test-org-element/inlinetask-parser ()
  "Test `inlinetask' parser."
  (when (featurep 'org-inlinetask)
    (let ((org-inlinetask-min-level 15))
      ;; Regular inlinetask.
      (should
       (eq 'inlinetask
	   (org-test-with-temp-text
	       "*************** Task\nTest\n*************** END"
	     (org-element-type (org-element-at-point)))))
      (should
       (eq 'inlinetask
	   (org-test-with-temp-text
	       "*************** Task\nTest\n***************   END"
	     (org-element-type (org-element-at-point)))))
      ;; Degenerate inlinetask.
      (should
       (eq 'inlinetask
	   (org-test-with-temp-text "*************** Task"
	     (org-element-type (org-element-at-point)))))
      ;; Mixed inlinetasks.
      (should-not
       (org-test-with-temp-text
	   "
*************** Task
*************** Task2
Contents
*************** END"
	 (forward-line)
	 (goto-char (org-element-property :end (org-element-at-point)))
	 (eobp)))
      ;; TODO keyword.
      (should
       (equal
	"TODO"
	(let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	  (org-test-with-temp-text "*************** TODO Task"
	    (org-element-property :todo-keyword (org-element-at-point))))))
      ;; Planning info.
      (should
       (org-test-with-temp-text "
*************** Task<point>
DEADLINE: <2012-03-29 thu.>
*************** END"
	 (org-element-property :deadline (org-element-at-point))))
      (should
       (eq 'planning
	   (org-test-with-temp-text "
*************** Task
<point>DEADLINE: <2012-03-29 thu.>
*************** END"
	     (org-element-type (org-element-at-point)))))
      (should-not
       (org-test-with-temp-text "
*************** Task<point>
DEADLINE: <2012-03-29 thu.>"
	 (org-element-property :deadline (org-element-at-point))))
      (should-not
       (eq 'planning
	   (org-test-with-temp-text "
*************** Task
<point>DEADLINE: <2012-03-29 thu.>"
	     (org-element-type (org-element-at-point)))))
      ;; Priority.
      (should
       (eq
	?A
	(org-test-with-temp-text "
*************** [#A] Task"
	  (forward-line)
	  (org-element-property :priority (org-element-at-point)))))
      ;; Tags.
      (should
       (equal
	'("test")
	(org-test-with-temp-text "
*************** Task :test:"
	  (forward-line)
	  (org-element-property :tags (org-element-at-point)))))
      ;; Regular properties are accessed through upper case keywords.
      (should
       (org-test-with-temp-text "
*************** Task
:PROPERTIES:
:foo: bar
:END:
*************** END"
	 (forward-line)
	 (org-element-property :FOO (org-element-at-point))))
      (should-not
       (org-test-with-temp-text "
*************** Task
:PROPERTIES:
:foo: bar
:END:
*************** END"
	 (forward-line)
	 (org-element-property :foo (org-element-at-point))))
      ;; Handle non-empty blank line at the end of buffer.
      (should
       (org-test-with-temp-text "*************** Task\n*************** END\n "
	 (= (org-element-property :end (org-element-at-point)) (point-max)))))))


;;;; Italic

(ert-deftest test-org-element/italic-parser ()
  "Test `italic' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "/italic/"
     (org-element-map (org-element-parse-buffer) 'italic #'identity nil t)))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (org-test-with-temp-text "/first line\nsecond line/"
       (org-element-map (org-element-parse-buffer) 'italic #'identity nil t)))
    '("first line\nsecond line"))))


;;;; Item

(ert-deftest test-org-element/item-parser ()
  "Test `item' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "- item"
     (org-element-map (org-element-parse-buffer) 'item 'identity)))
  ;; Counter.
  (should
   (= 6
      (org-element-property
       :counter
       (org-test-with-temp-text "6. [@6] item"
	 (org-element-map (org-element-parse-buffer) 'item 'identity nil t)))))
  ;; Tag
  (should
   (equal
    '("tag")
    (org-element-property
     :tag
     (org-test-with-temp-text "- tag :: description"
       (org-element-map (org-element-parse-buffer) 'item 'identity nil t)))))
  ;; No tags in ordered lists.
  (should-not
   (org-element-property
    :tag
    (org-test-with-temp-text "1. tag :: description"
      (org-element-map (org-element-parse-buffer) 'item 'identity nil t))))
  ;; Check-boxes
  (should
   (equal
    '(trans on off)
    (org-test-with-temp-text "
- [-] item 1
  - [X] item 1.1
  - [ ] item 1.2"
      (org-element-map (org-element-parse-buffer) 'item
	(lambda (item) (org-element-property :checkbox item))))))
  ;; Item starting with special syntax.
  (should
   (equal '(("- item"))
	  (org-test-with-temp-text "- - item"
	    (org-element-map (org-element-parse-buffer) 'paragraph
	      'org-element-contents))))
  ;; Block in an item: ignore indentation within the block.
  (should
   (org-test-with-temp-text
       "-<point> item\n  #+begin_src emacs-lisp\n(+ 1 1)\n  #+end_src"
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  ;; Parse `:pre-blank'.
  (should
   (= 0
      (org-test-with-temp-text "-<point> A"
	(org-element-property :pre-blank (org-element-at-point)))))
  (should
   (= 1
      (org-test-with-temp-text "-<point>\n  A"
	(org-element-property :pre-blank (org-element-at-point)))))
  (should
   (= 2
      (org-test-with-temp-text "-<point>\n\n  A"
	(org-element-property :pre-blank (org-element-at-point)))))
  ;; Last item in a list or sub-list has no `:post-blank' lines, since
  ;; those belong to the plain-list.
  (should
   (= 0
      (org-test-with-temp-text "- A\n\n- <point>B\n\nEnd list"
	(org-element-property :post-blank (org-element-at-point)))))
  (should
   (= 0
      (org-test-with-temp-text "- A\n\n  - B\n\n<point>  - C\n\n  End sub-list"
	(org-element-property :post-blank (org-element-at-point)))))
  (should
   (= 0
      (org-test-with-temp-text "1. foo\n   1. bar\n  2.<point>  baz\n\n2. lorem\nipsum"
	(org-element-property :post-blank (org-element-at-point))))))


;;;; Keyword

(ert-deftest test-org-element/keyword-parser ()
  "Test `keyword' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+KEYWORD: value"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Keywords are case-insensitive.
  (should
   (org-test-with-temp-text "#+keyword: value"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Affiliated keywords are not keywords.
  (should-not
   (org-test-with-temp-text "#+NAME: value
Paragraph"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Do not mix keywords with Babel calls and dynamic blocks.
  (should-not
   (org-test-with-temp-text "#+CALL: fun()"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  (should-not
   (org-test-with-temp-text "#+BEGIN: my-fun\nBody\n#+END:"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+KEYWORD: value\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; LaTeX Environment

(ert-deftest test-org-element/latex-environment-parser ()
  "Test `latex-environment' parser."
  (should
   (org-test-with-temp-text "\\begin{equation}\ne^{i\\pi}+1=0\n\\end{equation}"
     (org-element-map (org-element-parse-buffer) 'latex-environment 'identity)))
  ;; Allow nested environments.
  (should
   (equal
    "\\begin{outer}
\\begin{inner}
e^{i\\pi}+1=0
\\end{inner}
\\end{outer}"
    (org-test-with-temp-text "
\\begin{outer}
\\begin{inner}
e^{i\\pi}+1=0
\\end{inner}
\\end{outer}"
      (org-element-property
       :value
       (org-element-map
	   (org-element-parse-buffer) 'latex-environment 'identity nil t)))))
  ;; Allow environments with options and arguments.
  (should
   (eq 'latex-environment
       (org-test-with-temp-text
	   "\\begin{theorem}[Euler]\ne^{i\\pi}+1=0\n\\end{theorem}"
	 (org-element-type (org-element-at-point)))))
  (should
   (eq 'latex-environment
       (org-test-with-temp-text "\\begin{env}{arg}\nvalue\n\\end{env}"
	 (org-element-type (org-element-at-point)))))
  ;; Allow environments without newline after \begin{.}.
  (should
   (eq 'latex-environment
       (org-test-with-temp-text "\\begin{env}{arg}something\nvalue\n\\end{env}"
	 (org-element-type (org-element-at-point)))))
  ;; Allow one-line environments.
  (should
   (eq 'latex-environment
       (org-test-with-temp-text "\\begin{env}{arg}something\\end{env}"
	 (org-element-type (org-element-at-point)))))
  ;; Should not allow different tags.
  (should-not
   (eq 'latex-environment
       (org-test-with-temp-text "\\begin{env*}{arg}something\\end{env}"
				(org-element-type (org-element-at-point)))))
  ;; LaTeX environments must be on separate lines.
  (should-not
   (eq 'latex-environment
       (org-test-with-temp-text "\\begin{env} x \\end{env} y"
	 (org-element-type (org-element-at-point)))))
  (should-not
   (eq 'latex-environment
       (org-test-with-temp-text "y \\begin{env} x<point> \\end{env}"
	 (org-element-type (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "\\begin{env}\n\\end{env}\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; LaTeX Fragment

(ert-deftest test-org-element/latex-fragment-parser ()
  "Test `latex-fragment' parser."
  ;; Basic $...$ test.
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$"
	 (org-element-type (org-element-context)))))
  ;; Test valid characters after $...$ construct.
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$a"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$!"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$,"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$\""
	 (org-element-type (org-element-context)))))
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$)"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$ "
	 (org-element-type (org-element-context)))))
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$a$'"
	 (org-element-type (org-element-context)))))
  ;; Test forbidden characters inside $...$.
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$.a$"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$,a$"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$;a$"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$ a$"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$a.$"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$a,$"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'latex-fragment
       (org-test-with-temp-text "$a $"
	 (org-element-type (org-element-context)))))
  ;; Test $$...$$.
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "$$a$$"
	 (org-element-type (org-element-context)))))
  ;; Test \(...\).
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "\\(a\\)"
	 (org-element-type (org-element-context)))))
  ;; Test \[...\].
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "\\[a\\]"
	 (org-element-type (org-element-context)))))
  ;; Test fragment at the beginning of an item.
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "- $<point>x$"
	 (org-element-type (org-element-context))))))


;;;; Line Break

(ert-deftest test-org-element/line-break-parser ()
  "Test `line-break' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "Text \\\\"
     (org-element-map (org-element-parse-buffer) 'line-break 'identity)))
  ;; Line break with trailing white spaces.
  (should
   (org-test-with-temp-text "Text \\\\  "
     (org-element-map (org-element-parse-buffer) 'line-break 'identity)))
  ;; Three backslashes are too much.
  (should-not
   (org-test-with-temp-text "Text \\\\\\"
     (org-element-map (org-element-parse-buffer) 'line-break 'identity))))


;;;; Link

(ert-deftest test-org-element/link-parser ()
  "Test `link' parser."
  ;; Radio target.
  (should
   (equal
    "radio"
    (org-test-with-temp-text "<<<radio>>>A radio link"
      (org-update-radio-target-regexp)
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link #'identity nil t)))))
  (should
   (equal
    "radio"
    (org-test-with-temp-text "<<<radio>>><<<radio2>>><<<foo>>>A radio link"
      (org-update-radio-target-regexp)
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link #'identity nil t)))))
  (should
   (equal
    "radio"
    (let ((org-target-link-regexp-limit 9))
      (org-test-with-temp-text "<<<radio>>><<<radio2>>><<<foo>>>A radio link"
        (org-update-radio-target-regexp)
        (org-element-property
         :type
         (org-element-map (org-element-parse-buffer) 'link #'identity nil t))))))
  ;; Pathological case: radio target of length 1 at beginning of line
  ;; not followed by spaces.
  (should
   (org-test-with-temp-text "* <<<a>>>\n<point>a-bug"
     (org-update-radio-target-regexp)
     (org-element-parse-buffer)))
  ;; Pathological case: radio target in an emphasis environment.
  (should
   (eq 'bold
       (org-test-with-temp-text "* <<<radio>>>\n<point>*radio*"
	 (org-update-radio-target-regexp)
	 (org-element-type (org-element-context)))))
  (should
   (eq 'link
       (org-test-with-temp-text "* <<<radio>>>\n*<point>radio*"
	 (org-update-radio-target-regexp)
	 (org-element-type (org-element-context)))))
  ;; Standard link.
  ;;
  ;; ... with description.
  (should
   (equal
    '("Orgmode.org")
    (org-test-with-temp-text "[[https://orgmode.org][Orgmode.org]]"
      (org-element-contents
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... without description.
  (should
   (equal
    "https"
    (org-test-with-temp-text "[[https://orgmode.org]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... with expansion.
  (should
   (equal
    "//orgmode.org/worg"
    (org-test-with-temp-text "[[Org:worg]]"
      (let ((org-link-abbrev-alist '(("Org" . "https://orgmode.org/"))))
	(org-element-property
	 :path
	 (org-element-map (org-element-parse-buffer) 'link 'identity nil t))))))
  ;; ... with translation.
  (should
   (equal
    "127.0.0.1"
    (org-test-with-temp-text "[[https://orgmode.org]]"
      (let ((org-link-translation-function
	     (lambda (type _) (cons type "127.0.0.1"))))
	(org-element-property
	 :path
	 (org-element-map (org-element-parse-buffer) 'link
	   #'identity nil t))))))
  ;; ... custom-id link.
  (should
   (equal
    "custom-id"
    (org-test-with-temp-text "[[#some-id]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... coderef link.
  (should
   (equal
    "coderef"
    (org-test-with-temp-text "[[(reference)]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... fuzzy link.
  (should
   (equal
    "fuzzy"
    (org-test-with-temp-text "[[target-or-title]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... file-type link with search option.
  (should
   (equal
    '(("file" "projects.org" "*task title"))
    (org-test-with-temp-text "[[file:projects.org::*task title]]"
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (l) (list (org-element-property :type l)
		     (org-element-property :path l)
		     (org-element-property :search-option l)))))))
  ;; ... file-type link with application...
  (should
   (equal
    '("file" "projects.org" "emacs")
    (org-test-with-temp-text "[[file+emacs:projects.org]]"
      (let ((l (org-element-context)))
	(list (org-element-property :type l)
	      (org-element-property :path l)
	      (org-element-property :application l))))))
  ;; ... `:path' in a file-type link must be compatible with "file"
  ;; scheme in URI syntax, even if Org syntax isn't...
  (should
   (org-test-with-temp-text-in-file ""
     (let ((file (expand-file-name (buffer-file-name))))
       (insert (format "[[file://%s]]" file))
       (equal (org-element-property :path (org-element-context)) file))))
  (should
   (org-test-with-temp-text-in-file ""
     (let ((file (expand-file-name (buffer-file-name))))
       (insert (format "[[file:%s]]" file))
       (equal (org-element-property :path (org-element-context)) file))))
  ;; ... multi-line link.
  (should
   (equal "ls *.org"
	  (org-test-with-temp-text "[[shell:ls\n*.org]]"
	    (org-element-property :path (org-element-context)))))
  ;; Plain link.
  (should
   (org-test-with-temp-text "A link: https://orgmode.org"
     (org-element-map (org-element-parse-buffer) 'link 'identity)))
  ;; Angular link.  Follow RFC 3986.
  (should
   (eq 'link
       (org-test-with-temp-text "A link: <point><https://orgmode.org>"
	 (org-element-type (org-element-context)))))
  (should
   (equal "//orgmode.org"
	  (org-test-with-temp-text "A link: <point><https://orgmode\n.org>"
	    (org-element-property :path (org-element-context)))))
  ;; Link abbreviation.
  (should
   (equal "https"
	  (org-test-with-temp-text
	      "#+LINK: orgmode https://www.orgmode.org/\n[[orgmode:#docs]]"
	    (progn (org-mode-restart)
		   (goto-char (1- (point-max)))
		   (org-element-property :type (org-element-context))))))
  ;; Link abbreviation with spaces.
  (should
   (equal "https"
          (org-test-with-temp-text
              "#+LINK: \"Nu Html Checker\" https://validator.w3.org/nu/?doc=%h
              [[Nu Html Checker:test]]"
	    (progn (org-mode-restart)
		   (goto-char (1- (point-max)))
		   (org-element-property :type (org-element-context))))))
  ;; Link abbreviation in a secondary string.
  (should
   (equal "https"
	  (org-test-with-temp-text
	      "#+LINK: orgmode https://www.orgmode.org/\n* H [[orgmode:#docs]]"
	    (progn (org-mode-restart)
		   (org-element-map (org-element-parse-buffer) 'link
		     (lambda (link) (org-element-property :type link))
		     nil t nil t))))))


;;;; Macro

(ert-deftest test-org-element/macro-parser ()
  "Test `macro' parser."
  ;; Without arguments.
  (should
   (org-test-with-temp-text "{{{macro}}}"
     (org-element-map (org-element-parse-buffer) 'macro 'identity)))
  ;; With arguments.
  (should
   (org-test-with-temp-text "{{{macro(arg1,arg2)}}}"
     (org-element-map (org-element-parse-buffer) 'macro 'identity)))
  ;; Properly handle protected commas in arguments...
  (should
   (= 2
      (length
       (org-test-with-temp-text "{{{macro(arg1\\,arg1,arg2)}}}"
	 (org-element-property :args (org-element-context))))))
  ;; ... even when last argument ends with a protected comma.
  (should
   (equal '("C-,")
	  (org-test-with-temp-text "{{{macro(C-\\,)}}}"
	    (org-element-property :args (org-element-context)))))
  ;; Allow to escape escaping character.
  (should
   (equal '("C-\\" "")
	  (org-test-with-temp-text "{{{macro(C-\\\\,)}}}"
	    (org-element-property :args (org-element-context)))))
  ;; No need to escape backslashes elsewhere.
  (should
   (equal '("\\")
	  (org-test-with-temp-text "{{{macro(\\)}}}"
	    (org-element-property :args (org-element-context))))))


;;;; Node Property

(ert-deftest test-org-element/node-property ()
  "Test `node-property' parser."
  ;; Standard test.
  (should
   (equal '("abc" "value")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:abc: value\n:END:"
	    (let ((element (org-element-at-point)))
	      (list (org-element-property :key element)
		    (org-element-property :value element))))))
  ;; The insides of property blocks on document level are parsed the
  ;; same way as headline property blocks.  I.e. the concept of
  ;; `node-property' apply also for properties in those blocks.
  (should
   (equal '("abc" "value")
	  (org-test-with-temp-text ":PROPERTIES:\n<point>:abc: value\n:END:"
	    (let ((element (org-element-at-point)))
	      (list (org-element-property :key element)
		    (org-element-property :value element))))))
  ;; Value should be trimmed.
  (should
   (equal "value"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n<point>:abc: value  \n:END:"
	    (org-element-property :value (org-element-at-point)))))
  ;; A node property requires to be wrapped within a property drawer.
  (should-not
   (eq 'node-property
       (org-test-with-temp-text ":abc: value"
	 (org-element-type (org-element-at-point)))))
  ;; Accept empty properties.
  (should
   (equal '(("foo" "value") ("bar" ""))
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:foo: value\n:bar:\n:END:"
	    (org-element-map (org-element-parse-buffer) 'node-property
	      (lambda (p)
		(list (org-element-property :key p)
		      (org-element-property :value p))))))))


;;;; Paragraph

(ert-deftest test-org-element/paragraph-parser ()
  "Test `paragraph' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "Paragraph"
     (org-element-map (org-element-parse-buffer) 'paragraph 'identity nil t)))
  ;; Property find end of a paragraph stuck to another element.
  (should
   (eq ?#
       (org-test-with-temp-text "Paragraph\n# Comment"
	 (org-element-map (org-element-parse-buffer) 'paragraph
	   (lambda (p) (char-after (org-element-property :end p)))
	   nil t))))
  ;; Include ill-formed Keywords.
  (should
   (org-test-with-temp-text "#+wrong_keyword something"
     (org-element-map (org-element-parse-buffer) 'paragraph 'identity)))
  ;; Include incomplete-drawers.
  (should
   (org-test-with-temp-text "<point>:TEST:\nParagraph"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  (should
   (org-test-with-temp-text "<point>foo\n:end:\nbar"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  ;; Include incomplete blocks.
  (should
   (org-test-with-temp-text "<point>#+BEGIN_CENTER\nParagraph"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  (should
   (org-test-with-temp-text "<point>foo\n#+END_CENTER\nbar"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  ;; Include incomplete latex environments.
  (should
   (org-test-with-temp-text "<point>\begin{equation}\nParagraph"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  (should
   (org-test-with-temp-text "<point>Paragraph\n\begin{equation}"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  ;; Stop at affiliated keywords.
  (should
   (org-test-with-temp-text "Paragraph\n#+NAME: test\n| table |"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (not (org-element-property :name elem))
	    (= (org-element-property :end elem) (line-beginning-position 2))))))
  (should
   (org-test-with-temp-text
       "Paragraph\n#+CAPTION[with short caption]: test\n| table |"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (not (org-element-property :name elem))
	    (= (org-element-property :end elem) (line-beginning-position 2))))))
  ;; Do not steal affiliated keywords from container.
  (should
   (org-test-with-temp-text "#+ATTR_LATEX: test\n- item<point> 1"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (not (org-element-property :attr_latex elem))
	    (/= (org-element-property :begin elem) 1)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nC\n#+END_CENTER\n  "
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\n<point>C\n#+END_CENTER\n  "
     (= (org-element-property :end (org-element-at-point))
        (save-excursion
          (search-forward "END")
          (line-beginning-position))))))

;;;; Plain List

(ert-deftest test-org-element/plain-list-parser ()
  "Test `plain-list' parser."
  (org-test-with-temp-text "- item"
    (should (org-element-map (org-element-parse-buffer) 'plain-list 'identity)))
  ;; Blank lines after a list or sub-list belongs to that list.
  (should
   (= 1
      (org-test-with-temp-text "- A\n\n- B\n\nEnd list"
	(org-element-property :post-blank (org-element-at-point)))))
  (should
   (= 1
      (org-test-with-temp-text "- A\n\n<point>  - B\n\n  - C\n\n  End sub-list"
	(org-element-property :post-blank (org-element-at-point)))))
  ;; Blank lines after the list only belong to outer plain list,
  ;; however.
  (should
   (equal
    '(t t)
    (org-test-with-temp-text "
- outer
  - inner

Outside list"
      (let ((endings (org-element-map (org-element-parse-buffer) 'plain-list
		       (lambda (pl) (org-element-property :end pl)))))
	(list
	 ;; Move to ending of outer list.
	 (progn (goto-char (car endings)) (looking-at "Outside list"))
	 ;; Move to ending of inner list.
	 (progn (goto-char (nth 1 endings)) (looking-at "^$")))))))
  ;; Correctly compute end of list if it doesn't end at a line
  ;; beginning.
  (should
   (org-test-with-temp-text "- list\n   \n   "
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  ;; Correctly compute list ending when list is before first headline.
  (dolist (org-element-use-cache '(t nil))
    (org-test-with-temp-text "- list\n* Headline\n"
      (should (= (org-element-property :end (org-element-at-point)) 8)))))


;;;; Planning

(ert-deftest test-org-element/planning-parser ()
  "Test `planning' parser."
  ;; Test various keywords.
  (should
   (org-element-property
    :closed
    (org-test-with-temp-text "* H\n<point>CLOSED: [2012-03-29 thu.]"
      (org-element-at-point))))
  (should
   (org-element-property
    :deadline
    (org-test-with-temp-text "* H\n<point>DEADLINE: <2012-03-29 thu.>"
      (org-element-at-point))))
  (should
   (org-element-property
    :scheduled
    (org-test-with-temp-text "* H\n<point>SCHEDULED: <2012-03-29 thu.>"
      (org-element-at-point))))
  ;; Planning line only exists right after a headline.
  (should-not
   (eq 'planning
       (org-test-with-temp-text "DEADLINE: <2012-03-29 thu.>"
	 (org-element-type (org-element-at-point)))))
  (should-not
   (eq 'planning
       (org-test-with-temp-text
	   "* H\n# Comment\n<point>DEADLINE: <2012-03-29 thu.>"
	 (org-element-type (org-element-at-point)))))
  (should-not
   (eq 'planning
       (org-test-with-temp-text
	   "* H\n\n<point>DEADLINE: <2012-03-29 thu.>"
	 (org-element-type (org-element-at-point))))))


;;;; Property Drawer

(ert-deftest test-org-element/property-drawer-parser ()
  "Test `property-drawer' parser."
  ;; Standard test.
  (should
   (eq 'property-drawer
       (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  (should
   (eq 'property-drawer
       (org-test-with-temp-text
	   "* H\nDEADLINE: <2014-03-04 tue.>\n<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  ;; Parse property drawer at the beginning of the document, possibly
  ;; after some initial comments.
  (should
   (eq 'property-drawer
       (org-test-with-temp-text "<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  (should
   (eq 'property-drawer
       (org-test-with-temp-text "# C\n# C\n<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  (should
   (eq 'property-drawer
       (org-test-with-temp-text "\n  \t\n# C\n# C\n  \n\n\n<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  (should
   (eq 'property-drawer
       (org-test-with-temp-text "\n<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  ;; Allow properties without value and no property at all.
  (should
   (eq 'property-drawer
       (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:prop:\n:END:"
	 (org-element-type (org-element-at-point)))))
  (should
   (eq 'property-drawer
       (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:END:"
	 (org-element-type (org-element-at-point)))))
  ;; Ignore incomplete drawer, drawer at a wrong location or with
  ;; wrong contents.
  (should-not
   (eq 'property-drawer
       (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:prop: value"
	 (org-element-type (org-element-at-point)))))
  (should-not
   (eq 'property-drawer
       (org-test-with-temp-text
	   "* H\nParagraph\n<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  (should-not
   (eq 'property-drawer
       (org-test-with-temp-text
	   "* H\nParagraph\n<point>:PROPERTIES:\nparagraph\n:END:"
	 (org-element-type (org-element-at-point)))))
  (should-not
   (eq 'property-drawer
       (org-test-with-temp-text
	   "* H\n\n<point>:PROPERTIES:\n:prop: value\n:END:"
	 (org-element-type (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:END:\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Quote Block

(ert-deftest test-org-element/quote-block-parser ()
  "Test `quote-block' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\nText\n#+END_QUOTE"
     (org-element-map (org-element-parse-buffer) 'quote-block 'identity)))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE"
     (org-element-map (org-element-parse-buffer) 'quote-block 'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\nC\n#+END_QUOTE\n  "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Radio Target

(ert-deftest test-org-element/radio-target-parser ()
  "Test `radio-target' parser."
  ;; Standard test.
  (should
   (eq 'radio-target
       (org-test-with-temp-text "<<<radio>>>"
	 (org-element-type (org-element-context)))))
  ;; Radio targets with objects.
  (should
   (eq 'radio-target
       (org-test-with-temp-text "<<<radio \\alpha>>>"
	 (org-element-type (org-element-context)))))
  ;; Radio targets starting with an object.
  (should
   (eq 'radio-target
       (org-test-with-temp-text "<<<\\alpha radio>>>"
	 (org-element-type (org-element-context)))))
  ;; Radio targets cannot begin or end with white space.
  (should-not
   (eq 'radio-target
       (org-test-with-temp-text "<<< radio>>>"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'radio-target
       (org-test-with-temp-text "<<<radio >>>"
	 (org-element-type (org-element-context))))))


;;;; Section

(ert-deftest test-org-element/section-parser ()
  "Test `section' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "* Headline\nText"
     (org-element-map (org-element-parse-buffer) 'section 'identity)))
  ;; There's a section before the first headline.
  (should
   (org-test-with-temp-text "Text"
     (org-element-map (org-element-parse-buffer) 'section 'identity)))
  ;; A section cannot be empty.
  (should-not
   (org-test-with-temp-text "* Headline 1\n* Headline 2"
     (org-element-map (org-element-parse-buffer) 'section 'identity)))
  ;; A section doesn't contain sub-trees.
  (should-not
   (org-test-with-temp-text "* Head\nText\n** Sub-Head"
     (org-element-map
      (org-element-map (org-element-parse-buffer) 'section 'identity nil t)
      'headline 'identity))))


;;;; Special Block

(ert-deftest test-org-element/special-block-parser ()
  "Test `special-block' parser."
  ;; Standard test.
  (should
   (equal "SPECIAL"
	  (org-test-with-temp-text "#+BEGIN_SPECIAL\nText\n#+END_SPECIAL"
	    (org-element-property :type (org-element-at-point)))))
  ;; Special blocks are case sensitive.
  (should
   (equal "CaSe"
	  (org-test-with-temp-text "#+BEGIN_CaSe\nText\n#+END_CaSe"
	    (org-element-property :type (org-element-at-point)))))
  ;; Special blocks can contain paragraphs.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "#+BEGIN_SPECIAL\nText\n#+END_SPECIAL"
	 (forward-line)
	 (org-element-type (org-element-at-point)))))
  ;; Ignore incomplete block.
  (should-not
   (eq 'special-block
       (org-test-with-temp-text "#+BEGIN_SPECIAL"
	 (org-element-type (org-element-at-point)))))
  ;; Allow special characters in type.
  (should
   (equal '(special-block "SPECIAL*")
	  (org-test-with-temp-text "#+BEGIN_SPECIAL*\nContents\n#+END_SPECIAL*"
	    (let ((element (org-element-at-point)))
	      (list (org-element-type element)
		    (org-element-property :type element))))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_SPECIAL\nC\n#+END_SPECIAL\n "
     (= (org-element-property :end (org-element-at-point)) (point-max))))
  ;; When contents is empty, the parsed contents is nil.
  (should
   (org-test-with-temp-text "#+BEGIN_SPECIAL\n#+END_SPECIAL"
     (eq nil (org-element-contents (org-element-at-point)))))
  ;; Parse parameters if any, trimming blanks.
  (should
   (org-test-with-temp-text "#+BEGIN_SPECIAL* s  p   :w 3   \nContent.\n#+END_SPECIAL*"
     (equal "s  p   :w 3"
	    (org-element-property :parameters (org-element-at-point)))))
  ;; When parameters is blank, `:parameters' is nil.
  (should
   (org-test-with-temp-text "#+BEGIN_SPECIAL*     \t   \nContent.\n#+END_SPECIAL*"
     (eq nil (org-element-property :parameters (org-element-at-point))))
   ))




;;;; Src Block

(ert-deftest test-org-element/src-block-parser ()
  "Test `src-block' parser."
  ;; Regular tests.
  (should
   (org-test-with-temp-text "#+BEGIN_SRC org\nText\n#+END_SRC"
     (org-element-map (org-element-parse-buffer) 'src-block 'identity)))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_SRC"
     (org-element-map (org-element-parse-buffer) 'src-block 'identity)))
  ;; Properly un-escape code.
  (should
   (equal "* Headline\n #+keyword\nText\n"
	  (org-test-with-temp-text
	      "#+BEGIN_SRC org\n,* Headline\n ,#+keyword\nText\n#+END_SRC"
	    (org-element-property :value (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\nC\n#+END_SRC\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Statistics Cookie

(ert-deftest test-org-element/statistics-cookie ()
  "Test `statistics-cookie' parser."
  ;; With numbers.
  (should
   (org-test-with-temp-text "[1/2]"
     (org-element-map (org-element-parse-buffer) 'statistics-cookie 'identity)))
  ;; With percents.
  (should
   (org-test-with-temp-text "[33%]"
     (org-element-map
      (org-element-parse-buffer) 'statistics-cookie 'identity))))


;;;; Strike Through

(ert-deftest test-org-element/strike-through-parser ()
  "Test `strike-through' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "+strike-through+"
     (org-element-map (org-element-parse-buffer) 'strike-through #'identity)))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (org-test-with-temp-text "+first line\nsecond line+"
       (org-element-map
	   (org-element-parse-buffer) 'strike-through #'identity nil t)))
    '("first line\nsecond line"))))


;;;; Subscript

(ert-deftest test-org-element/subscript-parser ()
  "Test `subscript' parser."
  ;; Without braces.
  (should
   (org-test-with-temp-text "a_b"
     (org-element-map (org-element-parse-buffer) 'subscript 'identity)))
  ;; With braces.
  (should
   (org-test-with-temp-text "a_{b}"
     (org-element-map (org-element-parse-buffer) 'subscript 'identity)))
  ;; Multiple subscripts in a paragraph.
  (should
   (= 2
      (org-test-with-temp-text "a_b and c_d"
	(length
	 (org-element-map (org-element-parse-buffer) 'subscript 'identity))))))


;;;; Superscript

(ert-deftest test-org-element/superscript-parser ()
  "Test `superscript' parser."
  ;; Without braces.
  (should
   (org-test-with-temp-text "a^b"
     (org-element-map (org-element-parse-buffer) 'superscript 'identity)))
  ;; With braces.
  (should
   (org-test-with-temp-text "a^{b}"
     (org-element-map (org-element-parse-buffer) 'superscript 'identity)))
  ;; Multiple superscript in a paragraph.
  (should
   (= 2
      (org-test-with-temp-text "a^b and c^d"
	(length
	 (org-element-map
	  (org-element-parse-buffer) 'superscript 'identity))))))


;;;; Table

(ert-deftest test-org-element/table-parser ()
  "Test `table' parser."
  (should
   (org-test-with-temp-text "| a |"
     (org-element-map (org-element-parse-buffer) 'table 'identity)))
  ;; TBLFM keyword is case insensitive.
  (should
   (org-test-with-temp-text "| a |\n#+tblfm: test"
     (org-element-property
      :tblfm
      (org-element-map (org-element-parse-buffer) 'table 'identity nil t))))
  ;; Handle multiple TBLFM lines.
  (should
   (= 2
      (org-test-with-temp-text "| a |\n#+TBLFM: test1\n#+TBLFM: test2"
	(length (org-element-property
		 :tblfm
		 (org-element-map
		     (org-element-parse-buffer) 'table 'identity nil t))))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "| a |\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Table Cell

(ert-deftest test-org-element/table-cell-parser ()
  "Test `table-cell' parser."
  ;; Regular table cell.
  (should
   (org-test-with-temp-text "| a |"
     (org-element-map (org-element-parse-buffer) 'table-cell 'identity)))
  ;; Last vertical bar may be omitted.
  (should
   (org-test-with-temp-text "| a "
     (org-element-map (org-element-parse-buffer) 'table-cell 'identity))))


;;;; Table Row

(ert-deftest test-org-element/table-row-parser ()
  "Test `table-row' parser."
  (should
   (equal '(standard rule)
	  (org-test-with-temp-text "| a |\n|---|"
	    (org-element-map
	     (org-element-parse-buffer) 'table-row
	     (lambda (row) (org-element-property :type row)))))))


;;;; Target

(ert-deftest test-org-element/target-parser ()
  "Test `target' parser."
  (should
   (org-test-with-temp-text "<<target>>"
     (org-element-map (org-element-parse-buffer) 'target 'identity))))


;;;; Timestamp

(ert-deftest test-org-element/timestamp-parser ()
  "Test `timestamp' parser."
  ;; Active timestamp.
  (should
   (org-test-with-temp-text "<2012-03-29 16:40>"
     (eq (org-element-property :type (org-element-context)) 'active)))
  (should-not
   (org-test-with-temp-text "<2012-03-29 Thu>"
     (let ((timestamp (org-element-context)))
       (or (org-element-property :hour-start timestamp)
	   (org-element-property :minute-start timestamp)))))
  (should
   (equal '(2012 3 29 16 40)
	  (org-test-with-temp-text "<2012-03-29 Thu 16:40>"
	    (let ((object (org-element-context)))
	      (list (org-element-property :year-start object)
		    (org-element-property :month-start object)
		    (org-element-property :day-start object)
		    (org-element-property :hour-start object)
		    (org-element-property :minute-start object))))))
  ;; Inactive timestamp.
  (should
   (org-test-with-temp-text "[2012-03-29 Thu 16:40]"
     (eq (org-element-property :type (org-element-context)) 'inactive)))
  ;; Time range.
  (should
   (equal '(2012 3 29 16 40 7 30)
	  (org-test-with-temp-text "<2012-03-29 Thu 7:30-16:40>"
	    (let ((object (org-element-context)))
	      (list (org-element-property :year-end object)
		    (org-element-property :month-end object)
		    (org-element-property :day-end object)
		    (org-element-property :hour-end object)
		    (org-element-property :minute-end object)
		    (org-element-property :hour-start object)
		    (org-element-property :minute-start object))))))
  (should
   (eq 'active-range
       (org-test-with-temp-text "<2012-03-29 Thu 7:30-16:40>"
	 (org-element-property :type (org-element-context)))))
  ;; Date range.
  (should
   (org-test-with-temp-text "[2012-03-29 Thu 16:40]--[2012-03-29 Thu 16:41]"
     (eq (org-element-property :type (org-element-context)) 'inactive-range)))
  (should-not
   (org-test-with-temp-text "[2011-07-14 Thu]--[2012-03-29 Thu]"
     (let ((timestamp (org-element-context)))
       (or (org-element-property :hour-end timestamp)
	   (org-element-property :minute-end timestamp)))))
  ;; With repeater, repeater deadline, warning delay and combinations.
  (should
   (eq 'catch-up
       (org-test-with-temp-text "<2012-03-29 Thu ++1y>"
	 (org-element-property :repeater-type (org-element-context)))))
  (should
   (equal '(catch-up 2 year)
       (org-test-with-temp-text "<2012-03-29 Thu ++1y/2y>"
         (let ((ts (org-element-context)))
           (list (org-element-property :repeater-type ts)
                 (org-element-property :repeater-deadline-value ts)
                 (org-element-property :repeater-deadline-unit ts))))))
  (should
   (eq 'first
       (org-test-with-temp-text "<2012-03-29 Thu --1y>"
	 (org-element-property :warning-type (org-element-context)))))
  (should
   (equal '(cumulate all)
	  (org-test-with-temp-text "<2012-03-29 Thu +1y -1y>"
	    (let ((ts (org-element-context)))
	      (list (org-element-property :repeater-type ts)
		    (org-element-property :warning-type ts))))))
  (should
   (equal '(cumulate all 2 year)
          (org-test-with-temp-text "<2012-03-29 Thu +1y/2y -1y>"
            (let ((ts (org-element-context)))
              (list (org-element-property :repeater-type ts)
                    (org-element-property :warning-type ts)
                    (org-element-property :repeater-deadline-value ts)
                    (org-element-property :repeater-deadline-unit ts))))))
  ;; :range-type property
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    nil))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    nil))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00-13:00>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'timerange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00-12:00>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'timerange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun>--<2023-07-02 Sun>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun>--<2023-07-03 Mon>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00>--<2023-07-02 Sun 12:00>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00>--<2023-07-03 Mon 13:00>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00>--<2023-07-02 Sun>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00>--<2023-07-03 Mon>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00>--<2023-07-02 Sun 13:00>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange))
  (should
   (eq
    (org-test-with-temp-text "<2023-07-02 Sun 12:00 +5d>--<2023-07-02 Sun 13:00>"
      (org-element-property :range-type (org-element-timestamp-parser)))
    'daterange)))

;;;; Underline

(ert-deftest test-org-element/underline-parser ()
  "Test `underline' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "_underline_"
     (org-element-map (org-element-parse-buffer) 'underline #'identity)))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (org-test-with-temp-text "_first line\nsecond line_"
       (org-element-map
	   (org-element-parse-buffer) 'underline #'identity nil t)))
    '("first line\nsecond line")))
  ;; Nested underlines.
  (should
   (= 2
      (org-test-with-temp-text "__test__"
	(length
	 (org-element-map (org-element-parse-buffer) 'underline 'identity)))))
  ;; Starting after non-blank
  (should
   (eq 'underline
       (org-test-with-temp-text "(_under<point>line_)"
         (org-element-type (org-element-context)))))
  (should-not
   (eq 'underline
       (org-test-with-temp-text "x_under<point>line_)"
         (org-element-type (org-element-context))))))


;;;; Verbatim

(ert-deftest test-org-element/verbatim-parser ()
  "Test `verbatim' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "=verbatim="
     (org-element-map (org-element-parse-buffer) 'verbatim #'identity)))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-property
     :value
     (org-test-with-temp-text "=first line\nsecond line="
       (org-element-map (org-element-parse-buffer) 'verbatim #'identity nil t)))
    "first line\nsecond line")))


;;;; Verse Block

(ert-deftest test-org-element/verse-block-parser ()
  "Test `verse-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+BEGIN_VERSE\nVerse block\n#+END_VERSE"
     (org-element-map (org-element-parse-buffer) 'verse-block 'identity)))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "#+begin_verse\nVerse block\n#+end_verse"
     (org-element-map (org-element-parse-buffer) 'verse-block 'identity)))
  ;; Parse objects in verse blocks.
  (should
   (org-test-with-temp-text "#+BEGIN_VERSE\nVerse \\alpha\n#+END_VERSE"
     (org-element-map (org-element-parse-buffer) 'entity 'identity)))
  ;; Ignore incomplete verse block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_VERSE"
     (org-element-map (org-element-parse-buffer) 'verse-block 'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_VERSE\nC\n#+END_VERSE\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))

;;; Org data.

(ert-deftest test-org-element/org-data-parser ()
  "Test `org-data' parser."
  ;; Standard test.
  (org-test-with-temp-text "This is test."
    (let ((data (org-element-lineage (org-element-at-point) 'org-data)))
      (should (equal 1 (org-element-begin data)))
      (should (equal (point-max) (org-element-end data)))))
  ;; Parse top-level property drawer.
  (should
   (equal
    "bar"
    (org-test-with-temp-text ":PROPERTIES:
:FOO: bar
:END:"
      (org-element-property-inherited :FOO (org-element-at-point)))))
  ;; With leading comment line.
  (org-test-with-temp-text "# comment
:PROPERTIES:
:FOO: bar
:END:"
    (should (equal "bar" (org-element-property-inherited :FOO (org-element-at-point)))))
  ;; Blank line on top.
  (should
   (equal
    "bar"
    (org-test-with-temp-text "
:PROPERTIES:
:FOO: bar<point>
:END:"
      (org-element-property-inherited :FOO (org-element-at-point))))))


;;; Test Interpreters.

(ert-deftest test-org-element/interpret-data ()
  "Test `org-element-interpret-data' specifications."
  ;; Interpret simple affiliated keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:name "para") "Paragraph")))
    "#+name: para\nParagraph\n"))
  ;; Interpret multiple affiliated keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:attr_ascii ("line1" "line2")) "Paragraph")))
    "#+attr_ascii: line1\n#+attr_ascii: line2\nParagraph\n"))
  ;; Interpret parsed affiliated keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:caption (("caption"))) "Paragraph")))
    "#+caption: caption\nParagraph\n"))
  ;; Interpret dual affiliated keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:caption ((("long") "short"))) "Paragraph")))
    "#+caption[short]: long\nParagraph\n"))
  ;; Interpret multiple parsed dual keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph
		     (:caption ((("l1") "s1") (("l2") "s2"))) "Paragraph")))
    "#+caption[s1]: l1\n#+caption[s2]: l2\nParagraph\n"))
  ;; Pseudo objects and elements are transparent.
  (should
   (equal "A B"
	  (org-trim
	   (org-element-interpret-data
	    '(paragraph nil (pseudo-object (:post-blank 1) "A") "B")))))
  (should
   (equal "A\n\nB\n"
	  (org-element-interpret-data
	   '(center nil
		    (pseudo-element (:post-blank 1) (paragraph nil "A"))
		    (paragraph nil "B")))))
  ;; Obey post-blank property in strings.
  (should
   (equal "A "
	  (org-element-interpret-data
           (org-element-put-property "A" :post-blank 1))))
  ;; Obey post-blank property in elements
  (should
   (equal "Foo\n\n"
	  (org-element-interpret-data
           '(paragraph (:post-blank 1) "Foo"))))
  (should
   (equal "Foo\n\n"
	  (org-element-interpret-data
           '(section nil (paragraph (:post-blank 1) "Foo")))))
  (should
   (equal "Foo\n\n\n"
	  (org-element-interpret-data
           '(section (:post-blank 1) (paragraph (:post-blank 1) "Foo"))))))

(ert-deftest test-org-element/org-data-interpreter ()
  "Test `org-element-org-data-interpreter'."
  (should
   (equal (org-test-parse-and-interpret "Test.\n")
	  "Test.\n"))
  (should
   (equal (org-test-parse-and-interpret "

Test.\n")
	  "\n\nTest.\n")))

(ert-deftest test-org-element/center-block-interpreter ()
  "Test center block interpreter."
  (should
   (equal (org-test-parse-and-interpret "#+BEGIN_CENTER\nTest\n#+END_CENTER")
	  "#+begin_center\nTest\n#+end_center\n")))

(ert-deftest test-org-element/drawer-interpreter ()
  "Test drawer interpreter."
  (should
   (equal (org-test-parse-and-interpret ":TEST:\nTest\n:END:")
	  ":TEST:\nTest\n:END:\n")))

(ert-deftest test-org-element/dynamic-block-interpreter ()
  "Test dynamic block interpreter."
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN: myblock :parameter value1\nTest\n#+END:")
	  "#+begin: myblock :parameter value1\nTest\n#+end:\n")))

(ert-deftest test-org-element/footnote-definition-interpreter ()
  "Test footnote definition interpreter."
  (should (equal (org-test-parse-and-interpret "[fn:1] Test") "[fn:1] Test\n"))
  ;; Handle `:pre-blank' in definitions.
  (should
   (equal (org-test-parse-and-interpret "[fn:1]\nparagraph")
	  "[fn:1]\nparagraph\n"))
  (should
   (equal (org-test-parse-and-interpret "[fn:1]\n\nparagraph")
	  "[fn:1]\n\nparagraph\n")))

(ert-deftest test-org-element/headline-interpreter ()
  "Test headline and section interpreters."
  ;; 1. Standard test.
  (should (equal (org-test-parse-and-interpret "* Headline") "* Headline\n"))
  ;; 2. With TODO keywords.
  (should
   (equal (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	    (org-test-parse-and-interpret "* TODO Headline"))
	  "* TODO Headline\n"))
  ;; 3. With tags...
  ;;
  ;; 3.1. ... and a positive `org-tags-column' value.
  (should
   (equal (let ((org-tags-column 20))
	    (org-test-parse-and-interpret "* Headline :tag:"))
	  "* Headline          :tag:\n"))
  ;; 3.2. ... and a negative `org-tags-column' value.
  (should
   (equal (let ((org-tags-column -20))
	    (org-test-parse-and-interpret "* Headline :tag:"))
	  "* Headline     :tag:\n"))
  ;; 3.3. ... and a null `org-tags-column' value.
  (should
   (equal (let ((org-tags-column 0))
	    (org-test-parse-and-interpret "* Headline     :tag:"))
	  "* Headline :tag:\n"))
  ;; 4. With priority cookie.
  (should
   (equal (org-test-parse-and-interpret "* [#B] Headline")
	  "* [#B] Headline\n"))
  ;; 5. With comment keyword.
  (should
   (equal (let ((org-comment-string "COMMENT"))
	    (org-test-parse-and-interpret "* COMMENT Headline"))
	  "* COMMENT Headline\n"))
  ;; 6. Keep same number of blank lines before body.
  (should
   (equal (org-test-parse-and-interpret
	   "* Headline\n\n\nText after two blank lines.")
	  "* Headline\n\n\nText after two blank lines.\n"))
  ;; 8. Preserve `org-odd-levels-only' state.
  (should
   (equal "* H\n*** H2\n"
	  (let ((org-odd-levels-only t))
	    (org-test-parse-and-interpret "* H\n*** H2")))))

(ert-deftest test-org-element/inlinetask-interpreter ()
  "Test inlinetask interpretation."
  (when (featurep 'org-inlinetask)
    (let ((org-inlinetask-min-level 15))
      ;; 1. Regular inlinetask.
     (should (equal (org-test-parse-and-interpret
		     "*************** Task\nTest\n*************** END")
		    "*************** Task\nTest\n*************** end\n"))
     ;; 2. Degenerate inlinetask.
     (should (equal (org-test-parse-and-interpret "*************** Task")
		    "*************** Task\n"))
     ;; 3. Prefer degenerate form when there are no contents.
     (should (equal (org-test-parse-and-interpret
		     "*************** Task\n*************** end")
		    "*************** Task\n"))
     ;; 4. With TODO keywords.
     (should
      (equal (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	       (org-test-parse-and-interpret "*************** TODO Task"))
	     "*************** TODO Task\n"))
     ;; 5. With tags...
     ;;
     ;; 5.1. ... and a positive `org-tags-column' value.
     (should
      (equal (let ((org-tags-column 30))
	       (org-test-parse-and-interpret "*************** Task :tag:"))
	     "*************** Task          :tag:\n"))
     ;; 5.2. ... and a negative `org-tags-column' value.
     (should
      (equal (let ((org-tags-column -30))
	       (org-test-parse-and-interpret "*************** Task :tag:"))
	     "*************** Task     :tag:\n"))
     ;; 5.3. ... and a null `org-tags-column' value.
     (should
      (equal (let ((org-tags-column 0))
	       (org-test-parse-and-interpret "*************** Task     :tag:"))
	     "*************** Task :tag:\n"))
     ;; 6. With priority cookie.
     (should
      (equal (org-test-parse-and-interpret "*************** [#B] Task")
	     "*************** [#B] Task\n")))))

(ert-deftest test-org-element/plain-list-interpreter ()
  "Test plain-list and item interpreters."
  (let ((org-list-two-spaces-after-bullet-regexp nil))
    ;; Unordered list.
    (should (equal (org-test-parse-and-interpret "- item 1") "- item 1\n"))
    ;; Description list.
    (should
     (equal (org-test-parse-and-interpret "- tag :: desc") "- tag :: desc\n"))
    ;; Ordered list.
    (should
     (equal (let ((org-plain-list-ordered-item-terminator t))
	      (org-test-parse-and-interpret "1. Item"))
	    "1. Item\n"))
    (should
     (equal (let ((org-plain-list-ordered-item-terminator ?\)))
	      (org-test-parse-and-interpret "1) Item"))
	    "1) Item\n"))
    ;; Ordered list with counter.
    (should
     (equal (let ((org-plain-list-ordered-item-terminator t))
	      (org-test-parse-and-interpret "1. [@5] Item"))
	    "5. [@5] Item\n"))
    ;; List with check-boxes.
    (should
     (equal (org-test-parse-and-interpret
	     "- [-] Item 1\n  - [X] Item 2\n  - [ ] Item 3")
	    "- [-] Item 1\n  - [X] Item 2\n  - [ ] Item 3\n"))
    ;; Item not starting with a paragraph.
    (should
     (equal (org-test-parse-and-interpret "-\n  | a | b |")
	    "- \n  | a | b |\n"))
    ;; Handle `:pre-blank' in items.
    (should
     (equal (org-test-parse-and-interpret "-\n  paragraph")
	    "- \n  paragraph\n"))
    (should
     (equal (org-test-parse-and-interpret "-\n\n  paragraph")
	    "- \n\n  paragraph\n"))
    ;; Special case: correctly handle "*" bullets.
    (should (org-test-parse-and-interpret " * item"))
    ;; Special case: correctly handle empty items.
    (should (org-test-parse-and-interpret "-"))))

(ert-deftest test-org-element/quote-block-interpreter ()
  "Test quote block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_QUOTE\nTest\n#+END_QUOTE")
		 "#+begin_quote\nTest\n#+end_quote\n")))

(ert-deftest test-org-element/special-block-interpreter ()
  "Test special block interpreter."
  ;; No parameters
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_SPECIAL\nTest\n#+END_SPECIAL")
		 "#+begin_SPECIAL\nTest\n#+end_SPECIAL\n"))
  ;; No content
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_SPECIAL\n#+END_SPECIAL")
		 "#+begin_SPECIAL\n#+end_SPECIAL\n"))
  ;; Some parameters
  (should
   (equal (org-test-parse-and-interpret
           "#+BEGIN_special some parameters until EOL\nA very special content\n#+END_special")
          "#+begin_special some parameters until EOL\nA very special content\n#+end_special\n"))
  ;; No parameters (blanks only)
  (should
   (equal (org-test-parse-and-interpret
           "#+BEGIN_special   \t \nA very special content\n#+END_special")
          "#+begin_special\nA very special content\n#+end_special\n"))
  ;; Some parameters with leading and trailing blanks, no content, and
  ;; a /special/ name.
  (should
   (equal (org-test-parse-and-interpret
           "#+BEGIN_spe<c>ial  :a  :b \t  :c  \t \n#+END_spe<c>ial")
          "#+begin_spe<c>ial :a  :b \t  :c\n#+end_spe<c>ial\n")))

(ert-deftest test-org-element/babel-call-interpreter ()
  "Test Babel call interpreter."
  ;; Without argument.
  (should (equal (org-test-parse-and-interpret "#+CALL: test()")
		 "#+call: test()\n"))
  ;; With argument.
  (should (equal (org-test-parse-and-interpret "#+CALL: test(x=2)")
		 "#+call: test(x=2)\n"))
  ;; With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "#+CALL: test[:results output]() :results html")
		 "#+call: test[:results output]() :results html\n")))

(ert-deftest test-org-element/clock-interpreter ()
  "Test clock interpreter."
  ;; Running clock.
  (should
   (string-match
    "CLOCK: \\[2012-01-01 .* 00:01\\]"
    (org-test-parse-and-interpret "CLOCK: [2012-01-01 sun. 00:01]")))
  ;; Closed clock.
  (should
   (string-match
    "CLOCK: \\[2012-01-01 .* 00:01\\]--\\[2012-01-01 .* 00:02\\] =>  0:01"
    (org-test-parse-and-interpret "
CLOCK: [2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02] =>  0:01")))
  ;; Closed clock without timestamp.
  (should
   (string-match
    "CLOCK:  =>  0:01"
    (org-test-parse-and-interpret "CLOCK: => 0:01"))))

(ert-deftest test-org-element/comment-interpreter ()
  "Test comment interpreter."
  ;; Regular comment.
  (should (equal (org-test-parse-and-interpret "# Comment") "# Comment\n"))
  ;; Inline comment.
  (should (equal (org-test-parse-and-interpret "  # Comment")
		 "# Comment\n"))
  ;; Preserve indentation.
  (should (equal (org-test-parse-and-interpret "  # No blank\n#  One blank")
		 "# No blank\n#  One blank\n")))

(ert-deftest test-org-element/comment-block-interpreter ()
  "Test comment block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_COMMENT\nTest\n#+END_COMMENT")
		 "#+begin_comment\nTest\n#+end_comment\n"))
  ;; Accept missing final newline in value.
  (should
   (equal "#+begin_comment\nTest\n#+end_comment\n"
	  (org-element-interpret-data '(comment-block (:value "Test"))))))

(ert-deftest test-org-element/diary-sexp ()
  "Test diary-sexp interpreter."
  (should
   (equal
    (org-test-parse-and-interpret
     "%%(org-anniversary 1956  5 14)(2) Arthur Dent is %d years old")
    "%%(org-anniversary 1956  5 14)(2) Arthur Dent is %d years old\n")))

(ert-deftest test-org-element/example-block-interpreter ()
  "Test example block interpreter."
  ;; Without switches.
  (should (equal "#+begin_example\nTest\n#+end_example\n"
		 (let ((org-src-preserve-indentation t))
		   (org-test-parse-and-interpret
		    "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE"))))
  ;; With switches.
  (should
   (equal "#+begin_example -n -k\n(+ 1 1)\n#+end_example\n"
	  (let ((org-src-preserve-indentation t))
	    (org-test-parse-and-interpret
	     "#+BEGIN_EXAMPLE -n -k\n(+ 1 1)\n#+END_EXAMPLE"))))
  ;; Preserve code escaping.
  (should
   (equal
    (let ((org-src-preserve-indentation t))
      (org-test-parse-and-interpret
       "#+BEGIN_EXAMPLE\n,* Headline\n,#+KEYWORD: value\nText\n#+END_EXAMPLE"))
    "#+begin_example\n,* Headline\n,#+KEYWORD: value\nText\n#+end_example\n"))
  ;; Accept missing final newline in value.
  (should
   (equal
    "#+begin_example\nTest\n#+end_example\n"
    (let ((org-src-preserve-indentation t))
      (org-element-interpret-data '(example-block (:value "Test"))))))
  ;; Handle indentation.
  (should (equal "#+begin_example\n  Test\n#+end_example\n"
		 (let ((org-src-preserve-indentation nil)
		       (org-edit-src-content-indentation 2))
		   (org-test-parse-and-interpret
		    "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE"))))
  (should (equal "#+begin_example\n Test\n#+end_example\n"
		 (let ((org-src-preserve-indentation t)
		       (org-edit-src-content-indentation 2))
		   (org-test-parse-and-interpret
		    "#+BEGIN_EXAMPLE\n Test\n#+END_EXAMPLE")))))

(ert-deftest test-org-element/export-block-interpreter ()
  "Test export block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+begin_export HTML\nTest\n#+end_export")
		 "#+begin_export HTML\nTest\n#+end_export\n")))

(ert-deftest test-org-element/fixed-width-interpreter ()
  "Test fixed width interpreter."
  ;; Standard test.
  (should (equal (org-test-parse-and-interpret ": Test") ": Test\n"))
  ;; Preserve indentation.
  (should (equal (org-test-parse-and-interpret ":  2 blanks\n: 1 blank")
		 ":  2 blanks\n: 1 blank\n"))
  ;; Handle empty string.
  (should
   (equal (org-element-fixed-width-interpreter
	   '(fixed-width (:value "")) nil)
	  ":\n"))
  ;; Handle nil value.
  (should-not
   (org-element-fixed-width-interpreter
    '(fixed-width (:value nil)) nil)))

(ert-deftest test-org-element/horizontal-rule-interpreter ()
  "Test horizontal rule interpreter."
  (should (equal (org-test-parse-and-interpret "-------") "-----\n")))

(ert-deftest test-org-element/keyword-interpreter ()
  "Test keyword interpreter."
  (should (equal (org-test-parse-and-interpret "#+KEYWORD: value")
		 "#+keyword: value\n")))

(ert-deftest test-org-element/latex-environment-interpreter ()
  "Test latex environment interpreter."
  (should (equal (org-test-parse-and-interpret
		  "\\begin{equation}\n1+1=2\n\\end{equation}")
		 "\\begin{equation}\n1+1=2\n\\end{equation}\n"))
  (should (equal (org-test-parse-and-interpret
		  "\\begin{theorem}[me]\n1+1=2\n\\end{theorem}")
		 "\\begin{theorem}[me]\n1+1=2\n\\end{theorem}\n")))

(ert-deftest test-org-element/planning-interpreter ()
  "Test planning interpreter."
  (should
   (string-match
    "\\* Headline
DEADLINE: <2012-03-29 .*?> SCHEDULED: <2012-03-29 .*?> CLOSED: \\[2012-03-29 .*?\\]"
    (org-test-parse-and-interpret
     "* Headline
DEADLINE: <2012-03-29 thu.> SCHEDULED: <2012-03-29 thu.> CLOSED: [2012-03-29 thu.]"))))

(ert-deftest test-org-element/property-drawer-interpreter ()
  "Test property drawer interpreter."
  (should (equal (let ((org-property-format "%-10s %s"))
		   (org-test-parse-and-interpret
		    "* H\n:PROPERTIES:\n:prop: value\n:END:"))
		 "* H\n:PROPERTIES:\n:prop:     value\n:END:\n")))

(ert-deftest test-org-element/src-block-interpreter ()
  "Test src block interpreter."
  ;; With arguments.
  (should
   (equal (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation nil))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp :results silent\n(+ 1 1)\n#+END_SRC"))
	  "#+begin_src emacs-lisp :results silent\n  (+ 1 1)\n#+end_src\n"))
  ;; With switches.
  (should
   (equal (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation nil))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp -n -k\n(+ 1 1)\n#+END_SRC"))
	  "#+begin_src emacs-lisp -n -k\n  (+ 1 1)\n#+end_src\n"))
  ;; Preserve code escaping.
  (should
   (equal
    (let ((org-edit-src-content-indentation 2)
	  (org-src-preserve-indentation nil))
      (org-test-parse-and-interpret
       "#+BEGIN_SRC org\n,* Headline\n,#+KEYWORD: value\nText\n#+END_SRC"))
    "#+begin_src org\n  ,* Headline\n  ,#+KEYWORD: value\n  Text\n#+end_src\n"))
  ;; Do not apply `org-edit-src-content-indentation' when preserving
  ;; indentation.
  (should
   (equal (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation t))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"))
	  "#+begin_src emacs-lisp\n(+ 1 1)\n#+end_src\n"))
  (should
   (equal (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation nil))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n#+END_SRC"))
	  "#+begin_src emacs-lisp -i\n(+ 1 1)\n#+end_src\n"))
  ;; Accept missing final newline in value.
  (should
   (equal
    "#+begin_src emacs-lisp\n  Test\n#+end_src\n"
    (let ((org-edit-src-content-indentation 2)
	  (org-src-preserve-indentation nil))
      (org-element-interpret-data
       '(src-block (:language "emacs-lisp" :value "Test")))))))

(ert-deftest test-org-element/table-interpreter ()
  "Test table, table-row and table-cell interpreters."
  ;; 1. Simple table.
  (should (equal (org-test-parse-and-interpret "| a | b |\n| c | d |")
		 "| a | b |\n| c | d |\n"))
  ;; 2. With horizontal rules.
  (should (equal (org-test-parse-and-interpret
		  "| a | b |\n|---+---|\n| c | d |")
		 "| a | b |\n|---+---|\n| c | d |\n"))
  ;; 3. With meta-data.
  (should (equal (org-test-parse-and-interpret "| / | < | > |\n| * | 1 | 2 |")
		 "| / | < | > |\n| * | 1 | 2 |\n"))
  ;; 4. With a formula.
  (should
   (equal (org-test-parse-and-interpret
	   "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: @3=vmean(@1..@2)")
	  "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: @3=vmean(@1..@2)\n"))
  ;; 5. With multiple formulas.
  (should
   (equal (org-test-parse-and-interpret
	   "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: test1\n#+TBLFM: test2")
	  "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: test1\n#+TBLFM: test2\n")))

(ert-deftest test-org-element/timestamp-interpreter ()
  "Test timestamp interpreter."
  ;; Active.
  (should
   (string-match "<2012-03-29 .* 16:40>"
		 (org-test-parse-and-interpret "<2012-03-29 thu. 16:40>")))
  (should
   (string-match "<2012-03-29 .* 16:40>"
		 (org-element-timestamp-interpreter
		  '(timestamp
		    (:type active :year-start 2012 :month-start 3 :day-start 29
			   :hour-start 16 :minute-start 40)) nil)))
  (should
   (string-match
    "<2012-03-29 .* 16:40>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40 :year-end 2012 :month-end 3
	      :day-end 29 :hour-end 16 :minute-end 40)) nil)))
  ;; Inactive.
  (should
   (string-match "\\[2012-03-29 .* 16:40\\]"
		 (org-test-parse-and-interpret "[2012-03-29 thu. 16:40]")))
  (should
   (string-match
    "\\[2012-03-29 .* 16:40\\]"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type inactive :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40)) nil)))
  ;; Active daterange.
  (should
   (string-match "<2012-03-29 .* 16:40>--<2012-03-29 .* 16:41>"
		 (org-test-parse-and-interpret
		  "<2012-03-29 thu. 16:40>--<2012-03-29 thu. 16:41>")))
  ;;; No end time, dates are not equal
  (should
   ;; Expected result: "<2012-03-29 Thu 16:40>--<2012-03-30 Fri>"
   (string=
    (format
     "<%s>--<%s>"
     (format-time-string (cdr org-time-stamp-formats) (org-encode-time 0 40 16 29 03 2012))
     (format-time-string (car org-time-stamp-formats) (org-encode-time 0 0 0 30 03 2012)))
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active-range :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40 :year-end 2012 :month-end 3
	      :day-end 30)) nil)))
  ;;; No start time, dates are not equal
  (should
   ;; Expected result: "<2012-03-29 Thu>--<2012-03-30 Fri 16:40>"
   (string=
    (format
     "<%s>--<%s>"
     (format-time-string (car org-time-stamp-formats) (org-encode-time 0 0 0 29 03 2012))
     (format-time-string (cdr org-time-stamp-formats) (org-encode-time 0 40 16 30 03 2012)))
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active-range :year-start 2012 :month-start 3 :day-start 29
	      :hour-end 16 :minute-end 40 :year-end 2012 :month-end 3
	      :day-end 30)) nil)))
  (should
   (string-match
    "<2012-03-29 .* 16:40>--<2012-03-29 .* 16:40>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active-range :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40 :year-end 2012 :month-end 3
	      :day-end 29 :hour-end 16 :minute-end 40)) nil)))
  (should
   (string-match
    "<2012-03-29 .* 16:40>--<2012-03-29 .* 16:41>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active-range :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40 :year-end 2012 :month-end 3
	      :day-end 29 :hour-end 16 :minute-end 41)) nil)))
  ;; Inactive daterange.
  (should
   (string-match "\\[2012-03-29 .* 16:40\\]--\\[2012-03-29 .* 16:41\\]"
		 (org-test-parse-and-interpret
		  "[2012-03-29 thu. 16:40]--[2012-03-29 thu. 16:41]")))
  (should
   (string-match
    "\\[2012-03-29 .* 16:40\\]--\\[2012-03-29 .* 16:41\\]"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type inactive-range :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40 :year-end 2012 :month-end 3
	      :day-end 29 :hour-end 16 :minute-end 41)) nil)))
  ;; Active timerange
  (should
   (string-match "<2012-03-29 .* 16:40-16:41>"
		 (org-test-parse-and-interpret
		  "<2012-03-29 thu. 16:40-16:41>")))
  ;; Diary.
  (should (equal (org-test-parse-and-interpret "<%%(diary-float t 4 2)>")
		 "<%%(diary-float t 4 2)>\n"))
  ;; Diary with time.
  (should (equal (org-test-parse-and-interpret "<%%(diary-float t 4 2) 12:00>")
		 "<%%(diary-float t 4 2) 12:00>\n"))
  (should (equal (org-test-parse-and-interpret "<%%(diary-cyclic 1 1 1 2020) 12:00-14:00>")
		 "<%%(diary-cyclic 1 1 1 2020) 12:00-14:00>\n"))
  (org-test-with-temp-text "<%%(diary-float t 4 2) 12:00>"
    (let ((ts (org-element-context)))
      (should (org-element-type-p ts 'timestamp))
      (should (eq 'diary (org-element-property :type ts)))
      (should (eq nil (org-element-property :range-type ts)))
      (should (equal 12 (org-element-property :hour-start ts)))
      (should (equal 0 (org-element-property :minute-start ts)))
      (should-not (org-element-property :hour-end ts))
      (should-not (org-element-property :minute-end ts))))
  (org-test-with-temp-text "<%%(diary-float t 4 2) 12:00-14:01>"
    (let ((ts (org-element-context)))
      (should (org-element-type-p ts 'timestamp))
      (should (eq 'diary (org-element-property :type ts)))
      (should (eq 'timerange (org-element-property :range-type ts)))
      (should (equal 12 (org-element-property :hour-start ts)))
      (should (equal 0 (org-element-property :minute-start ts)))
      (should (equal 14 (org-element-property :hour-end ts)))
      (should (equal 1 (org-element-property :minute-end ts)))))
  ;; Timestamp with repeater interval, repeater deadline, with delay, with combinations.
  (should
   (string-match "<2012-03-29 .* \\+1y>"
		 (org-test-parse-and-interpret "<2012-03-29 thu. +1y>")))
  (should
   (string-match
    "<2012-03-29 .* \\+1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
	      :repeater-type cumulate :repeater-value 1 :repeater-unit year))
     nil)))
  (should
   (string-match
    "<2012-03-29 .* \\+1y/2y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
              :repeater-type cumulate :repeater-value 1 :repeater-unit year
              :repeater-deadline-value 2 :repeater-deadline-unit year))
     nil)))
  (should
   (string-match
    "<2012-03-29 .* -1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
	      :warning-type all :warning-value 1 :warning-unit year))
     nil)))
  (should
   (string-match
    "<2012-03-29 .* \\+1y -1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
	      :warning-type all :warning-value 1 :warning-unit year
	      :repeater-type cumulate :repeater-value 1 :repeater-unit year))
     nil)))
  (should
   (string-match
    "<2012-03-29 .* \\+1y/2y -1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
              :warning-type all :warning-value 1 :warning-unit year
              :repeater-type cumulate :repeater-value 1 :repeater-unit year
              :repeater-deadline-value 2 :repeater-deadline-unit year))
     nil)))
  ;; Timestamp range with repeater interval
  (should
   (string-match "<2012-03-29 .* \\+1y>--<2012-03-30 .* \\+1y>"
		 (org-test-parse-and-interpret
		  "<2012-03-29 Thu +1y>--<2012-03-30 Thu +1y>")))
  (should
   (string-match
    "<2012-03-29 .* \\+1y>--<2012-03-30 .* \\+1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active-range :year-start 2012 :month-start 3 :day-start 29
	      :year-end 2012 :month-end 3 :day-end 30 :repeater-type cumulate
	      :repeater-value 1 :repeater-unit year))
     nil)))
  ;; Tests for :range-type property
  ;;; Errors
  (should-error
   (org-element-timestamp-interpreter
    '(timestamp
      (:range-type timerange
                   :type active
                   :year-start 2023 :month-start 7 :day-start 10
                   :year-end 2023 :month-end 7 :day-end 10
                   :hour-start 17 :minute-start 30
                   :hour-end 17 :minute-end 30))
    nil))
  (should-error
   (org-element-timestamp-interpreter
    '(timestamp
      (:range-type daterange
                   :type active :year-start 2023 :month-start 7 :day-start 10
                   :hour-start 17 :minute-start 30)) nil))
  (should-error
   (org-element-timestamp-interpreter
    '(timestamp
      (:range-type timerange
                   :type inactive
                   :year-start 2023 :month-start 7 :day-start 10
                   :year-end 2023 :month-end 7 :day-end 10
                   :hour-start 17 :minute-start 30
                   :hour-end 17 :minute-end 30))
    nil))
  (should-error
   (org-element-timestamp-interpreter
    '(timestamp
      (:range-type daterange
                   :type inactive :year-start 2023 :month-start 7 :day-start 10
                   :hour-start 17 :minute-start 30)) nil))
  
  ;;; End part is nil
  (should
   ;; Expected result: "<2023-07-10 Mon>--<2023-07-10 Mon>"
   (string=
    (format
     "<%s>--<%s>"
     (format-time-string (car org-time-stamp-formats) (org-encode-time 0 0 0 10 7 2023))
     (format-time-string (car org-time-stamp-formats) (org-encode-time 0 0 0 10 7 2023)))
    (org-element-timestamp-interpreter
     '(timestamp
       (:range-type daterange
                    :type active-range :year-start 2023 :month-start 7 :day-start 10)) nil)))
  (should
   (string-match "<2023-07-10 .* 17:30-17:30>"
		 (org-element-timestamp-interpreter
		  '(timestamp
		    (:range-type timerange
                                 :type active-range :year-start 2023 :month-start 7 :day-start 10
		                 :hour-start 17 :minute-start 30)) nil)))
  (should
   ;; Expected result: "<2023-07-10 Mon 17:30>--<2023-07-10 Mon>"
   (string=
    (format
     "<%s>--<%s>"
     (format-time-string (cdr org-time-stamp-formats) (org-encode-time 0 30 17 10 7 2023))
     (format-time-string (car org-time-stamp-formats) (org-encode-time 0 0 0 10 7 2023)))
    (org-element-timestamp-interpreter
     '(timestamp
       (:range-type daterange
                    :type active-range :year-start 2023 :month-start 7 :day-start 10
		    :hour-start 17 :minute-start 30)) nil)))
  ;;; End is equal to start
  (should
   (string-match "<2023-07-10 .* 17:30-17:30>"
		 (org-element-timestamp-interpreter
		  '(timestamp
		    (:range-type timerange
                                 :type active-range
                                 :year-start 2023 :month-start 7 :day-start 10
                                 :year-end 2023 :month-end 7 :day-end 10
		                 :hour-start 17 :minute-start 30
                                 :hour-end 17 :minute-end 30)) nil)))
  (should
   (string-match "<2023-07-10 .* 17:30>--<2023-07-10 .* 17:30>"
		 (org-element-timestamp-interpreter
		  '(timestamp
		    (:range-type daterange
                                 :type active-range
                                 :year-start 2023 :month-start 7 :day-start 10
                                 :year-end 2023 :month-end 7 :day-end 10
		                 :hour-start 17 :minute-start 30
                                 :hour-end 17 :minute-end 30)) nil)))
  ;;;; End date is not equal to start date, but interpret the object as a timerange (:range-type 'timerange)
  (should
   (string-match "<2023-07-10 .* 17:30-18:30>"
		 (org-element-timestamp-interpreter
		  '(timestamp
		    (:range-type timerange
                                 :type active-range
                                 :year-start 2023 :month-start 7 :day-start 10
                                 :year-end 2023 :month-end 8 :day-end 10
                                 :hour-start 17 :minute-start 30
                                 :hour-end 18 :minute-end 30)) nil)))
  ;;;; End date is not equal to start date, interpret the object as a daterange (:range-type 'daterange)
  (should
   (string-match "<2023-07-10 .* 17:30>--<2023-08-10 .* 18:30>"
		 (org-element-timestamp-interpreter
		  '(timestamp
		    (:range-type daterange
                                 :type active-range
                                 :year-start 2023 :month-start 7 :day-start 10
		                 :year-end 2023 :month-end 8 :day-end 10
                                 :hour-start 17 :minute-start 30
                                 :hour-end 18 :minute-end 30)) nil))))

(ert-deftest test-org-element/verse-block-interpreter ()
  "Test verse block interpretation."
  (should
   (equal (org-test-parse-and-interpret "#+BEGIN_VERSE\nTest\n#+END_VERSE")
	  "#+begin_verse\nTest\n#+end_verse\n")))

(ert-deftest test-org-element/bold-interpreter ()
  "Test bold interpreter."
  (should (equal (org-test-parse-and-interpret "*text*") "*text*\n")))

(ert-deftest test-org-element/citation-interpreter ()
  "Test citation interpreter."
  (should
   (equal "[cite:@key]\n"
	  (org-test-parse-and-interpret "[cite:@key]")))
  (should
   (equal "[cite:-@key]\n"
	  (org-test-parse-and-interpret "[cite:-@key]")))
  (should
   (equal "[cite/style:@key]\n"
	  (org-test-parse-and-interpret "[cite/style:@key]")))
  (should
   (equal "[cite:pre @key]\n"
	  (org-test-parse-and-interpret "[cite:pre @key]")))
  (should
   (equal "[cite:@key post]\n"
	  (org-test-parse-and-interpret "[cite:@key post]")))
  (should
   (equal "[cite:@a ;b]\n"
	  (org-test-parse-and-interpret "[cite: @a ;b]")))
  (should
   (equal "[cite:@a;@b;@c]\n"
	  (org-test-parse-and-interpret "[cite:@a;@b;@c]")))
  (should
   (equal "[cite:common-pre ; @a]\n"
	  (org-test-parse-and-interpret "[cite:common-pre ; @a]")))
  (should
   (equal "[cite:@a ; common-post]\n"
	  (org-test-parse-and-interpret "[cite:@a ; common-post]"))))

(ert-deftest test-org-element/code-interpreter ()
  "Test code interpreter."
  (should (equal (org-test-parse-and-interpret "~text~") "~text~\n")))

(ert-deftest test-org-element/entity-interpreter ()
  "Test entity interpreter."
  ;; 1. Without brackets.
  (should
   (equal (org-test-parse-and-interpret "\\alpha text") "\\alpha text\n"))
  ;; 2. With brackets.
  (should
   (equal (org-test-parse-and-interpret "\\alpha{}text") "\\alpha{}text\n")))

(ert-deftest test-org-element/export-snippet-interpreter ()
  "Test export snippet interpreter."
  (should (equal (org-test-parse-and-interpret "@@backend:contents@@")
		 "@@backend:contents@@\n")))

(ert-deftest test-org-element/footnote-reference-interpreter ()
  "Test footnote reference interpreter."
  ;; Regular reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:1]") "Text[fn:1]\n"))
  ;; Named reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:label]")
		 "Text[fn:label]\n"))
  ;; Inline reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:label:def]")
		 "Text[fn:label:def]\n"))
  ;; Anonymous reference.
  (should (equal (org-test-parse-and-interpret "Text[fn::def]")
		 "Text[fn::def]\n")))

(ert-deftest test-org-element/inline-babel-call-interpreter ()
  "Test inline babel call interpreter."
  ;; Without arguments.
  (should (equal (org-test-parse-and-interpret "call_test()") "call_test()\n"))
  ;; With arguments.
  (should (equal (org-test-parse-and-interpret "call_test(x=2)")
		 "call_test(x=2)\n"))
  ;; With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "call_test[:results output]()[:results html]")
		 "call_test[:results output]()[:results html]\n")))

(ert-deftest test-org-element/inline-src-block-interpreter ()
  "Test inline src block interpreter."
  ;; 1. Without header argument.
  (should (equal (org-test-parse-and-interpret "src_emacs-lisp{(+ 1 1)}")
		 "src_emacs-lisp{(+ 1 1)}\n"))
  ;; 2. With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "src_emacs-lisp[:results silent]{(+ 1 1)}")
		 "src_emacs-lisp[:results silent]{(+ 1 1)}\n")))

(ert-deftest test-org-element/italic-interpreter ()
  "Test italic interpreter."
  (should (equal (org-test-parse-and-interpret "/text/") "/text/\n")))

(ert-deftest test-org-element/latex-fragment-interpreter ()
  "Test latex fragment interpreter."
  (should (equal (org-test-parse-and-interpret "\\command{}") "\\command{}\n"))
  (should (equal (org-test-parse-and-interpret "$x$") "$x$\n"))
  (should (equal (org-test-parse-and-interpret "$x+y$") "$x+y$\n"))
  (should (equal (org-test-parse-and-interpret "$$x+y$$") "$$x+y$$\n"))
  (should (equal (org-test-parse-and-interpret "\\(x+y\\)") "\\(x+y\\)\n"))
  (should (equal (org-test-parse-and-interpret "\\[x+y\\]") "\\[x+y\\]\n")))

(ert-deftest test-org-element/line-break-interpreter ()
  "Test line break interpreter."
  (should (equal (org-test-parse-and-interpret "First line \\\\ \nSecond line")
		 "First line \\\\\nSecond line\n")))

(ert-deftest test-org-element/link-interpreter ()
  "Test link interpreter."
  ;; Links targeted from a radio target.
  (should (equal (let ((org-target-link-regexp "radio-target"))
		   (org-test-parse-and-interpret "a radio-target"))
		 "a radio-target\n"))
  ;; Links without description.
  (should (equal (org-test-parse-and-interpret "[[https://orgmode.org]]")
		 "[[https://orgmode.org]]\n"))
  ;; Links with a description, even one containing a link.
  (should (equal (org-test-parse-and-interpret
		  "[[https://orgmode.org][Org mode]]")
		 "[[https://orgmode.org][Org mode]]\n"))
  (should (equal (org-test-parse-and-interpret
		  "[[https://orgmode.org][https://orgmode.org]]")
		 "[[https://orgmode.org][https://orgmode.org]]\n"))
  ;; File links.
  (should
   (equal (org-test-parse-and-interpret "[[file+emacs:todo.org]]")
	  "[[file+emacs:todo.org]]\n"))
  (should
   (equal (org-test-parse-and-interpret "[[file:todo.org::*task]]")
	  "[[file:todo.org::*task]]\n"))
  (should
   (equal (org-test-parse-and-interpret "[[/tmp/todo.org::*task]]")
	  "[[/tmp/todo.org::*task]]\n"))
  ;; Id links.
  (should (equal (org-test-parse-and-interpret "[[id:aaaa]]") "[[id:aaaa]]\n"))
  ;; Custom-id links.
  (should (equal (org-test-parse-and-interpret "[[#id]]") "[[#id]]\n"))
  ;; Code-ref links.
  (should (equal (org-test-parse-and-interpret "[[(ref)]]") "[[(ref)]]\n"))
  ;; Plain links.
  (should (equal (org-test-parse-and-interpret "https://orgmode.org")
		 "https://orgmode.org\n"))
  ;; Angular links.
  (should (equal (org-test-parse-and-interpret "<https://orgmode.org>")
		 "<https://orgmode.org>\n"))
  ;; Pathological case: link with a %-sign in description.
  (should (equal (org-test-parse-and-interpret "[[file://path][%s]]")
		 "[[file://path][%s]]\n")))

(ert-deftest test-org-element/macro-interpreter ()
  "Test macro interpreter."
  ;; 1. Without argument.
  (should (equal (org-test-parse-and-interpret "{{{test}}}") "{{{test}}}\n"))
  ;; 2. With arguments.
  (should (equal (org-test-parse-and-interpret "{{{test(arg1,arg2)}}}")
		 "{{{test(arg1,arg2)}}}\n")))

(ert-deftest test-org-element/radio-target-interpreter ()
  "Test radio target interpreter."
  (should (equal (org-test-parse-and-interpret "<<<some text>>>")
		 "<<<some text>>>\n")))

(ert-deftest test-org-element/statistics-cookie-interpreter ()
  "Test statistics cookie interpreter."
  ;; 1. Without percent
  (should (equal (org-test-parse-and-interpret "[0/1]") "[0/1]\n"))
  ;; 2. With percent.
  (should (equal (org-test-parse-and-interpret "[66%]") "[66%]\n")))

(ert-deftest test-org-element/strike-through-interpreter ()
  "Test strike through interpreter."
  (should (equal (org-test-parse-and-interpret "+target+") "+target+\n")))

(ert-deftest test-org-element/subscript-interpreter ()
  "Test subscript interpreter."
  ;; 1. Without brackets.
  (should (equal (org-test-parse-and-interpret "a_b") "a_b\n"))
  ;; 2. With brackets.
  (should (equal (org-test-parse-and-interpret "a_{b}") "a_{b}\n")))

(ert-deftest test-org-element/superscript-interpreter ()
  "Test superscript interpreter."
  ;; 1. Without brackets.
  (should (equal (org-test-parse-and-interpret "a^b") "a^b\n"))
  ;; 2. With brackets.
  (should (equal (org-test-parse-and-interpret "a^{b}") "a^{b}\n")))

(ert-deftest test-org-element/target-interpreter ()
  "Test target interpreter."
  (should (equal (org-test-parse-and-interpret "<<target>>") "<<target>>\n")))

(ert-deftest test-org-element/underline-interpreter ()
  "Test underline interpreter."
  (should (equal (org-test-parse-and-interpret "_text_") "_text_\n")))

(ert-deftest test-org-element/verbatim-interpreter ()
  "Test verbatim interpreter."
  (should (equal (org-test-parse-and-interpret "=text=") "=text=\n")))



;;; Test Granularity

(ert-deftest test-org-element/granularity ()
  "Test granularity impact on buffer parsing."
  (org-test-with-temp-text
      "* Head 1
** Head 2
#+BEGIN_CENTER
Centered paragraph.
#+END_CENTER
Paragraph \\alpha."
    ;; 1.1. Granularity set to `headline' should parse every headline
    ;;      in buffer, and only them.
    (let ((tree (org-element-parse-buffer 'headline)))
      (should (= 2 (length (org-element-map tree 'headline 'identity))))
      (should-not (org-element-map tree 'paragraph 'identity)))
    ;; 1.2. Granularity set to `greater-element' should not enter
    ;;      greater elements excepted headlines and sections.
    (let ((tree (org-element-parse-buffer 'greater-element)))
      (should (= 1 (length (org-element-map tree 'center-block 'identity))))
      (should (= 1 (length (org-element-map tree 'paragraph 'identity))))
      (should-not (org-element-map tree 'entity 'identity)))
    ;; 1.3. Granularity set to `element' should enter every
    ;;      greater-element.
    (let ((tree (org-element-parse-buffer 'element)))
      (should (= 2 (length (org-element-map tree 'paragraph 'identity))))
      (should-not (org-element-map tree 'entity 'identity)))
    ;; 1.4. Granularity set to `object' can see everything.
    (let ((tree (org-element-parse-buffer 'object)))
      (should (= 1 (length (org-element-map tree 'entity 'identity)))))))

(ert-deftest test-org-element/secondary-string-parsing ()
  "Test if granularity correctly toggles secondary strings parsing."
  ;; With a granularity bigger than `object', no secondary string
  ;; should be parsed.
  (should
   (stringp
    (org-test-with-temp-text "* Headline"
      (let ((headline
	     (org-element-map (org-element-parse-buffer 'headline) 'headline
	       #'identity nil 'first-match)))
	(org-element-property :title headline)))))
  (should
   (stringp
    (org-test-with-temp-text "* Headline\n- tag :: item"
      (let ((item (org-element-map (org-element-parse-buffer 'element) 'item
		    #'identity nil 'first-match)))
	(org-element-property :tag item)))))
  (when (featurep 'org-inlinetask)
    (should
     (stringp
      (let ((org-inlinetask-min-level 15))
	(org-test-with-temp-text "*************** Inlinetask"
	  (let ((inlinetask (org-element-map (org-element-parse-buffer 'element)
				'inlinetask
			      #'identity nil 'first-match)))
	    (org-element-property :title inlinetask)))))))
  ;; With a default granularity, secondary strings should be parsed.
  (should
   (listp
    (org-test-with-temp-text "* Headline"
      (let ((headline
	     (org-element-map (org-element-parse-buffer) 'headline
	       #'identity nil 'first-match)))
	(org-element-property :title headline)))))
  ;; `org-element-at-point' should never parse a secondary string.
  (should-not
   (listp
    (org-test-with-temp-text "* Headline"
      (org-element-property :title (org-element-at-point)))))
  ;; Preserve current local variables when parsing a secondary string.
  (should
   (let ((org-entities nil)
	 (org-entities-user nil))
     (org-test-with-temp-text "
#+CAPTION: \\foo
Text
# Local Variables:
# org-entities-user: ((\"foo\"))
# End:"
       (let ((safe-local-variable-values '((org-entities-user . (("foo"))))))
	 (hack-local-variables))
       (org-element-map (org-element-parse-buffer) 'entity
	 #'identity nil nil nil t)))))



;;; Test Visible Only Parsing

(ert-deftest test-org-element/parse-buffer-visible ()
  "Test `org-element-parse-buffer' with visible only argument."
  (should
   (equal '("H1" "H3" "H5")
	  (org-test-with-temp-text
	      "* H1\n** H2\n** H3 :visible:\n** H4\n** H5 :visible:"
	    (org-occur ":visible:")
	    (org-element-map (org-element-parse-buffer nil t) 'headline
	      (lambda (hl) (org-element-property :raw-value hl))))))
  (should
   (equal "Test"
	  (let ((contents "Test"))
	    (org-test-with-temp-text contents
	      (add-text-properties 0 1 '(invisible t) contents)
	      (org-element-map (org-element-parse-buffer nil t) 'plain-text
		#'org-no-properties nil t)))))
  (should
   (equal "Test"
	  (let ((contents "Test"))
	    (org-test-with-temp-text (concat "- " contents)
	      (add-text-properties 0 1 '(invisible t) contents)
	      (org-element-map (org-element-parse-buffer nil t) 'plain-text
		#'org-no-properties nil t))))))



;;; Test `:parent' Property

(ert-deftest test-org-element/parent-property ()
  "Test `:parent' property."
  ;; Elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nText\n#+END_CENTER"
    (let* ((tree (org-element-parse-buffer))
	   (parent (org-element-property
		    :parent
		    (org-element-map tree 'paragraph 'identity nil t))))
      (should parent)
      (should (eq (org-element-map tree 'center-block 'identity nil t)
		  parent))))
  ;; Objects.
  (org-test-with-temp-text "a_{/b/}"
    (let* ((tree (org-element-parse-buffer))
	   (parent (org-element-property
		    :parent
		    (org-element-map tree 'italic 'identity nil t))))
      (should parent)
      (should (eq parent
		  (org-element-map tree 'subscript 'identity nil t)))))
  ;; Secondary strings
  (org-test-with-temp-text "* /italic/"
    (let* ((tree (org-element-parse-buffer))
	   (parent (org-element-property
		    :parent (org-element-map tree 'italic 'identity nil t))))
      (should parent)
      (should (equal parent
		     (org-element-map tree 'headline 'identity nil t))))))



;;; Test Normalize Contents

(ert-deftest test-org-element/normalize-contents ()
  "Test `org-element-normalize-contents' specifications."
  ;; Remove maximum common indentation from element's contents.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n   Three spaces"))
    '(paragraph nil "Two spaces\n Three spaces")))
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\nNo space"))
    '(paragraph nil "  Two spaces\nNo space")))
  ;; Ignore objects within contents when computing maximum common
  ;; indentation.  However, if contents start with an object, common
  ;; indentation is 0.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil " One " (emphasis nil "space") "\n  Two spaces"))
    '(paragraph nil "One " (emphasis nil "space") "\n Two spaces")))
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil (verbatim nil "V") "No space\n  Two\n   Three"))
    '(paragraph nil (verbatim nil "V") "No space\n  Two\n   Three")))
  ;; Ignore blank lines.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n\n \n  Two spaces"))
    '(paragraph nil "Two spaces\n\n\nTwo spaces")))
  (should
   (equal
    '(paragraph nil " Two spaces\n" (verbatim nil "V") "\n Two spaces")
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n " (verbatim nil "V") "\n  Two spaces"))))
  (should
   (equal
    '(verse-block nil "line 1\n\nline 2")
    (org-element-normalize-contents
     '(verse-block nil "  line 1\n\n  line 2"))))
  ;; Recursively enter objects in order to compute common indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces " (bold nil " and\n One space")))
    '(paragraph nil " Two spaces " (bold nil " and\nOne space"))))
  ;; When optional argument is provided, ignore first line
  ;; indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "No space\n  Two spaces\n   Three spaces") t)
    '(paragraph nil "No space\nTwo spaces\n Three spaces")))
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil (verbatim nil "V") "No space\n  Two\n   Three") t)
    '(paragraph nil (verbatim nil "V") "No space\nTwo\n Three")))
  ;; Corner case: do not ignore indentation of string right after
  ;; a line break.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil " 1 space" (line-break) "  2 spaces"))
    '(paragraph nil "1 space" (line-break) " 2 spaces"))))



;;; Test Navigation Tools.

(ert-deftest test-org-element/at-point ()
  "Test `org-element-at-point' specifications."
  ;; Return closest element containing point.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "#+BEGIN_CENTER\nA\n#+END_CENTER"
	 (progn (search-forward "A")
		(org-element-type (org-element-at-point))))))
  ;; Point in other buffer.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "#+BEGIN_CENTER\nA\n#+END_CENTER"
	 (progn (search-forward "A")
		(org-element-type
                 (let ((mk (point-marker)))
                   (with-temp-buffer
                     (org-element-at-point mk))))))))
  ;; Correctly set `:parent' property.
  (should
   (eq 'center-block
       (org-test-with-temp-text "#+BEGIN_CENTER\nA\n#+END_CENTER"
	 (progn (search-forward "A")
		(org-element-type
		 (org-element-property :parent (org-element-at-point)))))))
  ;; Special case: at a blank line just below a headline, return that
  ;; headline.
  (should
   (equal "H1" (org-test-with-temp-text "* H1\n  \n* H2\n"
		 (forward-line)
		 (org-element-property :title (org-element-at-point)))))
  ;; Special case: at the very beginning of a table, return `table'
  ;; object instead of `table-row'.
  (should
   (eq 'table
       (org-test-with-temp-text "| a | b |"
	 (org-element-type (org-element-at-point)))))
  ;; Special case: at the very beginning of a list or sub-list, return
  ;; `plain-list' object instead of `item'.
  (should
   (eq 'plain-list
       (org-test-with-temp-text "- item"
	 (org-element-type (org-element-at-point)))))
  ;; Special case: at the closing line of a greater element, be sure
  ;; to return it instead of the last element in its contents.
  (should
   (eq 'center-block
       (org-test-with-temp-text "#+BEGIN_CENTER\nParagraph\n#+END_CENTER"
	 (progn (forward-line 2)
		(org-element-type (org-element-at-point))))))
  ;; Special case: at a blank line between two items, be sure to
  ;; return item above instead of the last element of its contents.
  (should
   (eq 'item
       (org-test-with-temp-text "- Para1\n\n- Para2"
	 (forward-line)
	 (org-element-type (org-element-at-point)))))
  ;; Special case: at the last blank line in a plain list, return it
  ;; instead of the last item.
  (should
   (eq 'plain-list
       (org-test-with-temp-text "- Para1\n- Para2\n\nPara3"
	 (progn (forward-line 2)
		(org-element-type (org-element-at-point))))))
  ;; Special case: at the last blank line in a plain list at the end of
  ;; a headline, return the plain list, not the last item, and not the
  ;; headline.
  (should
   (eq 'plain-list
       (org-test-with-temp-text "* Headline\n- Para1\n- Para2\n\nPara3\n* Another headline"
	 (progn (forward-line 3)
		(org-element-type (org-element-at-point))))))
  ;; Special case: when a list ends at the end of buffer and there's
  ;; no final newline, return last element in last item.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "- a"
	 (end-of-line)
	 (org-element-type (org-element-at-point)))))
  ;; Parse a list within a block itself contained in a list.
  (should
   (eq 'plain-list
       (org-test-with-temp-text
	   "- outer\n  #+begin_center\n  - inner\n  #+end_center"
	 (search-forward "inner")
	 (beginning-of-line)
	 (org-element-type (org-element-at-point)))))
  ;; Do not error at eob on an empty line.
  (should
   (org-test-with-temp-text "* H\n"
     (forward-line)
     (or (org-element-at-point) t)))
  ;; Return greater element when outside contents.
  (should
   (eq 'drawer
       (org-test-with-temp-text
        ":DRAWER:\ntest\n:EN<point>D:\n"
        (org-element-type (org-element-at-point)))))
  (should
   (eq 'drawer
       (org-test-with-temp-text
        ":DRA<point>WER:\ntest\n:END:\n"
        (org-element-type (org-element-at-point)))))
  ;; Return greater element when at :contents-end.
  (should
   (eq 'drawer
       (org-test-with-temp-text
        ":DRAWER:\ntest\n<point>:END:\n"
        (org-element-type (org-element-at-point))))))

(ert-deftest test-org-element/context ()
  "Test `org-element-context' specifications."
  ;; Return closest object containing point.
  (should
   (eq 'underline
       (org-test-with-temp-text "Some *text with _under<point>line_ text*"
	 (org-element-type (org-element-context)))))
  ;; Find objects in secondary strings.
  (should
   (eq 'underline
       (org-test-with-temp-text "* Headline _<point>with_ underlining"
	 (org-element-type (org-element-context)))))
  ;; Should not find objects in headline tags
  (should-not
   (eq 'timestamp
       (org-test-with-temp-text "* Headline <2020-04-01>        :tag1<point>:"
         (org-element-type (org-element-context)))))
  ;; Find objects in objects.
  (should
   (eq 'macro
       (org-test-with-temp-text "| a | {<point>{{macro}}} |"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'table-cell
       (org-test-with-temp-text "| a | b<point> {{{macro}}} |"
	 (org-element-type (org-element-context)))))
  ;; Find objects in item tags.
  (should
   (eq 'bold
       (org-test-with-temp-text "- *bo<point>ld* ::"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'bold
       (org-test-with-temp-text "- *bold* ::<point>"
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'bold
       (org-test-with-temp-text "- *bold* ::\n<point>"
	 (org-element-type (org-element-context)))))
  ;; Do not find objects in table rules.
  (should
   (eq 'table-row
       (org-test-with-temp-text "| a | b |\n|-<point>--|---|\n| c | d |"
	 (org-element-type (org-element-context)))))
  ;; Find objects in parsed affiliated keywords.
  (should
   (eq 'macro
       (org-test-with-temp-text "#+CAPTION: {<point>{{macro}}}\n| a | b |"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'bold
       (org-test-with-temp-text "#+caption: *<point>bold*\nParagraph"
	 (org-element-type (org-element-context)))))
  ;; Find objects at the end of buffer.
  (should
   (eq 'bold
       (org-test-with-temp-text "*bold*"
	 (goto-char (point-max))
	 (org-element-type (org-element-context)))))
  ;; Correctly set `:parent' property.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "Some *bold<point>* text"
	 (org-element-type
	  (org-element-property :parent (org-element-context))))))
  ;; Between two objects, return the second one.
  (should
   (eq 'macro
       (org-test-with-temp-text "<<target>><point>{{{test}}}"
	 (org-element-type (org-element-context)))))
  ;; Test optional argument.
  (should
   (eq 'underline
       (org-test-with-temp-text "Some *text with _under<point>line_ text*"
	 (org-element-type (org-element-context (org-element-at-point))))))
  ;; Special case: bold object at the beginning of a headline.
  (should
   (eq 'bold
       (org-test-with-temp-text "* *bo<point>ld*"
	 (org-element-type (org-element-context)))))
  ;; Special case: incomplete cell at the end of a table row.
  (should
   (eq 'table-cell
       (org-test-with-temp-text "|a|b|c<point>"
	 (org-element-type (org-element-context)))))
  ;; Special case: objects in inline footnotes.
  (should
   (eq 'link
       (org-test-with-temp-text "[fn::[[<point>https://orgmode.org]]]"
	 (org-element-type (org-element-context)))))
  ;; Special case: tags looking like a link.
  (should-not
   (eq 'link
       (org-test-with-temp-text "* Headline :file<point>:tags:"
	 (org-element-type (org-element-context)))))
  (should
   (eq 'link
       (org-test-with-temp-text "* Headline :file<point>:tags: :real:tag:"
	 (org-element-type (org-element-context)))))
  ;; Do not parse partial export snippets.
  (should-not
   (eq 'export-snippet
       (org-test-with-temp-text
           "<point>@@latex:\n\nparagraph\n\n@@"
         (org-element-type (org-element-context))))))



;;; Test Tools

(ert-deftest test-org-element/lineage ()
  "Test `org-element-lineage' specifications."
  ;; Regular tests.  When applied to an element or object returned by
  ;; `org-element-at-point' or `org-element-context', the list is
  ;; limited to the current section.
  (should
   (equal '(paragraph center-block section headline headline org-data)
	  (org-test-with-temp-text
	      "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	    (mapcar #'car (org-element-lineage (org-element-context))))))
  (should
   (equal '(paragraph center-block section headline headline org-data)
	  (org-test-with-temp-text
	      "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	    (mapcar #'car
		    (org-element-lineage
		     (org-element-map (org-element-parse-buffer) 'bold
		       #'identity nil t))))))
  ;; Test TYPES optional argument.
  (should
   (eq 'center-block
       (org-test-with-temp-text
	   "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	 (org-element-type
	  (org-element-lineage (org-element-context) 'center-block)))))
  (should-not
   (org-test-with-temp-text
       "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
     (org-element-lineage (org-element-context) '(example-block))))
  ;; Test WITH-SELF optional argument.
  (should
   (equal '(bold paragraph center-block section headline headline org-data)
	  (org-test-with-temp-text
	      "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	    (mapcar #'car (org-element-lineage (org-element-context) nil t)))))
  ;; When TYPES and WITH-SELF are provided, the latter is also checked
  ;; against the former.
  (should
   (org-test-with-temp-text
       "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
     (org-element-lineage (org-element-context) '(bold) t))))

(ert-deftest test-org-element/lineage-map ()
  "Test `org-element-lineage-map' specifications."
  (should
   (equal '(paragraph center-block section headline headline org-data)
	  (org-test-with-temp-text
	      "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	    (org-element-lineage-map (org-element-context) #'org-element-type))))
  ;; WITH-SELF.
  (should
   (equal '(bold paragraph center-block section headline headline org-data)
	  (org-test-with-temp-text
	      "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	    (org-element-lineage-map (org-element-context) #'org-element-type nil t))))
  ;; FUN as a Lisp form.
  (should
   (equal '("H2" "H1")
	  (org-test-with-temp-text
	      "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	    (org-element-lineage-map
                (org-element-context)
                '(org-element-property :raw-value node)))))
  ;; FIRST-MATCH
  (should
   (equal "H2"
	  (org-test-with-temp-text
	      "* H1\n** H2\n#+BEGIN_CENTER\n*bold<point>*\n#+END_CENTER"
	    (org-element-lineage-map
                (org-element-context)
                '(org-element-property :raw-value node)
              nil nil t)))))

(ert-deftest test-org-element/property-inherited ()
  "Test `org-element-property-inherited' specifications."
  ;; Property without self.
  (should
   (equal
    'bar
    (org-element-property-inherited
     :foo
     (car (org-element-contents
           (org-element-create
            'parent '(:foo bar)
            (org-element-create 'child '(:foo baz))))))))
  ;; With self
  (should
   (equal
    'baz
    (org-element-property-inherited
     :foo
     (car (org-element-contents
           (org-element-create
            'parent '(:foo bar)
            (org-element-create 'child '(:foo baz)))))
     'with-self)))
  ;; ACCUMULATE non-nil.
  (should
   (equal
    '(bar baz)
    (org-element-property-inherited
     :foo
     (car (org-element-contents
           (org-element-create
            'parent '(:foo bar)
            (org-element-create 'child '(:foo baz)))))
     'with-self 'accumulate)))
  ;; LITERAL-NIL.
  (should-not
   (org-element-property-inherited
    :foo
    (org-element-create 'child '(:foo "nil"))
    'with-self nil t))
  (should
   (org-element-property-inherited
    :foo
    (org-element-create 'child '(:foo "nil"))
    'with-self))
  ;; INCLUDE-NIL
  (should-not
   (org-element-property-inherited
    :foo
    (org-element-map
        (thread-last
          (org-element-create 'grandchild '(:foo baz))
          (org-element-create 'child '(:foo nil))
          (org-element-create 'parent '(:foo bar)))
        'grandchild #'identity nil t)
    nil nil nil t))
  (should
   (eq
    'bar
    (org-element-property-inherited
     :foo
     (org-element-map
         (thread-last
           (org-element-create 'grandchild '(:foo baz))
           (org-element-create 'child '(:foo nil))
           (org-element-create 'parent '(:foo bar)))
         'grandchild #'identity nil t))))
  ;; INCLUDE-NIL in accumulated.
  (should
   (equal
    '(bar nil baz)
    (org-element-property-inherited
     :foo
     (org-element-map
         (thread-last
           (org-element-create 'grandchild '(:foo baz))
           (org-element-create 'child '(:foo nil))
           (org-element-create 'parent '(:foo bar)))
         'grandchild #'identity nil t)
     'with-self 'accumulate nil t)))
  ;; PROPERTY as a list.
  (should
   (equal
    '(bar value)
    (org-element-property-inherited
     '(:foo :extra)
     (car (org-element-contents
           (org-element-create
            'parent '(:foo bar :extra value)
            (org-element-create 'child '(:foo baz)))))
     nil 'accumulate)))
  ;; Append list values
  (should
   (equal
    '(bar value value2)
    (org-element-property-inherited
     '(:foo :extra)
     (car (org-element-contents
           (org-element-create
            'parent '(:foo bar :extra (value value2))
            (org-element-create 'child '(:foo baz)))))
     nil 'accumulate))))


;;; Test Cache.
(ert-deftest test-org-element/cache-map ()
  "Test `org-element-cache-map'."
  (org-test-with-temp-text "* headline\n:DRAWER:\nparagraph\n:END:\n* headline 2"
    (should
     (equal
      '(org-data headline section drawer paragraph headline)
      (org-element-cache-map #'car :granularity 'element))))
  (should
   (equal
    '(org-data headline section drawer paragraph)
    (org-test-with-temp-text "* headline\n:DRAWER:\nparagraph\n:END:"
      (org-element-cache-map #'car :granularity 'element)))))

(ert-deftest test-org-element/cache ()
  "Test basic expectations and common pitfalls for cache."
  ;; Shift positions.
  (should
   (equal '(18 . 23)
	  (org-test-with-temp-text "para1\n\npara2\n\npara3"
	    (let ((org-element-use-cache t))
	      (save-excursion (goto-char (point-max)) (org-element-at-point))
	      (insert "add")
	      (forward-line 4)
	      (let ((element (org-element-at-point)))
		(cons (org-element-property :begin element)
		      (org-element-property :end element)))))))
  ;; Partial shifting: when the contents of a greater element are
  ;; modified, only shift ending positions.
  (should
   (org-test-with-temp-text
       "#+BEGIN_CENTER\nPara1\n\nPara2\n\nPara3\n#+END_CENTER"
     (let ((org-element-use-cache t))
       (save-excursion (search-forward "3") (org-element-at-point))
       (search-forward "Para2")
       (insert " ")
       (let ((element (org-element-property :parent (org-element-at-point))))
	 (equal (cons (org-element-property :begin element)
		      (org-element-property :end element))
		(cons (point-min) (point-max)))))))
  ;; Re-parent shifted elements.
  (should
   (eq 'item
       (org-test-with-temp-text "- item\n\n\n  para1\n  para2"
	 (let ((org-element-use-cache t))
	   (end-of-line)
	   (org-element-at-point)
	   (save-excursion (goto-char (point-max)) (org-element-at-point))
	   (forward-line)
	   (delete-char 1)
	   (goto-char (point-max))
	   (org-element-type
	    (org-element-property :parent (org-element-at-point)))))))
  ;; Preserve local structures when re-parenting.
  (should
   (eq 'table
       (let ((org-element-use-cache t))
	 (org-test-with-temp-text
	     "#+begin_center\nP0\n\n<point>\n\n  P1\n  | a | b |\n  | c | d |\n#+end_center"
	   (save-excursion (search-forward "| c |") (org-element-at-point))
	   (insert "- item")
	   (search-forward "| c |")
	   (beginning-of-line)
	   (org-element-type
	    (org-element-property :parent (org-element-at-point)))))))
  (should-not
   (eq 'center-block
       (org-test-with-temp-text
	   "#+begin_center\nP0\n\n<point>\n\n  P1\n  | a | b |\n#+end_center"
	 (let ((org-element-use-cache t))
	   (save-excursion (search-forward "| a |") (org-element-at-point))
	   (insert "- item")
	   (search-forward "| a |")
	   (beginning-of-line)
	   (org-element-type
	    (org-element-property :parent (org-element-at-point)))))))
  ;; When re-parenting, also propagate changes to list structures.
  (should
   (= 2
      (org-test-with-temp-text "\n  Para\n  - item<point>"
	(let ((org-element-use-cache t))
	  (org-element-at-point)
	  (goto-char (point-min))
	  (insert "- Top\n")
	  (search-forward "- item")
	  (beginning-of-line)
	  (length (org-element-property :structure (org-element-at-point)))))))
  ;; Modifying the last line of an element alters the element below.
  (should
   (org-test-with-temp-text "para1\n\npara2"
     (let ((org-element-use-cache t))
       (goto-char (point-max))
       (org-element-at-point)
       (forward-line -1)
       (insert "merge")
       (let ((element (org-element-at-point)))
	 (equal (cons (org-element-property :begin element)
		      (org-element-property :end element))
		(cons (point-min) (point-max)))))))
  ;; Modifying the first line of an element alters the element above.
  (should
   (org-test-with-temp-text ": fixed-width\n:not-fixed-width"
     (let ((org-element-use-cache t))
       (goto-char (point-max))
       (org-element-at-point)
       (search-backward ":")
       (forward-char)
       (insert " ")
       (let ((element (org-element-at-point)))
	 (equal (cons (org-element-property :begin element)
		      (org-element-property :end element))
		(cons (point-min) (point-max)))))))
  (org-test-with-temp-text ":DRAWER:\ntest\n:END:\n <point>#\nParagraph"
    (let ((org-element-use-cache t))
      (org-element-cache-map #'ignore :granularity 'element)
      (should (eq 'comment (org-element-type (org-element-at-point))))
      (should (eq 0 (org-element-property :post-blank (org-element-at-point (point-min)))))
      (insert " ") (delete-char -1)
      (org-element-cache-map #'ignore :granularity 'element)
      (delete-char 1)
      (should (eq 1 (org-element-property :post-blank (org-element-at-point (point-min)))))))
  ;; Sensitive change: adding a line alters document structure both
  ;; above and below.
  (should
   (eq 'example-block
       (org-test-with-temp-text "#+BEGIN_EXAMPLE\nPara1\n\nPara2\n"
	 (let ((org-element-use-cache t))
	   (goto-char (point-max))
	   (org-element-at-point)
	   (insert "#+END_EXAMPLE")
	   (search-backward "Para1")
	   (org-element-type (org-element-at-point))))))
  (should
   (eq 'example-block
       (org-test-with-temp-text "Para1\n\nPara2\n#+END_EXAMPLE"
	 (let ((org-element-use-cache t))
	   (save-excursion (goto-char (point-max)) (org-element-at-point))
	   (insert "#+BEGIN_EXAMPLE\n")
	   (search-forward "Para2")
	   (org-element-type (org-element-at-point))))))
  ;; Sensitive change: removing a line alters document structure both
  ;; above and below.
  (should
   (eq 'example-block
       (org-test-with-temp-text
	   "# +BEGIN_EXAMPLE\nPara1\n\nPara2\n#+END_EXAMPLE"
	 (let ((org-element-use-cache t))
	   (save-excursion (goto-char (point-max)) (org-element-at-point))
	   (forward-char)
	   (delete-char 1)
	   (search-forward "Para2")
	   (org-element-type (org-element-at-point))))))
  (should
   (eq 'example-block
       (org-test-with-temp-text
	   "#+BEGIN_EXAMPLE\nPara1\n\nPara2\n# +END_EXAMPLE"
	 (let ((org-element-use-cache t))
	   (save-excursion (goto-char (point-max)) (org-element-at-point))
	   (search-forward "# ")
	   (delete-char -1)
	   (search-backward "Para1")
	   (org-element-type (org-element-at-point))))))
  ;; Make sure that we do not generate intersecting elements.
  (should (eq 'paragraph
              (org-test-with-temp-text ":DRAWER:\nP1\n<point>\n:END:\n#+END_EXAMPLE"
                (let ((org-element-use-cache t))
                  (org-element-at-point (point-max))
                  (org-element-at-point)
                  (insert "#+BEGIN_EXAMPLE")
                  (org-element-type (org-element-at-point))))))
  ;; But yet correctly slurp obsolete elements inside a new element.
  (should (eq 'example-block
              (org-test-with-temp-text ":DRAWER:\nP1\n<point>\nP2\n#+END_EXAMPLE\n:END:"
                (let ((org-element-use-cache t))
                  (org-element-at-point (point-max))
                  (save-excursion
                    (re-search-forward "P2")
                    (should (eq 'paragraph (org-element-type (org-element-at-point))))
                    (re-search-forward "END_")
                    (should (eq 'paragraph (org-element-type (org-element-at-point)))))
                  (insert "#+BEGIN_EXAMPLE")
                  (re-search-forward "P2")
                  (should (eq 'example-block (org-element-type (org-element-at-point))))
                  (re-search-forward "END_")
                  (org-element-type (org-element-at-point))))))
  ;; Test edits near :end of element
  (should-not (eq 'headline
                  (org-test-with-temp-text "* H1\nP1\n<point>*H2\n"
                    (let ((org-element-use-cache t))
                      (org-element-cache-map #'ignore :granularity 'element)
                      (insert "Blah")
                      (org-element-type (org-element-at-point))))))
  (should-not (eq 'headline
                  (org-test-with-temp-text "* H1\nP1\n<point>*H2\n"
                    (let ((org-element-use-cache t))
                      (org-element-cache-map #'ignore :granularity 'element)
                      (backward-delete-char 1)
                      (org-element-type (org-element-at-point))))))
  (org-test-with-temp-text "Paragraph.\n #<point> comment"
    (let ((org-element-use-cache t))
      (org-element-cache-map #'ignore :granularity 'element)
      (should (eq 'comment (org-element-type (org-element-at-point))))
      (insert "not comment anymore")
      (org-element-cache-map #'ignore :granularity 'element)
      (should-not (eq 'comment (org-element-type (org-element-at-point))))
      (should (eq (org-element-at-point) (org-element-at-point 1)))))
  (should (eq 'headline
              (org-test-with-temp-text "* H1\nP1\n<point>* H2\n"
                (let ((org-element-use-cache t))
                  (org-element-cache-map #'ignore :granularity 'element)
                  (insert "Blah\n")
                  (org-element-type (org-element-at-point))))))
  ;; Corner case: watch out drawers named "PROPERTIES" as they are
  ;; fragile, unlike to other drawers.
  (should
   (eq 'node-property
       (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:A<point>\n:END:"
	 (let ((org-element-use-cache t))
	   (org-element-at-point)
	   (insert "+:")
	   (org-element-type (org-element-at-point))))))
  ;; Properly handle elements not altered by modifications but whose
  ;; parents were removed from cache.
  (should
   (org-test-with-temp-text
       "Paragraph\n\n\n\n#+begin_center\n<point>contents\n#+end_center"
     (let ((org-element-use-cache t)
	   (parent-end (point-max)))
       (org-element-at-point)
       (save-excursion (search-backward "Paragraph")
		       (forward-line 2)
		       (insert "\n  "))
       (eq (org-element-property
	    :end (org-element-property :parent (org-element-at-point)))
	   (+ parent-end 3))))))

(ert-deftest test-org-element/cache-affiliated ()
  "Test updating affiliated keywords."
  ;; Inserting a line right after other keywords.
  (let ((org-element-use-cache t))
    (org-test-with-temp-text "
#+caption: test
#+name: test
<point>
line"
      (org-element-cache-map #'ignore :granularity 'element)
      (should (eq 'keyword (org-element-type (org-element-at-point))))
      (insert "1")
      (should (eq 2 (org-element-property :begin (org-element-at-point)))))))

(ert-deftest test-org-element/cache-table ()
  "Test handling edits in tables."
  ;; Unindented second row of the table should not be re-parented by
  ;; inserted item.
  (should
   (eq 'table
       (let ((org-element-use-cache t))
	 (org-test-with-temp-text
	  "#+begin_center
P0

<point>

  P1
  | a | b |
| c | d |
#+end_center"
	  (save-excursion (search-forward "| c |") (org-element-at-point))
	  (insert "- item")
	  (search-forward "| c |")
	  (beginning-of-line)
	  (org-element-type (org-element-at-point))))))
  (should
   (eq 'table
       (let ((org-element-use-cache t))
	 (org-test-with-temp-text
	  "
- item 1

<point>
  | a | b |
| c | d |
#+end_center"
	  (save-excursion (search-forward "| c |") (org-element-at-point))
          (delete-char 1)
	  (search-forward "| c |")
	  (beginning-of-line)
	  (org-element-type (org-element-at-point))))))
  (should
   (eq 'table-row
       (let ((org-element-use-cache t))
	 (org-test-with-temp-text
	  "
- item 1
<point>
  | a | b |
| c | d |
#+end_center"
	  (save-excursion (search-forward "| c |") (org-element-at-point))
          (insert "\n")
	  (search-forward "| c |")
	  (beginning-of-line)
	  (org-element-type (org-element-at-point)))))))

(ert-deftest test-org-element/cache-headline ()
  "Test basic expectations and common pitfalls for cached headings."
  ;; Appending to final headline in a subtree.
  (org-test-with-temp-text
      "
* Heading
Aliquam erat volutpat.

*** Subheading
** Another
** Final
:PROPERTIES:
:ID: some
<point>:END:
* Heading 2
** End
"
    (let ((org-element-use-cache t))
      (org-element-at-point)
      (save-excursion
        (goto-char (point-max))
        (org-element-at-point))
      (insert ":CATEOGORY: cat\n")
      (search-backward "* Heading")
      (should
       (eq (org-element-property :end (org-element-at-point))
           (save-excursion
             (search-forward "* Heading 2")
             (line-beginning-position))))
      (search-forward "* Heading 2")
      (beginning-of-line)
      (insert "\n\n")
      (search-backward "* Heading")
      (should
       (eq (org-element-property :end (org-element-at-point))
           (save-excursion
             (search-forward "* Heading 2")
             (line-beginning-position))))))
  ;; Appending at eob.
  (org-test-with-temp-text
      "
* Heading
*** Sub-heading
** Another
*** 1
***** 2
** 3
 Aenean in sem ac leo mollis blandit.


<point>"
    (let ((org-element-use-cache t))
      (org-element-at-point (point-max))
      (insert "\n\nTest\n")
      (search-backward "* Heading")
      (should
       (eq (point-max)
           (org-element-property :end (org-element-at-point))))))
  ;; Breaking headline at eob.
  (org-test-with-temp-text
      "
* Heading
*** Sub-heading
<point>"
    (let ((org-element-use-cache t))
      (org-element-at-point (point-max))
      (insert "* heading 2")
      (beginning-of-line)
      (should
       (eq (point-max)
           (org-element-property :end (org-element-at-point))))
      (delete-char 1)
      (search-backward "* Heading")
      (should
       (eq (point-max)
           (org-element-property :end (org-element-at-point))))))
  ;; Inserting low-level headline in-between.
  (org-test-with-temp-text
      "
* Heading
*** Sub-heading
<point>
*** Sub-heading 2
*** Sub-heading 3
"
    (let ((org-element-use-cache t))
      (org-element-at-point (point-max))
      (insert "** heading 2")
      (search-forward "*** Sub-heading 2")
      (should
       (equal (org-element-property :parent (org-element-at-point))
              (progn
                (search-backward "** heading 2")
                (org-element-at-point))))))
  ;; Test when `org-element--cache-for-removal' modifies common parent
  ;; (`org-data' in this case) around changed region.
  (org-test-with-temp-text
      "blah
:DRAWER:
<point>test
:END:
paragraph
* headline"
    (let ((org-element-use-cache t))
      (org-element-at-point (point-max))
      (delete-region (point) (point-max))
      (should (eq 'paragraph (org-element-type (org-element-at-point))))))
  ;; Remove/re-introduce heading.
  (org-test-with-temp-text
      "
* 1
** 1-1
a
<point>** 1-2
a
"
    (let ((org-element-use-cache t))
      (org-element-at-point (point-max))
      (insert "FOO")
      (should
       (equal
        "1-1"
        (org-element-property
         :title
         (org-element-lineage (org-element-at-point) '(headline)))))
      (insert "\n")
      (should
       (equal
        "1"
        (org-element-property
         :title
         (org-element-lineage (org-element-at-point) '(headline))))))))

(ert-deftest test-org-element/cache-ignored-locals ()
  "Test `org-element-ignored-local-variables' value.
Anything holding element cache state must not be copied around
buffers, as in `org-element-copy-buffer' or
`org-export-copy-buffer'.  Otherwise, we may encounter
hard-to-debug errors when cache state is either not up-to-date or
modified by side effect, influencing the original values."
  (mapatoms
   (lambda (var)
     (when (and (boundp var)
                (symbol-value var)
                (string-match-p "^org-element--cache" (symbol-name var))
                (not (memq var '(org-element--cache-interrupt-C-g-max-count
                               org-element--cache-map-statistics-threshold
                               org-element--cache-variables
                               org-element--cache-interrupt-C-g-count
                               org-element--cache-interrupt-C-g
                               org-element--cache-element-properties
                               org-element--cache-sensitive-re
                               org-element--cache-hash-size
                               org-element--cache-non-modifying-commands
                               org-element--cache-self-verify-frequency
                               org-element--cache-diagnostics-level))))
       (should (memq var org-element-ignored-local-variables))))))

(ert-deftest test-org-element/cache-get-key ()
  "Test `org-element-cache-get-key' and `org-element-cache-store-key'."
  (org-test-with-temp-text
      "* Heading
Paragraph
with text <point>

Another paragraph."
    (org-element-cache-store-key
     (org-element-lineage (org-element-at-point) '(headline))
     :robust-key 'val-robust 'robust)
    (org-element-cache-store-key
     (org-element-lineage (org-element-at-point) '(headline))
     :fragile-key 'val-fragile)
    (insert "and more text.")
    (should (eq 'val-robust
                (org-element-cache-get-key
                 (org-element-lineage (org-element-at-point) '(headline))
                 :robust-key)))
    (should (eq 'not-found
                (org-element-cache-get-key
                 (org-element-lineage (org-element-at-point) '(headline))
                 :fragile-key 'not-found))))
  ;; No length change in the altered.
  (org-test-with-temp-text
      "* Heading
Paragraph
<point>with text

Another paragraph."
    (org-element-cache-store-key
     (org-element-lineage (org-element-at-point) '(headline))
     :robust-key 'val-robust 'robust)
    (org-element-cache-store-key
     (org-element-lineage (org-element-at-point) '(headline))
     :fragile-key 'val-fragile)
    (search-forward "with")
    (org-combine-change-calls (match-beginning 0) (match-end 0)
      (replace-match "asdf"))
    (should (eq 'val-robust
                (org-element-cache-get-key
                 (org-element-lineage (org-element-at-point) '(headline))
                 :robust-key)))
    (should (eq 'not-found
                (org-element-cache-get-key
                 (org-element-lineage (org-element-at-point) '(headline))
                 :fragile-key 'not-found)))))

(provide 'test-org-element)

;;; test-org-element.el ends here
