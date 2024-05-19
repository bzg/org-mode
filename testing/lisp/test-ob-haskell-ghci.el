;;; test-ob-haskell-ghci.el --- tests for ob-haskell.el GHCi  -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2024 Free Software Foundation, Inc.
;; Authors: Bruno BARBIER <brubar.cs@gmail.com>

;; This file is part of GNU Emacs.

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
;;

;;;; Useful references
;;
;;  - https://orgmode.org/worg/org-contrib/babel/languages/lang-compat.html
;;  - GHCi manual: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html

;;; Code:
;;

(require 'org-test "../testing/org-test")
(org-test-for-executable "ghci")
(unless (featurep 'haskell-mode)
  (signal 'missing-test-dependency '("haskell-mode")))
(unless (featurep 'haskell)
  (signal 'missing-test-dependency '("haskell")))


;;; Helpers
;;

(defun test-ob-haskell-ghci-checking-buffers (todo)
  "Check some buffer related invariants.."
  (when (get-buffer "*haskell*")
    (error "A buffer named '*haskell*' exists.  Can't safely test haskell blocks"))
  (prog1 (funcall todo)
    (when-let ((hb (get-buffer "*haskell*")))
      ;; We created a "*haskell*" buffer. That shouldn't happen.
      (error "'ob-haskell' created a buffer named '*haskell*'"))))



(defun test-ob-haskell-ghci (args content &optional preamble unprotected)
  "Execute the code block CONTENT in a new GHCi session; return the result.
Add ARGS to the code block argument line.  Insert PREAMBLE
before the code block.  When UNPROTECTED is non-nil, check pre/post conditions."
  (when (listp content)
    (setq content (string-join content "\n")))
  (unless preamble
    (setq preamble ""))
  (let ((todo (lambda ()
                (prog1 (org-test-with-temp-text
                           (concat preamble "\n" "#+begin_src haskell :compile no "
                                   args "\n" "<point>" content "\n#+end_src")
                         (org-babel-execute-src-block))))))
    (if unprotected (funcall todo)
      (test-ob-haskell-ghci-checking-buffers todo))))



;;; Tests


;;;; Hello Worlds.
;;

(ert-deftest ob-haskell/hello-world-value-pure ()
  (should (equal "Hello World!"
                 (test-ob-haskell-ghci "" "\"Hello World!\""))))

(ert-deftest ob-haskell/hello-world-value-IO ()
  (should (equal "Hello World!"
                 (test-ob-haskell-ghci "" "return \"Hello World!\""))))

(ert-deftest ob-haskell/hello-world-output ()
  (should (equal "Hello World!"
                 (test-ob-haskell-ghci ":results output" "putStrLn \"Hello World!\""))))

(ert-deftest ob-haskell/hello-world-output-nothing ()
  ;; GHCi prints the value on standard output.  So, the last value is part of the output.
  (should (equal "Hello World!"
                 (test-ob-haskell-ghci ":results output" "return \"Hello World!\""))))

(ert-deftest ob-haskell/hello-world-output-multilines ()
  (should (equal "Hello World!"
                 (test-ob-haskell-ghci ":results output" "
:{
main :: IO ()
main = putStrLn \"Hello World!\"
:}

main
"))))

;;;; Sessions
;;

(ert-deftest ob-haskell/sessions-must-not-share-variables ()
  "Sessions must not share variables."
  (test-ob-haskell-ghci ":session s1" "x=2" nil)
  (should (equal 2 (test-ob-haskell-ghci ":session s1" "x" nil)))
  (test-ob-haskell-ghci ":session s2" "x=3" nil)
  (should-not (equal 3 (test-ob-haskell-ghci ":session s1" "x" nil)))
  )

(ert-deftest ob-haskell/session-named-none-means-one-shot-sessions ()
  "When no session, use a new session.
\"none\" is a special name that means `no session'."
  (test-ob-haskell-ghci ":session none" "x=2" nil)
  (should-not (equal 2 (test-ob-haskell-ghci ":session \"none\"" "x" nil))))

(ert-deftest ob-haskell/reuse-variables-in-same-session ()
  "Reuse variables between blocks using the same session."
  (test-ob-haskell-ghci ":session s1" "x=2" nil)
  (should (equal 2 (test-ob-haskell-ghci ":session s1" "x"))))

(ert-deftest ob-haskell/may-use-the-*haskell*-session ()
  "The user may use the special *haskell* buffer."
  (when (get-buffer "*haskell*")
    (error "A buffer named '*haskell*' exists.  Can't run this test"))
  (unwind-protect
      (progn
        (test-ob-haskell-ghci ":session *haskell*" "x=2" nil :unprotected)
        (should (equal 2 (test-ob-haskell-ghci ":session *haskell*" "x" nil :unprotected))))
    (with-current-buffer "*haskell*"
      (let ((kill-buffer-query-functions nil)
            (kill-buffer-hook nil))
        (kill-buffer "*haskell*")))))




;;;; Values
;;

(ert-deftest ob-haskell/value-is-the-last-expression ()
  "Return the value of the last expression."
  (should (equal 3 (test-ob-haskell-ghci "" '("1" "1+1" "1+1+1"))))
  (should (equal 3 (test-ob-haskell-ghci "" '("x=1" "y=1+1" "x+y")))))

(ert-deftest ob-haskell/value-is-the-last-expression-2 ()
  "Return the value of the last expression."
  (should (equal 7 (test-ob-haskell-ghci "" "
putStrLn \"a string\"
return \"useless\"
3+4
"))))



(ert-deftest ob-haskell/eval-numbers ()
  "Evaluation of numbers."
  (should (equal 7 (test-ob-haskell-ghci "" "7")))
  (should (equal 7.5 (test-ob-haskell-ghci "" "7.5")))
  (should (equal 10.0 (test-ob-haskell-ghci "" "10::Double")))
  (should (equal 10   (test-ob-haskell-ghci "" "10::Int"))))


(ert-deftest ob-haskell/eval-strings ()
  "Evaluation of strings."
  (should (equal "a string" (test-ob-haskell-ghci "" "\"a string\""))))

;;;; Output without EOL
;;

(ert-deftest ob-haskell/output-without-eol-1 ()
  "Cannot get output from incomplete lines, when entered line by line."
  :expected-result :failed
  (should (equal "123"
                 (test-ob-haskell-ghci ":results output" "
  putStr(\"1\")
  putStr(\"2\")
  putStr(\"3\")
  putStr(\"\\n\")
"))))

(ert-deftest ob-haskell/output-without-eol-2 ()
  "Incomplete output lines are OK when using a multiline block."
  (should (equal "123"
                 (test-ob-haskell-ghci ":results output" "
:{
  do putStr(\"1\")
     putStr(\"2\")
     putStr(\"3\")
     putStr(\"\\n\")
:}
"))))

(ert-deftest ob-haskell/output-without-eol-3 ()
  "Incomplete output lines are OK on one line."
  (should (equal "123"
                 (test-ob-haskell-ghci ":results output" "
do { putStr(\"1\"); putStr(\"2\"); putStr(\"3\"); putStr(\"\\n\") }
"))))

;;;; Local variables
(ert-deftest ob-haskell/let-one-line ()
  "Local definitions on one line."
  (should (equal 6 (test-ob-haskell-ghci "" "let { x=2; y=3 } in x*y"))))

(ert-deftest ob-haskell/let-multilines-1 ()
  "Local definitions on multiple lines."
  (should (equal 6 (test-ob-haskell-ghci "" "
:{
 let { x=2
     ; y=3
     }
 in x*y
:}
"))))

(ert-deftest ob-haskell/let-multilines-2 ()
  "Local definitions on multiple lines, relying on indentation."
  (should (equal 6 (test-ob-haskell-ghci "" "
:{
  let x=2
      y=3
  in x*y
:}
"))))

;;;; Declarations with multiple lines.
(ert-deftest ob-haskell/decl-multilines-1 ()
  "A multiline declaration, then use it."
  (should (equal 3 (test-ob-haskell-ghci "" "
:{
let length' []    = 0
    length' (_:l) = 1 + length' l
:}
length' [1,2,3]
"))))

(ert-deftest ob-haskell/decl-multilines-2 ()
  "A multiline declaration, then use it."
  (should (equal 5 (test-ob-haskell-ghci "" "
:{
length'       :: [a] -> Int
length' []    =  0
length' (_:l) =  1 + length' l
:}

length' [1..5]
"))))


(ert-deftest ob-haskell/primes ()
  "From haskell.org."""
  (should (equal '(2 3 5 7 11 13 17 19 23 29)
                 (test-ob-haskell-ghci "" "
:{
primes = filterPrime [2..] where
  filterPrime (p:xs) =
    p : filterPrime [x | x <- xs, x `mod` p /= 0]
:}

take 10 primes
"))))

;;;; Lists
;;

(ert-deftest ob-haskell/a-simple-list ()
  "Evaluation of list of values."
  (should (equal '(1 2 3) (test-ob-haskell-ghci "" "[1,2,3]"))))


(ert-deftest ob-haskell/2D-lists ()
  "Evaluation of nested lists into a table."
  (should (equal '((1 2 3) (4 5 6))
                 (test-ob-haskell-ghci "" "[[1..3], [4..6]]"))))

(ert-deftest ob-haskell/2D-lists-multilines ()
  "Evaluation of nested lists into a table, as multilines."
  (should (equal '((1 2 3) (4 5 6) (7 8 9))
                 (test-ob-haskell-ghci "" "
:{
[ [1..3]
, [4..6]
, [7..9]
]
:}
"))))


;;;; Tuples
;;

(ert-deftest ob-haskell/a-simple-tuple ()
  "Evaluation of tuple of values."
  (should (equal '(1 2 3) (test-ob-haskell-ghci "" "(1,2,3)"))))


(ert-deftest ob-haskell/2D-tuples ()
  "Evaluation of nested tuples into a table."
  (should (equal '((1 2 3) (4 5 6))
                 (test-ob-haskell-ghci "" "((1,2,3), (4,5,6))"))))

(ert-deftest ob-haskell/2D-tuples-multilines ()
  "Evaluation of nested tuples into a table, as multilines."
  (should (equal '((1 2 3) (4 5 6) (7 8 9))
                 (test-ob-haskell-ghci "" "
:{
( (1,2,3)
, (4,5,6)
, (7,8,9)
)
:}
"))))


;;;; Data tables
;;

(ert-deftest ob-haskell/int-table-data ()
  "From worg: int-table-data."
  (should (equal 10 (test-ob-haskell-ghci ":var t=int-table-data"
                                          "sum [sum r | r <- t]"
                                          "#+name: int-table-data
    | 1 | 2 |
    | 3 | 4 |"))))

(ert-deftest ob-haskell/float-table-data ()
  "From worg: float-table-data."
  (should (equal 11.0 (test-ob-haskell-ghci ":var t=float-table-data"
                                            "sum [sum r | r <- t]"
                                            "#+name: float-table-data
    | 1.1 | 2.2 |
    | 3.3 | 4.4 |"))))

(ert-deftest ob-haskell/string-table-data ()
  "From worg: string-table-data."
  (should (equal "abcd" (test-ob-haskell-ghci ":var t=string-table-data"
                                              "concat [concat r | r <- t]"
                                              "#+name: string-table-data
    | a | b |
    | c | d |"))))

;;;; Reuse results
;;
(ert-deftest ob-haskell/reuse-table ()
  "Reusing a computed tables."
  (should (equal 78 (test-ob-haskell-ghci ":var t=a-table"
                                          "sum [sum r | r <- t]"
                                          "#+name: a-table
#+begin_src haskell
   [ [x..x+2] | x <- [1,4 .. 12] ]
#+end_src
"))))


;;;; Not defined errors
;;

(ert-deftest ob-haskell/not-defined ()
  "Evaluation of undefined variables."
  :expected-result :failed
  (should-error (test-ob-haskell-ghci "" "notDefined :: IO Int")))


(ert-deftest ob-haskell/not-defined-then-defined-1 ()
  "Evaluation of undefined variables.
This is a valid haskell source, but, invalid when entered one
line at a time in GHCi."
  :expected-result :failed
  (should-error (test-ob-haskell-ghci "" "
v :: Int
v = 4
")))


(ert-deftest ob-haskell/not-defined-then-defined-1-fixed ()
  "Like not-defined-then-defined-1, but using the mutiline marks."
  (let ((r (test-ob-haskell-ghci "" "
:{
  v :: Int
  v = 4
:}
")))
    (should (eq nil r))))

(ert-deftest ob-haskell/not-defined-then-defined-1-fixed-2 ()
  "Like not-defined-then-defined-1, but using one line."
  (should (eq nil (test-ob-haskell-ghci "" "v = 4 :: Int"))))



(ert-deftest ob-haskell/not-defined-then-defined-2 ()
  "Evaluation of undefined variables, followed by a correct one."
  ;; ghci output is:
  ;;  | <interactive>:2:1-4: error:
  ;;  |     • Variable not in scope: main :: IO ()
  ;;  |     • Perhaps you meant ‘min’ (imported from Prelude)
  ;;  | Hello, World!
  ;; and ob-haskell just reports the last line "Hello, World!".
  (should (string-match "Variable not in scope"
                        (test-ob-haskell-ghci ":results output" "
main :: IO ()
main = putStrLn \"Hello, World!\"
main
"))))

;;;; Imports
;;

(ert-deftest ob-haskell/import ()
  "Import and use library."
  (should (equal 65 (test-ob-haskell-ghci "" "
import Data.IORef
r <- newIORef 65
readIORef r
"))))

(ert-deftest ob-haskell/import-with-vars ()
  "Import and use library with vars."
  (should (equal 65 (test-ob-haskell-ghci ":var x=65" "
import Data.IORef
r <- newIORef x
readIORef r
"))))

;;;; What is the result?
;;

(ert-deftest ob-haskell/results-value-1 ()
  "Don't confuse output and values: nothing."
  (should (equal nil (test-ob-haskell-ghci ":results value" "return ()"))))

(ert-deftest ob-haskell/results-value-2 ()
  "Don't confuse output and values: a list."
  (should (equal '(1 2) (test-ob-haskell-ghci ":results value" "return [1,2]"))))

(ert-deftest ob-haskell/results-value-3 ()
  "Don't confuse output and values: nothing."
  (should (equal nil (test-ob-haskell-ghci ":results value" "putStrLn \"3\""))))

(ert-deftest ob-haskell/results-value-4 ()
  "Don't confuse output and values: nothing."
  (should (equal nil (test-ob-haskell-ghci ":results value" "
putStrLn \"3\"
return ()
"))))


;;;; GHCi commands
;;

(ert-deftest ob-haskell/ghci-type ()
  "The ghci meta command ':type'."
  (should (equal "n :: Int"
                 (test-ob-haskell-ghci ":results output" "let n=3::Int\n:type n"))))

(ert-deftest ob-haskell/ghci-info ()
  "The ghci meta command ':info' ."
  (should (string-match-p
           "repeat :: a -> \\[a\\][ \t]+-- Defined in ‘GHC.List’"
           (test-ob-haskell-ghci ":results output" ":info repeat"))))


(provide 'test-ob-haskell-ghci)

;;; test-ob-haskell-ghci.el ends here
