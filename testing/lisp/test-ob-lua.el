;;; test-ob-lua.el --- tests for ob-lua.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2016, 2019 Thibault Marin
;; Authors: Thibault Marin

;; This file is not part of GNU Emacs.

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
(unless (featurep 'ob-lua)
  (signal 'missing-test-dependency '("Support for Lua code blocks")))

(ert-deftest test-ob-lua/simple-value ()
  "Test associative array return by value."
  (should
   (= 2
      (org-test-with-temp-text
	  "#+name: eg
| a   | 1 |
| b   | 2 |

#+header: :results value
#+header: :var x = eg
#+begin_src lua
return x['b']
#+end_src"
        (org-babel-next-src-block)
        (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/simple-output ()
  "Test text output from table."
  (should
   (equal "result: c\n"
	  (org-test-with-temp-text
	      "#+name: eg
| a | b | c | d |

#+header: :results output
#+header: :var x = eg
#+begin_src lua
print('result: ' .. x[1][3])
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))


(ert-deftest test-ob-lua/colnames-yes-header-argument ()
  "Test table passing with `colnames' header."
  (should
   (equal "a"
	  (org-test-with-temp-text
	      "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
#+begin_src lua
return x[1]
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))


(ert-deftest test-ob-lua/colnames-yes-header-argument-pp ()
  "Test table passing with `colnames' header and `pp' option."
  (should
   (equal "a = 12\nb = 13"
	  (org-test-with-temp-text
	      "#+name: eg
| col | val |
|-----+-----|
| a   |  12 |
| b   |  13 |

#+header: :results value pp
#+header: :colnames yes
#+header: :var x = eg
#+begin_src lua
return x
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/colnames-nil-header-argument ()
  "Test table with `colnames' set to `nil'."
  (should
   (equal "1 = a\n2 = b"
	  (org-test-with-temp-text
	      "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
#+header: :results value pp
#+begin_src lua
return x
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/colnames-no-header-argument ()
  "Test table passing without `colnames'."
  (should
   (equal "1 = col\n2 = a\n3 = b"
	  (org-test-with-temp-text
	      "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
#+header: :results value pp
#+begin_src lua
return x
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/result/none ()
  "Test returning nothing."
  (should
   (equal
    ;; FIXME Update `ob-core' to output e.g. "{{{results(n/a)}}}" or
    ;; "{{{results(/no results/)}}}", for the empty verbatim breaks
    ;; e.g. HTML export.
    "src_lua{return} {{{results(==)}}}"
    (org-test-with-temp-text "src_lua{return}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/result/nil ()
  "Test returning nothing."
  (should
   (equal
    "src_lua{return nil} {{{results(=nil=)}}}"
    (org-test-with-temp-text "src_lua{return nil}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/result/nil/multiple ()
  "Test returning multiple nothings."
  (should
   (equal
    "src_lua{return nil, nil} {{{results(=nil\\, nil=)}}}"
    (org-test-with-temp-text "src_lua{return nil, nil}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/result/boolean ()
  "Test returning the boolean values true and false."
  (should
   (equal
    "src_lua{return true} {{{results(=true=)}}}"
    (org-test-with-temp-text "src_lua{return true}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max)))))
  (should
   (equal
    "src_lua{return false} {{{results(=false=)}}}"
    (org-test-with-temp-text "src_lua{return false}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/number/integer ()
  "Test returning integers."
  (should
   (equal
    "src_lua{return 1} {{{results(=1=)}}}"
    (org-test-with-temp-text "src_lua{return 1}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/number/integer/negative ()
  "Test returning negative integers."
  (should
   (equal
    "src_lua{return -1} {{{results(=-1=)}}}"
    (org-test-with-temp-text "src_lua{return -1}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/number/integer/multiple ()
  "Test returning multiple integers at once."
  (should
   (equal
    "src_lua{return 1, 2, 3} {{{results(=1\\, 2\\, 3=)}}}"
    (org-test-with-temp-text "src_lua{return 1, 2, 3}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/number/real ()
  "Test returning real numbers."
  (should
   (equal
    "src_lua{return 1.5} {{{results(=1.5=)}}}"
    (org-test-with-temp-text "src_lua{return 1.5}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/number/real/multiple ()
  "Test returning multiple real numbers at once."
  (should
   (equal
    "src_lua{return 1.5, 2.5, 3.5} {{{results(=1.5\\, 2.5\\, 3.5=)}}}"
    (org-test-with-temp-text "src_lua{return 1.5, 2.5, 3.5}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/number/infinity ()
  "Test returning the infinity."
  (should
   (equal
    "src_lua{return 1 / 0} {{{results(=inf=)}}}"
    (org-test-with-temp-text "src_lua{return 1 / 0}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/string/single-quotes ()
  "Test returning strings in single quotes."
  (should
   (equal
    "src_lua{return 'hello world'} {{{results(=hello world=)}}}"
    (org-test-with-temp-text "src_lua{return 'hello world'}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/string/double-quotes ()
  "Test returning strings in double quotes."
  (should
   (equal
    "src_lua{return \"hello world\"} {{{results(=hello world=)}}}"
    (org-test-with-temp-text "src_lua{return \"hello world\"}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/string/multiple ()
  "Test returning multiple strings at once."
  (should
   (equal
    "src_lua{return 'a', 'b'} {{{results(=a\\, b=)}}}"
    (org-test-with-temp-text "src_lua{return 'a', 'b'}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/string/list-like ()
  "Test returning strings that look like \"(...)\" lists."
  (should
   (equal
    (concat "src_lua{return string.match('A (B) C', '%b()')}"
            " {{{results(=(B)=)}}}")
    (org-test-with-temp-text
        "src_lua{return string.match('A (B) C', '%b()')}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/string/list-like/brackets ()
  "Test returning strings that look like \"[...]\" lists."
  (should
   (equal
    (concat "src_lua{return string.match('A [B] C', '%b[]')}"
            " {{{results(=[B]=)}}}")
    (org-test-with-temp-text
        "src_lua{return string.match('A [B] C', '%b[]')}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/string/list-like/curlies ()
  "Test returning strings that look like \"{...}\" lists."
  (should
   (equal
    (concat "src_lua{return string.match('A {B} C', '%b{}')}"
            " {{{results(={B}=)}}}")
    (org-test-with-temp-text
	"src_lua{return string.match('A {B} C', '%b{}')}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/results/string/list-like/multiple ()
  "Test returning multiple strings that look like \"(...)\" lists."
  (should
   (equal
    (concat "src_lua{return '(A)', '(B)'}"
            " {{{results(=(A)\\, (B)=)}}}")
    (org-test-with-temp-text
        "src_lua{return '(A)', '(B)'}"
      (org-babel-execute-src-block)
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/result/table ()
  "Test returning table references."
  (should
   (equal
    0
    (string-match
     "src_lua{return {}} {{{results(=table: 0x[0-9A-F]+=)}}}"
     (org-test-with-temp-text "src_lua{return {}}"
       (org-babel-execute-src-block)
       (buffer-substring-no-properties (point-min)
                                       (point-max)))))))

(ert-deftest test-ob-lua/result/table/pretty-print ()
  "Test returning and pretty-printing sequential tables."
  (should
   (equal (string-join
           '("#+BEGIN_SRC lua :results pp"
             "return {10, {20, 30, {40, 50}, 60}, 70}"
             "#+END_SRC"
             ""
             "#+RESULTS:"
             ": 1 = 10"
             ": 2 = "                   ; FIXME Trailing space.
             ":   1 = 20"
             ":   2 = 30"
             ":   3 = "                 ; FIXME Trailing space.
             ":     1 = 40"
             ":     2 = 50"
             ":   4 = 60"
             ": 3 = 70"
             "")
           "\n")
          (org-test-with-temp-text
              (string-join
               '("#+BEGIN_SRC lua :results pp"
                 "return {10, {20, 30, {40, 50}, 60}, 70}<point>"
                 "#+END_SRC")
               "\n")
	    (org-babel-execute-src-block)
            (buffer-substring-no-properties (point-min)
                                            (point-max))))))

(ert-deftest test-ob-lua/result/table/pretty-print/sorted ()
  "Test returning and pretty-printing non-sequential tables."
  (should
   (equal (string-join
           '("#+BEGIN_SRC lua :results pp"
             "return {b = 20, c = 30, a = 10}"
             "#+END_SRC"
             ""
             "#+RESULTS:"
             ;; NOTE The keys are sorted alphabetically.
             ": a = 10"
             ": b = 20"
             ": c = 30"
             "")
           "\n")
          (org-test-with-temp-text
              (string-join
               '("#+BEGIN_SRC lua :results pp"
                 "return {b = 20, c = 30, a = 10}"
                 "#+END_SRC")
               "\n")
	    (org-babel-execute-src-block)
            (buffer-substring-no-properties (point-min)
                                            (point-max))))))

(ert-deftest test-ob-lua/results/value-separator ()
  "Test customizing the separator of multiple values."
  ;; TODO Once Org Babel supports returning lists from inline blocks,
  ;; instead of trapping with the user error: "Inline error: list
  ;; result cannot be used", use those for multiple values.
  (should
   (equal
    "src_lua{return 1, 2, 3} {{{results(=1\t2\t3=)}}}"
    (org-test-with-temp-text "src_lua{return 1, 2, 3}"
      (let ((org-babel-lua-multiple-values-separator "\t"))
        (org-babel-execute-src-block))
      (buffer-substring-no-properties (point-min)
                                      (point-max))))))

(ert-deftest test-ob-lua/escaping-quotes ()
  (should
   (equal "A \" B"
          (org-test-with-temp-text "src_lua{return 'A \" B'}"
            (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/no-sessions ()
  (should
   (equal
    '(user-error "Sessions not supported for Lua")
    (should-error
     (org-test-with-temp-text "src_lua[:session 1]{}"
       (org-babel-execute-src-block))
     :type 'user-error))))

(provide 'test-ob-lua)

;;; test-ob-lua.el ends here
