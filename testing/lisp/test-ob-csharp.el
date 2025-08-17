;;; test-ob-csharp.el --- Tests for ob-csharp        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maximilian Kueffner

;; Author: Maximilian Kueffner <poverobuosodonati@gmail.com>

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

(unless (featurep 'ob-csharp)
  (signal 'missing-test-dependency '("Support for C# code blocks")))

(require 'ob-core)

(org-test-for-executable org-babel-csharp-compiler)

(ert-deftest test-ob-csharp/customized-compile-command-used ()
  "User specified compile command is used."
  (let* ((custom-fun (lambda (p b) (format "custom-compiler %s %s" p b)))
         (project "/tmp/placeholder/dummy.csproj")
         (binary "/tmp/placeholder/bin")
         (default-command (funcall org-babel-csharp-generate-compile-command project binary))
         (cmd-backup org-babel-csharp-generate-compile-command))
    (unwind-protect
        (progn
          (setq org-babel-csharp-generate-compile-command custom-fun)
          (should-not (string=
                       default-command
                       (funcall org-babel-csharp-generate-compile-command project binary)))
          (should (string= (funcall custom-fun project binary)
                           (funcall org-babel-csharp-generate-compile-command project binary))))
      (setq org-babel-csharp-generate-compile-command cmd-backup))))

(ert-deftest test-ob-csharp/customized-restore-command-used ()
  "User specified compile command is used."
  (let* ((custom-fun (lambda (p) (format "custom-restore %s" p)))
         (project "/tmp/placeholder/dummy.csproj")
         (default-command (funcall org-babel-csharp-generate-restore-command project))
         (cmd-backup org-babel-csharp-generate-restore-command))
    (unwind-protect
        (progn
          (setq org-babel-csharp-generate-restore-command custom-fun)
          (should-not (string=
                       default-command
                       (funcall org-babel-csharp-generate-restore-command project)))
          (should (string= (funcall custom-fun project)
                           (funcall org-babel-csharp-generate-restore-command project))))
      (setq org-babel-csharp-generate-restore-command cmd-backup))))

(ert-deftest test-ob-csharp/generate-project-file ()
  "Test intended parameterization of the project file generator."
  (should (stringp (org-babel-csharp--generate-project-file nil "net6.0")))
  (should (stringp (org-babel-csharp--generate-project-file '("a-ref") "net6.0")))
  (should (stringp (org-babel-csharp--generate-project-file '("a-ref" "b-ref") "net6.0")))
  (should-error (org-babel-csharp--generate-project-file nil nil))
  (should-error (org-babel-csharp--generate-project-file '(nil) "net6.0"))
  (should-error (org-babel-csharp--generate-project-file "a-ref" "net6.0")))

(ert-deftest test-ob-csharp/format-usings ()
  "Test intended parameterization of the C# using formatter."
  (should (string=
           "using namespaceA;\nusing namesaceB;"
           (org-babel-csharp--format-usings '("namespaceA" "namesaceB"))))
  (should (string=
           ""
           (org-babel-csharp--format-usings nil)))
  (should-error (org-babel-csharp--format-usings '("namespaceA" nil "namesaceB")))
  (should-error (org-babel-csharp--format-usings "singleUsing")))

(ert-deftest test-ob-csharp/extension-based-reference-types ()
  "Test if the references as supplied to the \"references\" header-arg are correctly parsed."
  (let ((nuget-inp '(("Nugetref" . "0.0.1")))
        (assembly-inp '("assembly.dll"))
        (project-inp '("project.csropj")))
    (should (string= (org-babel-csharp--format-refs nuget-inp) "\n\n  \n\n  <ItemGroup>\n    <PackageReference Include=\"Nugetref\" Version=\"0.0.1\" />\n  </ItemGroup>"))
    (should (string-search "\n\n  <ItemGroup>\n    <Reference Include=\"assembly\">\n      <HintPath>"
                           (org-babel-csharp--format-refs assembly-inp)))
    (should (string= (org-babel-csharp--format-refs project-inp) "\n\n  \n\n  <ItemGroup>\n    <PackageReference Include=\"project.csropj\" />\n  </ItemGroup>"))
    (should-error (org-babel-csharp--format-refs "not-a-list"))))
(ert-deftest test-ob-csharp/custom-class-name-header-argument ()
  "The generated class name matches the provided string or defaults to \"Program\"."
  (let ((cs-block (org-test-with-temp-text
                      "#+begin_src csharp :class \"MyAwesomeClass\"
  Console.WriteLine(\"ok\");
#+end_src"
                    (org-babel-expand-src-block))))
    (should     (string-search "class MyAwesomeClass" cs-block))
    (should-not (string-search "class Program" cs-block))))

(ert-deftest test-ob-csharp/custom-main-function-wrapping ()
  "Lax main function wrapping works."
  (let* ((dummy-block "#+begin_src csharp %s \nvar a = 1 + 1;\n#+end_src")
         (disable-main-quote (org-test-with-temp-text
                                 (format dummy-block ":main \"no\"")
                               (org-babel-expand-src-block)))
         (disable-main-plain (org-test-with-temp-text
                                 (format dummy-block ":main no")
                               (org-babel-expand-src-block)))
         (main-implicit (org-test-with-temp-text
                            (format dummy-block "")
                          (org-babel-expand-src-block)))
         (main-explicit (org-test-with-temp-text
                            (format dummy-block ":main anything")
                          (org-babel-expand-src-block)))
         (str-after-break (lambda (s) (substring s (+ 1 (string-search "\n" s)) -1))))
    (should (equal (funcall str-after-break disable-main-plain) (funcall str-after-break disable-main-quote)))
    (should (equal (funcall str-after-break main-implicit) (funcall str-after-break main-explicit)))
    (should-not (string-search "static void Main" disable-main-quote))
    (should-not (string-search "static void Main" disable-main-plain))
    (should (string-search "static void Main" main-implicit))
    (should (string-search "static void Main" main-explicit))))

(ert-deftest test-ob-csharp/int-from-var ()
  "Test of an integer variable."
  (org-test-with-temp-text "#+begin_src csharp :var i=42 :results silent
  Console.WriteLine(i);
#+end_src"
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/float-from-var ()
  "Test of a float variable."
  (org-test-with-temp-text "#+begin_src csharp :var f=3.14 :results silent
  Console.WriteLine(f);
#+end_src"
    (should (= 3.14 (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/string-from-var ()
  "Test of a string variable."
      (org-test-with-temp-text "#+begin_src csharp :var s=\"pi\" :results silent
  Console.WriteLine(s);
#+end_src"
        (should (string= "pi" (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/outputs-list ()
  "Test list output."
  (org-test-with-temp-text "#+begin_src csharp :results raw list silent
  Console.WriteLine(\"Item 1\");
  Console.WriteLine(\"Item 2\");
  Console.WriteLine(\"Item 3\");
  Console.WriteLine(\"Item 4\");
#+end_src"
    (should (equal "Item 1\nItem 2\nItem 3\nItem 4\n" (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/commandline-input ()
  "Test command line input."
  (org-test-with-temp-text "#+begin_src csharp :cmdline 3 :usings '(\"System\" \"System.Text\") :results silent
  int argInt = 0;
  Int32.TryParse(args[0], out argInt);

  Console.WriteLine(argInt * 14);
#+end_src"
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/custom-class-and-main ()
  "Test custom class with custom main function."
  (org-test-with-temp-text "#+begin_src csharp :class no :main no :results silent
  internal class ClassA
  {
      public ClassA(int i)
      {
          this.AnInt = i;
      }

      public int AnInt { get; set; }
  }

  public class Program
  {
      public static void Main(string[] args)
      {
          ClassA daInstance = new(123);

          Console.WriteLine(daInstance.AnInt);
      }
  }
#+end_src"
    (should (= 123 (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/tabular-format-output ()
  "Test for tabular output format."
  (org-test-with-temp-text "#+begin_src csharp :results table silent
  Console.WriteLine($\"In, questo, mondo, una, cosa\");
  Console.WriteLine($\"si, perde,  una,   si, trova\");
#+end_src"
    (should (equal '(("In" "questo" "mondo" "una" "cosa")
                     ("si" "perde" "una" "si" "trova"))
                   (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/nuget-reference ()
  "Test with nuget reference."
  (org-test-with-temp-text "#+begin_src csharp :references '((\"Newtonsoft.Json\" . \"13.0.3\")) :usings '(\"System\" \"Newtonsoft.Json\") :main no :project \"json-test\" :results verbatim silent
  public class DTO
  {
      public int TheInt { get; set; }
      public string TheString { get; set; }
  }

  static void Main(string[] args)
  {
      DTO myDto = new() { TheInt = 12, TheString = \"ok\" };

      string json = JsonConvert.SerializeObject(myDto, Formatting.Indented);
      Console.WriteLine($\"{json}\");
  }
#+end_src"
    (should (string= "{\n  \"TheInt\": 12,\n  \"TheString\": \"ok\"\n}\n"
                     (org-babel-execute-src-block)))))

(ert-deftest test-ob-csharp/respects-custom-nuget-config ()
  "Check that the provided NuGet.config is taken into account when evaluating a source block."
      (let ((nugetconf (make-temp-file "nuget")))
        (unwind-protect
            (progn
              (with-temp-buffer
                (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <configuration>
      <packageSources>
          <clear />
          <add key=\"local\" value=\"./local_packages\" />
      </packageSources>
  </configuration>")
                (write-file nugetconf))
              (org-test-with-temp-text (format "#+begin_src csharp :references '((\"Newtonsoft.Json\" . \"13.0.3\")) :usings '(\"Newtonsoft.Json.Linq\") :nugetconfig %S :results raw
    var js = JObject.Parse(\"{\\\"TheInt\\\": 12, \\\"TheString\\\": \\\"ok\\\"}\");
    Console.Write(js);
  ,#+end_src" nugetconf)
                (should-error (org-babel-execute-src-block))))
          (delete-file nugetconf))))

(ert-deftest test-ob-csharp/additional-project-flags-fails-with-invalid-syntax ()
  "Compilation fails when the `org-babel-csharp-additional-project-flags' is not xml formatted."
  (unwind-protect
      (progn
        (setq org-babel-csharp-additional-project-flags "somegarbage/>")
        (org-test-with-temp-text "#+begin_src csharp
  Console.WriteLine(\"ok\");
#+end_src"
          (should (eq nil (org-babel-execute-src-block)))))
    (setq org-babel-csharp-additional-project-flags nil)))

(ert-deftest test-ob-csharp/additional-project-flags-executes-with-xml-syntax ()
  "Compilation succeeds when the `org-babel-csharp-additional-project-flags' is xml formatted."
  (unwind-protect
      (progn
        (setq org-babel-csharp-additional-project-flags "<LangVersion>latest</LangVersion>")
        (org-test-with-temp-text "#+begin_src csharp
  Console.WriteLine(\"ok\");
#+end_src"
          (should (string= "ok"
                           (org-babel-execute-src-block)))))
    (setq org-babel-csharp-additional-project-flags nil)))

(ert-deftest test-ob-csharp/prologue-and-epilouge-expanded ()
  "Check if prologue and epilogue are written plain to start and end of the expanded block."
  (org-test-with-temp-text "#+begin_src csharp :prologue \"// File header\" :epilogue \"// file ends here\"
  Console.WriteLine(\"ok\");
#+end_src"
    (let ((block-expand (org-babel-expand-src-block)))
      (should (string= (substring block-expand 0 14) "// File header"))
      (should (string= (substring block-expand -17) "// file ends here")))))

(ert-deftest test-ob-csharp/invalid-additional-project-flags-fail ()
  "An invalid setting in `org-babel-csharp-additional-project-flags' fails."
  (let ((block "#+begin_src csharp\nConsole.WriteLine(1);\n#+end_src")
        (invalid-flags "<UserCustomPoperty>This is an invalid xml string (property not closed)"))
    (unwind-protect
        (progn
          (setq org-babel-csharp-additional-project-flags invalid-flags)
          (should-not (org-test-with-temp-text block (org-babel-execute-src-block))))
      (setq org-babel-csharp-additional-project-flags nil))))

(ert-deftest test-ob-csharp/valid-additional-project-flags-are-respected ()
  "A valid setting of `org-babel-csharp-additional-project-flags' is respected code block compilation."
  (let ((block "#+begin_src csharp\nConsole.WriteLine(1);\n#+end_src")
        (valid-flags "<Configuration>Release</Configuration>"))
    (unwind-protect
        (progn
          (setq org-babel-csharp-additional-project-flags valid-flags)
          (should (= 1 (org-test-with-temp-text block (org-babel-execute-src-block)))))
      (setq org-babel-csharp-additional-project-flags nil))))

;; requires at least 2 dotnet frameworks installed
(ert-deftest test-ob-csharp/framework-header-is-configurable ()
  "Check for additional framework header arguments."
  (skip-when (< (length (org-babel-csharp--find-dotnet-version)) 2))
  (let* ((src-result (lambda (v) (org-test-with-temp-text
                                     (format "#+begin_src csharp :framework \"net%s.0\"
  Console.WriteLine(\"ok\");
#+end_src" v)
                                   (org-babel-execute-src-block))))
         (res-first  (funcall src-result (car (org-babel-csharp--find-dotnet-version))))
         (res-second (funcall src-result (cadr (org-babel-csharp--find-dotnet-version)))))
    (should (string= res-first "ok"))
    (should (string= res-second "ok"))
    (should (string= res-first res-second))
    (should (eq nil (funcall src-result "nonexisting")))))

(ert-deftest test-ob-csharp/runtime-error-without-valid-dotnet-sdk ()
  "Unless there is a valid dotnet SDK found, evaluating a csharp block fails."
  (cl-letf (((symbol-function 'org-babel-csharp--find-dotnet-version) #'ignore))
    (org-test-with-temp-text "#+begin_src csharp
  Console.WriteLine(\"hi\");
#+end_src"
      (should-error (org-babel-execute-src-block)))))


(provide 'test-ob-csharp)
;;; test-ob-csharp.el ends here
