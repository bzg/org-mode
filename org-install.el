;;; org-install.el --- Autoloads for org.el

(autoload 'org-mode "org" "Org mode" t)
(autoload 'org-diary "org" "Diary entries from Org mode.")
(autoload 'org-agenda "org" "Multi-file agenda from Org mode." t)
(autoload 'org-store-link "org" "Store a link to the current location." t)
(autoload 'org-open-at-point-global "org" "Follow a link like Org-mode does." t)
(autoload 'org-insert-link-global "org" "Insert a link like Org-mode does." t)
(autoload 'orgtbl-mode "org" "Org tables as a minor mode." t)
(autoload 'turn-on-orgtbl "org" "Org tables as a minor mode.")
(autoload 'orgstruct-mode "org" "Org structure as a minor mode." t)
(autoload 'turn-on-orgstruct "org" "Org structure as a minor mode.")
(autoload 'org-cycle "org" "Subtree visibility cycling." t)
(autoload 'org-global-cycle "org" "Global visibility cycling." t)
(autoload 'org-agenda-list "org" "Produce calendar-like agenda view." t)
(autoload 'org-cycle-agenda-files "org" "Cycle through agenda-files." t)
(autoload 'org-todo-list "org" "Produce global TODO list." t)
(autoload 'org-tags-view "org" "Produce global TAGS agenda view." t)
(autoload 'org-agenda-list-stuck-projects "org" "List stuck projects." t)
(autoload 'org-remember "org" "Call remember or re-apply template" t)
(autoload 'org-remember-annotation "org")
(autoload 'org-remember-apply-template "org")
(autoload 'org-remember-handler "org")
(autoload 'org-export-icalendar-all-agenda-files "org"
  "Export all files in `org-agenda-files' to iCalendar .ics files." t)
(autoload 'org-export-icalendar-combine-agenda-files "org"
  "Export all files in `org-agenda-files' to a single combined iCalendar file." t)

(autoload 'org-export-as-latex-batch "org-export-latex")
(autoload 'org-export-as-latex-to-buffer "org-export-latex"
  "Call `org-exort-as-latex` with output to a temporary buffer" t)
(autoload 'org-replace-region-by-latex "org-export-latex"
  "Replace the region from BEG to END with its LaTeX export." t)
(autoload 'org-export-region-as-latex "org-export-latex"
  "Convert region from BEG to END in `org-mode' buffer to LaTeX." t)
(autoload 'org-export-as-latex "org-export-latex"
  "Export current buffer to a LaTeX file." t)

(autoload 'org-batch-agenda "org")
(autoload 'org-batch-agenda-csv "org")
(autoload 'org-store-agenda-views "org" "Store agenda views to files" t)
(autoload 'org-batch-store-agenda-views "org")

(autoload 'org-publish-current-file "org-publish" "Publish current file." t)
(autoload 'org-publish-current-project "org-publish"
  "Publish all files of current project." t)
(autoload 'org-publish "org-publish" "Publish a project." t)
(autoload 'org-publish-all "org-publish" "Publish all projects." t)

(autoload 'org-run-like-in-org-mode "Run a command like in Org-mode.")

(provide 'org-install)

