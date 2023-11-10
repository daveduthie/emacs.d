;; -*- lexical-binding: t; -*-
;;; Org mode tweaks

(use-package org
  :defer 1
  :bind (:map org-mode-map
              ("<tab>" . org-cycle))
  :config
  (require 'org-tempo)                  ; enable `< s TAB` shortcuts

  (setq org-agenda-files "~/Documents/org/agenda-files.org"
        org-directory "~/Documents/org/"

        org-clock-report-include-clocking-task t

        org-todo-keywords
        '((sequence
           "PROJ(p)"
           "HOLD(h)" "TODO(t)" "STRT(s)" "WAIT(w)"
           "|" "DONE(d)" "KILL(k)"))

        ;; Refile tweak
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-agenda-custom-commands
        '(("d" "Day"
           (;; One block with a standard agenda view
            (agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day "-0d")
                        (org-agenda-start-on-weekday nil)))
            ;; My top priority for the day
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "Top priority")
                        (org-agenda-skip-function
                         '(org-agenda-skip-if nil '(scheduled deadline)))))
            ;; Unprocessed inbox items
            (tags "inbox"
                  ((org-agenda-overriding-header "Inbox")
                   (org-agenda-max-entries 5)))
            ;; In-flight tasks
            (todo "STRT|WAIT"
                  ((org-agenda-overriding-header "In progress")
                   (org-agenda-skip-function
                    '(or (org-agenda-skip-if nil '(scheduled deadline))
                         (org-agenda-skip-entry-if 'regexp "\\[#A]")))))
            ;; Next actions
            (todo "TODO"
                  ((org-agenda-overriding-header "Next actions")
                   (org-agenda-max-entries 5)
                   (org-agenda-skip-function
                    '(or (org-agenda-skip-if nil '(scheduled deadline))
                         (org-agenda-skip-entry-if 'regexp "\\[#A]"))))))
           nil)))

  (setq org-capture-templates
        '(("i" "Inbox" entry (file "inbox.org"))
          ("m" "Meeting" entry (file+olp+datetree "notes.org")
           "* [M] %?"
           :tree-type month
           :clock-in t
           :clock-resume t)
          ("s" "Story" entry (file+olp+datetree "todo.org")
           "* TODO [I] %^{ISSUE_NUMBER|PL-}
  SCHEDULED: %T
  https://lifecheq.youtrack.cloud/issue//%\\1
  %?"
           :tree-type month)
          ("r" "Review" entry (file+olp+datetree "todo.org")
           "* TODO [R] %?
  SCHEDULED: %T"
           :tree-type month)
          ("c" "Current clock" entry (clock))))

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

(use-package org-indent-mode
  :hook org-mode)

(use-package org-modern
  :after org
  :config (global-org-modern-mode 1))

(use-package springbok
  :load-path "lisp/springbok")

(provide 'dd-org)
