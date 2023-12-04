;; -*- lexical-binding: t; -*-
;;; Org mode tweaks

(defun dd/org-agenda-day ()
  (interactive)
  (org-agenda nil "d"))

(use-package org
  :defer t
  :bind (("<f7>" . dd/org-agenda-day)
	 :map org-mode-map
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

(add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.1)))

(eval-after-load 'org
  '(setq org-src-block-faces
	 '(("emacs-lisp" modus-themes-nuanced-magenta)
	   ("elisp" modus-themes-nuanced-magenta)
	   ("clojure" modus-themes-nuanced-magenta)
	   ("clojurescript" modus-themes-nuanced-magenta)
	   ("c" modus-themes-nuanced-blue)
	   ("c++" modus-themes-nuanced-blue)
	   ("sh" modus-themes-nuanced-green)
	   ("shell" modus-themes-nuanced-green)
	   ("html" modus-themes-nuanced-yellow)
	   ("xml" modus-themes-nuanced-yellow)
	   ("css" modus-themes-nuanced-red)
	   ("scss" modus-themes-nuanced-red)
	   ("python" modus-themes-nuanced-green)
	   ("ipython" modus-themes-nuanced-magenta)
	   ("r" modus-themes-nuanced-cyan)
	   ("js" modus-themes-nuanced-cyan)
	   ("yaml" modus-themes-nuanced-cyan)
	   ("conf" modus-themes-nuanced-cyan)
	   ("docker" modus-themes-nuanced-cyan)
	   ("dockerfile" modus-themes-nuanced-cyan))))

;;; Make org mode easier on the eyes. Should this move?

(eval-after-load 'org
  '(mapc
    (lambda (face) (set-face-attribute face nil :inherit 'fixed-pitch))
    (list 'org-code
	  'org-link
	  'org-block
	  'org-table
	  'org-block-begin-line
	  'org-block-end-line
	  'org-meta-line
	  'org-document-info-keyword)))

(use-package ox-md
  :after org-mode)

(use-package org-indent-mode
  :hook org-mode)

(use-package org-modern
  :after org
  :config (global-org-modern-mode 1))

(use-package springbok
  :load-path "lisp/springbok"
  :commands (springbok-make-quick-note springbok-today)
  :bind (("<f8>" . #'springbok-today)))

(provide 'dd-org)
