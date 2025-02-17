;; -*- lexical-binding: t; -*-
;;; Org mode tweaks

(defun dd/org-agenda-default ()
  (interactive)
  (org-agenda nil "d"))

(defun dd/setup-org ()
  (setq-local line-spacing 0.1)
  (setq-local truncate-lines nil))

(use-package org
  :defer t
  :delight

  :bind (("C-c a" . #'dd/org-agenda-default)
	 ("C-c c" . #'org-capture)
	 :map org-mode-map
	 ("<tab>" . org-cycle)
	 ("M-." . #'org-open-at-point)
	 ("M-," . #'org-mark-ring-goto))

  :hook (org-mode . dd/setup-org)

  :config

  (setq org-agenda-files "~/Documents/org/agenda-files.org")
  (setq org-directory "~/Documents/org/")
  (setq org-clock-report-include-clocking-task t)

  (setq org-todo-keywords
	'((sequence
	   "TODO(t)" "STRT(s)" "WAIT(w)" "HOLD(h)" "PROJ(p)"
	   "|" "DONE(d)" "KILL(k)")))

  (setq org-log-done 'time)

  ;; Refile tweak
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-agenda-custom-commands
	'(("d" "Default"
	   (;; One block with a standard agenda view
	    (agenda "" ((org-agenda-span 7)
			(org-agenda-start-day "-0d")
			(org-agenda-start-on-weekday nil)))
	    ;; My top priority for the day
	    (tags-todo "+PRIORITY=\"A\""
		       ((org-agenda-overriding-header "Top priority")
			(org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))))
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
	   nil)
	  
	  ("g" "Get Things Done (GTD)"
	   ((agenda ""
		    ((org-agenda-skip-function
		      '(org-agenda-skip-entry-if 'deadline))
		     (org-deadline-warning-days 0)))
	    (todo "NEXT"
		  ((org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'deadline))
		   (org-agenda-prefix-format "  %i %-12:c [%e] ")
		   (org-agenda-overriding-header "\nTasks\n")))
	    (agenda nil
		    ((org-agenda-entry-types '(:deadline))
		     (org-agenda-format-date "")
		     (org-deadline-warning-days 7)
		     (org-agenda-skip-function
		      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
		     (org-agenda-overriding-header "\nDeadlines")))
	    (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
			(org-agenda-overriding-header "\nInbox\n")))
	    (tags "CLOSED>=\"<today>\""
		  ((org-agenda-overriding-header "\nCompleted today\n")))))))
  
  (setq org-capture-templates
	`(("i" "Inbox" entry (file "inbox.org") "* TODO %?")
	  ("t" "Task" entry (file+olp+datetree "todo.org") "* TODO %?\nSCHEDULED: %T"
	   :tree-type month)
	  ("c" "Current clock" entry (clock))))

  (setq org-agenda-hide-tags-regexp ".")

  (setq org-agenda-prefix-format
	'((agenda . " %i %?-12t% s")
          (todo   . " %i %-12:c")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))

  (setq org-refile-targets
	'((nil :maxlevel . 3)
	  (org-agenda-files :maxlevel . 3))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)

;;; Make org mode easier on the eyes.
  (mapc
   (lambda (face) (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-code
	 'org-link
	 'org-block
	 'org-table
	 'org-block-begin-line
	 'org-block-end-line
	 'org-meta-line
	 'org-document-info-keyword))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (eshell . t)
     (shell . t))))

(use-package org-tempo
  :after org)

(use-package ox-md
  :after org)

(use-package ox-pandoc
  :after ox)

(use-package org-indent
  :hook org-mode
  :delight)

(use-package org-habit
  :after org)

;; Nice to look up, but screws up navigation, as point does not 'stick' to the
;; beginning of the line when navigating, folding, etc.
(use-package org-modern
  :disabled t
  :after org
  :config
  (global-org-modern-mode 1)
  (setq org-modern-table nil))

(use-package ob-clojure
  :after org
  :config
  (setq org-babel-clojure-backend 'cider))

;; https://emacs.stackexchange.com/a/37974
(defun dd/org-read-datetree-date (d)
  "Parse a time string D and return a date to pass to the datetree functions."
  (let ((dtmp (nthcdr 3 (parse-time-string d))))
    (list (cadr dtmp) (car dtmp) (caddr dtmp))))

(defun dd/org-refile-to-archive-datetree ()
  "Refile an entry to a datetree under an archive."
  (interactive)
  (require 'org-datetree)
  (let ((dest (save-excursion
                (org-datetree-find-date-create
                 (dd/org-read-datetree-date (org-read-date t nil)))
                (point))))
    (org-refile nil nil (list nil (buffer-file-name) nil dest))))

(provide 'dd-org)
