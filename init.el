;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash
  :config (global-dash-fontify-mode))

(use-package eieio)

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook 'indent-spaces-mode))

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?x?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(use-package smerge-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t)))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook 'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

;;; Tequila worms

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;;; Dave packages

(setq ring-function 'ignore)

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(defun dd/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'dd/apply-theme)

(use-package meow
  :defer 1
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-use-clipboard t)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-global-mode 1))

(use-package eglot
  :ensure nil
  :hook ((clojure-mode clojure-ts-mode rust-mode terraform-mode) . eglot-ensure))

(use-package comp
  :config
  (setq native-comp-async-report-warnings-errors nil))

(use-package files
  :config
  (setq confirm-kill-processes nil)
  (setq create-lockfiles nil)
  (setq make-backup-files nil))

(use-package icomplete
  :init
  (fido-mode t)
  (fido-vertical-mode t))

(use-package paredit
  :delight
  :hook (prog-mode . enable-paredit-mode))

(use-package winner
  :init (winner-mode))

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
  :ensure nil
  :hook org-mode)

(use-package org-modern
  :after org
  :config (global-org-modern-mode 1))

(defvar dd/org-daily-path "~/Documents/org/daily")

(defvar dd/org-daily-template
  "\n#+STARTUP: showall")

(defun dd/org-daily-filename (date)
  (concat dd/org-daily-path "/"
          (format-time-string "%Y-%m-%d" date) ".org"))

(defun dd/org-daily (&optional date)
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (setq date (or date (current-time)))
  (find-file (dd/org-daily-filename date))
  (when (= 0 (buffer-size))
    (let ((datestr (format-time-string "#+TITLE: %Y-%m-%d %A" date)))
      (insert datestr)
      (insert dd/org-daily-template))))

(defun dd/org-today ()
  (interactive)
  (dd/org-daily))

(use-package clojure-mode
  :mode ("\\.clj(c|s)?\\'" . clojure-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq markdown-indent-on-enter nil)
  ;; Revisit?
  (setq cider-xref-fn-depth 90))

(use-package zprint-format
  :commands (zprint-format-buffer
	     zprint-format-region
	     zprint-format-on-save-mode))

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode))

(use-package company
  :defer 1
  :delight
  :hook (prog-mode . company-mode))

(use-package eldoc
  :delight)

(use-package autorevert
  :delight arev
  :init (global-auto-revert-mode 1))

(use-package browse-at-remote
  :defer 1
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(custom-set-faces
 '(default ((t (:inherit nil :height 140 :family "Iosevka SS08")))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package tab-bar
  :bind (("M-[" . #'tab-bar-switch-to-prev-tab)
	 ("M-]" . #'tab-bar-switch-to-next-tab)))

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
