;; -*- lexical-binding: t; -*-

(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "locutus")
(setq initial-buffer-choice t)
(setq initial-scratch-message "")
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

(setq-default fill-column 80)

(set-face-attribute 'default nil :inherit nil :height 130 :family "Iosevka SS14")
(set-face-attribute 'variable-pitch nil :inherit 'default :height 1.0 :family "Iosevka Etoile")

(defun dd/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi))
    ('dark (load-theme 'modus-vivendi))))

(use-package modus-themes
  :defer t
  :config (setq modus-themes-mixed-fonts t))

(use-package doom-themes
  :defer t
  :custom-face (cider-result-overlay-face ((t (:box (:line-width -1 :color "orange"))))))

(use-package emacs
  :hook (prog-mode . display-line-numbers-mode)
  :config (add-hook 'ns-system-appearance-change-functions #'dd/apply-theme))

(use-package face-remap
  :disabled t
  :hook (org-mode . (lambda () (variable-pitch-mode t)))
  :delight buffer-face-mode)

(use-package hl-line
  :disabled t
  :defer 1
  :init (global-hl-line-mode t))

;;; Buffer rules

(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq window-sides-slots '(1 1 1 1))

(defconst dd/display-buffer-rules
  (list
   ;; Show shells & references at the bottom of the frame
   `(,(rx (or "*Embark Export"
	      "*SQL"
	      "*cider-repl"
	      "*grep"
	      "*inf-clojure"
	      "*scheme*"
	      "*vterm"
	      "*xref"
	      "shell*"))
     display-buffer-in-side-window
     (side . bottom)
     (window . root)
     (window-height . 0.3))

   ;; Compilation output goes to the left
   `(,(rx "*compilation*")
     display-buffer-in-side-window
     (side . left)
     (slot . 0)
     (window-parameters . ((no-delete-other-windows . t)))
     (window-width . 0.4))

   `((or ,(rx "*Org Agenda*")
	 (and (derived-mode . org-mode)
	      "_journal.org"))
     (display-buffer-in-tab
      display-buffer-in-direction)
     (tab-name . "Org 🏠"))
   ;;
   ))

(setq display-buffer-alist dd/display-buffer-rules)

(defun dd/toggle-ui-rules ()
  (interactive)
  (setq display-buffer-alist
	(if display-buffer-alist
	    nil
	  dd/display-buffer-rules))
  (message (if display-buffer-alist
	       "Buffer rules on"
	     "Buffer rules off")))

;; (set-frame-parameter nil 'alpha 100)
;; (add-to-list 'default-frame-alist '(alpha . 100))

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
		   ace-window
                   scroll-down-command
                   recenter-top-bottom
                   other-window))
  (advice-add command :after #'pulse-line))

(provide 'dd-ui)
