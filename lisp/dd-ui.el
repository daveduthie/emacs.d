;; -*- lexical-binding: t; -*-

(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "locutus")
(setq initial-buffer-choice t)
(setq initial-scratch-message "")

(use-package modus-themes
  :init (load-theme 'modus-operandi t t)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise t)
  (set-face-attribute 'fixed-pitch nil :inherit nil :height 140 :family "Iosevka SS09")
  (set-face-attribute 'variable-pitch nil :inherit 'default :height 1.0 :family "Iosevka Etoile")

  (defun dd/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tinted t))
      ('dark (load-theme 'modus-vivendi-tinted t))))

  (add-hook 'ns-system-appearance-change-functions #'dd/apply-theme))

(use-package emacs
  :hook (prog-mode . display-line-numbers-mode))

(use-package face-remap
  :hook (org-mode . (lambda () (variable-pitch-mode t))))

;;; Buffer rules

(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq window-sides-slots '(1 1 1 1))

(defconst dd/display-buffer-rules
  (list
   ;; Show shells at the bottom of the frame
   '((or "shell\\*$" "\\*vterm" "\\*cider-repl" "\\*scheme\\*") display-buffer-in-side-window
     (side . bottom)
     (window . root)
     (window-height . 0.3))

   ;; Compilation output, grep, references, etc. go to the right
   `(,(rx (| "*compilation*" "*grep*" "*xref*"))
     display-buffer-in-side-window
     (side . right)
     (slot . 0)
     (window-parameters . ((no-delete-other-windows . t)))
     (window-width . 0.4))

   '((and (derived-mode . org-mode)
	  (or "\\*Org Agenda\\*" "[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}"))
     (display-buffer-in-tab display-buffer-in-direction)
     (tab-name . "🚀 Org"))
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

(global-set-key (kbd "C-c C-t tur") #'dd/toggle-ui-rules)

(provide 'dd-ui)
