;; -*- lexical-binding: t; -*-
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

(use-package emacs
  :hook (prog-mode . display-line-numbers-mode))

(custom-set-faces
 '(default ((t (:inherit nil :height 140 :family "Iosevka SS08")))))

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;;; Buffer rules

(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq window-sides-slots '(1 1 1 1))

(setq display-buffer-alist
      (list
       ;; Show shells at the bottom of the frame
       '("shell\\*$" display-buffer-in-side-window
	 (side . bottom)
	 ;; (window . root)
	 (window-height . 0.3))

       ;; Compilation output, grep, references, etc. go to the right
       `(,(rx (| "*compilation*" "*grep*" "*xref*"))
	 display-buffer-in-side-window
	 (side . right)
	 (slot . 0)
	 (window-parameters . ((no-delete-other-windows . t)))
	 (window-width . 0.4))

       ;; Show directory browser in a left side bar
       '((derived-mode . dired-mode)
	 display-buffer-in-side-window
	 (side . left)
	 (slot . 0)
	 (window-parameters . ((no-delete-other-windows . t)))
	 (window-width . 0.3))

       ;; test files go to the right
       `("[-_]test"
	 display-buffer-in-direction
	 (direction . right)
	 (window-width . 0.5))

       '((or (derived-mode . org-mode)
	     "\\*Org Agenda\\*")
	 (display-buffer-in-tab display-buffer-in-direction)
	 (tab-name . "🚀 Org"))
       ;;
       ))

(provide 'dd-ui)
