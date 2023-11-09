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

(provide 'dd-ui)
