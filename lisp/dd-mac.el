;; -*- lexical-binding: t; -*-
(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(use-package exec-path-from-shell
  :defer 1
  :config
  (when (memq window-system '(mac ns))
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

(provide 'dd-mac)
