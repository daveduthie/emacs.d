;; -*- lexical-binding: t; -*-
(use-package term/ns-win
  :if (equal system-type 'darwin)
  :config
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer 2
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package ns-auto-titlebar
  :if (memq window-system '(mac ns))
  :config (ns-auto-titlebar-mode))

(provide 'dd-mac)
