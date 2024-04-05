;; -*- lexical-binding: t; -*-
(use-package term/ns-win
  :if (equal system-type 'darwin)
  :config
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer 1
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(provide 'dd-mac)
