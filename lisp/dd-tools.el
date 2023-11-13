;; -*- lexical-binding: t; -*-
(use-package browse-at-remote
  :defer 1
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(use-package vterm
  :bind (("C-x p v" . vterm)))

(provide 'dd-tools)
