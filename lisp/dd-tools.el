;; -*- lexical-binding: t; -*-
(use-package browse-at-remote
  :defer 1
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(use-package vterm
  :bind (("C-x p v" . vterm))
  :config
  (define-advice vterm
      (:around (orig-fun &rest args) dd/project-vterm)
    (setq default-directory (project-root (project-current)))
    (setq vterm-buffer-name (concat "*vterm " (project-name (project-current)) "*"))
    (apply orig-fun args)))

(provide 'dd-tools)
