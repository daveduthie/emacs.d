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

(use-package dd-toggl
  :load-path "lisp"
  :commands (dd-toggl-start-from-region dd-toggl-stop-current-task))

(use-package jinx
  :defer 2
  :config (global-jinx-mode)
  :delight jinx-mode)

(use-package epa
  :defer t
  :config
  (setf epa-pinentry-mode 'loopback))

(use-package ox-pandoc
  :after ox
  :defer t)

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("s-<up>" . #'hs-hide-block)
	 ("s-<down>" . #'hs-show-block))
  :delight hs-minor-mode)

(use-package reveal
  :hook (hs-minor-mode . reveal-mode)
  :delight)

(provide 'dd-tools)
