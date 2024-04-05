;; -*- lexical-binding: t; -*-

(use-package eglot
  :defer 1
  :bind (("C-# ;" . #'eglot-code-actions)
         ("C-# '" . #'eglot-rename))
  :config
  (setq eglot-sync-connect 0)
  (setq eglot-autoshutdown t)
  (setq eglot-connect-timeout 120)
  (setq eglot-extend-to-xref t)
  (meow-leader-define-key '(";" . "C-# ;"))
  (meow-leader-define-key '("'" . "C-# '")))

(use-package flymake
  :bind (("M-n" . #'flymake-goto-next-error)
	 ("M-p" . #'flymake-goto-prev-error)))

(use-package breadcrumb
  :defer 2
  :config (breadcrumb-mode))

(use-package xref
  :bind (("M-." . #'xref-find-definitions)
         ("M-," . #'xref-go-back)
         ("M-/" . #'xref-find-references)))

(use-package corfu
  :bind
  (:map corfu-map
	("C-n" . #'corfu-next)
	("C-p" . #'corfu-previous)
	("<escape>" . #'corfu-quit)
	("<return>" . #'corfu-insert))
  :config
  (setq corfu-auto t)
  (global-corfu-mode)
  (setq tab-always-indent 'complete))

(provide 'dd-ide)
