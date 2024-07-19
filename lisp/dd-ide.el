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
  (with-eval-after-load 'meow
    (meow-leader-define-key '(";" . "C-# ;"))
    (meow-leader-define-key '("'" . "C-# '"))))

(use-package flymake
  :bind (("M-n" . #'flymake-goto-next-error)
	 ("M-p" . #'flymake-goto-prev-error)))

(use-package breadcrumb
  :disabled t
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

(use-package corfu-history
  :after (savehist)
  :load-path "lib/corfu/extensions"
  :hook (corfu-mode . corfu-history-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :load-path "lib/corfu/extensions"
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay (cons 2.0 1.0)))

(provide 'dd-ide)
