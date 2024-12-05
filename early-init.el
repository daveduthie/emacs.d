;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(use-package package
  :config
  (setq package-enable-at-startup nil))

(use-package emacs
  :config
  (setq gc-cons-threshold (* 50 1000 1000))
  (setq load-prefer-newer t))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package compat
  :defer t
  :load-path "lib/compat")

(use-package auto-compile
  :load-path "lib/auto-compile"
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; early-init.el ends here
