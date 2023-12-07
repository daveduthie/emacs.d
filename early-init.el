;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 50 1000 1000))
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(scroll-bar-mode 0)
(tool-bar-mode 0)

(use-package compat
  :defer t
  :load-path "lib/compat")

(use-package auto-compile
  :load-path "lib/auto-compile"
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
