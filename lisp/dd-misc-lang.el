;; -*- lexical-binding: t; -*-
(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode)
  :hook (terraform-mode . eglot-ensure))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package js
  :mode ("\\.m?jsx?\\'" . js-mode))

(provide 'dd-misc-lang)
