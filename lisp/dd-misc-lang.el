;; -*- lexical-binding: t; -*-
(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package js
  :mode ("\\.m?js\\'" . js-mode))

(provide 'dd-misc-lang)
