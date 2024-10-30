;; -*- lexical-binding: t; -*-

(use-package haskell-ts-mode
  :mode ("\\.hs\\'" . haskell-ts-mode)
  :config
  (haskell-ts-setup-eglot)
  :hook (haskell-ts-mode . eglot-ensure))

(provide 'dd-haskell)
