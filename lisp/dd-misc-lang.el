;; -*- lexical-binding: t; -*-

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode)
  :hook (terraform-mode . eglot-ensure))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package js
  :mode ("\\.m?jsx?\\'" . js-mode)
  :hook (js-mode . eglot-ensure))

(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'"))

(use-package lua-ts-mode
  :mode ("\\.lua\\'"))

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :config
  (add-to-list 'treesit-language-source-alist
               '(graphql "https://github.com/bkegley/tree-sitter-graphql")))

(provide 'dd-misc-lang)
