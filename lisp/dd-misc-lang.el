;; -*- lexical-binding: t; -*-

(use-package nix-ts-mode
  :init (defalias 'nix-mode 'nix-ts-mode) ; hack for org src blocks
  :mode ("\\.nix\\'" . nix-ts-mode))

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode)
  :config
  (defun dd/terraform-search-resource ()
    ;; WIP
    (interactive)
    (browse-url
     (format "https://duckduckgo.com/?q=%s"
	     (url-hexify-string (concat "terraform " (thing-at-point 'symbol))))))
  (defun dd/disable-electric-indent-local-mode ()
    (electric-indent-local-mode 0))

  :hook
  (terraform-mode . eglot-ensure)
  (terraform-mode . electric-pair-mode)
  (terraform-mode . dd/disable-electric-indent-local-mode)
  (terraform-mode . terraform-format-on-save-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package js
  :mode ("\\.m?[jt]sx?\\'" . js-mode)
  :hook (js-mode . eglot-ensure))

(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'")
  :hook ((yaml-ts-mode . display-line-numbers-mode)
	 (yaml-ts-mode . eglot-ensure)))

(use-package highligh-indent-guides
  :hook ((yaml-ts-mode sh-mode) . highlight-indent-guides-mode))

(use-package lua-ts-mode
  :mode ("\\.lua\\'"))

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :config
  (add-to-list 'treesit-language-source-alist
	       '(graphql "https://github.com/bkegley/tree-sitter-graphql")))

(defun dd/add-rust-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?< . ?>))))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(use-package rust-ts-mode
  :mode ("\\.rs\\'")
  :hook
  (rust-ts-mode . eglot-ensure)
  (rust-ts-mode . dd/add-rust-pairs)
  )

(use-package sgml-mode
  :defer t
  :config
  (define-key html-mode-map "\M-o" nil))

(use-package sqlformat
  :commands (sqlformat-buffer
	     sqlformat-region
	     sqlformat-on-save-mode)
  :config (setq sqlformat-command 'pgformatter))

(use-package go-mode
  :defer t
  :hook
  (go-mode . eglot-ensure))

(provide 'dd-misc-lang)
