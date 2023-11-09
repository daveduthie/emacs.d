(use-package clojure-mode
  :mode ("\\.clj(c|s)?\\'" . clojure-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq markdown-indent-on-enter nil)
  ;; Revisit?
  (setq cider-xref-fn-depth 90))

(use-package zprint-format
  :commands (zprint-format-buffer
	     zprint-format-region
	     zprint-format-on-save-mode))

(provide 'dd-clojure)
