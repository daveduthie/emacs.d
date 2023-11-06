(use-package eglot
  :bind (("C-, a" . #'eglot-code-actions)
         ("C-, r" . #'eglot-rename))
  :hook ((clojure-mode clojure-ts-mode rust-mode terraform-mode) . eglot-ensure)
  :config
  (setq eglot-sync-connect 0)
  (setq eglot-autoshutdown t))

(use-package xref
  :bind (("M-." . #'xref-find-definitions)
         ("M-," . #'xref-go-back)
         ("M-/" . #'xref-find-references)))

(provide 'dd-ide)
