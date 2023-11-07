(use-package eglot
  :bind (("C-# ;" . #'eglot-code-actions)
         ("C-# '" . #'eglot-rename))
  :hook ((clojure-mode clojure-ts-mode rust-mode terraform-mode) . eglot-ensure)
  :config
  (setq eglot-sync-connect 0)
  (setq eglot-autoshutdown t)
  (meow-leader-define-key '(";" . "C-# ;"))
  (meow-leader-define-key '("'" . "C-# '")))

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
	("<return>" . #'corfu-insert)
	("M-d" . #'corfu-show-documentation)
	("M-l" . #'corfu-show-location))
  :config
  (global-corfu-mode)
  (setq tab-always-indent 'complete))

(provide 'dd-ide)
