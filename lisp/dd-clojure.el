;; -*- lexical-binding: t; -*-
(use-package clojure-mode
  :mode ("\\.clj(c|s)?\\'" . clojure-mode)
  :hook (clojure-mode . eglot-ensure)
  :config
  (setq clojure-toplevel-inside-comment-form t)
  :delight)

;; Has some issues with indentation, and seems to break paredit somehow.
(use-package clojure-ts-mode
  :disabled t
  :mode ("\\.clj(c|s)?\\'" . clojure-ts-mode)
  :init
  (derived-mode-add-parents 'clojure-ts-mode '(clojure-mode))
  (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
  (add-to-list 'major-mode-remap-alist '(clojurescript-mode . clojure-ts-clojurescript-mode))
  (add-to-list 'major-mode-remap-alist '(clojurec-mode . clojure-ts-clojurec-mode))
  (defun dd/fix-clojure-ts-mode-indentation ()
    (setq-local lisp-indent-function #'clojure-indent-function))
  :hook ((clojure-ts-mode . dd/fix-clojure-ts-mode-indentation)
	 (clojure-ts-mode . eglot-ensure))
  :config
  (setq clojure-ts-indent-style 'fixed)
  (setq clojure-ts-toplevel-inside-comment-form t))

(use-package zprint-format
  :commands (zprint-format-buffer
	     zprint-format-region
	     zprint-format-on-save-mode)
  :delight zprint-format-on-save-mode)

(use-package jarchive
  :hook (clojure-mode . jarchive-mode)
  :delight)

;; def portal to the dd-dev namespace to allow dereferencing via @dd-dev/portal
(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (ns dd-dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; Flow storm

(defun flow-storm.api/local-connect ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "((requiring-resolve 'flow-storm.api/local-connect))"))


;;;; Sync deps - not quite in working order

(defun dd/sync-deps (&optional aliases)
  (interactive (list (completing-read "Aliases: " '("[ :dev ]"))))
  (let ((request
	 (format
	  "((requiring-resolve 'clojure.repl.deps/sync-deps) %s)"
	  aliases) ))
    (message request)
    (message (cider-interactive-eval request))))

;; Eval text register

(defun dd/cider-eval-register ()
  (interactive)
  (letrec ((register-text (with-temp-buffer
			    (if (fboundp 'consult-register)
				(consult-register)
			      (insert-register))
			    (buffer-string)))
	   (resp (cider-nrepl-sync-request:eval register-text)))
    (message (ansi-color-apply (nrepl-dict-get resp "out")))))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :bind (:map clojure-mode-map
	      ("C-c t p o" . #'portal.api/open)
	      ("C-c t p c" . #'portal.api/clear)
	      ("C-c t p q" . #'portal.api/close)
	      ;; TODO: bind dd/cider-eval-register
	      )
  :config
  (define-advice cider--ssh-hosts
      (:around (orig-fn &rest args) dd-disable-cider-ssh-host-retrieval nil))
  (setq markdown-indent-on-enter nil)
  ;; Revisit?
  (setq cider-xref-fn-depth 90)
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-eldoc-display-for-symbol-at-point t)
  (setq cider-eldoc-display-context-dependent-info nil)
  :delight)

(provide 'dd-clojure)
