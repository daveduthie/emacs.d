;; -*- lexical-binding: t; -*-
(use-package clojure-mode
  :mode ("\\.clj(c|s)?\\'" . clojure-mode)
  :hook (clojure-mode . eglot-ensure)
  :config
  (setq clojure-toplevel-inside-comment-form t)
  :delight)

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
  (let ((register-text (with-temp-buffer
			 (if (fboundp 'consult-register)
			     (consult-register)
			   (insert-register))
			 (buffer-string))))
    (message (cider-nrepl-sync-request:eval register-text))))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :bind (:map clojure-mode-map
	      ("C-c t p o" . #'portal.api/open)
	      ("C-c t p c" . #'portal.api/clear)
	      ("C-c t p q" . #'portal.api/close)
	      ;; TODO: bind dd/cider-eval-register
	      )
  :config
  (setq markdown-indent-on-enter nil)
  ;; Revisit?
  (setq cider-xref-fn-depth 90)
  :delight)

(provide 'dd-clojure)
