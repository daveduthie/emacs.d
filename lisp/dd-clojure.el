;; -*- lexical-binding: t; -*-
(use-package clojure-mode
  :mode ("\\.clj(c|s)?\\'" . clojure-mode)
  :hook (clojure-mode . eglot-ensure)
  :config (setq clojure-toplevel-inside-comment-form t)
  :delight)

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

;;;###autoload (autoload 'standard-clojure-format-buffer "standard-clojure-format" nil t)
;;;###autoload (autoload 'standard-clojure-format-region "standard-clojure-format" nil t)
;;;###autoload (autoload 'standard-clojure-format-on-save-mode "standard-clojure-format" nil t)
(reformatter-define standard-clojure-format
  :program "standard-clj"
  :args (append '("fix") (list input-file))
  :stdin nil
  :stdout nil
  :lighter "stdclj"
  :group 'standard-clojure-format)

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

(defun dd/letdefs ()
  "Transform Clojure bindings by wrapping each value with a def. It's tied to paredit"
  (interactive)
  (down-list) ; Move into the opening '['
  (condition-case nil
      (while t
	(let ((var-name (thing-at-point 'symbol)))
	  (forward-sexp)
	  (paredit-wrap-round)
	  (insert "def ")
	  (insert var-name)
	  (up-list)
	  (forward-sexp)
	  (backward-sexp)))  
    (error nil)))

(defun dd/letdefs-undo ()
  "Transform Clojure bindings by unwrapping each value. It's tied to paredit"
  (interactive)
  (down-list) ; Move into the opening '['
  (condition-case nil
      (while t
	(forward-sexp)
	(down-list)
	(forward-sexp 2)
	(paredit-raise-sexp)
	(forward-sexp)
	(backward-sexp))  
    (error nil)))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :bind (:map clojure-mode-map
	      ("C-c t p o" . #'portal.api/open)
	      ("C-c t p c" . #'portal.api/clear)
	      ("C-c t p q" . #'portal.api/close)
	      ;; TODO: bind dd/cider-eval-register
	      )
  :config
  (unbind-key "C-c RET" cider-mode-map)
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

(use-package kaocha-runner
  :after (cider-mode)
  :bind (:map clojure-mode-map
	      ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(defun dd/clj-project-scratch ()
  (interactive)
  (find-file (expand-file-name (format-time-string "dev/dd/scratch_%Y_%m_%d.cljc")
			       (vc-root-dir))))

(provide 'dd-clojure)
