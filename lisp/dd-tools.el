;; -*- lexical-binding: t; -*-
(use-package browse-at-remote
  :defer 1
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(use-package vterm
  :bind (("C-x p v" . vterm))
  :config
  (define-advice vterm
      (:around (orig-fun &rest args) dd/project-vterm)
    (setq default-directory (project-root (project-current)))
    (setq vterm-buffer-name (concat "*vterm " (project-name (project-current)) "*"))
    (apply orig-fun args)))

(use-package eshell-mode
  :defer t
  :config
  (add-to-list 'eshell-visual-commands "gh")
  (add-to-list 'eshell-visual-subcommands (list "bb" "shell")))

(use-package dd-toggl
  :load-path "lisp"
  :commands (dd-toggl-start-from-region dd-toggl-stop-current-task))

(use-package jinx
  :defer 2
  :config (global-jinx-mode)
  :delight jinx-mode)

(use-package epa
  :defer t
  :config
  (setf epa-pinentry-mode 'loopback))

(use-package ox-pandoc
  :after ox
  :defer t)

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("s-<up>" . #'hs-hide-block)
	 ("s-<down>" . #'hs-show-block))
  :delight hs-minor-mode)

(use-package reveal
  :hook (hs-minor-mode . reveal-mode)
  :delight)

(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model "mistral:latest")
  (setq gptel-backend (gptel-make-ollama "Ollama"
			:host "localhost:11434"
			:stream t
			:models '("mistral:latest"))))

(use-package dirvish
  :load-path "lib/dirvish/extensions"
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "~/src"                       "src")))
  :config
  (require 'dirvish-fd)
  (require 'dirvish-history)
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")

  ;; https://github.com/alexluigit/dirvish/pull/257
  (defun dirvish-dired-noselect-a (fn dir-or-list &optional flags)
    "Return buffer for DIR with FLAGS, FN is `dired-noselect'."
    (let* ((dir (if (consp dir-or-list) (car dir-or-list) dir-or-list))
	   (key (file-name-as-directory (expand-file-name dir)))
	   (this dirvish--this)
	   (dv (if (and this (eq this-command 'dired-other-frame)) (dirvish-new)
		 (or this (car (dirvish--find-reusable)) (dirvish-new))))
	   (bname buffer-file-name)
	   (remote (file-remote-p dir))
	   (flags (or flags (dv-ls-switches dv)))
	   (buffer (alist-get key (dv-roots dv) nil nil #'equal))
	   (new-buffer-p (not buffer)))
      (if this (set-window-dedicated-p nil nil) (setcar (dv-layout dv) nil))
      (when new-buffer-p
	(if (not remote)
	    (let ((dired-buffers nil))	; disable reuse from dired
	      (setq buffer (apply fn (list dir-or-list flags))))
	  (require 'dirvish-extras)
	  (setq buffer (dirvish-noselect-tramp fn dir-or-list flags remote)))
	(with-current-buffer buffer (dirvish-init-dired-buffer))
	(push (cons key buffer) (dv-roots dv))
	(push (cons key buffer) dired-buffers))
      (with-current-buffer buffer
	(cond (new-buffer-p nil)
	      ((and (not remote) (not (equal flags dired-actual-switches)))
	       (dired-sort-other flags))
	      ((eq dired-auto-revert-buffer t) (revert-buffer))
	      ((functionp dired-auto-revert-buffer)
	       (when (funcall dired-auto-revert-buffer dir) (revert-buffer))))
	(dirvish-prop :dv (dv-name dv))
	(dirvish-prop :gui (display-graphic-p))
	(dirvish-prop :remote remote)
	(dirvish-prop :root key)
	(when bname (dired-goto-file bname))
	(setf (dv-index dv) (cons key buffer))
	(run-hook-with-args 'dirvish-find-entry-hook key buffer)
	buffer)))
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  (setq dirvish-fd-default-dir "~")

  :bind	     ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map	   ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dired-up-directory)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)	  ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)	  ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-j" . dirvish-fd-jump)))

(provide 'dd-tools)
