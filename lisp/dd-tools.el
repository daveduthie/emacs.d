;; -*- lexical-binding: t; -*-
(use-package browse-at-remote
  :defer 1
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(use-package vterm
  :disabled t
  :bind (("C-x p v" . vterm))
  :config
  (define-advice vterm
      (:around (orig-fun &rest args) dd/project-vterm)
    (setq default-directory (project-root (project-current)))
    (setq vterm-buffer-name (concat "*vterm " (project-name (project-current)) "*"))
    (apply orig-fun args)))

(use-package eat
  :bind (("C-x p t" . eat-project)))

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
  :delight jinx-mode
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package epa
  :defer t
  :config
  (setf epa-pinentry-mode 'loopback))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("s-<up>" . #'hs-hide-block)
	 ("s-<down>" . #'hs-show-block))
  :delight hs-minor-mode)

(use-package reveal
  :hook (hs-minor-mode . reveal-mode)
  :delight)

(use-package dirvish
  :load-path "lib/dirvish/extensions"
  :defer 3
  :custom
  (dirvish-quick-access-entries	       ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "~/src"                       "src")))
  :config
  ;; (setq insert-directory-program (executable-find "gls"))
  (dirvish-override-dired-mode)
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
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

(use-package dirvish-emerge :after dirvish)
(use-package dirvish-fd :after dirvish)
(use-package dirvish-history :after dirvish)
(use-package dirvish-narrow :after dirvish)
(use-package dirvish-ls :after dirvish)

(use-package strokes
  :defer t
  :config
  (define-key global-map [(down-mouse-3)] 'strokes-do-stroke))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package know-your-http-well
  :load-path "lib/know-your-http-well/emacs"
  :commands (http-header
	     http-method
	     http-relation
	     http-status-code
	     http-status-code))

(provide 'dd-tools)
