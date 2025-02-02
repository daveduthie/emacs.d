;; -*- lexical-binding: t; -*-

(use-package tab-bar
  :bind (("M-[" . #'tab-bar-switch-to-prev-tab)
	 ("M-]" . #'tab-bar-switch-to-next-tab)
	 ("M-t" . #'tab-new)
	 ("M-W" . #'tab-close))
  :config
  (setq tab-bar-tab-name-function
        (lambda ()
          (if-let ((project (car (last (project-current)))))
            (file-name-nondirectory (directory-file-name (file-name-directory project)))
            (buffer-name)))))

(use-package vertico
  :init (vertico-mode))

(use-package vertico-mouse
  :load-path "lib/vertico/extensions"
  :after vertico
  :config (vertico-mouse-mode))

(use-package vertico-grid
  :load-path "lib/vertico/extensions"
  :after vertico)

(use-package vertico-buffer
  :load-path "lib/vertico/extensions"
  :after vertico)

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :load-path "lib/vertico/extensions"
  :after vertico
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-categories
	'(("file" grid)
	  (jinx grid (vertico-grid-annotate . 20)))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

(use-package savehist
  :init (savehist-mode))

(use-package project
  :after meow
  :config
  (meow-leader-define-key '("p". "C-x p")))

(use-package ibuffer-project
  :defer t
  :init
  (defun dd/enhance-ibuffer-with-ibuffer-project ()
    "Set up integration for `ibuffer' with `ibuffer-project'."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  (add-hook 'ibuffer-hook #'dd/enhance-ibuffer-with-ibuffer-project))

(use-package winner
  :init (winner-mode))

;; (use-package window
;;   :bind (("M-o" . #'other-window)))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-when-more-than 2)
  :bind (("M-o" . #'ace-window)))

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-free-space nil)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package consult
  :bind (("C-c i". #'consult-imenu)
	 ("C-x p b" . #'consult-project-buffer)
	 ("C-x b" . #'consult-buffer)
	 ("C-x p x" . #'consult-ripgrep)
	 ("C-S-s" . #'consult-line)
	 ("C-x r b" . #'consult-bookmark)))

(use-package isearch
  :defer t
  :config
  (setq isearch-wrap-pause 'no)
  (setq isearch-resume-in-command-history t))

(provide 'dd-navigation)
