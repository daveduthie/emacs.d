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

(use-package avy
  :bind (("M-j" . #'avy-goto-char-timer)))

(use-package savehist
  :init (savehist-mode))

(use-package project
  :after meow
  :config
  (meow-leader-define-key '("p". "C-x p")))

(use-package ibuffer-vc
  :defer t
  :init
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-vc-set-filter-groups-by-vc-root)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package winner
  :init (winner-mode))

(use-package mouse
  :bind (("s-n" . #'tear-off-window)))

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
  :bind (("M-g i". #'consult-imenu)
	 ("C-x p C-b" . #'consult-project-buffer)
	 ("C-x b" . #'consult-buffer)
	 ("M-s /" . #'consult-ripgrep)
	 ("C-S-s" . #'consult-line)
	 ("C-x r b" . #'consult-bookmark)))

(use-package isearch
  :defer t
  :config
  (setq isearch-wrap-pause 'no)
  (setq isearch-resume-in-command-history t))

(provide 'dd-navigation)
