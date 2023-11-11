;; -*- lexical-binding: t; -*-

(use-package tab-bar
  :bind (("M-[" . #'tab-bar-switch-to-prev-tab)
	 ("M-]" . #'tab-bar-switch-to-next-tab))
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

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

(use-package savehist
  :init (savehist-mode))

(use-package project
  :after meow
  :config
  (meow-leader-define-key '("p". "C-x p")))

(use-package winner
  :init (winner-mode))

(use-package window
  :bind (("M-o" . #'other-window)))

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-alh")
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(provide 'dd-navigation)
