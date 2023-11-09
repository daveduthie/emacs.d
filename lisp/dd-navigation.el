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

(provide 'dd-navigation)
