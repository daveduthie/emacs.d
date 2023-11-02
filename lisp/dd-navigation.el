(use-package icomplete
  :init
  (fido-mode t)
  (fido-vertical-mode t))

(use-package project
  :after meow
  :config
  (meow-leader-define-key '("<SPC>". #'project-find-file)))

(use-package winner
  :init (winner-mode))

(provide 'dd-navigation)
