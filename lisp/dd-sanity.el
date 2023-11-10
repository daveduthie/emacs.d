;; -*- lexical-binding: t; -*-
(setq ring-function 'ignore)

(use-package comp
  :config
  (setq native-comp-async-report-warnings-errors nil))

(use-package files
  :config
  (setq confirm-kill-processes nil)
  (setq create-lockfiles nil)
  (setq make-backup-files nil))

(use-package outline
  :defer t
  :delight outline-minor-mode)

(use-package reveal
  :defer t
  :delight)

(provide 'dd-sanity)