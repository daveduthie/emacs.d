;; -*- lexical-binding: t; -*-
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

(use-package so-long
  :config
  (global-so-long-mode))

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

(use-package repeat
  :config (repeat-mode))

(use-package emacs
  :config
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (setq kill-buffer-query-functions nil))

(provide 'dd-sanity)
