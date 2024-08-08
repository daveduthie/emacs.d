;; -*- lexical-binding: t; -*-
(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/Documents/org/zk"))
  (setq denote-known-keywords nil)
  (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq denote-date-prompt-use-org-read-date t)
  :bind (("<f7>" . denote-journal-extras-new-or-existing-entry)))

(provide 'dd-denote)
