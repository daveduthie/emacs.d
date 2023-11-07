;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(defvar config-libs
  '(dd-early-birds
    dd-long-tail
    dd-ui
    dd-editing
    dd-navigation
    dd-ide
    dd-org))

(dolist (lib config-libs)
  (require lib))

;;; Dave packages

(setq ring-function 'ignore)

(use-package comp
  :config
  (setq native-comp-async-report-warnings-errors nil))

(use-package files
  :config
  (setq confirm-kill-processes nil)
  (setq create-lockfiles nil)
  (setq make-backup-files nil))

(use-package clojure-mode
  :mode ("\\.clj(c|s)?\\'" . clojure-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq markdown-indent-on-enter nil)
  ;; Revisit?
  (setq cider-xref-fn-depth 90))

(use-package zprint-format
  :commands (zprint-format-buffer
	     zprint-format-region
	     zprint-format-on-save-mode))

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode))

(use-package eldoc
  :delight)

(use-package autorevert
  :delight arev
  :init (global-auto-revert-mode 1))

(use-package browse-at-remote
  :defer 1
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(custom-set-faces
 '(default ((t (:inherit nil :height 140 :family "Iosevka SS08")))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package tab-bar
  :bind (("M-[" . #'tab-bar-switch-to-prev-tab)
	 ("M-]" . #'tab-bar-switch-to-next-tab))
  :config
  (setq tab-bar-tab-name-function
        (lambda ()
          (if-let ((project (car (last (project-current)))))
            (file-name-nondirectory (directory-file-name (file-name-directory project)))
            (buffer-name)))))

(use-package outline
  :defer t
  :delight outline-minor-mode)

(use-package reveal
  :defer t
  :delight)

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
