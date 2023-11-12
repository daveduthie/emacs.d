;; -*- lexical-binding: t; -*-

(setq dd/ui-toggles
      (define-keymap
	"r" (cons "buffer display rules" #'dd/toggle-ui-rules)))

(setq dd/toggles
      (define-keymap
	"u" (cons "UI rules" dd/ui-toggles)))

(global-set-key (kbd "C-c C-t") (cons "Toggles" dd/toggles))


(provide 'dd-toggles)
