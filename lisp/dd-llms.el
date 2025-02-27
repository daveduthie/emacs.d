;; -*- lexical-binding: t; -*-

(use-package gptel
  :defer t
  :init
  (setq gptel-model 'gemini-pro)
  :config
  (setq gptel-default-mode 'org-mode)
  (defun dd/read-auth-source-secret (host user)
    (let ((result (auth-source-search :host host :user user)))
      (funcall (plist-get (car result) :secret))))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (dd/read-auth-source-secret "claude.ai" "dave.duthie@lifecheq.co.za")))
  (setq gptel-backend
	(gptel-make-gemini "Gemini"
	  :key (lambda () (dd/read-auth-source-secret "aistudio.google.com" "dave.duthie@lifecheq.co.za"))
	  :stream t)))

(provide 'dd-llms)
