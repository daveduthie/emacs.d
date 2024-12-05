;; -*- lexical-binding: t; -*-

(defun dd-llms--parse-json-resp ()
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'plist)
        (json-key-type 'symbol))
    (json-read)))

(defun dd-llms--setup-ollama (&rest more)
  (condition-case nil
      (letrec ((response (dd-llms--parse-json-resp))
	       (model-names (mapcar (lambda (x) (plist-get x 'name))
				    (plist-get response 'models))))
	(setq gptel-backend (gptel-make-ollama "Ollama"
			      :host "localhost:11434"
			      :stream t
			      :models model-names)))
    (error
     (message "Could not fetch ollama models"))))

(defun dd-llms--get-ollama-models ()
  (interactive)
  (require 'url-vars)
  (let ((url-request-method "GET"))
    (url-retrieve "http://127.0.0.1:11434/api/tags" #'dd-llms--setup-ollama)))

(defcustom default-model 'qwen2.5-coder:7b "Default model")
(defcustom default-chat-model 'llama3.1:latest "Default chat model")

(use-package gptel
  :defer 2
  :commands (gptel gptel-send)
  :bind (("C-c RET" . #'gptel-send))
  :config
  (dd-llms--get-ollama-models)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model default-model))

(use-package ellama
  :defer t
  :config
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model (symbol-name default-chat-model)
           :embedding-model (symbol-name default-model))))

(provide 'dd-llms)
