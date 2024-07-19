(defun dd-llms--parse-json-resp ()
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'plist)
	(json-key-type 'symbol)
	;; (json-array-type 'vector)
	)
    (json-read)))

(defun dd/set-ollama-models ()
  (interactive)
  (let ((url-request-method "GET"))
    (url-retrieve "http://127.0.0.1:11434/api/tags"
		  (lambda (&rest more)
		    (letrec ((response (dd-llms--parse-json-resp))
			     (model-names (mapcar (lambda (x) (plist-get x 'name))
						  (plist-get response 'models))))
		      (setq gptel-backend (gptel-make-ollama "Ollama"
					    :host "localhost:11434"
					    :stream t
					    :models model-names)))))))

(use-package gptel
  :commands (gptel gptel-send)
  :config
  (dd/set-ollama-models)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model "llama3:latest"))

(use-package ellama
  :defer t)

(provide 'dd-llms)
