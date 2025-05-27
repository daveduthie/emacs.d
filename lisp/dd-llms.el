;; -*- lexical-binding: t; -*-

(use-package gptel
  :defer t
  :bind (("C-c RET" . gptel-send))
  :init
  (setq gptel-model 'gpt-4.1)
  (setq gptel-default-mode 'org-mode)
  
  :config
  (defun dd/read-auth-source-secret (host user)
    (let ((result (auth-source-search :host host :user user)))
      (funcall (plist-get (car result) :secret))))

  (setq gptel-ollama-backend
	(gptel-make-ollama "Ollama"
	  :host "localhost:11434"
	  :stream t
	  :models '((granite3.2:8b
		     :description "Granite-3.2 is a family of long-context AI models from IBM"
		     :capabilities (tool-use))
		    (cogito:8b
		     :description "Cogito v1 Preview is a family of hybrid reasoning models"
		     :capabilities (tool-use)))))

  (setq gptel-gemini-backend
	(gptel-make-gemini "Gemini"
	  :key (lambda () (dd/read-auth-source-secret "aistudio.google.com" "dave.duthie@lifecheq.co.za"))
	  :stream t))
  
  (setq gptel-anthropic-backend
	(gptel-make-anthropic "Claude"
	  :stream t
	  :key (lambda () (dd/read-auth-source-secret "claude.ai" "dave.duthie@lifecheq.co.za"))))

  (setq gptel-copilot-backend (gptel-make-gh-copilot "Copilot"))
  
  (setq gptel-backend gptel-copilot-backend)
  
  (gptel-make-tool
   :name "read_buffer"			; javascript-style snake_case name
   :function (lambda (buffer)		; the function that will run
               (unless (buffer-live-p (get-buffer buffer))
		 (error "error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
		 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
		       :type string	; :type value must be a symbol
		       :description "the name of the buffer whose contents are to be retrieved"))
   :category "emacs")                     ; An arbitrary label for grouping
  
  (gptel-make-tool
   :name "create_file"                    ; javascript-style  snake_case name
   :function (lambda (path filename content)   ; the function that runs
               (let ((full-path (expand-file-name filename path)))
		 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
		 (format "Created file %s in %s" filename path)))
   :description "Create a new file with the specified content"
   :args (list '(:name "path"             ; a list of argument specifications
		       :type string
		       :description "The directory where to create the file")
               '(:name "filename"
		       :type string
		       :description "The name of the file to create")
               '(:name "content"
		       :type string
		       :description "The content to write to the file"))
   :category "filesystem")                ; An arbitrary label for grouping

  (gptel-make-tool
   :name "create_directory"
   :function (lambda (path)
               (let ((full-path (expand-file-name path)))
		 (if (file-exists-p full-path)
                     (format "Directory %s already exists" path)
                   (make-directory full-path t)
                   (format "Created directory %s" path))))
   :description "Create a new directory (including parent directories)"
   :args (list '(:name "path"
                       :type string
                       :description "The directory path to create"))
   :category "filesystem")

  (gptel-make-tool
   :name "list_directory"
   :function (lambda (path &optional full)
               (let ((files (directory-files (expand-file-name path) full)))
		 (string-join files "\n")))
   :description "List the contents of a directory"
   :args (list '(:name "path"
                       :type string
                       :description "The directory path to list")
               '(:name "full"
                       :type boolean
                       :description "Whether to show full paths (true) or just filenames (false)"))
   :category "filesystem")

  ;; TODO: this doesn't work very well at all
  (gptel-make-tool
   :name "web_search"
   :function (lambda (query &optional num-results)
	       (require 'url)
	       (require 'json)
	       (let* ((num (or num-results 5))
                      (encoded-query (url-encode-url query))
                      (url (format "https://api.duckduckgo.com/?q=%s&format=json" encoded-query))
                      (result (with-current-buffer (url-retrieve-synchronously url)
				(goto-char (point-min))
				(re-search-forward "^$")
				(json-read))))
		 (if (not result)
                     "No results found"
                   (let ((abstract (cdr (assoc 'AbstractText result)))
			 (related (cdr (assoc 'RelatedTopics result))))
                     (concat 
                      (if (string-empty-p abstract) "" (concat abstract "\n\n"))
                      (mapconcat 
		       (lambda (item)
			 (cdr (assoc 'Text item)))
		       (seq-take related num)
		       "\n"))))))
   :description "Search the web using DuckDuckGo"
   :args (list '(:name "query"
                       :type string
                       :description "The search query")
               '(:name "num_results"
                       :type number
                       :description "The number of results to return (default 5)"))
   :category "web")

  (gptel-make-tool
   :name "apropos_function"
   :function (lambda (pattern)
               (let ((matches (apropos-internal pattern #'fboundp)))
		 (if (null matches)
                     (format "No functions matching '%s' found" pattern)
                   (mapconcat
                    (lambda (sym)
                      (let ((doc (documentation sym)))
			(format "%s: %s" 
				sym
				(if doc
                                    (car (split-string doc "\n"))
                                  "No documentation"))))
                    matches
                    "\n"))))
   :description "Look up Emacs functions matching a pattern"
   :args (list '(:name "pattern"
                       :type string
                       :description "Pattern to search for (regexp)"))
   :category "emacs")

  (gptel-make-tool
   :name "apropos_variable"
   :function (lambda (pattern)
               (let ((matches (apropos-internal pattern #'boundp)))
		 (if (null matches)
                     (format "No variables matching '%s' found" pattern)
                   (mapconcat
                    (lambda (sym)
                      (let ((doc (documentation-property sym 'variable-documentation)))
			(format "%s: %s = %s" 
				sym
				(if doc
                                    (car (split-string doc "\n"))
                                  "No documentation")
				(prin1-to-string (symbol-value sym)))))
                    matches
                    "\n"))))
   :description "Look up Emacs variables matching a pattern"
   :args (list '(:name "pattern"
                       :type string
                       :description "Pattern to search for (regexp)"))
   :category "emacs")

  (gptel-make-tool
   :name "execute_elisp"
   :function (lambda (code)
               (condition-case err
                   (format "%S" (eval (read code)))
		 (error (format "Error: %s" (error-message-string err)))))
   :description "Execute Emacs Lisp code and return the result"
   :args (list '(:name "code"
                       :type string
                       :description "Elisp code to evaluate"))
   :category "emacs"))

;; Add some more  gptel / llm tools for programming inside emacs. E.g. include tools for creating and listing directories, thinking, and searching the internet using duckduckgo.

(provide 'dd-llms)
