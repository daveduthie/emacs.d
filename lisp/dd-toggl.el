;; -*- lexical-binding: t; -*-

;; (fset 'epg-wait-for-status 'ignore) ; probably a bad idea!
(setq epa-pinentry-mode 'loopback)
(setq dd-toggl--toggl-workspace-id 7868546)

(defun dd-toggl--password ()
  (auth-info-password (car (auth-source-search :host "api.track.toggl.com" :max 1))))

(defun dd-toggl--get-auth ()
  (format "Basic %s"
	  (base64-encode-string (concat (dd-toggl--password) ":api_token"))))

(setq dd-toggl--toggl-base-url "https://api.track.toggl.com/api/v9")

(defun dd-toggl--now-str ()
  (format-time-string "%Y-%m-%dT%TZ" nil t))

(cl-defun dd-toggl--request (&key method path payload callback)
  (let ((url-request-method method)
	(url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(dd-toggl--get-auth))))
	(url-request-data (when payload (json-encode payload))))
    (url-retrieve
     (concat dd-toggl--toggl-base-url path)
     (lambda (&rest _args)
       (funcall callback (dd-toggl--parse-json-resp))))))

(defun dd-toggl--parse-json-resp ()
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'plist)
	;; (json-key-type 'symbol)
	;; (json-array-type 'vector)
	)
    (json-read)))

(defun dd-toggl--start-task (description)
  (dd-toggl--request
   :method "POST"
   :path (format "/workspaces/%s/time_entries" dd-toggl--toggl-workspace-id)
   :payload `(("start" . ,(dd-toggl--now-str))
	      ("description" . ,description)
	      ("duration" . -1)
	      ("workspace_id" . ,dd-toggl--toggl-workspace-id)
	      ("created_with" . "emacs(dd-toggl)"))
   :callback (lambda (resp) (message "started: %s" (plist-get resp :description)))))

(defun dd-toggl-get-current-task (&optional cb)
  (interactive)
  (dd-toggl--request
   :method "GET"
   :path "/me/time_entries/current"
   :callback (lambda (resp)
	       (message "Running task: %s" (plist-get resp :description))
	       (when cb (cb resp)))))

(defun dd-toggl-stop-current-task ()
  (interactive)
  (dd-toggl-get-current-task
   (lambda (task)
     (dd-toggl--request
      :method "PATCH"
      :path (format "/workspaces/%s/time_entries/%s/stop"
	     dd-toggl--toggl-workspace-id
	     (plist-get task :id))
      :callback (lambda (response)
		  (message "stopped: %s" (plist-get response :description)))))))

(defun dd-toggl-start-from-region ()
  "Starts a task with description taken from active region"
  (interactive)
  (let ((description (buffer-substring (mark) (point))))
    (dd-toggl--start-task description)))

(provide 'dd-toggl)

