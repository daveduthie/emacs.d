;;  -*- lexical-binding: t; -*-
;; SPRINGBOK: Structured Planning and Rapid Information Note-Taking in Emacs by Observing Kernels
;;

(require 'springbok-daily)

;;; Quick notes

(defcustom springbok-quick-note-path "~/Documents/org/zk"
  "Where the quick notes live"
  :type 'string)

(defun springbok--asciify-string (string)
  "Convert STRING to ASCII string.
For example:
“passé” becomes “passe”"
  ;; Code originally by Teemu Likonen
  (with-temp-buffer
    (insert string)
    (call-process-region (point-min) (point-max) "iconv" t t nil "-f" "UTF-8" "-t" "ASCII")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun springbok--slug (str)
  (let* ((downcased (downcase (springbok--asciify-string str)))
	 (trimmed (replace-regexp-in-string "[- ]+" "-" downcased)))
    (replace-regexp-in-string "[^a-z0-9-]" "" trimmed)))

;;;###autoload
(defun springbok-make-quick-note ()
  (interactive)
  (let* ((title (read-string "Title: "))
	 (file-name (expand-file-name
		     (concat
		      (format-time-string "%Y-%m-%d_%H-%M_")
		      (springbok--slug title)
		      ".org")
		     springbok-quick-note-path)))
    (find-file file-name)
    (insert ":PROPERTIES:\n:ID: ")
    (call-process "uuidgen" nil t)
    (insert ":END:\n")
    (insert (concat "#+TITLE: " title))))

(provide 'springbok)
