;; -*- lexical-binding: t; -*-
(require 'org)
(require 'time-date)
(require 'transient)
(require 'cl-lib)

;;; Org daily

(defcustom springbok-daily-path "~/Documents/org/daily"
  "Path where springbok-daily files live"
  :type 'string)

(defun springbok-daily--format-date (date)
  (format-time-string "%Y-%m-%d" date))

(defun springbok-daily--filename (date)
  (expand-file-name (concat date ".org") springbok-daily-path))

(cl-defun springbok-daily--initial-content (&key date)
  (format-time-string "#+TITLE: %Y-%m-%d %A\n" date))

;; TODO: rename
(defun springbok-daily (&optional date)
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (let ((date (or date (current-time))))
    (find-file (springbok-daily--filename (springbok-daily--format-date date)))
    (when (= 0 (buffer-size))
      (insert (springbok-daily--initial-content :date date)))))

(defun springbok-today ()
  (interactive)
  (springbok-daily))

;;; Previous / Next transient

(defun springbok-daily--buffer-date ()
  (file-name-base (buffer-name)))

(defun springbok-daily--parse-date-string (date-string)
  (let ((time (iso8601-parse date-string)))
    (setf (decoded-time-hour time) 0)
    (setf (decoded-time-minute time) 0)
    (setf (decoded-time-second time) 0)
    time))

(defun springbok-daily--find-neighbouring-entry (date next)
  (let ((decoded-time (springbok-daily--parse-date-string date))
	(attempts 10))
    (decoded-time--alter-day decoded-time next)
    (while (natnump attempts)
      (let ((file-name (springbok-daily--filename
			(springbok-daily--format-date
			 (encode-time decoded-time)))))
	(message "file-name: %s" file-name)
        (if (file-exists-p file-name)
            (progn (setq attempts nil)
		   (find-file file-name))
	  (progn
	    (decoded-time--alter-day decoded-time next)
            (setq attempts (1- attempts))))))
    (when attempts (message "Gave up!"))))

(transient-define-suffix springbok-daily--prev ()
  "Previous"
  :transient t
  (interactive)
  (springbok-daily--find-neighbouring-entry (springbok-daily--buffer-date) nil))

(transient-define-suffix springbok-daily--next ()
  "Next"
  :transient t
  (interactive)
  (springbok-daily--find-neighbouring-entry (springbok-daily--buffer-date) t))

(transient-define-prefix springbok-daily-prev-next ()
  "Prefix that waves at the user persistently."
  [("<" "Previous" springbok-daily--prev)
   (">" "Next" springbok-daily--next)
   ("q" "Quit" (lambda () (interactive)))])

(provide 'springbok-daily)
