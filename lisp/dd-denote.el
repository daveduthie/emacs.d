;; -*- lexical-binding: t; -*-

(use-package denote
  :bind (("C-c d" . denote-journal-extras-new-or-existing-entry))
  :config
  (setq denote-directory (expand-file-name "~/Documents/org/zk"))
  (setq denote-known-keywords nil)
  (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq denote-date-prompt-use-org-read-date t)
  ;; From denote manual
  (defun dd/denote-org-extract-subtree (&optional silo)
    "Create new Denote note using current Org subtree.
Make the new note use the Org file type, regardless of the value of
`denote-file-type'.

With an optional SILO argument as a prefix (\\[universal-argument]), ask
user to select a SILO from `my-denote-silo-directories'.

Use the subtree title as the note's title.  If available, use the tags
of the heading are used as note keywords.

Delete the original subtree."
    (interactive
     (list (when current-prefix-arg
             (completing-read "Select a silo: " my-denote-silo-directories nil t))))
    (if-let ((text (org-get-entry))
             (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
	(let ((element (org-element-at-point))
              (tags (org-get-tags))
              (denote-user-enforced-denote-directory silo))
          (delete-region (org-entry-beginning-position)
			 (save-excursion (org-end-of-subtree t) (point)))
          (denote heading
                  tags
                  'org
                  nil
                  (or
                   ;; Check PROPERTIES drawer for :created: or :date:
                   (org-element-property :CREATED element)
                   (org-element-property :DATE element)
                   ;; Check the subtree for CLOSED
                   (org-element-property :raw-value
					 (org-element-property :closed element))))
          (insert text))
      (user-error "No subtree to extract; aborting"))))

(provide 'dd-denote)
