;;; ghl.el --- Open current file on http://github.com

;; You need to define *ghl-browser-cmd* e.g
;; (setq *ghl-browser-cmd* "/usr/bin/open -a '/Applications/Google Chrome.app' '%s'")

(require 'dev.el)

(defvar *ghl-github-url* "https://github.com/")

(defun ghl ()
  (interactive)
  (ghl-copy))

(defun ghl-open ()
  (interactive)
  (shell-command-to-string (format *ghl-browser-cmd* (ghl-get-url))))

(defun ghl-copy ()
  (interactive)
  (let ((link (ghl-get-url)))
    (kill-new link)
    (message (format "%s copied into clipboard" link))))


(defun ghl-get-url ()
  (let ((repo (read-from-minibuffer "Repo: " (dev-repo-name)))
        (branch (read-from-minibuffer "Branch: " (dev-branch-name))))

    (ghl-set-user!)
    (beginning-of-line)

    (concat *ghl-github-url*
            *ghl-github-user*
            "/"
            repo
            "/blob/"
            branch
            "/"
            (ghl-get-filepath repo)
            "#L" (int-to-string (1+ (count-lines 1 (point)))))))

(defun ghl-get-filepath (repo)
  (let* ((full-path (split-string (buffer-file-name) "/"))
         (path (subseq full-path
                       (1+ (cl-position repo full-path :test 'equal))
                       (length full-path))))
    (mapconcat 'identity path "/")))


(defun ghl-get-user ()
  (if (boundp '*ghl-github-user*)
      *ghl-github-user*))

(defun ghl-set-user! ()
  (let ((user (read-from-minibuffer "Username: " (ghl-get-user))))
    (if (equal user "")
        (error "error: no github username provided"))

    (setq *ghl-github-user* user)
    user))

(provide 'ghl)
