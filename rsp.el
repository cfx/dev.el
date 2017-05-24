(defvar *rsp-tmux-cmd* "tmux send-keys -t 1 'clear && docker-compose exec %s rspec %s' Enter")

(defun rsp ()
  (interactive)
  (shell-command (format *rsp-tmux-cmd* (dev-repo-name) (rsp-path))))

(defun rsp-block ()
  (interactive)
  (let ((path (concat (rsp-path)
                      ":"
                      (number-to-string (1+ (count-lines 1 (point)))))))
    (shell-command (format *rsp-tmux-cmd* (dev-repo-name) path))))

(defun rsp-open ()
  (interactive)
  (let* ((buf (buffer-file-name))
         (repo-path (dev-repo-path))
         (paths (rsp-paths repo-path buf)))

    (unless (string-match "_spec.rb" buf)
      (find-file (mapconcat 'identity
                            (list (car paths)
                                  repo-path
                                  "/spec"
                                  (cadr paths)
                                  "_spec.rb") "")))))

(defun rsp-all ()
  (interactive)
  (shell-command (format *rsp-tmux-cmd* (dev-repo-name) "spec")))

(defun rsp-path ()
  (let* ((buf (buffer-file-name))
         (filename (car (last (split-string buf "/"))))
         (paths (rsp-paths (dev-repo-path) buf)))

    (if (string-match "_spec.rb" filename)
        (mapconcat 'identity (list "." (cadr paths) ".rb") "")
      (mapconcat 'identity (list "spec" (cadr paths) "_spec.rb") ""))))

(defun rsp-paths (repo-path filepath)
  (let ((re (format "\\(%s\/app\\|%s\\|\.rb\\)" repo-path repo-path)))
    (split-string filepath re)))
