;;(setq *rsp-tmux-cmd* "clear && docker-compose exec %s rspec %s")
(setq *rsp-tmux-cmd* "clear && docker-compose exec %s rspec %s")

(defun rsp ()
  (interactive)
  (rsp-send-cmd (format *rsp-tmux-cmd* (dev-repo-name) (rsp-path))))


(defun rsp-block ()
  (interactive)
  (let ((path (concat (rsp-path)
                      ":"
                      (number-to-string (1+ (count-lines 1 (point)))))))
    (shell-command (format *rsp-tmux-cmd* (dev-repo-name) path))))

(defun rsp-tool ()
  (interactive)
  (call-process (format "docker exec -it %s which rspec > /dev/null" (dev-repo-name))))

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

(defun rsp-send-cmd (cmd &optional _num)
  (interactive)
  (let ((buf "rsp"))
    (ansi-term (getenv "SHELL") buf)
    (with-current-buffer (concat "*" buf "*")
      (lexical-let ((p (get-buffer-process (current-buffer))))
        (local-set-key (kbd "k")
                       (lambda ()
                         (interactive)
                         (delete-process p)
                         (kill-this-buffer)
                         (local-unset-key (kbd "k"))))


        (goto-char (process-mark p))
        (apply comint-input-sender (list p cmd))))))
