(require 'comint)
(require 'dev.el)

(setq *rsp-proc* nil)
(setq *rsp-test-all-cmd* "docker-compose exec %s rake test")
(setq *rsp-test-cmd* "a=$(if docker-compose exec %s which rspec > /dev/null;
then echo 'bundle exec rspec'; else echo ruby; fi); clear; docker-compose exec %s $a %s")

(define-minor-mode rsp-minor-mode
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "\C-v\C-k") (lambda ()
                                        (interactive)
                                        (delete-process *rsp-proc*)
                                        (kill-this-buffer)))
            map))

(defun rsp ()
  (interactive)
  (let ((container-name (dev-repo-name)))
    (rsp-send-cmd (format *rsp-test-cmd*
                          container-name
                          container-name
                          (rsp-path)))))

(defun rsp-block ()
  (interactive)
  (let ((path (concat (rsp-path)
                      ":"
                      (number-to-string (1+ (count-lines 1 (point))))))
        (container-name (dev-repo-name)))

    (rsp-send-cmd (format *rsp-test-cmd*
                          container-name
                          container-name
                          path))))

(defun rsp-open ()
  (interactive)
  (let* ((buf (buffer-file-name))
         (repo-path (dev-repo-path))
         (paths (rsp-paths repo-path buf)))

    (unless (string-match "_spec.rb" buf)
      (if (= (length (window-list)) 1)
	  (split-window-right))
      (other-window 1)
      (find-file (mapconcat 'identity
                            (list (car paths)
                                  repo-path
                                  "/spec"
                                  (cadr paths)
                                  "_spec.rb") "")))))

(defun rsp-all ()
  (interactive)
  (rsp-send-cmd (format *rsp-test-all-cmd* (dev-repo-name))))

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

(defun rsp-send-cmd (cmd)
  (let* ((buf "rsp")
         (buf-term (concat "*" buf "*")))

    (if (get-buffer buf-term)
        (switch-to-buffer buf-term)
      (ansi-term (getenv "SHELL") buf))

    (with-current-buffer buf-term
      (rsp-minor-mode 1)
      (setq *rsp-proc* (get-buffer-process (current-buffer)))
      (goto-char (process-mark *rsp-proc*))
      (apply comint-input-sender (list *rsp-proc* cmd)))))

(provide 'rsp)
