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
                          (rsp-path (rsp-cfg))))))

(defun rsp-block ()
  (interactive)
    (message (rsp-path (rsp-cfg)))
  (let ((path (concat (rsp-path (rsp-cfg))
                      ":"
                      (number-to-string (1+ (count-lines 1 (point)))))))

    (rsp-send-cmd (funcall (rsp-exec-cmd cfg) path))))

(defun rsp-test-dir    (cfg) (car cfg))
(defun rsp-test-suffix (cfg) (cadr cfg))
(defun rsp-file-suffix (cfg) (caddr cfg))
(defun rsp-path-regex  (cfg) (cadddr cfg))

(defun rsp-exec-cmd (cfg)
  (cadddr (cdr cfg)))


(defun rsp-cfg ()
  (let ((suffix (cadr (split-string (buffer-name) "\\."))))
    (if (equal suffix "rb")
	(let* ((test-dir "/spec" )
	       (test-suffix "_spec.rb")
	       (test-file-suffix ".rb")
	       (path-regex "\\(%s\/app\\|%s\\|\.rb\\)")
	       (exec-cmd (lexical-let ((container-name (dev-repo-name)))
			   (lambda (path)
			     (format *rsp-test-cmd*
				     container-name
				     container-name
				     path)))))

	  (list test-dir test-suffix test-file-suffix path-regex exec-cmd))

	(let* ((test-dir "/test")
	       (test-suffix "_test.exs")
	       (test-file-suffix ".exs")
	       (path-regex "\\(%s\/lib\\|%s\\|\.ex\\)")
	       (exec-cmd (lambda (path)
			   (format "mix test %s" path))))

	  (list test-dir test-suffix test-file-suffix path-regex exec-cmd)))))

(defun rsp-open ()
  (interactive)
  (let* ((buf (buffer-file-name))
	 (cfg (rsp-cfg))
         (repo-path (dev-repo-path))
         (paths (rsp-paths repo-path buf cfg)))

    (unless (string-match (rsp-test-suffix cfg) buf)
      (if (= (length (window-list)) 1)
	  (split-window-right))
      (other-window 1)

      (find-file (mapconcat 'identity
                            (list (car paths)
                                  repo-path
                                  (rsp-test-dir cfg)
                                  (cadr paths)
                                  (rsp-test-suffix cfg)) "")))))

(defun rsp-all ()
  (interactive)
  (rsp-send-cmd (format *rsp-test-all-cmd* (dev-repo-name))))

(defun rsp-path (cfg)
  (let* ((buf (buffer-file-name))
         (filename (car (last (split-string buf "/"))))
         (paths (rsp-paths (dev-repo-path) buf cfg)))

    (if (string-match (rsp-test-suffix) filename)
        (mapconcat 'identity (list "." (cadr paths) (rsp-file-suffix)) "")
      (mapconcat 'identity (list rsp-test-dir (cadr paths) (rsp-test-suffix)) ""))))

(defun rsp-paths (repo-path filepath cfg)
  (let ((re (format (rsp-path-regex cfg) repo-path repo-path)))
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
