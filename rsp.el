(require 'comint)
(require 'dev.el)

(defun rsp ()
  (interactive)
  (let* ((cfg (rsp-cfg))
	 (path (rsp-path cfg)))
    (dev-shell-cmd (funcall (rsp-exec-cmd cfg) path))))

(defun rsp-block ()
  (interactive)
  (let* ((cfg (rsp-cfg))
	 (path (concat (rsp-path cfg)
		       ":"
		       (number-to-string (1+ (count-lines 1 (point)))))))

    (dev-shell-cmd (funcall (rsp-exec-cmd cfg) path))))

(defun rsp-test-dir    (cfg) (car cfg))
(defun rsp-test-suffix (cfg) (cadr cfg))
(defun rsp-file-suffix (cfg) (caddr cfg))
(defun rsp-path-regex  (cfg) (cadddr cfg))

(defun rsp-exec-cmd (cfg)
  (cadddr (cdr cfg)))

(defun rsp-cfg ()
  (lexical-let ((suffix (cadr (split-string (buffer-name) "\\.")))
		(repo-name (dev-repo-name)))

    (if (equal suffix "rb")
	(let* ((test-dir "/spec" )
	       (test-suffix "_spec.rb")
	       (test-file-suffix ".rb")
	       (path-regex "\\(%s\/app\\|%s\\|\.rb\\)")
	       (exec-cmd (lambda (path)
			   (format "tmux send-keys -t \"0:%s.0\" \"bundle exec rspec %s\" Enter" repo-name path))))

	  (list test-dir test-suffix test-file-suffix path-regex exec-cmd))

	(let* ((test-dir "/test")
	       (test-suffix "_test.exs")
	       (test-file-suffix ".exs")
	       (path-regex "\\(%s\/lib\\|%s\\|\.ex\\)")
	       (exec-cmd (lambda (path)
			   (format "tmux send-keys -t \"0:%s.0\" \"mix test %s\" Enter" repo-name path))))

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


(defun rsp-path (cfg)
  (let* ((buf (buffer-file-name))
         (filename (car (last (split-string buf "/"))))
         (paths (rsp-paths (dev-repo-path) buf cfg)))

    (if (string-match (rsp-test-suffix cfg) filename)
        (mapconcat 'identity (list "." (cadr paths) (rsp-file-suffix cfg)) "")
      (mapconcat 'identity (list rsp-test-dir (cadr paths) (rsp-test-suffix cfg)) ""))))

(defun rsp-paths (repo-path filepath cfg)
  (let ((re (format (rsp-path-regex cfg) repo-path repo-path)))
    (split-string filepath re)))

(provide 'rsp)
