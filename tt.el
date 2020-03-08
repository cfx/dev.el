(require 'comint)
(require 'dev.el)

(defun tt ()
  (interactive)
  (let* ((cfg (tt-cfg))
	 (path (tt-path cfg)))
    (dev-shell-cmd (funcall (tt-exec-cmd cfg) path))))

(defun tt-block ()
  (interactive)
  (let* ((cfg (tt-cfg))
	 (path (concat (tt-path cfg)
		       ":"
		       (number-to-string (1+ (count-lines 1 (point)))))))

    (dev-shell-cmd (funcall (tt-exec-cmd cfg) path))))

(defun tt-test-dir    (cfg) (car cfg))
(defun tt-test-suffix (cfg) (cadr cfg))
(defun tt-file-suffix (cfg) (caddr cfg))
(defun tt-path-regex  (cfg) (cadddr cfg))

(defun tt-exec-cmd (cfg)
  (cadddr (cdr cfg)))

(defun tt-cfg ()
  (lexical-let ((suffix (cadr (split-string (buffer-name) "\\.")))
		(repo-name (dev-repo-name)))

    (if (equal suffix "rb")
	(let* ((test-dir "/spec" )
	       (test-suffix "_spec.rb")
	       (test-file-suffix ".rb")
	       (path-regex "\\(%s\/app\\|%s\\|\.rb\\)")
	       (exec-cmd (lambda (path)
			   (format "tmux send-keys -t \"0:%s.0\" \"bundle exec ttec %s\" Enter" repo-name path))))

	  (list test-dir test-suffix test-file-suffix path-regex exec-cmd))

	(let* ((test-dir "/test")
	       (test-suffix "_test.exs")
	       (test-file-suffix ".exs")
	       (path-regex "\\(%s\/lib\\|%s\\|\.ex\\)")
	       (exec-cmd (lambda (path)
			   (format "tmux send-keys -t \"0:%s.0\" \"mix test %s\" Enter" repo-name path))))

	  (list test-dir test-suffix test-file-suffix path-regex exec-cmd)))))

(defun tt-open ()
  (interactive)
  (let* ((buf (buffer-file-name))
	 (cfg (tt-cfg))
         (repo-path (dev-repo-path))
         (paths (tt-paths repo-path buf cfg)))

    (unless (string-match (tt-test-suffix cfg) buf)
      (if (= (length (window-list)) 1)
	  (split-window-right))
      (other-window 1)

      (find-file (mapconcat 'identity
                            (list (car paths)
                                  repo-path
                                  (tt-test-dir cfg)
                                  (cadr paths)
                                  (tt-test-suffix cfg)) "")))))


(defun tt-path (cfg)
  (let* ((buf (buffer-file-name))
         (filename (car (last (split-string buf "/"))))
         (paths (tt-paths (dev-repo-path) buf cfg)))

    (if (string-match (tt-test-suffix cfg) filename)
        (mapconcat 'identity (list "." (cadr paths) (tt-file-suffix cfg)) "")
      (mapconcat 'identity (list tt-test-dir (cadr paths) (tt-test-suffix cfg)) ""))))

(defun tt-paths (repo-path filepath cfg)
  (let ((re (format (tt-path-regex cfg) repo-path repo-path)))
    (split-string filepath re)))

(provide 'tt)
