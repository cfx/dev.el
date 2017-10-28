(require 'dev.el)

(defun gc (&optional callback)
  (interactive)
  (let ((str (dev-branch-name)))
    (string-match "^[A-Z]+\-[0-9]+" str)

    (let* ((prefix (gc-prefix str))
           (cmd "cd %s && git add . && git commit -am \"%s\"")
           (commit-msg (read-from-minibuffer "" prefix)))

      (shell-command (format cmd (dev-repo-path) commit-msg))

      (if callback (funcall callback)))))

(defun gc-prefix(str)
  (condition-case nil
      (let ((res (match-string 0 str)))
        (if (not (equal res "")) (format "(%s) wip" res)))

    (error nil)))

(defun gcp ()
  (interactive)
  (gc (lambda ()
        (shell-command
         (format "cd %s && git push" (dev-repo-path))))))


(provide 'gcp)
