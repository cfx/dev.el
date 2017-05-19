(require 'cl)

(defun dev-shell-cmd (cmd)
  (shell-command-to-string (concat cmd "| tr -d '\r\n'")))

(defun dev-repo-path ()
  (dev-shell-cmd "git rev-parse --show-toplevel"))

(defun dev-repo-name ()
  (dev-shell-cmd "basename `git rev-parse --show-toplevel`"))

(defun dev-branch-name ()
  (dev-shell-cmd "git rev-parse --abbrev-ref HEAD"))
