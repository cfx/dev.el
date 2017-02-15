;;; ghl.el --- Open current file on http://github.com

;; Copyright (C) 2015 Jozef Chraplewski

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; You need to define *ghl-browser-cmd* e.g
;; (setq *ghl-browser-cmd* "/usr/bin/open -a '/Applications/Google Chrome.app' '%s'")

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
    (shell-command (format "echo '%s' | pbcopy" link))
    (message (format "%s copied into clipboard" link))))


(defun ghl-get-url ()
  (let ((repo (read-from-minibuffer "Repo: " (ghl-get-repo-name)))
        (branch (read-from-minibuffer "Branch: " (ghl-get-branch-name)))
        (line (int-to-string (1+ (count-lines 1 (point))))))

    (ghl-set-user!)

    (concat *ghl-github-url*
            *ghl-github-user*
            "/" repo "/blob/" branch "/" (ghl-get-filepath repo) "#L" line)))

(defun ghl-get-filepath (repo)
  (let* ((full-path (split-string (buffer-file-name) "/"))
         (path (subseq full-path
                       (1+ (cl-position repo full-path :test 'equal))
                       (length full-path))))
    (mapconcat 'identity path "/")))

(defun ghl-repo-path ()
  (shell-command-to-string "git rev-parse --show-toplevel | tr -d '\r\n'"))

(defun ghl-get-repo-name ()
  (shell-command-to-string "basename `git rev-parse --show-toplevel` | tr -d '\r\n'"))

(defun ghl-get-branch-name ()
  (shell-command-to-string "git rev-parse --abbrev-ref HEAD | tr -d '\r\n'"))


(defun ghl-get-user ()
  (if (boundp '*ghl-github-user*)
      *ghl-github-user*))

(defun ghl-set-user! ()
  (let ((user (read-from-minibuffer "Username: " (ghl-get-user))))
    (if (equal user "")
        (error "error: no github username provided"))

    (setq *ghl-github-user* user)
    user))



(defun gc (&optional callback)
  (interactive)
  (let ((str (ghl-get-branch-name)))
    (string-match "^[A-Z]+\-[0-9]+" str)

    (let* ((prefix (gc-prefix str))
           (cmd "cd %s && git add . && git commit -am \"%s\"")
           (commit-msg (read-from-minibuffer "" prefix)))

      (shell-command (format cmd (ghl-repo-path) commit-msg))

      (if callback (funcall callback)))))

(defun gc-prefix(str)
  (condition-case nil
      (let ((res (match-string 0 str)))
        (if (not (equal res "")) (format "(%s) wip" res)))

    (error nil)))

(defun gcp ()
  (interactive)
  (gc (lambda () (shell-command-to-string "git push"))))
