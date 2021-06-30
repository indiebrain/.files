;;; indiebrain-project.el --- Extensions to project.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021  Aaron Kuehler

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my project.el extensions, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'indiebrain-common)
(require 'vc)

(defgroup indiebrain-project ()
  "Extensions for project.el and related libraries."
  :group 'project)

(defcustom indiebrain-project-project-roots (list "~/Developer")
  "List of directories with version-controlled projects.
To be used by `indiebrain-project-switch-project'."
  :type 'list
  :group 'indiebrain-project)

(defcustom indiebrain-project-commit-log-limit 25
  "Limit commit logs for project to N entries by default.
A value of 0 means 'unlimited'."
  :type 'integer
  :group 'indiebrain-project)

(defcustom indiebrain-project-large-file-lines 1000
  "How many lines constitute a 'large file' (integer).
This determines whether some automatic checks should be executed
or not, such as `indiebrain-project-flymake-mode-activate'."
  :type 'integer
  :group 'indiebrain-project)

;; Copied from Manuel Uberti:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
;;
;; Note that I prefer adding some dummy doc string over seeing spurious
;; compiler warnings.
(cl-defmethod project-root ((project (head local)))
  "Project root for PROJECT with HEAD and LOCAL."
  (cdr project))

;; Copied from Manuel Uberti and tweaked accordingly:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
(defun indiebrain-project--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (unless (executable-find "fd")
    (error "Cannot find 'fd' command in shell environment $PATH"))
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (split-string (shell-command-to-string command) "\0" t))))

;; Copied from Manuel Uberti:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
;;
;; Same principle for the dummy doc string.
(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects.

Project root for PROJECT with HEAD and LOCAL, plus optional
DIRS."
  (mapcan #'indiebrain-project--project-files-in-directory
          (or dirs (list (project-root project)))))

(defun indiebrain-project--list-projects ()
  "Produce list of projects in `indiebrain-project-project-roots'."
  (let* ((dirs indiebrain-project-project-roots)
         (dotless directory-files-no-dot-files-regexp)
         (cands (mapcan (lambda (d)
                          (directory-files d t dotless))
                        dirs)))
    (mapcar (lambda (d)
              (list (abbreviate-file-name d)))
            cands)))

;; FIXME: this is fragile since we do not store the original value of
;; `project--list' and may risk losing data.
;;;###autoload
(defun indiebrain-project-add-projects ()
  "Append `indiebrain-project--list-projects' to `project--list'."
  (interactive)
  (project--ensure-read-project-list)
  (let ((projects (indiebrain-project--list-projects)))
    (setq project--list (append projects project--list))
    (project--write-project-list)))

;; TODO: use `completing-read-multiple' and learn how to delete a list
;; from an alist.
;;;###autoload
(defun indiebrain-project-remove-project ()
  "Remove project from `project--list' using completion."
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
         (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))
    (project--write-project-list)))

(defun indiebrain-project--directory-subdirs (dir)
  "Return list of subdirectories in DIR."
  (cl-remove-if-not
   (lambda (x)
     (file-directory-p x))
   (directory-files-recursively dir ".*" t t)))

;; TODO: generalise this for all VC backends?  Which ones?
(defun indiebrain-project--directory-subdirs-no-git (dir)
  "Remove .git dirs from DIR."
  (cl-remove-if
   (lambda (x)
     (string-match-p "\\.git" x))
   (indiebrain-project--directory-subdirs dir)))

;; NOTE: in practice this is for `embark.el' (or equivalent
;; functionality), as it allows it to export the candidates in a Dired
;; buffer.
(defun indiebrain-project--subdirs-completion-table (dir)
  "Return list of subdirectories in DIR with completion table."
  (indiebrain-common-completion-table
   'file
   (indiebrain-project--directory-subdirs-no-git dir)))

(defvar indiebrain-project--subdir-hist '()
  "Minibuffer history for `indiebrain-project-find-subdir'.")

;;;###autoload
(defun indiebrain-project-find-subdir ()
  "Find subdirectories in the current project, using completion."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (subdirs (indiebrain-project--subdirs-completion-table dir))
         (directory (completing-read "Select Project subdir: " subdirs
                                     nil t nil 'indiebrain-project--subdir-hist)))
    (dired directory)
    (add-to-history 'indiebrain-project--subdir-hist dir)))

;; FIXME: the buttons at the bottom of the log for displaying more
;; commits do not seem to work with this.
;;;###autoload
(defun indiebrain-project-commit-log (&optional arg)
  "Print commit log for the current project.
With optional prefix ARG (\\[universal-argument]) shows expanded
commit messages and corresponding diffs.

The log is limited to the integer specified by
`indiebrain-project-commit-log-limit'.  A value of 0 means
'unlimited'."
  (interactive "P")
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (backend (vc-responsible-backend dir))
         (num indiebrain-project-commit-log-limit)
         (int (indiebrain-common-number-integer-p num))
         (limit (if (= int 0) t int))
         (diffs (if arg 'with-diff nil))
         (vc-log-short-style (unless diffs '(directory))))
    (vc-print-log-internal backend (list dir) nil nil limit diffs)))

;;;###autoload
(defun indiebrain-project-retrieve-tag ()
  "Run `vc-retrieve-tag' on project and switch to the root dir.
Basically switches to a new branch or tag."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (name
          (vc-read-revision "Tag name: "
                            (list dir)
                            (vc-responsible-backend dir))))
    (vc-retrieve-tag dir name)
    (project-dired)))

(autoload 'magit-status "magit")

;;;###autoload
(defun indiebrain-project-magit-status ()
  "Run `magit-status' on project."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr)))
    (magit-status dir)))

(defun indiebrain-project--max-line ()
  "Return the last line's number."
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

(defun indiebrain-project--large-file-p (&optional n)
  "Check if lines exceed `indiebrain-project-large-file-lines'.
Optional N integer overrides that variable's value."
  (let* ((num (or n indiebrain-project-large-file-lines))
         (int (indiebrain-common-number-integer-p num)))
    (> (indiebrain-project--max-line) int)))

;; Copied from Manuel Uberti:
;; <https://www.manueluberti.eu/emacs/2020/11/21/flymake-projects/>.
;;;###autoload
(defun indiebrain-project-flymake-mode-activate ()
  "Activate Flymake only for `project-known-project-roots'."
  (project--ensure-read-project-list)
  (let ((known-projects (project-known-project-roots))
        (pr (or (vc-root-dir)
                (locate-dominating-file "." ".git")
                default-directory))
        (modes (indiebrain-common-minor-modes-active)))
    (if (and (eq buffer-read-only nil)
             (member pr known-projects)
             (not (indiebrain-project--large-file-p))
             (not (member 'org-src-mode modes))
             (not (eq buffer-file-truename nil)))
        (flymake-mode 1)
      (flymake-mode -1))))

(defvar org-src-mode-hook)

(add-hook 'org-src-mode-hook #'indiebrain-project-flymake-mode-activate)
(add-hook 'prog-mode-hook #'indiebrain-project-flymake-mode-activate)

(provide 'indiebrain-project)
;;; indiebrain-project.el ends here
