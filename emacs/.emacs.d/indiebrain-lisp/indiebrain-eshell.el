;;; indiebrain-eshell.el --- Extensions to Eshell for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my Eshell extensions, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(require 'eshell)
(require 'esh-mode)
(require 'em-dirs)
(require 'em-hist)

;;;; Customisation options

(defgroup indiebrain-eshell ()
  "Extensions for Eshell and related libraries."
  :group 'shell)

(defcustom indiebrain-eshell-output-buffer "*Exported Eshell output*"
  "Name of buffer with the last output of Eshell command.
Used by `indiebrain-eshell-export'."
  :type 'string
  :group 'indiebrain-eshell)

(defcustom indiebrain-eshell-output-delimiter "* * *"
  "Delimiter for successive `indiebrain-eshell-export' outputs.
This is formatted internally to have newline characters before
and after it."
  :type 'string
  :group 'indiebrain-eshell)

;;;; Commands

(autoload 'ffap-file-at-point "ffap.el")

(defmacro indiebrain-eshell-ffap (name doc &rest body)
  "Make `find-file-at-point' commands for Eshell.
NAME is how the function is called.  DOC is the function's
documentation string.  BODY is the set of arguments passed to the
`if' statement to be evaluated when a file at point is present."
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((file (ffap-file-at-point)))
       (if file
           ,@body
         (user-error "No file at point")))))

(indiebrain-eshell-ffap
 indiebrain-eshell-ffap-insert
 "Insert (cat) contents of file at point."
 (progn
   (goto-char (point-max))
   (insert (format "cat %s" file))
   (eshell-send-input)))

(indiebrain-eshell-ffap
 indiebrain-eshell-ffap-kill-save
 "Add to kill-ring the absolute path of file at point."
 (progn
   (kill-new (format "%s/%s" (eshell/pwd) file))
   (message "Copied full path of %s" file)))

(indiebrain-eshell-ffap
 indiebrain-eshell-ffap-find-file
 "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
 (find-file file))

(indiebrain-eshell-ffap
 indiebrain-eshell-ffap-dired-jump
 "Jump to the parent directory of the file at point."
 (dired (file-name-directory file)))

(defun indiebrain-eshell--command-prompt-output ()
  "Capture last command prompt and its output."
  (let ((beg (save-excursion
               (goto-char (eshell-beginning-of-input))
               (goto-char (point-at-bol)))))
  (when (derived-mode-p 'eshell-mode)
    (buffer-substring-no-properties beg (eshell-end-of-output)))))

;;;###autoload
(defun indiebrain-eshell-export ()
  "Produce a buffer with output of the last Eshell command.
If `indiebrain-eshell-output-buffer' does not exist, create it.  Else
append to it, while separating multiple outputs with
`indiebrain-eshell-output-delimiter'."
  (interactive)
  (let ((eshell-output (indiebrain-eshell--command-prompt-output)))
    (with-current-buffer (get-buffer-create indiebrain-eshell-output-buffer)
      (goto-char (point-max))
      (unless (eq (point-min) (point-max))
        (insert (format "\n%s\n\n" indiebrain-eshell-output-delimiter)))
      (goto-char (point-at-bol))
      (insert eshell-output)
      (switch-to-buffer-other-window (current-buffer)))))

;;;###autoload
(defun indiebrain-eshell-redirect-to-buffer (buffer)
  "Complete the syntax for appending Eshell output to BUFFER."
  (interactive
   (list (read-buffer "Redirect to buffer: ")))
  (insert
   (format " >>> #<%s>" buffer)))

;;;###autoload
(defun indiebrain-eshell-narrow-output-highlight-regexp (regexp)
  "Narrow to last command output and highlight REGEXP."
  (interactive
   (list (read-regexp "Regexp to highlight")))
  (narrow-to-region (eshell-beginning-of-output)
                    (eshell-end-of-output))
  (goto-char (point-min))
  (highlight-regexp regexp 'hi-yellow)
  (message "Narrowed to last output and highlighted < %s >" regexp))

;;;###autoload
(defun indiebrain-eshell-complete-recent-dir (&optional arg)
  "Switch to a recent Eshell directory using completion.
With optional ARG prefix argument (\\[universal-argument]) also
open the directory in a `dired' buffer."
  (interactive "P")
  (let* ((dirs (ring-elements eshell-last-dir-ring))
         (dir (completing-read "Switch to recent dir: " dirs nil t)))
    (insert dir)
    (eshell-send-input)
    (when arg
      (dired dir))))

;;;###autoload
(defun indiebrain-eshell-complete-history ()
  "Insert element from Eshell history using completion."
  (interactive)
  (let ((hist (ring-elements eshell-history-ring)))
    (insert
     (completing-read "Input from history: " hist nil t))))

(autoload 'cl-remove-if-not "cl-seq")

;;;###autoload
(defun indiebrain-eshell-find-subdirectory-recursive ()
  "Recursive `eshell/cd' to subdirectory.
This command has the potential for infinite recursion: use it
wisely or prepare to call `eshell-interrupt-process'."
  (interactive)
  (let* ((dir (abbreviate-file-name (eshell/pwd)))
         (contents (directory-files-recursively dir ".*" t nil nil))
         (dirs (cl-remove-if-not (lambda (x)
                                   (or (file-directory-p x)
                                       (string-match-p "\\.git" x)))
                                 contents))
         (selection (completing-read
                     (format "Find sub-dir from %s: "
                             (propertize dir 'face 'success))
                     dirs nil t)))
    (insert selection)
    (eshell-send-input)))

;;;###autoload
(defun indiebrain-eshell-root-dir ()
  "Switch to the root directory of the present project."
  (interactive)
  (let ((root (or (vc-root-dir)
                  (locate-dominating-file "." ".git"))))
    (if root
        (progn
          (insert root)
          (eshell-send-input))
      (user-error "Cannot find a project root here"))))

(provide 'indiebrain-eshell)
;;; indiebrain-eshell.el ends here
