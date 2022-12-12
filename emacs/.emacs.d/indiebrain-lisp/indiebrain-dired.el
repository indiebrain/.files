;;; indiebrain-dired.el --- externsions to dired.el for my Emacs configuration  -*- lexical-binding: t; -*-


;; Copyright (C) 2012-2022  Aaron Kuehler <aaron.kuehler@gmail.com>

;; Author: Aaron <aaron.kuehler@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions to dired.el for my Emacs configuration:
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'indiebrain-common)

(defgroup indiebrain-dired ()
  "Extensions for Dired."
  :group 'dired)

;;;; File associations

(defcustom indiebrain-dired-media-extensions
  "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)"
  "Regular expression for media file extensions.
Also see the function `indiebrain-dired-media-player' and the variable
`indiebrain-dired-media-players'.
To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type 'string
  :group 'indiebrain-dired)

(defcustom indiebrain-dired-image-extensions
  "\\.\\(png\\|jpe?g\\|tiff\\)"
  "Regular expression for media file extensions.
Also see the function `indiebrain-dired-image-viewer' and the variable
`indiebrain-dired-image-viewers'.
To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type 'string
  :group 'indiebrain-dired)

(defcustom indiebrain-dired-media-players '("mpv" "vlc")
  "List of strings for media player programs.
Also see the function `indiebrain-dired-media-player' and the variable
`indiebrain-dired-media-extensions'.
To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type '(repeat string)
  :group 'indiebrain-dired)

(defcustom indiebrain-dired-image-viewers '("feh" "sxiv")
  "List of strings for image viewer programs.
Also see the function `indiebrain-dired-image-viewer' and the variable
`indiebrain-dired-image-extensions'.
To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type '(repeat string)
  :group 'indiebrain-dired)

(declare-function cl-remove-if "cl-lib")

(defmacro indiebrain-dired-file-association (name programs)
  "Make NAME function to check for PROGRAMS."
  (declare (indent defun))
  `(defun ,name ()
     ,(format "Return available program.
This checks each entry in `%s' and returns the first program that
is available on the system.  If none is present, it falls back to
xdg-open (for GNU/Linux only).
This function is for use in `dired-guess-shell-alist-user'."
              programs)
     (catch :found
       (dolist (p (append ,programs '("xdg-open")))
         (when (executable-find p)
           (throw :found p))))))

(indiebrain-dired-file-association
  indiebrain-dired-media-player
  indiebrain-dired-media-players)

(indiebrain-dired-file-association
  indiebrain-dired-image-viewer
  indiebrain-dired-image-viewers)

;;;; General commands

(autoload 'dired-mark-files-regexp "dired")
(autoload 'dired-toggle-marks "dired")
(autoload 'dired-do-kill-lines "dired-aux")

(defvar indiebrain-dired--limit-hist '()
  "Minibuffer history for `indiebrain-dired-limit-regexp'.")

;;;###autoload
(defun indiebrain-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.
With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.
Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'indiebrain-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines)
  (add-to-history 'indiebrain-dired--limit-hist regexp))

(defvar indiebrain-dired--find-grep-hist '()
  "Minibuffer history for `indiebrain-dired-grep-marked-files'.")

; Also see `indiebrain-search-grep' from indiebrain-search.el.
;;;###autoload
(defun indiebrain-dired-grep-marked-files (regexp &optional arg)
  "Run `find' with `grep' for REGEXP on marked files.
When no files are marked or when just a single one is marked,
search the entire directory instead.
With optional prefix ARG target a single marked file.
We assume that there is no point in marking a single file and
running find+grep on its contents.  Visit it and call `occur' or
run grep directly on it without the whole find part."
  (interactive
   (list
    (read-string "grep for PATTERN (marked files OR current directory): " nil 'indiebrain-dired--find-grep-hist)
    current-prefix-arg)
   dired-mode)
  (when-let* ((marks (dired-get-marked-files 'no-dir))
              (files (mapconcat #'identity marks " "))
              (args (if (or arg (length> marks 1))
                        ;; Thanks to Sean Whitton for pointing out an
                        ;; earlier superfluity of mine: we do not need
                        ;; to call grep through find when we already
                        ;; know the files we want to search in.  Check
                        ;; Sean's dotfiles:
                        ;; <https://git.spwhitton.name/dotfiles>.
                        ;;
                        ;; Any other errors or omissions are my own.
                        (format "grep -nH --color=auto %s %s" (shell-quote-argument regexp) files)
                      (concat
                       "find . -not " (shell-quote-argument "(")
                       " -wholename " (shell-quote-argument "*/.git*")
                       " -prune " (shell-quote-argument ")")
                       " -type f"
                       " -exec grep -nHE --color=auto " regexp " "
                       (shell-quote-argument "{}")
                       " " (shell-quote-argument ";") " "))))
    (compilation-start
     args
     'grep-mode
     (lambda (mode) (format "*indiebrain-dired-find-%s for '%s'" mode regexp))
     t)))

;;;; Subdir extras and Imenu setup

(defvar indiebrain-dired--directory-header-regexp "^ +\\(.+\\):\n"
  "Pattern to match Dired directory headings.")

;;;###autoload
(defun indiebrain-dired-subdirectory-next (&optional arg)
  "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir indiebrain-dired--directory-header-regexp))
    (goto-char (point-at-eol))
    (if (re-search-forward subdir nil t (or arg nil))
        (progn
          (goto-char (match-beginning 1))
          (goto-char (point-at-bol)))
      (goto-char pos))))

;;;###autoload
(defun indiebrain-dired-subdirectory-previous (&optional arg)
  "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir indiebrain-dired--directory-header-regexp))
    (goto-char (point-at-bol))
    (if (re-search-backward subdir nil t (or arg nil))
        (goto-char (point-at-bol))
      (goto-char pos))))

(autoload 'dired-current-directory "dired")
(autoload 'dired-kill-subdir "dired-aux")

;;;###autoload
(defun indiebrain-dired-remove-inserted-subdirs ()
  "Remove all inserted Dired subdirectories."
  (interactive)
  (goto-char (point-max))
  (while (and (indiebrain-dired-subdirectory-previous)
              (not (equal (dired-current-directory)
                          (expand-file-name default-directory))))
      (dired-kill-subdir)))

(autoload 'cl-remove-if-not "cl-seq")

(defun indiebrain-dired--dir-list (list)
  "Filter out non-directory file paths in LIST."
  (cl-remove-if-not
   (lambda (dir)
     (file-directory-p dir))
   list))

(defun indiebrain-dired--insert-dir (dir &optional flags)
  "Insert DIR using optional FLAGS."
  (dired-maybe-insert-subdir (expand-file-name dir) (or flags nil)))

(autoload 'dired-get-filename "dired")
(autoload 'dired-get-marked-files "dired")
(autoload 'dired-maybe-insert-subdir "dired-aux")
(defvar dired-subdir-switches)
(defvar dired-actual-switches)

;;;###autoload
(defun indiebrain-dired-insert-subdir (&optional arg)
  "Generic command to insert subdirectories in Dired buffers.
When items are marked, insert those which are subsirectories of
the current directory.  Ignore regular files.
If no marks are active and point is on a subdirectory line,
insert it directly.
If no marks are active and point is not on a subdirectory line,
prompt for a subdirectory using completion.
With optional ARG as a single prefix (`\\[universal-argument]')
argument, prompt for command line flags to pass to the underlying
'ls' program.
With optional ARG as a double prefix argument, remove all
inserted subdirectories."
  (interactive "p")
  (let* ((name (dired-get-marked-files))
         (flags (when (eq arg 4)
                  (read-string "Flags for `ls' listing: "
                               (or dired-subdir-switches dired-actual-switches)))))
    (cond  ; NOTE 2021-07-20: `length>', `length=' are from Emacs28
     ((eq arg 16)
      (indiebrain-dired-remove-inserted-subdirs))
     ((and (length> name 1) (indiebrain-dired--dir-list name))
      (mapc (lambda (file)
              (when (file-directory-p file)
                (indiebrain-dired--insert-dir file flags)))
            name))
     ((and (length= name 1) (file-directory-p (car name)))
      (indiebrain-dired--insert-dir (car name) flags))
     (t
      (let ((selection (read-directory-name "Insert directory: ")))
        (indiebrain-dired--insert-dir selection flags))))))

(defun indiebrain-dired--imenu-prev-index-position ()
  "Find the previous file in the buffer."
  (let ((subdir indiebrain-dired--directory-header-regexp))
    (re-search-backward subdir nil t)))

(defun indiebrain-dired--imenu-extract-index-name ()
  "Return the name of the file at point."
  (file-relative-name
   (buffer-substring-no-properties (+ (point-at-bol) 2)
                                   (1- (point-at-eol)))))

;;;###autoload
(defun indiebrain-dired-setup-imenu ()
  "Configure imenu for the current dired buffer.
Add this to `dired-mode-hook'."
  (set (make-local-variable 'imenu-prev-index-position-function)
       'indiebrain-dired--imenu-prev-index-position)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'indiebrain-dired--imenu-extract-index-name))

(provide 'indiebrain-dired)
;;; indiebrain-dired.el ends here
