;;; indiebrain-recentf.el --- Extensions to recentf.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021  Aaron Kuehler

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
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
;; Extensions to `recentf.el' for my Emacs configuration:
;; <https://github.com/indiebrain/.files/>.


;;; Code:

(require 'recentf)
(require 'indiebrain-common)

;;;###autoload
(defun indiebrain-recentf-keep-predicate (file)
  "Additional conditions for saving FILE in `recentf-list'.
Add this function to `recentf-keep'."
  (cond
   ((file-directory-p file) (file-readable-p file))))

(defvar indiebrain-recentf--history-files '()
  "Minibuffer history for indiebrain-recentf files.")

(defvar indiebrain-recentf--history-dirs '()
  "Minibuffer history for indiebrain-recentf directories.")

(defun indiebrain-recentf--files ()
  "Return completion table with files in `recentf-list'."
  (indiebrain-common-completion-table
   'file
   (mapcar 'abbreviate-file-name recentf-list)))

(defun indiebrain-recentf--files-prompt (files)
  "Helper of `indiebrain-recentf-recent-files' to read FILES."
  (let ((def (car indiebrain-recentf--history-files)))
    (completing-read
     (format "Recentf [%s]: " def)
     files nil t nil 'indiebrain-recentf--history-files def)))

;;;###autoload
(defun indiebrain-recentf-recent-files (file)
  "Select FILE from `recentf-list' using completion."
  (interactive
   (list (indiebrain-recentf--files-prompt (indiebrain-recentf--files))))
  (find-file file)
  (add-to-history 'indiebrain-recentf--history-files file))

(defun indiebrain-recentf--dirs ()
  "Return completion table with directories in `recentf-list'."
  (let ((list (mapcar 'abbreviate-file-name recentf-list)))
    (indiebrain-common-completion-table
     'file
     (delete-dups
      (mapcar (lambda (file)
                (if (file-directory-p file)
                    (directory-file-name file)
                  (substring (file-name-directory file) 0 -1)))
              list)))))

(defun indiebrain-recentf--dirs-prompt (dirs)
  "Helper of `indiebrain-recentf-recent-dirs' to read DIRS."
  (let ((def (car indiebrain-recentf--history-dirs)))
    (completing-read
     (format "Recent dir [%s]: " def)
     dirs nil t nil 'indiebrain-recentf--history-dirs def)))

;;;###autoload
(defun indiebrain-recentf-recent-dirs (dir)
  "Select DIR from `recentf-list' using completion."
  (interactive
   (list (indiebrain-recentf--dirs-prompt (indiebrain-recentf--dirs))))
  (find-file dir)
  (add-to-history 'indiebrain-recentf--history-dirs dir))

(provide 'indiebrain-recentf)
;;; indiebrain-recentf.el ends here
