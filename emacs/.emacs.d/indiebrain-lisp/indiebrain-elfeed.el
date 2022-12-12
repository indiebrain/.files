;;; indiebrain-emacs-elfeed.el --- Configuration for elfeed-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2022  Aaron Kuehler <aaron.kuehler@gmail.com>

;; Author: Aaron <aaron.kuehler@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.elfeed/licenses/>.

;;; Commentary:

;; Extensions for elfeed for use in my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'elfeed nil t)
(require 'url-util)
(require 'indiebrain-common)

(defgroup indiebrain-elfeed ()
  "Personal extensions for Elfeed."
  :group 'elfeed)

(defcustom indiebrain-elfeed-feeds-file
  (thread-last user-emacs-directory (expand-file-name "feeds.el.gpg"))
  "Path to file with `elfeed-feeds'."
  :type 'string
  :group 'indiebrain-elfeed)

(defcustom indiebrain-elfeed-archives-directory "~/Documents/feeds/"
  "Path to directory for storing Elfeed entries."
  :type 'string
  :group 'indiebrain-elfeed)

(defcustom indiebrain-elfeed-tag-faces nil
  "Add faces for certain tags.
The tags are: critical, important, personal."
  :type 'boolean
  :group 'indiebrain-elfeed)

(defcustom indiebrain-elfeed-search-tags '(critical important personal)
  "List of user-defined tags.
Used by `indiebrain-elfeed-toggle-tag'."
  :type 'list
  :group 'indiebrain-elfeed)

(defface indiebrain-elfeed-entry-critical '((t :inherit font-lock-warning-face))
  "Face for Elfeed entries tagged with `critical'.")

(defface indiebrain-elfeed-entry-important '((t :inherit font-lock-constant-face))
  "Face for Elfeed entries tagged with `important'.")

(defface indiebrain-elfeed-entry-personal '((t :inherit font-lock-variable-name-face))
  "Face for Elfeed entries tagged with `personal'.")

;;;; Utilities

;;;###autoload
(defun indiebrain-elfeed-load-feeds ()
  "Load file containing the `elfeed-feeds' list.
Add this to `elfeed-search-mode-hook'."
  (let ((feeds indiebrain-elfeed-feeds-file))
    (if (file-exists-p feeds)
        (load-file feeds)
      (user-error "Missing feeds' file"))))

(defvar elfeed-search-face-alist)

;;;###autoload
(defun indiebrain-elfeed-fontify-tags ()
  "Expand Elfeed faces if `indiebrain-elfeed-tag-faces' is non-nil."
  (if indiebrain-elfeed-tag-faces
      (setq elfeed-search-face-alist
            '((critical indiebrain-elfeed-entry-critical)
              (important indiebrain-elfeed-entry-important)
              (personal indiebrain-elfeed-entry-personal)
              (unread elfeed-search-unread-title-face)))
    (setq elfeed-search-face-alist
          '((unread elfeed-search-unread-title-face)))))

(defvar indiebrain-elfeed--tag-hist '()
  "History of inputs for `indiebrain-elfeed-toggle-tag'.")

(defun indiebrain-elfeed--character-prompt (tags)
  "Helper of `indiebrain-elfeed-toggle-tag' to read TAGS."
  (let ((def (car indiebrain-elfeed--tag-hist)))
    (completing-read
     (format "Toggle tag [%s]: " def)
     tags nil t nil 'indiebrain-elfeed--tag-hist def)))

(defvar elfeed-show-entry)
(declare-function elfeed-tagged-p "elfeed")
(declare-function elfeed-search-toggle-all "elfeed")
(declare-function elfeed-show-tag "elfeed")
(declare-function elfeed-show-untag "elfeed")

;;;###autoload
(defun indiebrain-elfeed-toggle-tag (tag)
  "Toggle TAG for the current item.
When the region is active in the `elfeed-search-mode' buffer, all
entries encompassed by it are affected.  Otherwise the item at
point is the target.  For `elfeed-show-mode', the current entry
is always the target.
The list of tags is provided by `indiebrain-elfeed-search-tags'."
  (interactive
   (list
    (intern
     (indiebrain-elfeed--character-prompt indiebrain-elfeed-search-tags))))
  (if (derived-mode-p 'elfeed-show-mode)
      (if (elfeed-tagged-p tag elfeed-show-entry)
          (elfeed-show-untag tag)
        (elfeed-show-tag tag))
    (elfeed-search-toggle-all tag)))

(defvar elfeed-show-truncate-long-urls)
(declare-function elfeed-entry-title "elfeed")
(declare-function elfeed-show-refresh "elfeed")

;;;; General commands

(defvar elfeed-search-filter-active)
(defvar elfeed-search-filter)
(declare-function elfeed-db-get-all-tags "elfeed")
(declare-function elfeed-search-update "elfeed")
(declare-function elfeed-search-clear-filter "elfeed")

(defun indiebrain-elfeed--format-tags (tags sign)
  "Prefix SIGN to each tag in TAGS."
  (mapcar (lambda (tag)
            (format "%s%s" sign tag))
          tags))

;;;###autoload
(defun indiebrain-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.
Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-indiebrainect
      (elfeed-search-clear-filter)
    (let* ((elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (indiebrain-elfeed--format-tags db-tags "+"))
           (minus-tags (indiebrain-elfeed--format-tags db-tags "-"))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           (tags (completing-read-multiple
                  "Apply one or more tags: "
                  all-tags #'indiebrain-common-crm-exclude-selected-p t))
           (input (string-join `(,elfeed-search-filter ,@tags) " ")))
      (setq elfeed-search-filter input))
    (elfeed-search-update :force)))

(provide 'indiebrain-elfeed)
;;; indiebrain-elfeed.el ends here
