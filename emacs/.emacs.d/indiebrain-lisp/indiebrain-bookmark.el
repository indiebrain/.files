;;; indiebrain-bookmark.el --- Extensions to bookmakrs for my Emacs configuration  -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Bookmark extensions for my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(require 'indiebrain-common)

(defgroup indiebrain-bookmark ()
  "Bookmark extras for my dotemacs."
  :group 'matching)

;;;; Extend Bookmark menu font-lock

(defface indiebrain-bookmark-url '((t :inherit link :underline nil))
  "Face for URL bookmarks.")

(defface indiebrain-bookmark-pdf '((t :inherit error))
  "Face for PDF bookmarks.")

(defface indiebrain-bookmark-directory '((t :inherit success))
  "Face for directory bookmarks.")

;; TODO 2021-09-08: We should be able to filter out bookmarks from the
;; likes of Info and VC-Dir which set a file path even though they are
;; not really intended to be visited as files.
(defconst indiebrain-bookmark-keywords
  `((,(concat "\\(.*\\)" " " indiebrain-common-url-regexp)
     (1 '(bold indiebrain-bookmark-url) t)
     (2 'indiebrain-bookmark-url t))
    ("\\(.*\\)\\( [~/].*\\.pdf\\)"
     (1 '(bold indiebrain-bookmark-pdf) t)
     (2 'indiebrain-bookmark-pdf t))
    ("\\(.*\\)\\( [~/].*/$\\)"
     (1 '(bold indiebrain-bookmark-directory) t)
     (2 'indiebrain-bookmark-directory t))
    ("\\(.*org.*last-stored.*\\)"
     (1 'shadow t)))
  "Extra font-lock patterns for the Bookmark menu.")

;;;###autoload
(define-minor-mode indiebrain-bookmark-extra-keywords
  "Apply extra font-lock rules to bookmark list buffers."
  :init-value nil
  :global t
  (if indiebrain-bookmark-extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil indiebrain-bookmark-keywords nil)
        (add-hook 'bookmark-bmenu-mode-hook #'indiebrain-bookmark-extra-keywords))
    (font-lock-remove-keywords nil indiebrain-bookmark-keywords)
    (remove-hook 'bookmark-bmenu-mode-hook #'indiebrain-bookmark-extra-keywords)
    (font-lock-flush (point-min) (point-max))))


(provide 'indiebrain-bookmark)
;;; indiebrain-bookmark.el ends here
