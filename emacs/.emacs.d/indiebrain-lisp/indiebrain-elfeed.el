;;; indiebrain-elfeed.el --- Elfeed extensions for my dotemacs -*- lexical-binding: t -*-

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
;; Extensions for Elfeed, intended for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'elfeed nil t)
(require 'indiebrain-common)

(defgroup indiebrain-elfeed ()
  "Personal extensions for Elfeed."
  :group 'elfeed)

(defcustom indiebrain-elfeed-feeds-file (concat user-emacs-directory "feeds.el.gpg")
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

(defcustom indiebrain-elfeed-laptop-resolution-breakpoint 1366
  "Determine video resolution based on this display width.
This is used to check whether I am on the laptop or whether an
external display is attached to it. In the latter case, a
`indiebrain-elfeed-video-resolution-large' video resolution will
be used, else `indiebrain-elfeed-video-resolution-small'."
  :type 'integer
  :group 'indiebrain-elfeed)

(defcustom indiebrain-elfeed-video-resolution-small 720
  "Set video resolution width for smaller displays."
  :type 'integer
  :group 'indiebrain-elfeed)

(defcustom indiebrain-elfeed-video-resolution-large 1080
  "Set video resolution width for larger displays."
  :type 'integer
  :group 'indiebrain-elfeed)

(defcustom indiebrain-elfeed-search-tags '(critical important personal)
  "List of user-defined tags.
Used by `indiebrain-elfeed-toggle-tag'."
  :type 'list
  :group 'indiebrain-elfeed)

(defface indiebrain-elfeed-entry-critical
  '((((class color) (min-colors 88) (background light))
     :inherit elfeed-search-title-face :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :inherit elfeed-search-title-face :foreground "#ff8059")
    (t :foreground "red"))
  "Face for Elfeed entries tagged with 'critical'.")

(defface indiebrain-elfeed-entry-important
  '((((class color) (min-colors 88) (background light))
     :inherit elfeed-search-title-face :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :inherit elfeed-search-title-face :foreground "#f0ce43")
    (t :foreground "yellow"))
  "Face for Elfeed entries tagged with 'important'.")

(defface indiebrain-elfeed-entry-personal
    '((((class color) (min-colors 88) (background light))
     :inherit elfeed-search-title-face :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :inherit elfeed-search-title-face :foreground "#2fafff")
    (t :foreground "blue"))
  "Face for Elfeed entries tagged with 'personal'.")

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

;;;###autoload
(defun indiebrain-elfeed-show-archive-entry ()
  "Store a plain text copy of the current `elfeed' entry.

The destination is defined in `indiebrain-elfeed-archives-directory'
and will be created if it does not exist."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (title (replace-regexp-in-string " " "-" (elfeed-entry-title entry)))
         (elfeed-show-truncate-long-urls nil)
         (archives (file-name-as-directory indiebrain-elfeed-archives-directory))
         (file (format "%s%s.txt" archives title)))
    (unless (file-exists-p archives)
      (make-directory archives t))
    (when (derived-mode-p 'elfeed-show-mode)
      ;; Refresh to expand truncated URLs
      (elfeed-show-refresh)
      (write-file file t)
      (message "Saved buffer at %s" file))))

;;;; General commands

(defvar elfeed-show-entry)
(declare-function elfeed-search-selected "elfeed")
(declare-function elfeed-entry-link "elfeed")

;;;###autoload
(defun indiebrain-elfeed-show-eww (&optional link)
  "Browse current entry's link or optional LINK in `eww'.

Only show the readable part once the website loads.  This can
fail on poorly-designed websites."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (or link (elfeed-entry-link entry))))
    (eww link)
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(declare-function elfeed-search-untag-all-unread "elfeed")
(declare-function elfeed-search-show-entry "elfeed")

;;;###autoload
(defun indiebrain-elfeed-search-open-other-window (&optional arg)
  "Browse `elfeed' entry in the other window.
With optional prefix ARG (\\[universal-argument]) browse the
entry in `eww' using the `indiebrain-elfeed-show-eww' wrapper."
  (interactive "P")
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (elfeed-entry-link entry))
         (win (selected-window)))
    (with-current-buffer (get-buffer "*elfeed-search*")
      (unless (one-window-p)              ; experimental
        (delete-other-windows win))
      (split-window win (/ (frame-height) 5) 'below)
      (other-window 1)
      (if arg
          (progn
            (when (eq major-mode 'elfeed-search-mode)
              (elfeed-search-untag-all-unread))
            (indiebrain-elfeed-show-eww link))
        (elfeed-search-show-entry entry)))))

(declare-function elfeed-kill-buffer "elfeed")
(declare-function elfeed-search-quit-window "elfeed")

;;;###autoload
(defun indiebrain-elfeed-kill-buffer-close-window-dwim ()
  "Do-what-I-mean way to handle `elfeed' windows and buffers.

When in an entry buffer, kill the buffer and return to the Elfeed
Search view.  If the entry is in its own window, delete it as
well.

When in the search view, close all other windows.  Else just kill
the buffer."
  (interactive)
  (let ((win (selected-window)))
    (cond ((eq major-mode 'elfeed-show-mode)
           (elfeed-kill-buffer)
           (unless (one-window-p) (delete-window win))
           (switch-to-buffer "*elfeed-search*"))
          ((eq major-mode 'elfeed-search-mode)
           (if (one-window-p)
               (elfeed-search-quit-window)
             (delete-other-windows win))))))

(defvar elfeed-search-filter-active)
(defvar elfeed-search-filter)
(declare-function elfeed-db-get-all-tags "elfeed")
(declare-function elfeed-search-update "elfeed")
(declare-function elfeed-search-clear-filter "elfeed")

;;;###autoload
(defun indiebrain-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-protect
      (elfeed-search-clear-filter)
    (let* ((elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (mapcar (lambda (tag)
                                (format "+%s" tag))
                              db-tags))
           (minus-tags (mapcar (lambda (tag)
                                 (format "-%s" tag))
                               db-tags))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           (tags (completing-read-multiple
                  "Apply one or more tags: "
                  all-tags #'indiebrain-common-crm-exclude-selected-p t))
           (input (string-join `(,elfeed-search-filter ,@tags) " ")))
      (setq elfeed-search-filter input))
    (elfeed-search-update :force)))

;;;; Elfeed multimedia extras

(defvar indiebrain-elfeed-mpv-buffer-name "*indiebrain-elfeed-mpv-output*"
  "Name of buffer holding Elfeed MPV output.")

(defun indiebrain-elfeed--video-resolution ()
  "Determine display resolution.
This checks `indiebrain-elfeed-laptop-resolution-breakpoint'."
  (if (<= (display-pixel-width) indiebrain-elfeed-laptop-resolution-breakpoint)
      indiebrain-elfeed-video-resolution-small
    indiebrain-elfeed-video-resolution-large))

(defun indiebrain-elfeed--get-mpv-buffer ()
  "Prepare `indiebrain-elfeed-mpv-buffer-name' buffer."
  (let ((buf (get-buffer indiebrain-elfeed-mpv-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer))))

(declare-function elfeed-entry-enclosures "elfeed")

;;;###autoload
(defun indiebrain-elfeed-mpv-dwim ()
  "Play entry link with the external MPV program.
When there is an audio enclosure (assumed to be a podcast), play
just the audio.  Else spawn a video player at a resolution that
accounts for the current monitor's width."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (elfeed-entry-link entry))
         (enclosure (elt (car (elfeed-entry-enclosures entry)) 0)) ; fragile?
         (audio "--no-video")
         ;; Here the display width checks if I am on the laptop
         (height (indiebrain-elfeed--video-resolution))
         (video                       ; this assumes mpv+youtube-dl
          (format "--ytdl-format=bestvideo[height\\<=?%s]+bestaudio/best" height))
         (buf (pop-to-buffer indiebrain-elfeed-mpv-buffer-name)))
    (indiebrain-elfeed--get-mpv-buffer)
    (if enclosure
        (progn
          (async-shell-command (format "mpv %s %s" audio enclosure) buf)
          (message "Launching MPV for %s" enclosure))
      (async-shell-command (format "mpv %s %s" video link) buf)
      (message "Launching MPV for %s" link))))

(provide 'indiebrain-elfeed)
;;; indiebrain-elfeed.el ends here
