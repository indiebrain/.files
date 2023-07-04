;;; indiebrain-emacs-dired.el ---Configuration for filesystem navigation and buffer management  -*- lexical-binding: t; -*-

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

;; Configuration for Emacs filesystem navigation and buffer management.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Dired file manager and indiebrain-dired.el extras
(indiebrain-emacs-package dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  ;; These arguments only work on GNU/Linux. As I use MacOS / Darwin for
  ;; work, I use a subset of arguments which work across multiple OS'es.
  ;;
  ;; (setq dired-listing-switches
  ;;       "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-listing-switches
        "-AGFhlv")
  (setq dired-use-ls-dired nil)
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; In Emacs 29 there is a binding for `repeat-mode' which let you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
  (indiebrain-emacs-keybind dired-jump-map "j" nil))

(indiebrain-emacs-package dired-aux
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (indiebrain-emacs-keybind dired-mode-map
    "C-+" #'dired-create-empty-file
    "M-s f" #'nil
    "C-x v v" #'dired-vc-next-action))

(indiebrain-emacs-package dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (indiebrain-emacs-keybind dired-mode-map "I" #'dired-info))

(indiebrain-emacs-package indiebrain-dired
  (setq indiebrain-dired-image-viewers '("feh" "sxiv"))
  (setq indiebrain-dired-media-players '("mpv" "vlc"))
  (setq indiebrain-dired-media-extensions
        "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)")
  (setq indiebrain-dired-image-extensions
        "\\.\\(png\\|jpe?g\\|tiff\\)")
  (setq dired-guess-shell-alist-user ; those are the defaults for ! and & in Dired
        `((,indiebrain-dired-image-extensions (indiebrain-dired-image-viewer))
          (,indiebrain-dired-media-extensions (indiebrain-dired-media-player))))

  (add-hook 'dired-mode-hook #'indiebrain-dired-setup-imenu)

  (indiebrain-emacs-keybind dired-mode-map
    "i" #'indiebrain-dired-insert-subdir ; override `dired-maybe-insert-subdir'
    "/" #'indiebrain-dired-limit-regexp
    "C-c C-l" #'indiebrain-dired-limit-regexp
    "M-n" #'indiebrain-dired-subdirectory-next
    "C-c C-n" #'indiebrain-dired-subdirectory-next
    "M-p" #'indiebrain-dired-subdirectory-previous
    "C-c C-p" #'indiebrain-dired-subdirectory-previous
    "M-s G" #'indiebrain-dired-grep-marked-files)) ; M-s g is `indiebrain-search-grep'

(indiebrain-emacs-package dired-subtree
  (:install t)
  (setq dired-subtree-use-backgrounds nil)
  (indiebrain-emacs-keybind dired-mode-map
    "<tab>" #'dired-subtree-toggle
    "<backtab>" #'dired-subtree-remove)) ; S-TAB

(indiebrain-emacs-package wdired
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(indiebrain-emacs-package image-dired
  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (indiebrain-emacs-keybind image-dired-thumbnail-mode-map
    "<return>" #'image-dired-thumbnail-display-external))

;;; dired-like mode for the trash (trashed.el)
(indiebrain-emacs-package trashed
  (:install t)
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Ibuffer (dired-like buffer list manager)
(indiebrain-emacs-package ibuffer
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 40 40 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (indiebrain-emacs-keybind global-map "C-x C-b" #'ibuffer)
  (indiebrain-emacs-keybind ibuffer-mode-map
    "* f" #'ibuffer-mark-by-file-name-regexp
    "* g" #'ibuffer-mark-by-content-regexp ; "g" is for "grep"
    "* n" #'ibuffer-mark-by-name-regexp
    "s n" #'ibuffer-do-sort-by-alphabetic  ; "sort name" mnemonic
    "/ g" #'ibuffer-filter-by-content))

(provide 'indiebrain-emacs-dired)
;;; indiebrain-emacs-dired.el ends here
