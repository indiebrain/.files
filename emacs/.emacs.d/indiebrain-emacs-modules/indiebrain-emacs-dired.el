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
(indiebrain-emacs-builtin-package 'dired
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
  (define-key dired-jump-map (kbd "j") nil))

(indiebrain-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'nil)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action)))

(indiebrain-emacs-builtin-package 'dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

(indiebrain-emacs-builtin-package 'indiebrain-dired
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

  (let ((map dired-mode-map))
    (define-key map (kbd "i") #'indiebrain-dired-insert-subdir) ; override `dired-maybe-insert-subdir'
    (define-key map (kbd "/") #'indiebrain-dired-limit-regexp)
    (define-key map (kbd "C-c C-l") #'indiebrain-dired-limit-regexp)
    (define-key map (kbd "M-n") #'indiebrain-dired-subdirectory-next)
    (define-key map (kbd "C-c C-n") #'indiebrain-dired-subdirectory-next)
    (define-key map (kbd "M-p") #'indiebrain-dired-subdirectory-previous)
    (define-key map (kbd "C-c C-p") #'indiebrain-dired-subdirectory-previous)
    (define-key map (kbd "M-s G") #'indiebrain-dired-grep-marked-files))) ; M-s g is `indiebrain-search-grep'

(indiebrain-emacs-elpa-package 'dired-subtree
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))) ; S-TAB

(indiebrain-emacs-builtin-package 'wdired
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(indiebrain-emacs-builtin-package 'image-dired
  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
    (kbd "<return>") #'image-dired-thumbnail-display-external))

;;; dired-like mode for the trash (trashed.el)
(indiebrain-emacs-elpa-package 'trashed
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Ibuffer (dired-like buffer list manager)
(indiebrain-emacs-builtin-package 'ibuffer
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
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  (let ((map ibuffer-mode-map))
    (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
    (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
    (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
    (define-key map (kbd "/ g") #'ibuffer-filter-by-content)))

(provide 'indiebrain-emacs-dired)
;;; indiebrain-emacs-dired.el ends here
