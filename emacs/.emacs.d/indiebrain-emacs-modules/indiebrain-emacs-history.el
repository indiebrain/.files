;;; indiebrain-emacs-history.el --- Emacs state and history configuration  -*- lexical-binding: t; -*-

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

;; Configuration for managing Emacs state and history
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Emacs server and desktop
(indiebrain-emacs-package server
  (add-hook 'after-init-hook #'server-start))

(indiebrain-emacs-package desktop
  (setq desktop-auto-save-timeout 300)
  (setq desktop-path `(,user-emacs-directory))
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save ".*")
  (setq desktop-buffers-not-to-save ".*")
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  (dolist (symbol '(kill-ring log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  (desktop-save-mode 1))

;;; Record cursor position
(indiebrain-emacs-package saveplace
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;;; Backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)


(provide 'indiebrain-emacs-history)
;;; indiebrain-emacs-history.el ends here
