;;; early-init.el --- Emacs Early Initialization     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Aaron

;; Author: Aaron <aaron.kuehler@gmail.com>
;; Keywords: internal
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

;; This configuration is loaded before Emacs loads the user's
;; initialization file - 'init.el'. Items commonly found here are the
;; default presentation settings, initialization of Emacs' package,
;; configuration for Emacs' native compilation system, etc.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(setq frame-resize-pixelwise t)

;; Prevent frame resizing during initialization
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set default frame size
(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1200)))
  (add-to-list var '(height . (text-pixels . 900))))

;; Disable default "help"-ful messages
(setq inhibit-splash-screen t)
(setq use-dialog-box t) ; only for mouse events
(setq use-file-dialog nil)
(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)


;; Initialize installed packages
(setq package-enable-at-startup t)

;; Allow package loading from the package cache
(defvar package-quickstart)
(setq package-quickstart t)

(setq native-comp-async-report-warnings-errors 'silent)
;;; early-init.el ends here
