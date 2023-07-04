;;; indiebrain-emacs-theme-extras.el --- common theme and user interface extensions  -*- lexical-binding: t; -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Common theme and user interface extensions. These features enhance
;; the user interface and are independent of the chosen theme.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Pylsar - draw attention to the "current" line
;;
;; Read the manual: <https://protesilaos.com/emacs/pulsar>
(indiebrain-emacs-package pulsar
  (:install t)

  (dolist (cmd '( narrow-to-page narrow-to-defun
                  narrow-to-region widen
                  logos-forward-page-dwim
                  logos-backward-page-dwim))
    (add-to-list 'pulsar-pulse-functions cmd))

  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-highlight-face 'pulsar-red)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)

  ;; pulsar does not define any key bindings. This is just my personal
  ;; preference. Remember to read the manual on the matter. Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (indiebrain-emacs-keybind global-map
    "C-x l" #'pulsar-pulse-line
    "C-x L" #'pulsar-highlight-dwim))

;;; LIN - enhance hl-line-mode in selection UIs
;;
;; Read the manual: <https://protesilaos.com/emacs/lin>.
(indiebrain-emacs-package lin
  (:install t)
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  (setq lin-face 'hl-line)
  (setq lin-mode-hooks
        '(dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1)) ; applies to all `lin-mode-hooks'

;;; Rainbow mode for colour previewing (rainbow-mode.el)
(indiebrain-emacs-package rainbow-mode
  (:install t)
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun indiebrain/rainbow-mode-in-themes ()
    (when-let* ((file (buffer-file-name))
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))

  (add-hook 'emacs-lisp-mode-hook #'indiebrain/rainbow-mode-in-themes))

;;; Line numbers and relevant indicators (indiebrain-sideline.el)
(indiebrain-emacs-package indiebrain-sideline
  (require 'display-line-numbers)
  ;; Set absolute line numbers. A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t)

  (indiebrain-emacs-package diff-hl
  (:install t)
    (setq diff-hl-draw-borders nil)
    (setq diff-hl-side 'left))

  (require 'hl-line)
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50) ; emacs28

  (require 'whitespace)

  (add-hook 'prog-mode-hook #'indiebrain-sideline-mode)

  (indiebrain-emacs-keybind global-map
    "<f6>" #'indiebrain-sideline-negative-space-toggle
    "<f7>" #'indiebrain-sideline-mode
    "C-c z" #'delete-trailing-whitespace))

;;; Fringe mode
(indiebrain-emacs-package fringe
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(indiebrain-emacs-package cursory
  (:install t)
  (setq cursory-presets
        '((box
           :blink-cursor-interval 0.8)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.5)
          (underscore
           :cursor-type (hbar . 2)
           :blink-cursor-blinks 50)
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'bar))

  ;; The other side of `cursory-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  (indiebrain-emacs-keybind global-map
    "C-c p" #'cursory-set-preset))

(provide 'indiebrain-emacs-theme-extras)
;;; indiebrain-emacs-theme-extras.el ends here
