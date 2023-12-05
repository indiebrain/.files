;;; indiebrain-emacs-conveniences.el --- Conveneinces for my Emacs configuration  -*- lexical-binding: t; -*-

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

;; Coneveniences for my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(indiebrain-emacs-keybind ctl-x-x-map
  "e" #'eval-buffer
  "f" #'follow-mode  ; override `font-lock-update'
  "r" #'rename-uniquely)

(with-eval-after-load 'org
  "i" #'indiebrain-org-id-headlines
  "h" #'indiebrain-org-ox-html)

;;; Mouse wheel behaviour
(indiebrain-emacs-package mouse
  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
  (setq mouse-drag-copy-region nil)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  (add-hook 'after-init-hook #'mouse-wheel-mode)
  (indiebrain-emacs-keybind global-map
    "C-M-<mouse-3>" #'tear-off-window))

;;; Scrolling behaviour
;; These four come from the C source code.
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1) ; affects `scroll-step'
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)

;;; Tooltips (tooltip-mode)
(indiebrain-emacs-package tooltip
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  (add-hook 'after-init-hook #'tooltip-mode))

;;; Auto revert mode
(indiebrain-emacs-package autorevert
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;;; Newline characters for file ending
(setq mode-require-final-newline 'visit-save)

;;; Go to last change
(indiebrain-emacs-package goto-last-change
  (:install t)
  (indiebrain-emacs-keybind global-map
    "C-z" #'goto-last-change))

;;; Repeatable key chords (repeat-mode)
(indiebrain-emacs-package repeat
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)

  (repeat-mode 1))

;;; Make Custom UI code disposable
(indiebrain-emacs-package cus-edit
                            (setq custom-file (make-temp-file "emacs-custom-")))

(provide 'indiebrain-emacs-conveniences)
;;; indiebrain-emacs-conveniences.el ends here
