;;; indiebrain-emacs-window.el --- Configuration for window and buffer management  -*- lexical-binding: t; -*-

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

;; Configure Emacs windows and buffers.
;;
;;

;;; Code:

;;; Unique names for buffers
(indiebrain-emacs-builtin-package 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; Window rules and basic tweaks (window.el)
(indiebrain-emacs-builtin-package 'window
  (setq display-buffer-alist
    `(;; no window
      ("\\`\\*Async Shell Command\\*\\'"
       (display-buffer-no-window))
      ;; bottom side window
      ("\\*Org Select\\*" ; the `org-capture' key selection
       (display-buffer-in-side-window)
       (dedicated . t)
       (side . bottom)
       (slot . 0)
       (window-parameters . ((mode-line-format . none))))
      ;; bottom buffer (NOT side window)
      ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
          (derived-mode . flymake-project-diagnostics-mode)
          (derived-mode . messages-buffer-mode)
          (derived-mode . backtrace-mode)
          "\\*\\(Warnings\\|Compile-Log\\)\\*"
          "\\*world-clock.*"))
       (display-buffer-reuse-mode-window display-buffer-at-bottom)
       (window-height . 0.3)
       (dedicated . t)
       (preserve-size . (t . t)))
      ("\\*Embark Actions\\*"
       (display-buffer-reuse-mode-window display-buffer-at-bottom)
       (window-height . fit-window-to-buffer)
       (window-parameters . ((no-other-window . t)
                 (mode-line-format . none))))
      ("\\*\\(Output\\|Register Preview\\).*"
       (display-buffer-reuse-mode-window display-buffer-at-bottom))
      ;; below current window
      ((derived-mode . help-mode) ; See the hooks for `visual-line-mode'
       (display-buffer-reuse-mode-window display-buffer-below-selected))
      ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
       (display-buffer-reuse-mode-window display-buffer-below-selected)
       (window-height . 0.1)
       (dedicated . t)
       (preserve-size . (t . t)))
      ((derived-mode . log-view-mode)
       (display-buffer-reuse-mode-window display-buffer-below-selected)
       (window-height . 0.3)
       (preserve-size . (t . t)))
      ((derived-mode . reb-mode) ; M-x re-builder
       (display-buffer-reuse-mode-window display-buffer-below-selected)
       (window-height . 4) ; note this is literal lines, not relative
       (dedicated . t)
       (preserve-size . (t . t)))
      ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
       (display-buffer-reuse-mode-window display-buffer-below-selected)
       (window-height . fit-window-to-buffer))
      ;; NOTE 2022-09-10: The following is for `ispell-word', though
      ;; it only works because I override `ispell-display-buffer'
      ;; with `indiebrain-spell-ispell-display-buffer' and change the
      ;; value of `ispell-choices-buffer'.  Check my indiebrain-spell.el
      ;; for the details.
      ("\\*ispell-top-choices\\*"
       (display-buffer-reuse-mode-window display-buffer-below-selected)
       (window-height . fit-window-to-buffer))
      ;; new frame
      ((or . ((derived-mode . Man-mode)
          (derived-mode . woman-mode)
          "\\*\\(Man\\|woman\\).*"))
       (display-buffer-reuse-window display-buffer-pop-up-frame)
       (pop-up-frame-parameters . ((width . (text-pixels . 640))
                       (height . (text-pixels . 360)))))
      ;; same window
      (indiebrain/display-buffer-shell-or-term-p ; see definition below
       (display-buffer-reuse-window display-buffer-same-window))))

  (defun indiebrain/display-buffer-shell-or-term-p (buffer &rest _)
    "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
    (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name (get-buffer buffer)))
      (with-current-buffer buffer
    ;; REVIEW 2022-07-14: Is this robust?
    (and (not (derived-mode-p 'message-mode 'text-mode))
         (derived-mode-p 'eshell-mode 'shell-mode 'comint-mode 'fundamental-mode)))))

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)

  (add-hook 'help-mode-hook #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode)

  (indiebrain-emacs-keybind global-map
    "C-x <down>" #'next-buffer
    "C-x <up>" #'previous-buffer
    "C-x C-n" #'next-buffer     ; override `set-goal-column'
    "C-x C-p" #'previous-buffer ; override `mark-page'
    "C-x !" #'delete-other-windows-vertically
    "C-x _" #'balance-windows      ; underscore
    "C-x -" #'fit-window-to-buffer ; hyphen
    "C-x +" #'balance-windows-area
    "C-x }" #'enlarge-window
    "C-x {" #'shrink-window
    "C-x >" #'enlarge-window-horizontally ; override `scroll-right'
    "C-x <" #'shrink-window-horizontally) ; override `scroll-left'
  (indiebrain-emacs-keybind resize-window-repeat-map
    ">" #'enlarge-window-horizontally
    "<" #'shrink-window-horizontally))

;;; Directional window motions (windmove)
(indiebrain-emacs-builtin-package 'windmove
  (setq windmove-create-window nil)     ; Emacs 27.1
  (indiebrain-emacs-keybind global-map
    ;; Those override some commands that are already available with
    ;; C-M-u, C-M-f, C-M-b.
    "C-M-<up>" #'windmove-up
    "C-M-<right>" #'windmove-right
    "C-M-<down>" #'windmove-down
    "C-M-<left>" #'windmove-left
    "C-M-S-<up>" #'windmove-swap-states-up
    "C-M-S-<right>" #'windmove-swap-states-right ; conflicts with `org-increase-number-at-point'
    "C-M-S-<down>" #'windmove-swap-states-down
    "C-M-S-<left>" #'windmove-swap-states-left))

(provide 'indiebrain-emacs-window)
;;; indiebrain-emacs-window.el ends here
