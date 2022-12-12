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
    (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name buffer))
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

  (let ((map global-map))
    (define-key map (kbd "C-x <down>") #'next-buffer)
    (define-key map (kbd "C-x <up>") #'previous-buffer)
    (define-key map (kbd "C-x C-n") #'next-buffer)     ; override `set-goal-column'
    (define-key map (kbd "C-x C-p") #'previous-buffer) ; override `mark-page'
    (define-key map (kbd "C-x !") #'delete-other-windows-vertically)
    (define-key map (kbd "C-x _") #'balance-windows)      ; underscore
    (define-key map (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
    (define-key map (kbd "C-x +") #'balance-windows-area)
    (define-key map (kbd "C-x }") #'enlarge-window)
    (define-key map (kbd "C-x {") #'shrink-window)
    (define-key map (kbd "C-x >") #'enlarge-window-horizontally) ; override `scroll-right'
    (define-key map (kbd "C-x <") #'shrink-window-horizontally)) ; override `scroll-left'
  (let ((map resize-window-repeat-map))
    (define-key map ">" #'enlarge-window-horizontally)
    (define-key map "<" #'shrink-window-horizontally)))

;;; Directional window motions (windmove)
(indiebrain-emacs-builtin-package 'windmove
  (setq windmove-create-window nil)     ; Emacs 27.1
  (let ((map global-map))
    ;; Those override some commands that are already available with
    ;; C-M-u, C-M-f, C-M-b.
    (define-key map (kbd "C-M-<up>") #'windmove-up)
    (define-key map (kbd "C-M-<right>") #'windmove-right)
    (define-key map (kbd "C-M-<down>") #'windmove-down)
    (define-key map (kbd "C-M-<left>") #'windmove-left)
    (define-key map (kbd "C-M-S-<up>") #'windmove-swap-states-up)
    (define-key map (kbd "C-M-S-<right>") #'windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
    (define-key map (kbd "C-M-S-<down>") #'windmove-swap-states-down)
    (define-key map (kbd "C-M-S-<left>") #'windmove-swap-states-left)))

;;; Tabs for window layouts (tab-bar.el and indiebrain-tab.el)
(indiebrain-emacs-builtin-package 'tab-bar
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
  (setq tab-bar-auto-width-max '(120 . 20)) ; Emacs 29

  (tab-bar-mode -1)                     ; see `indiebrain-tab-status-line'

  ;; Same concept as `winner-mode'.  See the `indiebrain-tab-winner-undo' and
  ;; its counterpart.
  (tab-bar-history-mode 1))

(indiebrain-emacs-builtin-package 'indiebrain-tab
  (setq indiebrain-tab-tab-select-num-threshold 3)
  (setq tab-bar-format                    ; Emacs 28
        '(tab-bar-format-tabs-groups
          tab-bar-format-align-right
          tab-bar-format-global))

  (add-hook 'after-init-hook #'indiebrain-tab-status-line)

  (let ((map global-map))
    (define-key map (kbd "C-x <right>") #'indiebrain-tab-winner-redo)
    (define-key map (kbd "C-x <left>") #'indiebrain-tab-winner-undo)
    (define-key map (kbd "<f8>") #'indiebrain-tab-status-line) ; unopinionated alternative: `indiebrain-tab-bar-toggle'
    (define-key map (kbd "C-x t t") #'indiebrain-tab-select-tab-dwim)))

(provide 'indiebrain-emacs-window)
;;; indiebrain-emacs-window.el ends here
