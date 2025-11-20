;;; indiebrain-emacs-essentials.el --- Essential customization's  -*- lexical-binding: t; -*-

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

;; Emacs-wide and general module configuration for Emacs.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Common auxiliary functions (indiebrain-common.el)
(indiebrain-emacs-package indiebrain-common)

;;; Common custom functions (indiebrain-simple.el)
(indiebrain-emacs-package indiebrain-simple
  (setq indiebrain-simple-insert-pair-alist
        '(("' Single quote"        . (39 39))     ; ' '
          ("\" Double quotes"      . (34 34))     ; " "
          ("` Elisp quote"         . (96 39))     ; ` '
          ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
          ("“ Double apostrophes"  . (8220 8221)) ; “ ”
          ("( Parentheses"         . (40 41))     ; ( )
          ("{ Curly brackets"      . (123 125))   ; { }
          ("[ Square brackets"     . (91 93))     ; [ ]
          ("< Angled brackets"     . (60 62))     ; < >
          ("= Equals signs"        . (61 61))     ; = =
          ("~ Tilde"               . (126 126))   ; ~ ~
          ("* Asterisks"           . (42 42))     ; * *
          ("/ Forward Slash"       . (47 47))     ; / /
          ("_ underscores"         . (95 95)))    ; _ _
        indiebrain-simple-date-specifier "%F"
        indiebrain-simple-time-specifier "%R %z"
        delete-pair-blink-delay 0.15 ; Emacs28 -- see `indiebrain-simple-delete-pair-dwim'
        indiebrain-simple-scratch-buffer-default-mode 'markdown-mode
        help-window-select t
        next-error-recenter '(4)) ; center of the window
  ;; General commands
  (indiebrain-emacs-keybind global-map
    "<insert>" nil
    "C-z" nil
    "C-x C-z" nil
    "C-h h" nil
    "M-`" nil
    "C-h ." #'indiebrain-simple-describe-symbol ; overrides `display-local-help'
    "C-h K" #'describe-keymap ; overrides `Info-goto-emacs-key-command-node'
    "C-h c" #'describe-char ; overrides `describe-key-briefly'
    "C-c s" #'indiebrain-simple-scratch-buffer

    ;; Commands for lines
    "M-o" #'delete-blank-lines   ; alias for C-x C-o
    "M-k" #'indiebrain-simple-kill-line-backward
    "C-S-w" #'indiebrain-simple-copy-line-or-region
    "C-S-y" #'indiebrain-simple-yank-replace-line-or-region
    "M-SPC" #'cycle-spacing
    "C-S-n" #'indiebrain-simple-multi-line-next
    "C-S-p" #'indiebrain-simple-multi-line-prev
    "<C-return>" #'indiebrain-simple-new-line-below
    "<C-S-return>" #'indiebrain-simple-new-line-above

    ;; Commands for text insertion or manipulation
    "C-=" #'indiebrain-simple-insert-date
    "C-<" #'indiebrain-simple-escape-url
    "C-'" #'indiebrain-simple-insert-pair
    "M-'" #'indiebrain-simple-insert-pair
    "M-\\" #'indiebrain-simple-delete-pair-dwim
    "<C-M-backspace>" #'backward-kill-sexp
    "M-c" #'capitalize-dwim
    "M-l" #'downcase-dwim        ; "lower" case
    "M-u" #'upcase-dwim

    ;; Commands for object transposition
    "C-t" #'indiebrain-simple-transpose-chars
    "C-x C-t" #'indiebrain-simple-transpose-lines
    "C-S-t" #'indiebrain-simple-transpose-paragraphs
    "C-x M-t" #'indiebrain-simple-transpose-sentences
    "C-M-t" #'indiebrain-simple-transpose-sexps
    "M-t" #'indiebrain-simple-transpose-words

    ;; Commands for marking objects
    "M-@" #'indiebrain-simple-mark-word       ; replaces `mark-word'
    "C-M-SPC" #'indiebrain-simple-mark-construct-dwim
    "C-M-d" #'indiebrain-simple-downward-list

    ;; Commands for paragraphs
    "M-Q" #'indiebrain-simple-unfill-region-or-paragraph

    ;; Commands for windows and pages
    "C-x n k" #'indiebrain-simple-delete-page-delimiters
    "C-x M" #'indiebrain-simple-monocle
    "C-x M-r" #'indiebrain-simple-swap-window-buffers

    ;; Commands for windows and pages
    "C-x n k" #'indiebrain-simple-delete-page-delimiters
    "C-x M" #'indiebrain-simple-monocle
    "C-x M-r" #'indiebrain-simple-swap-window-buffers))

;;; Keymap for buffers (Emacs28)
(indiebrain-emacs-keybind ctl-x-x-map
  "e" #'eval-buffer
  "f" #'follow-mode  ; override `font-lock-update'
  "r" #'rename-uniquely)

;;; Mouse wheel behavior
(indiebrain-emacs-package mouse
  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale))
        mouse-drag-copy-region nil
        make-pointer-invisible t
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)
  (add-hook 'after-init-hook #'mouse-wheel-mode)
  (indiebrain-emacs-keybind global-map
    "C-M-<mouse-3>" #'tear-off-window))

;;; Scrolling behavior
;; These four come from the C source code.
(setq-default scroll-preserve-screen-position t
              scroll-conservatively 1 ; affects `scroll-step'
              scroll-margin 0
              next-screen-context-lines 0)

;;; Delete selection
(indiebrain-emacs-package delsel
  (delete-selection-mode 1))

;;; Tooltips (tooltip-mode)
(indiebrain-emacs-package tooltip
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips nil
        tooltip-frame-parameters
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

;;; Cleanup superfluous whitespace when a buffer is saved
(add-hook 'before-save-hook #'whitespace-cleanup)

;;; Go to last change
(indiebrain-emacs-package goto-last-change
  (:install t)
  (indiebrain-emacs-keybind global-map
    "C-z" #'goto-last-change))

;;; Repeatable key chords (repeat-mode)
(indiebrain-emacs-package repeat
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t)
  (add-hook 'after-init-hook #'repeat-mode))

;;; Make Custom UI code disposable
(indiebrain-emacs-package cus-edit
  ;; Disable the damn thing
  (setq custom-file (make-temp-file "emacs-custom-")))

;;; Substitute - search/replace enhancements
(indiebrain-emacs-package substitute
  (:install t)
  (setq substitute-highlight t)
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)

  (indiebrain-emacs-keybind global-map
    "M-# s" #'substitute-target-below-point
    "M-# r" #'substitute-target-above-point
    "M-# d" #'substitute-target-in-defun
    "M-# b" #'substitute-target-in-buffer))

(indiebrain-emacs-package super-save
  (:install t)
  (super-save-mode 1))

;;; Shell (M-x shell)
(indiebrain-emacs-package shell
  (:install t)
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t) ; Emacs 29.1
  (setq shell-has-auto-cd nil) ; Emacs 29.1
  (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input)
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)

  (indiebrain-emacs-keybind global-map
    "<f1>" #'shell) ; I don't use F1 for help commands

  (indiebrain-emacs-keybind shell-mode-map
    "<up>" #'comint-previous-input
    "<down>" #'comint-next-input
    "C-c C-k" #'comint-clear-buffer
    "C-c C-w" #'comint-write-output))

(indiebrain-emacs-configure
  (setq auth-sources '("~/.authinfo.gpg")))

(provide 'indiebrain-emacs-essentials)
;;; indiebrain-emacs-essentials.el ends here
