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

;;; Read environment variables
(indiebrain-emacs-elpa-package 'exec-path-from-shell
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

;;; Common auxiliary functions (indiebrain-common.el)
(indiebrain-emacs-builtin-package 'indiebrain-common)

;;; Common custom functions (indiebrain-simple.el)
(indiebrain-emacs-builtin-package 'indiebrain-simple
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
  (let ((map global-map))
    (define-key map (kbd "<insert>") nil)
    (define-key map (kbd "C-z") nil)
    (define-key map (kbd "C-x C-z") nil)
    (define-key map (kbd "C-h h") nil)
    (define-key map (kbd "M-`") nil)
    (define-key map (kbd "C-h .") #'indiebrain-simple-describe-symbol) ; overrides `display-local-help'
    (define-key map (kbd "C-h K") #'describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    (define-key map (kbd "C-h c") #'describe-char) ; overrides `describe-key-briefly'
    (define-key map (kbd "C-c s") #'indiebrain-simple-scratch-buffer)

    ;; Commands for lines
    (define-key map (kbd "M-o") #'delete-blank-lines)   ; alias for C-x C-o
    (define-key map (kbd "M-k") #'indiebrain-simple-kill-line-backward)
    (define-key map (kbd "C-S-w") #'indiebrain-simple-copy-line-or-region)
    (define-key map (kbd "C-S-y") #'indiebrain-simple-yank-replace-line-or-region)
    (define-key map (kbd "M-SPC") #'cycle-spacing)
    (define-key map (kbd "C-S-n") #'indiebrain-simple-multi-line-next)
    (define-key map (kbd "C-S-p") #'indiebrain-simple-multi-line-prev)
    (define-key map (kbd "<C-return>") #'indiebrain-simple-new-line-below)
    (define-key map (kbd "<C-S-return>") #'indiebrain-simple-new-line-above)

    ;; Commands for text insertion or manipulation
    (define-key map (kbd "C-=") #'indiebrain-simple-insert-date)
    (define-key map (kbd "C-<") #'indiebrain-simple-escape-url)
    (define-key map (kbd "C-'") #'indiebrain-simple-insert-pair)
    (define-key map (kbd "M-'") #'indiebrain-simple-insert-pair)
    (define-key map (kbd "M-\\") #'indiebrain-simple-delete-pair-dwim)
    (define-key map (kbd "<C-M-backspace>") #'backward-kill-sexp)
    (define-key map (kbd "M-c") #'capitalize-dwim)
    (define-key map (kbd "M-l") #'downcase-dwim)        ; "lower" case
    (define-key map (kbd "M-u") #'upcase-dwim)

    ;; Commands for object transposition
    (define-key map (kbd "C-t") #'indiebrain-simple-transpose-chars)
    (define-key map (kbd "C-x C-t") #'indiebrain-simple-transpose-lines)
    (define-key map (kbd "C-S-t") #'indiebrain-simple-transpose-paragraphs)
    (define-key map (kbd "C-x M-t") #'indiebrain-simple-transpose-sentences)
    (define-key map (kbd "C-M-t") #'indiebrain-simple-transpose-sexps)
    (define-key map (kbd "M-t") #'indiebrain-simple-transpose-words)

    ;; Commands for marking objects
    (define-key map (kbd "M-@") #'indiebrain-simple-mark-word)       ; replaces `mark-word'
    (define-key map (kbd "C-M-SPC") #'indiebrain-simple-mark-construct-dwim)
    (define-key map (kbd "C-M-d") #'indiebrain-simple-downward-list)

    ;; Commands for paragraphs
    (define-key map (kbd "M-Q") #'indiebrain-simple-unfill-region-or-paragraph)

    ;; Commands for windows and pages
    (define-key map (kbd "C-x n k") #'indiebrain-simple-delete-page-delimiters)
    (define-key map (kbd "C-x M") #'indiebrain-simple-monocle)
    (define-key map (kbd "C-x M-r") #'indiebrain-simple-swap-window-buffers)

    ;; Commands for windows and pages
    (define-key map (kbd "C-x n k") #'indiebrain-simple-delete-page-delimiters)
    (define-key map (kbd "C-x M") #'indiebrain-simple-monocle)
    (define-key map (kbd "C-x M-r") #'indiebrain-simple-swap-window-buffers)))

;;; Keymap for buffers (Emacs28)
(let ((map ctl-x-x-map))
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely))

;;; Mouse wheel behavior
(indiebrain-emacs-builtin-package 'mouse
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
  (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window))

;;; Scrolling behavior
;; These four come from the C source code.
(setq-default scroll-preserve-screen-position t
              scroll-conservatively 1 ; affects `scroll-step'
              scroll-margin 0
              next-screen-context-lines 0)

;;; Delete selection
(indiebrain-emacs-builtin-package 'delsel
  (delete-selection-mode 1))

;;; Tooltips (tooltip-mode)
(indiebrain-emacs-builtin-package 'tooltip
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
(indiebrain-emacs-builtin-package 'autorevert
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;;; Newline characters for file ending
(setq mode-require-final-newline 'visit-save)

;;; Cleanup superfluous whitespace when a buffer is saved
(add-hook 'before-save-hook #'whitespace-cleanup)

;;; Go to last change
(indiebrain-emacs-elpa-package 'goto-last-change
  (define-key global-map (kbd "C-z") #'goto-last-change))

;;; Repeatable key chords (repeat-mode)
(indiebrain-emacs-builtin-package 'repeat
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

;;; Emoji input
(indiebrain-emacs-builtin-package 'emoji
  (defun indiebrain/emoji-insert (&optional transient)
    "Thin wrapper for `emoji-insert' and `emoji-search'.
When called with optional TRANSIENT as a prefix argument, use the
transient interface (transient.el), else pick an emoji with
minibuffer completion."
    (interactive "P")
    (let ((cmd (if transient 'emoji-insert 'emoji-search)))
      (call-interactively cmd)))

  ;; The default key bindings for Emoji are behind the C-x 8 e prefix.
  ;; Meanwhile, F2 does something useless in my workflow.
  ;; (define-key global-map (kbd "<f2>") #'indiebrain/emoji-insert)
  (define-key global-map (kbd "<f2>") #'indiebrain/emoji-insert))

;;; Make Custom UI code disposable
(indiebrain-emacs-builtin-package 'cus-edit
  ;; Disable the damn thing
  (setq custom-file (make-temp-file "emacs-custom-")))

;;; TMR May Ring (tmr is used to set timers)
(indiebrain-emacs-elpa-package 'tmr
  (setq tmr-notification-urgency 'normal
        tmr-description-list 'tmr-description-history)

  ;; You do not need these if you install the package.
  (require 'tmr-notification)
  (require 'tmr-tabulated)

  (let ((map global-map))
    (define-key map (kbd "C-c t t") #'tmr)
    (define-key map (kbd "C-c t T") #'tmr-with-description)
    (define-key map (kbd "C-c t l") #'tmr-tabulated-view) ; "list timers" mnemonic
    (define-key map (kbd "C-c t c") #'tmr-clone)
    (define-key map (kbd "C-c t k") #'tmr-cancel)
    (define-key map (kbd "C-c t s") #'tmr-reschedule)
    (define-key map (kbd "C-c t e") #'tmr-edit-description)
    (define-key map (kbd "C-c t r") #'tmr-remove)
    (define-key map (kbd "C-c t R") #'tmr-remove-finished)))

;;; Substitute - search/replace enhancements
(indiebrain-emacs-elpa-package 'substitute
  (setq substitute-highlight t)
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)

  (let ((map global-map))
    (define-key map (kbd "M-# s") #'substitute-target-below-point)
    (define-key map (kbd "M-# r") #'substitute-target-above-point)
    (define-key map (kbd "M-# d") #'substitute-target-in-defun)
    (define-key map (kbd "M-# b") #'substitute-target-in-buffer)))

(indiebrain-emacs-elpa-package 'super-save
  (super-save-mode 1))

(provide 'indiebrain-emacs-essentials)
;;; indiebrain-emacs-essentials.el ends here
