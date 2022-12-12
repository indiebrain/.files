;;; indiebrain-emacs-shell.el ---Configure Emacs shells  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Aaron

;; Author: Aaron <max@maximilian-m1.local>
;; Keywords:

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

;; Configuration for Emacs shells and other system interfaces
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Eshell and indiebrain-eshell.el
(indiebrain-emacs-builtin-package 'eshell
  (require 'esh-mode)
  (require 'esh-module)
  (setq eshell-modules-list             ; It works but may need review
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix))
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (require 'em-cmpl)
  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (require 'em-tramp)
  (setq password-cache t)
  (setq password-cache-expiry 600)

  (require 'em-hist)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

(indiebrain-emacs-builtin-package 'indiebrain-eshell
  (setq indiebrain-eshell-output-buffer "*Exported Eshell output*")
  (setq indiebrain-eshell-output-delimiter "* * *")
  (let ((map eshell-mode-map))
    (define-key map (kbd "M-k") #'eshell-kill-input)
    (define-key map (kbd "C-c C-f") #'indiebrain-eshell-ffap-find-file)
    (define-key map (kbd "C-c C-j") #'indiebrain-eshell-ffap-dired-jump)
    (define-key map (kbd "C-c C-w") #'indiebrain-eshell-ffap-kill-save)
    (define-key map (kbd "C-c C->") #'indiebrain-eshell-redirect-to-buffer)
    (define-key map (kbd "C-c C-e") #'indiebrain-eshell-export)
    (define-key map (kbd "C-c C-r") #'indiebrain-eshell-root-dir))
  (let ((map eshell-cmpl-mode-map))
    (define-key map (kbd "C-c TAB") #'indiebrain-eshell-ffap-insert) ; C-c C-i
    (define-key map (kbd "C-c C-h") #'indiebrain-eshell-narrow-output-highlight-regexp))
  (let ((map eshell-hist-mode-map))
    (define-key map (kbd "M-s") #'nil) ; I use this prefix for lots of more useful commands
    (define-key map (kbd "M-r") #'indiebrain-eshell-complete-history)
    (define-key map (kbd "C-c C-d") #'indiebrain-eshell-complete-recent-dir)
    (define-key map (kbd "C-c C-s") #'indiebrain-eshell-find-subdirectory-recursive)))

;;; Shell (M-x shell)
(indiebrain-emacs-builtin-package 'shell
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t))

;;; Tools for manual pages (manpages)
(indiebrain-emacs-builtin-package 'man
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

;;; Proced (process monitor, similar to `top')
(indiebrain-emacs-builtin-package 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

(indiebrain-emacs-builtin-package 'indiebrain-proced
  (add-hook 'proced-mode-hook #'indiebrain-proced-extra-keywords-mode))

;;; Pass interface (password-store)
(indiebrain-emacs-elpa-package 'password-store
  (setq password-store-time-before-clipboard-restore 30)
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  (define-key global-map (kbd "C-c k") #'password-store-copy))

(indiebrain-emacs-elpa-package 'pass)

(provide 'indiebrain-emacs-shell)
;;; indiebrain-emacs-shell.el ends here
