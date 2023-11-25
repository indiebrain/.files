;;; indiebrain-emacs-completion.el --- narrowing and completion configuration  -*- lexical-binding: t; -*-

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

;; Configuration for Emacs completion and narrowing.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Orderless completion style (and indiebrain-orderless.el)
(indiebrain-emacs-package orderless
  (:install t)
  (setq orderless-component-separator " +")
  ;; NOTE 2022-02-06: I made some major changes and this list may need
  ;; to be refined further. Remember to check my `completion-styles'
  ;; and the `completion-category-overrides'.
  (setq orderless-matching-styles
        '(orderless-prefixes
          orderless-strict-leading-initialism
          orderless-flex
          orderless-regexp))
  (setq orderless-style-dispatchers
        '(indiebrain-orderless-literal-dispatcher
          indiebrain-orderless-flex-dispatcher))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  (indiebrain-emacs-keybind minibuffer-local-completion-map
    "SPC" nil
    "?" nil))

(indiebrain-emacs-package indiebrain-orderless)

;;; Completion annotations (marginalia)
(indiebrain-emacs-package marginalia
  (:install t)
  (setq marginalia-max-relative-age 0)  ; time is absolute here!
  (marginalia-mode 1))

;;; Minibuffer configurations and Vertico
(indiebrain-emacs-package minibuffer
  (setq completion-styles '(basic orderless)) ; also see `completion-category-overrides'
  (setq completion-category-defaults nil)

  ;; A list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;;
  ;; From the `consult' package:
  ;;
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;;
  ;; From the `embark' package:
  ;;
  ;; - `embark-keybinding'
  ;;
  (setq completion-category-overrides
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order. In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (orderless basic partial-completion)))
          (project-file (styles . (basic substring partial-completion orderless)))
          (imenu (styles . (basic substring orderless)))
          (kill-ring (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))))

  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  (setq enable-recursive-minibuffers t)

  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60)               ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt. I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Add prompt indicator to `completing-read-multiple'. We display
  ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma. This
  ;; is copied from the README of the `vertico' package. I made some
  ;; small tweaks to propertize the segments of the prompt.
  (defun crm-indicator (args)
    (cons (format "[%s %s] %s"
                  (propertize "CRM" 'face 'error)
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'success)
                  (car args))
          (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(indiebrain-emacs-package savehist
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

(indiebrain-emacs-package vertico
  (:install t)
  ;; These are the default values, but check the user option
  ;; `vertico-multiform-categories' for per-category tweaks.
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)

  (vertico-mode 1)

  (indiebrain-emacs-keybind vertico-map
    "M-," #'vertico-quick-insert
    "M-." #'vertico-quick-exit)

  ;; This works with `file-name-shadow-mode'. When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only
  ;; your current input.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;;; Enhanced minibuffer commands (consult.el)
(indiebrain-emacs-package consult
  (:install t)
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")
  (setq consult-imenu-config
        '((emacs-lisp-mode :toplevel "Functions"
                           :types ((?f "Functions" font-lock-function-name-face)
                                   (?m "Macros"    font-lock-keyword-face)
                                   (?p "Packages"  font-lock-constant-face)
                                   (?t "Types"     font-lock-type-face)
                                   (?v "Variables" font-lock-variable-name-face)))))

  ;; Search C-h f for more "bookmark jump" handlers.
  (setq consult-bookmark-narrow
        `((?d "Docview" ,#'doc-view-bookmark-jump)
          (?e "Eshell" ,#'eshell-bookmark-jump)
          (?f "File" ,#'bookmark-default-handler)
          (?h "Help" ,#'help-bookmark-jump)
          (?i "Info" ,#'Info-bookmark-jump)
          (?m "Man" ,#'Man-bookmark-jump)
          (?v "VC Dir" ,#'vc-dir-bookmark-jump)
          (?w "EWW" ,#'indiebrain-eww-bookmark-jump)))
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
  (setq consult-find-args "find . -not ( -wholename */.* -prune )")
  (setq consult-preview-key 'any)

  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (require 'consult-imenu) ; the `imenu' extension is in its own file

  (indiebrain-emacs-keybind global-map
    "C-x r b" #'consult-bookmark ; override `bookmark-jump'
    "C-x M-:" #'consult-complex-command
    "C-x M-m" #'consult-minor-mode-menu
    "C-x M-k" #'consult-kmacro
    "M-g M-g" #'consult-goto-line
    "M-K" #'consult-keep-lines ; M-S-k is similar to M-S-5 (M-%
    "M-F" #'consult-focus-lines ; same principle
    "M-s M-b" #'consult-buffer
    "M-s M-f" #'consult-find
    "M-s M-g" #'consult-grep
    "M-s M-h" #'consult-history
    "M-s M-i" #'consult-imenu
    "M-s M-l" #'consult-line
    "M-s M-m" #'consult-mark
    "M-s M-s" #'consult-outline
    "M-s M-y" #'consult-yank-pop
    "C-x r r" #'consult-register) ; Use the register's prefix

  (indiebrain-emacs-keybind consult-narrow-map
    "?" #'consult-narrow-help)

  ;; see the `pulsar' package, which is configured in the
  ;; indiebrain-emacs-theme-extensions module:
  ;; <https://protesilaos.com/emacs/pulsar>
  (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with pulsar
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))

;;; Switch to directories (consult-dir.el)
(indiebrain-emacs-package consult-dir
  (:install t)
  (setq consult-dir-shadow-filenames nil)
  (setq consult-dir-sources '( consult-dir--source-bookmark
                               consult-dir--source-default
                               consult-dir--source-project
                               consult-dir--source-recentf))

  ;; Overrides `list-directory' in the `global-map', though I never used
  ;; that anyway.
  (dolist (map (list global-map minibuffer-local-filename-completion-map))
    (indiebrain-emacs-keybind map
      "C-x C-d" #'consult-dir)))

;;; Extended minibuffer actions and more (embark.el and indiebrain-embark.el)
(indiebrain-emacs-package embark
  (:install t)
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-quit-after-action t)     ; XXX: Read the doc string!

  ;; I never cycle and want to disable the key.  Normally, a nil value
  ;; disables a key binding but here that value is interpreted as the
  ;; binding for `embark-act'.  So I just add some obscure key that I
  ;; do not have.  I absolutely do not want to cycle by accident!
  (setq embark-cycle-key "<XF86Travel>")

  (setq embark-confirm-act-all nil)
  (setq embark-indicators
        '(embark-mixed-indicator
          embark-highlight-indicator))
  (setq embark-verbose-indicator-excluded-actions
        '("\\`customize-" "\\(local\\|global\\)-set-key"
          set-variable embark-cycle embark-keymap-help embark-isearch))
  (setq embark-verbose-indicator-buffer-sections
        `(target "\n" shadowed-targets " " cycle "\n" bindings))
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.2)
  (setq embark-verbose-indicator-display-action nil)
  (indiebrain-emacs-keybind global-map
    "C-," #'embark-act)
  (indiebrain-emacs-keybind embark-collect-mode-map
    "C-," #'embark-act)
  (indiebrain-emacs-keybind minibuffer-local-completion-map
    "C-," #'embark-act
    "C->" #'embark-become)
  (indiebrain-emacs-keybind embark-region-map
    "a" #'align-regexp
    "i" #'epa-import-keys-region
    "r" #'repunctuate-sentences ; overrides `rot13-region'
    "s" #'sort-lines
    "u" #'untabify)
  (indiebrain-emacs-keybind embark-symbol-map
    "." #'embark-find-definition
    "k" #'describe-keymap))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(indiebrain-emacs-package embark-consult
  (:install t))

(indiebrain-emacs-package indiebrain-embark
  (indiebrain-embark-keymaps 1)
  (indiebrain-embark-setup-packages 1))

;;; Completion for recent files and directories (indiebrain-recentf.el)
(indiebrain-emacs-package recentf
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (add-hook 'after-init-hook #'recentf-mode))

(indiebrain-emacs-package indiebrain-recentf
  (add-to-list 'recentf-keep 'indiebrain-recentf-keep-predicate)
  (indiebrain-emacs-keybind global-map
    "C-x C-r" #'indiebrain-recentf-recent-files-or-dirs))

;;; Corfu (in-buffer completion popup)
(indiebrain-emacs-package corfu
  (:install t)
  (global-corfu-mode 1)
  (indiebrain-emacs-keybind corfu-map
    "<tab>" #'corfu-complete)

  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

;;; CAPE (extra completion-at-point backends)
(indiebrain-emacs-package cape
  (:install t)
  (setq cape-dabbrev-min-length 3)
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

;;; Template-based in-buffer completion (tempel.el)
(indiebrain-emacs-package tempel
  (:install t)

  ;; Setup completion at point
  (defun contrib/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'contrib/tempel-setup-capf))

  (indiebrain-emacs-keybind global-map
    "M-+" #'tempel-complete ; Alternative: `tempel-expand'
    "M-*" #'tempel-insert)
  (indiebrain-emacs-keybind tempel-map
    "RET" #'tempel-done
    "C-p" #'tempel-previous
    "C-n" #'tempel-next))

;;; Enhance command-line completion (pcmpl-args)
(indiebrain-emacs-package pcmpl-args
  (:install t))

;;; Dabbrev (dynamic word completion)
(indiebrain-emacs-package dabbrev
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (indiebrain-emacs-keybind global-map
    "M-/" #'dabbrev-expand
    "C-x M-/" #'dabbrev-completion))

;;; Abbreviations or Abbrevs
(indiebrain-emacs-package abbrev
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
  (setq only-global-abbrevs nil)
  (setq save-abbrevs 'silently)

  (let ((table global-abbrev-table))
    (define-abbrev table "meweb" "https://aaronkuehler.com")
    (define-abbrev table "megit" "https://github.com/indiebrain"))

  (let ((table text-mode-abbrev-table))
    (define-abbrev table "visavis" "vis-à-vis")
    (define-abbrev table "cliche" "cliché")
    (define-abbrev table "latex" "LaTeX")
    (define-abbrev table "javascript" "JavaScript")
    (define-abbrev table "typescript" "TypeScript")
    (define-abbrev table "linkedin" "LinkedIn")
    (define-abbrev table "github" "Github")
    (define-abbrev table "gitlab" "GitLab")
    (define-abbrev table "sourcehut" "SourceHut")
    (define-abbrev table "libreplanet" "LibrePlanet")
    (define-abbrev table "emacsconf" "EmacsConf"))

  (indiebrain-emacs-keybind global-map
    "C-x a e" #'expand-abbrev
    "C-x a u" #'unexpand-abbrev)

  (dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
    (add-hook hook #'abbrev-mode)))

(provide 'indiebrain-emacs-completion)
;;; indiebrain-emacs-completion.el ends here
