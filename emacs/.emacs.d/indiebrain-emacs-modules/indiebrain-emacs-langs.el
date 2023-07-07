;;; indiebrain-emacs-langs.el --- Configuration for language specific modes  -*- lexical-binding: t; -*-

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

;; Configuration for various language-specific modes
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Paragraphs and fill-mode
(setq sentence-end-double-space nil)
(setq sentence-end-without-period nil)
(setq colon-double-space nil)
(setq use-hard-newlines nil)
(setq adaptive-fill-mode t)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;;; Comments (newcomment.el and indiebrain-comment.el)
(indiebrain-emacs-package newcomment
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (indiebrain-emacs-keybind global-map
    "C-:" #'comment-kill         ; C-S-;
    "M-;" #'comment-indent))

(indiebrain-emacs-package indiebrain-comment
  (setq indiebrain-comment-comment-keywords
        '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq indiebrain-comment-timestamp-format-concise "%F")
  (setq indiebrain-comment-timestamp-format-verbose "%F %T %z")
  (indiebrain-emacs-keybind global-map
    "C-;" #'indiebrain-comment-comment-dwim
    "C-x C-;" #'indiebrain-comment-timestamp-keyword))

;;; Configure 'electric' behaviour
(indiebrain-emacs-package electric
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

;;; Parentheses (show-paren-mode)
(indiebrain-emacs-package paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'child-frame) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode))

;;; Tabs, indentation, and the TAB key
(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Flyspell and indiebrain-spell.el (spell check)
(indiebrain-emacs-package flyspell
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_US")
  (indiebrain-emacs-keybind flyspell-mode-map
    "C-;" nil)
  (indiebrain-emacs-keybind ctl-x-x-map
    "s" #'flyspell-mode)

  ;; Enable spell checking in buffers where the major mode derives
  ;; from text-mode (IE most buffers where prose heavy editing is taking place).
  (add-hook 'text-mode-hook #'flyspell-mode)

  ;; Enable spell checking of comments in buffers where the major mode
  ;; derives from prog-mode (IE most buffers where code editing is
  ;; taking place.)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)) ; C-x x s

(indiebrain-emacs-package indiebrain-spell
  (setq indiebrain-spell-dictionaries
        '(("EN English" . "en")
          ("FR Français" . "fr")))

  ;; Also check indiebrain-spell.el for what I am doing with
  ;; `indiebrain-spell-ispell-display-buffer'.  Then refer to the
  ;; `display-buffer-alist' for the relevant entry.

  (indiebrain-emacs-keybind global-map
    "M-$" #'indiebrain-spell-spell-dwim
    "C-M-$" #'indiebrain-spell-change-dictionary))

;;; Dictionary
(indiebrain-emacs-package dictionary
  (setq dictionary-server "dict.org"
          dictionary-default-popup-strategy "lev" ; read doc string
          dictionary-create-buttons nil
          dictionary-use-single-buffer t)
  (indiebrain-emacs-keybind global-map "C-c d" #'dictionary-search))

;;; Flymake
(indiebrain-emacs-package flymake
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  (setq flymake-mode-line-counter-format
        '(" " flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))

  (defvar indiebrain/flymake-mode-projects-path
    (file-name-as-directory (expand-file-name "Developer" "~/"))
    "Path to my Git projects.")

 (defun indiebrain/flymake-mode-lexical-binding ()
    (when lexical-binding
      (flymake-mode 1)))

  (defun indiebrain/flymake-mode-in-my-projects ()
    (when-let* ((file (buffer-file-name))
                ((string-prefix-p indiebrain/flymake-mode-projects-path
                                  (expand-file-name file)))
                ((not (file-directory-p file)))
                ((file-regular-p file)))
      (add-hook 'find-file-hook #'indiebrain/flymake-mode-lexical-binding nil t)))

  (add-hook 'emacs-lisp-mode-hook #'indiebrain/flymake-mode-in-my-projects)

  (indiebrain-emacs-keybind ctl-x-x-map
    "m" #'flymake-mode) ; C-x x m
  (indiebrain-emacs-keybind flymake-mode-map
    "C-c ! s" #'flymake-start
    "C-c ! d" #'flymake-show-buffer-diagnostics ; Emacs28
    "C-c ! D" #'flymake-show-project-diagnostics ; Emacs28
    "C-c ! n" #'flymake-goto-next-error
    "C-c ! p" #'flymake-goto-prev-error))

;;;; Flymake + Shellcheck
(indiebrain-emacs-package flymake-shellcheck
  (:install t)
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;;;; Flymake + Proselint
(indiebrain-emacs-package flymake-proselint
  (:install t)
  (add-hook 'text-mode-hook #'flymake-proselint-setup))

;;;; Elisp packaging requirements
(indiebrain-emacs-package package-lint-flymake
  (:install t)
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;;; Flycheck
(indiebrain-emacs-package flycheck
  (:install t)
  (global-flycheck-mode))

;;;; Eldoc (elisp live documentation feedback)
(indiebrain-emacs-package eldoc
  (global-eldoc-mode 1))

;;;; Handle performance for very long lines (so-long.el)
(indiebrain-emacs-package so-long
  (global-so-long-mode 1))

;;;; asdf version manager integration
(indiebrain-emacs-package asdf
  (:install "https://github.com/tabfugnic/asdf.el")
  (asdf-enable))

;;;; Language Servers (lsp-mode)
(indiebrain-emacs-package lsp-mode
  (:install t)
  (setopt lsp-ruby-lsp-use-bundler t)

  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'ruby-mode-hook #'lsp-deferred))

(indiebrain-emacs-package lsp-ui
  (:install t))

;;; Language specific settings

;;;; Caddy server (caddyfile-mode)
(indiebrain-emacs-package caddyfile-mode
  (:install t))

;;;; CSS (css-mode)
(indiebrain-emacs-package css-mode
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq css-fontify-colors nil))

;;;; Containerization (docker and dockerfile-mode)
(indiebrain-emacs-package docker
  (:install t)
  (setq docker-command "podman")
  (setq docker-compose-command "podman-compose"))

(indiebrain-emacs-package dockerfile-mode
  (:install t)
  (add-to-list 'auto-mode-alist '("\\Dockerfile$" . dockerfile-mode))
  (add-to-list 'auto-mode-alist '("\\Containerfile$" . dockerfile-mode)))

;;;; Go-lang (go-mode)
(indiebrain-emacs-package go-mode
  (:install t)
  (add-hook 'before-save-hook #'gofmt-before-save))

;;;; GraphQL (graphql-mode)
(indiebrain-emacs-package graphql-mode
  (:install t))

;;;; JavaScript (js2-mode)
(indiebrain-emacs-package js2-mode
  (:install t)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings t)

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq flycheck-javascript-eslint-executable "node_modules/.bin/eslint"))

;;;; Markdown (markdown-mode)
(indiebrain-emacs-package markdown-mode
  (:install t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))

;;;; React JSX (rjsx-mode)
(indiebrain-emacs-package rjsx-mode
  (:install t)
  (add-to-list 'auto-mode-alist '("\\.tsx$" . rjsx-mode)))

;;;; Shell scripts (sh-mode)
(indiebrain-emacs-package sh-script
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

;;;; Text (text-mode)
(indiebrain-emacs-package text-mode
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode)))

;;;; Ruby (ruby-mode)
(indiebrain-emacs-package ruby-mode
  (setq ruby-insert-encoding-magic-comment nil
    ruby-deep-indent-paren nil
    ruby-indent-tabs-mode nil
    flycheck-ruby-rubocop-executable "bin/rubocop")

  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile[.lock]?$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode)))

(indiebrain-emacs-package indiebrain-langs-ruby
  (indiebrain-emacs-keybind ruby-mode-map
    "C-c C-c" 'xmp))

(indiebrain-emacs-package ruby-end
  (:install t))

(indiebrain-emacs-package inf-ruby
  (:install t))

(indiebrain-emacs-package rspec-mode
  (:install t)
  (setq compilation-scroll-output 'first-error)
  (add-hook 'after-init-hook #'inf-ruby-switch-setup))

;;;; Rust (rust-mode)
(indiebrain-emacs-package rust-mode
  (:install t))

(indiebrain-emacs-package flycheck-rust
  (:install t))

(indiebrain-emacs-package cargo
  (:install t)
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

;;;; TypeScript (typescript-mode)
(indiebrain-emacs-package typescript-mode
  (:install t)
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (setq typescript-indent-level 2)
  (setq typescript-indent-switch-clauses t))

;;;; Web / HTML (web-mode)
(indiebrain-emacs-package web-mode
  (:install t)
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html.erb$" . web-mode)))

;;;; YAML (yaml-mode)
(indiebrain-emacs-package yaml-mode
  (:install t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))


(provide 'indiebrain-emacs-langs)
;;; indiebrain-emacs-langs.el ends here
