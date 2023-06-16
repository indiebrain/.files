;;; indiebrain-emacs-search.el ---search configuration  -*- lexical-binding: t; -*-

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

;; Configuration for searching in Emacs
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Isearch, occur, grep, and extras (indiebrain-search.el)
(indiebrain-emacs-builtin-package 'isearch
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace nil)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  ;; These variables are from Emacs 28
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 3)
  (setq isearch-wrap-pause t)

  (indiebrain-emacs-keybind minibuffer-local-isearch-map
    "M-/" #'isearch-complete-edit)
  (indiebrain-emacs-keybind isearch-mode-map
    "C-g" #'isearch-cancel ; instead of `isearch-abort'
    "M-/" #'isearch-complete))

(indiebrain-emacs-builtin-package 'replace
  (setq list-matching-lines-jump-to-current-line nil)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'indiebrain-common-truncate-lines-silently) ; from `indiebrain-common.el'
  (indiebrain-emacs-keybind occur-mode-map "t" #'toggle-truncate-lines))

(indiebrain-emacs-builtin-package 'grep)

(indiebrain-emacs-builtin-package 'indiebrain-search
  (setq indiebrain-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq indiebrain-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (indiebrain-emacs-keybind global-map
    "M-s %" #'indiebrain-search-isearch-replace-symbol
    "M-s M-%" #'indiebrain-search-replace-markup ; see `indiebrain-search-markup-replacements'
    "M-s M-<" #'indiebrain-search-isearch-beginning-of-buffer
    "M-s M->" #'indiebrain-search-isearch-end-of-buffer
    "M-s g" #'indiebrain-search-grep
    "M-s u" #'indiebrain-search-occur-urls
    "M-s t" #'indiebrain-search-occur-todo-keywords
    "M-s M-t" #'indiebrain-search-grep-todo-keywords ; With C-u it runs `indiebrain-search-git-grep-todo-keywords'
    "M-s M-o" #'indiebrain-search-occur-outline
    "M-s M-u" #'indiebrain-search-occur-browse-url)
  (indiebrain-emacs-keybind isearch-mode-map
    "<up>" #'indiebrain-search-isearch-repeat-backward
    "<down>" #'indiebrain-search-isearch-repeat-forward
    "<backspace>" #'indiebrain-search-isearch-abort-dwim
    "<C-return>" #'indiebrain-search-isearch-other-end))

;;; Test regular expressions (re-builder)
(indiebrain-emacs-builtin-package 're-builder
  (setq reb-re-syntax 'read))

;;; wgrep (writable grep)
(indiebrain-emacs-elpa-package 'wgrep
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (indiebrain-emacs-keybind grep-mode-map
    "e" #'wgrep-change-to-wgrep-mode
    "C-x C-q" #'wgrep-change-to-wgrep-mode
    "C-c C-c" #'wgrep-finish-edit))

;;; Cross-references (xref.el)
(indiebrain-emacs-builtin-package 'xref
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program
        (cond
         ((executable-find "ugrep") 'ugrep)
         ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
         (t 'grep))))

;;; Built-in bookmarking framework (bookmark.el and indiebrain-bookmark.el)
(indiebrain-emacs-builtin-package 'bookmark
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode))

(indiebrain-emacs-builtin-package 'indiebrain-bookmark
  (indiebrain-bookmark-extra-keywords 1))

(provide 'indiebrain-emacs-search)
;;; indiebrain-emacs-search.el ends here
