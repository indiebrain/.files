;;; indiebrain-emacs-write.el --- Configuration for writing prose with Emacs  -*- lexical-binding: t; -*-

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

;; Configuration for writing prose with Emacs.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Outline mode and outline-minor-mode
(indiebrain-emacs-builtin-package 'outline
  (setq outline-minor-mode-highlight 'override)
  (setq outline-minor-mode-cycle t)
  (setq outline-minor-mode-use-buttons nil)
  (setq outline-minor-mode-use-margins nil)
  (indiebrain-emacs-keybind global-map
    "<f10>" #'outline-minor-mode))

;;; Denote (simple note-taking)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
(indiebrain-emacs-elpa-package 'denote
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/.notes"))
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default
  (setq denote-excluded-directories-regexp nil)

  ;; Use single-word keywords for a more disciplined workflow.
  (setq denote-allow-multi-word-keywords nil)

  (setq denote-date-format nil) ; read its doc string

  ;; By default, fontify backlinks in their bespoke buffer.
  (setq denote-link-fontify-backlinks t)

  ;; Generic (great if you rename files Denote-style in lots of places):
  (add-hook 'dired-mode-hook #'denote-dired-mode)

  (indiebrain-emacs-keybind global-map
    "C-c n n" #'denote
    "C-c n N" #'denote-type
    "C-c n o" #'denote-open-or-create
    "C-c n d" #'denote-date
    "C-c n s" #'denote-subdirectory
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    "C-c n i" #'denote-link ; "insert" mnemonic
    "C-c n I" #'denote-link-add-links
    "C-c n b" #'denote-link-backlinks
    "C-c n f f" #'denote-link-find-file
    "C-c n f b" #'denote-link-find-backlink
    "C-c n r" #'denote-rename-file)

  ;; Key bindings specifically for Dired.
  (indiebrain-emacs-keybind dired-mode-map
    "C-c C-d C-i" #'denote-link-dired-marked-notes
    "C-c C-d C-r" #'denote-dired-rename-marked-files)

  ;; Also see `denote-rename-file' further above.
  (indiebrain-emacs-keybind text-mode-map
    "C-c n R" #'denote-rename-file-using-front-matter)

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(indiebrain-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(indiebrain-emacs-elpa-package 'logos
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos--page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos--page-delimiter))
          (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos--page-delimiter))
          (conf-toml-mode . "^\\[")))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch t) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)

  ;; I don't need to do `with-eval-after-load' for the `modus-themes' as
  ;; I always load them before other relevant potentially packages.
  (add-hook 'modus-themes-after-load-theme-hook #'logos-update-fringe-in-buffers)

  (indiebrain-emacs-keybind global-map
    "C-x n n" #'logos-narrow-dwim
    "C-x ]" #'logos-forward-page-dwim
    "C-x [" #'logos-backward-page-dwim
    "M-]" #'logos-forward-page-dwim
    "M-[" #'logos-backward-page-dwim
    "<f9>" #'logos-focus-mode)

;;;; Extra tweaks
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun indiebrain/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'indiebrain/logos--recenter-top))

(provide 'indiebrain-emacs-write)
;;; indiebrain-emacs-write.el ends here
