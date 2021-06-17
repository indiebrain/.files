;;; indiebrain-diff.el --- Extensions to diff-mode.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021  Aaron Kuehler

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my diff-mode.el extensions, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.
;;
;; Make sure to also inspect indiebrain-vc.el and indiebrain-project.el for a more
;; complete view of what I have on the topic of version control.

;;; Code:

(require 'diff-mode)

(defgroup indiebrain-diff ()
  "Extensions for diff mode."
  :group 'diff)

;;;###autoload
(defun indiebrain-diff-buffer-dwim (&optional arg)
  "Diff buffer with its file's last saved state, or run `vc-diff'.
With optional prefix ARG (\\[universal-argument]) enable
highlighting of word-wise changes (local to the current buffer)."
  (interactive "P")
  (let ((buf))
    (if (buffer-modified-p)
        (progn
          (diff-buffer-with-file (current-buffer))
          (setq buf "*Diff*"))
      (vc-diff)
      (setq buf "*vc-diff*"))
    (when arg
      (with-current-buffer (get-buffer buf)
        (unless diff-refine
          (setq-local diff-refine 'font-lock))))))

(defvar-local indiebrain-diff--refine-diff-state 0
  "Current state of `indiebrain-diff-refine-dwim'.")

;;;###autoload
(defun indiebrain-diff-refine-cycle ()
  "Produce buffer-local, 'refined' or word-wise diffs in Diff mode.

Upon first invocation, refine the diff hunk at point or, when
none exists, the one closest to it. On second call, operate on
the entire buffer. And on the third time, remove all word-wise
fontification."
  (interactive)
  (let ((point (point)))
    (pcase indiebrain-diff--refine-diff-state
      (0
       (diff-refine-hunk)
       (setq indiebrain-diff--refine-diff-state 1))
      (1
       (setq-local diff-refine 'font-lock)
       (font-lock-flush)
       (goto-char point)
       (setq indiebrain-diff--refine-diff-state 2))
      (_
       (revert-buffer)
       (goto-char point)
       (recenter)
       (setq indiebrain-diff--refine-diff-state 0)))))

;;;###autoload
(defun indiebrain-diff-narrow-dwim (&optional arg)
  "Use `diff-restrict-view', or widen when already narrowed.
By default the narrowing effect applies to the focused diff hunk.
With optional prefix ARG (\\[universal-argument]) do it for the
current file instead."
  (interactive "P")
  (when (derived-mode-p 'diff-mode)
    (if (buffer-narrowed-p)
        (progn
          (widen)
          (message "Widened the view"))
      (if arg
          (progn
            (diff-restrict-view arg)
            (message "Narrowed to file"))
        (diff-restrict-view)
        (message "Narrowed to diff hunk")))))

(defvar modus-themes-diffs)

;;;###autoload
(defun indiebrain-diff-modus-themes-diffs ()
  "Configure `diff-font-lock-syntax' for accessibility.

A non-nil value for that variable will apply fontification to the
text while also trying to add the familiar diff styles. This can
easily result in inaccessible colour combinations.

My Modus themes, which are designed for the highest accessibility
standard in legibility, provide an option that can work well with
such non-nil values. Otherwise `diff-font-lock-syntax' should be
set to nil.

Run this function at the post theme load phase, such as with the
hook `modus-themes-after-load-theme-hook'."
  (if (eq modus-themes-diffs 'bg-only)
      (setq diff-font-lock-syntax 'hunk-also)
    (setq diff-font-lock-syntax nil)))

;;; Extend diff-mode font lock

(defface indiebrain-diff-diffstat-added
  '((t :inherit diff-indicator-added))
  "Face for diffstat added indicators (+).")

(defface indiebrain-diff-diffstat-removed
  '((t :inherit diff-indicator-removed))
  "Face for diffstat removed indicators (-).")

(defface indiebrain-diff-commit-header
  '((((class color) (min-colors 88) (background light))
     :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffffff"))
  "Face for diff commit header keys like 'Author:'.")

(defface indiebrain-diff-commit-hash
  '((((class color) (min-colors 88) (background light))
     :foreground "#184034")
    (((class color) (min-colors 88) (background dark))
     :foreground "#bfebe0")
    (t :inherit shadow))
  "Face for diff commit unique identifier (hash).")

(defface indiebrain-diff-commit-author
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00d3d0")
    (t :foreground "cyan"))
  "Face for diff commit author name.")

(defface indiebrain-diff-commit-email
  '((((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff")
    (t :foreground "blue"))
  "Face for diff commit author email.")

(defface indiebrain-diff-commit-date
  '((((class color) (min-colors 88) (background light))
     :foreground "#55348e")
    (((class color) (min-colors 88) (background dark))
     :foreground "#cfa6ff")
    (t :foreground "magenta"))
  "Face for diff commit date.")

(defface indiebrain-diff-commit-subject
  '((((class color) (min-colors 88) (background light))
     :foreground "#005a5f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#6ae4b9")
    (t :foreground "cyan"))
  "Face for diff commit message subject.")

;; NOTE 2021-01-30: These work in all scenaria I tried, but there may
;; still be errors or omissions.
(defconst indiebrain-diff-keywords
  '(("\\(^[^+@-]?\\)\\(.*?\s+|\s+\\)\\([0-9]*\\) \\(\\++\\)"
     ;; (2 'indiebrain-diff-diffstat-file-changed)
     (4 'indiebrain-diff-diffstat-added))
    ("\\(^[^+-]?\\)\\(\\+\\{3\\}\\) \\([ab].*?\\)"
     (2 'indiebrain-diff-diffstat-added))
    ("\\(^[^+-]?\\)\\(-+\\{3\\}\\) \\([ab].*?\\)"
     (2 'indiebrain-diff-diffstat-removed))
    ("\\(^[^+@-]?\\)\\(.*?\s+|\s+\\)\\([0-9]*\\) \\(\\++\\)?\\(-+\\)"
     ;; (2 'indiebrain-diff-diffstat-file-changed)
     (5 'indiebrain-diff-diffstat-removed))
    ;; ("\\([0-9]+ files? changed,.*\\)"
    ;;  (0 'indiebrain-diff-diffstat-file-changed))
    ("^---\n"
     (0 'indiebrain-diff-commit-header))
    ("\\(^commit \\)\\(.*\\)"
     (1 'indiebrain-diff-commit-header)
     (2 'indiebrain-diff-commit-hash))
    ("\\(^Author: \\)\\(.*\\)\\(<\\)\\(.*\\)\\(>\\)"
     (1 'indiebrain-diff-commit-header)
     (2 'indiebrain-diff-commit-author)
     (3 'indiebrain-diff-commit-header)
     (4 'indiebrain-diff-commit-email)
     (5 'indiebrain-diff-commit-header))
    ("\\(^From:\\|^To:\\|^Cc:\\) ?\\(.*\\)?\\(<\\)\\(.*\\)\\(>\\)"
     (1 'indiebrain-diff-commit-header)
     (2 'indiebrain-diff-commit-author)
     (3 'indiebrain-diff-commit-header)
     (4 'indiebrain-diff-commit-email)
     (5 'indiebrain-diff-commit-header))
    ("\\(^Subject:\\) \\(.*\\)"
     (1 'indiebrain-diff-commit-header)
     (2 'indiebrain-diff-commit-subject))
    ("\\(^From\\)\\( [0-9a-zA-Z]+ \\)\\(.*\\)"
     (1 'indiebrain-diff-commit-header)
     (2 'indiebrain-diff-commit-hash)
     (3 'indiebrain-diff-commit-date))
    ("\\(^Message-Id:\\) \\(<.+>\\)"
     (1 'indiebrain-diff-commit-header)
     (2 'indiebrain-diff-commit-hash))
    ("\\(^Date: \\)\\(.*\\)"
     (1 'indiebrain-diff-commit-header)
     (2 'indiebrain-diff-commit-date)))
  "Extra font-lock patterns for diff mode.")

;;;###autoload
(define-minor-mode indiebrain-diff-extra-keywords
  "Apply extra font-lock rules to diff buffers."
  :init-value nil
  :global t
  (if indiebrain-diff-extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil indiebrain-diff-keywords nil)
        (add-hook 'diff-mode-hook #'indiebrain-diff-extra-keywords))
    (font-lock-remove-keywords nil indiebrain-diff-keywords)
    (remove-hook 'diff-mode-hook #'indiebrain-diff-extra-keywords)
    (font-lock-flush (point-min) (point-max))))

(provide 'indiebrain-diff)
;;; indiebrain-diff.el ends here
