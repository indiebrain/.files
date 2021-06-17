;;; indiebrain-consult.el --- Tweak consult.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021  Aaron Kuehler

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tweaks for `consult.el' intended for my Emacs configuration:
;; https://github.com/indiebrain/.files/.

;;; Code:

(require 'consult nil t)
(require 'consult-imenu nil t)
(require 'indiebrain-pulse)

(defgroup indiebrain-consult ()
  "Tweaks for consult.el."
  :group 'minibuffer)

(defcustom indiebrain-consult-command-centre-list '(consult-line consult-mark)
  "Commands to run `indiebrain-consult-jump-recentre-hook'.
You must restart function `indiebrain-consult-set-up-hooks-mode' for
changes to take effect."
  :group 'indiebrain-consult
  :type 'list)

(defcustom indiebrain-consult-command-top-list '(consult-outline)
  "Commands to run `indiebrain-consult-jump-top-hook'.
You must restart function `indiebrain-consult-set-up-hooks-mode' for
changes to take effect."
  :group 'indiebrain-consult
  :type 'list)

;;;; Setup for some consult commands (TODO: needs review)

(defvar indiebrain-consult-jump-recentre-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun indiebrain-consult-after-jump-recentre (&rest _)
  "Run `indiebrain-consult-jump-recentre-hook'."
  (run-hooks 'indiebrain-consult-jump-recentre-hook))

(defvar indiebrain-consult-jump-top-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun indiebrain-consult-after-jump-top (&rest _)
  "Run `indiebrain-consult-jump-top-hook'."
  (run-hooks 'indiebrain-consult-jump-top-hook))

;;;###autoload
(define-minor-mode indiebrain-consult-set-up-hooks-mode
  "Set up hooks for Consult."
  :init-value nil
  :global t
  (if indiebrain-consult-set-up-hooks-mode
      (progn
        (dolist (fn indiebrain-consult-command-centre-list)
          (advice-add fn :after #'indiebrain-consult-after-jump-recentre))
        (dolist (fn indiebrain-consult-command-top-list)
          (advice-add fn :after #'indiebrain-consult-after-jump-top))
        (add-hook 'indiebrain-consult-jump-recentre-hook #'indiebrain-pulse-recentre-centre)
        (add-hook 'indiebrain-consult-jump-top-hook #'indiebrain-pulse-recentre-top)
        (add-hook 'indiebrain-consult-jump-top-hook #'indiebrain-pulse-show-entry))
    (dolist (fn indiebrain-consult-command-centre-list)
      (advice-remove fn #'indiebrain-consult-after-jump-recentre))
    (dolist (fn indiebrain-consult-command-top-list)
      (advice-remove fn #'indiebrain-consult-after-jump-top))
    (remove-hook 'indiebrain-consult-jump-recentre-hook #'indiebrain-pulse-recentre-centre)
    (remove-hook 'indiebrain-consult-jump-top-hook #'indiebrain-pulse-recentre-top)
    (remove-hook 'indiebrain-consult-jump-top-hook #'indiebrain-pulse-show-entry)))

;;;; Commands

(defvar consult--find-cmd)
(defvar consult--directory-prompt)
(declare-function consult--find "consult")
(autoload 'indiebrain-orderless-with-styles "indiebrain-orderless")

;;;###autoload
(defun indiebrain-consult-project-root ()
  "Return path to project or `default-directory'.
Intended to be assigned to `consult-project-root-function'."
  (or (vc-root-dir)
      (locate-dominating-file "." ".git")
      default-directory))

;;;###autoload
(defun indiebrain-consult-outline ()
  "Run `consult-outline' through `indiebrain-orderless-with-styles'."
  (interactive)
  (indiebrain-orderless-with-styles 'consult-outline))

;;;###autoload
(defun indiebrain-consult-imenu ()
  "Run `consult-imenu' through `indiebrain-orderless-with-styles'."
  (interactive)
  (indiebrain-orderless-with-styles 'consult-imenu))

;;;###autoload
(defun indiebrain-consult-line ()
  "Run `consult-line' through `indiebrain-orderless-with-styles'."
  (interactive)
  (indiebrain-orderless-with-styles 'consult-line))

;;;###autoload
(defun indiebrain-consult-yank ()
  "Run `consult-yank' through `indiebrain-orderless-with-styles'."
  (interactive)
  (indiebrain-orderless-with-styles 'consult-yank))

(provide 'indiebrain-consult)
;;; indiebrain-consult.el ends here
