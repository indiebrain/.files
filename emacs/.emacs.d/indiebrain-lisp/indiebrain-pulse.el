;;; indiebrain-pulse.el --- Extend pulse.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2012  Aaron Kuehler

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
;; Extensions to the built-in `pulse.el' library for my Emacs
;; configuration: <https://github.com/indiebrain/.files/>.

;;; Code:

(require 'pulse)

(defgroup indiebrain-pulse ()
  "Extensions for `pulse.el'."
  :group 'editing)

(defcustom indiebrain-pulse-pulse-command-list
  '(recenter-top-bottom reposition-window)
  "Commands that should automatically `indiebrain-pulse-pulse-line'.
You must restart function `indiebrain-pulse-advice-commands-mode' for
changes to take effect."
  :type 'list
  :group 'indiebrain-pulse)

(defface indiebrain-pulse-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Default face for `indiebrain-pulse-pulse-line'."
  :group 'indiebrain-pulse)

;;;###autoload
(defun indiebrain-pulse-pulse-line (&optional face)
  "Temporarily highlight the current line with optional FACE."
  (interactive)
  (let ((start (if (eobp)
                   (line-beginning-position 0)
                 (line-beginning-position)))
        (end (line-beginning-position 2))
        (pulse-delay .04)
        (face (or face 'indiebrain-pulse-line)))
    (pulse-momentary-highlight-region start end face)))

;;;###autoload
(defun indiebrain-pulse-recentre-top ()
  "Reposition at the top and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (let ((pulse-delay .05))
    (recenter 0)
    (indiebrain-pulse-pulse-line)))

;;;###autoload
(defun indiebrain-pulse-recentre-centre ()
  "Recentre and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (let ((pulse-delay .05))
    (recenter nil)
    (indiebrain-pulse-pulse-line)))

(autoload 'org-at-heading-p "org")
(autoload 'org-show-entry "org")
(autoload 'org-reveal "org")
(autoload 'outline-show-entry "outline")

;;;###autoload
(defun indiebrain-pulse-show-entry ()
  "Reveal index at point in outline views.
To be used with a hook such as `imenu-after-jump-hook'."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry)
    (org-reveal t))
   ((bound-and-true-p indiebrain-outline-minor-mode)
    (outline-show-entry))))

(defvar indiebrain-pulse-after-command-hook nil
  "Hook that runs after select commands.
To be used with `advice-add' after those functions declared in
`indiebrain-pulse-pulse-command-list'.")

(defun indiebrain-pulse-after-command (&rest _)
  "Run `indiebrain-pulse-after-command-hook'."
  (run-hooks 'indiebrain-pulse-after-command-hook))

;;;###autoload
(define-minor-mode indiebrain-pulse-advice-commands-mode
  "Set up for `indiebrain-pulse-pulse-command-list'."
  :init-value nil
  :global t
  (if indiebrain-pulse-advice-commands-mode
      (progn
        (dolist (fn indiebrain-pulse-pulse-command-list)
          (advice-add fn :after #'indiebrain-pulse-after-command))
        (add-hook 'indiebrain-pulse-after-command-hook #'indiebrain-pulse-pulse-line))
    (dolist (fn indiebrain-pulse-pulse-command-list)
      (advice-remove fn #'indiebrain-pulse-after-command))
    (remove-hook 'indiebrain-pulse-after-command-hook #'indiebrain-pulse-pulse-line)))

(provide 'indiebrain-pulse)
;;; indiebrain-pulse.el ends here
