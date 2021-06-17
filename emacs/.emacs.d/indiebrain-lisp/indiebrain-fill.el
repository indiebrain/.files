;;; indiebrain-fill.el --- Minor fill-mode tweaks for my dotemacs -*- lexical-binding: t -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my fill-mode extensions, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(defgroup indiebrain-fill ()
  "Tweak for filling paragraphs."
  :group 'fill)

(defcustom indiebrain-fill-default-column 72
  "Default width for `fill-column'."
  :type 'integer
  :group 'indiebrain-fill)

(defcustom indiebrain-fill-prog-mode-column 80
  "`prog-mode' width for `fill-column'.
Also see `indiebrain-fill-default-column'."
  :type 'integer
  :group 'indiebrain-fill)

(defun indiebrain-fill--fill-prog ()
  "Set local value of `fill-column' for programming modes.
Meant to be called via `prog-mode-hook'."
  (setq-local fill-column indiebrain-fill-prog-mode-column))

;;;###autoload
(define-minor-mode indiebrain-fill-fill-mode
  "Set up fill-mode and relevant variable."
  :init-value nil
  :global t
  (if indiebrain-fill-fill-mode
      (progn
        (setq-default fill-column indiebrain-fill-default-column)
        (add-hook 'prog-mode-hook #'indiebrain-fill--fill-prog)
        (add-hook 'text-mode-hook #'turn-on-auto-fill))
    (setq-default fill-column 70)
    (remove-hook 'prog-mode-hook #'indiebrain-fill--fill-prog)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)))

(provide 'indiebrain-fill)
;;; indiebrain-fill.el ends here
