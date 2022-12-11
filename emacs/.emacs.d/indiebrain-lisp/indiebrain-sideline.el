;;; indiebrain-sideline.el --- Line number and indicator extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Aaron

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

;; Extensions for line numbers and relevant indicators, intended to be
;; used as part of my Emacs configuration:
;; https://github.com/indiebrain/.files/

;;; Code:

(defgroup indiebrain-sideline ()
  "Setup for reading and presenting text-heavy buffers."
  :group 'files)

;;;###autoload
(define-minor-mode indiebrain-sideline-mode
  "Buffer-local wrapper mode for presentations."
  :init-value nil
  :global nil)

(autoload 'diff-hl-mode "diff-hl")

(defun indiebrain-sideline--diff-hl-toggle ()
  "Toggle buffer local diff indicators in the fringe."
  (if (or (bound-and-true-p diff-hl-mode)
          (not (bound-and-true-p indiebrain-sideline-mode)))
      (diff-hl-mode -1)
    (diff-hl-mode 1)))

(add-hook 'indiebrain-sideline-mode-hook #'indiebrain-sideline--diff-hl-toggle)

(defun indiebrain-sideline--numbers-toggle ()
  "Toggle line numbers."
  (if (or (bound-and-true-p display-line-numbers-mode)
          (not (bound-and-true-p indiebrain-sideline-mode)))
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(add-hook 'indiebrain-sideline-mode-hook #'indiebrain-sideline--numbers-toggle)

(defun indiebrain-sideline--hl-line-toggle ()
  "Toggle line highlight."
  (if (or (bound-and-true-p hl-line-mode)
          (not (bound-and-true-p indiebrain-sideline-mode)))
      (hl-line-mode -1)
    (hl-line-mode 1)))

(add-hook 'indiebrain-sideline-mode-hook #'indiebrain-sideline--hl-line-toggle)

(autoload 'whitespace-mode "whitespace")

;; We keep this separate, as I do not want it bundled up together with
;; the rest of the functionality included here.
;;;###autoload
(defun indiebrain-sideline-negative-space-toggle ()
  "Toggle the display of indentation and space characters."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(provide 'indiebrain-sideline)
;;; indiebrain-sideline.el ends here
