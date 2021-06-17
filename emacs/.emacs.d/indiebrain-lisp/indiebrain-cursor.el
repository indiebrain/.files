;;; indiebrain-cursor.el --- Extensions for the cursor -*- lexical-binding: t -*-

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
;; Extensions for the cursor, intended for my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(defgroup indiebrain-cursor ()
  "Tweaks for cursor appearance."
  :group 'cursor)

(defvar indiebrain-minibuffer-mini-cursors)

;;;###autoload
(define-minor-mode indiebrain-cursor-presentation-mode ()
  :init-value nil
  :global t
  (if indiebrain-cursor-presentation-mode
      (progn
        (setq-default indiebrain-minibuffer-mini-cursors nil) ; from `indiebrain-minibuffer.el'
        (setq-default cursor-type 'box)
        (setq-default cursor-in-non-selected-windows 'hollow)
        (setq-default blink-cursor-blinks 10)
        (setq-default blink-cursor-interval 0.5)
        (setq-default blink-cursor-delay 0.2)
        (blink-cursor-mode 1))
    (setq-default indiebrain-minibuffer-mini-cursors t)
    (setq-default cursor-type '(hbar . 3))
    (setq-default cursor-in-non-selected-windows 'hollow)
    (setq-default blink-cursor-blinks 50)
    (setq-default blink-cursor-interval 0.2)
    (setq-default blink-cursor-delay 0.2)
    (blink-cursor-mode 1)))

(provide 'indiebrain-cursor)
;;; indiebrain-cursor.el ends here
