;;; indiebrain-moody.el --- Extensions to moody.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my moody.el extensions, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(require 'indiebrain-common)
(require 'indiebrain-fonts)
(require 'moody nil t)

(defgroup indiebrain-moody ()
  "Tweaks for moody.el."
  :group 'mode-line)

(defcustom indiebrain-moody-font-height-multiplier 1.65
  "Multiple of the font size to derive the moody height."
  :type 'number
  :group 'indiebrain-moody)

(defun indiebrain-moody--height ()
  "Set Moody height to an even number.
Bind this to a hook that gets called after loading/changing the
mode line's typeface (or the default one if they are the same)."
  (let* ((font (face-font 'mode-line))
         (height (truncate (* indiebrain-moody-font-height-multiplier (aref (font-info font) 2))))
         (height-even (if (indiebrain-common-number-even-p height) height (+ height 1))))
    (if font
        height-even
      24)))

(defvar moody-mode-line-height)

(defun indiebrain-moody--mode-line-height ()
  "Set Moody height to the value of `indiebrain-moody--height'."
  (let ((height (indiebrain-moody--height)))
    (setq moody-mode-line-height height)))

(autoload 'moody-replace-mode-line-buffer-identification "moody")
(autoload 'moody-replace-vc-mode "moody")

;;;###autoload
(define-minor-mode indiebrain-moody-set-height
  "Toggle Moody for the mode line and configure its fonts."
  :init-value nil
  :global t
  (if indiebrain-moody-set-height
      (progn
        (moody-replace-mode-line-buffer-identification)
        (moody-replace-vc-mode)
        (add-hook 'indiebrain-fonts-set-typeface-hook #'indiebrain-moody--mode-line-height)
        (run-hooks 'indiebrain-fonts-set-typeface-hook))
    (let ((format (default-value 'mode-line-format)))
      (when (member 'moody-mode-line-buffer-identification format)
        (moody-replace-mode-line-buffer-identification 'reverse))
      (when (member '(vc-mode moody-vc-mode) format)
        (moody-replace-vc-mode 'reverse)))
    (remove-hook 'indiebrain-fonts-set-typeface-hook #'indiebrain-moody--mode-line-height)))

(defvar keycast-insert-after)

(defun indiebrain-moody-keycast-insert-after ()
  "Specify `keycast-insert-after' buffer identification."
  (setq keycast-insert-after
        (if indiebrain-moody-set-height
            'moody-mode-line-buffer-identification
          'mode-line-buffer-identification)))

(provide 'indiebrain-moody)
;;; indiebrain-moody.el ends here
