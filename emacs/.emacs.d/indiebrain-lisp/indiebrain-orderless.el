;;; indiebrain-orderless.el --- Extensions for Orderless -*- lexical-binding: t -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extensions for the Orderless completion style for use in my Emacs
;; setup: <https://github.com/indiebrain/.files/>.

;;; Code:

(defgroup indiebrain-orderless ()
  "Tweaks for the Orderless completion style."
  :group 'minibuffer)

(defcustom indiebrain-orderless-default-styles
  '(orderless-flex
    orderless-strict-leading-initialism
    orderless-regexp
    orderless-prefixes
    orderless-literal)
  "List that should be assigned to `orderless-matching-styles'."
  :type 'list
  :group 'indiebrain-orderless)

(defcustom indiebrain-orderless-alternative-styles
  '(orderless-literal
    orderless-prefixes
    orderless-strict-leading-initialism
    orderless-regexp)
  "Alternative list for `orderless-matching-styles'.

Unlike `indiebrain-orderless-default-styles', this variable is intended
for use on a case-by-case basis, with the help of the function
`indiebrain-orderless-with-styles'."
  :type 'list
  :group 'indiebrain-orderless)

(defun indiebrain-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun indiebrain-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

(defun indiebrain-orderless-flex-dispatcher (pattern _index _total)
  "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defvar orderless-matching-styles)

;;;###autoload
(defun indiebrain-orderless-with-styles (cmd &optional styles)
  "Call CMD with optional orderless STYLES.

STYLES is a list of pattern matching methods that is passed to
`orderless-matching-styles'. Its fallback value is that of
`indiebrain-orderless-alternative-styles'."
  (let ((orderless-matching-styles (or styles indiebrain-orderless-alternative-styles))
        (this-command cmd))
    (call-interactively cmd)))

(provide 'indiebrain-orderless)
;;; indiebrain-orderless.el ends here
