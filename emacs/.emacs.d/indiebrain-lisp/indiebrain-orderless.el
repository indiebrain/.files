;;; indiebrain-orderless.el --- Configuration for the orderless completion style package  -*- lexical-binding: t; -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for the orderless completion style used in my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;;; Style dispatchers

;;;###autoload
(defun indiebrain-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

;;;###autoload
(defun indiebrain-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

;;;###autoload
(defun indiebrain-orderless-flex-dispatcher (pattern _index _total)
  "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))


(defun orderless--strict-*-initialism (component &optional anchored)
  "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.
If ANCHORED is `start' require that the first initial appear in
the first word of the candidate. If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
  (orderless--separated-by
   '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
   (cl-loop for char across component collect `(seq word-start ,char))
   (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
   (when (eq anchored 'both)
     '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

(defun orderless-strict-initialism (component)
  "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters. Only non-letters can be in between the
words that start with the initials."
  (orderless--strict-*-initialism component))

(defun orderless-strict-leading-initialism (component)
  "Match a COMPONENT as a strict initialism, anchored at start.
See `orderless-strict-initialism'. Additionally require that the
first initial appear in the first word of the candidate."
  (orderless--strict-*-initialism component 'start))

(defun orderless-strict-full-initialism (component)
  "Match a COMPONENT as a strict initialism, anchored at both ends.
See `orderless-strict-initialism'. Additionally require that the
first and last initials appear in the first and last words of the
candidate, respectively."
  (orderless--strict-*-initialism component 'both))

(provide 'indiebrain-orderless)
;;; indiebrain-orderless.el ends here
