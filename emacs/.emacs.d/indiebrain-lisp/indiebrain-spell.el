;;; indiebrain-spell.el --- Extensions for spelling for my Emacs configuration  -*- lexical-binding: t; -*-

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

;; Spelling related extensions for my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(defgroup indiebrain-spell ()
  "Extensions for ispell and flyspell."
  :group 'ispell)

(defcustom indiebrain-spell-dictionaries
  '(("EN English" . "en")
    ("FR Français" . "fr"))
  "Alist of strings with descriptions and dictionary keys.
Used by `indiebrain-spell-change-dictionary'."
  :type 'alist
  :group 'indiebrain-spell)

(defvar indiebrain-spell--dictionary-hist '()
  "Input history for `indiebrain-spell-change-dictionary'.")

(defun indiebrain-spell--dictionary-prompt ()
  "Helper prompt to select from `indiebrain-spell-dictionaries'."
  (let ((def (car indiebrain-spell--dictionary-hist)))
    (completing-read
     (format "Select dictionary [%s]: " def)
     (mapcar #'car indiebrain-spell-dictionaries)
     nil t nil 'indiebrain-spell--dictionary-hist def)))

;;;###autoload
(defun indiebrain-spell-change-dictionary (dictionary)
  "Select a DICTIONARY from `indiebrain-spell-dictionaries'."
  (interactive
   (list (indiebrain-spell--dictionary-prompt)))
  (let* ((key (cdr (assoc dictionary indiebrain-spell-dictionaries)))
         (desc (car (assoc dictionary indiebrain-spell-dictionaries))))
    (ispell-change-dictionary key)
    (message "Switched dictionary to %s" (propertize desc 'face 'bold))))

(autoload 'flyspell-region "flyspell")
(autoload 'thing-at-point "thingatpt")
(autoload 'ispell-word "ispell")

;;;###autoload
(defun indiebrain-spell-spell-dwim (beg end)
  "Spellcheck between BEG END, current word, or select dictionary.
Use `flyspell-region' on the active region.  With point over a
word and no active region invoke `ispell-word'.  Else call
`indiebrain-spell-change-dictionary'."
  (interactive "r")
  (cond
   ((use-region-p)
    (flyspell-region beg end))
   ((thing-at-point 'word)
    (call-interactively 'ispell-word))
   (t
    (call-interactively 'indiebrain-spell-change-dictionary))))

(setq ispell-choices-buffer "*ispell-top-choices*")

(defun indiebrain-spell-ispell-display-buffer (buffer)
  "Function to override `ispell-display-buffer'.
Use this as `advice-add' to override the aforementioned Ispell
function.  Then you can control the buffer's specifics via
`display-buffer-alist' (how it ought to be!)."
  (pop-to-buffer buffer)
  (set-window-point (get-buffer-window buffer) (point-min)))

(advice-add #'ispell-display-buffer :override #'indiebrain-spell-ispell-display-buffer)

(provide 'indiebrain-spell)
;;; indiebrain-spell.el ends here
