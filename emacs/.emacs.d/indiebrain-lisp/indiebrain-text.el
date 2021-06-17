;;; indiebrain-text.el --- Extensions to text-mode.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my text-mode.el extensions, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.


;;; Code:

(require 'indiebrain-common)
(require 'indiebrain-simple)

;;;###autoload
(defun indiebrain-text-insert-heading (&optional arg)
  "Insert equal length heading delimiter below current line.

A heading delimiter is drawn as a series of dashes (-).  With
optional ARG, i.e. by prefixing \\[universal-argument], draw the
heading delimiter with equals signs (=).  The latter is
considered a heading level 1, while the former is level 2.

A heading delimiter is inserted only when that would not mess up
with existing headings or lists.  In such cases, point will move
to the next line.  For the purposes of this command, text that
starts with a number and no further delimiter is not consider a
list element.

This command is meant to be used in Text mode buffers and
compatible derivatives, such as Markdown mode, though not Org
mode which has its own conventions."
  (interactive "P")
  (cond
   ((derived-mode-p 'outline-mode)
    (user-error "Do not use `indiebrain-text-insert-heading' in `outline-mode' or derivatives!"))
   ((derived-mode-p 'text-mode)
    (let* ((num (- (point-at-eol) (point-at-bol)))
           (char (string-to-char (if arg "=" "-"))))
      (cond
       ((and (eobp)
             (or (indiebrain-common-line-regexp-p 'list 1)
                 (indiebrain-common-line-regexp-p 'heading 1)
                 (indiebrain-common-line-regexp-p 'empty 1)
                 (indiebrain-common-line-regexp-p 'indent 1)))
        (newline 1))
       ((or (indiebrain-common-line-regexp-p 'empty 1)
            (indiebrain-common-line-regexp-p 'indent 1))
        (indiebrain-simple-new-line-below))
       ((or (indiebrain-common-line-regexp-p 'list 1)
            (indiebrain-common-line-regexp-p 'heading 2))
        (if (indiebrain-common-line-regexp-p 'empty 3)
            (beginning-of-line 3)
          (indiebrain-simple-new-line-below)))
       ((or (indiebrain-common-line-regexp-p 'empty 2)
            (indiebrain-common-line-regexp-p 'indent 2))
        (indiebrain-simple-new-line-below)
        (insert (make-string num char))
        (newline 1)
        (beginning-of-line 2))
       (t
        (indiebrain-simple-new-line-below)
        (insert (make-string num char))
        (newline 2)))))))

(provide 'indiebrain-text)
;;; indiebrain-text.el ends here
