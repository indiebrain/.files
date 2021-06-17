;;; indiebrain-comment.el --- Extensions newcomment.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my newcomment.el extras, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(defgroup indiebrain-comment ()
  "Extensions for newcomment.el."
  :group 'comment)

(defcustom indiebrain-comment-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type 'list
  :group 'indiebrain-comment)

;;;###autoload
(defun indiebrain-comment-comment-dwim (arg)
  "Flexible, do-what-I-mean commenting.

If region is active and ARG is either a numeric argument greater
than one or a universal prefix (\\[universal-argument]), then
apply `comment-kill' on all comments in the region.

If the region is active and no ARG is supplied, or is equal to a
numeric prefix of 1, then toggle the comment status of the region.

Else toggle the comment status of the line at point.  With a
numeric prefix ARG, do so for ARGth lines (negative prefix
operates on the lines before point)."
  (interactive "p")
  (cond
   ((and (> arg 1) (use-region-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (num (count-lines beg end)))
      (save-excursion
        (goto-char beg)
        (comment-kill num))))
   ((use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end)))
   (t
    (save-excursion (comment-line (or arg 1))))))

(defvar indiebrain-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun indiebrain-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car indiebrain-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'indiebrain-comment--keyword-hist def)))

;;;###autoload
(defun indiebrain-comment-timestamp-keyword (keyword)
  "Add timestamped comment with KEYWORD.
When called interactively, the list of possible keywords is that
of `indiebrain-comment-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line, the comment is started
there.  Any existing text after the point will not be turned into
a comment but will be pushed to a new line instead.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is formatted as 'DELIMITER KEYWORD DATE:', with the
date being represented as Year-Month-Day."
  (interactive
   (list
    (indiebrain-comment--keyword-prompt indiebrain-comment-comment-keywords)))
  (let ((beg (point))
        (end (gensym))
        (string (format "%s %s: " keyword (format-time-string "%F"))))
    (cond
     ((eq beg (point-at-bol))
      (insert string)
      (setq end (point))
      (comment-region beg end))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

(provide 'indiebrain-comment)
;;; indiebrain-comment.el ends here
