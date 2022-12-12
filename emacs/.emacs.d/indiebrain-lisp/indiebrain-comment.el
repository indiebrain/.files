;;; indiebrain-comment.el --- Extensions to newcomment.el for my Emacs configuration  -*- lexical-binding: t; -*-

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

;; Extensions to newcomment.el for my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(require 'indiebrain-common)

(defgroup indiebrain-comment ()
  "Extensions for newcomment.el."
  :group 'comment)

(defcustom indiebrain-comment-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type '(repeat string)
  :group 'indiebrain-comment)

(defcustom indiebrain-comment-timestamp-format-concise "%F"
  "Specifier for date in `indiebrain-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :type 'string
  :group 'indiebrain-comment)

(defcustom indiebrain-comment-timestamp-format-verbose "%F %T %z"
  "Like `indiebrain-comment-timestamp-format-concise', but longer."
  :type 'string
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
(defun indiebrain-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.
When called interactively, the list of possible keywords is that
of `indiebrain-comment-comment-keywords', though it is possible to
input arbitrary text.
If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.
If point is anywhere else on the line, the comment is indented
with `comment-indent'.
The comment is always formatted as 'DELIMITER KEYWORD DATE:',
with the date format being controlled by the variable
`indiebrain-comment-timestamp-format-concise'.
With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `indiebrain-comment-timestamp-format-verbose'."
  (interactive
   (list
    (indiebrain-comment--keyword-prompt indiebrain-comment-comment-keywords)
    current-prefix-arg))
  (let* ((date (if verbose
                   indiebrain-comment-timestamp-format-verbose
                 indiebrain-comment-timestamp-format-concise))
         (string (format "%s %s: " keyword (format-time-string date)))
         (beg (point)))
    (cond
     ((or (eq beg (point-at-bol))
          (indiebrain-common-line-regexp-p 'empty))
      (let* ((maybe-newline (unless (indiebrain-common-line-regexp-p 'empty 1) "\n")))
        (insert
         (concat comment-start
                 (make-string
                  (comment-add nil)
                  (string-to-char comment-start))
                 comment-padding
                 string
                 comment-end))
        (indent-region beg (point))
        (when maybe-newline
          (save-excursion (insert maybe-newline)))))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

(provide 'indiebrain-comment)
;;; indiebrain-bookmark.el ends here
