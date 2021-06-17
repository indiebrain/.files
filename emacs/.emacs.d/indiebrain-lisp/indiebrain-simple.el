;;; indiebrain-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

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
;; Common commands for my Emacs:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(defgroup indiebrain-simple ()
  "Generic utilities for my dotemacs."
  :group 'editing)

;; Got those numbers from `string-to-char'
(defcustom indiebrain-simple-insert-pair-alist
  '(("' Single quote"        . (39 39))     ; ' '
    ("\" Double quotes"      . (34 34))     ; " "
    ("` Elisp quote"         . (96 39))     ; ` '
    ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
    ("“ Double apostrophes"  . (8220 8221)) ; “ ”
    ("( Parentheses"         . (40 41))     ; ( )
    ("{ Curly brackets"      . (123 125))   ; { }
    ("[ Square brackets"     . (91 93))     ; [ ]
    ("< Angled brackets"     . (60 62))     ; < >
    ("« Εισαγωγικά Gr quote" . (171 187))   ; « »
    ("= Equals signs"        . (61 61))     ; = =
    ("~ Tilde"               . (126 126))   ; ~ ~
    ("* Asterisks"           . (42 42))     ; * *
    ("_ underscores"         . (95 95)))    ; _ _
  "Alist of pairs for use with `indiebrain-simple-insert-pair-completion'."
  :type 'alist
  :group 'indiebrain-simple)

(defcustom indiebrain-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `indiebrain-simple-inset-date'."
  :type 'string
  :group 'indiebrain-simple)

(defcustom indiebrain-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `indiebrain-simple-inset-date'."
  :type 'string
  :group 'indiebrain-simple)

;;; Commands

;;;; General commands

(autoload 'symbol-at-point "thingatpt")

;;;###autoload
(defun indiebrain-simple-describe-symbol ()
  "Run `describe-symbol' for the `symbol-at-point'."
  (interactive)
  (describe-symbol (symbol-at-point)))

;;;; Comands for lines

;;;###autoload
(defun indiebrain-simple-new-line-below (&optional arg)
  "Create an empty line below the current one.
Move the point to the absolute beginning. Adapt indentation by
passing optional prefix ARG (\\[universal-argument]). Also see
`indiebrain-simple-new-line-above'."
  (interactive "P")
  (end-of-line)
  (if arg
      (newline-and-indent)
    (newline)))

;;;###autoload
(defun indiebrain-simple-new-line-above (&optional arg)
  "Create an empty line above the current one.
Move the point to the absolute beginning. Adapt indentation by
passing optional prefix ARG (\\[universal-argument])."
  (interactive "P")
  (let ((indent (or arg nil)))
    (if (or (bobp)
            (line-number-at-pos (point-min)))
        (progn
          (beginning-of-line)
          (newline)
          (forward-line -1))
      (forward-line -1)
      (indiebrain-simple-new-line-below indent))))

;;;###autoload
(defun indiebrain-simple-copy-line-or-region (&optional arg)
  "Kill-save the current line or active region.
With optional ARG (\\[universal-argument]) duplicate the target
instead. When region is active, also apply context-aware
indentation while duplicating."
  (interactive "P")
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (pbol (point-at-bol))
         (peol (point-at-eol))
         (indent (if (eq (or rbeg rend) pbol) nil arg)))
    (cond
     ((use-region-p)
      (if arg
          (let ((text (buffer-substring rbeg rend)))
            (when (eq (point) rbeg)
              (exchange-point-and-mark))
            (indiebrain-simple-new-line-below indent)
            (insert text))
        (copy-region-as-kill rbeg rend)
        (message "Current region copied")))
     (t
      (if arg
          (let ((text (buffer-substring pbol peol)))
            (goto-char (point-at-eol))
            (newline)
            (insert text))
        (copy-region-as-kill pbol peol)
        (message "Current line copied"))))))

;;;###autoload
(defun indiebrain-simple-yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point-at-bol) (point-at-eol)))
  (yank))

;;;###autoload
(defun indiebrain-simple-multi-line-next ()
  "Move point 15 lines down."
  (interactive)
  (forward-line 15))

;;;###autoload
(defun indiebrain-simple-multi-line-prev ()
  "Move point 15 lines up."
  (interactive)
  (forward-line -15))

;;;###autoload
(defun indiebrain-simple-kill-line-backward ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0))

;;;; Commands for text insertion or manipulation

(defvar indiebrain-simple--character-hist '()
  "History of inputs for `indiebrain-simple-insert-pair-completion'.")

(defun indiebrain-simple--character-prompt (chars)
  "Helper of `indiebrain-simple-insert-pair-completion' to read CHARS."
  (let ((def (car indiebrain-simple--character-hist)))
    (completing-read
     (format "Select character [%s]: " def)
     chars nil t nil 'indiebrain-simple--character-hist def)))

;;;###autoload
(defun indiebrain-simple-insert-pair-completion (&optional num)
  "Insert pair from `indiebrain-simple-insert-pair-alist'.
With optional NUM numeric argument, insert pair to NUMth
constructs. A negative number counts backwards."
  (interactive "p")
  (let* ((data indiebrain-simple-insert-pair-alist)
         (chars (mapcar #'car data))
         (choice (indiebrain-simple--character-prompt chars))
         (left (cadr (assoc choice data)))
         (right (caddr (assoc choice data)))
         (n (or num 1)))
    (insert-pair n left right)))

;;;###autoload
(defun indiebrain-simple-delete-pair-dwim ()
  "Delete pair following or preceding point.
For Emacs version 28 or higher, the feedback's delay is
controlled by `delete-pair-blink-delay'."
  (interactive)
  (if (eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (delete-pair -1)
    (delete-pair 1)))

;;;###autoload
(defun indiebrain-simple-insert-date (&optional arg)
  "Insert the current date as `indiebrain-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `indiebrain-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date indiebrain-simple-date-specifier)
         (time indiebrain-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))


(autoload 'ffap-url-at-point "ffap")
(defvar ffap-string-at-point-region)

;;;###autoload
(defun indiebrain-simple-escape-url ()
  "Wrap URL in angled brackets."
  (interactive)
  (when-let ((url (ffap-url-at-point)))
    (let* ((reg ffap-string-at-point-region)
           (beg (car reg))
           (end (cadr reg))
           (string (if (string-match-p "^mailto:" url)
                       (substring url 7)
                     url)))
      (delete-region beg end)
      (insert (format "<%s>" string)))))

;;;###autoload
(defun indiebrain-simple-cite-region (beg end &optional arg)
  "Cite text in region lines between BEG and END.

Region lines are always understood in absolute terms, regardless
of whether the region boundaries coincide with them.

With optional prefix ARG (\\[universal-argument]) prompt for a
description that will be placed on a new line at the top of the
newly formatted text."
  (interactive "*r\nP")
  (let* ((absolute-beg (if (< beg end)
                           (progn (goto-char beg) (point-at-bol))
                         (progn (goto-char end) (point-at-eol))))
         (absolute-end (if (< beg end)
                           (progn (goto-char end) (point-at-eol))
                         (progn (goto-char beg) (point-at-bol))))
         (prefix-text (if (< beg end)
                          (buffer-substring-no-properties absolute-beg beg)
                        (buffer-substring-no-properties absolute-end end)))
         (prefix (if (string-match-p "\\`[\t\s]+\\'" prefix-text)
                     prefix-text
                   (replace-regexp-in-string "\\`\\([\t\s]+\\).*" "\\1" prefix-text)))
         (description (if arg
                          (format "+----[ %s ]\n"
                                  (read-string "Add description: "))
                        "+----\n"))
         (marked-text (buffer-substring-no-properties absolute-beg absolute-end))
         (marked-text-new (replace-regexp-in-string "^.*?" (concat prefix "|") marked-text))
         (text (with-temp-buffer
                 (insert marked-text-new)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "^\\(^[\s\t]+\\)?.*?")
                   (forward-line -1)
                   (insert (concat prefix description))
                   (goto-char (point-max))
                   (forward-line 1)
                   (insert "\n")
                   (insert (concat prefix "+----")))
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (delete-region absolute-beg absolute-end)
    (insert text)))

;; `indiebrain-simple-insert-undercaret' was offered to me by Gregory
;; Heytings: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45068#250>.
;;;###autoload
(defun indiebrain-simple-insert-undercaret (&optional arg)
  "Draw carets below the characters on the current line or region."
  (interactive "p")
  (let* ((begin (if (region-active-p) (region-beginning) (line-beginning-position)))
         (end (if (region-active-p) (region-end) (line-end-position)))
         (lines (- (line-number-at-pos end) (line-number-at-pos begin) -1))
         (comment (and (/= arg 1) (= lines 1)))
         (final-forward-line -1))
    (goto-char begin)
    (dotimes (i lines)
      (let* ((line-begin (if (zerop i) begin (line-beginning-position)))
             (line-end (if (= (1+ i) lines) end (line-end-position)))
             (begin-column (progn (goto-char line-begin) (current-column)))
             (end-column (progn (goto-char line-end) (current-column)))
             (prefix-begin (line-beginning-position))
             (prefix-end (progn (beginning-of-line-text) (point)))
             (prefix-end-column (progn (goto-char prefix-end) (current-column)))
             (delta (if (< begin-column prefix-end-column) (- prefix-end-column begin-column) 0))
             (prefix-string (buffer-substring-no-properties prefix-begin prefix-end))
             (prefix (if (string-match-p "\\` *\\'" prefix-string) "" prefix-string))
             (whitespace (make-string (- (+ begin-column delta) (string-width prefix)) ?\ ))
             (do-under (< delta (- line-end line-begin)))
             (under (if do-under (make-string (- end-column begin-column delta) ?^) ""))
             (under-string (concat prefix whitespace under "\n")))
        (forward-line 1)
        (if do-under (insert under-string) (setq final-forward-line -2))
        (setq end (+ end (length under-string)))
        (when comment (insert prefix whitespace "\n"))))
    (forward-line final-forward-line)
    (goto-char (line-end-position))))

;;;; Commands for object transposition

(defmacro indiebrain-simple-transpose (name scope &optional doc)
  "Macro to produce transposition functions.
NAME is the function's symbol. SCOPE is the text object to
operate on. Optional DOC is the function's docstring.

Transposition over an active region will swap the object at
mark (region beginning) with the one at point (region end)"
  `(defun ,name (arg)
     ,doc
     (interactive "p")
     (let ((x (format "%s-%s" "transpose" ,scope)))
       (if (use-region-p)
           (funcall (intern x) 0)
         (funcall (intern x) arg)))))

(indiebrain-simple-transpose
 indiebrain-simple-transpose-lines
 "lines"
 "Transpose lines or swap over active region.")

(indiebrain-simple-transpose
 indiebrain-simple-transpose-paragraphs
 "paragraphs"
 "Transpose paragraphs or swap over active region.")

(indiebrain-simple-transpose
 indiebrain-simple-transpose-sentences
 "sentences"
 "Transpose sentences or swap over active region.")

(indiebrain-simple-transpose
 indiebrain-simple-transpose-sexps
 "sexps"
 "Transpose balanced expressions or swap over active region.")

;;;###autoload
(defun indiebrain-simple-transpose-chars ()
  "Always transposes the two characters before point.
There is no 'dragging' the character forward. This is the
behaviour of `transpose-chars' when point is at the end of the
line."
  (interactive)
  (transpose-chars -1)
  (forward-char))

;;;###autoload
(defun indiebrain-simple-transpose-words (arg)
  "Transpose ARG words.

If region is active, swap the word at mark (region beginning)
with the one at point (region end).

Otherwise, and while inside a sentence, this behaves as the
built-in `transpose-words', dragging forward the word behind the
point. The difference lies in its behaviour at the end or
beginnning of a line, where it will always transpose the word at
point with the one behind or ahead of it (effectively the
last/first two words)."
  (interactive "p")
  (cond
   ((use-region-p)
    (transpose-words 0))
   ((eq (point) (point-at-eol))
    (transpose-words -1))
   ((eq (point) (point-at-bol))
    (forward-word 1)
    (transpose-words 1))
   (t
    (transpose-words arg))))

;;;; Commands for marking syntactic constructs

(defmacro indiebrain-simple-mark (name object &optional docstring)
  "Produce function for marking small syntactic constructs.
NAME is how the function should be called. OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
  `(defun ,name (&optional arg allow-extend)
     ,docstring
     (interactive "P\np")
     (let ((x (format "%s-%s" "forward" ,object)))
       (cond ((and allow-extend
                   (or (and (eq last-command this-command) (mark t))
                       (region-active-p)))
              (setq arg (if arg (prefix-numeric-value arg)
                          (if (< (mark) (point)) -1 1)))
              (set-mark
               (save-excursion
                 (goto-char (mark))
                 (funcall (intern x) arg)
                 (point))))
             (t
              (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                (unless (consp bounds)
                  (user-error "No %s at point" ,object))
                (if (>= (prefix-numeric-value arg) 0)
                    (goto-char (car bounds))
                  (goto-char (cdr bounds)))
                (push-mark
                 (save-excursion
                   (funcall (intern x) (prefix-numeric-value arg))
                   (point)))
                (activate-mark)))))))

(indiebrain-simple-mark
 indiebrain-simple-mark-word
 "word"
 "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required. For a general purpose utility, use `indiebrain-simple-mark-symbol'
instead.")

(indiebrain-simple-mark
 indiebrain-simple-mark-symbol
 "symbol"
 "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point. A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

;;;###autoload
(defun indiebrain-simple-mark-sexp-backward (&optional arg)
  "Mark previous or ARGth balanced expression[s].
Just a convenient backward-looking `mark-sexp'."
  (interactive "P")
  (if arg
      (mark-sexp (- arg) t)
    (mark-sexp (- 1) t)))

;;;###autoload
(defun indiebrain-simple-mark-construct-dwim (&optional arg)
  "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `indiebrain-simple-mark-sexp-backward',
`mark-sexp', and `indiebrain-simple-mark-symbol'.

When point is over a symbol, mark the entirety of it. Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter. A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
the current one (so 3 would be 1+2 more). A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `indiebrain-simple-mark-sexp-backward', so be mindful of
where the point is). Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
  (interactive "P")
  (cond
   ((symbol-at-point)
    (indiebrain-simple-mark-symbol arg t))
   ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
    (indiebrain-simple-mark-sexp-backward arg))
   (t
    (mark-sexp arg t))))

;;;; Commands for code navigation (work in progress)

;;;###autoload
(defun indiebrain-simple-downward-list (&optional arg)
  "Like `backward-up-list' but defaults to a forward motion.
With optional ARG, move that many times in the given
direction (negative is forward due to this being a
'backward'-facing command)."
  (interactive "P")
  (backward-up-list (or arg -1)))

;;;; Commands for paragraphs

(defvar-local indiebrain-simple--auto-fill-cycle-state 1
  "Representation of `indiebrain-simple-auto-fill-cycle' state.")

;; Based on gungadin-cylocal.el (private communication with Christopher
;; Dimech---disclosed with permission).
;;;###autoload
(defun indiebrain-simple-auto-fill-cycle ()
  "Cycles auto fill for comments, everything, nothing."
  (interactive)
  (let ((n indiebrain-simple--auto-fill-cycle-state))
    (pcase n
      (2
       (message "Auto fill %s" (propertize "buffer" 'face 'warning))
       (setq-local comment-auto-fill-only-comments nil)
       (setq-local indiebrain-simple--auto-fill-cycle-state (1+ n)))
      (3
       (message "Disable auto fill")
       (auto-fill-mode 0)
       (setq-local indiebrain-simple--auto-fill-cycle-state (1+ n)))
      (_
       (message "Auto fill %s" (propertize "comments" 'face 'success))
       (setq-local comment-auto-fill-only-comments t)
       (auto-fill-mode 1)
       (setq-local indiebrain-simple--auto-fill-cycle-state 2)))))

;;;###autoload
(defun indiebrain-simple-unfill-region-or-paragraph (&optional beg end)
  "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and END, if active,
while respecting any empty lines (so multiple paragraphs are not
joined, just unfilled). If no region is active, operate on the
paragraph. The idea is to produce the opposite effect of both
`fill-paragraph' and `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph))))

;;;; Commands for windows

;; Inspired by Pierre Neidhardt's windower:
;; https://gitlab.com/ambrevar/emacs-windower/-/blob/master/windower.el
(defvar indiebrain-simple--windows-current nil
  "Current window configuration.")

;;;###autoload
(define-minor-mode indiebrain-simple-monocle
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window. Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  :lighter " -M-"
  :global nil
  (let ((win indiebrain-simple--windows-current))
    (if (one-window-p)
        (when win
          (set-window-configuration win))
      (setq indiebrain-simple--windows-current (current-window-configuration))
      (delete-other-windows))))

(defun indiebrain-simple--monocle-disable ()
  "Set variable `indiebrain-simple-monocle' to nil, when appropriate.
To be hooked to `window-configuration-change-hook'."
  (when (and indiebrain-simple-monocle (not (one-window-p)))
    (delete-other-windows)
    (indiebrain-simple-monocle -1)
    (set-window-configuration indiebrain-simple--windows-current)))

(add-hook 'window-configuration-change-hook #'indiebrain-simple--monocle-disable)

;;;; Commands for buffers

;;;###autoload
(defun indiebrain-simple-kill-buffer-current (&optional arg)
  "Kill current buffer or abort recursion when in minibuffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well."
  (interactive "P")
  (if (minibufferp)
      (abort-recursive-edit)
    (kill-buffer (current-buffer)))
  (when (and arg
             (not (one-window-p)))
    (delete-window)))

;;;###autoload
(defun indiebrain-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let* ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

(provide 'indiebrain-simple)
;;; indiebrain-simple.el ends here
