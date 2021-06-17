;;; indiebrain-minibuffer.el --- Extensions for the minibuffer -*- lexical-binding: t -*-

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
;; Extensions for the minibuffer, intended for my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

;;;; General utilities

(require 'indiebrain-common)

(defgroup indiebrain-minibuffer ()
  "Extensions for the minibuffer."
  :group 'minibuffer)

(defcustom indiebrain-minibuffer-completion-windows-regexp
  "\\*\\(Completions\\|Embark Collect \\(Live\\|Completions\\)\\)"
  "Regexp to match window names with completion candidates.
Used by `indiebrain-minibuffer--get-completion-window'."
  :group 'indiebrain-minibuffer
  :type 'string)

(defcustom indiebrain-minibuffer-mini-cursors nil
  "Allow `cursor-type' to be modified in the minibuffer.
Refer to the source of `indiebrain-minibuffer-mini-cursor' and
`indiebrain-minibuffer-completions-cursor'"
  :group 'indiebrain-minibuffer
  :type 'boolean)

(defcustom indiebrain-minibuffer-remove-shadowed-file-names nil
  "Delete shadowed parts of file names.

For example, if the user types ~/ after a long path name,
everything preceding the ~/ is removed so the interactive
selection process starts again from the user's $HOME.

Only works when variable `file-name-shadow-mode' is non-nil."
  :type 'boolean
  :group 'indiebrain-minibuffer)

(defcustom indiebrain-minibuffer-minimum-input 3
  "Live update completions when input is >= N.

Setting this to a value greater than 1 can help reduce the total
number of candidates that are being computed."
  :type 'integer
  :group 'indiebrain-minibuffer)

(defcustom indiebrain-minibuffer-live-update-delay 0.3
  "Delay in seconds before updating the Completions' buffer.

Set this to 0 to disable the delay."
  :type 'number
  :group 'indiebrain-minibuffer)

(defcustom indiebrain-minibuffer-completion-blocklist nil
  "Commands that do not do live updating of completions.

A less drastic measure is to set `indiebrain-minibuffer-minimum-input'
to an appropriate value.

The Completions' buffer can still be accessed with commands that
put it in a window (e.g. `indiebrain-minibuffer-toggle-completions',
`indiebrain-minibuffer-switch-to-completions-top')."
  :type '(repeat symbol)
  :group 'indiebrain-minibuffer)

(defcustom indiebrain-minibuffer-completion-passlist nil
  "Commands that do live updating of completions from the start.

This means that they ignore `indiebrain-minibuffer-minimum-input' and
the inherent constraint of updating the Completions' buffer only
upon user input.  Furthermore, they also bypass any possible
delay introduced by `indiebrain-minibuffer-live-update-delay'."
  :type '(repeat symbol)
  :group 'indiebrain-minibuffer)

;;;; Minibuffer behaviour

;; Thanks to Omar Antolín Camarena for providing the messageless and
;; stealthily.  Source: <https://github.com/oantolin/emacs-config>.
(defun indiebrain-minibuffer--messageless (fn &rest args)
  "Set `minibuffer-message-timeout' to 0.
Meant as advice around minibuffer completion FN with ARGS."
  (let ((minibuffer-message-timeout 0))
    (apply fn args)))

(dolist (fn '(exit-minibuffer
              choose-completion
              minibuffer-force-complete
              minibuffer-complete-and-exit
              minibuffer-force-complete-and-exit))
  (advice-add fn :around #'indiebrain-minibuffer--messageless))

;; Copied from Daniel Mendler's `vertico' library:
;; <https://github.com/minad/vertico>.
(defun indiebrain-minibuffer--crm-indicator (args)
  "Add prompt indicator to `completing-read-multiple' filter ARGS."
  (cons (concat "[CRM] " (car args)) (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'indiebrain-minibuffer--crm-indicator)

;; Adapted from Omar Antolín Camarena's live-completions library:
;; <https://github.com/oantolin/live-completions>.
(defun indiebrain-minibuffer--honor-inhibit-message (fn &rest args)
  "Skip applying FN to ARGS if `inhibit-message' is t.
Meant as `:around' advice for `minibuffer-message', which does
not honor minibuffer message."
  (unless inhibit-message
    (apply fn args)))

(advice-add #'minibuffer-message :around #'indiebrain-minibuffer--honor-inhibit-message)

;; Note that this solves bug#45686 and is only considered a temporary
;; measure: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45686>
(defun indiebrain-minibuffer--stealthily (fn &rest args)
  "Prevent minibuffer default from counting as a modification.
Meant as advice for FN `minibuf-eldef-setup-minibuffer' with rest
ARGS."
  (let ((inhibit-modification-hooks t))
    (apply fn args)))

(advice-add 'minibuf-eldef-setup-minibuffer :around #'indiebrain-minibuffer--stealthily)

;; Copied from icomplete.el
(defun indiebrain-minibuffer--field-beg ()
  "Determine beginning of completion."
  (if (window-minibuffer-p)
      (minibuffer-prompt-end)
    (nth 0 completion-in-region--data)))

;; Copied from icomplete.el
(defun indiebrain-minibuffer--field-end ()
  "Determine end of completion."
  (if (window-minibuffer-p)
      (point-max)
    (nth 1 completion-in-region--data)))

;; Copied from icomplete.el
(defun indiebrain-minibuffer--completion-category ()
  "Return completion category."
  (let* ((beg (indiebrain-minibuffer--field-beg))
         (md (completion--field-metadata beg)))
    (alist-get 'category (cdr md))))

;; Adapted from icomplete.el
(defun indiebrain-minibuffer--shadow-filenames (&rest _)
  "Hide shadowed file names."
  (let ((saved-point (point)))
    (when (and
           indiebrain-minibuffer-remove-shadowed-file-names
           (eq (indiebrain-minibuffer--completion-category) 'file)
           rfn-eshadow-overlay (overlay-buffer rfn-eshadow-overlay)
           (eq this-command 'self-insert-command)
           (= saved-point (indiebrain-minibuffer--field-end))
           (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
               (eq ?/ (char-before (- (point) 2)))))
      (delete-region (overlay-start rfn-eshadow-overlay)
                     (overlay-end rfn-eshadow-overlay)))))

(defun indiebrain-minibuffer--setup-shadow-files ()
  "Set up shadowed file name deletion.
To be assigned to `minibuffer-setup-hook'."
  (add-hook 'after-change-functions #'indiebrain-minibuffer--shadow-filenames nil t))

(add-hook 'minibuffer-setup-hook #'indiebrain-minibuffer--setup-shadow-files)

;;;; Cursor appearance

(defun indiebrain-minibuffer--cursor-type ()
  "Determine whether `cursor-type' is a list and return value.
If it is a list, this actually returns its car."
  (if (listp cursor-type)
      (car cursor-type)
    cursor-type))

;;;###autoload
(defun indiebrain-minibuffer-mini-cursor ()
  "Local value of `cursor-type' for `minibuffer-setup-hook'."
  (when indiebrain-minibuffer-mini-cursors
    (pcase (indiebrain-minibuffer--cursor-type)
      ('hbar (setq-local cursor-type '(hbar . 8)))
      ('bar (setq-local cursor-type '(hbar . 3)))
      (_  (setq-local cursor-type '(bar . 2))))))

;;;###autoload
(defun indiebrain-minibuffer-completions-cursor ()
  "Local value of `cursor-type' for `completion-list-mode-hook'."
  (when indiebrain-minibuffer-mini-cursors
    (pcase (indiebrain-minibuffer--cursor-type)
      ('hbar (setq-local cursor-type 'box))
      ('bar (setq-local cursor-type '(hbar . 8)))
      (_  (setq-local cursor-type '(bar . 3))))))

;;;; Basic minibuffer interactions

;;;###autoload
(defun indiebrain-minibuffer-focus-minibuffer ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(defun indiebrain-minibuffer--get-completion-window ()
  "Find a live window showing completion candidates."
  (get-window-with-predicate
   (lambda (window)
     (string-match-p
      indiebrain-minibuffer-completion-windows-regexp
      (format "%s" window)))))

(defun indiebrain-minibuffer-focus-mini-or-completions ()
  "Focus the active minibuffer or the completions' window.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`indiebrain-minibuffer-focus-minibuffer' and `switch-to-completions' in
succession.

What constitutes a completions' window is ultimately determined
by `indiebrain-minibuffer-completion-windows-regexp'."
  (interactive)
  (let* ((mini (active-minibuffer-window))
         (completions (indiebrain-minibuffer--get-completion-window)))
    (cond
     ((and mini (not (minibufferp)))
      (select-window mini nil))
     ((and completions (not (eq (selected-window) completions)))
      (select-window completions nil)))))

;; Adaptation of `icomplete-fido-backward-updir'.
;;;###autoload
(defun indiebrain-minibuffer-backward-updir ()
  "Delete char before point or go up a directory.
Must be bound to `minibuffer-local-filename-completion-map'."
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (indiebrain-minibuffer--completion-category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-delete-char)))

;;;; Minibuffer and Completions' buffer intersection
;; NOTE 2021-04-02: The bulk of this code resided in `indiebrain-embark.el'
;; because I was using Embark's live-updating completions' collection
;; buffer.  However, Emacs28 provides a one-column layout for the
;; default Completions' buffer, so it is easy to bring this here and
;; adapt it to work without the otherwise minor Embark extras.

(defface indiebrain-minibuffer-hl-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#b0d8ff" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#103265" :foreground "#ffffff")
    (t :inherit font-lock-string-face))
  "Face for current line in the completions' buffer."
  :group 'indiebrain-minibuffer)

(defface indiebrain-minibuffer-line-number
  '((default :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#f2eff3" :foreground "#252525")
    (((class color) (min-colors 88) (background dark))
     :background "#151823" :foreground "#dddddd")
    (t :inverse-video t))
  "Face for line numbers in the completions' buffer."
  :group 'indiebrain-minibuffer)

(defface indiebrain-minibuffer-line-number-current-line
  '((default :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#8ac7ff" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#142a79" :foreground "#ffffff")
    (t :inverse-video t))
  "Face for current line number in the completions' buffer."
  :group 'indiebrain-minibuffer)

(autoload 'display-line-numbers-mode "display-line-numbers")
(autoload 'face-remap-remove-relative "face-remap")

;;;###autoload
(defun indiebrain-minibuffer-display-line-numbers ()
  "Set up line numbers for the completions' buffer.
Add this to `completion-list-mode-hook'."
  (when (derived-mode-p 'completion-list-mode)
    (face-remap-add-relative 'line-number 'indiebrain-minibuffer-line-number)
    (face-remap-add-relative 'line-number-current-line
                             'indiebrain-minibuffer-line-number-current-line)
    (display-line-numbers-mode 1)))

;;;###autoload
(defun indiebrain-minibuffer-hl-line ()
  "Set up line highlighting for the completions' buffer.
Add this to `completion-list-mode-hook'."
  (when (derived-mode-p 'completion-list-mode)
    (face-remap-add-relative 'hl-line 'indiebrain-minibuffer-hl-line)
    (hl-line-mode 1)))

;; Thanks to Omar Antolín Camarena for recommending the use of
;; `cursor-sensor-functions' and the concomitant hook with
;; `cursor-censor-mode' instead of the dirty hacks I had before to
;; prevent the cursor from moving to that position where no completion
;; candidates could be found at point (e.g. it would break `embark-act'
;; as it could not read the topmost candidate when point was at the
;; beginning of the line, unless the point was moved forward).
(defun indiebrain-minibuffer--clean-completions ()
  "Keep only completion candidates in the Completions."
  (with-current-buffer standard-output
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (delete-region (point-at-bol) (1+ (point-at-eol)))
      (insert (propertize " "
                          'cursor-sensor-functions
                          (list
                           (lambda (_win prev dir)
                             (when (eq dir 'entered)
                               (goto-char prev))))))
      (put-text-property (point-min) (point) 'invisible t))))

(add-hook 'completion-list-mode-hook #'cursor-sensor-mode)
(add-hook 'completion-setup-hook #'indiebrain-minibuffer--clean-completions)

;; This is needed to circumvent what we implement in
;; indiebrain-minibuffer--clean-completions with regard to
;; `cursor-sensor-functions'.
;;;###autoload
(defun indiebrain-minibuffer-beginning-of-buffer ()
  "Go to the top of the Completions buffer."
  (interactive)
  (goto-char (1+ (point-min))))

(defun indiebrain-minibuffer--fit-completions-window ()
  "Fit Completions' buffer to its window."
  (setq-local window-resize-pixelwise t)
  (fit-window-to-buffer (get-buffer-window "*Completions*")
                        (floor (frame-height) 2) 1))

(defun indiebrain-minibuffer--input-string ()
  "Return the contents of the minibuffer as a string."
  (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))

(defun indiebrain-minibuffer--minimum-input ()
  "Test for minimum requisite input for live completions."
  (>= (length (indiebrain-minibuffer--input-string)) indiebrain-minibuffer-minimum-input))

;; Adapted from Omar Antolín Camarena's live-completions library:
;; <https://github.com/oantolin/live-completions>.
(defun indiebrain-minibuffer--live-completions (&rest _)
  "Update the *Completions* buffer.
Meant to be added to `after-change-functions'."
  (when (minibufferp) ; skip if we've exited already
    (let ((while-no-input-ignore-events '(selection-request)))
      (while-no-input
        (if (indiebrain-minibuffer--minimum-input)
            (condition-case nil
                (save-match-data
                  (save-excursion
                    (goto-char (point-max))
                    (let ((inhibit-message t)
                          ;; don't ring the bell in `minibuffer-completion-help'
                          ;; when <= 1 completion exists.
                          (ring-bell-function #'ignore))
                      (minibuffer-completion-help)
                      (indiebrain-minibuffer--fit-completions-window))))
              (quit (abort-recursive-edit)))
          (minibuffer-hide-completions))))))

(defun indiebrain-minibuffer--live-completions-timer (&rest _)
  "Update Completions with `indiebrain-minibuffer-live-update-delay'."
  (let ((delay indiebrain-minibuffer-live-update-delay))
    (when (>= delay 0)
      (run-with-idle-timer delay nil #'indiebrain-minibuffer--live-completions))))

(defun indiebrain-minibuffer--setup-completions ()
  "Set up the completions buffer."
  (cond
   ((member this-command indiebrain-minibuffer-completion-passlist)
    (setq-local indiebrain-minibuffer-minimum-input 0)
    (setq-local indiebrain-minibuffer-live-update-delay 0)
    (minibuffer-completion-help)
    (indiebrain-minibuffer--fit-completions-window)
    (add-hook 'after-change-functions #'indiebrain-minibuffer--live-completions nil t))
   ((unless (member this-command indiebrain-minibuffer-completion-blocklist)
      (add-hook 'after-change-functions #'indiebrain-minibuffer--live-completions-timer nil t)))))

(add-hook 'minibuffer-setup-hook #'indiebrain-minibuffer--setup-completions)

;;;###autoload
(defun indiebrain-minibuffer-toggle-completions ()
  "Toggle the presentation of the completions' buffer."
  (interactive)
  (if (get-buffer-window "*Completions*" 0)
      (minibuffer-hide-completions)
    (minibuffer-completion-help)
    (indiebrain-minibuffer--fit-completions-window)))

;;;###autoload
(defun indiebrain-minibuffer-keyboard-quit-dwim ()
  "Control the exit behaviour for completions' buffers.

If in a completions' buffer and unless the region is active, run
`abort-recursive-edit'.  Otherwise run `keyboard-quit'.

If the region is active, deactivate it.  A second invocation of
this command is then required to abort the session."
  (interactive)
  (when (derived-mode-p 'completion-list-mode)
    (if (use-region-p)
        (keyboard-quit)
      (abort-recursive-edit))))

(defun indiebrain-minibuffer--switch-to-completions ()
  "Subroutine for switching to the completions' buffer."
  (unless (get-buffer-window "*Completions*" 0)
    (minibuffer-completion-help))
  (switch-to-completions)
  (indiebrain-minibuffer--fit-completions-window))

;;;###autoload
(defun indiebrain-minibuffer-switch-to-completions-top ()
  "Switch to the top of the completions' buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (indiebrain-minibuffer--switch-to-completions)
  (goto-char (point-min))
  (next-completion 1))

;;;###autoload
(defun indiebrain-minibuffer-switch-to-completions-bottom ()
  "Switch to the bottom of the completions' buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (indiebrain-minibuffer--switch-to-completions)
  (goto-char (point-max))
  (next-completion -1)
  (goto-char (point-at-bol))
  (recenter
   (- -1
      (min (max 0 scroll-margin)
           (truncate (/ (window-body-height) 4.0))))
   t))

;;;###autoload
(defun indiebrain-minibuffer-next-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction it switches to the
minibuffer."
  (interactive "p")
  (if (or (eobp)
          (eq (point-max)
              (save-excursion (forward-line 1) (point))))
      (indiebrain-minibuffer-focus-minibuffer)
    (next-completion (or arg 1)))
  (setq this-command 'next-line))

;;;###autoload
(defun indiebrain-minibuffer-previous-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction it switches to the
minibuffer."
  (interactive "p")
  (let ((num (indiebrain-common-number-negative arg)))
    (if (or (bobp)
            (eq (point) (1+ (point-min)))) ; see hack in `indiebrain-minibuffer--clean-completions'
        (indiebrain-minibuffer-focus-minibuffer)
      (next-completion (or num 1)))))

;; ;; NOTE 2021-04-07: This was written as a temporary solution to get a
;; ;; copy of the completions' buffer.  It is no longer needed in my
;; ;; setup because Embark's ability to capture a snapshot of the
;; ;; completion candidates works as intended.  It also captures
;; ;; annotations provided by Marginalia and retains the default action
;; ;; attached to each completion candidate.
;; ;;
;; ;; I am keeping this here for posterity.
;; ;; ------------------------------------------------------------------
;; ;; This design is adapted from Omar Antolín Camarena's Embark:
;; ;; <https://github.com/oantolin/embark>.  We need to call the
;; ;; function after aborting the minibuffer, otherwise we cannot get
;; ;; the new window.
;; (defun indiebrain-minibuffer--run-after-abort (fn &rest args)
;;   "Call FN with rest ARGS while aborting recursive edit."
;;   (apply #'run-at-time 0 nil fn args)
;;   (abort-recursive-edit))
;;
;; (defun indiebrain-minibuffer--display-at-bottom (buf-name)
;;   "Display BUF-NAME in bottom window."
;;   (display-buffer-at-bottom
;;    (get-buffer buf-name)
;;    '((window-height . shrink-window-if-larger-than-buffer))))
;;
;;;###autoload
;; (defun indiebrain-minibuffer-save-completions ()
;;   "Save completions in a bespoke buffer."
;;   (interactive)
;;   (let* ((completion (when (active-minibuffer-window)
;;                        (save-excursion
;;                          (indiebrain-minibuffer-focus-minibuffer)
;;                          (indiebrain-minibuffer--input-string))))
;;          (buf-name (format "*%s # Completions*" completion)))
;;     (when (get-buffer buf-name)
;;       (kill-buffer buf-name))
;;     (minibuffer-completion-help)
;;     (with-current-buffer "*Completions*"
;;       (clone-buffer buf-name))
;;     (indiebrain-minibuffer--run-after-abort #'indiebrain-minibuffer--display-at-bottom buf-name)))

;;;###autoload
(defun indiebrain-minibuffer-choose-completion-exit ()
  "Run `choose-completion' in the Completions buffer and exit."
  (interactive)
  (when (and (derived-mode-p 'completion-list-mode)
             (active-minibuffer-window))
    (choose-completion)
    (minibuffer-force-complete-and-exit)))

(defun indiebrain-minibuffer--goto-line (n &optional args)
  "Go to line N in the Completions' with optional ARGS."
  (let ((bounds (count-lines (point-min) (point-max))))
    (if (<= n bounds)
        (progn
          `(,@args)
          (goto-char (point-min))
          (forward-line (1- n))
          (choose-completion))
      (user-error "%d is not within Completions' buffer bounds (%d)" n bounds))))

;;;###autoload
(defun indiebrain-minibuffer-choose-completion-number (n)
  "Select completion candidate on line number N with prefix arg.

The idea is to pass a prefix numeric argument that refers to a
line number in the Completions' buffer."
  (interactive "p")
  (if current-prefix-arg
      (cond
       ((and (derived-mode-p 'completion-list-mode)
             (active-minibuffer-window))
        (indiebrain-minibuffer--goto-line n))
       ((and (minibufferp)
             (indiebrain-minibuffer--get-completion-window))
        (indiebrain-minibuffer--goto-line n (select-window (indiebrain-minibuffer--get-completion-window))))
       (t
        (user-error "Only use this inside the minibuffer of the Completions")))
    (user-error "Pass a numeric prefix argument before calling this command")))

(defvar crm-completion-table)

;;;###autoload
(defun indiebrain-minibuffer-choose-completion-dwim ()
  "Append to minibuffer when at `completing-read-multiple' prompt.
Otherwise behave like `indiebrain-minibuffer-choose-completion-exit'."
  (interactive)
  (when (and (derived-mode-p 'completion-list-mode)
             (active-minibuffer-window))
    (choose-completion)
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (unless (eq (indiebrain-minibuffer--completion-category) 'file)
        (minibuffer-force-complete))
      (when crm-completion-table
        ;; FIXME 2021-04-02: assumes the `crm-separator' as constant.
        ;; UPDATE 2021-04-22: actually `crm-default-separator' is a
        ;; defconst, so I am leaving this here just in case I ever need
        ;; it.  We will have a problem if some command let-binds its own
        ;; value, but it is not our fault here...
        (insert ",")
        (let ((inhibit-message t))
          (switch-to-completions))))))

;;;; Simple actions for the "*Completions*" buffer

;; DEPRECATED: I just use Embark for such tasks, but am keeping this
;; around in case I ever need it.

;; Adapted from `choose-completion'.
(defun indiebrain-minibuffer--completion-at-point ()
  "Find completion candidate at point in the Completions buffer."
  (when (derived-mode-p 'completion-list-mode)
    (let (beg end)
      (cond
       ((and (not (eobp)) (get-text-property (point) 'mouse-face))
        (setq end (point) beg (1+ (point))))
       ((and (not (bobp))
             (get-text-property (1- (point)) 'mouse-face))
        (setq end (1- (point)) beg (point)))
       ;; ((and (bobp)   ; see hack in `indiebrain-minibuffer--clean-completions'
       ;;       (get-text-property (point) 'invisible))
       ;;  (save-excursion
       ;;    (forward-char 1)
       ;;    (setq end (point) beg (1+ (point)))))
       (t (user-error "No completion here")))
      (setq beg (previous-single-property-change beg 'mouse-face))
      (setq end (or (next-single-property-change end 'mouse-face)
                    (point-max)))
      (buffer-substring-no-properties beg end))))

(defmacro indiebrain-minibuffer-completions-buffer-act (name doc &rest body)
  "Produce NAME function with DOC and rest BODY.
This is meant to define some basic commands for use in the
Completions' buffer."
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((completions-buffer (get-buffer "*Completions*"))
           (symbol (indiebrain-minibuffer--completion-at-point)))
       (with-current-buffer completions-buffer
         ,@body))))

(indiebrain-minibuffer-completions-buffer-act
 indiebrain-minibuffer-completions-kill-symbol-at-point
 "Append `symbol-at-point' to the `kill-ring'.
Intended to be used from inside the Completions' buffer."
 (kill-new symbol)
 (message "Copied %s to kill-ring"
          (propertize symbol 'face 'success)))

(indiebrain-minibuffer-completions-buffer-act
 indiebrain-minibuffer-completions-insert-symbol-at-point
 "Add `symbol-at-point' to last active window.
Intended to be used from inside the Completions' buffer."
 (let ((window (window-buffer (get-mru-window))))
   (with-current-buffer window
     (insert symbol)
     (message "Inserted %s"
              (propertize symbol 'face 'success)))))

(indiebrain-minibuffer-completions-buffer-act
 indiebrain-minibuffer-completions-insert-symbol-at-point-exit
 "Add `symbol-at-point' to last window and exit all minibuffers.
Intended to be used from inside the Completions' buffer."
 (let ((window (window-buffer (get-mru-window))))
   (with-current-buffer window
     (insert symbol)
     (message "Inserted %s"
              (propertize symbol 'face 'success))))
 (top-level))

(provide 'indiebrain-minibuffer)
;;; indiebrain-minibuffer.el ends here
