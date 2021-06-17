;;; indiebrain-fonts.el --- Font configurations for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021  Aaron Kuehler

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.comy>
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
;; This set of configurations pertains to my font settings, for use in
;; my Emacs setup: <https://github.com/indiebrain/.files/>.

;;; Code:

;;; Customisation options
(defgroup indiebrain-fonts ()
  "Font-related configurations for my dotemacs."
  :group 'font)

;; NOTE: "Hack" and "Iosevka Comfy" are personal builds of Hack and
;; Iosevka respectively:
;;
;; 1. https://gitlab.com/indiebrainesilaos/hack-font-mod
;; 2. https://gitlab.com/indiebrainesilaos/iosevka-comfy
(defcustom indiebrain-fonts-typeface-sets-alist
  '((laptop  . ( :fixed-pitch-family "Hack"
                 :fixed-pitch-regular-weight normal
                 :fixed-pitch-heavy-weight bold
                 :fixed-pitch-height 80
                 :fixed-pitch-line-spacing 1
                 :variable-pitch-family "Helvetica"
                 :variable-pitch-height 1.0
                 :variable-pitch-regular-weight normal))

    (desktop . ( :fixed-pitch-family "Hack"
                 :fixed-pitch-regular-weight normal
                 :fixed-pitch-heavy-weight bold
                 :fixed-pitch-height 100
                 :fixed-pitch-line-spacing 1
                 :variable-pitch-family "Helvetica"
                 :variable-pitch-height 0.9
                 :variable-pitch-regular-weight normal))

    (video   . ( :fixed-pitch-family "Hack"
                 :fixed-pitch-regular-weight light
                 :fixed-pitch-heavy-weight semi-bold
                 :fixed-pitch-height 150
                 :fixed-pitch-line-spacing 1
                 :variable-pitch-family "Helvetica"
                 :variable-pitch-height 1.0
                 :variable-pitch-regular-weight normal)))
  "Alist of desired typeface properties.

The car of each cons cell is an arbitrary key that broadly
describes the display type.  We use 'laptop', 'desktop' though
any symbol will do, e.g. 'video'.

The cdr is a plist that specifies the typographic properties of
fixed-pitch and variable-pitch fonts.  A few notes about those
properties:

- We specify typographic properties both for the `fixed-pitch'
  and `variable-pitch' faces.  This allows us to be explicit
  about all font families that may be used by the active
  theme (Modus themes) under various circumstances (e.g. enabling
  `variable-pitch' for the UI, or using `variable-pitch-mode').

- A semibold weight can only be used by font families that have
  one.  Otherwise it is better to specify bold, in order to avoid
  any potential unpredictable behaviour.

- Never set the :variable-pitch-height to an absolute number
  because that will break the layout of `text-scale-adjust'.  Use
  a floating point instead, so that when the text scale is
  adjusted those expand or contract accordingly.

- An absolute height is only need for the `default' face, which
  we here designated as a fixed-pitch typeface (so the faces
  `fixed-pitch' and `default' share the same font family, though
  their role remains distinct).

- The line height applies to the entirety of the Emacs session.
  We declare it as :fixed-pitch-line-spacing because the face
  `default' starts with a fixed-pitch font family.

- No tests are performed to determined the presence of the font
  families specified herein.  It is assumed that those always
  exist.

It is recommended that the order of the cons cells follows from
the smallest to largest font heights, to simplify the process of
identifying the set that belongs to the small and larger display
respectively (see code of `indiebrain-fonts-laptop-desktop-keys')."
  :group 'indiebrain-fonts
  :type 'alist)

(defun indiebrain-fonts-laptop-desktop-keys ()
  "List laptop and desktop fontsets.
The elements of the list are the cars of the first two cons cells
of `indiebrain-fonts-laptop-desktop-keys-list'"
  (let ((sets (mapcar #'car indiebrain-fonts-typeface-sets-alist)))
    (list (nth 0 sets) (nth 1 sets))))

(defcustom indiebrain-fonts-laptop-desktop-keys-list
  (indiebrain-fonts-laptop-desktop-keys) ; '(laptop desktop)
  "Symbols for `indiebrain-fonts-fonts-per-monitor'.
This is a list whose first item denotes the smallest desirable
entry in `indiebrain-fonts-typeface-sets-alist' for use on a laptop or
just smaller monitor, while the second points to a larger
display's key in that same alist.

The helper function `indiebrain-fonts-laptop-desktop-keys' picks the
first two entries in `indiebrain-fonts-typeface-sets-alist'."
  :group 'indiebrain-fonts
  :type 'list)

(defcustom indiebrain-fonts-max-small-resolution-width 1366
  "Maximum width for use in `indiebrain-fonts-fonts-per-monitor'.
If the screen width is higher than this value (measuring pixels),
then the larger fonts will be used, as specified by the nth 1 of
`indiebrain-fonts-laptop-desktop-keys-list'.  Otherwise the smaller
fonts, else nth 0, are applied."
  :group 'indiebrain-fonts
  :type 'integer)

(defvar indiebrain-fonts-font-display-hist '()
  "History of inputs for display-related font associations.")

(defun indiebrain-fonts--set-face-attribute (face family &optional weight height)
  "Set FACE font to FAMILY, with optional HEIGHT and WEIGHT."
  (let* ((u (if (eq face 'default) 100 1.0))
         (h (or height u))
         (w (or weight 'normal)))
    ;; ;; Read this: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920>
    ;; ;; Hence why the following fails.  Keeping it for posterity...
    ;; (set-face-attribute face nil :family family :weight w :height h)
    (if (eq (face-attribute face :weight) w)
          (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight w 0)
      (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight w 0))
    (internal-set-lisp-face-attribute face :height h 0)))

(defun indiebrain-fonts--set-fonts-prompt ()
  "Promp for font set (used by `indiebrain-fonts-set-fonts')."
  (let ((def (nth 1 indiebrain-fonts-font-display-hist)))
    (completing-read
     (format "Select font set for DISPLAY [%s]: " def)
     (mapcar #'car indiebrain-fonts-typeface-sets-alist)
     nil t nil 'indiebrain-fonts-font-display-hist def)))

(defvar indiebrain-fonts-set-typeface-hook nil
  "Hook that runs after `indiebrain-fonts-set-fonts'.")

(defvar indiebrain-fonts--current-spec nil
  "Current font set in `indiebrain-fonts-typeface-sets-alist'.")

;;;###autoload
(defun indiebrain-fonts-set-fonts (display)
  "Set fonts based on font set assossiated with DISPLAY.

DISPLAY is a symbol that represents the car of a cons cell in
`indiebrain-fonts-typeface-sets-alist'."
  (interactive (list (indiebrain-fonts--set-fonts-prompt)))
  (if window-system
      (let* ((fonts (if (stringp display) (intern display) display))
             (properties (alist-get fonts indiebrain-fonts-typeface-sets-alist))
             (fixed-pitch-family (plist-get properties :fixed-pitch-family))
             (fixed-pitch-height (plist-get properties :fixed-pitch-height))
             (fixed-pitch-regular-weight (plist-get properties :fixed-pitch-regular-weight))
             (fixed-pitch-heavy-weight (plist-get properties :fixed-pitch-heavy-weight))
             (fixed-pitch-line-spacing (plist-get properties :fixed-pitch-line-spacing))
             (variable-pitch-family (plist-get properties :variable-pitch-family))
             (variable-pitch-height (plist-get properties :variable-pitch-height))
             (variable-pitch-regular-weight (plist-get properties :variable-pitch-regular-weight)))
        (indiebrain-fonts--set-face-attribute
         'default fixed-pitch-family fixed-pitch-regular-weight fixed-pitch-height)
        (indiebrain-fonts--set-face-attribute
         'fixed-pitch fixed-pitch-family fixed-pitch-regular-weight)
        (indiebrain-fonts--set-face-attribute
         'variable-pitch variable-pitch-family variable-pitch-regular-weight variable-pitch-height)
        (set-face-attribute 'bold nil :weight fixed-pitch-heavy-weight)
        (setq-default line-spacing fixed-pitch-line-spacing)
        (add-to-history 'indiebrain-fonts-font-display-hist (format "%s" display))
        (setq indiebrain-fonts--current-spec (format "%s" display))
        (run-hooks 'indiebrain-fonts-set-typeface-hook))
    (error "Not running a graphical Emacs; cannot set fonts")))

(defun indiebrain-fonts-restore-last ()
  "Restore last fontset.
This is necessary when/if changes to face specs alter some
typographic properties.  For example, when switching themes the
:weight of the `bold' face will be set to whatever the theme
specifies, typically 'bold', which is not what we always have on
our end."
  (let ((ultimate (nth 0 indiebrain-fonts-font-display-hist))
        (penultimate (nth 1 indiebrain-fonts-font-display-hist)))
    (if (string= ultimate indiebrain-fonts--current-spec)
        (indiebrain-fonts-set-fonts ultimate)
      (indiebrain-fonts-set-fonts penultimate))))

(defun indiebrain-fonts--display-type-for-monitor (&optional smaller larger)
  "Determine typeface specs based on monitor width.
Optional SMALLER and LARGER are two keys that point to entries in
`indiebrain-fonts-typeface-sets-alist'.  The default uses the relevant
keys from `indiebrain-fonts-laptop-desktop-keys-list'."
  (let* ((keys indiebrain-fonts-laptop-desktop-keys-list)
         (face-specs indiebrain-fonts-typeface-sets-alist)
         (small (or smaller (nth 0 keys)))
         (large (or larger (nth 1 keys)))
         (max-width indiebrain-fonts-max-small-resolution-width)
         (spec (if (<= (display-pixel-width) max-width)
                   small
                 large)))
    (unless (assoc spec face-specs)
      (error (concat "Key <<%s>> in `indiebrain-fonts-laptop-desktop-keys-list' "
                     "does not reference anything in "
                     "`indiebrain-fonts-typeface-sets-alist'")
             spec))
    spec))

;;;###autoload
(defun indiebrain-fonts-fonts-per-monitor ()
  "Use font settings based on screen size.
The breakpoint is `indiebrain-fonts-max-small-resolution-width', while
`indiebrain-fonts-laptop-desktop-keys-list' contains the keys of the
two font sets to be used: its first element should point at
smaller fonts than the second element."
  (when window-system
    (let ((display (indiebrain-fonts--display-type-for-monitor)))
      (indiebrain-fonts-set-fonts display))))

(provide 'indiebrain-fonts)
;;; indiebrain-fonts.el ends here
