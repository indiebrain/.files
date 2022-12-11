;;; indiebrain-emacs-font.el --- Fonts and typefaces configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2022  Aaron Kuehler <aaron.kuehler@gmail.com>

;; Author: Aaron <aaron.kuehler@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

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

;; Extensions and configuration for fonts and typefaces
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(indiebrain-emacs-elpa-package 'fontaine
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is a highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
  ;;
  ;; Iosevka Comfy            == monospaced, supports ligatures
  ;; Iosevka Comfy Fixed      == monospaced, no ligatures
  ;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
  ;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
  ;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
  ;; Iosevka Comfy Motion     == monospaced, supports ligatures, fancier glyphs
  ;; Iosevka Comfy Motion Duo == as above, but quasi-proportional
  (setq fontaine-presets
        '((small
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 80
           :variable-pitch-family "Iosevka Comfy Wide Duo")
          (regular
           :default-height 100)
          (large
           :default-weight semilight
           :default-height 140
           :bold-weight extrabold)
          (code-demo
           :default-family "Iosevka Comfy Fixed"
           :default-weight semilight
           :default-height 170
           :variable-pitch-family "Iosevka Comfy Duo"
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; Persist font configurations while switching themes (doing it with
  ;; the `modus-themes' and `ef-themes' via the hooks they provide).
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset))

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font))

;;; `variable-pitch-mode' setup

(define-key ctl-x-x-map (kbd "v") #'variable-pitch-mode)

(defun indiebrain/enable-variable-pitch ()
  (unless (or (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
              (member (buffer-name) '("*Colors*" "*Faces*" "*Quick Help*")))
    (variable-pitch-mode 1)))

(defvar indiebrain/enable-variable-pitch-in-hooks
  '(text-mode-hook
    help-mode-hook
    notmuch-show-mode-hook
    elfeed-show-mode-hook)
  "List of hook symbols to add `indiebrain/enable-variable-pitch' to.")

(dolist (hook indiebrain/enable-variable-pitch-in-hooks)
  (add-hook hook #'indiebrain/enable-variable-pitch))

(provide 'indiebrain-emacs-font)
;;; indiebrain-emacs-font.el ends here
