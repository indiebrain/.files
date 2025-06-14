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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions and configuration for fonts and typefaces
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(indiebrain-emacs-package fontaine
  (:install t)
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is a highly customized build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://github.com/indiebrain/iosevka-comfy>.
  ;;
  ;; | Family                          | Shapes | Spacing | Style      | Ligatures |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy                   | Sans   | Compact | Monospaced | Yes       |
  ;; | Iosevka Comfy Fixed             | Sans   | Compact | Monospaced | No        |
  ;; | Iosevka Comfy Duo               | Sans   | Compact | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Motion            | Slab   | Compact | Monospaced | Yes       |
  ;; | Iosevka Comfy Motion Fixed      | Slab   | Compact | Monospaced | No        |
  ;; | Iosevka Comfy Motion Duo        | Slab   | Compact | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Wide              | Sans   | Wide    | Monospaced | Yes       |
  ;; | Iosevka Comfy Wide Fixed        | Sans   | Wide    | Monospaced | No        |
  ;; | Iosevka Comfy Wide Duo          | Sans   | Wide    | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Wide Motion       | Slab   | Wide    | Monospaced | Yes       |
  ;; | Iosevka Comfy Wide Motion Fixed | Slab   | Wide    | Monospaced | No        |
  ;; | Iosevka Comfy Wide Motion Duo   | Slab   | Wide    | Duospaced  | Yes       |
  (setq fontaine-presets
        '((small
           :default-family "Iosevka Comfy Motion"
           :default-height 80
           :variable-pitch-family "Iosevka Comfy Duo")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 120
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (presentation
           :inherit medium
           :default-weight light
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 120
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

  (indiebrain-emacs-keybind global-map
    "C-c f" #'fontaine-set-preset
    "C-c F" #'fontaine-set-face-font))

;;; `variable-pitch-mode' setup

(indiebrain-emacs-keybind ctl-x-x-map
  "v" #'variable-pitch-mode)

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
