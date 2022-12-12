;;; indiebrain-emacs-modus-themes.el --- modus-themes configuration  -*- lexical-binding: t; -*-

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

;; Configuration for the highly accessible Modus Emacs themes. The modus
;; themes are built into Emacs 28+. For more information about the
;; themes see the manual <https://protesilaos.com/emacs/modus-themes>.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(indiebrain-emacs-elpa-package 'modus-themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-subtle-line-numbers nil
        modus-themes-intense-mouseovers nil
        modus-themes-deuteranopia nil
        modus-themes-tabs-accented nil
        modus-themes-variable-pitch-ui t
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        modus-themes-fringes 'subtle ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers nil

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', a natural number for extra padding (or a cons cell
        ;; of padding and NATNUM), and a floating point for the height of
        ;; the text relative to the base font size (or a cons cell of
        ;; height and FLOAT)
        modus-themes-mode-line nil

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup nil

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax nil

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(intense)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links nil

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat',
        ;; `accented', `faint', `variable-pitch', `underline',
        ;; `all-buttons', the symbol of any font weight as listed in
        ;; `modus-themes-weights', and a floating point number
        ;; (e.g. 0.9) for the height of the button's text.
        modus-themes-box-buttons nil

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(background intense gray)

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'. Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', `text-also', WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'. Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions
        '((matches . (semibold))
          (selection . (extrabold intense accented))
          (popup . (extrabold intense accented)))

        modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(no-extend accented)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs 'bg-only

        modus-themes-org-blocks nil ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch light 1.6))
          (header-date . (underline-today grayscale workaholic 1.2))
          (event . (accented italic varied))
          (scheduled . rainbow)
          (habit . simplified))

        ;; The `modus-themes-headings' is an alist with lots of possible
        ;; combinations, including per-heading-level tweaks: read the
        ;; manual or its doc string.
        modus-themes-headings
        '((0 . (variable-pitch light (height 1.8)))
          (1 . (variable-pitch light (height 1.6)))
          (2 . (rainbow variable-pitch light (height 1.4)))
          (3 . (rainbow variable-pitch regular (height 1.3)))
          (4 . (rainbow regular (height 1.2)))
          (5 . (rainbow (height 1.1)))
          (t . (variable-pitch extrabold))))

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)

  ;; A simple check to load the desired theme at startup based on what
  ;; the global preference for GNOME is. If such preference is not
  ;; registered, it just loads `modus-operandi'. Check my dotfiles for
  ;; the shell script called "delight", which handles system-wide theme
  ;; switching (as I bind the `modus-themes-toggle' to <f5>, delight is
  ;; bound to s-<f5> in the desktop's own custom key bindings).
  (if (string-match-p
       "dark"
       (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme"))
      (modus-themes-load-vivendi)
    (modus-themes-load-operandi))

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(provide 'indiebrain-emacs-modus-themes)

;;; indiebrain-emacs-modus-themes.el ends here