;;; init.el --- Entry point into personal Emacs configuration  -*- lexical-binding: t; -*-

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

;; This is the entry point into my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(defgroup indiebrain-emacs nil
  "User options for my custom Emacs configuration"
  :group 'file)

;; For those who follow my dotfiles and need a way to add their own
;; configuration on top of what's defined in my configuration See:
;; indiebrain-emacs-pre-custom.el and indiebrain-emacs-post-custom.el

(defcustom indiebrain-emacs-load-theme-family 'modus
  "Set of themes to load.
Valid values are the symbols of 'ef', 'modus', and 'standard',
which reference the 'ef-themes', 'modus-themes', and
'standard-themes' respectively.

A nil value does not load any of the above (use Emacs without a theme).

This user option must be set in the
'indiebrain-emacs-pre-custom.el' file. If that file exists in the
Emacs directory, it is loaded before all other modules of my
configuration during Emacs initialization."
  :group 'indiebrain-emacs--expand-file-name
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The 'ef-themes' module" ef)
                 (const :tag "The 'modus-themes' module" modus)
                 (const :tag "The 'standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom indiebrain-emacs-omit-packages nil
  "List of package names to not load.
This instrucs the relevant macros to not 'require' the given
package. In the case of 'indiebrain-emacs-elpa-package', the
package will not be installed if it is not already available on
the system.

This user option must be set in the
'indiebrain-emcas-pre-custom.el' file. If that file exists in the
Emacs directory, it is loaded before all other modules of my
configuration during Emacs initialization.")

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t) ; always start with *scratch*


;;;; Packages

(dolist (path '("indiebrain-lisp" "indiebrain-emacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'package)

;; All variables are for Emacs 28+
(setq package-name-column-width 40)
(setq package-version-column-width 14)
(setq package-status-column-width 12)
(setq package-archive-column-width 8)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("nongnu" . 1)))

(setq custom-safe-themes t)

(defmacro indiebrain-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Ignore PACKAGE if it is a member of `indiebrain-emacs-omit-packages'."
  (declare (indent 1))
  `(progn
     (unless (and (not (memq ,package indiebrain-emacs-omit-packages))
                  (require ,package nil 'noerror))
       (display-warning 'indiebrain-emacs
                        (format "Loading `%s' failed" ,package)
                        :warning))
     ,@body))

(defmacro indiebrain-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Try to install the PACKAGE if it is missing.

Ignore PACKAGE, including the step of installing it, if it is a
member of `indiebrain-emacs-omit-packages'."
  (declare (indent 1))
  `(unless (memq ,package indiebrain-emacs-omit-packages)
     (progn
       (when (not (package-installed-p ,package))
         (unless package-archive-contents
           (package-refresh-contents))
         (package-install ,package))
       (if (require ,package nil 'noerror)
           (progn ,@body)
         (display-warning 'indiebrain-emacs
                          (format "Loading `%s' failed" ,package)
                          :warning)))))

(defvar indiebrain-emacs-package-form-regexp
  "^(\\(indiebrain-emacs-.*-package\\|require\\) +'\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,indiebrain-emacs-package-form-regexp 2)))

;; For those who follow my dotfiles and need a way to add their own
;; configuration on top of what's defined in my configuration. The file
;; must exist at ~/.emacs.d/indiebrain-emacs-pre-custom.el
;;
;; This file allows the user to define their own preferences BEFORE
;; loading any of the modules defined in my configuration. For example,
;; the user option 'indiebrain-emacs-omit-packages' lets the user
;; specify which packages should not be loaded during Emacs
;; initialization. Search for all 'defcustom' forms in this file for
;; other customization options.
(when-let* ((file (locate-user-emacs-file "indiebrain-emacs-pre-custom.el"))
            ((file-exists-p file)))
  (load-file file))

(require 'indiebrain-emacs-essentials)

(pcase indiebrain-emacs-load-theme-family
  ('ef (require 'indiebrain-emacs-ef-themes))
  ('modus (require 'indiebrain-emacs-modus-themes))
  ('stadard (require 'indiebrain-emacs-standard-themes)))

(require 'indiebrain-emacs-theme-extras)
(require 'indiebrain-emacs-font)
(require 'indiebrain-emacs-modeline)
(require 'indiebrain-emacs-completion)
(require 'indiebrain-emacs-search)
(require 'indiebrain-emacs-dired)             ; dired and ibuffer
(require 'indiebrain-emacs-window)
(require 'indiebrain-emacs-vc)               ; git, diff, and related
(require 'indiebrain-emacs-write)             ; denote, logos, etc.
(require 'indiebrain-emacs-org)               ; org, calendar, appt
(require 'indiebrain-emacs-langs)
(require 'indiebrain-emacs-web)               ; eww, elfeed, rcirc
(require 'indiebrain-emacs-conveniences)

(setq safe-local-variable-values
      '((org-hide-leading-stars . t)
        (org-hide-macro-markers . t)))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/user-emacs.el OR ~/.emacs.d/indiebrain-emacs-post-custom.el
;;
;; The purpose of the "post customizations" is to make tweaks to what
;; I already define, such as to change the default theme.  See above
;; for the `indiebrain-emacs-pre-custom.el' to make changes BEFORE loading
;; any of my other configurations.
(when-let* ((file (or (locate-user-emacs-file "user-emacs.el")
                      (locate-user-emacs-file "indiebrain-emacs-post-custom.el")))
            ((file-exists-p file)))
  (load-file file))

;;; init.el ends here
