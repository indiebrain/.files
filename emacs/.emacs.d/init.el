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
Valid values are the symbols of 'ef', 'modus', 'nord', and 'standard',
which reference the 'ef-themes', 'modus-themes', 'nord-themes', and
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
                 (const :tag "The 'nord-themes' module" nord)
                 (const :tag "The 'standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom indiebrain-emacs-synchronized-directory
  "~/Sync"
  "A path which can be used to synchronize filesystem items."
  :group 'indiebrain-emacs--expand-file-name
  :type 'string)

(defcustom indiebrain-emacs-omit-packages nil
  "List of package names to not load.
This instructs the relevant macros to not `require' the given
package. In the case of `indiebrain-emacs-package', the package
will not be installed if it is not already available on the
system.

This user option must be set in the `indiebrain-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'indiebrain-emacs
  :type '(repeat symbol))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
(setq native-compile-prune-cache t) ; Emacs 29
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Ignore host-specific customization options.
(setq custom-file (make-temp-file "emacs-custom-"))

(put 'overwrite-mode 'disabled t)

;; Always start with *scratch*
(setq initial-buffer-choice t)


;;;; Packages

(dolist (path '("indiebrain-lisp" "indiebrain-emacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'package)

(setq package-quickstart t)
(setq package-vc-register-as-project nil)

(package-initialize)

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("melpa" . 1)))

(setq custom-safe-themes t)

(defun indiebrain-emacs-package-install (package &optional method)
  "Install PACKAGE with optional METHOD.

If METHOD is nil or the `builtin' symbol, PACKAGE is not
installed as it is considered part of Emacs.

If METHOD is a string, it must be a URL pointing to the version
controlled repository of PACKAGE.  Installation is done with
`package-vc-install'.

If METHOD is a quoted list, it must have a form accepted by
`package-vc-install' such as:

\\='(denote :url \"https://git.sr.ht/~protesilaos/denote\" :branch \"main\")

If METHOD is any other non-nil value, install PACKAGE using
`package-install'."
  (unless (or (eq method 'builtin) (null method))
    (unless (package-installed-p package)
      (when (or (stringp method) (listp method))
        (package-vc-install method))
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package))))

(defvar indiebrain-emacs-loaded-packages nil)

(defmacro indiebrain-emacs-package (package &rest body)
  "Require PACKAGE with BODY configurations.

PACKAGE is an unquoted symbol that is passed to `require'.  It
thus conforms with `featurep'.

BODY consists of ordinary Lisp expressions.  There are,
nevertheless, two unquoted plists that are treated specially:

1. (:install METHOD)
2. (:delay NUMBER)

These plists can be anywhere in BODY and are not part of its
final expansion.

The :install property is the argument passed to
`indiebrain-emacs-package-install' and has the meaning of METHOD
described therein.

The :delay property makes the evaluation of PACKAGE with the
expanded BODY happen with `run-with-timer'.

Also see `indiebrain-emacs-configure'."
  (declare (indent 1))
  (unless (memq package indiebrain-emacs-omit-packages)
    (let (install delay)
      (dolist (element body)
        (when (plistp element)
          (pcase (car element)
            (:install (setq install (cdr element)
                            body (delq element body)))
            (:delay (setq delay (cadr element)
                          body (delq element body))))))
      (let ((common `(,(when install
                         `(indiebrain-emacs-package-install ',package ,@install))
                      (require ',package)
                      (add-to-list 'indiebrain-emacs-loaded-packages ',package)
                      ,@body
                      ;; (message "Prot Emacs loaded package: %s" ',package)
                      )))
        (cond
         ((featurep package)
          `(progn ,@body))
         (delay
          `(run-with-timer ,delay nil (lambda () ,@(delq nil common))))
         (t
          `(progn ,@(delq nil common))))))))

;; Samples of `indiebrain-emacs-package' (expand them with `pp-macroexpand-last-sexp').

;; (indiebrain-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install '(denote . (:url "https://git.sr.ht/~protesilaos/denote" :branch "main")))
;;   (:delay 5)
;;   (setq denote-file-type nil))
;;
;; (indiebrain-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (:delay 5)
;;   (setq denote-file-type nil))
;;
;; (indiebrain-emacs-package denote
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (setq denote-file-type nil))
;;
;; (indiebrain-emacs-package denote
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))
;;
;; (indiebrain-emacs-package denote
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))
;;
;; (indiebrain-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))

(defmacro indiebrain-emacs-configure (&rest body)
  "Evaluate BODY as a `progn'.
BODY consists of ordinary Lisp expressions.  The sole exception
is an unquoted plist of the form (:delay NUMBER) which evaluates
BODY with NUMBER seconds of `run-with-timer'.

Note that `indiebrain-emacs-configure' does not try to autoload
anything.  Use it only for forms that evaluate regardless.

Also see `indiebrain-emacs-package'."
  (declare (indent 0))
  (let (delay)
    (dolist (element body)
      (when (plistp element)
        (pcase (car element)
          (:delay (setq delay (cadr element)
                        body (delq element body))))))
    (if delay
        `(run-with-timer ,delay nil (lambda () ,@body))
      `(progn ,@body))))

(defvar indiebrain-emacs-package-form-regexp
  "^(\\(indiebrain-emacs-.*-package\\|require\\) +'\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,indiebrain-emacs-package-form-regexp 2)))

(defmacro indiebrain-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (unless (and (null (car pair))
                         (null (cdr pair)))
              `(define-key map (kbd ,(car pair)) ,(cdr pair))))
          (cl-mapcar #'cons keys commands)))))

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
  ('nord (require 'indiebrain-emacs-nord-themes))
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
