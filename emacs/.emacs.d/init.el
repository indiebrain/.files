;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2012-2021  Aaron Kuehler <aaron.kuehler+public@gmail.com>

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.com>
;; URL: https://github.com/indiebrain/.files/
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file sets up the essentials for incorporating the Org mode files
;; which contains my Emacs configuration - indiebrain-emacs.org.
;; indiebrain-emacs.org follows what is known as "literate programming" -
;; more on literate programming in the indiebrain-emacs.org files. This
;; executable documentation is particularly suitable for sharing my
;; Emacs configuration with others - future versions of myself included.
;;
;; See my dotfiles: <https://github.com/indiebrain/.files/>.

;;; Code:

(require 'package)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; In addition to the GNU repository look for external packages in
;; these repositories.
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("non-gnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(defvar indiebrain-emacs-autoinstall-elpa nil
  "Whether `indiebrain-emacs-elpa-package' should install packages.
The default nil value means never to automatically install
packages. A non-nil value is always interpreted as consent for
auto-installing everything---this process does not cover manually
maintained git repositories, controlled by
`indiebrain-emacs-manual-package'.")

(defvar indiebrain-emacs-basic-init "basic-init.el"
  "Name of 'basic init' file.

This file is meant to store user configurations that are
evaluated before loading `indiebrain-emacs-configuration-main-file'
and, when available, `indiebrain-emacs-configuration-user-file'. Those
values control the behavior of the Emacs setup.

The only variable that is currently expected to be in the 'basic
init' file is `indiebrain-emacs-autoinstall-elpa'.

See `indiebrain-emacs-basic-init-setup' for the actual initialization
process.")

(defun indiebrain-emacs-basic-init-setup ()
  "Load 'basic-init.el' if it exists.
This is meant to evaluate forms that control the rest of my Emacs
setup."
  (let* ((init indiebrain-emacs-basic-init)
         (file (locate-user-emacs-file init)))
    (when (file-exists-p file)
      (load-file file))))

;; indiebrain-emacs.org appends to this list. The idea is to produce a
;; list of packages that we want to install on demand from an ELPA, when
;; `indiebrain-emacs-autoinstall-elpa' is set to nil (the default).
;;
;; So someone who tries to reproduce my Emacs setup will first get a
;; bunch of warnings about unavailable packages, though not
;; show-stopping errors, and will then have to use the command
;; `indiebrain-emacs-install-ensured'. After that command does its job,
;; a re-run of my Emacs configurations will yield the expected results.
;;
;; The assumption is that such a user will want to inspect the elements
;; of `indiebrain-emacs-ensure-install', remove from the setup whatever
;; code block they do not want, and then call the aforementioned
;; command.
;;
;; I do not want to maintain a setup that auto-installs everything on
;; first boot without requiring explicit consent. I think that is a bad
;; practice because it teaches the user to simply put their faith in the
;; provider.
(defvar indiebrain-emacs-ensure-install nil
  "List of package names used by `indiebrain-emacs-install-ensured'.")

(defun indiebrain-emacs-install-ensured ()
  "Install all `indiebrain-emacs-ensure-install' packages, if needed.
If a package is already installed, no further action is performed
on it."
  (interactive)
  (when (yes-or-no-p (format "Try to install %d packages?"
                             (length indiebrain-emacs-ensure-install)))
    (package-refresh-contents)
    (mapc (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          indiebrain-emacs-ensure-install)))

(defmacro indiebrain-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'indiebrain-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro indiebrain-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

When `indiebrain-emacs-autoinstall-elpa' is non-nil try to install the
package if it is missing."
  (declare (indent 1))
  `(progn
     (when (and indiebrain-emacs-autoinstall-elpa
                (not (package-installed-p ,package)))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'indiebrain-emacs (format "Loading `%s' failed" ,package) :warning)
       (add-to-list 'indiebrain-emacs-ensure-install ,package)
       (display-warning
        'indiebrain-emacs
        (format "Run `indiebrain-emacs-install-ensured' to install all packages in `indiebrain-emacs-ensure-install'")
        :warning))))

(defmacro indiebrain-emacs-manual-package (package &rest body)
  "Set up manually installed PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  (let ((path (thread-last user-emacs-directory
                (expand-file-name "contrib-lisp")
                (expand-file-name (symbol-name (eval package))))))
    `(progn
       (eval-and-compile
         (add-to-list 'load-path ,path))
       (if (require ,package nil 'noerror)
           (progn ,@body)
         (display-warning 'indiebrain-emacs (format "Loading `%s' failed" ,package) :warning)
         (display-warning 'indiebrain-emacs (format "This must be available at %s" ,path) :warning)))))

(require 'vc)
(setq vc-follow-symlinks t) ; Because my dotfiles are managed that way

;; "indiebrain-lisp" is for all my custom libraries; "contrib-lisp" is for
;; third-party code that I handle manually.
(dolist (path '("indiebrain-lisp" "contrib-lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq default-input-method nil)
(setq ring-bell-function 'ignore)
(setq default-directory "~/")

(setq use-short-answers t)    ; for Emacs28, replaces the defalias below
;; (defalias 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t)			; always start with *scratch*

;; I create an "el" version of my Org configuration file as a final step
;; before closing down Emacs (see further below).  This is done to load
;; the latest version of my code upon startup.  Also helps with
;; initialisation times.  Not that I care too much about those...

(defvar indiebrain-emacs-configuration-main-file "indiebrain-emacs"
  "Base name of the main configuration file.")

;; THIS IS EXPERIMENTAL.  Basically I want to test how we can let users
;; include their own customisations in addition to my own.  Those will
;; be stored in a separate Org file.
(defvar indiebrain-emacs-configuration-user-file "user-emacs"
  "Base name of user-specific configuration file.")

(defun indiebrain-emacs--expand-file-name (file extension)
  "Return canonical path to FILE to Emacs config with EXTENSION."
  (locate-user-emacs-file
   (concat file extension)))

(defun indiebrain-emacs-load-config ()
  "Load main Emacs configurations, either '.el' or '.org' file."
  (let* ((main-init indiebrain-emacs-configuration-main-file)
         (main-init-el (indiebrain-emacs--expand-file-name main-init ".el"))
         (main-init-org (indiebrain-emacs--expand-file-name main-init ".org"))
         (user-init indiebrain-emacs-configuration-user-file)
         (user-init-el (indiebrain-emacs--expand-file-name user-init ".el"))
         (user-init-org (indiebrain-emacs--expand-file-name user-init ".org")))
    (indiebrain-emacs-basic-init-setup)
    (require 'org)
    (if (file-exists-p main-init-el)    ; FIXME 2021-02-16: this should be improved
        (load-file main-init-el)
      (when (file-exists-p main-init-org)
        (org-babel-load-file main-init-org)))
    (if (file-exists-p user-init-el)
        (load-file user-init-el)
      (when (file-exists-p user-init-org)
        (org-babel-load-file user-init-org)))))

;; Load configurations.
(indiebrain-emacs-load-config)

;; The following is for when we close the Emacs session.
(declare-function org-babel-tangle-file "ob-tangle")

(defun indiebrain-emacs-build-config ()
  "Produce Elisp init from my Org dotemacs.
Add this to `kill-emacs-hook', to use the newest file in the next
session.  The idea is to reduce startup time, though just by
rolling it over to the end of a session rather than the beginning
of it."
  (let* ((main-init indiebrain-emacs-configuration-main-file)
         (main-init-el (indiebrain-emacs--expand-file-name main-init ".el"))
         (main-init-org (indiebrain-emacs--expand-file-name main-init ".org"))
         (user-init indiebrain-emacs-configuration-user-file)
         (user-init-el (indiebrain-emacs--expand-file-name user-init ".el"))
         (user-init-org (indiebrain-emacs--expand-file-name user-init ".org")))
    (when (file-exists-p main-init-el)
      (delete-file main-init-el))
    (when (file-exists-p user-init-el)
      (delete-file user-init-el))
    (require 'org)
    (when (file-exists-p main-init-org)
      (org-babel-tangle-file main-init-org main-init-el)
      (byte-compile-file main-init-el))
    (when (file-exists-p user-init-org)
      (org-babel-tangle-file user-init-org user-init-el)
      (byte-compile-file user-init-el))))

(add-hook 'kill-emacs-hook #'indiebrain-emacs-build-config)
;;; init.el ends here
