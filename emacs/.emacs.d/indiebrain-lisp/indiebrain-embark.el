;;; indiebrain-embark.el --- extensions to embark.el used in my Emacs configuration  -*- lexical-binding: t; -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for the embark context-sensitive action dispatching
;; package used in my Emacs configuration.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(require 'cl-lib)
(require 'embark nil t)
(require 'indiebrain-common)

(defgroup indiebrain-embark ()
  "Extensions for `embark'."
  :group 'editing)

;;;; Extra keymaps

(autoload 'consult-grep "consult")
(autoload 'consult-line "consult")
(autoload 'consult-imenu "consult")
(autoload 'consult-outline "consult")

(defvar indiebrain-embark-become-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'consult-find)
    (define-key map (kbd "g") 'consult-grep)
    map)
  "General custom cross-package `embark-become' keymap.")

(defvar indiebrain-embark-become-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "s") 'consult-outline) ; as my default is 'M-s M-s'
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)
(autoload 'indiebrain-recentf-recent-files "indiebrain-recentf")
(autoload 'project-switch-to-buffer "project")
(autoload 'project-find-file "project")

(defvar indiebrain-embark-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (define-key map (kbd "r") 'indiebrain-recentf-recent-files)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "F") 'project-find-file)
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode indiebrain-embark-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `indiebrain-embark-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps (list 'indiebrain-embark-become-general-map
                    'indiebrain-embark-become-line-map
                    'indiebrain-embark-become-file+buffer-map)))
    (if indiebrain-embark-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))

;;;; Keycast integration

;; Got this from Embark's wiki. Renamed it to placate the compiler:
;; <https://github.com/oantolin/embark/wiki/Additional-Configuration>.

(defvar keycast--this-command-keys)
(defvar keycast--this-command)

(defun indiebrain-embark--store-action-key+cmd (cmd)
  "Configure keycast variables for keys and CMD.
To be used as filter-return advice to `embark-keymap-prompter'."
  (setq keycast--this-command-keys (this-single-command-keys)
        keycast--this-command cmd))

(advice-add 'embark-keymap-prompter :filter-return #'indiebrain-embark--store-action-key+cmd)

(defun indiebrain-embark--force-keycast-update (&rest _)
  "Update keycast's mode line.
To be passed as advice before `embark-act' and others."
  (force-mode-line-update t))

(autoload 'embark-act "embark")
(autoload 'embark-act-noexit "embark")
(autoload 'embark-become "embark")

;; NOTE: This has a generic name because my plan is to add more packages
;; to it.
;;;###autoload
(define-minor-mode indiebrain-embark-setup-packages
  "Set up advice to integrate Embark with various commands."
  :init-value nil
  :global t
  (if (and indiebrain-embark-setup-packages
           (require 'keycast nil t))
      (dolist (cmd '(embark-act embark-become))
        (advice-add cmd :before #'indiebrain-embark--force-keycast-update))
    (dolist (cmd '(embark-act embark-become))
      (advice-remove cmd #'indiebrain-embark--force-keycast-update))))

(provide 'indiebrain-embark)
;;; indiebrain-embark.el ends here
