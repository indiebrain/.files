;;; indiebrain-logos.el --- Extensions for my dotemacs to help read, write, present -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021  Aaron Kuehler

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.com>
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
;; Extensions to help me read, write, present. For use in my Emacs
;; setup: <https://github.com/indiebrain/.files/>.

;;; Code:

(defgroup indiebrain-logos ()
  "Setup for reading and presenting text-heavy buffers."
  :group 'files)

(defcustom indiebrain-logos-org-presentation nil
  "Org files should switch to presentation view.
This concerns cases where variable `indiebrain-logos-focus-mode' is set
to non-nil and determines whether headings should be converted
into pseudo slides and indentation be adjusted accordingly."
  :type 'boolean
  :group 'indiebrain-logos)

(defcustom indiebrain-logos-variable-pitch nil
  "Non-programming buffers should switch to `variable-pitch-mode'.
In programming modes the default font is always used, as that is
assumed to be a monospaced typeface."
  :type 'boolean
  :group 'indiebrain-logos)

(defcustom indiebrain-logos-scroll-lock nil
  "Use centred scrolling while in focused view."
  :type 'boolean
  :group 'indiebrain-logos)

(defcustom indiebrain-logos-hidden-modeline nil
  "Hide the modeline."
  :type 'boolean
  :group 'indiebrain-logos)

(defvar indiebrain-logos--focus-mode-hook nil
  "Hook that runs from function `indiebrain-logos-focus-mode'.")

;;;###autoload
(define-minor-mode indiebrain-logos-focus-mode
  "Buffer-local wrapper mode for presentations.
Other tools should hook into `indiebrain-logos--focus-mode-hook' to
introduce their effects.  Otherwise this minor mode has no effect
on its own."
  :init-value nil
  :global nil
  :lighter " -Λ-"           ; greek lambda majuscule
  (run-hooks 'indiebrain-logos--focus-mode-hook))

(autoload 'buffer-face-mode "face-remap")
(autoload 'variable-pitch-mode "face-remap")

(defun indiebrain-logos--variable-pitch-toggle ()
  "Make text use `variable-pitch' face, except for programming."
  (when (and indiebrain-logos-variable-pitch
             (derived-mode-p 'text-mode))
    (if (or (bound-and-true-p buffer-face-mode)
            (not (bound-and-true-p indiebrain-logos-focus-mode)))
        (variable-pitch-mode -1)
      (variable-pitch-mode 1))))

(add-hook 'indiebrain-logos--focus-mode-hook #'indiebrain-logos--variable-pitch-toggle)

(autoload 'olivetti-mode "olivetti")

(defun indiebrain-logos--olivetti-toggle ()
  "Toggle the variable `olivetti-mode', if available."
  (if (or (bound-and-true-p olivetti-mode)
          (not (bound-and-true-p indiebrain-logos-focus-mode)))
      (olivetti-mode -1)
    (olivetti-mode 1)))

(add-hook 'indiebrain-logos--focus-mode-hook #'indiebrain-logos--olivetti-toggle)

(defun indiebrain-logos--fringe-toggle ()
  "Toggle fringe width."
  (if (or (= (car (window-fringes)) 0)
          (not (bound-and-true-p indiebrain-logos-focus-mode)))
      (set-window-fringes (selected-window) nil)
    (set-window-fringes (selected-window) 0 0)))

(add-hook 'indiebrain-logos--focus-mode-hook #'indiebrain-logos--fringe-toggle)

(autoload 'org-tree-slide-mode "org-tree-slide")

(defun indiebrain-logos--org-tree-slide-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and indiebrain-logos-org-presentation
               (eq mode 'org-mode))
      (if (or (bound-and-true-p org-tree-slide-mode)
              (not (bound-and-true-p indiebrain-logos-focus-mode)))
          (org-tree-slide-mode -1)
        (org-tree-slide-mode 1)))))

(add-hook 'indiebrain-logos--focus-mode-hook #'indiebrain-logos--org-tree-slide-mode)

(autoload 'org-indent-mode "org")

(defun indiebrain-logos--org-indent-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and indiebrain-logos-org-presentation
               (eq mode 'org-mode))
      (if (or (bound-and-true-p org-indent-mode)
              (not (bound-and-true-p indiebrain-logos-focus-mode)))
          (org-indent-mode -1)
        (org-indent-mode 1)))))

(add-hook 'indiebrain-logos--focus-mode-hook #'indiebrain-logos--org-indent-mode)

(defun indiebrain-logos--scroll-lock ()
  "Keep the point at the centre."
  (when indiebrain-logos-scroll-lock
    (if (or (bound-and-true-p scroll-lock-mode)
            (not (bound-and-true-p indiebrain-logos-focus-mode)))
        (scroll-lock-mode -1)
      (recenter nil)
      (scroll-lock-mode 1))))

(add-hook 'indiebrain-logos--focus-mode-hook #'indiebrain-logos--scroll-lock)

;; Based on Paul W. Rankin's code:
;; https://gist.github.com/rnkn/a522429ed7e784ae091b8760f416ecf8
(defun indiebrain-logos--hidden-modeline ()
  "Toggle mode line visibility."
  (when indiebrain-logos-hidden-modeline
    (if (or (eq mode-line-format nil)
            (not (bound-and-true-p indiebrain-logos-focus-mode)))
        (kill-local-variable 'mode-line-format)
      (setq-local mode-line-format nil)
      (force-mode-line-update))))

(add-hook 'indiebrain-logos--focus-mode-hook #'indiebrain-logos--hidden-modeline)

(provide 'indiebrain-logos)
;;; indiebrain-logos.el ends here
