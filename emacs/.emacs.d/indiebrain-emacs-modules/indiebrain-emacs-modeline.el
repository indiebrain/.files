;;; indiebrain-emacs-modeline.el --- modeline configuration  -*- lexical-binding: t; -*-

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

;; Configuration for the Emacs modeline.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Mode line
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; Remove the modeline's recursive edit indicators (square brackets).
;; Instead, my emacs configuration uses the 'recursion-indicator
;; provided by Daniel Mendler
;; <https://github.com/minad/recursion-indicator>.
(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

(setq mode-line-compact nil)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

(add-hook 'after-init-hook #'column-number-mode)

;;; Hide modeline "lighters" (minions.el)
(indiebrain-emacs-elpa-package 'minions
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-prominent-modes
        (list 'defining-kbd-macro
              'flymake-mode
              'indiebrain-simple-monocle))
  (minions-mode 1))

;;; Mode line recursion indicators
(indiebrain-emacs-elpa-package 'recursion-indicator
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")
  (recursion-indicator-mode 1))

;;; Display current time
(indiebrain-emacs-builtin-package 'time
  (setq display-time-format "%a %e %b, %H:%M")
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

;;; World clock
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Chicago" "Chicago")
          ("Brazil/Acre" "Rio Branco")
          ("America/New_York" "New York")
          ("Brazil/East" "Brasília")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Tehran" "Tehran")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Yekaterinburg" "Yekaterinburg")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Tokyo" "Tokyo")
          ("Asia/Vladivostok" "Vladivostok")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

;;; Keycast mode
(indiebrain-emacs-elpa-package 'keycast
  (setq keycast-mode-line-window-predicate #'keycast-active-frame-bottom-right-p)
  (setq keycast-separator-width 1)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (setq keycast-log-format "%-20K%C\n")
  (setq keycast-log-frame-alist
        '((minibuffer . nil)))
  (setq keycast-log-newest-first t))

(provide 'indiebrain-emacs-modeline)
;;; indiebrain-emacs-modeline.el ends here
