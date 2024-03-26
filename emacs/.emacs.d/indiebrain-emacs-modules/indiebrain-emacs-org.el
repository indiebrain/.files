;;; indiebrain-emacs-org.el --- Configuration for org-mode  -*- lexical-binding: t; -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure Emacs' org-mode personal information manager.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Org-mode (personal information manager)

(indiebrain-emacs-package org
  (setq org-directory (convert-standard-filename (file-name-concat indiebrain-emacs-synchronized-directory ".org")))
  (setq org-imenu-depth 7)
;;;; general settings
  (setq org-adapt-indentation nil)
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)

;;;; refile, todo
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")))
  (setq org-todo-keyword-faces
        '(("WAIT" . '(bold org-todo))
          ("MAYBE" . '(shadow org-todo))
          ("CANCEL" . '(bold org-done))))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

;;;; log
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-read-date-prefer-future 'time)

;;;; links
  (setq org-link-keep-stored-after-insertion nil)

;;;; capture
  (setq org-capture-templates
        `(("b" "Basic task for future review" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l")
           :empty-lines-after 1)
          ("c" "Clock in to a task" entry
           (file+headline "tasks.org" "Clocked tasks")
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %T\n"
                    ":PROPERTIES:\n"
                    ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                    ":END:\n\n"
                    "%a\n")
           :prepend t
           :clock-in t
           :clock-keep t
           :immediate-finish t
           :empty-lines-after 1)
          ("m" "Memorandum of conversation" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* Memorandum of conversation with %^{Person}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%?")
           :empty-lines-after 1)
          ("t" "Task with a due date" entry
           (file+headline "tasks.org" "Tasks with a date")
           ,(concat "* TODO %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)))

;;;; agenda
;;;;; Basic agenda setup
  (setq org-default-notes-file (make-temp-file "emacs-org-notes-")) ; send it to oblivion
  (setq org-agenda-files `(,org-directory ,denote-directory))
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  ;; In my `indiebrain-org.el' (see further below), I add
  ;; `org-agenda-to-appt' to various relevant hooks.
  ;;
  ;; Create reminders for tasks with a due date when this file is read.
  (run-at-time (* 60 5) nil #'org-agenda-to-appt)

;;;;; General agenda view options
  ;; NOTE 2021-12-07: Check further below my `org-agenda-custom-commands'
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)

;;;;; Agenda marks
  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

;;;;; Agenda diary entries
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary nil)

;;;;; Agenda follow mode
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

;;;;; Agenda multi-item tasks
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

;;;;; Agenda filters and restricted views
  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

;;;;; Agenda items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        (concat "Now " (make-string 70 ?-)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0600 0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " ....." "-----------------"))
  (setq org-agenda-default-appointment-duration nil)

;;;;; Agenda global to-do list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

;;;;; Agenda tagged items
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -100)

;;;;; Agenda entry
  ;; NOTE: I do not use this right now.  Leaving everything to its
  ;; default value.
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")

;;;;; Agenda logging and clocking
  ;; NOTE: I do not use these yet, though I plan to.  Leaving everything
  ;; to its default value for the time being.
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-agenda-clock-consistency-checks
        '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                         ("4:00")
                         :default-face ; This should definitely be reviewed
                         ((:background "DarkRed")
                          (:foreground "white"))
                         :overlap-face nil :gap-face nil :no-end-time-face nil
                         :long-face nil :short-face nil)))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;;;;; Agenda column view
  ;; NOTE I do not use these, but may need them in the future.
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

;;;;; Agenda habits
  (require 'org-habit)
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 9)

;;;; code blocks
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

;;;; export
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (require 'ox-texinfo)
  (require 'ox-md)
  ;; FIXME: how to remove everything else?
  (setq org-export-backends '(html texinfo md))

;;;; IDs
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id)

;;;; Hooks and key bindings

  ;; See the `pulsar' package configuration, defined elsewhere in this setup.
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-middle)
      (add-hook hook #'pulsar-reveal-entry)))

  (indiebrain-emacs-keybind global-map
    "C-c a" #'org-agenda
    "C-c c" #'org-capture
    "C-c l" #'org-store-link
    "C-c o" #'org-open-at-point-global)
  (indiebrain-emacs-keybind org-mode-map
    ;; I don't like that Org binds one zillion keys, so if I want one
    ;; for something more important, I disable it from here.
    "C-'" nil
    "C-," nil
    "M-;" nil
    "<C-return>" nil
    "<C-S-return>" nil
    "C-M-S-<right>" nil
    "C-M-S-<left>" nil
    "C-c M-l" #'org-insert-last-stored-link
    "C-c C-M-l" #'org-toggle-link-display))

;;; Custom extensions (indiebrain-org.el)
(indiebrain-emacs-package indiebrain-org
  (setq org-agenda-format-date #'indiebrain-org-agenda-format-date-aligned)

  ;; Check the variable `indiebrain-org-custom-daily-agenda' in indiebrain-org.el
  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,indiebrain-org-custom-daily-agenda
           ((org-agenda-fontify-priorities nil)
            (org-agenda-dim-blocked-tasks nil)))
          ("P" "Plain text daily agenda and top priorities"
           ,indiebrain-org-custom-daily-agenda
           ((org-agenda-with-colors nil)
            (org-agenda-prefix-format "%t %s")
            (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
            (org-agenda-fontify-priorities nil)
            (org-agenda-remove-tags t))
           ("agenda.txt"))))

  ;; I bind `org-agenda' to C-c a, so this one puts me straight into my
  ;; custom block agenda.
  (indiebrain-emacs-keybind global-map
    "C-c A" (lambda () (interactive) (org-agenda nil "A")))

  (indiebrain-emacs-keybind ctl-x-x-map
    "i" #'indiebrain-org-id-headlines
    "h" #'indiebrain-org-ox-html)

  (add-to-list 'org-capture-templates
               '("j" "Music suggestion (jukebox)" entry
                 (file+headline "tasks.org" "Music suggestions")
                 #'indiebrain-org-capture-jukebox
                 :empty-lines-after 1
                 :immediate-finish t)))

;;; org-modern
(indiebrain-emacs-package org-modern
  (:install t)
  (setq org-modern-label-border 1)
  (setq org-modern-variable-pitch nil)
  (setq org-modern-timestamp t)
  (setq org-modern-table nil)
  (setq org-modern-list ; I swap the defaults for + and *
        '((?+ . "•")
          (?- . "–")
          (?* . "◦")))
  (setq org-modern-internal-target nil)
  (setq org-modern-radio-target nil)

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;;; Calendar
(indiebrain-emacs-package calendar
  (setq diary-file (file-name-concat org-directory "diary"))
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  (require 'solar)
  (setq calendar-latitude 39.95         ; Not my actual coordinates
        calendar-longitude -75.16)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "-0400")
  (setq calendar-daylight-time-zone-name "-0500"))

;;; Appt (appointment reminders which also integrate with Org agenda)
(indiebrain-emacs-package appt
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6)

  (run-at-time 10 nil #'appt-activate 1))

;;; Github Flavored Markdown exporter (ox-gfm.el)
(indiebrain-emacs-package ox-gfm
  (:install t)
  (eval-after-load "org"
    '(require 'ox-gfm)))

(provide 'indiebrain-emacs-org)
;;; indiebrain-emacs-org.el ends here
