;;; indiebrain-emacs-vc.el --- Configuration for version control, diff, etc  -*- lexical-binding: t; -*-

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

;; Configuration for version control, diffs, etc.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

;;; Projects (project.el)

(indiebrain-emacs-package project
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")))
  (indiebrain-emacs-keybind global-map
    "C-x p <delete>" #'project-forget-project))

;;; Diff-mode (and indiebrain-diff.el extensions)
(indiebrain-emacs-package diff-mode
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
  (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax 'hunk-also)
  (indiebrain-emacs-keybind diff-mode-map
    "L" #'vc-print-root-log
    ;; Emacs 29 can use C-x v v in diff buffers, which is great, but now I
    ;; need quick access to it...
    "v" #'vc-next-action))

;;; Version control framework (vc.el and indiebrain-vc.el)
(indiebrain-emacs-package vc
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)
  ;; I can see the files from the Diff with C-c C-d
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  ;; I use a different account for git commits
  (setq add-log-mailing-address "info@indiebrainesilaos.com")
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        `("%d %h %ai %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  ;; These two are from Emacs 29
  (setq vc-git-log-edit-summary-target-len 50)
  (setq vc-git-log-edit-summary-max-len 70)

  (setq vc-follow-symlinks t)

  ;; NOTE: I override lots of the defaults
  (indiebrain-emacs-keybind global-map
    "C-x v B" #'vc-annotate ; Blame mnemonic
    "C-x v e" #'vc-ediff
    "C-x v k" #'vc-delete-file ; 'k' for kill==>delete is more common
    "C-x v G" #'vc-log-search  ; git log --grep
    "C-x v t" #'vc-create-tag
    "C-x v d" #'vc-diff)
  (indiebrain-emacs-keybind vc-dir-mode-map
    "t" #'vc-create-tag
    "O" #'vc-log-outgoing
    "o" #'vc-dir-find-file-other-window
    "d" #'vc-diff         ; parallel to D: `vc-root-diff'
    "k" #'vc-dir-delete-file
    "G" #'vc-revert)
  (indiebrain-emacs-keybind vc-git-stash-shared-map
    "a" 'vc-git-stash-apply-at-point
    "c" 'vc-git-stash ; "create" named stash
    "k" 'vc-git-stash-delete-at-point ; symmetry with `vc-dir-delete-file'
    "p" 'vc-git-stash-pop-at-point
    "s" 'vc-git-stash-snapshot)
  (indiebrain-emacs-keybind vc-annotate-mode-map
    "M-q" #'vc-annotate-toggle-annotation-visibility
    "C-c C-c" #'vc-annotate-goto-line
    "<return>" #'vc-annotate-find-revision-at-line)
  (indiebrain-emacs-keybind log-edit-mode-map
    "M-s" nil ; I use M-s for my search commands
    "M-r" nil) ; I use `consult-history'
  (indiebrain-emacs-keybind log-view-mode-map
    "<tab>" #'log-view-toggle-entry-display
    "<return>" #'log-view-find-revision
    "s" #'vc-log-search
    "o" #'vc-log-outgoing
    "f" #'vc-log-incoming
    "F" #'vc-update
    "P" #'vc-push))

;;; Agitate - A complement to vc-mode by Prot
;; Read the manual here: <https://protesilaos.com/emacs/agitate>.
(indiebrain-emacs-package agitate
  (:install t)
  (add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)

  (setq agitate-log-edit-informative-show-root-log t)

  (agitate-log-edit-informative-mode 1)

  (indiebrain-emacs-keybind global-map
    "C-x v =" #'agitate-diff-buffer-or-file ; replace `vc-diff'
    "C-x v g" #'agitate-vc-git-grep ; replace `vc-annotate'
    "C-x v f" #'agitate-vc-git-find-revision
    "C-x v s" #'agitate-vc-git-show
    "C-x v w" #'agitate-vc-git-kill-commit-message
    "C-x v p p" #'agitate-vc-git-format-patch-single
    "C-x v p n" #'agitate-vc-git-format-patch-n-from-head)
  (indiebrain-emacs-keybind diff-mode-map
    "C-c C-b" #'agitate-diff-refine-cycle ; replace `diff-refine-hunk'
    "C-c C-n" #'agitate-diff-narrow-dwim)
  (indiebrain-emacs-keybind log-view-mode-map
    "w" #'agitate-log-view-kill-revision
    "W" #'agitate-log-view-kill-revision-expanded)
  (indiebrain-emacs-keybind vc-git-log-view-mode-map
    "c" #'agitate-vc-git-format-patch-single)
  (indiebrain-emacs-keybind log-edit-mode-map
    "C-c C-i C-n" #'agitate-log-edit-insert-file-name
    ;; See user options `agitate-log-edit-emoji-collection' and
    ;; `agitate-log-edit-conventional-commits-collection'.
    "C-c C-i C-e" #'agitate-log-edit-emoji-commit
    "C-c C-i C-c" #'agitate-log-edit-conventional-commit))

;;; Interactive and powerful git front-end (Magit)

;; There is no need to install the package, as transient.el is built
;; into Emacs.  By requiring it, I prevent the installation of the
;; package, which would be done by Magit.
(indiebrain-emacs-package transient)

(indiebrain-emacs-package magit
  (:install t)
  (setq magit-define-global-key-bindings nil)

  (defun indiebrain-project--project-magit ()
    (interactive)
    (require 'project)
      (magit-status (project-root (project-current t))))

  (with-eval-after-load 'project
    (setq project-switch-commands
          (append project-switch-commands
                  '((indiebrain-project--project-magit "Magit Status" "m")))))

  (indiebrain-emacs-keybind global-map
    "C-c g" #'magit-status
    "C-x p m" #'magit-status)

  (require 'git-commit)
  (setq git-commit-summary-max-length 50)
  (setq git-commit-known-pseudo-headers
        '("Signed-off-by"
          "Acked-by"
          "Modified-by"
          "Cc"
          "Suggested-by"
          "Reported-by"
          "Tested-by"
          "Reviewed-by"))
  (setq git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line))

  (require 'magit-diff)
  (setq magit-diff-refine-hunk t)

  (require 'magit-repos)
  (setq magit-repository-directories
        '(("~/Git/Projects" . 1))))

(indiebrain-emacs-package forge
  (:install t)
  (setopt forge-topic-list-limit '(5 . -1))

  ;; Puts the newly-created PR ID onto the kill-ring
  ;; so we can call fetch-topic
  (defun indiebrain-forge--post-submit-callback-kill-topic-id (value _headers _status _req)
    (when t
      (when-let ((url (alist-get 'html_url value)))
          (when (string-match "\\([0-9]+\\)$" url)
            (let ((topic-id (match-string 1 url)))
              ;; If only forge exposed a way to fetch and update a single PR in the database in a non-interactive way...
              (kill-new topic-id))))))

  (add-hook 'forge-post-submit-callback-hook #'indiebrain-forge--post-submit-callback-kill-topic-id))

;;; Smerge and Ediff
(indiebrain-emacs-package smerge-mode)

(indiebrain-emacs-package ediff
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Tweak those for safer identification and removal
  (setq ediff-combination-pattern
        '("<<<<<<< indiebrain-ediff-combine Variant A" A
          ">>>>>>> indiebrain-ediff-combine Variant B" B
          "####### indiebrain-ediff-combine Ancestor" Ancestor
          "======= indiebrain-ediff-combine End"))

  ;; TODO automate process in a robust way, or at least offer a good key
  ;; binding.
  (defun indiebrain/ediff-flush-combination-pattern ()
    "Remove my custom `ediff-combination-pattern' markers.
This is a quick-and-dirty way to get rid of the markers that are
left behind by `smerge-ediff' when combining the output of two
diffs.  While this could be automated via a hook, I am not yet
sure this is a good approach."
    (interactive)
    (flush-lines ".*indiebrain-ediff.*" (point-min) (point-max) nil)))

(provide 'indiebrain-emacs-vc)
;;; indiebrain-emacs-vc.el ends here
