;;; indiebrain-proced.el --- Extensions for proced   -*- lexical-binding: t; -*-

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

;; Extensions to proced intended for use in my Emacs configuration
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:

(defgroup indiebrain-proced ()
  "Proced extras for my dotemacs."
  :group 'proced)

;;;; Extend `proced' faces

(defface indiebrain-proced-user '((t :inherit shadow))
  "Face for user indicator in `proced'.")

(defface indiebrain-proced-pid
  '((((class color) (min-colors 88) (background light))
     :foreground "#5317ac")
    (((class color) (min-colors 88) (background dark))
     :foreground "#b6a0ff"))
  "Face for PID indicator in `proced'.")

(defface indiebrain-proced-cpu
  '((((class color) (min-colors 88) (background light))
     :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f78fe7"))
  "Face for memory indicator in `proced'.")

(defface indiebrain-proced-mem
  '((((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff"))
  "Face for CPU indicator in `proced'.")

(defface indiebrain-proced-time-start
  '((((class color) (min-colors 88) (background light))
     :foreground "#30517f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#a0bfdf"))
  "Face for start time indicator in `proced'.")

(defface indiebrain-proced-time-duration
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00cdc8"))
  "Face for time indicator in `proced'.")

(defface indiebrain-proced-process nil
  "Face for process indicator in `proced'.")

(defconst indiebrain-proced-keywords
  `((,(concat "^\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+"
             "\\(.*?\\)\s+\\(.*?\\)\s+\\(.*\\)")
     (1 'indiebrain-proced-user)
     (2 'indiebrain-proced-pid)
     (3 'indiebrain-proced-cpu)
     (4 'indiebrain-proced-mem)
     (5 'indiebrain-proced-time-start)
     (6 'indiebrain-proced-time-duration)
     (7 'indiebrain-proced-process)))
  "Extra font-lock patterns for the `proced' menu.")

;;;###autoload
(define-minor-mode indiebrain-proced-extra-keywords
  "Apply extra font-lock rules to diff buffers."
  :init-value nil
  :global t
  (if indiebrain-proced-extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil indiebrain-proced-keywords nil)
        (add-hook 'proced-mode-hook #'indiebrain-proced-extra-keywords))
    (font-lock-remove-keywords nil indiebrain-proced-keywords)
    (remove-hook 'proced-mode-hook #'indiebrain-proced-extra-keywords)
    (font-lock-flush (point-min) (point-max))))

(provide 'indiebrain-proced)
;;; indiebrain-proced.el ends here
