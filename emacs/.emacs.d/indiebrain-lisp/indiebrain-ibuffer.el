;;; indiebrain-ibuffer.el --- Extensions to ibuffer.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my ibuffer.el extensions, for use in my Emacs setup:
;; <https://github.com/indiebrain/.files/>.

;;; Code:

(require 'ibuffer)

;;;###autoload
(defun indiebrain-ibuffer-buffers-major-mode (&optional arg)
  "Select buffers that match the current buffer's major mode.
With optional prefix ARG (\\[universal-argument]) produce an
`ibuffer' filtered accordingly.  Else use standard completion."
  (interactive "P")
  (let* ((major major-mode)
         (prompt "Buffers for"))
    (if arg
        (ibuffer t (format "*%s %s*" prompt major)
                 (list (cons 'used-mode major)))
      (switch-to-buffer
       (read-buffer
        (format "%s %s:" prompt major) nil t
        (lambda (pair) ; pair is (name-string . buffer-object)
          (with-current-buffer (cdr pair) (derived-mode-p major))))))))

;;;###autoload
(defun indiebrain-ibuffer-buffers-vc-root (&optional arg)
  "Select buffers that belong to the version controlled directory.
With optional prefix ARG (\\[universal-argument]) produce an
`ibuffer' filtered accordingly.  Else use standard completion."
  (interactive "P")
  (let* ((root (or (vc-root-dir)
                   (locate-dominating-file "." ".git")))
         (prompt "Buffers for VC"))
    (if root
        (if arg
            (ibuffer t (format "*%s %s*" prompt root)
                     (list (cons 'filename (expand-file-name root))))
          (switch-to-buffer
           (read-buffer
            (format "%s %s:" prompt root) nil t
            (lambda (pair) ; pair is (name-string . buffer-object)
              (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
      (user-error "Not in a version-controlled directory"))))

(provide 'indiebrain-ibuffer)
;;; indiebrain-ibuffer.el ends here
