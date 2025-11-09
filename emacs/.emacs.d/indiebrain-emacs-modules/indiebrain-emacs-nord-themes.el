;;; indiebrain-emacs-nord-themes.el --- nord-themes configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2025  Aaron Kuehler <aaron.kuehler@gmail.com>

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

;; Configuration for the nord-theme package.
;;
;; See my full configuration: https://github.com/indiebrain/.files/

;;; Code:
(indiebrain-emacs-package nord-theme
  (:install t)

  (custom-set-faces
   '(org-level-1 ((t (:weigth light :height 1.9))))
   '(org-level-2 ((t (:weigth ligth :height 1.8))))
   '(org-level-3 ((t (:weigth regular :height 1.7))))
   '(org-level-4 ((t (:weigth regular :height 1.6))))
   '(org-level-5 ((t (:weigth regular :height 1.5 ))))
   '(org-level-6 ((t (:weigth bold :height 1.4 ))))
   '(org-level-7 ((t (:weigth bold :height 1.3 ))))
   '(org-level-8 ((t (:weigth bold :height 1.2 )))))

  (load-theme 'nord t)
)

(provide 'indiebrain-emacs-nord-themes)

;;; indiebrain-emacs-nord-themes.el ends here
