;;; indiebrain-emacs-web.el --- Configuration for rendering web content  -*- lexical-binding: t; -*-

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

;;; Simple HTML Renderer (shr), Emacs Web Wowser (eww), Elpher, and indiebrain-eww.el
(indiebrain-emacs-package browse-url
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

(indiebrain-emacs-package goto-addr
  (setq goto-address-url-face 'link)
  (setq goto-address-url-mouse-face 'highlight)
  (setq goto-address-mail-face nil)
  (setq goto-address-mail-mouse-face 'highlight))

(indiebrain-emacs-package shr
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)          ; check `indiebrain-eww-readable'
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(indiebrain-emacs-package url-cookie
  (setq url-cookie-untrusted-urls '(".*")))

(indiebrain-emacs-package eww
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (setq eww-retrieve-command nil)

  (indiebrain-emacs-keybind eww-link-keymap
    "v" nil) ; stop overriding `eww-view-source'
  (indiebrain-emacs-keybind eww-mode-map
    "L" #'eww-list-bookmarks)
  (indiebrain-emacs-keybind dired-mode-map
    "E" #'eww-open-file) ; to render local HTML files
  (indiebrain-emacs-keybind eww-buffers-mode-map
    "d" #'eww-bookmark-kill)   ; it actually deletes
  (indiebrain-emacs-keybind eww-bookmark-mode-map
    "d" #'eww-bookmark-kill)) ; same

(indiebrain-emacs-package elpher    ; NOTE 2021-07-24: work-in-progress
  (:install t))

(indiebrain-emacs-package indiebrain-eww
  (setq indiebrain-eww-save-history-file
        (locate-user-emacs-file "indiebrain-eww-visited-history"))
  (setq indiebrain-eww-save-visited-history t)
  (setq indiebrain-eww-bookmark-link nil)

  (add-hook 'indiebrain-eww-history-mode-hook #'hl-line-mode)

  (define-prefix-command 'indiebrain-eww-map)
  (indiebrain-emacs-keybind global-map
    "C-c w" 'indiebrain-eww-map)
  (indiebrain-emacs-keybind indiebrain-eww-map
    "b" #'indiebrain-eww-visit-bookmark
    "e" #'indiebrain-eww-browse-dwim
    "s" #'indiebrain-eww-search-engine)
  (indiebrain-emacs-keybind eww-mode-map
    "B" #'indiebrain-eww-bookmark-page
    "D" #'indiebrain-eww-download-html
    "F" #'indiebrain-eww-find-feed
    "H" #'indiebrain-eww-list-history
    "b" #'indiebrain-eww-visit-bookmark
    "e" #'indiebrain-eww-browse-dwim
    "o" #'indiebrain-eww-open-in-other-window
    "E" #'indiebrain-eww-visit-url-on-page
    "J" #'indiebrain-eww-jump-to-url-on-page
    "R" #'indiebrain-eww-readable
    "Q" #'indiebrain-eww-quit))

;;; Elfeed feed/RSS reader
(indiebrain-emacs-package elfeed
  (:install t)
  (setq elfeed-use-curl nil)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))

  ;; Make sure to also check the section on shr and eww for how I handle
  ;; `shr-width' there.
  (add-hook 'elfeed-show-mode-hook
            (lambda () (setq-local shr-width (current-fill-column))))

  (indiebrain-emacs-keybind global-map
    "C-c e" #'elfeed)
  (indiebrain-emacs-keybind elfeed-search-mode-map
    "w" #'elfeed-search-yank
    "g" #'elfeed-update
    "G" #'elfeed-search-update--force)
  (indiebrain-emacs-keybind elfeed-show-mode-map
    "w" #'elfeed-show-yank))

(with-eval-after-load 'elfeed
  (indiebrain-emacs-package indiebrain-elfeed
    (setq indiebrain-elfeed-tag-faces t)
    (indiebrain-elfeed-fontify-tags)
    (add-hook 'elfeed-search-mode-hook #'indiebrain-elfeed-load-feeds)

    (indiebrain-emacs-keybind elfeed-search-mode-map
      "s" #'indiebrain-elfeed-search-tag-filter
      "+" #'indiebrain-elfeed-toggle-tag)
    (indiebrain-emacs-keybind elfeed-show-mode-map
      "+" #'indiebrain-elfeed-toggle-tag)))

;;; Elfeed extensions for watching videos (elfeed-tube)
(indiebrain-emacs-package elfeed-tube
  (:install t)
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  (indiebrain-emacs-keybind elfeed-show-mode-map
    "F" #'elfeed-tube-fetch
    "C-c C-s" #'elfeed-tube-save)
  (indiebrain-emacs-keybind elfeed-search-mode-map
    "F" #'elfeed-tube-fetch
    "C-c C-s" #'elfeed-tube-save))

(indiebrain-emacs-package mpv
  (:install t))

(indiebrain-emacs-package elfeed-tube-mpv
  (:install t)
  (indiebrain-emacs-keybind elfeed-search-mode-map
    "v" 'elfeed-tube-mpv)
  (indiebrain-emacs-keybind elfeed-show-mode-map
    "v" 'elfeed-tube-mpv
    "C-c C-v" 'elfeed-tube-mpv
    "C-c C-f" 'elfeed-tube-mpv-follow-mode
    "C-c C-w" 'elfeed-tube-mpv-where))

;;; Rcirc (IRC client)
;; (indiebrain-emacs-package rcirc
;;   (setq rcirc-server-alist
;;         `(("irc.libera.chat"
;;            :channels ("#emacs""#org-mode" "#rcirc" "#sr.ht")
;;            :port 6697 :encryption tls
;;            :password ,(indiebrain-common-auth-get-field "libera" :secret))))
;;
;;   (setq rcirc-prompt "%t> ") ; Read the docs or use (customize-set-variable 'rcirc-prompt "%t> ")
;;
;;   (setq rcirc-default-nick "indiebrain"
;;         rcirc-default-user-name rcirc-default-nick
;;         rcirc-default-full-name "Aaron Kuehler")
;;
;;   ;; NOTE 2021-11-28: Is there a canonical way to disable this?
;;   (setq rcirc-timeout-seconds most-positive-fixnum)
;;
;;   (rcirc-track-minor-mode 1)
;;
;;   (indiebrain-emacs-keybind global-map
;;     "C-c i" #'irc))indiebrain-common-auth-get-field

(provide 'indiebrain-emacs-web)
;;; indiebrain-emacs-org.el ends here
