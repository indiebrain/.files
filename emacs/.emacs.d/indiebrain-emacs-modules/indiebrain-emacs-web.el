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
(indiebrain-emacs-builtin-package 'browse-url
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

(indiebrain-emacs-builtin-package 'goto-addr
  (setq goto-address-url-face 'link)
  (setq goto-address-url-mouse-face 'highlight)
  (setq goto-address-mail-face nil)
  (setq goto-address-mail-mouse-face 'highlight))

(indiebrain-emacs-builtin-package 'shr
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)          ; check `indiebrain-eww-readable'
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(indiebrain-emacs-builtin-package 'url-cookie
  (setq url-cookie-untrusted-urls '(".*")))

(indiebrain-emacs-builtin-package 'eww
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

  (define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
  (define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill)) ; same

(indiebrain-emacs-elpa-package 'elpher)    ; NOTE 2021-07-24: work-in-progress

(indiebrain-emacs-builtin-package 'indiebrain-eww
  (setq indiebrain-eww-save-history-file
        (locate-user-emacs-file "indiebrain-eww-visited-history"))
  (setq indiebrain-eww-save-visited-history t)
  (setq indiebrain-eww-bookmark-link nil)

  (add-hook 'indiebrain-eww-history-mode-hook #'hl-line-mode)

  (define-prefix-command 'indiebrain-eww-map)
  (define-key global-map (kbd "C-c w") 'indiebrain-eww-map)
  (let ((map indiebrain-eww-map))
    (define-key map (kbd "b") #'indiebrain-eww-visit-bookmark)
    (define-key map (kbd "e") #'indiebrain-eww-browse-dwim)
    (define-key map (kbd "s") #'indiebrain-eww-search-engine))
  (let ((map eww-mode-map))
    (define-key map (kbd "B") #'indiebrain-eww-bookmark-page)
    (define-key map (kbd "D") #'indiebrain-eww-download-html)
    (define-key map (kbd "F") #'indiebrain-eww-find-feed)
    (define-key map (kbd "H") #'indiebrain-eww-list-history)
    (define-key map (kbd "b") #'indiebrain-eww-visit-bookmark)
    (define-key map (kbd "e") #'indiebrain-eww-browse-dwim)
    (define-key map (kbd "o") #'indiebrain-eww-open-in-other-window)
    (define-key map (kbd "E") #'indiebrain-eww-visit-url-on-page)
    (define-key map (kbd "J") #'indiebrain-eww-jump-to-url-on-page)
    (define-key map (kbd "R") #'indiebrain-eww-readable)
    (define-key map (kbd "Q") #'indiebrain-eww-quit)))

;;; Elfeed feed/RSS reader
(indiebrain-emacs-elpa-package 'elfeed
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

  (define-key global-map (kbd "C-c e") #'elfeed)
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "w") #'elfeed-search-yank)
    (define-key map (kbd "g") #'elfeed-update)
    (define-key map (kbd "G") #'elfeed-search-update--force))
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "w") #'elfeed-show-yank)))

(with-eval-after-load 'elfeed
  (indiebrain-emacs-builtin-package 'indiebrain-elfeed
    (setq indiebrain-elfeed-tag-faces t)
    (indiebrain-elfeed-fontify-tags)
    (add-hook 'elfeed-search-mode-hook #'indiebrain-elfeed-load-feeds)

    (let ((map elfeed-search-mode-map))
      (define-key map (kbd "s") #'indiebrain-elfeed-search-tag-filter)
      (define-key map (kbd "+") #'indiebrain-elfeed-toggle-tag))
    (define-key elfeed-show-mode-map (kbd "+") #'indiebrain-elfeed-toggle-tag)))

;;; Elfeed extensions for watching videos (elfeed-tube)
(indiebrain-emacs-elpa-package 'elfeed-tube
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "F") #'elfeed-tube-fetch)
    (define-key map [remap save-buffer] #'elfeed-tube-save))
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "F") #'elfeed-tube-fetch)
    (define-key map [remap save-buffer] #'elfeed-tube-save)))

(indiebrain-emacs-elpa-package 'mpv)

(indiebrain-emacs-elpa-package 'elfeed-tube-mpv
  (define-key elfeed-search-mode-map (kbd "v") 'elfeed-tube-mpv)
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "v") 'elfeed-tube-mpv)
    (define-key map (kbd "C-c C-v") 'elfeed-tube-mpv)
    (define-key map (kbd "C-c C-f") 'elfeed-tube-mpv-follow-mode)
    (define-key map (kbd "C-c C-w") 'elfeed-tube-mpv-where)))

;;; Rcirc (IRC client)
;; (indiebrain-emacs-builtin-package 'rcirc
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
;;   (define-key global-map (kbd "C-c i") #'irc))indiebrain-common-auth-get-field

(provide 'indiebrain-emacs-web)
;;; indiebrain-emacs-org.el ends here
