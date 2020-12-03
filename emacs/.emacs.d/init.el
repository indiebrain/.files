(require 'package)

;; In addition to the GNU repository look for external packages in
;; these repositories.
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; Allow packages to declare dependencies on system programs.
(use-package use-package-ensure-system-package
  :ensure t)

;; I create an "el" version of my Org configuration file as a final step
;; before closing down Emacs.  This is done to load the latest version
;; of my code upon startup.
;;
;; Also helps with initialisation times.  Not that I care too much about
;; those… Hence why I no longer bother with deferring package loading
;; either by default or on a case-by-case basis.
(let* ((conf "~/.emacs.d/emacs-init")
       (el (concat conf ".el"))
       (org (concat conf ".org")))
  (if (file-exists-p el)
      (load-file el)
    (use-package org)
    (org-babel-load-file org)))
