(eval-when-compile
  (require 'package)
  ;;  (require 'diminish)
  ;;  (require 'bind-key)
  )


(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

;; load our main config
(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))

;; load customize settings
(setq custom-file "~/.emacs.d/customize.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load-file custom-file)
(put 'narrow-to-region 'disabled nil)


;; the selected tab highlighting doesn't work otherwise...
(centaur-tabs-mode 0)
(centaur-tabs-mode t)
