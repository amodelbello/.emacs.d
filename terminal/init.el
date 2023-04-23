(setq package-user-dir "~/.emacs.d/terminal/elpa")
(setq user-emacs-directory "~/.emacs.d/terminal/")

(eval-when-compile
  (require 'package))


(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load-file "~/.emacs.d/terminal/settings.el")

;; load customize settings
(setq custom-file "~/.emacs.d/terminal/customize.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)
