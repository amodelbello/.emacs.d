;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

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
;; (add-to-list 'package-archives
;;              '("gnu-devel" . "https://elpa.gnu.org/devel/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package quelpa
  :ensure t)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(epa-file-enable)

;; load environment variables
(use-package dot-env
  :ensure t
  :quelpa
  (dot-env :repo "amodelbello/dot-env.el"
           :fetcher github :upgrade t)
  :config
  (dot-env-config))

;; Set font variables
(setq amo/font-family (dot-env-get 'FONT "DejaVu Sans Mono")
      amo/font-size (dot-env-get 'FONT_SIZE "13")
      amo/base-font-size (* (string-to-number amo/font-size) 10)
      amo/font (concat amo/font-family " " amo/font-size))

;; load configuration files
(org-babel-load-file (expand-file-name "~/.emacs.d/lisp/packages.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/lisp/programming.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/lisp/bindings.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/lisp/configuration.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/lisp/appearance.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/lisp/miscellaneous.org"))

;; load customize settings
(setq custom-file "~/.emacs.d/customize.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load-file custom-file)
(put 'narrow-to-region 'disabled nil)

;; the selected tab highlighting doesn't work otherwise...
(amo/reload-centaur-tabs)
