(load-file (concat user-emacs-directory "common/functions.el"))
(amo/load-straight (concat user-emacs-directory "terminal/"))

(use-package use-package-ensure-system-package
  :straight t)

;; Load dot-env envrionment
;; https://github.com/amodelbello/dot-env.el
(use-package dot-env
  :straight t
  :config
  (dot-env-config))

(amo/load-config-file "common/settings.org")
(amo/load-config-file "terminal/settings.org")
(amo/load-config-file "common/packages.org")
(amo/load-customize (concat user-emacs-directory "terminal/"))
