;; Garbage collect at the end of startup
(add-hook 'after-init-hook #'garbage-collect t)

(load-file "~/.emacs.d/straight-config.el")

(use-package use-package-ensure-system-package
  :straight t)

;; Load dot-env envrionment
(use-package dot-env
  :straight t
  :config
  (dot-env-config))

;; Set font variables
(setq amo/font-family (dot-env-get 'FONT "DejaVu Sans Mono")
      amo/font-size (dot-env-get 'FONT_SIZE "13")
      amo/base-font-size (* (string-to-number amo/font-size) 10)
      amo/font (concat amo/font-family " " amo/font-size))

;; Load config files
(org-babel-load-file (expand-file-name "~/.emacs.d/common-settings.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/common-packages.org"))
(load-file "~/.emacs.d/customize-settings.el")
