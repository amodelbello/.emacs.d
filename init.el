(load-file (concat user-emacs-directory "common/functions.el"))
(amo/load-straight)

;; Load dot-env envrionment
;; https://github.com/amodelbello/dot-env.el
(use-package dot-env
  :straight t
  :config
  (dot-env-config))

;; Set font variables
(setq amo/font-family (dot-env-get 'FONT "")
      amo/font-size (dot-env-get 'FONT_SIZE "13")
      amo/base-font-size (* (string-to-number amo/font-size) 10)
      amo/font (concat amo/font-family " " amo/font-size))

;; Load config files
(amo/load-config-file "common/settings.org")
(amo/load-config-file "gui/settings.org")
(amo/load-config-file "common/packages.org")
(amo/load-customize)
