;; Garbage collect at the end of startup
(add-hook 'after-init-hook #'garbage-collect t)

(load-file (concat user-emacs-directory "common/functions.el"))
(amo/load-straight)

(use-package use-package-ensure-system-package
  :straight t)

;; Compile Emacs Lisp libraries automatically.
;; https://github.com/jamescherti/compile-angel.el
(use-package compile-angel
  :ensure t
  :demand t
  :straight (compile-angel
             :type git
             :host github
             :repo "jamescherti/compile-angel.el")
  :config
  (compile-angel-on-save-mode)
  (compile-angel-on-load-mode))

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
