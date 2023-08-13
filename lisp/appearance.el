(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-minor-modes t
        doom-modeline-vcs-max-length 40
        doom-modeline-buffer-encoding t))

(use-package ef-themes
  :ensure t)

(use-package modus-themes
  :ensure t)
