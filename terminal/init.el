(setq user-emacs-directory "~/.emacs.d/terminal/")

(load-file "~/.emacs.d/straight-config.el")
(org-babel-load-file (expand-file-name "~/.emacs.d/common-settings.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/terminal/settings.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/common-packages.org"))
(load-file "~/.emacs.d/customize-settings.el")
