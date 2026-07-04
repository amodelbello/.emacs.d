(load-file (concat user-emacs-directory "init.el"))

(amo/load-config-file "terminal/settings.org")
(amo/load-customize (concat user-emacs-directory "terminal/"))
