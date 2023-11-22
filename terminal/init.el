(load-file (concat user-emacs-directory "common/functions.el"))
(amo/load-straight (concat user-emacs-directory "terminal/"))
(amo/load-customize (concat user-emacs-directory "terminal/"))

(amo/load-config-file "common/settings.org")
(amo/load-config-file "terminal/settings.org")
(amo/load-config-file "common/packages.org")
