(load-file (concat user-emacs-directory "common/functions.el"))
(load-straight (concat user-emacs-directory "terminal/"))
(load-customize (concat user-emacs-directory "terminal/"))

(org-babel-load-file (expand-file-name (concat user-emacs-directory "common/settings.org")))
(org-babel-load-file (expand-file-name (concat user-emacs-directory "terminal/settings.org")))
(org-babel-load-file (expand-file-name (concat user-emacs-directory "common/packages.org")))
