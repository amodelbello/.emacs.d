(setq user-emacs-directory "~/.emacs.d/terminal/")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(straight-use-package 'org)

(load-file "~/.emacs.d/terminal/settings.el")

;; load customize settings
(setq custom-file "~/.emacs.d/terminal/customize.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)
