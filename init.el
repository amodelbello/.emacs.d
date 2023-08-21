;;
;; straight.el bootstrap
;;
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

;; Load our main config
(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))

;; Load customize settings
(setq custom-file "~/.emacs.d/customize.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load-file custom-file)
(put 'narrow-to-region 'disabled nil)
