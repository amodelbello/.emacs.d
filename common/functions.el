(defun amo/load-straight (&optional base-dir)
  (defvar bootstrap-version)
  (let* ((base-dir (or base-dir user-emacs-directory))
         (bootstrap-file
          (expand-file-name
           (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el") base-dir))
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
  (straight-use-package 'org))

(defun amo/load-customize (&optional base-dir)
  (defvar custom-file)
  (let* ((base-dir (or base-dir user-emacs-directory)))
    (setq custom-file (concat base-dir "customize.el"))
    (unless (file-exists-p custom-file)
      (write-region "" "" custom-file))
    (load custom-file)))

(defun amo/load-config-file (config-file)
  (org-babel-load-file (expand-file-name (concat user-emacs-directory config-file))))
