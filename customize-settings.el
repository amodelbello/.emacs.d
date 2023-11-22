(setq custom-file (concat user-emacs-directory "customize.el"))
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)
