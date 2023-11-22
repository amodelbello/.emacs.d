(setq custom-file "~/.emacs.d/terminal/customize.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)
