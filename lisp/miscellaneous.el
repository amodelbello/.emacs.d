(defun amo/reload-centaur-tabs ()
  (interactive)
  (centaur-tabs-mode 0)
  (centaur-tabs-mode 1))
(defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))
  (defadvice custom-theme-save (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))
(add-hook 'after-load-theme-hook 'amo/reload-centaur-tabs)

(defun amo/buffer-display-minimal ()
  ;; (setq mode-line-format nil)
  (nlinum-mode 0))
(add-hook 'treemacs-mode-hook 'amo/buffer-display-minimal)

(defun amo/set-zero-line-spacing ()
  (setq line-spacing 0))
(add-hook 'calc-mode-hook #'amo/set-zero-line-spacing)
(add-hook 'calc-trail-mode-hook #'amo/set-zero-line-spacing)

(defun amo/before-save-actions ()
  (whitespace-cleanup)
  (delete-trailing-whitespace))
(add-hook 'before-save-hook #'amo/before-save-actions)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))
