(set-frame-font amo/font nil t)
(set-face-attribute 'fixed-pitch nil :font amo/font)
(set-face-attribute 'variable-pitch nil :font amo/font)
(set-face-attribute 'default nil :height amo/base-font-size)
(desktop-save-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode t)
(recentf-mode 1)
(whitespace-mode -1)
(global-display-line-numbers-mode 1)

(setq scroll-step 1
      recentf-max-menu-items 25
      recentf-max-saved-items 25
      save-interprogram-paste-before-kill t
      auto-mode-alist (append '(("\\.cl$" . lisp-mode))
                              auto-mode-alist)
      inferior-lisp-program "/usr/local/bin/sbcl"
      font-latex-fontify-script nil
      auto-save-default nil
      create-lockfiles nil
      undo-tree-enable-undo-in-region t
      delete-by-moving-to-trash t
      dired-kill-when-opening-new-dired-buffer t
      comment-column 64

      ;; Save backups to a central location
      make-backup-files t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq-default indent-tabs-mode nil
              org-catch-invisible-edits 'show
              global-tab-line-mode nil
              tab-line-mode nil
              tab-bar-mode nil
              line-spacing 0.3
              fill-column 100
              sentence-end-double-space nil
              visual-line-mode t
              whitespace-line-column 110)

;; Set the right mode when you create a buffer
(setq-default major-mode
              (lambda () (if buffer-file-name
                             (fundamental-mode)
                           (let ((buffer-file-name (buffer-name)))
                             (set-auto-mode)))))

;; Blink modeline instead of ring bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "Magenta")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(custom-set-variables '(ediff-split-window-function (quote split-window-horizontally)))
(custom-set-variables '(ediff-window-setup-function (quote ediff-setup-windows-plain)))
