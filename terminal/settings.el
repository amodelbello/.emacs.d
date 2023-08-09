;; A minimal terminal emacs config
;; Add `alias emacs="emacs -nw -q --load '~/.emacs.d/terminal/init.el'"`
;; to your shell configuration

(defun on-after-init()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(setq inhibit-startup-screen t
      save-interprogram-paste-before-kill t
      x-select-enable-clipboard t
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

(setq-default indent-tabs-mode nil
              global-tab-line-mode nil
              tab-line-mode nil
              tab-bar-mode nil
              line-spacing 0.3
              fill-column 100
              sentence-end-double-space nil
              visual-line-mode t
              whitespace-line-column 110)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-display-line-numbers-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(electric-pair-mode 1)
(delete-selection-mode 1)
(recentf-mode 1)
(whitespace-mode -1)
(define-prefix-command 'z-map)

(global-set-key (kbd "C-z") 'z-map)
(define-key z-map (kbd "t") 'customize-themes)

(defun open-settings-file ()
  "Open settings.el"
  (interactive)
  (find-file "~/.emacs.d/terminal/settings.el"))
(define-key z-map (kbd "s") 'open-settings-file)

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "Magenta")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(put 'narrow-to-region 'disabled nil)

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)
   ("M-z" . ace-window))
  :config
  (defvar aw-dispatch-always nil)
  (setq aw-scope 'frame)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)
         ("C-i" . avy-goto-char-2)))

(use-package crux
  :ensure t
  :bind
  (("s-o" . crux-smart-open-line-above)
   ("M-o" . crux-smart-open-line)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("C-c r" . crux-rename-file-and-buffer)
   ("s-j" . crux-top-join-line)
   ("s-k" . crux-kill-whole-line)
   ("<C-backspace>" . crux-kill-line-backwards)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
   ("C-c C-r" . nil))
  :config
  (ivy-mode)
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-display-style 'fancy
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (counsel-describe-variable . ivy--regex-fuzzy)
                                (counsel-describe-function . ivy--regex-fuzzy)
                                (swiper-isearch . ivy--regex-plus)
                                (t . ivy--regex-plus)))

  (use-package ivy-hydra
    :ensure t))

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git) ; will override the keybinding for `magit-file-dispatch'
   ("C-c j" . counsel-git-grep)
   ("C-c a" . counsel-ag)
   ("C-c t" . counsel-load-theme)
   ("C-x l" . counsel-locate)
   ("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("s-r" . counsel-recentf)
   :map minibuffer-local-map
   ("C-r" . counsl-minibuffer-history)))

(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch)
   :map read-expression-map
   ("C-r" . counsel-expression-history)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
