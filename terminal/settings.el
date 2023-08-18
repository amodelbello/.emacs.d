;; A minimal terminal emacs config
;; Add `alias emacs="emacs -nw -q --load '~/.emacs.d/terminal/init.el'"`
;; to your shell configuration


(defun on-after-init()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(straight-use-package
 '(nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))

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
(savehist-mode 1)
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

;; Reload config
(defun amo/reload-config ()
  "Reload configuration"
  (interactive)
  ;; (dot-env-load)
  (load-file "~/.emacs.d/terminal/init.el"))
(define-key z-map (kbd "r") #'amo/reload-config)

(put 'narrow-to-region 'disabled nil)

(use-package ace-window
  :straight t
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
  :straight t
  :bind (("C-;" . avy-goto-char)
         ("C-i" . avy-goto-char-2)))

(use-package company
  :straight t
  :config
  (global-company-mode))

(use-package crux
  :straight t
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

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("C-j" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-s" . consult-line)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s e" . consult-isearch-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :straight t

  :bind
  (("C-c ;" . embark-act)         ;; pick some comfortable binding
   ("C-c :" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))   ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup))
  :init
  (nerd-icons-completion-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package magit
  :straight t
  :bind
  (("C-x g" . magit)))

(use-package jump-char
  :straight (:host github :repo "lewang/jump-char"
                   :branch "master")
  :bind (("M-n" . jump-char-forward)
         ("M-N" . jump-char-backward)))

(use-package idle-highlight-mode
  :straight t
  :config
  (setq idle-highlight-idle-time 0.2
        idle-highlight-exclude-point t)
  :hook
  ((prog-mode text-mode) . idle-highlight-mode))

(use-package minions
  :straight t
  :config
  (minions-mode 1))

(use-package paredit
  :straight t
  :hook
  ((lisp-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (clojure-mode . paredit-mode)
   (clojurescript-mode . paredit-mode)
   (clojurec-mode . paredit-mode)
   (cider-repl-mode . paredit-mode)))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
        undo-tree-enable-undo-in-region t
        undo-tree-auto-save-history t)
  :diminish
  (undo-tree-mode))
