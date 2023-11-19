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

(global-display-line-numbers-mode 0)
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

;; Use C-h for delete-backward-char
(global-set-key (kbd "C-h") 'delete-backward-char)

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

;; Toggle line numbers
(defun amo/toggle-line-numbers ()
  "Toggle the display of line numbers"
  (interactive)
  (if (eq nil global-display-line-numbers-mode)
      (global-display-line-numbers-mode 1)
    (global-display-line-numbers-mode 0)))
(define-key z-map (kbd "l") 'amo/toggle-line-numbers)

(put 'narrow-to-region 'disabled nil)

(use-package ace-window
  :straight t
  :bind
  (("C-o" . ace-window))
  :config
  (defvar aw-dispatch-always nil)
  (setq aw-scope 'frame)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package avy
  :straight t)

(use-package company
  :straight t
  :config
  (global-company-mode))

(use-package crux
  :straight t
  :bind
  (("M-o" . crux-smart-open-line)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("C-c r" . crux-rename-file-and-buffer)
   ("<C-backspace>" . crux-kill-line-backwards)))

(use-package expand-region
  :straight t)

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
  :straight t
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

;; Terminal mappings to support iterm2 for Mac
;; https://www.emacswiki.org/emacs/iTerm2#h5o-11
(progn
     (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
     function-key-map)))
     (define-key map "\e[1;P9"  (kbd "H-a"))
     (define-key map "\e[1;P10" (kbd "H-b"))
     (define-key map "\e[1;P11" (kbd "H-c"))
     (define-key map "\e[1;P12" (kbd "H-d"))
     (define-key map "\e[1;P13" (kbd "H-e"))
     (define-key map "\e[1;P14" (kbd "H-f"))
     (define-key map "\e[1;P15" (kbd "H-g"))
     (define-key map "\e[1;P16" (kbd "H-h"))
     (define-key map "\e[1;P17" (kbd "H-i"))
     (define-key map "\e[1;P18" (kbd "H-j"))
     (define-key map "\e[1;P19" (kbd "H-k"))
     (define-key map "\e[1;P20" (kbd "H-l"))
     (define-key map "\e[1;P21" (kbd "H-m"))
     (define-key map "\e[1;P22" (kbd "H-n"))
     (define-key map "\e[1;P23" (kbd "H-o"))
     (define-key map "\e[1;P24" (kbd "H-p"))
     (define-key map "\e[1;P25" (kbd "H-q"))
     (define-key map "\e[1;P26" (kbd "H-r"))
     (define-key map "\e[1;P27" (kbd "H-s"))
     (define-key map "\e[1;P28" (kbd "H-t"))
     (define-key map "\e[1;P29" (kbd "H-u"))
     (define-key map "\e[1;P30" (kbd "H-v"))
     (define-key map "\e[1;P31" (kbd "H-w"))
     (define-key map "\e[1;P32" (kbd "H-x"))
     (define-key map "\e[1;P33" (kbd "H-y"))
     (define-key map "\e[1;P34" (kbd "H-z"))
     (define-key map "\e[1;P35" (kbd "H-0"))
     (define-key map "\e[1;P36" (kbd "H-1"))
     (define-key map "\e[1;P37" (kbd "H-2"))
     (define-key map "\e[1;P38" (kbd "H-3"))
     (define-key map "\e[1;P39" (kbd "H-4"))
     (define-key map "\e[1;P40" (kbd "H-5"))
     (define-key map "\e[1;P41" (kbd "H-6"))
     (define-key map "\e[1;P42" (kbd "H-7"))
     (define-key map "\e[1;P43" (kbd "H-8"))
     (define-key map "\e[1;P44" (kbd "H-9"))
     (define-key map "\e[1;P45" (kbd "H-<f1>"))
     (define-key map "\e[1;P46" (kbd "H-<f2>"))
     (define-key map "\e[1;P47" (kbd "H-<f3>"))
     (define-key map "\e[1;P48" (kbd "H-<f4>"))
     (define-key map "\e[1;P49" (kbd "H-<f5>"))
     (define-key map "\e[1;P50" (kbd "H-<f6>"))
     (define-key map "\e[1;P51" (kbd "H-<f7>"))
     (define-key map "\e[1;P52" (kbd "H-<f8>"))
     (define-key map "\e[1;P53" (kbd "H-<f9>"))
     (define-key map "\e[1;P54" (kbd "H-<f10>"))
     (define-key map "\e[1;P55" (kbd "H-<f11>"))
     (define-key map "\e[1;P56" (kbd "H-<f12>"))
     ))

(global-set-key (kbd "H-a") 'er/expand-region)
(global-set-key (kbd "H-b") 'er/contract-region) ;; FIXME - map to undo for some reason
(global-set-key (kbd "H-c") 'avy-goto-char-2)
(global-set-key (kbd "H-d") 'crux-smart-open-line-above)
(global-set-key (kbd "H-e") 'crux-top-join-line)
(global-set-key (kbd "H-f") 'crux-kill-whole-line)
(global-set-key (kbd "C-x H-c") 'comment-line)
