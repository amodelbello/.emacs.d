(use-package ace-window
  :ensure t
  :bind
  (("C-o" . ace-window)
   ("C-x o" . ace-window))
  :config
  (defvar aw-dispatch-always nil)
  (setq aw-scope 'frame)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package tex
  :ensure auctex
  :config (setq TeX-auto-save t
                TeX-parse-self t)
          (setq-default TeX-master nil))

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char)
         ("C-;" . avy-goto-char-2)
         :map org-mode-map
         ("C-'" . nil)))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-blink-duration 0.5
        beacon-blink-delay 0.1))

(use-package buffer-move
  :ensure t
  :bind
  (("<C-S-up>" . buf-move-up)
   ("<C-S-down>" . buf-move-down)
   ("<C-S-left>" . buf-move-left)
   ("<C-S-right>" .  buf-move-right)
   :map org-mode-map
   ("<C-S-up>" . buf-move-up)
   ("<C-S-down>" . buf-move-down)
   ("<C-S-left>" . buf-move-left)
   ("<C-S-right>" . buf-move-right)))

(use-package centaur-tabs
  :ensure t
  :init
  (centaur-tabs-mode t)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'under
        centaur-tabs-show-count nil
        x-underline-at-descent-line t
        uniquify-separator "/"
        uniquify-buffer-name-style 'forward
        centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  :bind
  ("s-M-<left>" . centaur-tabs-backward)
  ("s-M-<right>" . centaur-tabs-forward)
  ("s-S-<left>" . centaur-tabs-move-current-tab-to-left)
  ("s-S-<right>" . centaur-tabs-move-current-tab-to-right)
  ("s-W" . centaur-tabs-kill-all-buffers-in-current-group)
  :hook
  (popper-mode . centaur-tabs-local-mode))

(defun centaur-tabs-hide-tab (x)
  "Do not show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-match "\\`\*.*'\*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.4
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil)
  (custom-set-variables
   '(company-quick-access-modifier 'super))
  (use-package company-box
    :ensure t
    :hook
    (company-mode . company-box-mode)))

(use-package crux
  :ensure t
  :bind
  (("s-o" . crux-smart-open-line-above)
   ("M-o" . crux-smart-open-line)

   ; this all of a sudden stopped working
   ; switching to coucel-recentf for now
   ; ("s-r" . crux-recentf-find-file)

   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("C-c r" . crux-rename-file-and-buffer)
   ("s-j" . crux-top-join-line)
   ("s-k" . crux-kill-whole-line)
   ("<C-backspace>" . crux-kill-line-backwards)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package sqlite3
  :ensure t)

(when (memq window-system '(mac ns)) ;; MacOS
         (use-package exec-path-from-shell
           :ensure t
           :config
           (setq exec-path-from-shell-arguments nil) ; non-interactive, i.e. .zshenv not .zshrc
           (exec-path-from-shell-initialize)))
(when (memq window-system '(x)) ;; Linux
         (use-package exec-path-from-shell
           :ensure t
           :config
           (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :ensure t))

(setq-default ispell-program-name (dot-env-get 'ISPELL_PATH "/opt/homebrew/opt/ispell/bin/ispell"))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (define-key flyspell-mode-map (kbd "C-;") nil))))

(use-package git-messenger
  :ensure t
  :config (setq git-messenger:show-detail t
                git-messenger:use-magit-popup t)
  :bind ("C-x m" . git-messenger:popup-message))

(use-package git-timemachine
  :ensure t)

(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key (dot-env-get 'GPTEL_API_KEY)
        gptel-default-mode #'org-mode))

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config (setq grip-preview-use-webkit t
                grip-github-user "amodelbello"
                grip-github-password (dot-env-get 'GRIP_GITHUB_PASSWORD)))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
            ("org" (mode . org-mode))
            ("web" (or (mode . web-mode) (mode . js2-mode)))
            ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
            ("programming" (or
                            (mode . emacs-lisp-mode)
                            (mode . lisp-mode)
                            (mode . clojure-mode)
                            (mode . clojurescript-mode)
                            (mode . python-mode)
                            (mode . c-mode)
                            (mode . c++-mode)))
            ("text" (mode . text-mode))
            ("LaTeX" (mode . latex-mode))
            ("magit" (mode . magit-mode))
            ("dired" (mode . dired-mode))
            ("emacs" (or
                      (name . "^\\*scratch\\*$")
                      (name . "^\\*Warnings\\*$")
                      (name . "^\\*Messages\\*$")))))))
(add-hook 'ibuffer-mode-hook
        (lambda ()
          (ibuffer-auto-mode 1)
          (ibuffer-switch-to-saved-filter-groups "default")))

;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

(use-package idle-highlight-mode
  :ensure t
  :config
  (setq idle-highlight-idle-time 0.2
        idle-highlight-exclude-point t)
  :hook
  ((prog-mode text-mode) . idle-highlight-mode))

(use-package iedit
  :ensure t
  :bind ("C-\"" . iedit-mode))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
   :map org-mode-map
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
   ("C-c m" . counsel-mark-ring)
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

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :config
  (all-the-icons-ivy-rich-mode 1)
  (setq all-the-icons-ivy-rich-color-icon t))

(use-package flx
  :ensure t)

(use-package orderless
  :ensure t
  :config
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package ivy-prescient
  :ensure t
  :config (ivy-prescient-mode 1))

(use-package json-mode
  :ensure t)

(use-package jump-char
  :ensure t
  :quelpa
  (dot-env :repo "lewang/jump-char"
           :fetcher github :upgrade t)
  :bind (("M-n" . jump-char-forward)
         ("M-N" . jump-char-backward)))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit)))

(defun amo/org-mode-hook ()
  (org-indent-mode 1)

  ;; For some reason org-mode doesn't respect visual-line-mode when it loads.
  (visual-line-mode 0)
  (visual-line-mode 1))
(add-hook 'org-mode-hook 'amo/org-mode-hook)

;; Disable checkdoc in org-mode source blocks
(defun amo/disable-fylcheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))
(add-hook 'org-src-mode-hook 'amo/disable-fylcheck-in-org-src-block)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c m") 'counsel-org-goto)

(setq org-list-demote-modify-bullet
      '(("-" . "+") ("+" . "*") ("*" . "-"))
      org-list-allow-alphabetical t
      org-M-RET-may-split-line '((default . nil))
      org-use-tag-inheritance nil
      org-hide-emphasis-markers t
      org-directory (dot-env-get 'ORG_DIRECTORY_PATH "~/.emacs.d/org-directory")
      org-agenda-files (list org-directory)
      org-sprint-file (concat org-directory "/sprints.org")
      org-standup-file (concat org-directory "/standups.org")
      org-issue-file (concat org-directory "/issues.org")
      org-meeting-file (concat org-directory "/meeting-notes.org")
      org-union-file (concat org-directory "/union-notes.org")
      org-lists-file (concat org-directory "/lists.org")
      org-archive-location (concat org-directory "/_archive/%s_archive::"))

(setq org-capture-templates
      '(("s"
         "Sprint"
         entry
         (file org-sprint-file)
         "* %? %^G \nDEADLINE: %^t SCHEDULED: %^t %^{SCOPED_STORY_POINTS}p\n\n** Issues\n" :empty-lines-after 1 :prepend t)
        ("p"
         "Sprint Issue"
         entry
         (file org-sprint-file)
         "*** %? %^G \n %^{STORY_POINTS}p %^{ORIGINALLY_SCOPED}p %^{COMPLETED}p" :empty-lines 1 :prepend t)
        ("i"
         "Issue Note"
         entry
         (file+headline org-issue-file "Issue Items")
         "** %? %^G \n%T \n%i \n" :empty-lines 1 :prepend t)
        ("t"
         "Standup Note"
         entry
         (file+headline org-standup-file "Standup Items")
         "** %T Notes:\n%?\n%i\n" :empty-lines-after 1 :prepend t)
        ("m"
         "Meeting Note"
         entry
         (file+headline org-meeting-file "Meeting Items")
         "** %?\n%T\n%i\n" :empty-lines-after 1 :prepend t)
        ("u"
         "Union Note"
         entry
         (file+headline org-union-file "Meeting Items")
         "** %T %?\n%i\n" :empty-lines-after 1)
        ("e"
         "Emacs Idea"
         checkitem
         (file+headline org-lists-file "Emacs Ideas")
         "[ ] %?" :prepend t)
        ("k"
         "Keyboard Idea"
         checkitem
         (file+headline org-lists-file "Keyboard Ideas")
         "[ ] %?" :prepend t)
        ("r"
         "Interesting Albums"
         item
         (file+headline org-lists-file "Interesting Albums")
         "%?")))

(use-package org-superstar
  :ensure t
  :hook
  (org-mode . org-superstar-mode))

(use-package package-lint
  :ensure t)

(use-package paredit
  :ensure t
  :hook
  ((lisp-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (clojure-mode . paredit-mode)
   (clojurescript-mode . paredit-mode)
   (clojurec-mode . paredit-mode)
   (cider-repl-mode . paredit-mode)))

(use-package popper
  :ensure t ; or :straight t
  :bind (("s-3"   . popper-toggle-latest)
         ("s-4"   . popper-cycle)
         ("s-5" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*format-all-errors\\*"
          "\\*lsp-log\\*"
          "\\*flycheck errors\\*"
          "\\*cider-error\\*"
          "\\*cider-scratch\\*"
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Backtrace\\*"
          "\\*TeX Help\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^pop-"
          help-mode
          compilation-mode)
        popper-mode-line ""

        ;; Make popper buffers 1/2 window height
        popper-window-height (lambda (win)
                               (fit-window-to-buffer
                                win
                                (floor (frame-height) 2))))
  (popper-mode +1)
  (popper-echo-mode +1)
  (defun amo/add-popper-status-to-modeline ()
    "If buffer is a popper-type buffer, display POP in the modeline,
  in a doom-modeline friendly way"
    (if (popper-display-control-p (buffer-name))
        (add-to-list 'mode-line-misc-info "POP")
      (setq mode-line-misc-info (remove "POP" mode-line-misc-info))))
  (add-hook 'buffer-list-update-hook 'amo/add-popper-status-to-modeline))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package transpose-frame
  :ensure t
  :bind (("C->" . transpose-frame)))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-resize-icons 14)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package
  treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
        undo-tree-auto-save-history t)
  :diminish
  (undo-tree-mode))

(use-package web-mode
  :ensure t
  :custom
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2)
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" .  web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :commands web-mode)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (use-package yasnippet-snippets
    :ensure t))

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 0)          ; Slows down Emacs a lot. Disabling until there's a fix
  (setq yascroll:delay-to-hide nil
        yascroll:disabled-modes '(package-menu-mode image-mode)))
