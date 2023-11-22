An emacs configuration for both GUI and terminal. Instructions on setting up emacsclient for the terminal config can be found [here](terminal/daemon-config.md).


# Table of Contents

-   [Common Functions](#org2a9b804)
-   [Common General Configuration](#org2dbf613)
    -   [Variables and Modes](#org9b1f451)
    -   [Functions for custom bindings](#org377f12c)
    -   [Custom Bindings](#orgec26d28)
    -   [dired](#orgfa2f487)
    -   [Advice](#org1aa8536)
-   [Common Packages](#orgd9d384f)
    -   [Minibuffer & Completion](#org420191e)
        -   [Vertico](#org03166e8)
        -   [Embark](#org7e4552f)
        -   [Marginalia](#org355195b)
        -   [Consult](#org55ffccb)
        -   [orderless](#orgacf8f9a)
        -   [nerd-icons-completion](#orgc0cd9e6)
        -   [kind-icon](#org6f463a6)
        -   [which-key](#orgdabb9cb)
    -   [Other Useful Packages](#orgd90d74f)
        -   [Ace Window](#orgdca1696)
        -   [aggressive-indent-mode](#org49921da)
        -   [Avy](#orgd4a3c7d)
        -   [Crux](#orgf285803)
        -   [expand-region](#org3ed2f24)
        -   [Idle Highlight Mode](#org8d93886)
        -   [jump-char](#org8ee789d)
        -   [minions](#orgbff0bdb)
        -   [Paredit](#org30a4be4)
        -   [undo-tree](#org72c6afb)
    -   [Appearance](#orgc5eddaf)
        -   [Nerd Icons](#orgf70736e)
        -   [nerd-icons-dired](#org966c662)
-   [GUI Config](#org62ed725)
    -   [General Configuration](#orgc12a398)
        -   [Variables and Modes](#orgf0267a1)
        -   [Functions for custom bindings](#orgf8a6491)
        -   [Custom Bindings](#org3bc0ff7)
        -   [Functions for hooks](#org796ff4d)
        -   [Hooks](#orgca0224b)
        -   [ibuffer](#orgd39f232)
    -   [Programming](#org5ac301b)
        -   [Packages](#org52395b5)
        -   [Languages](#orgba135d7)
    -   [Version Control](#orgaf90bc0)
        -   [Magit](#orgc0eccb7)
        -   [magit-todos](#org035afb5)
        -   [git-messenger](#org5db6c67)
        -   [Git time machine](#org151493a)
        -   [diff-hl](#orgee8bef8)
    -   [Minibuffer & Completion](#org50ce562)
        -   [consult-projectile](#orge1f809c)
        -   [Corfu](#orgc2cf05f)
    -   [Org Mode](#org6cf83c8)
        -   [Org configuration](#org464c4d2)
        -   [org-superstar-mode](#org0b52be1)
    -   [Other Useful Packages](#orga712fc1)
        -   [buffer-move](#org3953b53)
        -   [denote](#org01452bb)
        -   [exec-path-from-shell](#orgeb6c60f)
        -   [Eyebrowse](#org3d5bcae)
        -   [gptel](#org17df495)
        -   [helpful](#org4454a8b)
        -   [ibuffer-projectile](#org93675b1)
        -   [markdown-mode](#orgc15f29e)
        -   [package-lint](#org774cbc7)
        -   [Popper](#org6e37bc3)
        -   [Projectile](#org5835248)
        -   [rainbow-delimiters](#org6f1d220)
        -   [Transpose Frame](#org6e776a4)
        -   [YASnippet](#org494d683)
    -   [Appearance](#org7350d1a)
        -   [Dashboard](#org1b1f434)
        -   [Doom Modeline](#org5fcaeb4)
        -   [Ef Themes](#org35585f6)
        -   [Modus Themes](#org8609a42)
        -   [nerd-icons-corfu](#org5dbb5ee)
-   [Terminal Config](#orgbe3bc32)
    -   [General Configuration](#orgc4044c7)
        -   [Variables and Modes](#org400453d)
        -   [Functions for custom bindings](#org6b40ddb)
        -   [Custom Bindings](#orgb5b4517)
        -   [Packages](#org4f57c35)
        -   [Terminal mappings](#orgaca140c)


<a id="org2a9b804"></a>

# Common Functions

These are functions needed by both configurations during initial startup.

```emacs-lisp
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
```


<a id="org2dbf613"></a>

# Common General Configuration



<a id="org9b1f451"></a>

## Variables and Modes

```emacs-lisp
(use-package emacs
  :config
  ;; Use C-h for delete-backward-char
  (global-set-key (kbd "C-h") 'delete-backward-char)

  ;; Set custom prefix key ("C-z")
  (define-prefix-command 'z-map)
  (global-set-key (kbd "C-z") 'z-map)

  (global-display-line-numbers-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p)
  (recentf-mode 1)
  (savehist-mode 1)
  (whitespace-mode -1)
  (delete-selection-mode 1)
  (blink-cursor-mode 0)
  (electric-pair-mode 1)
  (put 'narrow-to-region 'disabled nil)

  (setq save-interprogram-paste-before-kill t
        require-final-newline t
        enable-recursive-minibuffers t)

  (setq-default indent-tabs-mode nil
                global-tab-line-mode nil
                tab-line-mode nil
                tab-bar-mode nil
                line-spacing 0.3
                fill-column 100
                sentence-end-double-space nil
                visual-line-mode t
                whitespace-line-column 110)

  ;; Blink modeline instead of ring bell
  (setq ring-bell-function
        (lambda ()
          (let ((orig-fg (face-foreground 'mode-line)))
            (set-face-foreground 'mode-line "Magenta")
            (run-with-idle-timer 0.1 nil
                                 (lambda (fg) (set-face-foreground 'mode-line fg))
                                 orig-fg))))
```


<a id="org377f12c"></a>

## Functions for custom bindings

```emacs-lisp
;; Un-highlight region after mark jump
(defun amo/exchange-point-and-mark ()
  "Deactivates mark after exchanging point and mark"
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark))

;; Transpose chars and words backwards
(defun amo/transpose-chars-backwards ()
  "Just like transpose-chars but goes the other way"
  (interactive)
  (transpose-chars -1))

(defun amo/transpose-words-backwards ()
  "Just like transpose-words but goes the other way"
  (interactive)
  (transpose-words -1))

;; Move char after point to end of next word
(defun amo/transpose-char-to-end-of-next-word ()
  "Move char at point to the end of the next word.
Use case is to push closing parentheses out to contain subsequent characters
when a function is typed and the closing parenthesis is automatically added.
Skips over periods, quotes, and closing parentheses."
  (interactive)

  ;; helpers
  (defun amo/should-move-forward-one-char (c)
    (or
     (char-equal (following-char) ?\")
     (char-equal (following-char) ?\))))
  (defun amo/should-move-forward-one-word (c)
    (char-equal (following-char) ?.))
  (defun amo/should-move (c)
    (or
     (amo/should-move-forward-one-char c)
     (amo/should-move-forward-one-word c)))

  (forward-char)
  (let ((start (point))
        (end nil))
    (save-excursion
      (forward-word)
      (while (amo/should-move (following-char))
        (if (amo/should-move-forward-one-char (following-char))
            (forward-char)
          (forward-word)))
      (setq end (point)))
    (transpose-subr 'forward-char (- end start)))
  (backward-char))

;; Move lines up and down
(defmacro amo/save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun amo/move-line-up ()
  (interactive)
  (amo/save-column
   (transpose-lines 1)
   (forward-line -2)))

(defun amo/move-line-down ()
  (interactive)
  (amo/save-column
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))

;; Toggle capitalization of character at point
(defun amo/toggle-capitalization ()
  "Toggle the capitalization of the character at point."
  (interactive)
  (let ((char (char-after)))
    (cond ((eq char (upcase char)) (progn (amo/downcase-char 1) (forward-char)))
          ((eq char (downcase char)) (progn (upcase-char 1) (forward-char)))
          (t (message "No character at point.")))))

(defun amo/downcase-char (arg)
  "Lowercasify ARG chars starting from point.  Point doesn't move."
  (interactive "p")
  (save-excursion
    (downcase-region (point) (progn (forward-char arg) (point)))))

(defun amo/toggle-line-numbers ()
  "Toggle the display of line numbers"
  (interactive)
  (if (eq nil global-display-line-numbers-mode)
      (global-display-line-numbers-mode 1)
    (global-display-line-numbers-mode 0)))
```


<a id="orgec26d28"></a>

## Custom Bindings

```emacs-lisp
:bind (("C-z l" . amo/toggle-line-numbers)
       ("C-x C-x" . amo/exchange-point-and-mark)
       ("C-S-t" . amo/transpose-chars-backwards)
       ("M-T" . amo/transpose-words-backwards)
       ("C-S-f" . amo/transpose-char-to-end-of-next-word)
       ("M-z" . zap-up-to-char)))
```


<a id="orgfa2f487"></a>

## dired

```emacs-lisp
(use-package dired
  :config  (setq dired-kill-when-opening-new-dired-buffer t
                 dired-listing-switches "-al")

  ;; from https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
  (defun mydired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (mydired-sort))

  :hook ((dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("C-o" . nil)))
```


<a id="org1aa8536"></a>

## Advice

```emacs-lisp
;; Copy whole line to kill ring when no active region
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))
```


<a id="orgd9d384f"></a>

# Common Packages



<a id="org420191e"></a>

## Minibuffer & Completion


<a id="org03166e8"></a>

### Vertico

```emacs-lisp
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("C-j" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))
```


<a id="org7e4552f"></a>

### Embark

```emacs-lisp
(use-package embark
  :straight t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-\"" . embark-dwim)        ;; good alternative: M-.
   ("C-S-h B" . embark-bindings)) ;; alternative for `describe-bindings'

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
```


<a id="org355195b"></a>

### Marginalia

```emacs-lisp
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))
```


<a id="org55ffccb"></a>

### Consult

```emacs-lisp
;; Example configuration for Consult
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
         ("s-r" . consult-recent-file)
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
```


<a id="orgacf8f9a"></a>

### orderless

```emacs-lisp
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))
```


<a id="orgc0cd9e6"></a>

### nerd-icons-completion

```emacs-lisp
(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup))
  :init
  (nerd-icons-completion-mode 1))
```


<a id="org6f463a6"></a>

### kind-icon

```emacs-lisp
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
```


<a id="orgdabb9cb"></a>

### which-key

```emacs-lisp
(use-package which-key
  :straight t
  :config
  (which-key-mode))
```


<a id="orgd90d74f"></a>

## Other Useful Packages


<a id="orgdca1696"></a>

### Ace Window

```emacs-lisp
(use-package ace-window
  :straight t
  :bind
  (("C-o" . ace-window)
   ("C-x o" . ace-window))
  :config
  (setq aw-scope 'frame
        aw-dispatch-always nil
        aw-keys '(?j ?k ?d ?s ?a ?g ?h ?k ?l)))
```


<a id="org49921da"></a>

### aggressive-indent-mode

```emacs-lisp
(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-ts-mode))
```


<a id="orgd4a3c7d"></a>

### Avy

```emacs-lisp
(use-package avy
  :straight t
  :bind (("C-;" . avy-goto-char-2)
         :map org-mode-map
         ("C-'" . nil)
         :map flyspell-mode-map
         ("C-;" . nil)))
```


<a id="orgf285803"></a>

### Crux

```emacs-lisp
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
```


<a id="org3ed2f24"></a>

### expand-region

```emacs-lisp
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
```


<a id="org8d93886"></a>

### Idle Highlight Mode

```emacs-lisp
(use-package idle-highlight-mode
  :straight t
  :config
  (setq idle-highlight-idle-time 0.2
        idle-highlight-exclude-point t)
  :hook
  ((prog-mode text-mode) . idle-highlight-mode))

```


<a id="org8ee789d"></a>

### jump-char

```emacs-lisp
(use-package jump-char
  :straight (:host github :repo "lewang/jump-char"
               :branch "master")
  :bind (("M-n" . jump-char-forward)
         ("M-N" . jump-char-backward)))
```


<a id="orgbff0bdb"></a>

### minions

```emacs-lisp
(use-package minions
  :straight t
  :config
  (minions-mode 1))
```


<a id="org30a4be4"></a>

### Paredit

```emacs-lisp
(use-package paredit
  :straight t
  :hook
  ((lisp-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (clojure-mode . paredit-mode)
   (clojurescript-mode . paredit-mode)
   (clojurec-mode . paredit-mode)
   (cider-repl-mode . paredit-mode)))
```


<a id="org72c6afb"></a>

### undo-tree

```emacs-lisp
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
        undo-tree-enable-undo-in-region t
        undo-tree-auto-save-history t)
  :diminish
  (undo-tree-mode))
```


<a id="orgc5eddaf"></a>

## Appearance


<a id="orgf70736e"></a>

### Nerd Icons

```emacs-lisp
(use-package nerd-icons
  :straight t)
```


<a id="org966c662"></a>

### nerd-icons-dired

```emacs-lisp
(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))
```


<a id="org62ed725"></a>

# GUI Config



<a id="orgc12a398"></a>

## General Configuration


<a id="orgf0267a1"></a>

### Variables and Modes

```emacs-lisp
(use-package emacs
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)

  :config
  ;; Maximize frames when created
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Use C-H for help prefix
  (global-set-key (kbd "C-S-h") 'help-command)

  (set-frame-font amo/font nil t)
  (set-face-attribute 'fixed-pitch nil :font amo/font)
  (set-face-attribute 'variable-pitch nil :font amo/font)
  (set-face-attribute 'default nil :height amo/base-font-size)
  (desktop-save-mode 1)
  (global-hl-line-mode t)

  (setq warning-minimum-level :error
        scroll-step 1
        frame-inhibit-implied-resize t
        recentf-max-menu-items 25
        recentf-max-saved-items 25
        inferior-lisp-program "/usr/local/bin/sbcl"
        font-latex-fontify-script nil
        auto-save-default nil
        create-lockfiles nil
        delete-by-moving-to-trash t
        comment-column 64

        ;; Change mode based on file extension
        auto-mode-alist (append '(("\\.cl$" . lisp-mode)
                                  ("\\.mod$" . go-mod-ts-mode))
                                auto-mode-alist)

        ;; Save backups to a central location
        make-backup-files t
        backup-directory-alist '(("." . "~/.emacs.d/backups")) ;TODO: Don't hardcode
        delete-old-versions -1
        version-control t
        vc-make-backup-files t
        indent-tabs-mode nil
        auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;TODO: Don't hardcode
  (setq-default org-catch-invisible-edits 'show)
```


<a id="orgf8a6491"></a>

### Functions for custom bindings

```emacs-lisp
;; Open settings.org (this file)
(defun amo/open-settings-file ()
  "Open settings.org"
  (interactive)
  (find-file (concat user-emacs-directory "settings.org")))

;; Reload config
(defun amo/reload-config ()
  "Reload configuration"
  (interactive)
  ;; (dot-env-load)
  (load-file (concat user-emacs-directory "init.el")))

;; Delete desktop files and restart emacs
(defun amo/delete-emacs-desktop-files-restart ()
  "Delete saved desktop, then restart emacs"
  (interactive)
  (setq desktop-save-mode nil)
  (delete-file (concat user-emacs-directory ".emacs.desktop"))
  (delete-file (concat user-emacs-directory ".emacs.desktop.lock"))
  (restart-emacs))

;; Move point to other window immediately after split
(defun amo/split-window-below ()
  "Create a new window below and move point to new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun amo/split-window-horizontally()
  "Create a new window to the right and move point to new window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))


(defun amo/open-notes-directory ()
  "Open notes directory"
  (interactive)
  (find-file org-directory))

(defun amo/open-denote-directory ()
  "Open denote directory"
  (interactive)
  (find-file (concat org-directory "/denote")))
```


<a id="org3bc0ff7"></a>

### Custom Bindings

```emacs-lisp
:bind (
       ;; Custom prefix C-z
       ("C-z s" . amo/open-settings-file)
       ("C-z r" . amo/reload-config)
       ("C-z C-z r" . restart-emacs)
       ("C-z C-z C-z r" . amo/delete-emacs-desktop-files-restart)
       ("C-z t" . customize-themes)
       ("C-z e" . eshell)
       ("C-z n" . amo/open-notes-directory)
       ("C-z d" . amo/open-denote-directory)

       ;; Other bindings
       ("C-x 2" . amo/split-window-below)
       ("C-x 3" . amo/split-window-horizontally)
       ("M-<down>" . amo/move-line-down)
       ("M-<up>" . amo/move-line-up)
       ("C-'" . amo/toggle-capitalization)
       ("C-s-p" . scroll-down-line)
       ("C-s-n" . scroll-up-line)
       ("C-x K" . kill-buffer-and-window)
       ("M-SPC" . cycle-spacing)
       )
```


<a id="org796ff4d"></a>

### Functions for hooks

```emacs-lisp
:config
;; Don't auto-complete when we are debugging
(defun amo/comint-mode-actions ()
  (setq-local corfu-auto nil) [])

;; Cleanup whitespace
(defun amo/whitespace-cleanup ()
  (whitespace-cleanup)
  (delete-trailing-whitespace))
```


<a id="orgca0224b"></a>

### Hooks

```emacs-lisp
:hook ((comint-mode . amo/comint-mode-actions)
       (before-save . amo/whitespace-cleanup)
       (focus-out . garbage-collect)
       (text-mode . flyspell-mode)))
```


<a id="orgd39f232"></a>

### ibuffer

```emacs-lisp
(use-package ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . nil)))
```


<a id="org5ac301b"></a>

## Programming


<a id="org52395b5"></a>

### Packages

-   treesit-auto

    ```emacs-lisp
    (use-package treesit-auto
      :straight t
      :config
      (setq treesit-auto-install 'prompt)
      (global-treesit-auto-mode))
    ```

-   Eglot

    ```emacs-lisp
    (defun amo/eglot-format-on-save ()
      (when (bound-and-true-p eglot--managed-mode)
        (eglot-format)))
    
    (use-package eglot
      :straight t
      :defer t
      :hook ((after-save . amo/eglot-format-on-save)
             (python-ts-mode . eglot-ensure)
             (bash-ts-mode . eglot-ensure)
             (go-ts-mode . eglot-ensure)
             (haskell-mode . eglot-ensure))
      :bind (:map eglot-mode-map
                  ("<C-return>" . xref-find-references)
                  ("C-c e f n" . flymake-goto-next-error)
                  ("C-c e f p" . flymake-goto-prev-error)
                  ("C-c e r" . eglot-rename)
                  ("C-c e w r" . eglot-reconnect)))
    ```

-   RealGUD

    ```emacs-lisp
    (use-package realgud
      :straight t)
    ```


<a id="orgba135d7"></a>

### Languages

-   Golang

    -   go-ts-mode
    
        ```emacs-lisp
        (use-package go-ts-mode
          :hook (go-ts-mode . (lambda () (setq-local tab-width 4
                                                     electric-indent-inhibit t)
                                (aggressive-indent-mode -1))))
        ```

-   Haskell

    -   haskell-mode
    
        ```emacs-lisp
        (use-package haskell-mode
          :straight t
          :config (setq haskell-interactive-popup-errors nil)
          :hook (haskell-mode . (lambda () (interactive-haskell-mode t))))
        ```

-   Python

    -   interpreter
    
        ```emacs-lisp
        (when (executable-find "ipython")
          (setq python-shell-interpreter "ipython"))
        ```
    
    -   conda
    
        ```emacs-lisp
        (use-package conda
          :straight t
          :init
          (setq conda-anaconda-home (expand-file-name "~/opt/miniconda3")
                conda-env-home-directory (expand-file-name "~/opt/miniconda3")
                conda-env-autoactivate-mode t)
        
          (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
                                                 (conda-env-activate-for-buffer))))
          (setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format)))
        ```
    
    -   pyvenv
    
        ```emacs-lisp
        (use-package pyvenv
          :straight t
          :diminish
          :config
          (setq pyvenv-mode-line-indicator
                '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
          (pyvenv-mode +1))
        ```

-   Docker

    -   dockerfile-mode
    
        ```emacs-lisp
        (use-package dockerfile-mode
          :straight t)
        ```
    
    -   docker-compose-mode
    
        ```emacs-lisp
        (use-package docker-compose-mode
          :straight t)
        ```


<a id="orgaf90bc0"></a>

## Version Control


<a id="orgc0eccb7"></a>

### Magit

```emacs-lisp
(use-package magit
  :straight t
  :bind
  (("C-x g" . magit)))
```


<a id="org035afb5"></a>

### magit-todos

```emacs-lisp
(use-package magit-todos
  :straight t
  :hook ((magit-mode . magit-todos-mode)))
```


<a id="org5db6c67"></a>

### git-messenger

```emacs-lisp
(use-package git-messenger
  :straight t
  :config (setq git-messenger:show-detail t
                git-messenger:use-magit-popup t)
  :bind ("C-x m" . git-messenger:popup-message))
```


<a id="org151493a"></a>

### Git time machine

```emacs-lisp
(use-package git-timemachine
  :straight t)
```


<a id="orgee8bef8"></a>

### diff-hl

```emacs-lisp
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))
```


<a id="org50ce562"></a>

## Minibuffer & Completion


<a id="orge1f809c"></a>

### consult-projectile

```emacs-lisp
(use-package consult-projectile
  :straight t
  :config
  (define-key projectile-command-map (kbd "h") #'consult-projectile)
  (define-key projectile-command-map (kbd "f") #'consult-projectile-find-file)
  (define-key projectile-command-map (kbd "d") #'consult-projectile-find-dir)
  (define-key projectile-command-map (kbd "p") #'consult-projectile-switch-project)
  (define-key projectile-command-map (kbd "b") #'consult-projectile-switch-to-buffer))
```


<a id="orgc2cf05f"></a>

### Corfu

```emacs-lisp
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  (corfu-echo-mode 1)
  :custom (setq corfu-quit-at-boundary t)
  :config (setq corfu-auto t
                corfu-auto-prefix 1
                corfu-quit-no-match t
                corfu-popupinfo-delay '(1.0 . 0.5)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))
```


<a id="org6cf83c8"></a>

## Org Mode


<a id="org464c4d2"></a>

### Org configuration

```emacs-lisp
(use-package org
  :config
  (setq org-directory (dot-env-get 'ORG_DIRECTORY_PATH
                                   (concat user-emacs-directory "org-directory"))
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-notes-file (concat org-directory "/notes.org")
        org-union-file (concat org-directory "/union-notes.org")
        org-lists-file (concat org-directory "/lists.org")
        org-archive-location (concat org-directory "/_archive/%s_archive::"))

  (defun amo/org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1))

  ;; Disable checkdoc in org-mode source blocks
  (defun amo/disable-flycheck-in-org-src-block ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

  :hook
  (org-mode . amo/org-mode-hook)
  (org-src-mode . amo/disable-flycheck-in-org-src-block)

  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c h" . consult-org-heading))

  :custom
  (setq org-use-tag-inheritance t)
  (org-hide-emphasis-markers t)
  (org-list-demote-modify-bullet
   '(("-" . "+") ("+" . "*") ("*" . "-")))
  (org-list-allow-alphabetical t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-capture-templates
   '(("n"
      "General Note"
      entry
      (file org-notes-file)
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
      "%?"))))

(use-package ox-gfm
  :straight t
  :config
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))
```


<a id="org0b52be1"></a>

### org-superstar-mode

```emacs-lisp
(use-package org-superstar
  :straight t
  :hook
  (org-mode . org-superstar-mode))
```


<a id="orga712fc1"></a>

## Other Useful Packages


<a id="org3953b53"></a>

### buffer-move

```emacs-lisp
(use-package buffer-move
  :straight t
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
```


<a id="org01452bb"></a>

### denote

```emacs-lisp
(use-package denote
  :straight t
  :after org
  :config
  (setq denote-directory (dot-env-get 'DENOTE_DIRECTORY (concat org-directory "/denote"))
        denote-date-prompt-use-org-read-date t
        denote-known-keywords nil
        denote-allow-multi-word-keywords t)
  :hook ((dired-mode . denote-dired-mode))
  :bind (("C-c n n" . denote)
         ("C-c n N" . denote-type)
         ("C-c n d" . denote-date)
         ("C-c n z" . denote-signature)
         ("C-c n s" . denote-subdirectory)
         ("C-c n t" . denote-template)
         ("C-c n i" . denote-link)
         ("C-c n I" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n f f" . denote-find-link)
         ("C-c n f b" . denote-find-backlink)
         ("C-c n k a" . denote-keywords-add)
         ("C-c n k k" . denote-keywords-remove)
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)
         :map dired-mode-map
         ("C-c C-d C-i . denote-link-dired-marked-notes")
         ("C-c C-d C-r . denote-dired-rename-marked-files")
         ("C-c C-d C-R . denote-dired-rename-marked-files-using-front-matter")))
```


<a id="orgeb6c60f"></a>

### exec-path-from-shell

```emacs-lisp
(when (memq window-system '(mac ns x)) ;; Linux
  (use-package exec-path-from-shell
    :straight t
    :config
    (exec-path-from-shell-initialize)))
```


<a id="org3d5bcae"></a>

### Eyebrowse

```emacs-lisp
(use-package eyebrowse
  :straight t
  :config
  (setq eyebrowse-new-workspace (lambda () (dashboard-open)))
  (eyebrowse-mode))
```


<a id="org17df495"></a>

### gptel

```emacs-lisp
(use-package gptel
  :straight t
  :config
  (setq gptel-api-key (dot-env-get 'GPTEL_API_KEY)
        gptel-default-mode #'org-mode)
  :bind (("C-c g" . gptel)))
```


<a id="org4454a8b"></a>

### helpful

```emacs-lisp
(use-package helpful
  :straight t
  :bind (("C-S-h f" . helpful-callable)
         ("C-S-h v" . helpful-variable)
         ("C-S-h o" . helpful-symbol)
         ("C-S-h k" . helpful-key)
         ("C-S-h x" . helpful-command)
         ("C-S-h d" . helpful-at-point)
         ("C-S-h F" . helpful-function)))
```


<a id="org93675b1"></a>

### ibuffer-projectile

```emacs-lisp
(use-package ibuffer-projectile
  :straight t
  :after projectile
  :config (setq ibuffer-show-empty-filter-groups nil)
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))
         (ibuffer-mode . (lambda ()
                           (ibuffer-auto-mode 1)
                           (ibuffer-switch-to-saved-filter-groups "default"))))
  :bind (("C-x C-b" . ibuffer)))
```


<a id="orgc15f29e"></a>

### markdown-mode

```emacs-lisp
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
```


<a id="org774cbc7"></a>

### package-lint

```emacs-lisp
(use-package package-lint
  :straight t)
```


<a id="org6e37bc3"></a>

### Popper

```emacs-lisp
(use-package popper
  :straight t
  :bind (("s-3" . popper-toggle)
         ("s-4" . popper-cycle)
         ("s-5" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*format-all-errors\\*"
          "\\*cider-error\\*"
          "\\*cider-scratch\\*"
          "\\*Messages\\*"
          "\\*helpful"
          "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Backtrace\\*"
          "\\*TeX Help\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc\\*"
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
```


<a id="org5835248"></a>

### Projectile

```emacs-lisp
(use-package projectile
  :straight t
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'alien
        projectile-ignored-projects '("~/"))
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))
```


<a id="org6f1d220"></a>

### rainbow-delimiters

```emacs-lisp
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))
```


<a id="org6e776a4"></a>

### Transpose Frame

```emacs-lisp
(use-package transpose-frame
  :straight t
  :bind (("C->" . transpose-frame)))
```


<a id="org494d683"></a>

### YASnippet

```emacs-lisp
(use-package yasnippet
  :straight t
  :hook ((python-ts-mode . (lambda () (yas-activate-extra-mode 'python-mode))))
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")) ;TODO: Don't hardcode
  (use-package yasnippet-snippets
    :straight t))
```


<a id="org7350d1a"></a>

## Appearance


<a id="org1b1f434"></a>

### Dashboard

```emacs-lisp
(use-package dashboard
  :straight t
  :after nerd-icons
  :config
  (setq dashboard-center-content t
        dashboard-banner-logo-title "No! The beard stays. You go."
        dashboard-startup-banner 'logo
        dashboard-items '((projects . 10)
                          (recents  . 10)
                          (bookmarks . 5))
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t
        dashboard-set-footer nil)
  (dashboard-open))
```


<a id="org5fcaeb4"></a>

### Doom Modeline

```emacs-lisp
(straight-use-package '(f :type git :host github :repo "rejeep/f.el"))
(use-package doom-modeline
  :straight t
  :after f
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t
        doom-modeline-vcs-max-length 40
        doom-modeline-buffer-encoding t))
```


<a id="org35585f6"></a>

### Ef Themes

```emacs-lisp
(use-package ef-themes
  :straight t)
```


<a id="org8609a42"></a>

### Modus Themes

```emacs-lisp
(use-package modus-themes
  :straight t)
```


<a id="org5dbb5ee"></a>

### nerd-icons-corfu

```emacs-lisp
(use-package nerd-icons-corfu
  :straight t)
```


<a id="orgbe3bc32"></a>

# Terminal Config



<a id="orgc4044c7"></a>

## General Configuration


<a id="org400453d"></a>

### Variables and Modes

```emacs-lisp
(use-package emacs
  :config
  (setq inhibit-startup-screen t
        x-select-enable-clipboard t
        auto-save-default nil
        make-backup-files nil
        create-lockfiles nil)

  (add-hook 'before-save-hook 'whitespace-cleanup)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

```


<a id="org6b40ddb"></a>

### Functions for custom bindings

```emacs-lisp
(defun amo/modus-themes-toggle (&optional ignore-if-set)
  "Toggle modus theme, sets to operandi if unset"
  (interactive)
  (if (string-match "modus" (symbol-name (car custom-enabled-themes)))
      (when (not ignore-if-set)
        (modus-themes-toggle))
    (load-theme 'modus-operandi)))
(amo/modus-themes-toggle t)

(defun amo/open-settings-file ()
  "Open settings.el"
  (interactive)
  (find-file "~/.emacs.d/terminal/settings.org"))

;; Reload config
(defun amo/reload-config ()
  "Reload configuration"
  (interactive)
  ;; (dot-env-load)
  (load-file "~/.emacs.d/terminal/init.el"))
```


<a id="orgb5b4517"></a>

### Custom Bindings

```emacs-lisp
:bind (("C-z s" . amo/open-settings-file)
       ("C-z r" . amo/reload-config)
       ("C-z t" . amo/modus-themes-toggle)))
```


<a id="org4f57c35"></a>

### Packages

-   Magit

    ```emacs-lisp
    (use-package magit
      :straight t
      :bind
      (("C-x g" . magit)))
    ```

-   company

    ```emacs-lisp
    (use-package company
      :straight t
      :config
      (global-company-mode))
    ```


<a id="orgaca140c"></a>

### Terminal mappings

```emacs-lisp
(defun amo/define-escape-sequences-and-bindings ()
  (interactive)
  (progn
    (let ((map (if (boundp 'input-decode-map)
                   input-decode-map
                 function-key-map)))

      ;; C-=
      (define-key map "\e[1;P9"  (kbd "H-a"))
      (global-set-key (kbd "H-a") 'er/expand-region)

      ;; C--
      (define-key map "\e[1;P10" (kbd "H-b"))
      (global-set-key (kbd "H-b") 'er/contract-region) ;; FIXME - maps to undo for some reason

      ;; C-;
      (define-key map "\e[1;P11" (kbd "H-c"))
      (global-set-key (kbd "H-c") 'avy-goto-char-2)
      (global-set-key (kbd "C-x H-c") 'comment-line)

      ;; s-o
      (define-key map "\e[1;P12" (kbd "H-d"))
      (global-set-key (kbd "H-d") 'crux-smart-open-line-above)

      ;; s-j
      (define-key map "\e[1;P13" (kbd "H-e"))
      (global-set-key (kbd "H-e") 'crux-top-join-line)

      ;; s-k
      (define-key map "\e[1;P14" (kbd "H-f"))
      (global-set-key (kbd "H-f") 'crux-kill-whole-line)

      ;; C-s-p
      (define-key map "\e[1;P15" (kbd "H-g"))
      (global-set-key (kbd "H-g") 'scroll-down-line)

      ;; C-s-n
      (define-key map "\e[1;P16" (kbd "H-h"))
      (global-set-key (kbd "H-h") 'scroll-up-line)

      ;; C-S-h
      (define-key map "\e[1;P17" (kbd "H-i"))
      (global-set-key (kbd "H-i") 'help-command)

      ;; M-S<down>
      (define-key map "\e[1;P18" (kbd "H-j"))
      (global-set-key (kbd "H-j") 'amo/move-line-down)

      ;; M-S-<up>
      (define-key map "\e[1;P19" (kbd "H-k"))
      (global-set-key (kbd "H-k") 'amo/move-line-up)

      ;; C-'
      (define-key map "\e[1;P20" (kbd "H-l"))
      (global-set-key (kbd "H-l") 'amo/toggle-capitalization)

      ;; C-.
      (define-key map "\e[1;P21" (kbd "H-m"))
      (global-set-key (kbd "H-m") 'embark-act)

      ;; s-r
      (define-key map "\e[1;P22" (kbd "H-n"))
      (global-set-key (kbd "H-n") 'consult-recent-file)

      ;; Additional available keybindings
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
      (define-key map "\e[1;P56" (kbd "H-<f12>")))))

(add-hook 'server-after-make-frame-hook 'amo/define-escape-sequences-and-bindings)
(amo/define-escape-sequences-and-bindings)
```
