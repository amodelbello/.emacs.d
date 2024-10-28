An emacs configuration for both GUI and terminal. Instructions on setting up emacsclient for the terminal config can be found [here](https://github.com/amodelbello/.emacs.d/blob/main/terminal/daemon-config.org).


# Table of Contents

<<<<<<< HEAD
-   [Initial Setup](#orgbeb5ee9)
-   [Common Functions](#orge43e679)
-   [Common General Configuration](#orgbf1d7e3)
    -   [Variables and Modes](#orgf007795)
    -   [Functions for custom bindings](#orgca4ea07)
    -   [Custom Bindings](#org646d36c)
    -   [Hooks](#org36d0646)
    -   [dired](#org5527f09)
    -   [Advice](#org56c10c1)
-   [Common Packages](#org450942d)
    -   [Minibuffer & Completion](#org0fd1349)
        -   [Vertico](#orgbba031f)
        -   [Embark](#orgc72a00f)
        -   [Marginalia](#org9174f1d)
        -   [Consult](#orgf149517)
        -   [orderless](#org66d3493)
        -   [which-key](#org09dcd6e)
    -   [Other Useful Packages](#org3b808e9)
        -   [Ace Window](#orgb64f7c1)
        -   [aggressive-indent-mode](#orgb8368d8)
        -   [Avy](#org14cef52)
        -   [Crux](#org6b1987b)
        -   [el-patch](#orgb4824dc)
        -   [expand-region](#org236f423)
        -   [Idle Highlight Mode](#org8177a79)
        -   [jump-char](#org2f0d73d)
        -   [minions](#orgfc5444c)
        -   [move-lines](#org28d6ee0)
        -   [Paredit](#orgef3a06e)
        -   [undo-tree](#org7d1b6e8)
    -   [Appearance](#org525ccbb)
        -   [Nerd Icons](#org4309444)
        -   [nerd-icons-dired](#org868cc9e)
        -   [nerd-icons-completion](#orgb6f945a)
        -   [kind-icon](#org841567f)
-   [GUI Config](#orgf7fca35)
    -   [General Configuration](#orga2f939d)
        -   [Variables and Modes](#org9b58e3c)
        -   [Functions for custom bindings](#org76625e5)
        -   [Custom Bindings](#org4b6cb56)
        -   [Functions for hooks](#org9f273c7)
        -   [Hooks](#org197ea5a)
        -   [ibuffer](#org0e267f8)
    -   [Programming](#org0ea0ac9)
        -   [Packages](#orge85e206)
        -   [Languages](#org211b266)
    -   [Version Control](#org62e6e7f)
        -   [Magit](#org847149c)
        -   [magit-todos](#org9684286)
        -   [git-messenger](#org0ee1782)
        -   [Git time machine](#org8aaccc0)
        -   [diff-hl](#orga735284)
    -   [Minibuffer & Completion](#org5212094)
        -   [consult-projectile](#org6fdcd91)
        -   [consult-eglot](#org53ed79a)
        -   [Corfu](#org99cd3a7)
    -   [Org Mode](#org472390a)
        -   [Org configuration](#orgc5caaf3)
        -   [Org Modern](#org43f23db)
    -   [Other Useful Packages](#org1af8e61)
        -   [buffer-move](#orgbef07fb)
        -   [Casual Suite](#orgc1a75c8)
        -   [consult-denote](#orgfbb9220)
        -   [denote](#org6f69797)
        -   [exec-path-from-shell](#orge5a3a26)
        -   [Eyebrowse](#orgfab4f03)
        -   [flymake-margin](#orgc97d2a3)
        -   [gptel](#org14a2620)
        -   [helpful](#org0659a1c)
        -   [ibuffer-projectile](#org1bf7c9f)
        -   [markdown-mode](#org80c3a3f)
        -   [package-lint](#orgeea17b5)
        -   [Popper](#org3e2d86b)
        -   [Projectile](#org0823ed1)
        -   [ESS](#orgc67c8ca)
        -   [rainbow-delimiters](#org7877684)
        -   [Transpose Frame](#org05e2bb7)
        -   [YASnippet](#org0eb693c)
    -   [Appearance](#org7f4fc6b)
        -   [Dashboard](#org40e9aab)
        -   [Doom Modeline](#org7e62d91)
        -   [Ef Themes](#org17e5f24)
        -   [Modus Themes](#org56b577c)
        -   [nerd-icons-corfu](#orgbeee2b6)
        -   [magit-file-icons](#org06bd21c)
-   [Terminal Config](#org62535af)
    -   [General Configuration](#orge683d3f)
        -   [Variables and Modes](#org450a714)
        -   [Functions for custom bindings](#org5fd451c)
        -   [Custom Bindings](#org86285f3)
        -   [Packages](#orgc5b4670)
        -   [Terminal mappings](#org63e304f)


<a id="orgbeb5ee9"></a>
=======
-   [Initial Setup](#org7b4663e)
-   [Common Functions](#orgba66f32)
-   [Common General Configuration](#orgc703fa5)
    -   [Variables and Modes](#org0a463c5)
    -   [Functions for custom bindings](#org2d253f5)
    -   [Custom Bindings](#org9577e40)
    -   [Hooks](#orgdddccec)
    -   [dired](#org22d6be0)
    -   [Advice](#org4996b26)
-   [Common Packages](#orgd6ea301)
    -   [Minibuffer & Completion](#orgf9ccc00)
        -   [Vertico](#org189df8f)
        -   [Embark](#orge7987b6)
        -   [Marginalia](#org6c23cb4)
        -   [Consult](#orgc4b3ee8)
        -   [orderless](#orgde1cb80)
        -   [which-key](#orga99fa80)
    -   [Other Useful Packages](#orgd231dfd)
        -   [Ace Window](#org229391d)
        -   [aggressive-indent-mode](#org09f5a7c)
        -   [Avy](#orgdecaf38)
        -   [Crux](#orgfe5630e)
        -   [el-patch](#org28b5941)
        -   [expand-region](#org4113a47)
        -   [Idle Highlight Mode](#org9358c15)
        -   [jump-char](#org657cdb9)
        -   [minions](#org5454fdd)
        -   [move-lines](#org3585bd7)
        -   [Paredit](#org568848e)
        -   [undo-tree](#org9328e4a)
    -   [Appearance](#orga77cb16)
        -   [Nerd Icons](#org5df10a1)
        -   [nerd-icons-dired](#orgd8f6414)
        -   [nerd-icons-completion](#org5b37ee9)
        -   [kind-icon](#orgf198f18)
-   [GUI Config](#org80fb85b)
    -   [General Configuration](#org90dcb66)
        -   [Variables and Modes](#org1299dae)
        -   [Functions for custom bindings](#org332bcf2)
        -   [Custom Bindings](#org089f54e)
        -   [Functions for hooks](#orge717926)
        -   [Hooks](#orgef2c0dc)
        -   [ibuffer](#org7afb19b)
    -   [Programming](#orga96ce2a)
        -   [Packages](#orge213982)
        -   [Languages](#orge3cda58)
    -   [Version Control](#org9f1b23f)
        -   [Magit](#org0cb3276)
        -   [magit-todos](#orgca2d701)
        -   [git-messenger](#org5e9b10a)
        -   [Git time machine](#orgcd5a287)
        -   [diff-hl](#org4dea61f)
    -   [Minibuffer & Completion](#orga0f20ab)
        -   [consult-projectile](#org9c0a719)
        -   [consult-eglot](#org6e23d28)
        -   [Corfu](#org988aead)
    -   [Org Mode](#orgba2c17d)
        -   [Org configuration](#org14ec458)
        -   [Org Modern](#orgac22862)
    -   [Other Useful Packages](#org28060c7)
        -   [buffer-move](#orgd305625)
        -   [Casual Suite](#org4b296b1)
        -   [consult-denote](#orgf9a07b6)
        -   [denote](#org7c88eac)
        -   [exec-path-from-shell](#org27e7c81)
        -   [Eyebrowse](#org8703846)
        -   [flymake-margin](#org39791b2)
        -   [gptel](#org070a3ab)
        -   [helpful](#org36223c8)
        -   [ibuffer-projectile](#orgc7b361a)
        -   [markdown-mode](#org7480579)
        -   [package-lint](#orgf2d347e)
        -   [Popper](#org48cd241)
        -   [Projectile](#org79e501c)
        -   [ESS](#orgd0669d9)
        -   [rainbow-delimiters](#orgf32299e)
        -   [Transpose Frame](#orgcda2dd6)
        -   [YASnippet](#orge32db6c)
    -   [Appearance](#org63c0d0b)
        -   [Dashboard](#orgd0ce60a)
        -   [Doom Modeline](#org3fc157e)
        -   [Ef Themes](#org295f188)
        -   [Modus Themes](#org762e890)
        -   [nerd-icons-corfu](#org9b252a8)
        -   [magit-file-icons](#orgbb394be)
-   [Terminal Config](#org5199128)
    -   [General Configuration](#orgfb32850)
        -   [Variables and Modes](#org74cb1b3)
        -   [Functions for custom bindings](#orge710a6c)
        -   [Custom Bindings](#orgba86315)
        -   [Packages](#orgaa0b24d)
        -   [Terminal mappings](#org7cf6593)


<a id="org7b4663e"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

# Initial Setup

Before opening emacs with this configuration there are a few set up tasks that need to be performed:

1.  Install nerd-fonts: <https://github.com/rainstormstudio/nerd-icons.el>
2.  Create the following directories in `./emacs.d`
    -   `org-directory`
    -   `org-directory/denote`
    -   `backups`
3.  Configure environment variables: <https://github.com/amodelbello/dot-env.el> example `.env` file: [.env.example](.env.example)


<<<<<<< HEAD
<a id="orge43e679"></a>
=======
<a id="orgba66f32"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

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


<<<<<<< HEAD
<a id="orgbf1d7e3"></a>
=======
<a id="orgc703fa5"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

# Common General Configuration



<<<<<<< HEAD
<a id="orgf007795"></a>
=======
<a id="org0a463c5"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

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
  (visual-line-mode 1)
  (put 'narrow-to-region 'disabled nil)

  (setq save-interprogram-paste-before-kill t
        vc-follow-symlinks nil
        require-final-newline t
        enable-recursive-minibuffers t
        ispell-program-name (dot-env-get 'ISPELL_PATH "/usr/bin/ispell"))

  (setq-default indent-tabs-mode nil
                global-tab-line-mode nil
                tab-line-mode nil
                tab-bar-mode nil
                line-spacing 0.3
                fill-column 100
                sentence-end-double-space nil
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


<<<<<<< HEAD
<a id="orgca4ea07"></a>
=======
<a id="org2d253f5"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Functions for custom bindings

```emacs-lisp
(defun amo/copy-rectangle-to-kill-ring (Begin End)
  "Copy region as column (rectangle region) to `kill-ring'
(Because the native copy-rectangle-as-kill doesn't seem to work)
See also: `kill-rectangle', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_rectangle_text_to_clipboard.html'
Version: 2016-07-17"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat #'identity (extract-rectangle Begin End) "\n")))

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


<<<<<<< HEAD
<a id="org646d36c"></a>
=======
<a id="org9577e40"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Custom Bindings

```emacs-lisp
:bind (("C-z l" . amo/toggle-line-numbers)
       ("M-'" . end-of-visual-line)
       ("C-x C-x" . amo/exchange-point-and-mark)
       ("C-S-t" . amo/transpose-chars-backwards)
       ("M-T" . amo/transpose-words-backwards)
       ("C-S-f" . amo/transpose-char-to-end-of-next-word)
       ("M-z" . zap-up-to-char)
       ("C-x r M-w" . amo/copy-rectangle-to-kill-ring)))
```


<<<<<<< HEAD
<a id="org36d0646"></a>
=======
<a id="orgdddccec"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Hooks

```emacs-lisp
(add-hook 'before-save-hook 'amo/whitespace-cleanup)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook (lambda () (flyspell-mode -1)))
```


<<<<<<< HEAD
<a id="org5527f09"></a>
=======
<a id="org22d6be0"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

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


<<<<<<< HEAD
<a id="org56c10c1"></a>
=======
<a id="org4996b26"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

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


<<<<<<< HEAD
<a id="org450942d"></a>
=======
<a id="orgd6ea301"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

# Common Packages



<<<<<<< HEAD
<a id="org0fd1349"></a>
=======
<a id="orgf9ccc00"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Minibuffer & Completion


<<<<<<< HEAD
<a id="orgbba031f"></a>
=======
<a id="org189df8f"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Vertico

-   **Repo:** <https://github.com/minad/vertico>
-   **Description:** Performant and minimalistic vertical completion UI based on the default completion system.

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


<<<<<<< HEAD
<a id="orgc72a00f"></a>
=======
<a id="orge7987b6"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Embark

-   **Repo:** <https://github.com/oantolin/embark>
-   **Description:** Emacs Mini-Buffer Actions Rooted in Keymaps

```emacs-lisp
(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("C-S-h B" . embark-bindings)
         :map flyspell-mode-map
         ("C-." . nil))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
```


<<<<<<< HEAD
<a id="org9174f1d"></a>
=======
<a id="org6c23cb4"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Marginalia

-   **Repo:** <https://github.com/minad/marginalia>
-   **Description:** Enable rich annotations in the minibuffer

```emacs-lisp
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))
```


<<<<<<< HEAD
<a id="orgf149517"></a>
=======
<a id="orgc4b3ee8"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Consult

-   **Repo:** <https://github.com/minad/consult>
-   **Description:** Search and navigation commands based on the Emacs completion function completing-read

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


<<<<<<< HEAD
<a id="org66d3493"></a>
=======
<a id="orgde1cb80"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### orderless

-   **Repo:** <https://github.com/oantolin/orderless>
-   **Description:** Emacs completion style that matches multiple regexps in any order

```emacs-lisp
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))
```


<<<<<<< HEAD
<a id="org09dcd6e"></a>
=======
<a id="orga99fa80"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### which-key

-   **Repo:** <https://github.com/justbur/emacs-which-key>
-   **Description:** Emacs package that displays available keybindings in popup

```emacs-lisp
(use-package which-key
  :straight t
  :config
  (which-key-mode))
```


<<<<<<< HEAD
<a id="org3b808e9"></a>
=======
<a id="orgd231dfd"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Other Useful Packages


<<<<<<< HEAD
<a id="orgb64f7c1"></a>
=======
<a id="org229391d"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Ace Window

-   **Repo:** <https://github.com/abo-abo/ace-window>
-   **Description:** Quickly switch windows in Emacs

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


<<<<<<< HEAD
<a id="orgb8368d8"></a>
=======
<a id="org09f5a7c"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### aggressive-indent-mode

-   **Repo:** <https://github.com/Malabarba/aggressive-indent-mode>
-   **Description:** Emacs minor mode that keeps your code always indented

```emacs-lisp
(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-ts-mode))
```


<<<<<<< HEAD
<a id="org14cef52"></a>
=======
<a id="orgdecaf38"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Avy

-   **Repo:** <https://github.com/abo-abo/avy>
-   **Description:** Jumping to visible text using a char-based decision tree

```emacs-lisp
(use-package avy
  :straight t
  :bind (("C-;" . avy-goto-char-2)
         :map org-mode-map
         ("C-'" . nil)
         :map flyspell-mode-map
         ("C-;" . nil)))
```


<<<<<<< HEAD
<a id="org6b1987b"></a>
=======
<a id="orgfe5630e"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Crux

-   **Repo:** <https://github.com/bbatsov/crux>
-   **Docs:** <https://emacsredux.com/blog/2016/01/30/crux>
-   **Description:** A Collection of Ridiculously Useful eXtensions for Emacs

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


<<<<<<< HEAD
<a id="orgb4824dc"></a>
=======
<a id="org28b5941"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### el-patch

-   **Repo:** <https://github.com/radian-software/el-patch>
-   **Description:** ✨ Future-proof your Emacs Lisp customizations!

```emacs-lisp
(use-package el-patch
  :straight t)
```


<<<<<<< HEAD
<a id="org236f423"></a>
=======
<a id="org4113a47"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### expand-region

-   **Repo:** <https://github.com/magnars/expand-region.el>
-   **Description:** Emacs extension to increase selected region by semantic units.

```emacs-lisp
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
```


<<<<<<< HEAD
<a id="org8177a79"></a>
=======
<a id="org9358c15"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Idle Highlight Mode

-   **Repo:** <https://codeberg.org/ideasman42/emacs-idle-highlight-mode>
-   **Description:** Simple symbol highlighting package for Emacs

```emacs-lisp
(use-package idle-highlight-mode
  :straight t
  :config
  (setq idle-highlight-idle-time 0.2
        idle-highlight-exclude-point t)
  :hook
  ((prog-mode text-mode) . idle-highlight-mode))

```


<<<<<<< HEAD
<a id="org2f0d73d"></a>
=======
<a id="org657cdb9"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### jump-char

-   **Repo:** <https://github.com/lewang/jump-char>
-   **Description:** Navigation by character occurrence

```emacs-lisp
(use-package jump-char
  :straight (:host github :repo "lewang/jump-char"
               :branch "master")
  :bind (("M-n" . jump-char-forward)
         ("M-N" . jump-char-backward)))
```


<<<<<<< HEAD
<a id="orgfc5444c"></a>
=======
<a id="org5454fdd"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### minions

-   **Repo:** <https://github.com/tarsius/minions>
-   **Description:** A minor-mode menu for the mode line

```emacs-lisp
(use-package minions
  :straight t
  :config
  (minions-mode 1))
```


<<<<<<< HEAD
<a id="org28d6ee0"></a>
=======
<a id="org3585bd7"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### move-lines

-   **Repo:** <https://github.com/targzeta/move-lines>
-   **Description:** Emacs: moves current line or lines surrounding region up or down.
    
    ```emacs-lisp
    (use-package move-lines
      :straight (:host github :repo "targzeta/move-lines"
                       :branch "master")
      :config
      (global-set-key (kbd "M-<down>") 'move-lines-down))
      (global-set-key (kbd "M-<up>") 'move-lines-up)
    ```


<<<<<<< HEAD
<a id="orgef3a06e"></a>
=======
<a id="org568848e"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Paredit

-   **Repo:** <https://github.com/emacsmirror/paredit/blob/master/paredit.el>
-   **Docs:** <https://www.emacswiki.org/emacs/ParEdit>, <https://wikemacs.org/wiki/Paredit-mode>
-   **Description:** A minor mode for performing structured editing of S-expression data

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


<<<<<<< HEAD
<a id="org7d1b6e8"></a>
=======
<a id="org9328e4a"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### undo-tree

-   **Repo:** <https://github.com/apchamberlain/undo-tree.el>
-   **Docs:** <https://www.emacswiki.org/emacs/UndoTree>
-   **Description:** Visualize Emacs undo information as a graphical tree and navigate to previous states

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


<<<<<<< HEAD
<a id="org525ccbb"></a>
=======
<a id="orga77cb16"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Appearance


<<<<<<< HEAD
<a id="org4309444"></a>
=======
<a id="org5df10a1"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Nerd Icons

-   **Repo:** <https://github.com/rainstormstudio/nerd-icons.el>
-   **Description:** A library for easily using Nerd Font icons inside Emacs

```emacs-lisp
(use-package nerd-icons
  :straight t)
```


<<<<<<< HEAD
<a id="org868cc9e"></a>
=======
<a id="orgd8f6414"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### nerd-icons-dired

-   **Repo:** <https://github.com/rainstormstudio/nerd-icons-dired>
-   **Description:** Use nerd-icons for Dired

```emacs-lisp
(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))
```


<<<<<<< HEAD
<a id="orgb6f945a"></a>
=======
<a id="org5b37ee9"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### nerd-icons-completion

-   **Repo:** <https://github.com/rainstormstudio/nerd-icons-completion>
-   **Description:** Icons for candidates in minibuffer

```emacs-lisp
(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup))
  :init
  (nerd-icons-completion-mode 1))
```


<<<<<<< HEAD
<a id="org841567f"></a>
=======
<a id="orgf198f18"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### kind-icon

-   **Repo:** <https://github.com/jdtsmith/kind-icon>
-   **Description:** Completion kind text/icon prefix labelling for emacs in-region completion

```emacs-lisp
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
```


<<<<<<< HEAD
<a id="orgf7fca35"></a>
=======
<a id="org80fb85b"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

# GUI Config

Configuration and packages specific to GUI


<<<<<<< HEAD
<a id="orga2f939d"></a>
=======
<a id="org90dcb66"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## General Configuration


<<<<<<< HEAD
<a id="org9b58e3c"></a>
=======
<a id="org1299dae"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Variables and Modes

```emacs-lisp
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
(menu-bar-mode (string-to-number (dot-env-get 'MENU_BAR_MODE "1")))

(setq warning-minimum-level :error
      default-directory (dot-env-get 'DEFAULT_DIRECTORY "~/")
      scroll-step 1
      ;; split-width-threshold 120
      ;; split-height-threshold 160
      compilation-window-height 24
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
                              auto-mode-alist))
(setq-default org-catch-invisible-edits 'show)

;; Save backups to a central location
(let ((backup-directory (dot-env-get 'BACKUP_DIRECTORY "~/.emacs.d/backups/")))
  (setq backup-directory-alist `(("." . ,backup-directory))
        make-backup-files t
        delete-old-versions -1
        version-control t
        vc-make-backup-files t
        indent-tabs-mode nil))
```


<<<<<<< HEAD
<a id="org76625e5"></a>
=======
<a id="org332bcf2"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Functions for custom bindings

```emacs-lisp
;; Open settings.org (this file)
(defun amo/open-settings-file ()
  "Open settings.org"
  (interactive)
  (find-file (concat user-emacs-directory "gui/settings.org")))

;; Open common settings.org
(defun amo/open-common-settings-file ()
  "Open common/settings.org"
  (interactive)
  (find-file (concat user-emacs-directory "common/settings.org")))

;; Reload config
(defun amo/reload-config ()
  "Reload configuration"
  (interactive)
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
  (balance-windows)
  (other-window 1))

(defun amo/split-window-horizontally()
  "Create a new window to the right and move point to new window."
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1))

(defun amo/delete-window ()
  "Balance windows after deleting one"
  (interactive)
  (delete-window)
  (balance-windows))

(defun amo/open-notes-directory ()
  "Open notes directory"
  (interactive)
  (find-file org-directory))

(defun amo/open-denote-directory ()
  "Open denote directory"
  (interactive)
  (find-file (concat org-directory "/denote")))
```


<<<<<<< HEAD
<a id="org4b6cb56"></a>
=======
<a id="org089f54e"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Custom Bindings

```emacs-lisp

;; Custom prefix C-z
(global-set-key (kbd "C-z s") 'amo/open-settings-file)
(global-set-key (kbd "C-z C-z s") 'amo/open-common-settings-file)
(global-set-key (kbd "C-z r") 'amo/reload-config)
(global-set-key (kbd "C-z C-z r") 'restart-emacs)
(global-set-key (kbd "C-z C-z C-z r") 'amo/delete-emacs-desktop-files-restart)
(global-set-key (kbd "C-z t") 'customize-themes)
(global-set-key (kbd "C-z e") 'eshell)
(global-set-key (kbd "C-z n") 'amo/open-notes-directory)
(global-set-key (kbd "C-z d") 'amo/open-denote-directory)

;; Other bindings
(global-set-key (kbd "C-x 0") 'amo/delete-window)
(global-set-key (kbd "C-x 2") 'amo/split-window-below)
(global-set-key (kbd "C-x 3") 'amo/split-window-horizontally)
(global-set-key (kbd "M-<down>") 'amo/move-line-down)
(global-set-key (kbd "M-<up>") 'amo/move-line-up)
(global-set-key (kbd "C-'") 'amo/toggle-capitalization)
(global-set-key (kbd "C-s-p") 'scroll-down-line)
(global-set-key (kbd "C-s-n") 'scroll-up-line)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

```


<<<<<<< HEAD
<a id="org9f273c7"></a>
=======
<a id="orge717926"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Functions for hooks

```emacs-lisp
;; Don't auto-complete when we are debugging
(defun amo/comint-mode-actions ()
  (setq-local corfu-auto nil) [])

;; Cleanup whitespace
(defun amo/whitespace-cleanup ()
  (whitespace-cleanup)
  (delete-trailing-whitespace))
```


<<<<<<< HEAD
<a id="org197ea5a"></a>
=======
<a id="orgef2c0dc"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Hooks

```emacs-lisp
(add-hook 'comint-mode-hook 'amo/comint-mode-actions)
(add-hook 'focus-out-hook 'garbage-collect)
```


<<<<<<< HEAD
<a id="org0e267f8"></a>
=======
<a id="org7afb19b"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### ibuffer

```emacs-lisp
(use-package ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . nil)))
```


<<<<<<< HEAD
<a id="org0ea0ac9"></a>
=======
<a id="orga96ce2a"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Programming


<<<<<<< HEAD
<a id="orge85e206"></a>
=======
<a id="orge213982"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Packages

-   treesit-auto

    -   **Repo:** <https://github.com/renzmann/treesit-auto>
    -   **Description:** Automatic installation, usage, and fallback for tree-sitter major modes in Emacs
    
    ```emacs-lisp
    (use-package treesit-auto
      :straight t
      :custom
      (treesit-auto-install 'prompt)
      :config
      (treesit-auto-add-to-auto-mode-alist 'all)
      (global-treesit-auto-mode))
    ```

-   Eglot

    -   **Repo:** <https://github.com/joaotavora/eglot>
    -   **Description:** A client for Language Server Protocol servers
    
    ```emacs-lisp
    (defun amo/eglot-format-on-save ()
      (when (and (not (eq major-mode 'java-ts-mode)) (bound-and-true-p eglot--managed-mode))
        (eglot-format)))
    
    (use-package eglot
      :straight t
      :defer t
      :config (add-to-list 'eglot-server-programs
                           `((java-mode java-ts-mode) .
                             ("jdtls"
                              :initializationOptions
                              (:bundles [,(dot-env-get 'JAVA_DEBUG_SERVER_PATH)]))))
      :hook ((after-save . amo/eglot-format-on-save)
             (python-ts-mode . eglot-ensure)
             (bash-ts-mode . eglot-ensure)
             (json-ts-mode . eglot-ensure)
             (go-ts-mode . eglot-ensure)
             (haskell-mode . eglot-ensure)
             (rust-ts-mode . eglot-ensure)
             (java-ts-mode . eglot-ensure)
             (c-ts-mode . eglot-ensure)
             (c++-ts-mode . eglot-ensure))
      :bind (:map eglot-mode-map
                  ("<C-return>" . xref-find-references)
                  ("C-c e f" . consult-flymake)
                  ("C-c e r" . eglot-rename)
                  ("C-c e a" . eglot-code-actions)
                  ("C-c e c" . compile)
                  ("C-c e w r" . eglot-reconnect)))
    ```

-   Dape

    -   **Repo:** <https://github.com/svaante/dape>
    -   **Description:** Debug Adapter Protocol for Emacs
    
    ```emacs-lisp
    (use-package dape
      :straight t
      :config
      (setq dape-buffer-window-arrangement 'right)
      (setq dape-cwd-fn 'projectile-project-root))
    ```


<<<<<<< HEAD
<a id="org211b266"></a>
=======
<a id="orge3cda58"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Languages

-   Docker

    -   dockerfile-mode
    
        -   **Repo:** <https://github.com/spotify/dockerfile-mode>
        
        ```emacs-lisp
        (use-package dockerfile-mode
          :straight t)
        ```
    
    -   docker-compose-mode
    
        -   **Repo:** <https://github.com/meqif/docker-compose-mode>
        
        ```emacs-lisp
        (use-package docker-compose-mode
          :straight t)
        ```

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
    
        -   **Repo:** <https://github.com/haskell/haskell-mode>
        -   **Description:** Emacs mode for Haskell
        
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
    
        -   **Repo:** <https://github.com/necaris/conda.el>
        -   **Description:** Emacs helper library (and minor mode) to work with conda environments
        
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
    
        -   **Repo:** <https://github.com/jorgenschaefer/pyvenv>
        -   **Description:** Python virtual environment interface for Emacs
        
        ```emacs-lisp
        (use-package pyvenv
          :straight t
          :diminish
          :config
          (setq pyvenv-mode-line-indicator
                '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
          (pyvenv-mode +1))
        ```

-   Java

    -   eglot-java
    
        -   **Repo:** <https://github.com/yveszoundi/eglot-java>
        -   **Description:** Java extension for the eglot LSP client
        
        ```emacs-lisp
        (use-package eglot-java
          :straight t
          :bind
          (("C-c l n" . eglot-java-file-new)
           ("C-c l x" . eglot-java-run-main)
           ("C-c l t" . eglot-java-run-test)
           ("C-c l N" . eglot-java-project-new)
           ("C-c l T" . eglot-java-project-build-task)
           ("C-c l R" . eglot-java-project-build-refresh)))
        ```

-   Jinja2

    -   **Repo:** <https://github.com/paradoxxxzero/jinja2-mode>
    -   **Description:** Jinja2 mode for emacs
    
    ```emacs-lisp
    (use-package jinja2-mode
      :straight t)
    ```

-   SuperCollider

    -   **Repo:** <https://github.com/supercollider/scel>
    -   **Description:** Supercollider emacs package
    
    ```emacs-lisp
    (when (executable-find "sclang")
      (require 'sclang))
    ```


<<<<<<< HEAD
<a id="org62e6e7f"></a>
=======
<a id="org9f1b23f"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Version Control


<<<<<<< HEAD
<a id="org847149c"></a>
=======
<a id="org0cb3276"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Magit

-   **Repo:** <https://github.com/magit/magit>
-   **Docs:** <https://magit.vc/>
-   **Description:** It's Magit! A Git Porcelain inside Emacs.

```emacs-lisp
(use-package magit
  :straight t
  :bind
  (("C-x g" . magit)))
```


<<<<<<< HEAD
<a id="org9684286"></a>
=======
<a id="orgca2d701"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### magit-todos

-   **Repo:** <https://github.com/alphapapa/magit-todos>
-   **Description:** Show source files' TODOs (and FIXMEs, etc) in Magit status buffer

```emacs-lisp
(use-package magit-todos
  :straight t
  :hook ((magit-mode . magit-todos-mode)))
```


<<<<<<< HEAD
<a id="org0ee1782"></a>
=======
<a id="org5e9b10a"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### git-messenger

-   **Repo:** <https://github.com/emacsorphanage/git-messenger>
-   **Description:** Emacs Port of git-messenger.vim

```emacs-lisp
(use-package git-messenger
  :straight t
  :config (setq git-messenger:show-detail t
                git-messenger:use-magit-popup t)
  :bind ("C-x m" . git-messenger:popup-message))
```


<<<<<<< HEAD
<a id="org8aaccc0"></a>
=======
<a id="orgcd5a287"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Git time machine

-   **Repo:** <https://github.com/emacsmirror/git-timemachine>
-   **Description:** Walk through git revisions of a file

```emacs-lisp
(use-package git-timemachine
  :straight t)
```


<<<<<<< HEAD
<a id="orga735284"></a>
=======
<a id="org4dea61f"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### diff-hl

-   **Repo:** <https://github.com/dgutov/diff-hl>
-   **Description:** Emacs package for highlighting uncommitted changes

```emacs-lisp
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind
  (("C-c e n" . diff-hl-show-hunk-next)
   ("C-c e p" . diff-hl-show-hunk-previous)))
```


<<<<<<< HEAD
<a id="org5212094"></a>
=======
<a id="orga0f20ab"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Minibuffer & Completion


<<<<<<< HEAD
<a id="org6fdcd91"></a>
=======
<a id="org9c0a719"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### consult-projectile

-   **Repo:** <https://github.com/emacsmirror/consult-projectile>
-   **Description:** Consult integration for projectile

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


<<<<<<< HEAD
<a id="org53ed79a"></a>
=======
<a id="org6e23d28"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### consult-eglot

-   **Repo:** <https://github.com/mohkale/consult-eglot>
-   **Description:** Jump to workspace symbols with eglot and consult
    
    ```emacs-lisp
    (use-package consult-eglot
      :straight t)
    ```


<<<<<<< HEAD
<a id="org99cd3a7"></a>
=======
<a id="org988aead"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Corfu

-   **Repo:** <https://github.com/minad/corfu>
-   **Description:** corfu.el - COmpletion in Region FUnction

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
;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)
```


<<<<<<< HEAD
<a id="org472390a"></a>
=======
<a id="orgba2c17d"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Org Mode


<<<<<<< HEAD
<a id="orgc5caaf3"></a>
=======
<a id="org14ec458"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Org configuration

-   **Docs:** <https://orgmode.org/>
-   **Description:** A GNU Emacs major mode for keeping notes, authoring documents, computational notebooks, literate programming, maintaining to-do lists, planning projects, and more — in a fast and effective plain text system.

```emacs-lisp
(use-package org
  :config
  (setq org-directory (dot-env-get 'ORG_DIRECTORY
                                   (concat user-emacs-directory "org-directory"))
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-notes-file (concat org-directory "/notes.org")
        org-lists-file (concat org-directory "/lists.org")
        org-archive-location (concat org-directory "/_archive/%s_archive::")
        org-yank-folded-subtrees nil)

  (defun amo/org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1))

  (defun amo/org-mode-agenda-hook ()
    "For some reason the org-agenda-files var is never set"
    (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")))

  (advice-add
   'org-agenda
   :before
   (lambda (&rest r) (amo/org-mode-agenda-hook)))

  :hook
  (org-mode . amo/org-mode-hook)

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
      "Note"
      entry
      (file org-notes-file)
      "** %?\n%T\n%i\n" :empty-lines-after 1 :prepend t)
     ("t"
      "Todo"
      checkitem
      (file+headline org-notes-file "General Todos")
      "[ ] %T %?\n%i\n" :empty-lines-after 1)
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


<<<<<<< HEAD
<a id="org43f23db"></a>
=======
<a id="orgac22862"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Org Modern

-   **Repo:** <https://github.com/minad/org-modern>
-   **Description:** 🦄 Modern Org Style
    
    ```emacs-lisp
    (use-package org-modern
      :straight t
      :config (with-eval-after-load 'org (global-org-modern-mode)))
    ```


<<<<<<< HEAD
<a id="org1af8e61"></a>
=======
<a id="org28060c7"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Other Useful Packages


<<<<<<< HEAD
<a id="orgbef07fb"></a>
=======
<a id="orgd305625"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### buffer-move

-   **Repo:** <https://github.com/lukhas/buffer-move>
-   **Description:** Easily swap buffers

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


<<<<<<< HEAD
<a id="orgc1a75c8"></a>
=======
<a id="org4b296b1"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Casual Suite

-   **Repo:** <https://github.com/kickingvegas/casual-suite>
-   **Description:** Casual Suite - An umbrella package to support a single install point for all Casual porcelains.
    
    ```emacs-lisp
    (use-package casual-suite
      :straight t
      :bind
      (
       ("C-," . casual-avy-tmenu)
       :map Calc-mode-map ("C-i" . casual-calc-tmenu)
       :map Info-mode-map ("C-i" . casual-info-tmenu)
       :map dired-mode-map ("C-i" . casual-dired-tmenu)
       :map isearch-mode-map ("<f2>" . casual-isearch-tmenu)))
    ```


<<<<<<< HEAD
<a id="orgfbb9220"></a>
=======
<a id="orgf9a07b6"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### consult-denote

-   **Repo:** <https://github.com/protesilaos/consult-denote>
-   **Docs:** <https://protesilaos.com/emacs/consult-denote>
-   **Description:** Use Consult in tandem with Denote

```emacs-lisp
(use-package consult-denote
  :straight t
  :after denote
  :bind (("C-c n f" . consult-denote-find)
         ("C-c n g" . consult-denote-grep)))
```


<<<<<<< HEAD
<a id="org6f69797"></a>
=======
<a id="org7c88eac"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### denote

-   **Repo:** <https://github.com/protesilaos/denote>
-   **Docs:** <https://protesilaos.com/emacs/denote>
-   **Description:** Simple notes for Emacs with an efficient file-naming scheme

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


<<<<<<< HEAD
<a id="orge5a3a26"></a>
=======
<a id="org27e7c81"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### exec-path-from-shell

-   **Repo:** <https://github.com/purcell/exec-path-from-shell>
-   **Description:** Make Emacs use the $PATH set up by the user's shell

```emacs-lisp
(when (memq window-system '(mac ns x)) ;; Linux
  (use-package exec-path-from-shell
    :straight t
    :config
    (exec-path-from-shell-initialize)))
```


<<<<<<< HEAD
<a id="orgfab4f03"></a>
=======
<a id="org8703846"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Eyebrowse

-   **Repo:** <https://depp.brause.cc/eyebrowse/>
-   **Description:** A simple-minded way of managing window configs in Emacs

```emacs-lisp
(use-package eyebrowse
  :straight t
  :config
  (setq eyebrowse-new-workspace (lambda () (dashboard-open)))
  (global-set-key (kbd "C-c C-w n") 'eyebrowse-create-named-window-config)
  (eyebrowse-mode))
```


<<<<<<< HEAD
<a id="orgc97d2a3"></a>
=======
<a id="org39791b2"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### flymake-margin

-   **Repo:** <https://github.com/LionyxML/flymake-margin>
-   **Description:** A package to provide flymake into the margin world

```emacs-lisp
(use-package flymake-margin
  :straight (margin :type git
                    :host github
                    :repo "LionyxML/flymake-margin"
                    :files ("*.el"))
  :after flymake
  :config
  (flymake-margin-mode t))
```


<<<<<<< HEAD
<a id="org14a2620"></a>
=======
<a id="org070a3ab"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### gptel

-   **Repo:** <https://github.com/karthink/gptel>
-   **Description:** A no-frills ChatGPT client for Emacs

```emacs-lisp
(use-package gptel
  :straight t
  :config
  (setq gptel-api-key (dot-env-get 'GPTEL_API_KEY)
        gptel-default-mode #'org-mode)
  :bind (("C-c g" . gptel)))
```


<<<<<<< HEAD
<a id="org0659a1c"></a>
=======
<a id="org36223c8"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### helpful

-   **Repo:** <https://github.com/Wilfred/helpful>
-   **Description:** A better Emacs **help** buffer

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


<<<<<<< HEAD
<a id="org1bf7c9f"></a>
=======
<a id="orgc7b361a"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### ibuffer-projectile

-   **Repo:** <https://github.com/purcell/ibuffer-projectile>
-   **Description:** Group buffers in Emacs ibuffer-mode by their projectile root directory

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


<<<<<<< HEAD
<a id="org80c3a3f"></a>
=======
<a id="org7480579"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### markdown-mode

-   **Repo:** <https://github.com/jrblevin/markdown-mode>
-   **Description:** Emacs Markdown Mode

```emacs-lisp
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
```


<<<<<<< HEAD
<a id="orgeea17b5"></a>
=======
<a id="orgf2d347e"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### package-lint

-   **Repo:** <https://github.com/purcell/package-lint>
-   **Description:** A linting library for elisp package metadata

```emacs-lisp
(use-package package-lint
  :straight t)
```


<<<<<<< HEAD
<a id="org3e2d86b"></a>
=======
<a id="org48cd241"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Popper

-   **Repo:** <https://github.com/karthink/popper>
-   **Description:** Emacs minor-mode to summon and dismiss buffers easily

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


<<<<<<< HEAD
<a id="org0823ed1"></a>
=======
<a id="org79e501c"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Projectile

-   **Repo:** <https://github.com/bbatsov/projectile>
-   **Docs:** <https://docs.projectile.mx/projectile/index.html>
-   **Description:** Project navigation and management library for Emacs

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


<<<<<<< HEAD
<a id="orgc67c8ca"></a>
=======
<a id="orgd0669d9"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### ESS

-   **Repo:** <https://github.com/emacs-ess/ESS>
-   **Docs:** <https://ess.r-project.org/>

```emacs-lisp
(use-package ess
  :straight t)
```


<<<<<<< HEAD
<a id="org7877684"></a>
=======
<a id="orgf32299e"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### rainbow-delimiters

-   **Repo:** <https://github.com/Fanael/rainbow-delimiters>
-   **Description:** A "rainbow parentheses"-like mode which highlights delimiters

```emacs-lisp
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))
```


<<<<<<< HEAD
<a id="org05e2bb7"></a>
=======
<a id="orgcda2dd6"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Transpose Frame

-   **Docs:** <https://www.emacswiki.org/emacs/TransposeFrame>
-   **Description:** Interactive functions to transpose window arrangement in current frame

```emacs-lisp
(use-package transpose-frame
  :straight t
  :bind (("C->" . transpose-frame)))
```


<<<<<<< HEAD
<a id="org0eb693c"></a>
=======
<a id="orge32db6c"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### YASnippet

-   **Repo:** <https://github.com/joaotavora/yasnippet>
-   **Description:** A template system for Emacs

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


<<<<<<< HEAD
<a id="org7f4fc6b"></a>
=======
<a id="org63c0d0b"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## Appearance


<<<<<<< HEAD
<a id="org40e9aab"></a>
=======
<a id="orgd0ce60a"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Dashboard

-   **Repo:** <https://github.com/emacs-dashboard/emacs-dashboard>
-   **Description:** An extensible emacs dashboard

```emacs-lisp
(use-package dashboard
  :straight t
  :after nerd-icons
  :config
  (setq dashboard-center-content t
        dashboard-banner-logo-title "No! The beard stays. You go."
        dashboard-startup-banner 'logo
        dashboard-projects-backend 'projectile
        dashboard-items '((projects . 10)
                          (recents  . 10)
                          (bookmarks . 10))
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t
        dashboard-set-footer nil
        dashboard-footer-messages '(""))
  (dashboard-open))
```


<<<<<<< HEAD
<a id="org7e62d91"></a>
=======
<a id="org3fc157e"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Doom Modeline

-   **Repo:** <https://github.com/seagle0128/doom-modeline>
-   **Description:** A fancy and fast mode-line inspired by minimalism design

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


<<<<<<< HEAD
<a id="org17e5f24"></a>
=======
<a id="org295f188"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Ef Themes

-   **Repo:** <https://github.com/protesilaos/ef-themes>
-   **Description:** Colourful and legible themes for GNU Emacs

```emacs-lisp
(use-package ef-themes
  :straight t)
```


<<<<<<< HEAD
<a id="org56b577c"></a>
=======
<a id="org762e890"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Modus Themes

-   **Repo:** <https://github.com/protesilaos/modus-themes>
-   **Description:** Highly accessible themes for GNU Emacs

```emacs-lisp
(use-package modus-themes
  :straight t)
```


<<<<<<< HEAD
<a id="orgbeee2b6"></a>
=======
<a id="org9b252a8"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### nerd-icons-corfu

-   **Repo:** <https://github.com/LuigiPiucco/nerd-icons-corfu>
-   **Description:** Icons for corfu via nerd-icons

```emacs-lisp
(use-package nerd-icons-corfu
  :straight t)
```


<<<<<<< HEAD
<a id="org06bd21c"></a>
=======
<a id="orgbb394be"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### magit-file-icons

-   **Repo:** <https://github.com/gekoke/magit-file-icons>
-   **Description:** File icons for Magit

```emacs-lisp
(use-package magit-file-icons
  ;; :straight (margin :type git
  ;;                   :host github
  ;;                   :repo "gekoke/magit-file-icons"
  ;;                   :files ("*.el"))
  :straight t
  :init
  (magit-file-icons-mode 1)
  :custom
  ;; These are the default values:
  (magit-file-icons-enable-diff-file-section-icons t)
  (magit-file-icons-enable-untracked-icons t)
  (magit-file-icons-enable-diffstat-icons t))
```


<<<<<<< HEAD
<a id="org62535af"></a>
=======
<a id="org5199128"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

# Terminal Config

Configuration and packages specific to terminal


<<<<<<< HEAD
<a id="orge683d3f"></a>
=======
<a id="orgfb32850"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

## General Configuration


<<<<<<< HEAD
<a id="org450a714"></a>
=======
<a id="org74cb1b3"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Variables and Modes

```emacs-lisp
(use-package emacs
  :config
  (setq-default left-margin-width 1 right-margin-width 1)
  (set-window-margins nil 1)
  (setq inhibit-startup-screen t
        x-select-enable-clipboard t
        auto-save-default nil
        make-backup-files nil
        create-lockfiles nil)

  (add-hook 'before-save-hook 'whitespace-cleanup)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

```


<<<<<<< HEAD
<a id="org5fd451c"></a>
=======
<a id="orge710a6c"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Functions for custom bindings

```emacs-lisp
(defun amo/modus-themes-toggle (&optional ignore-if-set)
  "Toggle modus theme, sets to vivendi if unset"
  (interactive)
  (if (string-match "modus" (symbol-name (car custom-enabled-themes)))
      (when (not ignore-if-set)
        (modus-themes-toggle))
    (load-theme 'modus-vivendi)))
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


<<<<<<< HEAD
<a id="org86285f3"></a>
=======
<a id="orgba86315"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Custom Bindings

```emacs-lisp
:bind (("C-z s" . amo/open-settings-file)
       ("C-z r" . amo/reload-config)
       ("C-z t" . amo/modus-themes-toggle)))
```


<<<<<<< HEAD
<a id="orgc5b4670"></a>
=======
<a id="orgaa0b24d"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Packages

-   company

    -   **Repo:** <https://github.com/company-mode/company-mode>
    -   **Docs:** <https://company-mode.github.io/>
    -   **Description:** Modular in-buffer completion framework for Emacs
    
    [corfu-terminal](https://codeberg.org/akib/emacs-corfu-terminal) doesn't seem to work for some reason. Company is just fine though.
    
    ```emacs-lisp
    (use-package company
      :straight t
      :config
      (global-company-mode))
    ```


<<<<<<< HEAD
<a id="org63e304f"></a>
=======
<a id="org7cf6593"></a>
>>>>>>> 4970123 (Fix java debugger with eglot)

### Terminal mappings

-   **Info:** <https://www.emacswiki.org/emacs/iTerm2#h5o-11>

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
