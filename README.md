An emacs configuration for both GUI and terminal. Instructions on setting up emacsclient for the terminal config can be found [here](https://github.com/amodelbello/.emacs.d/blob/main/terminal/daemon-config.org).


# Table of Contents

-   [Initial Setup](#orgc9f66d4)
-   [Common Functions](#org9b44f29)
-   [Common General Configuration](#org95d92c3)
    -   [Variables and Modes](#orgb05378f)
    -   [Functions for custom bindings](#org0cb4c54)
    -   [use-package Style Bindings](#org02b5c69)
    -   [Traditional-Style Bindings](#orgc0eb5b2)
    -   [Hooks](#orga66423c)
    -   [dired](#org51a02d0)
    -   [Advice](#orgfe85715)
-   [Common Packages](#orgb509296)
    -   [Minibuffer & Completion](#orgd1bcd5c)
        -   [Vertico](#org3206b1f)
        -   [Embark](#org97cc136)
        -   [Marginalia](#orgeef6501)
        -   [Consult](#org9aba27b)
        -   [orderless](#org3a69aa2)
        -   [which-key](#orgd17261d)
    -   [Other Useful Packages](#org40c31ef)
        -   [Ace Window](#org727d73e)
        -   [aggressive-indent-mode](#orgb9cc6b9)
        -   [Avy](#org8ea6808)
        -   [Crux](#orgb37d96f)
        -   [el-patch](#orga2f821f)
        -   [expand-region](#orgd319db7)
        -   [Geiser (Scheme)](#org207e655)
        -   [Idle Highlight Mode](#org3b6c7f9)
        -   [jump-char](#orgf2c1380)
        -   [minions](#org1a548d6)
        -   [move-lines](#org2323bb2)
        -   [Paredit](#orgdfbd801)
        -   [rg.el](#orgefaeb00)
        -   [sicp-info](#org8ddf014)
        -   [undo-tree](#orged09503)
    -   [Appearance](#orgccf307f)
        -   [Standard Themes](#org32b42ef)
        -   [Nerd Icons](#org57401e9)
        -   [nerd-icons-dired](#org220cb35)
        -   [nerd-icons-completion](#orga16dca5)
        -   [kind-icon](#org17dbc5f)
-   [GUI Config](#org9502a75)
    -   [General Configuration](#orgac63e93)
        -   [Variables and Modes](#orgfee1fac)
        -   [Functions for custom bindings](#org94b87b5)
        -   [Custom Bindings](#org8e2ddab)
        -   [Functions for hooks](#org543eb1f)
        -   [Hooks](#org244521f)
        -   [ibuffer](#org0b14105)
    -   [Programming](#org5d9c124)
        -   [Packages](#org34b415f)
        -   [Languages](#orga3768ef)
    -   [Version Control](#org3db65e7)
        -   [Magit](#orgc2eb4b4)
        -   [magit-todos](#org646753d)
        -   [git-messenger](#orgf361508)
        -   [Git time machine](#org52697e2)
        -   [diff-hl](#org9f4eed4)
        -   [emsg-blame](#org159ac08)
    -   [Minibuffer & Completion](#org385b5c3)
        -   [consult-projectile](#org72c02cb)
        -   [consult-eglot](#org07bb655)
        -   [Corfu](#orgbef8114)
    -   [Org Mode](#orgbece978)
        -   [Org configuration](#org718e650)
        -   [Org Modern](#org27e8e32)
        -   [org-appear](#orga639b54)
    -   [Other Useful Packages](#orgda499fc)
        -   [buffer-move](#org77b68f2)
        -   [consult-denote](#orge2d9a05)
        -   [denote](#org9219bd1)
        -   [exec-path-from-shell](#org1d14226)
        -   [ESUP](#orgfc2fa21)
        -   [flymake-margin](#org420956f)
        -   [gptel](#org5d6fb7c)
        -   [helpful](#org3987567)
        -   [markdown-mode](#orgf5c1315)
        -   [package-lint](#org052c2fb)
        -   [perspective-el](#orgcbf4e4a)
        -   [Popper](#org860d2e8)
        -   [Projectile](#orge52f7c4)
        -   [ESS](#orgb6dd774)
        -   [rainbow-delimiters](#orge3e3a7d)
        -   [Transpose Frame](#orgcc02488)
        -   [YASnippet](#orgc50be82)
    -   [Appearance](#org24fd829)
        -   [Dashboard](#orgeebc58f)
        -   [Doom Modeline](#orgd2ed942)
        -   [Ef Themes](#orga9d5900)
        -   [Modus Themes](#org7ac3dcf)
        -   [nerd-icons-corfu](#orgebf7d4a)
        -   [magit-file-icons](#org1421a91)
-   [Terminal Config](#orgfcadee6)
    -   [General Configuration](#org1781c9d)
        -   [Variables and Modes](#orga0e627b)
        -   [Functions for custom bindings](#org681104e)
        -   [Custom Bindings](#org1e235fa)
        -   [Packages](#org8044cbe)
        -   [Terminal mappings](#org85e0f45)


<a id="orgc9f66d4"></a>

# Initial Setup

Before opening emacs with this configuration there are a few set up tasks that need to be performed:

1.  Install nerd-fonts: <https://github.com/rainstormstudio/nerd-icons.el>
2.  Create the following directories in `./emacs.d`
    -   `org-directory`
    -   `org-directory/denote`
    -   `backups`
3.  Configure environment variables: <https://github.com/amodelbello/dot-env.el> example `.env` file: [.env.example](.env.example)


<a id="org9b44f29"></a>

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


<a id="org95d92c3"></a>

# Common General Configuration



<a id="orgb05378f"></a>

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
  (turn-on-visual-line-mode)
  (visual-line-mode 1)
  (put 'narrow-to-region 'disabled nil)

  (setq save-interprogram-paste-before-kill t
        vc-follow-symlinks nil
        require-final-newline t
        enable-recursive-minibuffers t
        python-indent-guess-indent-offset-verbose nil
        custom-safe-themes t
        ispell-program-name (dot-env-get 'ISPELL_PATH "/usr/bin/ispell")
        dictionary-server "dict.org")

  (setq-default indent-tabs-mode nil
                global-tab-line-mode nil
                tab-line-mode nil
                tab-bar-mode nil
                line-spacing 0.3
                fill-column 100
                sentence-end-double-space nil
                whitespace-line-column 110)

  ;; Do not show those confusing warnings when installing packages
  ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

  ;; Blink modeline instead of ring bell
  (setq ring-bell-function
        (lambda ()
          (let ((orig-fg (face-foreground 'mode-line)))
            (set-face-foreground 'mode-line "Magenta")
            (run-with-idle-timer 0.1 nil
                                 (lambda (fg) (set-face-foreground 'mode-line fg))
                                 orig-fg))))

  ;; Use forward slashes to distinguish duplicate files
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
```


<a id="org0cb4c54"></a>

## Functions for custom bindings

```emacs-lisp
;; Open file based from .emacs.d directory
(defun amo/open-emacs-config-file (path)
  "Open a file within .emacs.d

PATH is path to file inside .emacs.d"
  (interactive)
  (find-file (concat user-emacs-directory path)))

;; Exports README.org
(defun amo/export-readme-file ()
  "Export README.org"
  (interactive)
  (let ((current-file-name (or (buffer-file-name) (buffer-name)))
        (is-file-p (buffer-file-name)))
    (find-file (concat user-emacs-directory "README.org"))
    (org-export-dispatch)
    (if is-file-p
        (find-file current-file-name)
      (switch-to-buffer current-file-name))))

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

;; Copy current file path & line number to kill ring
(defun amo/copy-file-path-and-line-number (arg)
  "Copy current file path & line number to kill ring

If called with the universal argument, provide full path.
Otherwise use the projectile project root if present"
  (interactive "P")
  (let* ((file-path (or (buffer-file-name) (buffer-name)))
         (project-path
          (if (and (null arg) (projectile-project-root))
              (concat (file-name-nondirectory (directory-file-name (projectile-project-root)))
                      "/" (string-replace
                           (or (projectile-project-root) "") ""
                           file-path))
            file-path)))
    (kill-new (concat project-path
                      ":" (number-to-string (line-number-at-pos))))
    (message "Copied current path and line number to kill ring!")))

;; From https://github.com/larstvei/dot-emacs
;; just-one-space removes all whitespace around a point - giving it a negative argument it removes newlines as well.
;; We wrap a interactive function around it to be able to bind it to a key.
;; In Emacs 24.4 cycle-spacing was introduced, and it works like just-one-space,
;; but when run in succession it cycles between one, zero and the original number of spaces.
(defun cycle-spacing-delete-newlines ()
  "Removes whitespace before and after the point."
  (interactive)
  (if (version< emacs-version "24.4")
      (just-one-space -1)
    (cycle-spacing -1)))

```


<a id="org02b5c69"></a>

## use-package Style Bindings

```emacs-lisp
:bind (("C-z g" . amo/export-readme-file)
       ("C-z l" . amo/toggle-line-numbers)
       ("C-z p" . amo/copy-file-path-and-line-number)
       ("M-'" . end-of-visual-line)
       ("C-x C-x" . amo/exchange-point-and-mark)
       ("C-S-t" . amo/transpose-chars-backwards)
       ("M-T" . amo/transpose-words-backwards)
       ("C-S-f" . amo/transpose-char-to-end-of-next-word)
       ("M-z" . zap-up-to-char)
       ("C-c t =" . text-scale-increase)
       ("C-c t -" . text-scale-decrease)
       ("C-z '" . indent-new-comment-line)
       ("C-x r M-w" . amo/copy-rectangle-to-kill-ring)
       ("M-/" . cycle-spacing-delete-newlines)
       ("C-c 1" . dictionary-lookup-definition)))
```


<a id="orgc0eb5b2"></a>

## Traditional-Style Bindings

Necessary when the bound function requires arguments

```emacs-lisp
(keymap-global-set "C-z s e" (lambda () (interactive) (amo/open-emacs-config-file ".env")))
(keymap-global-set "C-z s c s" (lambda () (interactive) (amo/open-emacs-config-file "common/settings.org")))
(keymap-global-set "C-z s c p" (lambda () (interactive) (amo/open-emacs-config-file "common/packages.org")))
(keymap-global-set "C-z s c f" (lambda () (interactive) (amo/open-emacs-config-file "common/functions.el")))
(keymap-global-set "C-z s g s" (lambda () (interactive) (amo/open-emacs-config-file "gui/settings.org")))
(keymap-global-set "C-z s t s" (lambda () (interactive) (amo/open-emacs-config-file "terminal/settings.org")))
```


<a id="orga66423c"></a>

## Hooks

```emacs-lisp
(add-hook 'before-save-hook 'amo/whitespace-cleanup)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook (lambda () (flyspell-mode -1)))
(add-hook 'prog-mode-hook 'subword-mode)
```


<a id="org51a02d0"></a>

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


<a id="orgfe85715"></a>

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


<a id="orgb509296"></a>

# Common Packages



<a id="orgd1bcd5c"></a>

## Minibuffer & Completion


<a id="org3206b1f"></a>

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
  (vertico-mode)
  :config
  (advice-add 'vertico--setup :before (lambda () (setq vertico-count 10)))
  (define-key minibuffer-local-map (kbd "s-'") (lambda ()
                                                 (interactive)
                                                 (let ((vertico-resize t))
                                                   (setq vertico-count (if (= vertico-count 10)
                                                                           (- (floor  (* (frame-height) 0.75)) 10)
                                                                         10))
                                                   (vertico--exhibit)))))
```


<a id="org97cc136"></a>

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


<a id="orgeef6501"></a>

### Marginalia

-   **Repo:** <https://github.com/minad/marginalia>
-   **Description:** Enable rich annotations in the minibuffer

```emacs-lisp
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))
```


<a id="org9aba27b"></a>

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


<a id="org3a69aa2"></a>

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


<a id="orgd17261d"></a>

### which-key

-   **Repo:** <https://github.com/justbur/emacs-which-key>
-   **Description:** Emacs package that displays available keybindings in popup

```emacs-lisp
(use-package which-key
  :straight t
  :config
  (which-key-mode))
```


<a id="org40c31ef"></a>

## Other Useful Packages


<a id="org727d73e"></a>

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


<a id="orgb9cc6b9"></a>

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


<a id="org8ea6808"></a>

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


<a id="orgb37d96f"></a>

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


<a id="orga2f821f"></a>

### el-patch

-   **Repo:** <https://github.com/radian-software/el-patch>
-   **Description:** ✨ Future-proof your Emacs Lisp customizations!

```emacs-lisp
(use-package el-patch
  :straight t)
```


<a id="orgd319db7"></a>

### expand-region

-   **Repo:** <https://github.com/magnars/expand-region.el>
-   **Description:** Emacs extension to increase selected region by semantic units.

```emacs-lisp
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
```


<a id="org207e655"></a>

### Geiser (Scheme)

-   **Repo:** <https://github.com/emacsmirror/geiser>
-   **Docs:** <https://www.nongnu.org/geiser/>
-   **Description:** GNU Emacs and Scheme talk to each other

```emacs-lisp
(use-package geiser-guile
  :straight t)
```


<a id="org3b6c7f9"></a>

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


<a id="orgf2c1380"></a>

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


<a id="org1a548d6"></a>

### minions

-   **Repo:** <https://github.com/tarsius/minions>
-   **Description:** A minor-mode menu for the mode line

```emacs-lisp
(use-package minions
  :straight t
  :config
  (minions-mode 1))
```


<a id="org2323bb2"></a>

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


<a id="orgdfbd801"></a>

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
   (scheme-mode . paredit-mode)
   (clojure-mode . paredit-mode)
   (clojurescript-mode . paredit-mode)
   (clojurec-mode . paredit-mode)
   (cider-repl-mode . paredit-mode))
  :bind (:map paredit-mode-map
              ("M-(" . paredit-wrap-round)
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly)
              ("M-<" . paredit-wrap-angled)
              ("C-M-<right>" . paredit-backward-barf-sexp)
              ("C-M-<left>" . paredit-backward-slurp-sexp)))
```


<a id="orgefaeb00"></a>

### rg.el

-   **Repo:** <https://github.com/dajva/rg.el>
-   **Docs:** <https://rgel.readthedocs.io/>
-   **Description:** Emacs search tool based on ripgrep

```emacs-lisp
(use-package rg
  :straight t
  :config
  (rg-enable-default-bindings))
```


<a id="org8ddf014"></a>

### sicp-info

-   **Repo:** <https://github.com/webframp/sicp-info?tab=readme-ov-file>
-   **Docs:** <http://www.neilvandyke.org/sicp-texi/>
-   **Description:** Stucture and Interpretation of Computer Progams in info format

```emacs-lisp
(use-package sicp
  :straight t)
```


<a id="orged09503"></a>

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


<a id="orgccf307f"></a>

## Appearance


<a id="org32b42ef"></a>

### Standard Themes

-   **Repo:** <https://github.com/protesilaos/standard-themes>
-   **Description:** Like the default GNU Emacs theme but more consistent

```emacs-lisp
(use-package standard-themes
  :straight t)
```


<a id="org57401e9"></a>

### Nerd Icons

-   **Repo:** <https://github.com/rainstormstudio/nerd-icons.el>
-   **Description:** A library for easily using Nerd Font icons inside Emacs

```emacs-lisp
(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))
```


<a id="org220cb35"></a>

### nerd-icons-dired

-   **Repo:** <https://github.com/rainstormstudio/nerd-icons-dired>
-   **Description:** Use nerd-icons for Dired

```emacs-lisp
(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))
```


<a id="orga16dca5"></a>

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


<a id="org17dbc5f"></a>

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


<a id="org9502a75"></a>

# GUI Config

Configuration and packages specific to GUI


<a id="orgac63e93"></a>

## General Configuration


<a id="orgfee1fac"></a>

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
      eshell-scroll-show-maximum-output nil

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

;; Rewrite the split-window-sensibly function
;; Reverse its preference and essentially prefer splitting side-by-side.
;; https://github.com/SophieBosio/.emacs.d
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Based on `split-window-sensibly', but prefers to split WINDOW side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq split-window-preferred-function 'split-window-really-sensibly)

;; Reuse the inactive window instead of creating a new one.
;; Setting both split-height-threshold and split-width-threshold to nil seems to ensure this.
;; https://github.com/SophieBosio/.emacs.d
(setq-default split-height-threshold nil
              split-width-threshold  nil
              fill-column            80) ;; Maximum line width
;; window-min-width       80) ;; No smaller windows than this
```


<a id="org94b87b5"></a>

### Functions for custom bindings

```emacs-lisp
;; Reload config
(defun amo/reload-config ()
  "Reload configuration"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;; Delete desktop files and restart emacs
(defun amo/delete-emacs-perspective-file-restart ()
  "Delete saved desktop, then restart emacs"
  (interactive)
  (setq desktop-save-mode nil)
  (delete-file (concat user-emacs-directory ".perspective"))
  (restart-emacs))

(defun amo/split-window (arg side)
  "Create a new window on SIDE.
Universal argument will split based off root window.
Double universal argument will split based off of window parent
Also balance windows and move point to new window"
  (interactive "p")
  (split-window
   (cond
    ((= arg 4) (frame-root-window))
    ((= arg 16) (window-parent (selected-window)))
    ((= arg 1) (selected-window)))
   nil side nil)
  (balance-windows)
  (other-window 1))

(defun amo/delete-window ()
  "Balance windows after deleting one"
  (interactive)
  (delete-window)
  (balance-windows))

(defun amo/open-main-notes-file ()
  "Open main notes file"
  (interactive)
  (find-file (concat org-directory "/notes.org")))

(defun amo/open-notes-directory ()
  "Open notes directory"
  (interactive)
  (find-file org-directory))

(defun amo/open-denote-directory ()
  "Open denote directory"
  (interactive)
  (find-file (concat org-directory "/denote")))
```


<a id="org8e2ddab"></a>

### Custom Bindings

```emacs-lisp

;; Custom prefix C-z
(global-set-key (kbd "C-z r") 'amo/reload-config)
(global-set-key (kbd "C-z C-z r") 'restart-emacs)
(global-set-key (kbd "C-z C-z C-z r") 'amo/delete-emacs-perspective-file-restart)
(global-set-key (kbd "C-z t") 'customize-themes)
(global-set-key (kbd "C-z e") 'eshell)
(global-set-key (kbd "C-z n") 'amo/open-main-notes-file)
(global-set-key (kbd "C-z C-z n") 'amo/open-notes-directory)
(global-set-key (kbd "C-z d") 'amo/open-denote-directory)

;; Other bindings
(global-set-key (kbd "C-x 0") 'amo/delete-window)
(global-set-key (kbd "C-x 2") (lambda (arg) (interactive "p") (amo/split-window arg 'below)))
(global-set-key (kbd "C-x 3") (lambda (arg) (interactive "p") (amo/split-window arg 'right)))
(global-set-key (kbd "M-<down>") 'amo/move-line-down)
(global-set-key (kbd "M-<up>") 'amo/move-line-up)
(global-set-key (kbd "C-'") 'amo/toggle-capitalization)
(global-set-key (kbd "C-s-p") 'scroll-down-line)
(global-set-key (kbd "C-s-n") 'scroll-up-line)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

```


<a id="org543eb1f"></a>

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


<a id="org244521f"></a>

### Hooks

```emacs-lisp
(add-hook 'comint-mode-hook 'amo/comint-mode-actions)
(add-hook 'focus-out-hook 'garbage-collect)
```


<a id="org0b14105"></a>

### ibuffer

```emacs-lisp
(use-package ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . nil)))
```


<a id="org5d9c124"></a>

## Programming


<a id="org34b415f"></a>

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


<a id="orga3768ef"></a>

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
          (setq conda-anaconda-home (expand-file-name (dot-env-get 'CONDA_PATH "~/opt/miniconda3"))
                conda-env-home-directory (expand-file-name (dot-env-get 'CONDA_PATH "~/opt/miniconda3"))
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


<a id="org3db65e7"></a>

## Version Control


<a id="orgc2eb4b4"></a>

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


<a id="org646753d"></a>

### magit-todos

-   **Repo:** <https://github.com/alphapapa/magit-todos>
-   **Description:** Show source files' TODOs (and FIXMEs, etc) in Magit status buffer

```emacs-lisp
(use-package magit-todos
  :straight t
  :hook ((magit-mode . magit-todos-mode)))
```


<a id="orgf361508"></a>

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


<a id="org52697e2"></a>

### Git time machine

-   **Repo:** <https://github.com/emacsmirror/git-timemachine>
-   **Description:** Walk through git revisions of a file

```emacs-lisp
(use-package git-timemachine
  :straight t)
```


<a id="org9f4eed4"></a>

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


<a id="org159ac08"></a>

### emsg-blame

-   **Repo:** <https://github.com/ISouthRain/emsg-blame>
-   **Description:** A simple, fast, asynchronous, customizable display, view of git blame commit in Emacs.

```emacs-lisp
(use-package emsg-blame
  :straight (:host github :repo "ISouthRain/emsg-blame")
  :config
  (global-emsg-blame-mode)
  (defun my--emsg-blame-display ()
    (message " %s • %s • %s" emsg-blame--commit-author emsg-blame--commit-date emsg-blame--commit-summary))
  (setq emsg-blame-display #'my--emsg-blame-display
        ;; Prevent eldoc from overwritting message buffer output from this package
        eldoc-message-function '(lambda (format-string &rest args) ())))
```


<a id="org385b5c3"></a>

## Minibuffer & Completion


<a id="org72c02cb"></a>

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


<a id="org07bb655"></a>

### consult-eglot

-   **Repo:** <https://github.com/mohkale/consult-eglot>
-   **Description:** Jump to workspace symbols with eglot and consult
    
    ```emacs-lisp
    (use-package consult-eglot
      :straight t)
    ```


<a id="orgbef8114"></a>

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


<a id="orgbece978"></a>

## Org Mode


<a id="org718e650"></a>

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


<a id="org27e8e32"></a>

### Org Modern

-   **Repo:** <https://github.com/minad/org-modern>
-   **Description:** 🦄 Modern Org Style
    
    ```emacs-lisp
    (use-package org-modern
      :straight t
      :config (with-eval-after-load 'org (global-org-modern-mode)))
    ```


<a id="orga639b54"></a>

### org-appear

-   **Repo:** <https://github.com/awth13/org-appear>
-   **Description:** Toggle visibility of hidden Org mode element parts upon entering and leaving an element

```emacs-lisp
(use-package org-appear
  :straight (:type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))
```


<a id="orgda499fc"></a>

## Other Useful Packages


<a id="org77b68f2"></a>

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


<a id="orge2d9a05"></a>

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


<a id="org9219bd1"></a>

### denote

-   **Repo:** <https://github.com/protesilaos/denote>
-   **Docs:** <https://protesilaos.com/emacs/denote>
-   **Description:** Simple notes for Emacs with an efficient file-naming scheme

```emacs-lisp
(use-package denote
  :straight t
  :after org
  :init
  ;; to prevent "non-prefix key" error
  (define-key global-map (kbd "C-c n") (make-sparse-keymap))
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


<a id="org1d14226"></a>

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


<a id="orgfc2fa21"></a>

### ESUP

-   **Repo:** <https://github.com/jschaf/esup>
-   **Description:** ESUP - Emacs Start Up Profiler

```emacs-lisp
(use-package esup
  :straight t
  :config
  (setq esup-depth 0))
```


<a id="org420956f"></a>

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


<a id="org5d6fb7c"></a>

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


<a id="org3987567"></a>

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


<a id="orgf5c1315"></a>

### markdown-mode

-   **Repo:** <https://github.com/jrblevin/markdown-mode>
-   **Description:** Emacs Markdown Mode

```emacs-lisp
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
```


<a id="org052c2fb"></a>

### package-lint

-   **Repo:** <https://github.com/purcell/package-lint>
-   **Description:** A linting library for elisp package metadata

```emacs-lisp
(use-package package-lint
  :straight t)
```


<a id="orgcbf4e4a"></a>

### perspective-el

-   **Repo:** <https://github.com/nex3/perspective-el>
-   **Description:** Perspectives for Emacs.

```emacs-lisp
(use-package perspective
  :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-c C-w"))
  (persp-state-default-file (concat user-emacs-directory ".perspective"))
  :init
  (persp-mode)
  (persp-state-load (concat user-emacs-directory ".perspective"))
  :config
  (setq switch-to-prev-buffer-skip
        (lambda (win buff bury-or-kill)
          (not (persp-is-current-buffer buff))))
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :hook ((kill-emacs . persp-state-save)
         (restart-emacs . persp-state-save))
  :bind (("C-x C-b" . persp-ibuffer)
         :map perspective-map
         ("," . persp-rename)
         ("." . persp-switch)
         ("'" . persp-prev)
         ("\"" . persp-kill)))
```


<a id="org860d2e8"></a>

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
          "\\*Geiser Debug\\*"
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


<a id="orge52f7c4"></a>

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


<a id="orgb6dd774"></a>

### ESS

-   **Repo:** <https://github.com/emacs-ess/ESS>
-   **Docs:** <https://ess.r-project.org/>
-   **Description:** Emacs Speaks Statistics: ESS

```emacs-lisp
(use-package ess
  :straight t)
```


<a id="orge3e3a7d"></a>

### rainbow-delimiters

-   **Repo:** <https://github.com/Fanael/rainbow-delimiters>
-   **Description:** A "rainbow parentheses"-like mode which highlights delimiters

```emacs-lisp
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))
```


<a id="orgcc02488"></a>

### Transpose Frame

-   **Docs:** <https://www.emacswiki.org/emacs/TransposeFrame>
-   **Description:** Interactive functions to transpose window arrangement in current frame

```emacs-lisp
(use-package transpose-frame
  :straight t
  :bind (("C->" . transpose-frame)))
```


<a id="orgc50be82"></a>

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


<a id="org24fd829"></a>

## Appearance


<a id="orgeebc58f"></a>

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


<a id="orgd2ed942"></a>

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


<a id="orga9d5900"></a>

### Ef Themes

-   **Repo:** <https://github.com/protesilaos/ef-themes>
-   **Description:** Colourful and legible themes for GNU Emacs

```emacs-lisp
(use-package ef-themes
  :straight t)
```


<a id="org7ac3dcf"></a>

### Modus Themes

-   **Repo:** <https://github.com/protesilaos/modus-themes>
-   **Description:** Highly accessible themes for GNU Emacs

```emacs-lisp
(use-package modus-themes
  :straight t)
```


<a id="orgebf7d4a"></a>

### nerd-icons-corfu

-   **Repo:** <https://github.com/LuigiPiucco/nerd-icons-corfu>
-   **Description:** Icons for corfu via nerd-icons

```emacs-lisp
(use-package nerd-icons-corfu
  :straight t)
```


<a id="org1421a91"></a>

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


<a id="orgfcadee6"></a>

# Terminal Config

Configuration and packages specific to terminal


<a id="org1781c9d"></a>

## General Configuration


<a id="orga0e627b"></a>

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


<a id="org681104e"></a>

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

;; Reload config
(defun amo/reload-config ()
  "Reload configuration"
  (interactive)
  ;; (dot-env-load)
  (load-file "~/.emacs.d/terminal/init.el"))
```


<a id="org1e235fa"></a>

### Custom Bindings

```emacs-lisp
:bind (("C-z r" . amo/reload-config)
       ("C-z t" . amo/modus-themes-toggle)))
```


<a id="org8044cbe"></a>

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


<a id="org85e0f45"></a>

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
