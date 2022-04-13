(use-package ace-window
  :ensure t
  :config (progn
	    (defvar aw-dispatch-always nil)
	    (global-set-key (kbd "C-x o") 'ace-window)
	    (global-set-key (kbd "s-w") 'ace-window)
	    (custom-set-faces
	     '(aw-leading-char-face
	       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(use-package counsel
  :ensure t
  :bind
    (("M-y" . counsel-yank-pop)
  :map ivy-minibuffer-map
    ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :init
    (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "%d/%d ")
    (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package nlinum
  :ensure t
  :init (global-nlinum-mode 1))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode 1))))

(use-package which-key
  :ensure t 
  :config
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(global-set-key (kbd "\e\es")
		(lambda ()
		  (interactive)
		  (find-file "~/.emacs.d/settings.org")))

(global-set-key (kbd "\e\ei")
		(lambda ()
		  (interactive)
		  (find-file "~/.emacs.d/init.el")))

(set-frame-font "DejaVu Sans Mono-14" nil t)
(setq-default line-spacing 0.3)
(setq-default fill-column 80)
(setq-default sentence-end-double-space nil)
(setq-default whitespace-line-column 110)

;; Because the line-spacing above messes up calc
(add-hook 'calc-mode-hook
          (lambda ()
            (setq line-spacing 0)))
(add-hook 'calc-trail-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(setq-default global-tab-line-mode nil)
(setq-default tab-line-mode nil)
(setq-default tab-bar-mode nil)
(toggle-scroll-bar -1)

; easily shrink window vertically
(global-set-key (kbd "C-x %") (kbd "C-u -1 C-x ^"))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
	   (quote (("default"
		    ("org" (mode . org-mode))
		    ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
		    ("web" (or (mode . web-mode) (mode . js2-mode)))
		    ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
		    ("mu4e" (or
			     (mode . mu4e-compose-mode)
			     (name . "\*mu4e\*")
			     ))
		    ("programming" (or
				    (mode . emacs-lisp-mode)
				    (mode . lisp-mode)
				    (mode . clojure-mode)
				    (mode . clojurescript-mode)
				    (mode . python-mode)
				    (mode . c-mode)
				    (mode . c++-mode)))
		    ("text" (mode . text-mode))
		    ("magit" (mode . magit-mode))
		    ("dired" (mode . dired-mode))
		    ("emacs" (or
			      (name . "^\\*scratch\\*$")
			      (name . "^\\*Messages\\*$")))
		    ))))
     (add-hook 'ibuffer-mode-hook
	       (lambda ()
		 (ibuffer-auto-mode 1)
		 (ibuffer-switch-to-saved-filter-groups "default")))

     ;; Don't show filter groups if there are no buffers in that group
     (setq ibuffer-show-empty-filter-groups nil)

(setq-default visual-line-mode t)
(desktop-save-mode 1)
(setq-default org-catch-invisible-edits 'show)
(setq-default prelude-whitespace nil)
;;    (save-interprogram-paste-before-kill t)
(setq auto-mode-alist (append '(("\\.cl$" . lisp-mode))
			      auto-mode-alist))

(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;keep cursor at same position when scrolling
;;(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
;; (global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
;; (global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
;; (global-set-key (kbd "C-M-q") 'query-replace)

;; Spell check
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; allow remembering risky variables
(defun risky-local-variable-p (sym &optional _ignored) nil)
