;; Set prefix key ("C-z")
;; "C-z" is the custom prefix key
(define-prefix-command 'z-map)
(global-set-key (kbd "C-z") 'z-map)

;; Open settings.org (this file)
(defun amo/open-settings-dir ()
  "Open lisp directory"
  (interactive)
  (dired "~/.emacs.d/lisp"))
(define-key z-map (kbd "s") #'amo/open-settings-dir)

;; Reload config
(defun amo/reload-config ()
  "Reload configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(define-key z-map (kbd "r") #'amo/reload-config)

;; Restart Emacs
(define-key z-map (kbd "C-z r") #'restart-emacs)

;; Open customize-themes
(define-key z-map (kbd "t") #'customize-themes)

;; Open centaur-tabs groups
(define-key z-map (kbd "a") #'centaur-tabs-counsel-switch-group)

;; Start eshell
(define-key z-map (kbd "e") #'eshell)

;; Open Calendar
(define-key z-map (kbd "c") #'calendar)

;; Open gptel
(define-key z-map (kbd "g") #'gptel)

;; Open notes directory
(defun amo/open-notes-file ()
  "Open notes file"
  (interactive)
  (find-file org-directory))
(define-key z-map (kbd "n") #'amo/open-notes-file)

;; Bindings for "IDE-like" tool windows
(global-set-key (kbd "s-1") #'treemacs)
(global-set-key (kbd "s-2") #'treemacs-select-window)
(global-set-key (kbd "s-7") #'lsp-treemacs-symbols)

;; Shrink window vertically
(global-set-key (kbd "C-x %") (kbd "C-u -1 C-x ^"))

;; One line scroll
(global-set-key (kbd "C-s-p") #'scroll-down-line)
(global-set-key (kbd "C-s-n") #'scroll-up-line)

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

(global-set-key (kbd "C-x 2") #'amo/split-window-below)
(global-set-key (kbd "C-x 3") #'amo/split-window-horizontally)

;; Un-highlight region after mark jump
(defun amo/exchange-point-and-mark ()
  "Deactivates mark after exchanging point and mark"
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark))
(global-set-key (kbd "C-x C-x") #'amo/exchange-point-and-mark)

;; Kill current buffer and delete current window
(global-set-key (kbd "C-x K") #'kill-buffer-and-window)

;; Transpose chars and words backwards
(defun amo/transpose-chars-backwards ()
  "Just like transpose-chars but goes the other way"
  (interactive)
  (transpose-chars -1))
(global-set-key (kbd "C-S-t") #'amo/transpose-chars-backwards)
(defun amo/transpose-words-backwards ()
  "Just like transpose-words but goes the other way"
  (interactive)
  (transpose-words -1))
(global-set-key (kbd "M-T") #'amo/transpose-words-backwards) ;; not sure why "M-S-t" doesn't work here

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
(global-set-key (kbd "C-S-f") #'amo/transpose-char-to-end-of-next-word)

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
(global-set-key (kbd "M-<down>") #'amo/move-line-down)
(global-set-key (kbd "M-<up>") #'amo/move-line-up)
