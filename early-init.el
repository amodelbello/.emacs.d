;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; non-nil to invoke the debugger on error
(setq debug-on-error nil)

;; Maximize garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;; Using straight.el so disable package.el
(setq package-enable-at-startup nil)

;; Disable UI elements before they appear
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Prevent frame resizing when fonts/UI elements load
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Reduce startup noise
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Native Compilation
(when (featurep 'native-compile)
  ;; Silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil
        native-comp-warning-on-missing-source nil)

  ;; Set compilation cache location
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; Prefer loading newest compiled files
(setq load-prefer-newer t)

;; Don't use GUI dialogs
(setq use-file-dialog nil
      use-dialog-box nil)

;; Disable bidirectional text scanning for performance
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disable warnings from legacy advice system
(setq ad-redefinition-action 'accept)

(set-language-environment "UTF-8")
(setq default-input-method nil)
