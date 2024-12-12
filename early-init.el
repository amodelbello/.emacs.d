;;; -*- lexical-binding: t -*-

;; Set the gc-cons-threshold to a ridiculously large number
;; and restore the default value after initialization
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20))))

;; From https://github.com/karthink/.emacs.d/blob/master/early-init.el
(setq package-enable-at-startup nil
      package-quickstart nil
      load-prefer-newer t)

;; Turn off mouse interface early in startup to avoid momentary display
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t)

(set-language-environment "UTF-8")
(setq default-input-method nil)
