(setq package-enable-at-startup nil)

;;
;; Turn off mouse interface early in startup to avoid momentary display
;;
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)
