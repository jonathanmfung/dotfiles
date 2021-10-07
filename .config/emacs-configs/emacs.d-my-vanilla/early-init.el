;; not sure if inhibit-startup-screen needs to be early
(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Fonts
(defvar jf/default-font-size 140)
(defvar jf/default-variable-font-size 140)
(set-face-attribute 'default nil :font "Iosevka SS14" :height jf/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka SS14" :height jf/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height jf/default-variable-font-size)
