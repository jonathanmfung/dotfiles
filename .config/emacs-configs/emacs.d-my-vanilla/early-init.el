;;; -*- lexical-binding: t -*-

;;; Code:
;; not sure if inhibit-startup-screen needs to be early
(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Fonts
;; For some reason, fonts aren't loading in early.init (2/26/22
;; (defvar jf/default-font-size 130)
;; (defvar jf/default-variable-font-size 130)
;; (set-face-attribute 'default nil :font "Iosevka SS14" :height jf/default-font-size)
;; (set-face-attribute 'fixed-pitch nil :font "Iosevka SS14" :height jf/default-font-size)
;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height jf/default-variable-font-size)

;; (set-fontset-font t nil (font-spec :height jf/default-font-size :name "Iosevka SS14"))

;; org-mode
(setq org-use-extra-keys t)

(provide 'early-init)
;;; early-init.el ends here
