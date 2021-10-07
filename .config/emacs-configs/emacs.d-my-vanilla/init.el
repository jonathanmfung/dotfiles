;; -*- lexical-binding: t; -*-

;;; Heaviliy inspired by https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org

;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; make gc temporarily less frequent
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defun jf/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.4f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jf/display-startup-time)

;;; Packages
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(window simple prism quelpa-use-package quelpa l lsp-mode lsp-haskell haskell-mode company company-mode icomplete-vertical selectrum rainbow-delimiters lispy which-key consult orderless marginalia vertico use-package)))


;;; Usability (completion, consult, etc.)

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disabled due to weird cursor behavior
(use-package vertico
  :init
  (vertico-mode))

;; Should have a built-in version when https://www.reddit.com/r/emacs/comments/mu3lpl/psa_icompleteverticalmode_is_available_since/
;; also should introduce =icomplete-scroll=
;; (use-package icomplete-vertical
;;   :demand t
;;   :custom
;;   (read-file-name-completion-ignore-case t)
;;   (read-buffer-completion-ignore-case t)
;;   (completion-ignore-case t)
;;   :config
;;   (icomplete-mode)
;;   (icomplete-vertical-mode)
;;   :bind (:map icomplete-minibuffer-map
;;               ("<down>" . icomplete-forward-completions)
;;               ("C-n" . icomplete-forward-completions)
;;               ("<up>" . icomplete-backward-completions)
;;               ("C-p" . icomplete-backward-completions)
;;               ("C-w" . backward-kill-word)
;; 	      ("RET" . icomplete-force-complete-and-exit)
;; 	      ("TAB" . icomplete-force-complete)))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; https://github.com/minad/vertico#configuration
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
	))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-project)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)       ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)     ;; orig. isearch-edit-string
         ("M-s l" . consult-line)        ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)) ;; needed by consult-line to detect isearch

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package which-key
  :init
  (which-key-mode))

;;; Visual Interface
(use-package modus-themes
  :ensure                               ; omit this to use the built-in themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package faces
  :ensure nil
  :init
  (set-frame-font "Iosevka SS14:pixelsize=18:weight=semibold" nil t))

;; font resizing
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(blink-cursor-mode -1)
(setq scroll-conservatively 101)


;;; Programming
(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package lsp-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))
(use-package lsp-haskell)

;;; company-mode
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;; general
(use-package general)

(general-define-key
 :prefix "C-c w"
 "h" 'windmove-left
 "j" 'windmove-down
 "k" 'windmove-up
 "l" 'windmove-right
 "H" 'windmove-swap-states-left
 "J" 'windmove-swap-states-down
 "K" 'windmove-swap-states-up
 "L" 'windmove-swap-states-right)

(general-define-key :prefix "C-c w"
		    "w" 'ace-window)

(use-package ace-window
  :init (ace-window-display-mode 1))

(use-package simple
  :ensure nil
  :init (setq-default fill-column 100)
  (auto-fill-mode))

(use-package avy
  :bind* (("C-j" . avy-goto-char-timer))
  :init (setq avy-timeout-seconds 0.3))

(use-package window
  :ensure nil
  :bind (("M-n" . scroll-down-line)
	 ("M-p" . scroll-up-line)))

(use-package emacs
  :ensure nil
  :init (setq sentence-end-double-space nil))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
