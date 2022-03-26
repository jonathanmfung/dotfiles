;; -*- lexical-binding: t; -*-
;;; Code:
;;; Heaviliy inspired by https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org

;;; speedup blub
;; https://www.reddit.com/r/emacs/comments/qw52na/emacs_271_earlyinit_file/hl0oo31/
(setq default-gc-threshold gc-cons-threshold
      default-gc-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      default-gc-percentage 0.8)

(setq load-prefer-newer t)

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
   '(org-fragtog corfu modus-themes multiple-cursors cider org redacted redacted-mode svg-tag-mode svg-tag slime vertico-posframe ess forge magit popwin helpful embark-consult embark consult-flycheck flycheck exec-path-from-shell nano-modeline mixed-pitch olivetti bufler window simple prism quelpa-use-package quelpa l lsp-mode lsp-haskell haskell-mode company company-mode icomplete-vertical selectrum rainbow-delimiters lispy which-key consult orderless marginalia vertico use-package))
 )

;;; Setting up path
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x pgtk))
	  (exec-path-from-shell-initialize)))
;;; auto-save
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;;; QoL
(defalias 'yes-or-no-p 'y-or-n-p)

;;; emacsclient QoL
(defun save-buffers-kill-terminal--safe ()
  "Based off 'save-buffers-kill-emacs'.
Wrapper around 'save-buffers-kill-terminal.
Meant to be used with emacsclient'"
  (interactive)
  (let ((confirm confirm-kill-emacs))
    (and (or (null confirm)
	     (funcall confirm "Exit this emacsclient?"))
	 (save-buffers-kill-terminal))))

(use-package files
  :ensure nil
  :init (setq confirm-kill-emacs 'y-or-n-p)
  :bind (("C-x C-c" . #'save-buffers-kill-terminal--safe)))

;;; Loading in any custom elisp
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))


;; https://github.com/tonyaldon/org-bars
(use-package org-bars
  :ensure nil)

;; https://github.com/oantolin/math-delimiters
(use-package math-delimiters
  :ensure nil)

;;; Usability (Vertico, Marginalia, Orderless, Embark)

(defun jf/vertico--backward-updir ()
  "Delete char before or go up directory for file cagetory completions.
https://github.com/minad/vertico/issues/65#issuecomment-875094896"
  (interactive)
  (let ((metadata (completion-metadata (minibuffer-contents)
                                       minibuffer-completion-table
                                       minibuffer-completion-predicate)))
    (if (and (eq (char-before) ?/)
             (eq (completion-metadata-get metadata 'category) 'file))
        (let ((new-path (minibuffer-contents)))
          (delete-region (minibuffer-prompt-end) (point-max))
          (insert (abbreviate-file-name
                   (file-name-directory
                    (directory-file-name
                     (expand-file-name new-path))))))
      (call-interactively 'backward-delete-char))))
;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package vertico
  :bind (:map vertico-map
	      ("DEL" . jf/vertico--backward-updir))
  :init
  (setq enable-recursive-minibuffers t)
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :init
  (marginalia-mode))

;; https://github.com/minad/vertico#configuration
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
	)
  (setq orderless-matching-styles '(orderless-regexp)))

(use-package embark
  :bind (("C-." . embark-act)
	 ("C-;" . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult))

;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;;   ;; Vertico commands are hidden in normal buffers.
;;   (setq read-extended-command-predicate
;;         #'command-completion-default-include-p)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

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
;;; Window Management

;; trying to add indicator when in ace-window,
;; but cannot use ace-window in lambda/functions
;; (defun jf/ace-window-message--helper ()
;;   (let ((pred (and ace-window-display-mode (< 2 (count-windows)))))
;;     (when pred
;;       (message "Select a window"))
;;     ))

(use-package ace-window
  :bind* (("M-o" . 'ace-window))
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-display-mode-overlay nil)
  (setq aw-scope 'frame)
  (defun jf/ace-window-display--helper (left-delim right-delim)
    "Extract window selecter chracter and wrap in LEFT-DELIM and RIGHT-DELIM.
For use with nano-modeline-default-mode.
Discovered window-parameter from https://oremacs.com/2015/03/12/ace-window-display-mode/."
    (let ((pred (and ace-window-display-mode (< 2 (count-windows)))))
      (when pred
	(concat left-delim (window-parameter (get-buffer-window) 'ace-window-path) right-delim ))))
  ;; (add-hook 'ace-window-display-mode-hook #'jf/ace-window-message--helper)
  )


;; This package messes with multi-window configuration
;; (use-package popwin
;;   :config
;;   (add-to-list 'popwin:special-display-config '(helpful-mode :stick t))
;;   (add-to-list 'popwin:special-display-config 'process-menu-mode)
;;   (add-to-list 'popwin:special-display-config '("*HS-Error*" :stick t))

;;   ( popwin-mode 1))


;;; Visual Interface
(use-package nano-modeline
  :after (ace-window)
  :functions jf/ace-window-display--helper
  :init (setq nano-modeline-position 'top)
  (ace-window-display-mode 1)
  (nano-modeline-mode)
  :config
  ;; this only affects the default buffers, not any special modes like mu4e, calendar, etc.
  ;; see https://github.com/rougier/nano-emacs/blob/master/nano-modeline.el for implementation
  (defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-modeline-mode-name))
          (branch      (nano-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             (concat (jf/ace-window-display--helper "{" "} ") buffer-name)
                             (concat "(" mode-name
                                     (if branch (concat ", "
							(propertize branch 'face 'italic)))
                                     ")" )
                             position))))

(use-package modus-themes
  :ensure   				; omit this to use the built-in themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-syntax '()
	modus-themes-prompts '()
	modus-themes-region '(bg-only no-extend)
	modus-themes-mode-line '()
	modus-themes-subtle-line-numbers t
	modus-themes-org-blocks 'tinted-background
	modus-themes-headings
	'((1 . (rainbow overline background))
	  (t . (rainbow overline no-bold)))
	modus-themes-completions
	'((matches . (intense))
          (selection . (accented intense))
          (popup . (accented))))
  (defun jf/modus-themes-custom-faces ()
    ;; [ status | name (primary) secondary ]
    (set-face-attribute 'nano-modeline-active-status-** nil :background (modus-themes-color 'red-intense-bg)
			:foreground (modus-themes-color 'fg-main))
    (set-face-attribute 'nano-modeline-inactive-status-** nil :background (modus-themes-color 'red-subtle-bg))
    (set-face-attribute 'nano-modeline-active-status-RW nil :background (modus-themes-color 'blue-intense-bg))
    (set-face-attribute 'nano-modeline-active-status-RO nil :background (modus-themes-color 'blue-intense-bg)))

  ;; https://protesilaos.com/modus-themes/#h:1487c631-f4fe-490d-8d58-d72ffa3bd474
  (add-hook 'modus-themes-after-load-theme-hook #'jf/modus-themes-custom-faces)

  (modus-themes-load-themes)


  (modus-themes-load-operandi)

  :bind
  ("<f5>" . modus-themes-toggle))

;;; Fonts
;; fonts are set in early-init
;; (use-package faces
;;   :ensure nil
;;   :init
;;   (set-frame-font "Iosevka SS14:pixelsize=18:weight=semibold" nil t))

(defvar jf/default-font-size 130)
(defvar jf/default-variable-font-size 130)
(set-face-attribute 'default nil :font "Iosevka SS14" :height jf/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka SS14" :height jf/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height jf/default-variable-font-size)

(set-fontset-font t nil (font-spec :height jf/default-font-size :name "Iosevka SS14"))

(use-package mixed-pitch
  :bind ("<f2>" . mixed-pitch-mode))

;; font resizing
(use-package hydra
  :init
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t)))

(blink-cursor-mode -1)
(setq scroll-conservatively 101)
(delete-selection-mode 1)

(defun jf/open-line-end ()
  (interactive)
  (end-of-line)
  (newline))

(use-package emacs
  :ensure nil
  :bind (("C-M-j" . #'jf/open-line-end)))

;;; text-mode
(add-hook 'text-mode-hook #'flyspell-mode)

;;; org-mode

(use-package org
  :ensure nil
  ;; this is a ZERO WIDTH NO-BREAK SPACE
  :init (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)

  ;;  from org-bars in user-emacs-directory/custom
  (add-hook 'org-mode-hook #'org-bars-mode)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  ;; this breaks org-cycle on bullet lists
  ;; (add-hook 'org-mode-hook #'org-no-ellipsis-in-headlines)
  :config (setq org-ellipsis "﻿")
  (plist-put org-format-latex-options :scale 3)
  (plist-put org-format-latex-options :foreground 'auto)
  (plist-put org-format-latex-options :background 'auto)
  :bind (:map org-mode-map ("$" . #'math-delimiters-insert)
	      ("C-c C-x d" . #'org-metadown)))

(use-package org-fragtog
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;;; Programming
(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
  :config (setq lispy-compat '(edebug cider)))

(use-package  helpful
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package magit)

(use-package forge
  :after magit)


(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; For Corfu
  (defun jf/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . jf/lsp-mode-setup-completion))

;;; Clojure/Cider
(use-package cider)

;;; Haskell
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'subword-mode))

(use-package haskell-interactive-mode
  :ensure nil
  :after (haskell-mode)
  :bind (:map haskell-interactive-mode-map
	      ;; used to override beginning-of-visual-line
	      ("C-a" . #'haskell-interactive-mode-bol)))

(use-package lsp-haskell
  :init (setq lsp-haskell-formatting-provider "brittany"))

;;;; R
(use-package ess
  :config (setq ess-use-ido nil)
  :init (add-hook 'inferior-ess-r-mode-hook (lambda () (setq-local comint-scroll-to-bottom-on-output t))))

;;; Org Babel
(use-package org
  :ensure nil
  :config (org-babel-do-load-languages
	   'org-babel-load-languages '((R . t)
				       (shell . t)
				       (python . t)))
  (setq org-confirm-babel-evaluate nil))
;;; electric
(electric-pair-mode 1)

;;; delete-trailing-whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Completion: company-mode & corfu
;; (use-package company
;;   :init
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   :bind* (:map company-active-map
;;                ("<return>" . nil)
;;                ("RET" . nil)
;;                ("C-<return>" . company-complete-selection)
;; 	       ("<tab>" . company-complete-selection)
;; 	       ("TAB" . company-complete-selection)))

(use-package corfu
  ;; All from README
  :custom
  (corfu-separator ?\s)	;; Orderless field separator
  (corfu-scroll-margin 1) ;; Use scroll margin

  :init
  (corfu-global-mode)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

;;; Visual Interface
(use-package simple
  :ensure nil
  :init (setq-default fill-column 100)
  (global-visual-line-mode))

;; (use-package topspace
;;   :init (global-topspace-mode))

;;; In-Buffer Navigation

(use-package avy
  :bind* (("C-j" . avy-goto-char-timer))
  :init (setq avy-timeout-seconds 0.3))

(defhydra jf/hydra-resize-window (:hint nil)
  ""
  ("b" shrink-window-horizontally ">narrower<")
  ("f" enlarge-window-horizontally "<wider>")
  ("p" shrink-window "-shorter-")
  ("n" enlarge-window "=taller=")
  ("=" balance-windows "balance")
  ("-" text-scale-decrease "zoom out")
  ("+" text-scale-increase "zoom in")
  ("q" nil)
  ("<f7>" nil))


(defun jf/hydra-resize-window--helper ()
  "Check case where only one window is open."
  (interactive)
  (if (and (= (count-windows) 1) (not (equal current-prefix-arg '(4))))
      (message "Only one window present")
    (let ((current-prefix-arg '())) (jf/hydra-resize-window/body))))

(use-package window
  :ensure nil
  :config (setq recenter-positions '(0.5 0.17 0.83))
  :bind (("M-n" . scroll-down-line)
	 ("M-p" . scroll-up-line)
	 ("<f7>" . #'jf/hydra-resize-window--helper)))

(use-package emacs
  :ensure nil
  :init (setq sentence-end-double-space nil)
  :bind ("M-O" . #'mode-line-other-buffer))

(use-package view
  :ensure nil
  :bind ("C-v" . #'View-scroll-half-page-forward)
  ("M-v" . #'View-scroll-half-page-backward))


;;; multiple-cursors
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-symbol-like-this)
	 ("C-<" . 'mc/mark-previous-symbol-like-this)
	 ("C-c C-," . 'mc/mark-all-symbols-like-this))
  :bind (:map mc/keymap ("<return>" . nil)))

;;; Visual QoL

(use-package hl-line
  :ensure nil
  :init (global-hl-line-mode))

(use-package olivetti
  :bind (("<f3>" . olivetti-mode))
  :init (setq olivetti-body-width 100))

(use-package pulse
  :ensure nil
  :init
  ;; https://karthinks.com/software/batteries-included-with-emacs/
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window ace-window))
    (advice-add command :after #'pulse-line)))

;;; flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.6))

(use-package consult-flycheck
  :bind ("M-g f" . consult-flycheck))

;;; flyspell
(use-package flyspell
  :ensure nil
  :config (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;;; window-divider
(use-package frame
  :ensure nil
  :init (setq window-divider-default-places 'bottom-only)
  (setq window-divider-default-bottom-width 1)
  (window-divider-mode))

;;; display-line-numbers
(use-package display-line-numbers
  :ensure nil
  :bind ("<f6>". display-line-numbers-mode)
  :init
  (setq display-line-numbers-minor-tick 10))

;; redacted-mode
(use-package redacted
  :bind (("<f4>" . #'redacted-mode))
  :init (add-hook 'redacted-mode-hook
		  (lambda () (read-only-mode (if redacted-mode 1 -1)))))

(setq gc-cons-percentage default-gc-percentage
      gc-cons-threshold default-gc-threshold)

(provide 'init.el)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
