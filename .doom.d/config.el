(defun make-obsolete (obsolete-name current-name &optional when)
  "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
OBSOLETE-NAME should be a function name or macro name (a symbol).

The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when) "23.1"))
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias (obsolete-name current-name
                                                        &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional access-type) "23.1"))
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)

(defmacro define-obsolete-variable-alias (obsolete-name current-name
                                                        &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
This uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(setq user-full-name  "Jonathan Fung"
      user-mail-address "jonathanfung2000@gmail.com")

;; "Source Code Pro", "Roboto Mono", "IBM Plex Mono", "JetBrains Mono"
;; doom-font (font-spec :family "JetBrains Mono" :size 16)
;; https://typeof.net/Iosevka/

(setq doom-font (font-spec :family "Iosevka SS14" :size 16 :weight 'semi-light)
      doom-big-font (font-spec :family "Iosevka SS14" :size 24)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16 :weight 'semi-light))

;; https://github.com/hlissner/doom-emacs/issues/3967
;; (setq doom-theme 'modus-operandi)
(setq doom-theme 'modus-vivendi)

;; (require 'modus-themes)                 ; common code
;; (require 'modus-operandi-theme)         ; light theme
;; (require 'modus-vivendi-theme)          ; dark theme

;; (load-theme 'modus-vivendi)           ; Dark theme
;; (load-theme 'modus-operandi)          ; Light theme


;;(global-set-key (kbd "<f5>") (lambda () (interactive) (modus-themes-toggle) (set-face-background 'mode-line "default")))

(global-set-key (kbd "<f5>") 'modus-themes-toggle)

(add-hook! 'modus-themes-after-load-theme-hook (set-face-background 'mode-line "default"))


;; Set customization options to values of your choice
(setq modus-themes-bold-constructs t
      modus-themes-slanted-constructs t
      modus-themes-syntax nil ; Lots of options---continue reading the manual
      modus-themes-no-mixed-fonts nil
      modus-themes-links nil ; Lots of options---continue reading the manual
      modus-themes-prompts 'subtle-accented ; {nil,'subtle-accented, 'intense-accented, 'subtle-gray, 'intense-gray}

      modus-themes-mode-line 'borderless ; {nil,'3d,'moody, 'borderless, 'borderless-3d, 'borderless-moody}
      modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
      modus-themes-fringes 'intense ; {nil,'subtle,'intense}

      modus-themes-lang-checkers 'colored-background ; Lots of options---continue reading the manual

      modus-themes-hl-line 'intense-background ; Lots of options---continue reading the manual

      modus-themes-subtle-line-numbers t ; {nil, t}
      modus-themes-paren-match nil ; {nil,'subtle-bold,'intense,'intense-bold}
      modus-themes-region 'no-extend ; Lots of options

      modus-themes-diffs nil ; {nil,'desaturated,'fg-only,'bg-only}

      modus-themes-org-blocks 'grayscale ; {nil,'grayscale,'rainbow}
      modus-themes-org-habit nil

      modus-themes-headings ; Lots of options---continue reading the manual
      '((1 . rainbow-section)
        ;; (2 . rainbow-line-no-bold)
        ;; (3 . no-bold)
        ;; (t . rainbow)
        ;; (t . rainbow-line)
        (t . rainbow-line-no-bold)
        ;; (t . highlight-no-bold)
        )

      modus-themes-scale-headings nil
      modus-themes-scale-1 1.1
      modus-themes-scale-2 1.15
      modus-themes-scale-3 1.21
      modus-themes-scale-4 1.27
      modus-themes-scale-5 1.33

      modus-themes-variable-pitch-ui nil
      modus-themes-variable-pitch-headings nil)

;; with Emacs 28, default seems to have a gray background for everything, this turns that to white
;; (setq modus-themes-operandi-color-overrides
;;       '(
;;         (bg-alt . "#ffffff")
;;         ))
;; (setq modus-themes-vivendi-color-overrides
;;       '(
;;         (bg-alt . "#000000")
;;         ))

;; only for "packaged variants" (?)
;; (modus-themes-load-themes)
;; (modus-themes-load-operandi)

;; Or load via a hook
;; (add-hook! 'after-init-hook #'modus-themes-load-operandi)

                                        ;includes part of the file's directory name at the beginning of the shared buffer name to make unique
(setq uniquify-buffer-name-style 'forward)
;; this may do the same thing as uniquify-buffer...
(setq ivy-rich-path-style 'abbrev)

;; (setq display-line-numbers-type 'visual)
(setq display-line-numbers-type nil)

                                        ; just editted these line 12/24
;; idk what these 2 lines do
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
;; (set-face-attribute 'default t :font "Source Code Pro-10")

;; CAUTION
;; This might be fatal, might turn off all keymaps
;; (setq display-battery-mode t)

;; (setq display-time-mode t)
(display-time-mode nil)
(setq display-time-default-load-average nil)
(setq line-number-mode t
      column-number-mode t)
(set-face-background 'mode-line "default")

(setq doom-modeline-buffer-encoding nil)
;; (setq doom-modeline-buffer-encoding t)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)

(setq hl-line-mode nil)
(map! :n "SPC t h" #'hl-line-mode)

; meant to only have hl-line highlight on end of line
(defun my-hl-line-range-function () (cons (line-end-position) (line-beginning-position 2)))
;(setq hl-line-range-function #'my-hl-line-range-function)

; standard full-width
(defun my-hl-line-range ()
  "Used as value of `hl-line-range-function'."
  (cons (line-beginning-position) (line-end-position)))

(setq-default hl-line-range-function #'my-hl-line-range)

(defun jf/toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 5))
  (redraw-frame (selected-frame)))

(map! :n "SPC t v" 'jf/toggle-line-spacing)

;;; term-cursor.el --- Change cursor shape in terminal -*- lexical-binding: t; coding: utf-8; -*-

;; Version: 0.4
;; Author: h0d
;; URL: https://github.com/h0d
;; Keywords: terminals
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Send terminal escape codes to change cursor shape in TTY Emacs.
;; Using VT520 DECSCUSR (cf https://invisible-island.net/xterm/ctlseqs/ctlseqs.html).
;; Does not interfere with GUI Emacs behavior.

;;; Code:

(defgroup term-cursor nil
  "Group for term-cursor."
  :group 'terminals
  :prefix 'term-cursor-)

;; Define escape codes for different cursors
(defcustom term-cursor-block-blinking "\e[1 q"
  "The escape code sent to terminal to set the cursor as a blinking box."
  :type 'string
  :group 'term-cursor)

(defcustom term-cursor-block-steady "\e[2 q"
  "The escape code sent to terminal to set the cursor as a steady box."
  :type 'string
  :group 'term-cursor)

(defcustom term-cursor-underline-blinking "\e[3 q"
  "The escape code sent to terminal to set the cursor as a blinking underscore."
  :type 'string
  :group 'term-cursor)

(defcustom term-cursor-underline-steady "\e[4 q"
  "The escape code sent to terminal to set the cursor as a steady underscore."
  :type 'string
  :group 'term-cursor)

(defcustom term-cursor-bar-blinking "\e[5 q"
  "The escape code sent to terminal to set the cursor as a blinking bar."
  :type 'string
  :group 'term-cursor)

(defcustom term-cursor-bar-steady "\e[6 q"
  "The escape code sent to terminal to set the cursor as a steady bar."
  :type 'string
  :group 'term-cursor)

;; Current cursor evaluation
(defcustom term-cursor-triggers (list 'blink-cursor-mode-hook 'lsp-ui-doc-frame-hook)
  "Hooks to add when the variable watcher might not be enough.
That is, hooks to trigger `term-cursor--immediate'."
  :type 'list
  :group 'term-cursor)

;;;###autoload
(define-minor-mode term-cursor-mode
  "Minor mode for term-cursor."
  :group 'term-cursor
  (if term-cursor-mode
      (term-cursor-watch)
    ;; else
    (term-cursor-unwatch)))

;;;###autoload
(define-globalized-minor-mode global-term-cursor-mode term-cursor-mode
  (lambda ()
    (term-cursor-mode t))
  :group 'term-cursor)

(defun term-cursor--normalize (cursor)
  "Return the actual value of CURSOR.
It can sometimes be a `cons' from which we only want the first element (cf `cursor-type')."
  (if (consp cursor)
      (car cursor)
    ;; else
    cursor))

(defun term-cursor--determine-esc (cursor blink)
  "Return an escape code depending on the CURSOR and whether it should BLINK."
  (cond (;; Vertical bar
	 (eq cursor 'bar)
	 (if blink term-cursor-bar-blinking
	   term-cursor-bar-steady))
	(;; Underscore
	 (eq cursor 'hbar)
	 (if blink term-cursor-underline-blinking
	   term-cursor-underline-steady))
	(;; Box — default value
	 t
	 (if blink term-cursor-block-blinking
	   term-cursor-block-steady))))

(defun term-cursor--eval (cursor blink)
  "Send escape code to terminal according to CURSOR and whether it should BLINK."
  (unless (display-graphic-p) ; Must be in TTY
    ;; CURSOR can be a `cons' (cf. `cursor-type')
    (setq cursor
	  (term-cursor--normalize cursor))

    ;; Ask terminal to display new cursor
    (send-string-to-terminal
     (term-cursor--determine-esc cursor blink))))

(defun term-cursor--immediate ()
  "Send an escape code without waiting for `term-cursor-watcher'."
  (term-cursor--eval cursor-type blink-cursor-mode))

(defun term-cursor-watcher (_symbol cursor operation _watch)
  "Change cursor shape through escape sequences depending on CURSOR.
Waits for OPERATION to be 'set."
  (when (eq operation 'set)  ; A new value must be set to the variable
    (term-cursor--eval cursor blink-cursor-mode)))

(defun term-cursor-watch ()
  "Start reacting to cursor change."
  (add-variable-watcher 'cursor-type #'term-cursor-watcher)
  (dolist (hook term-cursor-triggers)
    (add-hook hook #'term-cursor--immediate)))

(defun term-cursor-unwatch ()
  "Stop reacting to cursor change."
  (remove-variable-watcher 'cursor-type #'term-cursor-watcher)
  (dolist (hook term-cursor-triggers)
    (remove-hook hook #'term-cursor--immediate)))

(provide 'term-cursor)

;;; term-cursor.el ends here

(global-term-cursor-mode)

;; AUTHOR:  Nikolaj Schumacher -- https://github.com/nschum/fringe-helper.el
;;
(defun fringe-helper-convert (&rest strings)
  "Convert STRINGS into a vector usable for `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap.
Periods (.) are background-colored pixel; Xs are foreground-colored. The
fringe bitmap always is aligned to the right. If the fringe has half
width, only the left 4 pixels of an 8 pixel bitmap will be shown.
For example, the following code defines a diagonal line.
\(fringe-helper-convert
\"XX......\"
\"..XX....\"
\"....XX..\"
\"......XX\"\)"
  (unless (cdr strings)
    ;; only one string, probably with newlines
    (setq strings (split-string (car strings) "\n")))
  (apply 'vector
         (mapcar
          (lambda (str)
            (let ((num 0))
              (dolist (c (string-to-list str))
                (setq num (+ (* num 2) (if (eq c ?.) 0 1))))
              num))
          strings)))

;; this is default, a tilde
;; (setq vi-tilde-fringe-bitmap-array '[0 0 0 113 219 142 0 0])

(setq fringe-mode 8)

;; for some reason, my fringe is default to 4 pixels wide (defined in fringe-mode), but need the full 8
;; 23 is the max height
;; (setq vi-tilde-fringe-bitmap-array (fringe-helper-convert
;;                                     "xx......"
;;                                     ".xx....."
;;                                     "..xx...."
;;                                     "..xx...."
;;                                     ".xx....."
;;                                     "xx......"
;;                                     ))
(setq vi-tilde-fringe-bitmap-array (fringe-helper-convert
                                    "xxxx...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "xxxx...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    "...x...."
                                    ))

(setq vi-tilde-fringe-bitmap-array (fringe-helper-convert
                                    ".x.xx..."
                                    "..x.xx.."
                                    "x..x.xx."
                                    "xx..x.xx"
                                    "x..x.xx."
                                    "..x.xx.."
                                    ".x.xx..."
                                    ))

;; (setq vi-tilde-fringe-bitmap-array (fringe-helper-convert
;;                                     ".x..x..."
;;                                     "..x..x.."
;;                                     "x..x..x."
;;                                     ".x..x..x"
;;                                     "x..x..x."
;;                                     "..x..x.."
;;                                     ".x..x..."
;;                                     ))

;; (setq vi-tilde-fringe-bitmap-array (fringe-helper-convert
;;                                     "xxxxxxxx"
;;                                     "x..xx..x"
;;                                     "x..xx..x"
;;                                     "xxxxxxxx"
;;                                     "x..xx..x"
;;                                     "x..xx..x"
;;                                     "xxxxxxxx"
;;                                     "x..xx..x"
;;                                     "x..xx..x"
;;                                     "xxxxxxxx"
;;                                     ))

                                        ; Bind Zooms??
(map! :n "C-_" #'er/contract-region
      :n "C-+" #'er/expand-region)

;; ; unbind J,K,M
(map! :map evil-normal-state-map "J" nil
      "K" nil)
(map! :map evil-motion-state-map "M" nil
      "K" nil)

(map! :n "V" #'evil-visual-line)

;; ; rebind J,K for scrolling
(map! :n "J" #'evil-scroll-line-up)
(map! :n "K" #'evil-scroll-line-down)

;; ; bind M for contexual lookup
(map! :n "M" #'+lookup/documentation)

;; ;; Make evil-mode up/down operate in screen lines instead of actual lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; ;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

;; Bind toggles
(global-set-key (kbd "<f2>") 'mixed-pitch-mode)
(global-set-key (kbd "<f3>") 'olivetti-mode)
(global-set-key (kbd "<f4>") 'toggle-rot13-mode)
(setq olivetti-body-width 110)
;; (global-set-key (kbd "U") 'undo-tree-redo)

(setq fill-column 100)

;; Unbind language input switcher
(map! :map global-map "C-\\" nil)

;; Bind toggle for 80-char limit, buffer-wide
(map! :n "C-\\" 'display-fill-column-indicator-mode)

;; currently do not use org-roam, need to delete
;; (setq org-roam-directory "~/emacs/org-roam")
;; (setq org-roam-index-file "index.org")
;; (define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
;; (define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
;; (define-key org-roam-mode-map (kbd "C-c n j") #'org-roam-jump-to-index)
;; (define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
;; (define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-graph)
;; (define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
;; (require 'org-roam-protocol)

(setq global-undo-tree-mode t)

(map! :n "SPC f a" 'save-some-buffers)

;; (map! :map org-agenda-mode-map "SPC f a" 'save-some-buffers)

(map! :map doom-leader-map "f a" 'save-some-buffers)

(defun jf/find-buffer-pdf ()
  "Takes the current buffer's FILE-NAME and opens up FILE-NAME.pdf
Defaults to pdf-tools, or native doc-view."
  (interactive)
  (find-file
   (concat
    "./"
    (car (split-string (file-name-nondirectory (buffer-name)) "\\."))
    ".pdf")))

(map! :n "SPC o p" 'jf/find-buffer-pdf)

(map! :n "SPC e" 'eval-last-sexp)

(defun jf/org-latex-preview-scale (scale bool)
  (interactive (list (read-number "Scale: ") (y-or-n-p "Day Mode? ")))
  (setq org-format-latex-options `(
                                   :foreground ,(if bool (format "Black") (format "White"))
                                   :background default
                                   :scale ,scale
                                   :html-foreground "Black"
                                   :html-background "Transparent"
                                   :html-scale 1.0
                                   :matchers '("begin" "$1" "$" "$$" "\\(" "\\["))
        )
  )

(map! :n "SPC t c" 'transpose-chars)

(setq org-directory "~/org/")

(setq org-ellipsis " ▾")
(setq org-startup-folded 'content)

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;; (setq org-superstar-headline-bullets-list
;;       '("✸" ("◉" ?◈) "○" "▷"))

;; https://www.reddit.com/r/emacs/comments/lapujj/weekly_tipstricketc_thread/glvoifj/
(setq org-superstar-headline-bullets-list '("☰" "☷" "▶" "●" "✱" "✲" "✸" "⦿" "⌾" "◦"))

(setq org-highlight-latex-and-related '(native script))

(map! :n "SPC o l" 'link-hint-open-link-at-point)

(map! :map org-mode-map "C-j" nil
      "C-k" nil)

(map! :map org-mode-map "C-j" 'org-next-visible-heading
      "C-k" 'org-previous-visible-heading)

;; seems to break doom config ?
;; (require 'org-inlinetask)

;; https://www.reddit.com/r/orgmode/comments/6q6cdk/adding_files_to_the_agenda_list_recursively/
;; doom doctor: org-agenda-file-regexp seems to be void
;; (setq org-agenda-files (apply 'append
;;                   (mapcar
;;                    (lambda (directory)
;;                  (directory-files-recursively
;;                   directory org-agenda-file-regexp))
;;                    '("~/School/W21/" "~/org/"))))


;; (directory-files-recursively "~/org" (rx ".org" eos))
;; Need to manually update based on school term
(setq org-agenda-files '("~/org"
                         "~/org/blog"
                         "~/org/voxpop"
                         "~/org/resources"
                         "~/School/S21/ENGR_165_Manuf"
                         "~/School/S21/MSE_165C_Phase"
                         "~/School/S21/MSE_189C_Snr"
                         "~/School/S21/STATS_120C_Prob"
                         "~/rust/effex"
                         ))

(setq org-tag-faces
      '(("Phase" . "gold2")
        ("Nano" . "lime green")
        ("Manuf" . "red2")
        ("Snr" . "medium orchid")
        ("Stats" . "dodger blue")))

(setq org-agenda-start-day "+0"
      org-agenda-span 1) ;; for use with day-by-day view

(setq org-agenda-timegrid-use-ampm t)
(setq org-agenda-time-grid
      (quote
       ((daily today require-timed)
        (400 1200 1600 2000 2400)
        "  ⟿" "―――――――――――――――――――――――"))) ; 2400 is the next day

(setq org-super-agenda-date-format "%A, %e %b")
(setq org-super-agenda-header-separator ?―)
;; (setq org-super-agenda-header-separator "")
(org-super-agenda-mode)

(map! :map org-super-agenda-header-map "k" nil
      "j" nil)

                                        ; removes 'agenda' prefix coming from agenda.org
                                        ; also adds in effort level
                                        ; should be (todo   . " %i %-12:c") if using multiple files
(setq org-agenda-prefix-format
      '(
        ;; (agenda . "%i %-7T%?-12t% s")
        (agenda . "%i %?-12t% s")
        ;; (todo   . " %i %-12:c")
        (todo   . " [%e] ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

;; refreshes org agenda view every 60 seconds, but runs on any buffer
;; (run-with-idle-timer 60000 t (lambda () (org-agenda nil "z")) )


(set-face-attribute 'org-agenda-date nil
                    :weight 'bold :overline t :foreground "#00538b" )

(setq org-agenda-custom-commands
      '(("z" "Super View, Everyday"
         (
          (agenda "" ((org-super-agenda-groups
                       '((:name ""
                          :time-grid t
                          :date today
                          :deadline today
                          ;; :scheduled today
                          :order 0
                          :discard (:anything t)
                          )))))
          (alltodo "" ((org-agenda-overriding-header (concat
                                                      (make-string 1 ?\n)
                                                      "Today is " (org-read-date nil nil "+0d")
                                                      ))
                       (org-super-agenda-groups
                        '(
                          (:name "Overdue"
                           :deadline past
                           :order 0)
                          (:name "Scheduled"
                           :auto-planning t
                           :order 0)
                          (:name "========\n Personal"
                           :tag "Person"
                           :order 10)
                          (:name "Email"
                           :tag "Email"
                           :order 15)
                          (:discard (:anything t))
                          ))))
          ))))

(defun jf/org-agenda-day-by-day ()
  (interactive)
  (org-agenda nil "z"))
(map! :n "SPC o v" 'jf/org-agenda-day-by-day)

(defun jf/org-agenda-regular-view ()
  (interactive)
  (org-agenda nil "a"))
(map! :n "SPC o c" 'jf/org-agenda-regular-view)

;; from https://github.com/alphapapa/org-super-agenda/issues/59
;; function is needed to always eval relative dates
(defun jf/org-agenda-relative-deadline ()
  (interactive)
  (let ((org-super-agenda-groups
         `(
           (:name "Past"
            :deadline past)
           (:name "Next Items"
            :todo "NEXT")
           (:name "Clean up Notes"
            :todo "NOTE")
           (:name "Today's Time Blocks"
            :and (:todo "BLOCK"
                  :date today))
           (:name "Today"
            :deadline today)
           (:name "Tomorrow (+1)"
            ;; before acts as <
            :deadline (before ,(org-read-date nil nil "+2d")))
           (:name "Tomorrow Tomorrow (+2)"
            ;; if today is 1, should show (before (1+3)) = 1, 2,3
            :deadline (before ,(org-read-date nil nil "+3d")))
           (:name "Day After Tomorrow Tomorrow (+3)"
            :deadline (before ,(org-read-date nil nil "+4d")))
           (:name "Within a Week (+4..6)"
            :deadline (before ,(org-read-date nil nil "+7d")))
           (:name "Within 30 Days (+7..30)"
            :deadline (before ,(org-read-date nil nil "+31d")))
           (:name "========\n Personal"
            :tag "Person"
            :order 10)
           (:name "Email"
            :tag "Email"
            :order 15)
           (:discard (:anything t))
           )))
    (org-agenda nil "t")
    ;; (org-agenda-list)
    ;; this allows time grid to show with TODO, but doesn't catch
    ;; NEXT, Personal, and doesn't extend to 30 days
    ;; Text is also red for some reason
    ))

;; see https://github.com/alphapapa/org-super-agenda/issues/153
;; for a combined deadline-scheduled view with repeating items

(defun jf/org-agenda-relative-scheduled ()
  (interactive)
  (let ((org-super-agenda-groups
         `(
           (:name "Past"
            :scheduled past)
           (:name "Next Items"
            :todo "NEXT")
           (:name "Clean up Notes"
            :todo "NOTE")
           (:name "Today's Time Blocks"
            :and (:todo "BLOCK"
                  :date today))
           (:name "Scheduled Today"
            :scheduled today)
           (:name "Scheduled Tomorrow (+1)"
            :scheduled (before ,(org-read-date nil nil "+2d")))
           (:name "Scheduled Tomorrow Tomorrow (+2)"
            ;; if today is 1, should show (before (1+3)) = 1, 2,3
            :scheduled (before ,(org-read-date nil nil "+3d")))
           (:name "Scheduled Within a Week (+3..6)"
            :scheduled (before ,(org-read-date nil nil "+7d")))
           (:name "Scheduled Within 30 Days (+7..30)"
            :scheduled (before ,(org-read-date nil nil "+31d")))
           (:name "========\n Personal"
            :tag "Person"
            :order 10)
           (:name "Email"
            :tag "Email"
            :order 15)
           (:discard (:anything t))
           )))
    (org-agenda nil "t")))

(map! :map doom-leader-map "o b" nil)
(map! :n "SPC o b" 'jf/org-agenda-relative-deadline)
(map! :n "SPC o g" 'jf/org-agenda-relative-scheduled)

(defun jf/reset-relative-deadline-super-agenda ()
  (interactive)
  (org-agenda-quit)
  (jf/org-agenda-relative-deadline)
  )
(defun jf/reset-relative-scheduled-super-agenda ()
  (interactive)
  (org-agenda-quit)
  (jf/org-agenda-relative-scheduled)
  )

(map! :map org-agenda-mode-map "r" 'jf/reset-relative-deadline-super-agenda)
(map! :map org-agenda-mode-map "R" 'jf/reset-relative-scheduled-super-agenda)

(defhydra jf/hydra-agenda (:color blue
                           :hint nil)
  "
^Relative^      ^Absolute^      ^Time-Grid^
^^^--------------------------------------
_d_: deadline   _e_: everyday   _w_: regular
_s_: scheduled
  "
  ("d" jf/org-agenda-relative-deadline)
  ("s" jf/org-agenda-relative-scheduled)
  ("e" jf/org-agenda-day-by-day)
  ("w" jf/org-agenda-regular-view)
  )

;; (map! :n "SPC a" 'jf/hydra-agenda/body)
(map! :map doom-leader-map "a" 'jf/hydra-agenda/body)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "NOTE(m)" "BLOCK(b)" "STRT(s)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[+](P)" "[-](S)" "[?](W)" "|" "[X](D)")))

(setq org-todo-keyword-faces
      '(("[-]" . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("BLOCK" . +org-todo-active)
        ("NEXT" . +org-todo-active)
        ("[?]" . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)))

;; sort todos by deadline earliest first, then priority high first
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo deadline-up priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)) )

;; when set to t, toggling a repeating TODO item to DONE will reset the TODO prefix to the previous one
;; implemented when switching BLOCK -> DONE, which returns it to BLOCK and not TODO
;; reason: super-agenda selector is based on TODO name
(setq org-todo-repeat-to-state t)

(setq org-capture-templates
      '(("t" "Agenda TODO" entry (file "~/org/Agenda.org")
        "* TODO %? \n DEADLINE: %t" :prepend t)
        ("e" "email" entry (file+headline "~/org/Agenda.org" "Emails")
         "* TODO Reply: %? \n - %a" :prepend t)
        ("d" "designboard" entry (file "~/org/designboard.org")
         "* %? \n- %t" :prepend t)
      ))

(map! :n "SPC z" 'org-capture)

(setq org-latex-classes '(("article" "\\documentclass[11pt]{article}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          ("report" "\\documentclass[11pt]{report}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("book" "\\documentclass[11pt]{book}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("notes"
                           "\\documentclass[8pt]{article}
  \\usepackage[letterpaper, portrait, margin=1in]{geometry}
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage{amsmath}
  \\usepackage{amssymb}
  \\usepackage{hyperref}
  \\usepackage{tcolorbox}
% http://tug.ctan.org/macros/latex/contrib/minted/minted.pdf
  \\usepackage[cache=false]{minted}
  \\setminted{breaklines=true, breakanywhere=true}
% \\usemintedstyle{paraiso-light} % pygmentize -L styles
% \\usemintedstyle{emacs} % pygmentize -L styles
% \\usemintedstyle{colorful} % pygmentize -L styles
% \\usemintedstyle{rainbow_dash} % pygmentize -L styles
  \\usemintedstyle{tango} % pygmentize -L styles
% https://tex.stackexchange.com/questions/112559/box-around-minted-environment
% https://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/tcolorbox/tcolorbox.pdf
  \\BeforeBeginEnvironment{minted}{\\begin{tcolorbox}[colframe=black!85!white, colback=black!5!white, boxrule=0.3mm]}
  \\AfterEndEnvironment{minted}{\\end{tcolorbox}}
  \\usepackage{enumitem}
  \\setitemize{itemsep=0.5pt}
  \\usepackage{lastpage}
  \\usepackage{fancyhdr}
  \\pagestyle{fancy}
  \\fancyhf{}
  \\usepackage{titling} % allows \thetitle \theauthor \thedate
  \\rhead{\\theauthor}
  \\lhead{\\thetitle}
  \\rfoot{\\thepage{} of \\pageref{LastPage}}
  \\linespread{1}
  \\setlength{\\parindent}{0pt}
  \\setlength{\\parskip}{0.5em plus 0.1em minus 0.2em}
  \\hypersetup{pdfborder=0 0 0}
  \\setcounter{secnumdepth}{0}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                           )))

(map! :n "SPC r r" #'org-latex-export-to-pdf)

(setq org-export-headline-levels 5)

(setq org-format-latex-options '(:foreground default :background default :scale 3.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                                 ("begin" "$1" "$" "$$" "\\(" "\\[")) )

;; minted uses Pygments (python) to syntax highlight pdf exported source blocks
;; look into https://www.reddit.com/r/emacs/comments/lbkmmz/the_best_syntax_highlighting_in_a_pdf_youll_see_a/
;; as an alternative
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(map! :n "SPC t i" #'org-indent-mode)

(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  ;; directory is ~/.emacs.d/local/etc/templates
  :config
  (define-auto-insert "\\.org?$" "default-autoinsert.org"))

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun rasmus/org-prettify-symbols ()
    (interactive)
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?⏠) ;; ➤ ➟ ✎
                               ("#+end_src"   . ?⏡) ;; ⏹
                               ("#+header:" . ,rasmus/ob-header-symbol)
                               ("#+begin_quote" . ?❝)
                               ("#+end_quote" . ?❞)))))
    (turn-on-prettify-symbols-mode))

  (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols)

(setq swiper-use-visual-line nil)
(setq swiper-use-visual-line-p (lambda (a) nil))

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        ))
; add this into above hook to default to sorting by edit time
; (dired-sort-toggle-or-edit)

(map! :n "SPC o o" #'treemacs-visit-node-in-external-application)
(map! :n "SPC o t" #'treemacs)
(setq treemacs-position 'right
      treemacs-width 25
      treemacs-indentation 1)

;define function that syncs mbsync and refreshes notmuch
(defun jf/sync-email ()
  "Lists the contents of the current directory."
  (interactive)
  (shell-command "mbsync -a && notmuch new"))

; bind notmuch-hello view
(map! :n "SPC o n" #'notmuch-hello)
; bind custom function to sync mbsync and notmuch
(map! :n "SPC r s" 'jf/sync-email)

;; attempt to fix notmuch formatting
(setq notmuch-search-result-format
  '(("date" . "%12s ")
    ("count" . "%-6s ")
    ("authors" . "%-15s ")
    ("subject" . "%-10s ")
    ("tags" . "(%s)"))
)
(defun jf/establish-notmuch ()
  (interactive)
(setq notmuch-saved-searches '((:name "Personal"
                                :query "tag:inbox AND to:jonathanfung2000@gmail.com AND date:nov_3_2020..today AND NOT tag:delete")
                               (:name "UCI"
                                :query "tag:inbox AND to:fungjm@uci.edu AND date:nov_3_2020..today AND NOT tag:delete")
                               (:name "Clean Gen Inbox"
                                :query "tag:inbox AND date:nov_3_2020..today AND NOT to:fungjm@uci.edu AND NOT to:jonathanfung2000@gmail.com")
                               (:name "Flagged"
                                :query "tag:inbox AND tag:flagged")
                               (:name "Inbox"
                                :query "tag:inbox"))))

(map! :n "SPC r e" 'jf/establish-notmuch)

; this sets cursor of notmuch-hellow to first saved search
(add-hook 'notmuch-hello-refresh-hook
          (lambda ()
            (if (and (eq (point) (point-min))
                     (search-forward "Saved searches:" nil t))
                (progn
                  (forward-line)
                  (widget-forward 1))
              (if (eq (widget-type (widget-at)) 'editable-field)
                  (beginning-of-line)))))

(setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))

;;; scimax-inkscape.el --- Using inkscape in org-mode

;;; Commentary:
;;
;; This library provides a new org-mode link for inkscape svg files. When you
;; click on an inkscape link, it will open the figure in inkscape. A thumbnail
;; image will be placed on the inkscape link.
;;
;; Export to HTML:
;; (browse-url (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
;;   (org-html-export-to-html)))
;;
;; (org-open-file (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
;;   (org-latex-export-to-pdf)))
;;
;; inkscape does not allow you to create empty files. We save the template in a
;; variable and create them on demand.

(defcustom scimax-inkscape-thumbnail-width 300
  "Width of thumbnails in pts."
  :group 'scimax-inkscape
  :type 'integer)

(defcustom scimax-inkscape-template-svg
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"6in\"
   height=\"4in\"
   viewBox=\"0 100 152.4 201.6\"
   version=\"1.1\"
   id=\"svg8\"
   inkscape:version=\"0.92.2 (5c3e80d, 2017-08-06)\"
   sodipodi:docname=\"drawing.svg\">
  <defs
     id=\"defs2\" />
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#ffffff\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"0.0\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"1\"
     inkscape:cx=\"400\"
     inkscape:cy=\"214.9\"
     inkscape:document-units=\"in\"
     inkscape:current-layer=\"layer1\"
     showgrid=\"false\"
     units=\"in\"
     inkscape:window-width=\"1080\"
     inkscape:window-height=\"675\"
     inkscape:window-x=\"0\"
     inkscape:window-y=\"78\"
     inkscape:window-maximized=\"0\"
     inkscape:lockguides=\"true\"
     fit-margin-top=\"0\"
     fit-margin-left=\"0\"
     fit-margin-right=\"0\"
     fit-margin-bottom=\"0\" />
  <metadata
     id=\"metadata5\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
        <dc:title></dc:title>
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label=\"Layer 1\"
     inkscape:groupmode=\"layer\"
     id=\"layer1\"
     transform=\"translate(0,0)\" />
</svg>
"
  "Blank document for inkscape. You cannot create a file at the
  command line, so we put this template in and open it. This one works for Inkscape 0.92.2"
  :group 'scimax-inkscape
  :type 'string)


(defun scimax-inkscape-open (path)
  "Open the PATH in inkscape.
Make a new file if needed."
  (interactive)
  (unless (f-ext-p path "svg") (error "Must be an svg file."))
  (unless (file-exists-p path)
    (with-temp-file path
      (insert scimax-inkscape-template-svg)))
  (let ((display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window . ())))))
    (shell-command (format "inkscape %s &" path))))


(defun scimax-inkscape-thumbnail (start end path bracketp)
  "Put a thumbnail on an inkscape link."
  (let (img ov)
    (when (and
	   ;; got a path
	   path
	   ;; it is an image
	   (org-string-match-p (image-file-name-regexp) path)
	   ;; and it exists
	   (f-exists? path)
	   ;; and there is no overlay here.
	   (not (ov-at start)))
      (setq img (create-image
		 (expand-file-name path)
		 'imagemagick nil :width scimax-inkscape-thumbnail-width
		 :background "lightgray"))
      (setq ov (make-overlay start end))
      (overlay-put ov 'display img)
      (overlay-put ov 'face 'default)
      ;; (overlay-put ov 'before-string "inkscape:")
      (overlay-put ov 'org-image-overlay t)
      (overlay-put ov 'modification-hooks
		   (list
		    `(lambda (&rest args)
		       (org-display-inline-remove-overlay ,ov t ,start ,end))))
      (push ov org-inline-image-overlays))))


(defun scimax-inkscape-redraw-thumbnails (&rest args)
  "Use font-lock to redraw the links."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-restart-font-lock)))

;; This gets the thumbnails to be redrawn with inline image toggling.
(advice-add 'org-display-inline-images :after 'scimax-inkscape-redraw-thumbnails)


(defun scimax-inkscape-preprocess (backend)
  "Preprocessing function to run in `org-export-before-processing-hook'.
Here are two examples:
 (browse-url (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
  (org-html-export-to-html)))
 (org-open-file (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
  (org-latex-export-to-pdf)))"
  (let ((links (reverse (org-element-map (org-element-parse-buffer) 'link
			  (lambda (link)
			    (when (string= (org-element-property :type link) "inkscape")
			      link))))))
    (cl-loop for link in links
	     do
	     (goto-char (org-element-property :begin link))
	     (re-search-forward "inkscape:" (org-element-property :end link))
	     (replace-match "file:"))))


(org-link-set-parameters
 "inkscape"
 :follow 'scimax-inkscape-open
 :help-echo "Click to open in inkscape."
 :activate-func 'scimax-inkscape-thumbnail
 :export (lambda (path desc backend)
	   ;;  You need to use the `scimax-inkscape-preprocess' function in a hook for
	   ;; more advanced export options like captions.
	   (cond
	    ((eq 'latex backend)
	     (format "\\includesvg{%s}" path))
	    ((eq 'html backend)
	     (format "<img src=\"%s\"" path)))))


(defun scimax-inkscape-insert-drawing (path)
  "Convenience function to insert a drawing with filename PATH."
  (interactive "sFilename: ")
  (insert (format "inkscape:%s" path)))

;; original definition
;; (setq org-latex-pdf-process '("%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f"))

;; definition needed to latex export svgs (in [[./foo.svg]] format)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;; wwg.el --- writer word goals

;; Copyright (C) 2021 Andrea

;; Author: Andrea andrea-dev@hotmail.com>
;; Version: 0.0.0
;; Package-Version: 20210121
;; Keywords: writing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A word number countdown for your writing goals.
;;
;; This mode helps you staying focused on the number of words you
;; setup for your goals. The more you write the closer you get to your
;; self-set goal for this session.
;;
;; See documentation on https://github.com/ag91/writer-word-goals

;;; Code:

(defgroup wwg nil
  "Options specific to wwg."
  :tag "wwg"
  :group 'wwg)

(defvar wwg/active-timer-alist nil "Alist of (buffer . timer) bindings to cleanup achieved targets.")

(defcustom wwg/monitor-period 15 "How many seconds before checking if a writer has reached the target number of words. Defaults to a minute." :group 'wwg)

(defvar wwg/monitor-function 'wwg/check-count-and-beep-with-message-if-finished "The function to monitor the target was reached in buffer. It takes two arguments: a number (target) and the buffer. It should return any value when it finds the target satisfied for cleanup purposes.")

(defun wwg/check-count-and-beep-with-message-if-finished (target-count buffer)
  "Beep if TARGET-COUNT was reached in BUFFER."
  (let* ((total-so-far (with-current-buffer buffer (count-words (point-min) (point-max))))
         (remaining-words (- target-count total-so-far)))
    (if (<= remaining-words 0)
        (progn
          (beep)
          (message
           "Well done! You wrote %s words, and %s extra words!!"
           target-count
           (abs remaining-words))
          'finished)
      (progn
        (message
         "Okay! %s words left."
         remaining-words)
        nil))))

(defun wwg/run-monitor (target-number buffer)
  "Call `wwg/monitor-function' with TARGET-NUMBER and BUFFER and cleanup timer if completed."
  (when (and
         (eq buffer (current-buffer))
         (funcall wwg/monitor-function target-number buffer))
    (cancel-timer (car (alist-get buffer wwg/active-timer-alist)))))

(defun wwg/monitor-word-count-for-buffer (target-number buffer)
  "Monitor every `wwg/monitor-period' seconds if the writer reached the TARGET-NUMBER in BUFFER."
  (add-to-list
   'wwg/active-timer-alist
   (list
    buffer
    (run-with-timer
     wwg/monitor-period
     wwg/monitor-period
     `(lambda () (wwg/run-monitor ,target-number ,buffer))))))

(defun wwg/set-goal-current-buffer (number-of-words)
  "Monitor when you achieve the target NUMBER-OF-WORDS."
  (interactive "nHow many words do you want to write for this session?")
  (let ((buffer (current-buffer))
        (words-already-there (count-words (point-min) (point-max))))
    (wwg/monitor-word-count-for-buffer (+ number-of-words words-already-there) buffer)))

(defun wwg/set-1k-goal-current-buffer ()
  "Monitor when you achieve the target 1k words."
  (interactive)
  (wwg/set-goal-current-buffer 1000))


(provide 'wwg)
;;; wwg ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:

(after! persp-mode
(setq persp-emacsclient-init-frame-behaviour-override "main"))

;; (setq initial-buffer-choice t)

;; (setq desktop-auto-save-timeout 300)
;; (setq desktop-dirname "~/.emacs.d/.local/etc")
;; (setq desktop-base-file-name "desktop")
;; (setq desktop-load-locked-desktop t)
;; (desktop-save-mode 1)
;; (add-hook 'server-after-make-frame-hook 'desktop-read)

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode sql-mode tex-mode latex-mode julia-mode))

;; forces errors to show only when hover on symbol, not on sideline
(setq lsp-ui-sideline-show-diagnostics nil)

(map! :n "SPC t u" #'lsp-ui-doc-mode)
(setq lsp-ui-doc-max-height 24)
(setq lsp-ui-doc-max-width 80)
(setq lsp-ui-doc-delay 0.01)

;; this one is for testing, doesn't seem to work 5/26/21
(map! :n "SPC t y" #'lsp-ui-doc-focus-frame)

;; https://github.com/nnicandro/emacs-jupyter/issues/306
(require 'ob-jupyter)

;; used when using Pkg.add("LanguageServer")
(setq lsp-julia-package-dir nil)

                                        ; for some reason emacs can't find the julia bin in PATH
(setq julia-repl-executable-records
      '((default "~/julia-1.6.0/bin/julia")
        ))

(after! julia-repl
  (julia-repl-set-terminal-backend 'vterm)
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (julia . t)
   (python . t)
   (jupyter . t)))

(setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                    (:session . "jl")
                                                    (:kernel . "julia-1.6")))
;; just using a snippet instead
;; (add-to-list 'org-structure-template-alist
;; '("j" . "src jupyter-julia :session jl :results graphics"))
;; https://github.com/nnicandro/emacs-jupyter#jupyter-eval-use-overlays

(setq jupyter-eval-use-overlays t)
(set-face-attribute 'jupyter-eval-overlay nil
                    :underline t)

;; required to get lsp to work
;; https://github.com/non-Jedi/lsp-julia/issues/35
(setq lsp-enable-folding t)

(setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))

;; (map! :map julia-repl-mode-map "M-RET" 'julia-repl-send-region-or-line)
(map! :map jupyter-repl-interaction-mode-map "M-RET" 'jupyter-eval-line-or-region)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

;(annotate-mode)

;; enable elgantt - https://github.com/legalnonsense/elgantt/
;; (add-to-list 'load-path (concat user-emacs-directory "elgantt/")) ;; Or wherever it is located
;; (require 'elgantt)

;; (defun toggle-header-line-format ()
;;     "Toggle buffer-local var header-line-format as pseudo-top margin"
;;     (setq header-line-format (if (eq header-line-format nil) t nil))
;;     (interactive)
;;     (redraw-display))
;; (global-set-key (kbd "<f6>") 'toggle-header-line-format)
; use with set-face-font header-line
;(set-face-background 'header-line "white")

;(map! :n "SPC r r" #'pandoc-convert-to-pdf)

; unbind SPC p F
;(map! :map doom-leader-map "p F" nil)
; rebind SPC p F to search all projects' files
;(map! :n "SPC p F" #'projectile-find-file-in-known-projects)

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; ;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
;; (set-email-account! "Personal"
;;   '((mu4e-sent-folder       . "/gmail/[Gmail].Sent Mail")
;;     ;(mu4e-drafts-folder     . "/gmail/Drafts")
;;     (mu4e-trash-folder      . "/gmail/[Gmail].Trash")
;;     (mu4e-refile-folder     . "/gmail/[Gmail].All Mail")
;;     (smtpmail-smtp-user     . "jonathanfung2000@gmail.com")
;;     ;; (mu4e-compose-signature . "---\nHenrik Lissner"))
;;   t))
;; (set-email-account! "UCI"
;;   '((mu4e-sent-folder       . "/uci/[Gmail].Sent Mail")
;;     ;(mu4e-drafts-folder     . "/gmail/Drafts")
;;     (mu4e-trash-folder      . "/uci/[Gmail].Trash")
;;     (mu4e-refile-folder     . "/uci/[Gmail].All Mail")
;;     (smtpmail-smtp-user     . "fungjm@uci.edu")
;;     ;; (mu4e-compose-signature . "---\nHenrik Lissner"))
;;   t)
