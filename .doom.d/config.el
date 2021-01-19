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

;; (setq doom-font (font-spec :family "Source Code Pro" :size 24))
;; (setq doom-big-font (font-spec :family "Source Code Pro" :size 36))
(setq doom-font (font-spec :family "JetBrains Mono" :weight 'light ':size 24))
(setq doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 36))
(setq doom-variable-pitch-font (font-spec :family "Roboto" :size 24 :weight 'bold))

(require 'modus-themes)                 ; common code
(require 'modus-operandi-theme)         ; light theme
(require 'modus-vivendi-theme)          ; dark theme

(load-theme 'modus-vivendi t)           ; Dark theme
(load-theme 'modus-operandi t)          ; Light theme

(global-set-key (kbd "<f5>") (lambda () (interactive) (modus-themes-toggle) (set-face-background 'mode-line "default")))

;; Set customization options to values of your choice
(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-fringes 'intense ; {nil,'subtle,'intense}

      ;; Options for `modus-themes-lang-checkers': nil,
      ;; 'straight-underline, 'subtle-foreground,
      ;; 'subtle-foreground-straight-underline, 'intense-foreground,
      ;; 'intense-foreground-straight-underline, 'colored-background
      modus-themes-lang-checkers 'colored-background

      modus-themes-mode-line nil ; {nil,'3d,'moody}

      modus-themes-syntax nil ; Lots of options---continue reading the manual

      modus-themes-intense-hl-line t
      modus-themes-paren-match 'intense ; {nil,'subtle-bold,'intense,'intense-bold}

      modus-themes-links nil ; Lots of options---continue reading the manual
      modus-themes-no-mixed-fonts nil
      modus-themes-prompts 'subtle ; {nil,'subtle,'intense}
      modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
      modus-themes-region 'no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
      modus-themes-diffs nil ; {nil,'desaturated,'fg-only,'bg-only}
      modus-themes-org-blocks 'grayscale ; {nil,'grayscale,'rainbow}

      modus-themes-headings ; Lots of options---continue reading the manual
      '((1 . rainbow-section)
        ;; (2 . rainbow-line-no-bold)
        ;; (3 . no-bold)
        (t . rainbow-line))
      modus-themes-variable-pitch-headings nil
      modus-themes-scale-headings nil
      modus-themes-scale-1 1.1
      modus-themes-scale-2 1.15
      modus-themes-scale-3 1.21
      modus-themes-scale-4 1.27
      modus-themes-scale-5 1.33)

;; with Emacs 28, default seems to have a gray background for everything, this turns that to white
(setq modus-themes-operandi-color-overrides
      '(
        (bg-alt . "#ffffff")
        ))
(setq modus-themes-vivendi-color-overrides
      '(
        (bg-alt . "#000000")
        ))


;; Load the light theme (`modus-operandi')
(modus-themes-load-operandi)

;; Or load via a hook
;; (add-hook! 'after-init-hook #'modus-themes-load-operandi)

;includes part of the file's directory name at the beginning of the shared buffer name to make unique
(setq uniquify-buffer-name-style 'forward)
;; this may do the same thing as uniquify-buffer...
(setq ivy-rich-path-style 'abbrev)

; just editted these line 12/24
;; idk what these 2 lines do
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
;; (set-face-attribute 'default t :font "Source Code Pro-10")

                                        ; CAUTION
                                        ; This might be fatal, might turn off all keymaps
                                        ; (setq display-battery-mode t)

;; (setq display-time-mode t)
;; (setq display-time-default-load-average nil)
(setq line-number-mode nil)
(setq column-number-mode nil)
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

; Bind Zooms??
(map! :n "C-_" #'er/contract-region
      :n "C-+" #'er/expand-region)

;; ; unbind J,K,M
(map! :map evil-normal-state-map "J" nil
      "K" nil)
(map! :map evil-motion-state-map "M" nil
      "K" nil)

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

(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

;; Bind toggles
(global-set-key (kbd "<f2>") 'mixed-pitch-mode)
(global-set-key (kbd "<f3>") 'olivetti-mode)
(global-set-key (kbd "<f4>") 'toggle-rot13-mode)
(setq olivetti-body-width 90)
; ;; (global-set-key (kbd "U") 'undo-tree-redo)

; Unbind language input switcher
(map! :map global-map "C-\\" nil)
; Bind toggle for 80-char limit, buffer-wide
(map! :n "SPC t c" 'display-fill-column-indicator-mode)
(map! :n "C-\\" 'display-fill-column-indicator-mode)

;; ; currently do not use org-roam, need to delete
;; (setq org-roam-directory "~/emacs/org-roam")
;; (setq org-roam-index-file "index.org")
;(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
;(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
;(define-key org-roam-mode-map (kbd "C-c n j") #'org-roam-jump-to-index)
;(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
;(define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-graph)
;(define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
;(require 'org-roam-protocol)

(setq global-undo-tree-mode t)

(setq org-directory "~/org/")
(setq display-line-numbers-type 'relative)

(setq org-ellipsis " ▾")
(setq org-startup-folded 'content)

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-superstar-headline-bullets-list
      '("✸" ("◉" ?◈) "○" "▷"))

(map! :n "SPC o l" 'link-hint-open-link-at-point)

;; seems to break doom config
;; (require 'org-inlinetask')

;; https://www.reddit.com/r/orgmode/comments/6q6cdk/adding_files_to_the_agenda_list_recursively/
;; doom doctor: org-agenda-file-regexp seems to be void
;; (setq org-agenda-files (apply 'append
;;                   (mapcar
;;                    (lambda (directory)
;;                  (directory-files-recursively
;;                   directory org-agenda-file-regexp))
;;                    '("~/School/W21/" "~/org/"))))

;; Need to manually update based on school term
(setq org-agenda-files '("~/org" "~/School/W21/MAE_157_Light" "~/School/W21/MSE_141_Nano" "~/School/W21/MSE_175_Fail" "~/School/W21/MSE_189B_Snr" "~/School/W21/MSE_60_Synth" ))


(setq org-tag-faces
      '(("Synth" . "gold2") ("Nano" . "lime green") ("Light" . "red2")
        ("Snr" . "medium orchid") ("Fail" . "dodger blue")))

(setq org-agenda-start-day "+0")
(setq org-agenda-span 'week)

(setq org-agenda-timegrid-use-ampm t)
(setq org-agenda-time-grid
      (quote
       ((daily today require-timed)
        (400 1200 1600 2000 2400)
        "  ⟿" "―――――――――――――――――――――――")))
; 2400 is the next day

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

; this doesn't execute - jan 1st
(setq org-agenda-custom-commands
      '(("z" "Super View"
         (
          ;; (agenda "" ((org-super-agenda-groups
          ;;              '((:name ""
          ;;                       :time-grid t
          ;;                       :date today
          ;;                       :deadline today
          ;;                       ;; :scheduled today
          ;;                       :order 0
          ;;                       :discard (:anything t)
          ;;                       )))))
          (alltodo "" ((org-agenda-overriding-header (concat
                       (make-string 5 ?\n)
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
    (org-agenda nil "t")))

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
_d_: deadline   _e_: everyday   _w_: WHAT
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
      '((sequence "TODO(t)" "NEXT(n)" "NOTE(m)" "STRT(s)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
 (sequence "[ ](T)" "[+](P)" "[-](S)" "[?](W)" "|" "[X](D)")))

(setq org-todo-keyword-faces
'(("[-]" . +org-todo-active)
 ("STRT" . +org-todo-active)
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

(use-package! org-krita
  :config
  (add-hook 'org-mode-hook 'org-krita-mode))

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

; Rust
;; (setq lsp-rust-server 'rust-analyzer)
(map! :n "SPC t u" #'lsp-ui-doc-mode)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(after! lsp-rust
  (setq lsp-rust-server 'rust-analyzer))

;; (setq lsp-disabled-clients '(rls))

(after! persp-mode
(setq persp-emacsclient-init-frame-behaviour-override "main"))

;; (setq initial-buffer-choice t)

;; (setq desktop-auto-save-timeout 300)
;; (setq desktop-dirname "~/.emacs.d/.local/etc")
;; (setq desktop-base-file-name "desktop")
;; (setq desktop-load-locked-desktop t)
;; (desktop-save-mode 1)
;; (add-hook 'server-after-make-frame-hook 'desktop-read)

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
;;   t))
