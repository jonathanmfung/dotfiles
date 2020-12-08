(setq user-full-name  "Jonathan Fung"
      user-mail-address "jonathanfung2000@gmail.com")

;; (setq doom-font (font-spec :family "Source Code Pro" :height 120))
;; (setq doom-big-font (font-spec :family "Source Code Pro" :height 140))
;; (setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :height 100))
(setq doom-font (font-spec :family "Source Code Pro" :size 24))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 36))
(setq doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 36))
; size 30

; idk what this line does
; maybe turns on org mode for all .org files?? probably set when facing a bug
;; (setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))

; TODO: something about source code block fonts not being small

(defmacro modus-themes-format-sexp (sexp &rest objects)
  `(eval (read (format ,(format "%S" sexp) ,@objects))))

(load-theme 'modus-vivendi t)           ; Dark theme
(load-theme 'modus-operandi t)          ; Light theme

(dolist (theme '("operandi" "vivendi"))
  (modus-themes-format-sexp
   (defun modus-%1$s-theme-load ()
     (setq modus-%1$s-theme-bold-constructs t
           modus-%1$s-theme-slanted-constructs t
           modus-%1$s-theme-syntax nil ; {nil,faint,'yellow-comments,'green-strings,'yellow-comments-green-strings,'alt-syntax,'alt-syntax-yellow-comments}

           ; review
           modus-%1$s-theme-no-mixed-fonts nil

           modus-%1$s-theme-links nil ; {nil,'faint,'neutral-underline,'faint-neutral-underline,'no-underline}
           modus-%1$s-theme-prompts 'subtle ; {nil,'subtle,'intense}
           modus-%1$s-theme-mode-line nil ; {nil,'3d,'moody}

           ; review
           modus-%1$s-theme-completions 'opinionated ; {nil,'moderate,'opinionated}

           modus-%1$s-theme-fringes 'intense ; {nil,'subtle,'intense}
           modus-%1$s-theme-intense-hl-line t
           modus-%1$s-theme-intense-paren-match t
           modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
           modus-%1$s-theme-org-blocks 'grayscale ; {nil,'grayscale,'rainbow}
           modus-%1$s-theme-headings  ; Read further below in the manual for this one
            '((1 . rainbow-section)
              (2 . rainbow-line)
              (t . rainbow-line-no-bold))
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-scale-1 1.1
           modus-%1$s-theme-scale-2 1.15
           modus-%1$s-theme-scale-3 1.21
           modus-%1$s-theme-scale-4 1.27
           modus-%1$s-theme-scale-5 1.33)
           modus-%1$s-theme-variable-pitch-headings nil
     (load-theme 'modus-%1$s t))
   theme))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (modus-vivendi-theme-load)
        (doom/reset-font-size))
    (disable-theme 'modus-vivendi)
    (modus-operandi-theme-load)
    (doom/reset-font-size)))

(global-set-key (kbd "<f5>") 'modus-themes-toggle)

;includes part of the file's directory name at the beginning of the shared buffer name to make unique
(setq uniquify-buffer-name-style 'forward)
;; ; this may do the same thing as uniquify-buffer...
(setq ivy-rich-path-style 'abbrev)

;; ; idk what these 2 lines do
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
(set-face-attribute 'default t :font "Source Code Pro-10")

; CAUTION
; This might be fatal, might turn off all keymaps
;; (setq display-battery-mode t)
;; (setq display-time-mode t)
;; (setq display-time-default-load-average nil)
;; (setq doom-modeline-buffer-encoding nil)

(setq line-number-mode nil)
(setq column-number-mode nil)
(set-face-background 'mode-line "default")

(setq org-directory "~/org/")
(setq display-line-numbers-type 'relative)

(add-hook 'org-mode-hook 'pandoc-mode)
;; (add-hook 'after-save-hook #'pandoc-convert-to-pdf)

(setq org-agenda-files '("~/org/Agenda.org"))
(setq org-tag-faces
      '(("Poly" . "gold2") ("Cer" . "lime green") ("Xray" . "red2")
        ("Snr" . "medium orchid") ("Stat_112" . "dodger blue")))

(setq org-agenda-start-day "+0")

(org-super-agenda-mode)
(setq org-agenda-custom-commands
       '(("u" "Super view"
          ((agenda "" ((org-super-agenda-groups
                        '((:name "Next Items"
                           :time-grid t
                           :tag ("NEXT" "outbox"))
                          (:name "School"
                           :tag ("Poly" "Cer" "Xray" "Snr"))
                          (:name "Personal"
                           :tag "Person")
                          )))))
           (alltodo "" ((org-agenda-overriding-header "Projects")
                     (org-super-agenda-groups
                      '((:tag "Person")
                        (:discard (:anything t)))))))))
(setq org-agenda-custom-commands
      '(("z" "Super View"
         ((agenda "" ((org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          ;; (:name "Next to do"
                          ;;        :todo "NEXT"
                          ;;        :order 1)
                          ;; (:name "Important"
                          ;;        :tag "Important"
                          ;;        :priority "A"
                          ;;        :order 6)
                          ;; (:name "Due Today"
                          ;;        :deadline today
                          ;;        :order 2)
                          ;; (:name "Due Soon"
                          ;;        :deadline future
                          ;;        :order 8)
                          ;; (:name "Overdue"
                          ;;        :deadline past
                          ;;        :order 7)
                          (:name "Personal"
                                 :tag "Person"
                                 :order 10)
                          (:name "Email"
                                 :tag "Email"
                                 :order 15)
                          (:discard (:anything t))))))))))

(setq org-capture-templates
      '(("t" "Agenda TODO" entry (file "~/org/Agenda.org")
        "* TODO %?" :prepend t)
        ("e" "email" entry (file+headline "~/org/Agenda.org" "Emails")
         "* TODO Reply: %? \n - %a" :prepend t)
      ))

(add-to-list 'org-latex-classes
             '("notes"
                   "\\documentclass[8pt]{article}
\\usepackage[letterpaper, portrait, margin=1in]{geometry}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{enumitem} % for below
\\setitemize{itemsep=0.5pt} % adjusts vert space of (second?) itemize/bullet
\\usepackage{lastpage} %For getting page x of y
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\usepackage{titling} % allows \thetitle \theauthor \thedate
\\rhead{\\theauthor}
\\lhead{\\thetitle}
\\rfoot{\\thepage{} of \\pageref{LastPage}}
\\linespread{1}
\\setlength{\\parindent}{0pt}
\\hypersetup{pdfborder=0 0 0}
\\setcounter{secnumdepth}{0}"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")))

(map! :n "SPC r r" #'org-latex-export-to-pdf)

; Rust
(setq lsp-rust-server "rust-analyzer")
(map! :n "SPC t u" #'lsp-ui-doc-mode)

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

; unbind SPC p F
;(map! :map doom-leader-map "p F" nil)
; rebind SPC p F to search all projects' files
;(map! :n "SPC p F" #'projectile-find-file-in-known-projects)

;; (defun toggle-header-line-format ()
;;     "Toggle buffer-local var header-line-format as pseudo-top margin"
;;     (setq header-line-format (if (eq header-line-format nil) t nil))
;;     (interactive)
;;     (redraw-display))
;; (global-set-key (kbd "<f6>") 'toggle-header-line-format)
; use with set-face-font header-line
;(set-face-background 'header-line "white")

(map! :n "SPC o o" #'treemacs-visit-node-in-external-application)
(map! :n "SPC o t" #'treemacs)
(setq treemacs-position 'right
      treemacs-width 25
      treemacs-indentation 1)

;(map! :n "SPC r r" #'pandoc-convert-to-pdf)

;define function that syncs mbsync and refreshes notmuch
(defun sync-email ()
  "Lists the contents of the current directory."
  (interactive)
  (shell-command "mbsync -a && notmuch new"))

; bind notmuch-hello view
(map! :n "SPC o n" #'notmuch-hello)
; bind custom function to sync mbsync and notmuch
(map! :n "SPC r s" 'sync-email)

;; attempt to fix notmuch formatting
(setq notmuch-search-result-format
  '(("date" . "%12s ")
    ("count" . "%-6s ")
    ("authors" . "%-15s ")
    ("subject" . "%-10s ")
    ("tags" . "(%s)"))
)

(setq notmuch-saved-searches '((:name "Personal" :query "tag:inbox AND to:jonathanfung2000@gmail.com AND date:nov_3_2020..today AND NOT tag:delete")
                               (:name "UCI" :query "tag:inbox AND to:fungjm@uci.edu AND date:nov_3_2020..today AND NOT tag:delete")
                               (:name "Clean Inbox" :query "tag:inbox AND date:nov_3_2020..today")
                                   (:name "Flagged" :query "tag:inbox AND tag:flagged")
                               (:name "Inbox" :query "tag:inbox")))

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

;; enable elgantt - https://github.com/legalnonsense/elgantt/
;; (add-to-list 'load-path (concat user-emacs-directory "elgantt/")) ;; Or wherever it is located
;; (require 'elgantt)

(after! persp-mode
(setq persp-emacsclient-init-frame-behaviour-override "main"))

; (annotate-mode)

(setq hl-line-mode nil)
(map! :n "SPC t h" #'hl-line-mode)

; meant to only have hl-line highlight on end of line
(defun my-hl-line-range-function () (cons (line-end-position) (line-beginning-position 2)))
(setq hl-line-range-function #'my-hl-line-range-function)

;; Local Variables:
;; byte-compile-warnings: (not mapcar)
;; End:

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(set-email-account! "Personal"
  '((mu4e-sent-folder       . "/gmail/[Gmail].Sent Mail")
    ;(mu4e-drafts-folder     . "/gmail/Drafts")
    (mu4e-trash-folder      . "/gmail/[Gmail].Trash")
    (mu4e-refile-folder     . "/gmail/[Gmail].All Mail")
    (smtpmail-smtp-user     . "jonathanfung2000@gmail.com")
    ;; (mu4e-compose-signature . "---\nHenrik Lissner"))
  t))
(set-email-account! "UCI"
  '((mu4e-sent-folder       . "/uci/[Gmail].Sent Mail")
    ;(mu4e-drafts-folder     . "/gmail/Drafts")
    (mu4e-trash-folder      . "/uci/[Gmail].Trash")
    (mu4e-refile-folder     . "/uci/[Gmail].All Mail")
    (smtpmail-smtp-user     . "fungjm@uci.edu")
    ;; (mu4e-compose-signature . "---\nHenrik Lissner"))
  t))
