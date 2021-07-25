;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)


;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)



;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
(package! treemacs-evil)
(package! org-roam)
(package! modus-themes)
(package! mixed-pitch)
(package! olivetti)
(package! org-ql)
(package! cdlatex)
(package! org-superstar)
(package! bufler)

(package! helm-org-rifle)

;; look at undo-fu ? in doom-emacs with soon update (- jan 16, 21)
(package! undo-tree)

(package! vterm)

(package! jupyter)
(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))

(package! lispy)
(package! burly)

;; doom upgrade required to update even github packages
;; It works to: delete line and sync then repaste and sync
;; to circumvent upgrading all of doom and packages
(package! MetAPI
  :recipe (:host github :repo "jonathanmfung/MetAPI.el"))



;; (package! evil-terminal-cursor-changer)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))
;
;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
