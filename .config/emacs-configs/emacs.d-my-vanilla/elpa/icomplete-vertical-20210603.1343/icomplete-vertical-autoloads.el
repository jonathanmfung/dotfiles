;;; icomplete-vertical-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "icomplete-vertical" "icomplete-vertical.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from icomplete-vertical.el

(defvar icomplete-vertical-mode nil "\
Non-nil if Icomplete-Vertical mode is enabled.
See the `icomplete-vertical-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `icomplete-vertical-mode'.")

(custom-autoload 'icomplete-vertical-mode "icomplete-vertical" nil)

(autoload 'icomplete-vertical-mode "icomplete-vertical" "\
Display icomplete candidates vertically.

If called interactively, toggle `Icomplete-Vertical mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'icomplete-vertical-toggle "icomplete-vertical" "\
Toggle Icomplete Vertical mode without echo area message." t nil)

(register-definition-prefixes "icomplete-vertical" '("icomplete-vertical-"))

;;;***

;;;### (autoloads nil nil ("icomplete-vertical-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; icomplete-vertical-autoloads.el ends here
