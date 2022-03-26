;;; mixed-pitch-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mixed-pitch" "mixed-pitch.el" (0 0 0 0))
;;; Generated autoloads from mixed-pitch.el

(autoload 'mixed-pitch-mode "mixed-pitch" "\
Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.

This is a minor mode.  If called interactively, toggle the
`Mixed-Pitch mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `mixed-pitch-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

See the variable `mixed-pitch-fixed-pitch-faces' for a list of
which faces remain fixed pitch. The height and pitch of faces is
inherited from `variable-pitch' and `default'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "mixed-pitch" '("mixed-pitch-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mixed-pitch-autoloads.el ends here
