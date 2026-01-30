;;; hydra.el --- Hydra config -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; Hydra provides popup menus for related commands.
;; These Hydras are found in the hydra/ subdir, each has a hydr def., and two
;; autoloads, one for the hydra def and one for that def's keybinding.

;;; Code:

(use-package hydra
  :ensure t
  :defer t)

(let ((hydra-dir (expand-file-name "lisp/hydra" minimal-emacs-user-directory))
      (autoloads-file (expand-file-name
                       "lisp/hydra/hydra-autoloads.el"
                       minimal-emacs-user-directory)))
  (when (or (not (file-exists-p autoloads-file))
            (cl-some (lambda (f) (file-newer-than-file-p f autoloads-file))
                     (directory-files hydra-dir t "\\.el$")))
    (loaddefs-generate hydra-dir autoloads-file))
  (load autoloads-file t t))
;;; hydra.el ends here
