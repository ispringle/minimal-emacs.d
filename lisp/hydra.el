;;; hydra.el --- Hydra configuration -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; Hydra provides popup menus for related commands.

;;; Code:

(use-package hydra
  :ensure t
  :demand t
  :config
  ;; Load all hydra definitions
  (let* ((hydra-dir (expand-file-name "lisp/hydra" minimal-emacs-user-directory))
         (hydra-files (directory-files hydra-dir 't "^[^#].*\\.el$")))
    (mapc #'load hydra-files)))

;;; hydra.el ends here
