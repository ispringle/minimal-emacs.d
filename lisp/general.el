;;; general.el --- General keybinding framework -*- lexical-binding: t; -*-
;;; Commentary:
;; General provides a more convenient method for binding keys in Emacs.

;;; Code:

(use-package general
  :ensure t
  :demand t
  :general
  ("C-i" 'completion-at-point)
  :config
  (general-override-mode 1)
  (setq general-auto-unbind-keys t))

;;; general.el ends here
