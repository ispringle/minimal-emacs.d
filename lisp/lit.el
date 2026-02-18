;;; lit.el --- Lit web components support -*- lexical-binding: t; -*-
;;; Commentary:
;; Syntax highlighting for html`` and css`` tagged template literals in Lit.

;;; Code:

;; Set indentation to 2 spaces for all relevant modes
(setq typescript-ts-mode-indent-offset 2)
(setq typescript-indent-level 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq sgml-basic-offset 2)

(use-package template-literals-ts-mode
  :ensure t
  :hook ((js-ts-mode . template-literals-ts-mode)
         (typescript-ts-mode . template-literals-ts-mode)))

(use-package lit-ts-mode
  :ensure (:host github :repo "ispringle/lit-ts-mode"))

;;; lit.el ends here
