;;; elisp.el --- Emacs Lisp development tools -*- lexical-binding: t; -*-
;;; Commentary:
;; Development tools for Emacs Lisp including aggressive-indent, paredit,
;; highlight-defined, and elisp-refs.

;;; Code:

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode
  :hook (emacs-lisp-mode . paredit-mode)
  :general
  (:keymaps 'paredit-mode-map
   "RET" nil))

(use-package page-break-lines
  :ensure t
  :commands (page-break-lines-mode global-page-break-lines-mode)
  :hook (emacs-lisp-mode . page-break-lines-mode))

(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function elisp-refs-macro elisp-refs-variable
             elisp-refs-special elisp-refs-symbol))

;; Line length enforcement
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq fill-column 80)
            (setq whitespace-line-column 80)
            (setq whitespace-style '(face lines-tail))
            (whitespace-mode 1)))

;;; elisp.el ends here
