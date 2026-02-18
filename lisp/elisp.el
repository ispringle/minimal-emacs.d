;;; elisp.el --- Emacs Lisp development tools -*- lexical-binding: t; -*-
;;; Commentary:
;; Development tools for Emacs Lisp including aggressive-indent, paredit,
;; highlight-defined, and elisp-refs.

;;; Code:

(use-package aggressive-indent
  :ensure t
  :diminish
  :commands aggressive-indent-mode
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package lispy
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode common-lisp-mode) . lispy-mode)
  :custom
  (lispy-close-quotes-at-end-p t))

(use-package paredit
  :disabled
  :ensure t
  :commands paredit-mode
  :hook (emacs-lisp-mode . paredit-mode)
  :general
  (:keymaps 'paredit-mode-map
            "RET" nil))

(use-package page-break-lines
  :ensure t
  :diminish
  :commands (page-break-lines-mode global-page-break-lines-mode)
  :hook (emacs-lisp-mode . page-break-lines-mode))

(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function elisp-refs-macro elisp-refs-variable
                                 elisp-refs-special elisp-refs-symbol))

(use-package whitespace
  :ensure nil
  :diminish)

;; Line length enforcement
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq fill-column 80)
            (setq whitespace-line-column 80)
            (setq whitespace-style '(face lines-tail))
            (whitespace-mode 1)))

(use-package package-lint
  :defer t
  :commands (package-lint-current-buffer package-lint-buffer))

;;; elisp.el ends here
