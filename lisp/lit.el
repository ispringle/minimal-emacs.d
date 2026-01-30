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

;; Use polymode for multi-language highlighting in template literals
(use-package polymode
  :ensure t
  :config
  (setq polymode-prefix-key (kbd "C-c m"))

  ;; Use non-tree-sitter modes as host (polymode works better with them)
  (define-hostmode poly-lit-typescript-hostmode
    :mode 'typescript-mode)

  (define-hostmode poly-lit-javascript-hostmode
    :mode 'js-mode)

  ;; Define HTML template literal as inner mode (use mhtml-mode for better highlighting)
  (define-innermode poly-lit-html-template-innermode
    :mode 'mhtml-mode
    :head-matcher "html\\s-*`"
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host
    :fallback-mode 'host
    :allow-nested t)

  ;; Define CSS template literal as inner mode
  (define-innermode poly-lit-css-template-innermode
    :mode 'css-mode
    :head-matcher "css\\s-*`"
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host
    :fallback-mode 'host)

  ;; Define JavaScript expressions inside HTML templates (${...})
  (define-innermode poly-lit-js-expression-innermode
    :mode 'js-mode
    :head-matcher "\\${[ \t\n]*"
    :tail-matcher "[ \t\n]*}"
    :head-mode 'host
    :tail-mode 'host
    :protect-font-lock nil
    :protect-syntax nil)

  ;; Define the polymode for TypeScript + Lit
  (define-polymode poly-lit-ts-mode
    :hostmode 'poly-lit-typescript-hostmode
    :innermodes '(poly-lit-html-template-innermode
                  poly-lit-css-template-innermode
                  poly-lit-js-expression-innermode)
    :lighter " PolyLit")

  ;; Define the polymode for JavaScript + Lit
  (define-polymode poly-lit-js-mode
    :hostmode 'poly-lit-javascript-hostmode
    :innermodes '(poly-lit-html-template-innermode
                  poly-lit-css-template-innermode
                  poly-lit-js-expression-innermode)
    :lighter " PolyLit"))

;; Auto-activate for files with Lit imports
;; DISABLED: Testing eglot without polymode
;; (defun lit-maybe-enable-polymode ()
;;   "Enable poly-lit mode if file contains Lit imports."
;;   (when (and (buffer-file-name)
;;              (not (bound-and-true-p polymode-mode)))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (re-search-forward "from ['\"]lit['\"]\\|@lit/\\|import.*LitElement" nil t)
;;         (cond
;;          ((or (derived-mode-p 'typescript-ts-mode)
;;               (derived-mode-p 'typescript-mode))
;;           (poly-lit-ts-mode))
;;          ((or (derived-mode-p 'js-ts-mode)
;;               (derived-mode-p 'js-mode))
;;           (poly-lit-js-mode)))))))

;; (add-hook 'typescript-ts-mode-hook #'lit-maybe-enable-polymode)
;; (add-hook 'typescript-mode-hook #'lit-maybe-enable-polymode)
;; (add-hook 'js-ts-mode-hook #'lit-maybe-enable-polymode)
;; (add-hook 'js-mode-hook #'lit-maybe-enable-polymode)

;;; lit.el ends here
