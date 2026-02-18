;;; lsp.el --- LSP configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Language Server Protocol configuration using Eglot.

;;; Code:

(use-package eglot
  :ensure nil
  :commands (eglot-ensure eglot-rename eglot-format-buffer)
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  :config
  ;; Show all diagnostics in buffer
  (setq eglot-stay-out-of nil)

  ;; Enable flymake for visual indicators
  (add-hook 'eglot-managed-mode-hook #'flymake-mode))

(use-package eldoc
  :ensure nil
  :diminish
  :config
  ;; Gather full documentation from all sources
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; Allow multi-line in echo area initially (eldoc-box will intercept)
  (setq eldoc-echo-area-use-multiline-p t))

;; Better documentation display in a childframe
(use-package eldoc-box
  :ensure t
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode eldoc-box-help-at-point)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode) ;; Add this line
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-idle-delay 0.5)
  :general
  (:keymaps 'eglot-mode-map
            "C-c d" 'eldoc-box-help-at-point)
  :config
  (setq eldoc-box-only-multi-line t)
  (setq eldoc-box-cleanup-interval 0.2))

;; Flymake configuration for better visual feedback
(use-package flymake
  :ensure nil
  :diminish
  :custom
  ;; Show diagnostics in fringe
  (flymake-fringe-indicator-position 'left-fringe)
  ;; More visible error/warning faces
  (flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error))
  (flymake-warning-bitmap '(exclamation-mark compilation-warning))
  (flymake-note-bitmap '(exclamation-mark compilation-info)))

;;; lsp.el ends here
