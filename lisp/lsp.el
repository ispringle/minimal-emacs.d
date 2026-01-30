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
  (add-hook 'eglot-managed-mode-hook #'flymake-mode)

  ;; Configure TypeScript server to use Lit plugins
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode)
                 . ("typescript-language-server" "--stdio"
                    :initializationOptions
                    (:plugins [(:name "typescript-lit-html-plugin")
                               (:name "ts-lit-plugin"
                                :tags ["html" "css"])]))))

  ;; Also configure for TSX if needed
  (add-to-list 'eglot-server-programs
               '((tsx-ts-mode)
                 . ("typescript-language-server" "--stdio"
                    :initializationOptions
                    (:plugins [(:name "typescript-lit-html-plugin")
                               (:name "ts-lit-plugin"
                                :tags ["html" "css"])])))))

;; Better documentation display in a childframe
(use-package eldoc-box
  :ensure t
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode eldoc-box-help-at-point)
  :custom
  (eldoc-box-clear-with-C-g t)  ; Clear box with C-g
  (eldoc-idle-delay 0.5)  ; Wait before showing documentation
  :general
  (:keymaps 'eglot-mode-map
   "C-c d" 'eldoc-box-help-at-point)  ; Show docs on demand
  :config
  ;; Hide box when typing or moving cursor
  (setq eldoc-box-only-multi-line t)  ; Only show for multi-line docs
  (setq eldoc-box-cleanup-interval 0.2))  ; Hide quickly when moving

;; Flymake configuration for better visual feedback
(use-package flymake
  :ensure nil
  :custom
  ;; Show diagnostics in fringe
  (flymake-fringe-indicator-position 'left-fringe)
  ;; More visible error/warning faces
  (flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error))
  (flymake-warning-bitmap '(exclamation-mark compilation-warning))
  (flymake-note-bitmap '(exclamation-mark compilation-info)))

;;; lsp.el ends here
