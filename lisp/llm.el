;;; llm.el --- LLM integration configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Claude AI integration using claude-code.el and monet.

;;; Code:

;; Ensure transient is up to date (required by claude-code)
(use-package transient
  :ensure t
  :demand t)

;; Monet - Syntax highlighting for code blocks
(use-package monet
  :ensure (:host github :repo "stevemolitor/monet"))

;; Claude Code - Emacs interface for Claude AI
(use-package claude-code
  :ensure (:host github :repo "stevemolitor/claude-code.el")
  :after monet
  :custom
  ;; Use wrapper script to access jh-code claude
  (claude-code-program "claude-wrapper")

  ;; Use eat for terminal backend
  ;; (claude-code-terminal-backend 'eat)

  :config
  ;; IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)

  :bind-keymap
  ("C-c c" . claude-code-command-map)

  ;; Repeat map to cycle through auto-accept/plan/confirm modes with M
  :bind
  (:repeat-map my-claude-code-map
   ("M" . claude-code-cycle-mode)))

;;; llm.el ends here
