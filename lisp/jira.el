;;; jira.el --- JIRA integration -*- lexical-binding: t; -*-

;; Author:
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL:

;;; Commentary:

;; JIRA integration for Emacs

;;; Code:

(use-package jira
  :defer t
  :custom
  (jira-base-url "https://banno-jha.atlassian.net")
  (jira-token-is-personal-access-token nil)
  (jira-api-version 3))

;; Jira ticket completion for Magit branch creation and commit messages
(use-package jira-magit
  :load-path (lambda ()
               (expand-file-name "lib" minimal-emacs-lisp-directory))
  :defer t
  :config
  (jira-magit-setup)                     ; Enable branch name completion (C-c j)
  (jira-magit-enable-commit-completion)) ; Enable commit message completion (C-i)

(provide 'jira)
;;; jira.el ends here
