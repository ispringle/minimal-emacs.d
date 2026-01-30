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
  :config
  (setq jira-base-url "https://banno-jha.atlassian.net/"
        jira-token-is-personal-access-token nil
        jira-api-version 3))

;;; jira.el ends here
