;;; server.el --- Emacs server configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for the built-in Emacs server to enable emacsclient usage.

;;; Code:

(use-package server
  :ensure nil
  :commands server-start
  :hook (elpaca-after-init . server-start))

;;; server.el ends here
