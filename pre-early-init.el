;;; pre-early-init.el --- Pre-early initialization configuration -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; This file runs before early-init.el.
;; Contains basic configuration and directory setup for minimal-emacs.d.

;;; Code:

(setq debug-on-error t)

;; Add Homebrew and local bin to exec-path
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time 100)

(setq minimal-emacs-user-directory "~/.config/minimal-emacs.d/")

;; Redirect clutter to var/
(setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Elpaca replaces the built-in package manager
(setq minimal-emacs-package-initialize-and-refresh nil)

;;; pre-early-init.el ends here
