#!/usr/bin/env -S emacs --script
;;; gen-env-file.el --- Loads envvars into Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; This generates a file of env vars for loading.

;;; Code:

(defun gen-env-file (path)
  "Save envvars to a file at path"
  (let ((dirname (file-name-directory path)))
    (make-directory dirname t))
  (with-temp-file path
    (setq-local coding-system-to-write 'utf-8-unix)
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; This file was automatically genereated and will be overwritten.\n")
    (insert (pp-to-string process-environment))))

(gen-env-file "~/.config/minimal-emacs.d/local/env.el")

;;; gen-env-file.el ends here
