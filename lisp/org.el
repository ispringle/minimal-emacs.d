;;; org.el --- Org mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal org-mode configuration.
;; Uses latest org from ELPA, not the built-in version.

;;; Code:

;; Install org before it's loaded to ensure we use the latest version
;; Using Elpaca's org menu to get the latest from org ELPA
(use-package org
  :ensure (org :host nil
               :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
               :build (:not elpaca--compile-info)
               :pre-build (with-temp-buffer
                            (call-process "make" nil t t "autoloads"))
               :files (:defaults "etc"))
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-startup-folded t)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  :config
  (require 'org-id)
  (require 'org-attach-git)
  ;; Load all org config files from lisp/org/ directory
  (let* ((org-config-dir (expand-file-name
                          "org" (expand-file-name
                                 "lisp" minimal-emacs-user-directory)))
         (org-files (when (file-directory-p org-config-dir)
                      (directory-files org-config-dir t "^[^#].*\\.el$"))))
    (when org-files
      (mapc #'load org-files))))

;;; org.el ends here
