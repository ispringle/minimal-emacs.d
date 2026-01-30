;;; post-init.el --- Load configuration modules -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; This file loads all configuration modules from the lisp/ directory.

;;; Code:

(defvar minimal-emacs-lisp-directory
  (expand-file-name "lisp" minimal-emacs-user-directory)
  "Path to the lisp/ directory containing configuration modules.")

(let* ((lisp-dir minimal-emacs-lisp-directory)
       ;; Files that need to be loaded in a specific order
       (ordered-files '("compile.el" "general.el" "which-key.el"))
       ;; Load ordered files first
       (ordered-paths (mapcar (lambda (f) (expand-file-name f lisp-dir))
                              ordered-files))
       ;; Get all .el files in the directory
       (all-files (directory-files lisp-dir 't "^[^#].*\\.el$"))
       ;; Filter out the ordered files from the remaining files
       (remaining-files (seq-filter (lambda (f)
                                      (not (member f ordered-paths)))
                                    all-files)))
  ;; Load ordered files in sequence with elpaca-wait after each
  (dolist (file ordered-paths)
    (load file)
    (elpaca-wait))
  ;; Load remaining files
  (mapc #'load remaining-files))

;;; post-init.el ends here
