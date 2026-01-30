;;; post-init.el --- Load configuration modules -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; This file loads all configuration modules from the lisp/ directory.

;;; Code:

(defvar minimal-emacs-lisp-directory
  (expand-file-name "lisp" minimal-emacs-user-directory)
  "Path to the lisp/ directory containing configuration modules.")

(let* ((lisp-dir minimal-emacs-lisp-directory)
       (ordered-files '("compile.el" "general.el" "which-key.el"))
       (ignored-files '("llm.el"))
       (whitelist-files '())  ; If non-empty, ONLY load these
       (ordered-paths (mapcar (lambda (f) (expand-file-name f lisp-dir))
                              ordered-files))
       (ignored-paths (mapcar (lambda (f) (expand-file-name f lisp-dir))
                              ignored-files))
       (whitelist-paths (mapcar (lambda (f) (expand-file-name f lisp-dir))
                                whitelist-files))
       (all-files (directory-files lisp-dir 't "^[^#].*\\.el$"))
       (remaining-files (if whitelist-paths
                            (seq-filter (lambda (f)
                                          (and (member f whitelist-paths)
                                               (not (member f ordered-paths))))
                                        all-files)
                          (seq-filter (lambda (f)
                                        (not (or (member f ordered-paths)
                                                 (member f ignored-paths))))
                                      all-files))))
  (dolist (file ordered-paths)
    (unless (and whitelist-paths
                 (not (member file whitelist-paths)))
      (load file)
      (elpaca-wait)))
  (dolist (file remaining-files)
    (load file)))
;;; post-init.el ends here
