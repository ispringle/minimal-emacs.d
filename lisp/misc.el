;;; misc.el --- Miscellaneous settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Miscellaneous settings including backup configuration and exit confirmation.

;;; Code:

(setq confirm-kill-emacs 'y-or-n-p)

(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)

(defun reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  (load-file "~/.emacs.d/init.el")
  (message "Configuration reloaded!"))

(defun check-emacs-path (&optional executable)
  "Show exec-path and optionally test finding EXECUTABLE.
If called interactively with prefix arg, prompt for executable name to test.
Otherwise shows common executables."
  (interactive
   (list (when current-prefix-arg
           (read-string "Executable to find: "))))
  (with-output-to-temp-buffer "*Emacs Path Debug*"
    (princ "=== Emacs exec-path ===\n")
    (dolist (path exec-path)
      (princ (format "%s\n" path)))
    (princ "\n=== Testing executables ===\n")
    (if executable
        ;; Test user-provided executable
        (princ (format "%s: %s\n" executable
                       (or (executable-find executable) "NOT FOUND")))
      ;; Test common executables
      (dolist (exe '("git" "node" "python" "python3" "npm" "gcc" "clang"))
        (princ (format "%-10s %s\n" (concat exe ":")
                       (or (executable-find exe) "NOT FOUND")))))))

(general-define-key
 "C-c r" 'reload-config)

;;; misc.el ends here
