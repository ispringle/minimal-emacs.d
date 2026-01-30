;;; terminal.el --- Terminal emulator configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Terminal emulator using eat.

;;; Code:

(use-package eat
  :ensure (:type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :commands (eat eat-other-window eat-eshell-mode)
  :custom
  (eat-kill-buffer-on-exit t)
  :preface
  (defun eat-popup ()
    "Open eat in a popup at the bottom."
    (interactive)
    (let ((eat-buffer (get-buffer "*eat*")))
      (if (and eat-buffer (get-buffer-window eat-buffer))
          (delete-window (get-buffer-window eat-buffer))
        (eat))))
  (defun eat-here ()
    "Open eat in the current window."
    (interactive)
    (let ((display-buffer-alist nil))
      (eat)))
  :general
  (:prefix "C-c o"
           "T" #'eat-here
           "t" #'eat-popup)
  :config
  (eat-compile-terminfo)
  (add-to-list 'display-buffer-alist
               '("\\*eat\\*"
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.33)
                 (reusable-frames . visible))))

;;; terminal.el ends here
