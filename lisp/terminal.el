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
  :config
  ;; Compile terminfo for proper terminal emulation
  (eat-compile-terminfo)

  ;; Configure popup behavior for eat
  (add-to-list 'display-buffer-alist
               '("\\*eat\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.33)
                 (reusable-frames . visible))))

(defun eat-popup ()
  "Open eat in a popup at the bottom."
  (interactive)
  (let ((eat-buffer (get-buffer "*eat*")))
    (if (and eat-buffer (get-buffer-window eat-buffer))
        ;; If eat is visible, hide it
        (delete-window (get-buffer-window eat-buffer))
      ;; Otherwise, open it
      (eat))))

(defun eat-here ()
  "Open eat in the current window."
  (interactive)
  (let ((display-buffer-alist nil))  ; Ignore popup configuration
    (eat)))

(general-define-key
 :prefix "C-c o"
 "T" 'eat-here
 "t" 'eat-popup)

;;; terminal.el ends here
