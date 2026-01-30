;;; vc.el --- Version control configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Version control interface using magit and diff-hl.

;;; Code:

;; Ensure transient is up to date (required by magit and claude-code)
(use-package transient
  :ensure t
  :demand t)

;; Magit - Git interface for Emacs
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :general
  ("C-x g" 'magit-status
   "C-c g" 'magit-dispatch
   "C-c f g" 'magit-file-dispatch))

;; diff-hl - Highlight uncommitted changes in the fringe
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode global-diff-hl-mode diff-hl-magit-post-refresh)
  :hook
  ((prog-mode . diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

;;; vc.el ends here
