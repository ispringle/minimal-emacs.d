;;; editing.el --- General editing configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; General editing tools including undo, markdown, treesit, avy, apheleia,
;; yasnippet, and helpful.

;;; Code:

;; Symbol highlighting
(require 'hi-lock)

;; Stolen from:
;; https://www.jamescherti.com/emacs-symbol-highlighting-built-in-functions/
(defun simple-toggle-highlight-symbol-at-point ()
  "Toggle highlighting for the symbol at point."
  (interactive)
  (when-let* ((regexp (find-tag-default-as-symbol-regexp)))
    (if (member regexp (hi-lock--regexps-at-point))
        ;; Unhighlight symbol at point
        (hi-lock-unface-buffer regexp)
      ;; Highlight symbol at point
      (hi-lock-face-symbol-at-point))))

(general-define-key
 "s-s" 'simple-toggle-highlight-symbol-at-point)

;; Display fill column indicator
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo undo-fu-only-redo
                               undo-fu-only-redo-all undo-fu-disable-checkpoint)
  :general
  ("C-z" 'undo-fu-only-undo
   "C-S-z" 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (elpaca-after-init . undo-fu-session-global-mode))

(use-package markdown-mode
  :commands (gfm-mode gfm-view-mode markdown-mode markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :general
  (:keymaps 'markdown-mode-map
            "C-c C-e" 'markdown-do))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package buffer-terminator
  :ensure t
  :diminish buffer-terminator-mode
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 30 60))
  (buffer-terminator-interval (* 10 60))
  :config
  (buffer-terminator-mode 1))

(use-package inhibit-mouse
  :ensure t
  :diminish inhibit-mouse-mode
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode)
    (inhibit-mouse-mode 1)))

(use-package avy
  :ensure t
  :commands (avy-goto-char avy-goto-char-2 avy-next)
  :general
  ("C-'" 'avy-goto-char-2))

(use-package apheleia
  :ensure t
  :commands (apheleia-mode apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-global-mode)
  :hook (elpaca-after-init . yas-global-mode)
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)
  (yas-wrap-around-region nil)
  :init
  (setq yas-verbosity 0))

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-key
                              helpful-command helpful-at-point helpful-function)
  :general
  ([remap describe-command] 'helpful-command
   [remap describe-function] 'helpful-callable
   [remap describe-key] 'helpful-key
   [remap describe-symbol] 'helpful-symbol
   [remap describe-variable] 'helpful-variable)
  :custom
  (helpful-max-buffers 7))

;;; editing.el ends here
