;;; perspective.el --- perspective.el config -*- lexical-binding: t; -*-

;;; Commentary:
;; Basically workspaces.

;;; Code:

(use-package perspective
  :general
  ("C-x k" #'persp-kill-buffer*)
  :init
  (setq persp-mode-prefix-key (kbd "C-z"))
  :custom
  (persp-sort 'created)
  (persp-state-default-file
   (expand-file-name "persp-state" minimal-emacs-user-directory))
  :hook
  (elpaca-after-init . persp-mode)
  (kill-emacs . persp-state-save)
  :config
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))

;;; perspective.el ends here
