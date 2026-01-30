;;; vc.el --- Version control configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Version control interface using magit and diff-hl.

;;; Code:

;; Magit - Git interface for Emacs
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  :general
  ("C-x g" 'magit-status
   "C-c g" 'magit-dispatch
   "C-c f g" 'magit-file-dispatch))

;; diff-hl - Highlight uncommitted changes in the fringe
(use-package diff-hl
  :ensure t
  :diminish
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode 1))

(use-package conventional-commit
  :load-path (lambda ()
               (expand-file-name "lib" minimal-emacs-lisp-directory))
  :defer t)

(defun isp/custom-ux-scopes ()
  "Generate scope list for the custom-ux repository."
  (let ((root (expand-file-name
               "~/git/hub/Banno/custom-ux/packages/"))
        (scopes '()))
    (when (file-directory-p root)
      (let ((custom-apps-dir (expand-file-name "custom-apps/" root)))
        (when (file-directory-p custom-apps-dir)
          (dolist (dir (directory-files custom-apps-dir nil "^[^.]"))
            (when (file-directory-p
                   (expand-file-name dir custom-apps-dir))
              (push (concat dir "-customapp") scopes)))))
      (let ((power-ons-dir (expand-file-name "power-ons/" root)))
        (when (file-directory-p power-ons-dir)
          (dolist (dir (directory-files power-ons-dir nil "^[^.]"))
            (when (file-directory-p
                   (expand-file-name dir power-ons-dir))
              (push (concat dir "-poweron") scopes)))))
      (let ((templates-dir (expand-file-name "templates/" root)))
        (when (file-directory-p templates-dir)
          (dolist (dir (directory-files templates-dir nil "^[^.]"))
            (when (file-directory-p
                   (expand-file-name dir templates-dir))
              (push (concat dir "-template") scopes)))))
      (let ((poweron-lib-dir (expand-file-name "poweron-lib/" root)))
        (when (file-directory-p poweron-lib-dir)
          (push "poweron-lib" scopes))))
    (nreverse scopes)))

(defun isp/conventional-commit-setup-with-scopes ()
  "Set up conventional commit with project-specific scopes."
  (require 'conventional-commit)
  (setq-local completion-at-point-functions '(conventional-commit-capf))
  (when-let* ((root (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                        (vc-root-dir)
                        default-directory))
              (_ (string-match-p "custom-ux" root)))
    (setq-local conventional-commit-scope-table (isp/custom-ux-scopes))))

(add-hook 'git-commit-mode-hook #'isp/conventional-commit-setup-with-scopes)

;;; vc.el ends here
