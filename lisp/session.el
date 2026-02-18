;;; session.el --- Session persistence configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Session persistence including autorevert, recentf, savehist, and saveplace.

;;; Code:

(use-package autorevert
  :ensure nil
  :diminish
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook (elpaca-after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook (elpaca-after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  ;; Cleanup depth -90 ensures recentf-cleanup runs before recentf-save-list
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring register-alist mark-ring global-mark-ring
               search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook (elpaca-after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package tab-bar
  :ensure nil
  :hook (elpaca-after-init . tab-bar-mode)
  :general
  ("M-[" #'tab-bar-switch-to-prev-tab
   "M-]" #'tab-bar-switch-to-next-tab
   "C-x t t" #'tab-bar-switch-to-tab)
  :custom
  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil))

(use-package project
  :ensure nil
  :custom
  (project-switch-use-entire-map nil)
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-dir "Find dir")
     (project-dired "Dired")
     (magit-project-status "Magit" ?v)
     (project-eshell "Eshell")
     (project-shell "Shell")
     (project-find-regexp "Grep"))))

(use-package easysession
  :ensure t
  :diminish easysession-save-mode
  :custom
  (easysession-save-interval (* 10 60))
  :init
  (add-hook 'elpaca-after-init-hook #'easysession-save-mode 103)
  :config
  (with-eval-after-load 'easysession
    (require 'easysession-scratch)
    (require 'easysession-magit)
    (easysession-scratch-mode 1)
    (easysession-magit-mode 1)))

;;; session.el ends here
