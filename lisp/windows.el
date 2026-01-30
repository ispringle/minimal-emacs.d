;;; windows.el --- Window management configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Window switching and management.

;;; Code:

(use-package posframe
  :ensure t)

(use-package ace-window
  :ensure t
  :after posframe
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))
  (aw-scope 'frame)
  (aw-background t)
  :custom-face
  (aw-leading-char-face ((t (:height 500 :foreground "red"))))
  :config
  (posframe-delete-all)
  (ace-window-posframe-mode 1)
  :general
  ("C-x o" #'ace-window))

;;; windows.el ends here
