;;; windows.el --- Window management configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Window switching and management.

;;; Code:

(use-package ace-window
  :ensure t
  :commands ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ; Home row keys
  (aw-scope 'frame)  ; Only current frame
  (aw-background t)  ; Dim background when selecting
  :general
  ("C-x o" 'ace-window))

;;; windows.el ends here
