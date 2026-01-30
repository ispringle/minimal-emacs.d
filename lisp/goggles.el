;;; goggles.el --- Goggles configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for goggles - visual feedback for operations.

;;; Code:

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :custom
  (goggles-pulse t))

;;; goggles.el ends here
