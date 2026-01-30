;;; compile.el --- Native compilation setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Native compilation setup using compile-angel for better performance.

;;; Code:

;; MUST load before all other packages
(use-package compile-angel
  :demand t
  :ensure t
  :diminish compile-angel-on-load-mode
  :custom
  (compile-angel-verbose t)
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

;;; compile.el ends here
