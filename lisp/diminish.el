;;; diminish.el --- Clean up the modeline -*- lexical-binding: t; -*-
;;; Commentary:
;; Hide or shorten minor mode indicators in the modeline.

;;; Code:

(use-package diminish
  :ensure t
  :demand t
  :config
  ;; Diminish common minor modes
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  (diminish 'flycheck-mode)
  (diminish 'flymake-mode)
  (diminish 'which-key-mode)
  (diminish 'company-mode)
  (diminish 'yas-minor-mode)
  (diminish 'aggressive-indent-mode)
  (diminish 'paredit-mode)
  (diminish 'buffer-terminator-mode)
  (diminish 'inhibit-mouse-mode)
  (diminish 'apheleia-mode)
  (diminish 'diff-hl-mode))

;;; diminish.el ends here
