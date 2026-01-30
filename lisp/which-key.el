;;; which-key.el --- Which-key configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; which-key displays available keybindings in a popup.

;;; Code:

(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :commands which-key-mode
  :hook (elpaca-after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;;; which-key.el ends here
