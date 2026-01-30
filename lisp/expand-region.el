;;; expand-region.el --- Expand region configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for expand-region.

;;; Code:

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :general
  ("C-," 'er/expand-region))

;;; expand-region.el ends here
