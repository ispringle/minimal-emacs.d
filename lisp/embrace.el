;;; embrace.el --- Embrace configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for embrace - add/change/delete surrounding pairs.

;;; Code:

(use-package embrace
  :ensure t
  :commands (embrace-commander embrace-add embrace-delete embrace-change)
  :general
  ("C-c s" 'embrace-commander
   "C-c C-s a" 'embrace-add
   "C-c C-s d" 'embrace-delete
   "C-c C-s c" 'embrace-change))

;;; embrace.el ends here
