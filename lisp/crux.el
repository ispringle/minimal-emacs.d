;;; crux.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package crux
  :ensure t
  :general
  ("C-k" #'crux-smart-kill-line)
  ("C-S-k" #'crux-kill-whole-line)
  ("C-a" #'crux-move-beginning-of-line)
  ("C-o" #'crux-smart-open-line)
  ("C-S-o" #'crux-smart-open-line-above)
  ("C-c d" #'crux-duplicate-current-line-or-region)
  ("C-c D" #'crux-duplicate-and-comment-current-line-or-region)
  ("s-<backspace>" #'crux-kill-line-backwards)
  ("C-c R" #'crux-rename-file-and-buffer)
  ("C-c k" #'crux-kill-other-buffers)
  ("C-c TAB" #'crux-indent-rigidly-and-copy-to-clipboard)
  ("C-^" #'crux-top-join-line)
  ("C-c b" #'crux-switch-to-previous-buffer))

;;;.el ends here
