;;; comment.el --- DWIM comment util -*- lexical-binding: t; -*-

;;; Commentary:
;; A DWIM comment super utility.
;; M-;       → toggle current line
;; M-;       → toggle region (when active)
;; C-u 5 M-; → toggle 5 lines
;; C-u M-;   → original comment-dwim (append comment)

;;; Code:

(defun comment-dwim-line-or-region (arg)
  "Toggle comment on line or region.
With numeric prefix ARG, operate on ARG lines.
With universal prefix \\[universal-argument], behave like `comment-dwim'."
  (interactive "P")
  (cond
   ((consp arg)
    (comment-dwim nil))
   ((use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end)))
   ((save-excursion
      (beginning-of-line)
      (looking-at-p "\\s-*$"))
    (comment-dwim nil))
   (t
    (let ((n (or arg 1)))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position n))))))

(general-define-key "M-;" #'comment-dwim-line-or-region)

;;; comment.el ends here
