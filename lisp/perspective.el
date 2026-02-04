;;; perspective.el --- perspective.el config -*- lexical-binding: t; -*-

;;; Commentary:
;; Basically workspaces.

;;; Code:

(use-package perspective
  :general
  ("C-x k" #'persp-kill-buffer*)
  :init
  (setq persp-mode-prefix-key (kbd "C-z"))
  :custom
  (persp-sort 'oldest)
  (persp-state-default-file
   (expand-file-name "persp-state" minimal-emacs-user-directory))
  :hook
  (elpaca-after-init . persp-mode)
  (kill-emacs . persp-state-save)
  :config
  (defun persp-names ()
    "Return a list of the names of all perspectives on the `selected-frame'."
    (let ((persps (hash-table-values (perspectives-hash))))
      (cond ((eq persp-sort 'name)
             (sort (mapcar 'persp-name persps) 'string<))
            ((eq persp-sort 'access)
             (mapcar 'persp-name
                     (sort persps
                           (lambda (a b)
                             (time-less-p (persp-last-switch-time b)
                                          (persp-last-switch-time a))))))
            ((eq persp-sort 'created)
             (mapcar 'persp-name
                     (sort persps
                           (lambda (a b)
                             (time-less-p (persp-created-time b)
                                          (persp-created-time a))))))
            ((eq persp-sort 'oldest)
             (mapcar 'persp-name
                     (sort persps
                           (lambda (a b)
                             (time-less-p (persp-created-time a)
                                          (persp-created-time b)))))))))
  (with-eval-after-load 'consult
    (consult-customize consult-source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))

;;; perspective.el ends here
