;;; lisp/conventional-commit.el --- Completion for conventional commits -*- lexical-binding: t; -*-

(defgroup conventional-commit
  nil
  "Completion function for conventional commits."
  :link '(url-link "https://www.conventionalcommits.org/en/v1.0.0/")
  :group 'completion
  :group 'git-commit-mode)

(defcustom conventional-commit-type-list
  '("feat"
    "fix"
    "build"
    "chore"
    "ci"
    "docs"
    "style"
    "refactor"
    "perf"
    "test")
  "List of types allowed at the beginning of a message.

See <https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#type>
for an example."
  :type '(repeat string))

(defcustom conventional-commit-scope-table nil
  "List of scope names allowed for the project.

It is recommended that you set this as a directory-local variable
in your project.

You can use any completion table as the value. For example, it
can be a list of strings or a dynamic completion table created by
`lazy-completion-table'. See also
`completion-at-point-functions'."
  :type '(or (repeat string)
             function)
  :local t)

;;;###autoload
(defun conventional-commit-capf ()
  "`completion-at-point-functions' entry for conventional commits."
  ;; Only complete on the first line
  (when (= (line-number-at-pos) 1)
    (let ((line-start (line-beginning-position))
          (end (point)))
      (save-restriction
        ;; Narrow to region from start of line to point
        (narrow-to-region line-start end)
        (save-excursion
          (goto-char (point-min))
          (cond
           ;; Completing scope: after "type(" (only if scopes are defined)
           ((and conventional-commit-scope-table
                 (looking-at (rx (+ alpha) "(" (group (* (not (any ")")))) eos)))
            (list (match-beginning 1)
                  (match-end 1)
                  conventional-commit-scope-table
                  :exclusive t))
           ;; Completing type: at start of line with zero or more alpha chars
           ((looking-at (rx (group (* alpha)) eos))
            (list (match-beginning 1)
                  (match-end 1)
                  conventional-commit-type-list
                  :exclusive t))))))))

;;;###autoload
(defun conventional-commit-setup ()
  "Set up `completion-at-point-functions' for the current buffer."
  ;; Prepend (not append) so our capf runs first
  (add-hook 'completion-at-point-functions #'conventional-commit-capf
            nil 'local))

(provide 'conventional-commit)
;;; conventional-commit.el ends here

