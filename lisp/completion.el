;;; completion.el --- Completion framework configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Completion framework including corfu, cape, vertico, orderless, marginalia,
;; embark, and consult.

;;; Code:

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :general
  ("C-c p" 'cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (elpaca-after-init . marginalia-mode))

(use-package amx
  :ensure t
  :demand t
  :config
  (amx-mode 1))

(use-package embark
  :ensure t
  :commands (embark-act embark-dwim embark-export embark-collect
             embark-bindings embark-prefix-help-command)
  :general
  ("C-." 'embark-act
   "C-;" 'embark-dwim
   "C-h B" 'embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :general
  ("C-c M-x" 'consult-mode-command
   "C-c h" 'consult-history
   "C-c k" 'consult-kmacro
   "C-c m" 'consult-man
   "C-c i" 'consult-info
   [remap Info-search] 'consult-info
   "C-x M-:" 'consult-complex-command
   "C-x b" 'consult-buffer
   "C-x 4 b" 'consult-buffer-other-window
   "C-x 5 b" 'consult-buffer-other-frame
   "C-x t b" 'consult-buffer-other-tab
   "C-x r b" 'consult-bookmark
   "C-x p b" 'consult-project-buffer
   "M-#" 'consult-register-load
   "M-'" 'consult-register-store
   "C-M-#" 'consult-register
   "M-y" 'consult-yank-pop
   "M-g e" 'consult-compile-error
   "M-g f" 'consult-flymake
   "M-g g" 'consult-goto-line
   "M-g M-g" 'consult-goto-line
   "M-g o" 'consult-outline
   "M-g m" 'consult-mark
   "M-g k" 'consult-global-mark
   "M-g i" 'consult-imenu
   "M-g I" 'consult-imenu-multi
   "M-s d" 'consult-find
   "M-s c" 'consult-locate
   "M-s g" 'consult-grep
   "M-s G" 'consult-git-grep
   "M-s r" 'consult-ripgrep
   "M-s l" 'consult-line
   "M-s L" 'consult-line-multi
   "M-s k" 'consult-keep-lines
   "M-s u" 'consult-focus-lines
   "M-s e" 'consult-isearch-history)
  (:keymaps 'isearch-mode-map
   "M-e" 'consult-isearch-history
   "M-s e" 'consult-isearch-history
   "M-s l" 'consult-line
   "M-s L" 'consult-line-multi)
  (:keymaps 'minibuffer-local-map
   "M-s" 'consult-history
   "M-r" 'consult-history)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;;; completion.el ends here
