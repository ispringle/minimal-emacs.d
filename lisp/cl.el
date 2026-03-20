(use-package sly
  :ensure t
  :defer t
  :commands (sly sly-connect)
  :init
  (setq inferior-lisp-program "sbcl"
        sly-net-coding-system 'utf-8-unix)
  :config
  (setq sly-complete-symbol-function 'sly-flex-completions))

(use-package sly-asdf
  :ensure (:host github :repo "mmgeorge/sly-asdf")
  :after sly)

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-macrostep
  :ensure t
  :after sly)

(use-package clhs
  :ensure t
  :defer t
  :config
  (setq common-lisp-hyperspec-root
        "http://www.lispworks.com/documentation/HyperSpec/"))

(use-package common-lisp-snippets
  :ensure t
  :after yasnippet)

(add-hook 'common-lisp-mode-hook
          (lambda ()
            (setq-local tab-width 2
                        indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.asd\\'" . common-lisp-mode))
