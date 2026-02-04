;;; crux.el ---  hydra -*- no-byte-compile: t; lexical-binding: t; -*-
;;;###autoload (autoload 'hydra-crux/body "crux" nil t)
;;;###autoload (general-define-key "C-c x" 'hydra-crux/body)
(defhydra hydra-crux (:color blue :hint nil)
  "
^Kill/Edit^           ^Lines^                 ^Files/Buffers^           ^Other^
^^^^^^^^---------------------------------------------------------------------------
_k_: kill whole line  _o_: open line below    _r_: rename file+buffer   _s_: sudo edit
_K_: kill backwards   _O_: open line above    _D_: delete file+buffer   _u_: view url
_d_: duplicate        _j_: join line          _b_: previous buffer      _i_: ispell+abbrev
_c_: dup+comment      _a_: smart BoL          _x_: kill other buffers   _e_: open external
"
  ("k" crux-kill-whole-line)
  ("K" crux-kill-line-backwards)
  ("d" crux-duplicate-current-line-or-region)
  ("c" crux-duplicate-and-comment-current-line-or-region)
  ("o" crux-smart-open-line)
  ("O" crux-smart-open-line-above)
  ("j" crux-top-join-line)
  ("a" crux-move-beginning-of-line)
  ("r" crux-rename-file-and-buffer)
  ("D" crux-delete-file-and-buffer)
  ("b" crux-switch-to-previous-buffer)
  ("x" crux-kill-other-buffers)
  ("s" crux-sudo-edit)
  ("u" crux-view-url)
  ("i" crux-ispell-word-then-abbrev)
  ("e" crux-open-with)
  ("q" nil :exit t))

;;; crux.el ends here
