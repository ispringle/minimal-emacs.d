;;; combobulate.el --- combobulate hydra -*- no-byte-compile: t; lexical-binding: t; -*-

;;;###autoload (autoload 'hydra-combobulate/body "combobulate" nil t)
;;;###autoload (general-define-key "C-c C" 'hydra-combobulate/body)

(defhydra hydra-combobulate (:color pink :hint nil)
  "
^Navigate^           ^Edit^                ^Mark^              ^Misc^
^^^^^^^^-----------------------------------------------------------------
_n_: next sibling (C-M-n)     _t_: transpose (C-M-t)      _m_: mark node (C-M-h)    _p_: pretty print (C-c o p)
_p_: prev sibling (C-M-p)     _k_: kill (C-M-k)           _e_: mark defun (C-M-SPC) _P_: envelop (C-c o P)
_u_: up parent (C-M-u)        _c_: clone (C-M-c)          _d_: mark dwim            _D_: drag (C-c o D)
_d_: down child (C-M-d)       _v_: vanish (C-M-v)         ^ ^                       _._: edit node (C-c o e)
_a_: avy jump (C-c o j)       _r_: splice (C-M-<up>)      ^ ^                       _q_: quit
"
  ("n" combobulate-navigate-next)
  ("p" combobulate-navigate-previous)
  ("u" combobulate-navigate-up)
  ("d" combobulate-navigate-down)
  ("a" combobulate-avy-jump)
  
  ("t" combobulate-transpose-sexps)
  ("k" combobulate-kill-node-dwim)
  ("c" combobulate-clone-node-dwim)
  ("v" combobulate-vanish-node)
  ("r" combobulate-splice-up)
  
  ("m" combobulate-mark-node-dwim)
  ("e" combobulate-mark-defun)
  ("d" combobulate-mark-node-dwim)
  
  ("p" combobulate-pretty-print-node-at-point)
  ("P" combobulate-envelop-node)
  ("D" combobulate-drag-up)
  ("." combobulate-edit-node-at-point)
  
  ("q" nil :exit t))

;;; combobulate.el ends here
