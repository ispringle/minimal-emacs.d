;;; navigate.el --- Navigation hydra -*- no-byte-compile: t; lexical-binding: t; -*-

;;;###autoload (autoload 'hydra-navigate/body "navigate" nil t)
;;;###autoload (general-define-key "C-c n" 'hydra-navigate/body)

(defhydra hydra-navigate (:color pink :hint nil)
  "
^Char/Line^              ^Word^              ^Sentence^          ^Paragraph^         ^Sexp/List^
^^^^^^^^-------------------------------------------------------------------------------------------------------------
_n_: next line (C-n)     _F_: word → (M-f)   _A_: sent → (M-a)   _}_: para → (M-})   _C-f_: sexp → (C-M-f)
_p_: prev line (C-p)     _B_: word ← (M-b)   _E_: sent ← (M-e)   _{_: para ← (M-{)   _C-b_: sexp ← (C-M-b)
_f_: char → (C-f)                                                                  _C-d_: into list (C-M-d)
_b_: char ← (C-b)                                                                  _C-u_: out of list (C-M-u)
_a_: line start (C-a)
_e_: line end (C-e)
_m_: indentation (M-m)

^Scroll^                    ^Page^                   ^Buffer^              ^Other Window^
^^^^^^^^-------------------------------------------------------------------------------------------------------------
_M-n_: smooth down (M-n)    _v_: page down (C-v)     _<_: begin (M-<)      _C-v_: scroll other → (C-M-v)
_M-p_: smooth up (M-p)      _V_: page up (M-v)       _>_: end (M->)        _C-V_: scroll other ← (C-M-S-v)
_._: recenter (C-l)

_q_: quit
"
  ;; Char/Line
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("m" back-to-indentation)

  ;; Word
  ("F" forward-word)
  ("B" backward-word)

  ;; Sentence
  ("A" forward-sentence)
  ("E" backward-sentence)

  ;; Paragraph
  ("}" forward-paragraph)
  ("{" backward-paragraph)

  ;; Sexp/List
  ("C-f" forward-sexp)
  ("C-b" backward-sexp)
  ("C-d" down-list)
  ("C-u" backward-up-list)

  ;; Scroll
  ("M-n" smooth-scroll-down)
  ("M-p" smooth-scroll-up)
  ("." recenter-top-bottom)

  ;; Page
  ("v" scroll-up-command)
  ("V" scroll-down-command)

  ;; Buffer
  ("<" beginning-of-buffer)
  (">" end-of-buffer)

  ;; Other Window
  ("C-v" scroll-other-window)
  ("C-V" scroll-other-window-down)

  ("q" nil :exit t))

;;; navigate.el ends here
