;;; persistent-scratch.el --- Persistent scratch configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for persistent-scratch - save scratch buffer across sessions.

;;; Code:

(use-package persistent-scratch
  :demand
  :config (persistent-scratch-setup-default))

;;; persistent-scratch.el ends here
