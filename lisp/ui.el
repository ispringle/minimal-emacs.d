;;; ui.el --- UI and display configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; User interface and display settings including line numbers, scrolling,
;; and visual elements.

;;; Code:

;; Set font
(set-face-attribute 'default nil
                    :font "Maple Mono NF CN"
                    :height 120)  ; 12pt

;; Theme configuration
(defvar default-theme-light 'leuven
  "Default light theme.")

(defvar default-theme-dark 'leuven-dark
  "Default dark theme.")

(defun apply-theme (theme-name)
  "Load the specified THEME-NAME, disabling all other themes first."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme-name t))

(defun set-theme-by-appearance (appearance)
  "Set theme based on system APPEARANCE (light or dark)."
  (pcase appearance
    ('light (apply-theme default-theme-light))
    ('dark (apply-theme default-theme-dark))))

;; Set up system theme following on macOS (ns-system-appearance is from emacs-plus)
(when (eq system-type 'darwin)
  (when (boundp 'ns-system-appearance)
    (add-hook 'ns-system-appearance-change-functions
              #'set-theme-by-appearance)))

;; Fallback for non-Mac systems or when ns-system-appearance is not available
(unless (and (eq system-type 'darwin) (boundp 'ns-system-appearance))
  (add-hook 'elpaca-after-init-hook (lambda ()
                                      (set-theme-by-appearance 'light))))

(setq package-install-upgrade-built-in t)
(delete-selection-mode 1)

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(setq treesit-font-lock-level 4)

(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'show-paren-mode)
(add-hook 'after-init-hook #'winner-mode)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(add-hook 'after-init-hook #'window-divider-mode)
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

(setq tooltip-hide-delay 20)
(setq tooltip-delay 0.4)
(setq tooltip-short-delay 0.08)
(tooltip-mode 1)

;;; ui.el ends here
