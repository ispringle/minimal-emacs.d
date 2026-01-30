;;; scrolling.el --- Smooth scrolling configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Smooth scrolling with keyboard for a middle-ground between line-by-line
;; and full page scrolling.

;;; Code:

;; Smooth pixel-based scrolling for mouse/trackpad (except on emacs-mac which has native support)
(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

;; Smooth pixel-based scrolling for keyboard
(pixel-scroll-mode 1)

;; Configure smooth scrolling parameters
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 3)
(setq scroll-preserve-screen-position t)

;; Scroll multiple lines at once (faster than C-n/C-p, smoother than C-v/M-v)
(defun smooth-scroll-down ()
  "Scroll down smoothly by several lines."
  (interactive)
  (pixel-scroll-precision-scroll-down (* 5 (line-pixel-height))))

(defun smooth-scroll-up ()
  "Scroll up smoothly by several lines."
  (interactive)
  (pixel-scroll-precision-scroll-up (* 5 (line-pixel-height))))

(general-define-key
 "M-n" 'smooth-scroll-down
 "M-p" 'smooth-scroll-up)

;;; scrolling.el ends here
