;;; launch-app.el --- Launch MacOS apps from M-x -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds MacOS apps as available options when M-x is called, handing the launch
;; process to launchd to untie it from the Emacs pid.

;;; Code:

(defun isp/define-app-launchers ()
  "Dynamically defines launch/ commands for MacOS apps"
  (dolist (app (directory-files "/Applications" nil "\\.app$"))
    (let* ((name (file-name-sans-extension app))
           (sym (intern (format "launch/%s"
                                (downcase
                                 (replace-regexp-in-string " " "-" name))))))
      (unless (fboundp sym)
        (defalias sym
          (lambda ()
            (interactive)
            (call-process "open" nil 0 nil "-a" name))
          (format "Launch %s." name))))))

(defun isp/refresh-app-launchers ()
  "Rescan MacOS apps and define new launchers."
  (interactive)
  (isp/define-app-launchers))

(isp/define-app-launchers)
;;; launch-app.el ends here
