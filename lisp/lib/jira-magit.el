;;; jira-magit.el --- Jira ticket completion for Magit branch creation -*- lexical-binding: t; -*-

;; Author:
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (jira "0.1") (magit "3.0") (git-commit "3.0"))
;; Keywords: tools, vc
;; URL:

;;; Commentary:

;; Provides Jira ticket completion in two contexts:
;;
;; 1. Magit branch creation prompts
;; 2. Git commit message buffers
;;
;; Branch Creation Usage:
;;   (require 'jira-magit)
;;   (jira-magit-setup)  ; Enable with default keybinding (C-c j)
;;
;;   Customize the keybinding:
;;     (setq jira-magit-completion-key "C-c C-j")
;;     (jira-magit-setup)
;;
;;   Disable automatic keybinding:
;;     (setq jira-magit-completion-key nil)
;;
;; Commit Message Usage:
;;   (jira-magit-enable-commit-completion)
;;
;;   In a commit buffer, type a partial ticket key (e.g., "CUS-" or "WEB")
;;   and press C-i (completion-at-point) to complete with ticket summaries.
;;
;;   Disable commit completion:
;;     (setq jira-magit-enable-commit-completion nil)
;;     ;; or
;;     (jira-magit-disable-commit-completion)

;;; Code:

(defgroup jira-magit nil
  "Jira ticket completion for Magit branch creation."
  :group 'jira
  :group 'magit)

(defvar jira-magit--tickets-cache nil
  "Cached list of Jira tickets for completion.
Each entry is a cons of (KEY . SUMMARY).")

(defvar jira-magit--refresh-requested nil
  "Flag indicating that a refresh was requested during completion.")

(defcustom jira-magit-max-tickets 100
  "Maximum number of tickets to fetch from Jira."
  :type 'integer
  :group 'jira-magit)

(defun jira-magit--fetch-tickets-sync ()
  "Fetch tickets assigned to current user synchronously.
Returns list of (KEY . SUMMARY) cons cells, or nil on error."
  (condition-case err
      (progn
        (require 'jira-api)
        (require 'jira-table)
        (require 'jira-issues)
        (require 'request)
        (message "Loading Jira tickets...")
        (jira-api--initialize-current-url)
        (jira-api-get-basic-data)
        (let* ((response (jira-api-search
                         :params `(("jql" . "assignee = currentUser()")
                                   ("maxResults" . ,(number-to-string jira-magit-max-tickets))
                                   ("fields" . "key,summary"))
                         :sync t))
               ;; Extract data from the response object
               (data (request-response-data response))
               (issues (or (cdr (assoc 'issues data)) (vector)))
               (tickets nil))
          (cl-loop for issue across issues
                   do (let ((key (cdr (assoc 'key issue)))
                            (summary (cdr (assoc 'summary (cdr (assoc 'fields issue))))))
                        (when (and key summary)
                          (push (cons key summary) tickets))))
          (let ((result (nreverse tickets)))
            (message "Loaded %d Jira ticket%s"
                     (length result)
                     (if (= (length result) 1) "" "s"))
            result)))
    (error
     (message "Failed to fetch Jira tickets: %s" (error-message-string err))
     nil)))

(defun jira-magit--get-tickets (&optional force-refresh)
  "Get cached Jira tickets, or fetch them if not cached.
With optional FORCE-REFRESH, ignore cache and fetch fresh data.
Returns list of tickets or nil if none available."
  (when (or force-refresh (null jira-magit--tickets-cache))
    (setq jira-magit--tickets-cache (jira-magit--fetch-tickets-sync)))
  jira-magit--tickets-cache)

(defun jira-magit--refresh-and-continue ()
  "Refresh Jira tickets and restart completion.
This is meant to be called from within a completing-read session."
  (interactive)
  (setq jira-magit--refresh-requested t)
  (abort-recursive-edit))

(defvar jira-magit-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-r") #'jira-magit--refresh-and-continue)
    map)
  "Keymap active during Jira ticket completion.
\\<jira-magit-completion-map>
\\[jira-magit--refresh-and-continue] - Refresh ticket list from Jira")

(defun jira-magit--insert-ticket ()
  "Insert a Jira ticket key at point using completion.
Shows tickets as \"KEY: Summary\" but inserts only the key.
Press \\[jira-magit--refresh-and-continue] during completion to refresh the ticket list."
  (interactive)
  (let ((tickets (jira-magit--get-tickets))
        (done nil)
        key)
    ;; Handle empty ticket list
    (unless tickets
      (when (y-or-n-p "No Jira tickets found. Try fetching again? ")
        (setq tickets (jira-magit--get-tickets t))))

    (when tickets
      ;; Loop until user selects a ticket or cancels
      (while (not done)
        (setq jira-magit--refresh-requested nil)
        (condition-case nil
            (let* ((candidates (mapcar (lambda (ticket)
                                        (cons (concat (car ticket) ": " (cdr ticket))
                                              (car ticket)))
                                      tickets))
                   (selection (minibuffer-with-setup-hook
                                  (lambda ()
                                    (use-local-map (make-composed-keymap
                                                   jira-magit-completion-map
                                                   (current-local-map))))
                                (completing-read "Jira ticket (C-r to refresh): "
                                               candidates nil t))))
              (setq key (cdr (assoc selection candidates)))
              (setq done t))
          (quit
           ;; Check if quit was due to refresh request
           (if jira-magit--refresh-requested
               (setq tickets (jira-magit--get-tickets t))
             ;; User actually cancelled
             (setq done t
                   key nil)))))

      ;; Insert the selected ticket key
      (when key
        ;; Delete any previous ticket key or partial input
        ;; Match complete ticket pattern: LETTERS-DIGITS (e.g., CUS-1234, WEB-2067)
        (when (looking-back "\\b[A-Z]+-[0-9]+"
                           (max (point-min) (- (point) 30)))
          (delete-region (match-beginning 0) (match-end 0)))
        (insert key)))))

(defcustom jira-magit-completion-key "C-c j"
  "Key binding for Jira ticket completion in Magit branch prompts.
Set to nil to disable automatic keybinding setup."
  :type '(choice (string :tag "Key sequence")
                 (const :tag "No automatic binding" nil))
  :group 'jira-magit)

(defvar jira-magit-minibuffer-map nil
  "Keymap for Jira ticket completion in Magit minibuffer.")

(defun jira-magit--make-minibuffer-map ()
  "Create the minibuffer keymap with configured keybinding."
  (when jira-magit-completion-key
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd jira-magit-completion-key) #'jira-magit--insert-ticket)
      map)))

(defun jira-magit--setup-completion ()
  "Set up Jira ticket completion key binding in minibuffer.
Activates when creating or checking out branches in Magit.
Only activates if `jira-magit-completion-key' is non-nil."
  (when (and jira-magit-completion-key (minibufferp))
    (let ((prompt (minibuffer-prompt)))
      (when (string-match-p "\\(?:branch\\|checkout\\)" prompt)
        (let ((map (jira-magit--make-minibuffer-map)))
          (when map
            (use-local-map (make-composed-keymap map (current-local-map)))))))))

;;;###autoload
(defun jira-magit-setup ()
  "Enable Jira ticket completion in Magit branch prompts.
Adds hook to set up keybinding in minibuffer."
  (interactive)
  (add-hook 'minibuffer-setup-hook #'jira-magit--setup-completion))

;;;###autoload
(defun jira-magit-disable ()
  "Disable Jira ticket completion in Magit branch prompts.
Removes hook that sets up keybinding in minibuffer."
  (interactive)
  (remove-hook 'minibuffer-setup-hook #'jira-magit--setup-completion))

;;;###autoload
(defun jira-magit-clear-cache ()
  "Clear the Jira tickets cache.
Next ticket completion will fetch fresh data from Jira."
  (interactive)
  (setq jira-magit--tickets-cache nil)
  (message "Jira tickets cache cleared"))

;;; Completion-at-point support for commit buffers

(defun jira-magit-completion-at-point ()
  "Provide completion for Jira ticket keys in commit messages.
Completes when point is after a partial ticket key pattern like:
  FOO-123, WEB-, CUS, etc.

Shows tickets as \"KEY: Summary\" but completes to just the key."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (start (car bounds))
              (end (cdr bounds))
              (text (buffer-substring-no-properties start end)))
    ;; Only trigger if text looks like a partial ticket key
    (when (string-match-p "^[A-Z]\\{1,10\\}\\(-[0-9]*\\)?$" text)
      (let ((tickets (jira-magit--get-tickets)))
        (when tickets
          (list start end
                (mapcar (lambda (ticket)
                         (concat (car ticket) ": " (cdr ticket)))
                       tickets)
                :annotation-function (lambda (_) "")  ; Summary already in candidate
                :exit-function (lambda (candidate _status)
                                ;; Extract just the key from "KEY: Summary"
                                (when (string-match "^\\([A-Z]+-[0-9]+\\)" candidate)
                                  (let ((key (match-string 1 candidate)))
                                    (delete-region start end)
                                    (insert key))))
                :exclusive 'no))))))

;;;###autoload
(define-minor-mode jira-magit-commit-mode
  "Minor mode for Jira ticket completion in commit messages.
When enabled, provides completion-at-point for Jira ticket keys.
Type a partial ticket key (e.g., \"CUS-\" or \"WEB\") and press
\\[completion-at-point] to complete."
  :lighter " Jira"
  :group 'jira-magit
  (if jira-magit-commit-mode
      (add-hook 'completion-at-point-functions
                #'jira-magit-completion-at-point nil t)
    (remove-hook 'completion-at-point-functions
                 #'jira-magit-completion-at-point t)))

;;;###autoload
(defun jira-magit-enable-commit-completion ()
  "Enable Jira ticket completion in git-commit-mode buffers."
  (interactive)
  (add-hook 'git-commit-mode-hook #'jira-magit-commit-mode))

;;;###autoload
(defun jira-magit-disable-commit-completion ()
  "Disable Jira ticket completion in git-commit-mode buffers."
  (interactive)
  (remove-hook 'git-commit-mode-hook #'jira-magit-commit-mode))

(provide 'jira-magit)
;;; jira-magit.el ends here
