;;; niwa-subscription-usage.el --- Session subscription usage report -*- lexical-binding: t -*-

;;; Commentary:
;; One disposable account-wide usage report per originating session.  The report
;; owns freshness, refresh supersession, and cancellation; provider inspection
;; never changes conversation history or model/Goal accounting.

;;; Code:

(require 'niwa-cockpit)
(require 'niwa-report)
(require 'niwa-subscription-usage-provider)

(defvar-local niwa-subscription-usage--buffer nil "Report owned by this data buffer.")
(defvar-local niwa-subscription-usage--states nil "Alist of backend to retrieval state.")
(defvar-local niwa-subscription-usage--generation 0 "Refresh generation, including cancellations.")

(defvar-keymap niwa-subscription-usage-mode-map
  :parent niwa-report-mode-map
  "g" #'niwa-subscription-usage-refresh)

(define-derived-mode niwa-subscription-usage-mode niwa-report-mode "Subscription usage"
  "Read subscription quotas; g refreshes, q cancels and returns to the owner."
  (add-hook 'kill-buffer-hook #'niwa-subscription-usage--stop nil t))

(defun niwa-subscription-usage--stop ()
  "Invalidate callbacks and cancel every pending retrieval."
  (cl-incf niwa-subscription-usage--generation)
  (pcase-dolist (`(,_ . ,state) niwa-subscription-usage--states)
    (when-let* ((cancel (plist-get state :cancel)))
      (setf (plist-get state :cancel) nil)
      (funcall cancel))))

(defun niwa-subscription-usage--render ()
  "Render this report without touching its owner or moving focus."
  (niwa-report-render
   (list :title "Subscription usage" :mode #'niwa-subscription-usage-mode
         :subtitle "Account-wide quotas include activity outside niwa.  g refresh · q close"
         :sections
         (or (cl-loop for (backend . state) in niwa-subscription-usage--states
                      for name = (gptel-backend-name backend)
                      for result = (plist-get state :result)
                      for retrieved = (plist-get state :retrieved)
                      for loading = (plist-get state :cancel)
                      for error = (plist-get state :error)
                      collect
                      (list :id (plist-get state :id)
                            :title (if (plist-get state :current) (concat name " (current session)") name)
                            :body (concat "Last successful retrieval: "
                                          (if retrieved (format-time-string "%Y-%m-%d %H:%M:%S %Z" retrieved)
                                            "Unavailable") "\n"
                                          (when loading "Loading...\n")
                                          (when error (concat error "\n"))
                                          (when (and result (or loading error))
                                            "STALE — previous successful result; see its retrieval time above.\n")
                                          (or result "Unavailable"))))
             (list (list :id 'empty :title "No Codex OAuth or Claude Code backend is configured."))))))

(defun niwa-subscription-usage-refresh ()
  "Refresh account-wide quotas for all configured subscription backends."
  (interactive)
  (niwa-subscription-usage--stop)
  (let* ((data (niwa-cockpit-context-data-buffer niwa-cockpit--context))
         (backends (and data (with-current-buffer data (niwa-subscription-usage-provider-backends))))
         (model (and data (buffer-local-value 'gptel-model data)))
         (buffer (current-buffer))
         (generation niwa-subscription-usage--generation))
    (setq niwa-subscription-usage--states
          (cl-loop for (backend . current) in backends
                   for old = (alist-get backend niwa-subscription-usage--states)
                   collect (cons backend (list :current current :cancel #'ignore :error nil
                                               :id (or (plist-get old :id) (gensym "usage-"))
                                               :result (plist-get old :result)
                                               :retrieved (plist-get old :retrieved)))))
    (niwa-subscription-usage--render)
    (pcase-dolist (`(,backend . ,state) niwa-subscription-usage--states)
      (let* ((gptel-model (niwa-subscription-usage-provider-model backend (plist-get state :current) model))
             (cancel
              (niwa-subscription-usage-provider-fetch
               backend
               (lambda (text error)
                 (when (and (buffer-live-p buffer)
                            (= generation (buffer-local-value 'niwa-subscription-usage--generation buffer)))
                   (with-current-buffer buffer
                     (setf (plist-get state :cancel) nil
                           (plist-get state :error) error)
                     (when text
                       (setf (plist-get state :result) text
                             (plist-get state :retrieved) (current-time)))
                     (niwa-subscription-usage--render)))))))
        (if (plist-get state :cancel)
            (setf (plist-get state :cancel) cancel)
          (funcall cancel))))))

;;;###autoload
(defun niwa-subscription-usage-show ()
  "Open and retrieve subscription quotas for all configured subscription backends."
  (interactive)
  (let* ((context (niwa-cockpit-current-context))
         (_ (niwa-cockpit-require-owner "subscription usage report" context))
         (data (niwa-cockpit-context-data-buffer context))
         (origin (niwa-cockpit-context-view-buffer context))
         (buffer (buffer-local-value 'niwa-subscription-usage--buffer data)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer "*Subscription usage*"))
      (with-current-buffer data (setq niwa-subscription-usage--buffer buffer))
      (with-current-buffer buffer
        (niwa-report-render
         (list :title "Subscription usage" :mode #'niwa-subscription-usage-mode :sections nil)
         origin)
        (setq niwa-cockpit--context context)))
    (niwa-window-display-buffer
     buffer nil (niwa-cockpit-menu-return-function context))
    (niwa-subscription-usage-refresh)
    buffer))

(provide 'niwa-subscription-usage)
;;; niwa-subscription-usage.el ends here
