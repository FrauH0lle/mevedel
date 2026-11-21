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
(defvar-local niwa-subscription-usage--backend nil "Backend of the displayed result.")
(defvar-local niwa-subscription-usage--result nil "Last successful provider text.")
(defvar-local niwa-subscription-usage--retrieved nil "Time of last successful retrieval.")
(defvar-local niwa-subscription-usage--error nil "Last retrieval error.")
(defvar-local niwa-subscription-usage--loading nil "Non-nil during retrieval.")
(defvar-local niwa-subscription-usage--cancel nil "Pending retrieval cancellation function.")
(defvar-local niwa-subscription-usage--generation 0 "Refresh generation, including cancellations.")

(defvar-keymap niwa-subscription-usage-mode-map
  :parent niwa-report-mode-map
  "g" #'niwa-subscription-usage-refresh)

(define-derived-mode niwa-subscription-usage-mode niwa-report-mode "Subscription usage"
  "Read subscription quotas; g refreshes, q cancels and returns to the owner."
  (add-hook 'kill-buffer-hook #'niwa-subscription-usage--stop nil t))

(defun niwa-subscription-usage--stop ()
  "Cancel the current retrieval and invalidate callbacks before closing."
  (cl-incf niwa-subscription-usage--generation)
  (when-let* ((cancel niwa-subscription-usage--cancel))
    (setq niwa-subscription-usage--cancel nil)
    (funcall cancel)))

(defun niwa-subscription-usage--render ()
  "Render this report without touching its owner or moving focus."
  (niwa-report-render
   (list :title "Subscription usage" :mode #'niwa-subscription-usage-mode
         :subtitle "Account-wide quotas include activity outside niwa.  g refresh · q close"
         :sections
         (list
          (list :id 'status :title "Retrieval"
                :body
                (concat
                 (format "Provider: %s\nLast successful retrieval: %s\n"
                         (if niwa-subscription-usage--backend
                             (gptel-backend-name niwa-subscription-usage--backend) "Unavailable")
                         (if niwa-subscription-usage--retrieved
                             (format-time-string "%Y-%m-%d %H:%M:%S %Z" niwa-subscription-usage--retrieved)
                           "Unavailable"))
                 (when niwa-subscription-usage--loading "Loading...\n")
                 (when niwa-subscription-usage--error (concat niwa-subscription-usage--error "\n"))
                 (when (and niwa-subscription-usage--result
                            (or niwa-subscription-usage--loading niwa-subscription-usage--error))
                   "STALE — previous successful result; see its retrieval time above.\n")))
          (list :id 'quotas :title "Subscription quotas"
                :body (or niwa-subscription-usage--result "Unavailable"))))))

(defun niwa-subscription-usage-refresh ()
  "Resolve the current session backend and refresh its account-wide quotas."
  (interactive)
  (niwa-subscription-usage--stop)
  (let* ((data (niwa-cockpit-context-data-buffer niwa-cockpit--context))
         (backend (and data (buffer-local-value 'gptel-backend data)))
         (model (and data (buffer-local-value 'gptel-model data)))
         (buffer (current-buffer))
         (generation niwa-subscription-usage--generation)
         completed)
    (unless (eq backend niwa-subscription-usage--backend)
      (setq niwa-subscription-usage--result nil
            niwa-subscription-usage--retrieved nil))
    (setq niwa-subscription-usage--backend backend
          niwa-subscription-usage--error nil
          niwa-subscription-usage--loading t)
    (niwa-subscription-usage--render)
    (let* ((gptel-model model)
           (cancel
            (niwa-subscription-usage-provider-fetch
             backend
             (lambda (text error)
               (when (and (buffer-live-p buffer)
                          (= generation (buffer-local-value 'niwa-subscription-usage--generation buffer)))
                 (setq completed t)
                 (with-current-buffer buffer
                   (setq niwa-subscription-usage--loading nil
                         niwa-subscription-usage--cancel nil
                         niwa-subscription-usage--error error)
                   (when text
                     (setq niwa-subscription-usage--result text
                           niwa-subscription-usage--retrieved (current-time)))
                   (niwa-subscription-usage--render)))))))
      (if (and (not completed) (buffer-live-p buffer)
               (= generation niwa-subscription-usage--generation))
          (setq niwa-subscription-usage--cancel cancel)
        (funcall cancel)))))

;;;###autoload
(defun niwa-subscription-usage-show ()
  "Open and retrieve subscription quotas for the current session backend."
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
