;;; mevedel-subscription-usage.el --- Session subscription usage report -*- lexical-binding: t -*-

;;; Commentary:
;; One disposable account-wide usage report per originating session.  The report
;; owns freshness, refresh supersession, and cancellation; provider inspection
;; never changes conversation history or model/Goal accounting.

;;; Code:

(require 'mevedel-cockpit)
(require 'mevedel-report)
(require 'mevedel-subscription-usage-provider)

(defvar-local mevedel-subscription-usage--buffer nil "Report owned by this data buffer.")
(defvar-local mevedel-subscription-usage--backend nil "Backend of the displayed result.")
(defvar-local mevedel-subscription-usage--result nil "Last successful provider text.")
(defvar-local mevedel-subscription-usage--retrieved nil "Time of last successful retrieval.")
(defvar-local mevedel-subscription-usage--error nil "Last retrieval error.")
(defvar-local mevedel-subscription-usage--loading nil "Non-nil during retrieval.")
(defvar-local mevedel-subscription-usage--cancel nil "Pending retrieval cancellation function.")
(defvar-local mevedel-subscription-usage--generation 0 "Refresh generation, including cancellations.")

(defvar-keymap mevedel-subscription-usage-mode-map
  :parent mevedel-report-mode-map
  "g" #'mevedel-subscription-usage-refresh)

(define-derived-mode mevedel-subscription-usage-mode mevedel-report-mode "Subscription usage"
  "Read subscription quotas; g refreshes, q cancels and returns to the owner."
  (add-hook 'kill-buffer-hook #'mevedel-subscription-usage--stop nil t))

(defun mevedel-subscription-usage--stop ()
  "Cancel the current retrieval and invalidate callbacks before closing."
  (cl-incf mevedel-subscription-usage--generation)
  (when-let* ((cancel mevedel-subscription-usage--cancel))
    (setq mevedel-subscription-usage--cancel nil)
    (funcall cancel)))

(defun mevedel-subscription-usage--render ()
  "Render this report without touching its owner or moving focus."
  (mevedel-report-render
   (list :title "Subscription usage" :mode #'mevedel-subscription-usage-mode
         :subtitle "Account-wide quotas include activity outside mevedel.  g refresh · q close"
         :sections
         (list
          (list :id 'status :title "Retrieval"
                :body
                (concat
                 (format "Provider: %s\nLast successful retrieval: %s\n"
                         (if mevedel-subscription-usage--backend
                             (gptel-backend-name mevedel-subscription-usage--backend) "Unavailable")
                         (if mevedel-subscription-usage--retrieved
                             (format-time-string "%Y-%m-%d %H:%M:%S %Z" mevedel-subscription-usage--retrieved)
                           "Unavailable"))
                 (when mevedel-subscription-usage--loading "Loading...\n")
                 (when mevedel-subscription-usage--error (concat mevedel-subscription-usage--error "\n"))
                 (when (and mevedel-subscription-usage--result
                            (or mevedel-subscription-usage--loading mevedel-subscription-usage--error))
                   "STALE — previous successful result; see its retrieval time above.\n")))
          (list :id 'quotas :title "Subscription quotas"
                :body (or mevedel-subscription-usage--result "Unavailable"))))))

(defun mevedel-subscription-usage-refresh ()
  "Resolve the current session backend and refresh its account-wide quotas."
  (interactive)
  (mevedel-subscription-usage--stop)
  (let* ((data (mevedel-cockpit-context-data-buffer mevedel-cockpit--context))
         (backend (and data (buffer-local-value 'gptel-backend data)))
         (model (and data (buffer-local-value 'gptel-model data)))
         (buffer (current-buffer))
         (generation mevedel-subscription-usage--generation)
         completed)
    (unless (eq backend mevedel-subscription-usage--backend)
      (setq mevedel-subscription-usage--result nil
            mevedel-subscription-usage--retrieved nil))
    (setq mevedel-subscription-usage--backend backend
          mevedel-subscription-usage--error nil
          mevedel-subscription-usage--loading t)
    (mevedel-subscription-usage--render)
    (let* ((gptel-model model)
           (cancel
            (mevedel-subscription-usage-provider-fetch
             backend
             (lambda (text error)
               (when (and (buffer-live-p buffer)
                          (= generation (buffer-local-value 'mevedel-subscription-usage--generation buffer)))
                 (setq completed t)
                 (with-current-buffer buffer
                   (setq mevedel-subscription-usage--loading nil
                         mevedel-subscription-usage--cancel nil
                         mevedel-subscription-usage--error error)
                   (when text
                     (setq mevedel-subscription-usage--result text
                           mevedel-subscription-usage--retrieved (current-time)))
                   (mevedel-subscription-usage--render)))))))
      (if (and (not completed) (buffer-live-p buffer)
               (= generation mevedel-subscription-usage--generation))
          (setq mevedel-subscription-usage--cancel cancel)
        (funcall cancel)))))

;;;###autoload
(defun mevedel-subscription-usage-show ()
  "Open and retrieve subscription quotas for the current session backend."
  (interactive)
  (let* ((context (mevedel-cockpit-current-context))
         (_ (mevedel-cockpit-require-owner "subscription usage report" context))
         (data (mevedel-cockpit-context-data-buffer context))
         (origin (mevedel-cockpit-context-view-buffer context))
         (buffer (buffer-local-value 'mevedel-subscription-usage--buffer data)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer "*Subscription usage*"))
      (with-current-buffer data (setq mevedel-subscription-usage--buffer buffer))
      (with-current-buffer buffer
        (mevedel-report-render
         (list :title "Subscription usage" :mode #'mevedel-subscription-usage-mode :sections nil)
         origin)
        (setq mevedel-cockpit--context context)))
    (pop-to-buffer buffer)
    (mevedel-subscription-usage-refresh)
    buffer))

(provide 'mevedel-subscription-usage)
;;; mevedel-subscription-usage.el ends here
