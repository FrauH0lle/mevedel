;;; test-niwa-pending-inputs-settlement.el --- Replies during cleanup -*- lexical-binding: t -*-

;;; Commentary:
;; Hold real terminal settlement across composer submission and an early drain.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-pending-inputs)
(require 'niwa-presets)
(require 'niwa-turn)
(require 'niwa-view-composer)

(niwa-deftest niwa-view-send/terminal-settlement (:quiet t)
  (dolist (stage '(checkpoint torn-down))
    (niwa-engine-test--with-session :file-workspace t
      (let* ((gptel--known-backends nil)
             (view (generate-new-buffer " *settlement-reply-view*"))
             (reply "> 2. **ADR 0091:** may it get that one pointer edit?\nYes")
             (owner (if (eq ,engine 'native)
                        (gptel-make-fsm
                         :state 'DONE
                         :info (list :buffer buffer
                                     :niwa-request-id (niwa-request-id request)
                                     :niwa-request request))
                      request))
             checkpoint-ready resume admitted)
        (unwind-protect
            (progn
              (if (eq ,engine 'native)
                  (setf (niwa-request-fsm request) owner)
                (setf (niwa-engine-info owner)
                      (list :buffer buffer
                            :niwa-request-id (niwa-request-id request)
                            :niwa-request request
                            :external-history t :niwa-acp-outcome 'success)))
              (gptel-make-openai "Reply" :key "test" :models '(reply)
                                 :protocol "http" :host "127.0.0.1:1")
              (niwa-model-set-session-provider
               session (niwa-model-resolve-provider "Reply:reply") buffer)
              (setq-local gptel-system-prompt "Settlement reply fixture"
                          gptel-tools nil gptel-use-curl nil gptel-stream nil
                          gptel-prompt-transform-functions
                          '(niwa-skills--transform-apply-request-model-policy)
                          gptel-send--handlers
                          (niwa-preset--build-handlers
                           (copy-tree (default-value 'gptel-send--handlers))))
              (niwa-chat-install-request-hooks)
              (niwa-view--setup view buffer)
              ;; Keep admission fenced after the ordinary chain ends, so its
              ;; first drain necessarily runs too early.
              (niwa--turn-hold owner)
              (cl-letf (((symbol-function 'niwa--turn-checkpoint)
                         (lambda (machine)
                           (when (and (eq machine owner) (not checkpoint-ready))
                             (setq resume niwa--turn-resume)
                             'niwa-turn-pending)))
                        ((symbol-function 'gptel--url-get-response)
                         (lambda (fsm) (push fsm admitted)))
                        ((symbol-function 'gptel-curl-get-response)
                         (lambda (fsm) (push fsm admitted))))
                (niwa--complete-turn owner)
                (niwa-test--await 5 "Settlement did not reach checkpoint"
                  resume)
                (when (eq stage 'torn-down)
                  (setq checkpoint-ready t)
                  (funcall resume)
                  (niwa-test--await 5 "Request teardown did not finish"
                    (null niwa--current-request)))
                (with-current-buffer view
                  (niwa-view-test--insert-composer-draft reply)
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send))
                  (should (string-empty-p (niwa-view-composer-draft-text nil t))))
                (should (= 1 (length (niwa-session-pending-follow-ups session))))
                (should-not (niwa-session-pending-steering session))
                (when (eq stage 'checkpoint)
                  (should (eq request niwa--current-request))
                  (setq checkpoint-ready t)
                  (funcall resume)
                  (niwa-test--await 5 "Request teardown did not finish"
                    (null niwa--current-request)))
                (niwa-test--await 5 "Settlement chain did not release its hold"
                  (= 1 (plist-get (niwa-engine-info owner)
                                  :niwa-settlement-holds)))
                (should (niwa-turn-busy-p buffer))
                (niwa-view--drain-follow-up buffer)
                (should-not admitted)
                (should (= 1 (length (niwa-session-pending-follow-ups session))))
                (niwa--turn-release owner)
                (should-not (niwa-turn-busy-p buffer))
                (should-not (niwa--turn-publication-pending-p owner))
                (should-not (niwa-view--follow-up-auto-drain-blocked-p session))
                (should (eq 'follow-up (niwa-pending-inputs--idle-work buffer)))
                (niwa-test--await 5 "Reply was stranded after final hold release"
                  admitted)
                (should (= 1 (length admitted)))
                (should-not (niwa-session-pending-follow-ups session))
                (should-not (eq request niwa--current-request))
                (should (= 1 (how-many (regexp-quote reply) (point-min) (point-max))))
                ;; Re-offering cannot deliver the reply a second time.
                (niwa-view--drain-follow-up buffer)
                (should (= 1 (length admitted)))
                (gptel--fsm-transition (car admitted) 'DONE)
                (niwa-test--await 5 "Reply did not finish settlement"
                  (not (niwa-turn-busy-p buffer)))))
          (when (buffer-live-p view) (kill-buffer view))))))
  (engine)
  :doc "native terminal replies survive checkpoint and post-teardown holds"
  'native
  :doc "external terminal replies survive checkpoint and post-teardown holds"
  'external)

(niwa-deftest niwa--turn-release/failed-settlement (:quiet t)
  (dolist (status '(error aborted))
    (niwa-engine-test--with-session :file-workspace t
      (let* ((view (generate-new-buffer " *failed-settlement-view*"))
             (info (list :buffer buffer
                         :niwa-request-id (niwa-request-id request)
                         :niwa-request request))
             (owner (if (eq ,engine 'native)
                        (gptel-make-fsm :state 'ERRS :info info)
                      request))
             (offers 0))
        (unwind-protect
            (progn
              (if (eq ,engine 'native)
                  (setf (niwa-request-fsm request) owner)
                (setf (niwa-engine-info owner)
                      (append info (list :external-history t
                                         :niwa-acp-outcome status))))
              (niwa-view--setup view buffer)
              (niwa-session-enqueue-pending-input
               session 'follow-up '(:input "next reply"))
              (niwa--turn-hold owner)
              (cl-letf (((symbol-function
                          'niwa-pending-inputs-schedule-held-input-delivery)
                         (lambda (&rest _) (cl-incf offers))))
                (niwa--fail-turn owner status)
                (niwa-test--await 5 "Failed settlement did not finish"
                  (and (null niwa--current-request)
                       (= 1 (plist-get (niwa-engine-info owner)
                                       :niwa-settlement-holds))))
                (niwa--turn-release owner)
                (should-not (niwa-turn-busy-p buffer))
                (should (= 0 offers))
                (should (eq status (niwa-session-last-outcome session)))
                (should (= 1 (length (niwa-session-pending-follow-ups session))))))
          (when (buffer-live-p view) (kill-buffer view))))))
  (engine)
  :doc "native failure and abort do not offer queued input after release"
  'native
  :doc "external failure and abort do not offer queued input after release"
  'external)

(provide 'test-niwa-pending-inputs-settlement)
;;; test-niwa-pending-inputs-settlement.el ends here
