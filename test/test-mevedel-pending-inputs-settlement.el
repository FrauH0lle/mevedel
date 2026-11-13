;;; test-mevedel-pending-inputs-settlement.el --- Replies during cleanup -*- lexical-binding: t -*-

;;; Commentary:
;; Hold real terminal settlement across composer submission and an early drain.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-pending-inputs)
(require 'mevedel-presets)
(require 'mevedel-turn)
(require 'mevedel-view-composer)

(mevedel-deftest mevedel-view-send/terminal-settlement (:quiet t)
  (dolist (stage '(checkpoint torn-down))
    (mevedel-engine-test--with-session :file-workspace t
      (let* ((gptel--known-backends nil)
             (view (generate-new-buffer " *settlement-reply-view*"))
             (reply "> 2. **ADR 0091:** may it get that one pointer edit?\nYes")
             (owner (if (eq ,engine 'native)
                        (gptel-make-fsm
                         :state 'DONE
                         :info (list :buffer buffer
                                     :mevedel-request-id (mevedel-request-id request)
                                     :mevedel-request request))
                      request))
             checkpoint-ready resume admitted)
        (unwind-protect
            (progn
              (if (eq ,engine 'native)
                  (setf (mevedel-request-fsm request) owner)
                (setf (mevedel-engine-info owner)
                      (list :buffer buffer
                            :mevedel-request-id (mevedel-request-id request)
                            :mevedel-request request
                            :external-history t :mevedel-acp-outcome 'success)))
              (gptel-make-openai "Reply" :key "test" :models '(reply)
                                 :protocol "http" :host "127.0.0.1:1")
              (mevedel-model-set-session-provider
               session (mevedel-model-resolve-provider "Reply:reply") buffer)
              (setq-local gptel-system-prompt "Settlement reply fixture"
                          gptel-tools nil gptel-use-curl nil gptel-stream nil
                          gptel-prompt-transform-functions
                          '(mevedel-skills--transform-apply-request-model-policy)
                          gptel-send--handlers
                          (mevedel-preset--build-handlers
                           (copy-tree (default-value 'gptel-send--handlers))))
              (mevedel-chat-install-request-hooks)
              (mevedel-view--setup view buffer)
              ;; Keep admission fenced after the ordinary chain ends, so its
              ;; first drain necessarily runs too early.
              (mevedel--turn-hold owner)
              (cl-letf (((symbol-function 'mevedel--turn-checkpoint)
                         (lambda (machine)
                           (when (and (eq machine owner) (not checkpoint-ready))
                             (setq resume mevedel--turn-resume)
                             'mevedel-turn-pending)))
                        ((symbol-function 'gptel--url-get-response)
                         (lambda (fsm) (push fsm admitted)))
                        ((symbol-function 'gptel-curl-get-response)
                         (lambda (fsm) (push fsm admitted))))
                (mevedel--complete-turn owner)
                (mevedel-test--await 5 "Settlement did not reach checkpoint"
                  resume)
                (when (eq stage 'torn-down)
                  (setq checkpoint-ready t)
                  (funcall resume)
                  (mevedel-test--await 5 "Request teardown did not finish"
                    (null mevedel--current-request)))
                (with-current-buffer view
                  (goto-char (mevedel-view--input-start))
                  (insert reply)
                  (mevedel-view-send)
                  (should (string-empty-p (mevedel-view--input-text))))
                (should (= 1 (length (mevedel-session-pending-follow-ups session))))
                (should-not (mevedel-session-pending-steering session))
                (when (eq stage 'checkpoint)
                  (should (eq request mevedel--current-request))
                  (setq checkpoint-ready t)
                  (funcall resume)
                  (mevedel-test--await 5 "Request teardown did not finish"
                    (null mevedel--current-request)))
                (mevedel-test--await 5 "Settlement chain did not release its hold"
                  (= 1 (plist-get (mevedel-engine-info owner)
                                  :mevedel-settlement-holds)))
                (should (mevedel-turn-busy-p buffer))
                (mevedel-view--drain-follow-up buffer)
                (should-not admitted)
                (should (= 1 (length (mevedel-session-pending-follow-ups session))))
                (mevedel--turn-release owner)
                (should-not (mevedel-turn-busy-p buffer))
                (should-not (mevedel--turn-publication-pending-p owner))
                (should-not (mevedel-view--follow-up-auto-drain-blocked-p session))
                (should (eq 'follow-up (mevedel-pending-inputs--idle-work buffer)))
                (mevedel-test--await 5 "Reply was stranded after final hold release"
                  admitted)
                (should (= 1 (length admitted)))
                (should-not (mevedel-session-pending-follow-ups session))
                (should-not (eq request mevedel--current-request))
                (should (= 1 (how-many (regexp-quote reply) (point-min) (point-max))))
                ;; Re-offering cannot deliver the reply a second time.
                (mevedel-view--drain-follow-up buffer)
                (should (= 1 (length admitted)))
                (gptel--fsm-transition (car admitted) 'DONE)
                (mevedel-test--await 5 "Reply did not finish settlement"
                  (not (mevedel-turn-busy-p buffer)))))
          (when (buffer-live-p view) (kill-buffer view))))))
  (engine)
  :doc "native terminal replies survive checkpoint and post-teardown holds"
  'native
  :doc "external terminal replies survive checkpoint and post-teardown holds"
  'external)

(mevedel-deftest mevedel--turn-release/failed-settlement (:quiet t)
  (dolist (status '(error aborted))
    (mevedel-engine-test--with-session :file-workspace t
      (let* ((view (generate-new-buffer " *failed-settlement-view*"))
             (info (list :buffer buffer
                         :mevedel-request-id (mevedel-request-id request)
                         :mevedel-request request))
             (owner (if (eq ,engine 'native)
                        (gptel-make-fsm :state 'ERRS :info info)
                      request))
             (offers 0))
        (unwind-protect
            (progn
              (if (eq ,engine 'native)
                  (setf (mevedel-request-fsm request) owner)
                (setf (mevedel-engine-info owner)
                      (append info (list :external-history t
                                         :mevedel-acp-outcome status))))
              (mevedel-view--setup view buffer)
              (mevedel-session-enqueue-pending-input
               session 'follow-up '(:input "next reply"))
              (mevedel--turn-hold owner)
              (cl-letf (((symbol-function
                          'mevedel-pending-inputs-schedule-held-input-delivery)
                         (lambda (&rest _) (cl-incf offers))))
                (mevedel--fail-turn owner status)
                (mevedel-test--await 5 "Failed settlement did not finish"
                  (and (null mevedel--current-request)
                       (= 1 (plist-get (mevedel-engine-info owner)
                                       :mevedel-settlement-holds))))
                (mevedel--turn-release owner)
                (should-not (mevedel-turn-busy-p buffer))
                (should (= 0 offers))
                (should (eq status (mevedel-session-last-outcome session)))
                (should (= 1 (length (mevedel-session-pending-follow-ups session))))))
          (when (buffer-live-p view) (kill-buffer view))))))
  (engine)
  :doc "native failure and abort do not offer queued input after release"
  'native
  :doc "external failure and abort do not offer queued input after release"
  'external)

(provide 'test-mevedel-pending-inputs-settlement)
;;; test-mevedel-pending-inputs-settlement.el ends here
