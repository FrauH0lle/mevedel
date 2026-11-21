;;; test-niwa-turn-checkpoint-worker.el --- Suspended settlement -*- lexical-binding: t -*-

;;; Commentary:
;; A pending storage step keeps admission held without polling or running hooks.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-turn)
(require 'niwa-agent-control)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-transcript-restore)

(niwa-deftest niwa--defer-turn-steps
  (:vars ((niwa-transport--enabled-p t)))
  ,test
  (test)
  :doc "a pending step resumes once before later settlement steps"
  (let* ((buffer (generate-new-buffer " *pending-settlement*"))
         (fsm (gptel-make-fsm :info (list :buffer buffer :niwa-request-id "pending")))
         ready ended resume (calls 0))
    (unwind-protect
        (progn
          (niwa--defer-turn-steps
           fsm (list (lambda (_)
                       (cl-incf calls)
                       (unless ready
                         (setq resume niwa--turn-resume)
                         'niwa-turn-pending))
                     (lambda (_) (setq ended t))))
          (let ((until (+ (float-time) 2)))
            (while (and (not resume) (< (float-time) until)) (accept-process-output nil .01)))
          (should (functionp resume))
          (should-not ended)
          (should (= 1 (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds)))
          (accept-process-output nil .03)
          (should (= calls 1))
          (setq ready t)
          (funcall resume)
          (let ((until (+ (float-time) 2)))
            (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01)))
          (should ended)
          (should (= 2 calls))
          (should (zerop (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds)))
          (funcall resume)
          (accept-process-output nil .02)
          (should (= 2 calls)))
      (when (buffer-live-p buffer) (kill-buffer buffer))))

  :doc "source death cancels pending checkpoint work and rejects a late resume"
  (let* ((buffer (generate-new-buffer " *cancel-pending-settlement*"))
         (fsm (gptel-make-fsm :info (list :buffer buffer :niwa-request-id "cancel")))
         resume cancelled ended)
    (unwind-protect
        (progn
          (niwa--defer-turn-steps
           fsm (list (lambda (_)
                       (setq resume niwa--turn-resume)
                       (setf (gptel-fsm-info fsm)
                             (plist-put (gptel-fsm-info fsm) :niwa-checkpoint-cancel
                                        (lambda () (setq cancelled t))))
                       'niwa-turn-pending)
                     (lambda (_) (setq ended t))))
          (let ((until (+ (float-time) 2)))
            (while (and (not resume) (< (float-time) until)) (accept-process-output nil .01)))
          (should resume)
          (kill-buffer buffer)
          (should cancelled)
          (funcall resume)
          (accept-process-output nil .03)
          (should-not ended)
          (should (zerop (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest niwa-turn-save-acknowledges-after-publication ()
  (let ((niwa-transport--enabled-p t))
    (niwa-test-journal-capture--with-portable-session
     (lambda (session buffer)
       (let ((niwa-journal-enabled nil))
         (niwa-test-journal-capture--turn session buffer "First prompt" "First answer"))
       (let* ((fsm (gptel-make-fsm :info (list :buffer buffer :niwa-request-id "save")))
              (finish (symbol-function 'niwa-session-publication--job-complete))
              ack next-head (until (+ (float-time) 15)))
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert (propertize "\nPublished answer\n" 'gptel 'response))
           (cl-incf (niwa-session-turn-count session)))
         (cl-letf (((symbol-function 'niwa-session-publication--job-complete)
                    (lambda (&rest args)
                      (should (= 1 (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds)))
                      (should-not (plist-get (gptel-fsm-info fsm) :niwa-turn-saved))
                      (setq ack t)
                      (apply finish args))))
           (niwa--defer-turn-steps
            fsm (list #'niwa--turn-save
                      (lambda (_)
                        (setq next-head (niwa-session-durability-publication-head
                                         (niwa-session-save-path session))))))
           (while (and (not next-head) (< (float-time) until))
             (accept-process-output nil .01)))
         (should ack)
         (should next-head)
         (should (plist-get (gptel-fsm-info fsm) :niwa-turn-saved))
         (should (zerop (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds)))
         (should (string-search
                  "Published answer"
                  (niwa-session-artifacts-read-artifact session "segment-0001.chat.org" t))))))))

(ert-deftest niwa-turn-superseded-save-releases-settlement ()
  (let ((niwa-transport--enabled-p t))
    (niwa-test-journal-capture--with-portable-session
     (lambda (session buffer)
       (let ((niwa-journal-enabled nil))
         (niwa-test-journal-capture--turn session buffer "First prompt" "First answer"))
       (let ((fsm (gptel-make-fsm :info (list :buffer buffer :niwa-request-id "superseded")))
             retire cancelled ended (until (+ (float-time) 5)))
         (cl-letf (((symbol-function 'niwa-session-publication-submit)
                    (lambda (_owner _prepare _ack _mail-ids _retain _select superseded)
                      (setq retire superseded)
                      'pending)))
           (niwa--defer-turn-steps
            fsm (list #'niwa--turn-save (lambda (_) (setq ended t)))
            (lambda () (setq cancelled t)))
           (while (and (not retire) (< (float-time) until))
             (accept-process-output nil .01)))
         (should retire)
         (should (= 1 (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds)))
         (funcall retire)
         (should cancelled)
         (should-not ended)
         (should-not (plist-get (gptel-fsm-info fsm) :niwa-turn-saved))
         (should (zerop (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds))))))))

(ert-deftest niwa-session-reconcile-only-segments-with-execution-fields ()
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "First prompt" "First answer"))
     (let* ((root (niwa-session-save-path session))
            (plain (niwa-session-artifacts-segment-path root 1))
            (running (niwa-session-artifacts-segment-path root 2))
            (restore (symbol-function 'niwa-transcript-restore-properties))
            (parses 0) repaired)
       (niwa-session-publication-publish
        session
        (list (list :path running :coding 'utf-8-unix
                    :content
                    (with-temp-buffer
                      (org-mode)
                      (insert (test-niwa-session-persistence--execution-tool-block
                               "running" '(:kind bash :state running :live-execution-p t)))
                      (niwa-session-artifacts-stabilize-gptel-bounds)
                      (buffer-string)))
              (niwa-session-artifacts--sidecar-publication-artifact session buffer)))
       (setf (niwa-session-current-segment session) 2)
       (should (string-search "First answer" (niwa-session-persistence--segment-text session plain)))
       (cl-letf (((symbol-function 'niwa-transcript-restore-properties)
                  (lambda (&rest args) (cl-incf parses) (apply restore args))))
         (should (= 1 (niwa-session-persistence-reconcile-lost-execution-segments
                       session nil (lambda (artifact) (setq repaired artifact))))))
       (should (= parses 1))
       (should (equal running (plist-get repaired :path)))
       (should (string-search ":state lost" (plist-get repaired :content)))))))

(niwa-deftest niwa--turn-checkpoint
  (:vars ((niwa-transport--enabled-p t)))
  ,test
  (test)
  :doc "real checkpoint preparation holds admission until evidence is pinned"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Question" "Completed answer"))
     (let* ((fsm (gptel-make-fsm :info (list :buffer buffer :niwa-turn-saved t
                                           :niwa-request-id "checkpoint")))
            ended pinned)
       (niwa--defer-turn-steps
        fsm (list #'niwa--turn-checkpoint
                  (lambda (_)
                    (setq pinned (niwa-journal-pins-present-p (niwa-session-save-path session))
                          ended t))))
       (let ((until (+ (float-time) 15)))
         (while (and (not (plist-get (gptel-fsm-info fsm) :niwa-checkpoint-state))
                     (< (float-time) until))
           (accept-process-output nil .005)))
       (should (eq 'waiting (plist-get (gptel-fsm-info fsm) :niwa-checkpoint-state)))
       (should-not ended)
       (should (= 1 (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds)))
       (let ((until (+ (float-time) 15)))
         (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01)))
       (should ended)
       (should pinned)
       (should-not (plist-get (gptel-fsm-info fsm) :niwa-checkpoint-error))
       (should-not (plist-get (gptel-fsm-info fsm) :niwa-checkpoint-cancel))
       (should (zerop (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds))))))

  :doc "a debounced agent-state save during preparation still captures the turn silently"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Question" "Completed answer"))
     (let* ((fsm (gptel-make-fsm :info (list :buffer buffer :niwa-turn-saved t
                                           :niwa-request-id "checkpoint")))
            (niwa-session-persistence-agent-save-debounce .05)
            (head nil) ended warnings)
       (niwa-test--with-captured-diagnostics warnings
         (niwa--defer-turn-steps
          fsm (list #'niwa--turn-checkpoint (lambda (_) (setq ended t))))
         (let ((until (+ (float-time) 15)))
           (while (and (not (plist-get (gptel-fsm-info fsm) :niwa-checkpoint-state))
                       (< (float-time) until))
             (accept-process-output nil .005)))
         (should (eq 'waiting (plist-get (gptel-fsm-info fsm) :niwa-checkpoint-state)))
         (setq head (plist-get (niwa-session-publication session) :head))
         ;; Mailbox consumption schedules this sidecar publication in a
         ;; background-event turn; it lands while the child prepares.
         (niwa-agent-control--persist-session session)
         (let ((until (+ (float-time) 15)))
           (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01))))
       (should ended)
       (should-not (equal head (plist-get (niwa-session-publication session) :head)))
       (should-not (plist-get (gptel-fsm-info fsm) :niwa-checkpoint-error))
       (should (equal "" warnings))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
       (should (equal '(1) (mapcar (lambda (capture)
                                     (length (plist-get (plist-get capture :metadata) :turn-ids)))
                                   (niwa-journal-capture-list
                                    (niwa-session-workspace session)))))))))

;;; test-niwa-turn-checkpoint-worker.el ends here
