;;; test-mevedel-turn-checkpoint-worker.el --- Suspended settlement -*- lexical-binding: t -*-

;;; Commentary:
;; A pending storage step keeps admission held without polling or running hooks.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-turn)
(require 'mevedel-agent-control)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-transcript-restore)

(mevedel-deftest mevedel--defer-turn-steps
  (:vars ((mevedel-transport--enabled-p t)))
  ,test
  (test)
  :doc "a pending step resumes once before later settlement steps"
  (let* ((buffer (generate-new-buffer " *pending-settlement*"))
         (fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-request-id "pending")))
         ready ended resume (calls 0))
    (unwind-protect
        (progn
          (mevedel--defer-turn-steps
           fsm (list (lambda (_)
                       (cl-incf calls)
                       (unless ready
                         (setq resume mevedel--turn-resume)
                         'mevedel-turn-pending))
                     (lambda (_) (setq ended t))))
          (let ((until (+ (float-time) 2)))
            (while (and (not resume) (< (float-time) until)) (accept-process-output nil .01)))
          (should (functionp resume))
          (should-not ended)
          (should (= 1 (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
          (accept-process-output nil .03)
          (should (= calls 1))
          (setq ready t)
          (funcall resume)
          (let ((until (+ (float-time) 2)))
            (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01)))
          (should ended)
          (should (= 2 calls))
          (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
          (funcall resume)
          (accept-process-output nil .02)
          (should (= 2 calls)))
      (when (buffer-live-p buffer) (kill-buffer buffer))))

  :doc "source death cancels pending checkpoint work and rejects a late resume"
  (let* ((buffer (generate-new-buffer " *cancel-pending-settlement*"))
         (fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-request-id "cancel")))
         resume cancelled ended)
    (unwind-protect
        (progn
          (mevedel--defer-turn-steps
           fsm (list (lambda (_)
                       (setq resume mevedel--turn-resume)
                       (setf (gptel-fsm-info fsm)
                             (plist-put (gptel-fsm-info fsm) :mevedel-checkpoint-cancel
                                        (lambda () (setq cancelled t))))
                       'mevedel-turn-pending)
                     (lambda (_) (setq ended t))))
          (let ((until (+ (float-time) 2)))
            (while (and (not resume) (< (float-time) until)) (accept-process-output nil .01)))
          (should resume)
          (kill-buffer buffer)
          (should cancelled)
          (funcall resume)
          (accept-process-output nil .03)
          (should-not ended)
          (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest mevedel-turn-save-acknowledges-after-publication ()
  (let ((mevedel-transport--enabled-p t))
    (mevedel-test-journal-capture--with-portable-session
     (lambda (session buffer)
       (let ((mevedel-journal-enabled nil))
         (mevedel-test-journal-capture--turn session buffer "First prompt" "First answer"))
       (let* ((fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-request-id "save")))
              (finish (symbol-function 'mevedel-session-publication--job-complete))
              ack next-head (until (+ (float-time) 15)))
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert (propertize "\nPublished answer\n" 'gptel 'response))
           (cl-incf (mevedel-session-turn-count session)))
         (cl-letf (((symbol-function 'mevedel-session-publication--job-complete)
                    (lambda (&rest args)
                      (should (= 1 (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
                      (should-not (plist-get (gptel-fsm-info fsm) :mevedel-turn-saved))
                      (setq ack t)
                      (apply finish args))))
           (mevedel--defer-turn-steps
            fsm (list #'mevedel--turn-save
                      (lambda (_)
                        (setq next-head (mevedel-session-durability-publication-head
                                         (mevedel-session-save-path session))))))
           (while (and (not next-head) (< (float-time) until))
             (accept-process-output nil .01)))
         (should ack)
         (should next-head)
         (should (plist-get (gptel-fsm-info fsm) :mevedel-turn-saved))
         (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
         (should (string-search
                  "Published answer"
                  (mevedel-session-artifacts-read-artifact session "segment-0001.chat.org" t))))))))

(ert-deftest mevedel-turn-superseded-save-releases-settlement ()
  (let ((mevedel-transport--enabled-p t))
    (mevedel-test-journal-capture--with-portable-session
     (lambda (session buffer)
       (let ((mevedel-journal-enabled nil))
         (mevedel-test-journal-capture--turn session buffer "First prompt" "First answer"))
       (let ((fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-request-id "superseded")))
             retire cancelled ended (until (+ (float-time) 5)))
         (cl-letf (((symbol-function 'mevedel-session-publication-submit)
                    (lambda (_owner _prepare _ack _mail-ids _retain _select superseded)
                      (setq retire superseded)
                      'pending)))
           (mevedel--defer-turn-steps
            fsm (list #'mevedel--turn-save (lambda (_) (setq ended t)))
            (lambda () (setq cancelled t)))
           (while (and (not retire) (< (float-time) until))
             (accept-process-output nil .01)))
         (should retire)
         (should (= 1 (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
         (funcall retire)
         (should cancelled)
         (should-not ended)
         (should-not (plist-get (gptel-fsm-info fsm) :mevedel-turn-saved))
         (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds))))))))

(ert-deftest mevedel-session-reconcile-only-segments-with-execution-fields ()
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "First prompt" "First answer"))
     (let* ((root (mevedel-session-save-path session))
            (plain (mevedel-session-artifacts-segment-path root 1))
            (running (mevedel-session-artifacts-segment-path root 2))
            (restore (symbol-function 'mevedel-transcript-restore-properties))
            (parses 0) repaired)
       (mevedel-session-publication-publish
        session
        (list (list :path running :coding 'utf-8-unix
                    :content
                    (with-temp-buffer
                      (org-mode)
                      (insert (test-mevedel-session-persistence--execution-tool-block
                               "running" '(:kind bash :state running :live-execution-p t)))
                      (mevedel-session-artifacts-stabilize-gptel-bounds)
                      (buffer-string)))
              (mevedel-session-artifacts--sidecar-publication-artifact session buffer)))
       (setf (mevedel-session-current-segment session) 2)
       (should (string-search "First answer" (mevedel-session-persistence--segment-text session plain)))
       (cl-letf (((symbol-function 'mevedel-transcript-restore-properties)
                  (lambda (&rest args) (cl-incf parses) (apply restore args))))
         (should (= 1 (mevedel-session-persistence-reconcile-lost-execution-segments
                       session nil (lambda (artifact) (setq repaired artifact))))))
       (should (= parses 1))
       (should (equal running (plist-get repaired :path)))
       (should (string-search ":state lost" (plist-get repaired :content)))))))

(mevedel-deftest mevedel--turn-checkpoint
  (:vars ((mevedel-transport--enabled-p t)))
  ,test
  (test)
  :doc "real checkpoint preparation holds admission until evidence is pinned"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "Question" "Completed answer"))
     (let* ((fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-turn-saved t
                                           :mevedel-request-id "checkpoint")))
            ended pinned)
       (mevedel--defer-turn-steps
        fsm (list #'mevedel--turn-checkpoint
                  (lambda (_)
                    (setq pinned (mevedel-journal-pins-present-p (mevedel-session-save-path session))
                          ended t))))
       (let ((until (+ (float-time) 15)))
         (while (and (not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-state))
                     (< (float-time) until))
           (accept-process-output nil .005)))
       (should (eq 'waiting (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-state)))
       (should-not ended)
       (should (= 1 (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
       (let ((until (+ (float-time) 15)))
         (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01)))
       (should ended)
       (should pinned)
       (should-not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-error))
       (should-not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-cancel))
       (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds))))))

  :doc "a debounced agent-state save during preparation still captures the turn silently"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "Question" "Completed answer"))
     (let* ((fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-turn-saved t
                                           :mevedel-request-id "checkpoint")))
            (mevedel-session-persistence-agent-save-debounce .05)
            (head nil) ended warnings)
       (mevedel-test--with-captured-diagnostics warnings
         (mevedel--defer-turn-steps
          fsm (list #'mevedel--turn-checkpoint (lambda (_) (setq ended t))))
         (let ((until (+ (float-time) 15)))
           (while (and (not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-state))
                       (< (float-time) until))
             (accept-process-output nil .005)))
         (should (eq 'waiting (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-state)))
         (setq head (plist-get (mevedel-session-publication session) :head))
         ;; Mailbox consumption schedules this sidecar publication in a
         ;; background-event turn; it lands while the child prepares.
         (mevedel-agent-control--persist-session session)
         (let ((until (+ (float-time) 15)))
           (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01))))
       (should ended)
       (should-not (equal head (plist-get (mevedel-session-publication session) :head)))
       (should-not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-error))
       (should (equal "" warnings))
       (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
       (should (equal '(1) (mapcar (lambda (capture)
                                     (length (plist-get (plist-get capture :metadata) :turn-ids)))
                                   (mevedel-journal-capture-list
                                    (mevedel-session-workspace session)))))))))

;;; test-mevedel-turn-checkpoint-worker.el ends here
