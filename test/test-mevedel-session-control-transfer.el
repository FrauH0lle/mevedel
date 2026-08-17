;;; test-mevedel-session-control-transfer.el --- Transfer coordinator tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests session-owned transfer decisions without constructing a view.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-execution)
(require 'mevedel-session-control-transfer)
;; Loaded up front because the coordinator requires them lazily inside the
;; functions under test, which would otherwise redefine stubbed symbols
;; mid-test.
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-structs)

(require 'mevedel-transport)

(mevedel-deftest mevedel-session-control-transfer-drain-registry ()
  ,test
  (test)
  :doc "combines session drains and safely unregisters transient owners"
  (let ((session (mevedel-session--create :name "transfer"))
        (pending t))
    (should (mevedel-session-control-transfer-drained-p session))
    (let ((predicate (lambda () pending)))
      (should (eq predicate
                  (mevedel-session-control-transfer-register-drain
                   session predicate)))
      (should-not (mevedel-session-control-transfer-drained-p session))
      (setq pending nil)
      (should (mevedel-session-control-transfer-drained-p session))
      (mevedel-session-control-transfer-unregister-drain session predicate)
      (should-not (mevedel-session-control-transfer-drains session)))))

(mevedel-deftest mevedel-session-control-transfer-descriptor ()
  ,test
  (test)
  :doc "publishes semantic transfer actions without view presentation state"
  (let ((session (mevedel-session--create :name "transfer")))
    (setf (mevedel-session-control-transfer session)
          '(:state requested
            :request (:requester-label "/worker")))
    (should
     (equal
      '(:kind control-transfer
        :action grant
        :body "Control transfer requested by /worker  [g]rant  [k]eep"
        :help-echo "Grant or keep the current lease")
      (mevedel-session-control-transfer-descriptor session nil)))
    ;; The requester is in the same durable state as the owner and has
    ;; nothing to grant; showing it the owner's prompt offers a decision it
    ;; cannot make.
    (should
     (eq 'requested
         (plist-get (mevedel-session-control-transfer-descriptor session t)
                    :action)))
    (setf (mevedel-session-control-transfer session)
          '(:state quiescing :request (:requester-label "/worker")))
    (should
     (string-match-p
      "finishing"
      (plist-get (mevedel-session-control-transfer-descriptor session nil)
                 :body)))
    (should
     (string-match-p
      "waiting for the owner"
      (plist-get (mevedel-session-control-transfer-descriptor session t)
                 :body)))
    (setf (mevedel-session-control-transfer session) '(:state released))
    (should
     (equal
      '(:kind control-transfer
        :action request
        :body "Session is read-only  [r]equest control"
        :help-echo "Request cooperative control")
      (mevedel-session-control-transfer-descriptor session t)))
    ;; A writable session with no request has nothing to say at all.
    (should-not
     (mevedel-session-control-transfer-descriptor session nil))))

(mevedel-deftest mevedel-session-control-transfer-drain-blocker ()
  ,test
  (test)
  :doc "names the first thing holding a handoff open, and nothing when clear"
  (let ((session (mevedel-session--create :name "transfer")))
    (should-not (mevedel-session-control-transfer-drain-blocker session))
    (let ((predicate (lambda () t)))
      (mevedel-session-control-transfer-register-drain session predicate)
      (should (equal "the view"
                     (mevedel-session-control-transfer-drain-blocker
                      session)))
      (mevedel-session-control-transfer-unregister-drain
       session predicate))
    (setf (mevedel-session-pending-publication session) t)
    (should (equal "a publication"
                   (mevedel-session-control-transfer-drain-blocker session)))
    (cl-letf (((symbol-function 'mevedel-execution-session-live-p)
               (lambda (_session) t)))
      ;; A live execution outranks a queued publication: it is the wait the
      ;; user can actually end.
      (should (equal "a live execution"
                     (mevedel-session-control-transfer-drain-blocker
                      session))))))

(mevedel-deftest mevedel-session-control-transfer--follow-published ()
  ,test
  (test)
  :doc "advances a non-owner buffer only when the owner published something new"
  (let* ((session (mevedel-session--create :name "follow"))
         (buffer (generate-new-buffer " *mevedel-follow*"))
         (head "generation-old")
         (inserts 0))
    (setf (mevedel-session-save-path session) "/session/"
          (mevedel-session-publication session) '(:head "generation-old"))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-durability-publication-head)
              (lambda (&rest _) head))
             ((symbol-function 'mevedel-session-publication-read)
              (lambda (&rest _) (list :head head :sidecar "/sidecar")))
             ((symbol-function 'mevedel-session-persistence-load-sidecar)
              (lambda (&rest _) '(:version 1)))
             ((symbol-function 'mevedel-session-persistence-deserialize)
              (lambda (&rest _)
                (list :session (mevedel-session--create :name "follow"))))
             ((symbol-function
               'mevedel-session-control-transfer--insert-committed-segment)
              (lambda (&rest _) (cl-incf inserts))))
          ;; The head names the owner's committed generation, so an owner
          ;; that has published nothing new costs one observation and no
          ;; artifact reads at all.
          (should-not
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 0 inserts))
          (setq head "generation-new")
          (should
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 1 inserts))
          (should (equal "generation-new"
                         (plist-get (mevedel-session-publication session)
                                    :head)))
          (should (equal "/session/" (mevedel-session-save-path session)))
          ;; Advancing again needs a further publication, not another tick.
          (should-not
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 1 inserts))
          ;; Local edits are what the transfer path refuses to discard; a
          ;; timer must not resolve that conflict on the user's behalf.
          (setq head "generation-newer")
          (with-current-buffer buffer (insert "local edit"))
          (should (buffer-modified-p buffer))
          (should-not
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 1 inserts))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          ;; Following off holds the buffer where it is, but an explicit
          ;; refresh still reads.
          (with-current-buffer buffer
            (setq-local mevedel-session-follow-published nil))
          (should-not
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 1 inserts))
          (should
           (mevedel-session-control-transfer--follow-published
            session buffer t))
          (should (= 2 inserts)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-session-control-transfer-poll ()
  ,test
  (test)
  :doc "defers target I/O while the transport or a publication is busy"
  (let ((session (mevedel-session--create :name "transfer"))
        (owner 0)
        (requester 0))
    (cl-letf (((symbol-function
                'mevedel-session-control-transfer--poll-owner)
               (lambda (_session) (cl-incf owner) 'owner))
              ((symbol-function
                'mevedel-session-control-transfer--poll-requester)
               (lambda (_session _buffer) (cl-incf requester) 'requester)))
      (should (eq 'owner
                  (mevedel-session-control-transfer-poll session nil nil)))
      (should (eq 'requester
                  (mevedel-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      ;; Emacs runs filters and timers wherever the main loop waits,
      ;; including inside a TRAMP operation; polling from there wedges the
      ;; operation already running.
      (cl-letf (((symbol-function 'mevedel-transport-busy-p)
                 (lambda (&optional _path) t)))
        (should-not
         (mevedel-session-control-transfer-poll session nil nil))
        (should-not
         (mevedel-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (mevedel-session-publication-active-p session) t)
      (should-not (mevedel-session-control-transfer-poll session nil nil))
      (should-not (mevedel-session-control-transfer-poll session nil t))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (mevedel-session-publication-active-p session) nil)
      (should (eq 'owner
                  (mevedel-session-control-transfer-poll session nil nil)))
      (should (= 2 owner)))
    ;; A non-owner that did not acquire control still follows: the same tick
    ;; is what advances a joined client through the owner's published turns.
    (cl-letf (((symbol-function
                'mevedel-session-control-transfer--poll-requester)
               (lambda (&rest _) nil))
              ((symbol-function
                'mevedel-session-control-transfer--follow-published)
               (lambda (&rest _) 'followed)))
      (should (eq 'followed
                  (mevedel-session-control-transfer-poll session nil t))))))

(provide 'test-mevedel-session-control-transfer)

;;; test-mevedel-session-control-transfer.el ends here
