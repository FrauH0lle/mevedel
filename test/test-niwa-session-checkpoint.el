;;; test-niwa-session-checkpoint.el --- Committed checkpoint tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the owner API without publisher I/O: failed/deferred writes do not
;; commit, and installing one frozen conversation cannot retire another's mail.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-checkpoint)

(defun test-niwa-session-checkpoint--root (turn &optional segment)
  "Return root fields describing TURN in SEGMENT."
  (list :current-segment (or segment 1)
        :total-turn-count turn
        :prompt-index (list (cons (or segment 1)
                                 (list (list :turn turn :text "prompt"))))
        :file-snapshots (list (cons turn '(("file" . "snapshot"))))
        :chapters (list (list :segment 1 :title "chapter"))
        :first-user-message (copy-sequence "first")
        :latest-user-message (format "turn %s" turn)))

(defun test-niwa-session-checkpoint--mail (id &optional recipient)
  "Return a durable mailbox record with ID addressed to RECIPIENT."
  (list :id id :type 'MAIL :sender "/root/sender"
        :recipient (or recipient "/root") :payload (concat "message " id)
        :timestamp 1))

(defun test-niwa-session-checkpoint--ids (records)
  "Return the durable identities of RECORDS."
  (mapcar (lambda (record) (plist-get record :id)) records))

(niwa-deftest niwa-session-checkpoint-seed ()
  ,test
  (test)
  :doc "starts empty rather than trusting live state before first publication"
  (let* ((session (niwa-session--create
                   :current-segment 9 :turn-count 12
                   :prompt-index '((9 (:text "unpublished")))))
         (projected (niwa-session-checkpoint-project
                     session (test-niwa-session-checkpoint--root 12 9))))
    (should (= 1 (plist-get projected :current-segment)))
    (should (= 0 (plist-get projected :total-turn-count)))
    (should-not (plist-get projected :prompt-index)))
  :doc "detaches all seeded root fields, including strings, from decoded state"
  (let* ((session (niwa-session--create))
         (sidecar (test-niwa-session-checkpoint--root 3 2))
         (before (niwa-session-checkpoint--copy sidecar)))
    (niwa-session-checkpoint-seed session sidecar)
    (aset (plist-get sidecar :first-user-message) 0 ?X)
    (setf (plist-get sidecar :total-turn-count) 99)
    (let ((projected (niwa-session-checkpoint-project session nil)))
      (dolist (key niwa-session-checkpoint--root-keys)
        (should (equal (plist-get before key) (plist-get projected key))))))
  :doc "copies long histories without consuming a Lisp frame per prompt"
  (let* ((session (niwa-session--create))
         (prompts (cl-loop for turn from 1 to 3000
                           collect (list :turn turn :text "prompt")))
         (sidecar (list :prompt-index (list (cons 1 prompts)))))
    (niwa-session-checkpoint-seed session sidecar)
    (should (= 3000
               (length
                (cdar (plist-get
                       (niwa-session-checkpoint-project session nil)
                       :prompt-index)))))))

(niwa-deftest niwa-session-checkpoint-first-user-message ()
  ,test
  (test)
  :doc "reads only the committed preview and cannot mutate owner state"
  (let ((session (niwa-session--create)))
    (should-not (niwa-session-checkpoint-first-user-message session))
    (niwa-session-checkpoint-seed
     session (list :first-user-message (copy-sequence "first")))
    (let ((preview (niwa-session-checkpoint-first-user-message session))
          (candidate (niwa-session-checkpoint-capture
                      session "/root" '(:first-user-message "next"))))
      (aset preview 0 ?X)
      (should (equal "first" (niwa-session-checkpoint-first-user-message session)))
      (niwa-session-checkpoint-install session candidate)
      (should (equal "next" (niwa-session-checkpoint-first-user-message session))))))

(niwa-deftest niwa-session-checkpoint-consume ()
  ,test
  (test)
  :doc "keeps distinct identical mail and deduplicates durable identities"
  (let* ((session (niwa-session--create))
         (old (test-niwa-session-checkpoint--mail "old"))
         (new (test-niwa-session-checkpoint--mail "new")))
    (setf (plist-get old :payload) (copy-sequence "same")
          (plist-get new :payload) (copy-sequence "same"))
    (niwa-session-checkpoint-consume session "/root" (list old))
    (niwa-session-checkpoint-consume session "/root" (list new old))
    (aset (plist-get old :payload) 0 ?X)
    (let* ((raw (list :messages (list new)))
           (projected (niwa-session-checkpoint-project session raw))
           (mail (plist-get projected :messages)))
      (should (equal '("new" "old")
                     (test-niwa-session-checkpoint--ids mail)))
      (should (equal "same" (plist-get (cadr mail) :payload)))
      (should (equal (list new) (plist-get raw :messages)))))
  :doc "keeps durable identity but removes runtime-only fields from retained mail"
  (let* ((session (niwa-session--create))
         (record (append (test-niwa-session-checkpoint--mail "runtime")
                         '(:wake t :usage (:tokens 50)))))
    (niwa-session-checkpoint-consume session "/root" (list record))
    (let ((persisted (car (plist-get
                          (niwa-session-checkpoint-project session nil)
                          :messages))))
      (should (equal "runtime" (plist-get persisted :id)))
      (should-not (plist-member persisted :wake))
      (should-not (plist-member persisted :usage))))
  :doc "rejects missing identities and cross-conversation consumption"
  (let ((session (niwa-session--create)))
    (should-error (niwa-session-checkpoint-consume
                   session "/root" '((:payload "missing id"))))
    (should-error (niwa-session-checkpoint-consume
                   session "root" nil))
    (should-error (niwa-session-checkpoint-consume
                   session "/root/agent"
                   (list (test-niwa-session-checkpoint--mail "wrong"))))
    (should-not (plist-get
                 (niwa-session-checkpoint-project session nil) :messages))))

(niwa-deftest niwa-session-checkpoint-capture ()
  ,test
  (test)
  :doc "freezes exact IDs before nested consumption and later root completion"
  (let* ((session (niwa-session--create))
         (old-root (test-niwa-session-checkpoint--root 1))
         (new-root (test-niwa-session-checkpoint--root 2))
         (one (test-niwa-session-checkpoint--mail "one"))
         (two (test-niwa-session-checkpoint--mail "two")))
    (niwa-session-checkpoint-seed session old-root)
    (niwa-session-checkpoint-consume session "/root" (list one))
    (let* ((frozen (niwa-session-checkpoint-capture session "/root"))
           (candidate (niwa-session-checkpoint-with-root frozen new-root)))
      (niwa-session-checkpoint-consume session "/root" (list two))
      (setf (plist-get new-root :total-turn-count) 99)
      (let ((ordinary (niwa-session-checkpoint-project session new-root))
            (owned (niwa-session-checkpoint-project session new-root candidate)))
        (should (= 1 (plist-get ordinary :total-turn-count)))
        (should (equal '("two" "one")
                       (test-niwa-session-checkpoint--ids
                        (plist-get ordinary :messages))))
        (should (= 2 (plist-get owned :total-turn-count)))
        (should (equal '("two")
                       (test-niwa-session-checkpoint--ids
                        (plist-get owned :messages)))))
      ;; Merely serializing the candidate above did not install it.
      (should (= 1 (plist-get
                    (niwa-session-checkpoint-project session nil)
                    :total-turn-count)))
      (should (niwa-session-checkpoint-install session candidate))
      (should (niwa-session-checkpoint-install session candidate))
      (let ((committed (niwa-session-checkpoint-project session nil)))
        (should (= 2 (plist-get committed :total-turn-count)))
        (should (equal '("two")
                       (test-niwa-session-checkpoint--ids
                        (plist-get committed :messages))))))))

(niwa-deftest niwa-session-checkpoint-project ()
  ,test
  (test)
  :doc "an agent transcript retires only its own consumption, not live root fields"
  (let* ((session (niwa-session--create))
         (root-mail (test-niwa-session-checkpoint--mail "root"))
         (agent-mail (test-niwa-session-checkpoint--mail "agent" "/root/a"))
         (other-mail (test-niwa-session-checkpoint--mail "other" "/root/b"))
         (raw (append (test-niwa-session-checkpoint--root 8 4)
                      (list :session-name "new metadata"
                            :agent-registry
                            '((:path "/root/a" :activity idle :mailbox nil)
                              (:path "/root/b" :activity running :mailbox nil))))))
    (niwa-session-checkpoint-seed
     session (test-niwa-session-checkpoint--root 2))
    (niwa-session-checkpoint-consume session "/root" (list root-mail))
    (niwa-session-checkpoint-consume session "/root/a" (list agent-mail))
    (niwa-session-checkpoint-consume session "/root/b" (list other-mail))
    (let* ((candidate (niwa-session-checkpoint-capture session "/root/a"))
           (projected (niwa-session-checkpoint-project session raw candidate))
           (registry (plist-get projected :agent-registry)))
      (should (= 2 (plist-get projected :total-turn-count)))
      (should (equal "new metadata" (plist-get projected :session-name)))
      (should (equal (list root-mail) (plist-get projected :messages)))
      (should-not (plist-get (car registry) :mailbox))
      (should (equal (list other-mail) (plist-get (cadr registry) :mailbox)))
      (should-not (plist-get (cadr (plist-get raw :agent-registry)) :mailbox))
      (niwa-session-checkpoint-install session candidate)
      (let ((ordinary (niwa-session-checkpoint-project session raw)))
        (should-not (plist-get (car (plist-get ordinary :agent-registry)) :mailbox))
        (should (equal (list root-mail) (plist-get ordinary :messages))))))
  :doc "live Goal, Plan and native conversations survive metadata and unrelated-agent saves"
  (let* ((session (niwa-session--create))
         (committed (test-niwa-session-checkpoint--root 3 2))
         (raw (append
               (test-niwa-session-checkpoint--root 8 4)
               '(:goal (:id "goal" :status active :turns-run 5)
                 :plan-metadata (:status approved :plan-path "local/plans/accepted.md")
                 :external-conversations
                 (("/root" :engine claude-code :id "native-current" :state ready))))))
    (niwa-session-checkpoint-seed session committed)
    (dolist (candidate (list nil (niwa-session-checkpoint-capture session "/root/other")))
      (let ((projected (niwa-session-checkpoint-project session raw candidate)))
        (dolist (key niwa-session-checkpoint--root-keys)
          (should (equal (plist-get committed key) (plist-get projected key))))
        (dolist (key '(:goal :plan-metadata :external-conversations))
          (should (equal (plist-get raw key) (plist-get projected key))))))))

(niwa-deftest niwa-session-checkpoint-pending-message-p ()
  ,test
  (test)
  :doc "a session with no checkpoint state has no pending message and stays uninitialized"
  (let ((session (niwa-session--create)))
    (should-not
     (niwa-session-checkpoint-pending-message-p session "/root" "m1"))
    (should-not (niwa-session-checkpoint-state session)))
  :doc "a consumed message is pending for its conversation only"
  (let ((session (niwa-session--create)))
    (niwa-session-checkpoint-consume
     session "/root" (list (test-niwa-session-checkpoint--mail "m1")))
    (should (niwa-session-checkpoint-pending-message-p session "/root" "m1"))
    (should-not
     (niwa-session-checkpoint-pending-message-p session "/root" "m2"))
    (should-not
     (niwa-session-checkpoint-pending-message-p session "/root/a" "m1"))
    (should-not
     (niwa-session-checkpoint-pending-message-p session "/root" nil)))
  :doc "an installed candidate retires the message unless a publication proof remains"
  (let ((session (niwa-session--create
                                          :publication-active-p t)))
    (niwa-session-checkpoint-consume
     session "/root" (list (test-niwa-session-checkpoint--mail "m1")))
    (niwa-session-checkpoint-install
     session (niwa-session-checkpoint-capture session "/root"))
    ;; Retired from the ledger but proven retired for stale sidecars.
    (should (niwa-session-checkpoint-pending-message-p
             session "/root" "m1"))
    (setf (niwa-session-publication-active-p session) nil)
    (should (niwa-session-checkpoint-release-proofs session))
    (should-not (niwa-session-checkpoint-pending-message-p
                 session "/root" "m1")))
  :doc "an installed candidate with no outstanding publication retires the message"
  (let ((session (niwa-session--create)))
    (niwa-session-checkpoint-consume
     session "/root" (list (test-niwa-session-checkpoint--mail "m1")))
    (niwa-session-checkpoint-install
     session (niwa-session-checkpoint-capture session "/root"))
    (should-not (niwa-session-checkpoint-pending-message-p
                 session "/root" "m1")))
  :doc "queued preparations retain retirement proofs until their drain ends"
  (let ((session (niwa-session--create
                  :publication-preparations (list (cons #'ignore nil)))))
    (niwa-session-checkpoint-consume
     session "/root" (list (test-niwa-session-checkpoint--mail "m1")))
    (niwa-session-checkpoint-install
     session (niwa-session-checkpoint-capture session "/root"))
    (should-not (niwa-session-checkpoint-release-proofs session))
    (should (niwa-session-checkpoint-pending-message-p session "/root" "m1"))
    (setf (niwa-session-publication-preparations session) nil)
    (should (niwa-session-checkpoint-release-proofs session))
    (should-not (niwa-session-checkpoint-pending-message-p session "/root" "m1"))))

(niwa-deftest niwa-session-checkpoint-defer-ptc ()
  ,test
  (test)
  :doc "keeps deferred rows until matching commit and preserves post-freeze rows"
  (let* ((session (niwa-session--create))
         (root-row '(:id "root-tool" :state settled :result "root"))
         (agent-row '(:id "agent-tool" :state settled :result "agent"))
         (later-row '(:id "later-tool" :state settled :result "later"))
         (running-row '(:id "running-tool" :state running)))
    (niwa-session-checkpoint-defer-ptc session "/root" (list root-row))
    (niwa-session-checkpoint-defer-ptc session "/root/a" (list agent-row))
    (let ((candidate (niwa-session-checkpoint-capture session "/root/a")))
      (niwa-session-checkpoint-defer-ptc session "/root/a" (list later-row))
      (let* ((raw (list :ptc-checkpoints (list running-row)))
             (ordinary (niwa-session-checkpoint-project session raw))
             (owned (niwa-session-checkpoint-project session raw candidate)))
        (should (equal '("running-tool" "later-tool" "agent-tool" "root-tool")
                       (test-niwa-session-checkpoint--ids
                        (plist-get ordinary :ptc-checkpoints))))
        (should (equal '("running-tool" "later-tool" "root-tool")
                       (test-niwa-session-checkpoint--ids
                        (plist-get owned :ptc-checkpoints))))
        (niwa-session-checkpoint-install session candidate)
        (should (equal (plist-get owned :ptc-checkpoints)
                       (plist-get (niwa-session-checkpoint-project session raw)
                                  :ptc-checkpoints)))))))

(niwa-deftest niwa-session-checkpoint-valid-p ()
  ,test
  (test)
  :doc "restore invalidates staged candidates and starts independent pending ledgers"
  (let* ((session (niwa-session--create))
         (other (niwa-session--create))
         (candidate (niwa-session-checkpoint-capture
                     session "/root" (test-niwa-session-checkpoint--root 9))))
    (should (niwa-session-checkpoint-valid-p session candidate))
    (should-not (niwa-session-checkpoint-valid-p other candidate))
    ;; Publisher artifact copies must preserve candidate validity.
    (should (niwa-session-checkpoint-valid-p session (copy-tree candidate t)))
    (niwa-session-checkpoint-consume
     session "/root" (list (test-niwa-session-checkpoint--mail "pending")))
    (niwa-session-checkpoint-seed
     session (test-niwa-session-checkpoint--root 4 2))
    (should-not (niwa-session-checkpoint-valid-p session candidate))
    (should-not (niwa-session-checkpoint-install session candidate))
    (should-error (niwa-session-checkpoint-project session nil candidate))
    (let ((projected (niwa-session-checkpoint-project session nil)))
      (should (= 4 (plist-get projected :total-turn-count)))
      (should-not (plist-get projected :messages)))))

(niwa-deftest niwa-session-checkpoint-root-p ()
  ,test
  (test)
  :doc "only root candidates can carry root fields and completion is pure"
  (let* ((session (niwa-session--create))
         (root (niwa-session-checkpoint-capture session "/root"))
         (agent (niwa-session-checkpoint-capture session "/root/a")))
    (should (niwa-session-checkpoint-root-p root))
    (should-not (niwa-session-checkpoint-root-p agent))
    (should-not (niwa-session-checkpoint-root-p nil))
    (should-error (niwa-session-checkpoint-with-root agent nil))
    (let ((completed (niwa-session-checkpoint-with-root
                      root (test-niwa-session-checkpoint--root 7))))
      (should (= 7 (plist-get (niwa-session-checkpoint-project session nil completed)
                             :total-turn-count)))
      (should (= 0 (plist-get (niwa-session-checkpoint-project session nil root)
                             :total-turn-count))))))

(niwa-deftest niwa-session-checkpoint-release-proofs ()
  ,test
  (test)
  :doc "prevents stale queued sidecars replaying mail or PTC across rotation and retry"
  (let* ((session (niwa-session--create :publication-active-p t))
         (mail (test-niwa-session-checkpoint--mail "consumed"))
         (ptc '(:id "retired-tool" :conversation "/root" :state settled))
         (stale (list :messages (list mail) :ptc-checkpoints (list ptc))))
    (niwa-session-checkpoint-consume session "/root" (list mail))
    (niwa-session-checkpoint-defer-ptc session "/root" (list ptc))
    (let ((candidate (niwa-session-checkpoint-capture session "/root")))
      (niwa-session-checkpoint-install session candidate)
      (should-not (niwa-session-checkpoint-release-proofs session))
      (let ((projected (niwa-session-checkpoint-project session stale)))
        (should-not (plist-get projected :messages))
        (should-not (plist-get projected :ptc-checkpoints)))
      ;; A failed queued write retains its frozen bytes for retry after the
      ;; original receipt/tool span has moved into an archived transcript.
      (setf (niwa-session-publication-active-p session) nil
            (niwa-session-publication-uncommitted-batches session) '(retry))
      (should-not (niwa-session-checkpoint-release-proofs session))
      (should-not (plist-get (niwa-session-checkpoint-project session stale) :messages))
      (setf (niwa-session-publication-uncommitted-batches session) nil
            (niwa-session-publication-queue session) '(queued))
      (should-not (niwa-session-checkpoint-release-proofs session))
      (setf (niwa-session-publication-queue session) nil
            (niwa-session-pending-publication session) 'recovery)
      (should-not (niwa-session-checkpoint-release-proofs session))
      (setf (niwa-session-pending-publication session) nil)
      (should (niwa-session-checkpoint-release-proofs session))
      ;; No retired identity history remains once stale work cannot commit.
      (let ((projected (niwa-session-checkpoint-project session stale)))
        (should (equal (list mail) (plist-get projected :messages)))
        (should (equal (list ptc) (plist-get projected :ptc-checkpoints))))))
  :doc "an unqueued PID-style install needs no stale-publication proofs"
  (let* ((session (niwa-session--create))
         (mail (test-niwa-session-checkpoint--mail "consumed"))
         (raw (list :messages (list mail))))
    (niwa-session-checkpoint-consume session "/root" (list mail))
    (niwa-session-checkpoint-install
     session (niwa-session-checkpoint-capture session "/root"))
    (should (equal (list mail)
                   (plist-get (niwa-session-checkpoint-project session raw) :messages)))))

(provide 'test-niwa-session-checkpoint)
;;; test-niwa-session-checkpoint.el ends here
