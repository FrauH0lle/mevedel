;;; test-mevedel-session-checkpoint.el --- Committed checkpoint tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the owner API without publisher I/O: failed/deferred writes do not
;; commit, and installing one frozen conversation cannot retire another's mail.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-checkpoint)

(defun test-mevedel-session-checkpoint--root (turn &optional segment)
  "Return seven root fields describing TURN in SEGMENT."
  (list :current-segment (or segment 1)
        :total-turn-count turn
        :prompt-index (list (cons (or segment 1)
                                 (list (list :turn turn :text "prompt"))))
        :file-snapshots (list (cons turn '(("file" . "snapshot"))))
        :chapters (list (list :segment 1 :title "chapter"))
        :first-user-message (copy-sequence "first")
        :latest-user-message (format "turn %s" turn)))

(defun test-mevedel-session-checkpoint--mail (id &optional recipient)
  "Return a durable mailbox record with ID addressed to RECIPIENT."
  (list :id id :type 'MAIL :sender "/root/sender"
        :recipient (or recipient "/root") :payload (concat "message " id)
        :timestamp 1))

(defun test-mevedel-session-checkpoint--ids (records)
  "Return the durable identities of RECORDS."
  (mapcar (lambda (record) (plist-get record :id)) records))

(mevedel-deftest mevedel-session-checkpoint-seed ()
  ,test
  (test)
  :doc "starts empty rather than trusting live state before first publication"
  (let* ((session (mevedel-session--create
                   :current-segment 9 :turn-count 12
                   :prompt-index '((9 (:text "unpublished")))))
         (projected (mevedel-session-checkpoint-project
                     session (test-mevedel-session-checkpoint--root 12 9))))
    (should (= 1 (plist-get projected :current-segment)))
    (should (= 0 (plist-get projected :total-turn-count)))
    (should-not (plist-get projected :prompt-index)))
  :doc "detaches all seven seeded fields, including strings, from decoded state"
  (let* ((session (mevedel-session--create))
         (sidecar (test-mevedel-session-checkpoint--root 3 2))
         (before (mevedel-session-checkpoint--copy sidecar)))
    (mevedel-session-checkpoint-seed session sidecar)
    (aset (plist-get sidecar :first-user-message) 0 ?X)
    (setf (plist-get sidecar :total-turn-count) 99)
    (let ((projected (mevedel-session-checkpoint-project session nil)))
      (dolist (key mevedel-session-checkpoint--root-keys)
        (should (equal (plist-get before key) (plist-get projected key))))))
  :doc "copies long histories without consuming a Lisp frame per prompt"
  (let* ((session (mevedel-session--create))
         (prompts (cl-loop for turn from 1 to 3000
                           collect (list :turn turn :text "prompt")))
         (sidecar (list :prompt-index (list (cons 1 prompts)))))
    (mevedel-session-checkpoint-seed session sidecar)
    (should (= 3000
               (length
                (cdar (plist-get
                       (mevedel-session-checkpoint-project session nil)
                       :prompt-index)))))))

(mevedel-deftest mevedel-session-checkpoint-first-user-message ()
  ,test
  (test)
  :doc "reads only the committed preview and cannot mutate owner state"
  (let ((session (mevedel-session--create)))
    (should-not (mevedel-session-checkpoint-first-user-message session))
    (mevedel-session-checkpoint-seed
     session (list :first-user-message (copy-sequence "first")))
    (let ((preview (mevedel-session-checkpoint-first-user-message session))
          (candidate (mevedel-session-checkpoint-capture
                      session "/root" '(:first-user-message "next"))))
      (aset preview 0 ?X)
      (should (equal "first" (mevedel-session-checkpoint-first-user-message session)))
      (mevedel-session-checkpoint-install session candidate)
      (should (equal "next" (mevedel-session-checkpoint-first-user-message session))))))

(mevedel-deftest mevedel-session-checkpoint-consume ()
  ,test
  (test)
  :doc "keeps distinct identical mail and deduplicates durable identities"
  (let* ((session (mevedel-session--create))
         (old (test-mevedel-session-checkpoint--mail "old"))
         (new (test-mevedel-session-checkpoint--mail "new")))
    (setf (plist-get old :payload) (copy-sequence "same")
          (plist-get new :payload) (copy-sequence "same"))
    (mevedel-session-checkpoint-consume session "/root" (list old))
    (mevedel-session-checkpoint-consume session "/root" (list new old))
    (aset (plist-get old :payload) 0 ?X)
    (let* ((raw (list :messages (list new)))
           (projected (mevedel-session-checkpoint-project session raw))
           (mail (plist-get projected :messages)))
      (should (equal '("new" "old")
                     (test-mevedel-session-checkpoint--ids mail)))
      (should (equal "same" (plist-get (cadr mail) :payload)))
      (should (equal (list new) (plist-get raw :messages)))))
  :doc "keeps durable identity but removes runtime-only fields from retained mail"
  (let* ((session (mevedel-session--create))
         (record (append (test-mevedel-session-checkpoint--mail "runtime")
                         '(:wake t :usage (:tokens 50)))))
    (mevedel-session-checkpoint-consume session "/root" (list record))
    (let ((persisted (car (plist-get
                          (mevedel-session-checkpoint-project session nil)
                          :messages))))
      (should (equal "runtime" (plist-get persisted :id)))
      (should-not (plist-member persisted :wake))
      (should-not (plist-member persisted :usage))))
  :doc "rejects missing identities and cross-conversation consumption"
  (let ((session (mevedel-session--create)))
    (should-error (mevedel-session-checkpoint-consume
                   session "/root" '((:payload "missing id"))))
    (should-error (mevedel-session-checkpoint-consume
                   session "root" nil))
    (should-error (mevedel-session-checkpoint-consume
                   session "/root/agent"
                   (list (test-mevedel-session-checkpoint--mail "wrong"))))
    (should-not (plist-get
                 (mevedel-session-checkpoint-project session nil) :messages))))

(mevedel-deftest mevedel-session-checkpoint-capture ()
  ,test
  (test)
  :doc "freezes exact IDs before nested consumption and later root completion"
  (let* ((session (mevedel-session--create))
         (old-root (test-mevedel-session-checkpoint--root 1))
         (new-root (test-mevedel-session-checkpoint--root 2))
         (one (test-mevedel-session-checkpoint--mail "one"))
         (two (test-mevedel-session-checkpoint--mail "two")))
    (mevedel-session-checkpoint-seed session old-root)
    (mevedel-session-checkpoint-consume session "/root" (list one))
    (let* ((frozen (mevedel-session-checkpoint-capture session "/root"))
           (candidate (mevedel-session-checkpoint-with-root frozen new-root)))
      (mevedel-session-checkpoint-consume session "/root" (list two))
      (setf (plist-get new-root :total-turn-count) 99)
      (let ((ordinary (mevedel-session-checkpoint-project session new-root))
            (owned (mevedel-session-checkpoint-project session new-root candidate)))
        (should (= 1 (plist-get ordinary :total-turn-count)))
        (should (equal '("two" "one")
                       (test-mevedel-session-checkpoint--ids
                        (plist-get ordinary :messages))))
        (should (= 2 (plist-get owned :total-turn-count)))
        (should (equal '("two")
                       (test-mevedel-session-checkpoint--ids
                        (plist-get owned :messages)))))
      ;; Merely serializing the candidate above did not install it.
      (should (= 1 (plist-get
                    (mevedel-session-checkpoint-project session nil)
                    :total-turn-count)))
      (should (mevedel-session-checkpoint-install session candidate))
      (should (mevedel-session-checkpoint-install session candidate))
      (let ((committed (mevedel-session-checkpoint-project session nil)))
        (should (= 2 (plist-get committed :total-turn-count)))
        (should (equal '("two")
                       (test-mevedel-session-checkpoint--ids
                        (plist-get committed :messages))))))))

(mevedel-deftest mevedel-session-checkpoint-project ()
  ,test
  (test)
  :doc "an agent transcript retires only its own consumption, not live root fields"
  (let* ((session (mevedel-session--create))
         (root-mail (test-mevedel-session-checkpoint--mail "root"))
         (agent-mail (test-mevedel-session-checkpoint--mail "agent" "/root/a"))
         (other-mail (test-mevedel-session-checkpoint--mail "other" "/root/b"))
         (raw (append (test-mevedel-session-checkpoint--root 8 4)
                      (list :session-name "new metadata"
                            :agent-registry
                            '((:path "/root/a" :activity idle :mailbox nil)
                              (:path "/root/b" :activity running :mailbox nil))))))
    (mevedel-session-checkpoint-seed
     session (test-mevedel-session-checkpoint--root 2))
    (mevedel-session-checkpoint-consume session "/root" (list root-mail))
    (mevedel-session-checkpoint-consume session "/root/a" (list agent-mail))
    (mevedel-session-checkpoint-consume session "/root/b" (list other-mail))
    (let* ((candidate (mevedel-session-checkpoint-capture session "/root/a"))
           (projected (mevedel-session-checkpoint-project session raw candidate))
           (registry (plist-get projected :agent-registry)))
      (should (= 2 (plist-get projected :total-turn-count)))
      (should (equal "new metadata" (plist-get projected :session-name)))
      (should (equal (list root-mail) (plist-get projected :messages)))
      (should-not (plist-get (car registry) :mailbox))
      (should (equal (list other-mail) (plist-get (cadr registry) :mailbox)))
      (should-not (plist-get (cadr (plist-get raw :agent-registry)) :mailbox))
      (mevedel-session-checkpoint-install session candidate)
      (let ((ordinary (mevedel-session-checkpoint-project session raw)))
        (should-not (plist-get (car (plist-get ordinary :agent-registry)) :mailbox))
        (should (equal (list root-mail) (plist-get ordinary :messages))))))
  :doc "live Goal, Plan and native conversations survive metadata and unrelated-agent saves"
  (let* ((session (mevedel-session--create))
         (committed (test-mevedel-session-checkpoint--root 3 2))
         (raw (append
               (test-mevedel-session-checkpoint--root 8 4)
               '(:goal (:id "goal" :status active :turns-run 5)
                 :plan-metadata (:status approved :plan-path "local/plans/accepted.md")
                 :external-conversations
                 (("/root" :engine claude-code :id "native-current" :state ready))))))
    (mevedel-session-checkpoint-seed session committed)
    (dolist (candidate (list nil (mevedel-session-checkpoint-capture session "/root/other")))
      (let ((projected (mevedel-session-checkpoint-project session raw candidate)))
        (dolist (key mevedel-session-checkpoint--root-keys)
          (should (equal (plist-get committed key) (plist-get projected key))))
        (dolist (key '(:goal :plan-metadata :external-conversations))
          (should (equal (plist-get raw key) (plist-get projected key))))))))

(mevedel-deftest mevedel-session-checkpoint-defer-ptc ()
  ,test
  (test)
  :doc "keeps deferred rows until matching commit and preserves post-freeze rows"
  (let* ((session (mevedel-session--create))
         (root-row '(:id "root-tool" :state settled :result "root"))
         (agent-row '(:id "agent-tool" :state settled :result "agent"))
         (later-row '(:id "later-tool" :state settled :result "later"))
         (running-row '(:id "running-tool" :state running)))
    (mevedel-session-checkpoint-defer-ptc session "/root" (list root-row))
    (mevedel-session-checkpoint-defer-ptc session "/root/a" (list agent-row))
    (let ((candidate (mevedel-session-checkpoint-capture session "/root/a")))
      (mevedel-session-checkpoint-defer-ptc session "/root/a" (list later-row))
      (let* ((raw (list :ptc-checkpoints (list running-row)))
             (ordinary (mevedel-session-checkpoint-project session raw))
             (owned (mevedel-session-checkpoint-project session raw candidate)))
        (should (equal '("running-tool" "later-tool" "agent-tool" "root-tool")
                       (test-mevedel-session-checkpoint--ids
                        (plist-get ordinary :ptc-checkpoints))))
        (should (equal '("running-tool" "later-tool" "root-tool")
                       (test-mevedel-session-checkpoint--ids
                        (plist-get owned :ptc-checkpoints))))
        (mevedel-session-checkpoint-install session candidate)
        (should (equal (plist-get owned :ptc-checkpoints)
                       (plist-get (mevedel-session-checkpoint-project session raw)
                                  :ptc-checkpoints)))))))

(mevedel-deftest mevedel-session-checkpoint-valid-p ()
  ,test
  (test)
  :doc "restore invalidates staged candidates and starts independent pending ledgers"
  (let* ((session (mevedel-session--create))
         (other (mevedel-session--create))
         (candidate (mevedel-session-checkpoint-capture
                     session "/root" (test-mevedel-session-checkpoint--root 9))))
    (should (mevedel-session-checkpoint-valid-p session candidate))
    (should-not (mevedel-session-checkpoint-valid-p other candidate))
    ;; Publisher artifact copies must preserve candidate validity.
    (should (mevedel-session-checkpoint-valid-p session (copy-tree candidate t)))
    (mevedel-session-checkpoint-consume
     session "/root" (list (test-mevedel-session-checkpoint--mail "pending")))
    (mevedel-session-checkpoint-seed
     session (test-mevedel-session-checkpoint--root 4 2))
    (should-not (mevedel-session-checkpoint-valid-p session candidate))
    (should-not (mevedel-session-checkpoint-install session candidate))
    (should-error (mevedel-session-checkpoint-project session nil candidate))
    (let ((projected (mevedel-session-checkpoint-project session nil)))
      (should (= 4 (plist-get projected :total-turn-count)))
      (should-not (plist-get projected :messages)))))

(mevedel-deftest mevedel-session-checkpoint-root-p ()
  ,test
  (test)
  :doc "only root candidates can carry root fields and completion is pure"
  (let* ((session (mevedel-session--create))
         (root (mevedel-session-checkpoint-capture session "/root"))
         (agent (mevedel-session-checkpoint-capture session "/root/a")))
    (should (mevedel-session-checkpoint-root-p root))
    (should-not (mevedel-session-checkpoint-root-p agent))
    (should-not (mevedel-session-checkpoint-root-p nil))
    (should-error (mevedel-session-checkpoint-with-root agent nil))
    (let ((completed (mevedel-session-checkpoint-with-root
                      root (test-mevedel-session-checkpoint--root 7))))
      (should (= 7 (plist-get (mevedel-session-checkpoint-project session nil completed)
                             :total-turn-count)))
      (should (= 0 (plist-get (mevedel-session-checkpoint-project session nil root)
                             :total-turn-count))))))

(mevedel-deftest mevedel-session-checkpoint-release-proofs ()
  ,test
  (test)
  :doc "prevents stale queued sidecars replaying mail or PTC across rotation and retry"
  (let* ((session (mevedel-session--create :publication-active-p t))
         (mail (test-mevedel-session-checkpoint--mail "consumed"))
         (ptc '(:id "retired-tool" :conversation "/root" :state settled))
         (stale (list :messages (list mail) :ptc-checkpoints (list ptc))))
    (mevedel-session-checkpoint-consume session "/root" (list mail))
    (mevedel-session-checkpoint-defer-ptc session "/root" (list ptc))
    (let ((candidate (mevedel-session-checkpoint-capture session "/root")))
      (mevedel-session-checkpoint-install session candidate)
      (should-not (mevedel-session-checkpoint-release-proofs session))
      (let ((projected (mevedel-session-checkpoint-project session stale)))
        (should-not (plist-get projected :messages))
        (should-not (plist-get projected :ptc-checkpoints)))
      ;; A failed queued write retains its frozen bytes for retry after the
      ;; original receipt/tool span has moved into an archived transcript.
      (setf (mevedel-session-publication-active-p session) nil
            (mevedel-session-publication-uncommitted-batches session) '(retry))
      (should-not (mevedel-session-checkpoint-release-proofs session))
      (should-not (plist-get (mevedel-session-checkpoint-project session stale) :messages))
      (setf (mevedel-session-publication-uncommitted-batches session) nil
            (mevedel-session-publication-queue session) '(queued))
      (should-not (mevedel-session-checkpoint-release-proofs session))
      (setf (mevedel-session-publication-queue session) nil
            (mevedel-session-pending-publication session) 'recovery)
      (should-not (mevedel-session-checkpoint-release-proofs session))
      (setf (mevedel-session-pending-publication session) nil)
      (should (mevedel-session-checkpoint-release-proofs session))
      ;; No retired identity history remains once stale work cannot commit.
      (let ((projected (mevedel-session-checkpoint-project session stale)))
        (should (equal (list mail) (plist-get projected :messages)))
        (should (equal (list ptc) (plist-get projected :ptc-checkpoints))))))
  :doc "an unqueued PID-style install needs no stale-publication proofs"
  (let* ((session (mevedel-session--create))
         (mail (test-mevedel-session-checkpoint--mail "consumed"))
         (raw (list :messages (list mail))))
    (mevedel-session-checkpoint-consume session "/root" (list mail))
    (mevedel-session-checkpoint-install
     session (mevedel-session-checkpoint-capture session "/root"))
    (should (equal (list mail)
                   (plist-get (mevedel-session-checkpoint-project session raw) :messages)))))

(provide 'test-mevedel-session-checkpoint)
;;; test-mevedel-session-checkpoint.el ends here
