;;; test-mevedel-session-publication-async.el --- Async delivery adversaries -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the producer and persisted consumer, not just publication phase
;; flags.  Fault injection intercepts callbacks around real filesystem work.
;; Each fixture owns its staging files, timers, processes, and conversation.

;;; Code:

(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))

(defun test-mevedel-publication-async--failure (phase)
  "Fail PHASE once, assert rollback, then retry the same producer identity."
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let* ((symbol (pcase phase
                      ('admission 'mevedel-session-durability-admit-async)
                      ('reservation 'mevedel-session-durability-reserve-publication-lease-async)
                      ('generation 'mevedel-session-publication--prepare-generation)
                      ('head 'mevedel-session-durability-commit-publication-head-async)))
            (original (symbol-function symbol))
            (head (plist-get (mevedel-session-publication session) :head))
            (injections 0) acknowledgements)
       (cl-letf (((symbol-function symbol)
                  (lambda (&rest arguments)
                    (cl-incf injections)
                    (if (eq phase 'generation)
                        (error "Injected immutable generation failure")
                      (if (eq phase 'head)
                          (funcall (car (last arguments)) nil '(error "Injected head refusal"))
                        ;; Admission/reservation really ran.  Refuse their result
                        ;; at the continuation boundary, including fixed writes.
                        (let ((ack (car (last arguments))))
                          (apply original
                                 (append (butlast arguments)
                                         (list (lambda (_success _err)
                                                 (funcall ack nil '(error "Injected phase failure"))))))))))))
         (test-mevedel-publication-async--send
          session "retry-id" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (= injections 1))
       (should (= (length acknowledgements) 1))
       (should-not (caar acknowledgements))
       (should (cadar acknowledgements))
       (should-not (test-mevedel-publication-async--mail session))
       (should-not (test-mevedel-publication-async--restored-mail session))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (test-mevedel-publication-async--send
        session "retry-id" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-mevedel-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (caar acknowledgements))
       (should-not (cadar acknowledgements))
       (should (equal (list (secure-hash 'sha256 "/root:retry-id"))
                      (test-mevedel-publication-async--ids
                       (test-mevedel-publication-async--restored-mail session))))))))

(defun test-mevedel-publication-async--transcript-artifacts
    (session buffer logical content &optional checkpoint)
  "Return the artifacts of one transcript checkpoint of SESSION from BUFFER.
A non-fixed transcript LOGICAL holding CONTENT, an ancillary fixed file below
a missing directory chain, and the sidecar commit marker carrying CHECKPOINT."
  (let ((save-path (mevedel-session-save-path session)))
    (list (list :path (file-name-concat save-path logical) :content content)
          (list :path (file-name-concat save-path "notes/deep/extra.txt")
                :content "extra")
          (mevedel-session-artifacts--sidecar-artifact session buffer checkpoint))))

(mevedel-deftest mevedel-agent-control-enqueue-execution-result/async (:quiet t)
  ,test
  (test)
  :doc "overlapping completions return before commit and acknowledge FIFO exactly once"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           acknowledgements)
       (dolist (id '("first" "second"))
         (should (eq 'pending
                     (test-mevedel-publication-async--send
                      session id (lambda (ok err) (push (list id ok err) acknowledgements))))))
       ;; No event-loop opportunity has occurred.  Launch is not commitment.
       (should-not acknowledgements)
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should (mevedel-session-publication-busy-p session))
       (test-mevedel-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (equal '(("first" t nil) ("second" t nil)) (reverse acknowledgements)))
       (let ((ids (mapcar (lambda (id) (secure-hash 'sha256 (concat "/root:" id)))
                          '("first" "second"))))
         (should (equal ids (test-mevedel-publication-async--ids
                             (test-mevedel-publication-async--mail session))))
         (should (equal ids (test-mevedel-publication-async--ids
                             (test-mevedel-publication-async--restored-mail session)))))
       (accept-process-output nil 0.05)
       (should (= 2 (length acknowledgements))))))

  :doc "admission failure rolls back and retry has one stable durable identity"
  (test-mevedel-publication-async--failure 'admission)
  :doc "reservation failure rolls back the tentative mailbox despite fixed sidecar writes"
  (test-mevedel-publication-async--failure 'reservation)
  :doc "generation failure leaves no ghost mail and permits same-ID retry"
  (test-mevedel-publication-async--failure 'generation)
  :doc "head refusal reconciles old authority before rollback and same-ID retry"
  (test-mevedel-publication-async--failure 'head)

  :doc "a committed head with a lost process result reconciles and retry cannot duplicate mail"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((commit (symbol-function 'mevedel-session-durability-commit-publication-head-async))
           (lost 0) acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-commit-publication-head-async)
                  (lambda (owner head callback)
                    (funcall commit owner head
                             (lambda (ok err)
                               (should ok) (should-not err)
                               (cl-incf lost)
                               (funcall callback nil '(file-error "Program died after head commit")))))))
         (test-mevedel-publication-async--send
          session "lost-result" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (= lost 1))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session))))
       (test-mevedel-publication-async--send
        session "lost-result" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-mevedel-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (equal '((t nil) (t nil)) acknowledgements))
       (should (equal (list (secure-hash 'sha256 "/root:lost-result"))
                      (test-mevedel-publication-async--ids
                       (test-mevedel-publication-async--restored-mail session)))))))

  :doc "retry of consumed but uncheckpointed mail does not redeliver its stable identity"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let (acknowledgements)
       (test-mevedel-publication-async--send
        session "consumed" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-mevedel-publication-async--wait (lambda () acknowledgements))
       (mevedel-session-checkpoint-consume
        session "/root" (test-mevedel-publication-async--mail session))
       (mevedel-agent-control--set-mailbox-queue session "/root" nil)
       (test-mevedel-publication-async--send
        session "consumed" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-mevedel-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (equal '((t nil) (t nil)) acknowledgements))
       (should-not (test-mevedel-publication-async--mail session))
       ;; Until the transcript checkpoints consumption, restore must retain it.
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session))))
       (mevedel-session-artifacts-save session buffer nil t)
       (should-not (test-mevedel-publication-async--restored-mail session))))))

(mevedel-deftest mevedel-session-publication-submit/async (:quiet t)
  ,test
  (test)
  :doc "quit while staging removes partial files and fails the checkpoint once"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((write (symbol-function 'write-region))
           (writes 0) directories acknowledgements)
       (cl-letf (((symbol-function 'write-region)
                  (lambda (start end filename &rest arguments)
                    (when (and (stringp filename)
                               (string-match-p "mevedel-publication-" filename))
                      (push (file-name-directory filename) directories)
                      (when (= (cl-incf writes) 2) (signal 'quit nil)))
                    (apply write start end filename arguments))))
         (mevedel-session-publication-submit
          session
          (lambda ()
            (test-mevedel-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "Partial staging"))
          (lambda (ok err) (push (list ok err) acknowledgements)) nil t)
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (= writes 2))
       (should (equal '((nil (quit))) acknowledgements))
       (should-not (mevedel-session-publication-busy-p session))
       (should-not (mevedel-session-pending-publication session))
       (dolist (directory directories) (should-not (file-exists-p directory))))))

  :doc "quit in an acknowledged producer callback cannot strand its successor"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledgements diagnostics)
       (mevedel-test--with-captured-diagnostics diagnostics
         (test-mevedel-publication-async--send
          session "callback-quit"
          (lambda (ok err)
            (push (list ok err) acknowledgements)
            (signal 'quit nil)))
         (test-mevedel-publication-async--send
          session "after-callback-quit"
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait
          (lambda () (= 2 (length acknowledgements)))))
       (should (equal '((t nil) (t nil)) acknowledgements))
       (should (string-match-p "Publication acknowledgement failed: Quit" diagnostics))
       (should-not (mevedel-session-publication-busy-p session))
       (should (= 2 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "disabled transport still drains accepted overlapping publications exactly once"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((mevedel-transport--enabled-p nil) acknowledgements)
       (dolist (id '("disabled-one" "disabled-two"))
         (test-mevedel-publication-async--send
          session id (lambda (ok err) (push (list id ok err) acknowledgements))))
       (should-not acknowledgements)
       (test-mevedel-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (accept-process-output nil 0.02)
       (should (equal '(("disabled-one" t nil) ("disabled-two" t nil))
                      (reverse acknowledgements)))
       (test-mevedel-publication-async--wait
        (lambda () (not (mevedel-session-publication-busy-p session))))
       (should (= 2 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "global transport cancellation cannot strand a job or its queued successor"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((mevedel-transport--enabled-p t) acknowledgements)
       (dolist (id '("cancelled-one" "cancelled-two"))
         (test-mevedel-publication-async--send
          session id (lambda (ok err) (push (list id ok err) acknowledgements))))
       ;; Cancel both the job-advance and queued-preparation opportunities.
       (should (>= (hash-table-count mevedel-transport--pending) 2))
       (mevedel-transport-cancel-pending)
       (should-not acknowledgements)
       (test-mevedel-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (accept-process-output nil 0.02)
       (should (equal '(("cancelled-one" t nil) ("cancelled-two" t nil))
                      (reverse acknowledgements)))
       (test-mevedel-publication-async--wait
        (lambda () (not (mevedel-session-publication-busy-p session))))
       (should (= 2 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "an ACK can stage a new artifact without its predecessor cleanup losing the batch"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((logical "agents/ack-staged.chat.org") acknowledgements inherited-cells)
       (test-mevedel-publication-async--send
        session "nested-stage"
        (lambda (ok err)
          (push (list ok err) acknowledgements)
          (setq inherited-cells
                (list mevedel-session-durability--transaction-clock
                      mevedel-session-durability--asserted-directories
                      mevedel-session-recovery--mutation-cache
                      mevedel-session-durability--commit-riders
                      mevedel-session-durability--final-commit
                      mevedel-session-durability-async-process-function))
          (mevedel-session-publication-publish
           session (list (list :path (file-name-concat (mevedel-session-save-path session) logical)
                               :content "ACK staged transcript")))))
       (test-mevedel-publication-async--wait (lambda () acknowledgements))
       (should (equal '((t nil)) acknowledgements))
       (should (equal '(nil nil nil nil nil nil) inherited-cells))
       (should (mevedel-session-publication-uncommitted-artifact session logical))
       (mevedel-session-artifacts-save session buffer nil t)
       (should-not (mevedel-session-publication-uncommitted-artifact session logical))
       (should (equal "ACK staged transcript"
                      (mevedel-session-artifacts-read-artifact session logical t)))
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "cleanup failure after authoritative adoption cannot change a success acknowledgement"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((delete (symbol-function 'mevedel-session-publication--delete-batches))
           (failed 0) acknowledgements diagnostics)
       (mevedel-test--with-captured-diagnostics diagnostics
         (cl-letf (((symbol-function 'mevedel-session-publication--delete-batches)
                    (lambda (batches)
                      (funcall delete batches)
                      (cl-incf failed)
                      (error "Injected cleanup failure"))))
           (test-mevedel-publication-async--send
            session "cleanup" (lambda (ok err) (push (list ok err) acknowledgements)))
           (test-mevedel-publication-async--wait (lambda () acknowledgements))))
       (should (= failed 1))
       (should (string-match-p "Injected cleanup failure" diagnostics))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "collection scheduling failure after adoption warns but still succeeds exactly once"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((failed 0) acknowledgements diagnostics)
       (mevedel-test--with-captured-diagnostics diagnostics
         (cl-letf (((symbol-function 'mevedel-session-collection-schedule)
                    (lambda (_session)
                      (cl-incf failed)
                      (error "Injected collection scheduling failure"))))
           (test-mevedel-publication-async--send
            session "collection" (lambda (ok err) (push (list ok err) acknowledgements)))
           (test-mevedel-publication-async--wait (lambda () acknowledgements))))
       (should (= failed 1))
       (should (string-match-p "Injected collection scheduling failure" diagnostics))
       (should (equal '((t nil)) acknowledgements))
       (should-not (mevedel-session-publication-busy-p session))
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "disk restore on either side of head adoption excludes then includes tentative mail"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((reserve (symbol-function 'mevedel-session-durability-reserve-publication-lease-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-mevedel-publication-async--send
          session "restore" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () continuation))
         (should-not acknowledgements)
         (should (plist-get (car (test-mevedel-publication-async--mail session)) :uncommitted))
         (should-not (test-mevedel-publication-async--restored-mail session))
         (funcall continuation)
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should-not (plist-get (car (test-mevedel-publication-async--mail session)) :uncommitted))
       (should (equal (list (secure-hash 'sha256 "/root:restore"))
                      (test-mevedel-publication-async--ids
                       (test-mevedel-publication-async--restored-mail session)))))))

  :doc "a multi-artifact checkpoint publishes transcripts and ancillary fixed files asynchronously"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           (save-path (mevedel-session-save-path session))
           acknowledgements)
       (mevedel-session-publication-submit
        session
        (lambda ()
          (test-mevedel-publication-async--transcript-artifacts
           session buffer "segment-9.chat.org" "async transcript"))
        (lambda (ok err) (push (list ok err) acknowledgements)))
       (should-not acknowledgements)
       (test-mevedel-publication-async--wait (lambda () acknowledgements))
       (should (equal '((t nil)) acknowledgements))
       (should-not (equal head (plist-get (mevedel-session-publication session) :head)))
       (should (equal "extra"
                      (with-temp-buffer
                        (insert-file-contents
                         (file-name-concat save-path "notes/deep/extra.txt"))
                        (buffer-string))))
       (should (equal "async transcript"
                      (mevedel-session-artifacts-read-artifact
                       session "segment-9.chat.org" t)))
       (test-mevedel-publication-async--wait
        (lambda () (not (mevedel-session-publication-busy-p session))))
       (should-not (mevedel-session-pending-publication session)))))

  :doc "a nil preparation is a clean no-op success"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           acknowledgements)
       (mevedel-session-publication-submit
        session #'ignore (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-mevedel-publication-async--wait (lambda () acknowledgements))
       (should (equal '((t nil)) acknowledgements))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (test-mevedel-publication-async--wait
        (lambda () (not (mevedel-session-publication-busy-p session))))
       (should-not (mevedel-session-publication-active-p session)))))

  :doc "a replacing marker is refused before any target write"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           acknowledgements)
       (mevedel-session-publication-submit
        session
        (lambda ()
          (mapcar (lambda (artifact)
                    (if (plist-get artifact :commit-marker)
                        (plist-put (copy-sequence artifact) :replace t)
                      artifact))
                  (test-mevedel-publication-async--transcript-artifacts
                   session buffer "segment-9.chat.org" "refused")))
        (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-mevedel-publication-async--wait (lambda () acknowledgements))
       (should-not (caar acknowledgements))
       (should (cadar acknowledgements))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should-not (mevedel-session-pending-publication session))
       (should-not (file-exists-p
                    (file-name-concat (mevedel-session-save-path session)
                                      "notes/deep/extra.txt"))))))

  :doc "a checkpoint candidate is installed only after its head commits"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((install (symbol-function 'mevedel-session-checkpoint-install))
           (head (plist-get (mevedel-session-publication session) :head))
           installed acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-checkpoint-install)
                  (lambda (owner candidate)
                    (push (plist-get (mevedel-session-publication owner) :head) installed)
                    (funcall install owner candidate))))
         (mevedel-session-publication-submit
          session
          (lambda ()
            (test-mevedel-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "checkpointed"
             (mevedel-session-checkpoint-capture session "/root")))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length installed)))
       (should-not (equal head (car installed)))
       (should (equal (plist-get (mevedel-session-publication session) :head)
                      (car installed))))))

  :doc "a candidate invalidated before the head fails without installing or writing"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           (installs 0)
           acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-checkpoint-install)
                  (lambda (&rest _) (cl-incf installs))))
         (mevedel-session-publication-submit
          session
          (lambda ()
            (let ((candidate (mevedel-session-checkpoint-capture session "/root")))
              ;; Adopting another history invalidates every earlier candidate.
              (mevedel-session-checkpoint-seed
               session (mevedel-session-artifacts--live-sidecar session buffer))
              (test-mevedel-publication-async--transcript-artifacts
               session buffer "segment-9.chat.org" "stale" candidate)))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should-not (caar acknowledgements))
       (should (= 0 installs))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should-not (mevedel-session-pending-publication session)))))

  :doc "retain-failure keeps the frozen batch as recovery and a retry commits it"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((prepare (symbol-function 'mevedel-session-publication--prepare-generation))
           (head (plist-get (mevedel-session-publication session) :head))
           (failures 0)
           acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-publication--prepare-generation)
                  (lambda (&rest arguments)
                    (if (= 0 failures)
                        (progn (cl-incf failures)
                               (error "Injected immutable generation failure"))
                      (apply prepare arguments)))))
         (mevedel-test--with-captured-diagnostics nil
           (mevedel-session-publication-submit
            session
            (lambda ()
              (test-mevedel-publication-async--transcript-artifacts
               session buffer "segment-9.chat.org" "retained"))
            (lambda (ok err) (push (list ok err) acknowledgements))
            nil t)
           (test-mevedel-publication-async--wait (lambda () acknowledgements))
           (should-not (caar acknowledgements))
           (should (equal head (plist-get (mevedel-session-publication session) :head)))
           (should (mevedel-session-pending-publication session))
           (should (plist-get (mevedel-session-pending-publication session) :batches))
           (mevedel-session-publication-retry session)))
       (should-not (mevedel-session-pending-publication session))
       (should-not (equal head (plist-get (mevedel-session-publication session) :head)))
       (should (equal "retained"
                      (mevedel-session-artifacts-read-artifact
                       session "segment-9.chat.org" t)))
       (should (= 1 (length acknowledgements))))))

  :doc "without retain-failure the frozen batch is discarded and later mutation proceeds"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-publication--prepare-generation)
                  (lambda (&rest _) (error "Injected immutable generation failure"))))
         (mevedel-session-publication-submit
          session
          (lambda ()
            (test-mevedel-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "discarded"))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should-not (caar acknowledgements))
       (should-not (mevedel-session-pending-publication session))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should-not (mevedel-session-publication-uncommitted-artifact
                    session "segment-9.chat.org")))))

  :doc "a committed head whose result was lost reconciles, adopts, and installs once"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((original (symbol-function 'mevedel-session-durability-commit-publication-head-async))
            (install (symbol-function 'mevedel-session-checkpoint-install))
            (head (plist-get (mevedel-session-publication session) :head))
            (installs 0)
            acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-commit-publication-head-async)
                  (lambda (owner candidate callback)
                    ;; The head really commits; only its result is lost.
                    (funcall original owner candidate
                             (lambda (_success _error)
                               (funcall callback nil '(error "Injected lost result"))))))
                 ((symbol-function 'mevedel-session-checkpoint-install)
                  (lambda (owner candidate)
                    (cl-incf installs)
                    (funcall install owner candidate))))
         (mevedel-session-publication-submit
          session
          (lambda ()
            (test-mevedel-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "late"
             (mevedel-session-checkpoint-capture session "/root")))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 installs))
       (should-not (equal head (plist-get (mevedel-session-publication session) :head)))
       (should-not (mevedel-session-pending-publication session))
       (should (equal "late"
                      (mevedel-session-artifacts-read-artifact
                       session "segment-9.chat.org" t)))))))

(mevedel-deftest mevedel-session-publication--async-fixed-operations (:quiet t)
  ,test
  (test)
  :doc "one program proves the lease then ensures each parent level and writes each fixed artifact"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (mevedel-session-publication--stage-artifacts
                    session
                    (test-mevedel-publication-async--transcript-artifacts
                     session buffer "segment-9.chat.org" "transcript")))
            (plan (mevedel-session-publication--async-fixed-operations session batch))
            (operations (car plan)))
       (unwind-protect
           (progn
             ;; The immutable-only transcript contributes nothing; the deep
             ;; ancillary file brings its two parents, the sidecar none.
             (should (equal '(verify-latest make-directory make-directory write write)
                            (mapcar (lambda (operation) (plist-get operation :op))
                                    operations)))
             (should (equal '(3 4) (cdr plan)))
             (should (cl-every (lambda (index)
                                 (eq 'write (plist-get (nth index operations) :op)))
                               (cdr plan)))
             (should (equal ".el" (plist-get (car operations) :content)))
             (should (cl-every (lambda (operation) (plist-get operation :optional))
                               (seq-filter (lambda (operation)
                                             (eq 'make-directory (plist-get operation :op)))
                                           operations)))
             (should (equal (file-name-concat (mevedel-session-save-path session)
                                              "notes/deep/extra.txt")
                            (file-truename (plist-get (nth 3 operations) :path)))))
         (mevedel-session-publication--delete-batch batch)))))
  :doc "a session without a bound lease generation plans nothing"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((batch (mevedel-session-publication--stage-artifacts
                   session
                   (test-mevedel-publication-async--transcript-artifacts
                    session buffer "segment-9.chat.org" "transcript")))
           (lease (mevedel-session-lease session)))
       (unwind-protect
           (progn
             (setf (mevedel-session-lease session) nil)
             (should (equal '(nil) (mevedel-session-publication--async-fixed-operations
                                    session batch))))
         (setf (mevedel-session-lease session) lease)
         (mevedel-session-publication--delete-batch batch)))))
  :doc "a fixed artifact outside the session directory is refused"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (should-error
      (mevedel-session-publication--async-fixed-operations
       session (list :artifacts (list (list :path (make-temp-name "/tmp/mevedel-outside-")
                                            :source "/tmp/mevedel-source"))))))))

(mevedel-deftest mevedel-session-publication--project-staged-sidecar (:quiet t)
  ,test
  (test)
  :doc "an identity the checkpoint candidate consumed since freezing leaves the staged sidecar"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let (acknowledgements)
       (test-mevedel-publication-async--send
        session "stale-id" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-mevedel-publication-async--wait (lambda () acknowledgements))
       (let* ((mail (test-mevedel-publication-async--mail session))
              (id (plist-get (car mail) :id))
              (batch (mevedel-session-publication--stage-artifacts
                      session
                      (test-mevedel-publication-async--transcript-artifacts
                       session buffer "segment-9.chat.org" "transcript")))
              (marker (mevedel-session-publication--batch-marker batch))
              (source (plist-get marker :source)))
         (unwind-protect
             (progn
               (should (member id (test-mevedel-publication-async--ids
                                   (plist-get (mevedel-session-publication--read-staged-sidecar
                                               source)
                                              :messages))))
               ;; The mail is consumed after the sidecar was frozen, and the
               ;; candidate captured with that consumption retires it.
               (mevedel-session-checkpoint-consume session "/root" mail)
               (mevedel-agent-control--set-mailbox-queue session "/root" nil)
               (mevedel-session-publication--project-staged-sidecar
                session marker (mevedel-session-checkpoint-capture session "/root"))
               (should-not (member id (test-mevedel-publication-async--ids
                                       (plist-get
                                        (mevedel-session-publication--read-staged-sidecar
                                         source)
                                        :messages))))
               (should-not (plist-get marker :source-sha256)))
           (mevedel-session-publication--delete-batch batch))))))
  :doc "an already projected sidecar keeps its staged bytes"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (mevedel-session-publication--stage-artifacts
                    session
                    (test-mevedel-publication-async--transcript-artifacts
                     session buffer "segment-9.chat.org" "transcript")))
            (marker (mevedel-session-publication--batch-marker batch))
            (source (plist-get marker :source))
            (before (mevedel-session-publication--file-sha256 source)))
       (unwind-protect
           (progn
             (mevedel-session-publication--project-staged-sidecar session marker nil)
             (should (equal before (mevedel-session-publication--file-sha256 source))))
         (mevedel-session-publication--delete-batch batch)))))
  :doc "a session without a checkpoint owner is left untouched"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (mevedel-session-publication--stage-artifacts
                    session
                    (test-mevedel-publication-async--transcript-artifacts
                     session buffer "segment-9.chat.org" "transcript")))
            (marker (mevedel-session-publication--batch-marker batch))
            (source (plist-get marker :source))
            (before (mevedel-session-publication--file-sha256 source)))
       (let ((owner (mevedel-session-checkpoint-state session)))
         (unwind-protect
             (cl-letf (((symbol-function 'mevedel-session-checkpoint-project)
                        (lambda (&rest _) (ert-fail "Projected without an owner"))))
               (setf (mevedel-session-checkpoint-state session) nil)
               (mevedel-session-publication--project-staged-sidecar session marker nil)
               (should (equal before (mevedel-session-publication--file-sha256 source))))
           (setf (mevedel-session-checkpoint-state session) owner)
           (mevedel-session-publication--delete-batch batch)))))))

(defun test-mevedel-publication-async--interleaving (fail &optional latch-first)
  "Interleave checkpoint, observational registry save and latch with delivery.
FAIL refuses the reserved job, otherwise it commits before the other writers.
LATCH-FIRST starts the latch writer, rather than the root checkpoint, mid-job."
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((reserve (symbol-function 'mevedel-session-durability-reserve-publication-lease-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (setq continuation
                                     (lambda ()
                                       (funcall callback (and (not fail) ok)
                                                (if fail '(error "Injected rollback") err)))))))))
         (test-mevedel-publication-async--send
          session "interleaved" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () continuation))
         (should-not (test-mevedel-publication-async--restored-mail session))
         ;; An observational writer must queue PREPARATION, not frozen mail.
         (mevedel-session-artifacts-save-agent-registry session buffer)
         (with-current-buffer buffer (goto-char (point-max)) (insert "More streamed text\n"))
         (run-at-time 0.01 nil continuation)
         ;; Both are synchronous callers: they must drain before capturing or
         ;; mutating the generation the active job is proving.
         (when latch-first
           (should (mevedel-session-durability-set-unsettled-mutation session t)))
         (mevedel-session-artifacts-save session buffer nil t)
         (unless latch-first
           (should (mevedel-session-durability-set-unsettled-mutation session t)))
         (test-mevedel-publication-async--wait
          (lambda () (not (mevedel-session-publication-busy-p session)))))
       (should (= 1 (length acknowledgements)))
       (should (eq (not fail) (and (caar acknowledgements) t)))
       (should (mevedel-session-durability-unsettled-mutation-p session))
       (should (= (if fail 0 1) (length (test-mevedel-publication-async--restored-mail session))))
       (unless fail
         ;; Consumption after commit must not be resurrected by a deferred save.
         (let ((mail (test-mevedel-publication-async--mail session)))
           (mevedel-session-checkpoint-consume session "/root" mail)
           (mevedel-agent-control--set-mailbox-queue session "/root" nil)
           (mevedel-session-artifacts-save-agent-registry session buffer)
           (mevedel-session-artifacts-save session buffer nil t)
           (should-not (test-mevedel-publication-async--restored-mail session))))))))

(mevedel-deftest mevedel-session-publication-defer/async (:quiet t)
  ,test
  (test)
  :doc "quit in a queued preparation and its failure handler releases the next entry"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledged failures successor diagnostics)
       (mevedel-test--with-captured-diagnostics diagnostics
         (test-mevedel-publication-async--send
          session "preparation-quit" (lambda (&rest _) (setq acknowledged t)))
         (should (mevedel-session-publication-defer
                  session (lambda (_token) (signal 'quit nil))
                  (lambda (err) (push err failures) (signal 'quit nil))))
         (should (mevedel-session-publication-defer
                  session (lambda (_token) (setq successor t))))
         (test-mevedel-publication-async--wait (lambda () successor)))
       (should acknowledged)
       (should (equal '((quit)) failures))
       (should (string-match-p "Quit" diagnostics))
       (should-not (mevedel-session-publication-busy-p session))
       (should-not (mevedel-session-publication-preparation-token session)))))

  :doc "a timer cannot borrow a queued synchronous publication's held lease window"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((drain (symbol-function 'mevedel-session-publication--drain))
           injected timer-fired entered blocked prepared acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-publication--drain)
                  (lambda (&rest arguments)
                    (when (and (not injected)
                               (mevedel-session-publication-preparation-token session))
                      (setq injected t)
                      (should (mevedel-session-publication-active-p session))
                      (should-not (mevedel-session-publication-job session))
                      (run-at-time
                       0 nil
                       (lambda ()
                         (unwind-protect
                             (condition-case err
                                 (mevedel-session-durability-call-with-held-lease
                                  session (lambda ()
                                            (setq entered t)
                                            (mevedel-session-durability-set-unsettled-mutation session t)))
                               (user-error (setq blocked err)))
                           (setq timer-fired t))))
                      (test-mevedel-publication-async--wait (lambda () timer-fired)))
                    (apply drain arguments))))
         (test-mevedel-publication-async--send
          session "lease-window" (lambda (ok err) (push (list ok err) acknowledgements)))
         (should (mevedel-session-publication-defer
                  session (lambda (token)
                            (mevedel-session-artifacts-save session buffer nil t token)
                            (setq prepared t))))
         (test-mevedel-publication-async--wait (lambda () prepared))
         (mevedel-session-publication-barrier session))
       (should injected)
       (should timer-fired)
       (should blocked)
       (should-not entered)
       (should-not (mevedel-session-durability-unsettled-mutation-p session))
       (should (equal '((t nil)) acknowledgements))
       (should (mevedel-session-durability-set-unsettled-mutation session t))
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "a timer during preparation cannot inherit the owner's mutation-gate bypass"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((artifact (symbol-function 'mevedel-session-artifacts--sidecar-artifact))
           first-entered preparing timer-fired (nested-freezes 0) acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-artifacts--sidecar-artifact)
                  (lambda (&rest arguments)
                    (when preparing (cl-incf nested-freezes))
                    (unless first-entered
                      (setq first-entered t preparing t)
                      (unwind-protect
                          (progn
                            (run-at-time
                             0 nil
                             (lambda ()
                               (test-mevedel-publication-async--send
                                session "timer-second"
                                (lambda (ok err) (push (list "second" ok err) acknowledgements)))
                               (mevedel-session-artifacts-save-agent-registry session buffer)
                               (setq timer-fired t)))
                            (test-mevedel-publication-async--wait (lambda () timer-fired)))
                        (setq preparing nil)))
                    (apply artifact arguments))))
         (test-mevedel-publication-async--send
          session "timer-first"
          (lambda (ok err) (push (list "first" ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () (= 2 (length acknowledgements))))
         (mevedel-session-publication-barrier session))
       (should timer-fired)
       (should (= 0 nested-freezes))
       (should (equal '(("first" t nil) ("second" t nil)) (reverse acknowledgements)))
       (should (= 2 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "checkpoint, registry and latch writers preserve newly committed mail and consumption"
  (test-mevedel-publication-async--interleaving nil)
  :doc "a registry save requested during tentative delivery cannot resurrect rolled-back mail"
  (test-mevedel-publication-async--interleaving t)
  :doc "a latch change requested during reservation waits without erasing newly committed mail"
  (test-mevedel-publication-async--interleaving nil t)
  :doc "a latch change requested during reservation cannot resurrect rolled-back mail"
  (test-mevedel-publication-async--interleaving t t))

(mevedel-deftest mevedel-session-publication-barrier/async (:quiet t)
  ,test
  (test)
  :doc "a busy transport rejects a synchronous barrier before entering its wait loop"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledgements)
       (test-mevedel-publication-async--send
        session "busy-transport" (lambda (ok err) (push (list ok err) acknowledgements)))
       (cl-letf (((symbol-function 'mevedel-transport-busy-p) (lambda (&rest _) t))
                 ((symbol-function 'accept-process-output)
                  (lambda (&rest _) (ert-fail "Barrier waited behind its active transport"))))
         (should-error (mevedel-session-publication-barrier session) :type 'user-error))
       (should-not acknowledgements)
       (mevedel-session-publication-barrier session)
       (should (equal '((t nil)) acknowledgements)))))

  :doc "a synchronous publication owner cannot recursively drain its own queued preparation"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledgements)
       (setf (mevedel-session-publication-active-p session) t)
       (test-mevedel-publication-async--send
        session "active-owner" (lambda (ok err) (push (list ok err) acknowledgements)))
       (should-not (mevedel-session-publication-job session))
       (should (mevedel-session-publication-preparations session))
       (unwind-protect
           (cl-letf (((symbol-function 'accept-process-output)
                      (lambda (&rest _) (ert-fail "Barrier waited behind its synchronous owner"))))
             (should-error (mevedel-session-publication-barrier session) :type 'user-error))
         (setf (mevedel-session-publication-active-p session) nil))
       (mevedel-session-publication-barrier session)
       (should (equal '((t nil)) acknowledgements)))))

  :doc "a barrier re-kicks an existing job stranded between admission and freeze"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((advance (symbol-function 'mevedel-session-publication--advance-soon))
           paused acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-publication--advance-soon)
                  (lambda (job)
                    (if (eq 'freeze (mevedel-session-publication--job-phase job))
                        (setq paused t)
                      (funcall advance job)))))
         (test-mevedel-publication-async--send
          session "rekick" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () paused)))
       (should-not acknowledgements)
       (mevedel-session-publication-barrier session)
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "a bounded barrier times out without discarding or falsely acknowledging accepted work"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((admit (symbol-function 'mevedel-session-durability-admit-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-admit-async)
                  (lambda (owner callback)
                    (funcall admit owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-mevedel-publication-async--send
          session "timeout" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () continuation))
         (let ((mevedel-session-publication--barrier-seconds 0))
           (should-error (mevedel-session-publication-barrier session) :type 'user-error))
         (should-not acknowledgements)
         (should (mevedel-session-publication-busy-p session))
         (funcall continuation)
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "owner epoch replacement during admission fails closed with exactly one acknowledgement"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((admit (symbol-function 'mevedel-session-durability-admit-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-admit-async)
                  (lambda (owner callback)
                    (funcall admit owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-mevedel-publication-async--send
          session "epoch" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () continuation))
         (mevedel-session-checkpoint-seed
          session (mevedel-session-artifacts-build-sidecar session (mevedel-session-root-buffer session)))
         (funcall continuation)
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (= 1 (length acknowledgements)))
       (should-not (caar acknowledgements))
       (should-not (test-mevedel-publication-async--restored-mail session))
       (should-not (mevedel-session-publication-busy-p session)))))

  :doc "expired target lease between reservation and head cannot publish tentative mail"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((reserve (symbol-function 'mevedel-session-durability-reserve-publication-lease-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-mevedel-publication-async--send
          session "expired" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait (lambda () continuation))
         (let* ((directory (mevedel-session-durability--lease-path (mevedel-session-save-path session)))
                (lease (mevedel-session-durability--lease-head directory)))
           (setf (plist-get lease :renewed-at) 1 (plist-get lease :expires-at) 2)
           (mevedel-session-durability--write-generation directory lease))
         (funcall continuation)
         (test-mevedel-publication-async--wait (lambda () acknowledgements)))
       (should (= 1 (length acknowledgements)))
       (should-not (caar acknowledgements))
       (should-not (test-mevedel-publication-async--mail session))
       (should-not (test-mevedel-publication-async--restored-mail session))))))

(defun test-mevedel-publication-async--shutdown (kind)
  "Drain a running and queued delivery through the lifecycle boundary KIND."
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((admit (symbol-function 'mevedel-session-durability-admit-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-admit-async)
                  (lambda (owner callback)
                    (if continuation (funcall admit owner callback)
                      (funcall admit owner
                               (lambda (ok err)
                                 (setq continuation (lambda () (funcall callback ok err)))))))))
         (dolist (id '("closing-one" "closing-two"))
           (test-mevedel-publication-async--send
            session id (lambda (ok err) (push (list id ok err) acknowledgements))))
         (test-mevedel-publication-async--wait (lambda () continuation))
         (should-not acknowledgements)
         (run-at-time 0.01 nil continuation)
         (pcase kind
           ('buffer
            (with-current-buffer buffer
              (add-hook 'kill-buffer-query-functions #'mevedel-session-persistence-allow-buffer-close-p nil t)
              (add-hook 'kill-buffer-hook #'mevedel-session-persistence-release-on-kill nil t)
              (should (kill-buffer buffer))))
           ('exit
            (should (mevedel-session-persistence--allow-emacs-exit-p))
            (should-not (mevedel-session-persistence--stop-publications)))))
       (should (equal '(("closing-one" t nil) ("closing-two" t nil))
                      (reverse acknowledgements)))
       (should-not (mevedel-session-publication-busy-p session))
       (should (mevedel-session-publication-stopping-p session))
       (should (= 2 (length (test-mevedel-publication-async--restored-mail session))))
       (should-error (test-mevedel-publication-async--send session "too-late" #'ignore))))))

(mevedel-deftest mevedel-session-publication-stopping/async (:quiet t)
  ,test
  (test)
  :doc "buffer kill drains running and queued completions before releasing the lease"
  (test-mevedel-publication-async--shutdown 'buffer)
  :doc "Emacs exit query and stop phase drain running and queued completions"
  (test-mevedel-publication-async--shutdown 'exit))

(defun test-mevedel-publication-async--unresolved (committed action &optional queued)
  "Make a head outcome unobservable, then apply recovery ACTION.
COMMITTED selects whether the real head write ran before its result was lost.
QUEUED adds a second completion while the first job is unresolved."
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((commit (symbol-function 'mevedel-session-durability-commit-publication-head-async))
           (observe (symbol-function 'mevedel-session-durability-observe-publication-head-async))
           (observations 0) acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-commit-publication-head-async)
                  (lambda (owner head callback)
                    (if committed
                        (funcall commit owner head
                                 (lambda (ok err)
                                   (should ok) (should-not err)
                                   (funcall callback nil '(file-error "Lost head result"))))
                      (funcall callback nil '(file-error "Head write did not run")))))
                 ((symbol-function 'mevedel-session-durability-observe-publication-head-async)
                  (lambda (owner callback)
                    (funcall observe owner
                             (lambda (_head _err)
                               (cl-incf observations)
                               (funcall callback nil '(file-error "Head temporarily unreadable")))))))
         (test-mevedel-publication-async--send
          session "unresolved" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-mevedel-publication-async--wait
          (lambda ()
            (when-let* ((job (mevedel-session-publication-job session)))
              (eq 'unresolved (mevedel-session-publication--job-phase job))))))
       (should (= observations 1))
       (should-not acknowledgements)
       (should (mevedel-session-publication-busy-p session))
       (should-error (mevedel-session-publication-barrier session) :type 'user-error)
       (should (= (if committed 1 0)
                  (length (test-mevedel-publication-async--restored-mail session))))
       (when queued
         (test-mevedel-publication-async--send
          session "after-unresolved" (lambda (ok err) (push (list ok err) acknowledgements)))
         (should (mevedel-session-publication-preparations session)))
       (pcase action
         ('retry
          (should (equal (mevedel-session-publication-retry session)
                         (mevedel-session-publication session))))
         ('abandon
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (mevedel-session-publication-abandon session))))
       (test-mevedel-publication-async--wait
        (lambda () (= (if queued 2 1) (length acknowledgements))))
       (when queued (should (equal '((t nil) (t nil)) acknowledgements)))
       (should (eq committed (and (caar acknowledgements) t)))
       (should-not (mevedel-session-publication-busy-p session))
       (should-not (mevedel-session-pending-publication session))
       (should (= (+ (if committed 1 0) (if queued 1 0))
                  (length (test-mevedel-publication-async--mail session))))
       (should (= (+ (if committed 1 0) (if queued 1 0))
                  (length (test-mevedel-publication-async--restored-mail session))))))))

(mevedel-deftest mevedel-session-publication-retry/async (:quiet t)
  ,test
  (test)
  :doc "explicit retry reconciles an unresolved committed head and completes its original ACK"
  (test-mevedel-publication-async--unresolved t 'retry)
  :doc "retry succeeds when draining its queued successor advances the head again"
  (test-mevedel-publication-async--unresolved t 'retry t))

(mevedel-deftest mevedel-session-publication-abandon/async (:quiet t)
  ,test
  (test)
  :doc "explicit abandonment releases an unresolved uncommitted job and fails its original ACK"
  (test-mevedel-publication-async--unresolved nil 'abandon)

  :doc "abandoning retained checkpoint recovery deletes staging and permits close and exit"
  (dolist (kind '(buffer exit))
    (test-mevedel-publication-async--fixture
     (lambda (session buffer)
       (let ((head (plist-get (mevedel-session-publication session) :head))
             (commits 0) (observations 0) acknowledgements directories)
         (cl-letf (((symbol-function 'mevedel-session-durability-commit-publication-head-async)
                    (lambda (_owner _head callback)
                      (cl-incf commits)
                      (funcall callback nil '(file-error "Head write did not run"))))
                   ((symbol-function 'mevedel-session-durability-observe-publication-head-async)
                    (lambda (_owner callback)
                      (cl-incf observations)
                      (funcall callback nil '(file-error "Head temporarily unreadable")))))
           (mevedel-session-publication-submit
            session
            (lambda ()
              (test-mevedel-publication-async--transcript-artifacts
               session buffer "segment-9.chat.org" "Uncommitted checkpoint"))
            (lambda (ok err) (push (list ok err) acknowledgements)) nil t)
           (test-mevedel-publication-async--wait
            (lambda ()
              (when-let* ((job (mevedel-session-publication-job session)))
                (eq 'unresolved (mevedel-session-publication--job-phase job))))))
         (should (= commits 1))
         (should (= observations 1))
         (should-not acknowledgements)
         (should (mevedel-session-pending-publication session))
         (setq directories
               (mapcar (lambda (batch) (plist-get batch :directory))
                       (mevedel-session-publication--job-batches
                        (mevedel-session-publication-job session))))
         (should directories)
         (dolist (directory directories) (should (file-directory-p directory)))
         (with-current-buffer buffer
           (should-not (mevedel-session-persistence-allow-buffer-close-p)))
         (should-not (mevedel-session-persistence--allow-emacs-exit-p))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (should (mevedel-session-publication-abandon session)))
         (should (equal '((nil (error "Session publication was explicitly abandoned")))
                        acknowledgements))
         (should-not (mevedel-session-pending-publication session))
         (should-not (mevedel-session-publication-job session))
         (should-not (mevedel-session-publication-busy-p session))
         (should-not (mevedel-session-publication-active-p session))
         (should-not (mevedel-session-publication-uncommitted-batches session))
         (should-not (mevedel-session-publication-queue session))
         (dolist (directory directories) (should-not (file-exists-p directory)))
         (should (equal head (plist-get (mevedel-session-publication session) :head)))
         (should (mevedel-session-persistence--allow-emacs-exit-p))
         (with-current-buffer buffer
           (should (mevedel-session-persistence-allow-buffer-close-p)))
         (pcase kind
           ('buffer
            (with-current-buffer buffer
              (add-hook 'kill-buffer-query-functions
                        #'mevedel-session-persistence-allow-buffer-close-p nil t)
              (add-hook 'kill-buffer-hook #'mevedel-session-persistence-release-on-kill nil t)
              (should (kill-buffer buffer))))
           ('exit (should-not (mevedel-session-persistence--stop-publications))))
         (accept-process-output nil 0.05)
         (should (= 1 (length acknowledgements)))
         (should-not (mevedel-session-pending-publication session)))))))

(mevedel-deftest mevedel-agent-control-reoffer-mail/async (:quiet t)
  ,test
  (test)
  :doc "a close veto after nested drains reoffers two suppressed wakes once in FIFO order"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((notify (symbol-function 'mevedel-agent-control-notify))
           (stop (symbol-function 'mevedel-session-publication-stopping))
           reasons wake-tokens acknowledgements stopping-states offers vetoed nested)
       (mevedel-agent-control-wait
        session (lambda (reason)
                  (push (mevedel-session-publication-preparation-token session) wake-tokens)
                  (push reason reasons)) 10000)
       (should (mevedel-agent-control-root-waiting-p session))
       (cl-letf (((symbol-function 'mevedel-agent-control-notify)
                  (lambda (owner key function)
                    (funcall notify owner key
                             (lambda () (push (cadr key) offers) (funcall function)))))
                 ((symbol-function 'mevedel-session-publication-stopping)
                  (lambda (owner)
                    (prog1 (funcall stop owner)
                      (unless nested
                        (setq nested t)
                        (unwind-protect
                            (mevedel-session-persistence--drain-temporarily owner)
                          (setq nested nil)))))))
         (dolist (id '("veto-first" "veto-second"))
           (test-mevedel-publication-async--send
            session id
            (lambda (ok err)
              (push (mevedel-session-publication-stopping-p session) stopping-states)
              (push (list id ok err) acknowledgements))))
         (with-current-buffer buffer
           (setq-local kill-buffer-query-functions
                       (list #'mevedel-session-persistence-allow-buffer-close-p
                             (lambda () (setq vetoed t) nil)))
           (should-not (kill-buffer buffer))))
       (should vetoed)
       (should (buffer-live-p buffer))
       (should (equal '(t t) stopping-states))
       (should (equal '(("veto-first" t nil) ("veto-second" t nil))
                      (reverse acknowledgements)))
       (should-not (mevedel-session-publication-stopping-p session))
       (should-not reasons)
       (should-not offers)
       (should (= 2 (length (mevedel-session-mail-notifications session))))
       (let ((timer (mevedel-session-mail-notification-timer session)))
         (should (timerp timer))
         (mevedel-session-persistence--drain-temporarily session)
         (mevedel-agent-control-reoffer-mail session)
         (should (eq timer (mevedel-session-mail-notification-timer session))))
       (test-mevedel-publication-async--wait (lambda () (= 2 (length offers))))
       (should (equal (mapcar (lambda (id) (secure-hash 'sha256 (concat "/root:" id)))
                             '("veto-first" "veto-second"))
                      (reverse offers)))
       (should (equal '(mailbox) reasons))
       (should (equal '(nil) wake-tokens))
       (should-not (mevedel-agent-control-root-waiting-p session))
       (mevedel-agent-control-reoffer-mail session)
       (accept-process-output nil 0.02)
       (should (= 2 (length offers)))
       (should (equal '(mailbox) reasons))
       (should-not (mevedel-session-mail-notification-timer session))
       (should-not (mevedel-session-mail-notifications session))
       (should (= 2 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "a completed close suppresses the query's deferred wake reoffer"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let (reasons acknowledgements)
       (mevedel-agent-control-wait session (lambda (reason) (push reason reasons)) 10000)
       (test-mevedel-publication-async--send
        session "closed-with-mail" (lambda (ok err) (push (list ok err) acknowledgements)))
       (with-current-buffer buffer
         (setq-local kill-buffer-query-functions
                     (list #'mevedel-session-persistence-allow-buffer-close-p)
                     kill-buffer-hook
                     (list #'mevedel-session-persistence-release-on-kill
                           #'mevedel-view--on-data-killed))
         (should (kill-buffer buffer)))
       (should-not (buffer-live-p buffer))
       (should (equal '((t nil)) acknowledgements))
       (should (mevedel-session-publication-stopping-p session))
       (should-not (mevedel-session-mail-notifications session))
       (should-not (mevedel-session-mail-notification-timer session))
       (accept-process-output nil 0.05)
       (should-not reasons)
       (should (= 1 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "an idle close query with no suppressed offers allocates no notification timer"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((timers (append timer-list timer-idle-list)))
       (with-current-buffer buffer
         (should (mevedel-session-persistence-allow-buffer-close-p)))
       (should-not (mevedel-session-mail-notifications session))
       (should-not (mevedel-session-mail-notification-timer session))
       (should-not (seq-difference (append timer-list timer-idle-list) timers))))))

(defun test-mevedel-publication-async--notification-cancel (uninstall)
  "Cancel transport with pending notifications, using full UNINSTALL if non-nil."
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((mevedel-transport--enabled-p t)
           (admit (symbol-function 'mevedel-session-durability-admit-async))
           (wake (symbol-function 'mevedel-agent-control--wake))
           (admissions 0) (offers 0) continuation reasons acknowledgements)
       (mevedel-agent-control-wait session (lambda (reason) (push reason reasons)) 10000)
       (cl-letf (((symbol-function 'mevedel-session-durability-admit-async)
                  (lambda (owner callback)
                    (cl-incf admissions)
                    (funcall admit owner
                             (lambda (ok err)
                               (if (= admissions 1) (funcall callback ok err)
                                 (setq continuation (lambda () (funcall callback ok err))))))))
                 ((symbol-function 'mevedel-agent-control--wake)
                  (lambda (&rest arguments) (cl-incf offers) (apply wake arguments))))
         (dolist (id '("cancel-notify-one" "cancel-notify-two"))
           (test-mevedel-publication-async--send
            session id (lambda (ok err) (push (list id ok err) acknowledgements))))
         (test-mevedel-publication-async--wait (lambda () continuation))
         (should (= 1 (length (mevedel-session-mail-notifications session))))
         (should (mevedel-session-mail-notification-timer session))
         (should-not reasons)
         (if uninstall
             (progn (mevedel-uninstall) (should-not mevedel-transport--enabled-p))
           (mevedel-transport-cancel-pending))
         (funcall continuation)
         (test-mevedel-publication-async--wait
          (lambda () (and (= 2 (length acknowledgements)) (= offers 2))))
         (accept-process-output nil 0.1)
         (should (= offers 2))
         (should (equal '(mailbox) reasons))
         (should (equal '(("cancel-notify-one" t nil) ("cancel-notify-two" t nil))
                        (reverse acknowledgements)))
         (should-not (mevedel-session-publication-busy-p session))
         (should-not (mevedel-session-mail-notifications session))
         (should-not (mevedel-session-mail-notification-timer session))
         (should (= 2 (length (test-mevedel-publication-async--restored-mail session)))))))))

(mevedel-deftest mevedel-agent-control--deliver-mail/async (:quiet t)
  ,test
  (test)
  :doc "a queued delivery wakes its waiter once outside the preparation token"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (reasons wake-tokens acknowledgements)
       (mevedel-agent-control-wait
        session (lambda (reason)
                  (push (mevedel-session-publication-preparation-token session) wake-tokens)
                  (push reason reasons)) 10000)
       (dolist (id '("wake-first" "wake-second"))
         (test-mevedel-publication-async--send
          session id (lambda (ok err) (push (list id ok err) acknowledgements))))
       (test-mevedel-publication-async--wait
        (lambda () (and (= 2 (length acknowledgements)) reasons)))
       (mevedel-session-publication-barrier session)
       (accept-process-output nil 0.05)
       (should (equal '(mailbox) reasons))
       (should (equal '(nil) wake-tokens))
       (should (equal '(("wake-first" t nil) ("wake-second" t nil))
                      (reverse acknowledgements)))
       (should (= 2 (length (test-mevedel-publication-async--restored-mail session)))))))

  :doc "global transport cancellation cannot strand an accepted coalesced notification"
  (test-mevedel-publication-async--notification-cancel nil)

  :doc "full uninstall disables transport without stranding accepted coalesced notifications"
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil t nil "--batch" "-Q" "--eval"
            (prin1-to-string
             `(progn
                (setq load-path ',load-path)
                (when (native-comp-available-p)
                  (startup-redirect-eln-cache (file-name-concat (getenv "HOME") ".cache" "eln")))
                (require 'mevedel)
                (load ,(symbol-file 'test-mevedel-publication-async--notification-cancel 'defun) nil t)
                (let (diagnostics)
                  (mevedel-test--with-captured-diagnostics diagnostics
                    (test-mevedel-publication-async--notification-cancel t))
                  (should (equal "mevedel uninstalled successfully\n" diagnostics))))))))
      (ert-info ((buffer-string))
        (should (equal 0 status))
        (should (equal "" (buffer-string))))))

  :doc "notifications stay coalesced through successive busy jobs and wake once in FIFO order"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((admit (symbol-function 'mevedel-session-durability-admit-async))
           (queue (symbol-function 'mevedel-agent-control--queue-notification))
           (wake (symbol-function 'mevedel-agent-control--wake))
           (flush (symbol-function 'mevedel-agent-control--flush-notifications))
           (admissions 0) (flushes 0) continuation delivering offers acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-durability-admit-async)
                  (lambda (owner callback)
                    (cl-incf admissions)
                    (funcall admit owner
                             (lambda (ok err)
                               (if (= admissions 1) (funcall callback ok err)
                                 (setq continuation (lambda () (funcall callback ok err))))))))
                 ((symbol-function 'mevedel-agent-control--queue-notification)
                  (lambda (owner key function)
                    (funcall queue owner key
                             (lambda ()
                               (let ((previous delivering))
                                 (setq delivering (cadr key))
                                 (unwind-protect (funcall function)
                                   (setq delivering previous)))))))
                 ((symbol-function 'mevedel-agent-control--wake)
                  (lambda (&rest arguments)
                    (should-not (mevedel-session-publication-preparation-token session))
                    (should-not (mevedel-session-publication-busy-p session))
                    (push delivering offers)
                    (apply wake arguments)))
                 ((symbol-function 'mevedel-agent-control--flush-notifications)
                  (lambda (owner) (cl-incf flushes) (funcall flush owner))))
         (dolist (id '("fifo-one" "fifo-two" "fifo-three"))
           (test-mevedel-publication-async--send
            session id (lambda (ok err) (push (list id ok err) acknowledgements))))
         (dotimes (stage 2)
           (test-mevedel-publication-async--wait (lambda () continuation))
           (should (= (1+ stage) (length acknowledgements)))
           (should (= (1+ stage) (length (mevedel-session-mail-notifications session))))
           ;; Reoffering the same committed record must neither duplicate its
           ;; pending notification nor create another polling timer.
           (let ((timer (mevedel-session-mail-notification-timer session)))
             (mevedel-agent-control--deliver-mail
              session "/root" (car (test-mevedel-publication-async--mail session)))
             (should (eq timer (mevedel-session-mail-notification-timer session)))
             (should (= (1+ stage) (length (mevedel-session-mail-notifications session)))))
           (let ((target (+ flushes 2)))
             (test-mevedel-publication-async--wait (lambda () (>= flushes target))))
           (should-not offers)
           (should (<= 0.05 (mevedel-session-mail-notification-delay session) 0.5))
           (should (= 1 (cl-count-if
                         (lambda (timer)
                           (and (eq (timer--function timer)
                                    'mevedel-agent-control--flush-notifications)
                                (eq (car (timer--args timer)) session))) timer-list)))
           (let ((resume continuation))
             (setq continuation nil)
             (funcall resume)))
         (test-mevedel-publication-async--wait (lambda () (= 3 (length offers))))
         (accept-process-output nil 0.05)
         (should (equal (mapcar (lambda (id) (secure-hash 'sha256 (concat "/root:" id)))
                               '("fifo-one" "fifo-two" "fifo-three"))
                        (reverse offers)))
         (should (equal '(("fifo-one" t nil) ("fifo-two" t nil) ("fifo-three" t nil))
                        (reverse acknowledgements)))
         (should-not (mevedel-session-mail-notifications session))
         (should-not (mevedel-session-mail-notification-timer session))
         (should-not (mevedel-session-publication-busy-p session))
         (should (= 3 (length (test-mevedel-publication-async--restored-mail session)))))))))

(mevedel-deftest mevedel-agent-control--notification-blocked-p/async (:quiet t)
  ,test
  (test)
  :doc "notification admission respects both synchronous ownership and transport activity"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (should-not (mevedel-agent-control--notification-blocked-p session))
     (setf (mevedel-session-publication-active-p session) t)
     (should (mevedel-agent-control--notification-blocked-p session))
     (setf (mevedel-session-publication-active-p session) nil)
     (cl-letf (((symbol-function 'mevedel-transport-busy-p) (lambda (_) t)))
       (should (mevedel-agent-control--notification-blocked-p session)))
     (should-not (mevedel-agent-control--notification-blocked-p session)))))

(mevedel-deftest mevedel-agent-control--queue-notification/async (:quiet t)
  ,test
  (test)
  :doc "duplicate keys coalesce without replacing the original FIFO callback or timer"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (offers)
       (mevedel-agent-control--queue-notification session 'first (lambda () (push 'first offers)))
       (let ((timer (mevedel-session-mail-notification-timer session)))
         (mevedel-agent-control--queue-notification session 'first (lambda () (push 'duplicate offers)))
         (mevedel-agent-control--queue-notification session 'second (lambda () (push 'second offers)))
         (should (eq timer (mevedel-session-mail-notification-timer session))))
       (should (= 2 (length (mevedel-session-mail-notifications session))))
       (test-mevedel-publication-async--wait (lambda () (= 2 (length offers))))
       (should (equal '(first second) (reverse offers)))
       (should-not (mevedel-session-mail-notification-timer session))
       (should-not (mevedel-session-mail-notification-delay session))))))

(mevedel-deftest mevedel-agent-control--flush-notifications/async (:quiet t)
  ,test
  (test)
  :doc "a temporary stop retains deferred wake offers without a timer until reoffered"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (offered)
       (mevedel-agent-control--queue-notification session 'stopped (lambda () (setq offered t)))
       (setf (mevedel-session-publication-stopping-p session) t)
       (test-mevedel-publication-async--wait
        (lambda () (not (mevedel-session-mail-notification-timer session))))
       (should-not offered)
       (should (= 1 (length (mevedel-session-mail-notifications session))))
       (setf (mevedel-session-publication-stopping-p session) nil)
       (mevedel-agent-control-reoffer-mail session)
       (should-not offered)
       (test-mevedel-publication-async--wait (lambda () offered))
       (should-not (mevedel-session-mail-notifications session)))))

  :doc "a notification timer cannot inherit another transaction's dynamically bound authority"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (inherited)
       (mevedel-agent-control--queue-notification
        session 'authority
        (lambda ()
          (setq inherited
                (list mevedel-session-durability--transaction-clock
                      mevedel-session-durability--asserted-directories
                      mevedel-session-durability--commit-riders
                      mevedel-session-durability--final-commit
                      mevedel-session-durability-async-process-function
                      mevedel-session-recovery--mutation-cache))))
       (let ((mevedel-session-durability--transaction-clock '(sentinel))
             (mevedel-session-durability--asserted-directories '(sentinel))
             (mevedel-session-durability--commit-riders '(sentinel))
             (mevedel-session-durability--final-commit 'sentinel)
             (mevedel-session-durability-async-process-function #'ignore)
             (mevedel-session-recovery--mutation-cache '(sentinel)))
         (test-mevedel-publication-async--wait (lambda () inherited)))
       (should (equal '(nil nil nil nil nil nil) inherited))))))

(mevedel-deftest mevedel-agent-control--cancel-notifications/async (:quiet t)
  ,test
  (test)
  :doc "teardown cancels the owned timer and clears every notification slot"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (offered)
       (mevedel-agent-control--queue-notification session 'cancelled (lambda () (setq offered t)))
       (let ((timer (mevedel-session-mail-notification-timer session)))
         (mevedel-agent-control-teardown-session session)
         (should-not (memq timer timer-list)))
       (accept-process-output nil 0.1)
       (should-not offered)
       (should-not (mevedel-session-mail-notifications session))
       (should-not (mevedel-session-mail-notification-timer session))
       (should-not (mevedel-session-mail-notification-delay session))))))

(mevedel-deftest mevedel-session-publication-busy-p/async (:quiet t)
  ,test
  (test)
  :doc "accepted jobs stay busy until their acknowledgements settle"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledged)
       (should-not (mevedel-session-publication-busy-p session))
       (test-mevedel-publication-async--send
        session "busy" (lambda (ok err) (should ok) (should-not err) (setq acknowledged t)))
       (should (mevedel-session-publication-busy-p session))
       (test-mevedel-publication-async--wait (lambda () acknowledged))
       (should-not (mevedel-session-publication-busy-p session))))))

(mevedel-deftest mevedel-session-publication-async-eligible-p/async (:quiet t)
  ,test
  (test)
  :doc "only warm local portable sessions with a live root are eligible"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (should (mevedel-session-publication-async-eligible-p session))
     (let ((path (mevedel-session-save-path session))
           (root (mevedel-session-root-buffer session))
           (publication (mevedel-session-publication session)))
       (unwind-protect
           (progn
             (setf (mevedel-session-save-path session) "/ssh:uncontacted.invalid:/session/")
             (should-not (mevedel-session-publication-async-eligible-p session))
             (setf (mevedel-session-save-path session) path
                   (mevedel-session-publication session) nil)
             (should-not (mevedel-session-publication-async-eligible-p session))
             (setf (mevedel-session-publication session) publication
                   (mevedel-session-root-buffer session) nil)
             (should-not (mevedel-session-publication-async-eligible-p session))
             (setf (mevedel-session-root-buffer session) root
                   (mevedel-session-publication-stopping-p session) t)
             (should-not (mevedel-session-publication-async-eligible-p session)))
         (setf (mevedel-session-save-path session) path
               (mevedel-session-root-buffer session) root
               (mevedel-session-publication session) publication
               (mevedel-session-publication-stopping-p session) nil))))))

(mevedel-deftest mevedel-session-publication--write-diagnostics/async (:quiet t)
  ,test
  (test)
  :doc "exit drain persists an append queued behind an in-flight append and empties the backlog"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let ((path (file-name-concat (mevedel-session-save-path session) "exit-diagnostics.log"))
           (mevedel-session-publication--diagnostic-backlog (make-hash-table :test #'eq))
           (mevedel-session-control-fs--async-jobs nil)
           (mevedel-session-control-fs--async-wait nil))
       (mevedel-session-publication--write-diagnostics
        session (list (cons path "first\n")))
       ;; No event-loop opportunity has occurred: the first program has not
       ;; delivered, so its successor must join the backlog, not start I/O.
       (should mevedel-session-control-fs--async-jobs)
       (should (car (gethash session mevedel-session-publication--diagnostic-backlog)))
       (mevedel-session-publication--write-diagnostics
        session (list (cons path "second\n")))
       (should (equal (list (cons path "second\n"))
                      (cdr (gethash session mevedel-session-publication--diagnostic-backlog))))
       (should (mevedel-session-publication-diagnostic-backlog-p session))
       (let ((mevedel-session-control-fs--async-wait t))
         (mevedel-session-publication--write-diagnostics session nil))
       ;; Assert immediately after the exit flush, without a later save or
       ;; explicit wait that could conceal a stranded append.
       (should-not mevedel-session-control-fs--async-jobs)
       (should-not (car (gethash session mevedel-session-publication--diagnostic-backlog)))
       (should-not (mevedel-session-publication-diagnostic-backlog-p session))
       (should (equal "first\nsecond\n"
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))))))))

(defun test-mevedel-publication-async--staged-submit
    (session buffer select &optional logical content retain)
  "Submit LOGICAL's CONTENT through SESSION with SELECT; return the ack list."
  (let (acknowledgements)
    (mevedel-session-publication-submit
     session
     (lambda ()
       (test-mevedel-publication-async--transcript-artifacts
        session buffer (or logical "segment-8.chat.org") (or content "Staged text")))
     (lambda (ok err) (push (list ok err) acknowledgements))
     nil retain select)
    (test-mevedel-publication-async--wait (lambda () acknowledgements))
    acknowledgements))

(mevedel-deftest mevedel-session-publication--write-staged-content ()
  ,test
  (test)
  :doc "writes exactly the encoded bytes for Unicode, codings, raw and unibyte content"
  (let ((file (make-temp-file "mevedel-staged-write-"))
        (annotated nil))
    (unwind-protect
        (cl-letf (((default-value 'write-region-annotate-functions)
                   (list (lambda (&rest _) (setq annotated t) nil))))
          (dolist (spec `(("λ ünï \U0001F600\nline" utf-8-unix)
                          ("one\ntwo\n" utf-8-dos)
                          (,(string-to-multibyte "raw\200\377") utf-8-unix)
                          (,(unibyte-string 0 1 128 255 10) nil)))
            (let ((content (car spec)) (coding (cadr spec)))
              (with-temp-buffer
                (setq-local buffer-file-coding-system 'iso-8859-1)
                (insert "live buffer text")
                (let ((before (buffer-string)) (point (point)))
                  (mevedel-session-publication--write-staged-content
                   file content coding)
                  (should (equal before (buffer-string)))
                  (should (= point (point)))))
              (should (equal (if (multibyte-string-p content)
                                 (encode-coding-string content (or coding 'utf-8-unix))
                               content)
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally file)
                               (buffer-string))))))
          (should-not annotated))
      (delete-file file))))

(mevedel-deftest mevedel-session-publication--valid-digest-p ()
  ,test
  (test)
  :doc "accepts only lowercase 64-character hexadecimal digests"
  (should (mevedel-session-publication--valid-digest-p (secure-hash 'sha256 "x")))
  (dolist (bad (list nil 1 "" (make-string 64 ?A) (make-string 63 ?a)
                     (make-string 65 ?a) (concat (make-string 63 ?a) "g")
                     (concat (make-string 64 ?a) "\n")))
    (should-not (mevedel-session-publication--valid-digest-p bad))))

(mevedel-deftest mevedel-session-publication--require-digests ()
  ,test
  (test)
  :doc "demands a valid digest on every written artifact and ignores deletions"
  (let ((digest (secure-hash 'sha256 "x")))
    (should-not (mevedel-session-publication--require-digests
                 (list (list :logical "a" :source-sha256 digest)
                       (list :logical "b" :delete t))))
    (dolist (bad (list (list :logical "a")
                       (list :logical "a" :source-sha256 "short")))
      (should-error (mevedel-session-publication--require-digests (list bad))))))

(mevedel-deftest mevedel-session-publication--job-digest (:quiet t)
  ,test
  (test)
  :doc "hashes staged files with the real control program and never on the main thread"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let (acknowledgements)
       (cl-letf (((symbol-function 'mevedel-session-publication--files-sha256)
                  (lambda (&rest _) (error "Main-thread hash")))
                 ((symbol-function 'mevedel-session-publication--file-sha256)
                  (lambda (&rest _) (error "Main-thread hash")))
                 ((symbol-function 'mevedel-session-publication--generation-digests)
                  (lambda (&rest _) (error "Main-thread hash"))))
         (setq acknowledgements
               (test-mevedel-publication-async--staged-submit
                session buffer nil "segment-9.chat.org" "λ digest me")))
       (should (equal '((t nil)) acknowledgements))
       (should (equal (encode-coding-string "λ digest me" 'utf-8-unix)
                      (mevedel-session-artifacts-read-artifact
                       session "segment-9.chat.org" t))))))

  :doc "fails the job and rolls back on a failed, missing or malformed result"
  (dolist (outcome (list (lambda (_ops callback)
                           (funcall callback nil '(file-error "Injected hash failure")) nil)
                         (lambda (_ops callback) (funcall callback nil nil) nil)
                         (lambda (ops callback)
                           (funcall callback
                                    (mapcar (lambda (_) (list :status 'ok :value (list :sha256 "bad")))
                                            ops)
                                    nil)
                           nil)
                         (lambda (ops callback)
                           (funcall callback
                                    (mapcar (lambda (_) (list :status 'failed)) ops) nil)
                           nil)))
    (test-mevedel-publication-async--fixture
     (lambda (session buffer)
       (let ((head (plist-get (mevedel-session-publication session) :head)))
         (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program-async)
                    outcome))
           (let ((acknowledgements
                  (test-mevedel-publication-async--staged-submit
                   session buffer nil "segment-9.chat.org" "never published")))
             (should-not (caar acknowledgements))
             (should (cadar acknowledgements))))
         (should (equal head (plist-get (mevedel-session-publication session) :head)))
         (should-not (mevedel-session-publication-busy-p session))))))
  :doc "hashes undigested sources of retained earlier batches with the same program"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((save-path (mevedel-session-save-path session))
            (retained (mevedel-session-publication--stage-artifacts
                       session
                       (list (list :path (file-name-concat save-path "segment-6.chat.org")
                                   :content "λ retained"))))
            (hashed nil))
       (should-not (plist-get (car (plist-get retained :artifacts)) :source-sha256))
       (mevedel-session-publication--retain-uncommitted-batch session retained)
       (cl-letf* ((orig (symbol-function 'mevedel-session-control-fs-run-program-async))
                  ((symbol-function 'mevedel-session-control-fs-run-program-async)
                   (lambda (operations callback)
                     (setq hashed (append hashed
                                          (mapcar (lambda (op) (plist-get op :path))
                                                  operations)))
                     (funcall orig operations callback))))
         (should (equal '((t nil))
                        (test-mevedel-publication-async--staged-submit
                         session buffer nil "segment-5.chat.org" "new"))))
       (should (cl-some (lambda (path) (string-match-p "mevedel-publication-" path)) hashed))
       ;; Retained and new sources both went through the one program.
       (should (>= (length hashed) 2))
       (should (equal (encode-coding-string "λ retained" 'utf-8-unix)
                      (mevedel-session-artifacts-read-artifact
                       session "segment-6.chat.org" t)))
       (should (equal "new" (mevedel-session-artifacts-read-artifact
                             session "segment-5.chat.org" t))))))

  :doc "a live root edit during the digest cannot change the frozen published bytes"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((orig (symbol-function 'mevedel-session-control-fs-run-program-async))
           edited)
       (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program-async)
                  (lambda (operations callback)
                    (unless edited
                      (setq edited t)
                      (with-current-buffer buffer
                        (goto-char (point-max))
                        (insert "LIVE EDIT DURING DIGEST")))
                    (funcall orig operations callback))))
         (should (equal '((t nil))
                        (test-mevedel-publication-async--staged-submit
                         session buffer nil "segment-4.chat.org" "frozen text"))))
       (should edited)
       (should (equal "frozen text"
                      (mevedel-session-artifacts-read-artifact
                       session "segment-4.chat.org" t)))
       (with-current-buffer buffer
         (should (string-match-p "LIVE EDIT DURING DIGEST" (buffer-string))))))))

(mevedel-deftest mevedel-session-publication--job-select (:quiet t)
  ,test
  (test)
  :doc "a nil selector is a clean no-op that commits nothing and leaves no staging"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           directories)
       (cl-letf* ((orig (symbol-function 'mevedel-session-publication--stage-artifacts))
                  ((symbol-function 'mevedel-session-publication--stage-artifacts)
                   (lambda (&rest args)
                     (let ((batch (apply orig args)))
                       (push (plist-get batch :directory) directories)
                       batch))))
         (should (equal '((t nil))
                        (test-mevedel-publication-async--staged-submit
                         session buffer (lambda (_batch) nil)))))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should directories)
       (dolist (directory directories) (should-not (file-exists-p directory)))
       (should-not (mevedel-session-publication-busy-p session)))))

  :doc "the selector sees digests, its batch alone is published and the original is cleaned"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let (seen original-directory)
       (should (equal
                '((t nil))
                (test-mevedel-publication-async--staged-submit
                 session buffer
                 (lambda (batch)
                   (setq seen (mapcar (lambda (artifact) (plist-get artifact :source-sha256))
                                      (plist-get batch :artifacts))
                         original-directory (plist-get batch :directory))
                   (let ((copy (copy-sequence batch)))
                     (setf (plist-get copy :artifacts)
                           (cl-remove-if
                            (lambda (artifact)
                              (equal (plist-get artifact :logical) "notes/deep/extra.txt"))
                            (plist-get batch :artifacts)))
                     copy))
                 "segment-7.chat.org" "kept")))
       (should (cl-every #'mevedel-session-publication--valid-digest-p seen))
       (should-not (file-exists-p original-directory))
       (should (equal "kept" (mevedel-session-artifacts-read-artifact
                              session "segment-7.chat.org" t)))
       (should-not (mevedel-session-artifacts-artifact-present-p
                    session "notes/deep/extra.txt" t)))))

  :doc "a selected batch without a valid digest fails before anything is reserved"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           reserved)
       (cl-letf (((symbol-function 'mevedel-session-durability-reserve-publication-lease-async)
                  (lambda (&rest _) (setq reserved t))))
         ;; The same batch comes back, but its commit marker was pruned.
         (let ((acknowledgements
                (test-mevedel-publication-async--staged-submit
                 session buffer
                 (lambda (batch)
                   (setf (plist-get batch :artifacts)
                         (cl-remove-if (lambda (artifact) (plist-get artifact :commit-marker))
                                       (plist-get batch :artifacts)))
                   batch))))
           (should-not (caar acknowledgements))
           (should (cadar acknowledgements))))
       (should-not reserved)
       (should (equal head (plist-get (mevedel-session-publication session) :head))))))

  :doc "a selected batch without a valid digest fails before anything is reserved"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head))
           reserved)
       (cl-letf (((symbol-function 'mevedel-session-durability-reserve-publication-lease-async)
                  (lambda (&rest _) (setq reserved t))))
         (let ((acknowledgements
                (test-mevedel-publication-async--staged-submit
                 session buffer
                 (lambda (batch)
                   (let ((copy (copy-sequence batch)))
                     (setf (plist-get copy :artifacts)
                           (mapcar (lambda (artifact)
                                     (let ((a (copy-sequence artifact)))
                                       (setf (plist-get a :source-sha256) "forged")
                                       a))
                                   (plist-get batch :artifacts)))
                     copy)))))
           (should-not (caar acknowledgements))
           (should (cadar acknowledgements))))
       (should-not reserved)
       (should (equal head (plist-get (mevedel-session-publication session) :head))))))
  :doc "a nil selector is refused while retained recovery is pending and keeps it"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((save-path (mevedel-session-save-path session))
            (retained (mevedel-session-publication--stage-artifacts
                       session
                       (list (list :path (file-name-concat save-path "segment-3.chat.org")
                                   :content "retained recovery"))))
            (head (plist-get (mevedel-session-publication session) :head)))
       (mevedel-session-publication--retain-uncommitted-batch session retained)
       (let ((acknowledgements
              (test-mevedel-publication-async--staged-submit
               session buffer (lambda (_batch) nil))))
         (should-not (caar acknowledgements))
         (should (cadar acknowledgements)))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should (= 1 (length (mevedel-session-publication-uncommitted-batches session))))
       (should (file-exists-p (plist-get retained :directory)))
       (should-not (mevedel-session-publication-busy-p session)))))
)

(provide 'test-mevedel-session-publication-async)
;;; test-mevedel-session-publication-async.el ends here
