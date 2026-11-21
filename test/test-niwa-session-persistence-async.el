;;; test-niwa-session-persistence-async.el --- Async checkpoint integration -*- lexical-binding: t -*-

;;; Commentary:

;; Adversarial integration tests for the background checkpoint entry
;; `niwa-session-persistence-checkpoint-buffer'.  Each scenario runs on a
;; warm local portable session with real storage and holds real publication
;; jobs at lease reservation to interleave competing writers.

;;; Code:

(require 'niwa-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-publication-test-helpers"))
(require 'niwa-agent-runtime)
(require 'niwa-recovery)
(require 'niwa-session-codec)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)

(defconst test-niwa-persistence-async--root-transcript
  "segment-0001.chat.org"
  "Logical name of the first root transcript segment.")

(defun test-niwa-persistence-async--hold (count function)
  "Call FUNCTION while the next COUNT lease reservations are held.
FUNCTION receives a function returning the continuations captured so far,
oldest first, and a function releasing the oldest unreleased one from a
timer.  Reservations beyond COUNT run normally."
  (let ((reserve (symbol-function
                  'niwa-session-durability-reserve-publication-lease-async))
        (held nil) (released 0) (seen 0))
    (cl-letf (((symbol-function
                'niwa-session-durability-reserve-publication-lease-async)
               (lambda (owner callback)
                 (if (>= seen count)
                     (funcall reserve owner callback)
                   (cl-incf seen)
                   (funcall reserve owner
                            (lambda (ok err)
                              (setq held
                                    (append held
                                            (list (lambda ()
                                                    (funcall callback ok err)))))))))))
      (funcall function
               (lambda () held)
               (lambda ()
                 (let ((continuation (nth released held)))
                   (cl-incf released)
                   (run-at-time 0.01 nil continuation)))))))

(defun test-niwa-persistence-async--idle (session)
  "Wait until SESSION's publication gate is idle."
  (test-niwa-publication-async--wait
   (lambda () (not (niwa-session-publication-busy-p session)))))

(defun test-niwa-persistence-async--disk (session logical)
  "Return SESSION's committed artifact LOGICAL, or nil when absent."
  (ignore-errors
    (niwa-session-artifacts-read-artifact session logical t)))

(defun test-niwa-persistence-async--head (session)
  "Return SESSION's current publication head."
  (plist-get (niwa-session-publication session) :head))

(defun test-niwa-persistence-async--count (needle text)
  "Return how many times NEEDLE occurs in TEXT."
  (let ((count 0) (start 0) (text (or text "")))
    (while (string-match (regexp-quote needle) text start)
      (cl-incf count)
      (setq start (match-end 0)))
    count))

(defun test-niwa-persistence-async--restore (session)
  "Return SESSION's authoritative state read fresh from storage.
Neither the session's captured publication nor any cache is consulted: the
publication, its root transcript segment and sidecar are read and decoded
anew.  The result is a plist of :transcript, :turns, :latest and :mail IDs."
  (let* ((niwa-session-publication--generation-cache (make-hash-table :test #'equal))
         (niwa-session-publication--facts-cache (make-hash-table :test #'equal))
         (publication (niwa-session-publication-read
                       (niwa-session-save-path session)))
         (raw (niwa-session-persistence-load-sidecar
               (plist-get publication :sidecar)))
         (restored (niwa-session-codec-deserialize
                    raw (niwa-session-workspace session))))
    (list :transcript (niwa-session-artifacts--read-published-artifact
                       publication test-niwa-persistence-async--root-transcript)
          :turns (plist-get raw :total-turn-count)
          :latest (plist-get raw :latest-user-message)
          :mail (test-niwa-publication-async--ids
                 (test-niwa-publication-async--mail
                  (plist-get restored :session))))))

(defun test-niwa-persistence-async--type (buffer text)
  "Append TEXT as an unsaved model response to BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert (propertize text 'gptel 'response))))

(niwa-deftest niwa-session-persistence-checkpoint-buffer (:quiet t)
  ,test
  (test)
  :doc "a modified root commits in the background and restores from disk"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (test-niwa-persistence-async--type buffer "Checkpointed answer\n")
     (should (buffer-modified-p buffer))
     (niwa-session-persistence-checkpoint-buffer buffer)
     (test-niwa-persistence-async--idle session)
     (should-not (buffer-modified-p buffer))
     (should-not (buffer-local-value 'niwa-session--save-failed buffer))
     (should (= 1 (test-niwa-persistence-async--count
                   "Checkpointed answer"
                   (test-niwa-persistence-async--disk
                    session test-niwa-persistence-async--root-transcript))))))

  :doc "root and retained-agent checkpoints re-observe forgotten lease bytes"
  (test-niwa-publication-async--fixture
   (lambda (session root)
     (let* ((child (generate-new-buffer " *async-forgotten-lease-agent*"))
            (invocation (niwa-agent-invocation--create
                         :agent (niwa-agent--create :name "worker")
                         :agent-id "worker--forgotten-lease" :path "/root/worker"
                         :buffer child :parent-session session
                         :parent-data-buffer root)))
       (unwind-protect
           (progn
             (niwa-agent-runtime--setup-transcript invocation child)
             (with-current-buffer child
               (setq-local niwa--session session
                           niwa--agent-invocation invocation))
             (dolist (buffer (list root child))
               (setf (niwa-session-lease session)
                     (niwa-session-durability--strip-assumption
                      (niwa-session-lease session)))
               (test-niwa-persistence-async--type buffer "Re-observed checkpoint\n")
               (should (eq 'pending (niwa-session-persistence-checkpoint-buffer buffer)))
               (test-niwa-persistence-async--idle session)
               (should-not (buffer-local-value 'niwa-session--save-failed buffer))
               (should-not (buffer-modified-p buffer))
               (should (= 1 (test-niwa-persistence-async--count
                             "Re-observed checkpoint"
                             (test-niwa-persistence-async--disk
                              session
                              (if (eq buffer root)
                                  test-niwa-persistence-async--root-transcript
                                (niwa-agent-invocation-transcript-relative-path
                                 invocation))))))))
         (with-current-buffer child
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer child)))))

  :doc "forgotten lease bytes cannot publish over an unseen target head"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((cached (niwa-session-publication session))
           (directory (niwa-session-save-path session)))
       (niwa-session-publication-publish
        session
        (list (list :path (file-name-concat directory "unseen.txt") :content "Keep me")
              (list :path (file-name-concat directory "session.meta.el")
                    :content (niwa-session-artifacts-read-artifact session "session.meta.el" t)
                    :commit-marker t))
        t)
       (let ((target (niwa-session-publication session)))
         (should-not (equal (plist-get cached :head) (plist-get target :head)))
         (setf (niwa-session-publication session) cached
               (niwa-session-lease session)
               (niwa-session-durability--strip-assumption
                (niwa-session-lease session)))
         (test-niwa-persistence-async--type buffer "Stale checkpoint\n")
         (niwa-session-persistence-checkpoint-buffer buffer)
         (test-niwa-persistence-async--idle session)
         (should (buffer-local-value 'niwa-session--save-failed buffer))
         (should (buffer-modified-p buffer))
         (should (equal (plist-get target :head)
                        (niwa-session-durability-publication-head directory)))
         (setf (niwa-session-publication session)
               (niwa-session-publication-read directory))
         (should (equal "Keep me" (test-niwa-persistence-async--disk session "unseen.txt")))))))

  :doc "text typed while the job runs stays unsaved until the next checkpoint"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (test-niwa-persistence-async--hold
      1
      (lambda (held release)
        (test-niwa-persistence-async--type buffer "First text\n")
        (niwa-session-persistence-checkpoint-buffer buffer)
        (test-niwa-publication-async--wait (lambda () (funcall held)))
        (test-niwa-persistence-async--type buffer "Typed during job\n")
        (funcall release)
        (test-niwa-persistence-async--idle session)))
     (let ((disk (test-niwa-persistence-async--disk
                  session test-niwa-persistence-async--root-transcript)))
       (should (string-match-p "First text" disk))
       (should-not (string-match-p "Typed during job" disk)))
     ;; The edit made after the freeze is not reported saved.
     (should (buffer-modified-p buffer))
     (niwa-session-persistence-checkpoint-buffer buffer)
     (test-niwa-persistence-async--idle session)
     (should-not (buffer-modified-p buffer))
     (let ((disk (test-niwa-persistence-async--disk
                  session test-niwa-persistence-async--root-transcript)))
       (should (= 1 (test-niwa-persistence-async--count "Typed during job" disk)))
       (should (= 1 (test-niwa-persistence-async--count "First text" disk))))))

  :doc "a completion delivery overlapping a checkpoint commits both exactly once, in either order"
  (dolist (checkpoint-first '(nil t))
    (test-niwa-publication-async--fixture
     (lambda (session buffer)
       (let (acknowledgements)
         (test-niwa-persistence-async--hold
          1
          (lambda (held release)
            (test-niwa-persistence-async--type buffer "Overlap text\n")
            (if checkpoint-first
                (progn
                  (niwa-session-persistence-checkpoint-buffer buffer)
                  (test-niwa-publication-async--wait (lambda () (funcall held)))
                  (test-niwa-publication-async--send
                   session "overlap" (lambda (ok err) (push (list ok err) acknowledgements))))
              (test-niwa-publication-async--send
               session "overlap" (lambda (ok err) (push (list ok err) acknowledgements)))
              (test-niwa-publication-async--wait (lambda () (funcall held)))
              (niwa-session-persistence-checkpoint-buffer buffer))
            (funcall release)
            (test-niwa-persistence-async--idle session)))
         (should (equal '((t nil)) acknowledgements))
         (should-not (buffer-modified-p buffer))
         (should (equal (list (secure-hash 'sha256 "/root:overlap"))
                        (test-niwa-publication-async--ids
                         (test-niwa-publication-async--restored-mail session))))
         (should (= 1 (test-niwa-persistence-async--count
                       "Overlap text"
                       (test-niwa-persistence-async--disk
                        session test-niwa-persistence-async--root-transcript))))
         ;; The consumed mail's receipt retires with the transcript commit.
         (let ((mail (test-niwa-publication-async--mail session)))
           (niwa-session-checkpoint-consume session "/root" mail)
           (niwa-agent-control--set-mailbox-queue session "/root" nil)
           (test-niwa-persistence-async--type buffer "After consume\n")
           (niwa-session-persistence-checkpoint-buffer buffer)
           (test-niwa-persistence-async--idle session)
           (should-not (niwa-session-checkpoint-pending-message-p
                        session "/root" (plist-get (car mail) :id)))
           (should-not (test-niwa-publication-async--restored-mail session)))))))

  :doc "a checkpoint queued behind a synchronous save finds nothing to do"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (test-niwa-persistence-async--hold
      1
      (lambda (held release)
        (test-niwa-publication-async--send session "gate" #'ignore)
        (test-niwa-publication-async--wait (lambda () (funcall held)))
        (test-niwa-persistence-async--type buffer "Absorbed text\n")
        (niwa-session-persistence-checkpoint-buffer buffer)
        (funcall release)
        ;; The synchronous save drains the job and commits the edit first.
        (should (niwa-session-artifacts-save session buffer nil t))
        (let ((head (test-niwa-persistence-async--head session)))
          (test-niwa-persistence-async--idle session)
          (should (equal head (test-niwa-persistence-async--head session))))))
     (should-not (buffer-modified-p buffer))
     (should (= 1 (test-niwa-persistence-async--count
                   "Absorbed text"
                   (test-niwa-persistence-async--disk
                    session test-niwa-persistence-async--root-transcript))))))

  :doc "a cancelled before-save hook fails once without stranding the gate and permits retry"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((advance (symbol-function 'niwa-session-publication--advance))
           (submit (symbol-function 'niwa-session-publication-submit))
           (hooks (buffer-local-value 'before-save-hook buffer))
           (hook-calls 0) escaped acknowledgements warned)
       (with-current-buffer buffer
         (setq-local before-save-hook
                     (cons (lambda () (cl-incf hook-calls) (signal 'quit nil)) hooks)))
       (test-niwa-persistence-async--type buffer "Cancelled checkpoint\n")
       (cl-letf (((symbol-function 'niwa-session-publication--advance)
                  (lambda (job)
                    (condition-case nil (funcall advance job)
                      (quit (setq escaped t)))))
                 ((symbol-function 'niwa-session-publication-submit)
                  (lambda (owner prepare acknowledge &optional ids retain select)
                    (funcall submit owner prepare
                             (lambda (ok err)
                               (push (list ok err) acknowledgements)
                               (funcall acknowledge ok err)) ids retain select))))
         (niwa-test--with-captured-diagnostics warned
           (niwa-session-persistence-checkpoint-buffer buffer)
           (test-niwa-publication-async--wait (lambda () (or escaped acknowledgements))))
         (should (= hook-calls 1))
         (should-not escaped)
         (should (equal '((nil (quit))) acknowledgements))
         (should (string-match-p "Quit" warned))
         (should-not (niwa-session-publication-job session))
         (should-not (niwa-session-publication-busy-p session))
         (should-not (niwa-session-publication-active-p session))
         (should-not (niwa-session-publication-preparation-token session))
         (should-not (niwa-session-pending-publication session))
         (should-not (buffer-local-value 'niwa-session--checkpoint-pending buffer))
         (should (buffer-local-value 'niwa-session--save-failed buffer))
         (should (buffer-modified-p buffer))
         (should-not (string-match-p
                      "Cancelled checkpoint"
                      (test-niwa-persistence-async--disk
                       session test-niwa-persistence-async--root-transcript)))
         (with-current-buffer buffer (setq-local before-save-hook hooks))
         (niwa-session-persistence-checkpoint-buffer buffer)
         (test-niwa-persistence-async--idle session)
         (should (equal '((t nil) (nil (quit))) acknowledgements))
         (should-not (buffer-modified-p buffer))
         (should-not (buffer-local-value 'niwa-session--save-failed buffer))
         (should (= 1 (test-niwa-persistence-async--count
                       "Cancelled checkpoint"
                       (test-niwa-persistence-async--disk
                        session test-niwa-persistence-async--root-transcript))))))))

  :doc "a refused checkpoint retains its frozen bytes; an explicit retry commits them once"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((reserve (symbol-function
                     'niwa-session-durability-reserve-publication-lease-async))
           (fail t) warned)
       (test-niwa-persistence-async--type buffer "Retried text\n")
       (cl-letf (((symbol-function
                   'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (funcall callback (and (not fail) ok)
                                        (if fail '(error "Injected refusal") err)))))))
         (niwa-test--with-captured-diagnostics warned
           (niwa-session-persistence-checkpoint-buffer buffer)
           (test-niwa-persistence-async--idle session))
         ;; The refusal keeps the edit unsaved, marks the failure and retains
         ;; the frozen publication for explicit recovery.
         (should (buffer-local-value 'niwa-session--save-failed buffer))
         (should (buffer-modified-p buffer))
         (should (niwa-session-pending-publication session))
         ;; Definitive failure has no running job, but close and exit must
         ;; preserve its recovery surface and mutation authority.
         (should-not (niwa-session-publication-busy-p session))
         (with-current-buffer buffer
           (should-not (niwa-session-persistence-allow-buffer-close-p)))
         (should-not (niwa-session-publication-stopping-p session))
         (should-not (niwa-session-persistence--drain-for-close session buffer))
         (should-not (niwa-session-publication-stopping-p session))
         (should-not (niwa-session-persistence--allow-emacs-exit-p))
         (should (memq session (niwa-session-persistence--stop-publications)))
         (should (niwa-session-pending-publication session))
         (should (niwa-session-publication-stopping-p session))
         ;; A vetoed exit keeps explicit recovery available.
         (setf (niwa-session-publication-stopping-p session) nil)
         (should-not (string-match-p
                      "Retried text"
                      (or (test-niwa-persistence-async--disk
                           session test-niwa-persistence-async--root-transcript)
                          "")))
         ;; A pending publication is never blindly retried by a checkpoint.
         (let ((head (test-niwa-persistence-async--head session)))
           (setq fail nil)
           (niwa-session-persistence-checkpoint-buffer buffer)
           (test-niwa-persistence-async--idle session)
           (should (equal head (test-niwa-persistence-async--head session)))
           (should (niwa-session-pending-publication session))
           (should-not (string-match-p
                        "Retried text"
                        (or (test-niwa-persistence-async--disk
                             session test-niwa-persistence-async--root-transcript)
                            ""))))
         ;; Explicit recovery replays the frozen bytes and installs them.
         (niwa-session-publication-retry session)
         (test-niwa-persistence-async--idle session)
         (should-not (niwa-session-pending-publication session))
         (should (= 1 (test-niwa-persistence-async--count
                       "Retried text"
                       (test-niwa-persistence-async--disk
                        session test-niwa-persistence-async--root-transcript))))
         ;; Another checkpoint may now clear the failure without a duplicate.
         (niwa-session-persistence-checkpoint-buffer buffer)
         (test-niwa-persistence-async--idle session))
       (should-not (buffer-local-value 'niwa-session--save-failed buffer))
       (should-not (buffer-modified-p buffer))
       (should (= 1 (test-niwa-persistence-async--count
                     "Retried text"
                     (test-niwa-persistence-async--disk
                      session test-niwa-persistence-async--root-transcript)))))))

  :doc "closing drains an accepted checkpoint, then stops further admissions"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (test-niwa-persistence-async--hold
      1
      (lambda (held release)
        (test-niwa-persistence-async--type buffer "Close text\n")
        (niwa-session-persistence-checkpoint-buffer buffer)
        (test-niwa-publication-async--wait (lambda () (funcall held)))
        (funcall release)
        (should (niwa-session-persistence--drain-for-close session buffer))))
     (should (string-match-p
              "Close text"
              (test-niwa-persistence-async--disk
               session test-niwa-persistence-async--root-transcript)))
     (should (niwa-session-publication-stopping-p session))
     ;; Admission is stopped: a later edit starts no new job.
     (test-niwa-persistence-async--type buffer "Too late\n")
     (should-not (niwa-session-persistence-checkpoint-buffer buffer))
     (should-not (niwa-session-publication-busy-p session))
     (should-not (string-match-p
                  "Too late"
                  (test-niwa-persistence-async--disk
                   session test-niwa-persistence-async--root-transcript)))))

  :doc "an exit sweep drains the accepted checkpoint and blocks only unresolved sessions"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (test-niwa-persistence-async--hold
      1
      (lambda (held release)
        (test-niwa-persistence-async--type buffer "Exit text\n")
        (niwa-session-persistence-checkpoint-buffer buffer)
        (test-niwa-publication-async--wait (lambda () (funcall held)))
        (funcall release)
        (should-not (niwa-session-persistence--stop-publications))))
     (should (string-match-p
              "Exit text"
              (test-niwa-persistence-async--disk
               session test-niwa-persistence-async--root-transcript)))
     (should-not (niwa-session-publication-busy-p session))))

  :doc "a fresh restore sees the old checkpoint before commit and the new one, receipt retired, after"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let (baseline mid after id)
       ;; A delivered completion whose consumption awaits the transcript.
       (test-niwa-publication-async--send session "receipt" #'ignore)
       (test-niwa-persistence-async--idle session)
       (let ((mail (test-niwa-publication-async--mail session)))
         (setq id (plist-get (car mail) :id))
         (niwa-session-checkpoint-consume session "/root" mail)
         (niwa-agent-control--set-mailbox-queue session "/root" nil))
       (setq baseline (test-niwa-persistence-async--restore session))
       (should (equal (list id) (plist-get baseline :mail)))
       (should-not (string-match-p
                    "Consumed answer" (plist-get baseline :transcript)))
       (setf (niwa-session-turn-count session)
             (1+ (or (plist-get baseline :turns) 0)))
       (test-niwa-persistence-async--type buffer "Consumed answer\n")
       (test-niwa-persistence-async--hold
        1
        (lambda (held release)
          (niwa-session-persistence-checkpoint-buffer buffer)
          (test-niwa-publication-async--wait (lambda () (funcall held)))
          ;; Before the head commits storage still shows the old checkpoint,
          ;; and the live ledger retains the consumed receipt.
          (setq mid (test-niwa-persistence-async--restore session))
          (should (equal (plist-get baseline :turns) (plist-get mid :turns)))
          (should (equal (plist-get baseline :transcript)
                         (plist-get mid :transcript)))
          (should (equal (list id) (plist-get mid :mail)))
          (should (niwa-session-checkpoint-pending-message-p
                   session "/root" id))
          (funcall release)
          (test-niwa-persistence-async--idle session)))
       (setq after (test-niwa-persistence-async--restore session))
       ;; After it, the new transcript and root fields are authoritative and
       ;; the retained receipt is gone from storage and from the ledger.
       (should (= (1+ (or (plist-get baseline :turns) 0)) (plist-get after :turns)))
       (should (= 1 (test-niwa-persistence-async--count
                     "Consumed answer" (plist-get after :transcript))))
       (should-not (plist-get after :mail))
       (should-not (niwa-session-checkpoint-pending-message-p
                    session "/root" id)))))

  :doc "a multiline draft starting with > keeps its text and point through a checkpoint"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((view (generate-new-buffer " *async-composer-view*")))
       (unwind-protect
           (cl-letf (((symbol-function 'niwa-recovery-save) #'ignore))
             (niwa-view--setup view buffer)
             (let (text offset)
               (with-current-buffer view
                 (niwa-view-composer-insert "> draft\nsecond line")
                 (setq text (niwa-view-composer-draft-text)
                       offset (niwa-view-test--composer-point-offset)))
               (test-niwa-persistence-async--type buffer "Composer answer\n")
               (test-niwa-persistence-async--hold
                1
                (lambda (held release)
                  (niwa-session-persistence-checkpoint-buffer buffer)
                  (test-niwa-publication-async--wait (lambda () (funcall held)))
                  (funcall release)
                  (test-niwa-persistence-async--idle session)))
               (should-not (buffer-modified-p buffer))
               (with-current-buffer view
                 (should (equal text (niwa-view-composer-draft-text)))
                 (should (= offset (niwa-view-test--composer-point-offset))))))
         (when (buffer-live-p view)
           (with-current-buffer view (setq-local kill-buffer-hook nil))
           (kill-buffer view))))))

  :doc "repeated checkpoints coalesce into one accepted job until it completes"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((jobs 0)
           (submit (symbol-function 'niwa-session-publication-submit)))
       (cl-letf (((symbol-function 'niwa-session-publication-submit)
                  (lambda (&rest arguments)
                    (cl-incf jobs)
                    (apply submit arguments))))
         (test-niwa-persistence-async--hold
          1
          (lambda (held release)
            (test-niwa-persistence-async--type buffer "Coalesced text\n")
            (should (eq 'pending
                        (niwa-session-persistence-checkpoint-buffer buffer)))
            (test-niwa-publication-async--wait (lambda () (funcall held)))
            (should (buffer-local-value 'niwa-session--checkpoint-pending
                                        buffer))
            ;; Further requests while one is accepted add nothing.
            (should-not (niwa-session-persistence-checkpoint-buffer buffer))
            (test-niwa-persistence-async--type buffer "Also coalesced\n")
            (should-not (niwa-session-persistence-checkpoint-buffer buffer))
            (should (= 1 jobs))
            (funcall release)
            (test-niwa-persistence-async--idle session))))
       (should-not (buffer-local-value 'niwa-session--checkpoint-pending
                                       buffer))
       ;; One job froze before the second edit: that edit is not reported
       ;; saved, and the coalesced requests did not queue a second job.
       (should (= 1 jobs))
       (let ((disk (test-niwa-persistence-async--disk
                    session test-niwa-persistence-async--root-transcript)))
         (should (= 1 (test-niwa-persistence-async--count "Coalesced text" disk)))
         (should-not (string-match-p "Also coalesced" disk)))
       (should (buffer-modified-p buffer))
       ;; Completion frees the marker, so the unsaved edit can be accepted.
       (cl-letf (((symbol-function 'niwa-session-publication-submit)
                  (lambda (&rest arguments) (cl-incf jobs) (apply submit arguments))))
         (should (eq 'pending (niwa-session-persistence-checkpoint-buffer buffer)))
         (test-niwa-persistence-async--idle session))
       (should (= 2 jobs))
       (should-not (buffer-modified-p buffer))
       (should (= 1 (test-niwa-persistence-async--count
                     "Also coalesced"
                     (test-niwa-persistence-async--disk
                      session test-niwa-persistence-async--root-transcript)))))))

  :doc "read-only, inspection and non-owned buffers never start a job"
  (test-niwa-publication-async--fixture
   (lambda (session root)
     (let ((head (test-niwa-persistence-async--head session))
           (submit (symbol-function 'niwa-session-publication-submit))
           (jobs 0)
           (stranger (generate-new-buffer " *async-stranger*")))
       (unwind-protect
           (cl-letf (((symbol-function 'niwa-session-publication-submit)
                      (lambda (&rest arguments) (cl-incf jobs) (apply submit arguments))))
             (with-current-buffer stranger
               (setq-local niwa--session session)
               (insert "Not an owned conversation\n"))
             (test-niwa-persistence-async--type root "Root edit\n")
             (with-current-buffer root
               (setq-local niwa-session--read-only-mode t))
             (should-not (niwa-session-persistence-checkpoint-buffer root))
             (with-current-buffer root
               (setq-local niwa-session--read-only-mode nil
                           niwa-session--inspection-buffer-p t))
             (should-not (niwa-session-persistence-checkpoint-buffer root))
             (with-current-buffer root
               (setq-local niwa-session--inspection-buffer-p nil))
             (should-not (niwa-session-persistence-checkpoint-buffer stranger))
             (should (= 0 jobs))
             (should-not (niwa-session-publication-busy-p session))
             (should (equal head (test-niwa-persistence-async--head session)))
             ;; Once eligible again the same edit is accepted normally.
             (should (eq 'pending (niwa-session-persistence-checkpoint-buffer root)))
             (test-niwa-persistence-async--idle session)
             (should (= 1 jobs)))
         (with-current-buffer stranger
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer stranger)))))

  :doc "cold, file-backed and remote authorities fall back to the synchronous writer"
  (dolist (kind '(cold file remote))
    (test-niwa-publication-async--fixture
     (lambda (session buffer)
       (let ((publication (niwa-session-publication session))
             (save-path (niwa-session-save-path session))
             (submit (symbol-function 'niwa-session-publication-submit))
             (portable (symbol-function 'niwa-session-codec-portable-authority-p))
             (jobs 0) synchronous)
         (unwind-protect
             (progn
               (pcase kind
                 ('cold (setf (niwa-session-publication session) nil))
                 ('file nil)
                 ('remote (setf (niwa-session-save-path session)
                                "/ssh:async-test-host:/tmp/niwa-async-test/")))
               (test-niwa-persistence-async--type buffer "Fallback text\n")
               (cl-letf (((symbol-function 'niwa-session-codec-portable-authority-p)
                          (lambda (value)
                            (and (not (eq kind 'file))
                                 (funcall portable value))))
                         ((symbol-function 'niwa-session-publication-submit)
                          (lambda (&rest arguments)
                            (cl-incf jobs) (apply submit arguments)))
                         ((symbol-function 'niwa-session-persistence-autosave-buffer)
                          (lambda (value)
                            (push value synchronous)
                            'synchronous-result)))
                 (should-not (niwa-session-publication-async-eligible-p session))
                 (should (eq 'synchronous-result
                             (niwa-session-persistence-checkpoint-buffer buffer))))
               (should (equal (list buffer) synchronous))
               (should (= 0 jobs)))
           (setf (niwa-session-publication session) publication
                 (niwa-session-save-path session) save-path))))))

  :doc "an unregistered agent uses the synchronous fallback without clearing save failure"
  (test-niwa-publication-async--fixture
   (lambda (session root)
     (let* ((child (generate-new-buffer " *async-unregistered-agent*"))
            (invocation (niwa-agent-invocation--create
                         :agent (niwa-agent--create :name "worker")
                         :agent-id "worker--unregistered" :path "/root/worker"
                         :buffer child :parent-session session :parent-data-buffer root))
            (autosave (symbol-function 'niwa-session-persistence-autosave-buffer))
            (head (test-niwa-persistence-async--head session))
            (fallbacks 0))
       (unwind-protect
           (progn
             (with-current-buffer child
               (setq-local niwa--session session
                           niwa--agent-invocation invocation
                           niwa-session--save-failed t)
               (insert "Unregistered transcript text\n"))
             (cl-letf (((symbol-function 'niwa-session-persistence-autosave-buffer)
                        (lambda (buffer &optional token)
                          (cl-incf fallbacks)
                          (should-not (niwa-session-publication-busy-p session))
                          (should-not (buffer-local-value
                                       'niwa-session--checkpoint-pending buffer))
                          (funcall autosave buffer token))))
               (should (eq 'pending (niwa-session-persistence-checkpoint-buffer child)))
               (test-niwa-persistence-async--idle session))
             (should (= 1 fallbacks))
             (should (buffer-local-value 'niwa-session--save-failed child))
             (should (buffer-modified-p child))
             (should-not (niwa-agent-invocation-transcript-relative-path invocation))
             (should (equal head (test-niwa-persistence-async--head session))))
         (with-current-buffer child
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer child)))))

  :doc "a retained agent's transcript checkpoints in the background with its sidecar"
  (test-niwa-publication-async--fixture
   (lambda (session root)
     (let* ((child (generate-new-buffer " *async-agent*"))
            (invocation (niwa-agent-invocation--create
                         :agent (niwa-agent--create :name "worker")
                         :agent-id "worker--async" :path "/root/worker"
                         :buffer child :parent-session session
                         :parent-data-buffer root)))
       (unwind-protect
           (progn
             (niwa-agent-runtime--setup-transcript invocation child)
             (with-current-buffer child
               (setq-local niwa--session session
                           niwa--agent-invocation invocation)
               (insert "Agent checkpoint text\n"))
             (niwa-session-persistence-checkpoint-buffer child)
             (test-niwa-persistence-async--idle session)
             (should-not (niwa-session-publication-uncommitted-batches session))
             (should-not (niwa-agent-invocation-sidecar-dirty invocation))
             (should (= 1 (test-niwa-persistence-async--count
                           "Agent checkpoint text"
                           (test-niwa-persistence-async--disk
                            session
                            (niwa-agent-invocation-transcript-relative-path
                             invocation)))))
             ;; Nothing left to commit: a repeat is a no-op.
             (let ((head (test-niwa-persistence-async--head session)))
               (niwa-session-persistence-checkpoint-buffer child)
               (test-niwa-persistence-async--idle session)
               (should (equal head (test-niwa-persistence-async--head session)))))
         (with-current-buffer child
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer child))))))

(provide 'test-niwa-session-persistence-async)

;;; test-niwa-session-persistence-async.el ends here
