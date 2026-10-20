;;; test-mevedel-session-persistence-async.el --- Async checkpoint integration -*- lexical-binding: t -*-

;;; Commentary:

;; Adversarial integration tests for the background checkpoint entry
;; `mevedel-session-persistence-checkpoint-buffer'.  Each scenario runs on a
;; warm local portable session with real storage and holds real publication
;; jobs at lease reservation to interleave competing writers.

;;; Code:

(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))
(require 'mevedel-agent-runtime)
(require 'mevedel-session-persistence)

(defconst test-mevedel-persistence-async--root-transcript
  "segment-0001.chat.org"
  "Logical name of the first root transcript segment.")

(defun test-mevedel-persistence-async--hold (count function)
  "Call FUNCTION while the next COUNT lease reservations are held.
FUNCTION receives a function returning the continuations captured so far,
oldest first, and a function releasing the oldest unreleased one from a
timer.  Reservations beyond COUNT run normally."
  (let ((reserve (symbol-function
                  'mevedel-session-durability-reserve-publication-lease-async))
        (held nil) (released 0) (seen 0))
    (cl-letf (((symbol-function
                'mevedel-session-durability-reserve-publication-lease-async)
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

(defun test-mevedel-persistence-async--idle (session)
  "Wait until SESSION's publication gate is idle."
  (test-mevedel-publication-async--wait
   (lambda () (not (mevedel-session-publication-busy-p session)))))

(defun test-mevedel-persistence-async--disk (session logical)
  "Return SESSION's committed artifact LOGICAL, or nil when absent."
  (ignore-errors
    (mevedel-session-artifacts-read-artifact session logical t)))

(defun test-mevedel-persistence-async--head (session)
  "Return SESSION's current publication head."
  (plist-get (mevedel-session-publication session) :head))

(defun test-mevedel-persistence-async--count (needle text)
  "Return how many times NEEDLE occurs in TEXT."
  (let ((count 0) (start 0) (text (or text "")))
    (while (string-match (regexp-quote needle) text start)
      (cl-incf count)
      (setq start (match-end 0)))
    count))

(defun test-mevedel-persistence-async--restore (session)
  "Return SESSION's authoritative state read fresh from storage.
Neither the session's captured publication nor any cache is consulted: the
publication, its root transcript segment and sidecar are read and decoded
anew.  The result is a plist of :transcript, :turns, :latest and :mail IDs."
  (let* ((mevedel-session-publication--generation-cache (make-hash-table :test #'equal))
         (mevedel-session-publication--facts-cache (make-hash-table :test #'equal))
         (publication (mevedel-session-publication-read
                       (mevedel-session-save-path session)))
         (raw (mevedel-session-persistence-load-sidecar
               (plist-get publication :sidecar)))
         (restored (mevedel-session-codec-deserialize
                    raw (mevedel-session-workspace session))))
    (list :transcript (mevedel-session-artifacts--read-published-artifact
                       publication test-mevedel-persistence-async--root-transcript)
          :turns (plist-get raw :total-turn-count)
          :latest (plist-get raw :latest-user-message)
          :mail (test-mevedel-publication-async--ids
                 (test-mevedel-publication-async--mail
                  (plist-get restored :session))))))

(defun test-mevedel-persistence-async--type (buffer text)
  "Append TEXT as an unsaved model response to BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert (propertize text 'gptel 'response))))

(mevedel-deftest mevedel-session-persistence-checkpoint-buffer (:quiet t)
  ,test
  (test)
  :doc "a modified root commits in the background and restores from disk"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (test-mevedel-persistence-async--type buffer "Checkpointed answer\n")
     (should (buffer-modified-p buffer))
     (mevedel-session-persistence-checkpoint-buffer buffer)
     (test-mevedel-persistence-async--idle session)
     (should-not (buffer-modified-p buffer))
     (should-not (buffer-local-value 'mevedel-session--save-failed buffer))
     (should (= 1 (test-mevedel-persistence-async--count
                   "Checkpointed answer"
                   (test-mevedel-persistence-async--disk
                    session test-mevedel-persistence-async--root-transcript))))))

  :doc "text typed while the job runs stays unsaved until the next checkpoint"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (test-mevedel-persistence-async--hold
      1
      (lambda (held release)
        (test-mevedel-persistence-async--type buffer "First text\n")
        (mevedel-session-persistence-checkpoint-buffer buffer)
        (test-mevedel-publication-async--wait (lambda () (funcall held)))
        (test-mevedel-persistence-async--type buffer "Typed during job\n")
        (funcall release)
        (test-mevedel-persistence-async--idle session)))
     (let ((disk (test-mevedel-persistence-async--disk
                  session test-mevedel-persistence-async--root-transcript)))
       (should (string-match-p "First text" disk))
       (should-not (string-match-p "Typed during job" disk)))
     ;; The edit made after the freeze is not reported saved.
     (should (buffer-modified-p buffer))
     (mevedel-session-persistence-checkpoint-buffer buffer)
     (test-mevedel-persistence-async--idle session)
     (should-not (buffer-modified-p buffer))
     (let ((disk (test-mevedel-persistence-async--disk
                  session test-mevedel-persistence-async--root-transcript)))
       (should (= 1 (test-mevedel-persistence-async--count "Typed during job" disk)))
       (should (= 1 (test-mevedel-persistence-async--count "First text" disk))))))

  :doc "a completion delivery overlapping a checkpoint commits both exactly once, in either order"
  (dolist (checkpoint-first '(nil t))
    (test-mevedel-publication-async--fixture
     (lambda (session buffer)
       (let (acknowledgements)
         (test-mevedel-persistence-async--hold
          1
          (lambda (held release)
            (test-mevedel-persistence-async--type buffer "Overlap text\n")
            (if checkpoint-first
                (progn
                  (mevedel-session-persistence-checkpoint-buffer buffer)
                  (test-mevedel-publication-async--wait (lambda () (funcall held)))
                  (test-mevedel-publication-async--send
                   session "overlap" (lambda (ok err) (push (list ok err) acknowledgements))))
              (test-mevedel-publication-async--send
               session "overlap" (lambda (ok err) (push (list ok err) acknowledgements)))
              (test-mevedel-publication-async--wait (lambda () (funcall held)))
              (mevedel-session-persistence-checkpoint-buffer buffer))
            (funcall release)
            (test-mevedel-persistence-async--idle session)))
         (should (equal '((t nil)) acknowledgements))
         (should-not (buffer-modified-p buffer))
         (should (equal (list (secure-hash 'sha256 "/root:overlap"))
                        (test-mevedel-publication-async--ids
                         (test-mevedel-publication-async--restored-mail session))))
         (should (= 1 (test-mevedel-persistence-async--count
                       "Overlap text"
                       (test-mevedel-persistence-async--disk
                        session test-mevedel-persistence-async--root-transcript))))
         ;; The consumed mail's receipt retires with the transcript commit.
         (let ((mail (test-mevedel-publication-async--mail session)))
           (mevedel-session-checkpoint-consume session "/root" mail)
           (mevedel-agent-control--set-mailbox-queue session "/root" nil)
           (test-mevedel-persistence-async--type buffer "After consume\n")
           (mevedel-session-persistence-checkpoint-buffer buffer)
           (test-mevedel-persistence-async--idle session)
           (should-not (mevedel-session-checkpoint-pending-message-p
                        session "/root" (plist-get (car mail) :id)))
           (should-not (test-mevedel-publication-async--restored-mail session)))))))

  :doc "a checkpoint queued behind a synchronous save finds nothing to do"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (test-mevedel-persistence-async--hold
      1
      (lambda (held release)
        (test-mevedel-publication-async--send session "gate" #'ignore)
        (test-mevedel-publication-async--wait (lambda () (funcall held)))
        (test-mevedel-persistence-async--type buffer "Absorbed text\n")
        (mevedel-session-persistence-checkpoint-buffer buffer)
        (funcall release)
        ;; The synchronous save drains the job and commits the edit first.
        (should (mevedel-session-artifacts-save session buffer nil t))
        (let ((head (test-mevedel-persistence-async--head session)))
          (test-mevedel-persistence-async--idle session)
          (should (equal head (test-mevedel-persistence-async--head session))))))
     (should-not (buffer-modified-p buffer))
     (should (= 1 (test-mevedel-persistence-async--count
                   "Absorbed text"
                   (test-mevedel-persistence-async--disk
                    session test-mevedel-persistence-async--root-transcript))))))

  :doc "a cancelled before-save hook fails once without stranding the gate and permits retry"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((advance (symbol-function 'mevedel-session-publication--advance))
           (submit (symbol-function 'mevedel-session-publication-submit))
           (hooks (buffer-local-value 'before-save-hook buffer))
           (hook-calls 0) escaped acknowledgements warned)
       (with-current-buffer buffer
         (setq-local before-save-hook
                     (cons (lambda () (cl-incf hook-calls) (signal 'quit nil)) hooks)))
       (test-mevedel-persistence-async--type buffer "Cancelled checkpoint\n")
       (cl-letf (((symbol-function 'mevedel-session-publication--advance)
                  (lambda (job)
                    (condition-case nil (funcall advance job)
                      (quit (setq escaped t)))))
                 ((symbol-function 'mevedel-session-publication-submit)
                  (lambda (owner prepare acknowledge &optional ids retain select)
                    (funcall submit owner prepare
                             (lambda (ok err)
                               (push (list ok err) acknowledgements)
                               (funcall acknowledge ok err)) ids retain select))))
         (mevedel-test--with-captured-diagnostics warned
           (mevedel-session-persistence-checkpoint-buffer buffer)
           (test-mevedel-publication-async--wait (lambda () (or escaped acknowledgements))))
         (should (= hook-calls 1))
         (should-not escaped)
         (should (equal '((nil (quit))) acknowledgements))
         (should (string-match-p "Quit" warned))
         (should-not (mevedel-session-publication-job session))
         (should-not (mevedel-session-publication-busy-p session))
         (should-not (mevedel-session-publication-active-p session))
         (should-not (mevedel-session-publication-preparation-token session))
         (should-not (mevedel-session-pending-publication session))
         (should-not (buffer-local-value 'mevedel-session--checkpoint-pending buffer))
         (should (buffer-local-value 'mevedel-session--save-failed buffer))
         (should (buffer-modified-p buffer))
         (should-not (string-match-p
                      "Cancelled checkpoint"
                      (test-mevedel-persistence-async--disk
                       session test-mevedel-persistence-async--root-transcript)))
         (with-current-buffer buffer (setq-local before-save-hook hooks))
         (mevedel-session-persistence-checkpoint-buffer buffer)
         (test-mevedel-persistence-async--idle session)
         (should (equal '((t nil) (nil (quit))) acknowledgements))
         (should-not (buffer-modified-p buffer))
         (should-not (buffer-local-value 'mevedel-session--save-failed buffer))
         (should (= 1 (test-mevedel-persistence-async--count
                       "Cancelled checkpoint"
                       (test-mevedel-persistence-async--disk
                        session test-mevedel-persistence-async--root-transcript))))))))

  :doc "a refused checkpoint retains its frozen bytes; an explicit retry commits them once"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((reserve (symbol-function
                     'mevedel-session-durability-reserve-publication-lease-async))
           (fail t) warned)
       (test-mevedel-persistence-async--type buffer "Retried text\n")
       (cl-letf (((symbol-function
                   'mevedel-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (funcall callback (and (not fail) ok)
                                        (if fail '(error "Injected refusal") err)))))))
         (mevedel-test--with-captured-diagnostics warned
           (mevedel-session-persistence-checkpoint-buffer buffer)
           (test-mevedel-persistence-async--idle session))
         ;; The refusal keeps the edit unsaved, marks the failure and retains
         ;; the frozen publication for explicit recovery.
         (should (buffer-local-value 'mevedel-session--save-failed buffer))
         (should (buffer-modified-p buffer))
         (should (mevedel-session-pending-publication session))
         ;; Definitive failure has no running job, but close and exit must
         ;; preserve its recovery surface and mutation authority.
         (should-not (mevedel-session-publication-busy-p session))
         (with-current-buffer buffer
           (should-not (mevedel-session-persistence-allow-buffer-close-p)))
         (should-not (mevedel-session-publication-stopping-p session))
         (should-not (mevedel-session-persistence--drain-for-close session buffer))
         (should-not (mevedel-session-publication-stopping-p session))
         (should-not (mevedel-session-persistence--allow-emacs-exit-p))
         (should (memq session (mevedel-session-persistence--stop-publications)))
         (should (mevedel-session-pending-publication session))
         (should (mevedel-session-publication-stopping-p session))
         ;; A vetoed exit keeps explicit recovery available.
         (setf (mevedel-session-publication-stopping-p session) nil)
         (should-not (string-match-p
                      "Retried text"
                      (or (test-mevedel-persistence-async--disk
                           session test-mevedel-persistence-async--root-transcript)
                          "")))
         ;; A pending publication is never blindly retried by a checkpoint.
         (let ((head (test-mevedel-persistence-async--head session)))
           (setq fail nil)
           (mevedel-session-persistence-checkpoint-buffer buffer)
           (test-mevedel-persistence-async--idle session)
           (should (equal head (test-mevedel-persistence-async--head session)))
           (should (mevedel-session-pending-publication session))
           (should-not (string-match-p
                        "Retried text"
                        (or (test-mevedel-persistence-async--disk
                             session test-mevedel-persistence-async--root-transcript)
                            ""))))
         ;; Explicit recovery replays the frozen bytes and installs them.
         (mevedel-session-publication-retry session)
         (test-mevedel-persistence-async--idle session)
         (should-not (mevedel-session-pending-publication session))
         (should (= 1 (test-mevedel-persistence-async--count
                       "Retried text"
                       (test-mevedel-persistence-async--disk
                        session test-mevedel-persistence-async--root-transcript))))
         ;; Another checkpoint may now clear the failure without a duplicate.
         (mevedel-session-persistence-checkpoint-buffer buffer)
         (test-mevedel-persistence-async--idle session))
       (should-not (buffer-local-value 'mevedel-session--save-failed buffer))
       (should-not (buffer-modified-p buffer))
       (should (= 1 (test-mevedel-persistence-async--count
                     "Retried text"
                     (test-mevedel-persistence-async--disk
                      session test-mevedel-persistence-async--root-transcript)))))))

  :doc "closing drains an accepted checkpoint, then stops further admissions"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (test-mevedel-persistence-async--hold
      1
      (lambda (held release)
        (test-mevedel-persistence-async--type buffer "Close text\n")
        (mevedel-session-persistence-checkpoint-buffer buffer)
        (test-mevedel-publication-async--wait (lambda () (funcall held)))
        (funcall release)
        (should (mevedel-session-persistence--drain-for-close session buffer))))
     (should (string-match-p
              "Close text"
              (test-mevedel-persistence-async--disk
               session test-mevedel-persistence-async--root-transcript)))
     (should (mevedel-session-publication-stopping-p session))
     ;; Admission is stopped: a later edit starts no new job.
     (test-mevedel-persistence-async--type buffer "Too late\n")
     (should-not (mevedel-session-persistence-checkpoint-buffer buffer))
     (should-not (mevedel-session-publication-busy-p session))
     (should-not (string-match-p
                  "Too late"
                  (test-mevedel-persistence-async--disk
                   session test-mevedel-persistence-async--root-transcript)))))

  :doc "an exit sweep drains the accepted checkpoint and blocks only unresolved sessions"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (test-mevedel-persistence-async--hold
      1
      (lambda (held release)
        (test-mevedel-persistence-async--type buffer "Exit text\n")
        (mevedel-session-persistence-checkpoint-buffer buffer)
        (test-mevedel-publication-async--wait (lambda () (funcall held)))
        (funcall release)
        (should-not (mevedel-session-persistence--stop-publications))))
     (should (string-match-p
              "Exit text"
              (test-mevedel-persistence-async--disk
               session test-mevedel-persistence-async--root-transcript)))
     (should-not (mevedel-session-publication-busy-p session))))

  :doc "a fresh restore sees the old checkpoint before commit and the new one, receipt retired, after"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let (baseline mid after id)
       ;; A delivered completion whose consumption awaits the transcript.
       (test-mevedel-publication-async--send session "receipt" #'ignore)
       (test-mevedel-persistence-async--idle session)
       (let ((mail (test-mevedel-publication-async--mail session)))
         (setq id (plist-get (car mail) :id))
         (mevedel-session-checkpoint-consume session "/root" mail)
         (mevedel-agent-control--set-mailbox-queue session "/root" nil))
       (setq baseline (test-mevedel-persistence-async--restore session))
       (should (equal (list id) (plist-get baseline :mail)))
       (should-not (string-match-p
                    "Consumed answer" (plist-get baseline :transcript)))
       (setf (mevedel-session-turn-count session)
             (1+ (or (plist-get baseline :turns) 0)))
       (test-mevedel-persistence-async--type buffer "Consumed answer\n")
       (test-mevedel-persistence-async--hold
        1
        (lambda (held release)
          (mevedel-session-persistence-checkpoint-buffer buffer)
          (test-mevedel-publication-async--wait (lambda () (funcall held)))
          ;; Before the head commits storage still shows the old checkpoint,
          ;; and the live ledger retains the consumed receipt.
          (setq mid (test-mevedel-persistence-async--restore session))
          (should (equal (plist-get baseline :turns) (plist-get mid :turns)))
          (should (equal (plist-get baseline :transcript)
                         (plist-get mid :transcript)))
          (should (equal (list id) (plist-get mid :mail)))
          (should (mevedel-session-checkpoint-pending-message-p
                   session "/root" id))
          (funcall release)
          (test-mevedel-persistence-async--idle session)))
       (setq after (test-mevedel-persistence-async--restore session))
       ;; After it, the new transcript and root fields are authoritative and
       ;; the retained receipt is gone from storage and from the ledger.
       (should (= (1+ (or (plist-get baseline :turns) 0)) (plist-get after :turns)))
       (should (= 1 (test-mevedel-persistence-async--count
                     "Consumed answer" (plist-get after :transcript))))
       (should-not (plist-get after :mail))
       (should-not (mevedel-session-checkpoint-pending-message-p
                    session "/root" id)))))

  :doc "a multiline draft starting with > keeps its text and point through a checkpoint"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((view (generate-new-buffer " *async-composer-view*")))
       (unwind-protect
           (cl-letf (((symbol-function 'mevedel-recovery-save) #'ignore))
             (mevedel-view--setup view buffer)
             (let (text distance)
               (with-current-buffer view
                 (goto-char (point-max))
                 (insert "> draft\nsecond line")
                 (setq text (mevedel-view--input-text)
                       distance (- (point-max) (point))))
               (test-mevedel-persistence-async--type buffer "Composer answer\n")
               (test-mevedel-persistence-async--hold
                1
                (lambda (held release)
                  (mevedel-session-persistence-checkpoint-buffer buffer)
                  (test-mevedel-publication-async--wait (lambda () (funcall held)))
                  (funcall release)
                  (test-mevedel-persistence-async--idle session)))
               (should-not (buffer-modified-p buffer))
               (with-current-buffer view
                 (should (equal text (mevedel-view--input-text)))
                 (should (= distance (- (point-max) (point)))))))
         (when (buffer-live-p view)
           (with-current-buffer view (setq-local kill-buffer-hook nil))
           (kill-buffer view))))))

  :doc "repeated checkpoints coalesce into one accepted job until it completes"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((jobs 0)
           (submit (symbol-function 'mevedel-session-publication-submit)))
       (cl-letf (((symbol-function 'mevedel-session-publication-submit)
                  (lambda (&rest arguments)
                    (cl-incf jobs)
                    (apply submit arguments))))
         (test-mevedel-persistence-async--hold
          1
          (lambda (held release)
            (test-mevedel-persistence-async--type buffer "Coalesced text\n")
            (should (eq 'pending
                        (mevedel-session-persistence-checkpoint-buffer buffer)))
            (test-mevedel-publication-async--wait (lambda () (funcall held)))
            (should (buffer-local-value 'mevedel-session--checkpoint-pending
                                        buffer))
            ;; Further requests while one is accepted add nothing.
            (should-not (mevedel-session-persistence-checkpoint-buffer buffer))
            (test-mevedel-persistence-async--type buffer "Also coalesced\n")
            (should-not (mevedel-session-persistence-checkpoint-buffer buffer))
            (should (= 1 jobs))
            (funcall release)
            (test-mevedel-persistence-async--idle session))))
       (should-not (buffer-local-value 'mevedel-session--checkpoint-pending
                                       buffer))
       ;; One job froze before the second edit: that edit is not reported
       ;; saved, and the coalesced requests did not queue a second job.
       (should (= 1 jobs))
       (let ((disk (test-mevedel-persistence-async--disk
                    session test-mevedel-persistence-async--root-transcript)))
         (should (= 1 (test-mevedel-persistence-async--count "Coalesced text" disk)))
         (should-not (string-match-p "Also coalesced" disk)))
       (should (buffer-modified-p buffer))
       ;; Completion frees the marker, so the unsaved edit can be accepted.
       (cl-letf (((symbol-function 'mevedel-session-publication-submit)
                  (lambda (&rest arguments) (cl-incf jobs) (apply submit arguments))))
         (should (eq 'pending (mevedel-session-persistence-checkpoint-buffer buffer)))
         (test-mevedel-persistence-async--idle session))
       (should (= 2 jobs))
       (should-not (buffer-modified-p buffer))
       (should (= 1 (test-mevedel-persistence-async--count
                     "Also coalesced"
                     (test-mevedel-persistence-async--disk
                      session test-mevedel-persistence-async--root-transcript)))))))

  :doc "read-only, inspection and non-owned buffers never start a job"
  (test-mevedel-publication-async--fixture
   (lambda (session root)
     (let ((head (test-mevedel-persistence-async--head session))
           (submit (symbol-function 'mevedel-session-publication-submit))
           (jobs 0)
           (stranger (generate-new-buffer " *async-stranger*")))
       (unwind-protect
           (cl-letf (((symbol-function 'mevedel-session-publication-submit)
                      (lambda (&rest arguments) (cl-incf jobs) (apply submit arguments))))
             (with-current-buffer stranger
               (setq-local mevedel--session session)
               (insert "Not an owned conversation\n"))
             (test-mevedel-persistence-async--type root "Root edit\n")
             (with-current-buffer root
               (setq-local mevedel-session--read-only-mode t))
             (should-not (mevedel-session-persistence-checkpoint-buffer root))
             (with-current-buffer root
               (setq-local mevedel-session--read-only-mode nil
                           mevedel-session--inspection-buffer-p t))
             (should-not (mevedel-session-persistence-checkpoint-buffer root))
             (with-current-buffer root
               (setq-local mevedel-session--inspection-buffer-p nil))
             (should-not (mevedel-session-persistence-checkpoint-buffer stranger))
             (should (= 0 jobs))
             (should-not (mevedel-session-publication-busy-p session))
             (should (equal head (test-mevedel-persistence-async--head session)))
             ;; Once eligible again the same edit is accepted normally.
             (should (eq 'pending (mevedel-session-persistence-checkpoint-buffer root)))
             (test-mevedel-persistence-async--idle session)
             (should (= 1 jobs)))
         (with-current-buffer stranger
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer stranger)))))

  :doc "cold, file-backed and remote authorities fall back to the synchronous writer"
  (dolist (kind '(cold file remote))
    (test-mevedel-publication-async--fixture
     (lambda (session buffer)
       (let ((publication (mevedel-session-publication session))
             (save-path (mevedel-session-save-path session))
             (submit (symbol-function 'mevedel-session-publication-submit))
             (portable (symbol-function 'mevedel-session-codec-portable-authority-p))
             (jobs 0) synchronous)
         (unwind-protect
             (progn
               (pcase kind
                 ('cold (setf (mevedel-session-publication session) nil))
                 ('file nil)
                 ('remote (setf (mevedel-session-save-path session)
                                "/ssh:async-test-host:/tmp/mevedel-async-test/")))
               (test-mevedel-persistence-async--type buffer "Fallback text\n")
               (cl-letf (((symbol-function 'mevedel-session-codec-portable-authority-p)
                          (lambda (value)
                            (and (not (eq kind 'file))
                                 (funcall portable value))))
                         ((symbol-function 'mevedel-session-publication-submit)
                          (lambda (&rest arguments)
                            (cl-incf jobs) (apply submit arguments)))
                         ((symbol-function 'mevedel-session-persistence-autosave-buffer)
                          (lambda (value)
                            (push value synchronous)
                            'synchronous-result)))
                 (should-not (mevedel-session-publication-async-eligible-p session))
                 (should (eq 'synchronous-result
                             (mevedel-session-persistence-checkpoint-buffer buffer))))
               (should (equal (list buffer) synchronous))
               (should (= 0 jobs)))
           (setf (mevedel-session-publication session) publication
                 (mevedel-session-save-path session) save-path))))))

  :doc "an unregistered agent uses the synchronous fallback without clearing save failure"
  (test-mevedel-publication-async--fixture
   (lambda (session root)
     (let* ((child (generate-new-buffer " *async-unregistered-agent*"))
            (invocation (mevedel-agent-invocation--create
                         :agent (mevedel-agent--create :name "worker")
                         :agent-id "worker--unregistered" :path "/root/worker"
                         :buffer child :parent-session session :parent-data-buffer root))
            (autosave (symbol-function 'mevedel-session-persistence-autosave-buffer))
            (head (test-mevedel-persistence-async--head session))
            (fallbacks 0))
       (unwind-protect
           (progn
             (with-current-buffer child
               (setq-local mevedel--session session
                           mevedel--agent-invocation invocation
                           mevedel-session--save-failed t)
               (insert "Unregistered transcript text\n"))
             (cl-letf (((symbol-function 'mevedel-session-persistence-autosave-buffer)
                        (lambda (buffer &optional token)
                          (cl-incf fallbacks)
                          (should-not (mevedel-session-publication-busy-p session))
                          (should-not (buffer-local-value
                                       'mevedel-session--checkpoint-pending buffer))
                          (funcall autosave buffer token))))
               (should (eq 'pending (mevedel-session-persistence-checkpoint-buffer child)))
               (test-mevedel-persistence-async--idle session))
             (should (= 1 fallbacks))
             (should (buffer-local-value 'mevedel-session--save-failed child))
             (should (buffer-modified-p child))
             (should-not (mevedel-agent-invocation-transcript-relative-path invocation))
             (should (equal head (test-mevedel-persistence-async--head session))))
         (with-current-buffer child
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer child)))))

  :doc "a retained agent's transcript checkpoints in the background with its sidecar"
  (test-mevedel-publication-async--fixture
   (lambda (session root)
     (let* ((child (generate-new-buffer " *async-agent*"))
            (invocation (mevedel-agent-invocation--create
                         :agent (mevedel-agent--create :name "worker")
                         :agent-id "worker--async" :path "/root/worker"
                         :buffer child :parent-session session
                         :parent-data-buffer root)))
       (unwind-protect
           (progn
             (mevedel-agent-runtime--setup-transcript invocation child)
             (with-current-buffer child
               (setq-local mevedel--session session
                           mevedel--agent-invocation invocation)
               (insert "Agent checkpoint text\n"))
             (mevedel-session-persistence-checkpoint-buffer child)
             (test-mevedel-persistence-async--idle session)
             (should-not (mevedel-session-publication-uncommitted-batches session))
             (should-not (mevedel-agent-invocation-sidecar-dirty invocation))
             (should (= 1 (test-mevedel-persistence-async--count
                           "Agent checkpoint text"
                           (test-mevedel-persistence-async--disk
                            session
                            (mevedel-agent-invocation-transcript-relative-path
                             invocation)))))
             ;; Nothing left to commit: a repeat is a no-op.
             (let ((head (test-mevedel-persistence-async--head session)))
               (mevedel-session-persistence-checkpoint-buffer child)
               (test-mevedel-persistence-async--idle session)
               (should (equal head (test-mevedel-persistence-async--head session)))))
         (with-current-buffer child
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer child))))))

(provide 'test-mevedel-session-persistence-async)

;;; test-mevedel-session-persistence-async.el ends here
