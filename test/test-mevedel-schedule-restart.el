;;; test-mevedel-schedule-restart.el --- Wake-ups across Emacs restarts -*- lexical-binding: t -*-

;;; Commentary:
;; The startup-restore seam: persist a session with a pending wake-up, drop
;; every in-memory trace of it as a restart would, call the startup entry
;; point and advance the injectable clock.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-chat)
(require 'mevedel-schedule)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-codec)
(require 'mevedel-session-durability)
(require 'mevedel-session-fork)
(require 'mevedel-session-persistence)
(require 'mevedel-session-save-as)
(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))

(defconst mevedel-schedule-restart-test--now 1795176000
  "2026-11-20 13:00:00 in Europe/Zurich (12:00 UTC).")

(defmacro mevedel-schedule-restart-test--with-saved (&rest body)
  "Run BODY with a saved session `session' holding wake-ups made by BODY.
`buffer' is its root data buffer, `now' the clock, `submitted' the started
turns.  `restart' drops the session's in-memory state as an Emacs restart
would; `live' returns the session's live root buffer, or nil."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-schedule-restart-" t))
          (mevedel-user-dir (file-name-as-directory (file-name-concat root "user")))
          (workspace (test-mevedel-session-persistence--make-workspace
                      (file-name-concat root "workspace")))
          (session (mevedel-session-create "main" workspace))
          (buffer (generate-new-buffer " *schedule-restart*"))
          (now mevedel-schedule-restart-test--now)
          (mevedel-schedule-clock (lambda () now))
          submitted)
     (unwind-protect
         (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                    ;; A real turn would send a provider request and consume the mail.
                    (lambda (prompt &rest _) (push prompt submitted))))
           (with-current-buffer buffer
             (org-mode)
             (insert "Check CI later\n")
             (mevedel-session-artifacts-save session buffer)
             (setq-local mevedel--session session))
           (setf (mevedel-session-root-buffer session) buffer)
           (cl-flet ((restart ()
                       (test-mevedel-session-persistence--release-and-kill buffer session)
                       (setq mevedel-schedule--closed nil)
                       (mevedel-schedule--rearm))
                     (live ()
                       (mevedel-schedule-restart-test--live session)))
             ,@body))
       (while-let ((live (mevedel-schedule-restart-test--live session)))
         (test-mevedel-session-persistence--release-and-kill
          live (buffer-local-value 'mevedel--session live)))
       (setq mevedel-schedule--closed nil)
       (mevedel-schedule--rearm)
       (should-not mevedel-schedule--timer)
       (delete-directory root t)
       (mevedel-workspace-clear-registry))))

(defun mevedel-schedule-restart-test--live (session)
  "Return the live root data buffer of SESSION's id, or nil."
  (seq-find (lambda (buffer)
              (when-let* ((other (buffer-local-value 'mevedel--session buffer)))
                (and (eq buffer (mevedel-session-root-buffer other))
                     (equal (mevedel-session-session-id session)
                            (mevedel-session-session-id other)))))
            (buffer-list)))

(defun mevedel-schedule-restart-test--stored (session)
  "Return SESSION's committed sidecar."
  (mevedel-session-codec-read
   (file-name-concat (mevedel-session-save-path session) "session.meta.el")))

(defun mevedel-schedule-restart-test--mail (buffer)
  "Return the types of BUFFER's session's unread mail once it is committed."
  (let ((session (buffer-local-value 'mevedel--session buffer))
        mail)
    (mevedel-test--await 5 "The wake-up was not committed"
      (setq mail (mevedel-agent-control-context-mailbox session)))
    (mapcar (lambda (record) (plist-get record :type)) mail)))

(defun mevedel-schedule-restart-test--index-files ()
  "Return the wake-up index files under `mevedel-user-dir'."
  (let ((index (file-name-concat mevedel-user-dir "wake-ups")))
    (and (file-directory-p index) (directory-files index t "\\.el\\'"))))

(defun mevedel-schedule-restart-test--snapshot-index ()
  "Return the index files and their contents, to put back later."
  (mapcar (lambda (file)
            (cons file (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (mevedel-schedule-restart-test--index-files)))

(defun mevedel-schedule-restart-test--put-back (snapshot)
  "Write the index files of SNAPSHOT back, as if the Emacs crashed first."
  (dolist (entry snapshot)
    (make-directory (file-name-directory (car entry)) t)
    (with-temp-file (car entry) (insert (cdr entry)))))

(defun mevedel-schedule-restart-test--stored-mail (session)
  "Return the types of SESSION's committed mailbox records."
  (mapcar (lambda (record) (plist-get record :type))
          (plist-get (mevedel-schedule-restart-test--stored session) :messages)))

(mevedel-deftest mevedel-schedule-hand-over ()
  ,test
  (test)
  :doc "the store and index lose the wake-ups while the session keeps them in memory"
  (mevedel-schedule-restart-test--with-saved
    (should-not (mevedel-schedule-hand-over session))
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (let ((entries (mevedel-session-schedules session)))
      (should (mevedel-schedule-hand-over session))
      (should (equal entries (mevedel-session-schedules session)))
      (should-not (plist-get (mevedel-schedule-restart-test--stored session) :schedules))
      (should-not (mevedel-schedule-restart-test--index-files))))

  :doc "a failed commit signals and leaves the store, index and memory as they were"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (let ((entries (mevedel-session-schedules session))
          (index (mevedel-schedule-restart-test--snapshot-index)))
      (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                 (lambda (&rest _) (error "Injected storage failure"))))
        (should-error (mevedel-schedule-hand-over session)))
      (should (equal entries (mevedel-session-schedules session)))
      (should (= 1 (length (plist-get (mevedel-schedule-restart-test--stored session) :schedules))))
      (should (equal index (mevedel-schedule-restart-test--snapshot-index))))))

(mevedel-deftest mevedel-schedule-hand-back ()
  ,test
  (test)
  :doc "an abandoned hand-over commits and indexes the session's wake-ups again"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (let ((index (mevedel-schedule-restart-test--snapshot-index)))
      (mevedel-schedule-hand-over session)
      (mevedel-schedule-hand-back session)
      (should (= 1 (length (plist-get (mevedel-schedule-restart-test--stored session) :schedules))))
      (should (equal index (mevedel-schedule-restart-test--snapshot-index))))))

(mevedel-deftest mevedel-schedule-restore ()
  ,test
  (test)
  :doc "a wake-up due for a closed session reopens it headlessly and is delivered once"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (restart)
    (should-not (live))
    (mevedel-schedule-restore)
    (should mevedel-schedule--timer)
    (should-not (live))
    (setq now (+ now 1200))
    (let ((windows (window-list nil t))
          (frames (frame-list))
          notice)
      (mevedel-test--with-captured-messages notice
        (let ((inhibit-interaction t))
          (mevedel-schedule-run-due)))
      (should (string-search "mevedel: reopened session main for a wake-up" notice))
      (should (equal windows (window-list nil t)))
      (should (equal frames (frame-list)))
      (should-not (get-buffer-window (live) t)))
    (should (equal '(WAKE_UP) (mevedel-schedule-restart-test--mail (live))))
    (should-not (plist-get (mevedel-schedule-restart-test--stored session) :schedules))
    (should-not (mevedel-schedule-restart-test--index-files))
    (mevedel-test--await 5 "The reopened root was not woken" submitted)
    (should (= 1 (length submitted))))

  :doc "the index names a session while it holds a pending wake-up"
  (mevedel-schedule-restart-test--with-saved
    (let ((entry (mevedel-schedule-create session buffer "20m" "Check CI")))
      (should (= 1 (length (mevedel-schedule-restart-test--index-files))))
      (mevedel-schedule-cancel session buffer (plist-get entry :id))
      (should-not (mevedel-schedule-restart-test--index-files))))

  :doc "a session closed before its wake-up is due is reopened for it without a restart"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (test-mevedel-session-persistence--release-and-kill buffer session)
    (should mevedel-schedule--timer)
    (setq now (+ now 1200))
    (mevedel-test--with-captured-messages nil
      (mevedel-schedule-run-due))
    (should (equal '(WAKE_UP) (mevedel-schedule-restart-test--mail (live)))))

  :doc "entries of deleted sessions and malformed entries are discarded at startup"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (restart)
    (delete-directory (mevedel-session-save-path session) t)
    (with-temp-file (file-name-concat mevedel-user-dir "wake-ups" "junk.el")
      (insert "(not a directory"))
    (mevedel-schedule-restore)
    (should-not (mevedel-schedule-restart-test--index-files))
    (should-not mevedel-schedule--timer))

  :doc "a firing recorded before a crash is not repeated after the restart"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (let ((index (mevedel-schedule-restart-test--snapshot-index)))
      (setq now (+ now 1200))
      (mevedel-schedule-run-due)
      (should (equal '(WAKE_UP) (mevedel-schedule-restart-test--mail buffer)))
      ;; The crash comes after the store commit but before the index update.
      (restart)
      (mevedel-schedule-restart-test--put-back index))
    (setq now (+ now 600))
    (mevedel-schedule-restore)
    (should-not (mevedel-schedule-restart-test--index-files))
    (should-not mevedel-schedule--timer)
    (should-not (live))
    (should (equal '(WAKE_UP) (mevedel-schedule-restart-test--stored-mail session))))

  :doc "a session whose lease another Emacs holds is left to that Emacs"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (restart)
    (mevedel-schedule-restore)
    (let ((holder (make-process :name "schedule-lease-holder" :command '("sleep" "60")
                                :noquery t :sentinel #'ignore)))
      (unwind-protect
          (let ((identity (or (mevedel--process-identity (process-id holder))
                              (ert-skip "Process identity is unavailable on this host"))))
            (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
              (cl-letf (((symbol-function 'mevedel-session-durability--holder)
                         (lambda ()
                           (list :host (mevedel-session-durability--host) :uid (user-uid)
                                 :pid (process-id holder) :process identity))))
                (should (mevedel-session-durability-lease-acquire
                         (mevedel-session-save-path session) "*other client*"))))
            (setq now (+ now 1200))
            (mevedel-schedule-run-due)
            (should-not (live))
            (should-not mevedel-schedule--timer)
            (should-not (mevedel-schedule-restart-test--stored-mail session))
            (should (= 1 (length (plist-get (mevedel-schedule-restart-test--stored session)
                                            :schedules))))
            ;; The holder may close the session without firing; the next start
            ;; must still find it.
            (should (= 1 (length (mevedel-schedule-restart-test--index-files)))))
        (delete-process holder))))

  :doc "a wake-up that fell due while Emacs was down fires once with its overdue annotation"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (restart)
    (setq now (+ now 1200 7200))
    (mevedel-schedule-restore)
    (mevedel-test--with-captured-messages nil
      (mevedel-schedule-run-due))
    (should (equal '(WAKE_UP) (mevedel-schedule-restart-test--mail (live))))
    (should (string-search
             "overdue_by=\"2h 00m\""
             (mevedel-tools--message-delivery-block
              (car (mevedel-agent-control-context-mailbox
                    (buffer-local-value 'mevedel--session (live))))))))

  :doc "a wake-up that expired while Emacs was down is recorded as expired without a turn"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI" "30m")
    (restart)
    (setq now (+ now 3600))
    (mevedel-schedule-restore)
    (mevedel-test--with-captured-messages nil
      (mevedel-schedule-run-due))
    (let ((stored (plist-get (mevedel-schedule-restart-test--stored session) :schedules)))
      (should (equal '(expired) (mapcar (lambda (entry) (plist-get entry :status)) stored))))
    (should-not (mevedel-schedule-restart-test--stored-mail session))
    (should-not (mevedel-schedule-restart-test--index-files))
    (should-not submitted))

  :doc "a recurring wake-up fires once after a restart and stays scheduled for its next run"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "every 1h" "Triage new issues")
    (restart)
    (setq now (+ now (* 3 3600) 600))
    (mevedel-schedule-restore)
    (mevedel-test--with-captured-messages nil
      (mevedel-schedule-run-due))
    (should (equal '(WAKE_UP) (mevedel-schedule-restart-test--mail (live))))
    (let ((entries (plist-get (mevedel-schedule-restart-test--stored session) :schedules)))
      (should (equal '(pending) (mapcar (lambda (entry) (plist-get entry :status)) entries)))
      (should (= (+ now 3600) (plist-get (car entries) :due))))
    (should (= 1 (length (mevedel-schedule-restart-test--index-files))))
    (should mevedel-schedule--timer))

  :doc "after Save As only the copy holds the wake-up, so it fires once after a restart"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (let* ((parent (mevedel-session-save-path session))
           (copy (file-name-concat (file-name-directory (directory-file-name parent))
                                   "copy")))
      (mevedel-session-save-as-run session buffer "copy" "copy" copy)
      (should (equal (file-name-as-directory copy)
                     (file-name-as-directory (mevedel-session-save-path session))))
      (should-not (plist-get (mevedel-session-codec-read
                              (file-name-concat parent "session.meta.el"))
                             :schedules))
      (should (= 1 (length (plist-get (mevedel-schedule-restart-test--stored session)
                                      :schedules))))
      (should (= 1 (length (mevedel-schedule-restart-test--index-files))))
      (restart)
      (mevedel-schedule-restore)
      (should (equal (list (mevedel-schedule--directory-key copy))
                     (mapcar #'car mevedel-schedule--closed)))
      (setq now (+ now 1200))
      (mevedel-test--with-captured-messages nil
        (mevedel-schedule-run-due))
      (should (equal '(WAKE_UP) (mevedel-schedule-restart-test--mail (live))))))

  :doc "Save As stops before creating the copy when the original's wake-ups cannot be cleared"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (let* ((parent (mevedel-session-save-path session))
           (copy (file-name-concat (file-name-directory (directory-file-name parent))
                                   "copy"))
           (index (mevedel-schedule-restart-test--snapshot-index))
           (commit (symbol-function 'mevedel-agent-control-commit-session))
           (failure
            (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                       (lambda (committed &rest args)
                         ;; Only the original's clear fails.
                         (if (and (eq committed session)
                                  (null (mevedel-session-schedules committed)))
                             (error "Injected storage failure")
                           (apply commit committed args)))))
              (should-error (mevedel-session-save-as-run session buffer "copy" "copy" copy)
                            :type 'user-error))))
      (should (string-search "wake-ups" (cadr failure)))
      (should (string-search "Injected storage failure" (cadr failure)))
      (should-not (file-exists-p copy))
      (should (equal (file-name-as-directory parent)
                     (file-name-as-directory (mevedel-session-save-path session))))
      (should (= 1 (length (mevedel-schedule-pending session))))
      (should (= 1 (length (plist-get (mevedel-schedule-restart-test--stored session)
                                      :schedules))))
      (should (equal index (mevedel-schedule-restart-test--snapshot-index)))))

  :doc "a Save As that fails after clearing the original gives the wake-ups back to it"
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (let* ((parent (mevedel-session-save-path session))
           (copy (file-name-concat (file-name-directory (directory-file-name parent))
                                   "copy"))
           (index (mevedel-schedule-restart-test--snapshot-index)))
      (cl-letf (((symbol-function 'mevedel-session-fork-clone-session)
                 (lambda (&rest _) (error "Injected clone failure"))))
        (mevedel-test--with-captured-diagnostics nil
          (should-error (mevedel-session-save-as-run session buffer "copy" "copy" copy))))
      (should-not (file-exists-p copy))
      (should (= 1 (length (plist-get (mevedel-schedule-restart-test--stored session)
                                      :schedules))))
      (should (equal index (mevedel-schedule-restart-test--snapshot-index)))))

  :doc "an index entry whose session or entry file cannot be read is kept, not pruned"
  (when (zerop (user-uid))
    (ert-skip "File permissions do not bind root"))
  (mevedel-schedule-restart-test--with-saved
    (mevedel-schedule-create session buffer "20m" "Check CI")
    (restart)
    (let* ((sessions (file-name-directory
                      (directory-file-name (mevedel-session-save-path session))))
           (index (mevedel-schedule-restart-test--snapshot-index))
           (junk (file-name-concat mevedel-user-dir "wake-ups" "unreadable.el"))
           (modes (file-modes sessions))
           warning)
      (with-temp-file junk (insert "\"/nowhere\""))
      (unwind-protect
          (progn
            ;; No traversal: the session directory cannot even be stat'ed.
            (set-file-modes sessions #o600)
            (set-file-modes junk #o000)
            (mevedel-test--with-captured-diagnostics warning
              (mevedel-schedule-restore))
            (should (string-search "were not restored" warning))
            (should-not mevedel-schedule--closed)
            (should-not mevedel-schedule--timer))
        (set-file-modes sessions modes)
        (when (file-exists-p junk)
          (set-file-modes junk #o600)))
      (should (file-exists-p junk))
      (delete-file junk)
      (should (equal index (mevedel-schedule-restart-test--snapshot-index)))
      ;; Once readable again, the next restore schedules it.
      (mevedel-schedule-restore)
      (should (= 1 (length mevedel-schedule--closed)))))

  :doc "pruning a pointer cannot lose a wake-up the lease holder adds meanwhile"
  (mevedel-schedule-restart-test--with-saved
    (let ((entry (mevedel-schedule-create session buffer "20m" "First")))
      (let ((index (mevedel-schedule-restart-test--snapshot-index)))
        (mevedel-schedule-cancel session buffer (plist-get entry :id))
        ;; A pointer this Emacs left behind, as after a crash.
        (mevedel-schedule-restart-test--put-back index)))
    (let ((load (symbol-function 'mevedel-session-persistence-load-sidecar))
          added)
      (cl-letf (((symbol-function 'mevedel-session-persistence-load-sidecar)
                 (lambda (&rest args)
                   (prog1 (apply load args)
                     ;; The holder commits a new wake-up after the store was
                     ;; read empty; its pointer already exists.
                     (unless added
                       (setq added t)
                       (mevedel-schedule-create session buffer "1h" "Second"))))))
        (mevedel-schedule-restore))
      (should added))
    (should (= 1 (length (mevedel-schedule-restart-test--index-files)))))

  :doc "an unreadable confirming re-read restores the pointer it just deleted"
  (when (zerop (user-uid))
    (ert-skip "File permissions do not bind root"))
  (mevedel-schedule-restart-test--with-saved
    (let ((entry (mevedel-schedule-create session buffer "20m" "First")))
      (let ((index (mevedel-schedule-restart-test--snapshot-index)))
        (mevedel-schedule-cancel session buffer (plist-get entry :id))
        (mevedel-schedule-restart-test--put-back index)))
    (let* ((store (directory-file-name (mevedel-session-save-path session)))
           (modes (file-modes store))
           (load (symbol-function 'mevedel-session-persistence-load-sidecar))
           added warning)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-session-persistence-load-sidecar)
                     (lambda (&rest args)
                       (prog1 (apply load args)
                         ;; After the empty read: the holder adds a wake-up,
                         ;; then the store stops being searchable.
                         (unless added
                           (setq added t)
                           (mevedel-schedule-create session buffer "1h" "Second")
                           (set-file-modes store #o600))))))
            (mevedel-test--with-captured-diagnostics warning
              (mevedel-schedule-restore)))
        (set-file-modes store modes))
      (should added)
      (should (string-search "were not restored" warning)))
    (should (= 1 (length (mevedel-schedule-restart-test--index-files))))))

(provide 'test-mevedel-schedule-restart)
;;; test-mevedel-schedule-restart.el ends here
