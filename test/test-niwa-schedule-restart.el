;;; test-niwa-schedule-restart.el --- Wake-ups across Emacs restarts -*- lexical-binding: t -*-

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
(require 'niwa-agent-control)
(require 'niwa-chat)
(require 'niwa-schedule)
(require 'niwa-session-artifacts)
(require 'niwa-session-codec)
(require 'niwa-session-durability)
(require 'niwa-session-fork)
(require 'niwa-session-persistence)
(require 'niwa-session-save-as)
(require 'niwa-structs)
(require 'niwa-utilities)
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))

(defconst niwa-schedule-restart-test--now 1795176000
  "2026-11-20 13:00:00 in Europe/Zurich (12:00 UTC).")

(defmacro niwa-schedule-restart-test--with-saved (&rest body)
  "Run BODY with a saved session `session' holding wake-ups made by BODY.
`buffer' is its root data buffer, `now' the clock, `submitted' the started
turns.  `restart' drops the session's in-memory state as an Emacs restart
would; `live' returns the session's live root buffer, or nil."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-schedule-restart-" t))
          (niwa-user-dir (file-name-as-directory (file-name-concat root "user")))
          (workspace (test-niwa-session-persistence--make-workspace
                      (file-name-concat root "workspace")))
          (session (niwa-session-create "main" workspace))
          (buffer (generate-new-buffer " *schedule-restart*"))
          (now niwa-schedule-restart-test--now)
          (niwa-schedule-clock (lambda () now))
          submitted)
     (unwind-protect
         (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                    ;; A real turn would send a provider request and consume the mail.
                    (lambda (prompt &rest _) (push prompt submitted))))
           (with-current-buffer buffer
             (org-mode)
             (insert "Check CI later\n")
             (niwa-session-artifacts-save session buffer)
             (setq-local niwa--session session))
           (setf (niwa-session-root-buffer session) buffer)
           (cl-flet ((restart ()
                       (test-niwa-session-persistence--release-and-kill buffer session)
                       (setq niwa-schedule--closed nil)
                       (niwa-schedule--rearm))
                     (live ()
                       (niwa-schedule-restart-test--live session)))
             ,@body))
       (while-let ((live (niwa-schedule-restart-test--live session)))
         (test-niwa-session-persistence--release-and-kill
          live (buffer-local-value 'niwa--session live)))
       (setq niwa-schedule--closed nil)
       (niwa-schedule--rearm)
       (should-not niwa-schedule--timer)
       (delete-directory root t)
       (niwa-workspace-clear-registry))))

(defun niwa-schedule-restart-test--live (session)
  "Return the live root data buffer of SESSION's id, or nil."
  (seq-find (lambda (buffer)
              (when-let* ((other (buffer-local-value 'niwa--session buffer)))
                (and (eq buffer (niwa-session-root-buffer other))
                     (equal (niwa-session-session-id session)
                            (niwa-session-session-id other)))))
            (buffer-list)))

(defun niwa-schedule-restart-test--stored (session)
  "Return SESSION's committed sidecar."
  (niwa-session-codec-read
   (file-name-concat (niwa-session-save-path session) "session.meta.el")))

(defun niwa-schedule-restart-test--mail (buffer)
  "Return the types of BUFFER's session's unread mail once it is committed."
  (let ((session (buffer-local-value 'niwa--session buffer))
        mail)
    (niwa-test--await 5 "The wake-up was not committed"
      (setq mail (niwa-agent-control-context-mailbox session)))
    (mapcar (lambda (record) (plist-get record :type)) mail)))

(defun niwa-schedule-restart-test--index-files ()
  "Return the wake-up index files under `niwa-user-dir'."
  (let ((index (file-name-concat niwa-user-dir "wake-ups")))
    (and (file-directory-p index) (directory-files index t "\\.el\\'"))))

(defun niwa-schedule-restart-test--snapshot-index ()
  "Return the index files and their contents, to put back later."
  (mapcar (lambda (file)
            (cons file (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (niwa-schedule-restart-test--index-files)))

(defun niwa-schedule-restart-test--put-back (snapshot)
  "Write the index files of SNAPSHOT back, as if the Emacs crashed first."
  (dolist (entry snapshot)
    (make-directory (file-name-directory (car entry)) t)
    (with-temp-file (car entry) (insert (cdr entry)))))

(defun niwa-schedule-restart-test--stored-mail (session)
  "Return the types of SESSION's committed mailbox records."
  (mapcar (lambda (record) (plist-get record :type))
          (plist-get (niwa-schedule-restart-test--stored session) :messages)))

(niwa-deftest niwa-schedule-hand-over ()
  ,test
  (test)
  :doc "the store and index lose the wake-ups while the session keeps them in memory"
  (niwa-schedule-restart-test--with-saved
    (should-not (niwa-schedule-hand-over session))
    (niwa-schedule-create session buffer "20m" "Check CI")
    (let ((entries (niwa-session-schedules session)))
      (should (niwa-schedule-hand-over session))
      (should (equal entries (niwa-session-schedules session)))
      (should-not (plist-get (niwa-schedule-restart-test--stored session) :schedules))
      (should-not (niwa-schedule-restart-test--index-files))))

  :doc "a failed commit signals and leaves the store, index and memory as they were"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (let ((entries (niwa-session-schedules session))
          (index (niwa-schedule-restart-test--snapshot-index)))
      (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                 (lambda (&rest _) (error "Injected storage failure"))))
        (should-error (niwa-schedule-hand-over session)))
      (should (equal entries (niwa-session-schedules session)))
      (should (= 1 (length (plist-get (niwa-schedule-restart-test--stored session) :schedules))))
      (should (equal index (niwa-schedule-restart-test--snapshot-index))))))

(niwa-deftest niwa-schedule-hand-back ()
  ,test
  (test)
  :doc "an abandoned hand-over commits and indexes the session's wake-ups again"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (let ((index (niwa-schedule-restart-test--snapshot-index)))
      (niwa-schedule-hand-over session)
      (niwa-schedule-hand-back session)
      (should (= 1 (length (plist-get (niwa-schedule-restart-test--stored session) :schedules))))
      (should (equal index (niwa-schedule-restart-test--snapshot-index))))))

(niwa-deftest niwa-schedule-restore ()
  ,test
  (test)
  :doc "a wake-up due for a closed session reopens it headlessly and is delivered once"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (restart)
    (should-not (live))
    (niwa-schedule-restore)
    (should niwa-schedule--timer)
    (should-not (live))
    (setq now (+ now 1200))
    (let ((windows (window-list nil t))
          (frames (frame-list))
          notice)
      (niwa-test--with-captured-messages notice
        (let ((inhibit-interaction t))
          (niwa-schedule-run-due)))
      (should (string-search "niwa: reopened session main for a wake-up" notice))
      (should (equal windows (window-list nil t)))
      (should (equal frames (frame-list)))
      (should-not (get-buffer-window (live) t)))
    (should (equal '(WAKE_UP) (niwa-schedule-restart-test--mail (live))))
    (should-not (plist-get (niwa-schedule-restart-test--stored session) :schedules))
    (should-not (niwa-schedule-restart-test--index-files))
    (niwa-test--await 5 "The reopened root was not woken" submitted)
    (should (= 1 (length submitted))))

  :doc "the index names a session while it holds a pending wake-up"
  (niwa-schedule-restart-test--with-saved
    (let ((entry (niwa-schedule-create session buffer "20m" "Check CI")))
      (should (= 1 (length (niwa-schedule-restart-test--index-files))))
      (niwa-schedule-cancel session buffer (plist-get entry :id))
      (should-not (niwa-schedule-restart-test--index-files))))

  :doc "a session closed before its wake-up is due is reopened for it without a restart"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (test-niwa-session-persistence--release-and-kill buffer session)
    (should niwa-schedule--timer)
    (setq now (+ now 1200))
    (niwa-test--with-captured-messages nil
      (niwa-schedule-run-due))
    (should (equal '(WAKE_UP) (niwa-schedule-restart-test--mail (live)))))

  :doc "entries of deleted sessions and malformed entries are discarded at startup"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (restart)
    (delete-directory (niwa-session-save-path session) t)
    (with-temp-file (file-name-concat niwa-user-dir "wake-ups" "junk.el")
      (insert "(not a directory"))
    (niwa-schedule-restore)
    (should-not (niwa-schedule-restart-test--index-files))
    (should-not niwa-schedule--timer))

  :doc "a firing recorded before a crash is not repeated after the restart"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (let ((index (niwa-schedule-restart-test--snapshot-index)))
      (setq now (+ now 1200))
      (niwa-schedule-run-due)
      (should (equal '(WAKE_UP) (niwa-schedule-restart-test--mail buffer)))
      ;; The crash comes after the store commit but before the index update.
      (restart)
      (niwa-schedule-restart-test--put-back index))
    (setq now (+ now 600))
    (niwa-schedule-restore)
    (should-not (niwa-schedule-restart-test--index-files))
    (should-not niwa-schedule--timer)
    (should-not (live))
    (should (equal '(WAKE_UP) (niwa-schedule-restart-test--stored-mail session))))

  :doc "a session whose lease another Emacs holds is left to that Emacs"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (restart)
    (niwa-schedule-restore)
    (let ((holder (make-process :name "schedule-lease-holder" :command '("sleep" "60")
                                :noquery t :sentinel #'ignore)))
      (unwind-protect
          (let ((identity (or (niwa--process-identity (process-id holder))
                              (ert-skip "Process identity is unavailable on this host"))))
            (let ((niwa-session-durability--client-id (make-string 64 ?b)))
              (cl-letf (((symbol-function 'niwa-session-durability--holder)
                         (lambda ()
                           (list :host (niwa-session-durability--host) :uid (user-uid)
                                 :pid (process-id holder) :process identity))))
                (should (niwa-session-durability-lease-acquire
                         (niwa-session-save-path session) "*other client*"))))
            (setq now (+ now 1200))
            (niwa-schedule-run-due)
            (should-not (live))
            (should-not niwa-schedule--timer)
            (should-not (niwa-schedule-restart-test--stored-mail session))
            (should (= 1 (length (plist-get (niwa-schedule-restart-test--stored session)
                                            :schedules))))
            ;; The holder may close the session without firing; the next start
            ;; must still find it.
            (should (= 1 (length (niwa-schedule-restart-test--index-files)))))
        (delete-process holder))))

  :doc "a wake-up that fell due while Emacs was down fires once with its overdue annotation"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (restart)
    (setq now (+ now 1200 7200))
    (niwa-schedule-restore)
    (niwa-test--with-captured-messages nil
      (niwa-schedule-run-due))
    (should (equal '(WAKE_UP) (niwa-schedule-restart-test--mail (live))))
    (should (string-search
             "overdue_by=\"2h 00m\""
             (niwa-tools--message-delivery-block
              (car (niwa-agent-control-context-mailbox
                    (buffer-local-value 'niwa--session (live))))))))

  :doc "a wake-up that expired while Emacs was down is recorded as expired without a turn"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI" "30m")
    (restart)
    (setq now (+ now 3600))
    (niwa-schedule-restore)
    (niwa-test--with-captured-messages nil
      (niwa-schedule-run-due))
    (let ((stored (plist-get (niwa-schedule-restart-test--stored session) :schedules)))
      (should (equal '(expired) (mapcar (lambda (entry) (plist-get entry :status)) stored))))
    (should-not (niwa-schedule-restart-test--stored-mail session))
    (should-not (niwa-schedule-restart-test--index-files))
    (should-not submitted))

  :doc "a recurring wake-up fires once after a restart and stays scheduled for its next run"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "every 1h" "Triage new issues")
    (restart)
    (setq now (+ now (* 3 3600) 600))
    (niwa-schedule-restore)
    (niwa-test--with-captured-messages nil
      (niwa-schedule-run-due))
    (should (equal '(WAKE_UP) (niwa-schedule-restart-test--mail (live))))
    (let ((entries (plist-get (niwa-schedule-restart-test--stored session) :schedules)))
      (should (equal '(pending) (mapcar (lambda (entry) (plist-get entry :status)) entries)))
      (should (= (+ now 3600) (plist-get (car entries) :due))))
    (should (= 1 (length (niwa-schedule-restart-test--index-files))))
    (should niwa-schedule--timer))

  :doc "after Save As only the copy holds the wake-up, so it fires once after a restart"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (let* ((parent (niwa-session-save-path session))
           (copy (file-name-concat (file-name-directory (directory-file-name parent))
                                   "copy")))
      (niwa-session-save-as-run session buffer "copy" "copy" copy)
      (should (equal (file-name-as-directory copy)
                     (file-name-as-directory (niwa-session-save-path session))))
      (should-not (plist-get (niwa-session-codec-read
                              (file-name-concat parent "session.meta.el"))
                             :schedules))
      (should (= 1 (length (plist-get (niwa-schedule-restart-test--stored session)
                                      :schedules))))
      (should (= 1 (length (niwa-schedule-restart-test--index-files))))
      (restart)
      (niwa-schedule-restore)
      (should (equal (list (niwa-schedule--directory-key copy))
                     (mapcar #'car niwa-schedule--closed)))
      (setq now (+ now 1200))
      (niwa-test--with-captured-messages nil
        (niwa-schedule-run-due))
      (should (equal '(WAKE_UP) (niwa-schedule-restart-test--mail (live))))))

  :doc "Save As stops before creating the copy when the original's wake-ups cannot be cleared"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (let* ((parent (niwa-session-save-path session))
           (copy (file-name-concat (file-name-directory (directory-file-name parent))
                                   "copy"))
           (index (niwa-schedule-restart-test--snapshot-index))
           (commit (symbol-function 'niwa-agent-control-commit-session))
           (failure
            (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                       (lambda (committed &rest args)
                         ;; Only the original's clear fails.
                         (if (and (eq committed session)
                                  (null (niwa-session-schedules committed)))
                             (error "Injected storage failure")
                           (apply commit committed args)))))
              (should-error (niwa-session-save-as-run session buffer "copy" "copy" copy)
                            :type 'user-error))))
      (should (string-search "wake-ups" (cadr failure)))
      (should (string-search "Injected storage failure" (cadr failure)))
      (should-not (file-exists-p copy))
      (should (equal (file-name-as-directory parent)
                     (file-name-as-directory (niwa-session-save-path session))))
      (should (= 1 (length (niwa-schedule-pending session))))
      (should (= 1 (length (plist-get (niwa-schedule-restart-test--stored session)
                                      :schedules))))
      (should (equal index (niwa-schedule-restart-test--snapshot-index)))))

  :doc "a Save As that fails after clearing the original gives the wake-ups back to it"
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (let* ((parent (niwa-session-save-path session))
           (copy (file-name-concat (file-name-directory (directory-file-name parent))
                                   "copy"))
           (index (niwa-schedule-restart-test--snapshot-index)))
      (cl-letf (((symbol-function 'niwa-session-fork-clone-session)
                 (lambda (&rest _) (error "Injected clone failure"))))
        (niwa-test--with-captured-diagnostics nil
          (should-error (niwa-session-save-as-run session buffer "copy" "copy" copy))))
      (should-not (file-exists-p copy))
      (should (= 1 (length (plist-get (niwa-schedule-restart-test--stored session)
                                      :schedules))))
      (should (equal index (niwa-schedule-restart-test--snapshot-index)))))

  :doc "an index entry whose session or entry file cannot be read is kept, not pruned"
  (when (zerop (user-uid))
    (ert-skip "File permissions do not bind root"))
  (niwa-schedule-restart-test--with-saved
    (niwa-schedule-create session buffer "20m" "Check CI")
    (restart)
    (let* ((sessions (file-name-directory
                      (directory-file-name (niwa-session-save-path session))))
           (index (niwa-schedule-restart-test--snapshot-index))
           (junk (file-name-concat niwa-user-dir "wake-ups" "unreadable.el"))
           (modes (file-modes sessions))
           warning)
      (with-temp-file junk (insert "\"/nowhere\""))
      (unwind-protect
          (progn
            ;; No traversal: the session directory cannot even be stat'ed.
            (set-file-modes sessions #o600)
            (set-file-modes junk #o000)
            (niwa-test--with-captured-diagnostics warning
              (niwa-schedule-restore))
            (should (string-search "were not restored" warning))
            (should-not niwa-schedule--closed)
            (should-not niwa-schedule--timer))
        (set-file-modes sessions modes)
        (when (file-exists-p junk)
          (set-file-modes junk #o600)))
      (should (file-exists-p junk))
      (delete-file junk)
      (should (equal index (niwa-schedule-restart-test--snapshot-index)))
      ;; Once readable again, the next restore schedules it.
      (niwa-schedule-restore)
      (should (= 1 (length niwa-schedule--closed)))))

  :doc "pruning a pointer cannot lose a wake-up the lease holder adds meanwhile"
  (niwa-schedule-restart-test--with-saved
    (let ((entry (niwa-schedule-create session buffer "20m" "First")))
      (let ((index (niwa-schedule-restart-test--snapshot-index)))
        (niwa-schedule-cancel session buffer (plist-get entry :id))
        ;; A pointer this Emacs left behind, as after a crash.
        (niwa-schedule-restart-test--put-back index)))
    (let ((load (symbol-function 'niwa-session-persistence-load-sidecar))
          added)
      (cl-letf (((symbol-function 'niwa-session-persistence-load-sidecar)
                 (lambda (&rest args)
                   (prog1 (apply load args)
                     ;; The holder commits a new wake-up after the store was
                     ;; read empty; its pointer already exists.
                     (unless added
                       (setq added t)
                       (niwa-schedule-create session buffer "1h" "Second"))))))
        (niwa-schedule-restore))
      (should added))
    (should (= 1 (length (niwa-schedule-restart-test--index-files)))))

  :doc "an unreadable confirming re-read restores the pointer it just deleted"
  (when (zerop (user-uid))
    (ert-skip "File permissions do not bind root"))
  (niwa-schedule-restart-test--with-saved
    (let ((entry (niwa-schedule-create session buffer "20m" "First")))
      (let ((index (niwa-schedule-restart-test--snapshot-index)))
        (niwa-schedule-cancel session buffer (plist-get entry :id))
        (niwa-schedule-restart-test--put-back index)))
    (let* ((store (directory-file-name (niwa-session-save-path session)))
           (modes (file-modes store))
           (load (symbol-function 'niwa-session-persistence-load-sidecar))
           added warning)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-session-persistence-load-sidecar)
                     (lambda (&rest args)
                       (prog1 (apply load args)
                         ;; After the empty read: the holder adds a wake-up,
                         ;; then the store stops being searchable.
                         (unless added
                           (setq added t)
                           (niwa-schedule-create session buffer "1h" "Second")
                           (set-file-modes store #o600))))))
            (niwa-test--with-captured-diagnostics warning
              (niwa-schedule-restore)))
        (set-file-modes store modes))
      (should added)
      (should (string-search "were not restored" warning)))
    (should (= 1 (length (niwa-schedule-restart-test--index-files))))))

(provide 'test-niwa-schedule-restart)
;;; test-niwa-schedule-restart.el ends here
