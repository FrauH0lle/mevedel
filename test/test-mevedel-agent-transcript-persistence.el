;;; test-mevedel-agent-transcript-persistence.el --- Agent transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;; Sub-agent transcript persistence.  Covers the load-bearing
;; invariants: path validation, sanitization,
;; sidecar round-trip, shallow materialization, session-resume
;; transitions, fork pruning, and the helper surface in
;; `mevedel-agent-exec.el' and `mevedel-tool-ui.el'.
;;
;; FSM-driven dispatch flows (`mevedel-agent-runtime-dispatch'
;; with a real agent buffer + gptel-request) are exercised
;; through stubbed gptel functions because gptel-request is
;; transport-bound and not reachable from the test environment.

;;; Code:

(require 'gptel)
(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-reminders)
(require 'mevedel-agents)
(require 'mevedel-agent-exec)
(require 'mevedel-session-persistence)
(require 'mevedel-pipeline)
(require 'mevedel-tools)
(require 'mevedel-tool-ui)
(require 'mevedel-view)
(require 'mevedel-chat)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(mevedel-tools-register)

(defvar gptel-org-ignore-elements)
(defvar org-element-cache-persistent)
(defvar org-element-use-cache)
(defvar org-indent-mode)
(defvar org-mode-hook)
(declare-function org-entry-delete "org" (pom property))
(declare-function org-entry-put "org" (pom property value))
(declare-function org-indent-mode "org-indent" (&optional arg))


;;
;;; Setup helpers

(defun test-mevedel-spec21--make-workspace ()
  "Return (WORKSPACE . TEMPDIR) rooted in a fresh tempdir."
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-test-spec21-" t)))
         (basename (file-name-nondirectory (directory-file-name tempdir)))
         (_       (mevedel-workspace-clear-registry))
         (ws      (mevedel-workspace-get-or-create
                   'project basename tempdir basename)))
    (cons ws tempdir)))

(defun test-mevedel-spec21--release-and-kill (buf session)
  "Release SESSION's lock and kill BUF if alive."
  (when (and session (mevedel-session-save-path session))
    (mevedel-session-persistence-lock-release
     (mevedel-session-save-path session)))
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf)))

(defun test-mevedel-spec21--kill-buffer (buf)
  "Kill BUF without driving agent-buffer abort hooks."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (set-buffer-modified-p nil)
      (setq kill-buffer-hook nil))
    (kill-buffer buf)))

(defun test-mevedel-spec21--make-agent-buffer
    (agent-name agent-id parent-buffer session)
  "Create an agent invocation buffer for AGENT-NAME.
AGENT-ID, PARENT-BUFFER, and SESSION configure the invocation."
  (let* ((agent (mevedel-agent--create :name agent-name
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) agent-id)
    (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buffer)
    (setf (mevedel-agent-invocation-parent-session inv) session)
    (let ((buf (mevedel-agent-exec--allocate-agent-buffer
                inv parent-buffer)))
      (setf (mevedel-agent-invocation-buffer inv) buf)
      (cons inv buf))))

(defun test-mevedel-spec21--register-agent (parent-buffer agent-id inv)
  "Register INV under AGENT-ID in PARENT-BUFFER's live agent registry.
Returns nil; callers may pass the result to the shared cleanup helper."
  (let ((fsm (gptel-make-fsm
              :info (list :mevedel-agent-invocation inv
                          :buffer (mevedel-agent-invocation-buffer inv)))))
    (with-current-buffer parent-buffer
      (setf (alist-get agent-id mevedel-agent-runtime--fsms nil nil #'equal)
            fsm))
    nil))

;;
;;; Sidecar sanitization

(mevedel-deftest mevedel-session-persistence--sanitize-agent-transcripts ()
  ,test
  (test)

  :doc "passes through valid entries"
  (let* ((entry '("explorer--abc"
                  :agent-type "explorer" :description "test"
                  :path "agents/x.chat.org" :status running
                  :created-at "2026-04-25T14-00-00"
                  :updated-at "2026-04-25T14-00-00"
                  :parent-turn 1))
         (sanitized
          (mevedel-session-persistence--sanitize-agent-transcripts
           (list entry))))
    (should (equal (length sanitized) 1))
    (should (equal (caar sanitized) "explorer--abc"))
    (should (eq (plist-get (cdar sanitized) :status) 'running)))

  :doc "coerces unknown status to incomplete"
  (let* ((entry '("a--xyz" :status bogus :path "agents/a.chat.org"))
         (out (mevedel-session-persistence--sanitize-agent-transcripts
               (list entry))))
    (should (eq (plist-get (cdar out) :status) 'incomplete)))

  :doc "deduplicates by agent-id keeping newest updated-at"
  (let* ((entries
          '(("dup--id" :status completed
             :path "agents/dup.chat.org"
             :updated-at "2026-04-25T14-00-00")
            ("dup--id" :status running
             :path "agents/dup.chat.org"
             :updated-at "2026-04-25T15-00-00")))
         (out (mevedel-session-persistence--sanitize-agent-transcripts
               entries)))
    (should (equal (length out) 1))
    (should (eq (plist-get (cdar out) :status) 'running))))


;;
;;; Sidecar round-trip

(mevedel-deftest mevedel-session-persistence-agent-transcripts-roundtrip ()
  ,test
  (test)
  :doc "agent-transcripts round-trips serialize/deserialize"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (entry '("explorer--rt"
                        :agent-type "explorer"
                        :description "round trip"
                        :path "agents/explorer--rt.chat.org"
                        :status completed
                        :created-at "2026-04-25T14-00-00"
                        :updated-at "2026-04-25T14-00-30"
                        :parent-turn 3)))
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "explorer--rt" (cdr entry))))
          (let* ((sidecar
                  (mevedel-session-persistence-serialize
                   session
                   :first-user-message nil
                   :additional-roots nil))
                 (reloaded
                  (plist-get
                   (mevedel-session-persistence-deserialize sidecar)
                   :session))
                 (transcripts
                  (mevedel-session-agent-transcripts reloaded)))
            (should (equal (length transcripts) 1))
            (should (equal (caar transcripts) "explorer--rt"))
            (should (eq (plist-get (cdar transcripts) :status) 'completed))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))



;;
;;; Resume rewrites running -> incomplete

(mevedel-deftest mevedel-session-persistence--mark-running-incomplete-on-resume ()
  ,test
  (test)

  :doc "rewrites running -> incomplete by default"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "a--1" (list :status 'running))
                      (cons "b--2" (list :status 'completed))))
          (mevedel-session-persistence--mark-running-incomplete-on-resume
           session nil)
          (should (eq (plist-get (cdr (assoc "a--1"
                                             (mevedel-session-agent-transcripts
                                              session)))
                                 :status)
                      'incomplete))
          (should (eq (plist-get (cdr (assoc "b--2"
                                             (mevedel-session-agent-transcripts
                                              session)))
                                 :status)
                      'completed)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "skips rewrite in read-only attach mode"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "a--1" (list :status 'running))))
          (mevedel-session-persistence--mark-running-incomplete-on-resume
           session t)
          (should (eq (plist-get (cdr (assoc "a--1"
                                             (mevedel-session-agent-transcripts
                                              session)))
                                 :status)
                      'running)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Fork pruning

(mevedel-deftest mevedel-session-persistence--prune-agent-transcripts-after-fork ()
  ,test
  (test)
  :doc "drops entries whose parent-turn > fork-turn"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "early--a"
                            (list :status 'completed :parent-turn 2))
                      (cons "at-fork--b"
                            (list :status 'completed :parent-turn 5))
                      (cons "after--c"
                            (list :status 'completed :parent-turn 6))
                      (cons "way-after--d"
                            (list :status 'completed :parent-turn 10))))
          (mevedel-session-persistence--prune-agent-transcripts-after-fork
           session 5)
          (let ((kept (mapcar #'car
                              (mevedel-session-agent-transcripts session))))
            (should (member "early--a" kept))
            (should (member "at-fork--b" kept))
            (should-not (member "after--c" kept))
            (should-not (member "way-after--d" kept))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Shallow materialization

(mevedel-deftest mevedel-session-persistence--shallow-ensure-files ()
  ,test
  (test)

  :doc "creates session-dir + agents/ without writing session.meta.el"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*spec21-shallow-data*"))
               save-path)
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq save-path
                      (mevedel-session-persistence--shallow-ensure-files
                       session buf))
                (should save-path)
                (should (file-directory-p save-path))
                (should (file-directory-p (file-name-concat save-path "agents")))
                ;; Critical: session.meta.el must NOT exist yet.
                (should-not
                 (file-exists-p (file-name-concat save-path "session.meta.el")))
                ;; Lock acquired.
                (should (file-exists-p (file-name-concat save-path ".lock")))
                ;; Idempotent.
                (should (equal save-path
                               (mevedel-session-persistence--shallow-ensure-files
                                session buf))))
            (test-mevedel-spec21--release-and-kill buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Agent invocation persistence slots

(mevedel-deftest mevedel-agent-invocation-persistence-slots ()
  ,test
  (test)
  :doc "invocation struct has transcript persistence slots"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explorer--xyz")
    (setf (mevedel-agent-invocation-description inv) "test")
    (setf (mevedel-agent-invocation-parent-turn inv) 4)
    (setf (mevedel-agent-invocation-transcript-status inv) 'running)
    (setf (mevedel-agent-invocation-transcript-relative-path inv)
          "agents/explorer--xyz.chat.org")
    (should (equal (mevedel-agent-invocation-agent-id inv) "explorer--xyz"))
    (should (equal (mevedel-agent-invocation-description inv) "test"))
    (should (equal (mevedel-agent-invocation-parent-turn inv) 4))
    (should (eq (mevedel-agent-invocation-transcript-status inv) 'running))
    (should (equal (mevedel-agent-invocation-transcript-relative-path inv)
                   "agents/explorer--xyz.chat.org"))))


;;
;;; Agent buffer allocation

(mevedel-deftest mevedel-agent-transcript-persistence--allocate-agent-buffer ()
  ,test
  (test)

  :doc "buffer is org-mode and carries parent context bindings"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
	        (let* ((session (mevedel-session-create "main" workspace))
	               (parent-buf (generate-new-buffer "*spec21-parent*"))
	               (agent (mevedel-agent--create :name "explorer"
	                                             :system-prompt "stub"
	                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--abcd1234")
	          (with-current-buffer parent-buf
	            (setq-local mevedel--session session)
	            (setq-local mevedel--workspace workspace))
	          (let ((agent-buf
	                 (let ((gptel-org-ignore-elements
	                        '(property-drawer src-block))
			       (org-mode-hook
				(cons (lambda () (org-indent-mode +1))
				      org-mode-hook)))
	                   (mevedel-agent-exec--allocate-agent-buffer
	                    inv parent-buf))))
	            (unwind-protect
	                (with-current-buffer agent-buf
	                  (should (derived-mode-p 'org-mode))
	                  (should-not org-element-use-cache)
	                  (should-not org-element-cache-persistent)
	                  (should-not org-indent-mode)
	                  (should (equal '(property-drawer)
	                                 gptel-org-ignore-elements))
	                  (should (eq mevedel--session session))
	                  (should (eq mevedel--workspace workspace))
                  (should (eq mevedel--agent-invocation inv))
                  (should (string-match-p "mevedel-agent-abcd1234"
                                          (buffer-name))))
              (when (buffer-live-p agent-buf)
                (with-current-buffer agent-buf
                  (set-buffer-modified-p nil)
                  ;; Drop the kill hook so kill-buffer doesn't trip
                  ;; gptel-abort on a non-FSM buffer.
                  (setq kill-buffer-hook nil))
                (kill-buffer agent-buf))
              (kill-buffer parent-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Agent-result format / parse




;;
;;; mevedel--active-chat-buffer (agent buffer short-circuit)

(mevedel-deftest mevedel--active-chat-buffer-agent-buffer ()
  ,test
  (test)

  :doc "agent buffer returns parent-data-buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-parent-active*"))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--A")
          (setf (mevedel-agent-invocation-parent-data-buffer inv)
                parent-buf)
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (let ((agent-buf
                 (mevedel-agent-exec--allocate-agent-buffer
                  inv parent-buf)))
            (unwind-protect
                (with-current-buffer agent-buf
                  ;; Active-chat should return the parent, not the agent buffer.
                  (should (eq (mevedel--active-chat-buffer) parent-buf)))
              (when (buffer-live-p agent-buf)
                (with-current-buffer agent-buf
                  (set-buffer-modified-p nil)
                  (setq kill-buffer-hook nil))
                (kill-buffer agent-buf))
              (kill-buffer parent-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "dead parent falls through to scan branch"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-parent-dead*"))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--B")
          (setf (mevedel-agent-invocation-parent-data-buffer inv)
                parent-buf)
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (let ((agent-buf
                 (mevedel-agent-exec--allocate-agent-buffer
                  inv parent-buf)))
            (unwind-protect
                (progn
                  ;; Kill the parent.
                  (kill-buffer parent-buf)
                  (with-current-buffer agent-buf
                    ;; Falls through to scan branch; scan now skips
                    ;; agent buffers, so it returns nil here.
                    (should-not (eq (mevedel--active-chat-buffer)
                                    (current-buffer)))))
              (when (buffer-live-p agent-buf)
                (with-current-buffer agent-buf
                  (set-buffer-modified-p nil)
                  (setq kill-buffer-hook nil))
                (kill-buffer agent-buf)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Workspace sessions filter

(mevedel-deftest mevedel--workspace-sessions-skips-agent-buffers ()
  ,test
  (test)
  :doc "agent buffers carrying parent session are not returned"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (chat-buf (generate-new-buffer "*spec21-ws-chat*"))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--ws")
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (let ((agent-buf
                 (mevedel-agent-exec--allocate-agent-buffer inv chat-buf)))
            (unwind-protect
                (let ((sessions (mevedel--workspace-sessions workspace)))
                  ;; The chat buffer should be in the list.
                  (should (rassoc chat-buf sessions))
                  ;; The agent buffer should NOT be.
                  (should-not (rassoc agent-buf sessions)))
              (when (buffer-live-p agent-buf)
                (with-current-buffer agent-buf
                  (set-buffer-modified-p nil)
                  (setq kill-buffer-hook nil))
                (kill-buffer agent-buf))
              (kill-buffer chat-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Save helper (no-op for dead/unsaved buffers)

(mevedel-deftest mevedel-agent-exec--save-transcript-buffer ()
  ,test
  (test)

  :doc "no-op when buffer-file-name is unset"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent))
         (buf (generate-new-buffer "*spec21-save-no-file*")))
    (setf (mevedel-agent-invocation-buffer inv) buf)
    (unwind-protect
        (should-not (mevedel-agent-exec--save-transcript-buffer inv))
      (kill-buffer buf)))

  :doc "no-op for dead buffer"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent))
         (buf (generate-new-buffer "*spec21-save-dead*")))
    (setf (mevedel-agent-invocation-buffer inv) buf)
    (kill-buffer buf)
    (should-not (mevedel-agent-exec--save-transcript-buffer inv)))

  :doc "saves modified transcript without logging file-write messages"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (save-path (file-name-as-directory
                           (file-name-concat tempdir "session")))
               (rel "agents/explorer--silent.chat.org")
               (abs (expand-file-name rel save-path))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent))
               (buf (generate-new-buffer "*spec21-save-silent*"))
               marker)
          (make-directory (file-name-directory abs) t)
          (setf (mevedel-session-save-path session) save-path)
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--silent")
          (setf (mevedel-agent-invocation-buffer inv) buf)
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-transcript-relative-path inv) rel)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (insert "transcript\n")
                  (set-visited-file-name abs t t)
                  (set-buffer-modified-p t))
                (with-current-buffer (get-buffer-create "*Messages*")
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert "\n-- mevedel transcript save sentinel --\n")
                    (setq marker (copy-marker (point-max) t))))
                (should (mevedel-agent-exec--save-transcript-buffer inv))
                (let ((logged (with-current-buffer "*Messages*"
                                (buffer-substring-no-properties
                                 marker (point-max)))))
                  (should-not (string-match-p
                               (regexp-quote "explorer--silent.chat.org")
                               logged))
                  (should-not (string-match-p "\\bwritten\\b" logged))
                  (should-not (string-match-p "\\bWrote\\b" logged))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (set-buffer-modified-p nil)
                (setq kill-buffer-hook nil))
              (kill-buffer buf))))
	      (delete-directory tempdir t)
	      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-agent-exec--save-transcript-buffer-suppresses-undo-tree-history ()
  ,test
  (test)
  :doc "transcript save keeps before-save hooks but skips undo-tree history"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (save-path (file-name-as-directory
                           (file-name-concat tempdir "session")))
               (rel "agents/explorer--hooks.chat.org")
               (abs (expand-file-name rel save-path))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent))
               (buf (generate-new-buffer "*spec21-save-hooks*"))
               before-ran
               undo-ran)
          (make-directory (file-name-directory abs) t)
          (setf (mevedel-session-save-path session) save-path)
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--hooks")
          (setf (mevedel-agent-invocation-buffer inv) buf)
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-transcript-relative-path inv) rel)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (insert "transcript\n")
                  (set-visited-file-name abs t t)
                  (add-hook 'before-save-hook
                            (lambda () (setq before-ran t))
                            nil t)
                  (setq-local write-file-functions
                              '(undo-tree-save-history-from-hook))
                  (setq-local undo-tree-auto-save-history t)
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function
                            'undo-tree-save-history-from-hook)
                           (lambda (&rest _)
                             (setq undo-ran t)
                             nil)))
                  (should (mevedel-agent-exec--save-transcript-buffer inv)))
                (should before-ran)
                (should-not undo-ran))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (set-buffer-modified-p nil)
                (setq kill-buffer-hook nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-agent-exec--save-transcript-buffer-fast-property-writes ()
  ,test
  (test)
  :doc "transcript save before-save metadata avoids slow Org entry writes"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (save-path (file-name-as-directory
                           (file-name-concat tempdir "session")))
               (rel "agents/explorer--fast.chat.org")
               (abs (expand-file-name rel save-path))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent))
               (buf (generate-new-buffer "*spec21-save-fast*")))
          (make-directory (file-name-directory abs) t)
          (setf (mevedel-session-save-path session) save-path)
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--fast")
          (setf (mevedel-agent-invocation-buffer inv) buf)
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-transcript-relative-path inv) rel)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local mevedel--session session)
                  (insert "transcript\n")
                  (add-hook
                   'before-save-hook
                   (lambda ()
                     (mevedel-session-persistence--save-gptel-state-around
                      (lambda ()
                        (org-entry-put (point-min)
                                       "GPTEL_MODEL" "fake-model"))))
                   nil t)
                  (set-visited-file-name abs t t)
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'org-entry-put)
                           (lambda (&rest _)
                             (error "Slow org-entry-put should not run")))
                          ((symbol-function 'org-entry-delete)
                           (lambda (&rest _)
                             (error "Slow org-entry-delete should not run"))))
                  (should (mevedel-agent-exec--save-transcript-buffer inv)))
                (with-temp-buffer
                  (insert-file-contents abs)
                  (should (search-forward
                           ":GPTEL_MODEL: fake-model" nil t))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (set-buffer-modified-p nil)
                (setq kill-buffer-hook nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Insert injected prompt

(mevedel-deftest mevedel-agent-transcript-persistence--insert-injected-prompt ()
  ,test
  (test)
  :doc "appends BLOCK to the agent buffer at point-max by default"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent))
         (buf (generate-new-buffer "*spec21-inject*")))
    (setf (mevedel-agent-invocation-buffer inv) buf)
    (unwind-protect
        (with-current-buffer buf
          (insert "preceding\n")
          (mevedel-agent-exec--insert-injected-prompt
           inv "<agent-message from=\"sender\">hi</agent-message>")
          (should (string-match-p "preceding"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
          (should (string-match-p "<agent-message"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))
      (kill-buffer buf)))

  :doc "prepends BLOCK above the task heading when requested"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent))
         (buf (generate-new-buffer "*spec21-inject-prepend*")))
    (setf (mevedel-agent-invocation-buffer inv) buf)
    (unwind-protect
        (with-current-buffer buf
          (insert "* Agent Task: do work\nbody\n")
          (mevedel-agent-exec--insert-injected-prompt
           inv "<agent-message from=\"sender\">hi</agent-message>"
           'prepend)
          (let ((text (buffer-substring-no-properties
                       (point-min) (point-max))))
            (should (< (string-match-p "<agent-message" text)
                       (string-match-p "^\\* Agent Task:" text)))))
      (kill-buffer buf))))


;;
;;; Finalize sets terminal status




;;
;;; Foreground response wrapping with render-data




;;
;;; Render-agent renderer consumes render-data

(mevedel-deftest mevedel-tool-ui--render-agent-render-data ()
  ,test
  (test)

  :doc "header includes status badge without attribution when render-data is present"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (save-path (file-name-as-directory
                           (file-name-concat tempdir "fake-session"))))
          (make-directory (file-name-concat save-path "agents") t)
          (setf (mevedel-session-save-path session) save-path)
          (let* ((mevedel--session session)
                 (args '(:subagent_type "explorer" :description "test"))
                 (rd '(:kind agent-transcript :agent-id "explorer--rd"
                       :transcript-relative-path "agents/explorer--rd.chat.org"
                       :status running)))
            (let ((rendering (mevedel-tool-ui--render-agent
                              "Agent" args "result body" rd)))
              (should (string-match-p "\\[running\\]"
                                      (plist-get rendering :header)))
              (should-not (string-match-p "from explorer--rd"
                                          (plist-get rendering :header))))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "header omits suffix when render-data absent"
  (let* ((args '(:subagent_type "explorer" :description "test")))
    (let ((rendering (mevedel-tool-ui--render-agent
                      "Agent" args "result body" nil)))
      (should-not (string-match-p "\\[transcript:"
                                  (plist-get rendering :header))))))



(mevedel-deftest mevedel-tool-ui--handle-badge-verdict ()
  ,test
  (test)
  :doc "completed verifier handles show parsed verdict state"
  (should (string-match-p
           "verdict PASS"
           (mevedel-tool-ui--handle-badge
            '(:status completed :verdict pass))))
  (should (string-match-p
           "verdict FAIL"
           (mevedel-tool-ui--handle-badge
            '(:status completed :verdict fail))))
  (should (string-match-p
           "verdict PARTIAL"
           (mevedel-tool-ui--handle-badge
            '(:status completed :verdict partial)))))


;;
;;; LLM-facing string never carries transcript=

(mevedel-deftest mevedel-tools--agent-result-no-transcript-attr ()
  ,test
  (test)
  :doc "format helper never emits a transcript= attribute"
  (let ((s (mevedel-agent-runtime--agent-result-format
            "explorer--llm" "explorer" "desc" "body")))
    (should-not (string-match-p "transcript=" s))))


;;
;;; Wrap-callback ordering: forward-first / bookkeep-after

(mevedel-deftest mevedel-agent-exec--wrap-callback ()
  ,test
  (test)

  :doc "insertable events forward to gptel-cb before mevedel-cb"
  (let* ((order nil)
         (gptel-cb (lambda (&rest _) (push 'gptel order)))
         (mevedel-cb (lambda (&rest _) (push 'mevedel order)))
         (wrapped (mevedel-agent-exec--wrap-callback gptel-cb mevedel-cb)))
    (funcall wrapped "chunk" '(:context nil))
    (should (equal (nreverse order) '(gptel mevedel))))

  :doc "terminal events skip the gptel forward"
  (let* ((order nil)
         (gptel-cb (lambda (&rest _) (push 'gptel order)))
         (mevedel-cb (lambda (&rest _) (push 'mevedel order)))
         (wrapped (mevedel-agent-exec--wrap-callback gptel-cb mevedel-cb)))
    (funcall wrapped t '(:context nil))
    (should (equal order '(mevedel))))

  :doc "abort terminal also skips gptel forward"
  (let* ((order nil)
         (gptel-cb (lambda (&rest _) (push 'gptel order)))
         (mevedel-cb (lambda (&rest _) (push 'mevedel order)))
         (wrapped (mevedel-agent-exec--wrap-callback gptel-cb mevedel-cb)))
    (funcall wrapped 'abort '(:context nil))
    (should (equal order '(mevedel))))

  :doc "errors in gptel-cb don't strand mevedel-cb"
  (let* ((mevedel-fired nil)
         (gptel-cb (lambda (&rest _) (error "Boom")))
         (mevedel-cb (lambda (&rest _) (setq mevedel-fired t)))
         (wrapped (mevedel-agent-exec--wrap-callback gptel-cb mevedel-cb)))
    (funcall wrapped "chunk" '(:context nil))
    (should mevedel-fired))

  :doc "tolerates gptel's optional 3rd `raw' argument"
  (let* ((mevedel-args nil)
         (gptel-cb (lambda (&rest _args) nil))
         (mevedel-cb (lambda (resp _info &rest rest)
                       (setq mevedel-args (list resp rest))))
         (wrapped (mevedel-agent-exec--wrap-callback gptel-cb mevedel-cb)))
    (funcall wrapped "chunk" '(:context nil) 'raw-flag)
    (should (equal (car mevedel-args) "chunk"))
    (should (equal (cadr mevedel-args) '(raw-flag)))))


;;
;;; Path collision through allocation






;;
;;; Visible-window kill rule




;;
;;; mevedel-abort tears down sub-agent buffers

(mevedel-deftest mevedel-abort-targets-agent-buffers ()
  ,test
  (test)
  :doc "mevedel-abort phase-2 also matches registered agent buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-abort-parent*"))
               (agent-buf (generate-new-buffer "*mevedel-agent-fake*"))
               (fake-fsm (gptel-make-fsm
                          :info (list :buffer agent-buf))))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons "explorer--abrt" fake-fsm))))
          (let* ((collected nil)
                 (gptel--request-alist
                  (list (cons (current-buffer)
                              (cons fake-fsm #'ignore)))))
            (cl-letf (((symbol-function 'gptel-abort)
                       (lambda (buf)
                         (push buf collected)
                         (setq gptel--request-alist nil))))
              (with-current-buffer parent-buf
                (mevedel-abort)))
            (should (memq agent-buf collected)))
          (kill-buffer agent-buf)
          (kill-buffer parent-buf))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Renderer affordance excluded for malformed render-data

(mevedel-deftest mevedel-tool-ui--render-agent-malformed-render-data ()
  ,test
  (test)

  :doc "non-agent-transcript :kind yields no affordance"
  (let* ((args '(:subagent_type "explorer" :description "test"))
         (rd '(:kind something-else :agent-id "x")))
    (let ((rendering (mevedel-tool-ui--render-agent
                      "Agent" args "body" rd)))
      (should-not (string-match-p "\\[transcript:"
                                  (plist-get rendering :header)))))

  :doc "missing :transcript-relative-path yields no affordance"
  (let* ((args '(:subagent_type "explorer" :description "test"))
         (rd '(:kind agent-transcript :agent-id "x")))
    (let ((rendering (mevedel-tool-ui--render-agent
                      "Agent" args "body" rd)))
      (should-not (string-match-p "\\[transcript:"
                                  (plist-get rendering :header)))))

  :doc "path with .. is rejected"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (save-path (file-name-as-directory
                           (file-name-concat tempdir "fake-session"))))
          (make-directory (file-name-concat save-path "agents") t)
          (setf (mevedel-session-save-path session) save-path)
          (let* ((mevedel--session session)
                 (args '(:subagent_type "explorer" :description "test"))
                 (rd '(:kind agent-transcript :agent-id "x"
                       :transcript-relative-path "agents/../escape.chat.org"
                       :status running)))
            (let ((rendering (mevedel-tool-ui--render-agent
                              "Agent" args "body" rd)))
              (should-not (string-match-p "\\[transcript:"
                                          (plist-get rendering :header))))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; gptel-request runs in the agent buffer's context

(mevedel-deftest mevedel-agent-exec--run-current-buffer ()
  ,test
  (test)
  :doc "agent dispatch path makes the agent buffer current before gptel-request"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-cb-parent*"))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent))
               (agent-buf nil)
               (request-buffer nil))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--cb")
          (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setq agent-buf
                (mevedel-agent-exec--allocate-agent-buffer inv parent-buf))
          (setf (mevedel-agent-invocation-buffer inv) agent-buf)
          (let ((gptel-send--transitions
                 (or (and (boundp 'gptel-send--transitions)
                          gptel-send--transitions)
                     '((INIT . ((t . WAIT))))))
                (gptel--fsm-last
                 (gptel-make-fsm
                  :info (list :buffer parent-buf
                              :position (with-current-buffer parent-buf
                                          (point-marker))))))
            (cl-letf* (((symbol-function 'gptel--update-status)
                        #'ignore)
                       ((symbol-function 'mevedel-agent-exec--make-callback)
                        (lambda (&rest _) (lambda (&rest _) nil)))
                       ((symbol-function 'gptel-with-preset)
                        (lambda (_preset &rest body)
                          (eval (cons 'progn body) t)))
                       ((symbol-function 'mevedel-agent-runtime--augment-agent-handlers)
                        (lambda (handlers &rest _) handlers))
                       ((symbol-function 'mevedel-agent-runtime--inject-bwait-transition)
                        #'ignore)
                       ((symbol-function 'gptel-request)
                        (lambda (&rest _args)
                          (setq request-buffer (current-buffer))
                          (throw 'spec21-cb-done t))))
              (setf
               (mevedel-agent-invocation-frozen-configuration inv)
               (mevedel-agent-exec-freeze-configuration
                "explorer" inv
                (list :backend gptel-backend
                      :model gptel-model
                      :effort (and (boundp 'gptel-reasoning-effort)
                                   gptel-reasoning-effort))))
              (with-current-buffer parent-buf
                (catch 'spec21-cb-done
                  (mevedel-agent-exec--run
                   #'ignore "explorer" "test desc" "test prompt"
                   inv agent-buf)))))
          ;; The mocked gptel-request must have observed the agent
          ;; buffer as current -- without `with-current-buffer
          ;; agent-buffer' the call would land in the parent and
          ;; gptel's default `:position' marker would be wrong.
          (should (eq request-buffer agent-buf))
          (when (buffer-live-p agent-buf)
            (with-current-buffer agent-buf
              (set-buffer-modified-p nil)
              (setq kill-buffer-hook nil))
            (kill-buffer agent-buf))
          (kill-buffer parent-buf))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-agent-exec--run-first-turn ()
  ,test
  (test)
  :doc "agent dispatch tolerates a fresh parent buffer with no gptel--fsm-last"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-first-parent*"))
               (agent (mevedel-agent--create :name "coordinator"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent))
               (agent-buf nil)
               (tracking-marker nil)
               (request-buffer nil)
               (request-fsm nil)
               (request-use-tools nil))
          (with-current-buffer parent-buf
            (insert "### /coordinator run first turn\n")
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (setf (mevedel-agent-invocation-agent-id inv) "coordinator--first")
          (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setq agent-buf
                (mevedel-agent-exec--allocate-agent-buffer inv parent-buf))
          (setf (mevedel-agent-invocation-buffer inv) agent-buf)
          (let ((gptel-send--transitions
                 (or (and (boundp 'gptel-send--transitions)
                          gptel-send--transitions)
                     '((INIT . ((t . WAIT))))))
                (gptel--fsm-last nil))
            (cl-letf* (((symbol-function 'gptel--update-status)
                        #'ignore)
                       ((symbol-function 'mevedel-agent-exec--make-callback)
                        (lambda (_main _type _desc where _partial)
                          (setq tracking-marker where)
                          (lambda (&rest _) nil)))
                       ((symbol-function 'mevedel-agent-runtime--augment-agent-handlers)
                        (lambda (handlers &rest args)
                          (let ((prepend (plist-get args :prepend))
                                (append (plist-get args :append)))
                            (dolist (entry prepend)
                              (let ((cell (assq (car entry) handlers)))
                                (if cell
                                    (setcdr cell (append (cdr entry)
                                                         (cdr cell)))
                                  (push entry handlers))))
                            (dolist (entry append)
                              (let ((cell (assq (car entry) handlers)))
                                (if cell
                                    (setcdr cell (append (cdr cell)
                                                         (cdr entry)))
                                  (push entry handlers))))
                            handlers)))
                       ((symbol-function 'mevedel-agent-runtime--inject-bwait-transition)
                        #'ignore)
                       ((symbol-function 'gptel-request)
                        (lambda (&rest args)
                          (setq request-buffer (current-buffer))
                          (setq request-fsm (plist-get (cdr args) :fsm))
                          (setq request-use-tools gptel-use-tools)
                          (throw 'spec21-first-done t))))
              (setf
               (mevedel-agent-invocation-frozen-configuration inv)
               (mevedel-agent-exec-freeze-configuration
                "coordinator" inv
                (list :backend gptel-backend
                      :model gptel-model
                      :effort (and (boundp 'gptel-reasoning-effort)
                                   gptel-reasoning-effort))))
              (with-current-buffer parent-buf
                (catch 'spec21-first-done
                  (mevedel-agent-exec--run
                   #'ignore "coordinator" "first desc" "first prompt"
                   inv agent-buf)))))
          (should (markerp tracking-marker))
          (should (eq (marker-buffer tracking-marker) parent-buf))
          (should (eq request-buffer agent-buf))
          (should (eq request-use-tools 'force))
          (should (memq #'mevedel-agent-exec--clear-forced-tool-choice
                        (alist-get 'TPRE (gptel-fsm-handlers request-fsm))))
          (when (buffer-live-p agent-buf)
            (with-current-buffer agent-buf
              (set-buffer-modified-p nil)
              (setq kill-buffer-hook nil))
            (kill-buffer agent-buf))
          (kill-buffer parent-buf))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Preset values land buffer-local on the agent buffer

(mevedel-deftest mevedel-agent-exec--apply-request-locals-overrides-existing ()
  ,test
  (test)
  :doc "apply-request-locals overrides preexisting buffer-local values"
  ;; Regression: gptel-request copies these via `buffer-local-value'
  ;; when building its prompt buffer (gptel-request.el:1039-1054), so
  ;; pre-existing buffer-local values on the agent buffer would
  ;; otherwise shadow the active preset.
  (let ((buf (generate-new-buffer " *spec21-locals-override*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-tools '(stale-tools))
            (setq-local gptel-backend 'stale-backend)
            (setq-local gptel-model 'stale-model))
          (mevedel-agent-exec--apply-request-locals
           buf
           '((gptel-tools . (fresh-tools))
             (gptel-backend . fresh-backend)
             (gptel-model . fresh-model)))
          (should (equal (buffer-local-value 'gptel-tools buf)
                         '(fresh-tools)))
          (should (eq (buffer-local-value 'gptel-backend buf)
                      'fresh-backend))
          (should (eq (buffer-local-value 'gptel-model buf)
                      'fresh-model)))
      (kill-buffer buf))))


;;
;;; Mailbox round-trip

(mevedel-deftest mevedel-session-persistence-messages-roundtrip ()
  ,test
  (test)
  :doc "session :messages mailbox round-trips through serialize/deserialize"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (msg1 '(:type RESULT :sender "/root/explorer"
                       :recipient "/root" :outcome completed
                       :payload "one" :timestamp (12345 67890 0 0)))
               (msg2 '(:type MAIL :sender "/root/reviewer"
                       :recipient "/root" :payload "two"
                       :timestamp (12345 67891 0 0))))
          (setf (mevedel-session-messages session) (list msg1 msg2))
          (let* ((sidecar (mevedel-session-persistence-serialize
                           session
                           :first-user-message nil
                           :additional-roots nil))
                 (restored (plist-get
                            (mevedel-session-persistence-deserialize sidecar)
                            :session))
                 (msgs (mevedel-session-messages restored)))
            (should (equal (length msgs) 2))
            (should (equal (plist-get (car msgs) :sender)
                           "/root/explorer"))
            (should (equal "one" (plist-get (car msgs) :payload)))
            (should (equal (plist-get (cadr msgs) :sender)
                           "/root/reviewer"))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Background-agents-pending reminder

(mevedel-deftest mevedel-reminders-make-background-agents-pending ()
  ,test
  (test)

  :doc "fires while session has running background agents"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (reminder (mevedel-reminders-make-background-agents-pending)))
          (setf (mevedel-session-background-agents session)
                '("explorer--first" "explorer--second"))
          (should (funcall (mevedel-reminder-trigger reminder) session))
          (let ((content (funcall (mevedel-reminder-content reminder)
                                  session)))
            (should (string-match-p "2 background sub-agents still running"
                                    content))
            (should (string-match-p "explorer--first" content))
            (should (string-match-p "explorer--second" content))
            (should (string-match-p "<agent-result" content))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "does not fire when no background agents are pending"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (reminder (mevedel-reminders-make-background-agents-pending)))
          (setf (mevedel-session-background-agents session) nil)
          (should-not (funcall (mevedel-reminder-trigger reminder) session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))




;;
;;; Conditional SendMessage injection for background sub-agents

(mevedel-deftest mevedel-agent-exec--inject-sendmessage ()
  ,test
  (test)

  :doc "appends SendMessage to gptel-tools when present in registry"
  (let* ((buf (generate-new-buffer "*spec21-inject-sm*"))
         (sm-tool (gptel-make-tool
                   :name "SendMessage"
                   :function #'ignore
                   :description "Send a message"
                   :async t
                   :category "mevedel")))
    (unwind-protect
        (progn
          (with-current-buffer buf (setq-local gptel-tools nil))
          (cl-letf (((symbol-function 'gptel-get-tool)
                     (lambda (path)
                       (when (equal path '("mevedel" "SendMessage"))
                         sm-tool))))
            (mevedel-agent-exec--inject-sendmessage buf))
          (with-current-buffer buf
            (should (memq sm-tool gptel-tools))))
      (kill-buffer buf)))

  :doc "is idempotent: does not double-add when already present"
  (let* ((buf (generate-new-buffer "*spec21-inject-sm-idem*"))
         (sm-tool (gptel-make-tool
                   :name "SendMessage"
                   :function #'ignore
                   :description "Send a message"
                   :async t
                   :category "mevedel")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-tools (list sm-tool)))
          (cl-letf (((symbol-function 'gptel-get-tool)
                     (lambda (_path) sm-tool)))
            (mevedel-agent-exec--inject-sendmessage buf))
          (with-current-buffer buf
            (should (equal (length gptel-tools) 1))))
      (kill-buffer buf)))

  :doc "no-op when no SendMessage tool is registered"
  (let ((buf (generate-new-buffer "*spec21-inject-sm-missing*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (setq-local gptel-tools nil))
          (cl-letf (((symbol-function 'gptel-get-tool)
                     (lambda (_path) nil)))
            (mevedel-agent-exec--inject-sendmessage buf))
          (with-current-buffer buf
            (should (null gptel-tools))))
      (kill-buffer buf))))


;;
;;; Dispatch sets background-p on the invocation

(mevedel-deftest mevedel-agent-invocation-background-p-slot ()
  ,test
  (test)
  :doc "background-p is set from the dispatch's background argument"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    ;; Simulate dispatch's setter; the actual dispatch path is too
    ;; coupled to FSM internals to drive directly here.
    (setf (mevedel-agent-invocation-background-p inv) t)
    (should (eq (mevedel-agent-invocation-background-p inv) t))
    (setf (mevedel-agent-invocation-background-p inv) nil)
    (should (null (mevedel-agent-invocation-background-p inv)))))


;;
;;; Background-channels one-shot reminder

(mevedel-deftest mevedel-reminders-make-agent-background-channels ()
  ,test
  (test)

  :doc "fires for direct-main background invocations and lists only main"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent))
         (reminder (mevedel-reminders-make-agent-background-channels)))
    (setf (mevedel-agent-invocation-background-p inv) t)
    (should (funcall (mevedel-reminder-trigger reminder) inv))
    (let ((content (funcall (mevedel-reminder-content reminder) inv)))
      (should (string-match-p "running in the background" content))
      (should (string-match-p "to=\"main\"" content))
      (should-not (string-match-p "to=\"coordinator\"" content))
      (should-not (string-match-p "<agent-id>" content))
      (should (string-match-p "Ask" content))))

  :doc "coordinator-owned worker reminder names the coordinator id"
  (let* ((coord-agent (mevedel-agent--create :name "coordinator"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
         (coord-inv (mevedel-agent-invocation-create coord-agent))
         (coord-buf (generate-new-buffer "*spec21-reminder-coord*"))
         (worker-agent (mevedel-agent--create :name "explorer"
                                              :system-prompt "stub"
                                              :tools nil
                                              :reminders nil))
         (worker-inv (mevedel-agent-invocation-create worker-agent))
         (reminder (mevedel-reminders-make-agent-background-channels)))
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-agent-id coord-inv)
                "coordinator--parent")
          (with-current-buffer coord-buf
            (setq-local mevedel--agent-invocation coord-inv))
          (setf (mevedel-agent-invocation-parent-data-buffer worker-inv)
                coord-buf)
          (setf (mevedel-agent-invocation-background-p worker-inv) t)
          (let ((content (funcall (mevedel-reminder-content reminder)
                                  worker-inv)))
            (should (string-match-p "coordinator--parent" content))
            (should-not (string-match-p "SendMessage(to=\"main\"" content))
            (should (string-match-p "sibling workers" content))))
      (kill-buffer coord-buf)))

  :doc "does not fire for foreground (non-background-p) invocations"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent))
         (reminder (mevedel-reminders-make-agent-background-channels)))
    (setf (mevedel-agent-invocation-background-p inv) nil)
    (should-not (funcall (mevedel-reminder-trigger reminder) inv))))


;;
;;; SendMessage channel matrix

(mevedel-deftest mevedel-tools--resolve-recipient-channel-matrix ()
  ,test
  (test)

  :doc "main and direct main workers can address each other"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (chat-buffer (generate-new-buffer "*spec21-main-worker*"))
               worker worker-buf ov-buf)
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "explorer" "explorer--main" chat-buffer session)))
            (setq worker inv
                  worker-buf buf
                  ov-buf (test-mevedel-spec21--register-agent
                          chat-buffer "explorer--main" worker))
            (should (eq (mevedel-tools--resolve-recipient
                         "explorer--main" chat-buffer)
                        worker))
            (should (eq (mevedel-tools--resolve-recipient
                         "main" worker-buf)
                        session)))
          (test-mevedel-spec21--kill-buffer ov-buf)
          (test-mevedel-spec21--kill-buffer worker-buf)
          (test-mevedel-spec21--kill-buffer chat-buffer))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "coordinator and its own worker can address each other"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (chat-buffer (generate-new-buffer "*spec21-coord-worker*"))
               coord coord-buf worker worker-buf worker-ov)
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "coordinator" "coordinator--parent"
                        chat-buffer session)))
            (setq coord inv
                  coord-buf buf))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "explorer" "explorer--child"
                        coord-buf session)))
            (setq worker inv
                  worker-buf buf
                  worker-ov (test-mevedel-spec21--register-agent
                             coord-buf "explorer--child" worker)))
          (should (eq (mevedel-tools--resolve-recipient
                       "explorer--child" coord-buf)
                      worker))
          (should-not (mevedel-tools--resolve-recipient
                       "explorer" coord-buf))
          (should (eq (mevedel-tools--resolve-recipient
                       "coordinator--parent" worker-buf)
                      coord))
          (should (eq (mevedel-tools--resolve-recipient
                       "coordinator" worker-buf)
                      coord))
          (test-mevedel-spec21--kill-buffer worker-ov)
          (test-mevedel-spec21--kill-buffer worker-buf)
          (test-mevedel-spec21--kill-buffer coord-buf)
          (test-mevedel-spec21--kill-buffer chat-buffer))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "coordinator workers cannot bypass the coordinator to main"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (chat-buffer (generate-new-buffer "*spec21-worker-main-deny*"))
               coord-buf worker-buf)
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (pcase-let ((`(,_coord . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "coordinator" "coordinator--parent"
                        chat-buffer session)))
            (setq coord-buf buf))
          (pcase-let ((`(,_worker . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "explorer" "explorer--child"
                        coord-buf session)))
            (setq worker-buf buf))
          (should-not (mevedel-tools--resolve-recipient
                       "main" worker-buf))
          (test-mevedel-spec21--kill-buffer worker-buf)
          (test-mevedel-spec21--kill-buffer coord-buf)
          (test-mevedel-spec21--kill-buffer chat-buffer))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "main cannot address a coordinator-owned worker directly"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (chat-buffer (generate-new-buffer "*spec21-main-deny-child*"))
               coord-buf worker worker-buf worker-ov)
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (pcase-let ((`(,_coord . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "coordinator" "coordinator--parent"
                        chat-buffer session)))
            (setq coord-buf buf))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "explorer" "explorer--child"
                        coord-buf session)))
            (setq worker inv
                  worker-buf buf
                  worker-ov (test-mevedel-spec21--register-agent
                             coord-buf "explorer--child" worker)))
          (should-not (mevedel-tools--resolve-recipient
                       "explorer--child" chat-buffer))
          (test-mevedel-spec21--kill-buffer worker-ov)
          (test-mevedel-spec21--kill-buffer worker-buf)
          (test-mevedel-spec21--kill-buffer coord-buf)
          (test-mevedel-spec21--kill-buffer chat-buffer))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "sibling workers cannot address each other directly"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (chat-buffer (generate-new-buffer "*spec21-sibling-deny*"))
               coord-buf worker-a-buf worker-b worker-b-buf ov-a ov-b)
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (pcase-let ((`(,_coord . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "coordinator" "coordinator--parent"
                        chat-buffer session)))
            (setq coord-buf buf))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "explorer" "explorer--a"
                        coord-buf session)))
            (setq worker-a-buf buf
                  ov-a (test-mevedel-spec21--register-agent
                        coord-buf "explorer--a" inv)))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "explorer" "explorer--b"
                        coord-buf session)))
            (setq worker-b inv
                  worker-b-buf buf
                  ov-b (test-mevedel-spec21--register-agent
                        coord-buf "explorer--b" worker-b)))
          (should-not (mevedel-tools--resolve-recipient
                       "explorer--b" worker-a-buf))
          (should-not (mevedel-tools--resolve-recipient
                       "explorer" worker-a-buf))
          (test-mevedel-spec21--kill-buffer ov-a)
          (test-mevedel-spec21--kill-buffer ov-b)
          (test-mevedel-spec21--kill-buffer worker-a-buf)
          (test-mevedel-spec21--kill-buffer worker-b-buf)
          (test-mevedel-spec21--kill-buffer coord-buf)
          (test-mevedel-spec21--kill-buffer chat-buffer))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "coordinator alias resolves to the immediate parent, not another coordinator"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (chat-buffer (generate-new-buffer "*spec21-multi-coord*"))
               coord-a coord-a-buf coord-b coord-b-buf worker-buf
               ov-a ov-b)
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-runtime--fsms nil))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "coordinator" "coordinator--a"
                        chat-buffer session)))
            (setq coord-a inv
                  coord-a-buf buf
                  ov-a (test-mevedel-spec21--register-agent
                        chat-buffer "coordinator--a" coord-a)))
          (pcase-let ((`(,inv . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "coordinator" "coordinator--b"
                        chat-buffer session)))
            (setq coord-b inv
                  coord-b-buf buf
                  ov-b (test-mevedel-spec21--register-agent
                        chat-buffer "coordinator--b" coord-b)))
          (pcase-let ((`(,_worker . ,buf)
                       (test-mevedel-spec21--make-agent-buffer
                        "explorer" "explorer--under-a"
                        coord-a-buf session)))
            (setq worker-buf buf))
          ;; `ov-b' was registered after `ov-a', so old registry-scanning
          ;; behavior would have resolved the alias to coord-b.
          (should (eq (mevedel-tools--resolve-recipient
                       "coordinator" worker-buf)
                      coord-a))
          (test-mevedel-spec21--kill-buffer ov-a)
          (test-mevedel-spec21--kill-buffer ov-b)
          (test-mevedel-spec21--kill-buffer worker-buf)
          (test-mevedel-spec21--kill-buffer coord-a-buf)
          (test-mevedel-spec21--kill-buffer coord-b-buf)
          (test-mevedel-spec21--kill-buffer chat-buffer))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Agent registry propagation into sub-agent buffers

(mevedel-deftest mevedel-agent-exec--allocate-agent-buffer-propagates-agent-specs ()
  ,test
  (test)
  :doc "agent buffer carries the parent's mevedel-agent-exec--agents alist"
  ;; Without this, a sub-agent that itself dispatches further
  ;; sub-agents (coordinator -> explorer workers) sees a nil
  ;; registry and the worker's preset spec is never applied:
  ;; workers fire with default tooling and fail.
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer
                            "*spec21-agent-specs-parent*"))
               (specs '(("explorer" :tools (sentinel-tool))
                        ("coordinator" :tools nil)))
               (agent (mevedel-agent--create :name "coordinator"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-agent-exec--agents specs))
          (setf (mevedel-agent-invocation-agent-id inv) "coordinator--abc")
          (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
          (let ((agent-buf (mevedel-agent-exec--allocate-agent-buffer
                            inv parent-buf)))
            (unwind-protect
                (with-current-buffer agent-buf
                  ;; The coordinator's agent buffer must see the same
                  ;; registry the parent saw, so it can resolve
                  ;; `"explorer"' when it dispatches a worker.
                  (should (equal mevedel-agent-exec--agents specs)))
              (test-mevedel-spec21--kill-buffer agent-buf)))
          (kill-buffer parent-buf))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(provide 'test-mevedel-agent-transcript-persistence)
;;; test-mevedel-agent-transcript-persistence.el ends here
