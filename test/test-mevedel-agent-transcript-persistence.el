;;; test-mevedel-agent-transcript-persistence.el --- Spec 21 tests -*- lexical-binding: t -*-

;;; Commentary:

;; Sub-agent transcript persistence (spec 21).  Covers the
;; load-bearing invariants: path validation, sanitization,
;; sidecar round-trip, shallow materialization, session-resume
;; transitions, fork pruning, and the helper surface in
;; `mevedel-agent-exec.el' and `mevedel-tool-ui.el'.
;;
;; FSM-driven dispatch flows (`mevedel-tools--task--dispatch'
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


;;
;;; Path validation

(mevedel-deftest mevedel-session-persistence--validate-transcript-path ()
  ,test
  (test)

  :doc "accepts well-formed relative paths under agents/"
  (let* ((tmp (file-name-as-directory (make-temp-file "spec21-" t))))
    (unwind-protect
        (should (mevedel-session-persistence--validate-transcript-path
                 "agents/explore--2026-04-25T14-18-32--abcd1234.chat.org"
                 tmp))
      (delete-directory tmp t)))

  :doc "rejects absolute paths"
  (let* ((tmp (file-name-as-directory (make-temp-file "spec21-" t))))
    (unwind-protect
        (should-not
         (mevedel-session-persistence--validate-transcript-path
          "/etc/passwd.chat.org" tmp))
      (delete-directory tmp t)))

  :doc "rejects paths with .. segments"
  (let* ((tmp (file-name-as-directory (make-temp-file "spec21-" t))))
    (unwind-protect
        (should-not
         (mevedel-session-persistence--validate-transcript-path
          "agents/../escape.chat.org" tmp))
      (delete-directory tmp t)))

  :doc "rejects non-.chat.org suffix"
  (let* ((tmp (file-name-as-directory (make-temp-file "spec21-" t))))
    (unwind-protect
        (should-not
         (mevedel-session-persistence--validate-transcript-path
          "agents/transcript.txt" tmp))
      (delete-directory tmp t)))

  :doc "rejects paths that resolve outside agents/"
  (let* ((tmp (file-name-as-directory (make-temp-file "spec21-" t))))
    (unwind-protect
        (should-not
         (mevedel-session-persistence--validate-transcript-path
          "outside/file.chat.org" tmp))
      (delete-directory tmp t)))

  :doc "rejects nil and empty"
  (should-not (mevedel-session-persistence--validate-transcript-path nil "/tmp/"))
  (should-not (mevedel-session-persistence--validate-transcript-path "" "/tmp/")))


;;
;;; Sidecar sanitization

(mevedel-deftest mevedel-session-persistence--sanitize-agent-transcripts ()
  ,test
  (test)

  :doc "passes through valid entries"
  (let* ((entry '("explore--abc"
                  :agent-type "explore" :description "test"
                  :path "agents/x.chat.org" :status running
                  :created-at "2026-04-25T14-00-00"
                  :updated-at "2026-04-25T14-00-00"
                  :parent-turn 1))
         (sanitized
          (mevedel-session-persistence--sanitize-agent-transcripts
           (list entry))))
    (should (equal (length sanitized) 1))
    (should (equal (caar sanitized) "explore--abc"))
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
               (entry '("explore--rt"
                        :agent-type "explore"
                        :description "round trip"
                        :path "agents/explore--rt.chat.org"
                        :status completed
                        :created-at "2026-04-25T14-00-00"
                        :updated-at "2026-04-25T14-00-30"
                        :parent-turn 3)))
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "explore--rt" (cdr entry))))
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
            (should (equal (caar transcripts) "explore--rt"))
            (should (eq (plist-get (cdar transcripts) :status) 'completed))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Old sidecar (forward compatibility)

(mevedel-deftest mevedel-session-persistence-agent-transcripts-old-sidecar ()
  ,test
  (test)
  :doc "missing :agent-transcripts deserializes to nil"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((sidecar
                `(:version ,(mevedel-version)
                  :session-id "old-session"
                  :session-name "main"
                  :workspace
                  ,(mevedel-session-persistence--workspace-to-plist
                    workspace)
                  :tasks nil
                  :total-turn-count 0
                  :current-segment 1))
               (result (mevedel-session-persistence-deserialize sidecar))
               (session (plist-get result :session)))
          (should (null (mevedel-session-agent-transcripts session))))
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
      (mevedel-workspace-clear-registry)))

  :doc "returns nil when persistence is disabled"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let ((mevedel-session-persistence nil)
              (session (mevedel-session-create "main" workspace))
              (buf (generate-new-buffer "*spec21-shallow-disabled*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (should-not
                 (mevedel-session-persistence--shallow-ensure-files
                  session buf)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Agent invocation persistence slots

(mevedel-deftest mevedel-agent-invocation-persistence-slots ()
  ,test
  (test)
  :doc "invocation struct has spec 21 persistence slots"
  (let* ((agent (mevedel-agent--create :name "explore"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explore--xyz")
    (setf (mevedel-agent-invocation-description inv) "test")
    (setf (mevedel-agent-invocation-parent-turn inv) 4)
    (setf (mevedel-agent-invocation-transcript-status inv) 'running)
    (setf (mevedel-agent-invocation-transcript-relative-path inv)
          "agents/explore--xyz.chat.org")
    (should (equal (mevedel-agent-invocation-agent-id inv) "explore--xyz"))
    (should (equal (mevedel-agent-invocation-description inv) "test"))
    (should (equal (mevedel-agent-invocation-parent-turn inv) 4))
    (should (eq (mevedel-agent-invocation-transcript-status inv) 'running))
    (should (equal (mevedel-agent-invocation-transcript-relative-path inv)
                   "agents/explore--xyz.chat.org"))))


;;
;;; Agent buffer allocation

(mevedel-deftest mevedel-agent-exec--allocate-agent-buffer ()
  ,test
  (test)

  :doc "buffer is org-mode and carries parent context bindings"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-parent*"))
               (agent (mevedel-agent--create :name "explore"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explore--abcd1234")
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (let ((agent-buf
                 (mevedel-agent-exec--allocate-agent-buffer
                  inv parent-buf)))
            (unwind-protect
                (with-current-buffer agent-buf
                  (should (derived-mode-p 'org-mode))
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

(mevedel-deftest mevedel-tools--agent-result-format ()
  ,test
  (test)

  :doc "round-trips agent-id through format -> parse"
  (let ((s (mevedel-tools--agent-result-format
            "explore--abc" "explore" "test" "body content")))
    (should (equal (mevedel-tools--agent-result-parse-id s)
                   "explore--abc")))

  :doc "XML-escapes embedded quotes in description"
  (let ((s (mevedel-tools--agent-result-format
            "x--1" "x" "He said \"hi\"" "body")))
    (should (string-match-p "&quot;hi&quot;" s))
    (should-not (string-match-p
                 "description=\"He said \"hi\"\"" s)))

  :doc "XML-escapes ampersands"
  (let ((s (mevedel-tools--agent-result-format
            "x--1" "x" "A&B" "body")))
    (should (string-match-p "A&amp;B" s))))


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
               (agent (mevedel-agent--create :name "explore"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explore--A")
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
               (agent (mevedel-agent--create :name "explore"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explore--B")
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
               (agent (mevedel-agent--create :name "explore"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explore--ws")
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
  (let* ((agent (mevedel-agent--create :name "explore"
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
  (let* ((agent (mevedel-agent--create :name "explore"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent))
         (buf (generate-new-buffer "*spec21-save-dead*")))
    (setf (mevedel-agent-invocation-buffer inv) buf)
    (kill-buffer buf)
    (should-not (mevedel-agent-exec--save-transcript-buffer inv))))


;;
;;; Insert injected prompt

(mevedel-deftest mevedel-agent-exec--insert-injected-prompt ()
  ,test
  (test)
  :doc "appends BLOCK to the agent buffer at point-max"
  (let* ((agent (mevedel-agent--create :name "explore"
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
      (kill-buffer buf))))


;;
;;; Finalize sets terminal status

(mevedel-deftest mevedel-agent-exec--finalize ()
  ,test
  (test)

  :doc "marks status terminal and is idempotent"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-spec21--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (agent (mevedel-agent--create :name "explore"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explore--fin")
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-transcript-status inv) 'running)
          ;; Seed the session slot so finalize has somewhere to update.
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "explore--fin"
                            (list :status 'running
                                  :path "agents/explore--fin.chat.org"
                                  :parent-turn 1))))
          (mevedel-agent-exec--finalize inv 'completed)
          (should (eq (mevedel-agent-invocation-transcript-status inv)
                      'completed))
          (should (eq (plist-get (cdr (assoc "explore--fin"
                                             (mevedel-session-agent-transcripts
                                              session)))
                                 :status)
                      'completed))
          ;; Idempotent: a second call with a different status doesn't
          ;; flip the terminal one.
          (mevedel-agent-exec--finalize inv 'aborted)
          (should (eq (mevedel-agent-invocation-transcript-status inv)
                      'completed)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Foreground response wrapping with render-data

(mevedel-deftest mevedel-tools--task--wrap-foreground-response ()
  ,test
  (test)

  :doc "wraps with render-data when transcript path is set"
  (let* ((agent (mevedel-agent--create :name "explore"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explore--frw")
    (setf (mevedel-agent-invocation-transcript-relative-path inv)
          "agents/explore--frw.chat.org")
    (setf (mevedel-agent-invocation-transcript-status inv) 'completed)
    (let ((result
           (mevedel-tools--task--wrap-foreground-response
            "the response text" inv)))
      (should (consp result))
      (should (equal (plist-get result :result) "the response text"))
      (let ((rd (plist-get result :render-data)))
        (should (eq (plist-get rd :kind) 'agent-transcript))
        (should (equal (plist-get rd :agent-id) "explore--frw"))
        (should (equal (plist-get rd :transcript-relative-path)
                       "agents/explore--frw.chat.org")))))

  :doc "passes through when no transcript path"
  (let* ((agent (mevedel-agent--create :name "explore"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explore--nopath")
    (let ((result (mevedel-tools--task--wrap-foreground-response
                   "raw" inv)))
      (should (equal result "raw"))))

  :doc "passes through non-string responses unchanged"
  (let* ((agent (mevedel-agent--create :name "explore"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explore--ns")
    (setf (mevedel-agent-invocation-transcript-relative-path inv)
          "agents/explore--ns.chat.org")
    (let ((result (mevedel-tools--task--wrap-foreground-response
                   nil inv)))
      (should (eq result nil)))))


;;
;;; Render-agent renderer consumes render-data

(mevedel-deftest mevedel-tool-ui--render-agent-render-data ()
  ,test
  (test)

  :doc "header includes [transcript: STATUS] suffix when render-data present"
  (let* ((args '(:subagent_type "explore" :description "test"))
         (rd '(:kind agent-transcript :agent-id "explore--rd"
               :status running)))
    (let ((rendering (mevedel-tool-ui--render-agent
                      "Agent" args "result body" rd)))
      (should (string-match-p "\\[transcript: running\\]"
                              (plist-get rendering :header)))))

  :doc "header omits suffix when render-data absent"
  (let* ((args '(:subagent_type "explore" :description "test")))
    (let ((rendering (mevedel-tool-ui--render-agent
                      "Agent" args "result body" nil)))
      (should-not (string-match-p "\\[transcript:"
                                  (plist-get rendering :header))))))


;;
;;; LLM-facing string never carries transcript=

(mevedel-deftest mevedel-tools--agent-result-no-transcript-attr ()
  ,test
  (test)
  :doc "format helper never emits a transcript= attribute"
  (let ((s (mevedel-tools--agent-result-format
            "explore--llm" "explore" "desc" "body")))
    (should-not (string-match-p "transcript=" s))))


(provide 'test-mevedel-agent-transcript-persistence)
;;; test-mevedel-agent-transcript-persistence.el ends here
