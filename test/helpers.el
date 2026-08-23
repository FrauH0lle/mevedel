;;; tests/helpers.el -- Helper functions for tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

;; The shared fixture macros below let-bind these product variables, and a
;; macro expands in its consumer, so their owners must be loaded here rather
;; than in every consumer.
(require 'mevedel-permission-mode)
(require 'mevedel-plugin-registry)
(require 'mevedel-structs)

;; `gptel'
(declare-function gptel-make-openai "gptel")
(defvar gptel-backend)
(defvar gptel-model)

;; `mevedel-execution-process'
(defvar mevedel-execution-process--child-kill-delay)

;; `mevedel-permission-log'
(declare-function mevedel-permission-log-path "mevedel-permission-log"
                  (session))

;; `mevedel-transcript-audit'
(declare-function mevedel--read-hook-audit-record "mevedel-transcript-audit"
                  (text))

;; `mevedel-utilities'
(defvar mevedel--hook-audit-close)
(defvar mevedel--hook-audit-open)

;; `tabulated-list'
(defvar tabulated-list-entries)

;; `tramp'
(defvar tramp-histfile-override)
(defvar tramp-local-host-regexp)
(defvar tramp-methods)

(unless (cl-every
         (lambda (directory)
           (and directory
                (file-in-directory-p directory temporary-file-directory)))
         (cons (expand-file-name "~")
               (mapcar #'getenv
                       '("XDG_CACHE_HOME" "XDG_CONFIG_HOME"
                         "XDG_DATA_HOME" "XDG_STATE_HOME"))))
  (error "Tests require HOME and XDG roots under temporary-file-directory"))

;; gptel ships no default backend, and a session cannot be saved without one.
;; Tests that create chat buffers would otherwise warn on every teardown save
;; instead of exercising the persistence path they mean to cover.
(with-eval-after-load 'gptel
  (unless (default-value 'gptel-backend)
    (setq-default gptel-backend
                  (gptel-make-openai "mevedel-test"
                                     :key "test"
                                     :models '(mevedel-test-model)))
    (setq-default gptel-model 'mevedel-test-model)))

(defconst mevedel-test--muted-message-regexps
  '("\\`gptel chat restored\\.\\'"
    "\\`Type q to \\(?:restore previous buffer\\|delete help window\\)"
    ;; gptel reports an unconfigured backend on every send path a test
    ;; exercises without one.  The suite's default backend covers the cases
    ;; that need one; the rest are testing something else entirely.
    "\\`Could not activate gptel backend "
    ;; TRAMP narrates its own connection attempts, including the ones a
    ;; mock-method test means to fail.
    "\\`Tramp: "
    "Host name .* does not match "
    "\\`File error: Tramp failed to connect"
    "tramp-cleanup-this-connection"
    ;; Emacs commands a test drives directly.
    "\\`History search: "
    "\\`Mark set\\'"
    "\\`Copied\\'"
    "buffer-local while locally let-bound!"
    "\\`Cannot remove lock file ")
  "Third-party progress messages the run log must not carry.

Each entry names a message mevedel does not emit and cannot suppress at
its source.  Nothing mevedel itself reports belongs here: its messages
either signal a defect worth seeing or belong to a test that captures
them.")

(defun mevedel-test--mute-third-party-message (original &optional format &rest args)
  "Call ORIGINAL with FORMAT and ARGS unless the result is muted.
A muted call returns the text it would have shown, as `message' does."
  (let ((text (and format (apply #'format-message format args))))
    (if (and text
             (seq-some (lambda (regexp) (string-match-p regexp text))
                       mevedel-test--muted-message-regexps))
        text
      (apply original format args))))

(defun mevedel-test--permission-log-entries (session)
  "Read permission log entries for SESSION."
  (let ((file (mevedel-permission-log-path session))
        entries)
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (push (read (current-buffer)) entries))
          (end-of-file nil))))
    (nreverse entries)))

(defun mevedel-test--hook-audit-records (text)
  "Return hook audit records parsed from TEXT."
  (let (records)
    (with-temp-buffer
      (insert (or text ""))
      (goto-char (point-min))
      (while (search-forward mevedel--hook-audit-open nil t)
        (let ((record-start (point)))
          (when (search-forward mevedel--hook-audit-close nil t)
            (when-let* ((record
                         (mevedel--read-hook-audit-record
                          (buffer-substring-no-properties
                           record-start (match-beginning 0)))))
              (push record records))))))
    (nreverse records)))

(defun mevedel-test--drop-sessionless-permission-warning
    (original type message &rest args)
  "Call ORIGINAL unless MESSAGE is the sessionless permission warning.
TYPE and ARGS are passed through unchanged."
  (unless (and (eq type 'mevedel)
               (string-match-p "no session in context"
                               (format "%s" message)))
    (apply original type message args)))

(advice-add 'message :around #'mevedel-test--mute-third-party-message)


;; `mevedel-test--worktree-root' is fixed from this helper's location so a
;; test that temporarily binds `default-directory' cannot redirect the guard.
(defconst mevedel-test--worktree-root
  (file-name-as-directory
   (expand-file-name
    ".."
    (file-name-directory
     (or load-file-name buffer-file-name default-directory))))
  "Worktree root whose portable control artifacts tests must not mutate.")

(defvar mevedel-test--timestamp-offset 0
  "Seconds added to every timestamp mevedel formats inside a shifted clock.

A case that needs two durable saves to carry different whole-second stamps
sets this instead of sleeping past a second boundary.")

(defvar mevedel-test--format-time-string nil
  "The real `format-time-string' while a shifted clock is installed.")

(defmacro mevedel-test--with-shifted-clock (&rest body)
  "Run BODY with formatted timestamps shifted by the test's own offset.

Durable saves stamp `updated-at' and derive session ids at whole-second
resolution, so a case proving two saves differ used to sleep for over a
second.  Setting `mevedel-test--timestamp-offset' advances the clock those
stamps see, which is both instant and deterministic: the case no longer
depends on when in a second it happened to run."
  (declare (indent 0) (debug t))
  `(let ((mevedel-test--timestamp-offset 0)
         (mevedel-test--format-time-string
          (symbol-function 'format-time-string)))
     (cl-letf (((symbol-function 'format-time-string)
                (lambda (format &optional time &rest arguments)
                  (apply mevedel-test--format-time-string
                         format
                         (or time
                             (time-add (current-time)
                                       mevedel-test--timestamp-offset))
                         arguments))))
       ,@body)))

(defmacro mevedel-test--with-captured-diagnostics (place &rest body)
  "Run BODY collecting its messages and warnings into PLACE.

A test that deliberately injects a failure owns the diagnostic it provokes:
letting it reach the run log buries real failures in expected noise.  PLACE
is set to the collected text so the test can assert on it, or nil when the
test asserts the durable state the diagnostic merely echoes."
  (declare (indent 1) (debug (form body)))
  `(let ((mevedel-test--captured ""))
     (cl-letf* ((original-message (symbol-function 'message))
                ((symbol-function 'message)
                 (lambda (&optional format &rest args)
                   (when format
                     (setq mevedel-test--captured
                           (concat mevedel-test--captured
                                   (apply #'format format args) "\n")))
                   nil))
                ((symbol-function 'display-warning)
                 (lambda (_type warning &rest _)
                   (setq mevedel-test--captured
                         (concat mevedel-test--captured
                                 (format "%s" warning) "\n"))
                   nil)))
       (ignore original-message)
       (unwind-protect (progn ,@body)
         ,@(when place `((setq ,place mevedel-test--captured)))))))

(defmacro mevedel-test--with-captured-messages (place &rest body)
  "Run BODY collecting its messages into PLACE, leaving warnings alone.
Use this when BODY reports progress the run log does not need but a test
still inspects the warnings it raises."
  (declare (indent 1) (debug (form body)))
  `(let ((mevedel-test--captured ""))
     (cl-letf (((symbol-function 'message)
                (lambda (&optional format &rest args)
                  (when format
                    (setq mevedel-test--captured
                          (concat mevedel-test--captured
                                  (apply #'format format args) "\n")))
                  nil)))
       (unwind-protect (progn ,@body)
         ,@(when place `((setq ,place mevedel-test--captured)))))))

(defun mevedel-test--worktree-control-snapshot ()
  "Return content/shape snapshots of worktree-root portable controls.

The baseline may contain an artifact left by an earlier interrupted run; the
guard detects only changes made by the current test invocation."
  (mapcar
   (lambda (name)
     (let ((path (file-name-concat mevedel-test--worktree-root name)))
       (cons
        name
        (cond
         ((not (file-exists-p path)) nil)
         ((file-directory-p path)
          (cons
           'directory
           (mapcar
            (lambda (entry)
              (list
               (file-relative-name entry path)
               (cond
                ((file-directory-p entry) 'directory)
                ((file-regular-p entry)
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally entry)
                   (secure-hash 'sha256 (current-buffer))))
                ((file-symlink-p entry) 'symlink)
                (t 'other))))
            (sort (directory-files-recursively path ".*" t)
                  #'string<))))
         ((file-regular-p path)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (list 'file (secure-hash 'sha256 (current-buffer)))))
         ((file-symlink-p path) '(symlink))
         (t '(other))))))
   '(".lease" ".recovery" "agents" "plans" "file-history" "instructions"
     "session.meta.el")))

(defvar mevedel-agent--registry)

(defvar mevedel-test--builtin-agent-registry 'unset
  "The agent registry as the loaded modules built it.
Captured lazily, before the first test mutates it.")

(defun mevedel-test--restore-agent-registry ()
  "Restore `mevedel-agent--registry' to its as-loaded contents.
The cleanup for a test that registers agents: wiping the registry
instead would destroy the built-in agents later suites depend on."
  (when (eq mevedel-test--builtin-agent-registry 'unset)
    (error "Capture the agent registry before restoring it"))
  (setq mevedel-agent--registry
        (copy-sequence mevedel-test--builtin-agent-registry)))

(defun mevedel-test--capture-agent-registry ()
  "Capture the built-in agent registry once, before a test mutates it."
  (when (eq mevedel-test--builtin-agent-registry 'unset)
    (setq mevedel-test--builtin-agent-registry
          (copy-sequence mevedel-agent--registry))))

(defun mevedel-test--cancel-stray-lease-timers ()
  "Cancel portable lease renewal and deferred transport timers a test left.

A surviving timer performs target I/O from wherever the main loop is waiting
during later tests, which floods their output and can wedge a shared TRAMP
connection."
  (dolist (timer (append timer-list timer-idle-list))
    (when (or (memq (timer--function timer)
                    '(mevedel-session-durability-lease-renew
                      mevedel-transport-run-when-idle
                      mevedel-transport--retry))
              ;; A view poll timer whose buffer died through a hookless
              ;; kill is a stray wakeup holding a dead view.
              (and (eq (timer--function timer)
                       'mevedel-view--control-transfer-refresh)
                   (let ((view (car (timer--args timer))))
                     (and (bufferp view) (not (buffer-live-p view))))))
      (cancel-timer timer)))
  (when (fboundp 'mevedel-transport-cancel-pending)
    (mevedel-transport-cancel-pending)))

(defvar mevedel-test--release-leaked-state-p t
  "Whether each test releases global session and workspace state.
Bound to nil only to measure what that release costs.")

(defun mevedel-test--release-leaked-state ()
  "Drop live session and workspace state a test left registered.

Registered sessions and workspaces are process-global.  A test that leaves
them behind makes every later test carry that state: execution bookkeeping
walks the session registry, and workspace detection re-resolves roots whose
target is gone, which is slow, noisy, and order dependent."
  (when (boundp 'mevedel-execution--sessions)
    ;; Settling state a test left behind reports on that state; the report
    ;; belongs to the test that leaked it, not to the run log.
    (mevedel-test--with-captured-diagnostics nil
      (let ((mevedel-execution-process--child-kill-delay 0.05))
        (ignore-errors (mevedel-execution-teardown-all))))
    (clrhash mevedel-execution--sessions))
  (when (fboundp 'mevedel-workspace-clear-registry)
    (ignore-errors (mevedel-workspace-clear-registry)))
  ;; Generated-state exclusion is remembered per root for the process, so a
  ;; test that reuses a root would otherwise inherit the previous answer.
  (when (boundp 'mevedel-workspace--generated-state-ignored)
    (clrhash mevedel-workspace--generated-state-ignored)))

(defun mevedel-test--assert-worktree-controls-unchanged (before)
  "Signal when a test changes worktree-root session artifacts.
BEFORE is the snapshot captured before test setup; an existing artifact is
allowed, but creation, deletion, or mutation during the test is not.  The
watched names cover session control state and the session artifacts a
fixture writes when it derives paths from an unset save path."
  (let ((after (mevedel-test--worktree-control-snapshot)))
    (unless (equal before after)
      (error "Test changed worktree-root portable controls: %S -> %S"
             before after))))


;;
;;; TRAMP test helper

(defun mevedel-test--ensure-mock-tramp-method ()
  "Register the local-shell TRAMP method the tests address as remote.

The definition is inert data, and registering it once per process keeps a
test that addresses a `mevedelmock' path from depending on an earlier test
having registered it."
  (require 'tramp)
  (unless (assoc "mevedelmock" tramp-methods)
    (add-to-list
     'tramp-methods
     '("mevedelmock"
       (tramp-login-program "sh")
       (tramp-login-args (("-i")))
       (tramp-remote-shell "/bin/sh")
       (tramp-remote-shell-args ("-c"))
       (tramp-connection-timeout 10)))))

(mevedel-test--ensure-mock-tramp-method)

(defmacro mevedel-test--with-local-shell-tramp (hosts &rest body)
  "Run BODY through a local-shell TRAMP method for HOSTS.

The TERM-to-KILL grace runs at its production value: settlement is
zombie-aware and probes at the main exit, so a stop whose TERM worked
settles at the sentinel instead of riding the grace timers."
  (declare (indent 1) (debug (form body)))
  `(progn
     (mevedel-test--ensure-mock-tramp-method)
     (let ((tramp-local-host-regexp
            (concat "\\`"
                    (regexp-opt (append ,hosts (list (system-name))))
                    "\\'"))
           ;; A real login sets HOME on the remote side.  This method reuses a
           ;; local shell, so it inherits the blanked environment mevedel gives
           ;; a remote child, and TRAMP's history-file probe then asks a shell
           ;; with no HOME to `cd ~/'.  Bourne shells that decline to fall back
           ;; to the password entry -- dash, which is /bin/sh on Debian and
           ;; Ubuntu -- fail that probe and take every mock connection with
           ;; them.  No history file, no probe.
           (tramp-histfile-override t))
       (let ((original-support-tier
              (symbol-function 'mevedel-execution-target--support-tier)))
         (cl-letf (((symbol-function 'mevedel-execution-target--support-tier)
                    (lambda (method hop)
                      (if (eq method 'mevedelmock)
                          'supported
                        (funcall original-support-tier method hop)))))
           (unwind-protect
               (progn ,@body)
             (tramp-cleanup-all-connections)))))))


;;
;;; Skill test helpers

(defmacro mevedel-skills-test--with-model-backends (&rest body)
  "Run BODY with an isolated pair of gptel model backends."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     ,@body))

(defun mevedel-skills-test--make-session (&optional name)
  "Return a throwaway session named NAME with a minimal workspace."
  (let ((ws (mevedel-workspace--create
             ;; A real workspace category: durable authority resolution
             ;; refuses to guess one for a synthetic type.
             :type 'file :id "t" :root "/tmp/t" :name (or name "t")
             :file-cache (mevedel-file-cache--create
                          :table (make-hash-table :test #'equal)
                          :order nil :total-bytes 0))))
    (mevedel-session-create (or name "main") ws)))

(defmacro mevedel-skills-test--with-chat-buffer (session &rest body)
  "Run BODY in a temp buffer that mimics a mevedel chat buffer.
SESSION is bound to buffer-local `mevedel--session', and
`gptel-prompt-prefix-alist' is extended so the buffer's major mode
maps to \"### \"."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((gptel-prompt-prefix-alist
            (cons (cons major-mode "### ")
                  gptel-prompt-prefix-alist)))
       (setq mevedel--session ,session)
       ,@body)))

(defun mevedel-skills-test--write-skill (dir name frontmatter &optional body)
  "Create DIR/NAME/SKILL.md with FRONTMATTER and optional BODY."
  (let* ((skill-dir (file-name-as-directory (file-name-concat dir name)))
         (skill-file (file-name-concat skill-dir "SKILL.md")))
    (make-directory skill-dir t)
    (with-temp-file skill-file
      (insert "---\n")
      (insert frontmatter)
      (unless (string-suffix-p "\n" frontmatter)
        (insert "\n"))
      (insert "---\n")
      (when body
        (insert body)))
    skill-file))

(defun mevedel-skills-test--write-plugin-manifest (_user-dir repo json)
  "Create a test plugin manifest JSON for REPO in the install directory.
Return the plugin root directory."
  (let ((root (file-name-concat mevedel-plugin-install-directory repo)))
    (make-directory (file-name-concat root ".codex-plugin") t)
    (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
      (insert json))
    root))

(defun mevedel-skills-test--hook-fn (_event)
  "Test hook used by skill hook normalization."
  '(:additional-context "skill hook ran"))

(defun mevedel-skills-test--make-workspace (root)
  "Return a minimal workspace struct rooted at ROOT."
  (mevedel-workspace--create
   :type 'file :id root :root root :name "test"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defun mevedel-skills-test--stateful-skill (&rest slots)
  "Create a file-backed test skill from SLOTS."
  (unless (plist-member slots :source-file)
    (setq slots
          (plist-put slots :source-file
                     (format "/tmp/mevedel-test-skills/%s/SKILL.md"
                             (or (plist-get slots :name) "unnamed")))))
  (apply #'mevedel-skill--create slots))

(defun mevedel-skills-test--reset-watchers ()
  "Clear the skill modification-detection global registries."
  (maphash (lambda (_dir desc)
             (ignore-errors (file-notify-rm-watch desc)))
           mevedel-skills--watchers)
  (clrhash mevedel-skills--watchers)
  (clrhash mevedel-skills--remote-watch-states)
  (clrhash mevedel-skills--dir-buffers)
  (clrhash mevedel-skills--dirty-buffers)
  (clrhash mevedel-skills--mtime-cache))

(defun mevedel-test-file-cache-create ()
  "Return an empty file cache for tests."
  (mevedel-file-cache--create
   :table (make-hash-table :test #'equal)
   :order nil
   :total-bytes 0))


;;
;;; View test helpers

(defun mevedel-view-test--dry-run-request-data ()
  "Return current gptel request data after normal prompt transforms."
  (let ((fsm
         (gptel-request
           nil
           :buffer (current-buffer)
           :dry-run t
           :transforms
           (cons #'mevedel-view--transform-model-input
                 (remove #'mevedel-view--transform-model-input
                         gptel-prompt-transform-functions)))))
    (format "%S" (plist-get (gptel-fsm-info fsm) :data))))

(defun mevedel-view-test--insert-composer-draft (draft &optional point-offset)
  "Insert DRAFT into the editable composer and move point by POINT-OFFSET."
  (let ((start (mevedel-view--input-start))
        (inhibit-read-only t))
    (goto-char start)
    (insert draft)
    (remove-text-properties
     start (point)
     '(read-only nil
       mevedel-view-prompt nil
       font-lock-face nil
       face nil
       front-sticky nil
       rear-nonsticky nil))
    (goto-char (+ start (or point-offset (length draft))))))

(defun mevedel-view-test--abort-interactions (data-buffer)
  "Abort DATA-BUFFER's queued interactions without saving its session.
A fixture session has no durable home, so the production abort's save would
only report a failure the test never asked for."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (fboundp 'mevedel-permission-queue-abort-all)
        (mevedel-permission-queue-abort-all))
      (when (fboundp 'mevedel-plan-approval-abort)
        (mevedel-plan-approval-abort)))))

(defmacro mevedel-view-test--with-buffers (&rest body)
  "Execute BODY with data and view buffers bound and initialized."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *test-data*"))
         (view-buf (generate-new-buffer " *test-view*"))
         (mevedel-permission-mode 'ask)
         (mevedel-user-dir (file-name-as-directory
                            (make-temp-file "mevedel-view-user-" t)))
         (mevedel-plugin-extra-roots nil))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode)
             (setq-local gptel-response-separator "\n\n")
             (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
             ;; These buffers own no root lifecycle, so teardown drains the
             ;; queues a test may assert on without running the session save
             ;; that a fixture session cannot complete.
             (setq-local mevedel-view--abort-function
                         #'mevedel-view-test--abort-interactions))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t)))))

(defun mevedel-view-test--insert-data (data-buf text props)
  "Insert TEXT into DATA-BUF with gptel property value PROPS."
  (with-current-buffer data-buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert text)
      (when props
        (put-text-property start (point) 'gptel props)))))

(defun mevedel-view-test--count-substring (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0)
        position)
    (while (setq position (string-search needle text start))
      (cl-incf count)
      (setq start (+ position (length needle))))
    count))


;;
;;; Test macro

;; Adapted from https://github.com/radian-software/straight.el

(defmacro mevedel-test--template (template &optional vars &rest bindings)
  "Generate multiple test templates.

TEMPLATE is an implicitly backquoted form that serves as the base
structure for the generated tests.  It can contain placeholders
that will be filled with values from the bindings.

VARS is an optional list of symbols that define the destructuring
pattern for BINDINGS.  Each symbol in VARS will be bound to
corresponding values from BINDINGS.

BINDINGS is a list of values that will be used to fill the
template.  The number of bindings must be evenly divisible by the
number of VARS.  BINDINGS can include :doc keywords followed by
documentation strings for individual test cases.

The macro returns a list of filled templates, with each template
having its placeholders replaced by the corresponding values from
BINDINGS.  Each template is paired with its documentation string.

Implementation Details:
- If no VARS or BINDINGS are provided, returns just the TEMPLATE
- Normalizes BINDINGS to ensure each test case has a docstring
- Validates that BINDINGS match VARS structure
- Processes BINDINGS into environment variables
- Generates templates with proper variable bindings

Example:
  (mevedel-test--template
    (should (equal ,input ,expected))
  (input expected)
  1 1
  :doc \"First docstring\"
  2 4
  :doc \"Second docstring\"
  3 9)

This would generate:
  ((\"\"
  (should
   (equal 1 1)))
 (\"First docstring\"
  (should
   (equal 2 4)))
 (\"Second docstring\"
  (should
   (equal 3 9))))"
  (declare (indent 1) (debug t))
  ;; If no vars or bindings provided, return just the template
  (if (or (null vars) (null bindings))
      (list `("" ,template))
    ;; Ensure that each binding is preceeded by :doc "DOCSTRING" (or empty) and
    ;; assing them to docstrings and bindings
    (let ((normalized-bindings (mevedel-test--normalize-cases vars bindings))
          docstrings bindings)
      (while normalized-bindings
        (let ((item (pop normalized-bindings)))
          (if (and (keywordp item) (eq item :doc))
              (push (pop normalized-bindings) docstrings)
            (push item bindings))))
      ;; Restore order
      (setq bindings (nreverse bindings)
            docstrings (nreverse docstrings))
      ;; Check if bindings are evenly divisible by number of vars
      (let ((unbound (mod (length bindings) (length vars))))
        ;; Error if bindings don't match vars
        (unless (zerop unbound)
          (error "Uneven binding list: %S" (last bindings unbound)))
        ;; Process the bindings
        (let ((body nil)
              (bindings
               (eval
                `(cl-loop for ,vars on ',bindings
                          by (lambda (l) (nthcdr ,(length vars) l))
                          collect
                          (apply #'append
                                 (cl-mapcar #'list ',vars (list ,@vars)))))))
          ;; Iterate through bindings and generate templates
          (let ((cases (dolist (env bindings (mapcar (lambda (it) (eval it t))
                                                     (nreverse body)))
                         ;; Check if environment has even number of elements
                         (let ((even (mod (length env) 2)))
                           (unless even (error "Uneven binding list: %S" env)))
                         ;; Build the let bindings
                         (let (e)
                           (cl-loop for (var val) on env by #'cddr
                                    do (push (list var `(quote ,val)) e))
                           ;; Generate the template with bindings
                           (push `(let* ,(nreverse e) (backquote ,template)) body)))))
            ;; Combine dostrings and test cases
            (cl-loop for case in cases
                     for doc in docstrings
                     collect `(,doc . ,(list case)))))))))

(defun mevedel-test--normalize-cases (vars bindings)
  "Ensure each test case group has a docstring.
VARS is the list of variable names.  BINDINGS is the list of test
inputs/outputs and optional docstrings."
  (let ((i 0)
        item
        result)
    (while bindings
      (setq item (pop bindings))
      ;; Test if the current item is at the start of a test case (number of vars +
      ;; :doc + docstring)
      (if (zerop (mod i (+ (length vars) 2)))
          ;; Test if the current item is the keyword :doc
          (if (and (keywordp item) (eq item :doc))
              ;; Add current and next item to result
              (let ((next (pop bindings)))
                (push item result)
                (push next result)
                ;; increment index +1
                (cl-incf i))
            ;; Else add :doc keyword
            (push :doc result)
            ;; Add empty docstring
            (push "" result)
            ;; Add current item
            (push item result)
            ;; Increment index by 2
            (cl-incf i 2))
        (push item result))
      (cl-incf i))
    (nreverse result)))

(cl-defmacro mevedel-deftest (object
                            (&key before-each after-each expected-result
                                  doc tags vars vars* quiet
                                  &allow-other-keys)
                            &rest template)
  "Define one or more ERT test cases for OBJECT with TEMPLATE.

OBJECT is the symbol being tested.  It can be a function, macro,
or other symbol.

KEYWORD ARGUMENTS:
  :before-each - Form(s) to run before each test case
  :after-each  - Form(s) to run after each test case
  :quiet       - Capture the messages and warnings every case provokes
  :expected-result - Expected result type (:passed, :failed, etc)
  :doc         - Documentation string for the test
  :tags        - List of tags to apply to the test
  :vars        - Variables to bind using `let'
  :vars*       - Variables to bind using `let*'

The `let'/`let*' binding introduced via :vars and :vars* will
encompass the whole test body, including the code from
:before-each and :after-each.

:quiet declares that the function under test reports to the echo area or
raises a warning on the paths these cases take, and that the cases assert
the durable state those diagnostics echo rather than the text.  It wraps
each case in `mevedel-test--with-captured-diagnostics', which is the same
capture a single call site would use, at the granularity of one tested
function.  Reach for it when a case cannot avoid provoking correct
product output; prefer asserting the text with an explicit capture when
the diagnostic itself is the behaviour under test.

TEMPLATE:
TEMPLATE is a list of forms that will be expanded into test cases
using `mevedel-test--template'.  There are several patterns:

1. SIMPLE TEST (no variables):
   Just provide a test body.  Use ,test placeholder and (test) binding.

2. PARAMETERIZED TEST (with variables):
   Provide a template form with placeholders, variable names, and values.

3. MULTIPLE TEST CASES:
   Prefix test cases with :doc to provide individual descriptions.

AUTOMATIC TAGS:
The macro automatically adds tags based on OBJECT:
  - The object's name itself as a tag
  - \\='private if the name contains \\='--', otherwise \\='public
  - \\='macro if the object is a macro

EXAMPLES:

Example 1: Simple test (no parameters)
  (mevedel-deftest zenit-plist-map
    (:doc \"`zenit-plist-map' maps fn to plist\")
    (let ((plist \\='(:a 1 :b 2 :c 3)))
      (zenit-plist-map (lambda (key val) (1+ val)) plist)
      (should (equal \\='(:a 2 :b 3 :c 4) plist))))

Example 2: Simple test with placeholder
  (mevedel-deftest file-exists-p!
    (:doc \"`file-exists-p!' tests if one or more files exist.\")
    ,test
    (test)
    (should (file-exists-p! (file!)))
    (let ((test-file (mevedel-test-make-temp-file)))
      (should (equal (expand-file-name test-file) (file-exists-p! test-file)))
      (delete-file test-file)))

Example 3: Parameterized test with multiple cases
  (mevedel-deftest zenit-path
    (:doc \"`zenit-path' returns a path from segments\")
    (should (equal ,out (zenit-path ,@in)))
    (in out)
    (\"/tmp\" \"foo\" \"bar.txt\") \"/tmp/foo/bar.txt\"
    (\"foo\") (expand-file-name \"foo\")
    (\"/tmp\" \"foo\" nil \"bar.txt\") \"/tmp/foo/bar.txt\")

Example 4: Multiple test cases with individual docstrings
  (mevedel-deftest zenit-surrounded-p
    (:vars ((test-buffer (get-buffer-create \"test-buffer\")))
     :before-each
     (with-current-buffer test-buffer
       (erase-buffer)
       (emacs-lisp-mode))
     :after-each
     (kill-buffer test-buffer))
    ,test
    (test)
    :doc \"`zenit-surrounded-p' returns t when surrounded\"
    (with-current-buffer test-buffer
      (insert \"foo {bar} baz\")
      (goto-char 7)
      (should (zenit-surrounded-p \\='(:beg 5 :end 8 :op \"{\" :cl \"}\"))))

    :doc \"`zenit-surrounded-p' returns nil when not surrounded\"
    (with-current-buffer test-buffer
      (insert \"foo {bar} baz\")
      (goto-char 4)
      (should-not (zenit-surrounded-p \\='(:beg 5 :end 8 :op \"{\" :cl \"}\")))))

Example 5: Parameterized test with different assertions
  (mevedel-deftest zenit-file-cookie-p
    (:doc \"`zenit-file-cookie-p' returns the evaluated result\")
    (let ((test-file (mevedel-test-make-temp-file nil \".el\" ,fcookie)))
      (,assert (zenit-file-cookie-p test-file ,tcookie ,null-value))
      (delete-file test-file))
    (assert fcookie tcookie null-value)
    should \";;;###if (equal \\\"test\\\" \\\"test\\\")\" \"if\" nil
    should \";;;###foo-test (equal \\\"test\\\" \\\"test\\\")\" \"foo-test\" nil
    should \";;;###foo-test (equal \\\"test\\\" \\\"test\\\")\" \"if\" t
    should-not \";;;###foo-test (equal \\\"test\\\" \\\"test\\\")\" \"if\" nil)

Example 6: Test with setup/teardown and vars
  (mevedel-deftest zenit-syntax-ppss
    (:vars ((test-buffer (get-buffer-create \"test-buffer\")))
     :before-each
     (with-current-buffer test-buffer
       (erase-buffer)
       (emacs-lisp-mode)
       (setq zenit--sppss-memo-last-point nil
             zenit--sppss-memo-last-result nil))
     :after-each
     (kill-buffer test-buffer)
     :doc \"`zenit-syntax-ppss' parses syntax and caches state\")
    ,test
    (test)
    (with-current-buffer test-buffer
      (insert \"(hello \\\"world\\\") ; comment\")
      (goto-char 1)
      (let ((result1 (zenit-syntax-ppss 8)))
        (should (eq result1 (zenit-syntax-ppss 8))))))

PATTERN DETAILS:

For parameterized tests, the template form uses backquote syntax
where commas unquote variable values:
  - ,var    -> unquotes single variable
  - ,@var   -> unquotes and splices list variable
  - ,test   -> special placeholder for simple tests

The variable binding line lists variables that will be bound:
  (var1 var2 var3)

Then provide values in groups matching the variable count:
  value1 value2 value3     ; First test case
  value4 value5 value6     ; Second test case

Optionally prefix each case with :doc \"description\" for individual
test documentation.

See also:
  `mevedel-test--template' - Template expansion function
  `mevedel-test--normalize-cases' - Docstring normalization"
  (declare (indent defun) (debug t))
  ;; Initialize test counter and automatic tags
  (let ((counter 0)
        (autotags
         (delq nil
               (list
                object
                (if (string-match-p "--" (symbol-name object))
                    'private 'public)
                (if (macrop object) 'macro))))
        ;; Generate tests from template
        (tests (when template
                 (macroexpand `(mevedel-test--template ,@template)))))
    ;; Combine automatic and manual tags
    (setq tags (append autotags tags))
    ;; Generate the test forms
    `(progn
       ,@(mapcar
          (lambda (test)
            (let ((test-body
                   `(,@(when before-each
                         (if (cl-every #'listp before-each)
                             before-each
                           (list before-each)))
                     ,@(cdr test)
                     ,@(when after-each
                         (if (cl-every #'listp after-each)
                             after-each
                           (list after-each))))))
              (when quiet
                (setq test-body
                      `((mevedel-test--with-captured-diagnostics nil
                          ,@test-body))))
              (setq test-body
                    `((let ((worktree-controls-before
                             (mevedel-test--worktree-control-snapshot)))
                        (unwind-protect
                            (progn ,@test-body)
                          (mevedel-test--cancel-stray-lease-timers)
                          (when mevedel-test--release-leaked-state-p
                            (mevedel-test--release-leaked-state))
                          (mevedel-test--assert-worktree-controls-unchanged
                           worktree-controls-before)))))
              `(ert-deftest
                   ,(intern (concat
                             (format "%s/test" object)
                             (when (> (length tests) 1)
                               (format "@%d" (cl-incf counter)))))
                   ()
                 ,(or (and (stringp (car test))
                           (not (string-empty-p (car test)))
                           (car test))
                      doc
                      (when (fboundp object) (documentation object)))
                 ,@(when tags `(:tags ',tags))
                 ,@(when expected-result `(:expected-result ,expected-result))
                 ,@(cond
                   (vars*  `((let* ,vars* ,@test-body)))
                   (vars   `((let ,vars ,@test-body)))
                   (t      test-body)))))
          tests))))

(defun mevedel-test-enable-fontlocking ()
  "Enable fontlocking for `mevedel-deftest'."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<mevedel-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
;; Activate the font-locking
(mevedel-test-enable-fontlocking)


;;
;;; Test Functions

(defun mevedel-test-tabulated-row-cells (row)
  "Return ROW's displayed cells as plain strings.
ROW may be a `tabulated-list-entries' entry or its cell vector."
  (let ((cells (if (vectorp row) row (cadr row))))
    (mapcar
     (lambda (cell)
       (cond
        ((stringp cell) (substring-no-properties cell))
        ((null cell) "")
        (t (substring-no-properties (format "%s" cell)))))
     (append cells nil))))

(defun mevedel-test-tabulated-entries-cells (&optional entries)
  "Return ENTRIES as an alist of row ids to plain cell strings."
  (mapcar
   (lambda (entry)
     (cons (car entry) (mevedel-test-tabulated-row-cells entry)))
   (or entries tabulated-list-entries)))

(defun mevedel-test-same-items-p (expected actual &rest cl-keys)
  "Verify that EXPECTED and ACTUAL have the same items.
The order of items does not matter.  Return t if lists match, nil
otherwise.
CL-KEYS as in `cl-set-difference'.
\nKeywords supported:  :test :test-not :key
\n(fn EXPECTED ACTUAL [KEYWORD VALUE]...)"
  (and (null (apply #'cl-set-difference expected actual cl-keys))
       (null (apply #'cl-set-difference actual expected cl-keys))))

(defun mevedel-test-contains-items-p (expected actual &rest cl-keys)
  "Verify EXPECTED items are present in ACTUAL.
The order of items does not matter.  Return t if lists match, nil
otherwise.
CL-KEYS as in `cl-set-difference'.
\nKeywords supported: :test :test-not :key
\n(fn EXPECTED ACTUAL [KEYWORD VALUE]...)"
  (null (apply #'cl-set-difference expected actual cl-keys)))

(defmacro mevedel-test--with-displayed-buffer (&rest body)
  "Run BODY in a temp buffer displayed in the selected window.
Restores the window configuration and kills the buffer afterwards."
  (declare (indent 0))
  `(let ((config (current-window-configuration))
         (buffer (generate-new-buffer " *mevedel-displayed-test*")))
     (unwind-protect
         (progn
           (set-window-buffer (selected-window) buffer)
           (with-current-buffer buffer
             ,@body))
       (set-window-configuration config)
       (kill-buffer buffer))))

(provide 'helpers)
;;; helpers.el ends here
