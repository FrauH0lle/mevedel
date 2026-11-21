;;; loading.el --- Cold startup scenarios -*- lexical-binding: t -*-

;;; Commentary:
;; Runs only in Eask-isolated fresh subprocesses, with no test helpers.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'gptel-openai)
(require 'json)

(defconst niwa-test-loading--root
  (expand-file-name "../.." (file-name-directory load-file-name)))
(defconst niwa-test-loading--deferred
  '(niwa-tool-exec niwa-tool-exec-permission niwa-tool-patch
    niwa-tool-web niwa-tool-fs-read niwa-tool-fs-search
    niwa-shared-editing niwa-shared-library niwa-view
    niwa-collaboration-guest niwa-collaboration-files
    niwa-collaboration-artifact niwa-collaboration-editing
    niwa-collaboration-transport niwa-claude-code niwa-acp acp
    niwa-mcp niwa-claude-code-usage))

(defconst niwa-test-loading--answer "cold stub answer")

(defun niwa-test-loading--provider ()
  "Serve a local OpenAI-compatible endpoint and select it as the backend.
Every request receives `niwa-test-loading--answer', streamed when asked."
  (let* ((reply
          (lambda (process request)
            (let* ((stream (string-match-p "\"stream\": *true" request))
                   (message `((role . "assistant")
                              (content . ,niwa-test-loading--answer)))
                   (body
                    (if stream
                        (concat
                         "data: " (json-encode
                                   `((choices . [((index . 0) (delta . ,message))])))
                         "\n\ndata: " (json-encode
                                        `((choices . [((index . 0)
                                                       (delta . ,(make-hash-table))
                                                       (finish_reason . "stop"))])))
                         "\n\ndata: [DONE]\n\n")
                      (json-encode
                       `((choices . [((index . 0) (message . ,message)
                                      (finish_reason . "stop"))]))))))
              (process-send-string
               process
               (concat "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: "
                       (if stream "text/event-stream" "application/json")
                       "\r\n\r\n" body))
              (process-send-eof process))))
         (server
          (make-network-process
           :name "niwa-loading-provider" :server t :host 'local :service t
           :family 'ipv4 :noquery t :coding 'binary
           :filter
           (lambda (process input)
             (let* ((request (concat (process-get process 'request) input))
                    (case-fold-search t)
                    (end (string-search "\r\n\r\n" request))
                    (length (and end (string-match "^content-length: *\\([0-9]+\\)" request)
                                 (string-to-number (match-string 1 request)))))
               (process-put process 'request request)
               (when (and end (string-match-p "^expect: *100-continue" request)
                          (not (process-get process 'continued)))
                 (process-put process 'continued t)
                 (process-send-string process "HTTP/1.1 100 Continue\r\n\r\n"))
               (when (and end (>= (- (length request) end 4) (or length 0))
                          (not (process-get process 'answered)))
                 (process-put process 'answered t)
                 (funcall reply process request)))))))
    (setq gptel-backend
          (gptel-make-openai "Cold stub"
            :host (format "127.0.0.1:%d" (process-contact server :service))
            :protocol "http" :stream t :key "unused")
          gptel-model 'gpt-4o-mini)))

(defun niwa-test-loading--await (predicate)
  "Process events until PREDICATE holds."
  (with-timeout (15 (ert-fail "Cold scenario timed out"))
    (while (not (funcall predicate)) (accept-process-output nil 0.02))))

(defun niwa-test-loading--session ()
  "Return the live session's data buffer."
  (seq-find (lambda (buffer)
              (and (buffer-local-value 'niwa--session buffer)
                   (not (provided-mode-derived-p
                         (buffer-local-value 'major-mode buffer) 'niwa-view-mode))))
            (buffer-list)))

(defun niwa-test-loading--answers ()
  "Return how often the session transcript holds the stub answer."
  (with-current-buffer (niwa-test-loading--session)
    (how-many (regexp-quote niwa-test-loading--answer) (point-min) (point-max))))

(defun niwa-test-loading--settle (answers)
  "Wait until the session is idle with ANSWERS stub answers."
  (niwa-test-loading--await
   (lambda ()
     (and (niwa-test-loading--session)
          (not (niwa-turn-busy-p (niwa-test-loading--session)))
          (= answers (niwa-test-loading--answers))))))

(defun niwa-test-loading--send (text)
  "Submit TEXT from the session view composer and await its answer."
  (let ((answers (niwa-test-loading--answers)))
    (with-current-buffer (buffer-local-value
                          'niwa--view-buffer (niwa-test-loading--session))
      (niwa-view-composer-insert text)
      (with-current-buffer (niwa-view-composer-buffer)
        (niwa-view-send)))
    (niwa-test-loading--settle (1+ answers))))

(defun niwa-test-loading--entry-points ()
  "Return documented and `;;;###autoload' command candidates."
  (let (names)
    (with-temp-buffer
      (insert-file-contents (file-name-concat niwa-test-loading--root "README.md"))
      (while (re-search-forward "^| `\\(niwa[[:alnum:]-]*\\)`" nil t)
        (push (intern (match-string 1)) names)))
    (dolist (file (directory-files niwa-test-loading--root t "\\`niwa.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "^;;;###autoload\n(\\(?:defun\\|define-[a-z-]*mode\\) \\([^ ()]+\\)" nil t)
          (push (intern (match-string 1)) names))))
    (delete-dups names)))

(defun niwa-test-loading (scenario &optional state)
  "Exercise cold SCENARIO through installed entry points.
STATE names a directory that outlives the process; it defaults to a
temporary one."
  (let* ((root (or state (make-temp-file "niwa-loading-state-" t)))
         (default-directory (file-name-as-directory root))
         (user-emacs-directory (file-name-concat root "emacs/"))
         (inhibit-message t))
    (unwind-protect
        (progn
          (require 'niwa)
          (setq niwa-user-dir (file-name-concat root "user/"))
          (pcase scenario
            ('commands
             ;; Documented commands and autoload cookies are M-x entry points
             ;; as soon as the package loads, also from a source checkout.
             (let ((unreachable (seq-remove #'commandp (niwa-test-loading--entry-points)))
                   stale)
               ;; README tables also name options; every module tells them apart.
               (dolist (file (directory-files niwa-test-loading--root nil
                                              "\\`niwa.*\\.el\\'"))
                 (require (intern (file-name-base file))))
               (ert-info ((format "Unreachable commands: %S" unreachable))
                 (should-not (seq-filter #'commandp unreachable)))
               (mapatoms (lambda (symbol)
                           (when (and (string-prefix-p "niwa" (symbol-name symbol))
                                      (autoloadp (symbol-function symbol)))
                             (push symbol stale))))
               (ert-info ((format "Autoloads their file does not define: %S" stale))
                 (should-not stale))))
            ('notes
             (call-process "git" nil nil nil "init" "-q")
             (write-region ".niwa/\n" nil ".gitignore" nil 'silent)
             (write-region "noted code\n" nil "source.el" nil 'silent)
             (should-not (featurep 'niwa-notes))
             ;; A user's init enables visit restoration without loading notes.
             (niwa-notes-global-mode 1)
             (should-not (featurep 'niwa-notes))
             (let ((source (find-file-noselect "source.el")))
               (with-current-buffer source (niwa-note-create "Cold note"))
               (should (file-exists-p ".niwa/instructions.el"))
               (kill-buffer source))
             (with-current-buffer (find-file-noselect "source.el")
               (should (seq-some (lambda (ov)
                                   (string-search "Note 1 · You  Cold note"
                                                  (or (overlay-get ov 'after-string) "")))
                                 (overlays-in (point-min) (point-max))))
               (niwa-notes-toggle-labels)
               (should (seq-some
                        (lambda (ov)
                          (string-match-p "Cold note" (or (overlay-get ov 'after-string) "")))
                        (overlays-in (point-min) (point-max)))))
             (should-not (featurep 'niwa-chat))
             (should-not (featurep 'niwa-buddy)))
            ('note-lifecycle
             ;; Lifecycle verbs reach the conversation hint through a real
             ;; autoload, and persistence reaches the Rewind rule the same way.
             (call-process "git" nil nil nil "init" "-q")
             (write-region ".niwa/\n" nil ".gitignore" nil 'silent)
             (write-region "noted code\n" nil "source.el" nil 'silent)
             (should-not (featurep 'niwa-note-conversation))
             (with-current-buffer (find-file-noselect "source.el")
               (let ((note (niwa-note-create "Cold note"))
                     (workspace (niwa--instruction-buffer-workspace (current-buffer))))
                 (niwa-note-mark-sent (list note) "cold-session")
                 (niwa-note-hold-change (list note) '(:id "cold" :session-id "cold-session" :turn 1))
                 (should (featurep 'niwa-note-conversation))
                 (should (eq 'ready (niwa-notes-attention-state note)))
                 (niwa-notes-rewind-live workspace "cold-session" 0)
                 (should (eq 'unsent (overlay-get note 'niwa-note-state)))
                 (should-not (overlay-get note 'niwa-note-prepared-change)))))
            ('handoff-progress
             (require 'niwa-plan-handoff)
             (require 'niwa-view)
             (require 'niwa-view-composer)
             (should-not (featurep 'niwa-view-render))
             (should (autoloadp (symbol-function 'niwa-view-stream-end-progress)))
             ;; Exercise the live-buffer path before any view setup can supply
             ;; the stream owner's renderer dependency.
             (with-temp-buffer
               (niwa-view-stream-end-progress (current-buffer))
               (should niwa-view--request-progress-suppressed)
               (should-not niwa-view--spinner-status)
               (niwa-view-stream-show-progress
                (current-buffer) "Preparing plan..." 'plan-preparation)
               (should-not niwa-view--request-progress-suppressed)
               (should (equal "Preparing plan..." niwa-view--spinner-status))
               (let ((token (niwa-view-stream-claim-progress
                             (current-buffer) "Hook..." 'hook)))
                 (should (eq 'hook niwa-view--spinner-owner))
                 (niwa-view-stream-release-progress (current-buffer) token)
                 (should (eq 'plan-preparation niwa-view--spinner-owner))
                 (should (equal "Preparing plan..." niwa-view--spinner-status)))
               (niwa-view-stream-end-progress (current-buffer))
               (should niwa-view--request-progress-suppressed)
               (should-not niwa-view--spinner-status)
               (should-not niwa-view--spinner-timer)))
            ('request-progress
             (require 'gptel-request)
             (require 'niwa-view-stream)
             (should-not (featurep 'niwa-view-composer))
             (should-not (featurep 'niwa-agents))
             (with-temp-buffer
               (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
                 (should-not (niwa-view-stream-ensure-progress-for-fsm fsm))
                 (should (featurep 'niwa-view-composer))
                 (should-not (featurep 'niwa-agents))
                 (should (autoloadp (symbol-function 'niwa-agent-invocation-p)))
                 ;; The same predicate must recognize agents after first use,
                 ;; whether identified by request info or buffer-local state.
                 (require 'niwa-agents)
                 (let ((invocation (niwa-agent-invocation--create)))
                   (should (niwa-view--agent-fsm-p
                            (list :niwa-agent-invocation invocation) nil))
                   (should-not (niwa-view--agent-fsm-p
                                '(:niwa-agent-invocation invalid) nil))
                   (setq-local niwa--agent-invocation invocation)
                   (should (niwa-view--agent-fsm-p nil (current-buffer)))))))
            ('stop-shares
             (should (commandp 'niwa-collaboration-stop-all))
             (should-not (featurep 'niwa-collaboration-lobby))
             ;; A saved intent can exist before installation/startup restores
             ;; lobbies.  Stop all must forget it without opening a transport.
             (make-directory niwa-user-dir t)
             (let ((intent (file-name-concat niwa-user-dir "lobbies.el")))
               (with-temp-file intent (prin1 (list root) (current-buffer)))
               (cl-letf (((symbol-function 'message) #'ignore))
                 (call-interactively 'niwa-collaboration-stop-all))
               (should-not (file-exists-p intent))
               (should (zerop (hash-table-count niwa-collaboration-lobby--lobbies)))))
            ('artifacts
             (call-process "git" nil nil nil "init" "-q")
             (should-not (featurep 'niwa-artifacts-list))
             (call-interactively #'niwa-artifacts)
             (should (derived-mode-p 'niwa-artifacts-list-mode))
             (should-not (featurep 'niwa-shared-editing))
             (let* ((workspace (niwa-workspace))
                    (session (niwa-session--create :workspace workspace))
                    (file (file-name-concat root ".niwa/artifacts/cold/index.html")))
               (make-directory (file-name-directory file) t)
               (write-region "<p>cold</p>" nil file nil 'silent)
               (niwa-artifact-store-note-writes
                session (list (list :action 'write :path file)))
               (should (equal '("cold") (niwa-session-attached-artifacts session)))
               (should (= 1 (length (niwa-artifact-store-versions workspace "cold"))))
               (niwa-cockpit-surface-refresh)
               (should (= 1 (length tabulated-list-entries)))
               (should-not (featurep 'niwa-shared-editing))
               ;; Creating asks to share before anything is queued; declining
               ;; leaves no item and no editing helper behind.
               (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Document"))
                         ((symbol-function 'read-string) (lambda (&rest _) "Cold"))
                         ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                 (should-error (call-interactively #'niwa-artifacts-new-shared-item)
                               :type 'user-error))
               (should (equal '("cold") (niwa-artifact-store-ids workspace)))
               (should-not (seq-filter #'process-live-p (process-list))))
             (kill-buffer (current-buffer)))
            ('live-execution
             (require 'niwa-view)
             (let ((data (generate-new-buffer " *cold-execution-data*"))
                   (view (generate-new-buffer " *cold-execution-view*")))
               (unwind-protect
                   (progn
                     (with-current-buffer data (org-mode))
                     (niwa-view--setup view data)
                     (should-not (featurep 'niwa-view-audit))
                     (with-current-buffer view
                       ;; The call can yield before its source row arrives.
                       ;; Explicit navigation cold-loads the evidence owner.
                       (niwa-view-execution-show-in-conversation
                        '(:execution-id "cold-exec" :tool-use-id "cold-tool"
                          :owner "/root" :command "printf cold"
                          :execution-output "cold retained output")))
                     (should (featurep 'niwa-view-audit))
                     (with-current-buffer "*niwa execution result*"
                       (should buffer-read-only)
                       (should (equal "printf cold\n\ncold retained output"
                                      (buffer-string)))))
                 (when (get-buffer "*niwa execution result*")
                   (kill-buffer "*niwa execution result*"))
                 (kill-buffer view)
                 (kill-buffer data))))
            ('send
             (call-process "git" nil nil nil "init" "-q")
             (niwa-install)
             (niwa-test-loading--provider)
             (niwa)
             (niwa-test-loading--send "first cold message")
             ;; Memory writes load the patch implementation on demand.
             (should-not (featurep 'niwa-tool-patch))
             (with-current-buffer (niwa-test-loading--session)
               (call-interactively #'niwa-save-session)))
            ('resume
             (niwa-install)
             (niwa-test-loading--provider)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt table &rest _)
                          (let ((rows (all-completions "" table)))
                            (or (seq-find (lambda (entry) (string-prefix-p "Resume" entry))
                                          rows)
                                (error "Expected ordinary cold Resume; rows: %S" rows))))))
               (niwa))
             (should (= 1 (niwa-test-loading--answers)))
             (niwa-test-loading--send "resumed cold message")
             (with-current-buffer (niwa-test-loading--session)
               (should (string-search "first cold message" (buffer-string)))))
            ('installation
             (niwa-uninstall)
             (dotimes (_ 2)
               (niwa-install)
               (should (commandp 'niwa-compact))
               (should (commandp 'niwa-buddy-mode))
               (should (commandp 'niwa-buddy-global-mode))
               (should-not (featurep 'niwa-buddy))
               (should (commandp 'niwa-claude-code-setup))
               (should (memq #'niwa-journal-idle-session-opened niwa-session-start-hook))
               (dolist (tool (niwa-tool-all))
                 (should (functionp (niwa-tool-handler tool)))
                 (should (stringp (niwa-tool-prompt tool))))
               (should (> (length (niwa-tool-all)) 35))
               (when after-focus-change-function (funcall after-focus-change-function))
               (set-face-attribute 'default nil :foreground (face-attribute 'default :foreground))
               (run-hook-with-args 'enable-theme-functions 'user)
               (dolist (feature niwa-test-loading--deferred)
                 (ert-info ((format "Unexpected eager feature: %s" feature))
                   (should-not (featurep feature))))
               (dolist (spec niwa-tool-repair--shape-advice)
                 (should (advice-member-p (nth 2 spec) (car spec))))
               (niwa-uninstall)
               (dolist (spec niwa-tool-repair--shape-advice)
                 (should-not (advice-member-p (nth 2 spec) (car spec))))
               (dolist (feature niwa-test-loading--deferred)
                 (should-not (featurep feature)))))
            ('gptel
             (niwa-install)
             (with-temp-buffer
               (setq-local gptel-backend (gptel-make-openai "Cold native" :key "unused")
                           gptel-model 'gpt-4o-mini)
               (let ((fsm (gptel-request "hello" :dry-run t)))
                 (should (plist-get (gptel-fsm-info fsm) :data))))
             (dolist (feature '(acp niwa-acp niwa-mcp niwa-claude-code))
               (should-not (featurep feature))))
            ((or 'chat 'chat-in-directory)
             (niwa-install)
             (if (eq scenario 'chat-in-directory)
                 (niwa-in-directory default-directory)
               (niwa))
             (should (featurep 'niwa-view))
             (should (cl-some (lambda (buffer)
                                (with-current-buffer buffer
                                  (derived-mode-p 'niwa-view-mode)))
                              (buffer-list)))
             (should (featurep 'niwa-journal-idle))
             (require 'niwa-readiness)
             (dolist (feature '(acp niwa-acp niwa-mcp niwa-claude-code
                               niwa-claude-code-usage))
               (should-not (featurep feature))))
            ('tools
             (niwa-install)
             ;; Persisted references recover the catalog before implementations.
             (niwa-tool-clear-registry)
             (dolist (name '("Bash" "Eval" "Read" "Glob" "ApplyPatch" "WebFetch"))
               (should (niwa-tool-ensure name)))
             (dolist (feature niwa-test-loading--deferred)
               (should-not (featurep feature)))
             (require 'niwa-permission-mode)
             (require 'niwa-permission-rules)
             (let ((niwa-permission-mode 'full-auto)
                   (niwa-permission-rules nil) outcome)
               (funcall (niwa-tool-check-permission-async (niwa-tool-get "Eval"))
                        nil '(:expression "(+ 1 2)")
                        (lambda (value) (setq outcome value)))
               (should (eq 'allow outcome)))
             (let ((session (niwa-session--create :name "cold"))
                   result)
               (with-temp-buffer
                 (setq-local niwa--session session)
                 (funcall (gptel-tool-function
                           (niwa-tool-gptel-tool (niwa-tool-ensure "ListExecutions")))
                          (lambda (value) (setq result value))))
               (should (stringp result))
               (should-not (string-prefix-p "Error:" result)))
             (should (featurep 'niwa-tool-exec))
             (let ((file (file-name-concat root "sample.txt")))
               (write-region "cold read evidence\n" nil file nil 'silent)
               (let (result)
                 (funcall (niwa-tool-handler (niwa-tool-ensure "Read"))
                          (lambda (value) (setq result value)) (list :file_path file))
                 (should (string-match-p "cold read evidence" (plist-get result :result)))))
             (let ((tool (niwa-tool-ensure "ApplyPatch")))
               (should (listp (funcall (niwa-tool-get-paths tool)
                                      '(:patch "*** Begin Patch\n*** Add File: cold.txt\n+hello\n*** End Patch")))))
             (let ((tool (niwa-tool-ensure "WebFetch")))
               (should (equal "example.com"
                              (funcall (niwa-tool-get-domain tool)
                                       '(:url "https://example.com/path"))))))
            ('renderers
             (niwa-install)
             (dolist (row '(("Read" (:file_path "sample.txt") nil)
                            ("Glob" (:pattern "*.el") nil)
                            ("Bash" (:command "printf hello") nil)
                            ("ApplyPatch" nil (:kind patch :files nil))
                            ("WebFetch" (:url "https://example.com") nil)))
               (let* ((tool (niwa-tool-ensure (car row)))
                      (renderer (niwa-tool-renderer tool)))
                 (when (listp renderer) (setq renderer (cdr (assq 'success renderer))))
                 (should (plist-get (funcall renderer (car row) (cadr row) "hello" (caddr row))
                                    :header)))))
            ('worker-teardown
             (require 'niwa-journal-worker)
             ;; A maintenance Emacs can be inside a synchronous control-fs
             ;; shell.  Killing Emacs does not wait for that shell to finish.
             (let* ((ready (file-name-concat root "worker-ready"))
                    (finished (file-name-concat root "worker-finished"))
                    (process
                     (make-process
                      :name "loading-worker" :buffer nil :noquery t
                      :connection-type 'pipe :sentinel #'ignore
                      :command
                      (list (expand-file-name invocation-name invocation-directory)
                            "--batch" "-Q" "--eval"
                            (prin1-to-string
                             `(call-process "sh" nil nil nil "-c"
                                            "touch \"$1\"; sleep 0.5; touch \"$2\""
                                            "loading-worker" ,ready ,finished))))))
               (push process niwa-journal-worker--processes)
               (niwa-test-loading--await (lambda () (file-exists-p ready)))
               (advice-add 'niwa-journal-worker-stop-all :before
                           (lambda () (should (file-exists-p finished))))))
            ('native-btw
             (niwa-install)
             (setq gptel-backend (niwa-claude-code-register) gptel-model 'sonnet)
             (niwa)
             (let* ((parent (niwa-test-loading--session))
                    (parent-file (file-name-concat root "fixture-session.json"))
                    (fork-file (file-name-concat root "fixture-fork.json"))
                    side)
               (setenv "NIWA_ACP_TEST_HISTORY" root)
               (with-temp-file parent-file (insert "[\"cold native history\"]"))
               (should-not (featurep 'niwa-claude-code-btw))
               ;; Replace only the paid executable at its real loading boundary.
               (autoload 'niwa-claude-code-launch "niwa-claude-code")
               (advice-add
                'niwa-claude-code-launch :override
                (lambda (_system mcp _model _effort &optional id &rest _)
                  (list :command (executable-find "python3")
                        :args (list (file-name-concat niwa-test-loading--root
                                                     "test/fixtures/acp-agent.py"))
                        :cwd root :session-id id :mcp-servers mcp)))
               (with-current-buffer parent
                 (goto-char (point-max))
                 (insert "*** Accepted parent prompt\n"
                         (propertize "Visible parent answer\n" 'gptel 'response))
                 ;; Expand the struct setter only after the cold entry loaded it.
                 (eval '(setf (niwa-session-external-conversations niwa--session)
                              (list (list "root" :engine 'claude-code :id "fixture-session"
                                          :host (system-name) :directory niwa-claude-code-directory
                                          :state 'ready))) t))
               (with-current-buffer (buffer-local-value 'niwa--view-buffer parent)
                 (niwa-view-composer-insert "/btw")
                 (with-current-buffer (niwa-view-composer-buffer)
                   (niwa-view-send)))
               (niwa-test-loading--await
                (lambda ()
                  (setq side (buffer-local-value 'niwa-side-conversation--side-buffer parent))
                  (and (buffer-live-p side)
                       (buffer-live-p (buffer-local-value 'niwa--view-buffer side)))))
               (with-current-buffer (buffer-local-value 'niwa--view-buffer side)
                 (niwa-view-composer-insert "cold side question")
                 (with-current-buffer (niwa-view-composer-buffer)
                   (niwa-view-send)))
               (niwa-test-loading--await (lambda () (not (niwa-turn-busy-p side))))
               (with-current-buffer (buffer-local-value 'niwa--view-buffer side)
                 (should (string-search "cold native history" (buffer-string)))
                 (niwa-side-conversation-close))
               (niwa-test-loading--await (lambda () (not niwa-claude-code-btw--forks)))
               (should-not (file-exists-p fork-file))
               (should (equal "[\"cold native history\"]"
                              (with-temp-buffer
                                (insert-file-contents parent-file) (buffer-string))))))
            ('claude
             (let* ((backend (niwa-claude-code-register))
                    (gptel-backend backend) (gptel-model 'sonnet)
                    (gptel-tools nil) response)
               (should (niwa-engine-external-p backend))
               (should-not (featurep 'acp))
               ;; Substitute the paid launcher only; real generic dispatch,
               ;; transport loading, ACP protocol, and callbacks run unchanged.
               (autoload 'niwa-claude-code-launch "niwa-claude-code")
               (require 'niwa-claude-code-backend)
               (advice-add
                'niwa-claude-code-launch :override
                (lambda (&rest _)
                  (list :command (executable-find "python3")
                        :args (list (file-name-concat niwa-test-loading--root
                                                     "test/fixtures/acp-agent.py"))
                        :cwd root)))
               (niwa-engine-request-text
                backend "hello" "system" (lambda (text _info) (setq response text)))
               (with-timeout (5 (ert-fail "Claude fixture timed out"))
                 (while (not response) (accept-process-output nil 0.01)))
               (should (equal "answer:hello" response))
               (should (featurep 'acp))
               (let (done text)
                 (niwa-claude-code-request-workload
                  "workload" "system" nil
                  (lambda (value _info)
                    (if (eq value t) (setq done t)
                      (when (stringp value) (push value text)))) nil nil)
                 (with-timeout (5 (ert-fail "Claude workload fixture timed out"))
                   (while (not done) (accept-process-output nil 0.01)))
                 (should (equal "answer:workload" (apply #'concat (nreverse text)))))))
            ('room
             (niwa-install)
             (niwa)
             (require 'niwa-collaboration)
             (require 'niwa-collaboration-transport)
             (should-not (featurep 'niwa-collaboration-task))
             (let ((data (niwa-collaboration--current-data-buffer)))
               (should (buffer-live-p data))
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                         ((symbol-function 'niwa-collaboration--transport-open)
                          (lambda (&rest _) (list :state 'down)))
                         ((symbol-function 'niwa-collaboration--transport-stop) #'ignore))
                 (let ((room (car (niwa-collaboration--room-or-start data))))
                   (unwind-protect
                       (progn
                         (niwa-collaboration--publish-timer data)
                         (should (featurep 'niwa-collaboration-task))
                         (should (equal "tasks" (plist-get (plist-get room :tasks) :t)))
                         (should (= 0 (plist-get (plist-get room :tasks) :total))))
                     (niwa-collaboration--stop-internal room 'user-stop))))))
            ('lobby
             (require 'niwa-collaboration-lobby)
             (niwa-collaboration-lobby-restore)
             (make-directory niwa-user-dir t)
             (dolist (text '("" "nil" "(" "42"))
               (with-temp-file (niwa-collaboration-lobby--intent-path) (insert text))
               (niwa-collaboration-lobby-restore)
               (should-not (featurep 'niwa-collaboration-guest)))
             (niwa-collaboration-lobby--set-intended default-directory t)
             (autoload 'niwa-collaboration--transport-open "niwa-collaboration-transport")
             (advice-add 'niwa-collaboration--transport-open :override
                         (lambda (&rest _) (error "Fixture relay unavailable")))
             (let (warnings)
               (cl-letf (((symbol-function 'display-warning)
                          (lambda (&rest args) (push args warnings))))
                 (niwa-collaboration-lobby-restore))
               (should (featurep 'niwa-collaboration-guest))
               (should warnings)
               (should (equal (list default-directory) (niwa-collaboration-lobby--intended))))
             (let ((opened 0))
               (cl-letf (((symbol-function 'niwa-collaboration--transport-open)
                          (lambda (&rest _) (cl-incf opened) nil)))
                 (niwa-collaboration-lobby-restore)
                 (niwa-collaboration-lobby-restore)
                 (should (= opened 1))
                 (niwa-collaboration-lobby-stop)
                 (should-not (niwa-collaboration-lobby--intended)))))
            ('lobby-open
             ;; A lobby restored at startup opens a session saved by an
             ;; earlier Emacs for a browser guest.  Nothing has loaded the
             ;; chat view yet, so the open is its first load, inside the
             ;; lobby's `inhibit-interaction'.  Initializing D-Bus waits for
             ;; events; a host without a system bus skips that wait.  Make it
             ;; wait everywhere, as on a desktop, without contacting a real bus.
             (advice-add 'dbus-init-bus :override
                         (lambda (&rest _)
                           (when inhibit-interaction (read-event nil nil 0.001))
                           (signal 'dbus-error '("No fixture bus"))))
             (niwa-install)
             (should-not (featurep 'niwa-view-stream))
             (should-not (featurep 'dbus))
             (let* ((workspace (niwa-workspace))
                    (id (plist-get (plist-get (car (niwa-session-persistence-list-sessions
                                                    workspace))
                                              :summary)
                                   :session-id))
                    sent)
               (should (stringp id))
               (autoload 'niwa-collaboration--transport-open "niwa-collaboration-transport")
               (advice-add 'niwa-collaboration--transport-open :override
                           (lambda (&rest _) (list :state 'down)))
               (advice-add 'niwa-collaboration--transport-stop :override #'ignore)
               (advice-add 'niwa-collaboration--transport-send :override
                           (lambda (_transport peer frame) (push (cons peer frame) sent) t))
               (require 'niwa-collaboration-lobby)
               (niwa-collaboration-lobby--set-intended default-directory t)
               (niwa-collaboration-lobby-restore)
               (let ((lobby (niwa-collaboration-lobby--find workspace)))
                 (should lobby)
                 (puthash 2 '(:name "Writer" :writable t) (plist-get lobby :guests))
                 (niwa-collaboration-lobby--on-frame
                  (niwa-workspace-root workspace) 2
                  (list :t "open-session" :reqId 7 :id id)))
               (let ((reply (cdr (seq-find (lambda (entry)
                                             (equal "open-session" (plist-get (cdr entry) :t)))
                                           sent))))
                 ;; A refusal's message is the useful failure output.
                 (should (equal nil (plist-get reply :message)))
                 (should (eq t (plist-get reply :ok)))
                 (should (stringp (plist-get reply :link))))
               (should (niwa-test-loading--session))
               ;; Power policy belongs to animation, not to restoring a view.
               (should-not (featurep 'battery))
               (should-not (featurep 'dbus))
               (niwa-collaboration-lobby-stop))
             ;; The first animation's deferred poll, outside the guest frame,
             ;; loads the power backend and samples it.
             (let ((view (buffer-local-value 'niwa--view-buffer
                                             (niwa-test-loading--session)))
                   (queries 0) (refreshes 0))
               (setq battery-status-function
                     (lambda () (cl-incf queries) '((?L . "AC") (?B . "charging"))))
               (niwa-view-power-watch view (lambda () (cl-incf refreshes)))
               (should-not (featurep 'battery))
               (niwa-test-loading--await (lambda () (> queries 0)))
               (should (featurep 'battery))
               (should (= 1 queries))
               (should (= 1 refreshes))
               (should (= 60 (niwa-view-power-framerate 60 15 'auto t)))
               (niwa-view-power-unwatch view)))))
      (dolist (buffer (buffer-list))
        (when (buffer-local-value 'niwa--session buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (niwa-uninstall)
      ;; Cancelling a maintenance Emacs can leave its synchronous control-fs
      ;; shell running.  Let it finish before stopping delivery and removing
      ;; its workspace; accept only that process, not timers or callbacks.
      (when (featurep 'niwa-journal-worker)
        (dolist (process (copy-sequence niwa-journal-worker--processes))
          (let ((deadline (+ (float-time) 15)))
            (while (process-live-p process)
              (when (> (float-time) deadline)
                (ert-fail "Maintenance child did not exit"))
              (accept-process-output process 0.02 nil 1))))
        (niwa-journal-worker-stop-all))
      (unless state (delete-directory root t)))))
