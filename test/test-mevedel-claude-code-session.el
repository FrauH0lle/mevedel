;;; test-mevedel-claude-code-session.el --- Subscription session dispatch -*- lexical-binding: t -*-

;;; Commentary:
;; Select the subscription provider and send through the ordinary entry point.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-claude-code)

(defconst mevedel-claude-code-session-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-claude-code-register ()
  (let ((gptel--known-backends nil))
    (mevedel-claude-code-register)
    (let* ((provider (mevedel-model-resolve-provider "Claude Code:sonnet"))
           (backend (plist-get provider :backend)))
      (should (mevedel-claude-code-backend-p backend))
      (should (equal "sonnet" (gptel--model-name (plist-get provider :model))))
      (should-error (gptel--request-data backend nil) :type 'user-error))))

(mevedel-deftest mevedel--send-request/claude (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          launches)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Session system fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (lambda (system mcp model effort &optional id _hook)
                   (push (list system model effort id) launches)
                   (list :command (executable-find "python3")
                         :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp :session-id id))))
        (insert "hello")
        (mevedel--send-request "hello")
        (with-timeout (5 (ert-fail "Selected subscription turn did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        (should (string-search "answer:hello" (buffer-string)))
        (should-not (mevedel-request-fsm request))
        (should (= 1 (mevedel-session-turn-count session)))
        (should (equal "sonnet" (cadar launches)))
        (should-not (nth 3 (car launches)))
        (let* ((encoded (mevedel-session-codec-serialize session))
               (decoded (mevedel-session-codec-deserialize encoded workspace))
               (restored (plist-get decoded :session)))
          (should (equal (mevedel-session-external-conversations session)
                         (mevedel-session-external-conversations restored))))
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:opus") buffer)
        (goto-char (point-max))
        (insert "\nagain")
        (mevedel--send-request "again")
        (with-timeout (5 (ert-fail "Retained subscription turn did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        (should (= 2 (mevedel-session-turn-count session)))
        (should (equal "opus" (cadar launches)))
        (should (equal "fixture-session" (nth 3 (car launches))))
        (should (string-search "answer:again" (buffer-string)))))))

(mevedel-deftest mevedel-view--forward-input-now/claude (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let ((view (generate-new-buffer " *claude-composer*"))
          (gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude")))
      (unwind-protect
          (progn
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Composer fixture" gptel-tools nil)
            (mevedel-chat-install-request-hooks)
            (mevedel-view--setup view buffer)
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id _hook)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-session-test--peer)
                               :cwd root :mcp mcp :session-id id))))
              (with-current-buffer view
                (mevedel-view--forward-input-now "hello composer")
                (goto-char (mevedel-view--input-start))
                (insert "> Keep this draft\nand its second line"))
              (with-timeout (5 (ert-fail "Composer subscription request did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
            (should (= 1 (mevedel-session-turn-count session)))
            (should (string-search "answer:hello composer" (buffer-string)))
            (with-current-buffer view
              (should (equal "> Keep this draft\nand its second line"
                             (mevedel-view--input-text)))))
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest mevedel-session-codec/external-history (:quiet t)
  (mevedel-engine-test--with-session
    (setf (mevedel-session-external-conversations session)
          (list (list "root" :engine 'claude-code :id "retained"
                      :host "fixture" :directory root :state 'in-flight
                      :input-boundary '(1 . 12))))
    (let* ((encoded (mevedel-session-codec-serialize session))
           (restored (plist-get (mevedel-session-codec-deserialize encoded workspace) :session)))
      (should (eq 'uncertain (plist-get (cdar (mevedel-session-external-conversations restored)) :state)))
      (should (eq 'in-flight (plist-get (cdar (mevedel-session-external-conversations session)) :state)))
      (should (equal '(1 . 12) (plist-get (cdar (mevedel-session-external-conversations restored)) :input-boundary)))
      (dolist (boundary '("invalid" (1 2) (-1 . 2) (1 . -2)))
        (let ((bad (copy-tree encoded)))
          (plist-put (cdar (plist-get bad :external-conversations)) :input-boundary boundary)
          (should-error (mevedel-session-codec-validate-current-sidecar bad))))
      (dolist (invalid '(("bad") (("root" :engine other))
                         (("root" :engine claude-code :state unstarted :id "fake"))
                         (("root" :engine claude-code :state unstarted :host "fixture"))
                         (("root" :engine claude-code :state unstarted :directory "/tmp"))
                         (("root" :engine claude-code :state unstarted :tool-calls (("id" . "Bash"))))
                         (("root" :engine claude-code :id "" :host "fixture"
                           :directory "/tmp" :state ready))))
        (should-error
         (mevedel-session-codec-validate-current-sidecar
          (plist-put (copy-tree encoded) :external-conversations invalid))))
      (dolist (calls '((("same" . "Read") ("same" . "Bash"))
                       (("" . "Read")) (("id" . 3)) ("not-a-pair")))
        (let ((bad (copy-tree encoded)))
          (plist-put (cdar (plist-get bad :external-conversations)) :tool-calls calls)
          (should-error (mevedel-session-codec-validate-current-sidecar bad)))))))

(mevedel-deftest mevedel--send-request/claude-publication (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (busy t))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Publication fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'mevedel-transport-busy-p) (lambda (_path) busy))
                ((symbol-function 'mevedel-claude-code-launch)
                 (lambda (_system mcp _model _effort &optional id _hook)
                   (list :command (executable-find "python3")
                         :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp :session-id id))))
        (mevedel--send-request "hello")
        (with-timeout (5 (ert-fail "History publication was not deferred"))
          (while (not (gethash (list 'acp-prompt (mevedel-request-id request))
                               mevedel-transport--pending))
            (accept-process-output nil 0.01)))
        (should (equal '(("root" :engine claude-code :state unstarted))
                       (mevedel-session-external-conversations session)))
        (should-not (string-search "answer:hello" (buffer-string)))
        (setq busy nil)
        (with-timeout (5 (ert-fail "Deferred subscription prompt did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        (should (string-search "answer:hello" (buffer-string)))
        (should (eq 'ready (plist-get (cdar (mevedel-session-external-conversations session)) :state)))))))

(mevedel-deftest mevedel-engine-assert-local-history (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let ((gptel--known-backends nil))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setf (mevedel-session-turn-count session) 1
            (mevedel-session-external-conversations session)
            (list (list "root" :engine 'claude-code :id "retained" :host (system-name)
                        :directory root :state 'ready)))
      (should-error (mevedel-session-rewind-assert-stable-source session buffer "Rewind")
                    :type 'user-error)
      (should-error (mevedel-session-save-as-run session buffer "copy" "copy-id"
                                               (file-name-concat root "copy"))
                    :type 'user-error)
      (should-not (file-exists-p (file-name-concat root "copy")))
      (should-error (mevedel-side-conversation-open "side question") :type 'user-error)
      (mevedel-model-set-session-provider
       session (list :backend (gptel--make-backend :name "API" :models '(api)) :model 'api) buffer)
      (should-not (mevedel-claude-code-backend-p gptel-backend))
      (should (equal "API:api" (mevedel-session-model-provider session))))))

(mevedel-deftest mevedel-engine-record-history-edit/no-output (:quiet t)
  (dolist (phase '(active reopened))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            receipt restored restored-session)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Silent fixture" gptel-tools nil)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                         (lambda (_system mcp _model _effort &optional id _hook)
                           (list :command (executable-find "python3")
                                 :args (list mevedel-claude-code-session-test--peer)
                                 :cwd root :mcp mcp :session-id id
                                 :observe (lambda (_owner event)
                                            (when (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params event))))
                                              (setq receipt t)))))))
                (insert "wait-silent")
                (mevedel--send-request "wait-silent")
                (with-timeout (5 (ert-fail "No submitted-input receipt"))
                  (while (not receipt) (accept-process-output nil 0.01))))
              ;; Normal metadata saves may remove and replace properties.
              ;; They are not edits to the prompt retained by Claude.
              (gptel--save-state)
              (gptel--save-state)
              (should-not (eq 'diverged (plist-get (cdr (assoc "root" (mevedel-session-external-conversations session))) :state)))
              (goto-char (point-max))
              (insert "\nnext drafX")
              (delete-char -1)
              (insert "t")
              (should-not (eq 'diverged (plist-get (cdr (assoc "root" (mevedel-session-external-conversations session))) :state)))
              (when (eq phase 'reopened)
                (mevedel-abort buffer)
                (with-timeout (5 (ert-fail "Silent abort did not settle"))
                  (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
                (let ((path (mevedel-session-save-path session)))
                  (test-mevedel-session-persistence--release-and-kill buffer session)
                  (setq restored (mevedel-session-persistence-restore path nil nil workspace)
                        restored-session (buffer-local-value 'mevedel--session restored))))
              (with-current-buffer (or restored buffer)
                (goto-char (point-min))
                (search-forward "wait-silent")
                (delete-char -1)
                (insert "X")
                (should (eq 'diverged (plist-get (cdr (assoc "root" (mevedel-session-external-conversations mevedel--session))) :state)))
                (when (mevedel-turn-busy-p (current-buffer))
                  (mevedel-abort (current-buffer))
                  (with-timeout (5 (ert-fail "Edited turn did not settle"))
                    (while (mevedel-turn-busy-p (current-buffer)) (accept-process-output nil 0.01))))
                (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                           (lambda (&rest _) (ert-fail "Diverged history launched an agent"))))
                  (should (string-search "recover-history"
                                         (error-message-string
                                          (should-error (mevedel--send-request "continue") :type 'user-error)))))))
          (when (buffer-live-p restored)
            (test-mevedel-session-persistence--release-and-kill restored restored-session)))))))

(mevedel-deftest mevedel--send-request/claude-reopen (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          ids restored-buffer restored-session)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Reopen fixture" gptel-tools nil)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (_system mcp _model _effort &optional id _hook)
                       (push id ids)
                       (list :command (executable-find "python3")
                             :args (list mevedel-claude-code-session-test--peer)
                             :cwd root :mcp mcp :session-id id))))
            (insert "hello")
            (mevedel--send-request "hello")
            (with-timeout (5 (ert-fail "First persisted turn did not finish"))
              (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
            (let ((path (mevedel-session-save-path session)))
              (test-mevedel-session-persistence--release-and-kill buffer session)
              (setq restored-buffer (mevedel-session-persistence-restore path nil nil workspace)))
            (should-not (eq buffer restored-buffer))
            (with-current-buffer restored-buffer
              (setq restored-session mevedel--session)
              (should (mevedel-claude-code-backend-p gptel-backend))
              (should (string-search "answer:hello" (buffer-string)))
              (goto-char (point-max))
              (insert "\nfollow-uX")
              (delete-char -1)
              (insert "p")
              (mevedel--send-request "follow-up")
              (with-timeout (5 (ert-fail "Reopened external history did not finish"))
                (while (mevedel-turn-busy-p restored-buffer) (accept-process-output nil 0.01)))
              (should (string-search "answer:follow-up" (buffer-string)))
              (should (= 2 (mevedel-session-turn-count restored-session)))
              ;; Editing displayed evidence cannot silently rewrite native history.
              (goto-char (point-min))
              (search-forward "answer:hello")
              (delete-char -1)
              (insert "X")
              (should (eq 'diverged (plist-get (cdr (assoc "root" (mevedel-session-external-conversations restored-session))) :state)))
              (mevedel-session-artifacts-save restored-session restored-buffer)
              (let ((path (mevedel-session-save-path restored-session)))
                (test-mevedel-session-persistence--release-and-kill restored-buffer restored-session)
                (setq restored-buffer (mevedel-session-persistence-restore path nil nil workspace)))
              (with-current-buffer restored-buffer
                (setq restored-session mevedel--session)
                (should (string-search "answer:hellX" (buffer-string)))
                (should (string-search "recover-history"
                                       (error-message-string
                                        (should-error (mevedel--send-request "Must reconcile") :type 'user-error)))))
              (should (= 2 (length ids))))
            (should (equal '("fixture-session" nil) ids)))
        (when (buffer-live-p restored-buffer)
          (test-mevedel-session-persistence--release-and-kill restored-buffer restored-session))))))

(mevedel-deftest mevedel--send-request/claude-startup-retry (:quiet t)
  (pcase-dolist (`(,failure ,reopen) '((abort nil) (abort t) (startup nil) (startup t)))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (file (file-name-concat root "selected.txt"))
            (first t) ids late-context restored restored-session)
        (write-region "Selected context" nil file nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Retry fixture" gptel-tools nil
                    gptel-use-context (and (eq failure 'abort) 'user)
                    gptel-context (list file)
                    gptel-context-string-function
                    (lambda (callback _sources) (setq late-context callback)))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id _hook)
                         (when (and first (eq failure 'startup))
                           (error "Fixture startup failure"))
                         (push id ids)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-session-test--peer)
                               :cwd root :mcp mcp :session-id id :meta '((echoAllText . t))))))
              (mevedel--insert-user-turn "First attempt")
              (mevedel--send-request "First attempt")
              (when (eq failure 'abort) (mevedel-abort buffer))
              (with-timeout (5 (ert-fail "Failed startup did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (setq first nil)
              (when reopen
                (let ((path (mevedel-session-save-path session)))
                  (test-mevedel-session-persistence--release-and-kill buffer session)
                  (setq restored (mevedel-session-persistence-restore path nil nil workspace)))
                (setq restored-session (buffer-local-value 'mevedel--session restored)))
              (with-current-buffer (or restored buffer)
                (setq-local gptel-use-context nil gptel-tools nil)
                (goto-char (point-max))
                (mevedel--insert-user-turn "Retry after failed startup")
                (mevedel--send-request "Retry after failed startup")
                ;; A preparation callback from the old request cannot launch
                ;; or affect the replacement, including after buffer teardown.
                (when late-context (funcall late-context "Late context"))
                (with-timeout (5 (ert-fail "Startup retry did not settle"))
                  (while (mevedel-turn-busy-p (current-buffer))
                    (accept-process-output nil 0.01)))
                (let* ((text (buffer-string)) (answer (string-search "answer:" text)))
                  (should answer)
                  (should (string-search "Retry after failed startup" (substring text answer))))
                (should-not (string-search "answer:First attempt" (buffer-string)))
                (should (= 2 (mevedel-session-turn-count mevedel--session))))
              (should (equal '(nil) ids)))
          (when (buffer-live-p restored)
            (test-mevedel-session-persistence--release-and-kill restored restored-session)))))))

(mevedel-deftest mevedel--send-request/claude-outcome (:quiet t)
  ,test
  (test)

  :doc "turn totals normalize cache writes and ignore duplicate terminal replies"
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude")))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Usage fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (lambda (_system mcp _model _effort &optional id _hook)
                   (list :command (executable-find "python3")
                         :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp :session-id id
                         :normalize-outcome #'mevedel-claude-code--outcome
                         :meta '((promptResponse .
                                  ((stopReason . "end_turn")
                                   (usage . ((inputTokens . 100) (outputTokens . 23)
                                             (cachedReadTokens . 1000) (cachedWriteTokens . 17)
                                             (totalTokens . 1140)))
                                   (_meta . ((quota . ((token_count . ((totalTokens . 1140)))
                                                      (model_usage . [((token_count . ((totalTokens . 8000))))]))))))))))))
        (mevedel--send-request "hello")
        (with-timeout (5 (ert-fail "Usage turn did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        (should (equal '(:input 117 :output 23 :cached 1000 :cache 17)
                       (plist-get (mevedel-engine-info request) :tokens-full)))
        (should (= 1 (mevedel-session-turn-count session))))))

  :doc "terminal failure metadata is not mistaken for a successful end turn"
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude")))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Failure fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (lambda (_system mcp _model _effort &optional id _hook)
                   (list :command (executable-find "python3")
                         :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp :session-id id
                         :normalize-outcome #'mevedel-claude-code--outcome
                         :meta '((promptResponse .
                                  ((stopReason . "end_turn")
                                   (_meta . ((jetbrains . ((air . ((version . 1)
                                                                   (sessionFailure . ((severity . "error")
                                                                                      (title . "Subscription exhausted"))))))))))))))))
        (mevedel--send-request "hello")
        (with-timeout (5 (ert-fail "Failed turn did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
        (should (equal "Subscription exhausted" (plist-get (mevedel-engine-info request) :error)))
        (should (eq 'uncertain (plist-get (cdar (mevedel-session-external-conversations session)) :state)))))))

(mevedel-deftest mevedel--send-request/claude-prompt (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude")))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Projection fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (lambda (_system mcp _model _effort &optional id _hook)
                   (list :command (executable-find "python3")
                         :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp :session-id id))))
        (insert "hello"
                (mevedel-tool-render-data-format '(:type prompt-summary :body "display-only"))
                (mevedel--format-hook-audit-record '(:type guest-prompt :name "audit-only")))
        (mevedel--send-request)
        (with-timeout (5 (ert-fail "Prompt projection did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        ;; Publication rewrites the Org property drawer, so inspect the
        ;; canonical response span rather than a pre-publication offset.
        (save-excursion
          (goto-char (point-min))
          (let* ((span (text-property-search-forward 'gptel 'response t))
                 (answer (buffer-substring-no-properties
                          (prop-match-beginning span) (prop-match-end span))))
            (should (string-search "answer:hello" answer))
            (should-not (string-search "display-only" answer))
            (should-not (string-search mevedel--hook-audit-open answer))))))))

(mevedel-deftest mevedel-session-naming-consider/claude (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let ((gptel--known-backends nil)
          (mevedel-model-workloads '((naming :provider "Claude Code:haiku" :effort nil)))
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          systems models tool-rosters)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Root-only secret policy"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
      (setf (mevedel-session-naming-state session) 'pending)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (lambda (system mcp model _effort &optional id _hook)
                   (push system systems) (push model models) (push mcp tool-rosters)
                   (should-not id)
                   (list :command (executable-find "python3")
                         :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp))))
        (mevedel-session-naming-consider session "Fix parsing")
        (with-timeout (5 (ert-fail "Subscription naming did not settle"))
          (while mevedel-session-naming--cancel (accept-process-output nil 0.01)))
        (should (equal "answer:Fix parsing" (mevedel-session-name session)))
        (should (equal '("haiku") models))
        (should (equal '([]) tool-rosters))
        (should-not (string-search "Root-only secret policy" (car systems)))
        (should-not mevedel--current-request)
        (should-not (mevedel-session-external-conversations session))
        (should (= 0 (mevedel-session-turn-count session)))))))

(mevedel-deftest mevedel-permission-review-start/claude (:quiet t)
  (dolist (valid '(t nil))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-permission-reviewer 'auto)
            (mevedel-model-workloads '((guardian :provider "Claude Code:haiku" :effort nil)))
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            models systems results fallback)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setf (mevedel-session-permission-mode session) 'edits)
        (setq-local gptel-system-prompt "Root-only secret policy")
        (insert "Evaluate (+ 1 2) in this Emacs session.\n")
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (system mcp model _effort &optional id _hook)
                     (push model models) (push system systems)
                     (should-not id) (should (equal [] mcp))
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-session-test--peer)
                           :cwd root :mcp mcp
                           :meta `((responseText . ,(if valid
                                                       "{\"decision\":\"allow-once\",\"reason\":\"Exact requested Eval\"}"
                                                     "invalid guardian response")))))))
          (mevedel-permission-review-start
           (list :kind 'eval :tool-name "Eval" :session session :origin "/root"
                 :data-buffer buffer :request request :request-id (mevedel-request-id request)
                 :expression "(+ 1 2)" :mode "live" :permission-via 'mode
                 :callback (lambda (outcome) (push outcome results)))
           (lambda () (setq fallback t)))
          (with-timeout (5 (ert-fail "Subscription guardian did not settle"))
            (while mevedel-permission-review--pending (accept-process-output nil 0.01)))
          (should (equal '("haiku") models))
          (should-not (string-search "Root-only secret policy" (car systems)))
          (should-not (mevedel-session-permission-rules session))
          (if valid
              (progn (should (equal '(allow-once) results))
                     (should-not fallback))
            (should-not results)
            (should fallback))
          (should (eq request mevedel--current-request)))))))

(mevedel-deftest mevedel-context-summary-generate/claude (:quiet t)
  (dolist (valid '(t nil))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-model-context-limit 64000)
            (mevedel-model-workloads '((journal :provider "Claude Code:haiku" :effort nil)))
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            models results)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Root-only secret policy")
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (system mcp model _effort &optional id _hook)
                     (push model models)
                     (should-not id) (should (equal [] mcp))
                     (should-not (string-search "Root-only secret policy" system))
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-session-test--peer)
                           :cwd root :mcp mcp
                           :normalize-outcome #'mevedel-claude-code--outcome
                           :meta `((responseText . ,(if valid
                                                       "## Done\n- Fixed parser.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"
                                                     "invalid summary"))
                                   (promptResponse . ((stopReason . "end_turn")
                                                      (usage . ((inputTokens . 11) (cachedWriteTokens . 6)
                                                                (cachedReadTokens . 20) (outputTokens . 3))))))))))
          (mevedel-context-summary-generate
           "Frozen source evidence" 'digest
           (lambda (result) (should (eq buffer (current-buffer))) (push result results))
           :session session)
          (with-timeout (5 (ert-fail "Subscription digest did not settle"))
            (while (not results) (accept-process-output nil 0.01)))
          (should (equal '("haiku") models))
          (should (= 1 (length results)))
          (should (eq (if valid 'success 'error) (plist-get (car results) :outcome)))
          (should (= 17 (plist-get (car results) :input-tokens)))
          (should (= 20 (plist-get (car results) :cached-tokens)))
          (should (= 3 (plist-get (car results) :output-tokens)))
          (should (eq request mevedel--current-request))
          (should-not (mevedel-session-external-conversations session)))))))

(mevedel-deftest mevedel--start-directive-discussion/claude (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((file (file-name-concat root "directive.txt"))
           (source (find-file-noselect file))
           (gptel--known-backends nil)
           (mevedel-show-chat-buffer nil)
           (inhibit-interaction t)
           (gptel--known-presets (copy-tree gptel--known-presets))
           (mevedel-preset--registry (copy-tree mevedel-preset--registry))
           (mevedel-claude-code-directory (file-name-concat root "claude"))
           (prompt-response '((stopReason . "end_turn")))
           launches directive record batches (callbacks 0))
      (unwind-protect
          (progn
            (mevedel-tools-register)
            (mevedel--define-presets)
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Directive fixture" gptel-tools nil)
            (with-current-buffer source
              (setq-local mevedel--workspace workspace)
              (insert "alpha\n") (save-buffer)
              (setq directive (mevedel--create-directive-in
                               source (point-min) (1- (point-max)) nil "DISCUSS LOCAL"))
              (setq record (mevedel--directive-record directive))
              (setf (mevedel-directive-session-id record) (mevedel-session-session-id session))
              (overlay-put directive 'mevedel-directive-model-provider "Claude Code:haiku"))
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (system mcp model effort &optional id hook)
                         (push (list system model effort id) launches)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-session-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :control #'mevedel-claude-code--control
                               :observe #'mevedel-claude-code-context-observe
                               :normalize-outcome #'mevedel-claude-code--outcome
                               :meta `((hookCommand . ,hook) (toolBatches . ,batches)
                                       (promptResponse . ,prompt-response))))))
              (cl-labels ((wait ()
                            (with-timeout (5 (ert-fail "Directive ACP turn did not settle"))
                              (while (mevedel-turn-busy-p buffer)
                                (accept-process-output nil 0.01))))
                          (done (err owner)
                            (should-not err)
                            (should (mevedel-request-p owner))
                            (cl-incf callbacks)))
                (goto-char (point-max))
                (mevedel--insert-user-turn "ROOT PRIVATE")
                (mevedel--send-request "ROOT PRIVATE") (wait)
                (with-current-buffer source
                  (mevedel--start-directive-discussion directive #'done))
                (wait)
                (ert-info ((format "Directive diagnostics: %s" mevedel-test--captured))
                  (should (= 1 callbacks)))
                (should-not (nth 3 (car launches)))
                (should (equal "haiku" (nth 1 (car launches))))
                (should (equal "sonnet" (gptel--model-name gptel-model)))
                (let ((answer (mevedel-directive-discussion-turn-result
                               (car (mevedel-directive-discussion record)))))
                  (should (string-search "DISCUSS LOCAL" answer))
                  (should-not (string-search "ROOT PRIVATE" answer)))
                (with-current-buffer source
                  (mevedel--discuss-directive-turn directive "SECOND LOCAL" nil #'done))
                (wait)
                (should (= 2 callbacks))
                (should-not (nth 3 (car launches)))
                (should (equal "haiku" (nth 1 (car launches))))
                (should (equal "sonnet" (gptel--model-name gptel-model)))
                (let ((answer (mevedel-directive-discussion-turn-result
                               (car (last (mevedel-directive-discussion record))))))
                  (should (string-search "DISCUSS LOCAL" answer))
                  (should (string-search "SECOND LOCAL" answer))
                  (should-not (string-search "ROOT PRIVATE" answer)))
                (goto-char (point-max))
                (mevedel--insert-user-turn "ROOT FOLLOWUP")
                (mevedel--send-request "ROOT FOLLOWUP") (wait)
                (should (equal "fixture-session" (nth 3 (car launches))))
                (should (= 4 (mevedel-session-turn-count session)))
                (should (= 2 (length (mevedel-session-external-conversations session))))
                (should-not mevedel--current-directive-uuid)
                (setq prompt-response
                      '((stopReason . "end_turn")
                        (_meta . ((jetbrains . ((air . ((version . 1)
                                                        (sessionFailure . ((severity . "error")
                                                                           (title . "Subscription exhausted")))))))))))
                (let (error-result)
                  (with-current-buffer source
                    (mevedel--discuss-directive-turn
                     directive "Fail this turn" nil
                     (lambda (err _owner) (push err error-result))))
                  (wait)
                  (should (equal '("Subscription exhausted") error-result))
                  (should (eq 'error (mevedel-directive-discussion-turn-outcome
                                      (car (last (mevedel-directive-discussion record)))))))
                (setq prompt-response '((stopReason . "end_turn")))
                (let (error-result)
                  (let ((owner
                         (with-current-buffer source
                           (mevedel--discuss-directive-turn
                            directive "Cancel this turn" nil
                            (lambda (err _owner) (push err error-result))))))
                    (mevedel-request-cancel owner))
                  (wait)
                  (should (equal '(abort) error-result))
                  (should (eq 'aborted (mevedel-directive-discussion-turn-outcome
                                        (car (last (mevedel-directive-discussion record)))))))
                ;; Even a custom discussion preset exposing a mutator cannot
                ;; grant it authority over the captured read-only request.
                (setq batches
                      (vector (vector
                               `((name . "ApplyPatch") (id . "directive-patch")
                                 (args . ((patch . ,(format "*** Begin Patch\n*** Update File: %s\n@@\n-alpha\n+changed\n*** End Patch" file))))))))
                (let ((mevedel-action-preset-alist
                       '((implement . mevedel-implement) (discuss . mevedel-implement))))
                  (with-current-buffer source
                    (mevedel--discuss-directive-turn directive "Read only" nil #'done)))
                (wait)
                (should (= 3 callbacks))
                (should (equal "alpha\n" (with-temp-buffer (insert-file-contents file) (buffer-string))))
                (let ((answer (mevedel-directive-discussion-turn-result
                               (car (last (mevedel-directive-discussion record))))))
                  (should (string-match-p "denied\\|Denied" answer)))
                (with-current-buffer source (mevedel--implement-discussion directive #'done))
                (wait)
                (should (= 4 callbacks))
                (should (equal "changed\n" (with-temp-buffer (insert-file-contents file) (buffer-string))))
                (let ((attempt (car (mevedel-directive-attempts record))))
                  (should (eq 'success (mevedel-directive-attempt-outcome attempt)))
                  (should (string-search "+changed" (mevedel-directive-attempt-patch attempt)))
                  (should (eq 'complete (mevedel-directive-attempt-capture attempt))))
                (should (= 8 (mevedel-session-turn-count session)))
                (should (= 8 (plist-get
                              (car (read-from-string
                                    (mevedel-session-artifacts-read-artifact session "session.meta.el" t)))
                              :total-turn-count)))
                (should-not (string-search "Session auto-save failed" mevedel-test--captured))
                (should-not (string-search "FSM handler" mevedel-test--captured)))))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))))))

(mevedel-deftest mevedel--send-request/independent-directive-history (:quiet t)
  (mevedel-engine-test--with-session
    (let* ((gptel--known-backends nil)
           (api (gptel-make-openai "fixture-api" :key "fixture" :models '(gpt-4o)))
           (provider (list :backend api :model 'gpt-4o))
           (real-request (symbol-function 'gptel-request))
           called)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider session provider buffer)
      (setf (mevedel-session-turn-count session) 1
            (mevedel-session-prompt-index session) '((1 (:kind directive :cum-turn 1)))
            (mevedel-session-external-conversations session)
            (list (list "directive-id" :engine 'claude-code :id "isolated-id"
                        :host (system-name) :directory root :state 'ready)))
      (setq-local gptel-tools nil gptel-system-prompt "Root system"
                  gptel-prompt-transform-functions nil)
      (insert "First ordinary root prompt")
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (prompt &rest options)
                   (setq called t)
                   (apply real-request prompt (append options '(:dry-run t))))))
        (mevedel--send-request))
      (should called)
      ;; Completed directive turns are not evidence of root model history.
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (should (mevedel-claude-code-backend-p gptel-backend))
      (setf (mevedel-session-turn-count session) 2
            (mevedel-session-prompt-index session)
            '((1 (:kind directive :cum-turn 1) (:cum-turn 2))))
      (mevedel-model-set-session-provider session provider buffer)
      (should (eq api gptel-backend)))))

(mevedel-deftest mevedel-directive-plan-start/claude (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((source (find-file-noselect (file-name-concat root "planned.txt")))
           (view (generate-new-buffer " *claude-directive-plan*"))
           (gptel--known-backends nil)
           (gptel--known-presets (copy-tree gptel--known-presets))
           (mevedel-preset--registry (copy-tree mevedel-preset--registry))
           (mevedel-show-chat-buffer nil) (inhibit-interaction t)
           (mevedel-claude-code-directory (file-name-concat root "claude"))
           (proposal "<proposed_plan>\n# Local plan\n\nChange the captured expression, then run its tests.\n</proposed_plan>")
           (launches 0) directive record result)
      (unwind-protect
          (progn
            (mevedel-tools-register) (mevedel--define-presets)
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (mevedel-view--setup view buffer)
            (with-current-buffer view
              (goto-char (mevedel-view--input-start))
              (insert "> ordinary draft\nkeep its second line"))
            (with-current-buffer source
              (setq-local mevedel--workspace workspace)
              (insert "alpha\n") (save-buffer)
              (setq directive (mevedel--create-directive-in
                               source (point-min) (1- (point-max)) nil "Implement local expression"))
              (setq record (mevedel--directive-record directive))
              (setf (mevedel-directive-session-id record) (mevedel-session-session-id session))
              (overlay-put directive 'mevedel-directive-model-provider "Claude Code:haiku"))
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp model _effort &optional id _hook)
                         (should-not id) (cl-incf launches)
                         (should (equal (if (= launches 1) "haiku" "sonnet") model))
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-session-test--peer)
                               :cwd root :mcp mcp
                               :meta (and (= launches 1) `((responseText . ,proposal)))))))
              (cl-labels ((wait ()
                            (with-timeout (5 (ert-fail "Directive Plan ACP turn did not settle"))
                              (while (mevedel-turn-busy-p buffer)
                                (accept-process-output nil 0.01)))))
                (with-current-buffer source
                  (mevedel-directive-plan-start
                   directive 'implement #'mevedel--implement-directive-prompt
                   (lambda (err owner) (setq result (list err owner)))))
                (wait)
                (should-not result)
                (should (eq 'proposed (plist-get (mevedel-directive-plan record) :status)))
                (let* ((entry (mevedel-session-pending-plan-approval session))
                       (selection (copy-tree (plist-get entry :selection))))
                  (should entry)
                  (setq selection (plist-put selection :model-provider "Claude Code:sonnet"))
                  (mevedel-plan-approval-settle entry (list :accept t :selection selection)))
                (wait)
                (ert-info ((format "Plan diagnostics: %s" mevedel-test--captured))
                  (should result) (should-not (car result)))
                (should (= 2 launches))
                (should (eq 'settled (plist-get (mevedel-directive-plan record) :status)))
                (should-not (mevedel-session-pending-plan-approval session))
                (should-not (mevedel-session-directive-planning session))
                (should (string-search "Local plan"
                                       (mevedel-directive-attempt-request
                                        (car (mevedel-directive-attempts record)))))
                (with-current-buffer view
                  (should (equal "> ordinary draft\nkeep its second line"
                                 (mevedel-view--input-text)))))))
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))))))

(mevedel-deftest mevedel-agent-control-spawn/claude (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (file (file-name-concat root "child-evidence.txt"))
          record invocation outcome launches replay)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "ROOT PRIVATE INSTRUCTIONS")
      (dolist (name '("Read" "SendMessage" "ListAgents"))
        (mevedel-tool-ensure name))
      (write-region "child evidence\n" nil file nil 'silent)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (system mcp model _effort &optional id _hook)
                       (push (list system model id) launches)
                       (list :command (executable-find "python3")
                             :args (list mevedel-claude-code-session-test--peer)
                             :cwd root :mcp mcp :session-id id
                             :tool-id-field :claudecode/toolUseId
                             :normalize-outcome #'mevedel-claude-code--outcome
                             :observe #'mevedel-claude-code-context-observe
                             :check-context #'mevedel-claude-code-context-check
                             :meta
                             `((responseText . "Child completed")
                               (promptResponse . ((stopReason . "end_turn")
                                                  (usage . ((inputTokens . 12)
                                                            (outputTokens . 4)
                                                            (cachedWriteTokens . 0)))))
                               (toolBatches . [[((name . "Read") (id . ,(if replay "child-read-1" (format "child-read-%d" (length launches))))
                                                (args . ((file_path . ,file))))]]))))))
            (mevedel-agent-control-spawn
             session "reader" "Inspect the assigned file."
             (lambda (value) (setq outcome value record (plist-get value :record)))
             :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                          :tools '(Read)
                                          :system-prompt "CHILD INSTRUCTIONS")
             :on-invocation (lambda (value) (setq invocation value)))
            (should-not (plist-get outcome :error))
            (should (eq 'success (plist-get outcome :outcome)))
            (with-timeout (5 (ert-fail "Claude child did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should (eq 'idle (mevedel-agent-record-activity record)))
            (should (equal "Child completed" (mevedel-agent-record-settled-result record)))
            (should-not (mevedel-agent-invocation-runtime-fsm invocation))
            (should (string-search "CHILD INSTRUCTIONS" (caar launches)))
            (should-not (string-search "ROOT PRIVATE INSTRUCTIONS" (caar launches)))
            (should (= 0 (mevedel-session-turn-count session)))
            (should (eq request mevedel--current-request))
            (with-current-buffer (mevedel-agent-record-conversation-buffer record)
              (should (string-search "child evidence" (buffer-string))))
            (let ((mail (mevedel-agent-control-context-mailbox session)))
              (should (= 1 (length mail)))
              (should (= 16 (plist-get (car mail) :usage))))
            (should (eq 'ready (plist-get
                               (alist-get "/root/reader" (mevedel-session-external-conversations session)
                                          nil nil #'equal) :state)))
            (should-not (assoc "root" (mevedel-session-external-conversations session)))
            (setq-local gptel-model 'haiku gptel-system-prompt "CHANGED ROOT")
            (mevedel-agent-control-followup session "/root/reader" "Inspect again.")
            (setq invocation (mevedel-agent-record-invocation record))
            (with-timeout (5 (ert-fail "Claude child follow-up did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should (eq 'idle (mevedel-agent-record-activity record)))
            (should (equal "Child completed" (mevedel-agent-record-settled-result record)))
            (should (equal "sonnet" (cadar launches)))
            (should (equal "fixture-session" (nth 2 (car launches))))
            (should-not (string-search "CHANGED ROOT" (caar launches)))
            (should (= 2 (length (mevedel-agent-control-context-mailbox session))))
            (mevedel-agent-control-followup session "/root/reader" "wait")
            (setq invocation (mevedel-agent-record-invocation record))
            (with-timeout (5 (ert-fail "Claude child did not begin waiting"))
              (while (not (with-current-buffer (mevedel-agent-invocation-buffer invocation)
                            (string-search "waiting" (buffer-string))))
                (accept-process-output nil 0.01)))
            (mevedel-agent-control-interrupt session "/root/reader")
            (with-timeout (5 (ert-fail "Claude child cancellation did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should (mevedel-agent-invocation-runtime-settled-p invocation))
            (should (eq 'idle (mevedel-agent-record-activity record)))
            (let ((mail (mevedel-agent-control-context-mailbox session)))
              (should (= 3 (length mail)))
              (should (eq 'interrupted (plist-get (car (last mail)) :outcome))))
            (should-not (mevedel-agent-control-active-turn-p session))
            (should (eq request mevedel--current-request))
            (should (= 0 (mevedel-session-turn-count session)))
            (mevedel-agent-control-followup session "/root/reader" "crash")
            (setq invocation (mevedel-agent-record-invocation record))
            (with-timeout (5 (ert-fail "Child transport death did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (let ((mail (mevedel-agent-control-context-mailbox session)))
              (should (= 4 (length mail)))
              (should (eq 'errored (plist-get (car (last mail)) :outcome))))
            (mevedel-agent-control-followup session "/root/reader" "Inspect after the crash.")
            (setq invocation (mevedel-agent-record-invocation record))
            (with-timeout (5 (ert-fail "Child recovery did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should (equal "Child completed" (mevedel-agent-record-settled-result record)))
            (with-current-buffer (mevedel-agent-record-conversation-buffer record)
              (should (= 2 (mevedel-engine-test--count-evidence "Reconcile current state before continuing"))))
            (setq replay t)
            (mevedel-agent-control-followup session "/root/reader" "Inspect again, without replay.")
            (setq invocation (mevedel-agent-record-invocation record))
            (with-timeout (5 (ert-fail "Child replay rejection did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should (eq 'error (plist-get (mevedel-engine-info invocation) :mevedel-acp-outcome)))
            (should (= 3 (length (plist-get
                                 (alist-get "/root/reader" (mevedel-session-external-conversations session)
                                            nil nil #'equal) :tool-calls))))
            (should (= 0 (mevedel-session-turn-count session)))
            (let (unpublished)
              (should-error
               (mevedel-agent-runtime-dispatch
                nil "reader" "Unpublished follow-up"
                :frozen-configuration (mevedel-agent-invocation-frozen-configuration invocation)
                :path "/root/reader"
                :retained-id (mevedel-agent-record-id record)
                :retained-buffer (mevedel-agent-record-conversation-buffer record)
                :retained-transcript (mevedel-agent-record-conversation-location record)
                :on-invocation (lambda (value)
                                 (setq unpublished value)
                                 (error "Fixture rejected publication"))))
              (should (mevedel-agent-invocation-runtime-settled-p unpublished))
              (should-not (mevedel-agent-invocation-runtime-cancel unpublished))))
        (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
          (kill-buffer (mevedel-agent-record-conversation-buffer record)))))))

(mevedel-deftest mevedel-agent-control-followup/claude-mail (:quiet t)
  (dolist (acknowledge '(t nil "foreign" "mismatch" "error" "malformed" "oversized" "initial" "initial-large"))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (file (file-name-concat root "mail-evidence.txt"))
            (mail (concat "Steer this child only."
                          (when (member acknowledge '("oversized" "initial-large"))
                            (make-string 5000 #x1f600))))
            (initial (member acknowledge '("initial" "initial-large")))
            (delivered (or (eq acknowledge t) (member acknowledge '("initial" "initial-large"))))
            record outcome)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (dolist (name '("Read" "SendMessage" "ListAgents")) (mevedel-tool-ensure name))
        (write-region "mail fixture\n" nil file nil 'silent)
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id hook)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-session-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :control #'mevedel-claude-code--control
                               :observe #'mevedel-claude-code-context-observe
                               :meta `((hookCommand . ,hook)
                                       (hookAcknowledgement . ,(or acknowledge :false))
                                       (afterHookTool . ((name . "SendMessage")
                                                        (args . ((target . "/root/reader")
                                                                 (message . "Late mail remains.")))))
                                       (responseText . "Child completed")
                                       (toolBatches . ,(vector
                                                        (vconcat
                                                         (unless initial
                                                           (vector `((name . "SendMessage") (id . "queue-mail")
                                                                     (args . ((target . "/root/reader")
                                                                              (message . ,mail))))))
                                                         (vector `((name . "Read") (id . "mail-read")
                                                                   (args . ((file_path . ,file)))))))))))))
              (mevedel-agent-control-spawn
               session "reader" "Inspect the assigned file."
               (lambda (value) (setq outcome value record (plist-get value :record)))
               :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                            :tools '(Read SendMessage) :system-prompt "Child instructions")
               :on-invocation
               (lambda (_invocation)
                 (when initial
                   (mevedel-agent-control-followup session "/root/reader" mail))))
              (should-not (plist-get outcome :error))
              (with-timeout (5 (ert-fail "Mail delivery child did not settle"))
                (while (mevedel-agent-control-active-turn-p session)
                  (accept-process-output nil 0.01)))
              (should (equal "Child completed" (mevedel-agent-record-settled-result record)))
              (should (= (if delivered 1 2) (length (mevedel-agent-record-mailbox record))))
              (should (cl-some (lambda (message) (equal "Late mail remains." (plist-get message :payload)))
                               (mevedel-agent-record-mailbox record)))
              (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                (should (= (if delivered 1 0)
                           (how-many "<!-- mevedel-delivery:" (point-min) (point-max)))))
              (should-not (string-search "Steer this child only" (buffer-string))))
          (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
            (kill-buffer (mevedel-agent-record-conversation-buffer record))))))))

(mevedel-deftest mevedel--send-request/claude-initial-mail (:quiet t)
  (dolist (acknowledge '(t nil "foreign" "mismatch" "nested"))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude")))
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Session instructions" gptel-tools nil)
        (mevedel-agent-control-send-message
         session "/root" (concat "Large initial mail " (make-string 12000 ?x) " END-MAIL"))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (_system mcp _model _effort &optional id _hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-session-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :observe #'mevedel-claude-code-context-observe
                           :meta `((promptAcknowledgement . ,(or acknowledge :false))
                                   (responseText . "Complete"))))))
          (insert "Process my mail")
          (mevedel--send-request "Process my mail")
          (with-timeout (5 (ert-fail "Initial mail turn did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (should (= (if (eq acknowledge t) 0 1) (length (mevedel-session-messages session))))
          (should (= (if (eq acknowledge t) 1 0)
                     (how-many "Large initial mail" (point-min) (point-max))))
          (should (= (if (eq acknowledge t) 1 0)
                     (how-many "END-MAIL" (point-min) (point-max))))
          (unless acknowledge
            ;; A fresh turn must be able to deliver mail whose earlier receipt
            ;; was lost, without inheriting that turn's pending receipt state.
            (setq acknowledge t)
            (goto-char (point-max))
            (insert "\nTry my mail again")
            (mevedel--send-request "Try my mail again")
            (with-timeout (5 (ert-fail "Retried mail turn did not settle"))
              (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
            (should-not (mevedel-session-messages session))
            (should (= 1 (how-many "END-MAIL" (point-min) (point-max))))))))))

(mevedel-deftest mevedel--send-request/claude-context-updates (:quiet t)
  (dolist (receipt '(t nil mismatch unchanged compact revert final pending-mail))
    (mevedel-engine-test--with-session
      (let* ((gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
             (mevedel-system-retained-components '(memory))
             (state "Original memory")
             (changed (if (eq receipt 'unchanged) state "UPDATED-MEMORY-6492"))
             (file (file-name-concat root "evidence.txt"))
             (chunks 0))
        (setf (alist-get 'memory mevedel-system--prompt-components)
              (mevedel-system-prompt-component--create
               :name 'memory :producer (lambda (_context) state)))
        (write-region "Evidence" nil file nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Context update fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (when (eq receipt 'pending-mail)
          (mevedel-agent-control-send-message session "/root" "Unacknowledged initial mail"))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (_system mcp _model _effort &optional id hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-session-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :tool-id-field :claudecode/toolUseId
                           :control #'mevedel-claude-code--control
                           :check-context #'mevedel-claude-code-context-check
                           :observe (lambda (owner notification)
                                      (mevedel-claude-code-context-observe owner notification)
                                      (when (equal "agent_message_chunk"
                                                   (alist-get 'sessionUpdate (alist-get 'update (alist-get 'params notification))))
                                        (setq state (if (and (eq receipt 'revert) (> (cl-incf chunks) 1))
                                                        "Original memory" changed))))
                           :meta `((hookCommand . ,hook)
                                   (promptAcknowledgement . ,(if (eq receipt 'pending-mail) :false t))
                                   (compactBeforeBatch . ,(when (eq receipt 'compact) 2))
                                   (hookAcknowledgement . ,(pcase receipt ((or 'nil 'final) :false) ('mismatch "mismatch") (_ t)))
                                   (responseText . "Done")
                                   (toolBatches . ,(vconcat
                                                   (mapcar (lambda (id)
                                                             (vector `((name . "Read") (id . ,id)
                                                                       (args . ((file_path . ,file))))))
                                                           (if (eq receipt 'final) '("before-update" "changed")
                                                             '("before-update" "changed" "after-update"))))))))))
          (insert "Read with current context")
          (mevedel--send-request "Read with current context")
          (with-timeout (5 (ert-fail "Context update turn did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (ert-info ((format "receipt=%S" receipt))
            (should (eq (if (memq receipt '(t unchanged compact revert)) 'success 'error)
                        (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
            (should (= (if (memq receipt '(t unchanged compact revert)) 3 2)
                       (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (pcase receipt ((or 't 'revert) 1) ('compact 2) (_ 0))
                       (how-many "UPDATED-MEMORY-6492" (point-min) (point-max))))
            (should (= (if (eq receipt 'revert) 1 0)
                       (how-many "Original memory" (point-min) (point-max))))))))))

(mevedel-deftest mevedel-agent-control-spawn/claude-context-updates (:quiet t)
  (mevedel-engine-test--with-session
    (let* ((gptel--known-backends nil)
           (mevedel-claude-code-directory (file-name-concat root "claude"))
           (mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
           (mevedel-system-retained-components '(memory active-goal))
           (state "Original child memory")
           (file (file-name-concat root "evidence.txt"))
           record outcome)
      (setf (alist-get 'memory mevedel-system--prompt-components)
            (mevedel-system-prompt-component--create :name 'memory :producer (lambda (_) state))
            (alist-get 'active-goal mevedel-system--prompt-components)
            (mevedel-system-prompt-component--create :name 'active-goal :producer (lambda (_) "ROOT-ONLY-GOAL")))
      (write-region "Evidence" nil file nil 'silent)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (mevedel-tool-ensure "Read")
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (system mcp _model _effort &optional id hook)
                       (should (string-search "Original child memory" system))
                       (should-not (string-search "ROOT-ONLY-GOAL" system))
                       (list :command (executable-find "python3")
                             :args (list mevedel-claude-code-session-test--peer)
                             :cwd root :mcp mcp :session-id id
                             :tool-id-field :claudecode/toolUseId
                             :control #'mevedel-claude-code--control
                             :check-context #'mevedel-claude-code-context-check
                             :observe (lambda (owner notification)
                                        (mevedel-claude-code-context-observe owner notification)
                                        (when (equal "agent_message_chunk"
                                                     (alist-get 'sessionUpdate (alist-get 'update (alist-get 'params notification))))
                                          (setq state "CHILD-MEMORY-7258")))
                             :meta `((hookCommand . ,hook) (responseText . "Child completed")
                                     (toolBatches . [[((name . "Read") (id . "before-change") (args . ((file_path . ,file))))]
                                                    [((name . "Read") (id . "after-change") (args . ((file_path . ,file))))]]))))))
            (mevedel-agent-control-spawn
             session "reader" "Inspect the file."
             (lambda (value) (setq outcome value record (plist-get value :record)))
             :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                          :tools '(Read) :context-components '(memory)
                                          :system-prompt "Child instructions"))
            (should-not (plist-get outcome :error))
            (with-timeout (5 (ert-fail "Context update child did not settle"))
              (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
            (should (equal "Child completed" (mevedel-agent-record-settled-result record)))
            (with-current-buffer (mevedel-agent-record-conversation-buffer record)
              (should (= 1 (how-many "CHILD-MEMORY-7258" (point-min) (point-max))))
              (should-not (string-search "ROOT-ONLY-GOAL" (buffer-string))))
            (should-not (string-search "CHILD-MEMORY-7258" (buffer-string))))
        (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
          (kill-buffer (mevedel-agent-record-conversation-buffer record)))))))

(mevedel-deftest mevedel--send-request/claude-path-instructions (:quiet t)
  (dolist (receipt '(t nil late))
    (mevedel-engine-test--with-session
      (let* ((gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (directory (file-name-concat root "lib"))
             (file (file-name-concat directory "evidence.txt"))
             (instructions (file-name-concat directory "AGENTS.md"))
             (late-commits 0) queued)
        (make-directory directory)
        (write-region "Evidence" nil file nil 'silent)
        (write-region "PATH-GUIDANCE-4319" nil instructions nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Path instructions fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (_system mcp _model _effort &optional id hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-session-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :tool-id-field :claudecode/toolUseId
                           :control #'mevedel-claude-code--control
                           :check-context #'mevedel-claude-code-context-check
                           :observe (lambda (owner notification)
                                      (when (and (eq receipt 'late) (not queued)
                                                 (equal "hook_response"
                                                        (alist-get 'subtype (alist-get 'message (alist-get 'params notification)))))
                                        (setq queued t)
                                        ;; Replace an event already in flight.  Its receipt
                                        ;; must not consume the newer coalesced event.
                                        (mevedel-reminders-queue-turn-event
                                         buffer (cons 'workspace-instructions instructions)
                                         "LATE-GUIDANCE-3928" (lambda () (cl-incf late-commits))))
                                      (mevedel-claude-code-context-observe owner notification))
                           :meta `((hookCommand . ,hook) (hookAcknowledgement . ,(if receipt t :false))
                                   (responseText . "Done")
                                   (toolBatches . [[((name . "Read") (id . "first-read") (args . ((file_path . ,file))))]
                                                  [((name . "Read") (id . "second-read") (args . ((file_path . ,file))))]]))))))
          (insert "Read with applicable instructions")
          (mevedel--send-request "Read with applicable instructions")
          (with-timeout (5 (ert-fail "Path instructions turn did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (ert-info ((format "receipt=%S" receipt))
            (should (eq (if receipt 'success 'error)
                        (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
            (should (= (if receipt 2 1) (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (if receipt 1 0) (mevedel-engine-test--count-evidence "PATH-GUIDANCE-4319")))
            (should (eq (not (null receipt)) (not (null (mevedel-session-workspace-instruction-hashes session)))))
            (should (= (if (eq receipt 'late) 1 0) late-commits))
            (should (= late-commits (mevedel-engine-test--count-evidence "LATE-GUIDANCE-3928")))))))))

(mevedel-deftest mevedel--send-request/claude-compaction (:quiet t)
  (dolist (receipt '(t nil unchanged final))
    (mevedel-engine-test--with-session
      (let* ((gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
             (mevedel-system-retained-components '(memory))
             (state "Original memory")
             (changed (if (eq receipt 'unchanged) state "CURRENT-CONTEXT-3726"))
             (file (file-name-concat root "evidence.txt"))
             captured-system)
        (setf (alist-get 'memory mevedel-system--prompt-components)
              (mevedel-system-prompt-component--create
               :name 'memory :producer (lambda (_context) state)))
        (write-region "Evidence" nil file nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt (concat (make-string 12000 ?x) " LARGE-SYSTEM-END")
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (system mcp _model _effort &optional id hook)
                     (setq captured-system system)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-session-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :tool-id-field :claudecode/toolUseId
                           :control #'mevedel-claude-code--control
                           :check-context #'mevedel-claude-code-context-check
                           :observe (lambda (owner notification)
                                      (mevedel-claude-code-context-observe owner notification)
                                      (when (equal "agent_message_chunk"
                                                   (alist-get 'sessionUpdate (alist-get 'update (alist-get 'params notification))))
                                        (setq state changed)))
                           :meta `((hookCommand . ,hook) (compactBeforeBatch . 1)
                                   (hookAcknowledgement . ,(if (memq receipt '(nil final)) :false t))
                                   (responseText . "Done")
                                   (toolBatches . [[((name . "Read") (id . "before-compact")
                                                    (args . ((file_path . ,file))))]
                                                  ,(if (eq receipt 'final) []
                                                     (vector `((name . "Read") (id . "after-compact")
                                                               (args . ((file_path . ,file))))))]))))))
          (insert "Read before and after compaction")
          (mevedel--send-request "Read before and after compaction")
          (with-timeout (5 (ert-fail "Compaction turn did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (should (string-search "LARGE-SYSTEM-END" captured-system))
          (should (string-search "Original memory" captured-system))
          (should (eq (if (memq receipt '(t unchanged)) 'success 'error)
                      (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
          (ert-info ((format "receipt=%S" receipt))
            (should (= (if (memq receipt '(t unchanged)) 2 1)
                       (how-many "^#\\+begin_tool" (point-min) (point-max)))))
          (should (= (if (eq receipt t) 1 0)
                     (how-many "CURRENT-CONTEXT-3726" (point-min) (point-max)))))))))

(mevedel-deftest mevedel--send-request/claude-recovery (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (file (file-name-concat root "effects.txt"))
          (crash t) (receipt t) (call-id "mutate-once")
          restored restored-session)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Recovery fixture"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Bash"))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (_system mcp _model _effort &optional id _hook)
                       (list :command (executable-find "python3")
                             :args (list mevedel-claude-code-session-test--peer)
                             :cwd root :mcp mcp :session-id id
                             :tool-id-field :claudecode/toolUseId
                             :observe #'mevedel-claude-code-context-observe
                             :check-context #'mevedel-claude-code-context-check
                             :meta `((crashAfterTool . ,(if crash t :false))
                                     (promptAcknowledgement . ,(if receipt t :false))
                                     (responseText . "Done")
                                     (toolBatches . [[((name . "Bash") (id . ,call-id)
                                                      (args . ((command . ,(concat "printf 'effect\n' >> "
                                                                                   (shell-quote-argument file))))))]]))))))
            (insert "Append one line")
            (mevedel--send-request "Append one line")
            (with-timeout (5 (ert-fail "Crashing turn did not settle"))
              (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
            (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
            (should (equal "effect\n" (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (let* ((path (mevedel-session-save-path session))
                   (metadata (car (read-from-string
                                   (mevedel-session-artifacts-read-artifact session "session.meta.el" t))))
                   (history (cdr (assoc "root" (plist-get metadata :external-conversations)))))
              (should (equal '(("mutate-once" . "Bash")) (plist-get history :tool-calls)))
              (test-mevedel-session-persistence--release-and-kill buffer session)
              (setq restored (mevedel-session-persistence-restore path nil nil workspace)))
            (with-current-buffer restored
              (setq restored-session mevedel--session crash nil)
              (mevedel-agent-control-send-message restored-session "/root" "RECOVERY-MAIL-7851")
              ;; This fixture installs its minimal tool roster directly rather
              ;; than through a persisted named preset.
              (setq-local gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Bash"))))
              (dolist (ack '(nil t))
                (setq receipt ack call-id (if ack "mutate-once" "unacknowledged-call"))
                (goto-char (point-max))
                (insert "\nInspect previous effects before continuing")
                (let ((owner (mevedel--send-request "Inspect previous effects before continuing")))
                  (with-timeout (5 (ert-fail "Recovery turn did not settle"))
                    (while (mevedel-turn-busy-p restored) (accept-process-output nil 0.01)))
                  (ert-info ((format "ack=%S history=%S pending=%S" ack
                                     (mevedel-session-external-conversations restored-session)
                                     (plist-get (mevedel-engine-info owner) :mevedel-claude-recovery-pending)))
                    (should (eq 'error (plist-get (mevedel-engine-info owner) :mevedel-acp-outcome)))))
                (should (equal "effect\n" (with-temp-buffer (insert-file-contents file) (buffer-string)))))
              (should (= 1 (mevedel-engine-test--count-evidence "Reconcile current state before continuing")))
              (should (= 1 (how-many "RECOVERY-MAIL-7851" (point-min) (point-max))))
              (should-not (mevedel-session-messages restored-session))
              (should (= 1 (length (plist-get (cdr (assoc "root" (mevedel-session-external-conversations restored-session)))
                                              :tool-calls))))))
        (when (buffer-live-p restored)
          (test-mevedel-session-persistence--release-and-kill restored restored-session))))))

(mevedel-deftest mevedel--send-request/claude-admission-failure (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (file (file-name-concat root "must-not-exist.txt"))
          (publish (symbol-function 'mevedel-session-artifacts-publish-sidecar-state)))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Admission fixture"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Bash"))))
      (cl-letf (((symbol-function 'mevedel-session-artifacts-publish-sidecar-state)
                 (lambda (owner root-buffer)
                   (if (plist-get (cdr (assoc "root" (mevedel-session-external-conversations owner))) :tool-calls)
                       (error "Fixture durable admission write failed")
                     (funcall publish owner root-buffer))))
                ((symbol-function 'mevedel-claude-code-launch)
                 (lambda (_system mcp _model _effort &optional id _hook)
                   (list :command (executable-find "python3")
                         :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp :session-id id :tool-id-field :claudecode/toolUseId
                         :meta `((toolBatches . [[((name . "Bash") (id . "never-admitted")
                                                  (args . ((command . ,(concat "printf effect > "
                                                                               (shell-quote-argument file))))))]]))))))
        (insert "Perform the requested write")
        (mevedel--send-request "Perform the requested write")
        (with-timeout (5 (ert-fail "Failed admission did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        (should-not (file-exists-p file))
        (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
        (should (string-search "Fixture durable admission write failed" (buffer-string)))))))

(mevedel-deftest mevedel--send-request/claude-initial-reminder-after-fork-point (:quiet t)
  (mevedel-engine-test--with-session
    (let* ((gptel--known-backends nil)
           (mevedel-claude-code-directory (file-name-concat root "claude"))
           (nested (file-name-concat root "nested"))
           (instructions (file-name-concat nested "AGENTS.md"))
           (file (file-name-concat nested "source.txt"))
           (turn 0))
      (make-directory nested t)
      (write-region "FIRST GUIDANCE" nil instructions nil 'silent)
      (write-region "source contents" nil file nil 'silent)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Root reminder reproduction"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (lambda (_system mcp _model _effort &optional id hook)
                   (cl-incf turn)
                   (list :command (executable-find "python3") :args (list mevedel-claude-code-session-test--peer)
                         :cwd root :mcp mcp :session-id id :tool-id-field :claudecode/toolUseId
                         :observe #'mevedel-claude-code-context-observe
                         :check-context #'mevedel-claude-code-context-check
                         :control #'mevedel-claude-code--control
                         :meta `((hookCommand . ,hook) (responseText . "Finished native turn")
                                 (toolBatches . ,(if (= turn 1)
                                                   `[[((name . "Read") (id . "first-read")
                                                       (args . ((file_path . ,file))))]] [])))))))
        (mevedel--insert-user-turn "Read my source and follow its guidance.")
        (mevedel--send-request "Read my source and follow its guidance.")
        (with-timeout (5 (ert-fail "First turn did not finish"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
        (should (= 1 (mevedel-engine-test--count-evidence "FIRST GUIDANCE")))
        (write-region "SECOND GUIDANCE RESTORED" nil instructions nil 'silent)
        (goto-char (point-max))
        (mevedel--insert-user-turn "Continue using my changed guidance.")
        (let ((second (mevedel--send-request "Continue using my changed guidance.")))
          (with-timeout (5 (ert-fail "Second turn did not finish"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (should (eq 'success (plist-get (mevedel-engine-info second) :mevedel-acp-outcome))))
        ;; Context was acknowledged by the native peer and is trusted audit data.
        (should (equal (secure-hash 'sha256 "SECOND GUIDANCE RESTORED")
                       (cdr (assoc (list "/root" instructions)
                                   (mevedel-session-workspace-instruction-hashes session)))))
        (should (cl-some (lambda (span)
                           (string-search "SECOND GUIDANCE RESTORED"
                                          (prin1-to-string (plist-get span :record))))
                         (mevedel-transcript-audit-buffer-spans 'injected-reminders)))
        ;; The canonical evidence and view/provider consumers must retain the body.
        (ert-info ((mevedel-engine-test--evidence))
          (should (= 1 (mevedel-engine-test--count-evidence "SECOND GUIDANCE RESTORED"))))))))

(provide 'test-mevedel-claude-code-session)
;;; test-mevedel-claude-code-session.el ends here
