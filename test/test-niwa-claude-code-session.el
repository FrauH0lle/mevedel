;;; test-niwa-claude-code-session.el --- Subscription session dispatch -*- lexical-binding: t -*-

;;; Commentary:
;; Select the subscription provider and send through the ordinary entry point.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'niwa-claude-code)
(require 'niwa-claude-code-context)
(require 'niwa-journal-store)

(niwa-deftest niwa-engine-test--with-claude-session (:quiet t)
  (dolist (file-p '(nil t))
    (niwa-engine-test--with-claude-session :file-workspace file-p
      (should (eq (if file-p 'file 'project) (niwa-workspace-type workspace)))
      (should (eq 'portable (niwa-session-authority-mode session)))
      (should (niwa-claude-code-backend-p gptel-backend))
      (should (file-directory-p (niwa-session-save-path session)))
      (niwa-session-artifacts-save session buffer)
      (should (niwa-session-artifacts-read-artifact session "session.meta.el" t)))))

(niwa-deftest niwa-claude-code-register ()
  (let ((gptel--known-backends nil))
    (niwa-claude-code-register)
    (let* ((provider (niwa-model-resolve-provider "Claude Code:sonnet"))
           (backend (plist-get provider :backend)))
      (should (niwa-claude-code-backend-p backend))
      (should (equal "sonnet" (gptel--model-name (plist-get provider :model))))
      (should-error (gptel--request-data backend nil) :type 'user-error))))

(niwa-deftest niwa--send-request/claude (:quiet t)
  (niwa-engine-test--with-claude-session
    (let (launches)
      (setq-local gptel-system-prompt "Session system fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (system mcp model effort &optional id _hook)
                    (push (list system model effort id) launches)
                    nil))))
        (insert "hello")
        (niwa--send-request "hello")
        (niwa-test--await 5 "Selected subscription turn did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (string-search "answer:hello" (buffer-string)))
        (should-not (niwa-request-fsm request))
        (should (= 1 (niwa-session-turn-count session)))
        (should (equal "sonnet" (cadar launches)))
        (should-not (nth 3 (car launches)))
        (let* ((encoded (niwa-session-codec-serialize session))
               (decoded (niwa-session-codec-deserialize encoded workspace))
               (restored (plist-get decoded :session)))
          (should (equal (niwa-session-external-conversations session)
                         (niwa-session-external-conversations restored))))
        (niwa-model-set-session-provider
         session (niwa-model-resolve-provider "Claude Code:opus") buffer)
        (goto-char (point-max))
        (insert "\nagain")
        (niwa--send-request "again")
        (niwa-test--await 5 "Retained subscription turn did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (= 2 (niwa-session-turn-count session)))
        (should (equal "opus" (cadar launches)))
        (should (equal "fixture-session" (nth 3 (car launches))))
        (should (string-search "answer:again" (buffer-string)))))))

(niwa-deftest niwa-view--forward-input-now/claude (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let ((view (generate-new-buffer " *claude-composer*")))
      (unwind-protect
          (progn
            (setq-local gptel-system-prompt "Composer fixture" gptel-tools nil)
            (niwa-chat-install-request-hooks)
            (niwa-view--setup view buffer)
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          nil))))
              (with-current-buffer view
                (niwa-view--forward-input-now "hello composer")
                (niwa-view-composer-insert "> Keep this draft\nand its second line"))
              (niwa-test--await 5 "Composer subscription request did not settle"
                (not (niwa-turn-busy-p buffer))))
            (should (= 1 (niwa-session-turn-count session)))
            (should (string-search "answer:hello composer" (buffer-string)))
            (with-current-buffer view
              (should (equal "> Keep this draft\nand its second line"
                             (niwa-view-composer-draft-text)))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-session-codec/external-history (:quiet t)
  (niwa-engine-test--with-session
    (setf (niwa-session-external-conversations session)
          (list (list "root" :engine 'claude-code :id "retained"
                      :host "fixture" :directory root :state 'in-flight
                      :input-boundary '(1 . 12))))
    (let* ((encoded (niwa-session-codec-serialize session))
           (restored (plist-get (niwa-session-codec-deserialize encoded workspace) :session)))
      (should (eq 'uncertain (plist-get (cdar (niwa-session-external-conversations restored)) :state)))
      (should (eq 'in-flight (plist-get (cdar (niwa-session-external-conversations session)) :state)))
      (should (equal '(1 . 12) (plist-get (cdar (niwa-session-external-conversations restored)) :input-boundary)))
      (dolist (boundary '("invalid" (1 2) (-1 . 2) (1 . -2)))
        (let ((bad (copy-tree encoded)))
          (plist-put (cdar (plist-get bad :external-conversations)) :input-boundary boundary)
          (should-error (niwa-session-codec-validate-current-sidecar bad))))
      (dolist (invalid '(("bad") (("root" :engine other))
                         (("root" :engine claude-code :state unstarted :id "fake"))
                         (("root" :engine claude-code :state unstarted :host "fixture"))
                         (("root" :engine claude-code :state unstarted :directory "/tmp"))
                         (("root" :engine claude-code :id "" :host "fixture"
                           :directory "/tmp" :state ready))))
        (should-error
         (niwa-session-codec-validate-current-sidecar
          (plist-put (copy-tree encoded) :external-conversations invalid)))))))

(niwa-deftest niwa--send-request/claude-publication (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((busy t))
      (setq-local gptel-system-prompt "Publication fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (_path) busy))
                ((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    nil))))
        (niwa--send-request "hello")
        (niwa-test--await 5 "History publication was not deferred"
          (gethash (list 'acp-prompt (niwa-request-id request))
                   niwa-transport--pending))
        (should (equal '(("root" :engine claude-code :state unstarted))
                       (niwa-session-external-conversations session)))
        (should-not (string-search "answer:hello" (buffer-string)))
        (setq busy nil)
        (niwa-test--await 5 "Deferred subscription prompt did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (string-search "answer:hello" (buffer-string)))
        (should (eq 'ready (plist-get (cdar (niwa-session-external-conversations session)) :state)))))))

(niwa-deftest niwa-engine-assert-local-history (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (setf (niwa-session-turn-count session) 1
          (niwa-session-external-conversations session)
          (list (list "root" :engine 'claude-code :id "retained" :host (system-name)
                      :directory root :state 'ready)))
    (should-error (niwa-session-rewind-assert-stable-source session buffer "Rewind")
                  :type 'user-error)
    (should-error (niwa-session-save-as-run session buffer "copy" "copy-id"
                                             (file-name-concat root "copy"))
                  :type 'user-error)
    (should-not (file-exists-p (file-name-concat root "copy")))
    (should-error (niwa-side-conversation-open "side question") :type 'user-error)
    (niwa-model-set-session-provider
     session (list :backend (gptel--make-backend :name "API" :models '(api)) :model 'api) buffer)
    (should-not (niwa-claude-code-backend-p gptel-backend))
    (should (equal "API:api" (niwa-session-model-provider session)))
    ;; Unstarted and released markers hold no native history.
    (setf (niwa-session-external-conversations session)
          (list (list "root" :engine 'claude-code :state 'unstarted)
                (list "/root/reader" :engine 'claude-code :state 'unstarted)))
    (should-not (niwa-engine-assert-local-history session "Transfer"))
    ;; A retained child blocks whole-session operations, not root-only ones.
    (setf (niwa-session-external-conversations session)
          (list (list "/root/reader" :engine 'claude-code :id "child" :host (system-name)
                      :directory root :state 'ready)))
    (should-error (niwa-engine-assert-local-history session "Transfer") :type 'user-error)
    (should-not (niwa-engine-assert-local-history session "/btw" nil t))))

(niwa-deftest niwa-engine-external-history-p ()
  (let ((session (niwa-session--create)))
    (should-not (niwa-engine-external-history-p nil))
    (setf (niwa-session-external-conversations session)
          '(("root" :engine claude-code :state unstarted)))
    (should-not (niwa-engine-external-history-p session))
    (setf (niwa-session-external-conversations session)
          '(("/root/reader" :engine claude-code :id "child" :state ready)))
    (should (niwa-engine-external-history-p session))
    (should-not (niwa-engine-external-history-p session t))
    (push '("root" :engine claude-code :id "root" :state uncertain)
          (niwa-session-external-conversations session))
    (should (niwa-engine-external-history-p session t))))

(niwa-deftest niwa-engine-record-history-edit/no-output (:quiet t)
  (dolist (phase '(active reopened))
    (niwa-engine-test--with-claude-session
      (let (receipt restored restored-session)
        (setq-local gptel-system-prompt "Silent fixture" gptel-tools nil)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'niwa-claude-code-launch)
                         (niwa-engine-test--claude-launch
                          (lambda (_system mcp _model _effort &optional id _hook)
                            (list :observe (lambda (_owner event)
                                             (when (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params event))))
                                               (setq receipt t))))))))
                (insert "wait-silent")
                (niwa--send-request "wait-silent")
                (niwa-test--await 5 "No submitted-input receipt" receipt))
              ;; Normal metadata saves may remove and replace properties.
              ;; They are not edits to the prompt retained by Claude.
              (gptel--save-state)
              (gptel--save-state)
              (should-not (eq 'diverged (plist-get (cdr (assoc "root" (niwa-session-external-conversations session))) :state)))
              (goto-char (point-max))
              (insert "\nnext drafX")
              (delete-char -1)
              (insert "t")
              (should-not (eq 'diverged (plist-get (cdr (assoc "root" (niwa-session-external-conversations session))) :state)))
              (when (eq phase 'reopened)
                (niwa-abort buffer)
                (niwa-test--await 5 "Silent abort did not settle"
                  (not (niwa-turn-busy-p buffer)))
                (let ((path (niwa-session-save-path session)))
                  (test-niwa-session-persistence--release-and-kill buffer session)
                  (setq restored (niwa-session-persistence-restore path nil nil workspace)
                        restored-session (buffer-local-value 'niwa--session restored))))
              (with-current-buffer (or restored buffer)
                (goto-char (point-min))
                (search-forward "wait-silent")
                (delete-char -1)
                (insert "X")
                (should (eq 'diverged (plist-get (cdr (assoc "root" (niwa-session-external-conversations niwa--session))) :state)))
                (when (niwa-turn-busy-p (current-buffer))
                  (niwa-abort (current-buffer))
                  (niwa-test--await 5 "Edited turn did not settle"
                    (not (niwa-turn-busy-p (current-buffer)))))
                (cl-letf (((symbol-function 'niwa-claude-code-launch)
                           (lambda (&rest _) (ert-fail "Diverged history launched an agent"))))
                  (should (string-search "recover-history"
                                         (error-message-string
                                          (should-error (niwa--send-request "continue") :type 'user-error)))))))
          (when (buffer-live-p restored)
            (test-niwa-session-persistence--release-and-kill restored restored-session)))))))

(niwa-deftest niwa--send-request/claude-reopen (:quiet t)
  (niwa-engine-test--with-claude-session
    (let (ids restored-buffer restored-session)
      (setq-local gptel-system-prompt "Reopen fixture" gptel-tools nil)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (push id ids)
                        nil))))
            (insert "hello")
            (niwa--send-request "hello")
            (niwa-test--await 5 "First persisted turn did not finish"
              (not (niwa-turn-busy-p buffer)))
            (let ((path (niwa-session-save-path session)))
              (test-niwa-session-persistence--release-and-kill buffer session)
              (setq restored-buffer (niwa-session-persistence-restore path nil nil workspace)))
            (should-not (eq buffer restored-buffer))
            (with-current-buffer restored-buffer
              (setq restored-session niwa--session)
              (should (niwa-claude-code-backend-p gptel-backend))
              (should (string-search "answer:hello" (buffer-string)))
              (goto-char (point-max))
              (insert "\nfollow-uX")
              (delete-char -1)
              (insert "p")
              (niwa--send-request "follow-up")
              (niwa-test--await 5 "Reopened external history did not finish"
                (not (niwa-turn-busy-p restored-buffer)))
              (should (string-search "answer:follow-up" (buffer-string)))
              (should (= 2 (niwa-session-turn-count restored-session)))
              ;; Editing displayed evidence cannot silently rewrite native history.
              (goto-char (point-min))
              (search-forward "answer:hello")
              (delete-char -1)
              (insert "X")
              (should (eq 'diverged (plist-get (cdr (assoc "root" (niwa-session-external-conversations restored-session))) :state)))
              (niwa-session-artifacts-save restored-session restored-buffer)
              (let ((path (niwa-session-save-path restored-session)))
                (test-niwa-session-persistence--release-and-kill restored-buffer restored-session)
                (setq restored-buffer (niwa-session-persistence-restore path nil nil workspace)))
              (with-current-buffer restored-buffer
                (setq restored-session niwa--session)
                (should (string-search "answer:hellX" (buffer-string)))
                (should (string-search "recover-history"
                                       (error-message-string
                                        (should-error (niwa--send-request "Must reconcile") :type 'user-error)))))
              (should (= 2 (length ids))))
            (should (equal '("fixture-session" nil) ids)))
        (when (buffer-live-p restored-buffer)
          (test-niwa-session-persistence--release-and-kill restored-buffer restored-session))))))

(niwa-deftest niwa--send-request/claude-startup-retry (:quiet t)
  (pcase-dolist (`(,failure ,reopen) '((abort nil) (abort t) (startup nil) (startup t)))
    (niwa-engine-test--with-claude-session
      (let ((file (file-name-concat root "selected.txt"))
            (first t) ids late-context restored restored-session)
        (write-region "Selected context" nil file nil 'silent)
        (setq-local gptel-system-prompt "Retry fixture" gptel-tools nil
                    gptel-use-context (and (eq failure 'abort) 'user)
                    gptel-context (list file)
                    gptel-context-string-function
                    (lambda (callback _sources) (setq late-context callback)))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          (when (and first (eq failure 'startup))
                            (error "Fixture startup failure"))
                          (push id ids)
                          (list :meta '((echoAllText . t)))))))
              (niwa--insert-user-turn "First attempt")
              (niwa--send-request "First attempt")
              (when (eq failure 'abort) (niwa-abort buffer))
              (niwa-test--await 5 "Failed startup did not settle"
                (not (niwa-turn-busy-p buffer)))
              (setq first nil)
              (when reopen
                (let ((path (niwa-session-save-path session)))
                  (test-niwa-session-persistence--release-and-kill buffer session)
                  (setq restored (niwa-session-persistence-restore path nil nil workspace)))
                (setq restored-session (buffer-local-value 'niwa--session restored)))
              (with-current-buffer (or restored buffer)
                (setq-local gptel-use-context nil gptel-tools nil)
                (goto-char (point-max))
                (niwa--insert-user-turn "Retry after failed startup")
                (niwa--send-request "Retry after failed startup")
                ;; A preparation callback from the old request cannot launch
                ;; or affect the replacement, including after buffer teardown.
                (when late-context (funcall late-context "Late context"))
                (niwa-test--await 5 "Startup retry did not settle"
                  (not (niwa-turn-busy-p (current-buffer))))
                (let* ((text (buffer-string)) (answer (string-search "answer:" text)))
                  (should answer)
                  (should (string-search "Retry after failed startup" (substring text answer))))
                (should-not (string-search "answer:First attempt" (buffer-string)))
                (should (= 2 (niwa-session-turn-count niwa--session))))
              (should (equal '(nil) ids)))
          (when (buffer-live-p restored)
            (test-niwa-session-persistence--release-and-kill restored restored-session)))))))

(niwa-deftest niwa--send-request/claude-tools-refresh (:quiet t)
  ,test
  (test)

  :doc "a session preset refreshes stale tools after re-registration"
  (niwa-engine-test--with-claude-session
    (niwa-tools-register)
    (niwa--define-presets)
    (setq-local niwa-preset-extra-tool-specs
                '((niwa-implement (:tool "Agent"))))
    (niwa-preset-apply 'niwa-implement)
    (let ((previous gptel-tools) offered)
      (niwa-tools-register)
      (should-not (niwa-tool-for-gptel (car previous)))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (&rest _args)
                    (setq offered (plist-get (niwa-engine-info niwa--current-request) :tools))
                    nil))))
        (niwa--insert-user-turn "Refresh registered tools")
        (niwa--send-request "Refresh registered tools")
        (niwa-test--await 5 "Refreshed roster did not settle"
          (not (niwa-turn-busy-p buffer))))
      (should (eq 'success (niwa-session-last-outcome session)))
      (should (equal (mapcar #'gptel-tool-name previous)
                     (mapcar #'gptel-tool-name offered)))
      (should (cl-every #'niwa-tool-for-gptel offered))
      (let ((agent (cl-find "Agent" gptel-tools :key #'gptel-tool-name :test #'equal)))
        (should agent)
        (should-not (eq agent (niwa-tool-gptel-tool (niwa-tool-get "Agent" "niwa"))))
        (should (equal (vconcat (mapcar #'car (niwa-agents-specs)))
                       (plist-get (cl-find "role" (gptel-tool-args agent)
                                           :key (lambda (arg) (plist-get arg :name))
                                           :test #'equal)
                                  :enum))))
      (should (eq 'niwa-implement (niwa-session-preset-name session)))))

  :doc "a raw request-only tool spec is not replaced by the session preset"
  (niwa-engine-test--with-claude-session
    (niwa-tools-register)
    (niwa--define-presets)
    (niwa-preset-apply 'niwa-implement)
    (let ((offered 'not-launched))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (&rest _args)
                    (setq offered (plist-get (niwa-engine-info niwa--current-request) :tools))
                    nil))))
        (niwa--insert-user-turn "No tools")
        (niwa-with-preset '(:tools nil)
          (niwa--send-request "No tools"))
        (niwa-test--await 5 "Raw request-only roster did not settle"
          (not (niwa-turn-busy-p buffer))))
      (should-not offered)
      (should (eq 'success (niwa-session-last-outcome session)))
      (should (eq 'niwa-implement (niwa-session-preset-name session))))))

(niwa-deftest niwa--send-request/claude-roster-failure (:quiet t)
  ,test
  (test)

  :doc "pre-ACP roster failures settle and permit an explicit retry"
  (niwa-engine-test--with-claude-session
    (let ((launches 0)
          (gptel-tools
           (list (gptel--make-tool :name "Read" :category "niwa"
                                   :function #'ignore))))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (&rest _args)
                    (cl-incf launches)
                    (list :meta '((echoAllText . t)))))))
        (niwa--insert-user-turn "Rejected roster")
        (should (equal '(error "Tool Read is not registered with niwa")
                       (should-error (niwa--send-request "Rejected roster"))))
        (niwa-test--await 5 "Roster failure left the request busy"
          (not (niwa-turn-busy-p buffer)))
        (should (eq 'error (niwa-session-last-outcome session)))
        (should (= 1 (niwa-session-turn-count session)))
        (should (= 0 launches))
        (should-not (niwa-session-external-conversations session))
        (setq gptel-tools nil)
        (goto-char (point-max))
        (niwa--insert-user-turn "Retry after roster failure")
        (niwa--send-request "Retry after roster failure")
        (niwa-test--await 5 "Roster failure retry did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (= 1 launches))
        (should (= 2 (niwa-session-turn-count session)))
        (let* ((text (buffer-string)) (answer (string-search "answer:" text)))
          (should answer)
          (should (string-search "Retry after roster failure" (substring text answer)))))))

  :doc "a pre-ACP failure cannot settle a replacement request"
  (niwa-engine-test--with-claude-session
    (let (replacement)
      (cl-letf (((symbol-function 'niwa-tools-native-roster)
                 (lambda (_owner)
                   (niwa-request-end)
                   (setq replacement (niwa-request-begin session))
                   (error "Fixture ownership changed"))))
        (should (equal '(error "Fixture ownership changed")
                       (should-error (niwa--send-request "Replaced owner")))))
      (should (eq replacement niwa--current-request))
      (should (niwa-turn-busy-p buffer))
      (should-not (niwa-session-last-outcome session))
      (should (= 0 (niwa-session-turn-count session))))))

(niwa-deftest niwa--send-request/claude-outcome (:quiet t)
  ,test
  (test)

  :doc "turn totals normalize cache writes and ignore duplicate terminal replies"
  (niwa-engine-test--with-claude-session
    (setq-local gptel-system-prompt "Usage fixture" gptel-tools nil)
    (cl-letf (((symbol-function 'niwa-claude-code-launch)
               (niwa-engine-test--claude-launch
                (lambda (_system mcp _model _effort &optional id _hook)
                  (list :meta '((promptResponse .
                                                ((stopReason . "end_turn")
                                                 (usage . ((inputTokens . 100) (outputTokens . 23)
                                                           (cachedReadTokens . 1000) (cachedWriteTokens . 17)
                                                           (totalTokens . 1140)))
                                                 (_meta . ((quota . ((token_count . ((totalTokens . 1140)))
                                                                     (model_usage . [((token_count . ((totalTokens . 8000))))])))))))))))))
      (niwa--send-request "hello")
      (niwa-test--await 5 "Usage turn did not settle" (not (niwa-turn-busy-p buffer)))
      (should (equal '(:input 117 :output 23 :cached 1000 :cache 17)
                     (plist-get (niwa-engine-info request) :tokens-full)))
      (should (= 1 (niwa-session-turn-count session))))))

(niwa-deftest niwa--send-request/claude-prompt (:quiet t)
  (niwa-engine-test--with-claude-session
    (setq-local gptel-system-prompt "Projection fixture" gptel-tools nil)
    (cl-letf (((symbol-function 'niwa-claude-code-launch)
               (niwa-engine-test--claude-launch
                (lambda (_system mcp _model _effort &optional id _hook)
                  nil))))
      (insert "hello"
              (niwa-tool-render-data-format '(:type prompt-summary :body "display-only"))
              (niwa--format-hook-audit-record '(:type guest-prompt :name "audit-only")))
      (niwa--send-request)
      (niwa-test--await 5 "Prompt projection did not settle" (not (niwa-turn-busy-p buffer)))
      ;; Publication rewrites the Org property drawer, so inspect the
      ;; canonical response span rather than a pre-publication offset.
      (save-excursion
        (goto-char (point-min))
        (let* ((span (text-property-search-forward 'gptel 'response t))
               (answer (buffer-substring-no-properties
                        (prop-match-beginning span) (prop-match-end span))))
          (should (string-search "answer:hello" answer))
          (should-not (string-search "display-only" answer))
          (should-not (string-search niwa--hook-audit-open answer)))))))

(niwa-deftest niwa-session-naming-consider/claude (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let ((niwa-model-workloads '((naming :provider "Claude Code:haiku" :effort nil)))
          systems models tool-rosters)
      (setq-local gptel-system-prompt "Root-only secret policy"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
      (setf (niwa-session-naming-state session) 'pending)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (system mcp model _effort &optional id _hook)
                    (push system systems) (push model models) (push mcp tool-rosters)
                    (should-not id)
                    nil))))
        (niwa-session-naming-consider session "Fix parsing")
        (niwa-test--await 5 "Subscription naming did not settle"
          (not niwa-session-naming--cancel))
        (should (equal "answer:Fix parsing" (niwa-session-name session)))
        (should (equal '("haiku") models))
        (should (equal '([]) tool-rosters))
        (should-not (string-search "Root-only secret policy" (car systems)))
        (should-not niwa--current-request)
        (should-not (niwa-session-external-conversations session))
        (should (= 0 (niwa-session-turn-count session)))))))

(niwa-deftest niwa-permission-review-start/claude (:quiet t)
  (dolist (valid '(t nil))
    (niwa-engine-test--with-claude-session
      (let ((niwa-permission-reviewer 'auto)
            (niwa-model-workloads '((guardian :provider "Claude Code:haiku" :effort nil)))
            models systems results fallback)
        (setf (niwa-session-permission-mode session) 'edits)
        (setq-local gptel-system-prompt "Root-only secret policy")
        (insert "Evaluate (+ 1 2) in this Emacs session.\n")
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (system mcp model _effort &optional id _hook)
                      (push model models) (push system systems)
                      (should-not id) (should (equal [] mcp))
                      (list :meta `((responseText . ,(if valid
                                                         "{\"decision\":\"allow-once\",\"reason\":\"Exact requested Eval\"}"
                                                       "invalid guardian response"))))))))
          (niwa-permission-review-start
           (list :kind 'eval :tool-name "Eval" :session session :origin "/root"
                 :data-buffer buffer :request request :request-id (niwa-request-id request)
                 :expression "(+ 1 2)" :mode "live" :permission-via 'mode
                 :callback (lambda (outcome) (push outcome results)))
           (lambda () (setq fallback t)))
          (niwa-test--await 5 "Subscription guardian did not settle"
            (not niwa-permission-review--pending))
          (should (equal '("haiku") models))
          (should-not (string-search "Root-only secret policy" (car systems)))
          (should-not (niwa-session-permission-rules session))
          (if valid
              (progn (should (equal '(allow-once) results))
                     (should-not fallback))
            (should-not results)
            (should fallback))
          (should (eq request niwa--current-request)))))))

(niwa-deftest niwa-context-summary-generate/claude (:quiet t)
  (dolist (valid '(t nil))
    (niwa-engine-test--with-claude-session
      (let ((niwa-model-context-limit 64000)
            (niwa-model-workloads '((journal :provider "Claude Code:haiku" :effort nil)))
            models results)
        (setq-local gptel-system-prompt "Root-only secret policy")
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (system mcp model _effort &optional id _hook)
                      (push model models)
                      (should-not id) (should (equal [] mcp))
                      (should-not (string-search "Root-only secret policy" system))
                      (list :meta `((responseText . ,(if valid
                                                         "## Done\n- Fixed parser.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"
                                                       "invalid summary"))
                                    (promptResponse . ((stopReason . "end_turn")
                                                       (usage . ((inputTokens . 11) (cachedWriteTokens . 6)
                                                                 (cachedReadTokens . 20) (outputTokens . 3)))))))))))
          (niwa-context-summary-generate
           "Frozen source evidence" 'digest
           (lambda (result) (should (eq buffer (current-buffer))) (push result results))
           :session session)
          (niwa-test--await 5 "Subscription digest did not settle" results)
          (should (equal '("haiku") models))
          (should (= 1 (length results)))
          (should (eq (if valid 'success 'error) (plist-get (car results) :outcome)))
          (should (= 17 (plist-get (car results) :input-tokens)))
          (should (= 20 (plist-get (car results) :cached-tokens)))
          (should (= 3 (plist-get (car results) :output-tokens)))
          (should (eq request niwa--current-request))
          (should-not (niwa-session-external-conversations session)))))))

(niwa-deftest niwa--send-request/old-directive-history (:quiet t)
  (niwa-engine-test--with-session
    (let* ((gptel--known-backends nil)
           (api (gptel-make-openai "fixture-api" :key "fixture" :models '(gpt-4o)))
           (provider (list :backend api :model 'gpt-4o))
           (real-request (symbol-function 'gptel-request))
           called)
      (niwa-claude-code-register)
      (niwa-model-set-session-provider session provider buffer)
      (setf (niwa-session-turn-count session) 1
            (niwa-session-prompt-index session) '((1 (:kind directive :cum-turn 1)))
            (niwa-session-external-conversations session)
            (list (list "directive-id" :engine 'claude-code :id "isolated-id"
                        :host (system-name) :directory root :state 'ready)))
      (setq-local gptel-tools nil gptel-system-prompt "Root system"
                  gptel-prompt-transform-functions nil)
      (insert "First ordinary root prompt")
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (prompt &rest options)
                   (setq called t)
                   (apply real-request prompt (append options '(:dry-run t))))))
        (niwa--send-request))
      (should called)
      ;; Old directive index records are ordinary root history now.
      (should (niwa-engine-root-history-p session))
      (setf (niwa-session-turn-count session) 2
            (niwa-session-prompt-index session)
            '((1 (:kind directive :cum-turn 1) (:cum-turn 2))))
      (niwa-model-set-session-provider session provider buffer)
      (should (eq api gptel-backend)))))

(niwa-deftest niwa-agent-control-spawn/claude (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((file (file-name-concat root "child-evidence.txt"))
          record invocation outcome launches)
      (setq-local gptel-system-prompt "ROOT PRIVATE INSTRUCTIONS")
      (dolist (name '("Read" "SendMessage" "ListAgents"))
        (niwa-tool-ensure name))
      (write-region "child evidence\n" nil file nil 'silent)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (system mcp model _effort &optional id _hook)
                        (push (list system model id) launches)
                        (list :meta `((responseText . "Child completed")
                                      (promptResponse . ((stopReason . "end_turn")
                                                         (usage . ((inputTokens . 12)
                                                                   (outputTokens . 4)
                                                                   (cachedWriteTokens . 0)))))
                                      (toolBatches . [[((name . "Read") (id . ,(format "child-read-%d" (length launches)))
                                                        (args . ((file_path . ,file))))]])))))))
            (niwa-agent-control-spawn
             session "reader" "Inspect the assigned file."
             (lambda (value) (setq outcome value record (plist-get value :record)))
             :agent (niwa-agent--create :name "reader" :description "Read evidence"
                                          :tools '(Read)
                                          :system-prompt "CHILD INSTRUCTIONS")
             :on-invocation (lambda (value) (setq invocation value)))
            (should-not (plist-get outcome :error))
            (should (eq 'success (plist-get outcome :outcome)))
            (niwa-test--await 5 "Claude child did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (eq 'idle (niwa-agent-record-activity record)))
            (should (equal "Child completed" (niwa-agent-record-settled-result record)))
            (should-not (niwa-agent-invocation-runtime-fsm invocation))
            (should (string-search "CHILD INSTRUCTIONS" (caar launches)))
            (should-not (string-search "ROOT PRIVATE INSTRUCTIONS" (caar launches)))
            (should (= 0 (niwa-session-turn-count session)))
            (should (eq request niwa--current-request))
            (with-current-buffer (niwa-agent-record-conversation-buffer record)
              (should (string-search "child evidence" (buffer-string))))
            (let ((mail (niwa-agent-control-context-mailbox session)))
              (should (= 1 (length mail)))
              (should (= 16 (plist-get (car mail) :usage))))
            (should (eq 'ready (plist-get
                               (alist-get "/root/reader" (niwa-session-external-conversations session)
                                          nil nil #'equal) :state)))
            (should-not (assoc "root" (niwa-session-external-conversations session)))
            (setq-local gptel-model 'haiku gptel-system-prompt "CHANGED ROOT")
            (niwa-agent-control-send-message session "/root/reader" "Inspect again.")
            (setq invocation (niwa-agent-record-invocation record))
            (niwa-test--await 5 "Claude child follow-up did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (eq 'idle (niwa-agent-record-activity record)))
            (should (equal "Child completed" (niwa-agent-record-settled-result record)))
            (should (equal "sonnet" (cadar launches)))
            (should (equal "fixture-session" (nth 2 (car launches))))
            (should-not (string-search "CHANGED ROOT" (caar launches)))
            (should (= 2 (length (niwa-agent-control-context-mailbox session))))
            (niwa-agent-control-send-message session "/root/reader" "wait")
            (setq invocation (niwa-agent-record-invocation record))
            (niwa-test--await 5 "Claude child did not begin waiting"
              (with-current-buffer (niwa-agent-invocation-buffer invocation)
                (string-search "waiting" (buffer-string))))
            (niwa-agent-control-interrupt session "/root/reader")
            (niwa-test--await 5 "Claude child cancellation did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (niwa-agent-invocation-runtime-settled-p invocation))
            (should (eq 'idle (niwa-agent-record-activity record)))
            (let ((mail (niwa-agent-control-context-mailbox session)))
              (should (= 3 (length mail)))
              (should (eq 'interrupted (plist-get (car (last mail)) :outcome))))
            (should-not (niwa-agent-control-active-turn-p session))
            (should (eq request niwa--current-request))
            (should (= 0 (niwa-session-turn-count session)))
            (niwa-agent-control-send-message session "/root/reader" "crash")
            (setq invocation (niwa-agent-record-invocation record))
            (niwa-test--await 5 "Child transport death did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (let ((mail (niwa-agent-control-context-mailbox session)))
              (should (= 4 (length mail)))
              (should (eq 'errored (plist-get (car (last mail)) :outcome))))
            (niwa-agent-control-send-message session "/root/reader" "Inspect after the crash.")
            (setq invocation (niwa-agent-record-invocation record))
            (niwa-test--await 5 "Child recovery did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (equal "Child completed" (niwa-agent-record-settled-result record)))
            (with-current-buffer (niwa-agent-record-conversation-buffer record)
              (should (= 2 (niwa-engine-test--count-evidence "Reconcile current state before continuing"))))
            (should (= 0 (niwa-session-turn-count session)))
            (let (unpublished)
              (should-error
               (niwa-agent-runtime-dispatch
                nil "reader" "Unpublished follow-up"
                :frozen-configuration (niwa-agent-invocation-frozen-configuration invocation)
                :path "/root/reader"
                :retained-id (niwa-agent-record-id record)
                :retained-buffer (niwa-agent-record-conversation-buffer record)
                :retained-transcript (niwa-agent-record-conversation-location record)
                :on-invocation (lambda (value)
                                 (setq unpublished value)
                                 (error "Fixture rejected publication"))))
              (should (niwa-agent-invocation-runtime-settled-p unpublished))
              (should-not (niwa-agent-invocation-runtime-cancel unpublished))))
        (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
          (kill-buffer (niwa-agent-record-conversation-buffer record)))))))

(niwa-deftest niwa-agent-control-send-message/claude-mail (:quiet t)
  (dolist (acknowledge '(t nil "foreign" "mismatch" "error" "malformed" "oversized" "initial" "initial-large"))
    (niwa-engine-test--with-claude-session
      :file-workspace (not (eq acknowledge t))
      (let ((file (file-name-concat root "mail-evidence.txt"))
            (mail (concat "Steer this child only."
                          (when (member acknowledge '("oversized" "initial-large"))
                            (make-string 5000 #x1f600))))
            (initial (member acknowledge '("initial" "initial-large")))
            ;; An oversized message stops the hook; the continuation prompt
            ;; delivers it whole, and Claude makes no further call.
            (oversized (equal acknowledge "oversized"))
            (delivered (or (eq acknowledge t) (member acknowledge '("initial" "initial-large" "oversized"))))
            record outcome)
        (dolist (name '("Read" "SendMessage" "ListAgents")) (niwa-tool-ensure name))
        (write-region "mail fixture\n" nil file nil 'silent)
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id hook)
                          (list :meta `((hookCommand . ,hook)
                                        (hookAcknowledgement . ,(or acknowledge :false))
                                        (afterHookTool . ((name . "SendMessage")
                                                          (args . ((target . "/root/reader")
                                                                   (message . "Late mail remains.")))))
                                        (responseText . "Child completed")
                                        (continuationPrompts . [((responseText . "Child completed"))])
                                        (toolBatches . ,(vector
                                                         (vconcat
                                                          (unless initial
                                                            (vector `((name . "SendMessage") (id . "queue-mail")
                                                                      (args . ((target . "/root/reader")
                                                                               (message . ,mail))))))
                                                          (vector `((name . "Read") (id . "mail-read")
                                                                    (args . ((file_path . ,file))))))))))))))
              (niwa-agent-control-spawn
               session "reader" "Inspect the assigned file."
               (lambda (value) (setq outcome value record (plist-get value :record)))
               :agent (niwa-agent--create :name "reader" :description "Read evidence"
                                            :tools '(Read SendMessage) :system-prompt "Child instructions")
               :on-invocation
               (lambda (_invocation)
                 (when initial
                   (niwa-agent-control-send-message session "/root/reader" mail))))
              (should-not (plist-get outcome :error))
              (niwa-test--await 5 "Mail delivery child did not settle"
                (not (niwa-agent-control-active-turn-p session)))
              (ert-info ((format "acknowledge=%S" acknowledge))
                (should (equal "Child completed" (niwa-agent-record-settled-result record))))
              (should (= (cond (oversized 0) (delivered 1) (t 2))
                         (length (niwa-agent-record-mailbox record))))
              (should (eq (not oversized)
                          (cl-some (lambda (message) (equal "Late mail remains." (plist-get message :payload)))
                                   (niwa-agent-record-mailbox record))))
              (with-current-buffer (niwa-agent-record-conversation-buffer record)
                (should (= (if delivered 1 0)
                           (niwa-engine-test--count-evidence "<agent-message"))))
              (should-not (string-search "Steer this child only" (buffer-string))))
          (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
            (kill-buffer (niwa-agent-record-conversation-buffer record))))))))

(niwa-deftest niwa--send-request/claude-initial-mail (:quiet t)
  (dolist (acknowledge '(t nil "foreign" "mismatch" "nested"))
    (niwa-engine-test--with-claude-session
      :file-workspace (not (eq acknowledge t))
      (setq-local gptel-system-prompt "Session instructions" gptel-tools nil)
      (niwa-agent-control-send-message
       session "/root" (concat "Large initial mail " (make-string 12000 ?x) " END-MAIL"))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    (list :meta `((promptAcknowledgement . ,(or acknowledge :false))
                                  (responseText . "Complete")))))))
        (insert "Process my mail")
        (niwa--send-request "Process my mail")
        (niwa-test--await 5 "Initial mail turn did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (= (if (eq acknowledge t) 0 1) (length (niwa-session-messages session))))
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
          (niwa--send-request "Try my mail again")
          (niwa-test--await 5 "Retried mail turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (should-not (niwa-session-messages session))
          (should (= 1 (how-many "END-MAIL" (point-min) (point-max)))))))))

(niwa-deftest niwa--send-request/claude-context-updates (:quiet t)
  (dolist (receipt '(t nil mismatch unchanged compact revert final pending-mail))
    (niwa-engine-test--with-claude-session
      :file-workspace (not (eq receipt t))
      (let* ((niwa-system--prompt-components (copy-tree niwa-system--prompt-components))
             (niwa-system-retained-components '(memory))
             (state "Original memory")
             (changed (if (eq receipt 'unchanged) state "UPDATED-MEMORY-6492"))
             (file (file-name-concat root "evidence.txt"))
             (chunks 0))
        (setf (alist-get 'memory niwa-system--prompt-components)
              (niwa-system-prompt-component--create
               :name 'memory :producer (lambda (_context) state)))
        (write-region "Evidence" nil file nil 'silent)
        (setq-local gptel-system-prompt "Context update fixture"
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (when (eq receipt 'pending-mail)
          (niwa-agent-control-send-message session "/root" "Unacknowledged initial mail"))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :observe (lambda (owner notification)
                                       (niwa-claude-code-context-observe owner notification)
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
                                                               '("before-update" "changed" "after-update")))))))))))
          (insert "Read with current context")
          (niwa--send-request "Read with current context")
          (niwa-test--await 5 "Context update turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (ert-info ((format "receipt=%S" receipt))
            (should (eq (if (memq receipt '(t unchanged compact revert)) 'success 'error)
                        (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
            ;; An unreceived first prompt now withholds required observations,
            ;; not only mail, so no tool may run in that case.
            (should (= (cond ((eq receipt 'pending-mail) 0)
                             ((memq receipt '(t unchanged compact revert)) 3)
                             (t 2))
                       (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (pcase receipt ((or 't 'revert) 1) ('compact 2) (_ 0))
                       (niwa-engine-test--count-evidence "UPDATED-MEMORY-6492")))
            ;; The acknowledged prompt delivers the observations once; a
            ;; reverted section is recorded again as a mid-turn update.
            (should (= (pcase receipt ('revert 2) ('pending-mail 0) (_ 1))
                       (niwa-engine-test--count-evidence "Original memory")))))))))

(niwa-deftest niwa-agent-control-spawn/claude-context-updates (:quiet t)
  (niwa-engine-test--with-claude-session
    (let* ((niwa-system--prompt-components (copy-tree niwa-system--prompt-components))
           (niwa-system-retained-components '(memory active-goal))
           (state "Original child memory")
           (file (file-name-concat root "evidence.txt"))
           record outcome initial-content)
      (setf (alist-get 'memory niwa-system--prompt-components)
            (niwa-system-prompt-component--create :name 'memory :producer (lambda (_) state))
            (alist-get 'active-goal niwa-system--prompt-components)
            (niwa-system-prompt-component--create :name 'active-goal :producer (lambda (_) "ROOT-ONLY-GOAL")))
      (write-region "Evidence" nil file nil 'silent)
      (niwa-tool-ensure "Read")
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (system mcp _model _effort &optional id hook)
                        (should (equal "Child instructions" system))
                        (should-not (string-search "ROOT-ONLY-GOAL" system))
                        (list :observe (lambda (owner notification)
                                         (when-let* ((message (alist-get 'message (alist-get 'params notification)))
                                                     ((equal "user" (alist-get 'type message))))
                                           (setq initial-content
                                                 (mapconcat (lambda (part) (or (alist-get 'text part) ""))
                                                            (alist-get 'content (alist-get 'message message)) "\n")))
                                         (niwa-claude-code-context-observe owner notification)
                                         (when (equal "agent_message_chunk"
                                                      (alist-get 'sessionUpdate (alist-get 'update (alist-get 'params notification))))
                                           (setq state "CHILD-MEMORY-7258")))
                              :meta `((hookCommand . ,hook) (responseText . "Child completed")
                                      (toolBatches . [[((name . "Read") (id . "before-change") (args . ((file_path . ,file))))]
                                                      [((name . "Read") (id . "after-change") (args . ((file_path . ,file))))]])))))))
            (niwa-agent-control-spawn
             session "reader" "Inspect the file."
             (lambda (value) (setq outcome value record (plist-get value :record)))
             :agent (niwa-agent--create :name "reader" :description "Read evidence"
                                          :tools '(Read) :context-components '(memory)
                                          :system-prompt "Child instructions"))
            (should-not (plist-get outcome :error))
            (niwa-test--await 5 "Context update child did not settle"
              (not (niwa-agent-control-active-turn-p session)))
            (should (equal "Child completed" (niwa-agent-record-settled-result record)))
            (should (string-search "Original child memory" initial-content))
            (should (string-search "<system-reminder>" initial-content))
            (should-not (string-search "ROOT-ONLY-GOAL" initial-content))
            (with-current-buffer (niwa-agent-record-conversation-buffer record)
              (should (= 1 (niwa-engine-test--count-evidence "Original child memory")))
              (should (= 1 (niwa-engine-test--count-evidence "CHILD-MEMORY-7258")))
              (should-not (string-search "ROOT-ONLY-GOAL" (buffer-string))))
            (should-not (string-search "CHILD-MEMORY-7258" (buffer-string))))
        (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
          (kill-buffer (niwa-agent-record-conversation-buffer record)))))))

(niwa-deftest niwa--send-request/claude-path-instructions (:quiet t)
  (dolist (receipt '(t nil late))
    (niwa-engine-test--with-claude-session
      (let* ((directory (file-name-concat root "lib"))
             (file (file-name-concat directory "evidence.txt"))
             (instructions (file-name-concat directory "AGENTS.md"))
             (late-commits 0) queued)
        (make-directory directory)
        (write-region "Evidence" nil file nil 'silent)
        (write-region "PATH-GUIDANCE-4319" nil instructions nil 'silent)
        (setq-local gptel-system-prompt "Path instructions fixture"
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :observe (lambda (owner notification)
                                       (when (and (eq receipt 'late) (not queued)
                                                  (equal "hook_response"
                                                         (alist-get 'subtype (alist-get 'message (alist-get 'params notification)))))
                                         (setq queued t)
                                         ;; Replace an event already in flight.  Its receipt
                                         ;; must not consume the newer coalesced event.
                                         (niwa-reminders-queue-turn-event
                                          buffer (cons 'workspace-instructions instructions)
                                          "LATE-GUIDANCE-3928" (lambda () (cl-incf late-commits))))
                                       (niwa-claude-code-context-observe owner notification))
                            :meta `((hookCommand . ,hook) (hookAcknowledgement . ,(if receipt t :false))
                                    (responseText . "Done")
                                    (toolBatches . [[((name . "Read") (id . "first-read") (args . ((file_path . ,file))))]
                                                    [((name . "Read") (id . "second-read") (args . ((file_path . ,file))))]])))))))
          (insert "Read with applicable instructions")
          (niwa--send-request "Read with applicable instructions")
          (niwa-test--await 5 "Path instructions turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (ert-info ((format "receipt=%S" receipt))
            (should (eq (if receipt 'success 'error)
                        (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
            (should (= (if receipt 2 1) (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (if receipt 1 0) (niwa-engine-test--count-evidence "PATH-GUIDANCE-4319")))
            (should (eq (not (null receipt)) (not (null (niwa-session-workspace-instruction-hashes session)))))
            (should (= (if (eq receipt 'late) 1 0) late-commits))
            (should (= late-commits (niwa-engine-test--count-evidence "LATE-GUIDANCE-3928")))))))))

(niwa-deftest niwa--send-request/claude-compaction (:quiet t)
  (dolist (receipt '(t nil unchanged final))
    (niwa-engine-test--with-claude-session
      (let* ((niwa-system--prompt-components (copy-tree niwa-system--prompt-components))
             (niwa-system-retained-components '(memory))
             (state "Original memory")
             (changed (if (eq receipt 'unchanged) state "CURRENT-CONTEXT-3726"))
             (file (file-name-concat root "evidence.txt"))
             captured-system initial-content)
        (setf (alist-get 'memory niwa-system--prompt-components)
              (niwa-system-prompt-component--create
               :name 'memory :producer (lambda (_context) state)))
        (write-region "Evidence" nil file nil 'silent)
        (setq-local gptel-system-prompt (concat (make-string 12000 ?x) " LARGE-SYSTEM-END")
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (system mcp _model _effort &optional id hook)
                      (setq captured-system system)
                      (list :observe (lambda (owner notification)
                                       (when-let* ((message (alist-get 'message (alist-get 'params notification)))
                                                   ((equal "user" (alist-get 'type message))))
                                         (setq initial-content
                                               (mapconcat (lambda (part) (or (alist-get 'text part) ""))
                                                          (alist-get 'content (alist-get 'message message)) "\n")))
                                       (niwa-claude-code-context-observe owner notification)
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
                                                                 (args . ((file_path . ,file))))))])))))))
          (insert "Read before and after compaction")
          (niwa--send-request "Read before and after compaction")
          (niwa-test--await 5 "Compaction turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (should (string-search "LARGE-SYSTEM-END" captured-system))
          (should-not (string-search "Original memory" captured-system))
          (should (string-search "Original memory" initial-content))
          (should (string-search "<system-reminder>" initial-content))
          (should (eq (if (memq receipt '(t unchanged)) 'success 'error)
                      (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
          (ert-info ((format "receipt=%S" receipt))
            (should (= (if (memq receipt '(t unchanged)) 2 1)
                       (how-many "^#\\+begin_tool" (point-min) (point-max)))))
          (should (= (if (eq receipt t) 1 0)
                     (niwa-engine-test--count-evidence "CURRENT-CONTEXT-3726")))
          (should (= (if (eq receipt 'unchanged) 2 1)
                     (niwa-engine-test--count-evidence "Original memory"))))))))

(niwa-deftest niwa--send-request/claude-recovery (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((file (file-name-concat root "effects.txt"))
          (crash t) (receipt t) (call-id "mutate-once")
          restored restored-session)
      (setq-local gptel-system-prompt "Recovery fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Bash"))))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (list :meta `((crashAfterTool . ,(if crash t :false))
                                      (promptAcknowledgement . ,(if receipt t :false))
                                      (responseText . "Done")
                                      (toolBatches . [[((name . "Bash") (id . ,call-id)
                                                        (args . ((description . "Append recovery test effect")
                                                                 (command . ,(concat "printf 'effect\n' >> "
                                                                                     (shell-quote-argument file))))))]])))))))
            (insert "Append one line")
            (niwa--send-request "Append one line")
            (niwa-test--await 5 "Crashing turn did not settle"
              (not (niwa-turn-busy-p buffer)))
            (should (eq 'error (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
            (should (equal "effect\n" (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (let* ((path (niwa-session-save-path session))
                   (metadata (car (read-from-string
                                   (niwa-session-artifacts-read-artifact session "session.meta.el" t))))
                   (history (cdr (assoc "root" (plist-get metadata :external-conversations)))))
              (should (eq 'uncertain (plist-get history :state)))
              (test-niwa-session-persistence--release-and-kill buffer session)
              (setq restored (niwa-session-persistence-restore path nil nil workspace)))
            (with-current-buffer restored
              (setq restored-session niwa--session crash nil)
              (niwa-agent-control-send-message restored-session "/root" "RECOVERY-MAIL-7851")
              ;; This fixture installs its minimal tool roster directly rather
              ;; than through a persisted named preset.
              (setq-local gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Bash"))))
              (dolist (ack '(nil t))
                (setq receipt ack call-id (if ack "after-reconciliation" "unacknowledged-call"))
                (goto-char (point-max))
                (insert "\nInspect previous effects before continuing")
                (let ((owner (niwa--send-request "Inspect previous effects before continuing")))
                  (niwa-test--await 5 "Recovery turn did not settle"
                    (not (niwa-turn-busy-p restored)))
                  (ert-info ((format "ack=%S history=%S pending=%S" ack
                                     (niwa-session-external-conversations restored-session)
                                     (plist-get (niwa-engine-info owner) :niwa-claude-context-pending)))
                    (should (eq (if ack 'success 'error)
                                (plist-get (niwa-engine-info owner) :niwa-acp-outcome)))))
                ;; Unacknowledged reconciliation blocks tools; a new call runs once.
                (should (equal (if ack "effect\neffect\n" "effect\n")
                               (with-temp-buffer (insert-file-contents file) (buffer-string)))))
              (should (= 1 (niwa-engine-test--count-evidence "Reconcile current state before continuing")))
              (should (= 1 (how-many "RECOVERY-MAIL-7851" (point-min) (point-max))))
              (should-not (niwa-session-messages restored-session))))
        (when (buffer-live-p restored)
          (test-niwa-session-persistence--release-and-kill restored restored-session))))))

(niwa-deftest niwa--send-request/claude-initial-reminder-after-fork-point (:quiet t)
  (niwa-engine-test--with-claude-session
    (let* ((nested (file-name-concat root "nested"))
           (instructions (file-name-concat nested "AGENTS.md"))
           (file (file-name-concat nested "source.txt"))
           (turn 0))
      (make-directory nested t)
      (write-region "FIRST GUIDANCE" nil instructions nil 'silent)
      (write-region "source contents" nil file nil 'silent)
      (setq-local gptel-system-prompt "Root reminder reproduction"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id hook)
                    (cl-incf turn)
                    (list :meta `((hookCommand . ,hook) (responseText . "Finished native turn")
                                  (toolBatches . ,(if (= turn 1)
                                                      `[[((name . "Read") (id . "first-read")
                                                          (args . ((file_path . ,file))))]] []))))))))
        (niwa--insert-user-turn "Read my source and follow its guidance.")
        (niwa--send-request "Read my source and follow its guidance.")
        (niwa-test--await 5 "First turn did not finish" (not (niwa-turn-busy-p buffer)))
        (should (= 1 (niwa-engine-test--count-evidence "FIRST GUIDANCE")))
        (write-region "SECOND GUIDANCE RESTORED" nil instructions nil 'silent)
        (goto-char (point-max))
        (niwa--insert-user-turn "Continue using my changed guidance.")
        (let ((second (niwa--send-request "Continue using my changed guidance.")))
          (niwa-test--await 5 "Second turn did not finish" (not (niwa-turn-busy-p buffer)))
          (should (eq 'success (plist-get (niwa-engine-info second) :niwa-acp-outcome))))
        ;; Context was acknowledged by the native peer and is trusted audit data.
        (should (equal (secure-hash 'sha256 "SECOND GUIDANCE RESTORED")
                       (cdr (assoc (list "/root" instructions)
                                   (niwa-session-workspace-instruction-hashes session)))))
        (should (cl-some (lambda (span)
                           (string-search "SECOND GUIDANCE RESTORED"
                                          (prin1-to-string (plist-get span :record))))
                         (niwa-transcript-audit-buffer-spans 'injected-reminders)))
        ;; The canonical evidence and view/provider consumers must retain the body.
        (ert-info ((niwa-engine-test--evidence))
          (should (= 1 (niwa-engine-test--count-evidence "SECOND GUIDANCE RESTORED"))))))))

(defun test-niwa-claude-code-session--observation-types (text)
  "Return the selected-observation types recorded as injected in TEXT."
  (cl-loop for record in (niwa-transcript-audit-records text 'injected-reminders)
           append (cl-loop for item in (plist-get record :items)
                           for type = (plist-get item :type)
                           when (and (symbolp type) (string-prefix-p "context-" (symbol-name type)))
                           collect type)))

(niwa-deftest niwa--send-request/claude-records-system-observations (:quiet t)
  ,test
  (test)
  :doc "Claude delivers and records first-turn observations after user input, like gptel"
  (let (gptel-types claude-types gptel-data)
    ;; gptel delivers changed observations as a recorded turn-start injection.
    (niwa-engine-test--with-session
      (let* ((gptel--known-backends nil)
             (api (gptel-make-anthropic "Parity API" :key "fixture" :models '(claude-sonnet-4-5)))
             (real-request (symbol-function 'gptel-request)))
        (niwa-model-set-session-provider
         session (list :backend api :model 'claude-sonnet-4-5) buffer)
        (setq-local gptel-system-prompt "Parity fixture" gptel-tools nil
                    gptel-prompt-transform-functions nil)
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (prompt &rest options)
                     (let ((sent (apply real-request prompt (append options '(:dry-run t)))))
                       (niwa-context-delivery-stage sent)
                       (niwa-reminders--handle-inject sent)
                       (setq gptel-data (plist-get (gptel-fsm-info sent) :data))
                       sent))))
          (niwa--insert-user-turn "Parity task")
          (niwa--send-request))
        (setq gptel-types (test-niwa-claude-code-session--observation-types (buffer-string)))
        (should (equal "Parity fixture" (plist-get gptel-data :system)))
        (let* ((messages (plist-get gptel-data :messages))
               (first (niwa-context-delivery--message-text (aref messages 0)))
               (reminders (niwa-context-delivery--message-text (aref messages 1))))
          (should (string-search "Parity task" first))
          (dolist (record (niwa-transcript-audit-records (buffer-string) 'injected-reminders))
            (should (eq 'turn-start (plist-get record :phase)))
            (dolist (item (plist-get record :items))
              (should-not (string-search (plist-get item :body) first))
              (should (string-search (niwa-reminders-format-block (plist-get item :body))
                                     reminders)))))))
    (should (memq 'context-environment gptel-types))
    (niwa-engine-test--with-claude-session
      (let ((real-prompt (symbol-function 'niwa-claude-code-context-prompt))
            systems prompts)
        (setq-local gptel-system-prompt "Parity fixture" gptel-tools nil)
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (system &rest _) (push system systems) nil)))
                  ((symbol-function 'niwa-claude-code-context-prompt)
                   (lambda (&rest args)
                     (car (push (apply real-prompt args) prompts)))))
          (niwa--insert-user-turn "Parity task")
          (niwa--send-request "Parity task")
          (niwa-test--await 5 "First native turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
          (setq claude-types (test-niwa-claude-code-session--observation-types (buffer-string)))
          (should (equal (plist-get gptel-data :system) (car systems)))
          ;; Recorded observations reach the model after authored user input,
          ;; never through the native system prompt.
          (dolist (record (niwa-transcript-audit-records (buffer-string) 'injected-reminders))
            (dolist (item (plist-get record :items))
              (when (memq (plist-get item :type) claude-types)
                (should (eq 'turn-start (plist-get record :phase)))
                (should-not (string-search (plist-get item :body) (car systems)))
                (should (equal "Parity task" (alist-get 'text (aref (car prompts) 0))))
                (should (seq-some (lambda (part)
                                    (string-search (niwa-reminders-format-block (plist-get item :body))
                                                   (or (alist-get 'text part) "")))
                                  (substring (car prompts) 1))))))
          ;; Control: an unchanged baseline is not recorded again; one changed
          ;; section is recorded alone.
          (write-region "PARITY WORKSPACE GUIDANCE" nil (file-name-concat root "AGENTS.md") nil 'silent)
          (dolist (prompt '("Second native turn" "Third native turn"))
            (goto-char (point-max))
            (niwa--insert-user-turn prompt)
            (niwa--send-request prompt)
            (niwa-test--await 5 "Later native turn did not settle"
              (not (niwa-turn-busy-p buffer))))
          (let ((later (nthcdr (length claude-types)
                               (test-niwa-claude-code-session--observation-types (buffer-string)))))
            (should (equal '(context-workspace-config) later))
            (should (cl-some (lambda (record)
                               (cl-some (lambda (item)
                                          (and (eq 'context-workspace-config (plist-get item :type))
                                               (string-search "PARITY WORKSPACE GUIDANCE"
                                                              (plist-get item :body))))
                                        (plist-get record :items)))
                             (niwa-transcript-audit-records (buffer-string) 'injected-reminders))))
          ;; The real view lists the first turn's observations in its user turn.
          ;; Killing the data buffer at fixture teardown also kills this view.
          (let ((view (generate-new-buffer " *parity-view*")))
            (save-current-buffer
              (niwa-view--setup view buffer)
              (with-current-buffer view
                (niwa-view--full-rerender)
                (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                       (row (and (string-match "◇ \\([0-9]+\\) system reminders (\\([^)\n]*\\)" text)
                                 (match-beginning 0)))
                       (labels (and row (match-string 2 text))))
                  (ert-info (text)
                    (should row)
                    (should (< (string-search "Parity task" text) row
                               (string-search "Assistant" text)))
                    (dolist (type claude-types)
                      (should (string-search (symbol-name type) labels)))))))))))
    (should (equal (sort (copy-sequence gptel-types) #'string<)
                   (sort (copy-sequence claude-types) #'string<)))))

(niwa-deftest niwa--send-request/claude-stable-system-prompt (:quiet t)
  ,test
  (test)
  :doc "Journal publication and workspace edits reach resumed launches without changing the system"
  (niwa-engine-test--with-claude-session
    (let* ((niwa-journal-max-age-days nil)
           (niwa-system-retained-components '(journal workspace-config))
           (instructions (file-name-concat root "AGENTS.md"))
           (file (file-name-concat root "evidence.txt"))
           (real-prompt (symbol-function 'niwa-claude-code-context-prompt))
           (turn 0) systems ids prompts
           (niwa-post-tool-use-functions
            (list (lambda (_event)
                    (write-region "WORKSPACE-C-6630" nil instructions nil 'silent)
                    nil))))
      (niwa-journal-store-publish-digest
       root (list :capture-id (make-string 64 ?a) :session "cache-fixture"
                  :session-name "Cache fixture" :workspace (make-string 64 ?b)
                  :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                  :turns '(1) :turn-ids (list (make-string 64 ?d))
                  :created "2026-11-07T12:00:00Z" :model "provider:model")
       "## Done\n- none\n## Learned\n- JOURNAL-A-1182\n## Surprised\n- none\n## Unfinished\n- none")
      (write-region "WORKSPACE-A-1182" nil instructions nil 'silent)
      (write-region "Evidence" nil file nil 'silent)
      (setq-local gptel-system-prompt "Stable prompt fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (system _mcp _model _effort &optional id hook)
                    (cl-incf turn)
                    (push system systems)
                    (push id ids)
                    ;; The second turn's tool boundary delivers a further change.
                    (list :meta `((hookCommand . ,hook) (responseText . "Done")
                                  (toolBatches . ,(if (= turn 2)
                                                      `[[((name . "Read") (id . "mid-turn")
                                                          (args . ((file_path . ,file))))]
                                                        [((name . "Read") (id . "after-update")
                                                          (args . ((file_path . ,file))))]]
                                                    [])))))))
                ((symbol-function 'niwa-claude-code-context-prompt)
                 (lambda (&rest args)
                   (car (push (apply real-prompt args) prompts)))))
        (dolist (prompt '("First native turn" "Second native turn" "Third native turn"))
          (goto-char (point-max))
          (niwa--insert-user-turn prompt)
          (niwa--send-request prompt)
          (niwa-test--await 5 "Native turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
          ;; Real journal publication and workspace guidance change between
          ;; adapter starts.  The next launch must not rewrite the prefix.
          (when (equal prompt "First native turn")
            (niwa-journal-store-publish-digest
             root (list :capture-id (make-string 64 ?e) :session "cache-fixture"
                        :session-name "Cache fixture" :workspace (make-string 64 ?b)
                        :trigger 'session-end :segment 1 :source-revision (make-string 64 ?f)
                        :turns '(2) :turn-ids (list (make-string 64 ?1))
                        :created "2026-11-07T12:01:00Z" :model "provider:model")
             "## Done\n- none\n## Learned\n- JOURNAL-B-4417\n## Surprised\n- none\n## Unfinished\n- none")
            (write-region "WORKSPACE-B-4417" nil instructions nil 'silent)
            ;; Advance the existing discovery throttle without a wall-clock wait.
            (let ((observation (niwa-workspace-journal-observation workspace)))
              (setf (niwa-workspace-journal-observation workspace)
                    (plist-put observation :time (- (float-time) 11))))))
        (ert-info ((format "ids=%S" ids))
          (should (cadr ids))
          (should (equal (car ids) (cadr ids))))
        ;; Every resumed launch composes only the same static system prompt.
        (should (= 3 (length systems)))
        (should (equal (cadr systems) (caddr systems)))
        (should (equal (car systems) (cadr systems)))
        (should (cl-every (lambda (system) (equal "Stable prompt fixture" system)) systems))
        (let ((first (car (last prompts))))
          (should (equal "First native turn" (alist-get 'text (aref first 0))))
          (should (string-search "JOURNAL-A-1182" (alist-get 'text (aref first 1))))
          (should (string-search "WORKSPACE-A-1182" (alist-get 'text (aref first 1)))))
        ;; The change reaches the model once, in the second prompt's content.
        (let ((second (prin1-to-string (cadr prompts))))
          (should (string-search "JOURNAL-B-4417" second))
          (should (string-search "WORKSPACE-B-4417" second))
          (should (string-search "<system-reminder>" second)))
        ;; The third prompt repeats neither that nor the hook-delivered change.
        (should-not (string-search "JOURNAL-B-4417" (prin1-to-string (car prompts))))
        (should-not (string-search "WORKSPACE-C-6630" (prin1-to-string (car prompts))))
        (should (= 1 (niwa-engine-test--count-evidence "JOURNAL-B-4417")))
        (should (= 1 (niwa-engine-test--count-evidence "WORKSPACE-C-6630")))
        ;; The complete second journal observation also retains the first digest.
        (should (= 2 (niwa-engine-test--count-evidence "JOURNAL-A-1182")))))))

(niwa-deftest niwa--send-request/claude-observation-restoration (:quiet t)
  ,test
  (test)
  :doc "Compaction and a new native conversation restore reminders, not the system prompt"
  (niwa-engine-test--with-claude-session
    (let* ((niwa-system--prompt-components (copy-tree niwa-system--prompt-components))
           (niwa-system-retained-components '(memory))
           (state "MEMORY-ONE-2051")
           (file (file-name-concat root "evidence.txt"))
           (real-prompt (symbol-function 'niwa-claude-code-context-prompt))
           (turn 0) systems prompts)
      (setf (alist-get 'memory niwa-system--prompt-components)
            (niwa-system-prompt-component--create
             :name 'memory :producer (lambda (_context) state)))
      (write-region "Evidence" nil file nil 'silent)
      (setq-local gptel-system-prompt "Refresh fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (system _mcp _model _effort &optional _id hook)
                    (cl-incf turn)
                    (push system systems)
                    ;; The second turn compacts before its tool batch.
                    (list :meta `((hookCommand . ,hook) (responseText . "Done")
                                  (compactBeforeBatch . ,(when (= turn 2) 0))
                                  (toolBatches . ,(if (= turn 2)
                                                      `[[((name . "Read") (id . "after-compact")
                                                          (args . ((file_path . ,file))))]]
                                                    [])))))))
                ((symbol-function 'niwa-claude-code-context-prompt)
                 (lambda (&rest args)
                   (car (push (apply real-prompt args) prompts)))))
        (cl-flet ((send (prompt)
                    (goto-char (point-max))
                    (niwa--insert-user-turn prompt)
                    (niwa--send-request prompt)
                    (niwa-test--await 5 "Native turn did not settle"
                      (not (niwa-turn-busy-p buffer)))
                    (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))))
          (send "First turn")
          (setq state "MEMORY-TWO-7714")
          (send "Compacting turn")
          ;; Before compaction the resumed launch kept its system prompt.
          (should (equal (cadr systems) (car systems)))
          (send "After compaction")
          (should-not (string-search "MEMORY-TWO-7714" (car systems)))
          (should-not (string-search "MEMORY-TWO-7714" (prin1-to-string (car prompts))))
          ;; The turn-start change and the compaction restoration each deliver
          ;; the complete observation.  The following prompt repeats neither.
          (should (= 2 (niwa-engine-test--count-evidence "MEMORY-TWO-7714")))
          (let ((compacted (car systems)))
            (setq state "MEMORY-THREE-3390")
            (send "Resumed after update")
            ;; A further observation change still leaves the prefix intact.
            (should (equal compacted (car systems))))
          ;; A new native conversation starts from the current observations.
          (niwa-claude-code-release-history session)
          (send "New conversation")
          (should (string-search "MEMORY-THREE-3390" (prin1-to-string (car prompts))))
          (should (= 2 (niwa-engine-test--count-evidence "MEMORY-THREE-3390")))
          (should (cl-every (lambda (system) (equal "Refresh fixture" system)) systems))
          (should (= 5 turn)))))))

(provide 'test-niwa-claude-code-session)
;;; test-niwa-claude-code-session.el ends here
