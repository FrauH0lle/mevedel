;;; test-mevedel-session-codec.el --- Session sidecar codec tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-codec'.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))


;;
;;; Workspace round-trip

(mevedel-deftest mevedel-session-codec--workspace-to-plist ()
  ,test
  (test)
  :doc "captures durable identity and target-native root"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
               (plist (mevedel-session-codec--workspace-to-plist workspace)))
          (should (eq 'project    (plist-get plist :type)))
          (should (equal (mevedel-workspace-identity-read root)
                         (plist-get plist :workspace-id)))
          (should (equal (file-name-as-directory root)
                         (plist-get plist :target-native-root)))
          (should (equal (file-name-nondirectory
                          (directory-file-name root))
                         (plist-get plist :name))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "returns nil for a nil workspace"
  (should (null (mevedel-session-codec--workspace-to-plist nil))))


(mevedel-deftest mevedel-session-codec--workspace-from-plist ()
  ,test
  (test)
  :doc "accepts the currently opened matching workspace"
  (let ((root (make-temp-file "mevedel-workspace-match-" t)))
    (unwind-protect
        (let* ((workspace
                (test-mevedel-session-persistence--make-workspace root))
               (plist
                (mevedel-session-codec--workspace-to-plist workspace)))
          (should
           (equal (cons workspace nil)
                  (mevedel-session-codec--workspace-from-plist
                   plist workspace))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "requires confirmation for a different project-owned identity"
  (let ((saved-root (make-temp-file "mevedel-workspace-saved-" t))
        (opened-root (make-temp-file "mevedel-workspace-opened-" t)))
    (unwind-protect
        (let* ((saved
                (test-mevedel-session-persistence--make-workspace saved-root))
               (plist
                (mevedel-session-codec--workspace-to-plist saved))
               (opened
                (progn
                  (mevedel-workspace-clear-registry)
                  (test-mevedel-session-persistence--make-workspace
                   opened-root))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-codec--workspace-from-plist plist opened)
             :type 'user-error))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (should
             (equal (cons opened t)
                    (mevedel-session-codec--workspace-from-plist
                     plist opened)))))
      (delete-directory saved-root t)
      (delete-directory opened-root t)
      (mevedel-workspace-clear-registry))))


;;
;;; Permission rule hygiene

(mevedel-deftest mevedel-session-codec--filter-permission-rules ()
  ,test
  (test)
  :doc "keeps allow / deny / ask rules"
  (let ((rules '(("Read" :path "/x" :action allow)
                 ("Bash" :pattern "rm" :action deny)
                 ("Write" :path "/y" :action ask))))
    (should (equal rules
                   (mevedel-session-codec--filter-permission-rules rules))))
  :doc "drops rules with unknown actions"
  (let* ((rules '(("Read"  :path "/x" :action allow)
                  ("Write" :path "/y" :action future-action)
                  ("Bash"  :pattern "ls" :action allow)))
         (filtered (mevedel-session-codec--filter-permission-rules rules)))
    (should (= 2 (length filtered)))
    (should (equal "Read" (caar filtered)))
    (should (equal "Bash" (caadr filtered))))
  :doc "drops malformed entries"
  (let ((rules '(("Read" :path "/x" :action allow)
                 nil
                 "not a rule"
                 ("Bash" :pattern "echo" :action allow))))
    (should (= 2 (length
                  (mevedel-session-codec--filter-permission-rules rules))))))


(mevedel-deftest mevedel-session-codec--filter-resource-grants ()
  ,test
  (test)
  :doc "keeps exact read and write grants"
  (let ((grants '((:path "/tmp/read" :access read)
                  (:path "/tmp/write" :access write))))
    (should (equal grants
                   (mevedel-session-codec--filter-resource-grants
                    grants))))
  :doc "drops malformed grants and unknown access levels"
  (should
   (equal '((:path "/tmp/read" :access read))
          (mevedel-session-codec--filter-resource-grants
           '((:path "/tmp/read" :access read)
             (:path "/tmp/future" :access execute)
             (:access write)
             "not a grant")))))


;;
;;; Goal round-trip

(mevedel-deftest mevedel-session-codec--goal-to-plist ()
  ,test
  (test)
  :doc "captures the strict phase-free Goal schema"
  (let* ((goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'blocked
                :reason "Need an API credential."
                :token-budget 1000 :tokens-used 345
                :time-used-seconds 12 :turns-run 4
                :plan-reference "local/plans/accepted.md"
                :created-at "created" :updated-at "updated"))
         (plist (mevedel-session-codec--goal-to-plist goal)))
    (should (equal "g1" (plist-get plist :id)))
    (should (eq 'blocked (plist-get plist :status)))
    (should (= 1000 (plist-get plist :token-budget)))
    (should (= 345 (plist-get plist :tokens-used)))
    (should (= 12 (plist-get plist :time-used-seconds)))
    (should (= 4 (plist-get plist :turns-run)))
    (should (equal "local/plans/accepted.md" (plist-get plist :plan-reference)))
    (should (equal "Need an API credential." (plist-get plist :reason)))))


(mevedel-deftest mevedel-session-codec--goal-from-plist ()
  ,test
  (test)
  :doc "rebuilds the current strict Goal schema"
  (let ((goal (mevedel-session-codec--goal-from-plist
               '(:id "g1" :objective "Ship" :status active :reason nil
                 :token-budget 1000 :tokens-used 25
                 :time-used-seconds 7 :turns-run 2
                 :plan-reference "local/plans/accepted.md"
                 :created-at "created" :updated-at "updated"))))
    (should (mevedel-goal-p goal))
    (should (equal "Ship" (mevedel-goal-objective goal)))
    (should (= 1000 (mevedel-goal-token-budget goal)))
    (should (= 25 (mevedel-goal-tokens-used goal)))
    (should (= 7 (mevedel-goal-time-used-seconds goal)))
    (should (= 2 (mevedel-goal-turns-run goal))))
  :doc "round-trips a budget-limited Goal with its exact usage and reason"
  (let ((goal (mevedel-session-codec--goal-from-plist
               '(:id "g2" :objective "Ship" :status budget-limited
                 :reason "Token budget reached: 110/100 tokens used"
                 :token-budget 100 :tokens-used 110
                 :time-used-seconds 9 :turns-run 3
                 :plan-reference nil
                 :created-at "created" :updated-at "updated"))))
    (should (eq 'budget-limited (mevedel-goal-status goal)))
    (should (= 100 (mevedel-goal-token-budget goal)))
    (should (= 110 (mevedel-goal-tokens-used goal)))
    (should (equal "Token budget reached: 110/100 tokens used"
                   (mevedel-goal-reason goal))))
  :doc "keeps sessions without a Goal empty"
  (should-not (mevedel-session-codec--goal-from-plist nil))
  :doc "rejects old, incomplete, and unsafe Goal records"
  (let ((valid '(:id "g1" :objective "Ship" :status active
                 :reason nil :token-budget nil :tokens-used 0
                 :time-used-seconds 0 :turns-run 0 :plan-reference nil
                 :created-at "created" :updated-at "updated")))
    (dolist (change '((:id "../escape")
                      (:status unknown)
                      (:tokens-used -1)
                      (:plan-reference "../escape.md")
                      (:reason 42)))
      (let ((plist (copy-tree valid)))
        (setq plist (plist-put plist (car change) (cadr change)))
        (should-error
         (mevedel-session-codec--goal-from-plist plist)
         :type 'error)))
    (should-error
     (mevedel-session-codec--goal-from-plist
      '(:id "old" :objective "Ship" :status active :phase planning)))))


;;
;;; Task round-trip

(mevedel-deftest mevedel-session-codec--task-to-plist ()
  ,test
  (test)
  :doc "captures all task fields"
  (let* ((task (mevedel-task--create
                :id 7 :subject "S" :description "D"
                :status 'pending :owner "explorer"
                :blocks '(8) :blocked-by '(5 6)
                :completed-turn 12
                :metadata '(:priority low :tag "x")))
         (plist (mevedel-session-codec--task-to-plist task)))
    (should (= 7 (plist-get plist :id)))
    (should (equal "S" (plist-get plist :subject)))
    (should (equal "D" (plist-get plist :description)))
    (should (eq 'pending (plist-get plist :status)))
    (should (equal "explorer" (plist-get plist :owner)))
    (should (equal '(8) (plist-get plist :blocks)))
    (should (equal '(5 6) (plist-get plist :blocked-by)))
    (should (= 12 (plist-get plist :completed-turn)))
    (should (equal '(:priority low :tag "x")
                   (plist-get plist :metadata)))))


(mevedel-deftest mevedel-session-codec--task-from-plist ()
  ,test
  (test)
  :doc "rebuilds a task struct from plist"
  (let* ((plist (list :id 3 :subject "X" :description nil
                      :status 'completed :owner nil
                      :blocks nil :blocked-by nil
                      :completed-turn 9 :metadata nil))
         (task (mevedel-session-codec--task-from-plist plist)))
    (should (mevedel-task-p task))
    (should (= 3 (mevedel-task-id task)))
    (should (equal "X" (mevedel-task-subject task)))
    (should (eq 'completed (mevedel-task-status task)))
    (should (= 9 (mevedel-task-completed-turn task))))

  :doc "normalizes empty owner to nil"
  (let* ((plist (list :id 4 :subject "Y" :description nil
                      :status 'pending :owner ""
                      :blocks nil :blocked-by nil :metadata nil))
         (task (mevedel-session-codec--task-from-plist plist)))
    (should (mevedel-task-p task))
    (should (null (mevedel-task-owner task)))))


;;
;;; Top-level round-trip

(mevedel-deftest mevedel-session-codec-sanitize-agent-transcripts ()
  ,test
  (test)
  :doc "preserves canonical metadata and known statuses"
  (let* ((entry '("agent--one"
                  :agent-path "/root/one"
                  :path "agents/one.chat.org"
                  :status completed
                  :updated-at "2026-07-19T10-00-00"))
         (out (mevedel-session-codec-sanitize-agent-transcripts
               (list entry))))
    (should (equal out (list entry))))

  :doc "coerces unknown historical status to incomplete"
  (let ((out
         (mevedel-session-codec-sanitize-agent-transcripts
          '(("agent--one" :agent-path "/root/one"
             :path "agents/one.chat.org" :status bogus)))))
    (should (eq 'incomplete (plist-get (cdar out) :status))))

  :doc "keeps the newest metadata for a duplicate internal id"
  (let ((out
         (mevedel-session-codec-sanitize-agent-transcripts
          '(("agent--one" :agent-path "/root/one" :status completed
             :updated-at "2026-07-19T10-00-00")
            ("agent--one" :agent-path "/root/one" :status aborted
             :updated-at "2026-07-19T11-00-00")))))
    (should (= 1 (length out)))
    (should (eq 'aborted (plist-get (cdar out) :status)))))

(mevedel-deftest mevedel-session-codec-serialize ()
  ,test
  (test)
  :doc "serializes a fully populated session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((session (test-mevedel-session-persistence--make-session root))
               (_ (setf (mevedel-session-plan-mode session) t
                        (mevedel-session-preset-name session) 'test-preset
                        (mevedel-session-model-provider session)
                        "Test:test-model"
                        (mevedel-session-reasoning-effort session) 'high))
               (plist (mevedel-session-codec-serialize
                       session
                       :first-user-message "Refactor X"
                       :latest-user-message "Ship Y"
                       :additional-roots '(("alt" . "/tmp/alt")))))
          (should (equal mevedel-session-codec-format-version
                         (plist-get plist :version)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (plist-get plist :session-id)))
          (should (equal "main" (plist-get plist :session-name)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (plist-get plist :working-directory)))
          (should (plist-member plist :target-incarnation))
          (should (stringp (plist-get plist :target-incarnation)))
          (should (equal 'ask (plist-get plist :permission-mode)))
          (should (equal 'required (plist-get plist :sandbox-mode)))
          (should (eq t (plist-get plist :plan-mode)))
          (should (eq 'test-preset (plist-get plist :preset-name)))
          (should (equal "Test:test-model"
                         (plist-get plist :model-provider)))
          (should (eq 'high (plist-get plist :reasoning-effort)))
          (should-not (plist-member plist :preset-settings))
          (should (= 2 (plist-get plist :current-segment)))
          (should (= 5 (plist-get plist :total-turn-count)))
          (should (= 4 (plist-get plist :last-task-write-turn)))
          (should (equal '((nil :note "Main status" :updated-turn 4
                                :updated-at "2026-04-23T18:20:00+0200")
                           ("main" :note "Agent status" :updated-turn 4
                            :updated-at "2026-04-23T18:21:00+0200"))
                         (plist-get plist :task-status-notes)))
          (should (equal "Refactor X" (plist-get plist :first-user-message)))
          (should (equal "Ship Y" (plist-get plist :latest-user-message)))
          (should (equal '(("alt" . "/tmp/alt"))
                         (plist-get plist :additional-roots)))
          (should (equal '(("alpha" . "Alpha helper"))
                         (plist-get plist :skills-snapshot)))
          (should (equal (mevedel-session-workspace-instruction-hashes session)
                         (plist-get plist :workspace-instruction-hashes)))
          (should (= 3 (length (plist-get plist :permission-rules))))
          (should (= 2 (length (plist-get plist :resource-grants))))
          (should (= 2 (length (plist-get plist :tasks))))
          (should (plist-get plist :workspace))
          (should (plist-get plist :prompt-index))
          (should (plist-get plist :file-snapshots)))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "stores project identity and target-native workspace paths"
  (let* ((identity (make-string 64 ?a))
         (workspace
          (mevedel-workspace--create
           :type 'project
           :id "/ssh:first:/srv/project/"
           :root "/ssh:first:/srv/project/"
           :name "project"))
         (session (mevedel-session-create "main" workspace)))
    (mevedel-execution-target-seed-incarnation
     (mevedel-session-execution-target session) "remote-host-a")
    (cl-letf (((symbol-function 'mevedel-workspace-identity-read)
               (lambda (_root) identity)))
      (let* ((sidecar (mevedel-session-codec-serialize session))
             (saved-workspace (plist-get sidecar :workspace)))
        (should (equal identity
                       (plist-get saved-workspace :workspace-id)))
        (should (equal "/srv/project/"
                       (plist-get saved-workspace :target-native-root)))
        (should (equal "/srv/project/"
                       (plist-get sidecar :working-directory)))
        (should (equal "remote-host-a"
                       (plist-get sidecar :target-incarnation)))
        (should-not (file-remote-p
                     (plist-get saved-workspace :target-native-root))))))
  :doc "stores remote session authority without a client TRAMP prefix"
  (let* ((identity (make-string 64 ?a))
         (workspace
          (mevedel-workspace--create
           :type 'project
           :id "/ssh:first:/srv/project/"
           :root "/ssh:first:/srv/project/"
           :name "project"))
         (session (mevedel-session-create "main" workspace)))
    (mevedel-execution-target-seed-incarnation
     (mevedel-session-execution-target session) "remote-host-a")
    (setf (mevedel-session-permission-rules session)
          '(("Read" :path "/ssh:first:/srv/project/src/**" :action allow)
            ("Bash" :pattern "git status"
             :file-system
             ((:path "/ssh:first:/srv/shared/input" :access read))
             :action allow))
          (mevedel-session-resource-grants session)
          '((:path "/ssh:first:/srv/project/out" :access write)))
    (cl-letf (((symbol-function 'mevedel-workspace-identity-read)
               (lambda (_root) identity)))
      (let ((sidecar (mevedel-session-codec-serialize session)))
        (should
         (equal
          '(("Read" :path "/srv/project/src/**" :action allow)
            ("Bash" :pattern "git status"
             :file-system ((:path "/srv/shared/input" :access read))
             :action allow))
          (plist-get sidecar :permission-rules)))
        (should
         (equal '((:path "/srv/project/out" :access write))
                (plist-get sidecar :resource-grants)))
        (should-not
         (string-match-p "/ssh:first:"
                         (prin1-to-string sidecar))))))
  :doc "fork fields default nil for a non-fork session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((session (test-mevedel-session-persistence--make-session root))
               (plist (mevedel-session-codec-serialize session)))
          (should (null (plist-get plist :forked-from-session-id)))
          (should (null (plist-get plist :forked-from-turn)))
          (should (null (plist-get plist :fork-type)))
          (should (null (plist-get plist :forked-from-fork-point-id)))
          (should (null (plist-get plist :worktree-directory))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "serializes durable fork type and stable fork-point lineage"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-forked-from-session-id session) "source"
                (mevedel-session-forked-from-turn session) 3
                (mevedel-session-fork-type session) 'worktree
                (mevedel-session-forked-from-fork-point-id session)
                "stable-point"
                (mevedel-session-worktree-source-root session) "/repo/"
                (mevedel-session-worktree-directory session)
                "/repo/.worktrees/main-fork-1/"
                (mevedel-session-worktree-branch session)
                "worktree/main-fork-1"
                (mevedel-session-worktree-base-commit session) "abc123")
          (let ((plist (mevedel-session-codec-serialize session)))
            (should (eq 'worktree (plist-get plist :fork-type)))
            (should (equal "stable-point"
                           (plist-get plist
                                      :forked-from-fork-point-id)))
            (should (equal "/repo/.worktrees/main-fork-1/"
                           (plist-get plist :worktree-directory)))
            (should (equal "worktree/main-fork-1"
                           (plist-get plist :worktree-branch)))
            (should (equal "abc123"
                           (plist-get plist :worktree-base-commit)))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "materializes the canonical global mode when the session inherits it"
  (let ((root (make-temp-file "mevedel-test-proj-" t))
        (saved-mode (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-permission-mode session) nil)
          (set-default-toplevel-value 'mevedel-permission-mode 'edits)
          (should (eq 'edits
                      (plist-get
                       (mevedel-session-codec-serialize session)
                       :permission-mode))))
      (set-default-toplevel-value 'mevedel-permission-mode saved-mode)
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "refuses to persist retired permission modes"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-permission-mode session) 'default)
          (should-error (mevedel-session-codec-serialize session)
                        :type 'error))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "materializes and validates the canonical global sandbox mode"
  (let ((root (make-temp-file "mevedel-test-proj-" t))
        (saved-mode (default-toplevel-value 'mevedel-sandbox-mode)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-sandbox-mode session) nil)
          (set-default-toplevel-value 'mevedel-sandbox-mode 'off)
          (should (eq 'off
                      (plist-get
                       (mevedel-session-codec-serialize session)
                       :sandbox-mode)))
          (setf (mevedel-session-sandbox-mode session) 'auto)
          (should-error (mevedel-session-codec-serialize session)
                        :type 'error))
      (set-default-toplevel-value 'mevedel-sandbox-mode saved-mode)
      (when (file-directory-p root)
        (delete-directory root t)))))


(mevedel-deftest mevedel-session-codec-validate-current-sidecar ()
  ,test
  (test)
  :doc "accepts a complete current sidecar"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (should (eq plist
                (mevedel-session-codec-validate-current-sidecar
                 plist))))
  :doc "rejects a current-version sidecar with a missing required key"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (cl-remf plist :working-directory)
    (should-error
     (mevedel-session-codec-validate-current-sidecar plist)
     :type 'error))
  :doc "requires resource grants even when none are stored"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (cl-remf plist :resource-grants)
    (should-error
     (mevedel-session-codec-validate-current-sidecar plist)
     :type 'error))
  :doc "requires a nonblank target incarnation field"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (cl-remf plist :target-incarnation)
    (should-error
     (mevedel-session-codec-validate-current-sidecar plist)
     :type 'error)
    (dolist (incarnation '(42 "" "   "))
      (should-error
       (mevedel-session-codec-validate-current-sidecar
        (plist-put plist :target-incarnation incarnation))
       :type 'error))
    (should
     (mevedel-session-codec-validate-current-sidecar
      (plist-put plist :target-incarnation "remote-host-a"))))
  :doc "accepts only canonical persisted permission modes"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (dolist (mode '(ask edits full-auto))
      (should (eq plist
                  (mevedel-session-codec-validate-current-sidecar
                   (plist-put plist :permission-mode mode)))))
    (dolist (mode '(default accept-edits trust-all edit))
      (should-error
       (mevedel-session-codec-validate-current-sidecar
       (plist-put plist :permission-mode mode))
       :type 'error)))
  :doc "accepts only canonical persisted sandbox modes"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (dolist (mode '(best-effort required off))
      (should (eq plist
                  (mevedel-session-codec-validate-current-sidecar
                   (plist-put plist :sandbox-mode mode)))))
    (should-error
     (mevedel-session-codec-validate-current-sidecar
      (plist-put plist :sandbox-mode 'auto))
     :type 'error))
  :doc "accepts only boolean persisted Plan mode"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (dolist (mode '(nil t))
      (should (eq plist
                  (mevedel-session-codec-validate-current-sidecar
                   (plist-put plist :plan-mode mode)))))
    (should-error
     (mevedel-session-codec-validate-current-sidecar
     (plist-put plist :plan-mode 'plan))
     :type 'error))
  :doc "accepts only exact model labels and symbolic effort"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (should
     (mevedel-session-codec-validate-current-sidecar
      (plist-put
       (plist-put plist :model-provider "OpenAI:gpt-5")
       :reasoning-effort 'high)))
    (dolist (provider '("gpt-5" openai 42))
      (should-error
       (mevedel-session-codec-validate-current-sidecar
        (plist-put plist :model-provider provider))))
    (should-error
     (mevedel-session-codec-validate-current-sidecar
      (plist-put
       (plist-put plist :model-provider nil)
       :reasoning-effort "high"))))
  :doc "rejects prompt entries without current turn coordinates"
  (let ((plist
         (test-mevedel-session-persistence--complete-sidecar
          '(:prompt-index ((1 . ((:turn 1 :cum-turn 1))))))))
    (should-error
     (mevedel-session-codec-validate-current-sidecar plist)
     :type 'error)))


(mevedel-deftest mevedel-session-codec-deserialize ()
  ,test
  (test)
  :doc "round-trips a populated session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((source (test-mevedel-session-persistence--make-session root))
               (_ (setf (mevedel-session-plan-mode source) t
                        (mevedel-session-preset-name source) 'test-preset
                        (mevedel-session-model-provider source)
                        "Test:test-model"
                        (mevedel-session-reasoning-effort source) 'high
                        (mevedel-session-pending-steering source)
                        '((:id 1 :input "steer"))
                        (mevedel-session-pending-follow-ups source)
                        '((:id 2 :input "later"))
                        (mevedel-session-pending-input-next-id source) 3
                        (mevedel-session-pending-input-paused source) t
                        (mevedel-session-pending-input-failure-paused source)
                        t
                        (mevedel-session-agent-transcripts source)
                        '(("agent--one" :agent-path "/root/one"
                           :description "Historical agent"
                           :path "agents/one.chat.org"
                           :status completed :parent-turn 3))
                        (mevedel-session-messages source)
                        '((:type RESULT :sender "/root/worker"
                           :recipient "/root" :outcome completed
                           :payload "done" :timestamp (12345 67890 0 0))
                          (:type MAIL :sender "/root/reviewer"
                           :recipient "/root" :payload "note"
                           :timestamp (12345 67891 0 0)))))
               (plist (mevedel-session-codec-serialize
                       source
                       :first-user-message "Hi"
                       :latest-user-message "Later"))
               (result
                (mevedel-session-codec-deserialize
                 plist (mevedel-session-workspace source)))
               (session (plist-get result :session)))
          (should (mevedel-session-p session))
          (should (equal "main" (mevedel-session-name session)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (mevedel-session-working-directory session)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (mevedel-session-session-id session)))
          (should (eq 'ask (mevedel-session-permission-mode session)))
          (should (eq 'required (mevedel-session-sandbox-mode session)))
          (should (mevedel-session-plan-mode session))
          (should (eq 'test-preset (mevedel-session-preset-name session)))
          (should (equal "Test:test-model"
                         (mevedel-session-model-provider session)))
          (should (eq 'high
                      (mevedel-session-reasoning-effort session)))
          (should (= 5 (mevedel-session-turn-count session)))
          (should (= 4 (mevedel-session-last-task-write-turn session)))
          (should (equal '(("alpha" . "Alpha helper"))
                         (mevedel-session-skills-snapshot session)))
          (should (equal
                   (mevedel-session-workspace-instruction-hashes source)
                   (mevedel-session-workspace-instruction-hashes session)))
          (should (equal '((nil :note "Main status" :updated-turn 4
                                :updated-at "2026-04-23T18:20:00+0200")
                           ("main" :note "Agent status" :updated-turn 4
                            :updated-at "2026-04-23T18:21:00+0200"))
                         (mevedel-session-task-status-notes session)))
          (should (= 2 (mevedel-session-current-segment session)))
          (should (= 2 (length (mevedel-session-tasks session))))
          (should
           (equal (mevedel-session-agent-transcripts source)
                  (mevedel-session-agent-transcripts session)))
          (let ((messages (mevedel-session-messages session)))
            (should (= 2 (length messages)))
            (should (equal "/root/worker"
                           (plist-get (car messages) :sender)))
            (should (eq 'completed
                        (plist-get (car messages) :outcome)))
            (should (equal "done" (plist-get (car messages) :payload)))
            (should (equal "/root/reviewer"
                           (plist-get (cadr messages) :sender)))
            (should (equal "note"
                           (plist-get (cadr messages) :payload))))
          (should (= 3 (mevedel-task-completed-turn
                        (car (mevedel-session-tasks session)))))
          (should (= 3 (length (mevedel-session-permission-rules session))))
          (should (equal '((:path "/tmp/exact-read" :access read)
                           (:path "/tmp/exact-write" :access write))
                         (mevedel-session-resource-grants session)))
          (should (equal "Hi" (plist-get result :first-user-message)))
          (should (equal "Later" (plist-get result :latest-user-message)))
          (dolist (key '(:pending-steering :pending-follow-ups
                         :pending-input-next-id :pending-input-paused
                         :pending-input-failure-paused))
            (should-not (plist-member plist key)))
          (should-not (mevedel-session-pending-input-p session))
          (should-not (mevedel-session-pending-input-next-id session))
          (should-not (mevedel-session-pending-input-paused session))
          (should-not
           (mevedel-session-pending-input-failure-paused session))
          ;; touched-files / mentions-shown reset to empty hash tables
          (should (hash-table-p (mevedel-session-touched-files session)))
          (should (zerop (hash-table-count (mevedel-session-touched-files session))))
          (should (hash-table-p (mevedel-session-mentions-shown session)))
          (should (zerop (hash-table-count (mevedel-session-mentions-shown session))))
          ;; workspace identity recovered
          (let ((workspace (mevedel-session-workspace session)))
            (should (eq 'project (mevedel-workspace-type workspace)))
            (should (equal "test-id" (mevedel-workspace-id workspace))))
          (should
           (equal
            (mevedel-execution-target-incarnation
             (mevedel-session-execution-target session))
            (plist-get plist :target-incarnation)))
          (should (mevedel-session-execution-target session)))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "rejects the superseded session format without a compatibility reader"
  (let ((sidecar
         (test-mevedel-session-persistence--complete-sidecar
          '(:version "v0.5.0"))))
    (should-error
     (mevedel-session-codec-deserialize
      sidecar
      (mevedel-workspace--create
       :type 'project :id "/tmp/old-format/"
       :root "/tmp/old-format/" :name "old-format"))
     :type 'error))

  :doc "identity rebinding drops copied authority and additional roots"
  (let ((saved-root (make-temp-file "mevedel-rebind-saved-" t))
        (opened-root (make-temp-file "mevedel-rebind-opened-" t)))
    (unwind-protect
        (let* ((saved
                (test-mevedel-session-persistence--make-workspace saved-root))
               (saved-plist
                (mevedel-session-codec--workspace-to-plist saved))
               (opened
                (progn
                  (mevedel-workspace-clear-registry)
                  (test-mevedel-session-persistence--make-workspace
                   opened-root)))
               (opened-identity
                (mevedel-workspace-identity-read opened-root))
               (sidecar
                (test-mevedel-session-persistence--complete-sidecar
                 (list
                  :workspace saved-plist
                  :working-directory saved-root
                  :permission-rules
                  `(("Read" :path ,(file-name-concat saved-root "src/**")
                            :action allow))
                  :resource-grants
                  `((:path ,(file-name-concat saved-root "out")
                            :access write))
                  :additional-roots '(("shared" . "/srv/shared/"))))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-codec-deserialize sidecar opened)
             :type 'user-error))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (let* ((result
                    (mevedel-session-codec-deserialize sidecar opened))
                   (session (plist-get result :session))
                   (saved-again
                    (mevedel-session-codec-serialize session)))
              (should-not (mevedel-session-permission-rules session))
              (should-not (mevedel-session-resource-grants session))
              (should-not (plist-get result :additional-roots))
              (should
               (equal opened-identity
                      (plist-get (plist-get saved-again :workspace)
                                 :workspace-id))))))
      (delete-directory saved-root t)
      (delete-directory opened-root t)
      (mevedel-workspace-clear-registry)))

  :doc "requalifies target-native authority through the opened remote alias"
  (let* ((identity (make-string 64 ?a))
         (workspace
          (mevedel-workspace--create
           :type 'project
           :id "/ssh:second:/srv/project/"
           :root "/ssh:second:/srv/project/"
           :name "project"))
         (sidecar
          (test-mevedel-session-persistence--complete-sidecar
           (list
            :workspace
            (list :type 'project
                  :workspace-id identity
                  :target-native-root "/srv/project/"
                  :name "project")
            :working-directory "/srv/project/"
            :target-incarnation "remote-host-a"
            :permission-rules
            '(("Read" :path "/srv/project/src/**" :action allow)
              ("Bash" :pattern "git status"
               :file-system ((:path "/srv/shared/input" :access read))
               :action allow))
            :resource-grants
            '((:path "/srv/project/out" :access write))))))
    (cl-letf (((symbol-function 'mevedel-workspace-identity-read)
               (lambda (_root) identity))
              ((symbol-function 'file-in-directory-p)
               (lambda (_file _directory) t)))
      (let ((session
             (plist-get
              (mevedel-session-codec-deserialize sidecar workspace)
              :session)))
        (should
         (equal "remote-host-a"
                (mevedel-execution-target-incarnation
                 (mevedel-session-execution-target session))))
        (should
         (equal
          '(("Read" :path "/ssh:second:/srv/project/src/**" :action allow)
            ("Bash" :pattern "git status"
             :file-system
             ((:path "/ssh:second:/srv/shared/input" :access read))
             :action allow))
          (mevedel-session-permission-rules session)))
        (should
         (equal
          '((:path "/ssh:second:/srv/project/out" :access write))
          (mevedel-session-resource-grants session))))))

  :doc "drops persisted opaque and unknown agent task owners"
  (let ((root (make-temp-file "mevedel-task-owner-roundtrip-" t)))
    (unwind-protect
        (progn
          (mevedel-tools-register)
          (let* ((source
                (test-mevedel-session-persistence--make-session root))
               (configuration
                (test-mevedel-session-persistence--agent-configuration))
               (worker-id
                "worker--0123456789abcdef0123456789abcdef")
               (reviewer-id
                "reviewer--fedcba9876543210fedcba9876543210")
               (valid-owner "/root/worker/reviewer"))
          (setf
           (mevedel-session-agent-registry source)
           (list
            (cons
             "/root/worker"
             (mevedel-agent-record--create
              :id worker-id
              :path "/root/worker"
              :parent-path "/root"
              :role "default"
              :configuration configuration
              :activity 'idle
              :conversation-location "agents/worker.chat.org"))
            (cons
             valid-owner
             (mevedel-agent-record--create
              :id reviewer-id
              :path valid-owner
              :parent-path "/root/worker"
              :role "default"
              :configuration configuration
              :activity 'idle
              :conversation-location "agents/reviewer.chat.org")))
           (mevedel-session-tasks source)
           (list
            (mevedel-task--create
             :id 1 :subject "valid nested" :status 'pending
             :owner valid-owner :blocks '(2) :blocked-by '(3))
            (mevedel-task--create
             :id 2 :subject "opaque" :status 'pending
             :owner worker-id)
            (mevedel-task--create
             :id 3 :subject "unknown" :status 'pending
             :owner "/root/ghost"))
           (mevedel-session-task-status-notes source)
           (list
            (list valid-owner :note "valid")
            (list worker-id :note "opaque")
            (list "/root/ghost" :note "unknown")))
          (let* ((sidecar (mevedel-session-codec-serialize source))
                 (restored
                  (plist-get
                   (mevedel-session-codec-deserialize
                    sidecar (mevedel-session-workspace source))
                   :session)))
            (should
             (equal (list valid-owner)
                    (mapcar #'mevedel-task-owner
                            (mevedel-session-tasks restored))))
            (should-not
             (mevedel-task-blocks (car (mevedel-session-tasks restored))))
            (should-not
             (mevedel-task-blocked-by
              (car (mevedel-session-tasks restored))))
            (should
             (equal (list valid-owner)
                    (mapcar #'car
                            (mevedel-session-task-status-notes restored))))
            (should (= 2 (length
                          (mevedel-session-agent-registry restored)))))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-tool-clear-registry)
      (mevedel-workspace-clear-registry)))

  :doc "preserves an accepted standalone Plan implementation retry"
  (let* ((retry
          '(:step submit
            :selection (:location here :context summary
                        :execution direct :mode edits)
            :accepted (:path "local/plans/accepted.md"
                       :absolute-path "/tmp/session/local/plans/accepted.md"
                       :hash "abc")
            :summary "# Handoff"
            :failure "Transport refused"))
         (metadata (list :status 'accepted :implementation-retry retry))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :plan-metadata metadata)))
         (session (plist-get result :session)))
    (should (equal metadata (mevedel-session-plan-metadata session))))

  :doc "preserves a prepared standalone Plan Worktree target"
  (let* ((retry
          '(:step submit
            :selection (:location worktree :context fresh
                        :execution direct :mode full-auto
                        :branch "plan/topic")
            :accepted (:path "local/plans/source.md"
                       :absolute-path "/tmp/source/local/plans/source.md"
                       :hash "abc")
            :target-directory "/tmp/repo/.worktrees/topic/"
            :target-save-path "/tmp/repo/.mevedel/target/"
            :target-session-id "target-id"
            :target-accepted
            (:path "local/plans/accepted.md"
             :absolute-path "/tmp/repo/.mevedel/target/local/plans/accepted.md"
             :hash "abc")))
         (metadata (list :status 'accepted :implementation-retry retry))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :plan-metadata metadata)))
         (session (plist-get result :session)))
    (should (equal metadata (mevedel-session-plan-metadata session))))

  :doc "demotes a persisted active Goal to paused on session resume"
  (let* ((goal '(:id "g1" :objective "Ship" :status active :reason nil
                 :token-budget nil :tokens-used 9 :time-used-seconds 4
                 :turns-run 2 :plan-reference nil
                 :created-at "created" :updated-at "updated"))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :goal goal)))
         (restored (mevedel-session-goal (plist-get result :session))))
    (should (eq 'paused (mevedel-goal-status restored)))
    (should (equal "session resumed" (mevedel-goal-reason restored)))
    (should (= 9 (mevedel-goal-tokens-used restored))))

  :doc "loads an old Goal schema as no Goal"
  (let* ((old '(:id "g1" :objective "Ship" :status active
                :phase planning :approval-policy supervised))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :goal old))))
    (should-not (mevedel-session-goal (plist-get result :session))))
  :doc "drops permission rules with unknown actions"
  (let* ((plist (list :version mevedel-session-codec-format-version
                      :session-name "x"
                      :permission-rules
                      '(("Read"  :path "/x" :action allow)
                        ("Write" :path "/y" :action future-action))
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil))
         (session
          (plist-get
           (test-mevedel-session-persistence--deserialize-sidecar plist)
           :session)))
    (should (= 1 (length (mevedel-session-permission-rules session)))))
  :doc "preserves relocated working directories under the new workspace root"
  (let* ((old-root (make-temp-file "mevedel-old-root-" t))
         (new-root (make-temp-file "mevedel-new-root-" t))
         (old-cwd (file-name-concat old-root "packages/api"))
         (new-cwd (file-name-concat new-root "packages/api")))
    (unwind-protect
        (progn
          (make-directory old-cwd t)
          (make-directory new-cwd t)
          (let* ((saved-workspace
                  (test-mevedel-session-persistence--make-workspace old-root))
                 (saved-plist
                  (mevedel-session-codec--workspace-to-plist
                   saved-workspace)))
            (make-directory (file-name-concat new-root ".mevedel") t)
            (copy-file
             (file-name-concat old-root ".mevedel" "workspace-id")
             (file-name-concat new-root ".mevedel" "workspace-id"))
            (mevedel-workspace-clear-registry)
            (let* ((opened-workspace
                    (mevedel-workspace-get-or-create
                     'project new-root new-root "relocated-proj"))
                   (sidecar
                    (test-mevedel-session-persistence--complete-sidecar
                     (list :workspace saved-plist
                           :working-directory old-cwd)))
                   (session
                    (plist-get
                     (mevedel-session-codec-deserialize
                      sidecar opened-workspace)
                     :session)))
            (should (equal (file-name-as-directory new-cwd)
                           (mevedel-session-working-directory session))))))
      (mevedel-workspace-clear-registry)
      (when (file-directory-p old-root)
        (delete-directory old-root t))
      (when (file-directory-p new-root)
        (delete-directory new-root t))))

  :doc "preserves saved working directories already under a nested current root"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "mevedel-old-root-" t)))
         (new-root (file-name-as-directory
                    (file-name-concat old-root "packages" "api")))
         (saved-cwd new-root))
    (unwind-protect
        (progn
          (make-directory new-root t)
          (let* ((saved-workspace
                  (test-mevedel-session-persistence--make-workspace old-root))
                 (saved-plist
                  (mevedel-session-codec--workspace-to-plist
                   saved-workspace)))
            (make-directory (file-name-concat new-root ".mevedel") t)
            (copy-file
             (file-name-concat old-root ".mevedel" "workspace-id")
             (file-name-concat new-root ".mevedel" "workspace-id"))
            (mevedel-workspace-clear-registry)
            (let* ((opened-workspace
                    (mevedel-workspace-get-or-create
                     'project new-root new-root "nested-proj"))
                   (sidecar
                    (test-mevedel-session-persistence--complete-sidecar
                     (list :workspace saved-plist
                           :working-directory saved-cwd)))
                   (session
                    (plist-get
                     (mevedel-session-codec-deserialize
                      sidecar opened-workspace)
                     :session)))
              (should (equal new-root
                             (mevedel-session-working-directory session))))))
      (mevedel-workspace-clear-registry)
      (when (file-directory-p old-root)
        (delete-directory old-root t))))

  :doc "rejects restored working directories outside the workspace"
  (let ((root (make-temp-file "mevedel-restore-root-" t))
        (outside (make-temp-file "mevedel-restore-outside-" t)))
    (unwind-protect
        (let* ((workspace
                (test-mevedel-session-persistence--make-workspace root))
               (sidecar
                (test-mevedel-session-persistence--complete-sidecar
                 (list
                  :workspace
                  (mevedel-session-codec--workspace-to-plist workspace)
                  :working-directory outside))))
          (should-error
           (mevedel-session-codec-deserialize sidecar workspace)
           :type 'user-error))
      (delete-directory root t)
      (delete-directory outside t)
      (mevedel-workspace-clear-registry)))

  :doc "rejects restored symlink working directories outside the workspace"
  (let* ((root (make-temp-file "mevedel-restore-root-" t))
         (outside (make-temp-file "mevedel-restore-outside-" t))
         (link (file-name-concat root "linked-cwd")))
    (unwind-protect
        (let* ((workspace
                (test-mevedel-session-persistence--make-workspace root))
               (sidecar
                (test-mevedel-session-persistence--complete-sidecar
                 (list
                  :workspace
                  (mevedel-session-codec--workspace-to-plist workspace)
                  :working-directory link))))
          (make-symbolic-link outside link)
          (should-error
           (mevedel-session-codec-deserialize sidecar workspace)
           :type 'user-error))
      (when (file-symlink-p link)
        (delete-file link))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p outside)
        (delete-directory outside t))
      (mevedel-workspace-clear-registry))))


;;
;;; Sidecar IO

(mevedel-deftest mevedel-session-codec-write ()
  ,test
  (test)
  :doc "atomic write produces a readable plist"
  (let ((tmp (make-temp-file "mevedel-session-meta-test-" nil ".el")))
    (unwind-protect
        (let* ((plist `(:version ,mevedel-session-codec-format-version
                                 :session-name "main"
                                 :tasks nil
                                 :permission-rules nil)))
          (mevedel-session-codec-write tmp plist)
          (should (file-exists-p tmp))
          (let ((readback (mevedel-session-codec-read tmp)))
            (should (equal "main" (plist-get readback :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "atomic write preserves shared objects as readable circle syntax"
  (let* ((tmp (make-temp-file "mevedel-session-meta-test-" nil ".el"))
         (shared (propertize "prompt" 'mevedel-mention-binding
                             (list :token "prompt")))
         (plist (list :history (list shared shared))))
    (unwind-protect
        (progn
          (mevedel-session-codec-write tmp plist)
          (let* ((readback (mevedel-session-codec-read tmp))
                 (history (plist-get readback :history)))
            (should (eq (car history) (cadr history)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'test-mevedel-session-codec)
;;; test-mevedel-session-codec.el ends here
