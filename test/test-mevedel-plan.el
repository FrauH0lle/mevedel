;;; test-mevedel-plan.el --- Tests for mevedel-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-plan)
(require 'mevedel-interaction-prompt)
(require 'mevedel-prompt-submission)
(require 'mevedel-structs)
(require 'mevedel-view-composer)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-plan-mode-active-p
  (:doc "reads Plan state from the explicit or current session")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "test" :plan-mode t)))
    (should (mevedel-plan-mode-active-p session))
    (let ((mevedel--session session))
      (should (mevedel-plan-mode-active-p)))))

(mevedel-deftest mevedel-plan-mode-enter
  (:doc "enters Plan without changing the underlying permission mode")
  ,test
  (test)
  (let ((session (mevedel-session--create
                  :name "test" :permission-mode 'full-auto)))
    (mevedel-plan-mode-enter session)
    (should (mevedel-session-plan-mode session))
    (should (eq 'full-auto (mevedel-session-permission-mode session))))

  :doc "rejects every unfinished Goal status without changing state"
  (dolist (status '(active paused blocked))
    (let ((session
           (mevedel-session--create
            :name "test" :permission-mode 'auto
            :goal (mevedel-goal--create :status status))))
      (should-error (mevedel-plan-mode-enter session) :type 'user-error)
      (should-not (mevedel-session-plan-mode session))
      (should (eq 'auto (mevedel-session-permission-mode session)))))

  :doc "allows a completed Goal to remain as history"
  (let ((session
         (mevedel-session--create
          :name "test" :goal (mevedel-goal--create :status 'complete))))
    (should (mevedel-plan-mode-enter session))))

(mevedel-deftest mevedel-plan-mode-exit
  (:doc "leaves Plan without changing the underlying permission mode")
  ,test
  (test)
  (let ((session (mevedel-session--create
                  :name "test" :permission-mode 'auto :plan-mode t)))
    (mevedel-plan-mode-exit session)
    (should-not (mevedel-session-plan-mode session))
    (should (eq 'auto (mevedel-session-permission-mode session)))))

(mevedel-deftest mevedel-plan-mode--default-selection
  (:doc "defaults to Here, Current, Direct, and the underlying mode")
  ,test
  (test)
  (let ((session (mevedel-session--create
                  :name "test" :permission-mode 'auto)))
    (should (equal '(:location here :context current
                     :execution direct :mode auto)
                   (mevedel-plan-mode--default-selection session)))
    (should (eq 'auto (mevedel-session-permission-mode session)))))

(mevedel-deftest mevedel-plan-mode--render-approval
  (:doc "renders compact Direct axes without applying Mode before acceptance")
  ,test
  (test)
  (let* ((session (mevedel-session--create
                   :name "test" :permission-mode 'auto))
         (data-buffer (generate-new-buffer " *plan-approval-data*"))
         (view-buffer (generate-new-buffer " *plan-approval-view*"))
         (selection (mevedel-plan-mode--default-selection session))
         (entry (mevedel-plan-mode--approval-entry
                 "# Plan" data-buffer session selection))
         descriptor outcome rerendered)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_buffer) view-buffer))
                  ((symbol-function 'mevedel-view--fontify-as)
                   (lambda (text _mode) text))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (value)
                     (setq descriptor value)
                     (make-overlay (point-min) (point-min))))
                  ((symbol-function 'mevedel--prompt--settle)
                   (lambda (_overlay value) (setq outcome value)))
                  ((symbol-function 'mevedel-plan-approval-render)
                   (lambda (&rest _) (setq rerendered t))))
          (mevedel-plan-mode--render-approval entry)
          (let ((body (plist-get descriptor :body))
                (keymap (plist-get descriptor :keymap)))
            (dolist (text '("Location   Here" "Context    Current"
                            "Execution  Direct" "Mode       auto"))
              (should (string-match-p text body)))
            (call-interactively (lookup-key keymap (kbd "m")))
            (should rerendered)
            (should (eq 'full-auto (plist-get selection :mode)))
            (should (eq 'auto (mevedel-session-permission-mode session)))
            (call-interactively (lookup-key keymap (kbd "RET")))
            (should (equal '(:accept t
                             :selection (:location here :context current
                                         :execution direct :mode full-auto))
                           outcome))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-plan-mode--post-response
  (:doc "only root-assistant prose creates one proposal per rendered turn")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-proposal-" t))
         (session (mevedel-session--create
                   :name "test" :save-path save-dir :plan-mode t))
         entries)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((start (point)))
            (insert "<proposed_plan>\n# Root\n<detail>keep</detail>\n</proposed_plan>\n")
            (add-text-properties start (point) '(gptel response)))
          (let ((start (point)))
            (insert "<proposed_plan>\n# Tool\n</proposed_plan>\n")
            (add-text-properties start (point) '(gptel (tool . "call-1"))))
          (cl-letf (((symbol-function 'mevedel-plan-approval-present)
                     (lambda (entry &optional _session) (push entry entries))))
            (mevedel-plan-mode--post-response (point-min) (point-max))
            (mevedel-plan-mode--post-response (point-min) (point-max)))
          (should (= 1 (length entries)))
          (should (equal "# Root\n<detail>keep</detail>"
                         (plist-get (car entries) :body)))
          (should (equal '(:location here :context current
                           :execution direct :mode ask)
                         (plist-get (car entries) :selection)))
          (should (equal 'proposed
                         (plist-get (mevedel-session-plan-metadata session)
                                    :status))))
      (delete-directory save-dir t)))

  :doc "tool output alone cannot create a proposal"
  (let ((session (mevedel-session--create :name "test" :plan-mode t))
        presented)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (insert "<proposed_plan>\n# Tool only\n</proposed_plan>")
      (add-text-properties (point-min) (point-max)
                           '(gptel (tool . "call-1")))
      (cl-letf (((symbol-function 'mevedel-plan-approval-present)
                 (lambda (&rest _) (setq presented t))))
        (mevedel-plan-mode--post-response (point-min) (point-max)))
      (should-not presented)))

  :doc "injected agent output cannot create a proposal"
  (let ((session (mevedel-session--create :name "test" :plan-mode t))
        presented)
    (with-temp-buffer
      (insert "<agent-result sender=\"/root/worker\" recipient=\"/root\">\n"
              "<proposed_plan>\n# Agent only\n</proposed_plan>\n"
              "</agent-result>\n")
      (add-text-properties (point-min) (point-max) '(gptel response))
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-plan-approval-present)
                 (lambda (&rest _) (setq presented t))))
        (mevedel-plan-mode--post-response (point-min) (point-max)))
      (should-not presented))))

(mevedel-deftest mevedel-plan-mode--approval-callback
  (:doc "accepts immutably and starts one canonical Direct turn")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-direct-" t))
         (session (mevedel-session--create
                   :name "test" :save-path save-dir
                   :permission-mode 'auto :plan-mode t))
         (data-buffer (generate-new-buffer " *plan-direct-data*"))
         (view-buffer (generate-new-buffer " *plan-direct-view*"))
         hook-input implementation)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session
                        mevedel-permission-mode 'auto))
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (setq hook-input input)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action) (setq implementation action))))
            (mevedel-plan-mode--approval-callback
             "# Accepted\n\nDo it." data-buffer session
             '(:accept t
               :selection (:location here :context current
                           :execution direct :mode full-auto))))
          (let* ((metadata (mevedel-session-plan-metadata session))
                 (accepted (plist-get metadata :accepted-absolute-path)))
            (should-not (mevedel-session-plan-mode session))
            (should (eq 'full-auto
                        (mevedel-session-permission-mode session)))
            (should (eq 'approved (plist-get metadata :status)))
            (should-not (plist-member metadata :verification-pending))
            (should (file-exists-p accepted))
            (should (string-match-p (regexp-quote accepted) hook-input))
            (should (string-match-p "# Accepted" hook-input))
            (should (eq 'current (plist-get implementation :context)))
            (should (equal accepted
                           (plist-get implementation :plan-file)))
            (should-not (mevedel-session-goal session))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-validate
  (:doc "normalizes nonblank plans and rejects invalid input")
  ,test
  (test)
  (should (equal "# Plan" (mevedel-plan-validate "# Plan")))
  (should-error (mevedel-plan-validate "  \n"))
  (should-error (mevedel-plan-validate nil)))

(mevedel-deftest mevedel-plan-extract-proposed
  (:doc "extracts the last line-oriented proposed-plan block")
  ,test
  (test)
  (should (equal
           "second"
           (mevedel-plan-extract-proposed
            "<proposed_plan>\nfirst\n</proposed_plan>\n<proposed_plan>\nsecond\n</proposed_plan>")))
  (should-not
   (mevedel-plan-extract-proposed
    "text <proposed_plan>\nnot a plan\n</proposed_plan>"))
  :doc "accepts a concise authoritative reference without template headings"
  (should
   (equal
    "Implement ticket 2 in .scratch/feature/tickets.md."
    (mevedel-plan-extract-proposed
     "<proposed_plan>\nImplement ticket 2 in .scratch/feature/tickets.md.\n</proposed_plan>"))))

(mevedel-deftest mevedel-plan-strip-proposed
  (:doc "removes complete and streaming proposed-plan blocks")
  ,test
  (test)
  (should (equal
           "Intro\nTail"
           (mevedel-plan-strip-proposed
            "Intro\n<proposed_plan>\n# Plan\n</proposed_plan>\nTail")))
  (should (equal
           "Intro"
           (mevedel-plan-strip-proposed
            "Intro\n<proposed_plan>\n# Streaming plan\n"))))

(mevedel-deftest mevedel-plan--metadata-put
  (:doc "updates one plan metadata key without dropping others")
  ,test
  (test)
  (let ((session
         (mevedel-session--create :name "test" :plan-metadata '(:old t))))
    (mevedel-plan--metadata-put session :new 1)
    (should (equal '(:old t :new 1)
                   (mevedel-session-plan-metadata session)))))

(mevedel-deftest mevedel-plan-hash
  (:doc "ignores trailing whitespace in stable plan hashes")
  ,test
  (test)
  (should (equal (mevedel-plan-hash "# Plan")
                 (mevedel-plan-hash "# Plan\n"))))

(mevedel-deftest mevedel-plan-known-p
  (:doc "recognizes a hash recorded in session metadata")
  ,test
  (test)
  (let* ((plan "# Plan")
         (session
          (mevedel-session--create
           :name "test"
           :plan-metadata
           (list :presented-plan-hashes (list (mevedel-plan-hash plan))))))
    (should (mevedel-plan-known-p plan session))
    (should-not (mevedel-plan-known-p "# Other" session))))

(mevedel-deftest mevedel-plan-current-path
  (:doc "returns the current artifact path below the session directory")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-path-" t)))
    (unwind-protect
        (let ((session
               (mevedel-session--create :name "test" :save-path save-dir)))
          (should (equal (file-name-concat save-dir "plans" "current.md")
                         (mevedel-plan-current-path session)))
          (should (equal (file-name-concat save-dir "goals" "g1" "current.md")
                         (mevedel-plan-current-path
                          session nil "goals/g1/current.md"))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan--metadata-path
  (:doc "resolves the current artifact from persisted metadata")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-metadata-path-" t)))
    (unwind-protect
        (let ((session
               (mevedel-session--create
                :name "test" :save-path save-dir
                :plan-metadata '(:path "plans/current.md"))))
          (should (equal (file-name-concat save-dir "plans" "current.md")
                         (mevedel-plan--metadata-path session))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-write-current
  (:doc "writes a validated current artifact and returns its descriptor")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-write-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (mevedel-session--create :name "test" :save-path save-dir))
                 (artifact
                  (mevedel-plan-write-current "# Plan" session
                                              (current-buffer)
                                              "goals/g1/current.md")))
            (should (file-exists-p (plist-get artifact :absolute-path)))
            (should (equal "goals/g1/current.md"
                           (plist-get artifact :path)))
            (should (equal 'presented
                           (plist-get (mevedel-session-plan-metadata session)
                                      :status)))
            (should
             (equal (mevedel-plan-hash "# Plan")
                    (plist-get (mevedel-session-plan-metadata session)
                               :hash)))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-archive-accepted
  (:doc "reuses an identical deterministic accepted artifact")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-archive-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (mevedel-session--create :name "test" :save-path save-dir))
                 (artifact
                  (mevedel-plan-write-current
                   "# Plan" session (current-buffer) "current.md"))
                 (accepted
                  (mevedel-plan-archive-accepted
                   artifact session "goals/g1/cycle-001-plan.md")))
            (should (file-exists-p (plist-get accepted :absolute-path)))
            (should (equal (plist-get artifact :hash)
                           (plist-get accepted :hash)))
            (should (equal "goals/g1/cycle-001-plan.md"
                           (plist-get accepted :path)))
            (should
             (equal accepted
                    (mevedel-plan-archive-accepted
                     artifact session
                     "goals/g1/cycle-001-plan.md")))
            (let ((different
                   (mevedel-plan-write-current
                    "# Different" session (current-buffer) "current.md")))
              (should-error
               (mevedel-plan-archive-accepted
                different session "goals/g1/cycle-001-plan.md")
               :type 'error))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-current-body
  (:doc "reads the normalized current artifact body")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-body-" t))
         (path (file-name-concat save-dir "current.md")))
    (unwind-protect
        (progn
          (write-region "# Plan" nil path nil 'silent)
          (let ((session
                 (mevedel-session--create
                  :name "test" :save-path save-dir
                  :plan-metadata '(:path "current.md"))))
            (should (equal "# Plan" (mevedel-plan-current-body session)))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-current-exists-p
  (:doc "reports whether the recorded current artifact exists")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-exists-" t))
         (path (file-name-concat save-dir "current.md"))
         (session
          (mevedel-session--create
           :name "test" :save-path save-dir
           :plan-metadata '(:path "current.md"))))
    (unwind-protect
        (progn
          (should-not (mevedel-plan-current-exists-p session))
          (write-region "# Plan" nil path nil 'silent)
          (should (mevedel-plan-current-exists-p session)))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mark-approved
  (:doc "records current and accepted artifact descriptors")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "test" :turn-count 3)))
    (mevedel-plan-mark-approved
     session '(:path "current.md" :absolute-path "/tmp/current.md")
     '(:path "accepted.md" :absolute-path "/tmp/accepted.md"))
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should (eq 'approved (plist-get metadata :status)))
      (should (= 3 (plist-get metadata :approved-turn)))
      (should (equal "accepted.md" (plist-get metadata :accepted-path))))))

(mevedel-deftest mevedel-plan-accept
  (:doc "accepts a plan without depending on Goal controller state")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-accept-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test"
                          :save-path save-dir
                          :permission-mode 'full-auto
                          :turn-count 4)))
            (let* ((result (mevedel-plan-accept
                            "# Plan\n\nDo it." session (current-buffer)
                            nil
                            "goals/g1/current.md"
                            "goals/g1/cycle-001-plan.md"))
                   (current (plist-get result :current))
                   (accepted (plist-get result :accepted))
                   (metadata (mevedel-session-plan-metadata session)))
              (should (eq 'approved (plist-get metadata :status)))
              (should (file-exists-p
                       (plist-get current :absolute-path)))
              (should (file-exists-p
                       (plist-get accepted :absolute-path)))
              (should (equal (mevedel-plan-hash "# Plan\n\nDo it.")
                             (plist-get current :hash)))
              (should (equal "goals/g1/cycle-001-plan.md"
                             (plist-get accepted :path))))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-implementation-input
  (:doc "builds and validates explicit implementation input")
  ,test
  (test)
  (let ((path (make-temp-file "mevedel-plan-input-")))
    (unwind-protect
        (let ((input
               (mevedel-plan-implementation-input
                'focused (list :absolute-path path)
                'auto "Goal ID: g1")))
          (should (equal 'focused (plist-get input :context)))
          (should (equal path (plist-get input :plan-file)))
          (should (equal 'auto
                         (plist-get input :permission-mode)))
          (should-error
           (mevedel-plan-implementation-input
            'unknown (list :absolute-path path) 'ask "Goal ID: g1"))
          (should-error
           (mevedel-plan-implementation-input
            'focused (list :absolute-path path) 'ask nil))
          (should-error
           (mevedel-plan-implementation-input
            'full (list :absolute-path path) 'ask nil)))
      (delete-file path))))

(mevedel-deftest mevedel-plan-clear-verification-pending
  (:doc "clears the approved-plan verification flag")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :name "test" :plan-metadata '(:verification-pending t))))
    (mevedel-plan-clear-verification-pending session)
    (should-not
     (plist-get (mevedel-session-plan-metadata session)
                :verification-pending))))

(provide 'test-mevedel-plan)
;;; test-mevedel-plan.el ends here
