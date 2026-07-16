;;; test-mevedel-plan.el --- Tests for mevedel-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-plan)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

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
                                      :status)))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-archive-accepted
  (:doc "copies a current artifact to an immutable accepted artifact")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-archive-" t))
         (path (file-name-concat save-dir "current.md")))
    (unwind-protect
        (progn
          (write-region "# Plan" nil path nil 'silent)
          (let* ((session
                  (mevedel-session--create :name "test" :save-path save-dir))
                 (accepted
                  (mevedel-plan-archive-accepted
                   (list :absolute-path path) session
                   "goals/g1/cycle-001-plan.md")))
            (should (file-exists-p (plist-get accepted :absolute-path)))
            (should (equal "goals/g1/cycle-001-plan.md"
                           (plist-get accepted :path)))
            (should-error
             (mevedel-plan-archive-accepted
              (list :absolute-path path) session
              "goals/g1/cycle-001-plan.md"))))
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
