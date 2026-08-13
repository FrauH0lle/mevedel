;;; test-mevedel-plan.el --- Tests for mevedel-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-plan)
(require 'mevedel-session-persistence)
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

(mevedel-deftest mevedel-plan-current-path
  (:doc "returns the current artifact path below the session directory")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-path-" t)))
    (unwind-protect
        (let ((session
               (mevedel-session--create :name "test" :save-path save-dir)))
          (should (equal (file-name-concat save-dir "local" "plans" "current.md")
                         (mevedel-plan-current-path session)))
          (should-error
           (mevedel-plan-current-path session nil "goals/g1/current.md")
           :type 'wrong-number-of-arguments))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan--metadata-path
  (:doc "resolves the canonical current artifact despite stale metadata")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-metadata-path-" t)))
    (unwind-protect
        (let ((session
               (mevedel-session--create
                :name "test" :save-path save-dir
                :plan-metadata '(:path "plans/current.md"
                                 :absolute-path "/tmp/stale-current.md"))))
          (should (equal (file-name-concat save-dir "local" "plans" "current.md")
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
                                              (current-buffer))))
            (should (file-exists-p (plist-get artifact :absolute-path)))
            (should (equal "local/plans/current.md"
                           (plist-get artifact :path)))
            (should (equal (file-name-concat save-dir "local" "plans"
                                             "current.md")
                           (plist-get artifact :absolute-path)))
            (should-error
             (mevedel-plan-write-current "# Plan" session
                                         (current-buffer) "current.md")
             :type 'wrong-number-of-arguments)
            (should (equal 'presented
                           (plist-get (mevedel-session-plan-metadata session)
                                      :status)))
            (should
             (equal (mevedel-plan-hash "# Plan")
                    (plist-get (mevedel-session-plan-metadata session)
                               :hash)))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :presented-plan-hashes))))
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
                   "# Plan" session (current-buffer)))
                 (accepted
                 (mevedel-plan-archive-accepted
                   artifact session "local/plans/cycle-001-plan.md")))
            (should (file-exists-p (plist-get accepted :absolute-path)))
            (should (equal (plist-get artifact :hash)
                           (plist-get accepted :hash)))
            (should (equal "local/plans/cycle-001-plan.md"
                           (plist-get accepted :path)))
            (should
             (equal accepted
                    (mevedel-plan-archive-accepted
                     artifact session
                     "local/plans/cycle-001-plan.md")))
            (let ((different
                   (mevedel-plan-write-current
                    "# Different" session (current-buffer))))
              (should-error
               (mevedel-plan-archive-accepted
                different session "local/plans/cycle-001-plan.md")
               :type 'error))
            (dolist (invalid '("../outside.md" "/tmp/outside.md"
                               "goals/g1/cycle-001-plan.md"))
              (should-error
               (mevedel-plan-archive-accepted artifact session invalid)
               :type 'error))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-current-body
  (:doc "reads the canonical current artifact despite stale metadata")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-body-" t))
         (path (file-name-concat save-dir "local" "plans" "current.md"))
         (stale-path (file-name-concat save-dir "plans" "current.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Plan" nil path nil 'silent)
          (make-directory (file-name-directory stale-path) t)
          (write-region "# Stale" nil stale-path nil 'silent)
          (let ((session
                 (mevedel-session--create
                  :name "test" :save-path save-dir
                  :plan-metadata '(:path "plans/current.md"))))
            (should (equal "# Plan" (mevedel-plan-current-body session)))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-current-exists-p
  (:doc "reports whether the recorded current artifact exists")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-exists-" t))
         (path (file-name-concat save-dir "local" "plans" "current.md"))
         (session
          (mevedel-session--create
           :name "test" :save-path save-dir
           :plan-metadata '(:path "local/plans/current.md"))))
    (unwind-protect
        (progn
          (should-not (mevedel-plan-current-exists-p session))
          (make-directory (file-name-directory path) t)
          (write-region "# Plan" nil path nil 'silent)
          (should (mevedel-plan-current-exists-p session)))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mark-accepted
  (:doc "records current and accepted artifact descriptors")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "test" :turn-count 3)))
    (mevedel-plan-mark-accepted
     session '(:path "current.md" :absolute-path "/tmp/current.md")
     '(:path "accepted.md" :absolute-path "/tmp/accepted.md" :hash "h"))
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should (eq 'accepted (plist-get metadata :status)))
      (should (= 3 (plist-get metadata :accepted-turn)))
      (should (equal "accepted.md" (plist-get metadata :accepted-path)))
      (should (equal "h" (plist-get metadata :accepted-hash))))))

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
                            "# Plan\n\nDo it." session (current-buffer)))
                   (current (plist-get result :current))
                   (accepted (plist-get result :accepted))
                   (metadata (mevedel-session-plan-metadata session)))
              (should (eq 'accepted (plist-get metadata :status)))
              (should (file-exists-p
                       (plist-get current :absolute-path)))
              (should (file-exists-p
                       (plist-get accepted :absolute-path)))
              (should (equal (mevedel-plan-hash "# Plan\n\nDo it.")
                             (plist-get current :hash)))
              (should (equal "local/plans/current.md"
                             (plist-get current :path)))
              (should (string-match-p "\\`local/plans/accepted-[0-9]+-[0-9]+\\.md\\'"
                                      (plist-get accepted :path)))
              (should (equal (file-name-concat save-dir "local" "plans"
                                                "current.md")
                             (plist-get current :absolute-path)))
              (should (equal (file-name-concat save-dir
                                                (plist-get accepted :path))
                             (plist-get accepted :absolute-path)))
              (should (string-prefix-p "local/plans/accepted-"
                                       (plist-get accepted :path)))
              (should-error
               (mevedel-plan-accept
                "# Plan\n\nDo it." session (current-buffer) t
                "goals/g1/current.md" "local/plans/accepted.md")
               :type 'wrong-number-of-arguments))))
      (delete-directory save-dir t))))

(provide 'test-mevedel-plan)
;;; test-mevedel-plan.el ends here
