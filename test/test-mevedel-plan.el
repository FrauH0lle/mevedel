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
         (mevedel-session--create :authority-mode 'pid-lock :name "test" :plan-metadata '(:old t))))
    (mevedel-plan--metadata-put session :new 1)
    (should (equal '(:old t :new 1)
                   (mevedel-session-plan-metadata session)))))

(mevedel-deftest mevedel-plan-hash
  (:doc "ignores trailing whitespace in stable plan hashes")
  ,test
  (test)
  (should (equal (mevedel-plan-hash "# Plan")
                 (mevedel-plan-hash "# Plan\n"))))

(mevedel-deftest mevedel-plan-resource-address
  (:doc "converts contained plan paths to canonical local resource addresses")
  ,test
  (test)
  (should (equal "local://plans/current.md"
                 (mevedel-plan-resource-address "local/plans/current.md")))
  (should (equal "local://plans/accepted-20260813-120000.md"
                 (mevedel-plan-resource-address
                  "local/plans/accepted-20260813-120000.md")))
  (dolist (path '("../plans/current.md"
                  "local/plans/../current.md"
                  "plans/current.md"
                  "/tmp/current.md"
                  "local/plans/accepted.md"))
    (should-error (mevedel-plan-resource-address path))))

(mevedel-deftest mevedel-plan-current-path
  (:doc "returns the current artifact path below the session directory")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-path-" t)))
    (unwind-protect
        (let ((session
               (mevedel-session--create
                :authority-mode 'pid-lock :name "test" :save-path save-dir)))
          (should (equal (file-name-concat save-dir "local" "plans" "current.md")
                         (mevedel-plan-current-path session)))
          (should (equal (file-name-concat save-dir "goals" "g1" "current.md")
                         (mevedel-plan-current-path
                          session nil "goals/g1/current.md")))
          (should-error
           (mevedel-plan-current-path session nil "../escaped.md")))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-artifact-path-p
  (:doc "accepts only normalized session-relative artifact paths")
  ,test
  (test)
  (should (mevedel-plan-artifact-path-p "plans/accepted.md"))
  (dolist (path '(nil "" "/tmp/accepted.md" "../accepted.md"
                      "plans/../accepted.md" "plans//accepted.md"
                      "plans/accepted.md/" "~/accepted.md" ".lease"
                      ".publications/manifest.el"
                      "/ssh:other:/accepted.md"))
    (should-not (mevedel-plan-artifact-path-p path))))

(mevedel-deftest mevedel-plan-artifact-path
  (:doc "derives a qualified logical path without accepting another target")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "test"
          :save-path "/ssh:box:/srv/project/.mevedel/sessions/main/")))
    (should
     (equal "/ssh:box:/srv/project/.mevedel/sessions/main/plans/accepted.md"
            (mevedel-plan-artifact-path
             session '(:path "plans/accepted.md" :hash "h"))))
    (should-error
     (mevedel-plan-artifact-path
      session '(:path "/ssh:other:/tmp/accepted.md" :hash "h")))
    (should-error
     (mevedel-plan-artifact-path
      session '(:path "../accepted.md" :hash "h")))))

(mevedel-deftest mevedel-plan-read-artifact
  (:doc "normalizes verified resolver bytes and enforces the recorded hash")
  ,test
  (test)
  (let* ((session
          (mevedel-session--create :authority-mode 'pid-lock :name "test" :save-path "/tmp/session/"))
         (body "# Accepted")
         (artifact
          (list :path "plans/accepted.md" :hash (mevedel-plan-hash body))))
    (cl-letf (((symbol-function 'mevedel-session-artifacts-read-artifact)
               (lambda (seen-session logical &optional _committed-only)
                 (should (eq session seen-session))
                 (should (equal "plans/accepted.md" logical))
                 (encode-coding-string body 'utf-8-unix))))
      (should (equal body (mevedel-plan-read-artifact session artifact)))
      (should-error
       (mevedel-plan-read-artifact
        session '(:path "plans/accepted.md" :hash "wrong"))))))

(mevedel-deftest mevedel-plan-write-current
  (:doc "writes a validated relative artifact without durable absolute paths")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-write-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (mevedel-session--create :authority-mode 'pid-lock :name "test" :save-path save-dir))
                 (artifact
                  (mevedel-plan-write-current "# Plan" session
                                              (current-buffer)
                                              "goals/g1/current.md")))
            (should (file-exists-p
                     (file-name-concat save-dir "goals" "g1" "current.md")))
            (should (equal "goals/g1/current.md"
                           (plist-get artifact :path)))
            (should-not (plist-member artifact :absolute-path))
            (should (equal 'presented
                           (plist-get (mevedel-session-plan-metadata session)
                                      :status)))
            (should
             (equal (mevedel-plan-hash "# Plan")
                    (plist-get (mevedel-session-plan-metadata session)
                               :hash)))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :absolute-path))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :accepted-absolute-path))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :presented-plan-hashes))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-archive-accepted
  (:doc "archives only canonical addressable immutable artifacts")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-archive-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (mevedel-session--create
                   :authority-mode 'pid-lock :name "test"
                   :save-path save-dir))
                 (artifact
                  (mevedel-plan-write-current
                   "# Plan" session (current-buffer)))
                 (accepted (mevedel-plan-archive-accepted artifact session)))
            (should-not (plist-member accepted :absolute-path))
            (should (equal (plist-get artifact :hash)
                           (plist-get accepted :hash)))
            (should (string-match-p
                     "\\`local/plans/accepted-[0-9]+-[0-9]+\\.md\\'"
                     (plist-get accepted :path)))
            (should (file-exists-p
                     (file-name-concat save-dir (plist-get accepted :path))))
            ;; Every archive stays serializable as a canonical address.
            (should (string-prefix-p
                     "local://plans/accepted-"
                     (mevedel-plan-resource-address
                      (plist-get accepted :path))))
            ;; A second archive never overwrites the immutable first one.
            (let ((second (mevedel-plan-archive-accepted artifact session)))
              (should-not (equal (plist-get second :path)
                                 (plist-get accepted :path)))
              (should (file-exists-p
                       (file-name-concat save-dir
                                         (plist-get accepted :path)))))
            ;; A caller-chosen destination is deterministic and immutable.
            (let ((named (mevedel-plan-archive-accepted
                          artifact session "goals/g1/cycle-001-plan.md")))
              (should (equal "goals/g1/cycle-001-plan.md"
                             (plist-get named :path)))
              (should (file-exists-p
                       (file-name-concat
                        save-dir "goals" "g1" "cycle-001-plan.md"))))
            (should-error
             (mevedel-plan-archive-accepted '(:path "local/plans/current.md")
                                            session)
             :type 'error)))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-current-body
  (:doc "reads verified artifact bytes instead of a poisoned fixed cache")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-body-" t))
         (path (file-name-concat save-dir "local" "plans" "current.md"))
         (stale-path (file-name-concat save-dir "plans" "current.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Poisoned cache" nil path nil 'silent)
          (let ((session
                 (mevedel-session--create
                  :authority-mode 'pid-lock
                  :name "test" :save-path save-dir
                  :plan-metadata '(:path "current.md"))))
            (cl-letf (((symbol-function
                        'mevedel-session-artifacts-artifact-present-p)
                       (lambda (_session logical)
                         (should (equal "current.md" logical))
                         t))
                      ((symbol-function
                        'mevedel-session-artifacts-read-artifact)
                       (lambda (seen-session logical &optional _committed-only)
                         (should (eq session seen-session))
                         (should (equal "current.md" logical))
                         (encode-coding-string "# Published" 'utf-8-unix))))
              (should (equal "# Published"
                             (mevedel-plan-current-body session))))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mark-accepted
  (:doc "records current and accepted artifact descriptors")
  ,test
  (test)
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test" :turn-count 3)))
    (mevedel-plan-mark-accepted
     session '(:path "current.md" :hash "current-h")
     '(:path "accepted.md" :hash "h"))
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should (eq 'accepted (plist-get metadata :status)))
      (should (= 3 (plist-get metadata :accepted-turn)))
      (should (equal "accepted.md" (plist-get metadata :accepted-path)))
      (should (equal "h" (plist-get metadata :accepted-hash)))
      (should-not (plist-member metadata :absolute-path))
      (should-not (plist-member metadata :accepted-absolute-path)))))

(mevedel-deftest mevedel-plan-accept ()
  ,test
  (test)
  :doc "keeps the proposal resumable when the archive fails"
  ;; Acceptance publishes twice.  The metadata reset used to happen with the
  ;; first publication, so a second one that failed left the proposal
  ;; demoted to `presented' with no proposal id -- a state resume cannot
  ;; restore, even though acceptance never committed.
  (let ((save-dir (make-temp-file "mevedel-plan-archive-fail-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session (mevedel-session--create
                           :authority-mode 'pid-lock
                           :name "test"
                           :save-path save-dir
                           :permission-mode 'full-auto
                           :turn-count 4))
                 (markdown "# Plan\n\nDo it.")
                 (publishes 0)
                 (real (symbol-function
                        'mevedel-session-artifacts-publish-text)))
            (setf (mevedel-session-plan-metadata session)
                  (list :path "plan/current.md"
                        :hash (mevedel-plan-hash markdown)
                        :status 'proposed
                        :proposal-id (list "req" 4 (mevedel-plan-hash markdown))
                        :selection '(:kind whole)))
            (cl-letf (((symbol-function 'mevedel-session-artifacts-publish-text)
                       (lambda (&rest args)
                         (if (= (cl-incf publishes) 1)
                             (apply real args)
                           (error "Lease lost")))))
              (should-error
               (mevedel-plan-accept markdown session (current-buffer))))
            (let ((metadata (mevedel-session-plan-metadata session)))
              (should (eq 'proposed (plist-get metadata :status)))
              (should (equal (list "req" 4 (mevedel-plan-hash markdown))
                             (plist-get metadata :proposal-id)))
              (should (plist-member metadata :selection)))))
      (delete-directory save-dir t)))

  :doc "accepts a plan without depending on Goal controller state"
  (let ((save-dir (make-temp-file "mevedel-plan-accept-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :authority-mode 'pid-lock
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
              (should-not (plist-member current :absolute-path))
              (should-not (plist-member accepted :absolute-path))
              (should-not (plist-member metadata :absolute-path))
              (should-not (plist-member metadata :accepted-absolute-path))
              (should (equal (mevedel-plan-hash "# Plan\n\nDo it.")
                             (plist-get current :hash)))
              (should (equal "local/plans/current.md"
                             (plist-get current :path)))
              (should (string-match-p
                       "\\`local/plans/accepted-[0-9]+-[0-9]+\\.md\\'"
                       (plist-get accepted :path)))
              (should (file-exists-p
                       (file-name-concat save-dir
                                         (plist-get current :path))))
              (should (file-exists-p
                       (file-name-concat save-dir
                                         (plist-get accepted :path)))))
            ;; Explicit destinations let a Goal cycle own its own artifacts.
            (let* ((result (mevedel-plan-accept
                            "# Plan\n\nDo it." session (current-buffer) t
                            "goals/g1/current.md"
                            "goals/g1/cycle-001-plan.md"))
                   (current (plist-get result :current))
                   (accepted (plist-get result :accepted)))
              (should (equal "goals/g1/current.md" (plist-get current :path)))
              (should (equal "goals/g1/cycle-001-plan.md"
                             (plist-get accepted :path)))
              (should (file-exists-p
                       (file-name-concat save-dir "goals" "g1" "current.md")))
              (should (file-exists-p
                       (file-name-concat
                        save-dir "goals" "g1" "cycle-001-plan.md"))))))
      (delete-directory save-dir t))))

(provide 'test-mevedel-plan)
;;; test-mevedel-plan.el ends here
