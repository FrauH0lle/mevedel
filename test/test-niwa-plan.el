;;; test-niwa-plan.el --- Tests for niwa-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-plan)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "niwa-session-test-support"))

(defun niwa-plan-test--session (directory &rest slots)
  "Initialize a real leased plan writer at DIRECTORY with caller SLOTS.
The current temporary buffer owns its lease cleanup."
  (let ((session (apply #'test-niwa-session-persistence--leased-session directory slots)))
    (org-mode)
    (setq-local niwa--session session
                niwa--workspace (niwa-session-workspace session))
    (niwa-session-set-root-buffer session (current-buffer))
    (add-hook 'kill-buffer-hook
              (lambda ()
                (niwa-session-durability-lease-release directory session)
                (niwa-workspace-clear-registry)) nil t)
    (niwa-session-artifacts-assert-mutation-authority session (current-buffer))
    (niwa-session-publication-publish
     session (list (list :path "session.meta.el" :commit-marker t
                         :content (niwa-session-artifacts-printed-value
                                   (niwa-session-artifacts--live-sidecar session (current-buffer))))) t)
    session))

(niwa-deftest niwa-plan-validate
  (:doc "normalizes nonblank plans and rejects invalid input")
  ,test
  (test)
  (should (equal "# Plan" (niwa-plan-validate "# Plan")))
  (should-error (niwa-plan-validate "  \n"))
  (should-error (niwa-plan-validate nil)))

(niwa-deftest niwa-plan-extract-proposed
  (:doc "extracts the last complete proposed-plan block")
  ,test
  (test)
  (should (equal
           "second"
           (niwa-plan-extract-proposed
            "<proposed_plan>\nfirst\n</proposed_plan>\n<proposed_plan>\nsecond\n</proposed_plan>")))
  :doc "accepts glued opening tags and indented delimiters"
  (dolist (text '("No changes.<proposed_plan>\n# Plan\n</proposed_plan>"
                  "  <proposed_plan>\n# Plan\n  </proposed_plan>\n"
                  "Intro <proposed_plan> \t\n# Plan\n\t</proposed_plan> \t"))
    (should (equal "# Plan" (niwa-plan-extract-proposed text))))
  :doc "rejects incomplete blocks, inline examples, and delimiter discussion"
  (dolist (text '("<proposed_plan>\n# Incomplete"
                  "Use `<proposed_plan>` and `</proposed_plan>` tags."
                  "The opening delimiter is <proposed_plan>\n"
                  "<proposed_plan>inline example</proposed_plan>"
                  "<proposed_plan>\n# Plan\ntext </proposed_plan>"))
    (should-not (niwa-plan-extract-proposed text)))
  :doc "accepts a concise authoritative reference without template headings"
  (should
   (equal
    "Implement ticket 2 in .scratch/feature/tickets.md."
    (niwa-plan-extract-proposed
     "<proposed_plan>\nImplement ticket 2 in .scratch/feature/tickets.md.\n</proposed_plan>"))))

(niwa-deftest niwa-plan-strip-proposed
  (:doc "removes complete and streaming proposed-plan blocks")
  ,test
  (test)
  (should (equal
           "Intro\nTail"
           (niwa-plan-strip-proposed
            "Intro\n<proposed_plan>\n# Plan\n</proposed_plan>\nTail")))
  (should (equal
           "Intro"
           (niwa-plan-strip-proposed
            "Intro\n<proposed_plan>\n# Streaming plan\n")))
  :doc "hides glued and indented proposals during streaming and after completion"
  (dolist (text '("Intro<proposed_plan>\n# Plan\n</proposed_plan>\n"
                  "Intro\n  <proposed_plan>\n# Plan\n  </proposed_plan>"
                  "Intro<proposed_plan>\n# Streaming plan\n"))
    (should (equal "Intro" (niwa-plan-strip-proposed text))))
  :doc "preserves inline delimiter discussion"
  (let ((text "Use `<proposed_plan>` and `</proposed_plan>` tags."))
    (should (equal text (niwa-plan-strip-proposed text)))))

(niwa-deftest niwa-plan--metadata-put
  (:doc "updates one plan metadata key without dropping others")
  ,test
  (test)
  (let ((session
         (niwa-session--create :name "test" :plan-metadata '(:old t))))
    (niwa-plan--metadata-put session :new 1)
    (should (equal '(:old t :new 1)
                   (niwa-session-plan-metadata session)))))

(niwa-deftest niwa-plan-hash
  (:doc "ignores trailing whitespace in stable plan hashes")
  ,test
  (test)
  (should (equal (niwa-plan-hash "# Plan")
                 (niwa-plan-hash "# Plan\n"))))

(niwa-deftest niwa-plan-resource-address
  (:doc "converts contained plan paths to canonical local resource addresses")
  ,test
  (test)
  (should (equal "work://plans/current.md"
                 (niwa-plan-resource-address "local/plans/current.md")))
  (should (equal "work://plans/accepted-20260813-120000.md"
                 (niwa-plan-resource-address
                  "local/plans/accepted-20260813-120000.md")))
  (dolist (path '("../plans/current.md"
                  "local/plans/../current.md"
                  "plans/current.md"
                  "/tmp/current.md"
                  "local/plans/accepted.md"))
    (should-error (niwa-plan-resource-address path))))

(niwa-deftest niwa-plan-current-path
  (:doc "returns the current artifact path below the session directory")
  ,test
  (test)
  (let ((save-dir (make-temp-file "niwa-plan-path-" t)))
    (unwind-protect
        (let ((session
               (niwa-session--create
                :name "test" :save-path save-dir)))
          (should (equal (file-name-concat save-dir "local" "plans" "current.md")
                         (niwa-plan-current-path session)))
          (should (equal (file-name-concat save-dir "goals" "g1" "current.md")
                         (niwa-plan-current-path
                          session nil "goals/g1/current.md")))
          (should-error
           (niwa-plan-current-path session nil "../escaped.md")))
      (delete-directory save-dir t))))

(niwa-deftest niwa-plan-artifact-path-p
  (:doc "accepts only normalized session-relative artifact paths")
  ,test
  (test)
  (should (niwa-plan-artifact-path-p "plans/accepted.md"))
  (dolist (path '(nil "" "/tmp/accepted.md" "../accepted.md"
                      "plans/../accepted.md" "plans//accepted.md"
                      "plans/accepted.md/" "~/accepted.md" ".lease"
                      ".publications/manifest.el"
                      "/ssh:other:/accepted.md"))
    (should-not (niwa-plan-artifact-path-p path))))

(niwa-deftest niwa-plan-artifact-path
  (:doc "derives a qualified logical path without accepting another target")
  ,test
  (test)
  (let ((session
         (niwa-session--create
          :name "test"
          :save-path "/ssh:box:/srv/project/.niwa/sessions/main/")))
    (should
     (equal "/ssh:box:/srv/project/.niwa/sessions/main/plans/accepted.md"
            (niwa-plan-artifact-path
             session '(:path "plans/accepted.md" :hash "h"))))
    (should-error
     (niwa-plan-artifact-path
      session '(:path "/ssh:other:/tmp/accepted.md" :hash "h")))
    (should-error
     (niwa-plan-artifact-path
      session '(:path "../accepted.md" :hash "h")))))

(niwa-deftest niwa-plan-read-artifact
  (:doc "normalizes verified resolver bytes and enforces the recorded hash")
  ,test
  (test)
  (let* ((session
          (niwa-session--create :name "test" :save-path "/tmp/session/"))
         (body "# Accepted")
         (artifact
          (list :path "plans/accepted.md" :hash (niwa-plan-hash body))))
    (cl-letf (((symbol-function 'niwa-session-artifacts-read-artifact)
               (lambda (seen-session logical &optional _committed-only)
                 (should (eq session seen-session))
                 (should (equal "plans/accepted.md" logical))
                 (encode-coding-string body 'utf-8-unix))))
      (should (equal body (niwa-plan-read-artifact session artifact)))
      (should-error
       (niwa-plan-read-artifact
        session '(:path "plans/accepted.md" :hash "wrong"))))))

(niwa-deftest niwa-plan-write-current
  (:doc "writes a validated relative artifact without durable absolute paths")
  ,test
  (test)
  (let ((save-dir (make-temp-file "niwa-plan-write-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (niwa-plan-test--session save-dir :name "test"))
                 (artifact
                  (niwa-plan-write-current "# Plan" session
                                              (current-buffer)
                                              "goals/g1/current.md")))
            (niwa-session-artifacts-publish-sidecar-state session (current-buffer))
            (should (niwa-session-artifacts-artifact-present-p
                     session "goals/g1/current.md" t))
            (should (equal "# Plan" (niwa-plan-read-artifact session artifact)))
            (should (equal "goals/g1/current.md"
                           (plist-get artifact :path)))
            (should-not (plist-member artifact :absolute-path))
            (should (equal 'presented
                           (plist-get (niwa-session-plan-metadata session)
                                      :status)))
            (should
             (equal (niwa-plan-hash "# Plan")
                    (plist-get (niwa-session-plan-metadata session)
                               :hash)))
            (should-not
             (plist-member (niwa-session-plan-metadata session)
                           :absolute-path))
            (should-not
             (plist-member (niwa-session-plan-metadata session)
                           :accepted-absolute-path))
            (should-not
             (plist-member (niwa-session-plan-metadata session)
                           :presented-plan-hashes))))
      (delete-directory save-dir t))))

(niwa-deftest niwa-plan-archive-accepted
  (:doc "archives only canonical addressable immutable artifacts")
  ,test
  (test)
  (let ((save-dir (make-temp-file "niwa-plan-archive-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (niwa-plan-test--session save-dir :name "test"))
                 (artifact
                  (niwa-plan-write-current
                   "# Plan" session (current-buffer)))
                 (accepted (niwa-plan-archive-accepted artifact session)))
            (should-not (plist-member accepted :absolute-path))
            (should (equal (plist-get artifact :hash)
                           (plist-get accepted :hash)))
            (should (string-match-p
                     "\\`local/plans/accepted-[0-9]+-[0-9]+\\.md\\'"
                     (plist-get accepted :path)))
            (should (file-exists-p
                     (file-name-concat save-dir (plist-get accepted :path))))
            (should (equal "# Plan" (niwa-plan-read-artifact session accepted)))
            ;; Every archive stays serializable as a canonical address.
            (should (string-prefix-p
                     "work://plans/accepted-"
                     (niwa-plan-resource-address
                      (plist-get accepted :path))))
            ;; A second archive never overwrites the immutable first one.
            (let ((second (niwa-plan-archive-accepted artifact session)))
              (should-not (equal (plist-get second :path)
                                 (plist-get accepted :path)))
              (should (file-exists-p
                       (file-name-concat save-dir
                                         (plist-get accepted :path))))
              (should (equal "# Plan" (niwa-plan-read-artifact session accepted))))
            ;; A caller-chosen destination is deterministic and immutable.
            (let ((named (niwa-plan-archive-accepted
                          artifact session "goals/g1/cycle-001-plan.md")))
              (niwa-session-artifacts-publish-sidecar-state session (current-buffer))
              (should (equal "goals/g1/cycle-001-plan.md"
                             (plist-get named :path)))
              (should (niwa-session-artifacts-artifact-present-p
                       session "goals/g1/cycle-001-plan.md" t))
              (should (equal "# Plan" (niwa-plan-read-artifact session named))))
            (should-error
             (niwa-plan-archive-accepted '(:path "local/plans/current.md")
                                            session)
             :type 'error)))
      (delete-directory save-dir t))))

(niwa-deftest niwa-plan-current-body
  (:doc "reads verified artifact bytes instead of a poisoned fixed cache")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "niwa-plan-body-" t))
         (path (file-name-concat save-dir "local" "plans" "current.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Poisoned cache" nil path nil 'silent)
          (let ((session
                 (niwa-session--create
                  :name "test" :save-path save-dir
                  :plan-metadata '(:path "current.md"))))
            (cl-letf (((symbol-function
                        'niwa-session-artifacts-artifact-present-p)
                       (lambda (_session logical)
                         (should (equal "current.md" logical))
                         t))
                      ((symbol-function
                        'niwa-session-artifacts-read-artifact)
                       (lambda (seen-session logical &optional _committed-only)
                         (should (eq session seen-session))
                         (should (equal "current.md" logical))
                         (encode-coding-string "# Published" 'utf-8-unix))))
              (should (equal "# Published"
                             (niwa-plan-current-body session))))))
      (delete-directory save-dir t))))

(niwa-deftest niwa-plan-mark-accepted
  (:doc "records current and accepted artifact descriptors")
  ,test
  (test)
  (let ((session (niwa-session--create :name "test" :turn-count 3)))
    (niwa-plan-mark-accepted
     session '(:path "current.md" :hash "current-h")
     '(:path "accepted.md" :hash "h"))
    (let ((metadata (niwa-session-plan-metadata session)))
      (should (eq 'accepted (plist-get metadata :status)))
      (should (= 3 (plist-get metadata :accepted-turn)))
      (should (equal "accepted.md" (plist-get metadata :accepted-path)))
      (should (equal "h" (plist-get metadata :accepted-hash)))
      (should-not (plist-member metadata :absolute-path))
      (should-not (plist-member metadata :accepted-absolute-path)))))

(niwa-deftest niwa-plan-accept ()
  ,test
  (test)
  :doc "keeps the proposal resumable when the archive fails"
  ;; Acceptance publishes twice.  The metadata reset used to happen with the
  ;; first publication, so a second one that failed left the proposal
  ;; demoted to `presented' with no proposal id -- a state resume cannot
  ;; restore, even though acceptance never committed.
  (let ((save-dir (make-temp-file "niwa-plan-archive-fail-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session (niwa-plan-test--session save-dir
                           :name "test"
                           :permission-mode 'full-auto
                           :turn-count 4))
                 (markdown "# Plan\n\nDo it.")
                 (publishes 0)
                 (real (symbol-function
                        'niwa-session-artifacts-publish-text)))
            (setf (niwa-session-plan-metadata session)
                  (list :path "plan/current.md"
                        :hash (niwa-plan-hash markdown)
                        :status 'proposed
                        :proposal-id (list "req" 4 (niwa-plan-hash markdown))
                        :selection '(:kind whole)))
            (cl-letf (((symbol-function 'niwa-session-artifacts-publish-text)
                       (lambda (&rest args)
                         (if (= (cl-incf publishes) 1)
                             (apply real args)
                           (error "Lease lost")))))
              (should-error
               (niwa-plan-accept markdown session (current-buffer))))
            (let ((metadata (niwa-session-plan-metadata session)))
              (should (eq 'proposed (plist-get metadata :status)))
              (should (equal (list "req" 4 (niwa-plan-hash markdown))
                             (plist-get metadata :proposal-id)))
              (should (plist-member metadata :selection)))))
      (delete-directory save-dir t)))

  :doc "accepts a plan without depending on Goal controller state"
  (let ((save-dir (make-temp-file "niwa-plan-accept-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (niwa-plan-test--session save-dir
                          :name "test"
                          :permission-mode 'full-auto
                          :turn-count 4)))
            (let* ((result (niwa-plan-accept
                            "# Plan\n\nDo it." session (current-buffer)))
                   (current (plist-get result :current))
                   (accepted (plist-get result :accepted))
                   (metadata (niwa-session-plan-metadata session)))
              (should (eq 'accepted (plist-get metadata :status)))
              (should-not (plist-member current :absolute-path))
              (should-not (plist-member accepted :absolute-path))
              (should-not (plist-member metadata :absolute-path))
              (should-not (plist-member metadata :accepted-absolute-path))
              (should (equal (niwa-plan-hash "# Plan\n\nDo it.")
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
                                         (plist-get accepted :path))))
              (should (equal "# Plan\n\nDo it." (niwa-plan-read-artifact session current)))
              (should (equal "# Plan\n\nDo it." (niwa-plan-read-artifact session accepted))))
            ;; Explicit destinations let a Goal cycle own its own artifacts.
            (let* ((result (niwa-plan-accept
                            "# Plan\n\nDo it." session (current-buffer) t
                            "goals/g1/current.md"
                            "goals/g1/cycle-001-plan.md"))
                   (current (plist-get result :current))
                   (accepted (plist-get result :accepted)))
              (niwa-session-artifacts-publish-sidecar-state session (current-buffer))
              (should (equal "goals/g1/current.md" (plist-get current :path)))
              (should (equal "goals/g1/cycle-001-plan.md"
                             (plist-get accepted :path)))
              (should (niwa-session-artifacts-artifact-present-p
                       session "goals/g1/current.md" t))
              (should (niwa-session-artifacts-artifact-present-p
                       session "goals/g1/cycle-001-plan.md" t))
              (should (equal "# Plan\n\nDo it." (niwa-plan-read-artifact session current)))
              (should (equal "# Plan\n\nDo it." (niwa-plan-read-artifact session accepted))))))
      (delete-directory save-dir t))))

(provide 'test-niwa-plan)
;;; test-niwa-plan.el ends here
