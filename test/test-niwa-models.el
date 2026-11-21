;;; test-niwa-models.el --- Tests for model tier resolution -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'gptel-openai)
(require 'niwa-models)
(require 'niwa-readiness)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel-reasoning-effort)


;;
;;; Provider and tier resolution

(defmacro niwa-models-test--with-backends (&rest body)
  "Run BODY with an isolated gptel backend registry."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     (gptel-make-openai "Ollama" :key "test" :models '(llama3.1:8b))
     ,@body))

(niwa-deftest niwa-model-resolve-provider ()
  ,test
  (test)

  :doc "resolves gptel transient-style BACKEND:MODEL strings"
  (niwa-models-test--with-backends
    (let ((provider (niwa-model-resolve-provider "Fast:fast-model")))
      (should (equal "Fast" (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'fast-model (plist-get provider :model)))))

  :doc "splits at the first colon so provider strings support colon models"
  (niwa-models-test--with-backends
    (let ((provider (niwa-model-resolve-provider "Ollama:llama3.1:8b")))
      (should (equal "Ollama" (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'llama3.1:8b (plist-get provider :model)))))

  :doc "resolves structured (BACKEND . MODEL) specs"
  (niwa-models-test--with-backends
    (let ((provider (niwa-model-resolve-provider '("Balanced" . balanced-model))))
      (should (equal "Balanced"
                     (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'balanced-model (plist-get provider :model))))))

(niwa-deftest niwa-model-candidates
  (:doc "lists every registered model by its exact provider label, sorted")
  (niwa-models-test--with-backends
    (let ((candidates (niwa-model-candidates)))
      (should (equal '("Balanced:balanced-model" "Fast:fast-model"
                       "Ollama:llama3.1:8b")
                     (mapcar #'car candidates)))
      ;; Each label resolves back to the provider it stands for.
      (dolist (candidate candidates)
        (should (equal (cdr candidate)
                       (niwa-model-resolve-provider (car candidate))))))))

(niwa-deftest niwa-model-resolve-tier ()
  ,test
  (test)

  :doc "resolves configured tiers to provider plists"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers
           '((fast :provider "Fast:fast-model" :effort nil)
             (balanced :provider "Balanced:balanced-model")
             (strong))))
      (let ((provider (niwa-model-resolve-tier 'fast)))
        (should (equal "Fast" (gptel-backend-name
                               (plist-get provider :backend))))
        (should (eq 'fast-model (plist-get provider :model))))))

  :doc "unconfigured named tiers inherit current session policy"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers '((custom)))
          (gptel-backend (gptel-get-backend "Balanced"))
          (gptel-model 'balanced-model))
      (let ((policy (niwa-model-resolve-tier 'custom)))
        (should (eq gptel-backend (plist-get policy :backend)))
        (should (eq gptel-model (plist-get policy :model)))))))

(niwa-deftest niwa-model-tier-guide ()
  ,test
  (test)

  :doc "names each tier's provider and effort with a hint for the basic tiers"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers
           '((fast :provider "Fast:fast-model" :effort nil)
             (balanced :provider "Balanced:balanced-model" :effort high)
             (strong :provider "Fast:fast-model" :effort high)))
          (gptel-backend (gptel-get-backend "Ollama"))
          (gptel-model 'llama3.1:8b)
          (gptel-reasoning-effort 'medium))
      (let ((guide (niwa-model-tier-guide)))
        (should (string-search
                 "fast = Fast:fast-model (default effort), for search" guide))
        (should (string-search
                 "balanced = Balanced:balanced-model (high), for routine" guide))
        (should (string-search
                 "strong = Fast:fast-model (high), for hard design" guide))
        ;; Omitting the argument keeps the session model for roleless agents.
        (should (string-search "Ollama:llama3.1:8b (medium)" guide)))))

  :doc "collapses identical tiers into one sentence"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers '((fast) (balanced) (strong)))
          (gptel-backend (gptel-get-backend "Ollama"))
          (gptel-model 'llama3.1:8b)
          (gptel-reasoning-effort 'medium))
      (let ((guide (niwa-model-tier-guide)))
        (should (string-search "All tiers use the session model" guide))
        (should (string-search "Ollama:llama3.1:8b" guide))
        (should-not (string-search "fast =" guide)))
      (should (equal "all tiers use the session model"
                     (niwa-model-tier-guide t)))))

  :doc "reports a tier with no provider and no fallback as unavailable"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers
           '((fast :provider "Missing:nothing")
             (balanced :provider "Balanced:balanced-model")
             (strong)))
          (niwa-model-fallback-provider nil)
          (gptel-backend nil)
          (gptel-model nil)
          (gptel-reasoning-effort 'medium))
      (should (string-search "fast = unavailable"
                             (niwa-model-tier-guide)))
      (should (equal "fast=unavailable, balanced=balanced-model, strong=unavailable"
                     (niwa-model-tier-guide t)))))

  :doc "shows the fallback model for a tier whose provider is unavailable"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers
           '((fast :provider "Missing:nothing")
             (balanced :provider "Balanced:balanced-model")
             (strong)))
          (niwa-model-fallback-provider "Fast:fast-model")
          (gptel-backend (gptel-get-backend "Ollama"))
          (gptel-model 'llama3.1:8b)
          (gptel-reasoning-effort 'medium))
      (should (string-search "fast = Fast:fast-model (medium)"
                             (niwa-model-tier-guide)))))

  :doc "lists additional tiers without a usage hint"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers
           '((fast :provider "Fast:fast-model")
             (custom :provider "Balanced:balanced-model" :effort low)))
          (gptel-backend (gptel-get-backend "Ollama"))
          (gptel-model 'llama3.1:8b)
          (gptel-reasoning-effort 'medium))
      (should (string-search "; custom = Balanced:balanced-model (low)."
                             (niwa-model-tier-guide)))))

  :doc "compact form shows model names only"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers
           '((fast :provider "Fast:fast-model")
             (balanced :provider "Balanced:balanced-model")
             (strong :provider "Fast:fast-model" :effort high)))
          (gptel-backend (gptel-get-backend "Ollama"))
          (gptel-model 'llama3.1:8b)
          (gptel-reasoning-effort 'medium))
      (should (equal "fast=fast-model, balanced=balanced-model, strong=fast-model"
                     (niwa-model-tier-guide t))))))

(niwa-deftest niwa-model-supported-efforts
  ()
  ,test
  (test)

  :doc "returns only symbolic values from member metadata"
  (let ((old-effort (get 'test-model :reasoning-effort)))
    (unwind-protect
        (progn
          (put 'test-model :reasoning-effort '(member low "bad" high))
          (should (equal '(low high)
                         (niwa-model-supported-efforts 'test-model))))
      (put 'test-model :reasoning-effort old-effort)))

  :doc "returns nil for unsupported metadata shapes"
  (let ((old-effort (get 'test-model :reasoning-effort)))
    (unwind-protect
        (progn
          (put 'test-model :reasoning-effort '(choice low high))
          (should-not (niwa-model-supported-efforts 'test-model)))
      (put 'test-model :reasoning-effort old-effort))))

(niwa-deftest niwa-model-set-session-provider
  (:quiet t)
  ,test
  (test)

  :doc "model selection clears stale root model errors without releasing unrelated recovery holds"
  (niwa-models-test--with-backends
    (with-temp-buffer
      (let ((session (niwa-session--create :pending-input-failure-paused t)))
        (dolist (id '("model" "request" "authentication"))
          (niwa-recovery-report session id 'model "Unavailable model" t))
        (niwa-model-set-session-provider
         session (niwa-model-resolve-provider "Fast:fast-model"))
        (should-not (niwa-recovery-blocker session))
        (niwa-readiness-assert session)
        (should (niwa-session-pending-input-failure-paused session))
        (dolist (issue '(("request" . history) ("input" . input)
                         ("preset" . configuration) ("authentication" . authentication)))
          (niwa-recovery-report session (car issue) (cdr issue) "Needs recovery" t))
        (let ((before (copy-tree (niwa-session-recovery-issues session))))
          (niwa-model-set-session-provider
           session (niwa-model-resolve-provider "Balanced:balanced-model"))
          (should (equal before (niwa-session-recovery-issues session)))
          (should-error (niwa-readiness-assert session) :type 'user-error)))))

  :doc "stores an exact session provider and preserves supported effort"
  (niwa-models-test--with-backends
    (let* ((session (niwa-session--create :name "test"))
           (buffer (generate-new-buffer " *niwa-model-provider*"))
           (old-effort (get 'fast-model :reasoning-effort)))
      (unwind-protect
          (progn
            (put 'fast-model :reasoning-effort '(member low high))
            (with-current-buffer buffer
              (setq-local gptel-reasoning-effort 'high))
            (niwa-model-set-session-provider
             session
             (niwa-model-resolve-provider "Fast:fast-model")
             buffer)
            (should (equal "Fast:fast-model"
                           (niwa-session-model-provider session)))
            (should (eq 'high
                        (niwa-session-reasoning-effort session)))
            (with-current-buffer buffer
              (should (equal "Fast" (gptel-backend-name gptel-backend)))
              (should (eq 'fast-model gptel-model))
              (should (eq 'high gptel-reasoning-effort))))
        (put 'fast-model :reasoning-effort old-effort)
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  :doc "resets unsupported effort when the model changes"
  (niwa-models-test--with-backends
    (let* ((session (niwa-session--create :name "test"))
           (buffer (generate-new-buffer " *niwa-model-reset*"))
           (old-effort (get 'balanced-model :reasoning-effort)))
      (unwind-protect
          (progn
            (put 'balanced-model :reasoning-effort '(member low))
            (with-current-buffer buffer
              (setq-local gptel-reasoning-effort 'high))
            (niwa-model-set-session-provider
             session
             (niwa-model-resolve-provider "Balanced:balanced-model")
             buffer)
            (should-not (niwa-session-reasoning-effort session))
            (with-current-buffer buffer
              (should-not gptel-reasoning-effort)))
        (put 'balanced-model :reasoning-effort old-effort)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(niwa-deftest niwa-model-set-session-effort
  ()
  ,test
  (test)

  :doc "validates and stores the selected session effort"
  (niwa-models-test--with-backends
    (let* ((session (niwa-session--create :name "test"))
           (buffer (generate-new-buffer " *niwa-model-effort*"))
           (old-effort (get 'fast-model :reasoning-effort)))
      (unwind-protect
          (progn
            (put 'fast-model :reasoning-effort '(member low high))
            (with-current-buffer buffer
              (setq-local gptel-backend (gptel-get-backend "Fast"))
              (setq-local gptel-model 'fast-model))
            (niwa-model-set-session-effort session 'high buffer)
            (should (eq 'high
                        (niwa-session-reasoning-effort session)))
            (with-current-buffer buffer
              (should (eq 'high gptel-reasoning-effort)))
            (should-error
             (niwa-model-set-session-effort session 'medium buffer)
             :type 'user-error))
        (put 'fast-model :reasoning-effort old-effort)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(niwa-deftest niwa-model-apply-session-policy
  ()
  ,test
  (test)

  :doc "restores the persisted provider and effort into a live buffer"
  (niwa-models-test--with-backends
    (let* ((session
            (niwa-session--create
             :name "test" :model-provider "Fast:fast-model"
             :reasoning-effort 'high))
           (buffer (generate-new-buffer " *niwa-model-restore*"))
           (old-effort (get 'fast-model :reasoning-effort)))
      (unwind-protect
          (progn
            (put 'fast-model :reasoning-effort '(member low high))
            (niwa-model-apply-session-policy session buffer)
            (with-current-buffer buffer
              (should (equal "Fast" (gptel-backend-name gptel-backend)))
              (should (eq 'fast-model gptel-model))
              (should (eq 'high gptel-reasoning-effort))))
        (put 'fast-model :reasoning-effort old-effort)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(niwa-deftest niwa-model--merge-map
  ()
  ,test
  (test)
  :doc "replaces entries by name without mutating the base map"
  (let* ((base '((fast :provider "A:a") (strong :effort high)))
         (result (niwa-model--merge-map
                  '((fast :provider "B:b") (custom :effort low)) base)))
    (should (equal '(:provider "B:b") (alist-get 'fast result)))
    (should (equal '(:effort low) (alist-get 'custom result)))
    (should (equal '(:provider "A:a") (alist-get 'fast base)))))

(niwa-deftest niwa-model-merge-tiers
  ()
  ,test
  (test)
  :doc "accepts provider and effort tier fields"
  (should (equal '((fast :provider "Fast:fast-model" :effort low))
                 (niwa-model-merge-tiers
                  '((fast :provider "Fast:fast-model" :effort low)) nil)))
  :doc "rejects unknown tier fields"
  (should-error (niwa-model-merge-tiers '((fast :tier other)) nil)
                :type 'user-error))

(niwa-deftest niwa-model-merge-workloads
  ()
  ,test
  (test)
  :doc "merges workload entries by workload name"
  (should (equal '(:provider "Fast:fast-model" :effort high)
                 (alist-get
                  'planning
                  (niwa-model-merge-workloads
                   '((planning :provider "Fast:fast-model" :effort high))
                   '((planning :tier balanced) (review :tier strong))))))
  :doc "rejects simultaneous tier and provider selectors"
  (should-error
   (niwa-model-merge-workloads
    '((planning :tier balanced :provider "Fast:fast-model")) nil)
   :type 'user-error)
  :doc "accepts prefixed and qualified skill workload symbols"
  (let ((merged
         (niwa-model-merge-workloads
          '(($grill-with-docs :tier strong)
            ($plugin:code-review :provider "Fast:fast-model" :effort high))
          nil)))
    (should (equal '(:tier strong)
                   (alist-get '$grill-with-docs merged)))
    (should (equal '(:provider "Fast:fast-model" :effort high)
                   (alist-get '$plugin:code-review merged))))
  :doc "rejects string skill workload keys"
  (should-error
   (niwa-model-merge-workloads
    '(("$grill-with-docs" :tier strong)) nil)
   :type 'user-error))

(niwa-deftest niwa-model-skill-policy-fields
  ()
  ,test
  (test)
  :doc "combines frontmatter and preset field presence in stable order"
  (let ((niwa-model-workloads
         '(($alpha :effort nil)
           ($plugin:beta :tier missing))))
    (should (equal '(model effort)
                   (niwa-model-skill-policy-fields
                    "alpha" "front-tier" nil)))
    (should (equal '(model)
                   (niwa-model-skill-policy-fields
                    "plugin:beta" nil nil)))
    (should (equal '(effort)
                   (niwa-model-skill-policy-fields
                    "plain" nil 'high))))
  :doc "does not validate ignored preset values"
  (let ((niwa-model-workloads
         '(($alpha :tier missing :provider "Missing:model" :effort impossible))))
    (should (equal '(model effort)
                   (niwa-model-skill-policy-fields
                    "alpha" nil nil)))))

(niwa-deftest niwa-model-merge-skill-policy
  ()
  ,test
  (test)
  :doc "preset tier replaces model while frontmatter effort remains"
  (let ((niwa-model-tiers '((fast) (strong)))
        (niwa-model-workloads '(($alpha :tier strong))))
    (should (equal '(:model (:tier strong) :effort low)
                   (niwa-model-merge-skill-policy
                    "alpha" "fast" 'low))))
  :doc "preset effort replaces effort while frontmatter model remains"
  (let ((niwa-model-tiers '((fast)))
        (niwa-model-workloads '(($alpha :effort high))))
    (should (equal '(:model (:tier fast) :effort high)
                   (niwa-model-merge-skill-policy
                    "alpha" "fast" 'low))))
  :doc "qualified preset provider replaces invalid frontmatter model"
  (niwa-models-test--with-backends
    (let ((niwa-model-workloads
           '(($plugin:alpha :provider "Fast:fast-model"))))
      (let ((policy (niwa-model-merge-skill-policy
                     "plugin:alpha" "not-a-selector" 'medium)))
        (should (eq 'fast-model
                    (plist-get (plist-get policy :model) :model)))
        (should (eq 'medium (plist-get policy :effort))))))
  :doc "explicit nil preset fields replace frontmatter with inheritance"
  (let ((niwa-model-tiers '((fast)))
        (niwa-model-workloads '(($alpha :tier nil :effort nil))))
    (should (equal '(:model nil :effort nil)
                   (niwa-model-merge-skill-policy
                    "alpha" "fast" 'high))))
  :doc "missing preset fields retain frontmatter"
  (let ((niwa-model-tiers '((fast)))
        (niwa-model-workloads nil))
    (should (equal '(:model (:tier fast) :effort medium)
                   (niwa-model-merge-skill-policy
                    "alpha" "fast" 'medium))))
  :doc "conflicting preset selectors fail only during owning policy merge"
  (let ((niwa-model-workloads
         '(($alpha :tier strong :provider "Fast:fast-model"))))
    (should-error
     (niwa-model-merge-skill-policy "alpha" nil nil)
     :type 'user-error)))

(niwa-deftest niwa-model-validate-effort
  ()
  ,test
  (test)
  :doc "delegates effort validation to gptel model metadata"
  (let ((old-custom (get 'gptel-reasoning-effort 'custom-type))
        (old-effort (get 'fast-model :reasoning-effort)))
    (unwind-protect
        (progn
          (put 'gptel-reasoning-effort 'custom-type nil)
          (should-error
           (niwa-model-validate-effort 'fast-model 'high)
           :type 'user-error)
          (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
          (put 'fast-model :reasoning-effort '(member low medium high))
          (should (eq 'high
                      (niwa-model-validate-effort 'fast-model 'high)))
          (should-error
           (niwa-model-validate-effort 'fast-model 'max)
           :type 'user-error))
      (put 'gptel-reasoning-effort 'custom-type old-custom)
      (put 'fast-model :reasoning-effort old-effort))))

(niwa-deftest niwa-model-parse-effort
  ()
  ,test
  (test)
  :doc "normalizes model-facing strings without duplicating model validation"
  (progn
    (should-not (niwa-model-parse-effort nil))
    (should (eq 'high (niwa-model-parse-effort "high")))
    (dolist (value '("" "nil" medium 3))
      (should-error (niwa-model-parse-effort value) :type 'user-error))))

(niwa-deftest niwa-model-resolve-workload
  ()
  ,test
  (test)
  :doc "applies session, tier, workload, and explicit override precedence"
  (niwa-models-test--with-backends
    (let ((old-custom (get 'gptel-reasoning-effort 'custom-type))
          (old-fast (get 'fast-model :reasoning-effort))
          (old-llama (get 'llama3.1:8b :reasoning-effort)))
      (unwind-protect
          (let ((gptel-backend (gptel-get-backend "Balanced"))
                (gptel-model 'balanced-model)
                (gptel-reasoning-effort 'low)
                (niwa-model-tiers
                 '((strong :provider "Fast:fast-model" :effort medium)))
                (niwa-model-workloads
                 '((planning :tier strong :effort high))))
            (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
            (put 'fast-model :reasoning-effort '(member low medium high))
            (put 'llama3.1:8b :reasoning-effort '(member low medium high max))
            (let ((policy
                   (niwa-model-resolve-workload
                    'planning
                    (niwa-model-resolve-provider "Ollama:llama3.1:8b")
                    'max)))
              (should (equal "Ollama"
                             (gptel-backend-name
                              (plist-get policy :backend))))
              (should (eq 'llama3.1:8b (plist-get policy :model)))
              (should (eq 'max (plist-get policy :effort)))))
        (put 'gptel-reasoning-effort 'custom-type old-custom)
        (put 'fast-model :reasoning-effort old-fast)
        (put 'llama3.1:8b :reasoning-effort old-llama))))
  :doc "journal and memory default to balanced independently of buddy and summarization"
  (niwa-models-test--with-backends
    (let* ((niwa-model-tiers '((balanced :provider "Balanced:balanced-model")
                                 (fast :provider "Fast:fast-model")))
           (niwa-model-workloads
            (niwa-model-merge-workloads
             '((buddy :tier fast) (summarization :tier fast))
             (eval (car (get 'niwa-model-workloads 'standard-value)) t))))
      (dolist (workload '(journal memory))
        (should (eq 'balanced (plist-get (alist-get workload niwa-model-workloads) :tier)))
        (should (eq 'balanced-model
                    (plist-get (niwa-model-resolve-workload workload) :model))))))
  :doc "plan implementation defaults to session policy instead of a tier"
  (niwa-models-test--with-backends
    (let ((niwa-model-workloads
           (eval (car (get 'niwa-model-workloads 'standard-value)) t))
          (gptel-backend (gptel-get-backend "Balanced"))
          (gptel-model 'balanced-model)
          (gptel-reasoning-effort nil))
      (should (assq 'plan-implementation niwa-model-workloads))
      (should-not (alist-get 'plan-implementation niwa-model-workloads))
      (should (eq 'balanced-model
                  (plist-get (niwa-model-resolve-workload 'plan-implementation)
                             :model)))))
  :doc "Goal review uses strong independently of the ordinary verifier policy"
  (niwa-skills-test--with-model-backends
    (let ((niwa-model-workloads
           (eval (car (get 'niwa-model-workloads 'standard-value)) t))
          (niwa-model-tiers
           '((strong :provider "Balanced:balanced-model" :effort high)
             (balanced :provider "Fast:fast-model" :effort low)))
          (gptel-backend (gptel-get-backend "Fast"))
          (gptel-model 'fast-model)
          (gptel-reasoning-effort 'medium))
      (should (equal (list :backend (gptel-get-backend "Balanced")
                          :model 'balanced-model :effort 'high)
                     (niwa-model-resolve-workload 'goal-review)))
      (should (equal (list :backend (gptel-get-backend "Fast")
                          :model 'fast-model :effort 'low)
                     (niwa-model-resolve-workload 'verifier)))
      (setq niwa-model-tiers '((strong) (balanced)))
      (should (equal (list :backend gptel-backend
                          :model 'fast-model :effort 'medium)
                     (niwa-model-resolve-workload 'goal-review)))))
  :doc "the buddy workload defaults to the fast tier"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers '((fast :provider "Fast:fast-model")))
          (niwa-model-workloads '((buddy :tier fast))))
      (should (eq 'fast-model
                  (plist-get (niwa-model-resolve-workload 'buddy)
                             :model)))))
  :doc "the buddy workload honors an exact provider override"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers '((fast :provider "Fast:fast-model")))
          (niwa-model-workloads '((buddy :provider "Ollama:llama3.1:8b"))))
      (let ((policy (niwa-model-resolve-workload 'buddy)))
        (should (equal "Ollama"
                       (gptel-backend-name (plist-get policy :backend))))
        (should (eq 'llama3.1:8b (plist-get policy :model))))))
  :doc "a preset may override the buddy workload without touching others"
  (let ((merged (niwa-model-merge-workloads
                 '((buddy :provider "Fast:fast-model"))
                 '((buddy :tier fast) (guardian :tier fast)))))
    (should (equal '(:provider "Fast:fast-model") (alist-get 'buddy merged)))
    (should (equal '(:tier fast) (alist-get 'guardian merged))))
  :doc "missing workloads inherit the session provider and effort"
  (niwa-models-test--with-backends
    (let ((gptel-backend (gptel-get-backend "Balanced"))
          (gptel-model 'balanced-model)
          (gptel-reasoning-effort nil)
          (niwa-model-workloads nil))
      (let ((policy (niwa-model-resolve-workload 'missing)))
        (should (eq gptel-backend (plist-get policy :backend)))
        (should (eq gptel-model (plist-get policy :model)))
        (should-not (plist-get policy :effort)))))
  :doc "settled policy is fixed while a later dispatch sees session changes"
  (niwa-models-test--with-backends
    (let ((gptel-backend (gptel-get-backend "Balanced"))
          (gptel-model 'balanced-model)
          (gptel-reasoning-effort nil)
          (niwa-model-tiers
           '((first :provider "Fast:fast-model")
             (second :provider "Ollama:llama3.1:8b")))
          (niwa-model-workloads '((planning :tier first))))
      (let ((in-flight (niwa-model-resolve-workload 'planning)))
        (setq niwa-model-workloads '((planning :tier second)))
        (let ((next-phase (niwa-model-resolve-workload 'planning)))
          (should (eq 'fast-model (plist-get in-flight :model)))
          (should (eq 'llama3.1:8b (plist-get next-phase :model))))))))

(niwa-deftest niwa-model-current-label ()
  ,test
  (test)

  :doc "returns none when no current model is set"
  (with-temp-buffer
    (setq-local gptel-model nil)
    (should (equal "none" (niwa-model-current-label))))

  :doc "returns the current model name"
  (with-temp-buffer
    (setq-local gptel-model 'fast-model)
    (should (equal "fast-model" (niwa-model-current-label)))))

(niwa-deftest niwa-model-current-provider-label ()
  ,test
  (test)

  :doc "returns none when no model is set"
  (with-temp-buffer
    (setq-local gptel-model nil)
    (should (equal "none" (niwa-model-current-provider-label))))

  :doc "returns backend:model when both are set"
  (niwa-models-test--with-backends
    (with-temp-buffer
      (setq-local gptel-backend (gptel-get-backend "Fast"))
      (setq-local gptel-model 'fast-model)
      (should (equal "Fast:fast-model"
                     (niwa-model-current-provider-label)))))

  :doc "falls back to the model label without a backend"
  (with-temp-buffer
    (setq-local gptel-backend nil)
    (setq-local gptel-model 'balanced-model)
    (should (equal "balanced-model"
                   (niwa-model-current-provider-label)))))

(niwa-deftest niwa-model-parse-selector ()
  ,test
  (test)

  :doc "parses tier strings as tier selectors"
  (should (equal '(:tier fast)
                 (niwa-model-parse-selector "fast")))

  :doc "parses BACKEND:MODEL strings as concrete provider selectors"
  (niwa-models-test--with-backends
    (let ((selector (niwa-model-parse-selector "Fast:fast-model")))
      (should (equal "Fast" (gptel-backend-name
                             (plist-get selector :backend))))
      (should (eq 'fast-model (plist-get selector :model))))))

(niwa-deftest niwa-model-resolve-selector
  ()
  ,test
  (test)
  :doc "resolves configured tier selectors"
  (niwa-models-test--with-backends
    (let ((niwa-model-tiers
           '((custom :provider "Fast:fast-model"))))
      (should (eq 'fast-model
                  (plist-get
                   (niwa-model-resolve-selector '(:tier custom))
                   :model))))))

(niwa-deftest niwa-model-context-window-source ()
  ,test
  (test)
  :doc "distinguishes declared, configured, and built-in windows with correct units"
  (let ((model (make-symbol "context-provenance"))
        (niwa-model-context-limit nil))
    (should (eq 'built-in-fallback (niwa-model-context-window-source model)))
    (should (= 128000 (niwa-model-effective-context-window model)))
    (let ((niwa-model-context-limit 256000))
      (should (eq 'configured-fallback (niwa-model-context-window-source model)))
      (should (= 256000 (niwa-model-effective-context-window model)))
      (put model :context-window 272.5)
      (let ((gptel-model model))
        (should (eq 'model-metadata (niwa-model-context-window-source)))
        (should (= 272500 (niwa-model-effective-context-window)))))))

(niwa-deftest niwa-model--max-output-tokens ()
  ,test
  (test)
  :doc "prefers the explicit policy output limit"
  (should (= 321 (niwa-model--max-output-tokens '(:max-tokens 321)))))

(niwa-deftest niwa-model-usable-input-tokens ()
  ,test
  (test)
  :doc "caps the reserve for a small model context"
  (let ((model (make-symbol "small-model"))
        (niwa-model-reserve-tokens 20000))
    (put model :context-window 8)
    (should (= 4000 (niwa-model-usable-input-tokens
                     (list :model model :max-tokens nil)))))

  :doc "falls back to the configured context limit without model metadata"
  (let ((niwa-model-context-limit 40000)
        (niwa-model-reserve-tokens 10000))
    (should (= 30000 (niwa-model-usable-input-tokens
                      '(:model nil :max-tokens nil)))))

  :doc "preserves caller request parameters while merging provider defaults"
  (let* ((gptel--known-backends nil)
         (model (make-symbol "budget-model"))
         (backend
          (gptel-make-openai
            "Budget"
            :key "test"
            :models (list model)
            :request-params '(:max_tokens 4096)))
         (params (list :max_tokens 7 :custom t))
         (before (copy-tree params)))
    (put model :context-window 100)
    (niwa-model-usable-input-tokens
     (list :backend backend
           :model model
           :max-tokens nil
           :request-params params))
    (should (equal before params))))

(provide 'test-niwa-models)
;;; test-niwa-models.el ends here
