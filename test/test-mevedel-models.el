;;; test-mevedel-models.el --- Tests for model tier resolution -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'gptel-openai)
(require 'mevedel-models)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Provider and tier resolution

(defmacro mevedel-models-test--with-backends (&rest body)
  "Run BODY with an isolated gptel backend registry."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     (gptel-make-openai "Ollama" :key "test" :models '(llama3.1:8b))
     ,@body))

(mevedel-deftest mevedel-model-resolve-provider ()
  ,test
  (test)

  :doc "resolves gptel transient-style BACKEND:MODEL strings"
  (mevedel-models-test--with-backends
    (let ((provider (mevedel-model-resolve-provider "Fast:fast-model")))
      (should (equal "Fast" (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'fast-model (plist-get provider :model)))))

  :doc "splits at the first colon so provider strings support colon models"
  (mevedel-models-test--with-backends
    (let ((provider (mevedel-model-resolve-provider "Ollama:llama3.1:8b")))
      (should (equal "Ollama" (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'llama3.1:8b (plist-get provider :model)))))

  :doc "resolves structured (BACKEND . MODEL) specs"
  (mevedel-models-test--with-backends
    (let ((provider (mevedel-model-resolve-provider '("Balanced" . balanced-model))))
      (should (equal "Balanced"
                     (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'balanced-model (plist-get provider :model))))))

(mevedel-deftest mevedel-model-resolve-tier ()
  ,test
  (test)

  :doc "resolves configured tiers to provider plists"
  (mevedel-models-test--with-backends
    (let ((mevedel-model-tiers
           '((fast :provider "Fast:fast-model" :effort nil)
             (balanced :provider "Balanced:balanced-model")
             (strong))))
      (let ((provider (mevedel-model-resolve-tier 'fast)))
        (should (equal "Fast" (gptel-backend-name
                               (plist-get provider :backend))))
        (should (eq 'fast-model (plist-get provider :model))))))

  :doc "unconfigured named tiers inherit current session policy"
  (mevedel-models-test--with-backends
    (let ((mevedel-model-tiers '((custom)))
          (gptel-backend (gptel-get-backend "Balanced"))
          (gptel-model 'balanced-model))
      (let ((policy (mevedel-model-resolve-tier 'custom)))
        (should (eq gptel-backend (plist-get policy :backend)))
        (should (eq gptel-model (plist-get policy :model)))))))

(mevedel-deftest mevedel-model--merge-map
  ()
  ,test
  (test)
  :doc "replaces entries by name without mutating the base map"
  (let* ((base '((fast :provider "A:a") (strong :effort high)))
         (result (mevedel-model--merge-map
                  '((fast :provider "B:b") (custom :effort low)) base)))
    (should (equal '(:provider "B:b") (alist-get 'fast result)))
    (should (equal '(:effort low) (alist-get 'custom result)))
    (should (equal '(:provider "A:a") (alist-get 'fast base)))))

(mevedel-deftest mevedel-model-merge-tiers
  ()
  ,test
  (test)
  :doc "accepts provider and effort tier fields"
  (should (equal '((fast :provider "Fast:fast-model" :effort low))
                 (mevedel-model-merge-tiers
                  '((fast :provider "Fast:fast-model" :effort low)) nil)))
  :doc "rejects unknown tier fields"
  (should-error (mevedel-model-merge-tiers '((fast :tier other)) nil)
                :type 'user-error))

(mevedel-deftest mevedel-model-merge-workloads
  ()
  ,test
  (test)
  :doc "merges workload entries by workload name"
  (should (equal '(:provider "Fast:fast-model" :effort high)
                 (alist-get
                  'planning
                  (mevedel-model-merge-workloads
                   '((planning :provider "Fast:fast-model" :effort high))
                   '((planning :tier balanced) (review :tier strong))))))
  :doc "rejects simultaneous tier and provider selectors"
  (should-error
   (mevedel-model-merge-workloads
    '((planning :tier balanced :provider "Fast:fast-model")) nil)
   :type 'user-error)
  :doc "accepts prefixed and qualified skill workload symbols"
  (let ((merged
         (mevedel-model-merge-workloads
          '(($grill-with-docs :tier strong)
            ($plugin:code-review :provider "Fast:fast-model" :effort high))
          nil)))
    (should (equal '(:tier strong)
                   (alist-get '$grill-with-docs merged)))
    (should (equal '(:provider "Fast:fast-model" :effort high)
                   (alist-get '$plugin:code-review merged))))
  :doc "rejects string skill workload keys"
  (should-error
   (mevedel-model-merge-workloads
    '(("$grill-with-docs" :tier strong)) nil)
   :type 'user-error))

(mevedel-deftest mevedel-model-skill-policy-fields
  ()
  ,test
  (test)
  :doc "combines frontmatter and preset field presence in stable order"
  (let ((mevedel-model-workloads
         '(($alpha :effort nil)
           ($plugin:beta :tier missing))))
    (should (equal '(model effort)
                   (mevedel-model-skill-policy-fields
                    "alpha" "front-tier" nil)))
    (should (equal '(model)
                   (mevedel-model-skill-policy-fields
                    "plugin:beta" nil nil)))
    (should (equal '(effort)
                   (mevedel-model-skill-policy-fields
                    "plain" nil 'high))))
  :doc "does not validate ignored preset values"
  (let ((mevedel-model-workloads
         '(($alpha :tier missing :provider "Missing:model" :effort impossible))))
    (should (equal '(model effort)
                   (mevedel-model-skill-policy-fields
                    "alpha" nil nil)))))

(mevedel-deftest mevedel-model-merge-skill-policy
  ()
  ,test
  (test)
  :doc "preset tier replaces model while frontmatter effort remains"
  (let ((mevedel-model-tiers '((fast) (strong)))
        (mevedel-model-workloads '(($alpha :tier strong))))
    (should (equal '(:model (:tier strong) :effort low)
                   (mevedel-model-merge-skill-policy
                    "alpha" "fast" 'low))))
  :doc "preset effort replaces effort while frontmatter model remains"
  (let ((mevedel-model-tiers '((fast)))
        (mevedel-model-workloads '(($alpha :effort high))))
    (should (equal '(:model (:tier fast) :effort high)
                   (mevedel-model-merge-skill-policy
                    "alpha" "fast" 'low))))
  :doc "qualified preset provider replaces invalid frontmatter model"
  (mevedel-models-test--with-backends
    (let ((mevedel-model-workloads
           '(($plugin:alpha :provider "Fast:fast-model"))))
      (let ((policy (mevedel-model-merge-skill-policy
                     "plugin:alpha" "not-a-selector" 'medium)))
        (should (eq 'fast-model
                    (plist-get (plist-get policy :model) :model)))
        (should (eq 'medium (plist-get policy :effort))))))
  :doc "explicit nil preset fields replace frontmatter with inheritance"
  (let ((mevedel-model-tiers '((fast)))
        (mevedel-model-workloads '(($alpha :tier nil :effort nil))))
    (should (equal '(:model nil :effort nil)
                   (mevedel-model-merge-skill-policy
                    "alpha" "fast" 'high))))
  :doc "missing preset fields retain frontmatter"
  (let ((mevedel-model-tiers '((fast)))
        (mevedel-model-workloads nil))
    (should (equal '(:model (:tier fast) :effort medium)
                   (mevedel-model-merge-skill-policy
                    "alpha" "fast" 'medium))))
  :doc "conflicting preset selectors fail only during owning policy merge"
  (let ((mevedel-model-workloads
         '(($alpha :tier strong :provider "Fast:fast-model"))))
    (should-error
     (mevedel-model-merge-skill-policy "alpha" nil nil)
     :type 'user-error)))

(mevedel-deftest mevedel-model-validate-effort
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
           (mevedel-model-validate-effort 'fast-model 'high)
           :type 'user-error)
          (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
          (put 'fast-model :reasoning-effort '(member low medium high))
          (should (eq 'high
                      (mevedel-model-validate-effort 'fast-model 'high)))
          (should-error
           (mevedel-model-validate-effort 'fast-model 'max)
           :type 'user-error))
      (put 'gptel-reasoning-effort 'custom-type old-custom)
      (put 'fast-model :reasoning-effort old-effort))))

(mevedel-deftest mevedel-model-resolve-workload
  ()
  ,test
  (test)
  :doc "applies session, tier, workload, and explicit override precedence"
  (mevedel-models-test--with-backends
    (let ((old-custom (get 'gptel-reasoning-effort 'custom-type))
          (old-fast (get 'fast-model :reasoning-effort))
          (old-llama (get 'llama3.1:8b :reasoning-effort)))
      (unwind-protect
          (let ((gptel-backend (gptel-get-backend "Balanced"))
                (gptel-model 'balanced-model)
                (gptel-reasoning-effort 'low)
                (mevedel-model-tiers
                 '((strong :provider "Fast:fast-model" :effort medium)))
                (mevedel-model-workloads
                 '((planning :tier strong :effort high))))
            (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
            (put 'fast-model :reasoning-effort '(member low medium high))
            (put 'llama3.1:8b :reasoning-effort '(member low medium high max))
            (let ((policy
                   (mevedel-model-resolve-workload
                    'planning
                    (mevedel-model-resolve-provider "Ollama:llama3.1:8b")
                    'max)))
              (should (equal "Ollama"
                             (gptel-backend-name
                              (plist-get policy :backend))))
              (should (eq 'llama3.1:8b (plist-get policy :model)))
              (should (eq 'max (plist-get policy :effort)))))
        (put 'gptel-reasoning-effort 'custom-type old-custom)
        (put 'fast-model :reasoning-effort old-fast)
        (put 'llama3.1:8b :reasoning-effort old-llama))))
  :doc "missing workloads inherit the session provider and effort"
  (mevedel-models-test--with-backends
    (let ((gptel-backend (gptel-get-backend "Balanced"))
          (gptel-model 'balanced-model)
          (gptel-reasoning-effort nil)
          (mevedel-model-workloads nil))
      (let ((policy (mevedel-model-resolve-workload 'missing)))
        (should (eq gptel-backend (plist-get policy :backend)))
        (should (eq gptel-model (plist-get policy :model)))
        (should-not (plist-get policy :effort)))))
  :doc "settled policy is fixed while a later dispatch sees session changes"
  (mevedel-models-test--with-backends
    (let ((gptel-backend (gptel-get-backend "Balanced"))
          (gptel-model 'balanced-model)
          (gptel-reasoning-effort nil)
          (mevedel-model-tiers
           '((first :provider "Fast:fast-model")
             (second :provider "Ollama:llama3.1:8b")))
          (mevedel-model-workloads '((planning :tier first))))
      (let ((in-flight (mevedel-model-resolve-workload 'planning)))
        (setq mevedel-model-workloads '((planning :tier second)))
        (let ((next-phase (mevedel-model-resolve-workload 'planning)))
          (should (eq 'fast-model (plist-get in-flight :model)))
          (should (eq 'llama3.1:8b (plist-get next-phase :model))))))))

(mevedel-deftest mevedel-model-current-label ()
  ,test
  (test)

  :doc "returns none when no current model is set"
  (with-temp-buffer
    (should (equal "none" (mevedel-model-current-label))))

  :doc "returns the current model name"
  (with-temp-buffer
    (setq-local gptel-model 'fast-model)
    (should (equal "fast-model" (mevedel-model-current-label)))))

(mevedel-deftest mevedel-model-current-provider-label ()
  ,test
  (test)

  :doc "returns none when no model is set"
  (with-temp-buffer
    (should (equal "none" (mevedel-model-current-provider-label))))

  :doc "returns backend:model when both are set"
  (mevedel-models-test--with-backends
    (with-temp-buffer
      (setq-local gptel-backend (gptel-get-backend "Fast"))
      (setq-local gptel-model 'fast-model)
      (should (equal "Fast:fast-model"
                     (mevedel-model-current-provider-label)))))

  :doc "falls back to the model label without a backend"
  (with-temp-buffer
    (setq-local gptel-model 'balanced-model)
    (should (equal "balanced-model"
                   (mevedel-model-current-provider-label)))))

(mevedel-deftest mevedel-model-parse-selector ()
  ,test
  (test)

  :doc "parses tier strings as tier selectors"
  (should (equal '(:tier fast)
                 (mevedel-model-parse-selector "fast")))

  :doc "parses BACKEND:MODEL strings as concrete provider selectors"
  (mevedel-models-test--with-backends
    (let ((selector (mevedel-model-parse-selector "Fast:fast-model")))
      (should (equal "Fast" (gptel-backend-name
                             (plist-get selector :backend))))
      (should (eq 'fast-model (plist-get selector :model))))))

(mevedel-deftest mevedel-model-resolve-selector
  ()
  ,test
  (test)
  :doc "resolves configured tier selectors"
  (mevedel-models-test--with-backends
    (let ((mevedel-model-tiers
           '((custom :provider "Fast:fast-model"))))
      (should (eq 'fast-model
                  (plist-get
                   (mevedel-model-resolve-selector '(:tier custom))
                   :model))))))

(mevedel-deftest mevedel-model-apply-provider-to-info ()
  ,test
  (test)

  :doc "applies backend and model to FSM info and patches plist data"
  (mevedel-models-test--with-backends
    (let* ((backend (gptel-get-backend "Fast"))
           (provider (mevedel-model-resolve-provider "Fast:fast-model"))
           (info (mevedel-model-apply-provider-to-info
                  (list :backend backend
                        :data '(:model "old" :messages []))
                  provider)))
      (should (equal "Fast" (gptel-backend-name (plist-get info :backend))))
      (should (eq 'fast-model (plist-get info :model)))
      (should (equal "fast-model"
                     (plist-get (plist-get info :data) :model)))))

  :doc "rejects cross-backend switches after request data is realized"
  (mevedel-models-test--with-backends
    (let ((provider (mevedel-model-resolve-provider "Balanced:balanced-model")))
      (should-error
       (mevedel-model-apply-provider-to-info
        (list :backend (gptel-get-backend "Fast")
              :data '(:model "old" :messages []))
       provider)
       :type 'user-error))))

(mevedel-deftest mevedel-model-apply-policy-to-info
  ()
  ,test
  (test)
  :doc "records effort while applying the provider"
  (mevedel-models-test--with-backends
    (let* ((backend (gptel-get-backend "Fast"))
           (policy (append
                    (mevedel-model-resolve-provider "Fast:fast-model")
                    '(:effort high)))
           (info (mevedel-model-apply-policy-to-info
                  (list :backend backend) policy)))
      (should (eq 'fast-model (plist-get info :model)))
      (should (eq 'high (plist-get info :reasoning-effort))))))

(provide 'test-mevedel-models)
;;; test-mevedel-models.el ends here
