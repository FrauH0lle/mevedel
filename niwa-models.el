;;; niwa-models.el -- Model tier/provider resolution -*- lexical-binding: t -*-

;;; Commentary:

;; Session-local model-team policy.  Named tiers and workload assignments
;; resolve to exact backend/model/effort tuples at dispatch time.  Provider
;; and reasoning-effort details remain delegated to gptel.

;;; Code:

(require 'cl-lib)
(require 'niwa-engine)
(require 'niwa-recovery)

;; `gptel-request'
(declare-function gptel--merge-plists "ext:gptel-request" (&rest plists))
(declare-function gptel--model-request-params "ext:gptel-request" (model))
(declare-function gptel-backend-models "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-request-params "ext:gptel-request" (backend))
(declare-function gptel-get-backend "ext:gptel-request" (name))
(defvar gptel--known-backends)
(defvar gptel--request-params)

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(declare-function gptel--model-name "ext:gptel" (model))
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `niwa-structs'
(declare-function niwa-session-model-provider "niwa-structs" (cl-x) t)
(declare-function niwa-session-reasoning-effort "niwa-structs" (cl-x) t)
(declare-function niwa-session-set-model-provider
                  "niwa-structs" (session provider))
(declare-function niwa-session-set-reasoning-effort
                  "niwa-structs" (session effort))


;;
;;; Customization

(defcustom niwa-model-context-limit nil
  "Fallback context window in tokens.

Niwa uses this only when the active model has no declared
`:context-window'.  When nil, the fallback is 128000 tokens."
  :type '(choice (const :tag "Use built-in fallback" nil)
          (natnum :tag "Fallback token count"))
  :group 'niwa)

(defcustom niwa-model-reserve-tokens 20000
  "Token headroom reserved below the model context window."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-model-tiers
  '((fast)
    (balanced)
    (strong))
  "Alist mapping named tiers to provider and effort policy.

Each entry is (NAME :provider PROVIDER :effort EFFORT).  PROVIDER is nil,
\"BACKEND:MODEL\", or (BACKEND-NAME . MODEL).  Nil fields inherit the
session backend, model, and reasoning effort."
  :group 'niwa
  :type '(repeat sexp))

(defcustom niwa-model-workloads
  '((planning :tier balanced)
    (plan-implementation)
    (goal-review :tier strong)
    (naming :tier fast)
    (explorer :tier fast)
    (verifier :tier balanced)
    (reviewer :tier strong)
    (guardian :tier fast)
    (buddy :tier fast)
    (journal :tier balanced)
    (memory :tier balanced)
    (summarization :tier balanced))
  "Alist mapping workloads to tier or exact-provider policy.

Each entry is (WORKLOAD :tier TIER :provider PROVIDER :effort EFFORT).
`:tier' and `:provider' are mutually exclusive.  Missing entries and fields
inherit the current session values."
  :group 'niwa
  :type '(repeat sexp))


;;
;;; Context budget

(defsubst niwa-model-context-window (&optional model)
  "Return MODEL's context window in tokens.
MODEL defaults to `gptel-model'.  gptel stores `:context-window' in
thousands of tokens, sometimes as a float."
  (when-let* ((m (or model gptel-model))
              (kt (get m :context-window)))
    (round (* kt 1000))))

(defsubst niwa-model-effective-context-window (&optional model)
  "Return the context window in tokens to budget MODEL against.
MODEL defaults to `gptel-model'.  A model that declares no window falls
back to `niwa-model-context-limit', and then to a conservative
default, so a caller always has a number to divide."
  (or (niwa-model-context-window model)
      niwa-model-context-limit
      128000))

(defun niwa-model-context-window-source (&optional model)
  "Return the provenance of MODEL's effective context window.
MODEL defaults to `gptel-model'.  Metadata is a local declaration, not
confirmation from the provider that the declared window is available."
  (cond
   ((niwa-model-context-window model) 'model-metadata)
   (niwa-model-context-limit 'configured-fallback)
   (t 'built-in-fallback)))

(defun niwa-model--max-output-tokens (policy)
  "Return POLICY's configured maximum output token count, or zero."
  (let ((gptel-backend (plist-get policy :backend))
        (gptel-model (plist-get policy :model))
        (gptel-max-tokens (plist-get policy :max-tokens))
        (gptel--request-params (plist-get policy :request-params)))
    (or gptel-max-tokens
        (when (and gptel-backend
                   (fboundp 'gptel--merge-plists)
                   (fboundp 'gptel-backend-request-params)
                   (fboundp 'gptel--model-request-params))
          (let ((params
                 (gptel--merge-plists
                  (copy-sequence gptel--request-params)
                  (gptel-backend-request-params gptel-backend)
                  (gptel--model-request-params gptel-model))))
            (or (plist-get params :max_tokens)
                (plist-get params :maxOutputTokens)
                (plist-get params :max_output_tokens)
                (plist-get params :num_predict))))
        0)))

(defun niwa-model-usable-input-tokens (policy)
  "Return usable input context tokens for model POLICY.

POLICY is a plist carrying `:backend', `:model', `:max-tokens', and
`:request-params'.  The response reserve is capped at half the context
window so small local models keep a meaningful budget."
  (let* ((context (niwa-model-effective-context-window
                   (plist-get policy :model)))
         (reserve (max niwa-model-reserve-tokens
                       (niwa-model--max-output-tokens policy))))
    (max 1 (- context (min reserve (max 1 (/ context 2)))))))


;;
;;; Provider parsing

(defun niwa-model-normalize-tier (value)
  "Return VALUE as a configured tier symbol, or nil."
  (let ((sym (if (stringp value) (intern-soft value) value)))
    (and (symbolp sym) (assq sym niwa-model-tiers) sym)))

(defun niwa-model--merge-map (additions current)
  "Merge keyed ADDITIONS over CURRENT and return a fresh alist."
  (let ((result (copy-tree current)))
    (dolist (entry additions)
      (unless (and (consp entry) (symbolp (car entry)))
        (user-error "Invalid model policy entry %S" entry))
      (setf (alist-get (car entry) result) (copy-tree (cdr entry))))
    result))

(defun niwa-model-merge-tiers (additions current)
  "Merge tier ADDITIONS over CURRENT for preset composition."
  (dolist (entry additions)
    (let ((keys (cdr entry)))
      (unless (and (cl-evenp (length keys))
                   (cl-loop for (key _) on keys by #'cddr
                            always (memq key '(:provider :effort))))
        (user-error "Invalid model tier entry %S" entry))))
  (niwa-model--merge-map additions current))

(defun niwa-model-merge-workloads (additions current)
  "Merge workload ADDITIONS over CURRENT for preset composition."
  (dolist (entry additions)
    (let ((keys (cdr entry)))
      (unless (and (cl-evenp (length keys))
                   (cl-loop for (key _) on keys by #'cddr
                            always (memq key '(:tier :provider :effort))))
        (user-error "Invalid model workload entry %S" entry))
      (when (and (plist-member keys :tier) (plist-member keys :provider))
        (user-error "Workload %s cannot select both tier and provider"
                    (car entry)))))
  (niwa-model--merge-map additions current))

(defun niwa-model-provider-p (value)
  "Return non-nil when VALUE is a resolved provider plist."
  (and (listp value)
       (plist-member value :backend)
       (plist-member value :model)))

(defun niwa-model--split-provider-string (value)
  "Split VALUE as \"BACKEND:MODEL\" and return (BACKEND MODEL).

The split happens at the first colon so model names such as
\"llama3.1:8b\" remain intact."
  (when (and (stringp value)
             (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" value))
    (list (match-string 1 value)
          (match-string 2 value))))

(cl-defmethod niwa-model--find-model ((backend gptel-backend) model-name)
  "Return BACKEND's model matching MODEL-NAME, or nil."
  (cl-find-if
   (lambda (model)
     (equal (gptel--model-name model) model-name))
   (gptel-backend-models backend)))

(defun niwa-model-resolve-provider (spec &optional noerror)
  "Resolve provider SPEC to a plist (:backend BACKEND :model MODEL).

SPEC is either \"BACKEND:MODEL\" or (BACKEND-NAME . MODEL).  When
NOERROR is non-nil, return nil instead of signaling `user-error'."
  (condition-case err
      (let* ((pair (cond
                    ((stringp spec)
                     (or (niwa-model--split-provider-string spec)
                         (user-error "Model provider must be BACKEND:MODEL: %S"
                                     spec)))
                    ((consp spec)
                     (list (car spec) (cdr spec)))
                    (t
                     (user-error "Invalid model provider: %S" spec))))
             (backend-name (if (symbolp (car pair))
                               (symbol-name (car pair))
                             (car pair)))
             (model-name (if (symbolp (cadr pair))
                             (symbol-name (cadr pair))
                           (cadr pair)))
             (backend (gptel-get-backend backend-name))
             (model (and backend (niwa-model--find-model backend model-name))))
        (unless model
          (user-error "Model %s is not registered on backend %s"
                      model-name backend-name))
        (list :backend backend :model model))
    (error
     (if noerror
         nil
       (signal (car err) (cdr err))))))

(defun niwa-model--provider-label (provider)
  "Return the exact serializable label for resolved PROVIDER."
  (format "%s:%s"
          (gptel-backend-name (plist-get provider :backend))
          (gptel--model-name (plist-get provider :model))))

(defun niwa-model-candidates ()
  "Return registered model candidates as (LABEL . PROVIDER) pairs."
  (let (candidates)
    (dolist (entry (and (boundp 'gptel--known-backends)
                        gptel--known-backends))
      (let ((backend (cdr entry)))
        (dolist (model (and (fboundp 'gptel-backend-models)
                            (gptel-backend-models backend)))
          (let ((provider (list :backend backend :model model)))
            (push (cons (niwa-model--provider-label provider) provider)
                  candidates)))))
    (sort candidates (lambda (a b) (string< (car a) (car b))))))

(defun niwa-model-supported-efforts (model)
  "Return MODEL's declared reasoning effort symbols."
  (let ((type (and (symbolp model) (get model :reasoning-effort))))
    (and (consp type)
         (eq (car type) 'member)
         (cl-remove-if-not #'symbolp (cdr type)))))

(defun niwa-model-set-session-provider (session provider &optional buffer)
  "Set SESSION's resolved PROVIDER in BUFFER.
Keep the current reasoning effort when PROVIDER supports it; otherwise reset
the effort to its default and report the reset.  The selection resolves root
model failures; other recovery holds remain."
  (let ((buffer (or buffer (current-buffer)))
        (backend (plist-get provider :backend))
        (model (plist-get provider :model)))
    (unless (and backend model)
      (user-error "Invalid model provider"))
    (with-current-buffer buffer
      (let ((effort (and (boundp 'gptel-reasoning-effort)
                         gptel-reasoning-effort)))
        (when (and effort
                   (condition-case nil
                       (progn
                         (niwa-model-validate-effort model effort)
                         nil)
                     (user-error t)))
          (setq effort nil)
          (message "niwa: reasoning effort reset to default for %s"
                   (gptel--model-name model)))
        (setq-local gptel-backend backend)
        (setq-local gptel-model model)
        (setq-local gptel-reasoning-effort effort)
        (niwa-session-set-model-provider
         session (niwa-model--provider-label provider))
        (niwa-session-set-reasoning-effort session effort)))
    (dolist (issue (niwa-session-recovery-issues session))
      (when (and (equal "model" (plist-get issue :category))
                 (member (plist-get issue :id) '("model" "request" "authentication")))
        (niwa-recovery-clear session (plist-get issue :id))))
    provider))

(defun niwa-model-set-session-effort (session effort &optional buffer)
  "Set SESSION's reasoning EFFORT in BUFFER.
Nil selects the model's default effort."
  (with-current-buffer (or buffer (current-buffer))
    (niwa-model-validate-effort gptel-model effort)
    (setq-local gptel-reasoning-effort effort)
    (niwa-session-set-reasoning-effort session effort))
  effort)

(defcustom niwa-model-fallback-provider nil
  "Provider used when a saved selection disappears.
Nil uses the host's default backend and model.  Never searches other providers."
  :type '(choice (const :tag "Host default" nil) string) :group 'niwa)

(defun niwa-model-recover-provider (selector &optional session unavailable)
  "Resolve saved SELECTOR, using the configured fallback for SESSION.
Return nil when neither selection is available; using the fallback leaves
SESSION an informational notice.  Explicit user selections must use
`niwa-model-resolve-provider' instead.
UNAVAILABLE means live provider capabilities already rejected SELECTOR."
  (or (and (not unavailable)
           (niwa-model-resolve-provider selector t))
      (let* ((fallback (or niwa-model-fallback-provider
                           (when (and (default-value 'gptel-backend)
                                      (default-value 'gptel-model))
                             (niwa-model--provider-label
                              (list :backend (default-value 'gptel-backend)
                                    :model (default-value 'gptel-model))))))
             (provider (and fallback
                            (not (and unavailable (equal fallback selector)))
                            (niwa-model-resolve-provider fallback t))))
        (when (and session provider)
          (niwa-recovery-report
           session "model" 'model
           (format "%s is unavailable; using %s" selector fallback) nil))
        provider)))

(defun niwa-model-apply-session-policy (session &optional buffer)
  "Restore SESSION into BUFFER without making unavailable history unreadable.
A saved model with no available fallback blocks requests until a model is
selected; a model that resolves again clears that block."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((selector (niwa-session-model-provider session))
           (provider (and selector (niwa-model-recover-provider selector session))))
      (cond
       (provider
        (when (equal selector (niwa-model--provider-label provider))
          (niwa-recovery-clear session "model"))
        (setq-local gptel-backend (plist-get provider :backend)
                    gptel-model (plist-get provider :model))
        (niwa-session-set-model-provider session (niwa-model--provider-label provider)))
       (selector
        (niwa-recovery-report session "model" 'model
                                 (format "%s is unavailable; select a model" selector) t)))
      (let ((effort (niwa-session-reasoning-effort session)))
        (condition-case nil
            (niwa-model-validate-effort gptel-model effort)
          (user-error
           (niwa-recovery-report session "effort" 'configuration
                                    "Saved reasoning effort is unsupported; using the model default" nil)
           (setq effort nil)))
        (setq-local gptel-reasoning-effort effort)
        (niwa-session-set-reasoning-effort session effort))))
  session)


;;
;;; Tier and selector resolution

(defun niwa-model-tier-selector (tier)
  "Return a model selector plist for TIER."
  (let ((sym (niwa-model-normalize-tier tier)))
    (unless sym
      (user-error "Unknown model tier %S" tier))
    (list :tier sym)))

(defun niwa-model-resolve-tier (tier &optional noerror)
  "Resolve TIER through `niwa-model-tiers' over session defaults.
When NOERROR is non-nil, invalid configuration returns nil."
  (let* ((sym (niwa-model-normalize-tier tier))
         (configured (and sym (alist-get sym niwa-model-tiers))))
    (cond
     ((not sym)
      (if noerror
          nil
        (user-error "Unknown model tier %S" tier)))
     (t
      (condition-case err
          (let* ((provider-spec (plist-get configured :provider))
                 (provider (and provider-spec
                                (niwa-model-recover-provider
                                 provider-spec (bound-and-true-p niwa--session)))))
            (when (and provider-spec (not provider))
              (user-error "Provider %s is unavailable for tier %s" provider-spec tier))
            (list :backend (or (plist-get provider :backend) gptel-backend)
                  :model (or (plist-get provider :model) gptel-model)
                  :effort (if (plist-member configured :effort)
                              (plist-get configured :effort)
                            (and (boundp 'gptel-reasoning-effort)
                                 gptel-reasoning-effort))))
        (error
         (if noerror
             nil
           (signal (car err) (cdr err)))))))))

(defun niwa-model-parse-selector (value)
  "Parse VALUE as a skill or internal model selector.

VALUE must be a tier name or a concrete \"BACKEND:MODEL\" provider
string.  Returns either (:tier TIER) or (:backend BACKEND :model MODEL)."
  (cond
   ((null value) nil)
   ((niwa-model-normalize-tier value)
    (niwa-model-tier-selector value))
   ((and (stringp value)
         (niwa-model--split-provider-string value))
    (niwa-model-resolve-provider value))
   (t
    (user-error
     "Model must be a configured tier or BACKEND:MODEL: %S"
     value))))

(defun niwa-model-parse-effort (value)
  "Parse optional model-facing reasoning effort VALUE.

Model-specific support is validated later by
`niwa-model-resolve-workload'."
  (cond
   ((null value) nil)
   ((and (stringp value) (not (equal value "")))
    (or (intern value)
        (user-error "Reasoning effort must not be nil")))
   (t (user-error "Reasoning effort must be a non-empty string: %S" value))))

(defun niwa-model-resolve-selector (selector &optional noerror)
  "Resolve SELECTOR to a provider plist, or nil for inherit.

SELECTOR may be nil, (:tier TIER), or a provider plist.  When NOERROR is
non-nil, return nil instead of signaling for invalid selectors."
  (cond
   ((null selector) nil)
   ((niwa-model-provider-p selector) selector)
   ((plist-member selector :tier)
    (niwa-model-resolve-tier (plist-get selector :tier) noerror))
   (noerror nil)
   (t (user-error "Invalid model selector %S" selector))))


;;
;;; Skill workload policy

(defun niwa-model-skill-policy-fields (skill-name model effort)
  "Return policy fields declared for SKILL-NAME without validating them.

MODEL and EFFORT are the skill frontmatter values.  Preset-side fields come
from SKILL-NAME's `$skill-name' workload.  The result contains `model' and/or
`effort' in that order when either source declares the corresponding field.
This function deliberately does not parse model selectors or validate effort."
  (let ((spec (alist-get (intern (concat "$" skill-name))
                         niwa-model-workloads)))
    (delq nil
          (list (and (or model
                         (plist-member spec :tier)
                         (plist-member spec :provider))
                     'model)
                (and (or effort (plist-member spec :effort)) 'effort)))))

(defun niwa-model-merge-skill-policy (skill-name model effort)
  "Merge SKILL-NAME's preset workload over frontmatter MODEL and EFFORT.

The return value is `(:model SELECTOR :effort EFFORT)'.  A preset `:tier' or
`:provider' field replaces MODEL; a preset `:effort' field replaces EFFORT.
Missing preset fields retain their frontmatter value.  MODEL is parsed only
when no preset model selector replaces it, so superseded third-party metadata
cannot make an owning request fail.  Provider-dependent validation still
happens here only when the caller has established request ownership."
  (let* ((workload (intern (concat "$" skill-name)))
         (spec (alist-get workload niwa-model-workloads))
         (tier-p (plist-member spec :tier))
         (provider-p (plist-member spec :provider)))
    (when (and tier-p provider-p)
      (user-error "Workload %s cannot select both tier and provider" workload))
    (list :model
          (cond
           (tier-p
            (when-let* ((tier (plist-get spec :tier)))
              (niwa-model-tier-selector tier)))
           (provider-p
            (when-let* ((provider (plist-get spec :provider)))
              (or (niwa-model-recover-provider provider (bound-and-true-p niwa--session))
                  (user-error "No available provider for skill %s" skill-name))))
           (model (niwa-model-parse-selector model)))
          :effort
          (if (plist-member spec :effort)
              (plist-get spec :effort)
            effort))))

(defun niwa-model-validate-effort (model effort)
  "Return EFFORT when gptel says MODEL supports it, or signal an error."
  (when effort
    (let ((type (and (symbolp model) (get model :reasoning-effort))))
      (unless type
        (user-error "Model %s does not support reasoning effort"
                    (gptel--model-name model)))
      (unless (cl-typep effort type)
        (user-error "Reasoning effort %S is unsupported by model %s"
                    effort (gptel--model-name model)))))
  effort)

(defun niwa-model-resolve-workload
    (workload &optional explicit-selector explicit-effort)
  "Resolve WORKLOAD with optional explicit model and effort overrides.

Resolution starts from the current session backend/model/effort, then applies
the workload's tier, exact provider and effort, followed by explicit overrides."
  (let* ((name (and workload
                    (if (symbolp workload) workload (intern workload))))
         (spec (and name (alist-get name niwa-model-workloads)))
         (tier (plist-get spec :tier))
         (provider-spec (plist-get spec :provider)))
    (when (and tier provider-spec)
      (user-error "Workload %s cannot select both tier and provider" name))
    (let* ((policy (if tier
                       (niwa-model-resolve-tier tier)
                     (list :backend gptel-backend
                           :model gptel-model
                           :effort (and (boundp 'gptel-reasoning-effort)
                                        gptel-reasoning-effort))))
           (provider (and provider-spec
                          (or (niwa-model-recover-provider provider-spec
							      (bound-and-true-p niwa--session))
                              (user-error "Provider %s is unavailable for workload %s" provider-spec workload))))
           (explicit (and explicit-selector
                          (niwa-model-resolve-selector explicit-selector))))
      (when provider
        (setq policy (plist-put policy :backend (plist-get provider :backend))
              policy (plist-put policy :model (plist-get provider :model))))
      (when (plist-member spec :effort)
        (setq policy (plist-put policy :effort (plist-get spec :effort))))
      (when explicit
        (setq policy (plist-put policy :backend (plist-get explicit :backend))
              policy (plist-put policy :model (plist-get explicit :model)))
        (when (plist-member explicit :effort)
          (setq policy (plist-put policy :effort
                                  (plist-get explicit :effort)))))
      (when explicit-effort
        (setq policy (plist-put policy :effort explicit-effort)))
      (condition-case err
          (niwa-model-validate-effort
           (plist-get policy :model) (plist-get policy :effort))
        (user-error
         (if explicit-effort
             (signal (car err) (cdr err))
           (setq policy (plist-put policy :effort nil))
           (when (bound-and-true-p niwa--session)
             (niwa-recovery-report niwa--session "effort" 'configuration
                                      "Inherited reasoning effort is unsupported; using the model default" nil)))))
      policy)))

(defun niwa-model-current-label (&optional buffer)
  "Return BUFFER's current model label, or \"none\"."
  (let ((model (buffer-local-value 'gptel-model (or buffer (current-buffer)))))
    (if model (gptel--model-name model) "none")))

(defun niwa-model-current-provider-label (&optional buffer)
  "Return BUFFER's current backend:model label, or \"none\"."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((and (bound-and-true-p gptel-backend)
           (bound-and-true-p gptel-model))
      (format "%s:%s"
              (gptel-backend-name gptel-backend)
              (gptel--model-name gptel-model)))
     ((bound-and-true-p gptel-model)
      (niwa-model-current-label))
     (t "none"))))

(defun niwa-model-selection-delta (model effort)
  "Return how MODEL and EFFORT would change the current buffer's selection.
MODEL is a tier or BACKEND:MODEL text and EFFORT an effort name, either
nil, as a wake-up stores them.  Return a plist of `:model', the resolved
BACKEND:MODEL label, and `:effort', the resolved effort or \"default\", each
only when it differs from the current one.  A choice that no longer resolves
is returned as given."
  (condition-case nil
      ;; Without a session, an unsupported inherited effort is not reported
      ;; as a recovery issue: this only describes the choice.
      (let* ((policy (let ((niwa--session nil))
                       (niwa-model-resolve-workload
                        nil (niwa-model-parse-selector model)
                        (niwa-model-parse-effort effort))))
             (label (niwa-model--provider-label policy))
             (resolved (plist-get policy :effort)))
        (append (unless (equal label (niwa-model-current-provider-label))
                  (list :model label))
                (unless (equal resolved (bound-and-true-p gptel-reasoning-effort))
                  (list :effort (if resolved (format "%s" resolved) "default")))))
    (user-error (append (and model (list :model model)) (and effort (list :effort effort))))))


;;
;;; Agent tool description

(defconst niwa-model--tier-hints
  '((fast . "search, lookup and mechanical edits")
    (balanced . "routine implementation and verification")
    (strong . "hard design, debugging and review"))
  "Usage hint for each basic tier in the Agent tool's model description.")

(defun niwa-model--policy-label (policy &optional model-only)
  "Return POLICY's BACKEND:MODEL label, or only the model with MODEL-ONLY."
  (let ((model (gptel--model-name (plist-get policy :model))))
    (if model-only
        model
      (format "%s:%s" (gptel-backend-name (plist-get policy :backend)) model))))

(defun niwa-model--effort-label (effort)
  "Return the label for reasoning EFFORT, where nil is the provider default."
  (if effort (format "%s" effort) "default effort"))

(defun niwa-model-tier-guide (&optional compact)
  "Return the current buffer's resolved model tiers as prose for the model.
Each tier shows the provider and effort it selects, with a usage hint for
the basic tiers `fast', `balanced' and `strong'.  A tier that cannot be
resolved is `unavailable'.  When every tier is the session model the guide
says so instead of repeating it.  COMPACT returns only tier=model pairs."
  (let* ((session (list :backend gptel-backend :model gptel-model
                        :effort (bound-and-true-p gptel-reasoning-effort)))
         (tiers (mapcar (lambda (entry)
                          (let ((policy (niwa-model-resolve-tier
                                         (car entry) t)))
                            (cons (car entry)
                                  (and (plist-get policy :backend)
                                       (plist-get policy :model)
                                       policy))))
                        niwa-model-tiers))
         (session-label
          (format "%s (%s)" (niwa-model-current-provider-label)
                  (niwa-model--effort-label (plist-get session :effort)))))
    (cond
     ((cl-every (lambda (tier) (equal (cdr tier) session)) tiers)
      (if compact
          "all tiers use the session model"
        (format "All tiers use the session model (%s)." session-label)))
     (compact
      (mapconcat (lambda (tier)
                   (format "%s=%s" (car tier)
                           (if (cdr tier)
                               (niwa-model--policy-label (cdr tier) t)
                             "unavailable")))
                 tiers ", "))
     (t
      (format
       (concat "Tiers: %s. Omit model to use the role's workload tier; "
               "without a role the child inherits the session model (%s).")
       (mapconcat
        (lambda (tier)
          (let ((policy (cdr tier))
                (hint (alist-get (car tier) niwa-model--tier-hints)))
            (cond
             ((null policy) (format "%s = unavailable" (car tier)))
             (t (format "%s = %s (%s)%s" (car tier)
                        (niwa-model--policy-label policy)
                        (niwa-model--effort-label (plist-get policy :effort))
                        (if hint (concat ", for " hint) ""))))))
        tiers "; ")
       session-label)))))

(defun niwa-model-agent-tool-description ()
  "Return the Agent tool model-argument description for the current buffer."
  (concat
   "Optional model tier for this agent. "
   (niwa-model-tier-guide)))

(provide 'niwa-models)
;;; niwa-models.el ends here
