;;; mevedel-models.el -- Model tier/provider resolution -*- lexical-binding: t -*-

;;; Commentary:

;; Session-local model-team policy.  Named tiers and workload assignments
;; resolve to exact backend/model/effort tuples at dispatch time.  Provider
;; and reasoning-effort details remain delegated to gptel.

;;; Code:

(require 'cl-lib)

;; `gptel-request'
(declare-function gptel-backend-models "ext:gptel-request" (cl-x) t)
(declare-function gptel-get-backend "ext:gptel-request" (name))
(defvar gptel--known-backends)

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(declare-function gptel--model-name "ext:gptel" (model))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)


;;
;;; Customization

(defcustom mevedel-model-tiers
  '((fast)
    (balanced)
    (strong))
  "Alist mapping named tiers to provider and effort policy.

Each entry is (NAME :provider PROVIDER :effort EFFORT).  PROVIDER is nil,
\"BACKEND:MODEL\", or (BACKEND-NAME . MODEL).  Nil fields inherit the
session backend, model, and reasoning effort."
  :group 'mevedel
  :type '(repeat sexp))

(defcustom mevedel-model-workloads
  '((planning :tier balanced)
    (explorer :tier fast)
    (verifier :tier balanced)
    (reviewer :tier strong)
    (guardian :tier fast)
    (compaction :tier balanced))
  "Alist mapping workloads to tier or exact-provider policy.

Each entry is (WORKLOAD :tier TIER :provider PROVIDER :effort EFFORT).
`:tier' and `:provider' are mutually exclusive.  Missing entries and fields
inherit the current session values."
  :group 'mevedel
  :type '(repeat sexp))


;;
;;; Provider parsing

(defun mevedel-model-normalize-tier (value)
  "Return VALUE as a configured tier symbol, or nil."
  (let ((sym (if (stringp value) (intern-soft value) value)))
    (and (symbolp sym) (assq sym mevedel-model-tiers) sym)))

(defun mevedel-model--merge-map (additions current)
  "Merge keyed ADDITIONS over CURRENT and return a fresh alist."
  (let ((result (copy-tree current)))
    (dolist (entry additions)
      (unless (and (consp entry) (symbolp (car entry)))
        (user-error "Invalid model policy entry %S" entry))
      (setf (alist-get (car entry) result) (copy-tree (cdr entry))))
    result))

(defun mevedel-model-merge-tiers (additions current)
  "Merge tier ADDITIONS over CURRENT for preset composition."
  (dolist (entry additions)
    (let ((keys (cdr entry)))
      (unless (and (cl-evenp (length keys))
                   (cl-loop for (key _) on keys by #'cddr
                            always (memq key '(:provider :effort))))
        (user-error "Invalid model tier entry %S" entry))))
  (mevedel-model--merge-map additions current))

(defun mevedel-model-merge-workloads (additions current)
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
  (mevedel-model--merge-map additions current))

(defun mevedel-model-provider-p (value)
  "Return non-nil when VALUE is a resolved provider plist."
  (and (listp value)
       (plist-member value :backend)
       (plist-member value :model)))

(defun mevedel-model--split-provider-string (value)
  "Split VALUE as \"BACKEND:MODEL\" and return (BACKEND MODEL).

The split happens at the first colon so model names such as
\"llama3.1:8b\" remain intact."
  (when (and (stringp value)
             (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" value))
    (list (match-string 1 value)
          (match-string 2 value))))

(defun mevedel-model--find-model (backend model-name)
  "Return BACKEND's model matching MODEL-NAME, or nil."
  (cl-find-if
   (lambda (model)
     (equal (gptel--model-name model) model-name))
   (gptel-backend-models backend)))

(defun mevedel-model-resolve-provider (spec &optional noerror)
  "Resolve provider SPEC to a plist (:backend BACKEND :model MODEL).

SPEC is either \"BACKEND:MODEL\" or (BACKEND-NAME . MODEL).  When
NOERROR is non-nil, return nil instead of signaling `user-error'."
  (condition-case err
      (let* ((pair (cond
                    ((stringp spec)
                     (or (mevedel-model--split-provider-string spec)
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
             (model (and backend (mevedel-model--find-model backend model-name))))
        (unless model
          (user-error "Model %s is not registered on backend %s"
                      model-name backend-name))
        (list :backend backend :model model))
    (error
     (if noerror
         nil
       (signal (car err) (cdr err))))))


;;
;;; Tier and selector resolution

(defun mevedel-model-tier-selector (tier)
  "Return a model selector plist for TIER."
  (let ((sym (mevedel-model-normalize-tier tier)))
    (unless sym
      (user-error "Unknown model tier %S" tier))
    (list :tier sym)))

(defun mevedel-model-resolve-tier (tier &optional noerror)
  "Resolve TIER through `mevedel-model-tiers' over session defaults.
When NOERROR is non-nil, invalid configuration returns nil."
  (let* ((sym (mevedel-model-normalize-tier tier))
         (configured (and sym (alist-get sym mevedel-model-tiers))))
    (cond
     ((not sym)
      (if noerror
          nil
        (user-error "Unknown model tier %S" tier)))
     (t
      (condition-case err
          (let* ((provider-spec (plist-get configured :provider))
                 (provider (and provider-spec
                                (mevedel-model-resolve-provider
                                 provider-spec noerror))))
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

(defun mevedel-model-parse-selector (value)
  "Parse VALUE as a skill or internal model selector.

VALUE must be a tier name or a concrete \"BACKEND:MODEL\" provider
string.  Returns either (:tier TIER) or (:backend BACKEND :model MODEL)."
  (cond
   ((null value) nil)
   ((mevedel-model-normalize-tier value)
    (mevedel-model-tier-selector value))
   ((and (stringp value)
         (mevedel-model--split-provider-string value))
    (mevedel-model-resolve-provider value))
   (t
    (user-error
     "Model must be a configured tier or BACKEND:MODEL: %S"
     value))))

(defun mevedel-model-parse-effort (value)
  "Parse optional model-facing reasoning effort VALUE.

Model-specific support is validated later by
`mevedel-model-resolve-workload'."
  (cond
   ((null value) nil)
   ((and (stringp value) (not (equal value "")))
    (or (intern value)
        (user-error "Reasoning effort must not be nil")))
   (t (user-error "Reasoning effort must be a non-empty string: %S" value))))

(defun mevedel-model-resolve-selector (selector &optional noerror)
  "Resolve SELECTOR to a provider plist, or nil for inherit.

SELECTOR may be nil, (:tier TIER), or a provider plist.  When NOERROR is
non-nil, return nil instead of signaling for invalid selectors."
  (cond
   ((null selector) nil)
   ((mevedel-model-provider-p selector) selector)
   ((plist-member selector :tier)
    (mevedel-model-resolve-tier (plist-get selector :tier) noerror))
   (noerror nil)
   (t (user-error "Invalid model selector %S" selector))))


;;
;;; Skill workload policy

(defun mevedel-model-skill-policy-fields (skill-name model effort)
  "Return policy fields declared for SKILL-NAME without validating them.

MODEL and EFFORT are the skill frontmatter values.  Preset-side fields come
from SKILL-NAME's `$skill-name' workload.  The result contains `model' and/or
`effort' in that order when either source declares the corresponding field.
This function deliberately does not parse model selectors or validate effort."
  (let ((spec (alist-get (intern (concat "$" skill-name))
                         mevedel-model-workloads)))
    (delq nil
          (list (and (or model
                         (plist-member spec :tier)
                         (plist-member spec :provider))
                     'model)
                (and (or effort (plist-member spec :effort)) 'effort)))))

(defun mevedel-model-merge-skill-policy (skill-name model effort)
  "Merge SKILL-NAME's preset workload over frontmatter MODEL and EFFORT.

The return value is `(:model SELECTOR :effort EFFORT)'.  A preset `:tier' or
`:provider' field replaces MODEL; a preset `:effort' field replaces EFFORT.
Missing preset fields retain their frontmatter value.  MODEL is parsed only
when no preset model selector replaces it, so superseded third-party metadata
cannot make an owning request fail.  Provider-dependent validation still
happens here only when the caller has established request ownership."
  (let* ((workload (intern (concat "$" skill-name)))
         (spec (alist-get workload mevedel-model-workloads))
         (tier-p (plist-member spec :tier))
         (provider-p (plist-member spec :provider)))
    (when (and tier-p provider-p)
      (user-error "Workload %s cannot select both tier and provider" workload))
    (list :model
          (cond
           (tier-p
            (when-let* ((tier (plist-get spec :tier)))
              (mevedel-model-tier-selector tier)))
           (provider-p
            (when-let* ((provider (plist-get spec :provider)))
              (mevedel-model-resolve-provider provider)))
           (model (mevedel-model-parse-selector model)))
          :effort
          (if (plist-member spec :effort)
              (plist-get spec :effort)
            effort))))

(defun mevedel-model-validate-effort (model effort)
  "Return EFFORT when gptel says MODEL supports it, or signal an error."
  (when effort
    (unless (get 'gptel-reasoning-effort 'custom-type)
      (user-error "Installed gptel does not support reasoning effort"))
    (let ((type (and (symbolp model) (get model :reasoning-effort))))
      (unless type
        (user-error "Model %s does not support reasoning effort"
                    (gptel--model-name model)))
      (unless (cl-typep effort type)
        (user-error "Reasoning effort %S is unsupported by model %s"
                    effort (gptel--model-name model)))))
  effort)

(defun mevedel-model-resolve-workload
    (workload &optional explicit-selector explicit-effort)
  "Resolve WORKLOAD with optional explicit model and effort overrides.

Resolution starts from the current session backend/model/effort, then applies
the workload's tier, exact provider and effort, followed by explicit overrides."
  (let* ((name (and workload
                    (if (symbolp workload) workload (intern workload))))
         (spec (and name (alist-get name mevedel-model-workloads)))
         (tier (plist-get spec :tier))
         (provider-spec (plist-get spec :provider)))
    (when (and tier provider-spec)
      (user-error "Workload %s cannot select both tier and provider" name))
    (let* ((policy (if tier
                       (mevedel-model-resolve-tier tier)
                     (list :backend gptel-backend
                           :model gptel-model
                           :effort (and (boundp 'gptel-reasoning-effort)
                                        gptel-reasoning-effort))))
           (provider (and provider-spec
                          (mevedel-model-resolve-provider provider-spec)))
           (explicit (and explicit-selector
                          (mevedel-model-resolve-selector explicit-selector))))
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
      (mevedel-model-validate-effort
       (plist-get policy :model) (plist-get policy :effort))
      policy)))

(defun mevedel-model-current-label (&optional buffer)
  "Return BUFFER's current model label, or \"none\"."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((not (bound-and-true-p gptel-model)) "none")
     ((fboundp 'gptel--model-name) (gptel--model-name gptel-model))
     (t (format "%s" gptel-model)))))

(defun mevedel-model-current-provider-label (&optional buffer)
  "Return BUFFER's current backend:model label, or \"none\"."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((and (bound-and-true-p gptel-backend)
           (bound-and-true-p gptel-model))
      (format "%s:%s"
              (gptel-backend-name gptel-backend)
              (gptel--model-name gptel-model)))
     ((bound-and-true-p gptel-model)
      (mevedel-model-current-label))
     (t "none"))))


;;
;;; Tool descriptions and FSM application

(defun mevedel-model-agent-tool-description ()
  "Return the Agent tool model-argument description."
  (concat
   "Optional model tier for this agent invocation. Available tiers: "
   (mapconcat (lambda (entry) (symbol-name (car entry)))
              mevedel-model-tiers ", ")
   ". Omit this argument to use the agent workload declared by the current "
   "session preset."))

(defun mevedel-model-apply-provider-to-info (info provider)
  "Return INFO with PROVIDER applied to backend/model slots.

Also patches a realized plist request payload's :model field when the
backend is unchanged.  Realized gptel payloads are backend-specific;
cross-backend switches must happen before gptel builds `info' :data."
  (if (not provider)
      info
    (let ((backend (plist-get provider :backend))
          (model (plist-get provider :model))
          (current-backend (plist-get info :backend))
          (data (plist-get info :data)))
      (when (and current-backend
                 (listp data)
                 (not (eq backend current-backend)))
        (user-error
         "Cannot switch skill model backend after request data is realized: %s -> %s"
         (gptel-backend-name current-backend)
         (gptel-backend-name backend)))
      (setq info (plist-put info :backend backend))
      (setq info (plist-put info :model model))
      (when (listp data)
        (plist-put data :model (gptel--model-name model)))
      info)))

(defun mevedel-model-apply-policy-to-info (info policy)
  "Return INFO with resolved model POLICY recorded and applied."
  (setq info (mevedel-model-apply-provider-to-info info policy))
  (plist-put info :reasoning-effort (plist-get policy :effort)))

(provide 'mevedel-models)
;;; mevedel-models.el ends here
