;;; mevedel-models.el -- Model tier/provider resolution -*- lexical-binding: t -*-

;;; Commentary:

;; User-facing model selection helpers.  mevedel exposes a small tier
;; vocabulary (`fast', `balanced', `strong') while gptel dispatch needs
;; an exact backend/model pair.  This module parses provider specs,
;; resolves tiers, and applies resolved providers to gptel FSM info.

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


;;
;;; Customization

(defconst mevedel-model-tier-symbols '(fast balanced strong)
  "Valid mevedel model tier symbols.")

(defcustom mevedel-model-tiers
  '((fast . nil)
    (balanced . nil)
    (strong . nil))
  "Alist mapping model tiers to concrete gptel providers.

Keys are `fast', `balanced', and `strong'.  Values are one of:

- nil: inherit the current backend/model, subject to tier fallback.
- \"BACKEND:MODEL\": the same concrete provider notation shown by
  gptel's transient model selector.
- (BACKEND-NAME . MODEL): structured form where BACKEND-NAME is a
  string or symbol and MODEL is a string or symbol.

Bare model symbols are intentionally not accepted because gptel
models are scoped to backends."
  :group 'mevedel
  :type '(alist :key-type (choice (const fast)
                                  (const balanced)
                                  (const strong))
                :value-type (choice (const :tag "Inherit" nil)
                                    string
                                    (cons (choice string symbol)
                                          (choice string symbol)))))

(defcustom mevedel-model-workload-tiers
  '((explorer . fast)
    (coordinator . strong)
    (verifier . balanced)
    (reviewer . strong)
    (guardian . fast)
    (compaction . balanced))
  "Alist mapping model workloads to their default tier.

Keys are workload symbols.  Built-in workloads include sub-agent names
such as `explorer' and `verifier', plus helper requests such as
`guardian' and `compaction'.  Values are `fast', `balanced', or
`strong'.  An Agent tool call with an explicit model tier overrides the
sub-agent default for that invocation."
  :group 'mevedel
  :type '(alist :key-type symbol
                :value-type (choice (const fast)
                                    (const balanced)
                                    (const strong))))


;;
;;; Provider parsing

(defun mevedel-model-normalize-tier (value)
  "Return VALUE as a tier symbol, or nil when VALUE is not a tier."
  (let ((sym (if (stringp value) (intern-soft value) value)))
    (and (memq sym mevedel-model-tier-symbols) sym)))

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
  "Resolve TIER through `mevedel-model-tiers'.

Unconfigured `fast' and `strong' fall back to configured `balanced'.
Unconfigured `balanced' inherits the current backend/model and returns
nil.  When NOERROR is non-nil, invalid config returns nil and emits a
warning instead of signaling."
  (let* ((sym (mevedel-model-normalize-tier tier))
         (configured (and sym (alist-get sym mevedel-model-tiers)))
         (fallback (and (not (eq sym 'balanced))
                        (alist-get 'balanced mevedel-model-tiers)))
         (spec (or configured fallback)))
    (cond
     ((not sym)
      (if noerror
          nil
        (user-error "Unknown model tier %S" tier)))
     ((null spec) nil)
     (t
      (condition-case err
          (mevedel-model-resolve-provider spec noerror)
        (error
         (if noerror
             (progn
               (display-warning
                'mevedel
                (format "Invalid model tier config for %S: %s"
                        sym (error-message-string err))
                :warning)
               nil)
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
     "Model must be a tier (fast, balanced, strong) or BACKEND:MODEL: %S"
     value))))

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

(defun mevedel-model-workload-default-selector (workload)
  "Return WORKLOAD's configured default tier selector, or nil."
  (when-let* ((tier (alist-get (if (symbolp workload)
                                   workload
                                 (intern workload))
                               mevedel-model-workload-tiers)))
    (mevedel-model-tier-selector tier)))

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
   "Optional model tier for this agent invocation. Allowed values: "
   "fast, balanced, strong. Prefer balanced by default. Only pick fast "
   "when the task is clearly mechanical; only pick strong when the task "
   "clearly needs deep reasoning. Picking strong for trivial work is "
   "wasteful; picking fast for hard work is unsafe.\n\n"
   "fast: quick, low-cost work. Good for well-defined lookups, simple "
   "edits, summarizing tool output, format/lint fixes, single-step "
   "transformations where the right answer is obvious. Avoid for "
   "multi-file reasoning, debugging, or design decisions.\n\n"
   "balanced: the default. Good for typical implementation work: "
   "writing or modifying functions, reviewing code with context, "
   "exploring an unfamiliar codebase, most refactors. Pick this when "
   "the task is clearly real work but not unusually subtle.\n\n"
   "strong: heaviest, slowest tier. Good for hard reasoning: "
   "architecture decisions, ambiguous specifications, debugging subtle "
   "invariants (concurrency, type systems, ordering bugs), long-horizon "
   "planning. Avoid for well-scoped tasks where balanced would suffice."))

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

(provide 'mevedel-models)
;;; mevedel-models.el ends here
