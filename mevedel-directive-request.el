;;; mevedel-directive-request.el -- Directive request transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the directive request lifecycle: prompt construction for
;; implement/discuss/retry/request-changes, request admission against the
;; live session, the gptel dispatch, and the terminal settlement that
;; records attempts, discussion turns, and planning turns on the durable
;; directive record.  Chat owns the session buffers these requests run in;
;; the directive family owns the records they settle into.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel)
  (require 'gptel-request)
  (require 'mevedel-presets))


;; `cl-seq'
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(declare-function gptel-markdown-cycle-block "ext:gptel" nil)
(defvar gptel--markdown-block-map)
(defvar gptel-backend)
(defvar gptel-display-buffer-action)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-send--handlers)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request")
(declare-function gptel-fsm-state "ext:gptel-request")
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel-request "ext:gptel-request")
(defvar gptel-prompt-prefix-alist)
(defvar gptel-prompt-transform-functions)
(defvar gptel-response-separator)
(defvar gptel-stream)

;; `mevedel'
(defvar mevedel-show-chat-buffer)

;; `mevedel-chat'
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))
(declare-function mevedel-directive-next-activity-sequence
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-recompute-state
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-remove-subdirective
                  "mevedel-directive" (directive subdirective))

;; `mevedel-directive-frame'
(declare-function mevedel-directive-frame-display
                  "mevedel-directive-frame"
                  (directive view-buffer &optional focus))

;; `mevedel-directive-plan'
(declare-function mevedel-directive-plan-start
                  "mevedel-directive-plan"
                  (directive action prompt-fn callback))

;; `mevedel-directive-source'
(declare-function mevedel--delete-instruction
                  "mevedel-directive-source"
                  (instruction &optional buffer))
(declare-function mevedel--detached-directive-p
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-record
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-text
                  "mevedel-directive-source" (directive))
(declare-function mevedel--reconcile-directive-sources
                  "mevedel-directive-source" (workspace))
(declare-function mevedel--remove-directive-presentation
                  "mevedel-directive-source" (directive &optional buffer))
(declare-function mevedel--set-directive-status
                  "mevedel-directive-source" (directive status))
(declare-function mevedel--submitted-subdirectives
                  "mevedel-directive-source" (directive))

;; `mevedel-instruction-registry'
(declare-function mevedel--find-directive-by-uuid
                  "mevedel-instruction-registry" (uuid))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-instruction-registry"
                  (uuid &optional workspace))

;; `mevedel-models'
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))
(declare-function mevedel-model-validate-effort
                  "mevedel-models" (model effort))

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))
(declare-function mevedel-overlay-ui-directive-action-label
                  "mevedel-overlay-ui" (action))

;; `mevedel-overlays'
(declare-function mevedel--directive-llm-prompt
                  "mevedel-overlays" (directive))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))

;; `mevedel-permission-mode'
(declare-function mevedel--implementation-permission-mode-apply
                  "mevedel-permission-mode" (mode))
(declare-function mevedel--implementation-permission-mode-restore
                  "mevedel-permission-mode" ())

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-append-implementation-input
                  "mevedel-plan-handoff" (prompt selection))
(declare-function mevedel-plan-handoff-validate-skill-bindings
                  "mevedel-plan-handoff" (prompt session))

;; `mevedel-presets'
(declare-function mevedel-preset-apply "mevedel-presets"
		  (name &optional buffer))
(defvar mevedel--directive-read-only-request-p)
(defvar mevedel-action-preset-alist)
(defvar mevedel-default-chat-preset)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(declare-function mevedel-session-artifacts-ensure-files
                  "mevedel-session-artifacts" (session buffer))
(declare-function
 mevedel-session-artifacts-install-gptel-save-state-advice
 "mevedel-session-artifacts" nil)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-resume-id
                  "mevedel-session-persistence" (workspace session-id))

;; `mevedel-skills-input'
(declare-function mevedel-skills-input-prepare-user-input
                  "mevedel-skills-input" (text session))

;; `mevedel-structs'
(declare-function mevedel-directive-attempt--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-attempt-capture
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-captured-at
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-plan
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-discussion-turn-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-message
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-strip
                  "mevedel-tool-render-data" (string &optional expected-tool-use-id))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))

;; `mevedel-turn'
(declare-function mevedel-request-begin
                  "mevedel-turn" (session &optional directive-uuid))
(declare-function mevedel-request-end
                  "mevedel-turn" (&optional abort-plan-approval))

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
		  "mevedel-utilities" (start end))

;; `mevedel-view'
(defvar mevedel--agent-invocation)
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view--begin-external-turn
		  "mevedel-view-composer"
		  (display-text data-turn-start &optional kind
				hook-context no-spinner))

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace"
		  (&optional buffer))

;; `org-src'
(declare-function org-escape-code-in-string "ext:org-src" (s))


;;
;;; Prompt generation

(defun mevedel--implement-directive-prompt (content)
  "Generate an implementation prompt for CONTENT in the current buffer."
  (format
   "## TASK: Implement the following request.

### INSTRUCTIONS:

1. Read and understand the implementation request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools as needed
5. Create working, complete code that fulfills the request

### IMPLEMENTATION REQUEST:

%s"
   content))

(defun mevedel--request-changes-prompt
    (content directive feedback &optional new-context-p)
  "Build focused Request changes context for DIRECTIVE.
CONTENT contains the current request and freshly resolved references.  FEEDBACK
may be empty only when NEW-CONTEXT-P says new subdirectives supply the changes."
  (let* ((attempt (car (last (mevedel-directive-attempts directive))))
         (feedback (string-trim (or feedback ""))))
    (unless (memq 'request-changes (mevedel-directive-actions directive))
      (user-error "Request changes requires a successful current attempt"))
    (when (and (string-empty-p feedback) (not new-context-p))
      (user-error "Request changes requires feedback or new subdirectives"))
    (format
     "## TASK: Implement requested changes to the directive.

Current repository state is authoritative. Inspect it before editing; the
preceding patch below is historical evidence, not a patch to reapply.

### CURRENT REQUEST AND FRESH REFERENCES:

%s

### REQUESTED CHANGES:

%s

### IMMEDIATELY PRECEDING ATTEMPT (historical)

Captured at: %s
Capture completeness: %s

Answer:
%s

Historical observed patch:
%s%s"
     content
     (if (string-empty-p feedback)
         "Use the newly supplied directive context as the requested changes."
       feedback)
     (mevedel-directive-attempt-captured-at attempt)
     (upcase (symbol-name (mevedel-directive-attempt-capture attempt)))
     (mevedel-directive-attempt-result attempt)
     (mevedel-directive-attempt-patch attempt)
     (if-let* ((plan (mevedel-directive-attempt-plan attempt)))
         (format "\n\nAccepted plan from that attempt:\n%s" plan)
       ""))))

(defun mevedel--retry-directive-prompt (content directive guidance)
  "Build focused Retry context for DIRECTIVE from CONTENT and GUIDANCE."
  (let* ((attempt (car (last (mevedel-directive-attempts directive))))
         (guidance (string-trim (or guidance ""))))
    (unless (memq 'retry (mevedel-directive-actions directive))
      (user-error "Retry requires a failed or aborted current attempt"))
    (format
     "## TASK: Retry the directive implementation.

Current repository state is authoritative. Inspect it before editing; the
preceding partial patch is diagnostic evidence, not a patch to reapply.

### CURRENT REQUEST AND FRESH REFERENCES:

%s%s

### IMMEDIATELY PRECEDING FAILURE:

Captured at: %s
Capture completeness: %s

%s

Observed partial changes:
%s%s"
     content
     (if (string-empty-p guidance)
         ""
       (format "\n\n### OPTIONAL GUIDANCE:\n\n%s" guidance))
     (mevedel-directive-attempt-captured-at attempt)
     (upcase (symbol-name (mevedel-directive-attempt-capture attempt)))
     (mevedel-directive-attempt-result attempt)
     (mevedel-directive-attempt-patch attempt)
     (if-let* ((plan (mevedel-directive-attempt-plan attempt)))
         (format "\n\nAccepted plan from that attempt:\n%s" plan)
       ""))))

(defun mevedel--directive-bound-session-buffer (record workspace)
  "Return RECORD's live bound execution session buffer in WORKSPACE, or nil."
  (when-let* ((bound-id (mevedel-directive-session-id record)))
    (cl-loop
     for (_ . candidate) in (mevedel--workspace-sessions workspace)
     when (and (buffer-live-p candidate)
               (equal bound-id
                      (with-current-buffer candidate
                        (and (bound-and-true-p mevedel--session)
                             (mevedel-session-session-id mevedel--session)))))
     return candidate)))

(defun mevedel--attach-directive-skills (prompt record chat-buffer)
  "Append RECORD's selected skills to PROMPT and validate in CHAT-BUFFER.
Each skill's current source is reloaded at dispatch; a missing,
disabled, or malformed selection signals before any request starts."
  (if-let* ((skills (mevedel-directive-skills record)))
      (with-current-buffer chat-buffer
        (require 'mevedel-plan-handoff)
        (require 'mevedel-skills-input)
        (let ((result (mevedel-plan-handoff-append-implementation-input
                       prompt (list :skills skills))))
          (setq result (mevedel-skills-input-prepare-user-input
                        result mevedel--session))
          (mevedel-plan-handoff-validate-skill-bindings
           result mevedel--session)
          result))
    prompt))

(defun mevedel--directive-discussion-transcript (directive)
  "Return DIRECTIVE's current-request local discussion as plain text."
  (require 'mevedel-tool-render-data)
  (mapconcat
   (lambda (turn)
     (format "User: %s\nAssistant%s: %s"
             (mevedel-directive-discussion-turn-message turn)
             (if (eq (mevedel-directive-discussion-turn-outcome turn)
                     'success)
                 ""
               (format " (%s)"
                       (mevedel-directive-discussion-turn-outcome turn)))
             (string-trim-right
              (mevedel-tool-render-data-strip
               (mevedel-directive-discussion-turn-result turn)))))
   (cl-remove-if-not
    (lambda (turn)
      (equal (mevedel-directive-request directive)
             (mevedel-directive-discussion-turn-directive-request turn)))
    (mevedel-directive-discussion directive))
   "\n\n"))

(defun mevedel--discuss-directive-prompt
    (content &optional directive message attempt-index)
  "Generate a read-only discussion prompt from CONTENT.
When DIRECTIVE and MESSAGE are non-nil, include the complete directive-local
discussion.  ATTEMPT-INDEX attaches that implementation result."
  (let* ((discussion
          (and directive
               (mevedel--directive-discussion-transcript directive)))
         (attempt
          (and attempt-index
               (nth (1- attempt-index)
                    (mevedel-directive-attempts directive)))))
    (when (and attempt-index (not attempt))
      (user-error "Directive has no implementation attempt %d"
                  attempt-index))
    (format
     "## TASK: Answer the following request.

### INSTRUCTIONS:

1. Read and understand the request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools to access files as needed

### REQUEST:

%s%s%s"
     content
     (if (and discussion (not (string-empty-p discussion)))
         (format "\n\n### LOCAL DISCUSSION:\n\n%s" discussion)
       "")
     (concat
      (if attempt
          (format
           "\n\n### SELECTED IMPLEMENTATION ATTEMPT %d:\n\nRequest:\n%s\n\nResult:\n%s\n\nObserved patch:\n%s"
           attempt-index
           (mevedel-directive-attempt-request attempt)
           (mevedel-directive-attempt-result attempt)
           (mevedel-directive-attempt-patch attempt))
        "")
      (if message
          (format "\n\n### QUESTION:\n\n%s" message)
        "")))))

(defun mevedel--implement-discussion-prompt (content directive)
  "Generate an implementation prompt from CONTENT and DIRECTIVE discussion."
  (let ((discussion (mevedel--directive-discussion-transcript directive)))
    (unless (memq 'implement-this (mevedel-directive-actions directive))
      (user-error "Implement this requires a current discussion"))
    (when (string-empty-p discussion)
      (user-error "Directive has no discussion to implement"))
    (concat
     (mevedel--implement-directive-prompt content)
     "\n\n### DISCUSSION FEEDBACK:\n\n"
     discussion)))

(defun mevedel--directive-implementation-prompt
    (content directive &optional feedback)
  "Build DIRECTIVE's complete next implementation prompt from CONTENT.
FEEDBACK supplies requested changes or optional retry guidance."
  (let ((actions (mevedel-directive-actions directive)))
    (cond
     ((memq 'implement-this actions)
     (mevedel--implement-discussion-prompt content directive))
     ((memq 'request-changes actions)
      (mevedel--request-changes-prompt
       content directive feedback (mevedel-directive-subdirectives directive)))
     ((memq 'retry actions)
      (mevedel--retry-directive-prompt content directive feedback))
     ((memq 'implement actions)
      (mevedel--implement-directive-prompt content))
     (t (user-error "Directive action is already in progress")))))


;;
;;; Directive processing

(defvar-local mevedel--current-directive-uuid nil
  "UUID of the directive currently being processed.")

(defun mevedel--directive-display-text (action directive-text)
  "Return the human-facing transcript text for ACTION and DIRECTIVE-TEXT."
  (require 'mevedel-overlay-ui)
  (let ((label (mevedel-overlay-ui-directive-action-label action)))
    (if (string-empty-p (string-trim directive-text))
        label
      (format "%s: %s" label directive-text))))

(defun mevedel--insert-directive-turn
    (directive-id turn directive-text prompt action)
  "Insert a directive turn into the current chat data buffer.

DIRECTIVE-ID and TURN identify the durable directive boundary.
DIRECTIVE-TEXT is the short overlay text shown in the transcript.
PROMPT is the full LLM-facing prompt, inserted in a `:PROMPT:' drawer
for inspection.  Request-time projection excludes the complete turn
from ordinary conversation context.  ACTION is the directive action
symbol.  Return a marker positioned where the assistant response should
be inserted."
  (require 'mevedel-utilities)
  (require 'mevedel-transcript-audit)
  (let* ((summary directive-text)
         (action-str (symbol-name action))
         (is-org-mode (derived-mode-p 'org-mode))
         (header-prefix (if is-org-mode "" (format "`%s` " action-str)))
         (header-postfix (if is-org-mode (format " :%s:" action-str) ""))
         (truncated-summary
          (let* ((lines (split-string summary "\n" t "[[:space:]]*"))
                 (first-line (or (car lines) ""))
                 (prefix (or (alist-get major-mode gptel-prompt-prefix-alist) ""))
                 (used-length (+ (length prefix)
                                 (length header-prefix)
                                 (length header-postfix)))
                 (available-length (max 10 (- (or fill-column 70)
                                              used-length))))
            (truncate-string-to-width first-line available-length nil nil "...")))
         (full-prompt-str
          (if is-org-mode
              (progn
                (require 'org-src)
                (concat ":PROMPT:\n"
                        (org-escape-code-in-string prompt)
                        "\n:END:\n"))
            (concat "``` prompt\n" prompt "\n```\n"))))
    (goto-char (point-max))
    (insert
     (mevedel--format-hook-audit-record
      (list :type 'directive-turn-boundary
            :edge 'start
            :directive-id directive-id
            :action action
            :turn turn)))
    (let ((user-turn-start (point)))
      (unless (bobp)
        (insert gptel-response-separator))
      (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
        (let ((prefix-length (length prefix)))
          (unless (and (>= (point) (+ (point-min) prefix-length))
                       (string=
                        (buffer-substring-no-properties
                         (- (point) prefix-length) (point))
                        prefix))
            (unless (bolp)
              (insert "\n"))
            (insert prefix))))
      (insert (format "%s%s%s\n"
                      header-prefix truncated-summary header-postfix))
      (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
    (let ((cur-pt (point)))
      (insert (if (derived-mode-p 'markdown-mode)
                  (propertize full-prompt-str
                              'keymap gptel--markdown-block-map)
                full-prompt-str))
      (ignore-errors
        (if (derived-mode-p 'org-mode)
            (save-excursion
              (search-backward ":PROMPT:" cur-pt t)
              (when (looking-at "^:PROMPT:")
                (org-cycle)))
          (save-excursion
            (when (re-search-backward "^```" cur-pt t)
              (gptel-markdown-cycle-block))))))
    (copy-marker (point) nil)))

(defun mevedel--insert-directive-turn-end
    (directive-id turn action outcome activity-kind sequence)
  "Close a directive transcript turn at point.
DIRECTIVE-ID, TURN, ACTION, OUTCOME, ACTIVITY-KIND, and SEQUENCE link the
canonical transcript to its immutable workspace activity record."
  (require 'mevedel-transcript-audit)
  (insert
   (mevedel--format-hook-audit-record
    (list :type 'directive-turn-boundary
          :edge 'end
          :directive-id directive-id
          :action action
          :turn turn
          :outcome outcome
          :activity-kind activity-kind
          :sequence sequence))))

(defun mevedel--directive-save-buffer-p ()
  "Return non-nil when the current buffer should be saved before a directive.

Directive processing should offer to save normal modified file buffers so
subsequent file tools see the current source text on disk.  It should not
prompt for mevedel data or agent transcript buffers; those are persisted by
session/transcript autosave and may be modified while a request is still
settling."
  (and (buffer-file-name)
       (buffer-modified-p)
       (not (bound-and-true-p mevedel--session))
       (not (bound-and-true-p mevedel--agent-invocation))))

(defun mevedel--directive-model-policy (directive)
  "Return DIRECTIVE's resolved request-local model policy, or nil."
  (when-let* ((provider
               (overlay-get directive 'mevedel-directive-model-provider)))
    (require 'mevedel-models)
    (let* ((policy (mevedel-model-resolve-provider provider))
           (effort
            (overlay-get directive 'mevedel-directive-reasoning-effort)))
      (mevedel-model-validate-effort (plist-get policy :model) effort)
      (plist-put policy :effort effort))))

(defun mevedel--directive-session-buffer (directive workspace)
  "Return `(BUFFER . REBIND-P)' for DIRECTIVE in WORKSPACE."
  (let ((session-id (mevedel-directive-session-id directive)))
    (if (not session-id)
        (cons (mevedel--chat-buffer "main" t workspace) nil)
      (or
       (when-let* ((buffer (mevedel--directive-bound-session-buffer
                            directive workspace)))
         (cons buffer nil))
       (progn
         (require 'mevedel-session-persistence)
         (when-let* ((buffer
                      (mevedel-session-persistence-resume-id
                       workspace session-id)))
           (cons buffer nil)))
       (if (yes-or-no-p
            (format "Directive session %s is unavailable; rebind future activity to the current workspace session? "
                    session-id))
           (cons (mevedel--chat-buffer "main" t workspace) t)
         (user-error "Directive remains bound to unavailable session: %s"
                     session-id))))))

(defun mevedel--record-directive-terminal-activity
    (record action directive-text prompt result outcome checkpoint info
            options submitted-subdirectives)
  "Record one terminal directive ACTION and return its activity kind/sequence."
  (let ((sequence
         (and (memq action '(discuss plan implement request-changes retry))
              (mevedel-directive-next-activity-sequence record))))
    (pcase action
      ('plan
       (setf (mevedel-directive-planning record)
             (append
              (mevedel-directive-planning record)
               (list
                (list :sequence sequence
                      :action (plist-get options :planned-action)
                      :directive-request directive-text
                      :message (plist-get options :message)
                      :implementation-prompt
                      (plist-get (mevedel-directive-plan record)
                                 :implementation-prompt)
                      :proposal nil
                      :request prompt :result result :outcome outcome
                      :checkpoint checkpoint))))
       (mevedel-directive-recompute-state record)
       (cons 'planning sequence))
      ('discuss
       (setf (mevedel-directive-discussion record)
             (append
              (mevedel-directive-discussion record)
              (list
               (mevedel-directive-discussion-turn--create
                :sequence sequence :directive-request directive-text
                :message (plist-get options :message) :request prompt
                :result result :outcome outcome
                :attempt-index (plist-get options :attempt-index)
                :checkpoint checkpoint))))
       (mevedel-directive-recompute-state record)
       (cons 'discussion sequence))
      ((or 'implement 'request-changes 'retry)
       (setf (mevedel-directive-attempts record)
             (append
              (mevedel-directive-attempts record)
              (list
               (mevedel-directive-attempt--create
                :sequence sequence :action action
                :directive-request directive-text
                :request prompt :result result :outcome outcome
                :patch (or (plist-get info :mevedel-directive-patch) "")
                :capture (or (plist-get info :mevedel-directive-capture)
                             'incomplete)
                :covered-files
                (plist-get info :mevedel-directive-covered-files)
                :gaps (plist-get info :mevedel-directive-gaps)
                :untracked-effects
                (plist-get info :mevedel-directive-untracked-effects)
                :captured-at (format-time-string "%FT%T%z")
                :checkpoint checkpoint
                :plan (copy-tree (plist-get options :plan))
                :plan-context
                (and (plist-get options :plan)
                     (list
                      :request directive-text
                      :subdirectives
                      (mapcar
                       (lambda (subdirective)
                         (cons (mevedel-subdirective-id subdirective)
                               (mevedel-subdirective-request subdirective)))
                       submitted-subdirectives)))
                :plan-selection
                (copy-tree (plist-get options :plan-selection))
                :consumed-subdirectives
                (and (eq outcome 'success) submitted-subdirectives)))))
       (mevedel-directive-recompute-state record)
       (cons 'attempt sequence))
      (_ (error "Unknown directive action: %S" action)))))

(defun mevedel--consume-directive-subdirectives
    (record submitted-subdirectives workspace live-directive)
  "Consume SUBMITTED-SUBDIRECTIVES after LIVE-DIRECTIVE succeeds."
  (with-current-buffer (overlay-buffer live-directive)
    (dolist (submitted submitted-subdirectives)
      (let ((id (mevedel-subdirective-id submitted)))
        (if-let* ((child-directive
                   (mevedel--instruction-with-uuid id workspace)))
            (mevedel--delete-instruction child-directive)
          (when-let* ((current
                       (cl-find id (mevedel-directive-subdirectives record)
                                :key #'mevedel-subdirective-id :test #'equal)))
            (mevedel-directive-remove-subdirective record current)))))
    (save-excursion
      (goto-char (overlay-start live-directive))
      (unless (mevedel--detached-directive-p live-directive)
        (overlay-put live-directive 'evaporate t)))))

(defun mevedel--settle-directive-presentation
    (live-directive record workspace implementation-p submitted-subdirectives
                    err)
  "Settle LIVE-DIRECTIVE presentation after its terminal request."
  (when-let* ((live-directive live-directive)
              (directive-buffer (overlay-buffer live-directive)))
    (mevedel--set-directive-status
     live-directive (mevedel-directive-state record))
    (when (and err implementation-p)
      (overlay-put live-directive 'mevedel-directive-fail-reason
                   (if (eq err 'abort) "aborted" (format "%s" err))))
    (when (and implementation-p (not err))
      (mevedel--consume-directive-subdirectives
       record submitted-subdirectives workspace live-directive))
    (mevedel--update-instruction-overlay live-directive t)
    (with-current-buffer directive-buffer
      (pulse-momentary-highlight-region
       (overlay-start live-directive) (overlay-end live-directive)))))

(defun mevedel--directive-request-error (exit-code fsm)
  "Return the terminal error for EXIT-CODE and FSM, or nil on success."
  (cond
   (exit-code)
   ((eq (gptel-fsm-state fsm) 'ERRS)
    (let* ((info (gptel-fsm-info fsm))
           (error (plist-get info :error))
           (message (plist-get error :message)))
      (or message
          (format "%s: %s"
                  (plist-get error :type)
                  (plist-get info :status)))))))

(defun mevedel--send-directive-request
    (prompt chat-buffer response-start preset model-policy callback)
  "Send a directive PROMPT and invoke CALLBACK with its terminal error and FSM."
  (mevedel-with-preset preset
		       (let* ((request-callback
			       (lambda (exit-code fsm)
				 (funcall callback
					  (mevedel--directive-request-error exit-code fsm)
					  fsm)))
			      (fsm
			       (gptel-request
				prompt
				:buffer chat-buffer
				:position response-start
				:stream gptel-stream
				:transforms
				(append
				 gptel-prompt-transform-functions
				 (and model-policy
				      (list
				       (lambda (_fsm)
					 (setq-local
					  gptel-backend (plist-get model-policy :backend)
					  gptel-model (plist-get model-policy :model)
					  gptel-reasoning-effort
					  (plist-get model-policy :effort))))))
				:fsm (gptel-make-fsm :handlers gptel-send--handlers))))
			 (setf (gptel-fsm-info fsm)
			       (plist-put (gptel-fsm-info fsm)
					  :mevedel-request-callback request-callback))
			 fsm)))

(defun mevedel--process-directive
    (directive preset prompt-fn callback &optional options)
  "Process DIRECTIVE using PRESET and PROMPT-FN, calling CALLBACK when complete.

DIRECTIVE is the instruction overlay to process.
PRESET is the gptel preset to use (mevedel-implement or
mevedel-discuss).
PROMPT-FN is a function that generates the prompt from the directive
content.
CALLBACK is called with (err fsm) when processing completes.

Updates directive status and overlay, handles success/failure states.
OPTIONS carries local discussion metadata for read-only discussion turns."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (require 'mevedel-turn)
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((transient-buffer
         (and (overlay-get directive 'mevedel-transient-source-missing)
              (overlay-buffer directive)))
        cleanup-chat-buffer cleanup-record cleanup-prior-state
        cleanup-request-context-set-p cleanup-request-reserved-p
        cleanup-turn-start cleanup-mode-applied-p cleanup-planning-session)
    (condition-case err
        (let* ((model-policy (or (plist-get options :model-policy)
                                 (mevedel--directive-model-policy directive)))
               ;; Get chat buffer for the directive's buffer workspace
               (workspace (with-current-buffer (overlay-buffer directive)
			    (mevedel-workspace)))
               (record (setq cleanup-record
			     (mevedel--directive-record directive)))
               (_prior-state
		(setq cleanup-prior-state (mevedel-directive-state record)))
               (bound-session-id (mevedel-directive-session-id record))
               (session-choice
		(mevedel--directive-session-buffer record workspace))
               (chat-buffer (setq cleanup-chat-buffer (car session-choice)))
               (rebind-p (cdr session-choice))
               (directive-uuid (overlay-get directive 'mevedel-uuid))
               (directive-text (mevedel--directive-text directive))
               (content (mevedel--directive-llm-prompt directive))
               (action (overlay-get directive 'mevedel-directive-action))
               (planning-p (eq action 'plan))
               (discussion-p (eq action 'discuss))
               (implementation-p
		(memq action '(implement request-changes retry)))
               ;; Directive-selected skills attach to direct
               ;; implementation prompts; an accepted plan's card
               ;; selection (:plan-selection) already carries its own.
               (prompt
                (let ((built (funcall prompt-fn content)))
                  (if (and implementation-p
                           (not (plist-get options :plan-selection)))
                      (mevedel--attach-directive-skills
                       built record chat-buffer)
                    built)))
               (submitted-subdirectives
		(and implementation-p
		     (mevedel--submitted-subdirectives directive)))
               execution-session-id
               reserved-turn
               response-start
               settled-p
               (callback-fn
		(lambda (err fsm)
		  (unless settled-p
		    (setq settled-p t)
		    (let* ((info (gptel-fsm-info fsm))
			   (outcome (cond ((eq err 'abort) 'aborted)
					  (err 'error)
					  (t 'success)))
			   (result
			    (if err
				(if (eq err 'abort)
				    "Request aborted"
				  (format "%s" err))
                              (with-current-buffer chat-buffer
				(buffer-substring-no-properties
				 response-start (point-max)))))
			   (turn reserved-turn)
			   (checkpoint
			    (list :session-id execution-session-id :turn turn))
			   (live-directive
			    (or (and (overlay-buffer directive) directive)
				(mevedel--find-directive-by-uuid directive-uuid)))
			   (activity
			    (mevedel--record-directive-terminal-activity
			     record action directive-text prompt result outcome
			     checkpoint info options submitted-subdirectives)))
                      (with-current-buffer chat-buffer
			(let ((inhibit-read-only t))
			  (goto-char (point-max))
			  (mevedel--insert-directive-turn-end
			   directive-uuid turn action outcome
			   (car activity) (cdr activity)))
			(setq mevedel--current-directive-uuid nil
                              mevedel--directive-read-only-request-p nil))
                      (mevedel--settle-directive-presentation
                       live-directive record workspace implementation-p
                       submitted-subdirectives err)
                      (mevedel--reconcile-directive-sources workspace)
                      (unwind-protect
			  (when callback
			    (funcall callback err fsm))
			(when (buffer-live-p transient-buffer)
			  (when (overlay-buffer directive)
			    (mevedel--remove-directive-presentation directive))
			  (kill-buffer transient-buffer))))))))

          (with-current-buffer chat-buffer
            (when mevedel--current-request
              (user-error "A request is already active -- wait or abort first"))
            (when planning-p
              (when (mevedel-session-plan-mode mevedel--session)
                (user-error
                 "Leave ordinary Plan mode before planning a directive"))
              (when-let* ((active
                           (mevedel-session-directive-planning
                            mevedel--session)))
                (unless (and (plist-get options :plan-continuation)
                             (equal (plist-get active :directive-id)
                                    directive-uuid))
                  (user-error
                   "A directive workflow already occupies this session")))
              (setf (mevedel-session-directive-planning mevedel--session)
                    (list :directive-id directive-uuid
                          :action (plist-get options :planned-action)
                          :phase 'planning))
              (setq cleanup-planning-session mevedel--session))
            (when-let* ((mode (and implementation-p
                                    (plist-get options :permission-mode))))
              (mevedel--implementation-permission-mode-apply mode)
              (setq cleanup-mode-applied-p t))
            (setq cleanup-request-context-set-p t)
	    (setq mevedel--current-directive-uuid
		  (overlay-get directive 'mevedel-uuid)
                  mevedel--directive-read-only-request-p
                  (or discussion-p planning-p))
            (mevedel-session-artifacts-ensure-files mevedel--session chat-buffer)
	    (setq execution-session-id
		  (mevedel-session-session-id mevedel--session)))

	  (save-some-buffers nil #'mevedel--directive-save-buffer-p)

          (when (or discussion-p planning-p implementation-p)
            (mevedel--set-directive-status
             directive (cond (discussion-p 'discussing)
                             (planning-p 'planning)
                             (t 'implementing)))
	    (mevedel--update-instruction-overlay directive t)
	    (pulse-momentary-highlight-region
	     (overlay-start directive) (overlay-end directive)))

	  ;; Display view buffer if configured (fall back to data buffer)
	  (let ((view (or (buffer-local-value 'mevedel--view-buffer chat-buffer)
			  chat-buffer)))
	    (pcase mevedel-show-chat-buffer
	      ;; No focus argument: a request the user just started must not
	      ;; move point into the frame.
	      ('frame
	       (require 'mevedel-directive-frame)
	       (mevedel-directive-frame-display directive view))
	      ('window
	       (display-buffer view gptel-display-buffer-action))))

	(with-current-buffer chat-buffer
          (mevedel-session-artifacts-assert-new-mutation-authority
	   mevedel--session)
	    (mevedel-preset-apply
	     (alist-get mevedel-default-chat-preset mevedel-action-preset-alist))
	    (mevedel-request-begin mevedel--session directive-uuid)
	    (setq cleanup-request-reserved-p t)
	    (setq reserved-turn (mevedel-request-turn mevedel--current-request))
	    (setq cleanup-turn-start (copy-marker (point-max) nil))
	    (setq response-start
		  (mevedel--insert-directive-turn
		   directive-uuid reserved-turn
		   directive-text prompt
		   (overlay-get directive 'mevedel-directive-action)))
	    (when-let* ((view-buf mevedel--view-buffer)
			(_ (buffer-live-p view-buf)))
              (with-current-buffer view-buf
		(mevedel-view--begin-external-turn
		 (mevedel--directive-display-text
		  (overlay-get directive 'mevedel-directive-action)
		  directive-text)
		 response-start
		 'directive)))

	    (let ((fsm
		   (mevedel--send-directive-request
		    prompt chat-buffer response-start preset model-policy callback-fn)))
              (when (or (not bound-session-id) rebind-p)
		(setf (mevedel-directive-session-id record)
                      execution-session-id))
              fsm)))
      (t
       ;; Restore authoritative directive state before a view redraw can
       ;; replace its source presentation.
       (when cleanup-record
         (setf (mevedel-directive-state cleanup-record)
               cleanup-prior-state))
       (when (overlay-buffer directive)
         (mevedel--set-directive-status directive cleanup-prior-state)
         (mevedel--update-instruction-overlay directive t))
       (when (buffer-live-p cleanup-chat-buffer)
         (with-current-buffer cleanup-chat-buffer
           (when (and (markerp cleanup-turn-start)
                      (marker-position cleanup-turn-start))
             (let ((inhibit-read-only t))
               (delete-region cleanup-turn-start (point-max))))
           (when cleanup-request-reserved-p
             (condition-case cleanup-error
                 (mevedel-request-end)
               (error
                (setq mevedel--current-request nil)
                (display-warning
                 'mevedel
                 (format "Directive request cleanup failed: %s"
                         (error-message-string cleanup-error))
                 :warning))))
           (when cleanup-request-context-set-p
             (setq mevedel--current-directive-uuid nil
                   mevedel--directive-read-only-request-p nil)
             (when-let* ((view-buffer mevedel--view-buffer)
                         ((buffer-live-p view-buffer)))
               (with-current-buffer view-buffer
                 (mevedel-view--full-rerender))))))
       (when (and cleanup-mode-applied-p
                  (buffer-live-p cleanup-chat-buffer))
         (with-current-buffer cleanup-chat-buffer
           (mevedel--implementation-permission-mode-restore)))
       (when cleanup-planning-session
         (setf (mevedel-session-directive-planning cleanup-planning-session)
               nil))
       (when (markerp cleanup-turn-start)
         (set-marker cleanup-turn-start nil))
       (when (buffer-live-p transient-buffer)
         (when (overlay-buffer directive)
           (mevedel--remove-directive-presentation directive))
         (kill-buffer transient-buffer))
       (signal (car err) (cdr err))))))

(defun mevedel--start-directive-discussion (directive &optional callback)
  "Submit DIRECTIVE itself as its initial read-only discussion turn.
CALLBACK receives the ordinary directive terminal arguments."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((record (mevedel--directive-record directive)))
    (unless (memq 'discuss (mevedel-directive-actions record))
      (user-error "Initial discussion requires a Ready directive"))
    (overlay-put directive 'mevedel-directive-action 'discuss)
    (mevedel--process-directive
     directive (alist-get 'discuss mevedel-action-preset-alist)
     #'mevedel--discuss-directive-prompt
     callback
     (list :message (mevedel-directive-request record)))))

(defun mevedel--discuss-directive-turn
    (directive message &optional attempt-index callback)
  "Submit MESSAGE as DIRECTIVE's next read-only discussion turn.
ATTEMPT-INDEX attaches one implementation result.  CALLBACK receives the
ordinary directive terminal arguments."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (unless (and (stringp message) (not (string-empty-p (string-trim message))))
    (user-error "Discussion message must not be empty"))
  (let ((record (mevedel--directive-record directive)))
    (overlay-put directive 'mevedel-directive-action 'discuss)
    (mevedel--process-directive
     directive (alist-get 'discuss mevedel-action-preset-alist)
     (lambda (content)
       (mevedel--discuss-directive-prompt
        content record message attempt-index))
     callback
     (list :message message :attempt-index attempt-index))))

(defun mevedel--dispatch-directive-implementation
    (directive record action prompt-fn callback)
  "Run RECORD's ACTION implementation for DIRECTIVE, planning first when enabled.
PROMPT-FN builds the implementation prompt from resolved content and
CALLBACK receives the ordinary terminal (err fsm) arguments."
  (if (mevedel-directive-planning-enabled record)
      (progn
        (require 'mevedel-directive-plan)
        (mevedel-directive-plan-start directive action prompt-fn callback))
    (overlay-put directive 'mevedel-directive-action
                 (if (eq action 'implement-this) 'implement action))
    (mevedel--process-directive
     directive (alist-get 'implement mevedel-action-preset-alist)
     prompt-fn callback)))

(defun mevedel--implement-discussion (directive &optional callback)
  "Implement DIRECTIVE using its complete local discussion as feedback."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((record (mevedel--directive-record directive)))
    (mevedel--dispatch-directive-implementation
     directive record 'implement-this
     (lambda (content)
       (mevedel--implement-discussion-prompt content record))
     callback)))

(defun mevedel--request-directive-changes
    (directive feedback &optional callback)
  "Implement DIRECTIVE again using focused FEEDBACK and latest activity."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let* ((record (mevedel--directive-record directive))
         (new-context-p (mevedel-directive-subdirectives record)))
    ;; Validate before changing presentation or starting request setup.
    (mevedel--request-changes-prompt "" record feedback new-context-p)
    (mevedel--dispatch-directive-implementation
     directive record 'request-changes
     (lambda (content)
       (mevedel--request-changes-prompt
        content record feedback new-context-p))
     callback)))

(defun mevedel--retry-directive (directive guidance &optional callback)
  "Retry DIRECTIVE using its latest failure and optional GUIDANCE."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((record (mevedel--directive-record directive)))
    ;; Validate before changing presentation or starting request setup.
    (mevedel--retry-directive-prompt "" record guidance)
    (mevedel--dispatch-directive-implementation
     directive record 'retry
     (lambda (content)
       (mevedel--retry-directive-prompt content record guidance))
     callback)))

(provide 'mevedel-directive-request)
;;; mevedel-directive-request.el ends here
