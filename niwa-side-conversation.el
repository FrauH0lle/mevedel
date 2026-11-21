;;; niwa-side-conversation.el -- Ephemeral side conversations -*- lexical-binding: t -*-

;;; Commentary:

;; Implements `/btw' as one transient conversation forked from the current
;; root transcript.  The inherited transcript remains model-visible but is
;; not projected into the side view.

;;; Code:

(require 'niwa-utilities)

(require 'niwa-engine)

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel-request nil t)
  (require 'niwa-structs))

;; `gptel'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-context--collect "ext:gptel-context"
                  (&optional context-alist))
(declare-function gptel-context--wrap-in-buffer "ext:gptel-context"
                  (context-string &optional method))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)
(defvar gptel-backend)
(defvar gptel-context)
(defvar gptel-context-string-function)
(defvar gptel-model)
(defvar gptel-prompt-transform-functions)
(defvar gptel-reasoning-effort)
(defvar gptel-send--handlers)
(defvar gptel-send--transitions)
(defvar gptel-system-prompt)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)
(autoload 'gptel-context--collect "gptel-context")
(autoload 'gptel-context--wrap-in-buffer "gptel-context")

;; `niwa-agent-exec'
(declare-function niwa-agent-exec-request-snapshot
                  "niwa-agent-exec" (policy))
(autoload 'niwa-agent-exec-request-snapshot "niwa-agent-exec")

;; `niwa-chat'
(declare-function niwa-chat-install-request-hooks "niwa-chat" ())
(declare-function niwa-chat-prepare-transcript-buffer "niwa-chat" ())
(autoload 'niwa-chat-install-request-hooks "niwa-chat")
(autoload 'niwa-chat-prepare-transcript-buffer "niwa-chat")

;; `niwa-claude-code'
(declare-function niwa-claude-code-backend-p "niwa-claude-code-backend" (backend))

;; `niwa-claude-code-btw'
(declare-function niwa-claude-code-btw-assert-open "niwa-claude-code-btw" ())
(declare-function niwa-claude-code-btw-close "niwa-claude-code-btw" ())
(declare-function niwa-claude-code-btw-open
                  "niwa-claude-code-btw" (parent side history ready))
(declare-function niwa-claude-code-btw-parent-history "niwa-claude-code-btw" ())
(autoload 'niwa-claude-code-btw-close "niwa-claude-code-btw")
(autoload 'niwa-claude-code-btw-assert-open "niwa-claude-code-btw")
(autoload 'niwa-claude-code-btw-open "niwa-claude-code-btw")
(autoload 'niwa-claude-code-btw-parent-history "niwa-claude-code-btw")

;; `niwa-compact-evidence'
(declare-function niwa-compact-evidence-context-snapshot
                  "niwa-compact-evidence" (context))
(autoload 'niwa-compact-evidence-context-snapshot
  "niwa-compact-evidence")

;; `niwa-execution'
(declare-function niwa-execution-teardown-session
                  "niwa-execution" (session))

;; `niwa-hooks'
(declare-function niwa-hooks-effective-rules
                  "niwa-hooks"
                  (&optional session workspace request invocation))
(declare-function niwa-hooks-normalize-rules
                  "niwa-hooks" (rules &optional scope))
(defvar niwa-hooks-context-frozen-p)
(defvar niwa-hooks-tool-events)
(autoload 'niwa-hooks-effective-rules "niwa-hooks")
(autoload 'niwa-hooks-normalize-rules "niwa-hooks")

;; `niwa-mentions'
(declare-function niwa--transform-expand-mentions
                  "niwa-mentions" (fsm))
(declare-function niwa-mentions-prepare-user-input
                  "niwa-mentions" (text &optional session))
(autoload 'niwa--transform-expand-mentions "niwa-mentions")
(autoload 'niwa-mentions-prepare-user-input "niwa-mentions")

;; `niwa-permission-queue'
(declare-function niwa-permission-queue-abort-all
                  "niwa-permission-queue" (&optional session))
(autoload 'niwa-permission-queue-abort-all "niwa-permission-queue")

;; `niwa-permission-persistence'
(declare-function niwa-permission-persistence-load-resource-grants
                  "niwa-permission-persistence" (workspace))
(declare-function niwa-permission-persistence-load-rules
                  "niwa-permission-persistence" (workspace))
(autoload 'niwa-permission-persistence-load-resource-grants
  "niwa-permission-persistence")
(autoload 'niwa-permission-persistence-load-rules
  "niwa-permission-persistence")

;; `niwa-permission-rules'
(defvar niwa-permission-rules)
(defvar niwa-protected-paths)

;; `niwa-permissions'
(declare-function niwa-permission-freeze-context
                  "niwa-permissions" (persistent-rules resource-grants))
(autoload 'niwa-permission-freeze-context "niwa-permissions")

;; `niwa-presets'
(declare-function niwa--wrap-terminal-handlers
                  "niwa-presets" (handlers &optional transitions))
(declare-function niwa-preset--build-transitions
                  "niwa-presets" (transitions))
(autoload 'niwa--wrap-terminal-handlers "niwa-presets")
(autoload 'niwa-preset--build-transitions "niwa-presets")

;; `niwa-reminders'
(declare-function niwa-reminders--handle-inject
                  "niwa-reminders" (fsm))
(declare-function niwa-reminders-format-block
                  "niwa-reminders" (content))
(autoload 'niwa-reminders--handle-inject "niwa-reminders")
(autoload 'niwa-reminders-format-block "niwa-reminders")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-structs'
(declare-function niwa-request-ephemeral-p "niwa-structs" (cl-x) t)
(declare-function niwa-request-fsm "niwa-structs" (cl-x) t)
(declare-function niwa-request-id "niwa-structs" (cl-x) t)
(declare-function niwa-request-one-shot-mutations-p
                  "niwa-structs" (cl-x) t)
(declare-function niwa-request-skill-permission-rules
                  "niwa-structs" (cl-x) t)
(declare-function niwa-request-turn "niwa-structs" (cl-x) t)
(declare-function niwa-session-audit-session
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-create
                  "niwa-structs" (name workspace &optional working-directory))
(declare-function niwa-session-hook-rules "niwa-structs" (cl-x) t)
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-mode
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-rules
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-resource-grants
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-sandbox-mode
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)
(defvar niwa--workspace)

;; `niwa-system'
(defvar niwa-memory-dirs)

;; `niwa-tool-registry'
(declare-function niwa-tool-ensure "niwa-tool-registry" (name))
(declare-function niwa-tool-resolve-gptel
                  "niwa-tool-registry" (specs))

;; `niwa-transcript'
(declare-function niwa-transcript-segments
                  "niwa-transcript" (start end))
(autoload 'niwa-transcript-segments "niwa-transcript")

;; `niwa-turn'
(declare-function niwa-request-begin
                  "niwa-turn" (session &optional options))
(declare-function niwa-request-end
                  "niwa-turn" (&optional abort-plan-approval))
(autoload 'niwa-request-begin "niwa-turn")
(autoload 'niwa-request-end "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--restore-overlay
                  "niwa-utilities"
                  (buffer overlay-start overlay-end properties))
(autoload 'niwa--restore-overlay "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view--ensure
                  "niwa-view" (data-buf &optional view-name options))
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))
(defvar niwa-view--abort-function)
(autoload 'niwa-view--ensure "niwa-view")
(autoload 'niwa-view-rerender "niwa-view")

;; `niwa-view-composer'
(declare-function niwa-view--clear-input "niwa-view-composer" ())
(declare-function niwa-view-composer-owner "niwa-view-composer" ())
(autoload 'niwa-view-composer-owner "niwa-view-composer")
(declare-function niwa-view--forward-input "niwa-view-composer" t)
(declare-function niwa-view--input-text "niwa-view-composer" ())
(declare-function niwa-view-composer-draft-start "niwa-view-composer"
                  (&optional view))
(declare-function niwa-view-composer-draft-text "niwa-view-composer"
                  (&optional view without-properties))
(declare-function niwa-view-composer-insert "niwa-view-composer"
                  (text &optional position view))

;; `niwa-view-pair'
(declare-function niwa-view-composer-display "niwa-view-pair" (view-window))
(declare-function niwa-view-transcript-window "niwa-view-pair" (&optional window))

;; `niwa-view-render'
(declare-function niwa-view--full-rerender
                  "niwa-view-render"
                  (&optional transcript-buffer source-changed-p))

;; `niwa-view-stream'
(declare-function niwa-view-stream-ensure-progress-for-fsm
                  "niwa-view-stream" (fsm))
(autoload 'niwa-view-stream-ensure-progress-for-fsm
  "niwa-view-stream")

;; `niwa-workspace'
(defvar niwa-workspace-additional-roots)

(defconst niwa-side-conversation--tool-names
  '("Read" "Glob" "Grep"
    "XrefReferences" "XrefDefinitions" "Imenu" "Treesitter"
    "WebSearch" "WebFetch"
    "ApplyPatch" "Bash" "WriteStdin" "ListExecutions" "StopExecution")
  "Exact tool surface available to an ephemeral side conversation.")

(defvar-local niwa-side-conversation--side-buffer nil
  "Side data buffer owned by this parent data buffer.")

(defvar-local niwa-side-conversation--parent-buffer nil
  "Parent data buffer from which this side conversation was forked.")
;; Planted before the side buffer's major-mode change and read by its
;; kill-buffer cleanup, so it must survive `kill-all-local-variables'.
(put 'niwa-side-conversation--parent-buffer 'permanent-local t)

(defvar-local niwa-side-conversation--frozen-context nil
  "Invocation-time gptel context used by this side conversation.")
;; Holds temporary files/buffers that must be cleaned on kill even when
;; the buffer dies between context freezing and full initialization.
(put 'niwa-side-conversation--frozen-context 'permanent-local t)

(defvar-local niwa-side-conversation--frozen-request-denies nil
  "Active parent request denies inherited by each side request.")


;;
;;; Parent state helpers

(defun niwa-side-conversation--tool-hook-rules (rules)
  "Return a frozen copy of tool-pipeline hook RULES."
  (cl-remove-if-not
   (lambda (entry) (memq (car entry) niwa-hooks-tool-events))
   (copy-tree (niwa-hooks-normalize-rules rules))))

(defun niwa-side-conversation-parent-active-p (&optional buffer)
  "Return non-nil when BUFFER's side parent has an active request."
  (let* ((side (or buffer (current-buffer)))
         (parent
          (and (buffer-live-p side)
               (buffer-local-value
                'niwa-side-conversation--parent-buffer side))))
    (and (buffer-live-p parent)
         (buffer-local-value 'niwa--current-request parent))))

(defun niwa-side-conversation-mutation-warning (record effect)
  "Return a parent-activity warning for RECORD, marking RECORD as warned.

RECORD is a mutable plist carrying :data-buffer.  When that buffer's
side parent has an active request, set RECORD's
:parent-active-warning-shown-p and return a warning line naming EFFECT
\(e.g. \"applying this patch\").  Return nil otherwise.  Both
conversations share one workspace, so approving a mutation while the
parent still runs deserves an extra confirmation step."
  (when (niwa-side-conversation-parent-active-p
         (plist-get record :data-buffer))
    (plist-put record :parent-active-warning-shown-p t)
    (format "Warning: the parent request is still active; %s affects its workspace.\n"
            effect)))

(defun niwa-side-conversation-mutation-warning-pending-p (record)
  "Return non-nil when RECORD still owes the user a parent-activity warning.
Callers re-render RECORD (which emits the warning via
`niwa-side-conversation-mutation-warning') instead of settling."
  (and (niwa-side-conversation-parent-active-p
        (plist-get record :data-buffer))
       (not (plist-get record :parent-active-warning-shown-p))))

(defun niwa-side-conversation--interrupted-boundary (body)
  "Return a model-visible interrupted-context boundary containing BODY."
  (concat (propertize "\n" 'gptel 'ignore)
          (niwa-reminders-format-block body)
          (propertize "\n" 'gptel 'ignore)))

(defun niwa-side-conversation--request-locals ()
  "Return the current buffer's frozen gptel request locals."
  (let* ((fsm (and niwa--current-request
                   (niwa-request-fsm niwa--current-request)))
         (info (and fsm (gptel-fsm-info fsm))))
    (if (and info (plist-member info :niwa-request-locals))
        (copy-tree (plist-get info :niwa-request-locals))
      ;; Reachable only without an active request:
      ;; `niwa-side-conversation--snapshot' rejects active requests
      ;; that lack the transform-time snapshot keys.
      (niwa-agent-exec-request-snapshot
       (list :backend gptel-backend
             :model gptel-model
             ;; No global definition exists; the variable appears only
             ;; as buffer-local request state.
             :effort (and (boundp 'gptel-reasoning-effort)
                          gptel-reasoning-effort))))))


;;
;;; Frozen gptel context

(defun niwa-side-conversation--delete-frozen-context (&optional context)
  "Delete temporary resources owned by frozen CONTEXT."
  (let ((context (or context niwa-side-conversation--frozen-context)))
    (dolist (media (plist-get context :media))
      (ignore-errors (delete-file (car media))))
    (dolist (file (plist-get context :source-files))
      (ignore-errors (delete-file file)))
    (dolist (buffer (plist-get context :source-buffers))
      (when (buffer-live-p buffer)
        (ignore-errors
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer buffer))))))))

(defun niwa-side-conversation--freeze-context-range (range)
  "Return RANGE with marker endpoints replaced by fixed positions."
  (cons (if (markerp (car range))
            (marker-position (car range))
          (car range))
        (if (markerp (cdr range))
            (marker-position (cdr range))
          (cdr range))))

(defun niwa-side-conversation--restore-context-labels (text labels)
  "Replace temporary source names in TEXT according to LABELS."
  (when text
    (with-temp-buffer
      (insert text)
      (dolist (label labels)
        (goto-char (point-min))
        (while (search-forward (car label) nil t)
          (let ((properties
                 (text-properties-at (match-beginning 0))))
            (replace-match
             (if properties
                 (apply #'propertize (cdr label) properties)
               (cdr label))
             t t))))
      (buffer-string))))

(defun niwa-side-conversation--copy-context-sources (contexts)
  "Return immutable temporary copies of gptel CONTEXTS and their resources."
  (let (copies labels media source-buffers source-files)
    (condition-case err
        (progn
          (dolist (context contexts)
            (let* ((source (car context))
                   (spec (copy-tree (cdr context))))
              (when-let* ((bounds (plist-get spec :bounds)))
                (setq spec
                      (plist-put
                       spec :bounds
                       (if (consp (car-safe bounds))
                           (mapcar
                            #'niwa-side-conversation--freeze-context-range
                            bounds)
                         (niwa-side-conversation--freeze-context-range
                          bounds)))))
              (cond
               ((bufferp source)
                (let* ((contents
                        (with-current-buffer source
                          (save-restriction
                            (widen)
                            (buffer-substring (point-min) (point-max)))))
                       (mode (buffer-local-value 'major-mode source))
                       (copy
                        (generate-new-buffer
                         (format " *niwa-btw-context:%s*"
                                 (buffer-name source)))))
                  (push copy source-buffers)
                  (push (cons (buffer-name copy) (buffer-name source)) labels)
                  (with-current-buffer copy
                    (insert contents)
                    (setq-local major-mode mode)
                    (set-buffer-modified-p nil))
                  (let (overlays)
                    (dolist (overlay (plist-get spec :overlays))
                      (when (and (overlayp overlay)
                                 (eq (overlay-buffer overlay) source))
                        (push (niwa--restore-overlay
                               copy
                               (overlay-start overlay)
                               (overlay-end overlay)
                               (overlay-properties overlay))
                              overlays)))
                    (setq spec
                          (plist-put spec :overlays (nreverse overlays))))
                  (push (cons copy spec) copies)))
               ((stringp source)
                (let ((copy
                       (make-temp-file
                        "niwa-btw-context-" nil
                        (file-name-extension source t))))
                  (if (plist-get spec :mime)
                      (push (cons copy (copy-tree spec)) media)
                    (push copy source-files))
                  (copy-file source copy t)
                  (set-file-modes copy #o600)
                  (push (cons copy spec) copies)
                  (let ((abbreviated-copy (abbreviate-file-name copy))
                        (abbreviated-source
                         (abbreviate-file-name source)))
                    (push (cons abbreviated-copy abbreviated-source) labels)
                    (unless (and (equal abbreviated-copy copy)
                                 (equal abbreviated-source source))
                      (push (cons copy source) labels))))))))
          (list :contexts (nreverse copies)
                :labels (nreverse labels)
                :media media
                :source-buffers (nreverse source-buffers)
                :source-files (nreverse source-files)))
      ((error quit)
       (niwa-side-conversation--delete-frozen-context
        (list :media media
              :source-buffers source-buffers
              :source-files source-files))
       (signal (car err) (cdr err))))))

(defun niwa-side-conversation--freeze-gptel-context
    (owner-buffer callback)
  "Materialize the current gptel context, then call CALLBACK.
OWNER-BUFFER owns invocation-time source copies while formatting is pending."
  (if (not (and gptel-use-context gptel-context))
      (funcall callback nil)
    (let* ((resources
            (niwa-side-conversation--copy-context-sources
             (gptel-context--collect (copy-tree gptel-context))))
           (contexts (plist-get resources :contexts))
           (method gptel-use-context)
           (labels (plist-get resources :labels))
           (media (plist-get resources :media))
           (source-buffers (plist-get resources :source-buffers))
           (source-files (plist-get resources :source-files))
           done)
      (condition-case err
          (progn
            (when (buffer-live-p owner-buffer)
              (with-current-buffer owner-buffer
                (setq-local niwa-side-conversation--frozen-context
                            (list :method method
                                  :media media
                                  :source-buffers source-buffers
                                  :source-files source-files))))
            (let ((finish
                   (lambda (text)
                     (unless done
                       (setq done t)
                       (let (context handed-off)
                         (unwind-protect
                             (progn
                               (niwa-side-conversation--delete-frozen-context
                                (list :source-buffers source-buffers
                                      :source-files source-files))
                               (setq context
                                     (list
                                      :method method
                                      :text
                                      (niwa-side-conversation--restore-context-labels
                                       text labels)
                                      :media media))
                               (if (buffer-live-p owner-buffer)
                                   (progn
                                     (with-current-buffer owner-buffer
                                       (setq-local
                                        niwa-side-conversation--frozen-context
                                        context))
                                     (funcall callback context))
                                 (niwa-side-conversation--delete-frozen-context
                                  context))
                               (setq handed-off t))
                           (unless handed-off
                             (niwa-side-conversation--delete-frozen-context
                              (or context resources)))))))))
              (if (= (car (func-arity gptel-context-string-function)) 2)
                  (progn
                    (funcall gptel-context-string-function finish contexts)
                    nil)
                (funcall
                 finish
                 (funcall gptel-context-string-function contexts)))))
        ((error quit)
         (unless done
           (niwa-side-conversation--delete-frozen-context resources))
         (signal (car err) (cdr err)))))))

(defun niwa-side-conversation--transform-frozen-context (fsm)
  "Inject FSM's invocation-time gptel context into this request copy."
  (when-let* ((data-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p data-buffer))
              (context
               (buffer-local-value
                'niwa-side-conversation--frozen-context data-buffer))
              (text (plist-get context :text)))
    (gptel-context--wrap-in-buffer text (plist-get context :method))))

(defun niwa-side-conversation--transform-frozen-effort (fsm)
  "Install FSM's invocation-time reasoning effort in this request copy."
  (when-let* ((data-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p data-buffer)))
    (setq-local gptel-reasoning-effort
                (buffer-local-value 'gptel-reasoning-effort data-buffer))
    (setf (gptel-fsm-info fsm)
          (plist-put (gptel-fsm-info fsm)
                     :reasoning-effort gptel-reasoning-effort))))


;;
;;; Parent snapshot

(defun niwa-side-conversation--accepted-context-p ()
  "Return non-nil when the transcript contains any accepted model context."
  (or (cl-some (lambda (segment) (eq (car segment) 'user))
               (niwa-transcript-segments (point-min) (point-max)))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^#\\+begin_summary\\b" nil t))))

(defun niwa-side-conversation--marker-position (value)
  "Return VALUE's position when it belongs to the current buffer."
  (cond
   ((and (markerp value) (eq (marker-buffer value) (current-buffer)))
    (marker-position value))
   ((and (integerp value) (<= (point-min) value) (<= value (point-max)))
    value)))

(defun niwa-side-conversation--snapshot ()
  "Return the invocation-time model context for a new side conversation."
  (if (not niwa--current-request)
      (niwa-compact-evidence-context-snapshot 'all)
    (let* ((fsm (niwa-request-fsm niwa--current-request))
           (info (and fsm (gptel-fsm-info fsm)))
           (base (and info
                      (niwa-side-conversation--marker-position
                       (plist-get info :position))))
           (state (and fsm (gptel-fsm-state fsm)))
           (model-context (and info
                               (plist-get info :niwa-model-context)))
           (cutoff base)
           pending-tool-cutoff)
      (unless base
        (user-error "The active parent turn has no stable snapshot boundary"))
      (unless (stringp model-context)
        (user-error "The active parent turn has no effective context snapshot"))
      (cond
       ((eq state 'DONE)
        (setq cutoff (point-max)))
       ((memq state '(TPRE TOOL TRET))
        (when-let* ((tracking
                     (niwa-side-conversation--marker-position
                      (plist-get info :tracking-marker))))
          (setq cutoff (max cutoff tracking)))
        (when (eq state 'TOOL)
          (dolist (tool-overlay (overlays-in base cutoff))
            (when (overlay-get tool-overlay 'gptel-tool)
              (dolist (prompt-overlay (overlay-get tool-overlay 'prompt))
                (when (and (overlayp prompt-overlay)
                           (eq (overlay-buffer prompt-overlay)
                               (current-buffer)))
                  (setq pending-tool-cutoff
                        (min (or pending-tool-cutoff cutoff)
                             (overlay-start prompt-overlay))))))))))
      (dolist (segment
               (niwa-transcript-segments base (point-max)))
        (when (eq (car segment) 'tool)
          (setq cutoff (max cutoff (caddr segment)))))
      (when pending-tool-cutoff
        (setq cutoff (min cutoff pending-tool-cutoff)))
      (concat
       model-context
       (buffer-substring base cutoff)
       (niwa-side-conversation--interrupted-boundary
        "The inherited parent turn is incomplete. Any assistant or tool material from that turn is reference-only; do not treat it as a settled result.")))))


;;
;;; Request lifecycle

(defun niwa-side-conversation--busy-p (data-buffer)
  "Return non-nil when DATA-BUFFER owns active request work."
  (and
   (buffer-live-p data-buffer)
   (or (buffer-local-value 'niwa--current-request data-buffer)
       (and (boundp 'gptel--request-alist)
            (cl-some
             (lambda (entry)
               (eq (plist-get (gptel-fsm-info (cadr entry)) :buffer)
                   data-buffer))
             gptel--request-alist)))))

(defun niwa-side-conversation--validate-input (input)
  "Reject unsupported side-conversation syntax in INPUT."
  (when (string-match-p "\\`[ \t\n]*[/$][A-Za-z0-9_.:-]+" input)
    (user-error "Slash commands and skills are unavailable in /btw"))
  (when (string-match-p "\\(?:\\`\\|[[:space:]]\\)@agent:" input)
    (user-error "Agent mentions are unavailable in /btw")))

(defun niwa-side-conversation--handle-wait (fsm)
  "Begin the transient side request owned by FSM on its first WAIT."
  (let* ((info (gptel-fsm-info fsm))
         (data-buffer (plist-get info :buffer)))
    (when (and (not (plist-get info :niwa-request-begun))
               (buffer-live-p data-buffer))
      (with-current-buffer data-buffer
        (when niwa--session
          (niwa-session-artifacts-assert-new-mutation-authority
           niwa--session)
          (let ((request (niwa-request-begin niwa--session)))
            (setf (niwa-request-fsm request) fsm
                  (niwa-request-one-shot-mutations-p request) t
                  (niwa-request-ephemeral-p request) t
                  (niwa-request-skill-permission-rules request)
                  (copy-tree
                   niwa-side-conversation--frozen-request-denies))
            (setq info
                  (plist-put info :niwa-request-id
                             (niwa-request-id request)))
            (niwa-view-stream-ensure-progress-for-fsm fsm))))
      (setf (gptel-fsm-info fsm)
            (plist-put info :niwa-request-begun t)))))

(defun niwa-side-conversation--handle-terminal (fsm)
  "Settle the transient side request owned by terminal FSM."
  (when-let* ((data-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p data-buffer)))
    (with-current-buffer data-buffer
      (when (and niwa--current-request
                 (eq fsm (niwa-request-fsm niwa--current-request)))
        (setf (niwa-session-turn-count niwa--session)
              (max (niwa-session-turn-count niwa--session)
                   (niwa-request-turn niwa--current-request)))
        (niwa-request-end)))))

(defun niwa-side-conversation--abort (&optional _data-buffer)
  "Abort work in the current side data buffer without persistence."
  (if (niwa-claude-code-backend-p gptel-backend)
      (niwa-claude-code-btw-close)
    (let ((aborted (niwa-side-conversation--busy-p (current-buffer))))
      ;; Tool and permission callbacks may launch a follow-up provider request.
      ;; Settle them before draining gptel's process list.
      (when niwa--current-request
	(niwa-request-end))
      (niwa-permission-queue-abort-all niwa--session)
      (while (niwa-side-conversation--busy-p (current-buffer))
	(setq aborted t)
	(gptel-abort (current-buffer)))
      (when aborted
	(goto-char (point-max))
	(insert
	 (niwa-side-conversation--interrupted-boundary
          "The preceding side response was interrupted and is incomplete. Its assistant and tool material is reference-only; do not treat it as a settled result."))
	(niwa-view-rerender niwa--view-buffer)))))


;;
;;; Creation and teardown

(defun niwa-side-conversation--clear-parent-link ()
  "Clear this side conversation's link from its live parent."
  (when (buffer-live-p niwa-side-conversation--parent-buffer)
    (with-current-buffer niwa-side-conversation--parent-buffer
      (setq niwa-side-conversation--side-buffer nil))))

(defun niwa-side-conversation--close-owned ()
  "Close the side conversation owned by the current parent buffer."
  (when (buffer-live-p niwa-side-conversation--side-buffer)
    (let ((side niwa-side-conversation--side-buffer))
      (if-let* ((view (buffer-local-value 'niwa--view-buffer side))
                ((buffer-live-p view)))
          (kill-buffer view)
        (kill-buffer side)))))

(defun niwa-side-conversation--freeze-parent-state ()
  "Freeze the current parent buffer's session and permission state.

Returns a plist of invocation-time copies consumed by
`niwa-side-conversation--make-session' and
`niwa-side-conversation--init-side-buffer'."
  (let ((workspace (or niwa--workspace
                       (niwa-session-workspace niwa--session)))
        (native (and (niwa-claude-code-backend-p gptel-backend)
                     (niwa-claude-code-btw-parent-history))))
    (list
     :parent-session niwa--session
     :workspace workspace
     :working-directory
     (or (niwa-session-working-directory niwa--session)
         default-directory)
     :snapshot (if native "" (niwa-side-conversation--snapshot))
     :native-history native
     :request-locals (niwa-side-conversation--request-locals)
     :hook-rules (niwa-side-conversation--tool-hook-rules
                  (niwa-hooks-effective-rules
                   niwa--session workspace niwa--current-request nil))
     :additional-roots (copy-tree niwa-workspace-additional-roots)
     :memory-dirs (copy-tree niwa-memory-dirs)
     :temp-directory temporary-file-directory
     :default-rules (copy-tree niwa-permission-rules)
     :request-denies
     (and niwa--current-request
          (cl-remove-if-not
           (lambda (rule)
             (eq (plist-get (cdr rule) :action) 'deny))
           (copy-tree
            (niwa-request-skill-permission-rules
             niwa--current-request))))
     :protected-paths (copy-tree niwa-protected-paths)
     :persistent-rules
     (copy-tree (niwa-permission-persistence-load-rules workspace))
     :persistent-grants
     (copy-tree
      (niwa-permission-persistence-load-resource-grants workspace)))))

(defun niwa-side-conversation--make-session (frozen)
  "Create and return the transient side session from FROZEN parent state."
  (let* ((parent-session (plist-get frozen :parent-session))
         (side-session
          (niwa-session-create
           (format "btw (snapshot of %s)"
                   (niwa-session-name parent-session))
           (plist-get frozen :workspace)
           (plist-get frozen :working-directory))))
    (setf (niwa-session-permission-mode side-session)
          (niwa-session-permission-mode parent-session)
          (niwa-session-audit-session side-session) parent-session
          (niwa-session-hook-rules side-session)
          (plist-get frozen :hook-rules)
          (niwa-session-permission-rules side-session)
          (append (copy-tree (plist-get frozen :request-denies))
                  (copy-tree
                   (niwa-session-permission-rules parent-session)))
          (niwa-session-resource-grants side-session)
          (copy-tree (niwa-session-resource-grants parent-session))
          (niwa-session-sandbox-mode side-session)
          (niwa-session-sandbox-mode parent-session))
    side-session))

(defun niwa-side-conversation--init-side-buffer
    (side-data side-session frozen frozen-context)
  "Configure SIDE-DATA as a side transcript buffer and return its view.

SIDE-SESSION is the transient session for the conversation.  FROZEN is
the plist from `niwa-side-conversation--freeze-parent-state'.
FROZEN-CONTEXT is the materialized gptel context plist."
  (with-current-buffer side-data
    (niwa-chat-prepare-transcript-buffer)
    (setq-local niwa--session side-session
                niwa--current-request nil
                niwa--workspace (plist-get frozen :workspace)
                niwa-side-conversation--frozen-request-denies
                (plist-get frozen :request-denies)
                niwa-hooks-context-frozen-p t
                niwa-workspace-additional-roots
                (plist-get frozen :additional-roots)
                niwa-memory-dirs (plist-get frozen :memory-dirs)
                temporary-file-directory (plist-get frozen :temp-directory)
                default-directory (plist-get frozen :working-directory))
    (dolist (setting (plist-get frozen :request-locals))
      (set (make-local-variable (car setting)) (cdr setting)))
    (setq-local gptel-context
                (copy-tree (plist-get frozen-context :media))
                niwa-side-conversation--frozen-context frozen-context
                niwa-permission-rules (plist-get frozen :default-rules)
                niwa-protected-paths (plist-get frozen :protected-paths))
    (niwa-permission-freeze-context
     (plist-get frozen :persistent-rules)
     (plist-get frozen :persistent-grants))
    (let ((handlers (copy-tree (default-value 'gptel-send--handlers))))
      ;; Mention expansion stages reminder entries on the fsm; without
      ;; the reminder injector in this ephemeral chain they would never
      ;; reach the request.
      (setcdr (assq 'WAIT handlers)
              (cons #'niwa-side-conversation--handle-wait
                    (cons #'niwa-reminders--handle-inject
                          (cdr (assq 'WAIT handlers)))))
      (dolist (state '(DONE ERRS ABRT))
        (let ((entry (assq state handlers)))
          (setcdr entry
                  (append (cdr entry)
                          (list
                           #'niwa-side-conversation--handle-terminal)))))
      (setq-local gptel-send--handlers
                  (niwa--wrap-terminal-handlers
                   handlers (default-value 'gptel-send--transitions))))
    (setq-local gptel-send--transitions
                (niwa-preset--build-transitions
                 (copy-tree (default-value 'gptel-send--transitions)))
                gptel-prompt-transform-functions
                '(niwa--transform-expand-mentions
                  niwa-side-conversation--transform-frozen-effort
                  niwa-side-conversation--transform-frozen-context))
    (let ((guardrail
           (concat
            "You are in an ephemeral /btw side conversation. "
            "Its inherited parent context is frozen at invocation; "
            "later parent activity is unavailable and your replies "
            "are not merged back into the parent conversation. "
            "The side transcript is not persisted, but approved "
            "workspace effects are durable and shared with the parent. "
            "Slash commands, skills, Eval, delegation, tasks, Goals, "
            "Plan authority, and workflow settlement are unavailable.")))
      (setq-local
       gptel-system-prompt
       (cond
        ((null gptel-system-prompt) guardrail)
        ((stringp gptel-system-prompt)
         (concat gptel-system-prompt "\n\n" guardrail))
        ((listp gptel-system-prompt)
         (cons (concat (car gptel-system-prompt)
                       (and (car gptel-system-prompt) "\n\n")
                       guardrail)
               (cdr gptel-system-prompt)))
        (t
         (error "Expected a materialized gptel system prompt")))))
    (require 'niwa-tools)
    (dolist (name niwa-side-conversation--tool-names)
      (niwa-tool-ensure name))
    (setq-local
     gptel-tools
     (plist-get
      (niwa-tool-resolve-gptel
       (mapcar (lambda (name) (list :tool name))
               niwa-side-conversation--tool-names))
      :active)
     gptel-use-tools t)
    (niwa-chat-install-request-hooks)
    (insert (plist-get frozen :snapshot))
    (setq-local niwa-view--abort-function
                #'niwa-side-conversation--abort)
    (let* ((transcript-start (copy-marker (point-max) nil))
           (side-view
            (niwa-view--ensure
             side-data nil
             (list :side-conversation-p t
                   :transcript-start transcript-start))))
      (with-current-buffer side-view
        (use-local-map (copy-keymap (current-local-map)))
        (local-set-key (kbd "C-c C-z") #'niwa-side-conversation-close))
      side-view)))

(defun niwa-side-conversation--create (parent-data callback)
  "Create a side view forked from PARENT-DATA, then call CALLBACK."
  (let* ((frozen (niwa-side-conversation--freeze-parent-state))
         (side-session (niwa-side-conversation--make-session frozen))
         (parent-tick (buffer-chars-modified-tick parent-data))
         (side-data (generate-new-buffer " *niwa-btw-data*")))
    (with-current-buffer side-data
      ;; Plant cleanup before the asynchronous context freeze so a kill
      ;; during pending formatting still releases owned resources.  The
      ;; parent link and the kill-buffer entries are permanent-local and
      ;; survive the major-mode change in
      ;; `niwa-side-conversation--init-side-buffer'.
      (setq-local niwa-side-conversation--parent-buffer parent-data)
      (add-hook 'kill-buffer-hook
                #'niwa-side-conversation--delete-frozen-context nil t)
      (add-hook 'kill-buffer-hook
                #'niwa-side-conversation--clear-parent-link nil t))
    (setq niwa-side-conversation--side-buffer side-data)
    (add-hook 'kill-buffer-hook
              #'niwa-side-conversation--close-owned nil t)
    (condition-case err
        (niwa-side-conversation--freeze-gptel-context
         side-data
         (lambda (frozen-context)
           (condition-case callback-error
               (let ((initialize
                      (lambda ()
                        (let ((side-view
                               (niwa-side-conversation--init-side-buffer
                                side-data side-session frozen frozen-context)))
                          (with-current-buffer side-view
                            (niwa-view--full-rerender))
                          (funcall callback side-view)))))
                 (if-let* ((history (plist-get frozen :native-history)))
                     (progn
                       (unless (and (buffer-live-p parent-data)
                                    (= parent-tick (buffer-chars-modified-tick parent-data)))
                         (user-error "Parent history changed while opening /btw; try again"))
                       (niwa-claude-code-btw-open parent-data side-data history initialize))
                   (funcall initialize)))
             ;; Killing the side also clears its parent's link.
             ((error quit)
              (when (buffer-live-p side-data) (kill-buffer side-data))
              (signal (car callback-error) (cdr callback-error))))))
      ((error quit)
       (when (buffer-live-p side-data)
         (kill-buffer side-data))
       (signal (car err) (cdr err))))))


;;
;;; Commands

;;;###autoload
(defun niwa-side-conversation-close ()
  "Close the current ephemeral /btw side conversation."
  (interactive)
  (with-current-buffer (niwa-view-composer-owner)
    (unless (bound-and-true-p niwa-view--side-conversation-p)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "This is not a /btw side conversation"))
    ;; Settle remote mutation authority before the ordinary close guard runs.
    (when niwa--session
      (niwa-execution-teardown-session niwa--session))
    (kill-buffer (current-buffer))))

(defun niwa-side-conversation-send ()
  "Send the current side-composer prompt as an independent turn."
  (interactive)
  (with-current-buffer (niwa-view-composer-owner)
    (unless (bound-and-true-p niwa-view--side-conversation-p)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "This is not a /btw side conversation"))
    (unless (buffer-live-p niwa--data-buffer)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Side conversation data has been discarded"))
    (when (niwa-side-conversation--busy-p niwa--data-buffer)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "The /btw response is still active"))
    (with-current-buffer niwa--data-buffer
      (when (niwa-claude-code-backend-p gptel-backend)
        (niwa-with-diagnostic-errors (niwa-diagnostic-scope (current-buffer))
          (niwa-claude-code-btw-assert-open))))
    (let* ((raw (niwa-view--input-text))
           (session (buffer-local-value 'niwa--session niwa--data-buffer)))
      (when (string-empty-p raw)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Nothing to send"))
      (niwa-with-diagnostic-errors (niwa-diagnostic-scope session)
        (niwa-side-conversation--validate-input raw))
      (let ((input
             (with-current-buffer niwa--data-buffer
               (niwa-mentions-prepare-user-input raw session))))
        (niwa-view--forward-input
         input :display-text raw :prompt-checked t)))))

(defun niwa-side-conversation-open (&optional prompt)
  "Open this root buffer's side conversation and optionally send PROMPT."
  (when niwa-side-conversation--parent-buffer
    (user-error "Side conversations cannot be nested"))
  (unless (and niwa--session
               (niwa-session-workspace niwa--session))
    (user-error "No niwa session in this buffer"))
  (unless (niwa-claude-code-backend-p gptel-backend)
    (niwa-engine-assert-local-history niwa--session "/btw" gptel-backend t))
  (unless (niwa-side-conversation--accepted-context-p)
    (user-error "Send a parent prompt before opening /btw"))
  (when (and prompt (not (string-blank-p prompt)))
    (niwa-side-conversation--validate-input prompt))
  (let* ((parent-data (current-buffer))
         (parent-view niwa--view-buffer)
         (parent-draft
          (and (buffer-live-p parent-view)
               (niwa-view-composer-draft-text parent-view)))
         (side-data
          (and (buffer-live-p niwa-side-conversation--side-buffer)
               niwa-side-conversation--side-buffer))
         (side-view
          (and side-data
               (buffer-local-value 'niwa--view-buffer side-data))))
    (cl-labels
        ((display-side (view)
           (pop-to-buffer view)
           (when-let* ((window (niwa-view-transcript-window))
                       ((eq view (window-buffer window)))
                       (composer (niwa-view-composer-display window)))
             (select-window composer)))
         (show-side
           (view)
           (let ((data (buffer-local-value 'niwa--data-buffer view)))
             (when (and prompt (not (string-blank-p prompt)))
               (when (niwa-side-conversation--busy-p data)
                 (display-side view)
                 (user-error "The /btw response is still active"))
               (with-current-buffer view
                 (unless (string-blank-p (niwa-view-composer-draft-text view t))
                   (display-side view)
                   (user-error "The /btw composer already has a draft"))
                 (niwa-view-composer-insert
                  prompt (niwa-view-composer-draft-start view) view)
                 (niwa-side-conversation-send)))
             (when (and parent-draft (buffer-live-p parent-view)
                        (equal-including-properties
                         parent-draft (niwa-view-composer-draft-text parent-view)))
               (with-current-buffer parent-view
                 (niwa-view--clear-input)))
             (display-side view)
             view)))
      (cond
       ((buffer-live-p side-view)
        (show-side side-view))
       ((buffer-live-p side-data)
        (user-error "The /btw side conversation is still opening"))
       (t
        (niwa-side-conversation--create parent-data #'show-side))))))

(provide 'niwa-side-conversation)
;;; niwa-side-conversation.el ends here
