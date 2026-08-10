;;; mevedel-side-conversation.el -- Ephemeral side conversations -*- lexical-binding: t -*-

;;; Commentary:

;; Implements `/btw' as one transient conversation forked from the current
;; root transcript.  The inherited transcript remains model-visible but is
;; not projected into the side view.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel-request nil t)
  (require 'mevedel-structs))

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

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec-request-snapshot
                  "mevedel-agent-exec" (policy))

;; `mevedel-chat'
(declare-function mevedel-chat-install-request-hooks "mevedel-chat" ())
(declare-function mevedel-chat-prepare-transcript-buffer "mevedel-chat" ())

;; `mevedel-compact'
(declare-function mevedel-compact-context-snapshot
                  "mevedel-compact" (context))

;; `mevedel-hooks'
(declare-function mevedel-hooks-effective-rules
                  "mevedel-hooks"
                  (&optional session workspace request invocation))
(declare-function mevedel-hooks-normalize-rules
                  "mevedel-hooks" (rules &optional scope))
(defvar mevedel-hooks--context-frozen-p)
(defvar mevedel-hooks-tool-events)

;; `mevedel-mentions'
(declare-function mevedel--transform-expand-mentions
                  "mevedel-mentions" (fsm))
(declare-function mevedel-mentions-prepare-user-input
                  "mevedel-mentions" (text &optional session))

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue-abort-all
                  "mevedel-permission-queue" (&optional session))

;; `mevedel-permissions'
(declare-function mevedel-permission--load-persistent-resource-grants
                  "mevedel-permissions" (workspace))
(declare-function mevedel-permission--load-persistent-rules
                  "mevedel-permissions" (workspace))
(declare-function mevedel-permission-freeze-context
                  "mevedel-permissions" (persistent-rules resource-grants))
(defvar mevedel-permission-rules)
(defvar mevedel-protected-paths)

;; `mevedel-presets'
(declare-function mevedel--wrap-terminal-handlers
                  "mevedel-presets" (handlers &optional transitions))

;; `mevedel-reminders'
(declare-function mevedel-reminders-format-block
                  "mevedel-reminders" (content))

;; `mevedel-structs'
(declare-function mevedel-request-begin
                  "mevedel-structs" (session &optional directive-uuid))
(declare-function mevedel-request-directive-uuid
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-end
                  "mevedel-structs" (&optional abort-plan-approval))
(declare-function mevedel-request-ephemeral-p "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-fsm "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-one-shot-mutations-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-turn "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-audit-session
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-create
                  "mevedel-structs" (name workspace &optional working-directory))
(declare-function mevedel-session-hook-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-resource-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-sandbox-mode
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel--workspace)

;; `mevedel-system'
(defvar mevedel-memory-dirs)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-ensure "mevedel-tool-registry" (name))
(declare-function mevedel-tool-resolve-gptel
                  "mevedel-tool-registry" (specs))

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-exclude-directive-turns
                  "mevedel-transcript-audit" (&optional fsm))

;; `mevedel-utilities'
(declare-function mevedel--restore-overlay
                  "mevedel-utilities"
                  (buffer overlay-start overlay-end properties))

;; `mevedel-view'
(declare-function mevedel-view--ensure
                  "mevedel-view" (data-buf &optional view-name options))
(defvar mevedel-view--abort-function)

;; `mevedel-view-composer'
(declare-function mevedel-view--clear-input "mevedel-view-composer" ())
(declare-function mevedel-view--composer-snapshot
                  "mevedel-view-composer" (session))
(declare-function mevedel-view--forward-input "mevedel-view-composer" t)
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view--input-text "mevedel-view-composer" ())

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender
                  "mevedel-view-render"
                  (&optional transcript-buffer source-changed-p))

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-ensure-progress-for-fsm
                  "mevedel-view-stream" (fsm))

;; `mevedel-workspace'
(defvar mevedel-workspace-additional-roots)

(defconst mevedel-side-conversation--tool-names
  '("Read" "Glob" "Grep"
    "XrefReferences" "XrefDefinitions" "Imenu" "Treesitter"
    "WebSearch" "WebFetch" "YouTube"
    "ApplyPatch" "Bash" "WriteStdin" "ListExecutions" "StopExecution")
  "Exact tool surface available to an ephemeral side conversation.")

(defvar-local mevedel-side-conversation--side-buffer nil
  "Side data buffer owned by this parent data buffer.")

(defvar-local mevedel-side-conversation--parent-buffer nil
  "Parent data buffer from which this side conversation was forked.")
;; Planted before the side buffer's major-mode change and read by its
;; kill-buffer cleanup, so it must survive `kill-all-local-variables'.
(put 'mevedel-side-conversation--parent-buffer 'permanent-local t)

(defvar-local mevedel-side-conversation--frozen-context nil
  "Invocation-time gptel context used by this side conversation.")
;; Holds temporary files/buffers that must be cleaned on kill even when
;; the buffer dies between context freezing and full initialization.
(put 'mevedel-side-conversation--frozen-context 'permanent-local t)

(defvar-local mevedel-side-conversation--frozen-request-denies nil
  "Active parent request denies inherited by each side request.")


;;
;;; Parent state helpers

(defun mevedel-side-conversation--tool-hook-rules (rules)
  "Return a frozen copy of tool-pipeline hook RULES."
  (require 'mevedel-hooks)
  (cl-remove-if-not
   (lambda (entry) (memq (car entry) mevedel-hooks-tool-events))
   (copy-tree (mevedel-hooks-normalize-rules rules))))

(defun mevedel-side-conversation-parent-active-p (&optional buffer)
  "Return non-nil when BUFFER's side parent has an active request."
  (let* ((side (or buffer (current-buffer)))
         (parent
          (and (buffer-live-p side)
               (buffer-local-value
                'mevedel-side-conversation--parent-buffer side))))
    (and (buffer-live-p parent)
         (buffer-local-value 'mevedel--current-request parent))))

(defun mevedel-side-conversation-mutation-warning (record effect)
  "Return a parent-activity warning for RECORD, marking RECORD as warned.

RECORD is a mutable plist carrying :data-buffer.  When that buffer's
side parent has an active request, set RECORD's
:parent-active-warning-shown-p and return a warning line naming EFFECT
\(e.g. \"applying this patch\").  Return nil otherwise.  Both
conversations share one workspace, so approving a mutation while the
parent still runs deserves an extra confirmation step."
  (when (mevedel-side-conversation-parent-active-p
         (plist-get record :data-buffer))
    (plist-put record :parent-active-warning-shown-p t)
    (format "Warning: the parent request is still active; %s affects its workspace.\n"
            effect)))

(defun mevedel-side-conversation-mutation-warning-pending-p (record)
  "Return non-nil when RECORD still owes the user a parent-activity warning.
Callers re-render RECORD (which emits the warning via
`mevedel-side-conversation-mutation-warning') instead of settling."
  (and (mevedel-side-conversation-parent-active-p
        (plist-get record :data-buffer))
       (not (plist-get record :parent-active-warning-shown-p))))

(defun mevedel-side-conversation--interrupted-boundary (body)
  "Return a model-visible interrupted-context boundary containing BODY."
  (require 'mevedel-reminders)
  (concat (propertize "\n" 'gptel 'ignore)
          (mevedel-reminders-format-block body)
          "\n"))

(defun mevedel-side-conversation--request-locals ()
  "Return the current buffer's frozen gptel request locals."
  (require 'mevedel-agent-exec)
  (let* ((fsm (and mevedel--current-request
                   (mevedel-request-fsm mevedel--current-request)))
         (info (and fsm (gptel-fsm-info fsm))))
    (if (and info (plist-member info :mevedel-request-locals))
        (copy-tree (plist-get info :mevedel-request-locals))
      ;; Reachable only without an active request:
      ;; `mevedel-side-conversation--snapshot' rejects active requests
      ;; that lack the transform-time snapshot keys.
      (mevedel-agent-exec-request-snapshot
       (list :backend gptel-backend
             :model gptel-model
             ;; No global definition exists; the variable appears only
             ;; as buffer-local request state.
             :effort (and (boundp 'gptel-reasoning-effort)
                          gptel-reasoning-effort))))))


;;
;;; Frozen gptel context

(defun mevedel-side-conversation--delete-frozen-context (&optional context)
  "Delete temporary resources owned by frozen CONTEXT."
  (let ((context (or context mevedel-side-conversation--frozen-context)))
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

(defun mevedel-side-conversation--freeze-context-range (range)
  "Return RANGE with marker endpoints replaced by fixed positions."
  (cons (if (markerp (car range))
            (marker-position (car range))
          (car range))
        (if (markerp (cdr range))
            (marker-position (cdr range))
          (cdr range))))

(defun mevedel-side-conversation--restore-context-labels (text labels)
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

(defun mevedel-side-conversation--copy-context-sources (contexts)
  "Return immutable temporary copies of gptel CONTEXTS and their resources."
  (require 'mevedel-utilities)
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
                            #'mevedel-side-conversation--freeze-context-range
                            bounds)
                         (mevedel-side-conversation--freeze-context-range
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
                         (format " *mevedel-btw-context:%s*"
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
                        (push (mevedel--restore-overlay
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
                        "mevedel-btw-context-" nil
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
       (mevedel-side-conversation--delete-frozen-context
        (list :media media
              :source-buffers source-buffers
              :source-files source-files))
       (signal (car err) (cdr err))))))

(defun mevedel-side-conversation--freeze-gptel-context
    (owner-buffer callback)
  "Materialize the current gptel context, then call CALLBACK.
OWNER-BUFFER owns invocation-time source copies while formatting is pending."
  (if (not (and gptel-use-context gptel-context))
      (funcall callback nil)
    (require 'gptel-context)
    (let* ((resources
            (mevedel-side-conversation--copy-context-sources
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
                (setq-local mevedel-side-conversation--frozen-context
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
                               (mevedel-side-conversation--delete-frozen-context
                                (list :source-buffers source-buffers
                                      :source-files source-files))
                               (setq context
                                     (list
                                      :method method
                                      :text
                                      (mevedel-side-conversation--restore-context-labels
                                       text labels)
                                      :media media))
                               (if (buffer-live-p owner-buffer)
                                   (progn
                                     (with-current-buffer owner-buffer
                                       (setq-local
                                        mevedel-side-conversation--frozen-context
                                        context))
                                     (funcall callback context))
                                 (mevedel-side-conversation--delete-frozen-context
                                  context))
                               (setq handed-off t))
                           (unless handed-off
                             (mevedel-side-conversation--delete-frozen-context
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
           (mevedel-side-conversation--delete-frozen-context resources))
         (signal (car err) (cdr err)))))))

(defun mevedel-side-conversation--transform-frozen-context (fsm)
  "Inject FSM's invocation-time gptel context into this request copy."
  (when-let* ((data-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p data-buffer))
              (context
               (buffer-local-value
                'mevedel-side-conversation--frozen-context data-buffer))
              (text (plist-get context :text)))
    (require 'gptel-context)
    (gptel-context--wrap-in-buffer text (plist-get context :method))))

(defun mevedel-side-conversation--transform-frozen-effort (fsm)
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

(defun mevedel-side-conversation--accepted-context-p ()
  "Return non-nil when the transcript contains any accepted model context."
  (require 'mevedel-transcript)
  (or (cl-some (lambda (segment) (eq (car segment) 'user))
               (mevedel-transcript-segments (point-min) (point-max)))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^#\\+begin_summary\\b" nil t))))

(defun mevedel-side-conversation--marker-position (value)
  "Return VALUE's position when it belongs to the current buffer."
  (cond
   ((and (markerp value) (eq (marker-buffer value) (current-buffer)))
    (marker-position value))
   ((and (integerp value) (<= (point-min) value) (<= value (point-max)))
    value)))

(defun mevedel-side-conversation--snapshot ()
  "Return the invocation-time model context for a new side conversation."
  (require 'mevedel-compact)
  (if (not mevedel--current-request)
      (mevedel-compact-context-snapshot 'all)
    (let* ((fsm (mevedel-request-fsm mevedel--current-request))
           (info (and fsm (gptel-fsm-info fsm)))
           (base (and info
                      (mevedel-side-conversation--marker-position
                       (plist-get info :position))))
           (state (and fsm (gptel-fsm-state fsm)))
           (model-context (and info
                               (plist-get info :mevedel-model-context)))
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
                     (mevedel-side-conversation--marker-position
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
               (mevedel-transcript-segments base (point-max)))
        (when (eq (car segment) 'tool)
          (setq cutoff (max cutoff (caddr segment)))))
      (when pending-tool-cutoff
        (setq cutoff (min cutoff pending-tool-cutoff)))
      (concat
       model-context
       (buffer-substring base cutoff)
       (mevedel-side-conversation--interrupted-boundary
        "The inherited parent turn is incomplete. Any assistant or tool material from that turn is reference-only; do not treat it as a settled result.")))))


;;
;;; Request lifecycle

(defun mevedel-side-conversation--busy-p (data-buffer)
  "Return non-nil when DATA-BUFFER owns active request work."
  (and
   (buffer-live-p data-buffer)
   (or (buffer-local-value 'mevedel--current-request data-buffer)
       (and (boundp 'gptel--request-alist)
            (cl-some
             (lambda (entry)
               (eq (plist-get (gptel-fsm-info (cadr entry)) :buffer)
                   data-buffer))
             gptel--request-alist)))))

(defun mevedel-side-conversation--validate-input (input)
  "Reject unsupported side-conversation syntax in INPUT."
  (when (string-match-p "\\`[ \t\n]*[/$][A-Za-z0-9_.:-]+" input)
    (user-error "Slash commands and skills are unavailable in /btw"))
  (when (string-match-p "\\(?:\\`\\|[[:space:]]\\)@agent:" input)
    (user-error "Agent mentions are unavailable in /btw")))

(defun mevedel-side-conversation--handle-wait (fsm)
  "Begin the transient side request owned by FSM on its first WAIT."
  (let* ((info (gptel-fsm-info fsm))
         (data-buffer (plist-get info :buffer)))
    (when (and (not (plist-get info :mevedel-request-begun))
               (buffer-live-p data-buffer))
      (with-current-buffer data-buffer
        (when mevedel--session
          (let ((request (mevedel-request-begin mevedel--session)))
            (setf (mevedel-request-fsm request) fsm
                  (mevedel-request-one-shot-mutations-p request) t
                  (mevedel-request-ephemeral-p request) t
                  (mevedel-request-skill-permission-rules request)
                  (copy-tree
                   mevedel-side-conversation--frozen-request-denies))
            (setq info
                  (plist-put info :mevedel-request-id
                             (mevedel-request-id request)))
            (mevedel-view-stream-ensure-progress-for-fsm fsm))))
      (setf (gptel-fsm-info fsm)
            (plist-put info :mevedel-request-begun t)))))

(defun mevedel-side-conversation--handle-terminal (fsm)
  "Settle the transient side request owned by terminal FSM."
  (when-let* ((data-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p data-buffer)))
    (with-current-buffer data-buffer
      (when (and mevedel--current-request
                 (eq fsm (mevedel-request-fsm mevedel--current-request)))
        (setf (mevedel-session-turn-count mevedel--session)
              (max (mevedel-session-turn-count mevedel--session)
                   (mevedel-request-turn mevedel--current-request)))
        (mevedel-request-end)))))

(defun mevedel-side-conversation--abort (&optional _data-buffer)
  "Abort work in the current side data buffer without persistence."
  (let ((aborted (mevedel-side-conversation--busy-p (current-buffer))))
    ;; Tool and permission callbacks may launch a follow-up provider request.
    ;; Settle them before draining gptel's process list.
    (when mevedel--current-request
      (mevedel-request-end))
    (require 'mevedel-permission-queue)
    (mevedel-permission-queue-abort-all mevedel--session)
    (while (mevedel-side-conversation--busy-p (current-buffer))
      (setq aborted t)
      (gptel-abort (current-buffer)))
    (when aborted
      (goto-char (point-max))
      (insert
       (mevedel-side-conversation--interrupted-boundary
        "The preceding side response was interrupted and is incomplete. Its assistant and tool material is reference-only; do not treat it as a settled result."))
      (when (buffer-live-p mevedel--view-buffer)
        (with-current-buffer mevedel--view-buffer
          (mevedel-view--full-rerender))))))


;;
;;; Creation and teardown

(defun mevedel-side-conversation--clear-parent-link ()
  "Clear this side conversation's link from its live parent."
  (when (buffer-live-p mevedel-side-conversation--parent-buffer)
    (with-current-buffer mevedel-side-conversation--parent-buffer
      (setq mevedel-side-conversation--side-buffer nil))))

(defun mevedel-side-conversation--close-owned ()
  "Close the side conversation owned by the current parent buffer."
  (when (buffer-live-p mevedel-side-conversation--side-buffer)
    (let ((side mevedel-side-conversation--side-buffer))
      (if-let* ((view (buffer-local-value 'mevedel--view-buffer side))
                ((buffer-live-p view)))
          (kill-buffer view)
        (kill-buffer side)))))

(defun mevedel-side-conversation--freeze-parent-state ()
  "Freeze the current parent buffer's session and permission state.

Returns a plist of invocation-time copies consumed by
`mevedel-side-conversation--make-session' and
`mevedel-side-conversation--init-side-buffer'."
  (require 'mevedel-permissions)
  (let ((workspace (or mevedel--workspace
                       (mevedel-session-workspace mevedel--session))))
    (list
     :parent-session mevedel--session
     :workspace workspace
     :working-directory
     (or (mevedel-session-working-directory mevedel--session)
         default-directory)
     :snapshot (mevedel-side-conversation--snapshot)
     :request-locals (mevedel-side-conversation--request-locals)
     :hook-rules (mevedel-side-conversation--tool-hook-rules
                  (mevedel-hooks-effective-rules
                   mevedel--session workspace mevedel--current-request nil))
     :additional-roots (copy-tree mevedel-workspace-additional-roots)
     :memory-dirs (copy-tree mevedel-memory-dirs)
     :temp-directory temporary-file-directory
     :default-rules (copy-tree mevedel-permission-rules)
     :request-denies
     (and mevedel--current-request
          (cl-remove-if-not
           (lambda (rule)
             (eq (plist-get (cdr rule) :action) 'deny))
           (copy-tree
            (mevedel-request-skill-permission-rules
             mevedel--current-request))))
     :protected-paths (copy-tree mevedel-protected-paths)
     :persistent-rules
     (copy-tree (mevedel-permission--load-persistent-rules workspace))
     :persistent-grants
     (copy-tree
      (mevedel-permission--load-persistent-resource-grants workspace)))))

(defun mevedel-side-conversation--make-session (frozen)
  "Create and return the transient side session from FROZEN parent state."
  (let* ((parent-session (plist-get frozen :parent-session))
         (side-session
          (mevedel-session-create
           (format "btw (snapshot of %s)"
                   (mevedel-session-name parent-session))
           (plist-get frozen :workspace)
           (plist-get frozen :working-directory))))
    (setf (mevedel-session-permission-mode side-session)
          (mevedel-session-permission-mode parent-session)
          (mevedel-session-audit-session side-session) parent-session
          (mevedel-session-hook-rules side-session)
          (plist-get frozen :hook-rules)
          (mevedel-session-permission-rules side-session)
          (append (copy-tree (plist-get frozen :request-denies))
                  (copy-tree
                   (mevedel-session-permission-rules parent-session)))
          (mevedel-session-resource-grants side-session)
          (copy-tree (mevedel-session-resource-grants parent-session))
          (mevedel-session-sandbox-mode side-session)
          (mevedel-session-sandbox-mode parent-session))
    side-session))

(defun mevedel-side-conversation--init-side-buffer
    (side-data side-session frozen frozen-context)
  "Configure SIDE-DATA as a side transcript buffer and return its view.

SIDE-SESSION is the transient session for the conversation.  FROZEN is
the plist from `mevedel-side-conversation--freeze-parent-state'.
FROZEN-CONTEXT is the materialized gptel context plist."
  (with-current-buffer side-data
    (require 'mevedel-chat)
    (mevedel-chat-prepare-transcript-buffer)
    (setq-local mevedel--session side-session
                mevedel--current-request nil
                mevedel--workspace (plist-get frozen :workspace)
                mevedel-side-conversation--frozen-request-denies
                (plist-get frozen :request-denies)
                mevedel-hooks--context-frozen-p t
                mevedel-workspace-additional-roots
                (plist-get frozen :additional-roots)
                mevedel-memory-dirs (plist-get frozen :memory-dirs)
                temporary-file-directory (plist-get frozen :temp-directory)
                default-directory (plist-get frozen :working-directory))
    (dolist (setting (plist-get frozen :request-locals))
      (set (make-local-variable (car setting)) (cdr setting)))
    (setq-local gptel-context
                (copy-tree (plist-get frozen-context :media))
                mevedel-side-conversation--frozen-context frozen-context
                mevedel-permission-rules (plist-get frozen :default-rules)
                mevedel-protected-paths (plist-get frozen :protected-paths))
    (mevedel-permission-freeze-context
     (plist-get frozen :persistent-rules)
     (plist-get frozen :persistent-grants))
    (let ((handlers (copy-tree (default-value 'gptel-send--handlers))))
      (setcdr (assq 'WAIT handlers)
              (cons #'mevedel-side-conversation--handle-wait
                    (cdr (assq 'WAIT handlers))))
      (dolist (state '(DONE ERRS ABRT))
        (let ((entry (assq state handlers)))
          (setcdr entry
                  (append (cdr entry)
                          (list
                           #'mevedel-side-conversation--handle-terminal)))))
      (require 'mevedel-presets)
      (setq-local gptel-send--handlers
                  (mevedel--wrap-terminal-handlers
                   handlers (default-value 'gptel-send--transitions))))
    (setq-local gptel-send--transitions
                (copy-tree (default-value 'gptel-send--transitions))
                gptel-prompt-transform-functions
                '(mevedel--transform-expand-mentions
                  mevedel-transcript-exclude-directive-turns
                  mevedel-side-conversation--transform-frozen-effort
                  mevedel-side-conversation--transform-frozen-context))
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
    (require 'mevedel-tools)
    (require 'mevedel-tool-registry)
    (dolist (name mevedel-side-conversation--tool-names)
      (mevedel-tool-ensure name))
    (setq-local
     gptel-tools
     (plist-get
      (mevedel-tool-resolve-gptel
       (mapcar (lambda (name) (list :tool name))
               mevedel-side-conversation--tool-names))
      :active)
     gptel-use-tools t)
    (mevedel-chat-install-request-hooks)
    (insert (plist-get frozen :snapshot))
    (setq-local mevedel-view--abort-function
                #'mevedel-side-conversation--abort)
    (let ((transcript-start (copy-marker (point-max) nil)))
      (require 'mevedel-view)
      (mevedel-view--ensure
       side-data nil
       (list :side-conversation-p t
             :transcript-start transcript-start)))))

(defun mevedel-side-conversation--create (parent-data callback)
  "Create a side view forked from PARENT-DATA, then call CALLBACK."
  (let* ((frozen (mevedel-side-conversation--freeze-parent-state))
         (side-session (mevedel-side-conversation--make-session frozen))
         (side-data (generate-new-buffer " *mevedel-btw-data*")))
    (with-current-buffer side-data
      ;; Plant cleanup before the asynchronous context freeze so a kill
      ;; during pending formatting still releases owned resources.  The
      ;; parent link and the kill-buffer entries are permanent-local and
      ;; survive the major-mode change in
      ;; `mevedel-side-conversation--init-side-buffer'.
      (setq-local mevedel-side-conversation--parent-buffer parent-data)
      (add-hook 'kill-buffer-hook
                #'mevedel-side-conversation--delete-frozen-context nil t)
      (add-hook 'kill-buffer-hook
                #'mevedel-side-conversation--clear-parent-link nil t))
    (setq mevedel-side-conversation--side-buffer side-data)
    (add-hook 'kill-buffer-hook
              #'mevedel-side-conversation--close-owned nil t)
    (condition-case err
        (mevedel-side-conversation--freeze-gptel-context
         side-data
         (lambda (frozen-context)
           (condition-case callback-error
               (let ((side-view
                      (mevedel-side-conversation--init-side-buffer
                       side-data side-session frozen frozen-context)))
                 (with-current-buffer side-view
                   (mevedel-view--full-rerender))
                 (funcall callback side-view))
             ;; Killing the side buffer clears the parent link through
             ;; its kill-buffer hook.
             ((error quit)
              (when (buffer-live-p side-data)
                (kill-buffer side-data))
              (signal (car callback-error) (cdr callback-error))))))
      ((error quit)
       (when (buffer-live-p side-data)
         (kill-buffer side-data))
       (signal (car err) (cdr err))))))


;;
;;; Commands

(defun mevedel-side-conversation-send ()
  "Send the current side-composer prompt as an independent turn."
  (interactive)
  (unless (bound-and-true-p mevedel-view--side-conversation-p)
    (user-error "This is not a /btw side conversation"))
  (unless (buffer-live-p mevedel--data-buffer)
    (user-error "Side conversation data has been discarded"))
  (when (mevedel-side-conversation--busy-p mevedel--data-buffer)
    (user-error "The /btw response is still active"))
  (let* ((raw (mevedel-view--input-text))
         (session (buffer-local-value 'mevedel--session mevedel--data-buffer)))
    (when (string-empty-p raw)
      (user-error "Nothing to send"))
    (mevedel-side-conversation--validate-input raw)
    (let ((input
           (with-current-buffer mevedel--data-buffer
             (require 'mevedel-mentions)
             (mevedel-mentions-prepare-user-input raw session))))
      (mevedel-view--forward-input
       input :display-text raw :prompt-checked t))))

(defun mevedel-side-conversation-open (&optional prompt)
  "Open this root buffer's side conversation and optionally send PROMPT."
  (when mevedel-side-conversation--parent-buffer
    (user-error "Side conversations cannot be nested"))
  (unless (and mevedel--session
               (mevedel-session-workspace mevedel--session))
    (user-error "No mevedel session in this buffer"))
  (when (and mevedel--current-request
             (mevedel-request-directive-uuid mevedel--current-request))
    (user-error "/btw is unavailable during directive requests"))
  (unless (mevedel-side-conversation--accepted-context-p)
    (user-error "Send a parent prompt before opening /btw"))
  (when (and prompt (not (string-blank-p prompt)))
    (mevedel-side-conversation--validate-input prompt))
  (let* ((parent-data (current-buffer))
         (parent-view mevedel--view-buffer)
         (parent-session mevedel--session)
         (parent-composer
          (and (buffer-live-p parent-view)
               (with-current-buffer parent-view
                 (mevedel-view--composer-snapshot parent-session))))
         (side-data
          (and (buffer-live-p mevedel-side-conversation--side-buffer)
               mevedel-side-conversation--side-buffer))
         (side-view
          (and side-data
               (buffer-local-value 'mevedel--view-buffer side-data))))
    (cl-labels
        ((show-side
           (view)
           (let ((data (buffer-local-value 'mevedel--data-buffer view)))
             (when (and prompt (not (string-blank-p prompt)))
               (when (mevedel-side-conversation--busy-p data)
                 (pop-to-buffer view)
                 (user-error "The /btw response is still active"))
               (with-current-buffer view
                 (unless (string-empty-p (mevedel-view--input-text))
                   (pop-to-buffer view)
                   (user-error "The /btw composer already has a draft"))
                 (goto-char (mevedel-view--input-start))
                 (insert prompt)
                 (mevedel-side-conversation-send)))
             (when (and parent-composer (buffer-live-p parent-view))
               (with-current-buffer parent-view
                 (when (equal-including-properties
                        (plist-get parent-composer :text)
                        (buffer-substring
                         (mevedel-view--input-start) (point-max)))
                   (mevedel-view--clear-input))))
             (pop-to-buffer view)
             view)))
      (cond
       ((buffer-live-p side-view)
        (show-side side-view))
       ((buffer-live-p side-data)
        (user-error "The /btw side conversation is still opening"))
       (t
        (mevedel-side-conversation--create parent-data #'show-side))))))

(provide 'mevedel-side-conversation)
;;; mevedel-side-conversation.el ends here
