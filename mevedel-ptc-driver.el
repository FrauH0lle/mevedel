;;; mevedel-ptc-driver.el --- ToolScript driver -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Runs one Programmatic Tool Calling script through the ordinary mevedel tool
;; pipeline.  The interpreter supplies suspended tool requests; this driver
;; validates and dispatches them, records child audit state, reports progress,
;; enforces bounded fork/join, and settles cancellation and recovery state.
;;
;; The module has one execution interface:
;;
;;   (mevedel-ptc-driver-run CALLBACK SCRIPT ROSTER)
;;
;; SCRIPT is model-authored guest text and ROSTER is the request's closed list
;; of callable tool names.  CALLBACK receives the standard asynchronous tool
;; outcome.  Request policy and roster construction remain owned by
;; `mevedel-tool-ptc'.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-ptc-checkpoint'
(declare-function mevedel-ptc-checkpoint-start
                  "mevedel-ptc-checkpoint" (session buffer id script))
(declare-function mevedel-ptc-checkpoint-update
                  "mevedel-ptc-checkpoint" (session buffer id updates))

;; `mevedel-ptc-interpreter'
(declare-function mevedel-ptc-check-value
                  "mevedel-ptc-interpreter"
                  (state value &optional retain-p count-shared-p serializable-p))
(declare-function mevedel-ptc-close "mevedel-ptc-interpreter" (state))
(declare-function mevedel-ptc-intern "mevedel-ptc-interpreter" (state name))
(declare-function mevedel-ptc-keyword-p "mevedel-ptc-interpreter" (value))
(declare-function mevedel-ptc-start "mevedel-ptc-interpreter" (script roster))
(declare-function mevedel-ptc-step
                  "mevedel-ptc-interpreter"
                  (state &optional resume-value resume-retained-p))
(defvar mevedel-ptc-max-value-bytes)
(defvar mevedel-ptc-max-value-nodes)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-active-tool-use-id "mevedel-pipeline" ())
(declare-function mevedel-pipeline-run-tool-outcome
                  "mevedel-pipeline" (tool callback args &optional metadata))

;; `mevedel-structs'
(declare-function mevedel-request-push-canceller
                  "mevedel-structs" (request canceller))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (session))
(defvar mevedel--current-request)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-current-session
                  "mevedel-telemetry" (&optional buffer))
(declare-function mevedel-telemetry-finish
                  "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-start
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-media'
(declare-function mevedel-tool-media-reference-items
                  "mevedel-tool-media" (items))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-args "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-ensure
                  "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (tool))

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-handle-tool-progress
                  "mevedel-view-stream" (event))


;;;; Configuration

(defvar mevedel-ptc-parallelism 4
  "Maximum concurrent nested calls, customized by `mevedel-tool-ptc'.")


;;;; Nested calls

(defvar mevedel-ptc-driver--next-envelope-id 0
  "Fallback identity counter for ToolScript calls lacking a tool-use id.")

(defconst mevedel-ptc-driver--preview-length 200
  "Maximum characters of a nested result kept in the audit record.")

(defun mevedel-ptc-driver--convert-args (tool guest-args)
  "Convert GUEST-ARGS into a host keyword plist validated against TOOL.

Guest keywords live in the script's private symbol universe and are not
`eq' to host keywords, so each name is checked against the tool's
declared arguments and only then interned.  An undeclared argument is an
error naming what the tool does accept, which is also the script's
earliest chance to learn it got the contract wrong."
  (unless (fboundp 'mevedel-ptc-keyword-p)
    (require 'mevedel-ptc-interpreter))
  (let ((declared (mapcar (lambda (spec) (symbol-name (car spec)))
                          (mevedel-tool-args tool)))
        (name (mevedel-tool-name tool))
        (rest guest-args)
        (out nil))
    (while rest
      (let ((key (pop rest)))
        (unless (mevedel-ptc-keyword-p key)
          (error "%s: arguments must be :keyword value pairs" name))
        (unless rest
          (error "%s: argument %s has no value" name (symbol-name key)))
        (let ((bare (substring (symbol-name key) 1))
              (value (pop rest)))
          (unless (member bare declared)
            (error "%s has no argument `%s'; it accepts: %s"
                   name bare (string-join declared ", ")))
          (setq out (append out (list (intern (concat ":" bare)) value))))))
    (mevedel-ptc-check-value nil out nil nil t)
    out))

(defun mevedel-ptc-driver--preview (text)
  "Return a bounded preview of TEXT."
  (let ((text (if (stringp text) text (format "%S" text))))
    (if (> (length text) mevedel-ptc-driver--preview-length)
        (concat (substring text 0 mevedel-ptc-driver--preview-length) "...")
      text)))

(defconst mevedel-ptc-driver--arg-value-max 500
  "Maximum characters of one retained nested-call argument string.")

(defconst mevedel-ptc-driver--arg-total-max 4000
  "Maximum aggregate bytes retained for one nested call's arguments.")

(defconst mevedel-ptc-driver--arg-node-max 200
  "Maximum aggregate nodes retained for one nested call's arguments.")

(defconst mevedel-ptc-driver--child-detail-max 4000
  "Maximum printed size of one nested call's retained render data.")

(defun mevedel-ptc-driver--bound-strings (value limit)
  "Return VALUE with every string it contains truncated to LIMIT characters."
  (cond
   ((stringp value)
    (if (> (length value) limit)
        (concat (substring value 0 limit) "...")
      value))
   ((consp value)
    (cons (mevedel-ptc-driver--bound-strings (car value) limit)
          (mevedel-ptc-driver--bound-strings (cdr value) limit)))
   (t value)))

(defun mevedel-ptc-driver--child-args (plist)
  "Return PLIST bounded for retention in the audit record.

A child row renders through its own tool's renderer, which reads
arguments for header text, so a long value is truncated rather than
carried into the transcript whole."
  (unless (fboundp 'mevedel-ptc-check-value)
    (require 'mevedel-ptc-interpreter))
  (condition-case nil
      (let ((mevedel-ptc-max-value-bytes mevedel-ptc-driver--arg-total-max)
            (mevedel-ptc-max-value-nodes mevedel-ptc-driver--arg-node-max))
        (mevedel-ptc-check-value nil plist nil nil t)
        (mevedel-ptc-driver--bound-strings plist
                                         mevedel-ptc-driver--arg-value-max))
    (error nil)))

(defun mevedel-ptc-driver--child-render-data (data)
  "Return DATA when it is small enough to retain for a child row, else nil.

Oversized data is dropped whole rather than truncated: a nested
renderer reads its own structured fields, and a mangled value would
render worse than no value at all."
  (unless (fboundp 'mevedel-ptc-check-value)
    (require 'mevedel-ptc-interpreter))
  (when data
    (condition-case nil
        (let ((mevedel-ptc-max-value-bytes mevedel-ptc-driver--child-detail-max)
              (mevedel-ptc-max-value-nodes mevedel-ptc-driver--child-detail-max))
          (mevedel-ptc-check-value nil data nil nil t)
          data)
      (error nil))))

(defun mevedel-ptc-driver--render-value (value)
  "Return the model-visible rendering of a script's final VALUE."
  (cond
   ((stringp value) value)
   ((null value) "nil")
   (t (prin1-to-string value))))

(defun mevedel-ptc-driver--partial-summary (calls)
  "Return a bounded model-visible summary of the partial child CALLS audit."
  (if calls
      (concat
       "\n\nNested call audit:\n"
       (string-join
        (mapcar
         (lambda (call)
           (format "- %s %s (%s)"
                   (plist-get call :id)
                   (format "%s %s"
                           (plist-get call :tool)
                           (mevedel-ptc-driver--preview
                            (plist-get call :args)))
                   (plist-get call :status)))
         calls)
        "\n"))
    ""))

(defconst mevedel-ptc-driver--budget-kinds
  '(input expansion step time tool-call value retained-value atomic-work)
  "Typed interpreter failures that represent an exhausted guest budget.")

(defun mevedel-ptc-driver--finish-telemetry
    (span outcome nested-call-count &optional error-kind)
  "Finish SPAN for OUTCOME after NESTED-CALL-COUNT calls.
ERROR-KIND is the interpreter's typed failure category when available."
  (when span
    (apply #'mevedel-telemetry-finish span
           (append
            (list :outcome (if (eq outcome 'completed)
                               'final-value
                             outcome)
                  :nested-call-count nested-call-count)
            (when (and (eq outcome 'script-error)
                       (memq error-kind mevedel-ptc-driver--budget-kinds))
              (list :budget-kind error-kind))))))

(defun mevedel-ptc-driver--classify-outcome (state outcome)
  "Return (STATUS RESULT GUEST-VALUE) for structured OUTCOME under STATE."
  (let ((status (plist-get outcome :status))
        (reason (plist-get outcome :reason))
        (result (plist-get outcome :result)))
    (if (eq reason 'permission-denied)
        ;; Denial always aborts the envelope.  Bound an unexpectedly large
        ;; guardian explanation without turning denial into a guest error.
        (list 'denied (mevedel-ptc-driver--preview result) nil)
      (condition-case err
          (let ((guest-value
                 (if (eq status 'error)
                     (list (mevedel-ptc-intern state ":error") result)
                   result)))
            ;; Reject oversized child values before they enter the audit or
            ;; its durable checkpoint.  The interpreter checks again when
            ;; resuming so every guest-value ingress enforces the same budget.
            (mevedel-ptc-check-value state guest-value t nil t)
            (if (eq status 'error)
                (list 'error result guest-value)
              (list 'success result result)))
        ((mevedel-ptc-error error)
         (let* ((text
                 (format
                  "Error: nested tool result rejected: %s"
                  (if (eq (car err) 'mevedel-ptc-error)
                      (nth 2 err)
                    (error-message-string err))))
                ;; Keep the guest replacement small enough to fit after a
                ;; rejected sibling while retaining the detailed audit text.
                (guest-value
                 (list (mevedel-ptc-intern state ":error")
                       "Nested tool result rejected")))
           (mevedel-ptc-check-value state guest-value t)
           (list 'error text guest-value)))))))

(defun mevedel-ptc-driver--completed-count (calls)
  "Return the number of terminal child CALLS."
  (cl-count-if
   (lambda (call)
     (memq (plist-get call :status)
           '(success error denied cancelled interrupted)))
   calls))

(defun mevedel-ptc-driver--progress-calls (calls)
  "Return CALLS without full child results for transient live rendering."
  (mapcar (lambda (call)
            (list :id (plist-get call :id)
                  :tool (plist-get call :tool)
                  :status (plist-get call :status)))
          calls))


;;;; Driver

(defun mevedel-ptc-driver-run (callback script roster)
  "Run SCRIPT with ROSTER, delivering its outcome to CALLBACK."
  (require 'mevedel-ptc-interpreter)
  (require 'mevedel-telemetry)
  (let* ((data-buffer (current-buffer))
         (started-at (float-time))
         (envelope-id
          (or (mevedel-pipeline-active-tool-use-id)
              (format "ptc-%d"
                      (cl-incf mevedel-ptc-driver--next-envelope-id))))
         (session (mevedel-telemetry-current-session data-buffer))
         (checkpoint-session
          (and session (mevedel-session-execution-target session) session))
         (telemetry-span
          (when session
            (mevedel-telemetry-start
             session 'ptc-script :tool-use-id envelope-id))))
    (condition-case err
        (let* ((state (mevedel-ptc-start script roster))
               (child-number 0)
               (audit nil)
               (permission-waits nil)
               (settled nil)
               (interrupting nil)
               (child-cancellers nil)
               (batch-number 0)
               (pending nil)
               (active nil)
               (dispatch-many nil))
          (when checkpoint-session
            (require 'mevedel-ptc-checkpoint)
            (unless (mevedel-ptc-checkpoint-start
                     checkpoint-session data-buffer envelope-id script)
              (error "ToolScript audit checkpoint could not be persisted")))
          (letrec
              ((progress
               (lambda (type active-tool &optional known-total)
                  (ignore-errors
                    (when (fboundp 'mevedel-view-stream-handle-tool-progress)
                      (mevedel-view-stream-handle-tool-progress
                       (list :type type :data-buffer data-buffer
                             :tool-use-id envelope-id
                             :facts
                             (list :kind 'ptc :live-p (eq type 'progress)
                                   :active-tool active-tool
                                   :completed-count
                                   (mevedel-ptc-driver--completed-count audit)
                                   :known-total known-total
                                   :permission-waits
                                   (mapcar #'cdr permission-waits)
                                   :calls
                                   (mevedel-ptc-driver--progress-calls audit))))))))
               (finish
                (lambda (result status outcome &optional error-kind)
                  (unless settled
                    (setq settled t)
                    (let ((render-data
                           (list :kind 'ptc :outcome outcome
                                 :elapsed-seconds (- (float-time) started-at)
                                 :nested-call-count (length audit)
                                 :calls audit))
                          finalization-error)
                      (condition-case err
                          (progn
                            (mevedel-ptc-close state)
                            (when (and checkpoint-session
                                       (not
                                        (mevedel-ptc-checkpoint-update
                                         checkpoint-session data-buffer
                                         envelope-id
                                         (list :state 'settled :result result
                                               :render-data render-data))))
                              (error
                               "ToolScript final audit could not be persisted")))
                        (error (setq finalization-error err)))
                      (when finalization-error
                        (setq result
                              (format "Error: %s"
                                      (error-message-string
                                       finalization-error))
                              status 'error
                              outcome 'script-error
                              error-kind 'checkpoint
                              render-data
                              (plist-put render-data :outcome outcome))
                        ;; A transient write failure gets one retry with the
                        ;; honest error outcome.  Persistent failure still
                        ;; settles the outer call instead of stranding it.
                        (when checkpoint-session
                          (ignore-errors
                            (mevedel-ptc-checkpoint-update
                             checkpoint-session data-buffer envelope-id
                             (list :state 'settled :result result
                                   :render-data render-data)))))
                      (funcall progress 'terminal nil)
                      (ignore-errors
                        (mevedel-ptc-driver--finish-telemetry
                         telemetry-span outcome (length audit) error-kind))
                      (funcall callback
                               (list :result result :status status
                                     :render-data render-data))))))
               (checkpoint
                (lambda ()
                  (unless
                      (or (not checkpoint-session)
                          (mevedel-ptc-checkpoint-update
                           checkpoint-session data-buffer envelope-id
                           (list :render-data
                                 (list :kind 'ptc :outcome 'running
                                       :elapsed-seconds
                                       (- (float-time) started-at)
                                       :nested-call-count (length audit)
                                       :calls audit))))
                    (error "ToolScript child audit could not be persisted"))
                  t))
               (record
                (lambda (task status result &optional outcome)
                  (let* ((id (plist-get task :id))
                         (entry
                         (list :id id
                               :order (plist-get task :order)
                               :tool (or (plist-get task :name)
                                         (plist-get task :tool))
                               :args
                               (mevedel-ptc-driver--child-args
                                (or (plist-get task :plist)
                                    (plist-get task :args)))
                               :status status
                               :result result
                               :batch (plist-get task :batch)
                               :render-data
                               (mevedel-ptc-driver--child-render-data
                                (plist-get outcome :render-data))))
                        (media (plist-get outcome :media)))
                    (when media
                      (require 'mevedel-tool-media)
                      (setq entry
                            (plist-put
                             entry :media
                             (mevedel-tool-media-reference-items media))))
                    (setq audit
                          (cons entry
                                (cl-remove id audit
                                           :key (lambda (item)
                                                  (plist-get item :id))
                                           :test #'equal))))
                  (setq audit
                        (sort audit (lambda (a b)
                                      (< (plist-get a :order)
                                         (plist-get b :order)))))))
               (settle-children
                (lambda (status message)
                  (setq interrupting t pending nil active nil)
                  (dolist (call (copy-sequence audit))
                    (when (memq (plist-get call :status) '(queued running))
                      (funcall record call status message)))
                  (ignore-errors (funcall checkpoint))
                  (dolist (cancel child-cancellers)
                    (ignore-errors (funcall cancel)))))
               ;; Single-flight trampoline.  Most tool callbacks fire
               ;; synchronously, so resuming straight from one would recurse
               ;; per nested call and never return to the command loop --
               ;; which froze Emacs for the length of the script.  A timer
               ;; alone does not fix it either: a zero-delay timer can fire
               ;; inside a pipeline's own `accept-process-output' and nest
               ;; just the same.  So a resume only ever queues a value, and
               ;; exactly one `pump' runs at a time, from a timer, with the
               ;; stack unwound.
               (resume
                (lambda (value)
                  (setq pending (list value))
                  (unless active (funcall schedule))))
               (guard
                (lambda (function)
                  (condition-case err
                      (funcall function)
                    ((mevedel-ptc-error error)
                     (funcall settle-children
                              'interrupted "Interrupted after script failure")
                     (let ((typed-p (eq (car err) 'mevedel-ptc-error)))
                       (funcall
                        finish
                        (format "Error: %s"
                                (if typed-p
                                    (nth 2 err)
                                  (error-message-string err)))
                        'error 'script-error
                        (if typed-p (nth 1 err) 'script)))))))
               (schedule
                (lambda ()
                  (run-at-time
                   0 nil
                   (lambda ()
                     (funcall guard (lambda () (funcall tick)))))))
               (tick
                (lambda ()
                  (cond
                   ;; Fired inside a nested wait; come back once it unwinds.
                   (active (funcall schedule))
                   (pending
                    (let ((value (car pending)))
                      (setq pending nil active t)
                      (unwind-protect (funcall pump value)
                        (setq active nil))
                      (when pending (funcall schedule)))))))
               (pump
                (lambda (resume-value)
                  (unless settled
                    (let ((step (mevedel-ptc-step state resume-value t)))
                      (pcase (car step)
                        (:pause
                         ;; Interpreter fuel.  Returning to the command loop is
                         ;; the whole point: a tight guest loop must not freeze
                         ;; the view, the permission queue, or other sessions.
                         (funcall resume nil))
                        (:done
                         (let ((value (nth 1 step)))
                           ;; Rendering repeats shared references, so charge
                           ;; their logical serialized size before printing.
                           (mevedel-ptc-check-value state value nil t t)
                           (funcall finish
                                    (mevedel-ptc-driver--render-value value)
                                    'success 'completed)))
                        (:error
                         (funcall finish (format "Error: %s" (nth 2 step))
                                  'error 'script-error (nth 1 step)))
                        (:tool
                         (funcall dispatch (nth 1 step) (nth 2 step)))
                        (:tools
                         (funcall dispatch-many (nth 1 step))))))))
               (launch-child
                (lambda (task known-total complete)
                  (let ((name (plist-get task :name))
                        (child-id (plist-get task :id)))
                    (condition-case call-error
                        (let* ((_ (unless
                                      (fboundp
                                       'mevedel-tools--current-deferred-context)
                                    (require 'mevedel-tools)))
                               (tool (mevedel-tool-ensure name))
                               (_ (unless tool
                                    (error "Tool %s is unavailable" name)))
                               (plist
                                (mevedel-ptc-driver--convert-args
                                 tool (plist-get task :args))))
                          (plist-put task :plist plist)
                          (funcall record task 'running nil)
                          (plist-put
                           task :cancel
                           (with-current-buffer data-buffer
                             (mevedel-pipeline-run-tool-outcome
                              tool
                              (lambda (outcome)
                                (funcall
                                 guard
                                 (lambda ()
                                   (setq permission-waits
                                         (assoc-delete-all
                                          child-id permission-waits))
                                   ;; The host has completed this child even
                                   ;; if converting its result into a guest
                                   ;; value fails below.  Preserve that fact
                                   ;; so fatal settlement only interrupts its
                                   ;; unfinished siblings.
                                   (let ((raw-result
                                          (plist-get outcome :result)))
                                     (funcall
                                      record task
                                      (cond
                                       ((eq (plist-get outcome :reason)
                                            'permission-denied)
                                        'denied)
                                       ((eq (plist-get outcome :status) 'error)
                                        'error)
                                       (t 'success))
                                      (if (stringp raw-result)
                                          (mevedel-ptc-driver--preview raw-result)
                                        (format "Completed %s result"
                                                (type-of raw-result)))))
                                   (pcase-let
                                       ((`(,status ,result ,guest-value)
                                         (mevedel-ptc-driver--classify-outcome
                                          state outcome)))
                                     (funcall complete task status result
                                              guest-value outcome)))))
                              plist
                              (list :tool-use-id child-id
                                    :parent-tool-use-id envelope-id
                                    :source 'ptc
                                    :progress
                                    (lambda (event)
                                      (when (eq event 'permission-wait)
                                        (push (cons child-id name)
                                              permission-waits)
                                        (funcall progress 'progress name
                                                 known-total)))))))
                          (push (plist-get task :cancel) child-cancellers))
                      (error
                       ;; Malformed child calls are guest values, so a script
                       ;; can branch and retain already completed work.
                       (let* ((text (format "Error: %s"
                                            (error-message-string call-error)))
                              (guest-value
                               (list (mevedel-ptc-intern state ":error")
                                     text)))
                         (mevedel-ptc-check-value state guest-value t)
                         (funcall complete task 'error text
                                  guest-value
                                  nil)))))))
               (dispatch
                (lambda (name guest-args)
                  (let* ((order (cl-incf child-number))
                         (task (list :id (format "%s/%d" envelope-id order)
                                     :order order :name name :args guest-args)))
                    (funcall record task 'queued nil)
                    (funcall checkpoint)
                    (funcall progress 'progress name)
                    (funcall
                     launch-child task nil
                     (lambda (task status result guest-value outcome)
                       (if (and interrupting
                                (eq 'cancelled (plist-get outcome :reason)))
                           (progn
                             (funcall record task 'cancelled result outcome)
                             (funcall checkpoint)
                             (funcall progress 'progress nil))
                         (funcall record task status result outcome)
                         (funcall checkpoint)
                         (funcall progress 'progress nil)
                         (if (eq status 'denied)
                             (funcall
                              finish
                              (concat
                               (format "Error: script aborted because nested %s was denied: %s"
                                       name result)
                               (mevedel-ptc-driver--partial-summary audit))
                              'error 'denied)
                           (funcall resume guest-value))))))))
               (dispatch-batch
                (lambda (calls)
                  (let* ((total (length calls))
                         (results (make-vector total nil))
                         ;; A one-call join ran nothing concurrently, so it
                         ;; gets no batch identity and renders as an ordinary
                         ;; sequential row.
                         (batch (and (> total 1) (cl-incf batch-number)))
                         (queue
                          (cl-loop for call in calls
                                   for index from 0
                                   for order = (cl-incf child-number)
                                   collect (list :index index :order order
                                                 :batch batch
                                                 :id (format "%s/%d"
                                                             envelope-id order)
                                                 :name (car call)
                                                 :args (cadr call)
                                                 :cancel nil)))
                         (active 0)
                         (done 0)
                         (running nil)
                         (scheduled nil)
                         (joined nil)
                         (aborted nil))
                    (dolist (task queue)
                      (funcall record task 'queued nil))
                    (funcall checkpoint)
                    (letrec
                        ((schedule-batch
                          (lambda ()
                            (unless (or scheduled joined aborted)
                              (setq scheduled t)
                              (run-at-time
                               0 nil
                               (lambda ()
                                 (funcall
                                  guard
                                  (lambda ()
                                    (setq scheduled nil)
                                    (funcall batch-tick))))))))
                         (complete
                          (lambda (task status result guest-value
                                        &optional outcome)
                            (unless (or joined aborted settled)
                              (funcall record task status result outcome)
                              (funcall checkpoint)
                              (aset results (plist-get task :index) guest-value)
                              (setq active (1- active)
                                    done (1+ done)
                                    running (delq task running)
                                    permission-waits
                                    (assoc-delete-all (plist-get task :id)
                                                      permission-waits))
                              (funcall progress 'progress
                                       (and running
                                            (string-join
                                             (mapcar (lambda (item)
                                                       (plist-get item :name))
                                                     running)
                                             ", "))
                                       total)
                              (funcall schedule-batch))))
                         (start-one
                          (lambda (task)
                            (setq active (1+ active)
                                  running (append running (list task)))
                            (funcall
                             launch-child task total
                             (lambda (task status result guest-value outcome)
                               (let ((name (plist-get task :name)))
                                 (cond
                                  ((and (or aborted interrupting)
                                        (not settled)
                                        (eq (plist-get outcome :reason)
                                            'cancelled))
                                   (funcall record task 'cancelled result outcome)
                                   (funcall checkpoint))
                                  ((not (or joined aborted settled))
                                   (if (eq status 'denied)
                                       (progn
                                         (funcall record task status result outcome)
                                         (setq aborted t)
                                         (funcall
                                          settle-children 'cancelled
                                          "Cancelled after sibling denial")
                                         (setq queue nil)
                                         (funcall
                                          finish
                                          (concat
                                           (format
                                            "Error: batch aborted because nested %s was denied: %s"
                                            name result)
                                           (mevedel-ptc-driver--partial-summary
                                            audit))
                                          'error 'denied))
                                     (funcall complete task status result
                                              guest-value outcome)))))))))
                         (batch-tick
                          (lambda ()
                            (unless (or joined aborted settled)
                              (let ((slots (- (max 1 mevedel-ptc-parallelism)
                                              active)))
                                (while (and queue (> slots 0) (not aborted))
                                  (setq slots (1- slots))
                                  (funcall start-one (pop queue))))
                              (unless (or joined aborted settled)
                                (if (= done total)
                                    (progn
                                      (setq joined t)
                                      (funcall resume (append results nil)))
                                  (funcall progress 'progress
                                           (and running
                                                (string-join
                                                 (mapcar
                                                  (lambda (item)
                                                    (plist-get item :name))
                                                  running)
                                                 ", "))
                                           total)))))))
                      (funcall progress 'progress nil total)
                      (funcall schedule-batch))))))
            (setq dispatch-many dispatch-batch)
            (when (bound-and-true-p mevedel--current-request)
              (mevedel-request-push-canceller
               mevedel--current-request
               (lambda ()
                 (unless settled
                   (funcall
                    guard
                    (lambda ()
                      (funcall settle-children
                               'cancelled "Cancelled by request")
                      (funcall
                       finish
                       (concat "Error: script interrupted"
                               (mevedel-ptc-driver--partial-summary audit))
                       'error 'interrupted)))))))
            (funcall resume nil)))
      (error
       (let* ((typed-p (eq (car err) 'mevedel-ptc-error))
              (kind (if typed-p (nth 1 err) 'script))
              (message (if typed-p (nth 2 err) (error-message-string err)))
              (result (format "Error: %s" message)))
         (mevedel-ptc-driver--finish-telemetry
          telemetry-span 'script-error 0 kind)
         (funcall callback (list :result result :status 'error)))))))



(provide 'mevedel-ptc-driver)
;;; mevedel-ptc-driver.el ends here
