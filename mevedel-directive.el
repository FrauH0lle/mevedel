;;; mevedel-directive.el -- Directive lifecycle and rewind -*- lexical-binding: t -*-

;;; Commentary:

;; Directive mutation and derived lifecycle: nested-detail ordering, available
;; actions, state recomputed from surviving model activity, plan invalidation
;; on authored edits, and chronological workspace rewind.  The underlying
;; record structs remain in `mevedel-structs'.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  ;; Required for the cl-defstruct accessors and `setf' expanders of
  ;; directive record slots.
  (require 'mevedel-structs))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-abort
                  "mevedel-plan-mode" (&optional session outcome))

;; `mevedel-structs'
(declare-function mevedel-directive-attempt-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)


;;
;;; Directive mutation

(defun mevedel-directive-set-anchor (directive anchor)
  "Set DIRECTIVE's current ANCHOR."
  (setf (mevedel-directive-anchor directive) anchor))

(defun mevedel-directive-set-state (directive state)
  "Set DIRECTIVE's transient lifecycle STATE."
  (setf (mevedel-directive-state directive) state))

(defun mevedel-directive-set-planning-enabled (directive enabled)
  "Set DIRECTIVE's Plan-before-implementation preference to ENABLED."
  (setf (mevedel-directive-planning-enabled directive) (and enabled t)))

(defun mevedel-directive-set-skills (directive skills)
  "Set DIRECTIVE's implementation skill selection to SKILLS."
  (setf (mevedel-directive-skills directive) skills))

(defun mevedel-subdirective-copy (subdirective)
  "Return an independent snapshot of SUBDIRECTIVE."
  (mevedel-subdirective--create
   :id (substring-no-properties (mevedel-subdirective-id subdirective))
   :request (substring-no-properties (mevedel-subdirective-request subdirective))
   :anchor (copy-tree (mevedel-subdirective-anchor subdirective))))

(defun mevedel-subdirective-set-anchor (subdirective anchor)
  "Set SUBDIRECTIVE's current ANCHOR."
  (setf (mevedel-subdirective-anchor subdirective) anchor))

(defun mevedel-subdirective-set-request (subdirective request)
  "Set SUBDIRECTIVE's current REQUEST."
  (setf (mevedel-subdirective-request subdirective) request))

(defun mevedel-directive-sort-subdirectives (directive)
  "Sort DIRECTIVE's nested details by their source anchors."
  (setf
   (mevedel-directive-subdirectives directive)
   (sort
    (copy-sequence (mevedel-directive-subdirectives directive))
    (lambda (a b)
      (let* ((a-anchor (mevedel-subdirective-anchor a))
             (b-anchor (mevedel-subdirective-anchor b))
             (a-start (or (plist-get a-anchor :start) 0))
             (b-start (or (plist-get b-anchor :start) 0))
             (a-end (or (plist-get a-anchor :end) a-start))
             (b-end (or (plist-get b-anchor :end) b-start)))
        (or (< a-start b-start)
            (and (= a-start b-start)
                 (or (> a-end b-end)
                     (and (= a-end b-end)
                          (string-lessp (mevedel-subdirective-id a)
                                        (mevedel-subdirective-id b)))))))))))

(defun mevedel-workspace-add-directive (workspace directive)
  "Add DIRECTIVE to WORKSPACE."
  (push directive (mevedel-workspace-directives workspace))
  directive)

(defun mevedel-workspace-remove-directive (workspace directive)
  "Remove DIRECTIVE from WORKSPACE."
  (setf (mevedel-workspace-directives workspace)
        (delq directive (mevedel-workspace-directives workspace))))

(defun mevedel-workspace-set-directives (workspace directives)
  "Replace WORKSPACE's directive records with DIRECTIVES."
  (setf (mevedel-workspace-directives workspace) directives))


;;
;;; Derived lifecycle


(defun mevedel-directive-invalidate-plan (directive)
  "Invalidate DIRECTIVE's unstarted Plan authority."
  (when-let* ((plan (mevedel-directive-plan directive))
              ((not (eq (plist-get plan :status) 'implementing))))
    (setq plan (plist-put plan :status 'draft)
          plan (plist-put plan :invalidated t))
    (setf (mevedel-directive-plan directive) plan)
    (when-let* ((chat-buffer (plist-get plan :chat-buffer))
                ((buffer-live-p chat-buffer))
                (session (buffer-local-value 'mevedel--session chat-buffer))
                (entry (mevedel-session-pending-plan-approval session))
                ((equal (plist-get entry :directive-id)
                        (mevedel-directive-id directive))))
      (mevedel-plan-approval-abort session 'invalidated))
    t))

(defun mevedel-directive-add-subdirective (directive subdirective)
  "Add SUBDIRECTIVE to DIRECTIVE unless its identity is already present."
  (unless (cl-find (mevedel-subdirective-id subdirective)
                   (mevedel-directive-subdirectives directive)
                   :key #'mevedel-subdirective-id :test #'equal)
    (push subdirective (mevedel-directive-subdirectives directive))
    (mevedel-directive-sort-subdirectives directive)
    (mevedel-directive-invalidate-plan directive))
  subdirective)

(defun mevedel-directive-remove-subdirective (directive subdirective)
  "Remove SUBDIRECTIVE from DIRECTIVE."
  (setf (mevedel-directive-subdirectives directive)
        (delq subdirective (mevedel-directive-subdirectives directive)))
  (mevedel-directive-invalidate-plan directive)
  subdirective)

(defun mevedel-directive-set-request (directive request)
  "Set DIRECTIVE's current REQUEST and return it to Ready."
  (setf (mevedel-directive-request directive) request
        (mevedel-directive-state directive) nil)
  (mevedel-directive-invalidate-plan directive)
  request)

(defun mevedel-directive-request-changed-p (directive)
  "Return non-nil when DIRECTIVE differs from its latest attempt snapshot."
  (when-let* ((attempt (car (last (mevedel-directive-attempts directive)))))
    (not (equal (mevedel-directive-request directive)
                (mevedel-directive-attempt-directive-request attempt)))))

(defun mevedel-directive-actions (directive)
  "Return lifecycle actions currently available for DIRECTIVE."
  (pcase (if (memq (mevedel-directive-state directive)
                   '(implementing discussing planning))
             (mevedel-directive-state directive)
           (mevedel-directive-recompute-state directive))
    ((or 'nil 'ready) '(discuss implement))
    ('discussed '(continue-discussion implement-this))
    ('implemented '(discuss-result request-changes))
    ((or 'failed 'aborted) '(discuss-result retry))
    ((or 'implementing 'discussing 'planning) '(abort))))

(defun mevedel-directive-has-activity-p (directive)
  "Return non-nil when DIRECTIVE owns model-produced activity."
  (or (mevedel-directive-attempts directive)
      (mevedel-directive-discussion directive)
      (mevedel-directive-planning directive)))

(defun mevedel-directive-next-activity-sequence (directive)
  "Return the next monotonic activity sequence for DIRECTIVE."
  (1+ (apply #'max 0
             (delq nil
                   (append
                    (mapcar #'mevedel-directive-attempt-sequence
                            (mevedel-directive-attempts directive))
                    (mapcar #'mevedel-directive-discussion-turn-sequence
                            (mevedel-directive-discussion directive))
                    (mapcar (lambda (turn) (plist-get turn :sequence))
                            (mevedel-directive-planning directive)))))))

(defun mevedel-directive-recompute-state (directive)
  "Recompute DIRECTIVE state from its surviving model activity."
  (let* ((attempt (car (last (mevedel-directive-attempts directive))))
         (discussed-p
          (cl-some
           (lambda (turn)
             (and (eq 'success (mevedel-directive-discussion-turn-outcome turn))
                  (equal (mevedel-directive-request directive)
                         (mevedel-directive-discussion-turn-directive-request
                          turn))))
           (mevedel-directive-discussion directive)))
         (state
          (cond
           ((and discussed-p
                 (or (null attempt)
                     (mevedel-directive-request-changed-p directive)))
            'discussed)
           ((and attempt
                 (not (mevedel-directive-request-changed-p directive)))
            (pcase (mevedel-directive-attempt-outcome attempt)
              ('success 'implemented)
              ('aborted 'aborted)
              (_ 'failed))))))
    (setf (mevedel-directive-state directive) state)))

(defun mevedel-workspace-rewind-directives
    (workspace session-id target-turn)
  "Discard directive activity in WORKSPACE from SESSION-ID TARGET-TURN onward."
  (dolist (directive (mevedel-workspace-directives workspace))
    (let ((selection
           (copy-tree (plist-get (mevedel-directive-plan directive)
                                 :selection)))
          rewound-planned-attempt)
      (dolist (attempt (mevedel-directive-attempts directive))
        (let ((checkpoint (mevedel-directive-attempt-checkpoint attempt)))
          (when (and (equal session-id (plist-get checkpoint :session-id))
                     (>= (or (plist-get checkpoint :turn) 0) target-turn))
            (when (and (null rewound-planned-attempt)
                       (mevedel-directive-attempt-plan attempt))
              (setq rewound-planned-attempt attempt))
            (dolist
                (subdirective
                 (mevedel-directive-attempt-consumed-subdirectives attempt))
              (mevedel-directive-add-subdirective
               directive (mevedel-subdirective-copy subdirective))))))
      (setf
       (mevedel-directive-attempts directive)
       (cl-remove-if
        (lambda (attempt)
          (let ((checkpoint (mevedel-directive-attempt-checkpoint attempt)))
            (and (equal session-id (plist-get checkpoint :session-id))
                 (>= (or (plist-get checkpoint :turn) 0) target-turn))))
        (mevedel-directive-attempts directive))
       (mevedel-directive-discussion directive)
       (cl-remove-if
        (lambda (turn)
          (let ((checkpoint
                 (mevedel-directive-discussion-turn-checkpoint turn)))
            (and (equal session-id (plist-get checkpoint :session-id))
                 (>= (or (plist-get checkpoint :turn) 0) target-turn))))
        (mevedel-directive-discussion directive))
       (mevedel-directive-planning directive)
       (cl-remove-if
        (lambda (turn)
          (let ((checkpoint (plist-get turn :checkpoint)))
            (and (equal session-id (plist-get checkpoint :session-id))
                 (>= (or (plist-get checkpoint :turn) 0) target-turn))))
        (mevedel-directive-planning directive)))
      (when (and rewound-planned-attempt
                 (not
                  (equal
                   (mevedel-directive-attempt-plan-context
                    rewound-planned-attempt)
                   (list
                    :request (mevedel-directive-request directive)
                    :subdirectives
                    (mapcar
                     (lambda (subdirective)
                       (cons (mevedel-subdirective-id subdirective)
                             (mevedel-subdirective-request subdirective)))
                     (mevedel-directive-subdirectives directive))))))
        (setq rewound-planned-attempt nil))
      (if rewound-planned-attempt
          (setf (mevedel-directive-plan directive)
                (list :status 'accepted
                      :action (mevedel-directive-attempt-action
                               rewound-planned-attempt)
                      :proposal (mevedel-directive-attempt-plan
                                 rewound-planned-attempt)
                      :selection
                      (copy-tree
                       (mevedel-directive-attempt-plan-selection
                        rewound-planned-attempt))
                      :accepted-prompt
                      (mevedel-directive-attempt-request
                       rewound-planned-attempt)))
        (let* ((turns (mevedel-directive-planning directive))
               (latest (car (last turns)))
               (proposal-turn
                (car (last (cl-remove-if-not
                            (lambda (turn) (plist-get turn :proposal))
                            turns)))))
          (setf (mevedel-directive-plan directive)
                (and latest
                     (list :status (if (plist-get latest :proposal)
                                       'proposed
                                     'draft)
                           :action (plist-get latest :action)
                           :implementation-prompt
                           (plist-get latest :implementation-prompt)
                           :proposal (plist-get proposal-turn :proposal)
                           :selection selection)))))
      (mevedel-directive-recompute-state directive)))
  workspace)

(provide 'mevedel-directive)

;;; mevedel-directive.el ends here
