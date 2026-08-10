;;; mevedel-directive-persistence.el -- Directive record codec -*- lexical-binding: t -*-

;;; Commentary:

;; Serializes and validates workspace-owned directive activity records.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-directive'
(declare-function mevedel-directive-recompute-state
                  "mevedel-directive" (directive))

;; `mevedel-structs'
(declare-function mevedel-directive--create "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-attempt-action
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-capture
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-captured-at
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-consumed-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-covered-files
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-gaps
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-plan
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-plan-context
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-plan-selection
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-untracked-effects
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-discussion-turn-attempt-index
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-message
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning-enabled
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-subdirective-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--file-relative-name-or-absolute
                  "mevedel-utilities" (file directory))

(defun mevedel--serialize-directive-anchor (anchor base-directory)
  "Return serializable ANCHOR relative to BASE-DIRECTORY."
  (let ((copy (copy-tree anchor)))
    (when-let* ((file (plist-get copy :file))
                ((stringp base-directory)))
      (setq copy
            (plist-put copy :file
                       (mevedel--file-relative-name-or-absolute
                        file base-directory))))
    copy))

(defun mevedel--serialize-subdirective (subdirective base-directory)
  "Return SUBDIRECTIVE relative to BASE-DIRECTORY."
  (list :id (mevedel-subdirective-id subdirective)
        :request (substring-no-properties
                  (mevedel-subdirective-request subdirective))
        :anchor (mevedel--serialize-directive-anchor
                 (mevedel-subdirective-anchor subdirective)
                 base-directory)))

(defun mevedel--serialize-directive-plan (plan)
  "Return the durable fields from directive PLAN."
  (when plan
    (list :status (plist-get plan :status)
          :action (plist-get plan :action)
          :cancelled (and (plist-get plan :cancelled) t)
          :invalidated (and (plist-get plan :invalidated) t)
          :implementation-prompt (plist-get plan :implementation-prompt)
          :accepted-prompt (plist-get plan :accepted-prompt)
          :proposal (plist-get plan :proposal)
          :selection (copy-tree (plist-get plan :selection)))))

(defun mevedel--serialize-directives (workspace base-directory)
  "Return WORKSPACE directive records relative to BASE-DIRECTORY."
  (mapcar
   (lambda (directive)
     (list :id (mevedel-directive-id directive)
           :request (substring-no-properties
                     (mevedel-directive-request directive))
           :anchor (mevedel--serialize-directive-anchor
                    (mevedel-directive-anchor directive)
                    base-directory)
           :subdirectives
           (mapcar (lambda (subdirective)
                     (mevedel--serialize-subdirective
                      subdirective base-directory))
                   (mevedel-directive-subdirectives directive))
           :session-id (mevedel-directive-session-id directive)
           :planning-enabled
           (and (mevedel-directive-planning-enabled directive) t)
           :skills (copy-tree (mevedel-directive-skills directive))
           :plan (mevedel--serialize-directive-plan
                  (mevedel-directive-plan directive))
           :planning (copy-tree (mevedel-directive-planning directive))
           :attempts
           (mapcar
            (lambda (attempt)
              (list
               :sequence (mevedel-directive-attempt-sequence attempt)
               :action (mevedel-directive-attempt-action attempt)
               :directive-request
               (substring-no-properties
                (mevedel-directive-attempt-directive-request attempt))
               :request (substring-no-properties
                         (mevedel-directive-attempt-request attempt))
               :result (substring-no-properties
                        (mevedel-directive-attempt-result attempt))
               :outcome (mevedel-directive-attempt-outcome attempt)
               :patch (substring-no-properties
                       (mevedel-directive-attempt-patch attempt))
               :capture (mevedel-directive-attempt-capture attempt)
               :covered-files
               (mapcar
                (lambda (file)
                  (mevedel--file-relative-name-or-absolute
                   file base-directory))
                (mevedel-directive-attempt-covered-files attempt))
               :gaps
               (mapcar
                (lambda (gap)
                  (cons
                   (mevedel--file-relative-name-or-absolute
                    (car gap) base-directory)
                   (cdr gap)))
                (mevedel-directive-attempt-gaps attempt))
               :untracked-effects
               (copy-tree
                (mevedel-directive-attempt-untracked-effects attempt))
               :captured-at
               (mevedel-directive-attempt-captured-at attempt)
               :checkpoint
               (copy-tree (mevedel-directive-attempt-checkpoint attempt))
               :plan (mevedel-directive-attempt-plan attempt)
               :plan-context
               (copy-tree (mevedel-directive-attempt-plan-context attempt))
               :plan-selection
               (copy-tree (mevedel-directive-attempt-plan-selection attempt))
               :consumed-subdirectives
               (mapcar (lambda (subdirective)
                         (mevedel--serialize-subdirective
                          subdirective base-directory))
                       (mevedel-directive-attempt-consumed-subdirectives
                        attempt))))
            (mevedel-directive-attempts directive))
           :discussion
           (mapcar
            (lambda (turn)
              (list
               :sequence
               (mevedel-directive-discussion-turn-sequence turn)
               :directive-request
               (substring-no-properties
                (mevedel-directive-discussion-turn-directive-request turn))
               :message
               (substring-no-properties
                (mevedel-directive-discussion-turn-message turn))
               :request
               (substring-no-properties
                (mevedel-directive-discussion-turn-request turn))
               :result
               (substring-no-properties
                (mevedel-directive-discussion-turn-result turn))
               :outcome
               (mevedel-directive-discussion-turn-outcome turn)
               :attempt-index
               (mevedel-directive-discussion-turn-attempt-index turn)
               :checkpoint
               (copy-tree
                (mevedel-directive-discussion-turn-checkpoint turn))))
            (mevedel-directive-discussion directive))))
   (mevedel-workspace-directives workspace)))

(defun mevedel--deserialize-subdirective (serialized base-directory)
  "Return a nested directive from SERIALIZED relative to BASE-DIRECTORY."
  (let ((id (plist-get serialized :id))
        (request (plist-get serialized :request))
        (anchor (copy-tree (plist-get serialized :anchor))))
    (unless (and (stringp id)
                 (stringp request)
                 (eq 'attached (plist-get anchor :state))
                 (stringp (plist-get anchor :file))
                 (natnump (plist-get anchor :start))
                 (natnump (plist-get anchor :end))
                 (listp (plist-get anchor :evidence))
                 (listp (plist-get anchor :properties)))
      (user-error "Malformed mevedel directive list"))
    (setq anchor
          (plist-put anchor :file
                     (expand-file-name (plist-get anchor :file)
                                       base-directory)))
    (mevedel-subdirective--create
     :id id :request request :anchor anchor)))

(defun mevedel--directive-checkpoint-p (checkpoint)
  "Return non-nil when CHECKPOINT identifies one persisted session turn."
  (and (stringp (plist-get checkpoint :session-id))
       (natnump (plist-get checkpoint :turn))))

(defun mevedel--deserialize-directive-anchor (serialized base-directory)
  "Validate and expand directive anchor SERIALIZED from BASE-DIRECTORY."
  (let ((anchor (copy-tree serialized)))
    (unless
        (pcase (plist-get anchor :state)
          ('attached t)
          ('detached
           (and (stringp (plist-get anchor :file))
                (natnump (plist-get anchor :position))
                (pcase (plist-get anchor :source-order)
                  (`(,(pred natnump) ,(pred natnump)) t))))
          ((or 'source-missing 'archived)
           (and (stringp (plist-get anchor :file))
                (natnump (plist-get anchor :start))
                (natnump (plist-get anchor :end))
                (listp (plist-get anchor :evidence))
                (listp (plist-get anchor :properties)))))
      (user-error "Malformed mevedel directive list"))
    (when-let* ((file (plist-get anchor :file)))
      (setq anchor
            (plist-put anchor :file
                       (expand-file-name file base-directory))))
    anchor))

(defun mevedel--deserialize-directive-attempt (attempt base-directory)
  "Validate and restore ATTEMPT relative to BASE-DIRECTORY."
  (let ((sequence (plist-get attempt :sequence))
        (action (plist-get attempt :action))
        (directive-request (plist-get attempt :directive-request))
        (request (plist-get attempt :request))
        (result (plist-get attempt :result))
        (outcome (plist-get attempt :outcome))
        (patch (plist-get attempt :patch))
        (capture (plist-get attempt :capture))
        (covered-files (plist-get attempt :covered-files))
        (gaps (plist-get attempt :gaps))
        (untracked-effects (plist-get attempt :untracked-effects))
        (captured-at (plist-get attempt :captured-at))
        (checkpoint (plist-get attempt :checkpoint))
        (plan (plist-get attempt :plan))
        (plan-context (plist-get attempt :plan-context))
        (plan-selection (plist-get attempt :plan-selection))
        (consumed-subdirectives (plist-get attempt :consumed-subdirectives)))
    (unless
        (and (natnump sequence) (> sequence 0)
             (memq action '(implement request-changes retry))
             (stringp directive-request) (stringp request) (stringp result)
             (memq outcome '(success error aborted))
             (stringp patch) (memq capture '(complete incomplete))
             (listp covered-files) (cl-every #'stringp covered-files)
             (listp gaps)
             (cl-every (lambda (gap) (and (consp gap) (stringp (car gap))))
                       gaps)
             (plist-member attempt :untracked-effects)
             (listp untracked-effects)
             (cl-every (lambda (effect)
                         (and (consp effect)
                              (stringp (car effect))
                              (stringp (cdr effect))))
                       untracked-effects)
             (stringp captured-at)
             (plist-member attempt :plan)
             (or (null plan) (stringp plan))
             (plist-member attempt :plan-context)
             (or (null plan-context)
                 (and (stringp (plist-get plan-context :request))
                      (listp (plist-get plan-context :subdirectives))
                      (cl-every
                       (lambda (entry)
                         (and (consp entry) (stringp (car entry))
                              (stringp (cdr entry))))
                       (plist-get plan-context :subdirectives))))
             (plist-member attempt :plan-selection)
             (or (null plan-selection) (listp plan-selection))
             (plist-member attempt :consumed-subdirectives)
             (listp consumed-subdirectives)
             (mevedel--directive-checkpoint-p checkpoint))
      (user-error "Malformed mevedel directive list"))
    (mevedel-directive-attempt--create
     :sequence sequence :action action :directive-request directive-request
     :request request :result result :outcome outcome :patch patch
     :capture capture
     :covered-files (mapcar (lambda (file)
                              (expand-file-name file base-directory))
                            covered-files)
     :gaps (mapcar (lambda (gap)
                     (cons (expand-file-name (car gap) base-directory)
                           (cdr gap)))
                   gaps)
     :untracked-effects (copy-tree untracked-effects)
     :captured-at captured-at :checkpoint (copy-tree checkpoint)
     :plan plan :plan-context (copy-tree plan-context)
     :plan-selection (copy-tree plan-selection)
     :consumed-subdirectives
     (mapcar (lambda (subdirective)
               (mevedel--deserialize-subdirective
                subdirective base-directory))
             consumed-subdirectives))))

(defun mevedel--deserialize-directive-discussion-turn (turn)
  "Validate and restore one serialized directive discussion TURN."
  (let ((sequence (plist-get turn :sequence))
        (directive-request (plist-get turn :directive-request))
        (message (plist-get turn :message))
        (request (plist-get turn :request))
        (result (plist-get turn :result))
        (outcome (plist-get turn :outcome))
        (attempt-index (plist-get turn :attempt-index))
        (checkpoint (plist-get turn :checkpoint)))
    (unless
        (and (natnump sequence) (> sequence 0)
             (stringp directive-request) (stringp message)
             (stringp request) (stringp result)
             (memq outcome '(success error aborted))
             (or (null attempt-index)
                 (and (natnump attempt-index) (> attempt-index 0)))
             (mevedel--directive-checkpoint-p checkpoint))
      (user-error "Malformed mevedel directive list"))
    (mevedel-directive-discussion-turn--create
     :sequence sequence :directive-request directive-request
     :message message :request request :result result :outcome outcome
     :attempt-index attempt-index :checkpoint (copy-tree checkpoint))))

(defun mevedel--deserialize-directive-plan (plan)
  "Validate and copy one durable directive PLAN."
  (when plan
    (unless (and (memq (plist-get plan :status)
                       '(planning draft proposed accepted implementing settled))
                 (memq (plist-get plan :action)
                       '(implement implement-this request-changes retry))
                 (plist-member plan :cancelled)
                 (booleanp (plist-get plan :cancelled))
                 (plist-member plan :invalidated)
                 (booleanp (plist-get plan :invalidated))
                 (or (null (plist-get plan :implementation-prompt))
                     (stringp (plist-get plan :implementation-prompt)))
                 (or (null (plist-get plan :accepted-prompt))
                     (stringp (plist-get plan :accepted-prompt)))
                 (or (null (plist-get plan :proposal))
                     (stringp (plist-get plan :proposal)))
                 (or (null (plist-get plan :selection))
                     (listp (plist-get plan :selection))))
      (user-error "Malformed mevedel directive list"))
    (copy-tree plan)))

(defun mevedel--deserialize-directive-planning-turn (turn)
  "Validate and copy one directive planning TURN."
  (unless (and (natnump (plist-get turn :sequence))
               (> (plist-get turn :sequence) 0)
               (memq (plist-get turn :action)
                     '(implement implement-this request-changes retry))
               (stringp (plist-get turn :directive-request))
               (or (null (plist-get turn :message))
                   (stringp (plist-get turn :message)))
               (stringp (plist-get turn :implementation-prompt))
               (or (null (plist-get turn :proposal))
                   (stringp (plist-get turn :proposal)))
               (stringp (plist-get turn :request))
               (stringp (plist-get turn :result))
               (memq (plist-get turn :outcome) '(success error aborted))
               (mevedel--directive-checkpoint-p
                (plist-get turn :checkpoint)))
    (user-error "Malformed mevedel directive list"))
  (copy-tree turn))

(defun mevedel--deserialize-directives (serialized base-directory)
  "Return directive records from SERIALIZED relative to BASE-DIRECTORY."
  (mapcar
   (lambda (entry)
     (let ((id (plist-get entry :id))
           (request (plist-get entry :request))
           (anchor (plist-get entry :anchor))
           (subdirectives (plist-get entry :subdirectives))
           (session-id (plist-get entry :session-id))
           (planning-enabled (plist-get entry :planning-enabled))
           (skills (plist-get entry :skills))
           (plan (plist-get entry :plan))
           (planning (plist-get entry :planning))
           (attempts (plist-get entry :attempts))
           (discussion (plist-get entry :discussion)))
       (unless (and (stringp id) (stringp request)
                    (plist-member entry :subdirectives)
                    (listp subdirectives)
                    (plist-member entry :session-id)
                    (or (null session-id) (stringp session-id))
                    (plist-member entry :planning-enabled)
                    (booleanp planning-enabled)
                    (plist-member entry :skills)
                    (cl-every
                     (lambda (skill)
                       (and (stringp (plist-get skill :name))
                            (stringp (plist-get skill :source-file))))
                     skills)
                    (plist-member entry :plan)
                    (or (null plan) (listp plan))
                    (plist-member entry :planning)
                    (listp planning)
                    (plist-member entry :attempts)
                    (listp attempts)
                    (plist-member entry :discussion)
                    (listp discussion))
         (user-error "Malformed mevedel directive list"))
       (let ((directive
              (mevedel-directive--create
               :id id :request request
               :anchor (mevedel--deserialize-directive-anchor
                        anchor base-directory)
               :subdirectives
               (mapcar (lambda (subdirective)
                         (mevedel--deserialize-subdirective
                          subdirective base-directory))
                       subdirectives)
               :session-id session-id
               :planning-enabled planning-enabled
               :skills (copy-tree skills)
               :plan (mevedel--deserialize-directive-plan plan)
               :planning
               (mapcar #'mevedel--deserialize-directive-planning-turn
                       planning)
               :attempts
               (mapcar (lambda (attempt)
                         (mevedel--deserialize-directive-attempt
                          attempt base-directory))
                       attempts)
               :discussion
               (mapcar #'mevedel--deserialize-directive-discussion-turn
                       discussion))))
         (mevedel-directive-recompute-state directive)
         directive)))
   serialized))

(provide 'mevedel-directive-persistence)

;;; mevedel-directive-persistence.el ends here
