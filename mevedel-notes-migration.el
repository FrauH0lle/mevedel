;;; mevedel-notes-migration.el --- One-time workspace note conversion -*- lexical-binding: t -*-

;;; Commentary:

;; The only old workspace store reader.  Conversion preserves the original
;; bytes beside the store before replacing it under the instruction lock.
;; Session snapshots and exports are not migration inputs.

;;; Code:

(require 'cl-lib)
(require 'mevedel-persistence)
(require 'mevedel-session-control-fs)

(defun mevedel-notes-migration--anchor-shape-p (anchor)
  "Return non-nil when ANCHOR stores a well-formed source region.
Every durable anchor shape names a file and an ordered region, and
carries its evidence as a plist."
  (and (stringp (plist-get anchor :file))
       (natnump (plist-get anchor :start))
       (natnump (plist-get anchor :end))
       (<= (plist-get anchor :start) (plist-get anchor :end))
       (listp (plist-get anchor :evidence))))

(defun mevedel-notes-migration--read-subdirective (serialized base-directory)
  "Return a nested directive from SERIALIZED relative to BASE-DIRECTORY."
  (let ((id (plist-get serialized :id))
        (request (plist-get serialized :request))
        (anchor (copy-tree (plist-get serialized :anchor))))
    (unless (and (stringp id)
                 (stringp request)
                 (eq 'attached (plist-get anchor :state))
                 (mevedel-notes-migration--anchor-shape-p anchor)
                 (listp (plist-get anchor :properties)))
      (user-error "Malformed mevedel directive list"))
    (setq anchor
          (plist-put anchor :file
                     (expand-file-name (plist-get anchor :file)
                                       base-directory)))
    (list
     :id id :request request :anchor anchor)))

(defun mevedel-notes-migration--checkpoint-p (checkpoint)
  "Return non-nil when CHECKPOINT identifies one persisted session turn."
  (and (stringp (plist-get checkpoint :session-id))
       (natnump (plist-get checkpoint :turn))))

(defun mevedel-notes-migration--read-directive-anchor (serialized base-directory)
  "Validate and expand directive anchor SERIALIZED from BASE-DIRECTORY."
  (let ((anchor (copy-tree serialized)))
    (unless
        (pcase (plist-get anchor :state)
          ('attached (mevedel-notes-migration--anchor-shape-p anchor))
          ('detached
           (and (stringp (plist-get anchor :file))
                (natnump (plist-get anchor :position))
                (pcase (plist-get anchor :source-order)
                  (`(,(pred natnump) ,(pred natnump)) t))))
          ((or 'source-missing 'archived)
           (and (mevedel-notes-migration--anchor-shape-p anchor)
                (listp (plist-get anchor :properties)))))
      (user-error "Malformed mevedel directive list"))
    (when-let* ((file (plist-get anchor :file)))
      (setq anchor
            (plist-put anchor :file
                       (expand-file-name file base-directory))))
    anchor))

(defun mevedel-notes-migration--read-directive-attempt (attempt base-directory)
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
             (mevedel-notes-migration--checkpoint-p checkpoint))
      (user-error "Malformed mevedel directive list"))
    (list
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
               (mevedel-notes-migration--read-subdirective
                subdirective base-directory))
             consumed-subdirectives))))

(defun mevedel-notes-migration--read-directive-discussion-turn (turn)
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
             (mevedel-notes-migration--checkpoint-p checkpoint))
      (user-error "Malformed mevedel directive list"))
    (list
     :sequence sequence :directive-request directive-request
     :message message :request request :result result :outcome outcome
     :attempt-index attempt-index :checkpoint (copy-tree checkpoint))))

(defun mevedel-notes-migration--read-directive-plan (plan)
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

(defun mevedel-notes-migration--read-directive-planning-turn (turn)
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
               (mevedel-notes-migration--checkpoint-p
                (plist-get turn :checkpoint)))
    (user-error "Malformed mevedel directive list"))
  (copy-tree turn))

(defun mevedel-notes-migration--read-directives (serialized base-directory)
  "Return directive records from SERIALIZED relative to BASE-DIRECTORY."
  (let ((ids (make-hash-table :test #'equal)))
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
                (list
                 :id id :request request
                 :anchor (mevedel-notes-migration--read-directive-anchor
                          anchor base-directory)
                 :subdirectives
                 (mapcar (lambda (subdirective)
                           (mevedel-notes-migration--read-subdirective
                            subdirective base-directory))
                         subdirectives)
                 :session-id session-id
                 :planning-enabled planning-enabled
                 :skills (copy-tree skills)
                 :plan (mevedel-notes-migration--read-directive-plan plan)
                 :planning
                 (mapcar #'mevedel-notes-migration--read-directive-planning-turn
                         planning)
                 :attempts
                 (mapcar (lambda (attempt)
                           (mevedel-notes-migration--read-directive-attempt
                            attempt base-directory))
                         attempts)
                 :discussion
                 (mapcar #'mevedel-notes-migration--read-directive-discussion-turn
                         discussion))))
           (dolist (current-id
                    (cons id
                          (mapcar (lambda (child) (plist-get child :id))
                                  (plist-get directive :subdirectives))))
             (when (gethash current-id ids)
               (user-error "Malformed mevedel directive list"))
             (puthash current-id t ids))
           (let ((sequences (make-hash-table :test #'eql)))
             (dolist (sequence
                      (mapcar (lambda (turn) (plist-get turn :sequence))
                              (append (plist-get directive :attempts)
                                      (plist-get directive :discussion)
                                      (plist-get directive :planning))))
               (when (gethash sequence sequences)
                 (user-error "Malformed mevedel directive list"))
               (puthash sequence t sequences)))
           directive)))
     serialized)))

(defun mevedel-notes-migration--reparent (data)
  "Reparent DATA's surviving instructions around folded subdirectives."
  (let ((parents (make-hash-table :test #'equal)))
    (dolist (directive (plist-get data :directives))
      (dolist (child (plist-get directive :subdirectives))
        (puthash (plist-get child :id)
                 (or (plist-get (plist-get (plist-get child :anchor) :evidence)
                                :parent-uuid)
                     (plist-get directive :id))
                 parents)))
    (dolist (file (plist-get data :files))
      (dolist (instruction (plist-get (cdr file) :instructions))
        (let* ((anchor (plist-get instruction :anchor))
               (parent (plist-get anchor :parent-uuid))
               seen)
          (while (gethash parent parents)
            (when (member parent seen)
              (error "Cyclic subdirective anchors"))
            (push parent seen)
            (setq parent (gethash parent parents)))
          (when seen
            (setf (plist-get anchor :parent-uuid) parent
                  (plist-get instruction :anchor) anchor)))))))

(defun mevedel-notes-migration--convert (data)
  "Convert format-1 store DATA into the current note store."
  (setq data (copy-tree data))
  (mevedel-notes-migration--reparent data)
  ;; Archived, detached and source-missing records may have no presentation.
  ;; Reconstruct their note from the durable anchor without reading source.
  (dolist (directive (plist-get data :directives))
    (let* ((uuid (plist-get directive :id))
           (anchor (plist-get directive :anchor))
           (file (plist-get anchor :file))
           (cell (assoc file (plist-get data :files))))
      (unless cell
        (setq cell (cons file (list :instructions nil)))
        (push cell (plist-get data :files)))
      (unless (cl-find uuid (plist-get (cdr cell) :instructions)
                       :key (lambda (instruction)
                              (plist-get (plist-get instruction :properties)
                                         'mevedel-uuid))
                       :test #'equal)
        (let* ((properties (copy-tree (plist-get anchor :properties)))
               (position (max 1 (or (plist-get anchor :start)
                                    (plist-get anchor :position) 1))))
          (setq properties (plist-put properties 'mevedel-instruction t))
          (setq properties (plist-put properties 'mevedel-instruction-type 'directive))
          (setq properties (plist-put properties 'mevedel-uuid uuid))
          (push (list :overlay-start position
                      :overlay-end (max position (or (plist-get anchor :end) position))
                      :anchor (copy-tree (plist-get anchor :evidence))
                      :properties properties)
                (plist-get (cdr cell) :instructions))))))
  (dolist (file (plist-get data :files))
    (setcdr file
            (plist-put (cdr file) :instructions
                       (cl-remove-if
                        (lambda (instruction)
                          (eq (plist-get (plist-get instruction :properties)
                                         'mevedel-instruction-type)
                              'subdirective))
                        (plist-get (cdr file) :instructions))))
    (dolist (instruction (plist-get (cdr file) :instructions))
      (let* ((properties (plist-get instruction :properties))
             (type (plist-get properties 'mevedel-instruction-type))
             (directive (and (eq type 'directive)
                             (cl-find (plist-get properties 'mevedel-uuid)
                                      (plist-get data :directives)
                                      :key (lambda (record) (plist-get record :id))
                                      :test #'equal))))
        (when (or (eq type 'reference) directive)
          (setq properties
                (plist-put properties 'mevedel-note-text
                           (if directive
                               (mapconcat
                                #'identity
                                (cons (plist-get directive :request)
                                      (mapcar (lambda (child) (plist-get child :request))
                                              (plist-get directive :subdirectives)))
                                "\n\n")
                             (or (plist-get properties 'mevedel-commentary) ""))))
          (setq properties
                (plist-put properties 'mevedel-note-tags
                           (plist-get properties 'mevedel-reference-tags)))
          (setq properties (plist-put properties 'mevedel-note-author 'user))
          (setq properties (plist-put properties 'mevedel-note-state 'unsent))
          (when (eq (plist-get (plist-get directive :anchor) :state) 'archived)
            (setq properties (plist-put properties 'mevedel-note-state 'closed))
            (nconc instruction
                   (list :closed-reason 'archived :closed-at (float-time)
                         :open-state 'unsent
                         :closed-position
                         (when-let* ((content (plist-get (cdr file) :original-content)))
                           (with-temp-buffer
                             (insert content)
                             (cl-mapcan
                              (lambda (position)
                                (goto-char (max (point-min) (min (point-max) position)))
                                (list (line-number-at-pos) (- (point) (line-beginning-position))))
                              (list (plist-get instruction :overlay-start)
                                    (plist-get instruction :overlay-end)))))
                         :expiry-exempt t)))
          (when (and directive
                     (cl-some (lambda (key) (plist-get directive key))
                              '(:attempts :discussion :planning :plan)))
            (setq properties
                  (plist-put properties 'mevedel-note-legacy-history directive)))
          (setq properties (plist-put properties 'mevedel-instruction-type 'note))
          (setq properties (plist-put properties 'evaporate nil))
          (dolist (key '(mevedel-commentary mevedel-commentary-truncated
                        mevedel-reference-tags))
            (cl-remf properties key))
          (setf (plist-get instruction :properties) properties)))))
  (cl-remf data :directives)
  (let* ((ids (plist-get data :ids))
         (instructions (cl-mapcan
                        (lambda (file)
                          (copy-sequence (plist-get (cdr file) :instructions)))
                        (plist-get data :files)))
         (reserved (append (plist-get ids :used-ids) (plist-get ids :retired-ids)))
         (counter (apply #'max (or (plist-get ids :id-counter) 0)
                         (append reserved
                                 (delq nil (mapcar
                                            (lambda (instruction)
                                              (plist-get (plist-get instruction :properties)
                                                         'mevedel-id))
                                            instructions)))))
         (seen (make-hash-table :test #'eql)))
    (dolist (instruction instructions)
      (let* ((properties (plist-get instruction :properties))
             (id (plist-get properties 'mevedel-id)))
        (unless (and (integerp id) (> id 0) (not (gethash id seen)))
          (setq id (cl-incf counter)
                properties (plist-put properties 'mevedel-id id))
          (setf (plist-get instruction :properties) properties))
        (puthash id t seen)
        (cl-pushnew id reserved)))
    (setq ids (plist-put ids :id-counter counter))
    ;; The old allocator recycles retired numbers.  Reserve them instead so
    ;; historical chat numbers cannot silently acquire a different meaning.
    (setq ids (plist-put ids :retired-ids nil))
    (setq ids (plist-put ids :used-ids (sort (delete-dups reserved) #'<)))
    (setq data (plist-put data :ids ids)))
  (plist-put data :format mevedel-instruction-store-format-version))

(defun mevedel-notes-migrate-store (workspace bytes data)
  "Back up and convert WORKSPACE's format-1 DATA read as BYTES.
Return a current store observation.  The original store's hash and backup
bytes are checked under the ordinary instruction lock before replacement."
  (let* ((_ (mevedel-instruction-store--validate data workspace))
         (_ (unless (and (plist-member data :directives)
                         (proper-list-p (plist-get data :directives)))
              (error "Malformed legacy instruction store")))
         (_ (mevedel-notes-migration--read-directives
             (plist-get data :directives)
             (mevedel-instruction-store--root workspace)))
         (converted (mevedel-notes-migration--convert data))
         (_ (mevedel-instruction-store--validate converted workspace))
         (path (mevedel-instruction-store-path workspace))
         (backup (concat path ".format-1.bak"))
         (hash (secure-hash 'sha256 bytes))
         (new-bytes (mevedel-instruction-store--print converted))
         (results
          (mevedel-session-control-fs-run-program
           (list (list :op 'verify-hash :path path :content hash)
                 (list :op 'create :path backup :content bytes
                       :coding 'no-conversion :optional t)
                 (list :op 'verify-hash :path backup :content hash)
                 (list :op 'write :path path :content new-bytes
                       :coding 'no-conversion))
           (mevedel-instruction-store--lock-directory workspace))))
    (unless (eq (plist-get (car results) :status) 'ok)
      (error "Instruction store changed during note migration; retry opening it"))
    (when (eq (plist-get (nth 2 results) :status) 'mismatch)
      (error "Migration backup %s differs from %s; move the backup aside and retry"
             backup path))
    ;; An existing identical backup permits recovery after an interrupted run.
    ;; A different backup is never overwritten and prevents store replacement.
    (mapc #'mevedel-session-control-fs-program-value (nthcdr 2 results))
    (list :status 'ok :hash (secure-hash 'sha256 new-bytes) :data converted)))

(provide 'mevedel-notes-migration)
;;; mevedel-notes-migration.el ends here
