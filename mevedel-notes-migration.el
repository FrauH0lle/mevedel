;;; mevedel-notes-migration.el --- One-time workspace note conversion -*- lexical-binding: t -*-

;;; Commentary:

;; The only old workspace store reader.  Conversion preserves the original
;; bytes beside the store before replacing it under the instruction lock.
;; Session snapshots and exports are not migration inputs.

;;; Code:

(require 'cl-lib)
(require 'mevedel-persistence)
(require 'mevedel-session-control-fs)

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
                         :open-state 'unsent :closed-position '(1 0 1 0)
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
  (setq data (plist-put data :directives nil))
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
