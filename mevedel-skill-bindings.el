;;; mevedel-skill-bindings.el --- Atomic skill mention bindings -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the skill-specific contract layered on generic atomic mention text
;; properties: schema validation, exact token extents, persistence sanitation,
;; and range-aware invalidation after composer edits.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-mentions'
(declare-function mevedel-mentions-binding-ranges
                  "mevedel-mentions" (text))
(declare-function mevedel-mentions-copy-bound-text
                  "mevedel-mentions" (text))

;; `mevedel-skills-core'
(declare-function mevedel-session-get-skill
                  "mevedel-skills-core" (session name))
(declare-function mevedel-session-get-skill-by-source
                  "mevedel-skills-core" (session source-file))
(declare-function mevedel-skill-user-invocable-p
                  "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skills--skill-enabled-p
                  "mevedel-skills-core" (skill))


;;
;;; Lexical tokens

(defun mevedel-skill-bindings--candidate-names (raw-name)
  "Return lexical skill-name candidates for RAW-NAME in priority order."
  (let ((name raw-name)
        names)
    (while (> (length name) 0)
      (push name names)
      (let ((last (aref name (1- (length name)))))
        (setq name (if (memq last '(?. ?: ?-))
                       (substring name 0 -1)
                     ""))))
    (nreverse names)))

(defun mevedel-skill-bindings-token-start-p (text start)
  "Return non-nil when START begins a lexical `$skill' token in TEXT."
  (and (>= start 0)
       (< start (length text))
       (eq (aref text start) ?$)
       (or (= start 0)
           (not (string-match-p
                 "[[:alnum:]_]"
                 (char-to-string (aref text (1- start))))))))

(defun mevedel-skill-bindings-token-occurrences (text)
  "Return lexical `$skill' occurrences in TEXT in source order.
Each occurrence contains `:start' and `:candidates'.
Candidate names retain an exact punctuation-bearing name first, then
only trim trailing punctuation that is ambiguous with prose."
  (let ((position 0)
        occurrences)
    (while (string-match "\\$\\([A-Za-z0-9_.:-]+\\)" text position)
      (let ((start (match-beginning 0)))
        (when (mevedel-skill-bindings-token-start-p text start)
          (push (list :start start
                      :candidates
                      (mevedel-skill-bindings--candidate-names
                       (match-string 1 text)))
                occurrences)))
      (setq position (match-end 0)))
    (nreverse occurrences)))


;;
;;; Validation

(defun mevedel-skill-bindings--schema-valid-p (binding)
  "Return non-nil when BINDING has exactly the atomic skill schema."
  (and (proper-list-p binding)
       (= (length binding) 6)
       (plist-member binding :kind)
       (plist-member binding :name)
       (plist-member binding :source-file)))

(defun mevedel-skill-bindings--range-valid-p
    (text start end binding &optional occurrences)
  "Return non-nil when BINDING validly describes TEXT from START to END."
  (let* ((schema-valid
          (mevedel-skill-bindings--schema-valid-p binding))
         (name (and schema-valid (plist-get binding :name)))
         (source-file
          (and schema-valid (plist-get binding :source-file)))
         (occurrence
          (and schema-valid
               (cl-find start
                        (or occurrences
                            (mevedel-skill-bindings-token-occurrences text))
                        :key (lambda (item) (plist-get item :start))))))
    (and schema-valid
         (eq (plist-get binding :kind) 'skill)
         (stringp name)
         (stringp source-file)
         (file-name-absolute-p source-file)
         occurrence
         (member name (plist-get occurrence :candidates))
         (= end (+ start 1 (length name))))))

(defun mevedel-skill-bindings-valid-p (text)
  "Return non-nil when every atomic skill binding in TEXT is valid."
  (and (stringp text)
       (let ((occurrences
              (mevedel-skill-bindings-token-occurrences text)))
         (cl-every
          (lambda (range)
            (mevedel-skill-bindings--range-valid-p
             text
             (plist-get range :start)
             (plist-get range :end)
             (plist-get range :binding)
             occurrences))
          (mevedel-mentions-binding-ranges text)))))

(defun mevedel-skill-bindings-sanitize (text)
  "Return bound TEXT with invalid atomic skill binding runs removed."
  (let* ((copy (mevedel-mentions-copy-bound-text text))
         (occurrences
          (mevedel-skill-bindings-token-occurrences copy)))
    (dolist (range (mevedel-mentions-binding-ranges copy))
      (unless (mevedel-skill-bindings--range-valid-p
               copy
               (plist-get range :start)
               (plist-get range :end)
               (plist-get range :binding)
               occurrences)
        (remove-text-properties
         (plist-get range :start) (plist-get range :end)
         '(mevedel-mention-binding nil rear-nonsticky nil)
         copy)))
    copy))

(defun mevedel-skill-bindings-starting-at (text start)
  "Return the valid atomic skill binding starting at START in TEXT."
  (let* ((binding (and (< start (length text))
                       (get-text-property
                        start 'mevedel-mention-binding text)))
         (end (and binding
                   (or (next-single-property-change
                        start 'mevedel-mention-binding text (length text))
                       (length text)))))
    (and binding
         (or (= start 0)
             (not (get-text-property
                   (1- start) 'mevedel-mention-binding text)))
         (mevedel-skill-bindings--range-valid-p text start end binding)
         binding)))

(defun mevedel-skill-bindings-at (text start end name)
  "Return TEXT's exact skill binding for NAME from START to END, or nil."
  (let ((binding (mevedel-skill-bindings-starting-at text start)))
    (and binding
         (= end (or (next-single-property-change
                     start 'mevedel-mention-binding text (length text))
                    (length text)))
         (equal name (plist-get binding :name))
         binding)))

(defun mevedel-skill-bindings-resolve-outcome
    (text session start end name)
  "Return a structured resolution outcome for NAME at START..END in TEXT.
Success is `(:status ok :skill SKILL)'.  An unavailable, disabled, or no
longer invocable atomic binding returns `(:status error :reason REASON
:message MESSAGE)'.  Unbound unknown names are successful with a nil skill."
  (let* ((bound (mevedel-skill-bindings-starting-at text start))
         (binding (and bound
                       (mevedel-skill-bindings-at text start end name)))
         (source-file (plist-get binding :source-file))
         (skill
          (cond
           (binding
            (mevedel-session-get-skill-by-source session source-file))
           (bound nil)
           (t (mevedel-session-get-skill session name)))))
    (cond
     ((and binding (null skill))
      (list :status 'error :reason 'bound-source
            :message
            (format "Bound skill $%s is unavailable at %s" name source-file)))
     ((and binding skill (not (mevedel-skills--skill-enabled-p skill)))
      (list :status 'error :reason 'disabled
            :message
            (format "Skill $%s is disabled; enable it or escape it as \\$%s"
                    name name)))
     ((and binding skill (not (mevedel-skill-user-invocable-p skill)))
      (list :status 'error :reason 'not-user-invocable
            :message
            (format "Skill $%s is no longer user-invocable; escape it as \\$%s"
                    name name)))
     (t (list :status 'ok :skill skill)))))

(defun mevedel-skill-bindings-resolve (text session start end name)
  "Resolve the skill token NAME at START..END in TEXT for SESSION.
An atomic binding resolves only through its exact source and signals a
`user-error' when that source is unavailable or no longer invocable."
  (let ((outcome (mevedel-skill-bindings-resolve-outcome
                  text session start end name)))
    (if (eq (plist-get outcome :status) 'ok)
        (plist-get outcome :skill)
      (user-error "%s" (plist-get outcome :message)))))


;;
;;; Live editing

(defun mevedel-skill-bindings--buffer-run-at (position minimum maximum)
  "Return the binding run at POSITION between MINIMUM and MAXIMUM."
  (when (and (>= position minimum) (< position maximum))
    (when-let* ((binding (get-text-property
                          position 'mevedel-mention-binding)))
      (list :start
            (or (previous-single-property-change
                 (1+ position) 'mevedel-mention-binding nil minimum)
                minimum)
            :end
            (or (next-single-property-change
                 position 'mevedel-mention-binding nil maximum)
                maximum)
            :binding binding))))

(defun mevedel-skill-bindings-invalidate-edit
    (start end minimum maximum)
  "Invalidate skill bindings touched by an edit from START to END.
MINIMUM and MAXIMUM delimit the editable composer.  Only property runs
adjacent to the changed range are inspected."
  (let ((before start)
        runs)
    (while (and (> before minimum)
                (memq (char-before before) '(?. ?: ?-)))
      (setq before (1- before)))
    (dolist (position (delete-dups
                       (list (1- before)
                             (1- start) start (1- end) end)))
      (when-let* ((run (mevedel-skill-bindings--buffer-run-at
                        position minimum maximum)))
        (unless (cl-find run runs :test #'equal)
          (push run runs))))
    (dolist (run runs)
      (let ((run-start (plist-get run :start))
            (run-end (plist-get run :end)))
        (unless (mevedel-skill-bindings--range-valid-p
                 (buffer-substring minimum maximum)
                 (- run-start minimum)
                 (- run-end minimum)
                 (plist-get run :binding))
          (remove-text-properties
           run-start run-end
           '(mevedel-mention-binding nil rear-nonsticky nil)))))))

(provide 'mevedel-skill-bindings)
;;; mevedel-skill-bindings.el ends here
