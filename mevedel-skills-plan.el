;;; mevedel-skills-plan.el -- Deterministic user skill invocation plans -*- lexical-binding: t -*-

;;; Commentary:

;; Builds a data-only invocation plan from one original, atomically bound user
;; prompt.  Planning classifies command and instruction roles, preserves every
;; recognized occurrence, and deduplicates execution by canonical skill source.
;; It does not prepare bodies, run hooks, mutate request state, or dispatch work.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-valid-p
                  "mevedel-mention-bindings" (text))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--format-render-data-block
                  "mevedel-pipeline" (render-data))

;; `mevedel-skills-core'
(declare-function mevedel-skill-context "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-source-file "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skills--source-key
                  "mevedel-skills-core" (source-file))

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--inline-attachment-reminder
                  "mevedel-skills-invoke" (attachment))
(declare-function mevedel-skills--scan-skill-tokens
                  "mevedel-skills-invoke" (text resolver &optional allow-root))
(declare-function mevedel-skills-prepare
                  "mevedel-skills-invoke"
                  (skill arguments callback &rest keys))
(declare-function mevedel-skills-resolve-user-mention-outcome
                  "mevedel-skills-invoke"
                  (text session start end name))


;;
;;; Plan data

(cl-defstruct
    (mevedel-skill-plan-occurrence
     (:constructor mevedel-skill-plan-occurrence--create))
  "One recognized skill token in the original user prompt."
  start
  end
  name
  source-key
  skill
  role
  message)

(cl-defstruct
    (mevedel-skill-plan-entry
     (:constructor mevedel-skill-plan-entry--create))
  "One canonical skill contribution selected by an invocation plan."
  name
  skill
  role)

(cl-defstruct
    (mevedel-skill-invocation-plan
     (:constructor mevedel-skill-invocation-plan--create))
  "A deterministic plan derived from one original user prompt."
  text
  occurrences
  commands
  instructions
  entries
  arguments
  arguments-start
  fork-p)


;;
;;; Planning

(defun mevedel-skills-plan--resolve (text session)
  "Resolve live skill tokens in TEXT against SESSION.
Return available tokens and unavailable bound occurrences separately.
Unknown and intentionally literal tokens remain unresolved."
  (unless (fboundp 'mevedel-skills--scan-skill-tokens)
    (require 'mevedel-skills-invoke))
  (let (available unavailable)
    (dolist
        (token
         (mevedel-skills--scan-skill-tokens
          text
          (lambda (name start end)
            (let ((outcome
                   (mevedel-skills-resolve-user-mention-outcome
                    text session start end name)))
              (when (or (eq (plist-get outcome :status) 'unavailable)
                        (plist-get outcome :skill))
                outcome)))
          t))
      (let* ((outcome (plist-get token :value))
             (skill (plist-get outcome :skill)))
        (if (eq (plist-get outcome :status) 'unavailable)
            (push
             (mevedel-skill-plan-occurrence--create
              :start (plist-get token :start)
              :end (plist-get token :end)
              :name (plist-get token :name)
              :role 'unavailable
              :message (plist-get outcome :message))
             unavailable)
          (setf (plist-get token :value) skill)
          (push token available))))
    (list :tokens (nreverse available)
          :unavailable (nreverse unavailable))))

(defun mevedel-skills-plan--command-layout (text tokens)
  "Return leading command layout for resolved TOKENS in TEXT.
The result contains command token starts, the raw argument start, and whether
the leading command forks."
  (let ((command-starts (make-hash-table :test #'eql))
        (command-sources (make-hash-table :test #'equal))
        arguments-start
        fork-p)
    (when (and tokens
               (equal (string-match-p "\\S-" text)
                      (plist-get (car tokens) :start)))
      (let* ((root (car tokens))
             (root-skill (plist-get root :value))
             (cursor (plist-get root :end))
             (remaining (cdr tokens))
             done)
        (puthash (plist-get root :start) t command-starts)
        (puthash (mevedel-skills--source-key
                  (mevedel-skill-source-file root-skill))
                 t command-sources)
        (setq fork-p (eq (mevedel-skill-context root-skill) 'fork))
        (if fork-p
            (setq arguments-start cursor)
          (while (not done)
            (let ((next-start (string-match-p "\\S-" text cursor)))
              (cond
               ((null next-start)
                (setq arguments-start (length text)
                      done t))
               ((and (<= (+ next-start 2) (length text))
                     (string= "--" (substring text next-start
                                               (+ next-start 2)))
                     (or (= (+ next-start 2) (length text))
                         (string-match-p
                          "[[:space:]]"
                          (char-to-string (aref text (+ next-start 2))))))
                (setq arguments-start (+ next-start 2)
                      done t))
               ((or (null remaining)
                    (/= next-start (plist-get (car remaining) :start)))
                (setq arguments-start next-start
                      done t))
               (t
                (let* ((token (car remaining))
                       (skill (plist-get token :value))
                       (source
                        (mevedel-skills--source-key
                         (mevedel-skill-source-file skill))))
                  (if (or (eq (mevedel-skill-context skill) 'fork)
                          (and (not (gethash source command-sources))
                               (>= (hash-table-count command-sources) 6)))
                      (setq arguments-start next-start
                            done t)
                    (puthash (plist-get token :start) t command-starts)
                    (puthash source t command-sources)
                    (setq cursor (plist-get token :end)
                          remaining (cdr remaining)))))))))))
    (list :command-starts command-starts
          :arguments-start arguments-start
          :fork-p fork-p)))

(defun mevedel-skills-plan--classify (text tokens)
  "Classify resolved TOKENS in TEXT as commands or instructions."
  (let* ((layout (mevedel-skills-plan--command-layout text tokens))
         (command-starts (plist-get layout :command-starts))
         (arguments-start (plist-get layout :arguments-start))
         (arguments-start
          (and arguments-start
               (or (string-match-p "\\S-" text arguments-start)
                   (length text))))
         (arguments-end
          (and arguments-start
               (string-match-p "[[:space:]]*\\'" text arguments-start)))
         (occurrences
          (mapcar
           (lambda (token)
             (let ((skill (plist-get token :value)))
               (mevedel-skill-plan-occurrence--create
                :start (plist-get token :start)
                :end (plist-get token :end)
                :name (plist-get token :name)
                :source-key
                (mevedel-skills--source-key
                 (mevedel-skill-source-file skill))
                :skill skill
                :role (if (gethash (plist-get token :start) command-starts)
                          'command
                        'instruction))))
           tokens)))
    (list :occurrences occurrences
          :arguments (and arguments-start
                          (substring text arguments-start arguments-end))
          :arguments-start arguments-start
          :fork-p (plist-get layout :fork-p))))

(defun mevedel-skills-plan--build (text classification &optional unavailable)
  "Build an invocation plan for TEXT from CLASSIFICATION and UNAVAILABLE."
  (let* ((occurrences (plist-get classification :occurrences))
         (commands
          (cl-remove-duplicates
           (cl-remove-if-not
            (lambda (item)
              (eq (mevedel-skill-plan-occurrence-role item) 'command))
            occurrences)
           :key #'mevedel-skill-plan-occurrence-source-key
           :test #'equal :from-end t))
         (command-sources
          (mapcar #'mevedel-skill-plan-occurrence-source-key commands))
         (instructions
          (cl-remove-duplicates
           (cl-remove-if
            (lambda (item)
              (or (eq (mevedel-skill-plan-occurrence-role item) 'command)
                  (member (mevedel-skill-plan-occurrence-source-key item)
                          command-sources)))
            occurrences)
           :key #'mevedel-skill-plan-occurrence-source-key
           :test #'equal :from-end t))
         (make-entry
          (lambda (occurrence role)
            (mevedel-skill-plan-entry--create
             :name (mevedel-skill-plan-occurrence-name occurrence)
             :skill (mevedel-skill-plan-occurrence-skill occurrence)
             :role role)))
         (command-entries
          (mapcar (lambda (item) (funcall make-entry item 'command))
                  commands))
         (instruction-entries
          (mapcar (lambda (item) (funcall make-entry item 'instruction))
                  instructions)))
    (mevedel-skill-invocation-plan--create
     :text (copy-sequence text)
     :occurrences
     (sort (append occurrences unavailable)
           (lambda (a b)
             (< (mevedel-skill-plan-occurrence-start a)
                (mevedel-skill-plan-occurrence-start b))))
     :commands command-entries
     :instructions instruction-entries
     :entries (append command-entries instruction-entries)
     :arguments (plist-get classification :arguments)
     :arguments-start (plist-get classification :arguments-start)
     :fork-p (plist-get classification :fork-p))))

(defun mevedel-skills-plan-user-input (text session)
  "Return a deterministic user skill invocation plan for TEXT and SESSION.

The plan preserves every recognized occurrence and exposes unique command and
instruction entries.
Commands are ordered before instructions.  Exact canonical sources are
deduplicated, with a command entry winning over instruction occurrences of the
same source.

Unknown names and escaped, quoted, or code-span syntax are not planned.
Malformed atomic binding data signals a `user-error'."
  (require 'mevedel-mention-bindings)
  (unless (mevedel-mention-bindings-valid-p text)
    (user-error "Malformed mention binding"))
  (let ((resolved (mevedel-skills-plan--resolve text session)))
    (mevedel-skills-plan--build
     text
     (mevedel-skills-plan--classify
      text (plist-get resolved :tokens))
     (plist-get resolved :unavailable))))

(defun mevedel-skills-plan-replace-instructions
    (text occurrences &optional source-offset)
  "Replace selected derived OCCURRENCES in TEXT with inert placeholders.

TEXT may be the complete original prompt or a source slice.  SOURCE-OFFSET is
the slice's zero-based position in the original prompt and defaults to zero.
Instruction and unavailable occurrences fully contained in the slice are
replaced.  The
function uses recorded extents and never scans TEXT."
  (let* ((offset (or source-offset 0))
         (limit (+ offset (length text)))
         replacements)
    (dolist (occurrence occurrences)
      (let ((start (mevedel-skill-plan-occurrence-start occurrence))
            (end (mevedel-skill-plan-occurrence-end occurrence)))
        (when (and (memq (mevedel-skill-plan-occurrence-role occurrence)
                         '(instruction unavailable))
                   (>= start offset)
                   (<= end limit))
          (push occurrence replacements))))
    (with-temp-buffer
      (insert text)
      (dolist (occurrence
               (sort replacements
                     (lambda (a b)
                       (> (mevedel-skill-plan-occurrence-start a)
                          (mevedel-skill-plan-occurrence-start b)))))
        (let ((start (- (mevedel-skill-plan-occurrence-start occurrence)
                        offset))
              (end (- (mevedel-skill-plan-occurrence-end occurrence)
                      offset)))
          (delete-region (1+ start) (1+ end))
          (goto-char (1+ start))
          (insert
           (format "[skill:%s -- %s]"
                   (mevedel-skill-plan-occurrence-name occurrence)
                   (if (eq (mevedel-skill-plan-occurrence-role occurrence)
                           'unavailable)
                       "unavailable"
                     "attached")))))
      (buffer-string))))

(defun mevedel-skills-plan--aggregate-prepared (pairs)
  "Aggregate ordered prepared entry PAIRS into prompt components and policy."
  (let ((command-count
         (cl-count-if
          (lambda (pair)
            (eq (mevedel-skill-plan-entry-role (plist-get pair :entry))
                'command))
          pairs))
        command-bodies
        instruction-reminders
        permission-rules
        hook-rules
        invoked-skills
        hook-contexts
        hook-audits
        single-command-context)
    (dolist (pair pairs)
      (let* ((entry (plist-get pair :entry))
             (outcome (plist-get pair :outcome))
             (context (plist-get outcome :request-context)))
        (setq invoked-skills
              (append invoked-skills (plist-get context :invoked-skills))
              hook-contexts
              (append hook-contexts
                      (and (plist-get outcome :hook-context)
                           (list (plist-get outcome :hook-context))))
              hook-audits
              (append hook-audits (plist-get outcome :hook-audits)))
        (if (eq (mevedel-skill-plan-entry-role entry) 'command)
            (progn
              (push (or (plist-get outcome :body) "") command-bodies)
              (setq permission-rules
                    (append permission-rules
                            (plist-get context :permission-rules))
                    hook-rules
                    (append hook-rules (plist-get context :hook-rules)))
              (when (= command-count 1)
                (setq single-command-context context)))
          (push
           (mevedel-skills--inline-attachment-reminder
            (list :name (mevedel-skill-plan-entry-name entry)
                  :body (or (plist-get outcome :body) "")))
           instruction-reminders))))
    (list :command-count command-count
          :command-bodies (nreverse command-bodies)
          :instruction-reminders (nreverse instruction-reminders)
          :permission-rules permission-rules
          :hook-rules hook-rules
          :invoked-skills invoked-skills
          :hook-contexts hook-contexts
          :hook-audits hook-audits
          :single-command-context single-command-context)))

(defun mevedel-skills-plan--prepared-outcome (plan pairs)
  "Return the complete successful preparation outcome for PLAN and PAIRS."
  (let* ((aggregate (mevedel-skills-plan--aggregate-prepared pairs))
         (command-bodies (plist-get aggregate :command-bodies))
         (reminder-blocks
          (mapcar
           (lambda (reminder)
             (format "<system-reminder>\n%s\n</system-reminder>" reminder))
           (plist-get aggregate :instruction-reminders)))
         (main-input
          (if command-bodies
              (mapconcat #'identity command-bodies "\n\n")
            (mevedel-skills-plan-replace-instructions
             (mevedel-skill-invocation-plan-text plan)
             (mevedel-skill-invocation-plan-occurrences plan))))
         (model-input
          (mapconcat
           #'identity
           (append reminder-blocks (list main-input))
           "\n\n"))
         (request-context
          (list :permission-rules (plist-get aggregate :permission-rules)
                :hook-rules (plist-get aggregate :hook-rules)
                :invoked-skills (plist-get aggregate :invoked-skills))))
    (when (= (plist-get aggregate :command-count) 1)
      (let ((command-context
             (plist-get aggregate :single-command-context)))
        (setq request-context
              (plist-put request-context :model
                         (plist-get command-context :model))
              request-context
              (plist-put request-context :effort
                         (plist-get command-context :effort)))))
    (list :status 'ok
          :prepared-entries pairs
          :model-input model-input
          :warnings
          (mapcar #'mevedel-skill-plan-occurrence-message
                  (cl-remove-if-not
                   (lambda (occurrence)
                     (eq (mevedel-skill-plan-occurrence-role occurrence)
                         'unavailable))
                   (mevedel-skill-invocation-plan-occurrences plan)))
          :request-context request-context
          :hook-context
          (when-let* ((contexts (plist-get aggregate :hook-contexts)))
            (mapconcat #'identity contexts "\n\n"))
          :hook-audits (plist-get aggregate :hook-audits))))

(defun mevedel-skills-plan-prepare (plan callback &optional cancelled-p)
  "Prepare PLAN sequentially, then call CALLBACK with one complete outcome.

Unique commands prepare first, followed by unique instructions.  Command
arguments are PLAN's shared argument slice after recorded instruction
occurrences have become inert placeholders.  Instruction preparation always
receives empty arguments.  CANCELLED-P, when non-nil, is a zero-argument
predicate checked before and after every asynchronous preparation.  Once the
transaction fails or is cancelled, later callbacks have no effect."
  (let* ((entries (mevedel-skill-invocation-plan-entries plan))
         (command-count
          (cl-count-if
           (lambda (entry)
             (eq (mevedel-skill-plan-entry-role entry) 'command))
           entries))
        (command-arguments
         (if-let* ((arguments
                    (mevedel-skill-invocation-plan-arguments plan)))
             (mevedel-skills-plan-replace-instructions
              arguments
              (mevedel-skill-invocation-plan-occurrences plan)
              (mevedel-skill-invocation-plan-arguments-start plan))
           ""))
        prepared
        settled)
    (cl-labels
        ((cancelled ()
           (and cancelled-p (funcall cancelled-p)))
         (finish (outcome)
           (unless settled
             (setq settled t)
             (funcall callback outcome)))
         (cancel ()
           (finish '(:status error :reason cancelled
                     :message "Skill plan preparation was cancelled")))
         (prepare-next ()
           (cond
            ((cancelled) (cancel))
            ((null entries)
             (finish
              (mevedel-skills-plan--prepared-outcome
               plan (nreverse prepared))))
            (t
             (let* ((entry (pop entries))
                    (role (mevedel-skill-plan-entry-role entry))
                    (arguments (if (eq role 'command)
                                   command-arguments
                                 "")))
               (mevedel-skills-prepare
                (mevedel-skill-plan-entry-skill entry)
                arguments
                (lambda (outcome)
                  (unless settled
                    (cond
                     ((cancelled) (cancel))
                     ((not (eq (plist-get outcome :status) 'ok))
                      (finish
                       (append outcome
                               (list :entry entry
                                     :name
                                     (mevedel-skill-plan-entry-name entry)))))
                     (t
                      (push (list :entry entry :outcome outcome) prepared)
                      (prepare-next)))))
                :role role
                :origin 'user
                :policy-owner-p (and (eq role 'command)
                                     (= command-count 1))))))))
      (prepare-next))))

(defun mevedel-skills-plan-render-data (plan)
  "Return hidden inline-skill render data preserving PLAN's original text."
  (unless (fboundp 'mevedel-pipeline--format-render-data-block)
    (require 'mevedel-pipeline))
  (propertize
   (mevedel-pipeline--format-render-data-block
    (list :kind 'inline-skill
          :display-text (mevedel-skill-invocation-plan-text plan)))
   'gptel 'ignore))

(provide 'mevedel-skills-plan)
;;; mevedel-skills-plan.el ends here
