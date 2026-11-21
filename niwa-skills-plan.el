;;; niwa-skills-plan.el -- Deterministic user skill invocation plans -*- lexical-binding: t -*-

;;; Commentary:

;; Builds a data-only invocation plan from one original, atomically bound user
;; prompt.  Planning classifies command and instruction roles, preserves every
;; recognized occurrence, and deduplicates execution by canonical skill source.
;; It does not prepare bodies, run hooks, mutate request state, or dispatch work.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-valid-p
                  "niwa-mention-bindings" (text))
(autoload 'niwa-mention-bindings-valid-p "niwa-mention-bindings")

;; `niwa-reminders'
(declare-function niwa-reminders-format-block
                  "niwa-reminders" (content))
(autoload 'niwa-reminders-format-block "niwa-reminders")

;; `niwa-skills-core'
(declare-function niwa-skill-context "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-name "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-source-file "niwa-skills-core" (cl-x) t)
(declare-function niwa-skills-intersect-ptc-primitives
                  "niwa-skills-core" (left right))
(declare-function niwa-skills-source-key
                  "niwa-skills-core" (source-file))
(autoload 'niwa-skill-context "niwa-skills-core")
(autoload 'niwa-skill-name "niwa-skills-core")
(autoload 'niwa-skill-source-file "niwa-skills-core")
(autoload 'niwa-skills-intersect-ptc-primitives "niwa-skills-core")
(autoload 'niwa-skills-source-key "niwa-skills-core")

;; `niwa-skills-input'
(declare-function niwa-skills-input-resolve-mention
                  "niwa-skills-input"
                  (text session start end name))
(declare-function niwa-skills-input-scan-tokens
                  "niwa-skills-input" (text resolver &optional allow-root))
(autoload 'niwa-skills-input-resolve-mention "niwa-skills-input")
(autoload 'niwa-skills-input-scan-tokens "niwa-skills-input")

;; `niwa-skills-invoke'
(declare-function niwa-skills-format-attachment
                  "niwa-skills-invoke" (attachment))
(declare-function niwa-skills-format-body
                  "niwa-skills-invoke" (skill body))
(declare-function niwa-skills-prepare-many
                  "niwa-skills-invoke" (roots callback &rest keys))
(autoload 'niwa-skills-format-attachment "niwa-skills-invoke")
(autoload 'niwa-skills-format-body "niwa-skills-invoke")
(autoload 'niwa-skills-prepare-many "niwa-skills-invoke")

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-format
                  "niwa-tool-render-data" (render-data &optional tool-use-id))
(autoload 'niwa-tool-render-data-format "niwa-tool-render-data")


;;
;;; Plan data

(cl-defstruct
    (niwa-skill-plan-occurrence
     (:constructor niwa-skill-plan-occurrence--create))
  "One recognized skill token in the original user prompt."
  start
  end
  name
  source-key
  skill
  role
  message)

(cl-defstruct
    (niwa-skill-plan-entry
     (:constructor niwa-skill-plan-entry--create))
  "One canonical skill contribution selected by an invocation plan."
  name
  skill
  role)

(cl-defstruct
    (niwa-skill-invocation-plan
     (:constructor niwa-skill-invocation-plan--create))
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

(defun niwa-skills-plan--resolve (text session)
  "Resolve live skill tokens in TEXT against SESSION.
Return available tokens and unavailable bound occurrences separately.
Unknown and intentionally literal tokens remain unresolved."
  (let (available unavailable)
    (dolist
        (token
         (niwa-skills-input-scan-tokens
          text
          (lambda (name start end)
            (let ((outcome
                   (niwa-skills-input-resolve-mention
                    text session start end name)))
              (when (or (eq (plist-get outcome :status) 'unavailable)
                        (plist-get outcome :skill))
                outcome)))
          t))
      (let* ((outcome (plist-get token :value))
             (skill (plist-get outcome :skill)))
        (if (eq (plist-get outcome :status) 'unavailable)
            (push
             (niwa-skill-plan-occurrence--create
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

(defun niwa-skills-plan--command-layout (text tokens)
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
             (remaining (cdr tokens)))
        (puthash (plist-get root :start) t command-starts)
        (puthash (niwa-skills-source-key
                  (niwa-skill-source-file root-skill))
                 t command-sources)
        (setq fork-p (eq (niwa-skill-context root-skill) 'fork))
        (if fork-p
            (setq arguments-start cursor)
          (while (not arguments-start)
            (let ((next-start (string-match-p "\\S-" text cursor)))
              (cond
               ((null next-start)
                (setq arguments-start (length text)))
               ((and (<= (+ next-start 2) (length text))
                     (string= "--" (substring text next-start
                                               (+ next-start 2)))
                     (or (= (+ next-start 2) (length text))
                         (string-match-p
                          "[[:space:]]"
                          (char-to-string (aref text (+ next-start 2))))))
                (setq arguments-start (+ next-start 2)))
               ((or (null remaining)
                    (/= next-start (plist-get (car remaining) :start)))
                (setq arguments-start next-start))
               (t
                (let* ((token (car remaining))
                       (skill (plist-get token :value))
                       (source
                        (niwa-skills-source-key
                         (niwa-skill-source-file skill))))
                  (if (or (eq (niwa-skill-context skill) 'fork)
                          (and (not (gethash source command-sources))
                               (>= (hash-table-count command-sources) 6)))
                      (setq arguments-start next-start)
                    (puthash (plist-get token :start) t command-starts)
                    (puthash source t command-sources)
                    (setq cursor (plist-get token :end)
                          remaining (cdr remaining)))))))))))
    (list :command-starts command-starts
          :arguments-start arguments-start
          :fork-p fork-p)))

(defun niwa-skills-plan--classify (text tokens)
  "Classify resolved TOKENS in TEXT as commands or instructions."
  (let* ((layout (niwa-skills-plan--command-layout text tokens))
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
               (niwa-skill-plan-occurrence--create
                :start (plist-get token :start)
                :end (plist-get token :end)
                :name (plist-get token :name)
                :source-key
                (niwa-skills-source-key
                 (niwa-skill-source-file skill))
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

(defun niwa-skills-plan--build (text classification &optional unavailable)
  "Build an invocation plan for TEXT from CLASSIFICATION and UNAVAILABLE."
  (let* ((occurrences (plist-get classification :occurrences))
         (commands
          (cl-remove-duplicates
           (cl-remove-if-not
            (lambda (item)
              (eq (niwa-skill-plan-occurrence-role item) 'command))
            occurrences)
           :key #'niwa-skill-plan-occurrence-source-key
           :test #'equal :from-end t))
         (command-sources
          (mapcar #'niwa-skill-plan-occurrence-source-key commands))
         (instructions
          (cl-remove-duplicates
           (cl-remove-if
            (lambda (item)
              (or (eq (niwa-skill-plan-occurrence-role item) 'command)
                  (member (niwa-skill-plan-occurrence-source-key item)
                          command-sources)))
            occurrences)
           :key #'niwa-skill-plan-occurrence-source-key
           :test #'equal :from-end t))
         (make-entry
          (lambda (occurrence role)
            (niwa-skill-plan-entry--create
             :name (niwa-skill-plan-occurrence-name occurrence)
             :skill (niwa-skill-plan-occurrence-skill occurrence)
             :role role)))
         (command-entries
          (mapcar (lambda (item) (funcall make-entry item 'command))
                  commands))
         (instruction-entries
          (mapcar (lambda (item) (funcall make-entry item 'instruction))
                  instructions)))
    (niwa-skill-invocation-plan--create
     :text (copy-sequence text)
     :occurrences
     (sort (append occurrences unavailable)
           (lambda (a b)
             (< (niwa-skill-plan-occurrence-start a)
                (niwa-skill-plan-occurrence-start b))))
     :commands command-entries
     :instructions instruction-entries
     :entries (append command-entries instruction-entries)
     :arguments (plist-get classification :arguments)
     :arguments-start (plist-get classification :arguments-start)
     :fork-p (plist-get classification :fork-p))))

(defun niwa-skills-plan-user-input (text session &optional selected-skills)
  "Return a deterministic user skill invocation plan for TEXT and SESSION.

The plan preserves every recognized occurrence and exposes unique command and
instruction entries.
Commands are ordered before instructions.  Exact canonical sources are
deduplicated, with a command entry winning over instruction occurrences of the
same source.

Unknown names and escaped, quoted, or code-span syntax are not planned.
Malformed atomic binding data signals a `user-error'.
When SELECTED-SKILLS is non-nil, plan only those explicit names with TEXT
as their literal arguments; tokens inside TEXT carry no skill authority."
  (unless (niwa-mention-bindings-valid-p text)
    (user-error "Malformed mention binding"))
  (let* ((prefix (when selected-skills
                   (concat (mapconcat (lambda (name) (concat "$" name))
                                      selected-skills " ") " ")))
         (resolved (niwa-skills-plan--resolve (or prefix text) session))
         (tokens (plist-get resolved :tokens))
         (input (if prefix (concat prefix text) text)))
    (when (and selected-skills
               (/= (length selected-skills) (length tokens)))
      (user-error "A selected skill is no longer available"))
    (let ((classification (niwa-skills-plan--classify input tokens)))
      (when prefix
        (setq classification (plist-put classification :arguments text))
        (setq classification
              (plist-put classification :arguments-start (length prefix))))
      (niwa-skills-plan--build
       input classification (plist-get resolved :unavailable)))))

(defun niwa-skills-plan-replace-instructions
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
      (let ((start (niwa-skill-plan-occurrence-start occurrence))
            (end (niwa-skill-plan-occurrence-end occurrence)))
        (when (and (memq (niwa-skill-plan-occurrence-role occurrence)
                         '(instruction unavailable))
                   (>= start offset)
                   (<= end limit))
          (push occurrence replacements))))
    (with-temp-buffer
      (insert text)
      (dolist (occurrence
               (sort replacements
                     (lambda (a b)
                       (> (niwa-skill-plan-occurrence-start a)
                          (niwa-skill-plan-occurrence-start b)))))
        (let ((start (- (niwa-skill-plan-occurrence-start occurrence)
                        offset))
              (end (- (niwa-skill-plan-occurrence-end occurrence)
                      offset)))
          (delete-region (1+ start) (1+ end))
          (goto-char (1+ start))
          (insert
           (format "[skill:%s -- %s]"
                   (niwa-skill-plan-occurrence-name occurrence)
                   (if (eq (niwa-skill-plan-occurrence-role occurrence)
                           'unavailable)
                       "unavailable"
                     "attached")))))
      (buffer-string))))

(defun niwa-skills-plan--aggregate-prepared (pairs)
  "Aggregate ordered prepared entry PAIRS into prompt components and policy."
  (let ((command-count
         (cl-count-if
          (lambda (pair)
            (eq (niwa-skill-plan-entry-role (plist-get pair :entry))
                'command))
          pairs))
        command-bodies
        command-inputs
        instruction-attachments
        permission-rules
        (ptc-primitives :unrestricted)
        hook-rules
        (seen-attachments (make-hash-table :test #'equal))
        single-command-context)
    (dolist (pair pairs)
      (let* ((entry (plist-get pair :entry))
             (outcome (plist-get pair :outcome))
             (context (plist-get outcome :request-context)))
        (dolist (attachment (plist-get outcome :required-attachments))
          (let ((key
                 (cons
                  (or (niwa-skills-source-key
                       (plist-get attachment :source-file))
                      (plist-get attachment :skill))
                  (or (plist-get attachment :arguments) ""))))
            (unless (gethash key seen-attachments)
              (puthash key t seen-attachments)
              (push attachment instruction-attachments))))
        (if (eq (niwa-skill-plan-entry-role entry) 'command)
            (progn
              (push (or (plist-get outcome :body) "") command-bodies)
              (push (niwa-skills-format-body
                     (niwa-skill-plan-entry-skill entry)
                     (or (plist-get outcome :body) ""))
                    command-inputs)
              (setq permission-rules
                    (append permission-rules
                            (plist-get context :permission-rules))
                    ptc-primitives
                    (niwa-skills-intersect-ptc-primitives
                     ptc-primitives
                     (plist-get context :ptc-primitives))
                    hook-rules
                    (append hook-rules (plist-get context :hook-rules)))
              (when (= command-count 1)
                (setq single-command-context context)))
          (let* ((skill (niwa-skill-plan-entry-skill entry))
                 (attachment
                  (list :name (niwa-skill-plan-entry-name entry)
                        :body (or (plist-get outcome :body) "")
                        :arguments (or (plist-get outcome :arguments) "")
                        :source-file (and skill
                                          (niwa-skill-source-file skill))
                        :skill skill))
                 (key
                  (cons (or (and skill
                                 (niwa-skills-source-key
                                  (niwa-skill-source-file skill)))
                            skill
                            (niwa-skill-plan-entry-name entry))
                        (or (plist-get outcome :arguments) ""))))
            (unless (gethash key seen-attachments)
              (puthash key t seen-attachments)
              (push attachment instruction-attachments))))))
    (list :command-count command-count
          :command-bodies (nreverse command-bodies)
          :command-inputs (nreverse command-inputs)
          :instruction-attachments (nreverse instruction-attachments)
          :permission-rules permission-rules
          :ptc-primitives ptc-primitives
          :hook-rules hook-rules
          :single-command-context single-command-context)))

(defun niwa-skills-plan--prepared-outcome (plan pairs prepared)
  "Return the complete outcome for PLAN and PAIRS using PREPARED metadata."
  (let* ((aggregate (niwa-skills-plan--aggregate-prepared pairs))
         (command-bodies (plist-get aggregate :command-bodies))
         ;; These ride the submitted model-input (turn-bound provenance
         ;; of the prompt content) rather than the ephemeral reminder
         ;; channel: the outcome is consumed by several submission
         ;; paths, and a session-FIFO delivery could leak into an
         ;; unrelated turn when a prepared submission is cancelled.
         (reminder-blocks
          (mapcar
           (lambda (attachment)
             (niwa-reminders-format-block
              (niwa-skills-format-attachment attachment)))
           (plist-get aggregate :instruction-attachments)))
         (main-input
          (if command-bodies
              (mapconcat #'identity command-bodies "\n\n")
            (niwa-skills-plan-replace-instructions
             (niwa-skill-invocation-plan-text plan)
             (niwa-skill-invocation-plan-occurrences plan))))
         (command-inputs (plist-get aggregate :command-inputs))
         (model-input
          (mapconcat
           #'identity
           (append reminder-blocks
                   (list (if command-inputs
                             (mapconcat #'identity command-inputs "\n\n")
                           main-input)))
           "\n\n"))
         (request-context
          (list :permission-rules (plist-get aggregate :permission-rules)
                :ptc-primitives (plist-get aggregate :ptc-primitives)
                :hook-rules (plist-get aggregate :hook-rules)
                :invoked-skills (plist-get prepared :invoked-skills))))
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
          :render-prompt main-input
          :render-attachments (plist-get aggregate :instruction-attachments)
          :warnings
          (mapcar #'niwa-skill-plan-occurrence-message
                  (cl-remove-if-not
                   (lambda (occurrence)
                     (eq (niwa-skill-plan-occurrence-role occurrence)
                         'unavailable))
                   (niwa-skill-invocation-plan-occurrences plan)))
          :request-context request-context
          :hook-context (plist-get prepared :hook-context)
          :hook-audits (plist-get prepared :hook-audits))))

(defun niwa-skills-plan-prepare (plan callback &optional cancelled-p)
  "Prepare PLAN sequentially, then call CALLBACK with one complete outcome.

Unique commands prepare first, followed by unique instructions.  Command
arguments are PLAN's shared argument slice after recorded instruction
occurrences have become inert placeholders.  Instruction preparation always
receives empty arguments.  CANCELLED-P, when non-nil, is a zero-argument
predicate checked before and after every asynchronous preparation.  Once the
transaction fails or is cancelled, later callbacks have no effect."
  (let* ((entries (niwa-skill-invocation-plan-entries plan))
         (command-count
          (cl-count-if
           (lambda (entry)
             (eq (niwa-skill-plan-entry-role entry) 'command))
           entries))
        (command-arguments
         (if-let* ((arguments
                    (niwa-skill-invocation-plan-arguments plan)))
             (niwa-skills-plan-replace-instructions
              arguments
              (niwa-skill-invocation-plan-occurrences plan)
              (niwa-skill-invocation-plan-arguments-start plan))
           ""))
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
         (prepared (aggregate)
           (unless settled
             (cond
              ((cancelled) (cancel))
              ((not (eq (plist-get aggregate :status) 'ok))
               (finish aggregate))
              (t
               (finish
                (niwa-skills-plan--prepared-outcome
                 plan
                 (cl-mapcar (lambda (entry outcome)
                              (list :entry entry :outcome outcome))
                            entries (plist-get aggregate :outcomes))
                 aggregate)))))))
      (if (cancelled)
          (cancel)
        (niwa-skills-prepare-many
         (mapcar
          (lambda (entry)
            (let ((role (niwa-skill-plan-entry-role entry)))
              (list :skill (niwa-skill-plan-entry-skill entry)
                    :arguments (if (eq role 'command) command-arguments "")
                    :role role
                    :policy-owner-p (and (eq role 'command)
                                         (= command-count 1)))))
          entries)
         #'prepared
         :origin 'user :cancelled-p cancelled-p)))))

(defun niwa-skills-plan-render-data (plan prepared)
  "Return hidden render data for PLAN and its PREPARED outcome."
  (niwa-tool-render-data-format
   (list :kind 'inline-skill
         :display-text (niwa-skill-invocation-plan-text plan)
         :prompt (plist-get prepared :render-prompt)
         :attachments
         (mapcar
          (lambda (attachment)
            (list :name (plist-get attachment :name)
                  :body (niwa-skills-format-attachment attachment)))
          (plist-get prepared :render-attachments)))))

(provide 'niwa-skills-plan)
;;; niwa-skills-plan.el ends here
