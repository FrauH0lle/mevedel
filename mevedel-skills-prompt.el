;;; mevedel-skills-prompt.el -- Model-visible skill roster -*- lexical-binding: t -*-

;;; Commentary:

;; Owns request-time skill visibility: the budgeted system-prompt roster,
;; roster-change reminders, and path-activation notices.  It installs only
;; session-owned reminders and buffer-local activation hooks.

;;; Code:

(require 'cl-lib)
(require 'mevedel-reminders)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)

;; `gptel'
(defvar gptel-post-tool-call-functions)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)

;; `mevedel-compact'
(defvar mevedel-compact-context-limit)

;; `mevedel-reminders'
(defvar mevedel-reminders--current-chat-buffer)

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--current-invocation
                  "mevedel-skills-invoke" ())
(declare-function mevedel-skills--entry-base
                  "mevedel-skills-invoke" (skill &optional dormant))
(declare-function mevedel-skills--entry-description
                  "mevedel-skills-invoke" (skill &optional dormant))
(declare-function mevedel-skills--listing-candidates
                  "mevedel-skills-invoke" (session))
(declare-function mevedel-skills--listing-describe
                  "mevedel-skills-invoke" (skill &optional dormant))
(declare-function mevedel-skills--model-visible-p
                  "mevedel-skills-invoke" (skill &optional active-only))
(declare-function mevedel-skills--truncate-text
                  "mevedel-skills-invoke" (text limit))

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get
                  "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)


;;
;;; Skills prompt roster and reminders

(defun mevedel-skills--buffer-for-session (session)
  "Return a live buffer whose `mevedel--session' is SESSION, or nil."
  (catch 'found
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (local-variable-p 'mevedel--session buffer)
                 (eq (buffer-local-value 'mevedel--session buffer) session))
        (throw 'found buffer)))))

(defun mevedel-skills--current-reminder-buffer (session)
  "Return the chat buffer whose skills should be refreshed for SESSION."
  (let ((buffer mevedel-reminders--current-chat-buffer))
    (if (and (buffer-live-p buffer)
             (local-variable-p 'mevedel--session buffer)
             (eq (buffer-local-value 'mevedel--session buffer) session))
        buffer
      (mevedel-skills--buffer-for-session session))))

(defcustom mevedel-skills-listing-budget 0.02
  "Fraction of the context window allotted to the model-facing skills roster.

The dynamic prompt roster enumerates active, model-invocable skills so
the model can call them by name via the `Skill' tool.  This fraction of
`mevedel-compact-context-limit' (converted to characters at four chars
per token) caps the roster so it cannot crowd out the user's
conversation on long sessions."
  :type 'float
  :group 'mevedel)

(defun mevedel-skills--listing-budget-chars ()
  "Return the character budget for the model-facing skills roster.
Derived from `mevedel-skills-listing-budget' and the compact context
limit; assumes ~4 characters per token."
  (let ((limit (or (and (boundp 'mevedel-compact-context-limit)
                        mevedel-compact-context-limit)
                   200000)))
    (max 0 (floor (* mevedel-skills-listing-budget limit 4)))))

(defun mevedel-skills--format-listing-result (skills)
  "Return structured budgeted active roster data for SKILLS.

Descriptions are shortened before names disappear.  Whole entries are
omitted only when name-only entries cannot fit in
`mevedel-skills--listing-budget-chars'.  The returned plist contains
:text and :status, where :status is nil, `truncated', or `omitted'."
  (let* ((budget (mevedel-skills--listing-budget-chars))
         (header "### Available skills")
         (full-lines (mapcar #'mevedel-skills--listing-describe skills)))
    (cl-labels
        ((body (lines &optional notes)
           (string-join
            (append (list header "")
                    lines
                    (when notes
                      (cons "" notes)))
            "\n"))
         (fits-p (text)
           (<= (length text) budget))
         (omit-note (count)
           (format "%d skills omitted; ListSkills(query)." count)))
      (let* ((full-body (body full-lines))
             (name-lines
              (mapcar (lambda (skill)
                        (concat (mevedel-skills--entry-base skill) " "))
                      skills))
             (name-body (body name-lines))
             (short-note
              "Skill descriptions were shortened; ListSkills(query).")
             (short-note-fits (fits-p (body name-lines (list short-note)))))
        (cond
         ((fits-p full-body)
          (list :text full-body :status nil))
         ((fits-p name-body)
          (let ((lines (copy-sequence name-lines))
                (used (length (body name-lines
                                    (and short-note-fits
                                         (list short-note)))))
                (index 0)
                (shortened nil))
            (dolist (skill skills)
              (let* ((desc (mevedel-skills--entry-description skill))
                     (remaining (- budget used)))
                (cond
                 ((string-empty-p desc))
                 ((<= (length desc) remaining)
                  (setf (nth index lines)
                        (concat (nth index lines) desc))
                  (cl-incf used (length desc)))
                 ((> remaining 0)
                  (setf (nth index lines)
                        (concat (nth index lines)
                                (mevedel-skills--truncate-text
                                 desc remaining)))
                  (setq used budget
                        shortened t))
                 (t
                  (setq shortened t))))
              (cl-incf index))
            (list
             :text (body lines
                         (and shortened short-note-fits
                              (list short-note)))
             :status (and shortened 'truncated))))
         (t
          (let ((lines nil)
                (omitted 0))
            (dolist (line name-lines)
              (let ((candidate (append lines (list line))))
                (if (fits-p (body candidate))
                    (setq lines candidate)
                  (cl-incf omitted))))
            (let ((note (omit-note omitted)))
              (while (and lines (not (fits-p (body lines (list note)))))
                (setq lines (butlast lines))
                (cl-incf omitted)
                (setq note (omit-note omitted)))
              (list
               :text (body lines
                           (and (fits-p (body lines (list note)))
                                (list note)))
               :status (and (> omitted 0) 'omitted)
               :omitted omitted)))))))))

(defun mevedel-skills--format-listing (skills)
  "Format SKILLS as the budgeted active roster."
  (plist-get (mevedel-skills--format-listing-result skills) :text))

(defun mevedel-skills--listing-budget-status (skills)
  "Return budget status for SKILLS, or nil when the full roster fits."
  (plist-get (mevedel-skills--format-listing-result skills) :status))

(defconst mevedel-skills--prompt-contract
  "### How to use skills
- Trigger rules: If the user names a listed skill with unquoted `$SkillName` syntax or plain text, or the task clearly matches a listed skill description, call `Skill(name=...)` before proceeding. Quoted, escaped, or Markdown-code `$SkillName` text is literal and must not trigger a skill.
- Discovery: If unsure which skill applies, call `ListSkills(query)` with a short search term.
- Coordination: Choose the minimal applicable skill set. When multiple skills apply, state the order you will use them.
- Scope: Do not carry skill decisions across turns unless re-mentioned or newly matched."
  "Concise model-facing instructions for using mevedel skills.")

(defun mevedel-skills-prompt-section (session &optional buffer)
  "Return the dynamic skills prompt section for SESSION.
BUFFER, when non-nil, is used to refresh dirty skill roots before
rendering."
  (when (and buffer (buffer-live-p buffer))
    (mevedel-skills--ensure-fresh buffer session))
  (when-let* ((skills (mevedel-skills--listing-candidates session)))
    (concat "## Skills\n"
            "A skill is a reusable prompt recipe available through the `Skill` tool. The active skills for this session are listed below by canonical invocation name.\n\n"
            (mevedel-skills--format-listing skills)
            "\n\n"
            mevedel-skills--prompt-contract)))

(defun mevedel-skills--skill-snapshot (session)
  "Return SESSION's current active model-visible skill snapshot."
  (mapcar (lambda (skill)
            (cons (mevedel-skill-name skill)
                  (or (mevedel-skill-description skill) "")))
          (mevedel-skills--listing-candidates session)))

(defun mevedel-skills--roster-budget-status (session)
  "Return SESSION's current prompt-roster budget status."
  (when-let* ((skills (mevedel-skills--listing-candidates session)))
    (mevedel-skills--listing-budget-status skills)))

(defun mevedel-skills--format-roster-budget-reminder (status)
  "Return reminder text for roster budget STATUS."
  (pcase status
    ('omitted
     "The skills roster in the system prompt omitted some active skills because of budget. Use `ListSkills(query)` to search the full enabled model-invocable skill set.")
    ('truncated
     "The skills roster in the system prompt shortened some descriptions because of budget. Use `ListSkills(query)` to search for complete skill details.")
    (_ nil)))

(defun mevedel-skills--format-delta-list (entries with-descriptions)
  "Return a capped bullet list for ENTRIES.
When WITH-DESCRIPTIONS is non-nil, include descriptions from ENTRIES."
  (mapconcat
   (lambda (entry)
     (if with-descriptions
         (format "  - %s: %s" (car entry) (cdr entry))
       (format "  - %s" (car entry))))
   (cl-subseq entries 0 (min 10 (length entries)))
   "\n"))

(defun mevedel-skills--format-delta (added removed)
  "Return a model reminder for ADDED and REMOVED skill snapshot entries."
  (let ((parts (list "Available skills changed.")))
    (when added
      (push (concat "Added skills:\n"
                    (mevedel-skills--format-delta-list added t))
            parts)
      (when (> (length added) 10)
        (push (format "and %d more; use ListSkills(query)"
                      (- (length added) 10))
              parts)))
    (when removed
      (push (concat "Removed skills:\n"
                    (mevedel-skills--format-delta-list removed nil))
            parts)
      (when (> (length removed) 10)
        (push (format "and %d more; use ListSkills(query)"
                      (- (length removed) 10))
              parts)))
    (string-join (nreverse parts) "\n\n")))

(defun mevedel-reminders-make-skills-delta ()
  "Create the `skills-delta' reminder.

The first snapshot is silent.  Later active model-visible skill changes
are reported once and become the new snapshot."
  (let (delta)
    (mevedel-reminder-create
     :type 'skills-delta
     :trigger (lambda (session)
                (setq delta nil)
                (when-let* ((buffer (mevedel-skills--current-reminder-buffer
                                     session)))
                  (mevedel-skills--ensure-fresh buffer session))
                (let* ((current (mevedel-skills--skill-snapshot session))
                       (previous (mevedel-session-skills-snapshot session)))
                  (if (eq previous :uninitialized)
                      (progn
                        (setf (mevedel-session-skills-snapshot session)
                              current)
                        nil)
                    (let ((added (cl-remove-if
                                  (lambda (entry)
                                    (assoc (car entry) previous))
                                  current))
                          (removed (cl-remove-if
                                    (lambda (entry)
                                      (assoc (car entry) current))
                                    previous)))
                      (when (or added removed)
                        (setq delta (list :added added :removed removed))
                        t)))))
     :content (lambda (session)
                (prog1
                    (mevedel-skills--format-delta
                     (plist-get delta :added)
                     (plist-get delta :removed))
                  (setf (mevedel-session-skills-snapshot session)
                        (mevedel-skills--skill-snapshot session))
                  (setq delta nil)))
     :interval nil)))

(defun mevedel-reminders-make-skills-roster-budget ()
  "Create the `skills-roster-budget' reminder.

Emits once when the active prompt roster is truncated or omits entries,
and again if that budget status changes."
  (let ((last-status :uninitialized)
        status)
    (mevedel-reminder-create
     :type 'skills-roster-budget
     :trigger (lambda (session)
                (when-let* ((buffer (mevedel-skills--current-reminder-buffer
                                     session)))
                  (mevedel-skills--ensure-fresh buffer session))
                (setq status (mevedel-skills--roster-budget-status session))
                (let ((changed (not (eq status last-status))))
                  (setq last-status status)
                  (and status changed)))
     :content (lambda (_session)
                (mevedel-skills--format-roster-budget-reminder status))
     :interval nil)))

;;
;;; Conditional activation reminders

(defun mevedel-skills--activation-reminder (path skills)
  "Return a compact reminder that SKILLS activated because of PATH."
  (let* ((names (mapcar #'mevedel-skill-name skills))
         (shown (cl-subseq names 0 (min 5 (length names))))
         (more (- (length names) (length shown))))
    (format
     "Dormant path-scoped skills became relevant after `%s`: %s%s. They are now in the active skills roster. Use `Skill(name=...)` if applicable; use `ListSkills(query)` to search."
     path
     (mapconcat #'identity shown ", ")
     (if (> more 0)
         (format ", and %d more" more)
       ""))))

(defun mevedel-skills--queue-activation-reminder (session path skills)
  "Queue a path activation reminder for SESSION and SKILLS."
  (when-let* ((visible (cl-remove-if-not
                        (lambda (skill)
                          (mevedel-skills--model-visible-p skill t))
                        skills)))
    (mevedel-session-enqueue-pending-reminder
     session (mevedel-skills--activation-reminder path visible))
    (setf (mevedel-session-skills-snapshot session)
          (mevedel-skills--skill-snapshot session))))

(defun mevedel-skills--post-tool-activate (info)
  "Post-tool-call hook: activate conditional skills based on tool path.

INFO is the plist passed by `gptel-post-tool-call-functions'.  Extracts
the touched path via the tool struct's `get-path' slot and forwards it
to `mevedel-skills--maybe-activate'."
  (when-let* ((session (or (and (boundp 'mevedel--session) mevedel--session)
                           (when-let* ((inv (mevedel-skills--current-invocation)))
                             (mevedel-agent-invocation-parent-session inv))))
              (tool-name (plist-get info :name))
              (args (plist-get info :args))
              (tool (mevedel-tool-get tool-name))
              (get-path-fn (mevedel-tool-get-path tool)))
    (when-let* ((path (ignore-errors (funcall get-path-fn args))))
      (mevedel-skills--queue-activation-reminder
       session path
       (mevedel-skills--maybe-activate session path))))
  nil)

;;;###autoload
(defun mevedel-skills-install-activation-hook ()
  "Install the buffer-local post-tool-call activation hook."
  (add-hook 'gptel-post-tool-call-functions
            #'mevedel-skills--post-tool-activate nil t))

;;
;;; Reminder installation

;;;###autoload
(defun mevedel-skills-install-reminder (session)
  "Add the skills event reminders to SESSION if not already present.
Idempotent."
  (unless (cl-find 'skills-roster-budget
                   (mevedel-session-reminders session)
                   :key #'mevedel-reminder-type)
    (mevedel-session-add-reminder
     session (mevedel-reminders-make-skills-roster-budget)))
  (unless (cl-find 'skills-delta
                   (mevedel-session-reminders session)
                   :key #'mevedel-reminder-type)
    (mevedel-session-add-reminder
     session (mevedel-reminders-make-skills-delta)))
  session)

(provide 'mevedel-skills-prompt)
;;; mevedel-skills-prompt.el ends here
