;;; niwa-skills-prompt.el -- Model-visible skill roster -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the budgeted skill roster and optional path-discovery notices.
;; Catalog delivery uses retained context observations; path notices use
;; turn events and buffer-local activation hooks.

;;; Code:

(require 'cl-lib)
(require 'niwa-reminders)
(require 'niwa-skills-core)
(require 'niwa-skills-invoke)
(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-tool-registry)

;; `gptel'
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel-tools)

;; `niwa-agents'
(declare-function niwa-agent-invocation-parent-session
                  "niwa-agents" (cl-x) t)

;; `niwa-hooks'
(defvar niwa-post-tool-use-functions)

;; `niwa-models'
(declare-function niwa-model-effective-context-window "niwa-models"
                  (&optional model))
(defvar niwa-model-context-limit)
(autoload 'niwa-model-effective-context-window "niwa-models")

;; `niwa-skills-invoke'
(declare-function niwa-skills--current-invocation
                  "niwa-skills-invoke" ())
(declare-function niwa-skills--entry-base
                  "niwa-skills-invoke" (skill &optional dormant))
(declare-function niwa-skills--entry-description
                  "niwa-skills-invoke" (skill &optional dormant))
(declare-function niwa-skills--listing-candidates
                  "niwa-skills-invoke" (session))
(declare-function niwa-skills--model-visible-p
                  "niwa-skills-invoke" (skill &optional active-only))
(declare-function niwa-skills--truncate-text
                  "niwa-skills-invoke" (text limit))
(declare-function niwa-skills-request-model-policy
                  "niwa-skills-invoke" ())

;; `niwa-structs'
(defvar niwa--session)

;; `niwa-tool-ptc'
(declare-function niwa-tool-ptc--roster "niwa-tool-ptc" ())

;; `niwa-tool-permission'
(declare-function niwa-tool-permission-paths "niwa-tool-permission"
                  (tool args &optional context))
(autoload 'niwa-tool-permission-paths "niwa-tool-permission")

;; `niwa-tool-registry'
(declare-function niwa-tool-get
                  "niwa-tool-registry" (name &optional category))



;;
;;; Skills prompt roster and reminders

(defcustom niwa-skills-listing-budget 0.02
  "Fraction of the context window allotted to the model-facing skills roster.

The prompt roster enumerates active, model-invocable skills without path
restrictions.  Path-scoped skills use notices and ListSkills.  This fraction of
the active model's context window, or of `niwa-model-context-limit'
when the model declares none (converted to characters at four chars per
token), caps the roster so it cannot crowd out the user's conversation
on long sessions."
  :type 'float
  :group 'niwa)

(defun niwa-skills--listing-budget-chars ()
  "Return the character budget for the model-facing skills roster.
Derived from `niwa-skills-listing-budget' and the context window of
the model the pending request resolves to -- a Plan workload's or a
leading skill's override included, since gptel sizes the system prompt
before the transforms apply either; assumes ~4 characters per token."
  (let ((limit (niwa-model-effective-context-window
                (plist-get (niwa-skills-request-model-policy) :model))))
    (max 0 (floor (* niwa-skills-listing-budget limit 4)))))

(defun niwa-skills--short-purpose (skill)
  "Return a one-line purpose for SKILL; full descriptions remain searchable."
  (let* ((description (niwa-skills--entry-description skill))
         (line (car (split-string description "[\n\r]" t))))
    (niwa-skills--truncate-text
     (if (and line (string-match "[.!?]\\(?: \\|$\\)" line))
         (substring line 0 (1+ (match-beginning 0)))
       (or line ""))
     160)))

(defun niwa-skills--format-listing-result (skills)
  "Return structured budgeted active roster data for SKILLS.

Descriptions are shortened before names disappear.  Whole entries are
omitted only when name-only entries cannot fit in
`niwa-skills--listing-budget-chars'.  The returned plist contains
:text and :status, where :status is nil, `truncated', or `omitted'."
  (let* ((budget (niwa-skills--listing-budget-chars))
         (header "### Available skills")
         (full-lines
          (mapcar (lambda (skill)
                    (concat (niwa-skills--entry-base skill) " "
                            (niwa-skills--short-purpose skill)))
                  skills)))
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
              ;; Prefixes resolve canonical skill source paths.  Do not
              ;; repeat those filesystem queries for an unused fallback.
              (unless (fits-p full-body)
                (mapcar (lambda (skill)
                          (concat (niwa-skills--entry-base skill) " "))
                        skills)))
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
                (shortened nil))
            (cl-loop for tail on lines
                     for skill in skills do
              (let* ((desc (niwa-skills--short-purpose skill))
                     (remaining (- budget used)))
                (cond
                 ((string-empty-p desc))
                 ((<= (length desc) remaining)
                  (setf (car tail) (concat (car tail) desc))
                  (cl-incf used (length desc)))
                 ((> remaining 0)
                  (setf (car tail)
                        (concat (car tail)
                                (niwa-skills--truncate-text
                                 desc remaining)))
                  (setq used budget
                        shortened t))
                 (t
                  (setq shortened t)))))
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

(defun niwa-skills--system-roster-candidates (session)
  "Return SESSION's active model-visible skills without path restrictions.
Path-scoped skills use discovery notices and ListSkills results so file
activity does not rewrite the system prefix ahead of conversation history."
  (cl-remove-if #'niwa-skill-path-patterns
                (niwa-skills--listing-candidates session)))

(defun niwa-skills-prompt-section (session &optional buffer)
  "Return the dynamic skills prompt section for SESSION.
BUFFER, when non-nil, is used to refresh dirty skill roots before
rendering."
  (when (and buffer (buffer-live-p buffer))
    (niwa-skills-ensure-fresh buffer session))
  (when-let* ((skills (niwa-skills--system-roster-candidates session)))
    (let* ((listing-result (niwa-skills--format-listing-result skills))
           (listing (plist-get listing-result :text)))
      ;; Rendering also happens for prompt sizing and unchanged retained
      ;; context.  It is not evidence of a new advertisement to the model.
      (when (niwa-telemetry-detailed-p session)
        (niwa-telemetry-record
         session 'skills-roster-advertised
         :skill-count (length skills)
         :skill-names (mapcar #'niwa-skill-name skills)
         :budget-status (plist-get listing-result :status)
         :omitted-count (plist-get listing-result :omitted)
         :roster-chars (length listing)))
      (concat "## Skills\n"
              "A skill is a reusable prompt recipe. Call Skill and ListSkills through ToolCall. Skills without path restrictions are listed below by canonical invocation name; path-scoped skills are discoverable through matching-path notices and ListSkills.\n\n"
              listing
              "\n\nSearch ListSkills by purpose for full descriptions; invoke Skill to load its instructions."))))

;;
;;; Conditional activation reminders

(defvar-local niwa-skills--path-notices-seen nil
  "Skill discovery facts delivered to this conversation buffer.
Entries are (NAME SOURCE DESCRIPTION PATH-PATTERNS).  This is a live optional
notice throttle, not skill activation, invocation, or authority state.")

(defun niwa-skills--activation-reminder (path skill)
  "Return an optional discovery notice for SKILL matching PATH."
  (format
   "Optional skill `%s` matches the path `%s` from your tool call: %s\nUse ToolCall expression `(Skill :name \"...\")` if its guidance helps; ToolCall expression `(ListSkills :query \"...\")` gives details. This notice does not invoke the skill."
   (niwa-skill-name skill) path
   (niwa-skills--entry-description skill)))

(defun niwa-skills--commit-path-notice (buffer session fact)
  "Acknowledge delivered skill FACT in BUFFER if it still owns SESSION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq session niwa--session)
        (setq niwa-skills--path-notices-seen
              (cons fact (assoc-delete-all
                          (car fact) niwa-skills--path-notices-seen)))))))

(defun niwa-skills--queue-activation-reminder (buffer session path skills)
  "Queue optional discovery of SKILLS matching PATH in BUFFER's SESSION.
Each skill has its own coalescing key, so observing another path cannot erase
a different skill's notice.  Acknowledgement belongs to BUFFER and commits
only after delivery.  Path notices do not change the system-roster snapshot."
  (with-current-buffer buffer
    (when (or (cl-find "Skill" gptel-tools :key #'gptel-tool-name :test #'equal)
              (and (cl-find "ToolCall" gptel-tools :key #'gptel-tool-name :test #'equal)
                   (member "Skill" (niwa-tool-ptc--roster))))
      (dolist (skill skills)
        (let ((fact (list (niwa-skill-name skill)
                          (niwa-skill-source-file skill)
                          (niwa-skill-description skill)
                          (niwa-skill-path-patterns skill))))
          (when (and (niwa-skills--model-visible-p skill t)
                     (not (member fact niwa-skills--path-notices-seen)))
            (niwa-reminders-queue-turn-event
             buffer (cons 'skill-activation (niwa-skill-name skill))
             (niwa-skills--activation-reminder path skill)
             (lambda ()
               (niwa-skills--commit-path-notice buffer session fact)))))))))

(defun niwa-skills--post-tool-activate (info)
  "Discover optional path skills after a pipeline tool event INFO.
Catalogue activation is shared.  Notices are evaluated for this recipient
even if another conversation previously activated the same skill.  Post-tool
discovery cannot enforce required pre-action instructions.  Failed path
attempts can also make optional guidance discoverable."
  (when-let* ((session (or (and (boundp 'niwa--session) niwa--session)
                           (when-let* ((inv (niwa-skills--current-invocation)))
                             (niwa-agent-invocation-parent-session inv))))
              (tool-name (plist-get info :tool-name))
              (args (plist-get info :tool-input))
              (tool (niwa-tool-get tool-name (plist-get info :tool-category))))
    (dolist (path (niwa-tool-permission-paths tool args))
      (niwa-skills-maybe-activate session path)
      (niwa-skills--queue-activation-reminder
       (current-buffer) session path
       (cl-remove-if-not
        (lambda (skill)
          (niwa-skills--path-matches-p path (niwa-skill-path-patterns skill)))
        (niwa-session-skills session)))))
  nil)

;;;###autoload
(defun niwa-skills-install-activation-hook ()
  "Install discovery at the shared native/nested post-tool pipeline boundary."
  (add-hook 'niwa-post-tool-use-functions
            #'niwa-skills--post-tool-activate nil t))

(provide 'niwa-skills-prompt)
;;; niwa-skills-prompt.el ends here
