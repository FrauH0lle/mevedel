;;; mevedel-skills-prompt.el -- Model-visible skill roster -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the budgeted skill roster and optional path-discovery notices.
;; Catalog delivery uses retained context observations; path notices use
;; turn events and buffer-local activation hooks.

;;; Code:

(require 'cl-lib)
(require 'mevedel-reminders)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)

;; `gptel'
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel-tools)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)

;; `mevedel-hooks'
(defvar mevedel-post-tool-use-functions)

;; `mevedel-models'
(declare-function mevedel-model-effective-context-window "mevedel-models"
                  (&optional model))
(defvar mevedel-model-context-limit)
(autoload 'mevedel-model-effective-context-window "mevedel-models")

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--current-invocation
                  "mevedel-skills-invoke" ())
(declare-function mevedel-skills--entry-base
                  "mevedel-skills-invoke" (skill &optional dormant))
(declare-function mevedel-skills--entry-description
                  "mevedel-skills-invoke" (skill &optional dormant))
(declare-function mevedel-skills--listing-candidates
                  "mevedel-skills-invoke" (session))
(declare-function mevedel-skills--model-visible-p
                  "mevedel-skills-invoke" (skill &optional active-only))
(declare-function mevedel-skills--truncate-text
                  "mevedel-skills-invoke" (text limit))
(declare-function mevedel-skills-request-model-policy
                  "mevedel-skills-invoke" ())

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-tool-ptc'
(declare-function mevedel-tool-ptc--roster "mevedel-tool-ptc" ())

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-permission'
(declare-function mevedel-tool-permission-paths "mevedel-tool-permission"
                  (tool args &optional context))
(autoload 'mevedel-tool-permission-paths "mevedel-tool-permission")

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get
                  "mevedel-tool-registry" (name &optional category))



;;
;;; Skills prompt roster and reminders

(defcustom mevedel-skills-listing-budget 0.02
  "Fraction of the context window allotted to the model-facing skills roster.

The prompt roster enumerates active, model-invocable skills without path
restrictions.  Path-scoped skills use notices and ListSkills.  This fraction of
the active model's context window, or of `mevedel-model-context-limit'
when the model declares none (converted to characters at four chars per
token), caps the roster so it cannot crowd out the user's conversation
on long sessions."
  :type 'float
  :group 'mevedel)

(defun mevedel-skills--listing-budget-chars ()
  "Return the character budget for the model-facing skills roster.
Derived from `mevedel-skills-listing-budget' and the context window of
the model the pending request resolves to -- a Plan workload's or a
leading skill's override included, since gptel sizes the system prompt
before the transforms apply either; assumes ~4 characters per token."
  (let ((limit (mevedel-model-effective-context-window
                (plist-get (mevedel-skills-request-model-policy) :model))))
    (max 0 (floor (* mevedel-skills-listing-budget limit 4)))))

(defun mevedel-skills--short-purpose (skill)
  "Return a one-line purpose for SKILL; full descriptions remain searchable."
  (let* ((description (mevedel-skills--entry-description skill))
         (line (car (split-string description "[\n\r]" t))))
    (mevedel-skills--truncate-text
     (if (and line (string-match "[.!?]\\(?: \\|$\\)" line))
         (substring line 0 (1+ (match-beginning 0)))
       (or line ""))
     160)))

(defun mevedel-skills--format-listing-result (skills)
  "Return structured budgeted active roster data for SKILLS.

Descriptions are shortened before names disappear.  Whole entries are
omitted only when name-only entries cannot fit in
`mevedel-skills--listing-budget-chars'.  The returned plist contains
:text and :status, where :status is nil, `truncated', or `omitted'."
  (let* ((budget (mevedel-skills--listing-budget-chars))
         (header "### Available skills")
         (full-lines
          (mapcar (lambda (skill)
                    (concat (mevedel-skills--entry-base skill) " "
                            (mevedel-skills--short-purpose skill)))
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
                (shortened nil))
            (cl-loop for tail on lines
                     for skill in skills do
              (let* ((desc (mevedel-skills--short-purpose skill))
                     (remaining (- budget used)))
                (cond
                 ((string-empty-p desc))
                 ((<= (length desc) remaining)
                  (setf (car tail) (concat (car tail) desc))
                  (cl-incf used (length desc)))
                 ((> remaining 0)
                  (setf (car tail)
                        (concat (car tail)
                                (mevedel-skills--truncate-text
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

(defun mevedel-skills--system-roster-candidates (session)
  "Return SESSION's active model-visible skills without path restrictions.
Path-scoped skills use discovery notices and ListSkills results so file
activity does not rewrite the system prefix ahead of conversation history."
  (cl-remove-if #'mevedel-skill-path-patterns
                (mevedel-skills--listing-candidates session)))

(defun mevedel-skills-prompt-section (session &optional buffer)
  "Return the dynamic skills prompt section for SESSION.
BUFFER, when non-nil, is used to refresh dirty skill roots before
rendering."
  (when (and buffer (buffer-live-p buffer))
    (mevedel-skills-ensure-fresh buffer session))
  (when-let* ((skills (mevedel-skills--system-roster-candidates session)))
    (let* ((listing-result (mevedel-skills--format-listing-result skills))
           (listing (plist-get listing-result :text)))
      (when (fboundp 'mevedel-telemetry-record)
        (mevedel-telemetry-record
         session 'skills-roster-advertised
         :skill-count (length skills)
         :skill-names (mapcar #'mevedel-skill-name skills)
         :budget-status (plist-get listing-result :status)
         :omitted-count (plist-get listing-result :omitted)
         :roster-chars (length listing)))
      (concat "## Skills\n"
              "A skill is a reusable prompt recipe. Discover Skill and ListSkills through ToolSearch; invoke them through ToolCall. Skills without path restrictions are listed below by canonical invocation name; path-scoped skills are discoverable through matching-path notices and ListSkills.\n\n"
              listing
              "\n\nSearch ListSkills by purpose for full descriptions; invoke Skill to load its instructions."))))

;;
;;; Conditional activation reminders

(defvar-local mevedel-skills--path-notices-seen nil
  "Skill discovery facts delivered to this conversation buffer.
Entries are (NAME SOURCE DESCRIPTION PATH-PATTERNS).  This is a live optional
notice throttle, not skill activation, invocation, or authority state.")

(defun mevedel-skills--activation-reminder (path skill)
  "Return an optional discovery notice for SKILL matching PATH."
  (format
   "Optional skill `%s` matches the path `%s` from your tool call: %s\nUse ToolCall expression `(Skill :name \"...\")` if its guidance helps; ToolCall expression `(ListSkills :query \"...\")` gives details. This notice does not invoke the skill."
   (mevedel-skill-name skill) path
   (mevedel-skills--entry-description skill)))

(defun mevedel-skills--commit-path-notice (buffer session fact)
  "Acknowledge delivered skill FACT in BUFFER if it still owns SESSION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq session mevedel--session)
        (setq mevedel-skills--path-notices-seen
              (cons fact (assoc-delete-all
                          (car fact) mevedel-skills--path-notices-seen)))))))

(defun mevedel-skills--queue-activation-reminder (buffer session path skills)
  "Queue optional discovery of SKILLS matching PATH in BUFFER's SESSION.
Each skill has its own coalescing key, so observing another path cannot erase
a different skill's notice.  Acknowledgement belongs to BUFFER and commits
only after delivery.  Path notices do not change the system-roster snapshot."
  (with-current-buffer buffer
    (when (or (cl-find "Skill" gptel-tools :key #'gptel-tool-name :test #'equal)
              (and (cl-find "ToolCall" gptel-tools :key #'gptel-tool-name :test #'equal)
                   (member "Skill" (mevedel-tool-ptc--roster))))
      (dolist (skill skills)
        (let ((fact (list (mevedel-skill-name skill)
                          (mevedel-skill-source-file skill)
                          (mevedel-skill-description skill)
                          (mevedel-skill-path-patterns skill))))
          (when (and (mevedel-skills--model-visible-p skill t)
                     (not (member fact mevedel-skills--path-notices-seen)))
            (mevedel-reminders-queue-turn-event
             buffer (cons 'skill-activation (mevedel-skill-name skill))
             (mevedel-skills--activation-reminder path skill)
             (lambda ()
               (mevedel-skills--commit-path-notice buffer session fact)))))))))

(defun mevedel-skills--post-tool-activate (info)
  "Discover optional path skills after a pipeline tool event INFO.
Catalogue activation is shared.  Notices are evaluated for this recipient
even if another conversation previously activated the same skill.  Post-tool
discovery cannot enforce required pre-action instructions.  Failed path
attempts can also make optional guidance discoverable."
  (when-let* ((session (or (and (boundp 'mevedel--session) mevedel--session)
                           (when-let* ((inv (mevedel-skills--current-invocation)))
                             (mevedel-agent-invocation-parent-session inv))))
              (tool-name (plist-get info :tool-name))
              (args (plist-get info :tool-input))
              (tool (mevedel-tool-get tool-name (plist-get info :tool-category))))
    (dolist (path (mevedel-tool-permission-paths tool args))
      (mevedel-skills-maybe-activate session path)
      (mevedel-skills--queue-activation-reminder
       (current-buffer) session path
       (cl-remove-if-not
        (lambda (skill)
          (mevedel-skills--path-matches-p path (mevedel-skill-path-patterns skill)))
        (mevedel-session-skills session)))))
  nil)

;;;###autoload
(defun mevedel-skills-install-activation-hook ()
  "Install discovery at the shared native/nested post-tool pipeline boundary."
  (add-hook 'mevedel-post-tool-use-functions
            #'mevedel-skills--post-tool-activate nil t))

(provide 'mevedel-skills-prompt)
;;; mevedel-skills-prompt.el ends here
