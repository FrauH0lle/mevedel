;;; niwa-skills-input.el -- Skill input handling -*- lexical-binding: t -*-

;;; Commentary:

;; Owns user skill token binding, raw gptel dispatch, inline attachment
;; projection, and direct fork-result insertion.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'gptel)
(require 'niwa-mention-bindings)
(require 'niwa-utilities)

;; `cl-extra'
(declare-function cl-some "cl-extra" (predicate &rest sequences))

;; `cl-seq'
(declare-function cl-position "cl-seq" (item sequence &rest keys))

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(defvar gptel-post-response-functions)
(defvar gptel-response-separator)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-at
                  "niwa-mention-bindings" (text start end kind token))
(declare-function niwa-mention-bindings-copy-text
                  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-ranges
                  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-set
                  "niwa-mention-bindings" (start end binding &optional object))
(declare-function niwa-mention-bindings-skill-token-occurrences
                  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-starting-at
                  "niwa-mention-bindings" (text start))
(declare-function niwa-mention-bindings-valid-p
                  "niwa-mention-bindings" (text))

;; `niwa-mentions'
(declare-function niwa-mentions-replace-with-placeholder
                  "niwa-mentions" (start end placeholder))
(autoload 'niwa-mentions-replace-with-placeholder "niwa-mentions")

;; `niwa-reminders'
(declare-function niwa-reminders-stage-entry
                  "niwa-reminders" (fsm type body &optional commit))
(autoload 'niwa-reminders-stage-entry "niwa-reminders")

;; `niwa-resource'
(declare-function niwa-resource-encode-component
                  "niwa-resource" (value))
(declare-function niwa-resource-execute
                  "niwa-resource" (attempt &optional executor options))
(declare-function niwa-resource-prepare
                  "niwa-resource" (operation address context))
(declare-function niwa-resource-skill-digest
                  "niwa-resource" (source-file))
(autoload 'niwa-resource-encode-component "niwa-resource")
(autoload 'niwa-resource-execute "niwa-resource")
(autoload 'niwa-resource-prepare "niwa-resource")
(autoload 'niwa-resource-skill-digest "niwa-resource")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-skills-core'
(declare-function niwa-session-get-skill
                  "niwa-skills-core" (session name))
(declare-function niwa-session-get-skill-by-source
                  "niwa-skills-core" (session source-file))
(declare-function niwa-skill-name "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-p "niwa-skills-core" (object))
(declare-function niwa-skill-source-file "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-user-invocable-p
                  "niwa-skills-core" (cl-x) t)
(declare-function niwa-skills-install
                  "niwa-skills-core" (session &optional buffer))
(declare-function niwa-skills-skill-enabled-p
                  "niwa-skills-core" (skill))
(declare-function niwa-skills-source-key
                  "niwa-skills-core" (source-file))
(autoload 'niwa-session-get-skill "niwa-skills-core")
(autoload 'niwa-session-get-skill-by-source "niwa-skills-core")
(autoload 'niwa-skill-name "niwa-skills-core")
(autoload 'niwa-skill-p "niwa-skills-core")
(autoload 'niwa-skill-source-file "niwa-skills-core")
(autoload 'niwa-skill-user-invocable-p "niwa-skills-core")
(autoload 'niwa-skills-install "niwa-skills-core")
(autoload 'niwa-skills-skill-enabled-p "niwa-skills-core")
(autoload 'niwa-skills-source-key "niwa-skills-core")

;; `niwa-skills-invoke'
(declare-function niwa-skills-activate-context
                  "niwa-skills-invoke" (origin &rest keys))
(declare-function niwa-skills-clear-pending-context
                  "niwa-skills-invoke" ())
(declare-function niwa-skills-format-attachment
                  "niwa-skills-invoke" (attachment))
(declare-function niwa-skills-format-model-input
                  "niwa-skills-invoke" (prepared))
(declare-function niwa-skills-invoke
                  "niwa-skills-invoke"
                  (skill arguments callback &rest keys))
(declare-function niwa-skills-prepare-many
                  "niwa-skills-invoke" (roots callback &rest keys))
(autoload 'niwa-skills-activate-context "niwa-skills-invoke")
(autoload 'niwa-skills-clear-pending-context "niwa-skills-invoke")
(autoload 'niwa-skills-format-attachment "niwa-skills-invoke")
(autoload 'niwa-skills-format-model-input "niwa-skills-invoke")
(autoload 'niwa-skills-invoke "niwa-skills-invoke")
(autoload 'niwa-skills-prepare-many "niwa-skills-invoke")

;; `niwa-skills-syntax'
(declare-function niwa-skills-syntax-markdown-code-ranges
                  "niwa-skills-syntax" (text))
(autoload 'niwa-skills-syntax-markdown-code-ranges
  "niwa-skills-syntax")

;; `niwa-structs'
(declare-function niwa-request-id "niwa-structs" (cl-x))
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-format
                  "niwa-tool-render-data" (render-data &optional tool-use-id))
(autoload 'niwa-tool-render-data-format "niwa-tool-render-data")

;; `niwa-transcript'
(declare-function niwa-transcript-prompt-transform-start
                  "niwa-transcript" ())
(declare-function niwa-transcript-restore-ignored-properties
                  "niwa-transcript" (start end))
(autoload 'niwa-transcript-prompt-transform-start "niwa-transcript")
(autoload 'niwa-transcript-restore-ignored-properties "niwa-transcript")

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))
(autoload 'niwa--format-hook-audit-record "niwa-transcript-audit")

;; `niwa-turn'
(declare-function niwa--complete-turn "niwa-turn" (fsm))
(declare-function niwa-request-begin
                  "niwa-turn" (session &optional options))
(autoload 'niwa--complete-turn "niwa-turn")
(autoload 'niwa-request-begin "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--clear-user-turn-gptel-properties
                  "niwa-utilities" (start end))
(autoload 'niwa--clear-user-turn-gptel-properties "niwa-utilities")

;; `text-property-search'
(declare-function prop-match-end "text-property-search" (match))
(declare-function text-property-search-backward
                  "text-property-search"
                  (property &optional value predicate not-current))

;;
;;; Pending input state

(defvar-local niwa-skills-input--pending-inline-attachments nil
  "Prepared inline `$skill' attachments for the next prompt transform.

Each entry is a plist with :name, :body, :skill, and :arguments.
The stash lives on the chat/data buffer.  It is populated before
`gptel-send' and consumed by
`niwa-skills-input-transform-inline-attachments' in gptel's
temporary prompt buffer.")

(put 'niwa-skills-input--pending-inline-attachments 'permanent-local t)

;;
;;; Inline skill projection

(defun niwa-skills-input--inline-display-text (name arguments)
  "Return the compact view text for inline skill NAME and ARGUMENTS."
  (if (and arguments (not (string-empty-p arguments)))
      (format "$%s %s" name arguments)
    (format "$%s" name)))

(defun niwa-skills-input--format-inline-render-data
    (skill arguments prompt attachments)
  "Return hidden render-data for an expanded inline user SKILL.

ARGUMENTS is the raw user argument string.  PROMPT is the prepared root skill
body.  ATTACHMENTS are the prepared required-attachment plists whose names and
formatted bodies supply the view's collapsed attachment row.

The block is ignored by gptel and consumed by `niwa-view' so the
data buffer keeps the expanded prompt while the view can show the
original `$skill' invocation compactly."
  (let* ((name (niwa-skill-name skill))
         (data (list :kind 'inline-skill
                     :name name
                     :arguments arguments
                     :display-text
                     (niwa-skills-input--inline-display-text
                      name arguments)
                     :prompt prompt
                     :attachments
                     (mapcar (lambda (attachment)
                               (list :name (plist-get attachment :name)
                                     :body
                                     (niwa-skills-format-attachment
                                      attachment)))
                             attachments))))
    (niwa-tool-render-data-format data)))

(defun niwa-skills-input--format-inline-attachment-render-data (attachments)
  "Return hidden render-data for inline skill ATTACHMENTS."
  (let ((data (list :kind 'inline-skill-attachments
                    :skills (mapcar
                             (lambda (attachment)
                               (list :name (plist-get attachment :name)))
                             attachments))))
    (niwa-tool-render-data-format data)))

(defun niwa-skills-input--replace-inline-attachment-mentions
    (attachments start end)
  "Replace prompt mentions for prepared ATTACHMENTS between START and END.
Return attachments that were actually referenced, preserving first use."
  (let* ((text (buffer-substring start end))
         (by-name (make-hash-table :test #'equal))
         (by-source (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'equal))
         replacements
         used)
    (dolist (attachment attachments)
      (if-let* ((source-file (plist-get attachment :source-file)))
          (puthash source-file attachment by-source)
        (puthash (plist-get attachment :name) attachment by-name)))
    (dolist (token
             (niwa-skills-input-scan-tokens
              text
              (lambda (candidate token-start token-end)
                (if-let* ((binding
                           (niwa-mention-bindings-at
                            text token-start token-end 'skill
                            (concat "$" candidate))))
                    (gethash (plist-get binding :source-file) by-source)
                  (gethash candidate by-name)))))
      (let ((name (plist-get token :name))
            (attachment (plist-get token :value)))
        (push (list (+ start (plist-get token :start))
                    (+ start (plist-get token :end))
                    (format "[skill:%s -- %s]"
                            name
                            (if (plist-get attachment :unavailable)
                                "unavailable"
                              "attached")))
              replacements)
        (dolist (item (append (plist-get attachment :required-attachments)
                              (list attachment)))
          (let ((key (cons (or (plist-get item :source-file)
                               (plist-get item :name))
                           (or (plist-get item :arguments) ""))))
            (unless (gethash key seen)
              (puthash key t seen)
              (push item used))))))
    (dolist (replacement (sort replacements
                               (lambda (a b) (> (car a) (car b)))))
      (niwa-mentions-replace-with-placeholder
       (nth 0 replacement) (nth 1 replacement) (nth 2 replacement)))
    (nreverse used)))

(defun niwa-skills-input-transform-inline-attachments (fsm)
  "Expand prepared inline `$skill' attachments into prompt context.
Token replacement stays in the prompt text; the attachment bodies are
staged as reminder entries for the WAIT injector."
  (when-let* ((chat-buffer (and fsm (plist-get (gptel-fsm-info fsm) :buffer)))
              ((buffer-live-p chat-buffer))
              (attachments
               (buffer-local-value
                'niwa-skills-input--pending-inline-attachments chat-buffer)))
    (with-current-buffer chat-buffer
      (setq-local niwa-skills-input--pending-inline-attachments nil))
    (let ((used (niwa-skills-input--replace-inline-attachment-mentions
                 attachments
                 (niwa-transcript-prompt-transform-start)
                 (point-max))))
      (dolist (attachment used)
        (unless (plist-get attachment :unavailable)
          (niwa-reminders-stage-entry
           fsm 'skill-attachment
           (niwa-skills-format-attachment attachment)))))))


;;
;;; User skill dispatch

(defun niwa-skills-input-current-prompt-region ()
  "Return (START . END) of the pending prompt text in the chat buffer.

The pending prompt starts after the last text carrying a `gptel'
property, which marks prior LLM output.  Without one, the whole buffer
is the pending prompt.  END is always `point-max'.  Returns nil only
for an empty buffer."
  (save-excursion
    (goto-char (point-max))
    (let ((match (text-property-search-backward 'gptel nil nil t)))
      (cond
       (match (cons (prop-match-end match) (point-max)))
       ((< (point-min) (point-max))
        (cons (point-min) (point-max)))))))


(defun niwa-skills-input-parse-prefixed-line (text prefix)
  "Parse TEXT for a leading PREFIX command line.
Returns (NAME ARGS OFFSET) when TEXT starts (after optional leading
whitespace) with PREFIX followed by an identifier, or nil otherwise.
NAME is the command name; ARGS is the rest of TEXT after the
command name (the remainder of the first line plus every
subsequent line, joined and trimmed); OFFSET is the 0-based
character position of PREFIX within TEXT."
  (let* ((trimmed (string-trim-left text))
         (offset (- (length text) (length trimmed))))
    (when (and (> (length trimmed) 1) (eq (aref trimmed 0) prefix))
      (let* ((line-end (or (string-match "\n" trimmed) (length trimmed)))
             (line (substring trimmed 1 line-end))
             (rest (substring trimmed line-end))
             (space (string-match "[ \t]" line))
             (name (if space (substring line 0 space) line))
             (first-line-args (if space (substring line space) ""))
             (args (string-trim (concat first-line-args rest))))
        (when (string-match-p "\\`[A-Za-z0-9_.:-]+\\'" name)
          (list name args offset))))))

(defun niwa-skills-input-parse-skill-line (text)
  "Parse TEXT for a leading `$skill [args]' line.

User skill invocations take the user's prompt body as ARGS, and prompt
bodies are naturally multi-line."
  (niwa-skills-input-parse-prefixed-line text ?$))

(defun niwa-skills-input-escaped-position-p (text pos)
  "Return non-nil when POS in TEXT is escaped by an odd backslash run."
  (let ((i (1- pos))
        (count 0))
    (while (and (>= i 0)
                (eq (aref text i) ?\\))
      (cl-incf count)
      (cl-decf i))
    (not (zerop (% count 2)))))

(defun niwa-skills-input--word-char-p (ch)
  "Return non-nil when CH is an ASCII word character."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun niwa-skills-input--quote-delimiter-p (text pos)
  "Return non-nil when POS is a quote delimiter in TEXT."
  (and (not (niwa-skills-input-escaped-position-p text pos))
       (let ((quote (aref text pos)))
         (and (memq quote '(?\" ?\'))
              (or (eq quote ?\")
                  (not (and (niwa-skills-input--word-char-p
                             (and (> pos 0) (aref text (1- pos))))
                            (niwa-skills-input--word-char-p
                             (and (< (1+ pos) (length text))
                                  (aref text (1+ pos)))))))))))

(defun niwa-skills-input--quoted-inline-skill-p (text start end)
  "Return non-nil when TEXT START..END is inside single/double quotes."
  (let ((line-start (1+ (or (cl-position ?\n text :end start :from-end t)
                            -1)))
        (line-end (or (cl-position ?\n text :start end)
                      (length text)))
        quoted)
    (dolist (quote '(?\" ?\') quoted)
      (let ((open nil)
            (pos (1- start)))
        (while (and (not open) (>= pos line-start))
          (when (and (eq (aref text pos) quote)
                     (niwa-skills-input--quote-delimiter-p text pos))
            (setq open pos))
          (cl-decf pos))
        (when open
          (setq pos end)
          (while (and (not quoted) (< pos line-end))
            (when (and (eq (aref text pos) quote)
                       (niwa-skills-input--quote-delimiter-p text pos))
              (setq quoted t))
            (cl-incf pos)))))))

(defun niwa-skills-input--skill-token-live-p
    (text start end first-nonspace code-ranges &optional allow-root)
  "Return non-nil when TEXT START..END is a live `$skill' token.
When ALLOW-ROOT is nil, exclude the leading root skill invocation."
  (and (or allow-root (not (= start first-nonspace)))
       (not (niwa-skills-input-escaped-position-p text start))
       (not (niwa-skills-input--quoted-inline-skill-p text start end))
       (not (cl-some (lambda (range)
                       (and (< start (cdr range))
                            (< (car range) end)))
                     code-ranges))))

(defun niwa-skills-input-scan-tokens (text resolver &optional allow-root)
  "Return live `$skill' tokens in TEXT resolved through RESOLVER.
Each token has `:start', `:end', `:name', and `:value'.  When
ALLOW-ROOT is non-nil, include the leading root invocation."
  (let ((first-nonspace (or (string-match-p "\\S-" text) -1))
        (code-ranges (niwa-skills-syntax-markdown-code-ranges text))
        tokens)
    (dolist (occurrence
             (niwa-mention-bindings-skill-token-occurrences text))
      (let* ((start (plist-get occurrence :start))
             (resolved
              (cl-loop
               for candidate in (plist-get occurrence :candidates)
               for end = (+ start 1 (length candidate))
               for value = (and
                            (niwa-skills-input--skill-token-live-p
                             text start end first-nonspace code-ranges
                             allow-root)
                            (funcall resolver candidate start end))
               when value return (cons candidate value)))
             (name (car-safe resolved))
             (end (and name (+ start 1 (length name)))))
        (when resolved
          (push (list :start start :end end :name name
                      :value (cdr resolved))
                tokens))))
    (nreverse tokens)))

(defun niwa-skills-input-refresh-bound-input (text session)
  "Refresh SESSION discovery when TEXT carries atomic skill bindings.
Return TEXT unchanged.  Call this at a submission boundary so an exact
source resolves against its latest on-disk contents even when automatic
modification checking is disabled."
  (when (cl-some
         (lambda (range)
           (eq 'skill
               (plist-get (plist-get range :binding) :kind)))
         (niwa-mention-bindings-ranges text))
    (niwa-skills-install session (current-buffer)))
  text)

(defun niwa-skills-input-prepare-user-input (text session)
  "Prepare user TEXT with valid skill tokens bound in SESSION.
Existing valid bindings remain authoritative.  Manually typed known
tokens receive a binding when their skill has a stable source file.
Existing bindings trigger one explicit discovery refresh so exact
sources are checked before the submission leaves the composer."
  (unless (niwa-mention-bindings-valid-p text)
    (user-error "Malformed mention binding"))
  (let ((result (niwa-mention-bindings-copy-text text)))
    (niwa-skills-input-refresh-bound-input result session)
    (dolist (token
             (niwa-skills-input-scan-tokens
              result
              (lambda (candidate start end)
                (let ((binding (niwa-mention-bindings-starting-at
                                result start)))
                  (if binding
                      (niwa-mention-bindings-at
                       result start end 'skill (concat "$" candidate))
                    (niwa-session-get-skill session candidate))))
              t))
      (let ((start (plist-get token :start))
            (end (plist-get token :end))
            (skill (plist-get token :value)))
        (unless (get-text-property start 'niwa-mention-binding result)
          (when-let* (((niwa-skill-p skill))
                      (source-file (niwa-skill-source-file skill)))
            (niwa-mention-bindings-set
             start end
             (list :kind 'skill
                   :token (concat "$" (plist-get token :name))
                   :source-file source-file)
             result)))))
    result))

(defun niwa-skills-input--bound-source-available-p (name source-file session)
  "Return non-nil when SOURCE-FILE still resolves as skill NAME.
The resource resolver checks the exact source identity and current enabled
state; the executor callback intentionally does not read the skill body."
  (condition-case nil
      (let* ((address
              (format "skill://%s@%s"
                      (niwa-resource-encode-component name)
                      (niwa-resource-skill-digest source-file)))
             (attempt (niwa-resource-prepare
                       'read address (list :session session))))
        (niwa-resource-execute
         attempt
         (lambda (path _address)
           (and (file-regular-p path)
                (file-readable-p path))))
    )
    (error nil)))

(defun niwa-skills-input-resolve-mention
    (text session start end name)
  "Return the resolution outcome for user skill NAME at START..END in TEXT.
Success is `(:status ok :skill SKILL)'.  A valid binding whose target is
unavailable returns `(:status unavailable :message MESSAGE)'.
Unknown unbound names are successful with a nil skill."
  (let* ((token (concat "$" name))
         (property (and (< start (length text))
                        (get-text-property
                         start 'niwa-mention-binding text)))
         (bound (niwa-mention-bindings-starting-at text start))
         (binding (and bound
                       (niwa-mention-bindings-at
                        text start end 'skill token)))
         (source-file (plist-get binding :source-file))
         (skill
          (cond
           (binding
            (and (niwa-skills-input--bound-source-available-p
                  name source-file session)
                 (niwa-session-get-skill-by-source
                  session source-file)))
           (bound nil)
           (t (niwa-session-get-skill session name)))))
    (cond
     ((and property (null bound))
      (user-error "Malformed mention binding"))
     ((and binding (null skill))
      (list :status 'unavailable
            :message (format "bound skill $%s is unavailable" name)))
     ((and skill (not (niwa-skills-skill-enabled-p skill)))
      (list :status 'unavailable
            :message (format "skill $%s is disabled" name)))
     ((and skill (not (niwa-skill-user-invocable-p skill)))
      (list :status 'unavailable
            :message (format "skill $%s is not user-invocable" name)))
     (t (list :status 'ok :skill skill)))))

(defun niwa-skills-input--inline-skill-mentions
    (text session &optional allow-root)
  "Return live inline `$skill' mentions in TEXT for SESSION.

Unknown `$foo' text is ignored.  Known but unavailable skills remain
occurrences marked with :unavailable and :message so dispatch can annotate
them without preparing a skill body.  When ALLOW-ROOT is non-nil, include a
leading mention after command dispatch has declined it."
  (unless (niwa-mention-bindings-valid-p text)
    (user-error "Malformed mention binding"))
  (let* ((tokens
          (and session
               (niwa-skills-input-scan-tokens
                text
                (lambda (candidate start end)
                  (let ((outcome
                         (niwa-skills-input-resolve-mention
                          text session start end candidate)))
                    (if (eq (plist-get outcome :status) 'ok)
                        (plist-get outcome :skill)
                      (list :unavailable t
                            :message (plist-get outcome :message)))))
                allow-root)))
        (seen (make-hash-table :test #'equal))
        mentions)
    (while tokens
      (let* ((token (pop tokens))
             (start (plist-get token :start))
             (end (plist-get token :end))
             (name (plist-get token :name))
             (skill (plist-get token :value))
             (binding (niwa-mention-bindings-starting-at text start))
             (source-file
              (or (plist-get binding :source-file)
                  (and (niwa-skill-p skill)
                       (niwa-skill-source-file skill)))))
        (cond
           ((and (listp skill) (plist-get skill :unavailable))
            (push (list :name name
                        :start start
                        :end end
                        :source-file source-file
                        :unavailable t
                        :message (plist-get skill :message))
                  mentions))
           ((not (gethash (or (niwa-skill-source-file skill)
                              (niwa-skill-name skill))
                          seen))
            (puthash (or (niwa-skill-source-file skill)
                         (niwa-skill-name skill))
                     t seen)
            (push (list :name name
                        :start start
                        :end end
                        :source-file source-file
                        :skill skill)
                  mentions)))))
    (nreverse mentions)))

(defun niwa-skills-input-ensure-fresh-line ()
  "Leave point at the start of an empty line with a blank line above.
Called after deleting a slash-command region so the next insertion or
cursor rest position is visually separated from the preceding response."
  (unless (bolp) (insert "\n"))
  (unless (save-excursion
            (forward-line -1)
            (looking-at-p "^[ \t]*$"))
    (insert "\n")))

(defun niwa-skills-input-insert-fork-result (outcome)
  "Insert fork skill OUTCOME as an assistant response in the data buffer.

The current buffer must be the data buffer.  This path is used when a
fork skill suppresses the main `gptel-send'; it records the retained
agent's final result as the assistant side of that turn and
runs the normal post-response hooks so the view and persistence layers
observe the completed response."
  (let* ((request niwa--current-request)
         (scope (niwa-diagnostic-scope (current-buffer)))
         (render-data (plist-get outcome :render-data))
         (hook-audits (plist-get outcome :hook-audits))
         (result (or (plist-get outcome :result)
                     "Fork skill produced no result."))
         (result (if render-data
                     (concat result
                             (niwa-tool-render-data-format
                              render-data))
                   result)))
    (unless (bound-and-true-p niwa--current-request)
      (when (bound-and-true-p niwa--session)
        (niwa-session-artifacts-assert-new-mutation-authority
         niwa--session)
        (setq request
              (niwa-request-begin
               niwa--session))))
    (goto-char (point-max))
    (when-let* ((synthetic (plist-get outcome :synthetic-user-message)))
      (let ((user-turn-start (point)))
        (unless (bolp) (insert "\n"))
        (insert synthetic)
        (unless (bolp) (insert "\n"))
        (niwa--clear-user-turn-gptel-properties
         user-turn-start (point))))
    (when hook-audits
      (insert (mapconcat #'niwa--format-hook-audit-record
                         hook-audits "")))
    (unless (bolp) (insert "\n"))
    (insert gptel-response-separator)
    (let ((start (point)))
      (insert result)
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (let ((end (point)))
        (add-text-properties start end '(gptel response))
        (niwa-transcript-restore-ignored-properties start end)
        (condition-case err
            (run-hook-with-args 'gptel-post-response-functions start end)
          (error
           (niwa-diagnostic-warning
            scope 'niwa
            (format "Fork post-response hook failed: %s"
                    (error-message-string err))
            :warning)))
        (niwa-transcript-restore-ignored-properties start end)
        (niwa--complete-turn request)
        (gptel--update-status " Ready" 'success)))))

(defun niwa-skills-input-command-delete-start (command-pos)
  "Return where deleting a command starting at COMMAND-POS begins.
A command preceded only by indentation takes its whole line with it."
  (let ((line-start (save-excursion
                      (goto-char command-pos)
                      (line-beginning-position))))
    (if (string-match-p "\\`[ \t]*\\'"
                        (buffer-substring-no-properties line-start command-pos))
        line-start
      command-pos)))

(defun niwa-skills-input--handle-user-skill-outcome
    (skill outcome delete-start region-end continue-fn &optional scope)
  "Apply user skill OUTCOME in the current data buffer.

SKILL is the invoked user skill.  DELETE-START and REGION-END bound
the original `$skill' text.

CONTINUE-FN, when non-nil, resumes the original `gptel-send' after an
inline body has been inserted.  Fork outcomes suppress that send and
insert their result when the retained agent finishes.  Async callers supply
SCOPE before preparation can switch session ownership."
  (pcase (plist-get outcome :status)
    ('ok
     (pcase (plist-get outcome :kind)
       ('inline
        (delete-region delete-start region-end)
        (niwa-skills-input-ensure-fresh-line)
        (let* ((prompt (or (plist-get outcome :body)
                           (format "Skill '%s' produced no body."
                                   (niwa-skill-name skill))))
               (body (if (plist-get outcome :body)
                         (niwa-skills-format-model-input outcome)
                       prompt)))
          (insert body)
          (insert
           (niwa-skills-input--format-inline-render-data
            skill (plist-get outcome :arguments) prompt
            (plist-get outcome :required-attachments))))
        (when continue-fn
          (funcall continue-fn))
        'skill)
       ('fork
        (message "Skill '%s' dispatched; waiting for agent result..."
                 (niwa-skill-name skill))
        (niwa-skills-input-insert-fork-result outcome)
        'skill)
       (_
        (niwa-diagnostic-message
         (or scope (niwa-diagnostic-scope (current-buffer)))
         "Skill '%s' returned an unsupported outcome: %S"
                 (niwa-skill-name skill) outcome)
        'unknown)))
    (_
     (niwa-diagnostic-message
      (or scope (niwa-diagnostic-scope (current-buffer)))
      "Skill '%s' failed: %s"
              (niwa-skill-name skill)
              (plist-get outcome :message))
     'unknown)))

(defun niwa-skills-input-dispatch-command (&optional continue-fn)
  "Parse and dispatch a `$skill' invocation in the current chat buffer.

Returns:
- `skill'   a skill was expanded into the prompt region; caller
            should proceed with the send if CONTINUE-FN was nil.
- `unknown' preparation failed and the caller should abort the send.
- nil       no invocable leading `$skill' is present; caller should scan the
            complete prompt for inline attachments."
  (when-let* ((region (niwa-skills-input-current-prompt-region))
              (text (buffer-substring (car region) (cdr region)))
              (parsed (niwa-skills-input-parse-skill-line text)))
    (let* ((name (nth 0 parsed))
           (args (nth 1 parsed))
           (skill-pos (+ (car region) (nth 2 parsed)))
           (outcome
            (niwa-skills-input-resolve-mention
             text niwa--session
             (nth 2 parsed)
             (+ (nth 2 parsed) 1 (length name))
             name))
           (skill (plist-get outcome :skill)))
      (if (not (eq (plist-get outcome :status) 'ok))
          nil
        (when skill
          (let* ((buffer (current-buffer))
                 (scope (niwa-diagnostic-scope buffer))
                 ;; Preparation is asynchronous whenever the body injects, so
                 ;; the draft can move, shrink, or be replaced before the
                 ;; outcome arrives.  Markers follow an edit where integers
                 ;; would address the wrong text or past the end of the
                 ;; buffer, and the snapshot says whether this is still the
                 ;; command that was dispatched.
                 (delete-start (copy-marker
                                (niwa-skills-input-command-delete-start
                                 skill-pos)
                                nil))
                 (region-end (copy-marker (cdr region) t))
                 (snapshot (buffer-substring-no-properties
                            delete-start region-end)))
            (niwa-skills-invoke
             skill args
             (lambda (outcome)
               (unwind-protect
                   (when (and (buffer-live-p buffer)
                              (marker-position delete-start)
                              (marker-position region-end))
                     (with-current-buffer buffer
                       ;; The command may sit outside a narrowing the user
                       ;; applied meanwhile, and reading it must not signal.
                       (if (equal snapshot
                                  (save-restriction
                                    (widen)
                                    (buffer-substring-no-properties
                                     delete-start region-end)))
                           (niwa-skills-input--handle-user-skill-outcome
                            skill outcome
                            (marker-position delete-start)
                            (marker-position region-end)
                            continue-fn scope)
                         ;; Activation happened before preparation, so the
                         ;; skill's permission rules, model, and effort are
                         ;; staged in this buffer.  Abandoning without
                         ;; releasing them would apply them to whatever the
                         ;; user sends next.
                         (niwa-skills-input-clear-pending)
                         (niwa-diagnostic-message scope
                          (if (eq (plist-get outcome :kind) 'fork)
                              "skill draft changed during preparation; the fork agent's result was discarded"
                            "skill draft changed during preparation, not sent")))))
                 (set-marker delete-start nil)
                 (set-marker region-end nil)))
             :origin 'user)
            ;; The outcome decides what happened, and it may arrive after this
            ;; returns; the caller only needs to know a skill took the send.
            'skill))))))

(defun niwa-skills-input--prepare-inline-attachments (mentions callback)
  "Prepare inline skill MENTIONS, then call CALLBACK.
CALLBACK receives (:status ok :attachments LIST) or an error plist."
  (let ((available (cl-remove-if (lambda (mention)
                                   (plist-get mention :unavailable))
                                 mentions)))
    (if (null available)
        (funcall callback (list :status 'ok :attachments mentions))
      (niwa-skills-prepare-many
       (mapcar (lambda (mention)
                 (list :skill (plist-get mention :skill)
                       :arguments "" :role 'instruction
                       :policy-owner-p nil))
               available)
       (lambda (aggregate)
         (if (not (eq (plist-get aggregate :status) 'ok))
             (funcall callback aggregate)
           (let ((outcomes (plist-get aggregate :outcomes))
                 (records (plist-get aggregate :invoked-skills))
                 (seen (make-hash-table :test #'equal))
                 attachments)
             (dolist (mention mentions)
               (if (plist-get mention :unavailable)
                   (push mention attachments)
                 (let* ((outcome (pop outcomes))
                        (root
                         (list :name (plist-get mention :name)
                               :start (plist-get mention :start)
                               :end (plist-get mention :end)
                               :source-file (plist-get mention :source-file)
                               :skill (plist-get mention :skill)
                               :arguments ""
                               :body (plist-get outcome :body)
                               :required-attachments
                               (plist-get outcome :required-attachments))))
                   (dolist (attachment
                            (append (plist-get outcome :required-attachments)
                                    (list root)))
                     (let ((key
                            (cons
                             (or (niwa-skills-source-key
                                  (plist-get attachment :source-file))
                                 (plist-get attachment :skill))
                             (or (plist-get attachment :arguments) ""))))
                       (unless (gethash key seen)
                         (puthash key t seen)
                         (push attachment attachments)))))))
             (setq attachments (nreverse attachments))
             (when attachments
               (setf (plist-get (car attachments) :invoked-skills) records))
             (funcall callback
                      (list :status 'ok :attachments attachments)))))
       :origin 'user))))

(defun niwa-skills-input--stage-inline-attachments (attachments)
  "Store prepared inline ATTACHMENTS and return their render-data block."
  (setq-local niwa-skills-input--pending-inline-attachments attachments)
  (when-let* ((warnings
               (delq nil (mapcar (lambda (attachment)
                                   (plist-get attachment :message))
                                 attachments))))
    (niwa-diagnostic-message
     (niwa-diagnostic-scope (current-buffer))
     "%s" (mapconcat #'identity warnings "; ")))
  (let ((records
         (mapcan (lambda (attachment)
                   (copy-sequence (plist-get attachment :invoked-skills)))
                 attachments)))
    (dolist (record records)
      (niwa-skills-activate-context 'user :invoked-skill record)))
  (niwa-skills-input--format-inline-attachment-render-data attachments))

(defun niwa-skills-input-clear-pending ()
  "Clear pending inline attachment request state in the current buffer."
  (niwa-skills-clear-pending-context)
  (setq-local niwa-skills-input--pending-inline-attachments nil))

(defun niwa-skills-input-dispatch-inline-attachments
    (&optional continue-fn allow-root)
  "Prepare inline `$skill' attachments in the current prompt.
Return `skill' when preparation took ownership of continuing the send,
or nil when no inline attachments exist.  Unavailable skills are annotated
without blocking the send."
  (when-let* ((region (niwa-skills-input-current-prompt-region))
              (session (and (bound-and-true-p niwa--session)
                            niwa--session))
              ((not (bound-and-true-p
                     niwa-skills-input--pending-inline-attachments)))
              (text (buffer-substring (car region) (cdr region)))
              (mentions (niwa-skills-input--inline-skill-mentions
                         text session allow-root)))
    (let* ((buffer (current-buffer))
           (scope (niwa-diagnostic-scope buffer)))
      (niwa-skills-input--prepare-inline-attachments
       mentions
       (lambda (outcome)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (pcase (plist-get outcome :status)
               ('ok
                (goto-char (point-max))
                (insert
                 (niwa-skills-input--stage-inline-attachments
                  (plist-get outcome :attachments)))
                (when continue-fn
                  (funcall continue-fn)))
               (_
                (niwa-skills-input-clear-pending)
                (niwa-diagnostic-message scope "Inline skill failed: %s"
                         (or (plist-get outcome :message)
                             "unknown error"))))))))
      'skill)))


(provide 'niwa-skills-input)
;;; niwa-skills-input.el ends here
