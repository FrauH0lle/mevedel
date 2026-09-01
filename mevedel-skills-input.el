;;; mevedel-skills-input.el -- Skill input handling -*- lexical-binding: t -*-

;;; Commentary:

;; Owns user skill token binding, raw gptel dispatch, inline attachment
;; projection, and direct fork-result insertion.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'gptel)
(require 'mevedel-mention-bindings)

;; `cl-extra'
(declare-function cl-some "cl-extra" (predicate &rest sequences))

;; `cl-seq'
(declare-function cl-position "cl-seq" (item sequence &rest keys))

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(defvar gptel-post-response-functions)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-at
                  "mevedel-mention-bindings" (text start end kind token))
(declare-function mevedel-mention-bindings-copy-text
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-ranges
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings" (start end binding &optional object))
(declare-function mevedel-mention-bindings-skill-token-occurrences
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-starting-at
                  "mevedel-mention-bindings" (text start))
(declare-function mevedel-mention-bindings-valid-p
                  "mevedel-mention-bindings" (text))

;; `mevedel-mentions'
(declare-function mevedel-mentions-replace-with-placeholder
                  "mevedel-mentions" (start end placeholder))
(autoload 'mevedel-mentions-replace-with-placeholder "mevedel-mentions")

;; `mevedel-resource'
(declare-function mevedel-resource-encode-component
                  "mevedel-resource" (value))
(declare-function mevedel-resource-execute
                  "mevedel-resource" (attempt &optional executor options))
(declare-function mevedel-resource-prepare
                  "mevedel-resource" (operation address context))
(declare-function mevedel-resource-skill-digest
                  "mevedel-resource" (source-file))
(autoload 'mevedel-resource-encode-component "mevedel-resource")
(autoload 'mevedel-resource-execute "mevedel-resource")
(autoload 'mevedel-resource-prepare "mevedel-resource")
(autoload 'mevedel-resource-skill-digest "mevedel-resource")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(autoload 'mevedel-session-artifacts-assert-new-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-skills-core'
(declare-function mevedel-session-get-skill
                  "mevedel-skills-core" (session name))
(declare-function mevedel-session-get-skill-by-source
                  "mevedel-skills-core" (session source-file))
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-p "mevedel-skills-core" (object))
(declare-function mevedel-skill-source-file "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-user-invocable-p
                  "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skills-skill-enabled-p
                  "mevedel-skills-core" (skill))
(declare-function mevedel-skills-install
                  "mevedel-skills-core" (session &optional buffer))
(autoload 'mevedel-session-get-skill "mevedel-skills-core")
(autoload 'mevedel-session-get-skill-by-source "mevedel-skills-core")
(autoload 'mevedel-skill-name "mevedel-skills-core")
(autoload 'mevedel-skill-p "mevedel-skills-core")
(autoload 'mevedel-skill-source-file "mevedel-skills-core")
(autoload 'mevedel-skill-user-invocable-p "mevedel-skills-core")
(autoload 'mevedel-skills-skill-enabled-p "mevedel-skills-core")
(autoload 'mevedel-skills-install "mevedel-skills-core")

;; `mevedel-skills-invoke'
(declare-function mevedel-skills-activate-context
                  "mevedel-skills-invoke" (origin &rest keys))
(declare-function mevedel-skills-clear-pending-context
                  "mevedel-skills-invoke" ())
(declare-function mevedel-skills-invoke
                  "mevedel-skills-invoke"
                  (skill arguments callback &rest keys))
(declare-function mevedel-skills-prepare
                  "mevedel-skills-invoke"
                  (skill arguments callback &rest keys))
(autoload 'mevedel-skills-activate-context "mevedel-skills-invoke")
(autoload 'mevedel-skills-clear-pending-context "mevedel-skills-invoke")
(autoload 'mevedel-skills-invoke "mevedel-skills-invoke")
(autoload 'mevedel-skills-prepare "mevedel-skills-invoke")

;; `mevedel-skills-preparation'
(declare-function mevedel-skills-preparation-markdown-code-ranges
                  "mevedel-skills-preparation" (text))
(autoload 'mevedel-skills-preparation-markdown-code-ranges
  "mevedel-skills-preparation")

;; `mevedel-reminders'
(declare-function mevedel-reminders-stage-entry
                  "mevedel-reminders" (fsm type body &optional commit))
(autoload 'mevedel-reminders-stage-entry "mevedel-reminders")

;; `mevedel-structs'
(declare-function mevedel-request-id "mevedel-structs" (cl-x))
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-format
                  "mevedel-tool-render-data" (render-data &optional tool-use-id))
(autoload 'mevedel-tool-render-data-format "mevedel-tool-render-data")

;; `mevedel-transcript'
(declare-function mevedel-transcript-prompt-transform-start
                  "mevedel-transcript" ())
(declare-function mevedel-transcript-restore-ignored-properties
                  "mevedel-transcript" (start end))
(autoload 'mevedel-transcript-prompt-transform-start "mevedel-transcript")
(autoload 'mevedel-transcript-restore-ignored-properties "mevedel-transcript")

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))
(autoload 'mevedel--format-hook-audit-record "mevedel-transcript-audit")

;; `mevedel-turn'
(declare-function mevedel--complete-turn "mevedel-turn" (fsm))
(declare-function mevedel-request-begin
                  "mevedel-turn" (session &optional directive-uuid))
(autoload 'mevedel--complete-turn "mevedel-turn")
(autoload 'mevedel-request-begin "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
                  "mevedel-utilities" (start end))
(autoload 'mevedel--clear-user-turn-gptel-properties "mevedel-utilities")

;; `text-property-search'
(declare-function prop-match-end "text-property-search" (match))
(declare-function text-property-search-backward
                  "text-property-search"
                  (property &optional value predicate not-current))

;;
;;; Pending input state

(defvar-local mevedel-skills-input--pending-inline-attachments nil
  "Prepared inline `$skill' attachments for the next prompt transform.

Each entry is a plist with :name, :body, :skill, and :arguments.
The stash lives on the chat/data buffer.  It is populated before
`gptel-send' and consumed by
`mevedel-skills-input-transform-inline-attachments' in gptel's
temporary prompt buffer.")

(put 'mevedel-skills-input--pending-inline-attachments 'permanent-local t)

;;
;;; Inline skill projection

(defun mevedel-skills-input--inline-display-text (name arguments)
  "Return the compact view text for inline skill NAME and ARGUMENTS."
  (if (and arguments (not (string-empty-p arguments)))
      (format "$%s %s" name arguments)
    (format "$%s" name)))

(defun mevedel-skills-input--format-inline-render-data
    (skill arguments expanded-prompt)
  "Return hidden render-data for an expanded inline user SKILL.

ARGUMENTS is the raw user argument string.  EXPANDED-PROMPT is the exact
prepared prompt replacing the invocation.

The block is ignored by gptel and consumed by `mevedel-view' so the
data buffer keeps the expanded prompt while the view can show the
original `$skill' invocation compactly."
  (let* ((name (mevedel-skill-name skill))
         (data (list :kind 'inline-skill
                     :name name
                     :arguments arguments
                     :display-text
                     (mevedel-skills-input--inline-display-text
                      name arguments)
                     :expanded-prompt expanded-prompt))
         (block (mevedel-tool-render-data-format data)))
    block))

(defun mevedel-skills-input--insert-inline-user-skill-render-data
    (skill arguments expanded-prompt)
  "Insert hidden render data for SKILL, ARGUMENTS, and EXPANDED-PROMPT."
  (insert (mevedel-skills-input--format-inline-render-data
           skill arguments expanded-prompt)))

(defun mevedel-skills-input--format-inline-attachment-render-data (attachments)
  "Return hidden render-data for inline skill ATTACHMENTS."
  (let ((data (list :kind 'inline-skill-attachments
                    :skills (mapcar
                             (lambda (attachment)
                               (list :name (plist-get attachment :name)))
                             attachments))))
    (mevedel-tool-render-data-format data)))

(defun mevedel-skills-input-attachment-reminder (attachment)
  "Return system-reminder body for prepared inline skill ATTACHMENT."
  (format "The host already attached the skill `$%s` for this request. You must follow it without calling `Skill`.\n\n%s"
          (plist-get attachment :name)
          (or (plist-get attachment :body) "")))

(defun mevedel-skills-input--replace-inline-attachment-mentions
    (attachments start end)
  "Replace prompt mentions for prepared ATTACHMENTS between START and END.
Return attachments that were actually referenced, preserving first use."
  (let* ((text (buffer-substring start end))
         (by-name (make-hash-table :test #'equal))
         (by-source (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'eq))
         replacements
         used)
    (dolist (attachment attachments)
      (if-let* ((source-file (plist-get attachment :source-file)))
          (puthash source-file attachment by-source)
        (puthash (plist-get attachment :name) attachment by-name)))
    (dolist (token
             (mevedel-skills-input-scan-tokens
              text
              (lambda (candidate token-start token-end)
                (if-let* ((binding
                           (mevedel-mention-bindings-at
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
        (unless (gethash attachment seen)
          (puthash attachment t seen)
          (push attachment used))))
    (dolist (replacement (sort replacements
                               (lambda (a b) (> (car a) (car b)))))
      (mevedel-mentions-replace-with-placeholder
       (nth 0 replacement) (nth 1 replacement) (nth 2 replacement)))
    (nreverse used)))

(defun mevedel-skills-input-transform-inline-attachments (fsm)
  "Expand prepared inline `$skill' attachments into prompt context.
Token replacement stays in the prompt text; the attachment bodies are
staged as reminder entries for the WAIT injector."
  (when-let* ((chat-buffer (and fsm (plist-get (gptel-fsm-info fsm) :buffer)))
              ((buffer-live-p chat-buffer))
              (attachments
               (buffer-local-value
                'mevedel-skills-input--pending-inline-attachments chat-buffer)))
    (with-current-buffer chat-buffer
      (setq-local mevedel-skills-input--pending-inline-attachments nil))
    (let ((used (mevedel-skills-input--replace-inline-attachment-mentions
                 attachments
                 (mevedel-transcript-prompt-transform-start)
                 (point-max))))
      (dolist (attachment used)
        (unless (plist-get attachment :unavailable)
          (mevedel-reminders-stage-entry
           fsm 'skill-attachment
           (mevedel-skills-input-attachment-reminder attachment)))))))


;;
;;; User skill dispatch

(defun mevedel-skills-input-current-prompt-region ()
  "Return (START . END) of the pending prompt text in the chat buffer.

Locates the start of the current user prompt by preferring gptel's
`gptel' text property -- which marks prior LLM responses even when the
user has disabled the prompt prefix -- and falling back to the last
occurrence of the configured prompt prefix.  If neither boundary is
present the whole buffer is treated as the pending prompt.  END is
always `point-max'.  Returns nil only for an empty buffer."
  (save-excursion
    (goto-char (point-max))
    (let* ((match (text-property-search-backward 'gptel nil nil t))
           (prefix (alist-get major-mode gptel-prompt-prefix-alist))
           (has-prefix (and prefix (not (string-empty-p prefix))))
           start)
      (cond
       (match (setq start (prop-match-end match)))
       ((and has-prefix
             (progn (goto-char (point-max))
                    (search-backward prefix nil t)))
        (setq start (+ (point) (length prefix))))
       ((< (point-min) (point-max))
        (setq start (point-min))))
      (when (and start has-prefix)
        (save-excursion
          (goto-char start)
          (when (looking-at-p (regexp-quote prefix))
            (setq start (+ start (length prefix))))))
      (when start
        (cons start (point-max))))))


(defun mevedel-skills-input-parse-prefixed-line (text prefix)
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

(defun mevedel-skills-input-parse-skill-line (text)
  "Parse TEXT for a leading `$skill [args]' line.

User skill invocations take the user's prompt body as ARGS, and prompt
bodies are naturally multi-line."
  (mevedel-skills-input-parse-prefixed-line text ?$))

(defun mevedel-skills-input-escaped-position-p (text pos)
  "Return non-nil when POS in TEXT is escaped by an odd backslash run."
  (let ((i (1- pos))
        (count 0))
    (while (and (>= i 0)
                (eq (aref text i) ?\\))
      (cl-incf count)
      (cl-decf i))
    (not (zerop (% count 2)))))

(defun mevedel-skills-input--word-char-p (ch)
  "Return non-nil when CH is an ASCII word character."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun mevedel-skills-input--quote-delimiter-p (text pos)
  "Return non-nil when POS is a quote delimiter in TEXT."
  (and (not (mevedel-skills-input-escaped-position-p text pos))
       (let ((quote (aref text pos)))
         (and (memq quote '(?\" ?\'))
              (or (eq quote ?\")
                  (not (and (mevedel-skills-input--word-char-p
                             (and (> pos 0) (aref text (1- pos))))
                            (mevedel-skills-input--word-char-p
                             (and (< (1+ pos) (length text))
                                  (aref text (1+ pos)))))))))))

(defun mevedel-skills-input--quoted-inline-skill-p (text start end)
  "Return non-nil when TEXT START..END is inside single/double quotes."
  (let ((line-start (or (and (> start 0)
                             (string-match-p "\n"
                                             (substring text 0 start))
                             (1+ (cl-position ?\n text
                                               :end start
                                               :from-end t)))
                        0))
        (line-end (or (cl-position ?\n text :start end)
                      (length text)))
        quoted)
    (dolist (quote '(?\" ?\') quoted)
      (let ((open nil)
            (pos (1- start)))
        (while (and (not open) (>= pos line-start))
          (when (and (eq (aref text pos) quote)
                     (mevedel-skills-input--quote-delimiter-p text pos))
            (setq open pos))
          (cl-decf pos))
        (when open
          (setq pos end)
          (while (and (not quoted) (< pos line-end))
            (when (and (eq (aref text pos) quote)
                       (mevedel-skills-input--quote-delimiter-p text pos))
              (setq quoted t))
            (cl-incf pos)))))))

(defun mevedel-skills-input--skill-token-live-p
    (text start end first-nonspace code-ranges &optional allow-root)
  "Return non-nil when TEXT START..END is a live `$skill' token.
When ALLOW-ROOT is nil, exclude the leading root skill invocation."
  (and (or allow-root (not (= start first-nonspace)))
       (not (mevedel-skills-input-escaped-position-p text start))
       (not (mevedel-skills-input--quoted-inline-skill-p text start end))
       (not (cl-some (lambda (range)
                       (and (< start (cdr range))
                            (< (car range) end)))
                     code-ranges))))

(defun mevedel-skills-input-scan-tokens (text resolver &optional allow-root)
  "Return live `$skill' tokens in TEXT resolved through RESOLVER.
Each token has `:start', `:end', `:name', and `:value'.  When
ALLOW-ROOT is non-nil, include the leading root invocation."
  (let ((first-nonspace (or (string-match-p "\\S-" text) -1))
        (code-ranges (mevedel-skills-preparation-markdown-code-ranges text))
        tokens)
    (dolist (occurrence
             (mevedel-mention-bindings-skill-token-occurrences text))
      (let* ((start (plist-get occurrence :start))
             (resolved
              (cl-loop
               for candidate in (plist-get occurrence :candidates)
               for end = (+ start 1 (length candidate))
               for value = (and
                            (mevedel-skills-input--skill-token-live-p
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

(defun mevedel-skills-input-refresh-bound-input (text session)
  "Refresh SESSION discovery when TEXT carries atomic skill bindings.
Return TEXT unchanged.  Call this at a submission boundary so an exact
source resolves against its latest on-disk contents even when automatic
modification checking is disabled."
  (when (cl-some
         (lambda (range)
           (eq 'skill
               (plist-get (plist-get range :binding) :kind)))
         (mevedel-mention-bindings-ranges text))
    (mevedel-skills-install session (current-buffer)))
  text)

(defun mevedel-skills-input-prepare-user-input (text session)
  "Prepare user TEXT with valid skill tokens bound in SESSION.
Existing valid bindings remain authoritative.  Manually typed known
tokens receive a binding when their skill has a stable source file.
Existing bindings trigger one explicit discovery refresh so exact
sources are checked before the submission leaves the composer."
  (unless (mevedel-mention-bindings-valid-p text)
    (user-error "Malformed mention binding"))
  (let ((result (mevedel-mention-bindings-copy-text text)))
    (mevedel-skills-input-refresh-bound-input result session)
    (dolist (token
             (mevedel-skills-input-scan-tokens
              result
              (lambda (candidate start end)
                (let ((binding (mevedel-mention-bindings-starting-at
                                result start)))
                  (if binding
                      (mevedel-mention-bindings-at
                       result start end 'skill (concat "$" candidate))
                    (mevedel-session-get-skill session candidate))))
              t))
      (let ((start (plist-get token :start))
            (end (plist-get token :end))
            (skill (plist-get token :value)))
        (unless (get-text-property start 'mevedel-mention-binding result)
          (when-let* (((mevedel-skill-p skill))
                      (source-file (mevedel-skill-source-file skill)))
            (mevedel-mention-bindings-set
             start end
             (list :kind 'skill
                   :token (concat "$" (plist-get token :name))
                   :source-file source-file)
             result)))))
    result))

(defun mevedel-skills-input--bound-source-available-p (name source-file session)
  "Return non-nil when SOURCE-FILE still resolves as skill NAME.
The resource resolver checks the exact source identity and current enabled
state; the executor callback intentionally does not read the skill body."
  (condition-case nil
      (let* ((address
              (format "skill://%s@%s"
                      (mevedel-resource-encode-component name)
                      (mevedel-resource-skill-digest source-file)))
             (attempt (mevedel-resource-prepare
                       'read address (list :session session))))
        (mevedel-resource-execute
         attempt
         (lambda (path _address)
           (and (file-regular-p path)
                (file-readable-p path))))
    )
    (error nil)))

(defun mevedel-skills-input-resolve-mention
    (text session start end name)
  "Return the resolution outcome for user skill NAME at START..END in TEXT.
Success is `(:status ok :skill SKILL)'.  A valid binding whose target is
unavailable returns `(:status unavailable :message MESSAGE)'.
Unknown unbound names are successful with a nil skill."
  (let* ((token (concat "$" name))
         (property (and (< start (length text))
                        (get-text-property
                         start 'mevedel-mention-binding text)))
         (bound (mevedel-mention-bindings-starting-at text start))
         (binding (and bound
                       (mevedel-mention-bindings-at
                        text start end 'skill token)))
         (source-file (plist-get binding :source-file))
         (skill
          (cond
           (binding
            (and (mevedel-skills-input--bound-source-available-p
                  name source-file session)
                 (mevedel-session-get-skill-by-source
                  session source-file)))
           (bound nil)
           (t (mevedel-session-get-skill session name)))))
    (cond
     ((and property (null bound))
      (user-error "Malformed mention binding"))
     ((and binding (null skill))
      (list :status 'unavailable
            :message (format "bound skill $%s is unavailable" name)))
     ((and skill (not (mevedel-skills-skill-enabled-p skill)))
      (list :status 'unavailable
            :message (format "skill $%s is disabled" name)))
     ((and skill (not (mevedel-skill-user-invocable-p skill)))
      (list :status 'unavailable
            :message (format "skill $%s is not user-invocable" name)))
     (t (list :status 'ok :skill skill)))))

(defun mevedel-skills-input--inline-skill-mentions
    (text session &optional allow-root)
  "Return live inline `$skill' mentions in TEXT for SESSION.

Unknown `$foo' text is ignored.  Known but unavailable skills remain
occurrences marked with :unavailable and :message so dispatch can annotate
them without preparing a skill body.  When ALLOW-ROOT is non-nil, include a
leading mention after command dispatch has declined it."
  (unless (mevedel-mention-bindings-valid-p text)
    (user-error "Malformed mention binding"))
  (let* ((tokens
          (and session
               (mevedel-skills-input-scan-tokens
                text
                (lambda (candidate start end)
                  (let ((outcome
                         (mevedel-skills-input-resolve-mention
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
             (binding (mevedel-mention-bindings-starting-at text start))
             (source-file
              (or (plist-get binding :source-file)
                  (and (mevedel-skill-p skill)
                       (mevedel-skill-source-file skill)))))
        (cond
           ((and (listp skill) (plist-get skill :unavailable))
            (push (list :name name
                        :start start
                        :end end
                        :source-file source-file
                        :unavailable t
                        :message (plist-get skill :message))
                  mentions))
           ((not (gethash (or (mevedel-skill-source-file skill)
                              (mevedel-skill-name skill))
                          seen))
            (puthash (or (mevedel-skill-source-file skill)
                         (mevedel-skill-name skill))
                     t seen)
            (push (list :name name
                        :start start
                        :end end
                        :source-file source-file
                        :skill skill)
                  mentions)))))
    (nreverse mentions)))

(defun mevedel-skills-input-ensure-fresh-line ()
  "Leave point at the start of an empty line with a blank line above.
Called after deleting a slash-command region so the next insertion or
cursor rest position is visually separated from the preceding response.
Skipped when the slash command was preceded solely by the prompt prefix,
since in that case the prefix should remain on its own line and the body
should inline with it."
  (unless (bolp) (insert "\n"))
  (unless (save-excursion
            (forward-line -1)
              (looking-at-p "^[ \t]*$"))
    (insert "\n")))

(defun mevedel-skills-input-insert-fork-result (outcome)
  "Insert fork skill OUTCOME as an assistant response in the data buffer.

The current buffer must be the data buffer.  This path is used when a
fork skill suppresses the main `gptel-send'; it records the retained
agent's final result as the assistant side of that turn and
runs the normal post-response hooks so the view and persistence layers
observe the completed response."
  (let* ((render-data (plist-get outcome :render-data))
         (hook-audits (plist-get outcome :hook-audits))
         (result (or (plist-get outcome :result)
                     "Fork skill produced no result."))
         (result (if render-data
                     (concat result
                             (mevedel-tool-render-data-format
                              render-data))
                   result)))
    (unless (bound-and-true-p mevedel--current-request)
      (when (bound-and-true-p mevedel--session)
        (mevedel-session-artifacts-assert-new-mutation-authority
         mevedel--session)
        (mevedel-request-begin mevedel--session
                               (and (boundp 'mevedel--current-directive-uuid)
                                    mevedel--current-directive-uuid))))
    (goto-char (point-max))
    (when-let* ((synthetic (plist-get outcome :synthetic-user-message)))
      (let ((user-turn-start (point)))
        (unless (bolp) (insert "\n"))
        (insert synthetic)
        (unless (bolp) (insert "\n"))
        (mevedel--clear-user-turn-gptel-properties
         user-turn-start (point))))
    (when hook-audits
      (insert (mapconcat #'mevedel--format-hook-audit-record
                         hook-audits "")))
    (unless (bolp) (insert "\n"))
    (insert gptel-response-separator)
    (let ((start (point)))
      (insert result)
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (let ((end (point)))
        (add-text-properties start end '(gptel response))
        (mevedel-transcript-restore-ignored-properties start end)
        (condition-case err
            (run-hook-with-args 'gptel-post-response-functions start end)
          (error
           (display-warning
            'mevedel
            (format "Fork post-response hook failed: %s"
                    (error-message-string err))
            :warning)))
        (mevedel-transcript-restore-ignored-properties start end)
        (mevedel--complete-turn
         (gptel-make-fsm
          :info (list :buffer (current-buffer)
                      :mevedel-request-id
                      (mevedel-request-id mevedel--current-request))))
        (gptel--update-status " Ready" 'success)))))

(defun mevedel-skills-input-command-delete-context (command-pos)
  "Return deletion context for a command starting at COMMAND-POS.
The result is a plist with :delete-start and :after-prefix."
  (let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist))
         (has-prefix (and prefix (not (string-empty-p prefix))))
         (line-start (save-excursion
                       (goto-char command-pos)
                       (line-beginning-position)))
         (before-command
          (buffer-substring-no-properties line-start command-pos))
         (after-prefix (and has-prefix (equal before-command prefix)))
         (delete-start
          (cond
           (after-prefix command-pos)
           ((string-match-p "\\`[ \t]*\\'" before-command) line-start)
           (t command-pos))))
    (list :delete-start delete-start :after-prefix after-prefix)))

(defun mevedel-skills-input--handle-user-skill-outcome
    (skill outcome delete-start region-end after-prefix continue-fn)
  "Apply user skill OUTCOME in the current data buffer.

SKILL is the invoked user skill.  DELETE-START and REGION-END bound
the original `$skill' text.  AFTER-PREFIX means the command followed
the prompt prefix.

CONTINUE-FN, when non-nil, resumes the original `gptel-send' after an
inline body has been inserted.  Fork outcomes suppress that send and
insert their result when the retained agent finishes."
  (pcase (plist-get outcome :status)
    ('ok
     (pcase (plist-get outcome :kind)
       ('inline
        (delete-region delete-start region-end)
        (unless after-prefix
          (mevedel-skills-input-ensure-fresh-line))
        (let ((body (or (plist-get outcome :body)
                        (format "Skill '%s' produced no body."
                                (mevedel-skill-name skill)))))
          (insert body)
          (mevedel-skills-input--insert-inline-user-skill-render-data
           skill (plist-get outcome :arguments) body))
        (when continue-fn
          (funcall continue-fn))
        'skill)
       ('fork
        (message "Skill '%s' dispatched; waiting for agent result..."
                 (mevedel-skill-name skill))
        (mevedel-skills-input-insert-fork-result outcome)
        'skill)
       (_
        (message "Skill '%s' returned an unsupported outcome: %S"
                 (mevedel-skill-name skill) outcome)
        'unknown)))
    (_
     (message "Skill '%s' failed: %s"
              (mevedel-skill-name skill)
              (plist-get outcome :message))
     'unknown)))

(defun mevedel-skills-input-dispatch-command (&optional continue-fn)
  "Parse and dispatch a `$skill' invocation in the current chat buffer.

Returns:
- `skill'   a skill was expanded into the prompt region; caller
            should proceed with the send if CONTINUE-FN was nil.
- `unknown' preparation failed and the caller should abort the send.
- nil       no invocable leading `$skill' is present; caller should scan the
            complete prompt for inline attachments."
  (when-let* ((region (mevedel-skills-input-current-prompt-region))
              (text (buffer-substring (car region) (cdr region)))
              (parsed (mevedel-skills-input-parse-skill-line text)))
    (let* ((name (nth 0 parsed))
           (args (nth 1 parsed))
           (skill-pos (+ (car region) (nth 2 parsed)))
           (outcome
            (mevedel-skills-input-resolve-mention
             text mevedel--session
             (nth 2 parsed)
             (+ (nth 2 parsed) 1 (length name))
             name))
           (skill (plist-get outcome :skill)))
      (if (not (eq (plist-get outcome :status) 'ok))
          nil
        (when skill
          (let* ((delete-context
                  (mevedel-skills-input-command-delete-context skill-pos))
                 (after-prefix (plist-get delete-context :after-prefix))
                 (buffer (current-buffer))
                 ;; Preparation is asynchronous whenever the body injects, so
                 ;; the draft can move, shrink, or be replaced before the
                 ;; outcome arrives.  Markers follow an edit where integers
                 ;; would address the wrong text or past the end of the
                 ;; buffer, and the snapshot says whether this is still the
                 ;; command that was dispatched.
                 (delete-start (copy-marker
                                (plist-get delete-context :delete-start) nil))
                 (region-end (copy-marker (cdr region) t))
                 (snapshot (buffer-substring-no-properties
                            delete-start region-end)))
            (mevedel-skills-invoke
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
                           (mevedel-skills-input--handle-user-skill-outcome
                            skill outcome
                            (marker-position delete-start)
                            (marker-position region-end)
                            after-prefix continue-fn)
                         ;; Activation happened before preparation, so the
                         ;; skill's permission rules, model, and effort are
                         ;; staged in this buffer.  Abandoning without
                         ;; releasing them would apply them to whatever the
                         ;; user sends next.
                         (mevedel-skills-input-clear-pending)
                         (message
                          (if (eq (plist-get outcome :kind) 'fork)
                              "mevedel: skill draft changed during preparation; the fork agent's result was discarded"
                            "mevedel: skill draft changed during preparation, not sent")))))
                 (set-marker delete-start nil)
                 (set-marker region-end nil)))
             :origin 'user)
            ;; The outcome decides what happened, and it may arrive after this
            ;; returns; the caller only needs to know a skill took the send.
            'skill))))))

(defun mevedel-skills-input--prepare-inline-attachments
    (mentions callback &optional prepared)
  "Prepare inline skill MENTIONS, then call CALLBACK.
CALLBACK receives (:status ok :attachments LIST) or an error plist.
PREPARED is the accumulator used for sequential async preparation."
  (if (null mentions)
      (funcall callback
               (list :status 'ok :attachments (nreverse prepared)))
    (let* ((mention (car mentions))
           (skill (plist-get mention :skill))
           (name (plist-get mention :name)))
      (if (plist-get mention :unavailable)
          (mevedel-skills-input--prepare-inline-attachments
           (cdr mentions) callback (cons mention prepared))
        (mevedel-skills-prepare
         skill ""
         (lambda (outcome)
           (pcase (plist-get outcome :status)
             ('ok
              (if (eq (plist-get outcome :kind) 'instruction)
                  (mevedel-skills-input--prepare-inline-attachments
                   (cdr mentions) callback
                   (cons (list :name name
                               :start (plist-get mention :start)
                               :end (plist-get mention :end)
                               :source-file
                               (plist-get mention :source-file)
                               :skill skill
                               :arguments ""
                               :body (plist-get outcome :body)
                               :invoked-skills
                               (plist-get
                                (plist-get outcome :request-context)
                                :invoked-skills))
                         prepared))
                (funcall callback
                         (list :status 'error
                               :reason 'unsupported-context
                               :message
                               (format "Skill $%s cannot be attached inline."
                                       name)))))
             (_
              (funcall callback outcome))))
         :role 'instruction :origin 'user :policy-owner-p nil)))))

(defun mevedel-skills-input--prepare-inline-attachments-for-text
    (text session callback &optional allow-root)
  "Prepare inline `$skill' attachments in TEXT for SESSION.
CALLBACK receives the prepared outcome.  Return `skill' when preparation
took ownership or nil when TEXT has no inline attachments.  ALLOW-ROOT is
forwarded to inline mention scanning."
  (when-let* ((mentions (mevedel-skills-input--inline-skill-mentions
                         text session allow-root)))
    (mevedel-skills-input--prepare-inline-attachments mentions callback)
    'skill))

(defun mevedel-skills-input--stage-inline-attachments (attachments)
  "Store prepared inline ATTACHMENTS and return their render-data block."
  (setq-local mevedel-skills-input--pending-inline-attachments attachments)
  (when-let* ((warnings
               (delq nil (mapcar (lambda (attachment)
                                   (plist-get attachment :message))
                                 attachments))))
    (message "mevedel: %s" (mapconcat #'identity warnings "; ")))
  (let ((records
         (mapcan (lambda (attachment)
                   (copy-sequence (plist-get attachment :invoked-skills)))
                 attachments)))
    (dolist (record records)
      (mevedel-skills-activate-context 'user :invoked-skill record)))
  (mevedel-skills-input--format-inline-attachment-render-data attachments))

(defun mevedel-skills-input-clear-pending ()
  "Clear pending inline attachment request state in the current buffer."
  (mevedel-skills-clear-pending-context)
  (setq-local mevedel-skills-input--pending-inline-attachments nil))

(defun mevedel-skills-input-dispatch-inline-attachments
    (&optional continue-fn allow-root)
  "Prepare inline `$skill' attachments in the current prompt.
Return `skill' when preparation took ownership of continuing the send,
or nil when no inline attachments exist.  Unavailable skills are annotated
without blocking the send."
  (when-let* ((region (mevedel-skills-input-current-prompt-region))
              (session (and (bound-and-true-p mevedel--session)
                            mevedel--session))
              ((not (bound-and-true-p
                     mevedel-skills-input--pending-inline-attachments)))
              (text (buffer-substring (car region) (cdr region))))
    (let ((buffer (current-buffer)))
      (mevedel-skills-input--prepare-inline-attachments-for-text
       text session
       (lambda (outcome)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (pcase (plist-get outcome :status)
               ('ok
                (goto-char (point-max))
                (insert
                 (mevedel-skills-input--stage-inline-attachments
                  (plist-get outcome :attachments)))
                (when continue-fn
                  (funcall continue-fn)))
               (_
                (mevedel-skills-input-clear-pending)
                (message "Inline skill failed: %s"
                         (or (plist-get outcome :message)
                             "unknown error")))))))
       allow-root))))


(provide 'mevedel-skills-input)
;;; mevedel-skills-input.el ends here
