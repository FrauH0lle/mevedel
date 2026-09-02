;;; mevedel-skills-preparation.el -- Skill body preparation -*- lexical-binding: t -*-

;;; Commentary:

;; Owns provenance-preserving skill argument substitution and body
;; injection parsing/execution.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))
(require 'mevedel-skills-syntax)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))
(autoload 'mevedel-pipeline-run-tool "mevedel-pipeline")

;; `mevedel-skills-core'
(declare-function mevedel-skill-argument-names
                  "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-effort "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-source "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-source-dir "mevedel-skills-core" (cl-x) t)
(autoload 'mevedel-skill-argument-names "mevedel-skills-core")
(autoload 'mevedel-skill-effort "mevedel-skills-core")
(autoload 'mevedel-skill-name "mevedel-skills-core")
(autoload 'mevedel-skill-source "mevedel-skills-core")
(autoload 'mevedel-skill-source-dir "mevedel-skills-core")

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(autoload 'mevedel-session-execution-target "mevedel-structs")
(autoload 'mevedel-session-name "mevedel-structs")
(autoload 'mevedel-session-session-id "mevedel-structs")

;; `mevedel-tool-exec'
(declare-function mevedel-tool-exec--register "mevedel-tool-exec" ())
(autoload 'mevedel-tool-exec--register "mevedel-tool-exec")

;; `mevedel-tool-registry'
(declare-function copy-mevedel-tool "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-args "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get
                  "mevedel-tool-registry" (name &optional category))
(autoload 'copy-mevedel-tool "mevedel-tool-registry")
(autoload 'mevedel-tool-args "mevedel-tool-registry")
(autoload 'mevedel-tool-get "mevedel-tool-registry")

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-extract
                  "mevedel-tool-render-data"
                  (result-string &optional session expected-tool-use-id
                                 allow-payload-tool-use-id))
(autoload 'mevedel-tool-render-data-extract "mevedel-tool-render-data")

;;
;;; Argument tokenization

(defun mevedel-skills-preparation-parse-arguments (arguments)
  "Parse ARGUMENTS into a list of tokens, shell-style.
Returns nil when ARGUMENTS is nil or blank.  Falls back to
whitespace splitting when shell parsing fails (unbalanced quotes
etc.).  Empty tokens that can fall out of leading/trailing
whitespace are filtered.  Ports the parsing half of ccs's
argumentSubstitution.ts."
  (cond
   ((null arguments) nil)
   ((not (stringp arguments)) nil)
   ((string-blank-p arguments) nil)
   (t
    (cl-remove-if #'string-empty-p
                  (condition-case nil
                      (split-string-and-unquote arguments)
                    (error
                     (split-string arguments "[ \t\n]+" t)))))))


;;
;;; Variable substitution

(defconst mevedel-skills-preparation--non-author-text-property
  'mevedel-skills-non-author-text
  "Text property set on content not written literally in SKILL.md.")

(defconst mevedel-skills-preparation--literal-placeholder-property
  'mevedel-skills-literal-placeholder
  "Text property set on escaped placeholders that must stay literal.")

(defvar mevedel-skills-preparation--substitution-made-p nil
  "Non-nil when skill variable substitution replaced text.")

(defun mevedel-skills-preparation--word-char-p (ch)
  "Return non-nil when CH is a word character (`[A-Za-z0-9_]')."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun mevedel-skills-preparation--mark-non-author-text (value)
  "Return VALUE marked as text not written literally in SKILL.md."
  (let ((copy (copy-sequence (or value ""))))
    (add-text-properties
     0 (length copy)
     (list mevedel-skills-preparation--non-author-text-property t)
     copy)
    copy))

(defun mevedel-skills-preparation--property-range-p (text start end property)
  "Return non-nil when TEXT has PROPERTY anywhere from START to END."
  (let ((pos start)
        found)
    (while (and (< pos end) (not found))
      (if (get-text-property pos property text)
          (setq found t)
        (setq pos (or (next-single-property-change pos property text end)
                      end))))
    found))

(defun mevedel-skills-preparation--non-author-range-p (text start end)
  "Return non-nil when TEXT has any non-author content from START to END."
  (mevedel-skills-preparation--property-range-p
   text start end mevedel-skills-preparation--non-author-text-property))

(defun mevedel-skills-preparation--literal-placeholder-range-p (text start end)
  "Return non-nil when TEXT has literal placeholder content from START to END."
  (mevedel-skills-preparation--property-range-p
   text start end mevedel-skills-preparation--literal-placeholder-property))

(defun mevedel-skills-preparation--protected-substitution-range-p (text start end)
  "Return non-nil when TEXT from START to END must not be substituted."
  (or (mevedel-skills-preparation--non-author-range-p text start end)
      (mevedel-skills-preparation--literal-placeholder-range-p text start end)))

(defun mevedel-skills-preparation--replace-match-with-non-author (value)
  "Replace the current match with VALUE marked as non-author text."
  (let ((start (match-beginning 0))
        (end (match-end 0)))
    (setq mevedel-skills-preparation--substitution-made-p t)
    (delete-region start end)
    (goto-char start)
    (insert (mevedel-skills-preparation--mark-non-author-text value))))

(defconst mevedel-skills-preparation--literal-variable-placeholders
  '("${CLAUDE_SESSION_ID}"
    "${CLAUDE_SKILL_DIR}"
    "${CLAUDE_EFFORT}"
    "${MEVEDEL_SESSION_ID}"
    "${MEVEDEL_SKILL_DIR}"
    "${MEVEDEL_EFFORT}")
  "Literal skill variable placeholders supported by substitution.")

(defun mevedel-skills-preparation--placeholder-end-at-point (argument-names)
  "Return placeholder end at point, or nil when point is not at one.
ARGUMENT-NAMES is the list of named skill arguments."
  (or (cl-loop for placeholder in mevedel-skills-preparation--literal-variable-placeholders
               when (looking-at (regexp-quote placeholder))
               return (match-end 0))
      (when (looking-at "\\$ARGUMENTS\\(\\[[0-9]+\\]\\)?")
        (match-end 0))
      (when (looking-at "\\$\\([0-9]+\\)")
        (let ((end (match-end 0)))
          (unless (mevedel-skills-preparation--word-char-p (char-after end))
            end)))
      (cl-loop for name in argument-names
               for target = (concat "$" name)
               for end = (+ (point) (length target))
               when (and (looking-at (regexp-quote target))
                         (not (eq (char-after end) ?\[))
                         (not (mevedel-skills-preparation--word-char-p
                               (char-after end))))
               return end)))

(defun mevedel-skills-preparation--protect-escaped-placeholders (text argument-names)
  "Return TEXT with escaped placeholders made literal.
ARGUMENT-NAMES is the list of named skill arguments.
A backslash before a recognized placeholder, such as `\\$ARGUMENTS',
is removed and the placeholder is protected from substitution."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "\\$" nil t)
      (let ((slash (match-beginning 0))
            (dollar (1- (point))))
        (goto-char dollar)
        (let ((end (mevedel-skills-preparation--placeholder-end-at-point argument-names)))
          (if end
              (progn
                (delete-region slash (1+ slash))
                (add-text-properties
                 slash (1- end)
                 (list mevedel-skills-preparation--literal-placeholder-property t))
                (goto-char (1- end)))
            (goto-char (1+ dollar))))))
    (buffer-string)))

(defun mevedel-skills-preparation--substitute-named (text name value)
  "Replace `$NAME' with VALUE in TEXT, strict word-boundary matching.

Skips `$NAME[...]' (indexed access form) and `$NAMEident' (longer
identifier).  Emulates ccs's `\\=$NAME(?![\\=[\\=w])' regex.
Case-sensitive."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((target (concat "$" name)))
        (while (search-forward target nil t)
          (let ((next (char-after)))
            (cond
             ;; Followed by [ -> indexed-access form, skip
             ((eq next ?\[) nil)
             ;; Followed by word char -> longer identifier, skip
             ((mevedel-skills-preparation--word-char-p next) nil)
             ((mevedel-skills-preparation--protected-substitution-range-p
               (buffer-string) (match-beginning 0) (match-end 0))
              nil)
             (t
              (mevedel-skills-preparation--replace-match-with-non-author value))))))
      (buffer-string))))

(defun mevedel-skills-preparation--substitute-shorthand (text parsed-args)
  "Replace `$N' shorthand with PARSED-ARGS[N] (zero-based) in TEXT.

Strict word-boundary: `$1' followed by a word char (e.g. `$1foo') is
not substituted.  `$ARGUMENTS' starts with `A' (a word char following
the `$') so this regex naturally skips it.  Indices out of range are
substituted with the empty string.  Case-sensitive."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "\\$\\([0-9]+\\)" nil t)
        (let ((next (char-after)))
          (unless (or (mevedel-skills-preparation--word-char-p next)
                      (mevedel-skills-preparation--protected-substitution-range-p
                       (buffer-string) (match-beginning 0) (match-end 0)))
            (let* ((idx (string-to-number (match-string 1)))
                   (val (or (nth idx parsed-args) "")))
              (mevedel-skills-preparation--replace-match-with-non-author val)))))
      (buffer-string))))

(defun mevedel-skills-preparation--substitute-regexp
    (text regexp replacement-fn &optional author-only-p)
  "Replace REGEXP matches in TEXT with non-author replacement text.
REPLACEMENT-FN is called before the match is deleted, so it may use
`match-string' to inspect subgroups in the current buffer.
When AUTHOR-ONLY-P is non-nil, skip matches that overlap non-author text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (unless (or (mevedel-skills-preparation--literal-placeholder-range-p
                   (buffer-string) (match-beginning 0) (match-end 0))
                  (and author-only-p
                       (mevedel-skills-preparation--non-author-range-p
                        (buffer-string) (match-beginning 0) (match-end 0))))
        (mevedel-skills-preparation--replace-match-with-non-author
         (funcall replacement-fn))))
    (buffer-string)))

(defun mevedel-skills-preparation--target-native-source-dir (skill session)
  "Return SKILL's source directory in SESSION's model-facing path domain."
  (let* ((dir (and skill (mevedel-skill-source-dir skill)))
         (source (and skill (mevedel-skill-source skill)))
         (target (and session (mevedel-session-execution-target session))))
    (if (and dir target
             (mevedel-execution-target-remote-p target)
             (or (eq source 'project)
                 (and (eq source 'plugin) (file-remote-p dir))))
        (progn
          (unless (file-remote-p dir)
            (signal 'mevedel-execution-target-error
                    (list (format "Project skill is not on the session execution target: %s"
                                  dir))))
          (mevedel-execution-target-native-path target dir))
      dir)))

(defun mevedel-skills-preparation-substitute (text arguments session skill)
  "Return TEXT with skill placeholders expanded.

Algorithm follows ccs's `argumentSubstitution.ts' for unescaped
placeholders, with the mevedel extension that a backslash before a
recognized placeholder keeps it literal.  Substitution order
\=(zero-based throughout):

1. Named arguments from SKILL's `argument-names' slot, mapping
   ARGUMENT-NAMES[i] -> PARSED-ARGS[i].
2. `$ARGUMENTS[N]'.
3. `$N' shorthand.
4. `$ARGUMENTS' (the raw argument string).
5. `${CLAUDE_SESSION_ID}', `${CLAUDE_SKILL_DIR}', `${CLAUDE_EFFORT}',
   and their `${MEVEDEL_*}' aliases.  These are substituted after the
   placeholder-substituted check below.

If ARGUMENTS is non-empty AND none of steps 1-4 substituted
anything, append `\\nARGUMENTS: <raw>' so the body still receives
the user's input.

Named-argument matching uses strict word-boundary semantics so
`$foo' does not match `$foo[0]' or `$foobar'.  Numeric-only
argument names are filtered out at scan time
\\=(see `mevedel-skills--parse-argument-names') so they cannot
shadow `$0'/`$1' shorthand."
  (let* ((session-id (or (and session (mevedel-session-session-id session))
                         (and session (mevedel-session-name session))
                         ""))
         (skill-dir (or (mevedel-skills-preparation--target-native-source-dir
                         skill session)
                        ""))
         (effort (or (and skill
                          (mevedel-skill-effort skill)
                          (format "%s" (mevedel-skill-effort skill)))
                     ""))
         (argument-names (and skill (mevedel-skill-argument-names skill)))
         (raw-args arguments)
         (parsed-args (mevedel-skills-preparation-parse-arguments raw-args))
         (full (or raw-args ""))
         (mevedel-skills-preparation--substitution-made-p nil)
         (result (mevedel-skills-preparation--protect-escaped-placeholders
                  text argument-names)))
    ;; 1. Named arguments.
    (cl-loop for name in argument-names
             for i from 0
             for value = (or (nth i parsed-args) "")
             do (setq result
                      (mevedel-skills-preparation--substitute-named result name value)))
    ;; 2. $ARGUMENTS[N].
    (setq result
          (mevedel-skills-preparation--substitute-regexp
           result
           "\\$ARGUMENTS\\[\\([0-9]+\\)\\]"
           (lambda ()
             (or (nth (string-to-number (match-string 1))
                      parsed-args)
                 ""))))
    ;; 3. $N shorthand.
    (setq result (mevedel-skills-preparation--substitute-shorthand result parsed-args))
    ;; 4. $ARGUMENTS (full).
    (setq result
          (mevedel-skills-preparation--substitute-regexp
           result "\\$ARGUMENTS" (lambda () full)))
    ;; Decide append-fallback BEFORE the mevedel-specific ${...} subs
    ;; so they don't influence the "no placeholder substituted" check.
    (let ((args-substituted mevedel-skills-preparation--substitution-made-p))
      ;; 5. Claude-compatible and mevedel-native literal variables.
      (dolist (var `(("${CLAUDE_SESSION_ID}" . ,session-id)
                     ("${CLAUDE_SKILL_DIR}" . ,skill-dir)
                     ("${CLAUDE_EFFORT}" . ,effort)
                     ("${MEVEDEL_SESSION_ID}" . ,session-id)
                     ("${MEVEDEL_SKILL_DIR}" . ,skill-dir)
                     ("${MEVEDEL_EFFORT}" . ,effort)))
        (setq result
              (mevedel-skills-preparation--substitute-regexp
               result (regexp-quote (car var))
               (lambda () (cdr var)) t)))
      ;; 6. Append-fallback: only when args were supplied AND non-empty
      ;; AND nothing was substituted.
      (when (and (not args-substituted)
                 (stringp raw-args)
                 (not (string-empty-p raw-args)))
        (setq result
              (concat result "\n\nARGUMENTS: "
                      (mevedel-skills-preparation--mark-non-author-text raw-args)))))
    result))


;;
;;; Body injections

(define-error 'mevedel-skills-shell-abort
  "Skill body shell expansion failed; skill must abort.")

(defun mevedel-skills-preparation--classify-injection
    (result marker kind reason callback)
  "Report RESULT for MARKER to CALLBACK as an injection outcome.
KIND names the injection in messages and REASON is the failure reason to
report.  The pipeline's canonical status decides the outcome; the display
text only refines it, because a denial is worth naming separately.  Taking
the visible half also keeps the serialized side-channel block out of the
prompt, which only a tool result has stripped for it."
  (pcase-let* ((`(,visible . ,render-data)
                (mevedel-tool-render-data-extract result))
               (status (plist-get render-data :status)))
    (cond
     ((and (stringp visible)
           (string-prefix-p "Error: Permission denied" visible))
      (funcall callback
               `(:status error :reason permission-denied
                         :message ,(format "%s expansion %s denied: %s"
                                           kind marker visible))))
     ((or (eq status 'error)
          (mevedel-skills-preparation--injection-outcome-error-p visible))
      (funcall callback
               `(:status error :reason ,reason
                         :message ,(format "%s expansion %s failed: %s"
                                           kind marker visible))))
     (t
      (funcall callback
               `(:status ok :output ,(string-trim-right (or visible ""))))))))

(defun mevedel-skills-preparation--injection-outcome-error-p (result)
  "Return non-nil when pipeline RESULT means body injection failed."
  (and (stringp result)
       (or (string-prefix-p "Error:" result)
           (string-prefix-p "Command failed with exit code" result)
           (string-prefix-p "Failed to start process:" result))))

(defun mevedel-skills-preparation--shell-resource-error (skill session)
  "Return why SKILL shell injection cannot run in remote SESSION, or nil."
  (when-let* ((target (and session
                           (mevedel-session-execution-target session))))
    (when (mevedel-execution-target-remote-p target)
      (let ((source (and skill (mevedel-skill-source skill)))
            (dir (and skill (mevedel-skill-source-dir skill))))
        (cond
         ((not (memq source '(project plugin)))
          (format "Shell body injections from local %s skill %s cannot run in a remote session; the session stays on one execution target"
                  (or source 'unknown)
                  (or (and skill (mevedel-skill-name skill)) "resource")))
         ((not (file-remote-p dir))
          (format "Shell body injections from local %s skill %s cannot run in a remote session; the session stays on one execution target"
                  source (mevedel-skill-name skill)))
         (t
          (condition-case err
              (progn
                (mevedel-execution-target-native-path target dir)
                nil)
            (mevedel-execution-target-error
             (error-message-string err)))))))))

(defun mevedel-skills-preparation--run-shell-command-async (command marker callback)
  "Run COMMAND through the Bash tool pipeline, then call CALLBACK.

CALLBACK receives either \\=(:status ok :output STRING) or
\\=(:status error :reason SYMBOL :message STRING).  MARKER is the
original shell-injection marker used in diagnostics."
  (let ((tool (or (ignore-errors (mevedel-tool-get "Bash"))
                  (progn
                    (mevedel-tool-exec--register)
                    (ignore-errors (mevedel-tool-get "Bash"))))))
    (when tool
      (setq tool (copy-mevedel-tool tool))
      (setf (mevedel-tool-args tool)
            (append (mevedel-tool-args tool)
                    '((trust-literal-p boolean :optional
                                       "Internal trusted skill input.")
                      (suppress-sandbox-disclosure-p boolean :optional
                                                     "Keep execution metadata out of substituted output.")
                      (wait-for-completion-p boolean :optional
                                             "Wait for terminal settlement instead of yielding.")))))
    (cond
     ((null tool)
      (funcall callback
               `(:status error :reason shell-failure
                         :message "Bash tool is not registered.")))
     (t
      (condition-case err
          (progn
            (unless (fboundp 'mevedel-tools--current-deferred-context)
              (require 'mevedel-tools))
            (mevedel-pipeline-run-tool
             tool
             (lambda (result)
               (mevedel-skills-preparation--classify-injection
                result marker "Shell" 'shell-failure callback))
             (list :command command
                   :suppress-sandbox-disclosure-p t
                   :wait-for-completion-p t
                   :trust-literal-p
                   (mevedel-skills-syntax--author-ranges-p
                    command 0 (length command)))))
        (error
         (funcall callback
                  `(:status error :reason shell-failure
                            :message ,(format "Shell expansion %s errored: %s"
                                              marker
                                              (error-message-string err))))))))))

(defun mevedel-skills-preparation--run-elisp-expression-async (expression marker callback)
  "Run EXPRESSION through the Eval tool pipeline, then call CALLBACK.

CALLBACK receives either \\=(:status ok :output STRING) or
\\=(:status error :reason SYMBOL :message STRING).  MARKER is the
original elisp-injection marker used in diagnostics."
  (let ((tool (or (ignore-errors (mevedel-tool-get "Eval"))
                  (progn
                    (mevedel-tool-exec--register)
                    (ignore-errors (mevedel-tool-get "Eval"))))))
    (when tool
      (setq tool (copy-mevedel-tool tool))
      (setf (mevedel-tool-args tool)
            (append
             (mevedel-tool-args tool)
             '((trust-literal-p boolean :optional
                                "Internal trusted skill input.")
               (result-format string :optional
                              "Internal skill result format.")))))
    (cond
     ((null tool)
      (funcall callback
               `(:status error :reason elisp-failure
                         :message "Eval tool is not registered.")))
     (t
      (condition-case err
          (progn
            (unless (fboundp 'mevedel-tools--current-deferred-context)
              (require 'mevedel-tools))
            (mevedel-pipeline-run-tool
             tool
             (lambda (result)
               (mevedel-skills-preparation--classify-injection
                result marker "Elisp" 'elisp-failure callback))
             (list :expression expression
                   :trust-literal-p
                   (mevedel-skills-syntax--author-ranges-p
                    expression 0 (length expression))
                   :result-format "injection")))
        (error
         (funcall callback
                  `(:status error :reason elisp-failure
                            :message ,(format "Elisp expansion %s errored: %s"
                                              marker
                                              (error-message-string err))))))))))

(defun mevedel-skills-preparation--injection-match (text)
  "Return the next body-injection match in TEXT.

The return value is a plist with :start, :end, :command, and
:marker, or nil when TEXT contains no injection marker."
  (let ((matches nil)
        (markdown-code-ranges (mevedel-skills-syntax-markdown-code-ranges text)))
    (cl-labels
        ((scan-inline
          (opener kind payload-key)
          (let ((pos 0)
                (opener-re (regexp-quote opener))
                (len (length text)))
            (while (string-match opener-re text pos)
              (let* ((start (match-beginning 0))
                     (body-start (match-end 0))
                     (search body-start)
                     (done nil))
                (when (and (mevedel-skills-syntax--author-ranges-p
                            text start body-start)
                           (not (mevedel-skills-syntax--ranges-overlap-p
                                 markdown-code-ranges start body-start)))
                  (while (and (not done)
                              (string-match "`" text search))
                    (let ((close-start (match-beginning 0))
                          (close-end (match-end 0)))
                      (cond
                       ((string-match-p
                         "\n" (substring text body-start close-start))
                        (setq done t))
                       ((and (mevedel-skills-syntax--author-ranges-p
                              text close-start close-end)
                             (not (mevedel-skills-syntax--ranges-overlap-p
                                   markdown-code-ranges close-start close-end)))
                        (let ((payload (substring text body-start
                                                  close-start)))
                          (push (list :kind kind
                                      :start start
                                      :end close-end
                                      payload-key payload
                                      :marker (format "%s%s`" opener payload))
                                matches))
                        (setq done t))
                       (t
                        (setq search close-end))))))
                (setq pos (min len (max (1+ start) body-start)))))))
         (scan-fenced
          (opener-regexp kind payload-key marker)
          (let ((pos 0)
                (opener-re (concat "\\(^\\|\n\\)" opener-regexp)))
            (while (string-match opener-re text pos)
              (let* ((start (match-beginning 0))
                     (prefix-start (match-beginning 1))
                     (prefix-end (match-end 1))
                     (prefix (match-string 1 text))
                     (marker-start (+ start (length prefix)))
                     (body-start (match-end 0))
                     (search body-start)
                     (done nil))
                (when (and (mevedel-skills-syntax--author-ranges-p
                            text prefix-start prefix-end marker-start body-start)
                           (not (mevedel-skills-syntax--ranges-overlap-p
                                 markdown-code-ranges marker-start body-start)))
                  (while (and (not done)
                              (string-match "\n```\\(\n\\|\\'\\)"
                                            text search))
                    (let ((close-start (match-beginning 0))
                          (close-end (match-beginning 1))
                          (suffix-start (match-beginning 1))
                          (suffix-end (match-end 1)))
                      (if (and (mevedel-skills-syntax--author-ranges-p
                                text close-start close-end suffix-start suffix-end)
                               (not (mevedel-skills-syntax--ranges-overlap-p
                                     markdown-code-ranges close-start close-end)))
                          (let ((payload (substring text body-start
                                                    close-start)))
                            (push (list :kind kind
                                        :start start
                                        :end (match-end 0)
                                        payload-key payload
                                        :marker marker
                                        :prefix prefix
                                        :suffix (match-string 1 text))
                                  matches)
                            (setq done t))
                        (setq search (match-end 0))))))
                (setq pos (max (1+ start) body-start)))))))
      (scan-fenced (regexp-quote "```!\n")
                   'shell :command "(fenced shell block)")
      (scan-fenced "```!el[ \t]*\n"
                   'elisp :expression "(fenced elisp block)")
      (scan-inline "!`" 'shell :command)
      (scan-inline "!el`" 'elisp :expression)
      (car (sort matches
                 (lambda (a b)
                   (< (plist-get a :start)
                      (plist-get b :start))))))))

(defun mevedel-skills-preparation-expand-body
    (text callback &optional skill session)
  "Replace skill body injection markers in TEXT, then call CALLBACK.

CALLBACK receives either \\=(:status ok :body STRING) or
\\=(:status error :reason SYMBOL :message STRING).

Supported markers:
- !`COMMAND`          inline: run COMMAND, substitute stdout
- ```!\\nSCRIPT\\n``` fenced block: run SCRIPT as a shell script
- !el`EXPRESSION`     inline: evaluate EXPRESSION, substitute result
- ```!el\\nEXPR\\n``` fenced block: evaluate EXPR, substitute result

Each command/expression goes through its normal tool pipeline with
`:trust-literal-p t', so permission checking, execution, and
oversized-result persistence stay aligned with normal tool
execution.  SKILL and SESSION identify the invoking skill and target."
  (if-let* ((match (mevedel-skills-preparation--injection-match text)))
      (let ((start (plist-get match :start))
            (end (plist-get match :end))
            (kind (plist-get match :kind))
            (marker (plist-get match :marker))
            (prefix (or (plist-get match :prefix) ""))
            (suffix (or (plist-get match :suffix) ""))
            (origin-buffer (current-buffer)))
        (if-let* ((message (and (eq kind 'shell)
                                (mevedel-skills-preparation--shell-resource-error
                                 skill session))))
            (funcall callback
                     `(:status error :reason resource-target
                               :message ,message))
          (funcall
           (pcase kind
             ('shell #'mevedel-skills-preparation--run-shell-command-async)
             ('elisp #'mevedel-skills-preparation--run-elisp-expression-async))
           (or (plist-get match :command)
               (plist-get match :expression))
           marker
           (lambda (outcome)
             (if (not (buffer-live-p origin-buffer))
                 (funcall callback
                          `(:status error :reason aborted
                                    :message "Skill buffer was killed during body injection expansion."))
               (with-current-buffer origin-buffer
                 (pcase (plist-get outcome :status)
                   ('ok
                    (mevedel-skills-preparation-expand-body
                     (concat (substring text 0 start)
                             prefix
                             (mevedel-skills-preparation--mark-non-author-text
                              (plist-get outcome :output))
                             suffix
                             (substring text end))
                     callback skill session))
                   (_
                    (funcall callback outcome)))))))))
    (funcall callback `(:status ok :body ,(substring-no-properties text)))))


(provide 'mevedel-skills-preparation)
;;; mevedel-skills-preparation.el ends here
