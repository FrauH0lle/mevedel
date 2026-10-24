;;; mevedel-view-audit.el --- Transcript audit disclosures -*- lexical-binding: t -*-

;;; Commentary:

;; Renders parsed transcript audit attachments as collapsible view blocks.
;; Owns the read-only source identity and archive lookup shared by native
;; actions and independent browser inspectors.

;;; Code:

;; `cl-extra'
(declare-function cl-some "cl-extra" (predicate sequence &rest more-sequences))

;; `mevedel-hooks'
(declare-function mevedel-hooks-decision-reason "mevedel-hooks" (decision))

;; `mevedel-execution-transcript'
(declare-function mevedel-execution-transcript-pending-render-data
                  "mevedel-execution-transcript" (data-buffer tool-use-id))

;; `mevedel-reminders'
(declare-function mevedel-reminders--entry-label
                  "mevedel-reminders" (type))
(autoload 'mevedel-reminders--entry-label "mevedel-reminders")

;; `mevedel-resource'
(declare-function mevedel-resource-prepare "mevedel-resource"
                  (operation address context))
(autoload 'mevedel-resource-prepare "mevedel-resource")

;; `mevedel-structs'
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x) t)

;; `mevedel-transcript'
(declare-function mevedel-transcript--mailbox-any-block-at-point
                  "mevedel-transcript" (limit))
(declare-function mevedel-transcript-segments "mevedel-transcript"
                  (start end))
(autoload 'mevedel-transcript--mailbox-any-block-at-point "mevedel-transcript")

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-format-path
                  "mevedel-tool-repair" (path))
(autoload 'mevedel-tool-repair-format-path "mevedel-tool-repair")

;; `mevedel-tool-repair-diagnostics'
(declare-function mevedel-tool-repair-normalize-audit-record
                  "mevedel-tool-repair-diagnostics" (record))
(autoload 'mevedel-tool-repair-normalize-audit-record
  "mevedel-tool-repair-diagnostics")

;; `mevedel-transcript-audit'
(declare-function mevedel--hook-prompt-rewrite-audit-record
                  "mevedel-transcript-audit"
                  (event original submitted &optional reason))
(declare-function mevedel-transcript-audit-spans
                  "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-audit-records
                  "mevedel-transcript-audit" (text &optional type))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-transcript-segment
                  "mevedel-session-artifacts" (session descriptor))
(declare-function mevedel-session-artifacts-transcript-segments
                  "mevedel-session-artifacts"
                  (session live-buffer &optional agent-transcript-p))
(autoload 'mevedel-session-artifacts-read-transcript-segment "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-transcript-segments "mevedel-session-artifacts")
(declare-function mevedel-session-artifacts-transcript-segment-identity
                  "mevedel-session-artifacts" (session descriptor))
(autoload 'mevedel-session-artifacts-transcript-segment-identity
  "mevedel-session-artifacts")

;; `mevedel-view-agent'
(declare-function mevedel-view-open-agent-transcript
                  "mevedel-view-agent" (agent-path))
(autoload 'mevedel-view-open-agent-transcript "mevedel-view-agent")
(defvar mevedel-view--agent-path)

;; `mevedel-view-segments'
(declare-function mevedel-view-go-to-segment "mevedel-view-segments"
                  (&optional number))
(declare-function mevedel-view-historical-segment-p "mevedel-view-segments" ())
(declare-function mevedel-view-return-to-latest-segment
                  "mevedel-view-segments" (&optional event))
(autoload 'mevedel-view-go-to-segment "mevedel-view-segments" nil t)
(autoload 'mevedel-view-historical-segment-p "mevedel-view-segments")
(autoload 'mevedel-view-return-to-latest-segment "mevedel-view-segments" nil t)

;; `mevedel-view-disclosure'
(declare-function mevedel-view-disclosure-data-substring
                  "mevedel-view-disclosure"
                  (data-buf start end &optional properties))
(declare-function mevedel-view-disclosure-indent-text
                  "mevedel-view-disclosure" (text width))
(declare-function mevedel-view-disclosure-record-state
                  "mevedel-view-disclosure" (source vtype collapsed))
(declare-function mevedel-view-disclosure-section-bounds
                  "mevedel-view-disclosure" (&optional property))
(declare-function mevedel-view-disclosure-source-range
                  "mevedel-view-disclosure" (data-buffer start end))
(declare-function mevedel-view-disclosure-state-key
                  "mevedel-view-disclosure"
                  (source vtype &optional previous-key))

;; `mevedel-view-render'
(declare-function mevedel-view--execution-terminal-render-data
                  "mevedel-view-render" (data-buf tool-use-id &optional yielded-id))
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(declare-function mevedel-view--sandbox-summary-line
                  "mevedel-view-render" (summary))
(declare-function mevedel-view--tool-call-parse "mevedel-view-render"
                  (data-buf seg-start seg-end &optional raw))
(declare-function mevedel-view-render-add-display-properties
                  "mevedel-view-render" (start end &optional default-vtype))
(autoload 'mevedel-view--execution-terminal-render-data
  "mevedel-view-render")
(autoload 'mevedel-view--sandbox-summary-line "mevedel-view-render")
(autoload 'mevedel-view--tool-call-parse "mevedel-view-render")
(autoload 'mevedel-view-render-add-display-properties "mevedel-view-render")

;; `mevedel-view-segments'
(declare-function mevedel-view-segments-display-buffer
                  "mevedel-view-segments" ())
(autoload 'mevedel-view-segments-display-buffer "mevedel-view-segments")
(declare-function mevedel-view-segments--session "mevedel-view-segments" ())
(autoload 'mevedel-view-segments--session "mevedel-view-segments")
(declare-function mevedel-view-segments-current-number
                  "mevedel-view-segments" ())
(autoload 'mevedel-view-segments-current-number "mevedel-view-segments")

;; `mevedel-view-disclosure'
(declare-function mevedel-view-toggle-section "mevedel-view-disclosure" ())

(require 'mevedel-tool-render-data)
(require 'mevedel-transcript-audit)
(require 'mevedel-view-disclosure)

(defun mevedel-view--hook-audit-records-from-text
    (text &optional type data-buf source-start)
  "Return hook audit records parsed from TEXT.
When TYPE is non-nil, return only records with matching `:type'.
DATA-BUF and SOURCE-START attach each record's exact source range."
  (delq nil
        (mapcar
         (lambda (span)
           (let ((record (plist-get span :record)))
             (unless (and (null type)
                          (memq (plist-get record :type)
                                '(fork-point provider-tool-batch-start provider-tool-batch
                                  execution-archive execution-completion
                                  mailbox-receipt)))
               (if (and (buffer-live-p data-buf) source-start)
                   (append
                    record
                    (list :source
                          (mevedel-view-disclosure-source-range
                           data-buf
                           (+ source-start (plist-get span :start))
                           (+ source-start (plist-get span :end)))))
                 record))))
         (mevedel-transcript-audit-spans text type))))

(defun mevedel-view--hook-audit-key (record)
  "Return RECORD without view-local source metadata."
  (let (key)
    (while record
      (unless (eq (car record) :source)
        (setq key (append key (list (car record) (cadr record)))))
      (setq record (cddr record)))
    key))

(defun mevedel-view--merge-hook-audits (primary fallback)
  "Return PRIMARY plus FALLBACK records not already present.
PRIMARY records usually have source metadata and are preferred."
  (let ((records (copy-sequence primary)))
    (dolist (record fallback)
      (unless (cl-some
               (lambda (existing)
                 (equal (mevedel-view--hook-audit-key existing)
                        (mevedel-view--hook-audit-key record)))
               records)
        (setq records (append records (list record)))))
    records))

(defun mevedel-view--indent-hook-audit-text (text)
  "Return TEXT with a display-only expanded audit payload inset."
  (mevedel-view-disclosure-indent-text (or text "") 6))

(defun mevedel-view--hook-audit-value-text (value)
  "Return VALUE as stable text for expanded hook audit details."
  (if (stringp value)
      value
    (let ((print-level nil)
          (print-length nil)
          (print-circle t))
      (prin1-to-string value))))

(defun mevedel-view--format-hook-context-audit (record expanded)
  "Return grouped context audit RECORD text.
When EXPANDED is non-nil, include ordered handler details."
  (let ((handlers (plist-get record :handlers))
        (index 0))
    (concat
     (format "  ◇ %s hook added context · %d handler%s\n"
             (or (plist-get record :event) "Hook")
             (length handlers)
             (if (= 1 (length handlers)) "" "s"))
     (when expanded
       (mapconcat
        (lambda (handler)
          (setq index (1+ index))
          (concat
           (mevedel-view-disclosure-indent-text
            (format
             "%d. %s\n" index
             (pcase (plist-get handler :source)
               ('plugin (if-let* ((name (plist-get handler :plugin-name)))
                            (format "%s plugin" name)
                          "plugin hook"))
               ('project-file "project hook")
               ('user-file "user hook")
               ('native "native hook")
               (_ "configured hook")))
            4)
           (mevedel-view-disclosure-indent-text
            (concat
             "Handler: "
             (mevedel-view--hook-audit-value-text
              (or (plist-get handler :description)
                  (plist-get handler :function)
                  (plist-get handler :command)
                  "unknown"))
             "\n")
            7)
           (when-let* ((reason (plist-get handler :reason)))
             (mevedel-view-disclosure-indent-text
              (concat "Reason: " reason "\n") 7))
           (when-let* ((contexts (plist-get handler :contexts)))
             (concat
              (mevedel-view-disclosure-indent-text "Context:\n" 7)
              (mapconcat
               (lambda (context)
                 (mevedel-view-disclosure-indent-text (format "%s" context) 9))
               contexts "\n\n")
              "\n"))))
        handlers "\n")))))

(defun mevedel-view--prompt-rewrite-audit-record
    (event original submitted decision)
  "Return a prompt rewrite audit record, or nil if nothing changed."
  (mevedel--hook-prompt-rewrite-audit-record
   event original submitted
   (mevedel-hooks-decision-reason decision)))

(defun mevedel-view--user-turn-hook-audits (segments data-buf)
  "Return visible hook audit records in user SEGMENTS from DATA-BUF."
  (with-current-buffer data-buf
    (let (records)
      (dolist (seg segments)
        (when (memq (car seg)
                    '(user hook-context prompt render-data ignored))
          (let ((start (cadr seg)))
            (dolist (record
                     (mevedel-view--hook-audit-records-from-text
                      (buffer-substring start (caddr seg)) nil data-buf start))
              (push record records)))))
      (nreverse records))))

(defun mevedel-view--format-injected-reminders-audit (record expanded)
  "Return grouped system-reminder injection RECORD text.
When EXPANDED is non-nil, include each reminder's full body."
  (let* ((items (plist-get record :items))
         (index 0)
         (labels (mapconcat
                  (lambda (item)
                    (mevedel-reminders--entry-label
                     (plist-get item :type)))
                  items ", ")))
    (concat
     (format "  ◇ %d system reminder%s (%s)\n"
             (length items)
             (if (= 1 (length items)) "" "s")
             labels)
     (when expanded
       (mapconcat
        (lambda (item)
          (setq index (1+ index))
          (concat
           (mevedel-view-disclosure-indent-text
            (format "%d. %s\n" index
                    (mevedel-reminders--entry-label (plist-get item :type)))
            4)
           (mevedel-view--indent-hook-audit-text (plist-get item :body))
           "\n"))
        items "\n")))))

(defun mevedel-view--format-hook-audit-block (record expanded)
  "Return display text for hook audit RECORD.
When EXPANDED is non-nil, include record details."
  (pcase (plist-get record :type)
    ('execution-breadcrumb
     (mevedel-view-audit--breadcrumb-label record))
    ('tool-input-repair
     (condition-case nil
         (if-let* ((audit
                    (mevedel-tool-repair-normalize-audit-record record)))
             (concat
              (if (eq (plist-get audit :state) 'committed)
                  "  ◇ tool input repaired\n"
                "  ◇ tool input repair abandoned\n")
              (when expanded
                (mapconcat
                 (lambda (repair)
                   (mevedel-view-disclosure-indent-text
                    (concat
                     "Rule: " (symbol-name (plist-get repair :rule)) "\n"
                     "Path"
                     (if (= 1 (length (plist-get repair :paths))) ": " "s: ")
                     (mapconcat #'mevedel-tool-repair-format-path
                                (plist-get repair :paths) ", ")
                     "\nShape: "
                     (symbol-name (plist-get repair :before))
                     " -> " (symbol-name (plist-get repair :after)) "\n")
                    4))
                 (plist-get audit :repairs)
                 "")))
           "  ◇ tool input repair audit unavailable\n")
       (error "  ◇ tool input repair audit unavailable\n")))
    ('guest-prompt
     ;; Collaboration guest attribution renders as the turn heading
     ;; ("NAME (guest)" instead of "You"), so the record itself needs no
     ;; disclosure line.
     "")
    ('background-event
     ;; The generated wake heading and trigger are hidden in the view.
     "")
    ('prompt-rewrite
     (concat
      "  \u25c7 hook changed prompt\n"
      (when expanded
        (concat
         (mevedel-view-disclosure-indent-text
          (concat
           "Event: " (or (plist-get record :event) "UserPromptSubmit") "\n"
           (when-let* ((reason (plist-get record :reason)))
             (concat "Reason: " reason "\n"))
           "Original prompt:\n")
          4)
         (mevedel-view--indent-hook-audit-text
          (plist-get record :original))
         "\n"
         (mevedel-view-disclosure-indent-text "Submitted prompt:\n" 4)
         (mevedel-view--indent-hook-audit-text
          (plist-get record :submitted))
         "\n"))))
    ('tool-permission
     (concat
      "  \u25c7 hook changed tool permission\n"
      (when expanded
        (mevedel-view-disclosure-indent-text
         (concat
          "Event: " (or (plist-get record :event) "PreToolUse") "\n"
          "Outcome: " (or (plist-get record :outcome) "unknown") "\n"
          (when-let* ((reason (plist-get record :reason)))
            (concat "Reason: " reason "\n")))
         4))))
    ((or 'tool-context 'subagent-context 'compact-context)
     (mevedel-view--format-hook-context-audit record expanded))
    ('tool-input-rewrite
     (concat
      "  \u25c7 hook changed tool input\n"
      (when expanded
        (concat
         (mevedel-view-disclosure-indent-text
          (concat
           "Event: " (or (plist-get record :event) "PreToolUse") "\n"
           (when-let* ((reason (plist-get record :reason)))
             (concat "Reason: " reason "\n"))
           "Original input:\n")
          4)
         (mevedel-view--indent-hook-audit-text
          (mevedel-view--hook-audit-value-text
           (plist-get record :original-input)))
         "\n"
         (mevedel-view-disclosure-indent-text "Updated input:\n" 4)
         (mevedel-view--indent-hook-audit-text
          (mevedel-view--hook-audit-value-text
           (plist-get record :updated-input)))
         "\n"))))
    ('tool-result-rewrite
     (concat
      "  \u25c7 hook changed tool result\n"
      (when expanded
        (concat
         (mevedel-view-disclosure-indent-text
          (concat
           "Event: " (or (plist-get record :event) "PostToolUse") "\n"
           (when-let* ((reason (plist-get record :reason)))
             (concat "Reason: " reason "\n"))
           "Original result:\n")
          4)
         (mevedel-view--indent-hook-audit-text
          (plist-get record :original-result))
         "\n"
         (mevedel-view-disclosure-indent-text "Updated result:\n" 4)
         (mevedel-view--indent-hook-audit-text
          (plist-get record :updated-result))
         "\n"))))
    ('injected-reminders
     (mevedel-view--format-injected-reminders-audit record expanded))
    (_
     (concat
      "  \u25c7 hook audit\n"
      (when expanded
        (mevedel-view-disclosure-indent-text (format "%S\n" record) 4))))))

(defun mevedel-view-audit--hook-audit-state-key (record source)
  "Return the disclosure key for hook audit RECORD at SOURCE."
  (append (mevedel-view-disclosure-state-key source 'hook-audit)
          (list (mevedel-view--hook-audit-key record))))

(defun mevedel-view--insert-hook-audit-block (record &optional source)
  "Insert hook audit disclosure for RECORD.
SOURCE, when non-nil, is the source range in the data buffer.  The body
opens only when the reader expanded it: rendering in the remembered state
leaves nothing for the post-render restore to toggle, which would otherwise
rewrite the retained live tail and force a whole-turn rebuild per update."
  (when (and (listp record)
             (keywordp (car-safe record)))
    (let* ((start (point))
           (source (and (consp source) (cons (car source) (cdr source))))
           (key (mevedel-view-audit--hook-audit-state-key record source))
           (expanded (when-let* ((entry (mevedel-view-disclosure-state-for-key
                                         key)))
                       (not (cdr entry)))))
      (if (eq (plist-get record :type) 'execution-breadcrumb)
          (mevedel-view-audit--insert-breadcrumb record source)
        (insert (mevedel-view--format-hook-audit-block record expanded))
        (add-text-properties
         start (point)
         `(font-lock-face mevedel-view-hook-audit
           mevedel-view-type hook-audit
           mevedel-view-collapsed ,(not expanded)
           mevedel-view-hook-audit-record ,record
           mevedel-view-source ,source
           mevedel-view-source-key ,key))))))

(defun mevedel-view-audit-breadcrumb-status (record)
  "Return execution breadcrumb RECORD's terminal status word."
  (let* ((facts (plist-get record :facts))
         (termination (plist-get facts :termination)))
    (cond ((memq termination '(stopped interrupted cancelled owner-stopped))
           "Stopped")
          ((memq termination '(timeout timed-out expired)) "Timed out")
          ((not (memq (plist-get facts :outcome)
                      '(success no-match different false)))
           "Failed")
          (t "Completed"))))

(defun mevedel-view-audit--breadcrumb-label (record)
  "Return the one completion line for execution breadcrumb RECORD.
It is worded like a background notice; the transcript's own delivery of
the same completion renders nothing, so this is the only line."
  (let* ((facts (plist-get record :facts))
         (termination (plist-get facts :termination))
         (status (mevedel-view-audit-breadcrumb-status record))
         (failed (equal status "Failed"))
         (code (plist-get facts :exit-code))
         (monitor (eq (plist-get facts :kind) 'monitor))
         (events (plist-get facts :event-count))
         (command (or (plist-get facts :description)
                      (plist-get record :command)
                      (plist-get facts :command) "Bash"))
         (command (replace-regexp-in-string "[\n\r\t]+" " " command))
         (owner (plist-get record :owner))
         (sender (and (stringp owner)
                      (not (equal owner (or mevedel-view--agent-path "/root")))
                      (replace-regexp-in-string "[\n\r\t]+" " " owner))))
    (concat
     "  "
     (propertize "● " 'font-lock-face (if failed 'error 'mevedel-view-tool-metadata))
     (format "%s \"%s\" %s%s%s  "
             (if monitor "Monitor" "Background command")
             (truncate-string-to-width command 64 nil nil "…")
             (downcase status)
             (cond
              (monitor
               (if (integerp events)
                   (format " · %d event%s" events (if (= events 1) "" "s"))
                 ""))
              ((and (integerp code)
                    (memq termination '(nil exited signaled interrupted)))
               (format " (%s %d)"
                       (if (memq termination '(signaled interrupted))
                           "signal" "exit code")
                       code))
              (t ""))
             (if sender
                 (format " · %s" (truncate-string-to-width sender 48 nil nil "…"))
               "")))))

(defun mevedel-view-audit--insert-breadcrumb (record source)
  "Insert RECORD as a linked, non-expandable completion line at SOURCE."
  (let ((start (point))
        (link (propertize "[Show result]"
                          'font-lock-face 'link
                          'mouse-face 'highlight
                          'help-echo "RET: open execution result"
                          'mevedel-view-zone-activate
                          (lambda () (mevedel-view-audit-show-result record)))))
    (insert (mevedel-view-audit--breadcrumb-label record) link "\n")
    (add-text-properties start (point)
                         `(mevedel-view-type execution-breadcrumb
                           mevedel-view-source ,source
                           mevedel-view-execution-breadcrumb ,record))
    (let ((inhibit-read-only t))
      (mevedel-view-render-add-display-properties
       start (point) 'execution-breadcrumb))))

(defun mevedel-view-audit-mailbox-completion (text)
  "Return the execution facts delivered by the mailbox block TEXT.
Only the structural opener supplies identity and terminal facts; the body
is retained verbatim as evidence, never interpreted as metadata."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (skip-chars-forward " \t\r\n")
    (when-let* ((block (mevedel-transcript--mailbox-any-block-at-point (point-max)))
                ((eq (plist-get block :kind) 'agent-message))
                (body (buffer-substring-no-properties
                       (plist-get block :body-start) (plist-get block :body-end)))
                (record (mevedel-view-audit-mailbox-breadcrumb
                         body (plist-get block :id)
                         (buffer-substring-no-properties
                          (plist-get block :open-start) (plist-get block :open-end)))))
      (plist-put record :delivery-body body))))

(defun mevedel-view-audit--projected-in-p (record start limit &optional object)
  "Return non-nil if OBJECT from START to LIMIT projects RECORD's completion.
OBJECT is a string or nil for the current buffer.  A breadcrumb line
carries its own record; an activity group row carries the completions
folded into it, so a collapsed group still counts as projecting them."
  (cl-some
   (lambda (property)
     (let ((pos start) found)
       (while (and (< pos limit) (not found))
         (let ((value (get-text-property pos property object)))
           (setq found
                 (cl-some
                  (lambda (prior)
                    (and (equal (plist-get prior :execution-id)
                                (plist-get record :execution-id))
                         (equal (plist-get prior :owner)
                                (plist-get record :owner))))
                  (if (eq property 'mevedel-view-execution-completions)
                      value
                    (and value (list value))))
                 pos (or (next-single-property-change
                          pos property object limit)
                         limit))))
       found))
   '(mevedel-view-execution-breadcrumb mevedel-view-execution-completions)))

(defun mevedel-view-audit--stashed-breadcrumb-p (stash record)
  "Return non-nil if STASH contains RECORD's execution breadcrumb."
  (mevedel-view-audit--projected-in-p record 0 (length stash) stash))

(defun mevedel-view-audit-breadcrumb-present-p (record before)
  "Return non-nil if RECORD was already projected before BEFORE.
Identity is local to this receiving transcript, not to the global
execution, so parent and child may each display their own breadcrumb."
  (let ((found (mevedel-view-audit--projected-in-p record (point-min) before))
        (pos (point-min)))
    (unless found
      (setq pos (point-min))
      (while (and (< pos before) (not found))
        (let ((stash (get-text-property pos 'mevedel-view-stash)))
          (setq found (and (stringp stash)
                           (mevedel-view-audit--stashed-breadcrumb-p
                            stash record))
                pos (or (next-single-property-change
                         pos 'mevedel-view-stash nil before)
                        before)))))
    (or found
        (when-let* ((data (mevedel-view-segments-display-buffer))
                    ((buffer-live-p data))
                    (session (or (mevedel-view-segments--session)
                                 (buffer-local-value 'mevedel--session data))))
          (let* ((descriptors (mevedel-session-artifacts-transcript-segments
                               session data mevedel-view--agent-transcript-p))
                 (viewed-number
                  (or (mevedel-view-segments-current-number)
                      (plist-get (cl-find-if
                                  (lambda (entry) (plist-get entry :current-p))
                                  descriptors)
                                 :number)
                      ;; Agent archives have numbers; the live agent transcript
                      ;; is their successor but has no segment number of its own.
                      most-positive-fixnum)))
            (let ((key (cons (plist-get record :execution-id)
                             (plist-get record :owner))))
              (cl-some
               (lambda (descriptor)
                 (and (eq (plist-get descriptor :status) 'readable)
                      (not (plist-get descriptor :current-p))
                      (integerp (plist-get descriptor :number))
                      (< (plist-get descriptor :number) viewed-number)
                      (member key (mevedel-view-audit--archived-breadcrumbs
                                   session descriptor))))
               descriptors)))))))

(defvar-local mevedel-view-audit--archive-breadcrumbs nil
  "Map an archived segment's content identity to its breadcrumb keys.")

(defun mevedel-view-audit--archived-breadcrumbs (session descriptor)
  "Return (EXECUTION-ID . OWNER) for each breadcrumb in SESSION's DESCRIPTOR.
Count only origin audit records, not independent mailbox notices.
Every streamed update asks
again for each fresh breadcrumb in the live turn; rereading and reparsing
each archive per question made it most of a long session's render cost.
Results are kept in the asking view until the archive's content identity
changes."
  (let* ((identity (mevedel-session-artifacts-transcript-segment-identity
                    session descriptor))
         (cache (or mevedel-view-audit--archive-breadcrumbs
                    (setq mevedel-view-audit--archive-breadcrumbs
                          (make-hash-table :test #'equal))))
         (cached (if identity (gethash identity cache :missing) :missing)))
    (if (not (eq cached :missing))
        cached
      (when-let* ((older (condition-case nil
                             (mevedel-session-artifacts-read-transcript-segment
                              session descriptor)
                           (error nil))))
        (let ((key (lambda (other)
                     (cons (plist-get other :execution-id)
                           (plist-get other :owner))))
              keys)
          (unwind-protect
              (with-current-buffer older
                (save-restriction
                  (widen)
                  (setq keys
                        (mapcar key (mevedel-transcript-audit-records
                                     (buffer-substring (point-min) (point-max))
                                     'execution-breadcrumb)))))
            (kill-buffer older))
          (when identity
            (puthash identity keys cache))
          keys)))))

(defun mevedel-view-audit--source-has-tool-p (source tool-use-id)
  "Return non-nil if SOURCE contains a call owning TOOL-USE-ID."
  (when (and source (buffer-live-p (mevedel-view-segments-display-buffer)))
    (with-current-buffer (mevedel-view-segments-display-buffer)
      (let ((at (car source)) (end (cdr source)) match)
        (while (and (< at end) (not match))
          (let* ((prop (get-text-property at 'gptel))
                 (id (and (eq (car-safe prop) 'tool) (cdr prop))))
            (setq match (and (stringp id)
                             (or (equal id tool-use-id)
                                 (string-prefix-p (concat id "/") tool-use-id)))
                  at (or (next-single-property-change at 'gptel nil end)
                         end))))
        match))))

(defun mevedel-view-audit--stashed-result-p (stash tool-use-id)
  "Return non-nil if STASH retains TOOL-USE-ID's source-backed row."
  (let ((pos 0) (limit (length stash)) found)
    (while (and (< pos limit) (not found))
      (let ((source (get-text-property pos 'mevedel-view-source stash))
            (type (get-text-property pos 'mevedel-view-type stash))
            (child (get-text-property pos 'mevedel-view-tool-child stash)))
        (setq found (or (and child (equal (plist-get child :id) tool-use-id))
                        (and (memq type '(tool-summary tool-group tool-child))
                             (mevedel-view-audit--source-has-tool-p
                              source tool-use-id)))
              pos (min (or (next-single-property-change
                            pos 'mevedel-view-source stash limit) limit)
                       (or (next-single-property-change
                            pos 'mevedel-view-type stash limit) limit)
                       (or (next-single-property-change
                            pos 'mevedel-view-tool-child stash limit) limit)))))
    found))

(defun mevedel-view-audit--result-position (tool-use-id)
  "Find TOOL-USE-ID's rendered row in the current view, including folded groups."
  (let ((pos (point-min)) found group
        (parent-id (and (stringp tool-use-id)
                        (string-match "\\`\\(.*\\)/[0-9]+\\'" tool-use-id)
                        (match-string 1 tool-use-id))))
    (while (and (< pos (point-max)) (not found))
      (let ((source (get-text-property pos 'mevedel-view-source))
            (type (get-text-property pos 'mevedel-view-type))
            (child (get-text-property pos 'mevedel-view-tool-child)))
        (when (and (eq type 'turn-summary)
                   (when-let* ((stash (get-text-property
                                      pos 'mevedel-view-stash)))
                     (mevedel-view-audit--stashed-result-p
                      stash tool-use-id)))
          (setq found pos))
        (when (and child (equal (plist-get child :id) tool-use-id))
          (setq found pos))
        (when (and (not found)
                   (memq type '(tool-summary tool-group tool-child))
                   (mevedel-view-audit--source-has-tool-p source tool-use-id))
          (if (and parent-id
                   (or (eq type 'tool-summary)
                       (and (eq type 'tool-child)
                            (equal (plist-get child :id) parent-id)))
                   (let* ((render-data
                           (if (eq type 'tool-child)
                               (plist-get child :render-data)
                             (with-current-buffer
                                 (mevedel-view-segments-display-buffer)
                               (save-restriction
                                 (widen)
                                 ;; A nested parent's metadata does not make
                                 ;; this outer summary its direct-call row.
                                 (when (mevedel-tool-render-data-segment-bounds
                                        parent-id)
                                   (mevedel-tool-render-data-for-tool
                                    (current-buffer) parent-id))))))
                          (direct (mevedel-tool-render-data-direct-call
                                   "ToolCall" render-data)))
                     (and (equal (plist-get direct :tool) "Bash")
                          (equal (plist-get direct :id) tool-use-id))))
              ;; A direct ToolCall has no Bash child view row: its own
              ;; Bash-style summary is canonical, even while still collapsed.
              (setq found pos)
            (if (or (eq type 'tool-group)
                    (and parent-id
                         (or (eq type 'tool-summary)
                             (and (eq type 'tool-child)
                                  (not (equal (plist-get child :id)
                                              tool-use-id))))))
              (when (get-text-property pos 'mevedel-view-collapsed)
                (unless group (setq group pos)))
              (setq found pos)))))
      (setq pos (min (or (next-single-property-change
                          pos 'mevedel-view-source nil (point-max)) (point-max))
                     (or (next-single-property-change
                          pos 'mevedel-view-type nil (point-max)) (point-max)))))
    (or found group)))

(defun mevedel-view-audit--owner-record (record)
  "Resolve forwarded RECORD's identity across the opened child transcript.
The owning breadcrumb can be in an older segment after compaction, even when
the forwarded mailbox message appears only in the parent's current segment."
  (let* ((data (mevedel-view-segments-display-buffer))
         (session (and (buffer-live-p data)
                       (buffer-local-value 'mevedel--session data)))
         (find-record
          (lambda (buffer)
            (with-current-buffer buffer
              (save-restriction
                (widen)
                (cl-find-if
                 (lambda (candidate)
                   (equal (plist-get candidate :execution-id)
                          (plist-get record :execution-id)))
                 (mevedel-transcript-audit-records
                  (buffer-substring (point-min) (point-max))
                  'execution-breadcrumb))))))
         (current (and (buffer-live-p data) (funcall find-record data))))
    (or current
        (and session
             (catch 'found
               (dolist (descriptor (reverse (mevedel-session-artifacts-transcript-segments
                                             session data mevedel-view--agent-transcript-p)))
                 (when (and (eq (plist-get descriptor :status) 'readable)
                            (not (plist-get descriptor :current-p)))
                   (when-let* ((older
                                (condition-case nil
                                    (mevedel-session-artifacts-read-transcript-segment
                                     session descriptor)
                                  (error nil))))
                     (unwind-protect
                         (when-let* ((owner-record (funcall find-record older)))
                           (throw 'found owner-record))
                       (kill-buffer older)))))))
        record)))

(defun mevedel-view-audit-locate-source (session live-buffer tool-id &optional agent-transcript-p)
  "Return the readable descriptor owning TOOL-ID in SESSION's LIVE-BUFFER.
AGENT-TRANSCRIPT-P selects the child's own compact archives, never root
history.  Nested ToolCall IDs resolve their outer source.  This read-only
lookup releases archive buffers and never selects or modifies a view."
  (when (and session (buffer-live-p live-buffer) (stringp tool-id))
    (let ((parent-id tool-id)
          (descriptors (reverse (mevedel-session-artifacts-transcript-segments
                                 session live-buffer agent-transcript-p)))
          chosen)
      (while (string-match "/[0-9]+\\'" parent-id)
        (setq parent-id (substring parent-id 0 (match-beginning 0))))
      (dolist (descriptor descriptors)
        (when (and (not chosen) (eq (plist-get descriptor :status) 'readable))
          (let* ((current-p (plist-get descriptor :current-p))
                 (buffer (if current-p live-buffer
                           (condition-case nil
                               (mevedel-session-artifacts-read-transcript-segment
                                session descriptor)
                             (error nil)))))
            (when (buffer-live-p buffer)
              (unwind-protect
                  (with-current-buffer buffer
                    (save-restriction
                      (widen)
                      (let ((at (point-min)))
                        (while (and (< at (point-max)) (not chosen))
                          (when (equal (get-text-property at 'gptel)
                                       (cons 'tool parent-id))
                            (setq chosen descriptor))
                          (setq at (or (next-single-property-change
                                        at 'gptel nil (point-max))
                                       (point-max)))))))
                (unless current-p (kill-buffer buffer)))))))
      chosen)))

(defun mevedel-view-audit--select-source (record)
  "Select the archived segment owning RECORD's tool row, if any.
The completion's source target may be a later segment after compaction."
  (when-let* (((buffer-live-p mevedel--data-buffer))
              (session (buffer-local-value 'mevedel--session mevedel--data-buffer))
              (chosen (mevedel-view-audit-locate-source
                       session mevedel--data-buffer (plist-get record :tool-use-id)
                       mevedel-view--agent-transcript-p)))
      ;; The view's segment switcher follows this receiving transcript:
      ;; root segments for the session, agent compaction archives for an agent.
      (when chosen
        (if (plist-get chosen :current-p)
            (when (mevedel-view-historical-segment-p)
              (mevedel-view-return-to-latest-segment))
          (mevedel-view-go-to-segment (plist-get chosen :number))))))

(defun mevedel-view-audit--evidence (record)
  "Display retained read-only evidence or an explicit limitation for RECORD."
  (let* ((tool-id (plist-get record :tool-use-id))
         (data (and (buffer-live-p mevedel--data-buffer)
                    (or (mevedel-execution-transcript-pending-render-data
                         mevedel--data-buffer tool-id)
                        (mevedel-tool-render-data-for-tool
                         mevedel--data-buffer tool-id)
                        (mevedel-view--execution-terminal-render-data
                         nil tool-id (plist-get record :execution-id)))))
         (facts (plist-get record :facts))
         (output (or (plist-get data :execution-output)
                     (plist-get facts :execution-output)))
         (execution-error (plist-get data :execution-error))
         (sandbox (or (plist-get data :sandbox-summary)
                      (plist-get facts :sandbox-summary)))
         (preview-truncated
          (or (plist-get data :output-preview-truncated-p)
              (plist-get facts :output-preview-truncated-p)
              (> (or (plist-get data :omitted-output-bytes)
                     (plist-get facts :omitted-output-bytes) 0) 0)))
         (output-limited
          (or (eq (plist-get data :termination) 'output-limit)
              (eq (plist-get facts :termination) 'output-limit)
              (plist-get data :output-limit-p)
              (plist-get facts :output-limit-p)))
         (path (plist-get facts :output-path))
         (session (and (buffer-live-p mevedel--data-buffer)
                       (buffer-local-value 'mevedel--session mevedel--data-buffer)))
         (artifact (and session (stringp path)
                        (string-prefix-p "artifact://" path)
                        (condition-case nil
                            (mevedel-resource-prepare
                             'read path (list :session session))
                          (error nil))))
         (physical (and artifact (not (plist-get artifact :unavailable-p))
                        (plist-get artifact :physical-path)))
         (artifact-size
          (and (stringp physical)
               (condition-case nil
                   (and (file-regular-p physical) (file-readable-p physical)
                        (file-attribute-size (file-attributes physical)))
                 (file-error nil))))
         (output-bytes (or (plist-get data :output-bytes)
                           (plist-get facts :output-bytes)))
         (stale-artifact (and (integerp artifact-size)
                              (integerp output-bytes)
                              (< artifact-size output-bytes)))
         (buffer (get-buffer-create "*mevedel execution result*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or (plist-get record :command) (plist-get facts :command) "Bash")
                "\n\n")
        (when-let* ((disclosure (mevedel-view--sandbox-summary-line sandbox)))
          (insert disclosure "\n"))
        (cond
         (output-limited
          (insert "Output truncated: execution output limit reached; retained output may omit content.\n"))
         (stale-artifact
          (insert "Output truncated: retained artifact may omit later output.\n"))
         ((and preview-truncated
               (not (and (integerp artifact-size)
                         (integerp output-bytes))))
          (insert "Output truncated: retained preview may omit content.\n")))
        (when execution-error
          (insert (format "%s: %s\n"
                          (if (eq (plist-get data :termination) 'spawn-failed)
                              "Failed to start process"
                            "Execution error")
                          execution-error)))
        (cond
         ((integerp artifact-size)
          (when (and (zerop (cadr (insert-file-contents physical)))
                     (not execution-error))
            (insert "Execution produced no output.\n")))
         ((stringp output)
          (unless (and execution-error (string-empty-p output))
            (insert (if (string-empty-p output)
                        (if preview-truncated
                            "Retained preview contains no output.\n"
                          "Execution produced no output.\n")
                      output))))
         (t
          (insert "Original execution row and retained output are unavailable.\n"
                  (if (stringp path)
                      (format "Retained output address: %s\n" path)
                    ""))))
        (special-mode)))
    (display-buffer buffer)))

(defun mevedel-view-audit-show-result (record)
  "Reveal RECORD's canonical execution row, or display retained evidence."
  (interactive (list (get-text-property (point) 'mevedel-view-execution-breadcrumb)))
  (unless record (user-error "No execution breadcrumb at point"))
  (when (and (stringp (plist-get record :owner))
             (not (equal (plist-get record :owner) "/root"))
             (not (and mevedel-view--agent-transcript-p
                       (equal (plist-get record :owner)
                              mevedel-view--agent-path))))
    (condition-case nil
        (progn
          (mevedel-view-open-agent-transcript (plist-get record :owner))
          (when (with-current-buffer (window-buffer (selected-window))
                  mevedel-view--agent-transcript-p)
            (set-buffer (window-buffer (selected-window)))
            (mevedel-view--full-rerender)
            (setq record (mevedel-view-audit--owner-record record))))
      (error nil)))
  (mevedel-view-audit--select-source record)
  (let* ((id (plist-get record :tool-use-id))
         (pos (and id (mevedel-view-audit--result-position id))))
    (while (and pos (get-text-property pos 'mevedel-view-collapsed)
                (or (memq (get-text-property pos 'mevedel-view-type)
                          '(tool-group turn-summary))
                    (and (string-match-p "/[0-9]+\\'" (or id ""))
                         (not (equal (get-text-property
                                      pos 'mevedel-view-tool-use-id) id))
                         (not (equal (plist-get
                                      (get-text-property
                                       pos 'mevedel-view-tool-child) :id) id)))))
      (goto-char pos)
      (mevedel-view-toggle-section)
      (setq pos (mevedel-view-audit--result-position id)))
    (if (and pos (not (eq (get-text-property pos 'mevedel-view-type) 'tool-group)))
        (progn
          (goto-char pos)
          (when (get-text-property pos 'mevedel-view-collapsed)
            (mevedel-view-toggle-section))
          (when-let* ((window (get-buffer-window (current-buffer) t)))
            (set-window-point window (point))))
      (mevedel-view-audit--evidence record))))

(defun mevedel-view-audit--control-record-in-buffer (data execution-id)
  "Find EXECUTION-ID's source identity or retained evidence in DATA.
Return nil if this segment has neither.  Nested ToolCall Bash children have
their own source IDs even though they share the outer view row."
  (with-current-buffer data
    (save-restriction
      (widen)
      (let* ((tool-id
              (cl-labels
                  ((child-id (calls)
                     (cl-some
                      (lambda (child)
                        (let ((facts (plist-get child :render-data)))
                          (or (and (equal (plist-get child :tool) "Bash")
                                   (equal (plist-get facts :execution-id)
                                          execution-id)
                                   (plist-get child :id))
                              (and (equal (plist-get child :tool) "ToolCall")
                                   (child-id (plist-get facts :calls))))))
                      calls)))
                (let ((pos (point-min)) found)
                  (while (and (< pos (point-max)) (not found))
                    (let* ((property (get-text-property pos 'gptel))
                           (id (and (eq (car-safe property) 'tool)
                                    (cdr property)))
                           (bounds (and (stringp id)
                                        (mevedel-tool-render-data-segment-bounds
                                         id)))
                           (name (and bounds
                                      (plist-get
                                       (mevedel-view--tool-call-parse
                                        data (car bounds) (cdr bounds))
                                       :name)))
                           (facts (and (stringp id)
                                       (mevedel-tool-render-data-for-tool
                                        data id))))
                      (setq found
                            (or (and (equal name "Bash")
                                     (equal (plist-get facts :execution-id)
                                            execution-id)
                                     id)
                                (and (equal name "ToolCall")
                                     (child-id (plist-get facts :calls))))
                            pos (or (next-single-property-change
                                     pos 'gptel nil (point-max))
                                    (point-max)))))
                  found)))
             (audits (mevedel-transcript-audit-records
                      (buffer-substring (point-min) (point-max))))
             (breadcrumb
              (cl-find-if
               (lambda (candidate)
                 (and (eq (plist-get candidate :type) 'execution-breadcrumb)
                      (equal (plist-get candidate :execution-id) execution-id)))
               audits))
             (archived
              (unless breadcrumb
                (cl-find-if
                 (lambda (candidate)
                   (and (memq (plist-get candidate :type)
                              '(execution-archive execution-completion))
                        (equal (plist-get (plist-get candidate :render-data)
                                          :execution-id)
                               execution-id)))
                 (reverse audits))))
             (record (or breadcrumb
                         (and archived
                              (list :execution-id execution-id
                                    :tool-use-id (plist-get archived :tool-use-id)
                                    :facts (plist-get archived :render-data))))))
        (when (or tool-id record)
          (if tool-id
              (plist-put (or (copy-sequence record)
                             (list :execution-id execution-id))
                         :tool-use-id tool-id)
            record))))))

(defun mevedel-view-audit-execution-record (session live execution-id &optional agent-transcript-p)
  "Find EXECUTION-ID's trusted source record in SESSION's receiving LIVE buffer.
Search current and readable older transcripts without opening a view.
AGENT-TRANSCRIPT-P restricts history to this child's own compact archives."
  (when (buffer-live-p live)
    (or (mevedel-view-audit--control-record-in-buffer live execution-id)
        (and session (catch 'found
          (dolist (descriptor (reverse (mevedel-session-artifacts-transcript-segments
                                        session live agent-transcript-p)))
            (when (and (eq (plist-get descriptor :status) 'readable)
                       (not (plist-get descriptor :current-p)))
              (when-let* ((older
                           (condition-case nil
                               (mevedel-session-artifacts-read-transcript-segment session descriptor)
                             (error nil))))
                (unwind-protect
                    (when-let* ((match (mevedel-view-audit--control-record-in-buffer older execution-id)))
                      (throw 'found match))
                  (kill-buffer older))))))))))

(defun mevedel-view-audit-show-control-result (execution-id)
  "Open EXECUTION-ID's original Bash row from an input or stop interaction.
Search the displayed transcript and readable older segments by execution ID,
including nested ToolCall children.  When no row survives, show retained
evidence or an explicit absence."
  (interactive)
  (let* ((data (mevedel-view-segments-display-buffer))
         (record (and (buffer-live-p data)
                      (mevedel-view-audit--control-record-in-buffer
                       data execution-id)))
         (live (and (buffer-live-p mevedel--data-buffer)
                    mevedel--data-buffer))
         (session (and live (buffer-local-value 'mevedel--session live))))
    (unless record
      (setq record (mevedel-view-audit-execution-record
                    session live execution-id mevedel-view--agent-transcript-p)))
    (mevedel-view-audit-show-result
     (or record (list :execution-id execution-id)))))

(defun mevedel-view-audit-mailbox-breadcrumb (text sender open-tag)
  "Build execution evidence from mailbox TEXT, SENDER and OPEN-TAG.
The structural OPEN-TAG alone supplies identity and terminal facts.  TEXT
is retained verbatim, including any output that resembles metadata."
  (condition-case nil
      (with-temp-buffer
        (insert open-tag "</agent-message>")
        (let* ((node (car (xml-parse-region (point-min) (point-max))))
               (attributes (cadr node))
               (id (alist-get 'execution_id attributes))
               (code (alist-get 'exit_code attributes))
               (output-bytes (alist-get 'output_bytes attributes))
               (omitted (alist-get 'omitted_output_bytes attributes))
               (termination (alist-get 'termination attributes))
               (outcome (alist-get 'outcome attributes))
               (command (alist-get 'command attributes))
               (path (alist-get 'output_path attributes)))
          (when (and (eq (car node) 'agent-message)
                     (equal (alist-get 'type attributes) "EXECUTION")
                     (stringp id) (not (string-empty-p id)))
            (list :type 'execution-breadcrumb :execution-id id :owner sender
                  :command (or command "Bash")
                  :facts (list :outcome (and outcome (intern outcome))
                               :description (alist-get 'description attributes)
                               :termination (and termination (intern termination))
                               :exit-code (and code (string-to-number code))
                               :output-bytes (and output-bytes
                                                  (string-to-number output-bytes))
                               :omitted-output-bytes (and omitted
                                                          (string-to-number omitted))
                               :output-path path
                               :execution-output text)))))
    (error nil)))

(defun mevedel-view-audit--history-render-data-owner (pos)
  "Return the tool owning the render-data block around POS, or nil."
  (save-excursion
    (goto-char (or (previous-single-property-change
                   (min (1+ pos) (point-max)) 'gptel nil (point-min))
                  (point-min)))
    (let (owner)
      (while (and (not owner)
                  (search-forward mevedel-tool-render-data-open pos t))
        (when-let* ((block (mevedel-tool-render-data-block-at (match-beginning 0))))
          (when (and (> (cadr block) pos)
                     (or (mevedel-tool-render-data-call-range-p
                          (caddr block) (car block) (cadr block))
                         (mevedel-tool-render-data-trusted-range-p
                          (car block) (cadr block))))
            (setq owner (plist-get (caddr block) :mevedel-tool-use-id)))
          (goto-char (min pos (cadr block)))))
      owner)))

(defun mevedel-view-audit--history-delivery (start pos execution-id)
  "Scan from START for EXECUTION-ID's mailbox delivery around POS.
Return (RECORD . END), where RECORD is nil for other mailbox blocks or text.
END is the enclosing block's end, or POS when no block encloses it.  Resume
the next scan there so earlier blocks and ignored payloads are not reparsed."
  (save-excursion
    (goto-char start)
    (let (found)
      (while (and (not found) (re-search-forward "<agent-message\\_>" pos t))
        (goto-char (match-beginning 0))
        (if-let* ((block (mevedel-transcript--mailbox-any-block-at-point (point-max))))
            (let ((end (plist-get block :close-end)))
              (when (> end pos)
                (setq found block))
              (goto-char end))
          (forward-char)))
      (cons
       (when-let* ((block found)
                   (record (mevedel-view-audit-mailbox-completion
                            (buffer-substring-no-properties
                             (plist-get block :open-start) (plist-get block :close-end))))
                   ((equal execution-id (plist-get record :execution-id))))
         (list :name "Execution delivery"
               :args (list :execution_id execution-id
                           :sender (plist-get record :owner))
               :result (plist-get record :delivery-body)))
       (if found (plist-get found :close-end) pos)))))

(defun mevedel-view-audit--history-records (execution-id)
  "Return retained tool interactions concerning EXECUTION-ID in segment order.
Only text naming EXECUTION-ID is parsed.  Segments are read newest first and
the walk stops at the one holding the call that started the execution, since
nothing concerning it can be older."
  (let* ((live (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
         (display (mevedel-view-segments-display-buffer))
         (session (and (buffer-live-p live)
                       (buffer-local-value 'mevedel--session live)))
         records origin-p)
    (cl-labels
        ((collect (call)
           (let ((data (plist-get call :render-data)))
             (when (or (equal execution-id
                              (plist-get (plist-get call :args) :execution_id))
                       (equal execution-id (plist-get data :execution-id)))
               (push call records)
               (unless (plist-get (plist-get call :args) :execution_id)
                 (setq origin-p t)))
             (dolist (child (plist-get data :calls))
               (collect (list :name (plist-get child :tool)
                              :args (plist-get child :args)
                              :result (plist-get child :result)
                              :render-data (plist-get child :render-data))))))
         (collect-buffer (data)
           (let (seen mailbox-end)
             (setq records nil)
             (with-current-buffer data
               (save-restriction
                 (widen)
                 (save-excursion
                   (goto-char (point-min))
                   (setq mailbox-end (point-min))
                   (while (search-forward execution-id nil t)
                     (let* ((hit (match-beginning 0))
                            (property (get-text-property hit 'gptel))
                            (tool-id
                             (if (eq (car-safe property) 'tool)
                                 (cdr property)
                               (mevedel-view-audit--history-render-data-owner
                                hit)))
                            (delivery
                             (unless tool-id
                               (mevedel-view-audit--history-delivery
                                mailbox-end hit execution-id))))
                       (cond
                        (delivery
                         (when (car delivery)
                           (push (car delivery) records))
                         (setq mailbox-end (cdr delivery))
                         (goto-char (max (point) mailbox-end)))
                        ((and tool-id (not (member tool-id seen)))
                         (push tool-id seen)
                         ;; ponytail: each owning call rescans property runs
                         ;; from the buffer start; index ids if hits multiply.
                         (when-let* ((bounds
                                      (mevedel-tool-render-data-segment-bounds
                                       tool-id))
                                     (call (mevedel-view--tool-call-parse
                                            data (car bounds) (cdr bounds))))
                           (let ((retained (mevedel-tool-render-data-for-tool
                                            data tool-id)))
                             (collect (if retained
                                          (plist-put (copy-sequence call)
                                                     :render-data retained)
                                        call)))))))))))
             (nreverse records))))
      (if (and session (buffer-live-p live))
          (let (all)
            (catch 'origin
              (dolist (descriptor (reverse
                                   (mevedel-session-artifacts-transcript-segments
                                    session live mevedel-view--agent-transcript-p)))
                (when (eq (plist-get descriptor :status) 'readable)
                  (setq all
                        (append
                         (if (plist-get descriptor :current-p)
                             (collect-buffer live)
                           (when-let* ((older
                                        (condition-case nil
                                            (mevedel-session-artifacts-read-transcript-segment
                                             session descriptor)
                                          (error nil))))
                             (unwind-protect
                                 (collect-buffer older)
                               (kill-buffer older))))
                         all))
                  (when origin-p (throw 'origin nil)))))
            all)
        (when (buffer-live-p display)
          (collect-buffer display))))))

(defun mevedel-view-audit--history-result (text)
  "Return TEXT with its `<bash-execution/>' element spelled out as facts."
  (let ((text (substring-no-properties text)))
    (if-let* (((string-match "<bash-execution[^<>]*/>" text))
              (start (match-beginning 0))
              (end (match-end 0))
              (attributes
               (condition-case nil
                   (with-temp-buffer
                     (insert (match-string 0 text))
                     (cadr (car (xml-parse-region (point-min) (point-max)))))
                 (error nil))))
        (concat (string-trim-right (substring text 0 start))
                "\n\n"
                (mapconcat (lambda (attribute)
                             (format "  %s: %s" (car attribute) (cdr attribute)))
                           attributes "\n")
                (substring text end))
      text)))

(defun mevedel-view-audit-show-history (execution-id)
  "Open a read-only disclosure of retained tool records for EXECUTION-ID."
  (interactive "sExecution ID: ")
  (let ((records (mevedel-view-audit--history-records execution-id))
        (buffer (get-buffer-create "*mevedel execution history*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Execution history · %s\n\n" execution-id))
        (if records
            (dolist (call records)
              (insert (propertize (plist-get call :name)
                                  'font-lock-face 'bold)
                      "\n")
              (cl-loop for (key value) on (plist-get call :args) by #'cddr
                       do (insert (format "  %s: %s\n"
                                          (substring (symbol-name key) 1)
                                          (if (stringp value)
                                              value
                                            (prin1-to-string value)))))
              (unless (string-empty-p (or (plist-get call :result) ""))
                (insert "\n" (mevedel-view-audit--history-result
                              (plist-get call :result))
                        "\n"))
              (insert "\n"))
          (insert "Retained interaction records are unavailable in this transcript.\n"))
        (special-mode)))
    (display-buffer buffer)))

(defun mevedel-view-audit-insert-history-link (rendering)
  "Insert an explicit history action for expanded Bash RENDERING, if retained."
  (when-let* ((tool-id (plist-get rendering :tool-use-id))
              (data (mevedel-view-segments-display-buffer))
              ((buffer-live-p data))
              (id (or (plist-get rendering :execution-id)
                      (plist-get
                       (or (mevedel-tool-render-data-for-tool data tool-id)
                           (mevedel-execution-transcript-pending-render-data
                            data tool-id))
                       :execution-id))))
    (unless (eq (char-before) ?\n) (insert "\n"))
    (insert (propertize
             "[Execution history]"
             'font-lock-face 'link
             'mouse-face 'highlight
             'help-echo "RET: inspect retained execution interactions"
             'mevedel-view-zone-activate
             (lambda () (mevedel-view-audit-show-history id)))
            "\n")))

(defun mevedel-view-audit-toggle-hook-audit ()
  "Toggle a hook audit disclosure."
  (let* ((bounds (mevedel-view-disclosure-section-bounds))
         (source (and bounds
                      (get-text-property
                       (car bounds) 'mevedel-view-source)))
         (record (or (and bounds
                          (get-text-property
                           (car bounds) 'mevedel-view-hook-audit-record))
                     (and source
                          (buffer-live-p mevedel--data-buffer)
                          (car (mevedel-view--hook-audit-records-from-text
                                (mevedel-view-disclosure-data-substring
                                 mevedel--data-buffer
                                 (car source)
                                 (cdr source)))))))
         (collapsed (and bounds
                         (get-text-property
                          (car bounds) 'mevedel-view-collapsed)))
         (turn-id (and bounds
                       (get-text-property
                        (car bounds) 'mevedel-view-turn-id))))
    (unless bounds
      (user-error "No collapsible section at point"))
    (let ((inhibit-read-only t)
          (start (car bounds))
          (end (cdr bounds)))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (mevedel-view-disclosure-record-state-for-key
         (mevedel-view-audit--hook-audit-state-key record source)
         (not collapsed))
        (mevedel-view--insert-hook-audit-block record source)
        (when turn-id
          (put-text-property start (point)
                             'mevedel-view-turn-id turn-id))
        (mevedel-view-render-add-display-properties
         start (point) 'hook-audit)))))

(provide 'mevedel-view-audit)

;;; mevedel-view-audit.el ends here
