;;; mevedel-view-audit.el --- Transcript audit disclosures -*- lexical-binding: t -*-

;;; Commentary:

;; Renders parsed transcript audit attachments as collapsible view blocks.

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

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-direct-call
                  "mevedel-tool-render-data" (name data))
(declare-function mevedel-tool-render-data-for-tool
                  "mevedel-tool-render-data" (buffer tool-use-id))
(declare-function mevedel-tool-render-data-segment-bounds
                  "mevedel-tool-render-data" (tool-use-id))

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments "mevedel-transcript"
                  (start end))

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
(declare-function mevedel-session-artifacts-transcript-segments
                  "mevedel-session-artifacts" (session live-buffer))
(declare-function mevedel-session-artifacts-read-transcript-segment
                  "mevedel-session-artifacts" (session descriptor))
(autoload 'mevedel-session-artifacts-transcript-segments "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-transcript-segment "mevedel-session-artifacts")

;; `mevedel-view-agent'
(declare-function mevedel-view-open-agent-transcript
                  "mevedel-view-agent" (agent-path))
(autoload 'mevedel-view-open-agent-transcript "mevedel-view-agent")
(defvar mevedel-view--agent-path)
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-segments'
(declare-function mevedel-view-go-to-segment "mevedel-view-segments"
                  (&optional number))
(autoload 'mevedel-view-go-to-segment "mevedel-view-segments")

;; `mevedel-view-disclosure'
(declare-function mevedel-view-disclosure-data-substring
                  "mevedel-view-disclosure"
                  (data-buf start end &optional properties))
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
(declare-function mevedel-view-render-add-display-properties
                  "mevedel-view-render" (start end &optional default-vtype))
(autoload 'mevedel-view-render-add-display-properties "mevedel-view-render")
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(declare-function mevedel-view--tool-call-parse "mevedel-view-render"
                  (data-buf seg-start seg-end &optional raw))
(declare-function mevedel-view--execution-terminal-render-data
                  "mevedel-view-render" (data-buf tool-use-id &optional yielded-id))
(autoload 'mevedel-view--execution-terminal-render-data
  "mevedel-view-render")

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
                                  execution-archive execution-completion)))
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
  "Return TEXT indented for an expanded hook audit disclosure."
  (mapconcat (lambda (line) (concat "      " line))
             (split-string (or text "") "\n")
             "\n"))

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
           (format
            "    %d. %s\n" index
            (pcase (plist-get handler :source)
              ('plugin (if-let* ((name (plist-get handler :plugin-name)))
                           (format "%s plugin" name)
                         "plugin hook"))
              ('project-file "project hook")
              ('user-file "user hook")
              ('native "native hook")
              (_ "configured hook")))
           "       Handler: "
           (mevedel-view--hook-audit-value-text
            (or (plist-get handler :description)
                (plist-get handler :function)
                (plist-get handler :command)
                "unknown"))
           "\n"
           (when-let* ((reason (plist-get handler :reason)))
             (concat "       Reason: " reason "\n"))
           (when-let* ((contexts (plist-get handler :contexts)))
             (concat
              "       Context:\n"
              (mapconcat
               (lambda (context)
                 (mapconcat (lambda (line) (concat "         " line))
                            (split-string (format "%s" context) "\n") "\n"))
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
           (format "    %d. %s\n" index
                   (mevedel-reminders--entry-label (plist-get item :type)))
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
                   (concat
                    "    Rule: " (symbol-name (plist-get repair :rule)) "\n"
                    "    Path"
                    (if (= 1 (length (plist-get repair :paths))) ": " "s: ")
                    (mapconcat #'mevedel-tool-repair-format-path
                               (plist-get repair :paths) ", ")
                    "\n    Shape: "
                    (symbol-name (plist-get repair :before))
                    " -> " (symbol-name (plist-get repair :after)) "\n"))
                 (plist-get audit :repairs)
                 "")))
           "  ◇ tool input repair audit unavailable\n")
       (error "  ◇ tool input repair audit unavailable\n")))
    ('guest-prompt
     ;; Collaboration guest attribution renders as the turn heading
     ;; ("NAME (guest)" instead of "You"), so the record itself needs no
     ;; disclosure line.
     "")
    ('prompt-rewrite
     (concat
      "  \u25c7 hook changed prompt\n"
      (when expanded
        (concat
         "    Event: " (or (plist-get record :event) "UserPromptSubmit") "\n"
         (when-let* ((reason (plist-get record :reason)))
           (concat "    Reason: " reason "\n"))
         "    Original prompt:\n"
         (mevedel-view--indent-hook-audit-text
          (plist-get record :original))
         "\n"
         "    Submitted prompt:\n"
         (mevedel-view--indent-hook-audit-text
          (plist-get record :submitted))
         "\n"))))
    ('tool-permission
     (concat
      "  \u25c7 hook changed tool permission\n"
      (when expanded
        (concat
         "    Event: " (or (plist-get record :event) "PreToolUse") "\n"
         "    Outcome: " (or (plist-get record :outcome) "unknown") "\n"
         (when-let* ((reason (plist-get record :reason)))
           (concat "    Reason: " reason "\n"))))))
    ((or 'tool-context 'subagent-context 'compact-context)
     (mevedel-view--format-hook-context-audit record expanded))
    ('tool-input-rewrite
     (concat
      "  \u25c7 hook changed tool input\n"
      (when expanded
        (concat
         "    Event: " (or (plist-get record :event) "PreToolUse") "\n"
         (when-let* ((reason (plist-get record :reason)))
           (concat "    Reason: " reason "\n"))
         "    Original input:\n"
         (mevedel-view--indent-hook-audit-text
          (mevedel-view--hook-audit-value-text
           (plist-get record :original-input)))
         "\n"
         "    Updated input:\n"
         (mevedel-view--indent-hook-audit-text
          (mevedel-view--hook-audit-value-text
           (plist-get record :updated-input)))
         "\n"))))
    ('tool-result-rewrite
     (concat
      "  \u25c7 hook changed tool result\n"
      (when expanded
        (concat
         "    Event: " (or (plist-get record :event) "PostToolUse") "\n"
         (when-let* ((reason (plist-get record :reason)))
           (concat "    Reason: " reason "\n"))
         "    Original result:\n"
         (mevedel-view--indent-hook-audit-text
          (plist-get record :original-result))
         "\n"
         "    Updated result:\n"
         (mevedel-view--indent-hook-audit-text
          (plist-get record :updated-result))
         "\n"))))
    ('injected-reminders
     (mevedel-view--format-injected-reminders-audit record expanded))
    (_
     (concat
      "  \u25c7 hook audit\n"
      (when expanded
        (format "    %S\n" record))))))

(defun mevedel-view--insert-hook-audit-block
    (record &optional source expanded)
  "Insert hook audit disclosure for RECORD.
SOURCE, when non-nil, is the source range in the data buffer.
EXPANDED means insert the disclosure body expanded."
  (when (and (listp record)
             (keywordp (car-safe record)))
    (let ((start (point))
          (source (and (consp source) (cons (car source) (cdr source)))))
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
           mevedel-view-source-key
           ,(append
             (mevedel-view-disclosure-state-key source 'hook-audit)
             (list (mevedel-view--hook-audit-key record)))))))))

(defun mevedel-view-audit--breadcrumb-label (record)
  "Return the compact terminal label for execution breadcrumb RECORD."
  (let* ((facts (plist-get record :facts))
         (outcome (plist-get facts :outcome))
         (termination (plist-get facts :termination))
         (failed (not (memq outcome '(success no-match different false))))
         (status (cond ((memq termination '(stopped interrupted signaled cancelled
                                           owner-stopped))
                        "Stopped")
                       (failed "Failed")
                       (t "Finished")))
         (code (plist-get facts :exit-code))
         (command (or (plist-get record :command)
                      (plist-get facts :command) "Bash"))
         (command (replace-regexp-in-string "[\n\r\t]+" " " command))
         (owner (plist-get record :owner))
         (sender (and (stringp owner)
                      (not (equal owner (or mevedel-view--agent-path "/root")))
                      (replace-regexp-in-string "[\n\r\t]+" " " owner))))
    (format "  ↳ %s: %s%s%s  " status
            (truncate-string-to-width command 64 nil nil "…")
            (if (and failed (integerp code)) (format " · exit %d" code) "")
            (if sender
                (format " · %s" (truncate-string-to-width sender 48 nil nil "…"))
              ""))))

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

(defun mevedel-view-audit--mailbox-completion (text)
  "Return the execution completion delivered by the mailbox block TEXT."
  (when-let* ((start (and (string-match "<agent-message\\([^>]*\\)>" text)
                           (match-end 0)))
              (attributes (match-string 1 text))
              ((string-match-p "type=\"EXECUTION\"" attributes))
              (sender (and (string-match "sender=\"\\([^\"]+\\)\""
                                         attributes)
                           (match-string 1 attributes)))
              (finish (string-match "</agent-message>" text start))
              (body (substring text start finish))
              (record (mevedel-view-audit-mailbox-breadcrumb body sender)))
    (plist-put record :delivery-body body)))

(defun mevedel-view-audit--stashed-breadcrumb-p (stash record)
  "Return non-nil if STASH contains RECORD's execution breadcrumb."
  (let ((pos 0) (limit (length stash)) found)
    (while (and (< pos limit) (not found))
      (let ((prior (get-text-property
                    pos 'mevedel-view-execution-breadcrumb stash)))
        (setq found (and prior
                         (equal (plist-get prior :execution-id)
                                (plist-get record :execution-id))
                         (equal (plist-get prior :owner)
                                (plist-get record :owner)))
              pos (or (next-single-property-change
                       pos 'mevedel-view-execution-breadcrumb stash limit)
                      limit))))
    found))

(defun mevedel-view-audit-breadcrumb-present-p (record before)
  "Return non-nil if RECORD was already projected before BEFORE.
Identity is local to this receiving transcript, not to the global
execution, so parent and child may each display their own breadcrumb."
  (let ((pos (point-min)) found)
    (while (and (< pos before) (not found))
      (let ((prior (get-text-property pos 'mevedel-view-execution-breadcrumb)))
        (setq found (and prior
                         (equal (plist-get prior :execution-id)
                                (plist-get record :execution-id))
                         (equal (plist-get prior :owner)
                                (plist-get record :owner))))
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-execution-breadcrumb nil before)
                      before))))
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
                               session data))
                 (viewed-number
                  (or (mevedel-view-segments-current-number)
                      (plist-get (cl-find-if
                                  (lambda (entry) (plist-get entry :current-p))
                                  descriptors)
                                 :number)
                      ;; Agent archives have numbers; the live agent transcript
                      ;; is their successor but has no segment number of its own.
                      most-positive-fixnum)))
            (catch 'found
              (dolist (descriptor descriptors)
                (when (and (eq (plist-get descriptor :status) 'readable)
                           (not (plist-get descriptor :current-p))
                           (integerp (plist-get descriptor :number))
                           (< (plist-get descriptor :number) viewed-number))
                  (when-let* ((older (condition-case nil
                                        (mevedel-session-artifacts-read-transcript-segment
                                         session descriptor)
                                      (error nil))))
                    (unwind-protect
                        (with-current-buffer older
                          (save-restriction
                            (widen)
                            (let* ((audits (mevedel-transcript-audit-records
                                            (buffer-substring (point-min)
                                                              (point-max))
                                            'execution-breadcrumb))
                                   (matches
                                    (lambda (other)
                                      (and (equal (plist-get other :execution-id)
                                                  (plist-get record :execution-id))
                                           (equal (plist-get other :owner)
                                                  (plist-get record :owner))))))
                              (when (or (cl-some matches audits)
                                        (cl-some
                                         (lambda (segment)
                                           (and (eq (car segment) 'mailbox)
                                                (funcall
                                                 matches
                                                 (mevedel-view-audit--mailbox-completion
                                                  (buffer-substring-no-properties
                                                   (cadr segment)
                                                   (caddr segment))))))
                                         (mevedel-transcript-segments
                                          (point-min) (point-max))))
                                (throw 'found t)))))
                      (kill-buffer older)))))))))))

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
                             (mevedel-tool-render-data-for-tool
                              (mevedel-view-segments-display-buffer)
                              parent-id)))
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
                                             session data)))
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

(defun mevedel-view-audit--select-source (record)
  "Select the archived segment owning RECORD's tool row, if any.
The completion's source target may be a later segment after compaction."
  (when-let* ((tool-id (plist-get record :tool-use-id))
              ((buffer-live-p mevedel--data-buffer))
              (session (buffer-local-value 'mevedel--session mevedel--data-buffer)))
    (let ((parent-id tool-id)
          (descriptors (reverse (mevedel-session-artifacts-transcript-segments
                                 session mevedel--data-buffer)))
          chosen)
      (while (string-match "/[0-9]+\\'" parent-id)
        (setq parent-id (substring parent-id 0 (match-beginning 0))))
      (dolist (descriptor descriptors)
        (when (and (not chosen) (eq (plist-get descriptor :status) 'readable))
          (let* ((current-p (plist-get descriptor :current-p))
                 (buffer (if current-p mevedel--data-buffer
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
      ;; Agent compaction archives are read for fallback evidence, not root
      ;; segments in the parent session's segment switcher.
      (when (and chosen (not (plist-get chosen :logical)))
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
         (buffer (get-buffer-create "*mevedel execution result*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or (plist-get record :command) (plist-get facts :command) "Bash")
                "\n\n")
        (cond
         ((and (stringp physical) (file-regular-p physical)
               (file-readable-p physical))
          (when (zerop (cadr (insert-file-contents physical)))
            (insert "Execution produced no output.\n")))
         ((stringp output)
          (insert (if (string-empty-p output)
                      "Execution produced no output.\n"
                    output)))
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

(defun mevedel-view-audit-show-control-result (execution-id)
  "Open EXECUTION-ID's original Bash row from an input or stop interaction.
Use source-backed tool identities, including ToolCall children.  If the
original row has been removed, show retained evidence or an explicit absence."
  (interactive)
  (let* ((data (mevedel-view-segments-display-buffer))
         (tool-id
          (and (buffer-live-p data)
               (with-current-buffer data
                 (save-restriction
                   (widen)
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
                       found))))))
         (record (or (and (buffer-live-p data)
                          (with-current-buffer data
                            (let* ((audits (mevedel-transcript-audit-records
                                            (buffer-substring (point-min)
                                                              (point-max))))
                                   (breadcrumb
                                    (cl-find-if
                                     (lambda (candidate)
                                       (and (eq (plist-get candidate :type)
                                                'execution-breadcrumb)
                                            (equal (plist-get candidate
                                                              :execution-id)
                                                   execution-id)))
                                     audits))
                                   (archived
                                    (unless breadcrumb
                                      (cl-find-if
                                       (lambda (candidate)
                                         (and (memq (plist-get candidate :type)
                                                    '(execution-archive
                                                      execution-completion))
                                              (equal (plist-get
                                                      (plist-get candidate
                                                                 :render-data)
                                                      :execution-id)
                                                     execution-id)))
                                       (reverse audits)))))
                              (or breadcrumb
                                  (and archived
                                       (list :execution-id execution-id
                                             :tool-use-id
                                             (plist-get archived :tool-use-id)
                                             :facts (plist-get archived
                                                               :render-data)))))))
                     (list :execution-id execution-id :tool-use-id tool-id))))
    (when tool-id
      (setq record (plist-put (copy-sequence record) :tool-use-id tool-id)))
    (mevedel-view-audit-show-result record)))

(defun mevedel-view-audit-mailbox-breadcrumb (text sender)
  "Build a forwarded completion from EXECUTION mailbox TEXT and SENDER.
The mailbox remains authoritative model input; its view projects the
terminal facts and retained bounded payload without a second output card."
  (when (string-match "<bash-execution[^<>]*/>[[:space:]]*\\'" text)
    (condition-case nil
        (let ((xml (match-string 0 text))
              (output (string-trim-right
                       (substring text 0 (match-beginning 0)))))
          (with-temp-buffer
          (insert xml)
          (let* ((attributes (cadr (car (xml-parse-region (point-min) (point-max)))))
                 (id (alist-get 'execution_id attributes))
                 (code (alist-get 'exit_code attributes))
                 (termination (alist-get 'termination attributes))
                 (outcome (alist-get 'outcome attributes))
                 (command (alist-get 'command attributes))
                 (path (alist-get 'output_path attributes)))
            (when id
              (list :type 'execution-breadcrumb :execution-id id :owner sender
                    :command (or command "Bash")
                    :facts (list :outcome (and outcome (intern outcome))
                                 :termination (and termination (intern termination))
                                 :exit-code (and code (string-to-number code))
                                 :output-path path
                                 :execution-output output))))))
      (error nil))))

(defun mevedel-view-audit--history-records (execution-id)
  "Return retained tool interactions concerning EXECUTION-ID in segment order."
  (let* ((live (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
         (display (mevedel-view-segments-display-buffer))
         (session (and (buffer-live-p live)
                       (buffer-local-value 'mevedel--session live)))
         records)
    (cl-labels
        ((collect-buffer (data)
           (with-current-buffer data
             (save-restriction
               (widen)
               (cl-labels ((collect (call)
                             (when (or (equal execution-id
                                              (plist-get (plist-get call :args)
                                                         :execution_id))
                                       (equal execution-id
                                              (plist-get (plist-get call :render-data)
                                                         :execution-id)))
                               (push call records))
                             (dolist (child (plist-get (plist-get call :render-data)
                                                       :calls))
                               (collect (list :name (plist-get child :tool)
                                              :args (plist-get child :args)
                                              :result (plist-get child :result)
                                              :render-data (plist-get child :render-data))))))
                 (dolist (segment (mevedel-transcript-segments (point-min) (point-max)))
                   (pcase (car segment)
                     ('tool
                      (when-let* ((call (mevedel-view--tool-call-parse
                                         data (cadr segment) (caddr segment))))
                        (let ((retained (and (plist-get call :tool-use-id)
                                             (mevedel-tool-render-data-for-tool
                                              data (plist-get call :tool-use-id)))))
                          (collect (if retained
                                       (plist-put (copy-sequence call)
                                                  :render-data retained)
                                     call)))))
                     ('mailbox
                      (let* ((text (buffer-substring-no-properties
                                    (cadr segment) (caddr segment)))
                             (start (and (string-match "<agent-message\\([^>]*\\)>"
                                                       text)
                                         (match-end 0)))
                             (attributes (and start (match-string 1 text)))
                             (sender (and attributes
                                          (string-match "sender=\"\\([^\"]+\\)\""
                                                        attributes)
                                          (match-string 1 attributes)))
                             (finish (and start
                                          (string-match "</agent-message>" text start)))
                             (body (and finish (substring text start finish)))
                             (record (and body sender
                                          (string-match-p "type=\"EXECUTION\""
                                                          attributes)
                                          (mevedel-view-audit-mailbox-breadcrumb
                                           body sender))))
                        (when (equal (plist-get record :execution-id) execution-id)
                          (push (list :name "Execution delivery"
                                      :args (list :execution_id execution-id
                                                  :sender sender)
                                      :result body)
                                records)))))))))))
      (if (and session (buffer-live-p live))
          (dolist (descriptor (mevedel-session-artifacts-transcript-segments
                               session live))
            (when (eq (plist-get descriptor :status) 'readable)
              (if (plist-get descriptor :current-p)
                  (collect-buffer live)
                (when-let* ((older
                             (condition-case nil
                                 (mevedel-session-artifacts-read-transcript-segment
                                  session descriptor)
                               (error nil))))
                  (unwind-protect
                      (collect-buffer older)
                    (kill-buffer older))))))
        (when (buffer-live-p display)
          (collect-buffer display))))
    (nreverse records)))

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
              (insert (format "%s  %S\n" (plist-get call :name)
                              (plist-get call :args)))
              (unless (string-empty-p (or (plist-get call :result) ""))
                (insert (plist-get call :result) "\n"))
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
             "    [Execution history]\n"
             'font-lock-face 'link
             'mouse-face 'highlight
             'help-echo "RET: inspect retained execution interactions"
             'mevedel-view-zone-activate
             (lambda () (mevedel-view-audit-show-history id))))))

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
        (mevedel-view--insert-hook-audit-block record source collapsed)
        (mevedel-view-disclosure-record-state
         source 'hook-audit (not collapsed))
        (when turn-id
          (put-text-property start (point)
                             'mevedel-view-turn-id turn-id))
        (mevedel-view-render-add-display-properties
         start (point) 'hook-audit)))))

(provide 'mevedel-view-audit)

;;; mevedel-view-audit.el ends here
