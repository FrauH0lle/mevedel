;;; mevedel-view-audit.el --- Transcript audit disclosures -*- lexical-binding: t -*-

;;; Commentary:

;; Renders parsed transcript audit attachments as collapsible view blocks.

;;; Code:

;; `cl-extra'
(declare-function cl-some "cl-extra" (predicate sequence &rest more-sequences))

;; `mevedel-hooks'
(declare-function mevedel-hooks-decision-reason "mevedel-hooks" (decision))

;; `mevedel-structs'
(defvar mevedel--data-buffer)

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-format-path
                  "mevedel-tool-repair" (path))

;; `mevedel-tool-repair-diagnostics'
(declare-function mevedel-tool-repair-normalize-audit-record
                  "mevedel-tool-repair-diagnostics" (record))

;; `mevedel-transcript-audit'
(declare-function mevedel--hook-prompt-rewrite-audit-record
                  "mevedel-transcript-audit"
                  (event original submitted &optional reason))
(declare-function mevedel-transcript-audit-spans
                  "mevedel-transcript-audit" (text &optional type))

;; `mevedel-view-disclosure'
(declare-function mevedel-view-disclosure-data-substring
                  "mevedel-view-disclosure"
                  (data-buf start end &optional properties))
(declare-function mevedel-view-disclosure-record-state
                  "mevedel-view-disclosure" (source vtype collapsed))
(declare-function mevedel-view-disclosure-section-bounds
                  "mevedel-view-disclosure" ())
(declare-function mevedel-view-disclosure-source-range
                  "mevedel-view-disclosure" (data-buffer start end))
(declare-function mevedel-view-disclosure-state-key
                  "mevedel-view-disclosure"
                  (source vtype &optional previous-key))

;; `mevedel-view-render'
(declare-function mevedel-view-render-add-display-properties
                  "mevedel-view-render" (start end &optional default-vtype))

(defun mevedel-view--hook-audit-records-from-text
    (text &optional type data-buf source-start)
  "Return hook audit records parsed from TEXT.
When TYPE is non-nil, return only records with matching `:type'.
DATA-BUF and SOURCE-START attach each record's exact source range."
  (require 'mevedel-transcript-audit)
  (require 'mevedel-view-disclosure)
  (delq nil
        (mapcar
         (lambda (span)
           (let ((record (plist-get span :record)))
             (unless (and (null type)
                          (eq (plist-get record :type) 'fork-point))
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
  "Return hook audit records found in user SEGMENTS from DATA-BUF."
  (require 'mevedel-transcript-audit)
  (with-current-buffer data-buf
    (let (records)
      (dolist (seg segments)
        (when (memq (car seg)
                    '(user hook-context prompt render-data ignored))
          (let ((start (cadr seg)))
            (dolist (span
                     (mevedel-transcript-audit-spans
                      (buffer-substring start (caddr seg))))
              (push (append
                     (plist-get span :record)
                     (list :source
                           (mevedel-view-disclosure-source-range
                            data-buf
                            (+ start (plist-get span :start))
                            (+ start (plist-get span :end)))))
                    records)))))
      (nreverse records))))

(defun mevedel-view--format-hook-audit-block (record expanded)
  "Return display text for hook audit RECORD.
When EXPANDED is non-nil, include record details."
  (pcase (plist-get record :type)
    ('tool-input-repair
     (condition-case nil
         (progn
           (require 'mevedel-tool-repair)
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
             "  ◇ tool input repair audit unavailable\n"))
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
         (mapconcat (lambda (line) (concat "      " line))
                    (split-string (or (plist-get record :original) "") "\n")
                    "\n")
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
    ('tool-context
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
    ('subagent-context
     (mevedel-view--format-hook-context-audit record expanded))
    ('compact-context
     (mevedel-view--format-hook-context-audit record expanded))
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
           (list (mevedel-view--hook-audit-key record))))))))

(defun mevedel-view-audit-toggle-hook-audit ()
  "Toggle a hook audit disclosure."
  (require 'mevedel-view-disclosure)
  (require 'mevedel-view-render)
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
