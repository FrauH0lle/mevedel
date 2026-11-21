;;; niwa-context-delivery.el --- Retained current-context observations -*- lexical-binding: t -*-

;;; Commentary:

;; Deliver complete current facts after current input, through the reminder
;; transaction.  Repository guidance and Goal procedures are retained separately.
;; The last trusted delivery is the acknowledgement: restore, rewind, and
;; compaction need no independent persisted state.

;;; Code:

(require 'cl-lib)
(require 'niwa-system)
(require 'niwa-transcript-audit)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `niwa-agents'
(declare-function niwa-agent-context-components "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-agent "niwa-agents" (cl-x) t)
(defvar niwa--agent-invocation)

;; `niwa-reminders'
(declare-function niwa-reminders-stage-entry "niwa-reminders"
                  (fsm type body &optional commit))

;; `niwa-structs'
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace-instruction-hashes "niwa-structs" (cl-x) t)
(defvar niwa--session)

;; `niwa-tool-fs-read'
(declare-function niwa-tool-fs-read-workspace-context "niwa-tool-fs-read" (scope owner))
(autoload 'niwa-tool-fs-read-workspace-context "niwa-tool-fs-read")

;; `niwa-tools'
(defvar niwa-tools--current-engine)

;; `niwa-utilities'
(defvar niwa--hook-audit-close)
(defvar niwa--hook-audit-open)

(defvar-local niwa-context-delivery--cache nil
  "Derived transcript deliveries: (SCANNED-END . MESSAGE-RECORD-ALIST).
Records are newest first.")

(defvar-local niwa-context-delivery--edit-tick nil
  "Last character modification tick observed through normal edit hooks.")

(defun niwa-context-delivery--record-edit (&rest _)
  "Record a normal edit so hook-inhibited replacements can be detected."
  (setq niwa-context-delivery--edit-tick (buffer-chars-modified-tick)))

(defun niwa-context-delivery--invalidate (begin _end)
  "Discard derived observations when an edit at BEGIN touches scanned history."
  (when (and niwa-context-delivery--cache
             (or (not (equal niwa-context-delivery--edit-tick
                             (buffer-chars-modified-tick)))
                 (< begin (marker-position (car niwa-context-delivery--cache)))))
    (set-marker (car niwa-context-delivery--cache) nil)
    (setq niwa-context-delivery--cache nil)))

(defun niwa-context-delivery--records ()
  "Return this buffer's trusted injection records as (TEXT . RECORD), newest first.
TEXT is the record's realized user message.  Scan appended history
incrementally.  Editing scanned history invalidates the cache; cold restore
and compaction reconstruct solely from retained records."
  (add-hook 'before-change-functions #'niwa-context-delivery--invalidate nil t)
  (add-hook 'after-change-functions #'niwa-context-delivery--record-edit nil t)
  (unless (equal niwa-context-delivery--edit-tick (buffer-chars-modified-tick))
    (when niwa-context-delivery--cache
      (set-marker (car niwa-context-delivery--cache) nil))
    (setq niwa-context-delivery--cache nil)
    (niwa-context-delivery--record-edit))
  (unless (and niwa-context-delivery--cache
               (marker-position (car niwa-context-delivery--cache))
               (<= (point-min) (car niwa-context-delivery--cache) (point-max)))
    (setq niwa-context-delivery--cache (list (copy-marker (point-min)))))
  (save-excursion
    (goto-char (car niwa-context-delivery--cache))
    (while (search-forward niwa--hook-audit-open nil t)
      (let ((start (match-beginning 0)) (payload (point)))
        (when (and (niwa-transcript-audit-trusted-range-p start payload)
                   (search-forward niwa--hook-audit-close nil t))
          (when-let* (((niwa-transcript-audit-trusted-range-p start (point)))
                      (record (niwa--read-hook-audit-record
                               (buffer-substring-no-properties payload (match-beginning 0))))
                      ((eq (plist-get record :type) 'injected-reminders)))
            (let* ((items (plist-get record :items))
                   (text (mapconcat
                          (lambda (item)
                            (format "<system-reminder>\n%s\n</system-reminder>"
                                    (plist-get item :body)))
                          items "\n")))
              (push (cons text record) (cdr niwa-context-delivery--cache)))))))
    (set-marker (car niwa-context-delivery--cache) (point-max)))
  (cdr niwa-context-delivery--cache))

(defun niwa-context-delivery--previous (types &optional data)
  "Return latest trusted delivered bodies for TYPES in this buffer.
When DATA is supplied, acknowledge only complete user messages in that payload."
  (let* ((records (niwa-context-delivery--records))
         (key (cl-find-if (lambda (key) (plist-member data key))
                          '(:messages :input :contents)))
         (messages (if data
                       (mapcar #'niwa-context-delivery--message-text
                               (append (plist-get data key) nil))
                     (mapcar #'car (reverse records))))
         (index (make-hash-table :test #'equal))
         result)
    (dolist (record (reverse records))
      (puthash (car record) (plist-get (cdr record) :items) index))
    (dolist (text messages)
      (dolist (item (gethash text index))
        (when (memq (plist-get item :type) types)
          (setf (alist-get (plist-get item :type) result) (plist-get item :body)))))
    result))

(defun niwa-context-delivery-native (id)
  "Return latest trusted delivered bodies for native conversation ID.
Derive the alist solely from retained receipt records, in transcript order.
Records of other conversations, such as isolated turns, never count.
A nil ID starts without acknowledged observations."
  (when id
    (let (latest)
      (dolist (entry (reverse (niwa-context-delivery--records)))
        (let ((record (cdr entry)))
          (when (equal id (plist-get record :native))
            (dolist (item (plist-get record :items))
              (setf (alist-get (plist-get item :type) latest) (plist-get item :body))))))
      latest)))

(defun niwa-context-delivery--message-text (message)
  "Return MESSAGE's plain user text, excluding tool results and other roles."
  (when (equal (plist-get message :role) "user")
    (let ((content (or (plist-get message :content) (plist-get message :parts))))
      (cond
       ((stringp content) content)
       ((and (vectorp content)
             (cl-every (lambda (part)
                         (and (stringp (plist-get part :text))
                              (member (plist-get part :type) '(nil "text" "input_text"))))
                       content))
        (mapconcat (lambda (part) (plist-get part :text)) content ""))))))

(defun niwa-context-delivery--observations (names context)
  "Render selected NAMES in CONTEXT as independently retained observations.
Each section replaces only its earlier state; omitted sections stay applicable."
  (mapcar
   (lambda (name)
     (let* ((value (niwa-system--render-component
                    (alist-get name niwa-system--prompt-components) context))
            (body (if (and value (not (string-blank-p value)))
                      value "None currently available or active."))
            (title (capitalize (replace-regexp-in-string "-" " " (symbol-name name)))))
       (cons (intern (format "context-%s" name))
             (if (memq name '(workspace-config goal-policy))
                 (format "Current %s context (supersedes earlier %s observations):\n%s"
                         name name body)
               (format "## %s\nThis updates only %s; all other previously supplied state remains applicable. This section replaces earlier %s state and remains valid until updated.\n\n%s"
                       title title title
                       (replace-regexp-in-string "\\`## [^\n]+\n+" "" body))))))
   (cl-remove-if-not
    (lambda (name) (memq name names))
    '(workspace-config goal-policy environment active-goal tools skills memory journal resources))))

(defun niwa-context-delivery-stage (fsm)
  "Stage changed context sections for FSM as retained messages.
Use the recipient's selected components; never give a worker the root Goal."
  (when-let* ((buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when niwa--session
        (let* ((niwa-tools--current-engine fsm)
               (invocation (bound-and-true-p niwa--agent-invocation))
               (names (if invocation
                          (niwa-agent-context-components
                           (niwa-agent-invocation-agent invocation))
                        (niwa-system-context-components niwa--session)))
               (context (niwa-system--make-context
                         (niwa-session-workspace niwa--session)
                         (niwa-conversation-working-directory niwa--session)
                         niwa--session buffer))
               (observations (niwa-context-delivery--observations names context))
               (types (mapcar #'car observations))
               (previous (niwa-context-delivery--previous
                          types (plist-get (gptel-fsm-info fsm) :data))))
          ;; A retry of an undelivered WAIT replaces its staged observations.
          (let ((info (gptel-fsm-info fsm)))
            (when (plist-member info :niwa-reminder-entries)
              (plist-put info :niwa-reminder-entries
                         (cl-remove-if
                          (lambda (entry) (memq (plist-get entry :type) types))
                          (plist-get info :niwa-reminder-entries)))))
          (dolist (observation observations)
            (unless (equal (cdr observation) (alist-get (car observation) previous))
              (niwa-reminders-stage-entry
               fsm (car observation) (cdr observation))))
          ;; Engine transfer invalidates delivery, but keeps known paths so
          ;; startup failure/reopen cannot silently discard nested guidance.
          (when (and (not invocation)
                     (cl-some (lambda (entry)
                                (and (equal "/root" (caar entry)) (null (cdr entry))))
                              (niwa-session-workspace-instruction-hashes niwa--session)))
            (let ((instructions (niwa-tool-fs-read-workspace-context niwa--session "/root")))
              (cl-mapc (lambda (entry commit)
                         (niwa-reminders-stage-entry
                          fsm (plist-get entry :type) (plist-get entry :body) commit))
                       (plist-get instructions :entries) (plist-get instructions :commits)))))))))

(provide 'niwa-context-delivery)
;;; niwa-context-delivery.el ends here
