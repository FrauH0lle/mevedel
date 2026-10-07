;;; mevedel-context-delivery.el --- Retained current-context observations -*- lexical-binding: t -*-

;;; Commentary:

;; Deliver complete current facts after current input, through the reminder
;; transaction.  Repository guidance and Goal procedures are retained separately.
;; The last trusted delivery is the acknowledgement: restore, rewind, and
;; compaction need no independent persisted state.

;;; Code:

(require 'cl-lib)
(require 'mevedel-system)
(require 'mevedel-transcript-audit)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-agents'
(declare-function mevedel-agent-context-components "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-reminders'
(declare-function mevedel-reminders-stage-entry "mevedel-reminders"
                  (fsm type body &optional commit))

;; `mevedel-structs'
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace-instruction-hashes "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-tool-fs-read'
(declare-function mevedel-tool-fs-read-workspace-context "mevedel-tool-fs-read" (scope owner))
(autoload 'mevedel-tool-fs-read-workspace-context "mevedel-tool-fs-read")

;; `mevedel-utilities'
(defvar mevedel--hook-audit-close)
(defvar mevedel--hook-audit-open)

(defvar-local mevedel-context-delivery--cache nil
  "Derived transcript observations: (SCANNED-END . MESSAGE-RECORD-ALIST).")

(defvar-local mevedel-context-delivery--edit-tick nil
  "Last character modification tick observed through normal edit hooks.")

(defun mevedel-context-delivery--record-edit (&rest _)
  "Record a normal edit so hook-inhibited replacements can be detected."
  (setq mevedel-context-delivery--edit-tick (buffer-chars-modified-tick)))

(defun mevedel-context-delivery--invalidate (begin _end)
  "Discard derived observations when an edit at BEGIN touches scanned history."
  (when (and mevedel-context-delivery--cache
             (or (not (equal mevedel-context-delivery--edit-tick
                             (buffer-chars-modified-tick)))
                 (< begin (marker-position (car mevedel-context-delivery--cache)))))
    (set-marker (car mevedel-context-delivery--cache) nil)
    (setq mevedel-context-delivery--cache nil)))

(defun mevedel-context-delivery--previous (types &optional data)
  "Return latest trusted delivered bodies for TYPES in this buffer.
When DATA is supplied, acknowledge only complete user messages in that payload.
Scan appended history incrementally.  Editing scanned history invalidates the
cache; cold restore and compaction reconstruct solely from retained records."
  (add-hook 'before-change-functions #'mevedel-context-delivery--invalidate nil t)
  (add-hook 'after-change-functions #'mevedel-context-delivery--record-edit nil t)
  (unless (equal mevedel-context-delivery--edit-tick (buffer-chars-modified-tick))
    (when mevedel-context-delivery--cache
      (set-marker (car mevedel-context-delivery--cache) nil))
    (setq mevedel-context-delivery--cache nil)
    (mevedel-context-delivery--record-edit))
  (unless (and mevedel-context-delivery--cache
               (marker-position (car mevedel-context-delivery--cache))
               (<= (point-min) (car mevedel-context-delivery--cache) (point-max)))
    (setq mevedel-context-delivery--cache (list (copy-marker (point-min)))))
  (save-excursion
    (goto-char (car mevedel-context-delivery--cache))
    (while (search-forward mevedel--hook-audit-open nil t)
      (let ((start (match-beginning 0)) (payload (point)))
        (when (and (mevedel-transcript-audit-trusted-range-p start payload)
                   (search-forward mevedel--hook-audit-close nil t))
          (when-let* (((mevedel-transcript-audit-trusted-range-p start (point)))
                      (record (mevedel--read-hook-audit-record
                               (buffer-substring-no-properties payload (match-beginning 0))))
                      ((eq (plist-get record :type) 'injected-reminders)))
            (let* ((items (plist-get record :items))
                   (text (mapconcat
                          (lambda (item)
                            (format "<system-reminder>\n%s\n</system-reminder>"
                                    (plist-get item :body)))
                          items "\n")))
              (push (cons text items) (cdr mevedel-context-delivery--cache)))))))
    (set-marker (car mevedel-context-delivery--cache) (point-max)))
  (let* ((records (cdr mevedel-context-delivery--cache))
         (key (cl-find-if (lambda (key) (plist-member data key))
                          '(:messages :input :contents)))
         (messages (if data
                       (mapcar #'mevedel-context-delivery--message-text
                               (append (plist-get data key) nil))
                     (mapcar #'car (reverse records))))
         (index (make-hash-table :test #'equal))
         result)
    (dolist (record (reverse records))
      (puthash (car record) (cdr record) index))
    (dolist (text messages)
      (dolist (item (gethash text index))
        (when (memq (plist-get item :type) types)
          (setf (alist-get (plist-get item :type) result) (plist-get item :body)))))
    result))

(defun mevedel-context-delivery--message-text (message)
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

(defun mevedel-context-delivery--observations (names context)
  "Render selected NAMES in CONTEXT as independently retained observations.
Each section replaces only its earlier state; omitted sections stay applicable."
  (mapcar
   (lambda (name)
     (let* ((value (mevedel-system--render-component
                    (alist-get name mevedel-system--prompt-components) context))
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
    '(workspace-config goal-policy environment active-goal skills memory journal resources))))

(defun mevedel-context-delivery-stage (fsm)
  "Stage changed context sections for FSM as retained messages.
Use the recipient's selected components; never give a worker the root Goal."
  (when-let* ((buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when mevedel--session
        (let* ((invocation (bound-and-true-p mevedel--agent-invocation))
               (names (if invocation
                          (mevedel-agent-context-components
                           (mevedel-agent-invocation-agent invocation))
                        mevedel-system-retained-components))
               (context (mevedel-system--make-context
                         (mevedel-session-workspace mevedel--session)
                         (mevedel-session-working-directory mevedel--session)
                         mevedel--session buffer))
               (observations (mevedel-context-delivery--observations names context))
               (types (mapcar #'car observations))
               (previous (mevedel-context-delivery--previous
                          types (plist-get (gptel-fsm-info fsm) :data))))
          ;; A retry of an undelivered WAIT replaces its staged observations.
          (let ((info (gptel-fsm-info fsm)))
            (when (plist-member info :mevedel-reminder-entries)
              (plist-put info :mevedel-reminder-entries
                         (cl-remove-if
                          (lambda (entry) (memq (plist-get entry :type) types))
                          (plist-get info :mevedel-reminder-entries)))))
          (dolist (observation observations)
            (unless (equal (cdr observation) (alist-get (car observation) previous))
              (mevedel-reminders-stage-entry
               fsm (car observation) (cdr observation))))
          ;; Engine transfer invalidates delivery, but keeps known paths so
          ;; startup failure/reopen cannot silently discard nested guidance.
          (when (and (not invocation)
                     ;; Fresh directives own request-local instruction hashes.
                     (not (bound-and-true-p mevedel--current-directive-uuid))
                     (cl-some (lambda (entry)
                                (and (equal "/root" (caar entry)) (null (cdr entry))))
                              (mevedel-session-workspace-instruction-hashes mevedel--session)))
            (let ((instructions (mevedel-tool-fs-read-workspace-context mevedel--session "/root")))
              (cl-mapc (lambda (entry commit)
                         (mevedel-reminders-stage-entry
                          fsm (plist-get entry :type) (plist-get entry :body) commit))
                       (plist-get instructions :entries) (plist-get instructions :commits)))))))))

(provide 'mevedel-context-delivery)
;;; mevedel-context-delivery.el ends here
