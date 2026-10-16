;;; mevedel-view-execution.el --- Inline background execution inspection -*- lexical-binding: t -*-

;;; Commentary:

;; The status zone projects public user-authority execution snapshots.  One
;; buffer-local selection retains bounded output after completion; it is not a
;; process registry.  Passive events refresh that selection without consuming
;; output, acknowledging mail, polling, or redrawing the transcript.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-structs)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(autoload 'mevedel-agent-invocation-parent-data-buffer "mevedel-agents")

;; `mevedel-execution'
(declare-function mevedel-execution-count-user "mevedel-execution" (session))
(declare-function mevedel-execution-list-user "mevedel-execution" (session))
(declare-function mevedel-execution-stop-user "mevedel-execution" (session execution-id))
(autoload 'mevedel-execution-count-user "mevedel-execution")
(autoload 'mevedel-execution-list-user "mevedel-execution")
(autoload 'mevedel-execution-stop-user "mevedel-execution")
(defvar mevedel-execution-event-functions)
(defvar mevedel-execution-state-change-hook)

;; `mevedel-executions-list'
(declare-function mevedel-executions-list-open "mevedel-executions-list" (&optional context))
(autoload 'mevedel-executions-list-open "mevedel-executions-list" nil t)

;; `mevedel-resource'
(declare-function mevedel-resource-visit-path "mevedel-resource" (address &optional context))
(autoload 'mevedel-resource-visit-path "mevedel-resource")

;; `mevedel-view'
(declare-function mevedel-view--display-fragment-keymap "mevedel-view" (&rest maps))
(declare-function mevedel-view--render-status "mevedel-view" (&optional data-buf))
(declare-function mevedel-view--status-session "mevedel-view" (&optional data-buf))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-agent'
(declare-function mevedel-view-open-agent-transcript "mevedel-view-agent" (agent-path))
(autoload 'mevedel-view-open-agent-transcript "mevedel-view-agent" nil t)

;; `mevedel-view-audit'
(declare-function mevedel-view-audit-show-result "mevedel-view-audit" (record))
(autoload 'mevedel-view-audit-show-result "mevedel-view-audit" nil t)

(defvar-local mevedel-view-execution--selected nil
  "Selected public snapshot, with its :session and presentation-only flags.
Only this item can retain terminal facts and bounded :whole-output.")

(defun mevedel-view-execution--same-p (left right)
  "Whether LEFT and RIGHT identify the same owner's execution."
  (and left right
       (equal (plist-get left :owner) (plist-get right :owner))
       (equal (plist-get left :execution-id) (plist-get right :execution-id))))

(defun mevedel-view-execution--live (session)
  "Return public live snapshots for SESSION, avoiding enumeration when empty."
  (when (> (mevedel-execution-count-user session) 0)
    (mevedel-execution-list-user session)))

(defun mevedel-view-execution--current (session snapshot &optional live-only)
  "Resolve SNAPSHOT in the current SESSION, or signal a stale action.
With LIVE-ONLY, a retained terminal selection is not sufficient."
  (unless (eq session (mevedel-view--status-session))
    (user-error "Execution belongs to a different session"))
  (or (cl-find-if (lambda (item) (mevedel-view-execution--same-p item snapshot))
                  (mevedel-view-execution--live session))
      (and (not live-only)
           (eq session (plist-get mevedel-view-execution--selected :session))
           (mevedel-view-execution--same-p mevedel-view-execution--selected snapshot)
           mevedel-view-execution--selected)
      (user-error "Execution is no longer available")))

(defun mevedel-view-execution--toggle (session snapshot)
  "Toggle local inspection of SNAPSHOT belonging to SESSION."
  (let ((current (mevedel-view-execution--current session snapshot)))
    (setq mevedel-view-execution--selected
          (unless (and (eq session (plist-get mevedel-view-execution--selected :session))
                       (mevedel-view-execution--same-p mevedel-view-execution--selected current))
            (plist-put (copy-sequence current) :session session)))
    (mevedel-view--render-status)))

(defun mevedel-view-execution--action (text function)
  "Return TEXT carrying the local action FUNCTION, including its margins."
  (propertize text
              'keymap (mevedel-view--display-fragment-keymap)
              'mouse-face 'highlight
              'mevedel-view-zone-activate function))

(defun mevedel-view-execution--callback (action session &optional owner id)
  "Build ACTION for SESSION, OWNER and ID, retaining no volatile snapshot."
  (lambda ()
    (unless (eq session (mevedel-view--status-session))
      (user-error "Execution belongs to a different session"))
    (let ((identity (list :owner owner :execution-id id)))
      (pcase action
        ('toggle (mevedel-view-execution--toggle session identity))
        ('output (mevedel-view-execution--full-output session identity))
        ('conversation
         (mevedel-view-execution-show-in-conversation
          (mevedel-view-execution--current session identity)))
        ('stop
         (let ((current (mevedel-view-execution--current session identity t)))
           (mevedel-execution-stop-user session (plist-get current :execution-id))))
        ('all (mevedel-executions-list-open))))))

(defun mevedel-view-execution--state-label (snapshot)
  "Return an explicit lifecycle label for SNAPSHOT."
  (let ((state (plist-get snapshot :state))
        (termination (plist-get snapshot :termination)))
    (cond
     ((eq state 'stopping) "Stopping")
     ((or (plist-get snapshot :terminal-p)
          (memq state '(completed lost failed)))
      (cond
       ((eq state 'lost) "Lost")
       ((eq termination 'exited)
        (format "Completed (exit %s)" (or (plist-get snapshot :exit-code) "unknown")))
       (termination (format "Completed (%s)" termination))
       (t "Completed")))
     ((plist-get snapshot :unavailable-p) "Completed (details pending)")
     ((eq (plist-get snapshot :kind) 'monitor) "Watching")
     (t "Running"))))

(defun mevedel-view-execution--line (snapshot)
  "Return a whole clickable status row for SNAPSHOT."
  (let* ((session (mevedel-view--status-session))
         (selected (and (eq session (plist-get mevedel-view-execution--selected :session))
                        (mevedel-view-execution--same-p snapshot mevedel-view-execution--selected)))
         (monitor (eq (plist-get snapshot :kind) 'monitor))
         (owner (plist-get snapshot :owner))
         (label (or (and monitor (plist-get snapshot :description))
                    (plist-get snapshot :command) "Bash")))
    (mevedel-view-execution--action
     (concat "  " (if selected "▾ " "▸ ")
             (mevedel-view-execution--state-label snapshot) " · "
             (truncate-string-to-width
              (replace-regexp-in-string "[\n\r\t]+" " " label) 72 nil nil "…")
             (when monitor
               (concat
                (when-let* ((expires (plist-get snapshot :expires-at)))
                  (format " · expires %s" (format-time-string "%H:%M:%S" expires)))
                (format " · %d events" (or (plist-get snapshot :event-count) 0))))
             (unless (equal owner "/root") (format " · %s" owner)) "\n")
     (mevedel-view-execution--callback
      'toggle session owner (plist-get snapshot :execution-id)))))

(defun mevedel-view-execution--preview (snapshot)
  "Return at most three width-bounded preview lines for SNAPSHOT."
  (let* ((terminal (plist-get snapshot :terminal-p))
         (output (if terminal (plist-get snapshot :whole-output)
                   (plist-get snapshot :output-tail)))
         (lines (and (stringp output) (split-string (string-trim-right output) "\n")))
         (truncated (or (plist-get snapshot :output-preview-truncated-p)
                        (plist-get snapshot :whole-output-truncated-p)
                        (> (length lines) 3)
                        (cl-some (lambda (line) (> (string-width line) 100)) lines))))
    (concat
     "    Preview (not full output)"
     (when truncated " — truncated") ":\n"
     (cond
      ((or (not (stringp output))
           (and (string-empty-p output)
                (or (eq (plist-get snapshot :state) 'lost)
                    (> (or (plist-get snapshot :output-bytes) 0) 0))))
       "    Output unavailable.\n")
      ((string-empty-p output)
       (if terminal "    No output.\n" "    No output yet.\n"))
      (t (concat
          (mapconcat (lambda (line)
                       (concat "    " (truncate-string-to-width
                                        (replace-regexp-in-string "[\r\t]" " " line)
                                        100 nil nil "…")))
                     (last lines (min 3 (length lines))) "\n") "\n")))
     (when (or (eq (plist-get snapshot :termination) 'output-limit)
               (> (or (plist-get snapshot :omitted-output-bytes) 0) 0))
       "    Warning: output was truncated; retained output may omit content.\n"))))

(defun mevedel-view-execution--full-output (session snapshot)
  "Inspect full output for SESSION's SNAPSHOT without visiting its spool."
  (let* ((current (mevedel-view-execution--current session snapshot))
         (spool (plist-get current :artifact-path))
         (address (plist-get current :output-path))
         (path (or (and (stringp spool) (not (file-remote-p spool))
                        (file-readable-p spool) spool)
                   (and (stringp address) (string-prefix-p "artifact://" address)
                        (condition-case nil
                            (mevedel-resource-visit-path address (list :session session))
                          (error nil)))))
         (whole (plist-get current :whole-output))
         (buffer (generate-new-buffer "*mevedel execution output*")))
    (with-current-buffer buffer
      (insert (format "Full output · %s · %s\n\n"
                      (plist-get current :execution-id)
                      (mevedel-view-execution--state-label current)))
      (when (or (eq (plist-get current :termination) 'output-limit)
                (> (or (plist-get current :omitted-output-bytes) 0) 0))
        (insert "Warning: output was truncated; retained output may omit content.\n\n"))
      (cond
       ((and path (file-regular-p path) (file-readable-p path))
        (when (< (file-attribute-size (file-attributes path))
                 (or (plist-get current :output-bytes) 0))
          (insert "Warning: retained output is truncated or incomplete.\n\n"))
        (when (zerop (cadr (insert-file-contents path)))
          (insert (cond
                   ((or (eq (plist-get current :state) 'lost)
                        (> (or (plist-get current :output-bytes) 0) 0))
                    "Retained output is empty; output unavailable.\n")
                   ((plist-get current :terminal-p) "No output.\n")
                   (t "No output yet.\n")))))
       ((and (stringp whole) (not (plist-get current :whole-output-truncated-p))
             (or (not (string-empty-p whole))
                 (and (not (eq (plist-get current :state) 'lost))
                      (zerop (or (plist-get current :output-bytes) 0)))))
        (insert (if (string-empty-p whole) "No output.\n" whole)))
       (t
        (insert "Full output unavailable.\n\n" (mevedel-view-execution--preview current))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

(defun mevedel-view-execution-show-in-conversation (snapshot)
  "Show the originating tool row identified by SNAPSHOT, honoring its owner."
  (let ((owner (plist-get snapshot :owner))
        (record (list :execution-id (plist-get snapshot :execution-id)
                      :tool-use-id (plist-get snapshot :tool-use-id)
                      :facts snapshot)))
    (if (equal owner "/root")
        (mevedel-view-audit-show-result record)
      (mevedel-view-open-agent-transcript owner)
      (with-current-buffer (window-buffer (selected-window))
        (mevedel-view-audit-show-result record)))))

(defun mevedel-view-execution--details (session snapshot live)
  "Return expanded controls and preview for SESSION's SNAPSHOT.
LIVE indicates current public live membership, not retained selection state."
  (concat
   (mevedel-view-execution--preview snapshot)
   "    "
   (mevedel-view-execution--action
    "[Full output]" (mevedel-view-execution--callback
                     'output session (plist-get snapshot :owner)
                     (plist-get snapshot :execution-id)))
   "  "
   (mevedel-view-execution--action
    "[Show in conversation]"
    (mevedel-view-execution--callback
     'conversation session (plist-get snapshot :owner)
     (plist-get snapshot :execution-id)))
   (when live
     (concat
      "  "
      (mevedel-view-execution--action
       "[Stop]"
       (mevedel-view-execution--callback
        'stop session (plist-get snapshot :owner)
        (plist-get snapshot :execution-id)))))
   "\n"))

(defun mevedel-view-execution-fragment (session)
  "Return the background execution status fragment for SESSION, or nil."
  (unless (eq session (plist-get mevedel-view-execution--selected :session))
    (setq mevedel-view-execution--selected nil))
  (let* ((live (cl-remove-if-not (lambda (item) (plist-get item :yielded))
                                (mevedel-view-execution--live session)))
         (selected-live (cl-find-if
                         (lambda (item) (mevedel-view-execution--same-p
                                        item mevedel-view-execution--selected)) live))
         (rows (seq-take live 5)))
    (when selected-live
      (setq mevedel-view-execution--selected
            (plist-put (copy-sequence selected-live) :session session)))
    (when (and mevedel-view-execution--selected (not selected-live))
      ;; State notification precedes terminal publication.  Keep the selection
      ;; through that gap, but do not offer live controls on stale facts.
      (setq mevedel-view-execution--selected
            (plist-put mevedel-view-execution--selected :unavailable-p t)))
    (when (and selected-live (not (member selected-live rows)))
      (setq rows (append (seq-take rows 4) (list selected-live))))
    (when (or rows mevedel-view-execution--selected)
      (let* ((all (mevedel-view-execution--action
                   "All…"
                   (mevedel-view-execution--callback 'all session)))
             (body (concat "  Background work · " all "\n")))
        (when (and mevedel-view-execution--selected (not selected-live))
          (setq rows (append rows (list mevedel-view-execution--selected))))
        (dolist (snapshot rows)
          (setq body (concat body (mevedel-view-execution--line snapshot)))
          (when (mevedel-view-execution--same-p snapshot mevedel-view-execution--selected)
            (setq body (concat body (mevedel-view-execution--details
                                     session snapshot selected-live)))))
        (when (> (length live) 5)
          (setq body (concat body "  " (mevedel-view-execution--action
                                       (format "+%d more / all" (- (length live) 5))
                                       (get-text-property 0 'mevedel-view-zone-activate all)) "\n")))
        (list :namespace 'status :id 'executions :priority 50 :body body
              :body-suffix "\n" :navigatable t :entry 'executions)))))

(defun mevedel-view-execution--view (session data-buffer)
  "Resolve SESSION's parent view from originating DATA-BUFFER, without scans."
  (when (buffer-live-p data-buffer)
    (let* ((invocation (buffer-local-value 'mevedel--agent-invocation data-buffer))
           (parent (if invocation
                       (mevedel-agent-invocation-parent-data-buffer invocation)
                     data-buffer))
           (view (and (buffer-live-p parent)
                      (buffer-local-value 'mevedel--view-buffer parent))))
      (when (and (buffer-live-p view)
                 (with-current-buffer view
                   (and (derived-mode-p 'mevedel-view-mode)
                        (not mevedel-view--agent-transcript-p)
                        (eq session (mevedel-view--status-session)))))
        view))))

(defun mevedel-view-execution--state-changed (session data-buffer)
  "Refresh SESSION's live rows and Monitor counts from DATA-BUFFER."
  (when-let* ((view (mevedel-view-execution--view session data-buffer)))
    (with-current-buffer view (mevedel-view--render-status))))

(defun mevedel-view-execution--event (event)
  "Refresh only the selected inspector from passive progress or terminal EVENT."
  (when (memq (plist-get event :type) '(progress terminal))
    (let ((session (plist-get event :session)))
      (when-let* ((view (mevedel-view-execution--view session (plist-get event :data-buffer))))
        (with-current-buffer view
          (let* ((facts (plist-get event :facts))
                 (snapshot (append (list :owner (plist-get event :owner)) facts)))
            (when (and (eq session (plist-get mevedel-view-execution--selected :session))
                       (mevedel-view-execution--same-p snapshot mevedel-view-execution--selected)
                       (not (plist-get mevedel-view-execution--selected :terminal-p)))
              (let* ((previous mevedel-view-execution--selected)
                     (updated (copy-sequence previous))
                     (terminal (eq (plist-get event :type) 'terminal)))
                (while facts
                  (setq updated (plist-put updated (pop facts) (pop facts))))
                (if terminal
                    (setq updated (plist-put updated :terminal-p t)
                          updated (plist-put updated :whole-output (plist-get event :whole-output))
                          updated (plist-put updated :whole-output-truncated-p
                                             (plist-get event :whole-output-truncated-p)))
                  (setq updated (plist-put updated :output-tail (plist-get event :output-tail))
                        updated (plist-put updated :output-preview-truncated-p
                                           (plist-get event :output-preview-truncated-p))))
                (setq mevedel-view-execution--selected updated)
                ;; Quiet progress still updates facts, but need not enumerate
                ;; all executions and rebuild status for invisible changes.
                (when (or terminal
                          (not (and (equal (mevedel-view-execution--line previous)
                                           (mevedel-view-execution--line updated))
                                    (equal (mevedel-view-execution--preview previous)
                                           (mevedel-view-execution--preview updated)))))
                  (mevedel-view--render-status))))))))))

(add-hook 'mevedel-execution-state-change-hook #'mevedel-view-execution--state-changed)
(add-hook 'mevedel-execution-event-functions #'mevedel-view-execution--event)

(provide 'mevedel-view-execution)
;;; mevedel-view-execution.el ends here
