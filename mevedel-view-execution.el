;;; mevedel-view-execution.el --- Background-work status panel -*- lexical-binding: t -*-

;;; Commentary:

;; The status zone's background-work panel: one rule with counts and a
;; collapse toggle, then one row per active agent and per live yielded
;; execution (Bash or Monitor), needs-attention first.  Rows expand in place
;; to show the item's actions; a last row offers overflow and the
;; background-work cockpit.  Finished items leave the panel; their transcript
;; notices take over.
;;
;; Execution rows project public user-authority snapshots.  One buffer-local
;; selection retains a selected execution's bounded output after completion;
;; it is not a process registry.  Passive events refresh that selection
;; without consuming output, acknowledging mail, polling, or redrawing the
;; transcript.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-structs)
;; Agent rows are half the panel; every view loads this module anyway.
(require 'mevedel-view-agent)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-interrupt-user
                  "mevedel-agent-control" (session path))
(autoload 'mevedel-agent-control-interrupt-user "mevedel-agent-control")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(autoload 'mevedel-agent-invocation-parent-data-buffer "mevedel-agents")

;; `mevedel-background-list'
(declare-function mevedel-background-list-open "mevedel-background-list" (&optional context))
(autoload 'mevedel-background-list-open "mevedel-background-list" nil t)

;; `mevedel-execution'
(declare-function mevedel-execution-count-user "mevedel-execution" (session))
(declare-function mevedel-execution-list-user "mevedel-execution" (session))
(declare-function mevedel-execution-stop-user "mevedel-execution" (session execution-id))
(autoload 'mevedel-execution-count-user "mevedel-execution")
(autoload 'mevedel-execution-list-user "mevedel-execution")
(autoload 'mevedel-execution-stop-user "mevedel-execution")
(defvar mevedel-execution-event-functions)
(defvar mevedel-execution-state-change-hook)

;; `mevedel-resource'
(declare-function mevedel-resource-visit-path "mevedel-resource" (address &optional context))
(autoload 'mevedel-resource-visit-path "mevedel-resource")

;; `mevedel-view'
(declare-function mevedel-view--display-fragment-keymap "mevedel-view" (&rest maps))
(declare-function mevedel-view--render-status "mevedel-view" (&optional data-buf))
(declare-function mevedel-view--status-session "mevedel-view" (&optional data-buf))
(declare-function mevedel-view--zone-separator "mevedel-view" (label))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-audit'
(declare-function mevedel-view-audit-show-result "mevedel-view-audit" (record))
(autoload 'mevedel-view-audit-show-result "mevedel-view-audit" nil t)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-collapse-state
                  "mevedel-view-zone" (key &optional default))
(declare-function mevedel-view-zone-set-collapse-state
                  "mevedel-view-zone" (key collapsed))

(defconst mevedel-view-execution--collapse-key '(status background)
  "Fragment collapse key of the background-work panel.")

(defconst mevedel-view-execution--row-limit 5
  "Rows the panel lists before offering to show all.")

(defvar-local mevedel-view-execution--selected nil
  "Selected public snapshot, with its :session and presentation-only flags.
Only this item can retain terminal facts and bounded :whole-output.")

(defvar-local mevedel-view-execution--selected-agent nil
  "Canonical path of the expanded agent row, or nil.
At most one row is expanded: this and `mevedel-view-execution--selected'
exclude each other.")

(defvar-local mevedel-view-execution--show-all nil
  "Non-nil while the panel lists every row instead of the first few.")

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
            (plist-put (copy-sequence current) :session session))
          mevedel-view-execution--selected-agent nil)
    (mevedel-view--render-status)))

(defun mevedel-view-execution--toggle-agent (path)
  "Toggle the inline actions of the agent row at canonical PATH."
  (setq mevedel-view-execution--selected-agent
        (unless (equal path mevedel-view-execution--selected-agent) path))
  (when mevedel-view-execution--selected-agent
    (setq mevedel-view-execution--selected nil))
  (mevedel-view--render-status))

(defun mevedel-view-execution--agent-row (path)
  "Return the current status row of active agent PATH, or signal."
  (or (cl-find path (mevedel-view--agent-status-collect)
               :key (lambda (row) (plist-get row :path)) :test #'equal)
      (user-error "Agent %s is no longer active" path)))

(defun mevedel-view-execution--agent-conversation (path)
  "Show the Agent call that started PATH in its parent's conversation."
  (let ((row (mevedel-view-execution--agent-row path)))
    (unless (plist-get row :tool-use-id)
      (user-error "The call that started %s is not available" path))
    (mevedel-view-audit-show-result
     (list :owner (or (plist-get row :parent-path) "/root")
           :tool-use-id (plist-get row :tool-use-id)))))

(defun mevedel-view-execution-toggle-panel ()
  "Collapse or expand the background-work panel."
  (interactive)
  (mevedel-view-zone-set-collapse-state
   mevedel-view-execution--collapse-key
   (not (mevedel-view-zone-collapse-state mevedel-view-execution--collapse-key nil)))
  (mevedel-view--render-status))

(defun mevedel-view-execution-reset ()
  "Reset the panel's view-local expansion, overflow and collapse state."
  (setq mevedel-view-execution--selected nil
        mevedel-view-execution--selected-agent nil
        mevedel-view-execution--show-all nil)
  (mevedel-view-zone-set-collapse-state mevedel-view-execution--collapse-key nil)
  (mevedel-view--render-status))

(defun mevedel-view-execution--action (text function)
  "Return TEXT carrying the local action FUNCTION, including its margins.
A trailing newline stays activatable but gets no `mouse-face': Emacs
highlights a contiguous run of one `mouse-face' as a single region, so
highlighting row ends would join adjacent rows into one hover."
  (let ((text (propertize text
                          'keymap (mevedel-view--display-fragment-keymap)
                          'mouse-face 'highlight
                          'mevedel-view-zone-activate function)))
    (when (string-suffix-p "\n" text)
      (remove-text-properties (1- (length text)) (length text) '(mouse-face nil) text))
    text))

(defun mevedel-view-execution--face (text face)
  "Return TEXT displayed with FACE, whether or not font-lock is active."
  (propertize text 'face face 'font-lock-face face))

(defun mevedel-view-execution--button (text function help)
  "Return link-styled TEXT activating FUNCTION, described by HELP."
  (propertize (mevedel-view-execution--action
               (mevedel-view-execution--face text 'link) function)
              'help-echo help))

(defun mevedel-view-execution--callback (action session &optional owner id)
  "Build ACTION for SESSION, OWNER and ID, retaining no volatile snapshot.
Agent actions receive the agent's canonical path as OWNER and no ID."
  (lambda ()
    (unless (eq session (mevedel-view--status-session))
      (user-error "Background work belongs to a different session"))
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
        ('agent-toggle (mevedel-view-execution--toggle-agent owner))
        ('transcript
         (mevedel-view-execution--agent-row owner)
         (mevedel-view-agent-handle-activate owner))
        ('agent-conversation (mevedel-view-execution--agent-conversation owner))
        ('interrupt
         (mevedel-view-execution--agent-row owner)
         (mevedel-agent-control-interrupt-user session owner))
        ('collapse (mevedel-view-execution-toggle-panel))
        ('show-all
         (setq mevedel-view-execution--show-all (not mevedel-view-execution--show-all))
         (mevedel-view--render-status))
        ('manage (mevedel-background-list-open))))))

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

(defun mevedel-view-execution--state-face (snapshot)
  "Return the face signalling SNAPSHOT's lifecycle outcome."
  (let ((state (plist-get snapshot :state))
        (termination (plist-get snapshot :termination))
        (exit (plist-get snapshot :exit-code)))
    (cond
     ((eq state 'stopping) 'mevedel-view-tool-warning)
     ((memq state '(lost failed)) 'error)
     ((or (plist-get snapshot :terminal-p) (eq state 'completed))
      (cond
       ((and (eq termination 'exited) (eql exit 0)) 'success)
       ((eq termination 'exited) 'error)
       (termination 'mevedel-view-tool-warning)
       (t 'success)))
     ((plist-get snapshot :unavailable-p) 'mevedel-view-tool-metadata)
     (t 'mevedel-view-agent-running))))

(defun mevedel-view-execution--row (spec)
  "Return one whole clickable panel row described by plist SPEC.
SPEC holds :selected, :state, :state-face, :kind, :label, :facts (a list of
strings), :depth and :callback.  Every kind shares this grammar:
chevron, padded state, padded kind, label, then metadata."
  (mevedel-view-execution--action
   (concat "  "
           (make-string (* 2 (or (plist-get spec :depth) 0)) ?\s)
           (mevedel-view-execution--face
            (if (plist-get spec :selected) "▾ " "▸ ") 'mevedel-view-tool-metadata)
           (mevedel-view-execution--face
            (string-pad (plist-get spec :state) 9) (plist-get spec :state-face))
           " "
           (mevedel-view-execution--face
            (string-pad (plist-get spec :kind) 8) 'mevedel-view-tool-metadata)
           " "
           (truncate-string-to-width
            (replace-regexp-in-string "[\n\r\t]+" " " (plist-get spec :label))
            60 nil nil "…")
           (mevedel-view-execution--face
            (mapconcat (lambda (fact) (concat " · " fact)) (plist-get spec :facts) "")
            'mevedel-view-tool-metadata)
           "\n")
   (plist-get spec :callback)))

(defun mevedel-view-execution--line (snapshot)
  "Return a whole clickable status row for execution SNAPSHOT."
  (let* ((session (mevedel-view--status-session))
         (monitor (eq (plist-get snapshot :kind) 'monitor))
         (owner (plist-get snapshot :owner))
         (events (or (plist-get snapshot :event-count) 0)))
    (mevedel-view-execution--row
     (list :selected (and (eq session (plist-get mevedel-view-execution--selected :session))
                          (mevedel-view-execution--same-p
                           snapshot mevedel-view-execution--selected))
           :state (mevedel-view-execution--state-label snapshot)
           :state-face (mevedel-view-execution--state-face snapshot)
           :kind (if monitor "monitor" "bash")
           :label (or (plist-get snapshot :description)
                      (plist-get snapshot :command) "Bash")
           :facts (delq nil
                        (list (when monitor
                                (format "%d %s" events (if (= events 1) "event" "events")))
                              (when-let* ((expires (and monitor (plist-get snapshot :expires-at))))
                                (format "until %s" (format-time-string "%H:%M" expires)))
                              (unless (equal owner "/root") owner)))
           :callback (mevedel-view-execution--callback
                      'toggle session owner (plist-get snapshot :execution-id))))))

(defun mevedel-view-execution--agent-line (session row)
  "Return a whole clickable status row for SESSION's active agent ROW."
  (let ((path (plist-get row :path))
        (status (plist-get row :status)))
    (mevedel-view-execution--row
     (list :selected (equal path mevedel-view-execution--selected-agent)
           :state (pcase status ('blocked "Blocked") ('waiting "Waiting") (_ "Running"))
           :state-face (if (eq status 'running)
                           'mevedel-view-agent-running
                         'mevedel-view-tool-warning)
           :kind "agent"
           :label path
           :depth (plist-get row :depth)
           :facts (delq nil
                        (list (pcase (plist-get row :activity)
                                ('permission-blocked "waiting for permission")
                                ('interaction-blocked "waiting for input")
                                (_ (mevedel-view-agent-model-label
                                    (plist-get row :model-metadata))))))
           :callback (mevedel-view-execution--callback 'agent-toggle session path)))))

(defun mevedel-view-execution--agent-details (session row)
  "Return expanded model details and controls for SESSION's agent ROW."
  (let ((path (plist-get row :path)))
    (concat
     (when-let* ((details (mevedel-view-agent-model-details
                           (plist-get row :model-metadata))))
       (mevedel-view-execution--face
        (concat (replace-regexp-in-string "^" "    " details) "\n")
        'mevedel-view-tool-metadata))
     "    "
     (mevedel-view-execution--button
      "[Open transcript]" (mevedel-view-execution--callback 'transcript session path)
      "RET: open this agent's conversation")
     (when (plist-get row :tool-use-id)
       (concat "  " (mevedel-view-execution--button
                     "[Show in conversation]"
                     (mevedel-view-execution--callback 'agent-conversation session path)
                     "RET: show the Agent call that started this agent")))
     "  "
     (mevedel-view-execution--button
      "[Interrupt]" (mevedel-view-execution--callback 'interrupt session path)
      "RET: interrupt this agent's current turn")
     "\n")))

(defun mevedel-view-execution--preview (snapshot)
  "Return at most three width-bounded preview lines for SNAPSHOT."
  (let* ((terminal (plist-get snapshot :terminal-p))
         (output (if terminal (plist-get snapshot :whole-output)
                   (plist-get snapshot :output-tail)))
         (lines (and (stringp output) (split-string (string-trim-right output) "\n")))
         (truncated (or (plist-get snapshot :output-preview-truncated-p)
                        (plist-get snapshot :whole-output-truncated-p)
                        (> (length lines) 3)
                        (cl-some (lambda (line) (> (string-width line) 100)) lines)))
         (quiet (lambda (text)
                  (concat "    " (mevedel-view-execution--face
                                   text '(mevedel-view-muted italic))
                          "\n"))))
    (concat
     "    "
     (mevedel-view-execution--face
      (concat "Preview (not full output)" (when truncated " — truncated") ":")
      'mevedel-view-tool-metadata)
     "\n"
     (cond
      ((or (not (stringp output))
           (and (string-empty-p output)
                (or (eq (plist-get snapshot :state) 'lost)
                    (> (or (plist-get snapshot :output-bytes) 0) 0))))
       (funcall quiet "Output unavailable."))
      ((string-empty-p output)
       (funcall quiet (if terminal "No output." "No output yet.")))
      (t (concat
          (mapconcat (lambda (line)
                       (concat "    " (truncate-string-to-width
                                        (replace-regexp-in-string "[\r\t]" " " line)
                                        100 nil nil "…")))
                     (last lines (min 3 (length lines))) "\n") "\n")))
     (when (or (eq (plist-get snapshot :termination) 'output-limit)
               (> (or (plist-get snapshot :omitted-output-bytes) 0) 0))
       (concat "    "
               (mevedel-view-execution--face
                "Warning: output was truncated; retained output may omit content."
                'mevedel-view-tool-warning)
               "\n")))))

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
  ;; The audit navigator owns agent opening, source preparation and archived
  ;; segment selection.  Pre-opening here bypasses its preparation of a
  ;; settled agent's asynchronously rendered transcript.
  (mevedel-view-audit-show-result
   (list :owner (plist-get snapshot :owner)
         :execution-id (plist-get snapshot :execution-id)
         :tool-use-id (plist-get snapshot :tool-use-id)
         :facts snapshot)))

(defun mevedel-view-execution--details (session snapshot live)
  "Return expanded controls and preview for SESSION's SNAPSHOT.
LIVE indicates current public live membership, not retained selection state."
  (concat
   (mevedel-view-execution--preview snapshot)
   "    "
   (mevedel-view-execution--button
    "[Full output]" (mevedel-view-execution--callback
                     'output session (plist-get snapshot :owner)
                     (plist-get snapshot :execution-id))
    "RET: open the full retained output")
   "  "
   (mevedel-view-execution--button
    "[Show in conversation]"
    (mevedel-view-execution--callback
     'conversation session (plist-get snapshot :owner)
     (plist-get snapshot :execution-id))
    "RET: show the tool call that started this execution")
   (when live
     (concat
      "  "
      (mevedel-view-execution--button
       "[Stop]"
       (mevedel-view-execution--callback
        'stop session (plist-get snapshot :owner)
        (plist-get snapshot :execution-id))
       "RET: stop this execution")))
   "\n"))

(defun mevedel-view-execution--header (session agents live collapsed)
  "Return SESSION's background-work rule for AGENTS and LIVE executions.
It counts blocked agents, agents, commands and monitors, and ends with the
panel's collapse toggle, `[-]' or `[+]' when COLLAPSED."
  (let* ((blocked (cl-count-if (lambda (row) (memq (plist-get row :status)
                                                   '(blocked waiting)))
                               agents))
         (monitors (cl-count 'monitor live :key (lambda (item) (plist-get item :kind))))
         (commands (- (length live) monitors))
         (count (lambda (n singular plural)
                  (when (> n 0) (format "%d %s" n (if (= n 1) singular plural)))))
         (toggle (if collapsed "[+]" "[-]"))
         (header (mevedel-view--zone-separator
                  (concat
                   (string-join
                    (delq nil (list "background work"
                                    (funcall count blocked "blocked" "blocked")
                                    (funcall count (length agents) "agent" "agents")
                                    (funcall count commands "command" "commands")
                                    (funcall count monitors "monitor" "monitors")))
                    " · ")
                   " " toggle))))
    ;; A narrow rule may clip the label.  Keep the toggle reachable by
    ;; replacing the rule's leading dashes.
    (unless (string-search toggle header)
      (setq header (concat (substring header 0 1) toggle (substring header 4))))
    (when-let* ((label (and (> blocked 0) (format "%d blocked" blocked)))
                (start (string-search label header)))
      (add-text-properties start (+ start (length label))
                           '(face mevedel-view-tool-warning
                             font-lock-face mevedel-view-tool-warning)
                           header))
    (let ((start (string-search toggle header)))
      (concat (substring header 0 start)
              (mevedel-view-execution--button
               toggle (mevedel-view-execution--callback 'collapse session)
               (if collapsed "RET: show background work" "RET: hide background work"))
              (substring header (+ start (length toggle)))))))

(defun mevedel-view-execution--controls (session total)
  "Return the panel's last row for SESSION with TOTAL items.
It offers `[Show all N]' or `[Show fewer]' when rows overflow, and
`[Manage]', which opens the background-work cockpit."
  (concat
   "  "
   (when (> total mevedel-view-execution--row-limit)
     (concat (mevedel-view-execution--button
              (if mevedel-view-execution--show-all
                  "[Show fewer]"
                (format "[Show all %d]" total))
              (mevedel-view-execution--callback 'show-all session)
              (if mevedel-view-execution--show-all
                  "RET: list only the first rows"
                "RET: list every background item here"))
             "  "))
   (mevedel-view-execution--button
    "[Manage]" (mevedel-view-execution--callback 'manage session)
    "RET: open the background-work cockpit")
   "\n"))

(defun mevedel-view-execution-fragment (session)
  "Return SESSION's background-work panel fragment, or nil when idle."
  (unless (eq session (plist-get mevedel-view-execution--selected :session))
    (setq mevedel-view-execution--selected nil))
  (let* ((agents (mevedel-view--agent-status-collect))
         (live (cl-remove-if-not (lambda (item) (plist-get item :yielded))
                                (mevedel-view-execution--live session)))
         (selected-live (cl-find-if
                         (lambda (item) (mevedel-view-execution--same-p
                                        item mevedel-view-execution--selected)) live))
         ;; Needs attention first, then running work, then watchers.
         (items (append
                 (mapcar (lambda (row) (cons 'agent row))
                         (cl-remove-if-not (lambda (row) (memq (plist-get row :status)
                                                               '(blocked waiting)))
                                           agents))
                 (mapcar (lambda (row) (cons 'agent row))
                         (cl-remove-if (lambda (row) (memq (plist-get row :status)
                                                           '(blocked waiting)))
                                       agents))
                 (mapcar (lambda (item) (cons 'execution item))
                         (cl-remove 'monitor live :key (lambda (item) (plist-get item :kind))))
                 (mapcar (lambda (item) (cons 'execution item))
                         (cl-remove-if-not (lambda (item) (eq (plist-get item :kind) 'monitor))
                                           live))))
         (selected-item (cl-find-if
                         (lambda (item)
                           (if (eq (car item) 'agent)
                               (equal (plist-get (cdr item) :path)
                                      mevedel-view-execution--selected-agent)
                             (eq (cdr item) selected-live)))
                         items))
         (rows (if mevedel-view-execution--show-all
                   items
                 (seq-take items mevedel-view-execution--row-limit)))
         (collapsed (mevedel-view-zone-collapse-state
                     mevedel-view-execution--collapse-key nil)))
    (unless (cl-find mevedel-view-execution--selected-agent agents
                     :key (lambda (row) (plist-get row :path)) :test #'equal)
      (setq mevedel-view-execution--selected-agent nil))
    (when selected-live
      (setq mevedel-view-execution--selected
            (plist-put (copy-sequence selected-live) :session session)))
    (when (and mevedel-view-execution--selected (not selected-live))
      ;; State notification precedes terminal publication.  Keep the selection
      ;; through that gap, but do not offer live controls on stale facts.
      (setq mevedel-view-execution--selected
            (plist-put mevedel-view-execution--selected :unavailable-p t)))
    (when (and selected-item (not (memq selected-item rows)))
      (setq rows (append (seq-take rows (1- mevedel-view-execution--row-limit))
                         (list selected-item))))
    (when (and mevedel-view-execution--selected (not selected-live))
      (setq rows (append rows (list (cons 'execution mevedel-view-execution--selected)))))
    (when rows
      (let ((body (mevedel-view-execution--header session agents live collapsed)))
        (unless collapsed
          (dolist (item rows)
            (let ((data (cdr item)))
              (if (eq (car item) 'agent)
                  (progn
                    (setq body (concat body (mevedel-view-execution--agent-line session data)))
                    (when (equal (plist-get data :path) mevedel-view-execution--selected-agent)
                      (setq body (concat body (mevedel-view-execution--agent-details
                                               session data)))))
                (setq body (concat body (mevedel-view-execution--line data)))
                (when (mevedel-view-execution--same-p data mevedel-view-execution--selected)
                  (setq body (concat body (mevedel-view-execution--details
                                           session data selected-live)))))))
          (setq body (concat body (mevedel-view-execution--controls session (length items)))))
        (list :namespace 'status :id 'background :priority 50 :body body
              :body-suffix "\n" :navigatable t :entry 'background
              :collapsible t :collapse-key mevedel-view-execution--collapse-key
              :collapsed collapsed)))))

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
