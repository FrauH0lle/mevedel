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
(declare-function mevedel-view-zone-fragment-bounds "mevedel-view-zone" (zone id))
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

(defvar-local mevedel-view-execution--shown nil
  "Rows of the last rendered panel, as (KEY KIND DATA) in display order.")

(defvar-local mevedel-view-execution--held nil
  "Finished rows kept as inert lines while the pointer is on the panel.
Each entry is (KEY KIND DATA).  Removing a row under the mouse or point
would shift the rows the user is aiming at, so finished rows stay in
place until both leave the panel.")

(defvar-local mevedel-view-execution--release-timer nil
  "Timer that releases held rows once the pointer leaves the panel.")

(defvar-local mevedel-view-execution--unit-starts nil
  "Hash table of panel unit identity to its first-seen start time.
An agent tree keeps its place after its top-level agent finishes, whose
start would otherwise be lost.")

(defconst mevedel-view-execution--release-interval 0.5
  "Seconds between checks whether held rows can be released.")

(defvar mevedel-view-execution-grace-seconds 10
  "Seconds a finished row stays on the panel after the pointer is away.
While point or the mouse is on the panel, finished rows never leave;
leaving restarts this period.")

(defvar-local mevedel-view-execution--hovering nil
  "Whether the pointer was on the panel at the last check.")

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

(defun mevedel-view-execution--retain-selected ()
  "Give the completed selected inspector a fresh held-row grace period.
Retain only its summary, not the inspector's output payloads."
  (when-let* ((selected mevedel-view-execution--selected)
              ((or (plist-get selected :terminal-p)
                   (memq (plist-get selected :state) '(completed lost failed)))))
    (let ((data (copy-sequence selected))
          (key (mevedel-view-execution--key 'execution selected)))
      (cl-remf data :whole-output)
      (cl-remf data :output-tail)
      (setq data (plist-put data :held-p t)
            data (plist-put data :release-at
                            (+ (float-time) mevedel-view-execution-grace-seconds))
            mevedel-view-execution--held
            (cons (list key 'execution data)
                  (cl-remove key mevedel-view-execution--held :key #'car :test #'equal))))))

(defun mevedel-view-execution--toggle (session snapshot)
  "Toggle local inspection of SNAPSHOT belonging to SESSION."
  (let ((current (mevedel-view-execution--current session snapshot)))
    (mevedel-view-execution--retain-selected)
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
    (mevedel-view-execution--retain-selected)
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
        mevedel-view-execution--show-all nil
        mevedel-view-execution--shown nil
        mevedel-view-execution--held nil
        mevedel-view-execution--unit-starts nil
        mevedel-view-execution--hovering nil)
  (mevedel-view-execution--cancel-release)
  (mevedel-view-zone-set-collapse-state mevedel-view-execution--collapse-key nil)
  (mevedel-view--render-status))


;;;; Stable order and held rows

(defun mevedel-view-execution--key (kind data)
  "Return the identity of a KIND row showing DATA."
  (if (eq kind 'agent)
      (list 'agent (plist-get data :path))
    (list 'execution (plist-get data :owner) (plist-get data :execution-id))))

(defun mevedel-view-execution--start-time (data)
  "Return DATA's start as a float, or 0 when unknown."
  (let ((start (plist-get data :started-at)))
    (cond ((numberp start) (float start))
          (start (float-time start))
          (t 0.0))))

(defun mevedel-view-execution--agent-root (path)
  "Return the top-level agent path containing agent PATH."
  (let ((parts (split-string path "/" t)))
    (concat "/" (string-join (seq-take parts 2) "/"))))

(defun mevedel-view-execution--ordered (entries &optional starts)
  "Return ENTRIES, each (KEY KIND DATA), newest first.
New work then appears at the panel edge farthest from the composer, so
rows already shown keep their screen position: with the composer
anchored, only text above a change moves.  An agent and its descendants
form one unit, ordered by their top-level agent, in path order inside.
STARTS, a hash table, remembers each unit's start across calls and is
pruned to the units present."
  (let (units groups)
    (dolist (entry entries)
      (let* ((kind (nth 1 entry))
             (data (nth 2 entry))
             (group (if (eq kind 'agent)
                        (mevedel-view-execution--agent-root (plist-get data :path))
                      (car entry)))
             (unit (assoc group units)))
        (if unit
            (setcdr unit (append (cdr unit) (list entry)))
          (push (list group entry) units))))
    (setq groups (mapcar #'car units)
          units
          (mapcar
           (lambda (unit)
             (let* ((members (cdr unit))
                    (head (cl-find (car unit) members
                                   :key (lambda (entry) (plist-get (nth 2 entry) :path))
                                   :test #'equal))
                    (start (or (and starts (gethash (car unit) starts))
                               (if head
                                   (mevedel-view-execution--start-time (nth 2 head))
                                 (apply #'min (mapcar (lambda (entry)
                                                        (mevedel-view-execution--start-time
                                                         (nth 2 entry)))
                                                      members))))))
               (when starts (puthash (car unit) start starts))
               (cons start
                     (sort members
                           (lambda (left right)
                             (and (eq (nth 1 left) 'agent) (eq (nth 1 right) 'agent)
                                  (string-lessp (plist-get (nth 2 left) :path)
                                                (plist-get (nth 2 right) :path))))))))
           (nreverse units)))
    (when starts
      (let (stale)
        (maphash (lambda (group _start)
                   (unless (member group groups) (push group stale)))
                 starts)
        (dolist (group stale) (remhash group starts))))
    (apply #'append
           (mapcar #'cdr (sort units (lambda (left right) (> (car left) (car right))))))))

(defun mevedel-view-execution--pointer-on-panel-p ()
  "Return non-nil when point or the mouse is on the background-work panel.
Point counts in every window showing this view."
  (when-let* ((bounds (mevedel-view-zone-fragment-bounds 'status 'background)))
    (let ((start (plist-get bounds :start))
          (end (plist-get bounds :end)))
      (or (cl-some (lambda (window)
                     (and (<= start (window-point window)) (< (window-point window) end)))
                   (get-buffer-window-list (current-buffer) nil t))
          (pcase-let ((`(,frame ,x . ,y) (mouse-pixel-position)))
            (when (and (frame-live-p frame) (integerp x) (integerp y))
              (let* ((posn (posn-at-x-y x y frame))
                     (window (posn-window posn))
                     (pos (posn-point posn)))
                (and (windowp window)
                     (eq (window-buffer window) (current-buffer))
                     (integerp pos) (<= start pos) (< pos end)))))))))

(defun mevedel-view-execution--cancel-release ()
  "Cancel the held-row release timer."
  (when (timerp mevedel-view-execution--release-timer)
    (cancel-timer mevedel-view-execution--release-timer))
  (setq mevedel-view-execution--release-timer nil))

(defun mevedel-view-execution--check-release (buffer &optional timer)
  "Re-render BUFFER's panel when its held rows may change.
That is when the pointer enters or leaves the panel, restarting the grace
period, or when a held row's grace period ends away from the pointer.
TIMER is the calling timer, cancelled alone when BUFFER is gone."
  (if (not (buffer-live-p buffer))
      (when (timerp timer) (cancel-timer timer))
    (with-current-buffer buffer
      (if (null mevedel-view-execution--held)
          (mevedel-view-execution--cancel-release)
        (let ((hover (mevedel-view-execution--pointer-on-panel-p))
              (now (float-time)))
          (when (or (not (eq hover mevedel-view-execution--hovering))
                    (and (not hover)
                         (cl-some (lambda (held)
                                    (<= (plist-get (nth 2 held) :release-at) now))
                                  mevedel-view-execution--held)))
            (mevedel-view--render-status)))))))

(defun mevedel-view-execution--update-held (entries hover)
  "Return held rows after rendering live ENTRIES, scheduling their release.
Rows shown last time and finished since stay for
`mevedel-view-execution-grace-seconds'.  While HOVER, the pointer or point
being on the panel, holds they never leave; leaving restarts the period."
  (let ((live-keys (mapcar #'car entries))
        (deadline (+ (float-time) mevedel-view-execution-grace-seconds)))
    (dolist (shown mevedel-view-execution--shown)
      (unless (or (member (car shown) live-keys)
                  (assoc (car shown) mevedel-view-execution--held)
                  ;; An expanded execution keeps its own terminal inspector.
                  (and (eq (nth 1 shown) 'execution)
                       (mevedel-view-execution--same-p
                        (nth 2 shown) mevedel-view-execution--selected)))
        (push (list (car shown) (nth 1 shown)
                    (plist-put (plist-put (copy-sequence (nth 2 shown)) :held-p t)
                               :release-at deadline))
              mevedel-view-execution--held)))
    (when (and mevedel-view-execution--hovering (not hover))
      ;; Leaving the panel restarts every held row's grace period.
      (dolist (held mevedel-view-execution--held)
        (setcar (nthcdr 2 held) (plist-put (nth 2 held) :release-at deadline))))
    (setq mevedel-view-execution--hovering hover)
    (let ((now (float-time)))
      (setq mevedel-view-execution--held
            (cl-remove-if (lambda (held)
                            (or (member (car held) live-keys)
                                (and (not hover)
                                     (<= (plist-get (nth 2 held) :release-at) now))))
                          mevedel-view-execution--held)))
    (if mevedel-view-execution--held
        (unless (timerp mevedel-view-execution--release-timer)
          (let ((buffer (current-buffer))
                timer)
            (setq timer (run-with-timer
                         mevedel-view-execution--release-interval
                         mevedel-view-execution--release-interval
                         (lambda ()
                           (mevedel-view-execution--check-release buffer timer)))
                  mevedel-view-execution--release-timer timer)
            ;; A held row must not keep a timer alive past its view.
            (add-hook 'kill-buffer-hook #'mevedel-view-execution--cancel-release nil t)))
      (mevedel-view-execution--cancel-release))
    mevedel-view-execution--held))

(defun mevedel-view-execution--held-line (kind data)
  "Return the inert dimmed line of a finished KIND row showing DATA."
  (let ((face 'mevedel-view-tool-metadata))
    (concat
     "  "
     (make-string (* 2 (or (plist-get data :depth) 0)) ?\s)
     (mevedel-view-execution--face
      (concat "✓ "
              (string-pad (if (eq kind 'agent) "Finished" "Completed") 9)
              " "
              (string-pad (cond ((eq kind 'agent) "agent")
                                ((eq (plist-get data :kind) 'monitor) "monitor")
                                (t "bash"))
                          8)
              " "
              (truncate-string-to-width
               (replace-regexp-in-string
                "[\n\r\t]+" " "
                (or (if (eq kind 'agent) (plist-get data :path)
                      (or (plist-get data :description) (plist-get data :command)))
                    ""))
               60 nil nil "…")
              (when-let* ((exit (and (eq (plist-get data :termination) 'exited)
                                     (plist-get data :exit-code))))
                (format " · exit %s" exit)))
      face)
     "\n")))

(defun mevedel-view-execution--mouse-press (event)
  "Select the window at EVENT without activating background work.
A press in the already selected window keeps normal drag selection and
click activation.  A focus-changing press consumes only its click release."
  (interactive "e")
  (if (eq (posn-window (event-start event)) (selected-window))
      (mouse-drag-region event)
    (mouse-set-point event)
    (set-transient-map (define-keymap "<mouse-1>" #'ignore))))

(defvar-keymap mevedel-view-execution--action-map
  :doc "Background-work mouse press handling before shared activation."
  "<down-mouse-1>" #'mevedel-view-execution--mouse-press)

(defun mevedel-view-execution--action (text function)
  "Return TEXT carrying the local action FUNCTION, including its margins.
A trailing newline stays activatable but gets no `mouse-face': Emacs
highlights a contiguous run of one `mouse-face' as a single region, so
highlighting row ends would join adjacent rows into one hover."
  (let ((text (propertize text
                          'keymap (mevedel-view--display-fragment-keymap
                                   mevedel-view-execution--action-map)
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
      (propertize
       (concat (substring header 0 start)
               (mevedel-view-execution--button
                toggle (mevedel-view-execution--callback 'collapse session)
                (if collapsed "RET: show background work" "RET: hide background work"))
               (substring header (+ start (length toggle))))
       'mevedel-view-zone-toggle (mevedel-view-execution--callback 'collapse session)))))

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
         (entries (append
                   (mapcar (lambda (row)
                             (list (mevedel-view-execution--key 'agent row) 'agent row))
                           agents)
                   (mapcar (lambda (item)
                             (list (mevedel-view-execution--key 'execution item)
                                   'execution item))
                           live)))
         (hover (and (or mevedel-view-execution--shown mevedel-view-execution--held)
                     (mevedel-view-execution--pointer-on-panel-p)))
         (held (mevedel-view-execution--update-held entries hover))
         (items (mapcar (lambda (entry) (cons (nth 1 entry) (nth 2 entry)))
                        (mevedel-view-execution--ordered
                         (append entries held)
                         (or mevedel-view-execution--unit-starts
                             (setq mevedel-view-execution--unit-starts
                                   (make-hash-table :test #'equal))))))
         (selected-item (cl-find-if
                         (lambda (item)
                           (and (not (plist-get (cdr item) :held-p))
                                (if (eq (car item) 'agent)
                                    (equal (plist-get (cdr item) :path)
                                           mevedel-view-execution--selected-agent)
                                  (eq (cdr item) selected-live))))
                         items))
         (shown-keys (mapcar #'car mevedel-view-execution--shown))
         (rows (cond
                (mevedel-view-execution--show-all items)
                ;; Under the pointer, keep exactly the rows already shown:
                ;; new work at capacity would otherwise evict the bottom row
                ;; and shift every other row.  The rule still counts it.
                (hover
                 (cl-remove-if-not
                  (lambda (item)
                    (or (plist-get (cdr item) :held-p)
                        (member (mevedel-view-execution--key (car item) (cdr item))
                                shown-keys)))
                  items))
                (t
                 (let ((limited (seq-take items mevedel-view-execution--row-limit)))
                   (cl-remove-if-not
                    (lambda (item)
                      (or (memq item limited) (plist-get (cdr item) :held-p)))
                    items)))))
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
      (let ((limited (seq-take
                      (cl-remove-if (lambda (item) (plist-get (cdr item) :held-p)) rows)
                      (1- mevedel-view-execution--row-limit))))
        (setq rows (cl-remove-if-not
                    (lambda (item)
                      (or (memq item limited) (eq item selected-item)
                          (plist-get (cdr item) :held-p)))
                    items))))
    (when (and mevedel-view-execution--selected (not selected-live))
      (setq rows (append rows (list (cons 'execution mevedel-view-execution--selected)))))
    ;; Only live rows actually shown can finish under the pointer; hidden
    ;; overflow, a collapsed panel and a retained finished inspector hold
    ;; nothing.
    (setq mevedel-view-execution--shown
          (unless collapsed
            (let ((live-keys (mapcar #'car entries)))
              (delq nil
                    (mapcar (lambda (item)
                              (let ((key (mevedel-view-execution--key (car item) (cdr item))))
                                (and (member key live-keys)
                                     (list key (car item) (cdr item)))))
                            rows)))))
    (when rows
      (let ((body (mevedel-view-execution--header session agents live collapsed)))
        (unless collapsed
          (dolist (item rows)
            (let* ((data (cdr item))
                   (row
                    (cond
                     ((plist-get data :held-p)
                      (mevedel-view-execution--held-line (car item) data))
                     ((eq (car item) 'agent)
                      (concat (mevedel-view-execution--agent-line session data)
                              (when (equal (plist-get data :path)
                                           mevedel-view-execution--selected-agent)
                                (mevedel-view-execution--agent-details session data))))
                     (t
                      (concat (mevedel-view-execution--line data)
                              (when (mevedel-view-execution--same-p
                                     data mevedel-view-execution--selected)
                                (mevedel-view-execution--details session data selected-live)))))))
              (setq body
                    (concat body
                            (propertize row 'mevedel-view-zone-toggle
                                        (get-text-property
                                         0 'mevedel-view-zone-activate row))))))
          (setq body (concat body (mevedel-view-execution--controls session (length items)))))
        (list :namespace 'status :id 'background :priority 50 :body body
              :keymap (mevedel-view--display-fragment-keymap)
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

(defun mevedel-view-execution--hold-terminal-facts (event)
  "Record terminal EVENT's outcome on its shown or held row.
The terminal event can precede or follow the state change that removes
the row, so both the last shown rows and the held rows take the facts."
  (when-let* ((view (mevedel-view-execution--view
                     (plist-get event :session) (plist-get event :data-buffer))))
    (with-current-buffer view
      (let* ((facts (plist-get event :facts))
             (key (list 'execution (plist-get event :owner)
                        (plist-get facts :execution-id)))
             (rendered nil))
        (dolist (entry (append mevedel-view-execution--shown
                               mevedel-view-execution--held))
          (when (equal (car entry) key)
            (let ((data (nth 2 entry)))
              (setq data (plist-put data :termination (plist-get facts :termination))
                    data (plist-put data :exit-code (plist-get facts :exit-code)))
              (setcar (nthcdr 2 entry) data)
              (when (plist-get data :held-p) (setq rendered t)))))
        (when rendered (mevedel-view--render-status))))))

(defun mevedel-view-execution--event (event)
  "Refresh only the selected inspector from passive progress or terminal EVENT."
  (when (eq (plist-get event :type) 'terminal)
    (mevedel-view-execution--hold-terminal-facts event))
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
