;;; mevedel-view-composer.el -- View composer and send orchestration -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the editable composer, prompt submission, queued follow-ups, and
;; dispatch into the authoritative gptel data buffer.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `cl-extra'
(declare-function cl-some "cl-extra" (predicate sequence &rest more-sequences))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `dnd'
(declare-function dnd-get-local-file-name "dnd" (uri &optional must-exist))
(defvar dnd-protocol-alist)

;; `gptel'
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-hooks'
(declare-function mevedel-hooks-additional-context-string "mevedel-hooks"
                  (decision &optional event))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-format-context "mevedel-hooks" (entries))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function mevedel-hooks-take-session-context "mevedel-hooks" (session))

;; `mevedel-mentions'
(declare-function mevedel-mentions-file-paths-in-text
                  "mevedel-mentions" (text))
(declare-function mevedel-mentions-install "mevedel-mentions" ())

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" ())

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-effective "mevedel-permissions"
                  (&optional session data-buffer surface-buffer))
(declare-function mevedel-permission-mode-label "mevedel-permissions"
                  (&optional mode))
(declare-function mevedel-permission-mode-transition "mevedel-permissions"
                  (mode &optional prompt display-text hook-context))
(defvar mevedel-permission-mode)

;; `mevedel-review'
(declare-function mevedel-review--mark-command-outcome
                  "mevedel-review" (outcome))
(declare-function mevedel-review-command-skill-p "mevedel-review" (skill))
(declare-function mevedel-review-transform-outcome
                  "mevedel-review" (skill-name outcome))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-fork-now
                  "mevedel-session-persistence" (buffer))
(defvar mevedel-session--fork-pending)
(defvar mevedel-session--read-only-mode)

;; `mevedel-skills'
(declare-function mevedel-skills--clear-pending-inline-attachments
                  "mevedel-skills" ())
(declare-function mevedel-skills--inline-skill-mentions
                  "mevedel-skills" (text session))
(declare-function mevedel-skills--insert-fork-result "mevedel-skills" (outcome))
(declare-function mevedel-skills--parse-skill-line "mevedel-skills" (text))
(declare-function mevedel-skills--parse-slash-line "mevedel-skills" (text))
(declare-function mevedel-skills--prepare-inline-attachments-for-text
                  "mevedel-skills" (text session callback))
(declare-function mevedel-skills--remaining-argument-hint
                  "mevedel-skills" (skill arguments))
(declare-function mevedel-skills--slash-capf
                  "mevedel-skills"
                  (buffer session local-commands &optional input-start))
(declare-function mevedel-skills--stage-inline-attachments
                  "mevedel-skills" (attachments))
(declare-function mevedel-skills-format-inline-render-data
                  "mevedel-skills" (skill arguments))
(declare-function mevedel-skills-inline-display-text
                  "mevedel-skills" (name arguments))
(declare-function mevedel-skills-install-font-lock "mevedel-skills" ())
(declare-function mevedel-skills-invoke "mevedel-skills" t t)
(defvar mevedel-slash-commands)

;; `mevedel-skills-core'
(declare-function mevedel-session-get-skill
                  "mevedel-skills-core" (session name))
(declare-function mevedel-skill-context "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-user-invocable-p
                  "mevedel-skills-core" (cl-x) t)

;; `mevedel-structs'
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-session-activate-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-add-dropped-file-grant
                  "mevedel-structs" (session path))
(declare-function mevedel-session-clear-dropped-file-grants
                  "mevedel-structs" (session))
(declare-function mevedel-session-permission-mode
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pop-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-queued-user-messages
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-queued-user-messages
                  "mevedel-structs" (session queue))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-state-dir "mevedel-structs" (workspace))
(defvar mevedel--agent-invocation nil)
(defvar mevedel--compaction-in-flight nil)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel--workspace)

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))
(declare-function mevedel--strip-hook-audit-blocks
                  "mevedel-transcript-audit" (text))

;; `mevedel-transcript'
(defvar mevedel-transcript-queued-message-reminder)

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
                  "mevedel-utilities" (start end))
(declare-function mevedel--insert-user-role-block-at-marker
                  "mevedel-utilities" (block &optional marker))
(declare-function mevedel--normalize-message-text "mevedel-utilities" (text))

;; `mevedel-view'
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-marker)

;; `mevedel-view-agent'
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-audit'
(declare-function mevedel-view--prompt-rewrite-audit-record
                  "mevedel-view-audit" (event original submitted decision))

;; `mevedel-view-history'
(declare-function mevedel-view-history-add "mevedel-view-history" (input))
(declare-function mevedel-view-history-load "mevedel-view-history"
                  (&optional session))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))

;; `mevedel-view-markdown'
(autoload 'mevedel-view--normalize-local-file-uri-path
  "mevedel-view-markdown")

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(declare-function mevedel-view--history-insertion-marker
                  "mevedel-view-render" ())
(declare-function mevedel-view--hook-context-events-from-text
                  "mevedel-view-render" (text))
(declare-function mevedel-view--inline-skill-prompt-summary-body
                  "mevedel-view-render" (render-data))
(declare-function mevedel-view--insert-rendered-tool
                  "mevedel-view-render" (rendering source))
(declare-function mevedel-view--insert-user-message
                  "mevedel-view-render"
                  (text &optional kind hook-context prompt-summary-body
                        prompt-summary-source hook-audits))
(declare-function mevedel-view-reset-agent-ephemeral-state
                  "mevedel-view-render" (&optional data-buf))
(defvar mevedel-view--display-map)

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-active-response-marker
                  "mevedel-view-stream" (info data-buffer))
(declare-function mevedel-view-stream-begin-turn
                  "mevedel-view-stream"
                  (view-turn-start data-turn-start &optional no-spinner))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--in-flight-turn-start)

;; `mevedel-workspace'
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))

;; `select'
(declare-function gui-get-selection "select" (selection-symbol target-type))


;;
;;; Customization


(defcustom mevedel-view-clipboard-image-handlers
  (list
   (list (cons :command "wl-paste")
         (cons :save (lambda (file-path)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((coding-system-for-read 'binary)
                               (exit-code
                                (call-process "wl-paste" nil (list t nil)
                                              nil "--type" "image/png")))
                           (unless (zerop exit-code)
                             (error "Command wl-paste failed with exit code %d"
                                    exit-code))
                           (let ((coding-system-for-write 'binary))
                             (write-region (point-min) (point-max)
                                           file-path nil 'silent)))))))
   (list (cons :command "pngpaste")
         (cons :save (lambda (file-path)
                       (let ((exit-code
                              (call-process "pngpaste" nil nil nil
                                            file-path)))
                         (unless (zerop exit-code)
                           (error "Command pngpaste failed with exit code %d"
                                  exit-code))))))
   (list (cons :command "xclip")
         (cons :save (lambda (file-path)
                       (when-let* ((targets (and (eq (window-system) 'x)
                                                 (gui-get-selection
                                                  'CLIPBOARD 'TARGETS)))
                                   ((vectorp targets))
                                   ((not (cl-position 'image/png targets))))
                         (error "No image/png in clipboard"))
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((exit-code
                                (call-process "xclip" nil t nil
                                              "-selection" "clipboard"
                                              "-t" "image/png" "-o")))
                           (unless (zerop exit-code)
                             (error "Command xclip failed with exit code %d"
                                    exit-code))
                           (let ((coding-system-for-write 'binary))
                             (write-region (point-min) (point-max)
                                           file-path nil 'silent)))))))
   (list (cons :command "powershell")
         (cons :save (lambda (file-path)
                       (let ((exit-code
                              (call-process
                               "powershell" nil nil nil
                               "-Command"
                               (format "& {(Get-Clipboard -Format image).Save(%s)}"
                                       (shell-quote-argument file-path)))))
                         (unless (zerop exit-code)
                           (error "Command powershell failed with exit code %d"
                                  exit-code)))))))
  "Handlers for saving a clipboard image to a file.
Each handler is an alist with `:command' and `:save'.  The first
handler whose command exists is used by `mevedel-view-yank-dwim'."
  :type '(repeat (alist :key-type keyword :value-type sexp))
  :group 'mevedel)



;;
;;; Input prompt

(defconst mevedel-view--input-prompt "> "
  "Read-only prefix rendered at the start of the input zone.")

(defun mevedel-view--effective-permission-mode ()
  "Return the permission mode to apply to the current view buffer."
  (require 'mevedel-permissions)
  (mevedel-permission-mode-effective
   (and (boundp 'mevedel--session) mevedel--session)
   (and (boundp 'mevedel--data-buffer)
        (buffer-live-p mevedel--data-buffer)
        mevedel--data-buffer)
   (current-buffer)))

(defun mevedel-view--permission-mode-display (mode)
  "Return (LABEL FACE) for permission MODE."
  (require 'mevedel-permissions)
  (list
   (mevedel-permission-mode-label mode)
   (pcase mode
     ('plan 'mevedel-view-permission-mode-plan)
     ('accept-edits 'mevedel-view-permission-mode-accept-edits)
     ('trust-all 'mevedel-view-permission-mode-trust-all)
     (_ 'mevedel-view-permission-mode-default))))

(defconst mevedel-view--permission-mode-cycle
  '(default accept-edits trust-all plan)
  "Permission modes cycled by `mevedel-view-cycle-permission-mode'.")

(defun mevedel-view--next-permission-mode (&optional mode)
  "Return the permission mode after MODE in the view cycle.
Nil and unknown modes are treated as `default'."
  (let* ((current (if (memq mode mevedel-view--permission-mode-cycle)
                      mode
                    'default))
         (tail (cdr (memq current mevedel-view--permission-mode-cycle))))
    (or (car tail)
        (car mevedel-view--permission-mode-cycle))))

(defun mevedel-view--input-prompt-string (&optional mode)
  "Return the read-only input prompt string for permission MODE.
The prompt starts with a blank separator line so status and interaction
rows remain visually distinct from the editable composer."
  (let ((mode (or mode (mevedel-view--effective-permission-mode))))
    (if (eq mode 'default)
        (propertize (concat "\n" mevedel-view--input-prompt)
                    'font-lock-face 'mevedel-view-input-prompt)
      (pcase-let* ((`(,label ,face)
                    (mevedel-view--permission-mode-display mode))
                   (text (format "\n[%s]%s%s"
                                 label
                                 (make-string (max 1 (- 6 (length label))) ?\s)
                                 mevedel-view--input-prompt))
                   (label-start 2)
                   (label-end (+ label-start (length label))))
        (add-text-properties
         0 (length text)
         '(font-lock-face mevedel-view-input-prompt)
         text)
        (add-text-properties
         label-start label-end
         `(font-lock-face ,face)
         text)
        text))))




;;
;;; Composer state and redraw preservation

(defvar-local mevedel-view--input-marker nil
  "Marker separating request progress from the input zone.
Everything above this marker is read-only history/status/interaction
chrome; everything at or below it belongs to the input zone.  The input
zone starts with the read-only prompt prefix, followed by the editable
composer body.")



(defvar-local mevedel-view--skill-argument-hint-overlay nil
  "Zero-width overlay that displays skill argument guidance in the composer.")

(defvar-local mevedel-view--prompt-hook-pending nil
  "Non-nil while a `UserPromptSubmit' hook gate is pending for this view.
This covers the interval before the prompt has been accepted and before
`mevedel--current-request' exists in the data buffer.")



(defun mevedel-view--position-in-input-region-p (position)
  "Return non-nil when POSITION is in the editable composer."
  (and (boundp 'mevedel-view--input-marker)
       (markerp mevedel-view--input-marker)
       (marker-buffer mevedel-view--input-marker)
       (not (bound-and-true-p mevedel-view--agent-transcript-p))
       (ignore-errors
         (>= position (mevedel-view--input-start)))))

(defun mevedel-view--point-in-input-region-p ()
  "Return non-nil when point is in the editable composer."
  (mevedel-view--position-in-input-region-p (point)))

(defun mevedel-view--call-preserving-input-point (thunk)
  "Call THUNK, preserving point's offset inside the composer.
Redraws for spinners, agent status, and interaction prompts insert
or delete text above the editable input.  When the user is typing in
the composer, restore point by its input-relative offset after THUNK
finishes."
  (let* ((buffer (current-buffer))
         (preserve-p (mevedel-view--point-in-input-region-p))
         (offset (and preserve-p
                      (- (point) (mevedel-view--input-start))))
         result)
    (unwind-protect
        (setq result (funcall thunk))
      (when (and preserve-p
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (and (markerp mevedel-view--input-marker)
                     (marker-buffer mevedel-view--input-marker))
            (goto-char
             (min (point-max)
                  (+ (mevedel-view--input-start)
                     (max 0 offset))))))))
    result))

(defun mevedel-view--call-preserving-input-text (thunk)
  "Call THUNK without allowing it to mutate editable composer text.
History/status rendering should only change text above
`mevedel-view--input-marker'.  This guard restores the input body if a
late callback accidentally inserts transcript content below the prompt."
  (let* ((preserve-p
          (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
               (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker)))
         (text (and preserve-p
                    (buffer-substring-no-properties
                     (mevedel-view--input-start) (point-max))))
         result)
    (let ((inhibit-modification-hooks t))
      (setq result (funcall thunk)))
    (when (and preserve-p
               (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker))
      (let* ((start (mevedel-view--input-start))
             (current (buffer-substring-no-properties start (point-max))))
        (unless (equal current text)
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (delete-region start (point-max))
            (goto-char start)
            (insert text)))))
    result))

(defun mevedel-view--call-preserving-window-state (thunk)
  "Call THUNK while preserving `window-point' and `window-start'.
Preserves those values for every window displaying the current buffer.

Used to wrap delete-and-re-render operations so the user's scroll
position and caret do not jump back to the edit site on every
progress tick.  Positions that are no longer valid after BODY (e.g.
point was inside the deleted region) are quietly clamped to the
buffer.  When point is in the editable composer, preserve it by
offset from `mevedel-view--input-start' so streaming text inserted
above the composer does not strand point in rendered transcript text."
  (let* ((mevedel-view--pww-selected-window (selected-window))
          (mevedel-view--pww-current-buffer (current-buffer))
          (mevedel-view--pww-current-point (point))
          (mevedel-view--pww-current-input-offset
           (and (mevedel-view--point-in-input-region-p)
                (- (point) (mevedel-view--input-start))))
          (mevedel-view--pww-saved
           (mapcar (lambda (w)
                     (with-current-buffer mevedel-view--pww-current-buffer
                       (let ((wp (window-point w)))
                         (list w
                               wp
                               (window-start w)
                               (and (mevedel-view--position-in-input-region-p wp)
                                    (- wp (mevedel-view--input-start)))))))
                   (get-buffer-window-list (current-buffer) nil t))))
     (prog1 (funcall thunk)
       (let ((restored-current-point
              (if (and mevedel-view--pww-current-input-offset
                       (markerp mevedel-view--input-marker)
                       (marker-buffer mevedel-view--input-marker))
                  (+ (mevedel-view--input-start)
                     (max 0 mevedel-view--pww-current-input-offset))
                mevedel-view--pww-current-point)))
         (goto-char (min (point-max) restored-current-point)))
       (dolist (entry mevedel-view--pww-saved)
         (pcase-let ((`(,w ,wp ,ws ,input-offset) entry))
           (when (window-live-p w)
             (let ((restored-point
                    (if (and input-offset
                             (markerp mevedel-view--input-marker)
                             (marker-buffer mevedel-view--input-marker))
                        (+ (mevedel-view--input-start)
                           (max 0 input-offset))
                      wp)))
               (when restored-point
                 (set-window-point w (min (point-max) restored-point)))
               (when (eq w mevedel-view--pww-selected-window)
                 (goto-char (window-point w))))
             (when (and ws (<= ws (point-max)))
               (set-window-start w ws t))))))))

(defmacro mevedel-view--preserving-window-state (&rest body)
  "Execute BODY while preserving point and window positions."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-preserving-window-state
    (lambda () ,@body)))

(defun mevedel-view--call-with-render-boundaries-advancing (thunk)
  "Call THUNK while zone boundary markers advance across insertions."
  (let ((status-type (and (markerp mevedel-view--status-marker)
                          (marker-insertion-type mevedel-view--status-marker)))
        (interaction-type
         (and (markerp mevedel-view--interaction-marker)
              (marker-insertion-type mevedel-view--interaction-marker)))
        (input-type (and (markerp mevedel-view--input-marker)
                         (marker-insertion-type mevedel-view--input-marker))))
    (unwind-protect
        (progn
          (when (markerp mevedel-view--status-marker)
            (set-marker-insertion-type mevedel-view--status-marker t))
          (when (markerp mevedel-view--interaction-marker)
            (set-marker-insertion-type mevedel-view--interaction-marker t))
          (when (markerp mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t))
          (funcall thunk))
      (when (markerp mevedel-view--status-marker)
        (set-marker-insertion-type mevedel-view--status-marker status-type))
      (when (markerp mevedel-view--interaction-marker)
        (set-marker-insertion-type mevedel-view--interaction-marker
                                   interaction-type))
      (when (markerp mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker input-type)))))

(defmacro mevedel-view--with-render-boundaries-advancing (&rest body)
  "Execute BODY while zone boundary markers advance across insertions."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-with-render-boundaries-advancing
    (lambda () ,@body)))

(defun mevedel-view--call-preserving-user-view-state (thunk)
  "Call THUNK without moving the user's live view cursor.
Async redraws may insert, delete, or reconcile view-owned text while the
user is typing in the composer or browsing transcript history.  Preserve
all displayed windows plus the editable composer text around THUNK."
  (mevedel-view--preserving-window-state
    (mevedel-view--call-preserving-input-text
     (lambda ()
       (mevedel-view--call-preserving-input-point thunk)))))


;;
;;; File input

(defun mevedel-view--file-mention-needs-braces-p (path)
  "Return non-nil when PATH needs braced @file syntax."
  (string-match-p "[ \t\n#{}\\\\]" path))

(defun mevedel-view--escape-braced-file-path (path)
  "Escape PATH for `@file:{...}' syntax."
  (with-temp-buffer
    (dotimes (index (length path))
      (let ((ch (aref path index)))
        (when (memq ch '(?\\ ?\}))
          (insert "\\"))
        (insert-char ch)))
    (buffer-string)))

(defun mevedel-view--file-mention-token (path)
  "Return the visible @file mention token for PATH."
  (format "@file:%s"
          (if (mevedel-view--file-mention-needs-braces-p path)
              (format "{%s}"
                      (mevedel-view--escape-braced-file-path path))
            path)))

(defun mevedel-view--insert-dropped-file-mentions (paths)
  "Insert @file mentions for dropped PATHS into the composer."
  (mevedel-view--ensure-interactive-chat-view)
  (let ((session (mevedel-view--session))
        tokens)
    (unless session
      (user-error "No active session for dropped files"))
    (dolist (path paths)
      (let ((expanded (expand-file-name path)))
        (push (mevedel-view--file-mention-token expanded) tokens)
        (mevedel-session-add-dropped-file-grant session expanded)))
    (setq tokens (nreverse tokens))
    (when tokens
      (when (< (point) (mevedel-view--input-start))
        (goto-char (point-max)))
      (unless (or (= (point) (mevedel-view--input-start))
                  (memq (char-before) '(?\s ?\t ?\n)))
        (insert " "))
      (insert (string-join tokens " "))
      (unless (or (eobp) (memq (char-after) '(?\s ?\t ?\n)))
        (insert " "))
      (font-lock-flush (mevedel-view--input-start) (point-max)))))

(defun mevedel-view--dnd-local-file-paths (uris)
  "Return existing regular local file paths from DND URIS.
Directories are ignored; directory-drop expansion is intentionally out
of scope for the composer."
  (let (paths)
    (dolist (uri (ensure-list uris))
      (let ((path (and (stringp uri)
                       (mevedel-view--normalize-local-file-uri-path
                        (dnd-get-local-file-name uri nil)))))
        (cond
         ((not path)
          (message "mevedel: ignored non-local drop: %s" uri))
         ((not (file-exists-p path))
          (message "mevedel: ignored missing dropped file: %s" path))
         ((file-directory-p path)
          (message "mevedel: ignored directory drop: %s" path))
         (t
          (push path paths)))))
    (nreverse paths)))

(defun mevedel-view--dnd-handle-files (uris action)
  "Handle dropped local file URIS with DND ACTION.
URIS may be a single URI string or a list of URI strings.  Some DND
paths call protocol handlers in the single-URL shape even when the
handler advertises `dnd-multiple-handler'."
  (let ((paths (mevedel-view--dnd-local-file-paths uris)))
    (when paths
      (mevedel-view--insert-dropped-file-mentions paths)
      (or action 'copy))))

(put 'mevedel-view--dnd-handle-files 'dnd-multiple-handler t)

(defun mevedel-view--media-dir ()
  "Return the workspace media directory for clipboard images."
  (let* ((session (mevedel-view--session))
         (workspace (and session (mevedel-session-workspace session))))
    (unless workspace
      (user-error "No active session for clipboard image"))
    (let ((dir (file-name-concat (mevedel-workspace-state-dir workspace)
                                 "media")))
      (make-directory dir t)
      (require 'mevedel-workspace)
      (mevedel-workspace-ensure-generated-state-ignored workspace)
      dir)))

(defun mevedel-view--clipboard-image-path (dir)
  "Return a fresh clipboard image path under DIR."
  (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
         (base (file-name-concat dir (format "clipboard-%s" stamp)))
         (path (concat base ".png"))
         (n 1))
    (while (file-exists-p path)
      (setq path (format "%s-%d.png" base n))
      (cl-incf n))
    path))

(defun mevedel-view--save-clipboard-image (&optional no-error)
  "Save a clipboard image under `.mevedel/media/'.
Return the saved image path.  When NO-ERROR is non-nil, return nil
instead of signaling when no image is available."
  (condition-case err
      (let* ((dir (mevedel-view--media-dir))
             (file-path (mevedel-view--clipboard-image-path dir))
             (handler (cl-find-if
                       (lambda (entry)
                         (executable-find (alist-get :command entry)))
                       mevedel-view-clipboard-image-handlers)))
        (cond
         ((not handler)
          (unless no-error
            (error "No clipboard image utility found")))
         (t
          (condition-case err
              (funcall (alist-get :save handler) file-path)
            (error
             (when (file-exists-p file-path)
               (delete-file file-path))
             (unless no-error
               (signal (car err) (cdr err)))))
          (cond
           ((not (file-exists-p file-path))
            (unless no-error
              (error "Clipboard image file was not created")))
           ((zerop (nth 7 (file-attributes file-path)))
            (delete-file file-path)
            (unless no-error
              (error "No image found in clipboard")))
           (t file-path)))))
    (error
     (unless no-error
       (signal (car err) (cdr err))))))

(put 'mevedel-view-yank-dwim 'delete-selection 'yank)
(defun mevedel-view-yank-dwim (&optional arg)
  "Yank text, or save a clipboard image and insert it as an `@file'.
ARG is passed through from the interactive prefix."
  (interactive "*P")
  (if-let* (((window-system))
            (path (mevedel-view--save-clipboard-image t)))
      (mevedel-view--insert-dropped-file-mentions (list path))
    (yank arg)))

(defun mevedel-view--install-dnd ()
  "Install local file drag/drop support for the current view buffer."
  (require 'dnd)
  (let (rest)
    (dolist (entry dnd-protocol-alist)
      (unless (eq (cdr entry) 'mevedel-view--dnd-handle-files)
        (push entry rest)))
    (setq-local dnd-protocol-alist
                (cons '("^file:" . mevedel-view--dnd-handle-files)
                      (nreverse rest)))))

;;
;;; Initialization

(defun mevedel-view-composer-initialize ()
  "Initialize composer editing support in the current chat view."
  (unless mevedel-view--agent-transcript-p
    (require 'mevedel-mentions)
    (require 'mevedel-skills)
    (require 'mevedel-transcript)
    (require 'mevedel-transcript-audit)
    (require 'mevedel-utilities)
    (require 'mevedel-view-history)
    (mevedel-view-history-load mevedel--session)
    (add-hook 'completion-at-point-functions
              #'mevedel-view-slash-capf nil t)
    (mevedel-mentions-install)
    (mevedel-skills-install-font-lock)
    (mevedel-view--install-dnd)
    (add-hook 'post-command-hook
              #'mevedel-view--refresh-skill-argument-hint nil t)
    (add-hook 'after-change-functions
              #'mevedel-view--refresh-skill-argument-hint-after-change
              nil t)))

;;
;;; Input forwarding

(defun mevedel-view--ensure-interactive-chat-view ()
  "Signal when the current view buffer is not an editable chat view."
  (when mevedel-view--agent-transcript-p
    (user-error "Agent transcript views are read-only")))

(defun mevedel-view--transcript-gptel-send-blocked (&optional _arg)
  "Block `gptel-send' from transcript inspection views."
  (interactive "P")
  (user-error "Agent transcript views are read-only"))


(defun mevedel-view--begin-external-turn
    (display-text data-turn-start &optional kind hook-context no-spinner)
  "Begin a view turn initiated outside the editable input.

DISPLAY-TEXT is shown as the user-side turn in the view.
DATA-TURN-START is the data-buffer marker where the assistant
response for this turn begins.  KIND may be `directive'.  HOOK-CONTEXT
is model-visible hook context to summarize in the view.  When
NO-SPINNER is non-nil, render only the local user turn."
  (mevedel-view--ensure-interactive-chat-view)
  (let ((turn-start (mevedel-view--insert-user-message
                     display-text kind hook-context)))
    (when (eq kind 'directive)
      (when-let* ((drawer (mevedel-view--external-prompt-drawer
                           data-turn-start)))
        (save-excursion
          (goto-char (mevedel-view--history-insertion-marker))
          (mevedel-view--with-render-boundaries-advancing
            (let ((inhibit-read-only t)
                  (start (point)))
              (mevedel-view--insert-rendered-tool
               (list :header "Prompt"
                     :body (plist-get drawer :body)
                     :body-mode 'markdown-mode
                     :vtype 'prompt-summary
                     :initially-collapsed-p t)
               (cons (plist-get drawer :start)
                     (plist-get drawer :end)))
              (add-text-properties
               start (point)
               `(read-only t
                 keymap ,mevedel-view--display-map
                 front-sticky (read-only keymap)
                 rear-nonsticky (read-only keymap)))
              (setq turn-start (copy-marker (point) nil)))))))
    (mevedel-view-stream-begin-turn
     turn-start data-turn-start no-spinner)))

(defun mevedel-view--external-prompt-drawer (data-turn-start)
  "Return the prompt drawer ending before DATA-TURN-START, if any."
  (when-let* (((markerp data-turn-start))
              (data-buf (marker-buffer data-turn-start))
              ((buffer-live-p data-buf)))
    (with-current-buffer data-buf
      (save-excursion
        (goto-char data-turn-start)
        (when (re-search-backward "^:PROMPT:\n" nil t)
          (let ((drawer-start (match-beginning 0))
                (body-start (match-end 0)))
            (when (re-search-forward "^:END:[ \t]*\n?"
                                     data-turn-start t)
              (list :start drawer-start
                    :end (match-end 0)
                    :body (buffer-substring-no-properties
                           body-start (match-beginning 0))))))))))

(defun mevedel-view--prompt-start-position ()
  "Return the start of the read-only input prompt, or nil."
  (when-let* ((pos (text-property-any
                    (point-min) (point-max) 'mevedel-view-prompt t)))
    (while (and (> pos (point-min))
                (get-text-property (1- pos) 'mevedel-view-prompt))
      (setq pos (1- pos)))
    pos))

(defun mevedel-view--input-marker-position ()
  "Return the recovered start position of the input prompt.
When prompt text properties survive but zone markers have drifted past
that prompt into the editable composer, repair the marker ordering so
later prompt refreshes do not operate on the draft body."
  (if-let* ((prompt-start (mevedel-view--prompt-start-position)))
      (progn
        (when (and (markerp mevedel-view--input-marker)
                   (marker-buffer mevedel-view--input-marker)
                   (not (= (marker-position mevedel-view--input-marker)
                           prompt-start)))
          (set-marker mevedel-view--input-marker prompt-start))
        (dolist (marker (list mevedel-view--status-marker
                              mevedel-view--interaction-marker))
          (when (and (markerp marker)
                     (marker-buffer marker)
                     (let ((pos (marker-position marker)))
                       (and pos (> pos prompt-start))))
            (set-marker marker prompt-start)))
        prompt-start)
    (and (markerp mevedel-view--input-marker)
         (marker-position mevedel-view--input-marker))))

(defun mevedel-view--input-start ()
  "Return the buffer position where the user's editable input begins.
This is the position immediately after the read-only `> ' prompt that
follows `mevedel-view--input-marker'."
  (save-excursion
    (goto-char (or (mevedel-view--input-marker-position)
                   mevedel-view--input-marker))
    (while (get-text-property (point) 'mevedel-view-prompt)
      (forward-char 1))
    (point)))

(defun mevedel-view-refresh-input-prompt ()
  "Refresh the input prompt to reflect the current permission mode."
  (interactive)
  (unless mevedel-view--agent-transcript-p
    (when (and (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker))
      (mevedel-view--call-preserving-input-point
       (lambda ()
         (let* ((start (mevedel-view--input-marker-position))
                (end (mevedel-view--input-start))
                (status-type
                 (and (markerp mevedel-view--status-marker)
                      (marker-insertion-type mevedel-view--status-marker)))
                (interaction-type
                 (and (markerp mevedel-view--interaction-marker)
                      (marker-insertion-type mevedel-view--interaction-marker)))
                (input-type (marker-insertion-type mevedel-view--input-marker)))
           (save-excursion
             (goto-char start)
             (unwind-protect
                 (let ((inhibit-read-only t))
                   (when (markerp mevedel-view--status-marker)
                     (set-marker-insertion-type
                      mevedel-view--status-marker nil))
                   (when (markerp mevedel-view--interaction-marker)
                     (set-marker-insertion-type
                      mevedel-view--interaction-marker nil))
                   (set-marker-insertion-type mevedel-view--input-marker nil)
                   (delete-region start end)
                   (insert (mevedel-view--input-prompt-string))
                   (add-text-properties
                    start (point)
                    `(read-only t
                      mevedel-view-prompt t
                      front-sticky (read-only mevedel-view-prompt)
                      rear-nonsticky
                      (read-only mevedel-view-prompt font-lock-face))))
               (when (markerp mevedel-view--status-marker)
                 (set-marker-insertion-type
                  mevedel-view--status-marker status-type))
               (when (markerp mevedel-view--interaction-marker)
                 (set-marker-insertion-type
                  mevedel-view--interaction-marker interaction-type))
               (set-marker-insertion-type
                mevedel-view--input-marker input-type)))))))))

(defun mevedel-view-cycle-permission-mode ()
  "Cycle the current session's permission mode from the view buffer."
  (interactive)
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer
                        (buffer-live-p mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (or (and (boundp 'mevedel--session) mevedel--session)
                      (and data-buf
                           (buffer-local-value 'mevedel--session data-buf)))))
    (unless (and data-buf session)
      (user-error "No mevedel session for permission mode cycling"))
    (let* ((current (or (mevedel-session-permission-mode session)
                        'default))
           (next (mevedel-view--next-permission-mode current)))
      (require 'mevedel-permissions)
      (with-current-buffer data-buf
        (mevedel-permission-mode-transition next))
      (mevedel-view-refresh-input-prompt)
      (pcase-let ((`(,label ,_) (mevedel-view--permission-mode-display next)))
        (message "mevedel: permission mode %s" label))
      next)))

(defun mevedel-view--input-text ()
  "Return the user's composer text, trimmed."
  (let ((text (buffer-substring-no-properties
               (mevedel-view--input-start) (point-max))))
    (string-trim text)))

(defun mevedel-view--clear-input ()
  "Clear the user's composer text, leaving the prompt in place."
  (mevedel-view--ensure-interactive-chat-view)
  (when-let* ((session (mevedel-view--session)))
    (mevedel-session-clear-dropped-file-grants session))
  (delete-region (mevedel-view--input-start) (point-max))
  (mevedel-view--delete-skill-argument-hint))

(defun mevedel-view--session ()
  "Return the session associated with the current view buffer."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-view--queued-user-messages (&optional session)
  "Return SESSION's queued user messages."
  (when-let* ((sess (or session (mevedel-view--session))))
    (mevedel-session-queued-user-messages sess)))

(defun mevedel-view--set-queued-user-messages (queue &optional session)
  "Set SESSION's queued user message QUEUE."
  (when-let* ((sess (or session (mevedel-view--session))))
    (mevedel-session-set-queued-user-messages sess queue)))

(defun mevedel-view--mentioned-file-paths (input)
  "Return expanded @file paths mentioned in INPUT."
  (require 'mevedel-mentions)
  (mevedel-mentions-file-paths-in-text input))

(defun mevedel-view--pop-dropped-file-grants-for-input (input session)
  "Consume SESSION's pending drag/drop grants referenced by INPUT."
  (when session
    (mevedel-session-pop-dropped-file-grants
     session
     (mevedel-view--mentioned-file-paths input))))

(defun mevedel-view--activate-dropped-file-grants (paths session)
  "Activate exact-file drag/drop grant PATHS for SESSION."
  (when (and session paths)
    (mevedel-session-activate-dropped-file-grants session paths)))

(defun mevedel-view--prepare-inline-attachments-before-send
    (input display-text callback &optional on-block)
  "Prepare inline `$skill' attachments in INPUT before prompt echo.
CALLBACK receives the possibly augmented model input and the display
text to render in the view."
  (let ((session (mevedel-view--session))
        (view-buffer (current-buffer))
        (data-buffer mevedel--data-buffer))
    (if (not (and session (buffer-live-p data-buffer)))
        (funcall callback input display-text)
      (cl-labels
          ((finish (outcome)
             (when (buffer-live-p view-buffer)
               (with-current-buffer view-buffer
                 (pcase (plist-get outcome :status)
                   ('ok
                    (let ((render-data
                           (and (buffer-live-p data-buffer)
                                (with-current-buffer data-buffer
                                  (mevedel-skills--stage-inline-attachments
                                   (plist-get outcome :attachments))))))
                      (when render-data
                        (funcall callback
                                 (concat input render-data)
                                 (or display-text input)))))
                   (_
                    (when (buffer-live-p data-buffer)
                      (with-current-buffer data-buffer
                        (mevedel-skills--clear-pending-inline-attachments)))
                    (message "Inline skill failed: %s"
                             (or (plist-get outcome :message)
                                 "unknown error"))
                    (when on-block
                      (funcall on-block))))))))
        (pcase (with-current-buffer data-buffer
                 (mevedel-skills--prepare-inline-attachments-for-text
                  input session #'finish))
          ('skill nil)
          ('unknown
           (when on-block
             (funcall on-block)))
          (_
           (funcall callback input display-text)))))))

(defun mevedel-view--queued-user-message-requires-transform-p
    (entry &optional session)
  "Return non-nil when queued ENTRY must go through prompt transforms."
  (or (plist-get entry :requires-request-transform)
      (let ((input (mevedel-view--queued-user-message-model-input entry)))
        (or (string-match-p "@\\(?:ref\\|file\\|agent\\|mcp\\):" input)
            (and session
                 (mevedel-skills--inline-skill-mentions input session))))))

(defun mevedel-view--queued-user-messages-require-transform-p
    (queue &optional session)
  "Return non-nil when any queued entry in QUEUE needs transforms."
  (cl-some
   (lambda (entry)
     (mevedel-view--queued-user-message-requires-transform-p entry session))
   queue))

(defun mevedel-view--queued-user-message-dropped-file-grants (queue)
  "Return deduplicated dropped-file grants recorded in queued QUEUE."
  (let (grants)
    (dolist (entry queue)
      (dolist (path (plist-get entry :dropped-file-grants))
        (unless (member path grants)
          (push path grants))))
    (nreverse grants)))

(defun mevedel-view--queued-user-message-auto-drain-blocked-p (&optional session)
  "Return non-nil when SESSION queued messages should wait for user action."
  (when-let* ((sess (or session (mevedel-view--session))))
    (or (eq (mevedel-session-permission-mode sess) 'plan)
        (mevedel-session-plan-queue sess))))

(defun mevedel-view--queued-user-message-preview (input)
  "Return a one-line preview for queued INPUT."
  (let ((preview (string-trim
                  (replace-regexp-in-string "[ \t\n\r]+" " " input t t))))
    (if (> (length preview) 96)
        (concat (substring preview 0 93) "...")
      preview)))

(defun mevedel-view--queued-user-message-edit-text (entry)
  "Return the editable composer text for queued ENTRY."
  (mevedel--normalize-message-text
   (or (plist-get entry :history-input)
       (plist-get entry :input)
       (plist-get entry :display-text)
       "")))

(defun mevedel-view--queued-user-message-model-input (entry)
  "Return the model-visible text for queued ENTRY."
  (mevedel--normalize-message-text
   (or (plist-get entry :model-input)
       (let ((input (or (plist-get entry :input)
                        (plist-get entry :display-text)
                        "")))
         (if-let* ((context (plist-get entry :hook-context)))
             (concat input "\n\n" context)
           input)))))

(defun mevedel-view--queued-user-message-batch-block (queue)
  "Return one explicit user-role batch block for queued message QUEUE."
  (let ((index 0)
        blocks)
    (dolist (entry queue)
      (cl-incf index)
      (push (format "<queued-user-message index=\"%d\">\n%s%s\n</queued-user-message>"
                    index
                    (mevedel-view--queued-user-message-model-input entry)
                    (mapconcat #'mevedel--format-hook-audit-record
                               (plist-get entry :hook-audits)
                               ""))
            blocks))
    (format "<system-reminder>
%s
</system-reminder>

<queued-user-message-batch count=\"%d\">\n%s\n</queued-user-message-batch>"
            mevedel-transcript-queued-message-reminder
            (length queue)
            (string-join (nreverse blocks) "\n\n"))))

(defun mevedel-view--queued-user-messages-body (queue)
  "Return interaction-zone body text for queued user message QUEUE."
  (let ((index 0)
        lines)
    (dolist (entry queue)
      (cl-incf index)
      (push (format "  %d. %s"
                    index
                    (mevedel-view--queued-user-message-preview
                     (mevedel-view--queued-user-message-edit-text entry)))
            lines))
    (concat "\nQueued messages\n"
            "  C-c C-e edit batch; C-c C-q clear\n"
            (string-join (nreverse lines) "\n")
            "\n")))

(defun mevedel-view--queued-user-messages-render (&optional session)
  "Render queued user messages for SESSION into the interaction zone."
  (when-let* ((queue (mevedel-view--queued-user-messages session)))
    (mevedel-view--interaction-register
     (list :kind 'queued-user-message
           :id 'queued-user-messages
           :count (length queue)
           :body (mevedel-view--queued-user-messages-body queue)
           :help-echo "Queued user messages"))))

(defun mevedel-view--queue-user-message (input)
  "Run prompt hooks for INPUT and queue the accepted prompt."
  (setq input (mevedel--normalize-message-text input))
  (let ((session (mevedel-view--session)))
    (unless session
      (user-error "No active session for queued message"))
    (mevedel-view--run-prompt-submit-hook
     input nil
     (lambda (hook-input context audits)
       (when-let* ((live-session (mevedel-view--session)))
         (let* ((hook-input (mevedel--normalize-message-text hook-input))
                (context (mevedel--normalize-message-text context))
                (model-input (if context
                                 (concat hook-input "\n\n" context)
                               hook-input))
                (dropped-file-grants
                 (mevedel-view--pop-dropped-file-grants-for-input
                  model-input live-session))
                (entry (list :input input
                             :model-input model-input
                             :display-text hook-input
                             :hook-context context
                             :hook-audits audits
                             :history-input input
                             :dropped-file-grants dropped-file-grants
                             :requires-request-transform
                             (or dropped-file-grants
                                 (string-match-p
                                  "@\\(?:ref\\|file\\|agent\\|mcp\\):"
                                  model-input))
                             :queued-at-turn
                             (or (mevedel-session-turn-count live-session) 0))))
           (mevedel-view--set-queued-user-messages
            (append (mevedel-view--queued-user-messages live-session)
                    (list entry))
            live-session)
           (mevedel-view-history-add input)
           (when (string= (mevedel-view--input-text) input)
             (mevedel-view--clear-input))
           (mevedel-view--interaction-rebuild)
           (message "mevedel: queued message for the next turn")
           (mevedel-view--schedule-late-queued-user-message-drain)
           entry)))
     (lambda ()
       (mevedel-view--interaction-rebuild)))))

(defun mevedel-view-edit-last-queued-message ()
  "Remove the queued user-message batch and restore it to the composer."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (let* ((session (mevedel-view--session))
         (queue (and session (mevedel-view--queued-user-messages session))))
    (unless queue
      (user-error "No queued messages"))
    (let ((draft (string-join
                  (mapcar #'mevedel-view--queued-user-message-edit-text queue)
                  "\n\n")))
      (mevedel-view--set-queued-user-messages nil session)
      (mevedel-view--clear-input)
      (dolist (path (mevedel-view--queued-user-message-dropped-file-grants
                     queue))
        (mevedel-session-add-dropped-file-grant session path))
      (goto-char (mevedel-view--input-start))
      (insert draft)
      (goto-char (point-max))
      (mevedel-view--interaction-rebuild)
      queue)))

(defun mevedel-view-clear-queued-messages ()
  "Clear all queued user messages for the current session."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (let ((session (mevedel-view--session)))
    (unless (and session (mevedel-view--queued-user-messages session))
      (user-error "No queued messages"))
    (mevedel-view--set-queued-user-messages nil session)
    (mevedel-view--interaction-rebuild)
    (message "mevedel: cleared queued messages")))

(defun mevedel-view--delete-skill-argument-hint ()
  "Remove the composer skill argument hint overlay."
  (when (overlayp mevedel-view--skill-argument-hint-overlay)
    (delete-overlay mevedel-view--skill-argument-hint-overlay))
  (setq mevedel-view--skill-argument-hint-overlay nil))

(defun mevedel-view--skill-argument-hint ()
  "Return display-only skill argument hint for the current composer."
  (when-let* ((session (mevedel-view--session))
              (input-start (and (markerp mevedel-view--input-marker)
                                (marker-buffer mevedel-view--input-marker)
                                (mevedel-view--input-start)))
              ((>= (point) input-start))
              (text (buffer-substring-no-properties input-start (point-max)))
              (parsed (mevedel-skills--parse-skill-line text))
              (name (nth 0 parsed))
              (skill (mevedel-session-get-skill session name))
              ((mevedel-skill-user-invocable-p skill)))
    (mevedel-skills--remaining-argument-hint skill (nth 1 parsed))))

(defun mevedel-view--refresh-skill-argument-hint ()
  "Refresh the display-only skill argument hint in the composer."
  (let* ((input-marker-pos (and (markerp mevedel-view--input-marker)
                                (marker-buffer mevedel-view--input-marker)
                                (marker-position mevedel-view--input-marker)))
         (marker-at-prompt-p
          (and input-marker-pos
               (< input-marker-pos (point-max))
               (get-text-property input-marker-pos 'mevedel-view-prompt))))
    (cond
     ((or mevedel-view--agent-transcript-p
          (not input-marker-pos))
      (mevedel-view--delete-skill-argument-hint))
     ((and marker-at-prompt-p (< (point) input-marker-pos))
      (mevedel-view--delete-skill-argument-hint))
     ((< (point) (mevedel-view--input-start))
      (mevedel-view--delete-skill-argument-hint))
     (t
      (let ((hint (mevedel-view--skill-argument-hint)))
        (if (and hint (not (string-empty-p hint)))
            (progn
              (unless (overlayp mevedel-view--skill-argument-hint-overlay)
                (setq mevedel-view--skill-argument-hint-overlay
                      (make-overlay (point) (point) (current-buffer) nil t))
                (overlay-put mevedel-view--skill-argument-hint-overlay
                             'priority 10))
              (move-overlay mevedel-view--skill-argument-hint-overlay
                            (point) (point) (current-buffer))
              (overlay-put
               mevedel-view--skill-argument-hint-overlay
               'after-string
               (propertize (concat " " hint) 'font-lock-face 'shadow)))
          (mevedel-view--delete-skill-argument-hint)))))))

(defun mevedel-view--refresh-skill-argument-hint-after-change
    (&rest _ignore)
  "Refresh the skill argument hint after composer edits."
  (mevedel-view--refresh-skill-argument-hint))

(defun mevedel-view-slash-capf ()
  "Completion-at-point for slash commands and `$' skills in the composer.
Offers local slash commands at `/name', session skills at `$name',
and command argument completion for commands with finite choices."
  (when (and mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer)
             (>= (point) (mevedel-view--input-start)))
    (let ((session (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer)))
      (mevedel-skills--slash-capf
       mevedel--data-buffer session mevedel-slash-commands
       (mevedel-view--input-start)))))

(defun mevedel-view--start-fork-skill-turn
    (input display-text &optional hook-context)
  "Render and record a fork skill INPUT without calling `gptel-send'.

DISPLAY-TEXT is shown in the view for the user turn.  INPUT is written
to the data buffer as the authoritative user prompt.  The data-turn
marker is anchored after that prompt so the eventual fork result can be
rendered by the normal post-response hook.  HOOK-CONTEXT is summarized
in the view when present."
  (let ((view-turn-start
         (mevedel-view--insert-user-message display-text nil hook-context)))
    (mevedel-view--clear-input)
    (with-current-buffer mevedel--data-buffer
      (when mevedel--session
        (mevedel-request-begin
         mevedel--session
         (and (boundp 'mevedel--current-directive-uuid)
              mevedel--current-directive-uuid)))
      (goto-char (point-max))
      (let ((user-turn-start (point)))
        (insert gptel-response-separator)
        (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
          (let ((prefix-length (length prefix)))
            (unless (and (>= (point) (+ (point-min) prefix-length))
                         (string= (buffer-substring-no-properties
                                   (- (point) prefix-length) (point))
                                  prefix))
              (unless (bolp) (insert "\n"))
              (insert prefix))))
        (insert input "\n")
        (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
      (let ((data-turn-start (copy-marker (point) nil)))
        (with-current-buffer mevedel--view-buffer
          (mevedel-view-stream-begin-turn
           view-turn-start data-turn-start))))))

(defun mevedel-view--finish-fork-skill-outcome
    (name outcome view-buffer data-buffer &optional skill)
  "Handle fork skill OUTCOME for NAME."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (when (and (fboundp 'mevedel-review-command-skill-p)
               (mevedel-review-command-skill-p skill)
               (fboundp 'mevedel-review--mark-command-outcome))
      (setq outcome (mevedel-review--mark-command-outcome outcome)))
    (when (fboundp 'mevedel-review-transform-outcome)
      (setq outcome (mevedel-review-transform-outcome name outcome)))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (mevedel-skills--insert-fork-result outcome)))
         (_
          (message "Skill '%s' returned unsupported outcome: %S"
                   name outcome))))
      (_
       (with-current-buffer view-buffer
         (mevedel-view--stop-request-progress)
         (message "Skill '%s' failed: %s"
                  name
                  (or (plist-get outcome :message)
                      "unknown error")))
       (with-current-buffer data-buffer
         (when (bound-and-true-p mevedel--current-request)
           (mevedel-request-end))
         (gptel--update-status " Ready" 'success))))))

(defun mevedel-view--send-fork-skill
    (input name args skill display-text view-buffer data-buffer)
  "Run hooks and dispatch fork SKILL from user INPUT.
NAME and ARGS identify the user invocation; DISPLAY-TEXT is shown in the view.
VIEW-BUFFER and DATA-BUFFER are the paired session buffers."
  (mevedel-view--run-prompt-submit-hook
   input display-text
   (lambda (hook-input hook-context hook-audits)
     (when (and (buffer-live-p view-buffer)
                (buffer-live-p data-buffer))
       (if (not (equal hook-input input))
           (let ((model-input (if hook-context
                                  (concat hook-input "\n\n" hook-context)
                                hook-input)))
             (mevedel-view--forward-input
              model-input hook-input
              (lambda ()
                (mevedel-view-history-add hook-input)
                (mevedel-view--fork-if-pending))
              t nil hook-context hook-audits))
         (mevedel-view-history-add input)
         (mevedel-view--fork-if-pending)
         (mevedel-view--start-fork-skill-turn
          input display-text hook-context)
         (with-current-buffer data-buffer
           (mevedel-skills-invoke
            skill args
            (lambda (outcome)
              (mevedel-view--finish-fork-skill-outcome
               name outcome view-buffer data-buffer skill))
            :trigger 'user-skill
            :additional-context hook-context)))))))

(defun mevedel-view--finish-inline-skill-outcome
    (input name args skill display-text outcome view-buffer data-buffer)
  "Handle inline skill OUTCOME and then run `UserPromptSubmit'.
INPUT, NAME, ARGS, SKILL, and DISPLAY-TEXT describe the user invocation.
VIEW-BUFFER and DATA-BUFFER are the paired session buffers."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('inline
          (let* ((body (or (plist-get outcome :body)
                           (format "Skill '%s' produced no body." name)))
                 (skill-audits (plist-get outcome :hook-audits))
                 (render-data
                  (mevedel-skills-format-inline-render-data
                   skill
                   (or (plist-get outcome :arguments) args)))
                 (send-body
                  (lambda (hook-input context audits)
                    (let ((view-context
                           (mevedel-view--join-hook-contexts
                            (mevedel-hooks-format-context
                             (mevedel-view--hook-context-events-from-text
                              hook-input))
                            context))
                          (all-audits
                           (append skill-audits audits)))
                      (mevedel-view--forward-input
                       (concat (if context
                                   (concat hook-input "\n\n" context)
                                 hook-input)
                               render-data)
                       display-text
                       (lambda ()
                         (mevedel-view-history-add input)
                         (mevedel-view--fork-if-pending))
                       t nil view-context all-audits)))))
            (with-current-buffer view-buffer
              (mevedel-view--run-prompt-submit-hook
               body display-text send-body
                (lambda ()
                  (with-current-buffer data-buffer
                    (setq-local mevedel-skills--pending-request-context
                                nil)))))))
         (_
          (message "Skill '%s' returned unsupported outcome: %S"
                   name outcome))))
      (_
       (with-current-buffer view-buffer
         (message "Skill '%s' failed: %s"
                  name
                  (or (plist-get outcome :message)
                      "unknown error")))))))

(defun mevedel-view--send-inline-skill
    (input name args skill display-text view-buffer data-buffer)
  "Expand inline SKILL, then run prompt hooks on the model-visible body.
INPUT, NAME, ARGS, and DISPLAY-TEXT describe the user invocation.
VIEW-BUFFER and DATA-BUFFER are the paired session buffers."
  (with-current-buffer data-buffer
    (mevedel-skills-invoke
     skill args
     (lambda (outcome)
       (mevedel-view--finish-inline-skill-outcome
        input name args skill display-text outcome view-buffer data-buffer))
     :trigger 'user-skill)))

(defun mevedel-view--send-skill (input name args skill)
  "Dispatch user skill NAME with ARGS from INPUT."
  (let* ((fork-p (eq (mevedel-skill-context skill) 'fork))
         (view-buffer (current-buffer))
         (data-buffer mevedel--data-buffer)
         (display-text (if fork-p
                           (concat "$" name (when args (concat " " args)))
                         (mevedel-skills-inline-display-text name args))))
    (if fork-p
        (mevedel-view--send-fork-skill
         input name args skill display-text view-buffer data-buffer)
      (mevedel-view--send-inline-skill
       input name args skill display-text view-buffer data-buffer))))

(defun mevedel-view-send ()
  "Send the current composer text to the LLM via the data buffer.
Extracts text from the input zone, renders it in the history region,
forwards it to the data buffer, and calls `gptel-send'.  When the
input starts with a `/command' or known `$skill', dispatches it instead
of forwarding to the LLM.

If the data buffer is in rewind preview
state (`mevedel-session--fork-pending' is set), materialize the fork
just before the send actually reaches the LLM so empty input, unknown
slash commands, and local-only slash commands do not spuriously create a
fork."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (unless mevedel--data-buffer
    (user-error "No data buffer associated with this view"))
  (unless (buffer-live-p mevedel--data-buffer)
    (user-error "Data buffer has been killed"))
  (when mevedel-view--prompt-hook-pending
    (user-error "A prompt hook is still running -- wait or abort first"))
  (when (buffer-local-value 'mevedel--compaction-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  (when (buffer-local-value 'mevedel-session--read-only-mode
                            mevedel--data-buffer)
    (user-error "Session is open read-only (another host holds the lock)"))
  (let ((input (mevedel-view--input-text)))
    (when (string-empty-p input)
      (user-error "Nothing to send"))
    (let* ((slash-parsed (mevedel-skills--parse-slash-line input))
           (skill-parsed (mevedel-skills--parse-skill-line input))
           (skill (and skill-parsed
                       (with-current-buffer mevedel--data-buffer
                         (and (bound-and-true-p mevedel--session)
                              (mevedel-session-get-skill
                               mevedel--session (nth 0 skill-parsed)))))))
      (if (buffer-local-value 'mevedel--current-request mevedel--data-buffer)
          (if (or slash-parsed skill)
              (user-error "A request is already active -- wait or abort first")
            (mevedel-view--queue-user-message input))
        (cond
         (slash-parsed
          (let* ((name (nth 0 slash-parsed))
                 (args (nth 1 slash-parsed))
                 (local (assoc name mevedel-slash-commands)))
            (cond
             ((and local
                   (string= name "plan")
                   args
                   (not (string-blank-p args)))
              (mevedel-view--send-local-plan input args))
             (local
              (let ((result (with-current-buffer mevedel--data-buffer
                              (funcall (cdr local) args))))
                ;; Most local slash commands don't send a turn.  A command may
                ;; return this sentinel when it took ownership of the input.
                (unless (eq result 'mevedel-view-sent)
                  (when (stringp result)
                    (message "%s" result))
                  (mevedel-view-history-add input)
                  (mevedel-view--clear-input))))
             (t
              (message "Unknown slash command: /%s" name)))))
         (skill
          (mevedel-view--send-skill
           input (nth 0 skill-parsed) (nth 1 skill-parsed) skill))
         (t
          (mevedel-view--forward-input
           input nil
           (lambda ()
             (mevedel-view-history-add input)
             (mevedel-view--fork-if-pending))))))))
  ;; Ensure point ends up in the input zone.
  (goto-char (point-max)))

(defun mevedel-view--send-local-plan (input args)
  "Run pre-send check and dispatch local `/plan' with ARGS.
INPUT is the original composer text, including the slash command."
  (let ((view-buffer (current-buffer))
        (data-buffer mevedel--data-buffer))
    (mevedel-view--run-prompt-submit-hook
     args input
     (lambda (hook-input context _audits)
       (when (and (buffer-live-p view-buffer)
                  (buffer-live-p data-buffer))
         (with-current-buffer view-buffer
           (mevedel-view-history-add input)
           (mevedel-view--fork-if-pending)
           (mevedel-view--clear-input))
         (with-current-buffer data-buffer
           (require 'mevedel-permissions)
           (mevedel-permission-mode-transition
            'plan
            (if context
                (concat hook-input "\n\n" context)
              hook-input)
            hook-input
            context)))))))

(defun mevedel-view--fork-if-pending ()
  "Materialize the fork if the data buffer is in rewind preview state.
No-op otherwise.  Shared safety net for any path that actually sends
a turn to the LLM."
  (when (buffer-local-value 'mevedel-session--fork-pending mevedel--data-buffer)
    (require 'mevedel-session-persistence)
    (mevedel-view-reset-agent-ephemeral-state)
    (mevedel-session-persistence-fork-now mevedel--data-buffer)))

(defun mevedel-view--safe-hook-decision (event decision)
  "Return plist-shaped hook DECISION for EVENT, or nil.

Prompt hook callbacks run from process sentinels and can be backed by
user/project code.  Treat malformed values as no decision so symbols
such as `passed' cannot escape into `plist-get' or `plist-member'."
  (if (and (listp decision)
           (or (null decision)
               (keywordp (car-safe decision))))
      decision
    (display-warning
     'mevedel
     (format "Ignoring malformed %s hook decision: %S" event decision)
     :warning)
    nil))

(defun mevedel-view--take-pending-hook-context (session)
  "Return and clear SESSION's pending hook context as model-visible XML."
  (mevedel-hooks-take-session-context session))

(defun mevedel-view--join-hook-contexts (&rest contexts)
  "Return CONTEXTS joined as separate hook context blocks."
  (let ((contexts (delq nil contexts)))
    (when contexts
      (mapconcat #'identity contexts "\n\n"))))

(defun mevedel-view--run-prompt-submit-hook
    (input display-text callback &optional blocked-callback)
  "Run `UserPromptSubmit' for INPUT, then call CALLBACK if accepted.
DISPLAY-TEXT is the user-facing prompt text.  CALLBACK receives
`(HOOK-INPUT CONTEXT AUDITS)'."
  (mevedel-view--ensure-interactive-chat-view)
  (when mevedel-view--prompt-hook-pending
    (user-error "A prompt hook is still running -- wait or abort first"))
  (let ((view-buffer (current-buffer))
        (data-buffer mevedel--data-buffer))
    (unless (and data-buffer (buffer-live-p data-buffer))
      (user-error "Data buffer has been killed"))
    (setq mevedel-view--prompt-hook-pending t)
    (condition-case err
        (with-current-buffer data-buffer
          (require 'mevedel-hooks)
          (let ((session mevedel--session)
                (workspace mevedel--workspace))
            (mevedel-hooks-run-event
             'UserPromptSubmit
             (mevedel-hooks-event-plist
              'UserPromptSubmit session workspace
              :prompt input
             :display-text display-text)
             (lambda (decision)
               (when (buffer-live-p view-buffer)
                 (with-current-buffer view-buffer
                   (setq mevedel-view--prompt-hook-pending nil)
                   (when (buffer-live-p data-buffer)
                     (setq decision
                           (mevedel-view--safe-hook-decision
                            'UserPromptSubmit decision))
                     (cond
                      ((and (plist-member decision :continue)
                            (not (plist-get decision :continue)))
                       (when blocked-callback
                         (funcall blocked-callback))
                       (message "mevedel: prompt blocked by hook: %s"
                                (or (plist-get decision :stop-reason)
                                    "no reason provided")))
                      (t
                       (when-let* ((msg (plist-get decision :system-message)))
                         (message "mevedel: %s" msg))
                       (let* ((submitted
                               (if (stringp (plist-get decision :updated-input))
                                   (plist-get decision :updated-input)
                                 input))
                              (context
                               (mevedel-view--join-hook-contexts
                                (mevedel-view--take-pending-hook-context
                                 session)
                                (mevedel-hooks-additional-context-string
                                 decision 'UserPromptSubmit)))
                              (audit
                               (mevedel-view--prompt-rewrite-audit-record
                                'UserPromptSubmit input submitted decision)))
                         (funcall
                          callback
                          submitted
                          context
                          (and audit (list audit))))))))))
             session workspace nil nil)))
      (error
       (setq mevedel-view--prompt-hook-pending nil)
       (signal (car err) (cdr err))))))

(defun mevedel-view--forward-input
    (input &optional display-text before-send prompt-checked on-block
           hook-context hook-audits)
  "Render INPUT in the history region, forward to the data buffer, and send.
Helper for `mevedel-view-send'.  When DISPLAY-TEXT is non-nil, show
that in the view instead of INPUT (e.g., compact skill invocation).
Optional BEFORE-SEND is called after prompt hooks allow the send but
before any user-visible prompt or data-buffer prompt is inserted.  When
PROMPT-CHECKED is non-nil, skip `UserPromptSubmit' because the caller
already ran it.  ON-BLOCK is called when a prompt hook blocks.
HOOK-CONTEXT and HOOK-AUDITS are summarized in the view when
PROMPT-CHECKED is non-nil.

Anchors the incremental-render markers so progress hooks can redraw
the in-flight assistant turn as tool calls complete:
`mevedel-view--in-flight-turn-start' points into the view just above
the input zone (where the assistant turn will be rendered);
`mevedel-view--data-turn-start' points into the data buffer just
after the forwarded prompt, where the LLM's response will begin."
  (cl-labels
      ((send-now (model-input view-text context audits)
         (mevedel-view--prepare-inline-attachments-before-send
          model-input view-text
          (lambda (prepared-input prepared-display)
            (when before-send
              (funcall before-send))
            (mevedel-view--forward-input-now
             prepared-input prepared-display context audits))
          on-block)))
    (if prompt-checked
        (send-now input (or display-text input) hook-context hook-audits)
      (mevedel-view--run-prompt-submit-hook
       input display-text
       (lambda (hook-input context audits)
         (send-now
          (if context
              (concat hook-input "\n\n" context)
            hook-input)
          (or display-text hook-input)
          context
          audits))
       on-block))))

(defun mevedel-view--forward-input-now
    (input &optional display-text hook-context hook-audits)
  "Forward INPUT to gptel immediately, after prompt hooks have run.
DISPLAY-TEXT is shown in the view instead of INPUT when non-nil.
HOOK-CONTEXT and HOOK-AUDITS are summarized in the view when present."
  (mevedel-view--ensure-interactive-chat-view)
  (when (buffer-local-value 'mevedel--compaction-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  (let* ((input (mevedel--normalize-message-text input))
         (display-text (and display-text
                            (mevedel--normalize-message-text display-text)))
         (prompt-summary-body
          (mevedel-view--inline-skill-prompt-summary-body input))
         (session (mevedel-view--session))
         (dropped-file-grants
          (mevedel-view--pop-dropped-file-grants-for-input
           input session)))
    (let (data-turn-start
          hook-audits-with-source
          prompt-summary-source)
      ;; Forward to the data buffer first so immediate inline-skill
      ;; Prompt handles can expand through the same source-backed fold
      ;; path as a full rerender.
      (with-current-buffer mevedel--data-buffer
        (goto-char (point-max))
        (let ((user-turn-start (point))
              body-start)
          ;; Insert response separator
          (insert gptel-response-separator)
          ;; Insert prompt prefix if needed (e.g., org heading marker)
          (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
            (let ((prefix-length (length prefix)))
              (unless (and (>= (point) (+ (point-min) prefix-length))
                           (string= (buffer-substring-no-properties
                                     (- (point) prefix-length) (point))
                                    prefix))
                (unless (bolp) (insert "\n"))
                (insert prefix))))
          (setq body-start (point))
          (insert input "\n")
          (setq prompt-summary-source
                (and prompt-summary-body
                     (cons body-start (point))))
          (mevedel--clear-user-turn-gptel-properties
           user-turn-start (point)))
        (dolist (audit hook-audits)
          (let ((audit-start (point)))
            (insert (mevedel--format-hook-audit-record audit))
            (push (append audit
                          (list :source (cons audit-start (point))))
                  hook-audits-with-source)))
        (setq hook-audits-with-source (nreverse hook-audits-with-source))
        ;; Anchor the data-side marker after the forwarded prompt so
        ;; incremental renders extract only the in-flight assistant
        ;; segments from here forward.  Pushed onto the view buffer's
        ;; buffer-local so it is readable from `--render-incremental'
        ;; without switching buffers.
        (setq data-turn-start (copy-marker (point) nil)))
      ;; Render the user's message in the view after the data source is
      ;; known, but before the model request starts.
      (let ((turn-start
             (mevedel-view--insert-user-message
              (or display-text input) nil hook-context
              prompt-summary-body prompt-summary-source
              hook-audits-with-source)))
        (mevedel-view-stream-begin-turn turn-start data-turn-start)
        ;; Clear composer text.
        (mevedel-view--clear-input))
      (with-current-buffer mevedel--data-buffer
        (mevedel-view--activate-dropped-file-grants
         dropped-file-grants session)
        (gptel-send)))))

(defun mevedel-view--insert-queued-user-message-batch
    (data-buffer block &optional marker)
  "Insert queued-message batch BLOCK into DATA-BUFFER's transcript.

When MARKER is live in DATA-BUFFER, insert at MARKER and advance it
so the in-flight assistant response lands after the synthetic user
batch."
  (when (and (buffer-live-p data-buffer)
             (stringp block)
             (not (string-empty-p block)))
    (condition-case err
        (with-current-buffer data-buffer
          (let ((inhibit-read-only t))
            (mevedel--insert-user-role-block-at-marker block marker)))
      (error
       (message "mevedel: queued batch transcript insert failed: %S"
                err)))))

(defun mevedel-view--agent-fsm-p (info data-buffer)
  "Return non-nil when INFO or DATA-BUFFER belongs to an agent request."
  (or (and (fboundp 'mevedel-agent-invocation-p)
           (mevedel-agent-invocation-p
            (plist-get info :mevedel-agent-invocation)))
      (and (buffer-live-p data-buffer)
           (with-current-buffer data-buffer
             (bound-and-true-p mevedel--agent-invocation)))))

(defun mevedel-view--handle-queued-user-message-inject (fsm)
  "Drain queued follow-up batches into FSM's request at WAIT state.

The queue entries were already accepted by `UserPromptSubmit' when
they were queued.  This handler injects all currently queued entries
as a single user-role batch before `gptel--handle-wait' sends the next
HTTP request, then commits the batch by clearing the editable queue."
  (when-let* ((info (and fsm (fboundp 'gptel-fsm-info)
                         (gptel-fsm-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer))
              (session (buffer-local-value 'mevedel--session data-buffer))
              (queue (mevedel-session-queued-user-messages session))
              ((not (mevedel-view--queued-user-message-auto-drain-blocked-p
                     session)))
              ((not (with-current-buffer data-buffer
                      (mevedel-view--queued-user-messages-require-transform-p
                       queue session))))
              ((not (mevedel-view--agent-fsm-p info data-buffer)))
              (data (plist-get info :data)))
    (let* ((block (mevedel-view--queued-user-message-batch-block queue))
           (model-block (mevedel--strip-hook-audit-blocks block)))
      (gptel--inject-prompt
       (plist-get info :backend) data
       (list :role "user"
             :content model-block))
      (mevedel-view--insert-queued-user-message-batch
       data-buffer block
       (mevedel-view-stream-active-response-marker info data-buffer))
      (mevedel-view--set-queued-user-messages nil session)
      (when-let* ((view-buffer (buffer-local-value 'mevedel--view-buffer
                                                   data-buffer))
                  ((buffer-live-p view-buffer)))
        (with-current-buffer view-buffer
          (if (and (boundp 'mevedel--data-buffer)
                   (eq mevedel--data-buffer data-buffer))
              (mevedel-view--full-rerender)
            (mevedel-view--interaction-rebuild)))))))

(defun mevedel-view--drain-queued-user-message-batch (data-buffer)
  "Submit queued user messages for DATA-BUFFER, if no WAIT drained them."
  (when (buffer-live-p data-buffer)
    (let* ((view-buffer (buffer-local-value 'mevedel--view-buffer data-buffer))
           (session (buffer-local-value 'mevedel--session data-buffer)))
      (when (and session
                 (buffer-live-p view-buffer)
                 (not (buffer-local-value 'mevedel--current-request
                                          data-buffer)))
        (with-current-buffer view-buffer
          (when (and (not mevedel-view--agent-transcript-p)
                     (not mevedel-view--prompt-hook-pending)
                     (not (mevedel-view--queued-user-message-auto-drain-blocked-p
                           session))
                     (string-empty-p (mevedel-view--input-text)))
            (when-let* ((queue (mevedel-view--queued-user-messages session)))
              (let ((block (mevedel-view--queued-user-message-batch-block
                            queue))
                    (dropped-file-grants
                     (mevedel-view--queued-user-message-dropped-file-grants
                      queue)))
                (mevedel-view--forward-input
                 block block
                 (lambda ()
                   (mevedel-view--set-queued-user-messages nil session)
                   (mevedel-view--interaction-rebuild)
                   (mevedel-view--fork-if-pending)
                   (mevedel-view--activate-dropped-file-grants
                    dropped-file-grants session))
                 t
                 (lambda ()
                   (mevedel-view--interaction-rebuild)))))))))))

(defun mevedel-view--run-queued-user-message-drain (data-buffer)
  "Run queued user-message batch drain for DATA-BUFFER if it is live."
  (when (buffer-live-p data-buffer)
    (mevedel-view--drain-queued-user-message-batch data-buffer)))

(defun mevedel-view--schedule-late-queued-user-message-drain ()
  "Schedule a fallback drain when queueing completes after request cleanup."
  (when-let* ((data-buffer mevedel--data-buffer)
              ((buffer-live-p data-buffer))
              ((not (buffer-local-value 'mevedel--current-request
                                        data-buffer))))
    (run-at-time 0 nil
                 #'mevedel-view--run-queued-user-message-drain
                 data-buffer)))

(defun mevedel-view--schedule-queued-user-message-drain (fsm)
  "Schedule queued user-message batch drain after FSM completes successfully."
  (when-let* ((info (and fsm (fboundp 'gptel-fsm-info)
                         (gptel-fsm-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer)))
    (run-at-time 0 nil
                 #'mevedel-view--run-queued-user-message-drain
                 data-buffer)))

(defun mevedel-view-abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--stop-request-progress)
  (when-let* ((data-buf mevedel--data-buffer)
              (_ (buffer-live-p data-buf)))
    ;; Delegate to mevedel-abort which handles the full teardown
    (with-current-buffer data-buf
      (when (fboundp 'mevedel-abort)
        (funcall #'mevedel-abort)))))




(provide 'mevedel-view-composer)
;;; mevedel-view-composer.el ends here
