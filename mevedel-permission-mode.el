;;; mevedel-permission-mode.el -- Permission mode lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns permission-mode normalization, session scoping, transitions, and the
;; reminder/UI lifecycle attached to mode changes.

;;; Code:

(eval-when-compile
  (require 'mevedel-structs))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode-active-p
                  "mevedel-plan-mode" (&optional session))
(declare-function mevedel-plan-mode-exit
                  "mevedel-plan-mode" (&optional session))
(autoload 'mevedel-plan-mode-active-p "mevedel-plan-mode")
(autoload 'mevedel-plan-mode-exit "mevedel-plan-mode")

;; `mevedel-reminders'
(declare-function mevedel-reminders-make-full-auto-mode
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-full-auto-mode-exit
                  "mevedel-reminders" ())
(declare-function mevedel-session-ensure-reminder
                  "mevedel-reminders" (session reminder))
(declare-function mevedel-session-remove-reminder
                  "mevedel-reminders" (session type))
(autoload 'mevedel-reminders-make-full-auto-mode "mevedel-reminders")
(autoload 'mevedel-reminders-make-full-auto-mode-exit "mevedel-reminders")
(autoload 'mevedel-session-ensure-reminder "mevedel-reminders")
(autoload 'mevedel-session-remove-reminder "mevedel-reminders")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-mutation-authority
                  "mevedel-session-artifacts" (session &optional buffer))
(autoload 'mevedel-session-artifacts-assert-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-skills-ui'
(declare-function mevedel-view-refresh-associated-input-prompt
                  "mevedel-view-composer" ())

;; `mevedel-structs'
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)


(defun mevedel-permission-mode-data-buffer ()
  "Return the session data buffer reachable from `current-buffer', or nil.
The current buffer itself qualifies when it carries a live
`mevedel--session'; otherwise follow its `mevedel--data-buffer'
back-pointer (set on view buffers and derived buffers) to find the
authoritative data buffer.  Returns nil for any buffer not tied to a
session -- Customize UI, `*scratch*', init-file load, etc."
  (let ((cur (current-buffer)))
    (cond
     ((and (boundp 'mevedel--session)
           (buffer-local-value 'mevedel--session cur))
      cur)
     ((let ((db (and (boundp 'mevedel--data-buffer)
                     (buffer-local-value 'mevedel--data-buffer cur))))
        (and db (buffer-live-p db)
             (boundp 'mevedel--session)
             (buffer-local-value 'mevedel--session db)
             db))))))

(defvar mevedel-permission-mode--raw-set nil
  "Non-nil while setting permission mode without transition lifecycle.")

(defun mevedel-permission-mode-normalize (mode)
  "Return canonical permission MODE.
Only values valid in configuration and persisted state are accepted."
  (let ((mode (cond
               ((symbolp mode) mode)
               ((stringp mode) (intern (string-trim mode)))
               (t mode))))
    (if (memq mode '(ask edits full-auto))
        mode
      (user-error "Unknown permission mode: %s" mode))))

(defun mevedel-permission-mode-set-raw (mode)
  "Set permission MODE in the current scope without transition lifecycle.
This is for lifecycle helpers that already know which mode side effects
they are responsible for."
  (let ((mode (mevedel-permission-mode-normalize mode))
        (mevedel-permission-mode--raw-set t))
    (setopt mevedel-permission-mode mode)
    mode))

(defun mevedel-permission-mode--effective-session-mode (session)
  "Return SESSION's effective permission mode."
  (or (mevedel-session-permission-mode session)
      (and (boundp 'mevedel-permission-mode) mevedel-permission-mode)
      'ask))

(defun mevedel-permission-mode-effective
    (&optional session data-buffer surface-buffer)
  "Return the effective permission mode for SESSION and DATA-BUFFER.
SURFACE-BUFFER is the UI buffer whose local mode may override the data
buffer's fallback mode.  When omitted, DATA-BUFFER is used; without a
DATA-BUFFER, the current buffer is used."
  (let* ((surface-buffer (or surface-buffer
                             (and (buffer-live-p data-buffer) data-buffer)
                             (current-buffer)))
         (surface-mode
          (and (buffer-live-p surface-buffer)
               (buffer-local-value 'mevedel-permission-mode
                                   surface-buffer)))
         (surface-local
          (and (buffer-live-p surface-buffer)
               (local-variable-p 'mevedel-permission-mode surface-buffer)))
         (global-mode (and (boundp 'mevedel-permission-mode)
                           (default-toplevel-value
                            'mevedel-permission-mode))))
    (or (and session (mevedel-session-permission-mode session))
        (and (buffer-live-p data-buffer)
             (with-current-buffer data-buffer
               (and (boundp 'mevedel--session)
                    mevedel--session
                    (mevedel-session-permission-mode mevedel--session))))
        (and surface-local surface-mode)
        (and (buffer-live-p data-buffer)
             (with-current-buffer data-buffer
               (and (boundp 'mevedel-permission-mode)
                    mevedel-permission-mode)))
        global-mode
        'ask)))

(defun mevedel-permission-mode-label (&optional mode)
  "Return the compact user-facing label for permission MODE."
  (symbol-name (if (memq mode '(ask edits full-auto)) mode 'ask)))

(defun mevedel-permission-mode--apply-full-auto-lifecycle
    (previous-mode target-mode &optional session)
  "Synchronize full-auto reminders for PREVIOUS-MODE -> TARGET-MODE.
SESSION defaults to the current data buffer's session."
  (let* ((previous-mode (mevedel-permission-mode-normalize previous-mode))
         (target-mode (mevedel-permission-mode-normalize target-mode))
         (data-buf (mevedel-permission-mode-data-buffer))
         (session (or session
                      (and data-buf
                           (buffer-local-value 'mevedel--session data-buf)))))
    (when session
      (cond
       ((eq target-mode 'full-auto)
        (mevedel-session-remove-reminder session 'full-auto-mode-exit)
        (mevedel-session-ensure-reminder
         session (mevedel-reminders-make-full-auto-mode)))
       (t
        (mevedel-session-remove-reminder session 'full-auto-mode)
        (when (eq previous-mode 'full-auto)
          (mevedel-session-ensure-reminder
           session (mevedel-reminders-make-full-auto-mode-exit))))))))

(defun mevedel-permission-mode-transition (mode)
  "Transition the current session to permission MODE.
Runs mode-specific lifecycle hooks."
  (let* ((target (mevedel-permission-mode-normalize mode))
         (data-buf (mevedel-permission-mode-data-buffer))
         (session (and data-buf
                       (buffer-local-value 'mevedel--session data-buf))))
    (if (not session)
        (set-default-toplevel-value 'mevedel-permission-mode target)
      (with-current-buffer data-buf
        (mevedel-session-artifacts-assert-mutation-authority
         session data-buf)
        (when (and (fboundp 'mevedel-plan-mode-active-p)
                   (mevedel-plan-mode-active-p session))
          (mevedel-plan-mode-exit session))
        (let ((previous (mevedel-permission-mode--effective-session-mode
                         session)))
          (mevedel-permission-mode-set-raw target)
          (mevedel-permission-mode--apply-full-auto-lifecycle
           previous target session)
          ;; Permission mode can change before any view exists; the
          ;; composer module loads with the view, so a missing function
          ;; here just means there is no prompt to refresh yet.
          (when (fboundp 'mevedel-view-refresh-associated-input-prompt)
            (ignore-errors
              (mevedel-view-refresh-associated-input-prompt))))))
    target))

(defun mevedel-permission-mode-set-session-scoped (sym val slot-setter)
  "Scoped `:set' helper for session-backed customizations.

Generic setter body for a defcustom that shadows a `mevedel-session'
slot: when the change is made from inside a session, the session slot is
updated and the defcustom's global default is left alone; when made from
anywhere else, the global default is updated so subsequent sessions pick
it up.

SYM is the defcustom symbol.  VAL is the new value.  SLOT-SETTER is a
function `(SESSION VAL) -> _' that writes VAL into the appropriate
session struct slot via `setf'; it is the only per-variable knob, making
this helper reusable across any session-backed setting.

Scope resolution:
  - `current-buffer' carries a session (data buffer), or its
    `mevedel--data-buffer' back-pointer reaches one (view buffer): only
    that session is touched -- SLOT-SETTER updates the slot, SYM is set
    buffer-locally in the data buffer and its view buffer so
    `describe-variable' reports the same value in either buffer.  Other
    sessions and the global default remain unchanged.
  - Otherwise (Customize UI, `use-package :custom', `setopt' from a
    non-session buffer): `set-default-toplevel-value' installs the new
    default for future sessions; no sessions are touched.

Fires on `setopt', `customize-set-variable', `custom-set-variables',
`use-package :custom', and the Customize UI.  Plain `setq' and
`setq-local' bypass this setter entirely."
  (let* ((data-buf (mevedel-permission-mode-data-buffer))
         (session (and data-buf
                       (buffer-local-value 'mevedel--session data-buf))))
    ;; The defcustom's initial evaluation runs this setter with the
    ;; standard value, and the owning file may first load from inside a
    ;; session buffer -- an execution helper is one such path.  The
    ;; symbol must gain its global default regardless of scope, or it
    ;; stays void for the rest of the process everywhere outside that
    ;; one session.
    (unless (default-boundp sym)
      (set-default-toplevel-value sym val))
    (if session
        (progn
          (funcall slot-setter session val)
          (with-current-buffer data-buf
            (set (make-local-variable sym) val))
          (when-let* ((vb (buffer-local-value 'mevedel--view-buffer data-buf))
                      ((buffer-live-p vb)))
            (with-current-buffer vb
              (set (make-local-variable sym) val))))
      (set-default-toplevel-value sym val))))

(defun mevedel-permission-mode--set (sym val)
  "Set SYM to VAL for `mevedel-permission-mode'.

Thin wrapper around `mevedel-permission-mode-set-session-scoped' that
targets the session struct's `permission-mode' slot.  See that helper's
docstring for the full scoping contract."
  (setq val (mevedel-permission-mode-normalize val))
  (cond
   ((not (featurep 'mevedel-permission-mode))
    (set-default-toplevel-value sym val))
   (mevedel-permission-mode--raw-set
    (mevedel-permission-mode-set-session-scoped
     sym val
     (lambda (session v)
       (setf (mevedel-session-permission-mode session) v))))
   (t
    (let ((data-buf (mevedel-permission-mode-data-buffer)))
      (if data-buf
          (with-current-buffer data-buf
            (mevedel-permission-mode-transition val))
        (set-default-toplevel-value sym val))))))

(defun mevedel-permission-mode-get-session-scoped (sym slot-getter)
  "Return SYM through its session-scoped SLOT-GETTER.

When `current-buffer' reaches a session (directly or via
`mevedel--data-buffer' back-pointer), returns the value produced by
SLOT-GETTER called on that session -- so Customize widgets and tooling
that consult `:get' reflect the session-scoped value.  Otherwise returns
the global default for SYM.

SLOT-GETTER is a function `(SESSION) -> VALUE' reading the relevant
session struct slot."
  (let* ((data-buf (mevedel-permission-mode-data-buffer))
         (session (and data-buf
                       (buffer-local-value 'mevedel--session data-buf))))
    (if session
        (funcall slot-getter session)
      (default-toplevel-value sym))))

(defun mevedel-permission-mode--get (sym)
  "Return SYM's session-scoped `mevedel-permission-mode' value.

Returns the current session's `permission-mode' slot when the call is
made from inside a session; otherwise returns the global default."
  (mevedel-permission-mode-get-session-scoped
   sym #'mevedel-session-permission-mode))

(defcustom mevedel-permission-mode 'ask
  "Current permission mode.

Controls the default permission behavior when no explicit rules match.

  `ask'       - Allow recognized inspection and prompt for edits,
                uncertain Bash, and Eval.
  `edits'     - Apply native edits inside allowed roots automatically;
                Bash and Eval retain their normal checks.
  `full-auto' - Skip heuristic Bash and Eval prompts and run live Eval
                automatically.  Explicit denies and missing protected
                resource authority remain effective.

At the generic permission layer, `edits' authorizes tools in the native
`edit' group after their resource boundary is satisfied.  It does not
authorize Bash, Eval, or unrelated mutating tools.  Native edit previews
also apply without an interactive overlay in `edits'; `ask' prompts.

To change this mode at runtime, use `setopt' from the relevant buffer:
when called from inside a session buffer (a data buffer or its view
buffer) the change is scoped to that session only -- other open
sessions keep their current mode and the global default is left
untouched.  When called from any other buffer, the global default is
updated so future sessions pick it up.

The Customize UI is a global-write path in Emacs by design: opening
`customize-variable' or `customize-option' from a session buffer
switches into a dedicated `*Customize ...*' buffer, so at commit time
`current-buffer' is the Customize buffer and no session is in scope.
Changes made through the Customize UI therefore always update the
global default, never the current session.  Use `setopt' (or the
session UI) for session-scoped
changes.

Plain `setq' / `setq-local' bypass this path entirely and tool
execution reads the session slot first, so it would keep using the
old value.  See `mevedel-permission-mode--set' and
`mevedel-permission-mode-set-session-scoped'."
  :type '(choice
          (const :tag "Ask -- prompt for edits and uncertain execution" ask)
          (const :tag "Edits -- apply native edits, check Bash and Eval" edits)
          (const :tag "Full Auto -- skip heuristic execution prompts" full-auto))
  :set #'mevedel-permission-mode--set
  :get #'mevedel-permission-mode--get
  :local 'permanent
  :group 'mevedel)


;;
;;; Goal implementation override

(defvar-local mevedel--implementation-permission-mode-saved nil
  "Wrapped permission mode to restore after Goal implementation.")

(defun mevedel--implementation-permission-mode-apply (mode)
  "Temporarily apply implementation permission MODE for this request."
  (when (and (memq mode '(ask edits full-auto))
             (bound-and-true-p mevedel--session))
    (setq mevedel--implementation-permission-mode-saved
          (list (mevedel-session-permission-mode mevedel--session)))
    (mevedel-permission-mode-set-raw mode)
    (mevedel-view-refresh-associated-input-prompt)))

(defun mevedel--implementation-permission-mode-restore ()
  "Restore permission mode after a temporary Goal implementation override."
  (when (and mevedel--implementation-permission-mode-saved
             (bound-and-true-p mevedel--session))
    (let ((restore (car mevedel--implementation-permission-mode-saved)))
      (setq mevedel--implementation-permission-mode-saved nil)
      (setf (mevedel-session-permission-mode mevedel--session) restore)
      (if restore
          (setq-local mevedel-permission-mode restore)
        (kill-local-variable 'mevedel-permission-mode))
      (when (and (boundp 'mevedel--view-buffer)
                 (buffer-live-p mevedel--view-buffer))
        (with-current-buffer mevedel--view-buffer
          (if restore
              (setq-local mevedel-permission-mode restore)
            (kill-local-variable 'mevedel-permission-mode))))
      (mevedel-view-refresh-associated-input-prompt))))


;;
;;; Mode decisions

(defun mevedel-permission-mode-decision
    (mode read-only-p &optional native-edit-p reviewed-edit-p)
  "Determine permission from MODE and the tool's capability flags.
READ-ONLY-P identifies inspection tools.  NATIVE-EDIT-P identifies tools in
the native `edit' group; it never applies to Bash or Eval.  REVIEWED-EDIT-P
identifies edits whose handler supplies mandatory approval in `ask' mode.

Returns `allow' or `ask': a mode decides between automatic allowance and
a prompt, and every hard denial is an absolute policy upstream."
  (pcase mode
    ('full-auto 'allow)
    ('edits (if (or read-only-p native-edit-p) 'allow 'ask))
    ('ask (if (or read-only-p reviewed-edit-p) 'allow 'ask))
    ;; Unknown mode: fall through to ask
    (_ 'ask)))

(provide 'mevedel-permission-mode)
;;; mevedel-permission-mode.el ends here
