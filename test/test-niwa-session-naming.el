;;; test-niwa-session-naming.el --- Session title tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise naming through real session metadata and controlled model replies.

;;; Code:

(require 'ert)
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name
                                  byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-sandbox)
(require 'niwa-session-artifacts)
(require 'niwa-session-naming)
(require 'niwa-session-publication)
(require 'niwa-skills-ui)

(defmacro test-niwa-session-naming--with-session (&rest body)
  "Run BODY with an unnamed SESSION and its root BUFFER in a temp workspace."
  (declare (indent 0))
  `(pcase-let* ((`(,workspace . ,directory)
                 (test-niwa-session-persistence--make-tempdir-workspace))
                (session (niwa-session-create nil workspace))
                (buffer (generate-new-buffer " *test-session-naming*")))
     (unwind-protect
         (with-current-buffer buffer
           (org-mode)
           (setq-local niwa--session session
                       niwa--workspace workspace)
           (setf (niwa-session-root-buffer session) buffer)
           (insert "Fix the session chooser\n")
           ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (niwa-session-naming-cancel)))
       (test-niwa-session-persistence--release-and-kill buffer session)
       (delete-directory directory t)
       (niwa-workspace-clear-registry))))

(defmacro test-niwa-session-naming--with-telemetry (events &rest body)
  "Run BODY recording `niwa-telemetry-record' calls into EVENTS.
Each recorded event is a plist carrying its `:event' beside its props, the
same shape the real filter persists."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'niwa-telemetry-record)
              (lambda (_session event &rest props)
                (push (append (list :event event) props) ,events))))
     ,@body))

(defun test-niwa-session-naming--naming-props (events)
  "Return the single `session-naming' event among EVENTS."
  (let ((naming (cl-remove-if-not
                 (lambda (event) (eq 'session-naming (plist-get event :event)))
                 events)))
    (should (= 1 (length naming)))
    (car naming)))

(defun test-niwa-session-naming--telemetry (session)
  "Return SESSION's persisted telemetry entries, or nil."
  (let ((log (and (niwa-session-save-path session)
                  (file-name-concat (niwa-session-save-path session)
                                    "telemetry-log.el"))))
    (when (and log (file-exists-p log))
      (with-temp-buffer
        (insert-file-contents log)
        (goto-char (point-min))
        (let (entries)
          (condition-case nil
              (while t (push (read (current-buffer)) entries))
            (end-of-file))
          (nreverse entries))))))

(defmacro test-niwa-session-naming--with-engine (calls &rest body)
  "Run BODY recording isolated title requests into CALLS, newest first.
Each call is a plist of :backend, :model, :effort, :prompt, :callback,
:ready and :buffer; its canceller adds :cancelled."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'niwa-engine-request-text)
              (lambda (backend prompt _system callback &optional _stream _context ready)
                (let ((call (list :backend backend :model gptel-model
                                  :effort gptel-reasoning-effort :prompt prompt
                                  :callback callback :ready ready
                                  :buffer (current-buffer) :cancelled nil)))
                  (push call ,calls)
                  (lambda () (plist-put call :cancelled t))))))
     ,@body))

(defmacro test-niwa-session-naming--with-primary (primary &rest body)
  "Run BODY with the naming workload resolved to a distinct PRIMARY backend."
  (declare (indent 1))
  `(let ((,primary (gptel--make-backend :name "naming-primary")))
     (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                (lambda (&rest _)
                  (list :backend ,primary :model 'naming-model :effort nil))))
       ,@body)))

(defmacro test-niwa-session-naming--await (calls count)
  "Wait until CALLS holds COUNT title requests."
  `(with-timeout (3 (ert-fail "Title request did not start"))
     (while (< (length ,calls) ,count)
       (accept-process-output nil 0.01))))

(defun test-niwa-session-naming--issue (session)
  "Return SESSION's retained naming-failure notice, or nil."
  (cl-find "session-naming" (niwa-session-recovery-issues session)
           :key (lambda (issue) (plist-get issue :id)) :test #'equal))

(niwa-deftest niwa-session-naming-committed-authored-preview (:quiet t)
  ,test (test)
  :doc "commits only authored previews and never latches a harness delivery"
  (test-niwa-session-naming--with-session
    (erase-buffer)
    (dolist (type '(MAIL RESULT EXECUTION MONITOR MONITOR_TERMINAL))
      (niwa--insert-user-role-block-at-marker
       (niwa-tools--message-delivery-block
        (list :type type :sender "/root/worker" :recipient "/root"
              :outcome 'completed :payload "Harness delivery, not a prompt"))))
    (niwa-session-artifacts-save session buffer)
    (let* ((path (niwa-session-save-path session))
           (sidecar-path (niwa-session-artifacts-sidecar-path path)))
      (should-not (niwa-session-checkpoint-first-user-message session))
      (should-not (plist-get (niwa-session-codec-read sidecar-path)
                             :first-user-message))
      (goto-char (point-max))
      (insert "First authored request\n")
      (should (equal "First authored request"
                     (niwa-session-persistence-first-user-message buffer)))
      ;; An acknowledged sidecar cannot commit a preview ahead of its text.
      (niwa-session-persistence-save-agent-state session)
      (should-not (niwa-session-checkpoint-first-user-message session))
      (should-not (plist-get (niwa-session-codec-read sidecar-path)
                             :first-user-message))
      (niwa-session-artifacts-save session buffer)
      (should (equal "First authored request"
                     (niwa-session-checkpoint-first-user-message session)))
      (should (equal "First authored request"
                     (plist-get (niwa-session-codec-read sidecar-path)
                                :first-user-message)))
      (goto-char (point-max))
      (insert (propertize "Answer\n" 'gptel 'response))
      (niwa--insert-user-role-block-at-marker
       (niwa-tools--message-delivery-block
        '(:type MAIL :sender "/root/worker" :recipient "/root"
          :payload "Later harness delivery")))
      (goto-char (point-max))
      (insert "Later authored request\n")
      (niwa-session-persistence-save-agent-state session)
      (should (equal "First authored request"
                     (niwa-session-checkpoint-first-user-message session)))
      (niwa-session-artifacts-save session buffer)
      (test-niwa-session-persistence--release-and-kill buffer session)
      (setq buffer (niwa-session-persistence-restore path nil nil workspace)
            session (buffer-local-value 'niwa--session buffer))
      (should (equal "First authored request"
                     (niwa-session-checkpoint-first-user-message session)))
      (should (equal "First authored request"
                     (plist-get (niwa-session-codec-read sidecar-path)
                                :first-user-message))))))

(niwa-deftest niwa-session-naming-normalize ()
  ,test (test)
  :doc "preserves readable Unicode and collapses whitespace"
  (should (equal "Ändere die Sitzung"
                 (niwa-session-naming-normalize "  Ändere\n die\tSitzung  ")))
  :doc "rejects a blank display name"
  (should-error (niwa-session-naming-normalize " \n\t") :type 'user-error))

(niwa-deftest niwa-session-naming-consider (:quiet t)
  ,test (test)
  :doc "isolates the naming workload and persists a bounded title without moving files"
  (test-niwa-session-naming--with-session
    (let ((gptel-request-function (symbol-function 'gptel-request))
          (resolve (symbol-function 'niwa-model-resolve-workload))
          (original-id (niwa-session-session-id session))
          (authored (concat "Fix the session chooser " (make-string 2200 ?x)))
          reply request-buffer captured workload)
      (setq-local gptel-model 'gpt-4o-mini
                  gptel-reasoning-effort nil)
      (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                 (lambda (value &rest args)
                   (setq workload value)
                   (apply resolve value args)))
                ((symbol-function 'gptel-request)
                 (lambda (prompt &rest options)
                   (setq reply (plist-get options :callback)
                         request-buffer (current-buffer)
                         captured (list prompt options))
                   ;; Use the real gptel serializer without sending a request.
                   (let ((fsm (apply gptel-request-function prompt
                                     :dry-run t options)))
                     (should-not (plist-get (gptel-fsm-info fsm) :tools)))
                   (should-not gptel-use-context)
                   (should-not gptel-track-response)
                   (should-not (plist-get options :transforms)))))
        (niwa-session-naming-consider session authored)
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (should (eq workload 'naming))
        (should (= 2000 (length (car captured))))
        (should-not (eq buffer request-buffer))
        (let ((old-path (niwa-session-save-path session))
              (file buffer-file-name)
              (transcript (buffer-string)))
          (funcall reply "\"Fix the session chooser\"" nil)
          (should (equal "Fix the session chooser" (niwa-session-name session)))
          (should (equal original-id (niwa-session-session-id session)))
          (should (equal old-path (niwa-session-save-path session)))
          (should (equal file buffer-file-name))
          (should (equal transcript (buffer-string)))
          (should-not (buffer-live-p request-buffer))
          (should-not niwa-session-naming--cancel)
          (let* ((sidecar (niwa-session-codec-read
                           (niwa-session-artifacts-sidecar-path old-path)))
                 (restored (plist-get (niwa-session-codec-deserialize sidecar workspace) :session)))
            (should (equal "Fix the session chooser" (niwa-session-name restored)))
            (should (eq 'attempted (niwa-session-naming-state restored))))))))
  :doc "failed, cancelled and stale replies retain the fallback or manual title"
  (dolist (mode '(failure blank oversized manual cancelled killed timeout))
    (test-niwa-session-naming--with-session
      (let ((original-id (niwa-session-session-id session))
            (timer-function (symbol-function 'run-at-time))
            reply request-buffer timeout calls)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (time repeat function &rest args)
                     (if (equal time 30)
                         (progn (setq timeout function) nil)
                       (apply timer-function time repeat function args))))
                  ((symbol-function 'gptel-request)
                   (lambda (_prompt &rest options)
                     (setq calls (1+ (or calls 0))
                           request-buffer (current-buffer)
                           reply (plist-get options :callback)))))
          (niwa-session-naming-consider session "Fix this")
          (with-timeout (3 (ert-fail "Title request did not start"))
            (while (null reply) (accept-process-output nil 0.01)))
          (pcase mode
            ('failure (funcall reply nil nil))
            ('blank (funcall reply " \n " nil))
            ('oversized (funcall reply (make-string 4097 ?x) nil))
            ('manual (niwa-rename-session "My title"))
            ('cancelled (niwa-session-naming-cancel))
            ('killed (set-buffer-modified-p nil) (kill-buffer buffer))
            ('timeout (funcall timeout)))
          (funcall reply "Stale title" nil)
			(when (memq mode '(failure blank oversized timeout))
			  (should (cl-find "session-naming" (niwa-session-recovery-issues session)
					   :key (lambda (issue) (plist-get issue :id)) :test #'equal))
			  (should-not (niwa-recovery-blocker session)))
          (should (equal (if (eq mode 'manual) "My title" original-id)
                         (niwa-session-name session)))
          (should-not (buffer-live-p request-buffer))
          (niwa-session-naming-consider session "Second attempt")
          (should (= 1 calls))
          (should (eq (if (eq mode 'manual) 'explicit 'attempted)
                      (niwa-session-naming-state session)))))))
  :doc "cancellation during a save prevents an orphan inference request"
  (test-niwa-session-naming--with-session
    (let ((save (symbol-function 'niwa-session-artifacts-save))
          saved sent)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest args)
                   (prog1 (apply save args)
                     (setq saved t)
                     (niwa-session-naming-cancel))))
                ((symbol-function 'gptel-request)
                 (lambda (&rest _) (setq sent t))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Naming was not scheduled"))
          (while (not saved) (accept-process-output nil 0.01)))
        (should-not sent)
        (should-not niwa-session-naming--cancel))))
  :doc "manual rename during authority I/O wins over an automatic title"
  (test-niwa-session-naming--with-session
    (let ((assert-authority (symbol-function 'niwa-session-artifacts-assert-mutation-authority))
          reply injected)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options) (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
                   (lambda (&rest args)
                     (prog1 (apply assert-authority args)
                       (unless injected
                         (setq injected t)
                         (niwa-rename-session "Manual wins"))))))
          (funcall reply "Automatic loses" nil))
        (should injected)
        (should (equal "Manual wins" (niwa-session-name session))))))
  :doc "waits for publication before applying a valid title"
  (test-niwa-session-naming--with-session
    (let ((niwa-transport-retry-seconds 0.01) reply)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options) (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (unwind-protect
            (progn
              (setf (niwa-session-publication-active-p session) t)
              (funcall reply "Deferred title" nil)
              (should niwa-session-naming--cancel)
              (should (equal (niwa-session-session-id session)
                             (niwa-session-name session))))
          (setf (niwa-session-publication-active-p session) nil))
        (with-timeout (3 (ert-fail "Title did not apply after publication"))
          (while niwa-session-naming--cancel (accept-process-output nil 0.01)))
        (should (equal "Deferred title" (niwa-session-name session))))))
  :doc "generated titles obey four-word and character limits in either delivery mode"
  (dolist (stream '(nil t))
    (dolist (example '(("  \"Ändere\n die\tSitzung jetzt bitte\"  " . "Ändere die Sitzung jetzt")
                       ("One two three four five six" . "One two three four")
                       ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" .
                        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567")))
      (test-niwa-session-naming--with-session
        (let (reply)
          (setq-local gptel-stream stream)
          (cl-letf (((symbol-function 'gptel-request)
                     (lambda (_prompt &rest options)
                       (setq reply (plist-get options :callback)))))
            (niwa-session-naming-consider session "Name this task")
            (with-timeout (3 (ert-fail "Title request did not start"))
              (while (null reply) (accept-process-output nil 0.01)))
            (funcall reply (car example) (and stream '(:stream t)))
            (when stream (funcall reply t nil))
            (should (equal (cdr example) (niwa-session-name session))))))))
  :doc "explicit names and transient conversations do not request inference"
  (test-niwa-session-naming--with-session
    (setf (niwa-session-naming-state session) 'explicit)
    (niwa-session-naming-consider session "Fix this")
    (should-not niwa-session-naming--cancel)
    (setf (niwa-session-naming-state session) 'pending
          (niwa-session-audit-session session) (niwa-session-create "parent" workspace))
    (niwa-session-naming-consider session "Fix this")
    (should-not niwa-session-naming--cancel)))

(niwa-deftest niwa-session-naming-clear (:quiet t)
  ,test (test)
  :doc "clear preserves the title across resume and names the latest segment from new input"
  (test-niwa-session-naming--with-session
    (let (reply)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix old task")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (funcall reply "Old task" nil)
        (let ((id (niwa-session-session-id session))
              (path (niwa-session-save-path session)))
          (should (equal "Fix old task" (niwa-session-naming-input session)))
          (niwa-cmd--clear nil)
          (should (eq 'pending (niwa-session-naming-state session)))
          (should-not (niwa-session-naming-input session))
          (should (equal "Old task" (niwa-session-name session)))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (setq buffer (niwa-session-persistence-restore path nil nil workspace)
                session (buffer-local-value 'niwa--session buffer))
          (with-current-buffer buffer
            (should (= 2 (niwa-session-current-segment session)))
            (should (equal id (niwa-session-session-id session)))
            (should (equal "Old task" (niwa-session-name session)))
            (should (eq 'pending (niwa-session-naming-state session)))
            (setq reply nil)
            (niwa-session-naming-consider session "Implement new task")
            (with-timeout (3 (ert-fail "Replacement title request did not start"))
              (while (null reply) (accept-process-output nil 0.01)))
            (funcall reply "New task" nil)
            (should (equal "New task" (niwa-session-name session)))
            (should (equal path (niwa-session-save-path session)))
            (should (eq 'attempted (niwa-session-naming-state session))))))))
  :doc "explicit names survive clear and are not limited to four words"
  (test-niwa-session-naming--with-session
    (niwa-session-artifacts-save session buffer)
    (niwa-rename-session "This is my deliberately longer session name")
    (niwa-cmd--clear nil)
    (should (equal "This is my deliberately longer session name"
                   (niwa-session-name session)))
    (should (eq 'explicit (niwa-session-naming-state session)))
    (niwa-session-naming-consider session "Different task")
    (should-not niwa-session-naming--cancel))
  :doc "clear cancels old streamed or deferred replies and failed replacement retains the title"
  (dolist (deferred '(nil t))
    (test-niwa-session-naming--with-session
      (let (reply request-buffer)
        (setq-local gptel-stream t)
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (_prompt &rest options)
                     (setq reply (plist-get options :callback)
                           request-buffer (current-buffer)))))
          (niwa-session-naming-consider session "Old task")
          (with-timeout (3 (ert-fail "Title request did not start"))
            (while (null reply) (accept-process-output nil 0.01)))
          (let ((old-reply reply)
                (old-buffer request-buffer)
                (old-title (niwa-session-name session)))
            (funcall old-reply "Old title" '(:stream t))
            (when deferred
              (unwind-protect
                  (progn
                    (setf (niwa-session-publication-active-p session) t)
                    (funcall old-reply t nil))
                (setf (niwa-session-publication-active-p session) nil)))
            (niwa-cmd--clear nil)
            (funcall old-reply t nil)
            (should-not (buffer-live-p old-buffer))
            (should (equal old-title (niwa-session-name session)))
            (setq reply nil)
            (niwa-session-naming-consider session "New task")
            (with-timeout (3 (ert-fail "Replacement request did not start"))
              (while (null reply) (accept-process-output nil 0.01)))
            (funcall old-reply "Stale replacement" nil)
            (funcall reply nil nil)
            (should (equal old-title (niwa-session-name session)))
            (should (eq 'attempted (niwa-session-naming-state session)))
            (niwa-cmd--clear nil)
            (should (eq 'pending (niwa-session-naming-state session))))))))
  :doc "cancelled and confirmed nonmaterialized clear preserve manual ownership"
  (dolist (state '(pending attempted explicit))
    (test-niwa-session-naming--with-session
      (setf (niwa-session-naming-state session) state
            (niwa-session-naming-input session) (and (eq state 'attempted) "Old input"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (niwa-cmd--clear nil))
      (should (eq state (niwa-session-naming-state session)))
      (should (equal (and (eq state 'attempted) "Old input")
                     (niwa-session-naming-input session)))
      (should (equal "Fix the session chooser\n" (buffer-string)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (niwa-cmd--clear nil))
      (should (eq (if (eq state 'explicit) 'explicit 'pending)
                  (niwa-session-naming-state session)))
      (should-not (niwa-session-naming-input session))
      (should-not (niwa-session-save-path session))))
  :doc "failed sidecar publication preserves the old segment and naming attempt"
  (test-niwa-session-naming--with-session
    (niwa-session-artifacts-save session buffer)
    (setf (niwa-session-naming-state session) 'attempted
          (niwa-session-naming-input session) "Old input")
    (niwa-session-artifacts-save session buffer)
    (let ((text (buffer-string))
          (path buffer-file-name)
          (sidecar (lambda ()
                     (car (read-from-string
                           (niwa-session-artifacts-read-artifact
                            session "session.meta.el" t)))))
          (head (plist-get (niwa-session-publication session) :head))
          injected)
      (should (eq 'attempted (plist-get (funcall sidecar) :naming-state)))
      (should (equal "Old input" (plist-get (funcall sidecar) :naming-input)))
      (cl-letf (((symbol-function 'niwa-session-publication--publish-artifact)
                 (lambda (_artifact)
                   (setq injected t)
                   (signal 'file-error '("Injected sidecar publication failure")))))
        (should-error (niwa-cmd--clear nil) :type 'file-error))
      (should injected)
      (should (equal head (plist-get (niwa-session-publication session) :head)))
      (should (= 1 (niwa-session-current-segment session)))
      (should (equal path buffer-file-name))
      (should (equal text (buffer-string)))
      (should (eq 'attempted (niwa-session-naming-state session)))
      (should (equal "Old input" (niwa-session-naming-input session)))
      (should (eq 'attempted
                  (plist-get (funcall sidecar) :naming-state)))
      (should (equal "Old input"
                     (plist-get (funcall sidecar) :naming-input)))))
  :doc "a post-commit presentation error cannot roll back the new naming state"
  (test-niwa-session-naming--with-session
    (niwa-session-artifacts-save session buffer)
    (setf (niwa-session-naming-state session) 'attempted
          (niwa-session-naming-input session) "Old input")
    (let ((notify (symbol-function 'niwa-session-persistence-notify-session-event)))
      (cl-letf (((symbol-function 'niwa-session-persistence-notify-session-event)
                 (lambda (owner event &rest args)
                   (if (eq event 'rerender)
                       (error "Injected presentation failure")
                     (apply notify owner event args)))))
        (should-error (niwa-cmd--clear nil))))
    (should (= 2 (niwa-session-current-segment session)))
    (should (eq 'pending (niwa-session-naming-state session)))
    (should-not (niwa-session-naming-input session))
    (let ((sidecar (niwa-session-codec-read
                    (niwa-session-artifacts-sidecar-path
                     (niwa-session-save-path session)))))
      (should (= 2 (plist-get sidecar :current-segment)))
      (should (eq 'pending (plist-get sidecar :naming-state))))))

(niwa-deftest niwa-session-naming-stream ()
  ,test (test)
  :doc "inherits the root buffer's streaming choice and collects streamed chunks"
  (dolist (stream '(t nil))
    (test-niwa-session-naming--with-session
      (let ((original-id (niwa-session-session-id session))
            reply options request-buffer)
        (setq-local gptel-stream stream)
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (_prompt &rest request-options)
                     (setq options request-options
                           request-buffer (current-buffer)
                           reply (plist-get request-options :callback)))))
          (niwa-session-naming-consider session "Fix this")
          (with-timeout (3 (ert-fail "Title request did not start"))
            (while (null reply) (accept-process-output nil 0.01)))
          (should (eq stream (plist-get options :stream)))
          (should (eq stream (buffer-local-value 'gptel-stream request-buffer)))
          (if stream
              (progn
                (funcall reply "Fix the " (list :stream t))
                (funcall reply "session chooser" (list :stream t))
                (should (equal original-id (niwa-session-name session)))
                (funcall reply t nil))
            (funcall reply "Fix the session chooser" nil))
          (with-timeout (3 (ert-fail "Title did not apply"))
            (while (equal original-id (niwa-session-name session))
              (accept-process-output nil 0.01)))
          (should (equal "Fix the session chooser" (niwa-session-name session)))
          (should-not niwa-session-naming--cancel)))))
  :doc "a streamed title response is bounded and aborts its request"
  (test-niwa-session-naming--with-session
    (let ((original-id (niwa-session-session-id session))
          reply request-buffer)
      (setq-local gptel-stream t)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest request-options)
                   (setq request-buffer (current-buffer)
                         reply (plist-get request-options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (funcall reply (make-string 4097 ?x) (list :stream t))
        (should (equal original-id (niwa-session-name session)))
        (should-not (buffer-live-p request-buffer))
        (should-not niwa-session-naming--cancel)))))

(niwa-deftest niwa-session-naming-telemetry (:quiet t)
  ,test (test)
  :doc "classifies a provider failure and records no provider text"
  (test-niwa-session-naming--with-session
    (let (reply events)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (test-niwa-session-naming--with-telemetry events
          (funcall reply nil
                   (list :status "HTTP/2 400"
                         :error (list :type "invalid_request_error"
                                      :code "stream_required"
                                      :message "stream must be true"))))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'provider-error (plist-get props :outcome)))
          (should (eq 'provider (plist-get props :error-class)))
          (should (equal "HTTP/2 400" (plist-get props :provider-status)))
          (should (equal "invalid_request_error"
                         (plist-get props :provider-error-type)))
          (should (equal "stream_required"
                         (plist-get props :provider-error-code)))
          (should-not (plist-member props :error))
          (should-not (string-match-p "must be true" (format "%S" props)))))))
  :doc "writes the classified failure through the real telemetry filter"
  (test-niwa-session-naming--with-session
    (let (reply)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (funcall reply nil
                 (list :status "HTTP/2 400"
                       :error (list :type "invalid_request_error"
                                    :code "stream_required"
                                    :message "stream must be true")))
        (let ((event (test-niwa-session-naming--naming-props
                      (test-niwa-session-naming--telemetry session))))
          (should (eq 'provider-error (plist-get event :outcome)))
          (should (eq 'provider (plist-get event :error-class)))
          (should (equal "HTTP/2 400" (plist-get event :provider-status)))
          (should (equal "invalid_request_error"
                         (plist-get event :provider-error-type)))
          (should (equal "stream_required"
                         (plist-get event :provider-error-code)))
          (should-not (plist-get event :dropped-keys))
          (should-not (string-match-p "must be true" (format "%S" event)))))))
  :doc "records an unstructured provider error by class and status alone"
  (test-niwa-session-naming--with-session
    (let (reply events)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (test-niwa-session-naming--with-telemetry events
          (funcall reply nil (list :status "Curl failed with exit code 6"
                                   :error "Curl failure")))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'provider-error (plist-get props :outcome)))
          (should (eq 'provider (plist-get props :error-class)))
          (should (equal "Curl failed with exit code 6"
                         (plist-get props :provider-status)))
          (should-not (plist-get props :provider-error-type))
          (should-not (plist-get props :provider-error-code))))))
  :doc "separates an aborted request from a provider failure"
  (test-niwa-session-naming--with-session
    (let (reply events)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (test-niwa-session-naming--with-telemetry events
          (funcall reply 'abort (list :status "aborted")))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'aborted (plist-get props :outcome)))
          (should (eq 'aborted (plist-get props :error-class)))))))
  :doc "classifies a streamed completion with no title as validation"
  (test-niwa-session-naming--with-session
    (let ((original-id (niwa-session-session-id session)) reply events)
      (setq-local gptel-stream t)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (test-niwa-session-naming--with-telemetry events
          (funcall reply t nil))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'invalid-response (plist-get props :outcome)))
          (should (eq 'validation (plist-get props :error-class)))
          (should (equal original-id (niwa-session-name session)))))))
  :doc "classifies an oversized streamed title as validation"
  (test-niwa-session-naming--with-session
    (let (reply events)
      (setq-local gptel-stream t)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (test-niwa-session-naming--with-telemetry events
          (funcall reply (make-string 4097 ?x) (list :stream t)))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'invalid-response (plist-get props :outcome)))
          (should (eq 'validation (plist-get props :error-class)))))))
  :doc "classifies the watchdog as a timeout"
  (test-niwa-session-naming--with-session
    (let ((timer-function (symbol-function 'run-at-time)) reply timeout events)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (time repeat function &rest args)
                   (if (equal time 30)
                       (progn (setq timeout function) nil)
                     (apply timer-function time repeat function args))))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (test-niwa-session-naming--with-telemetry events
          (funcall timeout))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'timeout (plist-get props :outcome)))
          (should (eq 'timeout (plist-get props :error-class)))))))
  :doc "classifies a local failure by its signaled error symbol"
  (test-niwa-session-naming--with-session
    (let (events)
      (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                 (lambda (&rest _)
                   (signal 'user-error '("Naming policy unavailable")))))
        (test-niwa-session-naming--with-telemetry events
          (niwa-session-naming-consider session "Fix this")
          (with-timeout (3 (ert-fail "Naming did not settle"))
            (while (null events) (accept-process-output nil 0.02))))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'error (plist-get props :outcome)))
          (should (eq 'user-error (plist-get props :error-class)))
          (should-not (plist-member props :error))))))
  :doc "records a successful rename as its own outcome"
  (test-niwa-session-naming--with-session
    (let (reply events)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options)
                   (setq reply (plist-get options :callback)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (test-niwa-session-naming--with-telemetry events
          (funcall reply "\"Fix the session chooser\"" nil))
        (should (equal "Fix the session chooser" (niwa-session-name session)))
        (let ((props (test-niwa-session-naming--naming-props events)))
          (should (eq 'renamed (plist-get props :outcome))))))))

(niwa-deftest niwa-session-persistence--save-as (:quiet t)
  ,test (test)
  :doc "an explicit Save As name wins before and during title inference"
  (dolist (in-flight '(nil t))
    (test-niwa-session-naming--with-session
      (let ((old-id (niwa-session-session-id session)) reply request-buffer)
        (niwa-recovery-report session "session-naming" 'request "Naming failed" nil)
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (_prompt &rest options)
                     (setq reply (plist-get options :callback)
                           request-buffer (current-buffer))))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "  My saved session  ")))
          (when in-flight
            (niwa-session-naming-consider session "Fix this")
            (with-timeout (3 (ert-fail "Title request did not start"))
              (while (null reply) (accept-process-output nil 0.01))))
          (niwa-save-session t)
          (when reply (funcall reply "Stale title" nil))
          (should (equal "My saved session" (niwa-session-name session)))
          (should-not (equal old-id (niwa-session-session-id session)))
          (should (eq 'explicit (niwa-session-naming-state session)))
          (should-not (niwa-session-naming-input session))
          (should-not (test-niwa-session-naming--issue session))
          (should-not niwa-session-naming--cancel)
          (should-not (buffer-live-p request-buffer))
          (let ((sidecar (niwa-session-codec-read
                          (niwa-session-artifacts-sidecar-path
                           (niwa-session-save-path session)))))
            (should (equal "My saved session" (plist-get sidecar :session-name)))
            (should (plist-member sidecar :naming-input))
            (should-not (plist-get sidecar :naming-input))
            (should (eq 'explicit (plist-get sidecar :naming-state)))))))))

(niwa-deftest niwa-session-naming-cancel (:quiet t)
  ,test (test)
  :doc "cancels before dispatch without creating a request"
  (test-niwa-session-naming--with-session
    (let (sent)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (setq sent t))))
        (niwa-session-naming-consider session "Fix this")
        (should niwa-session-naming--cancel)
        (niwa-session-naming-cancel)
        (accept-process-output nil 0.02)
        (should-not sent)
        (should-not niwa-session-naming--cancel)))))

(niwa-deftest niwa-session-naming-rename-view (:quiet t)
  ,test (test)
  :doc "renaming from the view preserves a multiline composer draft"
  (test-niwa-session-naming--with-session
    (let ((view (niwa-view--ensure buffer))
          (draft "> Continue here\nwith another line"))
      (unwind-protect
          (with-current-buffer view
            (niwa-view-test--insert-composer-draft draft)
            (niwa-rename-session "Ändere die Sitzung")
            (should (equal draft (niwa-view--visible-draft)))
            (should (equal "Ändere die Sitzung" (niwa-session-name session))))
        (when (buffer-live-p view) (kill-buffer view)))))
  :doc "an asynchronous generated title preserves an active multiline draft"
  (test-niwa-session-naming--with-session
    (let ((draft "> Continue here\nwith another line") reply view)
      (unwind-protect
          (cl-letf (((symbol-function 'gptel-request)
                     (lambda (_prompt &rest options)
                       (setq reply (plist-get options :callback)))))
            (niwa-session-naming-consider session "Fix naming")
            (with-timeout (3 (ert-fail "Title request did not start"))
              (while (null reply) (accept-process-output nil 0.01)))
            (setq view (niwa-view--ensure buffer))
            (with-current-buffer view
              (niwa-view-test--insert-composer-draft draft))
            (funcall reply "Fix naming" nil)
            (with-current-buffer view
              (should (equal draft (niwa-view--visible-draft))))
            (should (equal "Fix naming" (niwa-session-name session))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa--pick-session (:quiet t)
  ,test (test)
  :doc "equal titles select distinct session buffers by their IDs"
  (test-niwa-session-naming--with-session
    (let* ((other (niwa--chat-buffer "other" t workspace directory))
           (other-session (buffer-local-value 'niwa--session other)))
      (unwind-protect
          (progn
            (niwa-session-naming-rename session buffer "Same title")
            (niwa-session-naming-rename other-session other "Same title")
            (should-error (niwa--chat-buffer "Same title" nil workspace)
                          :type 'user-error)
            (dolist (selected (list buffer other))
              (let ((selected-id (niwa-session-session-id
                                  (buffer-local-value 'niwa--session selected))))
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (_prompt collection &rest _)
                             (car (cl-find-if
                                   (lambda (entry)
                                     (string-suffix-p (concat "[" selected-id "]") (car entry)))
                                   collection)))))
                  (should (eq selected (niwa--pick-session
                                        (niwa--workspace-sessions workspace) nil)))))))
        (test-niwa-session-persistence--release-and-kill other other-session)))))

(niwa-deftest niwa-session-naming-rename (:quiet t)
  ,test
  (test)
  :doc "keeps the owned lease and commits renamed metadata through one head"
  (let* ((host "rename-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-rename-" t)))
         (niwa-session-durability--client-id (make-string 64 ?e))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-niwa-session-persistence--make-remote-restore-fixture
                         host local-root "rename transcript\n"))
                       (parent
                        (file-name-directory
                         (directory-file-name session-dir)))
                       (new-id (niwa-session-session-id session))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat parent new-id)))
                       (niwa-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (niwa-execution-target-identity
              (niwa-session-execution-target session))
             t niwa-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (niwa-session-durability-lease-acquire
                    session-dir "*remote-rename*" session))
                  (setf (niwa-session-publication session)
                        (niwa-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer " *remote-rename-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local niwa--session session)
                    (setq buffer-file-name segment)
                    (insert "rename transcript\n"))
                  (niwa-session-control-transfer-register-root-buffer session buffer)
                  (let ((generation
                         (plist-get (niwa-session-lease session)
                                    :generation))
                        (head-before
                         (plist-get (niwa-session-publication session)
                                    :head)))
                    (niwa-session-naming-rename session buffer "renamed")
                    (should (file-directory-p session-dir))
                    (should (file-directory-p new-save-path))
                    (should
                     (= generation
                        (plist-get (niwa-session-lease session)
                                   :generation)))
                    (should-not
                     (equal head-before
                            (plist-get (niwa-session-publication session)
                                       :head)))
                    (should
                     (equal "rename transcript\n"
                            (niwa-session-artifacts-read-artifact
                             session "segment-0001.chat.org" t)))
                    (let ((sidecar
                           (with-temp-buffer
                             (insert
                              (niwa-session-artifacts-read-artifact
                               session "session.meta.el" t))
                             (goto-char (point-min))
                             (read (current-buffer)))))
                      (should (equal "renamed"
                                     (plist-get sidecar :session-name)))
                      (should (equal new-id
                                     (plist-get sidecar :session-id))))))
              (when session
                (niwa-session-durability-lease-release
                 (niwa-session-save-path session) session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "restores the display name after a pre-CAS failure"
  (let* ((host "rename-rollback")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-rename-rollback-" t)))
         (niwa-session-durability--client-id (make-string 64 ?f))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-niwa-session-persistence--make-remote-restore-fixture
                         host local-root "rollback transcript\n"))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat
                          (file-name-directory
                           (directory-file-name session-dir))
                          "renamed-rollback")))
                       (niwa-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (niwa-execution-target-identity
              (niwa-session-execution-target session))
             t niwa-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (niwa-session-durability-lease-acquire
                    session-dir "*remote-rename-rollback*" session))
                  (setf (niwa-session-publication session)
                        (niwa-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer
                         " *remote-rename-rollback-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local niwa--session session)
                    (setq buffer-file-name segment)
                    (insert "rollback transcript\n"))
                  (niwa-session-control-transfer-register-root-buffer session buffer)
                  (let ((head-before
                         (plist-get (niwa-session-publication session)
                                    :head))
                        (publish-artifact
                         (symbol-function
                          'niwa-session-publication--publish-artifact)))
                    (cl-letf
                        (((symbol-function
                           'niwa-session-publication--publish-artifact)
                          (lambda (artifact)
                            (ignore artifact publish-artifact)
                            (signal 'file-error
                                    '("Injected Rename publication failure")))))
                      (should-error
                       (niwa-session-naming-rename session buffer "renamed")
                       :type 'file-error))
                    (should (file-directory-p session-dir))
                    (should-not (file-directory-p new-save-path))
                    (should (equal session-dir
                                   (niwa-session-save-path session)))
                    (should (equal "main" (niwa-session-name session)))
                    (should (equal segment
                                   (buffer-local-value
                                    'buffer-file-name buffer)))
                    (should (equal head-before
                                   (plist-get
                                    (niwa-session-publication session)
                                    :head)))
                    (should-not
                     (niwa-session-pending-publication session))
                    (should
                     (niwa-session-durability-lease-owned-p session))))
              (when session
                (niwa-session-durability-lease-release
                 (niwa-session-save-path session) session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-rename-session (:quiet t)
  ,test
  (test)
  :doc "renames the session-name field and the buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local niwa--session session)
                (insert "Hi\n")
                (niwa-session-artifacts-save session buf)
                (let* ((old-save-path (niwa-session-save-path session))
                       (artifact-directory
                        (file-name-concat old-save-path "tool-results"))
                       (ready (file-name-concat tempdir "rename-ready"))
                       (release (file-name-concat tempdir "rename-release"))
                       (niwa-sandbox-mode 'off)
                       initial blocked terminal execution-id)
                  (niwa-execution-start-bash
                   (lambda (value) (setq initial value))
                   :session session :data-buffer buf :owner "agent-a"
                   :owner-context session
                   :command
                   (list "sh" "-c"
                         (concat "printf before; : > \"$1\"; "
                                 "while ! test -e \"$2\"; do sleep 0.01; done; "
                                 "printf after")
                         "rename-execution" ready release)
                   :workdir tempdir :writable-roots (list tempdir)
                   :artifact-directory artifact-directory
                   :yield-time-ms 10)
                  (with-timeout (2 (error "Execution did not yield"))
                    (while (or (null initial) (not (file-exists-p ready)))
                      (accept-process-output nil 0.02)))
                  (should (eq 'running (plist-get (plist-get initial :facts) :state)))
                  (setq execution-id
                        (plist-get (plist-get initial :facts) :execution-id))
                  (niwa-rename-session "alt-permissions")
                  (should (equal "alt-permissions"
                                 (niwa-session-name session)))
                  (should (equal old-save-path (niwa-session-save-path session)))
                  (should (file-directory-p old-save-path))
                  ;; Buffer renamed per convention.
                  (should (string-match-p
                           "\\`\\*niwa:alt-permissions@"
                           (buffer-name buf)))
                  ;; The real child entered its gate and cannot finish until
                  ;; after the rename, even on a heavily loaded worker.
                  (should (file-exists-p ready))
                  (should-not (file-exists-p release))
                  (niwa-execution-observe
                   session "agent-a" execution-id
                   (lambda (value) (setq blocked value)) :wait-ms 0)
                  (should (eq 'running (plist-get (plist-get blocked :facts) :state)))
                  (with-temp-file release (insert "continue\n"))
                  (niwa-execution-observe
                   session "agent-a" execution-id
                   (lambda (value) (setq terminal value))
                   :wait-ms 5000)
                  (with-timeout (6 (error "Renamed execution did not finish"))
                    (while (null terminal)
                      (accept-process-output nil 0.02)))
                  (should (= 0
                             (plist-get (plist-get terminal :facts)
                                        :exit-code)))
                  (let ((artifact
                         (plist-get (plist-get terminal :facts) :output-path)))
                    (should (string-prefix-p "artifact://" artifact))
                    (should
                     (equal "beforeafter"
                            (niwa-resource-execute
                             (niwa-resource-prepare
                              'read artifact (list :session session))
                             (lambda (path _address)
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (buffer-string)))))))))
            (test-niwa-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "publishes the renamed sidecar through the critical seam"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *rename-publication*"))
           (publish-function
            (symbol-function 'niwa-session-artifacts-publish-sidecar-state))
           published)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "transcript\n")
            (niwa-session-artifacts-save session buffer)
            (let ((head (plist-get (niwa-session-publication session) :head)))
              (cl-letf
                  (((symbol-function
                     'niwa-session-artifacts-publish-sidecar-state)
                    (lambda (actual-session root-buffer &optional token)
                      (push (list actual-session root-buffer) published)
                      (funcall publish-function actual-session root-buffer token))))
                (niwa-rename-session "renamed"))
              (should (equal (list (list session buffer)) published))
              (should-not
               (equal head (plist-get (niwa-session-publication session) :head)))
              (should
               (equal "renamed"
                      (plist-get
                       (niwa-session-codec-read
                        (niwa-session-artifacts-sidecar-path
                         (niwa-session-save-path session)))
                       :session-name)))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry)))))


(niwa-deftest niwa-retry-session-naming (:quiet t)
  ,test (test)
  :doc "retries a failed title after resume from the recorded input and clears its notice"
  (test-niwa-session-naming--with-session
    (let (calls)
      (test-niwa-session-naming--with-engine calls
        (niwa-session-checkpoint-seed
         session (plist-put (niwa-session-checkpoint-project session nil)
                            :first-user-message "Preview only"))
        (niwa-session-naming-consider session "  Fix the session chooser\n  ")
        (test-niwa-session-naming--await calls 1)
        (should (equal "Fix the session chooser" (plist-get (car calls) :prompt)))
        (should (equal "Fix the session chooser" (niwa-session-naming-input session)))
        (funcall (plist-get (car calls) :callback) nil '(:status "HTTP/2 500"))
        (let ((notice (plist-get (test-niwa-session-naming--issue session) :message)))
          (should (string-search "M-x niwa-retry-session-naming" notice)))
        (niwa-session-artifacts-save session buffer)
        (let ((path (niwa-session-save-path session)))
          (should (equal "Fix the session chooser"
                         (plist-get (niwa-session-codec-read
                                     (niwa-session-artifacts-sidecar-path path))
                                    :naming-input)))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (setq buffer (niwa-session-persistence-restore path nil nil workspace)
                session (buffer-local-value 'niwa--session buffer)))
        (with-current-buffer buffer
          (should (eq 'attempted (niwa-session-naming-state session)))
          (should (equal "Fix the session chooser" (niwa-session-naming-input session)))
          (should (test-niwa-session-naming--issue session))
          (niwa-retry-session-naming)
          (test-niwa-session-naming--await calls 2)
          (should (equal "Fix the session chooser" (plist-get (car calls) :prompt)))
          (funcall (plist-get (car calls) :callback) "Chooser fix" nil)
          (should (equal "Chooser fix" (niwa-session-name session)))
          (should (eq 'attempted (niwa-session-naming-state session)))
          (should-not (test-niwa-session-naming--issue session))
          (should-not (test-niwa-session-naming--issue
                       (plist-get (niwa-session-codec-deserialize
                                   (niwa-session-codec-read
                                    (niwa-session-artifacts-sidecar-path
                                     (niwa-session-save-path session)))
                                   workspace)
                                  :session)))))))
  :doc "a view retry names its root session from supplied input"
  (test-niwa-session-naming--with-session
    (let ((view (niwa-view--ensure buffer)) calls)
      (unwind-protect
          (test-niwa-session-naming--with-engine calls
            (with-current-buffer view (niwa-retry-session-naming "Name from the view"))
            (test-niwa-session-naming--await calls 1)
            (should (equal "Name from the view" (plist-get (car calls) :prompt)))
            (should (equal "Name from the view" (niwa-session-naming-input session)))
            (funcall (plist-get (car calls) :callback) "View title" nil)
            (should (equal "View title" (niwa-session-name session))))
        (when (buffer-live-p view) (kill-buffer view)))))
  :doc "asks for input when none was recorded instead of using the preview"
  (test-niwa-session-naming--with-session
    (let (calls asked)
      (setf (niwa-session-naming-state session) 'attempted)
      (niwa-session-checkpoint-seed
       session (plist-put (niwa-session-checkpoint-project session nil)
                          :first-user-message "Preview only"))
      (test-niwa-session-naming--with-engine calls
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (setq asked t) "  ")))
          (should-error (niwa-retry-session-naming) :type 'user-error))
        (should asked)
        (should-not (niwa-session-naming-input session))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (propertize "Described task" 'face 'bold))))
          (niwa-retry-session-naming))
        (test-niwa-session-naming--await calls 1)
        (should (= 1 (length calls)))
        (should (equal "Described task" (plist-get (car calls) :prompt)))
        (should-not (text-properties-at 0 (niwa-session-naming-input session))))))
  :doc "explicit names, transient conversations and refused authority start nothing"
  (test-niwa-session-naming--with-session
    (let (calls)
      (test-niwa-session-naming--with-engine calls
        (setf (niwa-session-naming-state session) 'explicit)
        (should-error (niwa-retry-session-naming "Replace it") :type 'user-error)
        (setf (niwa-session-naming-state session) 'attempted
              (niwa-session-audit-session session) (niwa-session-create "parent" workspace))
        (should-error (niwa-retry-session-naming "Replace it") :type 'user-error)
        (setf (niwa-session-audit-session session) nil)
        (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
                   (lambda (&rest _) (user-error "Another client owns this session"))))
          (let ((err (should-error (niwa-retry-session-naming "Replace it")
                                   :type 'user-error)))
            (should (equal (format "niwa [%s]: Another client owns this session"
                                   (niwa-workspace-root workspace))
                           (error-message-string err)))))
        (accept-process-output nil 0.05)
        (should-not calls)
        (should-not niwa-session-naming--cancel)
        (should-not (niwa-session-naming-input session)))))
  :doc "supersedes a running title silently and records only the retry"
  (test-niwa-session-naming--with-session
    (let (calls events)
      (test-niwa-session-naming--with-telemetry events
        (test-niwa-session-naming--with-engine calls
          (niwa-session-naming-consider session "First task")
          (test-niwa-session-naming--await calls 1)
          (let ((old (car calls)))
            (niwa-retry-session-naming "Second task")
            (should (plist-get old :cancelled))
            (should-not (buffer-live-p (plist-get old :buffer)))
            (test-niwa-session-naming--await calls 2)
            (funcall (plist-get old :callback) "Stale title" nil)
            (funcall (plist-get (car calls) :callback) "Second title" nil))))
      (should (equal "Second title" (niwa-session-name session)))
      (let ((event (test-niwa-session-naming--naming-props events)))
        (should (eq 'renamed (plist-get event :outcome)))
        (should (eq 'retry (plist-get event :trigger))))))
  :doc "a manual rename drops the recorded input and the failure notice"
  (test-niwa-session-naming--with-session
    (let (calls)
      (test-niwa-session-naming--with-engine calls
        (niwa-session-naming-consider session "Fix this")
        (test-niwa-session-naming--await calls 1)
        (funcall (plist-get (car calls) :callback) nil '(:status "HTTP/2 500"))
        (should (test-niwa-session-naming--issue session))
        (niwa-rename-session "Manual name")
        (should (eq 'explicit (niwa-session-naming-state session)))
        (should-not (niwa-session-naming-input session))
        (should-not (test-niwa-session-naming--issue session))))))

(niwa-deftest niwa-session-naming-fallback (:quiet t)
  ,test (test)
  :doc "an unavailable naming provider falls back once to the session's own provider"
  (dolist (mode '(checked reported))
    (test-niwa-session-naming--with-session
      (test-niwa-session-naming--with-primary primary
        (let ((root-backend gptel-backend) (root-model gptel-model) calls events)
          (cl-letf (((symbol-function 'niwa-engine-text-unavailable)
                     (lambda (backend)
                       (and (eq mode 'checked) (eq backend primary)
                            "Codex login needs renewal; sign in or retry after refresh"))))
            (test-niwa-session-naming--with-telemetry events
              (test-niwa-session-naming--with-engine calls
                (niwa-session-naming-consider session "Fix this")
                (when (eq mode 'reported)
                  (test-niwa-session-naming--await calls 1)
                  (should (eq primary (plist-get (car calls) :backend)))
                  (funcall (plist-get (car calls) :callback) nil
                           '(:status "HTTP/2 401" :error (:code 401 :message "expired"))))
                (test-niwa-session-naming--await calls (if (eq mode 'reported) 2 1))
                (let ((call (car calls)))
                  (should (eq root-backend (plist-get call :backend)))
                  (should (eq root-model (plist-get call :model)))
                  (funcall (plist-get call :callback) "Fallback title" nil)))))
          (should (equal "Fallback title" (niwa-session-name session)))
          (should (eq root-backend gptel-backend))
          (should (eq root-model gptel-model))
          (let ((event (test-niwa-session-naming--naming-props events)))
            (should (eq 'renamed (plist-get event :outcome)))
            (should (plist-get event :fallback)))))))
  :doc "other failures and an equal provider never fall back"
  (dolist (mode '(error timeout invalid aborted same-provider))
    (test-niwa-session-naming--with-session
      (test-niwa-session-naming--with-primary primary
        (let ((timer-function (symbol-function 'run-at-time)) timeout calls events)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (time repeat function &rest args)
                       (if (equal time 30)
                           (progn (setq timeout function) (timer-create))
                         (apply timer-function time repeat function args))))
                    ((symbol-function 'niwa-model-resolve-workload)
                     (if (eq mode 'same-provider)
                         (lambda (&rest _) (list :backend gptel-backend :model gptel-model))
                       (symbol-function 'niwa-model-resolve-workload))))
            (test-niwa-session-naming--with-telemetry events
              (test-niwa-session-naming--with-engine calls
                (niwa-session-naming-consider session "Fix this")
                (test-niwa-session-naming--await calls 1)
                (funcall (plist-get (car calls) :ready))
                (let ((reply (plist-get (car calls) :callback)))
                  (pcase mode
                    ('error (funcall reply nil '(:status "HTTP/2 400" :error (:code "bad_request"))))
                    ('timeout (funcall timeout))
                    ('invalid (funcall reply " " nil))
                    ('aborted (funcall reply 'abort nil))
                    ('same-provider (funcall reply nil '(:status "HTTP/2 401")))))
                (accept-process-output nil 0.05))))
          (should (= 1 (length calls)))
          (let ((event (test-niwa-session-naming--naming-props events)))
            (should-not (plist-get event :fallback))
            (should-not (eq 'renamed (plist-get event :outcome))))))))
  :doc "a failed fallback ends the operation and names both attempts"
  (test-niwa-session-naming--with-session
    (test-niwa-session-naming--with-primary primary
      (let (calls events)
        (cl-letf (((symbol-function 'niwa-engine-text-unavailable)
                   (lambda (backend) (and (eq backend primary) "Install the provider runtime"))))
          (test-niwa-session-naming--with-telemetry events
            (test-niwa-session-naming--with-engine calls
              (niwa-session-naming-consider session "Fix this")
              (test-niwa-session-naming--await calls 1)
              (funcall (plist-get (car calls) :ready))
              (funcall (plist-get (car calls) :callback) nil
                       '(:status "HTTP/2 401" :error (:message "Incorrect key sk-secret-123")))
              (accept-process-output nil 0.05))))
        (should (= 1 (length calls)))
        (let ((notice (plist-get (test-niwa-session-naming--issue session) :message))
              (event (test-niwa-session-naming--naming-props events)))
          (should (string-search "naming-primary:naming-model failed before sending: Install the provider runtime" notice))
          (should (string-search "fallback to" notice))
          (should (string-search "waiting for the reply" notice))
          (should (string-search "HTTP/2 401" notice))
          (should-not (string-search "sk-secret" notice))
          (should (equal "Automatic session naming failed"
                         (niwa-recovery-public-message
                          (test-niwa-session-naming--issue session))))
          (should (eq 'provider-error (plist-get event :outcome)))
          (should (eq 'unavailable (plist-get event :error-class)))
          (should (eq 'response (plist-get event :stage)))
          (should (plist-get event :fallback))
          (should-not (string-search "sk-secret" (format "%S" event)))))))
  :doc "the fallback keeps the session's effort only when its model supports it"
  (with-temp-buffer
    (let ((primary (niwa-session-naming--candidate
                    (gptel--make-backend :name "naming-primary") 'naming-model nil))
          (model (make-symbol "effort-model")))
      (put model :reasoning-effort '(member low high))
      (setq-local gptel-backend (gptel--make-backend :name "root")
                  gptel-model model
                  gptel-reasoning-effort 'high)
      (should (eq 'high (niwa-session-naming--candidate-effort
                         (niwa-session-naming--fallback (current-buffer) primary))))
      (setq-local gptel-reasoning-effort 'max)
      (should-not (niwa-session-naming--candidate-effort
                   (niwa-session-naming--fallback (current-buffer) primary))))))

(niwa-deftest niwa-session-naming--unavailable-p ()
  ,test (test)
  :doc "only structured availability evidence classifies a failure"
  (dolist (info '((:status "HTTP/2 401") (:status "HTTP/1.1 403 Forbidden")
                  (:error (:code 401)) (:error (:code "invalid_api_key"))
                  (:error (:code "model_not_available"))
                  (:code unavailable :stage startup :error "Sign in")
                  (:code -32000 :stage startup :error "Authentication required")))
    (should (niwa-session-naming--unavailable-p info)))
  (dolist (info '((:status "HTTP/2 400") (:status "HTTP/2 500")
                  (:error (:code "model_not_found"))
                  (:error (:message "unauthorized: please log in"))
                  ;; JSON-RPC's generic server error after the prompt was sent.
                  (:code -32000 :stage response :error "Internal error")
                  (:stage startup :error "ACP startup timed out")
                  nil))
    (should-not (niwa-session-naming--unavailable-p info))))

(niwa-deftest niwa-session-naming-deadline (:quiet t)
  ,test (test)
  :doc "a title published before a presentation failure counts as renamed"
  (test-niwa-session-naming--with-session
    (let ((notify (symbol-function 'niwa-session-persistence-notify-session-event))
          calls events)
      (test-niwa-session-naming--with-telemetry events
        (test-niwa-session-naming--with-engine calls
          (niwa-session-naming-consider session "Fix this")
          (test-niwa-session-naming--await calls 1)
          (cl-letf (((symbol-function 'niwa-session-persistence-notify-session-event)
                     (lambda (owner event &rest args)
                       (if (eq event 'rename)
                           (error "Injected presentation failure")
                         (apply notify owner event args)))))
            (funcall (plist-get (car calls) :callback) "Committed title" nil))))
      (should (equal "Committed title" (niwa-session-name session)))
      (should (eq 'renamed (plist-get (test-niwa-session-naming--naming-props events)
                                      :outcome)))
      (should-not (test-niwa-session-naming--issue session))))
  :doc "the reply deadline starts once after submission and ends with the reply"
  (test-niwa-session-naming--with-session
    (let ((timer-function (symbol-function 'run-at-time))
          (niwa-transport-retry-seconds 0.01)
          ;; The spy times the 30 s reply deadline; keep lease renewal apart.
          (niwa-session-lease-renewal-seconds 31)
          deadlines calls events)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (time repeat function &rest args)
                   (if (equal time 30)
                       (progn (push function deadlines) (timer-create))
                     (apply timer-function time repeat function args)))))
        (test-niwa-session-naming--with-telemetry events
          (test-niwa-session-naming--with-engine calls
            (niwa-session-naming-consider session "Fix this")
            (test-niwa-session-naming--await calls 1)
            ;; Slow startup: nothing is timed before the prompt is submitted.
            (accept-process-output nil 0.05)
            (should-not deadlines)
            (funcall (plist-get (car calls) :ready))
            (funcall (plist-get (car calls) :ready))
            (should (= 1 (length deadlines)))
            (unwind-protect
                (progn
                  (setf (niwa-session-publication-active-p session) t)
                  (funcall (plist-get (car calls) :callback) "Deferred title" nil)
                  ;; The reply arrived; a late deadline is stale.
                  (funcall (car deadlines))
                  (should niwa-session-naming--cancel))
              (setf (niwa-session-publication-active-p session) nil))
            (with-timeout (3 (ert-fail "Title did not apply after publication"))
              (while niwa-session-naming--cancel (accept-process-output nil 0.01))))))
      (should (equal "Deferred title" (niwa-session-name session)))
      (should (eq 'renamed (plist-get (test-niwa-session-naming--naming-props events)
                                      :outcome)))))
  :doc "a synchronous reply settles without starting a deadline"
  (test-niwa-session-naming--with-session
    (let ((timer-function (symbol-function 'run-at-time))
          (niwa-session-lease-renewal-seconds 31)
          deadline)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (time repeat function &rest args)
                   (if (equal time 30) (setq deadline t)
                     (apply timer-function time repeat function args))))
                ((symbol-function 'niwa-engine-request-text)
                 (lambda (_backend _prompt _system callback &optional _stream _context ready)
                   (funcall callback "Quick title" nil)
                   (funcall ready)
                   (lambda () (ert-fail "A finished request was cancelled")))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title did not apply"))
          (while (not (equal "Quick title" (niwa-session-name session)))
            (accept-process-output nil 0.01)))
        (should-not deadline)
        (should-not niwa-session-naming--cancel))))
  :doc "cancellation during submission cancels once the canceller exists"
  (test-niwa-session-naming--with-session
    (let (cancelled request)
      (cl-letf (((symbol-function 'niwa-engine-request-text)
                 (lambda (&rest _)
                   (setq request (current-buffer))
                   (with-current-buffer buffer (niwa-session-naming-cancel))
                   (should (buffer-live-p request))
                   (lambda () (setq cancelled t)))))
        (niwa-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (not request) (accept-process-output nil 0.01)))
        (should cancelled)
        (should-not (buffer-live-p request))
        (should-not niwa-session-naming--cancel)))))

(provide 'test-niwa-session-naming)
;;; test-niwa-session-naming.el ends here
