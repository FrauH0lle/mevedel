;;; test-niwa-permission-mode.el -- Permission mode tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests session-scoped permission mode normalization, transitions, and UI
;; lifecycle behavior.

;;; Code:

(require 'niwa-permission-mode)
(require 'niwa-plan-mode)
(require 'niwa-reminders)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'niwa-tool-exec-permission)
(require 'niwa-view-composer)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-test--with-saved-permission-mode (&rest body)
  "Execute BODY with the global `niwa-permission-mode' preserved."
  (declare (indent 0) (debug t))
  `(let ((niwa-test--saved-mode
          (default-toplevel-value 'niwa-permission-mode)))
     (unwind-protect (progn ,@body)
       (set-default-toplevel-value 'niwa-permission-mode
                                   niwa-test--saved-mode))))


;;
;;; Mode decisions

(niwa-deftest niwa-permission-mode-normalize ()
  ,test
  (test)
  :doc "accepts only canonical permission modes"
  (dolist (mode '(ask edits full-auto))
    (should (eq mode (niwa-permission-mode-normalize mode)))
    (should (eq mode
                (niwa-permission-mode-normalize (symbol-name mode)))))
  :doc "rejects every superseded mode and alias"
  (dolist (mode '(default accept-edits trust-all auto edit))
    (should-error (niwa-permission-mode-normalize mode)
                  :type 'user-error)))

(niwa-deftest niwa-permission-mode-transition ()
  ,test
  (test)
  :doc "an explicit permission choice exits Plan"
  (with-temp-buffer
    (let ((session (niwa-session--create
                    :name "main" :permission-mode 'edits :plan-mode t)))
      (setq-local niwa--session session
                  niwa-permission-mode 'edits)
      (niwa-permission-mode-transition 'ask)
      (should-not (niwa-session-plan-mode session))
      (should (eq 'ask (niwa-session-permission-mode session)))))

  :doc "keeps a committed transition when an incomplete view cannot repaint"
  (let ((data-buffer (generate-new-buffer " *mev-mode-data*"))
        (view-buffer (generate-new-buffer " *mev-mode-view*")))
    (unwind-protect
        (let ((session (niwa-session--create
                        :name "main" :permission-mode 'edits)))
          (with-current-buffer view-buffer
            (insert "> quoted\nsecond line"))
          (with-current-buffer data-buffer
            (setq-local niwa--session session
                        niwa--view-buffer view-buffer
                        niwa-permission-mode 'edits)
            (should (eq 'ask (niwa-permission-mode-transition 'ask))))
          (should (eq 'ask (niwa-session-permission-mode session)))
          (with-current-buffer view-buffer
            (should (equal "> quoted\nsecond line" (buffer-string)))))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "full-auto settles pending operation and capability approvals once"
  (let ((session (niwa-session--create
                  :name "mode-queue" :permission-mode 'ask))
        (niwa-permission-rules nil)
        (niwa-permission-reviewer 'user)
        outcomes)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                 #'ignore))
        (niwa-tool-exec-permission-check-bash-async
         nil
         (list :command "unknown-mode-command"
               :sandbox_permissions "with_additional_permissions"
               :additional_permissions '(:network t)
               :justification "Test pending authority"
               :permission-context (list :mode 'ask :session session))
         (lambda (outcome) (push outcome outcomes)))
        (should (= 1 (length (niwa-session-permission-queue session))))
        (should-not outcomes)
        (niwa-permission-mode-transition 'full-auto)
        (should (equal '(allow) outcomes))
        (should-not (niwa-session-permission-queue session))
        (niwa-permission-mode-transition 'full-auto)
        (should (equal '(allow) outcomes)))))

  :doc "identity fencing preserves warm confinement readiness and observes each mutation"
  (dolist (reason '(nil sandbox-unavailable))
    (with-temp-buffer
      (let* ((target (niwa-execution-target-create "/ssh:review.invalid:/srv/project/"))
             (readiness (list :status (if reason 'blocked 'ready)
                              :reason reason :sandbox-mode 'required))
             (session (niwa-session--create
                       :execution-target target
                       :permission-mode 'edits))
             (full-probes 0) (observations 0))
        (setq-local niwa--session session)
        (setf (niwa-execution-target-readiness target) readiness)
        (cl-letf (((symbol-function 'niwa-execution-target--live-connection) #'ignore)
                  ((symbol-function 'niwa-execution-target--process-output)
                   (lambda (&rest _)
                     (cl-incf full-probes)
                     (error "Unexpected full readiness probe")))
                  ((symbol-function 'niwa-execution-target-observe-incarnation)
                   (lambda (_) (cl-incf observations))))
          (dotimes (_ 3)
            (should (eq readiness (niwa-execution-target-probe target nil 'required)))
            (niwa-session-artifacts-check-target-incarnation session (current-buffer)))
          (should (= 0 full-probes))
          (should (= 3 observations))
          (should (eq readiness (niwa-execution-target-readiness target)))
          (should (eq 'full-auto (niwa-permission-mode-transition 'full-auto)))
          (should (= 4 observations))))))

  :doc "remote Edits can change mode without weakening target identity checks"
  (dolist (mode '(ask full-auto))
    (with-temp-buffer
      (let* ((target (niwa-execution-target-create "/ssh:review.invalid:/srv/project/"))
             (session (niwa-session--create
                       :execution-target target
                       :permission-mode 'edits :sandbox-mode 'best-effort))
             observed)
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-execution-target-probe)
                   (lambda (target &optional _refresh sandbox-mode)
                     (niwa-execution-target--apply-sandbox-readiness
                      target '(:status ready) sandbox-mode)))
                  ((symbol-function 'niwa-sandbox-probe)
                   (lambda (&rest _) '(:available nil :reason "No Bubblewrap")))
                  ((symbol-function 'niwa-execution-target-observe-incarnation)
                   (lambda (_) (setq observed t))))
          (should (eq mode (niwa-permission-mode-transition mode)))
          (should observed)
          (should (eq mode (niwa-session-permission-mode session)))
          (setq observed nil)
          (cl-letf (((symbol-function 'niwa-execution-target-probe)
                     (lambda (&rest _) '(:status blocked :reason unreachable))))
            (should-error (niwa-permission-mode-transition 'edits) :type 'user-error)
            (should (eq mode (niwa-session-permission-mode session)))))))))

(niwa-deftest niwa-permission-mode-decision ()
  ,test
  (test)
  :doc "full-auto allows everything"
  (progn
    (should (eq (niwa-permission-mode-decision 'full-auto nil) 'allow))
    (should (eq (niwa-permission-mode-decision 'full-auto t) 'allow)))
  :doc "edits allows read-only and native edits but asks for other writes"
  (progn
    (should (eq (niwa-permission-mode-decision 'edits t) 'allow))
    (should (eq (niwa-permission-mode-decision 'edits nil t) 'allow))
    (should (eq (niwa-permission-mode-decision 'edits nil nil) 'ask)))
  :doc "ask allows read-only and reviewed edits but asks for other writes"
  (progn
    (should (eq (niwa-permission-mode-decision 'ask t) 'allow))
    (should (eq (niwa-permission-mode-decision 'ask nil) 'ask))
    (should (eq (niwa-permission-mode-decision 'ask nil t t) 'allow))))

(niwa-deftest niwa-permission-mode-effective ()
  ,test
  (test)
  :doc "uses session mode before data-buffer and global fallbacks"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (set-default-toplevel-value 'niwa-permission-mode 'full-auto)
            (with-current-buffer data-buf
              (setq-local niwa--session
                          (niwa-session--create
                           :name "data" :permission-mode 'edits)))
            (should (eq (niwa-permission-mode-effective session data-buf)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "uses data-buffer session when no session is passed"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
    (unwind-protect
        (with-current-buffer data-buf
          (setq-local niwa--session
                      (niwa-session--create
                       :name "data" :permission-mode 'edits))
          (should (eq (niwa-permission-mode-effective nil data-buf)
                      'edits)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "uses data-buffer local mode before global fallback"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
      (unwind-protect
          (progn
            (set-default-toplevel-value 'niwa-permission-mode 'ask)
            (with-current-buffer data-buf
              (setq-local niwa-permission-mode 'full-auto))
            (should (eq (niwa-permission-mode-effective nil data-buf)
                        'full-auto)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "uses explicit surface buffer local mode before data-buffer fallback"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*"))
        (surface-buf (generate-new-buffer " *mev-effective-surface*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (setq-local niwa-permission-mode 'ask))
          (with-current-buffer surface-buf
            (setq-local niwa-permission-mode 'full-auto))
          (should (eq (niwa-permission-mode-effective
                       nil data-buf surface-buf)
                      'full-auto)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (buffer-live-p surface-buf) (kill-buffer surface-buf))))

  :doc "ignores caller local mode when a data-buffer is explicit"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer data-buf
            (setq-local niwa-permission-mode 'ask))
          (setq-local niwa-permission-mode 'full-auto)
          (should (eq (niwa-permission-mode-effective nil data-buf)
                      'ask)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "honors trusted dynamic mode bindings without a session"
  (with-temp-buffer
    (let ((niwa-permission-mode 'full-auto))
      (should (eq 'full-auto (niwa-permission-mode-effective)))
      (should (eq 'full-auto (niwa-permission-mode-for-context nil)))))

  :doc "falls back to the global mode"
  (niwa-test--with-saved-permission-mode
    (set-default-toplevel-value 'niwa-permission-mode 'edits)
    (should (eq (niwa-permission-mode-effective) 'edits))))

(niwa-deftest niwa-permission-mode-label ()
  ,test
  (test)
  :doc "renders compact labels for permission modes"
  (should (equal "ask" (niwa-permission-mode-label 'ask)))
  (should (equal "edits" (niwa-permission-mode-label 'edits)))
  (should (equal "full-auto" (niwa-permission-mode-label 'full-auto)))
  (should (equal "ask" (niwa-permission-mode-label 'unknown))))

(niwa-deftest niwa-permission-mode--set ()
  ,test
  (test)
  :doc "no session context: updates the global default only"
  (niwa-test--with-saved-permission-mode
    (set-default-toplevel-value 'niwa-permission-mode 'ask)
    (with-temp-buffer
      (niwa-permission-mode--set 'niwa-permission-mode 'full-auto))
    (should (eq (default-toplevel-value 'niwa-permission-mode) 'full-auto)))

  :doc "from data buffer: updates session slot and its buffer-local, leaves default untouched"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (set-default-toplevel-value 'niwa-permission-mode 'ask)
            (with-current-buffer data-buf
              (niwa-permission-mode--set 'niwa-permission-mode 'edits))
            (should (eq (niwa-session-permission-mode session) 'edits))
            (should (eq (buffer-local-value 'niwa-permission-mode data-buf)
                        'edits))
            ;; Global default is NOT touched.
            (should (eq (default-toplevel-value 'niwa-permission-mode)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "from view buffer: back-pointer resolves to the session; data + view locals both update"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (view-buf (generate-new-buffer " *mev-test-view*")))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (setq-local niwa--view-buffer view-buf))
            (with-current-buffer view-buf
              (setq-local niwa--data-buffer data-buf))
            (set-default-toplevel-value 'niwa-permission-mode 'ask)
            (with-current-buffer view-buf
              (niwa-permission-mode--set
               'niwa-permission-mode 'edits))
            (should (eq (niwa-session-permission-mode session) 'edits))
            (should (eq (buffer-local-value 'niwa-permission-mode data-buf)
                        'edits))
            (should (eq (buffer-local-value 'niwa-permission-mode view-buf)
                        'edits))
            (should (eq (default-toplevel-value 'niwa-permission-mode)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (when (buffer-live-p view-buf) (kill-buffer view-buf)))))

  :doc "interactive aliases are rejected below the user-input boundary"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (should-error
               (niwa-permission-mode--set
                'niwa-permission-mode 'edit)
               :type 'user-error))
            (should (eq (niwa-session-permission-mode session)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "multiple sessions: only the current session is modified"
  (niwa-test--with-saved-permission-mode
    (let ((data-a (generate-new-buffer " *mev-test-a*"))
          (data-b (generate-new-buffer " *mev-test-b*")))
      (unwind-protect
          (let ((sess-a (niwa-session--create
                         :name "a" :permission-mode 'ask))
                (sess-b (niwa-session--create
                         :name "b" :permission-mode 'edits)))
            (with-current-buffer data-a
              (setq-local niwa--session sess-a))
            (with-current-buffer data-b
              (setq-local niwa--session sess-b))
            (set-default-toplevel-value 'niwa-permission-mode 'ask)
            (with-current-buffer data-a
              (niwa-permission-mode--set 'niwa-permission-mode 'full-auto))
            (should (eq (niwa-session-permission-mode sess-a) 'full-auto))
            (should (memq 'mode-constraints
                          (mapcar #'niwa-reminder-type
                                  (niwa-session-reminders sess-a))))
            ;; Session B and the global default are untouched.
            (should (eq (niwa-session-permission-mode sess-b) 'edits))
            (should (eq (default-toplevel-value 'niwa-permission-mode)
                        'ask))
            ;; Buffer-local was set in data-a, not in data-b.
            (should (eq (buffer-local-value 'niwa-permission-mode data-a)
                        'full-auto))
            (should-not (local-variable-p 'niwa-permission-mode data-b)))
        (when (buffer-live-p data-a) (kill-buffer data-a))
        (when (buffer-live-p data-b) (kill-buffer data-b)))))

  :doc "setq-local bypasses setter: session slot stays stale"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (setq-local niwa-permission-mode 'full-auto))
            (should (eq (buffer-local-value 'niwa-permission-mode data-buf)
                        'full-auto))
            (should (eq (niwa-session-permission-mode session) 'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(niwa-deftest niwa-permission-mode-set-session-scoped ()
  ,test
  (test)
  :doc "generic helper routes to slot-setter when a session is current"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (calls nil))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer data-buf
              (niwa-permission-mode-set-session-scoped
               'niwa-permission-mode 'edits
               (lambda (s v) (push (cons s v) calls)
                 (setf (niwa-session-permission-mode s) v))))
            (should (= (length calls) 1))
            (should (eq (cdar calls) 'edits))
            (should (eq (car (car calls)) session))
            (should (eq (niwa-session-permission-mode session) 'edits)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "generic helper updates global default when no session is current"
  (niwa-test--with-saved-permission-mode
    (let ((calls nil))
      (set-default-toplevel-value 'niwa-permission-mode 'ask)
      (with-temp-buffer
        (niwa-permission-mode-set-session-scoped
         'niwa-permission-mode 'edits
         (lambda (s v) (push (cons s v) calls))))
      (should-not calls)
      (should (eq (default-toplevel-value 'niwa-permission-mode)
                  'edits))))

  :doc "an unbound symbol gains its global default even inside a session"
  ;; The defcustom's initial evaluation runs the setter with the
  ;; standard value; when the owning file first loads from a session
  ;; buffer, routing the value only into the session slot left the
  ;; symbol void for the rest of the process -- `niwa-sandbox-mode'
  ;; stayed unbound after a hooks test loaded the sandbox through an
  ;; execution helper.
  (let ((sym (intern "niwa-test--scoped-unbound-var"))
        (data-buf (generate-new-buffer " *mev-test-data*")))
    (unwind-protect
        (let ((session (niwa-session--create
                        :name "test" :permission-mode 'ask))
              slot)
          (makunbound sym)
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (niwa-permission-mode-set-session-scoped
             sym 'best-effort
             (lambda (_s v) (setq slot v))))
          (should (eq slot 'best-effort))
          (should (default-boundp sym))
          (should (eq (default-toplevel-value sym) 'best-effort)))
      (makunbound sym)
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(niwa-deftest niwa-permission-mode--get ()
  ,test
  (test)
  :doc "no session context: returns the global default"
  (niwa-test--with-saved-permission-mode
    (set-default-toplevel-value 'niwa-permission-mode 'full-auto)
    (with-temp-buffer
      (should (eq (niwa-permission-mode--get 'niwa-permission-mode)
                  'full-auto))))

  :doc "from data buffer: returns the session slot, not the global default"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'edits)))
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (set-default-toplevel-value 'niwa-permission-mode 'ask)
            (with-current-buffer data-buf
              (should (eq (niwa-permission-mode--get 'niwa-permission-mode)
                          'edits))))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "from view buffer: back-pointer resolves to the session slot"
  (niwa-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (view-buf (generate-new-buffer " *mev-test-view*")))
      (unwind-protect
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'edits)))
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq-local niwa--data-buffer data-buf))
            (set-default-toplevel-value 'niwa-permission-mode 'ask)
            (with-current-buffer view-buf
              (should (eq (niwa-permission-mode--get 'niwa-permission-mode)
                          'edits))))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))))))

(niwa-deftest niwa-permission-mode ()
  ,test
  (test)
  :doc ":local 'permanent makes variable auto-buffer-local"
  (should (local-variable-if-set-p 'niwa-permission-mode))
  :doc ":local 'permanent sets permanent-local property"
  (should (get 'niwa-permission-mode 'permanent-local))
  :doc "buffer-local binding survives kill-all-local-variables"
  (let ((buf (generate-new-buffer " *mev-test-permanent*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local niwa-permission-mode 'full-auto)
          (kill-all-local-variables)
          (should (local-variable-p 'niwa-permission-mode))
          (should (eq niwa-permission-mode 'full-auto)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(niwa-deftest niwa-permission-mode-for-context ()
  ,test
  (test)
  :doc "captured baseline request cannot inherit a newer implementation turn"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'ask))
           (old (niwa-request--create :session session))
           (implementation (niwa-request--create
                            :session session :permission-mode-override 'full-auto)))
      (setq-local niwa--session session
                  niwa--current-request implementation)
      (should (eq 'full-auto (niwa-permission-mode-effective session)))
      (should (eq 'ask (niwa-permission-mode-for-context
                       (list :session session :buffer (current-buffer) :request old))))
      (should (eq 'ask (niwa-permission-mode-for-context
                       (list :session session :buffer (current-buffer)
                             :request nil :invocation nil))))
      (should (eq 'ask (niwa-session-permission-mode session)))))

  :doc "captured scoped authority survives Goal pause and buffer replacement"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'ask))
           (request (niwa-request--create
                     :session session :permission-mode-override 'edits)))
      (setq-local niwa--session session
                  niwa--current-request (niwa-request--create :session session))
      (should (eq 'edits (niwa-permission-mode-for-context
                         (list :session session :data-buffer (current-buffer)
                               :request request))))
      (should (eq 'required (niwa-sandbox-mode-effective
                            session (niwa-permission-mode-for-context
                                     (list :session session :request request)))))
      (setf (niwa-session-permission-mode session) 'full-auto)
      (should (eq 'edits (niwa-permission-mode-for-context
                         (list :session session :request request))))))

  :doc "an older agent invocation retains baseline rather than root override"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'ask))
           (old (niwa-agent-invocation--create :parent-session session))
           (child (niwa-agent-invocation--create
                   :parent-session session :permission-mode-override 'full-auto)))
      (setq-local niwa--session session
                  niwa--current-request
                  (niwa-request--create :session session :permission-mode-override 'full-auto))
      (should (eq 'ask (niwa-permission-mode-for-context
                       (list :session session :buffer (current-buffer) :invocation old))))
      (should (eq 'full-auto (niwa-permission-mode-for-context
                             (list :session session :invocation child)))))))

(provide 'test-niwa-permission-mode)
;;; test-niwa-permission-mode.el ends here
