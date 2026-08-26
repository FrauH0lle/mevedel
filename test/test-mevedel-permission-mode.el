;;; test-mevedel-permission-mode.el -- Permission mode tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests session-scoped permission mode normalization, transitions, and UI
;; lifecycle behavior.

;;; Code:

(require 'mevedel-permission-mode)
(require 'mevedel-plan-mode)
(require 'mevedel-reminders)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-view-composer)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-test--with-saved-permission-mode (&rest body)
  "Execute BODY with the global `mevedel-permission-mode' preserved."
  (declare (indent 0) (debug t))
  `(let ((mevedel-test--saved-mode
          (default-toplevel-value 'mevedel-permission-mode)))
     (unwind-protect (progn ,@body)
       (set-default-toplevel-value 'mevedel-permission-mode
                                   mevedel-test--saved-mode))))


;;
;;; Mode decisions

(mevedel-deftest mevedel-permission-mode-normalize ()
  ,test
  (test)
  :doc "accepts only canonical permission modes"
  (dolist (mode '(ask edits full-auto))
    (should (eq mode (mevedel-permission-mode-normalize mode)))
    (should (eq mode
                (mevedel-permission-mode-normalize (symbol-name mode)))))
  :doc "rejects every superseded mode and alias"
  (dolist (mode '(default accept-edits trust-all auto edit))
    (should-error (mevedel-permission-mode-normalize mode)
                  :type 'user-error)))

(mevedel-deftest mevedel-permission-mode-transition ()
  ,test
  (test)
  :doc "an explicit permission choice exits Plan"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :name "main" :permission-mode 'edits :plan-mode t)))
      (setq-local mevedel--session session
                  mevedel-permission-mode 'edits)
      (mevedel-permission-mode-transition 'ask)
      (should-not (mevedel-session-plan-mode session))
      (should (eq 'ask (mevedel-session-permission-mode session)))))

  :doc "keeps a committed transition when an incomplete view cannot repaint"
  (let ((data-buffer (generate-new-buffer " *mev-mode-data*"))
        (view-buffer (generate-new-buffer " *mev-mode-view*")))
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "main" :permission-mode 'edits)))
          (with-current-buffer view-buffer
            (insert "> quoted\nsecond line"))
          (with-current-buffer data-buffer
            (setq-local mevedel--session session
                        mevedel--view-buffer view-buffer
                        mevedel-permission-mode 'edits)
            (should (eq 'ask (mevedel-permission-mode-transition 'ask))))
          (should (eq 'ask (mevedel-session-permission-mode session)))
          (with-current-buffer view-buffer
            (should (equal "> quoted\nsecond line" (buffer-string)))))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-permission-mode-decision ()
  ,test
  (test)
  :doc "full-auto allows everything"
  (progn
    (should (eq (mevedel-permission-mode-decision 'full-auto nil) 'allow))
    (should (eq (mevedel-permission-mode-decision 'full-auto t) 'allow)))
  :doc "edits allows read-only and native edits but asks for other writes"
  (progn
    (should (eq (mevedel-permission-mode-decision 'edits t) 'allow))
    (should (eq (mevedel-permission-mode-decision 'edits nil t) 'allow))
    (should (eq (mevedel-permission-mode-decision 'edits nil nil) 'ask)))
  :doc "ask allows read-only and reviewed edits but asks for other writes"
  (progn
    (should (eq (mevedel-permission-mode-decision 'ask t) 'allow))
    (should (eq (mevedel-permission-mode-decision 'ask nil) 'ask))
    (should (eq (mevedel-permission-mode-decision 'ask nil t t) 'allow))))

(mevedel-deftest mevedel-permission-mode-effective ()
  ,test
  (test)
  :doc "uses session mode before data-buffer and global fallbacks"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (set-default-toplevel-value 'mevedel-permission-mode 'full-auto)
            (with-current-buffer data-buf
              (setq-local mevedel--session
                          (mevedel-session--create
                           :name "data" :permission-mode 'edits)))
            (should (eq (mevedel-permission-mode-effective session data-buf)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "uses data-buffer session when no session is passed"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
    (unwind-protect
        (with-current-buffer data-buf
          (setq-local mevedel--session
                      (mevedel-session--create
                       :name "data" :permission-mode 'edits))
          (should (eq (mevedel-permission-mode-effective nil data-buf)
                      'edits)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "uses data-buffer local mode before global fallback"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
      (unwind-protect
          (progn
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-buf
              (setq-local mevedel-permission-mode 'full-auto))
            (should (eq (mevedel-permission-mode-effective nil data-buf)
                        'full-auto)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "uses explicit surface buffer local mode before data-buffer fallback"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*"))
        (surface-buf (generate-new-buffer " *mev-effective-surface*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (setq-local mevedel-permission-mode 'ask))
          (with-current-buffer surface-buf
            (setq-local mevedel-permission-mode 'full-auto))
          (should (eq (mevedel-permission-mode-effective
                       nil data-buf surface-buf)
                      'full-auto)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (buffer-live-p surface-buf) (kill-buffer surface-buf))))

  :doc "ignores caller local mode when a data-buffer is explicit"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer data-buf
            (setq-local mevedel-permission-mode 'ask))
          (setq-local mevedel-permission-mode 'full-auto)
          (should (eq (mevedel-permission-mode-effective nil data-buf)
                      'ask)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "falls back to the global mode"
  (mevedel-test--with-saved-permission-mode
    (set-default-toplevel-value 'mevedel-permission-mode 'edits)
    (should (eq (mevedel-permission-mode-effective) 'edits))))

(mevedel-deftest mevedel-permission-mode-label ()
  ,test
  (test)
  :doc "renders compact labels for permission modes"
  (should (equal "ask" (mevedel-permission-mode-label 'ask)))
  (should (equal "edits" (mevedel-permission-mode-label 'edits)))
  (should (equal "full-auto" (mevedel-permission-mode-label 'full-auto)))
  (should (equal "ask" (mevedel-permission-mode-label 'unknown))))

(mevedel-deftest mevedel-permission-mode--set ()
  ,test
  (test)
  :doc "no session context: updates the global default only"
  (mevedel-test--with-saved-permission-mode
    (set-default-toplevel-value 'mevedel-permission-mode 'ask)
    (with-temp-buffer
      (mevedel-permission-mode--set 'mevedel-permission-mode 'full-auto))
    (should (eq (default-toplevel-value 'mevedel-permission-mode) 'full-auto)))

  :doc "from data buffer: updates session slot and its buffer-local, leaves default untouched"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-buf
              (mevedel-permission-mode--set 'mevedel-permission-mode 'edits))
            (should (eq (mevedel-session-permission-mode session) 'edits))
            (should (eq (buffer-local-value 'mevedel-permission-mode data-buf)
                        'edits))
            ;; Global default is NOT touched.
            (should (eq (default-toplevel-value 'mevedel-permission-mode)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "from view buffer: back-pointer resolves to the session; data + view locals both update"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (view-buf (generate-new-buffer " *mev-test-view*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--view-buffer view-buf))
            (with-current-buffer view-buf
              (setq-local mevedel--data-buffer data-buf))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer view-buf
              (mevedel-permission-mode--set
               'mevedel-permission-mode 'edits))
            (should (eq (mevedel-session-permission-mode session) 'edits))
            (should (eq (buffer-local-value 'mevedel-permission-mode data-buf)
                        'edits))
            (should (eq (buffer-local-value 'mevedel-permission-mode view-buf)
                        'edits))
            (should (eq (default-toplevel-value 'mevedel-permission-mode)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (when (buffer-live-p view-buf) (kill-buffer view-buf)))))

  :doc "interactive aliases are rejected below the user-input boundary"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (should-error
               (mevedel-permission-mode--set
                'mevedel-permission-mode 'edit)
               :type 'user-error))
            (should (eq (mevedel-session-permission-mode session)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "multiple sessions: only the current session is modified"
  (mevedel-test--with-saved-permission-mode
    (let ((data-a (generate-new-buffer " *mev-test-a*"))
          (data-b (generate-new-buffer " *mev-test-b*")))
      (unwind-protect
          (let ((sess-a (mevedel-session--create
                         :name "a" :permission-mode 'ask))
                (sess-b (mevedel-session--create
                         :name "b" :permission-mode 'edits)))
            (with-current-buffer data-a
              (setq-local mevedel--session sess-a))
            (with-current-buffer data-b
              (setq-local mevedel--session sess-b))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-a
              (mevedel-permission-mode--set 'mevedel-permission-mode 'full-auto))
            (should (eq (mevedel-session-permission-mode sess-a) 'full-auto))
            (should (memq 'full-auto-mode
                          (mapcar #'mevedel-reminder-type
                                  (mevedel-session-reminders sess-a))))
            ;; Session B and the global default are untouched.
            (should (eq (mevedel-session-permission-mode sess-b) 'edits))
            (should (eq (default-toplevel-value 'mevedel-permission-mode)
                        'ask))
            ;; Buffer-local was set in data-a, not in data-b.
            (should (eq (buffer-local-value 'mevedel-permission-mode data-a)
                        'full-auto))
            (should-not (local-variable-p 'mevedel-permission-mode data-b)))
        (when (buffer-live-p data-a) (kill-buffer data-a))
        (when (buffer-live-p data-b) (kill-buffer data-b)))))

  :doc "setq-local bypasses setter: session slot stays stale"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-permission-mode 'full-auto))
            (should (eq (buffer-local-value 'mevedel-permission-mode data-buf)
                        'full-auto))
            (should (eq (mevedel-session-permission-mode session) 'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(mevedel-deftest mevedel-permission-mode-set-session-scoped ()
  ,test
  (test)
  :doc "generic helper routes to slot-setter when a session is current"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (calls nil))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer data-buf
              (mevedel-permission-mode-set-session-scoped
               'mevedel-permission-mode 'edits
               (lambda (s v) (push (cons s v) calls)
                 (setf (mevedel-session-permission-mode s) v))))
            (should (= (length calls) 1))
            (should (eq (cdar calls) 'edits))
            (should (eq (car (car calls)) session))
            (should (eq (mevedel-session-permission-mode session) 'edits)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "generic helper updates global default when no session is current"
  (mevedel-test--with-saved-permission-mode
    (let ((calls nil))
      (set-default-toplevel-value 'mevedel-permission-mode 'ask)
      (with-temp-buffer
        (mevedel-permission-mode-set-session-scoped
         'mevedel-permission-mode 'edits
         (lambda (s v) (push (cons s v) calls))))
      (should-not calls)
      (should (eq (default-toplevel-value 'mevedel-permission-mode)
                  'edits))))

  :doc "an unbound symbol gains its global default even inside a session"
  ;; The defcustom's initial evaluation runs the setter with the
  ;; standard value; when the owning file first loads from a session
  ;; buffer, routing the value only into the session slot left the
  ;; symbol void for the rest of the process -- `mevedel-sandbox-mode'
  ;; stayed unbound after a hooks test loaded the sandbox through an
  ;; execution helper.
  (let ((sym (intern "mevedel-test--scoped-unbound-var"))
        (data-buf (generate-new-buffer " *mev-test-data*")))
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test" :permission-mode 'ask))
              slot)
          (makunbound sym)
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (mevedel-permission-mode-set-session-scoped
             sym 'best-effort
             (lambda (_s v) (setq slot v))))
          (should (eq slot 'best-effort))
          (should (default-boundp sym))
          (should (eq (default-toplevel-value sym) 'best-effort)))
      (makunbound sym)
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-permission-mode--get ()
  ,test
  (test)
  :doc "no session context: returns the global default"
  (mevedel-test--with-saved-permission-mode
    (set-default-toplevel-value 'mevedel-permission-mode 'full-auto)
    (with-temp-buffer
      (should (eq (mevedel-permission-mode--get 'mevedel-permission-mode)
                  'full-auto))))

  :doc "from data buffer: returns the session slot, not the global default"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'edits)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-buf
              (should (eq (mevedel-permission-mode--get 'mevedel-permission-mode)
                          'edits))))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "from view buffer: back-pointer resolves to the session slot"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (view-buf (generate-new-buffer " *mev-test-view*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'edits)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--data-buffer data-buf))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer view-buf
              (should (eq (mevedel-permission-mode--get 'mevedel-permission-mode)
                          'edits))))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))))))

(mevedel-deftest mevedel-permission-mode ()
  ,test
  (test)
  :doc ":local 'permanent makes variable auto-buffer-local"
  (should (local-variable-if-set-p 'mevedel-permission-mode))
  :doc ":local 'permanent sets permanent-local property"
  (should (get 'mevedel-permission-mode 'permanent-local))
  :doc "buffer-local binding survives kill-all-local-variables"
  (let ((buf (generate-new-buffer " *mev-test-permanent*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-permission-mode 'full-auto)
          (kill-all-local-variables)
          (should (local-variable-p 'mevedel-permission-mode))
          (should (eq mevedel-permission-mode 'full-auto)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(mevedel-deftest mevedel--implementation-permission-mode-apply ()
             ,test
             (test)

             :doc "temporarily applies and restores implementation permission mode"
             (let* ((session (mevedel-session--create
                              :name "test"
                              :workspace nil
                              :permission-mode 'ask
                              :permission-rules nil
                              :permission-queue nil
                              :pending-plan-approval nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-view-refresh-associated-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (mevedel--implementation-permission-mode-apply 'edits)
                       (should (eq 'edits
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(ask)
                                      mevedel--implementation-permission-mode-saved))
                       (mevedel--implementation-permission-mode-restore)
                       (should (eq 'ask
                                   (mevedel-session-permission-mode session)))
                       (should-not mevedel--implementation-permission-mode-saved)
                       (should (= 2 refreshed))))
                 (when (buffer-live-p buffer) (kill-buffer buffer))))

             :doc "temporarily applies explicit ask mode over restored full-auto mode"
             (let* ((session (mevedel-session--create
                              :name "test"
                              :workspace nil
                              :permission-mode 'full-auto
                              :permission-rules nil
                              :permission-queue nil
                              :pending-plan-approval nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-view-refresh-associated-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (mevedel--implementation-permission-mode-apply 'ask)
                       (should (eq 'ask
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(full-auto)
                                      mevedel--implementation-permission-mode-saved))
                       (mevedel--implementation-permission-mode-restore)
                       (should (eq 'full-auto
                                   (mevedel-session-permission-mode session)))
                       (should-not mevedel--implementation-permission-mode-saved)
                       (should (= 2 refreshed))))
                 (when (buffer-live-p buffer) (kill-buffer buffer))))

             :doc "restores inherited global permission mode as nil session override"
             (let* ((session (mevedel-session--create
                              :name "test"
                              :workspace nil
                              :permission-mode nil
                              :permission-rules nil
                              :permission-queue nil
                              :pending-plan-approval nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (mevedel-permission-mode 'ask)
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-view-refresh-associated-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (setq-local mevedel-permission-mode nil)
                       (mevedel--implementation-permission-mode-apply 'full-auto)
                       (should (eq 'full-auto
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(nil)
                                      mevedel--implementation-permission-mode-saved))
                       (mevedel--implementation-permission-mode-restore)
                       (should-not (mevedel-session-permission-mode session))
                       (should-not (local-variable-p 'mevedel-permission-mode
                                                     buffer))
                       (should (eq 'ask mevedel-permission-mode))
                       (should-not mevedel--implementation-permission-mode-saved)
                       (should (= 2 refreshed))))
                 (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'test-mevedel-permission-mode)
;;; test-mevedel-permission-mode.el ends here
