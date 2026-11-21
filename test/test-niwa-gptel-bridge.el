;;; test-niwa-gptel-bridge.el -- Tests for gptel bridge -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-transient)
(require 'niwa-session-persistence)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'niwa-cockpit)
(require 'niwa-gptel-bridge)
(require 'niwa-hooks)
(require 'niwa-mentions)
(require 'niwa-permissions)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-history)
(require 'niwa-workspace)

;; `gptel'
(defvar gptel-model)
(defvar gptel--set-buffer-locally)
(defvar gptel-system-prompt)
(defvar gptel-tools)

;; `gptel-transient'
(declare-function gptel--set-with-scope "ext:gptel-transient"
                  (sym value &optional scope))

;; `transient'
(defvar transient--prefix)
(defvar transient-post-exit-hook)

(defmacro niwa-gptel-bridge-test--with-buffers (&rest body)
  "Execute BODY with a paired data and view buffer."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-gptel-bridge-root-" t))
          (niwa-user-dir
           (file-name-as-directory
            (make-temp-file "niwa-gptel-bridge-user-" t)))
          (workspace (niwa-workspace-get-or-create
                      'project (format "gptel-bridge-%s" root) root "niwa"))
          (session (niwa-session-create "main" workspace))
          (data-buf (generate-new-buffer " *gptel-bridge-data*"))
          (view-buf (generate-new-buffer " *gptel-bridge-view*")))
     (unwind-protect
         (progn
           (niwa-gptel-bridge--clear-return-state)
           (niwa-gptel-bridge--cleanup-advice)
           (when (get-buffer "*gptel-prompt*")
             (kill-buffer "*gptel-prompt*"))
           (with-current-buffer data-buf
             (org-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local niwa--session session)
             (setq-local gptel-model 'gpt-5.5)
             (setq-local gptel-tools '(read edit)))
           (niwa-view--setup view-buf data-buf)
           ,@body)
       (niwa-gptel-bridge--clear-return-state)
       (niwa-gptel-bridge--cleanup-advice)
       (when (get-buffer "*gptel-prompt*")
         (kill-buffer "*gptel-prompt*"))
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p niwa-user-dir)
         (delete-directory niwa-user-dir t))
       (when (file-directory-p root)
         (delete-directory root t)))))

(niwa-deftest niwa-gptel-bridge-open ()
  ,test
  (test)
  :doc "view-launched bridge runs gptel-menu in the data buffer"
  (niwa-gptel-bridge-test--with-buffers
    (let ((window (selected-window))
          called-buffer
          called-prompt
          called-window-buffer)
      (with-current-buffer data-buf
        (setq-local gptel-system-prompt "data prompt")
        (setq-local gptel-model 'data-model)
        (setq-local gptel-tools '(data-tool)))
      (with-current-buffer view-buf
        (setq-local gptel-system-prompt "view prompt")
        (setq-local gptel-model 'view-model)
        (setq-local gptel-tools '(view-tool)))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)
                               called-prompt gptel-system-prompt
                               called-window-buffer
                               (window-buffer (selected-window)))
                         (with-current-buffer called-window-buffer
                           (gptel--set-with-scope
                            'gptel-system-prompt "bridge prompt" t)
                           (gptel--set-with-scope
                            'gptel-model 'bridge-model t)
                           (gptel--set-with-scope
                            'gptel-tools '(bridge-tool) t)))))
              (with-current-buffer view-buf
                (niwa-gptel-bridge-open)))
            (niwa-gptel-bridge--return-to-view))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))
      (should (eq called-buffer data-buf))
      (should (eq called-window-buffer data-buf))
      (should (equal called-prompt "data prompt"))
      (with-current-buffer data-buf
        (should gptel--set-buffer-locally)
        (should (local-variable-p 'gptel--set-buffer-locally))
        (should (equal gptel-system-prompt "bridge prompt"))
        (should (eq gptel-model 'bridge-model))
        (should (equal gptel-tools '(bridge-tool))))
      (with-current-buffer view-buf
        (should (equal gptel-system-prompt "view prompt"))
        (should (eq gptel-model 'view-model))
        (should (equal gptel-tools '(view-tool))))))

  :doc "transient exit restores the launching view window"
  (niwa-gptel-bridge-test--with-buffers
    (let ((window (selected-window)))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (set-window-buffer window data-buf))))
              (with-current-buffer view-buf
                (niwa-gptel-bridge-open)))
            (should (eq (window-buffer window) data-buf))
            (should (memq #'niwa-gptel-bridge--return-to-view
                          transient-post-exit-hook))
            (niwa-gptel-bridge--return-to-view)
            (should (eq (window-buffer window) view-buf)))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))))

  :doc "raw data-buffer bridge does not schedule view restoration"
  (niwa-gptel-bridge-test--with-buffers
    (cl-letf (((symbol-function 'gptel-menu) #'ignore))
      (with-current-buffer data-buf
        (niwa-gptel-bridge-open)))
    (should-not niwa-gptel-bridge--return-view-buffer)))

(niwa-deftest niwa-gptel-bridge--return-to-view ()
  ,test
  (test)
  :doc "prompt editing keeps restoration pending until data is displayed"
  (niwa-gptel-bridge-test--with-buffers
    (let ((window (selected-window))
          (prompt-buffer (get-buffer-create "*gptel-prompt*")))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (niwa-gptel-bridge--schedule-return-to-view view-buf data-buf)
            (with-current-buffer prompt-buffer
              (niwa-gptel-bridge--return-to-view))
            (should (eq niwa-gptel-bridge--return-view-buffer view-buf))
            (should (memq #'niwa-gptel-bridge--return-to-view
                          transient-post-exit-hook))
            (set-window-buffer window data-buf)
            (niwa-gptel-bridge--return-to-view)
            (should (eq (window-buffer window) view-buf))
            (should-not niwa-gptel-bridge--return-view-buffer))
        (when (buffer-live-p prompt-buffer)
          (kill-buffer prompt-buffer))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))))

  :doc "restores non-origin windows changed to the data buffer"
  (niwa-gptel-bridge-test--with-buffers
    (let ((configuration (current-window-configuration))
          (other-buf (generate-new-buffer " *test-gptel-bridge-other*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((left-window (selected-window))
                   (right-window (split-window-right)))
              (set-window-buffer left-window other-buf)
              (set-window-buffer right-window view-buf)
              (select-window right-window)
              (niwa-gptel-bridge--schedule-return-to-view
               view-buf data-buf)
              (should (eq niwa-gptel-bridge--return-window
                          right-window))
              (set-window-buffer left-window data-buf)
              (select-window left-window)
              (niwa-gptel-bridge--return-to-view)
              (should (eq (window-buffer left-window) other-buf))
              (should (eq (window-buffer right-window) view-buf))
              (should (eq (selected-window) right-window))
              (should-not niwa-gptel-bridge--return-view-buffer)))
        (when (window-configuration-p configuration)
          (set-window-configuration configuration))
        (when (buffer-live-p other-buf)
          (kill-buffer other-buf))))))

(niwa-deftest niwa-gptel-bridge--schedule-return-to-view ()
  ,test
  (test)
  :doc "restores the pending view when only its data buffer differs"
  (niwa-gptel-bridge-test--with-buffers
    (let ((configuration (current-window-configuration))
          (other-data (generate-new-buffer " *gptel-bridge-data-3*")))
      (unwind-protect
          (let ((window (selected-window)))
            (set-window-buffer window view-buf)
            (niwa-gptel-bridge--schedule-return-to-view view-buf data-buf)
            (set-window-buffer window data-buf)
            ;; The same view, paired with another data buffer, replaces the
            ;; window state just as thoroughly.
            (niwa-gptel-bridge--schedule-return-to-view view-buf other-data)
            (should (eq (window-buffer window) view-buf))
            (should (eq niwa-gptel-bridge--return-data-buffer other-data)))
        (when (window-configuration-p configuration)
          (set-window-configuration configuration))
        (when (buffer-live-p other-data) (kill-buffer other-data)))))

  :doc "restores an abandoned pending view before taking over the state"
  (niwa-gptel-bridge-test--with-buffers
    (let ((configuration (current-window-configuration))
          (other-view (generate-new-buffer " *gptel-bridge-view-2*"))
          (other-data (generate-new-buffer " *gptel-bridge-data-2*")))
      (unwind-protect
          (let ((window (selected-window)))
            (set-window-buffer window view-buf)
            (niwa-gptel-bridge--schedule-return-to-view view-buf data-buf)
            ;; The gptel command replaced the view with the raw data buffer
            ;; and the user abandoned it, leaving the restoration pending.
            (set-window-buffer window data-buf)
            (niwa-gptel-bridge--schedule-return-to-view
             other-view other-data)
            (should (eq (window-buffer window) view-buf))
            (should (eq niwa-gptel-bridge--return-view-buffer other-view))
            (should (eq niwa-gptel-bridge--return-data-buffer other-data)))
        (when (window-configuration-p configuration)
          (set-window-configuration configuration))
        (dolist (buffer (list other-view other-data))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(niwa-deftest niwa-gptel-bridge--edit-directive-advice ()
  ,test
  (test)
  :doc "leaves an edit outside the pending pair in its own buffer"
  ;; A pending restoration is one view's.  An edit launched from anywhere
  ;; else must not have its settings applied to that view's data buffer.
  (niwa-gptel-bridge-test--with-buffers
    (let (callback callback-buffer)
      (niwa-gptel-bridge--schedule-return-to-view view-buf data-buf)
      (with-temp-buffer
        (niwa-gptel-bridge--edit-directive-advice
         (lambda (&rest args)
           (setq callback (plist-get (cdr args) :callback)))
         'gptel-system-prompt
         :callback (lambda (_message) (setq callback-buffer (current-buffer)))))
      (should callback)
      (with-temp-buffer
        (let ((unrelated (current-buffer)))
          (funcall callback "new prompt")
          (should (eq callback-buffer unrelated))))))

  :doc "edit-directive callback runs in the data buffer"
  (niwa-gptel-bridge-test--with-buffers
    (let (callback callback-buffer callback-prompt)
      (with-current-buffer data-buf
        (setq-local gptel-system-prompt "data prompt"))
      (with-current-buffer view-buf
        (setq-local gptel-system-prompt "view prompt"))
      (niwa-gptel-bridge--schedule-return-to-view view-buf data-buf)
      ;; The bridge switches to the data buffer before gptel's menu runs,
      ;; so that is where the edit is launched from.
      (with-current-buffer data-buf
        (niwa-gptel-bridge--edit-directive-advice
         (lambda (&rest args)
           (setq callback (plist-get (cdr args) :callback)))
         'gptel-system-prompt
         :callback
         (lambda (_message)
           (setq callback-buffer (current-buffer)
                 callback-prompt gptel-system-prompt))))
      (should callback)
      (with-temp-buffer
        (setq-local gptel-system-prompt "prompt buffer")
        (funcall callback "new prompt"))
      (should (eq callback-buffer data-buf))
      (should (equal callback-prompt "data prompt"))
      (should-not (niwa-gptel-bridge--active-p))))

  :doc "edit-directive callback restores windows before reopening menu"
  (niwa-gptel-bridge-test--with-buffers
    (let ((configuration (current-window-configuration))
          (other-buf (generate-new-buffer " *test-gptel-bridge-callback*"))
          callback-message
          callback-buffer
          callback-window)
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((left-window (selected-window))
                   (right-window (split-window-right)))
              (set-window-buffer left-window other-buf)
              (set-window-buffer right-window view-buf)
              (with-current-buffer data-buf
                (setq-local gptel-system-prompt "data prompt"))
              (select-window right-window)
              (niwa-gptel-bridge--schedule-return-to-view
               view-buf data-buf)
              (let (callback)
                (niwa-gptel-bridge--edit-directive-advice
                 (lambda (&rest args)
                   (setq callback (plist-get (cdr args) :callback)))
                 'gptel-system-prompt
                 :callback
                 (lambda (message)
                   (setq callback-message message
                         callback-buffer (current-buffer)
                         callback-window (selected-window))
                   (should (equal gptel-system-prompt "data prompt"))))
                (set-window-buffer left-window data-buf)
                (select-window left-window)
                (funcall callback "new prompt")
                (should (equal callback-message "new prompt"))
                (should (eq callback-buffer data-buf))
                (should (eq callback-window right-window))
                (should (eq (window-buffer left-window) other-buf))
                (should (eq (window-buffer right-window) view-buf)))))
        (niwa-gptel-bridge--clear-return-state)
        (when (window-configuration-p configuration)
          (set-window-configuration configuration))
        (when (buffer-live-p other-buf)
          (kill-buffer other-buf)))))

  :doc "keeps bridge state when the callback opens another transient"
  (niwa-gptel-bridge-test--with-buffers
    (let (callback)
      (niwa-gptel-bridge--schedule-return-to-view view-buf data-buf)
      (with-current-buffer data-buf
        (niwa-gptel-bridge--edit-directive-advice
         (lambda (&rest args)
           (setq callback (plist-get (cdr args) :callback)))
         'gptel-system-prompt
         :callback
         (lambda (_message) nil)))
      (should callback)
      (let ((transient--prefix t))
        (funcall callback "new prompt"))
      (should (niwa-gptel-bridge--active-p)))))

(niwa-deftest niwa-gptel-bridge--cleanup-advice ()
  ,test
  (test)
  :doc "keeps temporary advice while view restoration is pending"
  (niwa-gptel-bridge-test--with-buffers
    (niwa-gptel-bridge--schedule-return-to-view view-buf data-buf)
    (niwa-gptel-bridge--install-advice)
    (niwa-gptel-bridge--cleanup-advice)
    (should (advice-member-p
             #'niwa-gptel-bridge--edit-directive-advice
             'gptel--edit-directive))
    (should (memq #'niwa-gptel-bridge--cleanup-advice
                  transient-post-exit-hook)))

  :doc "removes temporary advice after restoration clears"
  (niwa-gptel-bridge-test--with-buffers
    (niwa-gptel-bridge--install-advice)
    (niwa-gptel-bridge--clear-return-state)
    (niwa-gptel-bridge--cleanup-advice)
    (should-not
     (advice-member-p
      #'niwa-gptel-bridge--edit-directive-advice
      'gptel--edit-directive))
    (should-not (memq #'niwa-gptel-bridge--cleanup-advice
                      transient-post-exit-hook))))

(niwa-deftest niwa-gptel-bridge--steering-buffer ()
  ,test
  (test)
  :doc "resolves request ownership even from a tool inspection buffer"
  (niwa-gptel-bridge-test--with-buffers
    (with-temp-buffer
      (should-not (niwa-gptel-bridge--steering-buffer))
      (let ((info (list :buffer data-buf)))
        (should (eq data-buf (niwa-gptel-bridge--steering-buffer info)))
        (let ((gptel--fsm-last (gptel-make-fsm :info info)))
          (should (eq data-buf (niwa-gptel-bridge--steering-buffer))))))))

(niwa-deftest niwa-gptel-bridge--steer-advice (:quiet t)
  ,test
  (test)
  :doc "native steering runs hooks and queues FIFO without changing the draft"
  (niwa-gptel-bridge-test--with-buffers
    (let* ((draft "> first line\nsecond line")
           (info (list :buffer data-buf :niwa-request-id "bridge-steer"
                       :backend (gptel--make-openai) :data (list :messages [])
                       :history '(TRET)))
           (fsm (gptel-make-fsm :state 'TOOL :info info))
           (request (niwa-request--create
                     :id "bridge-steer" :session session :fsm fsm))
           (niwa-user-prompt-submit-functions
            (list (lambda (_event) '(:additional-context "prepared by hook")))))
      (with-current-buffer data-buf
        (setq-local niwa--current-request request
                    niwa--workspace workspace
                    gptel--fsm-last fsm))
      (with-current-buffer view-buf
        (niwa-view-composer-insert draft))
      (niwa-session-set-pending-input-paused session t)
      ;; Equal text must still preserve an independently authored draft.
      (dolist (text (list "first steering" draft))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) text)))
          (with-current-buffer data-buf
            (niwa-gptel-bridge--steer-advice #'ert-fail))))
      (let ((entries (niwa-session-pending-steering session)))
        (should (equal (mapcar (lambda (entry) (plist-get entry :input)) entries)
                       (list "first steering" draft)))
        (should (string-match-p "prepared by hook"
                                (plist-get (car entries) :model-input))))
      (niwa-tools--handle-steering-inject fsm)
      (should (= 2 (length (niwa-session-pending-steering session))))
      (should (= 0 (length (plist-get (plist-get info :data) :messages))))
      (should-not (plist-get info :steering-message))
      (should-not (plist-get info :post-tool))
      (with-current-buffer view-buf
        (should (equal draft (niwa-view-composer-draft-text))))))

  :doc "hook rejection preserves the draft and leaves both steering stores empty"
  (niwa-gptel-bridge-test--with-buffers
    (let* ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer data-buf)))
           (request (niwa-request--create :id "blocked" :session session
                                            :fsm fsm))
           (niwa-user-prompt-submit-functions
            (list (lambda (_event) '(:continue nil :stop-reason "blocked")))))
      (with-current-buffer data-buf
        (setq-local niwa--current-request request
                    gptel--fsm-last fsm))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "> retained\ndraft"))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "blocked text")))
        (with-current-buffer data-buf
          (niwa-gptel-bridge--steer-advice #'ert-fail)))
      (should-not (niwa-session-pending-steering session))
      (should-not (plist-get (gptel-fsm-info fsm) :steering-message))
      (with-current-buffer view-buf
        (should (equal "> retained\ndraft" (niwa-view-composer-draft-text))))))

  :doc "retained-agent native steering is refused before creating a TSTR message"
  (niwa-gptel-bridge-test--with-buffers
    (setf (niwa-session-workspace session)
          (niwa-workspace--create :root "/workspace/steering/"))
    (with-current-buffer data-buf
      (let ((niwa--agent-invocation 'retained-agent))
        (cl-letf (((symbol-function 'read-string) #'ert-fail))
          (should
           (equal
            (list 'user-error "niwa [/workspace/steering/]: Use SendMessage to steer a retained agent")
            (should-error (niwa-gptel-bridge--steer-advice #'ert-fail)
                          :type 'user-error))))))
    (should-not (niwa-session-pending-steering session)))

  :doc "blank input preserves pending entries and unrelated gptel delegates"
  (niwa-gptel-bridge-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--current-request
                  (niwa-request--create :id "blank" :session session)))
    (niwa-session-set-pending-inputs session 'steering '((:input "queued")))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " \n ")))
      (with-current-buffer data-buf
        (niwa-gptel-bridge--steer-advice #'ert-fail)))
    (should (equal '((:input "queued"))
                   (niwa-session-pending-steering session))))
  (with-temp-buffer
    (should (equal '(original arg)
                   (niwa-gptel-bridge--steer-advice
                    (lambda (&rest args) (cons 'original args)) 'arg))))

  :doc "a request replaced during minibuffer input cannot receive the steering"
  (niwa-gptel-bridge-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--current-request
                  (niwa-request--create :id "before" :session session))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (with-current-buffer data-buf
                     (setq niwa--current-request
                           (niwa-request--create :id "after" :session session)))
                   "for the previous turn")))
        (should-error (niwa-gptel-bridge--steer-advice #'ert-fail)
                      :type 'user-error)))
    (should-not (niwa-session-pending-steering session)))

  :doc "delayed preparation preserves a new draft and rejects a replaced request"
  (dolist (replace-request '(nil t))
    (niwa-gptel-bridge-test--with-buffers
      (let* ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer data-buf)))
             (request (niwa-request--create :id "delayed" :session session
                                              :fsm fsm))
             continuation)
        (with-current-buffer data-buf
          (setq-local niwa--current-request request))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "old input"))
                  ((symbol-function 'niwa-hooks-run-event)
                   (lambda (event _plist callback &rest _)
                     (should (eq event 'UserPromptSubmit))
                     (setq continuation callback))))
          (with-current-buffer data-buf
            (niwa-gptel-bridge--steer-advice #'ert-fail)))
        (should (functionp continuation))
        (when replace-request
          (with-current-buffer data-buf
            (setq niwa--current-request
                  (niwa-request--create :id "replacement" :session session))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "> newly authored\ndraft"))
        (funcall continuation nil)
        (should (eq (not replace-request)
                    (not (null (niwa-session-pending-steering session)))))
        (with-current-buffer view-buf
          (should-not niwa-view--prompt-hook-pending)
          (should (equal "> newly authored\ndraft" (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa-gptel-bridge--tool-steer-advice ()
  ,test
  (test)
  :doc "refuses native confirmation steering by its owning request"
  (niwa-gptel-bridge-test--with-buffers
    (with-temp-buffer
      (let* ((info (list :buffer data-buf))
             (overlay (make-overlay (point-min) (point-max))))
        (overlay-put overlay 'info info)
        (should-error
         (niwa-gptel-bridge--tool-steer-advice #'ert-fail 'calls overlay)
         :type 'user-error)
        (should-not (plist-get info :steering-message)))))
  :doc "preserves ordinary gptel confirmation arguments"
  (with-temp-buffer
    (let ((info (list :buffer (current-buffer))))
      (should (equal (list 'calls nil info)
                     (niwa-gptel-bridge--tool-steer-advice
                      #'list 'calls nil info))))))

(niwa-deftest niwa-gptel-bridge-install ()
  ,test
  (test)
  :doc "both native command entrances route through niwa until uninstalled"
  (let ((installed (advice-member-p #'niwa-gptel-bridge--steer-advice
                                    'gptel-send--steer)))
    (unwind-protect
        (progn
          (niwa-gptel-bridge-install)
          (niwa-gptel-bridge-install)
          (niwa-gptel-bridge-test--with-buffers
            (with-current-buffer data-buf
              (let ((niwa--agent-invocation 'retained-agent))
                (dolist (command '(gptel-send--steer gptel--suffix-steer))
                  (should-error (funcall command) :type 'user-error)))))
          (should (advice-member-p #'niwa-gptel-bridge--tool-steer-advice
                                   'gptel--steer-tool-calls)))
      (unless installed (niwa-gptel-bridge-uninstall)))))

(niwa-deftest niwa-gptel-bridge-uninstall ()
  ,test
  (test)
  :doc "uninstall removes routing from every native steering entry"
  (let ((installed (advice-member-p #'niwa-gptel-bridge--steer-advice
                                    'gptel-send--steer)))
    (unwind-protect
        (progn
          (niwa-gptel-bridge-install)
          (niwa-gptel-bridge-uninstall)
          (dolist (command '(gptel-send--steer gptel--suffix-steer))
            (should-not
             (advice-member-p #'niwa-gptel-bridge--steer-advice command)))
          (should-not
           (advice-member-p #'niwa-gptel-bridge--tool-steer-advice
                            'gptel--steer-tool-calls)))
      (when installed (niwa-gptel-bridge-install)))))

(provide 'test-niwa-gptel-bridge)

;;; test-niwa-gptel-bridge.el ends here
