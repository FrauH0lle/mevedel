;;; test-niwa-view-interaction.el --- Interaction-zone tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests managed interaction-zone registration and redraw behavior.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-transcript)
(require 'niwa-structs)
(require 'niwa-pipeline)
(require 'niwa-permissions)
(require 'niwa-tool-registry)
(require 'niwa-workspace)
(require 'niwa-session-persistence)
(require 'niwa-turn)
(require 'niwa-tool-ui)
(require 'niwa-permission-queue)
(require 'niwa-permission-prompt)
(require 'niwa-goal)
(require 'niwa-agents)
(require 'niwa-agent-control)
(require 'niwa-agent-runtime)
(require 'niwa-view-zone)

(defun test-niwa-view-interaction--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun test-niwa-view-interaction--caret-offset (view)
  "Return the composer caret offset from VIEW's draft start."
  (with-current-buffer (niwa-view-composer-buffer view)
    (- (point) (niwa-view-composer-draft-start view))))

(defun test-niwa-view-interaction--select-draft (draft offset)
  "Show and select the current view's composer with DRAFT at OFFSET."
  (let ((window (niwa-view-composer-display (selected-window))))
    (select-window window)
    (niwa-view-test--insert-composer-draft draft offset)
    window))

(niwa-deftest niwa-view--interaction-sync-focus ()
  (save-window-excursion ,test)
  (test)

  :doc "arrival preserves draft keys; explicit visit restores a multiline composer"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let* ((view-window (selected-window))
           (composer (test-niwa-view-interaction--select-draft "> quoted\nsecond line" 4))
           outcome)
      (with-current-buffer data-buf
        (niwa--prompt-user-with-overlay
         "Confirm" "Do the thing" "Proceed?" nil
         (lambda (value) (setq outcome value))))
      (should (eq composer (selected-window)))
      (should (= 4 (test-niwa-view-interaction--caret-offset view-buf)))
      (should-not (eq (key-binding (kbd "RET")) #'niwa--approve-request))
      (niwa-view-jump-to-interaction)
      (should (eq view-window (selected-window)))
      (should (get-text-property (point) 'niwa-prompt-focus))
      (should (eq (key-binding (kbd "RET")) #'niwa--approve-request))
      (niwa-view--interaction-rebuild)
      (should (get-text-property (point) 'niwa-prompt-focus))
      (call-interactively (key-binding (kbd "RET")))
      (should (eq outcome 'approve))
      (should (eq composer (selected-window)))
      (should (= (window-point composer) (+ (niwa-view-composer-draft-start view-buf) 4)))
      (should (equal (niwa-view-composer-draft-text view-buf t) "> quoted\nsecond line"))
      (should-not (buffer-local-value 'niwa-view--interaction-focus-return view-buf))))

  :doc "permission approval returns to the saved composer position"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let ((composer (test-niwa-view-interaction--select-draft "> draft\nnext" 3))
          outcome)
      (with-current-buffer data-buf
        (niwa-permission--prompt-async-with-content
         "Allow operation?\n" nil (lambda (value) (setq outcome value))))
      (should (= 3 (test-niwa-view-interaction--caret-offset view-buf)))
      (niwa-view-jump-to-interaction)
      (should (get-text-property (point) 'niwa-prompt-focus))
      (call-interactively (key-binding (kbd "RET")))
      (should (eq outcome 'allow-once))
      (should (eq composer (selected-window)))
      (should (= 3 (test-niwa-view-interaction--caret-offset view-buf)))
      (should (equal (niwa-view-composer-draft-text view-buf t) "> draft\nnext"))))

  :doc "Ask keeps focus through answer redraw and returns after submission"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let ((composer (test-niwa-view-interaction--select-draft "> draft\nnext" 3))
          answer)
      (with-current-buffer data-buf
        (niwa-tool-ask-ui-show
         (lambda (value) (setq answer value))
         [(:question "Run tests?" :options ["Yes" "No"])]))
      (should (= 3 (test-niwa-view-interaction--caret-offset view-buf)))
      (niwa-view-jump-to-interaction)
      (should (get-text-property (point) 'niwa-prompt-focus))
      (call-interactively (key-binding (kbd "RET")))
      (should-not answer)
      (should (eq 'submit (get-text-property (point) 'niwa-ask-entry)))
      (call-interactively (key-binding (kbd "C-c C-c")))
      (should (string-match-p "A1: Yes" answer))
      (should (eq composer (selected-window)))
      (should (= 3 (test-niwa-view-interaction--caret-offset view-buf)))
      (should (equal (niwa-view-composer-draft-text view-buf t) "> draft\nnext"))))

  :doc "a prompt leaves a reader in history and never refocuses on redraw"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (goto-char (point-min))
    (with-current-buffer data-buf
      (niwa--prompt-user-with-overlay
       "Confirm" "Do the thing" "Proceed?" nil #'ignore))
    (with-current-buffer view-buf
      (should (= (point) (point-min)))
      (niwa-view--interaction-rebuild)
      (should (= (point) (point-min)))
      (should-not niwa-view--interaction-focus-return)
      (niwa--prompt-dismiss-all)))

  :doc "cancel restores composer and a second prompt does not take existing focus"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let ((composer (test-niwa-view-interaction--select-draft "" 0)))
      (with-current-buffer data-buf
        (niwa--prompt-user-with-overlay
         "First" "One" "Proceed?" nil #'ignore))
      (should (eq composer (selected-window)))
      (niwa-view-jump-to-interaction)
      (let ((first (car niwa--prompt-overlays)))
        (with-current-buffer data-buf
          (niwa--prompt-user-with-overlay
           "Second" "Two" "Proceed?" nil #'ignore))
        (should (eq first (niwa--prompt--overlay-at-point
                           'niwa-user-request)))
        (niwa--prompt--settle first 'aborted)
        (should (eq composer (selected-window)))
        (should (= 0 (test-niwa-view-interaction--caret-offset view-buf)))
        (with-current-buffer view-buf
          (should-not niwa-view--interaction-focus-return)
          (niwa--prompt-dismiss-all))))))

(niwa-deftest niwa-view-jump-to-interaction ()
  (save-window-excursion ,test)
  (test)

  :doc "cycling preserves the original draft offset and leaves other windows alone"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let* ((other-view (split-window-right))
           (other (niwa-view-composer-display other-view))
           (composer (test-niwa-view-interaction--select-draft "draft\nnext" 3))
           (start (niwa-view-composer-draft-start view-buf)))
      (set-window-point other-view (with-current-buffer view-buf (point-min)))
      (set-window-point other (+ start 1))
      (with-current-buffer data-buf
        (niwa--prompt-user-with-overlay "First" "One" "Proceed?" nil #'ignore)
        (niwa--prompt-user-with-overlay "Second" "Two" "Proceed?" nil #'ignore))
      (should (eq composer (selected-window)))
      (should (= (point) (+ start 3)))
      (should (= (window-point other) (+ start 1)))
      (niwa-view-jump-to-interaction)
      (let ((first (get-text-property (point) 'niwa-view-interaction-id)))
        (niwa-view-jump-to-interaction)
        (should-not (equal first (get-text-property
                                 (point) 'niwa-view-interaction-id)))
        (niwa-view-jump-to-interaction)
        (should (equal first (get-text-property
                             (point) 'niwa-view-interaction-id))))
      (call-interactively (key-binding (kbd "RET")))
      (should (eq composer (selected-window)))
      (should (= (window-point composer) (+ start 3)))
      (should (= (window-point other) (+ start 1)))
      (with-current-buffer view-buf
        (should (= (window-point other-view) (point-min)))
        (niwa--prompt-dismiss-all))))

  :doc "only the selected visitor returns; the other draft caret stays put"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let* ((other-view (split-window-right))
           (other (niwa-view-composer-display other-view))
           (first (test-niwa-view-interaction--select-draft "draft\nnext" 3))
           (start (niwa-view-composer-draft-start view-buf)))
      (set-window-point other (+ start 1))
      (with-current-buffer data-buf
        (niwa--prompt-user-with-overlay "First" "One" "Proceed?" nil #'ignore))
      (niwa-view-jump-to-interaction)
      (with-selected-window other
        (niwa-view-jump-to-interaction)
        (should (eq other-view (selected-window))))
      (set-window-point other (+ start 2))
      (call-interactively (key-binding (kbd "RET")))
      (should (eq first (selected-window)))
      (should (= (window-point first) (+ start 3)))
      (should (= (window-point other) (+ start 2)))
      (should-not (buffer-local-value 'niwa-view--interaction-focus-return view-buf))))

  :doc "the first visit takes the card nearest the composer, then cycles outward"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (niwa-view-test--insert-composer-draft "draft" 2)
    (test-niwa-view-interaction--card 'oldest 'permission 1)
    (test-niwa-view-interaction--card 'middle 'plan 1)
    (test-niwa-view-interaction--card 'newest 'ask 1)
    (test-niwa-view-interaction--card 'queued 'pending-input 1)
    (let (visits)
      (dotimes (_ 4)
        (niwa-view-jump-to-interaction)
        (push (get-text-property (point) 'niwa-view-interaction-id) visits))
      (should (equal '(oldest middle newest oldest) (nreverse visits)))))

  :doc "settlement does not pull back a window that left the visited card"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let ((view-window (selected-window)))
      (test-niwa-view-interaction--select-draft "draft\nnext" 3)
      (with-current-buffer data-buf
        (niwa--prompt-user-with-overlay "First" "One" "Proceed?" nil #'ignore))
      (niwa-view-jump-to-interaction)
      (let ((prompt (car niwa--prompt-overlays)))
        (goto-char (point-min))
        (niwa--prompt--settle prompt 'aborted)
        (should (eq view-window (selected-window)))
        (should (= (point) (point-min)))
        (should (= 3 (test-niwa-view-interaction--caret-offset view-buf)))
        (should-not niwa-view--interaction-focus-return))))

  :doc "a hidden Plan approval is revealed and visited explicitly"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let* ((session (niwa-session--create :name "test"))
           (entry (list :session session :interaction-id 'plan :hidden t
                        :renderer
                        (lambda (_entry)
                          (niwa-view--interaction-register
                           '(:kind plan :id plan :count 1 :body "approval\n"))))))
      (setq-local niwa--session session)
      (with-current-buffer data-buf (setq-local niwa--session session))
      (setf (niwa-session-pending-plan-approval session) entry)
      (test-niwa-view-interaction--select-draft "draft" 2)
      (niwa-view-jump-to-interaction)
      (should-not (plist-get entry :hidden))
      (should (eq 'plan (get-text-property (point) 'niwa-view-interaction-id)))
      (should (= 2 (nth 2 (assq (selected-window)
                                niwa-view--interaction-focus-return))))))

  :doc "no pending card reports a user error without moving point"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let ((composer (test-niwa-view-interaction--select-draft "" 0)))
      (should-error (niwa-view-jump-to-interaction) :type 'user-error)
      (should (eq composer (selected-window)))
      (should (= 0 (test-niwa-view-interaction--caret-offset view-buf))))))

(niwa-deftest niwa-view--permission-absence-render
  (:doc "the expired-queue notice informs without disturbing the composer")
  ,test
  (test)

  :doc "the hold shows one notice, keeps the draft, and clearing removes it"
  (niwa-view-test--with-buffers
    (let ((niwa-permission-queue-timeout 3600)
          (session (niwa-session--create :name "absence")))
      (switch-to-buffer view-buf)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (setf (niwa-session-root-buffer session) data-buf)
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
        (setf (niwa-session-permission-absence session)
              (list :expired-at (float-time) :count 3))
        (niwa-view--interaction-rebuild)
        (should (string-match-p "3 permission requests expired at"
                                (buffer-string)))
        (should (string-match-p "Send a message to resume"
                                (buffer-string)))
        (should (niwa-view-interaction-pending-p))
        (should-not (niwa-view--interaction-active-work-paused-p))
        (should (equal (niwa-view-composer-draft-text view-buf t) "> quoted\nsecond line"))
        (should (= 4 (test-niwa-view-interaction--caret-offset view-buf)))
        (niwa-view--interaction-rebuild)
        (should (= 2 (length (split-string
                              (buffer-string)
                              "permission requests expired at"))))
        (niwa-permission-absence-clear session)
        (should-not (string-match-p "permission requests expired"
                                    (buffer-string)))
        (should-not (niwa-view-interaction-pending-p))
        (should (equal (niwa-view-composer-draft-text view-buf t) "> quoted\nsecond line"))
        (should (= 4 (test-niwa-view-interaction--caret-offset view-buf)))))))

(niwa-deftest niwa-view-interaction-ownership ()
  ,test
  (test)
  (dolist (symbol '(niwa-view--interaction-register
                    niwa-view--interaction-rebuild
                    niwa-view--interaction-clear))
    (should (equal "niwa-view-interaction"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))

(niwa-deftest niwa-view-interaction-initialize
  (:doc "creates fresh interaction state for each view")
  ,test
  (test)

  :doc "initialization replaces stale descriptor and overlay tables"
  (with-temp-buffer
    (setq-local niwa-view--interaction-descriptors
                (let ((table (make-hash-table :test #'equal)))
                  (puthash 'stale t table)
                  table))
    (setq-local niwa-view--interaction-overlays
                niwa-view--interaction-descriptors)
    (niwa-view-interaction-initialize)
    (should (hash-table-p niwa-view--interaction-descriptors))
    (should (hash-table-p niwa-view--interaction-overlays))
    (should-not (eq niwa-view--interaction-descriptors
                    niwa-view--interaction-overlays))
    (should (= 0 (hash-table-count niwa-view--interaction-descriptors)))
    (should (= 0 (hash-table-count niwa-view--interaction-overlays))))

  :doc "setting up a live view again releases its earlier interaction overlays"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "reinitialize"))
           (record (niwa-agent-record--create
                    :path "/root/worker" :activity 'running
                    :invocation (niwa-agent-invocation--create
                                 :path "/root/worker")))
           overlay)
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq overlay (niwa-view--interaction-register
                       '(:kind ask :id ask :origin "/root/worker" :body "ask"))))
      (should (= 1 (length (niwa-agent-record-blockers record))))
      (niwa-view--setup view-buf data-buf)
      (should-not (overlay-buffer overlay))
      (with-current-buffer view-buf
        (should-not (seq-find
                     (lambda (ov) (overlay-get ov 'niwa-view-interaction-id))
                     (overlays-in (point-min) (point-max)))))
      (should-not (niwa-agent-record-blockers record)))))

(niwa-deftest niwa-view--show-pending-plan
  (:doc "uses the clicked view and focuses hidden or visible approvals")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "test"))
           (render-count 0)
           (entry
            (list
             :session session
             :interaction-id 'plan
             :hidden t
             :renderer
             (lambda (_entry)
               (cl-incf render-count)
               (niwa-view--interaction-register
                '(:kind plan :id plan :count 1 :body "approval\n"))))))
      (setf (niwa-session-pending-plan-approval session) entry)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--interaction-rebuild))
      (save-window-excursion
        (let* ((source-window (selected-window))
               (view-window (split-window source-window))
               position)
          (set-window-buffer source-window data-buf)
          (set-window-buffer view-window view-buf)
          (with-current-buffer view-buf
            (setq position
                  (text-property-any
                   (point-min) (point-max)
                   'niwa-view-pending-plan t)))
          (select-window source-window)
          (niwa-view--show-pending-plan
           (list 'mouse-1 (list view-window position '(0 . 0) 0)))
          (should (eq view-window (selected-window)))
          (should (eq view-buf (current-buffer)))
          (should-not (plist-get entry :hidden))
          (should (= 1 render-count))
          (let ((start
                 (plist-get
                  (niwa-view-zone-fragment-bounds 'interaction 'plan)
                  :start)))
            (should (= start (point)))
            (goto-char (point-max))
            (niwa-view--show-pending-plan 13)
            (should (= start (point)))
            (should (= 1 render-count))))))))

(niwa-deftest niwa-view-interaction-pending-p
  (:doc "reports pending view-owned user interactions")
  ,test
  (test)

  :doc "detects prompt-hook and registered descriptor state"
  (with-temp-buffer
    (niwa-view-interaction-initialize)
    (should-not (niwa-view-interaction-pending-p))
    (setq-local niwa-view--prompt-hook-pending t)
    (should (niwa-view-interaction-pending-p))
    (setq-local niwa-view--prompt-hook-pending nil)
    (puthash 'ask '(:kind ask) niwa-view--interaction-descriptors)
    (should (niwa-view-interaction-pending-p (current-buffer))))

  :doc "rejects a dead view buffer"
  (let ((view (generate-new-buffer " *dead-interaction-view*")))
    (kill-buffer view)
    (should-not (niwa-view-interaction-pending-p view))))

(niwa-deftest niwa-view--interaction-pauses-active-work-p ()
  ,test
  (test)
  :doc "classifies actionable kinds and honors explicit overrides"
  (dolist (kind '(ask permission plan preview request))
    (should (niwa-view--interaction-pauses-active-work-p
             (list :kind kind))))
  (should-not
   (niwa-view--interaction-pauses-active-work-p
    '(:kind pending-input)))
  (should-not
   (niwa-view--interaction-pauses-active-work-p
    '(:kind preview :active-work-paused nil))))

(niwa-deftest niwa-view--interaction-active-work-paused-p ()
  ,test
  (test)
  :doc "detects visible actionable descriptors"
  (with-temp-buffer
    (niwa-view-interaction-initialize)
    (should-not (niwa-view--interaction-active-work-paused-p))
    (puthash 'ask '(:kind ask) niwa-view--interaction-descriptors)
    (should (niwa-view--interaction-active-work-paused-p)))

  :doc "detects hidden pending Plan approval"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "active-work")))
      (setf (niwa-session-pending-plan-approval session)
            '(:hidden t))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (should (niwa-view--interaction-active-work-paused-p))))))

(niwa-deftest niwa-view-interaction-blocking-p ()
  ,test
  (test)
  :doc "distinguishes user decisions from non-blocking interaction UI"
  (with-temp-buffer
    (niwa-view-interaction-initialize)
    (should-not (niwa-view-interaction-blocking-p))
    (puthash 'pending-input '(:kind pending-input)
             niwa-view--interaction-descriptors)
    (should-not (niwa-view-interaction-blocking-p))
    (puthash 'ask '(:kind ask) niwa-view--interaction-descriptors)
    (should (niwa-view-interaction-blocking-p))
    (remhash 'ask niwa-view--interaction-descriptors)
    (setq-local niwa-view--prompt-hook-pending t)
    (should (niwa-view-interaction-blocking-p))))

(niwa-deftest niwa-view--interaction-register ()
  ,test
  (test)
  :doc "replacement clears optional metadata while retaining the callback handle"
  (dolist (cleared '(nil (:keymap nil :help-echo nil :entry nil :activate nil)))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (let* ((base '(:kind preview :id preview :body "Preview"))
               (overlay
                (niwa-view--interaction-register
                 (append base (list :keymap (make-sparse-keymap)
                                    :help-echo "Details" :entry 'entry
                                    :activate #'ignore :read-only nil)))))
          (overlay-put overlay 'test-private-state 'retained)
          (dolist (property '(keymap help-echo niwa-view-interaction-entry
                             niwa-view-interaction-activate))
            (should (overlay-get overlay property)))
          (should-not (overlay-get overlay 'read-only))
          (should (eq overlay
                      (niwa-view--interaction-register
                       (append base cleared))))
          (dolist (property '(keymap help-echo niwa-view-interaction-entry
                             niwa-view-interaction-activate))
            (should-not (overlay-get overlay property)))
          (should (overlay-get overlay 'read-only))
          (should (eq 'retained (overlay-get overlay 'test-private-state)))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text view-buf t)))
          (should (= 4 (test-niwa-view-interaction--caret-offset view-buf)))))))

  :doc "acquires one blocker per non-permission interaction identity"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "interaction-state"))
           (invocation
            (niwa-agent-invocation--create :path "/root/worker"))
           (record
            (niwa-agent-record--create
             :path "/root/worker" :activity 'running
             :invocation invocation)))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--interaction-register
         '(:kind ask :id ask :origin "/root/worker" :body "ask"))
        (niwa-view--interaction-register
         '(:kind ask :id ask :origin "/root/worker" :body "updated"))
        (should (eq 'interaction-blocked
                    (niwa-agent-record-activity record)))
        (should (= 1 (length (niwa-agent-record-blockers record))))
        (niwa-view--interaction-register
         '(:kind permission :id permission :origin "/root/worker"
           :body "permission"))
        (should (= 1 (length (niwa-agent-record-blockers record)))))))

  :doc "actionable interactions pause active time without changing the draft"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "active-work-pause"))
           (request
            (niwa-request--create
             :session session
             :started-at (time-subtract
                          (current-time) (seconds-to-time 20))))
           (draft "> quoted\nsecond line"))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request request))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft draft 4)
        (dolist (kind '(ask permission plan preview request))
          (niwa-view--interaction-register
           (list :kind kind :id kind :origin "/root" :body "prompt"))
          (should (niwa-request-active-work-pause-started-at request))
          (should (equal draft (niwa-view-composer-draft-text view-buf t)))
          (when (memq kind '(ask preview request))
            (let ((started
                   (niwa-request-active-work-pause-started-at request)))
              (niwa-view--interaction-rebuild)
              (should
               (equal started
                      (niwa-request-active-work-pause-started-at request)))))
          (niwa-view--interaction-unregister kind)
          (should-not
           (niwa-request-active-work-pause-started-at request)))
        (niwa-view--interaction-register
         '(:kind pending-input :id pending-input :body "queued"))
        (should-not (niwa-request-active-work-pause-started-at request))
        (niwa-view--interaction-unregister 'pending-input)
        (niwa-view--interaction-register
         '(:kind preview :id fork :active-work-paused nil :body "fork"))
        (should-not (niwa-request-active-work-pause-started-at request))
        (niwa-view--interaction-unregister 'fork)
        (niwa-view--interaction-register
         '(:kind ask :id ask :origin "/root" :body "ask"))
        (let ((started
               (niwa-request-active-work-pause-started-at request)))
          (niwa-view--interaction-register
           '(:kind preview :id preview :origin "/root" :body "preview"))
          (niwa-view--interaction-unregister 'ask)
          (should (equal
                   started
                   (niwa-request-active-work-pause-started-at request))))
        (niwa-view--interaction-unregister 'preview)
        (should-not (niwa-request-active-work-pause-started-at request))
        (niwa-view--interaction-register
         '(:kind plan :id clear :origin "/root" :body "plan"))
        (should (niwa-request-active-work-pause-started-at request))
        (niwa-view--interaction-clear)
        (should-not (niwa-request-active-work-pause-started-at request))
        (niwa-view--interaction-register
         '(:kind request :id teardown :origin "/root" :body "request"))
        (niwa-request-push-canceller
         request
         (lambda ()
           (with-current-buffer view-buf
             (niwa-view--interaction-unregister 'teardown))))
        (with-current-buffer data-buf
          (niwa-request-end))
        (should-not (niwa-request-active-work-pause-started-at request))
        (should (equal draft (niwa-view-composer-draft-text view-buf t)))))))

(niwa-deftest niwa-view--interaction-unregister ()
  ,test
  (test)
  :doc "releases overlapping interactions independently and in any order"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "interaction-release"))
           (invocation
            (niwa-agent-invocation--create :path "/root/worker"))
           (record
            (niwa-agent-record--create
             :path "/root/worker" :activity 'running
             :invocation invocation)))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--interaction-register
         '(:kind ask :id ask :origin "/root/worker" :body "ask"))
        (niwa-view--interaction-register
         '(:kind preview :id preview :origin "/root/worker" :body "diff"))
        (should (= 2 (length (niwa-agent-record-blockers record))))
        (niwa-view--interaction-unregister 'ask)
        (should (eq 'interaction-blocked
                    (niwa-agent-record-activity record)))
        (niwa-view--interaction-unregister 'preview)
        (should (eq 'running (niwa-agent-record-activity record)))
        (should-not (niwa-agent-record-blockers record))))))

(niwa-deftest niwa-view--interaction-lifecycle-close
  (:doc "records base and effective permission modes at open and close")
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create
                    :name "interaction-telemetry"
                    :permission-mode 'full-auto))
          events)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-telemetry-record)
                   (lambda (_session event &rest props)
                     (push (cons event props) events))))
          (niwa-view--interaction-register
           '(:kind permission :id permission :origin "/root" :body "allow"))
          (niwa-view--interaction-unregister 'permission)
          (niwa-view--interaction-register
           '(:kind preview :id fork :active-work-paused nil :body "fork"))
          (niwa-view--interaction-unregister 'fork))
        (let ((opened 0))
          (dolist (event events)
            (should (eq 'full-auto
                        (plist-get (cdr event) :permission-mode-base)))
            (should (eq 'full-auto
                        (plist-get (cdr event) :permission-mode-effective)))
            (when (eq (car event) 'interaction-opened)
              (cl-incf opened)
              (pcase (plist-get (cdr event) :kind)
                ('permission
                 (should (plist-get (cdr event) :active-work-paused)))
                ('preview
                 (should-not
                  (plist-get (cdr event) :active-work-paused))))))
          (should (= 2 opened)))
        (should (assq 'interaction-closed events))))))

(niwa-deftest niwa-view--interaction-delete-overlay ()
  ,test
  (test)
  :doc "releases an overlay activity token at most once while deleting it"
  (with-temp-buffer
    (let ((overlay (make-overlay (point-min) (point-min)))
          (releases 0))
      (overlay-put overlay 'niwa-agent-activity-release
                   (lambda () (cl-incf releases)))
      (niwa-view--interaction-delete-overlay overlay)
      (niwa-view--interaction-delete-overlay overlay)
      (should (= 1 releases))
      (should-not (overlay-buffer overlay))
      (should-not
       (overlay-get overlay 'niwa-agent-activity-release)))))

(defun test-niwa-view-interaction--card (id kind lines)
  "Register a navigatable KIND card ID of LINES lines; return its body text."
  (let ((body (mapconcat (lambda (n) (format "%s line %d" id n))
                         (number-sequence 1 lines) "\n")))
    (niwa-view--interaction-register
     (list :kind kind :id id :count 1 :keymap (make-sparse-keymap)
           :body (concat body "\n")))
    body))

(defun test-niwa-view-interaction--card-start (id)
  "Return the buffer start of interaction card ID."
  (plist-get (niwa-view-zone-fragment-bounds 'interaction id) :start))

(niwa-deftest niwa-view--interaction-width-changed ()
  ,test
  (test)
  :doc "a narrower window re-fits the separator rule to one row"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (niwa-view-test--insert-composer-draft "> draft\nnext" 3)
      ;; A direct card; queue-backed ones return from their session on rebuild.
      (test-niwa-view-interaction--card 'card 'ask 1)
      (cl-flet ((separator-rows ()
                  (let ((start (plist-get (niwa-view-zone-fragment-bounds
                                           'interaction :separator)
                                          :start)))
                    (save-excursion
                      (goto-char start)
                      (count-screen-lines start (line-end-position))))))
        (should (= 1 (separator-rows)))
        (split-window-right 30)
        (niwa-view-zone--on-window-change nil)
        (should (= 1 (separator-rows))))
      (should (equal "> draft\nnext" (niwa-view-composer-draft-text view-buf t)))
      (should (= 3 (test-niwa-view-interaction--caret-offset view-buf))))))

(niwa-deftest niwa-view--interaction-rank ()
  ,test
  (test)

  :doc "cards stack newest farthest from the composer; pending input stays pinned"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nnext" 3)
      (test-niwa-view-interaction--card 'queued 'pending-input 1)
      (test-niwa-view-interaction--card 'old 'plan 1)
      (test-niwa-view-interaction--card 'mid 'ask 1)
      (test-niwa-view-interaction--card 'new 'permission 1)
      (cl-flet ((order ()
                  (sort (list 'queued 'old 'mid 'new)
                        :key #'test-niwa-view-interaction--card-start)))
        (should (equal '(new mid old queued) (order)))
        ;; Re-registration under the same id, as rebuilds do, keeps its place.
        (test-niwa-view-interaction--card 'old 'plan 2)
        (should (equal '(new mid old queued) (order)))
        ;; A card that closed and returns arrives anew.
        (niwa-view--interaction-unregister 'old)
        (test-niwa-view-interaction--card 'old 'plan 1)
        (should (equal '(old new mid queued) (order))))
      (should (equal "> draft\nnext" (niwa-view-composer-draft-text view-buf t)))
      (should (= 3 (test-niwa-view-interaction--caret-offset view-buf)))))

  :doc "an arriving card leaves a visited card on its screen row"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (forward-line 1)
        (dotimes (n 200) (insert (format "transcript %d\n" n))))
      (niwa-view-test--insert-composer-draft "> draft\nnext" 3)
      (test-niwa-view-interaction--card 'visited 'ask 3)
      (recenter -1)
      (niwa-view-jump-to-interaction)
      (let ((row (count-screen-lines
                  (window-start)
                  (test-niwa-view-interaction--card-start 'visited)))
            (offset (- (point) (test-niwa-view-interaction--card-start
                                'visited))))
        ;; A lower-ranked kind under the former per-kind ordering.
        (test-niwa-view-interaction--card 'arrived 'permission 5)
        (should (< (test-niwa-view-interaction--card-start 'arrived)
                   (test-niwa-view-interaction--card-start 'visited)))
        (should (= row (count-screen-lines
                        (window-start)
                        (test-niwa-view-interaction--card-start 'visited))))
        (should (= offset (- (point) (test-niwa-view-interaction--card-start
                                      'visited))))
        (should (equal "> draft\nnext" (niwa-view-composer-draft-text view-buf t)))))))

(niwa-deftest niwa-view--interaction-target-buffer
  (:doc "resolves the live parent view for queued interactions")
  ,test
  (test)

  :doc "agent data buffers fall back through invocation parent data"
  (let ((parent-data (generate-new-buffer " *test-parent-data-prompt*"))
        (parent-view (generate-new-buffer " *test-parent-view-prompt*"))
        (agent-data (generate-new-buffer " *test-agent-data-prompt*"))
        (session (niwa-session-create
                  "main"
                  (niwa-workspace--create
                   :type 'project :id "/tmp/prompt-view/"
                   :root "/tmp/prompt-view/" :name "prompt-view"))))
    (unwind-protect
        (let ((inv (niwa-agent-invocation--create
                    :agent-id "verifier--prompt123"
                    :parent-data-buffer parent-data
                    :buffer agent-data
                    :transcript-status 'running)))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--agent-invocation inv)
            (setq-local niwa--view-buffer nil))
          (should (eq parent-view
                      (niwa-view--interaction-target-buffer
                       agent-data)))
          (with-current-buffer agent-data
            (should (eq parent-view
                        (niwa-view--interaction-target-buffer)))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "a named owner's view hosts its interactions whatever buffer is current"
  ;; Timers and process callbacks run with the user's buffer current,
  ;; which can be another session's view.
  (niwa-view-test--with-buffers
    (let ((own-data data-buf) (own-view view-buf))
      (niwa-view-test--with-buffers
        (with-current-buffer view-buf
          (should (eq own-view (niwa-view--interaction-target-buffer own-data)))
          ;; Without an owner, the current view hosts it.
          (should (eq view-buf (niwa-view--interaction-target-buffer))))
        (with-current-buffer own-view (kill-buffer))
        ;; An owner without a view never borrows another session's.
        (with-current-buffer view-buf
          (should-error (niwa-view--interaction-target-buffer own-data)))))))

(niwa-deftest niwa-view--interaction-zone-render
  (:doc "renders and rebuilds interaction-zone fragments")
  ,test
  (test)

  :doc "permission and plan descriptors materialize as real text"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((map (make-sparse-keymap)))
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 2
               :body "\npermission\n" :keymap map
               :help-echo "Permission" :entry 'permission-entry
               :activate #'ignore))
        (niwa-view--interaction-register
         (list :kind 'plan :id 'plan :count 1
               :body "\nplan\n" :keymap map
               :help-echo "Plan" :entry 'plan-entry
               :activate #'ignore)))
      (should (equal "1 plan · 2 permissions pending"
                     (niwa-view--interaction-count-label)))
      (goto-char (point-min))
      (search-forward "1 plan · 2 permissions pending"
                      (niwa-view-transcript-tail))
      (should (eq 'interaction (get-text-property
                                (match-beginning 0)
                                'niwa-view-zone-namespace)))
      (should (eq :separator (get-text-property
                              (match-beginning 0)
                              'niwa-view-zone-id)))
      (should (overlayp (niwa-view-zone-region 'interaction)))
      (should (string-match-p "plan" (buffer-string)))
      (should (string-match-p "permission" (buffer-string)))
      (should (string-match-p "plan\n\npermission" (buffer-string)))
      (maphash
       (lambda (_id overlay)
         (should (< (overlay-start overlay) (overlay-end overlay)))
         (should (overlay-get overlay 'niwa-view-interaction-entry))
         (should (overlay-get overlay 'niwa-view-interaction-activate))
         (should (overlay-get overlay 'keymap))
         (should-not (overlay-get overlay 'before-string))
         (should (get-text-property
                  (overlay-start overlay)
                  'niwa-view-interaction-overlay))
         (should-not (get-text-property
                      (overlay-start overlay)
                      'mouse-face)))
       niwa-view--interaction-overlays)))

  :doc "only the pending Plan counter segment is actionable"
  (niwa-view-test--with-buffers
    (let ((session
           (niwa-session--create
            :name "test"
            :pending-plan-approval
            '(:interaction-id plan :hidden t))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 2
               :body "permission\n" :entry 'permission-entry
               :activate #'ignore))
        (goto-char (point-min))
        (search-forward "1 plan · 2 permissions pending")
        (let* ((plan-position (match-beginning 0))
               (permission-position
                (+ plan-position (length "1 plan · 2 ")))
               (map (get-text-property plan-position 'keymap)))
          (should (eq 'niwa-view--show-pending-plan
                      (lookup-key map (kbd "RET"))))
          (should (lookup-key map [mouse-1]))
          (should (eq 'link (get-text-property plan-position 'face)))
          (should-not (get-text-property permission-position 'keymap))
          (should-not
           (get-text-property permission-position
                              'niwa-view-pending-plan))))))

  :doc "shared activation does not call interaction descriptor callbacks"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((called nil))
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission needs an explicit outcome\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate (lambda () (setq called t))))
        (goto-char (point-min))
        (search-forward "permission needs" (niwa-view-transcript-tail))
        (should-error (niwa-view-activate-at-point) :type 'user-error)
        (should-not called))))

  :doc "fragment migration reuses descriptor overlays and metadata"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((map (make-sparse-keymap))
             (overlay
              (niwa-view--interaction-register
               (list :kind 'preview :id 'preview :count 1
                     :body "\nold body text\n" :keymap map
                     :help-echo "Preview" :entry 'preview-entry
                     :activate #'ignore))))
        (overlay-put overlay 'test-private-state 'kept)
        (goto-char (overlay-start overlay))
        (search-forward "body" (overlay-end overlay))
        (let ((updated
               (niwa-view--interaction-register
                (list :kind 'preview :id 'preview :count 1
                      :body "\nnew body text\n" :keymap map
                      :help-echo "Preview" :entry 'preview-entry
                      :activate #'ignore))))
          (should (eq overlay updated))
          (should (eq 'kept (overlay-get overlay 'test-private-state)))
          (should (eq overlay
                      (get-text-property
                       (overlay-start overlay)
                       'niwa-view-interaction-overlay)))
          (should (get-text-property (overlay-start overlay)
                                     'niwa-view-zone-key))
          (let ((text (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
            (should (= 1 (niwa-view-test--count-substring
                          "new body text" text)))
            (should (= 0 (niwa-view-test--count-substring
                          "old body text" text))))))))

  :doc "fragment rendering normalizes body without trailing newline"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((overlay
             (niwa-view--interaction-register
              (list :kind 'preview :id 'preview :count 1
                    :body "preview body" :keymap (make-sparse-keymap)
                    :entry 'preview-entry :activate #'ignore))))
        (should (equal "preview body\n"
                       (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay)))))))

  :doc "normalizes raw UTF-8 bytes in interaction descriptor bodies"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((raw (test-niwa-view-interaction--raw-bytes
                  #xe2 #x80 #x9c ?x #xe2 #x80 #x9d)))
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body (concat "\npermission " raw "\n")
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore)))
      (should (string-match-p "permission “x”" (buffer-string)))))

  :doc "ignores interaction markers that drift before the status zone"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (set-marker niwa-view--interaction-marker (point-min))
      (niwa-view--interaction-register
       (list :kind 'permission :id 'permission :count 1
             :body "\npermission below status\n"
             :keymap (make-sparse-keymap)
             :entry 'permission-entry
             :activate #'ignore))
      (let* ((text (buffer-substring-no-properties
                    (point-min) (niwa-view-transcript-tail)))
             (header (string-trim-right
                      (niwa-view--header-string data-buf)))
             (header-pos (string-search header text))
             (permission-pos (string-search "permission below status"
                                            text)))
        (should header-pos)
        (should permission-pos)
        (should (< header-pos permission-pos))
        (should (>= (overlay-start
                     (niwa-view-zone-region 'interaction))
                    (marker-position niwa-view--status-marker))))))

  :doc "ignores jointly drifted status and interaction markers before the header"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'project
                :id "/tmp/view-stale-header/"
                :root "/tmp/view-stale-header/"
                :name "view-stale-header"))
           (session (niwa-session-create "renamed" ws)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (set-marker niwa-view--status-marker (point-min))
        (set-marker niwa-view--interaction-marker (point-min))
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "\npermission below header\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore))
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
               (header "niwa")
               (header-pos (string-search header text))
               (permission-pos (string-search "permission below header"
                                              text)))
          (should header-pos)
          (should permission-pos)
          (should (< header-pos permission-pos))
          (should (>= (overlay-start
                       (niwa-view-zone-region 'interaction))
                      (niwa-view--header-end-position)))))))

  :doc "the request spinner is on the composer, not in the interaction transcript"
  (let ((niwa-view-spinner-animate nil))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission\n" :keymap (make-sparse-keymap)
               :entry 'permission-entry :activate #'ignore))
        (niwa-view--start-spinner "Working...")
        (should (overlayp (niwa-view-zone-region 'interaction)))
        (should (string-search "Working" (niwa-view-test--rail-text)))
        (should-not (string-search "Working" (buffer-string))))))

  :doc "clear-for-rebuild removes rebuild-owned fragment text"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--interaction-register
       (list :kind 'permission :id 'permission :count 1
             :body "permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'permission-entry :activate #'ignore))
      (niwa-view--interaction-clear-for-rebuild)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should-not (string-match-p "permission prompt" text))
        (should (= 0 (hash-table-count
                      niwa-view--interaction-descriptors)))
        (should (= 0 (hash-table-count
                      niwa-view--interaction-overlays))))))

  :doc "status redraw stays above existing permission interaction"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'project :id "/tmp/view-interaction-order/"
                :root "/tmp/view-interaction-order/"
                :name "view-interaction-order"))
           (session (niwa-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission prompt\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore))
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                   (lambda ()
                     (list (list :path "/root/verifier"
                                 :status 'blocked
                                 :role "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19))))
                  ((symbol-function 'niwa--prompt-block-face)
                   (lambda () 'ask)))
          (niwa-view--render-agent-status))
        (let (agent-pos separator-pos prompt-pos)
          (save-excursion
            (goto-char (point-min))
            (search-forward "Blocked   agent    /root/verifier")
            (setq agent-pos (match-beginning 0))
            (search-forward "1 permission pending")
            (setq separator-pos (match-beginning 0))
            (search-forward "permission prompt")
            (setq prompt-pos (match-beginning 0)))
          (should (eq 'status (get-text-property
                               agent-pos
                               'niwa-view-zone-namespace)))
          (should (eq 'background (get-text-property
                                   agent-pos 'niwa-view-zone-id)))
          (should (eq 'interaction (get-text-property
                                    separator-pos
                                    'niwa-view-zone-namespace)))
          (should (eq :separator (get-text-property
                                  separator-pos 'niwa-view-zone-id)))
          (should (< agent-pos separator-pos))
          (let ((permission-overlay
                 (gethash 'permission niwa-view--interaction-overlays)))
            (should (overlayp permission-overlay))
            (should (<= separator-pos (overlay-start permission-overlay))))
          (should (<= separator-pos prompt-pos))))))

  :doc "permission prompt appears below existing agent status"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'project :id "/tmp/view-interaction-after-status/"
                :root "/tmp/view-interaction-after-status/"
                :name "view-interaction-after-status"))
           (session (niwa-session-create "main" ws))
           (outcomes nil))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                   (lambda ()
                     (list (list :path "/root/verifier"
                                 :status 'blocked
                                 :role "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19)))))
          (niwa-view--render-agent-status))
        (should (string-match-p
                 "Blocked   agent    /root/verifier"
                 (buffer-substring-no-properties
                  (point-min) (niwa-view-transcript-tail)))))
      (with-current-buffer data-buf
        (niwa-permission-request
         (list :kind 'generic
               :tool-name "Read"
               :specifier-key :path
               :specifier-value "/tmp/after-status.txt"
               :include-always nil
               :origin "/root/verifier"
               :callback (lambda (outcome) (push outcome outcomes)))
         session))
      (with-current-buffer view-buf
        (should-not outcomes)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
               (agent-pos (string-search
                           "Blocked   agent    /root/verifier" text))
               (permission-pos (string-search "Permission Request" text)))
          (should agent-pos)
          (should permission-pos)
          (should (< agent-pos permission-pos))
          (should (string-search "1 permission pending" text))
          (goto-char (point-min))
          (search-forward "1 permission pending" (niwa-view-transcript-tail))
          (should (eq :separator (get-text-property
                                  (match-beginning 0)
                                  'niwa-view-zone-id)))))))

  :doc "permission queue renders only the FIFO head while request progress is visible"
  (let ((niwa-view-spinner-animate nil))
    (niwa-view-test--with-buffers
      (let ((session (niwa-session--create
                      :name "test"
                      :workspace nil
                      :permission-rules nil
                      :permission-mode 'ask
                      :permission-queue nil
                      :pending-plan-approval nil))
            outcomes)
        (with-current-buffer data-buf
          (setq-local niwa--session session)
          (setq-local niwa--view-buffer view-buf))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (niwa-view--start-spinner "Working..."))
        (cl-letf (((symbol-function 'niwa--prompt-block-face)
                   (lambda () 'ask)))
          (with-current-buffer data-buf
            (dolist (path '("/tmp/one.el" "/tmp/two.el" "/tmp/three.el"))
              (let ((captured-path path))
                (niwa-permission-request
                 (list :kind 'generic
                       :tool-name "Read"
                       :specifier-key :path
                       :specifier-value captured-path
                       :include-always nil
                       :origin "/root"
                       :callback
                       (lambda (outcome)
                         (push (cons captured-path outcome) outcomes)))
                 session)))))
        (with-current-buffer view-buf
          (cl-labels
              ((display-text ()
                 (buffer-substring-no-properties
                  (point-min) (niwa-view-transcript-tail)))
               (head-overlay ()
                 (let* ((entry (car (niwa-session-permission-queue
                                     session)))
                        (id (niwa-queue--entry-metadata-get
                             entry :interaction-id)))
                   (and id (gethash id niwa-view--interaction-overlays))))
               (settle-head ()
                 (niwa--prompt--settle (head-overlay) 'allow-once))
               (should-show (count visible-path hidden-paths)
                 (let ((text (display-text)))
                   (should (= 1 (niwa-view-test--count-substring
                                 "Permission Request" text)))
                   (should (string-search
                            (format "%d permission%s pending"
                                    count (if (= count 1) "" "s"))
                            text))
                   (should (string-search visible-path text))
                   (dolist (path hidden-paths)
                     (should-not (string-search path text))))))
            (should-show 3 "/tmp/one.el" '("/tmp/two.el" "/tmp/three.el"))
            (settle-head)
            (should-show 2 "/tmp/two.el" '("/tmp/one.el" "/tmp/three.el"))
            (settle-head)
            (should-show 1 "/tmp/three.el" '("/tmp/one.el" "/tmp/two.el"))
            (settle-head)
            (let ((text (display-text)))
              (should (= 0 (niwa-view-test--count-substring
                            "Permission Request" text)))
              (should-not (string-search "permission pending" text))
              (should-not (string-search "permissions pending" text))
              (should-not (string-search "/tmp/one.el" text))
              (should-not (string-search "/tmp/two.el" text))
              (should-not (string-search "/tmp/three.el" text)))
            (should (equal '(("/tmp/three.el" . allow-once)
                             ("/tmp/two.el" . allow-once)
                             ("/tmp/one.el" . allow-once))
                           outcomes)))))))

  :doc "render removes interaction fragment text from a dead region overlay"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--interaction-register
       (list :kind 'permission :id 'old-permission :count 1
             :body "old permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'old-entry :activate #'ignore))
      (delete-overlay (niwa-view-zone-region 'interaction))
      (niwa-view--interaction-clear-for-rebuild)
      (niwa-view--interaction-register
       (list :kind 'permission :id 'new-permission :count 1
             :body "new permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'new-entry :activate #'ignore))
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (= 0 (niwa-view-test--count-substring
                      "old permission prompt" text)))
        (should (= 1 (niwa-view-test--count-substring
                      "new permission prompt" text))))))

  :doc "clears stale interaction prompt overlays without settling"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((outcomes nil)
            (ov (make-overlay (point-min) (point-min)
                              (current-buffer) nil t)))
        (overlay-put ov 'niwa-view-interaction-id 'stale-permission)
        (overlay-put ov 'niwa-user-request t)
        (overlay-put ov 'niwa--callback
                     (lambda (outcome) (push outcome outcomes)))
        (setq niwa--prompt-overlays (list ov))
        (niwa-view--interaction-clear)
        (should-not (overlay-buffer ov))
        (should-not niwa--prompt-overlays)
        (should-not outcomes))))

  :doc "rebuild preserves direct request and ask prompts without settling"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let (overlays
            outcomes)
        (dolist (kind '(request ask))
          (let* ((captured-kind kind)
                 (id (list captured-kind))
                 (ov
                  (niwa-view--interaction-register
                   (list :kind captured-kind
                         :id id
                         :count 0
                         :body (format "\n%s prompt\n" captured-kind)
                         :keymap (make-sparse-keymap)
                         :activate
                         (lambda (outcome)
                           (push (cons captured-kind outcome) outcomes))))))
            (overlay-put ov 'niwa-user-request t)
            (overlay-put ov 'niwa--callback
                         (lambda (outcome)
                           (push (cons captured-kind outcome) outcomes)))
            (push (cons captured-kind ov) overlays)
            (cl-pushnew ov niwa--prompt-overlays :test #'eq)))
        (niwa-view--interaction-rebuild)
        (should-not outcomes)
        (should (= 2 (length niwa--prompt-overlays)))
        (dolist (pair overlays)
          (let* ((kind (car pair))
                 (ov (cdr pair))
                 (id (list kind)))
            (should (eq ov (gethash id
                                    niwa-view--interaction-overlays)))
            (should (overlay-buffer ov))
            (should (gethash id niwa-view--interaction-descriptors))))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "request prompt" text))
          (should (string-match-p "ask prompt" text)))
        (should (equal "1 request · 1 question pending"
                       (niwa-view--interaction-count-label)))
        (niwa--prompt--settle (cdr (assq 'request overlays)) 'deny)
        (niwa--prompt--settle (cdr (assq 'ask overlays)) 'aborted)
        (should (member '(request . deny) outcomes))
        (should (member '(ask . aborted) outcomes)))))

  :doc "does not focus interaction prompt while a live window is drafting"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (goto-char (point-min))
      (let* ((view-window (selected-window))
             (composer (test-niwa-view-interaction--select-draft "> quoted\nsecond line" 4)))
        (with-current-buffer view-buf
          (let ((overlay (niwa-view--interaction-register
                          (list :kind 'permission :id 'permission :count 1
                                :body "\npermission\n" :keymap (make-sparse-keymap)
                                :help-echo "Permission" :entry 'permission-entry
                                :activate #'ignore))))
            (should (= (window-point view-window) (point-min)))
            (should (overlay-get overlay 'read-only))
            (should (get-text-property (overlay-start overlay) 'read-only))))
        (should (eq composer (selected-window)))
        (should (= 4 (test-niwa-view-interaction--caret-offset view-buf)))
        (should (string= "> quoted\nsecond line" (niwa-view-composer-draft-text view-buf t)))
        (should-not (get-text-property (niwa-view-composer-draft-start) 'read-only)))))

  :doc "redraw regression: interaction update/rebuild preserves multiline > composer and removes stale body"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      ;; Status/task redraw coverage for this composer shape lives in
      ;; `niwa-tool-task-refresh-display'; this case fills the
      ;; interaction-zone redraw gap before the fragment migration.
      (let* ((draft "> first line\nsecond line")
             (point-offset (length "> first"))
             (old-body "old preview body")
             (current-body "current preview body")
             (map (make-sparse-keymap)))
        (cl-labels
            ((display-text ()
               (buffer-substring-no-properties
                (point-min) (niwa-view-transcript-tail)))
             (should-preserve-composer ()
               (should (string= draft (niwa-view-composer-draft-text view-buf t)))
               (should (= point-offset (test-niwa-view-interaction--caret-offset view-buf)))
               (with-current-buffer (niwa-view-composer-buffer view-buf)
                 (should (< (point) (point-max)))
                 (should (equal " line"
                                (buffer-substring-no-properties
                                 (point)
                                 (min (point-max)
                                      (+ (point) (length " line"))))))
                 (should-not (get-text-property
                              (niwa-view-composer-draft-start view-buf)
                              'read-only))))
             (should-show-current-body ()
               (let ((display (display-text)))
                 (should (= 1 (niwa-view-test--count-substring
                               current-body display)))
                 (should (= 0 (niwa-view-test--count-substring
                               old-body display)))
                 (should (equal "1 preview pending"
                                (niwa-view--interaction-count-label)))))
             (should-clear-bodies ()
               (let ((display (display-text)))
                 (should (= 0 (niwa-view-test--count-substring
                               current-body display)))
                 (should (= 0 (niwa-view-test--count-substring
                               old-body display))))))
          (niwa-view-test--insert-composer-draft draft point-offset)
          (niwa-view--interaction-register
           (list :kind 'preview :id 'preview :count 1
                 :body (concat "\n" old-body "\n")
                 :keymap map :help-echo "Preview" :activate #'ignore))
          (niwa-view--interaction-register
           (list :kind 'preview :id 'preview :count 1
                 :body (concat "\n" current-body "\n")
                 :keymap map :help-echo "Preview" :activate #'ignore))
          (should-preserve-composer)
          (should-show-current-body)
          (niwa-view--interaction-rebuild)
          (should-preserve-composer)
          (should-show-current-body)
          (niwa-view--interaction-rebuild)
          (should-preserve-composer)
          (should-show-current-body)
          (niwa-view--interaction-register
           (list :kind 'preview :id 'preview :count 1
                 :body (propertize (concat "\n" current-body "\n")
                                   'read-only t)
                 :body-properties-owned t
                 :keymap map :help-echo "Preview" :activate #'ignore))
          (niwa-view--interaction-rebuild)
          (should-preserve-composer)
          (should-show-current-body)
          (niwa-view--interaction-clear)
          (should-preserve-composer)
          (should-clear-bodies)))))

  :doc "incremental history render stays above fragment-backed interaction UI"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "*** Read files\n" nil)
    (niwa-view-test--insert-data data-buf "Working through it.\n" 'response)
    (with-current-buffer view-buf
      (let ((map (make-sparse-keymap)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min) t)))
        (setq niwa-view--in-flight-turn-start
              (copy-marker niwa-view--status-marker t))
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "\npermission\n" :keymap map
               :help-echo "Permission" :entry 'permission-entry
               :activate #'ignore))
        (cl-letf (((symbol-function 'niwa-view--interaction-rebuild)
                   #'ignore))
          (niwa-view-render-live-update data-buf))
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
               (assistant-pos (string-match-p "Assistant" text))
               (permission-pos (string-match-p "permission" text)))
          (should assistant-pos)
          (should permission-pos)
          (should (< assistant-pos permission-pos)))
        (niwa-view--interaction-clear)
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "Assistant" text))
          (should (string-match-p "Working through it" text))
          (should-not (string-match-p "permission" text))))))

  :doc "rerender rebuilds from live queues without settling"
  (niwa-view-test--with-buffers
      (let* ((session (niwa-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'ask
                       :permission-queue nil
                       :pending-plan-approval nil))
             (request
              (niwa-request--create
               :session session
               :started-at (current-time)))
             (plan-outcomes nil)
             (permission-outcomes nil))
        (with-current-buffer data-buf
          (setq-local niwa--session session)
          (setq-local niwa--current-request request))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (setf (niwa-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :renderer
                            (lambda (entry)
                              (niwa-view--interaction-register
                               (list :kind 'plan :id 'plan
                                     :entry entry :activate #'ignore
                                     :body "# Plan")))
                            :callback
                            (lambda (outcome)
                              (push outcome plan-outcomes))))
          (setf (niwa-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :specifier-value "/tmp/file.txt"
                            :include-always t
                            :session session
                            :callback
                            (lambda (outcome)
                              (push outcome permission-outcomes)))))
          (niwa-view--interaction-rebuild)
          (let ((pause-started
                 (niwa-request-active-work-pause-started-at request)))
            (should pause-started)
            (niwa-view--interaction-rebuild)
            (should (equal
                     pause-started
                     (niwa-request-active-work-pause-started-at request))))
          (should-not plan-outcomes)
          (should-not permission-outcomes)
          (should (equal "1 plan · 1 permission pending"
                         (niwa-view--interaction-count-label)))
          (let ((kinds nil))
            (maphash
             (lambda (_id descriptor)
               (push (plist-get descriptor :kind) kinds)
               (should (plist-member descriptor :entry))
               (should (plist-get descriptor :activate)))
             niwa-view--interaction-descriptors)
            (should (memq 'plan kinds))
            (should (memq 'permission kinds))))))

  :doc "nested authority prompt stays root-owned and preserves the active composer"
  (niwa-view-test--with-buffers
      (let* ((session (niwa-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'ask
                       :permission-queue nil
                       :pending-plan-approval nil))
             (agent (niwa-agent--create :name "verifier"))
             (inv (niwa-agent-invocation-create agent))
             (agent-buf (generate-new-buffer " *test-agent-perm*"))
             (origin "/root/worker/verifier")
             (draft "> first line\nsecond line")
             (point-offset (length "> first"))
             (outcomes nil))
        (unwind-protect
            (progn
              (setf (niwa-agent-invocation-agent-id inv)
                    "verifier--0123456789abcdef0123456789abcdef")
              (setf (niwa-agent-invocation-path inv) origin)
              (setf (niwa-agent-invocation-parent-data-buffer inv)
                    data-buf)
              (setf (niwa-agent-invocation-parent-session inv)
                    session)
              (with-current-buffer data-buf
                (setq-local niwa--session session)
                (niwa-request-begin session))
              (with-current-buffer view-buf
                (setq-local niwa--session session)
                (niwa-view-test--insert-composer-draft
                 draft point-offset))
              (setf (niwa-session-agent-registry session)
                    (list
                     (cons "/root/worker"
                           (niwa-agent-record--create
                            :path "/root/worker" :parent-path "/root"
                            :activity 'idle))
                     (cons origin
                           (niwa-agent-record--create
                            :path origin :parent-path "/root/worker"
                            :activity 'running :invocation inv))))
              (with-current-buffer agent-buf
                (org-mode)
                (setq-local niwa--session session)
                (setq-local niwa--agent-invocation inv)
                (setq-local niwa--view-buffer view-buf)
                (niwa-request-begin session))
              (cl-letf (((symbol-function 'niwa--prompt-block-face)
                         (lambda () 'ask)))
                (with-current-buffer agent-buf
                  (niwa-permission-request
                   (list :kind 'sandbox
                         :tool-name "Bash"
                         :detail "npx @emacs-eask/cli test"
                         :sandbox-permissions 'additive
                         :justification "Run the repository tests?"
                         :show-operation-authority t
                         :operation-pending-p nil
                         :requested-additional-permissions '(:network t)
                         :missing-additional-permissions '(:network t)
                         :include-always nil
                         :origin (niwa-current-origin)
                         :request-id
                         (niwa-request-id niwa--current-request)
                         :callback
                         (lambda (outcome)
                           (push outcome outcomes)))
                   session)))
              (let* ((entry (car (niwa-session-permission-queue session)))
                     (interaction-id
                      (niwa-queue--entry-metadata-get
                       entry :interaction-id)))
                (should interaction-id)
                (should (= 1 (niwa-agent-control--active-count session)))
                (should (equal origin (plist-get entry :origin)))
                (with-current-buffer view-buf
                  (let ((overlay
                         (gethash interaction-id
                                  niwa-view--interaction-overlays)))
                    (should overlay)
                    (should
                     (equal origin
                            (overlay-get
                             overlay 'niwa-view-interaction-origin))))
                  (should (string-match-p
                           "Invocation Authority Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                  (should (string-match-p
                           (regexp-quote (format "from %s" origin))
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                  (should (string= draft (niwa-view-composer-draft-text view-buf t)))
                  (should (= point-offset (test-niwa-view-interaction--caret-offset view-buf)))
                  (should-not niwa--prompt-overlays))
                (with-current-buffer agent-buf
                  (should-not
                   (string-match-p
                    "Invocation Authority Request"
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
                (with-current-buffer data-buf
                  (niwa-request-end))
                (should-not outcomes)
                (should (= 1 (niwa-agent-control--active-count session)))
                (should (= 1 (length
                              (niwa-session-permission-queue session))))
                (with-current-buffer view-buf
                  (should (gethash interaction-id
                                   niwa-view--interaction-overlays))
                  (cl-letf (((symbol-function 'niwa--prompt-block-face)
                             (lambda () 'ask)))
                    (niwa-view--full-rerender))
                  (should-not outcomes)
                  (should (= 1 (length
                                (niwa-session-permission-queue
                                 session))))
                  (should (string-match-p
                           "Invocation Authority Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                  (should (string= draft (niwa-view-composer-draft-text view-buf t)))
                  (should (= point-offset (test-niwa-view-interaction--caret-offset view-buf))))
                (with-current-buffer agent-buf
                  (niwa-request-end))
                (should (equal '(aborted) outcomes))))
          (when (buffer-live-p agent-buf)
            (kill-buffer agent-buf)))))

  :doc "interaction target ignores transcript inspection view"
  (niwa-view-test--with-buffers
      (let* ((session (niwa-session--create :name "test"))
             (agent (niwa-agent--create :name "verifier"))
             (inv (niwa-agent-invocation-create agent))
             (agent-buf (generate-new-buffer " *test-agent-data*"))
             (agent-view (generate-new-buffer " *test-agent-view*")))
        (unwind-protect
            (progn
              (setf (niwa-agent-invocation-agent-id inv) "verifier--abc")
              (setf (niwa-agent-invocation-parent-data-buffer inv)
                    data-buf)
              (with-current-buffer data-buf
                (setq-local niwa--session session))
              (with-current-buffer view-buf
                (setq-local niwa--session session))
              (with-current-buffer agent-buf
                (org-mode)
                (setq-local niwa--session session)
                (setq-local niwa--agent-invocation inv)
                (setq-local niwa--view-buffer view-buf))
              (niwa-view--setup
               agent-view agent-buf
               (list :agent-transcript-p t
                     :agent-id "verifier--abc"
                     :parent-view view-buf
                     :preserve-data-view-buffer t
                     :transcript-info
                     (list :agent-id "verifier--abc"
                           :status 'running
                           :buffer agent-buf
                           :live-buffer t
                           :session session)))
              (with-current-buffer agent-view
                (should (eq view-buf
                            (niwa-view--interaction-target-buffer
                             agent-buf)))))
          (when (buffer-live-p agent-view) (kill-buffer agent-view))
          (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))

  :doc "all interaction kinds keep arrival order, callbacks, and a leading-> draft across redraws"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((draft "> first line\nsecond line")
             (point-offset (length "> first"))
             ;; Arrival order differs from any per-kind ranking.
             (kinds '(plan permission pending-input ask preview request))
             (bodies '((pending-input . "follow-up body")
                       (permission . "permission body")
                       (ask . "question body")
                       (request . "access body")
                       (plan . "plan body")
                       (preview . "preview body")))
             overlays
             callbacks)
        (niwa-view-test--insert-composer-draft draft point-offset)
        (dolist (kind kinds)
          (let ((captured-kind kind))
            (push
             (cons kind
                   (niwa-view--interaction-register
                    (list :kind kind
                          :id kind
                          :count 1
                          :body (concat (alist-get kind bodies) "\n")
                          :keymap (make-sparse-keymap)
                          :entry (intern (format "%s-entry" kind))
                          :activate
                          (lambda (outcome)
                            (push (cons captured-kind outcome) callbacks)))))
             overlays)))
        (dotimes (_ 2)
          (niwa-view--interaction-render)
          (should (string= draft (niwa-view-composer-draft-text view-buf t)))
          (should (= point-offset (test-niwa-view-interaction--caret-offset view-buf))))
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
               (preview (string-search "preview body" text))
               (plan (string-search "plan body" text))
               (request (string-search "access body" text))
               (ask (string-search "question body" text))
               (permission (string-search "permission body" text))
               (queued (string-search "follow-up body" text)))
          ;; Newest farthest from the composer; pending input pinned to it.
          (should (< request preview ask permission plan queued)))
        (should (equal
                 "1 preview · 1 plan · 1 request · 1 question · 1 permission · 1 input pending"
                 (niwa-view--interaction-count-label)))
        (dolist (pair overlays)
          (let* ((kind (car pair))
                 (overlay (cdr pair))
                 (current (gethash kind niwa-view--interaction-overlays)))
            (should (eq overlay current))
            (should (eq (overlay-get current 'niwa-view-interaction-entry)
                        (intern (format "%s-entry" kind))))
            (funcall (overlay-get current 'niwa-view-interaction-activate)
                     'kept)))
        (should (equal kinds (mapcar #'car callbacks))))))))

(niwa-deftest niwa-view--interaction-rebuild ()
  ,test
  (test)
  :doc "registers the descriptor supplied by the transfer owner"
  (let ((session (niwa-session--create :name "control"))
        (descriptor '(:id control-transfer :kind request :body "transfer"))
        registered)
    (with-temp-buffer
      (setq-local niwa-view--agent-transcript-p nil)
      (cl-letf (((symbol-function 'niwa-view--session)
                 (lambda () session))
                ((symbol-function
                  'niwa-view-control-transfer-current-descriptor)
                 (lambda () descriptor))
                ((symbol-function 'niwa-view--interaction-register)
                 (lambda (value) (setq registered value)))
                ((symbol-function
                  'niwa-view--interaction-clear-for-rebuild)
                 #'ignore)
                ((symbol-function
                  'niwa-view--interaction-delete-stale-overlays)
                 #'ignore)
                ((symbol-function 'niwa-view--interaction-render)
                 #'ignore))
        (niwa-view--interaction-rebuild)))
    (should (equal descriptor registered)))

  :doc "an unchanged rebuild leaves zone text, point, and overlays untouched"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "rebuild-noop"))
          (descriptor '(:kind permission :id transfer-prompt :origin "/root"
                        :body "Grant control transfer?"))
          (draft "> quoted\nsecond line"))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft draft 4)
        (cl-letf (((symbol-function
                    'niwa-view-control-transfer-current-descriptor)
                   (lambda () descriptor)))
          (niwa-view--interaction-rebuild)
          (goto-char (point-min))
          (search-forward "Grant control")
          (let ((position (point))
                (tick (buffer-chars-modified-tick))
                (overlay (gethash 'transfer-prompt
                                  niwa-view--interaction-overlays)))
            (should (overlayp overlay))
            (niwa-view--interaction-rebuild)
            (niwa-view--interaction-rebuild)
            (should (= position (point)))
            (should (= tick (buffer-chars-modified-tick)))
            (should (eq overlay (gethash 'transfer-prompt
                                         niwa-view--interaction-overlays)))
            (should (equal draft (niwa-view-composer-draft-text view-buf t))))))))

  :doc "a queue rebuild does not toggle active progress through transfer UI"
  (let ((niwa-view-spinner-animate nil))
    (niwa-view-test--with-buffers
      (let* ((session (niwa-session--create
                       :name "rebuild-progress"
                       :permission-mode 'ask))
             (request (niwa-request--create
                       :session session :started-at (current-time)))
             (entry (list :kind 'generic
                          :tool-name "Read"
                          :specifier-value "/tmp/file.txt"
                          :include-always nil
                          :origin "/root"
                          :session session
                          :callback #'ignore)))
        (setf (niwa-session-permission-queue session) (list entry))
        (with-current-buffer data-buf
          (setq-local niwa--session session
                      niwa--current-request request))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (niwa-view--start-spinner "Working...")
          (cl-letf (((symbol-function
                      'niwa-view-control-transfer-current-descriptor)
                     (lambda ()
                       '(:kind control-transfer :id control-transfer
                         :body "Transfer status")))
                    ((symbol-function 'niwa-view--spinner-elapsed-label)
                     (lambda () nil)))
            (niwa-view--interaction-rebuild)
            (goto-char (point-min))
            (search-forward "Permission Request")
            (let ((position (point))
                  (tick (buffer-chars-modified-tick))
                  (pause-started
                   (niwa-request-active-work-pause-started-at request)))
              (should pause-started)
              (niwa-view--interaction-rebuild)
              (should (= position (point)))
              (should (= tick (buffer-chars-modified-tick)))
              (should (equal
                       pause-started
                       (niwa-request-active-work-pause-started-at
                        request)))))))))

  :doc "a producer error still reconciles the partially rebuilt zone"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "rebuild-error")))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--interaction-register
         '(:kind permission :id stale :body "stale permission"))
        (cl-letf (((symbol-function
                    'niwa-view-control-transfer-current-descriptor)
                   (lambda () (error "Producer failed"))))
          (should-error (niwa-view--interaction-rebuild)
                        :type 'error))
        (should-not (gethash 'stale niwa-view--interaction-descriptors))
        (should-not (gethash 'stale niwa-view--interaction-overlays))
        (should-not (string-search
                     "stale permission"
                     (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail))))))))

(defun test-niwa-view-interaction--run-deferred (timers)
  "Run idle-work callbacks captured in TIMERS, oldest first, until none remain.
TIMERS is a cons cell whose car holds captured `(FUNCTION . ARGS)' entries."
  (let ((idle-work '(niwa-pending-inputs--offer-idle-work-now
                     niwa-goal--scheduled-continuation
                     niwa-view--run-follow-up-drain)))
    (while (car timers)
      (let ((pending (reverse (car timers))))
        (setcar timers nil)
        (dolist (timer pending)
          (when (memq (car timer) idle-work)
            (apply (car timer) (cdr timer))))))))

(niwa-deftest niwa-view--interaction-offer-idle-work ()
  ,test
  (test)
  :doc "runs in a fresh Emacs that has not loaded the agent transcript view"
  (with-temp-buffer
    (should
     (zerop
      (call-process
       (expand-file-name invocation-name invocation-directory) nil t nil
       "--batch" "-Q" "-L"
       (file-name-directory (locate-library "niwa-view-interaction"))
       "--eval"
       (prin1-to-string
        '(progn
           (require 'niwa-view-interaction)
           (with-temp-buffer (niwa-view--interaction-offer-idle-work))
           (when (featurep 'niwa-view-agent)
             (error "Loaded the agent transcript view")))))))
    (should (equal "" (buffer-string))))

  :doc "a Goal held by a child agent's permission card continues once it is decided"
  (niwa-view-test--with-buffers
    (let* ((goal (niwa-goal--create
                  :id "held" :objective "Ship" :status 'active
                  :tokens-used 0 :time-used-seconds 0 :turns-run 0))
           (session (niwa-session--create :name "held-goal" :goal goal))
           (niwa-permission-reviewer 'user)
           (timers (list nil))
           outcome dispatched)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (switch-to-buffer view-buf)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) (car timers))
                   nil))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (_key _path thunk &rest _) (funcall thunk) t))
                ((symbol-function 'niwa--submit-generated-turn)
                 (lambda (&rest _) (setq dispatched t))))
        (with-current-buffer data-buf
          (niwa-permission-request
           (list :kind 'generic :tool-name "Read" :origin "/root/worker"
                 :data-buffer data-buf
                 :callback (lambda (value) (setq outcome value)))
           session))
        (should (niwa-session-permission-queue session))
        (should (eq 'interaction
                    (niwa-goal-continue-if-idle session data-buf)))
        (test-niwa-view-interaction--run-deferred timers)
        (should-not dispatched)
        (with-current-buffer view-buf
          (niwa-view-jump-to-interaction)
          (should (get-text-property (point) 'niwa-prompt-focus))
          (call-interactively (key-binding (kbd "RET"))))
        (should (eq 'allow-once outcome))
        (should-not (niwa-session-permission-queue session))
        (test-niwa-view-interaction--run-deferred timers)
        (should dispatched))))

  :doc "offers only once the last pending interaction closes"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "offer"))
          offers)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
                   (lambda (buffer) (push buffer offers))))
          (niwa-view--interaction-register
           '(:kind ask :id first :origin "/root" :body "first"))
          (niwa-view--interaction-register
           '(:kind ask :id second :origin "/root" :body "second"))
          (niwa-view--interaction-unregister 'first)
          (should-not offers)
          (niwa-view--interaction-unregister 'second)
          (should (equal (list data-buf) offers))))))

  :doc "a rebuild offers only when it drops an interaction"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "rebuild-offer"))
          (descriptor '(:kind permission :id transfer :origin "/root"
                        :body "Grant control?"))
          offers)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
                   (lambda (buffer) (push buffer offers)))
                  ((symbol-function
                    'niwa-view-control-transfer-current-descriptor)
                   (lambda () descriptor)))
          (niwa-view--interaction-rebuild)
          (niwa-view--interaction-rebuild)
          (should-not offers)
          (setq descriptor nil)
          (niwa-view--interaction-rebuild)
          (should (equal (list data-buf) offers))))))

  :doc "an agent transcript view never offers its parent's idle work"
  (niwa-view-test--with-buffers
    (let (offers)
      (with-current-buffer view-buf
        (setq-local niwa-view--agent-transcript-p t)
        (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
                   (lambda (buffer) (push buffer offers))))
          (niwa-view--interaction-offer-idle-work))
        (setq-local niwa-view--agent-transcript-p nil))
      (should-not offers))))

(provide 'test-niwa-view-interaction)
;;; test-niwa-view-interaction.el ends here

