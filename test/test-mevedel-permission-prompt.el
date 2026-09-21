;;; test-mevedel-permission-prompt.el --- Permission prompt tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests generic, Bash, Eval, and execution-authority prompt rendering.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution-target)
(require 'mevedel-interaction-prompt)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-permissions)
(require 'mevedel-side-conversation)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Controls

(mevedel-deftest mevedel-permission--prompt-approve-once
  ()
  ,test
  (test)
  :doc "settles the interaction-zone permission prompt from its real text"
  (with-temp-buffer
    (let* ((mevedel-view--interaction-descriptors
            (make-hash-table :test #'equal))
           (mevedel-view--interaction-overlays
            (make-hash-table :test #'equal))
           (received nil)
           (id '(:permission real-text)))
      (insert "interaction\n\n> ")
      (let ((ov (make-overlay (point-min) (1+ (point-min)) nil t)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-view-interaction-id id)
        (overlay-put ov 'priority 100)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (setq received outcome)))
        (push ov mevedel--prompt-overlays)
        (puthash id ov mevedel-view--interaction-overlays)
        (puthash id (list :id id) mevedel-view--interaction-descriptors)
        (goto-char (point-min))
        (let ((last-command-event ?a))
          (call-interactively #'mevedel-permission--prompt-approve-once))
        (should (eq received 'allow-once))
        (should-not (gethash id mevedel-view--interaction-overlays)))))

  :doc "falls back to normal typing when no permission prompt is active"
  (with-temp-buffer
    (let ((last-command-event ?a))
      (call-interactively #'mevedel-permission--prompt-approve-once))
    (should (equal (buffer-string) "a")))

  :doc "invalid directory scope leaves the approval card available for correction"
  (let ((root (make-temp-file "mevedel-prompt-scope-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert "permission")
          (let ((ov (make-overlay (point-min) (point-max)))
                received)
            (overlay-put ov 'mevedel-permission-prompt t)
            (overlay-put ov 'mevedel-view-interaction-entry
                         (list :kind 'sandbox
                               :session (mevedel-session--create
                                         :permission-mode 'ask :sandbox-mode 'required)
                               :resource-selection-cell
                               (list (list (list :path root :access 'write)))))
            (overlay-put ov 'mevedel--callback
                         (lambda (outcome) (setq received outcome)))
            (push ov mevedel--prompt-overlays)
            (goto-char (point-min))
            (should-error (mevedel-permission--prompt-approve-once)
                          :type 'user-error)
            (should-not received)
            (should-not (overlay-get ov 'mevedel-settled))
            (should (overlay-buffer ov))))
      (delete-directory root t)))

  :doc "shows a newly-active parent warning before one-shot mutation approval"
  (let ((side-buffer (generate-new-buffer " *mevedel-side-late-warning*")))
    (unwind-protect
        (with-temp-buffer
          (let* ((entry
                  (list :mutation-p t :data-buffer side-buffer
                        :session 'side-session))
                 received
                 rerendered)
            (insert "prompt")
            (let ((ov (make-overlay (point-min) (point-max))))
              (overlay-put ov 'mevedel-permission-prompt t)
              (overlay-put ov 'mevedel-view-interaction-entry entry)
              (overlay-put ov 'mevedel--callback
                           (lambda (outcome) (setq received outcome)))
              (push ov mevedel--prompt-overlays)
              (goto-char (point-min))
              (cl-letf
                  (((symbol-function
                     'mevedel-side-conversation-parent-active-p)
                    (lambda (&optional buffer)
                      (eq buffer side-buffer)))
                   ((symbol-function 'mevedel-permission-queue--render-head)
                    (lambda (_session)
                      (setq rerendered t)
                      (plist-put entry :parent-active-warning-shown-p t))))
                (mevedel-permission--prompt-approve-once)
                (should rerendered)
                (should-not received)
                (mevedel-permission--prompt-approve-once)
                (should (eq received 'allow-once))))))
      (kill-buffer side-buffer))))

(mevedel-deftest mevedel-permission--prompt-approve-session
  (:quiet t :doc "does not settle prompts that suppress session allow")
  (with-temp-buffer
    (let (received)
      (insert "prompt")
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-permission-suppress-allow-session t)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (push outcome received)))
        (push ov mevedel--prompt-overlays)
        (goto-char (point-min))
        (mevedel-permission--prompt-approve-session)
        (should-not received)
        (should (overlay-buffer ov))))))

(mevedel-deftest mevedel-permission--prompt-toggle-remember
  ()
  ,test
  (test)
  :doc "command and network toggles preserve their dependency"
  (with-temp-buffer
    (let* ((cell (list '(:operation t
                        :file-system ((:path "/cache" :access write))
                        :resource-grants ((:path "/input" :access read)))))
           (entry
            `(:session session
              :remember-authority-cell ,cell
              :requested-additional-permissions (:network t))))
      (insert "prompt")
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-view-interaction-entry entry)
        (goto-char (point-min))
        (cl-letf (((symbol-function
                    'mevedel-permission-queue--render-head)
                   #'ignore))
          (let ((last-command-event ?n))
            (mevedel-permission--prompt-toggle-remember))
          (should (plist-get (car cell) :operation))
          (should (plist-get (car cell) :network))
          (let ((last-command-event ?c))
            (mevedel-permission--prompt-toggle-remember))
          (should-not (plist-get (car cell) :operation))
          (should-not (plist-get (car cell) :network))
          (should-not (plist-get (car cell) :file-system))
          (should (plist-get (car cell) :resource-grants))))))

  :doc "path scopes are explicit and switching scopes never duplicates a grant"
  (with-temp-buffer
    (let* ((grant '(:path "/cache" :access write :recursive t))
           (cell (list nil))
           (entry `(:reusable-operation-p t :remember-authority-cell ,cell
                    :requested-additional-permissions (:file-system (,grant))))
           (ov (progn (insert "prompt") (make-overlay (point-min) (point-max)))))
      (overlay-put ov 'mevedel-permission-prompt t)
      (overlay-put ov 'mevedel-view-interaction-entry entry)
      (goto-char (point-min))
      (dolist (choice '(("With this command" . :file-system)
                        ("Independent path access" . :resource-grants)
                        ("Do not remember" . nil)))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt choices &rest _)
                     (if (equal prompt "Remember capability: ")
                         (caar choices)
                       (should (assoc (car choice) choices))
                       (car choice)))))
          (let ((last-command-event ?p))
            (mevedel-permission--prompt-toggle-remember)))
        (dolist (key '(:file-system :resource-grants))
          (should (equal (and (eq key (cdr choice)) (list grant))
                         (plist-get (car cell) key))))
        (should (plist-get (car cell) :operation)))
      (setf (plist-get entry :reusable-operation-p) nil)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (when (equal prompt "Remember path access: ")
                     (should-not (assoc "With this command" choices)))
                   (caar choices))))
        (let ((last-command-event ?p))
          (mevedel-permission--prompt-toggle-remember)))
      (should (equal (list grant) (plist-get (car cell) :resource-grants)))))

  :doc "path selection chooses command scope and enables command remembering"
  (with-temp-buffer
    (let* ((root "/ssh:display:/srv/project/")
           (target (mevedel-execution-target-create root))
           (session (mevedel-session--create
                     :execution-target target :working-directory root))
           (grant '(:path "/ssh:display:/external" :access write))
           (cell (list '(:operation t)))
           (entry
            `(:session ,session :reusable-operation-p t
              :remember-authority-cell ,cell
              :requested-additional-permissions
              (:file-system (,grant)))))
      (insert "prompt")
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-view-interaction-entry entry)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt choices &rest _)
                     (let ((choice (if (equal prompt "Remember capability: ")
                                       "Write /external (exact)"
                                     "With this command")))
                       (should (assoc choice choices))
                       choice)))
                  ((symbol-function
                    'mevedel-permission-queue--render-head)
                   #'ignore))
          (let ((last-command-event ?p))
            (mevedel-permission--prompt-toggle-remember)))
        (should (equal (list grant)
                       (plist-get (car cell) :file-system))))))

  :doc "recursive grants get a distinct completion label"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "test"))
           (exact '(:path "/srv/tree" :access read))
           (recursive '(:path "/srv/tree" :access read :recursive t))
           (cell (list '(:operation t)))
           (entry
            `(:session ,session :reusable-operation-p t
              :remember-authority-cell ,cell
              :requested-additional-permissions
              (:file-system (,exact ,recursive)))))
      (insert "prompt")
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-view-interaction-entry entry)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt choices &rest _)
                     (if (equal prompt "Remember capability: ")
                         (progn
                           (should (equal '("Read /srv/tree (exact)"
                                            "Read /srv/tree (recursive)")
                                          (mapcar #'car choices)))
                           "Read /srv/tree (recursive)")
                       "With this command")))
                  ((symbol-function
                    'mevedel-permission-queue--render-head)
                   #'ignore))
          (let ((last-command-event ?p))
            (mevedel-permission--prompt-toggle-remember)))
        (should (equal (list recursive)
                       (plist-get (car cell) :file-system)))))))


;;
;;; Rendering

(mevedel-deftest mevedel-permission--resource-label ()
  ,test
  (test)
  :doc "formats exact and recursive grant labels"
  (should (equal "Read /srv/tree (exact)"
                 (mevedel-permission--resource-label
                  nil '(:path "/srv/tree" :access read))))
  (should (equal "Write /srv/tree (recursive)"
                 (mevedel-permission--resource-label
                  nil '(:path "/srv/tree" :access write :recursive t)))))

(mevedel-deftest mevedel-permission--format-authority-capabilities
  ()
  ,test
  (test)
  :doc "distinguishes pending, granted, and missing authority"
  (let* ((root "/ssh:display:/srv/project/")
         (target (mevedel-execution-target-create root))
         (session (mevedel-session--create
                   :execution-target target :working-directory root))
         (text
          (mevedel-permission--format-authority-capabilities
           `(:session ,session
             :show-operation-authority t
             :operation-pending-p t
             :requested-additional-permissions
             (:network t
              :file-system
              ((:path "/ssh:display:/input" :access read)
               (:path "/ssh:display:/output" :access write)))
             :missing-additional-permissions
             (:network t
              :file-system
              ((:path "/ssh:display:/output" :access write)))))))
    (should (string-match-p "\\[ \\] Command" text))
    (should (string-match-p "\\[ \\] Network" text))
    (should (string-match-p "\\[x\\] Read /input" text))
    (should (string-match-p "\\[ \\] Write /output" text))
    (should-not (string-match-p "/ssh:" text))))

(mevedel-deftest mevedel-permission--format-remember-authority
  ()
  ,test
  (test)
  :doc "shows independent command, network, and exact-path selections"
  (let* ((root "/ssh:display:/srv/project/")
         (target (mevedel-execution-target-create root))
         (session (mevedel-session--create
                   :execution-target target :working-directory root))
         (write '(:path "/ssh:display:/output" :access write))
         (text
          (mevedel-permission--format-remember-authority
           `(:session ,session
             :reusable-operation-p t
             :remember-authority-cell
             ((:operation t :file-system (,write)))
             :requested-additional-permissions
             (:network t :file-system (,write))))))
    (should (string-match-p "\\[x\\] Command" text))
    (should (string-match-p "\\[ \\] Network with command" text))
    (should (string-match-p "Write /output (exact) -- With this command" text))
    (should (string-match-p "selected authority" text))
    (should (string-match-p "p selects a path" text))
    (should-not (string-match-p "/ssh:" text)))

  :doc "labels recursive grant selections"
  (let* ((session (mevedel-session--create :name "test"))
         (grant '(:path "/srv/tree" :access read :recursive t))
         (text
          (mevedel-permission--format-remember-authority
           `(:session ,session
             :remember-authority-cell ((:operation t))
             :requested-additional-permissions
             (:file-system (,grant))))))
    (should (string-match-p
             (regexp-quote "Read /srv/tree (recursive) -- Do not remember") text))))

(mevedel-deftest mevedel-permission--prompt-body
  ()
  ,test
  (test)
  :doc "includes session allow by default"
  (cl-letf (((symbol-function 'mevedel--prompt-block-face)
             (lambda () 'ask)))
    (should (string-match-p
             "remember selected authority for session"
             (mevedel-permission--prompt-body "Body\n" nil))))

  :doc "suppresses session allow without suppressing session deny"
  (cl-letf (((symbol-function 'mevedel--prompt-block-face)
             (lambda () 'ask)))
    (let ((body (mevedel-permission--prompt-body "Body\n" nil t)))
      (should-not (string-match-p "remember selected authority for session" body))
      (should (string-match-p "deny-session" body)))))

(mevedel-deftest mevedel-permission--format-cause
  ()
  ,test
  (test)
  :doc "admission mode and cause are distinct from Eval execution mode"
  (dolist (case '((workspace-boundary "outside")
                  (protected-path "protected")
                  (rule "rule")
                  (pre-tool-hook "PreToolUse")
                  (permission-request-hook "PermissionRequest")
                  (one-shot-mutation "one-time")
                  (mode "operation")
                  (sandbox-network "network")
                  (sandbox-filesystem "filesystem")
                  (sandbox-full-escalation "without confinement")))
    (let ((body (mevedel-permission--format-cause
                 (list :permission-mode-effective 'full-auto
                       :mode "batch" :permission-via (car case)))))
      (should (string-match-p "full-auto" body))
      (should (string-match-p (cadr case) body))
      (should-not (string-match-p "batch" body))))
  :doc "absent policy provenance is not guessed from the tool kind"
  (should-not (mevedel-permission--format-cause '(:kind bash))))

(mevedel-deftest mevedel-permission--prompt-async-with-content
  ()
  ,test
  (test)
  :doc "suppressed session allow reaches the body and local keymap"
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-body
          captured-keymap)
      (cl-letf (((symbol-function 'mevedel--prompt-block-face)
                 (lambda () 'ask))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda (&optional _buffer) target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (plist)
                   (setq captured-body (plist-get plist :body))
                   (setq captured-keymap (plist-get plist :keymap))
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-with-content
         "Body\n" t #'ignore nil nil t))
      (should-not (string-match-p "remember for session" captured-body))
      (should (eq (lookup-key captured-keymap (kbd "RET"))
                  #'mevedel-permission--prompt-approve-once))
      (should-not (lookup-key captured-keymap "s"))
      (should (lookup-key captured-keymap "A"))))

  :doc "rememberable authority binds only the capabilities it presents"
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-keymap)
      (cl-letf (((symbol-function 'mevedel--prompt-block-face)
                 (lambda () 'ask))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda (&optional _buffer) target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (plist)
                   (setq captured-keymap (plist-get plist :keymap))
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-with-content
         "Body\n" t #'ignore nil
         '(:reusable-operation-p t
           :remember-authority-cell ((:operation t))
           :requested-additional-permissions
           (:network t
            :file-system ((:path "/external" :access read))))))
      (dolist (key '("c" "n" "p" "s" "A"))
        (should (lookup-key captured-keymap key)))))

  :doc "warns at render time when a mutation can race an active parent"
  (let ((side-buffer (generate-new-buffer " *mevedel-side-warning*"))
        (parent-active nil))
    (unwind-protect
        (with-temp-buffer
          (let ((target (current-buffer)) captured-body entry)
            (cl-letf (((symbol-function
                        'mevedel-side-conversation-parent-active-p)
                       (lambda (&optional buffer)
                         (and (eq buffer side-buffer) parent-active))))
              (setq entry
                    (mevedel-permission--one-shot-prompt-entry
                     '(:mutation-p t) side-buffer))
              (setq parent-active t)
              (cl-letf
                  (((symbol-function 'mevedel--prompt-block-face)
                    (lambda () 'ask))
                   ((symbol-function 'mevedel--prompt--data-buffer)
                    (lambda (&optional _buffer) target))
                   ((symbol-function
                     'mevedel-view--interaction-target-buffer)
                    (lambda (_data-buffer) target))
                   ((symbol-function 'mevedel-view--interaction-register)
                    (lambda (plist)
                      (setq captured-body
                            (substring-no-properties
                             (plist-get plist :body)))
                      (make-overlay (point-min) (point-min))))
                   ((symbol-function 'mevedel--prompt--register-canceller)
                    #'ignore))
                (mevedel-permission--prompt-async-with-content
                 "Body\n" nil #'ignore nil entry t t))
              (should
               (string-match-p "parent request is still active"
                               captured-body))
              (should (plist-get entry
                                 :parent-active-warning-shown-p)))))
      (kill-buffer side-buffer)))
  :doc "does not describe read-only approval as a workspace change"
  (with-temp-buffer
    (let ((target (current-buffer)) captured-body)
      (cl-letf (((symbol-function
                  'mevedel-side-conversation-parent-active-p)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'mevedel--prompt-block-face)
                 (lambda () 'ask))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda (&optional _buffer) target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (plist)
                   (setq captured-body
                         (substring-no-properties (plist-get plist :body)))
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-with-content
         "Body\n" nil #'ignore nil
         '(:once-only t :mutation-p nil :data-buffer side)
         t t))
      (should-not
       (string-match-p "parent request is still active" captured-body))))

  :doc "browser Bash and Eval cards disclose selected one-shot scope without eliding the operation"
  (dolist (kind '(bash eval))
    (let* ((data (generate-new-buffer " *test-permission-source*"))
           (view (generate-new-buffer " *test-permission-view*"))
           (session (mevedel-session--create :name "test"))
           (draft "> keep this draft\nsecond line")
           (operation (mapconcat #'identity (make-list 30 "long operation text") "\n"))
           (selection (list '((:path "/srv/input" :access read))))
           (entry (list :kind kind :origin "/root" :once-only t
                        :permission-mode-effective 'full-auto
                        :permission-via 'one-shot-mutation
                        :command (and (eq kind 'bash) operation)
                        :expression (and (eq kind 'eval) operation)
                        :mode "batch" :callback #'ignore
                        :resource-selection-cell selection
                        :requested-additional-permissions
                        '(:file-system ((:path "/srv/input" :access read))))))
      (unwind-protect
          (progn
            (with-current-buffer data
              (org-mode)
              (setq-local mevedel--session session))
            (mevedel-view--setup view data)
            (with-current-buffer view
              (goto-char (mevedel-view--input-start))
              (insert draft))
            (with-current-buffer data
              (mevedel-permission--enqueue entry session))
            ;; A scope change redraws the same pending one-shot card.
            (dolist (scope '((:path "/srv/input" :access read)
                             (:path "/srv" :access write :recursive t)))
              (setcar selection (list scope))
              (with-current-buffer data
                (mevedel-permission-queue--render-head session))
              (with-current-buffer view
                (let* ((queued (car (mevedel-session-permission-queue session)))
                       (ov (gethash (mevedel-queue--entry-metadata-get
                                     queued :interaction-id)
                                    mevedel-view--interaction-overlays))
                       (remote (plist-get (overlay-get ov 'mevedel--remote) :body)))
                  (dolist (text (list "Permission mode at admission: full-auto"
                                     "this mutation requires one-time approval"
                                     "Selected authority:"
                                     (mevedel-permission--resource-label queued scope)))
                    (should (string-match-p (regexp-quote text) remote))
                    (should (string-match-p (regexp-quote text) (buffer-string))))
                  (should (string-match-p (regexp-quote operation) remote))
                  (should-not (string-match-p (regexp-quote operation) (buffer-string)))
                  (should (equal draft (mevedel-view--input-text)))))))
        (mevedel-permission-queue-abort-all session)
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p data) (kill-buffer data)))))

  :doc "does not bind permission actions globally in view mode"
  (dolist (key (list (kbd "RET") (kbd "TAB")
                     "a" "c" "n" "p" "s" "A" "d" "D" "f"))
    (should-not (lookup-key mevedel-view-mode-map key))))

(mevedel-deftest mevedel-permission-prompt-render ()
  ,test
  (test)
  :doc "rejects unknown kinds before creating a permission interaction"
  (let (rendered)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (&rest _) (setq rendered t))))
      (should-error
       (mevedel-permission-prompt-render '(:kind unknown) nil #'ignore 1)
       :type 'error))
    (should-not rendered)))

(mevedel-deftest mevedel-permission--prompt-async-eval
  ()
  ,test
  (test)
  :doc "accepts RET for allow-once"
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-body
          captured-keymap)
      (cl-letf (((symbol-function 'mevedel--prompt-block-face)
                 (lambda () 'ask))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda (&optional _buffer) target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (plist)
                   (setq captured-body (plist-get plist :body))
                   (setq captured-keymap (plist-get plist :keymap))
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-eval "(+ 1 2)" nil #'ignore nil nil))
      (should (string-match-p "RET" captured-body))
      (should (eq (lookup-key captured-keymap (kbd "RET"))
                  #'mevedel-permission--prompt-approve-once))
      (should-not (string-match-p "allow-session" captured-body))
      (should-not (string-match-p "deny-session" captured-body))
      (should-not (lookup-key captured-keymap "s"))
      (should-not (lookup-key captured-keymap "A"))
      (should-not (lookup-key captured-keymap "D"))))

  :doc "renders requested live mode and preserve_ui value"
  (let (content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (body _always _callback &rest _)
                 (setq content body))))
      (mevedel-permission--prompt-async-eval
       "(delete-other-windows)" nil #'ignore nil
       '(:mode "live" :preserve-ui nil)))
    (should
     (string-match-p
      "Mode: live (inherently unconfined; preserve_ui: false)"
      content)))
  :doc "renders requested batch mode"
  (let (content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (body _always _callback &rest _)
                 (setq content body))))
      (mevedel-permission--prompt-async-eval
       "(+ 1 2)" nil #'ignore nil '(:mode "batch" :preserve-ui t)))
    (should (string-match-p "Mode: batch" content))
    (should-not (string-match-p "preserve_ui" content))))

(mevedel-deftest mevedel-permission--prompt-async-attributed
  ()
  ,test
  (test)
  :doc "propagates one-shot prompts to the shared prompt path"
  (let (captured)
    (cl-letf (((symbol-function
                'mevedel-permission--prompt-async-with-content)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission--prompt-async-attributed
       "Edit" nil t "main" #'ignore nil '(:once-only t)))
    (should (nth 5 captured))
    (should (nth 6 captured)))

  :doc "renders a remote resource path in target-native form"
  (let* ((root "/ssh:display:/srv/project/")
         (target (mevedel-execution-target-create root))
         (session (mevedel-session--create
                   :execution-target target :working-directory root))
         captured)
    (cl-letf (((symbol-function
                'mevedel-permission--prompt-async-with-content)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission--prompt-async-attributed
       "Read" "/ssh:display:/srv/secret" t "main" #'ignore nil
       `(:session ,session)))
    (should (string-match-p "Path: /srv/secret" (car captured)))
    (should-not (string-match-p "/ssh:" (car captured)))))

(mevedel-deftest mevedel-permission--prompt-async-sandbox
  (:doc "renders one combined invocation authority request")
  ,test
  (test)
  (let (captured)
    (cl-letf (((symbol-function
                'mevedel-permission--prompt-async-with-content)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission--prompt-async-sandbox
       "Bash" "curl https://example.test" "Download the page?"
       "main" #'ignore 2
       '(:kind sandbox
         :reusable-operation-p t
         :remember-authority-cell ((:operation t))
         :show-operation-authority t
         :operation-pending-p nil
         :requested-additional-permissions
         (:network t
          :file-system
          ((:path "/external/input" :access read)
           (:path "/external/output" :access write)))
         :missing-additional-permissions
         (:file-system ((:path "/external/output" :access write)))
         :granted-additional-permissions
         (:network t
          :file-system ((:path "/external/input" :access read))))))
    (let ((content (nth 0 captured)))
      (should (string-match-p "Invocation Authority Request" content))
      (should (string-match-p "Download the page?" content))
      (should (string-match-p "Authority for this execution" content))
      (should (string-match-p
               "\\[x\\] already granted.*\\[ \\] granted by this approval"
               content))
      (should (string-match-p "selected authority" content))
      (should (string-match-p "\\[x\\] Command" content))
      (should (string-match-p "\\[x\\] Network" content))
      (should (string-match-p
               "\\[x\\] Read /external/input" content))
      (should (string-match-p
               "\\[ \\] Write /external/output" content))
      (should (string-match-p
               "\\[x\\] Command  (c toggles)" content)))
    (should-not (nth 1 captured))
    (should (= 2 (nth 3 captured)))
    (should (eq 'sandbox (plist-get (nth 4 captured) :kind)))
    (should-not (nth 5 captured))
    (should-not (nth 6 captured)))

  :doc "renders exact filesystem access with rule-creating choices"
  (let (captured)
    (cl-letf (((symbol-function
                'mevedel-permission--prompt-async-with-content)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission--prompt-async-sandbox
       "Bash" "cat /tmp/secret" "Read the requested file?"
       "main" #'ignore 1
       '(:kind sandbox
         :reusable-operation-p t
         :remember-authority-cell ((:operation t))
         :show-operation-authority t
         :operation-pending-p nil
         :requested-additional-permissions
         (:file-system ((:path "/tmp/secret" :access read)))
         :missing-additional-permissions
         (:file-system ((:path "/tmp/secret" :access read)))
         :include-always t)))
    (let ((content (nth 0 captured)))
      (should (string-match-p "Invocation Authority Request" content))
      (should (string-match-p "\\[ \\] Read /tmp/secret" content))
      (should (string-match-p "every unchecked capability" content)))
    (should (eq t (nth 1 captured)))
    (should-not (nth 5 captured))
    (should-not (nth 6 captured)))

  :doc "renders full escalation with an explicit bypass warning"
  (let (captured)
    (cl-letf (((symbol-function
                'mevedel-permission--prompt-async-with-content)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission--prompt-async-sandbox
       "Eval" "(delete-file \"important\")" "Run outside confinement?"
       "main" #'ignore 1
       '(:kind sandbox
         :sandbox-permissions require-escalated
         :include-always t)))
    (let ((content (nth 0 captured)))
      (should (string-match-p "Full Execution Escalation Request" content))
      (should (string-match-p "runs directly as your user" content))
      (should (string-match-p
               "Filesystem, network, and process confinement.*disabled"
               content)))
    (should (eq t (nth 1 captured)))
    (should-not (nth 5 captured))
    (should-not (nth 6 captured)))

  :doc "suppresses reusable allow choices for unsafe full escalation"
  (let (captured)
    (cl-letf (((symbol-function
                'mevedel-permission--prompt-async-with-content)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission--prompt-async-sandbox
       "Bash" "rm -rf /" "Run outside confinement?"
       "main" #'ignore 1
       '(:kind sandbox
         :sandbox-permissions require-escalated
         :include-always nil)))
    (should (string-match-p "Reusable allow is disabled" (nth 0 captured)))
    (should-not (nth 1 captured))
    (should (eq t (nth 5 captured)))
    (should-not (nth 6 captured)))

  :doc "propagates one-shot policy through full escalation prompts"
  (let (captured)
    (cl-letf (((symbol-function
                'mevedel-permission--prompt-async-with-content)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission--prompt-async-sandbox
       "Bash" "make test" "Run outside confinement?"
       "main" #'ignore 1
       '(:kind sandbox
         :sandbox-permissions require-escalated
         :include-always nil
         :once-only t)))
    (should (nth 5 captured))
    (should (nth 6 captured))))



(mevedel-deftest mevedel-permission--prompt-async-bash
  ()
  ,test
  (test)
  :doc "literal dangerous prompts offer exact reusable authority"
  (let (captured-include captured-suppress captured-content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (content include-always _cont &optional _count _entry
                                suppress-allow-session _once-only)
                 (setq captured-content content)
                 (setq captured-include include-always)
                 (setq captured-suppress suppress-allow-session))))
      (mevedel-permission--prompt-async-bash
       "sudo pwd" 'dangerous t nil #'ignore nil
       (list :reusable-operation-p t
             :allow-patterns '("sudo pwd"))))
    (should captured-include)
    (should-not captured-suppress)
    (should-not (string-match-p
                 "Session/permanent allow is disabled" captured-content))
    (should (string-match-p
             "Session/always allow will add: `sudo pwd'" captured-content)))

  :doc "complex prompts suppress session and persistent allow"
  (let (captured-include captured-suppress captured-content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (content include-always _cont &optional _count _entry
                                suppress-allow-session _once-only)
                 (setq captured-content content)
                 (setq captured-include include-always)
                 (setq captured-suppress suppress-allow-session))))
      (mevedel-permission--prompt-async-bash
       "FOO=bar make test" 'complex t nil #'ignore nil
       (list :reusable-operation-p nil
             :unparseable t :allow-patterns '("FOO=bar make test"))))
    (should-not captured-include)
    (should captured-suppress)
    (should (string-match-p
             "disabled for complex Bash commands" captured-content))
    (should-not (string-match-p
                 "Session/always allow will add" captured-content)))

  :doc "shows counted detected command summaries"
  (let (captured-content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (content _include-always _cont &optional _count _entry
                                _suppress-allow-session _once-only)
                 (setq captured-content (substring-no-properties content)))))
      (mevedel-permission--prompt-async-bash
       "git add -- a && git add -- b" 'unknown t nil #'ignore nil
       (list :commands '("git" "git")
             :commands-summary "git (2)")))
    (should (string-match-p "Detected commands: git (2)" captured-content))
    (should-not (string-match-p "git, git" captured-content)))

  :doc "one-shot prompts expose no reusable choices"
  (let (captured-include captured-suppress captured-once-only captured-content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (content include-always _cont &optional _count _entry
                                suppress-allow-session once-only)
                 (setq captured-content content)
                 (setq captured-include include-always)
                 (setq captured-suppress suppress-allow-session)
                 (setq captured-once-only once-only))))
      (mevedel-permission--prompt-async-bash
       "make test" 'unknown t nil #'ignore nil
       (list :once-only t
             :reusable-operation-p t
             :allow-patterns '("make test"))))
    (should-not captured-include)
    (should captured-suppress)
    (should captured-once-only)
    (should-not (string-match-p
                 "Session/always allow will add" captured-content))))


;;
;;; Elision

(mevedel-deftest mevedel-permission--elide
  ()
  ,test
  (test)
  :doc "returns short text whole"
  (let ((entry (list :kind 'bash)))
    (should (equal "ls -A" (mevedel-permission--elide "ls -A" entry nil 400))))

  :doc "returns long text whole without an entry to hold toggle state"
  (should (equal (make-string 500 ?x)
                 (mevedel-permission--elide (make-string 500 ?x) nil nil 400)))

  :doc "elides past the character limit and names the hidden remainder"
  (let* ((entry (list :kind 'bash))
         (shown (substring-no-properties
                 (mevedel-permission--elide (make-string 500 ?x)
                                            entry nil 400))))
    (should (string-prefix-p (make-string 400 ?x) shown))
    (should (string-match-p "100 more characters, TAB to expand" shown))
    (should-not (string-match-p (make-string 401 ?x) shown)))

  :doc "elides past the line limit"
  (let* ((entry (list :kind 'eval))
         (text (mapconcat #'number-to-string (number-sequence 1 30) "\n"))
         (shown (substring-no-properties
                 (mevedel-permission--elide text entry 20))))
    (should (string-prefix-p "1\n2\n" shown))
    (should-not (string-match-p "^21$" shown))
    (should (string-match-p "TAB to expand" shown)))

  :doc "shows everything once the entry is expanded"
  (let* ((entry (list :kind 'bash))
         (text (make-string 500 ?x)))
    (mevedel-queue--entry-metadata-put entry :expanded t)
    (let ((shown (substring-no-properties
                  (mevedel-permission--elide text entry nil 400))))
      (should (string-prefix-p text shown))
      (should (string-match-p "TAB to collapse" shown))))

  :doc "keeps text properties across elision"
  (let ((entry (list :kind 'bash)))
    (should (eq 'font-lock-string-face
                (get-text-property
                 0 'font-lock-face
                 (mevedel-permission--elide
                  (propertize (make-string 500 ?x)
                              'font-lock-face 'font-lock-string-face)
                  entry nil 400))))))

(mevedel-deftest mevedel--prompt-block-face ()
  ,test
  (test)
  :doc "uses the native code face of the prompt's major mode"
  (with-temp-buffer
    (org-mode)
    (should (eq 'org-block (mevedel--prompt-block-face)))
    (setq major-mode 'markdown-mode)
    (should (eq 'markdown-code-face (mevedel--prompt-block-face)))
    (fundamental-mode)
    (should (plist-get (mevedel--prompt-block-face) :extend))))

(provide 'test-mevedel-permission-prompt)

;;; test-mevedel-permission-prompt.el ends here
