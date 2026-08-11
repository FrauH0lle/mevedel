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
  (:doc "does not settle prompts that suppress session allow")
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
    (let* ((cell (list '(:operation t)))
           (entry
            `(:session session
              :remember-authority-cell ,cell
              :missing-additional-permissions (:network t))))
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
          (should-not (plist-get (car cell) :network))))))

  :doc "path selection toggles one exact grant"
  (with-temp-buffer
    (let* ((root "/ssh:display:/srv/project/")
           (target (mevedel-execution-target-create root))
           (session (mevedel-session--create
                     :execution-target target :working-directory root))
           (grant '(:path "/ssh:display:/external" :access write))
           (cell (list '(:operation t)))
           (entry
            `(:session ,session
              :remember-authority-cell ,cell
              :missing-additional-permissions
              (:file-system (,grant)))))
      (insert "prompt")
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-view-interaction-entry entry)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (should (equal "Write /external" (caar choices)))
                     (caar choices)))
                  ((symbol-function
                    'mevedel-permission-queue--render-head)
                   #'ignore))
          (let ((last-command-event ?p))
            (mevedel-permission--prompt-toggle-remember)))
        (should (equal (list grant)
                       (plist-get (car cell) :file-system)))))))


;;
;;; Rendering

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
             :missing-additional-permissions
             (:network t :file-system (,write))))))
    (should (string-match-p "\\[x\\] Command" text))
    (should (string-match-p "\\[ \\] Network with command" text))
    (should (string-match-p "\\[x\\] Write /output" text))
    (should (string-match-p "complete selected profile" text))
    (should (string-match-p "p selects an exact path" text))
    (should-not (string-match-p "/ssh:" text))))

(mevedel-deftest mevedel-permission--prompt-body
  ()
  ,test
  (test)
  :doc "includes session allow by default"
  (cl-letf (((symbol-function 'gptel-agent--block-bg)
             (lambda () 'ask)))
    (should (string-match-p
             "remember selected profile for session"
             (mevedel-permission--prompt-body "Body\n" nil))))

  :doc "suppresses session allow without suppressing session deny"
  (cl-letf (((symbol-function 'gptel-agent--block-bg)
             (lambda () 'ask)))
    (let ((body (mevedel-permission--prompt-body "Body\n" nil t)))
      (should-not (string-match-p "remember selected profile for session" body))
      (should (string-match-p "deny-session" body)))))

(mevedel-deftest mevedel-permission--prompt-async-with-content
  ()
  ,test
  (test)
  :doc "suppressed session allow reaches the body and local keymap"
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-body
          captured-keymap)
      (cl-letf (((symbol-function 'gptel-agent--block-bg)
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
      (cl-letf (((symbol-function 'gptel-agent--block-bg)
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
           :missing-additional-permissions
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
                  (((symbol-function 'gptel-agent--block-bg)
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
                ((symbol-function 'gptel-agent--block-bg)
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

  :doc "does not bind permission actions globally in view mode"
  (dolist (key (list (kbd "RET") (kbd "TAB")
                     "a" "c" "n" "p" "s" "A" "d" "D" "f"))
    (should-not (lookup-key mevedel-view-mode-map key))))

(mevedel-deftest mevedel-permission--prompt-async-eval
  (:doc "accepts RET for allow-once")
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-body
          captured-keymap)
      (cl-letf (((symbol-function 'gptel-agent--block-bg)
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
        (mevedel-permission--prompt-async-eval "Eval\n" #'ignore))
      (should (string-match-p "RET" captured-body))
      (should (eq (lookup-key captured-keymap (kbd "RET"))
                  #'mevedel-permission--prompt-approve-once))
      (should-not (string-match-p "allow-session" captured-body))
      (should-not (string-match-p "deny-session" captured-body))
      (should-not (lookup-key captured-keymap "s"))
      (should-not (lookup-key captured-keymap "A"))
      (should-not (lookup-key captured-keymap "D")))))

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
      (should (string-match-p "complete selected profile" content))
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

(mevedel-deftest mevedel-permission--format-bash-guardian
  ()
  ,test
  (test)
  :doc "formats guardian risk guidance"
  (let ((text
         (substring-no-properties
          (mevedel-permission--format-bash-guardian
           '(:risk high
             :recommendation deny
             :reason "Downloads and executes remote code.")))))
    (should (string-match-p "Risk: High" text))
    (should (string-match-p "Recommendation: Deny" text))
    (should (string-match-p "Downloads and executes remote code" text)))

  :doc "formats pending guidance"
  (let ((text (substring-no-properties
               (mevedel-permission--format-bash-guardian nil 'pending))))
    (should (string-match-p "Status: Analyzing command risk" text))
    (should-not (string-match-p "Risk:" text)))

  :doc "formats unavailable guidance"
  (let ((text (substring-no-properties
               (mevedel-permission--format-bash-guardian
                nil 'unavailable))))
    (should (string-match-p "Unavailable" text))
    (should-not (string-match-p "Risk:" text))))

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

(provide 'test-mevedel-permission-prompt)

;;; test-mevedel-permission-prompt.el ends here
