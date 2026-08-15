;;; test-mevedel-reminders.el --- Tests for mevedel-reminders.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'gptel-gemini)
(require 'gptel-request)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-permissions)
(require 'mevedel-session-persistence)
(require 'mevedel-file-state)
(require 'mevedel-tool-fs)
(require 'mevedel-compact)
(require 'mevedel-reminders)
(require 'mevedel-system)
(require 'mevedel-agents)
(require 'mevedel-tool-ui)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar imenu--index-alist)
(defvar mevedel--agent-invocation)

;; Declared special here so `let'-binding stays dynamic even when no
;; loaded module has defined the variable with a value yet.
(defvar mevedel--agent-invocation)


;;
;;; Struct creation

(mevedel-deftest mevedel-reminder-create
  ()
  ,test
  (test)
  :doc "`mevedel-reminder-create' populates all slots from keyword args"
  (let ((r (mevedel-reminder-create
            :type 'demo
            :trigger (lambda (_) t)
            :content (lambda (_) "hello")
            :interval 3
            :recipe '(verifier-read-only))))
    (should (eq 'demo (mevedel-reminder-type r)))
    (should (functionp (mevedel-reminder-trigger r)))
    (should (functionp (mevedel-reminder-content r)))
    (should (equal 3 (mevedel-reminder-interval r)))
    (should (null (mevedel-reminder-last-fired r)))
    (should (equal '(verifier-read-only)
                   (mevedel-reminder-recipe r))))

  :doc "`mevedel-reminder-create' accepts nil interval"
  (let ((r (mevedel-reminder-create
            :type 'demo
            :trigger (lambda (_) t)
            :content (lambda (_) "x"))))
    (should (null (mevedel-reminder-interval r))))

  :doc "`mevedel-reminder-create' rejects non-symbol type"
  (should-error (mevedel-reminder-create
                 :type "not-a-symbol"
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")))

  :doc "`mevedel-reminder-create' rejects non-function trigger"
  (should-error (mevedel-reminder-create
                 :type 'demo
                 :trigger "not-a-fn"
                 :content (lambda (_) "x")))

  :doc "`mevedel-reminder-create' rejects non-function content"
  (should-error (mevedel-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content "not-a-fn"))

  :doc "`mevedel-reminder-create' rejects non-integer, non-symbol interval"
  (should-error (mevedel-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")
                 :interval "5"))

  :doc "`mevedel-reminder-create' accepts `one-shot' interval"
  (let ((r (mevedel-reminder-create
            :type 'demo
            :trigger (lambda (_) t)
            :content (lambda (_) "x")
            :interval 'one-shot)))
    (should (eq 'one-shot (mevedel-reminder-interval r))))

  :doc "`mevedel-reminder-create' rejects unknown interval symbol"
  (should-error (mevedel-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")
                 :interval 'bogus))

  :doc "`mevedel-reminder-create' rejects untrusted durable recipes"
  (should-error (mevedel-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")
                 :recipe '(erase-buffer))))


;;
;;; Session reminder helpers

(mevedel-deftest mevedel-session-add-reminder
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry)
   :doc "`mevedel-session-add-reminder' appends reminders in registration order")
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r1 (mevedel-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "1")))
         (r2 (mevedel-reminder-create :type 'b
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "2"))))
    (mevedel-session-add-reminder session r1)
    (mevedel-session-add-reminder session r2)
    (should (equal (list r1 r2) (mevedel-session-reminders session)))))

(mevedel-deftest mevedel-session-remove-reminder
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry)
   :doc "`mevedel-session-remove-reminder' removes all reminders of a type")
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r1 (mevedel-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "1")))
         (r2 (mevedel-reminder-create :type 'b
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "2")))
         (r3 (mevedel-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "3"))))
    (mevedel-session-add-reminder session r1)
    (mevedel-session-add-reminder session r2)
    (mevedel-session-add-reminder session r3)
    (mevedel-session-remove-reminder session 'a)
    (should (equal (list r2) (mevedel-session-reminders session)))))


;;
;;; Firing logic

(mevedel-deftest mevedel-reminders--should-fire-p
  ()
  ,test
  (test)
  :doc "fires when trigger returns non-nil and no interval set"
  (let ((r (mevedel-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x"))))
    (should (mevedel-reminders--should-fire-p r 0 nil)))

  :doc "does not fire when trigger returns nil"
  (let ((r (mevedel-reminder-create :type 'a
                                    :trigger (lambda (_) nil)
                                    :content (lambda (_) "x"))))
    (should-not (mevedel-reminders--should-fire-p r 0 nil)))

  :doc "fires on first check even with interval (last-fired is nil)"
  (let ((r (mevedel-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 5)))
    (should (mevedel-reminders--should-fire-p r 0 nil)))

  :doc "does not fire before interval elapses"
  (let ((r (mevedel-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 5)))
    (setf (mevedel-reminder-last-fired r) 0)
    (should-not (mevedel-reminders--should-fire-p r 3 nil)))

  :doc "fires again after interval elapses"
  (let ((r (mevedel-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 5)))
    (setf (mevedel-reminder-last-fired r) 0)
    (should (mevedel-reminders--should-fire-p r 5 nil)))

  :doc "trigger receives the provided context object"
  (let* ((seen nil)
         (ctx (list 'ctx-marker))
         (r (mevedel-reminder-create
             :type 'a
             :trigger (lambda (c) (setq seen c) t)
             :content (lambda (_) "x"))))
    (mevedel-reminders--should-fire-p r 0 ctx)
    (should (eq seen ctx)))

  :doc "`one-shot' interval fires the first time"
  (let ((r (mevedel-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 'one-shot)))
    (should (mevedel-reminders--should-fire-p r 0 nil)))

  :doc "`one-shot' interval never fires again after last-fired is set"
  (let ((r (mevedel-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 'one-shot)))
    (setf (mevedel-reminder-last-fired r) 0)
    (should-not (mevedel-reminders--should-fire-p r 1 nil))
    (should-not (mevedel-reminders--should-fire-p r 9999 nil))))


;;
;;; Clone helpers

(mevedel-deftest mevedel-reminder-clone
  ()
  ,test
  (test)
  :doc "`mevedel-reminder-clone' copies all slots and resets last-fired"
  (let* ((trigger (lambda (_) t))
         (content (lambda (_) "x"))
         (r (mevedel-reminder-create
             :type 'a :trigger trigger :content content :interval 7
             :recipe '(verifier-read-only)))
         (_  (setf (mevedel-reminder-last-fired r) 42))
         (clone (mevedel-reminder-clone r)))
    (should (eq 'a (mevedel-reminder-type clone)))
    (should (eq trigger (mevedel-reminder-trigger clone)))
    (should (eq content (mevedel-reminder-content clone)))
    (should (equal 7 (mevedel-reminder-interval clone)))
    (should (null (mevedel-reminder-last-fired clone)))
    (should (equal '(verifier-read-only)
                   (mevedel-reminder-recipe clone))))

  :doc "`mevedel-reminder-clone' produces independent last-fired state"
  (let* ((r (mevedel-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "x")))
         (clone (mevedel-reminder-clone r)))
    (setf (mevedel-reminder-last-fired clone) 9)
    (should (equal 9 (mevedel-reminder-last-fired clone)))
    (should (null (mevedel-reminder-last-fired r)))))

(mevedel-deftest mevedel-reminders-clone-list
  ()
  ,test
  (test)
  :doc "`mevedel-reminders-clone-list' clones every reminder"
  (let* ((r1 (mevedel-reminder-create
              :type 'a :trigger (lambda (_) t) :content (lambda (_) "1")))
         (r2 (mevedel-reminder-create
              :type 'b :trigger (lambda (_) t) :content (lambda (_) "2")))
         (clones (mevedel-reminders-clone-list (list r1 r2))))
    (should (= 2 (length clones)))
    (should-not (eq r1 (nth 0 clones)))
    (should-not (eq r2 (nth 1 clones)))
    (should (eq 'a (mevedel-reminder-type (nth 0 clones))))
    (should (eq 'b (mevedel-reminder-type (nth 1 clones)))))

  :doc "clones track last-fired independently of the templates"
  (let* ((r (mevedel-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "x")
             :interval 3))
         (clone-a (car (mevedel-reminders-clone-list (list r))))
         (clone-b (car (mevedel-reminders-clone-list (list r)))))
    (mevedel-reminders--collect-from (list clone-a) 0 nil)
    (should (equal 0 (mevedel-reminder-last-fired clone-a)))
    (should (null (mevedel-reminder-last-fired clone-b)))
    (should (null (mevedel-reminder-last-fired r)))))

(mevedel-deftest mevedel-reminders--recipe-p
  ()
  ,test
  (test)
  :doc "accepts only trusted constructors with valid argument contracts"
  (should (mevedel-reminders--recipe-p '(mode-constraints 9)))
  (should (mevedel-reminders--recipe-p '(verifier-read-only)))
  (should-not (mevedel-reminders--recipe-p '(unknown-recipe)))
  (should-not (mevedel-reminders--recipe-p '(mode-constraints . 9)))
  (should-not (mevedel-reminders--recipe-p '(verifier-read-only 1)))
  (should-not (mevedel-reminders--recipe-p '(mode-constraints "often")))
  (should-not (mevedel-reminders--recipe-p '(max-turns-warning 2.0)))
  (should-not (mevedel-reminders--recipe-p '(token-usage 0.9 -1)))
  (should-not
   (mevedel-reminders--recipe-p
    `(mode-constraints ,(lambda () t)))))

(mevedel-deftest mevedel-reminders--recipe-arguments-p
  ()
  ,test
  (test)
  :doc "validates every declarative reminder argument contract"
  (should (mevedel-reminders--recipe-arguments-p 'no-args nil))
  (should (mevedel-reminders--recipe-arguments-p 'interval '(nil)))
  (should (mevedel-reminders--recipe-arguments-p 'interval '(0)))
  (should (mevedel-reminders--recipe-arguments-p 'ratio '(0.75)))
  (should (mevedel-reminders--recipe-arguments-p 'count '(40)))
  (should (mevedel-reminders--recipe-arguments-p
           'token-usage '(0.9 4)))
  (should-not (mevedel-reminders--recipe-arguments-p 'no-args '(1)))
  (should-not (mevedel-reminders--recipe-arguments-p 'interval '(-1)))
  (should-not (mevedel-reminders--recipe-arguments-p 'ratio '(1.1)))
  (should-not (mevedel-reminders--recipe-arguments-p 'count '(4.5)))
  (should-not (mevedel-reminders--recipe-arguments-p
               'token-usage '(0.9)))
  (should-not (mevedel-reminders--recipe-arguments-p 'unknown nil)))

(mevedel-deftest mevedel-reminders-serialize-agent-templates
  ()
  ,test
  (test)
  :doc "serializes trusted recipes and rejects closure-only templates"
  (let ((recipes
         (mevedel-reminders-serialize-agent-templates
          (list (mevedel-reminders-make-mode-constraints 9)
                (mevedel-reminders-make-verifier-read-only)))))
    (should (equal '((mode-constraints 9) (verifier-read-only)) recipes)))
  (should-error
   (mevedel-reminders-serialize-agent-templates
    (list (mevedel-reminder-create
           :type 'runtime-only
           :trigger (lambda (_) t)
           :content (lambda (_) "runtime"))))))

(mevedel-deftest mevedel-reminders-restore-agent-templates
  ()
  ,test
  (test)
  :doc "rebuilds frozen templates only through trusted recipe factories"
  (let ((restored
         (mevedel-reminders-restore-agent-templates
          '((mode-constraints 9) (verifier-read-only)))))
    (should (equal '(mode-constraints verifier-read-only)
                   (mapcar #'mevedel-reminder-type restored)))
    (should (= 9 (mevedel-reminder-interval (car restored)))))
  (should-error
   (mevedel-reminders-restore-agent-templates
    '((erase-buffer))))
  (should-error
   (mevedel-reminders-restore-agent-templates
    '((verifier-read-only 1))))
  (should-error
   (mevedel-reminders-restore-agent-templates
    '((max-turns-warning 4.0))))
  (should-error
   (mevedel-reminders-restore-agent-templates
    `((mode-constraints ,(lambda () t))))))


;;
;;; collect-from

(mevedel-deftest mevedel-reminders--collect-from
  ()
  ,test
  (test)
  :doc "`mevedel-reminders--collect-from' collects all firing reminders"
  (let* ((r1 (mevedel-reminder-create
              :type 'a :trigger (lambda (_) t) :content (lambda (_) "one")))
         (r2 (mevedel-reminder-create
              :type 'b :trigger (lambda (_) t) :content (lambda (_) "two")))
         (blocks (mevedel-reminders--collect-from (list r1 r2) 4 nil)))
    (should (equal (list "<system-reminder>\none\n</system-reminder>"
                         "<system-reminder>\ntwo\n</system-reminder>")
                   blocks))
    (should (equal 4 (mevedel-reminder-last-fired r1)))
    (should (equal 4 (mevedel-reminder-last-fired r2))))

  :doc "`mevedel-reminders--collect-from' passes context through to callbacks"
  (let* ((seen-trigger nil)
         (seen-content nil)
         (ctx (list 'agent-ctx))
         (r (mevedel-reminder-create
             :type 'a
             :trigger (lambda (c) (setq seen-trigger c) t)
             :content (lambda (c) (setq seen-content c) "ok"))))
    (mevedel-reminders--collect-from (list r) 0 ctx)
    (should (eq seen-trigger ctx))
    (should (eq seen-content ctx))))


;;
;;; Block formatting

(mevedel-deftest mevedel-reminders-format-block
  (:doc "`mevedel-reminders-format-block' wraps content in XML tags")
  (should (equal "<system-reminder>\nhello\n</system-reminder>"
                 (mevedel-reminders-format-block "hello"))))


;;
;;; Transform (integration with chat buffer)

(mevedel-deftest mevedel-reminders--transform
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "`mevedel-reminders--transform' leaves user text intact and stages blocks"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (prompt-buf (generate-new-buffer " *mevedel-test-prompt*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setf (mevedel-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK")))
            (mevedel-session-add-reminder
             session
             (mevedel-reminder-create
              :type 'demo
              :trigger (lambda (_) t)
              :content (lambda (_) "REMIND"))))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (mevedel-reminders--transform fsm)
            (should
             (equal
              "\n<hook-context>\n<hook-event name=\"SessionStart\">\nHOOK\n</hook-event>\n</hook-context>\nuser prompt body"
              (buffer-string)))
            (should
             (equal
              '("<system-reminder>\nREMIND\n</system-reminder>")
              (plist-get (gptel-fsm-info fsm)
                         :mevedel-reminder-blocks)))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "`mevedel-reminders--transform' is a no-op when no session is present"
  (let* ((prompt-buf (generate-new-buffer " *mevedel-test-prompt*"))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (with-current-buffer prompt-buf
          (insert "untouched")
          (mevedel-reminders--transform fsm)
          (should (equal "untouched" (buffer-string))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "`mevedel-reminders--transform' is a no-op when session has no reminders"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (prompt-buf (generate-new-buffer " *mevedel-test-prompt*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session))
          (with-current-buffer prompt-buf
            (insert "untouched")
            (mevedel-reminders--transform fsm)
            (should (equal "untouched" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf))))

(mevedel-deftest mevedel-reminders--handle-inject
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "injects staged reminders before the untouched current user message"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (backend (gptel-make-openai
                   "reminder-openai" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages
                     [(:role "assistant" :content "old")
                      (:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data
                           :mevedel-reminder-blocks
                           '("<system-reminder>\none\n</system-reminder>"
                             "<system-reminder>\ntwo\n</system-reminder>")))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (mevedel-reminders--handle-inject fsm)
          (let ((messages (plist-get data :messages)))
            (should (= 3 (length messages)))
            (should (equal "actual task"
                           (plist-get (aref messages 2) :content)))
            (should
             (equal
              "<system-reminder>\none\n</system-reminder>\n<system-reminder>\ntwo\n</system-reminder>"
              (plist-get (aref messages 1) :content))))
          (should-not (plist-get (gptel-fsm-info fsm)
                                 :mevedel-reminder-blocks))
          (mevedel-reminders--handle-inject fsm)
          (should (= 3 (length (plist-get data :messages)))))
      (kill-buffer chat-buf)))

  :doc "formats synthetic reminder messages for the active backend"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (backend (gptel-make-gemini
                   "reminder-gemini" :key "test" :models '(test)))
         (data (list :contents
                     [(:role "model" :parts [(:text "old")])
                      (:role "user" :parts [(:text "actual task")])]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data
                           :mevedel-reminder-blocks
                           '("<system-reminder>\ncontext\n</system-reminder>")))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (mevedel-reminders--handle-inject fsm)
          (let* ((contents (plist-get data :contents))
                 (reminder (aref contents 1)))
            (should (= 3 (length contents)))
            (should-not (plist-member reminder :content))
            (should
             (equal "<system-reminder>\ncontext\n</system-reminder>"
                    (plist-get (aref (plist-get reminder :parts) 0)
                               :text)))))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel-reminders-queue-turn-event
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "coalesces same-turn events and drops them when their owner changes"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (request-1 (mevedel-request--create :id "request-1" :session session))
         (request-2 (mevedel-request--create :id "request-2" :session session))
         (backend (gptel-make-openai
                   "reminder-events" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data)))
         committed)
    (unwind-protect
        (with-current-buffer chat-buf
          (setq-local mevedel--session session)
          (setq-local mevedel--current-request request-1)
          (mevedel-reminders-queue-turn-event
           chat-buf 'diagnostics "old" (lambda () (setq committed 'old)))
          (mevedel-reminders-queue-turn-event
           chat-buf 'diagnostics "new" (lambda () (setq committed 'new)))
          (should-not committed)
          (mevedel-reminders--handle-inject fsm)
          (should (eq committed 'new))
          (should (= 2 (length (plist-get data :messages))))
          (should
           (equal "<system-reminder>\nnew\n</system-reminder>"
                  (plist-get (aref (plist-get data :messages) 1) :content)))
          (mevedel-reminders-queue-turn-event
           chat-buf 'diagnostics "late" (lambda () (setq committed 'late)))
          (setq-local mevedel--current-request request-2)
          (mevedel-reminders--handle-inject fsm)
          (should (eq committed 'new))
          (should (= 2 (length (plist-get data :messages)))))
      (kill-buffer chat-buf))))


;;
;;; Agent invocation wiring

(mevedel-deftest mevedel-agent-invocation-create
  (:after-each (setq mevedel-agent--registry nil))
  ,test
  (test)

  :doc "creates an invocation with independent reminder clones"
  (let* ((r (mevedel-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "hi")))
         (_ (mevedel-define-agent inv-agent
              :description "Inv test"
              :tools nil
              :reminders (list r)))
         (agent (mevedel-agent-get "inv-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (clone (cl-find 'a (mevedel-agent-invocation-reminders inv)
                         :key #'mevedel-reminder-type)))
    (should (eq agent (mevedel-agent-invocation-agent inv)))
    (should (equal 0 (mevedel-agent-invocation-turn-count inv)))
    (should clone)
    ;; clone is not eq to original
    (should-not (eq r clone)))

  :doc "two invocations track last-fired independently"
  (let* ((r (mevedel-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "x")
             :interval 3))
         (_ (mevedel-define-agent inv-agent
              :description "Inv test"
              :tools nil
              :reminders (list r)))
         (agent (mevedel-agent-get "inv-agent"))
         (inv-a (mevedel-agent-invocation-create agent))
         (inv-b (mevedel-agent-invocation-create agent))
         (clone-a (cl-find 'a (mevedel-agent-invocation-reminders inv-a)
                           :key #'mevedel-reminder-type))
         (clone-b (cl-find 'a (mevedel-agent-invocation-reminders inv-b)
                           :key #'mevedel-reminder-type)))
    (mevedel-reminders--collect-from
     (mevedel-agent-invocation-reminders inv-a) 0 inv-a)
    (should (equal 0 (mevedel-reminder-last-fired clone-a)))
    (should (null (mevedel-reminder-last-fired clone-b))))

  :doc "reviewer invocation gets the read-only reminder attached"
  (let* ((_ (mevedel-define-agent reviewer
              :description "Reviewer test"
              :tools nil))
         (agent (mevedel-agent-get "reviewer"))
         (inv (mevedel-agent-invocation-create agent))
         (types (mapcar #'mevedel-reminder-type
                        (mevedel-agent-invocation-reminders inv))))
    (should (memq 'reviewer-read-only types)))

  :doc "verifier invocation still gets the read-only reminder attached"
  (let* ((_ (mevedel-define-agent verifier
              :description "Verifier test"
              :tools nil))
         (agent (mevedel-agent-get "verifier"))
         (inv (mevedel-agent-invocation-create agent))
         (types (mapcar #'mevedel-reminder-type
                        (mevedel-agent-invocation-reminders inv))))
    (should (memq 'verifier-read-only types)))

  :doc "ordinary agents do not get reviewer or verifier read-only reminders"
  (let* ((_ (mevedel-define-agent ordinary-agent
              :description "Ordinary test"
              :tools nil))
         (agent (mevedel-agent-get "ordinary-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (types (mapcar #'mevedel-reminder-type
                        (mevedel-agent-invocation-reminders inv))))
    (should-not (memq 'reviewer-read-only types))
    (should-not (memq 'verifier-read-only types))))









;;
;;; Tier 1 built-in reminders

(mevedel-deftest mevedel-reminders-make-mode-constraints
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when session permission mode is `ask'"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-mode-constraints))
         (mevedel-permission-mode 'ask))
    (should-not (mevedel-reminders--should-fire-p r 0 session)))

  :doc "fires when session permission mode is not `ask'"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-mode-constraints)))
    (setf (mevedel-session-permission-mode session) 'edits)
    (should (mevedel-reminders--should-fire-p r 0 session))
    (should (string-match-p "auto"
                            (funcall (mevedel-reminder-content r) session))))

  :doc "content varies by mode and falls back for unknown modes"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-mode-constraints)))
    (setf (mevedel-session-permission-mode session) 'edits)
    (should (string-match-p "auto"
                            (funcall (mevedel-reminder-content r) session)))
    (setf (mevedel-session-permission-mode session) 'weird-mode)
    (should (string-match-p "weird-mode"
                            (funcall (mevedel-reminder-content r) session))))

  :doc "default interval throttles to 5 turns"
  (let ((r (mevedel-reminders-make-mode-constraints)))
    (should (equal 5 (mevedel-reminder-interval r)))))

(mevedel-deftest mevedel-reminders-make-plan-mode
  (:doc "fires every turn only while Plan mode is active")
  ,test
  (test)
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "main" :plan-mode t))
         (reminder (mevedel-reminders-make-plan-mode))
         (skeleton
          (concat
           "<proposed_plan>\n"
           "# Concrete Plan Title\n\n"
           "## Summary\n"
           "- State the root cause or goal, intended behavior change, and important non-goals.\n\n"
           "## Key Changes\n"
           "- Group implementation bullets by subsystem or behavior, not by file inventory. Mention files, public APIs, interfaces, or data shape changes only when needed to remove ambiguity.\n\n"
           "## Regression Coverage\n"
           "- List the user-visible flows, edge cases, and failure scenarios that tests must cover.\n\n"
           "## Validation\n"
           "- List exact focused test/build commands to run.\n\n"
           "## Assumptions\n"
           "- Record defaults, compatibility assumptions, and intentionally unchanged behavior.\n"
           "</proposed_plan>")))
    (should (funcall (mevedel-reminder-trigger reminder) session))
    (should-not (mevedel-reminder-interval reminder))
    (let ((content (funcall (mevedel-reminder-content reminder) session)))
      (dolist (text '("Bash is limited"
                      "implementation request"
                      "Explore available evidence"
                      "genuine user preferences"
                      "replaces the previous proposal completely"
                      "Do not ask whether you should proceed"))
        (should (string-match-p text content)))
      (should (string-match-p (regexp-quote skeleton) content)))
    (setf (mevedel-reminder-last-fired reminder) 1)
    (should (mevedel-reminders--should-fire-p reminder 2 session))
    (should
     (string-match-p
      (regexp-quote skeleton)
      (funcall (mevedel-reminder-content reminder) session)))
    (setf (mevedel-session-plan-mode session) nil)
    (should-not (funcall (mevedel-reminder-trigger reminder) session))
    (let ((mevedel--current-request
           (mevedel-request--create :plan-read-only t)))
      (should (funcall (mevedel-reminder-trigger reminder) session)))
    (let ((mevedel--agent-invocation
           (mevedel-agent-invocation--create :plan-read-only t)))
      (should (funcall (mevedel-reminder-trigger reminder) session)))))

(mevedel-deftest mevedel-reminders-make-full-auto-mode
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "fires on full-auto entry and then sparsely"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-full-auto-mode)))
    (setf (mevedel-session-permission-mode session) 'full-auto)
    (should (mevedel-reminders--should-fire-p r 0 session))
    (setf (mevedel-reminder-last-fired r) 0)
    (should-not (mevedel-reminders--should-fire-p r 1 session))
    (should (mevedel-reminders--should-fire-p r 5 session))))

(mevedel-deftest mevedel-reminders-make-full-auto-mode-exit
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "full-auto-mode-exit fires once after leaving full-auto"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p2/" "/tmp/p2/" "p2"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-full-auto-mode-exit)))
    (setf (mevedel-session-permission-mode session) 'ask)
    (should (mevedel-reminders--should-fire-p r 0 session))
    (setf (mevedel-reminder-last-fired r) 0)
    (should-not (mevedel-reminders--should-fire-p r 1 session))))


(mevedel-deftest mevedel-reminders-make-max-turns-warning
  (:after-each (setq mevedel-agent--registry nil))
  ,test
  (test)

  :doc "does not fire below the threshold"
  (let* ((_ (mevedel-define-agent mt-agent
              :description "d"
              :tools nil
              :max-turns 10))
         (agent (mevedel-agent-get "mt-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (r (mevedel-reminders-make-max-turns-warning)))
    (setf (mevedel-agent-invocation-turn-count inv) 5)
    (should-not (mevedel-reminders--should-fire-p r 5 inv)))

  :doc "fires at the default 80% threshold"
  (let* ((_ (mevedel-define-agent mt-agent
              :description "d"
              :tools nil
              :max-turns 10))
         (agent (mevedel-agent-get "mt-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (r (mevedel-reminders-make-max-turns-warning)))
    (setf (mevedel-agent-invocation-turn-count inv) 8)
    (should (mevedel-reminders--should-fire-p r 8 inv))
    (should (string-match-p "8 of 10"
                            (funcall (mevedel-reminder-content r) inv))))

  :doc "does not fire for agents without a max-turns cap"
  (let* ((_ (mevedel-define-agent mt-agent
              :description "d"
              :tools nil))
         (agent (mevedel-agent-get "mt-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (r (mevedel-reminders-make-max-turns-warning)))
    (setf (mevedel-agent-invocation-turn-count inv) 999)
    (should-not (mevedel-reminders--should-fire-p r 999 inv)))

  :doc "is one-shot"
  (let ((r (mevedel-reminders-make-max-turns-warning)))
    (should (eq 'one-shot (mevedel-reminder-interval r)))))


(mevedel-deftest mevedel-reminders-make-verifier-read-only
  ()
  ,test
  (test)

  :doc "type is verifier-read-only"
  (let ((r (mevedel-reminders-make-verifier-read-only)))
    (should (eq 'verifier-read-only (mevedel-reminder-type r))))

  :doc "trigger fires unconditionally"
  (let ((r (mevedel-reminders-make-verifier-read-only)))
    (should (funcall (mevedel-reminder-trigger r) nil))
    (should (funcall (mevedel-reminder-trigger r) 'anything)))

  :doc "content mentions read-only and verification"
  (let* ((r (mevedel-reminders-make-verifier-read-only))
         (body (funcall (mevedel-reminder-content r) nil)))
    (should (string-match-p "CANNOT edit" body))
    (should (string-match-p "VERIFICATION" body))
    (should (string-match-p "environmental limitations" body))))

(mevedel-deftest mevedel-reminders-make-reviewer-read-only
  ()
  ,test
  (test)

  :doc "type is reviewer-read-only"
  (let ((r (mevedel-reminders-make-reviewer-read-only)))
    (should (eq 'reviewer-read-only (mevedel-reminder-type r))))

  :doc "trigger fires unconditionally"
  (let ((r (mevedel-reminders-make-reviewer-read-only)))
    (should (funcall (mevedel-reminder-trigger r) nil))
    (should (funcall (mevedel-reminder-trigger r) 'anything)))

  :doc "content mentions read-only JSON review reporting"
  (let* ((r (mevedel-reminders-make-reviewer-read-only))
         (body (funcall (mevedel-reminder-content r) nil)))
    (should (string-match-p "REVIEW-ONLY" body))
    (should (string-match-p "CANNOT edit" body))
    (should (string-match-p "write, or create files" body))
    (should (string-match-p "do not patch" body))
    (should (string-match-p "strict JSON" body))
    (should (string-match-p "findings" body)))

  :doc "fires every turn"
  (let ((r (mevedel-reminders-make-reviewer-read-only)))
    (should (null (mevedel-reminder-interval r)))))


(mevedel-deftest mevedel-reminders-make-verification-suggestion
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when session has no touched files"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (should-not (funcall (mevedel-reminder-trigger r) session)))

  :doc "fires once session has touched at least one file"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (puthash "/tmp/example.el" t (mevedel-session-touched-files session))
    (should (funcall (mevedel-reminder-trigger r) session))
    (should (string-match-p "verifier"
                            (funcall (mevedel-reminder-content r) session))))

  :doc "still fires after accepted-plan verification is cleared"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (setf (mevedel-session-plan-metadata session)
          '(:status accepted :verification-pending nil))
    (puthash "/tmp/example.el" t (mevedel-session-touched-files session))
    (should (funcall (mevedel-reminder-trigger r) session))
    (should-not (string-match-p
                 "accepted plan"
                 (funcall (mevedel-reminder-content r) session))))

  :doc "fires for rejected/cancelled/presented metadata without accepted wording"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (puthash "/tmp/example.el" t (mevedel-session-touched-files session))
    (dolist (status '(rejected cancelled presented))
      (setf (mevedel-session-plan-metadata session)
            (list :status status :verification-pending t))
      (should (funcall (mevedel-reminder-trigger r) session))
      (should-not (string-match-p
                   "accepted plan"
                   (funcall (mevedel-reminder-content r) session)))))

  :doc "fires for touched files while accepted-plan verification is pending"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (setf (mevedel-session-plan-metadata session)
          '(:status accepted :verification-pending t))
    (puthash "/tmp/example.el" t (mevedel-session-touched-files session))
    (should (funcall (mevedel-reminder-trigger r) session))
    (should (string-match-p "plan"
                            (funcall (mevedel-reminder-content r) session)))))


(mevedel-deftest mevedel-reminders-make-plan-reference
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "surfaces verified plan contents instead of a poisoned fixed cache"
  (let* ((tmp (make-temp-file "mevedel-plan-ref-" t))
         (ws (mevedel-workspace-get-or-create
              'file (file-name-as-directory tmp)
              (file-name-as-directory tmp) "pr"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-plan-reference))
         (plan-path (file-name-concat tmp "local" "plans" "current.md"))
         (accepted-path
          (file-name-concat tmp "local" "plans"
                            "accepted-20260813-120000.md")))
    (make-directory (file-name-directory plan-path) t)
    (write-region "# Current draft" nil plan-path nil 'silent)
    (write-region "# Accepted\n\nDo it." nil accepted-path nil 'silent)
    (setf (mevedel-session-save-path session) tmp)
    (setf (mevedel-session-turn-count session) 6)
    (setf (mevedel-session-plan-metadata session)
          '(:path "local/plans/current.md"
            :accepted-path "local/plans/accepted-20260813-120000.md"
            :status accepted :accepted-turn 5))
    (should (funcall (mevedel-reminder-trigger r) session))
    (let ((content (funcall (mevedel-reminder-content r) session)))
      (should (string-match-p
               "local://plans/accepted-20260813-120000.md"
               content))
      (should-not (string-match-p "local/plans/current.md" content))
      (should-not (string-match-p "local://plans/current.md" content))
      (should-not (string-match-p (regexp-quote tmp) content))
      (should (string-match-p "# Accepted" content))
      (should-not (string-match-p "# Current draft" content))))

  :doc "waits until after the acceptance turn before firing"
  (let* ((tmp (make-temp-file "mevedel-plan-ref-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "pr"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-plan-reference))
         (plan-path (file-name-concat tmp "local" "plans" "current.md"))
         (accepted-path
          (file-name-concat tmp "local" "plans"
                            "accepted-20260813-120000.md")))
    (make-directory (file-name-directory plan-path) t)
    (write-region "# Plan\n\nDo it." nil plan-path nil 'silent)
    (write-region "# Accepted\n\nDo it." nil accepted-path nil 'silent)
    (setf (mevedel-session-save-path session) tmp)
    (setf (mevedel-session-turn-count session) 5)
    (setf (mevedel-session-plan-metadata session)
          '(:path "local/plans/current.md"
            :accepted-path "local/plans/accepted-20260813-120000.md"
            :status accepted :accepted-turn 5))
    ;; A portable session reads its plan through the publication, never
    ;; the fixed cache, so the artifact has to be served here.
    (cl-letf (((symbol-function
                'mevedel-session-persistence-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-session-persistence-read-artifact)
               (lambda (&rest _)
                 (encode-coding-string "# Plan\n\nDo it." 'utf-8-unix))))
      (should-not (funcall (mevedel-reminder-trigger r) session))
      (setf (mevedel-session-turn-count session) 6)
      (should (funcall (mevedel-reminder-trigger r) session))))

  :doc "does not fire or read the mutable plan without an accepted artifact"
  (let* ((tmp (make-temp-file "mevedel-plan-ref-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "pr"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-plan-reference))
         (plan-path (file-name-concat tmp "local" "plans" "current.md")))
    (make-directory (file-name-directory plan-path) t)
    (write-region "# Mutable current plan" nil plan-path nil 'silent)
    (setf (mevedel-session-save-path session) tmp)
    (setf (mevedel-session-turn-count session) 6)
    (setf (mevedel-session-plan-metadata session)
          '(:path "local/plans/current.md"
            :status accepted :accepted-turn 5))
    (should-not (funcall (mevedel-reminder-trigger r) session))
    (should-not (funcall (mevedel-reminder-content r) session)))

  :doc "stays suppressed during a standalone Plan conversation"
  (let ((session
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "main" :plan-mode t :turn-count 2
          :plan-metadata '(:status accepted :accepted-turn 1))))
    (should-not
     (funcall (mevedel-reminder-trigger
               (mevedel-reminders-make-plan-reference))
              session))))


(mevedel-deftest mevedel-reminders-make-task-nudge
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when session has no tasks"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge)))
    (should-not (funcall (mevedel-reminder-trigger r) session)))

  :doc "does not fire before the task write is stale"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge 3)))
    (push (mevedel-task--create :id 1 :subject "do thing" :status 'in-progress)
          (mevedel-session-tasks session))
    (setf (mevedel-session-last-task-write-turn session) 5)
    (setf (mevedel-session-turn-count session) 7)
    (should-not (funcall (mevedel-reminder-trigger r) session)))

  :doc "fires when non-completed tasks are stale"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge 3)))
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main work" :status 'in-progress)
                (mevedel-task--create
                 :id 2 :subject "agent work" :status 'pending
                 :owner "worker")
                (mevedel-task--create
                 :id 3 :subject "done work" :status 'completed
                 :owner "worker")
                (mevedel-task--create
                 :id 4 :subject "completed-only work" :status 'completed
                 :owner "finished")))
    (setf (mevedel-session-last-task-write-turn session) 5)
    (setf (mevedel-session-turn-count session) 8)
    (should (funcall (mevedel-reminder-trigger r) session))
    (let ((content (substring-no-properties
                    (funcall (mevedel-reminder-content r) session))))
      (should (string-match-p "Main · 1 open · 0 done" content))
      (should (string-match-p "main work" content))
      (should (string-match-p "worker · 1 open · 1 done" content))
      (should (string-match-p "agent work" content))
      (should-not (string-match-p "finished" content))
      (should-not (string-match-p "completed-only work" content))
      (should-not (string-match-p "done work" content))
      (should-not (string-match-p "completed hidden" content))))

  :doc "does not fire when active tasks have no recorded write turn"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge 3)))
    (push (mevedel-task--create :id 1 :subject "do thing" :status 'pending)
          (mevedel-session-tasks session))
    (setf (mevedel-session-turn-count session) 100)
    (should-not (funcall (mevedel-reminder-trigger r) session)))

  :doc "does not fire when all tasks are completed"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge)))
    (push (mevedel-task--create :id 1 :subject "done thing" :status 'completed)
          (mevedel-session-tasks session))
    (setf (mevedel-session-last-task-write-turn session) 1)
    (setf (mevedel-session-turn-count session) 100)
    (should-not (funcall (mevedel-reminder-trigger r) session))))


(mevedel-deftest mevedel-reminders-install-defaults--verification
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "installs the verification-suggestion reminder"
  (let* ((tmp (make-temp-file "mevedel-install-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "ivs"))
         (session (mevedel-session-create "main" ws)))
    (mevedel-reminders-install-defaults session)
    (should (cl-some (lambda (r)
                       (eq (mevedel-reminder-type r) 'verification-suggestion))
                     (mevedel-session-reminders session)))))


(mevedel-deftest mevedel-reminders--diagnostics-record-current
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "classifies the first refresh against baseline and later refreshes against the last report"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "sample.el"))
         (ws (mevedel-workspace-get-or-create 'project root root "diag"))
         (session (mevedel-session-create "main" ws))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         (source nil)
         (backend (gptel-make-openai
                   "reminder-diagnostics" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat :backend backend :data data)))
         (old nil)
         (fixed nil)
         (new nil))
    (unwind-protect
        (progn
          (write-region "(message \"ok\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file)
                old (list file 1 "error" "old")
                fixed (list file 2 "warning" "fixed")
                new (list file 3 "warning" "new"))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda () (list old fixed))))
            (mevedel-reminders-diagnostics-before-edit chat file))
          (mevedel-reminders--diagnostics-record-current
           chat file (list old new))
          (mevedel-reminders--handle-inject fsm)
          (let ((body (plist-get
                       (aref (plist-get data :messages) 1) :content)))
            (should (string-match-p "New or changed diagnostics" body))
            (should (string-match-p "new" body))
            (should (string-match-p "Pre-existing diagnostics" body))
            (should (string-match-p "old" body))
            (should-not (string-match-p "fixed" body)))
          (mevedel-reminders--diagnostics-record-current
           chat file (list (list file 1 "error" "changed")))
          (mevedel-reminders--handle-inject fsm)
          (let ((body (plist-get
                       (aref (plist-get data :messages) 2) :content)))
            (should (string-match-p "changed" body))
            (should-not (string-match-p "Pre-existing diagnostics" body))
            (should-not (string-match-p "\\[error\\] old" body))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t))))
(mevedel-deftest mevedel-reminders-diagnostics-after-edit
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "starts active checkers and continues after their fresh results are recorded"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         started
         telemetry
         continued)
    (unwind-protect
        (progn
          (write-region "(message \"old\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flymake-mode t))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda () nil)))
            (mevedel-reminders-diagnostics-before-edit chat file))
          (cl-letf (((symbol-function 'flymake-start)
                     (lambda (&rest _) (setq started t)))
                    ((symbol-function 'flymake-disabled-backends)
                     (lambda () nil))
                    ((symbol-function 'mevedel-telemetry-current-session)
                     (lambda (&optional _buffer) session))
                    ((symbol-function 'mevedel-telemetry-record)
                     (lambda (_session _event &rest props)
                       (push (plist-get props :outcome) telemetry)))
                    ((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda () (list (list file 1 "error" "fresh")))))
            (mevedel-reminders-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should started)
          (should continued)
          (should (equal '(started ready) (nreverse telemetry)))
          (with-current-buffer chat
            (should (assq 'diagnostics
                          (plist-get mevedel-reminders--turn-events
                                     :items)))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t)))
  :doc "restarts an in-flight Flycheck run before collecting results"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         events
         continued)
    (unwind-protect
        (progn
          (write-region "(message \"old\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flycheck-mode t))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda () nil)))
            (mevedel-reminders-diagnostics-before-edit chat file))
          (cl-letf (((symbol-function 'flycheck-stop)
                     (lambda () (push 'stop events)))
                    ((symbol-function 'flycheck-get-checker-for-buffer)
                     (lambda () 'test-checker))
                    ((symbol-function 'flycheck-buffer)
                     (lambda ()
                       (push 'start events)
                       (run-hooks 'flycheck-after-syntax-check-hook)))
                    ((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda ()
                       (push 'collect events)
                       nil)))
            (mevedel-reminders-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should continued)
          (should (equal '(stop start collect) (nreverse events))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t)))
  :doc "continues without collecting stale overlays when checker startup fails"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         collected
         continued)
    (unwind-protect
        (progn
          (write-region "(message \"old\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flycheck-mode t))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda () nil)))
            (mevedel-reminders-diagnostics-before-edit chat file))
          (cl-letf (((symbol-function 'flycheck-stop) #'ignore)
                    ((symbol-function 'flycheck-get-checker-for-buffer)
                     (lambda () 'test-checker))
                    ((symbol-function 'flycheck-buffer)
                     (lambda () (error "Cannot start checker")))
                    ((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda ()
                       (setq collected t)
                       nil)))
            (mevedel-reminders-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should continued)
          (should-not collected))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t)))
  :doc "does not discard an unsaved buffer when the edited file changed on disk"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         started
         continued)
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flymake-mode t)
            (goto-char (point-max))
            (insert "unsaved\n"))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-reminders--collect-diagnostics-in-buffer)
                     (lambda () nil)))
            (mevedel-reminders-diagnostics-before-edit chat file))
          (with-temp-buffer
            (insert "external\n")
            (write-region nil nil file nil 'silent))
          (cl-letf (((symbol-function 'flymake-start)
                     (lambda (&rest _) (setq started t))))
            (mevedel-reminders-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should continued)
          (should-not started)
          (with-current-buffer source
            (should (string-match-p "unsaved" (buffer-string)))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (set-buffer-modified-p nil))
        (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t))))

(mevedel-deftest mevedel-reminders--diagnostics-run-checkers
  ()
  ,test
  (test)
  :doc "settles checker events, timeout, and dead-buffer cleanup exactly once"
  (let ((chat (generate-new-buffer " *mevedel-diag-chat*"))
        (source (generate-new-buffer " *mevedel-diag-source*"))
        (owner 'request-1)
        flymake-report timer-callback cancelled telemetry continued)
    (unwind-protect
        (cl-letf (((symbol-function 'flymake-make-report-fn)
                   (lambda (&rest _)
                     (lambda (&rest _) nil)))
                  ((symbol-function 'flymake-start)
                   (lambda (&rest _)
                     (let ((synchronous
                            (flymake-make-report-fn 'sync-backend)))
                       (setq flymake-report
                             (flymake-make-report-fn 'async-backend))
                       (funcall synchronous nil))))
                  ((symbol-function 'flymake-disabled-backends)
                   (lambda () nil))
                  ((symbol-function 'flycheck-stop) #'ignore)
                  ((symbol-function 'flycheck-get-checker-for-buffer)
                   (lambda () 'test-checker))
                  ((symbol-function 'flycheck-buffer) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (delay repeat function &rest _)
                     (should (= 30 delay))
                     (should-not repeat)
                     (setq timer-callback function)
                     'diagnostic-timer))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (setq cancelled timer)))
                  ((symbol-function 'mevedel-reminders--turn-owner)
                   (lambda (_) owner))
                  ((symbol-function
                    'mevedel-reminders--collect-diagnostics-in-buffer)
                   (lambda () '(("source.el" 1 "error" "fresh"))))
                  ((symbol-function
                    'mevedel-reminders--diagnostics-record-current)
                   (lambda (&rest _) '(:new (diagnostic))))
                  ((symbol-function
                    'mevedel-reminders--record-diagnostic-telemetry)
                   (lambda (_buffer outcome &optional _report)
                     (push outcome telemetry))))
          (mevedel-reminders--diagnostics-run-checkers
           chat "/tmp/source.el" source owner
           (lambda () (setq continued t)) t t)
          (should timer-callback)
          (should-not continued)
          (with-current-buffer source
            (run-hooks 'flycheck-after-syntax-check-hook))
          (should-not continued)
          (funcall flymake-report nil)
          (should continued)
          (should (eq 'diagnostic-timer cancelled))
          (should (equal '(started ready) (nreverse telemetry)))
          (with-current-buffer source
            (should-not flycheck-after-syntax-check-hook))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (mevedel-reminders--diagnostics-run-checkers
           chat "/tmp/source.el" source owner
           (lambda () (setq continued t)) nil t)
          (funcall timer-callback)
          (should continued)
          (should (eq 'diagnostic-timer cancelled))
          (should (equal '(started timeout) (nreverse telemetry)))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (cl-letf (((symbol-function 'flymake-start)
                     (lambda (&rest _)
                       (flymake-make-report-fn 'failed-backend)))
                    ((symbol-function 'flymake-disabled-backends)
                     (lambda () '(failed-backend))))
            (mevedel-reminders--diagnostics-run-checkers
             chat "/tmp/source.el" source owner
             (lambda () (setq continued t)) t nil))
          (should continued)
          (should-not timer-callback)
          (should (equal '(started ready) (nreverse telemetry)))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (let ((selected 0)
                (stopped 0)
                (started 0))
            (cl-letf (((symbol-function 'flycheck-get-checker-for-buffer)
                       (lambda () (cl-incf selected) nil))
                      ((symbol-function 'flycheck-stop)
                       (lambda () (cl-incf stopped)))
                      ((symbol-function 'flycheck-buffer)
                       (lambda () (cl-incf started))))
              (mevedel-reminders--diagnostics-run-checkers
               chat "/tmp/source.el" source owner
               (lambda () (setq continued t)) nil t))
            (should (= 1 selected))
            (should (zerop stopped))
            (should (zerop started)))
          (should continued)
          (should-not timer-callback)
          (should-not telemetry)
          (let ((continuation-calls 0)
                late-report)
            (cl-letf (((symbol-function 'flymake-start)
                       (lambda (&rest _)
                         (setq late-report
                               (flymake-make-report-fn 'failed-backend))
                         (error "Cannot start backend"))))
              (mevedel-reminders--diagnostics-run-checkers
               chat "/tmp/source.el" source owner
               (lambda () (cl-incf continuation-calls)) t nil))
            (should (= 1 continuation-calls))
            (funcall late-report nil)
            (should (= 1 continuation-calls)))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (mevedel-reminders--diagnostics-run-checkers
           chat "/tmp/source.el" source owner
           (lambda () (setq continued t)) nil t)
          (kill-buffer source)
          (funcall timer-callback)
          (should continued)
          (should (eq 'diagnostic-timer cancelled))
          (should (equal '(started stale) (nreverse telemetry))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat)))))

(mevedel-deftest mevedel-reminders--format-diagnostic-reports
  ()
  ,test
  (test)
  :doc "prioritizes new diagnostics and enforces per-file and total limits"
  (let* ((files '("/tmp/a.el" "/tmp/b.el" "/tmp/c.el" "/tmp/d.el"))
         (reports
          (mapcar
           (lambda (file)
             (list :path file
                   :new
                   (cl-loop for line from 1 to 12
                            collect (list file line "error"
                                          (format "problem-%d" line)))
                   :preexisting nil))
           files))
         (body (mevedel-reminders--format-diagnostic-reports reports)))
    (should (= 30 (with-temp-buffer
                    (insert body)
                    (goto-char (point-min))
                    (how-many "^  "))))
    (should (= 18 (get-text-property
                   0 'mevedel-diagnostics-omitted body)))
    (should (string-match-p "18 diagnostics omitted" body))
    (should (string-match-p "New or changed diagnostics" body))
    (should-not (string-match-p "Pre-existing diagnostics" body))))


(mevedel-deftest mevedel-reminders-make-edited-file
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when no cached files changed"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello"))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (should-not (mevedel-reminders--should-fire-p r 0 session)))
      (delete-directory tmp-root t)))

  :doc "fires and formats a diff for modified cached file"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "goodbye\n"))
            (set-file-times file future))
          (should (mevedel-reminders--should-fire-p r 0 session))
          (let ((content (funcall (mevedel-reminder-content r) session)))
            (should (string-match-p "MODIFIED:" content))
            (should (string-match-p (regexp-quote (expand-file-name file))
                                    content))
            (should (string-match-p "^-hello" content))
            (should (string-match-p "^\\+goodbye" content))))
      (delete-directory tmp-root t)))

  :doc "fires for deleted cached file and labels it"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello"))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (delete-file file)
          (should (mevedel-reminders--should-fire-p r 0 session))
          (let ((content (funcall (mevedel-reminder-content r) session)))
            (should (string-match-p "DELETED:" content))
            (should (string-match-p (regexp-quote (expand-file-name file))
                                    content))))
      (when (file-exists-p tmp-root) (delete-directory tmp-root t))))

  :doc "consuming changes prevents the same change from firing twice"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello"))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "world"))
            (set-file-times file future))
          ;; First firing formats + consumes.
          (funcall (mevedel-reminder-content r) session)
          (should-not (mevedel-reminders--should-fire-p r 0 session)))
      (delete-directory tmp-root t)))

  :doc "truncates diffs longer than the configured limit"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file 5)))
    (unwind-protect
        (progn
          (with-temp-file file
            (dotimes (i 40)
              (insert (format "line %d\n" i))))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file
              (dotimes (i 40)
                (insert (format "changed %d\n" i))))
            (set-file-times file future))
          (let ((content (funcall (mevedel-reminder-content r) session)))
            (should (string-match-p "more lines truncated" content))))
      (delete-directory tmp-root t)))

  :doc "memoizes detect-external-changes across trigger and content"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file))
         (call-count 0)
         (real-detect (symbol-function
                       'mevedel-file-cache-detect-external-changes)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "goodbye\n"))
            (set-file-times file future))
          (cl-letf (((symbol-function 'mevedel-file-cache-detect-external-changes)
                     (lambda (cache)
                       (cl-incf call-count)
                       (funcall real-detect cache))))
            ;; Trigger + content on a single firing should only hit
            ;; detect-external-changes once.
            (should (mevedel-reminders--should-fire-p r 0 session))
            (funcall (mevedel-reminder-content r) session)
            (should (= 1 call-count))
            ;; A fresh trigger on the next turn recomputes; no changes
            ;; this time, so content is not called.
            (setq call-count 0)
            (should-not (mevedel-reminders--should-fire-p r 1 session))
            (should (= 1 call-count))))
      (delete-directory tmp-root t))))


(mevedel-deftest mevedel-reminders-make-deferred-tools-roster
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when the session's deferred-set is empty"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-deferred-tools-roster)))
    (should-not (mevedel-reminders--should-fire-p r 0 session)))

  :doc "fires once (`one-shot') listing every entry with a ToolSearch hint"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-deferred-tools-roster)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "XrefReferences") . "Find references to a symbol")
            (("mevedel" "Edit") . "Replace text in a file")))
    (should (mevedel-reminders--should-fire-p r 0 session))
    (let ((body (funcall (mevedel-reminder-content r) session)))
      (should (string-match-p "XrefReferences" body))
      (should (string-match-p "Edit" body))
      (should (string-match-p "ToolSearch" body))
      (should (string-match-p "load=true" body))
      (should (string-match-p "not currently callable" body))
      (should (string-match-p "Do not call these tool names directly" body)))
    ;; Simulate firing bookkeeping and verify one-shot prevents re-fire.
    (setf (mevedel-reminder-last-fired r) 0)
    (should-not (mevedel-reminders--should-fire-p r 5 session)))

  :doc "name-only entries (nil summary) render without a colon"
  ;; Default for tools that did not supply :summary -- the deferred-set
  ;; entry's cdr is nil and the roster lists just "- NAME", keeping the
  ;; system reminder concise even when wrapped tools (introspection,
  ;; web) have multi-paragraph descriptions.
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-deferred-tools-roster)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel-introspection" "function_source") . nil)
            (("mevedel-introspection" "variable_value") . nil)))
    (let ((body (funcall (mevedel-reminder-content r) session)))
      (should (string-match-p "^- function_source$" body))
      (should (string-match-p "^- variable_value$" body))
      (should-not (string-match-p "function_source:" body))
      (should-not (string-match-p "variable_value:" body))))

  :doc "summary entries render as `- NAME: SUMMARY'"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-deferred-tools-roster)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Bash") . "Run a shell command.")))
    (let ((body (funcall (mevedel-reminder-content r) session)))
      (should (string-match-p "- Bash: Run a shell command\\." body))))

  :doc "empty-string summary falls through to name-only rendering"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-deferred-tools-roster)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Whatever") . "")))
    (let ((body (funcall (mevedel-reminder-content r) session)))
      (should (string-match-p "^- Whatever$" body))
      (should-not (string-match-p "Whatever:" body)))))

(mevedel-deftest mevedel-reminders-make-deferred-tools-expired
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when no tools expired"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-deferred-tools-expired)))
    (should-not (mevedel-reminders--should-fire-p r 0 session)))

  :doc "fires listing expired tool names and consumes the slot"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-deferred-tools-expired)))
    (setf (mevedel-session-deferred-expired session)
          '("XrefReferences" "XrefDefinitions"))
    (should (mevedel-reminders--should-fire-p r 1 session))
    (let ((body (funcall (mevedel-reminder-content r) session)))
      (should (string-match-p "XrefReferences" body))
      (should (string-match-p "XrefDefinitions" body))
      (should (string-match-p "ToolSearch" body))
      (should (string-match-p "load=true" body))
      (should (string-match-p "no longer callable" body))
      (should (string-match-p "Do not call these tool names directly" body)))
    ;; Content function consumed the list, so the reminder no longer fires.
    (should-not (mevedel-session-deferred-expired session))
    (should-not (mevedel-reminders--should-fire-p r 2 session))))


(mevedel-deftest mevedel-reminders-make-agent-deferred-tools-expired
  ()
  ,test
  (test)

  :doc "fires listing expired agent tool names and consumes the invocation slot"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :deferred-expired '("Imenu" "Treesitter")))
         (r (mevedel-reminders-make-agent-deferred-tools-expired)))
    (should (mevedel-reminders--should-fire-p r 1 inv))
    (let ((body (funcall (mevedel-reminder-content r) inv)))
      (should (string-match-p "Imenu" body))
      (should (string-match-p "Treesitter" body))
      (should (string-match-p "ToolSearch" body)))
    (should-not (mevedel-agent-invocation-deferred-expired inv))
    (should-not (mevedel-reminders--should-fire-p r 2 inv))))


(mevedel-deftest mevedel-reminders-make-pending-events
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "fires for queued pending reminders and consumes them"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-pending-events)))
    (mevedel-session-enqueue-pending-reminder session "first")
    (mevedel-session-enqueue-pending-reminder session "second")
    (should (mevedel-reminders--should-fire-p r 0 session))
    (should (equal "first\n\nsecond"
                   (funcall (mevedel-reminder-content r) session)))
    (should-not (mevedel-session-pending-reminders session))
    (should-not (mevedel-reminders--should-fire-p r 1 session))))

(mevedel-deftest mevedel-reminders-make-date-change
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "fires when the observed date differs and updates the session"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (today (format-time-string "%F"))
         (r (mevedel-reminders-make-date-change)))
    (setf (mevedel-session-last-observed-date session) "2000-01-01")
    (should (mevedel-reminders--should-fire-p r 0 session))
    (let ((body (funcall (mevedel-reminder-content r) session)))
      (should (string-match-p "2000-01-01" body))
      (should (string-match-p today body)))
    (should (equal today (mevedel-session-last-observed-date session)))
    (should-not (mevedel-reminders--should-fire-p r 1 session))))

(mevedel-deftest mevedel-reminders-make-context-pressure
  ()
  ,test
  (test)

  :doc "compaction-available fires when auto-compaction is enabled near threshold"
  (let ((r (mevedel-reminders-make-compaction-available 0.70))
        (mevedel-compact-auto t)
        (mevedel--compact-auto-disabled nil))
    (cl-letf (((symbol-function 'mevedel-reminders--compact-token-state)
               (lambda ()
                 '(:tokens 75 :threshold 80 :usable 100 :ratio 0.75)))
              ((symbol-function 'mevedel-reminders--compact-auto-available-p)
               (lambda () t)))
      (should (mevedel-reminders--should-fire-p r 0 nil))
      (should (string-match-p "Automatic compaction is available"
                              (funcall (mevedel-reminder-content r) nil)))))

  :doc "token-usage fires near high context pressure"
  (let ((r (mevedel-reminders-make-token-usage 0.90 4)))
    (cl-letf (((symbol-function 'mevedel-reminders--compact-token-state)
               (lambda ()
                 '(:tokens 95 :threshold 80 :usable 100 :ratio 0.95))))
      (should (mevedel-reminders--should-fire-p r 0 nil))
      (should (string-match-p "Context pressure is high"
                              (funcall (mevedel-reminder-content r) nil)))))

  :doc "compaction-available does not fire when auto-compaction is ineligible"
  (let ((r (mevedel-reminders-make-compaction-available 0.70)))
    (cl-letf (((symbol-function 'mevedel-reminders--compact-token-state)
               (lambda ()
                 '(:tokens 75 :threshold 80 :usable 100 :ratio 0.75)))
              ((symbol-function 'mevedel-reminders--compact-auto-available-p)
               (lambda () nil)))
      (should-not (mevedel-reminders--should-fire-p r 0 nil))))

  :doc "compaction availability checks chat-buffer-local eligibility"
  (let ((chat (generate-new-buffer " *mevedel-compact-reminder-chat*"))
        (r (mevedel-reminders-make-compaction-available 0.70)))
    (unwind-protect
        (let ((mevedel-reminders--current-chat-buffer chat))
          (with-current-buffer chat
            (setq-local mevedel--compact-auto-disabled t))
          (cl-letf (((symbol-function 'mevedel-reminders--compact-token-state)
                     (lambda ()
                       '(:tokens 75 :threshold 80 :usable 100 :ratio 0.75)))
                    ((symbol-function 'mevedel--compact-auto-eligible-p)
                     (lambda ()
                       (not (bound-and-true-p
                             mevedel--compact-auto-disabled)))))
            (should-not (mevedel-reminders--should-fire-p r 0 nil))))
      (kill-buffer chat))))

(mevedel-deftest mevedel-reminders-make-agent-listing-delta
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "initializes silently, then reports added and removed agent types"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-agent-listing-delta))
         (buf (generate-new-buffer " *mevedel-agent-delta*")))
    (unwind-protect
        (let ((mevedel-reminders--current-chat-buffer buf))
          (with-current-buffer buf
            (setq-local mevedel-agents--specs
                        '(("explorer" . (:description "Explore")))))
          (should-not (mevedel-reminders--should-fire-p r 0 session))
          (should (equal '(("explorer" . "Explore"))
                         (mevedel-session-agent-types-snapshot session)))
          (with-current-buffer buf
            (setq-local mevedel-agents--specs
                        '(("verifier" . (:description "Verify")))))
          (should (mevedel-reminders--should-fire-p r 1 session))
          (let ((body (funcall (mevedel-reminder-content r) session)))
            (should (string-match-p "Added:" body))
            (should (string-match-p "verifier" body))
            (should (string-match-p "Removed:" body))
            (should (string-match-p "explorer" body))))
      (kill-buffer buf))))


(mevedel-deftest mevedel-reminders-specialist-capabilities
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "xref availability accepts eglot, lsp, elisp, and readable etags backends"
  (require 'xref)
  (dolist (backend '(eglot lsp elisp))
    (cl-letf (((symbol-function 'xref-find-backend)
               (lambda () backend)))
      (should (mevedel-reminders--xref-available-in-buffer-p))))
  (let ((tags-file-name nil)
        (tags-table-list nil))
    (cl-letf (((symbol-function 'xref-find-backend)
               (lambda () 'etags)))
      (should-not (mevedel-reminders--xref-available-in-buffer-p))))
  (let ((tags-file-name (make-temp-file "mevedel-tags-"))
        (tags-table-list nil))
    (unwind-protect
        (cl-letf (((symbol-function 'xref-find-backend)
                   (lambda () 'etags)))
          (should (mevedel-reminders--xref-available-in-buffer-p)))
      (delete-file tags-file-name)))

  :doc "Imenu availability requires a non-empty user-visible index"
  (require 'imenu)
  (let ((imenu--index-alist nil))
    (cl-letf (((symbol-function 'imenu--make-index-alist)
               (lambda (&optional _noerror)
                 (setq imenu--index-alist '(("*Rescan*" . ignore)
                                            ("thing" . 1))))))
      (should (mevedel-reminders--imenu-available-in-buffer-p))))
  (let ((imenu--index-alist nil))
    (cl-letf (((symbol-function 'imenu--make-index-alist)
               (lambda (&optional _noerror)
                 (setq imenu--index-alist nil))))
      (should-not (mevedel-reminders--imenu-available-in-buffer-p))))

  :doc "Treesitter availability requires an active parser"
  (cl-letf (((symbol-function 'treesit-available-p)
             (lambda () t))
            ((symbol-function 'treesit-parser-list)
             (lambda (&optional _buffer _language) '(parser))))
    (should (mevedel-reminders--treesitter-available-in-buffer-p)))

  :doc "Elisp introspection capability is gated by live elisp workspace buffers and tools"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-specialist-" t)))
         (file (file-name-concat root "sample.el"))
         (ws (mevedel-workspace-get-or-create 'project root root "specialist"))
         (session (mevedel-session-create "main" ws))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "(defun sample () nil)\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (with-current-buffer buf
            (emacs-lisp-mode))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "function_source") . "source")))
          (cl-letf (((symbol-function
                      'mevedel-reminders--xref-available-in-buffer-p)
                     (lambda () nil))
                    ((symbol-function
                      'mevedel-reminders--imenu-available-in-buffer-p)
                     (lambda () nil))
                    ((symbol-function
                      'mevedel-reminders--treesitter-available-in-buffer-p)
                     (lambda () nil)))
            (should (plist-get
                     (mevedel-reminders-specialist-capabilities session)
                     :elisp-introspection))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory root t))))


(mevedel-deftest mevedel-reminders-make-specialist-availability
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "xref reminder fires from specialist capabilities and includes ToolSearch hint when deferred"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/sp/" "/tmp/sp/" "sp"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-xref-available)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "XrefReferences") . "refs")))
    (cl-letf (((symbol-function 'mevedel-reminders-specialist-capabilities)
               (lambda (_) '(:xref t))))
      (should (mevedel-reminders--should-fire-p r 0 session))
      (let ((body (funcall (mevedel-reminder-content r) session)))
        (should (string-match-p "XrefReferences" body))
        (should (string-match-p "ToolSearch" body)))))

  :doc "Imenu and Treesitter reminders are one-shot specialist reminders"
  (dolist (maker '(mevedel-reminders-make-imenu-available
                   mevedel-reminders-make-treesitter-available))
    (let ((r (funcall maker)))
      (should (eq 'one-shot (mevedel-reminder-interval r)))))

  :doc "Elisp introspection reminder excludes variable_value from routine guidance"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/el/" "/tmp/el/" "el"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-elisp-introspection-available)))
    (cl-letf (((symbol-function 'mevedel-reminders-specialist-capabilities)
               (lambda (_) '(:elisp-introspection t))))
      (let ((body (funcall (mevedel-reminder-content r) session)))
        (should (string-match-p "function_source" body))
        (should-not (string-match-p "variable_value`" body))))))


(mevedel-deftest mevedel-reminders-install-defaults
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "installs default session reminders"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws)))
    (mevedel-reminders-install-defaults session)
    (let ((types (mapcar #'mevedel-reminder-type
                         (mevedel-session-reminders session))))
      (should (memq 'pending-events types))
      (should (memq 'date-change types))
      (should (memq 'compaction-available types))
      (should (memq 'token-usage types))
      (should (memq 'agent-listing-delta types))
      (should (memq 'xref-available types))
      (should (memq 'imenu-available types))
      (should (memq 'treesitter-available types))
      (should (memq 'elisp-introspection-available types))
      (should (memq 'mode-constraints types))
      (should (memq 'plan-mode types))
      (should-not (memq 'diagnostics types))
      (should (memq 'edited-file types))
      (should (memq 'deferred-tools-roster types))
      (should (memq 'deferred-tools-expired types))
      (should (memq 'task-nudge types))
      (should (memq 'plan-reference types))))

  :doc "is idempotent - does not double-register"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws)))
    (mevedel-reminders-install-defaults session)
    (mevedel-reminders-install-defaults session)
    (let* ((types (mapcar #'mevedel-reminder-type
                          (mevedel-session-reminders session)))
           (pending-count (cl-count 'pending-events types))
           (date-count (cl-count 'date-change types))
           (compact-count (cl-count 'compaction-available types))
           (token-count (cl-count 'token-usage types))
           (agent-count (cl-count 'agent-listing-delta types))
           (xref-count (cl-count 'xref-available types))
           (imenu-count (cl-count 'imenu-available types))
           (treesitter-count (cl-count 'treesitter-available types))
           (elisp-count (cl-count 'elisp-introspection-available types))
           (mode-count (cl-count 'mode-constraints types))
           (plan-mode-count (cl-count 'plan-mode types))
           (edit-count (cl-count 'edited-file types))
           (roster-count (cl-count 'deferred-tools-roster types))
           (expired-count (cl-count 'deferred-tools-expired types))
           (nudge-count (cl-count 'task-nudge types))
           (plan-count (cl-count 'plan-reference types)))
      (should (= 1 pending-count))
      (should (= 1 date-count))
      (should (= 1 compact-count))
      (should (= 1 token-count))
      (should (= 1 agent-count))
      (should (= 1 xref-count))
      (should (= 1 imenu-count))
      (should (= 1 treesitter-count))
      (should (= 1 elisp-count))
      (should (= 1 mode-count))
      (should (= 1 plan-mode-count))
      (should (= 1 edit-count))
      (should (= 1 roster-count))
      (should (= 1 expired-count))
      (should (= 1 nudge-count))
      (should (= 1 plan-count)))))


(mevedel-deftest mevedel-agent-invocation-create/max-turns
  (:after-each (setq mevedel-agent--registry nil))
  ,test
  (test)

  :doc "prepends max-turns-warning reminder when agent has max-turns"
  (let* ((_ (mevedel-define-agent cap-agent
              :description "d"
              :tools nil
              :max-turns 20))
         (agent (mevedel-agent-get "cap-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (types (mapcar #'mevedel-reminder-type
                        (mevedel-agent-invocation-reminders inv))))
    (should (memq 'max-turns-warning types)))

  :doc "does not add max-turns-warning when agent has no max-turns"
  (let* ((_ (mevedel-define-agent nocap-agent
              :description "d"
              :tools nil))
         (agent (mevedel-agent-get "nocap-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (types (mapcar #'mevedel-reminder-type
                        (mevedel-agent-invocation-reminders inv))))
    (should-not (memq 'max-turns-warning types))))


(provide 'test-mevedel-reminders)

;;; test-mevedel-reminders.el ends here
