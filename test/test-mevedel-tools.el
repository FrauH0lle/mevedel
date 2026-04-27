;;; test-mevedel-tools.el --- Tests for mevedel-tools.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'gptel-request)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-permissions)
(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-reminders)
(require 'mevedel-agents)
(require 'mevedel-tools)
(require 'mevedel-tool-task)
(require 'mevedel-tool-web)
(require 'mevedel-tool-introspect)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun mevedel-tools-test--make-session ()
  "Create a fresh session for tests."
  (let ((ws (mevedel-workspace-get-or-create
             'project "/tmp/mt/" "/tmp/mt/" "mt")))
    (mevedel-session-create "main" ws)))

(defun mevedel-tools-test--make-fake-gptel-tool (name)
  "Return a minimal `gptel-tool' with NAME."
  (gptel-make-tool
   :name name
   :function (lambda (&rest _) "")
   :description (format "Fake tool %s" name)
   :args nil
   :category "mevedel"))


;;
;;; Polymorphic deferred accessors

(mevedel-deftest mevedel-tools--ctx-deferred-set
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "reads from a `mevedel-session'"
  (let ((session (mevedel-tools-test--make-session)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "X") . "x")))
    (should (equal '((("mevedel" "X") . "x"))
                   (mevedel-tools--ctx-deferred-set session))))

  :doc "reads from a `mevedel-agent-invocation'"
  (let* ((_ (mevedel-define-agent a1 :description "a" :tools nil))
         (agent (mevedel-agent-get "a1"))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-deferred-set inv)
          '((("mevedel" "Y") . "y")))
    (should (equal '((("mevedel" "Y") . "y"))
                   (mevedel-tools--ctx-deferred-set inv)))
    (setq mevedel-agent--registry nil))

  :doc "setf writes to the correct underlying slot per struct"
  (let* ((_ (mevedel-define-agent a2 :description "a" :tools nil))
         (agent (mevedel-agent-get "a2"))
         (inv (mevedel-agent-invocation-create agent))
         (session (mevedel-tools-test--make-session)))
    (setf (mevedel-tools--ctx-deferred-set session) '((("x" "a") . "1")))
    (setf (mevedel-tools--ctx-deferred-set inv) '((("y" "b") . "2")))
    (should (equal '((("x" "a") . "1"))
                   (mevedel-session-deferred-set session)))
    (should (equal '((("y" "b") . "2"))
                   (mevedel-agent-invocation-deferred-set inv)))
    (setq mevedel-agent--registry nil)))


(mevedel-deftest mevedel-tools--ctx-deferred-injected
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "setf through accessor pushes onto session injected alist"
  (let ((session (mevedel-tools-test--make-session)))
    (push (cons "Foo" 5) (mevedel-tools--ctx-deferred-injected session))
    (should (equal '(("Foo" . 5))
                   (mevedel-session-deferred-injected session))))

  :doc "setf through accessor pushes onto invocation injected alist"
  (let* ((_ (mevedel-define-agent a3 :description "a" :tools nil))
         (agent (mevedel-agent-get "a3"))
         (inv (mevedel-agent-invocation-create agent)))
    (push (cons "Bar" 3) (mevedel-tools--ctx-deferred-injected inv))
    (should (equal '(("Bar" . 3))
                   (mevedel-agent-invocation-deferred-injected inv)))
    (setq mevedel-agent--registry nil)))


;;
;;; Deferred context resolution

(mevedel-deftest mevedel-tools--deferred-context-for
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "returns the invocation from FSM :context overlay"
  (let* ((_ (mevedel-define-agent ctx-a1 :description "a" :tools nil))
         (agent (mevedel-agent-get "ctx-a1"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-ov*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (fsm (gptel-make-fsm :info (list :context ov))))
          (overlay-put ov 'mevedel-agent-invocation inv)
          (should (eq inv (mevedel-tools--deferred-context-for fsm))))
      (kill-buffer ov-buf)))

  :doc "falls back to buffer-local session when no overlay is attached"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-buf*")))
    (unwind-protect
        (let ((fsm (with-current-buffer buf
                     (setq-local mevedel--session session)
                     (gptel-make-fsm :info (list :buffer buf)))))
          (should (eq session (mevedel-tools--deferred-context-for fsm))))
      (kill-buffer buf)))

  :doc "returns nil when FSM has neither overlay nor live buffer"
  (let ((fsm (gptel-make-fsm :info nil)))
    (should-not (mevedel-tools--deferred-context-for fsm))))


;;
;;; Deferred search

(mevedel-deftest mevedel-tools--search-deferred
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "matches on tool name substring, case-insensitive"
  (let ((session (mevedel-tools-test--make-session)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "XrefReferences") . "Find references to a symbol")
            (("mevedel" "Edit") . "Replace text in a file")))
    (let ((matches (mevedel-tools--search-deferred session "xref")))
      (should (= 1 (length matches)))
      (should (equal '("mevedel" "XrefReferences") (car (car matches))))))

  :doc "matches on description substring"
  (let ((session (mevedel-tools-test--make-session)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "XrefReferences") . "Find references to a symbol")
            (("mevedel" "Edit") . "Replace text in a file")))
    (let ((matches (mevedel-tools--search-deferred session "replace")))
      (should (= 1 (length matches)))
      (should (equal '("mevedel" "Edit") (car (car matches))))))

  :doc "OR semantics: any term may match"
  (let ((session (mevedel-tools-test--make-session)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "XrefReferences") . "Find references to a symbol")
            (("mevedel" "Edit") . "Replace text in a file")
            (("mevedel" "Bash") . "Run a shell command")))
    (let ((matches (mevedel-tools--search-deferred session "xref shell")))
      (should (= 2 (length matches)))))

  :doc "no match returns empty list"
  (let ((session (mevedel-tools-test--make-session)))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Edit") . "Replace text in a file")))
    (should (null (mevedel-tools--search-deferred session "nothing"))))

  :doc "works on an agent-invocation ctx via polymorphic accessor"
  (let* ((_ (mevedel-define-agent search-a :description "a" :tools nil))
         (agent (mevedel-agent-get "search-a"))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-deferred-set inv)
          '((("mevedel" "Edit") . "Replace text in a file")))
    (let ((matches (mevedel-tools--search-deferred inv "edit")))
      (should (= 1 (length matches))))
    (setq mevedel-agent--registry nil)))


;;
;;; ToolSearch tool entry point

(mevedel-deftest mevedel-tools--tool-search
  (:before-each
   (progn
     (mevedel-tool-clear-registry)
     ;; Register a fake tool so gptel-get-tool can resolve it.
     (let ((tool (mevedel-tool--create
                  :name "Edit"
                  :category "mevedel"
                  :gptel-tool (mevedel-tools-test--make-fake-gptel-tool "Edit"))))
       (mevedel-tool-register tool)
       ;; Also register under gptel's known-tools so `gptel-get-tool' works.
       (setf (alist-get "Edit"
                        (alist-get "mevedel" gptel--known-tools nil nil #'equal)
                        nil nil #'equal)
             (mevedel-tool-gptel-tool tool))))
   :after-each
   (progn
     (mevedel-tool-clear-registry)
     (setf (alist-get "mevedel" gptel--known-tools nil t #'equal) nil)
     (mevedel-workspace-clear-registry)))
  ,test
  (test)

  :doc "returns search text without queueing when load is nil"
  (let* ((session (mevedel-tools-test--make-session))
         (mevedel--session session)
         (result nil))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Edit") . "Replace text in a file")))
    (mevedel-tools--tool-search (lambda (s) (setq result s)) "edit" nil)
    (should (string-match-p "Found 1 tool" result))
    (should (string-match-p "Edit" result))
    (should (null (mevedel-session-deferred-pending session))))

  :doc "queues matched tools onto deferred-pending when load is t"
  (let* ((session (mevedel-tools-test--make-session))
         (mevedel--session session)
         (result nil))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Edit") . "Replace text in a file")))
    (mevedel-tools--tool-search (lambda (s) (setq result s)) "edit" t)
    (should (string-match-p "loaded and ready" result))
    (should (= 1 (length (mevedel-session-deferred-pending session))))
    (should (equal "Edit"
                   (gptel-tool-name
                    (car (mevedel-session-deferred-pending session))))))

  :doc "treats :json-false as nil load"
  (let* ((session (mevedel-tools-test--make-session))
         (mevedel--session session)
         (result nil))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Edit") . "Replace text in a file")))
    (mevedel-tools--tool-search (lambda (s) (setq result s)) "edit" :json-false)
    (should (string-match-p "ToolSearch again with load=true" result))
    (should (null (mevedel-session-deferred-pending session))))

  :doc "no matches returns the empty-result message"
  (let* ((session (mevedel-tools-test--make-session))
         (mevedel--session session)
         (result nil))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Edit") . "Replace text in a file")))
    (mevedel-tools--tool-search (lambda (s) (setq result s)) "bogus" t)
    (should (string-match-p "No matching tools found" result))
    (should (null (mevedel-session-deferred-pending session)))))


;;
;;; WAIT handler -- TTL lifecycle

(defun mevedel-tools-test--make-fsm-with-ctx (ctx)
  "Build a minimal FSM whose :context overlay carries CTX.
CTX may be a `mevedel-session' or `mevedel-agent-invocation'."
  (let* ((buf (generate-new-buffer " *mt-fsm*"))
         (ov (with-current-buffer buf
               (insert "x")
               (make-overlay (point-min) (point-max))))
         (tag (if (mevedel-agent-invocation-p ctx)
                  'mevedel-agent-invocation
                'mevedel-session)))
    (overlay-put ov tag ctx)
    ;; The FSM context overlay lookup only checks
    ;; `mevedel-agent-invocation'; for session tests we'll stash the
    ;; session on the buffer and pass the buffer via :buffer.
    (let ((info (if (mevedel-agent-invocation-p ctx)
                    (list :context ov
                          :buffer buf
                          :backend (gptel-make-openai "fake"
                                                      :models '("fake")
                                                      :key "fake")
                          :tools nil
                          :data (list :tools nil))
                  (with-current-buffer buf
                    (setq-local mevedel--session ctx))
                  (list :buffer buf
                        :backend (gptel-make-openai "fake"
                                                    :models '("fake")
                                                    :key "fake")
                        :tools nil
                        :data (list :tools nil)))))
      (cons buf (gptel-make-fsm :info info)))))

(mevedel-deftest mevedel-tools--handle-deferred-inject
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "injects pending tools with initial TTL and adds them to payload"
  (let* ((session (mevedel-tools-test--make-session))
         (buf+fsm (mevedel-tools-test--make-fsm-with-ctx session))
         (buf (car buf+fsm))
         (fsm (cdr buf+fsm))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          (setf (mevedel-session-deferred-pending session) (list tool))
          (mevedel-tools--handle-deferred-inject fsm)
          (let ((injected (mevedel-session-deferred-injected session))
                (active (plist-get (gptel-fsm-info fsm) :tools)))
            (should (equal (list (cons "Edit" mevedel-deferred-tool-ttl))
                           injected))
            (should (= 1 (length active)))
            (should (equal "Edit" (gptel-tool-name (car active))))
            ;; pending cleared after injection
            (should (null (mevedel-session-deferred-pending session)))))
      (kill-buffer buf)))

  :doc "resets TTL for tools that were used since the last turn"
  (let* ((session (mevedel-tools-test--make-session))
         (buf+fsm (mevedel-tools-test--make-fsm-with-ctx session))
         (buf (car buf+fsm))
         (fsm (cdr buf+fsm))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          ;; Pretend Edit was injected two turns ago with TTL=3 and was
          ;; used this turn.
          (plist-put (gptel-fsm-info fsm) :tools (list tool))
          (setf (mevedel-session-deferred-injected session) '(("Edit" . 3)))
          (setf (mevedel-session-deferred-used session) '("Edit"))
          (mevedel-tools--handle-deferred-inject fsm)
          (should (equal (list (cons "Edit" mevedel-deferred-tool-ttl))
                         (mevedel-session-deferred-injected session)))
          ;; used slot cleared
          (should (null (mevedel-session-deferred-used session))))
      (kill-buffer buf)))

  :doc "decrements TTL for tools that were not used this turn"
  (let* ((session (mevedel-tools-test--make-session))
         (buf+fsm (mevedel-tools-test--make-fsm-with-ctx session))
         (buf (car buf+fsm))
         (fsm (cdr buf+fsm))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          (plist-put (gptel-fsm-info fsm) :tools (list tool))
          (setf (mevedel-session-deferred-injected session) '(("Edit" . 3)))
          (mevedel-tools--handle-deferred-inject fsm)
          (should (equal '(("Edit" . 2))
                         (mevedel-session-deferred-injected session))))
      (kill-buffer buf)))

  :doc "expires tools at TTL 0, removes them from payload, records expired"
  (let* ((session (mevedel-tools-test--make-session))
         (buf+fsm (mevedel-tools-test--make-fsm-with-ctx session))
         (buf (car buf+fsm))
         (fsm (cdr buf+fsm))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          (plist-put (gptel-fsm-info fsm) :tools (list tool))
          (setf (mevedel-session-deferred-injected session) '(("Edit" . 1)))
          (mevedel-tools--handle-deferred-inject fsm)
          ;; removed from active payload
          (should (null (plist-get (gptel-fsm-info fsm) :tools)))
          ;; no longer tracked as injected
          (should (null (mevedel-session-deferred-injected session)))
          ;; recorded on expired slot for reminder
          (should (equal '("Edit")
                         (mevedel-session-deferred-expired session))))
      (kill-buffer buf)))

  :doc "is a no-op when FSM has no deferred context"
  (let ((fsm (gptel-make-fsm :info (list :tools nil :data (list :tools nil)))))
    ;; Should not error
    (mevedel-tools--handle-deferred-inject fsm)))


;;
;;; Messages — polymorphic accessor

(mevedel-deftest mevedel-tools--ctx-messages
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "reads and writes a session mailbox"
  (let ((session (mevedel-tools-test--make-session)))
    (should (null (mevedel-tools--ctx-messages session)))
    (mevedel-tools--ctx-push-message
     session '(:from "alpha" :body "hi"))
    (should (equal '((:from "alpha" :body "hi"))
                   (mevedel-tools--ctx-messages session)))
    (setf (mevedel-tools--ctx-messages session) nil)
    (should (null (mevedel-session-messages session))))

  :doc "reads and writes an agent-invocation mailbox"
  (let* ((_ (mevedel-define-agent msg-a :description "a" :tools nil))
         (agent (mevedel-agent-get "msg-a"))
         (inv (mevedel-agent-invocation-create agent)))
    (mevedel-tools--ctx-push-message inv '(:from "main" :body "ping"))
    (mevedel-tools--ctx-push-message inv '(:from "main" :body "pong"))
    ;; Messages are pushed onto the head for O(1) enqueue; the drain
    ;; reverses so arrival order is preserved at delivery time.
    (should (equal '((:from "main" :body "ping")
                     (:from "main" :body "pong"))
                   (nreverse (mevedel-agent-invocation-messages inv))))))


;;
;;; Messages — SendMessage tool handler

(mevedel-deftest mevedel-tools--send-message
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "delivers to main session via alias \"main\""
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-sm*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (let ((result (mevedel-tools--send-message
                         (list :to "main" :message "hello"))))
            (should (stringp result))
            (should (string-match-p "main" result)))
          (let ((inbox (mevedel-session-messages session)))
            (should (= 1 (length inbox)))
            (should (equal "hello" (plist-get (car inbox) :body)))
            (should (equal "main" (plist-get (car inbox) :from)))))
      (kill-buffer buf)))

  :doc "delivers to an agent invocation by exact agent-id"
  (let* ((_ (mevedel-define-agent sm-a :description "a" :tools nil))
         (agent (mevedel-agent-get "sm-a"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-sm-ov*"))
         (buf (generate-new-buffer " *mt-sm-chat*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (fsm (gptel-make-fsm :info (list :context ov))))
          (overlay-put ov 'mevedel-agent-invocation inv)
          (with-current-buffer buf
            (setq-local mevedel--session
                        (mevedel-tools-test--make-session))
            (setq-local mevedel-tools--agents-fsm
                        (list (cons "sm-a--abc" fsm)))
            (mevedel-tools--send-message
             (list :to "sm-a--abc" :message "run")))
          (should (equal "run"
                         (plist-get (car (mevedel-agent-invocation-messages inv))
                                    :body))))
      (kill-buffer ov-buf)
      (kill-buffer buf)))

  :doc "does not deliver to an agent invocation by agent-type prefix"
  (let* ((_ (mevedel-define-agent sm-b :description "a" :tools nil))
         (agent (mevedel-agent-get "sm-b"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-sm-ov2*"))
         (buf (generate-new-buffer " *mt-sm-chat2*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (fsm (gptel-make-fsm :info (list :context ov))))
          (overlay-put ov 'mevedel-agent-invocation inv)
          (with-current-buffer buf
            (setq-local mevedel--session
                        (mevedel-tools-test--make-session))
            (setq-local mevedel-tools--agents-fsm
                        (list (cons "sm-b--xyz" fsm)))
            (should-error
             (mevedel-tools--send-message
              (list :to "sm-b" :message "go"))))
          (should-not (mevedel-agent-invocation-messages inv)))
      (kill-buffer ov-buf)
      (kill-buffer buf)))

  :doc "errors on unknown recipient"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-sm-err*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (should-error (mevedel-tools--send-message
                         (list :to "ghost" :message "x"))))
      (kill-buffer buf)))

  :doc "requires non-empty to and message"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-sm-req*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (should-error (mevedel-tools--send-message
                         (list :message "hi")))
          (should-error (mevedel-tools--send-message
                         (list :to "main" :message ""))))
      (kill-buffer buf))))


;;
;;; Messages — WAIT handler delivery

(mevedel-deftest mevedel-tools--handle-message-inject
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "drains invocation mailbox and prepends a user-role block on first turn"
  (let* ((_ (mevedel-define-agent mi-a :description "a" :tools nil))
         (agent (mevedel-agent-get "mi-a"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-mi-ov*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (data (list :messages (vector (list :role "user"
                                                   :content "first"))))
               (fsm (gptel-make-fsm
                     :info (list :context ov
                                 :backend nil
                                 :data data))))
          (overlay-put ov 'mevedel-agent-invocation inv)
          (setf (mevedel-agent-invocation-messages inv)
                '((:from "worker" :body "one")
                  (:from "worker" :body "two")))
          (mevedel-tools--handle-message-inject fsm)
          (should (null (mevedel-agent-invocation-messages inv)))
          (let ((msgs (plist-get data :messages)))
            (should (equal 2 (length msgs)))
            ;; First turn (turn-count=0): mailbox block is injected
            ;; AHEAD of the user task prompt so the API request matches
            ;; the audit-log ordering.
            (let ((content (plist-get (aref msgs 0) :content)))
              (should (string-match-p "<agent-message from=\"worker\">" content))
              (should (string-match-p "one" content))
              (should (string-match-p "two" content)))
            (should (equal "first" (plist-get (aref msgs 1) :content)))))
      (kill-buffer ov-buf)))

  :doc "appends after the prior turn on subsequent WAIT cycles"
  (let* ((_ (mevedel-define-agent mi-b :description "b" :tools nil))
         (agent (mevedel-agent-get "mi-b"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-mi-ov2*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (data (list :messages
                           (vector (list :role "user" :content "task")
                                   (list :role "assistant"
                                         :content "thinking"))))
               (fsm (gptel-make-fsm
                     :info (list :context ov
                                 :backend nil
                                 :data data))))
          (overlay-put ov 'mevedel-agent-invocation inv)
          (setf (mevedel-agent-invocation-turn-count inv) 1)
          (setf (mevedel-agent-invocation-messages inv)
                '((:from "worker" :body "follow-up")))
          (mevedel-tools--handle-message-inject fsm)
          (let ((msgs (plist-get data :messages)))
            (should (equal 3 (length msgs)))
            ;; Subsequent turn: appended at the end.
            (let ((content (plist-get (aref msgs 2) :content)))
              (should (string-match-p "follow-up" content)))))
      (kill-buffer ov-buf)))

  :doc "is a no-op when the mailbox is empty"
  (let* ((_ (mevedel-define-agent mi-b :description "a" :tools nil))
         (agent (mevedel-agent-get "mi-b"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-mi-ov2*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (data (list :messages (vector)))
               (fsm (gptel-make-fsm
                     :info (list :context ov
                                 :backend nil
                                 :data data))))
          (overlay-put ov 'mevedel-agent-invocation inv)
          (mevedel-tools--handle-message-inject fsm)
          (should (equal 0 (length (plist-get data :messages)))))
      (kill-buffer ov-buf)))

  :doc "is a no-op when FSM has no context"
  (let* ((data (list :messages (vector (list :role "user" :content "x"))))
         (fsm (gptel-make-fsm :info (list :data data))))
    (mevedel-tools--handle-message-inject fsm)
    (should (equal 1 (length (plist-get data :messages))))))


;;
;;; Background agent spawning

(mevedel-deftest mevedel-tools--task
  (:before-each (progn (mevedel-tool-clear-registry)
                       ;; Built-in agents reference tool groups (read,
                       ;; code, web, ...) that `mevedel-agent-invocation-create'
                       ;; resolves eagerly, so every tool category must
                       ;; be registered.  The task now rejects unknown
                       ;; agent types up front, so agents must be
                       ;; re-registered per subtest as well.
                       (mevedel-tool-fs--register)
                       (mevedel-tool-code--register)
                       (mevedel-tool-exec--register)
                       (mevedel-tool-ui--register)
                       (mevedel-tool-task--register)
                       (mevedel-tool-web--register)
                       (mevedel-tool-introspect--register)
                       (load-file (locate-library "mevedel-agents")))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "background mode calls main-cb immediately with launch status"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bg*"))
         (captured-cb nil)
         (result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          ;; Mock gptel-agent--task: capture the callback and return a
          ;; fake FSM with an overlay context.
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-tools--task
               (lambda (resp &rest _) (setq result resp))
               "explore" "survey" "survey files"
               t))
            ;; main-cb should have been called synchronously
            (should (stringp result))
            (should (string-match-p "background" result))
            (should (string-match-p "explore" result))
            ;; FSM should be registered
            (should (= 1 (length mevedel-tools--agents-fsm)))))
      (kill-buffer buf)))

  :doc "background agent result is delivered to parent mailbox"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bg2*"))
         (captured-cb nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-tools--task
               #'ignore "explore" "survey" "survey files"
               t))
            ;; Simulate sub-agent completing
            (funcall captured-cb "The exploration found 5 issues.")
            ;; Result should be in the parent session's mailbox
            (let* ((msgs (mevedel-session-messages session))
                   (msg (car msgs)))
              (should (= 1 (length msgs)))
              (should (string-match-p "explore" (plist-get msg :from)))
              (should (string-match-p "5 issues" (plist-get msg :body))))))
      (kill-buffer buf)))

  :doc "foreground mode does not call main-cb until sub-agent finishes"
  (let* ((buf (generate-new-buffer " *mt-fg*"))
         (captured-cb nil)
         (result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (mevedel-tools--task
             (lambda (resp &rest _) (setq result resp))
             "explore" "survey" "survey files"
             nil)
            ;; main-cb should NOT have been called yet
            (should (null result))
            ;; Simulate sub-agent completing
            (funcall captured-cb "Done.")
            (should (equal "Done." result))))
      (kill-buffer buf)))

  :doc "Agent handler treats `:run_in_background :json-false' as foreground"
  (require 'mevedel-tool-ui)
  (let* ((buf (generate-new-buffer " *mt-agent-false*"))
         (captured-cb nil)
         (result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (mevedel-tool-ui--agent
             (lambda (resp &rest _) (setq result resp))
             '(:subagent_type "explore"
               :description "survey"
               :prompt "survey files"
               :run_in_background :json-false))
            ;; Foreground path: main-cb must not fire yet, and the
            ;; launch-status string must NOT be returned synchronously.
            (should (null result))
            (funcall captured-cb "Done.")
            (should (equal "Done." result))))
      (kill-buffer buf)))

  :doc "unknown agent type is rejected up front with an Error response"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-unknown*"))
         (result nil)
         (runner-called nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
                     (lambda (&rest _)
                       (setq runner-called t)
                       (error "runner must not be called for unknown agent"))))
            (mevedel-tools--task
             (lambda (resp &rest _) (setq result resp))
             "no-such-agent-type" "oops" "do nothing")
            (should (null runner-called))
            (should (stringp result))
            (should (string-match-p "Unknown agent type: no-such-agent-type"
                                    result))
            ;; No background tracking should have been created.
            (should (null (mevedel-session-background-agents session)))
            (should (null mevedel-tools--agents-fsm))))
      (kill-buffer buf))))


;;
;;; BWAIT (background wait) mechanism

(mevedel-deftest mevedel-tools--background-agents-pending-p
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "returns nil when no background agents are pending"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bwait1*"))
         (info (list :buffer buf :context nil)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (should-not (mevedel-tools--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "returns non-nil when session has background agents"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bwait2*"))
         (info (list :buffer buf :context nil)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (mevedel-tools--ctx-push-background-agent session "explore--abc123")
          (should (mevedel-tools--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "returns non-nil when invocation has background agents"
  (require 'mevedel-tool-ui)
  (let* ((buf (generate-new-buffer " *mt-bwait3*"))
         (inv (mevedel-agent-invocation--create
               :agent (mevedel-agent--create :name "coordinator")))
         (ov (with-current-buffer buf
               (insert "x")
               (let ((o (make-overlay (point-min) (point-max))))
                 (overlay-put o 'mevedel-agent-invocation inv)
                 o)))
         (info (list :buffer buf :context ov)))
    (unwind-protect
        (progn
          (mevedel-tools--ctx-push-background-agent inv "explore--abc123")
          (should (mevedel-tools--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "returns non-nil when mailbox has messages but no background agents"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bwait4*"))
         (info (list :buffer buf :context nil)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (mevedel-tools--ctx-push-message
           session '(:from "explore--x" :body "done"))
          (should (mevedel-tools--background-agents-pending-p info)))
      (kill-buffer buf))))

(mevedel-deftest mevedel-preset--inject-bwait-transitions
  ()
  ,test
  (test)

  :doc "inserts BWAIT before DONE in TYPE and TRET transitions"
  (require 'mevedel-presets)
  (let* ((table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((tool-p . TPRE) (t . DONE)))
                  (TRET . ((error-p . ERRS) (result-p . WAIT) (t . DONE)))))
         (result (mevedel-preset--inject-bwait-transitions table)))
    ;; TYPE should have BWAIT before DONE
    (let ((type-transitions (cdr (assq 'TYPE result))))
      (should (= 3 (length type-transitions)))
      (should (eq 'TPRE (cdar type-transitions)))
      (should (eq 'BWAIT (cdadr type-transitions)))
      (should (eq 'DONE (cdaddr type-transitions))))
    ;; TRET should have BWAIT before DONE
    (let ((tret-transitions (cdr (assq 'TRET result))))
      (should (= 4 (length tret-transitions)))
      (should (eq 'BWAIT (cdr (nth 2 tret-transitions))))
      (should (eq 'DONE (cdr (nth 3 tret-transitions)))))
    ;; BWAIT state should exist with no outgoing transitions
    (let ((bwait-entry (assq 'BWAIT result)))
      (should bwait-entry)
      (should (null (cdr bwait-entry)))))

  :doc "does not modify unrelated states"
  (require 'mevedel-presets)
  (let* ((table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((t . DONE)))
                  (TRET . ((t . DONE)))))
         (result (mevedel-preset--inject-bwait-transitions table)))
    (should (equal '((t . WAIT)) (cdr (assq 'INIT result))))
    (should (equal '((t . TYPE)) (cdr (assq 'WAIT result)))))

  :doc "re-injecting an already-injected table is a no-op (no duplicate predicates)"
  (require 'mevedel-presets)
  (let* ((mevedel-tools--bwait-table-cache nil)
         (table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((tool-p . TPRE) (t . DONE)))
                  (TRET . ((error-p . ERRS) (result-p . WAIT) (t . DONE)))))
         (once (mevedel-preset--inject-bwait-transitions table))
         (twice (mevedel-preset--inject-bwait-transitions once)))
    ;; Re-injection returns the same object unchanged -- the injector
    ;; bails out when BWAIT is already present.
    (should (eq once twice))
    (let ((type-transitions (cdr (assq 'TYPE twice))))
      (should (= 3 (length type-transitions)))
      (should (eq 'BWAIT (cdadr type-transitions))))))

(mevedel-deftest mevedel-tools--task-bwait
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-tool-fs--register)
                       (mevedel-tool-code--register)
                       (mevedel-tool-exec--register)
                       (mevedel-tool-ui--register)
                       (mevedel-tool-task--register)
                       (mevedel-tool-web--register)
                       (mevedel-tool-introspect--register)
                       (load-file (locate-library "mevedel-agents")))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "background spawn tracks agent on parent context"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bwait-track*"))
         (captured-cb nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-tools--task
               #'ignore "explore" "survey" "survey files"
               t))
            ;; Agent ID should be tracked on the session.
            (should (= 1 (length (mevedel-session-background-agents session))))))
      (kill-buffer buf)))

  :doc "background completion removes agent from tracking"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bwait-rm*"))
         (captured-cb nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-tools--task
               #'ignore "explore" "survey" "survey files"
               t))
            ;; Complete the background agent.
            (funcall captured-cb "Done.")
            ;; Agent should be removed from tracking.
            (should (null (mevedel-session-background-agents session)))))
      (kill-buffer buf)))

  :doc "background completion resumes parent FSM from BWAIT"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bwait-resume*"))
         (captured-cb nil)
         (resumed nil)
         ;; A WAIT handler that records the transition instead of
         ;; firing an HTTP request.
         (parent-fsm (gptel-make-fsm
                      :table `((BWAIT) (WAIT . ((t . TYPE))))
                      :handlers `((WAIT ,(lambda (_fsm) (setq resumed t))))
                      :info (list :buffer buf))))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            ;; Simulate parent FSM dispatching a background agent.
            (let ((mevedel-tools--current-fsm parent-fsm))
              (mevedel-tools--task
               #'ignore "explore" "survey" "survey files"
               t))
            ;; Park the parent FSM in BWAIT.
            (setf (gptel-fsm-state parent-fsm) 'BWAIT)
            ;; Complete the background agent.
            (funcall captured-cb "Done.")
            ;; Parent FSM should have been resumed to WAIT.
            (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
            (should resumed)))
      (kill-buffer buf))))


(mevedel-deftest mevedel-tools--task-foreground-stash
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-tool-fs--register)
                       (mevedel-tool-code--register)
                       (mevedel-tool-exec--register)
                       (mevedel-tool-ui--register)
                       (mevedel-tool-task--register)
                       (mevedel-tool-web--register)
                       (load-file (locate-library "mevedel-agents")))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (mevedel-tool-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "foreground callback defers main-cb while background agents are pending"
  (let* ((mevedel-session-persistence nil)
         (session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-stash1*"))
         (coordinator-cb nil)
         (result nil)
         (call-count 0)
         (inv nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          ;; Step 1: Spawn the coordinator (foreground).
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-coordinator-fsm
                      (gptel-make-fsm
                       :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &optional invocation _agent-buffer)
                        (setq coordinator-cb cb
                              inv invocation)
                        (when inv
                          (overlay-put ov 'mevedel-agent-invocation inv))
                        fake-coordinator-fsm)))
            (mevedel-tools--task
             (lambda (resp &rest _)
               (cl-incf call-count)
               (setq result resp))
             "coordinator" "orchestrate" "do stuff")
            (should inv)
            ;; Step 2: Simulate the coordinator spawning a background
            ;; agent.  Directly push onto the invocation's
            ;; background-agents.
            (mevedel-tools--ctx-push-background-agent inv "explore--fake")
            ;; Step 3: The coordinator's LLM returns text-only while
            ;; children still pending -- main-cb must not fire.
            (funcall coordinator-cb "Waiting for results...")
            (should (null result))
            (should (zerop call-count))
            ;; Step 4: Child finishes, then coordinator fires final.
            (mevedel-tools--ctx-remove-background-agent inv "explore--fake")
            (funcall coordinator-cb "Final summary with results.")
            (should (stringp result))
            (should (string-match-p "Final summary" result))
            (should (= 1 call-count))
            ;; Step 5: A late duplicate 't' event must NOT double-fire.
            (funcall coordinator-cb "Redundant late response.")
            (should (= 1 call-count))
            (should (string-match-p "Final summary" result))))
      (kill-buffer buf)))

  :doc "foreground callback defers main-cb while mailbox holds pending results (race)"
  (let* ((mevedel-session-persistence nil)
         (session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-stash2*"))
         (coordinator-cb nil)
         (result nil)
         (call-count 0)
         (inv nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-coordinator-fsm
                      (gptel-make-fsm
                       :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &optional invocation _agent-buffer)
                        (setq coordinator-cb cb
                              inv invocation)
                        (when inv
                          (overlay-put ov 'mevedel-agent-invocation inv))
                        fake-coordinator-fsm)))
            (mevedel-tools--task
             (lambda (resp &rest _)
               (cl-incf call-count)
               (setq result resp))
             "coordinator" "orchestrate" "do stuff")
            (should inv)
            ;; Race: the background child finished BEFORE the parent
            ;; produced its text-only turn, so by callback time
            ;; `background-agents' is empty but `messages' still holds
            ;; an undelivered result.
            (mevedel-tools--ctx-push-message
             inv (list :from "explore--fake" :body "done"))
            (funcall coordinator-cb "Preliminary handoff.")
            ;; Must NOT fire yet -- the mailbox still has to drain.
            (should (null result))
            (should (zerop call-count))
            ;; Mailbox drains (simulating WAIT); coordinator fires final.
            (setf (mevedel-agent-invocation-messages inv) nil)
            (funcall coordinator-cb "Final summary with results.")
            (should (= 1 call-count))
            (should (string-match-p "Final summary" result))))
      (kill-buffer buf)))

  :doc "foreground callback bypasses gate on error/abort responses"
  (let* ((mevedel-session-persistence nil)
         (session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-stash3*"))
         (coordinator-cb nil)
         (result nil)
         (inv nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-coordinator-fsm
                      (gptel-make-fsm
                       :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &optional invocation _agent-buffer)
                        (setq coordinator-cb cb
                              inv invocation)
                        (when inv
                          (overlay-put ov 'mevedel-agent-invocation inv))
                        fake-coordinator-fsm)))
            (mevedel-tools--task
             (lambda (resp &rest _) (setq result resp))
             "coordinator" "orchestrate" "do stuff")
            (mevedel-tools--ctx-push-background-agent inv "explore--fake")
            ;; Error response must forward immediately so the parent
            ;; tool call doesn't hang on a dead child.
            (funcall coordinator-cb "Error: Task aborted by the user.")
            (should (stringp result))
            (should (string-match-p "Error:" result))))
      (kill-buffer buf))))



;;
;;; Watchdog, bg-callback hardening, prune

(mevedel-deftest mevedel-tools--bwait-watchdog-expire
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-tool-fs--register)
                       (mevedel-tool-code--register)
                       (mevedel-tool-exec--register)
                       (mevedel-tool-ui--register)
                       (mevedel-tool-task--register)
                       (mevedel-tool-web--register)
                       (mevedel-tool-introspect--register)
                       (load-file (locate-library "mevedel-agents")))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "empty mailbox after cleanup transitions parent to DONE"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-done*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (insert "aa")
          (let* ((ov-a (make-overlay (point-min) (1+ (point-min))))
                 (ov-b (make-overlay (1+ (point-min)) (point-max)))
                 (child-a (gptel-make-fsm :info (list :context ov-a)
                                          :handlers nil :state 'TOOL))
                 (child-b (gptel-make-fsm :info (list :context ov-b)
                                          :handlers nil :state 'TOOL))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explore--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child-a)
            (setf (alist-get "explore--B" mevedel-tools--agents-fsm nil nil #'equal)
                  child-b)
            (setf (mevedel-session-background-agents session)
                  '("explore--A" "explore--B"))
            (setf (mevedel-session-messages session) nil)
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'DONE (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (null mevedel-tools--agents-fsm))
            (should-not (overlay-buffer ov-a))
            (should-not (overlay-buffer ov-b))))
      (kill-buffer buf)))

  :doc "non-empty mailbox transitions parent to WAIT so drain fires"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-wait*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (setf (mevedel-session-background-agents session) '("explore--X"))
          (setf (mevedel-session-messages session)
                '((:from "explore--finished" :body "done")))
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))))
      (kill-buffer buf)))

  :doc "no-op when FSM has already left BWAIT"
  (let ((parent (gptel-make-fsm :handlers nil :state 'DONE)))
    (mevedel-tools--bwait-watchdog-expire parent)
    (should (eq 'DONE (gptel-fsm-state parent)))))


(mevedel-deftest mevedel-tools--task-bg-callback-hardening
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-tool-fs--register)
                       (mevedel-tool-code--register)
                       (mevedel-tool-exec--register)
                       (mevedel-tool-ui--register)
                       (mevedel-tool-task--register)
                       (mevedel-tool-web--register)
                       (mevedel-tool-introspect--register)
                       (load-file (locate-library "mevedel-agents")))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "bg callback removes agent from tracking even when push-message throws"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bg-harden*"))
         (captured-cb nil)
         (push-called 0)
         (remove-called 0))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-tools--task #'ignore "explore" "survey" "go" t))
            (should (= 1 (length (mevedel-session-background-agents session))))
            ;; Break push-message so the bg callback's push branch raises;
            ;; remove-background-agent MUST still run so the parent isn't
            ;; stranded in BWAIT.
            (cl-letf (((symbol-function 'mevedel-tools--ctx-push-message)
                       (lambda (&rest _)
                         (cl-incf push-called)
                         (error "simulated push failure")))
                      ((symbol-function 'mevedel-tools--ctx-remove-background-agent)
                       (lambda (_ctx _id)
                         (cl-incf remove-called))))
              (funcall captured-cb "child result"))
            (should (= 1 push-called))
            (should (= 1 remove-called))))
      (kill-buffer buf))))


(mevedel-deftest mevedel-tools--prune-stale-agents-fsm
  ()
  ,test
  (test)

  :doc "prunes terminal-state FSMs"
  (let ((buf (generate-new-buffer " *mt-prune-done*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((ov-a (make-overlay (point-min) (point-min)))
                 (ov-b (make-overlay (point-min) (point-min)))
                 (alive (gptel-make-fsm :info (list :context ov-a)
                                        :handlers nil :state 'WAIT))
                 (done  (gptel-make-fsm :info (list :context ov-b)
                                        :handlers nil :state 'DONE)))
            (setf (alist-get "alive" mevedel-tools--agents-fsm nil nil #'equal) alive)
            (setf (alist-get "done"  mevedel-tools--agents-fsm nil nil #'equal) done)
            (mevedel-tools--prune-stale-agents-fsm)
            (should (assoc "alive" mevedel-tools--agents-fsm))
            (should-not (assoc "done" mevedel-tools--agents-fsm))))
      (kill-buffer buf)))

  :doc "prunes TOOL-state FSMs whose context overlay was deleted"
  (let ((buf (generate-new-buffer " *mt-prune-tool*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((good-ov (make-overlay (point-min) (point-min)))
                 (bad-ov  (make-overlay (point-min) (point-min)))
                 (good (gptel-make-fsm :info (list :context good-ov)
                                       :handlers nil :state 'TOOL))
                 (bad  (gptel-make-fsm :info (list :context bad-ov)
                                       :handlers nil :state 'TOOL)))
            (setf (alist-get "good" mevedel-tools--agents-fsm nil nil #'equal) good)
            (setf (alist-get "bad"  mevedel-tools--agents-fsm nil nil #'equal) bad)
            ;; Simulate watchdog scrubbing the stranded child's overlay.
            (delete-overlay bad-ov)
            (mevedel-tools--prune-stale-agents-fsm)
            (should (assoc "good" mevedel-tools--agents-fsm))
            (should-not (assoc "bad" mevedel-tools--agents-fsm))))
      (kill-buffer buf))))


(provide 'test-mevedel-tools)
;;; test-mevedel-tools.el ends here
