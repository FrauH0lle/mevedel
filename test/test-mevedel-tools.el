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


(provide 'test-mevedel-tools)
;;; test-mevedel-tools.el ends here
