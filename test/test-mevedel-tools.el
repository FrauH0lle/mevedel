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
(require 'mevedel-agent-control)
(require 'mevedel-agent-exec)
(require 'mevedel-session-persistence)
(require 'mevedel-transcript-audit)
(require 'mevedel-tools)
(require 'mevedel-tool-task)
(require 'mevedel-tool-ui)
(require 'mevedel-tool-web)
(require 'mevedel-tool-introspect)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(declare-function gptel-make-openai "ext:gptel-openai" (name &rest args))

(defvar mevedel-tools-test--agent-registry
  (copy-sequence mevedel-agent--registry)
  "Stable built-in agent registry for isolated tool tests.")


;;
;;; Helpers

(defun mevedel-tools-test--make-session ()
  "Create a fresh tools-test session."
  (let ((ws (mevedel-workspace-get-or-create
             'project "/tmp/mt/" "/tmp/mt/" "mt")))
    (mevedel-session-create "main" ws)))

(defun mevedel-tools-test--make-fake-gptel-tool (name &optional category)
  "Return a minimal `gptel-tool' with NAME and CATEGORY."
  (gptel-make-tool
   :name name
   :function (lambda (&rest _) "")
   :description (format "Fake tool %s" name)
   :args nil
   :category (or category "mevedel")))

(defun mevedel-tools-test--register-agent-tools ()
  "Register the built-in tool surface needed by bundled agents."
  (mevedel-tools-register))

;;
;;; Tool registration

(mevedel-deftest mevedel-tools-register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers built-in and skill tools"
  (progn
    (mevedel-tools-register)
    (dolist (name '("Read" "Bash" "Skill" "ListSkills"))
      (should (mevedel-tool-get name "mevedel")))))


;;
;;; Polymorphic deferred accessors

(mevedel-deftest mevedel-tools-active-count ()
  ,test
  (test)

  :doc "returns zero when no tools are active"
  (with-temp-buffer
    (should (= 0 (mevedel-tools-active-count))))

  :doc "counts active gptel tools in the target buffer"
  (let ((buf (generate-new-buffer " *mev-tools-count*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-tools
                        (list (mevedel-tools-test--make-fake-gptel-tool "Read")
                              (mevedel-tools-test--make-fake-gptel-tool "Edit"))))
          (should (= 2 (mevedel-tools-active-count buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

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

  :doc "returns the invocation from FSM info"
  (let* ((_ (mevedel-define-agent ctx-a1 :description "a" :tools nil))
         (agent (mevedel-agent-get "ctx-a1"))
         (inv (mevedel-agent-invocation-create agent))
         (fsm (gptel-make-fsm
               :info (list :mevedel-agent-invocation inv))))
    (should (eq inv (mevedel-tools--deferred-context-for fsm))))

  :doc "falls back to buffer-local session when no overlay is attached"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-buf*")))
    (unwind-protect
        (let ((fsm (with-current-buffer buf
                     (setq-local mevedel--session session)
                     (gptel-make-fsm :info (list :buffer buf)))))
          (should (eq session (mevedel-tools--deferred-context-for fsm))))
      (kill-buffer buf)))

  :doc "agent buffers prefer their invocation over inherited session"
  (let* ((_ (mevedel-define-agent ctx-a2 :description "a" :tools nil))
         (agent (mevedel-agent-get "ctx-a2"))
         (inv (mevedel-agent-invocation-create agent))
         (session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-agent-buf*")))
    (unwind-protect
        (let ((fsm (with-current-buffer buf
                     (setq-local mevedel--session session)
                     (setq-local mevedel--agent-invocation inv)
                     (gptel-make-fsm :info (list :buffer buf)))))
          (should (eq inv (mevedel-tools--deferred-context-for fsm))))
      (kill-buffer buf)))

  :doc "returns nil when FSM has neither overlay nor live buffer"
  (let ((fsm (gptel-make-fsm :info nil)))
    (should-not (mevedel-tools--deferred-context-for fsm))))

(mevedel-deftest mevedel-tools--current-deferred-context
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "agent buffers fall back to their invocation before session"
  (let* ((_ (mevedel-define-agent ctx-cur-a1 :description "a" :tools nil))
         (agent (mevedel-agent-get "ctx-cur-a1"))
         (inv (mevedel-agent-invocation-create agent))
         (session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-agent-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel--agent-invocation inv)
          (let ((mevedel-tools--current-fsm nil))
            (should (eq inv (mevedel-tools--current-deferred-context)))))
      (kill-buffer buf))))


;;
;;; Unknown tool-call recovery

(defun mevedel-tools-test--make-tool-use-fsm
    (tool-use &optional tools session)
  "Return a plist carrying an FSM for TOOL-USE with active TOOLS.
When SESSION is non-nil, bind it in the FSM's buffer.  The returned
plist has :buffer, :fsm, and :transitions keys.  :transitions is a
function returning the states entered by test handlers."
  (let ((buf (generate-new-buffer " *mt-tool-use*"))
        (transitions nil))
    (when session
      (with-current-buffer buf
        (setq-local mevedel--session session)))
    (let ((fsm (gptel-make-fsm
                :state 'TOOL
                :table '((TOOL . ((t . TRET)))
                         (TRET . nil))
                :handlers `((TRET ,(lambda (_fsm)
                                      (push 'TRET transitions))))
                :info (list :buffer buf
                            :backend 'mevedel-test-backend
                            :tools tools
                            :tool-use tool-use))))
      (list :buffer buf
            :fsm fsm
            :transitions (lambda () transitions)))))

(defun mevedel-tools-test--tool-use-result (fsm name)
  "Return FSM's tool-use result for tool NAME."
  (plist-get
   (cl-find-if (lambda (tool-call)
                 (equal name (plist-get tool-call :name)))
               (plist-get (gptel-fsm-info fsm) :tool-use))
   :result))

(defun mevedel-tools-test--tool-result-string (fsm name)
  "Return FSM's display tool-result string for tool NAME."
  (nth 2
       (cl-find-if (lambda (entry)
                     (equal name (gptel-tool-name (car entry))))
                   (plist-get (gptel-fsm-info fsm) :tool-result))))

(mevedel-deftest mevedel-tools--settle-unknown-tool-calls
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "unknown deferred tool returns ToolSearch guidance and transitions"
  (let* ((session (mevedel-tools-test--make-session))
         (tool-use (list (list :name "Imenu"
                               :args '(:file_path "mevedel-tools.el")
                               :id "call_1")))
         (fixture (mevedel-tools-test--make-tool-use-fsm
                   tool-use nil session))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (transitions (plist-get fixture :transitions)))
    (unwind-protect
        (let ((inhibit-message t)
              (gptel-confirm-tool-calls nil))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Imenu") . "File outline")))
          (gptel--handle-tool-use fsm)
          (let ((result (mevedel-tools-test--tool-use-result fsm "Imenu")))
            (should (string-match-p "Tool Imenu is not currently loaded" result))
            (should (string-match-p
                     "ToolSearch(query=\\\"Imenu\\\", load=true)"
                     result))
            (should (equal result
                           (mevedel-tools-test--tool-result-string
                            fsm "Imenu"))))
          (should (equal '(TRET) (funcall transitions))))
      (kill-buffer buf)))

  :doc "unknown expired deferred tool returns ToolSearch guidance"
  (let* ((session (mevedel-tools-test--make-session))
         (tool-use (list (list :name "ExpiredImenu"
                               :args nil
                               :id "call_expired")))
         (fixture (mevedel-tools-test--make-tool-use-fsm
                   tool-use nil session))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (transitions (plist-get fixture :transitions)))
    (unwind-protect
        (let ((inhibit-message t)
              (gptel-confirm-tool-calls nil))
          (setf (mevedel-session-deferred-expired session) '("ExpiredImenu"))
          (gptel--handle-tool-use fsm)
          (let ((result (mevedel-tools-test--tool-use-result
                         fsm "ExpiredImenu")))
            (should (string-match-p
                     "Tool ExpiredImenu is not currently loaded"
                     result))
            (should (string-match-p
                     "ToolSearch(query=\"ExpiredImenu\", load=true)"
                     result)))
          (should (equal '(TRET) (funcall transitions))))
      (kill-buffer buf)))

  :doc "unknown deferred tool is settled before upstream generic fallback"
  (let* ((session (mevedel-tools-test--make-session))
         (tool-use (list (list :name "Imenu"
                               :args '(:file_path "mevedel-tools.el")
                               :id "call_before")))
         (fixture (mevedel-tools-test--make-tool-use-fsm
                   tool-use nil session))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (orig-result nil))
    (unwind-protect
        (let ((inhibit-message t))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Imenu") . "File outline")))
          (mevedel-tools--handle-tool-use-advice
           (lambda (_fsm)
             (setq orig-result
                   (mevedel-tools-test--tool-use-result fsm "Imenu")))
           fsm)
          (should (string-match-p "Tool Imenu is not currently loaded"
                                  orig-result))
          (should (string-match-p
                   "ToolSearch(query=\\\"Imenu\\\", load=true)"
                   orig-result)))
      (kill-buffer buf)))

  :doc "truly unknown tool returns generic guidance and transitions"
  (let* ((tool-use (list (list :name "NoSuchTool"
                               :args nil
                               :id "call_2")))
         (fixture (mevedel-tools-test--make-tool-use-fsm tool-use))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (transitions (plist-get fixture :transitions)))
    (unwind-protect
        (let ((inhibit-message t)
              (gptel-confirm-tool-calls nil))
          (gptel--handle-tool-use fsm)
          (let ((result (mevedel-tools-test--tool-use-result
                         fsm "NoSuchTool")))
            (should (string-match-p "Unknown tool NoSuchTool" result))
            (should (string-match-p "ToolSearch" result))
            (should (equal result
                           (mevedel-tools-test--tool-result-string
                            fsm "NoSuchTool"))))
          (should (equal '(TRET) (funcall transitions))))
      (kill-buffer buf)))

  :doc "gptel structured-output pseudo tool is not intercepted"
  (let* ((tool-use (list (list :name gptel--ersatz-json-tool
                               :args '(:value 1)
                               :id "call_json")))
         (fixture (mevedel-tools-test--make-tool-use-fsm tool-use))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (transitions (plist-get fixture :transitions)))
    (unwind-protect
        (let ((inhibit-message t)
              (gptel-confirm-tool-calls nil))
          (should (boundp 'gptel--ersatz-json-tool))
          (mevedel-tools--settle-unknown-tool-calls fsm)
          (should-not (mevedel-tools-test--tool-use-result
                       fsm gptel--ersatz-json-tool))
          (should-not (plist-get (gptel-fsm-info fsm) :tool-result))
          (should-not (funcall transitions)))
      (kill-buffer buf)))

  :doc "all-unknown multi-call list transitions after every result"
  (let* ((tool-use (list (list :name "MissingOne"
                               :args nil
                               :id "call_missing_1")
                         (list :name "MissingTwo"
                               :args nil
                               :id "call_missing_2")))
         (fixture (mevedel-tools-test--make-tool-use-fsm tool-use))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (transitions (plist-get fixture :transitions)))
    (unwind-protect
        (let ((inhibit-message t)
              (gptel-confirm-tool-calls nil))
          (gptel--handle-tool-use fsm)
          (should (string-match-p
                   "Unknown tool MissingOne"
                   (mevedel-tools-test--tool-use-result fsm "MissingOne")))
          (should (string-match-p
                   "Unknown tool MissingTwo"
                   (mevedel-tools-test--tool-use-result fsm "MissingTwo")))
          (should (equal '(TRET) (funcall transitions))))
      (kill-buffer buf)))

  :doc "mixed known sync and unknown calls transition after both results"
  (let* ((known (gptel-make-tool
                 :name "Known"
                 :function (lambda () "known ok")
                 :description "Known test tool"
                 :args nil
                 :category "mevedel"))
         (tool-use (list (list :name "Known" :args nil :id "call_known")
                         (list :name "Missing" :args nil :id "call_missing")))
         (fixture (mevedel-tools-test--make-tool-use-fsm
                   tool-use (list known)))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (transitions (plist-get fixture :transitions)))
    (unwind-protect
        (let ((inhibit-message t)
              (gptel-confirm-tool-calls nil))
          (gptel--handle-tool-use fsm)
          (should (equal "known ok"
                         (mevedel-tools-test--tool-use-result fsm "Known")))
          (should (string-match-p
                   "Unknown tool Missing"
                   (mevedel-tools-test--tool-use-result fsm "Missing")))
          (should (equal '(TRET) (funcall transitions))))
      (kill-buffer buf)))

  :doc "mixed known async and unknown calls wait for async result"
  (let* (async-callback
         (known (gptel-make-tool
                 :name "AsyncKnown"
                 :function (lambda (callback)
                             (setq async-callback callback))
                 :description "Async known test tool"
                 :args nil
                 :async t
                 :category "mevedel"))
         (tool-use (list (list :name "AsyncKnown"
                               :args nil
                               :id "call_async")
                         (list :name "MissingAsyncPeer"
                               :args nil
                               :id "call_missing_async_peer")))
         (fixture (mevedel-tools-test--make-tool-use-fsm
                   tool-use (list known)))
         (buf (plist-get fixture :buffer))
         (fsm (plist-get fixture :fsm))
         (transitions (plist-get fixture :transitions)))
    (unwind-protect
        (let ((inhibit-message t)
              (gptel-confirm-tool-calls nil))
          (gptel--handle-tool-use fsm)
          (should async-callback)
          (should (string-match-p
                   "Unknown tool MissingAsyncPeer"
                   (mevedel-tools-test--tool-use-result
                    fsm "MissingAsyncPeer")))
          (should-not (funcall transitions))
          (funcall async-callback "async ok")
          (should (equal "async ok"
                         (mevedel-tools-test--tool-use-result
                          fsm "AsyncKnown")))
          (should (equal '(TRET) (funcall transitions))))
      (kill-buffer buf))))


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
             (mevedel-tool-gptel-tool tool)))
     (let ((tool (mevedel-tool--create
                  :name "function_source"
                  :category "mevedel-introspection"
                  :summary "Read source"
                  :groups '(elisp)
                  :gptel-tool (mevedel-tools-test--make-fake-gptel-tool
                               "function_source"
                               "mevedel-introspection"))))
       (mevedel-tool-register tool)
       (setf (alist-get
              "function_source"
              (alist-get "mevedel-introspection"
                         gptel--known-tools nil nil #'equal)
              nil nil #'equal)
             (mevedel-tool-gptel-tool tool))))
   :after-each
   (progn
     (mevedel-tool-clear-registry)
     (setf (alist-get "mevedel" gptel--known-tools nil t #'equal) nil)
     (setf (alist-get "mevedel-introspection" gptel--known-tools nil t #'equal)
           nil)
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
    (should (string-match-p "exact tool name" result))
    (should (null (mevedel-session-deferred-pending session))))

  :doc "queues matched tools onto deferred-pending when load is t"
  (let* ((session (mevedel-tools-test--make-session))
         (mevedel--session session)
         (result nil))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "Edit") . "Replace text in a file")))
    (mevedel-tools--tool-search (lambda (s) (setq result s)) "edit" t)
    (should (string-match-p "available now" result))
    (should (= 1 (length (mevedel-session-deferred-pending session))))
    (should (equal "Edit"
                   (gptel-tool-name
                    (car (mevedel-session-deferred-pending session))))))

  :doc "loads deferred tools when the query matches their registered group"
  (let* ((session (mevedel-tools-test--make-session))
         (mevedel--session session)
         (result nil))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel-introspection" "function_source") . "Read source")))
    (mevedel-tools--tool-search (lambda (s) (setq result s)) "elisp" t)
    (should (string-match-p "available now" result))
    (should (= 1 (length (mevedel-session-deferred-pending session))))
    (should (equal "function_source"
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

  :doc "includes usage hints for known specialist tools"
  (let* ((session (mevedel-tools-test--make-session))
         (mevedel--session session)
         (result nil))
    (setf (mevedel-session-deferred-set session)
          '((("mevedel" "XrefReferences") . "Find references")))
    (mevedel-tools--tool-search (lambda (s) (setq result s)) "xref" nil)
    (should (string-match-p "XrefReferences(identifier, file_path)" result)))

  :doc "ToolSearch load argument says loaded tools are available now"
  (progn
    (mevedel-tool-ui--register)
    (let* ((tool (mevedel-tool-get "ToolSearch" "mevedel"))
           (load-arg (assoc 'load (mevedel-tool-args tool)))
           (description (nth 3 load-arg)))
      (should tool)
      (should (string-match-p "available now" description))
      (should (string-match-p "next tool call" description))
      (should-not (string-match-p "next model turn" description))))

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
  "Build a minimal FSM whose info carries CTX.
CTX may be a `mevedel-session' or `mevedel-agent-invocation'."
  (let* ((buf (generate-new-buffer " *mt-fsm*"))
         (_tag (if (mevedel-agent-invocation-p ctx)
                   'mevedel-agent-invocation
                 'mevedel-session)))
    ;; Invocation tests use the FSM info key directly; for session tests
    ;; stash the session on the buffer and pass the buffer via :buffer.
    (let ((info (if (mevedel-agent-invocation-p ctx)
                    (list :mevedel-agent-invocation ctx
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




;;
;;; Messages — SendMessage tool handler

;;; Messages — WAIT handler delivery

(mevedel-deftest mevedel-tools--message-delivery-block
  (:doc "formats canonical retained-agent mailbox records")
  ,test
  (test)

  :doc "RESULT records retain canonical paths, outcome, and payload"
  (let ((block
         (mevedel-tools--message-delivery-block
          '(:type RESULT
            :sender "/root/spec_review"
            :recipient "/root"
            :outcome completed
            :payload "Review complete."))))
    (should (string-match-p
             "<agent-result sender=\"/root/spec_review\" recipient=\"/root\" outcome=\"completed\">"
             block))
    (should (string-match-p "Review complete\\." block)))

  :doc "MAIL records retain canonical paths and escape delimiter-looking text"
  (let ((block
         (mevedel-tools--message-delivery-block
          '(:type MAIL
            :sender "/root/explorer"
            :recipient "/root/reviewer"
            :payload "literal <agent-result> and <agent-message>"))))
    (should (string-prefix-p
             "<agent-message type=\"MAIL\" sender=\"/root/explorer\" recipient=\"/root/reviewer\">"
             block))
    (should (string-match-p "&lt;agent-result" block))
    (should (string-match-p "&lt;agent-message" block)))

  :doc "USER records remain plain model payloads"
  (should (equal "steer this"
                 (mevedel-tools--message-delivery-block
                  '(:type USER :payload "steer this"))))

  :doc "unknown record types fail closed"
  (should-error
   (mevedel-tools--message-delivery-block '(:type UNKNOWN :payload "x"))))


(mevedel-deftest mevedel-tools--handle-message-inject
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "drains a root canonical MAIL exactly once and records it in transcript"
  (let* ((session (mevedel-tools-test--make-session))
         (buffer (generate-new-buffer " *mt-root-mail*"))
         (data (list :messages (vector)))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :backend nil :data data))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (setf (mevedel-session-messages session)
                (list '(:type MAIL
                        :sender "/root/worker"
                        :recipient "/root"
                        :payload "hello root")))
          (mevedel-tools--handle-message-inject fsm)
          (should-not (mevedel-session-messages session))
          (should (= 1 (length (plist-get data :messages))))
          (should
           (string-match-p
            "sender=\"/root/worker\" recipient=\"/root\""
            (plist-get (aref (plist-get data :messages) 0) :content)))
          (with-current-buffer buffer
            (should (string-match-p "hello root" (buffer-string))))
          (mevedel-tools--handle-message-inject fsm)
          (should (= 1 (length (plist-get data :messages)))))
      (kill-buffer buffer)))

  :doc "drains a retained child's canonical FIFO into its own transcript"
  (let* ((session (mevedel-tools-test--make-session))
         (buffer (generate-new-buffer " *mt-agent-mail*"))
         (invocation
          (mevedel-agent-invocation--create
           :path "/root/worker"
           :parent-session session
           :buffer buffer
           :turn-count 0))
         (record
          (mevedel-agent-record--create
           :path "/root/worker"
           :parent-path "/root"
           :activity 'running
           :invocation invocation))
         (data (list :messages
                     (vector (list :role "user" :content "task"))))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer
                           :backend nil
                           :data data
                           :mevedel-agent-invocation invocation))))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation invocation)
            (insert "* Agent Task: work\n"))
          (let ((mevedel--agent-invocation nil))
            (mevedel-agent-control-send-message
             session "/root/worker" "one")
            (mevedel-agent-control-send-message
             session "/root/worker" "two"))
          (mevedel-tools--handle-message-inject fsm)
          (should-not (mevedel-agent-record-mailbox record))
          (let* ((messages (append (plist-get data :messages) nil))
                 (text (mapconcat
                        (lambda (message) (plist-get message :content))
                        messages "\n")))
            (should (= 3 (length messages)))
            (should (< (string-match-p "one" text)
                       (string-match-p "two" text))))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (< (string-match-p "one" text)
                         (string-match-p "two" text))))))
      (kill-buffer buffer)))

  :doc "keeps USER model payload separate from transcript text and audits"
  (let* ((session (mevedel-tools-test--make-session))
         (buffer (generate-new-buffer " *mt-user-mail*"))
         (audit '(:type prompt-rewrite :event "UserPromptSubmit"
                        :original "original user"
                        :submitted "rewritten model"))
         (data (list :messages (vector)))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :backend nil :data data))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (setf (mevedel-session-messages session)
                (list (list :type 'USER
                            :sender "user"
                            :recipient "/root"
                            :payload "rewritten model"
                            :transcript-payload "original user"
                            :hook-audits (list audit))))
          (mevedel-tools--handle-message-inject fsm)
          (should (equal "rewritten model"
                         (plist-get (aref (plist-get data :messages) 0)
                                    :content)))
          (with-current-buffer buffer
            (let ((text (buffer-substring (point-min) (point-max))))
              (should (string-match-p "original user" text))
              (should-not (string-match-p "rewritten model" text))
              (should (equal (list audit)
                             (mevedel-transcript-audit-records text))))))
      (kill-buffer buffer)))

  :doc "does nothing when the FSM has no owning context"
  (let* ((data (list :messages (vector (list :role "user" :content "x"))))
         (fsm (gptel-make-fsm :info (list :data data))))
    (mevedel-tools--handle-message-inject fsm)
    (should (= 1 (length (plist-get data :messages))))))


(mevedel-deftest mevedel-tools--handle-agent-roster-inject ()
  ,test
  (test)
  :doc "injects each new direct child once and never exposes grandchildren"
  (let* ((session (mevedel-tools-test--make-session))
         (buffer (generate-new-buffer " *mt-agent-roster*"))
         (data (list :messages (vector)))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :backend nil :data data))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (setf (mevedel-session-agent-registry session)
                (list
                 (cons "/root/worker"
                       (mevedel-agent-record--create
                        :path "/root/worker" :parent-path "/root"
                        :role "worker" :activity 'running))
                 (cons "/root/worker/review"
                       (mevedel-agent-record--create
                        :path "/root/worker/review"
                        :parent-path "/root/worker"
                        :role "reviewer" :activity 'running))))
          (mevedel-tools--handle-agent-roster-inject fsm)
          (should (= 1 (length (plist-get data :messages))))
          (let ((content
                 (plist-get (aref (plist-get data :messages) 0) :content)))
            (should (string-match-p "Direct child agents:" content))
            (should (string-match-p "/root/worker.*worker" content))
            (should-not (string-match-p "/root/worker/review" content)))
          (mevedel-tools--handle-agent-roster-inject fsm)
          (should (= 1 (length (plist-get data :messages))))
          (push
           (cons "/root/explore"
                 (mevedel-agent-record--create
                  :path "/root/explore" :parent-path "/root"
                  :role "explorer" :activity 'idle))
           (mevedel-session-agent-registry session))
          (mevedel-tools--handle-agent-roster-inject fsm)
          (should (= 2 (length (plist-get data :messages))))
          (should
           (string-match-p
            "/root/explore.*explorer"
            (plist-get (aref (plist-get data :messages) 1) :content)))
          (should
           (string-match-p
            "New direct child agents:"
            (plist-get (aref (plist-get data :messages) 1) :content))))
      (kill-buffer buffer))))
(mevedel-deftest mevedel-agent-exec--record-activity
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "syncs running activity into transcript metadata"
  (let* ((session (mevedel-tools-test--make-session))
         (parent (generate-new-buffer " *mt-activity-parent*"))
         (agent-id "explorer--activity-sync")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-session session
               :parent-data-buffer parent
               :transcript-status 'running
               :call-count 20
               :activity (list (list :type 'waiting
                                     :summary "waiting")))))
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-transcripts session)
                (list (cons agent-id
                            '(:status running
                              :agent-type "explorer"
                              :description "validate"))))
          (mevedel-agent-exec--record-activity
           inv
           (list :type 'tool-finish
                 :tool-name "Read"
                 :summary "Read done"))
          (let ((entry (cdr (assoc agent-id
                                   (mevedel-session-agent-transcripts
                                    session)))))
            (should (= 20 (plist-get entry :calls)))
            (should (= 2 (length (plist-get entry :activity))))
            (should (equal "Read"
                           (plist-get (cadr (plist-get entry :activity))
                                      :tool-name)))))
      (kill-buffer parent))))






;;
;;; Watchdog, bg-callback hardening, prune










(provide 'test-mevedel-tools)
;;; test-mevedel-tools.el ends here
