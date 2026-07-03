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
(require 'mevedel-agent-exec)
(require 'mevedel-session-persistence)
(require 'mevedel-tools)
(require 'mevedel-tool-task)
(require 'mevedel-tool-ui)
(require 'mevedel-tool-web)
(require 'mevedel-tool-introspect)
(require 'mevedel-cockpit)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(declare-function gptel-make-openai "ext:gptel-openai" (name &rest args))


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

(defun mevedel-tools-test--cleanup-list (&rest buffers)
  "Kill tools cockpit test buffers and BUFFERS."
  (dolist (name (list mevedel-tools-list-buffer-name
                      "*mevedel tool details*"
                      mevedel-tools-help-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun mevedel-tools-test--open-list (session data-buffer &optional view-buffer)
  "Open a tools cockpit for SESSION owned by DATA-BUFFER."
  (let ((view-buffer (or view-buffer data-buffer)))
    (with-current-buffer data-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--view-buffer view-buffer)
      (mevedel-tools-list-open
       (list :view-buffer view-buffer
             :data-buffer data-buffer
             :origin-buffer data-buffer
             :session session
             :workspace (mevedel-session-workspace session))))))


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
;;; Tools listing surface

(mevedel-deftest mevedel-tools-list--status-cell ()
  ,test
  (test)

  :doc "formats state cells with visible labels"
  (dolist (state '(active deferred pending loaded expired))
    (should (equal (substring-no-properties
                    (mevedel-tools-list--status-cell state))
                   (symbol-name state)))))

(mevedel-deftest mevedel-tools-list--item-id ()
  ,test
  (test)

  :doc "uses state, category, and name as the stable row id"
  (should (equal (mevedel-tools-list--item-id
                  '(:state active :category "mevedel" :name "Read"))
                 '(active "mevedel" "Read"))))

(mevedel-deftest mevedel-tools-list--tool-matches-item-p ()
  ,test
  (test)

  :doc "matches tools by both category and name"
  (let ((tool (mevedel-tools-test--make-fake-gptel-tool "Run" "cat-a")))
    (should (mevedel-tools-list--tool-matches-item-p
             tool '(:category "cat-a" :name "Run")))
    (should-not (mevedel-tools-list--tool-matches-item-p
                 tool '(:category "cat-b" :name "Run")))
    (should-not (mevedel-tools-list--tool-matches-item-p
                 tool '(:category "cat-a" :name "Read")))))

(mevedel-deftest mevedel-tools-list--description-cell ()
  ,test
  (test)

  :doc "uses the first paragraph as a single-line table summary"
  (should (equal
           (mevedel-tools-list--description-cell
            '(:description "First line\ncontinues here.\n\nFull details."))
           "First line continues here."))

  :doc "prefers an explicit summary"
  (should (equal
           (mevedel-tools-list--description-cell
            '(:summary "Short summary" :description "Long details"))
           "Short summary")))

(mevedel-deftest mevedel-tools-list--tool-item ()
  ,test
  (test)

  :doc "builds active or pending items from gptel tools"
  (let* ((tool (mevedel-tools-test--make-fake-gptel-tool "Read"))
         (item (mevedel-tools-list--tool-item 'active tool)))
    (should (eq (plist-get item :state) 'active))
    (should (equal (plist-get item :name) "Read"))
    (should (equal (plist-get item :category) "mevedel"))
    (should (equal (plist-get item :description) "Fake tool Read"))
    (should (eq (plist-get item :tool) tool))))

(mevedel-deftest mevedel-tools-list--deferred-item ()
  ,test
  (test)

  :doc "builds deferred items from deferred-set entries"
  (let ((item (mevedel-tools-list--deferred-item
               '((mevedel "Edit") . "Replace text"))))
    (should (eq (plist-get item :state) 'deferred))
    (should (equal (plist-get item :name) "Edit"))
    (should (equal (plist-get item :category) 'mevedel))
    (should (equal (plist-get item :description) "Replace text"))))

(mevedel-deftest mevedel-tools-list--loaded-item ()
  ,test
  (test)

  :doc "builds loaded items with ttl cells"
  (let ((item (mevedel-tools-list--loaded-item '("Imenu" . 3))))
    (should (eq (plist-get item :state) 'loaded))
    (should (equal (plist-get item :name) "Imenu"))
    (should (equal (plist-get item :ttl) "3"))
    (should (string-match-p "Temporarily loaded"
                            (plist-get item :description)))))

(mevedel-deftest mevedel-tools-list--expired-item ()
  ,test
  (test)

  :doc "builds expired items"
  (let ((item (mevedel-tools-list--expired-item "Treesitter")))
    (should (eq (plist-get item :state) 'expired))
    (should (equal (plist-get item :name) "Treesitter"))
    (should (string-match-p "Expired" (plist-get item :description)))))

(mevedel-deftest mevedel-tools-list--collect-items
  (:after-each (mevedel-tools-test--cleanup-list))
  ,test
  (test)

  :doc "collects active, deferred, pending, loaded, and expired items"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-items*"))
         (active-tool (mevedel-tools-test--make-fake-gptel-tool "Read"))
         (pending-tool (mevedel-tools-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Imenu") . "List symbols")))
          (setf (mevedel-session-deferred-pending session)
                (list pending-tool))
          (setf (mevedel-session-deferred-injected session)
                '(("XrefReferences" . 2)))
          (setf (mevedel-session-deferred-expired session)
                '("Treesitter"))
          (let ((items (mevedel-tools-list--collect-items session data-buffer)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :state))
                                   items)
                           '(active deferred pending loaded expired)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :name))
                                   items)
                           '("Read" "Imenu" "Edit"
                             "XrefReferences" "Treesitter")))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--entry ()
  ,test
  (test)

  :doc "builds table cells from tool item state"
  (let* ((item '(:state loaded :name "Imenu" :category "mevedel"
                 :ttl "3" :description "List symbols"))
         (entry (mevedel-tools-list--entry item))
         (cells (cadr entry)))
    (should (equal (car entry) '(loaded "mevedel" "Imenu")))
    (should (equal (substring-no-properties (aref cells 0)) "loaded"))
    (should (equal (aref cells 1) "Imenu"))
    (should (equal (aref cells 2) "mevedel"))
    (should (equal (aref cells 3) "3"))
    (should (equal (aref cells 4) "List symbols")))

  :doc "keeps multiline details out of the table cell"
  (let* ((item '(:state active :name "Agent" :category "mevedel"
                 :description "Launch agents.\n\nForeground details."))
         (entry (mevedel-tools-list--entry item))
         (cells (cadr entry)))
    (should (equal (aref cells 4) "Launch agents."))))

(mevedel-deftest mevedel-tools-list--session-label ()
  ,test
  (test)

  :doc "returns the rendered session name or unknown"
  (let ((session (mevedel-tools-test--make-session))
        (data-buffer (generate-new-buffer " *mt-tools-label*")))
    (with-temp-buffer
      (mevedel-tools-list-mode)
      (should (equal (mevedel-tools-list--session-label) "unknown")))
    (unwind-protect
        (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
          (with-current-buffer buffer
            (should (equal (mevedel-tools-list--session-label) "main"))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--header-line ()
  ,test
  (test)

  :doc "summarizes row counts and key hints"
  (with-temp-buffer
    (mevedel-tools-list-mode)
    (setq mevedel-tools-list--items
          '((:state active :name "Read")
            (:state deferred :name "Edit")
            (:state pending :name "Imenu")
            (:state loaded :name "XrefReferences")
            (:state expired :name "Treesitter")))
    (let ((line (mevedel-tools-list--header-line)))
      (should (string-match-p
               (format "default TTL:%d" mevedel-deferred-tool-ttl)
               line))
      (should (string-match-p "active:1" line))
      (should (string-match-p "deferred:1" line))
      (should (string-match-p "RET details" line))
      (should (string-match-p "q back" line)))))

(mevedel-deftest mevedel-tools-list-open
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (setf (alist-get "mevedel" gptel--known-tools nil t #'equal)
                       nil)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-test--cleanup-list)))
  ,test
  (test)

  :doc "renders active, deferred, pending, loaded, and expired tool rows"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-data*"))
         (active-tool (mevedel-tools-test--make-fake-gptel-tool "Read"))
         (pending-tool (mevedel-tools-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text in a file")
                  (("mevedel" "Imenu") . "List symbols in a file")))
          (setf (mevedel-session-deferred-pending session)
                (list pending-tool))
          (setf (mevedel-session-deferred-injected session)
                '(("XrefReferences" . 3)))
          (setf (mevedel-session-deferred-expired session)
                '("Treesitter"))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (should (eq major-mode 'mevedel-tools-list-mode))
              (should (eq (mevedel-cockpit-context-session
                           (mevedel-cockpit-current-context))
                          session))
              (should (eq (mevedel-cockpit-context-data-buffer
                           (mevedel-cockpit-current-context))
                          data-buffer))
              (should (equal tabulated-list-sort-key '("Name" . nil)))
              (should (= 6 (length tabulated-list-entries)))
              (let ((read-row (cadr (assoc '(active "mevedel" "Read")
                                           tabulated-list-entries)))
                    (edit-row (cadr (assoc '(deferred "mevedel" "Edit")
                                           tabulated-list-entries)))
                    (pending-row (cadr (assoc '(pending "mevedel" "Edit")
                                              tabulated-list-entries)))
                    (loaded-row (cadr (assoc '(loaded "" "XrefReferences")
                                             tabulated-list-entries)))
                    (expired-row (cadr (assoc '(expired "" "Treesitter")
                                              tabulated-list-entries))))
                (should (equal (substring-no-properties (aref read-row 0))
                               "active"))
                (should (equal (aref read-row 2) "mevedel"))
                (should (equal (substring-no-properties (aref edit-row 0))
                               "deferred"))
                (should (equal (aref edit-row 4)
                               "Replace text in a file"))
                (should (equal (substring-no-properties (aref pending-row 0))
                               "pending"))
                (should (equal (aref loaded-row 3) "3"))
                (should (equal (substring-no-properties (aref expired-row 0))
                               "expired"))))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "rejects opening without a cockpit context"
  (with-temp-buffer
    (should-error (mevedel-tools-list-open) :type 'user-error)))

(mevedel-deftest mevedel-tools-list-refresh
  (:after-each (mevedel-tools-test--cleanup-list))
  ,test
  (test)

  :doc "refresh preserves the selected row where possible"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-refresh*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text")))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "mevedel" "Edit"))
              (setcdr (car (mevedel-session-deferred-set session))
                      "Updated")
              (mevedel-tools-list-refresh)
              (should (equal (tabulated-list-get-id)
                             '(deferred "mevedel" "Edit")))
              (should (equal (aref (cadr (assoc '(deferred "mevedel" "Edit")
                                                tabulated-list-entries))
                                  4)
                             "Updated")))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--selected-item
  (:after-each (mevedel-tools-test--cleanup-list))
  ,test
  (test)

  :doc "returns the item represented by the current row"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-selected*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (let ((item (mevedel-tools-list--selected-item)))
                (should (eq (plist-get item :state) 'active))
                (should (equal (plist-get item :name) "Read"))))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--selected-item-for-state
  (:after-each (mevedel-tools-test--cleanup-list))
  ,test
  (test)

  :doc "returns selected row item only for the requested state"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-state-name*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (let ((item (mevedel-tools-list--selected-item-for-state
                           'active)))
                (should (equal (plist-get item :name) "Read")))
              (should-not (mevedel-tools-list--selected-item-for-state
                           'deferred)))))
      (mevedel-tools-test--cleanup-list data-buffer)))

  :doc "distinguishes same-name rows by category"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-state-category*")))
    (unwind-protect
        (progn
          (setf (mevedel-session-deferred-set session)
                '((("cat-a" "Edit") . "A")
                  (("cat-b" "Edit") . "B")))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "cat-b" "Edit"))
              (let ((item (mevedel-tools-list--selected-item-for-state
                           'deferred)))
                (should (equal (plist-get item :category) "cat-b"))
                (should (equal (plist-get item :description) "B"))))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--detail-text ()
  ,test
  (test)

  :doc "formats selected row details"
  (let ((text (mevedel-tools-list--detail-text
               '(:state loaded :name "Imenu" :category "mevedel"
                 :ttl "3" :description "List symbols\n\nFull guidance"))))
    (should (string-match-p "Tool Imenu \\[loaded\\]" text))
    (should (string-match-p "Category: mevedel" text))
    (should (string-match-p "TTL: 3" text))
    (should (string-match-p "Full guidance" text))))

(mevedel-deftest mevedel-tools-list-details
  (:after-each (mevedel-tools-test--cleanup-list))
  ,test
  (test)

  :doc "opens details for the selected tool row"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-details*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (mevedel-tools-list-details))
            (with-current-buffer "*mevedel tool details*"
              (should (string-match-p "Tool Read \\[active\\]"
                                      (buffer-string))))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--main-data-buffer
  (:after-each (progn
                 (mevedel-workspace-clear-registry)
                 (setq mevedel-agent--registry nil)
                 (mevedel-tools-test--cleanup-list)))
  ,test
  (test)

  :doc "rejects agent data buffers before lifecycle mutation"
  (let* ((_ (mevedel-define-agent tool-child :description "child" :tools nil))
         (agent (mevedel-agent-get "tool-child"))
         (inv (mevedel-agent-invocation-create agent))
         (session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-child*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--agent-invocation inv))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (should-error (mevedel-tools-list--main-data-buffer)
                            :type 'user-error))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-tools-list-defer-active
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-test--cleanup-list)))
  ,test
  (test)

  :doc "moves selected active tool into only the current session's deferred set"
  (let* ((session (mevedel-tools-test--make-session))
         (other-session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-defer*"))
         (other-buffer (generate-new-buffer " *mt-tools-other*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Read"))
         (known-before (copy-tree gptel--known-tools)))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (with-current-buffer other-buffer
            (setq-local gptel-tools (list tool)))
          (setf (mevedel-session-deferred-set other-session)
                '((("mevedel" "Keep") . "keep")))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (mevedel-tools-list-defer-active)
              (should (assoc '(deferred "mevedel" "Read")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (null gptel-tools)))
          (with-current-buffer other-buffer
            (should (equal (list tool) gptel-tools)))
          (should (equal "Read" (cadr (caar (mevedel-session-deferred-set
                                             session)))))
          (should (equal '((("mevedel" "Keep") . "keep"))
                         (mevedel-session-deferred-set other-session)))
          (should (equal known-before gptel--known-tools)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer))))

  :doc "falls back to the active-tool prompt when point is not active"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-defer-prompt*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text")))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "mevedel" "Edit"))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Read")))
                (mevedel-tools-list-defer-active))
              (should (assoc '(deferred "mevedel" "Read")
                             tabulated-list-entries)))))
      (mevedel-tools-test--cleanup-list data-buffer)))

  :doc "selected active row only defers the matching category"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-defer-category*"))
         (cat-a (mevedel-tools-test--make-fake-gptel-tool "Run" "cat-a"))
         (cat-b (mevedel-tools-test--make-fake-gptel-tool "Run" "cat-b")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list cat-a cat-b)))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "cat-b" "Run"))
              (mevedel-tools-list-defer-active)
              (should (assoc '(deferred "cat-b" "Run")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (equal (list cat-a) gptel-tools)))
          (should (equal '((("cat-b" "Run") . "Fake tool Run"))
                         (mevedel-session-deferred-set session))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list-activate-deferred
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (setf (alist-get "mevedel" gptel--known-tools nil t #'equal)
                       nil)
                 (setf (alist-get "cat-a" gptel--known-tools nil t #'equal)
                       nil)
                 (setf (alist-get "cat-b" gptel--known-tools nil t #'equal)
                       nil)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-test--cleanup-list)))
  ,test
  (test)

  :doc "moves selected deferred tool into only the current session's active tools"
  (let* ((session (mevedel-tools-test--make-session))
         (other-session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-activate*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Edit"))
         known-before)
    (unwind-protect
        (progn
          (setf (alist-get "Edit"
                           (alist-get "mevedel"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                tool)
          (setq known-before (copy-tree gptel--known-tools))
          (with-current-buffer data-buffer
            (setq-local gptel-tools nil))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text in a file")))
          (setf (mevedel-session-deferred-injected session) '(("Edit" . 2)))
          (setf (mevedel-session-deferred-expired session) '("Edit"))
          (setf (mevedel-session-deferred-set other-session)
                '((("mevedel" "Edit") . "Other session copy")))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "mevedel" "Edit"))
              (mevedel-tools-list-activate-deferred)
              (should (assoc '(active "mevedel" "Edit")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (equal "Edit" (gptel-tool-name (car gptel-tools)))))
          (should (null (mevedel-session-deferred-set session)))
          (should (null (mevedel-session-deferred-injected session)))
          (should (null (mevedel-session-deferred-expired session)))
          (should (equal '((("mevedel" "Edit") . "Other session copy"))
                         (mevedel-session-deferred-set other-session)))
          (should (equal known-before gptel--known-tools)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "falls back to the deferred-tool prompt when point is not deferred"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-activate-prompt*"))
         (tool (mevedel-tools-test--make-fake-gptel-tool "Edit"))
         (active-tool (mevedel-tools-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (setf (alist-get "Edit"
                           (alist-get "mevedel"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                tool)
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text")))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Edit")))
                (mevedel-tools-list-activate-deferred))
              (should (assoc '(active "mevedel" "Edit")
                             tabulated-list-entries)))))
      (mevedel-tools-test--cleanup-list data-buffer)))

  :doc "selected deferred row only activates the matching category"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-activate-category*"))
         (cat-a (mevedel-tools-test--make-fake-gptel-tool "Run" "cat-a"))
         (cat-b (mevedel-tools-test--make-fake-gptel-tool "Run" "cat-b")))
    (unwind-protect
        (progn
          (setf (alist-get "Run"
                           (alist-get "cat-a"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                cat-a)
          (setf (alist-get "Run"
                           (alist-get "cat-b"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                cat-b)
          (setf (mevedel-session-deferred-set session)
                '((("cat-a" "Run") . "A")
                  (("cat-b" "Run") . "B")))
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list cat-a)))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "cat-b" "Run"))
              (mevedel-tools-list-activate-deferred)
              (should (assoc '(active "cat-b" "Run")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (equal (list cat-b cat-a) gptel-tools)))
          (should (equal '((("cat-a" "Run") . "A"))
                         (mevedel-session-deferred-set session))))
      (mevedel-tools-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list-search-load
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (setf (alist-get "mevedel" gptel--known-tools nil t #'equal)
                       nil)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-test--cleanup-list)))
  ,test
  (test)

  :doc "search/load queues matching deferred tools and refreshes pending state"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-search*"))
         (gtool (mevedel-tools-test--make-fake-gptel-tool "Edit"))
         (tool (mevedel-tool--create
                :name "Edit" :category "mevedel"
                :gptel-tool gtool))
         known-before)
    (unwind-protect
        (progn
          (mevedel-tool-register tool)
          (setf (alist-get "Edit"
                           (alist-get "mevedel"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                gtool)
          (setq known-before (copy-tree gptel--known-tools))
          (with-current-buffer data-buffer
            (setq-local gptel-tools nil))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text in a file")))
          (let ((buffer (mevedel-tools-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (let ((result (mevedel-tools-list-search-load "edit")))
                (should (string-match-p "available now" result)))
              (should (= 1 (length (mevedel-session-deferred-pending
                                    session))))
              (should (assoc '(pending "mevedel" "Edit")
                             tabulated-list-entries))
              (should (equal known-before gptel--known-tools)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-tools-list-open-gptel
  (:after-each (progn
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-test--cleanup-list)))
  ,test
  (test)

  :doc "gptel bridge command runs from the paired data buffer"
  (let* ((session (mevedel-tools-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-gptel*"))
         called-buffer)
    (unwind-protect
        (progn
          (mevedel-tools-test--open-list session data-buffer)
          (require 'gptel-transient)
          (with-current-buffer mevedel-tools-list-buffer-name
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)))))
              (mevedel-tools-list-open-gptel)))
          (should (eq called-buffer data-buffer)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-tools-list-help
  (:after-each (mevedel-tools-test--cleanup-list))
  ,test
  (test)

  :doc "opens tools cockpit help"
  (progn
    (mevedel-tools-list-help)
    (with-current-buffer mevedel-tools-help-buffer-name
      (should (string-match-p "RET  Show selected tool details"
                              (buffer-string)))
      (should (string-match-p "l    Search and load"
                              (buffer-string))))))

(mevedel-deftest mevedel-tools-list-quit
  (:after-each (mevedel-tools-test--cleanup-list))
  ,test
  (test)

  :doc "kills the tools cockpit and reopens the main session cockpit"
  (let* ((session (mevedel-tools-test--make-session))
         (view-buffer (generate-new-buffer " *mt-tools-quit-view*"))
         (data-buffer (generate-new-buffer " *mt-tools-quit-data*"))
         called-buffer)
    (unwind-protect
        (let ((buffer (mevedel-tools-test--open-list
                       session data-buffer view-buffer)))
          (require 'mevedel-menu)
          (cl-letf (((symbol-function 'mevedel-menu-open)
                     (lambda (&optional _area)
                       (setq called-buffer (current-buffer)))))
            (with-current-buffer buffer
              (mevedel-tools-list-quit)))
          (should-not (buffer-live-p buffer))
          (should (eq called-buffer data-buffer)))
      (mevedel-tools-test--cleanup-list view-buffer data-buffer))))


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
          (setf (mevedel-agent-invocation-buffer inv) ov-buf)
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

  :doc "does not deliver to a child invocation whose buffer is killed"
  (let* ((_ (mevedel-define-agent sm-dead :description "a" :tools nil))
         (agent (mevedel-agent-get "sm-dead"))
         (child-buf (generate-new-buffer " *mt-sm-dead-child*"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "sm-dead--abc"
               :buffer child-buf
               :transcript-status 'running))
         (buf (generate-new-buffer " *mt-sm-dead-chat*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session
                      (mevedel-tools-test--make-session))
          (setq-local mevedel-tools--agents-fsm
                      (list (cons "sm-dead--abc"
                                  (gptel-make-fsm
                                   :info (list :mevedel-agent-invocation inv)
                                   :handlers nil :state 'WAIT))))
          (kill-buffer child-buf)
          (should-error
           (mevedel-tools--send-message
            (list :to "sm-dead--abc" :message "run")))
          (should-not (mevedel-agent-invocation-messages inv))
          (should-not (assoc "sm-dead--abc" mevedel-tools--agents-fsm)))
      (when (buffer-live-p child-buf) (kill-buffer child-buf))
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
          (setf (mevedel-agent-invocation-buffer inv) ov-buf)
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

(mevedel-deftest mevedel-tools--message-delivery-block
  (:doc "escapes delimiter-looking text inside message bodies")
  ,test
  (test)

  :doc "literal agent-message delimiters do not become nested mailbox markup"
  (let ((block (mevedel-tools--message-delivery-block
                '(:from "worker"
                  :body "literal <agent-message from=\"inner\"> without close"))))
    (should (string-match-p "<agent-message from=\"worker\">" block))
    (should (string-match-p "&lt;agent-message from=\"inner\"" block))
    (should-not (string-match-p "literal <agent-message" block)))

  :doc "literal agent-result block in a message body is escaped"
  (let* ((body "<agent-result agent-id=\"example\">\nnot a result\n</agent-result>")
         (block (mevedel-tools--message-delivery-block
                 (list :from "worker" :body body))))
    (should (string-match-p "<agent-message from=\"worker\">" block))
    (should (string-match-p "&lt;agent-result agent-id=\"example\"" block))
    (should (string-match-p "&lt;/agent-result&gt;" block))
    (should-not (string-match-p "\n<agent-result" block)))

  :doc "generated agent-result deliveries are preserved structurally"
  (let ((block (mevedel-tools--message-delivery-block
                '(:from "worker"
                  :agent-result-p t
                  :body "<agent-result agent-id=\"worker\">\nresult\n</agent-result>"))))
    (should (string-prefix-p "<agent-result agent-id=\"worker\">" block))
    (should-not (string-match-p "<agent-message" block))))

(mevedel-deftest mevedel-tools--handle-message-inject
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "drains invocation mailbox and prepends a user-role block on first turn"
  (let* ((_ (mevedel-define-agent mi-a :description "a" :tools nil))
         (agent (mevedel-agent-get "mi-a"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-mi-ov*"))
         (agent-buf (generate-new-buffer " *mt-mi-agent*")))
    (setf (mevedel-agent-invocation-buffer inv) agent-buf)
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
          (with-current-buffer agent-buf
            (insert "* Agent Task: do work\nbody\n"))
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
            (should (equal "first" (plist-get (aref msgs 1) :content))))
          (with-current-buffer agent-buf
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (< (string-match-p "<agent-message" text)
                         (string-match-p "^\\* Agent Task:" text))))))
      (kill-buffer ov-buf)
      (kill-buffer agent-buf)))

  :doc "appends after the prior turn on subsequent WAIT cycles"
  (let* ((_ (mevedel-define-agent mi-b :description "b" :tools nil))
         (agent (mevedel-agent-get "mi-b"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mt-mi-ov2*"))
         (agent-buf (generate-new-buffer " *mt-mi-agent2*")))
    (setf (mevedel-agent-invocation-buffer inv) agent-buf)
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
          (with-current-buffer agent-buf
            (insert "* Agent Task: do work\nbody\n"))
          (setf (mevedel-agent-invocation-turn-count inv) 1)
          (setf (mevedel-agent-invocation-messages inv)
                '((:from "worker" :body "follow-up")))
          (mevedel-tools--handle-message-inject fsm)
          (let ((msgs (plist-get data :messages)))
            (should (equal 3 (length msgs)))
            ;; Subsequent turn: appended at the end.
            (let ((content (plist-get (aref msgs 2) :content)))
              (should (string-match-p "follow-up" content))))
          (with-current-buffer agent-buf
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (> (string-match-p "<agent-message" text)
                         (string-match-p "^\\* Agent Task:" text))))))
      (kill-buffer ov-buf)
      (kill-buffer agent-buf)))

  :doc "writes main-session mailbox deliveries into the data buffer"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-mi-main*")))
    (unwind-protect
        (let* ((data (list :messages
                           (vector (list :role "user"
                                         :content "original prompt"))))
               (fsm (gptel-make-fsm
                     :info (list :buffer buf
                                 :backend nil
                                 :data data))))
          (with-current-buffer buf
            (setq-local mevedel--session session)
            (insert (propertize "assistant response\n" 'gptel 'response)))
          (setf (mevedel-session-messages session)
                '((:from "explorer--abc" :body "hello main")))
          (mevedel-tools--handle-message-inject fsm)
          (should (null (mevedel-session-messages session)))
          (let ((msgs (plist-get data :messages)))
            (should (equal 2 (length msgs)))
            (should (string-match-p
                     "<agent-message from=\"explorer--abc\">"
                     (plist-get (aref msgs 1) :content)))
            (should (string-match-p
                     "hello main"
                     (plist-get (aref msgs 1) :content))))
          (with-current-buffer buf
            (goto-char (point-min))
            (should (search-forward "<agent-message from=\"explorer--abc\">"
                                    nil t))
            (should (null (get-text-property (match-beginning 0) 'gptel)))))
      (kill-buffer buf)))

  :doc "does not drain the parent session mailbox from agent buffers"
  (let* ((_ (mevedel-define-agent mi-agent-main :description "a" :tools nil))
         (agent (mevedel-agent-get "mi-agent-main"))
         (inv (mevedel-agent-invocation-create agent))
         (session (mevedel-tools-test--make-session))
         (agent-buf (generate-new-buffer " *mt-mi-agent-main*"))
         (result-block
          "<agent-result agent-id=\"reviewer--abc\" type=\"reviewer\">\nreview\n</agent-result>"))
    (unwind-protect
        (let* ((data (list :messages (vector)))
               (fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :backend nil
                                 :data data))))
          (with-current-buffer agent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv)
            (insert "* Agent Task: verify\n"))
          (setf (mevedel-session-messages session)
                (list (list :from "reviewer--abc"
                            :body result-block)))
          (mevedel-tools--handle-message-inject fsm)
          (should (equal 1 (length (mevedel-session-messages session))))
          (should (= 0 (length (plist-get data :messages))))
          (with-current-buffer agent-buf
            (goto-char (point-min))
            (should-not (search-forward "<agent-result" nil t))))
      (kill-buffer agent-buf)))

  :doc "refuses direct session transcript insertion into agent buffers"
  (let* ((_ (mevedel-define-agent mi-agent-write :description "a" :tools nil))
         (agent (mevedel-agent-get "mi-agent-write"))
         (inv (mevedel-agent-invocation-create agent))
         (session (mevedel-tools-test--make-session))
         (agent-buf (generate-new-buffer " *mt-mi-agent-write*"))
         (fsm (gptel-make-fsm :info (list :buffer agent-buf)))
         (result-block
          "<agent-result agent-id=\"reviewer--abc\" type=\"reviewer\">\nreview\n</agent-result>"))
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv)
            (insert "* Agent Task: verify\n"))
          (mevedel-tools--insert-session-injected-prompt
           session fsm result-block)
          (with-current-buffer agent-buf
            (goto-char (point-min))
            (should-not (search-forward "<agent-result" nil t))))
      (kill-buffer agent-buf)))

  :doc "advances the response marker after main-session mailbox insertion"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-mi-main-marker*")))
    (unwind-protect
        (let* ((data (list :messages
                           (vector (list :role "user"
                                         :content "active turn"))))
               (position nil)
               (fsm nil))
          (with-current-buffer buf
            (setq-local mevedel--session session)
            (insert "active turn\n")
            (setq position (copy-marker (point) nil)))
          (setq fsm
                (gptel-make-fsm
                 :info (list :buffer buf
                             :backend nil
                             :data data
                             :position position)))
          (setf (mevedel-session-messages session)
                '((:from "explorer--abc" :body "hello main")))
          (mevedel-tools--handle-message-inject fsm)
          (with-current-buffer buf
            (goto-char position)
            (insert (propertize "assistant response\n" 'gptel 'response))
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (< (string-match-p "<agent-message" text)
                         (string-match-p "assistant response" text)))
              (should (string-match-p "hello main" text)))))
      (kill-buffer buf)))

  :doc "preserves background agent-result blocks without message wrapping"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-mi-result*"))
         (result-block
          "<agent-result agent-id=\"explorer--abc\" type=\"explorer\">\nfound it\n</agent-result>"))
    (unwind-protect
        (let* ((data (list :messages (vector)))
               (fsm (gptel-make-fsm
                     :info (list :buffer buf
                                 :backend nil
                                 :data data))))
          (with-current-buffer buf
            (setq-local mevedel--session session))
          (setf (mevedel-session-messages session)
                (list (list :from "explorer--abc"
                            :body result-block
                            :agent-result-p t)))
          (mevedel-tools--handle-message-inject fsm)
          (let* ((msgs (plist-get data :messages))
                 (content (plist-get (aref msgs 0) :content)))
            (should (string-match-p
                     "\\`<agent-result agent-id=\"explorer--abc\""
                     content))
            (should-not (string-match-p "<agent-message" content)))
          (with-current-buffer buf
            (goto-char (point-min))
            (should (search-forward
                     "<agent-result agent-id=\"explorer--abc\""
                     nil t))
            (should-not (get-text-property (match-beginning 0) 'gptel))
            (should (search-forward "</agent-result>" nil t))
            (should-not (get-text-property (1- (match-end 0)) 'gptel))))
      (kill-buffer buf)))

  :doc "drops duplicate background agent-result deliveries"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-mi-result-dupe*"))
         (agent-id "explorer--abc123")
         (result-block
          (format
           "<agent-result agent-id=\"%s\" type=\"explorer\">\nfound it\n</agent-result>"
           agent-id)))
    (unwind-protect
        (let* ((data (list :messages (vector)))
               (fsm (gptel-make-fsm
                     :info (list :buffer buf
                                 :backend nil
                                 :data data))))
          (with-current-buffer buf
            (setq-local mevedel--session session)
            (insert result-block "\n"))
          (setf (mevedel-session-messages session)
                (list (list :from agent-id
                            :body result-block
                            :agent-result-p t)))
          (mevedel-tools--handle-message-inject fsm)
          (should-not (mevedel-session-messages session))
          (should (= 0 (length (plist-get data :messages))))
          (with-current-buffer buf
            (goto-char (point-min))
            (should (search-forward agent-id nil t))
            (should-not (search-forward agent-id nil t))))
      (kill-buffer buf)))

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
                      (lambda (_cb _type _desc _prompt &rest _)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-tools--task-by-name
               (lambda (resp &rest _) (setq result resp))
               "explorer" "survey" "survey files"
               t))
            ;; main-cb should have been called synchronously.
            ;; The launch status may be wrapped with render-data for
            ;; the running-handle badge when a transcript path is set
            ;; on the invocation; extract the visible string from
            ;; either shape.
            (let ((launch-string
                   (cond
                    ((stringp result) result)
                    ((and (listp result) (plist-get result :result))
                     (plist-get result :result))
	                    (t (error "Unexpected main-cb shape: %S" result)))))
              (should (stringp launch-string))
              (should (string-match-p "background" launch-string))
              (should (string-match-p "explorer" launch-string)))
            (when (and (listp result) (plist-get result :render-data))
              (should (eq t (plist-get (plist-get result :render-data)
                                        :background))))
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
              (mevedel-tools--task-by-name
               #'ignore "explorer" "survey" "survey files"
               t))
            ;; Simulate sub-agent completing
            (funcall captured-cb "The exploration found 5 issues.")
            ;; Result should be in the parent session's mailbox
            (let* ((msgs (mevedel-session-messages session))
                   (msg (car msgs)))
              (should (= 1 (length msgs)))
              (should (string-match-p "explorer" (plist-get msg :from)))
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
            (mevedel-tools--task-by-name
             (lambda (resp &rest _) (setq result resp))
             "explorer" "survey" "survey files"
             nil)
            ;; main-cb should NOT have been called yet
            (should (null result))
            ;; Simulate sub-agent completing
            (funcall captured-cb "Done.")
            (should (equal "Done." result))))
      (kill-buffer buf)))

  :doc "foreground stop completes the parent Agent tool callback"
  (require 'mevedel-tool-ui)
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-fg-stop*"))
         (result nil)
         (main-calls 0)
         (inv nil)
         (child-fsm nil)
         (stop-result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (_cb _type _desc _prompt
                                   &optional invocation _agent-buffer)
                        (setq inv invocation)
                        (setq child-fsm
                              (gptel-make-fsm
                               :info (list :context ov
                                           :buffer buf
                                           :mevedel-agent-invocation inv
                                           :callback #'ignore)
                               :state 'WAIT))
                        child-fsm))
                     ((symbol-function
                       'mevedel-agent-exec--save-transcript-buffer)
                      (lambda (_invocation) t))
                     ((symbol-function 'mevedel-agent-exec--handle-update)
                      (lambda (_invocation) nil))
                     ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                      (lambda (_invocation _status) nil))
                     ((symbol-function
                       'mevedel-session-persistence--update-transcript-entry)
                      (lambda (_session _agent-id _updates) nil))
                     ((symbol-function
                       'mevedel-session-persistence--write-sidecar-now)
                      (lambda (_session _buffer) t)))
            (mevedel-tools--task-by-name
             (lambda (resp &rest _)
               (cl-incf main-calls)
               (setq result resp))
             "explorer" "survey" "survey files"
             nil)
            (should inv)
            (should (functionp
                     (mevedel-agent-invocation-parent-tool-callback inv)))
            (should (assoc (mevedel-agent-invocation-agent-id inv)
                           mevedel-tools--agents-fsm))
            (setq stop-result
                  (mevedel-tools-stop-agent
                   (mevedel-agent-invocation-agent-id inv)
                   "manual stop"))
            (should (= 1 main-calls))
            (should (string-match-p "was stopped" result))
            (should (string-match-p "manual stop" result))
            (should (plist-get stop-result :completed-tool-callback))
            (should (eq 'aborted
                        (mevedel-agent-invocation-transcript-status inv)))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-tools--agents-fsm))))
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
             '(:subagent_type "explorer"
               :description "survey"
               :prompt "survey files"
               :run_in_background :json-false))
            ;; Foreground path: main-cb must not fire yet, and the
            ;; launch-status string must NOT be returned synchronously.
            (should (null result))
            (funcall captured-cb "Done.")
            (should (equal "Done." result))))
      (kill-buffer buf)))

  :doc "Agent handler forwards optional model tier"
  (require 'mevedel-tool-ui)
  (let ((captured nil)
        (result nil))
    (cl-letf (((symbol-function 'mevedel-tools--task-by-name)
               (lambda (_cb agent-type description prompt background model-tier)
                 (setq captured
                       (list :agent-type agent-type
                             :description description
                             :prompt prompt
                             :background background
                             :model-tier model-tier)))))
      (mevedel-tool-ui--agent
       (lambda (resp &rest _) (setq result resp))
       '(:subagent_type "explorer"
         :description "survey"
         :prompt "survey files"
         :model "fast"))
      (should (null result))
      (should (equal "explorer" (plist-get captured :agent-type)))
      (should (eq 'fast (plist-get captured :model-tier)))))

  :doc "Agent handler rejects concrete provider strings"
  (require 'mevedel-tool-ui)
  (let ((called nil)
        (result nil))
    (cl-letf (((symbol-function 'mevedel-tools--task-by-name)
               (lambda (&rest _)
                 (setq called t))))
      (mevedel-tool-ui--agent
       (lambda (resp &rest _) (setq result resp))
       '(:subagent_type "explorer"
         :description "survey"
         :prompt "survey files"
         :model "Claude:claude-sonnet-4-5"))
      (should (null called))
      (should (string-match-p "Unknown model tier" result))))

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
	                       (error "Runner must not be called for unknown agent"))))
            (mevedel-tools--task-by-name
             (lambda (resp &rest _) (setq result resp))
             "no-such-agent-type" "oops" "do nothing")
            (should (null runner-called))
            (should (stringp result))
            (should (string-match-p "Unknown agent type: no-such-agent-type"
                                    result))
            ;; No background tracking should have been created.
            (should (null (mevedel-session-background-agents session)))
            (should (null mevedel-tools--agents-fsm))))
      (kill-buffer buf)))

  :doc "background launch failure does not leave stale running reminder"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-bg-start-fail*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
	          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
	                     (lambda (&rest _)
	                       (error "Boom"))))
            (let ((mevedel-tools--current-fsm nil))
              (should-error
               (mevedel-tools--task-by-name
                #'ignore "explorer" "survey" "survey files" t))))
          (should (null (mevedel-session-background-agents session)))
          (should (null (mevedel-session-pending-reminders session)))
          (should (null mevedel-tools--agents-fsm)))
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
          (mevedel-tools--ctx-push-background-agent session "explorer--abc123")
          (should (mevedel-tools--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "returns non-nil when invocation has background agents"
  (require 'mevedel-tool-ui)
  (let* ((buf (generate-new-buffer " *mt-bwait3*"))
         (inv (mevedel-agent-invocation--create
               :agent (mevedel-agent--create :name "coordinator")))
         (info (list :buffer buf :mevedel-agent-invocation inv)))
    (unwind-protect
        (progn
          (mevedel-tools--ctx-push-background-agent inv "explorer--abc123")
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
           session '(:from "explorer--x" :body "done"))
          (should (mevedel-tools--background-agents-pending-p info)))
      (kill-buffer buf))))

(mevedel-deftest mevedel-tools--bwait-injected-table
  ()
  ,test
  (test)

  :doc "inserts BWAIT before DONE in TYPE and TRET transitions"
  (let* ((table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((tool-p . TPRE) (t . DONE)))
                  (TRET . ((error-p . ERRS) (result-p . WAIT) (t . DONE)))))
         (result (mevedel-tools--bwait-injected-table table)))
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
  (let* ((table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((t . DONE)))
                  (TRET . ((t . DONE)))))
         (result (mevedel-tools--bwait-injected-table table)))
    (should (equal '((t . WAIT)) (cdr (assq 'INIT result))))
    (should (equal '((t . TYPE)) (cdr (assq 'WAIT result)))))

  :doc "re-injecting an already-injected table is a no-op (no duplicate predicates)"
  (let* ((mevedel-tools--bwait-table-cache nil)
         (table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((tool-p . TPRE) (t . DONE)))
                  (TRET . ((error-p . ERRS) (result-p . WAIT) (t . DONE)))))
         (once (mevedel-tools--bwait-injected-table table))
         (twice (mevedel-tools--bwait-injected-table once)))
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
         (buf (generate-new-buffer " *mt-bwait-track*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (_cb _type _desc _prompt &rest _)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-tools--task-by-name
               #'ignore "explorer" "survey" "survey files"
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
              (mevedel-tools--task-by-name
               #'ignore "explorer" "survey" "survey files"
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
              (mevedel-tools--task-by-name
               #'ignore "explorer" "survey" "survey files"
               t))
            ;; Park the parent FSM in BWAIT.
            (setf (gptel-fsm-state parent-fsm) 'BWAIT)
            ;; Complete the background agent.
            (funcall captured-cb "Done.")
            ;; Parent FSM should have been resumed to WAIT.
            (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
            (should resumed)))
      (kill-buffer buf)))

  :doc "watchdog keeps BWAIT parked when messages and live agents coexist"
  (let* ((session (mevedel-tools-test--make-session))
         (parent (generate-new-buffer " *mt-bwait-mixed*"))
         (agent-buf (generate-new-buffer " *mt-bwait-mixed-agent*"))
         (live-inv (mevedel-agent-invocation--create
                    :buffer agent-buf
                    :background-p t))
         (live-fsm (gptel-make-fsm
                    :state 'WAIT
                    :info (list :buffer agent-buf
                                :mevedel-agent-invocation live-inv)))
         (fsm (gptel-make-fsm
               :state 'BWAIT
               :info (list :buffer parent)))
         transitioned
         rearmed)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        (list (cons "agent-live" live-fsm))))
          (setf (mevedel-session-background-agents session)
                (list "agent-live"))
          (setf (mevedel-session-messages session)
                (list (list :from "done" :body "result")))
          (let ((mevedel-agent-background-timeout 1))
            (cl-letf (((symbol-function 'gptel--fsm-transition)
                       (lambda (_fsm state) (setq transitioned state)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _args) (setq rearmed t))))
              (mevedel-tools--bwait-watchdog-expire fsm)))
          (should-not transitioned)
          (should rearmed)
          (should (equal '("agent-live")
                         (mevedel-session-background-agents session)))
          (should (mevedel-session-messages session)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer parent))))


(mevedel-deftest mevedel-agent-exec--record-activity
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "syncs running background activity into transcript metadata"
  (let* ((session (mevedel-tools-test--make-session))
         (parent (generate-new-buffer " *mt-activity-parent*"))
         (agent-id "explorer--activity-sync")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-session session
               :parent-data-buffer parent
               :transcript-status 'running
               :background-p t
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
            (mevedel-tools--task-by-name
             (lambda (resp &rest _)
               (cl-incf call-count)
               (setq result resp))
             "coordinator" "orchestrate" "do stuff")
            (should inv)
            ;; Step 2: Simulate the coordinator spawning a background
            ;; agent.  Directly push onto the invocation's
            ;; background-agents.
            (mevedel-tools--ctx-push-background-agent inv "explorer--fake")
            ;; Step 3: The coordinator's LLM returns text-only while
            ;; children still pending -- main-cb must not fire.
            (funcall coordinator-cb "Waiting for results...")
            (should (null result))
            (should (zerop call-count))
            (should (assoc (mevedel-agent-invocation-agent-id inv)
                           mevedel-tools--agents-fsm))
            ;; Step 4: Child finishes, then coordinator fires final.
            (mevedel-tools--ctx-remove-background-agent inv "explorer--fake")
            (funcall coordinator-cb "Final summary with results.")
            (should (stringp result))
            (should (string-match-p "Final summary" result))
            (should (= 1 call-count))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-tools--agents-fsm))
            ;; Step 5: A late duplicate 't' event must NOT double-fire.
            (funcall coordinator-cb "Redundant late response.")
            (should (= 1 call-count))
            (should (string-match-p "Final summary" result))))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
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
            (mevedel-tools--task-by-name
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
             inv (list :from "explorer--fake" :body "done"))
            (funcall coordinator-cb "Preliminary handoff.")
            ;; Must NOT fire yet -- the mailbox still has to drain.
            (should (null result))
            (should (zerop call-count))
            (should (assoc (mevedel-agent-invocation-agent-id inv)
                           mevedel-tools--agents-fsm))
            ;; Mailbox drains (simulating WAIT); coordinator fires final.
            (setf (mevedel-agent-invocation-messages inv) nil)
            (funcall coordinator-cb "Final summary with results.")
            (should (= 1 call-count))
            (should (string-match-p "Final summary" result))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-tools--agents-fsm))))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
      (kill-buffer buf)))

  :doc "foreground callback removes parent registry from an agent buffer"
  (let* ((mevedel-session-persistence nil)
         (session (mevedel-tools-test--make-session))
         (parent-buf (generate-new-buffer " *mt-stash-agent-parent*"))
         (callback-buf (generate-new-buffer " *mt-stash-agent-callback*"))
         (coordinator-cb nil)
         (result nil)
         (inv nil))
    (unwind-protect
        (progn
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm nil)
            (cl-letf* ((ov (progn (insert "x")
                                  (make-overlay (point-min) (point-max))))
                       (fake-coordinator-fsm
                        (gptel-make-fsm
                         :info (list :context ov :buffer parent-buf)))
                       ((symbol-function 'mevedel-agent-exec--run)
                        (lambda (cb _type _desc _prompt
                                    &optional invocation _agent-buffer)
                          (setq coordinator-cb cb
                                inv invocation)
                          (when inv
                            (overlay-put ov 'mevedel-agent-invocation inv))
                          fake-coordinator-fsm)))
              (mevedel-tools--task-by-name
               (lambda (resp &rest _) (setq result resp))
               "coordinator" "orchestrate" "do stuff")
              (should inv)
              (should (assoc (mevedel-agent-invocation-agent-id inv)
                             mevedel-tools--agents-fsm))))
          (with-current-buffer callback-buf
            (setq-local mevedel-tools--agents-fsm nil)
            (funcall coordinator-cb "Final summary from agent buffer."))
          (should (string-match-p "Final summary" result))
          (with-current-buffer parent-buf
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-tools--agents-fsm)))
          (with-current-buffer callback-buf
            (should-not mevedel-tools--agents-fsm)))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
      (when (buffer-live-p callback-buf) (kill-buffer callback-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

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
            (mevedel-tools--task-by-name
             (lambda (resp &rest _) (setq result resp))
             "coordinator" "orchestrate" "do stuff")
            (mevedel-tools--ctx-push-background-agent inv "explorer--fake")
            ;; Error response must forward immediately so the parent
            ;; tool call doesn't hang on a dead child.
            (funcall coordinator-cb "Error: Task aborted by the user.")
            (should (stringp result))
            (should (string-match-p "Error:" result))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-tools--agents-fsm))))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
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

  :doc "stranded agents synthesize mailbox results and transition parent to WAIT"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-done*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (insert "aa")
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (setf (mevedel-session-background-agents session)
                  '("explorer--A" "explorer--B"))
            (setf (mevedel-session-messages session) nil)
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 2 (length (mevedel-session-messages session))))
            (dolist (msg (mevedel-session-messages session))
              (let ((body (plist-get msg :body)))
                (should (string-match-p "<agent-result" body))
                (should (string-match-p "became stranded" body))
                (should (string-match-p "No saved transcript path" body))))
            (should (null mevedel-tools--agents-fsm))))
      (kill-buffer buf)))

  :doc "stranded agent with queued result only clears stale tracking"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-queued-result*"))
         (body (mevedel-tools--agent-result-format
                "explorer--A" "explorer" "survey" "real result"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session)
                  (list (list :from "explorer--A"
                              :body body
                              :agent-result-p t
                              :timestamp (current-time))))
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 1 (length (mevedel-session-messages session))))
            (let ((queued (plist-get (car (mevedel-session-messages session))
                                     :body)))
              (should (string-match-p "real result" queued))
              (should-not (string-match-p "became stranded" queued)))
            (should (null mevedel-tools--agents-fsm))))
      (kill-buffer buf)))

  :doc "stranded agent result includes Read-able transcript path from sidecar metadata"
  (let* ((session (mevedel-tools-test--make-session))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-bwait-stranded" t)))
         (rel-path "agents/explorer--partial.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (buf (generate-new-buffer " *mt-wd-transcript*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (with-temp-file abs-path
            (insert "saved transcript"))
          (setf (mevedel-session-agent-transcripts session)
                `(("explorer--A" :agent-type "explorer"
                   :description "survey files"
                   :path ,rel-path
                   :status running)))
          (setf (mevedel-session-background-agents session)
                '("explorer--A"))
          (setf (mevedel-session-messages session) nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 1 (length (mevedel-session-messages session))))
            (let* ((msg (car (mevedel-session-messages session)))
                   (body (plist-get msg :body)))
              (should (string-match-p "<agent-result agent-id=\"explorer--A\""
                                      body))
              (should (string-match-p "survey files" body))
              (should (string-match-p
                       (regexp-quote (format "Transcript: %s" abs-path))
                       body))
              (should (string-match-p
                       (regexp-quote (format "Read(file_path=%S)" abs-path))
                       body)))
            (let ((entry (cdr (assoc "explorer--A"
                                     (mevedel-session-agent-transcripts
                                      session)))))
              (should (eq 'incomplete (plist-get entry :status))))))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (kill-buffer buf)))

  :doc "stranded agent result rejects symlinked transcript path"
  (let* ((session (mevedel-tools-test--make-session))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-bwait-symlink" t)))
         (target (make-temp-file "mevedel-bwait-symlink-target"))
         (rel-path "agents/explorer--partial.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (buf (generate-new-buffer " *mt-wd-symlink*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (make-symbolic-link target abs-path)
          (setf (mevedel-session-agent-transcripts session)
                `(("explorer--A" :agent-type "explorer"
                   :description "survey files"
                   :path ,rel-path
                   :status running)))
          (setf (mevedel-session-background-agents session)
                '("explorer--A"))
          (setf (mevedel-session-messages session) nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-tools--bwait-watchdog-expire parent)
            (let* ((msg (car (mevedel-session-messages session)))
                   (body (plist-get msg :body)))
              (should-not (string-match-p "Transcript:" body))
              (should-not (string-match-p "Read(file_path=" body))
              (should (string-match-p "No saved transcript path" body)))))
      (when (file-exists-p target) (delete-file target))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (kill-buffer buf)))

  :doc "non-empty mailbox transitions parent to WAIT so drain fires"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-wait*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (setf (mevedel-session-background-agents session) '("explorer--X"))
          (setf (mevedel-session-messages session)
                '((:from "explorer--finished" :body "done")))
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))))
      (kill-buffer buf)))

  :doc "slow live agents remain parked in BWAIT and re-arm watchdog"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-live*"))
         (agent-buf (generate-new-buffer " *mt-wd-live-agent*"))
         (mevedel-tools--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (timer-count 0))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation-create agent))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'TOOL))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (mevedel-agent-invocation-buffer inv) agent-buf)
            (setf (alist-get "explorer--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (cl-incf timer-count)
                         'timer)))
              (mevedel-tools--bwait-watchdog-expire parent))
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))
            (should (assoc "explorer--A" mevedel-tools--agents-fsm))
            (should (gethash "explorer--A"
                             mevedel-tools--background-watchdogs))
            (should (= 1 timer-count))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "background progress resets no-progress grace while parked in BWAIT"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-progress*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-progress-agent*"))
         (mevedel-tools--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (delay &rest _args)
                         (push delay delays)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'mevedel-tools-stop-agent)
                       (lambda (&rest args) (setq stopped args))))
              (mevedel-tools--bwait-watchdog-expire parent)
              (setf (mevedel-agent-invocation-activity inv)
                    '((:type tool-start :time 105.0)))
              (setq clock 109.0)
              (mevedel-tools--bwait-watchdog-expire parent))
            (should-not stopped)
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '(10 6) (nreverse delays)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "BWAIT handler arms background no-progress without stranded watchdog"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-handle*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-handle-agent*"))
         (mevedel-tools--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout nil)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (delay &rest _args)
                         (push delay delays)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'gptel--update-status)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'mevedel-tools-stop-agent)
                       (lambda (agent-id reason parent-buffer)
                         (setq stopped (list agent-id reason parent-buffer))
                         (setf (mevedel-session-background-agents session)
                               nil)
                         (gptel--fsm-transition parent 'WAIT))))
              (mevedel-tools--handle-bwait parent)
              (should (equal '(10) (nreverse delays)))
              (should (gethash "explorer--A"
                               mevedel-tools--background-watchdogs))
              (setq clock 110.0)
              (mevedel-tools--bwait-watchdog-expire parent))
            (should (equal "explorer--A" (car stopped)))
            (should (string-match-p "no progress" (cadr stopped)))
            (should (eq buf (caddr stopped)))
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should-not (gethash "explorer--A"
                                 mevedel-tools--background-watchdogs))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "early BWAIT watchdog timer reschedules remaining background grace"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-early*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-early-agent*"))
         (mevedel-tools--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (delay &rest _args)
                         (push delay delays)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'mevedel-tools-stop-agent)
                       (lambda (&rest args) (setq stopped args))))
              (mevedel-tools--bwait-watchdog-expire parent)
              (setq clock 105.0)
              (mevedel-tools--bwait-watchdog-expire parent))
            (should-not stopped)
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '(10 5) (nreverse delays)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "background TOOL-state agent stops after no-progress grace"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-stop*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-stop-agent*"))
         (mevedel-tools--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'TOOL))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args) 'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'mevedel-tools-stop-agent)
                       (lambda (agent-id reason parent-buffer)
                         (setq stopped (list agent-id reason parent-buffer))
                         (setf (mevedel-session-background-agents session)
                               nil)
                         (gptel--fsm-transition parent 'WAIT))))
              (mevedel-tools--bwait-watchdog-expire parent)
              (setq clock 110.0)
              (mevedel-tools--bwait-watchdog-expire parent))
            (should (equal "explorer--A" (car stopped)))
            (should (string-match-p "no progress" (cadr stopped)))
            (should (eq buf (caddr stopped)))
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should-not (gethash "explorer--A"
                                 mevedel-tools--background-watchdogs))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "failed background no-progress stop keeps parent in BWAIT"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-stop-fail*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-stop-fail-agent*"))
         (mevedel-tools--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout nil)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         (timer-count 0)
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (cl-incf timer-count)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
	                      ((symbol-function 'mevedel-tools-stop-agent)
	                       (lambda (&rest args)
	                         (setq stopped args)
	                         (error "Simulated stop failure"))))
              (mevedel-tools--bwait-watchdog-expire parent)
              (setq clock 110.0)
              (mevedel-tools--bwait-watchdog-expire parent))
            (should (equal "explorer--A" (car stopped)))
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))
            (should (gethash "explorer--A"
                             mevedel-tools--background-watchdogs))
            (should (= 2 timer-count))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "stale child FSM with killed buffer recovers as stranded"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-killed-child*"))
         (agent-buf (generate-new-buffer " *mt-wd-killed-agent*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :description "stale child"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :buffer agent-buf
                                     :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (kill-buffer agent-buf)
            (setf (alist-get "explorer--A" mevedel-tools--agents-fsm nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 1 (length (mevedel-session-messages session))))
            (let ((body (plist-get (car (mevedel-session-messages session))
                                   :body)))
              (should (string-match-p "became stranded" body)))
            (should (null mevedel-tools--agents-fsm))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "does not synthesize duplicate result already in parent transcript"
  (let* ((session (mevedel-tools-test--make-session))
         (buf (generate-new-buffer " *mt-wd-transcript-dupe*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-tools--agents-fsm nil)
          (insert (mevedel-tools--agent-result-format
                   "explorer--A" "explorer" "survey" "real result")
                  "\n")
          (setf (mevedel-session-background-agents session)
                '("explorer--A"))
          (setf (mevedel-session-messages session) nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-tools--bwait-watchdog-expire parent)
            (should (eq 'DONE (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (null (mevedel-session-messages session)))))
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
              (mevedel-tools--task-by-name #'ignore "explorer" "survey" "go" t))
            (should (= 1 (length (mevedel-session-background-agents session))))
            ;; Break push-message so the bg callback's push branch raises;
            ;; remove-background-agent MUST still run so the parent isn't
            ;; stranded in BWAIT.
	            (cl-letf (((symbol-function 'mevedel-tools--ctx-push-message)
	                       (lambda (&rest _)
	                         (cl-incf push-called)
	                         (error "Simulated push failure")))
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
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv-a (mevedel-agent-invocation-create agent))
                 (inv-b (mevedel-agent-invocation-create agent))
                 (_live-buffers
                  (setf (mevedel-agent-invocation-buffer inv-a) buf
                        (mevedel-agent-invocation-buffer inv-b) buf))
                 (alive (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-a)
                         :handlers nil :state 'WAIT))
                 (done  (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-b)
                         :handlers nil :state 'DONE)))
            (setf (alist-get "alive" mevedel-tools--agents-fsm nil nil #'equal) alive)
            (setf (alist-get "done"  mevedel-tools--agents-fsm nil nil #'equal) done)
            (mevedel-tools--prune-stale-agents-fsm)
            (should (assoc "alive" mevedel-tools--agents-fsm))
            (should-not (assoc "done" mevedel-tools--agents-fsm))))
      (kill-buffer buf)))

  :doc "prunes TOOL-state FSMs that no longer carry an invocation"
  (let ((buf (generate-new-buffer " *mt-prune-tool*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation-create agent))
                 (_live-buffer
                  (setf (mevedel-agent-invocation-buffer inv) buf))
                 (good (gptel-make-fsm
                        :info (list :mevedel-agent-invocation inv)
                       :handlers nil :state 'TOOL))
                 (bad  (gptel-make-fsm :info nil
                       :handlers nil :state 'TOOL)))
            (setf (alist-get "good" mevedel-tools--agents-fsm nil nil #'equal) good)
            (setf (alist-get "bad"  mevedel-tools--agents-fsm nil nil #'equal) bad)
            (mevedel-tools--prune-stale-agents-fsm)
            (should (assoc "good" mevedel-tools--agents-fsm))
            (should-not (assoc "bad" mevedel-tools--agents-fsm))))
      (kill-buffer buf)))

  :doc "prunes FSMs whose invocation transcript status is terminal"
  (let ((buf (generate-new-buffer " *mt-prune-terminal-inv*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-tools--agents-fsm nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv-a (mevedel-agent-invocation-create agent))
                 (inv-b (mevedel-agent-invocation-create agent))
                 (_live-buffers
                  (setf (mevedel-agent-invocation-buffer inv-a) buf
                        (mevedel-agent-invocation-buffer inv-b) buf))
                 (alive (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-a)
                         :handlers nil :state 'WAIT))
                 (done  (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-b)
                         :handlers nil :state 'WAIT)))
            (setf (mevedel-agent-invocation-transcript-status inv-a)
                  'running)
            (setf (mevedel-agent-invocation-transcript-status inv-b)
                  'error)
            (setf (alist-get "alive" mevedel-tools--agents-fsm
                             nil nil #'equal)
                  alive)
            (setf (alist-get "done" mevedel-tools--agents-fsm
                             nil nil #'equal)
                  done)
            (mevedel-tools--prune-stale-agents-fsm)
            (should (assoc "alive" mevedel-tools--agents-fsm))
            (should-not (assoc "done" mevedel-tools--agents-fsm))))
      (kill-buffer buf))))


(provide 'test-mevedel-tools)
;;; test-mevedel-tools.el ends here
