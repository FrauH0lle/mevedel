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
(require 'mevedel-turn)
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


;;
;;; Helpers

(defun test-mevedel-reminders--content (reminder ctx)
  "Return REMINDER's body for CTX, applying any staged commit.
Content returns either a body string or a `:body'/`:commit' plist, so
this collapses both shapes to the delivered text."
  (let ((result (funcall (mevedel-reminder-content reminder) ctx)))
    (if (stringp result)
        result
      (when-let* ((commit (plist-get result :commit)))
        (funcall commit))
      (plist-get result :body))))

(defun test-mevedel-reminders--collect-and-commit (reminders turn ctx)
  "Collect REMINDERS at TURN for CTX, run their commits, return the blocks."
  (let ((staged (mevedel-reminders--collect-from reminders turn ctx)))
    (mapc #'funcall (plist-get staged :commits))
    (mapcar (lambda (entry)
              (mevedel-reminders-format-block (plist-get entry :body)))
            (plist-get staged :entries))))

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
            :recipe '(pending-events))))
    (should (eq 'demo (mevedel-reminder-type r)))
    (should (functionp (mevedel-reminder-trigger r)))
    (should (functionp (mevedel-reminder-content r)))
    (should (equal 3 (mevedel-reminder-interval r)))
    (should (null (mevedel-reminder-last-fired r)))
    (should (equal '(pending-events)
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
             :recipe '(pending-events)))
         (_  (setf (mevedel-reminder-last-fired r) 42))
         (clone (mevedel-reminder-clone r)))
    (should (eq 'a (mevedel-reminder-type clone)))
    (should (eq trigger (mevedel-reminder-trigger clone)))
    (should (eq content (mevedel-reminder-content clone)))
    (should (equal 7 (mevedel-reminder-interval clone)))
    (should (null (mevedel-reminder-last-fired clone)))
    (should (equal '(pending-events)
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
    (test-mevedel-reminders--collect-and-commit (list clone-a) 0 nil)
    (should (equal 0 (mevedel-reminder-last-fired clone-a)))
    (should (null (mevedel-reminder-last-fired clone-b)))
    (should (null (mevedel-reminder-last-fired r)))))

(mevedel-deftest mevedel-reminders--recipe-p
  ()
  ,test
  (test)
  :doc "accepts only trusted constructors with valid argument contracts"
  (should (mevedel-reminders--recipe-p '(mode-constraints 9)))
  (should (mevedel-reminders--recipe-p '(pending-events)))
  (should-not (mevedel-reminders--recipe-p '(unknown-recipe)))
  (should-not (mevedel-reminders--recipe-p '(mode-constraints . 9)))
  (should-not (mevedel-reminders--recipe-p '(pending-events 1)))
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
                (mevedel-reminders-make-pending-events)))))
    (should (equal '((mode-constraints 9) (pending-events)) recipes)))
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
          '((mode-constraints 9) (pending-events)))))
    (should (equal '(mode-constraints pending-events)
                   (mapcar #'mevedel-reminder-type restored)))
    (should (= 9 (mevedel-reminder-interval (car restored)))))
  (should-error
   (mevedel-reminders-restore-agent-templates
    '((erase-buffer))))
  (should-error
   (mevedel-reminders-restore-agent-templates
    '((pending-events 1))))
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
         (staged (mevedel-reminders--collect-from (list r1 r2) 4 nil)))
    (should (equal '((:type a :body "one")
                     (:type b :body "two"))
                   (plist-get staged :entries)))
    ;; Firing is staged, not applied, until the payload carries it.
    (should-not (mevedel-reminder-last-fired r1))
    (mapc #'funcall (plist-get staged :commits))
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
    (test-mevedel-reminders--collect-and-commit (list r) 0 ctx)
    (should (eq seen-trigger ctx))
    (should (eq seen-content ctx))))


;;
;;; Block formatting

(mevedel-deftest mevedel-reminders-format-block
  (:doc "`mevedel-reminders-format-block' wraps content in XML tags")
  (should (equal "<system-reminder>\nhello\n</system-reminder>"
                 (mevedel-reminders-format-block "hello"))))

(mevedel-deftest mevedel-reminders--entry-label
  ()
  ,test
  (test)
  :doc "renders a symbol type as its name"
  (should (equal "skills-delta" (mevedel-reminders--entry-label 'skills-delta)))
  :doc "renders a cons turn-event key as car:cdr"
  (should (equal "specialist:read"
                 (mevedel-reminders--entry-label '(specialist . read)))))

(mevedel-deftest mevedel-reminders-rearm-plan-reference
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "clears only the plan-reference reminder's fired mark"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (plan-ref (mevedel-reminder-create
                    :type 'plan-reference :interval 'one-shot
                    :trigger (lambda (_) t) :content (lambda (_) "plan")))
         (other (mevedel-reminder-create
                 :type 'other :interval 'one-shot
                 :trigger (lambda (_) t) :content (lambda (_) "other"))))
    (setf (mevedel-reminder-last-fired plan-ref) 3
          (mevedel-reminder-last-fired other) 3)
    (mevedel-session-add-reminder session plan-ref)
    (mevedel-session-add-reminder session other)
    (mevedel-reminders-rearm-plan-reference session)
    (should-not (mevedel-reminder-last-fired plan-ref))
    (should (= 3 (mevedel-reminder-last-fired other)))
    ;; A reset one-shot may fire again.
    (should (mevedel-reminders--should-fire-p plan-ref 4 session))))

(mevedel-deftest mevedel-reminders-make-fork-provenance
  ()
  ,test
  (test)
  :doc "fires sparsely for forks and never for ordinary sessions"
  (let ((reminder (mevedel-reminders-make-fork-provenance)))
    (should (eq 'fork-provenance (mevedel-reminder-type reminder)))
    (should (equal 20 (mevedel-reminder-interval reminder)))
    (should-not (funcall (mevedel-reminder-trigger reminder)
                         (mevedel-session--create :name "main")))
    (should (funcall (mevedel-reminder-trigger reminder)
                     (mevedel-session--create
                      :name "fork" :forked-from-session-id "src")))))

(mevedel-deftest mevedel-reminders--injection-record
  ()
  ,test
  (test)
  :doc "captures phase and every entry's type and body"
  (should (equal '(:type injected-reminders
                   :phase turn-start
                   :items ((:type a :body "one") (:type b :body "two")))
                 (mevedel-reminders--injection-record
                  '((:type a :body "one") (:type b :body "two"))
                  'turn-start)))
  :doc "retains complete large UTF-8 bodies for later reconstruction"
  (let* ((body (concat (make-string 20000 ?x) "€ complete tail"))
         (record (mevedel-reminders--injection-record
                  (list (list :type 'big :body body)) 'mid-turn)))
    (should (equal body (plist-get (car (plist-get record :items)) :body)))))

(mevedel-deftest mevedel-reminders--write-injection-record
  ()
  ,test
  (test)
  :doc "writes a hidden audit block at the marker and advances it"
  (with-temp-buffer
    (insert "user prompt\n")
    (let* ((marker (point-marker))
           (info (list :position marker))
           (entries '((:type demo :body "REMIND"))))
      (mevedel-reminders--write-injection-record
       info (current-buffer) entries 'turn-start)
      (let ((spans (mevedel-transcript-audit-spans (buffer-string))))
        (should (= 1 (length spans)))
        (should (equal '(:type injected-reminders
                         :phase turn-start
                         :items ((:type demo :body "REMIND")))
                       (plist-get (car spans) :record))))
      ;; Later response insertion at the marker lands after the record.
      (should (= (marker-position marker) (point-max)))
      (should-not (string-match-p "<system-reminder>" (buffer-string)))))
  :doc "rejects delivery when no live transcript marker exists"
  (with-temp-buffer
    (insert "user prompt\n")
    (should-error
     (mevedel-reminders--write-injection-record
      (list :position nil) (current-buffer)
      '((:type demo :body "REMIND")) 'mid-turn))
    (should (equal "user prompt\n" (buffer-string)))))

(mevedel-deftest mevedel-reminders-stage-entry
  ()
  ,test
  (test)
  :doc "appends entries and commits to the FSM info in call order"
  (let* ((fsm (gptel-make-fsm :info nil))
         (ran nil))
    (mevedel-reminders-stage-entry fsm 'first "one")
    (mevedel-reminders-stage-entry fsm 'second "two"
                                   (lambda () (setq ran t)))
    (let ((info (gptel-fsm-info fsm)))
      (should (equal '((:type first :body "one")
                       (:type second :body "two"))
                     (plist-get info :mevedel-reminder-entries)))
      (should (= 1 (length (plist-get info :mevedel-reminder-commits))))
      (funcall (car (plist-get info :mevedel-reminder-commits)))
      (should ran)))
  :doc "ignores an empty or non-string body"
  (let ((fsm (gptel-make-fsm :info nil)))
    (mevedel-reminders-stage-entry fsm 'noop "")
    (mevedel-reminders-stage-entry fsm 'noop nil)
    (should-not (plist-get (gptel-fsm-info fsm)
                           :mevedel-reminder-entries))))

(mevedel-deftest mevedel-reminders-stage-commit
  (:doc "appends only function commits to the FSM info in call order")
  (let* ((fsm (gptel-make-fsm :info nil))
         (first (lambda () 'first))
         (second (lambda () 'second)))
    (mevedel-reminders-stage-commit fsm first)
    (mevedel-reminders-stage-commit fsm 'not-a-function)
    (mevedel-reminders-stage-commit fsm second)
    (should (equal (list first second)
                   (plist-get (gptel-fsm-info fsm)
                              :mevedel-reminder-commits)))))


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
              '((:type demo :body "REMIND"))
              (plist-get (gptel-fsm-info fsm)
                         :mevedel-reminder-entries)))))
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

(mevedel-deftest mevedel-reminders--agent-transform
  ()
  ,test
  (test)
  :doc "stages the invocation's reminder roster with the invocation as context"
  (let* ((seen-ctx nil)
         (agent-buf (generate-new-buffer " *mevedel-test-agent*"))
         (inv (mevedel-agent-invocation--create
               :turn-count 3
               :reminders
               (list (mevedel-reminder-create
                      :type 'agent-demo
                      :trigger (lambda (_) t)
                      :content (lambda (ctx)
                                 (setq seen-ctx ctx)
                                 "AGENT REMIND")))))
         (fsm (gptel-make-fsm :info (list :buffer agent-buf))))
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (setq-local mevedel--agent-invocation inv))
          (mevedel-reminders--agent-transform fsm)
          (should (eq seen-ctx inv))
          (should (equal '((:type agent-demo :body "AGENT REMIND"))
                         (plist-get (gptel-fsm-info fsm)
                                    :mevedel-reminder-entries)))
          ;; The fired mark is a deferred commit, exactly as on the
          ;; session path.
          (let ((reminder (car (mevedel-agent-invocation-reminders inv))))
            (should-not (mevedel-reminder-last-fired reminder))
            (mapc #'funcall (plist-get (gptel-fsm-info fsm)
                                       :mevedel-reminder-commits))
            (should (= 3 (mevedel-reminder-last-fired reminder)))))
      (kill-buffer agent-buf)))
  :doc "is a no-op for a buffer without an agent invocation"
  (let* ((buf (generate-new-buffer " *mevedel-test-agent*"))
         (fsm (gptel-make-fsm :info (list :buffer buf))))
    (unwind-protect
        (progn
          (mevedel-reminders--agent-transform fsm)
          (should-not (plist-get (gptel-fsm-info fsm)
                                 :mevedel-reminder-entries)))
      (kill-buffer buf))))

(mevedel-deftest mevedel-reminders--handle-inject
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "appends retained reminders after the untouched current user message"
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
                           :mevedel-reminder-entries
                           '((:type one :body "one")
                             (:type two :body "two"))))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session))
            (insert "actual task\n")
            (setf (gptel-fsm-info fsm)
                  (plist-put (gptel-fsm-info fsm)
                             :position (point-marker))))
          (mevedel-reminders--handle-inject fsm)
          (let ((messages (plist-get data :messages)))
            (should (= 3 (length messages)))
            (should (equal "actual task"
                           (plist-get (aref messages 1) :content)))
            (should
             (equal
              "<system-reminder>\none\n</system-reminder>\n<system-reminder>\ntwo\n</system-reminder>"
              (plist-get (aref messages 2) :content))))
          (should-not (plist-get (gptel-fsm-info fsm)
                                 :mevedel-reminder-entries))
          ;; The chat buffer records what was injected as one hidden
          ;; audit block; the blocks themselves never touch it.
          (with-current-buffer chat-buf
            (let ((spans (mevedel-transcript-audit-spans (buffer-string))))
              (should (= 1 (length spans)))
              (should (equal '(:type injected-reminders
                               :phase turn-start
                               :items ((:type one :body "one")
                                       (:type two :body "two")))
                             (plist-get (car spans) :record))))
            (should-not (string-match-p "<system-reminder>"
                                        (buffer-string))))
          (mevedel-reminders--handle-inject fsm)
          (should (= 3 (length (plist-get data :messages))))
          ;; Idempotent for the record too.
          (with-current-buffer chat-buf
            (should (= 1 (length (mevedel-transcript-audit-spans
                                  (buffer-string)))))))
      (kill-buffer chat-buf)))

  :doc "keeps consumable state when injection fails"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (prompt-buf (generate-new-buffer " *mevedel-test-prompt*"))
         (backend (gptel-make-openai
                   "reminder-fail" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data)))
         (reminder (mevedel-reminders-make-pending-events)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session))
            (setf (mevedel-session-pending-reminders session) '("EVENT"))
            (setf (mevedel-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK")))
            (mevedel-session-add-reminder session reminder)
            (mevedel-reminders-queue-turn-event
             chat-buf 'demo "TURN"))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (mevedel-reminders--transform fsm))
          (cl-letf (((symbol-function 'gptel--inject-prompt)
                     (lambda (&rest _) (error "Injection failed"))))
            (should-error (mevedel-reminders--handle-inject fsm)))
          ;; A successful payload injection without a transcript marker must
          ;; roll back too, preserving both payload and pending state.
          (should-error (mevedel-reminders--handle-inject fsm))
          (should (equal data '(:messages [(:role "user" :content "actual task")])))
          ;; Nothing reached the model, so nothing may be consumed.
          (should (equal '("EVENT")
                         (mevedel-session-pending-reminders session)))
          (should-not (mevedel-reminder-last-fired reminder))
          (with-current-buffer chat-buf
            (should (plist-get mevedel-reminders--turn-events :items)))
          ;; Hook context is reserved for the in-flight request rather
          ;; than left pending, so a mid-request taker cannot deliver it
          ;; twice; settling the dead turn returns it.
          (should-not (mevedel-session-hook-context-pending session))
          (should (mevedel-reminders-restore-reserved-context chat-buf))
          (should (mevedel-session-hook-context-pending session)))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "consumes staged state exactly once when injection succeeds"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (prompt-buf (generate-new-buffer " *mevedel-test-prompt*"))
         (backend (gptel-make-openai
                   "reminder-once" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data
                           :position (with-current-buffer chat-buf (point-marker)))))
         (reminder (mevedel-reminders-make-pending-events)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session))
            (setf (mevedel-session-pending-reminders session) '("EVENT"))
            (setf (mevedel-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK")))
            (mevedel-session-add-reminder session reminder)
            (mevedel-reminders-queue-turn-event chat-buf 'demo "TURN"))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (mevedel-reminders--transform fsm))
          (mevedel-reminders--handle-inject fsm)
          (should-not (mevedel-session-pending-reminders session))
          (should-not (mevedel-session-hook-context-pending session))
          (should (mevedel-reminder-last-fired reminder))
          (with-current-buffer chat-buf
            (should-not (plist-get mevedel-reminders--turn-events :items)))
          (let ((count (length (plist-get data :messages))))
            (mevedel-reminders--handle-inject fsm)
            (should (= count (length (plist-get data :messages))))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "a dead turn returns the hook context it reserved"
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
                  '((:event "SessionStart" :body "HOOK"))))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (mevedel-reminders--transform fsm))
          (should-not (mevedel-session-hook-context-pending session))
          ;; The turn dies without delivering, and every dead turn ends its
          ;; request, so that teardown hands the reservation back.
          (with-current-buffer chat-buf
            (mevedel-request-end))
          (should (equal '((:event "SessionStart" :body "HOOK"))
                         (mevedel-session-hook-context-pending session)))
          ;; And only once: a second teardown adds nothing.
          (with-current-buffer chat-buf
            (mevedel-request-end))
          (should (= 1 (length
                        (mevedel-session-hook-context-pending session)))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "consumes hook context even when no reminder block fires"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (prompt-buf (generate-new-buffer " *mevedel-test-prompt*"))
         (backend (gptel-make-openai
                   "reminder-context" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session))
            (setf (mevedel-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK"))))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (mevedel-reminders--transform fsm)
            ;; The context rides the prompt text, so it contributes no
            ;; block, and its commit must still run.
            (should (string-match-p "HOOK" (buffer-string))))
          (should-not (plist-get (gptel-fsm-info fsm)
                                 :mevedel-reminder-entries))
          (mevedel-reminders--handle-inject fsm)
          (should-not (mevedel-session-hook-context-pending session)))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

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
                           :position (with-current-buffer chat-buf (point-marker))
                           :mevedel-reminder-entries
                           '((:type ctx :body "context"))))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (mevedel-reminders--handle-inject fsm)
          (let* ((contents (plist-get data :contents))
                 (reminder (aref contents 2)))
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
               :info (list :buffer chat-buf :backend backend :data data
                           :position (with-current-buffer chat-buf (point-marker)))))
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
  (:before-each (mevedel-test--capture-agent-registry)
   :after-each (mevedel-test--restore-agent-registry))
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
    (test-mevedel-reminders--collect-and-commit
     (mevedel-agent-invocation-reminders inv-a) 0 inv-a)
    (should (equal 0 (mevedel-reminder-last-fired clone-a)))
    (should (null (mevedel-reminder-last-fired clone-b))))

  :doc "role names do not impose undeclared reminder policy"
  (dolist (name '("reviewer" "verifier" "custom"))
    (let* ((agent (mevedel-agent--create :name name :tools nil))
           (inv (mevedel-agent-invocation-create agent)))
      (should-not (mevedel-agent-invocation-reminders inv)))))









;;
;;; Tier 1 built-in reminders

(mevedel-deftest mevedel-reminders-make-mode-constraints
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when session permission mode is `ask'"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-mode-constraints)))
    (setf (mevedel-session-permission-mode session) 'ask)
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
      (dolist (text '("Eval is unavailable"
                      "session-owned work:// descendants"
                      "directive Planning permits no file writes"
                      "Bash is limited"
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
  (:before-each (mevedel-test--capture-agent-registry)
   :after-each (mevedel-test--restore-agent-registry))
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


(mevedel-deftest mevedel-reminders-make-verification-suggestion
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not suggest implementation verification after reading a file"
  (let* ((file (make-temp-file "mevedel-verifier-read-" nil ".el" "(provide 'fixture)\n"))
         (session (mevedel-session--create
                   :turn-count 1 :touched-files (make-hash-table :test #'equal)))
         (reminder (mevedel-reminders-make-verification-suggestion)))
    (unwind-protect
        (progn
          (mevedel-session-record-interaction session file 'read 0)
          (should-not (mevedel-reminders--should-fire-p reminder 1 session)))
      (delete-file file)))

  :doc "does not revive old modifications during later read-only turns"
  (let* ((session (mevedel-session--create
                   :turn-count 12 :touched-files (make-hash-table :test #'equal)))
         (reminder (mevedel-reminders-make-verification-suggestion)))
    (mevedel-session-record-interaction session "/tmp/example.el" 'modify 1)
    (mevedel-session-record-interaction session "/tmp/example.el" 'read 12)
    (should-not (mevedel-reminders--should-fire-p reminder 12 session))
    (mevedel-session-record-interaction session "/tmp/example.el" 'modify 12)
    (should (mevedel-reminders--should-fire-p reminder 12 session)))

  :doc "does not fire when session has no touched files"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (should-not (funcall (mevedel-reminder-trigger r) session)))

  :doc "fires after the latest turn modified a file"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (mevedel-session-record-interaction session "/tmp/example.el" 'modify 0)
    (should (funcall (mevedel-reminder-trigger r) session))
    (should (string-match-p "verifier"
                            (funcall (mevedel-reminder-content r) session))))

  :doc "keeps generic verification guidance for recent edits after plan verification"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (setf (mevedel-session-plan-metadata session)
          '(:status accepted :verification-pending nil))
    (mevedel-session-record-interaction session "/tmp/example.el" 'modify 0)
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
    (mevedel-session-record-interaction session "/tmp/example.el" 'modify 0)
    (dolist (status '(rejected cancelled presented))
      (setf (mevedel-session-plan-metadata session)
            (list :status status :verification-pending t))
      (should (funcall (mevedel-reminder-trigger r) session))
      (should-not (string-match-p
                   "accepted plan"
                   (funcall (mevedel-reminder-content r) session)))))

  :doc "adds accepted-plan guidance after a recent modification"
  (let* ((tmp (make-temp-file "mevedel-vs-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-verification-suggestion)))
    (setf (mevedel-session-plan-metadata session)
          '(:status accepted :verification-pending t))
    (mevedel-session-record-interaction session "/tmp/example.el" 'modify 0)
    (should (funcall (mevedel-reminder-trigger r) session))
    (should (string-match-p "plan"
                            (funcall (mevedel-reminder-content r) session)))))


(mevedel-deftest mevedel-reminders-make-user-revised-patch
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "fires once, names the revised changes, and round-trips its recipe"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/urp/" "/tmp/urp/" "urp"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-user-revised-patch
             "one.txt hunk 2, fresh.el (new file content)")))
    (should (mevedel-reminders--should-fire-p r 0 session))
    (let ((content (funcall (mevedel-reminder-content r) session)))
      (should (string-search "one.txt hunk 2" content))
      (should (string-search "Do not revert" content)))
    (setf (mevedel-reminder-last-fired r) 0)
    (should-not (mevedel-reminders--should-fire-p r 1 session))
    (let* ((recipe (mevedel-reminder-recipe r))
           (restored (car (mevedel-reminders-restore-agent-templates
                           (list recipe)))))
      (should (eq (mevedel-reminder-type restored) 'user-revised-patch))
      (should (equal recipe (mevedel-reminder-recipe restored))))))


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
               "work://plans/accepted-20260813-120000.md"
               content))
      (should-not (string-match-p "local/plans/current.md" content))
      (should-not (string-match-p "work://plans/current.md" content))
      (should-not (string-match-p (regexp-quote tmp) content))
      (should (string-match-p "# Accepted" content))
      (should-not (string-match-p "# Current draft" content)))
    ;; An active Goal suppresses this reminder only when it already carries
    ;; the exact accepted plan reference.
    (setf (mevedel-session-goal session)
          (mevedel-goal--create :status 'active))
    (should (funcall (mevedel-reminder-trigger r) session))
    (setf (mevedel-goal-plan-reference (mevedel-session-goal session))
          "local/plans/other.md")
    (should (funcall (mevedel-reminder-trigger r) session))
    (setf (mevedel-goal-plan-reference (mevedel-session-goal session))
          "local/plans/accepted-20260813-120000.md")
    (should-not (funcall (mevedel-reminder-trigger r) session)))

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
                'mevedel-session-artifacts-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-session-artifacts-read-artifact)
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
      ;; The nudge sends the model the shape TaskList returns, not the
      ;; panel rendering, so display decisions cannot alter it.
      (should (string-match-p "#1 \\[in_progress\\] main work" content))
      (should (string-match-p
               "#2 \\[pending\\] agent work owner=worker" content))
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
          (let ((content (test-mevedel-reminders--content r session)))
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
          (let ((content (test-mevedel-reminders--content r session)))
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
          (test-mevedel-reminders--content r session)
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
          (let ((content (test-mevedel-reminders--content r session)))
            (should (string-match-p "more lines truncated" content))))
      (delete-directory tmp-root t)))

  :doc "reports a fingerprint-only cache entry without a diff"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "big.log" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file))
         (mevedel-file-cache-max-file-bytes 1024))
    (unwind-protect
        (progn
          (with-temp-file file (insert (make-string 4096 ?x)))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert (make-string 8192 ?y)))
            (set-file-times file future))
          (should (mevedel-reminders--should-fire-p r 0 session))
          (let ((content (test-mevedel-reminders--content r session)))
            (should (string-match-p "MODIFIED:" content))
            (should (string-match-p "no diff available" content))
            ;; Neither side of the change was ever read, so no file body
            ;; can have reached the reminder.
            (should-not (string-match-p "xxxx" content))
            (should-not (string-match-p "yyyy" content))))
      (delete-directory tmp-root t)))

  :doc "reports without a diff once content passes the diff byte cap"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "mevedel-ef-" t)))
         (file (expand-file-name "wide.txt" tmp-root))
         (ws (mevedel-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-edited-file))
         (mevedel-reminders-edited-file-max-diff-bytes 512))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (mevedel-file-cache-put
           (mevedel-workspace-file-cache ws)
           (mevedel-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert (make-string 4096 ?z)))
            (set-file-times file future))
          (let ((content (test-mevedel-reminders--content r session)))
            (should (string-match-p "no diff available" content))
            (should-not (string-match-p "zzzz" content))))
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
            (test-mevedel-reminders--content r session)
            (should (= 1 call-count))
            ;; A fresh trigger on the next turn recomputes; no changes
            ;; this time, so content is not called.
            (setq call-count 0)
            (should-not (mevedel-reminders--should-fire-p r 1 session))
            (should (= 1 call-count))))
      (delete-directory tmp-root t))))


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
                   (test-mevedel-reminders--content r session)))
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
    (let ((body (test-mevedel-reminders--content r session)))
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
          (let* ((result (funcall (mevedel-reminder-content r) session))
                 (body (plist-get result :body))
                 (commit (plist-get result :commit)))
            (should (string-match-p "Added:" body))
            (should (string-match-p "verifier" body))
            (should (string-match-p "Removed:" body))
            (should (string-match-p "explorer" body))
            ;; The commit runs outside the transform's buffer binding, so
            ;; it must store the roster the content captured rather than
            ;; re-read it and find nothing.
            (setq body nil)
            (let ((mevedel-reminders--current-chat-buffer nil))
              (funcall commit))
            (should (equal '(("verifier" . "Verify"))
                           (mevedel-session-agent-types-snapshot session)))))
      (kill-buffer buf))))


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
      (should-not (memq 'date-change types))
      (should (memq 'compaction-available types))
      (should (memq 'token-usage types))
      (should (memq 'agent-listing-delta types))
      (should (memq 'mode-constraints types))
      (should (memq 'plan-mode types))
      (should-not (memq 'diagnostics types))
      (should (memq 'edited-file types))
      (should-not (memq 'tool-discovery types))
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
           (mode-count (cl-count 'mode-constraints types))
           (plan-mode-count (cl-count 'plan-mode types))
           (edit-count (cl-count 'edited-file types))
           (nudge-count (cl-count 'task-nudge types))
           (plan-count (cl-count 'plan-reference types)))
      (should (= 1 pending-count))
      (should (= 0 date-count))
      (should (= 1 compact-count))
      (should (= 1 token-count))
      (should (= 1 agent-count))
      (should (= 1 mode-count))
      (should (= 1 plan-mode-count))
      (should (= 1 edit-count))
      (should (= 1 nudge-count))
      (should (= 1 plan-count)))))


(mevedel-deftest mevedel-agent-invocation-create/max-turns
  (:before-each (mevedel-test--capture-agent-registry)
   :after-each (mevedel-test--restore-agent-registry))
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
