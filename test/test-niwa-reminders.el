;;; test-niwa-reminders.el --- Tests for niwa-reminders.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-hooks)

(require 'gptel)
(require 'gptel-gemini)
(require 'gptel-openai-responses)
(require 'gptel-request)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'niwa-permissions)
(require 'niwa-permission-review)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-file-state)
(require 'niwa-tool-fs)
(require 'niwa-compact)
(require 'niwa-reminders)
(require 'niwa-turn)
(require 'niwa-system)
(require 'niwa-agents)
(require 'niwa-tool-ui)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun test-niwa-reminders--prepared-content (reminder session)
  "Return REMINDER's body for SESSION inside a prepared dispatch.
The `edited-file' reminder reports diffs only for changes whose diffs
`niwa-reminders-prepare-edited-files' prepared before staging."
  (unless (memq reminder (niwa-session-reminders session))
    (niwa-session-add-reminder session reminder))
  (with-temp-buffer
    (setq-local niwa--session session)
    (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer))))
          content ran)
      (niwa-reminders-prepare-edited-files
       fsm (lambda (_)
             (setq content (test-niwa-reminders--content reminder session)
                   ran t)))
      (with-timeout (10 (ert-fail "Edited-file diffs were never prepared"))
        (while (not ran) (accept-process-output nil 0.02)))
      content)))

(defun test-niwa-reminders--content (reminder ctx)
  "Return REMINDER's body for CTX, applying any staged commit.
Content returns either a body string or a `:body'/`:commit' plist, so
this collapses both shapes to the delivered text."
  (let ((result (funcall (niwa-reminder-content reminder) ctx)))
    (if (stringp result)
        result
      (when-let* ((commit (plist-get result :commit)))
        (funcall commit))
      (plist-get result :body))))

(defun test-niwa-reminders--collect-and-commit (reminders turn ctx)
  "Collect REMINDERS at TURN for CTX, run their commits, return the blocks."
  (let ((staged (niwa-reminders--collect-from reminders turn ctx)))
    (mapc #'funcall (plist-get staged :commits))
    (mapcar (lambda (entry)
              (niwa-reminders-format-block (plist-get entry :body)))
            (plist-get staged :entries))))

(defvar imenu--index-alist)
(defvar niwa--agent-invocation)

;; Declared special here so `let'-binding stays dynamic even when no
;; loaded module has defined the variable with a value yet.
(defvar niwa--agent-invocation)


;;
;;; Turn ownership

(niwa-deftest niwa-reminders-turn-owner ()
  ,test
  (test)
  :doc "returns the request in a fresh Emacs that has not loaded agents"
  (with-temp-buffer
    (should
     (zerop
      (call-process
       (expand-file-name invocation-name invocation-directory) nil t nil
       "--batch" "-Q" "--eval"
       (prin1-to-string
        `(progn
           (setq load-path ',load-path)
           (require 'niwa-reminders)
           (with-temp-buffer
             (setq-local niwa--current-request 'request)
             (unless (eq 'request (niwa-reminders-turn-owner))
               (error "Lost the request owner")))
           (when (featurep 'niwa-agents)
             (error "Loaded the agents module")))))))
    (should (equal "" (buffer-string))))

  :doc "prefers an unsettled agent invocation over the request"
  (with-temp-buffer
    (let ((invocation (niwa-agent-invocation--create)))
      (setq-local niwa--current-request (niwa-request--create :id "owner")
                  niwa--agent-invocation invocation)
      (should (eq invocation (niwa-reminders-turn-owner)))
      (setf (niwa-agent-invocation-runtime-settled-p invocation) t)
      (should (eq niwa--current-request (niwa-reminders-turn-owner))))))


;;
;;; Struct creation

(niwa-deftest niwa-reminder-create
  ()
  ,test
  (test)
  :doc "`niwa-reminder-create' populates all slots from keyword args"
  (let ((r (niwa-reminder-create
            :type 'demo
            :trigger (lambda (_) t)
            :content (lambda (_) "hello")
            :interval 3
            :recipe '(pending-events))))
    (should (eq 'demo (niwa-reminder-type r)))
    (should (functionp (niwa-reminder-trigger r)))
    (should (functionp (niwa-reminder-content r)))
    (should (equal 3 (niwa-reminder-interval r)))
    (should (null (niwa-reminder-last-fired r)))
    (should (equal '(pending-events)
                   (niwa-reminder-recipe r))))

  :doc "`niwa-reminder-create' accepts nil interval"
  (let ((r (niwa-reminder-create
            :type 'demo
            :trigger (lambda (_) t)
            :content (lambda (_) "x"))))
    (should (null (niwa-reminder-interval r))))

  :doc "`niwa-reminder-create' rejects non-symbol type"
  (should-error (niwa-reminder-create
                 :type "not-a-symbol"
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")))

  :doc "`niwa-reminder-create' rejects non-function trigger"
  (should-error (niwa-reminder-create
                 :type 'demo
                 :trigger "not-a-fn"
                 :content (lambda (_) "x")))

  :doc "`niwa-reminder-create' rejects non-function content"
  (should-error (niwa-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content "not-a-fn"))

  :doc "`niwa-reminder-create' rejects non-integer, non-symbol interval"
  (should-error (niwa-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")
                 :interval "5"))

  :doc "`niwa-reminder-create' accepts `one-shot' interval"
  (let ((r (niwa-reminder-create
            :type 'demo
            :trigger (lambda (_) t)
            :content (lambda (_) "x")
            :interval 'one-shot)))
    (should (eq 'one-shot (niwa-reminder-interval r))))

  :doc "`niwa-reminder-create' rejects unknown interval symbol"
  (should-error (niwa-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")
                 :interval 'bogus))

  :doc "`niwa-reminder-create' rejects untrusted durable recipes"
  (should-error (niwa-reminder-create
                 :type 'demo
                 :trigger (lambda (_) t)
                 :content (lambda (_) "x")
                 :recipe '(erase-buffer))))


;;
;;; Session reminder helpers

(niwa-deftest niwa-session-add-reminder
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry)
   :doc "`niwa-session-add-reminder' appends reminders in registration order")
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (r1 (niwa-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "1")))
         (r2 (niwa-reminder-create :type 'b
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "2"))))
    (niwa-session-add-reminder session r1)
    (niwa-session-add-reminder session r2)
    (should (equal (list r1 r2) (niwa-session-reminders session)))))

(niwa-deftest niwa-session-remove-reminder
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry)
   :doc "`niwa-session-remove-reminder' removes all reminders of a type")
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (r1 (niwa-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "1")))
         (r2 (niwa-reminder-create :type 'b
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "2")))
         (r3 (niwa-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "3"))))
    (niwa-session-add-reminder session r1)
    (niwa-session-add-reminder session r2)
    (niwa-session-add-reminder session r3)
    (niwa-session-remove-reminder session 'a)
    (should (equal (list r2) (niwa-session-reminders session)))))


;;
;;; Firing logic

(niwa-deftest niwa-reminders--should-fire-p
  ()
  ,test
  (test)
  :doc "fires when trigger returns non-nil and no interval set"
  (let ((r (niwa-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x"))))
    (should (niwa-reminders--should-fire-p r 0 nil)))

  :doc "does not fire when trigger returns nil"
  (let ((r (niwa-reminder-create :type 'a
                                    :trigger (lambda (_) nil)
                                    :content (lambda (_) "x"))))
    (should-not (niwa-reminders--should-fire-p r 0 nil)))

  :doc "fires on first check even with interval (last-fired is nil)"
  (let ((r (niwa-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 5)))
    (should (niwa-reminders--should-fire-p r 0 nil)))

  :doc "does not fire before interval elapses"
  (let ((r (niwa-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 5)))
    (setf (niwa-reminder-last-fired r) 0)
    (should-not (niwa-reminders--should-fire-p r 3 nil)))

  :doc "fires again after interval elapses"
  (let ((r (niwa-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 5)))
    (setf (niwa-reminder-last-fired r) 0)
    (should (niwa-reminders--should-fire-p r 5 nil)))

  :doc "trigger receives the provided context object"
  (let* ((seen nil)
         (ctx (list 'ctx-marker))
         (r (niwa-reminder-create
             :type 'a
             :trigger (lambda (c) (setq seen c) t)
             :content (lambda (_) "x"))))
    (niwa-reminders--should-fire-p r 0 ctx)
    (should (eq seen ctx)))

  :doc "`one-shot' interval fires the first time"
  (let ((r (niwa-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 'one-shot)))
    (should (niwa-reminders--should-fire-p r 0 nil)))

  :doc "`one-shot' interval never fires again after last-fired is set"
  (let ((r (niwa-reminder-create :type 'a
                                    :trigger (lambda (_) t)
                                    :content (lambda (_) "x")
                                    :interval 'one-shot)))
    (setf (niwa-reminder-last-fired r) 0)
    (should-not (niwa-reminders--should-fire-p r 1 nil))
    (should-not (niwa-reminders--should-fire-p r 9999 nil))))


;;
;;; Clone helpers

(niwa-deftest niwa-reminder-clone
  ()
  ,test
  (test)
  :doc "`niwa-reminder-clone' copies all slots and resets last-fired"
  (let* ((trigger (lambda (_) t))
         (content (lambda (_) "x"))
         (r (niwa-reminder-create
             :type 'a :trigger trigger :content content :interval 7
             :recipe '(pending-events)))
         (_  (setf (niwa-reminder-last-fired r) 42
                    (niwa-reminder-mode-constraints-observation r) "delivered body"))
         (clone (niwa-reminder-clone r)))
    (should (eq 'a (niwa-reminder-type clone)))
    (should (eq trigger (niwa-reminder-trigger clone)))
    (should (eq content (niwa-reminder-content clone)))
    (should (equal 7 (niwa-reminder-interval clone)))
    (should (null (niwa-reminder-last-fired clone)))
    (should-not (niwa-reminder-mode-constraints-observation clone))
    (should (equal '(pending-events)
                   (niwa-reminder-recipe clone)))
    (should-not (eq r clone))
    (setcar (niwa-reminder-recipe clone) 'date-change)
    (should (equal '(pending-events) (niwa-reminder-recipe r)))
    (should (= 42 (niwa-reminder-last-fired r))))

  :doc "`niwa-reminder-clone' produces independent last-fired state"
  (let* ((r (niwa-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "x")))
         (clone (niwa-reminder-clone r)))
    (setf (niwa-reminder-last-fired clone) 9)
    (should (equal 9 (niwa-reminder-last-fired clone)))
    (should (null (niwa-reminder-last-fired r)))))

(niwa-deftest niwa-reminders-clone-list
  ()
  ,test
  (test)
  :doc "`niwa-reminders-clone-list' clones every reminder"
  (let* ((r1 (niwa-reminder-create
              :type 'a :trigger (lambda (_) t) :content (lambda (_) "1")))
         (r2 (niwa-reminder-create
              :type 'b :trigger (lambda (_) t) :content (lambda (_) "2")))
         (clones (niwa-reminders-clone-list (list r1 r2))))
    (should (= 2 (length clones)))
    (should-not (eq r1 (nth 0 clones)))
    (should-not (eq r2 (nth 1 clones)))
    (should (eq 'a (niwa-reminder-type (nth 0 clones))))
    (should (eq 'b (niwa-reminder-type (nth 1 clones)))))

  :doc "clones track last-fired independently of the templates"
  (let* ((r (niwa-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "x")
             :interval 3))
         (clone-a (car (niwa-reminders-clone-list (list r))))
         (clone-b (car (niwa-reminders-clone-list (list r)))))
    (test-niwa-reminders--collect-and-commit (list clone-a) 0 nil)
    (should (equal 0 (niwa-reminder-last-fired clone-a)))
    (should (null (niwa-reminder-last-fired clone-b)))
    (should (null (niwa-reminder-last-fired r)))))

(niwa-deftest niwa-reminders--recipe-p
  ()
  ,test
  (test)
  :doc "accepts only trusted constructors with valid argument contracts"
  (should (niwa-reminders--recipe-p '(mode-constraints)))
  (should-not (niwa-reminders--recipe-p '(mode-constraints 9)))
  (should (niwa-reminders--recipe-p '(pending-events)))
  (should-not (niwa-reminders--recipe-p '(unknown-recipe)))
  (should-not (niwa-reminders--recipe-p '(mode-constraints . 9)))
  (should-not (niwa-reminders--recipe-p '(pending-events 1)))
  (should-not (niwa-reminders--recipe-p '(mode-constraints "often")))
  (should-not (niwa-reminders--recipe-p '(max-turns-warning 2.0)))
  (should-not (niwa-reminders--recipe-p '(token-usage 0.9 -1)))
  (should-not
   (niwa-reminders--recipe-p
    `(mode-constraints ,(lambda () t)))))

(niwa-deftest niwa-reminders--recipe-arguments-p
  ()
  ,test
  (test)
  :doc "validates every declarative reminder argument contract"
  (should (niwa-reminders--recipe-arguments-p 'no-args nil))
  (should (niwa-reminders--recipe-arguments-p 'interval '(nil)))
  (should (niwa-reminders--recipe-arguments-p 'interval '(0)))
  (should (niwa-reminders--recipe-arguments-p 'ratio '(0.75)))
  (should (niwa-reminders--recipe-arguments-p 'count '(40)))
  (should (niwa-reminders--recipe-arguments-p
           'token-usage '(0.9 4)))
  (should-not (niwa-reminders--recipe-arguments-p 'no-args '(1)))
  (should-not (niwa-reminders--recipe-arguments-p 'interval '(-1)))
  (should-not (niwa-reminders--recipe-arguments-p 'ratio '(1.1)))
  (should-not (niwa-reminders--recipe-arguments-p 'count '(4.5)))
  (should-not (niwa-reminders--recipe-arguments-p
               'token-usage '(0.9)))
  (should-not (niwa-reminders--recipe-arguments-p 'unknown nil)))

(niwa-deftest niwa-reminders-serialize-agent-templates
  ()
  ,test
  (test)
  :doc "serializes trusted recipes and rejects closure-only templates"
  (let* ((mode (niwa-reminders-make-mode-constraints))
         (_ (setf (niwa-reminder-mode-constraints-observation mode) "delivered body"))
         (recipes
          (niwa-reminders-serialize-agent-templates
           (list mode (niwa-reminders-make-pending-events)))))
    (should (equal '((mode-constraints) (pending-events)) recipes))
    (should-not (niwa-reminder-mode-constraints-observation
                 (car (niwa-reminders-restore-agent-templates recipes)))))
  (should-error
   (niwa-reminders-serialize-agent-templates
    (list (niwa-reminder-create
           :type 'runtime-only
           :trigger (lambda (_) t)
           :content (lambda (_) "runtime"))))))

(niwa-deftest niwa-reminders-restore-agent-templates
  ()
  ,test
  (test)
  :doc "rebuilds frozen templates only through trusted recipe factories"
  (let ((restored
         (niwa-reminders-restore-agent-templates
          '((mode-constraints) (pending-events)))))
    (should (equal '(mode-constraints pending-events)
                   (mapcar #'niwa-reminder-type restored)))
    (should-not (niwa-reminder-interval (car restored))))
  (should-error
   (niwa-reminders-restore-agent-templates
    '((erase-buffer))))
  (should-error
   (niwa-reminders-restore-agent-templates
    '((pending-events 1))))
  (should-error
   (niwa-reminders-restore-agent-templates
    '((max-turns-warning 4.0))))
  (should-error
   (niwa-reminders-restore-agent-templates
    `((mode-constraints ,(lambda () t))))))


;;
;;; collect-from

(niwa-deftest niwa-reminders--collect-from
  ()
  ,test
  (test)
  :doc "`niwa-reminders--collect-from' collects all firing reminders"
  (let* ((r1 (niwa-reminder-create
              :type 'a :trigger (lambda (_) t) :content (lambda (_) "one")))
         (r2 (niwa-reminder-create
              :type 'b :trigger (lambda (_) t) :content (lambda (_) "two")))
         (staged (niwa-reminders--collect-from (list r1 r2) 4 nil)))
    (should (equal '((:type a :body "one")
                     (:type b :body "two"))
                   (plist-get staged :entries)))
    ;; Firing is staged, not applied, until the payload carries it.
    (should-not (niwa-reminder-last-fired r1))
    (mapc #'funcall (plist-get staged :commits))
    (should (equal 4 (niwa-reminder-last-fired r1)))
    (should (equal 4 (niwa-reminder-last-fired r2))))

  :doc "`niwa-reminders--collect-from' passes context through to callbacks"
  (let* ((seen-trigger nil)
         (seen-content nil)
         (ctx (list 'agent-ctx))
         (r (niwa-reminder-create
             :type 'a
             :trigger (lambda (c) (setq seen-trigger c) t)
             :content (lambda (c) (setq seen-content c) "ok"))))
    (test-niwa-reminders--collect-and-commit (list r) 0 ctx)
    (should (eq seen-trigger ctx))
    (should (eq seen-content ctx))))


;;
;;; Block formatting

(niwa-deftest niwa-reminders-format-block
  (:doc "`niwa-reminders-format-block' wraps content in XML tags")
  (should (equal "<system-reminder>\nhello\n</system-reminder>"
                 (niwa-reminders-format-block "hello"))))

(niwa-deftest niwa-reminders--entry-label
  ()
  ,test
  (test)
  :doc "renders a symbol type as its name"
  (should (equal "skills-delta" (niwa-reminders--entry-label 'skills-delta)))
  :doc "renders a cons turn-event key as car:cdr"
  (should (equal "specialist:read"
                 (niwa-reminders--entry-label '(specialist . read)))))

(niwa-deftest niwa-reminders-rearm-plan-reference
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "clears only the plan-reference reminder's fired mark"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (plan-ref (niwa-reminder-create
                    :type 'plan-reference :interval 'one-shot
                    :trigger (lambda (_) t) :content (lambda (_) "plan")))
         (other (niwa-reminder-create
                 :type 'other :interval 'one-shot
                 :trigger (lambda (_) t) :content (lambda (_) "other"))))
    (setf (niwa-reminder-last-fired plan-ref) 3
          (niwa-reminder-last-fired other) 3)
    (niwa-session-add-reminder session plan-ref)
    (niwa-session-add-reminder session other)
    (niwa-reminders-rearm-plan-reference session)
    (should-not (niwa-reminder-last-fired plan-ref))
    (should (= 3 (niwa-reminder-last-fired other)))
    ;; A reset one-shot may fire again.
    (should (niwa-reminders--should-fire-p plan-ref 4 session))))

(niwa-deftest niwa-reminders-make-fork-provenance
  ()
  ,test
  (test)
  :doc "fires sparsely for forks and never for ordinary sessions"
  (let ((reminder (niwa-reminders-make-fork-provenance)))
    (should (eq 'fork-provenance (niwa-reminder-type reminder)))
    (should (equal 20 (niwa-reminder-interval reminder)))
    (should-not (funcall (niwa-reminder-trigger reminder)
                         (niwa-session--create :name "main")))
    (should (funcall (niwa-reminder-trigger reminder)
                     (niwa-session--create
                      :name "fork" :forked-from-session-id "src")))))

(niwa-deftest niwa-reminders--injection-record
  ()
  ,test
  (test)
  :doc "captures phase and every entry's type and body"
  (should (equal '(:type injected-reminders
                   :phase turn-start
                   :items ((:type a :body "one") (:type b :body "two")))
                 (niwa-reminders--injection-record
                  '((:type a :body "one") (:type b :body "two"))
                  'turn-start)))
  :doc "retains complete large UTF-8 bodies for later reconstruction"
  (let* ((body (concat (make-string 20000 ?x) "€ complete tail"))
         (record (niwa-reminders--injection-record
                  (list (list :type 'big :body body)) 'mid-turn)))
    (should (equal body (plist-get (car (plist-get record :items)) :body)))))

(niwa-deftest niwa-reminders--write-injection-record
  ()
  ,test
  (test)
  :doc "writes a hidden audit block at the marker and advances it"
  (with-temp-buffer
    (insert "user prompt\n")
    (let* ((marker (point-marker))
           (info (list :position marker))
           (entries '((:type demo :body "REMIND"))))
      (niwa-reminders--write-injection-record
       info (current-buffer) entries 'turn-start)
      (let ((spans (niwa-transcript-audit-spans (buffer-string))))
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
     (niwa-reminders--write-injection-record
      (list :position nil) (current-buffer)
      '((:type demo :body "REMIND")) 'mid-turn))
    (should (equal "user prompt\n" (buffer-string)))))

(niwa-deftest niwa-reminders--stage-batch
  (:doc "preserves collected empty bodies and defers commits in batch order")
  (let* ((fsm (gptel-make-fsm :info (list :untouched t)))
         (ran nil))
    (niwa-reminders-stage-commit fsm (lambda () (push 'first ran)))
    (niwa-reminders--stage-batch
     fsm '((:type empty :body "")) (list (lambda () (push 'second ran))))
    (niwa-reminders--stage-batch
     fsm '((:type last :body "last")) nil)
    (should-not ran)
    (should (plist-get (gptel-fsm-info fsm) :untouched))
    (should (equal '((:type empty :body "") (:type last :body "last"))
                   (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
    (mapc #'funcall (plist-get (gptel-fsm-info fsm) :niwa-reminder-commits))
    (should (equal '(second first) ran))))

(niwa-deftest niwa-reminders-stage-entry
  ()
  ,test
  (test)
  :doc "appends entries and commits to the FSM info in call order"
  (let* ((fsm (gptel-make-fsm :info nil))
         (ran nil))
    (niwa-reminders-stage-entry fsm 'first "one")
    (niwa-reminders-stage-entry fsm 'second "two"
                                   (lambda () (setq ran t)))
    (let ((info (gptel-fsm-info fsm)))
      (should (equal '((:type first :body "one")
                       (:type second :body "two"))
                     (plist-get info :niwa-reminder-entries)))
      (should (= 1 (length (plist-get info :niwa-reminder-commits))))
      (funcall (car (plist-get info :niwa-reminder-commits)))
      (should ran)))
  :doc "ignores an empty or non-string body"
  (let ((fsm (gptel-make-fsm :info nil)))
    (niwa-reminders-stage-entry fsm 'noop "")
    (niwa-reminders-stage-entry fsm 'noop nil)
    (should-not (plist-get (gptel-fsm-info fsm)
                           :niwa-reminder-entries))))

(niwa-deftest niwa-reminders-stage-commit
  (:doc "appends only function commits to the FSM info in call order")
  (let* ((fsm (gptel-make-fsm :info nil))
         (first (lambda () 'first))
         (second (lambda () 'second)))
    (niwa-reminders-stage-commit fsm first)
    (niwa-reminders-stage-commit fsm 'not-a-function)
    (niwa-reminders-stage-commit fsm second)
    (should (equal (list first second)
                   (plist-get (gptel-fsm-info fsm)
                              :niwa-reminder-commits)))))


;;
;;; Transform (integration with chat buffer)

(niwa-deftest niwa-reminders--transform
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "`niwa-reminders--transform' leaves user text intact and stages blocks"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (prompt-buf (generate-new-buffer " *niwa-test-prompt*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setf (niwa-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK")))
            (niwa-session-add-reminder
             session
             (niwa-reminder-create
              :type 'demo
              :trigger (lambda (_) t)
              :content (lambda (_) "REMIND"))))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (niwa-reminders-stage-entry
             fsm 'earlier "EARLIER"
             (lambda ()
               (with-current-buffer chat-buf
                 (should niwa-reminders--reserved-hook-context))))
            (niwa-reminders--transform fsm)
            (should
             (equal
              "\n<hook-context>\n<hook-event name=\"SessionStart\">\nHOOK\n</hook-event>\n</hook-context>\nuser prompt body"
              (buffer-string)))
            (should
             (equal
              '((:type earlier :body "EARLIER")
                (:type demo :body "REMIND"))
              (plist-get (gptel-fsm-info fsm)
                         :niwa-reminder-entries)))
            (let ((commits (plist-get (gptel-fsm-info fsm)
                                      :niwa-reminder-commits)))
              (should (= 3 (length commits)))
              (mapc #'funcall commits)
              (with-current-buffer chat-buf
                (should-not niwa-reminders--reserved-hook-context))
              (should (equal 0 (niwa-reminder-last-fired
                                (car (niwa-session-reminders session))))))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "`niwa-reminders--transform' is a no-op when no session is present"
  (let* ((prompt-buf (generate-new-buffer " *niwa-test-prompt*"))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (with-current-buffer prompt-buf
          (insert "untouched")
          (niwa-reminders--transform fsm)
          (should (equal "untouched" (buffer-string))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "`niwa-reminders--transform' is a no-op when session has no reminders"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (prompt-buf (generate-new-buffer " *niwa-test-prompt*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session))
          (with-current-buffer prompt-buf
            (insert "untouched")
            (niwa-reminders--transform fsm)
            (should (equal "untouched" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf))))

(niwa-deftest niwa-reminders--agent-transform
  ()
  ,test
  (test)
  :doc "stages the invocation's reminder roster with the invocation as context"
  (let* ((seen-ctx nil)
         (agent-buf (generate-new-buffer " *niwa-test-agent*"))
         (inv (niwa-agent-invocation--create
               :turn-count 3
               :reminders
               (list (niwa-reminder-create
                      :type 'agent-demo
                      :trigger (lambda (_) t)
                      :content (lambda (ctx)
                                 (setq seen-ctx ctx)
                                 "AGENT REMIND")))))
         (fsm (gptel-make-fsm :info (list :buffer agent-buf))))
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (setq-local niwa--agent-invocation inv))
          (niwa-reminders-stage-entry
           fsm 'earlier "EARLIER"
           (lambda ()
             (should-not (niwa-reminder-last-fired
                          (car (niwa-agent-invocation-reminders inv))))))
          (niwa-reminders--agent-transform fsm)
          (should (eq seen-ctx inv))
          (should (equal '((:type earlier :body "EARLIER")
                           (:type agent-demo :body "AGENT REMIND"))
                         (plist-get (gptel-fsm-info fsm)
                                    :niwa-reminder-entries)))
          ;; The fired mark is a deferred commit, exactly as on the
          ;; session path.
          (let ((reminder (car (niwa-agent-invocation-reminders inv))))
            (should-not (niwa-reminder-last-fired reminder))
            (mapc #'funcall (plist-get (gptel-fsm-info fsm)
                                       :niwa-reminder-commits))
            (should (= 3 (niwa-reminder-last-fired reminder)))))
      (kill-buffer agent-buf)))
  :doc "is a no-op for a buffer without an agent invocation"
  (let* ((buf (generate-new-buffer " *niwa-test-agent*"))
         (fsm (gptel-make-fsm :info (list :buffer buf))))
    (unwind-protect
        (progn
          (niwa-reminders--agent-transform fsm)
          (should-not (plist-get (gptel-fsm-info fsm)
                                 :niwa-reminder-entries)))
      (kill-buffer buf))))

(niwa-deftest niwa-reminders--handle-inject
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "appends retained reminders after the untouched current user message"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (backend (gptel-make-openai
                   "reminder-openai" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages
                     [(:role "assistant" :content "old")
                      (:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data
                           :niwa-reminder-entries
                           '((:type one :body "one")
                             (:type two :body "two"))))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create :id "request-1"
                                                 :session session))
            (insert "actual task\n")
            (setf (gptel-fsm-info fsm)
                  (plist-put (gptel-fsm-info fsm)
                             :position (point-marker))))
          (niwa-reminders--handle-inject fsm)
          (let ((messages (plist-get data :messages)))
            (should (= 3 (length messages)))
            (should (equal "actual task"
                           (plist-get (aref messages 1) :content)))
            (should
             (equal
              "<system-reminder>\none\n</system-reminder>\n<system-reminder>\ntwo\n</system-reminder>"
              (plist-get (aref messages 2) :content))))
          (should-not (plist-get (gptel-fsm-info fsm)
                                 :niwa-reminder-entries))
          ;; The chat buffer records what was injected as one hidden
          ;; audit block; the blocks themselves never touch it.
          (with-current-buffer chat-buf
            (let ((spans (niwa-transcript-audit-spans (buffer-string))))
              (should (= 1 (length spans)))
              (should (equal '(:type injected-reminders
                               :phase turn-start
                               :items ((:type one :body "one")
                                       (:type two :body "two")))
                             (plist-get (car spans) :record))))
            (should-not (string-match-p "<system-reminder>"
                                        (buffer-string))))
          (niwa-reminders--handle-inject fsm)
          (should (= 3 (length (plist-get data :messages))))
          ;; Idempotent for the record too.
          (with-current-buffer chat-buf
            (should (= 1 (length (niwa-transcript-audit-spans
                                  (buffer-string)))))))
      (kill-buffer chat-buf)))

  :doc "keeps consumable state when injection fails"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (prompt-buf (generate-new-buffer " *niwa-test-prompt*"))
         (backend (gptel-make-openai
                   "reminder-fail" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data)))
         (reminder (niwa-reminders-make-pending-events)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create :id "request-1"
                                                 :session session))
            (setf (niwa-session-pending-reminders session) '("EVENT"))
            (setf (niwa-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK")))
            (niwa-session-add-reminder session reminder)
            (niwa-reminders-queue-turn-event
             chat-buf 'demo "TURN"))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (niwa-reminders--transform fsm))
          (cl-letf (((symbol-function 'gptel--inject-prompt)
                     (lambda (&rest _) (error "Injection failed"))))
            (should-error (niwa-reminders--handle-inject fsm)))
          ;; A successful payload injection without a transcript marker must
          ;; roll back too, preserving both payload and pending state.
          (should-error (niwa-reminders--handle-inject fsm))
          (should (equal data '(:messages [(:role "user" :content "actual task")])))
          ;; Nothing reached the model, so nothing may be consumed.
          (should (equal '("EVENT")
                         (niwa-session-pending-reminders session)))
          (should-not (niwa-reminder-last-fired reminder))
          (with-current-buffer chat-buf
            (should (plist-get niwa-reminders--turn-events :items)))
          ;; Hook context is reserved for the in-flight request rather
          ;; than left pending, so a mid-request taker cannot deliver it
          ;; twice; settling the dead turn returns it.
          (should-not (niwa-session-hook-context-pending session))
          (should (niwa-reminders-restore-reserved-context chat-buf))
          (should (niwa-session-hook-context-pending session)))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "consumes staged state exactly once when injection succeeds"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (prompt-buf (generate-new-buffer " *niwa-test-prompt*"))
         (backend (gptel-make-openai
                   "reminder-once" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data
                           :position (with-current-buffer chat-buf (point-marker)))))
         (reminder (niwa-reminders-make-pending-events)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create :id "request-1"
                                                 :session session))
            (setf (niwa-session-pending-reminders session) '("EVENT"))
            (setf (niwa-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK")))
            (niwa-session-add-reminder session reminder)
            (niwa-reminders-queue-turn-event chat-buf 'demo "TURN"))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (niwa-reminders--transform fsm))
          (niwa-reminders--handle-inject fsm)
          (should-not (niwa-session-pending-reminders session))
          (should-not (niwa-session-hook-context-pending session))
          (should (niwa-reminder-last-fired reminder))
          (with-current-buffer chat-buf
            (should-not (plist-get niwa-reminders--turn-events :items)))
          (let ((count (length (plist-get data :messages))))
            (niwa-reminders--handle-inject fsm)
            (should (= count (length (plist-get data :messages))))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "a dead turn returns the hook context it reserved"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (prompt-buf (generate-new-buffer " *niwa-test-prompt*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setf (niwa-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK"))))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (niwa-reminders--transform fsm))
          (should-not (niwa-session-hook-context-pending session))
          ;; The turn dies without delivering, and every dead turn ends its
          ;; request, so that teardown hands the reservation back.
          (with-current-buffer chat-buf
            (niwa-request-end))
          (should (equal '((:event "SessionStart" :body "HOOK"))
                         (niwa-session-hook-context-pending session)))
          ;; And only once: a second teardown adds nothing.
          (with-current-buffer chat-buf
            (niwa-request-end))
          (should (= 1 (length
                        (niwa-session-hook-context-pending session)))))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "consumes hook context even when no reminder block fires"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (prompt-buf (generate-new-buffer " *niwa-test-prompt*"))
         (backend (gptel-make-openai
                   "reminder-context" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "actual task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create :id "request-1"
                                                 :session session))
            (setf (niwa-session-hook-context-pending session)
                  '((:event "SessionStart" :body "HOOK"))))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (niwa-reminders--transform fsm)
            ;; The context rides the prompt text, so it contributes no
            ;; block, and its commit must still run.
            (should (string-match-p "HOOK" (buffer-string))))
          (should-not (plist-get (gptel-fsm-info fsm)
                                 :niwa-reminder-entries))
          (niwa-reminders--handle-inject fsm)
          (should-not (niwa-session-hook-context-pending session)))
      (kill-buffer chat-buf)
      (kill-buffer prompt-buf)))

  :doc "formats synthetic reminder messages for the active backend"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (backend (gptel-make-gemini
                   "reminder-gemini" :key "test" :models '(test)))
         (data (list :contents
                     [(:role "model" :parts [(:text "old")])
                      (:role "user" :parts [(:text "actual task")])]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :backend backend :data data
                           :position (with-current-buffer chat-buf (point-marker))
                           :niwa-reminder-entries
                           '((:type ctx :body "context"))))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create :id "request-1"
                                                 :session session)))
          (niwa-reminders--handle-inject fsm)
          (let* ((contents (plist-get data :contents))
                 (reminder (aref contents 2)))
            (should (= 3 (length contents)))
            (should-not (plist-member reminder :content))
            (should
             (equal "<system-reminder>\ncontext\n</system-reminder>"
                    (plist-get (aref (plist-get reminder :parts) 0)
                               :text)))))
      (kill-buffer chat-buf))))

(niwa-deftest niwa-reminders-queue-turn-event
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "coalesces same-turn events and drops them when their owner changes"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-test-chat*"))
         (request-1 (niwa-request--create :id "request-1" :session session))
         (request-2 (niwa-request--create :id "request-2" :session session))
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
          (setq-local niwa--session session)
          (setq-local niwa--current-request request-1)
          (niwa-reminders-queue-turn-event
           chat-buf 'diagnostics "old" (lambda () (setq committed 'old)))
          (niwa-reminders-queue-turn-event
           chat-buf 'diagnostics "new" (lambda () (setq committed 'new)))
          (should-not committed)
          (niwa-reminders--handle-inject fsm)
          (should (eq committed 'new))
          (should (= 2 (length (plist-get data :messages))))
          (should
           (equal "<system-reminder>\nnew\n</system-reminder>"
                  (plist-get (aref (plist-get data :messages) 1) :content)))
          (niwa-reminders-queue-turn-event
           chat-buf 'diagnostics "late" (lambda () (setq committed 'late)))
          (setq-local niwa--current-request request-2)
          (niwa-reminders--handle-inject fsm)
          (should (eq committed 'new))
          (should (= 2 (length (plist-get data :messages)))))
      (kill-buffer chat-buf))))


;;
;;; Agent invocation wiring

(niwa-deftest niwa-agent-invocation-create
  (:before-each (niwa-test--capture-agent-registry)
   :after-each (niwa-test--restore-agent-registry))
  ,test
  (test)

  :doc "creates an invocation with independent reminder clones"
  (let* ((r (niwa-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "hi")))
         (_ (niwa-define-agent inv-agent
              :description "Inv test"
              :tools nil
              :reminders (list r)))
         (agent (niwa-agent-get "inv-agent"))
         (inv (niwa-agent-invocation-create agent))
         (clone (cl-find 'a (niwa-agent-invocation-reminders inv)
                         :key #'niwa-reminder-type)))
    (should (eq agent (niwa-agent-invocation-agent inv)))
    (should (equal 0 (niwa-agent-invocation-turn-count inv)))
    (should clone)
    ;; clone is not eq to original
    (should-not (eq r clone)))

  :doc "two invocations track last-fired independently"
  (let* ((r (niwa-reminder-create
             :type 'a
             :trigger (lambda (_) t)
             :content (lambda (_) "x")
             :interval 3))
         (_ (niwa-define-agent inv-agent
              :description "Inv test"
              :tools nil
              :reminders (list r)))
         (agent (niwa-agent-get "inv-agent"))
         (inv-a (niwa-agent-invocation-create agent))
         (inv-b (niwa-agent-invocation-create agent))
         (clone-a (cl-find 'a (niwa-agent-invocation-reminders inv-a)
                           :key #'niwa-reminder-type))
         (clone-b (cl-find 'a (niwa-agent-invocation-reminders inv-b)
                           :key #'niwa-reminder-type)))
    (test-niwa-reminders--collect-and-commit
     (niwa-agent-invocation-reminders inv-a) 0 inv-a)
    (should (equal 0 (niwa-reminder-last-fired clone-a)))
    (should (null (niwa-reminder-last-fired clone-b))))

  :doc "role names do not impose undeclared reminder policy"
  (dolist (name '("reviewer" "verifier" "custom"))
    (let* ((agent (niwa-agent--create :name name :tools nil))
           (inv (niwa-agent-invocation-create agent)))
      (should-not (niwa-agent-invocation-reminders inv)))))









;;
;;; Tier 1 built-in reminders

(niwa-deftest niwa-reminders-make-mode-constraints
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "advertises initial Ask once and does not repeat it unchanged"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-mode-constraints)))
    (setf (niwa-session-permission-mode session) 'ask)
    (should (niwa-reminders--should-fire-p r 0 session))
    (test-niwa-reminders--collect-and-commit (list r) 0 session)
    (should-not (niwa-reminders--should-fire-p r 10 session)))

  :doc "fires when session permission mode is not `ask'"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-mode-constraints)))
    (setf (niwa-session-permission-mode session) 'edits)
    (should (niwa-reminders--should-fire-p r 0 session))
    (should (string-match-p "auto"
                            (funcall (niwa-reminder-content r) session))))

  :doc "content varies by mode and falls back for unknown modes"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-mode-constraints)))
    (setf (niwa-session-permission-mode session) 'edits)
    (should (string-match-p "auto"
                            (funcall (niwa-reminder-content r) session)))
    (setf (niwa-session-permission-mode session) 'weird-mode)
    (should (string-match-p "weird-mode"
                            (funcall (niwa-reminder-content r) session))))

  :doc "mode and reviewer changes reach provider payload only once delivered"
  (dolist (host '("example.test" "api.openai.com"))
    (dolist (scenario '((ask (full-auto user) (nil user))
                        (full-auto (nil auto) (ask auto))
                        (ask (nil user) (nil auto) (nil user))
                        (edits (nil user) (nil auto) (nil user))))
      (with-temp-buffer
        (let* ((session (niwa-session--create
                         :permission-mode (car scenario)))
               (niwa-permission-reviewer 'user)
               (reminder (niwa-reminders-make-mode-constraints))
               (backend (gptel-make-openai
                         "scoped-reminder" :host host :key "test" :models '(test)))
               ;; The default OpenAI host selects the native Responses backend.
               (payload-key (if (gptel-openai-responses-p backend) :input :messages))
               previous)
          (setq-local niwa--session session)
          (dolist (state (cdr scenario))
            (setq niwa-permission-reviewer (cadr state))
            (setq-local niwa--current-request
                        (niwa-request--create
                         :session session :permission-mode-override (car state)))
            (let* ((mode (or (car state) (car scenario)))
                   (expected (funcall (niwa-reminder-content reminder) session))
                   (batch (niwa-reminders--collect-from
                           (list reminder) 0 session))
                   (data (list payload-key (vector (list :role "user" :content "task"))))
                   (fsm (gptel-make-fsm
                         :info (list :buffer (current-buffer) :backend backend
                                     :data data :position (copy-marker (point-max))))))
              (should (equal previous (niwa-reminder-mode-constraints-observation reminder)))
              (should (eq mode (niwa-reminders--session-mode session)))
              (should (cl-find 'mode-constraints (plist-get batch :entries)
                               :key (lambda (entry) (plist-get entry :type))))
              (should (= 1 (length (plist-get batch :entries))))
              (niwa-reminders--stage-batch
               fsm (plist-get batch :entries) (plist-get batch :commits))
              (should (eq data (plist-get (gptel-fsm-info fsm) :data)))
              (should (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
              (niwa-reminders--handle-inject fsm)
              (let* ((messages (plist-get data payload-key))
                     (body (plist-get (aref messages (1- (length messages))) :content)))
                (ert-info ((format "Host %S scenario %S state %S" host scenario state))
                  (should (= 2 (length messages)))
                  (should (equal "task" (plist-get (aref messages 0) :content)))
                  (should (string-search expected body))))
              (should (equal expected (niwa-reminder-mode-constraints-observation reminder)))
              (should (eq (car scenario) (niwa-session-permission-mode session)))
              (should-not (plist-get (niwa-reminders--collect-from (list reminder) 0 session)
                                     :entries))
              (setq previous expected)))
          (should-not (plist-get (niwa-reminders--collect-from (list reminder) 0 session)
                                 :entries))))))

  :doc "Ask and Edits describe automatic review without changing user or Full Access text"
  (let* ((session (niwa-session--create))
         (r (niwa-reminders-make-mode-constraints))
         (niwa-permission-reviewer 'auto))
    (dolist (mode '(ask edits))
      (setf (niwa-session-permission-mode session) mode)
      (let ((body (funcall (niwa-reminder-content r) session)))
        (dolist (text '("automatic reviewer first"
                        "typically approves task-relevant observational live Eval and clearly scoped requests without involving the user"
                        "mutations, unknown effects and unclear scope go to the user"))
          (should (string-search text body)))
        (should-not (string-search "require approval" body))))
    (dolist (case '((edits . "Permission mode: `edits'. Native edits and confined Bash/fresh Eval are automatic. Ordinary native reads include OS-readable paths; configured credential masks remain. Live Eval and additional authority require approval.")
                    (full-auto . "Permission mode: `full-auto'. Full Access under the target OS account disables confinement and permission prompts, including default protected paths. Native tools and live Eval are automatic. Live Eval runs inside Emacs without confinement. Explicit hard denies, Plan restrictions and integrity checks remain. Keep destructive tool calls deliberate.")))
      (setf (niwa-session-permission-mode session) (car case))
      (when (eq (car case) 'full-auto)
        (test-niwa-reminders--collect-and-commit (list r) 0 session))
      (dolist (reviewer (if (eq (car case) 'full-auto) '(auto user) '(user)))
        (setq niwa-permission-reviewer reviewer)
        (should (equal (cdr case) (funcall (niwa-reminder-content r) session)))
        (when (eq (car case) 'full-auto)
          (should-not (niwa-reminders--should-fire-p r 0 session))))))

  :doc "each mode advisory fires once until the effective mode changes"
  (let* ((session (niwa-session--create :permission-mode 'ask))
         (r (niwa-reminders-make-mode-constraints)))
    (dolist (mode '(ask edits full-auto ask full-auto))
      (setf (niwa-session-permission-mode session) mode)
      (should (= 1 (length (test-niwa-reminders--collect-and-commit
                            (list r) 0 session))))
      (dolist (turn '(0 1 5 10 100))
        (should-not (niwa-reminders--should-fire-p r turn session))))
    (let ((body (funcall (niwa-reminder-content r) session)))
      (dolist (text '("default protected paths" "Live Eval runs inside Emacs without confinement"
                      "Explicit hard denies" "Plan restrictions" "integrity checks"
                      "Keep destructive tool calls deliberate"))
        (should (string-search text body))))))

(niwa-deftest niwa-reminders-stage-plan-mode
  (:doc "restores sticky and read-only request Plan guidance once, without changing history")
  ,test
  (test)
  (dolist (kind '(sticky read-only-request inactive agent))
    (with-temp-buffer
      (setq-local niwa--session
                  (unless (eq kind 'agent)
                    (niwa-session--create
                     :name "plan"
                     :plan-mode (eq kind 'sticky))))
      (setq-local niwa--current-request
                  (when (eq kind 'read-only-request)
                    (niwa-request--create :plan-read-only t)))
      (insert "Compacted conversation\n")
      (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
        (niwa-reminders-stage-plan-mode fsm)
        (niwa-reminders-stage-plan-mode fsm)
        (let ((entries (plist-get (gptel-fsm-info fsm)
                                  :niwa-reminder-entries)))
          (if (memq kind '(sticky read-only-request))
              (progn
                (should (= 1 (length entries)))
                (should (eq 'plan-mode (plist-get (car entries) :type)))
                (should (equal
                         (funcall (niwa-reminder-content
                                   (niwa-reminders-make-plan-mode))
                                  niwa--session)
                         (plist-get (car entries) :body)))
                (let ((reminder (niwa-reminders-make-plan-mode)))
                  (setf (niwa-session-reminders niwa--session) (list reminder))
                  (test-niwa-reminders--collect-and-commit
                   (list reminder) 1 niwa--session)
                  (let ((next (gptel-make-fsm :info (list :buffer (current-buffer)))))
                    (niwa-reminders-stage-plan-mode next)
                    (let ((body (plist-get (car (plist-get (gptel-fsm-info next)
                                                          :niwa-reminder-entries)) :body)))
                      (should (string-search "niwa://plan-mode.md" body))
                      (should-not (string-search "## Summary" body))))))
            (should-not entries)))
        (should (equal "Compacted conversation\n" (buffer-string)))))))

(niwa-deftest niwa-reminders-make-plan-mode
  (:doc "fires every turn only while Plan mode is active")
  ,test
  (test)
  (let* ((session (niwa-session--create :name "main" :plan-mode t))
         (reminder (niwa-reminders-make-plan-mode))
         (skeleton
          (concat
           "<proposed_plan>\n"
           "# Concrete Plan Title\n\n"
           "## Summary\n"
           "- Root cause or goal, behavior change, non-goals.\n\n"
           "## Key Changes\n"
           "- Group by subsystem or behavior; name files, APIs, interfaces, or data shapes only to remove ambiguity.\n\n"
           "## Regression Coverage\n"
           "- User-visible flows, edge cases, and failures to test.\n\n"
           "## Validation\n"
           "- Exact focused test/build commands.\n\n"
           "## Assumptions\n"
           "- Defaults, compatibility assumptions, unchanged behavior.\n"
           "</proposed_plan>")))
    (setf (niwa-session-reminders session) (list reminder))
    (should (funcall (niwa-reminder-trigger reminder) session))
    (should-not (niwa-reminder-interval reminder))
    (let ((content (funcall (niwa-reminder-content reminder) session)))
      (dolist (text '("Eval is unavailable"
                      "session-owned work:// descendants"
                      "requests carrying inherited Plan limits permit ApplyPatch only"
                      "Agents you start inherit these Plan limits"
                      "critique of a draft plan"
                      "Bash is limited"
                      "implementation request"
                      "Explore available evidence"
                      "genuine user preferences"
                      "replaces the previous proposal completely"
                      "Do not ask whether you should proceed"))
        (should (string-match-p text content)))
      (should (string-match-p (regexp-quote skeleton) content)))
    (test-niwa-reminders--collect-and-commit (list reminder) 1 session)
    (should (niwa-reminders--should-fire-p reminder 2 session))
    (let ((reference (funcall (niwa-reminder-content reminder) session)))
      (should (string-search "niwa://plan-mode.md" reference))
      (should (string-search "<proposed_plan>...</proposed_plan>" reference))
      (should-not (string-search "## Summary" reference))
      (should-not (string-search "\n" reference)))
    (setf (niwa-session-plan-mode session) nil)
    (should-not (funcall (niwa-reminder-trigger reminder) session))
    (let ((niwa--current-request
           (niwa-request--create :plan-read-only t)))
      (should (funcall (niwa-reminder-trigger reminder) session)))
    (let ((niwa--agent-invocation
           (niwa-agent-invocation--create :plan-read-only t)))
      (should (funcall (niwa-reminder-trigger reminder) session))))

  :doc "retained Plan agents get one full template and then a recovery reference"
  (let* ((reminder (niwa-reminders-make-plan-mode))
         (niwa--agent-invocation
          (niwa-agent-invocation--create :plan-read-only t :reminders (list reminder))))
    (let ((first (niwa-reminders--collect-from (list reminder) 0 niwa--agent-invocation)))
      (should (string-search "## Summary" (plist-get (car (plist-get first :entries)) :body)))
      ;; Staging without receipt leaves the full contract retryable.
      (should-not (niwa-reminder-last-fired reminder)))
    (test-niwa-reminders--collect-and-commit (list reminder) 0 niwa--agent-invocation)
    (let ((next (niwa-reminders--collect-from (list reminder) 1 niwa--agent-invocation)))
      (should (string-search "niwa://plan-mode.md"
                             (plist-get (car (plist-get next :entries)) :body)))
      (should-not (string-search "## Summary"
                                 (plist-get (car (plist-get next :entries)) :body))))))

(niwa-deftest niwa-reminders-rearm-plan-mode
  (:doc "new Plan episodes restore the full template without changing other reminders")
  (let* ((r (niwa-reminders-make-plan-mode))
         (mode (niwa-reminders-make-mode-constraints))
         (session (niwa-session--create :plan-mode t :reminders (list r mode))))
    (test-niwa-reminders--collect-and-commit (list r mode) 1 session)
    (should-not (string-search "## Summary" (funcall (niwa-reminder-content r) session)))
    (niwa-reminders-rearm-plan-mode session)
    (should (string-search "## Summary" (funcall (niwa-reminder-content r) session)))
    (should (= 1 (niwa-reminder-last-fired mode)))))


(niwa-deftest niwa-reminders-make-max-turns-warning
  (:before-each (niwa-test--capture-agent-registry)
   :after-each (niwa-test--restore-agent-registry))
  ,test
  (test)

  :doc "does not fire below the threshold"
  (let* ((_ (niwa-define-agent mt-agent
              :description "d"
              :tools nil
              :max-turns 10))
         (agent (niwa-agent-get "mt-agent"))
         (inv (niwa-agent-invocation-create agent))
         (r (niwa-reminders-make-max-turns-warning)))
    (setf (niwa-agent-invocation-turn-count inv) 5)
    (should-not (niwa-reminders--should-fire-p r 5 inv)))

  :doc "fires at the default 80% threshold"
  (let* ((_ (niwa-define-agent mt-agent
              :description "d"
              :tools nil
              :max-turns 10))
         (agent (niwa-agent-get "mt-agent"))
         (inv (niwa-agent-invocation-create agent))
         (r (niwa-reminders-make-max-turns-warning)))
    (setf (niwa-agent-invocation-turn-count inv) 8)
    (should (niwa-reminders--should-fire-p r 8 inv))
    (should (string-match-p "8 of 10"
                            (funcall (niwa-reminder-content r) inv))))

  :doc "does not fire for agents without a max-turns cap"
  (let* ((_ (niwa-define-agent mt-agent
              :description "d"
              :tools nil))
         (agent (niwa-agent-get "mt-agent"))
         (inv (niwa-agent-invocation-create agent))
         (r (niwa-reminders-make-max-turns-warning)))
    (setf (niwa-agent-invocation-turn-count inv) 999)
    (should-not (niwa-reminders--should-fire-p r 999 inv)))

  :doc "is one-shot"
  (let ((r (niwa-reminders-make-max-turns-warning)))
    (should (eq 'one-shot (niwa-reminder-interval r)))))


(niwa-deftest niwa-reminders-agent-turn-warnings
  (:doc "collects only the due max-turns warning from the invocation's reminders"
   :before-each (niwa-test--capture-agent-registry)
   :after-each (niwa-test--restore-agent-registry))
  (let* ((_ (niwa-define-agent mt-agent
              :description "d"
              :tools nil
              :max-turns 10))
         (inv (niwa-agent-invocation-create (niwa-agent-get "mt-agent"))))
    (setf (niwa-agent-invocation-reminders inv)
          (list (niwa-reminders-make-max-turns-warning)
                (niwa-reminders-make-pending-events)))
    (setf (niwa-agent-invocation-turn-count inv) 5)
    (should-not (plist-get (niwa-reminders-agent-turn-warnings inv 5) :entries))
    (setf (niwa-agent-invocation-turn-count inv) 8)
    (let ((batch (niwa-reminders-agent-turn-warnings inv 8)))
      (should (equal '(max-turns-warning)
                     (mapcar (lambda (entry) (plist-get entry :type)) (plist-get batch :entries))))
      (should (plist-get batch :commits)))))


(niwa-deftest niwa-reminders-make-verification-suggestion
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "does not suggest implementation verification after reading a file"
  (let* ((file (make-temp-file "niwa-verifier-read-" nil ".el" "(provide 'fixture)\n"))
         (session (niwa-session--create
                   :turn-count 1 :touched-files (make-hash-table :test #'equal)))
         (reminder (niwa-reminders-make-verification-suggestion)))
    (unwind-protect
        (progn
          (niwa-session-record-interaction session file 'read 0)
          (should-not (niwa-reminders--should-fire-p reminder 1 session)))
      (delete-file file)))

  :doc "does not revive old modifications during later read-only turns"
  (let* ((session (niwa-session--create
                   :turn-count 12 :touched-files (make-hash-table :test #'equal)))
         (reminder (niwa-reminders-make-verification-suggestion)))
    (niwa-session-record-interaction session "/tmp/example.el" 'modify 1)
    (niwa-session-record-interaction session "/tmp/example.el" 'read 12)
    (should-not (niwa-reminders--should-fire-p reminder 12 session))
    (niwa-session-record-interaction session "/tmp/example.el" 'modify 12)
    (should-not (niwa-reminders--should-fire-p reminder 12 session))
    (niwa-session-record-interaction session "/tmp/second.el" 'modify 12)
    (should (niwa-reminders--should-fire-p reminder 12 session)))

  :doc "does not fire when session has no touched files"
  (let* ((tmp (make-temp-file "niwa-vs-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-verification-suggestion)))
    (unwind-protect
        (progn
          (should-not (funcall (niwa-reminder-trigger r) session)))
      (delete-directory tmp t)))

  :doc "fires after the latest turn modified multiple implementation files"
  (let* ((tmp (make-temp-file "niwa-vs-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-verification-suggestion)))
    (unwind-protect
        (progn
          (niwa-session-record-interaction session "/tmp/example.el" 'modify 0)
          (niwa-session-record-interaction session "/tmp/second.el" 'modify 0)
          (should (funcall (niwa-reminder-trigger r) session))
          (should (string-match-p "independent verification"
                                  (funcall (niwa-reminder-content r) session))))
      (delete-directory tmp t)))

  :doc "keeps generic verification guidance for recent edits after plan verification"
  (let* ((tmp (make-temp-file "niwa-vs-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-verification-suggestion)))
    (unwind-protect
        (progn
          (setf (niwa-session-plan-metadata session)
                '(:status accepted :verification-pending nil))
          (niwa-session-record-interaction session "/tmp/example.el" 'modify 0)
          (niwa-session-record-interaction session "/tmp/second.el" 'modify 0)
          (should (funcall (niwa-reminder-trigger r) session))
          (should-not (string-match-p
                       "accepted plan"
                       (funcall (niwa-reminder-content r) session))))
      (delete-directory tmp t)))

  :doc "fires for rejected/cancelled/presented metadata without accepted wording"
  (let* ((tmp (make-temp-file "niwa-vs-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-verification-suggestion)))
    (unwind-protect
        (progn
          (niwa-session-record-interaction session "/tmp/example.el" 'modify 0)
          (niwa-session-record-interaction session "/tmp/second.el" 'modify 0)
          (dolist (status '(rejected cancelled presented))
            (setf (niwa-session-plan-metadata session)
                  (list :status status :verification-pending t))
            (should (funcall (niwa-reminder-trigger r) session))
            (should-not (string-match-p
                         "accepted plan"
                         (funcall (niwa-reminder-content r) session)))))
      (delete-directory tmp t)))

  :doc "adds accepted-plan guidance after a recent modification"
  (let* ((tmp (make-temp-file "niwa-vs-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "vs"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-verification-suggestion)))
    (unwind-protect
        (progn
          (setf (niwa-session-plan-metadata session)
                '(:status accepted :verification-pending t))
          (niwa-session-record-interaction session "/tmp/example.el" 'modify 0)
          (niwa-session-record-interaction session "/tmp/second.el" 'modify 0)
          (should (funcall (niwa-reminder-trigger r) session))
          (should (string-match-p "plan"
                                  (funcall (niwa-reminder-content r) session))))
      (delete-directory tmp t)))

  :doc "ignores docs, isolated edits, and generated or vendored code"
  (dolist (paths '(("/tmp/one.el")
                   ("/tmp/README.md" "/tmp/guide.org")
                   ("/tmp/one.el" "/tmp/guide.md")
                   ("/tmp/docs/one.el" "/tmp/docs/two.el")
                   ("/tmp/build/one.js" "/tmp/vendor/two.el")
                   ("/tmp/node_modules/a.js" "/tmp/.eask/b.el")))
    (let ((session (niwa-session--create
                    :turn-count 0 :touched-files (make-hash-table :test #'equal)))
          (r (niwa-reminders-make-verification-suggestion)))
      (dolist (path paths)
        (niwa-session-record-interaction session path 'modify 0))
      (should-not (niwa-reminders--should-fire-p r 0 session))))

  :doc "commits at most once in a turn and preserves retry and ten-turn throttle"
  (let ((session (niwa-session--create
                  :turn-count 0 :touched-files (make-hash-table :test #'equal)))
        (r (niwa-reminders-make-verification-suggestion)))
    (dolist (path '("/tmp/src/a.el" "/tmp/test/a.el"))
      (niwa-session-record-interaction session path 'modify 0))
    ;; Cancelled collection must remain retryable; delivery is the commit seam.
    (should (plist-get (niwa-reminders--collect-from (list r) 0 session) :entries))
    (should (= 1 (length (test-niwa-reminders--collect-and-commit (list r) 0 session))))
    (should-not (plist-get (niwa-reminders--collect-from (list r) 0 session) :entries))
    (dolist (turn '(1 9 10))
      (setf (niwa-session-turn-count session) turn)
      (dolist (path '("/tmp/src/a.el" "/tmp/test/a.el"))
        (niwa-session-record-interaction session path 'modify turn))
      (should (eq (>= turn 10)
                  (and (niwa-reminders--should-fire-p r turn session) t)))))

  :doc "WAIT injection delivers the advisory once and unsubmitted staging stays retryable"
  (with-temp-buffer
    (let* ((r (niwa-reminders-make-verification-suggestion))
           (session (niwa-session--create
                     :turn-count 0 :touched-files (make-hash-table :test #'equal)
                     :reminders (list r)))
           (backend (gptel-make-openai "verification-delivery"
                      :host "example.test" :key "test" :models '(test)))
           (fsm (gptel-make-fsm
                 :info (list :buffer (current-buffer) :backend backend
                             :position (copy-marker (point-max))))))
      (setq-local niwa--session session)
      (dolist (path '("/tmp/src/a.el" "/tmp/src/b.el"))
        (niwa-session-record-interaction session path 'modify 0))
      (let ((batch (niwa-reminders--collect-from (list r) 0 session)))
        (niwa-reminders--stage-batch
         fsm (plist-get batch :entries) (plist-get batch :commits)))
      (niwa-reminders--handle-inject fsm)
      (should-not (niwa-reminder-last-fired r))
      (should (plist-get (niwa-reminders--collect-from (list r) 0 session) :entries))
      (let ((data (list :messages (vector (list :role "user" :content "task")))))
        (setf (gptel-fsm-info fsm) (plist-put (gptel-fsm-info fsm) :data data))
        (niwa-reminders--handle-inject fsm)
        (niwa-reminders--handle-inject fsm)
        (should (= 2 (length (plist-get data :messages))))
        (should (string-search "independent verification"
                               (plist-get (aref (plist-get data :messages) 1) :content))))
      (should (= 0 (niwa-reminder-last-fired r)))
      (should-not (plist-get (niwa-reminders--collect-from (list r) 0 session) :entries)))))


(niwa-deftest niwa-reminders-make-user-revised-patch
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "fires once, names the revised changes, and round-trips its recipe"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/urp/" "/tmp/urp/" "urp"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-user-revised-patch
             "one.txt hunk 2, fresh.el (new file content)")))
    (should (niwa-reminders--should-fire-p r 0 session))
    (let ((content (funcall (niwa-reminder-content r) session)))
      (should (string-search "one.txt hunk 2" content))
      (should (string-search "Do not revert" content)))
    (setf (niwa-reminder-last-fired r) 0)
    (should-not (niwa-reminders--should-fire-p r 1 session))
    (let* ((recipe (niwa-reminder-recipe r))
           (restored (car (niwa-reminders-restore-agent-templates
                           (list recipe)))))
      (should (eq (niwa-reminder-type restored) 'user-revised-patch))
      (should (equal recipe (niwa-reminder-recipe restored))))))


(niwa-deftest niwa-reminders-make-plan-reference
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "surfaces verified plan contents instead of a poisoned fixed cache"
  (let* ((tmp (make-temp-file "niwa-plan-ref-" t))
         (ws (niwa-workspace-get-or-create
              'file (file-name-as-directory tmp)
              (file-name-as-directory tmp) "pr"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-plan-reference))
         (plan-path (file-name-concat tmp "local" "plans" "current.md"))
         (accepted-path
          (file-name-concat tmp "local" "plans"
                            "accepted-20260813-120000.md")))
    (unwind-protect
        (progn
          (niwa-workspace-identity-ensure tmp)
          (make-directory (file-name-directory plan-path) t)
          (write-region "# Current draft" nil plan-path nil 'silent)
          (write-region "# Accepted\n\nDo it." nil accepted-path nil 'silent)
          (setf (niwa-session-save-path session) tmp)
          (setf (niwa-session-turn-count session) 6)
          (setf (niwa-session-plan-metadata session)
                '(:path "local/plans/current.md"
                  :accepted-path "local/plans/accepted-20260813-120000.md"
                  :status accepted :accepted-turn 5))
          (with-temp-buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "Plan accepted\n")
            (niwa-session-artifacts-assert-mutation-authority session)
            (niwa-session-publication-publish
             session (list (list :path accepted-path :content "# Accepted\n\nDo it.")))
            (niwa-session-artifacts-save session (current-buffer)))
          (write-region "# Poisoned accepted cache" nil accepted-path nil 'silent)
          (should (funcall (niwa-reminder-trigger r) session))
          (let ((content (funcall (niwa-reminder-content r) session)))
            (should (string-match-p
                     "work://plans/accepted-20260813-120000.md"
                     content))
            (should-not (string-match-p "local/plans/current.md" content))
            (should-not (string-match-p "work://plans/current.md" content))
            (should-not (string-match-p (regexp-quote tmp) content))
            (should (string-match-p "# Accepted" content))
            (should-not (string-match-p "Poisoned" content))
            (should-not (string-match-p "# Current draft" content)))
          ;; An active Goal suppresses this reminder only when it already carries
          ;; the exact accepted plan reference.
          (setf (niwa-session-goal session)
                (niwa-goal--create :status 'active))
          (should (funcall (niwa-reminder-trigger r) session))
          (setf (niwa-goal-plan-reference (niwa-session-goal session))
                "local/plans/other.md")
          (should (funcall (niwa-reminder-trigger r) session))
          (setf (niwa-goal-plan-reference (niwa-session-goal session))
                "local/plans/accepted-20260813-120000.md")
          (should-not (funcall (niwa-reminder-trigger r) session)))
      (niwa-session-persistence-lock-release tmp session)
      (delete-directory tmp t)))

  :doc "waits until after the acceptance turn before firing"
  (let* ((tmp (make-temp-file "niwa-plan-ref-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "pr"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-plan-reference))
         (plan-path (file-name-concat tmp "local" "plans" "current.md"))
         (accepted-path
          (file-name-concat tmp "local" "plans"
                            "accepted-20260813-120000.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory plan-path) t)
          (write-region "# Plan\n\nDo it." nil plan-path nil 'silent)
          (write-region "# Accepted\n\nDo it." nil accepted-path nil 'silent)
          (setf (niwa-session-save-path session) tmp)
          (setf (niwa-session-turn-count session) 5)
          (setf (niwa-session-plan-metadata session)
                '(:path "local/plans/current.md"
                  :accepted-path "local/plans/accepted-20260813-120000.md"
                  :status accepted :accepted-turn 5))
          ;; A portable session reads its plan through the publication, never
          ;; the fixed cache, so the artifact has to be served here.
          (cl-letf (((symbol-function
                      'niwa-session-artifacts-artifact-present-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'niwa-session-artifacts-read-artifact)
                     (lambda (&rest _)
                       (encode-coding-string "# Plan\n\nDo it." 'utf-8-unix))))
            (should-not (funcall (niwa-reminder-trigger r) session))
            (setf (niwa-session-turn-count session) 6)
            (should (funcall (niwa-reminder-trigger r) session))))
      (delete-directory tmp t)))

  :doc "does not fire or read the mutable plan without an accepted artifact"
  (let* ((tmp (make-temp-file "niwa-plan-ref-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "pr"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-plan-reference))
         (plan-path (file-name-concat tmp "local" "plans" "current.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory plan-path) t)
          (write-region "# Mutable current plan" nil plan-path nil 'silent)
          (setf (niwa-session-save-path session) tmp)
          (setf (niwa-session-turn-count session) 6)
          (setf (niwa-session-plan-metadata session)
                '(:path "local/plans/current.md"
                  :status accepted :accepted-turn 5))
          (should-not (funcall (niwa-reminder-trigger r) session))
          (should-not (funcall (niwa-reminder-content r) session)))
      (delete-directory tmp t)))

  :doc "stays suppressed during a standalone Plan conversation"
  (let ((session
         (niwa-session--create
          :name "main" :plan-mode t :turn-count 2
          :plan-metadata '(:status accepted :accepted-turn 1))))
    (should-not
     (funcall (niwa-reminder-trigger
               (niwa-reminders-make-plan-reference))
              session))))


(niwa-deftest niwa-reminders-make-task-nudge
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when session has no tasks"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-task-nudge)))
    (should-not (funcall (niwa-reminder-trigger r) session)))

  :doc "does not fire before the task write is stale"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-task-nudge 3)))
    (push (niwa-task--create :id 1 :subject "do thing" :status 'in-progress)
          (niwa-session-tasks session))
    (setf (niwa-session-last-task-write-turn session) 5)
    (setf (niwa-session-turn-count session) 7)
    (should-not (funcall (niwa-reminder-trigger r) session)))

  :doc "fires when non-completed tasks are stale"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-task-nudge 3)))
    (setf (niwa-session-tasks session)
          (list (niwa-task--create
                 :id 1 :subject "main work" :status 'in-progress)
                (niwa-task--create
                 :id 2 :subject "agent work" :status 'pending
                 :owner "worker")
                (niwa-task--create
                 :id 3 :subject "done work" :status 'completed
                 :owner "worker")
                (niwa-task--create
                 :id 4 :subject "completed-only work" :status 'completed
                 :owner "finished")))
    (setf (niwa-session-last-task-write-turn session) 5)
    (setf (niwa-session-turn-count session) 8)
    (should (funcall (niwa-reminder-trigger r) session))
    (let ((content (substring-no-properties
                    (funcall (niwa-reminder-content r) session))))
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
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-task-nudge 3)))
    (push (niwa-task--create :id 1 :subject "do thing" :status 'pending)
          (niwa-session-tasks session))
    (setf (niwa-session-turn-count session) 100)
    (should-not (funcall (niwa-reminder-trigger r) session)))

  :doc "labels idle-child work and throttles repeated delivered stale reminders"
  (let* ((session (niwa-session--create
                   :name "main" :turn-count 8 :last-task-write-turn 5
                   :tasks (list (niwa-task--create
                                 :id 1 :subject "Unfinished child work"
                                 :status 'pending :owner "/root/child"))))
         (r (niwa-reminders-make-task-nudge 3))
         (batch (niwa-reminders--collect-from (list r) 8 session)))
    (should (= 1 (length (plist-get batch :entries))))
    (let ((body (plist-get (car (plist-get batch :entries)) :body)))
      (should (string-match-p "Child-owned tasks" body))
      (should (string-match-p "reconciling their RESULT" body))
      (should (string-match-p "owner=/root/child" body))
      (should (string-match-p "failure/interruption leaves them open" body)))
    ;; Collection alone does not consume an undelivered reminder.
    (should (plist-get (niwa-reminders--collect-from (list r) 8 session) :entries))
    (mapc #'funcall (plist-get batch :commits))
    (dotimes (offset 3)
      (setf (niwa-session-turn-count session) (+ 8 offset))
      (should-not (plist-get (niwa-reminders--collect-from
                             (list r) (+ 8 offset) session) :entries)))
    (setf (niwa-session-turn-count session) 11)
    (should (plist-get (niwa-reminders--collect-from (list r) 11 session) :entries)))

  :doc "does not fire when all tasks are completed"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-task-nudge)))
    (push (niwa-task--create :id 1 :subject "done thing" :status 'completed)
          (niwa-session-tasks session))
    (setf (niwa-session-last-task-write-turn session) 1)
    (setf (niwa-session-turn-count session) 100)
    (should-not (funcall (niwa-reminder-trigger r) session))))


(niwa-deftest niwa-reminders-install-defaults--verification
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "installs the verification-suggestion reminder"
  (let* ((tmp (make-temp-file "niwa-install-vs-" t))
         (ws (niwa-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "ivs"))
         (session (niwa-session-create "main" ws)))
    (unwind-protect
        (progn
          (niwa-reminders-install-defaults session)
          (should (cl-some (lambda (r)
                             (eq (niwa-reminder-type r) 'verification-suggestion))
                           (niwa-session-reminders session))))
      (delete-directory tmp t))))


(niwa-deftest niwa-reminders-make-edited-file
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when no cached files changed"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (should-not (niwa-reminders--should-fire-p r 0 session)))
      (delete-directory tmp-root t)))

  :doc "formats a Unicode cached-file diff without an interactive encoding prompt"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello \u03bb\n"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "goodbye \u2192\n"))
            (set-file-times file future))
          (should (niwa-reminders--should-fire-p r 0 session))
          (cl-letf (((symbol-function 'select-safe-coding-system-interactively)
                     (lambda (&rest _) (ert-fail "Unexpected encoding prompt"))))
            (let* ((noninteractive nil)
                   (coding-system-for-write nil)
                   (content (test-niwa-reminders--prepared-content r session)))
              (should (string-match-p "MODIFIED:" content))
              (should (string-match-p (regexp-quote (expand-file-name file))
                                      content))
              (should (string-match-p "^-hello \u03bb" content))
              (should (string-match-p "^\\+goodbye \u2192" content)))))
      (delete-directory tmp-root t)))

  :doc "fires for deleted cached file and labels it"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (delete-file file)
          (should (niwa-reminders--should-fire-p r 0 session))
          (let ((content (test-niwa-reminders--content r session)))
            (should (string-match-p "DELETED:" content))
            (should (string-match-p (regexp-quote (expand-file-name file))
                                    content))))
      (when (file-exists-p tmp-root) (delete-directory tmp-root t))))

  :doc "consuming changes prevents the same change from firing twice"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "world"))
            (set-file-times file future))
          ;; First firing formats + consumes.
          (test-niwa-reminders--content r session)
          (should-not (niwa-reminders--should-fire-p r 0 session)))
      (delete-directory tmp-root t)))

  :doc "truncates diffs longer than the configured limit"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file 5)))
    (unwind-protect
        (progn
          (with-temp-file file
            (dotimes (i 40)
              (insert (format "line %d\n" i))))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file
              (dotimes (i 40)
                (insert (format "changed %d\n" i))))
            (set-file-times file future))
          (let ((content (test-niwa-reminders--prepared-content r session)))
            (should (string-match-p "more lines truncated" content))))
      (delete-directory tmp-root t)))

  :doc "reports a fingerprint-only cache entry without a diff"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "big.log" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file))
         (niwa-file-cache-max-file-bytes 1024))
    (unwind-protect
        (progn
          (with-temp-file file (insert (make-string 4096 ?x)))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert (make-string 8192 ?y)))
            (set-file-times file future))
          (should (niwa-reminders--should-fire-p r 0 session))
          (let ((content (test-niwa-reminders--content r session)))
            (should (string-match-p "MODIFIED:" content))
            (should (string-match-p "no diff available" content))
            ;; Neither side of the change was ever read, so no file body
            ;; can have reached the reminder.
            (should-not (string-match-p "xxxx" content))
            (should-not (string-match-p "yyyy" content))))
      (delete-directory tmp-root t)))

  :doc "reports without a diff once content passes the diff byte cap"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "wide.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file))
         (niwa-reminders-edited-file-max-diff-bytes 512))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert (make-string 4096 ?z)))
            (set-file-times file future))
          (let ((content (test-niwa-reminders--content r session)))
            (should (string-match-p "no diff available" content))
            (should-not (string-match-p "zzzz" content))))
      (delete-directory tmp-root t)))

  :doc "reports a modified file without a diff outside a prepared dispatch"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "goodbye\n"))
            (set-file-times file future))
          (cl-letf (((symbol-function 'niwa-generate-diff)
                     (lambda (&rest _) (ert-fail "Diff started while staging"))))
            (let ((content (test-niwa-reminders--content r session)))
              (should (string-match-p "MODIFIED:" content))
              (should (string-match-p "no diff available" content)))))
      (delete-directory tmp-root t)))

  :doc "continues at once when nothing needs a diff"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file))
         ran)
    (unwind-protect
        (with-temp-buffer
          (niwa-session-add-reminder session r)
          (setq-local niwa--session session)
          (should-not (niwa-reminders-prepare-edited-files
                       (gptel-make-fsm :info (list :buffer (current-buffer)))
                       (lambda (_) (setq ran t))))
          (should ran))
      (delete-directory tmp-root t)))

  :doc "a cancelled preparation stops its diff and never continues"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file))
         diff-callback stopped ran)
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "goodbye\n"))
            (set-file-times file future))
          (with-temp-buffer
            (niwa-session-add-reminder session r)
            (setq-local niwa--session session)
            (cl-letf (((symbol-function 'niwa-generate-diff)
                       (lambda (_old _new _path callback)
                         (setq diff-callback callback)
                         (lambda () (setq stopped t)))))
              (let ((cancel (niwa-reminders-prepare-edited-files
                             (gptel-make-fsm :info (list :buffer (current-buffer)))
                             (lambda (_) (setq ran t)))))
                (should (functionp cancel))
                (should-not ran)
                (should (funcall cancel))
                (should stopped)
                (should-not (funcall cancel))
                ;; A late settlement cannot revive the dispatch.
                (funcall diff-callback "late diff" nil)
                (should-not ran)))))
      (delete-directory tmp-root t)))

  :doc "memoizes detect-external-changes across trigger and content"
  (let* ((tmp-root (file-name-as-directory (make-temp-file "niwa-ef-" t)))
         (file (expand-file-name "foo.txt" tmp-root))
         (ws (niwa-workspace-get-or-create
              'project tmp-root tmp-root "ef"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-edited-file))
         (call-count 0)
         (real-detect (symbol-function
                       'niwa-file-cache-detect-external-changes)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (niwa-file-cache-put
           (niwa-workspace-file-cache ws)
           (niwa-file-state-from-file file))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file file (insert "goodbye\n"))
            (set-file-times file future))
          (cl-letf (((symbol-function 'niwa-file-cache-detect-external-changes)
                     (lambda (cache)
                       (cl-incf call-count)
                       (funcall real-detect cache))))
            ;; Trigger + content on a single firing should only hit
            ;; detect-external-changes once.
            (should (niwa-reminders--should-fire-p r 0 session))
            (test-niwa-reminders--content r session)
            (should (= 1 call-count))
            ;; A fresh trigger on the next turn recomputes; no changes
            ;; this time, so content is not called.
            (setq call-count 0)
            (should-not (niwa-reminders--should-fire-p r 1 session))
            (should (= 1 call-count))))
      (delete-directory tmp-root t))))


(niwa-deftest niwa-reminders-make-pending-events
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "fires for queued pending reminders and consumes them"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-pending-events)))
    (niwa-session-enqueue-pending-reminder session "first")
    (niwa-session-enqueue-pending-reminder session "second")
    (should (niwa-reminders--should-fire-p r 0 session))
    (should (equal "first\n\nsecond"
                   (test-niwa-reminders--content r session)))
    (should-not (niwa-session-pending-reminders session))
    (should-not (niwa-reminders--should-fire-p r 1 session))))

(niwa-deftest niwa-reminders-make-date-change
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "fires when the observed date differs and updates the session"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (today (format-time-string "%F"))
         (r (niwa-reminders-make-date-change)))
    (setf (niwa-session-last-observed-date session) "2000-01-01")
    (should (niwa-reminders--should-fire-p r 0 session))
    (let ((body (test-niwa-reminders--content r session)))
      (should (string-match-p "2000-01-01" body))
      (should (string-match-p today body)))
    (should (equal today (niwa-session-last-observed-date session)))
    (should-not (niwa-reminders--should-fire-p r 1 session))))

(niwa-deftest niwa-reminders-make-context-pressure
  ()
  ,test
  (test)

  :doc "compaction-available fires when auto-compaction is enabled near threshold"
  (let ((r (niwa-reminders-make-compaction-available 0.70))
        (niwa-compact-auto t)
        (niwa--compact-auto-disabled nil))
    (cl-letf (((symbol-function 'niwa-reminders--compact-token-state)
               (lambda ()
                 '(:tokens 75 :threshold 80 :usable 100 :ratio 0.75)))
              ((symbol-function 'niwa-reminders--compact-auto-available-p)
               (lambda () t)))
      (should (niwa-reminders--should-fire-p r 0 nil))
      (should (string-match-p "Automatic compaction is available"
                              (funcall (niwa-reminder-content r) nil)))))

  :doc "token-usage fires near high context pressure"
  (let ((r (niwa-reminders-make-token-usage 0.90 4)))
    (cl-letf (((symbol-function 'niwa-reminders--compact-token-state)
               (lambda ()
                 '(:tokens 95 :threshold 80 :usable 100 :ratio 0.95))))
      (should (niwa-reminders--should-fire-p r 0 nil))
      (should (string-match-p "Context pressure is high"
                              (funcall (niwa-reminder-content r) nil)))))

  :doc "compaction-available does not fire when auto-compaction is ineligible"
  (let ((r (niwa-reminders-make-compaction-available 0.70)))
    (cl-letf (((symbol-function 'niwa-reminders--compact-token-state)
               (lambda ()
                 '(:tokens 75 :threshold 80 :usable 100 :ratio 0.75)))
              ((symbol-function 'niwa-reminders--compact-auto-available-p)
               (lambda () nil)))
      (should-not (niwa-reminders--should-fire-p r 0 nil))))

  :doc "compaction availability checks chat-buffer-local eligibility"
  (let ((chat (generate-new-buffer " *niwa-compact-reminder-chat*"))
        (r (niwa-reminders-make-compaction-available 0.70)))
    (unwind-protect
        (let ((niwa-reminders--current-chat-buffer chat))
          (with-current-buffer chat
            (setq-local niwa--compact-auto-disabled t))
          (cl-letf (((symbol-function 'niwa-reminders--compact-token-state)
                     (lambda ()
                       '(:tokens 75 :threshold 80 :usable 100 :ratio 0.75)))
                    ((symbol-function 'niwa--compact-auto-eligible-p)
                     (lambda ()
                       (not (bound-and-true-p
                             niwa--compact-auto-disabled)))))
            (should-not (niwa-reminders--should-fire-p r 0 nil))))
      (kill-buffer chat))))

(niwa-deftest niwa-reminders-make-agent-listing-delta
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "initializes silently, then reports added and removed agent types"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (r (niwa-reminders-make-agent-listing-delta))
         (buf (generate-new-buffer " *niwa-agent-delta*")))
    (unwind-protect
        (let ((niwa-reminders--current-chat-buffer buf))
          (with-current-buffer buf
            (setq-local niwa-agents--specs
                        '(("explorer" . (:description "Explore")))))
          (should-not (niwa-reminders--should-fire-p r 0 session))
          (should (equal '(("explorer" . "Explore"))
                         (niwa-session-agent-types-snapshot session)))
          (with-current-buffer buf
            (setq-local niwa-agents--specs
                        '(("verifier" . (:description "Verify")))))
          (should (niwa-reminders--should-fire-p r 1 session))
          (let* ((result (funcall (niwa-reminder-content r) session))
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
            (let ((niwa-reminders--current-chat-buffer nil))
              (funcall commit))
            (should (equal '(("verifier" . "Verify"))
                           (niwa-session-agent-types-snapshot session)))))
      (kill-buffer buf))))


(niwa-deftest niwa-reminders-install-defaults
  (:after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "installs default session reminders"
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws)))
    (niwa-reminders-install-defaults session)
    (let ((types (mapcar #'niwa-reminder-type
                         (niwa-session-reminders session))))
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
  (let* ((ws (niwa-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws)))
    (niwa-reminders-install-defaults session)
    (niwa-reminders-install-defaults session)
    (let* ((types (mapcar #'niwa-reminder-type
                          (niwa-session-reminders session)))
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


(niwa-deftest niwa-agent-invocation-create/max-turns
  (:before-each (niwa-test--capture-agent-registry)
   :after-each (niwa-test--restore-agent-registry))
  ,test
  (test)

  :doc "prepends max-turns-warning reminder when agent has max-turns"
  (let* ((_ (niwa-define-agent cap-agent
              :description "d"
              :tools nil
              :max-turns 20))
         (agent (niwa-agent-get "cap-agent"))
         (inv (niwa-agent-invocation-create agent))
         (types (mapcar #'niwa-reminder-type
                        (niwa-agent-invocation-reminders inv))))
    (should (memq 'max-turns-warning types)))

  :doc "does not add max-turns-warning when agent has no max-turns"
  (let* ((_ (niwa-define-agent nocap-agent
              :description "d"
              :tools nil))
         (agent (niwa-agent-get "nocap-agent"))
         (inv (niwa-agent-invocation-create agent))
         (types (mapcar #'niwa-reminder-type
                        (niwa-agent-invocation-reminders inv))))
    (should-not (memq 'max-turns-warning types))))


(provide 'test-niwa-reminders)

;;; test-niwa-reminders.el ends here
