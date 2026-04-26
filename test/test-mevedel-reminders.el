;;; test-mevedel-reminders.el --- Tests for mevedel-reminders.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'gptel-request)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-permissions)
(require 'mevedel-file-state)
(require 'mevedel-tool-fs)
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
            :interval 3)))
    (should (eq 'demo (mevedel-reminder-type r)))
    (should (functionp (mevedel-reminder-trigger r)))
    (should (functionp (mevedel-reminder-content r)))
    (should (equal 3 (mevedel-reminder-interval r)))
    (should (null (mevedel-reminder-last-fired r))))

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
                 :interval 'bogus)))


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
             :type 'a :trigger trigger :content content :interval 7))
         (_  (setf (mevedel-reminder-last-fired r) 42))
         (clone (mevedel-reminder-clone r)))
    (should (eq 'a (mevedel-reminder-type clone)))
    (should (eq trigger (mevedel-reminder-trigger clone)))
    (should (eq content (mevedel-reminder-content clone)))
    (should (equal 7 (mevedel-reminder-interval clone)))
    (should (null (mevedel-reminder-last-fired clone))))

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

(mevedel-deftest mevedel-reminders--format-block
  (:doc "`mevedel-reminders--format-block' wraps content in XML tags")
  (should (equal "<system-reminder>\nhello\n</system-reminder>"
                 (mevedel-reminders--format-block "hello"))))


;;
;;; Collect

(mevedel-deftest mevedel-reminders--collect
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "collects blocks for all firing reminders and updates last-fired"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r1 (mevedel-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "one")))
         (r2 (mevedel-reminder-create :type 'b
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "two"))))
    (mevedel-session-add-reminder session r1)
    (mevedel-session-add-reminder session r2)
    (setf (mevedel-session-turn-count session) 7)
    (let ((blocks (mevedel-reminders--collect session)))
      (should (equal (list "<system-reminder>\none\n</system-reminder>"
                           "<system-reminder>\ntwo\n</system-reminder>")
                     blocks))
      (should (equal 7 (mevedel-reminder-last-fired r1)))
      (should (equal 7 (mevedel-reminder-last-fired r2)))))

  :doc "skips reminders whose trigger returns nil"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r1 (mevedel-reminder-create :type 'a
                                      :trigger (lambda (_) t)
                                      :content (lambda (_) "one")))
         (r2 (mevedel-reminder-create :type 'b
                                      :trigger (lambda (_) nil)
                                      :content (lambda (_) "two"))))
    (mevedel-session-add-reminder session r1)
    (mevedel-session-add-reminder session r2)
    (let ((blocks (mevedel-reminders--collect session)))
      (should (equal (list "<system-reminder>\none\n</system-reminder>") blocks))
      (should (null (mevedel-reminder-last-fired r2)))))

  :doc "throttles firing by interval across multiple collects"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminder-create :type 'a
                                     :trigger (lambda (_) t)
                                     :content (lambda (_) "x")
                                     :interval 3)))
    (mevedel-session-add-reminder session r)
    ;; Turn 0: first collect fires (last-fired is nil)
    (setf (mevedel-session-turn-count session) 0)
    (should (equal 1 (length (mevedel-reminders--collect session))))
    (should (equal 0 (mevedel-reminder-last-fired r)))
    ;; Turn 1: interval not yet elapsed
    (setf (mevedel-session-turn-count session) 1)
    (should (null (mevedel-reminders--collect session)))
    ;; Turn 3: interval elapsed, fires again
    (setf (mevedel-session-turn-count session) 3)
    (should (equal 1 (length (mevedel-reminders--collect session))))
    (should (equal 3 (mevedel-reminder-last-fired r))))

  :doc "trigger and content functions receive the session struct"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (seen-trigger nil)
         (seen-content nil)
         (r (mevedel-reminder-create
             :type 'a
             :trigger (lambda (s) (setq seen-trigger s) t)
             :content (lambda (s) (setq seen-content s) "ok"))))
    (mevedel-session-add-reminder session r)
    (mevedel-reminders--collect session)
    (should (eq seen-trigger session))
    (should (eq seen-content session))))


;;
;;; Transform (integration with chat buffer)

(mevedel-deftest mevedel-reminders--transform
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "`mevedel-reminders--transform' prepends blocks to the prompt via FSM"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*"))
         (prompt-buf (generate-new-buffer " *mevedel-test-prompt*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (mevedel-session-add-reminder
             session
             (mevedel-reminder-create
              :type 'demo
              :trigger (lambda (_) t)
              :content (lambda (_) "REMIND"))))
          (with-current-buffer prompt-buf
            (insert "user prompt body")
            (mevedel-reminders--transform fsm)
            (should (string-match-p
                     "<system-reminder>\nREMIND\n</system-reminder>\nuser prompt body"
                     (buffer-string)))))
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
         (inv (mevedel-agent-invocation-create agent)))
    (should (eq agent (mevedel-agent-invocation-agent inv)))
    (should (equal 0 (mevedel-agent-invocation-turn-count inv)))
    (should (= 1 (length (mevedel-agent-invocation-reminders inv))))
    ;; clone is not eq to original
    (should-not (eq r (car (mevedel-agent-invocation-reminders inv)))))

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
         (inv-b (mevedel-agent-invocation-create agent)))
    (mevedel-reminders--collect-from
     (mevedel-agent-invocation-reminders inv-a) 0 inv-a)
    (should (equal 0 (mevedel-reminder-last-fired
                      (car (mevedel-agent-invocation-reminders inv-a)))))
    (should (null (mevedel-reminder-last-fired
                   (car (mevedel-agent-invocation-reminders inv-b)))))))


(mevedel-deftest mevedel-tools--handle-wait-inject
  (:after-each (setq mevedel-agent--registry nil))
  ,test
  (test)

  :doc "prepends a user-role message block built from firing reminders on first turn"
  (let* ((r (mevedel-reminder-create
             :type 'note
             :trigger (lambda (_) t)
             :content (lambda (_) "REMIND-ME")))
         (_ (mevedel-define-agent wait-agent
              :description "WAIT inject test"
              :tools nil
              :reminders (list r)))
         (agent (mevedel-agent-get "wait-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mevedel-test-ov*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (data (list :messages (vector (list :role "user"
                                                   :content "original"))))
               (fsm (gptel-make-fsm
                     :info (list :context ov
                                 :backend nil
                                 :data data))))
          (overlay-put ov 'mevedel-agent-invocation inv)
          (mevedel-tools--handle-wait-inject fsm)
          ;; Turn count incremented
          (should (equal 1 (mevedel-agent-invocation-turn-count inv)))
          ;; Message vector grew by 1 user-role block.  On the first
          ;; WAIT cycle the reminder is injected ahead of the task
          ;; prompt (audit-log-friendly order).
          (let ((msgs (plist-get data :messages)))
            (should (equal 2 (length msgs)))
            (should (equal "user" (plist-get (aref msgs 0) :role)))
            (should (string-match-p "REMIND-ME"
                                    (plist-get (aref msgs 0) :content)))
            (should (equal "original" (plist-get (aref msgs 1) :content)))))
      (kill-buffer ov-buf)))

  :doc "advances turn count even when no reminders fire"
  (let* ((_ (mevedel-define-agent wait-quiet-agent
              :description "No reminders"
              :tools nil))
         (agent (mevedel-agent-get "wait-quiet-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mevedel-test-ov*")))
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
          (mevedel-tools--handle-wait-inject fsm)
          (should (equal 1 (mevedel-agent-invocation-turn-count inv)))
          ;; Messages untouched
          (should (equal 0 (length (plist-get data :messages)))))
      (kill-buffer ov-buf)))

  :doc "is a no-op when FSM has no agent invocation"
  (let* ((data (list :messages (vector (list :role "user" :content "x"))))
         (fsm (gptel-make-fsm :info (list :data data))))
    (mevedel-tools--handle-wait-inject fsm)
    (should (equal 1 (length (plist-get data :messages)))))

  :doc "respects interval throttling across multiple WAIT cycles"
  (let* ((r (mevedel-reminder-create
             :type 'throttled
             :trigger (lambda (_) t)
             :content (lambda (_) "TICK")
             :interval 2))
         (_ (mevedel-define-agent wait-throttle-agent
              :description "Interval test"
              :tools nil
              :reminders (list r)))
         (agent (mevedel-agent-get "wait-throttle-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mevedel-test-ov*")))
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
          ;; 4 cycles at interval 2: fires on 0 and 2 (2 injections).
          (dotimes (_ 4) (mevedel-tools--handle-wait-inject fsm))
          (should (equal 4 (mevedel-agent-invocation-turn-count inv)))
          (should (equal 2 (length (plist-get data :messages)))))
      (kill-buffer ov-buf))))


(mevedel-deftest mevedel-tools--augment-agent-handlers
  ()
  ,test
  (test)

  :doc "prepend merges new handlers at the head of matching state entries"
  (let* ((h (lambda (_fsm) 'extra))
         (base `((WAIT ,#'ignore)
                 (DONE ,#'car)
                 (ERRS ,#'cdr)))
         (result (mevedel-tools--augment-agent-handlers
                  base :prepend `((WAIT . (,h))))))
    (should (equal (list h #'ignore) (cdr (assq 'WAIT result))))
    (should (equal (list #'car) (cdr (assq 'DONE result))))
    (should (equal (list #'cdr) (cdr (assq 'ERRS result))))
    ;; original alist not mutated
    (should (equal (list #'ignore) (cdr (assq 'WAIT base)))))

  :doc "append merges new handlers at the tail of matching state entries"
  (let* ((h (lambda (_fsm) 'extra))
         (base `((WAIT ,#'ignore)
                 (DONE ,#'car)))
         (result (mevedel-tools--augment-agent-handlers
                  base :append `((DONE . (,h)) (ERRS . (,h))))))
    (should (equal (list #'car h) (cdr (assq 'DONE result))))
    (should (equal (list h) (cdr (assq 'ERRS result))))
    (should (equal (list #'ignore) (cdr (assq 'WAIT result)))))

  :doc "creates missing state entries for both prepend and append"
  (let* ((h (lambda (_fsm) 'extra))
         (base `((WAIT ,#'ignore)))
         (result (mevedel-tools--augment-agent-handlers
                  base
                  :prepend `((TOOL . (,h)))
                  :append `((DONE . (,h))))))
    (should (equal (list h) (cdr (assq 'TOOL result))))
    (should (equal (list h) (cdr (assq 'DONE result))))))



;;
;;; Tier 1 built-in reminders

(mevedel-deftest mevedel-reminders-make-mode-constraints
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when session permission mode is `default'"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-mode-constraints))
         (mevedel-permission-mode 'default))
    (should-not (mevedel-reminders--should-fire-p r 0 session)))

  :doc "fires when session permission mode is non-default"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-mode-constraints)))
    (setf (mevedel-session-permission-mode session) 'plan)
    (should (mevedel-reminders--should-fire-p r 0 session))
    (should (string-match-p "plan"
                            (funcall (mevedel-reminder-content r) session))))

  :doc "content varies by mode and falls back for unknown modes"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-mode-constraints)))
    (setf (mevedel-session-permission-mode session) 'accept-edits)
    (should (string-match-p "accept-edits"
                            (funcall (mevedel-reminder-content r) session)))
    (setf (mevedel-session-permission-mode session) 'weird-mode)
    (should (string-match-p "weird-mode"
                            (funcall (mevedel-reminder-content r) session))))

  :doc "default interval throttles to 5 turns"
  (let ((r (mevedel-reminders-make-mode-constraints)))
    (should (equal 5 (mevedel-reminder-interval r)))))


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
    (should (string-match-p "VERIFICATION" body))))


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
                            (funcall (mevedel-reminder-content r) session)))))


(mevedel-deftest mevedel-reminders-make-task-nudge
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when session has no tasks"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge)))
    (should-not (funcall (mevedel-reminder-trigger r) session)))

  :doc "fires when session has non-completed tasks"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge)))
    (push (mevedel-task--create :id 1 :subject "do thing" :status 'in-progress)
          (mevedel-session-tasks session))
    (should (funcall (mevedel-reminder-trigger r) session))
    (should (string-match-p "task" (funcall (mevedel-reminder-content r) session))))

  :doc "does not fire when all tasks are completed"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/tn/" "/tmp/tn/" "tn"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-task-nudge)))
    (push (mevedel-task--create :id 1 :subject "done thing" :status 'completed)
          (mevedel-session-tasks session))
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


(mevedel-deftest mevedel-reminders-make-diagnostics
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "does not fire when no workspace buffers have diagnostics"
  (let* ((tmp (make-temp-file "mevedel-diag-" t))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "diag"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-diagnostics)))
    (unwind-protect
        (should-not (mevedel-reminders--should-fire-p r 0 session))
      (delete-directory tmp t)))

  :doc "fires and formats when a workspace buffer has Flymake diagnostics"
  (let* ((tmp (make-temp-file "mevedel-diag-" t))
         (file (expand-file-name "foo.el" tmp))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "diag"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-diagnostics))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          ;; Emulate flymake-mode being active with one diagnostic
          (setq-local flymake-mode t)
          (cl-letf (((symbol-function 'flymake-diagnostics)
                     (lambda (&rest _)
                       (list (list 'mock-diag)))))
            (cl-letf (((symbol-function 'flymake-diagnostic-beg)
                       (lambda (_) (point-min)))
                      ((symbol-function 'flymake-diagnostic-type)
                       (lambda (_) :error))
                      ((symbol-function 'flymake-diagnostic-text)
                       (lambda (_) "bad thing")))
              (should (mevedel-reminders--should-fire-p r 0 session))
              (let ((content (funcall (mevedel-reminder-content r) session)))
                (should (string-match-p "bad thing" content))
                (should (string-match-p "error" content))
                (should (string-match-p "foo\\.el" content))))))
      (kill-buffer buf)
      (delete-directory tmp t)))

  :doc "fires and formats when a workspace buffer has Flycheck errors"
  (let* ((tmp (make-temp-file "mevedel-diag-" t))
         (file (expand-file-name "bar.el" tmp))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "diag"))
         (session (mevedel-session-create "main" ws))
         (r (mevedel-reminders-make-diagnostics))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local flycheck-mode t)
          (cl-letf (((symbol-function 'flycheck-overlay-errors-in)
                     (lambda (&rest _) (list (list 'mock-err))))
                    ((symbol-function 'flycheck-error-line)
                     (lambda (_) 42))
                    ((symbol-function 'flycheck-error-level)
                     (lambda (_) 'warning))
                    ((symbol-function 'flycheck-error-message)
                     (lambda (_) "style nit")))
            (should (mevedel-reminders--should-fire-p r 0 session))
            (let ((content (funcall (mevedel-reminder-content r) session)))
              (should (string-match-p "style nit" content))
              (should (string-match-p "warning" content))
              (should (string-match-p "42" content))
              (should (string-match-p "bar\\.el" content)))))
      (kill-buffer buf)
      (delete-directory tmp t)))

  :doc "merges Flymake and Flycheck output from the same buffer"
  (let* ((tmp (make-temp-file "mevedel-diag-" t))
         (file (expand-file-name "both.el" tmp))
         (ws (mevedel-workspace-get-or-create
              'project (file-name-as-directory tmp)
              (file-name-as-directory tmp) "diag"))
         (session (mevedel-session-create "main" ws))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local flymake-mode t)
          (setq-local flycheck-mode t)
          (cl-letf (((symbol-function 'flymake-diagnostics)
                     (lambda (&rest _) (list 'fm)))
                    ((symbol-function 'flymake-diagnostic-beg)
                     (lambda (_) (point-min)))
                    ((symbol-function 'flymake-diagnostic-type)
                     (lambda (_) :error))
                    ((symbol-function 'flymake-diagnostic-text)
                     (lambda (_) "flymake-msg"))
                    ((symbol-function 'flycheck-overlay-errors-in)
                     (lambda (&rest _) (list 'fc)))
                    ((symbol-function 'flycheck-error-line)
                     (lambda (_) 7))
                    ((symbol-function 'flycheck-error-level)
                     (lambda (_) 'warning))
                    ((symbol-function 'flycheck-error-message)
                     (lambda (_) "flycheck-msg")))
            (let ((diags (mevedel-reminders--collect-diagnostics session)))
              (should (= 2 (length diags)))
              (should (cl-some (lambda (d) (string-match-p "flymake-msg" (nth 3 d)))
                               diags))
              (should (cl-some (lambda (d) (string-match-p "flycheck-msg" (nth 3 d)))
                               diags)))))
      (kill-buffer buf)
      (delete-directory tmp t))))


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
      (should (string-match-p "ToolSearch" body)))
    ;; Simulate firing bookkeeping and verify one-shot prevents re-fire.
    (setf (mevedel-reminder-last-fired r) 0)
    (should-not (mevedel-reminders--should-fire-p r 5 session))))

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
      (should (string-match-p "ToolSearch" body)))
    ;; Content function consumed the list, so the reminder no longer fires.
    (should-not (mevedel-session-deferred-expired session))
    (should-not (mevedel-reminders--should-fire-p r 2 session))))


(mevedel-deftest mevedel-reminders-install-defaults
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "installs mode-constraints, diagnostics, edited-file, deferred-tools, and task-nudge reminders"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws)))
    (mevedel-reminders-install-defaults session)
    (let ((types (mapcar #'mevedel-reminder-type
                         (mevedel-session-reminders session))))
      (should (memq 'mode-constraints types))
      (should (memq 'diagnostics types))
      (should (memq 'edited-file types))
      (should (memq 'deferred-tools-roster types))
      (should (memq 'deferred-tools-expired types))
      (should (memq 'task-nudge types))))

  :doc "is idempotent - does not double-register"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws)))
    (mevedel-reminders-install-defaults session)
    (mevedel-reminders-install-defaults session)
    (let* ((types (mapcar #'mevedel-reminder-type
                          (mevedel-session-reminders session)))
           (mode-count (cl-count 'mode-constraints types))
           (diag-count (cl-count 'diagnostics types))
           (edit-count (cl-count 'edited-file types))
           (roster-count (cl-count 'deferred-tools-roster types))
           (expired-count (cl-count 'deferred-tools-expired types))
           (nudge-count (cl-count 'task-nudge types)))
      (should (= 1 mode-count))
      (should (= 1 diag-count))
      (should (= 1 edit-count))
      (should (= 1 roster-count))
      (should (= 1 expired-count))
      (should (= 1 nudge-count)))))


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
