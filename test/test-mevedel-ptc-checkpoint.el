;;; test-mevedel-ptc-checkpoint.el -- Durable ToolCall checkpoint tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the compact ToolCall checkpoint lifecycle and transcript recovery.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-ptc-checkpoint)
(require 'mevedel-agents)
(require 'mevedel-structs)
(require 'mevedel-tool-render-data)

(defun test-mevedel-ptc-checkpoint--session (&optional checkpoints)
  "Return a passive session containing CHECKPOINTS."
  (mevedel-session--create :name "test" :ptc-checkpoints checkpoints))


;;
;;; Checkpoint lifecycle

(mevedel-deftest mevedel-ptc-checkpoint--put ()
  ,test
  (test)
  :doc "replaces the checkpoint with the same identity"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "p/1" :state running :value old)))))
    (mevedel-ptc-checkpoint--put
     session '(:id "p/1" :state settled :value new))
    (should (equal '((:id "p/1" :state settled :value new))
                   (mevedel-session-ptc-checkpoints session)))))

(mevedel-deftest mevedel-ptc-checkpoint-start ()
  ,test
  (test)
  :doc "does nothing without a durable session"
  (should-not (mevedel-ptc-checkpoint-start nil nil "p" "(+ 1 2)"))
  :doc "rolls back when no authoritative buffer can publish the checkpoint"
  (let ((session (test-mevedel-ptc-checkpoint--session)))
    (should-not
     (mevedel-ptc-checkpoint-start session nil "p" "(+ 1 2)"))
    (should-not (mevedel-session-ptc-checkpoints session)))
  :doc "records into an existing sidecar without a full save"
  (let ((session (test-mevedel-ptc-checkpoint--session))
        written)
    (cl-letf (((symbol-function 'mevedel-session-persistence-write-sidecar-now)
               (lambda (s _buffer)
                 (setq written (mevedel-session-ptc-checkpoints s))
                 t))
              ((symbol-function 'mevedel-session-artifacts-save)
               (lambda (&rest _) (error "Full save for an existing sidecar"))))
      (should (mevedel-ptc-checkpoint-start session nil "p" "(+ 1 2)"))
      (should (equal '("p") (mapcar (lambda (c) (plist-get c :id)) written)))
      (should (equal "/root" (plist-get (car written) :conversation)))))
  :doc "materializes a fresh session through the full save"
  (let ((session (test-mevedel-ptc-checkpoint--session))
        saved)
    (cl-letf (((symbol-function 'mevedel-session-persistence-write-sidecar-now)
               (lambda (&rest _) nil))
              ((symbol-function 'mevedel-session-artifacts-save)
               (lambda (&rest args) (setq saved args) t)))
      (should (mevedel-ptc-checkpoint-start session nil "p" "(+ 1 2)"))
      (should (equal (list session nil nil t) saved))
      (should (mevedel-session-ptc-checkpoints session)))))

(mevedel-deftest mevedel-ptc-checkpoint--conversation ()
  ,test
  (test)
  :doc "uses the originating agent path but persists through the root buffer"
  (let ((session (test-mevedel-ptc-checkpoint--session))
        (root (generate-new-buffer " *ptc-root*"))
        (agent (generate-new-buffer " *ptc-agent*"))
        buffers)
    (unwind-protect
        (progn
          (setf (mevedel-session-root-buffer session) root)
          (with-current-buffer agent
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create :path "/root/worker")))
          (cl-letf (((symbol-function 'mevedel-session-persistence-write-sidecar-now)
                     (lambda (_session buffer) (push buffer buffers) t)))
            (should (mevedel-ptc-checkpoint-start session agent "p" "1"))
            (should (equal "/root/worker"
                           (plist-get (car (mevedel-session-ptc-checkpoints session))
                                      :conversation)))
            (should (mevedel-ptc-checkpoint-update
                     session agent "p" '(:state settled)))
            (should (equal (list root root) buffers))))
      (kill-buffer agent)
      (kill-buffer root)))
  :doc "rejects an invocation without a canonical retained path"
  (with-temp-buffer
    (setq-local mevedel--agent-invocation
                (mevedel-agent-invocation--create :path "worker"))
    (should-error (mevedel-ptc-checkpoint--conversation (current-buffer)))))

(mevedel-deftest mevedel-ptc-checkpoint-update ()
  ,test
  (test)
  :doc "treats an absent session as already durable"
  (should (mevedel-ptc-checkpoint-update nil nil "p" '(:state settled)))
  :doc "rolls memory back when the sidecar rewrite fails"
  (let* ((before '((:id "p" :state running)))
         (session (test-mevedel-ptc-checkpoint--session before)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence-write-sidecar-now)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-ptc-checkpoint-update
        session nil "p" '(:state settled)))
      (should (equal before
                     (mevedel-session-ptc-checkpoints session))))))

(mevedel-deftest mevedel-ptc-checkpoint-note ()
  ,test
  (test)
  :doc "treats an absent session as recorded"
  (should (mevedel-ptc-checkpoint-note nil "p" '(:state settled)))
  :doc "merges into memory without any sidecar write"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "p" :state running :render-data nil))))
        (wrote nil))
    (cl-letf (((symbol-function
                'mevedel-session-persistence-write-sidecar-now)
               (lambda (&rest _) (setq wrote t))))
      (should (mevedel-ptc-checkpoint-note
               session "p" '(:render-data (:kind ptc :outcome running))))
      (should-not wrote)
      (should (equal '(:kind ptc :outcome running)
                     (plist-get
                      (car (mevedel-session-ptc-checkpoints session))
                      :render-data)))))
  :doc "returns nil for an unknown checkpoint id"
  (should-not (mevedel-ptc-checkpoint-note
               (test-mevedel-ptc-checkpoint--session)
               "missing" '(:state settled))))

(mevedel-deftest mevedel-ptc-checkpoint-clear-settled ()
  ,test
  (test)
  :doc "defers only root's settled rows until the root transcript commits"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "running" :conversation "/root" :state running)
            (:id "settled" :conversation "/root" :state settled)
            (:id "agent" :conversation "/root/worker" :state settled)))))
    (with-temp-buffer
      (insert (propertize "settled root tool" 'gptel '(tool . "settled")))
      (mevedel-ptc-checkpoint-clear-settled session))
    (should (equal '("running" "agent")
                   (mapcar (lambda (row) (plist-get row :id))
                           (mevedel-session-ptc-checkpoints session))))
    (let* ((raw (list :ptc-checkpoints (mevedel-session-ptc-checkpoints session)))
           (ordinary (mevedel-session-checkpoint-project session raw))
           (candidate (mevedel-session-checkpoint-capture session "/root"))
           (owned (mevedel-session-checkpoint-project session raw candidate)))
      (should (= 3 (length (plist-get ordinary :ptc-checkpoints))))
      (should (= 2 (length (plist-get owned :ptc-checkpoints))))
      (mevedel-session-checkpoint-install session candidate)
      (should (equal (plist-get owned :ptc-checkpoints)
                     (plist-get (mevedel-session-checkpoint-project session raw)
                                :ptc-checkpoints)))))
  :doc "an agent's completed turn cannot retire root or sibling checkpoints"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "root" :conversation "/root" :state settled)
            (:id "agent" :conversation "/root/worker" :state settled)
            (:id "sibling" :conversation "/root/other" :state settled)))))
    (with-temp-buffer
      (insert (propertize "settled agent tool" 'gptel '(tool . "agent")))
      (mevedel-ptc-checkpoint-clear-settled session "/root/worker"))
    (should (equal '("root" "sibling")
                   (mapcar (lambda (row) (plist-get row :id))
                           (mevedel-session-ptc-checkpoints session)))))
  :doc "a post-tool save cannot retire a settled row not yet in its transcript"
  (let* ((row '(:id "agent" :conversation "/root/worker" :state settled))
         (session (test-mevedel-ptc-checkpoint--session (list row))))
    (with-temp-buffer
      (mevedel-ptc-checkpoint-clear-settled session "/root/worker")
      (should (equal (list row) (mevedel-session-ptc-checkpoints session)))
      (let ((candidate (mevedel-session-checkpoint-capture session "/root/worker")))
        (should (equal
                 (list row)
                 (plist-get
                  (mevedel-session-checkpoint-project
                   session (list :ptc-checkpoints (list row)) candidate)
                  :ptc-checkpoints)))))))


;;
;;; Restore

(mevedel-deftest mevedel-ptc-checkpoint--interrupted-render-data ()
  ,test
  (test)
  :doc "marks unfinished children and the envelope interrupted"
  (let* ((checkpoint
          '(:render-data
            (:kind ptc :outcome running
             :calls ((:id "p/1" :status running)
                     (:id "p/2" :status success)
                     (:id "p/3" :status queued)))))
         (before (copy-tree checkpoint t))
         (data (mevedel-ptc-checkpoint--interrupted-render-data checkpoint)))
    (should (eq 'interrupted (plist-get data :outcome)))
    (should (equal checkpoint before))
    (should (= 3 (plist-get data :nested-call-count)))
    (should (equal '(interrupted success interrupted)
                   (mapcar (lambda (call) (plist-get call :status))
                           (plist-get data :calls))))
    (should (equal '(:outcome interrupted :calls nil :nested-call-count 0)
                   (mevedel-ptc-checkpoint--interrupted-render-data nil)))))

(mevedel-deftest mevedel-ptc-checkpoint--insert ()
  ,test
  (test)
  :doc "appends a parseable interrupted ToolCall tool block"
  (with-temp-buffer
    (mevedel-ptc-checkpoint--insert
     '(:id "p" :args (:expression "(+ 1 2)") :state running
       :render-data (:kind ptc :outcome running :calls nil)))
    (should (string-match-p "^#\\+begin_tool (ToolCall recovered"
                            (buffer-string)))
    (should (string-match-p "script interrupted by Emacs restart"
                            (buffer-string)))
    (should (mevedel-tool-render-data-segment-bounds "p"))
    (should (plist-get
             (cdr (mevedel-tool-render-data-extract (buffer-string) nil "p" t))
             :kind)))
  :doc "escapes settled results that contain Org block terminators"
  (with-temp-buffer
    (mevedel-ptc-checkpoint--insert
     '(:id "p" :args (:expression "1") :state settled
       :result "before\n#+end_tool\nafter"
       :render-data (:kind ptc :outcome completed :calls nil)))
    (goto-char (point-min))
    (should (= 1 (how-many "^#\\+end_tool$")))
    (should (search-forward ",#+end_tool" nil t))
    (should (mevedel-tool-render-data-segment-bounds "p"))))

(mevedel-deftest mevedel-ptc-checkpoint-reconcile ()
  ,test
  (test)
  :doc "inserts only missing rows and consumes every checkpoint"
  (let* ((checkpoints
          '((:id "present" :conversation "/root" :args (:expression "1") :state settled
             :result "1" :render-data (:kind ptc :outcome completed :calls nil))
            (:id "missing" :conversation "/root" :args (:expression "2") :state running
             :render-data (:kind ptc :outcome running :calls nil))))
         (session (test-mevedel-ptc-checkpoint--session checkpoints)))
    (with-temp-buffer
      (insert (propertize "existing" 'gptel '(tool . "present")))
      (should (equal '(1 . 2)
                     (mevedel-ptc-checkpoint-reconcile session)))
      (should (mevedel-tool-render-data-segment-bounds "missing"))
      (should-not (mevedel-session-ptc-checkpoints session))
      (setf (mevedel-session-ptc-checkpoints session) (list (car checkpoints)))
      (should (equal '(0 . 1)
                     (mevedel-ptc-checkpoint-reconcile session)))
      (should-not (mevedel-session-ptc-checkpoints session))))
  :doc "recovery inserts only the receiving conversation and retains until commit"
  (let* ((root-row '(:id "root" :conversation "/root" :args (:expression "1")
                       :state running :render-data (:kind ptc :calls nil)))
         (agent-row '(:id "agent" :conversation "/root/worker" :args (:expression "2")
                        :state settled :result "2" :render-data (:kind ptc :calls nil)))
         (session (test-mevedel-ptc-checkpoint--session (list root-row agent-row))))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create :path "/root/worker"))
      (should (equal '(1 . 1) (mevedel-ptc-checkpoint-reconcile session)))
      (should (mevedel-tool-render-data-segment-bounds "agent"))
      (should-not (mevedel-tool-render-data-segment-bounds "root"))
      (should (equal (list root-row) (mevedel-session-ptc-checkpoints session)))
      (let* ((raw (list :ptc-checkpoints (mevedel-session-ptc-checkpoints session)))
             (ordinary (mevedel-session-checkpoint-project session raw))
             (candidate (mevedel-session-checkpoint-capture session "/root/worker")))
        (should (equal (list root-row agent-row) (plist-get ordinary :ptc-checkpoints)))
        (should (equal (list root-row)
                       (plist-get (mevedel-session-checkpoint-project session raw candidate)
                                  :ptc-checkpoints)))
        (mevedel-session-checkpoint-install session candidate)
        (should (equal (list root-row)
                       (plist-get (mevedel-session-checkpoint-project session raw)
                                  :ptc-checkpoints)))))))

(provide 'test-mevedel-ptc-checkpoint)

;;; test-mevedel-ptc-checkpoint.el ends here
