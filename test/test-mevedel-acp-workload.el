;;; test-mevedel-acp-workload.el --- Isolated ACP workloads -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise a workload's private MCP scope over a real protocol subprocess.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-sigpipe-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-sigpipe-test-support"))
(require 'mevedel-acp-workload)

(defconst mevedel-acp-workload-test--file (or load-file-name buffer-file-name)
  "This file, which the SIGPIPE-ignoring child loads.")

(defun mevedel-acp-workload-test--hold (seconds)
  "Keep Emacs from reading any process for SECONDS, as a long command does."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end))))

(defun mevedel-acp-workload-test--run (meta tool &optional control)
  "Run a workload scripted by META with TOOL and optional hook CONTROL.
Return its events, oldest first, once it has completed."
  (with-temp-buffer
    (let (events)
      (mevedel-acp-workload-request
       (lambda (mcp hook)
         (list :command (executable-find "python3") :args (list mevedel-test--acp-peer)
               :cwd temporary-file-directory :mcp mcp :tool-id-field :claudecode/toolUseId
               :control control
               :meta (append `((hookCommand . ,hook)) meta)))
       "batch" (list tool)
       (lambda (response info) (push (list response info) events))
       #'ignore (lambda () nil))
      (mevedel-test--await 10 "Workload did not complete"
        (and events (memq (caar events) '(t nil abort))))
      (ert-info ((format "terminal=%S" (car events)))
        (should (eq t (caar events))))
      (nreverse events))))

;; A reply written after its peer closed kills a batch Emacs that does not
;; ignore SIGPIPE; these workloads run in the child the next case starts.
(when (mevedel-test-sigpipe-child-p)
  (mevedel-deftest mevedel-acp-workload-request/lost-mcp-reply (:quiet t)
    ,test
    (test)

    :doc "a tool result lost with its bridge stays local and the agent completes the workload"
    (let* ((tool (gptel--make-tool-internal
                  :name "Hold" :description "Hold" :args nil
                  :function (lambda ()
                              ;; The bridge dies while the tool works.
                              (mevedel-acp-workload-test--hold 1)
                              "LOCAL-WORKLOAD-RESULT")))
           (events (mevedel-acp-workload-test--run
                    '((dropBridge . t) (responseText . "WORKLOAD-OUTCOME-4417")
                      (toolBatches . [[((name . "Hold") (id . "lost-1") (args . ()))]]))
                    tool))
           (terminal (car (last events))))
      (should (eq t (car terminal)))
      (should (equal '((:kind tool :id 2)) (plist-get (cadr terminal) :mcp-lost)))
      (should (assoc "WORKLOAD-OUTCOME-4417" events))
      (should (cl-some (lambda (event)
                         (and (eq 'tool-result (car-safe (car event)))
                              (equal "LOCAL-WORKLOAD-RESULT" (nth 2 (cadr (car event))))))
                       events)))

    :doc "a hook reply lost at the hook's timeout leaves the agent to complete the workload"
    (let* ((controls 0)
           (tool (gptel--make-tool-internal
                  :name "Hold" :description "Hold" :args nil
                  :function (lambda () "QUICK-RESULT")))
           (events (mevedel-acp-workload-test--run
                    '((hookTimeout . 0.2) (responseText . "WORKLOAD-OUTCOME-5530")
                      (toolBatches . [[((name . "Hold") (id . "quick-1") (args . ()))]]))
                    tool
                    (lambda (_boundary _event)
                      (cl-incf controls)
                      ;; The native hook gives up while this answers.
                      (mevedel-acp-workload-test--hold 1)
                      '(:continue t))))
           (terminal (car (last events))))
      (should (= 1 controls))
      (should (eq t (car terminal)))
      (should (assoc "WORKLOAD-OUTCOME-5530" events))
      (should (equal '((:kind hook :id 1)) (plist-get (cadr terminal) :mcp-lost))))))

(mevedel-deftest mevedel-acp-workload-request ()
  ,test
  (test)
  :doc "workloads whose MCP replies lose their client, where SIGPIPE is ignored"
  (mevedel-test-run-sigpipe-tests mevedel-acp-workload-test--file
                                  "\\`mevedel-acp-workload-request/lost-mcp-reply/")

  :doc "unexpected live-client write failure settles synchronous and asynchronous workloads"
  (dolist (async '(nil t))
    (with-temp-buffer
      (let* ((send (symbol-function 'process-send-string))
             (effects 0) failed terminal cancel-tool-timer
             (tool (gptel--make-tool-internal
                    :name "Reply" :description "Reply" :args nil :async async
                    :function
                    (if async
                        (lambda (callback)
                          (cl-incf effects)
                          (setq cancel-tool-timer
                                (run-at-time 0 nil callback "WORKLOAD-RESULT"))
                          (lambda () (cancel-timer cancel-tool-timer)))
                      (lambda () (cl-incf effects) "WORKLOAD-RESULT")))))
        (unwind-protect
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (process text)
                         (if (and (not failed)
                                  (string-prefix-p "mevedel-mcp" (process-name process))
                                  (string-search "WORKLOAD-RESULT" text))
                             (progn
                               (should (process-live-p process))
                               (setq failed t)
                               (error "Injected live-client write failure"))
                           (funcall send process text)))))
              (mevedel-acp-workload-request
               (lambda (mcp hook)
                 (list :command (executable-find "python3")
                       :args (list mevedel-test--acp-peer)
                       :cwd temporary-file-directory :mcp mcp
                       :tool-id-field :claudecode/toolUseId
                       :meta `((hookCommand . ,hook)
                               (toolBatches . [[((name . "Reply") (id . "reply-1")
                                                 (args . ()))]]))))
               "batch" (list tool)
               (lambda (response info)
                 (when (memq response '(nil t abort))
                   (push (list response info) terminal)))
               #'ignore (lambda () nil))
              (mevedel-test--await 3 "Live write failure stranded the workload" terminal)
              (should failed)
              (should (= effects 1))
              (should (= (length terminal) 1))
              (should-not (caar terminal))
              (should (equal "Injected live-client write failure"
                             (plist-get (cadar terminal) :error))))
          (when cancel-tool-timer (cancel-timer cancel-tool-timer))))))

  :doc "a quitting tool canceller still releases the real MCP socket and client"
  (with-temp-buffer
    (let* ((start (symbol-function 'mevedel-mcp-start))
           server client cancel started terminal (cancelled 0)
           (tool (gptel--make-tool-internal
                  :name "Wait" :description "Wait" :args nil :async t
                  :function
                  (lambda (_callback)
                    (setq started t)
                    (lambda ()
                      (cl-incf cancelled)
                      (signal 'quit '(tool-cleanup)))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-mcp-start)
                     (lambda (&rest args)
                       (setq server (apply start args)))))
            (setq cancel
                  (mevedel-acp-workload-request
                   (lambda (mcp hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-test--acp-peer)
                           :cwd temporary-file-directory :mcp mcp
                           :tool-id-field :claudecode/toolUseId
                           :meta `((hookCommand . ,hook)
                                   (toolBatches . [[((name . "Wait") (id . "wait-1")
                                                     (args . ()))]]))))
                   "batch" (list tool)
                   (lambda (response _info)
                     (when (memq response '(nil t abort))
                       (push response terminal)))
                   nil nil))
            (mevedel-test--await 3 "Tool did not start" started)
            (setq client (car (mevedel-mcp-clients server)))
            (should (process-live-p client))
            (funcall cancel)
            (should (equal terminal '(abort)))
            (should (= cancelled 1))
            (should-not (process-live-p client))
            (should-not (file-exists-p (mevedel-mcp-directory server)))
            (funcall cancel)
            (should (= cancelled 1)))
        (when cancel (funcall cancel))
        ;; Also release evidence left by the negative control.
        (when (and client (process-live-p client)) (delete-process client))
        (when (and server (file-exists-p (mevedel-mcp-directory server)))
          (delete-directory (mevedel-mcp-directory server) t)))))

  :doc "nonlocal exits retire their workload once without masking the original exit"
  (dolist (stage '(launch before-tool tool sync-result async-result stream control))
    (dolist (exit-kind '(quit throw))
      (dolist (cleanup-failure '(nil error quit))
        (with-temp-buffer
          (let (dispatch control receive result cancel events diagnostics
                         (mevedel--warn-once-table (make-hash-table :test #'equal))
                         (effects 0) (stops 0) (aborts 0))
            (cl-labels ((leave ()
                          (if (eq exit-kind 'quit)
                              (signal 'quit '(original-exit))
                            (throw 'original-exit 'preserved)))
                        (cleanup ()
                          (pcase cleanup-failure
                            ('error (error "Cleanup failed"))
                            ('quit (signal 'quit '(cleanup-exit))))))
              ;; Exercise the public workload entry point and its retained
              ;; transport callbacks, including a result after dispatch returns.
              (cl-letf (((symbol-function 'mevedel-mcp-start)
                         (lambda (_tools call hook &rest _)
                           (setq dispatch call control hook) 'server))
                        ((symbol-function 'mevedel-mcp-configuration) (lambda (_) nil))
                        ((symbol-function 'mevedel-mcp-hook-command) (lambda (_) nil))
                        ((symbol-function 'mevedel-mcp-stop)
                         (lambda (_) (cl-incf stops) (cleanup)))
                        ((symbol-function 'mevedel-acp-text-request)
                         (lambda (_launch _prompt callback _stream)
                           (setq receive callback)
                           (lambda ()
                             (cl-incf aborts)
                             (funcall receive 'abort nil)
                             (cleanup)))))
                (unwind-protect
                    (progn
                      (mevedel-test--with-captured-diagnostics diagnostics
                        (let ((observed
                               (condition-case err
                                   (catch 'original-exit
                                     (setq cancel
                                           (mevedel-acp-workload-request
                                            (lambda (_mcp _hook)
                                              (when (eq stage 'launch) (leave))
                                              (list :tool-id-field :id :control
                                                    (lambda (_boundary _event) (leave))))
                                            "probe"
                                            (list (gptel--make-tool-internal
                                                   :name "Probe" :async (not (eq stage 'sync-result))
                                                   :function (if (eq stage 'sync-result)
                                                                 (lambda () (cl-incf effects) "done")
                                                               (lambda (callback)
                                                                 (cl-incf effects)
                                                                 (setq result callback)
                                                                 (when (eq stage 'tool) (leave))))))
                                            (lambda (response info)
                                              (if (memq response '(abort nil t))
                                                  (progn (push (list response info) events) (cleanup))
                                                (leave)))
                                            (lambda (_tool _args)
                                              (when (eq stage 'before-tool) (leave))) nil))
                                     (pcase stage
                                       ('control (funcall control nil #'ignore))
                                       ('stream (funcall receive "chunk" nil))
                                       (_ (funcall dispatch "Probe" nil '(:id "one")
                                                   (lambda (_) (leave)))
                                          (when (eq stage 'async-result)
                                            (funcall result "done"))))
                                     'returned)
                                 (quit err))))
                          (should (equal observed
                                         (if (eq exit-kind 'quit)
                                             '(quit original-exit) 'preserved))))
                        (should (= stops 1))
                        (should (= aborts (if (eq stage 'launch) 0 1)))
                        (should (= effects (if (memq stage '(tool sync-result async-result)) 1 0)))
                        (should (equal (mapcar #'car events) '(abort)))
                        (when result (funcall result "late"))
                        (when receive (funcall receive t nil))
                        (when cancel (funcall cancel))
                        (should (= stops 1))
                        (should (= aborts (if (eq stage 'launch) 0 1)))
                        (should (= (length events) 1))
                        (should-not (memq cancel kill-buffer-hook)))
                      (should (equal diagnostics
                                     (if (eq cleanup-failure 'error)
                                         (concat "ACP workload MCP cleanup failed: Cleanup failed\n"
                                                 (unless (eq stage 'launch)
                                                   "ACP workload ACP cleanup failed: Cleanup failed\n"))
                                       ""))))
                  (when cancel (condition-case nil (funcall cancel) ((error quit) nil)))))))))))

  :doc "teardown errors warn once per owner without changing terminal outcomes"
  (dolist (outcome '(t nil abort))
    (dolist (cleanup-failure '(nil error quit))
      (with-temp-buffer
        (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
              (stops 0) (aborts 0) receive events diagnostics)
          (cl-labels ((cleanup ()
                        (pcase cleanup-failure
                          ('error (error "Cleanup failed"))
                          ('quit (signal 'quit '(cleanup-exit))))))
            (cl-letf (((symbol-function 'mevedel-mcp-start) (lambda (&rest _) 'server))
                      ((symbol-function 'mevedel-mcp-configuration) (lambda (_) nil))
                      ((symbol-function 'mevedel-mcp-hook-command) (lambda (_) nil))
                      ((symbol-function 'mevedel-mcp-stop)
                       (lambda (_) (cl-incf stops) (cleanup)))
                      ((symbol-function 'mevedel-acp-text-request)
                       (lambda (_launch _prompt callback _stream)
                         (setq receive callback)
                         (lambda () (cl-incf aborts) (cleanup)))))
              (mevedel-test--with-captured-diagnostics diagnostics
                (dotimes (_ 2)
                  (let ((cancel (mevedel-acp-workload-request
                                 (lambda (_mcp _hook) nil) "probe" nil
                                 (lambda (response info) (push (list response info) events))
                                 nil nil)))
                    (if (eq outcome 'abort) (funcall cancel)
                      (funcall receive outcome (and (null outcome) '(:error "Original failure"))))
                    (funcall receive t nil)
                    (funcall cancel))))
              (should (equal (mapcar #'car events) (list outcome outcome)))
              (when (null outcome)
                (should (equal (mapcar (lambda (event) (plist-get (cadr event) :error)) events)
                               '("Original failure" "Original failure"))))
              (should (= stops 2))
              (should (= aborts 2))
              (should (= (hash-table-count mevedel--warn-once-table)
                         (if (eq cleanup-failure 'error) 2 0)))
              (should (equal diagnostics
                             (if (eq cleanup-failure 'error)
                                 (concat "ACP workload MCP cleanup failed: Cleanup failed\n"
                                         "ACP workload ACP cleanup failed: Cleanup failed\n"
                                         "mevedel: ACP workload MCP cleanup failed: Cleanup failed\n"
                                         "mevedel: ACP workload ACP cleanup failed: Cleanup failed\n")
                               ""))))))))))

(provide 'test-mevedel-acp-workload)
;;; test-mevedel-acp-workload.el ends here
