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
                                  "\\`mevedel-acp-workload-request/lost-mcp-reply/"))

(provide 'test-mevedel-acp-workload)
;;; test-mevedel-acp-workload.el ends here
