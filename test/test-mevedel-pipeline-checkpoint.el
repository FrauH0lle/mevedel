;;; test-mevedel-pipeline-checkpoint.el --- Final ToolCall durability -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the provider pipeline and the committed recovery it leaves behind.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))
(require 'mevedel-tools)
(require 'mevedel-ptc-checkpoint)

(mevedel-deftest mevedel-pipeline-run-tool/checkpoint (:quiet t)
  (let* ((root (make-temp-file "mevedel-final-checkpoint-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "checkpoint" workspace))
         (buffer (generate-new-buffer " *checkpoint-pipeline*"))
         (text (make-string (* 128 1024) ?x))
         (tool (mevedel-tool--create
                :name "CheckpointProbe" :category "mevedel"
                :handler (lambda (_args) (list :result text))
                :description "Test result" :prompt "Test result"
                :read-only-p t :max-result-size 30000))
         (gtool (gptel-make-tool :name "CheckpointProbe" :description "Test result"
                                 :function #'ignore :args nil :async t :category "mevedel"))
         delivered)
    (unwind-protect
        (with-current-buffer buffer
          (mevedel-chat-prepare-transcript-buffer)
          (setq-local mevedel--session session mevedel--workspace workspace
                      default-directory (file-name-as-directory root))
          (setf (mevedel-session-permission-mode session) 'full-auto
                (mevedel-tool-gptel-tool tool) gtool)
          (mevedel-session-set-root-buffer session buffer)
          (mevedel-session-artifacts-ensure-files session buffer)
          (mevedel-tool-register tool)
          (mevedel-tool-ptc--register)
          (setq-local gptel-tools (list gtool))
          (mevedel-pipeline-run-tool
           (mevedel-tool-ensure "ToolCall")
           (lambda (output)
             ;; Capture the committed snapshot before any later save.
             (setq delivered
                   (list output
                         (mevedel-session-publication-read
                          (mevedel-session-save-path session))
                         (mevedel-session-artifacts-read-artifact
                          session "session.meta.el" t))))
           '(:expression "(CheckpointProbe)"))
          (with-timeout (3 (ert-fail "Tool completion was not delivered"))
            (while (not delivered) (accept-process-output nil 0.01)))
          (let* ((output (nth 0 delivered))
                 (publication (nth 1 delivered))
                 (sidecar (nth 2 delivered))
                 (checkpoint (car (plist-get (read sidecar) :ptc-checkpoints)))
                 (results (seq-filter
                           (lambda (entry) (string-prefix-p "tool-results/" (car entry)))
                           (plist-get publication :artifacts))))
            (should (< (length sidecar) 30000))
            (should (eq (plist-get checkpoint :state) 'settled))
            (should (string-prefix-p "<persisted-output>" (plist-get checkpoint :result)))
            (should (= (length results) 1))
            (should (equal text (mevedel-session-artifacts-read-artifact
                                session (caar results) t)))
            (should (string-prefix-p (plist-get checkpoint :result) output))
            (with-temp-buffer
              (mevedel-ptc-checkpoint--insert checkpoint)
              (should (< (buffer-size) 10000))
              (should (search-backward "<persisted-output>" nil t)))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (mevedel-workspace-clear-registry)
      (delete-directory root t))))

(mevedel-deftest mevedel-pipeline--step-ptc-checkpoint ()
  ,test
  (test)
  :doc "passes through calls with no durable checkpoint"
  (let ((context '(:result "ready")) delivered)
    (mevedel-pipeline--step-ptc-checkpoint
     context (lambda (value) (setq delivered value)) #'ert-fail)
    (should (eq context delivered)))
  :doc "retries a failed commit with an honest error and fails once"
  (let ((context '(:ptc-checkpoint-id "id" :result "ready"
                  :render-data (:kind ptc :outcome success)))
        updates failures)
    (cl-letf (((symbol-function 'mevedel-ptc-checkpoint-update)
               (lambda (_session _buffer _id update)
                 (push update updates) (> (length updates) 1))))
      (mevedel-pipeline--step-ptc-checkpoint
       context (lambda (&rest _) (ert-fail "Delivered uncommitted success"))
       (lambda (reason updated) (push (list reason updated) failures))))
    (should (= (length updates) 2))
    (should (= (length failures) 1))
    (should (string-prefix-p "Error:" (plist-get (car updates) :result)))
    (should (eq 'script-error (plist-get (plist-get (car updates) :render-data) :outcome))))
  :doc "a downstream error cannot rewrite an already committed checkpoint"
  (let ((writes 0))
    (cl-letf (((symbol-function 'mevedel-ptc-checkpoint-update)
               (lambda (&rest _) (cl-incf writes) t)))
      (should-error
       (mevedel-pipeline--step-ptc-checkpoint
        '(:ptc-checkpoint-id "id" :result "ready")
        (lambda (&rest _) (error "Downstream failure"))
        (lambda (&rest _) (ert-fail "Reclassified committed checkpoint"))))
      (should (= writes 1)))))

(provide 'test-mevedel-pipeline-checkpoint)
;;; test-mevedel-pipeline-checkpoint.el ends here
