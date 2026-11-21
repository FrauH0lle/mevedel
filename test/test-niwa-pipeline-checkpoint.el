;;; test-niwa-pipeline-checkpoint.el --- Final ToolCall durability -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the provider pipeline and the committed recovery it leaves behind.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-session-test-support"))
(require 'niwa-pipeline)
(require 'niwa-ptc-checkpoint)
(require 'niwa-tools)

(niwa-deftest niwa-pipeline-run-tool/checkpoint (:quiet t)
  (let* ((root (make-temp-file "niwa-final-checkpoint-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "checkpoint" workspace))
         (buffer (generate-new-buffer " *checkpoint-pipeline*"))
         (text (make-string (* 128 1024) ?x))
         (tool (niwa-tool--create
                :name "CheckpointProbe" :category "niwa"
                :handler (lambda (_args) (list :result text))
                :description "Test result" :prompt "Test result"
                :read-only-p t :max-result-size 30000))
         (gtool (gptel-make-tool :name "CheckpointProbe" :description "Test result"
                                 :function #'ignore :args nil :async t :category "niwa"))
         delivered)
    (unwind-protect
        (with-current-buffer buffer
          (niwa-chat-prepare-transcript-buffer)
          (setq-local niwa--session session niwa--workspace workspace
                      default-directory (file-name-as-directory root))
          (setf (niwa-session-permission-mode session) 'full-auto
                (niwa-tool-gptel-tool tool) gtool)
          (niwa-session-set-root-buffer session buffer)
          (niwa-session-artifacts-ensure-files session buffer)
          (niwa-tool-register tool)
          (niwa-tool-ptc--register)
          (setq-local gptel-tools (list gtool))
          (niwa-pipeline-run-tool
           (niwa-tool-ensure "ToolCall")
           (lambda (output)
             ;; Capture the committed snapshot before any later save.
             (setq delivered
                   (list output
                         (niwa-session-publication-read
                          (niwa-session-save-path session))
                         (niwa-session-artifacts-read-artifact
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
            (should (equal text (niwa-session-artifacts-read-artifact
                                session (caar results) t)))
            (should (string-prefix-p (plist-get checkpoint :result) output))
            (with-temp-buffer
              (niwa-ptc-checkpoint--insert checkpoint)
              (should (< (buffer-size) 10000))
              (should (search-backward "<persisted-output>" nil t)))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-pipeline--step-ptc-checkpoint ()
  ,test
  (test)
  :doc "passes through calls with no durable checkpoint"
  (let ((context '(:result "ready")) delivered)
    (niwa-pipeline--step-ptc-checkpoint
     context (lambda (value) (setq delivered value)) #'ert-fail)
    (should (eq context delivered)))
  :doc "retries a failed commit with an honest error and fails once"
  (let ((context '(:ptc-checkpoint-id "id" :result "ready"
                  :render-data (:kind ptc :outcome success)))
        updates failures)
    (cl-letf (((symbol-function 'niwa-ptc-checkpoint-update)
               (lambda (_session _buffer _id update)
                 (push update updates) (> (length updates) 1))))
      (niwa-pipeline--step-ptc-checkpoint
       context (lambda (&rest _) (ert-fail "Delivered uncommitted success"))
       (lambda (reason updated) (push (list reason updated) failures))))
    (should (= (length updates) 2))
    (should (= (length failures) 1))
    (should (string-prefix-p "Error:" (plist-get (car updates) :result)))
    (should (eq 'script-error (plist-get (plist-get (car updates) :render-data) :outcome))))
  :doc "a downstream error cannot rewrite an already committed checkpoint"
  (let ((writes 0))
    (cl-letf (((symbol-function 'niwa-ptc-checkpoint-update)
               (lambda (&rest _) (cl-incf writes) t)))
      (should-error
       (niwa-pipeline--step-ptc-checkpoint
        '(:ptc-checkpoint-id "id" :result "ready")
        (lambda (&rest _) (error "Downstream failure"))
        (lambda (&rest _) (ert-fail "Reclassified committed checkpoint"))))
      (should (= writes 1)))))

(provide 'test-niwa-pipeline-checkpoint)
;;; test-niwa-pipeline-checkpoint.el ends here
