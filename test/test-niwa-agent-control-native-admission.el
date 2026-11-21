;;; test-niwa-agent-control-native-admission.el --- Held child admission -*- lexical-binding: t -*-

;;; Commentary:
;; Real retained child turns keep waking mail through native preparation.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'niwa-agent-control)
(require 'niwa-agent-runtime)
(require 'niwa-claude-code)
(require 'niwa-claude-code-fork)

(niwa-deftest niwa-agent-control-send-message/native-preparation (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((niwa-claude-code-fork--sessions nil)
          (launches 0)
          (mail "Keep this child message through native preparation")
          record outcome)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (&rest _)
                        (cl-incf launches)
                        '(:meta ((responseText . "Child completed")))))))
            (niwa-agent-control-spawn
             session "reader" "Start then wait for mail."
             (lambda (value) (setq outcome value record (plist-get value :record)))
             :agent (niwa-agent--create :name "reader" :description "Read evidence"
                                          :tools nil :system-prompt "Child instructions"))
            (should (eq 'success (plist-get outcome :outcome)))
            (niwa-test--await 5 "Initial child did not settle"
              (eq 'idle (niwa-agent-record-activity record)))
            (let* ((child (niwa-agent-record-conversation-buffer record))
                   (before (with-current-buffer child (buffer-string)))
                   (publication (niwa-session-publication session)))
              (niwa-claude-code-fork-histories
               nil
               (lambda (_histories diagnosis _cleanup)
                 (should-not diagnosis)
                 (should
                  (eq 'waiting
                      (plist-get (niwa-agent-control-send-message
                                  session "/root/reader" mail)
                                 :delivery)))
                 (let ((queued (car (niwa-agent-record-mailbox record))))
                   (should (equal mail (plist-get queued :payload)))
                   (should (plist-get queued :wake))
                   (dotimes (_ 2) (niwa-agent-control--offer-agent-wakes session))
                   (should (eq queued (car (niwa-agent-record-mailbox record))))
                   (should (plist-get queued :wake)))
                 (should (= 1 launches))
                 (should (eq 'idle (niwa-agent-record-activity record)))
                 (should (equal before (with-current-buffer child (buffer-string))))
                 (let ((head (niwa-session-publication session)))
                   (should-error
                    (niwa-agent-runtime-dispatch
                     nil "reader" "Direct admission must refuse"
                     :path "/root/reader"
                     :frozen-configuration (niwa-agent-record-configuration record)
                     :retained-id (niwa-agent-record-id record)
                     :retained-buffer child
                     :retained-transcript (niwa-agent-record-conversation-location record))
                    :type 'user-error)
                   (should (equal head (niwa-session-publication session))))
                 (should (equal before (with-current-buffer child (buffer-string)))))
               session)
              (should-not (equal publication (niwa-session-publication session)))
              (niwa-test--await 5 "Released preparation did not wake queued child"
                (and (= 2 launches)
                     (eq 'idle (niwa-agent-record-activity record))))
              (should-not (niwa-agent-record-mailbox record))
              (with-current-buffer child
                (should (= 1 (niwa-engine-test--count-evidence (regexp-quote mail)))))
              (should (= 2 launches))))
        (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
          (kill-buffer (niwa-agent-record-conversation-buffer record)))))))

(provide 'test-niwa-agent-control-native-admission)
;;; test-niwa-agent-control-native-admission.el ends here
