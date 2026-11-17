;;; test-mevedel-agent-control-native-admission.el --- Held child admission -*- lexical-binding: t -*-

;;; Commentary:
;; Real retained child turns keep waking mail through native preparation.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-agent-control)
(require 'mevedel-agent-runtime)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-fork)

(mevedel-deftest mevedel-agent-control-send-message/native-preparation (:quiet t)
  (mevedel-engine-test--with-claude-session
    (let ((mevedel-claude-code-fork--sessions nil)
          (launches 0)
          (mail "Keep this child message through native preparation")
          record outcome)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (&rest _)
                        (cl-incf launches)
                        '(:meta ((responseText . "Child completed")))))))
            (mevedel-agent-control-spawn
             session "reader" "Start then wait for mail."
             (lambda (value) (setq outcome value record (plist-get value :record)))
             :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                          :tools nil :system-prompt "Child instructions"))
            (should (eq 'success (plist-get outcome :outcome)))
            (mevedel-test--await 5 "Initial child did not settle"
              (eq 'idle (mevedel-agent-record-activity record)))
            (let* ((child (mevedel-agent-record-conversation-buffer record))
                   (before (with-current-buffer child (buffer-string)))
                   (publication (mevedel-session-publication session)))
              (mevedel-claude-code-fork-histories
               nil
               (lambda (_histories diagnosis _cleanup)
                 (should-not diagnosis)
                 (should
                  (eq 'waiting
                      (plist-get (mevedel-agent-control-send-message
                                  session "/root/reader" mail)
                                 :delivery)))
                 (let ((queued (car (mevedel-agent-record-mailbox record))))
                   (should (equal mail (plist-get queued :payload)))
                   (should (plist-get queued :wake))
                   (dotimes (_ 2) (mevedel-agent-control--offer-agent-wakes session))
                   (should (eq queued (car (mevedel-agent-record-mailbox record))))
                   (should (plist-get queued :wake)))
                 (should (= 1 launches))
                 (should (eq 'idle (mevedel-agent-record-activity record)))
                 (should (equal before (with-current-buffer child (buffer-string))))
                 (let ((head (mevedel-session-publication session)))
                   (should-error
                    (mevedel-agent-runtime-dispatch
                     nil "reader" "Direct admission must refuse"
                     :path "/root/reader"
                     :frozen-configuration (mevedel-agent-record-configuration record)
                     :retained-id (mevedel-agent-record-id record)
                     :retained-buffer child
                     :retained-transcript (mevedel-agent-record-conversation-location record))
                    :type 'user-error)
                   (should (equal head (mevedel-session-publication session))))
                 (should (equal before (with-current-buffer child (buffer-string)))))
               session)
              (should-not (equal publication (mevedel-session-publication session)))
              (mevedel-test--await 5 "Released preparation did not wake queued child"
                (and (= 2 launches)
                     (eq 'idle (mevedel-agent-record-activity record))))
              (should-not (mevedel-agent-record-mailbox record))
              (with-current-buffer child
                (should (= 1 (mevedel-engine-test--count-evidence (regexp-quote mail)))))
              (should (= 2 launches))))
        (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
          (kill-buffer (mevedel-agent-record-conversation-buffer record)))))))

(provide 'test-mevedel-agent-control-native-admission)
;;; test-mevedel-agent-control-native-admission.el ends here
