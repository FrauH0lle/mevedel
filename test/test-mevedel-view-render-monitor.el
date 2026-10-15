;;; test-mevedel-view-render-monitor.el --- Monitor rendering tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise source, progress and terminal ownership at each ToolCall depth.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-tool-ptc)
(require 'mevedel-execution-transcript)

(mevedel-deftest mevedel-view--render-tool-call/monitor ()
  ,test
  (test)
  :doc "Monitor rows reconcile initial, progress and terminal facts at every nesting depth"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "Monitor" :category "mevedel"
      :renderer (lambda (_name _args result data)
                  (list :header (format "Watching: %s · %s events"
                                        (plist-get data :description)
                                        (plist-get data :event-count))
                        :body result :execution-id (plist-get data :execution-id)
                        :live-execution-p (plist-get data :live-execution-p)))))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
    (let* ((facts '(:kind monitor :execution-id "watch-1" :state running
                   :description "Build failures" :expires-at 1700000000.0
                   :event-count 1 :live-execution-p t :execution-output "initial"))
           (child (list :tool "Monitor" :id "watch-tool" :status 'success
                        :args '(:command "tail -F build.log" :description "Build failures")
                        :result "envelope" :render-data facts))
           (direct-data (list :kind 'ptc :status 'success :outcome 'completed
                              :direct-tool "Monitor" :calls (list child)))
           (call (list :name "Monitor" :tool-use-id "watch-tool"
                       :args (plist-get child :args) :result "envelope" :render-data facts))
           (direct (list :name "ToolCall" :tool-use-id "outer" :result "envelope"
                         :args '(:expression "(Monitor)") :render-data direct-data))
           (nested-direct (list :tool "ToolCall" :id "outer" :status 'success
                                :args '(:expression "(Monitor)") :result "envelope"
                                :render-data direct-data)))
      (with-current-buffer view-buf
        (setq-local mevedel-view--execution-events (make-hash-table :test #'equal))
        ;; A renderer must not disturb a draft, even when its first character is >.
        (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "> draft\nsecond line"))
        (let ((text (buffer-string)) (position (point)) (undo buffer-undo-list))
          (dolist (phase '(initial progress terminal))
            (let ((expected-output (symbol-name phase))
                  (count (pcase phase ('initial 1) ('progress 2) ('terminal 3))))
              (when (eq phase 'progress)
                (puthash "watch-tool"
                         (list :type 'progress :output-tail "progress"
                               :facts '(:kind monitor :execution-id "watch-1"
                                        :state running :description "Build failures"
                                        :expires-at 1700000000.0 :event-count 2))
                         mevedel-view--execution-events))
              (when (eq phase 'terminal)
                (remhash "watch-tool" mevedel-view--execution-events)
                (with-current-buffer data-buf
                  (setq-local mevedel-execution-transcript--pending-terminals
                              (make-hash-table :test #'equal))
                  (puthash "watch-tool"
                           '(:render-data (:kind monitor :execution-id "watch-1"
                                           :state completed :description "Build failures"
                                           :event-count 3 :status success
                                           :execution-output "terminal"))
                           mevedel-execution-transcript--pending-terminals)))
              (dolist (row (list (mevedel-view--render-tool-call call data-buf)
                                 (mevedel-view--render-tool-call direct data-buf)
                                 (mevedel-view--child-call-rendering child)
                                 (mevedel-view--child-call-rendering nested-direct)))
                (should (equal (format "Watching: Build failures · %d events" count)
                               (plist-get row :header)))
                (should (equal expected-output (plist-get row :body)))
                (should (equal "watch-1" (plist-get row :execution-id))))))
          (should (equal text (buffer-string)))
          (should (= position (point)))
          (should (equal undo buffer-undo-list)))))))

(provide 'test-mevedel-view-render-monitor)
;;; test-mevedel-view-render-monitor.el ends here
