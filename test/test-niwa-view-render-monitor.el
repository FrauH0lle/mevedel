;;; test-niwa-view-render-monitor.el --- Monitor rendering tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise source, progress and terminal ownership at each ToolCall depth.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-tool-ptc)
(require 'niwa-execution-transcript)

(niwa-deftest niwa-view--render-tool-call/monitor ()
  ,test
  (test)
  :doc "Monitor rows reconcile initial, progress and terminal facts at every nesting depth"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "Monitor" :category "niwa"
      :renderer (lambda (_name _args result data)
                  (list :header (format "Watching: %s · %s events"
                                        (plist-get data :description)
                                        (plist-get data :event-count))
                        :body result :execution-id (plist-get data :execution-id)
                        :live-execution-p (plist-get data :live-execution-p)))))
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
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
        (setq-local niwa-view--execution-events (make-hash-table :test #'equal))
        ;; A renderer must not disturb a draft, even when its first character is >.
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (let* ((buffers (list view-buf (niwa-view-composer-buffer)))
               (snapshot (lambda ()
                           (mapcar (lambda (buffer)
                                     (with-current-buffer buffer
                                       (list (buffer-string) (point) buffer-undo-list)))
                                   buffers)))
               (before (funcall snapshot)))
          (dolist (phase '(initial progress terminal))
            (let ((expected-output (symbol-name phase))
                  (count (pcase phase ('initial 1) ('progress 2) ('terminal 3))))
              (when (eq phase 'progress)
                (puthash "watch-tool"
                         (list :type 'progress :output-tail "progress"
                               :facts '(:kind monitor :execution-id "watch-1"
                                        :state running :description "Build failures"
                                        :expires-at 1700000000.0 :event-count 2))
                         niwa-view--execution-events))
              (when (eq phase 'terminal)
                (remhash "watch-tool" niwa-view--execution-events)
                (with-current-buffer data-buf
                  (setq-local niwa-execution-transcript--pending-terminals
                              (make-hash-table :test #'equal))
                  (puthash "watch-tool"
                           '(:render-data (:kind monitor :execution-id "watch-1"
                                           :state completed :description "Build failures"
                                           :event-count 3 :status success
                                           :execution-output "terminal"))
                           niwa-execution-transcript--pending-terminals)))
              (dolist (row (list (niwa-view--render-tool-call call data-buf)
                                 (niwa-view--render-tool-call direct data-buf)
                                 (niwa-view--child-call-rendering child)
                                 (niwa-view--child-call-rendering nested-direct)))
                (should (equal (format "Watching: Build failures · %d events" count)
                               (plist-get row :header)))
                (should (equal expected-output (plist-get row :body)))
                (should (equal "watch-1" (plist-get row :execution-id))))))
          ;; Text, caret and undo history of both the view and the composer.
          (should (equal before (funcall snapshot))))))))

(provide 'test-niwa-view-render-monitor)
;;; test-niwa-view-render-monitor.el ends here
