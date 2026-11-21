;;; test-niwa-view-cursor-jump.el --- Pending Ask redraw probes -*- lexical-binding: t -*-

;;; Commentary:
;; Reproduce background redraws while explicitly visiting a pending Ask card.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-tool-ui)
(require 'niwa-agent-control)
(require 'niwa-tool-exec)

(niwa-deftest niwa-view-pending-ask-redraw ()
  (dolist (location '(ask background composer))
    (niwa-view-test--with-buffers
      (save-window-excursion
        (delete-other-windows)
        (let* ((session (niwa-session--create :name "cursor-jump" :root-buffer data-buf))
               (record (niwa-agent-record--create
                        :id "pair-merge" :path "/root/pair-merge"
                        :parent-path "/root" :activity 'running))
               (niwa-view-spinner-animate nil)
               (niwa-view-tool-boundary-render-delay 0)
               (transcript (selected-window))
               composer answered)
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/pair-merge" record)))
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (insert "*** Check background wake\n"))
          (switch-to-buffer view-buf)
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (setq niwa-view--in-flight-turn-start
                (copy-marker niwa-view--status-marker))
          (niwa-view-test--insert-data data-buf "Checking the wake policy.\n" 'response)
          (setq niwa-view--pending-tool-calls
                '((("ToolCall" . "ask") "ToolCall" "Ask about wake policy"
                   (:tool-use-id "ask-tool"))))
          (niwa-view-render-live-update data-buf)
          (niwa-view--render-status)
          (niwa-view--start-spinner "Waiting for tools...")
          (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (setq composer (niwa-view-composer-display transcript))
          (should (window-live-p composer))
          (select-window composer)
          (should (= 4 (niwa-view-test--composer-point-offset)))
          (with-current-buffer data-buf
            (niwa-tool-ask-ui-show
             (lambda (answer) (setq answered answer))
             [(:question "Does any sender wake the root?"
                         :options ["Yes, any sender wakes" "No, only child agents"])]))
          (should (eq composer (selected-window)))
          (should (= 4 (niwa-view-test--composer-point-offset)))
          (execute-kbd-macro (kbd "C-c C-j"))
          (should (eq transcript (selected-window)))
          (should (get-text-property (point) 'niwa-prompt-focus))
          (should (string-search "pair-merge" (buffer-string)))
          (when (eq location 'background)
            (goto-char (point-min))
            (search-forward "[Manage]")
            (backward-char (length "[Manage]")))
          (let ((anchor (niwa-view-zone-position-anchor (point))))
            (should anchor)
            (when (eq location 'composer) (select-window composer))
            (let ((selected (selected-window))
                  (draft-point (window-point composer))
                  (draft-start (window-start composer)))
              (with-current-buffer view-buf ,action)
              (ert-info ((format "location=%S anchor=%S" location anchor))
                (should (eq selected (selected-window)))
                (with-current-buffer view-buf
                  (should (= (window-point transcript)
                             (niwa-view-zone-anchor-position anchor)))
                  (if (eq location 'background)
                      (should (looking-at-p "\\[Manage\\]"))
                    (should (get-text-property (window-point transcript)
                                               'niwa-prompt-focus))))
                (should (= draft-point (window-point composer)))
                (should (= draft-start (window-start composer)))
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
                (when (eq location 'ask)
                  (execute-kbd-macro (kbd "RET"))
                  (execute-kbd-macro (kbd "C-c C-c"))
                  (should (string-match-p "A1: Yes, any sender wakes" answered))
                  (should (eq composer (selected-window)))
                  (should (= 4 (niwa-view-test--composer-point-offset)))))))))))
  (action)
  :doc "no-op user-state preservation keeps the Ask focus"
  (niwa-view-call-preserving-user-view-state #'ignore)
  :doc "agent refresh keeps the Ask focus"
  (niwa-view-agent--refresh-owned "/root/pair-merge")
  :doc "agent status redraw keeps the Ask focus"
  (niwa-view--render-agent-status)
  :doc "status redraw keeps the Ask focus"
  (niwa-view--render-status)
  :doc "progress redraw keeps the Ask focus"
  (niwa-view--render-request-progress)
  :doc "spinner semantic tick keeps the Ask focus"
  (progn
    (setq niwa-view--spinner-last-second nil)
    (cl-letf (((symbol-function 'niwa-view--animation-target-visible-p)
               (lambda (&rest _) t)))
      (niwa-view--spinner-tick)))
  :doc "interaction redraw keeps the Ask focus"
  (niwa-view--interaction-render)
  :doc "incremental live update keeps the Ask focus"
  (niwa-view-render-live-update data-buf)
  :doc "new response text keeps the visited live zone"
  (progn
    (niwa-view-test--insert-data data-buf "More response text.\n" 'response)
    (niwa-view-render-live-update data-buf))
  :doc "response settlement keeps the visited live zone"
  (with-current-buffer data-buf
    (niwa-view-stream-render-response (point-min) (point-max)))
  :doc "stream stop keeps the visited live zone"
  (niwa-view-stream-stop)
  :doc "pipeline accepted permission redraw keeps the Ask focus"
  (niwa-view-stream-handle-pipeline-event
   (list :type 'permission-approved :data-buffer data-buf
         :tool-use-id "ask-tool" :name "ToolCall" :args nil
         :permission-approvals '("test")))
  :doc "full rerender keeps the Ask focus"
  (niwa-view--full-rerender)
  :doc "live Eval window restoration keeps the visited live zone"
  (let ((expression (format "(with-current-buffer %S (niwa-view--full-rerender))"
                            (buffer-name view-buf)))
        result)
    (with-current-buffer data-buf
      (niwa-tool-exec--eval-live
       (lambda (response) (setq result (plist-get response :result)))
       expression "injection" t))
    (should (stringp result))
    (should-not (string-prefix-p "Error:" result)))
  :doc "stale agent handle fallback keeps the Ask focus"
  (progn
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (1+ (point-min))
                         'niwa-view-agent-path "/root/pair-merge")
      (put-text-property (point-min) (1+ (point-min))
                         'niwa-view-agent-handle-p t))
    (should-not (niwa-view-agent--refresh-owned "/root/pair-merge"))
    (should (eq 'full niwa-view--pending-render-kind))
    (when (timerp niwa-view--render-timer)
      (cancel-timer niwa-view--render-timer))
    (niwa-view--flush-scheduled-render view-buf t))
  :doc "scheduled full rerender keeps the Ask focus"
  (progn
    (niwa-view-rerender view-buf)
    (when (timerp niwa-view--render-timer)
      (cancel-timer niwa-view--render-timer))
    (niwa-view--flush-scheduled-render view-buf))
  :doc "settled batched installation keeps the Ask focus"
  (let ((install (symbol-function 'niwa-view-render--install-batch))
        (installed 0)
        (steps 0))
    (setq niwa-view--in-flight-turn-start nil
          niwa-view--data-turn-start nil
          niwa-view--pending-tool-calls nil)
    (cl-letf (((symbol-function 'niwa-view-render--install-batch)
               (lambda (job) (cl-incf installed) (funcall install job)))
              ((symbol-function 'niwa-view-render--full-now)
               (lambda (&rest _) (ert-fail "Unexpected synchronous fallback"))))
      (niwa-view-rerender view-buf)
      (when (timerp niwa-view--render-timer)
        (cancel-timer niwa-view--render-timer))
      (niwa-view--flush-scheduled-render view-buf)
      (while (and niwa-view-render--batch (< (cl-incf steps) 100))
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should-not niwa-view-render--batch)
      (should (= 1 installed)))))

(provide 'test-niwa-view-cursor-jump)
;;; test-niwa-view-cursor-jump.el ends here
