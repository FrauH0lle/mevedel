;;; test-mevedel-view-cursor-jump.el --- Pending Ask redraw probes -*- lexical-binding: t -*-

;;; Commentary:
;; Reproduce background redraws while explicitly visiting a pending Ask card.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-tool-ui)
(require 'mevedel-agent-control)
(require 'mevedel-tool-exec)

(mevedel-deftest mevedel-view-pending-ask-redraw ()
  (dolist (location '(ask background composer))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (delete-other-windows)
        (let* ((session (mevedel-session--create :name "cursor-jump" :root-buffer data-buf))
               (record (mevedel-agent-record--create
                        :id "pair-merge" :path "/root/pair-merge"
                        :parent-path "/root" :activity 'running))
               (mevedel-view-spinner-animate nil)
               (mevedel-view-tool-boundary-render-delay 0)
               (transcript (selected-window))
               composer answered)
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/pair-merge" record)))
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (insert "*** Check background wake\n"))
          (switch-to-buffer view-buf)
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--status-marker))
          (mevedel-view-test--insert-data data-buf "Checking the wake policy.\n" 'response)
          (setq mevedel-view--pending-tool-calls
                '((("ToolCall" . "ask") "ToolCall" "Ask about wake policy"
                   (:tool-use-id "ask-tool"))))
          (mevedel-view-render-live-update data-buf)
          (mevedel-view--render-status)
          (mevedel-view--start-spinner "Waiting for tools...")
          (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (setq composer (mevedel-view-composer-display transcript))
          (should (window-live-p composer))
          (select-window composer)
          (should (= 4 (mevedel-view-test--composer-point-offset)))
          (with-current-buffer data-buf
            (mevedel-tool-ask-ui-show
             (lambda (answer) (setq answered answer))
             [(:question "Does any sender wake the root?"
                         :options ["Yes, any sender wakes" "No, only child agents"])]))
          (should (eq composer (selected-window)))
          (should (= 4 (mevedel-view-test--composer-point-offset)))
          (execute-kbd-macro (kbd "C-c C-j"))
          (should (eq transcript (selected-window)))
          (should (get-text-property (point) 'mevedel-prompt-focus))
          (should (string-search "pair-merge" (buffer-string)))
          (when (eq location 'background)
            (goto-char (point-min))
            (search-forward "[Manage]")
            (backward-char (length "[Manage]")))
          (let ((anchor (mevedel-view-zone-position-anchor (point))))
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
                             (mevedel-view-zone-anchor-position anchor)))
                  (if (eq location 'background)
                      (should (looking-at-p "\\[Manage\\]"))
                    (should (get-text-property (window-point transcript)
                                               'mevedel-prompt-focus))))
                (should (= draft-point (window-point composer)))
                (should (= draft-start (window-start composer)))
                (should (equal "> draft\nsecond line" (mevedel-view-composer-draft-text)))
                (when (eq location 'ask)
                  (execute-kbd-macro (kbd "RET"))
                  (execute-kbd-macro (kbd "C-c C-c"))
                  (should (string-match-p "A1: Yes, any sender wakes" answered))
                  (should (eq composer (selected-window)))
                  (should (= 4 (mevedel-view-test--composer-point-offset)))))))))))
  (action)
  :doc "no-op user-state preservation keeps the Ask focus"
  (mevedel-view-call-preserving-user-view-state #'ignore)
  :doc "agent refresh keeps the Ask focus"
  (mevedel-view-agent--refresh-owned "/root/pair-merge")
  :doc "agent status redraw keeps the Ask focus"
  (mevedel-view--render-agent-status)
  :doc "status redraw keeps the Ask focus"
  (mevedel-view--render-status)
  :doc "progress redraw keeps the Ask focus"
  (mevedel-view--render-request-progress)
  :doc "spinner semantic tick keeps the Ask focus"
  (progn
    (setq mevedel-view--spinner-last-second nil)
    (cl-letf (((symbol-function 'mevedel-view--animation-target-visible-p)
               (lambda (&rest _) t)))
      (mevedel-view--spinner-tick)))
  :doc "interaction redraw keeps the Ask focus"
  (mevedel-view--interaction-render)
  :doc "incremental live update keeps the Ask focus"
  (mevedel-view-render-live-update data-buf)
  :doc "new response text keeps the visited live zone"
  (progn
    (mevedel-view-test--insert-data data-buf "More response text.\n" 'response)
    (mevedel-view-render-live-update data-buf))
  :doc "response settlement keeps the visited live zone"
  (with-current-buffer data-buf
    (mevedel-view-stream-render-response (point-min) (point-max)))
  :doc "stream stop keeps the visited live zone"
  (mevedel-view-stream-stop)
  :doc "pipeline accepted permission redraw keeps the Ask focus"
  (mevedel-view-stream-handle-pipeline-event
   (list :type 'permission-approved :data-buffer data-buf
         :tool-use-id "ask-tool" :name "ToolCall" :args nil
         :permission-approvals '("test")))
  :doc "full rerender keeps the Ask focus"
  (mevedel-view--full-rerender)
  :doc "live Eval window restoration keeps the visited live zone"
  (let ((expression (format "(with-current-buffer %S (mevedel-view--full-rerender))"
                            (buffer-name view-buf)))
        result)
    (with-current-buffer data-buf
      (mevedel-tool-exec--eval-live
       (lambda (response) (setq result (plist-get response :result)))
       expression "injection" t))
    (should (stringp result))
    (should-not (string-prefix-p "Error:" result)))
  :doc "stale agent handle fallback keeps the Ask focus"
  (progn
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (1+ (point-min))
                         'mevedel-view-agent-path "/root/pair-merge")
      (put-text-property (point-min) (1+ (point-min))
                         'mevedel-view-agent-handle-p t))
    (should-not (mevedel-view-agent--refresh-owned "/root/pair-merge"))
    (should (eq 'full mevedel-view--pending-render-kind))
    (when (timerp mevedel-view--render-timer)
      (cancel-timer mevedel-view--render-timer))
    (mevedel-view--flush-scheduled-render view-buf t))
  :doc "scheduled full rerender keeps the Ask focus"
  (progn
    (mevedel-view-rerender view-buf)
    (when (timerp mevedel-view--render-timer)
      (cancel-timer mevedel-view--render-timer))
    (mevedel-view--flush-scheduled-render view-buf))
  :doc "settled batched installation keeps the Ask focus"
  (let ((install (symbol-function 'mevedel-view-render--install-batch))
        (installed 0)
        (steps 0))
    (setq mevedel-view--in-flight-turn-start nil
          mevedel-view--data-turn-start nil
          mevedel-view--pending-tool-calls nil)
    (cl-letf (((symbol-function 'mevedel-view-render--install-batch)
               (lambda (job) (cl-incf installed) (funcall install job)))
              ((symbol-function 'mevedel-view-render--full-now)
               (lambda (&rest _) (ert-fail "Unexpected synchronous fallback"))))
      (mevedel-view-rerender view-buf)
      (when (timerp mevedel-view--render-timer)
        (cancel-timer mevedel-view--render-timer))
      (mevedel-view--flush-scheduled-render view-buf)
      (while (and mevedel-view-render--batch (< (cl-incf steps) 100))
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should-not mevedel-view-render--batch)
      (should (= 1 installed)))))

(provide 'test-mevedel-view-cursor-jump)
;;; test-mevedel-view-cursor-jump.el ends here
