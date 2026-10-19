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
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-tool-ui)
(require 'mevedel-agent-control)

(mevedel-deftest mevedel-view-pending-ask-redraw ()
  (dolist (location '(ask background composer))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (let* ((session (mevedel-session--create :name "cursor-jump" :root-buffer data-buf))
               (record (mevedel-agent-record--create
                        :id "pair-merge" :path "/root/pair-merge"
                        :parent-path "/root" :activity 'running))
               (mevedel-view-spinner-animate nil)
               (mevedel-view-tool-boundary-render-delay 0))
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
          (with-current-buffer data-buf
            (mevedel-tool-ask-ui-show
             #'ignore
             [(:question "Does any sender wake the root?"
                         :options ["Yes, any sender wakes" "No, only child agents"])]))
          (mevedel-view-jump-to-interaction)
          (should (get-text-property (point) 'mevedel-prompt-focus))
          (should (string-search "pair-merge" (buffer-string)))
          (pcase location
            ('background
             (goto-char (point-min))
             (search-forward "[Manage]")
             (backward-char (length "[Manage]")))
            ('composer (goto-char (+ (mevedel-view--input-start) 4))))
          (let* ((before (point))
                 (anchor (mevedel-view-zone-position-anchor before))
                 (flight (marker-position mevedel-view--in-flight-turn-start)))
            ,action
            (ert-info ((format "location=%S before=%S after=%S in-flight=%S new-flight=%S status=%S focus=%S anchor=%S text=%S"
                               location before (point) flight
                               (and (markerp mevedel-view--in-flight-turn-start)
                                    (marker-position mevedel-view--in-flight-turn-start))
                               (marker-position mevedel-view--status-marker)
                               (get-text-property (point) 'mevedel-prompt-focus) anchor
                               (buffer-substring-no-properties
                                (max (point-min) (- (point) 20))
                                (min (point-max) (+ (point) 30)))))
              (pcase location
                ('ask
                 (should (get-text-property (point) 'mevedel-prompt-focus))
                 (should (assq (selected-window) mevedel-view--interaction-focus-return)))
                ('background (should (looking-at-p "\\[Manage\\]")))
                ('composer (should (= 4 (- (point) (mevedel-view--input-start))))))
              (when anchor
                (should (= (point) (mevedel-view-zone-anchor-position anchor))))
              (should (= (window-point (selected-window)) (point)))
              (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
              (when (eq location 'ask)
                (call-interactively (key-binding (kbd "RET")))
                (call-interactively (key-binding (kbd "C-c C-c")))
                (should (= 4 (- (point) (mevedel-view--input-start)))))))))))
  (action)
  :doc "no-op user-state preservation keeps the Ask focus"
  (mevedel-view--call-preserving-user-view-state #'ignore)
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
  :doc "pipeline accepted permission redraw keeps the Ask focus"
  (mevedel-view-stream-handle-pipeline-event
   (list :type 'permission-approved :data-buffer data-buf
         :tool-use-id "ask-tool" :name "ToolCall" :args nil
         :permission-approvals '("test")))
  :doc "full rerender keeps the Ask focus"
  (mevedel-view--full-rerender)
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
