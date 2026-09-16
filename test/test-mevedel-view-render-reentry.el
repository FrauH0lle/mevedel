;;; test-mevedel-view-render-reentry.el --- Reentrant projection tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise projection entry points while fontification delivers another event.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(mevedel-deftest mevedel-view-render-live-update/reentry ()
  ,test
  (test)

  :doc "terminal settlement during fontification leaves one response and no live tail"
  (dolist (terminal '(direct observer stop
                     (full . direct) (full . observer) (full . stop)))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data
       data-buf
       "Conclusion.\n\nThe appropriate fix:\n\n```html\n<meta charset=\"utf-8\">\n```\n\nNo files changed.\n"
       'response)
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--status-marker nil))
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (let ((original (symbol-function 'mevedel-view--fontify-response))
              entered)
          (cl-letf (((symbol-function 'mevedel-view--fontify-response)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (unless entered
                           (setq entered t)
                           (pcase (if (consp terminal) (cdr terminal) terminal)
                             ('direct
                              (mevedel-view-render-settle
                               data-buf 1 (with-current-buffer data-buf (point-max))))
                             ('observer
                              (with-current-buffer data-buf
                                (mevedel-view-stream-render-response 1 (point-max))))
                             ('stop (mevedel-view-stream-stop))))))))
            (if (consp terminal)
                (mevedel-view--full-rerender)
              (mevedel-view-render-live-update data-buf)))
          (should entered))
        (should-not mevedel-view--live-data-tail-start)
        (should (= 1 (mevedel-view-test--count-substring
                      "<meta charset=" (buffer-string))))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (= (point) (+ 4 (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view-render-mutate/ownership ()
  ,test
  (test)

  :doc "nested mutations coalesce without running inside the active writer"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let (events)
        (mevedel-view-render-mutate
         'outer
         (lambda ()
           (push 'start events)
           (mevedel-view-render-mutate 'refresh (lambda () (push 'obsolete events)))
           (mevedel-view-render-mutate 'refresh (lambda () (push 'latest events)))
           (push 'end events)))
        (should (equal (nreverse events) '(start end latest)))
        (should-not mevedel-view-render--owner)
        (should-not mevedel-view-render--pending))))

  :doc "replacement invalidates queued old work and errors release ownership"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let (events)
        (should-error
         (mevedel-view-render-mutate
          'outer
          (lambda ()
            (mevedel-view-render-mutate 'old (lambda () (push 'old events)))
            (mevedel-view-render-mutate
             'replacement (lambda () (push 'replacement events)) t)
            (error "Injected render failure"))))
        (should (equal events '(replacement)))
        (should-not mevedel-view-render--owner)
        (mevedel-view-render-mutate 'next (lambda () (push 'next events)))
        (should (equal events '(next replacement)))))))

(mevedel-deftest mevedel-view-disclosure/reentry ()
  ,test
  (test)

  :doc "full rendering during disclosure expansion preserves exactly one expanded row"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "ReentryTool" :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "ReentryTool: details" :body result
                        :body-mode 'text-mode :vtype 'tool-summary
                        :initially-collapsed-p t))))
    (mevedel-view-test--insert-data
     data-buf "(:name \"ReentryTool\" :args nil)\n\nUnique tool body\n"
     '(tool . "reentry-tool"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((original (symbol-function 'mevedel-view-render-insert-expanded-disclosure))
            entered)
        (cl-letf (((symbol-function 'mevedel-view-render-insert-expanded-disclosure)
                   (lambda (&rest args)
                     (unless entered
                       (setq entered t)
                       (mevedel-view--full-rerender))
                     (apply original args))))
          (save-excursion
            (goto-char (point-min))
            (search-forward "ReentryTool: details")
            (mevedel-view-toggle-section)))
        (should entered))
      (should (= 1 (mevedel-view-test--count-substring
                    "Unique tool body" (buffer-string))))
      (should (= 1 (mevedel-view-test--count-substring
                    "ReentryTool: details" (buffer-string))))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (should (= (point) (+ 4 (mevedel-view--input-start))))))

  :doc "full rendering during disclosure collapse preserves the requested fold"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf "<system-reminder>\nCollapsed reminder body\n</system-reminder>\n" nil)
    (mevedel-view-test--insert-data data-buf "Adjacent response\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "System reminder")
      (mevedel-view-toggle-section)
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((original (symbol-function 'mevedel-view-render-collapsed-disclosure)) entered)
        (cl-letf (((symbol-function 'mevedel-view-render-collapsed-disclosure)
                   (lambda (&rest args)
                     (unless entered
                       (setq entered t)
                       (mevedel-view--full-rerender))
                     (apply original args))))
          (save-excursion
            (goto-char (point-min))
            (search-forward "System reminder")
            (mevedel-view-toggle-section)))
        (should entered))
      (should-not (string-search "Collapsed reminder body" (buffer-string)))
      (should (= 1 (mevedel-view-test--count-substring
                    "System reminder" (buffer-string))))
      (should (= 1 (mevedel-view-test--count-substring
                    "Adjacent response" (buffer-string))))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (should (= (point) (+ 4 (mevedel-view--input-start)))))))

(mevedel-deftest mevedel-view-stream/replacement ()
  ,test
  (test)

  :doc "queued old terminal cleanup cannot clear a replacement turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Old response\n" 'response)
    (with-current-buffer view-buf
      (let ((start (with-current-buffer data-buf (copy-marker (point-max)))))
        (mevedel-view-render-mutate
         'outer
         (lambda ()
           (mevedel-view-stream-stop)
           (mevedel-view-stream-begin-turn mevedel-view--status-marker start)))
        (should (marker-position mevedel-view--data-turn-start))
        (should (= start mevedel-view--data-turn-start))
        (should-not mevedel-view-render--terminal-p)))))

(mevedel-deftest mevedel-view-agent/refresh-reentry ()
  ,test
  (test)
  :doc "multiple agent handles are rediscovered after nested full rendering"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "ReentryAgent" :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "Agent: owned worker" :body result
                        :body-mode 'text-mode :vtype 'agent-handle
                        :agent-path "/root/worker" :agent-status 'running
                        :initially-collapsed-p t))))
    (dotimes (n 2)
      (mevedel-view-test--insert-data
       data-buf "(:name \"ReentryAgent\" :args nil)\n\nAgent detail\n"
       (cons 'tool (format "reentry-agent-%d" n)))
      (mevedel-view-test--insert-data data-buf "Between calls\n" 'response))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((original (symbol-function 'mevedel-view--segment-rendering))
            entered)
        (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (unless entered
                         (setq entered t)
                         (with-current-buffer data-buf
                           (goto-char (point-min))
                           (insert "*** Added authoritative prompt\n"))
                         (mevedel-view--full-rerender))))))
          (mevedel-view--refresh-agent-rendering-now "/root/worker"))
        (should entered))
      (should (= 2 (length (car (mevedel-view--agent-handle-refresh-points
                                "/root/worker")))))
      (should (= 2 (mevedel-view-test--count-substring
                    "Agent: owned worker" (buffer-string))))
      (should (= 1 (mevedel-view-test--count-substring
                    "Added authoritative prompt" (buffer-string))))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (should (= (point) (+ 4 (mevedel-view--input-start)))))))

(mevedel-deftest mevedel-view-render-mutate/terminal-cleanup ()
  ,test
  (test)
  :doc "a failing queued disclosure cannot discard later mandatory terminal cleanup"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Final response\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view-stream-begin-turn
       mevedel-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (should-error
       (mevedel-view-render-mutate
        'outer
        (lambda ()
          (mevedel-view-render-mutate 'bad-disclosure
                                      (lambda () (error "Queued failure")))
          (mevedel-view-stream-stop))))
      (should-not mevedel-view--data-turn-start)
      (should-not mevedel-view--in-flight-turn-start)
      (should-not mevedel-view--spinner-timer)
      (should-not mevedel-view-render--owner)))

  :doc "source replacement cannot discard a queued terminal's required release"
  (dolist (terminal '(stop observer))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "Final response\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view-stream-begin-turn
         mevedel-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (mevedel-view--schedule-render 'incremental data-buf 60)
        (should (timerp mevedel-view--render-timer))
        (mevedel-view-render-mutate
         'outer
         (lambda ()
           (if (eq terminal 'stop)
               (mevedel-view-stream-stop)
             (with-current-buffer data-buf
               (mevedel-view-stream-render-response 1 (point-max))))
           (mevedel-view--full-rerender data-buf t)))
        (should-not mevedel-view--data-turn-start)
        (should-not mevedel-view--in-flight-turn-start)
        (should-not mevedel-view--spinner-timer)
        (should-not mevedel-view--pending-render-kind)
        (should-not mevedel-view--render-timer)
        (should (= 1 (mevedel-view-test--count-substring
                      "Final response" (buffer-string))))))))

(mevedel-deftest mevedel-view-agent-live-transcript/queued-error ()
  ,test
  (test)
  :doc "deferred observer failures warn locally without failing the outer writer"
  (dolist (operation '(mevedel-view-agent-live-transcript-start
                       mevedel-view-agent-live-transcript-finalize))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "Last good agent projection\n" 'response)
      (let ((invocation (mevedel-agent-invocation--create
                         :buffer data-buf :transcript-status 'completed))
            warnings completed before)
        (with-current-buffer view-buf
          (setq mevedel-view--agent-transcript-p t)
          (mevedel-view--full-rerender)
          (setq before (buffer-string))
          (cl-letf (((symbol-function 'mevedel-view--fontify-response)
                     (lambda (&rest _) (error "Injected observer failure")))
                    ((symbol-function 'mevedel--warn-once)
                     (lambda (&rest args) (push args warnings))))
            (mevedel-view-render-mutate
             'outer
             (lambda ()
               (funcall operation invocation)
               (mevedel-view-render-mutate
                'after-observer (lambda () (setq completed t))))))
          (should completed)
          (should (equal before (buffer-string)))
          (should (= 1 (length warnings)))
          (should (string-match-p "Injected observer failure"
                                  (car (last (car warnings)))))
          (should-not mevedel-view-render--owner)
          (should-not mevedel-view-render--pending)
          (should-not mevedel-view--spinner-timer)
          (mevedel-view-render-mutate 'retry (lambda () (setq completed 'retry)))
          (should (eq completed 'retry)))))))

(mevedel-deftest mevedel-view-render-settle/reentry ()
  ,test
  (test)
  :doc "live and full renders requested during settlement cannot revive its tail"
  (dolist (nested '(live full))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "Only settled response\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view-stream-begin-turn
         mevedel-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (let ((original (symbol-function 'mevedel-view--fontify-response)) entered)
          (cl-letf (((symbol-function 'mevedel-view--fontify-response)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (unless entered
                           (setq entered t)
                           (if (eq nested 'live)
                               (mevedel-view-render-live-update data-buf)
                             (mevedel-view--full-rerender)))))))
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response 1 (point-max))))
          (should entered))
        (should (= 1 (mevedel-view-test--count-substring
                      "Only settled response" (buffer-string))))
        (should-not mevedel-view--live-data-tail-start)
        (should-not mevedel-view--data-turn-start)
        (should-not mevedel-view--in-flight-turn-start)
        (should-not mevedel-view--spinner-timer)
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (= (point) (+ 4 (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view-agent-live-transcript-finalize/reentry ()
  ,test
  (test)
  :doc "agent settlement during live rendering retains the source and reader"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Agent final answer\n" 'response)
    (let ((invocation (mevedel-agent-invocation--create
                       :buffer data-buf :transcript-status 'completed :call-count 2)))
      (with-current-buffer view-buf
        (setq mevedel-view--agent-transcript-p t)
        (mevedel-view-stream-begin-turn
         mevedel-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Agent final")
        (let ((reader (buffer-substring-no-properties
                       (line-beginning-position) (point)))
              (reader-point (point))
              (original (symbol-function 'mevedel-view--fontify-response)) entered)
          (cl-letf (((symbol-function 'mevedel-view--fontify-response)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (unless entered
                           (setq entered t)
                           (mevedel-view-agent-live-transcript-finalize invocation))))))
            (mevedel-view-render-live-update data-buf))
          (should entered)
          (should (= reader-point (point)))
          (should (equal reader (buffer-substring-no-properties
                                 (line-beginning-position) (point)))))
        (should (eq mevedel--data-buffer data-buf))
        (should (eq 'completed (plist-get mevedel-view--agent-transcript-info :status)))
        (should-not (plist-get mevedel-view--agent-transcript-info :live-buffer))
        (should (= 1 (mevedel-view-test--count-substring
                      "Agent final answer" (buffer-string))))
        (should-not mevedel-view--data-turn-start)
        (should-not mevedel-view--in-flight-turn-start)
        (should-not mevedel-view--live-data-tail-start)
        (should-not mevedel-view-render--owner)))))

(mevedel-deftest mevedel-view-disclosure/desired-state ()
  ,test
  (test)
  :doc "queued expansion and collapse apply desired state, not two old toggles"
  (dolist (collapse '(nil t))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data
       data-buf "<system-reminder>\nUnique reminder body\n</system-reminder>\n" nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (when collapse
          (goto-char (point-min))
          (search-forward "System reminder")
          (mevedel-view-toggle-section))
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (mevedel-view-render-mutate
         'outer
         (lambda ()
           (dotimes (_ 2)
             (save-excursion
               (goto-char (point-min))
               (search-forward "System reminder")
               (mevedel-view-toggle-section)))
           (mevedel-view--full-rerender)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "System reminder")
          (should (eq collapse (and (get-text-property
                                    (1- (point)) 'mevedel-view-collapsed) t))))
        (should (= (if collapse 0 1)
                   (mevedel-view-test--count-substring
                    "Unique reminder body" (buffer-string))))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (= (point) (+ 4 (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view-agent/adjacent-reentry ()
  ,test
  (test)
  :doc "both refresh nesting orders preserve two handles and an expanded reminder"
  (dolist (order '(full-inside-refresh refresh-inside-full))
    (dolist (reader '(nil t))
      (mevedel-view-test--with-buffers
        (mevedel-tool-register
         (mevedel-tool--create
          :name "AdjacentAgent" :category "mevedel"
          :renderer (lambda (_name _args result _data)
                      (list :header "Agent: adjacent worker" :body result
                            :body-mode 'text-mode :vtype 'agent-handle
                            :agent-path "/root/worker" :agent-status 'running
                            :initially-collapsed-p t))))
        (dotimes (n 2)
          (mevedel-view-test--insert-data
           data-buf "(:name \"AdjacentAgent\" :args nil)\n\nAgent detail\n"
           (cons 'tool (format "adjacent-agent-%d" n)))
          (mevedel-view-test--insert-data data-buf "Between calls\n" 'response))
        (mevedel-view-test--insert-data
         data-buf "<system-reminder>\nAdjacent expanded reminder\n</system-reminder>\n" nil)
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (goto-char (point-min))
          (search-forward "System reminder")
          (mevedel-view-toggle-section)
          (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (when reader
            (goto-char (point-min))
            (search-forward "Between"))
          (let ((reader-point (point))
                (original (symbol-function 'mevedel-view--segment-rendering)) entered)
            (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                       (lambda (&rest args)
                         (prog1 (apply original args)
                           (unless entered
                             (setq entered t)
                             (if (eq order 'full-inside-refresh)
                                 (mevedel-view--full-rerender)
                               (mevedel-view--refresh-agent-rendering-now "/root/worker")))))))
              (if (eq order 'full-inside-refresh)
                  (mevedel-view--refresh-agent-rendering-now "/root/worker")
                (mevedel-view--full-rerender)))
            (should entered)
            (should (= (point) (if reader reader-point
                                (+ 4 (mevedel-view--input-start))))))
          (should (= 2 (length (car (mevedel-view--agent-handle-refresh-points
                                    "/root/worker")))))
          (should (= 2 (mevedel-view-test--count-substring
                        "Agent: adjacent worker" (buffer-string))))
          (should (= 1 (mevedel-view-test--count-substring
                        "Adjacent expanded reminder" (buffer-string))))
          (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
          (let ((pos (point-min)))
            (while (< pos mevedel-view--status-marker)
              (when-let* ((source (get-text-property pos 'mevedel-view-source)))
                (should (eq data-buf (marker-buffer (car source))))
                (should (<= 1 (car source) (cdr source)
                            (with-current-buffer data-buf (point-max)))))
              (setq pos (1+ pos)))))))))

(mevedel-deftest mevedel-view-disclosure/source-replacement ()
  ,test
  (test)
  :doc "a queued disclosure cannot expand a replacement source at the same offset"
  (with-temp-buffer
    (org-mode)
    (insert "<system-reminder>\nNew source body\n</system-reminder>\n")
    (let ((replacement (current-buffer)))
      (mevedel-view-test--with-buffers
        (mevedel-view-test--insert-data
         data-buf "<system-reminder>\nOld source body\n</system-reminder>\n" nil)
        (with-current-buffer replacement
          (setq-local mevedel--view-buffer view-buf))
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (mevedel-view-render-mutate
           'outer
           (lambda ()
             (save-excursion
               (goto-char (point-min))
               (search-forward "System reminder")
               (mevedel-view-toggle-section))
             (setq mevedel--data-buffer replacement)
             (mevedel-view--full-rerender replacement t)))
          (should-not (string-search "Old source body" (buffer-string)))
          (should-not (string-search "New source body" (buffer-string)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "System reminder")
            (should (get-text-property (1- (point)) 'mevedel-view-collapsed))
            (should (eq replacement
                        (marker-buffer (car (get-text-property
                                             (1- (point)) 'mevedel-view-source)))))
            (mevedel-view-toggle-section)
            (should (string-search "New source body" (buffer-string))))
          (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
          (should (= (point) (+ 4 (mevedel-view--input-start)))))))))

(provide 'test-mevedel-view-render-reentry)
;;; test-mevedel-view-render-reentry.el ends here
