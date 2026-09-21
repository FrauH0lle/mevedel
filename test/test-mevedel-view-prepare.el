;;; test-mevedel-view-prepare.el --- Large tool preparation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise real threads through scheduled projection and the normal renderer.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-prepare)

(defun mevedel-prepare-test--wait (predicate)
  "Run callbacks until PREDICATE holds, with a bounded test deadline."
  (let ((deadline (+ (float-time) 5)))
    (while (not (funcall predicate))
      (should (< (float-time) deadline))
      (sleep-for 0.002))))

(defmacro mevedel-prepare-test--with-tool (&rest body)
  "Run BODY with a settled tool and a draft, cleaning up every owned thread."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (let ((mevedel-view-prepare--min-size 1) threads)
       (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
       (with-current-buffer data-buf
         (insert "#+begin_tool\n")
         (insert (propertize "(:name \"Custom\" :args nil)\n\nbody\n"
                             'gptel '(tool . "prepare-id")))
         (insert "#+end_tool\n"))
       (unwind-protect
           (with-current-buffer view-buf
             (mevedel-view--full-rerender)
             (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
             (clrhash mevedel-view--tool-rendering-cache)
             ,@body)
         (when (buffer-live-p view-buf)
           (with-current-buffer view-buf
             (setq threads (delq nil (mapcar #'mevedel-view-prepare--job-thread mevedel-view-prepare--jobs)))
             (mevedel-view-prepare-cancel)))
         (mevedel-prepare-test--wait
          (lambda () (not (cl-some #'thread-live-p threads))))))))

(defmacro mevedel-prepare-test--paused (&rest body)
  "Start a real parser paused at a checkpoint, then run BODY.
BODY can release it by setting RELEASED and inspect WORKER."
  (declare (indent 0))
  `(let ((original (symbol-function 'mevedel-view-prepare--checkpoint))
         paused released worker)
     (cl-letf (((symbol-function 'mevedel-view-prepare--checkpoint)
                (lambda (job)
                  (setq paused t)
                  (while (not released) (sleep-for 0.001))
                  (funcall original job))))
       (unwind-protect
           (progn
             (mevedel-view-render-batched-full)
             (mevedel-prepare-test--wait (lambda () paused))
             (setq worker (mevedel-view-prepare--job-thread (car mevedel-view-prepare--jobs)))
             (should (thread-live-p worker))
             ,@body)
         (setq released t)
         (when (and worker (thread-live-p worker)) (thread-signal worker 'quit nil))
         (when worker
           (mevedel-prepare-test--wait (lambda () (not (thread-live-p worker)))))))))

(mevedel-deftest mevedel-view-prepare ()
  ,test
  (test)
  :doc "shows a pending row immediately and converges without changing the draft"
  (mevedel-prepare-test--with-tool
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (mevedel-view-render-batched-full)
      (should mevedel-view-prepare--jobs)
      (should (string-search "Custom: preparing result" (buffer-string)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (mevedel-prepare-test--wait
       (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max))))))
  :doc "new typing and selection survive publication"
  (mevedel-prepare-test--with-tool
    (mevedel-view-render-batched-full)
    (goto-char (point-max)) (insert "\nnew typing")
    (set-mark (- (point) 6)) (setq mark-active t)
    (mevedel-prepare-test--wait
     (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
    (should (equal "> draft\nsecond line\nnew typing" (mevedel-view--input-text)))
    (should mark-active)
    (should (equal "typing" (buffer-substring-no-properties (mark) (point))))
    (should-not (string-search "preparing result" (buffer-string)))))

(mevedel-deftest mevedel-view-prepare/context ()
  ,test
  (test)
  :doc "parsing does not invoke the renderer, which sees current main-thread context"
  (mevedel-prepare-test--with-tool
    (let ((calls nil))
      (mevedel-tool-register
       (mevedel-tool--create
        :name "Custom" :category "mevedel"
        :renderer (lambda (&rest _)
                    (push (current-thread) calls)
                    '(:header "Current renderer" :status success))))
      (mevedel-view-render-batched-full)
      (should-not calls)
      (mevedel-prepare-test--wait
       (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
      (should (equal calls (list (current-thread))))
      (should (string-search "Current renderer" (buffer-string)))))
  :doc "request failures keep precedence over a malformed tool call"
  (mevedel-prepare-test--with-tool
    (with-current-buffer data-buf
      (erase-buffer)
      (insert (mevedel-tool-render-data-format
               '(:kind request-summary :outcome error :backend "Example" :message "No reply"))))
    (let* ((prepared (mevedel-view--prepare-tool-segment data-buf 1 (with-current-buffer data-buf (point-max))))
           (rendering (mevedel-view--render-prepared-tool prepared data-buf t)))
      (should (plist-get prepared :request-failure))
      (should-not (plist-get prepared :call))
      (should (eq (plist-get rendering :vtype) 'request-failure))
      (should (eq (plist-get rendering :status) 'error))
      (should (string-search "No reply" (plist-get rendering :body))))))

(mevedel-deftest mevedel-view-prepare/local-publication ()
  (mevedel-prepare-test--with-tool
    (with-current-buffer data-buf
      (goto-char (point-max))
      (insert "*** Later question\n")
      (insert (propertize "Later answer\n" 'gptel 'response)))
    (let ((render (symbol-function 'mevedel-view--render-turn))
          (calls 0))
      (mevedel-prepare-test--paused
        (mevedel-prepare-test--wait (lambda () (not mevedel-view-render--batch)))
        (let ((expected-draft (mevedel-view--input-text)))
          (cl-letf (((symbol-function 'mevedel-view--render-turn)
                     (lambda (&rest args) (cl-incf calls) (apply render args))))
            (setq released t)
            (mevedel-prepare-test--wait
             (lambda () (and (not mevedel-view-prepare--jobs)
                             (not mevedel-view-render--batch)))))
          (should (= calls 1))
          (should (equal expected-draft (mevedel-view--input-text)))
          (should (string-search "Later answer" (buffer-string)))
          (should-not (string-search "preparing result" (buffer-string))))))))

(mevedel-deftest mevedel-view-render--refresh-source ()
  ,test
  (test)
  :doc "rejects changed text, provenance, and a replaced source without mutation"
  (dolist (change '(text property source))
    (mevedel-prepare-test--with-tool
      (let* ((pos (car (last (mevedel-view--rendered-turn-starts))))
             (context (get-text-property pos 'mevedel-view-turn-context))
             (turn (plist-get context :turn))
             (before (buffer-string)))
        (pcase change
          ('text (with-current-buffer data-buf
                   (goto-char (point-max)) (insert "more")))
          ('property (with-current-buffer data-buf
                       (put-text-property 1 2 'gptel 'response))))
        (let ((mevedel--data-buffer
               (if (eq change 'source) view-buf mevedel--data-buffer)))
          (should-not (mevedel-view-render--refresh-source
                       data-buf (plist-get turn :start) (plist-get turn :end))))
        (should (equal-including-properties before (buffer-string))))))
  :doc "keeps a whole-turn fold across local replacement"
  (mevedel-prepare-test--with-tool
    (let* ((pos (car (last (mevedel-view--rendered-turn-starts))))
           (context (get-text-property pos 'mevedel-view-turn-context))
           (turn (plist-get context :turn)))
      (goto-char pos)
      (mevedel-view-render-toggle-turn nil)
      (should (eq 'turn-summary (get-text-property pos 'mevedel-view-type)))
      (should (eq context (get-text-property pos 'mevedel-view-turn-context)))
      (should (mevedel-view-render--refresh-source
               data-buf (plist-get turn :start) (plist-get turn :end)))
      (should (eq 'turn-summary (get-text-property pos 'mevedel-view-type)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))))
  :doc "rolls back a failed local projection before requesting the full fallback"
  (mevedel-prepare-test--with-tool
    (let* ((pos (car (last (mevedel-view--rendered-turn-starts))))
           (context (get-text-property pos 'mevedel-view-turn-context))
           (turn (plist-get context :turn))
           (before (buffer-string)))
      (cl-letf (((symbol-function 'mevedel-view--render-turn)
                 (lambda (&rest _) (insert "partial") (error "injected render failure"))))
        (should-not (mevedel-view-render--refresh-source
                     data-buf (plist-get turn :start) (plist-get turn :end))))
      (should (equal-including-properties before (buffer-string)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

(mevedel-deftest mevedel-view-prepare/changed-session ()
  (mevedel-prepare-test--with-tool
    (mevedel-prepare-test--paused
      (let ((full (symbol-function 'mevedel-view-render-batched-full))
            (calls 0))
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local mevedel--session
                            (mevedel-session--create :name "changed context")))
              (cl-letf (((symbol-function 'mevedel-view-render-batched-full)
                         (lambda () (cl-incf calls) (funcall full))))
                (setq released t)
                (mevedel-prepare-test--wait
                 (lambda () (and (not mevedel-view-prepare--jobs)
                                 (not mevedel-view-render--batch)))))
              (should (> calls 0))
              (should-not (string-search "preparing result" (buffer-string)))
              (should (equal "> draft\nsecond line" (mevedel-view--input-text))))
          (with-current-buffer data-buf (setq-local mevedel--session nil)))))))

(mevedel-deftest mevedel-view-prepare/lifetime ()
  ,test
  (test)
  :doc "a synchronous full render retires pending preparation"
  (mevedel-prepare-test--with-tool
    (mevedel-view-render-batched-full)
    (should mevedel-view-prepare--jobs)
    (mevedel-view--full-rerender)
    (should-not mevedel-view-prepare--jobs)
    (should-not mevedel-view-prepare--timer)
    (should-not (string-search "preparing result" (buffer-string)))))

(mevedel-deftest mevedel-view-prepare/checkpoints ()
  ,test
  (test)
  :doc "cancellation at a preparation checkpoint never invokes a renderer"
  (mevedel-prepare-test--with-tool
    (let ((checkpoints 0) cancelled
          (mevedel-view-render--parse-checkpoint
           (lambda () (signal 'quit nil))))
      (condition-case nil
          (progn
            (mevedel-view--prepare-tool-segment data-buf 1 (with-current-buffer data-buf (point-max)))
            (cl-incf checkpoints))
        (quit (setq cancelled t)))
      (should cancelled)
      (should (= checkpoints 0)))))

(mevedel-deftest mevedel-view-prepare/source-changes ()
  ,test
  (test)
  :doc "edits, appends and restored provenance converge to current source"
  (dolist (mutation '(text append properties truncate))
    (mevedel-prepare-test--with-tool
      (mevedel-prepare-test--paused
        (with-current-buffer data-buf
          (pcase mutation
            ('text (goto-char (point-min)) (search-forward "body")
                   (replace-match "replacement result"))
            ('append (goto-char (point-max)) (insert "\n*** Another prompt\n"))
            ('properties (put-text-property 24 25 'mevedel-render-data t))
            ('truncate (delete-region 24 (point-max)))))
        (setq released t)
        (mevedel-prepare-test--wait
         (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
        (let ((actual (buffer-substring-no-properties (point-min) (point-max))))
          (mevedel-view--full-rerender)
          (ert-info ((format "Mutation: %s" mutation))
            (should (equal actual (buffer-substring-no-properties (point-min) (point-max))))))))))

(mevedel-deftest mevedel-view-prepare/cancellation ()
  ,test
  (test)
  :doc "killing either buffer or changing the view mode stops owned work"
  (dolist (target '(view source mode))
    (mevedel-prepare-test--with-tool
      (mevedel-prepare-test--paused
        (pcase target
          ('view (kill-buffer view-buf))
          ('source (kill-buffer data-buf))
          ('mode (fundamental-mode)))
        (mevedel-prepare-test--wait (lambda () (not (thread-live-p worker))))
        (when (buffer-live-p view-buf)
          (with-current-buffer view-buf
            (should-not mevedel-view-prepare--jobs)
            (should-not mevedel-view-prepare--timer)))))))

(mevedel-deftest mevedel-view-prepare/attention ()
  ,test
  (test)
  :doc "focus return resumes paused preparation without losing its row"
  (mevedel-prepare-test--with-tool
    (let (unattended)
      (cl-letf (((symbol-function 'mevedel-view--unattended-p) (lambda (&rest _) unattended)))
        (mevedel-prepare-test--paused
          (setq unattended t released t)
          (sleep-for 0.01)
          (should mevedel-view-prepare--jobs)
          (should (thread-live-p worker))
          (setq unattended nil)
          (mevedel-view-prepare-resume)
          (mevedel-prepare-test--wait
           (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
          (should-not (string-search "preparing result" (buffer-string))))))))

(mevedel-deftest mevedel-view-prepare/fallback ()
  ,test
  (test)
  :doc "a worker failure falls back once instead of retrying indefinitely"
  (mevedel-prepare-test--with-tool
    (let ((original (symbol-function 'mevedel-view--prepare-tool-segment))
          (main (current-thread)) (worker-calls 0))
      (cl-letf (((symbol-function 'mevedel-view--prepare-tool-segment)
                 (lambda (&rest args)
                   (unless (eq main (current-thread))
                     (cl-incf worker-calls)
                     (error "Preparation failed"))
                   (apply original args))))
        (mevedel-view-render-batched-full)
        (mevedel-prepare-test--wait
         (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
        (should (= 1 worker-calls))
        (should-not (string-search "preparing result" (buffer-string)))))))

(mevedel-deftest mevedel-view-prepare/timers ()
  ,test
  (test)
  :doc "worker waits never dispatch editor timers or stale-job recovery"
  (mevedel-prepare-test--with-tool
    (let (timer calls)
      (unwind-protect
          (mevedel-prepare-test--paused
            (setq timer (run-at-time 0 0.001
                                     (lambda () (push (current-thread) calls))))
            (sleep-for 0.025)
            (should calls)
            (should (cl-every (lambda (thread) (eq thread main-thread)) calls))
            (with-current-buffer data-buf
              (goto-char (point-min)) (search-forward "body")
              (replace-match "replacement result"))
            (sleep-for 0.01)
            (setq released t)
            (mevedel-prepare-test--wait
             (lambda () (and (not mevedel-view-prepare--jobs)
                             (not mevedel-view-render--batch))))
            (should-not (string-search "preparing result" (buffer-string))))
        (when timer (cancel-timer timer))))))

(mevedel-deftest mevedel-view-prepare/queue ()
  ,test
  (test)
  :doc "evicted source identities reuse a paused job without duplicate admission"
  (mevedel-prepare-test--with-tool
    (mevedel-prepare-test--paused
      (let ((job (car mevedel-view-prepare--jobs)))
        (with-current-buffer data-buf (clrhash mevedel-view--source-revisions))
        (mevedel-view-render-batched-full)
        (should (equal mevedel-view-prepare--jobs (list job)))
        (setq released t)
        (mevedel-prepare-test--wait
         (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
        (should-not (string-search "preparing result" (buffer-string))))))
  :doc "publication queued behind an active writer validates after ownership"
  (mevedel-prepare-test--with-tool
    (mevedel-prepare-test--paused
      (let ((job (car mevedel-view-prepare--jobs)))
        (mevedel-view-render-mutate
         'test
         (lambda ()
           (with-current-buffer data-buf
             (goto-char (point-min)) (search-forward "body")
             (replace-match "replacement result"))
           (mevedel-view-prepare--advance view-buf)
           (should (memq job mevedel-view-prepare--jobs))))
        (setq released t)
        (mevedel-prepare-test--wait
         (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
        (should-not (string-search "preparing result" (buffer-string))))))
  :doc "a publication exception retires the job and recovers through the normal renderer"
  (mevedel-prepare-test--with-tool
    (let ((original (symbol-function 'mevedel-view--render-prepared-tool)) failed)
      (cl-letf (((symbol-function 'mevedel-view--render-prepared-tool)
                 (lambda (&rest args)
                   (unless failed (setq failed t) (error "Publication failed"))
                   (apply original args))))
        (mevedel-view-render-batched-full)
        (mevedel-prepare-test--wait
         (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
        (should failed)
        (should-not (string-search "preparing result" (buffer-string)))))))

(mevedel-deftest mevedel-view-prepare/multiple ()
  ,test
  (test)
  :doc "several tools deduplicate their jobs and converge in source order"
  (mevedel-prepare-test--with-tool
    (dotimes (index 4)
      (with-current-buffer data-buf
        (goto-char (point-max))
        (insert "\n#+begin_tool\n"
                (propertize (format "(:name \"Custom\" :args nil)\n\nresult %d\n" index)
                            'gptel (cons 'tool (format "multi-%d" index)))
                "#+end_tool\n")))
    (mevedel-view--full-rerender)
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (clrhash mevedel-view--tool-rendering-cache)
      (mevedel-prepare-test--paused
        (should (= 5 (length mevedel-view-prepare--jobs)))
        (mevedel-view-render-batched-full)
        (should (= 5 (length mevedel-view-prepare--jobs)))
        (should (= 1 (cl-count-if #'mevedel-view-prepare--job-thread mevedel-view-prepare--jobs)))
        (setq released t)
        (mevedel-prepare-test--wait
         (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))
  :doc "a previously expanded tool remains expanded after preparation completes"
  (mevedel-prepare-test--with-tool
    (goto-char (point-min)) (search-forward "Custom")
    (mevedel-view-toggle-section)
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (clrhash mevedel-view--tool-rendering-cache)
      (goto-char (point-max))
      (mevedel-view-render-batched-full)
      (mevedel-prepare-test--wait
       (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))

(mevedel-deftest mevedel-view-prepare--redraw ()
  ,test
  (test)
  :doc "live first-arrival preparation updates the active turn and preserves the draft"
  (mevedel-prepare-test--with-tool
    (let ((expected (buffer-substring-no-properties (point-min) (mevedel-view--history-insertion-marker)))
          (source-start (with-current-buffer data-buf
                          (goto-char (point-min)) (search-forward "#+begin_tool")
                          (line-beginning-position))))
      (goto-char (point-min)) (search-forward "Assistant")
      (mevedel-view-stream-begin-turn
       (copy-marker (line-beginning-position))
       (with-current-buffer data-buf (copy-marker source-start)))
      (let ((mevedel-view-prepare-enabled t))
        (mevedel-view-render-live-update data-buf))
      (should mevedel-view-prepare--jobs)
      (mevedel-prepare-test--wait
       (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (should (equal expected (buffer-substring-no-properties (point-min) (mevedel-view--history-insertion-marker)))))))

(mevedel-deftest mevedel-view-prepare/malformed ()
  ,test
  (test)
  :doc "a complete but unrenderable tool does not retry preparation forever"
  (mevedel-prepare-test--with-tool
    (with-current-buffer data-buf
      (goto-char (point-min)) (search-forward ":name") (replace-match ":missing-name"))
    (mevedel-view--full-rerender)
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (mevedel-view-render-batched-full)
      (mevedel-prepare-test--wait
       (lambda () (and (not mevedel-view-prepare--jobs) (not mevedel-view-render--batch))))
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))

(mevedel-deftest mevedel-view-prepare/stale-pause ()
  ,test
  (test)
  :doc "stale source recovery waits for focus and an idle transport"
  (dolist (barrier '(focus transport))
    (mevedel-prepare-test--with-tool
      (let (blocked (redraws 0)
            (original (symbol-function 'mevedel-view-prepare--redraw)))
        (cl-letf (((symbol-function 'mevedel-view--unattended-p)
                   (lambda (&rest _) (and blocked (eq barrier 'focus))))
                  ((symbol-function 'mevedel-transport-busy-p)
                   (lambda (&rest _) (and blocked (eq barrier 'transport))))
                  ((symbol-function 'mevedel-view-prepare--redraw)
                   (lambda (&rest args) (cl-incf redraws) (apply original args))))
          (mevedel-prepare-test--paused
            (setq blocked t)
            (with-current-buffer data-buf
              (goto-char (point-min)) (search-forward "body")
              (replace-match "replacement result"))
            (setq released t)
            (sleep-for 0.02)
            (ert-info ((format "Barrier: %s" barrier))
              (should (= 0 redraws)))
            (setq blocked nil)
            (mevedel-view-prepare-resume)
            (mevedel-prepare-test--wait
             (lambda () (and (not mevedel-view-prepare--jobs)
                             (not mevedel-view-render--batch))))
            (should (> redraws 0))
            (should-not (string-search "preparing result" (buffer-string)))))))))

(provide 'test-mevedel-view-prepare)
;;; test-mevedel-view-prepare.el ends here
