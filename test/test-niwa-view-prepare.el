;;; test-niwa-view-prepare.el --- Large tool preparation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise real threads through scheduled projection and the normal renderer.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-prepare)

(defun niwa-prepare-test--wait (predicate)
  "Run callbacks until PREDICATE holds, with a bounded test deadline."
  (let ((deadline (+ (float-time) 5)))
    (while (not (funcall predicate))
      (should (< (float-time) deadline))
      (sleep-for 0.002))))

(defmacro niwa-prepare-test--with-tool (&rest body)
  "Run BODY with a settled tool and a draft, cleaning up every owned thread."
  (declare (indent 0))
  `(niwa-view-test--with-buffers
     (let ((niwa-view-prepare--min-size 1) threads)
       (niwa-view-test--insert-data data-buf "*** Prompt\n" nil)
       (with-current-buffer data-buf
         (insert "#+begin_tool\n")
         (insert (propertize "(:name \"Custom\" :args nil)\n\nbody\n"
                             'gptel '(tool . "prepare-id")))
         (insert "#+end_tool\n"))
       (unwind-protect
           (with-current-buffer view-buf
             (niwa-view--full-rerender)
             (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
             (clrhash niwa-view--tool-rendering-cache)
             ,@body)
         (when (buffer-live-p view-buf)
           (with-current-buffer view-buf
             (setq threads (delq nil (mapcar #'niwa-view-prepare--job-thread niwa-view-prepare--jobs)))
             (niwa-view-prepare-cancel)))
         (niwa-prepare-test--wait
          (lambda () (not (cl-some #'thread-live-p threads))))))))

(defmacro niwa-prepare-test--paused (&rest body)
  "Start a real parser paused at a checkpoint, then run BODY.
BODY can release it by setting RELEASED and inspect WORKER."
  (declare (indent 0))
  `(let ((original (symbol-function 'niwa-view-prepare--checkpoint))
         paused released worker)
     (cl-letf (((symbol-function 'niwa-view-prepare--checkpoint)
                (lambda (job)
                  (setq paused t)
                  (while (not released) (thread-yield))
                  (funcall original job))))
       (unwind-protect
           (progn
             (niwa-view-render-batched-full)
             (niwa-prepare-test--wait (lambda () paused))
             (setq worker (niwa-view-prepare--job-thread (car niwa-view-prepare--jobs)))
             (should (thread-live-p worker))
             ,@body)
         (setq released t)
         (when (and worker (thread-live-p worker)) (thread-signal worker 'quit nil))
         (when worker
           (niwa-prepare-test--wait (lambda () (not (thread-live-p worker)))))))))

(niwa-deftest niwa-view-prepare ()
  ,test
  (test)
  :doc "shows a pending row after staged preparation and preserves the draft"
  (niwa-prepare-test--with-tool
    (goto-char (point-max))
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (niwa-prepare-test--paused
        (should niwa-view-prepare--jobs)
        (should (string-search "Custom: preparing result" (buffer-string)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
        (setq released t)
        (niwa-prepare-test--wait
         (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))
  :doc "new typing and selection survive publication"
  (niwa-prepare-test--with-tool
    (niwa-view-render-batched-full)
    (niwa-view-composer-insert "\nnew typing")
    (with-current-buffer (niwa-view-composer-buffer)
      (set-mark (- (point) 6))
      (setq mark-active t))
    (niwa-prepare-test--wait
     (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
    (should (equal "> draft\nsecond line\nnew typing" (niwa-view-composer-draft-text)))
    (with-current-buffer (niwa-view-composer-buffer)
      (should mark-active)
      (should (equal "typing" (buffer-substring-no-properties (mark) (point)))))
    (should-not (string-search "preparing result" (buffer-string)))))

(niwa-deftest niwa-view-prepare/groups
  (:doc "a result being prepared keeps its tool run folded into one row")
  (niwa-view-test--with-buffers
    (let ((niwa-view-prepare--min-size 400)
          (niwa-view-tool-group-collapse-threshold 3))
      (niwa-view-test--insert-data data-buf "*** Prompt\n" nil)
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\n%s\n"
                 i (if (= i 3) (make-string 600 ?y) "short"))
         `(tool . ,(format "call_%d" i))))
      (niwa-view-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (clrhash niwa-view--tool-rendering-cache)
        (goto-char (point-max))
        (let ((expected (buffer-substring-no-properties (point-min) (point-max)))
              threads)
          (unwind-protect
              (niwa-prepare-test--paused
                (should niwa-view-prepare--jobs)
                ;; The pending row is a member: the run keeps its one row.
                (should (equal expected (buffer-substring-no-properties
                                         (point-min) (point-max))))
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
                (setq released t)
                (niwa-prepare-test--wait
                 (lambda () (and (not niwa-view-prepare--jobs)
                                 (not niwa-view-render--batch))))
                (should (equal expected (buffer-substring-no-properties
                                         (point-min) (point-max)))))
            (setq threads (delq nil (mapcar #'niwa-view-prepare--job-thread
                                            niwa-view-prepare--jobs)))
            (niwa-view-prepare-cancel)
            (niwa-prepare-test--wait
             (lambda () (not (cl-some #'thread-live-p threads))))))))))

(niwa-deftest niwa-view-prepare/context ()
  ,test
  (test)
  :doc "parsing does not invoke the renderer, which sees current main-thread context"
  (niwa-prepare-test--with-tool
    (let ((calls nil))
      (niwa-tool-register
       (niwa-tool--create
        :name "Custom" :category "niwa"
        :renderer (lambda (&rest _)
                    (push (current-thread) calls)
                    '(:header "Current renderer" :status success))))
      (niwa-view-render-batched-full)
      (should-not calls)
      (niwa-prepare-test--wait
       (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
      (should (equal calls (list (current-thread))))
      (should (string-search "Current renderer" (buffer-string)))))
  :doc "request failures keep precedence over a malformed tool call"
  (niwa-prepare-test--with-tool
    (with-current-buffer data-buf
      (erase-buffer)
      (insert (niwa-tool-render-data-format
               '(:kind request-summary :outcome error :backend "Example" :message "No reply"))))
    (let* ((prepared (niwa-view--prepare-tool-segment data-buf 1 (with-current-buffer data-buf (point-max))))
           (rendering (niwa-view--render-prepared-tool prepared data-buf t)))
      (should (plist-get prepared :request-failure))
      (should-not (plist-get prepared :call))
      (should (eq (plist-get rendering :vtype) 'request-failure))
      (should (eq (plist-get rendering :status) 'error))
      (should (string-search "No reply" (plist-get rendering :body))))))

(niwa-deftest niwa-view-prepare/local-publication ()
  (niwa-prepare-test--with-tool
    (with-current-buffer data-buf
      (goto-char (point-max))
      (insert "*** Later question\n")
      (insert (propertize "Later answer\n" 'gptel 'response)))
    (let ((render (symbol-function 'niwa-view--render-turn))
          (calls 0))
      (niwa-prepare-test--paused
        (niwa-prepare-test--wait (lambda () (not niwa-view-render--batch)))
        (let ((expected-draft (niwa-view-composer-draft-text)))
          (cl-letf (((symbol-function 'niwa-view--render-turn)
                     (lambda (&rest args) (cl-incf calls) (apply render args))))
            (setq released t)
            (niwa-prepare-test--wait
             (lambda () (and (not niwa-view-prepare--jobs)
                             (not niwa-view-render--batch)))))
          (should (= calls 1))
          (should (equal expected-draft (niwa-view-composer-draft-text)))
          (should (string-search "Later answer" (buffer-string)))
          (should-not (string-search "preparing result" (buffer-string))))))))

(niwa-deftest niwa-view-render--refresh-source ()
  ,test
  (test)
  :doc "rejects changed text, provenance, and a replaced source without mutation"
  (dolist (change '(text property source))
    (niwa-prepare-test--with-tool
      (let* ((pos (car (last (niwa-view--rendered-turn-starts))))
             (context (get-text-property pos 'niwa-view-turn-context))
             (turn (plist-get context :turn))
             (before (buffer-string)))
        (pcase change
          ('text (with-current-buffer data-buf
                   (goto-char (point-max)) (insert "more")))
          ('property (with-current-buffer data-buf
                       (put-text-property 1 2 'gptel 'response))))
        (let ((niwa--data-buffer
               (if (eq change 'source) view-buf niwa--data-buffer)))
          (should-not (niwa-view-render--refresh-source
                       data-buf (plist-get turn :start) (plist-get turn :end))))
        (should (equal-including-properties before (buffer-string))))))
  :doc "keeps a whole-turn fold across local replacement"
  (niwa-prepare-test--with-tool
    (let* ((pos (car (last (niwa-view--rendered-turn-starts))))
           (context (get-text-property pos 'niwa-view-turn-context))
           (turn (plist-get context :turn)))
      (goto-char pos)
      (niwa-view-render-toggle-turn nil)
      (should (eq 'turn-summary (get-text-property pos 'niwa-view-type)))
      (should (eq context (get-text-property pos 'niwa-view-turn-context)))
      (should (niwa-view-render--refresh-source
               data-buf (plist-get turn :start) (plist-get turn :end)))
      (should (eq 'turn-summary (get-text-property pos 'niwa-view-type)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))))
  :doc "rolls back a failed local projection before requesting the full fallback"
  (niwa-prepare-test--with-tool
    (let* ((pos (car (last (niwa-view--rendered-turn-starts))))
           (context (get-text-property pos 'niwa-view-turn-context))
           (turn (plist-get context :turn))
           (before (buffer-string)))
      (cl-letf (((symbol-function 'niwa-view--render-turn)
                 (lambda (&rest _) (insert "partial") (error "injected render failure"))))
        (should-not (niwa-view-render--refresh-source
                     data-buf (plist-get turn :start) (plist-get turn :end))))
      (should (equal-including-properties before (buffer-string)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))

(niwa-deftest niwa-view-prepare/changed-session ()
  (niwa-prepare-test--with-tool
    (niwa-prepare-test--paused
      (let ((full (symbol-function 'niwa-view-render-batched-full))
            (calls 0))
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local niwa--session
                            (niwa-session--create :name "changed context")))
              (cl-letf (((symbol-function 'niwa-view-render-batched-full)
                         (lambda () (cl-incf calls) (funcall full))))
                (setq released t)
                (niwa-prepare-test--wait
                 (lambda () (and (not niwa-view-prepare--jobs)
                                 (not niwa-view-render--batch)))))
              (should (> calls 0))
              (should-not (string-search "preparing result" (buffer-string)))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
          (with-current-buffer data-buf (setq-local niwa--session nil)))))))

(niwa-deftest niwa-view-prepare/lifetime ()
  ,test
  (test)
  :doc "a synchronous full render retires pending preparation"
  (niwa-prepare-test--with-tool
    (niwa-prepare-test--paused
      (should niwa-view-prepare--jobs)
      (niwa-view--full-rerender)
      (should-not niwa-view-prepare--jobs)
      (should-not niwa-view-prepare--timer)
      (should-not (string-search "preparing result" (buffer-string))))))

(niwa-deftest niwa-view-prepare/checkpoints ()
  ,test
  (test)
  :doc "cancellation at a preparation checkpoint never invokes a renderer"
  (niwa-prepare-test--with-tool
    (let ((checkpoints 0) cancelled
          (niwa-view-render--parse-checkpoint
           (lambda () (signal 'quit nil))))
      (condition-case nil
          (progn
            (niwa-view--prepare-tool-segment data-buf 1 (with-current-buffer data-buf (point-max)))
            (cl-incf checkpoints))
        (quit (setq cancelled t)))
      (should cancelled)
      (should (= checkpoints 0)))))

(niwa-deftest niwa-view-prepare/source-changes ()
  ,test
  (test)
  :doc "edits, appends and restored provenance converge to current source"
  (dolist (mutation '(text append properties truncate))
    (niwa-prepare-test--with-tool
      (niwa-prepare-test--paused
        (with-current-buffer data-buf
          (pcase mutation
            ('text (goto-char (point-min)) (search-forward "body")
                   (replace-match "replacement result"))
            ('append (goto-char (point-max)) (insert "\n*** Another prompt\n"))
            ('properties (put-text-property 24 25 'niwa-render-data t))
            ('truncate (delete-region 24 (point-max)))))
        (setq released t)
        (niwa-prepare-test--wait
         (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
        (let ((actual (buffer-substring-no-properties (point-min) (point-max))))
          (niwa-view--full-rerender)
          (ert-info ((format "Mutation: %s" mutation))
            (should (equal actual (buffer-substring-no-properties (point-min) (point-max))))))))))

(niwa-deftest niwa-view-prepare/cancellation ()
  ,test
  (test)
  :doc "killing either buffer or changing the view mode stops owned work"
  (dolist (target '(view source mode))
    (niwa-prepare-test--with-tool
      (niwa-prepare-test--paused
        (pcase target
          ('view (kill-buffer view-buf))
          ('source (kill-buffer data-buf))
          ('mode (fundamental-mode)))
        (niwa-prepare-test--wait (lambda () (not (thread-live-p worker))))
        (when (buffer-live-p view-buf)
          (with-current-buffer view-buf
            (should-not niwa-view-prepare--jobs)
            (should-not niwa-view-prepare--timer)))))))

(niwa-deftest niwa-view-prepare/attention ()
  ,test
  (test)
  :doc "focus return resumes paused preparation without losing its row"
  (niwa-prepare-test--with-tool
    (let (unattended)
      (cl-letf (((symbol-function 'niwa-view--unattended-p) (lambda (&rest _) unattended)))
        (niwa-prepare-test--paused
          (setq unattended t released t)
          (sleep-for 0.01)
          (should niwa-view-prepare--jobs)
          (should (thread-live-p worker))
          (setq unattended nil)
          (niwa-view-prepare-resume)
          (niwa-prepare-test--wait
           (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
          (should-not (string-search "preparing result" (buffer-string))))))))

(niwa-deftest niwa-view-prepare/fallback ()
  ,test
  (test)
  :doc "a worker failure falls back once instead of retrying indefinitely"
  (niwa-prepare-test--with-tool
    (let ((original (symbol-function 'niwa-view--prepare-tool-segment))
          (main (current-thread)) (worker-calls 0))
      (cl-letf (((symbol-function 'niwa-view--prepare-tool-segment)
                 (lambda (&rest args)
                   (unless (eq main (current-thread))
                     (cl-incf worker-calls)
                     (error "Preparation failed"))
                   (apply original args))))
        (niwa-view-render-batched-full)
        (niwa-prepare-test--wait
         (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
        (should (= 1 worker-calls))
        (should-not (string-search "preparing result" (buffer-string)))))))

(niwa-deftest niwa-view-prepare/timers ()
  ,test
  (test)
  :doc "worker waits never dispatch editor timers or stale-job recovery"
  (niwa-prepare-test--with-tool
    (let (timer calls)
      (unwind-protect
          (niwa-prepare-test--paused
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
            (niwa-prepare-test--wait
             (lambda () (and (not niwa-view-prepare--jobs)
                             (not niwa-view-render--batch))))
            (should-not (string-search "preparing result" (buffer-string))))
        (when timer (cancel-timer timer))))))

(niwa-deftest niwa-view-prepare/process-sentinels ()
  (niwa-prepare-test--with-tool
    (let (process unattended calls errors)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-view--unattended-p)
                     (lambda (&rest _) unattended)))
            (setq process
                  (make-process
                   :name "niwa-prepare-sentinel-test"
                   :command '("sh" "-c" "sleep 0.01")
                   :connection-type 'pipe :noquery t
                   :sentinel
                   (lambda (child _event)
                     (push (current-thread) calls)
                     ;; Native compilation drains final output in its sentinel.
                     (condition-case err
                         (while (accept-process-output child 0.001 nil 1))
                       (error (push err errors))))))
            (niwa-view-render-batched-full)
            (niwa-view-prepare--advance view-buf)
            (setq unattended t)
            ;; Let the worker wait across child exit, without dispatching
            ;; callbacks on the main thread until afterward.
            (let ((deadline (+ (float-time) 5)))
              (while (and (process-live-p process) (< (float-time) deadline))
                (thread-yield)))
            (should-not (process-live-p process))
            (should-not calls)
            (setq unattended nil)
            (niwa-view-prepare-resume)
            (niwa-prepare-test--wait
             (lambda () (and calls (not niwa-view-prepare--jobs)
                             (not niwa-view-render--batch))))
            (should-not errors)
            (should (cl-every (lambda (thread) (eq thread main-thread)) calls))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
        (when process
          (set-process-sentinel process #'ignore)
          (delete-process process))))))

(niwa-deftest niwa-view-prepare/queue ()
  ,test
  (test)
  :doc "evicted source identities reuse a paused job without duplicate admission"
  (niwa-prepare-test--with-tool
    (niwa-prepare-test--paused
      (let ((job (car niwa-view-prepare--jobs)))
        (with-current-buffer data-buf (clrhash niwa-view--source-revisions))
        (niwa-view-render-batched-full)
        (should (equal niwa-view-prepare--jobs (list job)))
        (setq released t)
        (niwa-prepare-test--wait
         (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
        (should-not (string-search "preparing result" (buffer-string))))))
  :doc "publication queued behind an active writer validates after ownership"
  (niwa-prepare-test--with-tool
    (niwa-prepare-test--paused
      ;; A transcript writer would otherwise finish the batch and retire the job.
      (niwa-prepare-test--wait (lambda () (not niwa-view-render--batch)))
      (let ((job (car niwa-view-prepare--jobs)))
        (should job)
        (niwa-view-render-mutate
         'test
         (lambda ()
           (with-current-buffer data-buf
             (goto-char (point-min)) (search-forward "body")
             (replace-match "replacement result"))
           (niwa-view-prepare--advance view-buf)
           (should (memq job niwa-view-prepare--jobs))))
        (setq released t)
        (niwa-prepare-test--wait
         (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
        (should-not (string-search "preparing result" (buffer-string))))))
  :doc "a publication exception retires the job and recovers through the normal renderer"
  (niwa-prepare-test--with-tool
    (let ((original (symbol-function 'niwa-view--render-prepared-tool)) failed)
      (cl-letf (((symbol-function 'niwa-view--render-prepared-tool)
                 (lambda (&rest args)
                   (unless failed (setq failed t) (error "Publication failed"))
                   (apply original args))))
        (niwa-view-render-batched-full)
        (niwa-prepare-test--wait
         (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
        (should failed)
        (should-not (string-search "preparing result" (buffer-string)))))))

(niwa-deftest niwa-view-prepare/multiple ()
  ,test
  (test)
  :doc "several tools deduplicate their jobs and converge in source order"
  (niwa-prepare-test--with-tool
    (dotimes (index 4)
      (with-current-buffer data-buf
        (goto-char (point-max))
        (insert "\n#+begin_tool\n"
                (propertize (format "(:name \"Custom\" :args nil)\n\nresult %d\n" index)
                            'gptel (cons 'tool (format "multi-%d" index)))
                "#+end_tool\n")))
    (niwa-view--full-rerender)
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (clrhash niwa-view--tool-rendering-cache)
      (niwa-prepare-test--paused
        (should (= 5 (length niwa-view-prepare--jobs)))
        (niwa-view-render-batched-full)
        (should (= 5 (length niwa-view-prepare--jobs)))
        (should (= 1 (cl-count-if #'niwa-view-prepare--job-thread niwa-view-prepare--jobs)))
        (setq released t)
        (niwa-prepare-test--wait
         (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))
  :doc "a previously expanded tool remains expanded after preparation completes"
  (niwa-prepare-test--with-tool
    (goto-char (point-min)) (search-forward "Custom")
    (niwa-view-toggle-section)
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (clrhash niwa-view--tool-rendering-cache)
      (goto-char (point-max))
      (niwa-view-render-batched-full)
      (niwa-prepare-test--wait
       (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))

(niwa-deftest niwa-view-prepare--redraw ()
  ,test
  (test)
  :doc "live first-arrival preparation updates the active turn and preserves the draft"
  (niwa-prepare-test--with-tool
    (let ((expected (buffer-substring-no-properties (point-min) (niwa-view--history-insertion-marker)))
          (source-start (with-current-buffer data-buf
                          (goto-char (point-min)) (search-forward "#+begin_tool")
                          (line-beginning-position))))
      (goto-char (point-min)) (search-forward "Assistant")
      (niwa-view-stream-begin-turn
       (copy-marker (line-beginning-position))
       (with-current-buffer data-buf (copy-marker source-start)))
      (let ((niwa-view-prepare-enabled t))
        (niwa-view-render-live-update data-buf))
      (should niwa-view-prepare--jobs)
      (niwa-prepare-test--wait
       (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
      (should (equal expected (buffer-substring-no-properties (point-min) (niwa-view--history-insertion-marker)))))))

(niwa-deftest niwa-view-prepare/malformed ()
  ,test
  (test)
  :doc "a complete but unrenderable tool does not retry preparation forever"
  (niwa-prepare-test--with-tool
    (with-current-buffer data-buf
      (goto-char (point-min)) (search-forward ":name") (replace-match ":missing-name"))
    (niwa-view--full-rerender)
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (niwa-view-render-batched-full)
      (niwa-prepare-test--wait
       (lambda () (and (not niwa-view-prepare--jobs) (not niwa-view-render--batch))))
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))

(niwa-deftest niwa-view-prepare/stale-pause ()
  ,test
  (test)
  :doc "stale source recovery waits for focus and an idle transport"
  (dolist (barrier '(focus transport))
    (niwa-prepare-test--with-tool
      (let (blocked (redraws 0)
            (original (symbol-function 'niwa-view-prepare--redraw)))
        (cl-letf (((symbol-function 'niwa-view--unattended-p)
                   (lambda (&rest _) (and blocked (eq barrier 'focus))))
                  ((symbol-function 'niwa-transport-busy-p)
                   (lambda (&rest _) (and blocked (eq barrier 'transport))))
                  ((symbol-function 'niwa-view-prepare--redraw)
                   (lambda (&rest args) (cl-incf redraws) (apply original args))))
          (niwa-prepare-test--paused
            (setq blocked t)
            (with-current-buffer data-buf
              (goto-char (point-min)) (search-forward "body")
              (replace-match "replacement result"))
            (setq released t)
            (sleep-for 0.02)
            (ert-info ((format "Barrier: %s" barrier))
              (should (= 0 redraws)))
            (setq blocked nil)
            (niwa-view-prepare-resume)
            (niwa-prepare-test--wait
             (lambda () (and (not niwa-view-prepare--jobs)
                             (not niwa-view-render--batch))))
            (should (> redraws 0))
            (should-not (string-search "preparing result" (buffer-string)))))))))

(provide 'test-niwa-view-prepare)
;;; test-niwa-view-prepare.el ends here
