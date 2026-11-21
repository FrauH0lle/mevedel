;;; test-niwa-view-render-batch.el --- Batched history projection -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise callback ownership and live reader state during complete rebuilds.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)

(defmacro niwa-batch-test--with-history (&rest body)
  "Run BODY with six exchanges and a multiline composer draft."
  (declare (indent 0))
  `(niwa-view-test--with-buffers
     (dotimes (index 6)
       (niwa-view-test--insert-data data-buf (format "Prompt %d\n" index) nil)
       (niwa-view-test--insert-data data-buf (format "Response %d with target text.\n" index) 'response))
     (with-current-buffer view-buf
       (niwa-view--full-rerender)
       (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
       ,@body)))

(defun niwa-batch-test--start-projection (view)
  "Start a settled batch and advance VIEW through source preparation."
  (niwa-view-render-batched-full)
  (cl-loop repeat 100 while (and niwa-view-render--batch
                                 (eq 'prepare (plist-get niwa-view-render--batch :phase)))
           do (niwa-view-render--batch-step view niwa-view-render--batch))
  (should (eq 'project (plist-get niwa-view-render--batch :phase))))

(defun niwa-batch-test--start-projection-showing (view text)
  "Start a batch in VIEW while its window shows only through TEXT.
Turns below TEXT stay offscreen, so they remain pending placeholders."
  (let ((end (save-excursion
               (goto-char (point-min)) (search-forward text) (point))))
    (cl-letf (((symbol-function 'window-end) (lambda (&rest _) end)))
      (niwa-batch-test--start-projection view))))

(niwa-deftest niwa-view-render-batched-full ()
  ,test
  (test)
  :doc "cache collection waits for the complete staged projection"
  (niwa-batch-test--with-history
    (puthash 'obsolete 'obsolete niwa-view--tool-rendering-cache)
    (niwa-batch-test--start-projection view-buf)
    (should (gethash 'obsolete niwa-view--tool-rendering-cache))
    (cl-loop repeat 30 while niwa-view-render--batch do
      (niwa-view-render--batch-step view-buf niwa-view-render--batch))
    (should-not niwa-view-render--batch)
    (should-not (gethash 'obsolete niwa-view--tool-rendering-cache))
    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))
  :doc "projects current history first and converges to synchronous output"
  (niwa-batch-test--with-history
    (goto-char (point-max))
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (niwa-batch-test--start-projection view-buf)
      (should niwa-view-render--batch)
      (should (string-search "Response 5" (buffer-string)))
      (should (string-search "Loading conversation turn" (buffer-string)))
      (cl-loop repeat 30 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max))))))
  :doc "new draft edits and selection survive every callback"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (niwa-view-composer-insert "\nnew typing")
    (let* ((composer (niwa-view-composer-buffer))
           (offset (with-current-buffer composer
                     (set-mark (- (point) 6)) (setq mark-active t)
                     (- (point) (niwa-view-composer-draft-start view-buf)))))
      (cl-loop repeat 30 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch)
        (should (equal "> draft\nsecond line\nnew typing" (niwa-view-composer-draft-text nil t)))
        (with-current-buffer composer
          (should (= (- (point) (niwa-view-composer-draft-start view-buf)) offset))
          (should mark-active)
          (should (= (- (point) (mark)) 6))))))
  :doc "selected history and its window start are rendered before restoration"
  (niwa-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min)) (search-forward "Response 2 with target text")
      (set-mark (- (point) 11)) (setq mark-active t)
      (let ((start (line-beginning-position)))
        (set-window-start (selected-window) start t))
      (niwa-batch-test--start-projection view-buf)
      (cl-loop repeat 30 while niwa-view-render--batch do
        (should (equal "target text" (buffer-substring-no-properties (region-beginning) (region-end))))
        (should (save-excursion (goto-char (window-start)) (looking-at "Response 2")))
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should (equal "target text" (buffer-substring-no-properties (region-beginning) (region-end))))))
  :doc "a reader's pinned prompt survives before deferred turns are rendered"
  (niwa-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min))
      (search-forward "Response 2 with target text")
      (set-window-start nil (line-beginning-position) t)
      (should (equal "Prompt 2" (cdr (niwa-view--pinned-prompt
                                      (selected-window)))))
      (niwa-batch-test--start-projection view-buf)
      (should niwa-view-render--batch)
      (should (equal "Prompt 2" (cdr (niwa-view--pinned-prompt
                                      (selected-window)))))
      (should (string-search "Prompt 2" (niwa-view--sticky-prompt-line)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (cl-loop repeat 30 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch)
        (should (equal "Prompt 2" (cdr (niwa-view--pinned-prompt
                                        (selected-window))))))))
  :doc "moving to another rendered response during a batch pins its own prompt"
  (niwa-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min))
      (search-forward "Response 0 with target text")
      (set-window-start nil (line-beginning-position) t)
      (search-forward "Response 4 with target text")
      (niwa-batch-test--start-projection-showing view-buf "Response 0")
      (should niwa-view-render--batch)
      (goto-char (point-min))
      (search-forward "Response 4 with target text")
      (set-window-start nil (line-beginning-position) t)
      (should (equal "Prompt 4" (cdr (niwa-view--pinned-prompt
                                      (selected-window)))))
      (should (string-search "Prompt 4" (niwa-view--sticky-prompt-line)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (cl-loop repeat 30 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch)
        (should (equal "Prompt 4" (cdr (niwa-view--pinned-prompt
                                        (selected-window)))))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))))
  :doc "scrolling onto a pending response still pins and reveals its own prompt"
  (niwa-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min))
      (search-forward "Response 0 with target text")
      (set-window-start nil (line-beginning-position) t)
      (niwa-batch-test--start-projection-showing view-buf "Response 0")
      (let ((entry (cl-find-if
                    (lambda (entry)
                      (and (eq 'assistant (plist-get (car entry) :role))
                           (with-current-buffer data-buf
                             (string-search
                              "Response 4"
                              (buffer-substring-no-properties
                               (plist-get (car entry) :start)
                               (plist-get (car entry) :end))))))
                    (plist-get niwa-view-render--batch :pending))))
        (should entry)
        (set-window-start nil (nth 1 entry) t)
        (should (equal "Prompt 4" (cdr (niwa-view--pinned-prompt
                                        (selected-window)))))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (niwa-view--jump-to-pinned-prompt
         (car (niwa-view--pinned-prompt (selected-window))))
        (should (looking-at "You"))
        (should (save-excursion (forward-line 1) (looking-at "Prompt 4")))
        (should niwa-view-render--batch)
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))))
  :doc "both windows keep their own pinned prompt during deferred rendering"
  (niwa-batch-test--with-history
    (save-window-excursion
      (let ((left (selected-window))
            (right (split-window-right)))
        (set-window-buffer left view-buf)
        (set-window-buffer right view-buf)
        (goto-char (point-min))
        (search-forward "Response 1 with target text")
        (set-window-start left (line-beginning-position) t)
        (search-forward "Response 4 with target text")
        (set-window-start right (line-beginning-position) t)
        (niwa-batch-test--start-projection view-buf)
        (should (equal "Prompt 1" (cdr (niwa-view--pinned-prompt left))))
        (should (equal "Prompt 4" (cdr (niwa-view--pinned-prompt right))))
        (should (string-search "Prompt 1"
                               (with-selected-window left
                                 (niwa-view--sticky-prompt-line))))
        (should (string-search "Prompt 4"
                               (with-selected-window right
                                 (niwa-view--sticky-prompt-line)))))))
  :doc "in-flight history uses the existing synchronous reconciliation"
  (niwa-batch-test--with-history
    (niwa-view-stream-begin-turn
     (niwa-view--history-insertion-marker)
     (with-current-buffer data-buf (copy-marker (point-max))))
    (niwa-view-render-batched-full)
    (should-not niwa-view-render--batch)
    (should-not (string-search "Loading conversation turn" (buffer-string)))
    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))

(niwa-deftest niwa-view-render--plan-fontify-step/disclosure ()
  ,test
  (test)
  :doc "tool preparation records a mutation guard without capturing the view"
  (with-temp-buffer
    (insert "old view")
    (let ((job (list :turns nil :fontify nil :fontify-turns nil
                     :fontify-segments nil :tools-turns nil :tool-state-tick nil :tool-states nil
                     :prepare-stage 'fontify)))
      (cl-letf (((symbol-function 'niwa-view-disclosure-capture-state)
                 (lambda (&rest _) (ert-fail "Unexpected preparation capture"))))
        (niwa-view-render--plan-fontify-step job))
      (should (eq 'tools (plist-get job :prepare-stage)))
      (should (eql (buffer-modified-tick) (plist-get job :tool-state-tick)))
      (put-text-property 1 2 'niwa-view-collapsed t)
      (should-not (eql (buffer-modified-tick) (plist-get job :tool-state-tick)))))
  :doc "only transcript mutations invalidate prepared tools; newer draft edits survive"
  (dolist (change-view '(nil t))
    (niwa-batch-test--with-history
      (niwa-view-render-batched-full)
      (cl-loop repeat 100
               until (eq 'finish (plist-get niwa-view-render--batch :prepare-stage))
               do (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (let* ((job niwa-view-render--batch)
             (cache (plist-get job :tool-cache)))
        (puthash 'sentinel t cache)
        (niwa-view-composer-insert " newer")
        (when change-view
          (with-silent-modifications
            (put-text-property (point-min) (1+ (point-min)) 'niwa-view-collapsed t)))
        (niwa-view-render--batch-step view-buf job)
        (should (eq (not change-view) (gethash 'sentinel cache)))
        (should (equal "> draft\nsecond line newer" (niwa-view-composer-draft-text nil t)))
        (cl-loop repeat 30 while niwa-view-render--batch do
                 (niwa-view-render--batch-step view-buf niwa-view-render--batch))))))

(niwa-deftest niwa-view-render-batched-full/other-writers ()
  ,test
  (test)
  :doc "queued status and interaction updates leave unfinished history batched"
  (niwa-batch-test--with-history
    (let ((session (niwa-session--create :name "batch chrome")))
      (setq-local niwa--session session)
      (with-current-buffer data-buf (setq-local niwa--session session))
      (niwa-batch-test--start-projection view-buf)
      (let ((batch niwa-view-render--batch))
        (niwa-view-render-mutate
         'full-batch
         (lambda ()
           (setf (niwa-session-recovery-issues session)
                 '((:message "Visible batched status" :blocking t)))
           (niwa-view--render-status data-buf)
           (niwa-view--interaction-register
            '(:id batched-request :kind request :body "Visible batched interaction"))))
        (should (eq batch niwa-view-render--batch))
        (should (string-search "Loading conversation turn" (buffer-string)))
        (should (string-search "Visible batched status" (buffer-string)))
        (should (string-search "Visible batched interaction" (buffer-string)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (with-current-buffer (niwa-view-composer-buffer) (point)))))))

  :doc "a synchronous full projection retires the old batch"
  (niwa-batch-test--with-history
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (niwa-batch-test--start-projection view-buf)
      (let ((old niwa-view-render--batch))
        (niwa-view--full-rerender)
        (should-not niwa-view-render--batch)
        (niwa-view-render--batch-step view-buf old)
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))
  :doc "other writers see a complete projection and retain newer draft edits"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (niwa-view-composer-insert " new edit")
    (niwa-view-render-mutate
     'test-reader
     (lambda ()
       (should-not niwa-view-render--batch)
       (should-not (string-search "Loading conversation turn" (buffer-string)))
       (should (string-search "Response 0" (buffer-string)))))
    (should (equal "> draft\nsecond line new edit" (niwa-view-composer-draft-text nil t)))))

(niwa-deftest niwa-view--flush-scheduled-render/batched ()
  ,test
  (test)
  :doc "the normal scheduler starts a batch and explicit immediate work finishes"
  (niwa-batch-test--with-history
    (let ((niwa-view-rerender-debounce 60))
      (niwa-view-rerender view-buf)
      (cancel-timer niwa-view--render-timer)
      (niwa-view--flush-scheduled-render view-buf)
      (should niwa-view-render--batch)
      (niwa-view--schedule-render 'full data-buf 0)
      (should-not niwa-view-render--batch)
      (should-not (string-search "Loading conversation turn" (buffer-string)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))))

(niwa-deftest niwa-view-render--priority-turns ()
  (niwa-batch-test--with-history
    (goto-char (point-min)) (search-forward "Response 2")
    (let* ((plan (niwa-view--full-rerender-plan data-buf data-buf view-buf nil))
           (turns (niwa-view-render--priority-turns (plist-get plan :turns))))
      (should (= (length turns) 1))
      (should (eq (plist-get (car turns) :role) 'assistant))
      (should (with-current-buffer data-buf
                (string-search "Response 2" (buffer-substring-no-properties
                                             (plist-get (car turns) :start)
                                             (plist-get (car turns) :end))))))))

(niwa-deftest niwa-view--group-into-turns/slices ()
  ,test
  (test)
  :doc "a slice boundary preserves lookahead and assistant grouping"
  (niwa-view-test--with-buffers
    (dolist (entry '(("Prompt\n" . nil)
                     ("First response\n" . response)
                     ("\n" . nil)
                     ("Second response\n" . response)
                     ("Next prompt\n" . nil)
                     ("Final response\n" . response)))
      (niwa-view-test--insert-data data-buf (car entry) (cdr entry)))
    (let* ((segments (with-current-buffer data-buf
                       (niwa-transcript-segments (point-min) (point-max))))
           (expected (niwa-view--group-into-turns segments data-buf))
           (state nil)
           (rest segments))
      (while (cdr rest)
        (setq state (niwa-view--group-into-turns
                     (list (car rest)) data-buf state (cadr rest))
              rest (cdr rest)))
      (should (equal expected (niwa-view--group-into-turns rest data-buf state))))))

(niwa-deftest niwa-view-render-batched-full/preparation ()
  ,test
  (test)
  :doc "a scheduled rebuild retains the old display until canonical preparation finishes"
  (niwa-batch-test--with-history
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (niwa-view-render-batched-full)
      (should niwa-view-render--batch)
      (should (eq 'prepare (plist-get niwa-view-render--batch :phase)))
      (should (equal before (buffer-substring-no-properties (point-min) (point-max))))
      (cl-loop repeat 100 while (and niwa-view-render--batch
                                     (eq 'prepare (plist-get niwa-view-render--batch :phase)))
               do (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should (eq 'project (plist-get niwa-view-render--batch :phase)))
      (cl-loop repeat 100 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should-not niwa-view-render--batch)
      (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))))

(niwa-deftest niwa-view-render-batched-full/fontify-preparation ()
  ,test
  (test)
  :doc "whole-response Markdown work yields before publication and releases its job"
  (niwa-batch-test--with-history
    ;; Fixture rendering may already have populated the real Markdown cache.
    (clrhash niwa-view--response-fontify-cache)
    (setq niwa-view--response-cache-entries 0)
    (let ((before (buffer-substring-no-properties (point-min) (point-max)))
          (started 0) (steps 0) (cancelled 0))
      (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
                 (lambda () 'markdown-ts-mode))
                ((symbol-function 'niwa-view--markdown-fontify-job-start)
                 (lambda (text) (cl-incf started) (cons text 0)))
                ((symbol-function 'niwa-view--markdown-fontify-job-step)
                 (lambda (job _size)
                   (cl-incf steps)
                   (setcdr job (1+ (cdr job)))
                   (>= (cdr job) 2)))
                ((symbol-function 'niwa-view--markdown-fontify-job-result)
                 (lambda (job)
                   (propertize (car job) 'font-lock-face 'bold)))
                ((symbol-function 'niwa-view--markdown-fontify-job-cancel)
                 (lambda (_job) (cl-incf cancelled))))
        (niwa-view-render-batched-full)
        (cl-loop repeat 100 while (and niwa-view-render--batch
                                       (eq 'prepare (plist-get niwa-view-render--batch :phase)))
                 do (progn
                      (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))
                      (niwa-view-render--batch-step view-buf niwa-view-render--batch)))
        (should (eq 'project (plist-get niwa-view-render--batch :phase)))
        (should (> started 0))
        (should (>= steps (* 2 started)))
        (should (= started cancelled))
        (should (> (hash-table-count (plist-get niwa-view-render--batch :fontified)) 0))
        (cl-loop repeat 100 while niwa-view-render--batch do
          (niwa-view-render--batch-step view-buf niwa-view-render--batch))
        (should (equal before (buffer-substring-no-properties (point-min) (point-max))))))
  :doc "a source edit cancels an unfinished private Markdown job"
  (niwa-batch-test--with-history
    (clrhash niwa-view--response-fontify-cache)
    (setq niwa-view--response-cache-entries 0)
    (let (cancelled)
      (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
                 (lambda () 'markdown-ts-mode))
                ((symbol-function 'niwa-view--markdown-fontify-job-start)
                 (lambda (text) (list text)))
                ((symbol-function 'niwa-view--markdown-fontify-job-step)
                 (lambda (&rest _) nil))
                ((symbol-function 'niwa-view--markdown-fontify-job-cancel)
                 (lambda (job) (setq cancelled job))))
        (niwa-view-render-batched-full)
        (cl-loop repeat 100 until (plist-get niwa-view-render--batch :fontify)
                 do (niwa-view-render--batch-step view-buf niwa-view-render--batch))
        (should (plist-get niwa-view-render--batch :fontify))
        (should (zerop (hash-table-count niwa-view--response-fontify-cache)))
        (with-current-buffer data-buf
          (goto-char (point-max)) (insert "edited"))
        (niwa-view-render--batch-step view-buf niwa-view-render--batch)
        (should cancelled)
        (should (zerop (hash-table-count niwa-view--response-fontify-cache)))
        (should (eq 'prepare (plist-get niwa-view-render--batch :phase)))))))

(niwa-deftest niwa-view-render-batched-full/markdown-snapshot ()
  ,test
  (test)
  :doc "sync and batch probe grammars once and a new projection sees installation"
  (dolist (batched '(nil t))
    (niwa-batch-test--with-history
      ;; Each availability scenario starts cold, regardless of host grammars.
      (clrhash niwa-view--response-fontify-cache)
      (setq niwa-view--response-cache-entries 0)
      (let ((available nil) (probes 0) (starts 0)
            (start (symbol-function 'niwa-view--markdown-fontify-job-start)))
        (cl-letf (((symbol-function 'treesit-language-available-p)
                   (lambda (_language &rest _) (cl-incf probes) available))
                  ((symbol-function 'niwa-view--fontify-as)
                   (lambda (text _mode) text))
                  ((symbol-function 'niwa-view--markdown-fontify-setup)
                   (lambda (buffer _mode)
                     (with-current-buffer buffer (fundamental-mode))))
                  ((symbol-function 'niwa-view--markdown-fontify-job-start)
                   (lambda (text) (cl-incf starts) (funcall start text))))
          (dotimes (round 2)
            (setq probes 0)
            (if batched
                (progn
                  (niwa-view-render-batched-full)
                  ;; Availability changes between preparation callbacks must not
                  ;; change this job's key or publication mode mid-projection.
                  (setq available t)
                  (cl-loop repeat 150 while niwa-view-render--batch do
                           (niwa-view-render--batch-step view-buf niwa-view-render--batch))
                  (should-not niwa-view-render--batch))
              (niwa-view--full-rerender))
            (should (= probes (if (zerop round) 1 2)))
            (when (and batched (zerop round)) (should (zerop starts)))
            (setq available t))
          (when batched (should (= starts 6)))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= (+ (niwa-view-composer-draft-start) 4)
                     (with-current-buffer (niwa-view-composer-buffer) (point)))))))))

(niwa-deftest niwa-view-render-batched-full/fork-index ()
  ,test
  (test)
  :doc "sync and deferred batch headers share one restricted fork-point scan"
  (dolist (batched '(nil t))
    (niwa-batch-test--with-history
      (let ((session (niwa-session--create :session-id "source" :save-path "/tmp/source/"))
            (calls 0)
            (discover (symbol-function 'niwa-session-artifacts-fork-point-spans)))
        (with-current-buffer data-buf
          (setq-local niwa--session session)
          (goto-char (point-max))
          (insert (niwa--format-hook-audit-record
                   '(:type fork-point :fork-point-id "last-response"))))
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-session-artifacts-fork-point-spans)
                   (lambda (data) (cl-incf calls) (funcall discover data)))
                  ((symbol-function 'niwa-session-persistence-conversation-variants)
                   (lambda (_session id &optional _sessions)
                     (should (equal "last-response" id))
                     '((:variant-origin source :summary (:session-id "source"))
                       (:variant-origin conversation :summary (:session-id "child"))))))
          (if batched
              (progn
                (niwa-view-render-batched-full)
                (cl-loop repeat 150 while niwa-view-render--batch do
                         (niwa-view-render--batch-step view-buf niwa-view-render--batch))
                (should-not niwa-view-render--batch))
            (niwa-view--full-rerender))
          (should (= calls 1))
          (should (string-search "[⇆ Source · 2 variants]" (buffer-string)))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= (+ (niwa-view-composer-draft-start) 4)
                     (with-current-buffer (niwa-view-composer-buffer) (point)))))))))

(niwa-deftest niwa-view-render-batched-full/response-promotion ()
  ,test
  (test)
  :doc "cold batch-only Markdown output survives the job and skips the next preparation"
  (dolist (capacity '(256 2))
    (niwa-batch-test--with-history
      (let ((starts 0) (niwa-view-render-cache-max-entries capacity))
        (clrhash niwa-view--response-fontify-cache)
        (setq niwa-view--response-cache-entries 0)
        (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
                   (lambda () 'markdown-ts-mode))
                  ((symbol-function 'niwa-view--markdown-fontify-job-start)
                   (lambda (text) (cl-incf starts) (list text)))
                  ((symbol-function 'niwa-view--markdown-fontify-job-step)
                   (lambda (&rest _) t))
                  ((symbol-function 'niwa-view--markdown-fontify-job-result)
                   (lambda (job) (propertize (car job) 'font-lock-face 'bold)))
                  ((symbol-function 'niwa-view--markdown-fontify-job-cancel) #'ignore))
          (dotimes (_ (if (= capacity 256) 2 1))
            (niwa-view-render-batched-full)
            (cl-loop repeat 150 while niwa-view-render--batch do
                     (niwa-view-render--batch-step view-buf niwa-view-render--batch)
                     (should (<= (hash-table-count niwa-view--response-fontify-cache) capacity))
                     (should (<= niwa-view--response-cache-entries capacity)))
            (should-not niwa-view-render--batch)
            (should (= starts 6))
            (should (= (hash-table-count niwa-view--response-fontify-cache)
                       niwa-view--response-cache-entries))
            (save-excursion
              (goto-char (point-min)) (search-forward "Response 5")
              (should (eq 'bold (get-text-property (1- (point)) 'font-lock-face))))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
            (should (= (+ (niwa-view-composer-draft-start) 4)
                       (with-current-buffer (niwa-view-composer-buffer) (point))))))))))

(niwa-deftest niwa-view-render--plan-tool-step ()
  ,test
  (test)
  :doc "preparation and publication request the same merged tool-audit tuple"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data
     data-buf "(:name \"Unknown\" :args nil)\nFirst result.\n" '(tool . "one"))
    (niwa-view-test--insert-data
     data-buf (niwa--format-hook-audit-record
               '(:type tool-hook :phase post-tool :message "Audit"))
     'niwa-hook-audit)
    (niwa-view-test--insert-data data-buf "Remaining result.\n" '(tool . "one"))
    (niwa-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((original (symbol-function 'niwa-view--tool-segment-entry))
            prepared published (phase 'prepare))
        (cl-letf (((symbol-function 'niwa-view--tool-segment-entry)
                   (lambda (segment data)
                     (if (eq phase 'prepare) (push segment prepared) (push segment published))
                     (funcall original segment data))))
          (niwa-view-render-batched-full)
          (cl-loop repeat 150
                   until (eq 'finish (plist-get niwa-view-render--batch :prepare-stage))
                   do (niwa-view-render--batch-step view-buf niwa-view-render--batch))
          (setq phase 'publish)
          (cl-loop repeat 150 while niwa-view-render--batch do
                   (niwa-view-render--batch-step view-buf niwa-view-render--batch))
          (should (= (length prepared) 1))
          (should published)
          (should (cl-every (lambda (segment) (equal (car prepared) segment)) published))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))))
  :doc "an interrupted entry resumes without repeating previously committed entries"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf (insert (make-string 30 ?x)))
    (let* ((segments '((tool 1 10) (tool 10 20) (tool 20 31)))
           (job (list :data data-buf :view view-buf :plan '(:start 1 :end 31)
                      :tools-turns (list (list :segments segments)) :tools-segments nil
                      :tool-cache (make-hash-table :test 'equal) :prepare-stage 'tools))
           calls)
      (cl-letf (((symbol-function 'niwa-view--tool-segment-entry)
                 (lambda (segment _data)
                   (push segment calls)
                   (when (= (cadr segment) 10) (throw 'interrupt nil)))))
        (catch 'interrupt (niwa-view-render--plan-tool-step job)))
      (should (equal (cdr segments) (plist-get job :tools-segments)))
      (setq calls nil)
      (cl-letf (((symbol-function 'niwa-view--tool-segment-entry)
                 (lambda (segment _data) (push segment calls))))
        (niwa-view-render--plan-tool-step job))
      (should (equal (cdr segments) (nreverse calls)))
      (should (eq 'finish (plist-get job :prepare-stage))))))

(niwa-deftest niwa-view-render-batched-full/newly-remembered-fold ()
  ,test
  (test)
  :doc "publication capture cannot mark a prepared summary-only row as fully expanded"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data
     data-buf
     (concat "(:name \"Read\" :args (:file_path \"/tmp/remembered.el\"))\n\n"
             (make-string 800 ?x) "\nUntruncated last body line.\n")
     '(tool . "remembered"))
    (niwa-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min)) (search-forward "Read:")
      (niwa-view-toggle-section)
      (should (string-search "Untruncated last body line." (buffer-string)))
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      ;; The old view still displays an expanded row.  Only publication's fresh
      ;; capture will remember it, after cold tool preparation chose a summary.
      (niwa-view-disclosure-reset-state)
      (niwa-view-render-batched-full)
      (cl-loop repeat 150 while niwa-view-render--batch do
               (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should-not niwa-view-render--batch)
      (should (string-search "Untruncated last body line." (buffer-string)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should (= (+ (niwa-view-composer-draft-start) 4)
                 (with-current-buffer (niwa-view-composer-buffer) (point)))))))

(niwa-deftest niwa-view-render-batched-full/tool-preparation ()
  ,test
  (test)
  :doc "scheduled projection prepares source-backed tool entries before rendering"
  (niwa-batch-test--with-history
    (niwa-view-test--insert-data
     data-buf "(:name \"Unknown\" :args nil)\nTool result.\n" '(tool . "one"))
    (niwa-view--full-rerender)
    (let ((before (buffer-substring-no-properties (point-min) (point-max)))
          (calls 0)
          (entry (symbol-function 'niwa-view--tool-segment-entry)))
      (cl-letf (((symbol-function 'niwa-view--tool-segment-entry)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply entry args))))
        (niwa-batch-test--start-projection view-buf)
        (should (> calls 0))
        (should (> (hash-table-count
                    (plist-get niwa-view-render--batch :tool-cache)) 0))
        (cl-loop repeat 100 while niwa-view-render--batch do
          (niwa-view-render--batch-step view-buf niwa-view-render--batch))
        (should (equal before (buffer-substring-no-properties
                               (point-min) (point-max))))))))

(niwa-deftest niwa-view-render--batch-turn ()
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let* ((job niwa-view-render--batch)
           (entry (car (plist-get job :pending))))
      (niwa-view-render--batch-turn job entry)
      (should (< (nth 1 entry) (nth 2 entry)))
      (should-not (string-search "Loading conversation turn"
                                (buffer-substring (nth 1 entry) (nth 2 entry)))))))

(niwa-deftest niwa-view-render--batch-step ()
  ,test
  (test)
  :doc "one callback consumes exactly one pending turn"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let* ((job niwa-view-render--batch)
           (count (length (plist-get job :pending))))
      (niwa-view-render--batch-step view-buf job)
      (should (eq job niwa-view-render--batch))
      (should (= (length (plist-get job :pending)) (1- count)))))
  :doc "obsolete callbacks cannot consume a newer job"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let ((old niwa-view-render--batch))
      (niwa-batch-test--start-projection view-buf)
      (let ((current niwa-view-render--batch)
            (before (buffer-string)))
        (niwa-view-render--batch-step view-buf old)
        (should (eq current niwa-view-render--batch))
        (should (equal before (buffer-string))))))
  :doc "source changes retire the plan and eventually project current text"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let ((old niwa-view-render--batch))
      (with-current-buffer data-buf
        (goto-char (point-max)) (insert (propertize "Changed source.\n" 'gptel 'response)))
      (niwa-view-render--batch-step view-buf old)
      (should-not (eq old niwa-view-render--batch))
      (cl-loop repeat 30 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should (string-search "Changed source." (buffer-string)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))
  :doc "failed batch rolls back its partial insertion and recovers canonically"
  (niwa-batch-test--with-history
    (let ((expected (buffer-substring-no-properties (point-min) (point-max)))
          (render (symbol-function 'niwa-view--render-turn)) failed)
      (niwa-batch-test--start-projection view-buf)
      (cl-letf (((symbol-function 'niwa-view--render-turn)
                 (lambda (&rest args)
                   (if failed (apply render args)
                     (setq failed t)
                     (insert "Partial failed turn")
                     (error "Injected batch failure")))))
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should-not niwa-view-render--batch)
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))

(niwa-deftest niwa-view-render-cancel-batch ()
  ,test
  (test)
  :doc "cancellation releases timer and pending source markers"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let* ((job niwa-view-render--batch)
           (timer (plist-get job :timer))
           (start (nth 1 (car (plist-get job :pending)))))
      (should (memq timer timer-list))
      (niwa-view-render-cancel-batch)
      (should-not niwa-view-render--batch)
      (should-not (marker-buffer start))
      (should-not (memq timer timer-list))))
  :doc "view death cancels pending callbacks"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let ((timer (plist-get niwa-view-render--batch :timer)))
      (kill-buffer view-buf)
      (should-not (memq timer timer-list)))))

(niwa-deftest niwa-view-render-resume-batch ()
  ,test
  (test)
  :doc "focus loss pauses callbacks; focus recovery arms exactly one timer"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let* ((job niwa-view-render--batch)
           (pending (length (plist-get job :pending))))
      (cl-letf (((symbol-function 'niwa-view--unattended-p) (lambda (&rest _) t)))
        (niwa-view-render--batch-step view-buf job)
        (niwa-view-render-resume-batch)
        (should-not (plist-get job :timer))
        (should (= pending (length (plist-get job :pending)))))
      (niwa-view-composer-insert " while paused")
      (niwa-view--resume-render-if-attended view-buf)
      (let ((timer (plist-get job :timer)))
        (should (memq timer timer-list))
        (niwa-view-render-resume-batch)
        (should (eq timer (plist-get job :timer))))
      (cl-loop repeat 30 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should-not niwa-view-render--batch)
      (should (equal "> draft\nsecond line while paused" (niwa-view-composer-draft-text nil t))))
  :doc "a paused job releases its collection hold until it resumes"
  (niwa-batch-test--with-history
    (let ((holds (make-hash-table :test #'eq)))
      (cl-letf (((symbol-function 'niwa--gc-hold)
                 (lambda (key predicate) (puthash key predicate holds)))
                ((symbol-function 'niwa--gc-release)
                 (lambda (key) (remhash key holds))))
        (niwa-batch-test--start-projection view-buf)
        (let ((job niwa-view-render--batch))
          (should (gethash job holds))
          (cl-letf (((symbol-function 'niwa-view--unattended-p) (lambda (&rest _) t)))
            (niwa-view-render--batch-step view-buf job))
          (should-not (gethash job holds))
          (niwa-view--resume-render-if-attended view-buf)
          (should (gethash job holds))
          (should (funcall (gethash job holds)))
          (niwa-view-render-cancel-batch))))))
  :doc "transport contention postpones a turn without consuming its source"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let* ((job niwa-view-render--batch)
           (pending (length (plist-get job :pending))))
      (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&rest _) t)))
        (niwa-view-render--batch-step view-buf job)
        (should (= pending (length (plist-get job :pending))))
        (should (memq (plist-get job :timer) timer-list)))
      (niwa-view-render--batch-step view-buf job)
      (should (= (1- pending) (length (plist-get job :pending)))))))

(niwa-deftest niwa-view-render--start-batch ()
  ,test
  (test)
  :doc "a nested request installs its plan only after the owning writer exits"
  (niwa-batch-test--with-history
    (niwa-view-render-mutate
     'outer
     (lambda ()
       (niwa-view-render-batched-full)
       (should-not niwa-view-render--batch)
       (niwa-view-composer-insert " latest edit")))
    (should niwa-view-render--batch)
    (cl-loop repeat 30 while niwa-view-render--batch do
      (niwa-view-render--batch-step view-buf niwa-view-render--batch))
    (should-not niwa-view-render--batch)
    (should (equal "> draft\nsecond line latest edit" (niwa-view-composer-draft-text nil t))))
  :doc "defers collection during the job and a short grace after it"
  (niwa-batch-test--with-history
    (let (live-p)
      (cl-letf (((symbol-function 'niwa--gc-hold)
                 (lambda (_key predicate) (setq live-p predicate))))
        (niwa-view-render-batched-full))
      (should (funcall live-p))
      (let ((job niwa-view-render--batch))
        (niwa-view-render-cancel-batch)
        ;; The finished job keeps its hold through the grace period.
        (should (funcall live-p))
        (setf (plist-get job :settled-until) (1- (float-time)))
        (should-not (funcall live-p)))))
  :doc "a queued writer after batch installation sees complete history"
  (niwa-batch-test--with-history
    (niwa-view-render-mutate
     'outer
     (lambda ()
       (niwa-view-render-batched-full)
       (niwa-view-render-mutate
        'later
        (lambda ()
          (should-not niwa-view-render--batch)
          (should-not (string-search "Loading conversation turn" (buffer-string)))
          (should (string-search "Response 0" (buffer-string)))))))
    (should-not niwa-view-render--batch))
  :doc "mouse release of a held elapsed tick does not finish staged history"
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let* ((job niwa-view-render--batch)
           (pending (length (plist-get job :pending)))
           ticked)
      (let ((track-mouse 'drag-tracking))
        (niwa-view-render-mutate 'elapsed-tick (lambda () (setq ticked t)))
        (should-not ticked))
      (let ((track-mouse nil))
        (niwa-view-render--resume-after-mouse view-buf))
      (should ticked)
      (should (eq job niwa-view-render--batch))
      (should (= pending (length (plist-get job :pending))))))
  :doc "failure while installing priority turns releases work and restores the view"
  (niwa-batch-test--with-history
    (let ((before (buffer-substring-no-properties (point-min) (point-max))) job)
      (cl-letf (((symbol-function 'niwa-view-render--batch-turn)
                 (lambda (current _entry)
                   (setq job current)
                   (error "Injected initial failure"))))
        (niwa-view-render-batched-full)
        (cl-loop repeat 100 while niwa-view-render--batch do
          (niwa-view-render--batch-step view-buf niwa-view-render--batch)))
      (should job)
      (should-not niwa-view-render--batch)
      (dolist (entry (plist-get job :pending))
        (should-not (marker-buffer (nth 1 entry)))
        (should-not (marker-buffer (nth 2 entry))))
      (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))))

(niwa-deftest niwa-view-render-batched-full/disclosures ()
  ,test
  (test)
  :doc "expanded tool bodies and manually folded turns survive all callbacks"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data
     data-buf "(:name \"Unknown\" :args nil)\nBody retained on expansion.\n" '(tool . "one"))
    (dotimes (index 4)
      (niwa-view-test--insert-data data-buf (format "Prompt %d\n" index) nil)
      (niwa-view-test--insert-data data-buf (format "Response %d\n" index) 'response))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min)) (search-forward "Unknown")
      (niwa-view-toggle-section)
      (should (string-search "Body retained on expansion" (buffer-string)))
      (goto-char (point-min)) (search-forward "Response 1")
      (search-backward "Assistant")
      (niwa-view-toggle-section)
      (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
        (goto-char (point-max))
        (niwa-batch-test--start-projection view-buf)
        (cl-loop repeat 30 while niwa-view-render--batch do
          (niwa-view-render--batch-step view-buf niwa-view-render--batch))
        (should-not niwa-view-render--batch)
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max))))))))

(niwa-deftest niwa-view-render--batch-step/reentry
  (:doc "a timer queued inside a writer consumes its entry only after ownership transfers")
  (niwa-batch-test--with-history
    (niwa-batch-test--start-projection view-buf)
    (let* ((job niwa-view-render--batch)
           (count (length (plist-get job :pending))))
      (niwa-view-render-mutate
       'full-batch
       (lambda ()
         (niwa-view-render--batch-step view-buf job)
         (should (= count (length (plist-get job :pending))))))
      (should (= (1- count) (length (plist-get job :pending)))))))

(niwa-deftest niwa-view-render--priority-turns/visible-queue
  (:doc "renders prompts, reader anchors and every visible response at publication")
  (niwa-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min)) (search-forward "Response 1")
      (set-window-start (selected-window) (line-beginning-position) t)
      (let ((end (save-excursion (search-forward "Response 4") (point))))
        (cl-letf (((symbol-function 'window-end) (lambda (&rest _) end)))
          (let* ((plan (niwa-view--full-rerender-plan data-buf data-buf view-buf nil))
                 (turns (plist-get plan :turns)))
            (should (> (length (niwa-view-render--priority-turns turns))
                       (length (niwa-view-render--priority-turns turns t)))))
          (niwa-view-render-batched-full)))
      (cl-loop repeat 100 while (and niwa-view-render--batch
                                     (eq 'prepare (plist-get niwa-view-render--batch :phase)))
               do (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should niwa-view-render--batch)
      (dolist (visible '("Response 1" "Response 2" "Response 3" "Response 4"))
        (should (string-search visible (buffer-string))))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (dolist (entry (plist-get niwa-view-render--batch :pending))
        (should-not (with-current-buffer data-buf
                      (string-match-p
                       "Response [1-4]"
                       (buffer-substring-no-properties
                        (plist-get (car entry) :start)
                        (plist-get (car entry) :end)))))))))

(defun niwa-batch-test--turn-ids ()
  "Return rendered turn ids before the prompt boundary, in buffer order."
  (let ((pos (point-min)) (limit (niwa-view-transcript-tail)) ids)
    (while (< pos limit)
      (when-let* ((id (get-text-property pos 'niwa-view-turn-id)))
        (unless (eq id (car ids)) (push id ids)))
      (setq pos (or (next-single-property-change pos 'niwa-view-turn-id nil limit)
                    limit)))
    (nreverse ids)))

(defun niwa-batch-test--append-fork-record (data-buf)
  "Append a settled fork-point record to DATA-BUF as a save does."
  (with-current-buffer data-buf
    (save-excursion
      (goto-char (point-max))
      (insert (niwa--format-hook-audit-record
               (list :type 'fork-point :fork-point-id "fp-test" :segment 1
                     :turn 6 :file-turn 6 :cum-turn 6 :captured-file-turn 6))))))

(niwa-deftest niwa-view-render-refresh-latest-turn ()
  ,test
  (test)
  :doc "re-renders only the newest turn so its source covers appended metadata"
  (niwa-batch-test--with-history
    (let ((text (buffer-substring-no-properties (point-min) (point-max)))
          (ids (niwa-batch-test--turn-ids))
          full)
      (niwa-batch-test--append-fork-record data-buf)
      (cl-letf (((symbol-function 'niwa-view-rerender)
                 (lambda (&rest _) (setq full t))))
        (niwa-view-render-refresh-latest-turn data-buf))
      (should-not full)
      (should-not niwa-view-render--batch)
      (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
      ;; Earlier turns keep their rendered text; the newest keeps its identity.
      (should (equal ids (niwa-batch-test--turn-ids)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should (= (+ (niwa-view-composer-draft-start) 4)
                 (with-current-buffer (niwa-view-composer-buffer) (point))))
      (goto-char (point-min))
      (search-forward "Response 5")
      (let* ((turn-start (car (niwa-view-disclosure-section-bounds
                               'niwa-view-turn-id)))
             (source (get-text-property turn-start 'niwa-view-source)))
        (should (= (cdr source) (with-current-buffer data-buf (point-max)))))))
  :doc "schedules a full projection when the tail is no longer the newest turn"
  (niwa-batch-test--with-history
    (let (full)
      (niwa-view-test--insert-data data-buf "Prompt 6\n" nil)
      (niwa-view-test--insert-data data-buf "Response 6.\n" 'response)
      (cl-letf (((symbol-function 'niwa-view-rerender)
                 (lambda (&rest _) (setq full t))))
        (niwa-view-render-refresh-latest-turn data-buf))
      (should full)
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))
  :doc "leaves an in-progress history job to revalidate the changed source"
  (niwa-batch-test--with-history
    (let (full)
      (niwa-batch-test--start-projection view-buf)
      (niwa-batch-test--append-fork-record data-buf)
      (cl-letf (((symbol-function 'niwa-view-rerender)
                 (lambda (&rest _) (setq full t))))
        (niwa-view-render-refresh-latest-turn data-buf))
      (should-not full)
      (should niwa-view-render--batch)
      (cl-loop repeat 100 while niwa-view-render--batch do
        (niwa-view-render--batch-step view-buf niwa-view-render--batch))
      (should-not (string-search "Loading conversation turn" (buffer-string))))))

(niwa-deftest niwa-view-render/batch-rail-isolation ()
  ,test
  (test)
  :doc "filling transcript placeholders leaves the idle rail and draft unchanged"
  (niwa-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (let ((expected (buffer-substring-no-properties (point-min) (point-max)))
            (rail (niwa-view-test--rail-text)))
        (goto-char (point-min))
        (search-forward "Response 0 with target text")
        (set-window-start nil (line-beginning-position) t)
        (with-current-buffer (niwa-view-composer-buffer)
          (goto-char (+ (niwa-view-composer-draft-start view-buf) 4)))
        ;; The newest reply is offscreen, so publication ends in a placeholder.
        (niwa-batch-test--start-projection-showing view-buf "Response 0")
        (should (string-search "Loading conversation turn"
                               (buffer-substring-no-properties
                                (- (niwa-view-transcript-tail) 80)
                                (niwa-view-transcript-tail))))
        (cl-loop repeat 30 while niwa-view-render--batch do
          (niwa-view-render--batch-step view-buf niwa-view-render--batch))
        (should (equal expected
                       (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal rail (niwa-view-test--rail-text)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (with-current-buffer (niwa-view-composer-buffer) (point))))))))

(provide 'test-niwa-view-render-batch)
;;; test-niwa-view-render-batch.el ends here
