;;; test-niwa-gptel-stream-bridge.el -- gptel stream bridge tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the private gptel streaming compatibility boundary independently
;; from live View progress and rendering.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'niwa-gptel-stream-bridge)
(require 'niwa-transport)
(require 'niwa-structs)

(defmacro niwa-gptel-stream-bridge-test--with-data-buffer (&rest body)
  "Create one niwa data buffer, then evaluate BODY."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *niwa-stream-bridge-data*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode)
             (setq-local niwa--session
                         (niwa-session--create :name "stream-bridge")))
           ,@body)
       (when (buffer-live-p data-buf)
         (kill-buffer data-buf)))))

;;
;;; Advice lifecycle

(niwa-deftest niwa-gptel-stream-bridge-install ()
  ,test
  (test)
  :doc "install enables advice and deferred installers honor that state"
  (let ((niwa-gptel-stream-bridge--gptel-stream-advice-installed nil)
        (calls 0))
    (cl-letf (((symbol-function 'niwa-gptel-stream-bridge--install-advice)
               (lambda () (cl-incf calls))))
      (niwa-gptel-stream-bridge-install)
      (should niwa-gptel-stream-bridge--gptel-stream-advice-installed)
      (should (> calls 0))
      (niwa-gptel-stream-bridge-uninstall)
      (should-not niwa-gptel-stream-bridge--gptel-stream-advice-installed))))

(niwa-deftest niwa-gptel-stream-bridge--insert-tracked
  (:vars ((installed
           (advice-member-p
            #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
            'gptel-curl--stream-insert-response)))
   :before-each
   (advice-remove
    'gptel-curl--stream-insert-response
    #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice)
   :after-each
   (when installed
     (advice-add
      'gptel-curl--stream-insert-response :around
      #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice)))
  ,test
  (test)
  :doc "real gptel insert is visible to read-only observers as a proven tail edit"
  (with-temp-buffer
    (insert (make-string 2048 ?x) "\nAnswer")
    (let* ((start (point-max))
           (source (niwa-transcript-source-create 1))
           (info (list :position (copy-marker 2000)
                       :tracking-marker (copy-marker start)
                       :transformer #'identity))
           observed
           (gptel-post-stream-hook
            (list (lambda ()
                    (setq observed (niwa-transcript-source-change source)))))
           (inhibit-modification-hooks t))
      (niwa-transcript-source-accept source)
      (niwa-gptel-stream-bridge--insert-tracked
       #'gptel-curl--stream-insert-response " more" info nil)
      (should (= (1- start) observed))
      (should (= observed (niwa-transcript-source-change source)))
      (should (eq #'identity (plist-get info :transformer)))
      (should (string-suffix-p "Answer more" (buffer-string)))))
  :doc "a point-only transformer cannot hide a relocated prefix insertion"
  (with-temp-buffer
    (insert "Original prefix\nAnswer")
    (let* ((source (niwa-transcript-source-create 1))
           (info (list :position (copy-marker (point-max))
                       :tracking-marker (copy-marker (point-max))
                       :transformer (lambda (text) (goto-char 1) text)))
           observed
           (gptel-post-stream-hook
            (list (lambda ()
                    (setq observed (niwa-transcript-source-change source)))))
           (inhibit-modification-hooks t))
      (niwa-transcript-source-accept source)
      (niwa-gptel-stream-bridge--insert-tracked
       #'gptel-curl--stream-insert-response "Relocated " info nil)
      (should (string-prefix-p "Relocated Original" (buffer-string)))
      (should (eql 0 observed))
      (should (eql 0 (niwa-transcript-source-change source)))))
  :doc "arbitrary transformer and hook edits fail closed, including their errors"
  (dolist (phase '(transformer hook))
    (dolist (fail '(nil t))
      (with-temp-buffer
        (insert "Original prefix\nAnswer")
        (let* ((source (niwa-transcript-source-create 1))
               (foreign (lambda (&optional text)
                          (save-excursion (goto-char 1) (insert "Changed"))
                          (when fail (error "Foreign failure"))
                          text))
               (info (list :position (copy-marker (point-max))
                           :tracking-marker (copy-marker (point-max))
                           :transformer (and (eq phase 'transformer) foreign)))
               (gptel-post-stream-hook (and (eq phase 'hook) (list foreign)))
               (inhibit-modification-hooks t))
          (niwa-transcript-source-accept source)
          (if fail
              (should-error
               (niwa-gptel-stream-bridge--insert-tracked
                #'gptel-curl--stream-insert-response " tail" info nil)
               :type 'error)
            (niwa-gptel-stream-bridge--insert-tracked
             #'gptel-curl--stream-insert-response " tail" info nil))
          (should (= 0 (niwa-transcript-source-change source)))
          (should-not (niwa-transcript-source-before source))
          (should (eq (and (eq phase 'transformer) foreign)
                      (plist-get info :transformer)))))))
  :doc "preserves a downstream transformer replacement and bypasses non-string responses"
  (with-temp-buffer
    (insert "Answer")
    (let* ((info (list :position (copy-marker (point-max))
                       :tracking-marker (copy-marker (point-max))
                       :transformer #'identity))
           (gptel-post-stream-hook
            (list (lambda () (plist-put info :transformer #'upcase))))
           (inhibit-modification-hooks t))
      (niwa-gptel-stream-bridge--insert-tracked
       #'gptel-curl--stream-insert-response " tail" info nil)
      (should (eq #'upcase (plist-get info :transformer)))
      (should (eq 'delegated
                  (niwa-gptel-stream-bridge--insert-tracked
                   (lambda (&rest _) 'delegated) '(tool-call) info nil))))))

(niwa-deftest niwa-gptel-stream-bridge--repair-gptel-stream-info ()
  ,test
  (test)
  :doc "reanchors detached position markers for niwa stream callbacks"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (with-current-buffer data-buf
     (insert "Prompt\n"))
   (let ((position (copy-marker 1 nil))
         (tracking (copy-marker 1 nil))
         (reasoning (copy-marker 1 nil)))
     (set-marker position nil)
     (set-marker tracking nil)
     (set-marker reasoning nil)
     (let ((info (list :buffer data-buf
                       :position position
                       :tracking-marker tracking
                       :reasoning-marker reasoning)))
       (should (eq (niwa-gptel-stream-bridge--repair-gptel-stream-info info) info))
       (let ((new-position (plist-get info :position)))
         (should (markerp new-position))
         (should (eq (marker-buffer new-position) data-buf))
         (should (= (marker-position new-position)
                    (with-current-buffer data-buf (point-max)))))
       (should-not (plist-get info :tracking-marker))
       (should-not (plist-get info :reasoning-marker)))))

  :doc "leaves unrelated gptel stream info untouched"
  (with-temp-buffer
    (let ((position (copy-marker (point) nil)))
      (set-marker position nil)
      (let ((info (list :buffer (current-buffer)
                        :position position)))
        (niwa-gptel-stream-bridge--repair-gptel-stream-info info)
        (should (eq (plist-get info :position) position))
        (should-not (marker-position position))))))

(niwa-deftest niwa-gptel-stream-bridge--gptel-stream-insert-response-advice ()
  ,test
  (test)

  :doc "repairs detached markers before delegating to gptel streaming"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let ((position (copy-marker 1 nil))
         (niwa-gptel-stream-bridge-insert-batch-delay nil))
     (set-marker position nil)
     (let ((info (list :buffer data-buf :position position)))
       (should
        (eq (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
             (lambda (_response callback-info &optional _raw)
               (let ((marker (plist-get callback-info :position)))
                 (and (markerp marker)
                      (eq (marker-buffer marker) data-buf)
                      (marker-position marker)
                      'delegated)))
             "chunk"
             info)
            'delegated)))))

  :doc "niwa stream insertion suppresses after-change hooks but keeps post-stream hook"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let ((after-change-ran nil)
         (post-stream-ran nil)
         (niwa-gptel-stream-bridge-insert-batch-delay nil)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (with-current-buffer data-buf
       (setq-local after-change-functions nil)
       (setq-local gptel-post-stream-hook nil)
       (add-hook 'after-change-functions
                 (lambda (&rest _) (setq after-change-ran t))
                 nil t)
       (add-hook 'gptel-post-stream-hook
                 (lambda () (setq post-stream-ran t))
                 nil t))
     (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
      (lambda (response callback-info &optional _raw)
        (with-current-buffer (plist-get callback-info :buffer)
          (goto-char (plist-get callback-info :position))
          (insert response)
          (run-hooks 'gptel-post-stream-hook)))
      "chunk" info)
     (should-not after-change-ran)
     (should post-stream-ran)
     (with-current-buffer data-buf
       (should (string-match-p "chunk" (buffer-string))))))

  :doc "batching coalesces consecutive string stream chunks"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (niwa-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "a" info)
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "b" info)
       (should-not calls)
       (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
       (should (equal '(("ab" nil)) calls)))))

  :doc "batching flushes pending strings before non-string events"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (niwa-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "text" info)
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig '(tool-call . nil) info)
       (should (equal '(((tool-call) nil) ("text" nil)) calls)))))

  :doc "batching flushes when raw insertion mode changes"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (niwa-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "before" info nil)
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "raw" info t)
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "normal" info nil)
       (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
       (should (equal '(("normal" nil) ("raw" t) ("before" nil)) calls)))))

  :doc "batching flushes before stream cleanup"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let* ((process (make-pipe-process
                    :name "niwa-test-stream-cleanup"))
          (calls nil)
          (niwa-gptel-stream-bridge-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil))))
          (fsm (gptel-make-fsm :info info))
          (gptel--request-alist (list (cons process (cons fsm #'ignore)))))
     (unwind-protect
         (cl-labels ((orig (response _info &optional raw)
                       (push (list response raw) calls)))
           (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
            #'orig "a" info)
           (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
            #'orig "b" info)
           (niwa-gptel-stream-bridge--gptel-stream-cleanup-advice
            (lambda (_process status)
              (push (list 'cleanup status) calls))
            process "finished")
           (should (equal '((cleanup "finished") ("ab" nil)) calls)))
       (when (process-live-p process)
         (delete-process process)))))

  :doc "batching drops stale batches after stream buffer teardown"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (niwa-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "stale" info)
       (kill-buffer data-buf)
       (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
       (should-not calls))))

  :doc "nil or zero batch delay preserves immediate insertion"
  (dolist (delay '(nil 0))
    (niwa-gptel-stream-bridge-test--with-data-buffer
     (let ((calls nil)
           (niwa-gptel-stream-bridge-insert-batch-delay delay)
           (info (list :buffer data-buf
                       :position (with-current-buffer data-buf
                                   (copy-marker (point-max) nil)))))
       (cl-labels ((orig (response _info &optional raw)
                     (push (list response raw) calls)))
         (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
          #'orig "a" info)
         (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
          #'orig "b" info)
         (should (equal '(("b" nil) ("a" nil)) calls)))))))

(niwa-deftest niwa-gptel-stream-bridge--wrap-gptel-stream-transformer ()
  ,test
  (test)
  :doc "stale gptel stream transformer errors return the raw chunk"
  (niwa-gptel-stream-bridge-test--with-data-buffer
   (let ((info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil))
                     :transformer
                     (lambda (_str)
                       (error "Selecting deleted buffer")))))
     (niwa-gptel-stream-bridge--repair-gptel-stream-info info)
     (should (plist-get info :niwa-transformer-wrapped))
     (let (diagnostics)
       (niwa-test--with-captured-diagnostics diagnostics
                                                (should (equal "raw chunk"
                                                               (funcall (plist-get info :transformer)
                                                                        "raw chunk"))))
       (should (string-match-p "stale gptel stream transformer"
                               diagnostics))))))

(defun niwa-gptel-stream-bridge-test--await-status (process status)
  "Wait up to two seconds for PROCESS to report STATUS."
  (with-timeout (2 nil)
    (while (not (eq (process-status process) status))
      (accept-process-output nil 0.01)))
  (process-status process))

(defmacro niwa-gptel-stream-bridge-test--with-paced-stream (&rest body)
  "Run BODY with a registered niwa stream PROCESS, its INFO and BUFFER.
PROCESS is a real `sleep' child; SENTINEL-CALLS collects what reaches
gptel's sentinel through the cleanup advice."
  (declare (indent 0) (debug t))
  `(niwa-gptel-stream-bridge-test--with-data-buffer
     (skip-unless (and (executable-find "sleep")
                       (not (memq system-type '(windows-nt ms-dos)))))
     (let* ((niwa-gptel-stream-bridge-insert-batch-delay 0.2)
            (niwa-transport--paused nil)
            (niwa-transport--pause-guard nil)
            (sentinel-calls nil)
            (buffer (generate-new-buffer " *niwa-test-stream-pace*"))
            (process (make-process
                      :name "niwa-test-stream-pace" :command '("sleep" "30")
                      :buffer buffer :connection-type 'pipe :noquery t
                      :sentinel
                      (lambda (process status)
                        (niwa-gptel-stream-bridge--gptel-stream-cleanup-advice
                         (lambda (_process status) (push status sentinel-calls))
                         process status))))
            (info (list :buffer data-buf :stop-reason nil))
            (fsm (gptel-make-fsm :info info))
            (gptel--request-alist (list (cons process (cons fsm #'ignore)))))
       (unwind-protect
           (progn ,@body)
         (when (process-live-p process)
           (delete-process process))
         (when niwa-transport--pause-guard
           (delete-process niwa-transport--pause-guard))
         (kill-buffer buffer)))))

(niwa-deftest niwa-gptel-stream-bridge--pace-reading ()
  ,test
  (test)
  :doc "stops a local stream until the batch is due, unseen by the sentinel"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (niwa-gptel-stream-bridge--note-stream-start process)
    (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
    (should (eq 'stop (niwa-gptel-stream-bridge-test--await-status
                       process 'stop)))
    (should (memq process niwa-transport--paused))
    ;; A second read while stopped arms no further timer.
    (let ((timer (process-get process 'niwa-transport--resume-timer)))
      (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
      (should (eq timer (process-get process
                                     'niwa-transport--resume-timer))))
    (should (eq 'run (niwa-gptel-stream-bridge-test--await-status process 'run)))
    (should-not niwa-transport--paused)
    (should-not sentinel-calls)
    (delete-process process)
    (accept-process-output nil 0.05)
    (should (= 1 (length sentinel-calls))))

  :doc "ignores a stop reason left by an earlier tool round"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (plist-put info :stop-reason "tool_use")
    (niwa-gptel-stream-bridge--note-stream-start process)
    (should-not (niwa-gptel-stream-bridge--stream-ended-p process))
    (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
    (should (process-get process 'niwa-transport--resume-timer))
    ;; This round's own stop reason ends pacing and continues curl at once.
    (plist-put info :stop-reason (copy-sequence "end_turn"))
    (should (niwa-gptel-stream-bridge--stream-ended-p process))
    (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
    (should-not (process-get process 'niwa-transport--resume-timer))
    (should (eq 'run (niwa-gptel-stream-bridge-test--await-status process 'run))))

  :doc "lets a finished OpenAI-compatible stream exit at once"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (niwa-gptel-stream-bridge--note-stream-start process)
    (with-current-buffer buffer (insert "data: {}\n\ndata: [DONE]\n\n"))
    (niwa-gptel-stream-bridge--pace-reading process "data: [DONE]\n\n")
    (should-not (process-get process 'niwa-transport--resume-timer))
    (should (eq 'run (process-status process))))

  :doc "keeps reading while curl delivers a backlog"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (niwa-gptel-stream-bridge--note-stream-start process)
    (niwa-gptel-stream-bridge--pace-reading
     process (make-string niwa-gptel-stream-bridge--pace-backlog-bytes ?x))
    (should-not (process-get process 'niwa-transport--resume-timer)))

  :doc "leaves streams that are not niwa's alone"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (plist-put info :buffer buffer)
    (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
    (should-not (process-get process 'niwa-transport--resume-timer)))

  :doc "continues paused streams on uninstall"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (niwa-gptel-stream-bridge--note-stream-start process)
    (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
    (should (eq 'stop (niwa-gptel-stream-bridge-test--await-status process 'stop)))
    (let ((niwa-gptel-stream-bridge--gptel-stream-advice-installed t))
      (cl-letf (((symbol-function 'niwa-gptel-stream-bridge--uninstall-advice) #'ignore))
        (niwa-gptel-stream-bridge-uninstall)))
    (should-not niwa-transport--paused)
    (should (eq 'run (niwa-gptel-stream-bridge-test--await-status process 'run))))

  :doc "never stops a stream without batching or a non-subprocess connection"
  (let ((process (make-pipe-process :name "niwa-test-stream-pace-pipe"))
        (signals nil))
    (unwind-protect
        (cl-letf (((symbol-function 'signal-process)
                   (lambda (&rest args) (push args signals) 0)))
          (let ((niwa-gptel-stream-bridge-insert-batch-delay 0.2))
            (niwa-gptel-stream-bridge--pace-reading process "x"))
          (dolist (delay '(nil 0))
            (let ((niwa-gptel-stream-bridge-insert-batch-delay delay))
              (niwa-gptel-stream-bridge--pace-reading process "x")))
          (should-not signals))
      (delete-process process))))

(defun niwa-gptel-stream-bridge-test--batch-falls-due (process)
  "Make PROCESS's pause end now, when its batch falls due in real use.
The read that paused PROCESS also started the batch, so both are due
together; a test flushes at once instead of waiting for the pair."
  (timer-set-time (process-get process 'niwa-transport--resume-timer) nil))

(niwa-deftest niwa-gptel-stream-bridge--flush-batch-due ()
  ,test
  (test)
  :doc "renders and continues the stream in the batch's own timer"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (let* ((events nil)
           (unrelated-fired nil)
           ;; An unrelated timer can expire during the event-loop waits.
           (unrelated (run-at-time 0 nil (lambda () (setq unrelated-fired t))))
           (timers (copy-sequence timer-list)))
      (cl-labels ((orig (response _info &optional _raw)
                    (push (list 'insert response) events)
                    ;; What the view's stream hook does during the flush.
                    (should niwa-gptel-stream-bridge-flushing)
                    (niwa-gptel-stream-bridge-after-flush
                     (lambda ()
                       (should-not niwa-gptel-stream-bridge-flushing)
                       (push 'render events)))
                    (niwa-gptel-stream-bridge-after-flush
                     (lambda () (error "Injected follow-up failure")))
                    (niwa-gptel-stream-bridge-after-flush
                     (lambda () (push 'second events)))))
        (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
         #'orig "chunk" info)
        (let ((timer (plist-get info :niwa-stream-insert-timer)))
          (should (eq #'niwa-gptel-stream-bridge--flush-batch-due
                      (timer--function timer)))
          (niwa-gptel-stream-bridge--note-stream-start process)
          (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
          (should (eq 'stop (niwa-gptel-stream-bridge-test--await-status
                             process 'stop)))
          (with-timeout (2 nil)
            (while (not unrelated-fired) (accept-process-output nil 0.01)))
          (should unrelated-fired)
          (should-not (memq unrelated timer-list))
          (niwa-gptel-stream-bridge-test--batch-falls-due process)
          (let (messages)
            (niwa-test--with-captured-diagnostics messages
              (niwa-gptel-stream-bridge--flush-batch-due info))
            (should (string-match-p "Injected follow-up failure" messages)))
          ;; Inserted, rendered and continued: no timer is left for any of it.
          (should (equal '(second render (insert "chunk")) events))
          (should-not niwa-transport--paused)
          (should-not (process-get process 'niwa-transport--resume-timer))
          ;; Existing timers may have fired; only newly pending timers leak.
          (should-not (cl-set-difference timer-list timers :test #'eq))
          (should (eq 'run (niwa-gptel-stream-bridge-test--await-status
                            process 'run)))))))

  :doc "continues the stream and runs follow-ups when the insert fails"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (let ((rendered nil))
      (cl-labels ((orig (_response _info &optional _raw)
                    (niwa-gptel-stream-bridge-after-flush
                     (lambda () (setq rendered t)))
                    (error "Injected insert failure")))
        (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
         #'orig "chunk" info)
        (niwa-gptel-stream-bridge--note-stream-start process)
        (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
        (should (eq 'stop (niwa-gptel-stream-bridge-test--await-status
                           process 'stop)))
        (niwa-gptel-stream-bridge-test--batch-falls-due process)
        (should-error (niwa-gptel-stream-bridge--flush-batch-due info))
        (should rendered)
        (should-not niwa-transport--paused))))

  :doc "continues every stream when batching was turned off meanwhile"
  (niwa-gptel-stream-bridge-test--with-paced-stream
    (cl-labels ((orig (&rest _) nil))
      (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
       #'orig "chunk" info)
      (niwa-gptel-stream-bridge--note-stream-start process)
      (niwa-gptel-stream-bridge--pace-reading process "data: {}\n\n")
      (should (memq process niwa-transport--paused))
      (let ((niwa-gptel-stream-bridge-insert-batch-delay nil))
        (niwa-gptel-stream-bridge--flush-batch-due info))
      (should-not niwa-transport--paused)))

  :doc "real gptel insertion locks its marker before same-pass observers run"
  (niwa-gptel-stream-bridge-test--with-data-buffer
    (let ((info (list :buffer data-buf))
          (niwa-gptel-stream-bridge-insert-batch-delay 30)
          (timers (copy-sequence timer-list))
          observed)
      (with-current-buffer data-buf
        (setq-local
         gptel-post-stream-hook
         (list (lambda ()
                 (should (marker-insertion-type
                          (plist-get info :tracking-marker)))
                 (niwa-gptel-stream-bridge-after-flush
                  (lambda ()
                    (should-not (marker-insertion-type
                                 (plist-get info :tracking-marker)))
                    (setq observed
                          (with-current-buffer data-buf
                            (buffer-substring-no-properties
                             (plist-get info :position)
                             (plist-get info :tracking-marker))))))))))
      (unwind-protect
          (progn
            (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
             #'gptel-curl--stream-insert-response "first " info)
            (niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
             #'gptel-curl--stream-insert-response "second" info)
            (should-not observed)
            (niwa-gptel-stream-bridge--flush-batch-due info)
            (should (equal observed "first second"))
            (should-not (cl-set-difference timer-list timers :test #'eq)))
        (when-let* ((timer (plist-get info :niwa-stream-insert-timer)))
          (cancel-timer timer))))))

(niwa-deftest niwa-gptel-stream-bridge-after-flush ()
  ,test
  (test)
  :doc "defers to a zero-delay timer outside a batch's own flush"
  (let* ((ran nil)
         (timer (niwa-gptel-stream-bridge-after-flush (lambda () (setq ran t)))))
    (unwind-protect
        (progn
          (should (timerp timer))
          (should-not ran)
          (with-timeout (2 nil)
            (while (not ran) (accept-process-output nil 0.01)))
          (should ran))
      (cancel-timer timer))))

(niwa-deftest niwa-gptel-stream-bridge--gptel-stream-filter-advice ()
  ,test
  (test)
  :doc "buffers stream chunks until gptel registers the process FSM"
  (let ((process (make-pipe-process
                  :name "niwa-test-stream-filter"))
        (gptel--request-alist nil)
        (calls nil)
        scheduled)
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-gptel-stream-bridge--schedule-gptel-stream-filter-flush)
                   (lambda (proc) (setq scheduled proc))))
          (niwa-gptel-stream-bridge--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: a\n")
          (should (eq scheduled process))
          (should-not calls)
          (should (equal "event: a\n"
                         (process-get
                          process
                          'niwa-gptel-stream-bridge--pending-output)))
          (setf (alist-get process gptel--request-alist)
                (cons 'fake-fsm #'ignore))
          (niwa-gptel-stream-bridge--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: b\n")
          (should (equal '("event: a\nevent: b\n") calls))
          (should-not
           (process-get
            process 'niwa-gptel-stream-bridge--pending-output)))
      (when (process-live-p process)
        (delete-process process)))))

(niwa-deftest niwa-gptel-stream-bridge--install-if-enabled ()
  ,test
  (test)
  :doc "deferred installer is a no-op after uninstall disables stream repair"
  (let ((calls 0)
        (niwa-gptel-stream-bridge--gptel-stream-advice-installed nil))
    (cl-letf (((symbol-function 'niwa-gptel-stream-bridge--install-advice)
               (lambda () (cl-incf calls))))
      (niwa-gptel-stream-bridge--install-if-enabled)
      (should (= calls 0))
      (let ((niwa-gptel-stream-bridge--gptel-stream-advice-installed t))
        (niwa-gptel-stream-bridge--install-if-enabled))
      (should (= calls 1)))))

(niwa-deftest niwa-gptel-stream-bridge-uninstall ()
  ,test
  (test)
  :doc "uninstall disables future deferred installs"
  (let ((niwa-gptel-stream-bridge--gptel-stream-advice-installed t))
    (cl-letf (((symbol-function 'niwa-gptel-stream-bridge--uninstall-advice)
               #'ignore))
      (niwa-gptel-stream-bridge-uninstall))
    (should-not niwa-gptel-stream-bridge--gptel-stream-advice-installed)))

(provide 'test-niwa-gptel-stream-bridge)

;;; test-niwa-gptel-stream-bridge.el ends here
