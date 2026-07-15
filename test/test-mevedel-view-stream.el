;;; test-mevedel-view-stream.el --- Streaming view tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-stream)
(require 'mevedel-agents)
(require 'mevedel-agent-runtime)
(require 'mevedel-file-state)
(require 'mevedel-hooks)
(require 'mevedel-mentions)
(require 'mevedel-permission-queue)
(require 'mevedel-pipeline)
(require 'mevedel-preview-mode)
(require 'mevedel-review)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-ui)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-media)
(require 'mevedel-goal)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-repair)
(require 'mevedel-tool-task)
(require 'mevedel-tool-ui)
(require 'mevedel-view-zone)
(require 'mevedel-view-history)
(require 'mevedel-workspace)

(defmacro mevedel-view-stream-test--with-buffers (&rest body)
  "Create paired data and view buffers, then evaluate BODY."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *mevedel-stream-data*"))
         (view-buf (generate-new-buffer " *mevedel-stream-view*"))
         (mevedel-user-dir
          (file-name-as-directory
           (make-temp-file "mevedel-view-stream-user-" t)))
         (mevedel-plugin-extra-roots nil))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode)
             (setq-local mevedel--current-request nil)
             (setq-local mevedel--session nil)
             (setq-local gptel-response-separator "\n\n")
             (setq-local gptel-prompt-prefix-alist '((org-mode . "*** "))))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf)
         (with-current-buffer view-buf
           (mevedel-view-stream-stop))
         (kill-buffer view-buf))
       (when (buffer-live-p data-buf)
         (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t)))))

(defun mevedel-view-stream-test--insert-data (buffer text property)
  "Insert TEXT with gptel PROPERTY into BUFFER."
  (with-current-buffer buffer
    (let ((start (point-max)))
      (goto-char start)
      (insert text)
      (put-text-property start (point) 'gptel property))))

(defun mevedel-view-stream-test--insert-composer-draft
    (draft &optional point-offset)
  "Insert DRAFT into the editable composer and move point by POINT-OFFSET."
  (let ((start (mevedel-view--input-start))
        (inhibit-read-only t))
    (goto-char start)
    (insert draft)
    (remove-text-properties
     start (point)
     '(read-only nil
       mevedel-view-prompt nil
       font-lock-face nil
       face nil
       front-sticky nil
       rear-nonsticky nil))
    (goto-char (+ start (or point-offset (length draft))))))

(defun mevedel-view-stream-test--insert-repair-audited-tool (data-buf)
  "Insert one completed repair-audited tool call into DATA-BUF."
  (with-current-buffer data-buf
    (let ((start (point)))
      (insert "(:name \"Collect\" :args (:names [\"alice\"]))\n\ncompleted\n")
      (put-text-property start (point) 'gptel '(tool . "repair-call")))
    (let ((start (point)))
      (insert
       (mevedel-tool-repair-format-audit-block
        'committed
        '((:rule wrap-array-singleton :source generic
                :paths ((names)) :before string :after array))))
      (put-text-property start (point) 'gptel 'ignore))))

(defun mevedel-view-stream-test--count-substring (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0)
        position)
    (while (setq position (string-search needle text start))
      (cl-incf count)
      (setq start (+ position (length needle))))
    count))

(mevedel-deftest mevedel-view-stream-begin-turn ()
  ,test
  (test)
  :doc "records independent active-turn markers and cleanup releases them"
  (mevedel-view-stream-test--with-buffers
    (let ((data-start (with-current-buffer data-buf
                        (copy-marker (point-max) nil))))
      (with-current-buffer view-buf
        (let ((view-start (copy-marker mevedel-view--input-marker nil)))
          (mevedel-view-stream-begin-turn view-start data-start)
          (should (markerp mevedel-view--in-flight-turn-start))
          (should (markerp mevedel-view--data-turn-start))
          (should-not (eq view-start mevedel-view--in-flight-turn-start))
          (should-not (eq data-start mevedel-view--data-turn-start))
          (mevedel-view-stream-stop)
          (should-not mevedel-view--in-flight-turn-start)
          (should-not mevedel-view--data-turn-start))))))

(mevedel-deftest mevedel-view-stream-schedule ()
  ,test
  (test)
  :doc "async streaming redraw preserves multiline leading-> draft and point"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
    (let ((data-start (with-current-buffer data-buf
                        (copy-marker (point-min) nil)))
          (draft "> quoted\nsecond line"))
      (mevedel-view-stream-test--insert-data
       data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--data-turn-start data-start)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay repeat callback &rest args)
                     (unless repeat
                       (apply callback args))
                     'scheduled)))
          (with-current-buffer data-buf
            (mevedel-view-stream-schedule)))
        (setq mevedel-view--stream-render-timer nil)
        (should (string-match-p
                 "Assistant text"
                 (buffer-substring-no-properties
                  (point-min) (mevedel-view--input-start))))
        (should (equal draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))))))

(mevedel-deftest mevedel-view--render-stream-update
  (:doc "rolls back failed agent observer renders")
  ,test
  (test)
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq-local mevedel-view--agent-transcript-p t)
      (let ((before (buffer-string))
            warned)
        (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                   (lambda (&rest _)
                     (let ((inhibit-read-only t))
                       (delete-region (point-min) (point-max)))
                     (error "Observer render failed")))
                  ((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warned t))))
          (mevedel-view--render-stream-update data-buf))
        (should warned)
        (should (equal before (buffer-string)))))))

(mevedel-deftest mevedel-view-stream-install ()
  ,test
  (test)
  :doc "install enables advice and deferred installers honor that state"
  (let ((mevedel-view-stream--gptel-stream-advice-installed nil)
        (calls 0))
    (cl-letf (((symbol-function 'mevedel-view-stream--install-advice)
               (lambda () (cl-incf calls))))
      (mevedel-view-stream-install)
      (should mevedel-view-stream--gptel-stream-advice-installed)
      (should (> calls 0))
      (mevedel-view-stream-uninstall)
      (should-not mevedel-view-stream--gptel-stream-advice-installed))))

(mevedel-deftest mevedel-view-stream-ensure-progress-for-fsm ()
  ,test
  (test)
  :doc "direct top-level FSM anchors both sides and requests progress"
  (mevedel-view-stream-test--with-buffers
    (let* ((position (with-current-buffer data-buf
                       (copy-marker (point-max) nil)))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf :position position)))
           ensured)
      (cl-letf (((symbol-function 'mevedel-view--agent-fsm-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-view--ensure-request-progress)
                 (lambda (buffer &optional _status)
                   (setq ensured buffer))))
        (mevedel-view-stream-ensure-progress-for-fsm fsm))
      (with-current-buffer view-buf
        (should (eq ensured data-buf))
        (should (markerp mevedel-view--data-turn-start))
        (should (= (marker-position mevedel-view--data-turn-start)
                   (marker-position position)))
        (should (markerp mevedel-view--in-flight-turn-start))))))

;;
;;; gptel stream and bridge helpers

(mevedel-deftest mevedel-view-stream--repair-gptel-stream-info ()
  ,test
  (test)
  :doc "reanchors detached position markers for mevedel stream callbacks"
  (mevedel-view-stream-test--with-buffers
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
        (should (eq (mevedel-view-stream--repair-gptel-stream-info info) info))
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
        (mevedel-view-stream--repair-gptel-stream-info info)
        (should (eq (plist-get info :position) position))
        (should-not (marker-position position))))))

(mevedel-deftest mevedel-view-stream--gptel-stream-insert-response-advice ()
  ,test
  (test)
  :doc "repairs detached markers before delegating to gptel streaming"
  (mevedel-view-stream-test--with-buffers
    (let ((position (copy-marker 1 nil))
          (mevedel-view-stream-insert-batch-delay nil))
      (set-marker position nil)
      (let ((info (list :buffer data-buf :position position)))
        (should
         (eq (mevedel-view-stream--gptel-stream-insert-response-advice
              (lambda (_response callback-info &optional _raw)
                (let ((marker (plist-get callback-info :position)))
                  (and (markerp marker)
                       (eq (marker-buffer marker) data-buf)
                       (marker-position marker)
                       'delegated)))
              "chunk"
             info)
             'delegated))))))

(mevedel-deftest mevedel-view-stream--gptel-stream-insert-response-advice/performance ()
  ,test
  (test)

  :doc "mevedel stream insertion suppresses after-change hooks but keeps post-stream hook"
  (mevedel-view-stream-test--with-buffers
    (let ((after-change-ran nil)
          (post-stream-ran nil)
          (mevedel-view-stream-insert-batch-delay nil)
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
      (mevedel-view-stream--gptel-stream-insert-response-advice
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
  (mevedel-view-stream-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view-stream--gptel-stream-insert-response-advice
         #'orig "a" info)
        (mevedel-view-stream--gptel-stream-insert-response-advice
         #'orig "b" info)
        (should-not calls)
        (mevedel-view-stream--flush-gptel-stream-insert-batch info)
        (should (equal '(("ab" nil)) calls)))))

  :doc "batching flushes pending strings before non-string events"
  (mevedel-view-stream-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view-stream--gptel-stream-insert-response-advice
         #'orig "text" info)
        (mevedel-view-stream--gptel-stream-insert-response-advice
         #'orig '(tool-call . nil) info)
        (should (equal '(((tool-call) nil) ("text" nil)) calls)))))

  :doc "batching flushes when raw insertion mode changes"
  (mevedel-view-stream-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view-stream--gptel-stream-insert-response-advice
         #'orig "raw" info t)
        (mevedel-view-stream--gptel-stream-insert-response-advice
         #'orig "normal" info nil)
        (mevedel-view-stream--flush-gptel-stream-insert-batch info)
        (should (equal '(("normal" nil) ("raw" t)) calls)))))

  :doc "batching flushes before stream cleanup"
  (mevedel-view-stream-test--with-buffers
    (let* ((process (make-pipe-process
                     :name "mevedel-test-stream-cleanup"))
           (calls nil)
           (mevedel-view-stream-insert-batch-delay 10)
           (info (list :buffer data-buf
                       :position (with-current-buffer data-buf
                                   (copy-marker (point-max) nil))))
           (fsm (gptel-make-fsm :info info))
           (gptel--request-alist (list (cons process (cons fsm #'ignore)))))
      (unwind-protect
          (cl-labels ((orig (response _info &optional raw)
                        (push (list response raw) calls)))
            (mevedel-view-stream--gptel-stream-insert-response-advice
             #'orig "a" info)
            (mevedel-view-stream--gptel-stream-insert-response-advice
             #'orig "b" info)
            (mevedel-view-stream--gptel-stream-cleanup-advice
             (lambda (_process status)
               (push (list 'cleanup status) calls))
             process "finished")
            (should (equal '((cleanup "finished") ("ab" nil)) calls)))
        (when (process-live-p process)
          (delete-process process)))))

  :doc "batching drops stale batches after stream buffer teardown"
  (mevedel-view-stream-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view-stream--gptel-stream-insert-response-advice
         #'orig "stale" info)
        (kill-buffer data-buf)
        (mevedel-view-stream--flush-gptel-stream-insert-batch info)
        (should-not calls))))

  :doc "nil or zero batch delay preserves immediate insertion"
  (dolist (delay '(nil 0))
    (mevedel-view-stream-test--with-buffers
      (let ((calls nil)
            (mevedel-view-stream-insert-batch-delay delay)
            (info (list :buffer data-buf
                        :position (with-current-buffer data-buf
                                    (copy-marker (point-max) nil)))))
        (cl-labels ((orig (response _info &optional raw)
                      (push (list response raw) calls)))
          (mevedel-view-stream--gptel-stream-insert-response-advice
           #'orig "a" info)
          (mevedel-view-stream--gptel-stream-insert-response-advice
           #'orig "b" info)
          (should (equal '(("b" nil) ("a" nil)) calls)))))))

(mevedel-deftest mevedel-view-stream--wrap-gptel-stream-transformer ()
  ,test
  (test)
  :doc "stale gptel stream transformer errors return the raw chunk"
  (mevedel-view-stream-test--with-buffers
    (let ((info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil))
                      :transformer
                      (lambda (_str)
                        (error "Selecting deleted buffer")))))
      (mevedel-view-stream--repair-gptel-stream-info info)
      (should (plist-get info :mevedel-transformer-wrapped))
      (should (equal "raw chunk"
                     (funcall (plist-get info :transformer)
                              "raw chunk"))))))

(mevedel-deftest mevedel-view-stream--gptel-stream-filter-advice ()
  ,test
  (test)
  :doc "buffers stream chunks until gptel registers the process FSM"
  (let ((process (make-pipe-process
                  :name "mevedel-test-stream-filter"))
        (gptel--request-alist nil)
        (calls nil)
        scheduled)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-view-stream--schedule-gptel-stream-filter-flush)
                   (lambda (proc) (setq scheduled proc))))
          (mevedel-view-stream--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: a\n")
          (should (eq scheduled process))
          (should-not calls)
          (should (equal "event: a\n"
                         (process-get
                          process 'mevedel-view--pending-stream-output)))
          (setf (alist-get process gptel--request-alist)
                (cons 'fake-fsm #'ignore))
          (mevedel-view-stream--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: b\n")
          (should (equal '("event: a\nevent: b\n") calls))
          (should-not
           (process-get process 'mevedel-view--pending-stream-output)))
      (when (process-live-p process)
        (delete-process process)))))

(mevedel-deftest mevedel-view-stream--install-if-enabled ()
  ,test
  (test)
  :doc "deferred installer is a no-op after uninstall disables stream repair"
  (let ((calls 0)
        (mevedel-view-stream--gptel-stream-advice-installed nil))
    (cl-letf (((symbol-function 'mevedel-view-stream--install-advice)
               (lambda () (cl-incf calls))))
      (mevedel-view-stream--install-if-enabled)
      (should (= calls 0))
      (let ((mevedel-view-stream--gptel-stream-advice-installed t))
        (mevedel-view-stream--install-if-enabled))
      (should (= calls 1)))))

(mevedel-deftest mevedel-view-stream-uninstall ()
  ,test
  (test)
  :doc "uninstall disables future deferred installs"
  (let ((mevedel-view-stream--gptel-stream-advice-installed t))
    (cl-letf (((symbol-function 'mevedel-view-stream--uninstall-advice)
               #'ignore))
      (mevedel-view-stream-uninstall))
    (should-not mevedel-view-stream--gptel-stream-advice-installed)))


;;
;;; Pending tool lifecycle

(mevedel-deftest mevedel-view--pending-tool-key
  (:doc "keys pending tool calls by call id when available")
  ,test
  (test)
  :doc "uses backend call id before name/args fingerprint"
  (should (equal "call-1"
                 (mevedel-view--pending-tool-key
                  '(:id "call-1" :name "Read" :args (:file_path "a")))))
  :doc "identical calls with distinct ids stay distinct"
  (should-not (equal
               (mevedel-view--pending-tool-key
                '(:id "call-1" :name "Read" :args (:file_path "a")))
               (mevedel-view--pending-tool-key
                '(:id "call-2" :name "Read" :args (:file_path "a"))))))


(mevedel-deftest mevedel-view--pending-tool-calls
  (:doc "tracks and renders the pending-tool live tail")
  ,test
  (test)

  :doc "pre/post hooks add and remove entries by call id"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:id "call-1" :name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view-stream-pre-tool
            '(:id "call-2" :name "Grep" :args (:pattern "x"))))
          (with-current-buffer view-buf
            (should (equal '(("call-1" . "Calling Read: a...")
                             ("call-2" . "Calling Grep: x..."))
                           mevedel-view--pending-tool-calls)))
          (should-not
           (mevedel-view-stream-post-tool
            '(:id "call-1" :name "Read" :args (:file_path "a"))))))
      (with-current-buffer view-buf
        (should (equal '(("call-2" . "Calling Grep: x..."))
                       mevedel-view--pending-tool-calls)))
      (should (= 3 render-count))))

  :doc "tool-boundary renders are debounced and coalesced"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0.02))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:id "call-1" :name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view-stream-pre-tool
            '(:id "call-2" :name "Grep" :args (:pattern "x")))))
        (should (= 0 render-count))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (= render-count 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should (= 1 render-count)))))

  :doc "immediate pending live tail inserts above status-zone content"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker status-start)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--status-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail recovers above status-zone content when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail stays above status-zone content after immediate render"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 1 (cl-loop with start = 0
                                      while (string-match "Calling Read: a"
                                                          text start)
                                      count t
                                      do (setq start (match-end 0))))))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail recovers after existing history when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail stays after existing live assistant text"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
          (insert (propertize "Assistant\n"
                              'mevedel-view-type 'turn-header
                              'mevedel-view-turn-role 'assistant))
          (insert (propertize "Existing response\n"
                              'mevedel-view-type 'response
                              'mevedel-view-source '(1 . 2)))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (existing (string-match-p "Existing response" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp existing))
                (should (numberp calling))
                (should (numberp status))
                (should (< existing calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail stays above propertized status rows"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert (propertize "AGENT STATUS\n"
                              'mevedel-view-type 'agent-handle
                              'mevedel-view-agent-status t))
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "AGENT STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail ignores stale attached status marker"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker (point-min))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (header (string-match-p "mevedel" text))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp header))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< header previous))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail recovers after collapsed turn summaries"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert (propertize "Previous turn\n"
                              'mevedel-view-type 'turn-summary))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "incremental render preserves status rows when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view--render-incremental data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render recovers stale in-flight marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (mevedel-view--set-in-flight-turn-start (point-min))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view--render-incremental data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render prefers source recovery over stale in-flight marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (mevedel-view--after-header-position)))
        (goto-char (marker-position mevedel-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'mevedel-view-type 'turn-summary))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (mevedel-view--set-in-flight-turn-start history-start)
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view--render-incremental data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (previous (string-match-p "Previous turn" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp previous))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< previous response))
          (should (< response status))))))

  :doc "incremental render ignores stale attached status marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker (point-min))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (mevedel-view--set-in-flight-turn-start (point-min))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view--render-incremental data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (header (string-match-p "mevedel" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp header))
          (should-not old)
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< header response))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render ignores status marker before existing history"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (mevedel-view--after-header-position)))
        (goto-char (marker-position mevedel-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'mevedel-view-type 'turn-summary))
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker history-start)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view--render-incremental data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (previous (string-match-p "Previous turn" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp previous))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< previous response))
          (should (< response status))))))

  :doc "incremental render ignores status marker inside status row"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (let ((row-start (point)))
          (insert (propertize "TASK STATUS\n"
                              'mevedel-view-type 'agent-handle
                              'display "TASK STATUS"
                              'keymap mevedel-view--display-map
                              'read-only t))
          (set-marker mevedel-view--status-marker (+ row-start 5)))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view--render-incremental data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< response status))))))

  :doc "incremental render restores aggregate status when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:agent-id "verifier--abc123"
                   :status running
                   :agent-type "verifier"
                   :description "verify"
                   :calls 1)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (mevedel-view--render-agent-status)
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (set-marker mevedel-view--status-marker nil)
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (mevedel-view--render-incremental data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Agent: verifier" text))
                 (prompt (string-match-p "^> " text)))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (numberp prompt))
            (should (< response status))
            (should (< status prompt)))))))

  :doc "aggregate status stays below pending live tail with detached status marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:agent-id "verifier--abc123"
                   :status running
                   :agent-type "verifier"
                   :description "verify"
                   :calls 1)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (set-marker mevedel-view--status-marker nil)
            (set-marker mevedel-view--interaction-marker (point))
            (set-marker mevedel-view--input-marker (point))
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min))))
            (setq mevedel-view--pending-tool-calls
                  (list (cons "call-1" "Calling Read: a"))))
          (should (progn (mevedel-view--render-incremental data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (calling (string-match-p "Calling Read: a" text))
                 (status (string-match-p "Agent: verifier" text)))
            (should-not old)
            (should (numberp response))
            (should (numberp calling))
            (should (numberp status))
            (should (< response calling))
            (should (< calling status)))))))

  :doc "aggregate status ignores stale attached status marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:agent-id "verifier--abc123"
                   :status running
                   :agent-type "verifier"
                   :description "verify"
                   :calls 1)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (insert "TASK STATUS\n")
            (set-marker mevedel-view--status-marker (point-min))
            (set-marker mevedel-view--interaction-marker (point))
            (set-marker mevedel-view--input-marker (point))
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (mevedel-view--render-incremental data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (header (string-match-p "mevedel" text))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Agent: verifier" text)))
            (should (numberp header))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (< header response))
            (should (< response status)))))))

  :doc "pending live tail ignores stale input marker"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--input-marker (point-min))
          (mevedel-view--set-in-flight-turn-start (point-min))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "tool hooks do not return rendered agent-status strings to gptel"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _)
                   #(" ─── agents: 1 running [+] ─────────────────────────────────\n"
                     0 61 (font-lock-face mevedel-view-zone-separator)))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:id "call-1" :name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view-stream-post-tool
            '(:id "call-1" :name "Read" :args (:file_path "a"))))))))

  :doc "Agent pre-tool hook does not add a duplicate pending Calling Agent line"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:id "call-1" :name "Agent"
              :args (:subagent_type "explorer"))))))
      (with-current-buffer view-buf
        (should-not mevedel-view--pending-tool-calls)
        (should (= 1 render-count)))))

  :doc "rendering caps visible calls and adds a truncation tail"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-pending-tools-visible-max 2))
        (setq mevedel-view--pending-tool-calls
              '(("1" . "Calling Read...")
                ("2" . "Calling Grep...")
                ("3" . "Calling Bash...")))
        (mevedel-view--insert-pending-tool-lines
         (cl-subseq mevedel-view--pending-tool-calls 0 2))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Calling Read" text))
          (should (string-match-p "Calling Grep" text))
          (should-not (string-match-p "Calling Bash" text))
          (should (string-match-p "1 more tools running" text))))))

  :doc "post-tool hook removes only the completed pending fragment"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq mevedel-view--pending-tool-calls
              '(("call-1" . "Calling Read...")
                ("call-2" . "Calling Grep...")))
        (mevedel-view--refresh-pending-tool-lines))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (mevedel-view-stream-post-tool
               '(:id "call-1" :name "Read" :args (:file_path "a"))))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) (point-max))))
                (should-not (string-match-p "Calling Read" text))
                (should (string-match-p "Calling Grep" text))
                (goto-char (point-min))
                (should (search-forward "Calling Grep" nil t))
                (let ((grep-pos (match-beginning 0)))
                  (should (eq 'history-live
                              (get-text-property
                               grep-pos 'mevedel-view-zone-namespace)))
                  (should (equal "call-2"
                                 (get-text-property
                                  grep-pos 'mevedel-view-zone-id))))
                (should (equal '(("call-2" . "Calling Grep..."))
                               mevedel-view--pending-tool-calls)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "repair audit redraw preserves a single-line composer and point"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-repair-audited-tool data-buf)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (mevedel-view-stream-test--insert-composer-draft "keep drafting" 5))
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer data-buf
        (mevedel-view-stream-post-tool
         '(:id "repair-call" :name "Collect"
               :args (:names ["alice"])))))
    (with-current-buffer view-buf
      (should (equal "keep drafting" (mevedel-view--input-text)))
      (should (= (point) (+ (mevedel-view--input-start) 5)))
      (should (string-match-p
               "tool input repaired"
               (buffer-substring-no-properties
                (point-min) mevedel-view--input-marker)))))

  :doc "repair audit redraw preserves a multiline > composer and point"
  (mevedel-view-stream-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (mevedel-view-stream-test--insert-repair-audited-tool data-buf)
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (mevedel-view-stream-test--insert-composer-draft draft 4))
      (let ((mevedel-view-tool-boundary-render-delay 0))
        (with-current-buffer data-buf
          (mevedel-view-stream-post-tool
           '(:id "repair-call" :name "Collect"
                 :args (:names ["alice"])))))
      (with-current-buffer view-buf
        (should (equal draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should (string-match-p
                 "tool input repaired"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))

  :doc "post-tool hook deletes the live tail when no replacement text is ready"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Agent: explorer...")))
      (mevedel-view--insert-pending-tool-lines
       mevedel-view--pending-tool-calls)
      (should (string-match-p "Calling Agent"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
    (with-current-buffer data-buf
      (mevedel-view-stream-post-tool
       '(:id "call-1" :name "Agent" :args (:subagent_type "explorer"))))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should-not (string-match-p "Calling Agent" text))
        (should-not mevedel-view--pending-tool-calls))))

  :doc "final response render clears pending live tail before rendering"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "final answer\n" 'response)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Agent: explorer...")))
      (mevedel-view--insert-pending-tool-lines
       mevedel-view--pending-tool-calls))
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "final answer" text))
        (should-not (string-match-p "Calling Agent" text))
        (should-not mevedel-view--pending-tool-calls))))

  :doc "full rerender preserves ordinary calling text and recreates pending fragments"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Read...")))
      (let ((inhibit-read-only t)
            (tail-start (marker-position mevedel-view--input-marker)))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (insert "Assistant\n| Calling Read...\n")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker tail-start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (= 2 (mevedel-view-stream-test--count-substring
                      "Calling Read" text))))
      (let (plain-seen fragment-pos)
        (goto-char (point-min))
        (while (search-forward "Calling Read" nil t)
          (let ((pos (match-beginning 0)))
            (if (eq 'history-live
                    (get-text-property pos 'mevedel-view-zone-namespace))
                (setq fragment-pos pos)
              (setq plain-seen t))))
        (should plain-seen)
        (should fragment-pos)
        (should (equal "call-1"
                       (get-text-property
                        fragment-pos 'mevedel-view-zone-id)))
        (should (eq (mevedel-view-zone-region 'history-live)
                    (get-text-property
                     fragment-pos 'mevedel-view-zone-region))))))

  :doc "cleanup ignores stale pending regions without live fragments"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start
            stale-region)
        (goto-char mevedel-view--input-marker)
        (setq start (point))
        (insert "Calling Read...\n")
        (setq stale-region
              (make-overlay start (point) (current-buffer) nil nil))
        (overlay-put stale-region 'mevedel-view-zone 'history-live))
      (mevedel-view--delete-pending-tool-live-lines)
      (should (string-match-p "Calling Read"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
      (should-not (mevedel-view-zone-region 'history-live))))

  :doc "incremental render preserves live tail when no replacement content is ready"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (start nil))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\n› Calling Read: mevedel-pipeline.el...\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq mevedel-view--pending-tool-calls nil)
      (mevedel-view--render-incremental data-buf)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "Calling Read: mevedel-pipeline.el" text)))))

  :doc "explicit response bounds do not blank live tail without replacement content"
  (mevedel-view-stream-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max))
        (setq end (point-max)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (tail-start nil))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (setq tail-start (point))
          (insert "Assistant\n... Thinking... (1 lines)\nCalling Read...\n")
          (setq mevedel-view--in-flight-turn-start
                (copy-marker tail-start nil))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (set-marker-insertion-type mevedel-view--input-marker nil))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq mevedel-view--pending-tool-calls nil)
        (mevedel-view--render-incremental data-buf start end)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Assistant" text))
          (should (string-match-p "Calling Read" text))))))

  :doc "incremental render keeps progress row beside pending tool details"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (mevedel-view--start-spinner "Thinking...")
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Read...")))
      (mevedel-view--render-incremental data-buf)
      (should (mevedel-view--request-progress-visible-p))
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "Working" text))
        (should (string-match-p "Calling Read" text))))))


;;
;;; Re-render idempotence with renderer


;;
;;; Request progress

(mevedel-deftest mevedel-view--start-spinner ()
  ,test
  (test)
  :doc "creates and removes progress/request fragment"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Working...")
      (goto-char (point-min))
      (should (search-forward "Working" mevedel-view--input-marker t))
      (goto-char (match-beginning 0))
      (should (eq 'progress
                  (get-text-property
                   (point) 'mevedel-view-zone-namespace)))
      (should (eq 'request
                  (get-text-property
                   (point) 'mevedel-view-zone-id)))
      (should (eq (mevedel-view-zone-region 'progress)
                  (get-text-property
                   (point) 'mevedel-view-zone-region)))
      (let ((zone-text (buffer-substring-no-properties
                        (point)
                        (mevedel-view--input-start))))
        (should (string-match-p "Working[^\n]*\n\n> \\'" zone-text)))
      (mevedel-view--stop-spinner)
      (should-not (text-property-any
                   (point-min) mevedel-view--input-marker
                   'mevedel-view-zone-namespace 'progress))))

  :doc "request spinner stays below queued interaction text"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-queued"
                       :root temporary-file-directory
                       :name "spinner-queued"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (setf (mevedel-session-queued-user-messages session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (mevedel-view--interaction-rebuild)
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (queued (string-match-p "queued while busy" text))
               (working (string-match-p "Working" text))
               (prompt (string-match-p "\n> " text working)))
          (should queued)
          (should working)
          (should prompt)
          (should (< queued working))
          (should (< working prompt)))
        (mevedel-view--stop-spinner))))

  :doc "queued interaction rebuild suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-queued-hooks"
                       :root temporary-file-directory
                       :name "spinner-queued-hooks"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (setf (mevedel-session-queued-user-messages session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (let ((changes 0))
          (add-hook 'after-change-functions
                    (lambda (&rest _ignore)
                      (cl-incf changes))
                    nil t)
          (mevedel-view--interaction-rebuild)
          (should (= 0 changes)))
        (mevedel-view--stop-spinner))))

  :doc "pending tool rows stay above queued text and request spinner"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-pending-queued"
                       :root temporary-file-directory
                       :name "spinner-pending-queued"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (mevedel-view--insert-pending-tool-lines
         (list (cons "call-1" "Calling Read: a")))
        (setf (mevedel-session-queued-user-messages session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (mevedel-view--interaction-rebuild)
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (calling (string-match-p "Calling Read: a" text))
               (queued (string-match-p "queued while busy" text))
               (working (string-match-p "Working" text))
               (prompt (string-match-p "\n> " text working)))
          (should calling)
          (should queued)
          (should working)
          (should prompt)
          (should (< calling queued))
          (should (< queued working))
          (should (< working prompt)))
        (mevedel-view--stop-spinner))))

  :doc "pending tool refresh suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((changes 0))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read…")))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--refresh-pending-tool-lines)
        (should (= 0 changes))
        (let ((pos (text-property-any
                    (point-min) mevedel-view--input-marker
                    'mevedel-view-pending-tool-live t)))
          (should pos)
          (should (eq 'history-live
                      (get-text-property
                       pos 'mevedel-view-zone-namespace)))
          (should (equal "call-1"
                         (get-text-property
                          pos 'mevedel-view-zone-id)))
          (should (eq (mevedel-view-zone-region 'history-live)
                      (get-text-property
                       pos 'mevedel-view-zone-region)))
          (should (< pos (marker-position mevedel-view--status-marker)))))))

  :doc "pending tool refresh preserves composer text and point"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-stream-test--insert-composer-draft "draft\n> keep typing" 8)
      (let ((input-offset (- (point) (mevedel-view--input-start))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read…")))
        (mevedel-view--refresh-pending-tool-lines)
        (should (equal "draft\n> keep typing"
                       (mevedel-view--input-text)))
        (should (= (- (point) (mevedel-view--input-start))
                   input-offset)))))

  :doc "request progress render preserves selected-window history point"
  (mevedel-view-stream-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (insert (propertize "Earlier answer\n"
                              'mevedel-view-type 'response
                              'mevedel-view-source '(1 . 2)))
          (set-marker-insertion-type mevedel-view--input-marker nil)
          (set-marker mevedel-view--status-marker
                      mevedel-view--input-marker)
          (set-marker mevedel-view--interaction-marker
                      mevedel-view--input-marker))
        (goto-char (point-min))
        (search-forward "Earlier")
        (goto-char (match-beginning 0))
        (let ((point-before (point)))
          (setq mevedel-view--spinner-start-time (current-time))
          (mevedel-view--ensure-request-progress data-buf)
          (should (= (window-point (selected-window)) point-before))
          (should (= (point) point-before))
          (should (looking-at-p "Earlier"))))))

  :doc "pre-tool render preserves selected-window composer point"
  (mevedel-view-stream-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0))
        (with-current-buffer view-buf
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker nil))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (mevedel-view-stream-test--insert-composer-draft
           "> quoted\nsecond line" 4))
        (let ((input-offset
               (with-current-buffer view-buf
                 (- (window-point (selected-window))
                    (mevedel-view--input-start)))))
          (with-current-buffer data-buf
            (mevedel-view-stream-pre-tool
             '(:id "call-1" :name "Read" :args (:file_path "foo.el"))))
          (with-current-buffer view-buf
            (should (mevedel-view--position-in-input-region-p
                     (window-point (selected-window))))
            (should (= (- (window-point (selected-window))
                          (mevedel-view--input-start))
                       input-offset))
            (should (= (point) (window-point (selected-window))))
            (should (equal "> quoted\nsecond line"
                           (mevedel-view--input-text)))
            (should (looking-at-p "oted")))))))

  :doc "pending tool refresh keeps ordinary calling rows and adds a fragment"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char mevedel-view--input-marker)
        (insert (propertize "Calling Read...\n"
                            'mevedel-view-source '(1 . 2)
                            'mevedel-view-type 'response))
        (set-marker-insertion-type mevedel-view--input-marker t)
        (insert "| Calling Read...\n")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker (point-min) nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--pending-tool-calls
            (list (cons "call-1" "Calling Read...")))
      (mevedel-view--refresh-pending-tool-lines)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (= 3 (mevedel-view-stream-test--count-substring
                      "Calling Read" text))))
      (let (occurrences)
        (goto-char (point-min))
        (while (search-forward "Calling Read" nil t)
          (push (list :source (get-text-property
                               (match-beginning 0) 'mevedel-view-source)
                      :namespace (get-text-property
                                  (match-beginning 0)
                                  'mevedel-view-zone-namespace))
                occurrences))
        (should (cl-some (lambda (entry) (plist-get entry :source))
                         occurrences))
        (should (cl-some (lambda (entry)
                           (eq 'history-live (plist-get entry :namespace)))
                         occurrences))
        (should (cl-some (lambda (entry)
                           (and (not (plist-get entry :source))
                                (not (plist-get entry :namespace))))
                         occurrences)))))

  :doc "update replaces spinner text"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--update-spinner "Calling Read...")
      (let ((text (buffer-substring-no-properties
                   (overlay-start
                    (mevedel-view-zone-region 'progress))
                   (overlay-end
                    (mevedel-view-zone-region 'progress)))))
        (should (string-match-p "Calling Read" text))
        (should-not (string-match-p "Thinking" text)))
      (mevedel-view--stop-spinner)))

  :doc "default spinner shows working elapsed time and active agents"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-agents"
                       :root temporary-file-directory
                       :name "spinner-agents"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 12))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons "coordinator--spin"
                        '(:status running :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (with-current-buffer view-buf
        (mevedel-view--start-spinner "Thinking...")
        (let ((text (buffer-substring-no-properties
                     (overlay-start
                      (mevedel-view-zone-region 'progress))
                     (overlay-end
                      (mevedel-view-zone-region 'progress)))))
          (should (string-match-p "Working\\.\\.\\." text))
          (should (string-match-p "[0-9]+s" text))
          (should (string-match-p "1 agent running" text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner ticks replace dynamic metadata instead of appending it"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-no-pileup"
                       :root temporary-file-directory
                       :name "spinner-no-pileup"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 12))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons "coordinator--spin"
                        '(:status running :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (with-current-buffer view-buf
        (mevedel-view--start-spinner "Thinking...")
        (dotimes (_ 3)
          (mevedel-view--spinner-tick))
        (let ((text (buffer-substring-no-properties
                     (overlay-start
                      (mevedel-view-zone-region 'progress))
                     (overlay-end
                      (mevedel-view-zone-region 'progress)))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "agent running" text start)
                                count t
                                do (setq start (match-end 0)))))
          (should-not (string-match-p
                       "agent running.*agent running" text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner tick suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read...")))
            (changes 0))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--spinner-tick)
        (should (= 0 changes))
        (mevedel-view--stop-spinner))))

  :doc "decorated spinner status is normalized to its base label"
  (should (equal "Working..."
                 (mevedel-view--spinner-base-status
                  "Working... · 14s · 1 agent running · 21s · 2 agents running")))

  :doc "spinner marker refresh keeps the live line outside composer input"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (goto-char (point-max))
      (insert "/auto")
      (should (equal "/auto" (mevedel-view--input-text)))
      (mevedel-view--spinner-tick)
      (should (equal "/auto" (mevedel-view--input-text)))
      (mevedel-view--stop-spinner)))

  :doc "spinner tick preserves composer point while drafting"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view-stream-test--insert-composer-draft "/auto")
      (let ((point-before (point)))
        (mevedel-view--spinner-tick)
        (should (equal "/auto" (mevedel-view--input-text)))
        (should (= (point) point-before)))
      (mevedel-view--stop-spinner)))

  :doc "spinner tick preserves point on a visible permission prompt"
  (let ((mevedel-view-spinner-animate nil))
    (mevedel-view-stream-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (insert (make-string 10 ?\n))))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "Permission Request\nTool: Read\nPath: .git/HEAD\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry :activate #'ignore))
        (mevedel-view--start-spinner "Working...")
        (goto-char (overlay-start (mevedel-view-zone-region 'interaction)))
        (search-forward "Read")
        (let ((permission-point (point)))
          (save-excursion
            (goto-char (point-min))
            (forward-line 2)
            (set-window-start (selected-window) (point) t))
          (goto-char permission-point)
          (set-window-point (selected-window) permission-point))
        (redisplay t)
        (should (> (window-start) (point-min)))
        (should (>= (window-end nil t) (point-max)))
        (let ((point-before (point)))
          (mevedel-view--spinner-tick)
          (should (= point-before (point))))
        (mevedel-view--stop-spinner))))

  :doc "input read excludes request progress fragment"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (goto-char (point-max))
      (insert "/auto")
      (should (equal "/auto" (mevedel-view--input-text)))))

  :doc "ASCII fallback frames can be selected"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-animate nil)
            (mevedel-view-spinner-frames mevedel-view-spinner-ascii-frames)
            (mevedel-view--spinner-frame-index 0))
        (mevedel-view--start-spinner "Working...")
        (let ((text (buffer-substring-no-properties
                     (overlay-start
                      (mevedel-view-zone-region 'progress))
                     (overlay-end
                      (mevedel-view-zone-region 'progress)))))
          (should (string-match-p "- Working" text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner tick updates pending tool frame spans"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (should (string-match-p "- Calling Read"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        (mevedel-view--spinner-tick)
        (let ((frame-pos (text-property-any
                          (point-min) (point-max)
                          'mevedel-view-inline-spinner-frame t)))
          (should frame-pos)
          (should (equal (get-text-property frame-pos 'display) "+"))))))

  :doc "spinner tick does not move point on pending tool frame"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (goto-char (text-property-any
                    (point-min) (point-max)
                    'mevedel-view-inline-spinner-frame t))
        (let ((point-before (point)))
          (mevedel-view--spinner-tick)
          (should (= (point) point-before))))))

  :doc "incremental response keeps request progress row visible"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Partial answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--render-incremental data-buf)
        (should (mevedel-view--request-progress-visible-p))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Partial answer" text))
          (should (string-match-p "Working" text))))))

  :doc "direct data-buffer request begin starts request progress"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "direct-progress"
                       :root temporary-file-directory
                       :name "direct-progress"))
           (session (mevedel-session-create "main" workspace))
           position fsm)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at (current-time)))
        (setq position (copy-marker (point-max) nil))
        (setq fsm (gptel-make-fsm
                   :info (list :buffer data-buf :position position))))
      (with-current-buffer view-buf
        (setq mevedel-view--request-progress-suppressed t))
      (mevedel-view-stream-ensure-progress-for-fsm fsm)
      (with-current-buffer view-buf
        (should-not mevedel-view--request-progress-suppressed)
        (should (mevedel-view--request-progress-visible-p))
        (should (markerp mevedel-view--data-turn-start))
        (should (= (marker-position mevedel-view--data-turn-start)
                   (marker-position position)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Working" text))))))

  :doc "pre-tool render keeps request progress and adds pending line"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-spinner-frames '("-" "+"))
          (mevedel-view--spinner-frame-index 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (mevedel-view-stream-spinner-hook
         '(:name "Read" :args (:file_path "foo.el")))
        (mevedel-view-stream-pre-tool
         '(:id "call-1" :name "Read" :args (:file_path "foo.el"))))
      (with-current-buffer view-buf
        (should (mevedel-view--request-progress-visible-p))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-inline-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-pending-tool-live t))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Working" text))
          (should (string-match-p "Calling Read: foo.el" text))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Calling Read" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "spinner hook does not duplicate pending tool status in flight"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (with-current-buffer data-buf
      (mevedel-view-stream-spinner-hook
       '(:name "Agent" :args (:subagent_type "explorer"))))
    (with-current-buffer view-buf
      (should-not (mevedel-view--request-progress-visible-p))
      (should-not (string-match-p "Calling Agent"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))

  :doc "Agent pre-tool keeps request progress without duplicate pending line"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-spinner-frames '("-" "+"))
          (mevedel-view--spinner-frame-index 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (mevedel-view-stream-spinner-hook
         '(:name "Agent" :args (:subagent_type "verifier")))
        (mevedel-view-stream-pre-tool
         '(:id "agent-1" :name "Agent"
                :args (:subagent_type "verifier"))))
      (with-current-buffer view-buf
        (should (mevedel-view--request-progress-visible-p))
        (should-not mevedel-view--pending-tool-calls)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Working" text))
          (should-not (string-match-p "Calling Agent" text))))))
)


;;
;;; Final response rendering

(mevedel-deftest mevedel-view-stream-render-response ()
  ,test
  (test)
  :doc "renders user + assistant turn"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "*** Hello world\n" nil)
    (mevedel-view-stream-test--insert-data data-buf "Hi! How can I help?\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Hello world" text))
        (should (string-match-p "How can I help" text)))))

  :doc "clears stale compaction lock on final response"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "*** Hello world\n" nil)
    (mevedel-view-stream-test--insert-data data-buf "Hi!\n" 'response)
    (with-current-buffer data-buf
      (setq-local mevedel--compaction-in-flight t)
      (mevedel-view-stream-render-response (point-min) (point-max))
      (should-not mevedel--compaction-in-flight)))

  :doc "final response renders durable worked footer and hides side channel"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "worked-footer"
                       :root temporary-file-directory
                       :name "worked-footer"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 390)))
           data-turn-start response-start response-end)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil))
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--start-spinner "Thinking..."))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start response-end)
        (should (string-search "request-summary"
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
      (with-current-buffer view-buf
        (should-not (mevedel-view--request-progress-visible-p))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Done" text))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "Working\\.\\.\\." text))
          (should-not (string-match-p "mevedel-render-data" text)))
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "mevedel-render-data" text))))))

  :doc "final response moves stale request summary after later streamed text"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "worked-footer-tail"
                       :root temporary-file-directory
                       :name "worked-footer-tail"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 9)))
           response-start)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "```python\nprint('one')\n```\n"
       'response)
      (mevedel-view-stream-test--insert-data
       data-buf
       (mevedel-pipeline--format-render-data-block
        '(:kind request-summary :elapsed-seconds 4))
       'ignore)
      (mevedel-view-stream-test--insert-data
       data-buf
       "```javascript\nconsole.log('two');\n```\n"
       'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start (point-max)))
      (with-current-buffer data-buf
        (let* ((data (buffer-substring-no-properties
                      (point-min) (point-max)))
               (summary (string-match-p "request-summary" data))
               (javascript (string-match-p "```javascript" data)))
          (should summary)
          (should javascript)
          (should (> summary javascript))
          (should-not (string-match-p ":elapsed-seconds 4" data))))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (worked (string-match-p "Worked for" text))
               (javascript (string-match-p "javascript ⧉" text)))
          (should javascript)
          (should worked)
          (should (> worked javascript))
          (should-not (string-match-p "mevedel-render-data" text))))))

  :doc "renders tool calls as one-liners"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-stream-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should-not (string-match-p "file content" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "full rerender preserves tool result rewrite audit details"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     (concat
      "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\n"
      "updated result"
      (mevedel--format-hook-audit-record
       '(:type tool-result-rewrite
               :event "PostToolUse"
               :original-result "original result"
               :updated-result "updated result"
               :reason "redacted"))
      "\n")
     '(tool . "call_1"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "full rerender preserves ignored tool result audit segments"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nupdated")
        (put-text-property start (point) 'gptel '(tool . "call_1")))
      (let ((start (point)))
        (insert
         (mevedel--format-hook-audit-record
          '(:type tool-result-rewrite
                  :event "PostToolUse"
                  :original-result "original result"
                  :updated-result "updated result"
                  :reason "redacted")))
        (put-text-property start (point) 'gptel 'ignore))
      (let ((start (point)))
        (insert " result\n")
        (put-text-property start (point) 'gptel '(tool . "call_1"))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "decorates agent-result blocks inside assistant responses"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     (concat "Review update.\n"
             "<agent-result agent-id=\"reviewer--abc\" type=\"reviewer\">\n"
             "{\"findings\":[]}\n"
             "</agent-result>\n"
             "Final answer.\n")
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Review update" text))
        (should (string-match-p "✓ finished reviewer--abc" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should (string-match-p "Final answer" text))
        (should-not (string-match-p "<agent-result" text)))
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✓ finished reviewer--abc" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should-not (string-match-p "<agent-result" text)))))

  :doc "renders completed Markdown source blocks as view panels"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "Here is `code`:\n```emacs-lisp\n(message \"hi\")\n```\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer data-buf
      (should (string-match-p
               "```emacs-lisp"
               (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Here is `code`" text))
        (should (string-match-p "emacs-lisp ⧉" text))
        (should (string-match-p "(message \"hi\")" text))
        (should-not (string-match-p "```emacs-lisp" text))
        (should-not (string-match-p "#\\+begin_src" text)))))

  :doc "keeps incomplete streaming Markdown source blocks raw"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "```emacs-lisp\n(message \"hi\")\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "```emacs-lisp" text))
        (should (string-match-p "(message \"hi\")" text)))))

  :doc "assistant prose file line reference is buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-line-" t))
         (file (file-name-concat root "mevedel-session-persistence.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-line"
                     :root root :name "response-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "See mevedel-session-persistence.el:187.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "mevedel-session-persistence.el:187")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 187 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "assistant inline code file line reference is buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-inline-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-inline-line"
                     :root root :name "response-inline-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "inline\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "See `file.el:42`.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 42 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "assistant source block file line reference is not buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-src-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-src-line"
                     :root root :name "response-src-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "src\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "```emacs-lisp\nfile.el:42\n```\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (should-not (button-at (match-beginning 0))))))
      (delete-directory root t)))

  :doc "expanded assistant response preserves file line buttons"
  (let* ((root (make-temp-file "mevedel-view-response-expand-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-expand-line"
                     :root root :name "response-expand-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "expand\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "See file.el:42.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (goto-char (match-beginning 0))
              (mevedel-view-toggle-section)
              (goto-char (point-min))
              (search-forward "See")
              (goto-char (match-beginning 0))
              (mevedel-view-toggle-section)
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 42 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "renders bracket indexing literally inside response code blocks"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "```r\neval(f[[3]], df)\n```\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "r ⧉" text))
        (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
        (should-not (string-match-p "```r" text))
        (should-not (string-match-p "eval(f3, df)" text)))
      (let ((pos (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "[[3]]" mevedel-view--input-marker t)
                     (match-beginning 0)))))
        (should pos)
        (should-not (get-text-property pos 'htmlize-link))
        (should-not (get-text-property pos 'help-echo))
        (should-not (get-text-property pos 'mouse-face)))))

  :doc "does not render spurious user turn for gptel tool scaffolding"
  ;; gptel inserts `#+begin_tool ... ' and `#+end_tool' around the
  ;; propertised tool content with no `gptel' property, so the
  ;; separator text between a user prompt and the tool content shows
  ;; up as a `user' segment.  Must not render as a second "You" turn.
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "\n\n#+begin_tool (Read :file_path \"/tmp/test.el\")\n"
     nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-stream-test--insert-data data-buf "\n#+end_tool\n" nil)
    (mevedel-view-stream-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let* ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker))
             (you-count (cl-count-if (lambda (line) (string= line "You"))
                                     (split-string text "\n"))))
        (should (= 0 you-count))
        (should-not (string-match-p "#\\+begin_tool" text))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "restored stale tool bounds render and expand without garbled fragments"
  (mevedel-view-stream-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "RecoverRead"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "RecoverRead: /tmp/f"
                        :body result
                        :initially-collapsed-p t))))
    (with-current-buffer data-buf
      (let (block-start block-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property (+ block-start 20) (- block-end 12)
                           'gptel '(tool . "call_1"))
        (insert "Fixed the byte-compilation warning.\n")
        (save-excursion
          (search-backward "ation warning")
          (setq response-start (point)))
        (put-text-property response-start (point) 'gptel 'response))
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "RecoverRead: /tmp/f" text))
        (should (string-match-p
                 "Fixed the byte-compilation warning" text))
        (should-not (string-match-p "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text))
        (should-not (string-match-p "^You$\\|Thinking" text)))
      (goto-char (point-min))
      (search-forward "RecoverRead: /tmp/f")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "file body" text))
        (should-not (string-match-p
                     "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text)))))

  :doc "renders repeated read calls as individual tool rows"
  (mevedel-view-stream-test--with-buffers
    (dotimes (i 4)
      (mevedel-view-stream-test--insert-data
       data-buf
       (format "(:name \"Read\" :args (:file_path \"/tmp/file%d.el\"))\n\ncontent %d\n"
               i i)
       `(tool . ,(format "call_%d" i))))
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p "Reading 4 files" text))
        (dolist (file '("file0.el" "file1.el" "file2.el" "file3.el"))
          (should (string-match-p
                   (format "Read: .*%s" (regexp-quote file))
                   text))))))

  :doc "renders user turn when in-flight marker outlives echoed user block"
  (mevedel-view-stream-test--with-buffers
    (let (start end)
      (mevedel-view-stream-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max))
        (setq start (point-max)))
      (mevedel-view-stream-test--insert-data data-buf "*** Second\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response start end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Second" text))
          (should (string-match-p "Second response" text))))))

  :doc "does not duplicate visible send-path user echo after flag is consumed"
  (mevedel-view-stream-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max)))
      (mevedel-view-stream-test--insert-data data-buf "*** Second\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Second")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response start end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Second response" text))))))

  :doc "final response includes reasoning before hook start"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start response-start response-end)
      (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (mevedel-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nroot cause thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start response-end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Bash" text))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))
          (should-not (string-match-p "root cause thought" text)))
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "root cause thought" text))))))

  :doc "final response widening does not duplicate visible user echo"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start response-start response-end)
      (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nlate thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start response-end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))))))

  :doc "second-turn incremental render stays above interaction zone without duplication"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max))
        (goto-char (point-max))
        (insert "\n\n*** Second\n")
        (setq data-turn-start (copy-marker (point) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--interaction-register
         (list :kind 'permission
               :id 'permission
               :count 1
               :body "\npermission\n"
               :keymap (make-sparse-keymap)
               :help-echo "Permission"
               :entry 'permission-entry
               :activate #'ignore))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "Second"))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--render-incremental data-buf)
        (mevedel-view--render-incremental data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n")))
               (partial-count
                (cl-loop with start = 0
                         while (string-match "Partial response" text start)
                         count t
                         do (setq start (match-end 0))))
               (second-pos (string-match "You\nSecond" text))
               (partial-pos (string-match "Partial response" text)))
          (should (= 2 assistant-count))
          (should (= 1 partial-count))
          (should second-pos)
          (should partial-pos)
          (should (< second-pos partial-pos))))))

  :doc "incremental render suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start
          (changes 0))
      (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--pending-tool-calls
              (list (cons 'read "Calling Read…")))
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--render-incremental data-buf)
        (should (= 0 changes))
        (should (string-match-p
                 "Partial response"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))

  :doc "does not duplicate the original user turn after mailbox insertion"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        ;; Simulate the real send-path marker landing inside the nil
        ;; user-property run, which `--extract-segments' expands
        ;; backward to the beginning of the prompt.
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Thinking\n" 'ignore)
      (mevedel-view-stream-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (insert "✉ from explorer\nhi\n\n")
                (setq mevedel-view--in-flight-turn-start
                      (copy-marker (point) nil))
                (insert "Assistant\nold live tail\n")
                (set-marker mevedel-view--status-marker (point))
                (set-marker mevedel-view--interaction-marker (point))
                (set-marker mevedel-view--input-marker (point)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--user-pre-rendered nil)
        (mevedel-view--render-incremental data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Assistant text" text))))))

  :doc "renders thinking blocks as summaries"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "line 1\nline 2\nline 3\n" 'ignore)
    (mevedel-view-stream-test--insert-data data-buf "The answer is 42.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Thinking" text))
        (should-not (string-match-p "line 1" text))
        (should (string-match-p "42" text)))))

  :doc "trims contaminated thinking source to structural reasoning block"
  (mevedel-view-stream-test--with-buffers
    (let (reasoning-start)
      (mevedel-view-stream-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (mevedel-view-stream-test--insert-data
       data-buf
       "**Output observed:**\n  `ok`\n\nVERDICT: PASS\n</agent-result>\n"
       nil)
      (with-current-buffer data-buf
        (setq reasoning-start (point)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nreal thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (let ((source (get-text-property (point) 'mevedel-view-source))
              (line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (should (equal (car source) reasoning-start))
          (should (string-match-p "Thinking\\.\\.\\. (1 lines)" line)))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "real thought" text))
          (should-not (string-match-p "VERDICT: PASS" text))
          (should-not (string-match-p "</agent-result>" text))))))

  :doc "hides proposed-plan wrappers outside Goal planning"
  (mevedel-view-stream-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "test"
                    :workspace nil
                    :permission-mode 'ask)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-stream-test--insert-data
       data-buf
       "Normal\n<proposed_plan>\n# Plan\n</proposed_plan>\nAfter\n"
       'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "<proposed_plan>" text))
          (should (string-match-p "# Plan" text))))))

  :doc "strips proposed-plan tags from visible Goal planning responses"
  (mevedel-view-stream-test--with-buffers
    (let ((session
           (mevedel-session--create
            :name "test"
            :workspace nil
            :permission-mode 'ask
            :goal (mevedel-goal--create
                   :status 'active
                   :phase 'planning))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-stream-test--insert-data
       data-buf
       "Normal\n<proposed_plan>\n# Plan\n</proposed_plan>\nAfter\n"
       'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "<proposed_plan>" text))
	  (should-not (string-match-p "# Plan" text))
	  (should (string-match-p "Normal" text))
	  (should (string-match-p "After" text))))))

  :doc "strips an incomplete proposed-plan block during Goal planning"
  (mevedel-view-stream-test--with-buffers
    (let ((session
           (mevedel-session--create
            :name "test"
            :workspace nil
            :permission-mode 'ask
            :goal (mevedel-goal--create
                   :status 'active
                   :phase 'planning))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (should (equal "Normal"
                       (mevedel-view--visible-response-text
                        "Normal\n<proposed_plan>\n# Streaming\n"))))))

  :doc "keeps historical proposed-plan protocol hidden after Goal planning"
  (mevedel-view-stream-test--with-buffers
    (let* ((tmp (make-temp-file "mevedel-view-plan-" t))
           (plan-path (file-name-concat tmp "plans" "current.md"))
           (old-plan-hash
            (mevedel-plan-hash "# Old plan\n"))
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :save-path tmp
                     :permission-mode 'ask
                     :plan-metadata
                     (list :path "plans/current.md"
                           :status 'approved
                           :presented-plan-hashes
                           (list old-plan-hash)))))
      (unwind-protect
          (progn
            (make-directory (file-name-directory plan-path) t)
            (write-region "# Current plan\n" nil plan-path nil 'silent)
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "Normal\n<proposed_plan>\n# Old plan\n</proposed_plan>\nAfter\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should-not (string-match-p "<proposed_plan>" text))
                (should-not (string-match-p "# Old plan" text))
                (should (string-match-p "Normal" text))
                (should (string-match-p "After" text)))))
        (delete-directory tmp t))))

  :doc "renders ignored directive PROMPT drawer as collapsed user section"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "*** Change alpha :implement:\n" nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     ":PROMPT:\n## TASK\nFull hidden prompt.\n:END:\n"
     'ignore)
    (mevedel-view-stream-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Implement: Change alpha" text))
        (should-not (string-match-p "Change alpha :implement:" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "Full hidden prompt" text))
        (should (string-match-p "Done" text)))
      (goto-char (point-min))
      (search-forward "Implement:")
      (should (eq 'mevedel-view-directive-action
                  (get-text-property (match-beginning 0)
                                     'font-lock-face)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Full hidden prompt" expanded)))))

  :doc "renders inline skills as compact invocation with collapsed prompt"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "*** You are helping with this user request:\n\nSay hi!\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (mevedel-pipeline--format-render-data-block
          '(:kind inline-skill
                  :name "emacs-context-snapshot"
                  :arguments "Say hi!"
                  :display-text
                  "$emacs-context-snapshot\nSay hi!")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p
                 "\\$emacs-context-snapshot\nSay hi!"
                 text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "You are helping with this user request"
                                    text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You are helping with this user request"
                                expanded)))))

  :doc "renders nil-property inline skill metadata as compact invocation"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     (concat
      "*** # Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
      (mevedel-pipeline--format-render-data-block
       '(:kind inline-skill
               :name "green-loop"
               :arguments "current changes"
               :display-text "$green-loop current changes")))
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "\\$green-loop current changes" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "Run the loop" text))
        (should-not (string-match-p "mevedel-render-data" text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "# Green Loop" expanded))
        (should (string-match-p "Run the loop" expanded)))))

  :doc "request summary before inline skill does not absorb user turn"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "Previous answer.\n" 'response)
    (mevedel-view-stream-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind request-summary :elapsed-seconds 120))
     'ignore)
    (mevedel-view-stream-test--insert-data
     data-buf
     "*** # Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
     nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind inline-skill
              :name "green-loop"
              :arguments "current changes"
              :display-text "$green-loop current changes"))
     'ignore)
    (mevedel-view-stream-test--insert-data data-buf "Restarting checks.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Previous answer" text))
        (should (string-match-p "Worked for 2m" text))
        (should (string-match-p "You\n\\$green-loop current changes" text))
        (should (string-match-p "Restarting checks" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "mevedel-render-data" text)))))

  :doc "expanded inline skill prompt omits saved org property drawer"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     ":PROPERTIES:\n:GPTEL_PRESET: mevedel-implement\n:GPTEL_MODEL: gpt-5.5\n:GPTEL_BOUNDS: ((ignore (1 2)))\n:END:\n\n*** Skill prompt body\n\nVisible model prompt.\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (mevedel-pipeline--format-render-data-block
          '(:kind inline-skill
                  :name "green-loop"
                  :arguments "commits a b"
                  :display-text
                  "$green-loop\ncommits a b")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Visible model prompt" expanded))
        (should-not (string-match-p ":PROPERTIES:" expanded))
        (should-not (string-match-p "GPTEL_BOUNDS" expanded)))))

  :doc "expanded external Prompt survives in-flight incremental render"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "*** Change alpha :implement:\n" nil)
      (mevedel-view-stream-test--insert-data
       data-buf
       ":PROMPT:\n## TASK\nFull hidden prompt.\n:END:\n"
       'ignore)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (mevedel-view--begin-external-turn
         "Implement: Change alpha" data-turn-start 'directive)
        (goto-char (point-min))
        (search-forward "Prompt")
        (mevedel-view-toggle-section)
        (should (string-match-p
                 "Full hidden prompt"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))
      (mevedel-view-stream-test--insert-data data-buf "Assistant answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Full hidden prompt" text))
          (should (string-match-p "Assistant answer" text)))))))

(provide 'test-mevedel-view-stream)
;;; test-mevedel-view-stream.el ends here
