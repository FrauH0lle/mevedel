;;; test-mevedel-gptel-stream-bridge.el -- gptel stream bridge tests -*- lexical-binding: t -*-

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
(require 'mevedel-gptel-stream-bridge)
(require 'mevedel-structs)

(defmacro mevedel-gptel-stream-bridge-test--with-data-buffer (&rest body)
  "Create one mevedel data buffer, then evaluate BODY."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *mevedel-stream-bridge-data*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode)
             (setq-local mevedel--session
                         (mevedel-session--create :name "stream-bridge")))
           ,@body)
       (when (buffer-live-p data-buf)
         (kill-buffer data-buf)))))

(defun mevedel-gptel-stream-bridge-test--count-substring (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0)
        position)
    (while (setq position (string-search needle text start))
      (cl-incf count)
      (setq start (+ position (length needle))))
    count))

;;
;;; Advice lifecycle

(mevedel-deftest mevedel-gptel-stream-bridge-install ()
  ,test
  (test)
  :doc "install enables advice and deferred installers honor that state"
  (let ((mevedel-gptel-stream-bridge--gptel-stream-advice-installed nil)
        (calls 0))
    (cl-letf (((symbol-function 'mevedel-gptel-stream-bridge--install-advice)
               (lambda () (cl-incf calls))))
      (mevedel-gptel-stream-bridge-install)
      (should mevedel-gptel-stream-bridge--gptel-stream-advice-installed)
      (should (> calls 0))
      (mevedel-gptel-stream-bridge-uninstall)
      (should-not mevedel-gptel-stream-bridge--gptel-stream-advice-installed))))

(mevedel-deftest mevedel-gptel-stream-bridge--repair-gptel-stream-info ()
  ,test
  (test)
  :doc "reanchors detached position markers for mevedel stream callbacks"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
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
       (should (eq (mevedel-gptel-stream-bridge--repair-gptel-stream-info info) info))
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
        (mevedel-gptel-stream-bridge--repair-gptel-stream-info info)
        (should (eq (plist-get info :position) position))
        (should-not (marker-position position))))))

(mevedel-deftest mevedel-gptel-stream-bridge--gptel-handle-wait-advice ()
  ,test
  (test)
  :doc "keeps an open reasoning fence across a tool continuation request"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let* ((mevedel-gptel-stream-bridge-insert-batch-delay nil)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil))
                      :include-reasoning 'ignore))
          (fsm (gptel-make-fsm :info info)))
     (gptel--display-reasoning-stream "thinking" info)
     ;; Current gptel tracks whether its rendered fence is still open.
     (plist-put info :reasoning-open t)
     (should (plist-get info :reasoning-open))
     (should (mevedel-gptel-stream-bridge--gptel-stream-info-p info))
     (should
      (eq (mevedel-gptel-stream-bridge--gptel-handle-wait-advice
           (lambda (_fsm)
             (plist-put info :reasoning-open nil)
             'continued)
           fsm)
          'continued))
     (should (plist-get info :reasoning-open))
     (gptel--display-reasoning-stream t info)
     (with-current-buffer data-buf
       (let ((text (buffer-string)))
         (should
          (= 1 (mevedel-gptel-stream-bridge-test--count-substring
                "#+begin_reasoning" text)))
         (should
          (= 1 (mevedel-gptel-stream-bridge-test--count-substring
                "#+end_reasoning" text))))))))

(mevedel-deftest mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice ()
  ,test
  (test)

  :doc "repairs detached markers before delegating to gptel streaming"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let ((position (copy-marker 1 nil))
         (mevedel-gptel-stream-bridge-insert-batch-delay nil))
     (set-marker position nil)
     (let ((info (list :buffer data-buf :position position)))
       (should
        (eq (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
             (lambda (_response callback-info &optional _raw)
               (let ((marker (plist-get callback-info :position)))
                 (and (markerp marker)
                      (eq (marker-buffer marker) data-buf)
                      (marker-position marker)
                      'delegated)))
             "chunk"
             info)
            'delegated)))))

  :doc "mevedel stream insertion suppresses after-change hooks but keeps post-stream hook"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let ((after-change-ran nil)
         (post-stream-ran nil)
         (mevedel-gptel-stream-bridge-insert-batch-delay nil)
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
     (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
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
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (mevedel-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "a" info)
       (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "b" info)
       (should-not calls)
       (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
       (should (equal '(("ab" nil)) calls)))))

  :doc "batching flushes pending strings before non-string events"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (mevedel-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "text" info)
       (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig '(tool-call . nil) info)
       (should (equal '(((tool-call) nil) ("text" nil)) calls)))))

  :doc "batching flushes when raw insertion mode changes"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (mevedel-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "raw" info t)
       (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "normal" info nil)
       (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
       (should (equal '(("normal" nil) ("raw" t)) calls)))))

  :doc "batching flushes before stream cleanup"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let* ((process (make-pipe-process
                    :name "mevedel-test-stream-cleanup"))
          (calls nil)
          (mevedel-gptel-stream-bridge-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil))))
          (fsm (gptel-make-fsm :info info))
          (gptel--request-alist (list (cons process (cons fsm #'ignore)))))
     (unwind-protect
         (cl-labels ((orig (response _info &optional raw)
                       (push (list response raw) calls)))
           (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
            #'orig "a" info)
           (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
            #'orig "b" info)
           (mevedel-gptel-stream-bridge--gptel-stream-cleanup-advice
            (lambda (_process status)
              (push (list 'cleanup status) calls))
            process "finished")
           (should (equal '((cleanup "finished") ("ab" nil)) calls)))
       (when (process-live-p process)
         (delete-process process)))))

  :doc "batching drops stale batches after stream buffer teardown"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let ((calls nil)
         (mevedel-gptel-stream-bridge-insert-batch-delay 10)
         (info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil)))))
     (cl-labels ((orig (response _info &optional raw)
                   (push (list response raw) calls)))
       (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
        #'orig "stale" info)
       (kill-buffer data-buf)
       (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
       (should-not calls))))

  :doc "nil or zero batch delay preserves immediate insertion"
  (dolist (delay '(nil 0))
    (mevedel-gptel-stream-bridge-test--with-data-buffer
     (let ((calls nil)
           (mevedel-gptel-stream-bridge-insert-batch-delay delay)
           (info (list :buffer data-buf
                       :position (with-current-buffer data-buf
                                   (copy-marker (point-max) nil)))))
       (cl-labels ((orig (response _info &optional raw)
                     (push (list response raw) calls)))
         (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
          #'orig "a" info)
         (mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
          #'orig "b" info)
         (should (equal '(("b" nil) ("a" nil)) calls)))))))

(mevedel-deftest mevedel-gptel-stream-bridge--wrap-gptel-stream-transformer ()
  ,test
  (test)
  :doc "stale gptel stream transformer errors return the raw chunk"
  (mevedel-gptel-stream-bridge-test--with-data-buffer
   (let ((info (list :buffer data-buf
                     :position (with-current-buffer data-buf
                                 (copy-marker (point-max) nil))
                     :transformer
                     (lambda (_str)
                       (error "Selecting deleted buffer")))))
     (mevedel-gptel-stream-bridge--repair-gptel-stream-info info)
     (should (plist-get info :mevedel-transformer-wrapped))
     (let (diagnostics)
       (mevedel-test--with-captured-diagnostics diagnostics
                                                (should (equal "raw chunk"
                                                               (funcall (plist-get info :transformer)
                                                                        "raw chunk"))))
       (should (string-match-p "stale gptel stream transformer"
                               diagnostics))))))

(mevedel-deftest mevedel-gptel-stream-bridge--gptel-stream-filter-advice ()
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
                    'mevedel-gptel-stream-bridge--schedule-gptel-stream-filter-flush)
                   (lambda (proc) (setq scheduled proc))))
          (mevedel-gptel-stream-bridge--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: a\n")
          (should (eq scheduled process))
          (should-not calls)
          (should (equal "event: a\n"
                         (process-get
                          process
                          'mevedel-gptel-stream-bridge--pending-output)))
          (setf (alist-get process gptel--request-alist)
                (cons 'fake-fsm #'ignore))
          (mevedel-gptel-stream-bridge--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: b\n")
          (should (equal '("event: a\nevent: b\n") calls))
          (should-not
           (process-get
            process 'mevedel-gptel-stream-bridge--pending-output)))
      (when (process-live-p process)
        (delete-process process)))))

(mevedel-deftest mevedel-gptel-stream-bridge--install-if-enabled ()
  ,test
  (test)
  :doc "deferred installer is a no-op after uninstall disables stream repair"
  (let ((calls 0)
        (mevedel-gptel-stream-bridge--gptel-stream-advice-installed nil))
    (cl-letf (((symbol-function 'mevedel-gptel-stream-bridge--install-advice)
               (lambda () (cl-incf calls))))
      (mevedel-gptel-stream-bridge--install-if-enabled)
      (should (= calls 0))
      (let ((mevedel-gptel-stream-bridge--gptel-stream-advice-installed t))
        (mevedel-gptel-stream-bridge--install-if-enabled))
      (should (= calls 1)))))

(mevedel-deftest mevedel-gptel-stream-bridge-uninstall ()
  ,test
  (test)
  :doc "uninstall disables future deferred installs"
  (let ((mevedel-gptel-stream-bridge--gptel-stream-advice-installed t))
    (cl-letf (((symbol-function 'mevedel-gptel-stream-bridge--uninstall-advice)
               #'ignore))
      (mevedel-gptel-stream-bridge-uninstall))
    (should-not mevedel-gptel-stream-bridge--gptel-stream-advice-installed)))

(provide 'test-mevedel-gptel-stream-bridge)

;;; test-mevedel-gptel-stream-bridge.el ends here
