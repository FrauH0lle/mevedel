;;; test-mevedel-view-execution-sandbox.el --- Sandbox rows -*- lexical-binding: t -*-

;;; Commentary:
;; Real confined services and call records projected through both user surfaces.

;;; Code:

(require 'helpers (file-name-concat
                  (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "mevedel-execution-test-helpers"))
(require 'mevedel-view)
(require 'mevedel-view-execution)
(require 'mevedel-background-list)
(require 'mevedel-cockpit)
(require 'mevedel-eval-sandbox)
(require 'mevedel-execution)
(require 'mevedel-sandbox)
(require 'mevedel-session-artifacts)

(defmacro mevedel-view-sandbox-test--fixture (&rest body)
  "Run BODY with a real SESSION, ROOT, paired buffers and isolated sandboxes."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((root (make-temp-file "sandbox-row-" t))
            (session (test-mevedel-execution--session root))
            (mevedel-eval-sandbox--sessions (make-hash-table :test #'eq))
            (mevedel-eval-sandbox-idle-timeout nil)
            (initial-timers (copy-sequence timer-list)))
       (unwind-protect
           (save-window-excursion
             (unless (plist-get (mevedel-sandbox-probe) :available)
               (ert-skip "No confinement backend"))
             (with-current-buffer data-buf (setq-local mevedel--session session))
             (setf (mevedel-session-root-buffer session) data-buf)
             (mevedel-session-artifacts-assert-new-mutation-authority session)
             (setq initial-timers (copy-sequence timer-list))
             (switch-to-buffer view-buf)
             (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                       ((symbol-function 'mouse-pixel-position)
                        (lambda () (cons (selected-frame) '(-1 . -1)))))
               ,@body))
         (mevedel-execution-teardown-session session)
         (when-let* ((buffer (get-buffer mevedel-background-list-buffer-name)))
           (kill-buffer buffer))
         (delete-directory root t)))))

(defun mevedel-view-sandbox-test--call (session root data expression &optional wait yield-ms)
  "Start EXPRESSION in SESSION at ROOT from DATA with YIELD-MS delay.
WAIT for settlement if non-nil."
  (let (done)
    (with-current-buffer data
      (mevedel-execution-start-service-call
       #'ignore
       (lambda (call)
         (mevedel-eval-sandbox-call
          session "/root" expression
          (lambda (reply)
            (setq done t)
            (mevedel-execution-service-call-settle
             call (list :output (or (plist-get reply :value) "ended") :outcome 'success)))
          :call call :workdir root :writable-roots (list root)))
       :session session :owner "/root" :data-buffer data :workdir root
       :yield-time-ms (or yield-ms 10000)))
    (when wait (test-mevedel-execution--wait (lambda () done) 10))))

(mevedel-deftest mevedel-view-execution--sandbox-line ()
  (mevedel-view-sandbox-test--fixture ,test)
  (test)
  :doc "idle rows are detached, inert, navigable and stoppable without moving the draft"
  (progn
    (should-not (mevedel-eval-sandbox-any-live-p session))
    (should-not (mevedel-eval-sandbox-list-idle session nil))
    (should (= 0 (hash-table-count mevedel-eval-sandbox--sessions)))
    (cl-letf (((symbol-function 'mevedel-eval-sandbox-list-idle)
               (lambda (&rest _) (ert-fail "Empty registry must not enumerate"))))
      (should-not (mevedel-view-execution-fragment session)))
    (goto-char (point-max))
    (insert "> draft\nsecond line")
    (let ((offset (- (point) (mevedel-view--input-start)))
          (window (selected-window)) (start (window-start)))
      (mevedel-view-sandbox-test--call session root data-buf "t" t)
      (should (mevedel-eval-sandbox-any-live-p session))
      (should (= offset (- (point) (mevedel-view--input-start))))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (should (eq window (selected-window)))
      (should (= start (window-start))))
    (let* ((rows (mevedel-eval-sandbox-list-idle session nil))
           (row (car rows)) (line (mevedel-view-execution--sandbox-line session row)))
      (should (= 1 (length rows)))
      (should (string-match-p "Idle.*sandbox.* /root.*[0-9]+s.*[0-9]+ features.*Stop" line))
      (should-not (text-property-not-all 0 (length line) 'mevedel-view-background-elapsed nil line))
      (aset (plist-get row :owner) 1 ?X)
      (should (equal "/root" (plist-get (car (mevedel-eval-sandbox-list-idle session nil)) :owner))))
    (should-not (mevedel-execution-list session "/root"))
    (should (= 0 (mevedel-execution-count-user session)))
    (should-not (mevedel-execution-list-user session))
    (should-not (cl-remove-if-not #'timer--repeat-delay
                                  (cl-set-difference timer-list initial-timers)))
    (let ((timers (copy-sequence timer-list)) (renders 0)
          (render (symbol-function 'mevedel-view--render-status)))
      (cl-letf (((symbol-function 'mevedel-view--render-status)
                 (lambda (&rest args) (cl-incf renders) (apply render args))))
        (accept-process-output nil 0.2))
      (should (= 0 renders))
      (should-not (cl-set-difference timer-list timers))
      (should-not mevedel-view-execution--elapsed-targets)
      (should-not mevedel-view-execution--release-timer))
    (goto-char (point-min))
    (search-forward "Stop")
    (call-interactively (key-binding (kbd "RET")))
    (should-not (mevedel-eval-sandbox-any-live-p session))
    (should-not (mevedel-eval-sandbox-list-idle session nil))
    (should-not (mevedel-view-execution-fragment session)))
  :doc "first foreground admission is visible while real preparation is pending"
  (let ((prepare (symbol-function 'mevedel-execution--prepare)) pending)
    (cl-letf (((symbol-function 'mevedel-execution--prepare)
               (lambda (&rest args) (setq pending args) #'ignore)))
      (mevedel-view-sandbox-test--call session root data-buf "t"))
    (should pending)
    (should (mevedel-eval-sandbox-any-live-p session))
    (should (string-match-p "Running.*eval.*sandbox" (buffer-string)))
    (should-not (mevedel-eval-sandbox-list-idle session nil))
    (let ((calls (mevedel-execution-list-user session)))
      (should (= 1 (length calls)))
      (should-not (plist-get (car calls) :yielded)))
    (apply prepare pending)
    (test-mevedel-execution--wait
     (lambda () (mevedel-eval-sandbox-list-idle session nil)) 10)
    (should (string-search "Idle" (buffer-string)))
    (mevedel-execution-stop-all-user session)
    (should-not (mevedel-eval-sandbox-any-live-p session)))
  :doc "foreground and yielded calls replace idle rows once, then restore one idle row"
  (progn
    (mevedel-view-sandbox-test--call session root data-buf "t" t)
    (goto-char (point-min))
    (search-forward "Idle")
    (mevedel-view-sandbox-test--call session root data-buf "(sleep-for 30)" nil 1)
    (should (mevedel-eval-sandbox-any-live-p session))
    (let* ((entries (mevedel-view-execution-collect session))
           (row (nth 2 (car entries)))
           (body (plist-get (mevedel-view-execution-fragment session) :body))
           (context (list :session session)))
      (should (= 1 (length entries)))
      (should (eq 'execution (caaar entries)))
      (should (eq 'execution (nth 1 (car entries))))
      (should (eq 'sandbox (plist-get row :eval-mode)))
      (should (string-match-p "Running.*eval.*sandbox" body))
      (should (string-match-p "Running.*eval.*sandbox" (buffer-string)))
      (should-not (string-search "Idle" body))
      (let ((items (mevedel-background-list--collect context)))
        (should (= 1 (length items)))
        (should (string-search "persistent sandbox"
                               (aref (cadr (mevedel-background-list--entry (car items) context)) 5)))))
    (test-mevedel-execution--wait (lambda () (mevedel-execution-list session "/root")))
    (should (= 1 (length (mevedel-view-execution-collect session))))
    (mevedel-execution-stop-all-user session)
    (should-not (mevedel-eval-sandbox-any-live-p session))
    (should-not (mevedel-view-execution-fragment session))
    (mevedel-view-sandbox-test--call session root data-buf "t" t)
    (should (string-search "Idle" (plist-get (mevedel-view-execution-fragment session) :body)))
    (should-not mevedel-view-execution--held))
  :doc "cockpit Stop and idle expiry remove rows through the death notification"
  (progn
    (mevedel-view-sandbox-test--call session root data-buf "t" t)
    (with-current-buffer
        (mevedel-background-list-open
         (list :session session :view-buffer view-buf :data-buffer data-buf :origin-buffer view-buf))
      (should (= 1 (length tabulated-list-entries)))
      (let ((row (cadr (car tabulated-list-entries))))
        (should (equal "sandbox" (aref row 0)))
        (should (equal "/root" (aref row 3)))
        (should (string-match-p "[0-9]+s" (aref row 4)))
        (should (string-match-p "[0-9]+ features" (aref row 5))))
      (mevedel-background-list-stop)
      (should-not tabulated-list-entries))
    (switch-to-buffer view-buf)
    (goto-char (point-max))
    (insert "> draft\nsecond line")
    (let ((mevedel-eval-sandbox-idle-timeout 0.1)
          (offset (- (point) (mevedel-view--input-start)))
          (window (selected-window)) (start (window-start)))
      (mevedel-view-sandbox-test--call session root data-buf "t" t)
      (should (string-search "Idle" (with-current-buffer view-buf (buffer-string))))
      (test-mevedel-execution--wait
       (lambda () (not (mevedel-eval-sandbox-list-idle session nil))))
      (with-current-buffer view-buf
        (should-not (string-search "Idle" (buffer-string)))
        (should-not mevedel-view-execution--release-timer)
        (should (= offset (- (point) (mevedel-view--input-start))))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (eq window (selected-window)))
        (should (= start (window-start))))
      (with-current-buffer (get-buffer mevedel-background-list-buffer-name)
        (should-not tabulated-list-entries)))))

(provide 'test-mevedel-view-execution-sandbox)
;;; test-mevedel-view-execution-sandbox.el ends here
