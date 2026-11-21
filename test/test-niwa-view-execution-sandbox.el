;;; test-niwa-view-execution-sandbox.el --- Sandbox rows -*- lexical-binding: t -*-

;;; Commentary:
;; Real confined services and call records projected through both user surfaces.

;;; Code:

(require 'helpers (file-name-concat
                  (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "niwa-execution-test-helpers"))
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-execution)
(require 'niwa-background-list)
(require 'niwa-cockpit)
(require 'niwa-eval-sandbox)
(require 'niwa-execution)
(require 'niwa-sandbox)
(require 'niwa-session-artifacts)

(defun niwa-view-sandbox-test--draft-offset ()
  "Return the caret offset from the draft start, read in the composer buffer."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(defmacro niwa-view-sandbox-test--fixture (&rest body)
  "Run BODY with a real SESSION, ROOT, paired buffers and isolated sandboxes."
  (declare (indent 0) (debug t))
  `(niwa-view-test--with-buffers
     (let* ((root (make-temp-file "sandbox-row-" t))
            (session (test-niwa-execution--session root))
            (niwa-eval-sandbox--sessions (make-hash-table :test #'eq))
            (niwa-eval-sandbox-idle-timeout nil)
            (initial-timers (copy-sequence timer-list)))
       (unwind-protect
           (save-window-excursion
             (unless (plist-get (niwa-sandbox-probe) :available)
               (ert-skip "No confinement backend"))
             (with-current-buffer data-buf (setq-local niwa--session session))
             (setf (niwa-session-root-buffer session) data-buf)
             (niwa-session-artifacts-assert-new-mutation-authority session)
             (setq initial-timers (copy-sequence timer-list))
             (switch-to-buffer view-buf)
             (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                       ((symbol-function 'mouse-pixel-position)
                        (lambda () (cons (selected-frame) '(-1 . -1)))))
               ,@body))
         (niwa-execution-teardown-session session)
         (when-let* ((buffer (get-buffer niwa-background-list-buffer-name)))
           (kill-buffer buffer))
         (delete-directory root t)))))

(defun niwa-view-sandbox-test--call (session root data expression &optional wait yield-ms)
  "Start EXPRESSION in SESSION at ROOT from DATA with YIELD-MS delay.
WAIT for settlement if non-nil."
  (let (done)
    (with-current-buffer data
      (niwa-execution-start-service-call
       #'ignore
       (lambda (call)
         (niwa-eval-sandbox-call
          session "/root" expression
          (lambda (reply)
            (setq done t)
            (niwa-execution-service-call-settle
             call (list :output (or (plist-get reply :value) "ended") :outcome 'success)))
          :call call :workdir root :writable-roots (list root)))
       :session session :owner "/root" :data-buffer data :workdir root
       :yield-time-ms (or yield-ms 10000)))
    (when wait (test-niwa-execution--wait (lambda () done) 10))))

(niwa-deftest niwa-view-execution--sandbox-line ()
  (niwa-view-sandbox-test--fixture ,test)
  (test)
  :doc "idle rows are detached, inert, navigable and stoppable without moving the draft"
  (progn
    (should-not (niwa-eval-sandbox-any-live-p session))
    (should-not (niwa-eval-sandbox-list-idle session nil))
    (should (= 0 (hash-table-count niwa-eval-sandbox--sessions)))
    (cl-letf (((symbol-function 'niwa-eval-sandbox-list-idle)
               (lambda (&rest _) (ert-fail "Empty registry must not enumerate"))))
      (should-not (niwa-view-execution-fragment session)))
    (niwa-view-test--insert-composer-draft "> draft\nsecond line")
    (let ((offset (niwa-view-sandbox-test--draft-offset))
          (window (selected-window)) (start (window-start)))
      (niwa-view-sandbox-test--call session root data-buf "t" t)
      (should (niwa-eval-sandbox-any-live-p session))
      (should (= offset (niwa-view-sandbox-test--draft-offset)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
      (should (eq window (selected-window)))
      (should (= start (window-start))))
    (let* ((rows (niwa-eval-sandbox-list-idle session nil))
           (row (car rows)) (line (niwa-view-execution--sandbox-line session row)))
      (should (= 1 (length rows)))
      (should (string-match-p "Idle.*sandbox.* /root.*[0-9]+s.*[0-9]+ features.*Stop" line))
      (should-not (text-property-not-all 0 (length line) 'niwa-view-background-elapsed nil line))
      (aset (plist-get row :owner) 1 ?X)
      (should (equal "/root" (plist-get (car (niwa-eval-sandbox-list-idle session nil)) :owner))))
    (should-not (niwa-execution-list session "/root"))
    (should (= 0 (niwa-execution-count-user session)))
    (should-not (niwa-execution-list-user session))
    (should-not (cl-remove-if-not #'timer--repeat-delay
                                  (cl-set-difference timer-list initial-timers)))
    (let ((timers (copy-sequence timer-list)) (renders 0)
          (render (symbol-function 'niwa-view--render-status)))
      (cl-letf (((symbol-function 'niwa-view--render-status)
                 (lambda (&rest args) (cl-incf renders) (apply render args))))
        (accept-process-output nil 0.2))
      (should (= 0 renders))
      (should-not (cl-set-difference timer-list timers))
      (should-not niwa-view-execution--elapsed-targets)
      (should-not niwa-view-execution--release-timer))
    (goto-char (point-min))
    (search-forward "Stop")
    (call-interactively (key-binding (kbd "RET")))
    (should-not (niwa-eval-sandbox-any-live-p session))
    (should-not (niwa-eval-sandbox-list-idle session nil))
    (should-not (niwa-view-execution-fragment session)))
  :doc "first foreground admission is visible while real preparation is pending"
  (let ((prepare (symbol-function 'niwa-execution--prepare)) pending)
    (cl-letf (((symbol-function 'niwa-execution--prepare)
               (lambda (&rest args) (setq pending args) #'ignore)))
      (niwa-view-sandbox-test--call session root data-buf "t"))
    (should pending)
    (should (niwa-eval-sandbox-any-live-p session))
    (should (string-match-p "Running.*eval.*sandbox" (buffer-string)))
    (should-not (niwa-eval-sandbox-list-idle session nil))
    (let ((calls (niwa-execution-list-user session)))
      (should (= 1 (length calls)))
      (should-not (plist-get (car calls) :yielded)))
    (apply prepare pending)
    (test-niwa-execution--wait
     (lambda () (niwa-eval-sandbox-list-idle session nil)) 10)
    (should (string-search "Idle" (buffer-string)))
    (niwa-execution-stop-all-user session)
    (should-not (niwa-eval-sandbox-any-live-p session)))
  :doc "foreground and yielded calls replace idle rows once, then restore one idle row"
  (progn
    (niwa-view-sandbox-test--call session root data-buf "t" t)
    (goto-char (point-min))
    (search-forward "Idle")
    (niwa-view-sandbox-test--call session root data-buf "(sleep-for 30)" nil 1)
    (should (niwa-eval-sandbox-any-live-p session))
    (let* ((entries (niwa-view-execution-collect session))
           (row (nth 2 (car entries)))
           (body (plist-get (niwa-view-execution-fragment session) :body))
           (context (list :session session)))
      (should (= 1 (length entries)))
      (should (eq 'execution (caaar entries)))
      (should (eq 'execution (nth 1 (car entries))))
      (should (eq 'sandbox (plist-get row :eval-mode)))
      (should (string-match-p "Running.*eval.*sandbox" body))
      (should (string-match-p "Running.*eval.*sandbox" (buffer-string)))
      (should-not (string-search "Idle" body))
      (let ((items (niwa-background-list--collect context)))
        (should (= 1 (length items)))
        (should (string-search "persistent sandbox"
                               (aref (cadr (niwa-background-list--entry (car items) context)) 5)))))
    (test-niwa-execution--wait (lambda () (niwa-execution-list session "/root")))
    (should (= 1 (length (niwa-view-execution-collect session))))
    (niwa-execution-stop-all-user session)
    (should-not (niwa-eval-sandbox-any-live-p session))
    (should-not (niwa-view-execution-fragment session))
    (niwa-view-sandbox-test--call session root data-buf "t" t)
    (should (string-search "Idle" (plist-get (niwa-view-execution-fragment session) :body)))
    (should-not niwa-view-execution--held))
  :doc "cockpit Stop and idle expiry remove rows through the death notification"
  (progn
    (niwa-view-sandbox-test--call session root data-buf "t" t)
    (with-current-buffer
        (niwa-background-list-open
         (list :session session :view-buffer view-buf :data-buffer data-buf :origin-buffer view-buf))
      (should (= 1 (length tabulated-list-entries)))
      (let ((row (cadr (car tabulated-list-entries))))
        (should (equal "sandbox" (aref row 0)))
        (should (equal "/root" (aref row 3)))
        (should (string-match-p "[0-9]+s" (aref row 4)))
        (should (string-match-p "[0-9]+ features" (aref row 5))))
      (niwa-background-list-stop)
      (should-not tabulated-list-entries))
    (switch-to-buffer view-buf)
    (niwa-view-test--insert-composer-draft "> draft\nsecond line")
    (let ((niwa-eval-sandbox-idle-timeout 0.1)
          (offset (niwa-view-sandbox-test--draft-offset))
          (window (selected-window)) (start (window-start)))
      (niwa-view-sandbox-test--call session root data-buf "t" t)
      (should (string-search "Idle" (with-current-buffer view-buf (buffer-string))))
      (test-niwa-execution--wait
       (lambda () (not (niwa-eval-sandbox-list-idle session nil))))
      (with-current-buffer view-buf
        (should-not (string-search "Idle" (buffer-string)))
        (should-not niwa-view-execution--release-timer)
        (should (= offset (niwa-view-sandbox-test--draft-offset)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
        (should (eq window (selected-window)))
        (should (= start (window-start))))
      (with-current-buffer (get-buffer niwa-background-list-buffer-name)
        (should-not tabulated-list-entries)))))

(provide 'test-niwa-view-execution-sandbox)
;;; test-niwa-view-execution-sandbox.el ends here
