;;; test-mevedel-preview-mode.el --- Tests for mevedel-preview-mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-preview-mode)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-tool-fs)
(require 'mevedel-file-state)
(require 'mevedel-view)
(require 'mevedel-mentions)
(require 'mevedel-skills)
(require 'gptel-agent-tools)

;; Stubs for globals defined in `mevedel-chat' that auto-apply's
;; dependencies reach for during a bare preview-mode test.
(defvar mevedel-plans-directory
  (file-name-concat ".mevedel" "plans"))

(defvar mevedel--diff-preview-buffer-name "*mevedel-diff-preview*")
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defun mevedel-preview-test--make-overlay (buffer &optional props)
  "Create a bare preview overlay in BUFFER with PROPS alist merged in.
The overlay spans one character so it's a legal region to delete on
cleanup."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert "x")
    (let ((ov (make-overlay (1- (point)) (point) nil t)))
      (overlay-put ov 'mevedel-inline-preview t)
      (dolist (kv props)
        (overlay-put ov (car kv) (cdr kv)))
      ov)))

(defmacro mevedel-preview-test--with-view-buffers (&rest body)
  "Run BODY with data/view buffers and a temporary workspace."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-preview-view-" t))
          (workspace (mevedel-workspace-get-or-create
                      'project root root "preview-view"))
          (data-buf (generate-new-buffer " *preview-data*"))
          (view-buf (generate-new-buffer " *preview-view*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (fundamental-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local mevedel--workspace workspace)
             (setq-local mevedel--session
                         (mevedel-session-create "foo" workspace)))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf)
         (kill-buffer view-buf))
       (when (buffer-live-p data-buf)
         (kill-buffer data-buf))
       (when (file-directory-p root)
         (delete-directory root t))
       (mevedel-workspace-clear-registry))))


;;
;;; Minor mode lifecycle

(mevedel-deftest mevedel-preview-mode ()
  ,test
  (test)
  :doc "registering the first overlay activates the mode"
  (with-temp-buffer
    (should-not mevedel-preview-mode)
    (let ((ov (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register ov)
      (should mevedel-preview-mode)
      (should (equal (list ov) mevedel-preview-mode--pending))))
  :doc "unregistering the last overlay deactivates the mode"
  (with-temp-buffer
    (let ((ov (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--unregister ov)
      (should-not mevedel-preview-mode)
      (should-not mevedel-preview-mode--pending)))
  :doc "unregistering a non-last overlay keeps the mode active"
  (with-temp-buffer
    (let ((a (mevedel-preview-test--make-overlay (current-buffer)))
          (b (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (mevedel-preview-mode--unregister a)
      (should mevedel-preview-mode)
      (should (equal (list b) mevedel-preview-mode--pending))))
  :doc "lighter shows pending count"
  (with-temp-buffer
    (let ((a (mevedel-preview-test--make-overlay (current-buffer)))
          (b (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register a)
      (should (equal " Preview[1]" (mevedel-preview-mode--lighter)))
      (mevedel-preview-mode--register b)
      (should (equal " Preview[2]" (mevedel-preview-mode--lighter))))))


;;
;;; View interaction previews

(mevedel-deftest mevedel-preview-mode--view-interaction-preview ()
  ,test
  (test)
  :doc "fresh non-file-visiting data buffer shows preview in the view interaction zone"
  (mevedel-preview-test--with-view-buffers
    (let* ((target (file-name-concat root "target.txt"))
           (temp (make-temp-file "mevedel-preview-proposed-"))
           (called nil))
      (with-temp-file target
        (insert "old\n"))
      (with-temp-file temp
        (insert "new\n"))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "draft")
        (goto-char (+ (mevedel-view--input-start) 2)))
      (with-current-buffer data-buf
        (should-not buffer-file-name)
        (mevedel-preview-mode-add-preview
         :temp-file temp
         :path target
         :callback (lambda (_result) (setq called t))
         :tool-name "Edit")
        (should-not buffer-file-name))
      (with-current-buffer view-buf
        (let* ((pending mevedel-preview-mode--pending)
               (ov (car pending))
               (text (buffer-substring-no-properties (point-min) (point-max)))
               (preview-pos (string-search "Proposed changes to target.txt" text))
               (prompt-pos (mevedel-view--input-marker-position)))
          (should (= 1 (length pending)))
          (should (overlay-get ov 'mevedel--interaction-id))
          (should (buffer-live-p (overlay-get ov 'mevedel--diff-buffer)))
          (should (equal "1 preview pending"
                         (mevedel-view--interaction-count-label)))
          (should preview-pos)
          (should (< (+ (point-min) preview-pos) prompt-pos))
          (should (= (point) (+ (mevedel-view--input-start) 2)))
          (should (overlay-get ov 'read-only))
          (should (get-text-property (overlay-start ov) 'read-only))
          (should (string= "draft"
                           (buffer-substring-no-properties
                            (mevedel-view--input-start) (point-max))))
          (should-not called)
          (mevedel-preview-mode-dismiss-all))))))

(mevedel-deftest mevedel-preview-mode--view-interaction-multiple-previews ()
  ,test
  (test)
  :doc "multiple pending view previews are counted and settle exactly once"
  (mevedel-preview-test--with-view-buffers
    (let ((results nil)
          targets)
      (dotimes (i 3)
        (let* ((target (file-name-concat root (format "target-%d.txt" i)))
               (temp (make-temp-file "mevedel-preview-proposed-"))
               (content (format "new-%d\n" i)))
          (push (cons target content) targets)
          (with-temp-file target
            (insert (format "old-%d\n" i)))
          (with-temp-file temp
            (insert content))
          (with-current-buffer data-buf
            (mevedel-preview-mode-add-preview
             :temp-file temp
             :path target
             :callback (lambda (result) (push result results))
             :tool-name "Edit"
             :apply-fn (let ((temp temp)
                             (target target))
                         (lambda ()
                           (copy-file temp target t)))))))
      (with-current-buffer view-buf
        (should (= 3 (length mevedel-preview-mode--pending)))
        (should (equal "3 previews pending"
                       (mevedel-view--interaction-count-label)))
        (mevedel-preview-mode-approve-all)
        (should-not mevedel-preview-mode--pending)
        (should-not (mevedel-view--interaction-count-label)))
      (should (= 3 (length results)))
      (dolist (pair targets)
        (should (string= (cdr pair)
                         (with-temp-buffer
                           (insert-file-contents (car pair))
                           (buffer-string))))))))

(mevedel-deftest mevedel-preview-mode--view-interaction-preview-rebuild ()
  ,test
  (test)
  :doc "pending view preview survives interaction rebuild and remains approvable"
  (mevedel-preview-test--with-view-buffers
    (let* ((target (file-name-concat root "target.txt"))
           (temp (make-temp-file "mevedel-preview-proposed-"))
           result)
      (with-temp-file target
        (insert "old\n"))
      (with-temp-file temp
        (insert "new\n"))
      (with-current-buffer data-buf
        (mevedel-preview-mode-add-preview
         :temp-file temp
         :path target
         :callback (lambda (value) (setq result value))
         :tool-name "Edit"
         :apply-fn (let ((temp temp)
                         (target target))
                     (lambda ()
                       (copy-file temp target t)))))
      (with-current-buffer view-buf
        (let ((original-overlay (car mevedel-preview-mode--pending)))
          (mevedel-view--interaction-rebuild)
          (should (eq original-overlay (car mevedel-preview-mode--pending)))
          (should (overlay-buffer original-overlay))
          (should (string-search
                   "Proposed changes to target.txt"
                   (buffer-substring-no-properties (point-min) (point-max))))
          (mevedel-preview-mode-toggle-overlay original-overlay)
          (should (get-char-property (overlay-start original-overlay)
                                     'invisible))
          (mevedel-preview-mode-toggle-overlay original-overlay)
          (mevedel-preview-mode--approve-overlay original-overlay)))
      (should result)
      (should (string= "new\n"
                       (with-temp-buffer
                         (insert-file-contents target)
                         (buffer-string)))))))

(mevedel-deftest mevedel-preview-mode--create-overlay ()
  ,test
  (test)
  :doc "raw preview insertion is read-only and preserves point"
  (let ((root (make-temp-file "mevedel-preview-raw-" t))
        (temp (make-temp-file "mevedel-preview-proposed-"))
        target)
    (unwind-protect
        (with-temp-buffer
          (setq target (file-name-concat root "target.txt"))
          (insert "draft")
          (goto-char (point-min))
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'default)))
            (let ((ov (mevedel-preview-mode--create-overlay
                       "-old\n+new\n" temp target #'ignore
                       (current-buffer) nil root "target.txt")))
              (should (= (point) (point-min)))
              (should (overlay-get ov 'read-only))
              (should (get-text-property (overlay-start ov) 'read-only))
              (should (string-match-p
                       "Proposed changes to target.txt"
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
              (mevedel-preview-mode-dismiss-all))))
      (when (file-exists-p temp)
        (delete-file temp))
      (when (file-directory-p root)
        (delete-directory root t)))))


;;
;;; Approve / reject / feedback via helpers

(mevedel-deftest mevedel-preview-mode--approve-overlay ()
  ,test
  (test)
  :doc "calls apply-fn, fires callback with (:result :render-data) plist, unregisters"
  (with-temp-buffer
    (let* ((result nil)
           (applied nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))
                  (mevedel--apply-fn . ,(lambda () (setq applied t)))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--approve-overlay ov)
      (should applied)
      (should (listp result))
      (should (string-match-p "approved and applied to /tmp/fake.txt"
                              (plist-get result :result)))
      (should (eq 'diff (plist-get (plist-get result :render-data) :kind)))
      (should-not mevedel-preview-mode--pending)
      (should-not (overlay-buffer ov))))
  :doc "apply-fn error is reported via callback as plain string without throwing"
  (with-temp-buffer
    (let* ((result nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))
                  (mevedel--apply-fn . ,(lambda () (error "boom")))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--approve-overlay ov)
      (should (stringp result))
      (should (string-match-p "Error applying changes: boom" result))
      (should-not mevedel-preview-mode--pending))))

(mevedel-deftest mevedel-preview-mode--reject-overlay ()
  ,test
  (test)
  :doc "without feedback, callback gets plain rejection message"
  (with-temp-buffer
    (let* ((result nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--reject-overlay ov)
      (should (string-match-p "rejected by user" result))
      (should-not (string-match-p "feedback" result))
      (should-not mevedel-preview-mode--pending)))
  :doc "with feedback, callback gets feedback embedded"
  (with-temp-buffer
    (let* ((result nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--reject-overlay ov "use a different approach")
      (should (string-match-p "use a different approach" result))
      (should-not mevedel-preview-mode--pending))))


;;
;;; Batch operations

(mevedel-deftest mevedel-preview-mode-approve-all ()
  ,test
  (test)
  :doc "approves every pending overlay"
  (with-temp-buffer
    (let* ((results nil)
           (cb (lambda (r) (push r results)))
           (a (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")
                 (mevedel--final-callback . ,cb)
                 (mevedel--apply-fn . ,(lambda () nil)))))
           (b (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/b.txt")
                 (mevedel--final-callback . ,cb)
                 (mevedel--apply-fn . ,(lambda () nil))))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (mevedel-preview-mode-approve-all)
      (should (= 2 (length results)))
      (should (seq-every-p (lambda (r) (string-match-p "approved" (plist-get r :result))) results))
      (should-not mevedel-preview-mode--pending))))

(mevedel-deftest mevedel-preview-mode-reject-all ()
  ,test
  (test)
  :doc "rejects every pending overlay and calls mevedel-abort once"
  (with-temp-buffer
    (let* ((results nil)
           (abort-count 0)
           (cb (lambda (r) (push r results)))
           (a (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")
                 (mevedel--final-callback . ,cb))))
           (b (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/b.txt")
                 (mevedel--final-callback . ,cb)))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (cl-letf (((symbol-function 'mevedel-abort)
                 (lambda (&rest _) (cl-incf abort-count))))
        (mevedel-preview-mode-reject-all))
      (should (= 2 (length results)))
      (should (seq-every-p (lambda (r) (string-match-p "rejected" r)) results))
      (should (= 1 abort-count))
      (should-not mevedel-preview-mode--pending)))
  :doc "with no pending overlays, does not call mevedel-abort"
  (with-temp-buffer
    (let ((abort-count 0))
      (cl-letf (((symbol-function 'mevedel-abort)
                 (lambda (&rest _) (cl-incf abort-count))))
        (mevedel-preview-mode-reject-all))
      (should (zerop abort-count)))))

(mevedel-deftest mevedel-preview-mode-dismiss-all ()
  ,test
  (test)
  :doc "cleans up overlays and fires each callback with Error: aborted"
  (with-temp-buffer
    (let* ((received nil)
           (cb (lambda (r) (push r received)))
           (a (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")
                 (mevedel--final-callback . ,cb))))
           (b (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/b.txt")
                 (mevedel--final-callback . ,cb)))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (mevedel-preview-mode-dismiss-all)
      (should (= 2 (length received)))
      (should (cl-every (lambda (r) (equal r "Error: aborted")) received))
      (should-not mevedel-preview-mode--pending)
      (should-not (overlay-buffer a))
      (should-not (overlay-buffer b)))))


;;
;;; Ediff stub cleanup

(mevedel-deftest mevedel-preview-mode--ediff-stub-cleanup ()
  ,test
  (test)
  :doc "cleanup-overlay deletes the stub file when flag is set"
  (let ((stub (make-temp-file "mevedel-stub-")))
    (with-temp-buffer
      (let ((ov (mevedel-preview-test--make-overlay
                 (current-buffer)
                 `((mevedel--real-path . ,stub)
                   (mevedel--ediff-created-stub . t)))))
        (mevedel-preview-mode--register ov)
        (mevedel-preview-mode--cleanup-overlay ov)
        (should-not (file-exists-p stub)))))
  :doc "cleanup-overlay preserves existing files without the stub flag"
  (let ((real-file (make-temp-file "mevedel-real-")))
    (with-temp-buffer
      (let ((ov (mevedel-preview-test--make-overlay
                 (current-buffer)
                 `((mevedel--real-path . ,real-file)))))
        (mevedel-preview-mode--register ov)
        (mevedel-preview-mode--cleanup-overlay ov)
        (should (file-exists-p real-file))
        (delete-file real-file))))
  :doc "apply-overlay clears the stub flag so approve preserves content"
  (let ((stub (make-temp-file "mevedel-stub-" nil nil "approved content\n")))
    (unwind-protect
        (with-temp-buffer
          (let* ((result nil)
                 (ov (mevedel-preview-test--make-overlay
                      (current-buffer)
                      `((mevedel--real-path . ,stub)
                        (mevedel--ediff-created-stub . t)
                        (mevedel--final-callback . ,(lambda (r) (setq result r)))
                        (mevedel--apply-fn . ,(lambda () nil))))))
            (mevedel-preview-mode--register ov)
            (mevedel-preview-mode--approve-overlay ov)
            (should (string-match-p "approved" (plist-get result :result)))
            (should (file-exists-p stub))))
      (when (file-exists-p stub) (delete-file stub)))))


;;
;;; Canceller wiring

(mevedel-deftest mevedel-preview-mode--register-cancel-fn ()
  ,test
  (test)
  :doc "registering a preview pushes a canceller onto the current request"
  (with-temp-buffer
    (setq-local mevedel--current-request
                (mevedel-request--create
                 :file-snapshots (make-hash-table :test #'equal)))
    (let ((ov (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")))))
      (mevedel-preview-mode--register ov)
      (should (= 1 (length (mevedel-request-cancellers
                            mevedel--current-request))))
      (should (functionp (car (mevedel-request-cancellers
                               mevedel--current-request))))))
  :doc "registering a second preview in the same request does not double-register"
  (with-temp-buffer
    (setq-local mevedel--current-request
                (mevedel-request--create
                 :file-snapshots (make-hash-table :test #'equal)))
    (let ((ov1 (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/a.txt"))))
          (ov2 (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/b.txt")))))
      (mevedel-preview-mode--register ov1)
      (mevedel-preview-mode--register ov2)
      (should (= 1 (length (mevedel-request-cancellers
                            mevedel--current-request))))))
  :doc "drained canceller fires every overlay's callback with Error: aborted"
  (let ((chat-buf (generate-new-buffer " *preview-test-chat*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :file-snapshots (make-hash-table :test #'equal)))
          (let* ((received nil)
                 (cb (lambda (r) (push r received)))
                 (ov (mevedel-preview-test--make-overlay
                      chat-buf
                      `((mevedel--real-path . "/tmp/a.txt")
                        (mevedel--final-callback . ,cb)))))
            (mevedel-preview-mode--register ov)
            (mevedel-request-drain-cancellers mevedel--current-request)
            (should (equal received '("Error: aborted")))
            (should-not (buffer-local-value 'mevedel-preview-mode--pending
                                            chat-buf))
            ;; Second drain is a no-op because the list was already
            ;; emptied atomically.
            (mevedel-request-drain-cancellers mevedel--current-request)
            (should (= 1 (length received)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))


;;
;;; Effective permission mode

(mevedel-deftest mevedel-preview-mode--effective-mode ()
  ,test
  (test)
  :doc "session slot wins over buffer-local variable"
  (with-temp-buffer
    (setq-local mevedel--session
                (mevedel-session--create :permission-mode 'accept-edits))
    (setq-local mevedel-permission-mode 'plan)
    (should (eq 'accept-edits (mevedel-preview-mode--effective-mode))))
  :doc "buffer-local variable used when session has no mode"
  (with-temp-buffer
    (setq-local mevedel--session nil)
    (setq-local mevedel-permission-mode 'trust-all)
    (should (eq 'trust-all (mevedel-preview-mode--effective-mode))))
  :doc "falls back to 'default when neither is set"
  (with-temp-buffer
    (setq-local mevedel--session nil)
    (setq-local mevedel-permission-mode nil)
    (should (eq 'default (mevedel-preview-mode--effective-mode)))))


;;
;;; Auto-apply

(defun mevedel-preview-test--make-auto-apply-buffer (path)
  "Create a chat-like buffer with the minimal state `--auto-apply' needs."
  (let ((buf (generate-new-buffer " *preview-test-auto*")))
    (with-current-buffer buf
      (setq-local mevedel--workspace
                  (mevedel-workspace--create
                   :root (file-name-directory path)))
      (setq-local mevedel--session
                  (mevedel-session--create
                   :workspace mevedel--workspace
                   :touched-files (make-hash-table :test #'equal))))
    buf))

(mevedel-deftest mevedel-preview-mode--auto-apply ()
  ,test
  (test)
  :doc "applies temp-file to path, fires callback with (:result :render-data) plist"
  (let* ((tmp (make-temp-file "mev-auto-src-" nil ".txt" "new body\n"))
         (real (make-temp-file "mev-auto-dst-" nil ".txt" "old body\n"))
         (chat (mevedel-preview-test--make-auto-apply-buffer real))
         result)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (mevedel-preview-mode--auto-apply
             tmp real
             (lambda (r) (setq result r))
             (lambda () (copy-file tmp real t))
             "Write"))
          (should (listp result))
          (should (string-match-p "auto-applied" (plist-get result :result)))
          (let ((rd (plist-get result :render-data)))
            (should (eq 'diff (plist-get rd :kind)))
            (should (equal real (plist-get rd :path)))
            (should (stringp (plist-get rd :patch))))
          ;; Temp file consumed
          (should-not (file-exists-p tmp))
          ;; Destination updated
          (should (equal "new body\n"
                         (with-temp-buffer
                           (insert-file-contents real) (buffer-string)))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (file-exists-p tmp) (delete-file tmp))
      (when (file-exists-p real) (delete-file real))))
  :doc "apply-fn error surfaces as plain error string, not a plist"
  (let* ((tmp (make-temp-file "mev-auto-src-" nil ".txt" "new\n"))
         (real (make-temp-file "mev-auto-dst-" nil ".txt" "old\n"))
         (chat (mevedel-preview-test--make-auto-apply-buffer real))
         result)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (mevedel-preview-mode--auto-apply
             tmp real
             (lambda (r) (setq result r))
             (lambda () (error "boom"))
             "Write"))
          (should (stringp result))
          (should (string-match-p "Error auto-applying" result))
          (should (string-match-p "boom" result))
          ;; Temp file still cleaned up on error
          (should-not (file-exists-p tmp)))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (file-exists-p tmp) (delete-file tmp))
      (when (file-exists-p real) (delete-file real)))))


;;
;;; Approve-and-trust (S key)

(mevedel-deftest mevedel-preview-mode-approve-and-trust ()
  ,test
  (test)
  :doc "approves every pending overlay and flips the session mode"
  (let ((chat (generate-new-buffer " *preview-trust-chat*"))
        (view (generate-new-buffer " *preview-trust-view*"))
        results)
    (unwind-protect
        (let* ((cb (lambda (r) (push r results)))
               (session (mevedel-session--create :permission-mode 'default))
               (a (mevedel-preview-test--make-overlay
                   chat
                   `((mevedel--real-path . "/tmp/a.txt")
                     (mevedel--data-buffer . ,chat)
                     (mevedel--final-callback . ,cb)
                     (mevedel--apply-fn . ,(lambda () nil)))))
               (b (mevedel-preview-test--make-overlay
                   chat
                   `((mevedel--real-path . "/tmp/b.txt")
                     (mevedel--data-buffer . ,chat)
                     (mevedel--final-callback . ,cb)
                     (mevedel--apply-fn . ,(lambda () nil))))))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer view)
            (setq-local mevedel-permission-mode 'default)
            (with-current-buffer view
              (setq-local mevedel--data-buffer chat)
              (setq-local mevedel-permission-mode 'default))
            (mevedel-preview-mode--register a)
            (mevedel-preview-mode--register b)
            (mevedel-preview-mode-approve-and-trust))
          (should (= 2 (length results)))
          (should (eq 'accept-edits
                      (mevedel-session-permission-mode session)))
          (should (eq 'accept-edits
                      (buffer-local-value 'mevedel-permission-mode chat)))
          (should (eq 'accept-edits
                      (buffer-local-value 'mevedel-permission-mode view)))
          (should-not (buffer-local-value 'mevedel-preview-mode--pending chat)))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p view) (kill-buffer view))))

  :doc "with no pending overlays: no error, no mode flip"
  (with-temp-buffer
    (setq-local mevedel--session
                (mevedel-session--create :permission-mode 'default))
    (setq-local mevedel-permission-mode 'default)
    (mevedel-preview-mode-approve-and-trust)
    ;; Mode stays unchanged because data-buffer is not derivable.
    (should (eq 'default
                (mevedel-session-permission-mode mevedel--session)))
    (should (eq 'default mevedel-permission-mode))))

(provide 'test-mevedel-preview-mode)
;;; test-mevedel-preview-mode.el ends here
