;;; test-niwa-shared-editing-lifecycle.el --- Shared content lifecycle -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared items belong to the workspace store: session lifecycle operations
;; neither copy nor roll them back.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))
(require 'niwa-shared-editing)

(defun test-niwa-shared-editing-call (buffer args)
  "Call the real shared editor in BUFFER with ARGS and await its reply."
  (with-current-buffer buffer
    (let (reply)
      (niwa-shared-editing-call (niwa-session-workspace niwa--session)
                                   args (lambda (value) (setq reply value)))
      (let ((deadline (+ (float-time) 15)))
        (while (and (not reply) (< (float-time) deadline))
          (accept-process-output nil 0.03)))
      (should reply)
      (should-not (plist-get reply :error))
      (plist-get reply :result))))

(niwa-deftest niwa-shared-editing-call/lifecycle
  (:doc "Fork shares the workspace's item; Rewind and cold Resume leave it as stored" :quiet t)
  (let* ((fixture (test-niwa-session-persistence--make-fork-ready))
         (buffer (plist-get fixture :buffer))
         (session (plist-get fixture :session))
         (parent-path (niwa-session-save-path session))
         (niwa-artifact-lease--held (make-hash-table :test #'equal))
         child resumed)
    (unwind-protect
        (progn
          ;; The fork fixture exposes its historical selection. Restore the
          ;; actual live segment before exercising a fresh Rewind transaction.
          (with-current-buffer buffer
            (erase-buffer)
            (insert (decode-coding-string
                     (niwa-session-artifacts-read-artifact
                      session "segment-0003.chat.org" t) 'utf-8-unix))
            (niwa-transcript-restore-properties))
          (test-niwa-shared-editing-call
           buffer '(:action "create" :id "board" :kind "whiteboard" :title "Original"
                    :opId "create" :actor "Guest"))
          (setq child (niwa-session-fork-create buffer '(:fork-point-id "fixture-fork") 'conversation))
          (test-niwa-shared-editing-call
           child '(:action "rename" :id "board" :opId "rename" :actor "Guest" :title "Child"))
          ;; One item, seen by both sessions.
          (should (equal "Child" (plist-get (test-niwa-shared-editing-call
                                             buffer '(:action "read" :id "board"))
                                            :title)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (niwa-session-rewind-rewind buffer '(:segment 2 :fork-point-id "fixture-fork" :turn 1 :file-turn 1 :cum-turn 2) 'after))
          (should (equal "Child" (plist-get (test-niwa-shared-editing-call
                                             buffer '(:action "read" :id "board"))
                                            :title)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (setq resumed (niwa-session-persistence-restore parent-path nil nil (plist-get fixture :workspace)))
          (should (equal "Child" (plist-get (test-niwa-shared-editing-call
                                             resumed '(:action "read" :id "board"))
                                            :title))))
      (niwa-shared-editing-stop)
      (niwa-artifact-lease-release-all)
      (dolist (extra (list child resumed))
        (when (buffer-live-p extra)
          (with-current-buffer extra
            (when (buffer-live-p niwa--view-buffer) (kill-buffer niwa--view-buffer))
            (set-buffer-modified-p nil))
          (kill-buffer extra)))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture))))

(provide 'test-niwa-shared-editing-lifecycle)
;;; test-niwa-shared-editing-lifecycle.el ends here
