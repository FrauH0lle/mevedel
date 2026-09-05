;;; test-mevedel-shared-editing-lifecycle.el --- Shared content lifecycle -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared editing tests through the real host and existing public seams.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-shared-editing)

(defun test-mevedel-shared-editing-call (buffer args)
  "Call the real shared editor in BUFFER with ARGS and await its reply."
  (with-current-buffer buffer
    (let (reply)
      (mevedel-shared-editing-call mevedel--session args (lambda (value) (setq reply value)))
      (let ((deadline (+ (float-time) 15)))
        (while (and (not reply) (< (float-time) deadline))
          (accept-process-output nil 0.03)))
      (should reply)
      (should-not (plist-get reply :error))
      (plist-get reply :result))))

(mevedel-deftest mevedel-shared-editing-call/lifecycle
		 (:doc "Fork, Rewind, Save As, and cold Resume preserve independent editable assets" :quiet t)
		 (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
			(buffer (plist-get fixture :buffer))
			(session (plist-get fixture :session))
			(parent-path (mevedel-session-save-path session))
			child resumed)
		   (unwind-protect
		       (progn
			 ;; The fork fixture exposes its historical selection. Restore the
			 ;; actual live segment before exercising a fresh Rewind transaction.
			 (with-current-buffer buffer
			   (erase-buffer)
			   (insert-file-contents (mevedel-session-artifacts-segment-path parent-path 3))
			   (mevedel-transcript-restore-properties))
			 (let* ((image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jv1sAAAAASUVORK5CYII=")
				(made (test-mevedel-shared-editing-call
				       buffer
				       (list :action "import" :format "native" :id "board" :opId "import" :actor "Guest"
					     :data (mevedel-shared-editing--json
						    (list :format "mevedel-editable-1" :kind "whiteboard" :title "Original"
							  :content (vector (list :id "image" :type "image" :box [0 0 100 100] :src image))))))))
			   (setq child (mevedel-session-fork-create buffer '(:fork-point-id "fixture-fork") 'conversation))
			   (should (equal (plist-get made :content)
					  (plist-get (test-mevedel-shared-editing-call child '(:action "read" :id "board")) :content)))
			   (test-mevedel-shared-editing-call child '(:action "rename" :id "board" :opId "rename" :actor "Guest" :title "Child"))
			   (should (equal "Original" (plist-get (test-mevedel-shared-editing-call buffer '(:action "read" :id "board")) :title)))
			   (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
			     (mevedel-session-rewind-rewind buffer '(:segment 2 :fork-point-id "fixture-fork" :turn 1 :file-turn 1 :cum-turn 2) 'after))
			   (should (equal (plist-get made :content)
					  (plist-get (test-mevedel-shared-editing-call buffer '(:action "read" :id "board")) :content)))
			   (with-current-buffer buffer
			     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Copied")))
			       (mevedel-save-session t)))
			   (should-not (equal parent-path (mevedel-session-save-path session)))
			   (should (equal (plist-get made :content)
					  (plist-get (test-mevedel-shared-editing-call buffer '(:action "read" :id "board")) :content)))
			   (with-current-buffer buffer (set-buffer-modified-p nil))
			   (kill-buffer buffer)
			   (setq resumed (mevedel-session-persistence-restore parent-path nil nil (plist-get fixture :workspace)))
			   (should (equal (plist-get made :content)
					  (plist-get (test-mevedel-shared-editing-call resumed '(:action "read" :id "board")) :content)))
			   (should (equal "Original" (plist-get (test-mevedel-shared-editing-call resumed '(:action "read" :id "board")) :title)))))
		     (dolist (extra (list child resumed))
		       (when (buffer-live-p extra)
			 (with-current-buffer extra
			   (mevedel-shared-editing-stop)
			   (when (buffer-live-p mevedel--view-buffer) (kill-buffer mevedel--view-buffer))
			   (set-buffer-modified-p nil))
			 (kill-buffer extra)))
		     (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(provide 'test-mevedel-shared-editing-lifecycle)
;;; test-mevedel-shared-editing-lifecycle.el ends here
