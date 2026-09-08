;;; test-mevedel-journal-capture-branches.el -- Journal branch lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Uses actual Rewind and Conversation Fork commands over saved root turns.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-session-rewind)
(require 'mevedel-session-fork)

(mevedel-deftest mevedel-journal-capture-checkpoint/branches (:quiet t)
  ,test
  (test)
  :doc "real Rewind preserves sealed evidence while replacement turns receive fresh coverage"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "First request" "First result")
     (let ((target (copy-sequence (cdar (mevedel-session-rewind--prompt-candidates session)))))
       (mevedel-test-journal-capture--turn session buffer "Abandoned second request" "Abandoned second result")
       (let* ((workspace (mevedel-session-workspace session))
              (original (car (mevedel-journal-capture-seal session buffer 'session-end)))
              (old-ids (plist-get (plist-get original :metadata) :turn-ids))
              (old-evidence (mevedel-journal-capture-evidence workspace original)))
         (save-window-excursion
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (should (mevedel-session-rewind-rewind buffer target 'after))))
         (should (= 1 (mevedel-session-turn-count session)))
         (mevedel-test-journal-capture--turn session buffer "Replacement request" "Replacement decisive result")
         (let* ((current (cl-find-if
                          (lambda (capture) (not (equal (plist-get capture :id) (plist-get original :id))))
                          (mevedel-journal-capture-list workspace)))
                (metadata (plist-get current :metadata)))
           (should (equal '(2) (plist-get metadata :turns)))
           (should (= 1 (length (plist-get metadata :turn-ids))))
           (should-not (member (car (plist-get metadata :turn-ids)) old-ids))
           (should (string-match-p "Replacement decisive result" (mevedel-journal-capture-evidence workspace current)))
           (should-not (string-match-p "Abandoned second result" (mevedel-journal-capture-evidence workspace current))))
         (should (equal old-evidence (mevedel-journal-capture-evidence workspace original)))
         (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))))))

  :doc "real Conversation Fork excludes inherited covered turns and owns its new source pin"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Parent request" "Parent decisive result")
     (let* ((workspace (mevedel-session-workspace session))
            (target (copy-sequence (cdar (mevedel-session-rewind--prompt-candidates session))))
            (original (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (parent-text (with-current-buffer buffer (buffer-string)))
            child-buffer child)
       (unwind-protect
           (save-window-excursion
             (setq child-buffer (mevedel-session-fork-conversation-fork buffer target)
                   child (buffer-local-value 'mevedel--session child-buffer))
             (should-not (equal (mevedel-session-session-id child) (mevedel-session-session-id session)))
             (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path child)))
             (mevedel-test-journal-capture--turn child child-buffer "Child request" "Child decisive result")
             (let* ((capture (cl-find-if
                              (lambda (entry) (equal (mevedel-session-session-id child)
                                                    (plist-get (plist-get entry :metadata) :session)))
                              (mevedel-journal-capture-list workspace)))
                    (metadata (plist-get capture :metadata))
                    (evidence (mevedel-journal-capture-evidence workspace capture)))
               (should (equal '(2) (plist-get metadata :turns)))
               (should-not (equal (plist-get original :id) (plist-get capture :id)))
               (should (string-match-p "Child decisive result" evidence))
               (should-not (string-match-p "Parent decisive result" evidence))
               (should (mevedel-journal-pins-present-p (mevedel-session-save-path child))))
             (should (equal parent-text (with-current-buffer buffer (buffer-string))))
             (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
         (when (buffer-live-p child-buffer)
           (let ((view (buffer-local-value 'mevedel--view-buffer child-buffer)))
             (with-current-buffer child-buffer (set-buffer-modified-p nil))
             (when (buffer-live-p view) (kill-buffer view)))
           (when (buffer-live-p child-buffer) (kill-buffer child-buffer)))
         (when child
           (mevedel-session-persistence-lock-release (mevedel-session-save-path child) child)))))))

(provide 'test-mevedel-journal-capture-branches)
;;; test-mevedel-journal-capture-branches.el ends here
