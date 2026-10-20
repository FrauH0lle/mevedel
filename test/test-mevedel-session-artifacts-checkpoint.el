;;; test-mevedel-session-artifacts-checkpoint.el --- Save capture tests -*- lexical-binding: t -*-

;;; Commentary:

;; Root transcript capture is shared by synchronous saves and background jobs.

;;; Code:

(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))

(mevedel-deftest mevedel-session-artifacts-prepare-save (:quiet t)
  ,test
  (test)
  :doc "unchanged bytes prepare no publication and finish without changing the head"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((head (plist-get (mevedel-session-publication session) :head))
            (prepared (with-current-buffer buffer
                        (mevedel-session-artifacts-call-with-checkpoint
                         session
                         (lambda ()
                           (mevedel-session-artifacts-prepare-save session buffer))))))
       (should-not (plist-get prepared :artifacts))
       (should (functionp (plist-get prepared :finish)))
       (should (equal (mevedel-session-save-path session)
                      (funcall (plist-get prepared :finish))))
       (should (equal head (plist-get (mevedel-session-publication session) :head))))))

  :doc "capture publishes nothing and finish preserves edits newer than frozen bytes"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head)) prepared)
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert (propertize "Frozen answer\n" 'gptel 'response))
         (setq prepared
               (mevedel-session-artifacts-call-with-checkpoint
                session
                (lambda () (mevedel-session-artifacts-prepare-save session buffer))))
         (should (buffer-modified-p))
         (should (equal head (plist-get (mevedel-session-publication session) :head)))
         (let* ((artifacts (plist-get prepared :artifacts))
                (marker (car (last artifacts))))
           (should (equal "session.meta.el"
                          (file-name-nondirectory (plist-get marker :path))))
           (should (plist-get marker :commit-marker))
           (should (plist-get marker :checkpoint))
           (should (seq-some
                    (lambda (artifact)
                      (string-match-p "Frozen answer" (or (plist-get artifact :content) "")))
                    artifacts))
           (goto-char (point-max))
           (insert "Newer edit\n")
           (mevedel-session-publication-publish session artifacts nil t)
           (funcall (plist-get prepared :finish))
           (should (buffer-modified-p))
           (let ((disk (mevedel-session-artifacts-read-artifact
                        session "segment-0001.chat.org" t)))
             (should (string-match-p "Frozen answer" disk))
             (should-not (string-match-p "Newer edit" disk)))))))))

(provide 'test-mevedel-session-artifacts-checkpoint)

;;; test-mevedel-session-artifacts-checkpoint.el ends here
