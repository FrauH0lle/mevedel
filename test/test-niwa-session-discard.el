;;; test-niwa-session-discard.el -- Cold discard integration -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise explicit discard against committed, closed portable sessions.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-process)
(require 'niwa-session-persistence)
(require 'niwa-session-durability)
(require 'niwa-session-recovery)
(require 'niwa-execution-target)

(defconst niwa-test-session-discard--digest
  "## Done\n- Observed: result (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")

(defun niwa-test-session-discard--with-cold (function &optional prepare)
  "Call FUNCTION with WORKSPACE, ENTRY, CAPTURE and closed SESSION.
Run PREPARE with the live session, buffer and capture before closing."
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (directory (niwa-session-save-path session))
            (capture (car (niwa-journal-capture-list workspace))))
       (should capture)
       (when prepare (funcall prepare session buffer capture))
       (niwa-session-durability-lease-release directory session)
       (let ((niwa-journal-enabled nil))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (should-not (buffer-live-p (niwa-session-root-buffer session)))
       (let ((entry (cl-find directory
                             (niwa-session-persistence-list-sessions workspace)
                             :key (lambda (row) (plist-get row :save-path))
                             :test #'equal)))
         (should entry)
         (should (plist-get entry :publication))
         (funcall function workspace entry capture session))))))

(defun niwa-test-session-discard--bytes (path)
  "Read PATH without decoding."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun niwa-test-session-discard--snapshot (directory)
  "Return relative names and hashes of every file below DIRECTORY."
  (mapcar (lambda (path)
            (cons (file-relative-name path directory)
                  (secure-hash 'sha256 (niwa-test-session-discard--bytes path))))
          (sort (directory-files-recursively directory "." nil
                                             (lambda (_) t)) #'string<)))

(defun niwa-test-session-discard--advance (session)
  "Commit a replacement publication under SESSION's real authority."
  (let* ((directory (niwa-session-save-path session))
         (sidecar (niwa-session-persistence-load-sidecar
                   (plist-get (niwa-session-publication-read directory) :sidecar))))
    (should (niwa-session-durability-lease-acquire directory "replacement" session))
    (unwind-protect
        (progn
          (plist-put sidecar :session-name "Replacement")
          (niwa-session-publication-publish
           session (list (list :path (file-name-concat directory "session.meta.el")
                               :content (prin1-to-string sidecar) :commit-marker t)) t))
      (niwa-session-durability-lease-release directory session))))

(defun niwa-test-session-discard--install-recovery (session)
  "Install valid recovery bytes and their owned marker for SESSION."
  (let ((recovery (make-temp-file "niwa-discard-recovery-" t)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat recovery "files"))
          (write-region "original bytes" nil (file-name-concat recovery "files" "before.el") nil 'silent)
          (niwa-test--with-captured-diagnostics nil
            (niwa-session-recovery-record-failure session "Rewind incomplete" recovery))
          (should (niwa-session-recovery-read (niwa-session-save-path session))))
      (when (file-directory-p recovery) (delete-directory recovery t)))))

(niwa-deftest niwa-session-persistence-discard/cold ()
  ,test
  (test)
  :doc "cold ready and sealed captures record durable omissions before source deletion"
  (dolist (sealed '(nil t))
    (niwa-test-session-discard--with-cold
     (lambda (workspace entry capture _session)
       (let* ((id (plist-get capture :id))
              (directory (plist-get entry :save-path))
              (capture-directory (niwa-journal-capture--directory workspace id)))
         (should (niwa-journal-pins-present-p directory))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (should (niwa-session-persistence-discard workspace entry)))
         (should-not (file-exists-p directory))
         (should-not (niwa-journal-capture-list workspace))
         (should (file-regular-p (file-name-concat capture-directory "discard.json")))
         (should (niwa-journal-discard-accepted
                  (list :status 'cancelled
                        :payload (niwa-test-session-discard--bytes
                                  (file-name-concat capture-directory "discard.json"))) id))))
     (lambda (session buffer _capture)
       (when sealed (niwa-journal-capture-seal session buffer 'session-end)))))

  :doc "cold accepted digest is published and preserved instead of becoming an omission"
  (niwa-test-session-discard--with-cold
   (lambda (workspace entry capture _session)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-persistence-discard workspace entry)))
     (let ((digest (car (niwa-journal-store-entries (niwa-workspace-root workspace)))))
       (should (equal (plist-get capture :id) (plist-get digest :capture-id)))
       (should (equal niwa-test-session-discard--digest (plist-get digest :body))))
     (should-not (file-exists-p
                  (niwa-journal-capture--file workspace (plist-get capture :id) "discard.json")))
     (should-not (file-exists-p (plist-get entry :save-path))))
   (lambda (session buffer capture)
     (niwa-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (niwa-session-workspace session))
            (claim (niwa-journal-claim-acquire
                    (niwa-journal-process--attempts workspace capture) 120)))
       (should (niwa-journal-claim-settle claim 'completed niwa-test-session-discard--digest)))))

  :doc "cold malformed, unready, orphan and foreign-owned captures keep their exact pin bytes"
  (dolist (kind '(malformed-pin malformed-capture unready orphan foreign-owner))
    (niwa-test-session-discard--with-cold
     (lambda (workspace entry capture _session)
       (let* ((directory (plist-get entry :save-path))
              (id (plist-get capture :id))
              (pin (niwa-journal-pins--path directory id))
              (capture-directory (niwa-journal-capture--directory workspace id))
              claim)
         (pcase kind
           ('malformed-pin (write-region "broken pin" nil pin nil 'silent))
           ('malformed-capture
            (write-region "broken descriptor" nil
                          (file-name-concat capture-directory "capture.json") nil 'silent))
           ('unready (delete-file (file-name-concat capture-directory "ready")))
           ('orphan (delete-directory capture-directory t))
           ('foreign-owner
            (setq claim (niwa-journal-claim-acquire
                         (file-name-concat
                          (niwa-journal-store-state-directory (niwa-journal-process--key workspace))
                          "digest-run") 120))))
         (let ((before (niwa-test-session-discard--bytes pin)))
           (unwind-protect
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                 (let ((err (should-error (niwa-session-persistence-discard workspace entry)
                                          :type 'user-error)))
                   (should (string-match-p "[Jj]ournal\\|capture\\|pin\\|busy"
                                           (error-message-string err))))
                 (should (file-directory-p directory))
                 (should (equal before (niwa-test-session-discard--bytes pin)))
                 (should-not (file-exists-p (file-name-concat capture-directory "discard.json"))))
             (when claim (niwa-journal-claim-settle claim 'cancelled ""))))))))

  :doc "cold target incarnation replacement refuses without publishing or changing grants"
  (niwa-test-session-discard--with-cold
   (lambda (workspace entry _capture _session)
     (let* ((root (niwa-workspace-root workspace))
            (before (niwa-test-session-discard--snapshot root))
            prompted)
       (cl-letf (((symbol-function 'niwa-execution-target--local-incarnation)
                  (lambda () "replacement-incarnation"))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq prompted t))))
         (let ((err (should-error (niwa-session-persistence-discard workspace entry)
                                  :type 'user-error)))
           (should (string-match-p (regexp-quote root) (error-message-string err)))))
       (should-not prompted)
       (should (equal before (niwa-test-session-discard--snapshot root))))))

  :doc "cold selected publication replacement before or during confirmation is never deleted"
  (dolist (timing '(before during))
    (niwa-test-session-discard--with-cold
     (lambda (workspace entry _capture session)
       (let ((directory (plist-get entry :save-path))
             replacement before prompted)
         (when (eq timing 'before)
           (niwa-test-session-discard--advance session)
           (setq replacement (niwa-session-publication-read directory)
                 before (niwa-test-session-discard--snapshot directory)))
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _)
                      (setq prompted t)
                      (when (eq timing 'during)
                        (niwa-test-session-discard--advance session)
                        (setq replacement (niwa-session-publication-read directory)))
                      t)))
           (let ((err (should-error (niwa-session-persistence-discard workspace entry)
                                    :type 'user-error)))
             (should (string-match-p "changed" (error-message-string err)))))
         (should (eq prompted (eq timing 'during)))
         (should (file-directory-p directory))
         (should (equal (plist-get replacement :head)
                        (plist-get (niwa-session-publication-read directory) :head)))
         (should-not (equal (plist-get replacement :head)
                            (plist-get (plist-get entry :publication) :head)))
         (when before
           (should (equal before (niwa-test-session-discard--snapshot directory))))))))

  :doc "cold recovery abandonment and omission survive final deletion failure with truthful retry"
  (niwa-test-session-discard--with-cold
   (lambda (workspace entry capture _session)
     (let* ((directory (plist-get entry :save-path))
            (recovery (niwa-session-recovery-read directory))
            (bytes (file-name-concat (plist-get recovery :directory) "files" "before.el")))
       (should (equal "original bytes" (niwa-test-session-discard--bytes bytes)))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'niwa-session-durability--delete-owned-session)
                 (lambda (&rest _)
                   (should-not (niwa-session-recovery-read directory))
                   (should-not (niwa-journal-pins-present-p directory))
                   (should-not
                    (plist-get (plist-get (niwa-session-durability-cleanup-assessment directory)
                                          :head) :unsettled-mutation))
                   (error "Injected final deletion failure"))))
         (let ((err (should-error (niwa-session-persistence-discard workspace entry)
                                  :type 'user-error)))
           (should (string-match-p "retained.*may have completed.*Injected final deletion failure"
                                   (error-message-string err)))))
       (should (file-directory-p directory))
       (should-not (file-exists-p bytes))
       (should-not (niwa-session-recovery-read directory))
       (should-not (niwa-journal-pins-present-p directory))
       (should (file-regular-p (niwa-journal-capture--file workspace (plist-get capture :id) "discard.json")))
       (should (memq (plist-get (niwa-session-durability-cleanup-assessment directory) :state)
                     '(available expired)))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (should (niwa-session-persistence-discard workspace entry)))
       (should-not (file-exists-p directory))))
   (lambda (session _buffer _capture)
     (should (niwa-session-durability-set-unsettled-mutation session t))
     (niwa-test-session-discard--install-recovery session)))

  :doc "cold unsupported hidden recovery bytes survive approved marker abandonment"
  (niwa-test-session-discard--with-cold
   (lambda (workspace entry capture _session)
     (let* ((directory (plist-get entry :save-path))
            (recovery (niwa-session-recovery-read directory))
            (unknown (file-name-concat directory ".recovery" ".unknown"))
            (content "Unsupported recovery evidence\n"))
       (should (file-regular-p (plist-get recovery :marker)))
       (write-region content nil unknown nil 'silent)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (let ((err (should-error (niwa-session-persistence-discard workspace entry)
                                  :type 'user-error)))
           (should (string-match-p "retained.*may have completed" (error-message-string err)))))
       (should (file-directory-p directory))
       (should (equal content (niwa-test-session-discard--bytes unknown)))
       (should-not (file-exists-p (plist-get recovery :marker)))
       (should-not (file-exists-p (plist-get recovery :directory)))
       (should-not (niwa-journal-pins-present-p directory))
       (should (file-regular-p (niwa-journal-capture--file workspace (plist-get capture :id) "discard.json")))))
   (lambda (session _buffer _capture)
     (niwa-test-session-discard--install-recovery session)))

  :doc "cold remote chooser entry discards through the target without restoration"
  (let ((local-root (file-name-as-directory (make-temp-file "niwa-discard-remote-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("discard-remote")
          (cl-destructuring-bind (workspace _session directory _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               "discard-remote" local-root "Committed remote transcript\n")
            (let ((entry (car (niwa-session-persistence-list-sessions workspace))))
              (should (file-remote-p (plist-get entry :save-path)))
              (should (equal directory (plist-get entry :save-path)))
              (should (file-directory-p (file-local-name directory)))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (should (niwa-session-persistence-discard workspace entry)))
              (should-not (file-exists-p (file-local-name directory)))
              (should-not (niwa-session-persistence-list-sessions workspace)))))
      (delete-directory local-root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-session-discard)
;;; test-niwa-session-discard.el ends here
