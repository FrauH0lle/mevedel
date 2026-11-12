;;; test-mevedel-session-discard.el -- Cold discard integration -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise explicit discard against committed, closed portable sessions.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-process)
(require 'mevedel-session-persistence)
(require 'mevedel-session-durability)
(require 'mevedel-session-recovery)
(require 'mevedel-execution-target)

(defconst mevedel-test-session-discard--digest
  "## Done\n- Observed: result (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")

(defun mevedel-test-session-discard--with-cold (function &optional prepare)
  "Call FUNCTION with WORKSPACE, ENTRY, CAPTURE and closed SESSION.
Run PREPARE with the live session, buffer and capture before closing."
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (directory (mevedel-session-save-path session))
            (capture (car (mevedel-journal-capture-list workspace))))
       (should capture)
       (when prepare (funcall prepare session buffer capture))
       (mevedel-session-durability-lease-release directory session)
       (let ((mevedel-journal-enabled nil))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (should-not (buffer-live-p (mevedel-session-root-buffer session)))
       (let ((entry (cl-find directory
                             (mevedel-session-persistence-list-sessions workspace)
                             :key (lambda (row) (plist-get row :save-path))
                             :test #'equal)))
         (should entry)
         (should (plist-get entry :publication))
         (funcall function workspace entry capture session))))))

(defun mevedel-test-session-discard--bytes (path)
  "Read PATH without decoding."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun mevedel-test-session-discard--snapshot (directory)
  "Return relative names and hashes of every file below DIRECTORY."
  (mapcar (lambda (path)
            (cons (file-relative-name path directory)
                  (secure-hash 'sha256 (mevedel-test-session-discard--bytes path))))
          (sort (directory-files-recursively directory "." nil
                                             (lambda (_) t)) #'string<)))

(defun mevedel-test-session-discard--advance (session)
  "Commit a replacement publication under SESSION's real authority."
  (let* ((directory (mevedel-session-save-path session))
         (sidecar (mevedel-session-persistence-load-sidecar
                   (plist-get (mevedel-session-publication-read directory) :sidecar))))
    (should (mevedel-session-durability-lease-acquire directory "replacement" session))
    (unwind-protect
        (progn
          (plist-put sidecar :session-name "Replacement")
          (mevedel-session-publication-publish
           session (list (list :path (file-name-concat directory "session.meta.el")
                               :content (prin1-to-string sidecar) :commit-marker t)) t))
      (mevedel-session-durability-lease-release directory session))))

(defun mevedel-test-session-discard--install-recovery (session)
  "Install valid recovery bytes and their owned marker for SESSION."
  (let ((recovery (make-temp-file "mevedel-discard-recovery-" t)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat recovery "files"))
          (write-region "original bytes" nil (file-name-concat recovery "files" "before.el") nil 'silent)
          (mevedel-test--with-captured-diagnostics nil
            (mevedel-session-recovery-record-failure session "Rewind incomplete" recovery))
          (should (mevedel-session-recovery-read (mevedel-session-save-path session))))
      (when (file-directory-p recovery) (delete-directory recovery t)))))

(mevedel-deftest mevedel-session-persistence-discard/cold ()
  ,test
  (test)
  :doc "cold ready and sealed captures record durable omissions before source deletion"
  (dolist (sealed '(nil t))
    (mevedel-test-session-discard--with-cold
     (lambda (workspace entry capture _session)
       (let* ((id (plist-get capture :id))
              (directory (plist-get entry :save-path))
              (capture-directory (mevedel-journal-capture--directory workspace id)))
         (should (mevedel-journal-pins-present-p directory))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (should (mevedel-session-persistence-discard workspace entry)))
         (should-not (file-exists-p directory))
         (should-not (mevedel-journal-capture-list workspace))
         (should (file-regular-p (file-name-concat capture-directory "discard.json")))
         (should (mevedel-journal-discard-accepted
                  (list :status 'cancelled
                        :payload (mevedel-test-session-discard--bytes
                                  (file-name-concat capture-directory "discard.json"))) id))))
     (lambda (session buffer _capture)
       (when sealed (mevedel-journal-capture-seal session buffer 'session-end)))))

  :doc "cold accepted digest is published and preserved instead of becoming an omission"
  (mevedel-test-session-discard--with-cold
   (lambda (workspace entry capture _session)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (mevedel-session-persistence-discard workspace entry)))
     (let ((digest (car (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
       (should (equal (plist-get capture :id) (plist-get digest :capture-id)))
       (should (equal mevedel-test-session-discard--digest (plist-get digest :body))))
     (should-not (file-exists-p
                  (mevedel-journal-capture--file workspace (plist-get capture :id) "discard.json")))
     (should-not (file-exists-p (plist-get entry :save-path))))
   (lambda (session buffer capture)
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (mevedel-session-workspace session))
            (claim (mevedel-journal-claim-acquire
                    (mevedel-journal-process--attempts workspace capture) 120)))
       (should (mevedel-journal-claim-settle claim 'completed mevedel-test-session-discard--digest)))))

  :doc "cold malformed, unready, orphan and foreign-owned captures keep their exact pin bytes"
  (dolist (kind '(malformed-pin malformed-capture unready orphan foreign-owner))
    (mevedel-test-session-discard--with-cold
     (lambda (workspace entry capture _session)
       (let* ((directory (plist-get entry :save-path))
              (id (plist-get capture :id))
              (pin (mevedel-journal-pins--path directory id))
              (capture-directory (mevedel-journal-capture--directory workspace id))
              claim)
         (pcase kind
           ('malformed-pin (write-region "broken pin" nil pin nil 'silent))
           ('malformed-capture
            (write-region "broken descriptor" nil
                          (file-name-concat capture-directory "capture.json") nil 'silent))
           ('unready (delete-file (file-name-concat capture-directory "ready")))
           ('orphan (delete-directory capture-directory t))
           ('foreign-owner
            (setq claim (mevedel-journal-claim-acquire
                         (file-name-concat
                          (mevedel-journal-store-state-directory (mevedel-journal-process--key workspace))
                          "digest-run") 120))))
         (let ((before (mevedel-test-session-discard--bytes pin)))
           (unwind-protect
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                 (let ((err (should-error (mevedel-session-persistence-discard workspace entry)
                                          :type 'user-error)))
                   (should (string-match-p "[Jj]ournal\\|capture\\|pin\\|busy"
                                           (error-message-string err))))
                 (should (file-directory-p directory))
                 (should (equal before (mevedel-test-session-discard--bytes pin)))
                 (should-not (file-exists-p (file-name-concat capture-directory "discard.json"))))
             (when claim (mevedel-journal-claim-settle claim 'cancelled ""))))))))

  :doc "cold target incarnation replacement refuses without publishing or changing grants"
  (mevedel-test-session-discard--with-cold
   (lambda (workspace entry _capture _session)
     (let* ((root (mevedel-workspace-root workspace))
            (before (mevedel-test-session-discard--snapshot root))
            prompted)
       (cl-letf (((symbol-function 'mevedel-execution-target--local-incarnation)
                  (lambda () "replacement-incarnation"))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq prompted t))))
         (let ((err (should-error (mevedel-session-persistence-discard workspace entry)
                                  :type 'user-error)))
           (should (string-match-p (regexp-quote root) (error-message-string err)))))
       (should-not prompted)
       (should (equal before (mevedel-test-session-discard--snapshot root))))))

  :doc "cold selected publication replacement before or during confirmation is never deleted"
  (dolist (timing '(before during))
    (mevedel-test-session-discard--with-cold
     (lambda (workspace entry _capture session)
       (let ((directory (plist-get entry :save-path))
             replacement before prompted)
         (when (eq timing 'before)
           (mevedel-test-session-discard--advance session)
           (setq replacement (mevedel-session-publication-read directory)
                 before (mevedel-test-session-discard--snapshot directory)))
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _)
                      (setq prompted t)
                      (when (eq timing 'during)
                        (mevedel-test-session-discard--advance session)
                        (setq replacement (mevedel-session-publication-read directory)))
                      t)))
           (let ((err (should-error (mevedel-session-persistence-discard workspace entry)
                                    :type 'user-error)))
             (should (string-match-p "changed" (error-message-string err)))))
         (should (eq prompted (eq timing 'during)))
         (should (file-directory-p directory))
         (should (equal (plist-get replacement :head)
                        (plist-get (mevedel-session-publication-read directory) :head)))
         (should-not (equal (plist-get replacement :head)
                            (plist-get (plist-get entry :publication) :head)))
         (when before
           (should (equal before (mevedel-test-session-discard--snapshot directory))))))))

  :doc "cold recovery abandonment and omission survive final deletion failure with truthful retry"
  (mevedel-test-session-discard--with-cold
   (lambda (workspace entry capture _session)
     (let* ((directory (plist-get entry :save-path))
            (recovery (mevedel-session-recovery-read directory))
            (bytes (file-name-concat (plist-get recovery :directory) "files" "before.el")))
       (should (equal "original bytes" (mevedel-test-session-discard--bytes bytes)))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'mevedel-session-durability--delete-owned-session)
                 (lambda (&rest _)
                   (should-not (mevedel-session-recovery-read directory))
                   (should-not (mevedel-journal-pins-present-p directory))
                   (should-not
                    (plist-get (plist-get (mevedel-session-durability-cleanup-assessment directory)
                                          :head) :unsettled-mutation))
                   (error "Injected final deletion failure"))))
         (let ((err (should-error (mevedel-session-persistence-discard workspace entry)
                                  :type 'user-error)))
           (should (string-match-p "retained.*may have completed.*Injected final deletion failure"
                                   (error-message-string err)))))
       (should (file-directory-p directory))
       (should-not (file-exists-p bytes))
       (should-not (mevedel-session-recovery-read directory))
       (should-not (mevedel-journal-pins-present-p directory))
       (should (file-regular-p (mevedel-journal-capture--file workspace (plist-get capture :id) "discard.json")))
       (should (memq (plist-get (mevedel-session-durability-cleanup-assessment directory) :state)
                     '(available expired)))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (should (mevedel-session-persistence-discard workspace entry)))
       (should-not (file-exists-p directory))))
   (lambda (session _buffer _capture)
     (should (mevedel-session-durability-set-unsettled-mutation session t))
     (mevedel-test-session-discard--install-recovery session)))

  :doc "cold unsupported hidden recovery bytes survive approved marker abandonment"
  (mevedel-test-session-discard--with-cold
   (lambda (workspace entry capture _session)
     (let* ((directory (plist-get entry :save-path))
            (recovery (mevedel-session-recovery-read directory))
            (unknown (file-name-concat directory ".recovery" ".unknown"))
            (content "Unsupported recovery evidence\n"))
       (should (file-regular-p (plist-get recovery :marker)))
       (write-region content nil unknown nil 'silent)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (let ((err (should-error (mevedel-session-persistence-discard workspace entry)
                                  :type 'user-error)))
           (should (string-match-p "retained.*may have completed" (error-message-string err)))))
       (should (file-directory-p directory))
       (should (equal content (mevedel-test-session-discard--bytes unknown)))
       (should-not (file-exists-p (plist-get recovery :marker)))
       (should-not (file-exists-p (plist-get recovery :directory)))
       (should-not (mevedel-journal-pins-present-p directory))
       (should (file-regular-p (mevedel-journal-capture--file workspace (plist-get capture :id) "discard.json")))))
   (lambda (session _buffer _capture)
     (mevedel-test-session-discard--install-recovery session)))

  :doc "cold remote chooser entry discards through the target without restoration"
  (let ((local-root (file-name-as-directory (make-temp-file "mevedel-discard-remote-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("discard-remote")
          (cl-destructuring-bind (workspace _session directory _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               "discard-remote" local-root "Committed remote transcript\n")
            (let ((entry (car (mevedel-session-persistence-list-sessions workspace))))
              (should (file-remote-p (plist-get entry :save-path)))
              (should (equal directory (plist-get entry :save-path)))
              (should (file-directory-p (file-local-name directory)))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (should (mevedel-session-persistence-discard workspace entry)))
              (should-not (file-exists-p (file-local-name directory)))
              (should-not (mevedel-session-persistence-list-sessions workspace)))))
      (delete-directory local-root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-session-discard)
;;; test-mevedel-session-discard.el ends here
