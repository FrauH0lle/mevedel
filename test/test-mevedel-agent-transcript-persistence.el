;;; test-mevedel-agent-transcript-persistence.el --- Agent transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;; Durable Agent V2 transcript metadata, files, context, and root mailbox.

;;; Code:

(require 'gptel)
(require 'mevedel)
(require 'mevedel-agents)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun test-mevedel-agent-transcript--workspace ()
  "Return a fresh workspace and its temporary root."
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-agent-transcript-" t)))
         (name (file-name-nondirectory (directory-file-name root))))
    (mevedel-workspace-clear-registry)
    (cons (mevedel-workspace-get-or-create
           'project name root name)
          root)))

(defun test-mevedel-agent-transcript--release (buffer session root)
  "Release SESSION, kill BUFFER, and delete ROOT."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (mevedel-session-persistence-lock-release save-path))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)
      (setq-local kill-buffer-hook nil))
    (kill-buffer buffer))
  (when (file-directory-p root)
    (delete-directory root t))
  (mevedel-workspace-clear-registry))


;;
;;; Historical transcript metadata

(mevedel-deftest mevedel-session-persistence--sanitize-agent-transcripts ()
  ,test
  (test)
  :doc "preserves canonical paths and known statuses"
  (let* ((entry '("agent--one"
                  :agent-path "/root/one"
                  :path "agents/one.chat.org"
                  :status completed
                  :updated-at "2026-07-19T10-00-00"))
         (out (mevedel-session-persistence--sanitize-agent-transcripts
               (list entry))))
    (should (equal out (list entry))))

  :doc "coerces unknown historical status to incomplete"
  (let ((out
         (mevedel-session-persistence--sanitize-agent-transcripts
          '(("agent--one" :agent-path "/root/one"
             :path "agents/one.chat.org" :status bogus)))))
    (should (eq 'incomplete (plist-get (cdar out) :status))))

  :doc "keeps the newest metadata for a duplicate internal id"
  (let ((out
         (mevedel-session-persistence--sanitize-agent-transcripts
          '(("agent--one" :agent-path "/root/one" :status completed
             :updated-at "2026-07-19T10-00-00")
            ("agent--one" :agent-path "/root/one" :status aborted
             :updated-at "2026-07-19T11-00-00")))))
    (should (= 1 (length out)))
    (should (eq 'aborted (plist-get (cdar out) :status)))))

(mevedel-deftest mevedel-session-persistence-agent-transcripts-roundtrip ()
  ,test
  (test)
  :doc "round-trips canonical historical transcript metadata"
  (cl-destructuring-bind (workspace . root)
      (test-mevedel-agent-transcript--workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (entry
                '("agent--one" :agent-path "/root/one"
                  :description "Historical agent"
                  :path "agents/one.chat.org"
                  :status completed :parent-turn 3)))
          (setf (mevedel-session-agent-transcripts session) (list entry))
          (let* ((sidecar
                  (mevedel-session-persistence-serialize session))
                 (restored
                  (plist-get
                   (mevedel-session-persistence-deserialize sidecar)
                   :session)))
            (should
             (equal (list entry)
                    (mevedel-session-agent-transcripts restored)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--prune-agent-transcripts-after-fork ()
  ,test
  (test)
  :doc "keeps only historical transcripts within the forked turn"
  (cl-destructuring-bind (workspace . root)
      (test-mevedel-agent-transcript--workspace)
    (unwind-protect
        (let ((session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-agent-transcripts session)
                '(("agent--early" :agent-path "/root/early"
                   :parent-turn 2 :status completed)
                  ("agent--late" :agent-path "/root/late"
                   :parent-turn 6 :status completed)))
          (mevedel-session-persistence--prune-agent-transcripts-after-fork
           session 5)
          (should
           (equal '("agent--early")
                  (mapcar #'car
                          (mevedel-session-agent-transcripts session)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


;;
;;; Conversation files

(mevedel-deftest mevedel-session-persistence--shallow-ensure-files ()
  ,test
  (test)
  :doc "creates the locked agent directory before the first root turn"
  (cl-destructuring-bind (workspace . root)
      (test-mevedel-agent-transcript--workspace)
    (let ((session (mevedel-session-create "main" workspace))
          (buffer (generate-new-buffer " *agent-transcript-root*")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (let ((save-path
                   (mevedel-session-persistence--shallow-ensure-files
                    session buffer)))
              (should (file-directory-p
                       (file-name-concat save-path "agents")))
              (should (file-exists-p
                       (file-name-concat save-path ".lock")))
              (should-not (file-exists-p
                           (file-name-concat save-path "session.meta.el")))
              (should
               (equal save-path
                      (mevedel-session-persistence--shallow-ensure-files
                       session buffer)))))
        (test-mevedel-agent-transcript--release buffer session root)))))

;;
;;; Root mailbox

(mevedel-deftest mevedel-session-persistence-messages-roundtrip ()
  ,test
  (test)
  :doc "round-trips canonical unread root mail and results"
  (cl-destructuring-bind (workspace . root)
      (test-mevedel-agent-transcript--workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (messages
                '((:type RESULT :sender "/root/worker" :recipient "/root"
                   :outcome completed :payload "done"
                   :timestamp (12345 67890 0 0))
                  (:type MAIL :sender "/root/reviewer" :recipient "/root"
                   :payload "note" :timestamp (12345 67891 0 0)))))
          (setf (mevedel-session-messages session) messages)
          (let* ((sidecar (mevedel-session-persistence-serialize session))
                 (restored
                  (plist-get
                   (mevedel-session-persistence-deserialize sidecar)
                   :session)))
            (let ((actual (mevedel-session-messages restored)))
              (should (= 2 (length actual)))
              (should (equal "/root/worker"
                             (plist-get (car actual) :sender)))
              (should (eq 'completed
                          (plist-get (car actual) :outcome)))
              (should (equal "done" (plist-get (car actual) :payload)))
              (should (equal "/root/reviewer"
                             (plist-get (cadr actual) :sender)))
              (should (equal "note"
                             (plist-get (cadr actual) :payload))))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-agent-transcript-persistence)

;;; test-mevedel-agent-transcript-persistence.el ends here
