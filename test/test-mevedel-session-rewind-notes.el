;;; test-mevedel-session-rewind-notes.el --- Rewind and Redo note settlement -*- lexical-binding: t -*-

;;; Commentary:

;; Tests that Rewind and Redo settle workspace notes through the store, and
;; that Rewind prunes in memory when no store file exists.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-chat)
(require 'mevedel-notes)
(require 'mevedel-session-rewind)
(require 'mevedel-persistence)
(require 'mevedel-session-recovery)

(mevedel-deftest mevedel-rewind-notes (:quiet t)
  ,test
  (test)
  :doc "without a store file, Rewind prunes the discarded turn's note activity in memory"
  ;; The store file is gone when the Rewind runs (deleted, or a failed
  ;; sync), so the notes live only in memory.
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (sid (mevedel-session-session-id session))
           (data-buf (generate-new-buffer " *test-no-store-rewind*"))
           (source-file (file-name-concat tempdir "source.el"))
           (change (lambda (turn) (list :id (format "c%d" turn)
                                        :session-id sid :turn turn)))
           (prepared (lambda (note)
                       (overlay-get note 'mevedel-note-prepared-change)))
           source kept pruned)
      (unwind-protect
          (progn
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "AAAAA\nBBBBB\n" nil source-file nil 'silent)
            (setq source (find-file-noselect source-file))
            (with-current-buffer source
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (setq kept (mevedel-note-create "kept"))
              (forward-line 1)
              (setq pruned (mevedel-note-create "pruned")))
            (mevedel-note-mark-sent (list kept pruned) sid)
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local mevedel--workspace workspace)
              (setq-local gptel-backend
                          (test-mevedel-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (progn
                (mevedel-session-artifacts-save session data-buf)
                (goto-char (point-max))
                (insert "First prompt\n")
                (let ((start (point)))
                  (insert "First reply.\n")
                  (put-text-property start (point) 'gptel 'response))
                (overlay-put kept 'mevedel-note-prepared-change (funcall change 1))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-artifacts-save session data-buf t)
                (goto-char (point-max))
                (insert "Second prompt\n")
                (let ((start (point)))
                  (insert "Second reply.\n")
                  (put-text-property start (point) 'gptel 'response))
                (overlay-put pruned 'mevedel-note-prepared-change (funcall change 2))
                (setf (mevedel-session-turn-count session) 2)
                (mevedel-session-artifacts-save session data-buf t)))
            (when (file-exists-p (mevedel-instruction-store-path workspace))
              (delete-file (mevedel-instruction-store-path workspace)))
            (let ((target
                   (copy-sequence
                    (cdr (cl-find-if
                          (lambda (entry)
                            (= 2 (plist-get (cdr entry) :cum-turn)))
                          (mevedel-session-rewind--prompt-candidates
                           session))))))
              (should target)
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'mevedel--run-session-start-hooks)
                         #'ignore))
                (should (mevedel-session-rewind-rewind data-buf target))))
            (should (= 1 (mevedel-session-turn-count session)))
            (let ((kept (mevedel--instruction-with-uuid
                         (overlay-get kept 'mevedel-uuid) workspace))
                  (pruned (mevedel--instruction-with-uuid
                           (overlay-get pruned 'mevedel-uuid) workspace)))
              ;; Turn 2's activity goes; turn 1's activity and both notes stay.
              (should-not (funcall prepared pruned))
              (should (equal (funcall change 1) (funcall prepared kept)))
              (should (eq 'sent (overlay-get kept 'mevedel-note-state)))
              (should (eq 'sent (overlay-get pruned 'mevedel-note-state)))))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))
        (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
          (kill-buffer impact))
        (test-mevedel-session-persistence--release-and-kill data-buf session)
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-redo-notes ()
  ,test
  (test)
  :doc "keeps other sessions' notes and later edits, and ignores an old note snapshot"
  ;; The restored head still carries an `instructions/current.el' written by
  ;; an older version, holding an earlier text of this session's note and
  ;; none of the other session's.  The store, not that file, says what the
  ;; notes are after the restore.
  (let* ((host "redo-notes")
         (local-root
          (file-name-as-directory (make-temp-file "mevedel-redo-notes-" t)))
         buffer workspace)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-persistence--reset-instructions)
          (cl-destructuring-bind (ws session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (setq workspace ws)
            (let* ((mevedel-session-durability--client-id (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (sid (mevedel-session-session-id session))
                   (note (lambda (id uuid text session-id)
                           (list :overlay-start 1 :overlay-end 6
                                 :anchor (list :uuid uuid :text "(defu")
                                 :properties
                                 (list 'mevedel-instruction t 'mevedel-id id
                                       'mevedel-uuid uuid
                                       'mevedel-instruction-type 'note
                                       'mevedel-note-text text
                                       'mevedel-note-author 'user
                                       'mevedel-note-state 'sent
                                       'mevedel-note-session-id session-id))))
                   (record (lambda (&rest notes)
                             (list :format mevedel-instruction-store-format-version
                                   :ids (list :id-counter 2 :used-ids '(1 2)
                                              :retired-ids nil)
                                   :files (list (list "source.el"
                                                       :original-content "(defun beta () t)\n"
                                                       :instructions notes))
                                   :rewinds nil))))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should (mevedel-session-durability-lease-acquire
                       session-dir "*redo-notes*" session))
              (unwind-protect
                  (cl-labels
                      ((publish (transcript turns &rest extra)
                         (setf (mevedel-session-turn-count session) turns)
                         (mevedel-session-publication-publish
                          session
                          (append
                           (list (list :path segment :content transcript))
                           extra
                           (list
                            (list :path
                                  (mevedel-session-artifacts-sidecar-path
                                   session-dir)
                                  :content
                                  (mevedel-session-artifacts-printed-value
                                   (mevedel-session-artifacts--live-sidecar
                                    session (current-buffer)))
                                  :commit-marker t :replace t)))
                          t)
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (setf (mevedel-session-publication session)
                          (mevedel-session-publication-read session-dir))
                    (set-file-times
                     (file-name-concat
                      session-dir
                      (plist-get (mevedel-session-publication session) :head))
                     (time-subtract nil 60))
                    (let ((restorable
                           (publish
                            "Original transcript\n" 1
                            (list :path (file-name-concat
                                         session-dir "instructions" "current.el")
                                  :content
                                  (mevedel-session-artifacts-printed-value
                                   (funcall record
                                            (funcall note 1 "N1" "mine v1" sid)))))))
                      (publish "Rewound transcript\n" 0)
                      ;; The store: this session's note edited since, and
                      ;; another session's note the old snapshot never held.
                      (should (mevedel-instruction-store--write
                               workspace nil
                               (funcall record
                                        (funcall note 1 "N1" "mine v2" sid)
                                        (funcall note 2 "N2" "theirs" "other"))))
                      ;; Emacs restarted since: memory holds nothing, observed nothing.
                      (test-mevedel-session-persistence--reset-instructions)
                      (setq buffer (generate-new-buffer " *redo-notes-live*"))
                      (with-current-buffer buffer
                        (org-mode)
                        (insert "Rewound transcript\n")
                        (setq-local mevedel--session session)
                        (setq buffer-file-name segment))
                      (cl-letf (((symbol-function 'completing-read)
                                 (lambda (&rest _)
                                   (mevedel-session-rewind--published-head-label
                                    (cl-find restorable
                                             (mevedel-session-rewind-published-heads
                                              session)
                                             :key (lambda (entry)
                                                    (plist-get entry :head))
                                             :test #'equal))))
                                ((symbol-function 'read-multiple-choice)
                                 (lambda (&rest _) '(?f "conversation and files")))
                                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                                ((symbol-function 'mevedel--run-session-start-hooks)
                                 #'ignore)
                                ((symbol-function
                                  'mevedel-session-recovery-refresh-session-buffers)
                                 #'ignore)
                                ((symbol-function 'message) #'ignore))
                        (with-current-buffer buffer
                          (should (mevedel-redo))))
                      (should (= 1 (mevedel-session-turn-count session)))
                      ;; Both notes are as the store says.
                      (should (equal '("mine v2" "theirs")
                                     (sort (cl-loop
                                            for file in
                                            (plist-get
                                             (mevedel--instruction-state
                                              (mevedel--instruction-workspace-key
                                               workspace))
                                             :instructions)
                                            append
                                            (mapcar
                                             (lambda (n)
                                               (plist-get (plist-get n :properties)
                                                          'mevedel-note-text))
                                             (plist-get (cdr file) :instructions)))
                                           #'string<)))))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when workspace (mevedel--clear-instruction-state workspace))
      (test-mevedel-session-persistence--reset-instructions)
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-session-rewind-notes)

;;; test-mevedel-session-rewind-notes.el ends here
