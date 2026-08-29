;;; test-mevedel-persistence.el -- Tests for mevedel-persistence.el -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for instruction snapshot persistence.

;;; Code:

(require 'mevedel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel--load-instructions-file
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "round-trips directive identity, execution binding, activity, and overlay"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-directive-persistence-" t)))
         (source (file-name-concat root "source.el"))
         (snapshot (file-name-concat root "instructions.el"))
         (workspace (mevedel-workspace--create
                     :type 'file :id root :root root :name "test"))
         source-buffer directive child id)
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "(message \"hello\")\n"))
          (setq source-buffer (find-file-noselect source))
          (with-current-buffer source-buffer
            (setq-local mevedel--workspace workspace)
            (setq directive
                  (mevedel--create-directive-in
                   source-buffer (point-min) (1- (point-max))
                   nil "Keep this request"))
            (setq id (mevedel-directive-id
                      (mevedel--directive-record directive)))
            (goto-char (point-min))
            (search-forward "hello")
            (setq child
                  (mevedel--create-directive-in
                   source-buffer (match-beginning 0) (match-end 0)
                   nil "Current nested detail"))
            (let ((record (mevedel--directive-record directive)))
              (setf (mevedel-directive-session-id record) "session-1"
                    (mevedel-directive-attempts record)
                    (list
                     (mevedel-directive-attempt--create
                      :sequence 1
                      :action 'implement
                      :directive-request "Keep this request"
                      :request "Exact submitted request"
                      :result "Exact answer"
                      :outcome 'success
                      :patch "diff --git a/source.el b/source.el\n"
                      :capture 'incomplete
                      :covered-files (list source)
                      :gaps (list (cons (file-name-concat root "missing.el")
                                        'not-observed))
                      :captured-at "2026-08-02T01:00:00+0200"
                      :checkpoint '(:session-id "session-1" :turn 3)
                      :consumed-subdirectives
                      (list
                       (mevedel-subdirective--create
                        :id "consumed-child" :request "Consumed detail"
                        :anchor
                        (list :state 'attached :file source
                              :start 2 :end 5 :properties nil)))))
                    (mevedel-directive-discussion record)
                    (list
                     (mevedel-directive-discussion-turn--create
                      :sequence 2
                      :directive-request "Keep this request"
                      :message "Why this change?"
                      :request "Exact discussion request"
                      :result "Because it is safer."
                      :outcome 'success
                      :attempt-index 1
                      :checkpoint '(:session-id "session-1" :turn 4)))
                    (mevedel-directive-state record) 'implemented))
            (mevedel--write-instructions-file snapshot root t t t)
            (mevedel--clear-instruction-state workspace)
            (mevedel--load-instructions-file
             snapshot root nil t workspace))
          (let* ((record (car (mevedel-workspace-directives workspace)))
                 (restored (mevedel--instruction-with-uuid id workspace)))
            (should (equal id (mevedel-directive-id record)))
            (should (equal "Keep this request"
                           (mevedel-directive-request record)))
            (should (equal source
                           (plist-get (mevedel-directive-anchor record)
                                      :file)))
            (should (equal "session-1" (mevedel-directive-session-id record)))
            (should (eq 'implemented (mevedel-directive-state record)))
            (let ((subdirective
                   (car (mevedel-directive-subdirectives record))))
              (should (equal "Current nested detail"
                             (mevedel-subdirective-request subdirective)))
              (should (equal (overlay-get child 'mevedel-uuid)
                             (mevedel-subdirective-id subdirective))))
            (let ((attempt (car (mevedel-directive-attempts record))))
              (should (= 1 (mevedel-directive-attempt-sequence attempt)))
              (should (equal "Keep this request"
                             (mevedel-directive-attempt-directive-request
                              attempt)))
              (should (equal "Exact submitted request"
                             (mevedel-directive-attempt-request attempt)))
              (should (equal "Exact answer"
                             (mevedel-directive-attempt-result attempt)))
              (should (eq 'success
                          (mevedel-directive-attempt-outcome attempt)))
              (should (eq 'incomplete
                          (mevedel-directive-attempt-capture attempt)))
              (should (equal (list source)
                             (mevedel-directive-attempt-covered-files attempt)))
              (should
               (equal (list (cons (file-name-concat root "missing.el")
                                  'not-observed))
                      (mevedel-directive-attempt-gaps attempt)))
              (should (equal '(:session-id "session-1" :turn 3)
                             (mevedel-directive-attempt-checkpoint attempt)))
              (should (equal "2026-08-02T01:00:00+0200"
                             (mevedel-directive-attempt-captured-at attempt)))
              (should
               (equal "Consumed detail"
                      (mevedel-subdirective-request
                       (car
                        (mevedel-directive-attempt-consumed-subdirectives
                         attempt))))))
            (let ((turn (car (mevedel-directive-discussion record))))
              (should (= 2
                         (mevedel-directive-discussion-turn-sequence turn)))
              (should (equal "Keep this request"
                             (mevedel-directive-discussion-turn-directive-request
                              turn)))
              (should (equal "Why this change?"
                             (mevedel-directive-discussion-turn-message turn)))
              (should (equal "Exact discussion request"
                             (mevedel-directive-discussion-turn-request turn)))
              (should (equal "Because it is safer."
                             (mevedel-directive-discussion-turn-result turn)))
              (should (eq 'success
                          (mevedel-directive-discussion-turn-outcome turn)))
              (should (= 1
                         (mevedel-directive-discussion-turn-attempt-index turn)))
              (should (equal '(:session-id "session-1" :turn 4)
                             (mevedel-directive-discussion-turn-checkpoint turn))))
            (should (overlayp restored))
            (should (eq record (mevedel--directive-record restored)))
            (should (eq 'implemented (mevedel--directive-state restored)))))
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (delete-directory root t)))

  :doc "patches outdated content without an external diff program"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-outdated-persistence-" t)))
         (source (file-name-concat root "source.el"))
         (snapshot (file-name-concat root "instructions.el"))
         (workspace (mevedel-workspace--create
                     :type 'file :id root :root root :name "test"))
         (original "alpha\nreference\nmiddle\ndirective\nomega\n")
         (current "prefix\nalpha\nreference\nmiddle inserted\ndirective\nomega\nsuffix\n")
         (mevedel-patch-outdated-instructions t)
         source-buffer reference-id directive-id diagnostics)
    (unwind-protect
        (progn
          (with-temp-file source (insert original))
          (setq source-buffer (find-file-noselect source))
          (with-current-buffer source-buffer
            (setq-local mevedel--workspace workspace)
            (goto-char (point-min))
            (search-forward "reference")
            (setq reference-id
                  (overlay-get
                   (mevedel--create-reference-in
                    source-buffer (match-beginning 0) (match-end 0))
                   'mevedel-uuid))
            (search-forward "directive")
            (setq directive-id
                  (overlay-get
                   (mevedel--create-directive-in
                    source-buffer (match-beginning 0) (match-end 0)
                    nil "Keep this request")
                   'mevedel-uuid))
            (mevedel--write-instructions-file snapshot root t t t)
            (mevedel--clear-instruction-state workspace)
            (erase-buffer)
            (insert current)
            (save-buffer))
          (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
            (mevedel-test--with-captured-diagnostics diagnostics
              (mevedel--load-instructions-file
               snapshot root nil t workspace)))
          (should mevedel-patch-outdated-instructions)
          (should-not (string-match-p "requires.*diff" diagnostics))
          (with-current-buffer source-buffer
            (should (equal current (buffer-string)))
            (dolist (pair `((,reference-id . "reference")
                            (,directive-id . "directive")))
              (let ((overlay (mevedel--instruction-with-uuid
                              (car pair) workspace)))
                (should (overlayp overlay))
                (should (equal (cdr pair)
                               (buffer-substring-no-properties
                                (overlay-start overlay)
                                (overlay-end overlay))))))))
      (mevedel--clear-instruction-state workspace)
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (delete-directory root t)))

  :doc "round-trips detached position, source order, activity, and actions"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-detached-persistence-" t)))
         (source (file-name-concat root "source.el"))
         (snapshot (file-name-concat root "instructions.el"))
         (workspace (mevedel-workspace--create
                     :type 'file :id root :root root :name "test"))
         source-buffer id)
    (unwind-protect
        (progn
          (with-temp-file source (insert "before\ntarget\nafter\n"))
          (setq source-buffer (find-file-noselect source))
          (with-current-buffer source-buffer
            (setq-local mevedel--workspace workspace)
            (let* ((directive
                    (mevedel--create-directive-in
                     source-buffer 8 14 nil "Detached request"))
                   (record (mevedel--directive-record directive)))
              (setq id (mevedel-directive-id record))
              (setf (mevedel-directive-state record) 'implemented
                    (mevedel-directive-attempts record)
                    (list (mevedel-directive-attempt--create
                           :sequence 1
                           :action 'implement
                           :directive-request "Detached request"
                           :request "Exact" :result "Done" :outcome 'success
                           :patch "" :capture 'complete
                           :captured-at "2026-08-02T01:00:00+0200"
                           :checkpoint '(:session-id "session" :turn 1))))
              (delete-region 8 14)
              (mevedel--write-instructions-file snapshot root t t t)
              (mevedel--clear-instruction-state workspace)
              (mevedel--load-instructions-file
               snapshot root nil t workspace)))
          (let* ((record (car (mevedel-workspace-directives workspace)))
                 (anchor (mevedel-directive-anchor record))
                 (restored (mevedel--instruction-with-uuid id workspace)))
            (should (eq 'detached (plist-get anchor :state)))
            (should (= 8 (plist-get anchor :position)))
            (should (equal '(8 14) (plist-get anchor :source-order)))
            (should (= 1 (length (mevedel-directive-attempts record))))
            (should (= (overlay-start restored) (overlay-end restored)))
            (should (keymapp (overlay-get restored 'keymap)))
            (should (string-match-p
                     "DETACHED.*IMPLEMENTED"
                     (substring-no-properties
                      (overlay-get restored 'before-string))))))
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (delete-directory root t)))

  :doc "rejects the superseded overlay-owned directive shape"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-old-directive-shape-" t)))
         (snapshot (file-name-concat root "instructions.el"))
         (workspace (mevedel-workspace--create
                     :type 'file :id root :root root :name "test")))
    (unwind-protect
        (progn
          (with-temp-file snapshot
            (prin1 (list :version (mevedel-version)
                         :ids '(:id-counter 0 :used-ids nil :retired-ids nil)
                         :files nil)
                   (current-buffer)))
          (should-error
           (mevedel--load-instructions-file snapshot root nil t workspace)
           :type 'user-error))
      (delete-directory root t))))

(mevedel-deftest mevedel--stash-instructions-on-kill
  ()
  ,test
  (test)
  :doc "leaves a buffer with no file alone, registry untouched"
  (let ((activations 0))
    (cl-letf (((symbol-function 'mevedel--instruction-activate-buffer)
               (lambda (&rest _) (setq activations (1+ activations)))))
      (with-temp-buffer
        (mevedel--stash-instructions-on-kill))
      ;; The global hook sees every buffer Emacs kills, internal ones
      ;; included; none of them can carry a stashable instruction.
      (should (zerop activations)))))


(mevedel-deftest mevedel--setup-buffer-hooks
  ()
  ,test
  (test)
  :doc "keeps the global kill hook at one entry and the rest buffer-local"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-buffer-hooks-" t)))
         (preinstalled (memq #'mevedel--stash-instructions-on-kill
                             (default-value 'kill-buffer-hook)))
         buffers)
    (unwind-protect
        (progn
          (dotimes (i 3)
            (let ((file (file-name-concat root (format "source-%d.el" i))))
              (with-temp-file file (insert ";; hook target\n"))
              (push (find-file-noselect file) buffers)))
          (let ((baseline (length (default-value 'kill-buffer-hook))))
            (dolist (buffer buffers)
              (mevedel--setup-buffer-hooks buffer)
              (mevedel--setup-buffer-hooks buffer))
            ;; Six setups may add at most the one named function; a
            ;; per-buffer closure on the global hook is the leak this
            ;; test exists to catch.
            (should (<= (length (default-value 'kill-buffer-hook))
                        (1+ baseline)))
            (should (= 1 (cl-count #'mevedel--stash-instructions-on-kill
                                   (default-value 'kill-buffer-hook)))))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (should mevedel--buffer-hooks-setup)
              (should (local-variable-p 'post-command-hook))
              (should (local-variable-p 'before-revert-hook))
              (should (local-variable-p 'after-revert-hook)))))
      (unless preinstalled
        (remove-hook 'kill-buffer-hook #'mevedel--stash-instructions-on-kill))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t))))

(mevedel-deftest mevedel-persistence--write-save-file ()
  ,test
  (test)
  :doc "a save that dies mid-write leaves the previous snapshot readable"
  ;; The file is the durable record of every workspace instruction; the
  ;; old in-place `with-temp-file' truncated it before writing, so a
  ;; crash lost all instructions at once.
  (let* ((root (make-temp-file "mevedel-save-atomic-" t))
         (path (file-name-concat root "instructions.eld")))
    (unwind-protect
        (progn
          (mevedel-persistence--write-save-file path '(:version 1 :files (a)))
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (error "Disk full"))))
            (should-error
             (mevedel-persistence--write-save-file
              path '(:version 2 :files (b)))))
          (should (equal '(:version 1 :files (a))
                         (with-temp-buffer
                           (insert-file-contents path)
                           (read (current-buffer)))))
          (should-not (directory-files root nil "mevedel-write")))
      (delete-directory root t))))

(provide 'test-mevedel-persistence)
;;; test-mevedel-persistence.el ends here
