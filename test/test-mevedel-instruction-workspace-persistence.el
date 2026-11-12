;;; test-mevedel-instruction-workspace-persistence.el -- Reproductions for per-session instruction persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Instruction state is per workspace in memory.  It used to be persisted only
;; by session saves (`mevedel-session-artifacts-save-instructions') and
;; restored by resume (`mevedel-session-artifacts-load-instructions'), so
;; resuming one session replaced the workspace set.  Each case below
;; reproduces one consequence of that; the workspace instruction store fixes
;; all three.  The real save and load producers run against real files in a
;; temporary workspace.
;;
;; Every case first asserts a control: the session snapshot really holds
;; (or lacks) the data, so the store, not the snapshot, carries the result.

;;; Code:

(require 'mevedel-notes)

(require 'mevedel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-instruction-test-support"))
(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))

(defmacro test-mevedel-instruction-ws--with-workspace (vars &rest body)
  "Run BODY with WORKSPACE, SOURCE (a buffer) and DATA (a data buffer) bound.
VARS is (WORKSPACE SOURCE DATA).  Registries start empty and are cleaned up."
  (declare (indent 1))
  (let ((tempdir (make-symbol "tempdir")))
    `(cl-destructuring-bind (,(nth 0 vars) . ,tempdir)
         (test-mevedel-session-persistence--make-tempdir-workspace)
       (let ((,(nth 1 vars) nil)
             (,(nth 2 vars) (generate-new-buffer "*test-data-buf*"))
             (file (file-name-concat ,tempdir "source.el"))
             (sessions nil))
         (unwind-protect
             (progn
               (test-mevedel-session-persistence--reset-instructions)
               (write-region "(defun beta () t)\n(defun gamma () t)\n"
                             nil file nil 'silent)
               (setq ,(nth 1 vars) (find-file-noselect file))
               (with-current-buffer ,(nth 1 vars)
                 (setq-local mevedel--workspace ,(nth 0 vars)))
               ,@body)
           (dolist (s sessions)
             (when (mevedel-session-save-path s)
               (mevedel-session-persistence-lock-release
                (mevedel-session-save-path s) s)))
           (when (buffer-live-p ,(nth 2 vars))
             (with-current-buffer ,(nth 2 vars) (set-buffer-modified-p nil))
             (kill-buffer ,(nth 2 vars)))
           (when (buffer-live-p ,(nth 1 vars))
             (with-current-buffer ,(nth 1 vars) (set-buffer-modified-p nil))
             (kill-buffer ,(nth 1 vars)))
           (delete-directory ,tempdir t)
           (test-mevedel-session-persistence--reset-instructions)
           (mevedel-workspace-clear-registry))))))

(defun test-mevedel-instruction-ws--save (name workspace data)
  "Create session NAME in WORKSPACE and save it from DATA like a turn save.
Returns the session."
  (let ((session (mevedel-session-create name workspace)))
    (setf (mevedel-session-turn-count session) 1)
    (with-current-buffer data
      (setq-local mevedel--workspace workspace)
      (setq-local mevedel--session session)
      (org-mode)
      (erase-buffer)
      (insert "turn of " name "\n")
      (mevedel-session-artifacts-save session data))
    session))

(defun test-mevedel-instruction-ws--resume (session workspace data)
  "Restore SESSION's instructions into WORKSPACE from DATA as resume does.
Mirrors the call in `mevedel-session-persistence--restore-buffer'."
  (with-current-buffer data
    (setq-local mevedel--workspace workspace)
    (mevedel-session-artifacts-load-instructions session data)))

(defun test-mevedel-instruction-ws--requests (workspace)
  "Return the sorted directive requests of WORKSPACE."
  (sort (mapcar #'mevedel-directive-request
                (mevedel-workspace-directives workspace))
        #'string<))

(defun test-mevedel-instruction-ws--reference-count (workspace source)
  "Return how many live notes SOURCE holds in WORKSPACE."
  (mevedel--instruction-activate-workspace workspace)
  (cl-count-if (lambda (instruction)
                               (eq 'note (overlay-get instruction 'mevedel-instruction-type)))
               (alist-get source (mevedel--instruction-alist))))

(defun test-mevedel-instruction-ws--activity (workspace)
  "Return (SESSION-ID ATTEMPT-COUNT) of WORKSPACE's first directive."
  (let ((record (car (mevedel-workspace-directives workspace))))
    (list (mevedel-directive-session-id record)
          (length (mevedel-directive-attempts record)))))

(defun test-mevedel-instruction-ws--snapshot-text (session)
  "Return the raw current instruction snapshot text of SESSION."
  (with-temp-buffer
    (insert-file-contents
     (mevedel-session-artifacts-instructions-current-path
      (mevedel-session-save-path session)))
    (buffer-string)))

(mevedel-deftest test-mevedel-instruction-ws-resume-drops-other-session
  (:quiet t)
  ,test
  (test)
  :doc "case 1: resuming session A keeps directives and notes saved by B"
  (test-mevedel-instruction-ws--with-workspace (workspace source data)
    (with-current-buffer source
      (mevedel--create-directive-in source (point-min) (+ 5 (point-min))
                                    nil "made before A saved"))
    (let ((a (test-mevedel-instruction-ws--save "a" workspace data)))
      (push a sessions)
      (with-current-buffer source
        (mevedel--create-directive-in source (+ 6 (point-min)) (+ 9 (point-min))
                                      nil "made in B")
        (mevedel-instruction-test--create-note-in source (point-min) (point-max)))
      (let ((b (test-mevedel-instruction-ws--save "b" workspace data)))
        (push b sessions)
        ;; Control: B's file really holds B's directive and note.
        (should (string-match-p "made in B"
                                (test-mevedel-instruction-ws--snapshot-text b)))
        (should (= 1 (test-mevedel-instruction-ws--reference-count
                      workspace source)))
        ;; Resume A; the unchanged workspace still owns B's instructions.
        (test-mevedel-instruction-ws--resume a workspace data)
        (should (equal '("made before A saved" "made in B")
                       (test-mevedel-instruction-ws--requests workspace)))
        (should (= 1 (test-mevedel-instruction-ws--reference-count
                      workspace source)))))))

(mevedel-deftest test-mevedel-instruction-ws-unsaved-lost-on-restart
  (:quiet t)
  ,test
  (test)
  :doc "case 2: directives created after the last session save survive a restart"
  (test-mevedel-instruction-ws--with-workspace (workspace source data)
    (with-current-buffer source
      (mevedel--create-directive-in source (point-min) (+ 5 (point-min))
                                    nil "saved with the turn"))
    (let ((a (test-mevedel-instruction-ws--save "a" workspace data)))
      (push a sessions)
      (with-current-buffer source
        (mevedel--create-directive-in source (+ 6 (point-min)) (+ 9 (point-min))
                                      nil "created, then Emacs exits"))
      ;; Control: the live workspace has both, the only session snapshot
      ;; has one; the workspace store is the other carrier.
      (should (= 2 (length (mevedel-workspace-directives workspace))))
      (should-not (string-match-p
                   "then Emacs exits"
                   (test-mevedel-instruction-ws--snapshot-text a)))
      (should (file-exists-p (mevedel-instruction-store-path workspace)))
      ;; Restart: fresh registry, then resume the only session.
      (with-current-buffer source (set-buffer-modified-p nil))
      (mevedel--clear-instruction-state workspace)
      (test-mevedel-session-persistence--reset-instructions)
      (test-mevedel-instruction-ws--resume a workspace data)
      (should (equal '("created, then Emacs exits" "saved with the turn")
                     (test-mevedel-instruction-ws--requests workspace))))))

(mevedel-deftest test-mevedel-instruction-ws-activity-follows-last-saver
  (:quiet t)
  ,test
  (test)
  :doc "case 3: directive activity is the same whichever session resumes"
  (test-mevedel-instruction-ws--with-workspace (workspace source data)
    (let (directive a b)
      (with-current-buffer source
        (setq directive
              (mevedel--create-directive-in source (point-min)
                                            (+ 5 (point-min)) nil "do it")))
      (setq a (test-mevedel-instruction-ws--save "a" workspace data))
      (push a sessions)
      ;; Activity recorded in the workspace record; only B saves afterwards.
      (let ((record (mevedel--directive-record directive)))
        (setf (mevedel-directive-session-id record) "ran-in-b"
              (mevedel-directive-attempts record)
              (list (mevedel-directive-attempt--create
                     :sequence 1 :action 'implement
                     :directive-request "do it" :request "do it"
                     :result "done" :outcome (quote success) :patch ""
                     :capture (quote incomplete) :covered-files nil :gaps nil
                     :checkpoint (quote (:session-id "ran-in-b" :turn 1))
                     :consumed-subdirectives nil
                     :captured-at "2026-11-07T00:00:00+0100"))))
      (setq b (test-mevedel-instruction-ws--save "b" workspace data))
      (push b sessions)
      ;; Control: B's file carries the activity, A's does not.
      (should (string-match-p "ran-in-b"
                              (test-mevedel-instruction-ws--snapshot-text b)))
      (should-not (string-match-p "ran-in-b"
                                  (test-mevedel-instruction-ws--snapshot-text a)))
      (test-mevedel-instruction-ws--resume b workspace data)
      (let ((via-b (test-mevedel-instruction-ws--activity workspace)))
        (should (equal '("ran-in-b" 1) via-b))
        (test-mevedel-instruction-ws--resume a workspace data)
        (should (equal via-b
                       (test-mevedel-instruction-ws--activity workspace)))))))

(provide 'test-mevedel-instruction-workspace-persistence)
;;; test-mevedel-instruction-workspace-persistence.el ends here
