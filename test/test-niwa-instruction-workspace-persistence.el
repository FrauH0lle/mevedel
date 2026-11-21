;;; test-niwa-instruction-workspace-persistence.el -- Reproductions for per-session instruction persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Instruction state is per workspace in memory.  It used to be persisted only
;; by session saves (`niwa-session-artifacts-save-instructions') and
;; restored by resume (`niwa-session-artifacts-load-instructions'), so
;; resuming one session replaced the workspace set.  Each case below
;; reproduces one consequence of that; the workspace instruction store fixes
;; all three.  The real save and load producers run against real files in a
;; temporary workspace.
;;
;; Every case first asserts a control: the store really holds the data and
;; the session wrote no snapshot, so the store alone carries the result.

;;; Code:

(require 'niwa-notes)

(require 'niwa)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))

(defmacro test-niwa-instruction-ws--with-workspace (vars &rest body)
  "Run BODY with WORKSPACE, SOURCE (a buffer) and DATA (a data buffer) bound.
VARS is (WORKSPACE SOURCE DATA).  Registries start empty and are cleaned up."
  (declare (indent 1))
  (let ((tempdir (make-symbol "tempdir")))
    `(cl-destructuring-bind (,(nth 0 vars) . ,tempdir)
         (test-niwa-session-persistence--make-tempdir-workspace)
       (let ((,(nth 1 vars) nil)
             (,(nth 2 vars) (generate-new-buffer "*test-data-buf*"))
             (file (file-name-concat ,tempdir "source.el"))
             (sessions nil))
         (unwind-protect
             (progn
               (test-niwa-session-persistence--reset-instructions)
               (write-region "(defun beta () t)\n(defun gamma () t)\n"
                             nil file nil 'silent)
               (setq ,(nth 1 vars) (find-file-noselect file))
               (with-current-buffer ,(nth 1 vars)
                 (setq-local niwa--workspace ,(nth 0 vars)))
               ,@body)
           (dolist (s sessions)
             (when (niwa-session-save-path s)
               (niwa-session-persistence-lock-release
                (niwa-session-save-path s) s)))
           (when (buffer-live-p ,(nth 2 vars))
             (with-current-buffer ,(nth 2 vars) (set-buffer-modified-p nil))
             (kill-buffer ,(nth 2 vars)))
           (when (buffer-live-p ,(nth 1 vars))
             (with-current-buffer ,(nth 1 vars) (set-buffer-modified-p nil))
             (kill-buffer ,(nth 1 vars)))
           (delete-directory ,tempdir t)
           (test-niwa-session-persistence--reset-instructions)
           (niwa-workspace-clear-registry))))))

(defun test-niwa-instruction-ws--save (name workspace data)
  "Create session NAME in WORKSPACE and save it from DATA like a turn save.
Returns the session."
  (let ((session (niwa-session-create name workspace)))
    (setf (niwa-session-turn-count session) 1)
    (with-current-buffer data
      (setq-local niwa--workspace workspace)
      (setq-local niwa--session session)
      (org-mode)
      (erase-buffer)
      (insert "turn of " name "\n")
      (niwa-session-artifacts-save session data))
    session))

(defun test-niwa-instruction-ws--resume (session workspace data)
  "Restore SESSION's instructions into WORKSPACE from DATA as resume does.
Mirrors the call in `niwa-session-persistence--restore-buffer'."
  (with-current-buffer data
    (setq-local niwa--workspace workspace)
    (niwa-session-artifacts-load-instructions session)))

(defun test-niwa-instruction-ws--note (source text line)
  "Create note TEXT on LINE of SOURCE and return its overlay."
  (with-current-buffer source
    (goto-char (point-min))
    (forward-line (1- line))
    (niwa-note-create text)))

(defun test-niwa-instruction-ws--texts (workspace)
  "Return the sorted note texts of WORKSPACE."
  (sort (mapcar (lambda (note) (plist-get (plist-get note :properties) 'niwa-note-text))
                (niwa-notes-open workspace))
        #'string<))

(defun test-niwa-instruction-ws--note-count (workspace source)
  "Return how many live notes SOURCE holds in WORKSPACE."
  (niwa--instruction-activate-workspace workspace)
  (cl-count-if (lambda (instruction)
                 (eq 'note (overlay-get instruction 'niwa-instruction-type)))
               (alist-get source (niwa--instruction-alist))))

(defun test-niwa-instruction-ws--activity (workspace)
  "Return (SESSION-ID CHANGE-ID) of WORKSPACE's first note."
  (let ((properties (plist-get (car (niwa-notes-open workspace)) :properties)))
    (list (plist-get properties 'niwa-note-session-id)
          (plist-get (plist-get properties 'niwa-note-prepared-change) :id))))

(defun test-niwa-instruction-ws--store-text (workspace)
  "Return the raw workspace store text of WORKSPACE."
  (with-temp-buffer
    (insert-file-contents (niwa-instruction-store-path workspace))
    (buffer-string)))

(defun test-niwa-instruction-ws--no-snapshot-p (session)
  "Return non-nil when SESSION wrote no instruction snapshot."
  (not (file-exists-p
        (file-name-concat (niwa-session-save-path session) "instructions"))))

(niwa-deftest test-niwa-instruction-ws-resume-drops-other-session
  (:quiet t)
  ,test
  (test)
  :doc "case 1: resuming session A keeps notes saved by B"
  (test-niwa-instruction-ws--with-workspace (workspace source data)
    (test-niwa-instruction-ws--note source "made before A saved" 1)
    (let ((a (test-niwa-instruction-ws--save "a" workspace data)))
      (push a sessions)
      (test-niwa-instruction-ws--note source "made in B" 2)
      (let ((b (test-niwa-instruction-ws--save "b" workspace data)))
        (push b sessions)
        ;; Control: the store holds B's note; B wrote no snapshot.
        (should (string-match-p "made in B"
                                (test-niwa-instruction-ws--store-text workspace)))
        (should (test-niwa-instruction-ws--no-snapshot-p b))
        (should (= 2 (test-niwa-instruction-ws--note-count workspace source)))
        ;; Resume A; the unchanged workspace still owns B's notes.
        (test-niwa-instruction-ws--resume a workspace data)
        (should (equal '("made before A saved" "made in B")
                       (test-niwa-instruction-ws--texts workspace)))
        (should (= 2 (test-niwa-instruction-ws--note-count workspace source)))))))

(niwa-deftest test-niwa-instruction-ws-unsaved-lost-on-restart
  (:quiet t)
  ,test
  (test)
  :doc "case 2: notes created after the last session save survive a restart"
  (test-niwa-instruction-ws--with-workspace (workspace source data)
    (test-niwa-instruction-ws--note source "saved with the turn" 1)
    (let ((a (test-niwa-instruction-ws--save "a" workspace data)))
      (push a sessions)
      (test-niwa-instruction-ws--note source "created, then Emacs exits" 2)
      ;; Control: the live workspace has both and the session wrote no
      ;; snapshot; the workspace store is the carrier.
      (should (= 2 (length (niwa-notes-open workspace))))
      (should (test-niwa-instruction-ws--no-snapshot-p a))
      (should (file-exists-p (niwa-instruction-store-path workspace)))
      ;; Restart: fresh registry, then resume the only session.
      (with-current-buffer source (set-buffer-modified-p nil))
      (niwa--clear-instruction-state workspace)
      (test-niwa-session-persistence--reset-instructions)
      (test-niwa-instruction-ws--resume a workspace data)
      (should (equal '("created, then Emacs exits" "saved with the turn")
                     (test-niwa-instruction-ws--texts workspace))))))

(niwa-deftest test-niwa-instruction-ws-activity-follows-last-saver
  (:quiet t)
  ,test
  (test)
  :doc "case 3: note activity is the same whichever session resumes"
  (test-niwa-instruction-ws--with-workspace (workspace source data)
    (let ((note (test-niwa-instruction-ws--note source "do it" 1))
          a b)
      (setq a (test-niwa-instruction-ws--save "a" workspace data))
      (push a sessions)
      ;; Activity recorded on the workspace note; only B saves afterwards.
      (overlay-put note 'niwa-note-state 'sent)
      (overlay-put note 'niwa-note-session-id "ran-in-b")
      (overlay-put note 'niwa-note-prepared-change
                   '(:id "change-1" :session-id "ran-in-b" :turn 1))
      (setq b (test-niwa-instruction-ws--save "b" workspace data))
      (push b sessions)
      ;; Control: the store carries the activity; neither session wrote a
      ;; snapshot.
      (should (string-match-p "ran-in-b"
                              (test-niwa-instruction-ws--store-text workspace)))
      (should (test-niwa-instruction-ws--no-snapshot-p a))
      (should (test-niwa-instruction-ws--no-snapshot-p b))
      (test-niwa-instruction-ws--resume b workspace data)
      (let ((via-b (test-niwa-instruction-ws--activity workspace)))
        (should (equal '("ran-in-b" "change-1") via-b))
        (test-niwa-instruction-ws--resume a workspace data)
        (should (equal via-b
                       (test-niwa-instruction-ws--activity workspace)))))))

(provide 'test-niwa-instruction-workspace-persistence)
;;; test-niwa-instruction-workspace-persistence.el ends here
