;;; test-niwa-session-artifacts-applied.el --- Applied prepared changes -*- lexical-binding: t -*-

;;; Commentary:

;; Recording an applied prepared change under its originating turn, in real
;; temporary file and portable sessions with real source and backup files.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))
(require 'niwa-session-artifacts)
(require 'niwa-session-publication)
(require 'niwa-session-rewind)

(defvar test-niwa-applied--root nil "Root of the running fixture.")

(defmacro test-niwa-applied--with-session (portable &rest body)
  "Run BODY with `session', `buffer' and `root' for a materialized turn 1.
PORTABLE selects a portable session over a file one."
  (declare (indent 1))
  `(let* ((root (make-temp-file "niwa-applied-" t))
          (workspace (if ,portable
                         (test-niwa-session-persistence--make-workspace root)
                       (test-niwa-session-persistence--make-file-workspace root)))
          (session (niwa-session-create "main" workspace))
          (buffer (generate-new-buffer " *applied*")))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (org-mode)
             (setq-local niwa--session session)
             (insert "Request\n" (propertize "Response\n" 'gptel 'response)))
           (setf (niwa-session-turn-count session) 1)
           (niwa-session-artifacts-save session buffer t)
           ,@body)
       (test-niwa-session-persistence--release-and-kill buffer session)
       (delete-directory root t)
       (niwa-workspace-clear-registry))))

(defun test-niwa-applied--file (root name content)
  "Write CONTENT to NAME under ROOT and return its path."
  (let ((path (file-name-concat root name)))
    (write-region content nil path nil 'silent)
    path))

(defun test-niwa-applied--change (path before)
  "Return a prepared change of PATH whose source held BEFORE bytes."
  (list :path path :before (list :path path :exists t :bytes before)))

(defun test-niwa-applied--seed (session turn &rest pairs)
  "Capture current bytes under SESSION's TURN for PAIRS of path and pre-content."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) table))
    ;; Portable pre-images become readable only once published, as
    ;; `niwa-session-artifacts-record-applied' publishes its own.
    (let ((niwa-session-artifacts--collecting-critical-artifacts-p
           (niwa-session-codec-portable-authority-p session))
          (niwa-session-artifacts--critical-artifacts nil))
      (niwa-session-artifacts-snapshot-modified session turn table)
      (when niwa-session-artifacts--critical-artifacts
        (niwa-session-publication-publish
         session (nreverse niwa-session-artifacts--critical-artifacts) t)
        (niwa-session-artifacts-save
         session (niwa-session-root-buffer session) nil t)))))

(defun test-niwa-applied--entry (session turn path)
  "Return SESSION's TURN checkpoint plist for PATH."
  (cdr (assoc path (cdr (assq turn (niwa-session-file-snapshots session))))))

(niwa-deftest niwa-session-artifacts-record-applied
  (:quiet t)
  ,test
  (test)
  :doc "entries for untouched paths survive the turn re-snapshot"
  (progn
    (test-niwa-applied--with-session nil
      (let ((a (test-niwa-applied--file root "a.txt" "a1"))
            (b (test-niwa-applied--file root "b.txt" "b1")))
        (test-niwa-applied--seed session 1 b "b0")
        (test-niwa-applied--file root "a.txt" "a2")
        (niwa-session-artifacts-record-applied
         session 1 (list (test-niwa-applied--change a "a1")))
        (should (test-niwa-applied--entry session 1 a))
        (should (equal (list a b)
                       (mapcar #'car (cdr (assq 1 (niwa-session-file-snapshots session))))))
        (should (plist-get (test-niwa-applied--entry session 1 b) :pre-backup-name)))))
  :doc "an existing gap is kept and later edits give an edited-after gap"
  (test-niwa-applied--with-session nil
    (let ((a (test-niwa-applied--file root "a.txt" "a1"))
          (b (test-niwa-applied--file root "b.txt" "b1")))
      (test-niwa-applied--seed session 1 a '(:gap "lost") b "b0")
      (test-niwa-applied--file root "a.txt" "a2")
      (test-niwa-applied--file root "b.txt" "b2")
      (niwa-session-artifacts-record-applied
       session 1 (list (test-niwa-applied--change a "a1")
                       (test-niwa-applied--change b "elsewhere")))
      (should (equal "lost" (plist-get (test-niwa-applied--entry session 1 a) :gap)))
      (should (equal "edited after turn 1"
                     (plist-get (test-niwa-applied--entry session 1 b) :gap)))))
  :doc "matching bytes keep the previous pre-image; a new path uses the change's bytes"
  (test-niwa-applied--with-session nil
    (let ((a (test-niwa-applied--file root "a.txt" "a1"))
          (c (test-niwa-applied--file root "c.txt" "c1")))
      (test-niwa-applied--seed session 1 a "a0")
      (test-niwa-applied--file root "a.txt" "a2")
      (test-niwa-applied--file root "c.txt" "c2")
      (niwa-session-artifacts-record-applied
       session 1 (list (test-niwa-applied--change a "a1")
                       (test-niwa-applied--change c "c1")))
      (should (equal "a0" (niwa-session-artifacts-read-backup
                           session (plist-get (test-niwa-applied--entry session 1 a)
                                              :pre-backup-name))))
      (should (equal "c1" (niwa-session-artifacts-read-backup
                           session (plist-get (test-niwa-applied--entry session 1 c)
                                              :pre-backup-name))))))
  :doc "backups are readable from the committed publication"
  (progn
    (test-niwa-applied--with-session nil
      (let ((a (test-niwa-applied--file root "a.txt" "a1")))
        (test-niwa-applied--file root "a.txt" "a2")
        (niwa-session-artifacts-record-applied
         session 1 (list (test-niwa-applied--change a "a1")))
        (let ((entry (test-niwa-applied--entry session 1 a)))
          (should (equal "a2" (niwa-session-artifacts-read-backup
                               session (plist-get entry :backup-name))))
          (should (equal "a1" (niwa-session-artifacts-read-backup
                               session (plist-get entry :pre-backup-name))))))))
  :doc "application backups only on request, from the change's own bytes"
  (test-niwa-applied--with-session nil
    (let ((a (test-niwa-applied--file root "a.txt" "a1"))
          (b (test-niwa-applied--file root "b.txt" "b1"))
          seen)
      (test-niwa-applied--seed session 1 a "a0")
      (test-niwa-applied--file root "a.txt" "a2")
      (test-niwa-applied--file root "b.txt" "b2")
      (niwa-session-artifacts-record-applied
       session 1 (list (test-niwa-applied--change a "a1"))
       :then (lambda (files) (push (list 'without files) seen)))
      (niwa-session-artifacts-record-applied
       session 1 (list (test-niwa-applied--change b "b1"))
       :application-backups t
       :then (lambda (files) (push (list 'with files) seen)))
      (should (null (cadr (assq 'without seen))))
      (let ((entry (cdr (car (cadr (assq 'with seen))))))
        (should (equal "b1" (niwa-session-artifacts-read-backup
                             session (plist-get entry :pre-backup-name))))
        (should-not (plist-member entry :version))
        (should-not (plist-member entry :backup-time)))))
  :doc "request snapshots receive pre-content only for the live turn"
  (test-niwa-applied--with-session nil
    (let ((a (test-niwa-applied--file root "a.txt" "a1")))
      (test-niwa-applied--file root "a.txt" "a2")
      (dolist (live '(1 2))
        (let ((request (niwa-request--create
                        :turn live :file-snapshots (make-hash-table :test #'equal))))
          (with-current-buffer buffer (setq-local niwa--current-request request))
          (niwa-session-artifacts-record-applied
           session 1 (list (test-niwa-applied--change a "a1")))
          (should (equal (= live 1)
                         (and (gethash a (niwa-request-file-snapshots request)) t)))))))
  :doc "the live turn is not re-snapshotted over the merged cell"
  (test-niwa-applied--with-session nil
    (let ((a (test-niwa-applied--file root "a.txt" "a1"))
          (b (test-niwa-applied--file root "b.txt" "b1"))
          (request (niwa-request--create
                    :turn 1 :file-snapshots (make-hash-table :test #'equal))))
      (test-niwa-applied--seed session 1 b "b0")
      (test-niwa-applied--file root "a.txt" "a2")
      (with-current-buffer buffer (setq-local niwa--current-request request))
      (niwa-session-artifacts-record-applied
       session 1 (list (test-niwa-applied--change a "a1")))
      (should (test-niwa-applied--entry session 1 a))
      (should (test-niwa-applied--entry session 1 b))))
  :doc "THEN runs once on success; failures stop it and the save"
  (test-niwa-applied--with-session t
    (let ((a (test-niwa-applied--file root "a.txt" "a1"))
          (saves 0) (thens 0)
          (save (symbol-function 'niwa-session-artifacts-save)))
      (test-niwa-applied--file root "a.txt" "a2")
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest args) (cl-incf saves) (apply save args))))
        (niwa-session-artifacts-record-applied
         session 1 (list (test-niwa-applied--change a "a1"))
         :then (lambda (_) (cl-incf thens)))
        (should (equal '(1 1) (list thens saves)))
        (test-niwa-applied--file root "a.txt" "a3")
        (cl-letf (((symbol-function 'niwa-session-publication-publish)
                   (lambda (&rest _) (error "Publication failed"))))
          (should-error
           (niwa-session-artifacts-record-applied
            session 1 (list (test-niwa-applied--change a "a2"))
            :then (lambda (_) (cl-incf thens)))))
        (should (equal '(1 1) (list thens saves)))
        (should-error
         (niwa-session-artifacts-record-applied
          session 1 (list (test-niwa-applied--change a "a3"))
          :then (lambda (_) (cl-incf thens) (error "Then failed"))))
        (should (equal '(2 1) (list thens saves))))))
  :doc "a failed pre-image read stops THEN and the save"
  (test-niwa-applied--with-session nil
    (let ((a (test-niwa-applied--file root "a.txt" "a1"))
          (saves 0) (thens 0)
          (save (symbol-function 'niwa-session-artifacts-save)))
      (test-niwa-applied--seed session 1 a "a0")
      (test-niwa-applied--file root "a.txt" "a2")
      (delete-file
       (plist-get
        (cdr (assoc (file-name-concat
                     "file-history"
                     (plist-get (test-niwa-applied--entry session 1 a)
                                :pre-backup-name))
                    (plist-get (niwa-session-publication session) :artifacts)))
        :published))
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest args) (cl-incf saves) (apply save args))))
        (should-error
         (niwa-session-artifacts-record-applied
          session 1 (list (test-niwa-applied--change a "a1"))
          :then (lambda (_) (cl-incf thens)))))
      (should (equal '(0 0) (list thens saves))))))

(provide 'test-niwa-session-artifacts-applied)
;;; test-niwa-session-artifacts-applied.el ends here
