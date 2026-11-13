;;; test-mevedel-session-artifacts-applied.el --- Applied prepared changes -*- lexical-binding: t -*-

;;; Commentary:

;; Recording an applied prepared change under its originating turn, in real
;; temporary file and portable sessions with real source and backup files.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-session-artifacts)
(require 'mevedel-session-publication)
(require 'mevedel-session-rewind)

(defvar test-mevedel-applied--root nil "Root of the running fixture.")

(defmacro test-mevedel-applied--with-session (portable &rest body)
  "Run BODY with `session', `buffer' and `root' for a materialized turn 1.
PORTABLE selects a portable session over a file one."
  (declare (indent 1))
  `(let* ((root (make-temp-file "mevedel-applied-" t))
          (workspace (if ,portable
                         (test-mevedel-session-persistence--make-workspace root)
                       (test-mevedel-session-persistence--make-file-workspace root)))
          (session (mevedel-session-create "main" workspace))
          (buffer (generate-new-buffer " *applied*")))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (org-mode)
             (setq-local mevedel--session session)
             (insert "Request\n" (propertize "Response\n" 'gptel 'response)))
           (setf (mevedel-session-turn-count session) 1)
           (mevedel-session-artifacts-save session buffer t)
           ,@body)
       (test-mevedel-session-persistence--release-and-kill buffer session)
       (delete-directory root t)
       (mevedel-workspace-clear-registry))))

(defun test-mevedel-applied--file (root name content)
  "Write CONTENT to NAME under ROOT and return its path."
  (let ((path (file-name-concat root name)))
    (write-region content nil path nil 'silent)
    path))

(defun test-mevedel-applied--change (path before)
  "Return a prepared change of PATH whose source held BEFORE bytes."
  (list :path path :before (list :path path :exists t :bytes before)))

(defun test-mevedel-applied--seed (session turn &rest pairs)
  "Capture current bytes under SESSION's TURN for PAIRS of path and pre-content."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) table))
    (mevedel-session-artifacts-snapshot-modified session turn table)))

(defun test-mevedel-applied--entry (session turn path)
  "Return SESSION's TURN checkpoint plist for PATH."
  (cdr (assoc path (cdr (assq turn (mevedel-session-file-snapshots session))))))

(mevedel-deftest mevedel-session-artifacts-record-applied
  (:quiet t)
  ,test
  (test)
  :doc "entries for untouched paths survive the turn re-snapshot"
  (dolist (portable '(nil t))
    (test-mevedel-applied--with-session portable
      (let ((a (test-mevedel-applied--file root "a.txt" "a1"))
            (b (test-mevedel-applied--file root "b.txt" "b1")))
        (test-mevedel-applied--seed session 1 b "b0")
        (test-mevedel-applied--file root "a.txt" "a2")
        (mevedel-session-artifacts-record-applied
         session 1 (list (test-mevedel-applied--change a "a1")))
        (should (test-mevedel-applied--entry session 1 a))
        (should (equal (list a b)
                       (mapcar #'car (cdr (assq 1 (mevedel-session-file-snapshots session))))))
        (should (plist-get (test-mevedel-applied--entry session 1 b) :pre-backup-name)))))
  :doc "an existing gap is kept and later edits give an edited-after gap"
  (test-mevedel-applied--with-session nil
    (let ((a (test-mevedel-applied--file root "a.txt" "a1"))
          (b (test-mevedel-applied--file root "b.txt" "b1")))
      (test-mevedel-applied--seed session 1 a '(:gap "lost") b "b0")
      (test-mevedel-applied--file root "a.txt" "a2")
      (test-mevedel-applied--file root "b.txt" "b2")
      (mevedel-session-artifacts-record-applied
       session 1 (list (test-mevedel-applied--change a "a1")
                       (test-mevedel-applied--change b "elsewhere")))
      (should (equal "lost" (plist-get (test-mevedel-applied--entry session 1 a) :gap)))
      (should (equal "edited after turn 1"
                     (plist-get (test-mevedel-applied--entry session 1 b) :gap)))))
  :doc "matching bytes keep the previous pre-image; a new path uses the change's bytes"
  (test-mevedel-applied--with-session nil
    (let ((a (test-mevedel-applied--file root "a.txt" "a1"))
          (c (test-mevedel-applied--file root "c.txt" "c1")))
      (test-mevedel-applied--seed session 1 a "a0")
      (test-mevedel-applied--file root "a.txt" "a2")
      (test-mevedel-applied--file root "c.txt" "c2")
      (mevedel-session-artifacts-record-applied
       session 1 (list (test-mevedel-applied--change a "a1")
                       (test-mevedel-applied--change c "c1")))
      (should (equal "a0" (mevedel-session-rewind-read-backup
                           session (plist-get (test-mevedel-applied--entry session 1 a)
                                              :pre-backup-name))))
      (should (equal "c1" (mevedel-session-rewind-read-backup
                           session (plist-get (test-mevedel-applied--entry session 1 c)
                                              :pre-backup-name))))))
  :doc "backups are readable from file and portable sessions"
  (dolist (portable '(nil t))
    (test-mevedel-applied--with-session portable
      (let ((a (test-mevedel-applied--file root "a.txt" "a1")))
        (test-mevedel-applied--file root "a.txt" "a2")
        (mevedel-session-artifacts-record-applied
         session 1 (list (test-mevedel-applied--change a "a1")))
        (let ((entry (test-mevedel-applied--entry session 1 a)))
          (should (equal "a2" (mevedel-session-rewind-read-backup
                               session (plist-get entry :backup-name))))
          (should (equal "a1" (mevedel-session-rewind-read-backup
                               session (plist-get entry :pre-backup-name))))
          (unless portable
            (should (file-exists-p
                     (file-name-concat (mevedel-session-save-path session)
                                       "file-history"
                                       (plist-get entry :backup-name)))))))))
  :doc "application backups only on request, from the change's own bytes"
  (test-mevedel-applied--with-session nil
    (let ((a (test-mevedel-applied--file root "a.txt" "a1"))
          (b (test-mevedel-applied--file root "b.txt" "b1"))
          seen)
      (test-mevedel-applied--seed session 1 a "a0")
      (test-mevedel-applied--file root "a.txt" "a2")
      (test-mevedel-applied--file root "b.txt" "b2")
      (mevedel-session-artifacts-record-applied
       session 1 (list (test-mevedel-applied--change a "a1"))
       :then (lambda (files) (push (list 'without files) seen)))
      (mevedel-session-artifacts-record-applied
       session 1 (list (test-mevedel-applied--change b "b1"))
       :application-backups t
       :then (lambda (files) (push (list 'with files) seen)))
      (should (null (cadr (assq 'without seen))))
      (let ((entry (cdr (car (cadr (assq 'with seen))))))
        (should (equal "b1" (mevedel-session-rewind-read-backup
                             session (plist-get entry :pre-backup-name))))
        (should-not (plist-member entry :version))
        (should-not (plist-member entry :backup-time)))))
  :doc "request snapshots receive pre-content only for the live turn"
  (test-mevedel-applied--with-session nil
    (let ((a (test-mevedel-applied--file root "a.txt" "a1")))
      (test-mevedel-applied--file root "a.txt" "a2")
      (dolist (live '(1 2))
        (let ((request (mevedel-request--create
                        :turn live :file-snapshots (make-hash-table :test #'equal))))
          (with-current-buffer buffer (setq-local mevedel--current-request request))
          (mevedel-session-artifacts-record-applied
           session 1 (list (test-mevedel-applied--change a "a1")))
          (should (equal (= live 1)
                         (and (gethash a (mevedel-request-file-snapshots request)) t)))))))
  :doc "the live turn is not re-snapshotted over the merged cell"
  (test-mevedel-applied--with-session nil
    (let ((a (test-mevedel-applied--file root "a.txt" "a1"))
          (b (test-mevedel-applied--file root "b.txt" "b1"))
          (request (mevedel-request--create
                    :turn 1 :file-snapshots (make-hash-table :test #'equal))))
      (test-mevedel-applied--seed session 1 b "b0")
      (test-mevedel-applied--file root "a.txt" "a2")
      (with-current-buffer buffer (setq-local mevedel--current-request request))
      (mevedel-session-artifacts-record-applied
       session 1 (list (test-mevedel-applied--change a "a1")))
      (should (test-mevedel-applied--entry session 1 a))
      (should (test-mevedel-applied--entry session 1 b))))
  :doc "THEN runs once on success; failures stop it and the save"
  (test-mevedel-applied--with-session t
    (let ((a (test-mevedel-applied--file root "a.txt" "a1"))
          (saves 0) (thens 0)
          (save (symbol-function 'mevedel-session-artifacts-save)))
      (test-mevedel-applied--file root "a.txt" "a2")
      (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                 (lambda (&rest args) (cl-incf saves) (apply save args))))
        (mevedel-session-artifacts-record-applied
         session 1 (list (test-mevedel-applied--change a "a1"))
         :then (lambda (_) (cl-incf thens)))
        (should (equal '(1 1) (list thens saves)))
        (test-mevedel-applied--file root "a.txt" "a3")
        (cl-letf (((symbol-function 'mevedel-session-publication-publish)
                   (lambda (&rest _) (error "Publication failed"))))
          (should-error
           (mevedel-session-artifacts-record-applied
            session 1 (list (test-mevedel-applied--change a "a2"))
            :then (lambda (_) (cl-incf thens)))))
        (should (equal '(1 1) (list thens saves)))
        (should-error
         (mevedel-session-artifacts-record-applied
          session 1 (list (test-mevedel-applied--change a "a3"))
          :then (lambda (_) (cl-incf thens) (error "Then failed"))))
        (should (equal '(2 1) (list thens saves)))))))

(provide 'test-mevedel-session-artifacts-applied)
;;; test-mevedel-session-artifacts-applied.el ends here
