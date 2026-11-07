;;; test-mevedel-persistence-instruction-store.el -- Workspace instruction store tests -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace instruction store through its callers' interface, on real
;; files and the real pinned control-file programs.  Another client is
;; played at the store boundary: it reads the record, changes it the way an
;; edit in another Emacs would, and writes it with the hash it read.  Every
;; claim has a control showing the discriminating condition matters.

;;; Code:

(require 'mevedel)
(require 'mevedel-persistence)
(require 'mevedel-directive)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))

(defmacro test-mevedel-instruction-store--with-workspace (vars &rest body)
  "Run BODY with VARS (WORKSPACE SOURCE) in a fresh workspace.
SOURCE visits `source.el' holding two functions.  The workspace has its
`.mevedel/' identity, as one that ever saved a session does."
  (declare (indent 1))
  (let ((tempdir (make-symbol "tempdir")))
    `(cl-destructuring-bind (,(nth 0 vars) . ,tempdir)
         (test-mevedel-session-persistence--make-tempdir-workspace)
       (let ((,(nth 1 vars) nil)
             (file (file-name-concat ,tempdir "source.el")))
         (unwind-protect
             (progn
               (test-mevedel-session-persistence--reset-instructions)
               (write-region "(defun beta () t)\n(defun gamma () t)\n"
                             nil file nil 'silent)
               (setq ,(nth 1 vars) (find-file-noselect file))
               (with-current-buffer ,(nth 1 vars)
                 (setq-local mevedel--workspace ,(nth 0 vars)))
               ,@body)
           (when (buffer-live-p ,(nth 1 vars))
             (with-current-buffer ,(nth 1 vars) (set-buffer-modified-p nil))
             (kill-buffer ,(nth 1 vars)))
           (setq mevedel-instruction-store--dirty nil)
           (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
           (delete-directory ,tempdir t)
           (test-mevedel-session-persistence--reset-instructions)
           (mevedel-workspace-clear-registry))))))

(defun test-mevedel-instruction-store--directive (source text &optional line)
  "Create directive TEXT on LINE (1 or 2) of SOURCE and return its overlay."
  (with-current-buffer source
    (goto-char (point-min))
    (forward-line (1- (or line 1)))
    (mevedel--create-directive-in source (point) (+ 5 (point)) nil text)))

(defun test-mevedel-instruction-store--restart (workspace)
  "Forget WORKSPACE's in-memory instructions, as a fresh Emacs would."
  (mevedel--clear-instruction-state workspace)
  (test-mevedel-session-persistence--reset-instructions))

(defun test-mevedel-instruction-store--record (workspace)
  "Return WORKSPACE's store record."
  (plist-get (mevedel-instruction-store--read workspace) :data))

(defun test-mevedel-instruction-store--stored-request (workspace uuid)
  "Return the request of directive UUID in WORKSPACE's store."
  (plist-get (cl-find uuid (plist-get (test-mevedel-instruction-store--record workspace)
                                      :directives)
                      :key (lambda (record) (plist-get record :id)) :test #'equal)
             :request))

(defvar test-mevedel-instruction-store--writer nil
  "The real store writer, captured before a test stubs it.")

(defun test-mevedel-instruction-store--other-client (workspace function)
  "Change WORKSPACE's store record with FUNCTION as another client would."
  (let* ((observation (mevedel-instruction-store--read workspace))
         (record (funcall function (copy-tree (plist-get observation :data)))))
    (should (funcall (or test-mevedel-instruction-store--writer
                         #'mevedel-instruction-store--write)
                     workspace (plist-get observation :hash) record))))

(defun test-mevedel-instruction-store--edit-record (record uuid key value)
  "Return store RECORD with KEY of directive UUID set to VALUE."
  (plist-put record :directives
             (mapcar (lambda (directive)
                       (if (equal uuid (plist-get directive :id))
                           (plist-put (copy-sequence directive) key value)
                         directive))
                     (plist-get record :directives))))

(defun test-mevedel-instruction-store--requests (workspace)
  "Return WORKSPACE's in-memory directive requests, sorted."
  (sort (mapcar #'mevedel-directive-request
                (mevedel-workspace-directives workspace))
        #'string<))

(defun test-mevedel-instruction-store--attempt (session-id turn sequence)
  "Return a successful attempt of SESSION-ID's TURN with SEQUENCE."
  (mevedel-directive-attempt--create
   :sequence sequence :action 'implement
   :directive-request "do it" :request "do it" :result "done"
   :outcome 'success :patch "" :capture 'incomplete :covered-files nil
   :gaps nil :checkpoint (list :session-id session-id :turn turn)
   :consumed-subdirectives nil :captured-at "2026-11-07T00:00:00+0100"))

(defun test-mevedel-instruction-store--attempt-turns (workspace uuid)
  "Return the checkpoint turns of directive UUID's attempts in WORKSPACE's store."
  (mapcar (lambda (attempt) (plist-get (plist-get attempt :checkpoint) :turn))
          (plist-get (cl-find uuid (plist-get (test-mevedel-instruction-store--record
                                               workspace)
                                              :directives)
                              :key (lambda (record) (plist-get record :id))
                              :test #'equal)
                     :attempts)))


;;; Writes, restart and concurrent clients

(mevedel-deftest mevedel-instruction-store-sync (:quiet t)
  ,test
  (test)
  :doc "a creation is written at once and survives a restart"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (test-mevedel-instruction-store--directive source "kept")
    (with-current-buffer source
      (goto-char (point-min))
      (forward-line 1)
      (mevedel--create-reference-in source (point) (line-end-position)))
    (test-mevedel-instruction-store--restart workspace)
    (should-not (mevedel-workspace-directives workspace))
    (mevedel-instruction-store-sync workspace)
    (should (equal '("kept") (test-mevedel-instruction-store--requests workspace)))
    (mevedel--instruction-activate-workspace workspace)
    (should (= 1 (cl-count-if #'mevedel--referencep
                              (alist-get source (mevedel--instruction-alist))))))
  :doc "control: without the store the same restart loses the creation"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (cl-letf (((symbol-function 'mevedel-instruction-store--applicable-p) #'ignore))
      (test-mevedel-instruction-store--directive source "kept")
      (should-not (file-exists-p (mevedel-instruction-store-path workspace)))
      (test-mevedel-instruction-store--restart workspace)
      (mevedel-instruction-store-sync workspace)
      (should-not (mevedel-workspace-directives workspace))))
  :doc "an edit to an instruction another client changed is refused, its text kept"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--directive source "base"))
           (uuid (overlay-get overlay 'mevedel-uuid))
           (kill-ring nil))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (test-mevedel-instruction-store--edit-record
                    record uuid :request "theirs")))
      (mevedel--set-directive-request overlay "mine")
      (should (eq 'refused (car (mevedel-instruction-store-sync workspace))))
      (should (equal "theirs" (test-mevedel-instruction-store--stored-request
                               workspace uuid)))
      (should (equal '("theirs") (test-mevedel-instruction-store--requests workspace)))
      (should (equal "mine" (car kill-ring)))))
  :doc "control: the same edit without a concurrent change is written"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--directive source "base"))
           (uuid (overlay-get overlay 'mevedel-uuid)))
      (mevedel--set-directive-request overlay "mine")
      (should (eq 'written (mevedel-instruction-store-sync workspace)))
      (should (equal "mine" (test-mevedel-instruction-store--stored-request
                             workspace uuid)))))
  :doc "changes to different instructions merge without losing either"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--directive source "one" 1))
           (second (test-mevedel-instruction-store--directive source "two" 2))
           (first-id (overlay-get first 'mevedel-uuid))
           (second-id (overlay-get second 'mevedel-uuid)))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (test-mevedel-instruction-store--edit-record
                    record first-id :request "one by them")))
      (mevedel--set-directive-request second "two by me")
      (should (eq 'merged (mevedel-instruction-store-sync workspace)))
      (should (equal "one by them" (test-mevedel-instruction-store--stored-request
                                    workspace first-id)))
      (should (equal "two by me" (test-mevedel-instruction-store--stored-request
                                  workspace second-id)))
      (should (equal '("one by them" "two by me")
                     (test-mevedel-instruction-store--requests workspace)))))
  :doc "a creation after a concurrent write is retried and renumbers a taken id"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--directive source "one" 1))
           (first-id (overlay-get first 'mevedel-uuid))
           (next-id (1+ (overlay-get first 'mevedel-id))))
      ;; The other client creates an instruction taking the next display id.
      (test-mevedel-instruction-store--other-client
       workspace
       (lambda (record)
         (let* ((entry (car (plist-get record :files)))
                (clone (copy-tree (car (plist-get (cdr entry) :instructions))))
                (properties (plist-get clone :properties)))
           (setq properties (plist-put properties 'mevedel-id next-id))
           (setq properties (plist-put properties 'mevedel-instruction-type 'reference))
           (setq properties (plist-put properties 'mevedel-uuid "their-reference"))
           (setq clone (plist-put clone :properties properties))
           (setq clone (plist-put clone :anchor
                                  (plist-put (copy-tree (plist-get clone :anchor))
                                             :uuid "their-reference")))
           (setcdr entry (plist-put (copy-sequence (cdr entry)) :instructions
                                    (append (plist-get (cdr entry) :instructions)
                                            (list clone))))
           (plist-put record :ids (list :id-counter next-id
                                        :used-ids (list (1- next-id) next-id)
                                        :retired-ids nil)))))
      (let ((mine (test-mevedel-instruction-store--directive source "two" 2)))
        (should (equal next-id (overlay-get mine 'mevedel-id))))
      (let* ((record (test-mevedel-instruction-store--record workspace))
             (ids (cl-loop for (_ . entry) in (plist-get record :files)
                           append (mapcar (lambda (instr)
                                            (plist-get (plist-get instr :properties)
                                                       'mevedel-id))
                                          (plist-get entry :instructions)))))
        (should (= 3 (length ids)))
        (should (equal ids (delete-dups (copy-sequence ids))))
        (should (equal "one" (test-mevedel-instruction-store--stored-request
                              workspace first-id))))))
  :doc "an activity append lands on a directive another client edited"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--directive source "do it"))
           (uuid (overlay-get overlay 'mevedel-uuid))
           (record (mevedel--directive-record overlay)))
      (setf (mevedel-directive-session-id record) "s1")
      (should (eq 'written (mevedel-instruction-store-sync workspace)))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (data)
                   (test-mevedel-instruction-store--edit-record
                    data uuid :request "edited by them")))
      (setf (mevedel-directive-attempts record)
            (list (test-mevedel-instruction-store--attempt "s1" 1 1)))
      (should (eq 'merged (mevedel-instruction-store-sync workspace)))
      (should (equal "edited by them" (test-mevedel-instruction-store--stored-request
                                       workspace uuid)))
      (should (equal '(1) (test-mevedel-instruction-store--attempt-turns workspace uuid)))))
  :doc "control: an append to a directive rebound by another client is refused"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--directive source "do it"))
           (uuid (overlay-get overlay 'mevedel-uuid))
           (record (mevedel--directive-record overlay)))
      (setf (mevedel-directive-session-id record) "s1")
      (mevedel-instruction-store-sync workspace)
      (test-mevedel-instruction-store--other-client
       workspace (lambda (data)
                   (test-mevedel-instruction-store--edit-record
                    data uuid :session-id "s2")))
      (setf (mevedel-directive-attempts record)
            (list (test-mevedel-instruction-store--attempt "s1" 1 1)))
      (should (eq 'refused (car (mevedel-instruction-store-sync workspace))))
      (should-not (test-mevedel-instruction-store--attempt-turns workspace uuid))))
  :doc "an identical activity entry already in the store counts as appended"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--directive source "do it"))
           (uuid (overlay-get overlay 'mevedel-uuid))
           (record (mevedel--directive-record overlay)))
      (setf (mevedel-directive-session-id record) "s1")
      (mevedel-instruction-store-sync workspace)
      (setf (mevedel-directive-attempts record)
            (list (test-mevedel-instruction-store--attempt "s1" 1 1)))
      (let ((with-attempt (cl-find uuid (plist-get (mevedel-instruction-store--serialize
                                                    workspace)
                                                   :directives)
                                   :key (lambda (r) (plist-get r :id)) :test #'equal)))
        (test-mevedel-instruction-store--other-client
         workspace (lambda (data)
                     (test-mevedel-instruction-store--edit-record
                      (test-mevedel-instruction-store--edit-record
                       data uuid :attempts (plist-get with-attempt :attempts))
                      uuid :request "edited by them"))))
      (should (eq 'merged (mevedel-instruction-store-sync workspace)))
      (should (equal '(1) (test-mevedel-instruction-store--attempt-turns workspace uuid)))))
  :doc "a write landing between this client's read and write is merged on retry"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--directive source "one" 1))
           (second (test-mevedel-instruction-store--directive source "two" 2))
           (first-id (overlay-get first 'mevedel-uuid))
           (second-id (overlay-get second 'mevedel-uuid))
           (write (symbol-function 'mevedel-instruction-store--write))
           (test-mevedel-instruction-store--writer write)
           (calls 0) first-result)
      (mevedel--set-directive-request second "two by me")
      (cl-letf (((symbol-function 'mevedel-instruction-store--write)
                 (lambda (&rest args)
                   (when (= 1 (cl-incf calls))
                     (test-mevedel-instruction-store--other-client
                      workspace (lambda (record)
                                  (test-mevedel-instruction-store--edit-record
                                   record first-id :request "one by them")))
                     (setq first-result (apply write args))
                     first-result)
                   (if (= calls 1) first-result (apply write args)))))
        (should (eq 'merged (mevedel-instruction-store-sync workspace))))
      ;; Control: the interleaving was reached and made the first write stale.
      (should (= 2 calls))
      (should-not first-result)
      (should (equal "one by them" (test-mevedel-instruction-store--stored-request
                                    workspace first-id)))
      (should (equal "two by me" (test-mevedel-instruction-store--stored-request
                                  workspace second-id)))))
  :doc "a write waits for another process holding the store lock"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (skip-unless (executable-find "flock"))
    (let* ((overlay (test-mevedel-instruction-store--directive source "one"))
           (lock (mevedel-instruction-store--lock-directory workspace))
           (ready (file-name-concat (mevedel-workspace-root workspace) "locked"))
           (holder (start-process "store-lock" nil "flock" lock "-c"
                                  (format "touch %s; sleep 1.5"
                                          (shell-quote-argument ready))))
           (deadline (+ (float-time) 5)))
      (unwind-protect
          (progn
            (while (and (not (file-exists-p ready)) (< (float-time) deadline))
              (accept-process-output holder 0.05))
            (should (file-exists-p ready))
            (mevedel--set-directive-request overlay "after the lock")
            (let ((start (float-time)))
              (should (eq 'written (mevedel-instruction-store-sync workspace)))
              (should (> (- (float-time) start) 0.8))))
        (when (process-live-p holder) (delete-process holder)))
      ;; Control: without a holder the same write does not wait.
      (mevedel--set-directive-request overlay "unlocked")
      (let ((start (float-time)))
        (should (eq 'written (mevedel-instruction-store-sync workspace)))
        (should (< (- (float-time) start) 0.8))))))


;;; Format version

(mevedel-deftest mevedel-instruction-store--read (:quiet t)
  ,test
  (test)
  :doc "absent, readable, unsupported and corrupt stores are told apart"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (should (eq 'absent (plist-get (mevedel-instruction-store--read workspace) :status)))
    (test-mevedel-instruction-store--directive source "one")
    (should (eq 'ok (plist-get (mevedel-instruction-store--read workspace) :status)))
    (dolist (case '(("(:format 99 :directives nil)" . unsupported)
                    ("(:format 1 :ids nil :directives ((:id 7)) :files nil)" . corrupt)
                    ("(:format 1 :ids nil" . corrupt)
                    ("(:format 1 :ids nil :directives nil :files nil) extra" . corrupt)))
      (with-temp-file (mevedel-instruction-store-path workspace)
        (insert (car case)))
      (should (eq (cdr case)
                  (plist-get (mevedel-instruction-store--read workspace) :status)))))
  :doc "an unreadable store refuses a write and keeps memory and bytes"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-mevedel-instruction-store--directive source "one")))
      (dolist (bytes '("(:format 99 :directives nil)\n" "(:format 1 :ids nil"))
        (with-temp-file (mevedel-instruction-store-path workspace)
          (insert bytes))
        (mevedel--set-directive-request overlay "changed")
        (should-error (mevedel-instruction-store-sync workspace) :type 'user-error)
        (should (equal bytes (with-temp-buffer
                               (insert-file-contents
                                (mevedel-instruction-store-path workspace))
                               (buffer-string))))
        (should (equal '("changed")
                       (test-mevedel-instruction-store--requests workspace))))))
  :doc "control: the same change against a readable store is written"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-mevedel-instruction-store--directive source "one")))
      (mevedel--set-directive-request overlay "changed")
      (should (eq 'written (mevedel-instruction-store-sync workspace))))))


;;; Pending workspaces

(mevedel-deftest mevedel-instruction-store-state (:quiet t)
  ,test
  (test)
  :doc "a workspace with sessions but no store is pending and creates none"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (cl-letf (((symbol-function 'mevedel-instruction-store--write)
               (lambda (&rest _) (error "No write expected"))))
      (make-directory (file-name-concat (mevedel-session-artifacts-sessions-dir workspace)
                                        "20261107T000000-aaaaaaaaaaaa")
                      t)
      (should (eq 'pending (mevedel-instruction-store-state workspace)))
      (test-mevedel-instruction-store--directive source "one")
      (should (eq 'pending (mevedel-instruction-store-sync workspace)))
      (should-not (file-exists-p (mevedel-instruction-store-path workspace)))))
  :doc "a staged migration's report keeps the workspace pending"
  (test-mevedel-instruction-store--with-workspace (workspace _source)
    (make-directory (mevedel-instruction-store-migration-directory workspace) t)
    (write-region "(:format 1)" nil
                  (file-name-concat (mevedel-instruction-store-migration-directory
                                     workspace)
                                    "report.el")
                  nil 'silent)
    (should (eq 'pending (mevedel-instruction-store-state workspace))))
  :doc "sessions kept in an absolute sessions directory also make it pending"
  (test-mevedel-instruction-store--with-workspace (workspace _source)
    (let ((mevedel-sessions-directory
           (file-name-concat (mevedel-workspace-root workspace) "elsewhere")))
      (make-directory (file-name-concat mevedel-sessions-directory "s") t)
      (should (eq 'pending (mevedel-instruction-store-state workspace)))))
  :doc "control: without sessions or a report the workspace is new"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (should (eq 'new (mevedel-instruction-store-state workspace)))
    (test-mevedel-instruction-store--directive source "one")
    (should (eq 'active (mevedel-instruction-store-state workspace))))
  :doc "without a .mevedel directory no store or directory is created"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-plain-" t)))
         (workspace (mevedel-workspace-get-or-create 'file "plain" root "plain")))
    (unwind-protect
        (progn
          (should (eq 'none (mevedel-instruction-store-sync workspace)))
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "a store created by another client replaces a pending one's state, exported"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (make-directory (file-name-concat (mevedel-session-artifacts-sessions-dir workspace)
                                      "s")
                    t)
    (test-mevedel-instruction-store--directive source "only here")
    (should (eq 'pending (mevedel-instruction-store-sync workspace)))
    (should (mevedel-instruction-store--write
             workspace nil (list :format mevedel-instruction-store-format-version
                                 :ids (list :id-counter 0 :used-ids nil :retired-ids nil)
                                 :directives nil :files nil :rewinds nil)))
    (should (eq 'replaced (mevedel-instruction-store-sync workspace)))
    (should-not (mevedel-workspace-directives workspace))
    (let ((exports (directory-files (mevedel-instruction-store-migration-directory
                                     workspace)
                                    t "\\`unsynced-")))
      (should (= 1 (length exports)))
      (should (string-match-p "only here" (with-temp-buffer
                                            (insert-file-contents (car exports))
                                            (buffer-string)))))))


;;; Export and import

(mevedel-deftest mevedel-load-instructions (:quiet t)
  ,test
  (test)
  :doc "an export loads back and replaces the store"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((export (file-name-concat (mevedel-workspace-root workspace) "export.el")))
      (test-mevedel-instruction-store--directive source "exported")
      (with-current-buffer source (mevedel-save-instructions export))
      (with-current-buffer source
        (mevedel--delete-instruction
         (car (mevedel--instructions-in (point-min) (point-max)))))
      (mevedel-instruction-store-sync workspace)
      (should-not (plist-get (test-mevedel-instruction-store--record workspace) :directives))
      (with-current-buffer source (mevedel-load-instructions export))
      (should (equal '("exported") (test-mevedel-instruction-store--requests workspace)))
      (should (= 1 (length (plist-get (test-mevedel-instruction-store--record workspace)
                                      :directives))))))
  :doc "loading resolves a pending workspace by creating its store"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((export (file-name-concat (mevedel-workspace-root workspace) "candidate.el")))
      (test-mevedel-instruction-store--directive source "candidate")
      (with-current-buffer source (mevedel-save-instructions export))
      (delete-file (mevedel-instruction-store-path workspace))
      (test-mevedel-instruction-store--restart workspace)
      (make-directory (file-name-concat (mevedel-session-artifacts-sessions-dir workspace)
                                        "s")
                      t)
      (should (eq 'pending (mevedel-instruction-store-state workspace)))
      (with-current-buffer source (mevedel-load-instructions export))
      (should (eq 'active (mevedel-instruction-store-state workspace)))
      (should (equal "candidate"
                     (plist-get (car (plist-get (test-mevedel-instruction-store--record
                                                 workspace)
                                                :directives))
                                :request)))))
  :doc "loading refuses to replace a store changed since it was observed"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((export (file-name-concat (mevedel-workspace-root workspace) "export.el"))
           (overlay (test-mevedel-instruction-store--directive source "mine"))
           (uuid (overlay-get overlay 'mevedel-uuid)))
      (with-current-buffer source (mevedel-save-instructions export))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (test-mevedel-instruction-store--edit-record
                    record uuid :request "theirs")))
      (with-current-buffer source
        (should-error (mevedel-load-instructions export) :type 'user-error))
      (should (equal "theirs" (test-mevedel-instruction-store--stored-request
                               workspace uuid))))))


;;; Rewind intents

(defmacro test-mevedel-instruction-store--with-rewind (vars &rest body)
  "Run BODY with VARS (WORKSPACE SESSION UUID) around a directive with activity.
SESSION has two committed turns; the directive holds one attempt for
each, both written to the store."
  (declare (indent 1))
  `(test-mevedel-instruction-store--with-workspace (,(nth 0 vars) source)
     (let* ((,(nth 1 vars) (mevedel-session-create "s" ,(nth 0 vars)))
            (overlay (test-mevedel-instruction-store--directive source "do it"))
            (,(nth 2 vars) (overlay-get overlay 'mevedel-uuid))
            (record (mevedel--directive-record overlay))
            (sid (mevedel-session-session-id ,(nth 1 vars))))
       (setf (mevedel-directive-session-id record) sid
             (mevedel-directive-attempts record)
             (list (test-mevedel-instruction-store--attempt sid 1 1)
                   (test-mevedel-instruction-store--attempt sid 2 2))
             (mevedel-session-turn-count ,(nth 1 vars)) 2)
       (should (eq 'written (mevedel-instruction-store-sync ,(nth 0 vars))))
       ,@body)))

(mevedel-deftest mevedel-instruction-store-rewind-end (:quiet t)
  ,test
  (test)
  :doc "a committed Rewind prunes the store and keeps the live directive record"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (let ((record (car (mevedel-workspace-directives workspace))))
      (should (mevedel-instruction-store-rewind-begin session 1 2))
      (should (plist-get (test-mevedel-instruction-store--record workspace) :rewinds))
      (mevedel-workspace-rewind-directives
       workspace (mevedel-session-session-id session) 2)
      (setf (mevedel-session-turn-count session) 1)
      (mevedel-instruction-store-rewind-end session t)
      (should (equal '(1) (test-mevedel-instruction-store--attempt-turns workspace uuid)))
      (should-not (plist-get (test-mevedel-instruction-store--record workspace) :rewinds))
      (should (eq record (car (mevedel-workspace-directives workspace))))))
  :doc "a rolled-back Rewind drops its intent and keeps the activity"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (mevedel-instruction-store-rewind-end session nil)
    (should (equal '(1 2) (test-mevedel-instruction-store--attempt-turns workspace uuid)))
    (should-not (plist-get (test-mevedel-instruction-store--record workspace) :rewinds)))
  :doc "another client's write during the Rewind survives its prune"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (test-mevedel-instruction-store--other-client
     workspace (lambda (data)
                 (test-mevedel-instruction-store--edit-record
                  data uuid :request "edited during Rewind")))
    (setf (mevedel-session-turn-count session) 1)
    (mevedel-instruction-store-rewind-end session t)
    (should (equal '(1) (test-mevedel-instruction-store--attempt-turns workspace uuid)))
    (should (equal "edited during Rewind"
                   (test-mevedel-instruction-store--stored-request workspace uuid))))
  :doc "a failed post-commit prune is owed and resolved before the next request"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (setf (mevedel-session-turn-count session) 1)
    (cl-letf (((symbol-function 'mevedel-instruction-store--write) #'ignore))
      (should-error (mevedel-instruction-store-rewind-end session t)))
    (should (equal '(1 2) (test-mevedel-instruction-store--attempt-turns workspace uuid)))
    (should (mevedel-instruction-store-owes-p session))
    (should (mevedel-instruction-store-settle-session session))
    (should (equal '(1) (test-mevedel-instruction-store--attempt-turns workspace uuid)))))

(mevedel-deftest mevedel-instruction-store-settle-session (:quiet t)
  ,test
  (test)
  :doc "a crash before the commit point drops the intent at resume"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (test-mevedel-instruction-store--restart workspace)
    (should (mevedel-instruction-store-settle-session session))
    (should (equal '(1 2) (test-mevedel-instruction-store--attempt-turns workspace uuid)))
    (should-not (plist-get (test-mevedel-instruction-store--record workspace) :rewinds)))
  :doc "a crash after the commit point prunes at resume"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (setf (mevedel-session-turn-count session) 1)
    (test-mevedel-instruction-store--restart workspace)
    (should (mevedel-instruction-store-settle-session session))
    (should (equal '(1) (test-mevedel-instruction-store--attempt-turns workspace uuid))))
  :doc "another session's Rewind prunes only its own activity"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (let ((other (mevedel-session-create "t" workspace))
          (record (car (mevedel-workspace-directives workspace))))
      (setf (mevedel-directive-attempts record)
            (append (mevedel-directive-attempts record)
                    (list (test-mevedel-instruction-store--attempt
                           (mevedel-session-session-id other) 1 3)))
            (mevedel-session-turn-count other) 1)
      (mevedel-instruction-store-sync workspace)
      (should (mevedel-instruction-store-rewind-begin session 1 2))
      (should (mevedel-instruction-store-rewind-begin other 0 1))
      (setf (mevedel-session-turn-count other) 0)
      (should (mevedel-instruction-store-settle-session other))
      (should (equal '(1 2) (test-mevedel-instruction-store--attempt-turns workspace uuid)))
      (should (= 1 (length (plist-get (test-mevedel-instruction-store--record workspace)
                                      :rewinds))))
      (setf (mevedel-session-turn-count session) 1)
      (should (mevedel-instruction-store-settle-session session))
      (should (equal '(1) (test-mevedel-instruction-store--attempt-turns workspace uuid)))))
  :doc "an intent matching neither turn count is refused"
  (test-mevedel-instruction-store--with-rewind (workspace session _uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (setf (mevedel-session-turn-count session) 5)
    (should-error (mevedel-instruction-store-settle-session session) :type 'user-error)
    (should (plist-get (test-mevedel-instruction-store--record workspace) :rewinds)))
  :doc "control: activity above the committed turn without an intent is kept"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    ;; A lost turn's attempt carries a turn the session never committed.
    (setf (mevedel-session-turn-count session) 1)
    (should-not (mevedel-instruction-store-settle-session session))
    (should (equal '(1 2) (test-mevedel-instruction-store--attempt-turns workspace uuid)))))

(provide 'test-mevedel-persistence-instruction-store)
;;; test-mevedel-persistence-instruction-store.el ends here
