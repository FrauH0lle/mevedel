;;; test-niwa-persistence-instruction-store.el -- Workspace instruction store tests -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace instruction store through its callers' interface, on real
;; files and the real pinned control-file programs.  Another client is
;; played at the store boundary: it reads the record, changes it the way an
;; edit in another Emacs would, and writes it with the hash it read.  Every
;; claim has a control showing the discriminating condition matters.

;;; Code:

(require 'niwa-notes)

(require 'niwa)
(require 'niwa-persistence)
(require 'niwa-session-control-fs)
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

(defmacro test-niwa-instruction-store--with-workspace (vars &rest body)
  "Run BODY with VARS (WORKSPACE SOURCE) in a fresh workspace.
SOURCE visits `source.el' holding two functions.  The workspace has its
`.niwa/' identity, as one that ever saved a session does."
  (declare (indent 1))
  (let ((tempdir (make-symbol "tempdir")))
    `(cl-destructuring-bind (,(nth 0 vars) . ,tempdir)
         (test-niwa-session-persistence--make-tempdir-workspace)
       ;; Pending writes other tests left behind are not this case's: a
       ;; flush here sees only its own workspace.
       (let ((,(nth 1 vars) nil)
             (niwa-instruction-store--dirty nil)
             (niwa-instruction-store--failed nil)
             (file (file-name-concat ,tempdir "source.el")))
         (unwind-protect
             (progn
               (test-niwa-session-persistence--reset-instructions)
               (write-region "(defun beta () t)\n(defun gamma () t)\n"
                             nil file nil 'silent)
               (setq ,(nth 1 vars) (find-file-noselect file))
               (with-current-buffer ,(nth 1 vars)
                 (setq-local niwa--workspace ,(nth 0 vars)))
               ,@body)
           (when (buffer-live-p ,(nth 1 vars))
             (with-current-buffer ,(nth 1 vars) (set-buffer-modified-p nil))
             (kill-buffer ,(nth 1 vars)))
           (setq niwa-instruction-store--dirty nil)
           (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
           (delete-directory ,tempdir t)
           (test-niwa-session-persistence--reset-instructions)
           (niwa-workspace-clear-registry))))))

(defun test-niwa-instruction-store--note (source text &optional line)
  "Create note TEXT on LINE (1 or 2) of SOURCE and return its overlay."
  (with-current-buffer source
    (goto-char (point-min))
    (forward-line (1- (or line 1)))
    (niwa-note-create text)))

(defun test-niwa-instruction-store--save-session (workspace name)
  "Save a session NAME of WORKSPACE with the real session writer.
Creating WORKSPACE's store is not under test, so its sync is stubbed."
  (let ((session (niwa-session-create name workspace))
        (buffer (generate-new-buffer " *stored-session*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "Hello\n")
          (cl-letf (((symbol-function 'niwa-instruction-store-sync) #'ignore))
            (niwa-session-artifacts-save session buffer)))
      (kill-buffer buffer))))

(defun test-niwa-instruction-store--restart (workspace)
  "Forget WORKSPACE's in-memory instructions, as a fresh Emacs would."
  (niwa--clear-instruction-state workspace)
  (test-niwa-session-persistence--reset-instructions))

(defun test-niwa-instruction-store--reactivate (workspace)
  "Restart, then resolve WORKSPACE's struct anew as a fresh Emacs would.
Return the new workspace struct."
  (test-niwa-instruction-store--restart workspace)
  (niwa-workspace-clear-registry)
  (niwa-workspace-get-or-create
   (niwa-workspace-type workspace) (niwa-workspace-id workspace)
   (niwa-workspace-root workspace) (niwa-workspace-name workspace)))

(defun test-niwa-instruction-store--listed (workspace)
  "Observe WORKSPACE's store and return its note texts."
  (niwa-instruction-store-observe workspace)
  (test-niwa-instruction-store--texts workspace))

(defun test-niwa-instruction-store--record (workspace)
  "Return WORKSPACE's store record."
  (plist-get (niwa-instruction-store--read workspace) :data))

(defun test-niwa-instruction-store--stored (workspace uuid key)
  "Return property KEY of note UUID in WORKSPACE's store."
  (catch 'found
    (dolist (entry (plist-get (test-niwa-instruction-store--record workspace) :files))
      (dolist (instruction (plist-get (cdr entry) :instructions))
        (let ((properties (plist-get instruction :properties)))
          (when (equal uuid (plist-get properties 'niwa-uuid))
            (throw 'found (plist-get properties key))))))))

(defun test-niwa-instruction-store--stored-text (workspace uuid)
  "Return the text of note UUID in WORKSPACE's store."
  (test-niwa-instruction-store--stored workspace uuid 'niwa-note-text))

(defvar test-niwa-instruction-store--writer nil
  "The real store writer, captured before a test stubs it.")

(defun test-niwa-instruction-store--other-client (workspace function)
  "Change WORKSPACE's store record with FUNCTION as another client would."
  (let* ((observation (niwa-instruction-store--read workspace))
         (record (funcall function (copy-tree (plist-get observation :data)))))
    (should (funcall (or test-niwa-instruction-store--writer
                         #'niwa-instruction-store--write)
                     workspace (plist-get observation :hash) record))))

(defun test-niwa-instruction-store--edit-record (record uuid key value)
  "Return store RECORD with property KEY of note UUID set to VALUE."
  (dolist (entry (plist-get record :files) record)
    (dolist (instruction (plist-get (cdr entry) :instructions))
      (let ((properties (plist-get instruction :properties)))
        (when (equal uuid (plist-get properties 'niwa-uuid))
          (plist-put instruction :properties (plist-put properties key value)))))))

(defun test-niwa-instruction-store--texts (workspace)
  "Return WORKSPACE's in-memory note texts, sorted."
  (sort (mapcar (lambda (note) (plist-get (plist-get note :properties) 'niwa-note-text))
                (niwa-notes-open workspace))
        #'string<))

(defun test-niwa-instruction-store--prepared-turn (workspace uuid)
  "Return the turn of note UUID's prepared change in WORKSPACE's store."
  (plist-get (test-niwa-instruction-store--stored
              workspace uuid 'niwa-note-prepared-change)
             :turn))


;;; Writes, restart and concurrent clients

(niwa-deftest niwa-instruction-store-sync (:quiet t)
  ,test
  (test)
  :doc "a creation is written at once and is back when the workspace activates"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (test-niwa-instruction-store--note source "kept")
    (let ((fresh (test-niwa-instruction-store--reactivate workspace)))
      (should (equal '("kept") (test-niwa-instruction-store--texts fresh)))
      (should (equal '("kept") (test-niwa-instruction-store--listed fresh)))
      ;; The visited source got its note back; other files wait for a visit.
      (niwa--instruction-activate-workspace fresh)
      (should (= 1 (cl-count-if (lambda (instruction)
                               (eq 'note (overlay-get instruction 'niwa-instruction-type)))
                                (alist-get source (niwa--instruction-alist)))))))
  :doc "control: without activation the restarted workspace lists nothing"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (test-niwa-instruction-store--note source "kept")
    (cl-letf (((symbol-function 'niwa-instruction-store-activate) #'ignore)
              ((symbol-function 'niwa-instruction-store-observe) #'ignore))
      (let ((fresh (test-niwa-instruction-store--reactivate workspace)))
        (should-not (test-niwa-instruction-store--texts fresh)))))
  :doc "observing the store reads another client's change"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-niwa-instruction-store--note source "mine"))
           (uuid (overlay-get overlay 'niwa-uuid)))
      (should (equal '("mine") (test-niwa-instruction-store--listed workspace)))
      (test-niwa-instruction-store--other-client
       workspace (lambda (record)
                   (test-niwa-instruction-store--edit-record
                    record uuid 'niwa-note-text "changed elsewhere")))
      (should (equal '("changed elsewhere")
                     (test-niwa-instruction-store--listed workspace)))
      ;; Control: an unchanged store costs one hash and changes nothing.
      (should (eq 'current (niwa-instruction-store-observe workspace)))))
  :doc "unchanged sync hashes but does not read, while a local edit still reads and writes"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((note (test-niwa-instruction-store--note source "base"))
           (read-store (symbol-function 'niwa-instruction-store--read))
           (reads 0))
      (niwa-instruction-store-sync workspace)
      (cl-letf (((symbol-function 'niwa-instruction-store--read)
                 (lambda (&rest args) (cl-incf reads) (apply read-store args))))
        (should (eq 'unchanged (niwa-instruction-store-sync workspace)))
        (should (= 0 reads))
        (niwa-note-edit "edited" note)
        (should (eq 'written (niwa-instruction-store-sync workspace)))
        (should (= 1 reads)))
      (should (equal '("edited") (test-niwa-instruction-store--listed workspace)))))
  :doc "uncertain sync evidence falls back to a full read without losing notes"
  (dolist (scenario '(hash-error absent observation changed corrupt))
    (test-niwa-instruction-store--with-workspace (workspace source)
      (let* ((note (test-niwa-instruction-store--note source "base"))
             (uuid (overlay-get note 'niwa-uuid))
             (read-store (symbol-function 'niwa-instruction-store--read))
             (hash-store (symbol-function 'niwa-instruction-store--current-hash))
             (reads 0))
        (niwa-instruction-store-sync workspace)
        (pcase scenario
          ('absent (delete-file (niwa-instruction-store-path workspace)))
          ('observation
           (setf (plist-get (niwa--instruction-state
                            (niwa--instruction-workspace-key workspace)) :store-observation) nil))
          ('changed
           (test-niwa-instruction-store--other-client
            workspace (lambda (record)
                        (test-niwa-instruction-store--edit-record
                         record uuid 'niwa-note-text "theirs"))))
          ('corrupt (write-region "broken" nil (niwa-instruction-store-path workspace) nil 'silent)))
        (cl-letf (((symbol-function 'niwa-instruction-store--read)
                   (lambda (&rest args) (cl-incf reads) (apply read-store args)))
                  ((symbol-function 'niwa-instruction-store--current-hash)
                   (lambda (&rest args)
                     (if (eq scenario 'hash-error) (error "Hash unavailable")
                       (apply hash-store args)))))
          (if (eq scenario 'corrupt)
              (should-error (niwa-instruction-store-sync workspace))
            (should (memq (niwa-instruction-store-sync workspace) '(unchanged written merged))))
          (should (= 1 reads)))
        (should (equal (if (eq scenario 'changed) '("theirs") '("base"))
                       (test-niwa-instruction-store--texts workspace))))))
  :doc "a note edited in the minibuffer is written once, after it returns"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-niwa-instruction-store--note source "base"))
           (uuid (overlay-get overlay 'niwa-uuid))
           (syncs 0)
           (sync (symbol-function 'niwa-instruction-store-sync)))
      (cl-letf (((symbol-function 'niwa-instruction-store-sync)
                 (lambda (&rest args) (cl-incf syncs) (apply sync args))))
        (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
          (dolist (text '("b" "ba" "bas"))
            (niwa-note-edit text overlay)
            (niwa-instruction-store-flush)))
        (should (= 0 syncs))
        (should (memq workspace niwa-instruction-store--dirty))
        ;; Control: once the minibuffer returns, one flush writes it.
        (niwa-instruction-store-flush)
        (should (= 1 syncs)))
      (should (equal "bas" (test-niwa-instruction-store--stored-text
                            workspace uuid)))))
  :doc "a failing write is retried once per checkpoint interval, not per command"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let ((attempts 0)
          (niwa-session-checkpoint-interval 30))
      (cl-letf (((symbol-function 'niwa-instruction-store-sync)
                 (lambda (&rest _) (cl-incf attempts) (error "Disk full")))
                ((symbol-function 'message) #'ignore))
        (test-niwa-instruction-store--note source "base")
        (should (memq workspace niwa-instruction-store--dirty))
        (setq attempts 0)
        (dotimes (_ 10) (niwa-instruction-store-flush))
        (should (= 1 attempts))
        (should (memq workspace niwa-instruction-store--dirty))
        (should (memq #'niwa-instruction-store-flush post-command-hook))
        ;; Controls: a forced flush (exit) tries at once; so does a flush
        ;; once the interval has passed.
        (niwa-instruction-store-flush t)
        (should (= 2 attempts))
        (setf (alist-get workspace niwa-instruction-store--failed)
              (time-subtract nil 31))
        (niwa-instruction-store-flush)
        (should (= 3 attempts)))))
  :doc "control: without the store a restart loses the creation"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (cl-letf (((symbol-function 'niwa-instruction-store--applicable-p) #'ignore))
      (test-niwa-instruction-store--note source "kept")
      (should-not (file-exists-p (niwa-instruction-store-path workspace)))
      (test-niwa-instruction-store--restart workspace)
      (niwa-instruction-store-sync workspace)
      (should-not (test-niwa-instruction-store--texts workspace))))
  :doc "an edit to an instruction another client changed is refused, its text kept"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-niwa-instruction-store--note source "base"))
           (uuid (overlay-get overlay 'niwa-uuid))
           (kill-ring nil))
      (test-niwa-instruction-store--other-client
       workspace (lambda (record)
                   (test-niwa-instruction-store--edit-record
                    record uuid 'niwa-note-text "theirs")))
      (niwa-note-edit "mine" overlay)
      (should (eq 'refused (car (niwa-instruction-store-sync workspace))))
      (should (equal "theirs" (test-niwa-instruction-store--stored-text
                               workspace uuid)))
      (should (equal '("theirs") (test-niwa-instruction-store--texts workspace)))
      (should (equal "mine" (car kill-ring)))))
  :doc "control: the same edit without a concurrent change is written"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-niwa-instruction-store--note source "base"))
           (uuid (overlay-get overlay 'niwa-uuid)))
      (niwa-note-edit "mine" overlay)
      (should (eq 'written (niwa-instruction-store-sync workspace)))
      (should (equal "mine" (test-niwa-instruction-store--stored-text
                             workspace uuid)))))
  :doc "changes to different instructions merge without losing either"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((first (test-niwa-instruction-store--note source "one" 1))
           (second (test-niwa-instruction-store--note source "two" 2))
           (first-id (overlay-get first 'niwa-uuid))
           (second-id (overlay-get second 'niwa-uuid)))
      (test-niwa-instruction-store--other-client
       workspace (lambda (record)
                   (test-niwa-instruction-store--edit-record
                    record first-id 'niwa-note-text "one by them")))
      (niwa-note-edit "two by me" second)
      (should (eq 'merged (niwa-instruction-store-sync workspace)))
      (should (equal "one by them" (test-niwa-instruction-store--stored-text
                                    workspace first-id)))
      (should (equal "two by me" (test-niwa-instruction-store--stored-text
                                  workspace second-id)))
      (should (equal '("one by them" "two by me")
                     (test-niwa-instruction-store--texts workspace)))))
  :doc "a merged write drops a note another client deleted"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((first (test-niwa-instruction-store--note source "one" 1))
           (second (test-niwa-instruction-store--note source "two" 2))
           (uuid (overlay-get first 'niwa-uuid)))
      (test-niwa-instruction-store--other-client
       workspace (lambda (record)
                   (dolist (entry (plist-get record :files) record)
                     (setcdr entry
                             (plist-put (cdr entry) :instructions
                                        (cl-remove uuid (plist-get (cdr entry) :instructions)
                                                   :key (lambda (instruction)
                                                          (plist-get (plist-get instruction
                                                                                :properties)
                                                                     'niwa-uuid))
                                                   :test #'equal))))))
      (niwa-note-edit "two by me" second)
      (should (eq 'merged (niwa-instruction-store-sync workspace)))
      (should (equal '("two by me") (test-niwa-instruction-store--texts workspace)))))
  :doc "a creation after a concurrent write is retried and renumbers a taken id"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((first (test-niwa-instruction-store--note source "one" 1))
           (first-id (overlay-get first 'niwa-uuid))
           (next-id (1+ (overlay-get first 'niwa-id))))
      ;; The other client creates an instruction taking the next display id.
      (test-niwa-instruction-store--other-client
       workspace
       (lambda (record)
         (let* ((entry (car (plist-get record :files)))
                (clone (copy-tree (car (plist-get (cdr entry) :instructions))))
                (properties (plist-get clone :properties)))
           (setq properties (plist-put properties 'niwa-id next-id))
           (setq properties (plist-put properties 'niwa-instruction-type 'note))
           (setq properties (plist-put properties 'niwa-uuid "their-reference"))
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
      (let ((mine (test-niwa-instruction-store--note source "two" 2)))
        (should (equal next-id (overlay-get mine 'niwa-id))))
      (let* ((record (test-niwa-instruction-store--record workspace))
             (ids (cl-loop for (_ . entry) in (plist-get record :files)
                           append (mapcar (lambda (instr)
                                            (plist-get (plist-get instr :properties)
                                                       'niwa-id))
                                          (plist-get entry :instructions)))))
        (should (= 3 (length ids)))
        (should (equal ids (delete-dups (copy-sequence ids))))
        (should (equal "one" (test-niwa-instruction-store--stored-text
                              workspace first-id))))))
  :doc "a stale client's new note cannot reuse a number past the other counter"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((first (test-niwa-instruction-store--note source "one" 1))
           (uuid (overlay-get first 'niwa-uuid)))
      (test-niwa-instruction-store--other-client
       workspace (lambda (record)
                   (plist-put record :ids '(:id-counter 10 :used-ids (1) :retired-ids nil))))
      (let* ((mine (test-niwa-instruction-store--note source "two" 2))
             (new-uuid (overlay-get mine 'niwa-uuid)))
        (should (= 11 (test-niwa-instruction-store--stored workspace new-uuid 'niwa-id)))
        (should (= 1 (test-niwa-instruction-store--stored workspace uuid 'niwa-id))))))
  :doc "editing an existing note preserves its number below another client's counter"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((note (test-niwa-instruction-store--note source "one"))
           (uuid (overlay-get note 'niwa-uuid)))
      (test-niwa-instruction-store--other-client
       workspace (lambda (record)
                   (plist-put record :ids '(:id-counter 10 :used-ids (1) :retired-ids nil))))
      (niwa-note-edit "edited" note)
      (should (eq 'merged (niwa-instruction-store-sync workspace)))
      (should (= 1 (test-niwa-instruction-store--stored workspace uuid 'niwa-id)))))
  :doc "a write landing between this client's read and write is merged on retry"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((first (test-niwa-instruction-store--note source "one" 1))
           (second (test-niwa-instruction-store--note source "two" 2))
           (first-id (overlay-get first 'niwa-uuid))
           (second-id (overlay-get second 'niwa-uuid))
           (write (symbol-function 'niwa-instruction-store--write))
           (test-niwa-instruction-store--writer write)
           (calls 0) first-result)
      (niwa-note-edit "two by me" second)
      (cl-letf (((symbol-function 'niwa-instruction-store--write)
                 (lambda (&rest args)
                   (when (= 1 (cl-incf calls))
                     (test-niwa-instruction-store--other-client
                      workspace (lambda (record)
                                  (test-niwa-instruction-store--edit-record
                                   record first-id 'niwa-note-text "one by them")))
                     (setq first-result (apply write args))
                     first-result)
                   (if (= calls 1) first-result (apply write args)))))
        (should (eq 'merged (niwa-instruction-store-sync workspace))))
      ;; Control: the interleaving was reached and made the first write stale.
      (should (= 2 calls))
      (should-not first-result)
      (should (equal "one by them" (test-niwa-instruction-store--stored-text
                                    workspace first-id)))
      (should (equal "two by me" (test-niwa-instruction-store--stored-text
                                  workspace second-id)))))
  :doc "a write waits for another process holding the store lock"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (skip-unless (executable-find "flock"))
    (let* ((overlay (test-niwa-instruction-store--note source "one"))
           (lock (niwa-instruction-store--lock-directory workspace))
           (ready (file-name-concat (niwa-workspace-root workspace) "locked"))
           (holder (start-process "store-lock" nil "flock" lock "-c"
                                  (format "touch %s; sleep 1.5"
                                          (shell-quote-argument ready))))
           (deadline (+ (float-time) 5)))
      (unwind-protect
          (progn
            (while (and (not (file-exists-p ready)) (< (float-time) deadline))
              (accept-process-output holder 0.05))
            (should (file-exists-p ready))
            (niwa-note-edit "after the lock" overlay)
            (let ((start (float-time)))
              (should (eq 'written (niwa-instruction-store-sync workspace)))
              (should (> (- (float-time) start) 0.8))))
        (when (process-live-p holder) (delete-process holder)))
      ;; Control: without a holder the same write does not wait.
      (niwa-note-edit "unlocked" overlay)
      (let ((start (float-time)))
        (should (eq 'written (niwa-instruction-store-sync workspace)))
        (should (< (- (float-time) start) 0.8))))))


;;; Format version

(niwa-deftest niwa-instruction-store--read (:quiet t)
  ,test
  (test)
  :doc "absent, readable, unsupported and corrupt stores are told apart"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (should (eq 'absent (plist-get (niwa-instruction-store--read workspace) :status)))
    (test-niwa-instruction-store--note source "one")
    (should (eq 'ok (plist-get (niwa-instruction-store--read workspace) :status)))
    (dolist (case '(("(:format 99 :files nil)" . unsupported)
                    ("(:format 2 :ids nil :files (7))" . corrupt)
                    ("(:format 1 :ids nil" . corrupt)
                    ("(:format 1 :ids nil :files nil) extra" . corrupt)))
      (with-temp-file (niwa-instruction-store-path workspace)
        (insert (car case)))
      (should (eq (cdr case)
                  (plist-get (niwa-instruction-store--read workspace) :status)))))
  :doc "an unreadable store refuses a write and keeps memory and bytes"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-niwa-instruction-store--note source "one")))
      (dolist (bytes '("(:format 99 :files nil)\n" "(:format 1 :ids nil"))
        (with-temp-file (niwa-instruction-store-path workspace)
          (insert bytes))
        (niwa-note-edit "changed" overlay)
        (should-error (niwa-instruction-store-sync workspace) :type 'user-error)
        (should (equal bytes (with-temp-buffer
                               (insert-file-contents
                                (niwa-instruction-store-path workspace))
                               (buffer-string))))
        (should (equal '("changed")
                       (test-niwa-instruction-store--texts workspace))))))
  :doc "control: the same change against a readable store is written"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-niwa-instruction-store--note source "one")))
      (niwa-note-edit "changed" overlay)
      (should (eq 'written (niwa-instruction-store-sync workspace)))))
  :doc "an unreadable store is reported once and not re-read after every command"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-niwa-instruction-store--note source "one"))
          (reads 0)
          (read (symbol-function 'niwa-instruction-store--read)))
      (with-temp-file (niwa-instruction-store-path workspace)
        (insert "(:format 1 :ids nil"))
      (cl-letf (((symbol-function 'niwa-instruction-store--read)
                 (lambda (&rest args) (cl-incf reads) (apply read args))))
        (niwa-note-edit "changed" overlay)
        (niwa-instruction-store-flush)
        (should (= 1 reads))
        (should (memq workspace niwa-instruction-store--dirty))
        (should (alist-get workspace niwa-instruction-store--failed))
        (niwa-instruction-store-flush)
        (should (= 1 reads))
        ;; Control: the next observation point reads it again.
        (should-error (niwa-instruction-store-observe workspace)
                      :type 'niwa-instruction-store-unreadable)
        (should (= 2 reads)))))
  :doc "a stale write whose fallback cannot read the store keeps the change pending"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-niwa-instruction-store--note source "one"))
          (reads 0)
          (read (symbol-function 'niwa-instruction-store--read)))
      (niwa-note-edit "changed" overlay)
      (cl-letf (((symbol-function 'niwa-instruction-store--write) #'ignore)
                ((symbol-function 'niwa-instruction-store--read)
                 (lambda (&rest args)
                   (if (> (cl-incf reads) 2)
                       (list :status 'corrupt :reason "gone")
                     (apply read args)))))
        (should (eq 'stale (niwa-instruction-store-sync workspace))))
      (should (= 3 reads))
      (should (memq workspace niwa-instruction-store--dirty))
      (should (equal '("changed") (test-niwa-instruction-store--texts workspace)))))
  :doc "the carrier reads, creates and refuses stale writes over a TRAMP path"
  (niwa-test--with-local-shell-tramp '("instruction-store")
    (let* ((local (file-name-as-directory (make-temp-file "niwa-remote-store-" t)))
           (root (format "/niwamock:instruction-store:%s" local))
           (workspace (niwa-workspace-get-or-create 'project root root "remote-store"))
           (record (list :format niwa-instruction-store-format-version
                         :ids (list :id-counter 0 :used-ids nil :retired-ids nil)
                         :files nil :rewinds nil)))
      (unwind-protect
          (progn
            (niwa-workspace-identity-ensure root)
            (should (eq 'absent (plist-get (niwa-instruction-store--read workspace)
                                           :status)))
            (let ((hash (niwa-instruction-store--write workspace nil record)))
              (should hash)
              (should (file-exists-p (file-name-concat local ".niwa"
                                                       "instructions.el")))
              (let ((observation (niwa-instruction-store--read workspace)))
                (should (eq 'ok (plist-get observation :status)))
                (should (equal hash (plist-get observation :hash))))
              ;; Control: both preconditions refuse once they no longer hold.
              (should-not (niwa-instruction-store--write workspace nil record))
              (should-not (niwa-instruction-store--write
                           workspace (make-string 64 ?0) record))
              (should (niwa-instruction-store--write workspace hash record))))
        (delete-directory local t)
        (niwa-workspace-clear-registry)))))


;;; Flushing and store creation

(niwa-deftest niwa-instruction-store-applied-backups
  (:quiet t :doc "session save prunes unused file history when the workspace has no notes store")
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((niwa-sessions-directory (file-name-concat (file-name-directory file) "sessions"))
           (session (niwa-session-create "without-notes" workspace)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "Hello\n")
            (niwa-session-artifacts-save session (current-buffer) nil t)
            (delete-file (niwa-instruction-store-path workspace))
            (niwa--clear-instruction-state workspace)
            (let* ((logical (file-name-concat "file-history" (secure-hash 'sha256 "stale")))
                   (stale (file-name-concat (niwa-session-save-path session) logical)))
              (niwa-session-publication-publish
               session
               (list (list :path stale :content "stale")
                     (list :path (niwa-session-artifacts-sidecar-path
                                  (niwa-session-save-path session))
                           :content (niwa-session-artifacts-read-artifact session "session.meta.el" t)
                           :commit-marker t)))
              (should (equal "stale" (niwa-session-artifacts-read-artifact session logical t)))
              (should-not (file-exists-p (niwa-instruction-store-path workspace)))
              (niwa-session-artifacts-save session (current-buffer) nil t)
              (should-not (assoc logical (plist-get (niwa-session-publication session) :artifacts)))))
        (niwa-session-persistence-lock-release (niwa-session-save-path session) session)))))

(niwa-deftest niwa-instruction-store-flush (:quiet t)
  ,test
  (test)
  :doc "a failed flush backs off in an Emacs that has not loaded session persistence"
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (root (file-name-directory (locate-library "niwa-persistence"))))
    (with-temp-buffer
      (let ((status
             (call-process
              emacs nil t nil "--batch" "-Q"
              "--eval"
              (prin1-to-string
               `(progn
                  (setq load-path ',(cons root load-path))
                  (require 'niwa-persistence)
                  (when (featurep 'niwa-session-persistence)
                    (error "Session persistence must start unloaded"))
                  (let ((workspace (niwa-workspace--create
                                    :type 'project :id "x" :root "/tmp/x/"
                                    :name "x"))
                        (attempts 0))
                    (cl-letf (((symbol-function 'niwa-instruction-store-sync)
                               (lambda (&rest _)
                                 (cl-incf attempts) (error "Disk full")))
                              ((symbol-function 'message) #'ignore))
                      (push workspace niwa-instruction-store--dirty)
                      (dotimes (_ 3) (niwa-instruction-store-flush))
                      (unless (= attempts 1)
                        (error "Expected 1 attempt, got %d" attempts)))))))))
        (ert-info ((buffer-string))
          (should (equal 0 status))))))
  :doc "exit flush writes an edit held through an unreadable store restored to identical bytes"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-niwa-instruction-store--note source "one"))
           (uuid (overlay-get overlay 'niwa-uuid))
           (path (niwa-instruction-store-path workspace)))
      (niwa-instruction-store-flush t)
      (let ((good (niwa-session-artifacts-read-file-raw path)))
        (write-region "(:format 999 :ids nil :files nil)\n" nil path nil 'silent)
        (niwa-note-edit "changed" overlay)
        (niwa-instruction-store-flush)
        (let ((pending (memq workspace niwa-instruction-store--dirty))
              (failed (alist-get workspace niwa-instruction-store--failed)))
          (let ((coding-system-for-write 'no-conversion))
            (write-region good nil path nil 'silent))
          (should (eq 'current (niwa-instruction-store-observe workspace)))
          (niwa-instruction-store--flush-at-exit)
          (should (equal "changed" (test-niwa-instruction-store--stored-text workspace uuid)))
          (should pending)
          (should failed)
          (should-not (memq workspace niwa-instruction-store--dirty)))))))


(niwa-deftest niwa-instruction-store-sync/absent (:quiet t)
  ,test
  (test)
  :doc "a workspace without a store is new until its first note creates one"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (should (eq 'new (niwa-instruction-test--store-state workspace)))
    (test-niwa-instruction-store--note source "one")
    (should (eq 'active (niwa-instruction-test--store-state workspace))))
  :doc "without a .niwa directory no store or directory is created"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-plain-" t)))
         (workspace (niwa-workspace-get-or-create 'file "plain" root "plain")))
    (unwind-protect
        (progn
          (should (eq 'none (niwa-instruction-store-sync workspace)))
          (should-not (file-exists-p (file-name-concat root ".niwa"))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))


;;; Export and import

(niwa-deftest niwa-load-instructions (:quiet t)
  ,test
  (test)
  :doc "an export loads back and replaces the store"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((export (file-name-concat (niwa-workspace-root workspace) "export.el"))
           (uuid (overlay-get (test-niwa-instruction-store--note source "exported")
                              'niwa-uuid)))
      (with-current-buffer source (niwa-save-instructions export))
      (niwa-note-delete (niwa--instruction-with-uuid uuid workspace))
      (niwa-instruction-store-sync workspace)
      (should (eq 'closed (test-niwa-instruction-store--stored
                           workspace uuid 'niwa-note-state)))
      (with-current-buffer source (niwa-load-instructions export))
      (should (equal '("exported") (test-niwa-instruction-store--texts workspace)))
      (should (eq 'unsent (test-niwa-instruction-store--stored
                           workspace uuid 'niwa-note-state)))))
  :doc "loading refuses to replace a store changed since it was observed"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let* ((export (file-name-concat (niwa-workspace-root workspace) "export.el"))
           (overlay (test-niwa-instruction-store--note source "mine"))
           (uuid (overlay-get overlay 'niwa-uuid)))
      (with-current-buffer source (niwa-save-instructions export))
      (test-niwa-instruction-store--other-client
       workspace (lambda (record)
                   (test-niwa-instruction-store--edit-record
                    record uuid 'niwa-note-text "theirs")))
      (with-current-buffer source
        (should-error (niwa-load-instructions export) :type 'user-error))
      (should (equal "theirs" (test-niwa-instruction-store--stored-text
                               workspace uuid)))))
  :doc "an import over a store this Emacs never read is refused"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let ((export (file-name-concat (niwa-workspace-root workspace) "export.el")))
      (test-niwa-instruction-store--note source "stored")
      (with-current-buffer source (niwa-save-instructions export))
      (let ((before (test-niwa-instruction-store--record workspace))
            (fresh (test-niwa-instruction-store--reactivate workspace)))
        ;; Activation loaded it; dropping the observation is what a forget does.
        (niwa-instruction-store--forget fresh)
        (with-temp-buffer
          (setq-local niwa--workspace fresh)
          (should-error (niwa-load-instructions export) :type 'user-error))
        (should (equal before (test-niwa-instruction-store--record workspace)))
        ;; Control: after a listing observes the store, the import proceeds.
        (test-niwa-instruction-store--listed fresh)
        (with-temp-buffer
          (setq-local niwa--workspace fresh)
          (niwa-load-instructions export))
        (should (equal '("stored") (test-niwa-instruction-store--texts fresh))))))
  :doc "an export from another installation's version loads"
  (test-niwa-instruction-store--with-workspace (workspace source)
    (let ((export (file-name-concat (niwa-workspace-root workspace) "export.el")))
      (test-niwa-instruction-store--note source "elsewhere")
      (with-current-buffer source (niwa-save-instructions export))
      (let ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer)))))
        (with-temp-file export
          (prin1 (plist-put data :version "9.9.9") (current-buffer)))
        (with-current-buffer source (niwa-load-instructions export))
        (should (equal '("elsewhere") (test-niwa-instruction-store--texts workspace)))
        ;; Control: a file without the shared format from another version is refused.
        (with-temp-file export
          (prin1 (plist-put (copy-sequence (niwa-instruction-store--without
                                            data '(:format)))
                            :version "9.9.9")
                 (current-buffer)))
        (with-current-buffer source
          (should-error (niwa-load-instructions export) :type 'user-error)))))
  :doc "an import where no .niwa exists creates none"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-plain-import-" t)))
         (workspace (niwa-workspace-get-or-create 'file "plain-import" root
                                                     "plain-import"))
         (file (file-name-concat root "source.el"))
         (export (file-name-concat root "export.el"))
         buffer)
    (unwind-protect
        (progn
          (write-region "(defun beta () t)\n" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (setq-local niwa--workspace workspace)
            (niwa-test--with-captured-diagnostics nil
              (niwa-note-create "plain"))
            (niwa-save-instructions export)
            (niwa-load-instructions export))
          (should (equal '("plain") (test-niwa-instruction-store--texts workspace)))
          (should-not (file-exists-p (file-name-concat root ".niwa"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (niwa--clear-instruction-state workspace)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))


;;; Rewind intents

(defmacro test-niwa-instruction-store--with-rewind (vars &rest body)
  "Run BODY with VARS (WORKSPACE SESSION UUID) around a note with activity.
SESSION has two committed turns and sent the note, which holds a change
SESSION prepared in turn 2, written to the store."
  (declare (indent 1))
  `(test-niwa-instruction-store--with-workspace (,(nth 0 vars) source)
     (let* ((,(nth 1 vars) (niwa-session-create "s" ,(nth 0 vars)))
            (overlay (test-niwa-instruction-store--note source "do it"))
            (,(nth 2 vars) (overlay-get overlay 'niwa-uuid)))
       (test-niwa-instruction-store--prepare
        overlay (niwa-session-session-id ,(nth 1 vars)) 2)
       (setf (niwa-session-turn-count ,(nth 1 vars)) 2)
       (should (eq 'written (niwa-instruction-store-sync ,(nth 0 vars))))
       ,@body)))

(defun test-niwa-instruction-store--prepare (note session-id turn)
  "Mark NOTE sent by SESSION-ID with a change prepared in TURN.
The overlay properties are what preparing a change leaves behind."
  (niwa-note-mark-sent (list note) session-id)
  (niwa-note-hold-change (list note)
                            (list :id (format "%s-%d" session-id turn)
                                  :session-id session-id :turn turn)))

(niwa-deftest niwa-instruction-store-rewind-end (:quiet t)
  ,test
  (test)
  :doc "a committed Rewind prunes the store and the live note, keeping the note sent"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (should (plist-get (test-niwa-instruction-store--record workspace) :rewinds))
    (setf (niwa-session-turn-count session) 1)
    (niwa-instruction-store-rewind-end session t)
    (should-not (test-niwa-instruction-store--prepared-turn workspace uuid))
    (should-not (plist-get (test-niwa-instruction-store--record workspace) :rewinds))
    ;; Only the discarded turn's activity goes: the note stays sent and linked.
    (should (eq 'sent (test-niwa-instruction-store--stored
                       workspace uuid 'niwa-note-state)))
    (let ((live (niwa--instruction-with-uuid uuid workspace)))
      (should (overlay-buffer live))
      (should-not (overlay-get live 'niwa-note-prepared-change))
      (should (equal (niwa-session-session-id session)
                     (overlay-get live 'niwa-note-session-id)))))
  :doc "a rolled-back Rewind drops its intent and keeps the activity"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (niwa-instruction-store-rewind-end session nil)
    (should (equal 2 (test-niwa-instruction-store--prepared-turn workspace uuid)))
    (should-not (plist-get (test-niwa-instruction-store--record workspace) :rewinds)))
  :doc "another client's write during the Rewind survives its prune"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (test-niwa-instruction-store--other-client
     workspace (lambda (data)
                 (test-niwa-instruction-store--edit-record
                  data uuid 'niwa-note-text "edited during Rewind")))
    (setf (niwa-session-turn-count session) 1)
    (niwa-instruction-store-rewind-end session t)
    (should-not (test-niwa-instruction-store--prepared-turn workspace uuid))
    (should (equal "edited during Rewind"
                   (test-niwa-instruction-store--stored-text workspace uuid))))
  :doc "a failed post-commit prune is owed and resolved before the next request"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (setf (niwa-session-turn-count session) 1)
    (cl-letf (((symbol-function 'niwa-instruction-store--write) #'ignore))
      (should-error (niwa-instruction-store-rewind-end session t)))
    (should (equal 2 (test-niwa-instruction-store--prepared-turn workspace uuid)))
    (should (niwa-instruction-store-owes-p session))
    (should (niwa-instruction-store-settle-session session))
    (should-not (test-niwa-instruction-store--prepared-turn workspace uuid)))
  :doc "without a store file a committed Rewind prunes the live notes"
  ;; Rule 6: replaces the Rewind-time snapshot load, the only thing that
  ;; pruned notes when no store file existed (`rewind-begin' returned nil).
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (delete-file (niwa-instruction-store-path workspace))
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (setf (niwa-session-turn-count session) 1)
    (niwa-instruction-store-rewind-end session t)
    (let ((live (niwa--instruction-with-uuid uuid workspace)))
      (should-not (overlay-get live 'niwa-note-prepared-change))
      (should (eq 'sent (overlay-get live 'niwa-note-state))))
    ;; The prune wrote no store file.
    (should-not (file-exists-p (niwa-instruction-store-path workspace))))
  :doc "control: without a store file a rolled-back Rewind keeps the live activity"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (delete-file (niwa-instruction-store-path workspace))
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (niwa-instruction-store-rewind-end session nil)
    (should (overlay-get (niwa--instruction-with-uuid uuid workspace)
                         'niwa-note-prepared-change))))

(niwa-deftest niwa-instruction-store-settle-session (:quiet t)
  ,test
  (test)
  :doc "a crash before the commit point drops the intent at resume"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (test-niwa-instruction-store--restart workspace)
    (should (niwa-instruction-store-settle-session session))
    (should (equal 2 (test-niwa-instruction-store--prepared-turn workspace uuid)))
    (should-not (plist-get (test-niwa-instruction-store--record workspace) :rewinds)))
  :doc "a crash after the commit point prunes at resume"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (setf (niwa-session-turn-count session) 1)
    (test-niwa-instruction-store--restart workspace)
    (should (niwa-instruction-store-settle-session session))
    (should-not (test-niwa-instruction-store--prepared-turn workspace uuid)))
  :doc "an unreadable store at resume keeps a committed Rewind owed until it reads"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (let* ((path (niwa-instruction-store-path workspace))
           bytes)
      (should (niwa-instruction-store-rewind-begin session 1 2))
      ;; The Rewind committed; Emacs died before the store was pruned.
      (setf (niwa-session-turn-count session) 1)
      (test-niwa-instruction-store--restart workspace)
      (setq bytes (with-temp-buffer
                    (insert-file-contents-literally path)
                    (buffer-string)))
      (with-temp-file path (insert "(:format 1 :ids nil"))
      ;; Resume fails on the unreadable store.
      (should-error (niwa-instruction-store-restore session))
      ;; Admission must keep refusing while the intent cannot be read.
      (should (niwa-instruction-store-owes-p session))
      (should-error (niwa-instruction-store-settle-session session)
                    :type 'niwa-instruction-store-unreadable)
      (let ((coding-system-for-write 'no-conversion))
        (write-region bytes nil path nil 'silent))
      (should (niwa-instruction-store-settle-session session))
      (should-not (test-niwa-instruction-store--prepared-turn workspace uuid))
      (should-not (niwa-instruction-store-owes-p session))))
  :doc "control: a session resumed without an intent owes nothing"
  (test-niwa-instruction-store--with-rewind (workspace session _uuid)
    (test-niwa-instruction-store--restart workspace)
    (niwa-instruction-store-restore session)
    (should-not (niwa-instruction-store-owes-p session)))
  :doc "another session's Rewind prunes only its own activity"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    (let* ((other (niwa-session-create "t" workspace))
           (theirs (test-niwa-instruction-store--note source "theirs" 2))
           (their-uuid (overlay-get theirs 'niwa-uuid)))
      (test-niwa-instruction-store--prepare
       theirs (niwa-session-session-id other) 1)
      (setf (niwa-session-turn-count other) 1)
      (niwa-instruction-store-sync workspace)
      (should (niwa-instruction-store-rewind-begin session 1 2))
      (should (niwa-instruction-store-rewind-begin other 0 1))
      (setf (niwa-session-turn-count other) 0)
      (should (niwa-instruction-store-settle-session other))
      (should (equal 2 (test-niwa-instruction-store--prepared-turn workspace uuid)))
      (should-not (test-niwa-instruction-store--prepared-turn workspace their-uuid))
      (should (eq 'unsent (test-niwa-instruction-store--stored
                           workspace their-uuid 'niwa-note-state)))
      (should (= 1 (length (plist-get (test-niwa-instruction-store--record workspace)
                                      :rewinds))))
      (setf (niwa-session-turn-count session) 1)
      (should (niwa-instruction-store-settle-session session))
      (should-not (test-niwa-instruction-store--prepared-turn workspace uuid))))
  :doc "an intent matching neither turn count is refused"
  (test-niwa-instruction-store--with-rewind (workspace session _uuid)
    (should (niwa-instruction-store-rewind-begin session 1 2))
    (setf (niwa-session-turn-count session) 5)
    (should-error (niwa-instruction-store-settle-session session) :type 'user-error)
    (should (plist-get (test-niwa-instruction-store--record workspace) :rewinds)))
  :doc "control: activity above the committed turn without an intent is kept"
  (test-niwa-instruction-store--with-rewind (workspace session uuid)
    ;; A lost turn's change carries a turn the session never committed.
    (setf (niwa-session-turn-count session) 1)
    (should-not (niwa-instruction-store-settle-session session))
    (should (equal 2 (test-niwa-instruction-store--prepared-turn workspace uuid)))))



;;; Over TRAMP

(niwa-deftest niwa-instruction-store-observe/remote (:quiet t)
  ,test
  (test)
  :doc "activation, observation, merge and Rewind over a TRAMP root"
  (niwa-test--with-local-shell-tramp '("instruction-remote")
    (let* ((local (file-name-as-directory (make-temp-file "niwa-remote-ws-" t)))
           (root (format "/niwamock:instruction-remote:%s" local))
           (programs 0)
           (run (symbol-function 'niwa-session-control-fs-run-program))
           (counted (lambda (function)
                      (setq programs 0)
                      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                                 (lambda (&rest args) (cl-incf programs) (apply run args))))
                        (prog1 (funcall function) (setq programs programs)))))
           (note (lambda (text session)
                   (list "source.el" :original-content "(defun beta () t)\n"
                         :instructions
                         (list (list :overlay-start 1 :overlay-end 6
                                     :anchor (list :uuid "N1" :text "(defu")
                                     :properties
                                     (list 'niwa-instruction t 'niwa-id 1
                                           'niwa-uuid "N1"
                                           'niwa-instruction-type 'note
                                           'niwa-note-text text
                                           'niwa-note-author 'user
                                           'niwa-note-state 'sent
                                           'niwa-note-session-id session
                                           'niwa-note-prepared-change
                                           (list :id "c" :session-id session :turn 2)))))))
           (record (lambda (files)
                     (list :format niwa-instruction-store-format-version
                           :ids (list :id-counter 1 :used-ids '(1) :retired-ids nil)
                           :files files :rewinds nil)))
           workspace session)
      (unwind-protect
          (progn
            (setq workspace (niwa-workspace-get-or-create
                             'project root root "instruction-remote"))
            (niwa-workspace-identity-ensure root)
            (setq session (niwa-session-create "s" workspace root))
            (let ((sid (niwa-session-session-id session)))
              ;; Another client writes the store; activation loads it in one read.
              (should (niwa-instruction-store--write
                       workspace nil
                       (funcall record (list (funcall note "first" sid)))))
              (niwa-workspace-clear-registry)
              (setq workspace (niwa-workspace-get-or-create
                               'project root root "instruction-remote"))
              (setf (niwa-session-workspace session) workspace)
              (should (equal '("first")
                             (test-niwa-instruction-store--texts workspace)))
              ;; An unchanged store costs one hash program.
              (should (eq 'current (funcall counted
                                            (lambda ()
                                              (niwa-instruction-store-observe
                                               workspace)))))
              (should (= 1 programs))
              ;; A change elsewhere is merged at the next observation:
              ;; hash, read, and a write when memory differs.
              (test-niwa-instruction-store--other-client
               workspace (lambda (data)
                           (test-niwa-instruction-store--edit-record
                            data "N1" 'niwa-note-text "second")))
              (funcall counted (lambda () (niwa-instruction-store-observe workspace)))
              (should (<= programs 3))
              (should (equal '("second")
                             (test-niwa-instruction-store--texts workspace)))
              ;; Rewind: intent, commit, prune.
              (setf (niwa-session-turn-count session) 2)
              (should (funcall counted
                               (lambda ()
                                 (niwa-instruction-store-rewind-begin session 1 2))))
              (should (= 2 programs))
              (setf (niwa-session-turn-count session) 1)
              (funcall counted (lambda () (niwa-instruction-store-rewind-end session t)))
              (should (<= programs 3))
              (should-not (test-niwa-instruction-store--prepared-turn workspace "N1"))
              (should-not (plist-get (test-niwa-instruction-store--record workspace)
                                     :rewinds))))
        (when workspace (niwa--clear-instruction-state workspace))
        (test-niwa-session-persistence--reset-instructions)
        (delete-directory local t)
        (niwa-workspace-clear-registry)))))

(provide 'test-niwa-persistence-instruction-store)
;;; test-niwa-persistence-instruction-store.el ends here
