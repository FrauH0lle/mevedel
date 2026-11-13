;;; test-mevedel-persistence-instruction-store.el -- Workspace instruction store tests -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace instruction store through its callers' interface, on real
;; files and the real pinned control-file programs.  Another client is
;; played at the store boundary: it reads the record, changes it the way an
;; edit in another Emacs would, and writes it with the hash it read.  Every
;; claim has a control showing the discriminating condition matters.

;;; Code:

(require 'mevedel-notes)

(require 'mevedel)
(require 'mevedel-persistence)
(require 'mevedel-session-control-fs)
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

(defmacro test-mevedel-instruction-store--with-workspace (vars &rest body)
  "Run BODY with VARS (WORKSPACE SOURCE) in a fresh workspace.
SOURCE visits `source.el' holding two functions.  The workspace has its
`.mevedel/' identity, as one that ever saved a session does."
  (declare (indent 1))
  (let ((tempdir (make-symbol "tempdir")))
    `(cl-destructuring-bind (,(nth 0 vars) . ,tempdir)
         (test-mevedel-session-persistence--make-tempdir-workspace)
       ;; Pending writes other tests left behind are not this case's: a
       ;; flush here sees only its own workspace.
       (let ((,(nth 1 vars) nil)
             (mevedel-instruction-store--dirty nil)
             (mevedel-instruction-store--failed nil)
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

(defun test-mevedel-instruction-store--note (source text &optional line)
  "Create note TEXT on LINE (1 or 2) of SOURCE and return its overlay."
  (with-current-buffer source
    (goto-char (point-min))
    (forward-line (1- (or line 1)))
    (mevedel-note-create text)))

(defun test-mevedel-instruction-store--save-session (workspace name)
  "Save a session NAME of WORKSPACE with the real session writer.
Creating WORKSPACE's store is not under test, so its sync is stubbed."
  (let ((session (mevedel-session-create name workspace))
        (buffer (generate-new-buffer " *stored-session*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "Hello\n")
          (cl-letf (((symbol-function 'mevedel-instruction-store-sync) #'ignore))
            (mevedel-session-artifacts-save session buffer)))
      (kill-buffer buffer))))

(defun test-mevedel-instruction-store--restart (workspace)
  "Forget WORKSPACE's in-memory instructions, as a fresh Emacs would."
  (mevedel--clear-instruction-state workspace)
  (test-mevedel-session-persistence--reset-instructions))

(defun test-mevedel-instruction-store--reactivate (workspace)
  "Restart, then resolve WORKSPACE's struct anew as a fresh Emacs would.
Return the new workspace struct."
  (test-mevedel-instruction-store--restart workspace)
  (mevedel-workspace-clear-registry)
  (mevedel-workspace-get-or-create
   (mevedel-workspace-type workspace) (mevedel-workspace-id workspace)
   (mevedel-workspace-root workspace) (mevedel-workspace-name workspace)))

(defun test-mevedel-instruction-store--listed (workspace)
  "Observe WORKSPACE's store and return its note texts."
  (mevedel-instruction-store-observe workspace)
  (test-mevedel-instruction-store--texts workspace))

(defun test-mevedel-instruction-store--record (workspace)
  "Return WORKSPACE's store record."
  (plist-get (mevedel-instruction-store--read workspace) :data))

(defun test-mevedel-instruction-store--stored (workspace uuid key)
  "Return property KEY of note UUID in WORKSPACE's store."
  (catch 'found
    (dolist (entry (plist-get (test-mevedel-instruction-store--record workspace) :files))
      (dolist (instruction (plist-get (cdr entry) :instructions))
        (let ((properties (plist-get instruction :properties)))
          (when (equal uuid (plist-get properties 'mevedel-uuid))
            (throw 'found (plist-get properties key))))))))

(defun test-mevedel-instruction-store--stored-text (workspace uuid)
  "Return the text of note UUID in WORKSPACE's store."
  (test-mevedel-instruction-store--stored workspace uuid 'mevedel-note-text))

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
  "Return store RECORD with property KEY of note UUID set to VALUE."
  (dolist (entry (plist-get record :files) record)
    (dolist (instruction (plist-get (cdr entry) :instructions))
      (let ((properties (plist-get instruction :properties)))
        (when (equal uuid (plist-get properties 'mevedel-uuid))
          (plist-put instruction :properties (plist-put properties key value)))))))

(defun test-mevedel-instruction-store--texts (workspace)
  "Return WORKSPACE's in-memory note texts, sorted."
  (sort (mapcar (lambda (note) (plist-get (plist-get note :properties) 'mevedel-note-text))
                (mevedel-notes--entries workspace))
        #'string<))

(defun test-mevedel-instruction-store--prepared-turn (workspace uuid)
  "Return the turn of note UUID's prepared change in WORKSPACE's store."
  (plist-get (test-mevedel-instruction-store--stored
              workspace uuid 'mevedel-note-prepared-change)
             :turn))


;;; Writes, restart and concurrent clients

(mevedel-deftest mevedel-instruction-store-sync (:quiet t)
  ,test
  (test)
  :doc "a creation is written at once and is back when the workspace activates"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (test-mevedel-instruction-store--note source "kept")
    (let ((fresh (test-mevedel-instruction-store--reactivate workspace)))
      (should (equal '("kept") (test-mevedel-instruction-store--texts fresh)))
      (should (equal '("kept") (test-mevedel-instruction-store--listed fresh)))
      ;; The visited source got its note back; other files wait for a visit.
      (mevedel--instruction-activate-workspace fresh)
      (should (= 1 (cl-count-if (lambda (instruction)
                               (eq 'note (overlay-get instruction 'mevedel-instruction-type)))
                                (alist-get source (mevedel--instruction-alist)))))))
  :doc "control: without activation the restarted workspace lists nothing"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (test-mevedel-instruction-store--note source "kept")
    (cl-letf (((symbol-function 'mevedel-instruction-store-activate) #'ignore)
              ((symbol-function 'mevedel-instruction-store-observe) #'ignore))
      (let ((fresh (test-mevedel-instruction-store--reactivate workspace)))
        (should-not (test-mevedel-instruction-store--texts fresh)))))
  :doc "observing the store reads another client's change"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--note source "mine"))
           (uuid (overlay-get overlay 'mevedel-uuid)))
      (should (equal '("mine") (test-mevedel-instruction-store--listed workspace)))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (test-mevedel-instruction-store--edit-record
                    record uuid 'mevedel-note-text "changed elsewhere")))
      (should (equal '("changed elsewhere")
                     (test-mevedel-instruction-store--listed workspace)))
      ;; Control: an unchanged store costs one hash and changes nothing.
      (should (eq 'current (mevedel-instruction-store-observe workspace)))))
  :doc "unchanged sync hashes but does not read, while a local edit still reads and writes"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((note (test-mevedel-instruction-store--note source "base"))
           (read-store (symbol-function 'mevedel-instruction-store--read))
           (reads 0))
      (mevedel-instruction-store-sync workspace)
      (cl-letf (((symbol-function 'mevedel-instruction-store--read)
                 (lambda (&rest args) (cl-incf reads) (apply read-store args))))
        (should (eq 'unchanged (mevedel-instruction-store-sync workspace)))
        (should (= 0 reads))
        (mevedel-note-edit "edited" note)
        (should (eq 'written (mevedel-instruction-store-sync workspace)))
        (should (= 1 reads)))
      (should (equal '("edited") (test-mevedel-instruction-store--listed workspace)))))
  :doc "uncertain sync evidence falls back to a full read without losing notes"
  (dolist (scenario '(hash-error absent observation changed corrupt))
    (test-mevedel-instruction-store--with-workspace (workspace source)
      (let* ((note (test-mevedel-instruction-store--note source "base"))
             (uuid (overlay-get note 'mevedel-uuid))
             (read-store (symbol-function 'mevedel-instruction-store--read))
             (hash-store (symbol-function 'mevedel-instruction-store--current-hash))
             (reads 0))
        (mevedel-instruction-store-sync workspace)
        (pcase scenario
          ('absent (delete-file (mevedel-instruction-store-path workspace)))
          ('observation
           (setf (plist-get (mevedel--instruction-state
                            (mevedel--instruction-workspace-key workspace)) :store-observation) nil))
          ('changed
           (test-mevedel-instruction-store--other-client
            workspace (lambda (record)
                        (test-mevedel-instruction-store--edit-record
                         record uuid 'mevedel-note-text "theirs"))))
          ('corrupt (write-region "broken" nil (mevedel-instruction-store-path workspace) nil 'silent)))
        (cl-letf (((symbol-function 'mevedel-instruction-store--read)
                   (lambda (&rest args) (cl-incf reads) (apply read-store args)))
                  ((symbol-function 'mevedel-instruction-store--current-hash)
                   (lambda (&rest args)
                     (if (eq scenario 'hash-error) (error "Hash unavailable")
                       (apply hash-store args)))))
          (if (eq scenario 'corrupt)
              (should-error (mevedel-instruction-store-sync workspace))
            (should (memq (mevedel-instruction-store-sync workspace) '(unchanged written merged))))
          (should (= 1 reads)))
        (should (equal (if (eq scenario 'changed) '("theirs") '("base"))
                       (test-mevedel-instruction-store--texts workspace))))))
  :doc "a note edited in the minibuffer is written once, after it returns"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--note source "base"))
           (uuid (overlay-get overlay 'mevedel-uuid))
           (syncs 0)
           (sync (symbol-function 'mevedel-instruction-store-sync)))
      (cl-letf (((symbol-function 'mevedel-instruction-store-sync)
                 (lambda (&rest args) (cl-incf syncs) (apply sync args))))
        (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
          (dolist (text '("b" "ba" "bas"))
            (mevedel-note-edit text overlay)
            (mevedel-instruction-store-flush)))
        (should (= 0 syncs))
        (should (memq workspace mevedel-instruction-store--dirty))
        ;; Control: once the minibuffer returns, one flush writes it.
        (mevedel-instruction-store-flush)
        (should (= 1 syncs)))
      (should (equal "bas" (test-mevedel-instruction-store--stored-text
                            workspace uuid)))))
  :doc "a failing write is retried once per checkpoint interval, not per command"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((attempts 0)
          (mevedel-session-checkpoint-interval 30))
      (cl-letf (((symbol-function 'mevedel-instruction-store-sync)
                 (lambda (&rest _) (cl-incf attempts) (error "Disk full")))
                ((symbol-function 'message) #'ignore))
        (test-mevedel-instruction-store--note source "base")
        (should (memq workspace mevedel-instruction-store--dirty))
        (setq attempts 0)
        (dotimes (_ 10) (mevedel-instruction-store-flush))
        (should (= 1 attempts))
        (should (memq workspace mevedel-instruction-store--dirty))
        (should (memq #'mevedel-instruction-store-flush post-command-hook))
        ;; Controls: a forced flush (exit) tries at once; so does a flush
        ;; once the interval has passed.
        (mevedel-instruction-store-flush t)
        (should (= 2 attempts))
        (setf (alist-get workspace mevedel-instruction-store--failed)
              (time-subtract nil 31))
        (mevedel-instruction-store-flush)
        (should (= 3 attempts)))))
  :doc "control: without the store a restart loses the creation"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (cl-letf (((symbol-function 'mevedel-instruction-store--applicable-p) #'ignore))
      (test-mevedel-instruction-store--note source "kept")
      (should-not (file-exists-p (mevedel-instruction-store-path workspace)))
      (test-mevedel-instruction-store--restart workspace)
      (mevedel-instruction-store-sync workspace)
      (should-not (test-mevedel-instruction-store--texts workspace))))
  :doc "an edit to an instruction another client changed is refused, its text kept"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--note source "base"))
           (uuid (overlay-get overlay 'mevedel-uuid))
           (kill-ring nil))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (test-mevedel-instruction-store--edit-record
                    record uuid 'mevedel-note-text "theirs")))
      (mevedel-note-edit "mine" overlay)
      (should (eq 'refused (car (mevedel-instruction-store-sync workspace))))
      (should (equal "theirs" (test-mevedel-instruction-store--stored-text
                               workspace uuid)))
      (should (equal '("theirs") (test-mevedel-instruction-store--texts workspace)))
      (should (equal "mine" (car kill-ring)))))
  :doc "control: the same edit without a concurrent change is written"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--note source "base"))
           (uuid (overlay-get overlay 'mevedel-uuid)))
      (mevedel-note-edit "mine" overlay)
      (should (eq 'written (mevedel-instruction-store-sync workspace)))
      (should (equal "mine" (test-mevedel-instruction-store--stored-text
                             workspace uuid)))))
  :doc "changes to different instructions merge without losing either"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--note source "one" 1))
           (second (test-mevedel-instruction-store--note source "two" 2))
           (first-id (overlay-get first 'mevedel-uuid))
           (second-id (overlay-get second 'mevedel-uuid)))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (test-mevedel-instruction-store--edit-record
                    record first-id 'mevedel-note-text "one by them")))
      (mevedel-note-edit "two by me" second)
      (should (eq 'merged (mevedel-instruction-store-sync workspace)))
      (should (equal "one by them" (test-mevedel-instruction-store--stored-text
                                    workspace first-id)))
      (should (equal "two by me" (test-mevedel-instruction-store--stored-text
                                  workspace second-id)))
      (should (equal '("one by them" "two by me")
                     (test-mevedel-instruction-store--texts workspace)))))
  :doc "a merged write drops a note another client deleted"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--note source "one" 1))
           (second (test-mevedel-instruction-store--note source "two" 2))
           (uuid (overlay-get first 'mevedel-uuid)))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (dolist (entry (plist-get record :files) record)
                     (setcdr entry
                             (plist-put (cdr entry) :instructions
                                        (cl-remove uuid (plist-get (cdr entry) :instructions)
                                                   :key (lambda (instruction)
                                                          (plist-get (plist-get instruction
                                                                                :properties)
                                                                     'mevedel-uuid))
                                                   :test #'equal))))))
      (mevedel-note-edit "two by me" second)
      (should (eq 'merged (mevedel-instruction-store-sync workspace)))
      (should (equal '("two by me") (test-mevedel-instruction-store--texts workspace)))))
  :doc "a creation after a concurrent write is retried and renumbers a taken id"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--note source "one" 1))
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
           (setq properties (plist-put properties 'mevedel-instruction-type 'note))
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
      (let ((mine (test-mevedel-instruction-store--note source "two" 2)))
        (should (equal next-id (overlay-get mine 'mevedel-id))))
      (let* ((record (test-mevedel-instruction-store--record workspace))
             (ids (cl-loop for (_ . entry) in (plist-get record :files)
                           append (mapcar (lambda (instr)
                                            (plist-get (plist-get instr :properties)
                                                       'mevedel-id))
                                          (plist-get entry :instructions)))))
        (should (= 3 (length ids)))
        (should (equal ids (delete-dups (copy-sequence ids))))
        (should (equal "one" (test-mevedel-instruction-store--stored-text
                              workspace first-id))))))
  :doc "a stale client's new note cannot reuse a number past the other counter"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--note source "one" 1))
           (uuid (overlay-get first 'mevedel-uuid)))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (plist-put record :ids '(:id-counter 10 :used-ids (1) :retired-ids nil))))
      (let* ((mine (test-mevedel-instruction-store--note source "two" 2))
             (new-uuid (overlay-get mine 'mevedel-uuid)))
        (should (= 11 (test-mevedel-instruction-store--stored workspace new-uuid 'mevedel-id)))
        (should (= 1 (test-mevedel-instruction-store--stored workspace uuid 'mevedel-id))))))
  :doc "editing an existing note preserves its number below another client's counter"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((note (test-mevedel-instruction-store--note source "one"))
           (uuid (overlay-get note 'mevedel-uuid)))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (plist-put record :ids '(:id-counter 10 :used-ids (1) :retired-ids nil))))
      (mevedel-note-edit "edited" note)
      (should (eq 'merged (mevedel-instruction-store-sync workspace)))
      (should (= 1 (test-mevedel-instruction-store--stored workspace uuid 'mevedel-id)))))
  :doc "a write landing between this client's read and write is merged on retry"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((first (test-mevedel-instruction-store--note source "one" 1))
           (second (test-mevedel-instruction-store--note source "two" 2))
           (first-id (overlay-get first 'mevedel-uuid))
           (second-id (overlay-get second 'mevedel-uuid))
           (write (symbol-function 'mevedel-instruction-store--write))
           (test-mevedel-instruction-store--writer write)
           (calls 0) first-result)
      (mevedel-note-edit "two by me" second)
      (cl-letf (((symbol-function 'mevedel-instruction-store--write)
                 (lambda (&rest args)
                   (when (= 1 (cl-incf calls))
                     (test-mevedel-instruction-store--other-client
                      workspace (lambda (record)
                                  (test-mevedel-instruction-store--edit-record
                                   record first-id 'mevedel-note-text "one by them")))
                     (setq first-result (apply write args))
                     first-result)
                   (if (= calls 1) first-result (apply write args)))))
        (should (eq 'merged (mevedel-instruction-store-sync workspace))))
      ;; Control: the interleaving was reached and made the first write stale.
      (should (= 2 calls))
      (should-not first-result)
      (should (equal "one by them" (test-mevedel-instruction-store--stored-text
                                    workspace first-id)))
      (should (equal "two by me" (test-mevedel-instruction-store--stored-text
                                  workspace second-id)))))
  :doc "a write waits for another process holding the store lock"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (skip-unless (executable-find "flock"))
    (let* ((overlay (test-mevedel-instruction-store--note source "one"))
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
            (mevedel-note-edit "after the lock" overlay)
            (let ((start (float-time)))
              (should (eq 'written (mevedel-instruction-store-sync workspace)))
              (should (> (- (float-time) start) 0.8))))
        (when (process-live-p holder) (delete-process holder)))
      ;; Control: without a holder the same write does not wait.
      (mevedel-note-edit "unlocked" overlay)
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
    (test-mevedel-instruction-store--note source "one")
    (should (eq 'ok (plist-get (mevedel-instruction-store--read workspace) :status)))
    (dolist (case '(("(:format 99 :files nil)" . unsupported)
                    ("(:format 2 :ids nil :files (7))" . corrupt)
                    ("(:format 1 :ids nil" . corrupt)
                    ("(:format 1 :ids nil :files nil) extra" . corrupt)))
      (with-temp-file (mevedel-instruction-store-path workspace)
        (insert (car case)))
      (should (eq (cdr case)
                  (plist-get (mevedel-instruction-store--read workspace) :status)))))
  :doc "an unreadable store refuses a write and keeps memory and bytes"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-mevedel-instruction-store--note source "one")))
      (dolist (bytes '("(:format 99 :files nil)\n" "(:format 1 :ids nil"))
        (with-temp-file (mevedel-instruction-store-path workspace)
          (insert bytes))
        (mevedel-note-edit "changed" overlay)
        (should-error (mevedel-instruction-store-sync workspace) :type 'user-error)
        (should (equal bytes (with-temp-buffer
                               (insert-file-contents
                                (mevedel-instruction-store-path workspace))
                               (buffer-string))))
        (should (equal '("changed")
                       (test-mevedel-instruction-store--texts workspace))))))
  :doc "control: the same change against a readable store is written"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-mevedel-instruction-store--note source "one")))
      (mevedel-note-edit "changed" overlay)
      (should (eq 'written (mevedel-instruction-store-sync workspace)))))
  :doc "an unreadable store is reported once and not re-read after every command"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-mevedel-instruction-store--note source "one"))
          (reads 0)
          (read (symbol-function 'mevedel-instruction-store--read)))
      (with-temp-file (mevedel-instruction-store-path workspace)
        (insert "(:format 1 :ids nil"))
      (cl-letf (((symbol-function 'mevedel-instruction-store--read)
                 (lambda (&rest args) (cl-incf reads) (apply read args))))
        (mevedel-note-edit "changed" overlay)
        (mevedel-instruction-store-flush)
        (should (= 1 reads))
        (should (memq workspace mevedel-instruction-store--dirty))
        (should (alist-get workspace mevedel-instruction-store--failed))
        (mevedel-instruction-store-flush)
        (should (= 1 reads))
        ;; Control: the next observation point reads it again.
        (should-error (mevedel-instruction-store-observe workspace)
                      :type 'mevedel-instruction-store-unreadable)
        (should (= 2 reads)))))
  :doc "a stale write whose fallback cannot read the store keeps the change pending"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((overlay (test-mevedel-instruction-store--note source "one"))
          (reads 0)
          (read (symbol-function 'mevedel-instruction-store--read)))
      (mevedel-note-edit "changed" overlay)
      (cl-letf (((symbol-function 'mevedel-instruction-store--write) #'ignore)
                ((symbol-function 'mevedel-instruction-store--read)
                 (lambda (&rest args)
                   (if (> (cl-incf reads) 2)
                       (list :status 'corrupt :reason "gone")
                     (apply read args)))))
        (should (eq 'stale (mevedel-instruction-store-sync workspace))))
      (should (= 3 reads))
      (should (memq workspace mevedel-instruction-store--dirty))
      (should (equal '("changed") (test-mevedel-instruction-store--texts workspace)))))
  :doc "the carrier reads, creates and refuses stale writes over a TRAMP path"
  (mevedel-test--with-local-shell-tramp '("instruction-store")
    (let* ((local (file-name-as-directory (make-temp-file "mevedel-remote-store-" t)))
           (root (format "/mevedelmock:instruction-store:%s" local))
           (workspace (mevedel-workspace-get-or-create 'project root root "remote-store"))
           (record (list :format mevedel-instruction-store-format-version
                         :ids (list :id-counter 0 :used-ids nil :retired-ids nil)
                         :files nil :rewinds nil)))
      (unwind-protect
          (progn
            (mevedel-workspace-identity-ensure root)
            (should (eq 'absent (plist-get (mevedel-instruction-store--read workspace)
                                           :status)))
            (let ((hash (mevedel-instruction-store--write workspace nil record)))
              (should hash)
              (should (file-exists-p (file-name-concat local ".mevedel"
                                                       "instructions.el")))
              (let ((observation (mevedel-instruction-store--read workspace)))
                (should (eq 'ok (plist-get observation :status)))
                (should (equal hash (plist-get observation :hash))))
              ;; Control: both preconditions refuse once they no longer hold.
              (should-not (mevedel-instruction-store--write workspace nil record))
              (should-not (mevedel-instruction-store--write
                           workspace (make-string 64 ?0) record))
              (should (mevedel-instruction-store--write workspace hash record))))
        (delete-directory local t)
        (mevedel-workspace-clear-registry)))))


;;; Flushing and store creation

(mevedel-deftest mevedel-instruction-store-applied-backups
  (:quiet t :doc "session save prunes unused file history when the workspace has no notes store")
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((mevedel-sessions-directory (file-name-concat (file-name-directory file) "sessions"))
           (session (mevedel-session-create "without-notes" workspace)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "Hello\n")
            (mevedel-session-artifacts-save session (current-buffer) nil t)
            (delete-file (mevedel-instruction-store-path workspace))
            (mevedel--clear-instruction-state workspace)
            (let ((stale (mevedel-session-artifacts-backup-path
                          (mevedel-session-save-path session) (secure-hash 'sha256 "stale"))))
              (make-directory (file-name-directory stale) t)
              (write-region "stale" nil stale nil 'silent)
              (should-not (file-exists-p (mevedel-instruction-store-path workspace)))
              (mevedel-session-artifacts-save session (current-buffer) nil t)
              (should-not (file-exists-p stale))))
        (mevedel-session-persistence-lock-release (mevedel-session-save-path session) session)))))

(mevedel-deftest mevedel-instruction-store-flush (:quiet t)
  ,test
  (test)
  :doc "a failed flush backs off in an Emacs that has not loaded session persistence"
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (root (file-name-directory (locate-library "mevedel-persistence"))))
    (with-temp-buffer
      (let ((status
             (call-process
              emacs nil t nil "--batch" "-Q"
              "--eval"
              (prin1-to-string
               `(progn
                  (setq load-path ',(cons root load-path))
                  (require 'mevedel-persistence)
                  (when (featurep 'mevedel-session-persistence)
                    (error "Session persistence must start unloaded"))
                  (let ((workspace (mevedel-workspace--create
                                    :type 'project :id "x" :root "/tmp/x/"
                                    :name "x"))
                        (attempts 0))
                    (cl-letf (((symbol-function 'mevedel-instruction-store-sync)
                               (lambda (&rest _)
                                 (cl-incf attempts) (error "Disk full")))
                              ((symbol-function 'message) #'ignore))
                      (push workspace mevedel-instruction-store--dirty)
                      (dotimes (_ 3) (mevedel-instruction-store-flush))
                      (unless (= attempts 1)
                        (error "Expected 1 attempt, got %d" attempts)))))))))
        (ert-info ((buffer-string))
          (should (equal 0 status))))))
  :doc "exit flush writes an edit held through an unreadable store restored to identical bytes"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((overlay (test-mevedel-instruction-store--note source "one"))
           (uuid (overlay-get overlay 'mevedel-uuid))
           (path (mevedel-instruction-store-path workspace)))
      (mevedel-instruction-store-flush t)
      (let ((good (mevedel-session-artifacts-read-file-raw path)))
        (write-region "(:format 999 :ids nil :files nil)\n" nil path nil 'silent)
        (mevedel-note-edit "changed" overlay)
        (mevedel-instruction-store-flush)
        (let ((pending (memq workspace mevedel-instruction-store--dirty))
              (failed (alist-get workspace mevedel-instruction-store--failed)))
          (let ((coding-system-for-write 'no-conversion))
            (write-region good nil path nil 'silent))
          (should (eq 'current (mevedel-instruction-store-observe workspace)))
          (mevedel-instruction-store--flush-at-exit)
          (should (equal "changed" (test-mevedel-instruction-store--stored-text workspace uuid)))
          (should pending)
          (should failed)
          (should-not (memq workspace mevedel-instruction-store--dirty)))))))


(mevedel-deftest mevedel-instruction-store-sync/absent (:quiet t)
  ,test
  (test)
  :doc "a workspace without a store is new until its first note creates one"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (should (eq 'new (mevedel-instruction-test--store-state workspace)))
    (test-mevedel-instruction-store--note source "one")
    (should (eq 'active (mevedel-instruction-test--store-state workspace))))
  :doc "without a .mevedel directory no store or directory is created"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-plain-" t)))
         (workspace (mevedel-workspace-get-or-create 'file "plain" root "plain")))
    (unwind-protect
        (progn
          (should (eq 'none (mevedel-instruction-store-sync workspace)))
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


;;; Export and import

(mevedel-deftest mevedel-load-instructions (:quiet t)
  ,test
  (test)
  :doc "an export loads back and replaces the store"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((export (file-name-concat (mevedel-workspace-root workspace) "export.el"))
           (uuid (overlay-get (test-mevedel-instruction-store--note source "exported")
                              'mevedel-uuid)))
      (with-current-buffer source (mevedel-save-instructions export))
      (mevedel-note-delete (mevedel--instruction-with-uuid uuid workspace))
      (mevedel-instruction-store-sync workspace)
      (should (eq 'closed (test-mevedel-instruction-store--stored
                           workspace uuid 'mevedel-note-state)))
      (with-current-buffer source (mevedel-load-instructions export))
      (should (equal '("exported") (test-mevedel-instruction-store--texts workspace)))
      (should (eq 'unsent (test-mevedel-instruction-store--stored
                           workspace uuid 'mevedel-note-state)))))
  :doc "loading refuses to replace a store changed since it was observed"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let* ((export (file-name-concat (mevedel-workspace-root workspace) "export.el"))
           (overlay (test-mevedel-instruction-store--note source "mine"))
           (uuid (overlay-get overlay 'mevedel-uuid)))
      (with-current-buffer source (mevedel-save-instructions export))
      (test-mevedel-instruction-store--other-client
       workspace (lambda (record)
                   (test-mevedel-instruction-store--edit-record
                    record uuid 'mevedel-note-text "theirs")))
      (with-current-buffer source
        (should-error (mevedel-load-instructions export) :type 'user-error))
      (should (equal "theirs" (test-mevedel-instruction-store--stored-text
                               workspace uuid)))))
  :doc "an import over a store this Emacs never read is refused"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((export (file-name-concat (mevedel-workspace-root workspace) "export.el")))
      (test-mevedel-instruction-store--note source "stored")
      (with-current-buffer source (mevedel-save-instructions export))
      (let ((before (test-mevedel-instruction-store--record workspace))
            (fresh (test-mevedel-instruction-store--reactivate workspace)))
        ;; Activation loaded it; dropping the observation is what a forget does.
        (mevedel-instruction-store--forget fresh)
        (with-temp-buffer
          (setq-local mevedel--workspace fresh)
          (should-error (mevedel-load-instructions export) :type 'user-error))
        (should (equal before (test-mevedel-instruction-store--record workspace)))
        ;; Control: after a listing observes the store, the import proceeds.
        (test-mevedel-instruction-store--listed fresh)
        (with-temp-buffer
          (setq-local mevedel--workspace fresh)
          (mevedel-load-instructions export))
        (should (equal '("stored") (test-mevedel-instruction-store--texts fresh))))))
  :doc "an export from another installation's version loads"
  (test-mevedel-instruction-store--with-workspace (workspace source)
    (let ((export (file-name-concat (mevedel-workspace-root workspace) "export.el")))
      (test-mevedel-instruction-store--note source "elsewhere")
      (with-current-buffer source (mevedel-save-instructions export))
      (let ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer)))))
        (with-temp-file export
          (prin1 (plist-put data :version "9.9.9") (current-buffer)))
        (with-current-buffer source (mevedel-load-instructions export))
        (should (equal '("elsewhere") (test-mevedel-instruction-store--texts workspace)))
        ;; Control: a file without the shared format from another version is refused.
        (with-temp-file export
          (prin1 (plist-put (copy-sequence (mevedel-instruction-store--without
                                            data '(:format)))
                            :version "9.9.9")
                 (current-buffer)))
        (with-current-buffer source
          (should-error (mevedel-load-instructions export) :type 'user-error)))))
  :doc "an import where no .mevedel exists creates none"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-plain-import-" t)))
         (workspace (mevedel-workspace-get-or-create 'file "plain-import" root
                                                     "plain-import"))
         (file (file-name-concat root "source.el"))
         (export (file-name-concat root "export.el"))
         buffer)
    (unwind-protect
        (progn
          (write-region "(defun beta () t)\n" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (setq-local mevedel--workspace workspace)
            (mevedel-test--with-captured-diagnostics nil
              (mevedel-note-create "plain"))
            (mevedel-save-instructions export)
            (mevedel-load-instructions export))
          (should (equal '("plain") (test-mevedel-instruction-store--texts workspace)))
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (mevedel--clear-instruction-state workspace)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


;;; Rewind intents

(defmacro test-mevedel-instruction-store--with-rewind (vars &rest body)
  "Run BODY with VARS (WORKSPACE SESSION UUID) around a note with activity.
SESSION has two committed turns and sent the note, which holds a change
SESSION prepared in turn 2, written to the store."
  (declare (indent 1))
  `(test-mevedel-instruction-store--with-workspace (,(nth 0 vars) source)
     (let* ((,(nth 1 vars) (mevedel-session-create "s" ,(nth 0 vars)))
            (overlay (test-mevedel-instruction-store--note source "do it"))
            (,(nth 2 vars) (overlay-get overlay 'mevedel-uuid)))
       (test-mevedel-instruction-store--prepare
        overlay (mevedel-session-session-id ,(nth 1 vars)) 2)
       (setf (mevedel-session-turn-count ,(nth 1 vars)) 2)
       (should (eq 'written (mevedel-instruction-store-sync ,(nth 0 vars))))
       ,@body)))

(defun test-mevedel-instruction-store--prepare (note session-id turn)
  "Mark NOTE sent by SESSION-ID with a change prepared in TURN.
The overlay properties are what preparing a change leaves behind."
  (overlay-put note 'mevedel-note-state 'sent)
  (overlay-put note 'mevedel-note-session-id session-id)
  (overlay-put note 'mevedel-note-prepared-change
               (list :id (format "%s-%d" session-id turn)
                     :session-id session-id :turn turn))
  (overlay-put note 'mevedel-note-attention-state 'ready))

(mevedel-deftest mevedel-instruction-store-rewind-end (:quiet t)
  ,test
  (test)
  :doc "a committed Rewind prunes the store and the live note, keeping the note sent"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (should (plist-get (test-mevedel-instruction-store--record workspace) :rewinds))
    (setf (mevedel-session-turn-count session) 1)
    (mevedel-instruction-store-rewind-end session t)
    (should-not (test-mevedel-instruction-store--prepared-turn workspace uuid))
    (should-not (plist-get (test-mevedel-instruction-store--record workspace) :rewinds))
    ;; Only the discarded turn's activity goes: the note stays sent and linked.
    (should (eq 'sent (test-mevedel-instruction-store--stored
                       workspace uuid 'mevedel-note-state)))
    (let ((live (mevedel--instruction-with-uuid uuid workspace)))
      (should (overlay-buffer live))
      (should-not (overlay-get live 'mevedel-note-prepared-change))
      (should (equal (mevedel-session-session-id session)
                     (overlay-get live 'mevedel-note-session-id)))))
  :doc "a rolled-back Rewind drops its intent and keeps the activity"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (mevedel-instruction-store-rewind-end session nil)
    (should (equal 2 (test-mevedel-instruction-store--prepared-turn workspace uuid)))
    (should-not (plist-get (test-mevedel-instruction-store--record workspace) :rewinds)))
  :doc "another client's write during the Rewind survives its prune"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (test-mevedel-instruction-store--other-client
     workspace (lambda (data)
                 (test-mevedel-instruction-store--edit-record
                  data uuid 'mevedel-note-text "edited during Rewind")))
    (setf (mevedel-session-turn-count session) 1)
    (mevedel-instruction-store-rewind-end session t)
    (should-not (test-mevedel-instruction-store--prepared-turn workspace uuid))
    (should (equal "edited during Rewind"
                   (test-mevedel-instruction-store--stored-text workspace uuid))))
  :doc "a failed post-commit prune is owed and resolved before the next request"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (setf (mevedel-session-turn-count session) 1)
    (cl-letf (((symbol-function 'mevedel-instruction-store--write) #'ignore))
      (should-error (mevedel-instruction-store-rewind-end session t)))
    (should (equal 2 (test-mevedel-instruction-store--prepared-turn workspace uuid)))
    (should (mevedel-instruction-store-owes-p session))
    (should (mevedel-instruction-store-settle-session session))
    (should-not (test-mevedel-instruction-store--prepared-turn workspace uuid))))

(mevedel-deftest mevedel-instruction-store-settle-session (:quiet t)
  ,test
  (test)
  :doc "a crash before the commit point drops the intent at resume"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (test-mevedel-instruction-store--restart workspace)
    (should (mevedel-instruction-store-settle-session session))
    (should (equal 2 (test-mevedel-instruction-store--prepared-turn workspace uuid)))
    (should-not (plist-get (test-mevedel-instruction-store--record workspace) :rewinds)))
  :doc "a crash after the commit point prunes at resume"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (setf (mevedel-session-turn-count session) 1)
    (test-mevedel-instruction-store--restart workspace)
    (should (mevedel-instruction-store-settle-session session))
    (should-not (test-mevedel-instruction-store--prepared-turn workspace uuid)))
  :doc "an unreadable store at resume keeps a committed Rewind owed until it reads"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (let* ((path (mevedel-instruction-store-path workspace))
           bytes)
      (should (mevedel-instruction-store-rewind-begin session 1 2))
      ;; The Rewind committed; Emacs died before the store was pruned.
      (setf (mevedel-session-turn-count session) 1)
      (test-mevedel-instruction-store--restart workspace)
      (setq bytes (with-temp-buffer
                    (insert-file-contents-literally path)
                    (buffer-string)))
      (with-temp-file path (insert "(:format 1 :ids nil"))
      ;; Resume fails on the unreadable store.
      (should-error (mevedel-instruction-store-restore session))
      ;; Admission must keep refusing while the intent cannot be read.
      (should (mevedel-instruction-store-owes-p session))
      (should-error (mevedel-instruction-store-settle-session session)
                    :type 'mevedel-instruction-store-unreadable)
      (let ((coding-system-for-write 'no-conversion))
        (write-region bytes nil path nil 'silent))
      (should (mevedel-instruction-store-settle-session session))
      (should-not (test-mevedel-instruction-store--prepared-turn workspace uuid))
      (should-not (mevedel-instruction-store-owes-p session))))
  :doc "control: a session resumed without an intent owes nothing"
  (test-mevedel-instruction-store--with-rewind (workspace session _uuid)
    (test-mevedel-instruction-store--restart workspace)
    (mevedel-instruction-store-restore session)
    (should-not (mevedel-instruction-store-owes-p session)))
  :doc "another session's Rewind prunes only its own activity"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    (let* ((other (mevedel-session-create "t" workspace))
           (theirs (test-mevedel-instruction-store--note source "theirs" 2))
           (their-uuid (overlay-get theirs 'mevedel-uuid)))
      (test-mevedel-instruction-store--prepare
       theirs (mevedel-session-session-id other) 1)
      (setf (mevedel-session-turn-count other) 1)
      (mevedel-instruction-store-sync workspace)
      (should (mevedel-instruction-store-rewind-begin session 1 2))
      (should (mevedel-instruction-store-rewind-begin other 0 1))
      (setf (mevedel-session-turn-count other) 0)
      (should (mevedel-instruction-store-settle-session other))
      (should (equal 2 (test-mevedel-instruction-store--prepared-turn workspace uuid)))
      (should-not (test-mevedel-instruction-store--prepared-turn workspace their-uuid))
      (should (eq 'unsent (test-mevedel-instruction-store--stored
                           workspace their-uuid 'mevedel-note-state)))
      (should (= 1 (length (plist-get (test-mevedel-instruction-store--record workspace)
                                      :rewinds))))
      (setf (mevedel-session-turn-count session) 1)
      (should (mevedel-instruction-store-settle-session session))
      (should-not (test-mevedel-instruction-store--prepared-turn workspace uuid))))
  :doc "an intent matching neither turn count is refused"
  (test-mevedel-instruction-store--with-rewind (workspace session _uuid)
    (should (mevedel-instruction-store-rewind-begin session 1 2))
    (setf (mevedel-session-turn-count session) 5)
    (should-error (mevedel-instruction-store-settle-session session) :type 'user-error)
    (should (plist-get (test-mevedel-instruction-store--record workspace) :rewinds)))
  :doc "control: activity above the committed turn without an intent is kept"
  (test-mevedel-instruction-store--with-rewind (workspace session uuid)
    ;; A lost turn's change carries a turn the session never committed.
    (setf (mevedel-session-turn-count session) 1)
    (should-not (mevedel-instruction-store-settle-session session))
    (should (equal 2 (test-mevedel-instruction-store--prepared-turn workspace uuid)))))



;;; Over TRAMP

(mevedel-deftest mevedel-instruction-store-observe/remote (:quiet t)
  ,test
  (test)
  :doc "activation, observation, merge and Rewind over a TRAMP root"
  (mevedel-test--with-local-shell-tramp '("instruction-remote")
    (let* ((local (file-name-as-directory (make-temp-file "mevedel-remote-ws-" t)))
           (root (format "/mevedelmock:instruction-remote:%s" local))
           (programs 0)
           (run (symbol-function 'mevedel-session-control-fs-run-program))
           (counted (lambda (function)
                      (setq programs 0)
                      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                                 (lambda (&rest args) (cl-incf programs) (apply run args))))
                        (prog1 (funcall function) (setq programs programs)))))
           (note (lambda (text session)
                   (list "source.el" :original-content "(defun beta () t)\n"
                         :instructions
                         (list (list :overlay-start 1 :overlay-end 6
                                     :anchor (list :uuid "N1" :text "(defu")
                                     :properties
                                     (list 'mevedel-instruction t 'mevedel-id 1
                                           'mevedel-uuid "N1"
                                           'mevedel-instruction-type 'note
                                           'mevedel-note-text text
                                           'mevedel-note-author 'user
                                           'mevedel-note-state 'sent
                                           'mevedel-note-session-id session
                                           'mevedel-note-prepared-change
                                           (list :id "c" :session-id session :turn 2)))))))
           (record (lambda (files)
                     (list :format mevedel-instruction-store-format-version
                           :ids (list :id-counter 1 :used-ids '(1) :retired-ids nil)
                           :files files :rewinds nil)))
           workspace session)
      (unwind-protect
          (progn
            (setq workspace (mevedel-workspace-get-or-create
                             'project root root "instruction-remote"))
            (mevedel-workspace-identity-ensure root)
            (setq session (mevedel-session-create "s" workspace root))
            (let ((sid (mevedel-session-session-id session)))
              ;; Another client writes the store; activation loads it in one read.
              (should (mevedel-instruction-store--write
                       workspace nil
                       (funcall record (list (funcall note "first" sid)))))
              (mevedel-workspace-clear-registry)
              (setq workspace (mevedel-workspace-get-or-create
                               'project root root "instruction-remote"))
              (setf (mevedel-session-workspace session) workspace)
              (should (equal '("first")
                             (test-mevedel-instruction-store--texts workspace)))
              ;; An unchanged store costs one hash program.
              (should (eq 'current (funcall counted
                                            (lambda ()
                                              (mevedel-instruction-store-observe
                                               workspace)))))
              (should (= 1 programs))
              ;; A change elsewhere is merged at the next observation:
              ;; hash, read, and a write when memory differs.
              (test-mevedel-instruction-store--other-client
               workspace (lambda (data)
                           (test-mevedel-instruction-store--edit-record
                            data "N1" 'mevedel-note-text "second")))
              (funcall counted (lambda () (mevedel-instruction-store-observe workspace)))
              (should (<= programs 3))
              (should (equal '("second")
                             (test-mevedel-instruction-store--texts workspace)))
              ;; Rewind: intent, commit, prune.
              (setf (mevedel-session-turn-count session) 2)
              (should (funcall counted
                               (lambda ()
                                 (mevedel-instruction-store-rewind-begin session 1 2))))
              (should (= 2 programs))
              (setf (mevedel-session-turn-count session) 1)
              (funcall counted (lambda () (mevedel-instruction-store-rewind-end session t)))
              (should (<= programs 3))
              (should-not (test-mevedel-instruction-store--prepared-turn workspace "N1"))
              (should-not (plist-get (test-mevedel-instruction-store--record workspace)
                                     :rewinds))))
        (when workspace (mevedel--clear-instruction-state workspace))
        (test-mevedel-session-persistence--reset-instructions)
        (delete-directory local t)
        (mevedel-workspace-clear-registry)))))

(provide 'test-mevedel-persistence-instruction-store)
;;; test-mevedel-persistence-instruction-store.el ends here
