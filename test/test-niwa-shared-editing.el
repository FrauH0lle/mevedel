;;; test-niwa-shared-editing.el --- Shared editing acceptance -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared editing tests through the real host and existing public seams.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))
(require 'niwa-shared-editing)

(require 'niwa-artifact-backend)
(require 'niwa-artifact-store)

(defmacro niwa-shared-editing-test--with-workspace (&rest body)
  "Run BODY with WORKSPACE in a temp ROOT, its own runtimes and leases."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "niwa-editing-" t)))
          (workspace (niwa-workspace--create :type 'file :id "w" :root root :name "w"))
          (niwa-shared-editing--runtimes (make-hash-table :test #'equal))
          (niwa-artifact-lease--held (make-hash-table :test #'equal))
          (niwa-session-durability--client-id (make-string 64 ?a)))
     (unwind-protect (progn ,@body)
       (niwa-shared-editing-stop)
       (maphash (lambda (_directory held)
                  (when (timerp (plist-get held :timer))
                    (cancel-timer (plist-get held :timer))))
                niwa-artifact-lease--held)
       (delete-directory root t))))

(defun niwa-shared-editing-test--call (workspace args)
  "Run ARGS for WORKSPACE through the queue and return the reply."
  (let (reply)
    (niwa-shared-editing-call workspace args (lambda (value) (setq reply value)))
    (let ((deadline (+ (float-time) 10)))
      (while (and (not reply) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    reply))

(defun niwa-shared-editing-test--create (workspace kind title &rest keys)
  "Create a KIND item titled TITLE in WORKSPACE with KEYS; return the reply."
  (let (reply)
    (apply #'niwa-shared-editing-create workspace kind title "Host"
           (lambda (value) (setq reply value)) keys)
    (niwa-test--await 10 "creation settles" reply)
    reply))

(niwa-deftest niwa-shared-editing-create
  (:quiet t)
  ,test (test)
  :doc "creates a titled item under one id, attaches it and saves the session's sidecar"
  (niwa-shared-editing-test--with-workspace
    (let ((session (niwa-session--create :name "main" :workspace workspace))
          (data (generate-new-buffer " *create-data*"))
          written)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-session-persistence-write-sidecar-now)
                     (lambda (&rest args) (setq written args))))
            (let* ((reply (niwa-shared-editing-test--create
                           workspace "document" "  Notes  " :session session :buffer data))
                   (id (plist-get reply :id)))
              (should-not (plist-get reply :error))
              (should (niwa-artifact-store-id-p id))
              (should (equal (list id) (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace))))
              (should (equal '(:kind document :title "Notes")
                             (cl-subseq (niwa-artifact-store-meta workspace id) 0 4)))
              (should (equal (list id) (niwa-session-attached-artifacts session)))
              (should (equal (list session data) written))))
        (kill-buffer data))))

  :doc "refuses an invalid kind or title before queuing anything"
  (niwa-shared-editing-test--with-workspace
    (dolist (bad '(("slides" "Deck") ("whiteboard" "   ") ("document" nil)))
      (should-error (niwa-shared-editing-create
                     workspace (car bad) (cadr bad) "Host" #'ignore)
                    :type 'user-error))
    (should-error (niwa-shared-editing-create
                   workspace "document" (make-string 201 ?x) "Host" #'ignore)
                  :type 'user-error)
    (should-not (plist-get (niwa-shared-editing--runtime workspace) :queue))
    (should (equal "x" (niwa-shared-editing-check-new "document" " x "))))

  :doc "refuses a read-only session at once, and one that loses authority before commit"
  (niwa-shared-editing-test--with-workspace
    (let ((session (niwa-session--create :name "main" :workspace workspace))
          (data (generate-new-buffer " *create-data*")))
      (unwind-protect
          (progn
            (with-current-buffer data
              (setq-local niwa-session--read-only-mode t))
            (should-error (niwa-shared-editing-create
                           workspace "whiteboard" "Board" "Host" #'ignore
                           :session session :buffer data)
                          :type 'user-error)
            (should-not (plist-get (niwa-shared-editing--runtime workspace) :queue))
            (with-current-buffer data
              (setq-local niwa-session--read-only-mode nil))
            ;; Another client takes control while the create waits in the queue.
            (let (reply)
              (niwa-shared-editing-create
               workspace "whiteboard" "Board" "Host" (lambda (value) (setq reply value))
               :session session :buffer data)
              (with-current-buffer data
                (setq-local niwa-session--read-only-mode t))
              (niwa-test--await 10 "creation settles" reply)
              (should (plist-get reply :error))
              (should-not (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace)))
              (should-not (niwa-session-attached-artifacts session))))
        (kill-buffer data))))

  :doc "a caller's ended authority creates nothing"
  (niwa-shared-editing-test--with-workspace
    (should (plist-get (niwa-shared-editing-test--create
                        workspace "whiteboard" "Board" :authorize (lambda () nil))
                       :error))
    (should-not (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace))))

  :doc "keeps a created item and reports a failed attachment instead of failing"
  (niwa-shared-editing-test--with-workspace
    (let ((session (niwa-session--create :name "main" :workspace workspace)))
      (cl-letf (((symbol-function 'niwa-artifact-store-attach)
                 (lambda (&rest _) (error "Disk full"))))
        (let ((reply (niwa-shared-editing-test--create
                      workspace "whiteboard" "Board" :session session)))
          (should-not (plist-get reply :error))
          (should (equal "Disk full" (plist-get reply :attach-error)))
          (should (equal (list (plist-get reply :id))
                         (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace)))))))))

(niwa-deftest niwa-shared-editing--send
  (:doc "Large helper requests use small UTF-8 writes without changing JSON framing")
  (let* ((args (list :title (concat (make-string 1020 ?x) "λ 🌱")
                     :data (make-string 10000 ?é)))
         (expected (concat (niwa-shared-editing--json args) "\n"))
         chunks)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_process chunk)
                 (should (<= (string-bytes (encode-coding-string chunk 'utf-8-unix)) 4096))
                 (push chunk chunks))))
      (niwa-shared-editing--send nil args))
    (should (> (length chunks) 1))
    (should (equal expected (apply #'concat (nreverse chunks))))))

(niwa-deftest niwa-shared-editing--process
  (:doc "Helper replies frame fragmented Unicode, multiple lines, and bound unfinished bytes")
  (let ((runtime (list :active (list :requestId 1 :callback #'ignore)))
        replies)
    (unwind-protect
        (progn
          (let* ((process (niwa-shared-editing--process runtime))
                 (filter (process-filter process))
                 (line "{\"requestId\":1,\"result\":{\"title\":\"λ 🌱\"}}\n"))
            (cl-letf (((symbol-function 'niwa-shared-editing--accept)
                       (lambda (_runtime _job reply) (push reply replies))))
              (funcall filter process (substring line 0 12))
              (should-not replies)
              (niwa-transport-call-as-remote-operation
               (lambda ()
                 (let ((timer-list nil))
                   (funcall filter process (concat (substring line 12) line)))))
              (sleep-for 0.01)
              (should (= (length replies) 2))
              (should (equal (plist-get (plist-get (car replies) :result) :title) "λ 🌱"))
              (should-not (process-get process :partial))
              (should (= (process-get process :partial-bytes) 0)))
            ;; A UTF-8 fragment can exceed the byte limit before any newline.
            (process-put process :partial-bytes (1- (* 64 1024 1024)))
            (funcall filter process "λ")
            (should-not (process-live-p process))))
      (when-let* ((process (plist-get runtime :process)))
        (delete-process process)))))

(niwa-deftest niwa-shared-editing-call
  ()
  ,test (test)
  :doc "Commits a board into the store under its lease and reopens it"
  (niwa-shared-editing-test--with-workspace
    (let ((reply (niwa-shared-editing-test--call
                  workspace '(:action "create" :id "board1" :kind "whiteboard"
                              :title "Architecture" :actor "Alice" :opId "one"))))
      (should-not (plist-get reply :error))
      (should (= 1 (plist-get (plist-get reply :result) :revision)))
      (should (file-exists-p (file-name-concat root ".niwa/artifacts/.state/board1/state.json")))
      (should (equal '(:kind whiteboard :title "Architecture" :file "state.json")
                     (cl-subseq (niwa-artifact-store-meta workspace "board1") 0 6)))
      (should (niwa-artifact-lease-held workspace "board1"))
      (should (equal '("board1") (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace))))
      (should (equal "Architecture"
                     (plist-get (car (niwa-shared-editing-list workspace)) :title)))
      (niwa-shared-editing-stop)
      (should (equal "Architecture"
                     (plist-get (plist-get (niwa-shared-editing-test--call
                                            workspace '(:action "read" :id "board1"))
                                           :result)
                                :title)))
      ;; A rename reaches the store's metadata, and the item's open
      ;; conversation follows it.
      (let (named)
        (cl-letf (((symbol-function 'niwa-artifact-store-name-conversation)
                   (lambda (named-workspace id) (push (cons named-workspace id) named))))
          (should-not (plist-get (niwa-shared-editing-test--call
                                  workspace '(:action "rename" :id "board1" :title "Later"
                                              :actor "Alice" :opId "two"))
                                 :error)))
        (should (equal (list (cons workspace "board1")) named)))
      (should (equal "Later" (plist-get (niwa-artifact-store-meta workspace "board1")
                                        :title)))))

  :doc "uncommitted batched requests still count toward pending request limits"
  (niwa-shared-editing-test--with-workspace
    (let ((runtime (niwa-shared-editing--runtime workspace)))
      (plist-put runtime :batch
                 (list (list (list :bytes (* 32 1024 1024) :callback #'ignore))))
      (should-error (niwa-shared-editing-call
                     workspace '(:action "read" :id "board") #'ignore))
      (plist-put runtime :batch (make-list 16 (list (list :callback #'ignore))))
      (plist-put runtime :queue (make-list 48 (list :callback #'ignore)))
      (should-error (niwa-shared-editing-call
                     workspace '(:action "read" :id "board") #'ignore))))

  :doc "A request arriving inside TRAMP survives its temporary timer list"
  (niwa-shared-editing-test--with-workspace
    (let (reply)
      (niwa-transport-call-as-remote-operation
       (lambda ()
         (let ((timer-list nil))
           (niwa-shared-editing-call
            workspace '(:action "list") (lambda (value) (setq reply value))))))
      ;; Survival across the temporary timer list is independent of helper
      ;; startup and transport scheduling latency.
      (niwa-test--await 10 "deferred shared-editing reply" reply)
      (should (equal reply '(:result [])))))

  :doc "Exporting a historical document previews its saved content without a lease or mutation"
  (niwa-shared-editing-test--with-workspace
    (should-not (plist-get
                 (niwa-shared-editing-test--call
                  workspace '(:action "import" :id "doc" :format "markdown" :data "# First"
                              :actor "Alice" :opId "one")) :error))
    (niwa-shared-editing-save-version workspace "doc")
    (should-not (plist-get
                 (niwa-shared-editing-test--call
                  workspace '(:action "rename" :id "doc" :title "Later"
                              :actor "Alice" :opId "two")) :error))
    (niwa-artifact-lease-release workspace "doc")
    (let* ((before (niwa-shared-editing--read workspace "doc"))
           (reply (niwa-shared-editing-test--call
                   workspace '(:action "export" :id "doc" :version 1 :format "native")))
           (exported (niwa-shared-editing--parse (plist-get (plist-get reply :result) :text))))
      (should-not (plist-get reply :error))
      (should (equal "Imported document" (plist-get exported :title)))
      (should (equal before (niwa-shared-editing--read workspace "doc")))
      ;; A preview takes no lease.
      (should-not (niwa-artifact-lease-held workspace "doc"))
      (should (plist-get (niwa-shared-editing-test--call
                         workspace '(:action "export" :id "doc" :version 0 :format "html"))
                        :error))))

  :doc "Store observers see the committed state and title once per mutation"
  (niwa-shared-editing-test--with-workspace
    (let* (observed
           (niwa-artifact-store-changed-functions
            (list (lambda (changed)
                    (should (eq workspace changed))
                    (push (list (plist-get (niwa-artifact-store-meta workspace "board") :title)
                                (plist-get (niwa-shared-editing--read workspace "board") :title))
                          observed)))))
      (niwa-shared-editing-test--call
       workspace '(:action "create" :id "board" :kind "whiteboard"
                   :title "First" :actor "Alice" :opId "one"))
      (niwa-shared-editing-test--call
       workspace '(:action "rename" :id "board" :title "Later"
                   :actor "Alice" :opId "two"))
      (should (equal '(("Later" "Later") ("First" "First")) observed))))

  :doc "Content-only commits reach store observers together, after a delay"
  (niwa-shared-editing-test--with-workspace
    (let* ((observed 0)
           (niwa-artifact-store--content-changes (make-hash-table :test #'equal))
           (niwa-artifact-store-changed-functions
            (list (lambda (_changed) (cl-incf observed)))))
      (niwa-shared-editing-test--call
       workspace '(:action "create" :id "board" :kind "whiteboard"
                   :title "First" :actor "Alice" :opId "one"))
      (should (= 1 observed))
      (let ((state (niwa-shared-editing--read workspace "board"))
            timer)
        (dotimes (step 2)
          (niwa-shared-editing--commit
           workspace (plist-put (copy-sequence state) :revision (+ 2 step))))
        (should (= 1 observed))
        ;; The metadata itself is current at once.
        (should (= 3 (plist-get (niwa-artifact-store-meta workspace "board") :revision)))
        (setq timer (gethash (niwa-workspace-root workspace)
                             niwa-artifact-store--content-changes))
        (should (timerp timer))
        (cancel-timer timer)
        (apply (timer--function timer) (timer--args timer))
        (should (= 2 observed))
        (should (= 0 (hash-table-count niwa-artifact-store--content-changes)))
        ;; A title change announces at once and carries pending content.
        (niwa-shared-editing--commit
         workspace (plist-put (copy-sequence state) :revision 4))
        (setq timer (gethash (niwa-workspace-root workspace)
                             niwa-artifact-store--content-changes))
        (niwa-shared-editing--commit
         workspace (plist-put (plist-put (copy-sequence state) :revision 5) :title "New"))
        (should (= 3 observed))
        (should-not (memq timer timer-list))
        ;; A held timer that fires anyway announces nothing twice.
        (apply (timer--function timer) (timer--args timer))
        (should (= 3 observed)))))

  :doc "Another Emacs's item is read-only here"
  (niwa-shared-editing-test--with-workspace
    (should-not (plist-get (niwa-shared-editing-test--call
                            workspace '(:action "create" :id "board1" :kind "whiteboard"
                                        :title "Plan" :actor "Alice" :opId "one"))
                           :error))
    (let ((niwa-session-durability--client-id (make-string 64 ?b))
          (niwa-artifact-lease--held (make-hash-table :test #'equal)))
      (should (string-match-p "being edited in Emacs"
                              (plist-get (niwa-shared-editing-test--call
                                          workspace '(:action "rename" :id "board1"
                                                      :title "Theirs" :actor "Bob"
                                                      :opId "two"))
                                         :error)))
      ;; Reading needs no lease.
      (should (equal "Plan" (plist-get (plist-get (niwa-shared-editing-test--call
                                                   workspace '(:action "read" :id "board1"))
                                                  :result)
                                       :title)))))

  :doc "Availability is optional, read-only, and recovers after runtime and resource repair"
  (niwa-shared-editing-test--with-workspace
    (let* ((directory (make-temp-file "niwa-editing-status-" t))
           (resources niwa-shared-editing--directory)
           (node niwa-shared-editing-node-program))
      (unwind-protect
          (cl-labels ((call (action)
                        (let ((reply (niwa-shared-editing-test--call
                                      workspace (list :action action))))
                          (should reply)
                          reply)))
            (let ((niwa-shared-editing-node-program
                   (file-name-concat directory "missing-node")))
              (should (string-match-p "Install Node" (plist-get (call "status") :error)))
              (should (equal [] (plist-get (call "list") :result))))
            (should (eq t (plist-get (plist-get (call "status") :result) :available)))
            (let ((niwa-shared-editing--directory directory))
              (should (string-match-p "resources" (plist-get (call "status") :error))))
            (copy-file (file-name-concat resources "host.bundle.mjs")
                       (file-name-concat directory "host.bundle.mjs"))
            (let ((niwa-shared-editing--directory directory))
              (should (string-match-p "resources" (plist-get (call "status") :error)))
              (dolist (file '("resvg.wasm" "font.ttf" "Excalifont.ttf" "Nunito.ttf" "ComicShanns.ttf"))
                (copy-file (file-name-concat resources file)
                           (file-name-concat directory file)))
              (should (eq t (plist-get (plist-get (call "status") :result) :available))))
            ;; A configured runtime change also invalidates a live helper.
            (let ((niwa-shared-editing-node-program
                   (file-name-concat directory "missing-node")))
              (should (plist-get (call "status") :error)))
            (let ((niwa-shared-editing-node-program node))
              (should (eq t (plist-get (plist-get (call "status") :result) :available))))
            (should-not (file-exists-p (niwa-artifact-store-directory workspace)))
            (should (equal (sort (directory-files directory nil "^[^.]") #'string<)
                           '("ComicShanns.ttf" "Excalifont.ttf" "Nunito.ttf" "font.ttf" "host.bundle.mjs" "resvg.wasm"))))
        (delete-directory directory t)))))

(niwa-deftest niwa-shared-editing--drain
  ()
  ,test (test)
  :doc "The queue never asks to take an item over; a refusal settles the job"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "Plan"
                 :actor "Alice" :opId "one"))
    (cl-letf (((symbol-function 'niwa-artifact-lease-ensure)
               (lambda (_workspace _id &optional ask)
                 (should-not ask)
                 (user-error "This needs a decision in Emacs on the host first"))))
      (should (string-match-p "decision in Emacs"
                              (plist-get (niwa-shared-editing-test--call
                                          workspace '(:action "rename" :id "board" :title "X"
                                                      :actor "Guest" :opId "a"))
                                         :error))))
    ;; A quit during target I/O settles the job and leaves the queue working.
    (cl-letf (((symbol-function 'niwa-artifact-lease-ensure)
               (lambda (&rest _) (signal 'quit nil))))
      (should (equal "Editing operation cancelled"
                     (plist-get (niwa-shared-editing-test--call
                                 workspace '(:action "rename" :id "board" :title "X"
                                             :actor "Alice" :opId "b"))
                                :error))))
    (should-not (plist-get (niwa-shared-editing--runtime workspace) :active))
    (should (= 1 (length (plist-get (niwa-shared-editing-test--call
                                     workspace '(:action "list"))
                                    :result)))))

  :doc "Stopping the runtime during lease I/O settles the job once and sends nothing"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "Plan"
                 :actor "Alice" :opId "one"))
    (let ((runtime (niwa-shared-editing--runtime workspace))
          (replies nil))
      (cl-letf (((symbol-function 'niwa-artifact-lease-ensure)
                 (lambda (&rest _) (niwa-shared-editing-stop runtime "Helper exited") t)))
        (niwa-shared-editing-call
         workspace '(:action "rename" :id "board" :title "Lost" :actor "Alice" :opId "two")
         (lambda (reply) (push reply replies)))
        (niwa-test--await 10 "runtime stop settles" replies))
      (should (equal '((:error "Helper exited")) replies))
      (should-not (plist-get runtime :active))
      (should-not (plist-get runtime :queue))
      (should-not (process-live-p (plist-get runtime :process)))
      (should (equal "Plan" (plist-get (niwa-shared-editing--read workspace "board")
                                       :title)))))

  :doc "Editing a deleted item fails without leasing it again"
  (niwa-shared-editing-test--with-workspace
    (should (equal "This item no longer exists"
                   (plist-get (niwa-shared-editing-test--call
                               workspace '(:action "rename" :id "ghost" :title "X"
                                           :actor "Guest" :opId "a"))
                              :error)))
    (should-not (file-exists-p (niwa-artifact-lease-directory workspace "ghost"))))

  :doc "An interrupted create, its metadata written but not its state, retries"
  (niwa-shared-editing-test--with-workspace
    (make-directory (niwa-artifact-store-artifact-directory workspace "board") t)
    (niwa-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (should-not (plist-get (niwa-shared-editing-test--call
                            workspace '(:action "create" :id "board" :kind "whiteboard"
                                        :title "Plan" :actor "Alice" :opId "one"))
                           :error))
    (should (niwa-shared-editing-present-p workspace "board"))
    ;; Any authored directory or orphaned bookkeeping directory is taken.
    (make-directory (niwa-artifact-store-artifact-directory workspace "page") t)
    (make-directory (niwa-artifact-store-bookkeeping-directory workspace "orphan") t)
    (should (string-match-p "already exists"
                            (plist-get (niwa-shared-editing-test--call
                                        workspace '(:action "create" :id "orphan" :kind "document"
                                                    :title "P" :actor "Alice" :opId "three"))
                                       :error)))
    (should (string-match-p "already exists"
                            (plist-get (niwa-shared-editing-test--call
                                        workspace '(:action "create" :id "page" :kind "document"
                                                    :title "P" :actor "Alice" :opId "two"))
                                       :error)))))

(niwa-deftest niwa-shared-editing-stop
  ()
  ,test (test)
  :doc "Stopping retires jobs before callbacks and ignores late replies"
  (niwa-shared-editing-test--with-workspace
    (let* ((runtime (niwa-shared-editing--runtime workspace))
           (calls 0)
           (job (list :args '(:id "board")
                      :callback (lambda (reply)
                                  (should (plist-get reply :error))
                                  (cl-incf calls)
                                  (when (= calls 1)
                                    (niwa-shared-editing-stop runtime))))))
      (plist-put runtime :active job)
      (niwa-shared-editing-stop runtime)
      (should (= calls 1))
      (should-not (plist-get runtime :active))
      ;; This reply was queued before timeout or explicit stop settled JOB.
      (niwa-shared-editing--accept
       runtime job '(:state (:id "board" :kind "whiteboard" :title "Late")))
      (should-not (file-exists-p (niwa-artifact-store-directory workspace)))
      (should (= calls 1))))

  :doc "Stopping during a commit settles it once and then stops the helper"
  (niwa-shared-editing-test--with-workspace
    (let* ((calls 0) reply runtime
           (niwa-artifact-backend-change-hook
            (list (lambda (&rest _) (niwa-shared-editing-stop runtime)))))
      (setq runtime (niwa-shared-editing--runtime workspace))
      (niwa-shared-editing-call
       workspace '(:action "create" :id "committed" :kind "whiteboard"
                   :title "Committed" :actor "Alice" :opId "one")
       (lambda (result) (cl-incf calls) (setq reply result)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (not reply) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (= calls 1))
      (should-not (plist-get reply :error))
      (should-not (process-live-p (plist-get runtime :process)))
      (should-not (niwa-shared-editing--live-p runtime))
      (should (= 1 (plist-get (niwa-shared-editing--read workspace "committed")
                              :revision))))))

(niwa-deftest niwa-shared-editing-present-p
  (:doc "Finds committed whiteboards and documents, skipping unreadable metadata")
  (niwa-shared-editing-test--with-workspace
    (dolist (spec '(("board" whiteboard "state.json") ("page" html "index.html")))
      (make-directory (niwa-artifact-store-artifact-directory workspace (car spec)) t)
      (niwa-artifact-store-create-meta workspace (car spec) (nth 2 spec) (cadr spec) "Plan")
      (write-region "not json" nil (niwa-artifact-store-primary-path workspace (car spec))
                    nil 'silent))
    ;; A committed item's metadata records its revision.
    (niwa-artifact-store-update-meta workspace "board" :revision 1)
    (should (equal '((:id "board" :kind "whiteboard" :title "Plan" :revision 1))
                   (niwa-shared-editing-list workspace)))
    ;; Unreadable metadata leaves its artifact out, not the listing.
    (make-directory (niwa-artifact-store-bookkeeping-directory workspace "bad") t)
    (write-region "(:kind" nil (file-name-concat
                                (niwa-artifact-store-bookkeeping-directory workspace "bad")
                                "meta.el")
                  nil 'silent)
    (should (equal '("board") (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace))))
    (should (niwa-shared-editing-present-p workspace "board"))
    (should-not (niwa-shared-editing-present-p workspace "page"))))

(niwa-deftest niwa-shared-editing--parse
  (:doc "Preserves empty mark attributes, arrays, nulls and false through exact patch reads")
  (let* ((json "{\"before\":null,\"marks\":[{\"type\":\"bold\",\"attrs\":{}}],\"empty\":[],\"flag\":false}")
         (value (niwa-shared-editing--parse json)))
    (should (null (plist-get value :before)))
    (should (hash-table-p (plist-get (aref (plist-get value :marks) 0) :attrs)))
    (should (equal json (niwa-shared-editing--json value)))))

(niwa-deftest niwa-shared-editing--json
  (:doc "Unicode JSON remains text when nested in model or browser messages")
  (let* ((value (list :title (string #x2014 #x03bb #x1f331) :empty nil :flag :json-false))
         (text (niwa-shared-editing--json value))
         (envelope (json-serialize (list :text text))))
    (should (multibyte-string-p text))
    (should (equal (plist-get (json-parse-string envelope :object-type 'plist) :text) text))
    (should (equal value (niwa-shared-editing--parse text)))))

(niwa-deftest niwa-shared-editing--delete
  (:doc "Deleting through the editing queue removes the artifact and names who deleted it")
  (niwa-shared-editing-test--with-workspace
    (let (observed)
      (niwa-shared-editing-test--call
       workspace '(:action "create" :id "board" :kind "whiteboard" :title "Board"
                   :actor "Alice" :opId "one"))
      (let ((niwa-artifact-backend-change-hook
             (list (lambda (_workspace state _result) (push state observed)))))
        (should (equal '(:id "board" :deleted t)
                       (plist-get (niwa-shared-editing-test--call
                                   workspace '(:action "delete" :id "board" :actor "Guest: Ann"))
                                  :result)))
        (should-not (file-exists-p (niwa-artifact-store-artifact-directory workspace "board")))
        (should-not (file-exists-p (niwa-artifact-lease-directory workspace "board")))
        (should (equal '(:id "board" :deleted t :actor "Guest: Ann") (car observed)))
        (should-not (niwa-shared-editing-list workspace))
        (should (equal "This item no longer exists"
                       (plist-get (niwa-shared-editing-test--call
                                   workspace '(:action "delete" :id "board"))
                                  :error)))))))

(niwa-deftest niwa-shared-editing--commit
  (:doc "A concurrent metadata edit prevents stale rename metadata from overwriting it")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let ((run (symbol-function 'niwa-artifact-lease-run))
          (rename '(:action "rename" :id "board" :title "Later" :actor "Alice" :opId "two"))
          (edits 0))
      ;; Metadata changing before every write: the retry from a fresh read
      ;; fails too, and nothing is written.
      (cl-letf (((symbol-function 'niwa-artifact-lease-run)
                 (lambda (owner id operations)
                   (niwa-artifact-store-update-meta
                    owner id :dedicated-session (format "conversation-%d" (cl-incf edits)))
                   (funcall run owner id operations))))
        (should (plist-get (niwa-shared-editing-test--call workspace rename) :error)))
      (should (= 2 edits))
      (should (equal "First" (plist-get (niwa-shared-editing--read workspace "board") :title)))
      ;; Metadata changed once since this Emacs committed: the commit rereads
      ;; it and keeps the concurrent change.
      (cl-letf (((symbol-function 'niwa-artifact-lease-run)
                 (lambda (owner id operations)
                   (when (= 2 edits)
                     (niwa-artifact-store-update-meta
                      owner id :dedicated-session (format "conversation-%d" (cl-incf edits))))
                   (funcall run owner id operations))))
        (should-not (plist-get (niwa-shared-editing-test--call workspace rename) :error)))
      (should (equal "conversation-3"
                     (plist-get (niwa-artifact-store-meta workspace "board") :dedicated-session)))
      (should (equal "Later" (plist-get (niwa-artifact-store-meta workspace "board") :title))))))

(niwa-deftest niwa-shared-editing--committed
  (:doc "An edit under the lease of the last commit reads nothing; a new holding reads the disk")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let ((programs 0)
          (run (symbol-function 'niwa-session-control-fs-run-program)))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (&rest args) (cl-incf programs) (apply run args))))
        (should-not (plist-get (niwa-shared-editing-test--call
                                workspace '(:action "rename" :id "board" :title "Second"
                                            :actor "Alice" :opId "two"))
                               :error))
        ;; The fenced write only.
        (should (= 1 programs))
        ;; Another Emacs changes the item after this one lost the lease.
        (niwa-artifact-lease--forget (niwa-artifact-lease-directory workspace "board"))
        (delete-file (niwa-shared-editing--state-path workspace "board"))
        (should (string-match-p "no longer exists"
                                (plist-get (niwa-shared-editing-test--call
                                            workspace '(:action "read" :id "board"))
                                           :error)))))))

(niwa-deftest niwa-shared-editing--committed-expired
  (:doc "reads stop using cached content once the holding has expired")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let* ((directory (niwa-artifact-lease-directory workspace "board"))
           (held (gethash directory niwa-artifact-lease--held)))
      (plist-put held :renewed 0)
      ;; Another host takes over while this one's heartbeat has not run.
      (cl-letf (((symbol-function 'niwa-artifact-lease--now)
                 (lambda (_directory) (+ (floor (float-time)) 1000))))
        (let ((niwa-artifact-lease--held (make-hash-table :test #'equal))
              (niwa-shared-editing--runtimes (make-hash-table :test #'equal))
              (niwa-session-durability--client-id (make-string 64 ?b)))
          (unwind-protect
              (progn
                (niwa-test--with-captured-messages nil
                  (niwa-artifact-lease-acquire workspace "board"))
                (should-not
                 (plist-get (niwa-shared-editing-test--call
                             workspace '(:action "rename" :id "board" :title "Foreign"
                                         :actor "Bob" :opId "foreign")) :error)))
            (niwa-shared-editing-stop)
            (niwa-artifact-lease-release-all)))
        (let ((reply (niwa-shared-editing-test--call
                      workspace '(:action "read" :id "board"))))
          (should-not (plist-get reply :error))
          (should (equal "Foreign" (plist-get (plist-get reply :result) :title))))))))

(niwa-deftest niwa-shared-editing--defer ()
  ,test
  (test)
  :doc "queued edits of one item commit once and are announced in order"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let ((commits 0) (commit (symbol-function 'niwa-shared-editing--commit))
          announced replies)
      (cl-letf (((symbol-function 'niwa-shared-editing--commit)
                 (lambda (&rest args) (cl-incf commits) (apply commit args))))
        (let ((niwa-artifact-backend-change-hook
               (list (lambda (_workspace state _result)
                       (push (list (plist-get state :title) commits) announced)))))
          (dolist (title '("A" "B" "C"))
            (niwa-shared-editing-call
             workspace (list :action "rename" :id "board" :title title
                             :actor "Alice" :opId title)
             (lambda (reply) (push reply replies))))
          (niwa-test--await 10 "edits settle" (= 3 (length replies)))))
      (should (= 1 commits))
      (should-not (cl-some (lambda (reply) (plist-get reply :error)) replies))
      ;; Announced after the commit, each with its own state.
      (should (equal '(("A" 1) ("B" 1) ("C" 1)) (reverse announced)))
      (should (equal "C" (plist-get (niwa-shared-editing--read workspace "board") :title)))
      (should (equal "C" (plist-get (niwa-artifact-store-meta workspace "board") :title)))))

  :doc "a continuing edit that fails leaves the batch committed on its own"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let (replies)
      (niwa-shared-editing-call
       workspace '(:action "rename" :id "board" :title "Kept" :actor "Alice" :opId "a")
       (lambda (reply) (push (cons "a" reply) replies)))
      (niwa-shared-editing-call
       workspace '(:action "rename" :id "board" :title "Refused" :actor "Alice" :opId "b")
       (lambda (reply) (push (cons "b" reply) replies))
       (lambda () (equal "Refused" "never")))
      (niwa-test--await 10 "edits settle" (= 2 (length replies)))
      (should-not (plist-get (cdr (assoc "a" replies)) :error))
      (should (plist-get (cdr (assoc "b" replies)) :error))
      (should (equal "Kept" (plist-get (niwa-shared-editing--read workspace "board") :title)))))

  :doc "stopping with a batch pending reports its edits unsaved"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let ((runtime (niwa-shared-editing--runtime workspace)) replies)
      (cl-letf (((symbol-function 'niwa-shared-editing--drain)
                 (let ((drain (symbol-function 'niwa-shared-editing--drain)))
                   (lambda (runtime)
                     ;; Stop as soon as an edit waits in a batch.
                     (if (plist-get runtime :batch)
                         (niwa-shared-editing-stop runtime)
                       (funcall drain runtime))))))
        (dolist (title '("A" "B"))
          (niwa-shared-editing-call
           workspace (list :action "rename" :id "board" :title title
                           :actor "Alice" :opId title)
           (lambda (reply) (push reply replies))))
        (niwa-test--await 10 "edits settle" (= 2 (length replies))))
      (should (cl-every (lambda (reply) (plist-get reply :error)) replies))
      (should-not (plist-get runtime :batch))
      (should (equal "First" (plist-get (niwa-shared-editing--read workspace "board") :title)))))

  :doc "a batch whose lease was lost and claimed again is not committed"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let (replies lost)
      (cl-letf (((symbol-function 'niwa-shared-editing--drain)
                 (let ((drain (symbol-function 'niwa-shared-editing--drain)))
                   (lambda (runtime)
                     ;; Another Emacs may have edited the item meanwhile.
                     (when (and (plist-get runtime :batch) (not lost))
                       (setq lost t)
                       (niwa-artifact-lease--forget
                        (niwa-artifact-lease-directory workspace "board")))
                     (funcall drain runtime)))))
        (dolist (title '("A" "B"))
          (niwa-shared-editing-call
           workspace (list :action "rename" :id "board" :title title
                           :actor "Alice" :opId title)
           (lambda (reply) (push reply replies))))
        (niwa-test--await 10 "edits settle" (= 2 (length replies))))
      (should lost)
      (should (cl-every (lambda (reply) (plist-get reply :error)) replies))
      (should (equal "First" (plist-get (niwa-shared-editing--read workspace "board") :title)))))

  :doc "cancelling a batched edit leaves the item unchanged"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let (replies cancel)
      (cl-letf (((symbol-function 'niwa-shared-editing--drain)
                 (let ((drain (symbol-function 'niwa-shared-editing--drain)))
                   (lambda (runtime)
                     (when (plist-get runtime :batch) (funcall cancel))
                     (funcall drain runtime)))))
        (setq cancel (niwa-shared-editing-call
                      workspace '(:action "rename" :id "board" :title "A"
                                  :actor "Alice" :opId "A")
                      (lambda (reply) (push reply replies))))
        (niwa-shared-editing-call
         workspace '(:action "rename" :id "board" :title "B" :actor "Alice" :opId "B")
         (lambda (reply) (push reply replies)))
        (niwa-test--await 10 "edits settle" (= 2 (length replies))))
      (should (cl-every (lambda (reply) (plist-get reply :error)) replies))
      (should (equal "First" (plist-get (niwa-shared-editing--read workspace "board") :title)))))

  :doc "a stop requested while a batch commits on its own takes effect after it"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let ((runtime (niwa-shared-editing--runtime workspace))
          (commit (symbol-function 'niwa-shared-editing--commit))
          replies)
      (cl-letf (((symbol-function 'niwa-shared-editing--commit)
                 (lambda (&rest args)
                   ;; The edit's timeout or the helper's exit, during target I/O.
                   (niwa-shared-editing-stop runtime)
                   (apply commit args))))
        (niwa-shared-editing-call
         workspace '(:action "rename" :id "board" :title "Kept" :actor "Alice" :opId "a")
         (lambda (reply) (push (cons "a" reply) replies)))
        (niwa-shared-editing-call
         workspace '(:action "rename" :id "board" :title "Refused" :actor "Alice" :opId "b")
         (lambda (reply) (push (cons "b" reply) replies))
         (lambda () (equal "Refused" "never")))
        (niwa-test--await 10 "edits settle" (= 2 (length replies))))
      (should-not (plist-get (cdr (assoc "a" replies)) :error))
      (should (plist-get (cdr (assoc "b" replies)) :error))
      (should-not (niwa-shared-editing--live-p runtime))
      (should (equal "Kept" (plist-get (niwa-shared-editing--read workspace "board") :title))))))

(niwa-deftest niwa-shared-editing--finish
  (:doc "waits for a busy store before committing a pending batch")
  (niwa-shared-editing-test--with-workspace
    (let* ((runtime (niwa-shared-editing--runtime workspace))
           (job (list :callback #'ignore))
           (busy t)
           flushed)
      (plist-put runtime :active job)
      (plist-put runtime :batch (list (cons (list :callback #'ignore) nil)))
      (cl-letf (((symbol-function 'niwa-shared-editing--store-busy-p)
                 (lambda (_runtime) busy))
                ((symbol-function 'niwa-shared-editing--flush-batch)
                 (lambda (runtime)
                   (setq flushed t)
                   (plist-put runtime :batch nil))))
        (niwa-shared-editing--finish runtime job '(:result t))
        (should-not flushed)
        (should (eq job (plist-get runtime :active)))
        (setq busy nil)
        (niwa-test--await 2 "finish retries" flushed)
        (should-not (plist-get runtime :active))))))

(niwa-deftest niwa-shared-editing--validate-batch ()
  ,test
  (test)
  :doc "a deferred edit still requires authority and can be cancelled before commit"
  (dolist (withdraw '(cancel revoke))
    (niwa-shared-editing-test--with-workspace
      (niwa-shared-editing-test--call
       workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                   :actor "Alice" :opId "one"))
      (let ((accept (symbol-function 'niwa-shared-editing--accept))
            (authorized t) cancel replies)
        (cl-letf (((symbol-function 'niwa-shared-editing--accept)
                   (lambda (runtime job reply)
                     (when (plist-get runtime :batch)
                       (if (eq withdraw 'cancel) (funcall cancel)
                         (setq authorized nil)))
                     (funcall accept runtime job reply))))
          (setq cancel
                (niwa-shared-editing-call
                 workspace '(:action "rename" :id "board" :title "Withdrawn"
                             :actor "Alice" :opId "a")
                 (lambda (reply) (push reply replies)) (lambda () authorized)))
          (niwa-shared-editing-call
           workspace '(:action "background" :id "board" :background "#ffffff"
                       :actor "Bob" :opId "b")
           (lambda (reply) (push reply replies)))
          (niwa-test--await 10 "edits settle" (= 2 (length replies))))
        (should (cl-every (lambda (reply) (plist-get reply :error)) replies))
        (should (equal "First" (plist-get (niwa-shared-editing--read workspace "board") :title))))))

  :doc "a batch from a previous lease holding cannot overwrite a later holder's edit"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let ((drain (symbol-function 'niwa-shared-editing--drain)) changed replies)
      (cl-letf (((symbol-function 'niwa-shared-editing--drain)
                 (lambda (runtime)
                   (when (and (plist-get runtime :batch) (not changed))
                     (setq changed t)
                     (niwa-artifact-lease-release workspace "board")
                     (niwa-artifact-lease-acquire workspace "board")
                     (let ((state (niwa-shared-editing--read workspace "board")))
                       (setq state (plist-put state :title "Foreign"))
                       (setq state (plist-put state :revision 2))
                       (niwa-shared-editing--commit workspace state)))
                   (funcall drain runtime))))
        (dolist (title '("A" "B"))
          (niwa-shared-editing-call
           workspace (list :action "rename" :id "board" :title title
                           :actor "Alice" :opId title)
           (lambda (reply) (push reply replies))))
        (niwa-test--await 10 "edits settle" (= 2 (length replies))))
      (should (cl-every (lambda (reply) (plist-get reply :error)) replies))
      (should (equal "Foreign" (plist-get (niwa-shared-editing--read workspace "board") :title))))))

(niwa-deftest niwa-shared-editing--flush-batch
  (:doc "a stop requested during fallback commit settles every callback once and stops")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (let ((runtime (niwa-shared-editing--runtime workspace))
          (commit (symbol-function 'niwa-shared-editing--commit)) replies)
      (cl-letf (((symbol-function 'niwa-shared-editing--commit)
                 (lambda (&rest args)
                   (niwa-shared-editing-stop runtime)
                   (apply commit args))))
        (niwa-shared-editing-call
         workspace '(:action "rename" :id "board" :title "Kept" :actor "Alice" :opId "a")
         (lambda (reply) (push (cons "a" reply) replies)))
        (niwa-shared-editing-call
         workspace '(:action "rename" :id "board" :title "Refused" :actor "Alice" :opId "b")
         (lambda (reply) (push (cons "b" reply) replies)) (lambda () nil))
        (niwa-shared-editing-call
         workspace '(:action "read" :id "board")
         (lambda (reply) (push (cons "c" reply) replies)))
        (niwa-test--await 10 "edits settle" (= 3 (length replies))))
      (should-not (plist-get (cdr (assoc "a" replies)) :error))
      (should (plist-get (cdr (assoc "b" replies)) :error))
      (should (plist-get (cdr (assoc "c" replies)) :error))
      (should-not (niwa-shared-editing--live-p runtime))
      (should (equal "Kept" (plist-get (niwa-shared-editing--read workspace "board") :title))))))

(niwa-deftest niwa-shared-editing--commit-directory
  (:doc "an authored directory replaced by a file cannot partially commit a new state")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard"
                 :title "First" :actor "Alice" :opId "one"))
    (let* ((state (niwa-shared-editing--read workspace "board"))
           (authored (niwa-artifact-store-artifact-directory workspace "board")))
      (delete-directory authored)
      (with-temp-file authored (insert "occupied"))
      (should-error (niwa-shared-editing--commit
                     workspace (plist-put (copy-sequence state) :title "Later")))
      (should (equal state (niwa-shared-editing--read workspace "board")))
      (should (equal "occupied" (niwa-artifact-store--read-bytes authored))))))

(niwa-deftest niwa-shared-editing--read
  (:doc "pinned reads and commits reject linked ancestors and leaves without repeated host walks")
  (dolist (leaf '("meta.el" "state.json" nil))
    (niwa-shared-editing-test--with-workspace
      (should-not (plist-get
                   (niwa-shared-editing-test--call
                    workspace '(:action "create" :id "board" :kind "whiteboard"
                                :title "Board" :actor "Alice" :opId "one")) :error))
      (let* ((state (niwa-shared-editing--read workspace "board"))
             (directory (directory-file-name
                         (file-name-directory (niwa-shared-editing--state-path workspace "board"))))
             (path (if leaf (file-name-concat directory leaf) directory))
             (bytes (when leaf (niwa-artifact-store--read-bytes path)))
             (victim (file-name-concat root "outside")))
        (cl-letf (((symbol-function 'niwa-resource-within-root-p)
                   (lambda (&rest _) (ert-fail "Repeated host path proof"))))
          (should (equal state (niwa-shared-editing--read workspace "board")))
          (niwa-shared-editing--commit workspace state))
        (rename-file path victim)
        (make-symbolic-link victim path)
        (should-error (niwa-shared-editing--read workspace "board"))
        (should-error (niwa-shared-editing--commit workspace state))
        (if leaf
            (should (equal bytes (niwa-artifact-store--read-bytes victim)))
          (should (file-exists-p (file-name-concat victim "state.json"))))))))

(niwa-deftest niwa-shared-editing-list-batches
  (:doc "catalog batches metadata only and skips linked metadata without hiding siblings")
  (niwa-shared-editing-test--with-workspace
    (dotimes (index 35)
      (let* ((id (format "doc%d" index))
             (path (file-name-concat (niwa-artifact-store-directory workspace)
                                     ".state" id "meta.el")))
        (make-directory (file-name-directory path) t)
        (with-temp-file path
          (prin1 (list :kind 'document :file "state.json" :title id :revision 7)
                 (current-buffer)))))
    (let* ((store (niwa-artifact-store-directory workspace))
           (linked (file-name-concat store ".state/doc3/meta.el"))
           (victim (file-name-concat root "outside"))
           (run (symbol-function 'niwa-session-control-fs-run-program))
           calls catalog)
      (rename-file linked victim)
      (make-symbolic-link victim linked)
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (operations &optional lock)
                   (push operations calls)
                   (funcall run operations lock))))
        (setq catalog (niwa-shared-editing-list workspace)))
      (should (= 34 (length catalog)))
      (should-not (cl-find "doc3" catalog :key (lambda (entry) (plist-get entry :id)) :test #'equal))
      (should (= 2 (length calls)))
      (should (cl-every
               (lambda (batch)
                 (and (<= (length batch) 32)
                      (cl-every (lambda (operation)
                                  (and (eq 'read (plist-get operation :op))
                                       (string-suffix-p "/meta.el" (plist-get operation :path)))) batch)))
               calls)))))

(niwa-deftest niwa-shared-editing-list
  (:doc "Catalogs read metadata without opening the full CRDT state or history")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (niwa-shared-editing-test--call
     workspace '(:action "rename" :id "board" :title "Later" :actor "Alice" :opId "two"))
    (cl-letf (((symbol-function 'niwa-shared-editing--read)
               (lambda (&rest _) (ert-fail "Catalog opened full item state"))))
      (should (equal '((:id "board" :kind "whiteboard" :title "Later" :revision 2))
                     (niwa-shared-editing-list workspace))))))

(niwa-deftest niwa-shared-editing-save-version
  (:doc "Keeps versions without receipts or history, and restores one as an edit")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (should (= 1 (niwa-shared-editing-save-version workspace "board" "s1")))
    (let ((version (niwa-shared-editing--parse
                    (with-temp-buffer
                      (insert-file-contents
                       (niwa-artifact-store-version-path workspace "board" 1))
                      (buffer-string)))))
      (should (plist-get version :crdt))
      (should-not (plist-member version :receipts))
      (should-not (plist-member version :transactions)))
    (niwa-shared-editing-test--call
     workspace '(:action "rename" :id "board" :title "Second" :actor "Alice" :opId "two"))
    (let (reply)
      (niwa-shared-editing-restore workspace "board" 1 "Host"
                                      (lambda (value) (setq reply value)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (not reply) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should-not (plist-get reply :error)))
    (let ((state (niwa-shared-editing--read workspace "board")))
      (should (equal "First" (plist-get state :title)))
      (should (= 3 (plist-get state :revision))))
    ;; The state just committed serves while this Emacs holds the item;
    ;; without the lease, only the store does.
    (cl-letf (((symbol-function 'niwa-shared-editing--read)
               (lambda (&rest _) (error "Read"))))
      (should (= 2 (niwa-shared-editing-save-version workspace "board")))
      (niwa-artifact-lease-release workspace "board")
      (should-error (niwa-shared-editing-save-version workspace "board")))
    (should (equal "First"
                   (plist-get (niwa-shared-editing--parse
                               (with-temp-buffer
                                 (insert-file-contents
                                  (niwa-artifact-store-version-path workspace "board" 2))
                                 (buffer-string)))
                              :title)))))

(niwa-deftest niwa-shared-editing-save-version--released
  (:doc "Reads the store once the lease was released, though held again since")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (niwa-artifact-lease-release workspace "board")
    ;; Another Emacs takes it, renames it and releases it.
    (let ((niwa-session-durability--client-id (make-string 64 ?b))
          (niwa-artifact-lease--held (make-hash-table :test #'equal))
          (state (niwa-shared-editing--read workspace "board")))
      (niwa-artifact-lease-acquire workspace "board")
      (niwa-shared-editing--commit workspace (plist-put (copy-sequence state) :title "Laptop"))
      (niwa-artifact-lease-release workspace "board"))
    ;; Taken back without an edit of this Emacs's own.
    (niwa-artifact-lease-ensure workspace "board")
    (niwa-shared-editing-save-version workspace "board")
    (should (equal "Laptop"
                   (plist-get (niwa-shared-editing--parse
                               (with-temp-buffer
                                 (insert-file-contents
                                  (niwa-artifact-store-version-path workspace "board" 1))
                                 (buffer-string)))
                              :title)))))

(niwa-deftest niwa-shared-editing-save-version-later
  ()
  ,test (test)
  :doc "Versions an item after the saves queued before it"
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one")
     #'ignore)
    (niwa-shared-editing-save-version-later workspace "board" "s1")
    (let ((deadline (+ (float-time) 10)))
      (while (and (not (niwa-artifact-store-versions workspace "board"))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (equal "s1" (plist-get (car (niwa-artifact-store-versions workspace "board"))
                                   :session))))

  :doc "attributes a deferred failure after the origin buffer dies and another workspace is current"
  (let* ((workspace (niwa-workspace--create :root "/origin/project/"))
         (origin (generate-new-buffer " *version-origin*"))
         callback captured)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local niwa--workspace workspace)
            (cl-letf (((symbol-function 'niwa-artifact-backend-call)
                       (lambda (_workspace _args reply &rest _) (setq callback reply))))
              (niwa-shared-editing-save-version-later workspace "board")))
          (kill-buffer origin)
          (with-temp-buffer
            (setq-local niwa--workspace (niwa-workspace--create :root "/other/project/"))
            (niwa-test--with-captured-messages captured
              (funcall callback '(:error "Disk full"))))
          (should (equal "niwa [/origin/project/]: No version of board was saved: Disk full\n"
                         captured)))
      (when (buffer-live-p origin) (kill-buffer origin)))))

(niwa-deftest niwa-shared-editing-duplicate
  (:doc "Copies an item into an independent one under its own lease")
  (niwa-shared-editing-test--with-workspace
    (niwa-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "Plan"
                 :actor "Alice" :opId "one"))
    (should (equal "copy" (niwa-artifact-store-duplicate workspace "board" "copy")))
    (should (equal "copy" (plist-get (niwa-shared-editing--read workspace "copy") :id)))
    (should (equal '(:kind whiteboard :title "Plan")
                   (cl-subseq (niwa-artifact-store-meta workspace "copy") 0 4)))
    (should (= 1 (length (niwa-artifact-store-versions workspace "copy"))))
    (should-error (niwa-shared-editing-duplicate workspace "board" "copy"))
    (make-directory (niwa-artifact-store-bookkeeping-directory workspace "orphan"))
    (should-error (niwa-shared-editing-duplicate workspace "board" "orphan"))
    (should (file-directory-p (niwa-artifact-store-bookkeeping-directory workspace "orphan")))
    (should-error (niwa-shared-editing-duplicate workspace "board" "../x"))))

(provide 'test-niwa-shared-editing)
;;; test-niwa-shared-editing.el ends here
