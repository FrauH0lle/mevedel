;;; test-mevedel-shared-editing.el --- Shared editing acceptance -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared editing tests through the real host and existing public seams.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-shared-editing)

(require 'mevedel-artifact-store)

(defmacro mevedel-shared-editing-test--with-workspace (&rest body)
  "Run BODY with WORKSPACE in a temp ROOT, its own runtimes and leases."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "mevedel-editing-" t)))
          (workspace (mevedel-workspace--create :type 'file :id "w" :root root :name "w"))
          (mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
          (mevedel-artifact-lease--held (make-hash-table :test #'equal))
          (mevedel-session-durability--client-id (make-string 64 ?a)))
     (unwind-protect (progn ,@body)
       (mevedel-shared-editing-stop)
       (maphash (lambda (_directory held)
                  (when (timerp (plist-get held :timer))
                    (cancel-timer (plist-get held :timer))))
                mevedel-artifact-lease--held)
       (delete-directory root t))))

(defun mevedel-shared-editing-test--call (workspace args)
  "Run ARGS for WORKSPACE through the queue and return the reply."
  (let (reply)
    (mevedel-shared-editing-call workspace args (lambda (value) (setq reply value)))
    (let ((deadline (+ (float-time) 10)))
      (while (and (not reply) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    reply))

(mevedel-deftest mevedel-shared-editing--send
  (:doc "Large helper requests use small UTF-8 writes without changing JSON framing")
  (let* ((args (list :title (concat (make-string 1020 ?x) "λ 🌱")
                     :data (make-string 10000 ?é)))
         (expected (concat (mevedel-shared-editing--json args) "\n"))
         chunks)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_process chunk)
                 (should (<= (string-bytes (encode-coding-string chunk 'utf-8-unix)) 4096))
                 (push chunk chunks))))
      (mevedel-shared-editing--send nil args))
    (should (> (length chunks) 1))
    (should (equal expected (apply #'concat (nreverse chunks))))))

(mevedel-deftest mevedel-shared-editing--process
  (:doc "Helper replies frame fragmented Unicode, multiple lines, and bound unfinished bytes")
  (let ((runtime (list :active (list :requestId 1 :callback #'ignore)))
        replies)
    (unwind-protect
        (progn
          (let* ((process (mevedel-shared-editing--process runtime))
                 (filter (process-filter process))
                 (line "{\"requestId\":1,\"result\":{\"title\":\"λ 🌱\"}}\n"))
            (cl-letf (((symbol-function 'mevedel-shared-editing--accept)
                       (lambda (_runtime _job reply) (push reply replies))))
              (funcall filter process (substring line 0 12))
              (should-not replies)
              (funcall filter process (concat (substring line 12) line))
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

(mevedel-deftest mevedel-shared-editing-call
  ()
  ,test (test)
  :doc "Commits a board into the store under its lease and reopens it"
  (mevedel-shared-editing-test--with-workspace
    (let ((reply (mevedel-shared-editing-test--call
                  workspace '(:action "create" :id "board1" :kind "whiteboard"
                              :title "Architecture" :actor "Alice" :opId "one"))))
      (should-not (plist-get reply :error))
      (should (= 1 (plist-get (plist-get reply :result) :revision)))
      (should (file-exists-p (file-name-concat root ".mevedel/artifacts/board1/state.json")))
      (should (equal '(:kind whiteboard :title "Architecture" :file "state.json")
                     (cl-subseq (mevedel-artifact-store-meta workspace "board1") 0 6)))
      (should (mevedel-artifact-lease-held-p workspace "board1"))
      (should (equal '("board1") (mevedel-shared-editing-ids workspace)))
      (should (equal "Architecture"
                     (plist-get (car (mevedel-shared-editing-list workspace)) :title)))
      (mevedel-shared-editing-stop)
      (should (equal "Architecture"
                     (plist-get (plist-get (mevedel-shared-editing-test--call
                                            workspace '(:action "read" :id "board1"))
                                           :result)
                                :title)))
      ;; A rename reaches the store's metadata.
      (should-not (plist-get (mevedel-shared-editing-test--call
                              workspace '(:action "rename" :id "board1" :title "Later"
                                          :actor "Alice" :opId "two"))
                             :error))
      (should (equal "Later" (plist-get (mevedel-artifact-store-meta workspace "board1")
                                        :title)))))

  :doc "Another Emacs's item is read-only here"
  (mevedel-shared-editing-test--with-workspace
    (should-not (plist-get (mevedel-shared-editing-test--call
                            workspace '(:action "create" :id "board1" :kind "whiteboard"
                                        :title "Plan" :actor "Alice" :opId "one"))
                           :error))
    (let ((mevedel-session-durability--client-id (make-string 64 ?b))
          (mevedel-artifact-lease--held (make-hash-table :test #'equal)))
      (should (string-match-p "being edited in Emacs"
                              (plist-get (mevedel-shared-editing-test--call
                                          workspace '(:action "rename" :id "board1"
                                                      :title "Theirs" :actor "Bob"
                                                      :opId "two"))
                                         :error)))
      ;; Reading needs no lease.
      (should (equal "Plan" (plist-get (plist-get (mevedel-shared-editing-test--call
                                                   workspace '(:action "read" :id "board1"))
                                                  :result)
                                       :title)))))

  :doc "Availability is optional, read-only, and recovers after runtime and resource repair"
  (mevedel-shared-editing-test--with-workspace
    (let* ((directory (make-temp-file "mevedel-editing-status-" t))
           (resources mevedel-shared-editing--directory)
           (node mevedel-shared-editing-node-program))
      (unwind-protect
          (cl-labels ((call (action)
                        (let ((reply (mevedel-shared-editing-test--call
                                      workspace (list :action action))))
                          (should reply)
                          reply)))
            (let ((mevedel-shared-editing-node-program
                   (file-name-concat directory "missing-node")))
              (should (string-match-p "Install Node" (plist-get (call "status") :error)))
              (should (equal [] (plist-get (call "list") :result))))
            (should (eq t (plist-get (plist-get (call "status") :result) :available)))
            (let ((mevedel-shared-editing--directory directory))
              (should (string-match-p "resources" (plist-get (call "status") :error))))
            (copy-file (file-name-concat resources "host.bundle.mjs")
                       (file-name-concat directory "host.bundle.mjs"))
            (let ((mevedel-shared-editing--directory directory))
              (should (string-match-p "resources" (plist-get (call "status") :error)))
              (dolist (file '("resvg.wasm" "font.ttf" "Excalifont.ttf" "Nunito.ttf" "ComicShanns.ttf"))
                (copy-file (file-name-concat resources file)
                           (file-name-concat directory file)))
              (should (eq t (plist-get (plist-get (call "status") :result) :available))))
            ;; A configured runtime change also invalidates a live helper.
            (let ((mevedel-shared-editing-node-program
                   (file-name-concat directory "missing-node")))
              (should (plist-get (call "status") :error)))
            (let ((mevedel-shared-editing-node-program node))
              (should (eq t (plist-get (plist-get (call "status") :result) :available))))
            (should-not (file-exists-p (mevedel-artifact-store-directory workspace)))
            (should (equal (sort (directory-files directory nil "^[^.]") #'string<)
                           '("ComicShanns.ttf" "Excalifont.ttf" "Nunito.ttf" "font.ttf" "host.bundle.mjs" "resvg.wasm"))))
        (delete-directory directory t)))))

(mevedel-deftest mevedel-shared-editing--drain
  ()
  ,test (test)
  :doc "The queue never asks to take an item over; a refusal settles the job"
  (mevedel-shared-editing-test--with-workspace
    (mevedel-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "Plan"
                 :actor "Alice" :opId "one"))
    (cl-letf (((symbol-function 'mevedel-artifact-lease-ensure)
               (lambda (_workspace _id &optional ask)
                 (should-not ask)
                 (user-error "This needs a decision in Emacs on the host first"))))
      (should (string-match-p "decision in Emacs"
                              (plist-get (mevedel-shared-editing-test--call
                                          workspace '(:action "rename" :id "board" :title "X"
                                                      :actor "Guest" :opId "a"))
                                         :error))))
    ;; A quit during target I/O settles the job and leaves the queue working.
    (cl-letf (((symbol-function 'mevedel-artifact-lease-ensure)
               (lambda (&rest _) (signal 'quit nil))))
      (should (equal "Editing operation cancelled"
                     (plist-get (mevedel-shared-editing-test--call
                                 workspace '(:action "rename" :id "board" :title "X"
                                             :actor "Alice" :opId "b"))
                                :error))))
    (should-not (plist-get (mevedel-shared-editing--runtime workspace) :active))
    (should (= 1 (length (plist-get (mevedel-shared-editing-test--call
                                     workspace '(:action "list"))
                                    :result)))))

  :doc "Stopping the runtime during lease I/O settles the job once and sends nothing"
  (mevedel-shared-editing-test--with-workspace
    (mevedel-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "Plan"
                 :actor "Alice" :opId "one"))
    (let ((runtime (mevedel-shared-editing--runtime workspace))
          (replies nil))
      (cl-letf (((symbol-function 'mevedel-artifact-lease-ensure)
                 (lambda (&rest _) (mevedel-shared-editing-stop runtime "Helper exited") t)))
        (mevedel-shared-editing-call
         workspace '(:action "rename" :id "board" :title "Lost" :actor "Alice" :opId "two")
         (lambda (reply) (push reply replies)))
        (let ((deadline (+ (float-time) 1)))
          (while (< (float-time) deadline)
            (accept-process-output nil 0.05))))
      (should (equal '((:error "Helper exited")) replies))
      (should-not (process-live-p (plist-get runtime :process)))
      (should (equal "Plan" (plist-get (mevedel-shared-editing--read workspace "board")
                                       :title)))))

  :doc "Editing a deleted item fails without leasing it again"
  (mevedel-shared-editing-test--with-workspace
    (should (equal "This item no longer exists"
                   (plist-get (mevedel-shared-editing-test--call
                               workspace '(:action "rename" :id "ghost" :title "X"
                                           :actor "Guest" :opId "a"))
                              :error)))
    (should-not (file-exists-p (mevedel-artifact-lease-directory workspace "ghost"))))

  :doc "An interrupted create, its metadata written but not its state, retries"
  (mevedel-shared-editing-test--with-workspace
    (make-directory (mevedel-artifact-store-artifact-directory workspace "board") t)
    (mevedel-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (should-not (plist-get (mevedel-shared-editing-test--call
                            workspace '(:action "create" :id "board" :kind "whiteboard"
                                        :title "Plan" :actor "Alice" :opId "one"))
                           :error))
    (should (mevedel-shared-editing-present-p workspace "board"))
    ;; Any other directory is taken.
    (make-directory (mevedel-artifact-store-artifact-directory workspace "page") t)
    (should (string-match-p "already exists"
                            (plist-get (mevedel-shared-editing-test--call
                                        workspace '(:action "create" :id "page" :kind "document"
                                                    :title "P" :actor "Alice" :opId "two"))
                                       :error)))))

(mevedel-deftest mevedel-shared-editing-stop
  (:doc "Stopping during a commit settles it once and then stops the helper")
  (mevedel-shared-editing-test--with-workspace
    (let* ((calls 0) reply runtime
           (mevedel-shared-editing-change-hook
            (list (lambda (&rest _) (mevedel-shared-editing-stop runtime)))))
      (setq runtime (mevedel-shared-editing--runtime workspace))
      (mevedel-shared-editing-call
       workspace '(:action "create" :id "committed" :kind "whiteboard"
                   :title "Committed" :actor "Alice" :opId "one")
       (lambda (result) (cl-incf calls) (setq reply result)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (not reply) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (= calls 1))
      (should-not (plist-get reply :error))
      (should-not (process-live-p (plist-get runtime :process)))
      (should-not (mevedel-shared-editing--live-p runtime))
      (should (= 1 (plist-get (mevedel-shared-editing--read workspace "committed")
                              :revision))))))

(mevedel-deftest mevedel-shared-editing-list
  (:doc "Lists whiteboards and documents from their metadata, never their state")
  (mevedel-shared-editing-test--with-workspace
    (dolist (spec '(("board" whiteboard "state.json") ("page" html "index.html")))
      (make-directory (mevedel-artifact-store-artifact-directory workspace (car spec)) t)
      (write-region "not json" nil
                    (file-name-concat (mevedel-artifact-store-artifact-directory
                                       workspace (car spec))
                                      (nth 2 spec))
                    nil 'silent)
      (mevedel-artifact-store-create-meta workspace (car spec) (nth 2 spec) (cadr spec) "Plan"))
    (should (equal '((:id "board" :kind "whiteboard" :title "Plan"))
                   (mevedel-shared-editing-list workspace)))
    (should (equal '("board") (mevedel-shared-editing-ids workspace)))
    (should (mevedel-shared-editing-present-p workspace "board"))
    (should-not (mevedel-shared-editing-present-p workspace "page"))))

(mevedel-deftest mevedel-shared-editing--parse
  (:doc "Preserves empty mark attributes, arrays, nulls and false through exact patch reads")
  (let* ((json "{\"before\":null,\"marks\":[{\"type\":\"bold\",\"attrs\":{}}],\"empty\":[],\"flag\":false}")
         (value (mevedel-shared-editing--parse json)))
    (should (null (plist-get value :before)))
    (should (hash-table-p (plist-get (aref (plist-get value :marks) 0) :attrs)))
    (should (equal json (mevedel-shared-editing--json value)))))

(mevedel-deftest mevedel-shared-editing--json
  (:doc "Unicode JSON remains text when nested in model or browser messages")
  (let* ((value (list :title (string #x2014 #x03bb #x1f331) :empty nil :flag :json-false))
         (text (mevedel-shared-editing--json value))
         (envelope (json-serialize (list :text text))))
    (should (multibyte-string-p text))
    (should (equal (plist-get (json-parse-string envelope :object-type 'plist) :text) text))
    (should (equal value (mevedel-shared-editing--parse text)))))

(mevedel-deftest mevedel-shared-editing--delete
  (:doc "Deleting through the editing queue removes the artifact and names who deleted it")
  (mevedel-shared-editing-test--with-workspace
    (let (observed)
      (mevedel-shared-editing-test--call
       workspace '(:action "create" :id "board" :kind "whiteboard" :title "Board"
                   :actor "Alice" :opId "one"))
      (let ((mevedel-shared-editing-change-hook
             (list (lambda (_workspace state _result) (push state observed)))))
        (should (equal '(:id "board" :deleted t)
                       (plist-get (mevedel-shared-editing-test--call
                                   workspace '(:action "delete" :id "board" :actor "Guest: Ann"))
                                  :result)))
        (should-not (file-exists-p (mevedel-artifact-store-artifact-directory workspace "board")))
        (should-not (file-exists-p (mevedel-artifact-lease-directory workspace "board")))
        (should (equal '(:id "board" :deleted t :actor "Guest: Ann") (car observed)))
        (should-not (mevedel-shared-editing-list workspace))
        (should (equal "This item no longer exists"
                       (plist-get (mevedel-shared-editing-test--call
                                   workspace '(:action "delete" :id "board"))
                                  :error)))))))

(mevedel-deftest mevedel-shared-editing-save-version
  (:doc "Keeps versions without receipts or history, and restores one as an edit")
  (mevedel-shared-editing-test--with-workspace
    (mevedel-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one"))
    (should (= 1 (mevedel-shared-editing-save-version workspace "board" "s1")))
    (let ((version (mevedel-shared-editing--parse
                    (with-temp-buffer
                      (insert-file-contents
                       (mevedel-artifact-store-version-path workspace "board" 1))
                      (buffer-string)))))
      (should (plist-get version :crdt))
      (should-not (plist-member version :receipts))
      (should-not (plist-member version :transactions)))
    (mevedel-shared-editing-test--call
     workspace '(:action "rename" :id "board" :title "Second" :actor "Alice" :opId "two"))
    (let (reply)
      (mevedel-shared-editing-restore workspace "board" 1 "Host"
                                      (lambda (value) (setq reply value)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (not reply) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should-not (plist-get reply :error)))
    (let ((state (mevedel-shared-editing--read workspace "board")))
      (should (equal "First" (plist-get state :title)))
      (should (= 3 (plist-get state :revision))))
    ;; The state just committed serves while this Emacs holds the item;
    ;; without the lease, only the store does.
    (cl-letf (((symbol-function 'mevedel-shared-editing--read)
               (lambda (&rest _) (error "Read"))))
      (should (= 2 (mevedel-shared-editing-save-version workspace "board")))
      (mevedel-artifact-lease-release workspace "board")
      (should-error (mevedel-shared-editing-save-version workspace "board")))
    (should (equal "First"
                   (plist-get (mevedel-shared-editing--parse
                               (with-temp-buffer
                                 (insert-file-contents
                                  (mevedel-artifact-store-version-path workspace "board" 2))
                                 (buffer-string)))
                              :title)))))

(mevedel-deftest mevedel-shared-editing-save-version-later
  (:doc "Versions an item after the saves queued before it")
  (mevedel-shared-editing-test--with-workspace
    (mevedel-shared-editing-call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "First"
                 :actor "Alice" :opId "one")
     #'ignore)
    (mevedel-shared-editing-save-version-later workspace "board" "s1")
    (let ((deadline (+ (float-time) 10)))
      (while (and (not (mevedel-artifact-store-versions workspace "board"))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (equal "s1" (plist-get (car (mevedel-artifact-store-versions workspace "board"))
                                   :session)))))

(mevedel-deftest mevedel-shared-editing-duplicate
  (:doc "Copies an item into an independent one under its own lease")
  (mevedel-shared-editing-test--with-workspace
    (mevedel-shared-editing-test--call
     workspace '(:action "create" :id "board" :kind "whiteboard" :title "Plan"
                 :actor "Alice" :opId "one"))
    (should (equal "copy" (mevedel-artifact-store-duplicate workspace "board" "copy")))
    (should (equal "copy" (plist-get (mevedel-shared-editing--read workspace "copy") :id)))
    (should (equal '(:kind whiteboard :title "Plan")
                   (cl-subseq (mevedel-artifact-store-meta workspace "copy") 0 4)))
    (should (= 1 (length (mevedel-artifact-store-versions workspace "copy"))))
    (should-error (mevedel-shared-editing-duplicate workspace "board" "copy"))
    (should-error (mevedel-shared-editing-duplicate workspace "board" "../x"))))

(provide 'test-mevedel-shared-editing)
;;; test-mevedel-shared-editing.el ends here
