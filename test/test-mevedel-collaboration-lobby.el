;;; test-mevedel-collaboration-lobby.el --- Collaboration lobby tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the per-workspace lobby: persisted credentials, the session
;; listing, opening, creating and deleting sessions from guest frames,
;; the lobby lifecycle, and restarting lobbies with Emacs.

;;; Code:

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
(require 'cl-lib)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-lobby)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-share)
(require 'mevedel-collaboration-transport)
(require 'mevedel-session-persistence)
(require 'mevedel-session-durability)
(require 'mevedel-structs)
(require 'mevedel-utilities)
;; Restoring a session loads the chat view.  Its first load initializes
;; D-Bus (through `battery'), which reads events and so cannot happen
;; inside a lobby frame; load it here so lease cases test ownership only.
(require 'mevedel-view-stream)
(require 'mevedel-workspace)
(require 'mevedel-workspace-schedule)
(require 'mevedel-workspace-schedule-runs)

(defun mevedel-collaboration-lobby-test--workspace (root)
  "Return a workspace struct rooted at ROOT, outside the registry."
  (mevedel-workspace--create
   :type 'project :id root :root root :name "proj"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defmacro mevedel-collaboration-lobby-test--with-user-dir (&rest body)
  "Run BODY with `mevedel-user-dir' in a fresh temporary directory."
  (declare (indent 0))
  `(let ((mevedel-user-dir (file-name-as-directory
                            (make-temp-file "mevedel-lobby-user-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory mevedel-user-dir t))))

(defmacro mevedel-collaboration-lobby-test--with-root (root &rest body)
  "Bind ROOT to a fresh temporary directory around BODY.
`mevedel-user-dir' is a fresh temporary directory too, so recording a
running lobby touches no real state."
  (declare (indent 1))
  `(mevedel-collaboration-lobby-test--with-user-dir
     (let ((,root (file-name-as-directory
                   (make-temp-file "mevedel-lobby-" t))))
       (unwind-protect (progn ,@body)
         (delete-directory ,root t)))))

(defun mevedel-collaboration-lobby-test--lobby (workspace &rest keys)
  "Return a lobby plist for WORKSPACE with KEYS added."
  (append keys
          (list :transport 'transport :workspace workspace
                :directory (mevedel-workspace-root workspace)
                :project "proj"
                :write-token (make-string 16 ?w)
                :owner-token (make-string 16 ?o)
                :link-view "view-link" :link-full "full-link"
                :link-owner "owner-link"
                :guests (make-hash-table :test #'eql))))

(defun mevedel-collaboration-lobby-test--session (id)
  "Return a session struct with session ID."
  (let ((session (mevedel-session--create :name id)))
    (setf (mevedel-session-session-id session) id)
    session))

(defun mevedel-collaboration-lobby-test--dead-pid ()
  "Return the PID of a child process that has exited and been reaped."
  (let ((child (make-process :name "lobby-dead-holder" :command '("true")
                             :noquery t :sentinel #'ignore)))
    (while (process-live-p child) (accept-process-output child 0.01))
    (process-id child)))

(defun mevedel-collaboration-lobby-test--file-bytes (path)
  "Return PATH's exact contents, or nil when it does not exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (buffer-string))))

(defun mevedel-collaboration-lobby-test--session-live-p (id)
  "Return the live buffer of the session with ID, or nil."
  (seq-find (lambda (buffer)
              (when-let* ((session (buffer-local-value 'mevedel--session
                                                       buffer)))
                (equal id (mevedel-session-session-id session))))
            (buffer-list)))

(defun mevedel-collaboration-lobby-test--open-leased (holder &optional interfere guest)
  "Open a real saved session with a foreign lease naming HOLDER.
INTERFERE receives the lease directory and observed head before a claim.
GUEST defaults to a writable guest.  Return the reply, lease bytes and head,
captured prompts, and whether the restored buffer is live and read-only."
  (cl-destructuring-bind (workspace . root)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* (prompts lease-directory before sent
           (mevedel-collaboration-lobby--lobbies
            (make-hash-table :test #'equal))
           (session (mevedel-session-create "main" workspace))
           (id (mevedel-session-session-id session))
           (data (generate-new-buffer " *lobby-leased*"))
           (lobby (mevedel-collaboration-lobby-test--lobby workspace))
           (claim-next (symbol-function 'mevedel-session-durability--claim-next))
           (ask (lambda (prompt &rest _)
                  (push prompt prompts)
                  (signal 'inhibited-interaction (list prompt)))))
      (unwind-protect
          (progn
            (with-current-buffer data
              (org-mode)
              (insert "Saved transcript\n")
              (mevedel-session-artifacts-save session data))
            (test-mevedel-session-persistence--release-and-kill data session)
            (setq lease-directory
                  (mevedel-session-durability--lease-path
                   (mevedel-session-save-path session)))
            (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
              (cl-letf (((symbol-function 'mevedel-session-durability--holder)
                         (lambda () holder)))
                (should (mevedel-session-durability-lease-acquire
                         (mevedel-session-save-path session) "*other client*"))))
            (setq before (mevedel-collaboration-lobby-test--lease-bytes lease-directory))
            (puthash 2 (or guest '(:name "Writer" :writable t))
                     (plist-get lobby :guests))
            (puthash root lobby mevedel-collaboration-lobby--lobbies)
            (cl-letf (((symbol-function
                        'mevedel-collaboration--transport-send)
                       (lambda (_transport peer frame)
                         (push (cons peer frame) sent) t))
                      ((symbol-function 'mevedel-collaboration--start)
                       (lambda (_session _buffer)
                         '(:link-view "v" :link-full "f" :link-owner "o")))
                      ((symbol-function 'y-or-n-p) ask)
                      ((symbol-function 'yes-or-no-p) ask)
                      ((symbol-function 'read-char-choice) ask)
                      ((symbol-function 'read-directory-name) ask)
                      ((symbol-function 'mevedel-session-durability--claim-next)
                       (lambda (directory expected buffer-name &rest args)
                         (when (and interfere expected)
                           (funcall interfere directory expected))
                         (apply claim-next directory expected buffer-name args))))
              (mevedel-collaboration-lobby--on-frame
               root 2 (list :t "open-session" :reqId 7 :id id)))
            (should (= 1 (length sent)))
            (list :reply (cdar sent)
                  :before before
                  :after (mevedel-collaboration-lobby-test--lease-bytes lease-directory)
                  :head (mevedel-session-durability--lease-head lease-directory)
                  :prompts prompts
                  :read-only
                  (when-let* ((buffer (mevedel-collaboration-lobby-test--session-live-p id)))
                    (buffer-local-value 'buffer-read-only buffer))
                  :live (and (mevedel-collaboration-lobby-test--session-live-p
                              id)
                             t)))
        (let ((restored (mevedel-collaboration-lobby-test--session-live-p id)))
          (test-mevedel-session-persistence--release-and-kill
           restored (and restored (buffer-local-value 'mevedel--session
                                                      restored))))
        (when (buffer-live-p data) (kill-buffer data))
        (delete-directory root t)
        (mevedel-workspace-clear-registry)))))

(defun mevedel-collaboration-lobby-test--lease-bytes (directory)
  "Return every lease file's path and bytes below DIRECTORY."
  (mapcar (lambda (path)
            (cons path (mevedel-collaboration-lobby-test--file-bytes path)))
          (sort (directory-files-recursively directory ".") #'string<)))

(defun mevedel-collaboration-lobby-test--holder-record (pid &optional identity)
  "Return same-host holder evidence for PID with IDENTITY."
  (list :host (mevedel-session-durability--host) :uid (user-uid)
        :pid pid :process (or identity
                             (mevedel--process-identity (emacs-pid)))))

(defun mevedel-collaboration-lobby-test--holder ()
  "Start a live child process to stand in for a lease holder."
  (make-process :name "lobby-live-holder" :command '("sleep" "60")
                :noquery t :sentinel #'ignore))

(defun mevedel-collaboration-lobby-test--identity (process)
  "Return PROCESS's current OS identity, skipping where none is available."
  (or (mevedel--process-identity (process-id process))
      (ert-skip "Process identity is unavailable on this host")))

(defun mevedel-collaboration-lobby-test--other-identity (identity &optional boot)
  "Return a valid IDENTITY for a different process incarnation.
With BOOT, keep the start ticks and replace the boot id instead, as
after a reboot; otherwise keep the boot and move the start earlier."
  (should (string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" identity))
  (let ((boot-id (match-string 1 identity))
        (ticks (string-to-number (match-string 2 identity))))
    (if boot
        (format "%s:%d" boot ticks)
      (format "%s:%d" boot-id (max 0 (- ticks 1000))))))

(defun mevedel-collaboration-lobby-test--should-open (outcome)
  "Assert OUTCOME opened the session under an owned lease, asking nothing."
  (should (equal '(:t "open-session" :reqId 7 :ok t :link "f")
                 (plist-get outcome :reply)))
  (should-not (plist-get outcome :prompts))
  (should (plist-get outcome :live))
  (should-not (plist-get outcome :read-only))
  (should (equal mevedel-session-durability--client-id
                 (plist-get (plist-get outcome :head) :client-id)))
  (let ((holder (plist-get (plist-get outcome :head) :holder)))
    (should (equal (emacs-pid) (plist-get holder :pid)))
    (should (equal (mevedel-session-durability--host) (plist-get holder :host)))
    (should (equal (mevedel--process-identity (emacs-pid))
                   (plist-get holder :process)))))

(defun mevedel-collaboration-lobby-test--should-inspect (outcome)
  "Assert OUTCOME opened read-only without changing the foreign lease."
  (should (equal '(:t "open-session" :reqId 7 :ok t :link "f")
                 (plist-get outcome :reply)))
  (should (plist-get outcome :live))
  (should (plist-get outcome :read-only))
  (should-not (plist-get outcome :prompts))
  (should (equal (plist-get outcome :before) (plist-get outcome :after))))

(defun mevedel-collaboration-lobby-test--should-refuse (outcome &optional message)
  "Assert OUTCOME refused the guest and left the lease byte for byte.
MESSAGE, when non-nil, is the refusal the guest must receive."
  (let ((reply (plist-get outcome :reply)))
    (should (eq :json-false (plist-get reply :ok)))
    (should-not (plist-get reply :link))
    (when message
      (should (equal message (plist-get reply :message)))))
  (should-not (plist-get outcome :live))
  (should (equal (plist-get outcome :before) (plist-get outcome :after))))

(mevedel-deftest mevedel-collaboration-lobby--read-credentials
  (:doc "accepts only complete, well-formed stored credentials")
  (mevedel-collaboration-lobby-test--with-root root
    (let ((path (file-name-concat root "lobby"))
          (b64 #'mevedel-collaboration--base64url))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      (with-temp-file path (insert "(unbalanced"))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      ;; A truncated key is not a weaker lobby, it is no lobby.
      (with-temp-file path
        (prin1 (list :room-id "abcdefghijkl"
                     :key (funcall b64 (make-string 31 ?k))
                     :write-token (funcall b64 (make-string 16 ?w))
                     :owner-token (funcall b64 (make-string 16 ?o)))
               (current-buffer)))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      (with-temp-file path
        (prin1 (list :room-id "bad/id"
                     :key (funcall b64 (make-string 32 ?k))
                     :write-token (funcall b64 (make-string 16 ?w))
                     :owner-token (funcall b64 (make-string 16 ?o)))
               (current-buffer)))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      (with-temp-file path
        (prin1 (list :room-id "abcdefghijkl"
                     :key (funcall b64 (make-string 32 ?k))
                     :write-token (funcall b64 (make-string 16 ?w))
                     :owner-token (funcall b64 (make-string 16 ?o)))
               (current-buffer)))
      (should (equal (list :room-id "abcdefghijkl"
                           :key (make-string 32 ?k)
                           :write-token (make-string 16 ?w)
                           :owner-token (make-string 16 ?o))
                     (mevedel-collaboration-lobby--read-credentials path))))))

(mevedel-deftest mevedel-collaboration-lobby--credentials
  (:doc "creates private credentials once, reuses them, and rotates them")
  (mevedel-collaboration-lobby-test--with-root root
    (let* ((workspace (mevedel-collaboration-lobby-test--workspace root))
           (path (mevedel-collaboration-lobby--credentials-path workspace))
           (first (mevedel-collaboration-lobby--credentials workspace)))
      (should (equal (file-name-concat root ".mevedel/" "lobby") path))
      (should (= #o600 (file-modes path)))
      (should (= 32 (length (plist-get first :key))))
      ;; A restart reads the same link back.
      (should (equal first (mevedel-collaboration-lobby--credentials
                            workspace)))
      (let ((rotated (mevedel-collaboration-lobby--credentials workspace t)))
        (should-not (equal (plist-get first :room-id)
                           (plist-get rotated :room-id)))
        (should-not (equal (plist-get first :key) (plist-get rotated :key)))
        (should (equal rotated (mevedel-collaboration-lobby--credentials
                                workspace)))
        (should (= #o600 (file-modes path))))
      ;; Unreadable credentials are replaced rather than trusted.
      (with-temp-file path (insert "garbage"))
      (let ((fresh (mevedel-collaboration-lobby--credentials workspace)))
        (should (= 16 (length (plist-get fresh :owner-token))))
        (should (equal fresh (mevedel-collaboration-lobby--credentials
                              workspace)))))))

(mevedel-deftest mevedel-collaboration-lobby--preview
  (:doc "prefers the latest prompt, flattens whitespace, and bounds length")
  (progn
    (should-not (mevedel-collaboration-lobby--preview nil))
    (should (equal "first"
                   (mevedel-collaboration-lobby--preview
                    '(:first-user-message "first"))))
    (should (equal "latest words"
                   (mevedel-collaboration-lobby--preview
                    '(:first-user-message "first"
                      :latest-user-message "  latest\n\n words "))))
    (should (= mevedel-collaboration-lobby--max-preview-chars
               (string-width
                (mevedel-collaboration-lobby--preview
                 (list :latest-user-message (make-string 500 ?x))))))))

(mevedel-deftest mevedel-collaboration-lobby--updated
  (:doc "converts a sidecar timestamp to epoch seconds")
  (progn
    (should-not (mevedel-collaboration-lobby--updated nil))
    (should-not (mevedel-collaboration-lobby--updated '(:updated-at "never")))
    (should (= (truncate (float-time (encode-time
                                      (list 14 49 0 15 9 2026 nil -1 nil))))
               (mevedel-collaboration-lobby--updated
                '(:updated-at "2026-09-15T00-49-14"))))))

(mevedel-deftest mevedel-collaboration-lobby--live-sessions
  (:doc "keys live root sessions by session id")
  (let ((buffer (generate-new-buffer " *lobby-live*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "s1")))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace) `(("design" . ,buffer)))))
            (should (equal `(("s1" "design" . ,buffer))
                           (mevedel-collaboration-lobby--live-sessions
                            'workspace)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-collaboration-lobby--rows
  (:doc "lists unsaved live sessions first and marks open saved ones")
  (let ((live-buffer (generate-new-buffer " *lobby-live*"))
        (shared-buffer (generate-new-buffer " *lobby-shared*"))
        (s3 (list :session-id "s3")))
    (unwind-protect
        (let ((mevedel-collaboration--rooms
               (mevedel-test-room-registry
                (list :data-buffer shared-buffer))))
          (with-current-buffer live-buffer
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "fresh")))
          (with-current-buffer shared-buffer
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "s2")))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace)
                       `(("new" . ,live-buffer) ("renamed" . ,shared-buffer))))
                    ((symbol-function 'mevedel-artifact-store-dedicated-p)
                     (lambda (_workspace session-id id)
                       (equal (list session-id id) '("s3" "board"))))
                    ((symbol-function
                      'mevedel-session-persistence-list-sessions)
                     (lambda (_workspace &optional _cached)
                       `((:save-path "/s2/"
                          :summary (:session-id "s2" :session-name "old"
                                    :updated-at "2026-09-15T00-49-14"
                                    :latest-user-message "hello"))
                         (:save-path "/s3/" :summary ,s3)))))
            (let ((rows (mevedel-collaboration-lobby--rows 'workspace)))
              (should (equal '("fresh" "s2" "s3")
                             (mapcar (lambda (row) (plist-get row :id)) rows)))
              (should (equal '(:id "fresh" :name "new" :updated nil
                               :preview nil :live t :shared :json-false)
                             (nth 0 rows)))
              ;; The live name wins over the saved one, and an open
              ;; shared session says so.
              (should (equal "renamed" (plist-get (nth 1 rows) :name)))
              (should (eq t (plist-get (nth 1 rows) :shared)))
              (should (equal "hello" (plist-get (nth 1 rows) :preview)))
              (should (integerp (plist-get (nth 1 rows) :updated)))
              (should (equal '(:id "s3" :name "Untitled" :updated nil
                               :preview nil :live :json-false
                               :shared :json-false)
                             (nth 2 rows))))
            ;; An artifact's conversation is reached from its artifact.
            (setf (mevedel-session-dedicated-artifact
                   (buffer-local-value 'mevedel--session live-buffer))
                  "board")
            (plist-put s3 :dedicated-artifact "board")
            (should (equal '("s2")
                           (mapcar (lambda (row) (plist-get row :id))
                                   (mevedel-collaboration-lobby--rows 'workspace))))))
      (kill-buffer live-buffer)
      (kill-buffer shared-buffer))))

(mevedel-deftest mevedel-collaboration-lobby-hidden-session
  (:quiet t :doc "dedicated live sessions cannot be opened, renamed or deleted by guessed id")
  (let* ((buffer (generate-new-buffer " *lobby-hidden*"))
         (session (mevedel-collaboration-lobby-test--session "hidden"))
         (lobby (list :workspace 'workspace :guests (make-hash-table :test #'eql)))
         reply)
    (unwind-protect
        (progn
          (setf (mevedel-session-dedicated-artifact session) "board")
          (with-current-buffer buffer (setq-local mevedel--session session))
          (puthash 1 '(:owner t :writable t) (plist-get lobby :guests))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_) `(("hidden" . ,buffer))))
                    ((symbol-function 'mevedel-session-persistence-list-sessions)
                     (lambda (&rest _) nil))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (setq reply frame))))
            (should (equal "No such session"
                           (mevedel-collaboration-lobby--delete-failure
                            lobby '(:owner t) "hidden")))
            (should (buffer-live-p buffer))
            (should-error (mevedel-collaboration-lobby--session-buffer lobby "hidden"))
            (mevedel-collaboration-lobby--handle-rename
             lobby 1 '(:reqId 1 :id "hidden" :name "forged"))
            (should (eq :json-false (plist-get reply :ok)))
            (should (equal "hidden" (mevedel-session-name session)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(mevedel-deftest mevedel-collaboration-lobby--frame
  (:doc "caps the listing and reports how many rows it left out")
  (let ((mevedel-collaboration-lobby--max-sessions 2))
    (cl-letf (((symbol-function 'mevedel-collaboration-lobby--rows)
               (lambda (_workspace) '((:id "a") (:id "b") (:id "c"))))
              ((symbol-function 'mevedel-collaboration--workspace-key)
               (lambda (lobby) (format "key-%s" (plist-get lobby :workspace)))))
      (let ((frame (mevedel-collaboration-lobby--frame
                    '(:workspace w :project "proj"))))
        (should (equal "lobby" (plist-get frame :t)))
        (should (equal "proj" (plist-get frame :project)))
        (should (equal "key-w" (plist-get frame :workspace)))
        (should (equal [(:id "a") (:id "b")] (plist-get frame :sessions)))
        (should (= 1 (plist-get frame :omitted)))
        ;; Rows travel as a JSON array of objects.
        (should (string-prefix-p "{\"t\":\"lobby\",\"project\":\"proj\",\"workspace\":\"key-w\",\"sessions\":[{"
                                 (json-encode frame)))))))

(mevedel-deftest mevedel-collaboration-lobby--session-buffer (:quiet t)
  ,test
  (test)
  :doc "uses a live session, restores a listed one, and refuses others"
  (let ((live (generate-new-buffer " *lobby-live*"))
        restored)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "s1")))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace) `(("one" . ,live))))
                    ((symbol-function
                      'mevedel-session-persistence-list-sessions)
                     (lambda (_workspace &optional _cached)
                       '((:save-path "/sessions/s2/"
                          :summary (:session-id "s2")))))
                    ((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (dir _source _override workspace)
                       (setq restored (list dir workspace))
                       'restored-buffer)))
            (let ((lobby '(:workspace ws)))
              (should (eq live (mevedel-collaboration-lobby--session-buffer
                                lobby "s1")))
              (should-not restored)
              (should (eq 'restored-buffer
                          (mevedel-collaboration-lobby--session-buffer
                           lobby "s2")))
              (should (equal '("/sessions/s2/" ws) restored))
              ;; An id is a selection among listed sessions, never a path.
              (should-error (mevedel-collaboration-lobby--session-buffer
                             lobby "../../etc")))))
      (kill-buffer live)))
  :doc "opens a session whose same-host holder died, without asking"
  (mevedel-collaboration-lobby-test--should-open
   (mevedel-collaboration-lobby-test--open-leased
    (mevedel-collaboration-lobby-test--holder-record
     (mevedel-collaboration-lobby-test--dead-pid))))
  :doc "opens a session whose recorded PID now names another incarnation"
  (let ((reuser (mevedel-collaboration-lobby-test--holder)))
    (unwind-protect
        (let ((identity (mevedel-collaboration-lobby-test--identity reuser)))
          (mevedel-collaboration-lobby-test--should-open
           (mevedel-collaboration-lobby-test--open-leased
            (mevedel-collaboration-lobby-test--holder-record
             (process-id reuser)
             (mevedel-collaboration-lobby-test--other-identity identity)))))
      (delete-process reuser)))
  :doc "inspects without changing a live same-host holder's lease"
  (let ((holder (mevedel-collaboration-lobby-test--holder)))
    (unwind-protect
        (let ((identity (mevedel-collaboration-lobby-test--identity holder)))
          (mevedel-collaboration-lobby-test--should-inspect
           (mevedel-collaboration-lobby-test--open-leased
            (mevedel-collaboration-lobby-test--holder-record
             (process-id holder) identity))))
      (delete-process holder)))
  :doc "inspects without changing a live holder whose identity proves nothing"
  (let ((holder (mevedel-collaboration-lobby-test--holder)))
    (unwind-protect
        (let ((identity (mevedel-collaboration-lobby-test--identity holder)))
          (dolist (recorded (list nil "" "garbage" 42 (list identity)
                                  (mevedel-collaboration-lobby-test--other-identity
                                   identity "00000000-0000-4000-8000-000000000000")
                                  (concat "not-a-boot-id:"
                                          (cadr (split-string identity ":")))
                                  (concat (car (split-string identity ":"))
                                          ":-5")
                                  (concat (car (split-string identity ":"))
                                          ":")
                                  (upcase identity)
                                  (concat (car (split-string identity ":"))
                                          ":0" (cadr (split-string identity ":")))
                                  (concat identity "x")))
            (ert-info ((format "recorded identity %S" recorded))
              (mevedel-collaboration-lobby-test--should-inspect
               (mevedel-collaboration-lobby-test--open-leased
                (list :host (mevedel-session-durability--host) :uid (user-uid)
                      :pid (process-id holder) :process recorded))))))
      (delete-process holder)))
  :doc "inspects another host's lease even with a dead local PID"
  (mevedel-collaboration-lobby-test--should-inspect
   (mevedel-collaboration-lobby-test--open-leased
    (plist-put (mevedel-collaboration-lobby-test--holder-record
                (mevedel-collaboration-lobby-test--dead-pid))
               :host "other-boot/other-machine/pid:[1]")))
  :doc "refuses a view link, leaving even a dead holder's lease untouched"
  (mevedel-collaboration-lobby-test--should-refuse
   (mevedel-collaboration-lobby-test--open-leased
    (mevedel-collaboration-lobby-test--holder-record
     (mevedel-collaboration-lobby-test--dead-pid))
    nil '(:name "Viewer"))
   "A view link cannot open sessions")
  :doc "inspects a same-host lease without a usable PID"
  (dolist (pid '(nil "4242" 0 -1 1.5))
    (mevedel-collaboration-lobby-test--should-inspect
     (mevedel-collaboration-lobby-test--open-leased
      (mevedel-collaboration-lobby-test--holder-record pid))))
  :doc "a competing claim wins without being overwritten by lobby restore"
  (let* (competitor
         (outcome
          (mevedel-collaboration-lobby-test--open-leased
           (mevedel-collaboration-lobby-test--holder-record
            (mevedel-collaboration-lobby-test--dead-pid))
           (lambda (directory expected)
             (setq competitor
                   (plist-put (plist-put (plist-put (copy-sequence expected)
                                                   :generation (1+ (plist-get expected :generation)))
                                        :client-id (make-string 64 ?c))
                              :holder (mevedel-session-durability--holder)))
             (mevedel-session-durability--write-generation directory competitor)))))
    (should (eq t (plist-get (plist-get outcome :reply) :ok)))
    (should-not (plist-get outcome :prompts))
    (should (plist-get outcome :live))
    (should (plist-get outcome :read-only))
    (should (equal competitor (plist-get outcome :head)))))

(mevedel-deftest mevedel-collaboration-lobby--handle-open
  (:doc "hands writable guests the session room at their own tier")
  (let* ((guests (make-hash-table :test #'eql))
         (lobby (list :transport 'transport :guests guests :workspace 'ws))
         (session (mevedel-collaboration-lobby-test--session "s1"))
         (buffer (generate-new-buffer " *lobby-open*"))
         (room '(:link-view "v" :link-full "f" :link-owner "o"))
         sent started)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (puthash 1 '(:name "Viewer") guests)
          (puthash 2 '(:name "Writer" :writable t) guests)
          (puthash 3 '(:name "Owner" :writable t :owner t) guests)
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent) t))
                    ((symbol-function 'mevedel-collaboration-lobby--session-buffer)
                     (lambda (_lobby id)
                       (pcase id
                         ("s1" buffer)
                         ("asks" (signal 'inhibited-interaction nil))
                         (_ (error "No such session")))))
                    ((symbol-function 'mevedel-collaboration--start)
                     (lambda (started-session data-buffer)
                       (setq started (list started-session data-buffer))
                       room)))
            (let ((open (lambda (peer id)
                          (setq sent nil)
                          (mevedel-collaboration-lobby--handle-open
                           lobby peer (list :reqId 7 :id id))
                          (cdar sent))))
              (should (equal '(:t "open-session" :reqId 7 :ok :json-false
                               :message "A view link cannot open sessions")
                             (funcall open 1 "s1")))
              (should-not started)
              (should (equal '(:t "open-session" :reqId 7 :ok t :link "f")
                             (funcall open 2 "s1")))
              (should (equal (list session buffer) started))
              (should (equal "o" (plist-get (funcall open 3 "s1") :link)))
              ;; Nobody may be at the keyboard to answer a question.
              (should (equal "This session needs a decision in Emacs first"
                             (plist-get (funcall open 3 "asks") :message)))
              (should (string-match-p
                       "could not be opened: No such session"
                       (plist-get (funcall open 3 "gone") :message)))
              (should (eq :json-false (plist-get (funcall open 3 "") :ok)))
              ;; Neither an unknown peer nor a malformed request id is
              ;; answered.
              (should-not (funcall open 9 "s1"))
              (setq sent nil)
              (mevedel-collaboration-lobby--handle-open
               lobby 3 '(:reqId "x" :id "s1"))
              (should-not sent))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-collaboration-lobby--handle-delete
  (:doc "deletes an owner's session, closing a live one, and relists it")
  (let* ((guests (make-hash-table :test #'eql))
         (lobby (list :transport 'transport :guests guests :workspace 'ws))
         (live (generate-new-buffer " *lobby-delete-live*"))
         (in-use nil)
         (busy nil)
         (saved '((:save-path "/sessions/old/" :summary (:session-id "old"))
                  (:save-path "/sessions/live/" :summary (:session-id "live"))))
         sent deleted stop-reason)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "live"))
            (add-hook 'kill-buffer-hook
                      (lambda () (setq stop-reason mevedel-collaboration-stop-reason))
                      nil t))
          (puthash 2 '(:name "Writer" :writable t) guests)
          (puthash 3 '(:name "Owner" :writable t :owner t) guests)
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent) t))
                    ((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace)
                       (and (buffer-live-p live) `(("draw" . ,live)))))
                    ((symbol-function 'mevedel-turn-busy-p)
                     (lambda (&optional buffer)
                       (should (eq live buffer))
                       busy))
                    ((symbol-function
                      'mevedel-session-persistence-list-sessions)
                     (lambda (_workspace &optional _cached) saved))
                    ((symbol-function 'mevedel-session-persistence-delete)
                     (lambda (workspace path)
                       (should (eq 'ws workspace))
                       (when (eq in-use 'error) (error "Disk gone"))
                       (unless in-use (push path deleted))))
                    ((symbol-function 'mevedel-collaboration-lobby--listings)
                     (lambda (_lobby) '((:t "lobby") :t "lobby"))))
            (let ((delete (lambda (peer id)
                            (setq sent nil)
                            (mevedel-collaboration-lobby--handle-delete
                             lobby peer (list :reqId 4 :id id))
                            (plist-get (cdar (last sent)) :message))))
              (should (equal "Only an owner link can delete sessions"
                             (funcall delete 2 "old")))
              ;; A running turn keeps its session.
              (setq busy t)
              (should (equal "draw is still working; stop it first"
                             (funcall delete 3 "live")))
              (should (buffer-live-p live))
              (setq busy nil)
              (should (equal "No such session" (funcall delete 3 "../x")))
              (should (equal "No session named" (funcall delete 3 nil)))
              (setq in-use t)
              (should (equal "The session is still in use elsewhere"
                             (funcall delete 3 "old")))
              (setq in-use 'error)
              (should (equal "Session could not be deleted: Disk gone"
                             (funcall delete 3 "old")))
              (should-not deleted)
              (setq in-use nil)
              (funcall delete 3 "old")
              (should (equal '("/sessions/old/") deleted))
              (should (equal '((3 :t "delete-session" :reqId 4 :ok t))
                             (last sent)))
              ;; Every guest's list drops the deleted row.
              (should (equal '(2 3) (sort (mapcar #'car (butlast sent)) #'<)))
              (should (cl-every (lambda (entry)
                                  (equal "lobby" (plist-get (cdr entry) :t)))
                                (butlast sent)))
              ;; A live session is closed, ending its room, before deletion,
              ;; since its buffer would save it straight back.
              (should-not (funcall delete 3 "live"))
              (should-not (buffer-live-p live))
              (should (eq 'session-deleted stop-reason))
              (should (equal '("/sessions/live/" "/sessions/old/") deleted))
              ;; One never saved is gone with its buffer.
              (setq live (generate-new-buffer " *lobby-delete-unsaved*")
                    saved nil deleted nil)
              (with-current-buffer live
                (setq-local mevedel--session
                            (mevedel-collaboration-lobby-test--session "live")))
              (should-not (funcall delete 3 "live"))
              (should-not (buffer-live-p live))
              (should-not deleted))))
      (when (buffer-live-p live) (kill-buffer live)))))

(mevedel-deftest mevedel-collaboration-lobby--on-frame
  (:doc "admits guests, routes lobby frames, and contains failures")
  (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (lobby (list :transport 'transport
                      :guests (make-hash-table :test #'eql)
                      :write-token (make-string 16 ?w)
                      :owner-token (make-string 16 ?o)))
         sent routed interaction)
    (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) sent) t))
              ((symbol-function 'mevedel-collaboration-lobby--listings)
               (lambda (_lobby) '((:t "lobby") :t "lobby")))
              ((symbol-function 'mevedel-model-candidates)
               (lambda () '(("Codex:gpt-6-luna" . provider))))
              ((symbol-function 'mevedel-collaboration-lobby--handle-open)
               (lambda (_lobby peer _frame)
                 (setq interaction inhibit-interaction)
                 (push (cons 'open peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-new-session)
               (lambda (room peer _frame)
                 (should (eq lobby room))
                 (push (cons 'new peer) routed)))
              ((symbol-function 'mevedel-collaboration-lobby--handle-delete)
               (lambda (_lobby peer _frame)
                 (push (cons 'delete peer) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-list)
               (lambda (_lobby peer _frame root)
                 (push (list 'files peer root) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-get)
               (lambda (_lobby peer _frame root)
                 (push (list 'file-get peer root) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-upload)
               (lambda (_lobby peer _frame root)
                 (push (list 'file-upload peer root) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-remove)
               (lambda (_lobby peer _frame root)
                 (push (list 'file-remove peer root) routed)))
              ((symbol-function 'mevedel-collaboration--handle-artifact-get)
               (lambda (_lobby peer _frame) (push (list 'artifact-get peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-artifact-delete)
               (lambda (_lobby peer _frame) (push (list 'artifact-delete peer) routed)))
              ((symbol-function 'mevedel-collaboration-discussions-handle)
               (lambda (_lobby peer _frame) (push (list 'discussion peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-store-list)
               (lambda (_lobby peer _frame) (push (list 'store-list peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-store-action)
               (lambda (_lobby peer _frame) (push (list 'store-action peer) routed))))
      ;; A refresh from a peer that never said hello gets nothing.
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "lobby-refresh"))
      (should-not sent)
      (mevedel-collaboration-lobby--on-frame
       "/root/" 1
       (list :t "hello" :proto mevedel-collaboration--protocol-version
             :name "Phone"
             :writeToken (mevedel-collaboration--base64url
                          (make-string 16 ?w))))
      (should (equal '((1 :t "lobby")) sent))
      (should (plist-get (mevedel-collaboration--guest lobby 1) :writable))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "lobby-refresh"))
      (should (= 2 (length sent)))
      ;; Only an owner can create a session here, so only it learns the
      ;; models to create one on.
      (mevedel-collaboration-lobby--on-frame
       "/root/" 2
       (list :t "hello" :proto mevedel-collaboration--protocol-version
             :name "Owner"
             :writeToken (mevedel-collaboration--base64url
                          (make-string 16 ?w))
             :ownerToken (mevedel-collaboration--base64url
                          (make-string 16 ?o))))
      (should (equal '(2 :t "lobby" :models ["Codex:gpt-6-luna"]) (car sent)))
      (setq sent (cdr sent))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "open-session"))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "new-session"))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "delete-session"))
      (should (equal '((delete . 1) (new . 1) (open . 1)) routed))
      (should (eq t interaction))
      ;; Project file frames reach the files module with the lobby's root.
      (setq routed nil)
      (dolist (type '("files" "file-get" "file-upload" "file-remove"))
        (mevedel-collaboration-lobby--on-frame "/root/" 1 (list :t type)))
      (should (equal '((file-remove 1 "/root/") (file-upload 1 "/root/")
                       (file-get 1 "/root/") (files 1 "/root/"))
                     routed))
      ;; The workspace's artifacts are reached by store identity.
      (setq routed nil)
      (dolist (type '("artifact-get" "artifact-delete" "discussion"
                      "store-list" "store-action"))
        (mevedel-collaboration-lobby--on-frame "/root/" 1 (list :t type)))
      (should (equal '((store-action 1) (store-list 1) (discussion 1)
                       (artifact-delete 1) (artifact-get 1))
                     routed))
      (setq routed '((new . 1) (open . 1)))
      ;; Another workspace's frames never reach this lobby.
      (mevedel-collaboration-lobby--on-frame "/other/" 1 '(:t "open-session"))
      (should (= 2 (length routed)))
      ;; A failing handler warns the host and tells only its sender, and
      ;; the lobby keeps working.
      (setq sent nil)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby--handle-open)
                 (lambda (&rest _) (error "Boom"))))
        (let (captured)
          (mevedel-test--with-captured-diagnostics captured
            (mevedel-collaboration-lobby--on-frame
             "/root/" 1 '(:t "open-session")))
          (should (string-search "A guest's open-session frame failed: Boom"
                                 captured))))
      (should (equal '((1 :t "notice"
                          :message "The host could not handle this action"))
                     sent))
      (should (eq lobby (gethash "/root/"
                                 mevedel-collaboration-lobby--lobbies))))))

(mevedel-deftest mevedel-collaboration-lobby--on-control
  (:doc "forgets a guest that left")
  (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (guests (make-hash-table :test #'eql)))
    (puthash "/root/" (list :guests guests)
             mevedel-collaboration-lobby--lobbies)
    (puthash 1 '(:name "a") guests)
    (puthash 2 '(:name "b") guests)
    (mevedel-collaboration-lobby--on-control "/root/" 'peer-joined 3)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1234)))
      (mevedel-collaboration-lobby--on-control "/root/" 'peer-left 1))
    (let ((lobby (gethash "/root/" mevedel-collaboration-lobby--lobbies)))
      (should (= 1234 (plist-get lobby :last-guest-departure)))
      (mevedel-collaboration-lobby--on-control "/root/" 'peer-left 99)
      (should (= 1234 (plist-get lobby :last-guest-departure))))
    (should (equal '(2) (hash-table-keys guests)))))

(mevedel-deftest mevedel-collaboration-lobby--on-state
  (:doc "drops every guest when the relay connection goes down")
  (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (guests (make-hash-table :test #'eql)))
    (puthash "/root/" (list :guests guests)
             mevedel-collaboration-lobby--lobbies)
    (puthash 1 '(:name "a") guests)
    (mevedel-collaboration-lobby--on-state "/root/" 'open)
    (should (= 1 (hash-table-count guests)))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1234)))
      (mevedel-collaboration-lobby--on-state "/root/" 'down))
    (let ((lobby (gethash "/root/" mevedel-collaboration-lobby--lobbies)))
      (should (= 1234 (plist-get lobby :last-guest-departure)))
      (mevedel-collaboration-lobby--on-state "/root/" 'down)
      (should (= 1234 (plist-get lobby :last-guest-departure))))
    (should (= 0 (hash-table-count guests)))))

(mevedel-deftest mevedel-collaboration-lobby--workspace
  (:doc "refuses a directory that does not exist")
  (should-error (mevedel-collaboration-lobby--workspace
                 "/nonexistent/mevedel-lobby/")
                :type 'user-error))

(mevedel-deftest mevedel-collaboration-lobby-live-p
  (:doc "reports only a live lobby of the given workspace")
  (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
        (workspace (mevedel-workspace--create :root "/root/"))
        (other (mevedel-workspace--create :root "/other/")))
    (should-not (mevedel-collaboration-lobby-live-p nil))
    (should-not (mevedel-collaboration-lobby-live-p workspace))
    (puthash "/root/" (list :project "root") mevedel-collaboration-lobby--lobbies)
    (should (mevedel-collaboration-lobby-live-p workspace))
    (should-not (mevedel-collaboration-lobby-live-p other))))

(mevedel-deftest mevedel-collaboration-lobby-start
  (:doc "starts one lobby per workspace whose links survive a restart")
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (mevedel-collaboration-relay-url "wss://relay.example")
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace root))
          (sleep-updates 0)
          opened stopped announced)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby--workspace)
                 (lambda (_directory) workspace))
                ((symbol-function 'mevedel-sleep-update)
                 (lambda () (cl-incf sleep-updates)))
                ((symbol-function 'mevedel-collaboration--announce-lobby)
                 (lambda (announced-workspace)
                   (push (and (mevedel-collaboration-lobby--find announced-workspace) t)
                         announced)))
                ((symbol-function 'mevedel-collaboration--transport-open)
                 (lambda (url _key &rest _callbacks)
                   (push url opened)
                   (list :url url)))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 (lambda (transport) (push transport stopped)))
                ((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (&rest _) t)))
        (let ((lobby (mevedel-collaboration-lobby-start root)))
          (should (= 1 sleep-updates))
          (should (numberp (plist-get lobby :started-at)))
          (should-not (plist-get lobby :last-guest-departure))
          (should (eq lobby (mevedel-collaboration-lobby--find workspace)))
          (should (equal "Lobby: proj" (plist-get lobby :session-label)))
          (should (equal root (plist-get lobby :directory)))
          (should (string-match-p
                   "\\`https://relay.example/#[A-Za-z0-9_-]+\\."
                   (plist-get lobby :link-owner)))
          (should (equal (list (format "wss://relay.example/r/%s?role=host"
                                       (plist-get lobby :room-id)))
                         opened))
          ;; The workspace's rooms hear that it runs, once it does.
          (should (equal '(t) announced))
          (should (memq #'mevedel-collaboration-lobby--stop-all
                        kill-emacs-hook))
          ;; It runs until stopped, so the next Emacs restarts it.
          (should (equal (list root)
                         (mevedel-collaboration-lobby--intended)))
          ;; A second start is the same lobby, not a second room.
          (should (eq lobby (mevedel-collaboration-lobby-start root)))
          (should (= 1 sleep-updates))
          (should (= 1 (length opened)))
          (mevedel-collaboration-lobby--stop lobby 'user-stop)
          (should-not (mevedel-collaboration-lobby--find workspace))
          ;; And that it stopped, once it has.
          (should (equal '(nil t) announced))
          (should-not (mevedel-collaboration-lobby--intended))
          (should-not kill-emacs-hook)
          ;; Restarting revives the very same links.
          (let ((again (mevedel-collaboration-lobby-start root)))
            (should (= 2 sleep-updates))
            (should-not (eq lobby again))
            (should (equal (plist-get lobby :link-owner)
                           (plist-get again :link-owner)))
            (mevedel-collaboration-lobby--stop again 'user-stop))
          (should (= 2 (length stopped))))))))

(mevedel-deftest mevedel-collaboration-lobby--stop
  ()
  ,test
  (test)
  :doc "says goodbye to connected guests except when Emacs exits"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (kill-emacs-hook (list #'mevedel-collaboration-lobby--stop-all))
          (workspace (mevedel-collaboration-lobby-test--workspace "/root/"))
          sent stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent) t))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 (lambda (transport) (push transport stopped))))
        (let ((lobby (mevedel-collaboration-lobby-test--lobby workspace)))
          (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
          (mevedel-collaboration-lobby--stop lobby 'user-stop)
          ;; Nobody connected, nobody told.
          (should-not sent)
          (should (equal '(transport) stopped))
          (should-not kill-emacs-hook))
        (let ((lobby (mevedel-collaboration-lobby-test--lobby workspace)))
          (puthash 1 '(:name "a") (plist-get lobby :guests))
          (mevedel-collaboration-lobby--stop lobby 'rotated)
          (should (equal '((0 :t "bye" :reason "rotated")) sent))
          (setq sent nil)
          ;; Exiting tells nobody: the lobby restarts with the next Emacs.
          (let (announced)
            (cl-letf (((symbol-function 'mevedel-collaboration--announce-lobby)
                       (lambda (workspace) (push workspace announced))))
              (mevedel-collaboration-lobby--stop lobby 'emacs-exit))
            (should-not announced))
          (should-not sent)))))

  :doc "only an Emacs exit leaves the lobby recorded to restart"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace "/root/")))
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-stop)
                 #'ignore))
        (mevedel-collaboration-lobby--set-intended "/root/" t)
        (mevedel-collaboration-lobby--stop
         (mevedel-collaboration-lobby-test--lobby workspace) 'emacs-exit)
        (should (equal '("/root/") (mevedel-collaboration-lobby--intended)))
        (mevedel-collaboration-lobby--stop
         (mevedel-collaboration-lobby-test--lobby workspace) 'user-stop)
        (should-not (mevedel-collaboration-lobby--intended))))))

(mevedel-deftest mevedel-collaboration-lobby--stop-all
  (:doc "stops every live lobby and keeps them recorded to restart")
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (kill-emacs-hook nil)
          stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-stop)
                 (lambda (transport) (push transport stopped))))
        (dolist (root '("/a/" "/b/"))
          (mevedel-collaboration-lobby--set-intended root t)
          (puthash root
                   (mevedel-collaboration-lobby-test--lobby
                    (mevedel-collaboration-lobby-test--workspace root)
                    :transport root)
                   mevedel-collaboration-lobby--lobbies))
        (mevedel-collaboration-lobby--stop-all)
        (should (= 0 (hash-table-count mevedel-collaboration-lobby--lobbies)))
        (should (equal '("/a/" "/b/") (sort stopped #'string<)))
        (should (equal '("/a/" "/b/")
                       (mevedel-collaboration-lobby--intended)))))))

(mevedel-deftest mevedel-collaboration-lobby--status
  (:doc "reports each lobby without its secrets")
  (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal)))
    (should-not (mevedel-collaboration-lobby--status))
    (let ((lobby (mevedel-collaboration-lobby-test--lobby
                  (mevedel-collaboration-lobby-test--workspace "/root/")
                  :session-label "Lobby: proj")))
      (puthash 1 '(:name "a") (plist-get lobby :guests))
      (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-open-p)
                 (lambda (_transport) t)))
        (should (equal "Lobby: proj: relay connected; 1 guest; Keep-awake request: active"
                       (mevedel-collaboration-lobby--status)))))))

(mevedel-deftest mevedel-collaboration-lobby--intent-path
  (:doc "keeps the running lobbies in the user directory")
  (let ((mevedel-user-dir "/home/user/.mevedel/"))
    (should (equal "/home/user/.mevedel/lobbies.el"
                   (mevedel-collaboration-lobby--intent-path)))))

(mevedel-deftest mevedel-collaboration-lobby--intended
  (:doc "reads the recorded roots and ignores a damaged record")
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((path (mevedel-collaboration-lobby--intent-path)))
      (should-not (mevedel-collaboration-lobby--intended))
      (with-temp-file path (insert "(\"/a/\" 7 \"/b/\")"))
      (should (equal '("/a/" "/b/") (mevedel-collaboration-lobby--intended)))
      (with-temp-file path (insert "(\"/a/\""))
      (should-not (mevedel-collaboration-lobby--intended))
      (with-temp-file path (insert "(\"/a/\" . \"/b/\")"))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby--set-intended
  ()
  ,test
  (test)
  :doc "adds each root once and removes the record with the last one"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((path (mevedel-collaboration-lobby--intent-path)))
      (mevedel-collaboration-lobby--set-intended "/a/" t)
      (mevedel-collaboration-lobby--set-intended "/b/" t)
      (mevedel-collaboration-lobby--set-intended "/a/" t)
      (should (equal '("/a/" "/b/") (mevedel-collaboration-lobby--intended)))
      (mevedel-collaboration-lobby--set-intended "/a/" nil)
      (should (equal '("/b/") (mevedel-collaboration-lobby--intended)))
      (mevedel-collaboration-lobby--set-intended "/b/" nil)
      (should-not (file-exists-p path))
      ;; Forgetting what was never recorded writes nothing.
      (mevedel-collaboration-lobby--set-intended "/c/" nil)
      (should-not (file-exists-p path))))

  :doc "reports a record it cannot write instead of failing the lobby"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-user-dir (file-name-concat mevedel-user-dir "blocked/"))
          captured)
      ;; A file where the directory should be makes the write fail.
      (with-temp-file (directory-file-name mevedel-user-dir))
      (mevedel-test--with-captured-diagnostics captured
        (mevedel-collaboration-lobby--set-intended "/a/" t))
      (should (string-match-p "Could not record the lobby of /a/" captured))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby-restore
  ()
  ,test
  (test)
  :doc "restarts each recorded lobby and reports one that fails"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((broken (file-name-as-directory
                   (file-name-concat root "broken")))
          started captured)
      (make-directory broken)
      (mevedel-collaboration-lobby--set-intended broken t)
      (mevedel-collaboration-lobby--set-intended root t)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby-start)
                 (lambda (directory)
                   (push directory started)
                   (when (equal directory broken)
                     (user-error "No mevedel workspace at %s" directory)))))
        (mevedel-test--with-captured-diagnostics captured
          (mevedel-collaboration-lobby-restore)))
      (should (equal (list root broken) started))
      (should (string-match-p
               (regexp-quote (format "Lobby of %s not restarted" broken))
               captured))
      ;; A failed restart is retried with the next Emacs.
      (should (equal (list broken root)
                     (mevedel-collaboration-lobby--intended)))))

  :doc "an unavailable remote root cannot prompt or prevent later lobbies from restarting"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((remote "/ssh:unavailable:/project/")
          started captured
          (directory-p (symbol-function 'file-directory-p)))
      (mevedel-collaboration-lobby--set-intended remote t)
      (mevedel-collaboration-lobby--set-intended root t)
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (directory)
                   (if (equal directory remote)
                       (progn
                         (should inhibit-interaction)
                         (signal 'inhibited-interaction '("Remote authentication needed")))
                     (funcall directory-p directory))))
                ((symbol-function 'mevedel-collaboration-lobby-start)
                 (lambda (directory) (push directory started))))
        (mevedel-test--with-captured-diagnostics captured
          (mevedel-collaboration-lobby-restore)))
      (should (equal (list root) started))
      (should (string-match-p "not restarted" captured))
      (should (equal (list remote root) (mevedel-collaboration-lobby--intended)))))

  :doc "forgets a lobby whose directory is gone"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((gone (file-name-as-directory (file-name-concat root "gone")))
          started captured)
      (mevedel-collaboration-lobby--set-intended gone t)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby-start)
                 (lambda (directory) (push directory started))))
        (mevedel-test--with-captured-diagnostics captured
          (mevedel-collaboration-lobby-restore)))
      (should-not started)
      (should (string-match-p "forgotten" captured))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby-stop
  ()
  ,test
  (test)
  :doc "stops the running lobby so it does not restart with Emacs"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (mevedel-collaboration-relay-url "wss://relay.example")
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace root)))
      (cl-letf (((symbol-function 'mevedel-workspace)
                 (lambda (&optional _buffer) workspace))
                ((symbol-function 'mevedel-collaboration-lobby--workspace)
                 (lambda (_directory) workspace))
                ((symbol-function 'mevedel-collaboration--transport-open)
                 (lambda (url &rest _) (list :url url)))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 #'ignore))
        (mevedel-collaboration-lobby-start root)
        (mevedel-test--with-captured-messages nil
          (mevedel-collaboration-lobby-stop))
        (should-not (mevedel-collaboration-lobby--find workspace))
        (should-not (mevedel-collaboration-lobby--intended)))))

  :doc "ends the retries of a lobby that failed to restart"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (workspace (mevedel-collaboration-lobby-test--workspace root))
          captured)
      (mevedel-collaboration-lobby--set-intended root t)
      (cl-letf (((symbol-function 'mevedel-workspace)
                 (lambda (&optional _buffer) workspace)))
        (mevedel-test--with-captured-messages captured
          (mevedel-collaboration-lobby-stop)))
      (should (string-match-p "no lobby is running" captured))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby-rotate
  (:doc "replaces stored credentials and restarts a running lobby")
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (mevedel-collaboration-relay-url "wss://relay.example")
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace root))
          presented opened)
      (cl-letf (((symbol-function 'mevedel-workspace)
                 (lambda (&optional _buffer) workspace))
                ((symbol-function 'mevedel-collaboration-lobby--workspace)
                 (lambda (_directory) workspace))
                ((symbol-function 'mevedel-collaboration--transport-open)
                 (lambda (url &rest _) (push url opened) (list :url url)))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 #'ignore)
                ((symbol-function 'mevedel-collaboration-share-present)
                 (lambda (lobby) (push lobby presented))))
        (let ((before (mevedel-collaboration-lobby--credentials workspace)))
          (mevedel-test--with-captured-messages nil
            (mevedel-collaboration-lobby-rotate))
          (should-not presented)
          (should-not (equal before (mevedel-collaboration-lobby--credentials
                                     workspace))))
        (let* ((running (mevedel-collaboration-lobby-start root))
               (old-link (plist-get running :link-owner)))
          (mevedel-collaboration-lobby-rotate)
          (should (= 1 (length presented)))
          (should-not (eq running (car presented)))
          (should-not (equal old-link (plist-get (car presented) :link-owner)))
          (should (= 2 (length opened)))
          (mevedel-collaboration-lobby--stop (car presented) 'user-stop))))))

(defmacro mevedel-collaboration-lobby-test--with-stubbed-transport (&rest body)
  "Run BODY with lobby transports that record stops instead of closing.
The list of stopped transports is bound to `stopped'."
  (declare (indent 0))
  `(let (stopped)
     (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                (lambda (&rest _) t))
               ((symbol-function 'mevedel-collaboration--transport-stop)
                (lambda (transport) (push transport stopped)))
               ((symbol-function 'mevedel-collaboration--transport-open-p)
                (lambda (transport) (eq transport 'transport)))
               ((symbol-function 'mevedel-collaboration-presence-publish)
                #'ignore)
               ((symbol-function 'mevedel-collaboration-share-dismiss)
                #'ignore))
       ,@body)))

(mevedel-deftest mevedel-collaboration-lobby-shares
  (:doc "lists running lobbies, then recorded ones that are not running, without links")
  (mevedel-collaboration-lobby-test--with-user-dir
    (mevedel-collaboration-lobby-test--with-stubbed-transport
      (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
             (workspace (mevedel-collaboration-lobby-test--workspace "/root/"))
             (lobby (mevedel-collaboration-lobby-test--lobby workspace)))
        (should-not (mevedel-collaboration-lobby-shares))
        (puthash 1 (list :name "Phone") (plist-get lobby :guests))
        (puthash 2 (list :name "Laptop") (plist-get lobby :guests))
        (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
        (mevedel-collaboration-lobby--set-intended "/root/" t)
        (mevedel-collaboration-lobby--set-intended "/srv/excal/" t)
        (let ((shares (mevedel-collaboration-lobby-shares)))
          (should (equal (list (list :kind 'lobby :label "proj" :guests 2
                                     :connected t :directory "/root/")
                               (list :kind 'recorded-lobby :label "excal"
                                     :guests 0 :connected nil
                                     :directory "/srv/excal/"))
                         shares))
          (should-not (string-search "link" (format "%S" shares))))))))

(mevedel-deftest mevedel-collaboration-lobby-show-directory
  (:doc "shows a running lobby and restarts a recorded one only when asked")
  (mevedel-collaboration-lobby-test--with-user-dir
    (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (workspace (mevedel-collaboration-lobby-test--workspace "/root/"))
           (lobby (mevedel-collaboration-lobby-test--lobby workspace))
           presented started)
      (cl-letf (((symbol-function 'mevedel-collaboration-share-present)
                 (lambda (shown) (push shown presented)))
                ((symbol-function 'mevedel-collaboration-lobby-start)
                 (lambda (root) (push root started) lobby)))
        (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
        (mevedel-collaboration-lobby-show-directory "/root/")
        (should (equal (list lobby) presented))
        (clrhash mevedel-collaboration-lobby--lobbies)
        ;; Stopped meanwhile: never restarted behind the user's back.
        (should-error (mevedel-collaboration-lobby-show-directory "/root/")
                      :type 'user-error)
        ;; Restart needs the recording, which is the earlier consent.
        (should-error (mevedel-collaboration-lobby-show-directory "/root/" t)
                      :type 'user-error)
        (should-not started)
        (mevedel-collaboration-lobby--set-intended "/root/" t)
        (mevedel-collaboration-lobby-show-directory "/root/" t)
        (should (equal '("/root/") started))
        (should (equal (list lobby lobby) presented))))))

(mevedel-deftest mevedel-collaboration-lobby-stop-directory
  (:doc "stops a running lobby and forgets a recorded one, so neither restarts")
  (mevedel-collaboration-lobby-test--with-user-dir
    (mevedel-collaboration-lobby-test--with-stubbed-transport
      (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
             (workspace (mevedel-collaboration-lobby-test--workspace "/root/"))
             (lobby (mevedel-collaboration-lobby-test--lobby workspace))
             messages)
        (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
        (mevedel-collaboration-lobby--set-intended "/root/" t)
        (mevedel-collaboration-lobby--set-intended "/srv/excal/" t)
        (mevedel-test--with-captured-messages messages
          (mevedel-collaboration-lobby-stop-directory "/root/")
          (mevedel-collaboration-lobby-stop-directory "/srv/excal/"))
        (should (equal '(transport) stopped))
        (should (zerop (hash-table-count mevedel-collaboration-lobby--lobbies)))
        (should-not (mevedel-collaboration-lobby--intended))
        (should (string-match-p "lobby stopped for proj" messages))
        (should (string-match-p "/srv/excal/ no longer restarts" messages))))))

(mevedel-deftest mevedel-collaboration-lobby-stop-every
  (:doc "stops every running lobby and forgets recorded ones, counting both")
  (mevedel-collaboration-lobby-test--with-user-dir
    (mevedel-collaboration-lobby-test--with-stubbed-transport
      (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
             (workspace (mevedel-collaboration-lobby-test--workspace "/root/"))
             (lobby (mevedel-collaboration-lobby-test--lobby workspace)))
        (should (zerop (mevedel-collaboration-lobby-stop-every)))
        (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
        (mevedel-collaboration-lobby--set-intended "/root/" t)
        (mevedel-collaboration-lobby--set-intended "/srv/excal/" t)
        (should (= 2 (mevedel-collaboration-lobby-stop-every)))
        (should (equal '(transport) stopped))
        (should (zerop (hash-table-count mevedel-collaboration-lobby--lobbies)))
        (should-not (mevedel-collaboration-lobby--intended))
        (should-not (mevedel-collaboration-lobby-shares))))))

(defmacro mevedel-collaboration-lobby-test--with-relay (&rest body)
  "Start a lobby and run BODY with the relay frame and lifecycle callbacks."
  (declare (indent 0))
  `(mevedel-collaboration-lobby-test--with-root root
     (let ((workspace (mevedel-collaboration-lobby-test--workspace root))
           (mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (mevedel-collaboration-relay-url "wss://relay.example")
           (kill-emacs-hook nil) callbacks controls replies recipients lobby)
       (cl-letf (((symbol-function 'mevedel-workspace) (lambda (&optional _) workspace))
                 ((symbol-function 'mevedel-collaboration--transport-open)
                  (lambda (_url _key &rest options) (setq callbacks options) 'relay))
                 ((symbol-function 'mevedel-collaboration--transport-control)
                  (lambda (_transport frame) (push frame controls)))
                 ((symbol-function 'mevedel-collaboration--transport-send)
                  (lambda (_transport peer frame) (push peer recipients) (push frame replies)))
                 ((symbol-function 'mevedel-collaboration--transport-stop) #'ignore))
         (setq lobby (mevedel-collaboration-lobby-start root))
         (cl-flet* ((send (peer frame) (funcall (plist-get callbacks :on-frame) peer frame))
                    (hello (peer id tier)
                      (send peer (list :t "hello" :proto mevedel-collaboration--protocol-version
                                       :guestId id :writeToken (unless (eq tier 'view)
                                                                 (mevedel-collaboration--base64url
                                                                  (plist-get lobby :write-token)))
                                       :ownerToken (when (eq tier 'owner)
                                                     (mevedel-collaboration--base64url
                                                      (plist-get lobby :owner-token)))))))
           ,@body)))))

(mevedel-deftest mevedel-collaboration-lobby/push-tiers
  (:doc "only authenticated full and owner guests change their browser subscription")
  (mevedel-collaboration-lobby-test--with-relay
    (let ((subscribe '(:t "push-subscribe" :guestId "forged-id" :endpoint "https://push.test/a" :active t)))
      (send 1 subscribe)
      (hello 1 "browser-a" 'view)
      (send 1 subscribe)
      (send 1 '(:t "push-unsubscribe"))
      (send 1 '(:t "push-state" :active t))
      (should-not controls)
      (dolist (tier '(full owner))
        (hello 1 "browser-a" tier)
        (send 1 '(:t "push-subscribe" :endpoint "http://invalid.test"))
        (should-not controls)
        (send 1 subscribe)
        (send 1 '(:t "push-state" :active :json-false))
        (send 1 '(:t "push-unsubscribe" :guestId "forged-id"))
        (should (equal (nreverse controls)
                       '((:t "push-subscribe" :peer 1 :guestId "browser-a" :endpoint "https://push.test/a" :active t)
                         (:t "push-state" :peer 1 :guestId "browser-a" :active :json-false)
                         (:t "push-unsubscribe" :guestId "browser-a"))))
        (setq controls nil)))))

(mevedel-deftest mevedel-collaboration-lobby/push-restore
  (:doc "only current opt-ins replay inactive, after relay reconnect and after host restart")
  (mevedel-collaboration-lobby-test--with-relay
    (hello 1 "browser-a" 'full)
    (send 1 '(:t "push-subscribe" :endpoint "https://push.test/old"))
    (send 1 '(:t "push-subscribe" :endpoint "https://push.test/new" :active t))
    (hello 2 "browser-b" 'owner)
    (send 2 '(:t "push-subscribe" :endpoint "https://push.test/b"))
    (send 2 '(:t "push-unsubscribe"))
    (dolist (event '(reconnect restart))
      (setq controls nil)
      (pcase event
        ('reconnect (funcall (plist-get callbacks :on-state) 'down))
        ('restart (mevedel-collaboration-lobby--stop-all)
                  (mevedel-collaboration-lobby-restore)))
      (funcall (plist-get callbacks :on-state) 'open)
      (should (equal controls '((:t "push-subscribe" :peer 0 :guestId "browser-a"
                                 :endpoint "https://push.test/new" :active :json-false)))))))

(mevedel-deftest mevedel-collaboration-lobby/push-rotate
  (:doc "rotating credentials discards even an offline phone's stored endpoint")
  (mevedel-collaboration-lobby-test--with-relay
    (hello 1 "browser-a" 'full)
    (send 1 '(:t "push-subscribe" :endpoint "https://push.test/private"))
    (let ((store (car (directory-files (mevedel-workspace-state-dir workspace) t "^lobby-push-"))))
      (should (= #o600 (file-modes store)))
      (setq controls nil)
      (cl-letf (((symbol-function 'mevedel-collaboration-share-present) #'ignore))
        (mevedel-collaboration-lobby-rotate))
      (funcall (plist-get callbacks :on-state) 'open)
      (should-not controls)
      (should-not (file-exists-p store)))))

(mevedel-deftest mevedel-collaboration-lobby/push-write-failure
  (:doc "an unsuccessful unsubscribe is reported, not forwarded or forgotten")
  (mevedel-collaboration-lobby-test--with-relay
    (hello 1 "browser-a" 'owner)
    (send 1 '(:t "push-subscribe" :endpoint "https://push.test/a"))
    (let* ((dir (directory-file-name (mevedel-workspace-state-dir workspace)))
           (backup (concat dir ".offline")))
      (rename-file dir backup)
      (with-temp-file dir)
      (setq controls nil replies nil)
      (send 1 '(:t "push-unsubscribe"))
      (should (equal '(:t "notice" :message "The host could not handle this action") (car replies)))
      (should-not controls)
      (delete-file dir)
      (rename-file backup dir)
      (funcall (plist-get callbacks :on-state) 'open)
      (should (equal "https://push.test/a" (plist-get (car controls) :endpoint))))))

(mevedel-deftest mevedel-collaboration-lobby/push-invalid-store
  (:doc "a damaged or oversized store leaves a reachable lobby where phones can re-enroll")
  (mevedel-collaboration-lobby-test--with-relay
    (hello 1 "browser-a" 'full)
    (send 1 '(:t "push-subscribe" :endpoint "https://push.test/a"))
    (let ((store (car (directory-files (mevedel-workspace-state-dir workspace) t "^lobby-push-"))))
      (dolist (bad (list "{broken" (make-string (* 301 1024) ?x)
                         "{\"browser-a\":\"http://invalid\"}"))
        (mevedel-collaboration-lobby--stop-all)
        (with-temp-file store (insert bad))
        (setq controls nil replies nil)
        (let (warning)
          (mevedel-test--with-captured-diagnostics warning (mevedel-collaboration-lobby-restore))
          (should (string-search "quarantined" warning)))
        (funcall (plist-get callbacks :on-state) 'open)
        (should-not controls)
        (hello 1 "browser-a" 'full)
        (should (seq-find (lambda (frame) (equal "lobby" (plist-get frame :t))) replies))
        (send 1 '(:t "push-unsubscribe"))
        (send 1 '(:t "push-subscribe" :endpoint "https://push.test/new"))
        (should (equal "https://push.test/new" (plist-get (car controls) :endpoint)))
        (should (seq-find (lambda (file)
                            (and (= #o600 (file-modes file))
                                 (equal bad (mevedel-collaboration-lobby-test--file-bytes file))))
                          (directory-files (file-name-directory store) t "\\.damaged-")))))))

(mevedel-deftest mevedel-collaboration-lobby/push-unsafe-store
  (:doc "an unsafe store stays untouched while non-push lobby actions remain available")
  (mevedel-collaboration-lobby-test--with-relay
    (hello 1 "browser-a" 'full)
    (send 1 '(:t "push-subscribe" :endpoint "https://push.test/a"))
    (let* ((store (car (directory-files (mevedel-workspace-state-dir workspace) t "^lobby-push-")))
           (target (concat store ".original")) warning)
      (mevedel-collaboration-lobby--stop-all)
      (rename-file store target)
      (make-symbolic-link target store)
      (mevedel-test--with-captured-diagnostics warning (mevedel-collaboration-lobby-restore))
      (should (string-search "disabled" warning))
      (setq controls nil replies nil)
      (funcall (plist-get callbacks :on-state) 'open)
      (hello 2 "browser-b" 'full)
      (send 2 '(:t "push-subscribe" :endpoint "https://push.test/b"))
      (should (equal "push-refused" (plist-get (car replies) :kind)))
      (should-not controls)
      (send 2 '(:t "lobby-refresh"))
      (should (equal "lobby" (plist-get (car replies) :t)))
      (should (equal target (file-symlink-p store)))
      (should (equal "{\"browser-a\":\"https://push.test/a\"}"
                     (mevedel-collaboration-lobby-test--file-bytes target))))))

(mevedel-deftest mevedel-collaboration-lobby/push-full
  (:doc "a full relay room rejects new devices without evicting existing ones")
  (mevedel-collaboration-lobby-test--with-relay
    (dotimes (i 64)
      (hello 1 (format "browser-%d" i) 'owner)
      (send 1 (list :t "push-subscribe" :endpoint (format "https://push.test/%d" i))))
    (should (= 64 (length controls)))
    (hello 2 "browser-extra" 'full)
    (setq controls nil replies nil recipients nil)
    (send 2 '(:t "push-subscribe" :endpoint "https://push.test/extra"))
    (should-not controls)
    (should (equal '(2) recipients))
    (should (equal '((:t "notice" :kind "push-refused"
                      :message "Lobby notifications are full; unsubscribe another device and try again."))
                   replies))
    (send 1 '(:t "push-subscribe" :endpoint "https://push.test/new"))
    (should (equal "https://push.test/new" (plist-get (car controls) :endpoint)))
    (mevedel-collaboration-lobby--stop-all)
    (mevedel-collaboration-lobby-restore)
    (setq controls nil)
    (funcall (plist-get callbacks :on-state) 'open)
    (should (equal (sort (mapcar (lambda (frame) (plist-get frame :guestId)) controls) #'string<)
                   (sort (cl-loop for i below 64 collect (format "browser-%d" i)) #'string<)))))

(mevedel-deftest mevedel-collaboration-lobby/schedule-tiers
  (:doc "each admitted tier receives only the workspace schedule fields it may see")
  (mevedel-collaboration-lobby-test--with-relay
    (cl-letf (((symbol-function 'mevedel-workspace-schedule-list)
               (lambda (ws)
                 (should (eq ws workspace))
                 '((:id "s-ab1234" :name "Private job" :timing (:kind cron :expression "0 9 * * 1")
                    :due nil :status paused :last-outcome skipped :prompt "secret prompt"
                    :policy "private policy" :last-error "private error" :session-id "private session")))))
      (dolist (tier '(view full owner))
        (hello 1 "browser-a" tier)
        (let ((listing (seq-find (lambda (frame) (equal "lobby" (plist-get frame :t))) replies)))
          (should (equal (plist-get listing :schedules)
                         (vector (append '(:id "s-ab1234" :timing "0 9 * * 1" :nextRun nil
                                           :status "paused" :lastOutcome "skipped")
                                         (unless (eq tier 'view) '(:name "Private job"))))))
          (setq replies nil))))))

(mevedel-deftest mevedel-collaboration-lobby/inaccessible-schedule-store
  (:doc "An inaccessible schedule store does not hide sessions or presence from a guest.")
  (mevedel-collaboration-lobby-test--with-relay
    (let ((store (file-name-concat (mevedel-workspace-state-dir workspace) "schedules")) warning diagnostic)
      (make-directory (file-name-directory store) t)
      (write-region "not a directory" nil store nil 'silent)
      (let ((cause (error-message-string (should-error (mevedel-workspace-schedule-list workspace)))))
        (with-temp-buffer
          (setq-local mevedel--session (mevedel-collaboration-lobby-test--session "live"))
          (setf (mevedel-session-workspace mevedel--session) workspace)
          (dolist (request '(view owner refresh))
            (setq replies nil)
            (setq warning nil)
            (mevedel-test--with-captured-messages diagnostic
              (cl-letf (((symbol-function 'display-warning)
                         (lambda (_type text &rest _) (setq warning text))))
                (pcase request
                  ('view (hello 1 "viewer" 'view))
                  ('owner (hello 2 "owner" 'owner))
                  ('refresh (send 1 '(:t "lobby-refresh"))))))
            (let ((listing (seq-find (lambda (f) (equal "lobby" (plist-get f :t))) replies)))
              (should (equal "live" (plist-get (aref (plist-get listing :sessions) 0) :id)))
              (should-not (plist-member listing :schedules)))
            (unless (eq request 'refresh)
              (should (seq-find (lambda (f) (equal "presence" (plist-get f :t))) replies)))
            (if (eq request 'view)
                (should (string-search cause warning))
              (should-not warning)
              (should (string-search cause diagnostic)))))
        (setq replies nil)
        (mevedel-test--with-captured-diagnostics warning
          (send 2 '(:t "schedule-action" :reqId 42 :id "s-ab1234" :action "pause")))
        (should (equal replies '((:t "schedule-action" :reqId 42 :ok :json-false
                                    :message "Schedule could not be changed"))))
        (should (string-search cause warning))))))

(mevedel-deftest mevedel-collaboration-lobby/schedule-action
  (:doc "only an authenticated owner can change a current schedule in this workspace")
  (mevedel-collaboration-lobby-test--with-relay
    (let* ((key (plist-get lobby :key)) (id "s-ab1234") (seq 0) challenges
           (wire (append (list :key key :peers (make-hash-table)) callbacks))
           (mevedel-schedule--workspaces nil) (mevedel-schedule--timer nil)
           (now (mevedel-schedule-now))
           (job (list :id id :name "Private" :prompt "secret" :preset 'mevedel-implement
                      :policy '(:model-provider "test:model" :reasoning-effort nil :permission-mode full-auto)
                      :timing '(:kind interval :seconds 900) :overlap 'skip
                      :created-at now :due (+ now 900) :status 'active)))
      (cl-labels
          ((sealed (peer frame)
             (let ((text (json-encode (append frame (list :challenge (alist-get peer challenges)
                                                          :seq (cl-incf seq) :nonce "browser")))))
               (mevedel-collaboration--transport-receive
                wire (make-websocket-frame :opcode 'binary :completep t
                      :payload (mevedel-collaboration--envelope-pack
                                peer (mevedel-collaboration--seal key text))))))
           (action (peer target verb &optional req)
             (setq replies nil recipients nil)
             (sealed peer (list :t "schedule-action" :reqId (or req 32) :id target :action verb
                                :workspace "/other" :path "../other" :owner t))
             (car replies))
           (rows (status &optional outcome)
             (let (seen)
               (cl-loop for peer in recipients for frame in replies when (equal "lobby" (plist-get frame :t))
                        do (let ((row (aref (plist-get frame :schedules) 0)))
                             (should (equal id (plist-get row :id)))
                             (should (equal outcome (plist-get row :lastOutcome)))
                             (should (equal status (plist-get row :status))) (push peer seen)))
               (should (equal '(1 2 3) (sort seen #'<))))))
        (unwind-protect
            (progn
              (dolist (peer '(1 2 3))
                (mevedel-collaboration--transport-receive
                 wire (make-websocket-frame :opcode 'text :completep t :payload
                       (json-encode (list :t "peer-joined" :peer peer))))
                (push (cons peer (plist-get (car replies) :challenge)) challenges)
                (should-not (action peer id "pause"))
                (sealed peer (list :t "hello" :proto mevedel-collaboration--protocol-version
                                   :writeToken (when (> peer 1) (mevedel-collaboration--base64url
                                                                 (plist-get lobby :write-token)))
                                   :ownerToken (when (= peer 3) (mevedel-collaboration--base64url
                                                                 (plist-get lobby :owner-token))))))
              (setq recipients nil replies nil)
              (mevedel-workspace-schedule-create workspace job)
              (rows "active")
              (dolist (peer '(1 2))
                (should (eq :json-false (plist-get (action peer id "pause") :ok))))
              (should-not (action 3 id "pause" -1))
              (dolist (invalid '(("s-ab1234" "delete") ("../s-ab1234" "pause") ("s-999999" "pause")))
                (should (eq :json-false (plist-get (action 3 (car invalid) (cadr invalid)) :ok))))
              (should (equal job (mevedel-workspace-schedule-get workspace id)))
              (pcase-dolist (`(,verb ,status) '(("pause" paused) ("resume" active)))
                (should (equal '(:t "schedule-action" :reqId 32 :ok t) (action 3 id verb)))
                (should (eq status (plist-get (mevedel-workspace-schedule-get workspace id) :status)))
                (rows (symbol-name status)))
              (setq recipients nil replies nil)
              (mevedel-workspace-schedule-update
               workspace id (lambda (entry) (setf (plist-get entry :status) 'finished)
                              (plist-put entry :last-outcome 'done)))
              (rows "finished" "done")
              (mevedel-test--with-captured-diagnostics nil
                (should (equal "Schedule could not be changed" (plist-get (action 3 id "resume") :message)))))
          (when (timerp mevedel-schedule--timer) (cancel-timer mevedel-schedule--timer)))))))

(mevedel-deftest mevedel-collaboration-lobby/schedule-changes
  (:doc "a store change refreshes all admitted peers once, and a viewer refresh stays local")
  (mevedel-collaboration-lobby-test--with-relay
    (let ((reads 0) jobs)
      (cl-letf (((symbol-function 'mevedel-workspace-schedule-list)
                 (lambda (ws) (should (eq workspace ws)) (cl-incf reads) jobs)))
        (hello 1 "browser-a" 'view)
        (hello 2 "browser-b" 'owner)
        (setq reads 0 recipients nil replies nil
              jobs '((:id "s-ab1234" :name "Private" :timing (:kind interval :seconds 900)
                      :status active :due 3600 :last-outcome private-error)))
        (run-hook-with-args 'mevedel-workspace-schedule-changed-functions workspace)
        (should (= 1 reads))
        (should (equal '(1 2) (sort (copy-sequence recipients) #'<)))
        (cl-loop for frame in replies for peer in recipients
                 for row = (aref (plist-get frame :schedules) 0) do
                 (should (equal (and (= peer 2) "Private") (plist-get row :name)))
                 (should-not (plist-member row :lastOutcome))
                 (should (= 3600 (float-time (date-to-time (plist-get row :nextRun))))))
        (setq recipients nil)
        (send 1 '(:t "lobby-refresh"))
        (should (equal '(1) recipients))))))

(provide 'test-mevedel-collaboration-lobby)
;;; test-mevedel-collaboration-lobby.el ends here
