;;; test-mevedel-remote-artifact.el --- Remote artifact consumer tests -*- lexical-binding: t; -*-

;;; Commentary:

;; mevedel as the consumer of an artifact link.  A fake origin stands in
;; for the remote-artifact package's bridge: it receives the requests the
;; remote adapter sends and answers as an artifact room does.  The tests
;; drive the attach and detach commands, the model's tools, `shared://'
;; addresses and the Project artifacts listing, and check what reaches the
;; origin and what stays out of the local store.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-report-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-report-test-support"))
(require 'mevedel-artifact-backend)
(require 'mevedel-artifact-store)
(require 'mevedel-artifacts-list)
(require 'mevedel-cockpit)
(require 'mevedel-pipeline)
(require 'mevedel-remote-artifact)
(require 'mevedel-shared-editing)
(require 'mevedel-tools)
(require 'mevedel-workspace)

(defconst mevedel-remote-artifact-test--link
  "https://relay.example.org/#room-1.c2VjcmV0LXNlY3JldC1zZWNyZXQtc2VjcmV0LXNlY3JldC0x"
  "An artifact link as the user pastes it.")

(defconst mevedel-remote-artifact-test--overview
  "shared://snt-pipeline whiteboard \"SNT analysis pipeline\" revision 2\nd7b24fe8c3bd {\"id\":\"start\",\"type\":\"rectangle\",\"x\":0,\"y\":0}\n"
  "The board as the origin's read shows it.")

(defun mevedel-remote-artifact-test--origin ()
  "Return a fake origin: a plist the fake bridge answers from.
`:requests' collects every request, newest first; `:reachable',
`:writable' and `:title' shape the answers."
  (list :requests nil :reachable t :writable t :title "SNT analysis pipeline"
        :kind "whiteboard"))

(defun mevedel-remote-artifact-test--answer (origin body)
  "Return ORIGIN's reply to request BODY, as an artifact room gives it."
  (let ((action (plist-get body :action)))
    (pcase action
      ("read" (list :result (list :text (if (plist-get body :history)
                                            "contribution c1 revision 2 Host"
                                          mevedel-remote-artifact-test--overview))))
      ("view" (list :result (list :png "iVBORw0KGgo=" :mime "image/png")))
      ((or "patch" "rename" "background" "image" "insert" "revert")
       (cond
        ((not (plist-get origin :writable)) (list :error "This artifact link is view-only"))
        ((equal "stale" (plist-get (aref (or (plist-get body :changes) [nil]) 0) :hash))
         (list :error "Stale hash" :code "stale" :revision 3
               :targets ["d7b24fe8c3bd {\"id\":\"start\"}"]))
        (t (list :result
                 (list :text (concat "{\"id\":\"snt-pipeline\",\"kind\":\"whiteboard\","
                                     "\"revision\":3,\"address\":\"shared://snt-pipeline\","
                                     "\"contribution\":\"c2\",\"changed\":[]}")
                       :png "iVBORw0KGgo=")))))
      (_ (list :error "An artifact link does not permit that")))))

(defun mevedel-remote-artifact-test--bridge (origin)
  "Return a bridge transport answering from ORIGIN, asynchronously."
  (lambda (request callback)
    (plist-put origin :requests (cons request (plist-get origin :requests)))
    (run-at-time
     0 nil callback
     (if (not (plist-get origin :reachable))
         (list :error "Origin unavailable (revoked, deleted, or offline)")
       (append (list :welcome (list :id "snt-pipeline" :kind (plist-get origin :kind)
                                    :title (plist-get origin :title)
                                    :readOnly (if (plist-get origin :writable) :json-false t)))
               (when-let* ((body (plist-get request :body)))
                 (list :reply (mevedel-remote-artifact-test--answer origin body))))))))

(defmacro mevedel-remote-artifact-test--with-session (&rest body)
  "Run BODY in a live session buffer with the model's tools and a fake origin.
BODY sees WORKSPACE, SESSION, ORIGIN, DIRECTORY and the local functions
`run', `text' and `attach'.  `attach' attaches the fake origin's link
under its origin id."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "mevedel-remote-artifact-" t))
          (workspace (mevedel-workspace--create :root directory))
          (session (mevedel-session--create :save-path directory
                                            :workspace workspace :working-directory directory
                                            :permission-mode 'full-auto :authority-mode 'pid-lock))
          (buffer (generate-new-buffer " *remote-artifact-test*"))
          (origin (mevedel-remote-artifact-test--origin))
          (mevedel-user-dir (file-name-as-directory (file-name-concat directory "user")))
          (mevedel-remote-artifact--transport (mevedel-remote-artifact-test--bridge origin))
          (mevedel-remote-artifact--records :unread)
          (mevedel-remote-artifact--status (make-hash-table :test #'equal))
          (mevedel-remote-artifact--workspaces (make-hash-table :test #'equal))
          (mevedel-remote-artifact-display-name "tester@host")
          (mevedel-artifact-backend--registry (make-hash-table :test #'equal))
          (mevedel-artifact-backend--restored (make-hash-table :test #'equal))
          (mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
          (mevedel-artifact-lease--held (make-hash-table :test #'equal))
          (mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
     (unwind-protect
         (with-current-buffer buffer
           (setq-local mevedel--session session)
           (setf (mevedel-session-root-buffer session) buffer)
           (mevedel-tools-register)
           (cl-labels ((run (name args &optional status)
                         (let (reply)
                           (mevedel-pipeline-run-tool-outcome
                            (mevedel-tool-get name) (lambda (value) (setq reply value)) args)
                           (let ((deadline (+ (float-time) 10)))
                             (while (and (not reply) (< (float-time) deadline))
                               (accept-process-output nil 0.05)))
                           (unless (eq (or status 'success) (plist-get reply :status))
                             (ert-fail (list name args (plist-get reply :status)
                                             (plist-get reply :result))))
                           reply))
                       (text (reply)
                         (let ((result (plist-get reply :result)))
                           (if (stringp result) result (format "%S" result))))
                       (attach (&optional name)
                         (mevedel-remote-artifact-attach
                          workspace mevedel-remote-artifact-test--link name)))
             ,@body))
       (mevedel-shared-editing-stop)
       (mevedel-artifact-lease-release-all)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (delete-directory directory t))))

(mevedel-deftest mevedel-remote-artifact-attach
  (:doc "Attaching registers the artifact under its origin id or a chosen name, outside the store"
   :quiet t)
  (mevedel-remote-artifact-test--with-session
    ;; The command reads the link without echo and keeps it out of messages.
    (let ((inhibit-message t) shown)
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) mevedel-remote-artifact-test--link))
                ((symbol-function 'mevedel-workspace) (lambda (&rest _) workspace))
                ((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format-message format args) shown))))
        (should (equal "snt-pipeline" (call-interactively #'mevedel-remote-artifact-attach))))
      (should (cl-some (lambda (line) (string-search "snt-pipeline" line)) shown))
      (should-not (cl-some (lambda (line) (string-search "room-1" line)) shown)))
    (should (mevedel-artifact-backend-registered-p workspace "snt-pipeline"))
    ;; Only the welcome was asked for, under the configured display name.
    (let ((request (car (plist-get origin :requests))))
      (should (equal mevedel-remote-artifact-test--link (plist-get request :link)))
      (should (equal "tester@host" (plist-get request :name)))
      (should-not (plist-get request :body)))
    ;; It is listed with the project's artifacts and attaches like one.
    (should (equal '(("snt-pipeline" "whiteboard" "SNT analysis pipeline"))
                   (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :kind)
                                               (plist-get row :title)))
                           (mevedel-artifact-backend-catalog workspace 'items))))
    (mevedel-artifact-store-attach session "snt-pipeline")
    (should (equal '(1 . 1) (mevedel-artifacts-list-count session)))
    ;; Nothing about it is in the local store; the per-user record is private.
    (should-not (mevedel-artifact-store-ids workspace))
    (should-not (file-exists-p (file-name-concat directory ".mevedel" "artifacts")))
    (let ((record (file-name-concat mevedel-user-dir "remote-artifacts.eld")))
      (should (file-exists-p record))
      (should (= #o600 (file-modes record))))
    ;; Attaching the same link again keeps the one registration.
    (should (equal "snt-pipeline" (attach)))
    (should (= 1 (length (mevedel-artifact-backend-catalog workspace 'ids))))
    ;; A chosen name is the alias; the origin id still reaches the origin.
    (should (equal "pipeline" (attach "pipeline")))
    (should (equal '("pipeline")
                   (mapcar (lambda (row) (plist-get row :id))
                           (mevedel-artifact-backend-catalog workspace 'ids))))
    ;; A local artifact's id is refused as a name; another is asked for.
    (make-directory (file-name-concat directory ".mevedel" "artifacts" "taken") t)
    (should-error (attach "taken") :type 'user-error)
    (should-error (attach "bad name") :type 'user-error)
    ;; An unreachable origin attaches nothing.
    (plist-put origin :reachable nil)
    (should (string-search "Origin unavailable"
                           (cadr (should-error (attach "other") :type 'user-error))))
    (should-not (mevedel-artifact-backend-registered-p workspace "other"))
    ;; Detaching forgets it in the registry and in the record.
    (mevedel-remote-artifact-detach workspace "pipeline")
    (should (equal '((:id "taken")) (mevedel-artifact-backend-catalog workspace 'ids)))
    (should-not (mevedel-remote-artifact--records-of workspace))
    (should-error (mevedel-remote-artifact-detach workspace "pipeline") :type 'user-error)))

(mevedel-deftest mevedel-remote-artifact--call
  (:doc "shared:// Read and Grep and SharedEdit reach the origin under the alias, never the store"
   :quiet t)
  (mevedel-remote-artifact-test--with-session
    (attach "pipeline")
    (cl-labels ((body () (plist-get (car (plist-get origin :requests)) :body))
                (name () (plist-get (car (plist-get origin :requests)) :name)))
      ;; Reads go to the origin's read and view, with the display name only.
      (should (string-search "d7b24fe8c3bd" (text (run "Read" '(:file_path "shared://pipeline")))))
      (should (equal '(:action "read") (body)))
      (should (equal "tester@host" (name)))
      (should (string-search "contribution c1"
                             (text (run "Read" '(:file_path "shared://pipeline/history")))))
      (should (equal '(:action "read" :history t) (body)))
      (let ((file (file-name-concat directory "board.png")))
        (run "SharedSaveImage" (list :source "shared://pipeline/view.png" :file_path file))
        (should (equal '(:action "view") (body)))
        (should (equal (base64-decode-string "iVBORw0KGgo=")
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally file)
                         (buffer-string)))))
      ;; Grep searches the same lines and reports them at the alias.
      (let ((found (text (run "Grep" '(:pattern "rectangle" :path "shared://pipeline"
                                                :output_mode "content")))))
        (should (string-search "shared://pipeline" found))
        (should (string-search "d7b24fe8c3bd" found)))
      (should (string-search "shared://pipeline"
                             (text (run "Grep" '(:pattern "rectangle" :path "shared://")))))
      ;; An edit sends only SharedEdit's arguments, attributed to the agent.
      (let ((result (text (run "SharedEdit"
                               '(:id "pipeline" :action "patch"
                                     :changes [(:id "next" :after (:type "rectangle" :x 200))])))))
        (should (string-search "\"id\":\"pipeline\"" result))
        (should (string-search "\"address\":\"shared://pipeline\"" result)))
      (should (equal '(:action "patch" :changes [(:id "next" :after (:type "rectangle" :x 200))])
                     (body)))
      (should (equal "tester@host /root" (name)))
      (should (member "pipeline" (mevedel-session-attached-artifacts session)))
      (run "SharedEdit" '(:id "pipeline" :action "rename" :title "Pipeline"))
      (should (equal '(:action "rename" :title "Pipeline") (body)))
      (run "SharedEdit" '(:id "pipeline" :action "insert" :item "Builtin/box" :x 1 :y 2))
      (should (equal '(:action "insert" :item "Builtin/box" :x 1 :y 2) (body)))
      ;; A stale hash returns the current lines, as for a local item.
      (should (string-search "\"code\":\"stale\""
                             (text (run "SharedEdit" '(:id "pipeline" :action "patch"
                                                           :changes [(:id "start" :hash "stale")])
                                        'error))))
      ;; Discussions and deletion stay at the origin; versions are its own.
      (should (string-search "stay at its origin"
                             (text (run "Read" '(:file_path "shared://pipeline/discussions")
                                        'error))))
      (let ((sent (length (plist-get origin :requests))) failure)
        (mevedel-artifact-store-save-version workspace "pipeline" "session-1")
        (mevedel-artifact-store-delete workspace "pipeline" nil
                                       (lambda (reason) (setq failure reason)))
        (let ((deadline (+ (float-time) 5)))
          (while (and (not failure) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (should (string-search "detach it here" failure))
        (should (= sent (length (plist-get origin :requests)))))
      ;; A view-only link and an unreachable origin give their errors.
      (plist-put origin :writable nil)
      (should (string-search "view-only"
                             (text (run "SharedEdit" '(:id "pipeline" :action "rename" :title "X")
                                        'error))))
      (plist-put origin :reachable nil)
      (should (string-search "Origin unavailable (revoked, deleted, or offline)"
                             (text (run "Read" '(:file_path "shared://pipeline") 'error))))
      ;; None of it touched the local store.
      (should-not (mevedel-artifact-store-ids workspace))
      (should-not (file-exists-p (file-name-concat directory ".mevedel" "artifacts"))))))

(mevedel-deftest mevedel-remote-artifact--call/cancellation
  (:doc "A cancelled or no longer authorized request discards the origin's reply"
   :quiet t)
  (mevedel-remote-artifact-test--with-session
    (attach)
    (let* ((held nil)
           (mevedel-remote-artifact--transport
            (lambda (_request callback) (setq held callback)))
           (answer (list :welcome (list :id "snt-pipeline" :kind "whiteboard" :title "T")
                         :reply (list :result (list :text "{\"id\":\"snt-pipeline\"}"))))
           (replies nil)
           (committed 0))
      (cl-labels ((call (&optional authorize)
                    (mevedel-remote-artifact--call
                     workspace '(:action "rename" :id "snt-pipeline" :title "New")
                     (lambda (reply) (push reply replies))
                     authorize
                     (lambda (deliver) (cl-incf committed) (funcall deliver))))
                  (settle ()
                    (let ((deadline (+ (float-time) 5)))
                      (while (and (not replies) (< (float-time) deadline))
                        (accept-process-output nil 0.02)))
                    (pop replies)))
        ;; Cancelled while the edit is on its way: the reply is discarded.
        (funcall (call))
        (funcall held answer)
        (should (equal '(:error "Editing operation cancelled") (settle)))
        (should (zerop committed))
        ;; Authority that ended meanwhile discards it too.
        (let ((allowed t))
          (call (lambda () allowed))
          (setq allowed nil)
          (funcall held answer))
        (should (equal '(:error "Editing authority ended") (settle)))
        ;; A request not authorized at all is never sent.
        (setq held nil)
        (call #'ignore)
        (should (equal '(:error "Editing authority ended") (settle)))
        (should-not held)
        ;; Otherwise the reply is delivered through the commit.
        (call)
        (funcall held answer)
        (should (equal "snt-pipeline" (plist-get (plist-get (plist-get (settle) :result) :model)
                                                 :id)))
        (should (= 1 committed))))))

(mevedel-deftest mevedel-remote-artifact-registrations
  (:doc "Attached artifacts come back in a fresh Emacs; origin changes are announced"
   :quiet t)
  (mevedel-remote-artifact-test--with-session
    (attach "pipeline")
    ;; A fresh Emacs reads the record on its first lookup of the workspace.
    (let ((mevedel-artifact-backend--registry (make-hash-table :test #'equal))
          (mevedel-artifact-backend--restored (make-hash-table :test #'equal))
          (mevedel-remote-artifact--records :unread))
      (should (equal '((:id "pipeline")) (mevedel-artifact-backend-catalog workspace 'ids)))
      (should (string-search "d7b24fe8c3bd" (text (run "Read" '(:file_path "shared://pipeline"))))))
    ;; A record entry clashing with a local artifact is not restored.
    (make-directory (file-name-concat directory ".mevedel" "artifacts" "pipeline") t)
    (let ((mevedel-artifact-backend--registry (make-hash-table :test #'equal))
          (mevedel-artifact-backend--restored (make-hash-table :test #'equal)))
      (should-not (mevedel-artifact-backend-registered-p workspace "pipeline")))
    (delete-directory (file-name-concat directory ".mevedel") t)
    ;; The origin's change event renames it everywhere it is listed.
    (let* ((heard nil)
           (mevedel-artifact-backend-change-hook
            (list (lambda (changed state _result)
                    (when (eq changed workspace) (push state heard))))))
      (mevedel-remote-artifact--event
       (mevedel-remote-artifact--ref mevedel-remote-artifact-test--link)
       '(:event "changed" :id "snt-pipeline" :kind "whiteboard" :title "Renamed" :revision 7))
      (should (equal '((:id "pipeline" :kind "whiteboard" :title "Renamed" :revision 7)) heard))
      (should (equal '((:id "pipeline" :kind "whiteboard" :title "Renamed" :revision 7))
                     (mevedel-artifact-backend-catalog workspace 'items))))))

(mevedel-deftest mevedel-remote-artifact--catalog
  (:doc "Project artifacts marks remote artifacts with origin and reachability, and detaches them"
   :quiet t)
  (mevedel-remote-artifact-test--with-session
    (attach "pipeline")
    (let ((view (generate-new-buffer " *remote-artifact-view*")))
      (unwind-protect
          (cl-labels ((cell (column)
                        (aref (cadr (assoc "pipeline" tabulated-list-entries)) column))
                      (settle ()
                        (let ((deadline (+ (float-time) 1)))
                          (while (< (float-time) deadline) (accept-process-output nil 0.02)))))
            (with-current-buffer
                (mevedel-artifacts-list-open
                 (list :session session :view-buffer view :data-buffer buffer
                       :origin-buffer view :workspace workspace))
              (should (equal "SNT analysis pipeline" (cell 0)))
              (should (equal "whiteboard" (cell 1)))
              (should (equal "remote relay.example.org, reachable" (cell 6)))
              (mevedel-cockpit-goto-id "pipeline")
              (let ((details (mevedel-report-test-text
                              (mevedel-artifacts-list--details
                               (mevedel-cockpit-surface-selected) nil))))
                (should (string-match-p "Origin +relay.example.org" details))
                (should (string-match-p "Origin id +snt-pipeline" details))
                (should (string-match-p "Link +edit" details)))
              ;; A listing checks a link not checked lately, and shows the outcome.
              (plist-put origin :reachable nil)
              (puthash (mevedel-remote-artifact--ref mevedel-remote-artifact-test--link)
                       '(:reachable yes :checked 0) mevedel-remote-artifact--status)
              (mevedel-cockpit-surface-refresh "pipeline")
              (settle)
              (mevedel-cockpit-surface-refresh "pipeline")
              (should (equal "remote relay.example.org, unavailable" (cell 6)))
              ;; What only a local artifact has is refused, not created.
              (mevedel-cockpit-goto-id "pipeline")
              (dolist (command '(mevedel-artifacts-list-open-browser
                                 mevedel-artifacts-list-conversation
                                 mevedel-artifacts-list-duplicate
                                 mevedel-artifacts-list-versions
                                 mevedel-artifacts-list-save-version))
                (should (string-search "lives at its origin"
                                       (cadr (should-error (funcall command)
                                                           :type 'user-error)))))
              (should-not (file-exists-p (file-name-concat directory ".mevedel" "artifacts")))
              ;; Deleting it detaches it; the origin keeps it.
              (let ((sent (length (plist-get origin :requests))))
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                  (mevedel-artifacts-list-delete))
                (should (= sent (length (plist-get origin :requests)))))
              (should-not (assoc "pipeline" tabulated-list-entries))
              (should-not (mevedel-remote-artifact--records-of workspace))))
        (when-let* ((cockpit (get-buffer mevedel-artifacts-list-buffer-name)))
          (kill-buffer cockpit))
        (kill-buffer view)))))

(defconst mevedel-remote-artifact-test--fake-room
  (file-name-concat (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or buffer-file-name load-file-name byte-compile-current-file))))
                    "remote-artifact" "test" "fake-room.mjs")
  "The package's fake artifact room, which replays the shared frame fixtures.")

(mevedel-deftest mevedel-remote-artifact--bridge-send
  (:doc "The shipped package's bridge serves reads and edits from an artifact room"
   :quiet t)
  (mevedel-remote-artifact-test--with-session
    (skip-unless (executable-find "node"))
    (let* ((script (file-name-concat directory "origin.mjs"))
           (mevedel-remote-artifact--transport #'mevedel-remote-artifact--bridge-send)
           (mevedel-remote-artifact--process nil)
           (mevedel-remote-artifact--pending (make-hash-table :test #'eql))
           (mevedel-remote-artifact-command nil)
           (output "")
           (room nil))
      (with-temp-file script
        (insert (format "import { startFakeRoom } from %S;\n"
                        (concat "file://" mevedel-remote-artifact-test--fake-room))
                "const room = await startFakeRoom();\n"
                "process.stdout.write(JSON.stringify({ link: room.link('edit') }) + '\\n');\n"
                "process.stdin.on('end', async () => { await room.close(); process.exit(0); });\n"
                "process.stdin.resume();\n"))
      (unwind-protect
          (let ((link nil))
            (setq room (make-process
                        :name "remote-artifact-test-origin" :buffer nil :noquery t
                        :connection-type 'pipe
                        :command (list (executable-find "node") script)
                        :filter (lambda (_process chunk) (setq output (concat output chunk)))))
            (let ((deadline (+ (float-time) 10)))
              (while (and (not (string-search "\n" output)) (< (float-time) deadline))
                (accept-process-output room 0.05)))
            (setq link (plist-get (json-parse-string output :object-type 'plist) :link))
            ;; Attaching reads the welcome through the bridge.
            (should (equal "snt-pipeline" (mevedel-remote-artifact-attach workspace link)))
            (should (string-search "3f9a0c12b7 {\"id\":\"box\""
                                   (text (run "Read" '(:file_path "shared://snt-pipeline")))))
            (should (string-search "d7b24fe8c3bd"
                                   (text (run "SharedEdit"
                                              '(:id "snt-pipeline" :action "patch"
                                                    :changes [(:id "next"
                                                                   :after (:type "rectangle" :x 200 :y 0
                                                                                 :width 120 :height 60))])))))
            ;; A stopped bridge starts again on the next request.
            (let ((first mevedel-remote-artifact--process))
              (mevedel-remote-artifact-stop)
              (should (string-search "3f9a0c12b7"
                                     (text (run "Read" '(:file_path "shared://snt-pipeline")))))
              (should (process-live-p mevedel-remote-artifact--process))
              (should-not (eq first mevedel-remote-artifact--process)))
            ;; Once the origin is gone the artifact is unavailable.
            (process-send-eof room)
            (let ((deadline (+ (float-time) 10)))
              (while (and (process-live-p room) (< (float-time) deadline))
                (accept-process-output room 0.05)))
            (should (string-search "Origin unavailable (revoked, deleted, or offline)"
                                   (text (run "Read" '(:file_path "shared://snt-pipeline")
                                              'error)))))
        (mevedel-remote-artifact-stop)
        (when (process-live-p room) (delete-process room))))))

(provide 'test-mevedel-remote-artifact)
;;; test-mevedel-remote-artifact.el ends here
