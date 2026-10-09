;;; test-mevedel-collaboration-artifact.el --- focused collaboration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for the extracted collaboration feature module.

;;; Code:

(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-agent-control)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-pending-inputs)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-view-agent)
(require 'mevedel-view-render)
(require 'mevedel-workspace)

(require 'mevedel-artifact-store)
(require 'mevedel-collaboration-artifact-projection)
(require 'mevedel-collaboration-artifact)

(mevedel-deftest mevedel-collaboration--artifact-fields
  (:doc "projects selected ApplyPatch render data inside the artifact store")
  (let* ((save-path (make-temp-file "mevedel-collab-artifacts-" t))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root save-path :name "w"))
         (dir (mevedel-artifact-store-directory workspace))
         (session (mevedel-session--create :name "s" :workspace workspace))
         (path (file-name-concat dir "mockup/index.html")))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (make-directory (file-name-directory path) t)
          (write-region "<h1>hi</h1>" nil path nil 'silent)
          (let ((fields (car (mevedel-collaboration--artifact-fields
                              `(:kind patch
                                :files ((:kind add :path ,path)))))))
            (should (equal "mockup/index.html" (plist-get fields :artifact)))
            (should (= 11 (plist-get fields :size)))
            (should (equal (expand-file-name path)
                           (plist-get fields :artifact-path)))
            (should-not (plist-member fields :missing)))
          (should-not
           (mevedel-collaboration--artifact-fields
            `(:kind patch :files
              ((:kind add :path ,(file-name-concat save-path "notes.html"))
               (:kind add :path ,(file-name-concat dir "mockup/meta.el"))
               (:kind add :path ,(file-name-concat dir "mockup/versions/000001.html"))
               (:kind delete :path ,path)))))
          (should-not (mevedel-collaboration--artifact-fields
                       `(:kind media :files ((:kind add :path ,path)))))
          ;; The stat is memoized against per-tick target round trips,
          ;; and a settling ApplyPatch drops the entry so the card follows.
          (write-region "<h1>hello again</h1>" nil path nil 'silent)
          (should (= 11 (plist-get
                         (car (mevedel-collaboration--artifact-fields
                               `(:kind patch
                                 :files ((:kind update :path ,path)))))
                         :size)))
          (mevedel-collaboration--artifact-stat-invalidate)
          (should (= 20 (plist-get
                         (car (mevedel-collaboration--artifact-fields
                               `(:kind patch
                                 :files ((:kind update :path ,path)))))
                         :size)))
          ;; A deleted artifact still projects, marked missing, so it
          ;; reads as deleted rather than as a gap in the log.
          (delete-file path)
          (mevedel-collaboration--artifact-stat-invalidate)
          (let ((fields (car (mevedel-collaboration--artifact-fields
                              `(:kind patch
                                :files ((:kind update :path ,path)))))))
            (should (eq t (plist-get fields :missing)))
            (should-not (plist-member fields :size)))
          ;; Without a session there is no artifact store at all.
          (with-temp-buffer
            (should-not (mevedel-collaboration--artifact-fields
                         `(:kind patch
                           :files ((:kind add :path ,path))))))
          ;; A remote session's TRAMP directory accepts the model's
          ;; target-native path and maps host I/O back to the TRAMP form.
          (with-temp-buffer
            (setq-local mevedel--session
                        (mevedel-session--create
                         :name "r"
                         :workspace (mevedel-workspace--create
                                     :type 'project :id "r"
                                     :root "/ssh:example:/base/" :name "r")))
            (cl-letf (((symbol-function
                        'mevedel-collaboration--artifact-stat)
                       (lambda (seen)
                         (should (equal "/ssh:example:/base/.mevedel/artifacts/m/m.html"
                                        seen))
                         (cons 5 nil))))
              (let ((fields (car (mevedel-collaboration--artifact-fields
                                  '(:kind patch
                                    :files
                                    ((:kind add
                                      :path "/base/.mevedel/artifacts/m/m.html")))))))
                (should (equal "m/m.html" (plist-get fields :artifact)))
                (should (equal "/ssh:example:/base/.mevedel/artifacts/m/m.html"
                               (plist-get fields :artifact-path)))))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory save-path t))))


(mevedel-deftest mevedel-collaboration--tool-segment-records
  (:doc "expands selected ApplyPatch files into stable artifact cards")
  (let* ((save-path (make-temp-file "mevedel-collab-patch-artifacts-" t))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root save-path :name "w"))
         (dir (mevedel-artifact-store-directory workspace))
         (one (file-name-concat dir "a/one.html"))
         (two (file-name-concat dir "a/two.md"))
         (code (file-name-concat save-path "code.el"))
         parsed)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session
                      (mevedel-session--create :name "s" :workspace workspace))
          (insert "tool")
          (make-directory (file-name-concat dir "a") t)
          (write-region "one" nil one nil 'silent)
          (write-region "two" nil two nil 'silent)
          (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                     (lambda (_buffer _start _end) parsed)))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Applied patch: 2 changes"
                    :render-data
                    (:kind patch :files
                     ((:kind add :added 1 :deleted 0 :diff "" :path ,one)
                      (:kind add :added 1 :deleted 0 :diff "" :path ,two)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (equal '("a/one.html" "a/two.md")
                             (mapcar (lambda (record)
                                       (plist-get record :artifact))
                                     records)))
              (should (= 2 (length (delete-dups
                                    (mapcar (lambda (record)
                                              (plist-get record :id))
                                            records)))))
              (should (= 1 (length
                            (mevedel-collaboration--tool-records records)))))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Applied patch: 2 changes"
                    :render-data
                    (:kind patch :files
                     ((:kind update :added 1 :deleted 0 :diff "" :path ,code)
                      (:kind move :added 1 :deleted 0 :diff "" :path ,code :move-path ,one)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (= 2 (length records)))
              (should-not (plist-get (car records) :artifact))
              (should (equal "a/one.html"
                             (plist-get (cadr records) :artifact))))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Error: patch failed"
                    :render-data
                    (:kind patch :files ((:kind add :added 1 :deleted 0 :diff "" :path ,one)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (= 1 (length records)))
              (should-not (plist-get (car records) :artifact)))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory save-path t))))


(mevedel-deftest mevedel-collaboration-notify-artifacts-changed
  (:doc "drops cached artifact stats and re-publishes the workspace's rooms")
  (let* ((data-buffer (generate-new-buffer " *collab-artifacts-data*"))
         (workspace (mevedel-workspace--create :type 'project :id "w"))
         (session (mevedel-session--create :name "artifacts" :workspace workspace))
         (room (list :session session :data-buffer data-buffer
                     :guests (make-hash-table :test #'eql)
                     :transport 'transport))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         (path (make-temp-file "mevedel-collab-stat-"))
         published)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                   (lambda (target) (push target published))))
          ;; Prime the memo, then delete behind it: the stale size
          ;; survives until this seam drops the cache.
          (should (= 0 (car (mevedel-collaboration--artifact-stat
                             (expand-file-name path)))))
          (delete-file path)
          (mevedel-collaboration-notify-artifacts-changed workspace)
          (should (equal (list room) published))
          (should (cdr (mevedel-collaboration--artifact-stat
                        (expand-file-name path))))
          ;; A workspace without rooms still drops the cache, publishes
          ;; nothing, and does not error.
          (mevedel-collaboration-notify-artifacts-changed
           (mevedel-workspace--create :type 'project :id "other"))
          (should (= 1 (length published))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (when (file-exists-p path) (delete-file path))
      (kill-buffer data-buffer))))


(mevedel-deftest mevedel-collaboration--artifact-mime
  (:doc "maps artifact extensions case-insensitively and defaults to octet-stream")
  (progn
    (should (equal "text/html"
                   (mevedel-collaboration--artifact-mime "Mockup.HTML")))
    (should (equal "text/markdown"
                   (mevedel-collaboration--artifact-mime "notes.md")))
    (should (equal "image/png"
                   (mevedel-collaboration--artifact-mime "shot.png")))
    (should (equal "application/octet-stream"
                   (mevedel-collaboration--artifact-mime "data.bin")))
    (should (equal "application/octet-stream"
                   (mevedel-collaboration--artifact-mime "noext")))))

(mevedel-deftest mevedel-collaboration--handle-artifact-delete
  (:doc "deletes a published artifact and its comments for writable links only")
  (let* ((root (make-temp-file "mevedel-guest-artifact-delete-" t))
         (save-path (file-name-concat root "session"))
         ;; A file workspace, so the pid-lock session matches its authority.
         (workspace (mevedel-workspace--create :type 'file :id "w"
                                               :root root :name "w"))
         (dir (expand-file-name (mevedel-artifact-store-directory workspace)))
         (path (file-name-concat dir "mockup" "index.html"))
         (comments (file-name-concat
                    save-path (mevedel-collaboration--artifact-comment-logical
                               "mockup/index.html")))
         (session (mevedel-session--create :name "s" :save-path save-path
                                           :workspace workspace
                                           :authority-mode 'pid-lock))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :guests guests :transport 'transport
                     :records
                     (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                                 :artifact "mockup/index.html" :artifact-path path))))
         sent)
    (puthash 1 (list :name "viewer" :writable nil :ready t) guests)
    (puthash 2 (list :name "writer" :writable t :ready t) guests)
    (unwind-protect
        (progn
          (dolist (file (list path comments))
            (make-directory (file-name-directory file) t)
            (with-temp-file file (insert "x")))
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame) (push (cons peer frame) sent) t))
                    ((symbol-function 'mevedel-collaboration--publish) #'ignore))
            (cl-labels ((reply (peer id)
                          (setq sent nil)
                          (mevedel-collaboration--handle-artifact-delete
                           room peer (list :reqId 3 :id id))
                          (cdr (car sent))))
              (should (string-match-p "not delete" (plist-get (reply 1 "tool-1") :error)))
              (should (file-exists-p path))
              (should (eq t (plist-get (reply 2 "tool-1") :ok)))
              ;; The whole artifact goes, not only the carded file.
              (should-not (file-exists-p (file-name-concat dir "mockup")))
              (should-not (file-exists-p comments))
              (should (equal "artifact-delete" (plist-get (reply 2 "nope") :t)))
              (should (plist-get (reply 2 "nope") :error)))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--handle-artifact-get
  (:doc "answers published artifacts in bounded chunks and refuses everything else")
  (let* ((save-path (make-temp-file "mevedel-guest-artifact-" t))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root save-path :name "w"))
         (dir (expand-file-name (mevedel-artifact-store-directory workspace)))
         (path (file-name-concat dir "mockup.html"))
         (outside (file-name-concat save-path "outside.txt"))
         (escape (file-name-concat dir "escape.txt"))
         (session (mevedel-session--create :name "s" :workspace workspace))
         (guests (make-hash-table :test #'eql))
         (content (make-string 1000 ?x))
         (room (list :session session :guests guests :transport 'transport
                     :records
                     (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                                 :artifact "mockup.html"
                                 :artifact-path path)
                           (list :id "tool-2" :kind "tool" :name "ApplyPatch"
                                 :artifact "escape"
                                 :artifact-path "/etc/passwd")
                           (list :id "tool-3" :kind "tool" :name "Bash"))))
         (now 1000.0)
         sent)
    ;; A read-only guest may fetch: the card is read state.
    (puthash 1 (list :name "viewer" :writable nil :ready t) guests)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'float-time)
                   (lambda (&optional _) now)))
          (make-directory dir t)
          (let ((coding-system-for-write 'binary))
            (write-region content nil path nil 'silent)
            (write-region "private" nil outside nil 'silent))
          (make-symbolic-link outside escape)
          (setf (plist-get room :records)
                (append (plist-get room :records)
                        (list (list :id "tool-4" :kind "tool" :name "ApplyPatch"
                                    :artifact "escape.txt"
                                    :artifact-path escape))))
          ;; An id outside the published set, a tool record without an
          ;; artifact, and a record whose path escaped the directory all
          ;; earn the same bounded refusal, touching no file.
          (dolist (id '("nope" "tool-3" "tool-2" "tool-4"))
            (setq now (+ now 2.0) sent nil)
            (mevedel-collaboration--handle-artifact-get
             room 1 (list :reqId 1 :id id))
            (should (= 1 (length sent)))
            (should (equal "artifact" (plist-get (cdr (car sent)) :t)))
            (should (stringp (plist-get (cdr (car sent)) :error))))
          ;; A published artifact answers final-flagged base64 chunks
          ;; carrying name, type, and decoded size, each under the wire
          ;; bound.
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-frame-json-bytes 600))
            (mevedel-collaboration--handle-artifact-get
             room 1 (list :reqId 7 :id "tool-1")))
          (setq sent (nreverse sent))
          (should (> (length sent) 1))
          (let ((first-frame (cdr (car sent))))
            (should (equal "artifact" (plist-get first-frame :t)))
            (should (= 7 (plist-get first-frame :reqId)))
            (should (equal "mockup.html" (plist-get first-frame :name)))
            (should (equal "text/html" (plist-get first-frame :mime)))
            (should (= 1000 (plist-get first-frame :size))))
          (should (eq :json-false
                      (plist-get (cdr (car sent)) :final)))
          (should (eq t (plist-get (cdr (car (last sent))) :final)))
          (dolist (entry sent)
            (should (<= (string-bytes (json-encode
                                       (cdr entry)))
                        600)))
          (should (equal content
                         (base64-decode-string
                          (mapconcat (lambda (entry)
                                       (plist-get (cdr entry) :data))
                                     sent))))
          ;; A repeat inside the throttle window is dropped silently.
          (setq now (+ now 0.5) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 8 :id "tool-1"))
          (should-not sent)
          ;; Empty files still carry metadata and one final empty chunk.
          (write-region "" nil path nil 'silent)
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 12 :id "tool-1"))
          (should (equal '((1 :t "artifact" :reqId 12 :id "tool-1"
                             :name "mockup.html" :mime "text/html"
                             :size 0 :data "" :final t))
                         sent))
          ;; A failed write stops a multi-chunk transfer immediately.
          (write-region content nil path nil 'silent)
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-frame-json-bytes 600))
            (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                       (lambda (_transport peer frame)
                         (push (cons peer frame) sent)
                         nil)))
              (mevedel-collaboration--handle-artifact-get
               room 1 (list :reqId 13 :id "tool-1"))))
          (should (= 1 (length sent)))
          (should (eq :json-false (plist-get (cdar sent) :final)))
          (should (= 600 (string-bytes (json-encode (cdar sent)))))
          ;; The read itself is capped at max+1, which is the authoritative
          ;; overflow check even if the file changes after containment.
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-artifact-bytes 10)
                (read-function
                 (symbol-function 'insert-file-contents-literally))
                read-end)
            (cl-letf
                (((symbol-function 'insert-file-contents-literally)
                  (lambda (filename &optional visit beg end replace)
                    (setq read-end end)
                    (funcall read-function filename visit beg end replace))))
              (mevedel-collaboration--handle-artifact-get
               room 1 (list :reqId 9 :id "tool-1")))
            (should (= 11 read-end)))
          (should (string-match-p "too large"
                                  (plist-get (cdr (car sent)) :error)))
          ;; A deleted file is a targeted refusal, not a broken stream.
          (delete-file path)
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 10 :id "tool-1"))
          (should (string-match-p "deleted"
                                  (plist-get (cdr (car sent)) :error)))
          ;; An unregistered peer and a malformed request id get nothing.
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 9 (list :reqId 11 :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId "11" :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId -1 :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId #x20000000000000 :id "tool-1"))
          (should-not sent))
      (delete-directory save-path t))))


(provide 'test-mevedel-collaboration-artifact)
;;; test-mevedel-collaboration-artifact.el ends here
