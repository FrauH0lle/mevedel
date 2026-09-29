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

(require 'mevedel-collaboration-artifact-projection)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-transcript-audit)
(require 'mevedel-view)

(mevedel-deftest mevedel-collaboration--artifact-fields
  (:doc "projects selected ApplyPatch render data inside the artifacts directory")
  (let* ((save-path (make-temp-file "mevedel-collab-artifacts-" t))
         (dir (mevedel-session-artifacts-artifacts-dir save-path))
         (session (mevedel-session--create :name "s" :save-path save-path))
         (path (file-name-concat dir "mockup.html")))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (make-directory dir t)
          (write-region "<h1>hi</h1>" nil path nil 'silent)
          (let ((fields (car (mevedel-collaboration--artifact-fields
                              `(:kind patch
                                :files ((:kind add :path ,path)))))))
            (should (equal "mockup.html" (plist-get fields :artifact)))
            (should (= 11 (plist-get fields :size)))
            (should (equal (expand-file-name path)
                           (plist-get fields :artifact-path)))
            (should-not (plist-member fields :missing)))
          (should-not
           (mevedel-collaboration--artifact-fields
            `(:kind patch :files
              ((:kind add :path ,(file-name-concat save-path "notes.html"))
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
          ;; Without a session there is no artifacts directory at all.
          (with-temp-buffer
            (should-not (mevedel-collaboration--artifact-fields
                         `(:kind patch
                           :files ((:kind add :path ,path))))))
          ;; A remote session's TRAMP directory accepts the model's
          ;; target-native path and maps host I/O back to the TRAMP form.
          (with-temp-buffer
            (setq-local mevedel--session
                        (mevedel-session--create
                         :name "r" :save-path "/ssh:example:/base"))
            (cl-letf (((symbol-function
                        'mevedel-collaboration--artifact-stat)
                       (lambda (seen)
                         (should (equal "/ssh:example:/base/artifacts/m.html"
                                        seen))
                         (cons 5 nil))))
              (let ((fields (car (mevedel-collaboration--artifact-fields
                                  '(:kind patch
                                    :files
                                    ((:kind add
                                      :path "/base/artifacts/m.html")))))))
                (should (equal "m.html" (plist-get fields :artifact)))
                (should (equal "/ssh:example:/base/artifacts/m.html"
                               (plist-get fields :artifact-path)))))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory save-path t))))


(mevedel-deftest mevedel-collaboration--tool-segment-records
  (:doc "expands selected ApplyPatch files into stable artifact cards")
  (let* ((save-path (make-temp-file "mevedel-collab-patch-artifacts-" t))
         (dir (mevedel-session-artifacts-artifacts-dir save-path))
         (one (file-name-concat dir "one.html"))
         (two (file-name-concat dir "two.md"))
         (code (file-name-concat save-path "code.el"))
         parsed)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session
                      (mevedel-session--create :name "s" :save-path save-path))
          (insert "tool")
          (make-directory dir t)
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
              (should (equal '("one.html" "two.md")
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
              (should (equal "one.html"
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
  (:doc "drops cached artifact stats and re-publishes the shared room")
  (let* ((data-buffer (generate-new-buffer " *collab-artifacts-data*"))
         (session (mevedel-session--create :name "artifacts"))
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
          (mevedel-collaboration-notify-artifacts-changed session)
          (should (equal (list room) published))
          (should (cdr (mevedel-collaboration--artifact-stat
                        (expand-file-name path))))
          ;; An unshared session still drops the cache, publishes nothing,
          ;; and does not error.
          (mevedel-collaboration-notify-artifacts-changed
           (mevedel-session--create :name "other"))
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

(mevedel-deftest mevedel-collaboration--handle-artifact-get
  (:doc "answers published artifacts in bounded chunks and refuses everything else")
  (let* ((save-path (make-temp-file "mevedel-guest-artifact-" t))
         (dir (file-name-as-directory
               (expand-file-name
                (mevedel-session-artifacts-artifacts-dir save-path))))
         (path (file-name-concat dir "mockup.html"))
         (outside (file-name-concat save-path "outside.txt"))
         (escape (file-name-concat dir "escape.txt"))
         (session (mevedel-session--create :name "s" :save-path save-path))
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


;;
;;; Artifact comments

(mevedel-deftest mevedel-collaboration--artifact-comment-string
  (:doc "accepts bounded strings and rejects empty, oversized and non-strings")
  (progn
    (should (equal "a" (mevedel-collaboration--artifact-comment-string "a" :quote)))
    (should-not (mevedel-collaboration--artifact-comment-string "" :quote))
    (should (equal "" (mevedel-collaboration--artifact-comment-string "" :label t)))
    (should-not (mevedel-collaboration--artifact-comment-string
                 (make-string 513 ?x) :quote))
    (should-not (mevedel-collaboration--artifact-comment-string 7 :quote))))

(mevedel-deftest mevedel-collaboration--artifact-comment-anchor ()
  ,test
  (test)
  :doc "rebuilds guest anchors from known bounded fields only"
  (let ((anchor (mevedel-collaboration--artifact-comment-anchor
                 '(:kind "box" :selector "main > section:nth-of-type(2)"
                   :label "Schema › area · 3 elements"
                   :sig (:tag "section" :h "00ff00ff00ff00ff" :evil "x")
                   :quote "share" :start 12
                   :region (:x0 0 :y0 0.25 :x1 0.5 :y1 1)
                   :count 3 :script "alert(1)"))))
    (should (equal anchor
                   '(:kind "box" :selector "main > section:nth-of-type(2)"
                     :label "Schema › area · 3 elements"
                     :sig (:tag "section" :h "00ff00ff00ff00ff")
                     :quote "share" :start 12
                     :region (:x0 0 :y0 0.25 :x1 0.5 :y1 1)
                     :count 3))))
  :doc "drops malformed optional parts and defaults an unknown kind"
  (should (equal (mevedel-collaboration--artifact-comment-anchor
                  '(:kind "evil" :selector "p" :label nil
                    :sig (:tag "Bad Tag" :h "zz")
                    :start -4 :quote "x"
                    :region (:x0 0.5 :y0 0 :x1 0.2 :y1 1)))
                 '(:kind "element" :selector "p" :label "" :quote "x" :start 0)))
  :doc "refuses anchors without a usable selector"
  (progn
    (should-not (mevedel-collaboration--artifact-comment-anchor '(:label "x")))
    (should-not (mevedel-collaboration--artifact-comment-anchor
                 (list :selector (make-string 1001 ?a))))
    (should-not (mevedel-collaboration--artifact-comment-anchor '("p")))
    (should-not (mevedel-collaboration--artifact-comment-anchor nil))))

(mevedel-deftest mevedel-collaboration--artifact-comment-snapshot ()
  ,test
  (test)
  :doc "names the artifact file, target, quote, box and excerpts"
  (let ((snapshot (mevedel-collaboration--artifact-comment-snapshot
                   '(:artifact "schema.html" :artifact-path "/tmp/a/schema.html")
                   '(:kind "box" :selector "main" :label "Schema › area · 2 elements"
                     :quote "share" :region (:x0 0 :y0 0 :x1 0.5 :y1 1) :count 2)
                   '(:text "Two cards" :html "<div>Two cards</div>"))))
    (should (string-match-p "^Comment on session artifact schema.html$" snapshot))
    (should (string-match-p "^File: /tmp/a/schema.html$" snapshot))
    (should (string-match-p "^Target: Schema › area · 2 elements$" snapshot))
    (should (string-match-p "^Selected text: \"share\"$" snapshot))
    (should (string-match-p "x 0-0.5, y 0-1 .*(2 elements covered)" snapshot))
    (should (string-match-p "```html\n<div>Two cards</div>\n```" snapshot)))
  :doc "omits oversized guest excerpts instead of truncating them silently"
  (let ((snapshot (mevedel-collaboration--artifact-comment-snapshot
                   '(:artifact "a.html")
                   '(:kind "element" :selector "p" :label "")
                   (list :text (make-string 4001 ?x) :html 7))))
    (should-not (string-match-p "Target text" snapshot))
    (should-not (string-match-p "Target HTML" snapshot))
    (should-not (string-match-p "^Target:" snapshot))))

(defun mevedel-test--artifact-comment-room (data-buf)
  "Return a room for DATA-BUF with a live session and one HTML artifact."
  (let* ((workspace (mevedel-workspace--create :type 'file :id "artifact-comment"
                                               :root temporary-file-directory))
         (session (mevedel-session-create "main" workspace)))
    (with-current-buffer data-buf
      (setq-local mevedel--session session mevedel--workspace workspace))
    (mevedel-session-set-pending-input-paused session t)
    (list :session session :data-buffer data-buf :transport 'transport
          :guests (make-hash-table :test #'eql)
          :records (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                               :artifact "schema.html"
                               :artifact-path "/tmp/schema.html")
                         (list :id "tool-2" :kind "tool" :name "ApplyPatch"
                               :artifact "notes.md" :artifact-path "/tmp/notes.md")
                         (list :id "tool-3" :kind "tool" :name "ApplyPatch"
                               :artifact "gone.html" :artifact-path "/tmp/gone.html"
                               :missing t)))))

(defconst mevedel-test--artifact-comment-frame
  '(:t "artifact-comment" :reqId 3 :id "tool-1"
    :commentId "0123456789abcdef0123" :text "  Make this bigger  "
    :anchor (:kind "word" :selector "#hero > p:nth-of-type(1)"
             :label "SNT Schema v2 › word \"share\"" :quote "share" :start 9
             :sig (:tag "p" :h "0123456789abcdef"))
    :context (:text "countries share the same tables" :html "<p>countries share</p>"))
  "A well-formed guest comment frame on the published schema.html artifact.")

(mevedel-deftest mevedel-collaboration--artifact-comment-queue ()
  ,test
  (test)
  :doc "queues an attributed follow-up with host-built context and stays idempotent"
  (mevedel-view-test--with-buffers
    (let* ((room (mevedel-test--artifact-comment-room data-buf))
           (session (plist-get room :session))
           (guest '(:name "Alice" :guest-id "alice" :writable t :role "full"))
           (frame (copy-tree mevedel-test--artifact-comment-frame))
           (reply (mevedel-collaboration--artifact-comment-queue room guest frame))
           (entry (car (mevedel-session-pending-follow-ups session)))
           (shared (plist-get entry :shared-question)))
      (should (plist-get reply :queued))
      (should (equal (plist-get reply :commentId) "0123456789abcdef0123"))
      (should (equal (plist-get shared :kind) "artifact"))
      (should (equal (plist-get shared :artifact) "schema.html"))
      (should (equal (plist-get shared :text) "Make this bigger"))
      (should-not (plist-get shared :itemId))
      (should (equal (plist-get (plist-get shared :anchor) :quote) "share"))
      (should (string-prefix-p
               (concat "Make this bigger\n\nShared content snapshot (user-provided data):\n"
                       "Comment on session artifact schema.html\nFile: /tmp/schema.html")
               (plist-get entry :input)))
      (should (equal (plist-get entry :guest-name) "Alice"))
      ;; The Emacs view folds the context behind an artifact label.
      (should (equal (plist-get (mevedel-transcript-audit-shared-context
                                 (plist-get entry :input) shared)
                                :label)
                     "Artifact comment · schema.html · SNT Schema v2 › word \"share\""))
      ;; A retry with the same identity does not queue a second comment.
      (should (plist-get (mevedel-collaboration--artifact-comment-queue
                          room guest (copy-tree frame))
                         :queued))
      (should (= 1 (length (mevedel-session-pending-follow-ups session))))))
  :doc "refuses read-only links, bad identities, non-HTML, missing and unknown artifacts"
  (mevedel-view-test--with-buffers
    (let ((room (mevedel-test--artifact-comment-room data-buf))
          (writer '(:name "Alice" :guest-id "alice" :writable t :role "full")))
      (dolist (case (list (list '(:name "V" :writable nil) nil "not comment")
                          (list writer '(:commentId "bad id") "identity")
                          (list writer '(:id "tool-2") "Only HTML")
                          (list writer '(:id "tool-3") "not published")
                          (list writer '(:id "nope") "not published")
                          (list writer '(:text "   ") "comment is required")
                          (list writer '(:anchor (:label "x")) "could not be identified")))
        (let ((frame (copy-tree mevedel-test--artifact-comment-frame)))
          (cl-loop for (key value) on (nth 1 case) by #'cddr
                   do (setq frame (plist-put frame key value)))
          (let ((err (should-error (mevedel-collaboration--artifact-comment-queue
                                    room (car case) frame))))
            (should (string-match-p (nth 2 case) (error-message-string err))))))
      (should-not (mevedel-session-pending-follow-ups (plist-get room :session))))))

(mevedel-deftest mevedel-collaboration--artifact-comment-known-p
  (:doc "finds queued and delivered artifact comments by identity")
  (mevedel-view-test--with-buffers
    (let* ((room (mevedel-test--artifact-comment-room data-buf))
           (session (plist-get room :session))
           (guest '(:name "Alice" :guest-id "alice" :writable t :role "full")))
      (should-not (mevedel-collaboration--artifact-comment-known-p
                   room "0123456789abcdef0123"))
      (mevedel-collaboration--artifact-comment-queue
       room guest (copy-tree mevedel-test--artifact-comment-frame))
      (should (mevedel-collaboration--artifact-comment-known-p
               room "0123456789abcdef0123"))
      (mevedel-session-set-pending-input-paused session nil)
      (cl-letf (((symbol-function 'gptel-send) #'ignore))
        (mevedel-view--drain-follow-up data-buf))
      (should-not (mevedel-session-pending-follow-ups session))
      (should (mevedel-collaboration--artifact-comment-known-p
               room "0123456789abcdef0123"))
      (should-not (mevedel-collaboration--artifact-comment-known-p room "other")))))

(mevedel-deftest mevedel-collaboration--handle-artifact-comment
  (:doc "answers the sender with a receipt or a refusal and never signals")
  (mevedel-view-test--with-buffers
    (let* ((room (mevedel-test--artifact-comment-room data-buf))
           sent)
      (puthash 1 (list :name "Alice" :guest-id "alice" :writable t :ready t)
               (plist-get room :guests))
      (puthash 2 (list :name "Viewer" :writable nil :ready t)
               (plist-get room :guests))
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame) (push (cons peer frame) sent) t)))
        (mevedel-collaboration--handle-artifact-comment
         room 1 (copy-tree mevedel-test--artifact-comment-frame))
        (should (equal (caar sent) 1))
        (should (equal (plist-get (cdar sent) :t) "artifact-comment"))
        (should (equal (plist-get (cdar sent) :reqId) 3))
        (should (plist-get (cdar sent) :queued))
        (setq sent nil)
        (mevedel-collaboration--handle-artifact-comment
         room 2 (plist-put (copy-tree mevedel-test--artifact-comment-frame)
                           :commentId "fedcba9876543210fedc"))
        (should (stringp (plist-get (cdar sent) :error)))
        ;; Without a valid request id nothing is answered at all.
        (setq sent nil)
        (mevedel-collaboration--handle-artifact-comment
         room 1 (plist-put (copy-tree mevedel-test--artifact-comment-frame) :reqId "x"))
        (should-not sent)
        (should (= 1 (length (mevedel-session-pending-follow-ups
                              (plist-get room :session)))))))))

(provide 'test-mevedel-collaboration-artifact)
;;; test-mevedel-collaboration-artifact.el ends here
