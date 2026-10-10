;;; test-mevedel-collaboration-artifact-comments.el --- artifact comment tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Anchors, the per-artifact comment store, guest actions and the routing
;; of assistant requests for comments on store artifacts.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-artifact-store)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-collaboration-artifact-comments)
(require 'mevedel-pending-inputs)
(require 'mevedel-structs)
(require 'mevedel-transcript-audit)
(require 'mevedel-view)
(require 'mevedel-workspace)

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


(mevedel-deftest mevedel-collaboration--artifact-comment-context
  (:doc "keeps bounded excerpts and drops oversized or malformed ones")
  (progn
    (should (equal '(:text "Two cards" :html "<div/>")
                   (mevedel-collaboration--artifact-comment-context
                    '(:text "Two cards" :html "<div/>" :evil "x"))))
    (should (equal '(:html "<p/>")
                   (mevedel-collaboration--artifact-comment-context
                    (list :text (make-string 4001 ?x) :html "<p/>"))))
    (should-not (mevedel-collaboration--artifact-comment-context '(:text 7)))
    (should-not (mevedel-collaboration--artifact-comment-context "text"))))

(mevedel-deftest mevedel-collaboration--artifact-comment-snapshot ()
  ,test
  (test)
  :doc "names the file, target, quote, box, excerpts and the whole thread"
  (let ((snapshot (mevedel-collaboration--artifact-comment-snapshot
                   '(:artifact "schema.html" :artifact-path "/tmp/a/schema.html")
                   '(:id "c" :actor "Alice" :text "Make it a row"
                     :anchor (:kind "box" :selector "main" :label "Schema › area · 2 elements"
                              :quote "share" :region (:x0 0 :y0 0 :x1 0.5 :y1 1) :count 2)
                     :context (:text "Two cards" :html "<div>Two cards</div>")
                     :replies [(:id "r" :actor "Bob" :text "Of four")]))))
    (should (string-match-p "^Artifact schema.html$" snapshot))
    (should (string-match-p "^File: /tmp/a/schema.html$" snapshot))
    (should (string-match-p "^Target: Schema › area · 2 elements$" snapshot))
    (should (string-match-p "^Selected text: \"share\"$" snapshot))
    (should (string-match-p "x 0-0.5, y 0-1 .*(2 elements covered)" snapshot))
    (should (string-match-p "```html\n<div>Two cards</div>\n```" snapshot))
    (should (string-match-p "Comment thread:\n- Alice: Make it a row\n- Bob: Of four\\'" snapshot)))
  :doc "describes a message about the whole artifact by file and scope only"
  (let ((snapshot (mevedel-collaboration--artifact-comment-snapshot
                   '(:artifact "a.html" :artifact-path "/tmp/a.html") nil)))
    (should (equal snapshot "Artifact a.html\nFile: /tmp/a.html\nScope: the whole artifact"))))

(defun mevedel-test--artifact-comment-store (workspace id file &optional content)
  "Create WORKSPACE's store artifact ID whose FILE holds CONTENT.
Without CONTENT the artifact's file is missing.  Return the file's path."
  (let ((path (file-name-concat (mevedel-artifact-store-directory workspace) id file)))
    (make-directory (file-name-directory path) t)
    (mevedel-artifact-store-create-meta workspace id file)
    (when content (write-region content nil path nil 'silent))
    path))

(defmacro mevedel-test--with-artifact-comment-room (&rest body)
  "Run BODY with ROOM: a live PID-lock session, a writable guest and artifacts.
DIRECTORY is the workspace root, WORKSPACE its file workspace with store
artifacts schema, notes and gone, and SENT collects outgoing frames."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (let* (;; The room's records are this fixture's, not a projection.
            (mevedel-artifact-store-changed-functions nil)
            (directory (file-name-as-directory
                        (make-temp-file "mevedel-artifact-comments-" t)))
            (workspace (mevedel-workspace--create :type 'file :id "artifact-comments"
                                                  :root directory))
            (session (mevedel-session-create "main" workspace))
            (schema (mevedel-test--artifact-comment-store
                     workspace "schema" "schema.html" "<p>schema</p>"))
            (room (list :session session :data-buffer data-buf :transport 'transport
                        :guests (make-hash-table :test #'eql)
                        :records (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                                             :artifact "schema/schema.html"
                                             :artifact-path schema)
                                       (list :id "tool-2" :kind "tool" :name "ApplyPatch"
                                             :artifact "notes/notes.md"
                                             :artifact-path (mevedel-test--artifact-comment-store
                                                             workspace "notes" "notes.md" "n"))
                                       (list :id "tool-3" :kind "tool" :name "ApplyPatch"
                                             :artifact "gone/gone.html"
                                             :artifact-path (mevedel-test--artifact-comment-store
                                                             workspace "gone" "gone.html")
                                             :missing t)
                                       (list :id "tool-4" :kind "tool" :name "ApplyPatch"
                                             :artifact "schema/extra.html"
                                             :artifact-path (file-name-concat
                                                             (file-name-directory schema)
                                                             "extra.html")))))
            (mevedel-collaboration--rooms (mevedel-test-room-registry room))
            (guest (list :name "Alice" :guest-id "alice" :writable t :role "full"))
            sent)
       (puthash 1 guest (plist-get room :guests))
       (setf (mevedel-session-save-path session) (file-name-concat directory "session")
             (mevedel-session-authority-mode session) 'pid-lock)
       (with-current-buffer data-buf
         (setq-local mevedel--session session mevedel--workspace workspace))
       (mevedel-session-set-pending-input-paused session t)
       (unwind-protect
           (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                      (lambda (_transport peer frame) (push (cons peer frame) sent) t))
                     ;; The fake save path has no committed sidecar to update.
                     ((symbol-function 'mevedel-recovery-save-now) #'ignore))
             ,@body)
         (delete-directory directory t)))))

(defconst mevedel-test--artifact-comment-anchor
  '(:kind "word" :selector "#hero > p:nth-of-type(1)"
    :label "SNT Schema v2 › word \"share\"" :quote "share" :start 9
    :sig (:tag "p" :h "0123456789abcdef"))
  "A well-formed guest anchor on the schema.html artifact.")

(defun mevedel-test--artifact-comment-post (&rest fields)
  "Return a post frame for schema.html overridden by FIELDS."
  (let ((frame (list :t "artifact-comment" :reqId 3 :action "post" :id "tool-1"
                     :commentId "0123456789abcdef0123" :text "  Make this bigger  "
                     :anchor (copy-tree mevedel-test--artifact-comment-anchor)
                     :context '(:text "countries share the same tables")
                     :toAssistant t)))
    (cl-loop for (key value) on fields by #'cddr
             do (setq frame (plist-put frame key value)))
    frame))

(mevedel-deftest mevedel-collaboration--artifact-comments-read ()
  ,test
  (test)
  :doc "round-trips the store and treats a missing one as no comments"
  (mevedel-test--with-artifact-comment-room
    (should-not (mevedel-collaboration--artifact-comments-read workspace "schema"))
    (mevedel-collaboration--artifact-comments-write
     workspace "schema" (list '(:id "c" :actor "Alice" :text "Hi" :resolved :json-false
                                :replies [])))
    (should (file-exists-p (file-name-concat directory ".mevedel/artifacts/.state/schema/comments.json")))
    (let ((comment (car (mevedel-collaboration--artifact-comments-read workspace "schema"))))
      (should (equal (plist-get comment :text) "Hi"))
      (should (eq (plist-get comment :resolved) :json-false)))
    ;; Comments are no whiteboard or document.
    (should-not (mevedel-shared-editing-list workspace)))
  :doc "refuses a store that belongs to another artifact"
  (mevedel-test--with-artifact-comment-room
    (let ((path (file-name-concat directory ".mevedel/artifacts/.state/schema/comments.json")))
      (write-region "{\"artifact\":\"other\",\"comments\":[]}" nil path nil 'silent)
      (should-error (mevedel-collaboration--artifact-comments-read workspace "schema")))))

(mevedel-deftest mevedel-collaboration--artifact-comment-action ()
  ,test
  (test)
  :doc "posts a comment, publishes it and sends it to the artifact's own conversation"
  (mevedel-test--with-artifact-comment-room
    (let* ((reply (mevedel-collaboration--artifact-comment-action
                   room guest (mevedel-test--artifact-comment-post)))
           (entry (car (mevedel-session-pending-follow-ups session)))
           (shared (plist-get entry :shared-question))
           (stored (car (mevedel-collaboration--artifact-comments-read workspace "schema")))
           (broadcast (cdr (cl-find 0 sent :key #'car))))
      (should (equal (plist-get reply :commentId) "0123456789abcdef0123"))
      (should (plist-get reply :queued))
      (should (equal (plist-get stored :text) "Make this bigger"))
      (should (equal (plist-get stored :actor) "Alice"))
      (should (equal (plist-get (plist-get stored :anchor) :quote) "share"))
      (should (equal (plist-get stored :context) '(:text "countries share the same tables")))
      ;; Guests receive the list without the stored excerpts.
      (should (equal (plist-get broadcast :t) "artifact-comments"))
      (should (equal (plist-get broadcast :artifact) "schema"))
      (should-not (plist-member (aref (plist-get broadcast :comments) 0) :context))
      ;; The request is an item conversation about the artifact.
      (should (equal (plist-get shared :itemId) "artifact:schema"))
      ;; The room's session answers the thread and is attached.
      (should (equal (plist-get reply :session) (mevedel-session-session-id session)))
      (should (equal (plist-get stored :session) (mevedel-session-session-id session)))
      (should (equal '("schema") (mevedel-session-attached-artifacts session)))
      (should (equal (plist-get shared :commentId) "0123456789abcdef0123"))
      (should (equal (plist-get shared :questionId) "0123456789abcdef0123"))
      (should (equal (plist-get shared :scope) "selection"))
      (should (string-prefix-p
               (concat "Make this bigger\n\nShared content snapshot (user-provided data):\n"
                       "Artifact schema/schema.html\nFile: " schema)
               (plist-get entry :input)))
      (should (equal (plist-get (mevedel-transcript-audit-shared-context
                                 (plist-get entry :input) shared)
                                :label)
                     "Artifact comment · schema/schema.html · SNT Schema v2 › word \"share\""))
      ;; A retried post neither stores nor queues it twice.
      (mevedel-collaboration--artifact-comment-action
       room guest (mevedel-test--artifact-comment-post))
      (should (= 1 (length (mevedel-collaboration--artifact-comments-read workspace "schema"))))
      (should (= 1 (length (mevedel-session-pending-follow-ups session))))))
  :doc "keeps people-only comments and replies out of the model, and threads replies"
  (mevedel-test--with-artifact-comment-room
    (mevedel-collaboration--artifact-comment-action
     room guest (mevedel-test--artifact-comment-post :toAssistant :json-false))
    (should-not (mevedel-session-pending-follow-ups session))
    (let ((reply (mevedel-collaboration--artifact-comment-action
                  room guest (list :action "reply" :id "tool-1"
                                   :commentId "0123456789abcdef0123"
                                   :replyId "fedcba9876543210fedc" :text "And bolder"
                                   :toAssistant t))))
      (should (equal (plist-get reply :replyId) "fedcba9876543210fedc"))
      (let* ((entry (car (mevedel-session-pending-follow-ups session)))
             (shared (plist-get entry :shared-question)))
        (should (equal (plist-get shared :questionId) "fedcba9876543210fedc"))
        (should (equal (plist-get shared :text) "And bolder"))
        (should (string-match-p "- Alice: Make this bigger\n- Alice: And bolder"
                                (plist-get entry :input)))))
    (should (= 1 (length (plist-get (car (mevedel-collaboration--artifact-comments-read
                                          workspace "schema"))
                                    :replies)))))
  :doc "resolves and reopens, and refuses replies to a resolved comment"
  (mevedel-test--with-artifact-comment-room
    (mevedel-collaboration--artifact-comment-action
     room guest (mevedel-test--artifact-comment-post :toAssistant :json-false))
    (should (eq t (plist-get (mevedel-collaboration--artifact-comment-action
                              room guest (list :action "resolve" :id "tool-1"
                                               :commentId "0123456789abcdef0123" :resolved t))
                             :resolved)))
    (should (eq t (plist-get (car (mevedel-collaboration--artifact-comments-read
                                   workspace "schema"))
                             :resolved)))
    (should-error (mevedel-collaboration--artifact-comment-action
                   room guest (list :action "reply" :id "tool-1"
                                    :commentId "0123456789abcdef0123"
                                    :replyId "fedcba9876543210fedc" :text "Late")))
    (mevedel-collaboration--artifact-comment-action
     room guest (list :action "resolve" :id "tool-1" :commentId "0123456789abcdef0123"
                      :resolved :json-false))
    (should (eq :json-false (plist-get (car (mevedel-collaboration--artifact-comments-read
                                             workspace "schema"))
                                       :resolved))))
  :doc "asks about the whole artifact without storing a comment"
  (mevedel-test--with-artifact-comment-room
    (mevedel-collaboration--artifact-comment-action
     room guest (list :action "ask" :id "tool-1" :questionId "abcdefabcdefabcdef12"
                      :text "Tighten the intro"))
    (let* ((entry (car (mevedel-session-pending-follow-ups session)))
           (shared (plist-get entry :shared-question)))
      (should (equal (plist-get shared :scope) "whole"))
      (should-not (plist-get shared :commentId))
      (should (string-suffix-p "Scope: the whole artifact" (plist-get entry :input)))
      (should (equal (plist-get (mevedel-transcript-audit-shared-context
                                 (plist-get entry :input) shared)
                                :label)
                     "Artifact · schema/schema.html · Whole artifact")))
    (should-not (mevedel-collaboration--artifact-comments-read workspace "schema")))
  :doc "carries the sender's files with a message about the whole artifact"
  (mevedel-test--with-artifact-comment-room
    (let ((media (file-name-as-directory (make-temp-file "mevedel-artifact-files-" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--media-dir) (lambda () media)))
            (with-current-buffer data-buf (setq-local mevedel--view-buffer view-buf))
            (mevedel-collaboration--artifact-comment-action
             room guest (list :action "ask" :id "tool-1" :questionId "abcdefabcdefabcdef34"
                              :text "Match this" :images
                              (list (list :mime "text/plain"
                                          :data (base64-encode-string "notes" t)))))
            (let ((paths (plist-get (car (mevedel-session-pending-follow-ups session))
                                    :guest-paths)))
              (should (= 1 (length paths)))
              (should (string-prefix-p media (car paths)))))
        (delete-directory media t))))
  :doc "lists for view links but refuses their writes, bad input and non-HTML artifacts"
  (mevedel-test--with-artifact-comment-room
    (let ((viewer '(:name "V" :writable nil)))
      (should (equal [] (plist-get (mevedel-collaboration--artifact-comment-action
                                    room viewer (list :action "list" :id "tool-1"))
                                   :comments)))
      (dolist (case (list (list viewer (mevedel-test--artifact-comment-post) "not comment")
                          (list guest (mevedel-test--artifact-comment-post :commentId "bad id")
                                "identity")
                          (list guest (mevedel-test--artifact-comment-post :id "tool-2") "Only HTML")
                          (list guest (mevedel-test--artifact-comment-post :id "tool-3")
                                "not published")
                          (list guest (mevedel-test--artifact-comment-post :id "nope")
                                "not published")
                          (list guest (mevedel-test--artifact-comment-post :id "tool-4")
                                "main file")
                          (list guest (mevedel-test--artifact-comment-post :text "   ")
                                "message is required")
                          (list guest (mevedel-test--artifact-comment-post :anchor '(:label "x"))
                                "could not be identified")
                          (list guest (mevedel-test--artifact-comment-post :action "evil")
                                "Unknown")))
        (let ((err (should-error (mevedel-collaboration--artifact-comment-action
                                  room (car case) (nth 1 case)))))
          (should (string-match-p (nth 2 case) (error-message-string err)))))
      (should-not (mevedel-collaboration--artifact-comments-read workspace "schema"))
      (should-not (mevedel-session-pending-follow-ups session)))))

(mevedel-deftest mevedel-collaboration--artifact-answering-buffer ()
  ,test
  (test)
  :doc "keeps a thread with its session, else the room's, else the dedicated one"
  (mevedel-test--with-artifact-comment-room
    (let ((other (generate-new-buffer " *other-session*"))
          (dedicated (generate-new-buffer " *dedicated-session*"))
          (lobby (list :workspace workspace :transport 'transport
                       :guests (make-hash-table :test #'eql))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-artifact-store-session-buffer)
                     (lambda (_workspace id) (and (equal id "other") other)))
                    ((symbol-function 'mevedel-artifact-store-conversation)
                     (lambda (_workspace _id) dedicated)))
            (should (eq data-buf (mevedel-collaboration--artifact-answering-buffer
                                  room "schema" nil)))
            (should (eq data-buf (mevedel-collaboration--artifact-answering-buffer
                                  room "schema" (mevedel-session-session-id session))))
            (should (eq other (mevedel-collaboration--artifact-answering-buffer
                               room "schema" "other")))
            ;; A thread whose session is gone falls back to the dedicated one.
            (should (eq dedicated (mevedel-collaboration--artifact-answering-buffer
                                   room "schema" "gone")))
            ;; The lobby has no session of its own.
            (should (eq dedicated (mevedel-collaboration--artifact-answering-buffer
                                   lobby "schema" nil)))
            (should (eq other (mevedel-collaboration--artifact-answering-buffer
                               lobby "schema" "other"))))
        (kill-buffer other)
        (kill-buffer dedicated)))))

(mevedel-deftest mevedel-collaboration--artifact-question-known-p
  (:doc "finds queued and delivered artifact requests by identity")
  (mevedel-test--with-artifact-comment-room
    (should-not (mevedel-collaboration--artifact-question-known-p
                 data-buf "0123456789abcdef0123"))
    (mevedel-collaboration--artifact-comment-action
     room guest (mevedel-test--artifact-comment-post))
    (should (mevedel-collaboration--artifact-question-known-p data-buf "0123456789abcdef0123"))
    (mevedel-session-set-pending-input-paused session nil)
    (cl-letf (((symbol-function 'gptel-send) #'ignore))
      (mevedel-view--drain-follow-up data-buf))
    (should-not (mevedel-session-pending-follow-ups session))
    (should (mevedel-collaboration--artifact-question-known-p data-buf "0123456789abcdef0123"))
    (should-not (mevedel-collaboration--artifact-question-known-p data-buf "other"))))

(mevedel-deftest mevedel-collaboration--handle-artifact-comment
  (:doc "answers the sender with a receipt or a refusal and never signals")
  (mevedel-test--with-artifact-comment-room
    (puthash 1 guest (plist-get room :guests))
    (puthash 2 (list :name "Viewer" :writable nil :ready t) (plist-get room :guests))
    (mevedel-collaboration--handle-artifact-comment room 1 (mevedel-test--artifact-comment-post))
    (let ((answer (cdr (cl-find 1 sent :key #'car))))
      (should (equal (plist-get answer :t) "artifact-comment"))
      (should (equal (plist-get answer :reqId) 3))
      (should (plist-get answer :queued)))
    (setq sent nil)
    (mevedel-collaboration--handle-artifact-comment
     room 2 (mevedel-test--artifact-comment-post :commentId "fedcba9876543210fedc"))
    (should (stringp (plist-get (cdar sent) :error)))
    ;; Without a valid request id nothing is answered at all.
    (setq sent nil)
    (mevedel-collaboration--handle-artifact-comment
     room 1 (mevedel-test--artifact-comment-post :reqId "x"))
    (should-not sent)))

(provide 'test-mevedel-collaboration-artifact-comments)
;;; test-mevedel-collaboration-artifact-comments.el ends here
