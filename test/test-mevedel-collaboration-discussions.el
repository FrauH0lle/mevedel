;;; test-mevedel-collaboration-discussions.el --- Discussion tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Discussions about store items: targets, the HTML artifact store, guest
;; actions, which session answers, the request the model receives, and
;; resolving a selection sent alone.

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
(require 'mevedel-collaboration-discussions)
(require 'mevedel-pending-inputs)
(require 'mevedel-structs)
(require 'mevedel-transcript-audit)
(require 'mevedel-turn)
(require 'mevedel-view)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-collaboration-discussions--artifact-anchor ()
  ,test
  (test)
  :doc "rebuilds guest anchors from known bounded fields only"
  (should (equal (mevedel-collaboration-discussions--artifact-anchor
                  '(:kind "box" :selector "main > section:nth-of-type(2)"
                    :label "Schema › area · 3 elements"
                    :sig (:tag "section" :h "00ff00ff00ff00ff" :evil "x")
                    :quote "share" :start 12
                    :region (:x0 0 :y0 0.25 :x1 0.5 :y1 1)
                    :count 3 :script "alert(1)"))
                 '(:kind "box" :selector "main > section:nth-of-type(2)"
                   :label "Schema › area · 3 elements"
                   :sig (:tag "section" :h "00ff00ff00ff00ff")
                   :quote "share" :start 12
                   :region (:x0 0 :y0 0.25 :x1 0.5 :y1 1)
                   :count 3)))
  :doc "drops malformed optional parts and defaults an unknown kind"
  (should (equal (mevedel-collaboration-discussions--artifact-anchor
                  '(:kind "evil" :selector "p" :label nil
                    :sig (:tag "Bad Tag" :h "zz")
                    :start -4 :quote "x"
                    :region (:x0 0.5 :y0 0 :x1 0.2 :y1 1)))
                 '(:kind "element" :selector "p" :label "" :quote "x" :start 0)))
  :doc "refuses anchors without a usable selector"
  (progn
    (should-not (mevedel-collaboration-discussions--artifact-anchor '(:label "x")))
    (should-not (mevedel-collaboration-discussions--artifact-anchor
                 (list :selector (make-string 1001 ?a))))
    (should-not (mevedel-collaboration-discussions--artifact-anchor '("p")))
    (should-not (mevedel-collaboration-discussions--artifact-anchor nil))))

(mevedel-deftest mevedel-collaboration-discussions--artifact-context
  (:doc "keeps bounded excerpts and drops oversized or malformed ones")
  (progn
    (should (equal '(:text "Two cards" :html "<div/>")
                   (mevedel-collaboration-discussions--artifact-context
                    '(:text "Two cards" :html "<div/>" :evil "x"))))
    (should (equal '(:html "<p/>")
                   (mevedel-collaboration-discussions--artifact-context
                    (list :text (make-string 4001 ?x) :html "<p/>"))))
    (should-not (mevedel-collaboration-discussions--artifact-context '(:text 7)))
    (should-not (mevedel-collaboration-discussions--artifact-context "text"))))

(mevedel-deftest mevedel-collaboration-discussions--artifact-capture ()
  ,test
  (test)
  :doc "names the file, target, quote, box and excerpts"
  (let ((capture (mevedel-collaboration-discussions--artifact-capture
                  '(:artifact "schema.html" :artifact-path "/tmp/a/schema.html")
                  '(:id "c"
                    :target (:kind "box" :selector "main" :label "Schema › area · 2 elements"
                             :quote "share" :region (:x0 0 :y0 0 :x1 0.5 :y1 1) :count 2)
                    :context (:text "Two cards" :html "<div>Two cards</div>")))))
    (should (string-match-p "^Artifact schema.html$" capture))
    (should (string-match-p "^File: /tmp/a/schema.html$" capture))
    (should (string-match-p "^Target: Schema › area · 2 elements$" capture))
    (should (string-match-p "^Selected text: \"share\"$" capture))
    (should (string-match-p "x 0-0.5, y 0-1 .*(2 elements covered)" capture))
    (should (string-match-p "```html\n<div>Two cards</div>\n```\\'" capture)))
  :doc "describes the whole artifact by file and scope only"
  (should (equal (mevedel-collaboration-discussions--artifact-capture
                  '(:artifact "a.html" :artifact-path "/tmp/a.html")
                  '(:id "general" :target (:kind "whole")))
                 "Artifact a.html\nFile: /tmp/a.html\nScope: the whole artifact")))

(mevedel-deftest mevedel-collaboration-discussions-alone-request
  (:doc "A selection sent alone is the request; documents and artifacts drop it, boards keep it")
  (progn
    (let ((board (mevedel-collaboration-discussions-alone-request "whiteboard")))
      (should (string-match-p "selected objects" board))
      (should (string-match-p "leave those objects on the board" board)))
    (let ((document (mevedel-collaboration-discussions-alone-request "document")))
      (should (string-match-p "selected passage" document))
      (should (string-match-p "remove that instruction text from the document" document)))
    (let ((artifact (mevedel-collaboration-discussions-alone-request "artifact")))
      (should (string-match-p "selected part" artifact))
      (should (string-match-p "remove that instruction text from the artifact" artifact)))
    (dolist (kind '("whiteboard" "document" "artifact"))
      (should (string-match-p "ask what the sender wants"
                              (mevedel-collaboration-discussions-alone-request kind))))))

(mevedel-deftest mevedel-collaboration-discussions--thread
  (:doc "lists messages oldest first, keeps the newest within bounds and shows alone sends")
  (let ((mevedel-collaboration-discussions--thread-chars 60))
    (should (equal (mevedel-collaboration-discussions--thread
                    '(:messages [(:actor "Alice" :text "first message that is quite long")
                                 (:actor "Bob" :text "" :alone t)
                                 (:actor "Alice" :text "last")]))
                   (concat "Discussion:\n- Bob: "
                           mevedel-collaboration-discussions-alone-text
                           "\n- Alice: last")))))

(defun mevedel-test--discussion-store (workspace id file &optional content)
  "Create WORKSPACE's store artifact ID whose FILE holds CONTENT.
Without CONTENT the artifact's file is missing.  Return the file's path."
  (let ((path (file-name-concat (mevedel-artifact-store-directory workspace) id file)))
    (make-directory (file-name-directory path) t)
    (mevedel-artifact-store-create-meta workspace id file)
    (when content (write-region content nil path nil 'silent))
    path))

(defmacro mevedel-test--with-discussion-room (&rest body)
  "Run BODY with ROOM: a live PID-lock session, a writable guest and artifacts.
DIRECTORY is the workspace root, WORKSPACE its file workspace with store
artifacts schema, notes and gone, and SENT collects outgoing frames."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (let* (;; The room's records are this fixture's, not a projection.
            (mevedel-artifact-store-changed-functions nil)
            (directory (file-name-as-directory
                        (make-temp-file "mevedel-discussions-" t)))
            (workspace (mevedel-workspace--create :type 'file :id "discussions"
                                                  :root directory))
            (session (mevedel-session-create "main" workspace))
            (schema (mevedel-test--discussion-store
                     workspace "schema" "schema.html" "<p>schema</p>"))
            (room (list :session session :data-buffer data-buf :transport 'transport
                        :guests (make-hash-table :test #'eql)
                        :records (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                                             :artifact "schema/schema.html"
                                             :artifact-path schema)
                                       (list :id "tool-2" :kind "tool" :name "ApplyPatch"
                                             :artifact "notes/notes.md"
                                             :artifact-path (mevedel-test--discussion-store
                                                             workspace "notes" "notes.md" "n"))
                                       (list :id "tool-3" :kind "tool" :name "ApplyPatch"
                                             :artifact "gone/gone.html"
                                             :artifact-path (mevedel-test--discussion-store
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

(defconst mevedel-test--discussion-anchor
  '(:kind "word" :selector "#hero > p:nth-of-type(1)"
    :label "SNT Schema v2 › word \"share\"" :quote "share" :start 9
    :sig (:tag "p" :h "0123456789abcdef"))
  "A well-formed guest anchor on the schema.html artifact.")

(defun mevedel-test--discussion-post (&rest fields)
  "Return a post frame for schema.html overridden by FIELDS."
  (let ((frame (list :t "discussion" :reqId 3 :action "post" :item "tool-1"
                     :discussionId "0123456789abcdef0123"
                     :messageId "1123456789abcdef0123"
                     :text "  Make this bigger  "
                     :target (copy-tree mevedel-test--discussion-anchor)
                     :context '(:text "countries share the same tables")
                     :toAssistant t)))
    (cl-loop for (key value) on fields by #'cddr
             do (setq frame (plist-put frame key value)))
    frame))

(defun mevedel-test--discussion-act (room guest args)
  "Perform discussion ARGS for GUEST in ROOM; return the reply or signal its error."
  (let (reply)
    (mevedel-collaboration-discussions-perform room guest args (lambda (r) (setq reply r)))
    (when (plist-get reply :error) (error "%s" (plist-get reply :error)))
    reply))

(defun mevedel-test--discussions (workspace id)
  "Return the stored discussions of WORKSPACE's artifact ID."
  (mevedel-collaboration-discussions--artifact-read workspace id))

(mevedel-deftest mevedel-collaboration-discussions--artifact-read ()
  ,test
  (test)
  :doc "round-trips the store, treats a missing one as none, and drops the old comment file"
  (mevedel-test--with-discussion-room
    (should-not (mevedel-test--discussions workspace "schema"))
    (let ((obsolete (mevedel-collaboration-discussions--artifact-file
                     workspace "schema" "comments.json")))
      (make-directory (file-name-directory obsolete) t)
      (write-region "{\"comments\":[]}" nil obsolete nil 'silent)
      (mevedel-collaboration-discussions--artifact-write
       workspace "schema" (list '(:id "c" :target (:kind "whole") :messages []
                                  :resolved :json-false)))
      (should-not (file-exists-p obsolete)))
    (should (file-exists-p (file-name-concat directory
                                             ".mevedel/artifacts/.state/schema/discussions.json")))
    (should (eq :json-false (plist-get (car (mevedel-test--discussions workspace "schema"))
                                       :resolved)))
    (should-not (mevedel-shared-editing-list workspace))))

(mevedel-deftest mevedel-collaboration-discussions-perform ()
  ,test
  (test)
  :doc "starts a discussion, publishes it and sends it to the artifact's conversation"
  (mevedel-test--with-discussion-room
    (let* ((reply (mevedel-test--discussion-act room guest (mevedel-test--discussion-post)))
           (entry (car (mevedel-session-pending-follow-ups session)))
           (shared (plist-get entry :shared-question))
           (stored (car (mevedel-test--discussions workspace "schema")))
           (message (aref (plist-get stored :messages) 0))
           (broadcast (cdr (cl-find 0 sent :key #'car))))
      (should (equal (plist-get reply :discussionId) "0123456789abcdef0123"))
      (should (equal (plist-get reply :messageId) "1123456789abcdef0123"))
      (should (plist-get reply :queued))
      (should (equal (plist-get message :text) "Make this bigger"))
      (should (equal (plist-get message :actor) "Alice"))
      (should (equal (plist-get (plist-get stored :target) :quote) "share"))
      (should (equal (plist-get stored :quote) "share"))
      (should (equal (plist-get stored :context) '(:text "countries share the same tables")))
      ;; Guests receive the list without the stored excerpts.
      (should (equal (plist-get broadcast :t) "discussions"))
      (should (equal (plist-get broadcast :item) "artifact:schema"))
      (should-not (plist-member (aref (plist-get broadcast :discussions) 0) :context))
      ;; The room's session answers and is recorded and attached.
      (should (equal (plist-get reply :session) (mevedel-session-session-id session)))
      (should (equal (plist-get (car (mevedel-test--discussions workspace "schema")) :session)
                     (mevedel-session-session-id session)))
      (should (equal '("schema") (mevedel-session-attached-artifacts session)))
      (should (equal (plist-get shared :itemId) "artifact:schema"))
      (should (equal (plist-get shared :kind) "artifact"))
      (should (equal (plist-get shared :discussionId) "0123456789abcdef0123"))
      (should (equal (plist-get shared :questionId) "1123456789abcdef0123"))
      (should (equal (plist-get shared :scope) "selection"))
      (should (string-prefix-p
               (concat "Make this bigger\n\nShared content snapshot (user-provided data):\n"
                       "Artifact schema/schema.html\nFile: " schema)
               (plist-get entry :input)))
      (should (string-suffix-p "Discussion:\n- Alice: Make this bigger" (plist-get entry :input)))
      (should (equal (plist-get (mevedel-transcript-audit-shared-context
                                 (plist-get entry :input) shared)
                                :label)
                     "Discussion · schema/schema.html · SNT Schema v2 › word \"share\""))
      ;; A retried post neither stores nor queues it twice.
      (mevedel-test--discussion-act room guest (mevedel-test--discussion-post))
      (should (= 1 (length (mevedel-test--discussions workspace "schema"))))
      (should (= 1 (length (plist-get (car (mevedel-test--discussions workspace "schema"))
                                      :messages))))
      (should (= 1 (length (mevedel-session-pending-follow-ups session))))))
  :doc "keeps notes out of the model, and threads replies"
  (mevedel-test--with-discussion-room
    (mevedel-test--discussion-act room guest (mevedel-test--discussion-post :toAssistant :json-false))
    (should-not (mevedel-session-pending-follow-ups session))
    (let ((reply (mevedel-test--discussion-act
                  room guest (list :action "reply" :item "tool-1"
                                   :discussionId "0123456789abcdef0123"
                                   :messageId "fedcba9876543210fedc" :text "And bolder"
                                   :toAssistant t))))
      (should (equal (plist-get reply :messageId) "fedcba9876543210fedc"))
      (let* ((entry (car (mevedel-session-pending-follow-ups session)))
             (shared (plist-get entry :shared-question)))
        (should (equal (plist-get shared :questionId) "fedcba9876543210fedc"))
        (should (equal (plist-get shared :text) "And bolder"))
        (should (string-match-p "- Alice: Make this bigger\n- Alice: And bolder"
                                (plist-get entry :input)))))
    (should (= 2 (length (plist-get (car (mevedel-test--discussions workspace "schema"))
                                    :messages))))
    ;; Asking again sends the discussion as it stands, with its own identity.
    (mevedel-test--discussion-act
     room guest (list :action "ask" :item "tool-1" :discussionId "0123456789abcdef0123"
                      :questionId "aaaaaaaaaaaaaaaaaaaa"))
    (should (equal "aaaaaaaaaaaaaaaaaaaa"
                   (plist-get (plist-get (car (last (mevedel-session-pending-follow-ups session)))
                                         :shared-question)
                              :questionId))))
  :doc "resolves and reopens, refusing additions to a resolved discussion"
  (mevedel-test--with-discussion-room
    (mevedel-test--discussion-act room guest (mevedel-test--discussion-post :toAssistant :json-false))
    (should (eq t (plist-get (mevedel-test--discussion-act
                              room guest (list :action "resolve" :item "tool-1"
                                               :discussionId "0123456789abcdef0123" :resolved t))
                             :resolved)))
    (let ((stored (car (mevedel-test--discussions workspace "schema"))))
      (should (eq t (plist-get stored :resolved)))
      (should (equal "Alice" (plist-get stored :resolvedBy))))
    (should-error (mevedel-test--discussion-act
                   room guest (list :action "reply" :item "tool-1"
                                    :discussionId "0123456789abcdef0123"
                                    :messageId "fedcba9876543210fedc" :text "Late")))
    (mevedel-test--discussion-act
     room guest (list :action "resolve" :item "tool-1" :discussionId "0123456789abcdef0123"
                      :resolved :json-false))
    (should (eq :json-false (plist-get (car (mevedel-test--discussions workspace "schema"))
                                       :resolved))))
  :doc "collects whole-artifact messages in the general discussion, which stays open"
  (mevedel-test--with-discussion-room
    (dolist (id '("abcdefabcdefabcdef12" "abcdefabcdefabcdef13"))
      (mevedel-test--discussion-act
       room guest (list :action "post" :item "artifact:schema" :target '(:kind "whole")
                        :messageId id :text "Tighten the intro" :toAssistant t)))
    (let* ((stored (mevedel-test--discussions workspace "schema"))
           (general (car stored))
           (entry (car (mevedel-session-pending-follow-ups session)))
           (shared (plist-get entry :shared-question)))
      (should (= 1 (length stored)))
      (should (equal "general" (plist-get general :id)))
      (should (= 2 (length (plist-get general :messages))))
      (should (equal (plist-get shared :scope) "whole"))
      (should (equal (plist-get shared :discussionId) "general"))
      (should (string-suffix-p "Scope: the whole artifact" (plist-get entry :input)))
      (should (equal (plist-get (mevedel-transcript-audit-shared-context
                                 (plist-get entry :input) shared)
                                :label)
                     "Discussion · schema/schema.html · Whole item")))
    (should-error (mevedel-test--discussion-act
                   room guest (list :action "resolve" :item "tool-1" :discussionId "general"
                                    :resolved t)))
    ;; The whole artifact has no instruction of its own.
    (let ((err (should-error (mevedel-test--discussion-act
                              room guest (list :action "post" :item "tool-1"
                                               :target '(:kind "whole")
                                               :messageId "abcdefabcdefabcdef14"
                                               :text "  " :toAssistant t)))))
      (should (string-match-p "message is required" (error-message-string err)))))
  :doc "sends a selection alone to the assistant, but never alone as a note"
  (mevedel-test--with-discussion-room
    (mevedel-test--discussion-act room guest (mevedel-test--discussion-post :text "   "))
    (let* ((entry (car (mevedel-session-pending-follow-ups session)))
           (shared (plist-get entry :shared-question))
           (message (aref (plist-get (car (mevedel-test--discussions workspace "schema"))
                                     :messages)
                          0)))
      (should (eq t (plist-get message :alone)))
      (should (equal "" (plist-get message :text)))
      (should (eq t (plist-get shared :alone)))
      (should (string-prefix-p (mevedel-collaboration-discussions-alone-request "artifact")
                               (plist-get entry :input)))
      (should (string-match-p (regexp-quote mevedel-collaboration-discussions-alone-text)
                              (plist-get entry :input))))
    (let ((err (should-error (mevedel-test--discussion-act
                              room guest (mevedel-test--discussion-post
                                          :discussionId "2123456789abcdef0123"
                                          :messageId "3123456789abcdef0123"
                                          :text nil :toAssistant :json-false)))))
      (should (string-match-p "message is required" (error-message-string err))))
    (should-error (mevedel-test--discussion-act
                   room guest (list :action "reply" :item "tool-1"
                                    :discussionId "0123456789abcdef0123"
                                    :messageId "fedcba9876543210fedc" :text ""
                                    :toAssistant t))))
  :doc "carries the sender's files with a message"
  (mevedel-test--with-discussion-room
    (let ((media (file-name-as-directory (make-temp-file "mevedel-discussion-files-" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--media-dir) (lambda () media)))
            (with-current-buffer data-buf (setq-local mevedel--view-buffer view-buf))
            (mevedel-test--discussion-act
             room guest (list :action "post" :item "tool-1" :target '(:kind "whole")
                              :messageId "abcdefabcdefabcdef34" :text "Match this"
                              :toAssistant t
                              :images (list (list :mime "text/plain"
                                                  :data (base64-encode-string "notes" t)))))
            (let ((paths (plist-get (car (mevedel-session-pending-follow-ups session))
                                    :guest-paths)))
              (should (= 1 (length paths)))
              (should (string-prefix-p media (car paths)))))
        (delete-directory media t))))
  :doc "lists for view links but refuses their writes, bad input and non-HTML artifacts"
  (mevedel-test--with-discussion-room
    (let ((viewer '(:name "V" :writable nil)))
      (should (equal [] (plist-get (mevedel-test--discussion-act
                                    room viewer (list :action "list" :item "tool-1"))
                                   :discussions)))
      (dolist (case (list (list viewer (mevedel-test--discussion-post) "not discuss")
                          (list guest (mevedel-test--discussion-post :discussionId "bad id")
                                "identity")
                          (list guest (mevedel-test--discussion-post :messageId nil) "identity")
                          (list guest (mevedel-test--discussion-post :item "tool-2") "Only HTML")
                          (list guest (mevedel-test--discussion-post :item "tool-3")
                                "not published")
                          (list guest (mevedel-test--discussion-post :item "nope")
                                "not published")
                          (list guest (mevedel-test--discussion-post :item "tool-4")
                                "main file")
                          (list guest (mevedel-test--discussion-post :target '(:label "x"))
                                "could not be identified")
                          (list guest (mevedel-test--discussion-post :text 7) "text")
                          (list guest (mevedel-test--discussion-post :action "evil")
                                "Unknown")))
        (let ((err (should-error (mevedel-test--discussion-act room (car case) (nth 1 case)))))
          (should (string-match-p (nth 2 case) (error-message-string err)))))
      (should-not (mevedel-test--discussions workspace "schema"))
      (should-not (mevedel-session-pending-follow-ups session)))))

(mevedel-deftest mevedel-collaboration-discussions-answering-buffer ()
  ,test
  (test)
  :doc "answers in the room's own session; the lobby follows the discussion"
  (mevedel-test--with-discussion-room
    (let ((other (generate-new-buffer " *other-session*"))
          (dedicated (generate-new-buffer " *dedicated-session*"))
          (lobby (list :workspace workspace :transport 'transport
                       :guests (make-hash-table :test #'eql))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-artifact-store-session-buffer)
                     (lambda (_workspace id) (and (equal id "other") other)))
                    ((symbol-function 'mevedel-artifact-store-conversation)
                     (lambda (_workspace _id) dedicated)))
            (should (eq data-buf (mevedel-collaboration-discussions-answering-buffer
                                  room "schema" nil)))
            ;; A room's links never prompt another session.
            (should (eq data-buf (mevedel-collaboration-discussions-answering-buffer
                                  room "schema" "other")))
            ;; A discussion whose session is gone falls back to the dedicated one.
            (should (eq dedicated (mevedel-collaboration-discussions-answering-buffer
                                   lobby "schema" "gone")))
            (should (eq dedicated (mevedel-collaboration-discussions-answering-buffer
                                   lobby "schema" nil)))
            (should (eq other (mevedel-collaboration-discussions-answering-buffer
                               lobby "schema" "other"))))
        (kill-buffer other)
        (kill-buffer dedicated)))))

(mevedel-deftest mevedel-collaboration-discussions--question-known-p
  (:doc "finds queued and delivered discussion requests by identity")
  (mevedel-test--with-discussion-room
    (should-not (mevedel-collaboration-discussions--question-known-p
                 data-buf "1123456789abcdef0123"))
    (mevedel-test--discussion-act room guest (mevedel-test--discussion-post))
    (should (mevedel-collaboration-discussions--question-known-p data-buf "1123456789abcdef0123"))
    (mevedel-session-set-pending-input-paused session nil)
    (cl-letf (((symbol-function 'gptel-send) #'ignore))
      (mevedel-view--drain-follow-up data-buf))
    (should-not (mevedel-session-pending-follow-ups session))
    (should (mevedel-collaboration-discussions--question-known-p data-buf "1123456789abcdef0123"))
    ;; The delivered prompt names the turn the session is now running.
    (should (equal "1123456789abcdef0123"
                   (plist-get (plist-get (with-current-buffer data-buf
                                           mevedel-view--turn-guest-attribution)
                                         :shared)
                              :questionId)))
    (should-not (mevedel-collaboration-discussions--question-known-p data-buf "other"))))

(mevedel-deftest mevedel-collaboration-discussions-turn-settled ()
  ,test
  (test)
  :doc "resolves a selection sent alone once its turn completes, attributed to the assistant"
  (mevedel-test--with-discussion-room
    (mevedel-test--discussion-act room guest (mevedel-test--discussion-post :text nil))
    (let ((shared (plist-get (car (mevedel-session-pending-follow-ups session)) :shared-question)))
      (with-current-buffer data-buf
        (setq mevedel-view--turn-guest-attribution (list :shared shared))
        (run-hook-with-args 'mevedel-turn-settled-functions 'success)
        (should-not mevedel-view--turn-guest-attribution)))
    (let ((stored (car (mevedel-test--discussions workspace "schema"))))
      (should (eq t (plist-get stored :resolved)))
      (should (equal "Assistant" (plist-get stored :resolvedBy)))))
  :doc "leaves it open when the turn fails or is cancelled"
  (mevedel-test--with-discussion-room
    (mevedel-test--discussion-act room guest (mevedel-test--discussion-post :text nil))
    (let ((shared (plist-get (car (mevedel-session-pending-follow-ups session)) :shared-question)))
      (dolist (outcome '(error abort))
        (with-current-buffer data-buf
          (setq mevedel-view--turn-guest-attribution (list :shared shared))
          (run-hook-with-args 'mevedel-turn-settled-functions outcome)
          (should-not mevedel-view--turn-guest-attribution))))
    (should-not (eq t (plist-get (car (mevedel-test--discussions workspace "schema"))
                                 :resolved))))
  :doc "never resolves a discussion with a written message"
  (mevedel-test--with-discussion-room
    (mevedel-test--discussion-act room guest (mevedel-test--discussion-post))
    (let ((shared (plist-get (car (mevedel-session-pending-follow-ups session)) :shared-question)))
      (with-current-buffer data-buf
        (setq mevedel-view--turn-guest-attribution (list :shared shared))
        (run-hook-with-args 'mevedel-turn-settled-functions 'success)))
    (should-not (eq t (plist-get (car (mevedel-test--discussions workspace "schema"))
                                 :resolved)))))

(mevedel-deftest mevedel-collaboration-discussions-handle
  (:doc "answers the sender with a receipt or a refusal and never signals")
  (mevedel-test--with-discussion-room
    (puthash 2 (list :name "Viewer" :writable nil :ready t) (plist-get room :guests))
    (mevedel-collaboration-discussions-handle room 1 (mevedel-test--discussion-post))
    (let ((answer (cdr (cl-find 1 sent :key #'car))))
      (should (equal (plist-get answer :t) "discussion"))
      (should (equal (plist-get answer :reqId) 3))
      (should (plist-get answer :queued)))
    (setq sent nil)
    (mevedel-collaboration-discussions-handle
     room 2 (mevedel-test--discussion-post :discussionId "fedcba9876543210fedc"))
    (should (stringp (plist-get (cdar sent) :error)))
    ;; Without a valid request id nothing is answered at all.
    (setq sent nil)
    (mevedel-collaboration-discussions-handle
     room 1 (mevedel-test--discussion-post :reqId "x"))
    (should-not sent)))

(provide 'test-mevedel-collaboration-discussions)
;;; test-mevedel-collaboration-discussions.el ends here
