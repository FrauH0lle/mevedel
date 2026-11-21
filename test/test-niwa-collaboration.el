;;; test-niwa-collaboration.el --- Collaboration facade and projection tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the collaboration facade, publication, lifecycle, and public commands.

;;; Code:

(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'niwa-agent-control)
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-task)
(require 'niwa-collaboration-transport)
(require 'niwa-collaboration)
(require 'niwa-collaboration-artifact-room)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-lobby)
(require 'niwa-pending-inputs)
(require 'niwa-permission-queue)
(require 'niwa-prompt-submission)
(require 'niwa-artifact-store)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-transcript)
(require 'niwa-transcript-audit)
(require 'niwa-chat)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-composer)
(require 'niwa-view-disclosure)
(require 'niwa-view-input-files)
(require 'niwa-view-render)
(require 'niwa-workspace)
(require 'niwa-skills-invoke)
(require 'niwa-skills-ui)


;;
;;; Credentials and links

(niwa-deftest niwa-collaboration--base64url
  (:doc "encodes URL-safe unpadded base64, decodes it back, and returns nil for malformed input")
  (progn
    (dolist (count '(16 32 48))
      (let* ((bytes (niwa-collaboration--random-bytes count))
             (encoded (niwa-collaboration--base64url bytes)))
        (should (string-match-p "\\`[A-Za-z0-9_-]+\\'" encoded))
        (should (equal bytes
                       (niwa-collaboration--base64url-decode encoded)))))
    (should (equal (niwa-collaboration--base64url
                    (unibyte-string #xfb #xff #xfe))
                   "-__-"))
    (should-not (niwa-collaboration--base64url-decode nil))
    (should-not (niwa-collaboration--base64url-decode "not base64 !!"))))

(niwa-deftest niwa-collaboration--random-bytes
  (:doc "returns the requested count and fails without OS randomness")
  (progn
    (should (= 32 (string-bytes (niwa-collaboration--random-bytes 32))))
    (cl-letf (((symbol-function 'insert-file-contents-literally)
               (lambda (&rest _) (error "No such file"))))
      (should-error (niwa-collaboration--random-bytes 32)
                    :type 'user-error))))

(niwa-deftest niwa-collaboration--relay-origins
  (:doc "derives the web origin from a ws origin and rejects other shapes")
  (progn
    (let ((niwa-collaboration-relay-url "wss://collab.example"))
      (should (equal '("wss://collab.example" . "https://collab.example")
                     (niwa-collaboration--relay-origins))))
    (let ((niwa-collaboration-relay-url "ws://127.0.0.1:7466/"))
      (should (equal '("ws://127.0.0.1:7466" . "http://127.0.0.1:7466")
                     (niwa-collaboration--relay-origins))))
    (dolist (bad '("https://collab.example" "collab.example"
                   "wss://collab.example/path" nil ""))
      (let ((niwa-collaboration-relay-url bad))
        (should-error (niwa-collaboration--relay-origins)
                      :type 'user-error)))))

(niwa-deftest niwa-collaboration--links
  (:doc "builds three links whose secrets are prefixes of each other")
  (let ((links (niwa-collaboration--links
                "https://collab.example" "room" (make-string 32 ?k)
                (make-string 16 ?w) (make-string 16 ?o)))
        (secret (lambda (bytes)
                  (niwa-collaboration--base64url bytes))))
    (should (equal (format "https://collab.example/#room.%s"
                           (funcall secret (make-string 32 ?k)))
                   (plist-get links :link-view)))
    (should (equal (format "https://collab.example/#room.%s"
                           (funcall secret (concat (make-string 32 ?k)
                                                   (make-string 16 ?w))))
                   (plist-get links :link-full)))
    (should (equal (format "https://collab.example/#room.%s"
                           (funcall secret (concat (make-string 32 ?k)
                                                   (make-string 16 ?w)
                                                   (make-string 16 ?o))))
                   (plist-get links :link-owner)))))

(niwa-deftest niwa-collaboration--guest-link
  (:doc "hands a guest a room at its own tier and no higher")
  (let ((room '(:link-view "v" :link-full "f" :link-owner "o")))
    (should (equal "v" (niwa-collaboration--guest-link room '(:name "a"))))
    (should (equal "f" (niwa-collaboration--guest-link
                        room '(:writable t))))
    (should (equal "o" (niwa-collaboration--guest-link
                        room '(:writable t :owner t))))))

(niwa-deftest niwa-collaboration--sanitize-guest-name
  (:doc "bounds guest names, strips control characters, and never yields empty")
  (progn
    (should (equal "guest" (niwa-collaboration--sanitize-guest-name nil)))
    (should (equal "guest" (niwa-collaboration--sanitize-guest-name "  ")))
    (should (equal "a b" (niwa-collaboration--sanitize-guest-name
                          "a\n\t\rb")))
    (should (= niwa-collaboration--max-guest-name-chars
               (length (niwa-collaboration--sanitize-guest-name
                        (make-string 200 ?x)))))))


;;
;;; Canonical projection

(niwa-deftest niwa-collaboration--canonical-records
  ()
  ,test
  (test)
  :doc "uses allowlisted canonical text, stable identities, and revision zero"
  (with-temp-buffer
    (insert "prompt\nanswer\n")
    (let ((data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'niwa-transcript-segments)
                 (lambda (_start _end)
                   (list (list 'user 1 4) (list 'response 4 11))))
                ((symbol-function 'niwa-view--user-turn-text)
                 (lambda (_segments _buffer) "visible prompt"))
                ((symbol-function 'niwa-view--visible-response-text)
                 (lambda (_text) "visible answer")))
        (let ((records (niwa-collaboration--canonical-records data-buffer)))
          (should (= 2 (length records)))
          (should (equal '(0 0) (mapcar (lambda (r) (plist-get r :revision))
                                        records)))
          (should-not (string-match-p "\\`user-[0-9]+\\'"
                                      (plist-get (car records) :id)))))))

  :doc "distinguishes repeated text, omits blanks, and attributes only user turns"
  (with-temp-buffer
    (let (segments)
      (dolist (item '((user . "repeat\n") (response . "repeat\n")
                      (user . " \n") (response . "repeat\n")
                      (user . "repeat\n")))
        (let ((start (point)))
          (insert (cdr item))
          (push (list (car item) start (point)) segments)))
      (setq segments (nreverse segments))
      (let ((last-user-start (cadr (car (last segments)))))
        (cl-letf (((symbol-function 'niwa-transcript-segments)
                   (lambda (_start _end) segments))
                  ((symbol-function 'niwa-transcript-audit-turn-attributions)
                   (lambda () (list (cons last-user-start (list :name "phone"))))))
          (let* ((records (niwa-collaboration--canonical-records
                           (current-buffer)))
                 (ids (mapcar (lambda (record) (plist-get record :id)) records)))
            (should (equal '("user" "assistant" "assistant" "user")
                           (mapcar (lambda (record) (plist-get record :kind)) records)))
            (should (equal '("repeat" "repeat" "repeat" "repeat")
                           (mapcar (lambda (record) (plist-get record :text)) records)))
            (should (= 4 (length (delete-dups (copy-sequence ids)))))
            (should (equal ids
                           (mapcar (lambda (record) (plist-get record :id))
                                   (niwa-collaboration--canonical-records
                                    (current-buffer)))))
            (should (equal '(nil nil nil "phone")
                           (mapcar (lambda (record) (plist-get record :guest)) records))))))))

  :doc "tags every record of a shared-item turn with the item for room filtering"
  (with-temp-buffer
    (let (segments)
      (dolist (item '((user . "room\n") (response . "room answer\n")
                      (user . "board\n") (response . "board answer\n")))
        (let ((start (point)))
          (insert (cdr item))
          (push (list (car item) start (point)) segments)))
      (setq segments (nreverse segments))
      (let ((board-start (cadr (nth 2 segments))))
        (cl-letf (((symbol-function 'niwa-transcript-segments)
                   (lambda (_start _end) segments))
                  ((symbol-function 'niwa-shared-conversation-ranges)
                   (lambda ()
                     (list (list :start board-start :end (point-max)
                                 :shared (list :itemId "board" :questionId "q")))))
                  ((symbol-function 'niwa-transcript-audit-turn-attributions)
                   (lambda () nil)))
          (let ((records (niwa-collaboration--canonical-records (current-buffer))))
            (should (equal '(nil nil "board" "board")
                           (mapcar (lambda (record) (plist-get record :item)) records)))
            (should (equal "board"
                           (cdr (assoc "item" (niwa-collaboration--json-record
                                               (nth 3 records))))))))))))

(niwa-deftest niwa-collaboration--scope-at
  (:doc "names the shared item owning a position")
  (let ((items (list (list :start 15 :end 40 :shared (list :itemId "board")))))
    (should (equal '(:item "board") (niwa-collaboration--scope-at items 25)))
    (should-not (niwa-collaboration--scope-at items 40))
    (should-not (niwa-collaboration--scope-at nil 5))))

(niwa-deftest niwa-collaboration--scoped
  (:doc "adds each discussion field to a record and leaves main-chat records alone")
  (progn
    (should (equal '(:id "a" :item "board")
                   (niwa-collaboration--scoped (list :id "a") '(:item "board"))))
    (should (equal '(:id "a") (niwa-collaboration--scoped (list :id "a") nil)))))

(niwa-deftest niwa-collaboration--tool-detail
  (:doc "summarizes the primary operand on one bounded line")
  (progn
    (should (equal "head -5 notes.txt"
                   (niwa-collaboration--tool-detail
                    '(:command "head -5 notes.txt\nsecond line"))))
    (should (equal "/tmp/a.el"
                   (niwa-collaboration--tool-detail
                    '(:file_path "/tmp/a.el"))))
    (should-not (niwa-collaboration--tool-detail nil))
    (should-not (niwa-collaboration--tool-detail '(:other 42)))
    (should (<= (length (niwa-collaboration--tool-detail
                         (list :command (make-string 900 ?x))))
                (+ 200 (length "\n[truncated]"))))))

(niwa-deftest niwa-collaboration--allowlist
  (:doc "serializes only visible canonical fields from adversarial records")
  (let* ((records
          (list (list :id "user" :kind "user" :revision 0 :text "visible"
                      :hidden "secret" :raw-html "<script>secret</script>"
                      :prompt "pending" :media "/secret.png")
                (list :id "assistant" :kind "assistant" :revision 1
                      :text "answer" :render-data "hidden")
                (list :id "tool" :kind "tool" :revision 0 :name "Bash"
                      :status "completed" :summary "Bash" :result "done"
                      :permission "secret" :execution-target "/target")
                (list :id "artifact" :kind "tool" :revision 0
                      :name "ApplyPatch"
                      :status "completed" :summary "ApplyPatch" :result "ok"
                      :artifact "mockup.html" :size 11
                      :artifact-path "/home/secret/artifacts/mockup.html")))
         (payload (mapcar #'niwa-collaboration--json-record records)))
    (should (equal '(("id" . "user") ("kind" . "user")
                     ("revision" . 0) ("text" . "visible"))
                   (car payload)))
    (should (equal '("id" "kind" "revision" "text")
                   (mapcar #'car (car payload))))
    (should (equal '("id" "kind" "revision" "text")
                   (mapcar #'car (cadr payload))))
    (should (equal '("id" "kind" "revision" "name" "status" "summary"
                     "result")
                   (mapcar #'car (caddr payload))))
    ;; An artifact record exposes its name and size, never the
    ;; host-side filesystem path it resolves to.
    (should (equal '("id" "kind" "revision" "name" "status" "summary"
                     "result" "artifact" "size")
                   (mapcar #'car (nth 3 payload))))
    (should-not (string-match-p "secret\\|script\\|target"
                                (json-encode payload)))))

(niwa-deftest niwa-collaboration--canonical-excluded-spans
  (:doc "projects visible payload exactly while excluding control and unknown spans")
  (with-temp-buffer
    (let ((visible-user "Visible user [bad](javascript:alert(1)) <img src=secret.png>\n")
          (visible-answer "Visible answer with <b>literal</b> text\n"))
      (insert visible-user)
      (let ((response-start (point)))
        (insert visible-answer)
        (put-text-property response-start (point) 'gptel 'response))
      (dolist (block '("#+begin_reasoning\nVisible thinking\n#+end_reasoning\n"
                       "<system-reminder>\nreminder secret\n</system-reminder>\n"
                       "<hook-context>\nhook secret\n</hook-context>\n"
                       "<!-- niwa-render-data -->\n(:kind media :path \"/secret.png\")\n<!-- /niwa-render-data -->\n"
                       "<!-- niwa-hook-audit -->\nunknown secret\n<!-- /niwa-hook-audit -->\n"))
        (let ((start (point)))
          (insert block)
          (put-text-property start (point) 'gptel 'ignore)))
      (let ((unknown-start (point)))
        (insert "Unknown kind payload must stay hidden\n")
        (put-text-property unknown-start (point) 'gptel 'ignore))
      (let ((tool-start (point)))
        (insert "#+begin_tool (Read :file_path \"/tmp/visible\")\n"
                "(:name \"Read\" :args (:file_path \"/tmp/visible\"))\n\n"
                "Visible tool result\n"
                "#+end_tool\n")
        (put-text-property tool-start (point) 'gptel '(tool . "visible-tool")))
      (let* ((records (niwa-collaboration--canonical-records (current-buffer)))
             (payload (mapcar #'niwa-collaboration--json-record records)))
        (should (equal '("user" "assistant" "thinking" "tool")
                       (mapcar (lambda (record) (plist-get record :kind)) records)))
        (should (equal (string-trim visible-user)
                       (cdr (assoc "text" (car payload)))))
        (should (equal (string-trim visible-answer)
                       (cdr (assoc "text" (cadr payload)))))
        (should (equal "Visible thinking"
                       (cdr (assoc "text" (nth 2 payload)))))
        (should (equal "Visible tool result"
                       (cdr (assoc "result" (nth 3 payload)))))
        (let ((json (json-encode payload)))
          (dolist (hidden '("reminder secret" "hook secret"
                            "/secret.png" "unknown secret"
                            "Unknown kind payload"))
            (should-not (string-match-p (regexp-quote hidden) json))))))))

(niwa-deftest niwa-collaboration--local-remote-parity
  (:doc "keeps local and remote-backed projections semantically identical")
  (with-temp-buffer
    (insert "Prompt from the local session\n")
    (let ((response-start (point)))
      (insert "Answer from the shared session\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((tool-start (point)))
      (insert "#+begin_tool (Bash :command \"true\")\n"
              "(:name \"Bash\" :args (:command \"true\"))\n\n"
              "done\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "parity-tool")))
    ;; Both rooms project the same data buffer, so equality between them
    ;; would be vacuous.  What matters is that nothing target-specific
    ;; reaches the wire at all.
    (let* ((remote-room (list :data-buffer (current-buffer)
                              :session 'remote-session
                              :execution-target "/ssh:someone@example:/srv/"))
           (wire (json-encode
                  (mapcar #'niwa-collaboration--json-record
                          (niwa-collaboration--project-records
                           remote-room)))))
      (should (string-match-p "Answer from the shared session" wire))
      (should-not (string-match-p "execution-target\\|/ssh:\\|example" wire)))))

(niwa-deftest niwa-collaboration--tool-record
  (:doc "marks empty settled tool output completed rather than running")
  (with-temp-buffer
    (insert "tool")
    (let ((record (niwa-collaboration--tool-record
                   '(:name "Bash" :result "") "tool")))
      (should (equal "completed" (plist-get record :status)))
      (should (equal "" (plist-get record :result))))
    ;; Leading spaces on the first result line are significant alignment
    ;; (Read's right-aligned line numbers); only newlines are trimmed.
    (let ((record (niwa-collaboration--tool-record
                   '(:name "Read" :args (:file_path "/tmp/a.el")
                     :result "\n  1\t;;; header\n  2\tbody\n")
                   "tool")))
      (should (string-prefix-p "  1\t" (plist-get record :result)))
      (should (equal "/tmp/a.el" (plist-get record :detail))))
    ;; ApplyPatch carries the authored patch as a dedicated diff field.
    (let ((record (niwa-collaboration--tool-record
                   '(:name "ApplyPatch"
                     :args (:patch "@@ -1 +1 @@\n-old\n+new")
                     :result "Applied patch: 1 changes")
                   "tool")))
      (should (equal "@@ -1 +1 @@\n-old\n+new" (plist-get record :diff)))
      (should (equal "@@ -1 +1 @@\n-old\n+new"
                     (cdr (assoc "diff"
                                 (niwa-collaboration--json-record
                                  record))))))
    ;; Settled selected ApplyPatch render data is the artifact authority.
    (let* ((save-path (make-temp-file "niwa-collab-tool-artifact-" t))
           (workspace (niwa-workspace--create :type 'project :id "w"
                                                 :root save-path :name "w"))
           (dir (niwa-artifact-store-directory workspace))
           (path (file-name-concat dir "mockup.html")))
      (unwind-protect
          (progn
            (setq-local niwa--session
                        (niwa-session--create :name "s"
                                                 :workspace workspace))
            (make-directory dir t)
            (write-region "<h1>hi</h1>" nil path nil 'silent)
            (cl-letf (((symbol-function 'niwa-view--tool-call-parse)
                       (lambda (_buffer _start _end)
                         `(:name "ApplyPatch" :args (:patch "patch")
                           :result "Applied patch: 1 changes"
                           :render-data
                           (:kind patch :files
                            ((:kind add :added 1 :deleted 0 :diff "" :path ,path)))))))
              (let* ((record (car (niwa-collaboration--tool-segment-records
                                   (current-buffer) '(tool 1 5))))
                     (json (niwa-collaboration--json-record record)))
                (should (equal "mockup.html" (plist-get record :artifact)))
                (should (= 11 (cdr (assoc "size" json))))
                (should (equal "mockup.html" (cdr (assoc "artifact" json))))
                (should-not (assoc "artifact-path" json)))))
        (kill-local-variable 'niwa--session)
        (niwa-collaboration--artifact-stat-invalidate)
        (delete-directory save-path t)))))

(niwa-deftest niwa-collaboration--pre-tool
  (:doc "publishes one stable running tool record before settled completion")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :records nil :pending-tools nil
                       :tool-call-occurrences (make-hash-table :test #'equal)
                       :guests (make-hash-table :test #'eql)))
           (info '(:name "Bash" :args (:command "true")))
         (niwa-collaboration--rooms (niwa-test-room-registry room))
           canonical invalidated)
      (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
                 (lambda (&rest _) canonical))
                ((symbol-function
                  'niwa-collaboration--artifact-stat-invalidate)
                 (lambda () (setq invalidated t))))
        (should-not (niwa-collaboration--pre-tool info))
        (let* ((pending (plist-get room :pending-tools))
               (entry (car pending))
               (id (plist-get entry :id)))
          (should (= 1 (length pending)))
          (should (equal "running" (plist-get entry :status)))
          (should (stringp id))
          (should-not (niwa-collaboration--pre-tool info))
          (should (= 1 (length (plist-get room :pending-tools))))
          (setq canonical
                (list (list :id "canonical" :kind "tool" :revision 0
                            :name "Bash" :status "completed"
                            :summary "Bash" :result "" :truncated nil)))
          (should-not (niwa-collaboration--post-tool
                       '(:name "Bash" :args (:command "true") :result "")))
          (setq entry (car (plist-get room :records)))
          (should (equal id (plist-get entry :id)))
          (should (equal "completed" (plist-get entry :status)))
          (should (equal "" (plist-get entry :result)))
          (should-not (plist-get room :pending-tools))
          ;; Without a native tool-use id, later projections must keep the
          ;; adopted pending id after its pending entry has been removed.
          (setq canonical
                (list (list :id "canonical" :kind "tool" :revision 0
                            :name "Bash" :status "completed"
                            :summary "Bash" :result "" :truncated nil)))
          (niwa-collaboration--publish room)
          (should (equal id (plist-get (car (plist-get room :records)) :id)))
          (should-not invalidated)
          ;; ApplyPatch can settle several target-native paths, so the small
          ;; qualified-path stat cache is cleared wholesale.
          (niwa-collaboration--post-tool
           '(:name "ApplyPatch" :args (:patch "patch")
             :result ""))
          (should invalidated))))))

(niwa-deftest niwa-collaboration--post-tool/before-insertion
  (:doc "settlement broadcasts before gptel inserts the canonical tool result")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :tool-call-occurrences (make-hash-table :test #'equal)
                       :guests (make-hash-table :test #'eql)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           sent)
      (cl-letf (((symbol-function 'niwa-collaboration--broadcast)
                 (lambda (_room frame)
                   (when (equal "record" (plist-get frame :t))
                     (push (plist-get frame :record) sent)))))
        (niwa-collaboration--pre-tool
         '(:name "Bash" :args (:command "true")))
        (should (equal "running" (cdr (assoc "status" (car sent)))))
        (niwa-collaboration--post-tool
         '(:name "Bash" :args (:command "true") :result "done"))
        (should (= 2 (length sent)))
        (should (equal "completed" (cdr (assoc "status" (car sent)))))
        (should (equal "done" (cdr (assoc "result" (car sent)))))
        (should (= 2 (cdr (assoc "revision" (car sent)))))
        (should (equal (cdr (assoc "id" (car sent)))
                       (cdr (assoc "id" (cadr sent)))))))))

(niwa-deftest niwa-collaboration--pre-tool-poll
  (:doc "does not publish a transient pending card for an empty-input poll")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :pending-tools nil :records nil))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (publishes 0))
      (cl-letf (((symbol-function 'niwa-collaboration--publish)
                 (lambda (_) (cl-incf publishes))))
        (should-not (niwa-collaboration--pre-tool
                     '(:name "WriteStdin" :args (:execution_id "exec-1" :chars ""))))
        (should (= 0 publishes))
        (should-not (plist-get room :pending-tools))
        (should-not (plist-get room :records))
        (should-not (niwa-collaboration--post-tool
                     '(:name "WriteStdin" :args (:execution_id "exec-1" :chars "")
                       :result "new output")))
        (should (= 1 publishes))))))

(niwa-deftest niwa-collaboration--project-records
  (:doc "merges a pending tool with its landed canonical record even when the settlement info missed it")
  (let* ((canonical
          (list (list :id "canonical-1" :kind "tool" :revision 0
                      :name "Bash" :status "completed"
                      :summary "Bash" :result "transcript-formatted output"
                      :truncated nil)))
         ;; Still "running": the post-tool settlement never matched it.
         (stuck (list :id "pending-1" :kind "tool" :revision 0
                      :name "Bash" :status "running" :summary "Bash"
                      :result "" :truncated nil :pending t
                      :identity-fixed t :call-key "k1"
                      :baseline-tool-count 0 :baseline-record-count 0))
         (room (list :data-buffer 'data :pending-tools (list stuck))))
    (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
               (lambda (&rest _) (mapcar #'copy-sequence canonical))))
      (let ((records (niwa-collaboration--project-records room)))
        ;; One card, keeping the pending identity with canonical content.
        (should (= 1 (length records)))
        (should (equal "pending-1" (plist-get (car records) :id)))
        (should (equal "completed" (plist-get (car records) :status)))
        (should (equal "transcript-formatted output"
                       (plist-get (car records) :result)))))
    ;; A pending whose canonical record has not landed stays visible.
    (let ((room (list :data-buffer 'data
                      :pending-tools (list (copy-sequence stuck)))))
      (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
                 (lambda (&rest _) nil)))
        (let ((records (niwa-collaboration--project-records room)))
          (should (= 1 (length records)))
          (should (equal "running" (plist-get (car records) :status))))))))

(niwa-deftest niwa-collaboration--project-records/order
  (:doc "pending rows retain their baselines and order among canonical records")
  (let* ((canonical (cl-loop for index below 1000
                             collect (list :id (format "answer-%d" index)
                                           :kind "assistant")))
         (pending (cl-loop for baseline in '(0 0 500 1000)
                           for index from 1
                           collect (list :id (format "pending-%d" index)
                                         :kind "tool" :status "running"
                                         :baseline-record-count baseline)))
         (room (list :data-buffer 'data :pending-tools pending)))
    (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
               (lambda (&rest _) canonical)))
      (let ((records (niwa-collaboration--project-records room)))
        (should (equal canonical
                       (cl-remove-if (lambda (record)
                                       (equal "tool" (plist-get record :kind)))
                                     records)))
        (should (equal '("pending-1" "pending-2" "pending-3" "pending-4")
                       (mapcar (lambda (index) (plist-get (nth index records) :id))
                               '(0 1 502 1003))))))))

(niwa-deftest niwa-collaboration--project-records-hidden-poll
  (:doc "a settled direct poll removes its pending card only after its hidden canonical row lands")
  (let* ((landed nil)
         (room (list :data-buffer (current-buffer)
                     :pending-tools
                     (list (list :id "poll" :kind "tool" :name "ToolCall"
                                 :status "completed" :pending t
                                 :baseline-tool-count 0 :baseline-record-count 0)))))
    (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
               (lambda (&rest _) nil))
              ((symbol-function 'niwa-collaboration--suppressed-tool-landed-p)
               (lambda (_data _entry) landed)))
      (should (= 1 (length (niwa-collaboration--project-records room))))
      (setq landed t)
      (should-not (niwa-collaboration--project-records room))
      (should-not (plist-get room :pending-tools)))))

(niwa-deftest niwa-collaboration--clean-response
  (:doc "fails closed when canonical response projection is unavailable")
  (cl-letf (((symbol-function 'niwa-view--visible-response-text)
             (lambda (_) (error "Projection failure"))))
    (should-error (niwa-collaboration--clean-response "hidden"))))

(niwa-deftest niwa-collaboration--clean-user
  (:doc "fails closed when canonical user projection is unavailable")
  (with-temp-buffer
    (cl-letf (((symbol-function 'niwa-view--user-turn-text)
               (lambda (_segments _buffer) nil)))
      (should-error
       (niwa-collaboration--clean-user '(user 1 2) (current-buffer))))))


;;
;;; Publication

(niwa-deftest niwa-collaboration--publish
  (:doc "broadcasts simultaneous changes in canonical order with incremented revisions")
  (let* ((old (list (list :id "a" :kind "assistant" :revision 0 :text "one")
                    (list :id "b" :kind "assistant" :revision 0 :text "two")))
         (new (list (list :id "a" :kind "assistant" :revision 0 :text "ONE")
                    (list :id "b" :kind "assistant" :revision 0 :text "TWO")))
         (sent nil)
         (guests (make-hash-table :test #'eql))
         (room (list :records old :transport 'transport :guests guests)))
    (puthash 1 (list :name "g" :writable nil :ready t) guests)
    (cl-letf (((symbol-function 'niwa-collaboration--project-records)
               (lambda (_) new))
              ((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (niwa-collaboration--publish room))
    (setq sent (nreverse sent))
    ;; Publication also settles the status strip; the records are what
    ;; this case is about.
    (setq sent (cl-remove-if-not
                (lambda (entry) (equal "record" (plist-get (cdr entry) :t)))
                sent))
    (should (equal '(0 0) (mapcar #'car sent)))
    (should (equal '("a" "b")
                   (mapcar (lambda (entry)
                             (cdr (assoc "id" (plist-get (cdr entry) :record))))
                           sent)))
    (should (equal '(1 1)
                   (mapcar (lambda (entry)
                             (cdr (assoc "revision"
                                         (plist-get (cdr entry) :record))))
                           sent)))))

(niwa-deftest niwa-collaboration--publish/oversized-record
  (:doc "one record too large to send costs the guest only that record")
  (let* ((old (list (list :id "a" :kind "assistant" :revision 0 :text "one")
                    (list :id "b" :kind "assistant" :revision 0 :text "two")
                    (list :id "c" :kind "assistant" :revision 0 :text "three")))
         (new (list (list :id "a" :kind "assistant" :revision 0 :text "secret ONE")
                    (list :id "b" :kind "assistant" :revision 0 :text "TWO")))
         (sent nil)
         (guests (make-hash-table :test #'eql))
         (room (list :records old :transport 'transport :guests guests))
         captured)
    (puthash 1 (list :name "g" :writable nil :ready t) guests)
    (cl-letf (((symbol-function 'niwa-collaboration--project-records)
               (lambda (_) new))
              ((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport _peer frame)
                 ;; The transport refuses record a as over the wire bound.
                 (if (equal "a" (cdr (assoc "id" (plist-get frame :record))))
                     (signal 'niwa-collaboration-frame-too-large
                             (list "9 bytes over the 1048540-byte bound"))
                   (push (plist-get frame :t) sent)
                   t))))
      (niwa-test--with-captured-diagnostics captured
        (niwa-collaboration--publish room)))
    ;; The later record, the removal and the status still reach the guest.
    (setq sent (nreverse sent))
    (should (equal '("record" "remove" "status")
                   (cl-remove-if-not (lambda (type) (member type '("record" "remove" "status")))
                                     sent)))
    (should (string-search "Collaboration frame not sent" captured))
    (should (string-search "bytes over" captured))
    (should-not (string-search "secret" captured))))

(niwa-deftest niwa-collaboration--send-ui-requests/oversized
  (:doc "a greeting skips an oversized pending request and still sends the later ones")
  (with-temp-buffer
    (insert "requests")
    (let* ((requests (make-hash-table :test #'eql))
           (room (list :transport 'transport :ui-requests requests))
           (sent nil)
           captured)
      (dolist (entry '((1 . "huge") (2 . "small")))
        (let ((overlay (make-overlay 1 2)))
          (overlay-put overlay 'niwa--remote (list :body (cdr entry)))
          (puthash (car entry) overlay requests)))
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport _peer frame)
                   ;; The transport refuses the first request as over the bound.
                   (if (equal "huge" (plist-get frame :body))
                       (signal 'niwa-collaboration-frame-too-large
                               (list "1048576 bytes over the 1048540-byte bound"))
                     (push (plist-get frame :reqId) sent)
                     t))))
        (niwa-test--with-captured-diagnostics captured
          (niwa-collaboration--send-ui-requests room 7)))
      (should (equal '(2) sent))
      (should (string-search "Collaboration frame not sent" captured)))))

(niwa-deftest niwa-collaboration--publish/repeated
  (:doc "unchanged publishes preserve the revision for the next change and joins")
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (insert (propertize "First" 'gptel 'response))
    (let ((room (list :data-buffer (current-buffer)
                      :guests (make-hash-table :test #'eql)))
          sent)
      (cl-letf (((symbol-function 'niwa-collaboration--broadcast)
                 (lambda (_room frame)
                   (when (equal "record" (plist-get frame :t))
                     (push (plist-get frame :record) sent)))))
        (niwa-collaboration--publish room)
        (should (= 1 (plist-get (car (plist-get room :records)) :revision)))
        (niwa-collaboration--publish room)
        (should (= 1 (length sent)))
        (should (= 1 (plist-get (car (plist-get room :records)) :revision)))
        (insert (propertize " second" 'gptel 'response))
        (niwa-collaboration--publish room)
        (should (= 2 (length sent)))
        (should (= 2 (cdr (assoc "revision" (car sent)))))
        (should (equal "First second" (cdr (assoc "text" (car sent)))))))))

(niwa-deftest niwa-collaboration--attribute-fork-points
  (:doc "names each settled turn's fork point on its last assistant record")
  (with-temp-buffer
    (let (segments)
      (cl-flet ((add (type text)
                  (let ((start (point)))
                    (insert text)
                    (push (list type start (1- (point))) segments)))
                (fork (id turn)
                  (insert (niwa--format-hook-audit-record
                           (list :type 'fork-point :fork-point-id id :cum-turn turn)))))
        (add 'user "first prompt\n")
        (add 'response "early answer\n")
        (add 'response "settled answer\n")
        (fork "fork-1" 1)
        ;; A turn without a response gets no fork point of its own; the
        ;; earlier turn's answer must not take this one's.
        (add 'user "second prompt\n")
        (fork "fork-2" 2)
        ;; A turn that has not settled has no fork point yet.
        (add 'user "third prompt\n")
        (add 'response "streaming answer\n"))
      (setq segments (nreverse segments))
      (cl-letf (((symbol-function 'niwa-transcript-segments)
                 (lambda (_start _end) segments))
                ((symbol-function 'niwa-view--user-turn-text)
                 (lambda (turn-segments buffer)
                   (let ((segment (car turn-segments)))
                     (string-trim (with-current-buffer buffer
                                    (buffer-substring (cadr segment)
                                                      (caddr segment)))))))
                ((symbol-function 'niwa-view--visible-response-text)
                 #'string-trim))
        (let* ((records (niwa-collaboration--canonical-records
                         (current-buffer)))
               (answers (cl-remove-if-not
                         (lambda (record)
                           (equal "assistant" (plist-get record :kind)))
                         records)))
          (should (equal '("early answer" "settled answer" "streaming answer")
                         (mapcar (lambda (record) (plist-get record :text))
                                 answers)))
          (should (equal '(nil "fork-1" nil)
                         (mapcar (lambda (record) (plist-get record :forkPoint))
                                 answers)))
          (should (equal '(nil 1 nil)
                         (mapcar (lambda (record) (plist-get record :forkTurn))
                                 answers)))
          (should (= 1 (cdr (assoc "forkTurn"
                                  (niwa-collaboration--json-record (nth 1 answers))))))
          (should (equal "fork-1"
                         (cdr (assoc "forkPoint"
                                     (niwa-collaboration--json-record
                                      (nth 1 answers)))))))))))

(niwa-deftest niwa-collaboration--attribute-guest-prompts
  (:doc "attributes each guest record to the nearest preceding user turn")
  (with-temp-buffer
    (insert "first prompt\nanswer\nsecond prompt\n")
    (insert (niwa--format-hook-audit-record
             (list :type 'guest-prompt :name "phone")))
    (cl-letf (((symbol-function 'niwa-transcript-segments)
               (lambda (_start _end)
                 (list (list 'user 1 13) (list 'response 14 20)
                       (list 'user 21 34))))
              ((symbol-function 'niwa-view--user-turn-text)
               (lambda (segments _buffer)
                 (if (= 1 (cadr (car segments))) "first prompt"
                   "second prompt")))
              ((symbol-function 'niwa-view--visible-response-text)
               (lambda (_text) "answer")))
      (let* ((records (niwa-collaboration--canonical-records
                       (current-buffer)))
             (first-user (nth 0 records))
             (second-user (nth 2 records)))
        ;; The attribution block sits after both prompts, so it names the
        ;; last user turn starting before it: the second one.
        (should-not (plist-member first-user :guest))
        (should (equal "phone" (plist-get second-user :guest)))
        (should (equal "phone"
                       (cdr (assoc "guest"
                                   (niwa-collaboration--json-record
                                    second-user)))))))
    ;; Segment repair can grow a user turn's END past its own audit block
    ;; (absorbing the trailing hidden records); attribution anchors on the
    ;; turn's START, so the badge must stay on the second turn even then.
    (cl-letf (((symbol-function 'niwa-transcript-segments)
               (lambda (_start _end)
                 ;; The audit block written earlier sits after position 34;
                 ;; the second user segment's end has drifted beyond it.
                 (list (list 'user 1 13) (list 'response 14 20)
                       (list 'user 21 4000))))
              ((symbol-function 'niwa-view--user-turn-text)
               (lambda (segments _buffer)
                 (if (= 1 (cadr (car segments))) "first prompt"
                   "second prompt")))
              ((symbol-function 'niwa-view--visible-response-text)
               (lambda (_text) "answer")))
      (let* ((records (niwa-collaboration--canonical-records
                       (current-buffer))))
        (should-not (plist-member (nth 0 records) :guest))
        (should (equal "phone" (plist-get (nth 2 records) :guest)))))))

(niwa-deftest niwa-collaboration--canonical-records-failure
  (:doc "Persisted provider failures remain visible in canonical shared conversations")
  (with-temp-buffer
    (insert (niwa-tool-render-data-format
             '(:kind request-summary :elapsed-seconds 1 :outcome error
               :message "Provider unavailable" :error-data (:secret "not projected"))))
    (let ((records (niwa-collaboration--canonical-records (current-buffer))))
      ;; The failure notice is followed by the turn's closing line.
      (should (equal (mapcar (lambda (record) (plist-get record :kind)) records)
                     '("assistant" "request-summary")))
      (should (equal (plist-get (car records) :status) "failed"))
      (should (equal (plist-get (car records) :text)
                     "Assistant request failed. Provider unavailable"))
      (should-not (string-match-p "not projected" (prin1-to-string records))))))

(niwa-deftest niwa-collaboration--safe-accepted-prompt
  (:doc "publishes accepted prompt insertion and isolates observer failure")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           published stopped)
      (cl-letf (((symbol-function 'niwa-collaboration--publish)
                 (lambda (_room) (setq published t))))
        (should-not (niwa-collaboration--safe-accepted-prompt
                     (current-buffer)))
        (should published))
      (cl-letf (((symbol-function 'niwa-collaboration--publish)
                 (lambda (_) (error "Viewer failed")))
                ((symbol-function 'niwa-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason)))
                ((symbol-function 'display-warning) (lambda (&rest _) nil)))
        (should-not (niwa-collaboration--safe-accepted-prompt
                     (current-buffer)))
        (should-not stopped)))))

(niwa-deftest niwa-collaboration--observer-failure
  (:doc "warns the host with the error and tells guests once until a publish succeeds")
  (let ((room (list :data-buffer nil :records nil
                    :session (niwa-session--create
                              :workspace (niwa-workspace--create :root "/origin/project/"))))
        warnings notices)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings)))
              ((symbol-function 'niwa-collaboration--broadcast)
               (lambda (_room frame) (push frame notices)))
              ((symbol-function 'niwa-collaboration--project-records) #'ignore)
              ((symbol-function 'niwa-collaboration--publish-queue) #'ignore)
              ((symbol-function 'niwa-collaboration--publish-status) #'ignore)
              ((symbol-function 'niwa-collaboration--publish-history) #'ignore)
              ((symbol-function 'niwa-collaboration--publish-agents) #'ignore)
              ((symbol-function 'niwa-collaboration--publish-tasks) #'ignore))
      (niwa-collaboration--observer-failure room '(error "Projection broke"))
      (niwa-collaboration--observer-failure room '(error "Projection broke"))
      (should (= 1 (length warnings)))
      (should (equal "niwa [/origin/project/]: Live collaboration update failed: Projection broke"
                     (car warnings)))
      (should (= 1 (length notices)))
      (should-not (string-search "/origin/project/" (prin1-to-string notices)))
      (should-not (string-search "still running" (plist-get (car notices) :message)))
      (niwa-collaboration--publish room)
      (niwa-collaboration--observer-failure room '(error "Projection broke"))
      (should (= 2 (length warnings))))))

(niwa-deftest niwa-collaboration--accepted-prompt-insertion-seams
  (:doc "publishes ordinary composer and generated turns at insertion")
  (let ((data-buffer (generate-new-buffer " *collaboration-prompt-seams*"))
        ordinary generated)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-response-separator "\n\n")
            (setq-local niwa-compact-run-in-flight nil))
          (with-temp-buffer
            (let ((niwa--data-buffer data-buffer))
              (cl-letf (((symbol-function 'niwa-view--ensure-interactive-chat-view)
                         (lambda () nil))
                        ((symbol-function 'niwa-session-naming-consider) #'ignore)
				       ((symbol-function 'niwa-readiness-assert) #'ignore)
                        ((symbol-function 'niwa-view--session)
                         (lambda () 'session))
                        ((symbol-function 'niwa-request-assert-target-ready)
                         (lambda (&rest _) nil))
                        ((symbol-function 'niwa-view--pop-dropped-file-grants-for-input)
                         (lambda (&rest _) nil))
                        ((symbol-function 'niwa--normalize-message-text)
                         (lambda (text) text))
                        ((symbol-function 'niwa-view--inline-skill-prompt-summary-body)
                         (lambda (_text) nil))
                        ((symbol-function 'niwa--clear-user-turn-gptel-properties)
                         (lambda (&rest _) nil))
                        ((symbol-function 'niwa-collaboration--safe-accepted-prompt)
                         (lambda (buffer) (setq ordinary (eq buffer data-buffer))))
                        ((symbol-function 'niwa-view--insert-user-message)
                         (lambda (&rest _) 1))
                        ((symbol-function 'niwa-view-stream-begin-turn)
                         (lambda (&rest _) nil))
                        ((symbol-function 'niwa-view--clear-input)
                         (lambda () nil))
                        ((symbol-function 'niwa-view--activate-dropped-file-grants)
                         (lambda (&rest _) nil))
                        ((symbol-function 'gptel-send) (lambda () nil)))
                (niwa-view--forward-input-now "ordinary prompt")))
          (with-current-buffer data-buffer
            (cl-letf (((symbol-function 'niwa-collaboration--safe-accepted-prompt)
                       (lambda (buffer) (setq generated (eq buffer data-buffer))))
                      ((symbol-function 'niwa-view--begin-external-turn)
                       (lambda (&rest _) nil)))
              (niwa--insert-local-user-turn "generated prompt")))
          (should ordinary)
          (should generated)
          (with-current-buffer data-buffer
            (should (string-match-p "ordinary prompt" (buffer-string)))
            (should (string-match-p "generated prompt" (buffer-string)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))))


;;
;;; Relay and guest-facing facade integration

(niwa-deftest niwa-collaboration--host-headers
  (:doc "omits an unconfigured host token and sends a configured one")
  (let ((niwa-collaboration-relay-host-token nil))
    (should-not (niwa-collaboration--host-headers))
    (setq niwa-collaboration-relay-host-token "")
    (should-not (niwa-collaboration--host-headers))
    (setq niwa-collaboration-relay-host-token "s3cret")
    (should (equal '(("X-Niwa-Host-Token" . "s3cret"))
                   (niwa-collaboration--host-headers)))))


(niwa-deftest niwa-collaboration--publish-queue
  (:doc "broadcasts pending queue state to guests only when it changes")
  (let* ((workspace (niwa-workspace-get-or-create
                     'project "/tmp/collab-queue/" "/tmp/collab-queue/" "cq"))
         (session (niwa-session-create "main" workspace))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :guests guests
                     :transport 'transport :queue nil))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t)))
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "phone-guest-id")
                   guests)
          (niwa-collaboration--publish-queue room)
          (should (equal '((1 . (:t "queue" :pending 0 :paused nil)))
                         sent))
          ;; An unchanged queue is not re-sent.
          (setq sent nil)
          (niwa-collaboration--publish-queue room)
          (should-not sent)
          ;; A queued entry and a pause both move the state.
          (let ((entry (niwa-session-enqueue-pending-input
                        session 'follow-up (list :input "later"))))
            (niwa-collaboration--publish-queue room)
            (should (equal '((1 . (:t "queue" :pending 1 :paused nil)))
                           sent))
            ;; The ack tells the sender where in line it landed.
            (should (= 1 (niwa-collaboration--queue-position room entry)))
            (setq sent nil)
            (niwa-session-set-pending-input-paused session t)
            (niwa-collaboration--publish-queue room)
            (should (equal '((1 . (:t "queue" :pending 1 :paused t)))
                           sent))
            ;; An entry that drained between enqueue and ack has no place.
            (niwa-session-set-pending-inputs session 'follow-up nil)
            (should-not (niwa-collaboration--queue-position room entry)))
          ;; A guest's own entries ride its queue frame -- id, live
          ;; position, and its own text echoed back -- and only its own:
          ;; another guest's unsent text never travels to it.
          (setq sent nil)
          (niwa-session-set-pending-input-paused session nil)
          (niwa-session-enqueue-pending-input
           session 'follow-up (list :input "someone else's"
                                    :guest-id "other-guest-id"))
          (let ((mine (niwa-session-enqueue-pending-input
                       session 'follow-up
                       (list :input "my question"
                             :guest-id "phone-guest-id"))))
            (niwa-collaboration--publish-queue room)
            (let* ((frame (cdr (car (last sent))))
                   (own (plist-get frame :own)))
              (should (= 2 (plist-get frame :pending)))
              (should (= 1 (length own)))
              (let ((record (aref (vconcat own) 0)))
                (should (equal (plist-get mine :id)
                               (cdr (assoc "id" record))))
                (should (= 2 (cdr (assoc "position" record))))
                (should (equal "my question"
                               (cdr (assoc "text" record))))))))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-collaboration--on-session-event
  (:doc "re-publishes replaced history and status on rename")
  (let (published history)
    (cl-letf (((symbol-function 'niwa-collaboration-notify-request-changed)
               (lambda (buffer) (push buffer published)))
              ((symbol-function 'niwa-collaboration-notify-history-changed)
               (lambda (buffer) (push buffer history))))
      (niwa-collaboration--on-session-event 'data 'rename)
      (niwa-collaboration--on-session-event 'data 'replace-history)
      (niwa-collaboration--on-session-event 'other-room 'replace-history)
      (niwa-collaboration--on-session-event 'data 'saved)
      (should (equal '(data) published))
      (should (equal '(other-room data data) history)))))

(niwa-deftest niwa-collaboration--publish-status
  (:doc "broadcasts busy and name changes once and tells a joining guest directly")
  (let* ((guests (make-hash-table :test #'eql))
         (workspace (niwa-workspace--create :root "/project/"))
         (session (niwa-session--create :workspace workspace))
         (data-buffer (generate-new-buffer " *collab-status-data*"))
         (room (list :data-buffer data-buffer :session session :guests guests
                     :push-guests (make-hash-table :test #'equal)
                     :transport 'transport :status nil))
         sent controls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'niwa-collaboration--transport-control)
                   (lambda (transport control)
                     (push (cons transport control) controls)
                     t)))
          (puthash 1 (list :name "g" :writable nil :ready t) guests)
          (with-current-buffer data-buffer
            (setq-local niwa--session session gptel-model 'test-model))
          ;; The first publish establishes the baseline the guests hold.
          (niwa-collaboration--publish-status room)
          (let ((frame (cdr (car sent))))
            (should (equal "status" (plist-get frame :t)))
            (should (eq :json-false (plist-get frame :busy)))
            ;; The strip reports what the Emacs mode line reports.
            (should (equal "test-model" (plist-get frame :model))))
          ;; Unchanged state is not repeated.
          (setq sent nil)
          (niwa-collaboration--publish-status room)
          (should-not sent)
          (with-current-buffer data-buffer
            (setq-local niwa--current-request 'request))
          (niwa-collaboration--publish-status room)
          (should (eq t (plist-get (cdr (car sent)) :busy)))
          (setq sent nil)
          (with-current-buffer data-buffer
            (setq-local niwa--current-request nil))
          (niwa-collaboration--publish-status room)
          (should (eq :json-false (plist-get (cdr (car sent)) :busy)))
          ;; An idle snapshot is not itself a completion signal.
          (should-not controls)
          ;; Re-publishing the same idle state sends neither status nor push.
          (setq sent nil controls nil)
          (niwa-collaboration--publish-status room)
          (should-not sent)
          (should-not controls)
          ;; The live-page fallback needs the same explicit user edge as Push.
          (let* ((niwa-collaboration--rooms (make-hash-table :test #'eq))
                 (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
                 (phones (make-hash-table :test #'equal))
                 (lobby (list :workspace workspace :guests (make-hash-table)
                              :push-guests phones :transport 'lobby)))
            (puthash "phone" '(:endpoint "https://push.test/phone" :writable t) phones)
            (puthash (niwa-workspace-root workspace) lobby niwa-collaboration-lobby--lobbies)
            (puthash data-buffer room niwa-collaboration--rooms)
            (niwa-collaboration-notify-request-changed data-buffer 'user)
            (should (= 2 (length controls)))
            (should (member '(transport :t "push") controls))
            (should (member '(lobby :t "push" :guestIds ["phone"]) controls))
            (should (equal "completion" (plist-get (cdar sent) :kind)))
            ;; A teardown callback can already have started a new request.
            (setq controls nil sent nil)
            (with-current-buffer data-buffer
              (setq-local niwa--current-request 'successor))
            (niwa-collaboration-notify-request-changed data-buffer 'user)
            (should-not controls)
            (should (equal "status" (plist-get (cdar sent) :t)))
            ;; A completion in a private session is not a workspace notice.
            (remhash data-buffer niwa-collaboration--rooms)
            (niwa-collaboration-notify-request-changed data-buffer 'user)
            (should-not controls)
            (with-current-buffer data-buffer (setq-local niwa--current-request nil))
            (dolist (trigger '(nil goal-continuation workspace-schedule background-event))
              (niwa-collaboration-notify-request-changed data-buffer trigger))
            (should-not controls)
            (niwa-collaboration-notify-request-changed data-buffer 'user)
            (should-not controls))
          ;; The session's name heads the room and follows a rename.
          (setq controls nil)
          (let ((session (niwa-session--create :name "2026-10-05T17-00-abc")))
            (plist-put room :session session)
            (niwa-collaboration--publish-status room)
            (should (equal "2026-10-05T17-00-abc"
                           (plist-get (cdr (car sent)) :name)))
            (setq sent nil)
            (setf (niwa-session-name session) "Pretty diagram")
            (niwa-collaboration--publish-status room)
            (should (equal "Pretty diagram" (plist-get (cdr (car sent)) :name)))
            (should-not controls)))
      (kill-buffer data-buffer))))

(niwa-deftest niwa-view--drain-guest-invocation
  (:doc "dispatches a queued guest invocation and dequeues it"
   :quiet t)
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "invoke"))
          (niwa-collaboration-guest-skills '((full "plan")))
          ran)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (niwa-session-enqueue-pending-input
       session 'follow-up (list :input "" :guest-invoke "plan"
                               :guest-role 'full
                               :guest-id "g1" :inert-skills t))
      (cl-letf (((symbol-function 'niwa-plan-mode-enter)
                 (lambda (&rest _) (setq ran t)))
                ((symbol-function
                  'niwa-session-artifacts-assert-new-mutation-authority)
                 (lambda (&rest _) nil)))
        (niwa-view--drain-follow-up data-buf))
      ;; The command ran, and the entry it came from is gone: a local
      ;; command inserts no turn, so nothing else would ever clear it.
      (should ran)
      (should-not (niwa-session-pending-follow-ups session)))))

(niwa-deftest niwa-view--refuse-guest-follow-up (:quiet t)
  ,test
  (test)
  :doc "drops a guest entry whose turn would ask in Emacs and tells the guest"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "refuse"))
           (attachment (make-temp-file "niwa-guest-attachment-"))
           notified warned)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (niwa-session-enqueue-pending-input
       session 'follow-up (list :input "Hello" :guest-name "Happy Hare" :guest-id "g1"
                               :guest-paths (list attachment)))
      (cl-letf (((symbol-function
                  'niwa-session-artifacts-assert-new-mutation-authority)
                 (lambda (&rest _) nil))
                ;; The turn would ask in Emacs, e.g. to take over a lease.
                ((symbol-function 'niwa-view--dispatch-follow-up-entry)
                 (lambda (&rest _)
                   (signal 'inhibited-interaction (list "Take over the lease? (y or n)"))))
                ((symbol-function 'niwa-collaboration-notify-guest)
                 (lambda (_session guest-id message)
                   (setq notified (list guest-id message))))
                ((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (setq warned message))))
        (niwa-view--drain-follow-up data-buf))
      (should-not (niwa-session-pending-follow-ups session))
      (should-not (file-exists-p attachment))
      (should (equal "g1" (car notified)))
      (should (string-search "your message was not sent" (cadr notified)))
      (should (string-search "Happy Hare" warned))
      ;; Only the host sees the question, which can name hosts and paths.
      (should (string-search ": Take over the lease? (y or n)" warned))
      (should-not (string-search "lease" (cadr notified)))))
  :doc "drops a refused guest entry instead of blocking the queue behind it"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "refuse"))
           notified warned)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (niwa-session-enqueue-pending-input
       session 'follow-up (list :input "/compact" :guest-name "Happy Hare" :guest-id "g1"))
      (niwa-session-enqueue-pending-input
       session 'follow-up (list :input "Later" :guest-name "Happy Hare" :guest-id "g1"))
      (cl-letf (((symbol-function
                  'niwa-session-artifacts-assert-new-mutation-authority)
                 (lambda (&rest _) nil))
                ((symbol-function 'niwa-view--dispatch-follow-up-entry)
                 (lambda (_kind _entry input &rest _)
                   (user-error "Manual compaction is unavailable for %s" input)))
                ((symbol-function 'niwa-collaboration-notify-guest)
                 (lambda (_session guest-id message)
                   (setq notified (list guest-id message))))
                ((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (setq warned message))))
        (niwa-view--drain-follow-up data-buf))
      (should (equal '("Later") (mapcar (lambda (entry) (plist-get entry :input))
                                        (niwa-session-pending-follow-ups session))))
      (should (equal "g1" (car notified)))
      ;; Errors can name hosts and paths: only the host sees the reason.
      (should (string-search "not sent" (cadr notified)))
      (should-not (string-search "compaction" (cadr notified)))
      (should (string-search "Manual compaction is unavailable" warned))
      (should (string-search "Happy Hare" warned))
      ;; A refusal blocks nothing the host does.
      (should-not (niwa-recovery-blocker session))))

  :doc "holds a guest entry behind the blocking issue its refusal reported"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "refuse"))
           (attachment (make-temp-file "niwa-guest-attachment-"))
           notified warned)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq-local niwa--session session))
            (niwa-session-enqueue-pending-input
             session 'follow-up (list :input "Hello" :guest-name "Happy Hare" :guest-id "g1"
                                     :guest-paths (list attachment)))
            (cl-letf (((symbol-function
                        'niwa-session-artifacts-assert-new-mutation-authority)
                       (lambda (&rest _) nil))
                      ;; Readiness reports the expired login, then refuses.
                      ((symbol-function 'niwa-view--dispatch-follow-up-entry)
                       (lambda (&rest _)
                         (niwa-recovery-report session "authentication" 'authentication
                                                  "Codex login needs renewal" t)
                         (user-error "Codex login needs renewal; sign in or retry after refresh")))
                      ((symbol-function 'niwa-recovery-save) #'ignore)
                      ((symbol-function 'niwa-recovery-save-now) #'ignore)
                      ((symbol-function 'niwa-collaboration-notify-guest)
                       (lambda (_session guest-id message)
                         (setq notified (list guest-id message))))
                      ((symbol-function 'display-warning)
                       (lambda (_type message &rest _) (setq warned message))))
              (niwa-view--drain-follow-up data-buf))
            (should (equal '("Hello") (mapcar (lambda (entry) (plist-get entry :input))
                                              (niwa-session-pending-follow-ups session))))
            (should-not (plist-get (car (niwa-session-pending-follow-ups session)) :state))
            (should (file-exists-p attachment))
            (should (equal "g1" (car notified)))
            (should (string-search "Provider sign-in is required" (cadr notified)))
            (should (string-search "will be sent" (cadr notified)))
            (should (string-search "was held: Codex login needs renewal" warned))
            ;; The queue waits for the issue instead of retrying.
            (should (niwa-view--follow-up-auto-drain-blocked-p session)))
        (when (file-exists-p attachment) (delete-file attachment)))))

  :doc "keeps an attempt that may have reached the transcript for review"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "refuse"))
           notified)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (niwa-session-enqueue-pending-input
       session 'follow-up (list :input "Hello" :guest-name "Happy Hare" :guest-id "g1"))
      (cl-letf (((symbol-function
                  'niwa-session-artifacts-assert-new-mutation-authority)
                 (lambda (&rest _) nil))
                ((symbol-function 'niwa-view--dispatch-follow-up-entry)
                 (lambda (&rest _)
                   (with-current-buffer data-buf (insert "Hello"))
                   (error "Publication failed")))
                ((symbol-function 'niwa-collaboration-notify-guest)
                 (lambda (_session guest-id message)
                   (setq notified (list guest-id message))))
                ((symbol-function 'display-warning) #'ignore))
        (niwa-view--drain-follow-up data-buf))
      (should (eq 'failed-turn
                  (plist-get (car (niwa-session-pending-follow-ups session)) :state)))
      (should (niwa-session-pending-input-failure-paused session))
      (should (string-search "review" (cadr notified)))))

  :doc "keeps a guest entry queued while a turn settles or compaction runs"
  (dolist (busy '(settling compacting))
    (niwa-view-test--with-buffers
      (let ((session (niwa-session--create :name "busy"))
            notified)
        (with-current-buffer data-buf
          (setq-local niwa--session session)
          (if (eq busy 'settling)
              (setq-local niwa--turn-settlements-pending (list 'fsm))
            (setq-local niwa-compact-run-in-flight t)))
        (with-current-buffer view-buf
          (setq-local niwa--session session))
        (niwa-session-enqueue-pending-input
         session 'follow-up (list :input "Hello" :guest-name "Happy Hare" :guest-id "g1"))
        (cl-letf (((symbol-function
                    'niwa-session-artifacts-assert-new-mutation-authority)
                   (lambda (&rest _) nil))
                  ((symbol-function 'niwa-collaboration-notify-guest)
                   (lambda (&rest args) (setq notified args))))
          (niwa-view--drain-follow-up data-buf))
        (should (niwa-session-pending-follow-ups session))
        (should-not notified)))))

(niwa-deftest niwa-collaboration--refuse-unseen-question
		 (:doc "refuses a question on a daemon's invisible frame while sharing, and only then")
		 (progn
		   (should (memq #'niwa-collaboration--refuse-unseen-question minibuffer-setup-hook))
		   (let ((niwa-collaboration--rooms (make-hash-table :test #'eq))
			 (daemon t) (initial t))
		     (cl-letf (((symbol-function 'daemonp) (lambda () daemon))
			       ((symbol-function 'active-minibuffer-window) #'selected-window)
			       ((symbol-function 'frame-initial-p) (lambda (&rest _) initial))
			       ((symbol-function 'minibuffer-prompt)
				(lambda () (propertize "Password for root@target: " 'face 'bold))))
		       ;; Nothing is shared: questions keep their usual course.
		       (should-not (niwa-collaboration--refuse-unseen-question))
		       (puthash 'room '(:session nil) niwa-collaboration--rooms)
		       (should (equal '(inhibited-interaction "Password for root@target:")
				      (should-error (niwa-collaboration--refuse-unseen-question)
						    :type 'inhibited-interaction)))
		       ;; A person on a client frame answers it.
		       (setq initial nil)
		       (should-not (niwa-collaboration--refuse-unseen-question))
		       ;; An ordinary Emacs has someone at the keyboard.
		       (setq initial t daemon nil)
		       (should-not (niwa-collaboration--refuse-unseen-question))))))

(niwa-deftest niwa-collaboration-notify-queue-changed
  (:doc "re-publishes the queue when it changes without a request")
  (let* ((workspace (niwa-workspace-get-or-create
                     'project "/tmp/collab-notify/" "/tmp/collab-notify/" "cn"))
         (session (niwa-session-create "main" workspace))
         (guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-notify-data*"))
         (room (list :session session :guests guests
                     :data-buffer data-buffer :transport 'transport
                     :queue nil :status nil))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t)))
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "g1")
                   guests)
          ;; A local command drains its entry without starting a turn,
          ;; so nothing else would tell the guest its card is stale.
          (niwa-session-enqueue-pending-input
           session 'follow-up (list :input "" :guest-invoke "plan"))
          (niwa-collaboration-notify-queue-changed session)
          (should (cl-find-if (lambda (entry)
                                (equal "queue" (plist-get (cdr entry) :t)))
                              sent))
          (setq sent nil)
          (niwa-session-set-pending-inputs session 'follow-up nil)
          (niwa-collaboration-notify-queue-changed session)
          (let ((queue (cl-find-if
                        (lambda (entry)
                          (equal "queue" (plist-get (cdr entry) :t)))
                        sent)))
            (should queue)
            (should (= 0 (plist-get (cdr queue) :pending)))
            (should-not (plist-get (cdr queue) :own)))
          ;; An unshared session is simply not a room.
          (should-not (niwa-collaboration-notify-queue-changed nil)))
      (kill-buffer data-buffer)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-view--drop-disallowed-guest-skills
  (:doc "drops queued guest invocations the allowlist no longer admits"
   :quiet t)
  (let ((session (niwa-session--create :name "s"))
        (niwa-collaboration-guest-skills '((full "plan") (owner . t))))
    (niwa-session-enqueue-pending-input
     session 'follow-up '(:input "" :guest-invoke "plan" :guest-role full))
    (niwa-session-enqueue-pending-input
     session 'follow-up '(:input "" :guest-invoke "review" :guest-role full))
    ;; The same name queued under the owner tier stays.
    (niwa-session-enqueue-pending-input
     session 'follow-up '(:input "owner" :guest-invoke "review"
                          :guest-role owner))
    (niwa-session-enqueue-pending-input
     session 'follow-up '(:input "plain question"))
    (should (equal '("" "owner" "plain question")
                   (mapcar (lambda (entry) (plist-get entry :input))
                           (niwa-view--drop-disallowed-guest-skills
                            session))))
    ;; Losing admission for any selected skill drops the entire message.
    (niwa-session-enqueue-pending-input
     session 'follow-up '(:input "combined" :guest-skills ("missing")
                          :guest-role full))
    ;; Nothing to drop leaves the queue untouched and quiet.
    (should (= 3 (length (niwa-view--drop-disallowed-guest-skills
                          session))))))


;;
;;; Room lifecycle

(niwa-deftest niwa-collaboration--keep-awake-state
  (:doc "uses guests or the later start/departure baseline, with an exact cutoff")
  (let* ((niwa-collaboration-keep-awake-idle-time 100)
         (guests (make-hash-table :test #'eql))
         (room (list :started-at 1000 :last-guest-departure nil :guests guests)))
    (should (= 1100 (niwa-collaboration--keep-awake-state room 1099)))
    (should-not (niwa-collaboration--keep-awake-state room 1100))
    (plist-put room :last-guest-departure 1050)
    (should (= 1150 (niwa-collaboration--keep-awake-state room 1100)))
    (plist-put room :last-guest-departure 900)
    (should (= 1100 (niwa-collaboration--keep-awake-state room 1099)))
    (puthash 1 '(:name "guest") guests)
    (should (eq t (niwa-collaboration--keep-awake-state room 10000)))
    (clrhash guests)
    (let ((niwa-collaboration-keep-awake-idle-time nil))
      (should (eq t (niwa-collaboration--keep-awake-state room 10000))))
    (let ((niwa-collaboration-keep-awake-idle-time 0))
      (should-not (niwa-collaboration--keep-awake-state room 1000)))
    (should-not (niwa-collaboration--keep-awake-state '(:guests nil) 1000))))

(niwa-deftest niwa-collaboration-sleep-reasons
  (:doc "deduplicates open shares, lobbies and shared artifacts even through relay outages")
  (let ((niwa-collaboration--rooms (make-hash-table :test #'eq))
        (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
        (niwa-collaboration-artifact-room--rooms (make-hash-table :test #'equal))
        (niwa-collaboration-keep-awake-idle-time 100))
    (should-not (niwa-collaboration-sleep-reasons 1000))
    (puthash 'a '(:started-at 1000) niwa-collaboration--rooms)
    (puthash 'b '(:started-at 1000) niwa-collaboration--rooms)
    (puthash "/root/" '(:started-at 1000) niwa-collaboration-lobby--lobbies)
    (puthash '("/root/" . "a") '(:started-at 900) niwa-collaboration-artifact-room--rooms)
    (puthash '("/root/" . "b") '(:started-at 1000)
             niwa-collaboration-artifact-room--rooms)
    (should (equal '("collaboration share active" "collaboration lobby active"
                     "shared artifact active")
                   (niwa-collaboration-sleep-reasons 1099)))
    (should-not (niwa-collaboration-sleep-reasons 1100))
    ;; A connected consumer keeps it awake past the idle interval.
    (let ((guests (make-hash-table :test #'eql)))
      (puthash 1 '(:name "remote") guests)
      (puthash '("/root/" . "a") (list :started-at 900 :guests guests)
               niwa-collaboration-artifact-room--rooms)
      (should (equal '("shared artifact active")
                     (niwa-collaboration-sleep-reasons 5000))))
    (clrhash niwa-collaboration-artifact-room--rooms)
    (let ((niwa-collaboration-keep-awake-idle-time nil))
      (clrhash niwa-collaboration--rooms)
      (should (equal '("collaboration lobby active")
                     (niwa-collaboration-sleep-reasons 2000)))
      (clrhash niwa-collaboration-lobby--lobbies)
      (should-not (niwa-collaboration-sleep-reasons 2000)))))

(niwa-deftest niwa-collaboration--keep-awake-status
  (:doc "shows active, idle expiry and expired request states")
  (let ((niwa-collaboration-keep-awake-idle-time 100)
        (room '(:started-at 1000)))
    (should (equal (format "Keep-awake request: until %s (idle cutoff)"
                           (format-time-string "%Y-%m-%d %H:%M:%S %Z" 1100))
                   (niwa-collaboration--keep-awake-status room 1000)))
    (should (equal "Keep-awake request: idle cutoff reached"
                   (niwa-collaboration--keep-awake-status room 1100)))
    (let ((niwa-collaboration-keep-awake-idle-time nil))
      (should (equal "Keep-awake request: active"
                     (niwa-collaboration--keep-awake-status room 1100))))))

(niwa-deftest niwa-collaboration--room-status
  (:doc "includes the keep-awake request without claiming OS success")
  (let ((room (list :session-label "Shared" :started-at 1000
                    :guests (make-hash-table :test #'eql)))
        (niwa-collaboration-keep-awake-idle-time 100))
    (cl-letf (((symbol-function 'niwa-collaboration--transport-open-p)
               (lambda (_) nil))
              ((symbol-function 'float-time) (lambda (&optional _) 1100)))
      (should (equal "Shared: relay reconnecting; no guest connected; Keep-awake request: idle cutoff reached"
                     (niwa-collaboration--room-status room))))))

(niwa-deftest niwa-collaboration--start
  (:doc "builds the room and all three bearer links without scheduling expiry")
  (with-temp-buffer
    (let ((niwa-collaboration-relay-url "ws://127.0.0.1:1")
          (niwa-collaboration-relay-host-token "test-token")
          (niwa-collaboration--rooms (make-hash-table :test #'eq))
          (session (niwa-session--create :name "share"))
          (sleep-updates 0)
          dialed scheduled room)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'niwa-collaboration--transport-open)
                       (lambda (url _key &rest _) (setq dialed url) 'transport))
                      ((symbol-function 'niwa-sleep-update)
                       (lambda () (cl-incf sleep-updates)))
                      ((symbol-function 'niwa-collaboration--canonical-records)
                       (lambda (&rest _) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest args)
                         (push args scheduled)
                         'timer)))
              (setq room (niwa-collaboration--start session
                                                        (current-buffer))))
            (should (= 1 sleep-updates))
            (should (numberp (plist-get room :started-at)))
            (should-not (plist-get room :last-guest-departure))
            (should (string-match
                     "\\`ws://127\\.0\\.0\\.1:1/r/\\([A-Za-z0-9_-]+\\)\\?role=host\\'"
                     dialed))
            (let ((room-id (match-string 1 dialed)))
              (should (equal room-id (plist-get room :room-id)))
              (should (string-prefix-p
                       (format "http://127.0.0.1:1/#%s." room-id)
                       (plist-get room :link-view)))
              (should (string-prefix-p
                       (format "http://127.0.0.1:1/#%s." room-id)
                       (plist-get room :link-full)))
              (should (string-prefix-p
                       (format "http://127.0.0.1:1/#%s." room-id)
                       (plist-get room :link-owner))))
            ;; Each tier's secret is the previous one plus its own token,
            ;; which is what lets the viewer tell them apart by length.
            (let* ((secret
                    (lambda (slot)
                      (niwa-collaboration--base64url-decode
                       (car (last (split-string (plist-get room slot)
                                                "\\."))))))
                   (key (plist-get room :key)))
              (should (equal (funcall secret :link-view) key))
              (should (equal (funcall secret :link-full)
                             (concat key (plist-get room :write-token))))
              (should (equal (funcall secret :link-owner)
                             (concat key (plist-get room :write-token)
                                     (plist-get room :owner-token))))
              (should (= (length (funcall secret :link-owner)) 64))
              ;; A distinct token, not a repeat of the write one.
              (should-not (equal (plist-get room :owner-token)
                                 (plist-get room :write-token))))
            (should-not scheduled)
            ;; The room follows its session's renames.
            (let (published)
              (cl-letf (((symbol-function
                          'niwa-collaboration-notify-request-changed)
                         (lambda (buffer) (push buffer published))))
                (niwa-session-control-transfer-notify
                 session 'rename "renamed"))
              (should (equal (list (current-buffer)) published)))
            ;; Restarting for the same session reuses the room.
            (should (eq room (niwa-collaboration--start
                              session (current-buffer)))))
        (when room
          (niwa-session-control-transfer-unregister-observer
           session (plist-get room :session-observer)))
        (remove-hook 'kill-emacs-hook
                     #'niwa-collaboration--stop-for-emacs)
        (remove-hook 'niwa-interaction-prompt-created-hook
                     #'niwa-collaboration--on-prompt-created)
        (remove-hook 'niwa-interaction-prompt-settled-hook
                     #'niwa-collaboration--on-prompt-settled)))))

(defmacro niwa-test--with-stub-relay (sent &rest body)
  "Run BODY with the relay transport stubbed, pushing (PEER . FRAME) to SENT.
Remove the global hooks a started room installs afterwards."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (cl-letf (((symbol-function 'niwa-collaboration--transport-open)
                  (lambda (url &rest _) (list :url url)))
                 ((symbol-function 'niwa-collaboration--transport-send)
                  (lambda (_transport peer frame)
                    (push (cons peer frame) ,sent)
                    t))
                 ((symbol-function 'niwa-collaboration--transport-stop)
                  #'ignore)
                 ((symbol-function 'niwa-collaboration--transport-control)
                  #'ignore)
                 ((symbol-function 'niwa-collaboration-presence-publish)
                  #'ignore)
                 ((symbol-function 'niwa-sleep-update) #'ignore)
                 ((symbol-function 'niwa-collaboration--canonical-records)
                  (lambda (&rest _) nil)))
         ,@body)
     (dolist (room (niwa-collaboration--room-list))
       (niwa-collaboration--stop-internal room 'user-stop))
     (remove-hook 'kill-emacs-hook #'niwa-collaboration--stop-for-emacs)
     (remove-hook 'niwa-interaction-prompt-created-hook
                  #'niwa-collaboration--on-prompt-created)
     (remove-hook 'niwa-interaction-prompt-settled-hook
                  #'niwa-collaboration--on-prompt-settled)))

(defun niwa-test--adopted-bodies (data-buf)
  "Share DATA-BUF and return the ui-request bodies a joining guest receives."
  (let ((niwa-collaboration-relay-url "ws://127.0.0.1:1")
        (niwa-collaboration--rooms (make-hash-table :test #'eq))
        sent)
    (niwa-test--with-stub-relay sent
      (let ((room (niwa-collaboration--start
                   (niwa-session--create :name "late-share") data-buf)))
        (puthash 7 (list :name "g" :writable t :ready t)
                 (plist-get room :guests))
        (setq sent nil)
        (niwa-collaboration--send-ui-requests room 7)
        (mapcar (lambda (entry)
                  (should (equal "ui-request" (plist-get (cdr entry) :t)))
                  (plist-get (cdr entry) :body))
                (reverse sent))))))

(niwa-deftest niwa-collaboration--adopt-prompts ()
  ,test
  (test)

  :doc "shows guests a request raised before the session was shared"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (niwa--prompt-user-with-overlay
       "Confirm" "Raised before sharing" "Proceed?" nil #'ignore))
    (let ((bodies (niwa-test--adopted-bodies data-buf)))
      (should (= 1 (length bodies)))
      (should (string-match-p "Raised before sharing" (car bodies)))))

  :doc "shows guests a queued tool permission raised before the session was shared"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :permission-mode 'ask)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (niwa-permission-request
         (list :kind 'generic :tool-name "EarlyTool" :origin "/root"
               :data-buffer data-buf :callback #'ignore)
         session))
      (let ((bodies (niwa-test--adopted-bodies data-buf)))
        (should (= 1 (length bodies)))
        (should (string-match-p "EarlyTool" (car bodies))))))

  :doc "adopts waiting interactions in the order they opened"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (niwa--prompt-user-with-overlay
       "Confirm" "First request" "Proceed?" nil #'ignore)
      (niwa--prompt-user-with-overlay
       "Confirm" "Second request" "Proceed?" nil #'ignore))
    (let ((bodies (niwa-test--adopted-bodies data-buf)))
      (should (= 2 (length bodies)))
      (should (string-match-p "First request" (nth 0 bodies)))
      (should (string-match-p "Second request" (nth 1 bodies))))))

(niwa-deftest niwa-collaboration-hand-off ()
  ,test
  (test)

  :doc "hands each guest the successor room at its own tier"
  (let ((source (generate-new-buffer " *collab-handoff-source*"))
        (target (generate-new-buffer " *collab-handoff-target*"))
        (niwa-collaboration-relay-url "ws://127.0.0.1:1")
        (niwa-collaboration--rooms (make-hash-table :test #'eq))
        sent)
    (unwind-protect
        (niwa-test--with-stub-relay sent
          (with-current-buffer target
            (setq-local niwa--session
                        (niwa-session--create :name "implement-plan")))
          (let ((room (niwa-collaboration--start
                       (niwa-session--create :name "plan") source))
                (guests '((1 :name "viewer")
                          (2 :name "writer" :writable t)
                          (3 :name "owner" :writable t :owner t))))
            (pcase-dolist (`(,peer . ,guest) guests)
              (puthash peer guest (plist-get room :guests)))
            (let ((successor (niwa-collaboration-hand-off source target)))
              (should (eq successor
                          (niwa-collaboration--room-for-buffer target)))
              (should-not (niwa-collaboration--room-for-buffer source))
              (pcase-dolist (`(,peer . ,link)
                             `((1 . ,(plist-get successor :link-view))
                               (2 . ,(plist-get successor :link-full))
                               (3 . ,(plist-get successor :link-owner))))
                (let ((frame (cdr (assq peer sent))))
                  (should (equal "bye" (plist-get frame :t)))
                  (should (equal "handoff" (plist-get frame :reason)))
                  (should (equal "implement-plan" (plist-get frame :name)))
                  (should (equal link (plist-get frame :link)))))
              ;; Per-guest farewells replace the room-wide broadcast.
              (should-not (assq 0 sent)))))
      (kill-buffer source)
      (kill-buffer target)))

  :doc "names an unnamed successor by nothing rather than its ID"
  (let ((source (generate-new-buffer " *collab-handoff-source*"))
        (target (generate-new-buffer " *collab-handoff-target*"))
        (niwa-collaboration-relay-url "ws://127.0.0.1:1")
        (niwa-collaboration--rooms (make-hash-table :test #'eq))
        sent)
    (unwind-protect
        (niwa-test--with-stub-relay sent
          (with-current-buffer target
            ;; A fork is named by its ID until its first prompt titles it.
            (setq-local niwa--session
                        (niwa-session--create :name "20261019T0800-fork"
                                                 :session-id "20261019T0800-fork"
                                                 :naming-state 'pending)))
          (let ((room (niwa-collaboration--start
                       (niwa-session--create :name "plan") source)))
            (puthash 2 (list :name "writer" :writable t)
                     (plist-get room :guests))
            (should (niwa-collaboration-hand-off source target))
            (let ((frame (cdr (assq 2 sent))))
              (should (equal "handoff" (plist-get frame :reason)))
              (should-not (plist-get frame :name))
              (should (stringp (plist-get frame :link))))))
      (kill-buffer source)
      (kill-buffer target)))

  :doc "does nothing for a session that is not shared"
  (let ((source (generate-new-buffer " *collab-handoff-source*"))
        (target (generate-new-buffer " *collab-handoff-target*"))
        (niwa-collaboration--rooms (make-hash-table :test #'eq))
        sent)
    (unwind-protect
        (niwa-test--with-stub-relay sent
          (with-current-buffer target
            (setq-local niwa--session (niwa-session--create :name "x")))
          (should-not (niwa-collaboration-hand-off source target))
          (should-not (niwa-collaboration--room-list))
          (should-not sent))
      (kill-buffer source)
      (kill-buffer target)))

  :doc "tells guests the work continues in Emacs when the successor cannot be shared"
  (let ((source (generate-new-buffer " *collab-handoff-source*"))
        (target (generate-new-buffer " *collab-handoff-target*"))
        (niwa-collaboration-relay-url "ws://127.0.0.1:1")
        (niwa-collaboration--rooms (make-hash-table :test #'eq))
        sent warning)
    (unwind-protect
        (niwa-test--with-stub-relay sent
          (let ((room (niwa-collaboration--start
                       (niwa-session--create :name "plan") source)))
            (puthash 2 (list :name "writer" :writable t)
                     (plist-get room :guests))
            ;; TARGET has no session, so sharing it fails.
            (with-current-buffer target
              (setq-local niwa--workspace (niwa-workspace--create :root "/target/project/")))
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (_type message &rest _) (setq warning message))))
              (should-not (niwa-collaboration-hand-off source target)))
            (should (string-match-p "Could not share" warning))
            (should (string-prefix-p "niwa [/target/project/]: " warning))
            (should (= 1 (cl-count ?\[ warning)))
            (should-not (niwa-collaboration--room-list))
            (should (equal '((0 :t "bye" :reason "handoff-failed")) sent))))
      (kill-buffer source)
      (kill-buffer target))))

(niwa-deftest niwa-collaboration--multi-room
  (:doc "gives each shared session its own independent room")
  (let ((data-a (generate-new-buffer " *collab-multi-a*"))
        (data-b (generate-new-buffer " *collab-multi-b*"))
        (niwa-collaboration-relay-url "ws://127.0.0.1:1")
        (niwa-collaboration-relay-host-token "test-token")
        (niwa-collaboration--rooms (make-hash-table :test #'eq))
        (session-a (niwa-session--create :name "a"))
        (session-b (niwa-session--create :name "b"))
        stopped-transports room-a room-b)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--transport-open)
                   (lambda (url &rest _) (list :url url)))
                  ((symbol-function 'niwa-collaboration--transport-stop)
                   (lambda (transport) (push transport stopped-transports)))
                  ((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (&rest _) t))
                  ((symbol-function 'niwa-collaboration--canonical-records)
                   (lambda (&rest _) nil)))
          (setq room-a (niwa-collaboration--start session-a data-a)
                room-b (niwa-collaboration--start session-b data-b))
          ;; Two live rooms with distinct credentials and links.
          (should (= 2 (length (niwa-collaboration--room-list))))
          (should-not (eq room-a room-b))
          (should-not (equal (plist-get room-a :key)
                             (plist-get room-b :key)))
          (should-not (equal (plist-get room-a :link-view)
                             (plist-get room-b :link-view)))
          ;; A frame for one data buffer reaches only that room.
          (let (handled)
            (cl-letf (((symbol-function 'niwa-collaboration--handle-hello)
                       (lambda (room _peer _frame) (push room handled))))
              (niwa-collaboration--on-frame data-a 1
                                               (list :t "hello" :proto 3)))
            (should (equal (list room-a) handled)))
          ;; Stopping one room leaves the other live and untouched.
          (niwa-collaboration--stop-internal room-a 'user-stop)
          (should (equal (list room-b)
                         (niwa-collaboration--room-list)))
          (should (equal (list (plist-get room-a :transport))
                         stopped-transports))
          (niwa-collaboration--stop-internal room-b 'user-stop)
          (should-not (niwa-collaboration--room-list)))
      (kill-buffer data-a)
      (kill-buffer data-b)
      (remove-hook 'kill-emacs-hook
                   #'niwa-collaboration--stop-for-emacs)
      (remove-hook 'niwa-interaction-prompt-created-hook
                   #'niwa-collaboration--on-prompt-created)
      (remove-hook 'niwa-interaction-prompt-settled-hook
                   #'niwa-collaboration--on-prompt-settled))))

(niwa-deftest niwa-collaboration--stop-internal
  (:doc "finishes core teardown when share presentation cleanup signals")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (sent nil)
           (transport-stopped nil)
           (session (niwa-session--create :name "stopping"))
           (publish-timer (run-at-time 60 nil #'ignore))
           (room (list :transport 'transport
                       :session session
                       :session-observer
                       (niwa-session-control-transfer-register-observer
                        session #'ignore)
                       :data-buffer (current-buffer)
                       :guests guests
                       :publish-timer publish-timer)))
      (puthash 1 (list :name "g") guests)
      (let ((niwa-collaboration--rooms
             (niwa-test-room-registry room)))
        (cl-letf
            (((symbol-function 'niwa-collaboration-share-dismiss)
              (lambda (_room) (error "Injected share cleanup failure")))
             ((symbol-function 'niwa-collaboration--transport-send)
              (lambda (_transport peer frame)
                (push (cons peer frame) sent)
                t))
             ((symbol-function 'niwa-collaboration--transport-stop)
              (lambda (_transport) (setq transport-stopped t))))
          (niwa-collaboration--stop-internal room 'user-stop))
        (should-not (niwa-collaboration--room-list)))
      (should-not (memq publish-timer timer-list))
      (should (equal "bye" (plist-get (cdr (car sent)) :t)))
      (should transport-stopped)
      ;; The stopped room no longer follows its session.
      (should-not (gethash session
                           niwa-session-control-transfer--observers)))))

(niwa-deftest niwa-collaboration--stop-for-buffer
  (:doc "stops the room when its owning data buffer is killed")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           stopped)
      (cl-letf (((symbol-function 'niwa-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason))))
        (niwa-collaboration--stop-for-buffer)
        (should (eq 'data-buffer-killed stopped))
        ;; A caller killing it for a reason tells the guests that one.
        (let ((niwa-collaboration-stop-reason 'artifact-deleted))
          (niwa-collaboration--stop-for-buffer))
        (should (eq 'artifact-deleted stopped))))))

(niwa-deftest niwa-collaboration--stop-for-session
  ()
  ,test
  (test)

  :doc "stops the room when its owning data buffer ends the session"
  (with-temp-buffer
    (let* ((stopped nil)
           (room (list :data-buffer (current-buffer)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (niwa-session-end-reason "kill-buffer"))
      (cl-letf (((symbol-function 'niwa-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason))))
        (niwa-collaboration--stop-for-session)
        (should (eq 'data-buffer-killed stopped)))))

  :doc "a session ending at Emacs exit stops its room as Emacs exit does"
  (with-temp-buffer
    (let* ((stopped nil)
           (room (list :data-buffer (current-buffer)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (niwa-session-end-reason "exit"))
      (cl-letf (((symbol-function 'niwa-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason))))
        (niwa-collaboration--stop-for-session)
        (should (eq 'emacs-exit stopped))))))

(niwa-deftest niwa-collaboration--stop-for-emacs
  (:doc "stops every room when Emacs exits")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           stopped)
      (cl-letf (((symbol-function 'niwa-collaboration--stop-internal)
                 (lambda (seen-room reason)
                   (setq stopped (cons seen-room reason)))))
        (niwa-collaboration--stop-for-emacs)
        (should (eq room (car stopped)))
        (should (eq 'emacs-exit (cdr stopped)))))))

;;; test-niwa-collaboration.el ends here
