;;; test-mevedel-collaboration.el --- Collaboration facade and projection tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the collaboration facade, publication, lifecycle, and public commands.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-agent-control)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-pending-inputs)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-chat)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-composer)
(require 'mevedel-view-input-files)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-ui)


;;
;;; Credentials and links

(mevedel-deftest mevedel-collaboration--base64url
  (:doc "encodes URL-safe unpadded base64, decodes it back, and returns nil for malformed input")
  (progn
    (dolist (count '(16 32 48))
      (let* ((bytes (mevedel-collaboration--random-bytes count))
             (encoded (mevedel-collaboration--base64url bytes)))
        (should (string-match-p "\\`[A-Za-z0-9_-]+\\'" encoded))
        (should (equal bytes
                       (mevedel-collaboration--base64url-decode encoded)))))
    (should (equal (mevedel-collaboration--base64url
                    (unibyte-string #xfb #xff #xfe))
                   "-__-"))
    (should-not (mevedel-collaboration--base64url-decode nil))
    (should-not (mevedel-collaboration--base64url-decode "not base64 !!"))))

(mevedel-deftest mevedel-collaboration--random-bytes
  (:doc "returns the requested count and fails without OS randomness")
  (progn
    (should (= 32 (string-bytes (mevedel-collaboration--random-bytes 32))))
    (cl-letf (((symbol-function 'insert-file-contents-literally)
               (lambda (&rest _) (error "No such file"))))
      (should-error (mevedel-collaboration--random-bytes 32)
                    :type 'user-error))))

(mevedel-deftest mevedel-collaboration--relay-origins
  (:doc "derives the web origin from a ws origin and rejects other shapes")
  (progn
    (let ((mevedel-collaboration-relay-url "wss://collab.example"))
      (should (equal '("wss://collab.example" . "https://collab.example")
                     (mevedel-collaboration--relay-origins))))
    (let ((mevedel-collaboration-relay-url "ws://127.0.0.1:7466/"))
      (should (equal '("ws://127.0.0.1:7466" . "http://127.0.0.1:7466")
                     (mevedel-collaboration--relay-origins))))
    (dolist (bad '("https://collab.example" "collab.example"
                   "wss://collab.example/path" nil ""))
      (let ((mevedel-collaboration-relay-url bad))
        (should-error (mevedel-collaboration--relay-origins)
                      :type 'user-error)))))

(mevedel-deftest mevedel-collaboration--sanitize-guest-name
  (:doc "bounds guest names, strips control characters, and never yields empty")
  (progn
    (should (equal "guest" (mevedel-collaboration--sanitize-guest-name nil)))
    (should (equal "guest" (mevedel-collaboration--sanitize-guest-name "  ")))
    (should (equal "a b" (mevedel-collaboration--sanitize-guest-name
                          "a\n\t\rb")))
    (should (= mevedel-collaboration--max-guest-name-chars
               (length (mevedel-collaboration--sanitize-guest-name
                        (make-string 200 ?x)))))))


;;
;;; Canonical projection

(mevedel-deftest mevedel-collaboration--canonical-records
  (:doc "uses allowlisted canonical text, stable identities, and revision zero")
  (with-temp-buffer
    (insert "prompt\nanswer\n")
    (let ((data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'mevedel-transcript-segments)
                 (lambda (_start _end)
                   (list (list 'user 1 4) (list 'response 4 11))))
                ((symbol-function 'mevedel-view--user-turn-text)
                 (lambda (_segments _buffer) "visible prompt"))
                ((symbol-function 'mevedel-view--visible-response-text)
                 (lambda (_text) "visible answer")))
        (let ((records (mevedel-collaboration--canonical-records data-buffer)))
          (should (= 2 (length records)))
          (should (equal '(0 0) (mapcar (lambda (r) (plist-get r :revision))
                                        records)))
          (should-not (string-match-p "\\`user-[0-9]+\\'"
                                      (plist-get (car records) :id))))))))

(mevedel-deftest mevedel-collaboration--directive-at
  (:doc "maps a position to its owning directive range")
  (let ((ranges (list (list :start 10 :end 20 :directive-id "dir-1")
                      (list :start 30 :end 40 :directive-id "dir-2"))))
    (should (equal "dir-1" (mevedel-collaboration--directive-at ranges 10)))
    (should (equal "dir-1" (mevedel-collaboration--directive-at ranges 19)))
    (should-not (mevedel-collaboration--directive-at ranges 20))
    (should (equal "dir-2" (mevedel-collaboration--directive-at ranges 35)))
    (should-not (mevedel-collaboration--directive-at ranges 5))
    (should-not (mevedel-collaboration--directive-at nil 5))))

(mevedel-deftest mevedel-collaboration--directive-ranges
  (:doc "tags records inside directive turns and serializes the id")
  (with-temp-buffer
    (insert "prompt\nanswer\n")
    (let ((data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'mevedel-transcript-segments)
                 (lambda (_start _end)
                   (list (list 'user 1 7) (list 'response 8 14))))
                ((symbol-function 'mevedel-collaboration--directive-ranges)
                 (lambda ()
                   (list (list :start 8 :end 14 :directive-id "dir-1"))))
                ((symbol-function 'mevedel-view--user-turn-text)
                 (lambda (_segments _buffer) "visible prompt"))
                ((symbol-function 'mevedel-view--visible-response-text)
                 (lambda (_text) "visible answer")))
        (let* ((records (mevedel-collaboration--canonical-records data-buffer))
               (user (car records))
               (response (cadr records)))
          (should-not (plist-member user :directive))
          (should (equal "dir-1" (plist-get response :directive)))
          (should (equal "dir-1"
                         (cdr (assoc "directive"
                                     (mevedel-collaboration--json-record
                                      response))))))))))

(mevedel-deftest mevedel-collaboration--tool-detail
  (:doc "summarizes the primary operand on one bounded line")
  (progn
    (should (equal "head -5 notes.txt"
                   (mevedel-collaboration--tool-detail
                    '(:command "head -5 notes.txt\nsecond line"))))
    (should (equal "/tmp/a.el"
                   (mevedel-collaboration--tool-detail
                    '(:file_path "/tmp/a.el"))))
    (should-not (mevedel-collaboration--tool-detail nil))
    (should-not (mevedel-collaboration--tool-detail '(:other 42)))
    (should (<= (length (mevedel-collaboration--tool-detail
                         (list :command (make-string 900 ?x))))
                (+ 200 (length "\n[truncated]"))))))

(mevedel-deftest mevedel-collaboration--allowlist
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
         (payload (mapcar #'mevedel-collaboration--json-record records)))
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
                                (mevedel-collaboration--json-string payload)))))

(mevedel-deftest mevedel-collaboration--canonical-excluded-spans
  (:doc "projects visible payload exactly while excluding control and unknown spans")
  (with-temp-buffer
    (let ((visible-user "Visible user [bad](javascript:alert(1)) <img src=secret.png>\n")
          (visible-answer "Visible answer with <b>literal</b> text\n"))
      (insert visible-user)
      (let ((response-start (point)))
        (insert visible-answer)
        (put-text-property response-start (point) 'gptel 'response))
      (dolist (block '("#+begin_reasoning\nreason secret\n#+end_reasoning\n"
                       "<system-reminder>\nreminder secret\n</system-reminder>\n"
                       "<hook-context>\nhook secret\n</hook-context>\n"
                       "<!-- mevedel-render-data -->\n(:kind media :path \"/secret.png\")\n<!-- /mevedel-render-data -->\n"
                       "<!-- mevedel-hook-audit -->\nunknown secret\n<!-- /mevedel-hook-audit -->\n"))
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
      (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
             (payload (mapcar #'mevedel-collaboration--json-record records)))
        (should (= 3 (length records)))
        (should (equal (string-trim visible-user)
                       (cdr (assoc "text" (car payload)))))
        (should (equal (string-trim visible-answer)
                       (cdr (assoc "text" (cadr payload)))))
        (should (equal "Visible tool result"
                       (cdr (assoc "result" (nth 2 payload)))))
        (let ((json (mevedel-collaboration--json-string payload)))
          (dolist (hidden '("reason secret" "reminder secret" "hook secret"
                            "/secret.png" "unknown secret"
                            "Unknown kind payload"))
            (should-not (string-match-p (regexp-quote hidden) json))))))))

(mevedel-deftest mevedel-collaboration--local-remote-parity
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
           (wire (mevedel-collaboration--json-string
                  (mapcar #'mevedel-collaboration--json-record
                          (mevedel-collaboration--project-records
                           remote-room)))))
      (should (string-match-p "Answer from the shared session" wire))
      (should-not (string-match-p "execution-target\\|/ssh:\\|example" wire)))))

(mevedel-deftest mevedel-collaboration--tool-record
  (:doc "marks empty settled tool output completed rather than running")
  (with-temp-buffer
    (insert "tool")
    (let ((record (mevedel-collaboration--tool-record
                   '(:name "Bash" :result "") "tool")))
      (should (equal "completed" (plist-get record :status)))
      (should (equal "" (plist-get record :result))))
    ;; Leading spaces on the first result line are significant alignment
    ;; (Read's right-aligned line numbers); only newlines are trimmed.
    (let ((record (mevedel-collaboration--tool-record
                   '(:name "Read" :args (:file_path "/tmp/a.el")
                     :result "\n  1\t;;; header\n  2\tbody\n")
                   "tool")))
      (should (string-prefix-p "  1\t" (plist-get record :result)))
      (should (equal "/tmp/a.el" (plist-get record :detail))))
    ;; ApplyPatch carries the authored patch as a dedicated diff field.
    (let ((record (mevedel-collaboration--tool-record
                   '(:name "ApplyPatch"
                     :args (:patch "@@ -1 +1 @@\n-old\n+new")
                     :result "Applied patch: 1 changes")
                   "tool")))
      (should (equal "@@ -1 +1 @@\n-old\n+new" (plist-get record :diff)))
      (should (equal "@@ -1 +1 @@\n-old\n+new"
                     (cdr (assoc "diff"
                                 (mevedel-collaboration--json-record
                                  record))))))
    ;; Settled selected ApplyPatch render data is the artifact authority.
    (let* ((save-path (make-temp-file "mevedel-collab-tool-artifact-" t))
           (dir (mevedel-session-artifacts-artifacts-dir save-path))
           (path (file-name-concat dir "mockup.html")))
      (unwind-protect
          (progn
            (setq-local mevedel--session
                        (mevedel-session--create :name "s"
                                                 :save-path save-path))
            (make-directory dir t)
            (write-region "<h1>hi</h1>" nil path nil 'silent)
            (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                       (lambda (_buffer _start _end)
                         `(:name "ApplyPatch" :args (:patch "patch")
                           :result "Applied patch: 1 changes"
                           :render-data
                           (:kind patch :files
                            ((:kind add :path ,path)))))))
              (let* ((record (car (mevedel-collaboration--tool-segment-records
                                   (current-buffer) '(tool 1 5))))
                     (json (mevedel-collaboration--json-record record)))
                (should (equal "mockup.html" (plist-get record :artifact)))
                (should (= 11 (cdr (assoc "size" json))))
                (should (equal "mockup.html" (cdr (assoc "artifact" json))))
                (should-not (assoc "artifact-path" json)))))
        (kill-local-variable 'mevedel--session)
        (mevedel-collaboration--artifact-stat-invalidate)
        (delete-directory save-path t)))))

(mevedel-deftest mevedel-collaboration--pre-tool
  (:doc "publishes one stable running tool record before settled completion")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :records nil :pending-tools nil
                       :tool-call-occurrences (make-hash-table :test #'equal)
                       :guests (make-hash-table :test #'eql)))
           (info '(:name "Bash" :args (:command "true")))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           canonical invalidated)
      (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
                 (lambda (_) canonical))
                ((symbol-function
                  'mevedel-collaboration--artifact-stat-invalidate)
                 (lambda () (setq invalidated t))))
        (should-not (mevedel-collaboration--pre-tool info))
        (let* ((pending (plist-get room :pending-tools))
               (entry (car pending))
               (id (plist-get entry :id)))
          (should (= 1 (length pending)))
          (should (equal "running" (plist-get entry :status)))
          (should (stringp id))
          (should-not (mevedel-collaboration--pre-tool info))
          (should (= 1 (length (plist-get room :pending-tools))))
          (setq canonical
                (list (list :id "canonical" :kind "tool" :revision 0
                            :name "Bash" :status "completed"
                            :summary "Bash" :result "" :truncated nil)))
          (should-not (mevedel-collaboration--post-tool
                       '(:name "Bash" :args (:command "true") :result "")))
          (setq entry (car (plist-get room :records)))
          (should (equal id (plist-get entry :id)))
          (should (equal "completed" (plist-get entry :status)))
          (should (equal "" (plist-get entry :result)))
          (should-not (plist-get room :pending-tools))
          (should-not invalidated)
          ;; ApplyPatch can settle several target-native paths, so the small
          ;; qualified-path stat cache is cleared wholesale.
          (mevedel-collaboration--post-tool
           '(:name "ApplyPatch" :args (:patch "patch")
             :result ""))
          (should invalidated))))))

(mevedel-deftest mevedel-collaboration--project-records
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
    (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
               (lambda (_) (mapcar #'copy-sequence canonical))))
      (let ((records (mevedel-collaboration--project-records room)))
        ;; One card, keeping the pending identity with canonical content.
        (should (= 1 (length records)))
        (should (equal "pending-1" (plist-get (car records) :id)))
        (should (equal "completed" (plist-get (car records) :status)))
        (should (equal "transcript-formatted output"
                       (plist-get (car records) :result)))))
    ;; A pending whose canonical record has not landed stays visible.
    (let ((room (list :data-buffer 'data
                      :pending-tools (list (copy-sequence stuck)))))
      (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
                 (lambda (_) nil)))
        (let ((records (mevedel-collaboration--project-records room)))
          (should (= 1 (length records)))
          (should (equal "running" (plist-get (car records) :status))))))))

(mevedel-deftest mevedel-collaboration--clean-response
  (:doc "fails closed when canonical response projection is unavailable")
  (cl-letf (((symbol-function 'mevedel-view--visible-response-text)
             (lambda (_) (error "Projection failure"))))
    (should-error (mevedel-collaboration--clean-response "hidden"))))

(mevedel-deftest mevedel-collaboration--clean-user
  (:doc "fails closed when canonical user projection is unavailable")
  (with-temp-buffer
    (cl-letf (((symbol-function 'mevedel-view--user-turn-text)
               (lambda (_segments _buffer) nil)))
      (should-error
       (mevedel-collaboration--clean-user '(user 1 2) (current-buffer))))))


;;
;;; Publication

(mevedel-deftest mevedel-collaboration--publish
  (:doc "broadcasts simultaneous changes in canonical order with incremented revisions")
  (let* ((old (list (list :id "a" :kind "assistant" :revision 0 :text "one")
                    (list :id "b" :kind "assistant" :revision 0 :text "two")))
         (new (list (list :id "a" :kind "assistant" :revision 0 :text "ONE")
                    (list :id "b" :kind "assistant" :revision 0 :text "TWO")))
         (sent nil)
         (guests (make-hash-table :test #'eql))
         (room (list :records old :transport 'transport :guests guests)))
    (puthash 1 (list :name "g" :writable nil :ready t) guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--project-records)
               (lambda (_) new))
              ((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (mevedel-collaboration--publish room))
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

(mevedel-deftest mevedel-collaboration--attribute-guest-prompts
  (:doc "attributes each guest record to the nearest preceding user turn")
  (with-temp-buffer
    (insert "first prompt\nanswer\nsecond prompt\n")
    (insert (mevedel--format-hook-audit-record
             (list :type 'guest-prompt :name "phone")))
    (cl-letf (((symbol-function 'mevedel-transcript-segments)
               (lambda (_start _end)
                 (list (list 'user 1 13) (list 'response 14 20)
                       (list 'user 21 34))))
              ((symbol-function 'mevedel-view--user-turn-text)
               (lambda (segments _buffer)
                 (if (= 1 (cadr (car segments))) "first prompt"
                   "second prompt")))
              ((symbol-function 'mevedel-view--visible-response-text)
               (lambda (_text) "answer")))
      (let* ((records (mevedel-collaboration--canonical-records
                       (current-buffer)))
             (first-user (nth 0 records))
             (second-user (nth 2 records)))
        ;; The attribution block sits after both prompts, so it names the
        ;; last user turn starting before it: the second one.
        (should-not (plist-member first-user :guest))
        (should (equal "phone" (plist-get second-user :guest)))
        (should (equal "phone"
                       (cdr (assoc "guest"
                                   (mevedel-collaboration--json-record
                                    second-user)))))))
    ;; Segment repair can grow a user turn's END past its own audit block
    ;; (absorbing the trailing hidden records); attribution anchors on the
    ;; turn's START, so the badge must stay on the second turn even then.
    (cl-letf (((symbol-function 'mevedel-transcript-segments)
               (lambda (_start _end)
                 ;; The audit block written earlier sits after position 34;
                 ;; the second user segment's end has drifted beyond it.
                 (list (list 'user 1 13) (list 'response 14 20)
                       (list 'user 21 4000))))
              ((symbol-function 'mevedel-view--user-turn-text)
               (lambda (segments _buffer)
                 (if (= 1 (cadr (car segments))) "first prompt"
                   "second prompt")))
              ((symbol-function 'mevedel-view--visible-response-text)
               (lambda (_text) "answer")))
      (let* ((records (mevedel-collaboration--canonical-records
                       (current-buffer))))
        (should-not (plist-member (nth 0 records) :guest))
        (should (equal "phone" (plist-get (nth 2 records) :guest)))))))

(mevedel-deftest mevedel-collaboration--safe-accepted-prompt
  (:doc "publishes accepted prompt insertion and isolates observer failure")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           published stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                 (lambda (_room) (setq published t))))
        (should-not (mevedel-collaboration--safe-accepted-prompt
                     (current-buffer)))
        (should published))
      (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                 (lambda (_) (error "Viewer failed")))
                ((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason)))
                ((symbol-function 'display-warning) (lambda (&rest _) nil)))
        (should-not (mevedel-collaboration--safe-accepted-prompt
                     (current-buffer)))
        (should (eq 'observer-failure stopped))))))

(mevedel-deftest mevedel-collaboration--accepted-prompt-insertion-seams
  (:doc "publishes ordinary composer and generated turns at insertion")
  (let ((data-buffer (generate-new-buffer " *collaboration-prompt-seams*"))
        ordinary generated)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist nil)
            (setq-local mevedel-compact-run-in-flight nil))
          (with-temp-buffer
            (let ((mevedel--data-buffer data-buffer))
              (cl-letf (((symbol-function 'mevedel-view--ensure-interactive-chat-view)
                         (lambda () nil))
                        ((symbol-function 'mevedel-view--session)
                         (lambda () 'session))
                        ((symbol-function 'mevedel-request-assert-target-ready)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel-view--pop-dropped-file-grants-for-input)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel--normalize-message-text)
                         (lambda (text) text))
                        ((symbol-function 'mevedel-view--inline-skill-prompt-summary-body)
                         (lambda (_text) nil))
                        ((symbol-function 'mevedel--clear-user-turn-gptel-properties)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel-collaboration--safe-accepted-prompt)
                         (lambda (buffer) (setq ordinary (eq buffer data-buffer))))
                        ((symbol-function 'mevedel-view--insert-user-message)
                         (lambda (&rest _) 1))
                        ((symbol-function 'mevedel-view-stream-begin-turn)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel-view--clear-input)
                         (lambda () nil))
                        ((symbol-function 'mevedel-view--activate-dropped-file-grants)
                         (lambda (&rest _) nil))
                        ((symbol-function 'gptel-send) (lambda () nil)))
                (mevedel-view--forward-input-now "ordinary prompt")))
          (with-current-buffer data-buffer
            (cl-letf (((symbol-function 'mevedel-collaboration--safe-accepted-prompt)
                       (lambda (buffer) (setq generated (eq buffer data-buffer))))
                      ((symbol-function 'mevedel-view--begin-external-turn)
                       (lambda (&rest _) nil)))
              (mevedel--insert-local-user-turn "generated prompt")))
          (should ordinary)
          (should generated)
          (with-current-buffer data-buffer
            (should (string-match-p "ordinary prompt" (buffer-string)))
            (should (string-match-p "generated prompt" (buffer-string)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))))


;;
;;; Relay and guest-facing facade integration

(mevedel-deftest mevedel-collaboration--host-headers
  (:doc "omits an unconfigured host token and sends a configured one")
  (let ((mevedel-collaboration-relay-host-token nil))
    (should-not (mevedel-collaboration--host-headers))
    (setq mevedel-collaboration-relay-host-token "")
    (should-not (mevedel-collaboration--host-headers))
    (setq mevedel-collaboration-relay-host-token "s3cret")
    (should (equal '(("X-Mevedel-Host-Token" . "s3cret"))
                   (mevedel-collaboration--host-headers)))))


(mevedel-deftest mevedel-collaboration--publish-queue
  (:doc "broadcasts pending queue state to guests only when it changes")
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "/tmp/collab-queue/" "/tmp/collab-queue/" "cq"))
         (session (mevedel-session-create "main" workspace))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :guests guests
                     :transport 'transport :queue nil))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t)))
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "phone-guest-id")
                   guests)
          (mevedel-collaboration--publish-queue room)
          (should (equal '((1 . (:t "queue" :pending 0 :paused nil)))
                         sent))
          ;; An unchanged queue is not re-sent.
          (setq sent nil)
          (mevedel-collaboration--publish-queue room)
          (should-not sent)
          ;; A queued entry and a pause both move the state.
          (let ((entry (mevedel-session-enqueue-pending-input
                        session 'follow-up (list :input "later"))))
            (mevedel-collaboration--publish-queue room)
            (should (equal '((1 . (:t "queue" :pending 1 :paused nil)))
                           sent))
            ;; The ack tells the sender where in line it landed.
            (should (= 1 (mevedel-collaboration--queue-position room entry)))
            (setq sent nil)
            (mevedel-session-set-pending-input-paused session t)
            (mevedel-collaboration--publish-queue room)
            (should (equal '((1 . (:t "queue" :pending 1 :paused t)))
                           sent))
            ;; An entry that drained between enqueue and ack has no place.
            (mevedel-session-set-pending-inputs session 'follow-up nil)
            (should-not (mevedel-collaboration--queue-position room entry)))
          ;; A guest's own entries ride its queue frame -- id, live
          ;; position, and its own text echoed back -- and only its own:
          ;; another guest's unsent text never travels to it.
          (setq sent nil)
          (mevedel-session-set-pending-input-paused session nil)
          (mevedel-session-enqueue-pending-input
           session 'follow-up (list :input "someone else's"
                                    :guest-id "other-guest-id"))
          (let ((mine (mevedel-session-enqueue-pending-input
                       session 'follow-up
                       (list :input "my question"
                             :guest-id "phone-guest-id"))))
            (mevedel-collaboration--publish-queue room)
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
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-collaboration--publish-status
  (:doc "broadcasts busy transitions once and tells a joining guest directly")
  (let* ((guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-status-data*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport :status nil))
         sent controls)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'mevedel-collaboration--transport-control)
                   (lambda (_transport control)
                     (push control controls)
                     t)))
          (puthash 1 (list :name "g" :writable nil :ready t) guests)
          (with-current-buffer data-buffer
            (setq-local gptel-model 'test-model))
          ;; The first publish establishes the baseline the guests hold.
          (mevedel-collaboration--publish-status room)
          (let ((frame (cdr (car sent))))
            (should (equal "status" (plist-get frame :t)))
            (should (eq :json-false (plist-get frame :busy)))
            ;; The strip reports what the Emacs mode line reports.
            (should (equal "test-model" (plist-get frame :model))))
          ;; Unchanged state is not repeated.
          (setq sent nil)
          (mevedel-collaboration--publish-status room)
          (should-not sent)
          (with-current-buffer data-buffer
            (setq-local mevedel--current-request 'request))
          (mevedel-collaboration--publish-status room)
          (should (eq t (plist-get (cdr (car sent)) :busy)))
          (setq sent nil)
          (with-current-buffer data-buffer
            (setq-local mevedel--current-request nil))
          (mevedel-collaboration--publish-status room)
          (should (eq :json-false (plist-get (cdr (car sent)) :busy)))
          (should (equal '(:t "push") (car controls)))
          ;; Re-publishing the same idle state sends neither status nor push.
          (setq sent nil controls nil)
          (mevedel-collaboration--publish-status room)
          (should-not sent)
          (should-not controls))
      (kill-buffer data-buffer))))

(mevedel-deftest mevedel-view--drain-guest-invocation
  (:doc "dispatches a queued guest invocation and dequeues it"
   :quiet t)
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "invoke"))
          (mevedel-collaboration-guest-skills '("plan"))
          ran)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-session-enqueue-pending-input
       session 'follow-up (list :input "" :guest-invoke "plan"
                               :guest-id "g1" :inert-skills t))
      (cl-letf (((symbol-function 'mevedel-plan-mode-enter)
                 (lambda (&rest _) (setq ran t)))
                ((symbol-function
                  'mevedel-session-artifacts-assert-new-mutation-authority)
                 (lambda (&rest _) nil)))
        (mevedel-view--drain-follow-up data-buf))
      ;; The command ran, and the entry it came from is gone: a local
      ;; command inserts no turn, so nothing else would ever clear it.
      (should ran)
      (should-not (mevedel-session-pending-follow-ups session)))))

(mevedel-deftest mevedel-collaboration-notify-queue-changed
  (:doc "re-publishes the queue when it changes without a request")
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "/tmp/collab-notify/" "/tmp/collab-notify/" "cn"))
         (session (mevedel-session-create "main" workspace))
         (guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-notify-data*"))
         (room (list :session session :guests guests
                     :data-buffer data-buffer :transport 'transport
                     :queue nil :status nil))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t)))
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "g1")
                   guests)
          ;; A local command drains its entry without starting a turn,
          ;; so nothing else would tell the guest its card is stale.
          (mevedel-session-enqueue-pending-input
           session 'follow-up (list :input "" :guest-invoke "plan"))
          (mevedel-collaboration-notify-queue-changed session)
          (should (cl-find-if (lambda (entry)
                                (equal "queue" (plist-get (cdr entry) :t)))
                              sent))
          (setq sent nil)
          (mevedel-session-set-pending-inputs session 'follow-up nil)
          (mevedel-collaboration-notify-queue-changed session)
          (let ((queue (cl-find-if
                        (lambda (entry)
                          (equal "queue" (plist-get (cdr entry) :t)))
                        sent)))
            (should queue)
            (should (= 0 (plist-get (cdr queue) :pending)))
            (should-not (plist-get (cdr queue) :own)))
          ;; An unshared session is simply not a room.
          (should-not (mevedel-collaboration-notify-queue-changed nil)))
      (kill-buffer data-buffer)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-view--drop-disallowed-guest-skills
  (:doc "drops queued guest invocations the allowlist no longer names"
   :quiet t)
  (let ((session (mevedel-session--create :name "s"))
        (mevedel-collaboration-guest-skills '("plan")))
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "" :guest-invoke "plan"))
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "" :guest-invoke "review"))
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "plain question"))
    (should (equal '("" "plain question")
                   (mapcar (lambda (entry) (plist-get entry :input))
                           (mevedel-view--drop-disallowed-guest-skills
                            session))))
    ;; Nothing to drop leaves the queue untouched and quiet.
    (should (= 2 (length (mevedel-view--drop-disallowed-guest-skills
                          session))))))


;;
;;; Room lifecycle

(mevedel-deftest mevedel-collaboration--start
  (:doc "builds the room, both bearer links, and the TTL timer")
  (with-temp-buffer
    (let ((mevedel-collaboration-relay-url "ws://127.0.0.1:1")
          (mevedel-collaboration-relay-host-token "test-token")
          (mevedel-collaboration-share-ttl 60)
          (mevedel-collaboration--rooms (make-hash-table :test #'eq))
          (session (mevedel-session--create :name "share"))
          dialed room)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mevedel-collaboration--transport-open)
                       (lambda (url _key &rest _) (setq dialed url) 'transport))
                      ((symbol-function 'mevedel-collaboration--canonical-records)
                       (lambda (_) nil)))
              (setq room (mevedel-collaboration--start session
                                                        (current-buffer))))
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
                       (plist-get room :link-full))))
            ;; The full secret embeds the write token after the room key.
            (let* ((view-secret (car (last (split-string
                                            (plist-get room :link-view)
                                            "\\."))))
                   (full-secret (car (last (split-string
                                            (plist-get room :link-full)
                                            "\\.")))))
              (should (equal (mevedel-collaboration--base64url-decode
                              view-secret)
                             (plist-get room :key)))
              (should (equal (mevedel-collaboration--base64url-decode
                              full-secret)
                             (concat (plist-get room :key)
                                     (plist-get room :write-token)))))
            (should (timerp (plist-get room :ttl-timer)))
            ;; Restarting for the same session reuses the room.
            (should (eq room (mevedel-collaboration--start
                              session (current-buffer)))))
        (when-let* ((timer (plist-get room :ttl-timer)))
          (cancel-timer timer))
        (remove-hook 'kill-emacs-hook
                     #'mevedel-collaboration--stop-for-emacs)
        (remove-hook 'mevedel-interaction-prompt-created-hook
                     #'mevedel-collaboration--on-prompt-created)
        (remove-hook 'mevedel-interaction-prompt-settled-hook
                     #'mevedel-collaboration--on-prompt-settled)))))

(mevedel-deftest mevedel-collaboration--multi-room
  (:doc "gives each shared session its own independent room")
  (let ((data-a (generate-new-buffer " *collab-multi-a*"))
        (data-b (generate-new-buffer " *collab-multi-b*"))
        (mevedel-collaboration-relay-url "ws://127.0.0.1:1")
        (mevedel-collaboration-relay-host-token "test-token")
        (mevedel-collaboration-share-ttl nil)
        (mevedel-collaboration--rooms (make-hash-table :test #'eq))
        (session-a (mevedel-session--create :name "a"))
        (session-b (mevedel-session--create :name "b"))
        stopped-transports room-a room-b)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-open)
                   (lambda (url &rest _) (list :url url)))
                  ((symbol-function 'mevedel-collaboration--transport-stop)
                   (lambda (transport) (push transport stopped-transports)))
                  ((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (&rest _) t))
                  ((symbol-function 'mevedel-collaboration--canonical-records)
                   (lambda (_) nil)))
          (setq room-a (mevedel-collaboration--start session-a data-a)
                room-b (mevedel-collaboration--start session-b data-b))
          ;; Two live rooms with distinct credentials and links.
          (should (= 2 (length (mevedel-collaboration--room-list))))
          (should-not (eq room-a room-b))
          (should-not (equal (plist-get room-a :key)
                             (plist-get room-b :key)))
          (should-not (equal (plist-get room-a :link-view)
                             (plist-get room-b :link-view)))
          ;; A frame for one data buffer reaches only that room.
          (let (handled)
            (cl-letf (((symbol-function 'mevedel-collaboration--handle-hello)
                       (lambda (room _peer _frame) (push room handled))))
              (mevedel-collaboration--on-frame data-a 1
                                               (list :t "hello" :proto 2)))
            (should (equal (list room-a) handled)))
          ;; Stopping one room leaves the other live and untouched.
          (mevedel-collaboration--stop-internal room-a 'user-stop)
          (should (equal (list room-b)
                         (mevedel-collaboration--room-list)))
          (should (equal (list (plist-get room-a :transport))
                         stopped-transports))
          (mevedel-collaboration--stop-internal room-b 'user-stop)
          (should-not (mevedel-collaboration--room-list)))
      (kill-buffer data-a)
      (kill-buffer data-b)
      (remove-hook 'kill-emacs-hook
                   #'mevedel-collaboration--stop-for-emacs)
      (remove-hook 'mevedel-interaction-prompt-created-hook
                   #'mevedel-collaboration--on-prompt-created)
      (remove-hook 'mevedel-interaction-prompt-settled-hook
                   #'mevedel-collaboration--on-prompt-settled))))

(mevedel-deftest mevedel-collaboration--stop-internal
  (:doc "finishes core teardown when share presentation cleanup signals")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (cancelled nil)
           (sent nil)
           (transport-stopped nil)
           (room (list :transport 'transport
                       :data-buffer (current-buffer)
                       :guests guests
                       :publish-timer 'publish-timer
                       :ttl-timer 'ttl-timer)))
      (puthash 1 (list :name "g") guests)
      (let ((mevedel-collaboration--rooms
             (mevedel-test-room-registry room)))
        (cl-letf
            (((symbol-function 'cancel-timer)
              (lambda (timer) (push timer cancelled)))
             ((symbol-function 'mevedel-collaboration-share-dismiss)
              (lambda (_room) (error "Injected share cleanup failure")))
             ((symbol-function 'mevedel-collaboration--transport-send)
              (lambda (_transport peer frame)
                (push (cons peer frame) sent)
                t))
             ((symbol-function 'mevedel-collaboration--transport-stop)
              (lambda (_transport) (setq transport-stopped t))))
          (mevedel-collaboration--stop-internal room 'user-stop))
        (should-not (mevedel-collaboration--room-list)))
      (should (equal '(publish-timer ttl-timer) (nreverse cancelled)))
      (should (equal "bye" (plist-get (cdr (car sent)) :t)))
      (should transport-stopped))))

(mevedel-deftest mevedel-collaboration--lifecycle-hooks
  (:doc "stops on data-buffer, session, TTL, and Emacs lifecycle teardown")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           reasons)
      (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (push reason reasons)))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (mevedel-collaboration--stop-for-buffer)
        (mevedel-collaboration--stop-for-session)
        (mevedel-collaboration--stop-for-ttl (current-buffer))
        (mevedel-collaboration--stop-for-emacs))
      (should (equal '(emacs-exit ttl-expired data-buffer-killed
                                  data-buffer-killed)
                     reasons)))))

(mevedel-deftest mevedel-collaboration--stop-for-session
  (:doc "stops the room when its owning data buffer ends the session")
  (with-temp-buffer
    (let* ((stopped nil)
           (room (list :data-buffer (current-buffer)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
      (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason))))
        (mevedel-collaboration--stop-for-session)
        (should (eq 'data-buffer-killed stopped))))))

;;; test-mevedel-collaboration.el ends here
