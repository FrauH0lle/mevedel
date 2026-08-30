;;; test-mevedel-collaboration.el --- Collaboration facade and projection tests -*- lexical-binding: t; -*-

;;; Commentary:

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
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-pending-inputs)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-chat)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-input-files)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-ui)

(defun mevedel-collab-test--rooms (&rest rooms)
  "Return a room registry holding ROOMS, keyed by their data buffers."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (room rooms)
      (puthash (or (plist-get room :data-buffer) (make-symbol "room"))
               room table))
    table))


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
                      :permission "secret" :execution-target "/target")))
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
    (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
               (lambda (_buffer _start _end)
                 '(:name "Bash" :result ""))))
      (let ((record (mevedel-collaboration--tool-record
                     (current-buffer) '(tool 1 5))))
        (should (equal "completed" (plist-get record :status)))
        (should (equal "" (plist-get record :result)))))
    ;; Leading spaces on the first result line are significant alignment
    ;; (Read's right-aligned line numbers); only newlines are trimmed.
    (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
               (lambda (_buffer _start _end)
                 '(:name "Read" :args (:file_path "/tmp/a.el")
                   :result "\n  1\t;;; header\n  2\tbody\n"))))
      (let ((record (mevedel-collaboration--tool-record
                     (current-buffer) '(tool 1 5))))
        (should (string-prefix-p "  1\t" (plist-get record :result)))
        (should (equal "/tmp/a.el" (plist-get record :detail)))))
    ;; ApplyPatch carries the authored patch as a dedicated diff field.
    (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
               (lambda (_buffer _start _end)
                 '(:name "ApplyPatch"
                   :args (:patch "@@ -1 +1 @@\n-old\n+new")
                   :result "Applied patch: 1 changes"))))
      (let ((record (mevedel-collaboration--tool-record
                     (current-buffer) '(tool 1 5))))
        (should (equal "@@ -1 +1 @@\n-old\n+new" (plist-get record :diff)))
        (should (equal "@@ -1 +1 @@\n-old\n+new"
                       (cdr (assoc "diff"
                                   (mevedel-collaboration--json-record
                                    record)))))))))

(mevedel-deftest mevedel-collaboration--pre-tool
  (:doc "publishes one stable running tool record before settled completion")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :records nil :pending-tools nil
                       :tool-call-occurrences (make-hash-table :test #'equal)
                       :guests (make-hash-table :test #'eql)))
           (info '(:name "Bash" :args (:command "true")))
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room))
           canonical)
      (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
                 (lambda (_) canonical)))
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
          (should-not (plist-get room :pending-tools)))))))

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
    (should (equal '(0 0) (mapcar #'car sent)))
    (should (equal '("record" "record")
                   (mapcar (lambda (entry) (plist-get (cdr entry) :t)) sent)))
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
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room))
           published stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                 (lambda (_room) (setq published t))))
        (should-not (mevedel-collaboration--safe-accepted-prompt
                     (current-buffer)))
        (should published))
      (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                 (lambda (_) (error "viewer failed")))
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
;;; Snapshot delivery

(defun test-mevedel-collaboration--chunk-frame-bytes (chunk)
  "Return the encoded size of the snapshot frame carrying CHUNK."
  (string-bytes
   (mevedel-collaboration--json-string
    (list :t "snapshot-chunk" :records (vconcat chunk) :final t))))

(mevedel-deftest mevedel-collaboration--snapshot-chunks ()
  ,test
  (test)
  :doc "splits records into chunks each under the wire bound"
  (progn
    (should-not (mevedel-collaboration--snapshot-chunks nil))
    (let* ((record (list :id "assistant-x" :kind "assistant" :revision 0
                         :text (make-string 100000 937)))
           (chunks (mevedel-collaboration--snapshot-chunks
                    (make-list 12 record))))
      (should (> (length chunks) 1))
      (should (= 12 (apply #'+ (mapcar #'length chunks))))
      (dolist (chunk chunks)
        (should (<= (test-mevedel-collaboration--chunk-frame-bytes chunk)
                    mevedel-collaboration--max-message-bytes)))))

  :doc "bounds the frame it sends, separators included"
  ;; Enough tiny records that the separators alone carry a chunk over the
  ;; bound: summing record sizes admits about 21,400 of these, whose frame
  ;; is some 20 KiB larger than the sum says.
  (let* ((record (list :id "t" :kind "user" :revision 0 :text "hi"))
         (chunks (mevedel-collaboration--snapshot-chunks
                  (make-list 30000 record))))
    (should (= 30000 (apply #'+ (mapcar #'length chunks))))
    (dolist (chunk chunks)
      (should (<= (test-mevedel-collaboration--chunk-frame-bytes chunk)
                  mevedel-collaboration--max-message-bytes))))

  :doc "keeps a worst-case escaped record inside one frame"
  ;; The raw-text bound reserves room for escaping plus the record's own
  ;; keys, so a maximal record of six-fold expanding characters must still
  ;; travel rather than be dropped.
  (let* ((record (list :id "esc" :kind "assistant" :revision 0
                       :text (make-string
                              mevedel-collaboration--max-record-text-bytes
                              ?\C-a)))
         (chunks (mevedel-collaboration--snapshot-chunks (list record))))
    (should (= 1 (apply #'+ (mapcar #'length chunks))))
    (should (<= (test-mevedel-collaboration--chunk-frame-bytes (car chunks))
                mevedel-collaboration--max-message-bytes)))

  :doc "drops a record too large to travel in a frame of its own"
  (let* ((oversized (list :id "huge" :kind "assistant" :revision 0
                          :text (make-string
                                 mevedel-collaboration--max-message-bytes
                                 ?x)))
         (small (list :id "small" :kind "user" :revision 0 :text "hi"))
         (chunks (mevedel-collaboration--snapshot-chunks
                  (list small oversized small))))
    (should (= 2 (apply #'+ (mapcar #'length chunks))))
    (dolist (chunk chunks)
      (should (<= (test-mevedel-collaboration--chunk-frame-bytes chunk)
                  mevedel-collaboration--max-message-bytes)))))

(mevedel-deftest mevedel-collaboration--send-snapshot
  (:doc "sends a targeted welcome then final-flagged snapshot chunks")
  (let* ((guests (make-hash-table :test #'eql))
         (records (list (list :id "u" :kind "user" :revision 0 :text "hi")))
         (room (list :transport 'transport :guests guests :records records))
         sent)
    (puthash 7 (list :name "g" :writable t :ready t) guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (mevedel-collaboration--send-snapshot room 7))
    (setq sent (nreverse sent))
    (should (equal '(7 7) (mapcar #'car sent)))
    (let ((welcome (cdr (nth 0 sent)))
          (chunk (cdr (nth 1 sent))))
      (should (equal "welcome" (plist-get welcome :t)))
      (should (eq :json-false (plist-get welcome :readOnly)))
      (should (= 1 (plist-get welcome :recordCount)))
      (should (equal "snapshot-chunk" (plist-get chunk :t)))
      (should (eq t (plist-get chunk :final)))
      (should (= 1 (length (plist-get chunk :records)))))
    ;; The invocation roster rides the welcome for writable guests only.
    (let ((mevedel-collaboration-guest-skills '("plan")))
      (setq sent nil)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent)
                   t))
                ((symbol-function 'mevedel-collaboration--guest-roster)
                 (lambda (_room) '((("name" . "plan") ("kind" . "command"))))))
        (mevedel-collaboration--send-snapshot room 7)
        (should (equal "plan"
                       (cdr (assoc "name"
                                   (aref (plist-get (cdr (car (last sent)))
                                                    :commands)
                                         0)))))
        (puthash 8 (list :name "viewer" :writable nil :ready t) guests)
        (setq sent nil)
        (mevedel-collaboration--send-snapshot room 8)
        (should-not (plist-member (cdr (car (last sent))) :commands))))))

;;
;;; Inbound guest frames

(mevedel-deftest mevedel-collaboration--handle-hello
  (:doc "rejects a protocol mismatch and classifies write-token possession")
  (let* ((guests (make-hash-table :test #'eql))
         (token (mevedel-collaboration--random-bytes 16))
         (room (list :transport 'transport :guests guests
                     :write-token token :records nil
                     :ui-requests (make-hash-table :test #'eql)))
         sent)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (mevedel-collaboration--handle-hello room 3 (list :proto 999 :name "x"))
      (should (equal "error" (plist-get (cdr (car sent)) :t)))
      (should (= 0 (hash-table-count guests)))
      (setq sent nil)
      (mevedel-collaboration--handle-hello
       room 1 (list :proto mevedel-collaboration--protocol-version
                    :name "Phone"
                    :guestId "phone-guest-id"
                    :writeToken (mevedel-collaboration--base64url token)))
      (mevedel-collaboration--handle-hello
       room 2 (list :proto mevedel-collaboration--protocol-version
                    :name "Laptop\n<evil>"
                    :guestId "in valid!"
                    :writeToken (mevedel-collaboration--base64url
                                 (mevedel-collaboration--random-bytes 16))))
      (let ((writer (gethash 1 guests))
            (viewer (gethash 2 guests)))
        (should (plist-get writer :writable))
        (should (equal "Phone" (plist-get writer :name)))
        ;; The stable guest identity survives from hello; a malformed
        ;; one is dropped rather than stored.
        (should (equal "phone-guest-id" (plist-get writer :guest-id)))
        (should-not (plist-get viewer :guest-id))
        (should-not (plist-get viewer :writable))
        (should (equal "Laptop <evil>" (plist-get viewer :name))))
      (let ((welcomes (cl-remove-if-not
                       (lambda (entry)
                         (equal "welcome" (plist-get (cdr entry) :t)))
                       sent)))
        (should (= 2 (length welcomes)))))))

(mevedel-deftest mevedel-collaboration--handle-prompt--media-cleanup
  (:doc "leaves no saved image behind when the prompt is not queued")
  (let* ((guests (make-hash-table :test #'eql))
         (root (file-name-as-directory
                (make-temp-file "mevedel-guest-media-" t)))
         (data-buffer (generate-new-buffer " *collab-media-data*"))
         (view-buffer (generate-new-buffer " *collab-media-view*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport))
         (image (list :mime "image/png"
                      :data (base64-encode-string "\211PNG\r\n" t))))
    (unwind-protect
        (progn
          (puthash 1 (list :name "Phone" :writable t :ready t) guests)
          (with-current-buffer data-buffer
            ;; A live view but no session: the real enqueue refuses, which
            ;; is the path that used to leave the media behind.
            (setq-local mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--media-dir)
                     (lambda () root))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (&rest _) t)))
            (mevedel-collaboration--handle-prompt
             room 1 (list :text "look at this" :images (list image))))
          (should-not (directory-files root nil "\\`guest-"))
          ;; The duplicate latch must not have swallowed the retry either,
          ;; since nothing was actually queued.
          (should-not (plist-get (gethash 1 guests) :last-prompt)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--handle-prompt--media-failure
  (:doc "keeps the room when a guest image cannot be written")
  (let* ((guests (make-hash-table :test #'eql))
         (root (file-name-as-directory
                (make-temp-file "mevedel-guest-media-fail-" t)))
         (data-buffer (generate-new-buffer " *collab-fail-data*"))
         (view-buffer (generate-new-buffer " *collab-fail-view*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport))
         (image (list :mime "image/png"
                      :data (base64-encode-string "PNGDATA" t)))
         (writes 0)
         diagnostics)
    (unwind-protect
        (progn
          (puthash 1 (list :name "Phone" :writable t :ready t) guests)
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer)
            (setq-local mevedel--session
                        (mevedel-session--create :name "share")))
          ;; The suite preloads the handler's owners before `write-region'
          ;; is stubbed.
          (mevedel-test--with-captured-diagnostics diagnostics
            (cl-letf* (((symbol-function 'mevedel-view--media-dir)
                        (lambda () root))
                       (real (symbol-function 'write-region))
                       ((symbol-function 'write-region)
                        (lambda (&rest args)
                          ;; The set is attached whole or not at all, so the
                          ;; first image must not survive the second's
                          ;; failure.
                          (if (= (cl-incf writes) 1)
                              (apply real args)
                            (error "Disk full"))))
                       ((symbol-function
                         'mevedel-collaboration--transport-send)
                        (lambda (&rest _) t)))
              ;; A failed write must not reach the frame handler, which
              ;; treats an error as an observer failure and stops the room.
              (mevedel-collaboration--handle-prompt
               room 1 (list :text "look" :images (list image image)))))
          (should (string-match-p "could not be saved" diagnostics))
          (should (= 2 writes))
          (should-not (directory-files root nil "\\`guest-"))
          (should-not (plist-get (gethash 1 guests) :last-prompt)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--handle-prompt
  (:doc "queues a writable guest prompt and ignores unauthorized or invalid ones")
  (let* ((guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-prompt-data*"))
         (view-buffer (generate-new-buffer " *collab-prompt-view*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport))
         enqueued rebuilt drained sent granted)
    (unwind-protect
        (progn
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "phone-guest-id")
                   guests)
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer)
            ;; A real struct: the turn-count accessor is a defsubst that
            ;; load order may have inlined beyond a stub's reach.
            (setq-local mevedel--session
                        (mevedel-session--create :name "share")))
          (cl-letf (((symbol-function 'mevedel-session-enqueue-pending-input)
                     (lambda (_session category entry)
                       (setq enqueued (cons category entry))
                       (plist-put (copy-sequence entry) :id 7)))
                    ((symbol-function 'mevedel--normalize-message-text)
                     (lambda (text) (string-trim text)))
                    ((symbol-function 'mevedel-view--interaction-rebuild)
                     (lambda () (setq rebuilt t)))
                    ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
                     (lambda () (setq drained t)))
                    ((symbol-function 'mevedel-view--pop-dropped-file-grants-for-input)
                     (lambda (_input _session) '(grant)))
                    ((symbol-function 'mevedel-session-add-dropped-file-grant)
                     (lambda (_session path) (push path granted)))
                    ((symbol-function 'mevedel-collaboration--save-guest-attachments)
                     (lambda (images)
                       (when images '("/tmp/media/guest-1.jpg"))))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent)
                       t)))
            (mevedel-collaboration--handle-prompt
             room 1 (list :t "prompt" :text " check the tests \n"
                          :name "DonHugo\n!"
                          :images (list (list :mime "image/jpeg"
                                              :data "ignored")))))
          (should (equal 'follow-up (car enqueued)))
          ;; Attached images become @file mentions with read grants.
          (should (equal "check the tests @file:/tmp/media/guest-1.jpg"
                         (plist-get (cdr enqueued) :input)))
          (should (equal '("/tmp/media/guest-1.jpg") granted))
          (should (equal '(grant)
                         (plist-get (cdr enqueued) :dropped-file-grants)))
          ;; The prompt frame's name refreshes the hello-time default.
          (should (equal "DonHugo !" (plist-get (cdr enqueued) :guest-name)))
          (should (equal "DonHugo !" (plist-get (gethash 1 guests) :name)))
          (should (numberp (plist-get (cdr enqueued) :queued-at-turn)))
          ;; The entry remembers who queued it and which files it owns,
          ;; which is what the retract seam checks against.
          (should (equal "phone-guest-id"
                         (plist-get (cdr enqueued) :guest-id)))
          (should (equal '("/tmp/media/guest-1.jpg")
                         (plist-get (cdr enqueued) :guest-paths)))
          ;; The guest gets a targeted queued acknowledgement carrying
          ;; the entry id its retract control needs.
          (should (equal '(1 . (:t "queued" :id 7)) (car sent)))
          (should rebuilt)
          (should drained)
          ;; A byte-identical repeat within the duplicate window is a
          ;; double-fired client submit and is dropped.
          (setq enqueued nil sent nil)
          (cl-letf (((symbol-function 'mevedel-session-enqueue-pending-input)
                     (lambda (_session category entry)
                       (setq enqueued (cons category entry))))
                    ((symbol-function 'mevedel--normalize-message-text)
                     (lambda (text) (string-trim text)))
                    ((symbol-function 'mevedel-view--interaction-rebuild)
                     (lambda () nil))
                    ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
                     (lambda () nil))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (&rest _) t)))
            (mevedel-collaboration--handle-prompt
             room 1 (list :t "prompt" :text " check the tests \n"))
            (should-not enqueued)
            ;; Different text inside the window still goes through.
            (mevedel-collaboration--handle-prompt
             room 1 (list :t "prompt" :text "another question"))
            (should (equal "another question"
                           (plist-get (cdr enqueued) :input))))
          ;; Unauthorized and invalid prompts never reach the queue.
          (let ((rejects nil))
            (puthash 8 (list :name "Viewer" :writable nil :ready t) guests)
            (cl-letf (((symbol-function 'mevedel-session-enqueue-pending-input)
                       (lambda (&rest args) (setq rejects args))))
              ;; Read-only guest.
              (mevedel-collaboration--handle-prompt
               room 8 (list :t "prompt" :text "denied"))
              ;; Unregistered peer.
              (mevedel-collaboration--handle-prompt
               room 9 (list :t "prompt" :text "denied"))
              ;; Oversized, blank, and non-string prompts from a writable
              ;; guest.
              (mevedel-collaboration--handle-prompt
               room 1 (list :t "prompt"
                            :text (make-string
                                   (1+ mevedel-collaboration--max-prompt-bytes)
                                   ?x)))
              (mevedel-collaboration--handle-prompt
               room 1 (list :t "prompt" :text "   "))
              (mevedel-collaboration--handle-prompt
               room 1 (list :t "prompt" :text 42)))
            (should-not rejects)))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer))))

(mevedel-deftest mevedel-collaboration--host-headers
  (:doc "sends the relay host token as a header only when one is configured")
  (let ((mevedel-collaboration-relay-host-token nil))
    (should-not (mevedel-collaboration--host-headers))
    (setq mevedel-collaboration-relay-host-token "")
    (should-not (mevedel-collaboration--host-headers))
    (setq mevedel-collaboration-relay-host-token "s3cret")
    (should (equal '(("X-Mevedel-Host-Token" . "s3cret"))
                   (mevedel-collaboration--host-headers)))))

(mevedel-deftest mevedel-collaboration--guest-directive-id
  (:doc "accepts an id the workspace still has and drops anything else")
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "/tmp/collab-dir/" "/tmp/collab-dir/" "collab"))
         (session (mevedel-session-create "main" workspace))
         (room (list :session session)))
    (unwind-protect
        (progn
          (setf (mevedel-workspace-directives workspace)
                (list (mevedel-directive--create :id "dir-1")))
          (should (equal "dir-1"
                         (mevedel-collaboration--guest-directive-id
                          room (list :directive "dir-1"))))
          ;; A stale filter, a non-string, and no filter at all all send
          ;; to main chat rather than failing the prompt.
          (should-not (mevedel-collaboration--guest-directive-id
                       room (list :directive "dir-gone")))
          (should-not (mevedel-collaboration--guest-directive-id
                       room (list :directive 42)))
          (should-not (mevedel-collaboration--guest-directive-id
                       room (list :text "no filter")))
          ;; A room with no session cannot resolve anything.
          (should-not (mevedel-collaboration--guest-directive-id
                       (list :session nil) (list :directive "dir-1"))))
      (mevedel-workspace-clear-registry))))

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

(mevedel-deftest mevedel-collaboration--handle-retract
  (:doc "removes only the sender's own pending entry with its attachments")
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "/tmp/collab-retract/" "/tmp/collab-retract/"
                     "cr"))
         (session (mevedel-session-create "main" workspace))
         (guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-retract-data*"))
         (view-buffer (generate-new-buffer " *collab-retract-view*"))
         (media (make-temp-file "mevedel-retract-media-" nil ".jpg" "img"))
         (room (list :session session :guests guests
                     :data-buffer data-buffer :transport 'transport))
         rebuilt)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer))
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "phone-guest-id")
                   guests)
          (puthash 2 (list :name "Tablet" :writable t :ready t
                           :guest-id "tablet-guest-id")
                   guests)
          (let ((mine (mevedel-session-enqueue-pending-input
                       session 'follow-up
                       (list :input "mine" :guest-id "phone-guest-id"
                             :guest-paths (list media))))
                (theirs (mevedel-session-enqueue-pending-input
                         session 'follow-up
                         (list :input "theirs"
                               :guest-id "tablet-guest-id"))))
            (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                       (lambda () (setq rebuilt t)))
                      ((symbol-function
                        'mevedel-collaboration--transport-send)
                       (lambda (&rest _) t)))
              ;; Another guest cannot retract it, nor can a bogus id.
              (mevedel-collaboration--handle-retract
               room 2 (list :id (plist-get mine :id)))
              (mevedel-collaboration--handle-retract
               room 1 (list :id 999))
              (should (= 2 (length
                            (mevedel-session-pending-follow-ups session))))
              (should (file-exists-p media))
              ;; An entry the drain is delivering right now is no longer
              ;; retractable: its files are about to be read mid-turn.
              (plist-put mine :delivering t)
              (mevedel-collaboration--handle-retract
               room 1 (list :id (plist-get mine :id)))
              (should (= 2 (length
                            (mevedel-session-pending-follow-ups session))))
              (should (file-exists-p media))
              (plist-put mine :delivering nil)
              ;; The owner can, and the attachment leaves with it.
              (mevedel-collaboration--handle-retract
               room 1 (list :id (plist-get mine :id)))
              (should (equal (list theirs)
                             (mevedel-session-pending-follow-ups session)))
              (should-not (file-exists-p media))
              (should rebuilt))))
      (when (file-exists-p media) (delete-file media))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-collaboration--publish-status
  (:doc "broadcasts busy transitions once and tells a joining guest directly")
  (let* ((guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-status-data*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport :busy nil))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t)))
          (puthash 1 (list :name "g" :writable nil :ready t) guests)
          ;; Idle at first: no transition, nothing broadcast.
          (mevedel-collaboration--publish-status room)
          (should-not sent)
          (with-current-buffer data-buffer
            (setq-local mevedel--current-request 'request))
          (mevedel-collaboration--publish-status room)
          (should (equal '((0 . (:t "status" :busy t))) sent))
          ;; Unchanged state is not repeated.
          (setq sent nil)
          (mevedel-collaboration--publish-status room)
          (should-not sent)
          (with-current-buffer data-buffer
            (setq-local mevedel--current-request nil))
          (mevedel-collaboration--publish-status room)
          (should (equal '((0 . (:t "status" :busy :json-false))) sent)))
      (kill-buffer data-buffer))))

(mevedel-deftest mevedel-collaboration--save-guest-attachments
  (:doc "saves accepted types within budget and drops invalid sets whole")
  (let ((dir (make-temp-file "mevedel-guest-images" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--media-dir)
                   (lambda () dir)))
          (let ((paths (mevedel-collaboration--save-guest-attachments
                        (list (list :mime "image/jpeg"
                                    :data (base64-encode-string "jpegbytes"))
                              (list :mime "image/png"
                                    :data (base64-encode-string "pngbytes"))))))
            (should (= 2 (length paths)))
            (should (string-suffix-p ".jpg" (nth 0 paths)))
            (should (string-suffix-p ".png" (nth 1 paths)))
            (should (equal "jpegbytes"
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally (nth 0 paths))
                             (buffer-string)))))
          ;; A text attachment rides the same path; Read decides text
          ;; or media from the extension downstream.
          (let ((paths (mevedel-collaboration--save-guest-attachments
                        (list (list :mime "text/x-patch"
                                    :data (base64-encode-string "--- a\n"))))))
            (should (= 1 (length paths)))
            (should (string-suffix-p ".patch" (car paths))))
          ;; Unknown type, malformed data, over-budget, and too many
          ;; attachments each drop the whole set.
          (should-not (mevedel-collaboration--save-guest-attachments
                       (list (list :mime "image/svg+xml"
                                   :data (base64-encode-string "x")))))
          (should-not (mevedel-collaboration--save-guest-attachments
                       (list (list :mime "image/png" :data "not base64!"))))
          (should-not (mevedel-collaboration--save-guest-attachments
                       (list (list :mime "image/png"
                                   :data (base64-encode-string
                                          (make-string
                                           (1+ mevedel-collaboration--max-attachment-bytes)
                                           ?x))))))
          (should-not (mevedel-collaboration--save-guest-attachments
                       (make-list
                        (1+ mevedel-collaboration--max-prompt-attachments)
                        (list :mime "image/png"
                              :data (base64-encode-string "x")))))
          (should-not (mevedel-collaboration--save-guest-attachments nil)))
      (delete-directory dir t))))

(mevedel-deftest mevedel-collaboration--guest-invocable-p
  (:doc "accepts allowlisted names and refuses unsafe ones outright")
  (let ((mevedel-collaboration-guest-skills '("plan" "review" "mode")))
    (should (mevedel-collaboration--guest-invocable-p "plan"))
    (should (mevedel-collaboration--guest-invocable-p "review"))
    ;; Listed but unsafe: `mode' reaches full-auto, so the deny-set wins
    ;; over a mistaken allowlist entry.
    (should-not (mevedel-collaboration--guest-invocable-p "mode"))
    (should-not (mevedel-collaboration--guest-invocable-p "compact"))
    (should-not (mevedel-collaboration--guest-invocable-p nil))))

(mevedel-deftest mevedel-collaboration--handle-prompt--invoke
  (:doc "queues an allowlisted invocation by name and refuses anything else")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests
                       :transport 'transport :session 'session))
           (mevedel-collaboration-guest-skills '("plan" "mode"))
           enqueued sent)
      (puthash 1 (list :name "Phone" :writable t :ready t
                       :guest-id "phone-guest-id")
               guests)
      (puthash 2 (list :name "Viewer" :writable nil :ready t) guests)
      (setq-local mevedel--view-buffer (current-buffer))
      (cl-letf (((symbol-function 'mevedel-view-enqueue-external-follow-up)
                 (lambda (_data-buffer text &rest keys)
                   (setq enqueued (cons text keys))
                   (list :id 9 :input text)))
                ((symbol-function 'mevedel-collaboration--queue-position)
                 (lambda (&rest _) 1))
                ((symbol-function 'mevedel-collaboration--publish-queue)
                 (lambda (_room) nil))
                ((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent)
                   t)))
        ;; Unlisted, unsafe-listed, and read-only guests are all refused;
        ;; free text is never scanned for a sigil.
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "compact" :text ""))
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "mode" :text "full-auto"))
        (mevedel-collaboration--handle-prompt
         room 2 (list :invoke "plan" :text ""))
        (should-not enqueued)
        ;; An allowlisted invocation queues by name, with the text as its
        ;; arguments and skill planning left inert.
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text "add a retry cap"))
        (should (equal "add a retry cap" (car enqueued)))
        (should (equal "plan" (plist-get (cdr enqueued) :invoke)))
        (should (equal "phone-guest-id"
                       (plist-get (cdr enqueued) :guest-id)))
        (should (equal '(1 . (:t "queued" :id 9 :position 1)) (car sent)))
        ;; A bare invocation carries no arguments at all.
        (setq enqueued nil)
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text ""))
        (should (equal "" (car enqueued)))
        ;; A double-fired tap inside the duplicate window is one send.
        (setq enqueued nil)
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text ""))
        (should-not enqueued)))))

(mevedel-deftest mevedel-collaboration--guest-roster
  (:doc "describes each allowlisted name with its namespace and hint")
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create :name "roster"))
           (room (list :data-buffer data-buf :session session))
           ;; `remember' is a skill and only a skill; `review' is
           ;; installed into the command alist at runtime by its own
           ;; module, so it would resolve as a command here.
           (mevedel-collaboration-guest-skills
            '("plan" "remember" "mode" "nonexistent")))
      (with-current-buffer data-buf
        (setq-local mevedel--view-buffer view-buf))
      (cl-letf (((symbol-function 'mevedel-skills-user-visible-skills)
                 (lambda (&rest _)
                   (list (mevedel-skill--create
                          :name "remember" :argument-hint "[focus]")))))
        (let ((roster (mevedel-collaboration--guest-roster room)))
          ;; A local command and a skill are both offered, each tagged
          ;; with the namespace the viewer needs to render its sigil.
          (should (equal '("plan" "remember")
                         (mapcar (lambda (e) (cdr (assoc "name" e))) roster)))
          (should (equal '("command" "skill")
                         (mapcar (lambda (e) (cdr (assoc "kind" e))) roster)))
          (should (equal "[prompt]" (cdr (assoc "hint" (nth 0 roster)))))
          (should (equal "[focus]" (cdr (assoc "hint" (nth 1 roster)))))
          ;; Unsafe and unresolvable names never become buttons.
          (should-not (assoc "mode" roster))
          (should-not (assoc "nonexistent" roster)))))))

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

(mevedel-deftest mevedel-collaboration--handle-abort ()
  ,test
  (test)
  :doc "aborts for a writable guest and ignores read-only guests"
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests))
           aborted)
      (puthash 1 (list :name "Viewer" :writable nil :ready t) guests)
      (puthash 2 (list :name "Writer" :writable t :ready t) guests)
      (cl-letf (((symbol-function 'mevedel-view--abort-data-buffer)
                 (lambda (buffer) (setq aborted buffer))))
        (mevedel-collaboration--handle-abort room 1)
        (should-not aborted)
        (mevedel-collaboration--handle-abort room 2)
        (should (eq (current-buffer) aborted)))))

  :doc "guest abort cancels a slow prompt hook before request dispatch"
  (mevedel-view-test--with-buffers
    (let* ((guests (make-hash-table :test #'eql))
           (session (mevedel-session--create :name "share"))
           (room (list :data-buffer data-buf :guests guests))
           late-callback submission send-called)
      (puthash 1 (list :name "Writer" :writable t :ready t) guests)
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace nil
                    mevedel--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (setq late-callback callback)))
                ((symbol-function 'mevedel-view--abort-data-buffer) #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (mevedel-view--run-prompt-submit-hook
           "guest prompt" "guest prompt"
           (lambda (_accepted) (gptel-send)))
          (setq submission mevedel-view--prompt-hook-pending))
        (mevedel-collaboration--handle-abort room 1)
        (funcall late-callback nil))
      (should (eq 'cancelled
                  (mevedel-prompt-submission-state submission)))
      (should-not send-called)
      (with-current-buffer data-buf
        (should (string-empty-p (buffer-string)))))))

(mevedel-deftest mevedel-collaboration--on-prompt-created
  (:doc "presents remote-capable prompts to writable guests only, gated by the defcustom")
  (with-temp-buffer
    (insert "prompt text")
    (let* ((guests (make-hash-table :test #'eql))
           (requests (make-hash-table :test #'eql))
           (room (list :transport 'transport :guests guests
                       :data-buffer (current-buffer)
                       :ui-requests requests))
           (overlay (make-overlay 1 5))
           sent)
      (overlay-put overlay 'mevedel--remote
                   '(:body "Run rm -rf /tmp/x?"
                     :options ((allow-once . "Allow once")
                               (deny-once . "Deny"))
                     :feedback t))
      (overlay-put overlay 'mevedel-view-interaction-id 'patch-review)
      (puthash 1 (list :name "phone" :writable t :ready t) guests)
      (puthash 2 (list :name "laptop" :writable nil :ready t) guests)
      (let ((mevedel-collaboration--rooms (mevedel-collab-test--rooms room))
            (mevedel-collaboration-remote-interactions t))
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t)))
          ;; An overlay without a remote descriptor is never broadcast.
          (mevedel-collaboration--on-prompt-created (make-overlay 1 2))
          (should-not sent)
          (mevedel-collaboration--on-prompt-created overlay)
          (should (= 1 (length sent)))
          ;; Writable guest only; the read-only guest sees nothing.
          (should (= 1 (car (car sent))))
          (let ((frame (cdr (car sent))))
            (should (equal "ui-request" (plist-get frame :t)))
            (should (equal "Run rm -rf /tmp/x?" (plist-get frame :body)))
            (should (equal '("Allow once" "Deny")
                           (mapcar (lambda (option)
                                     (cdr (assoc "label" option)))
                                   (append (plist-get frame :options) nil))))
            (should (eq t (plist-get frame :allowFeedback)))
            (should (= 1 (hash-table-count requests)))
            ;; An in-flight body-only update reuses the request and removes
            ;; every decision from the guest card.
            (setq sent nil)
            (overlay-put overlay 'mevedel--remote
                         '(:body "Applying patch" :body-kind "text"))
            (mevedel-collaboration--on-prompt-created overlay)
            (let ((update (cdr (car sent))))
              (should (equal (plist-get frame :reqId)
                             (plist-get update :reqId)))
              (should (equal "Applying patch" (plist-get update :body)))
              (should (= 0 (length (plist-get update :options))))
              (should (eq :json-false
                          (plist-get update :allowFeedback))))
            ;; A late-joining writable guest receives the active request.
            (setq sent nil)
            (mevedel-collaboration--send-ui-requests room 7)
            (should (equal (plist-get frame :reqId)
                           (plist-get (cdr (car sent)) :reqId))))
          ;; The defcustom gates the whole surface.
          (let ((mevedel-collaboration-remote-interactions nil))
            (setq sent nil)
            (mevedel-collaboration--on-prompt-created overlay)
            (should-not sent)))))))

(mevedel-deftest mevedel-collaboration--handle-ui-response
  (:doc "settles once with the mapped outcome and ignores unauthorized answers")
  (with-temp-buffer
    (insert "prompt text")
    (let* ((guests (make-hash-table :test #'eql))
           (requests (make-hash-table :test #'eql))
           (room (list :transport 'transport :guests guests
                       :data-buffer (current-buffer)
                       :ui-requests requests))
           (overlay (make-overlay 1 5))
           (accepted nil)
           settled sent)
      (overlay-put overlay 'mevedel--remote
                   (list :options
                         (list '(allow-once . "Allow once")
                               (cons (lambda () (setq accepted t)) "Accept"))
                         :feedback t))
      (puthash 1 (list :name "phone" :writable t :ready t) guests)
      (puthash 2 (list :name "laptop" :writable nil :ready t) guests)
      (puthash 41 overlay requests)
      (let ((mevedel-collaboration--rooms (mevedel-collab-test--rooms room))
            (mevedel-collaboration-remote-interactions t))
        (cl-letf (((symbol-function 'mevedel--prompt--settle)
                   (lambda (_overlay outcome) (setq settled outcome)))
                  ((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          ;; A read-only guest and an unknown request id are ignored.
          (mevedel-collaboration--handle-ui-response
           room 2 (list :reqId 41 :option 0))
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 999 :option 0))
          (should-not settled)
          ;; A symbol option settles through the shared settle.
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 0))
          (should (eq 'allow-once settled))
          ;; A function option runs instead of settling.
          (setq settled nil)
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 1))
          (should accepted)
          (should-not settled)
          ;; Feedback maps to the standard feedback outcome.
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :feedback "  needs a dry run  "))
          (should (equal '(feedback . "needs a dry run") settled))
          ;; A questionnaire answer set reaches the answer handler
          ;; atomically, trimmed; incomplete answers are refused.
          (let ((received nil))
            (overlay-put overlay 'mevedel--remote
                         (append (overlay-get overlay 'mevedel--remote)
                                 (list :answer
                                       (lambda (answers)
                                         (setq received answers)))))
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '(" MVP first " "Yes")))
            (should (equal '("MVP first" "Yes") received))
            (setq received nil)
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '("MVP first" "   ")))
            (should-not received)
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '("MVP first" 42)))
            (should-not received)
            ;; A guest is untrusted, and an answer reaches the model and the
            ;; transcript exactly as a guest prompt does, so it carries the
            ;; same byte budget.
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41
                          :answers
                          (list (make-string
                                 (1+ mevedel-collaboration--max-prompt-bytes)
                                 ?x)
                                "Yes")))
            (should-not received)
            ;; Answers that each clear the budget but together exceed it
            ;; still arrive in one tool result.
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41
                          :answers
                          (list (make-string
                                 (/ mevedel-collaboration--max-prompt-bytes 2)
                                 ?x)
                                (make-string
                                 mevedel-collaboration--max-prompt-bytes ?y))))
            (should-not received))
          (setq settled nil)
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41
                        :feedback (make-string
                                   (1+ mevedel-collaboration--max-prompt-bytes)
                                   ?x)))
          (should-not settled)
          ;; A cancel response runs the remote cancel handler when the
          ;; interaction offers one; without a handler, or from a
          ;; read-only guest, it is ignored.
          (let ((cancelled nil))
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :cancel t))
            (should-not cancelled)
            (overlay-put overlay 'mevedel--remote
                         (append (overlay-get overlay 'mevedel--remote)
                                 (list :cancel
                                       (lambda () (setq cancelled t)))))
            (mevedel-collaboration--handle-ui-response
             room 2 (list :reqId 41 :cancel t))
            (should-not cancelled)
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :cancel t))
            (should cancelled))
          ;; A questionnaire overlay's frame carries the questions and
          ;; advertises its cancel affordance.
          (overlay-put overlay 'mevedel--remote
                       (append (overlay-get overlay 'mevedel--remote)
                               (list :questions
                                     (lambda ()
                                       '((("question" . "Which?")
                                          ("options" . [(("label" . "A"))])))))))
          (let ((frame (mevedel-collaboration--ui-request-frame 41 overlay)))
            (should (equal "Which?"
                           (cdr (assoc "question"
                                       (aref (plist-get frame :questions)
                                             0)))))
            (should (eq t (plist-get frame :allowCancel))))
          ;; Settlement dismisses the request everywhere writable.
          (mevedel-collaboration--on-prompt-settled overlay)
          (should (= 0 (hash-table-count requests)))
          (should (equal '(1 . (:t "ui-request-end" :reqId 41))
                         (car sent)))
          ;; A late answer after dismissal is ignored silently.
          (setq settled nil)
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 0))
          (should-not settled))))))

(mevedel-deftest mevedel-collaboration--on-frame
  (:doc "dispatches known frames and stops the room on handler failure")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)))
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room))
           handled stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--handle-hello)
                 (lambda (_room peer _frame) (setq handled peer))))
        (mevedel-collaboration--on-frame (current-buffer) 5
                                         (list :t "hello" :proto 2)))
      (should (= 5 handled))
      ;; Unknown frame types are tolerated.
      (mevedel-collaboration--on-frame (current-buffer) 5
                                       (list :t "future-frame"))
      (cl-letf (((symbol-function 'mevedel-collaboration--handle-prompt)
                 (lambda (&rest _) (error "Handler fault")))
                ((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason)))
                ((symbol-function 'display-warning) (lambda (&rest _) nil)))
        (mevedel-collaboration--on-frame (current-buffer) 5
                                         (list :t "prompt" :text "x"))
        (should (eq 'observer-failure stopped))))))

(mevedel-deftest mevedel-collaboration--on-control
  (:doc "drops a guest on peer-left and waits for hello on peer-joined")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests))
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
      (mevedel-collaboration--on-control (current-buffer) 'peer-joined 1)
      (should (= 0 (hash-table-count guests)))
      (puthash 1 (list :name "g") guests)
      (mevedel-collaboration--on-control (current-buffer) 'peer-left 1)
      (should (= 0 (hash-table-count guests))))))

(mevedel-deftest mevedel-collaboration--on-state
  (:doc "clears the guest registry when the relay connection drops")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests))
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
      (puthash 1 (list :name "g") guests)
      (mevedel-collaboration--on-state (current-buffer) 'down)
      (should (= 0 (hash-table-count guests)))
      (mevedel-collaboration--on-state (current-buffer) 'open)
      (mevedel-collaboration--on-state (current-buffer) 'stopped))))

;;
;;; Room lifecycle

(mevedel-deftest mevedel-collaboration--start
  (:doc "builds the room, both bearer links, and the TTL timer")
  (with-temp-buffer
    (let ((mevedel-collaboration-relay-url "ws://127.0.0.1:1")
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
  (:doc "says bye to guests, stops the transport, and cancels timers")
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
      (let ((mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
        (cl-letf (((symbol-function 'cancel-timer)
                   (lambda (timer) (push timer cancelled)))
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
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room))
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
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
      (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason))))
        (mevedel-collaboration--stop-for-session)
        (should (eq 'data-buffer-killed stopped))))))

;;
;;; Observer and command boundaries

(mevedel-deftest mevedel-collaboration--safe-post-stream
  (:doc "contains observer failures without signaling into the request")
  (with-temp-buffer
    (let* ((stopped nil)
           (room (list :data-buffer (current-buffer)))
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
      (cl-letf (((symbol-function 'mevedel-collaboration--post-stream)
                 (lambda () (error "Observer failure")))
                ((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason)))
                ((symbol-function 'display-warning) (lambda (&rest _) nil)))
        (should-not (mevedel-collaboration--safe-post-stream))
        (should (eq 'observer-failure stopped))))))

(mevedel-deftest mevedel-collaboration-status
  (:doc "reports safe active and inactive status without exposing secrets")
  (let* ((messages nil)
         (guests (make-hash-table :test #'eql))
         (room (list :session-label "share"
                     :transport 'transport
                     :key "secret-key-bytes"
                     :write-token "secret-token"
                     :link-full "http://example/#room.full-secret"
                     :link-view "http://example/#room.view-secret"
                     :guests guests))
         (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
    (puthash 1 (list :name "Phone" :writable t :ready t) guests)
    (puthash 2 (list :name "Laptop" :writable nil :ready t) guests)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'mevedel-collaboration--transport-open-p)
               (lambda (_) t)))
      (mevedel-collaboration-status)
      (should (string-match-p "share" (car messages)))
      (should (string-match-p "connected" (car messages)))
      (should (string-match-p "Phone" (car messages)))
      (should (string-match-p "Laptop (view)" (car messages)))
      (should-not (string-match-p "secret" (car messages)))
      (clrhash mevedel-collaboration--rooms)
      (mevedel-collaboration-status)
      (should (string-match-p "inactive" (car messages))))))

(mevedel-deftest mevedel-collaboration-status--preserves-composer
  (:doc "preserves a multiline composer draft beginning with >")
  (with-temp-buffer
    (insert "> first line\nsecond line\n> third line")
    (let* ((before (buffer-string))
           (room (list :session-label "draft" :transport nil
                       :guests (make-hash-table :test #'eql)))
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (mevedel-collaboration-status))
      (should (equal before (buffer-string))))))

(mevedel-deftest mevedel-collaboration-stop
  (:doc "stops the current or only room and never another session's share")
  (let* ((stopped nil)
         (messages nil)
         (room (list :transport 'transport :session-label "share"))
         (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
    (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
               (lambda (stop-room reason) (push (cons stop-room reason)
                                                stopped)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      ;; From a session that is not shared, another session's share must
      ;; survive: report instead of tearing it down.
      (with-temp-buffer
        (cl-letf (((symbol-function
                    'mevedel-collaboration--current-data-buffer)
                   (lambda () (current-buffer))))
          (mevedel-collaboration-stop)))
      (should-not stopped)
      (should (string-match-p "no active share" (car messages)))
      ;; Outside any session context, stop falls back to every share.
      (cl-letf (((symbol-function
                  'mevedel-collaboration--current-data-buffer)
                 (lambda () nil)))
        (mevedel-collaboration-stop))
      (should (equal (list (cons room 'user-stop)) stopped))
      (should (string-match-p "stopped" (car messages)))
      (clrhash mevedel-collaboration--rooms)
      (mevedel-collaboration-stop)
      (should (string-match-p "not active" (car messages))))))

(mevedel-deftest mevedel-collaboration--room-for-overlay
  (:doc "resolves side-conversation interaction overlays to the parent session's room")
  (let* ((parent-data (generate-new-buffer " *collab-overlay-parent*"))
         (side-data (generate-new-buffer " *collab-overlay-side*"))
         (side-view (generate-new-buffer " *collab-overlay-view*"))
         (room (list :data-buffer parent-data))
         (mevedel-collaboration--rooms (mevedel-collab-test--rooms room)))
    (unwind-protect
        (progn
          (require 'mevedel-side-conversation)
          (with-current-buffer side-data
            (setq-local mevedel-side-conversation--parent-buffer
                        parent-data))
          (with-current-buffer side-view
            (setq-local mevedel--data-buffer side-data)
            (insert "prompt")
            ;; A /btw permission prompt renders in the side view; its
            ;; authority surface is the parent session's room.
            (should (eq room (mevedel-collaboration--room-for-overlay
                              (make-overlay 1 2))))))
      (kill-buffer side-view)
      (kill-buffer side-data)
      (kill-buffer parent-data))))

(mevedel-deftest mevedel-collaboration--dead-dial-warning
  (:doc "warns once when the relay dial never opens, and never after it has")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)))
           (mevedel-collaboration--rooms (mevedel-collab-test--rooms room))
           warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type text &rest _) (push text warnings))))
        ;; A share whose dial never succeeded is not live; say so once.
        (mevedel-collaboration--on-state (current-buffer) 'down)
        (mevedel-collaboration--on-state (current-buffer) 'down)
        (should (= 1 (length warnings)))
        (should (string-match-p "not live" (car warnings)))
        ;; An ordinary drop after a working connection stays quiet.
        (setq warnings nil)
        (mevedel-collaboration--on-state (current-buffer) 'open)
        (mevedel-collaboration--on-state (current-buffer) 'down)
        (should-not warnings)))))

(mevedel-deftest mevedel-collaboration-view
  (:doc "discloses secrets and bearer-link scope before starting")
  (let ((session (mevedel-session--create :name "share"))
        (data-buffer (generate-new-buffer " *collaboration-disclosure*"))
        prompts)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function
                      'mevedel-collaboration--current-data-buffer)
                     (lambda () data-buffer))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (push prompt prompts)
                       nil)))
            (should-error (mevedel-collaboration-view) :type 'user-error))
          (should (string-match-p "credentials or secrets" (car prompts)))
          (should (string-match-p "bearer" (car prompts))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-collaboration--share-content
  (:doc "renders the selected link's QR, its scope, and the key legend")
  (let* ((room (list :session-label "share"
                     :key "raw-key"
                     :link-view "http://x/#room.view-secret"
                     :link-full "http://x/#room.full-secret"))
         (view (mevedel-collaboration--share-content room 'view))
         (full (mevedel-collaboration--share-content room 'full)))
    ;; The view side shows only the view link and says it is read-only;
    ;; the full side is loud about what its bearer can do.
    (should (string-match-p "room\\.view-secret" view))
    (should-not (string-match-p "room\\.full-secret" view))
    (should (string-match-p "read-only" view))
    (should (string-match-p "room\\.full-secret" full))
    (should-not (string-match-p "room\\.view-secret" full))
    (should (string-match-p "FULL CONTROL" full))
    ;; A real QR block is present and the two codes differ.
    (should (string-match-p "█" view))
    (should-not (equal view full))
    ;; The raw key never renders, and the key legend does.
    (dolist (content (list view full))
      (should-not (string-match-p "raw-key" content))
      (should (string-match-p "TAB" content))
      (should (string-match-p "bearer" content)))
    ;; The QR is the convenience; the link is the payload.  An
    ;; unavailable or failing encoder must not take the links with it.
    (cl-letf (((symbol-function 'qrencode)
               (lambda (&rest _) (error "No QR for you"))))
      (let ((degraded (mevedel-collaboration--share-content room 'view)))
        (should (string-match-p "room\\.view-secret" degraded))
        (should (string-match-p "QR unavailable" degraded))
        (should (string-match-p "TAB" degraded))))))

(mevedel-deftest mevedel-collaboration--show-share-frame
  (:doc "falls back to a window when child-frame creation signals")
  (let ((room (list :session-label "share"
                    :link-view "http://x/#room.view"
                    :link-full "http://x/#room.full"))
        popped)
    (unwind-protect
        ;; A theme with a cyclic face spec makes any make-frame signal
        ;; (seen live with doom-one's gnus faces); the share must still
        ;; be presentable.
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&optional _) t))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _)
                     (error "Face inheritance results in inheritance cycle")))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq popped buffer))))
          (mevedel-collaboration--show-share-frame room)
          (should (buffer-live-p popped))
          (should (equal "*mevedel share*" (buffer-name popped))))
      (when-let* ((buffer (get-buffer "*mevedel share*")))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-collaboration--report-links
  (:doc "copies the full link, opens the share frame, and keeps links out of messages")
  (let ((room (list :link-full "http://x/#room.full"
                    :link-view "http://x/#room.view"
                    :session-label "share"
                    :key "raw-key"))
        killed shown messages)
    (cl-letf (((symbol-function 'kill-new)
               (lambda (text) (setq killed text)))
              ((symbol-function 'mevedel-collaboration--show-share-frame)
               (lambda (shown-room) (setq shown shown-room)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (mevedel-collaboration--report-links room))
    (should (equal "http://x/#room.full" killed))
    (should (eq room shown))
    ;; The links live in the share frame now; *Messages* is durable and
    ;; carries neither bearer link.
    (should-not (string-match-p "room\\.full" (car messages)))
    (should-not (string-match-p "room\\.view" (car messages)))
    (should-not (string-match-p "raw-key" (car messages)))))

(mevedel-deftest mevedel-cmd--collab
  (:doc "does not return a bearer URL to slash dispatch")
  (cl-letf (((symbol-function 'mevedel-collaboration-view)
             (lambda () "http://127.0.0.1:1/#room.secret")))
    (should-not (mevedel-cmd--collab "view"))
    (should-not (mevedel-cmd--collab ""))))

(mevedel-deftest mevedel-skills--dispatch-slash-command
  (:doc "dispatches /collab without copying its bearer URL into messages")
  (with-temp-buffer
    (let ((gptel-prompt-prefix-alist '((fundamental-mode . "### ")))
          (messages nil))
      (insert "### /collab view")
      (cl-letf (((symbol-function 'mevedel-collaboration-view)
                 (lambda () "http://127.0.0.1:1/#room.secret"))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should-not (seq-some (lambda (message)
                                (string-match-p "room\\.secret" message))
                              messages))))))

(mevedel-deftest mevedel-skills-local-command-active-request-p
  (:doc "allows collaboration safety commands while a request is active")
  (progn
    (should (mevedel-skills-local-command-active-request-p "collab" "status"))
    (should (mevedel-skills-local-command-active-request-p "collab" "stop"))))

;;; test-mevedel-collaboration.el ends here
