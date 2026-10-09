;;; test-mevedel-collaboration-agent.el --- focused collaboration tests -*- lexical-binding: t; -*-

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
(require 'mevedel-artifact-store)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-view-agent)
(require 'mevedel-view-render)
(require 'mevedel-workspace)

(require 'mevedel-collaboration-agent)

(mevedel-deftest mevedel-collaboration--agent-rows
  (:doc "lists agents sorted by path, settled ones with terminal outcomes")
  (let* ((registry
          (list (cons "/root/worker-2"
                      (mevedel-agent-record--create
                       :path "/root/worker-2" :role 'worker
                       :activity 'running))
                (cons "/root/explorer-1"
                      (mevedel-agent-record--create
                       :path "/root/explorer-1" :role 'explorer
                       :activity 'permission-blocked))
                (cons "/root/worker-1"
                      (mevedel-agent-record--create
                       :path "/root/worker-1"
                       :activity 'waiting))
                (cons "/root/worker-3"
                      (mevedel-agent-record--create
                       :path "/root/worker-3" :role 'worker
                       :activity 'idle
                       :settled-outcome 'completed))
                (cons "/root/worker-4"
                      (mevedel-agent-record--create
                       :path "/root/worker-4" :role 'worker
                       :activity 'idle
                       :settled-outcome 'errored))
                (cons "/root/worker-5"
                      (mevedel-agent-record--create
                       :path "/root/worker-5" :role 'worker
                       :activity 'idle
                       :settled-outcome 'interrupted))
                (cons "/root/worker-6"
                      (mevedel-agent-record--create
                       :path "/root/worker-6" :role 'worker
                       :activity 'idle))))
         (session (mevedel-session--create :name "agents"
                                           :agent-registry registry))
         (rows (mevedel-collaboration--agent-rows
                (list :session session))))
    ;; An idle record without a settled outcome is not reported as done.
    (should (equal '("/root/explorer-1" "/root/worker-1" "/root/worker-2"
                     "/root/worker-3" "/root/worker-4" "/root/worker-5")
                   (mapcar (lambda (row) (cdr (assoc "path" row))) rows)))
    ;; Settled agents keep travelling with their terminal outcome, so
    ;; the viewer's finished list can reach their retained transcripts.
    (should (equal '("blocked" "waiting" "running" "done" "errored"
                     "interrupted")
                   (mapcar (lambda (row) (cdr (assoc "status" row))) rows)))
    (should (equal "explorer" (cdr (assoc "role" (nth 0 rows)))))
    ;; A record without a role sends no role field at all.
    (should-not (assoc "role" (nth 1 rows)))
    ;; A room without a session has no roster.
    (should-not (mevedel-collaboration--agent-rows (list :session nil)))))

(mevedel-deftest mevedel-collaboration--publish-agents
  (:doc "broadcasts the roster once per change, an emptied roster included")
  (let* ((guests (make-hash-table :test #'eql))
         (session (mevedel-session--create
                   :name "agents"
                   :agent-registry
                   (list (cons "/root/worker-1"
                               (mevedel-agent-record--create
                                :path "/root/worker-1" :role 'worker
                                :activity 'running)))))
         (room (list :session session :guests guests :transport 'transport))
         sent)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (puthash 1 (list :name "g" :writable nil :ready t) guests)
      (mevedel-collaboration--publish-agents room)
      (let ((frame (cdr (car sent))))
        (should (equal "agents" (plist-get frame :t)))
        (should (= 1 (length (plist-get frame :agents))))
        (should (equal "/root/worker-1"
                       (cdr (assoc "path"
                                   (aref (plist-get frame :agents) 0))))))
      ;; An unchanged roster is not repeated.
      (setq sent nil)
      (mevedel-collaboration--publish-agents room)
      (should-not sent)
      ;; Settling the last agent broadcasts the empty roster, so the
      ;; guest's strip is cleared rather than frozen on stale rows.
      (setf (mevedel-session-agent-registry session) nil)
      (mevedel-collaboration--publish-agents room)
      (should (equal [] (plist-get (cdr (car sent)) :agents))))))

(mevedel-deftest mevedel-collaboration-notify-agents-changed
  (:doc "schedules the shared room's coalesced publication and ignores others")
  (let* ((data-buffer (generate-new-buffer " *collab-agents-data*"))
         (session (mevedel-session--create :name "agents"))
         (room (list :session session :data-buffer data-buffer
                     :guests (make-hash-table :test #'eql)
                     :transport 'transport))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--schedule-publish)
                   (lambda (target) (push target scheduled))))
          (mevedel-collaboration-notify-agents-changed session)
          (should (equal (list room) scheduled))
          ;; An unshared session is simply not a room.
          (mevedel-collaboration-notify-agents-changed
           (mevedel-session--create :name "other"))
          (should (= 1 (length scheduled)))
          (should-not (mevedel-collaboration-notify-agents-changed nil)))
      (kill-buffer data-buffer))))


(mevedel-deftest mevedel-collaboration--agent-conversation
  (:doc "resolves a registry path to its live conversation buffer only")
  (let* ((buffer (generate-new-buffer " *agent-conversation*"))
         (record (mevedel-agent-record--create
                  :path "/root/worker-1" :conversation-buffer buffer))
         (session (mevedel-session--create
                   :name "s"
                   :agent-registry (list (cons "/root/worker-1" record))))
         (room (list :session session)))
    (unwind-protect
        (progn
          (should (eq buffer (mevedel-collaboration--agent-conversation
                              room "/root/worker-1")))
          ;; A path outside the registry never reaches the filesystem.
          (should-not (mevedel-collaboration--agent-conversation
                       room "/root/worker-2"))
          (should-not (mevedel-collaboration--agent-conversation room 5))
          (should-not (mevedel-collaboration--agent-conversation
                       (list :session nil) "/root/worker-1"))
          ;; A cold agent has no resident conversation.
          (kill-buffer buffer)
          (should-not (mevedel-collaboration--agent-conversation
                       room "/root/worker-1")))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(mevedel-deftest mevedel-collaboration--queue-agent-load
  (:doc "cold loading survives guest frames processed inside a TRAMP timer suspension")
  (let* ((record (mevedel-agent-record--create :path "/root/cold"))
         (session (mevedel-session--create
                   :agent-registry (list (cons "/root/cold" record))))
         (room (list :session session))
         (mevedel-transport--held-timers nil)
         loaded)
    (require 'mevedel-transport)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-transport-nested-p) (lambda () t))
                  ((symbol-function 'mevedel-collaboration--load-agent)
                   (lambda (seen-room path)
                     (should (eq room seen-room))
                     (should (equal path "/root/cold"))
                     (setq loaded t))))
          (let ((timer-list nil))
            (should (mevedel-collaboration--queue-agent-load room "/root/cold"))
            (should (mevedel-collaboration--queue-agent-load room "/root/cold"))
            (should-not timer-list))
          (should (= 1 (length mevedel-transport--held-timers)))
          (let ((timer (car mevedel-transport--held-timers)))
            (apply (timer--function timer) (timer--args timer)))
          (should loaded))
      (mapc #'cancel-timer mevedel-transport--held-timers))))

(mevedel-deftest mevedel-collaboration--load-agent
  (:doc "loads a cold agent once off the frame handler and refuses a failed load")
  (let* ((data (generate-new-buffer " *agent-load-root*"))
         (cold (mevedel-agent-record--create :path "/root/cold"))
         (broken (mevedel-agent-record--create :path "/root/broken"))
         (session (mevedel-session--create
                   :name "s"
                   :agent-registry (list (cons "/root/cold" cold)
                                         (cons "/root/broken" broken))))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :guests guests :transport 'transport
                     :data-buffer data))
         (now 1000.0)
         loaded timers sent)
    (puthash 1 (list :name "viewer" :writable nil :ready t) guests)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport _peer frame) (push frame sent) t))
                  ((symbol-function 'float-time) (lambda (&optional _) now))
                  ((symbol-function 'run-at-time)
                   (lambda (_time _repeat fn &rest args)
                     (push (cons fn args) timers)))
                  ((symbol-function 'mevedel-collaboration--room-for-buffer)
                   (lambda (buffer) (and (eq buffer data) room)))
                  ((symbol-function 'mevedel-collaboration--canonical-records)
                   (lambda (_buffer) nil))
                  ((symbol-function 'mevedel-agent-persistence-ensure-conversation)
                   (lambda (seen-session record root _readonly)
                     (should (eq session seen-session))
                     (should (eq data root))
                     (when (eq record broken) (error "Corrupt transcript"))
                     (setq loaded (generate-new-buffer " *agent-loaded*"))
                     (setf (mevedel-agent-record-conversation-buffer record) loaded))))
          ;; A cold agent queues one load and sends nothing yet.
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 1 :path "/root/cold"))
          (should-not sent)
          (should (= 1 (length timers)))
          ;; A poll while the load is pending queues no second load.
          (setq now 1002.0)
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 2 :path "/root/cold"))
          (should-not sent)
          (should (= 1 (length timers)))
          ;; Once loaded, the next poll gets the transcript.
          (apply (caar timers) (cdar timers))
          (should (buffer-live-p loaded))
          (setq now 1004.0)
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 3 :path "/root/cold"))
          (should (eq t (plist-get (car sent) :final)))
          (should-not (plist-get (car sent) :error))
          ;; A failed load is refused on the next poll and never retried.
          (setq timers nil sent nil now 1006.0)
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 4 :path "/root/broken"))
          (apply (caar timers) (cdar timers))
          (setq now 1008.0)
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 5 :path "/root/broken"))
          (should (stringp (plist-get (car sent) :error)))
          (should (= 1 (length timers)))
          ;; A load queued for a room that has since stopped does nothing.
          (setf (mevedel-agent-record-conversation-buffer cold) nil)
          (setq timers nil now 1010.0)
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 6 :path "/root/cold"))
          (cl-letf (((symbol-function 'mevedel-collaboration--room-for-buffer)
                     #'ignore))
            (apply (caar timers) (cdar timers)))
          (should-not (mevedel-agent-record-conversation-buffer cold)))
      (when (buffer-live-p loaded) (kill-buffer loaded))
      (kill-buffer data))))

(mevedel-deftest mevedel-collaboration--handle-fetch-agent
  (:doc "answers chunked projected records with an unchanged latch and a throttle")
  (let* ((save-path (make-temp-file "mevedel-agent-artifact-" t))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root save-path :name "w"))
         (artifact-dir (mevedel-artifact-store-directory workspace))
         (artifact-path (file-name-concat artifact-dir "agent.html"))
         (root-artifact-path (file-name-concat artifact-dir "root.html"))
         (buffer (generate-new-buffer " *agent-fetch*"))
         (record (mevedel-agent-record--create
                  :path "/root/worker-1" :conversation-buffer buffer))
         (session (mevedel-session--create
                   :name "s"
                   :save-path save-path
                   :workspace workspace
                   :agent-registry (list (cons "/root/worker-1" record))))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :guests guests :transport 'transport
                     :records
                     (list (list :id "agent-artifact" :kind "tool"
                                 :artifact "root.html"
                                 :artifact-path root-artifact-path))))
         (now 1000.0)
         (canonical
          (list (list :id "u" :kind "user" :revision 0
                      :text "find the bug")
                (list :id "a" :kind "assistant" :revision 0
                      :text "Looking")))
         sent)
    ;; A read-only guest may fetch: the transcript is read state.
    (puthash 1 (list :name "viewer" :writable nil :ready t) guests)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'float-time)
                   (lambda (&optional _) now))
                  ((symbol-function 'mevedel-collaboration--canonical-records)
                   (lambda (seen)
                     (should (eq buffer seen))
                     canonical)))
          ;; A path outside the registry earns a targeted refusal.
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 1 :path "/root/missing"))
          (should (equal '(1) (mapcar #'car sent)))
          (should (equal "agent" (plist-get (cdr (car sent)) :t)))
          (should (stringp (plist-get (cdr (car sent)) :error)))
          ;; A registry path answers final-flagged records with a digest.
          (setq now 1002.0 sent nil)
          (let ((mevedel-collaboration--max-frame-json-bytes 250))
            (mevedel-collaboration--handle-fetch-agent
             room 1 (list :reqId 2 :path "/root/worker-1")))
          (setq sent (nreverse sent))
          (should (> (length sent) 1))
          (dolist (entry sent)
            (should (<= (string-bytes (json-encode
                                       (cdr entry)))
                        250)))
          (let ((frame (cdr (car sent))))
            (should (equal "agent" (plist-get frame :t)))
            (should (= 2 (plist-get frame :reqId)))
            (should (equal "/root/worker-1" (plist-get frame :path)))
            (should (eq :json-false (plist-get frame :final)))
            (should (= 1 (length (plist-get frame :records))))
            (should (equal "u" (cdr (assoc "id"
                                           (aref (plist-get frame :records)
                                                 0)))))
            (should (stringp (plist-get frame :digest)))
            (should (eq t (plist-get (cdr (car (last sent))) :final)))
            ;; A matching known digest earns one unchanged frame instead
            ;; of the transcript again.
            (setq now 1004.0 sent nil)
            (mevedel-collaboration--handle-fetch-agent
             room 1 (list :reqId 3 :path "/root/worker-1"
                          :known (plist-get frame :digest)))
            (should (= 1 (length sent)))
            (should (eq t (plist-get (cdr (car sent)) :unchanged)))
            (should-not (plist-member (cdr (car sent)) :records)))
          ;; A repeat inside the throttle window is dropped silently;
          ;; the viewer's next poll catches up.
          (setq now 1004.5 sent nil)
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 4 :path "/root/worker-1"))
          (should-not sent)
          ;; An unregistered peer and a malformed request id get nothing.
          (setq now 1010.0 sent nil)
          (mevedel-collaboration--handle-fetch-agent
           room 9 (list :reqId 5 :path "/root/worker-1"))
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId "5" :path "/root/worker-1"))
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId -1 :path "/root/worker-1"))
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId #x20000000000000 :path "/root/worker-1"))
          (should-not sent)
          ;; An artifact card sent through an agent transcript is valid fetch
          ;; authority for that guest even though the root transcript lacks it.
          (make-directory artifact-dir t)
          (write-region "<h1>agent</h1>" nil artifact-path nil 'silent)
          (write-region "<h1>root</h1>" nil root-artifact-path nil 'silent)
          (setq canonical
                (list (list :id "agent-artifact" :kind "tool" :revision 0
                            :name "ApplyPatch" :artifact "agent.html"
                            :artifact-path artifact-path)))
          (setq now 1012.0 sent nil)
          (mevedel-collaboration--handle-fetch-agent
           room 1 (list :reqId 6 :path "/root/worker-1"))
          (let ((published-id
                 (plist-get
                  (car (plist-get (gethash 1 guests) :agent-artifacts))
                  :id)))
            (should (string-prefix-p "agent-artifact-" published-id))
            (should-not (equal "agent-artifact" published-id))
            (setq now 1014.0 sent nil)
            (mevedel-collaboration--handle-artifact-get
             room 1 (list :reqId 7 :id published-id))
            (should (equal "<h1>agent</h1>"
                           (base64-decode-string
                            (mapconcat (lambda (entry)
                                         (plist-get (cdr entry) :data))
                                       (nreverse sent)))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-collaboration--handle-execution-result-get
  () ,test (test)
  :doc "parent breadcrumb has no stdout; authorized click prefers child evidence"
  (let* ((directory (make-temp-file "mevedel-guest-result-" t))
         (session (mevedel-session--create :name "parent" :save-path directory
                                          :current-segment 2 :authority-mode 'pid-lock))
         (child-session (mevedel-session--create :name "child" :save-path
                                                (file-name-concat directory "child")
                                                :current-segment 1))
         (parent (generate-new-buffer " *guest parent result*"))
         (child (generate-new-buffer " *guest child result*"))
         (agent (mevedel-agent-record--create :path "/root/child"
                                             :conversation-buffer child))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :data-buffer parent :transport 'transport
                     :guests guests))
         sent)
    (setf (mevedel-session-agent-registry session) (list (cons "/root/child" agent)))
    (puthash 1 (list :ready t :writable nil) guests)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "forwarded bounded output\n"
                    "<bash-execution execution_id=\"exec-1\" command=\"exit 2\" "
                    "outcome=\"failure\" exit_code=\"2\"/>\n"
                    "</agent-message>\n"))
          (with-current-buffer child
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session child-session)
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "tool-1"
                       :render-data (:execution-id "exec-1" :state completed
                                     :execution-output "CANONICAL CHILD OUTPUT")))))
          (should (equal "CANONICAL CHILD OUTPUT"
                         (plist-get
                          (plist-get
                           (car (mevedel-transcript-audit-records
                                 (with-current-buffer child (buffer-string))))
                           :render-data)
                          :execution-output)))
          (should (eq 'execution-completion
                      (plist-get (car (mevedel-transcript-audit-records
                                       (with-current-buffer child (buffer-string))))
                                 :type)))
          (should (mevedel-session-artifacts-transcript-segments
                   child-session child))
          (should (mevedel-collaboration--agent-conversation room "/root/child"))
          (should (equal 'readable
                         (plist-get (car (last (mevedel-session-artifacts-transcript-segments
                                                child-session child))) :status)))
          (should (equal "CANONICAL CHILD OUTPUT"
                         (plist-get (mevedel-collaboration--child-execution-facts
                                     room "/root/child" "exec-1")
                                    :execution-output)))
          ;; A normally settled Bash updates its row and leaves only a
          ;; breadcrumb audit: it need not emit execution-completion.
          (with-current-buffer child
            (erase-buffer)
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"exit 2\"))\ninitial")
              (insert (mevedel-tool-render-data-format
                       '(:execution-id "exec-1" :command "exit 2"
                         :state completed :outcome failure :status error
                         :execution-output "CANONICAL CHILD OUTPUT") "tool-1"))
              (put-text-property start (point) 'gptel '(tool . "tool-1")))
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "exec-1"
                       :tool-use-id "tool-1" :owner "/root/child"
                       :command "exit 2"
                       :facts (:execution-id "exec-1" :state completed
                               :outcome failure)))))
          (should (equal "CANONICAL CHILD OUTPUT"
                         (plist-get (mevedel-collaboration--child-execution-facts
                                     room "/root/child" "exec-1")
                                    :execution-output)))
          (let* ((records (mevedel-collaboration--canonical-records parent))
                 (record (car records))
                 (wire (json-encode (mevedel-collaboration--json-record record))))
            (should (= 1 (length records)))
            (should (equal "execution" (plist-get record :kind)))
            (should (equal "failed" (plist-get record :status)))
            (should (equal "/root/child" (plist-get (plist-get record :execution)
                                                  :owner)))
            (should-not (string-match-p "bounded output\\|CANONICAL CHILD OUTPUT"
                                        wire)))
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent) t)))
            (mevedel-collaboration--handle-execution-result-get
             room 999 '(:reqId 1 :owner "/root/child" :executionId "exec-1"))
            (should-not sent)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 1 :owner "/root/other" :executionId "exec-1"))
            (should (plist-get (cdar sent) :error))
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 2 :owner "/root/child" :executionId "exec-1"))
            (should (equal "child" (plist-get (cdar sent) :source)))
            (should (equal "CANONICAL CHILD OUTPUT" (plist-get (cdar sent) :output)))
            (should (= 1 (caar sent)))
            ;; Repeating a scan too soon earns a bounded retry response.
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 3 :owner "/root/child" :executionId "exec-1"))
            (should (equal "Retry result fetch shortly."
                           (plist-get (cdar sent) :error)))
            ;; After child loss and parent compaction, retained forwarded
            ;; evidence still answers from a readable older parent segment.
            (write-region (with-current-buffer parent (buffer-string)) nil
                          (mevedel-session-artifacts-segment-path directory 1)
                          nil 'silent)
            (with-current-buffer parent (erase-buffer))
            (kill-buffer child)
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 4 :owner "/root/child" :executionId "exec-1"))
            (should (equal "forwarded" (plist-get (cdar sent) :source)))
            (should (equal "forwarded bounded output" (plist-get (cdar sent) :output)))
            ;; A gone archive has no authority or result; no path is accepted
            ;; from the guest as a substitute.
            (delete-file (mevedel-session-artifacts-segment-path directory 1))
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 5 :owner "/root/child" :executionId "exec-1"
                     :outputPath "artifact://unrelated"))
            (should (equal "missing" (plist-get (cdar sent) :source)))
            (should (plist-get (cdar sent) :error))))
      (when (buffer-live-p parent) (kill-buffer parent))
      (when (buffer-live-p child) (kill-buffer child))
      (delete-directory directory t)))
  :doc "bounded retained output never exceeds the guest result frame budget"
  (let* ((directory (make-temp-file "mevedel-guest-result-bound-" t))
         (session (mevedel-session--create :name "bound" :save-path directory
                                          :current-segment 1))
         (parent (generate-new-buffer " *guest result bound*"))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :data-buffer parent :transport 'transport
                     :guests guests)) sent)
    (puthash 1 (list :ready t :writable nil) guests)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    (make-string 60000 ?x) "\n"
                    "<bash-execution execution_id=\"exec-large\" "
                    "outcome=\"success\"/>\n</agent-message>\n"))
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (setq sent frame) t)))
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 1 :owner "/root/child" :executionId "exec-large"))
            (should (eq t (plist-get sent :truncated)))
            (should (<= (string-bytes (plist-get sent :output)) 50000))
            (should (< (string-bytes (json-encode sent)) 100000))))
      (kill-buffer parent)
      (delete-directory directory t)))
  :doc "trusted root and child breadcrumbs resolve terminal rows without forwarding"
  (let* ((directory (make-temp-file "mevedel-local-guest-result-" t))
         (session (mevedel-session--create :name "root" :save-path directory))
         (child-session (mevedel-session--create
                         :name "child" :save-path (file-name-concat directory "child")))
         (parent (generate-new-buffer " *local guest result*"))
         (child (generate-new-buffer " *local child result*"))
         (agent (mevedel-agent-record--create :path "/root/child"
                                             :conversation-buffer child))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :data-buffer parent :transport 'transport
                     :guests guests)) sent)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/child" agent)))
    (puthash 1 (list :ready t :writable nil) guests)
    (unwind-protect
        (progn
          (cl-loop for buffer in (list parent child)
                   for owner in '("/root" "/root/child")
                   for own-session in (list session child-session)
                   for id in '("exec-root" "exec-child")
                   for output in '("ROOT CANONICAL" "CHILD CANONICAL") do
                   (with-current-buffer buffer
                     (delay-mode-hooks (org-mode))
                     (setq-local mevedel--session own-session)
                     (let ((start (point)))
                       (insert "(:name \"Bash\" :args (:command \"echo done\"))\ninitial")
                       (insert (mevedel-tool-render-data-format
                                (list :execution-id id :command "echo done"
                                      :state 'completed :outcome 'success
                                      :execution-output output) id))
                       (put-text-property start (point) 'gptel (cons 'tool id)))
                     (insert (mevedel--format-hook-audit-record
                              (list :type 'execution-breadcrumb :execution-id id
                                    :tool-use-id id :owner owner :command "echo done"
                                    :facts (list :execution-id id :state 'completed
                                                 :outcome 'success))))))
          (let ((records (mevedel-collaboration--canonical-records parent))
                (child-records (mevedel-collaboration--canonical-records child)))
            (should (= 2 (length records)))
            (should (= 2 (length child-records)))
            (should (equal "execution" (plist-get (cadr records) :kind)))
            (should (equal "execution" (plist-get (cadr child-records) :kind)))
            (should-not (string-search "ROOT CANONICAL"
                                       (json-encode (mevedel-collaboration--json-record
                                                     (cadr records))))))
          (with-current-buffer parent
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "exec-root"
                       :tool-use-id "exec-root" :owner "/root"
                       :command "echo done"
                       :facts (:execution-id "exec-root" :outcome success)))
                    (mevedel--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "exec-other"
                       :tool-use-id "other" :owner "/root"
                       :command "echo done"
                       :facts (:execution-id "exec-other" :outcome failure
                               :exit-code 2)))))
          (let ((records (mevedel-collaboration--canonical-records parent)))
            (should (= 3 (length records)))
            (should (equal "failed" (plist-get (nth 2 records) :status)))
            (should-not (equal (plist-get (nth 1 records) :id)
                               (plist-get (nth 2 records) :id))))
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (setq sent frame) t)))
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 1 :owner "/root" :executionId "exec-root"))
            (should (equal "child" (plist-get sent :source)))
            (should (equal "ROOT CANONICAL" (plist-get sent :output)))
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 2 :owner "/root/child" :executionId "exec-child"))
            (should (equal "CHILD CANONICAL" (plist-get sent :output)))
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 3 :owner "/root/other" :executionId "exec-child"
                     :outputPath "artifact://forged"))
            (should (equal "missing" (plist-get sent :source)))
            (should (plist-get sent :error))
            ;; The breadcrumb still authorizes a read, but missing source
            ;; output is reported instead of guessed from the original text.
            (with-current-buffer parent
              (delete-region (point-min)
                             (plist-get (car (mevedel-transcript-audit-buffer-spans
                                              'execution-breadcrumb)) :start)))
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 4 :owner "/root" :executionId "exec-root"))
            (should (equal "missing" (plist-get sent :source)))
            (should (plist-get sent :error))
            (with-current-buffer parent (erase-buffer))
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 5 :owner "/root" :executionId "exec-root"))
            (should (equal "missing" (plist-get sent :source)))))
      (when (buffer-live-p parent) (kill-buffer parent))
      (when (buffer-live-p child) (kill-buffer child))
      (delete-directory directory t)))
  :doc "local nested Bash audit supplies retained output without a child row or artifact"
  (let* ((directory (make-temp-file "mevedel-nested-guest-result-" t))
         (session (mevedel-session--create :name "root" :save-path directory))
         (child-session (mevedel-session--create
                         :name "child" :save-path (file-name-concat directory "child")))
         (parent (generate-new-buffer " *nested guest result*"))
         (child (generate-new-buffer " *nested child result*"))
         (agent (mevedel-agent-record--create :path "/root/child"
                                             :conversation-buffer child))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :data-buffer parent :transport 'transport
                     :guests guests)) sent)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/child" agent)))
    (puthash 1 (list :ready t :writable nil) guests)
    (unwind-protect
        (progn
          (cl-loop for buffer in (list parent child)
                   for owner in '("/root" "/root/child")
                   for own-session in (list session child-session)
                   for id in '("exec-nested-root" "exec-nested-child")
                   for tool-id in '("outer-root/1" "outer-child/1")
                   for output in '("ROOT NESTED OUTPUT" "CHILD NESTED OUTPUT") do
                   (with-current-buffer buffer
                     (delay-mode-hooks (org-mode))
                     (setq-local mevedel--session own-session)
                     (insert (mevedel--format-hook-audit-record
                              (list :type 'execution-breadcrumb
                                    :execution-id id :owner owner
                                    :tool-use-id tool-id :command "exit 1"
                                    :facts (list :execution-id id :outcome 'failure)))
                             (mevedel--format-hook-audit-record
                              (list :type 'execution-completion
                                    :tool-use-id tool-id :owner owner
                                    :render-data
                                    (list :execution-id id :state 'completed
                                          :outcome 'failure :status 'error
                                          :output-path "artifact://executions/unavailable.log"
                                          :execution-output output)))
                             (mevedel--format-hook-audit-record
                              (list :type 'execution-completion
                                    :tool-use-id "unrelated/1" :owner owner
                                    :render-data
                                    (list :execution-id id :state 'completed
                                          :execution-output "WRONG TOOL"))))))
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (setq sent frame) t)))
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 1 :owner "/root" :executionId "exec-nested-root"))
            (should (equal "child" (plist-get sent :source)))
            (should (equal "ROOT NESTED OUTPUT" (plist-get sent :output)))
            (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
            (mevedel-collaboration--handle-execution-result-get
             room 1 '(:reqId 2 :owner "/root/child" :executionId "exec-nested-child"))
            (should (equal "child" (plist-get sent :source)))
            (should (equal "CHILD NESTED OUTPUT" (plist-get sent :output)))))
      (when (buffer-live-p parent) (kill-buffer parent))
      (when (buffer-live-p child) (kill-buffer child))
      (delete-directory directory t))))


(provide 'test-mevedel-collaboration-agent)
;;; test-mevedel-collaboration-agent.el ends here
