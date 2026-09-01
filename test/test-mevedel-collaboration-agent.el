;;; test-mevedel-collaboration-agent.el --- focused collaboration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for the extracted collaboration feature module.

;;; Code:

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
          ;; A cold agent is refused rather than hydrated.
          (kill-buffer buffer)
          (should-not (mevedel-collaboration--agent-conversation
                       room "/root/worker-1")))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(mevedel-deftest mevedel-collaboration--handle-fetch-agent
  (:doc "answers chunked projected records with an unchanged latch and a throttle")
  (let* ((save-path (make-temp-file "mevedel-agent-artifact-" t))
         (artifact-dir
          (mevedel-session-artifacts-artifacts-dir save-path))
         (artifact-path (file-name-concat artifact-dir "agent.html"))
         (root-artifact-path (file-name-concat artifact-dir "root.html"))
         (buffer (generate-new-buffer " *agent-fetch*"))
         (record (mevedel-agent-record--create
                  :path "/root/worker-1" :conversation-buffer buffer))
         (session (mevedel-session--create
                   :name "s"
                   :save-path save-path
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
            (should (<= (string-bytes (mevedel-collaboration--json-string
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


(provide 'test-mevedel-collaboration-agent)
;;; test-mevedel-collaboration-agent.el ends here
