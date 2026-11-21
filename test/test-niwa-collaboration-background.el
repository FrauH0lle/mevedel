;;; test-niwa-collaboration-background.el --- Background panel tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Host background frames and typed control authority.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-agent)
(require 'niwa-agent-control)
(require 'niwa-sandbox)
(require 'niwa-view-execution)
(require 'niwa-collaboration-background nil t)
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-execution-test-helpers"))

(niwa-deftest niwa-collaboration-background-frame
  (:doc "Uses native ordering and bounded three-line output with model facts")
  (let* ((record (niwa-agent-record--create
                  :path "/root/child" :activity 'running
                  :model-metadata '(:backend "Provider" :model "child-model" :effort "high")))
         (session (niwa-session--create :name "background"
                                          :agent-registry (list (cons "/root/child" record))))
         (room (list :session session))
         (live (list (list :kind 'monitor :owner "/root/child" :execution-id "exec-1"
                           :yielded t :started-at 20 :state 'running
                           :description "Watch tests" :output-bytes 999 :output-lines 4
                           :output-tail (concat "old\n" (make-string 2000 ?x) "\nnext\nlast\n")
                           :artifact-path "/private/spool"))))
    (cl-letf (((symbol-function 'niwa-view-execution--live) (lambda (_) live)))
      (let* ((frame (niwa-collaboration--background-frame room))
             (rows (plist-get frame :rows))
             (row (aref rows 0)))
        (should (equal "background-work" (plist-get frame :t)))
        (should (= 2 (length rows)))
        (should (equal "monitor" (plist-get row :kind)))
        (should (equal "child-model" (plist-get row :model)))
        (should (equal "high" (plist-get (aref rows 1) :effort)))
        (should (equal "exec-1" (plist-get row :id)))
        (should (= 0 (plist-get row :order)))
        (should (= 3 (length (plist-get row :preview))))
        (should (<= (length (aref (plist-get row :preview) 0)) 512))
        (should (eq t (plist-get row :previewTruncated)))
        (should-not (plist-get row :outputPath))
        (should-not (string-match-p "/private/spool" (json-encode frame)))
        (should (equal "Provider" (plist-get (aref rows 1) :backend)))))))

(niwa-deftest niwa-collaboration-background-terminal
  (:doc "Terminal events survive registry removal and share native stable order")
  (let* ((session (niwa-session--create :name "terminal"))
         (room (list :session session))
         (live (list (list :kind 'bash :owner "/root" :execution-id "new"
                           :yielded t :state 'running :started-at 30 :output-tail "")))
         scheduled)
    (cl-letf (((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'niwa-collaboration--schedule-publish)
               (lambda (_) (setq scheduled t)))
              ((symbol-function 'niwa-view-execution--live) (lambda (_) live)))
      ;; First seen while live: this captures start and identity before terminal removal.
      (push (list :kind 'bash :owner "/root" :execution-id "old" :yielded t
                  :state 'running :started-at 10 :output-tail "before") live)
      (niwa-collaboration--background-frame room)
      (setq live (cdr live))
      (niwa-collaboration--background-event
       (list :type 'terminal :session session :owner "/root"
             :facts '(:kind bash :execution-id "old" :state completed :output-bytes 5
                      :output-lines 1 :termination exited :exit-code 0)
             :whole-output "after"))
      (should scheduled)
      (let* ((rows (plist-get (niwa-collaboration--background-frame room) :rows))
             (terminal (aref rows 1)))
        (should (equal '("new" "old") (mapcar (lambda (r) (plist-get r :id)) rows)))
        (should (eq t (plist-get terminal :terminal)))
        (should (= 10.0 (plist-get terminal :startedAt)))
        (should (equal ["after" "" ""] (plist-get terminal :preview)))))))

(niwa-deftest niwa-collaboration-background-stop
  (:doc "Dispatch admits full/owner, refuses viewers and stale or forged identities")
  (let* ((session (niwa-session--create :name "stop"))
         (room (list :session session))
         (guest (list :writable nil))
         (live (list '(:owner "/root/child" :execution-id "exec-1" :yielded t)))
         stopped)
    (cl-letf (((symbol-function 'niwa-collaboration--guest) (lambda (&rest _) guest))
              ((symbol-function 'niwa-view-execution--live) (lambda (_) live))
              ((symbol-function 'niwa-execution-stop-user)
               (lambda (s id) (push (list s id) stopped))))
      (let ((frame '(:t "background-stop" :owner "/root/child" :id "exec-1")))
        (should-error (niwa-collaboration--dispatch-frame room 1 frame) :type 'user-error)
        (should-not stopped)
        (setq guest (list :writable t))
        (niwa-collaboration--dispatch-frame room 1 frame)
        (should (equal (list (list session "exec-1")) stopped))
        (setq guest (list :writable t :owner t))
        (niwa-collaboration--dispatch-frame room 1 frame)
        (should (= 2 (length stopped)))
        (should-error (niwa-collaboration--dispatch-frame room 1
                       '(:t "background-stop" :owner "/root/other" :id "exec-1")))
        (should-error (niwa-collaboration--dispatch-frame room 1
                       '(:t "background-stop" :owner "/root/child" :id "exec-1" :key "forged")))
        (setq live nil)
        (should-error (niwa-collaboration--dispatch-frame room 1 frame))
        (should (= 2 (length stopped)))))))

(niwa-deftest niwa-collaboration-background-agent-stop
  (:doc "Only a known retained child can be interrupted, never root or another room")
  (let* ((record (niwa-agent-record--create :path "/root/child" :activity 'running))
         (session (niwa-session--create :name "agents" :agent-registry
                                          (list (cons "/root/child" record))))
         (room (list :session session))
         interrupted)
    (cl-letf (((symbol-function 'niwa-collaboration--guest)
               (lambda (&rest _) '(:writable t)))
              ((symbol-function 'niwa-agent-control-interrupt-user)
               (lambda (s path) (push (list s path) interrupted))))
      (niwa-collaboration--handle-background-stop room 1 '(:path "/root/child"))
      (should (equal (list (list session "/root/child")) interrupted))
      (dolist (frame '((:path "/root") (:path "/root/foreign")
                       (:path "/root/child" :id "ambiguous")))
        (should-error (niwa-collaboration--handle-background-stop room 1 frame)))
      (should (= 1 (length interrupted))))))

(niwa-deftest niwa-collaboration-background-publish
  (:doc "Publishes only changed frames including empty and pads unavailable output")
  (let* ((session (niwa-session--create :name "publish"))
         (room (list :session session))
         (live (list '(:kind bash :owner "/root" :execution-id "one" :yielded t
                       :state running :started-at 1 :output-bytes 8)))
         sent)
    (cl-letf (((symbol-function 'niwa-view-execution--live) (lambda (_) live))
              ((symbol-function 'niwa-collaboration--broadcast)
               (lambda (_ frame) (push frame sent))))
      (niwa-collaboration--publish-background room)
      (niwa-collaboration--publish-background room)
      (should (= 1 (length sent)))
      (let ((row (aref (plist-get (car sent) :rows) 0)))
        (should (eq :json-false (plist-get row :previewAvailable)))
        (should (equal ["Output unavailable." "" ""] (plist-get row :preview))))
      (setq live nil)
      (niwa-collaboration--publish-background room)
      (should (equal [] (plist-get (car sent) :rows))))))

(niwa-deftest niwa-collaboration-background-terminal-metadata
  (:doc "Retains the completed owner's effective model and prunes expired terminal facts")
  (let* ((record (niwa-agent-record--create
                  :path "/root/child" :activity 'running
                  :model-metadata '(:backend "Provider" :model "old-model" :effort "high")))
         (session (niwa-session--create :name "metadata"
                                          :agent-registry (list (cons "/root/child" record))))
         (room (list :session session))
         (now 100.0)
         (live (list '(:kind bash :owner "/root/child" :execution-id "old"
                       :yielded t :started-at 10 :state running :output-tail ""))))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
              ((symbol-function 'niwa-view-execution--live) (lambda (_) live))
              ((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'niwa-collaboration--schedule-publish) #'ignore))
      (niwa-collaboration--background-frame room)
      (setq live nil)
      (setf (niwa-agent-record-activity record) 'idle
            (niwa-agent-record-settled-outcome record) 'completed)
      (niwa-collaboration--background-event
       (list :type 'terminal :session session :owner "/root/child"
             :facts '(:kind bash :execution-id "old" :state completed)
             :whole-output "done"))
      (setf (niwa-agent-record-model-metadata record) '(:model "new-model"))
      (let ((rows (plist-get (niwa-collaboration--background-frame room) :rows)))
        (should (= 2 (length rows)))
        (dolist (row (append rows nil))
          (should (eq t (plist-get row :terminal)))
          (should (equal "old-model" (plist-get row :model)))))
      (setq now (+ now niwa-collaboration--background-terminal-seconds 1))
      (should (equal [] (plist-get (niwa-collaboration--background-frame room) :rows)))
      (should-not (plist-get room :background-terminal)))))

(niwa-deftest niwa-collaboration-background-bounds
  (:doc "Bounds terminal and wire rows, prioritizes live work and keeps native ordering")
  (let* ((session (niwa-session--create :name "bounds"))
         (room (list :session session))
         (niwa-collaboration--background-row-limit 3)
         (live (list '(:kind bash :owner "/root" :execution-id "live"
                       :yielded t :started-at 1 :state running :output-tail ""))))
    (cl-letf (((symbol-function 'niwa-view-execution--live) (lambda (_) live))
              ((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'niwa-collaboration--schedule-publish) #'ignore))
      (dotimes (index 5)
        (niwa-collaboration--background-event
         (list :type 'terminal :session session :owner "/root" :started-at (+ 2 index)
               :facts (list :kind 'bash :execution-id (format "e%d" index) :state 'completed)
               :whole-output "")))
      (should (= 3 (length (plist-get room :background-terminal))))
      (let* ((frame (niwa-collaboration--background-frame room))
             (ids (mapcar (lambda (row) (plist-get row :id)) (plist-get frame :rows))))
        (should (equal '("e4" "e3" "live") ids))
        (should (eq t (plist-get frame :truncated))))
      (setq live (cl-loop for index below 5
                          collect (list :kind 'bash :owner "/root" :execution-id (format "a%d" index)
                                        :yielded t :started-at (+ 10 index) :state 'running)))
      (let* ((frame (niwa-collaboration--background-frame room))
             (ids (mapcar (lambda (row) (plist-get row :id)) (plist-get frame :rows))))
        (should (equal '("a4" "a3" "a2") ids))))))

(niwa-deftest niwa-collaboration-background-real-execution
  (:doc "Real managed process events feed the frame and typed stop preserves terminal evidence")
  (let* ((root (make-temp-file "niwa-background-host-" t))
         (session (test-niwa-execution--session root))
         (foreign (test-niwa-execution--session root))
         (room (list :session session))
         (niwa-sandbox-mode 'off)
         (niwa-execution-mailbox-delivery-function (lambda (_event _context ack) (funcall ack t nil) t))
         (niwa-execution-event-functions (list #'niwa-collaboration--background-event))
         initial id started)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--room-for-session)
                   (lambda (s) (and (eq s session) room)))
                  ((symbol-function 'niwa-collaboration--schedule-publish) #'ignore)
                  ((symbol-function 'niwa-collaboration--guest)
                   (lambda (&rest _) '(:writable t))))
          (setq initial (test-niwa-execution--start-managed
                         session root '("sh" "-c" "printf 'ready\n'; sleep 30")
                         :owner "/root/child")
                id (plist-get (plist-get initial :facts) :execution-id))
          (should-not (plist-get initial :error))
          (should (stringp id))
          (test-niwa-execution--wait
           (lambda () (> (or (plist-get (car (niwa-execution-list-user session)) :output-bytes) 0) 0)))
          (let* ((frame (niwa-collaboration--background-frame room))
                 (row (aref (plist-get frame :rows) 0)))
            (setq started (plist-get row :startedAt))
            (should (> started 0))
            (should (equal "ready" (aref (plist-get row :preview) 0)))
            (should-error
             (niwa-collaboration--dispatch-frame
              (list :session foreign) 1 (list :t "background-stop" :owner "/root/child" :id id)))
            (should (= 1 (niwa-execution-count-user session)))
            (niwa-collaboration--dispatch-frame
             room 1 (list :t "background-stop" :owner "/root/child" :id id :key (plist-get row :key))))
          (test-niwa-execution--wait
           (lambda () (and (zerop (niwa-execution-count-user session))
                           (plist-get room :background-terminal))))
          (let ((row (aref (plist-get (niwa-collaboration--background-frame room) :rows) 0)))
            (should (eq t (plist-get row :terminal)))
            (should (= started (plist-get row :startedAt)))
            (should (equal "stopped" (plist-get row :termination)))
            (should (equal "ready" (aref (plist-get row :preview) 0))))
          (should-error
           (niwa-collaboration--dispatch-frame room 1
            (list :t "background-stop" :owner "/root/child" :id id))))
      (niwa-execution-teardown-session session)
      (niwa-execution-teardown-session foreign)
      (delete-directory root t))))

(niwa-deftest niwa-collaboration-background-wire-budget
  (:doc "An encoded byte budget bounds many long output rows without dropping live work first")
  (let* ((session (niwa-session--create :name "wire-budget"))
         (room (list :session session))
         (live (cl-loop for index below 100
                        collect (list :kind 'bash :owner "/root" :execution-id (format "e%d" index)
                                      :yielded t :started-at index :state 'running
                                      :description (make-string 1024 ?\")
                                      :output-tail (mapconcat #'identity
                                                              (make-list 3 (make-string 512 ?界)) "\n")))))
    (cl-letf (((symbol-function 'niwa-view-execution--live) (lambda (_) live)))
      (let ((frame (niwa-collaboration--background-frame room)))
        (should (<= (string-bytes (json-encode frame)) (* 64 1024)))
        (should (eq t (plist-get frame :truncated)))
        (should (> (length (plist-get frame :rows)) 0))))))

(niwa-deftest niwa-collaboration-background-status-label
  (:doc "Uses native lifecycle labels and discloses preview/output truncation")
  (let* ((session (niwa-session--create :name "labels"))
         (room (list :session session))
         (live nil))
    (cl-letf (((symbol-function 'niwa-view-execution--live) (lambda (_) live))
              ((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'niwa-collaboration--schedule-publish) #'ignore))
      (niwa-collaboration--background-event
       (list :type 'terminal :session session :owner "/root" :started-at 1
             :facts '(:kind bash :execution-id "failed" :state completed :termination exited
                      :exit-code 1 :outcome failure :omitted-output-bytes 20)
             :whole-output "tail"))
      (let ((row (aref (plist-get (niwa-collaboration--background-frame room) :rows) 0)))
        (should (equal "Completed (exit 1)" (plist-get row :statusLabel)))
        (should (equal "failure" (plist-get row :outcome)))
        (should (string-match-p "output was truncated" (plist-get row :previewLabel))))
      (setq live (list '(:kind monitor :owner "/root" :execution-id "monitor"
                         :yielded t :started-at 2 :state running :output-tail "1\n2\n3\n4")))
      (let ((row (aref (plist-get (niwa-collaboration--background-frame room) :rows) 0)))
        (should (equal "Watching" (plist-get row :statusLabel)))
        (should (string-match-p "truncated" (plist-get row :previewLabel)))))))

(niwa-deftest niwa-collaboration-background-terminal-age
  (:doc "JSON carries original terminal age for fresh joins without restarting settled agent clocks")
  (let* ((record (niwa-agent-record--create :path "/root/child" :activity 'running))
         (session (niwa-session--create :name "age" :agent-registry
                                          (list (cons "/root/child" record))))
         (room (list :session session))
         (now 100.0)
         (live (list '(:kind bash :owner "/root" :execution-id "old"
                       :yielded t :started-at 10 :state running :output-tail "")))
         (event (list :type 'terminal :session session :owner "/root"
                      :facts '(:kind bash :execution-id "old" :state completed)
                      :whole-output "done")))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
              ((symbol-function 'niwa-view-execution--live) (lambda (_) live))
              ((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'niwa-collaboration--schedule-publish) #'ignore))
      (let ((frame (niwa-collaboration--background-frame room)))
        (dolist (row (append (plist-get frame :rows) nil))
          (should-not (plist-member row :terminalAgeSeconds))))
      (setq live nil)
      (setf (niwa-agent-record-activity record) 'idle
            (niwa-agent-record-settled-outcome record) 'completed)
      (niwa-collaboration--background-event event)
      (niwa-collaboration--background-frame room)
      (setq now 125.0)
      ;; Duplicate evidence and repeated reads must not mint new completion times.
      (niwa-collaboration--background-event event)
      (dotimes (_ 2)
        (let* ((wire (json-parse-string
                      (json-encode (niwa-collaboration--background-frame room))
                      :object-type 'plist :array-type 'list))
               (rows (plist-get wire :rows)))
          (should (= 2 (length rows)))
          (dolist (row rows)
            (let ((age (plist-get row :terminalAgeSeconds)))
              (should (numberp age))
              (should (= 25.0 age))
              ;; A new browser has no remaining native ten-second visible grace.
              (should (zerop (max 0 (- 10000 (* 1000 age))))))))))))

(niwa-deftest niwa-collaboration-background-terminal-age-publish
  (:doc "Elapsed terminal age alone does not publish an otherwise unchanged panel")
  (let* ((session (niwa-session--create :name "age-publish"))
         (room (list :session session))
         (now 100.0)
         sent)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
              ((symbol-function 'niwa-view-execution--live) (lambda (_) nil))
              ((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'niwa-collaboration--schedule-publish) #'ignore)
              ((symbol-function 'niwa-collaboration--broadcast)
               (lambda (_ frame) (push frame sent))))
      (niwa-collaboration--background-event
       (list :type 'terminal :session session :owner "/root"
             :facts '(:kind bash :execution-id "old" :state completed) :whole-output "done"))
      (niwa-collaboration--publish-background room)
      (setq now 105.0)
      (niwa-collaboration--publish-background room)
      (should (= 1 (length sent)))
      (setq now 131.0)
      (niwa-collaboration--publish-background room)
      (should (= 2 (length sent)))
      (should (equal [] (plist-get (car sent) :rows))))))

(niwa-deftest niwa-collaboration-background-live-output
  (:doc "Full output reads real running root/child Bash and Monitor with source-backed authority")
  (dolist (case '(("/root" bash "tool-root")
                  ("/root/child" bash "tool-child/0")
                  ("/root" monitor "tool-monitor")))
    (let* ((root (make-temp-file "niwa-background-output-" t))
           (session (test-niwa-execution--session root))
           (foreign (niwa-session--create :name "foreign"))
           (parent (generate-new-buffer " *background output parent*"))
           (child (generate-new-buffer " *background output child*"))
           (owner (nth 0 case))
           (kind (nth 1 case))
           (tool-id (nth 2 case))
           (source-id (car (split-string tool-id "/")))
           (buffer (if (equal owner "/root") parent child))
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer parent :transport 'transport
                       :guests guests))
           (niwa-sandbox-mode 'off)
           (niwa-execution-mailbox-delivery-function (lambda (_event _context ack) (funcall ack t nil) t))
           observation id sent)
      (puthash 1 (list :ready t :writable nil) guests)
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/child"
                        (niwa-agent-record--create
                         :path "/root/child" :conversation-buffer child))))
      (unwind-protect
          (progn
            (dolist (data (list parent child))
              (with-current-buffer data
                (setq-local niwa--session session)))
            (with-current-buffer buffer
              (insert (propertize "running source\n" 'gptel (cons 'tool source-id))))
            (niwa-execution-start-bash
             (lambda (value) (setq observation value))
             :session session :data-buffer buffer :owner owner
             :command '("sh" "-c" "printf 'first\\nsecond\\nthird\\nlast\\n'; sleep 30")
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat (niwa-session-save-path session)
                                                 "tool-results" "executions")
             :tool-use-id tool-id :kind kind :description "Inspect running output"
             :yield-time-ms 10)
            (test-niwa-execution--wait (lambda () observation))
            (should-not (plist-get observation :error))
            (setq id (plist-get (plist-get observation :facts) :execution-id))
            (should (stringp id))
            (test-niwa-execution--wait
             (lambda () (> (or (plist-get (car (niwa-execution-list-user session))
                                          :output-bytes) 0) 0)))
            ;; No terminal breadcrumb exists while the process is still running.
            (with-current-buffer buffer
              (should-not (niwa-transcript-audit-buffer-spans 'execution-breadcrumb)))
            (should (niwa-view-audit-locate-source
                     session buffer tool-id (not (equal owner "/root"))))
            (let ((local (niwa-collaboration--local-execution room owner id)))
              (should (plist-get (car local) :live))
              (should (string-prefix-p "artifact://executions/"
                                       (plist-get (plist-get (car local) :facts) :output-path)))
              (should (equal "first\nsecond\nthird\nlast\n"
                             (niwa-collaboration--retained-execution-output
                              session (plist-get (car local) :facts)))))
            (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                       (lambda (_transport _peer frame) (setq sent frame) t)))
              (niwa-collaboration--handle-execution-result-get
               room 1 (list :reqId 1 :owner owner :executionId id
                            :outputPath "artifact://unrelated"))
              (should (equal "live" (plist-get sent :source)))
              (should (equal "first\nsecond\nthird\nlast\n" (plist-get sent :output)))
              (should-not (plist-get sent :error))
              (should-not (plist-get sent :truncated))
              ;; Registry membership alone is insufficient without the source row.
              (with-current-buffer buffer (erase-buffer))
              (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
              (niwa-collaboration--handle-execution-result-get
               room 1 (list :reqId 2 :owner owner :executionId id))
              (should (equal "missing" (plist-get sent :source)))
              (should (equal "" (plist-get sent :output)))
              (with-current-buffer buffer
                (insert (propertize "running source\n" 'gptel (cons 'tool source-id))))
              ;; A different owner, unknown ID, or foreign room cannot read it.
              ;; Even a matching source tool ID in the other known transcript
              ;; must not substitute for the execution's registry owner.
              (with-current-buffer (if (eq buffer parent) child parent)
                (insert (propertize "other owner's source\n" 'gptel (cons 'tool source-id))))
              (dolist (request (list (list :owner (if (equal owner "/root") "/root/child" "/root")
                                          :executionId id)
                                     (list :owner "/root/unknown" :executionId id)
                                     (list :owner owner :executionId "unknown")))
                (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
                (niwa-collaboration--handle-execution-result-get
                 room 1 (append '(:reqId 3) request))
                (should (equal "missing" (plist-get sent :source)))
                (should (equal "" (plist-get sent :output))))
              (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
              (niwa-collaboration--handle-execution-result-get
               (list :session foreign :data-buffer parent :transport 'transport :guests guests)
               1 (list :reqId 4 :owner owner :executionId id))
              (should (equal "missing" (plist-get sent :source)))
              (should (equal "" (plist-get sent :output)))
              (when (equal owner "/root/child")
                (setf (niwa-session-agent-registry session) nil)
                (plist-put (gethash 1 guests) :last-execution-result-fetch nil)
                (niwa-collaboration--handle-execution-result-get
                 room 1 (list :reqId 5 :owner owner :executionId id))
                (should (equal "missing" (plist-get sent :source))))))
        (niwa-execution-teardown-session session)
        (kill-buffer parent)
        (kill-buffer child)
        (delete-directory root t)))))

(niwa-deftest niwa-collaboration-background-retained-output
  (:doc "Terminal artifact output beats inline fallback, stays bounded and releases resource attempts")
  (let* ((root (make-temp-file "niwa-background-retained-" t))
         (session (test-niwa-execution--session root))
         (buffer (generate-new-buffer " *background retained output*"))
         (directory (file-name-concat (niwa-session-save-path session) "tool-results" "executions"))
         (path (file-name-concat directory "execution-retained.log"))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :data-buffer buffer :transport 'transport :guests guests))
         (output (concat "canonical\n" (make-string 60000 ?x)))
         sent)
    (puthash 1 (list :ready t :writable nil) guests)
    (unwind-protect
        (progn
          (make-directory directory t)
          (write-region output nil path nil 'silent)
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (insert (niwa--format-hook-audit-record
                     '(:type execution-breadcrumb :owner "/root"
                       :execution-id "retained" :tool-use-id "tool-retained"
                       :facts (:execution-id "retained" :state completed
                               :output-path "artifact://executions/execution-retained.log"
                               :execution-output "inline fallback")))))
          (let ((attempts (hash-table-count niwa-resource--attempt-table)))
            (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                       (lambda (_transport _peer frame) (setq sent frame) t)))
              (niwa-collaboration--handle-execution-result-get
               room 1 '(:reqId 1 :owner "/root" :executionId "retained")))
            (should (= attempts (hash-table-count niwa-resource--attempt-table))))
          (should (equal "child" (plist-get sent :source)))
          (should (= 50000 (string-bytes (plist-get sent :output))))
          (should (string-prefix-p "canonical\n" (plist-get sent :output)))
          (should (string-suffix-p "\n[truncated]" (plist-get sent :output)))
          (should (plist-get sent :truncated))
          (should-not (plist-get sent :error)))
      (kill-buffer buffer)
      (delete-directory root t))))

(provide 'test-niwa-collaboration-background)
;;; test-niwa-collaboration-background.el ends here
