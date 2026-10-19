;;; test-mevedel-collaboration-background.el --- Background panel tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Host background frames and typed control authority.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-agent-control)
(require 'mevedel-sandbox)
(require 'mevedel-view-execution)
(require 'mevedel-collaboration-background nil t)
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-execution-test-helpers"))

(mevedel-deftest mevedel-collaboration-background-frame
  (:doc "Uses native ordering and bounded three-line output with model facts")
  (let* ((record (mevedel-agent-record--create
                  :path "/root/child" :activity 'running
                  :model-metadata '(:backend "Provider" :model "child-model" :effort "high")))
         (session (mevedel-session--create :name "background"
                                          :agent-registry (list (cons "/root/child" record))))
         (room (list :session session))
         (live (list (list :kind 'monitor :owner "/root/child" :execution-id "exec-1"
                           :yielded t :started-at 20 :state 'running
                           :description "Watch tests" :output-bytes 999 :output-lines 4
                           :output-tail (concat "old\n" (make-string 2000 ?x) "\nnext\nlast\n")
                           :artifact-path "/private/spool"))))
    (cl-letf (((symbol-function 'mevedel-view-execution--live) (lambda (_) live)))
      (let* ((frame (mevedel-collaboration--background-frame room))
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

(mevedel-deftest mevedel-collaboration-background-terminal
  (:doc "Terminal events survive registry removal and share native stable order")
  (let* ((session (mevedel-session--create :name "terminal"))
         (room (list :session session))
         (live (list (list :kind 'bash :owner "/root" :execution-id "new"
                           :yielded t :state 'running :started-at 30 :output-tail "")))
         scheduled)
    (cl-letf (((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--schedule-publish)
               (lambda (_) (setq scheduled t)))
              ((symbol-function 'mevedel-view-execution--live) (lambda (_) live)))
      ;; First seen while live: this captures start and identity before terminal removal.
      (push (list :kind 'bash :owner "/root" :execution-id "old" :yielded t
                  :state 'running :started-at 10 :output-tail "before") live)
      (mevedel-collaboration--background-frame room)
      (setq live (cdr live))
      (mevedel-collaboration--background-event
       (list :type 'terminal :session session :owner "/root"
             :facts '(:kind bash :execution-id "old" :state completed :output-bytes 5
                      :output-lines 1 :termination exited :exit-code 0)
             :whole-output "after"))
      (should scheduled)
      (let* ((rows (plist-get (mevedel-collaboration--background-frame room) :rows))
             (terminal (aref rows 1)))
        (should (equal '("new" "old") (mapcar (lambda (r) (plist-get r :id)) rows)))
        (should (eq t (plist-get terminal :terminal)))
        (should (= 10.0 (plist-get terminal :startedAt)))
        (should (equal ["after" "" ""] (plist-get terminal :preview)))))))

(mevedel-deftest mevedel-collaboration-background-stop
  (:doc "Dispatch admits full/owner, refuses viewers and stale or forged identities")
  (let* ((session (mevedel-session--create :name "stop"))
         (room (list :session session))
         (guest (list :writable nil))
         (live (list '(:owner "/root/child" :execution-id "exec-1" :yielded t)))
         stopped)
    (cl-letf (((symbol-function 'mevedel-collaboration--guest) (lambda (&rest _) guest))
              ((symbol-function 'mevedel-view-execution--live) (lambda (_) live))
              ((symbol-function 'mevedel-execution-stop-user)
               (lambda (s id) (push (list s id) stopped))))
      (let ((frame '(:t "background-stop" :owner "/root/child" :id "exec-1")))
        (should-error (mevedel-collaboration--dispatch-frame room 1 frame) :type 'user-error)
        (should-not stopped)
        (setq guest (list :writable t))
        (mevedel-collaboration--dispatch-frame room 1 frame)
        (should (equal (list (list session "exec-1")) stopped))
        (setq guest (list :writable t :owner t))
        (mevedel-collaboration--dispatch-frame room 1 frame)
        (should (= 2 (length stopped)))
        (should-error (mevedel-collaboration--dispatch-frame room 1
                       '(:t "background-stop" :owner "/root/other" :id "exec-1")))
        (should-error (mevedel-collaboration--dispatch-frame room 1
                       '(:t "background-stop" :owner "/root/child" :id "exec-1" :key "forged")))
        (setq live nil)
        (should-error (mevedel-collaboration--dispatch-frame room 1 frame))
        (should (= 2 (length stopped)))))))

(mevedel-deftest mevedel-collaboration-background-agent-stop
  (:doc "Only a known retained child can be interrupted, never root or another room")
  (let* ((record (mevedel-agent-record--create :path "/root/child" :activity 'running))
         (session (mevedel-session--create :name "agents" :agent-registry
                                          (list (cons "/root/child" record))))
         (room (list :session session))
         interrupted)
    (cl-letf (((symbol-function 'mevedel-collaboration--guest)
               (lambda (&rest _) '(:writable t)))
              ((symbol-function 'mevedel-agent-control-interrupt-user)
               (lambda (s path) (push (list s path) interrupted))))
      (mevedel-collaboration--handle-background-stop room 1 '(:path "/root/child"))
      (should (equal (list (list session "/root/child")) interrupted))
      (dolist (frame '((:path "/root") (:path "/root/foreign")
                       (:path "/root/child" :id "ambiguous")))
        (should-error (mevedel-collaboration--handle-background-stop room 1 frame)))
      (should (= 1 (length interrupted))))))

(mevedel-deftest mevedel-collaboration-background-publish
  (:doc "Publishes only changed frames including empty and pads unavailable output")
  (let* ((session (mevedel-session--create :name "publish"))
         (room (list :session session))
         (live (list '(:kind bash :owner "/root" :execution-id "one" :yielded t
                       :state running :started-at 1 :output-bytes 8)))
         sent)
    (cl-letf (((symbol-function 'mevedel-view-execution--live) (lambda (_) live))
              ((symbol-function 'mevedel-collaboration--broadcast)
               (lambda (_ frame) (push frame sent))))
      (mevedel-collaboration--publish-background room)
      (mevedel-collaboration--publish-background room)
      (should (= 1 (length sent)))
      (let ((row (aref (plist-get (car sent) :rows) 0)))
        (should (eq :json-false (plist-get row :previewAvailable)))
        (should (equal ["Output unavailable." "" ""] (plist-get row :preview))))
      (setq live nil)
      (mevedel-collaboration--publish-background room)
      (should (equal [] (plist-get (car sent) :rows))))))

(mevedel-deftest mevedel-collaboration-background-terminal-metadata
  (:doc "Retains the completed owner's effective model and prunes expired terminal facts")
  (let* ((record (mevedel-agent-record--create
                  :path "/root/child" :activity 'running
                  :model-metadata '(:backend "Provider" :model "old-model" :effort "high")))
         (session (mevedel-session--create :name "metadata"
                                          :agent-registry (list (cons "/root/child" record))))
         (room (list :session session))
         (now 100.0)
         (live (list '(:kind bash :owner "/root/child" :execution-id "old"
                       :yielded t :started-at 10 :state running :output-tail ""))))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
              ((symbol-function 'mevedel-view-execution--live) (lambda (_) live))
              ((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--schedule-publish) #'ignore))
      (mevedel-collaboration--background-frame room)
      (setq live nil)
      (setf (mevedel-agent-record-activity record) 'idle
            (mevedel-agent-record-settled-outcome record) 'completed)
      (mevedel-collaboration--background-event
       (list :type 'terminal :session session :owner "/root/child"
             :facts '(:kind bash :execution-id "old" :state completed)
             :whole-output "done"))
      (setf (mevedel-agent-record-model-metadata record) '(:model "new-model"))
      (let ((rows (plist-get (mevedel-collaboration--background-frame room) :rows)))
        (should (= 2 (length rows)))
        (dolist (row (append rows nil))
          (should (eq t (plist-get row :terminal)))
          (should (equal "old-model" (plist-get row :model)))))
      (setq now (+ now mevedel-collaboration--background-terminal-seconds 1))
      (should (equal [] (plist-get (mevedel-collaboration--background-frame room) :rows)))
      (should-not (plist-get room :background-terminal)))))

(mevedel-deftest mevedel-collaboration-background-bounds
  (:doc "Bounds terminal and wire rows, prioritizes live work and keeps native ordering")
  (let* ((session (mevedel-session--create :name "bounds"))
         (room (list :session session))
         (mevedel-collaboration--background-row-limit 3)
         (live (list '(:kind bash :owner "/root" :execution-id "live"
                       :yielded t :started-at 1 :state running :output-tail ""))))
    (cl-letf (((symbol-function 'mevedel-view-execution--live) (lambda (_) live))
              ((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--schedule-publish) #'ignore))
      (dotimes (index 5)
        (mevedel-collaboration--background-event
         (list :type 'terminal :session session :owner "/root" :started-at (+ 2 index)
               :facts (list :kind 'bash :execution-id (format "e%d" index) :state 'completed)
               :whole-output "")))
      (should (= 3 (length (plist-get room :background-terminal))))
      (let* ((frame (mevedel-collaboration--background-frame room))
             (ids (mapcar (lambda (row) (plist-get row :id)) (plist-get frame :rows))))
        (should (equal '("e4" "e3" "live") ids))
        (should (eq t (plist-get frame :truncated))))
      (setq live (cl-loop for index below 5
                          collect (list :kind 'bash :owner "/root" :execution-id (format "a%d" index)
                                        :yielded t :started-at (+ 10 index) :state 'running)))
      (let* ((frame (mevedel-collaboration--background-frame room))
             (ids (mapcar (lambda (row) (plist-get row :id)) (plist-get frame :rows))))
        (should (equal '("a4" "a3" "a2") ids))))))

(mevedel-deftest mevedel-collaboration-background-real-execution
  (:doc "Real managed process events feed the frame and typed stop preserves terminal evidence")
  (let* ((root (make-temp-file "mevedel-background-host-" t))
         (session (test-mevedel-execution--session root))
         (foreign (test-mevedel-execution--session root))
         (room (list :session session))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-mailbox-delivery-function (lambda (&rest _) t))
         (mevedel-execution-event-functions (list #'mevedel-collaboration--background-event))
         initial id started)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--room-for-session)
                   (lambda (s) (and (eq s session) room)))
                  ((symbol-function 'mevedel-collaboration--schedule-publish) #'ignore)
                  ((symbol-function 'mevedel-collaboration--guest)
                   (lambda (&rest _) '(:writable t))))
          (setq initial (test-mevedel-execution--start-managed
                         session root '("sh" "-c" "printf 'ready\n'; sleep 30")
                         :owner "/root/child")
                id (plist-get (plist-get initial :facts) :execution-id))
          (should-not (plist-get initial :error))
          (should (stringp id))
          (test-mevedel-execution--wait
           (lambda () (> (or (plist-get (car (mevedel-execution-list-user session)) :output-bytes) 0) 0)))
          (let* ((frame (mevedel-collaboration--background-frame room))
                 (row (aref (plist-get frame :rows) 0)))
            (setq started (plist-get row :startedAt))
            (should (> started 0))
            (should (equal "ready" (aref (plist-get row :preview) 0)))
            (should-error
             (mevedel-collaboration--dispatch-frame
              (list :session foreign) 1 (list :t "background-stop" :owner "/root/child" :id id)))
            (should (= 1 (mevedel-execution-count-user session)))
            (mevedel-collaboration--dispatch-frame
             room 1 (list :t "background-stop" :owner "/root/child" :id id :key (plist-get row :key))))
          (test-mevedel-execution--wait
           (lambda () (and (zerop (mevedel-execution-count-user session))
                           (plist-get room :background-terminal))))
          (let ((row (aref (plist-get (mevedel-collaboration--background-frame room) :rows) 0)))
            (should (eq t (plist-get row :terminal)))
            (should (= started (plist-get row :startedAt)))
            (should (equal "stopped" (plist-get row :termination)))
            (should (equal "ready" (aref (plist-get row :preview) 0))))
          (should-error
           (mevedel-collaboration--dispatch-frame room 1
            (list :t "background-stop" :owner "/root/child" :id id))))
      (mevedel-execution-teardown-session session)
      (mevedel-execution-teardown-session foreign)
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration-background-wire-budget
  (:doc "An encoded byte budget bounds many long output rows without dropping live work first")
  (let* ((session (mevedel-session--create :name "wire-budget"))
         (room (list :session session))
         (live (cl-loop for index below 100
                        collect (list :kind 'bash :owner "/root" :execution-id (format "e%d" index)
                                      :yielded t :started-at index :state 'running
                                      :description (make-string 1024 ?\")
                                      :output-tail (mapconcat #'identity
                                                              (make-list 3 (make-string 512 ?界)) "\n")))))
    (cl-letf (((symbol-function 'mevedel-view-execution--live) (lambda (_) live)))
      (let ((frame (mevedel-collaboration--background-frame room)))
        (should (<= (string-bytes (json-encode frame)) (* 64 1024)))
        (should (eq t (plist-get frame :truncated)))
        (should (> (length (plist-get frame :rows)) 0))))))

(mevedel-deftest mevedel-collaboration-background-status-label
  (:doc "Uses native lifecycle labels and discloses preview/output truncation")
  (let* ((session (mevedel-session--create :name "labels"))
         (room (list :session session))
         (live nil))
    (cl-letf (((symbol-function 'mevedel-view-execution--live) (lambda (_) live))
              ((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--schedule-publish) #'ignore))
      (mevedel-collaboration--background-event
       (list :type 'terminal :session session :owner "/root" :started-at 1
             :facts '(:kind bash :execution-id "failed" :state completed :termination exited
                      :exit-code 1 :outcome failure :omitted-output-bytes 20)
             :whole-output "tail"))
      (let ((row (aref (plist-get (mevedel-collaboration--background-frame room) :rows) 0)))
        (should (equal "Completed (exit 1)" (plist-get row :statusLabel)))
        (should (equal "failure" (plist-get row :outcome)))
        (should (string-match-p "output was truncated" (plist-get row :previewLabel))))
      (setq live (list '(:kind monitor :owner "/root" :execution-id "monitor"
                         :yielded t :started-at 2 :state running :output-tail "1\n2\n3\n4")))
      (let ((row (aref (plist-get (mevedel-collaboration--background-frame room) :rows) 0)))
        (should (equal "Watching" (plist-get row :statusLabel)))
        (should (string-match-p "truncated" (plist-get row :previewLabel)))))))

(mevedel-deftest mevedel-collaboration-background-terminal-age
  (:doc "JSON carries original terminal age for fresh joins without restarting settled agent clocks")
  (let* ((record (mevedel-agent-record--create :path "/root/child" :activity 'running))
         (session (mevedel-session--create :name "age" :agent-registry
                                          (list (cons "/root/child" record))))
         (room (list :session session))
         (now 100.0)
         (live (list '(:kind bash :owner "/root" :execution-id "old"
                       :yielded t :started-at 10 :state running :output-tail "")))
         (event (list :type 'terminal :session session :owner "/root"
                      :facts '(:kind bash :execution-id "old" :state completed)
                      :whole-output "done")))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
              ((symbol-function 'mevedel-view-execution--live) (lambda (_) live))
              ((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--schedule-publish) #'ignore))
      (let ((frame (mevedel-collaboration--background-frame room)))
        (dolist (row (append (plist-get frame :rows) nil))
          (should-not (plist-member row :terminalAgeSeconds))))
      (setq live nil)
      (setf (mevedel-agent-record-activity record) 'idle
            (mevedel-agent-record-settled-outcome record) 'completed)
      (mevedel-collaboration--background-event event)
      (mevedel-collaboration--background-frame room)
      (setq now 125.0)
      ;; Duplicate evidence and repeated reads must not mint new completion times.
      (mevedel-collaboration--background-event event)
      (dotimes (_ 2)
        (let* ((wire (json-parse-string
                      (json-encode (mevedel-collaboration--background-frame room))
                      :object-type 'plist :array-type 'list))
               (rows (plist-get wire :rows)))
          (should (= 2 (length rows)))
          (dolist (row rows)
            (let ((age (plist-get row :terminalAgeSeconds)))
              (should (numberp age))
              (should (= 25.0 age))
              ;; A new browser has no remaining native ten-second visible grace.
              (should (zerop (max 0 (- 10000 (* 1000 age))))))))))))

(mevedel-deftest mevedel-collaboration-background-terminal-age-publish
  (:doc "Elapsed terminal age alone does not publish an otherwise unchanged panel")
  (let* ((session (mevedel-session--create :name "age-publish"))
         (room (list :session session))
         (now 100.0)
         sent)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
              ((symbol-function 'mevedel-view-execution--live) (lambda (_) nil))
              ((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--schedule-publish) #'ignore)
              ((symbol-function 'mevedel-collaboration--broadcast)
               (lambda (_ frame) (push frame sent))))
      (mevedel-collaboration--background-event
       (list :type 'terminal :session session :owner "/root"
             :facts '(:kind bash :execution-id "old" :state completed) :whole-output "done"))
      (mevedel-collaboration--publish-background room)
      (setq now 105.0)
      (mevedel-collaboration--publish-background room)
      (should (= 1 (length sent)))
      (setq now 131.0)
      (mevedel-collaboration--publish-background room)
      (should (= 2 (length sent)))
      (should (equal [] (plist-get (car sent) :rows))))))

(provide 'test-mevedel-collaboration-background)
;;; test-mevedel-collaboration-background.el ends here
