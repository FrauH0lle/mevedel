;;; test-niwa-collaboration-history.el --- Archived browser history -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise archived publication through the same room and fetch boundaries.

;;; Code:
(require 'helpers (file-name-concat (file-name-directory load-file-name) "helpers"))
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-history)
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-task)
(require 'niwa-collaboration-transport)
(require 'niwa-collaboration-artifact)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-workspace-identity)
(require 'niwa-tool-render-data)
(require 'niwa-tool-exec)
(require 'niwa-tool-ptc)
(require 'niwa-transcript-restore)
(require 'niwa-execution-transcript)
(require 'org)

(defmacro niwa-test--raw-segments (&rest body)
  "Run BODY with archive segments served from raw files under the save path.
The fixtures write segments directly instead of publishing generations."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'niwa-session-artifacts-artifact-present-p)
              (lambda (session logical &rest _)
                (file-exists-p
                 (expand-file-name logical (niwa-session-save-path session)))))
             ((symbol-function 'niwa-session-artifacts-find-artifact-noselect)
              (lambda (session logical &rest _)
                (let ((buffer (generate-new-buffer " *raw segment*")))
                  (with-current-buffer buffer
                    (insert-file-contents
                     (expand-file-name logical (niwa-session-save-path session)))
                    (delay-mode-hooks (org-mode)))
                  buffer))))
     ,@body))

(niwa-deftest niwa-collaboration-history-rename-notification
  (:doc "rename schedules the current chat heading as well as room status")
  (let (status history)
    (cl-letf (((symbol-function 'niwa-collaboration-notify-request-changed)
               (lambda (buffer) (setq status buffer)))
              ((symbol-function 'niwa-collaboration-notify-history-changed)
               (lambda (buffer) (setq history buffer))))
      (niwa-collaboration--on-session-event 'data 'rename)
      (should (eq status 'data))
      (should (eq history 'data)))))

(niwa-deftest niwa-collaboration--publish-history
  (:doc "projects canonical clear-separated chats and refreshes their titles")
  (let* ((session (niwa-session--create
                   :name "Current work" :current-segment 4
                   :chapters '((:segment 2 :title "Earlier work" :closed-at 1))))
         (room (list :session session :transport 'test))
         sent)
    (cl-letf (((symbol-function 'niwa-collaboration--broadcast)
               (lambda (_room frame) (push (niwa-test--frame frame) sent))))
      (niwa-collaboration--publish-history room)
      (let ((chats (append (plist-get (car sent) :records) nil)))
        (should (= 2 (length chats)))
        (should (equal "chat" (plist-get (car chats) :kind)))
        (should (equal "Earlier work" (plist-get (car chats) :title)))
        (should (= 1 (plist-get (car chats) :firstSegment)))
        (should (= 2 (plist-get (car chats) :lastSegment)))
        (should (= 3 (plist-get (cadr chats) :firstSegment))))
      (setq sent nil)
      (niwa-collaboration--publish-history room)
      (should-not sent)
      (setf (niwa-session-name session) "Renamed work")
      (niwa-collaboration--publish-history room)
      (should (equal "Renamed work"
                     (plist-get (aref (plist-get (car sent) :records) 1) :title))))))

(niwa-deftest niwa-collaboration-archived-artifacts
  (:doc "publishes and serves archived artifacts after compaction and a cold room start")
  (niwa-test--raw-segments
    (let* ((directory (make-temp-file "niwa-history-" t))
         (artifact (file-name-concat directory ".niwa" "artifacts" "design.html"))
         (session (niwa-session--create :name "history" :save-path directory
                                         :workspace (niwa-workspace--create
                                                     :type 'file :id "w" :root directory)
                                         :authority-mode 'portable :current-segment 2))
         (data (generate-new-buffer " *history live*"))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :data-buffer data :guests guests :transport 'test))
         sent)
    (unwind-protect
        (progn
          (make-directory (file-name-directory artifact) t)
          (write-region "<h1>Design</h1>" nil artifact nil 'silent)
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\nMake a design\n"
                    "#+begin_tool (ApplyPatch :patch \"patch\")\n"
                    "(:name \"ApplyPatch\" :args (:patch \"patch\"))\n\nApplied patch\n"
                    (niwa-tool-render-data-format
                     ;; Model-authored relative paths resolve against the artifact store.
                     '(:kind patch :files ((:kind add :path "design.html" :added 1 :deleted 0 :diff "")))
                     "old-patch")
                    "#+end_tool\n")
            (dotimes (_ 3)
              (goto-char (point-min))
              (search-forward "#+begin_tool")
              (let ((start (match-beginning 0)))
                (search-forward "#+end_tool")
                (org-entry-put (point-min) "GPTEL_BOUNDS"
                               (prin1-to-string `((tool (,start ,(point) "old-patch")))))))
            (write-region (point-min) (point-max)
                          (niwa-session-artifacts-segment-path directory 1) nil 'silent))
          (with-current-buffer data (setq-local niwa--session session))
          (puthash 1 (list :ready t :writable nil) guests)
          (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (_transport _peer frame) (push (niwa-test--frame frame) sent) t)))
            (niwa-collaboration--publish room)
            (let* ((index (cl-find "history-index" sent :key (lambda (f) (plist-get f :t)) :test #'equal))
                   (record (car (append (plist-get index :records) nil))))
              (should (equal "design.html" (plist-get record :artifact)))
              (should-not (plist-member record :artifact-path))
              (let ((id (plist-get record :id)))
                (should (string-prefix-p "history-1-" id))
                (setq sent nil)
                (niwa-collaboration--handle-artifact-get room 1 (list :reqId 1 :id id))
                (should (equal "<h1>Design</h1>" (base64-decode-string (plist-get (car sent) :data))))
              ;; Reconnect/cold publication reconstructs the same authority.
              (let ((cold (list :session session :data-buffer data :guests guests :transport 'test)))
                (should (equal (niwa-collaboration--history-artifacts cold)
                               (niwa-collaboration--history-artifacts room))))
              ;; Routine publishes do not reread archived transcript bodies.
              (cl-letf (((symbol-function 'niwa-session-artifacts-read-segment)
                         (lambda (&rest _) (ert-fail "Archive reread during live publication"))))
                (niwa-collaboration--publish room))
              (setq sent nil)
              (niwa-collaboration--handle-history-get room 999 '(:reqId 2 :segment 1))
              (should-not sent)
              (dolist (number '(0 -1 2 1.5 "../session.meta.el"))
                (setq sent nil)
                (niwa-collaboration--handle-history-get room 1 (list :reqId 2 :segment number))
                (should (plist-get (car sent) :error)))
              (setq sent nil)
              (niwa-collaboration--handle-history-get room 1 '(:reqId 3 :segment 1))
              (should (eq t (plist-get (car sent) :final)))
              (let ((json (json-encode sent)))
                (should (string-match-p "Make a design" json))
                (should (string-match-p "design.html" json))
                (should-not (string-match-p "artifact-path\\|niwa-render-data" json))
                (should-not (string-match-p (regexp-quote directory) json)))
              ;; Fast repeat requests receive an explicit retry response.
              (niwa-collaboration--handle-history-get room 1 '(:reqId 4 :segment 1))
              (should (plist-get (car sent) :error))
              (delete-file artifact)
              (niwa-collaboration--artifact-stat-invalidate)
              (should (plist-get (car (niwa-collaboration--history-artifacts room)) :missing))
              (delete-file (niwa-session-artifacts-segment-path directory 1))
              (plist-put (gethash 1 guests) :last-history-fetch nil)
              (niwa-collaboration--handle-history-get room 1 '(:reqId 5 :segment 1))
              (should (plist-get (car sent) :error))))
          (should-not (plist-get room :records))))
      (kill-buffer data)
      (niwa-collaboration--artifact-stat-invalidate)
      (delete-directory directory t)))))

(niwa-deftest niwa-collaboration--history-artifacts
  (:doc "refreshes archived titles without rereading transcripts or warm metadata")
  (let* ((directory (make-temp-file "niwa-history-titles-" t))
         (workspace (niwa-workspace--create :type 'file :id "w" :root directory))
         (session (niwa-session--create :save-path directory :workspace workspace :current-segment 2))
         (path (file-name-concat (niwa-artifact-store-directory workspace) "page" "index.html"))
         (room (list :session session :history-source (cons directory 2)
                     :history-archives (list (cons 1 (list (list :id "history-1-card"
                                                               :artifact "page/index.html"
                                                               :artifact-path path :store "page"))))))
         (calls 0))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "hello" nil path nil 'silent)
          (niwa-artifact-store-create-meta workspace "page" "index.html" 'html "Initial")
          (cl-letf (((symbol-function 'niwa-session-artifacts-read-segment)
                     (lambda (&rest _) (ert-fail "Archive reread during title publication"))))
            (should (equal "Initial" (plist-get (car (niwa-collaboration--history-artifacts room)) :title)))
            (niwa-collaboration--artifact-stat-invalidate)
            (cl-letf (((symbol-function 'niwa-artifact-store-meta)
                       (lambda (&rest _) (cl-incf calls) '(:title "Renamed" :file "index.html"))))
              (dotimes (_ 2)
                (should (equal "Renamed" (plist-get (car (niwa-collaboration--history-artifacts room)) :title))))
              (should (= calls 1)))))
      (niwa-collaboration--artifact-stat-invalidate)
      (delete-directory directory t))))

(niwa-deftest niwa-collaboration--handle-history-get
  (:doc "remote callback reads wait for transport and recheck room and guest authority")
  (niwa-test--raw-segments
    (dolist (revocation '(nil guest room))
    (let* ((session (niwa-session--create :name "deferred-history"
                                            :save-path temporary-file-directory :current-segment 2))
           (guest (list :ready t))
           (guests (make-hash-table :test #'eql))
           (room (list :room-id "history" :session session :guests guests))
           (live-room room)
           (niwa-transport--enabled-p t)
           (niwa-transport--pending (make-hash-table :test #'equal))
           sent read)
      (puthash 1 guest guests)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-collaboration--room-for-session)
                     (lambda (_) live-room))
                    ((symbol-function 'niwa-collaboration--history-records)
                     (lambda (&rest _)
                       (should-not (niwa-transport-nested-p))
                       (setq read t)
                       (list '(:id "saved" :text "Archived answer"))))
                    ((symbol-function 'niwa-collaboration--publish-history) #'ignore)
                    ((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (_transport _peer frame) (push (niwa-test--frame frame) sent))))
            (niwa-transport-call-as-remote-operation
             (lambda ()
               (let ((timer-list nil))
                 (niwa-collaboration--handle-history-get room 1 '(:reqId 1 :segment 1))
                 ;; A second request admitted after the rate window must
                 ;; retain its own reply while the same transport stays busy.
                 (plist-put guest :last-history-fetch (- (float-time) 2))
                 (niwa-collaboration--handle-history-get room 1 '(:reqId 2 :segment 1)))))
            (should-not read)
            (should-not sent)
            (pcase revocation ('guest (remhash 1 guests)) ('room (setq live-room nil)))
            ;; Authority is checked after transport becomes idle; this is a
            ;; progress watchdog, not a requirement to reply within one second.
            (let ((deadline (+ (float-time) 5)))
              (while (and (> (hash-table-count niwa-transport--pending) 0)
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (= 0 (hash-table-count niwa-transport--pending)))
            (if revocation
                (progn (should-not read) (should-not sent))
              (should read)
              (should (equal '(1 2) (sort (mapcar (lambda (frame) (plist-get frame :reqId)) sent) #'<)))
              (should (cl-every (lambda (frame) (plist-get frame :final)) sent))))
        (niwa-transport-cancel-pending))))))

(niwa-deftest niwa-collaboration--history-records-bash-completion
  (:doc "later hidden terminal evidence updates the original archived Bash row")
  (niwa-test--raw-segments
    (let* ((directory (make-temp-file "niwa-history-bash-" t))
         (session (niwa-session--create :name "history-bash" :save-path directory
                                          :authority-mode 'portable :current-segment 2))
         (data (generate-new-buffer " *history bash live*"))
         (room (list :session session :data-buffer data))
         (niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (unwind-protect
        (progn
          (niwa-tool-register
           (niwa-tool--create :name "Bash" :category "niwa"
                                 :renderer #'niwa-tool-exec--render-bash))
          (with-temp-buffer
            (delay-mode-hooks (org-mode))
            (niwa-transcript-enable-gptel-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"exit 1\"))\n\ninitial")
              (put-text-property start (point) 'gptel '(tool . "bash-hist"))
              (insert (niwa-tool-render-data-format
                       '(:execution-id "exec-hist" :command "exit 1"
                         :state running :live-execution-p t)
                       "bash-hist")))
            (insert "\n")
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max)
                          (niwa-session-artifacts-segment-path directory 1)
                          nil 'silent))
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (niwa-transcript-enable-gptel-mode)
            (setq-local niwa--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (niwa--format-hook-audit-record
                     '(:type execution-archive :tool-use-id "bash-hist"
                       :render-data (:execution-id "exec-hist" :command "exit 1"
                                     :state running :live-execution-p t))))
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "bash-hist"
                       :render-data (:execution-id "exec-hist" :command "exit 1"
                                     :state completed :status error :outcome failure
                                     :exit-code 1 :execution-output "initial\nfinal"))))
            (let ((live (niwa-collaboration--canonical-records data)))
              (should-not live)))
          (let ((history (niwa-collaboration--history-records room 1)))
            (should (= 1 (length history)))
            (should (equal "history-1-tool-bash-hist"
                           (plist-get (car history) :id)))
            (should (equal "failed" (plist-get (car history) :status)))
            (should (equal "initial\nfinal" (plist-get (car history) :result))))
          (niwa-tool-register
           (niwa-tool--create :name "ToolCall" :category "niwa"
                                 :renderer #'niwa-tool-ptc--render))
          (with-temp-buffer
            (delay-mode-hooks (org-mode))
            (niwa-transcript-enable-gptel-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (let ((start (point)))
              (insert "(:name \"ToolCall\" :args (:expression \"Bash\"))\nraw model result")
              (put-text-property start (point) 'gptel '(tool . "outer"))
              (insert (niwa-tool-render-data-format
                       '(:kind ptc :outcome completed
                         :calls ((:id "outer/1" :tool "Bash" :status success
                                  :args (:command "exit 1") :result "initial"
                                  :render-data (:execution-id "exec-child"
                                                :state running))))
                       "outer")))
            (insert "\n")
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max)
                          (niwa-session-artifacts-segment-path directory 1)
                          nil 'silent))
          (with-current-buffer data
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "outer/1"
                       :render-data (:execution-id "exec-child" :command "exit 1"
                                     :state completed :status error :outcome failure
                                     :exit-code 1 :execution-output "initial\nchild final")))))
          (let* ((history (niwa-collaboration--history-records room 1))
                 (record (car history))
                 (child (aref (plist-get (plist-get record :presentation) :children)
                              0)))
            (should (= 1 (length history)))
            (should (equal "raw model result" (plist-get record :result)))
            (should (equal "failed" (plist-get child :status)))
            (should (string-match-p "child final" (plist-get child :body))))
          (with-current-buffer data
            (let ((plan (niwa-execution-transcript-prepare-archive
                         data '("outer/1"))))
              (should (= 1 (length (plist-get plan :completed))))
              (niwa-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max)
                            (niwa-session-artifacts-segment-path directory 2)
                            nil 'silent)
              (erase-buffer)
              (insert (niwa-execution-transcript-archive-text plan))
              (setf (niwa-session-current-segment session) 3)
              (should-not (niwa-collaboration--canonical-records data))))
          (let* ((history (niwa-collaboration--history-records room 1))
                 (record (car history))
                 (child (aref (plist-get (plist-get record :presentation) :children)
                              0)))
            (should (= 1 (length history)))
            (should (equal "raw model result" (plist-get record :result)))
            (should (equal "failed" (plist-get child :status)))
            (should (string-match-p "child final" (plist-get child :body))))
          (should-not (niwa-collaboration--history-records room 2)))
      (kill-buffer data)
      (delete-directory directory t)))))

(niwa-deftest niwa-collaboration--history-records-second-compaction
  (:doc "second compaction keeps the original Bash result without duplicate cards")
  (niwa-test--raw-segments
    (let* ((directory (make-temp-file "niwa-history-twice-" t))
         (session (niwa-session--create :name "history-twice"
                                          :save-path directory
                                          :authority-mode 'portable
                                          :current-segment 2))
         (data (generate-new-buffer " *history twice live*"))
         (room (list :session session :data-buffer data))
         (niwa-tool--registry (copy-hash-table niwa-tool--registry))
         first-plan)
    (unwind-protect
        (progn
          (niwa-tool-register
           (niwa-tool--create :name "Bash" :category "niwa"
                                 :renderer #'niwa-tool-exec--render-bash))
          (with-temp-buffer
            (delay-mode-hooks (org-mode))
            (niwa-transcript-enable-gptel-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"exit 1\"))\n\ninitial")
              (put-text-property start (point) 'gptel '(tool . "bash-twice"))
              (insert (niwa-tool-render-data-format
                       '(:execution-id "exec-twice" :command "exit 1"
                         :state running :live-execution-p t)
                       "bash-twice")))
            (insert "\n")
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (setq first-plan (niwa-execution-transcript-prepare-archive
                              (current-buffer) '("bash-twice")))
            (should (= 1 (length (plist-get first-plan :live))))
            (write-region (point-min) (point-max)
                          (niwa-session-artifacts-segment-path directory 1)
                          nil 'silent))
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (niwa-transcript-enable-gptel-mode)
            (setq-local niwa--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (niwa-execution-transcript-archive-text first-plan))
            (niwa-execution-transcript-commit-archive data first-plan)
            (niwa-execution-transcript-handle-event
             (list :type 'terminal :data-buffer data :tool-use-id "bash-twice"
                   :owner "/root" :whole-output "initial\nfinal"
                   :facts '(:execution-id "exec-twice" :command "exit 1"
                            :state completed :outcome failure :exit-code 1)))
            (let ((plan (niwa-execution-transcript-prepare-archive
                         data '("bash-twice"))))
              (should (= 1 (length (plist-get plan :completed))))
              (niwa-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max)
                            (niwa-session-artifacts-segment-path directory 2)
                            nil 'silent)
              (erase-buffer)
              (insert (niwa-execution-transcript-archive-text plan))
              (setf (niwa-session-current-segment session) 3)
              (should-not (niwa-collaboration--canonical-records data))))
          (let* ((history (niwa-collaboration--history-records room 1))
                 (record (car history)))
            (should (= 1 (length history)))
            (should (equal "history-1-tool-bash-twice" (plist-get record :id)))
            (should (equal "failed" (plist-get record :status)))
            (should (equal "initial\nfinal" (plist-get record :result))))
          (let* ((records (niwa-collaboration--history-records room 2))
                 (record (car records)))
            (should (= 1 (length records)))
            (should (equal "execution" (plist-get record :kind)))
            (should (equal "failed" (plist-get record :status)))
            (should-not (plist-member record :result)))
          (with-current-buffer data
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "bash-twice"
                       :render-data (:execution-id "exec-twice" :command "exit 1"
                                     :state completed :status error :outcome failure
                                     :exit-code 2
                                     :execution-output "initial\nfinal\nlater")))))
          (should (equal "initial\nfinal\nlater"
                         (plist-get (car (niwa-collaboration--history-records
                                          room 1)) :result)))
          ;; A missing intermediate archive must not hide completion retained
          ;; by the current segment, nor synthesize another primary output.
          (delete-file (niwa-session-artifacts-segment-path directory 2))
          (let ((record (car (niwa-collaboration--history-records room 1))))
            (should (equal "initial\nfinal\nlater" (plist-get record :result)))
            (should (equal "failed" (plist-get record :status)))
            (should-not (string-match-p
                         "completion unavailable"
                         (plist-get (plist-get record :presentation) :header))))
          (with-current-buffer data
            (erase-buffer)
            (should-not (niwa-collaboration--canonical-records data)))
          (let ((record (car (niwa-collaboration--history-records room 1))))
            (should (equal "warning" (plist-get record :status)))
            (should (string-match-p "completion unavailable"
                                    (plist-get (plist-get record :presentation)
                                               :header)))
            (should (equal "initial" (plist-get record :result))))
          ;; A still-running owner outranks uncertainty from the missing file.
          (cl-letf (((symbol-function 'niwa-execution-list-user)
                     (lambda (_session)
                       '((:execution-id "exec-twice" :state running
                          :output-tail "still working")))))
            (let ((record (car (niwa-collaboration--history-records room 1))))
              (should (equal "running" (plist-get record :status)))
              (should-not (string-match-p "unknown"
                                          (plist-get (plist-get record :presentation)
                                                     :header)))))
          (let ((record (car (niwa-collaboration--history-records
                              (list :session session :data-buffer nil) 1))))
            (should (equal "initial" (plist-get record :result)))))
      (kill-buffer data)
      (delete-directory directory t)))))

(niwa-deftest niwa-collaboration--history-records-compaction
  (:doc "an archived segment projects its leading summary as a compaction record")
  (niwa-test--raw-segments
    (let* ((directory (make-temp-file "niwa-history-compaction-" t))
         (session (niwa-session--create :name "history-compaction"
                                           :save-path directory
                                           :authority-mode 'portable
                                           :current-segment 2)))
    (unwind-protect
        (progn
          (with-temp-file (niwa-session-artifacts-segment-path directory 1)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
                    (niwa-session-artifacts-summary-block "Archived handoff")
                    "\nQuestion\n"))
          (let* ((records (niwa-collaboration--history-records
                           (list :session session :data-buffer nil) 1))
                 (summary (car records)))
            (should (equal "compaction" (plist-get summary :kind)))
            (should (string-prefix-p "history-1-compaction-"
                                     (plist-get summary :id)))
            (should (string-suffix-p "Archived handoff" (plist-get summary :text)))
            (should (equal '("compaction" "user")
                           (mapcar (lambda (record) (plist-get record :kind))
                                   records)))))
      (delete-directory directory t)))))

(niwa-deftest niwa-collaboration-forwarded-execution-compaction-dedup
  (:doc "first readable mailbox owns breadcrumb across archives and current transcript")
  (niwa-test--raw-segments
    (let* ((directory (make-temp-file "niwa-history-execution-" t))
         (session (niwa-session--create :name "history-execution"
                                          :save-path directory :authority-mode 'portable
                                          :current-segment 3))
         (data (generate-new-buffer " *forwarded history live*"))
         (child (generate-new-buffer " *forwarded history child*"))
         (room (list :session session :data-buffer data))
         (mailbox (concat "<agent-message type=\"EXECUTION\" sender=\"/root/child\" "
                          "execution_id=\"exec-duplicate\" command=\"exit 2\" "
                          "outcome=\"failure\" exit_code=\"2\">\n"
                          "retained output\n"
                          "</agent-message>\n"))
         (root-breadcrumb
          (niwa--format-hook-audit-record
           '(:type execution-breadcrumb :owner "/root"
             :execution-id "exec-duplicate" :tool-use-id "outer/1"
             :facts (:execution-id "exec-duplicate" :outcome success))))
         (distinct (replace-regexp-in-string
                    "exec-duplicate" "exec-distinct" mailbox t t)))
    (unwind-protect
        (progn
          (dotimes (index 2)
            (with-temp-buffer
              (delay-mode-hooks (org-mode))
              (insert mailbox (if (= index 1) distinct "") root-breadcrumb)
              (niwa-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max)
                            (niwa-session-artifacts-segment-path
                             directory (1+ index)) nil 'silent)))
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert mailbox distinct root-breadcrumb))
          ;; A child owns a separate receiving transcript even when its
          ;; execution identity also appears in the parent's forwarded mail.
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/child"
                            (niwa-agent-record--create
                             :path "/root/child" :conversation-buffer child))))
          (with-current-buffer child
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert (niwa--format-hook-audit-record
                     '(:type execution-breadcrumb :owner "/root/child"
                       :execution-id "exec-duplicate" :tool-use-id "child/1"
                       :facts (:execution-id "exec-duplicate" :outcome failure)))))
          (should (= 1 (length (niwa-collaboration--canonical-records child))))
          (let ((first (niwa-collaboration--history-records room 1))
                (second (niwa-collaboration--history-records room 2))
                (current (niwa-collaboration--canonical-records data)))
            (should (= 2 (cl-count "execution" first :key
                                   (lambda (record) (plist-get record :kind)) :test #'equal)))
            (should (= 1 (cl-count "execution" second :key
                                   (lambda (record) (plist-get record :kind)) :test #'equal)))
            (should-not (cl-find "execution" current :key
                                 (lambda (record) (plist-get record :kind)) :test #'equal))
            (should (equal "exec-duplicate"
                           (plist-get (plist-get (car first) :execution) :id)))
            (should (equal "exec-distinct"
                           (plist-get (plist-get (car second) :execution) :id))))
          ;; Missing older evidence must not erase a newer, still readable
          ;; mailbox delivery from the guest's projection.
          (delete-file (niwa-session-artifacts-segment-path directory 1))
          (let ((second (niwa-collaboration--history-records room 2)))
            (should (= 3 (cl-count "execution" second :key
                                   (lambda (record) (plist-get record :kind)) :test #'equal)))))
      (kill-buffer child)
      (kill-buffer data)
      (delete-directory directory t)))))

(niwa-deftest niwa-collaboration-execution-dedup-lazy-archive-reads
  (:doc "text-only publishes avoid archive I/O and source changes invalidate memoized ids")
  (let* ((directory (make-temp-file "niwa-dedup-reads-" t))
         (workspace (niwa-workspace--create
                     :type 'file :id directory :root directory :name "dedup"))
         (session (niwa-session-create "dedup-reads" workspace))
         (data (generate-new-buffer " *dedup read live*"))
         (reloaded (generate-new-buffer " *dedup read reloaded*"))
         (path (niwa-session-artifacts-segment-path directory 1))
         (mailbox (concat "<agent-message type=\"EXECUTION\" sender=\"/root/child\" "
                          "execution_id=\"exec-dedup\" command=\"echo done\" "
                          "outcome=\"success\" exit_code=\"0\">\n"
                          "retained output\n"
                          "</agent-message>\n"))
         (read-segment (symbol-function 'niwa-session-artifacts-read-transcript-segment))
         (reads 0))
    (unwind-protect
        (progn
          (niwa-workspace-identity-ensure directory)
          (setf (niwa-session-save-path session) directory
                (niwa-session-current-segment session) 2)
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert "ordinary streamed text\n"))
          (niwa-session-artifacts-assert-mutation-authority session data)
          (niwa-session-publication-publish
           session (list (list :path path :content mailbox)))
          (niwa-session-artifacts-save session data)
          (with-current-buffer reloaded
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert mailbox))
          (cl-letf (((symbol-function 'niwa-session-artifacts-read-transcript-segment)
                     (lambda (&rest args)
                       (cl-incf reads)
                       (apply read-segment args))))
            (dotimes (_ 3)
              (with-current-buffer data (insert "more text\n"))
              (niwa-collaboration--canonical-records data))
            (should (= 0 reads))
            (with-current-buffer data (insert mailbox))
            (should-not (cl-find "execution"
                                 (niwa-collaboration--canonical-records data)
                                 :key (lambda (record) (plist-get record :kind))
                                 :test #'equal))
            (should (= 1 reads))
            (dotimes (_ 3)
              (with-current-buffer data (insert "streaming continues\n"))
              (niwa-collaboration--canonical-records data))
            (should (= 1 reads))
            ;; A fresh live buffer has no derived cache: cold projection
            ;; reconstructs its own identities from the archive.
            (should-not (cl-find "execution"
                                 (niwa-collaboration--canonical-records reloaded)
                                 :key (lambda (record) (plist-get record :kind))
                                 :test #'equal))
            (should (= 2 reads))
            ;; Replaced segment evidence changes the source fingerprint; the
            ;; live record is no longer suppressed by stale archive state.
            (niwa-session-publication-publish
             session (list (list :path path :content "no execution remains\n")))
            (niwa-session-artifacts-save session data)
            (should (cl-find "execution"
                             (niwa-collaboration--canonical-records data)
                             :key (lambda (record) (plist-get record :kind))
                             :test #'equal))
            (should (= 3 reads))
            ;; Compaction adds a new earlier source.  It invalidates the
            ;; memo even while the live text and execution ID stay the same.
            (setf (niwa-session-current-segment session) 3)
            (niwa-session-publication-publish
             session (list (list :path (niwa-session-artifacts-segment-path directory 2)
                                 :content mailbox)))
            (niwa-session-artifacts-save session data)
            (should-not (cl-find "execution"
                                 (niwa-collaboration--canonical-records data)
                                 :key (lambda (record) (plist-get record :kind))
                                 :test #'equal))
            (should (= 5 reads))
            (niwa-collaboration--canonical-records data)
            (should (= 5 reads))))
      (kill-buffer data)
      (kill-buffer reloaded)
      (niwa-session-persistence-lock-release directory session)
      (delete-directory directory t))))

;;; test-niwa-collaboration-history.el ends here
