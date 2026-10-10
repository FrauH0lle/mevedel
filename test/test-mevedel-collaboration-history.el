;;; test-mevedel-collaboration-history.el --- Archived browser history -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise archived publication through the same room and fetch boundaries.

;;; Code:
(require 'helpers (file-name-concat (file-name-directory load-file-name) "helpers"))
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-history)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-task)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-session-artifacts)
(require 'mevedel-tool-render-data)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ptc)
(require 'mevedel-transcript-restore)
(require 'mevedel-execution-transcript)
(require 'org)

(mevedel-deftest mevedel-collaboration-archived-artifacts
  (:doc "publishes and serves archived artifacts after compaction and a cold room start")
  (let* ((directory (make-temp-file "mevedel-history-" t))
         (artifact (file-name-concat directory ".mevedel" "artifacts" "design.html"))
         (session (mevedel-session--create :name "history" :save-path directory
                                         :workspace (mevedel-workspace--create
                                                     :type 'file :id "w" :root directory)
                                         :authority-mode 'pid-lock :current-segment 2))
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
                    (mevedel-tool-render-data-format
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
                          (mevedel-session-artifacts-segment-path directory 1) nil 'silent))
          (with-current-buffer data (setq-local mevedel--session session))
          (puthash 1 (list :ready t :writable nil) guests)
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (push (mevedel-test--frame frame) sent) t)))
            (mevedel-collaboration--publish room)
            (let* ((index (cl-find "history-index" sent :key (lambda (f) (plist-get f :t)) :test #'equal))
                   (record (car (append (plist-get index :records) nil))))
              (should (equal "design.html" (plist-get record :artifact)))
              (should-not (plist-member record :artifact-path))
              (let ((id (plist-get record :id)))
                (should (string-prefix-p "history-1-" id))
                (setq sent nil)
                (mevedel-collaboration--handle-artifact-get room 1 (list :reqId 1 :id id))
                (should (equal "<h1>Design</h1>" (base64-decode-string (plist-get (car sent) :data))))
              ;; Reconnect/cold publication reconstructs the same authority.
              (let ((cold (list :session session :data-buffer data :guests guests :transport 'test)))
                (should (equal (mevedel-collaboration--history-artifacts cold)
                               (mevedel-collaboration--history-artifacts room))))
              ;; Routine publishes do not reread archived transcript bodies.
              (cl-letf (((symbol-function 'mevedel-session-artifacts-read-segment)
                         (lambda (&rest _) (ert-fail "Archive reread during live publication"))))
                (mevedel-collaboration--publish room))
              (setq sent nil)
              (mevedel-collaboration--handle-history-get room 999 '(:reqId 2 :segment 1))
              (should-not sent)
              (dolist (number '(0 -1 2 1.5 "../session.meta.el"))
                (setq sent nil)
                (mevedel-collaboration--handle-history-get room 1 (list :reqId 2 :segment number))
                (should (plist-get (car sent) :error)))
              (setq sent nil)
              (mevedel-collaboration--handle-history-get room 1 '(:reqId 3 :segment 1))
              (should (eq t (plist-get (car sent) :final)))
              (let ((json (json-encode sent)))
                (should (string-match-p "Make a design" json))
                (should (string-match-p "design.html" json))
                (should-not (string-match-p "artifact-path\\|mevedel-render-data" json))
                (should-not (string-match-p (regexp-quote directory) json)))
              ;; Fast repeat requests receive an explicit retry response.
              (mevedel-collaboration--handle-history-get room 1 '(:reqId 4 :segment 1))
              (should (plist-get (car sent) :error))
              (delete-file artifact)
              (mevedel-collaboration--artifact-stat-invalidate)
              (should (plist-get (car (mevedel-collaboration--history-artifacts room)) :missing))
              (delete-file (mevedel-session-artifacts-segment-path directory 1))
              (plist-put (gethash 1 guests) :last-history-fetch nil)
              (mevedel-collaboration--handle-history-get room 1 '(:reqId 5 :segment 1))
              (should (plist-get (car sent) :error))))
          (should-not (plist-get room :records))))
      (kill-buffer data)
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory directory t))))

(mevedel-deftest mevedel-collaboration--handle-history-get
  (:doc "remote callback reads wait for transport and recheck room and guest authority")
  (dolist (revocation '(nil guest room))
    (let* ((session (mevedel-session--create :name "deferred-history"
                                            :save-path temporary-file-directory :current-segment 2))
           (guest (list :ready t))
           (guests (make-hash-table :test #'eql))
           (room (list :room-id "history" :session session :guests guests))
           (live-room room)
           (mevedel-transport--enabled-p t)
           (mevedel-transport--pending (make-hash-table :test #'equal))
           sent read)
      (puthash 1 guest guests)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-collaboration--room-for-session)
                     (lambda (_) live-room))
                    ((symbol-function 'mevedel-collaboration--history-records)
                     (lambda (&rest _)
                       (should-not (mevedel-transport-nested-p))
                       (setq read t)
                       (list '(:id "saved" :text "Archived answer"))))
                    ((symbol-function 'mevedel-collaboration--publish-history) #'ignore)
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (push (mevedel-test--frame frame) sent))))
            (mevedel-transport-call-as-remote-operation
             (lambda ()
               (let ((timer-list nil))
                 (mevedel-collaboration--handle-history-get room 1 '(:reqId 1 :segment 1))
                 ;; A second request admitted after the rate window must
                 ;; retain its own reply while the same transport stays busy.
                 (plist-put guest :last-history-fetch (- (float-time) 2))
                 (mevedel-collaboration--handle-history-get room 1 '(:reqId 2 :segment 1)))))
            (should-not read)
            (should-not sent)
            (pcase revocation ('guest (remhash 1 guests)) ('room (setq live-room nil)))
            (let ((deadline (+ (float-time) 1)))
              (while (and (> (hash-table-count mevedel-transport--pending) 0)
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (= 0 (hash-table-count mevedel-transport--pending)))
            (if revocation
                (progn (should-not read) (should-not sent))
              (should read)
              (should (equal '(1 2) (sort (mapcar (lambda (frame) (plist-get frame :reqId)) sent) #'<)))
              (should (cl-every (lambda (frame) (plist-get frame :final)) sent))))
        (mevedel-transport-cancel-pending)))))

(mevedel-deftest mevedel-collaboration--history-records-bash-completion
  (:doc "later hidden terminal evidence updates the original archived Bash row")
  (let* ((directory (make-temp-file "mevedel-history-bash-" t))
         (session (mevedel-session--create :name "history-bash" :save-path directory
                                          :authority-mode 'pid-lock :current-segment 2))
         (data (generate-new-buffer " *history bash live*"))
         (room (list :session session :data-buffer data))
         (mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (unwind-protect
        (progn
          (mevedel-tool-register
           (mevedel-tool--create :name "Bash" :category "mevedel"
                                 :renderer #'mevedel-tool-exec--render-bash))
          (with-temp-buffer
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-enable-gptel-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"exit 1\"))\n\ninitial")
              (put-text-property start (point) 'gptel '(tool . "bash-hist"))
              (insert (mevedel-tool-render-data-format
                       '(:execution-id "exec-hist" :command "exit 1"
                         :state running :live-execution-p t)
                       "bash-hist")))
            (insert "\n")
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max)
                          (mevedel-session-artifacts-segment-path directory 1)
                          nil 'silent))
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-enable-gptel-mode)
            (setq-local mevedel--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-archive :tool-use-id "bash-hist"
                       :render-data (:execution-id "exec-hist" :command "exit 1"
                                     :state running :live-execution-p t))))
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "bash-hist"
                       :render-data (:execution-id "exec-hist" :command "exit 1"
                                     :state completed :status error :outcome failure
                                     :exit-code 1 :execution-output "initial\nfinal"))))
            (let ((live (mevedel-collaboration--canonical-records data)))
              (should-not live)))
          (let ((history (mevedel-collaboration--history-records room 1)))
            (should (= 1 (length history)))
            (should (equal "history-1-tool-bash-hist"
                           (plist-get (car history) :id)))
            (should (equal "failed" (plist-get (car history) :status)))
            (should (equal "initial\nfinal" (plist-get (car history) :result))))
          (mevedel-tool-register
           (mevedel-tool--create :name "ToolCall" :category "mevedel"
                                 :renderer #'mevedel-tool-ptc--render))
          (with-temp-buffer
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-enable-gptel-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (let ((start (point)))
              (insert "(:name \"ToolCall\" :args (:expression \"Bash\"))\nraw model result")
              (put-text-property start (point) 'gptel '(tool . "outer"))
              (insert (mevedel-tool-render-data-format
                       '(:kind ptc :outcome completed
                         :calls ((:id "outer/1" :tool "Bash" :status success
                                  :args (:command "exit 1") :result "initial"
                                  :render-data (:execution-id "exec-child"
                                                :state running))))
                       "outer")))
            (insert "\n")
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max)
                          (mevedel-session-artifacts-segment-path directory 1)
                          nil 'silent))
          (with-current-buffer data
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "outer/1"
                       :render-data (:execution-id "exec-child" :command "exit 1"
                                     :state completed :status error :outcome failure
                                     :exit-code 1 :execution-output "initial\nchild final")))))
          (let* ((history (mevedel-collaboration--history-records room 1))
                 (record (car history))
                 (child (aref (plist-get (plist-get record :presentation) :children)
                              0)))
            (should (= 1 (length history)))
            (should (equal "raw model result" (plist-get record :result)))
            (should (equal "failed" (plist-get child :status)))
            (should (string-match-p "child final" (plist-get child :body))))
          (with-current-buffer data
            (let ((plan (mevedel-execution-transcript-prepare-archive
                         data '("outer/1"))))
              (should (= 1 (length (plist-get plan :completed))))
              (mevedel-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max)
                            (mevedel-session-artifacts-segment-path directory 2)
                            nil 'silent)
              (erase-buffer)
              (insert (mevedel-execution-transcript-archive-text plan))
              (setf (mevedel-session-current-segment session) 3)
              (should-not (mevedel-collaboration--canonical-records data))))
          (let* ((history (mevedel-collaboration--history-records room 1))
                 (record (car history))
                 (child (aref (plist-get (plist-get record :presentation) :children)
                              0)))
            (should (= 1 (length history)))
            (should (equal "raw model result" (plist-get record :result)))
            (should (equal "failed" (plist-get child :status)))
            (should (string-match-p "child final" (plist-get child :body))))
          (should-not (mevedel-collaboration--history-records room 2)))
      (kill-buffer data)
      (delete-directory directory t))))

(mevedel-deftest mevedel-collaboration--history-records-second-compaction
  (:doc "second compaction keeps the original Bash result without duplicate cards")
  (let* ((directory (make-temp-file "mevedel-history-twice-" t))
         (session (mevedel-session--create :name "history-twice"
                                          :save-path directory
                                          :authority-mode 'pid-lock
                                          :current-segment 2))
         (data (generate-new-buffer " *history twice live*"))
         (room (list :session session :data-buffer data))
         (mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
         first-plan)
    (unwind-protect
        (progn
          (mevedel-tool-register
           (mevedel-tool--create :name "Bash" :category "mevedel"
                                 :renderer #'mevedel-tool-exec--render-bash))
          (with-temp-buffer
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-enable-gptel-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"exit 1\"))\n\ninitial")
              (put-text-property start (point) 'gptel '(tool . "bash-twice"))
              (insert (mevedel-tool-render-data-format
                       '(:execution-id "exec-twice" :command "exit 1"
                         :state running :live-execution-p t)
                       "bash-twice")))
            (insert "\n")
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (setq first-plan (mevedel-execution-transcript-prepare-archive
                              (current-buffer) '("bash-twice")))
            (should (= 1 (length (plist-get first-plan :live))))
            (write-region (point-min) (point-max)
                          (mevedel-session-artifacts-segment-path directory 1)
                          nil 'silent))
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-enable-gptel-mode)
            (setq-local mevedel--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (mevedel-execution-transcript-archive-text first-plan))
            (mevedel-execution-transcript-commit-archive data first-plan)
            (mevedel-execution-transcript-handle-event
             (list :type 'terminal :data-buffer data :tool-use-id "bash-twice"
                   :owner "/root" :whole-output "initial\nfinal"
                   :facts '(:execution-id "exec-twice" :command "exit 1"
                            :state completed :outcome failure :exit-code 1)))
            (let ((plan (mevedel-execution-transcript-prepare-archive
                         data '("bash-twice"))))
              (should (= 1 (length (plist-get plan :completed))))
              (mevedel-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max)
                            (mevedel-session-artifacts-segment-path directory 2)
                            nil 'silent)
              (erase-buffer)
              (insert (mevedel-execution-transcript-archive-text plan))
              (setf (mevedel-session-current-segment session) 3)
              (should-not (mevedel-collaboration--canonical-records data))))
          (let* ((history (mevedel-collaboration--history-records room 1))
                 (record (car history)))
            (should (= 1 (length history)))
            (should (equal "history-1-tool-bash-twice" (plist-get record :id)))
            (should (equal "failed" (plist-get record :status)))
            (should (equal "initial\nfinal" (plist-get record :result))))
          (let* ((records (mevedel-collaboration--history-records room 2))
                 (record (car records)))
            (should (= 1 (length records)))
            (should (equal "execution" (plist-get record :kind)))
            (should (equal "failed" (plist-get record :status)))
            (should-not (plist-member record :result)))
          (with-current-buffer data
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "bash-twice"
                       :render-data (:execution-id "exec-twice" :command "exit 1"
                                     :state completed :status error :outcome failure
                                     :exit-code 2
                                     :execution-output "initial\nfinal\nlater")))))
          (should (equal "initial\nfinal\nlater"
                         (plist-get (car (mevedel-collaboration--history-records
                                          room 1)) :result)))
          ;; A missing intermediate archive must not hide completion retained
          ;; by the current segment, nor synthesize another primary output.
          (delete-file (mevedel-session-artifacts-segment-path directory 2))
          (let ((record (car (mevedel-collaboration--history-records room 1))))
            (should (equal "initial\nfinal\nlater" (plist-get record :result)))
            (should (equal "failed" (plist-get record :status)))
            (should-not (string-match-p
                         "completion unavailable"
                         (plist-get (plist-get record :presentation) :header))))
          (with-current-buffer data
            (erase-buffer)
            (should-not (mevedel-collaboration--canonical-records data)))
          (let ((record (car (mevedel-collaboration--history-records room 1))))
            (should (equal "warning" (plist-get record :status)))
            (should (string-match-p "completion unavailable"
                                    (plist-get (plist-get record :presentation)
                                               :header)))
            (should (equal "initial" (plist-get record :result))))
          ;; A still-running owner outranks uncertainty from the missing file.
          (cl-letf (((symbol-function 'mevedel-execution-list-user)
                     (lambda (_session)
                       '((:execution-id "exec-twice" :state running
                          :output-tail "still working")))))
            (let ((record (car (mevedel-collaboration--history-records room 1))))
              (should (equal "running" (plist-get record :status)))
              (should-not (string-match-p "unknown"
                                          (plist-get (plist-get record :presentation)
                                                     :header)))))
          (let ((record (car (mevedel-collaboration--history-records
                              (list :session session :data-buffer nil) 1))))
            (should (equal "initial" (plist-get record :result)))))
      (kill-buffer data)
      (delete-directory directory t))))

(mevedel-deftest mevedel-collaboration-forwarded-execution-compaction-dedup
  (:doc "first readable mailbox owns breadcrumb across archives and current transcript")
  (let* ((directory (make-temp-file "mevedel-history-execution-" t))
         (session (mevedel-session--create :name "history-execution"
                                          :save-path directory :authority-mode 'pid-lock
                                          :current-segment 3))
         (data (generate-new-buffer " *forwarded history live*"))
         (child (generate-new-buffer " *forwarded history child*"))
         (room (list :session session :data-buffer data))
         (mailbox (concat "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                          "retained output\n"
                          "<bash-execution execution_id=\"exec-duplicate\" command=\"exit 2\" "
                          "outcome=\"failure\" exit_code=\"2\"/>\n"
                          "</agent-message>\n"))
         (root-breadcrumb
          (mevedel--format-hook-audit-record
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
              (mevedel-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max)
                            (mevedel-session-artifacts-segment-path
                             directory (1+ index)) nil 'silent)))
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert mailbox distinct root-breadcrumb))
          ;; A child owns a separate receiving transcript even when its
          ;; execution identity also appears in the parent's forwarded mail.
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/child"
                            (mevedel-agent-record--create
                             :path "/root/child" :conversation-buffer child))))
          (with-current-buffer child
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-breadcrumb :owner "/root/child"
                       :execution-id "exec-duplicate" :tool-use-id "child/1"
                       :facts (:execution-id "exec-duplicate" :outcome failure)))))
          (should (= 1 (length (mevedel-collaboration--canonical-records child))))
          (let ((first (mevedel-collaboration--history-records room 1))
                (second (mevedel-collaboration--history-records room 2))
                (current (mevedel-collaboration--canonical-records data)))
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
          (delete-file (mevedel-session-artifacts-segment-path directory 1))
          (let ((second (mevedel-collaboration--history-records room 2)))
            (should (= 3 (cl-count "execution" second :key
                                   (lambda (record) (plist-get record :kind)) :test #'equal)))))
      (kill-buffer child)
      (kill-buffer data)
      (delete-directory directory t))))

(mevedel-deftest mevedel-collaboration-execution-dedup-lazy-archive-reads
  (:doc "text-only publishes avoid archive I/O and source changes invalidate memoized ids")
  (let* ((directory (make-temp-file "mevedel-dedup-reads-" t))
         (session (mevedel-session--create :name "dedup-reads" :save-path directory
                                          :authority-mode 'pid-lock :current-segment 2))
         (data (generate-new-buffer " *dedup read live*"))
         (reloaded (generate-new-buffer " *dedup read reloaded*"))
         (path (mevedel-session-artifacts-segment-path directory 1))
         (mailbox (concat "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                          "retained output\n"
                          "<bash-execution execution_id=\"exec-dedup\" command=\"echo done\" "
                          "outcome=\"success\" exit_code=\"0\"/>\n"
                          "</agent-message>\n"))
         (read-segment (symbol-function 'mevedel-session-artifacts-read-transcript-segment))
         (reads 0))
    (unwind-protect
        (progn
          (with-temp-file path (insert mailbox))
          (with-current-buffer data
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert "ordinary streamed text\n"))
          (with-current-buffer reloaded
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert mailbox))
          (cl-letf (((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                     (lambda (&rest args)
                       (cl-incf reads)
                       (apply read-segment args))))
            (dotimes (_ 3)
              (with-current-buffer data (insert "more text\n"))
              (mevedel-collaboration--canonical-records data))
            (should (= 0 reads))
            (with-current-buffer data (insert mailbox))
            (should-not (cl-find "execution"
                                 (mevedel-collaboration--canonical-records data)
                                 :key (lambda (record) (plist-get record :kind))
                                 :test #'equal))
            (should (= 1 reads))
            (dotimes (_ 3)
              (with-current-buffer data (insert "streaming continues\n"))
              (mevedel-collaboration--canonical-records data))
            (should (= 1 reads))
            ;; A fresh live buffer has no derived cache: cold projection
            ;; reconstructs its own identities from the archive.
            (should-not (cl-find "execution"
                                 (mevedel-collaboration--canonical-records reloaded)
                                 :key (lambda (record) (plist-get record :kind))
                                 :test #'equal))
            (should (= 2 reads))
            ;; Replaced segment evidence changes the source fingerprint; the
            ;; live record is no longer suppressed by stale archive state.
            (with-temp-file path (insert "no execution remains\n"))
            (should (cl-find "execution"
                             (mevedel-collaboration--canonical-records data)
                             :key (lambda (record) (plist-get record :kind))
                             :test #'equal))
            (should (= 3 reads))
            ;; Compaction adds a new earlier source.  It invalidates the
            ;; memo even while the live text and execution ID stay the same.
            (with-temp-file (mevedel-session-artifacts-segment-path
                             directory 2)
              (insert mailbox))
            (setf (mevedel-session-current-segment session) 3)
            (should-not (cl-find "execution"
                                 (mevedel-collaboration--canonical-records data)
                                 :key (lambda (record) (plist-get record :kind))
                                 :test #'equal))
            (should (= 5 reads))
            (mevedel-collaboration--canonical-records data)
            (should (= 5 reads))))
      (kill-buffer data)
      (kill-buffer reloaded)
      (delete-directory directory t))))

;;; test-mevedel-collaboration-history.el ends here
