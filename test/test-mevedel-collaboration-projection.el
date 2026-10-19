;;; test-mevedel-collaboration-projection.el --- Projection cost contracts -*- lexical-binding: t -*-

;;; Commentary:
;; Growing transcripts must not repeatedly recover every historical tool
;; block, and a retained projection must equal a full one.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration-guest)
(require 'mevedel-tool-ptc)
(require 'mevedel-tool-render-data)

(mevedel-deftest mevedel-collaboration--agent-mailbox ()
  ,test
  (test)
  :doc "agent results retain envelope outcomes, bodies, identities and wire fields"
  (dolist (outcome '("completed" "errored" "interrupted" nil))
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (insert (format "<agent-result sender=\"/root/reviewer\"%s>\n"
                      (if outcome (format " outcome=\"%s\"" outcome) ""))
              "Completed successfully? Body prose is not status.\n</agent-result>\n")
      (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
             (record (car records))
             (wire (mevedel-collaboration--json-record record)))
        (should (= 1 (length records)))
        (should (equal "agent-notice" (plist-get record :kind)))
        (should (equal "/root/reviewer" (alist-get "sender" wire nil nil #'equal)))
        (should (equal (or outcome "incomplete") (alist-get "outcome" wire nil nil #'equal)))
        (should (equal "result" (alist-get "mailType" wire nil nil #'equal)))
        (should (string-match-p "Body prose" (plist-get record :text)))
        (should (equal records (mevedel-collaboration--canonical-records (current-buffer)))))))
  :doc "ordinary messages remain inspectable and distinct in source order"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (dotimes (_ 2)
      (insert "<agent-message type=\"MAIL\" sender=\"/root/reviewer\">\n"
              "interim **finding**\n</agent-message>\n"))
    (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
      (should (= 2 (length records)))
      (should (equal "message" (plist-get (car records) :mailType)))
      (should-not (equal (plist-get (car records) :id) (plist-get (cadr records) :id))))))

(ert-deftest mevedel-collaboration-approval-presentation ()
  "Canonical tool projection carries bounded display-only approval reasons."
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "#+begin_tool\n"
            (propertize
             (concat "(:name \"ApprovalProbe\" :args nil)\nError: failed\n"
                     (mevedel-tool-render-data-format
                      '(:status error :permission-approvals
                        ((:source reviewer :outcome allow-once :reason "Safe inspection")))
                      "approved"))
             'gptel '(tool . "approved"))
            "#+end_tool\n")
    (let* ((record (car (mevedel-collaboration--canonical-records (current-buffer))))
           (presentation (plist-get record :presentation)))
      (should (equal "approved" (plist-get record :toolUseId)))
      (should (equal "approved" (alist-get "toolUseId"
                                          (mevedel-collaboration--json-record record)
                                          nil nil #'equal)))
      (should (equal "Safe inspection" (plist-get presentation :approval)))
      (should (equal "failed" (plist-get presentation :status)))
      (should-not (string-match-p "Safe inspection" (plist-get record :result))))))

(mevedel-deftest mevedel-collaboration--activity-records ()
  (let* ((records
          (cl-loop for name in '("Read" "Grep" "Read" "Eval") for n from 1
                   collect (list :id (format "tool-%s" n) :kind "tool"
                                 :status (if (= n 2) "failed" "completed")
                                 :presentation (list :name name :groupable t))))
         (grouped (mevedel-collaboration--activity-records records))
         (activity (plist-get (car grouped) :activity)))
    (should (equal "tool-1" (plist-get activity :id)))
    (should (equal "Read 2 files, searched 1 pattern, evaluated 1 form"
                   (plist-get activity :summary)))
    (should (equal "1 failed: Grep" (plist-get activity :failures)))
    (should (equal "failed" (plist-get activity :status)))
    (should (cl-every (lambda (record) (equal activity (plist-get record :activity))) grouped))
    (should-not (plist-get (car (mevedel-collaboration--activity-records (cl-subseq records 0 3)))
                           :activity))
    (should-not (plist-get (car (mevedel-collaboration--activity-records
                                (append (cl-subseq records 0 2)
                                        (list '(:id "result" :kind "agent-notice" :mailType "result"))
                                        (cl-subseq records 2)))) :activity))
    (let* ((completion (list :id "completion" :kind "execution" :status "completed"
                             :execution '(:owner "/root" :ownPath "/root" :id "bash-1")))
           (message (list :id "message" :kind "agent-notice" :mailType "message"))
           (with-notices (mevedel-collaboration--activity-records
                          (append records (list completion message))))
           (activity (plist-get (car with-notices) :activity)))
      (should (string-match-p "1 command completed, received 1 message" (plist-get activity :summary)))
      (should (equal activity (plist-get completion :activity)))
      (setf (plist-get (car records) :status) "running")
      (let ((running (plist-get (car (mevedel-collaboration--activity-records records)) :activity)))
        (should (= 1 (plist-get running :running)))
        (should (equal "running" (plist-get running :status)))
        (should (equal "1 failed: Grep" (plist-get running :failures)))))))

(mevedel-deftest mevedel-collaboration-tool-presentation/approval ()
  ,test
  (test)
  :doc "direct and nested approvals remain child-scoped within the projection budget"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
    (let* ((reason (make-string 3000 ?x))
           (child (list :id "outer/1" :tool "ApprovalProbe" :status 'error
                        :args nil :result "Error: failed" :render-data
                        (list :permission-approvals
                              (list (list :source 'reviewer :outcome 'allow-once :reason reason)))))
           (data (list :kind 'ptc :status 'success :outcome 'completed :calls (list child)))
           (call (list :name "ToolCall" :args '(:expression "(ApprovalProbe)")
                       :result "Error: failed" :render-data data))
           (nested (mevedel-collaboration-tool-presentation call)))
      (should-not (plist-get nested :approval))
      (should (equal "outer/1" (plist-get (aref (plist-get nested :children) 0) :sourceId)))
      (should (equal "failed" (plist-get (aref (plist-get nested :children) 0) :status)))
      (should (<= (length (plist-get (aref (plist-get nested :children) 0) :approval)) 1024))
      (setf (plist-get call :render-data) (plist-put data :direct-tool "ApprovalProbe"))
      (let ((direct (mevedel-collaboration-tool-presentation call)))
        (should (equal "ApprovalProbe" (plist-get direct :name)))
        (should (equal "failed" (plist-get direct :status)))
        (should (stringp (plist-get direct :approval)))))))

(mevedel-deftest mevedel-collaboration--pipeline-tool-event ()
  ,test
  (test)
  :doc "browser pending approvals bind exact IDs and survive out-of-order canonical settlement"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let* ((data (current-buffer))
           (room (list :data-buffer data :guests (make-hash-table :test #'eql)
                       :tool-call-occurrences (make-hash-table :test #'equal)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           (call '(:name "ApprovalProbe" :args nil)))
      (cl-labels ((event (type id &rest facts)
                    (run-hook-with-args
                     'mevedel-pipeline-tool-event-hook
                     (append (list :type type :data-buffer data :tool-use-id id) call facts))))
        (mevedel-collaboration--pre-tool call)
        (mevedel-collaboration--pre-tool call)
        (event 'started "first")
        (event 'started "second")
        (event 'permission-approved "second" :permission-approvals
               '((:source reviewer :outcome allow-once :reason "Second only")))
        (let ((records (plist-get room :records)))
          (should (= 2 (length records)))
          (should-not (plist-get (plist-get (car records) :presentation) :approval))
          (ert-info ((format "Pending: %S" (plist-get room :pending-tools)))
            (should (equal "Second only" (plist-get (plist-get (cadr records) :presentation) :approval)))))
        (event 'settled "second" :status 'error)
        (mevedel-collaboration--post-tool (append call '(:result "Error: failed")))
        (should (equal "running" (plist-get (car (plist-get room :records)) :status)))
        (let ((second-id (plist-get (cadr (plist-get room :records)) :id)))
          (insert "#+begin_tool\n"
                  (propertize
                   (concat "(:name \"ApprovalProbe\" :args nil)\nError: failed\n"
                           (mevedel-tool-render-data-format
                            '(:status error :permission-approvals
                              ((:source reviewer :outcome allow-once :reason "Second only"))) "second"))
                   'gptel '(tool . "second"))
                  "#+end_tool\n")
          (mevedel-collaboration--publish room)
          (let ((settled (cl-find second-id (plist-get room :records)
                                  :key (lambda (r) (plist-get r :id)) :test #'equal)))
            (should (equal "failed" (plist-get settled :status)))
            (should (equal "Second only" (plist-get (plist-get settled :presentation) :approval)))))
        (should (= 1 (length (plist-get room :pending-tools))))
        (should (equal "first" (plist-get (car (plist-get room :pending-tools)) :tool-use-id))))))
  :doc "nested pending approvals decorate only the child, and late events do not change terminal hints"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
    (let* ((data (current-buffer))
           (room (list :data-buffer data :guests (make-hash-table :test #'eql)
                       :tool-call-occurrences (make-hash-table :test #'equal)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
      (cl-labels ((event (type id parent name &rest facts)
                    (run-hook-with-args
                     'mevedel-pipeline-tool-event-hook
                     (append (list :type type :data-buffer data :tool-use-id id
                                   :parent-tool-use-id parent :name name) facts))))
        (event 'started "outer" nil "ToolCall")
        (event 'started "child" "outer" "ApprovalProbe")
        (event 'permission-approved "child" "outer" "ApprovalProbe" :permission-approvals
               '((:source reviewer :outcome allow-once :reason "Nested only")))
        (dotimes (terminal 3)
          (when (= terminal 1)
            (event 'settled "child" "outer" "ApprovalProbe" :status 'error)
            (event 'started "child" "outer" "ApprovalProbe")
            (event 'permission-approved "child" "outer" "ApprovalProbe" :permission-approvals
                   '((:source reviewer :outcome allow-once :reason "Late"))))
          (when (= terminal 2)
            (event 'settled "outer" nil "ToolCall" :status 'success)
            (event 'started "outer" nil "ToolCall"))
          ;; Nested events share the room's coalesced publication boundary.
          (mevedel-collaboration--publish room)
          (let* ((presentation (plist-get (car (plist-get room :records)) :presentation))
                 (child (aref (plist-get presentation :children) 0)))
            (should-not (plist-get presentation :approval))
            (should (equal "Nested only" (plist-get child :approval)))
            (should (equal (if (= terminal 2) "warning" "running")
                           (plist-get presentation :status)))
            (should (equal (if (> terminal 0) "failed" "running")
                           (plist-get child :status)))))
        (when (timerp (plist-get room :publish-timer))
          (cancel-timer (plist-get room :publish-timer))))))
  :doc "nested bursts project once per publication and root settlement remains immediate"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                          :renderer #'mevedel-tool-ptc--render))
    (let* ((data (current-buffer))
           (room (list :data-buffer data :guests (make-hash-table :test #'eql)
                       :tool-call-occurrences (make-hash-table :test #'equal)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           (render (symbol-function 'mevedel-collaboration-tool-presentation))
           (renders 0))
      (cl-labels ((event (type id &rest facts)
                    (mevedel-collaboration--pipeline-tool-event
                     (append (list :type type :data-buffer data :tool-use-id id
                                   :parent-tool-use-id (unless (equal id "outer") "outer")
                                   :name (if (equal id "outer") "ToolCall" "Probe")) facts))))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-collaboration-tool-presentation)
                       (lambda (parsed) (cl-incf renders) (funcall render parsed))))
              (event 'started "outer")
              (dotimes (index 32)
                (event 'started (format "outer/%d" index))
                (event 'permission-approved (format "outer/%d" index)
                       :permission-approvals
                       '((:source reviewer :outcome allow-once :reason "Child only"))))
              ;; Direct consumers see the latest facts even before the timer.
              (let* ((display (plist-get (car (mevedel-collaboration--project-records room)) :presentation))
                     (children (plist-get display :children)))
                (should-not (plist-get display :approval))
                (should (= 32 (length children)))
                (cl-loop for child across children for index from 0 do
                         (should (equal (plist-get child :id) (format "outer/%d" index)))
                         (should (equal (plist-get child :approval) "Child only"))
                         (should (equal (plist-get child :status) "running"))))
              (should (timerp (plist-get room :publish-timer)))
              (cancel-timer (plist-get room :publish-timer))
              (mevedel-collaboration--publish-timer data)
              (should-not (plist-get room :publish-timer))
              (dotimes (index 32)
                (event 'settled (format "outer/%d" index) :status 'error))
              (event 'settled "outer" :status 'success)
              (let ((display (plist-get (car (plist-get room :records)) :presentation)))
                (should (equal (plist-get display :status) "warning"))
                (cl-loop for child across (plist-get display :children) do
                         (should (equal (plist-get child :status) "failed"))))
              (should (<= renders 3)))
          (when (timerp (plist-get room :publish-timer))
            (cancel-timer (plist-get room :publish-timer))))))))

(mevedel-deftest mevedel-collaboration--refresh-pipeline-presentation
  (:doc "only dirty trees are rendered, preserving clean presentation identity")
  (let* ((entry (list :pipeline-dirty t :status "running" :presentation nil
                      :pipeline-calls '((:tool-use-id "read" :name "Read" :status success))))
         (calls 0))
    (cl-letf (((symbol-function 'mevedel-collaboration-tool-presentation)
               (lambda (_parsed) (cl-incf calls) (list :status "completed"))))
      (mevedel-collaboration--refresh-pipeline-presentation entry)
      (let ((display (plist-get entry :presentation)))
        (should (equal (plist-get entry :status) "completed"))
        (should-not (plist-get entry :pipeline-dirty))
        (mevedel-collaboration--refresh-pipeline-presentation entry)
        (should (eq display (plist-get entry :presentation)))
        (should (= calls 1))))))

(mevedel-deftest mevedel-collaboration--pipeline-tool-event/read-producer
  (:doc "real Read hook/pipeline args bind identical siblings and settle out of order")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let* ((data (current-buffer))
           (room (list :data-buffer data :guests (make-hash-table :test #'eql)
                       :tool-call-occurrences (make-hash-table :test #'equal)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           (tool (mevedel-tool-ensure "Read"))
           (hook '(:name "Read" :args (:limit 5 :file_path "a.el")))
           (args (mevedel-tool--positional-to-plist
                  (gptel--map-tool-args (mevedel-tool-gptel-tool tool) (plist-get hook :args))
                  (mevedel-tool-args tool))))
      (should-not (equal args (plist-get hook :args)))
      (cl-labels ((event (type id &rest facts)
                    (mevedel-collaboration--pipeline-tool-event
                     (append (list :type type :data-buffer data :tool-use-id id
                                   :name "Read" :args args) facts)))
                  (land (id status result)
                    (insert "#+begin_tool\n"
                            (propertize
                             (concat (prin1-to-string hook) "\n" result "\n"
                                     (mevedel-tool-render-data-format (list :status status) id))
                             'gptel (cons 'tool id))
                            "#+end_tool\n")
                    (mevedel-collaboration--publish room)))
        (mevedel-collaboration--pre-tool hook)
        (let ((first-id (plist-get (car (plist-get room :records)) :id)))
          (event 'started "first")
          (should (= 1 (length (plist-get room :pending-tools))))
          (should (equal first-id (plist-get (car (plist-get room :records)) :id)))
          (mevedel-collaboration--pre-tool hook)
          (event 'started "second")
          (should (= 2 (length (plist-get room :pending-tools))))
          (should (equal '("first" "second")
                         (mapcar (lambda (row) (plist-get row :tool-use-id))
                                 (plist-get room :pending-tools))))
          (let ((second-id (plist-get (cadr (plist-get room :records)) :id)))
            (event 'settled "second" :status 'error)
            ;; The ID-less gptel post-hook must not consume the running twin.
            (mevedel-collaboration--post-tool (append hook '(:result "Error: failed")))
            (should (equal '("running" "failed")
                           (mapcar (lambda (row) (plist-get row :status))
                                   (plist-get room :records))))
            (land "second" 'error "Error: failed")
            (should (= 1 (length (plist-get room :pending-tools))))
            (should (equal "first" (plist-get (car (plist-get room :pending-tools)) :tool-use-id)))
            (event 'settled "first" :status 'success)
            (mevedel-collaboration--post-tool (append hook '(:result "done")))
            (land "first" 'success "done")
            (should-not (plist-get room :pending-tools))
            (should (= 2 (length (plist-get room :records))))
            (should (equal (list second-id first-id)
                           (mapcar (lambda (row) (plist-get row :id))
                                   (plist-get room :records))))))))))

(mevedel-deftest mevedel-collaboration--tool-call-key
  (:doc "structural call keys ignore omitted nils, not deep or long argument differences")
  (let ((pairs (list (list nil :json-false)
                     (list (make-list 40 'a) (append (make-list 39 'a) '(b)))
                     (list '(:a (:b (:c (:d a)))) '(:a (:b (:c (:d b))))))))
    (should (equal (mevedel-collaboration--tool-call-key
                    '(:name "Read" :args (:file_path "a" :limit 5)))
                   (mevedel-collaboration--tool-call-key
                    '(:name "Read" :args (:offset nil :limit 5 :file_path "a")))))
    (dolist (values pairs)
      (should-not
       (equal (mevedel-collaboration--tool-call-key (list :name "Probe" :args (list :value (car values))))
              (mevedel-collaboration--tool-call-key (list :name "Probe" :args (list :value (cadr values)))))))))

(mevedel-deftest mevedel-collaboration--pre-tool/structural-ids
  (:doc "structurally distinct arguments get distinct browser record identities")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let* ((room (list :data-buffer (current-buffer) :guests (make-hash-table :test #'eql)
                       :tool-call-occurrences (make-hash-table :test #'equal)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
      (dolist (args '((:file_path "f" :offset 5)
                      (:file_path "f" :offset "5")
                      (:pattern "a) (:x . b")
                      (:pattern "a" :x "b")))
        (mevedel-collaboration--pre-tool (list :name "Probe" :args args)))
      (should (= 4 (length (plist-get room :records))))
      (should (= 4 (length (delete-dups
                            (mapcar (lambda (record) (plist-get record :id))
                                    (plist-get room :records)))))))))

(mevedel-deftest mevedel-collaboration--canonical-records/bounded-history
  (:doc "stream updates preserve tool records with linear boundary searches")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((count 32)
          (scan (symbol-function 'mevedel-transcript--tool-block-end-from-start)))
      (dotimes (index count)
        (insert "#+begin_tool\n"
                (propertize
                 (format "(:name \"Read\" :args (:file_path \"/tmp/fixture\"))\nresult %d\n" index)
                 'gptel (cons 'tool (format "call-%d" index)))
                "#+end_tool\n\n"))
      (dotimes (_ 2)
        (goto-char (point-max))
        (insert (propertize "Another streamed sentence.\n" 'gptel 'response))
        (let ((scans 0) records)
          (cl-letf (((symbol-function 'mevedel-transcript--tool-block-end-from-start)
                     (lambda (&rest args)
                       (cl-incf scans)
                       (apply scan args))))
            (setq records (mevedel-collaboration--canonical-records (current-buffer))))
          (let ((tools (seq-filter (lambda (r) (equal "tool" (plist-get r :kind))) records)))
            (should (= count (length tools)))
            (dotimes (index count)
              (should (equal (format "result %d" index)
                             (plist-get (nth index tools) :result)))))
          (should (<= scans (* 4 count))))))))

(defun mevedel-test--projection-piece (index)
  "Return synthetic transcript text for piece INDEX.
Small value ranges repeat texts, so occurrence-numbered ids are exercised."
  (pcase (% index 7)
    (0 (format "Prompt %d about the parser.\n\n" (random 3)))
    (1 (propertize (format "Answer %d.\n\n" (random 3)) 'gptel 'response))
    (2 (concat "#+begin_tool\n"
               (propertize
                (format "(:name \"Read\" :args (:file_path \"/tmp/f%d\"))\n,* result %d\n"
                        index (random 3))
                'gptel (cons 'tool (format "call-%d-%d" index (random 1000))))
               "#+end_tool\n\n"))
    ;; No tool-use id: identity falls back to text and occurrence.
    (3 (concat "#+begin_tool\n"
               (propertize "(:name \"Bash\" :args (:command \"true\"))\ndone\n"
                           'gptel '(tool))
               "#+end_tool\n\n"))
    (4 (mevedel--format-hook-audit-record
        (list :type 'guest-prompt :name (format "guest-%d" (random 2)))))
    (5 (mevedel-tool-render-data-format
        '(:kind request-summary :elapsed-seconds 1 :outcome error
          :message "Provider unavailable")))
    (_ (propertize "Same answer.\n\n" 'gptel 'response))))

(defun mevedel-test--projection-mutate (step)
  "Apply one random transcript mutation; return its name for STEP."
  (let* ((size (buffer-size))
         (pos (1+ (random (max 1 size))))
         (kind (random 10)))
    (pcase kind
      (0 (goto-char (point-max))
         (insert (propertize (format "Streamed %d.\n" step) 'gptel 'response))
         'stream)
      (1 (goto-char (point-max))
         (insert (mevedel-test--projection-piece (random 7)))
         'append)
      (2 (goto-char pos)
         (insert (mevedel-test--projection-piece (random 7)))
         'insert)
      (3 (delete-region pos (min (point-max) (+ pos (random 24))))
         'delete)
      (4 (put-text-property pos (min (point-max) (+ pos 1 (random 40)))
                            'gptel (nth (random 3) '(response ignore nil)))
         'property)
      ;; Stream inserts and render-data patches run with hooks inhibited.
      (5 (with-silent-modifications
           (put-text-property pos (min (point-max) (+ pos 1 (random 40)))
                              'invisible t))
         'silent-property)
      (6 (when (< pos (point-max))
           (let ((props (text-properties-at pos)))
             (goto-char pos)
             (delete-char 1)
             (insert (apply #'propertize "Z" props))))
         'same-length)
      (7 (let ((text (buffer-string)))
           (erase-buffer)
           (insert text))
         'revert)
      (8 (erase-buffer)
         (dotimes (index (+ 3 (random 6)))
           (insert (mevedel-test--projection-piece index)))
         'reload)
      ;; The parse unescapes Org only in Org buffers.
      (_ (if (derived-mode-p 'org-mode) (fundamental-mode)
           (mevedel--transcript-org-mode))
         'mode))))

(mevedel-deftest mevedel-collaboration--canonical-records/retained
  (:doc "a retained projection equals a full one across transcript mutations")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((cache (mevedel-collaboration--projection-cache-create)))
      (random "mevedel-collaboration-retained-projection")
      (dotimes (index 14)
        (insert (mevedel-test--projection-piece index)))
      (dotimes (step 300)
        (let* ((before (buffer-string))
               (mutation (mevedel-test--projection-mutate step))
               (narrow (and (zerop (random 8)) (> (buffer-size) 2)))
               (from (and narrow (1+ (random (/ (buffer-size) 2)))))
               (to (and narrow (+ from 1 (random (- (point-max) from))))))
          (save-restriction
            (when narrow (narrow-to-region from to))
            ;; A mutation can break a tool block; both projections must
            ;; then fail alike.
            (let ((retained (condition-case err
                                (mevedel-collaboration--canonical-records
                                 (current-buffer) nil nil cache)
                              (error err)))
                  (full (condition-case err
                            (mevedel-collaboration--canonical-records
                             (current-buffer))
                          (error err))))
              (should (equal (list step mutation narrow retained)
                             (list step mutation narrow full)))
              ;; Undo a mutation that broke the transcript, so later steps
              ;; keep comparing records rather than the same error.
              (when (and (consp full) (symbolp (car full)) (get (car full) 'error-conditions))
                (widen)
                (erase-buffer)
                (insert before))
              ;; Callers rewrite ids and revisions in the records they get.
              (when (plist-get (car-safe retained) :id)
                (dolist (record retained)
                  (plist-put record :id "rewritten")
                  (plist-put record :revision 99)
                  (plist-put record :guest "rewritten"))))))))))

(mevedel-deftest mevedel-collaboration--canonical-records/streaming
  (:doc "a streamed sentence reprojects only the growing response")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (dotimes (index 8)
      (insert (format "Prompt %d\n\n" index)
              (propertize (format "Answer %d\n\n" index) 'gptel 'response)
              "#+begin_tool\n"
              (propertize (format "(:name \"Read\" :args (:file_path \"/tmp/%d\"))\nresult\n"
                                  index)
                          'gptel (cons 'tool (format "call-%d" index)))
              "#+end_tool\n\n"))
    (insert (propertize "Streaming" 'gptel 'response))
    (let ((cache (mevedel-collaboration--projection-cache-create))
          (parse (symbol-function 'mevedel-view--tool-call-parse))
          (clean (symbol-function 'mevedel-collaboration--clean-response))
          (parses 0) (cleans 0))
      (mevedel-collaboration--canonical-records (current-buffer) nil nil cache)
      (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                 (lambda (&rest args) (cl-incf parses) (apply parse args)))
                ((symbol-function 'mevedel-collaboration--clean-response)
                 (lambda (&rest args) (cl-incf cleans) (apply clean args))))
        (let ((inhibit-modification-hooks t))
          (insert (propertize " more text." 'gptel 'response)))
        (let ((records (mevedel-collaboration--canonical-records
                        (current-buffer) nil nil cache)))
          (should (equal "Streaming more text."
                         (plist-get (car (last records)) :text)))
          (should (= 0 parses))
          (should (= 1 cleans)))
        ;; An unmodified buffer reprojects nothing at all.
        (mevedel-collaboration--canonical-records (current-buffer) nil nil cache)
        (should (= 1 cleans))))))

(mevedel-deftest mevedel-collaboration--canonical-records/live-inputs
  (:doc "a retained Bash record follows its execution without a text change")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "#+begin_tool\n"
            (propertize "(:name \"Bash\" :args (:command \"make\"))\nstarted\n"
                        'gptel '(tool . "bash-1"))
            "#+end_tool\n")
    (let ((cache (mevedel-collaboration--projection-cache-create))
          terminal)
      (cl-letf (((symbol-function 'mevedel-execution-transcript-pending-render-data)
                 (lambda (_buffer id) (and (equal id "bash-1") terminal))))
        (should (equal "started"
                       (plist-get (car (mevedel-collaboration--canonical-records
                                        (current-buffer) nil nil cache))
                                  :result)))
        (setq terminal '(:execution-output "finished"))
        (let ((retained (mevedel-collaboration--canonical-records
                         (current-buffer) nil nil cache)))
          (should (equal "finished" (plist-get (car retained) :result)))
          (should (equal retained (mevedel-collaboration--canonical-records
                                   (current-buffer)))))))))

(mevedel-deftest mevedel-collaboration--canonical-records/plan-mode
  (:doc "leaving plan mode reprojects a response it hid part of")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Plan it\n\n"
            (propertize "Here is the plan:\n<proposed_plan>\n# Step one\n"
                        'gptel 'response))
    (let ((cache (mevedel-collaboration--projection-cache-create)))
      (setq-local mevedel--session (mevedel-session--create :name "plan"))
      (setf (mevedel-session-plan-mode mevedel--session) t)
      (mevedel-collaboration--canonical-records (current-buffer) nil nil cache)
      (setf (mevedel-session-plan-mode mevedel--session) nil)
      (should (equal (mevedel-collaboration--canonical-records (current-buffer))
                     (mevedel-collaboration--canonical-records
                      (current-buffer) nil nil cache))))))

(mevedel-deftest mevedel-collaboration--source-unchanged-p
  (:doc "detects character, property and accessibility changes")
  (with-temp-buffer
    (insert "ab" (propertize "cd" 'gptel 'response) "ef")
    (let ((source (mevedel-collaboration--source 2 6)))
      (should (mevedel-collaboration--source-unchanged-p 2 source))
      (should-not (mevedel-collaboration--source-unchanged-p 1 source))
      (put-text-property 4 5 'gptel 'ignore)
      (should-not (mevedel-collaboration--source-unchanged-p 2 source))
      (put-text-property 4 5 'gptel 'response)
      (should (mevedel-collaboration--source-unchanged-p 2 source))
      (save-restriction
        (narrow-to-region 3 7)
        (should-not (mevedel-collaboration--source-unchanged-p 2 source)))
      (goto-char 3)
      (delete-char 1)
      (insert "X")
      (should-not (mevedel-collaboration--source-unchanged-p 2 source)))))

(mevedel-deftest mevedel-collaboration--observe
  (:doc "records lookups only while a segment is projected")
  (progn
    (should (= 3 (mevedel-collaboration--observe #'+ 1 2)))
    (let ((mevedel-collaboration--observed (list nil)))
      (should (= 3 (mevedel-collaboration--observe #'+ 1 2)))
      (should (equal (list (list #'+ '(1 2) 3))
                     (car mevedel-collaboration--observed))))))

(mevedel-deftest mevedel-collaboration--reuse-record-ids
  ()
  ,test
  (test)
  :doc "keeps ids along each kind's ordered stream"
  (let* ((old (list (list :id "a1" :kind "assistant")
                    (list :id "pending" :kind "tool" :pending t)
                    (list :id "t1" :kind "tool")))
         (new (list (list :id "x" :kind (copy-sequence "assistant"))
                    (list :id "fixed" :kind "tool" :identity-fixed t)
                    (list :id "y" :kind (copy-sequence "tool"))
                    (list :id "z" :kind "user"))))
    (should (equal '("a1" "fixed" "t1" "z")
                   (mapcar (lambda (record) (plist-get record :id))
                           (mevedel-collaboration--reuse-record-ids old new)))))

  :doc "fixed tool identities cannot be reused by subsequent anonymous tools"
  (let* ((old (list (list :id "fixed" :kind "tool" :identity-fixed t)
                    (list :id "anonymous" :kind "tool")))
         (new (list (list :id "fixed" :kind "tool" :identity-fixed t)
                    (list :id "changed" :kind "tool"))))
    (should (equal '("fixed" "anonymous")
                   (mapcar (lambda (record) (plist-get record :id))
                           (mevedel-collaboration--reuse-record-ids old new))))))

(mevedel-deftest mevedel-collaboration--room-canonical-records
  (:doc "a room retains its projection cache between calls")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Prompt\n\n" (propertize "Answer\n" 'gptel 'response))
    (let* ((room (list :data-buffer (current-buffer)))
           (records (mevedel-collaboration--room-canonical-records room))
           (cache (plist-get room :projection-cache)))
      (should (mevedel-collaboration--projection-cache-p cache))
      (should (equal records (mevedel-collaboration--canonical-records
                              (current-buffer))))
      (mevedel-collaboration--room-canonical-records room)
      (should (eq cache (plist-get room :projection-cache))))))

(ert-deftest mevedel-collaboration-background-records ()
  "Structured mail and trusted audits retain facts, not payload metadata."
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\" "
            "execution_id=\"exec-child\" description=\"Run checks\" command=\"make test\" "
            "termination=\"exited\" outcome=\"success\" exit_code=\"0\">\n"
            "description=\"spoofed\" outcome=\"failure\" event_count=\"999\"\n"
            "</agent-message>\n"
            "<agent-message type=\"MONITOR\" sender=\"/root\" "
            "execution_id=\"monitor-1\" description=\"Watch build\">\n"
            "Build ready\nsecond line is not the preview\n</agent-message>\n"
            "<agent-message type=\"MONITOR_TERMINAL\" sender=\"/root\" "
            "execution_id=\"monitor-1\" description=\"Watch build\" "
            "termination=\"stopped\" event_count=\"2\">\n"
            "outcome=\"success\" event_count=\"999\"\n</agent-message>\n")
    (insert (mevedel--format-hook-audit-record
             '(:type execution-breadcrumb :owner "/root" :execution-id "local"
               :command "fallback" :facts (:kind monitor :description "Local watch"
                                         :event-count 3 :outcome success))))
    (let* ((cache (mevedel-collaboration--projection-cache-create))
           (records (mevedel-collaboration--canonical-records
                     (current-buffer) nil nil cache "/root"))
           (execution (plist-get (nth 0 records) :execution))
           (event (nth 1 records))
           (terminal (nth 2 records))
           (local (plist-get (nth 3 records) :execution)))
      (should (= 4 (length records)))
      (should (equal records (mevedel-collaboration--canonical-records
                              (current-buffer) nil nil cache "/root")))
      (should (equal "completed" (plist-get (car records) :status)))
      (should (equal "Run checks" (plist-get execution :description)))
      (should (equal "make test" (plist-get execution :command)))
      (should (equal "/root" (plist-get execution :ownPath)))
      (should (equal "monitor-event" (plist-get event :kind)))
      (should (equal "Build ready" (plist-get event :text)))
      (should (equal "Watch build" (plist-get event :description)))
      (should (equal "Watch build" (cdr (assoc "description"
                                              (mevedel-collaboration--json-record event)))))
      (should (equal "cancelled" (plist-get terminal :status)))
      (should (= 2 (plist-get (plist-get terminal :execution) :eventCount)))
      (should (equal "monitor" (plist-get local :kind)))
      (should (= 3 (plist-get local :eventCount)))
      (should-not (string-match-p "spoofed\\|999\\|second line" (prin1-to-string records)))
      (should (equal "/root/child"
                     (plist-get (plist-get
                                 (car (mevedel-collaboration--canonical-records
                                       (current-buffer) nil nil cache "/root/child"))
                                 :execution) :ownPath))))))

(ert-deftest mevedel-collaboration-monitor-event-empty-body ()
  "Empty monitor payloads project safely; missing IDs are not event records."
  (dolist (body '("" "   \n\t"))
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (insert "<agent-message type=\"MONITOR\" sender=\"/root\" "
              "execution_id=\"monitor-empty\" description=\"Watch build\">\n"
              body "\n</agent-message>\n")
      (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
        (should (= 1 (length records)))
        (should (equal "monitor-event" (plist-get (car records) :kind)))
        (should (equal "" (plist-get (car records) :text))))))
  (dolist (id '("" " execution_id=\"\""))
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (insert "<agent-message type=\"MONITOR\" sender=\"/root\"" id ">\n"
              "Build ready\n</agent-message>\n")
      (should-not
       (cl-find "monitor-event"
                (mevedel-collaboration--canonical-records (current-buffer))
                :key (lambda (record) (plist-get record :kind)) :test #'equal)))))

(ert-deftest mevedel-collaboration-background-wake-hidden ()
  "Only trusted wake attribution hides a prompt, preserving the response."
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((prompt "Background work has reported new information.\n"))
      (insert prompt)
      (insert (mevedel--format-hook-audit-record '(:type background-event)))
      (insert (propertize "I checked the result.\n" 'gptel 'response))
      (let* ((cache (mevedel-collaboration--projection-cache-create))
             (records (mevedel-collaboration--canonical-records
                       (current-buffer) nil nil cache)))
        (should (equal '("assistant") (mapcar (lambda (r) (plist-get r :kind)) records)))
        (should (equal records (mevedel-collaboration--canonical-records
                                (current-buffer) nil nil cache))))
      (erase-buffer)
      (insert prompt)
      (should (equal "user" (plist-get
                             (car (mevedel-collaboration--canonical-records
                                   (current-buffer))) :kind))))))

(provide 'test-mevedel-collaboration-projection)
;;; test-mevedel-collaboration-projection.el ends here
