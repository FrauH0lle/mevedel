;;; test-mevedel-directive-activity.el -- Directive activity tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for workspace-owned directive activity views.

;;; Code:

(require 'mevedel-directive-activity)
(require 'mevedel-chat)
(require 'mevedel-overlays)
(require 'mevedel-persistence)
(require 'mevedel-session-persistence)
(require 'mevedel-workspace)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-directive-activity-test--make-directive (request)
  "Return a workspace, source buffer, and directive for REQUEST."
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "activity" :root "/tmp"
                     :name "activity"))
         (buffer (generate-new-buffer " *directive-activity-source*")))
    (with-current-buffer buffer
      (insert "source")
      (setq-local mevedel--workspace workspace)
      (list workspace buffer
            (mevedel--create-directive-in
             buffer (point-min) (point-max) nil request)))))

(defun mevedel-directive-activity-test--discard (fixture)
  "Discard source and activity buffers belonging to FIXTURE."
  (when-let* ((buffer (cadr fixture))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq-local kill-buffer-hook nil))
    (kill-buffer buffer))
  (when-let* ((workspace (car fixture)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (eq workspace
                     (buffer-local-value
                      'mevedel-directive-activity--workspace buffer)))
        (kill-buffer buffer)))))


;;
;;; Activity surface

(mevedel-deftest mevedel-open-directive-activity
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "opens one managed activity buffer from an attached source directive"
  (let* ((fixture (mevedel-directive-activity-test--make-directive
                   "Initial request"))
         (workspace (car fixture))
         (directive (caddr fixture))
         (record (car (mevedel-workspace-directives workspace))))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (let ((activity (mevedel-open-directive-activity directive)))
            (should (buffer-live-p activity))
            (with-current-buffer activity
              (should (derived-mode-p 'mevedel-directive-activity-mode))
              (should (eq workspace mevedel-directive-activity--workspace))
              (should (eq record mevedel-directive-activity--directive))
              (should (string-match-p "Initial request" (buffer-string)))
              (should (string-match-p "Ready" (buffer-string)))
              (should (string-match-p "Attached" (buffer-string)))
              (should (string-match-p "No activity yet" (buffer-string)))
              (should (markerp mevedel-directive-activity--input-marker))
              (should (string-match-p "C-c RET: discuss" (buffer-string))))
            (should-not
             (string-match-p
              "No activity yet"
              (substring-no-properties
               (overlay-get directive 'before-string))))))
      (mevedel-directive-activity-test--discard fixture)))

  :doc "reuses the activity buffer and its workspace record"
  (let* ((fixture (mevedel-directive-activity-test--make-directive "One"))
         (directive (caddr fixture)))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (let ((first (mevedel-open-directive-activity directive))
                (second (mevedel-open-directive-activity directive)))
            (should (eq first second))
            (should
             (eq (buffer-local-value
                  'mevedel-directive-activity--directive first)
                 (buffer-local-value
                  'mevedel-directive-activity--directive second)))))
      (mevedel-directive-activity-test--discard fixture)))

  :doc "opens source-missing activity without a live source overlay"
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Missing"))
         (workspace (car fixture))
         (directive (caddr fixture))
         (record (car (mevedel-workspace-directives workspace))))
    (unwind-protect
        (progn
          (mevedel-directive-set-anchor record '(:state source-missing))
          (delete-overlay directive)
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) buffer)))
            (let ((activity
                   (mevedel-open-directive-activity record workspace)))
              (with-current-buffer activity
                (should (string-match-p "Source missing" (buffer-string)))
                (should (string-match-p "Missing" (buffer-string)))))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-refresh
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "rerenders request edits as Ready with a request-changed qualifier")
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Before"))
         (workspace (car fixture))
         (record (car (mevedel-workspace-directives workspace)))
         activity)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (setq activity
                (mevedel-open-directive-activity (caddr fixture)))
          (setf (mevedel-directive-attempts record)
                (list
                 (mevedel-directive-attempt--create
                  :directive-request "Before" :outcome 'success
                  :checkpoint '(:session-id "session-1" :turn 1)))
                (mevedel-directive-state record) 'implemented)
          (mevedel-directive-set-request record "After")
          (with-current-buffer activity
            (mevedel-directive-activity-refresh)
            (should (string-match-p "After" (buffer-string)))
            (should-not (string-match-p "Before" (buffer-string)))
            (should (string-match-p "Ready · request changed"
                                    (buffer-string)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity--input-text
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "preserves a multiline leading-> composer draft and point on redraw")
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Discuss"))
         (workspace (car fixture))
         (record (car (mevedel-workspace-directives workspace)))
         (draft "> first line\nsecond line")
         activity)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (setq activity (mevedel-open-directive-activity (caddr fixture)))
          (with-current-buffer activity
            (goto-char mevedel-directive-activity--input-marker)
            (insert draft)
            (goto-char (+ mevedel-directive-activity--input-marker 7))
            (setf (mevedel-directive-state record) 'discussing)
            (mevedel-directive-activity-refresh)
            (should (equal draft (mevedel-directive-activity--input-text)))
            (should (= (point)
                       (+ mevedel-directive-activity--input-marker 7)))
            (should (string-match-p "Discussing" (buffer-string)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity--discussion-fragments
  (:doc "renders durable local discussion and its attached attempt")
  (let ((directive
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :discussion
          (list
           (mevedel-directive-discussion-turn--create
            :message "What changed?"
            :request "Exact discussion request"
            :result "The parser changed."
            :outcome 'success
            :attempt-index 1
            :checkpoint '(:session-id "session-1" :turn 2))))))
    (let ((fragments
           (mevedel-directive-activity--discussion-fragments directive)))
      (should (= 1 (length fragments)))
      (let ((text (plist-get (car fragments) :body)))
        (should (string-match-p "What changed?" text))
        (should (string-match-p "The parser changed" text))
        (should (string-match-p "Attempt 1" text))
        (should-not (string-match-p "Exact discussion request" text))))))

(mevedel-deftest mevedel-directive-activity--activity-fragments
  (:doc "interleaves attempts and discussions by settlement sequence")
  (let* ((attempt-1
          (mevedel-directive-attempt--create
           :sequence 1 :request "First" :result "Done" :outcome 'success
           :patch "" :capture 'complete
           :checkpoint '(:session-id "session-1" :turn 1)))
         (discussion
          (mevedel-directive-discussion-turn--create
           :sequence 2 :message "Question" :request "Exact"
           :result "Answer" :outcome 'success
           :checkpoint '(:session-id "session-1" :turn 2)))
         (attempt-2
          (mevedel-directive-attempt--create
           :sequence 3 :request "Second" :result "Done" :outcome 'success
           :patch "" :capture 'complete
           :checkpoint '(:session-id "session-1" :turn 3)))
         (directive
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :attempts (list attempt-1 attempt-2)
           :discussion (list discussion)))
         (fragments
          (mevedel-directive-activity--activity-fragments directive)))
    (should
     (equal '(attempt discussion attempt)
            (mapcar
             (lambda (fragment)
               (car (plist-get fragment :id)))
             fragments)))))

(mevedel-deftest mevedel-directive-activity-submit
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "submits the local draft with the selected attempt and clears it")
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Discuss"))
         (directive (caddr fixture))
         captured activity)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer))
                  ((symbol-function 'mevedel--discuss-directive-turn)
                   (lambda (selected message attempt-index callback)
                     (setq captured (list selected message attempt-index))
                     (funcall callback nil nil)
                     'accepted)))
          (setq activity (mevedel-open-directive-activity directive))
          (with-current-buffer activity
            (setq mevedel-directive-activity--selected-attempt-index 2)
            (goto-char mevedel-directive-activity--input-marker)
            (insert "> question\nmore")
            (should (eq 'accepted
                        (mevedel-directive-activity-submit)))
            (should (equal (list directive "> question\nmore" 2) captured))
            (should (equal "" (mevedel-directive-activity--input-text)))
            (should-not mevedel-directive-activity--selected-attempt-index)))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-set-action
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "dispatches multiline Request changes and optional-guidance Retry")
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Change"))
         (directive (caddr fixture))
         calls activity)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer))
                  ((symbol-function 'mevedel--request-directive-changes)
                   (lambda (selected feedback callback)
                     (push (list 'request-changes selected feedback) calls)
                     (funcall callback nil nil)
                     'changes-accepted))
                  ((symbol-function 'mevedel--retry-directive)
                   (lambda (selected guidance callback)
                     (push (list 'retry selected guidance) calls)
                     (funcall callback nil nil)
                     'retry-accepted)))
          (setq activity (mevedel-open-directive-activity directive))
          (with-current-buffer activity
            (mevedel-directive-activity-set-action 'request-changes)
            (should (string-match-p "REQUEST CHANGES" (buffer-string)))
            (goto-char mevedel-directive-activity--input-marker)
            (insert "> first\nsecond")
            (should (eq 'changes-accepted
                        (mevedel-directive-activity-submit)))
            (mevedel-directive-activity-set-action 'retry)
            (should (string-match-p "RETRY" (buffer-string)))
            (should (eq 'retry-accepted
                        (mevedel-directive-activity-submit))))
          (should
           (equal
            (list (list 'retry directive "")
                  (list 'request-changes directive "> first\nsecond"))
            calls)))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-discuss-result
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "selects the implementation attempt at point for the next question")
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Discuss"))
         (workspace (car fixture))
         (record (car (mevedel-workspace-directives workspace)))
         (attempt
          (mevedel-directive-attempt--create
           :request "Request" :result "Answer" :outcome 'success
           :patch "" :capture 'complete :covered-files nil :gaps nil
           :checkpoint '(:session-id "session-1" :turn 1)))
         activity)
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record) (list attempt))
          (setf (mevedel-directive-state record) 'implemented)
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) buffer)))
            (setq activity (mevedel-open-directive-activity (caddr fixture))))
          (with-current-buffer activity
            (goto-char (point-min))
            (let ((position
                   (text-property-any
                    (point-min) mevedel-directive-activity--composer-marker
                    'mevedel-view-zone-entry attempt)))
              (should position)
              (goto-char position)
              (mevedel-directive-activity-discuss-result)
              (should (eq 'discuss
                          mevedel-directive-activity--composer-action))
              (should (string-match-p "DISCUSSION" (buffer-string)))
              (should-not (string-match-p "implement this" (buffer-string)))
              (should (= 1
                         mevedel-directive-activity--selected-attempt-index))
              (should (= (point)
                         mevedel-directive-activity--input-marker)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-implement-this
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "starts implementation from the same directive's complete discussion")
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Implement"))
         (workspace (car fixture))
         (directive (caddr fixture))
         (record (car (mevedel-workspace-directives workspace)))
         captured activity)
    (unwind-protect
        (progn
          (setf (mevedel-directive-discussion record)
                (list
                 (mevedel-directive-discussion-turn--create
                  :message "Prefer the smaller API"
                  :request "Exact" :result "Agreed" :outcome 'success
                  :checkpoint '(:session-id "session-1" :turn 1))))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) buffer))
                    ((symbol-function 'mevedel--implement-discussion)
                     (lambda (selected callback)
                       (setq captured selected)
                       (funcall callback nil nil)
                       'accepted)))
            (setq activity (mevedel-open-directive-activity directive))
            (with-current-buffer activity
              (should (eq 'accepted
                          (mevedel-directive-activity-implement-this)))
              (should (eq directive captured)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-view-patch
  ()
  ,test
  (test)
  :doc "projects the selected attempt patch into the reusable patch viewer"
  (let ((attempt
         (mevedel-directive-attempt--create
          :request "Request" :result "Answer" :outcome 'success
          :patch "diff --git a/a b/a\n" :capture 'complete
          :covered-files '("/tmp/a") :gaps nil
          :checkpoint '(:session-id "session-1" :turn 1)))
        projected)
    (with-temp-buffer
      (insert (propertize "Attempt" 'mevedel-directive-attempt attempt))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'mevedel--replace-patch-buffer)
                 (lambda (patch) (setq projected patch))))
        (mevedel-directive-activity-view-patch)
        (should (equal "diff --git a/a b/a\n" projected)))))

  :doc "renders immutable attempt details and complete no-change capture"
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Current"))
         (workspace (car fixture))
         (record (car (mevedel-workspace-directives workspace)))
         activity)
    (unwind-protect
        (progn
          (setf (mevedel-directive-state record) 'implemented
                (mevedel-directive-session-id record) "session-1"
                (mevedel-directive-attempts record)
                (list
                 (mevedel-directive-attempt--create
                  :directive-request "Current"
                  :request "Exact submitted request"
                  :result "Exact answer"
                  :outcome 'success :patch "" :capture 'complete
                  :captured-at "2026-08-02T01:00:00+0200"
                  :covered-files '("/tmp/a") :gaps nil
                  :consumed-subdirectives
                  (list
                   (mevedel-subdirective--create
                    :id "detail-1" :request "Use the immutable detail"
                    :anchor '(:state attached)))
                  :checkpoint '(:session-id "session-1" :turn 2))))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) buffer)))
            (setq activity
                  (mevedel-open-directive-activity (caddr fixture))))
          (with-current-buffer activity
            (should (eq 'request-changes
                        mevedel-directive-activity--composer-action))
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "Implemented" text))
              (should (string-match-p "Exact submitted request" text))
              (should (string-match-p "Exact answer" text))
              (should (string-match-p "Use the immutable detail" text))
              (should (string-match-p "Complete capture; no changes" text))
              (should
               (string-match-p
                (regexp-quote "2026-08-02T01:00:00+0200") text))
              (should (string-match-p "session-1, turn 2" text)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-rewind
  ()
  ,test
  (test)
  :doc "rewinds from an effectful attempt through its exact execution session checkpoint"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'test :id "rewind" :root "/tmp" :name "rewind"))
         (record
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)))
         (attempt
          (mevedel-directive-attempt--create
           :outcome 'error :patch "" :capture 'incomplete
           :gaps '((:path "/tmp/missed" :reason "capture failed"))
           :checkpoint '(:session-id "session-1" :turn 2)))
         (session
          (mevedel-session--create :session-id "session-1"))
         (session-buffer (generate-new-buffer " *directive-rewind-session*"))
         rewound)
    (unwind-protect
        (progn
          (with-current-buffer session-buffer
            (setq-local mevedel--session session))
          (with-temp-buffer
            (mevedel-directive-activity-mode)
            (setq-local mevedel-directive-activity--workspace workspace
                        mevedel-directive-activity--directive record)
            (let ((position (point)))
              (insert (propertize "Attempt"
                                  'mevedel-view-zone-entry attempt))
              (goto-char position))
            (cl-letf
                (((symbol-function 'mevedel--workspace-sessions)
                  (lambda (_) (list (cons "main" session-buffer))))
                 ((symbol-function
                   'mevedel-session-persistence-rewind-checkpoint)
                  (lambda (owner checkpoint &optional buffer)
                    (setq rewound (list owner checkpoint buffer))
                    t)))
              (should (mevedel-directive-activity-rewind))))
          (should (eq workspace (car rewound)))
          (should (= 2 (plist-get (cadr rewound) :turn)))
          (should (eq session-buffer (caddr rewound)))
          (should (eq #'mevedel-directive-activity-rewind
                      (keymap-lookup
                       mevedel-directive-activity-mode-map "R"))))
      (kill-buffer session-buffer)))
  :doc "rejects attempts with a complete no-change capture"
  (let ((attempt
         (mevedel-directive-attempt--create
          :outcome 'success :patch "" :capture 'complete
          :covered-files nil :gaps nil
          :checkpoint '(:session-id "session-1" :turn 1))))
    (with-temp-buffer
      (mevedel-directive-activity-mode)
      (let ((position (point)))
        (insert (propertize "Attempt" 'mevedel-view-zone-entry attempt))
        (goto-char position))
      (should-error (mevedel-directive-activity-rewind)
                    :type 'user-error)))

  :doc "delegates a cold execution checkpoint to session persistence"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'test :id "cold-rewind" :root "/tmp" :name "cold-rewind"))
         (record
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)))
         (attempt
          (mevedel-directive-attempt--create
           :outcome 'success :capture 'complete :covered-files '("/tmp/a")
           :checkpoint '(:session-id "cold-session" :turn 4)))
         rewound)
    (unwind-protect
        (progn
          (mevedel-workspace-add-directive workspace record)
          (with-temp-buffer
            (mevedel-directive-activity-mode)
            (setq-local mevedel-directive-activity--workspace workspace
                        mevedel-directive-activity--directive record)
            (let ((position (point)))
              (insert (propertize "Attempt"
                                  'mevedel-view-zone-entry attempt))
              (goto-char position))
            (cl-letf
                (((symbol-function 'mevedel--workspace-sessions)
                  (lambda (_) nil))
                 ((symbol-function
                   'mevedel-session-persistence-rewind-checkpoint)
                  (lambda (owner checkpoint &optional buffer)
                    (setq rewound (list owner checkpoint buffer))
                    t))
                 ((symbol-function 'mevedel-directive-activity-refresh)
                  #'ignore))
              (should (mevedel-directive-activity-rewind))))
          (should (eq workspace (car rewound)))
          (should (= 4 (plist-get (cadr rewound) :turn)))
          (should-not (caddr rewound)))
      nil)))

(mevedel-deftest mevedel-list-directives
  (:doc "opens the selected workspace record without a source point")
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "list" :root "/tmp" :name "list"))
         (record (mevedel-directive--create
                  :id "directive-1" :request "Choose me"
                  :anchor '(:state detached) :state nil))
         selected)
    (mevedel-workspace-add-directive workspace record)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (caar collection)))
              ((symbol-function 'mevedel-open-directive-activity)
               (lambda (directive selected-workspace)
                 (setq selected (list directive selected-workspace)))))
      (mevedel-list-directives workspace)
      (should (equal (list record workspace) selected)))))

(mevedel-deftest mevedel-directive-activity-goto-source
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "navigates from activity to the live attached source anchor"
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Go"))
         (source (cadr fixture))
         (directive (caddr fixture))
         activity)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (set-buffer buffer)
                     buffer)))
          (setq activity (mevedel-open-directive-activity directive))
          (save-current-buffer
            (set-buffer activity)
            (mevedel-directive-activity-goto-source)
            (should (eq source (current-buffer)))
            (should (= (point) (overlay-start directive)))))
      (mevedel-directive-activity-test--discard fixture)))

  :doc "rejects navigation when no attached source is live"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "missing" :root "/tmp" :name "missing"))
         (record (mevedel-directive--create
                  :id "gone" :request "Gone"
                  :anchor '(:state detached) :state nil)))
    (with-temp-buffer
      (mevedel-directive-activity-mode)
      (setq-local mevedel-directive-activity--workspace workspace
                  mevedel-directive-activity--directive record)
      (should-error (mevedel-directive-activity-goto-source)
                    :type 'user-error))))

(provide 'test-mevedel-directive-activity)
;;; test-mevedel-directive-activity.el ends here
