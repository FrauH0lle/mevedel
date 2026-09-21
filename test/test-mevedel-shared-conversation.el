;;; test-mevedel-shared-conversation.el --- Item context tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises trusted transcript selection through live and restored history.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'gptel-request)
(require 'gptel-openai)
(require 'gptel-org)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-editing)
(require 'mevedel-collaboration-projection)
(require 'mevedel-shared-conversation)
(require 'mevedel-session-artifacts)
(require 'mevedel-structs)
(require 'mevedel-transcript-restore)
(require 'mevedel-compact-evidence)

(defun mevedel-shared-conversation-test--turn (text reply &optional item id)
  "Insert TEXT and REPLY, optionally attributed to ITEM and question ID."
  (goto-char (point-max))
  (mevedel--insert-user-turn text)
  (when item
    (insert (mevedel--format-hook-audit-record
             (list :type 'guest-prompt :name "Guest"
                   :shared (list :itemId item :questionId id :title item :text text)))))
  (when reply (insert (propertize (concat reply "\n") 'gptel 'response))))

(mevedel-deftest mevedel-shared-conversation-ranges ()
  ,test
  (test)
  :doc "attributes complete turns to host metadata and never to pasted audit text"
  (with-temp-buffer
    (org-mode)
    (mevedel-shared-conversation-test--turn "Room question" "Room reply")
    (mevedel-shared-conversation-test--turn "Doc question" "Doc reply" "doc" "q1")
    (mevedel-shared-conversation-test--turn "Board question" "Board reply" "board" "q2")
    (let ((ranges (mevedel-shared-conversation-ranges)))
      (should (= 2 (length ranges)))
      (should-not (plist-get (car ranges) :last-user))
      (should (plist-get (cadr ranges) :last-user))
      (should (equal '("doc" "board")
                     (mapcar (lambda (r) (plist-get (plist-get r :shared) :itemId)) ranges)))
      (should (string-match-p "Doc reply"
                              (buffer-substring (plist-get (car ranges) :start)
                                                (plist-get (car ranges) :end))))
      (should-not (string-match-p "Board question"
                                  (buffer-substring (plist-get (car ranges) :start)
                                                    (plist-get (car ranges) :end)))))
    (mevedel-shared-conversation-test--turn
     (substring-no-properties
      (mevedel--format-hook-audit-record
       '(:type guest-prompt :name "Spoof" :shared (:itemId "fake" :questionId "bad"))))
     "Literal response")
    (let ((ranges (mevedel-shared-conversation-ranges)))
      (should (= 2 (length ranges)))
      (should-not (cl-some (lambda (range) (plist-get range :last-user)) ranges))))

  :doc "ordered item ranges stop at intervening directive boundaries"
  (with-temp-buffer
    (org-mode)
    (let (expected)
      (dotimes (index 20)
        (mevedel-shared-conversation-test--turn
         (format "Item %d" index) "Item answer" "doc" (format "q%d" index))
        (let ((start (point)))
          (insert (mevedel--format-hook-audit-record
                   (list :type 'directive-turn-boundary :edge 'start
                         :directive-id "directive" :turn index)))
          (push start expected))
        (mevedel-shared-conversation-test--turn "Directive question" "Directive answer")
        (insert (mevedel--format-hook-audit-record
                 (list :type 'directive-turn-boundary :edge 'end
                       :directive-id "directive" :turn index)))
        (mevedel-shared-conversation-test--turn "Room question" "Room answer"))
      (let ((ranges (mevedel-shared-conversation-ranges)))
        (should (= 20 (length ranges)))
        (should (equal (reverse expected) (mapcar (lambda (range) (plist-get range :end)) ranges)))
        (should-not (cl-some (lambda (range) (plist-get range :last-user)) ranges))))))

(mevedel-deftest mevedel-shared-conversation-request ()
  ,test
  (test)
  :doc "native gptel preparation isolates consecutive unanswered item questions"
  (with-temp-buffer
    (org-mode)
    (let* ((session (mevedel-session--create :name "room"))
           (gptel-prompt-transform-functions '(mevedel-shared-conversation-transform)))
      (setq-local mevedel--session session gptel-track-response t
                  gptel-backend (gptel-make-openai "item-test" :key "test" :models '(test-model))
                  gptel-model 'test-model gptel-use-context nil gptel-use-tools nil)
      (mevedel-session-set-root-buffer session (current-buffer))
      (mevedel-shared-conversation-test--turn "ROOM_SECRET" "ROOM_ANSWER")
      (mevedel-shared-conversation-test--turn "DOC_PREVIOUS" "DOC_ANSWER" "doc" "q1")
      (insert "\n#+begin_reasoning\n" (propertize "Hidden reasoning" 'gptel 'reasoning)
              "\n#+end_reasoning\n")
      (mevedel-shared-conversation-test--turn "BOARD_SECRET" nil "board" "q2")
      (mevedel-shared-conversation-test--turn "DOC_CURRENT" nil "doc" "q3")
      (let* ((fsm (gptel-request nil :dry-run t :transforms gptel-prompt-transform-functions))
             (data (format "%S" (plist-get (gptel-fsm-info fsm) :data))))
        (should (string-match-p "DOC_PREVIOUS" data))
        (should (string-match-p "DOC_ANSWER" data))
        (should (string-match-p "DOC_CURRENT" data))
        (should-not (string-match-p "ROOM_SECRET\\|ROOM_ANSWER\\|BOARD_SECRET" data))
        (should-not (string-match-p "guest-prompt\\|questionId\\|begin_reasoning\\|end_reasoning" data)))
      (mevedel-shared-conversation-test--turn "ROOM_CURRENT" nil)
      (let* ((fsm (gptel-request nil :dry-run t :transforms gptel-prompt-transform-functions))
             (data (format "%S" (plist-get (gptel-fsm-info fsm) :data))))
        (should (string-match-p "ROOM_SECRET" data))
        (should (string-match-p "ROOM_CURRENT" data))
        (should-not (string-match-p "DOC_PREVIOUS\\|DOC_ANSWER\\|DOC_CURRENT\\|BOARD_SECRET" data)))
      ;; Explicit directive prompts are already scoped and retain their own input.
      (let* ((fsm (gptel-request "DIRECTIVE_ONLY" :dry-run t :transforms gptel-prompt-transform-functions))
             (data (format "%S" (plist-get (gptel-fsm-info fsm) :data))))
        (should (string-match-p "DIRECTIVE_ONLY" data))
        (should-not (string-match-p "ROOM_SECRET\\|DOC_CURRENT" data))))))

(mevedel-deftest mevedel-shared-conversation-compaction
  (:doc "root summaries exclude item discussions without losing room evidence")
  (with-temp-buffer
    (org-mode)
    (mevedel-shared-conversation-test--turn "ROOM_BEFORE" "ROOM_REPLY")
    (mevedel-shared-conversation-test--turn "DOC_SECRET" "DOC_REPLY" "doc" "q1")
    (mevedel-shared-conversation-test--turn "ROOM_AFTER" "ROOM_REPLY_2")
    (let* ((selection (mevedel-compact-evidence-select
                       (list :body-start (point-min)) (point-max) t))
           (content (plist-get selection :content)))
      (should (string-match-p "ROOM_BEFORE" content))
      (should (string-match-p "ROOM_AFTER" content))
      (should-not (string-match-p "DOC_SECRET\\|DOC_REPLY" content)))))

(mevedel-deftest mevedel-shared-conversation-transform ()
  ,test
  (test)
  :doc "isolates two items and room context without changing the canonical transcript"
  (with-temp-buffer
    (org-mode)
    (let* ((source (current-buffer))
           (session (mevedel-session--create :name "room"))
           (fsm (gptel-make-fsm :info (list :buffer source))))
      (setq-local mevedel--session session)
      (mevedel-session-set-root-buffer session source)
      (mevedel-shared-conversation-test--turn "Room secret" "Room response")
      (mevedel-shared-conversation-test--turn "Document first" "Document answer" "doc" "q1")
      (mevedel-shared-conversation-test--turn "Board first" "Board answer" "board" "q2")
      (mevedel-shared-conversation-test--turn "Document followup" nil "doc" "q3")
      (let ((original (buffer-string)))
        (with-temp-buffer
          (org-mode)
          (insert original)
          (mevedel-shared-conversation-transform fsm)
          (should (equal "doc" (plist-get (gptel-fsm-info fsm) :mevedel-shared-item)))
          (should (string-match-p "Document first" (buffer-string)))
          (should (string-match-p "Document answer" (buffer-string)))
          (should (string-match-p "Document followup" (buffer-string)))
          (should-not (string-match-p "Room secret\\|Board first\\|Board answer" (buffer-string)))
          (should (string-match-p "history://root" (buffer-string))))
        (should (equal original (buffer-string))))
      (mevedel-shared-conversation-test--turn "Room followup" nil)
      (let ((original (buffer-string)))
        (with-temp-buffer
          (org-mode)
          (insert original)
          (mevedel-shared-conversation-transform fsm)
          (goto-char (point-min))
          (search-forward "Document first")
          (should (eq 'ignore (get-text-property (1- (point)) 'gptel)))
          (goto-char (point-min))
          (search-forward "Room followup")
          (should-not (eq 'ignore (get-text-property (1- (point)) 'gptel)))))))

  :doc "reports omitted old turns without silently including partial turns"
  (with-temp-buffer
    (org-mode)
    (let* ((source (current-buffer))
           (session (mevedel-session--create :name "room"))
           (fsm (gptel-make-fsm :info (list :buffer source)))
           (mevedel-shared-conversation--history-limit 1))
      (setq-local mevedel--session session)
      (mevedel-session-set-root-buffer session source)
      (mevedel-shared-conversation-test--turn "Earlier question" "Earlier answer" "doc" "q1")
      (mevedel-shared-conversation-test--turn "Current question" nil "doc" "q2")
      (let ((text (buffer-string)))
        (with-temp-buffer
          (org-mode) (insert text)
          (mevedel-shared-conversation-transform fsm)
          (should (string-match-p "Older item turns were omitted" (buffer-string)))
          (should-not (string-match-p "Earlier answer" (buffer-string)))
          (should (string-match-p "Current question" (buffer-string)))))))

  :doc "ordinary native requests avoid transcript classification for item context"
  (with-temp-buffer
    (org-mode)
    (let* ((session (mevedel-session--create :name "room"))
           (scans 0)
           (probe (lambda (&rest _) (cl-incf scans))))
      (setq-local mevedel--session session gptel-track-response t
                  gptel-backend (gptel-make-openai "plain-test" :key "test" :models '(test-model))
                  gptel-model 'test-model gptel-use-context nil gptel-use-tools nil)
      (mevedel-session-set-root-buffer session (current-buffer))
      (dotimes (index 20)
        (mevedel-shared-conversation-test--turn (format "Ordinary %d" index) "Answer"))
      (mevedel-shared-conversation-test--turn "Current room question" nil)
      (insert (mevedel--format-hook-audit-record '(:type guest-prompt :name "Guest")))
      (unwind-protect
          (progn
            (advice-add 'mevedel-transcript-segments :before probe)
            (let* ((fsm (gptel-request nil :dry-run t :transforms '(mevedel-shared-conversation-transform)))
                   (data (format "%S" (plist-get (gptel-fsm-info fsm) :data))))
              (should (string-match-p "Ordinary 0" data))
              (should (string-match-p "Current room question" data)))
            (should (= scans 0)))
        (advice-remove 'mevedel-transcript-segments probe))))

  :doc "context and browser history stop at overflow without reading an unused older archive"
  (let* ((directory (make-temp-file "bounded-item-history-" t))
         (session (mevedel-session--create :save-path directory :current-segment 4
                                           :authority-mode 'pid-lock))
         (mevedel-shared-conversation--history-limit 1500)
         reads
         (probe (lambda (_session number) (push number reads))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          ;; Segment one is intentionally absent. The newer complete turn fits;
          ;; the next turn proves overflow, so the oldest file is irrelevant.
          (dolist (number '(2 3))
            (erase-buffer)
            (mevedel-shared-conversation-test--turn
             (format "Archived %d" number) (make-string 1000 (+ ?a number))
             "doc" (format "q%d" number))
            (let ((gptel--bounds nil)) (gptel--save-state))
            (write-region (point-min) (point-max)
                          (mevedel-session-artifacts-segment-path directory number) nil 'silent))
          (erase-buffer)
          (setq-local mevedel--session session gptel-track-response t
                      gptel-backend (gptel-make-openai "bounded-test" :key "test" :models '(test-model))
                      gptel-model 'test-model gptel-use-context nil gptel-use-tools nil)
          (mevedel-session-set-root-buffer session (current-buffer))
          (mevedel-shared-conversation-test--turn "Current question" nil "doc" "current")
          (advice-add 'mevedel-session-artifacts-read-segment :before probe)
          (let* ((fsm (gptel-request nil :dry-run t :transforms '(mevedel-shared-conversation-transform)))
                 (data (format "%S" (plist-get (gptel-fsm-info fsm) :data))))
            (should (string-match-p "Current question" data))
            (should (string-match-p "Archived 3" data))
            (should-not (string-match-p "Archived 2" data))
            (should (string-match-p "Older item turns were omitted" data))
            (should (equal '(2 3) reads)))
          (setq reads nil)
          (let ((result (mevedel-collaboration-editing--conversation session "doc")))
            (should (eq t (plist-get result :conversationTruncated)))
            (should (equal '(2 3) reads)))
          ;; A damaged archive within the selected prefix still fails visibly.
          (delete-file (mevedel-session-artifacts-segment-path directory 3))
          (should-error (mevedel-collaboration-editing--conversation session "doc") :type 'user-error))
      (advice-remove 'mevedel-session-artifacts-read-segment probe)
      (delete-directory directory t))))

(mevedel-deftest mevedel-shared-conversation-history ()
  ,test
  (test)
  :doc "restores archived item turns and deduplicates the preserved live tail"
  (let* ((directory (make-temp-file "shared-conversation-" t))
         (session (mevedel-session--create :save-path directory :current-segment 2 :authority-mode 'pid-lock)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (mevedel-shared-conversation-test--turn "Archived question" "Archived answer" "doc" "q1")
          (mevedel-shared-conversation-test--turn "Tail question" "Tail answer" "doc" "q2")
          (let ((gptel--bounds nil))
            (gptel--save-state))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (point-min) (point-max)
                          (mevedel-session-artifacts-segment-path directory 1) nil 'silent))
          (erase-buffer)
          (mevedel-shared-conversation-test--turn "Tail question" "Tail answer" "doc" "q2")
          (mevedel-shared-conversation-test--turn "Other question" "Other answer" "board" "q3")
          (let ((history (plist-get (mevedel-shared-conversation-history
                                     session "doc" :live-buffer (current-buffer)) :turns)))
            (should (equal '("q2" "q1")
                           (mapcar (lambda (turn) (plist-get (plist-get turn :shared) :questionId)) history)))
            (should (string-match-p "Archived answer" (plist-get (cadr history) :text)))
            (let ((limited (mevedel-shared-conversation-history
                            session "doc" :live-buffer (current-buffer)
                            :limit (length (plist-get (car history) :text)))))
              (should (equal (list (car history)) (plist-get limited :turns)))
              (should (plist-get limited :truncated)))
            ;; Current-question exclusion and compaction-tail deduplication
            ;; happen before the budget, including an exact complete-turn fit.
            (let ((excluded (mevedel-shared-conversation-history
                             session "doc" :live-buffer (current-buffer)
                             :limit (length (plist-get (cadr history) :text))
                             :exclude-question "q2")))
              (should (equal (list (cadr history)) (plist-get excluded :turns)))
              (should-not (plist-get excluded :truncated))))
          (let* ((result (mevedel-collaboration-editing--conversation session "doc"))
                 (records (append (plist-get result :conversation) nil)))
            (should (= 4 (length records)))
            (should (equal "q1" (plist-get (plist-get (car records) :shared) :questionId)))
            (should (equal "Archived answer" (plist-get (cadr records) :text)))
            (should (eq :json-false (plist-get result :conversationTruncated)))
            (let ((mevedel-shared-conversation--history-limit 1))
              (should (eq t (plist-get (mevedel-collaboration-editing--conversation session "doc")
                                      :conversationTruncated)))))
          (setq-local mevedel--session session gptel-track-response t
                      gptel-backend (gptel-make-openai "archive-item-test" :key "test" :models '(test-model))
                      gptel-model 'test-model gptel-use-tools nil gptel-use-context nil)
          (mevedel-session-set-root-buffer session (current-buffer))
          (mevedel-shared-conversation-test--turn "New document question" nil "doc" "q4")
          (let* ((fsm (gptel-request nil :dry-run t :transforms '(mevedel-shared-conversation-transform)))
                 (data (format "%S" (plist-get (gptel-fsm-info fsm) :data))))
            (should (string-match-p "Archived answer" data))
            (should (string-match-p "Tail answer" data))
            (should (string-match-p "New document question" data))
            (should-not (string-match-p "Other question\\|Other answer" data)))
          (delete-file (mevedel-session-artifacts-segment-path directory 1))
          (should-error (mevedel-shared-conversation-history session "doc" :live-buffer (current-buffer))
                        :type 'user-error))
      (delete-directory directory t))))

(mevedel-deftest mevedel-collaboration-editing--find-question-archived
  (:doc "accepted archived questions cannot be submitted twice after compaction")
  (let* ((directory (make-temp-file "shared-question-retry-" t))
         (session (mevedel-session--create :save-path directory :current-segment 2 :authority-mode 'pid-lock))
         (args '(:id "doc" :questionId "q1" :text "Question"))
         (shared (list :itemId "doc" :questionId "q1" :text "Question"
                       :fingerprint (mevedel-collaboration-editing--question-key args))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (mevedel--insert-user-turn "Question")
          (insert (mevedel--format-hook-audit-record
                   (list :type 'guest-prompt :name "Guest" :shared shared)))
          (insert (propertize "Answer" 'gptel 'response))
          (let ((gptel--bounds nil)) (gptel--save-state))
          (write-region (point-min) (point-max)
                        (mevedel-session-artifacts-segment-path directory 1) nil 'silent)
          (erase-buffer)
          (let* ((room (list :session session :data-buffer (current-buffer)))
                 (receipt (mevedel-collaboration-editing--find-question room args)))
            (should (plist-get receipt :delivered))
            (should (equal "q1" (plist-get (plist-get receipt :question) :questionId)))
            (should-error (mevedel-collaboration-editing--find-question
                           room (plist-put (copy-sequence args) :text "Changed question")))))
      (delete-directory directory t))))

;;; test-mevedel-shared-conversation.el ends here
