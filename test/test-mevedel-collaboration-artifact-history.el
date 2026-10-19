;;; test-mevedel-collaboration-artifact-history.el --- Artifact answers -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the authenticated discussion action against real archived turns.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'gptel)
(require 'gptel-org)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-collaboration-artifact-history)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-discussions)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-view-interaction)

(defmacro mevedel-artifact-history-test--with-room (&rest body)
  "Run BODY with an isolated ROOM, SESSION, GUEST and store ITEM."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "artifact-history-" t))
          (workspace (mevedel-workspace--create :type 'file :id "history" :root directory))
          (session (mevedel-session--create :name "History" :session-id "history-session"
                                            :workspace workspace :authority-mode 'pid-lock
                                            :save-path directory :current-segment 2))
          (buffer (generate-new-buffer " *artifact-history*"))
          (room (list :session session :data-buffer buffer :transport 'test
                      :guests (make-hash-table :test #'eql)))
          (guest (list :name "Reader" :writable nil))
          (mevedel-sessions-directory "sessions")
          (mevedel-session-persistence--list-sessions-cache (make-hash-table :test #'equal))
          (mevedel-collaboration--rooms (mevedel-test-room-registry room))
          (mevedel-artifact-store-changed-functions nil)
          reply)
     (unwind-protect
         (progn
           (puthash 1 guest (plist-get room :guests))
           (mevedel-session-set-root-buffer session buffer)
           (with-current-buffer buffer
             (delay-mode-hooks (org-mode))
             (setq-local mevedel--session session mevedel--workspace workspace))
           (let ((path (file-name-concat (mevedel-artifact-store-directory workspace)
                                       "page" "index.html")))
             (make-directory (file-name-directory path) t)
             (write-region "<p>Page</p>" nil path nil 'silent)
             (mevedel-artifact-store-create-meta workspace "page" "index.html"))
           ,@body)
       (kill-buffer buffer)
       (delete-directory directory t))))

(defun mevedel-artifact-history-test--turn (question answer &optional item)
  "Insert QUESTION and ANSWER about ITEM, with canonical message attribution."
  (mevedel--insert-user-turn question)
  (when item
    (insert (mevedel--format-hook-audit-record
             (list :type 'guest-prompt :name "Reader"
                   :shared (list :kind "artifact" :itemId item :questionId question
                                 :discussionId "general" :messageId question :text question)))))
  (insert (propertize (concat answer "\n") 'gptel 'response)))

(mevedel-deftest mevedel-collaboration-artifact-history ()
  ,test
  (test)
  :doc "view links receive archived item answers, never unrelated room turns"
  (mevedel-artifact-history-test--with-room
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (mevedel-artifact-history-test--turn "Review" "Archived answer" "artifact:page")
      (mevedel-artifact-history-test--turn "Private room question" "Private room answer")
      (let ((gptel--bounds nil)) (gptel--save-state))
      (write-region (point-min) (point-max)
                    (mevedel-session-artifacts-segment-path directory 1) nil 'silent))
    (mevedel-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page" :discussionId "general")
     (lambda (value) (setq reply value)))
    (should-not (plist-get reply :error))
    (let ((records (json-parse-string (json-encode (plist-get reply :conversation))
                                      :object-type 'plist :array-type 'list)))
      (should (equal '("user" "assistant") (mapcar (lambda (r) (plist-get r :kind)) records)))
      (should (string-match-p "Archived answer" (prin1-to-string records)))
      (should-not (string-match-p "Private room" (prin1-to-string records)))
      (should (equal "Review" (plist-get (plist-get (car records) :shared) :messageId))))
    ;; An unchanged poll must perform neither archive reads nor projection.
    (let ((digest (plist-get reply :conversationDigest)) (reads 0) (projections 0))
      (cl-letf* ((read-original (symbol-function 'mevedel-session-artifacts-read-segment))
                 (project-original (symbol-function 'mevedel-collaboration--canonical-records))
                 ((symbol-function 'mevedel-session-artifacts-read-segment)
                  (lambda (&rest args) (cl-incf reads) (apply read-original args)))
                 ((symbol-function 'mevedel-collaboration--canonical-records)
                  (lambda (&rest args) (cl-incf projections) (apply project-original args))))
        (mevedel-collaboration-discussions-perform
         room guest (list :action "history" :item "artifact:page" :digest digest)
         (lambda (value) (setq reply value)))
        (should (eq t (plist-get reply :conversationUnchanged)))
        (should-not (plist-member reply :conversation))
        (should (= reads 0)) (should (= projections 0))
        (with-current-buffer buffer
          (mevedel-artifact-history-test--turn "Review" "New live answer" "artifact:page"))
        (mevedel-collaboration-discussions-perform
         room guest (list :action "history" :item "artifact:page" :digest digest)
         (lambda (value) (setq reply value)))
        (should-not (plist-get reply :error))
        (should (= 2 (length (plist-get reply :conversation))))
        (should-not (equal digest (plist-get reply :conversationDigest)))
        (should (string-match-p "New live answer" (prin1-to-string reply)))
        (should-not (string-match-p "Archived answer" (prin1-to-string reply)))
        (should (= projections 1)))))

  :doc "reports unavailable archives and bounded whole-turn omissions explicitly"
  (mevedel-artifact-history-test--with-room
    (mevedel-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page")
     (lambda (value) (setq reply value)))
    (should (string-match-p "Segment file is missing" (plist-get reply :error)))
    (with-current-buffer buffer
      (mevedel-artifact-history-test--turn "Review" "Long live answer" "artifact:page"))
    (let ((mevedel-shared-conversation--history-limit 1))
      (mevedel-collaboration-discussions-perform
       room guest '(:action "history" :item "artifact:page")
       (lambda (value) (setq reply value))))
    (should-not (plist-get reply :error))
    (should (eq t (plist-get reply :conversationTruncated)))
    (should (= 0 (length (plist-get reply :conversation)))))

  :doc "wire responses strip host-only fields from canonical records"
  (mevedel-artifact-history-test--with-room
    (setf (mevedel-session-current-segment session) 1)
    (with-current-buffer buffer
      (mevedel-artifact-history-test--turn "Review" "Answer" "artifact:page"))
    (let ((original (symbol-function 'mevedel-collaboration--canonical-records)) (injected 0))
      (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
                 (lambda (&rest args)
                   (let ((records (apply original args)))
                     (cl-incf injected)
                     (setcar records (plist-put (car records) :artifact-path "/host/private/path"))
                     records))))
        (mevedel-collaboration-discussions-perform
         room guest '(:action "history" :item "artifact:page")
         (lambda (value) (setq reply value))))
      (should-not (plist-get reply :error))
      (should (= injected 1))
      (should-not (string-match-p "artifact-path\\|/host/private/path" (json-encode reply)))))

  :doc "dedicated lobby history notices interaction-only changes without sharing or reprojecting"
  (mevedel-artifact-history-test--with-room
    (let ((view (generate-new-buffer " *artifact-decision*"))
          (lobby (list :workspace workspace))
          (item (list :store "page" :workspace workspace :item "artifact:page"))
          (project-original (symbol-function 'mevedel-collaboration--canonical-records))
          (projections 0))
      (unwind-protect
          (progn
            (setf (mevedel-session-current-segment session) 1
                  (mevedel-session-dedicated-artifact session) "page")
            (mevedel-artifact-store-update-meta workspace "page" :dedicated-session "history-session")
            (with-current-buffer buffer (setq-local mevedel--view-buffer view))
            (with-current-buffer view
              (setq-local mevedel-view--interaction-descriptors (make-hash-table :test #'equal)))
            (setq reply (mevedel-collaboration-artifact-history lobby guest item nil))
            (let ((digest (plist-get reply :conversationDigest))
                  (tick (with-current-buffer buffer (buffer-chars-modified-tick))))
              (with-current-buffer view
                (puthash "ask" '(:type ask :body "PRIVATE QUESTION BODY")
                         mevedel-view--interaction-descriptors))
              (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
                         (lambda (&rest args) (cl-incf projections) (apply project-original args))))
                (setq reply (mevedel-collaboration-artifact-history
                             lobby guest item (list :digest digest))))
              (should-not (plist-get reply :conversationUnchanged))
              (should (eq t (plist-get (plist-get reply :conversationDecision) :pending)))
              (should (equal "History" (plist-get (plist-get reply :conversationDecision) :title)))
              (should-not (string-match-p "PRIVATE QUESTION BODY\\|ui-request\\|:link" (prin1-to-string reply)))
              (should (= projections 0))
              (should (= tick (with-current-buffer buffer (buffer-chars-modified-tick))))
              (setq digest (plist-get reply :conversationDigest))
              (setq reply (mevedel-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should (eq t (plist-get reply :conversationUnchanged)))
              ;; Replacing an interaction while another is still pending must
              ;; also invalidate the digest, not just false/true transitions.
              (with-current-buffer view
                (remhash "ask" mevedel-view--interaction-descriptors)
                (puthash "permission" '(:body "PRIVATE APPROVAL BODY")
                         mevedel-view--interaction-descriptors))
              (setq reply (mevedel-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should-not (plist-get reply :conversationUnchanged))
              (should-not (string-match-p "PRIVATE APPROVAL BODY" (prin1-to-string reply)))
              (setq digest (plist-get reply :conversationDigest))
              (setf (mevedel-session-name session) (make-string 1000 ?x))
              (setq reply (mevedel-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should-not (plist-get reply :conversationUnchanged))
              (should (<= (length (plist-get (plist-get reply :conversationDecision) :title)) 160))
              (setq digest (plist-get reply :conversationDigest))
              (should-not (plist-get (mevedel-collaboration-artifact-history room guest item nil)
                                     :conversationDecision))
              (with-current-buffer view (clrhash mevedel-view--interaction-descriptors))
              (setq reply (mevedel-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should-not (plist-get reply :conversationUnchanged))
              (should-not (plist-get reply :conversationDecision))
              (should (= 1 (hash-table-count mevedel-collaboration--rooms)))))
        (kill-buffer view))))

  :doc "checks continuing authority, unknown threads and read budgets"
  (mevedel-artifact-history-test--with-room
    (mevedel-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page")
     (lambda (value) (setq reply value)) (lambda () nil))
    (should (equal "Discussion authority ended" (plist-get reply :error)))
    (mevedel-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page" :discussionId "unknown")
     (lambda (value) (setq reply value)))
    (should (equal "No such discussion" (plist-get reply :error)))
    (plist-put guest :store-reads (cons (float-time) 100))
    (mevedel-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page")
     (lambda (value) (setq reply value)))
    (should (string-match-p "Too many requests" (plist-get reply :error)))))

(mevedel-deftest mevedel-collaboration-artifact-history--source ()
  ,test
  (test)
  :doc "room never reads another answering session; lobby follows only the selected session"
  (mevedel-artifact-history-test--with-room
    (let* ((item (list :store "page" :workspace workspace))
           (discussion (list :session "history-session")))
      (should (eq session (car (mevedel-collaboration-artifact-history--source
                               room item '(:session "another-session")))))
      (should (eq buffer (cdr (mevedel-collaboration-artifact-history--source
                              nil item discussion))))
      (should-not (mevedel-collaboration-artifact-history--source nil item nil))
      (should-error (mevedel-collaboration-artifact-history--source
                     nil item '(:session "gone")))))

  :doc "cold answering sessions deserialize read-only; missing sessions fall back to existing dedicated history"
  (mevedel-artifact-history-test--with-room
    (let* ((cold (mevedel-session-create "Cold answering" workspace))
           (id (mevedel-session-session-id cold))
           (path (file-name-as-directory
                  (file-name-concat (mevedel-session-artifacts-sessions-dir workspace) id)))
           (item (list :store "page" :workspace workspace)))
      (make-directory path t)
      (setf (mevedel-session-save-path cold) path
            (mevedel-session-authority-mode cold) 'pid-lock)
      (mevedel-workspace-identity-ensure directory)
      (mevedel-session-codec-write (file-name-concat path "session.meta.el")
                                  (mevedel-session-codec-serialize cold))
      (let* ((source (mevedel-collaboration-artifact-history--source
                      nil item (list :session id)))
             (restored (car source)))
        (should (equal id (mevedel-session-session-id restored)))
        (should (equal path (mevedel-session-save-path restored)))
        (should-not (cdr source))
        (should-not (mevedel-session-root-buffer restored))
        (should-not (file-exists-p (file-name-concat path ".lock"))))
      (mevedel-artifact-store-update-meta workspace "page" :dedicated-session "history-session")
      (should (eq buffer (cdr (mevedel-collaboration-artifact-history--source
                              nil item '(:session "gone"))))))))

(mevedel-deftest mevedel-collaboration-artifact-history--identity ()
  ,test
  (test)
  :doc "source identity changes on live edits, archive recovery and atomic archive replacement"
  (mevedel-artifact-history-test--with-room
    (let ((before (mevedel-collaboration-artifact-history--identity session buffer)))
      (with-current-buffer buffer (insert "Changed"))
      (should-not (equal before (mevedel-collaboration-artifact-history--identity session buffer)))
      (setq before (mevedel-collaboration-artifact-history--identity session buffer))
      (write-region "Archive restored" nil
                    (mevedel-session-artifacts-segment-path directory 1) nil 'silent)
      (should-not (equal before (mevedel-collaboration-artifact-history--identity session buffer)))
      (setq before (mevedel-collaboration-artifact-history--identity session buffer))
      (let ((replacement (make-temp-file (file-name-concat directory "replace-"))))
        (write-region "Replacement archive" nil replacement nil 'silent)
        (rename-file replacement (mevedel-session-artifacts-segment-path directory 1) t))
      (should-not (equal before (mevedel-collaboration-artifact-history--identity session buffer)))))

  :doc "poll source identity probes only the current segment, regardless of archive count"
  (mevedel-artifact-history-test--with-room
    (setf (mevedel-session-current-segment session) 400)
    (let* ((probes 0) (probe (lambda (&rest _) (cl-incf probes))))
      (unwind-protect
          (progn
            (advice-add 'mevedel-session-artifacts-transcript-segment-identity :before probe)
            (mevedel-collaboration-artifact-history--identity session buffer)
            (should (= probes 1)))
        (advice-remove 'mevedel-session-artifacts-transcript-segment-identity probe)))))

(mevedel-deftest mevedel-collaboration-artifact-history--project
  (:doc "projects attributed message identities from disposable cold current and archived segments")
  (mevedel-artifact-history-test--with-room
    (setf (mevedel-session-current-segment session) 1)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (mevedel-artifact-history-test--turn "Cold review" "Cold answer" "artifact:page")
      (let ((gptel--bounds nil)) (gptel--save-state))
      (write-region (point-min) (point-max)
                    (mevedel-session-artifacts-segment-path directory 1) nil 'silent))
    (let ((result (mevedel-collaboration-artifact-history--project session nil "artifact:page")))
      (should (= 2 (length (plist-get result :conversation))))
      (should (string-match-p "Cold answer" (prin1-to-string result)))
      (should (equal "Cold review"
                     (plist-get (plist-get (aref (plist-get result :conversation) 0) :shared)
                                :messageId))))))

(provide 'test-mevedel-collaboration-artifact-history)
;;; test-mevedel-collaboration-artifact-history.el ends here
