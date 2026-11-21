;;; test-niwa-collaboration-artifact-history.el --- Artifact answers -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the authenticated discussion action against real archived turns.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-session-test-support"))
(require 'gptel)
(require 'gptel-org)
(require 'niwa-collaboration)
(require 'niwa-collaboration-artifact)
(require 'niwa-collaboration-artifact-history)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-discussions)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-view-interaction)

(defmacro niwa-artifact-history-test--raw-segments (&rest body)
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

(defmacro niwa-artifact-history-test--with-room (&rest body)
  "Run BODY with an isolated ROOM, SESSION, GUEST and store ITEM."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "artifact-history-" t))
          (workspace (niwa-workspace--create :type 'file :id "history" :root directory))
          (session (niwa-session--create :name "History" :session-id "history-session"
                                            :workspace workspace :authority-mode 'portable
                                            :save-path directory :current-segment 2))
          (buffer (generate-new-buffer " *artifact-history*"))
          (room (list :session session :data-buffer buffer :transport 'test
                      :guests (make-hash-table :test #'eql)))
          (guest (list :name "Reader" :writable nil))
          (niwa-sessions-directory "sessions")
          (niwa-session-persistence--list-sessions-cache (make-hash-table :test #'equal))
          (niwa-collaboration--rooms (niwa-test-room-registry room))
          (niwa-artifact-store-changed-functions nil)
          reply)
     (unwind-protect
         (progn
           (puthash 1 guest (plist-get room :guests))
           (niwa-session-set-root-buffer session buffer)
           (with-current-buffer buffer
             (delay-mode-hooks (org-mode))
             (setq-local niwa--session session niwa--workspace workspace))
           (let ((path (file-name-concat (niwa-artifact-store-directory workspace)
                                       "page" "index.html")))
             (make-directory (file-name-directory path) t)
             (write-region "<p>Page</p>" nil path nil 'silent)
             (niwa-artifact-store-create-meta workspace "page" "index.html"))
           (niwa-artifact-history-test--raw-segments ,@body))
       (kill-buffer buffer)
       (delete-directory directory t))))

(defun niwa-artifact-history-test--turn (question answer &optional item)
  "Insert QUESTION and ANSWER about ITEM, with canonical message attribution."
  (niwa--insert-user-turn question)
  (when item
    (insert (niwa--format-hook-audit-record
             (list :type 'guest-prompt :name "Reader"
                   :shared (list :kind "artifact" :itemId item :questionId question
                                 :discussionId "general" :messageId question :text question)))))
  (insert (propertize (concat answer "\n") 'gptel 'response)))

(niwa-deftest niwa-collaboration-artifact-history ()
  ,test
  (test)
  :doc "view links receive archived item answers, never unrelated room turns"
  (niwa-artifact-history-test--with-room
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (niwa-artifact-history-test--turn "Review" "Archived answer" "artifact:page")
      (niwa-artifact-history-test--turn "Private room question" "Private room answer")
      (let ((gptel--bounds nil)) (gptel--save-state))
      (write-region (point-min) (point-max)
                    (niwa-session-artifacts-segment-path directory 1) nil 'silent))
    (niwa-collaboration-discussions-perform
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
      (cl-letf* ((read-original (symbol-function 'niwa-session-artifacts-read-segment))
                 (project-original (symbol-function 'niwa-collaboration--canonical-records))
                 ((symbol-function 'niwa-session-artifacts-read-segment)
                  (lambda (&rest args) (cl-incf reads) (apply read-original args)))
                 ((symbol-function 'niwa-collaboration--canonical-records)
                  (lambda (&rest args) (cl-incf projections) (apply project-original args))))
        (niwa-collaboration-discussions-perform
         room guest (list :action "history" :item "artifact:page" :digest digest)
         (lambda (value) (setq reply value)))
        (should (eq t (plist-get reply :conversationUnchanged)))
        (should-not (plist-member reply :conversation))
        (should (= reads 0)) (should (= projections 0))
        (with-current-buffer buffer
          (niwa-artifact-history-test--turn "Review" "New live answer" "artifact:page"))
        (niwa-collaboration-discussions-perform
         room guest (list :action "history" :item "artifact:page" :digest digest)
         (lambda (value) (setq reply value)))
        (should-not (plist-get reply :error))
        (should (= 2 (length (plist-get reply :conversation))))
        (should-not (equal digest (plist-get reply :conversationDigest)))
        (should (string-match-p "New live answer" (prin1-to-string reply)))
        (should-not (string-match-p "Archived answer" (prin1-to-string reply)))
        (should (= projections 1)))))

  :doc "reports unavailable archives and bounded whole-turn omissions explicitly"
  (niwa-artifact-history-test--with-room
    (niwa-test--with-captured-messages nil
      (niwa-collaboration-discussions-perform
       room guest '(:action "history" :item "artifact:page")
       (lambda (value) (setq reply value))))
    (should (equal "An answering-session transcript segment is unavailable" (plist-get reply :error)))
    (with-current-buffer buffer
      (niwa-artifact-history-test--turn "Review" "Long live answer" "artifact:page"))
    (let ((niwa-shared-conversation--history-limit 1))
      (niwa-collaboration-discussions-perform
       room guest '(:action "history" :item "artifact:page")
       (lambda (value) (setq reply value))))
    (should-not (plist-get reply :error))
    (should (eq t (plist-get reply :conversationTruncated)))
    (should (= 0 (length (plist-get reply :conversation)))))

  :doc "archive failures expose only logical segments and keep host diagnostics local"
  (dolist (failure '(unreadable missing read-error cold-current))
    (niwa-artifact-history-test--with-room
      (let* ((number (if (eq failure 'cold-current) 2 1))
             (path (niwa-session-artifacts-segment-path directory number))
             (messages nil)
             (original (symbol-function 'insert-file-contents)))
        (with-current-buffer buffer
          (niwa-artifact-history-test--turn "Recent review" "Partial live answer" "artifact:page"))
        (pcase failure
          ('unreadable (make-directory path))
          ('read-error (write-region "Archived text" nil path nil 'silent))
          ('cold-current (plist-put room :data-buffer nil)))
        (cl-letf (((symbol-function 'insert-file-contents)
                   (lambda (filename &rest args)
                     (if (and (eq failure 'read-error) (equal filename path))
                         (signal 'file-error (list "PRIVATE READ DETAIL" filename))
                       (apply original filename args)))))
          (niwa-test--with-captured-messages messages
            (niwa-collaboration-discussions-perform
             room guest '(:action "history" :item "artifact:page")
             (lambda (value) (setq reply value)))))
        (should-not (string-match-p (regexp-quote directory) (json-encode reply)))
        (should-not (string-match-p "PRIVATE READ DETAIL" (json-encode reply)))
        (should-not (plist-member reply :conversation))
        (should-not (plist-get room :artifact-conversations))
        (should (equal (if (eq failure 'cold-current)
                           (format "Session history segment %d is unavailable" number)
                         "An answering-session transcript segment is unavailable")
                       (plist-get reply :error)))
        (should (string-match-p (regexp-quote path) messages))
        (should (string-match-p
                 (pcase failure
                   ('unreadable "Is a directory")
                   ('read-error "PRIVATE READ DETAIL")
                   (_ "Segment file is missing"))
                 messages)))))

  :doc "wire responses strip host-only fields from canonical records"
  (niwa-artifact-history-test--with-room
    (setf (niwa-session-current-segment session) 1)
    (with-current-buffer buffer
      (niwa-artifact-history-test--turn "Review" "Answer" "artifact:page"))
    (let ((original (symbol-function 'niwa-collaboration--canonical-records)) (injected 0))
      (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
                 (lambda (&rest args)
                   (let ((records (apply original args)))
                     (cl-incf injected)
                     (setcar records (plist-put (car records) :artifact-path "/host/private/path"))
                     records))))
        (niwa-collaboration-discussions-perform
         room guest '(:action "history" :item "artifact:page")
         (lambda (value) (setq reply value))))
      (should-not (plist-get reply :error))
      (should (= injected 1))
      (should-not (string-match-p "artifact-path\\|/host/private/path" (json-encode reply)))))

  :doc "dedicated lobby history notices interaction-only changes without sharing or reprojecting"
  (niwa-artifact-history-test--with-room
    (let ((view (generate-new-buffer " *artifact-decision*"))
          (lobby (list :workspace workspace))
          (item (list :store "page" :workspace workspace :item "artifact:page"))
          (project-original (symbol-function 'niwa-collaboration--canonical-records))
          (projections 0))
      (unwind-protect
          (progn
            (setf (niwa-session-current-segment session) 1
                  (niwa-session-dedicated-artifact session) "page")
            (niwa-artifact-store-update-meta workspace "page" :dedicated-session "history-session")
            (with-current-buffer buffer (setq-local niwa--view-buffer view))
            (with-current-buffer view
              (setq-local niwa-view--interaction-descriptors (make-hash-table :test #'equal)))
            (setq reply (niwa-collaboration-artifact-history lobby guest item nil))
            (let ((digest (plist-get reply :conversationDigest))
                  (tick (with-current-buffer buffer (buffer-chars-modified-tick))))
              (with-current-buffer view
                (puthash "ask" '(:type ask :body "PRIVATE QUESTION BODY")
                         niwa-view--interaction-descriptors))
              (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
                         (lambda (&rest args) (cl-incf projections) (apply project-original args))))
                (setq reply (niwa-collaboration-artifact-history
                             lobby guest item (list :digest digest))))
              (should-not (plist-get reply :conversationUnchanged))
              (should (eq t (plist-get (plist-get reply :conversationDecision) :pending)))
              (should (equal "History" (plist-get (plist-get reply :conversationDecision) :title)))
              (should-not (string-match-p "PRIVATE QUESTION BODY\\|ui-request\\|:link" (prin1-to-string reply)))
              (should (= projections 0))
              (should (= tick (with-current-buffer buffer (buffer-chars-modified-tick))))
              (setq digest (plist-get reply :conversationDigest))
              (setq reply (niwa-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should (eq t (plist-get reply :conversationUnchanged)))
              ;; Replacing an interaction while another is still pending must
              ;; also invalidate the digest, not just false/true transitions.
              (with-current-buffer view
                (remhash "ask" niwa-view--interaction-descriptors)
                (puthash "permission" '(:body "PRIVATE APPROVAL BODY")
                         niwa-view--interaction-descriptors))
              (setq reply (niwa-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should-not (plist-get reply :conversationUnchanged))
              (should-not (string-match-p "PRIVATE APPROVAL BODY" (prin1-to-string reply)))
              (setq digest (plist-get reply :conversationDigest))
              (setf (niwa-session-name session) (make-string 1000 ?x))
              (setq reply (niwa-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should-not (plist-get reply :conversationUnchanged))
              (should (<= (length (plist-get (plist-get reply :conversationDecision) :title)) 160))
              (setq digest (plist-get reply :conversationDigest))
              (should-not (plist-get (niwa-collaboration-artifact-history room guest item nil)
                                     :conversationDecision))
              (with-current-buffer view (clrhash niwa-view--interaction-descriptors))
              (setq reply (niwa-collaboration-artifact-history lobby guest item (list :digest digest)))
              (should-not (plist-get reply :conversationUnchanged))
              (should-not (plist-get reply :conversationDecision))
              (should (= 1 (hash-table-count niwa-collaboration--rooms)))))
        (kill-buffer view))))

  :doc "checks continuing authority, unknown threads and read budgets"
  (niwa-artifact-history-test--with-room
    (niwa-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page")
     (lambda (value) (setq reply value)) (lambda () nil))
    (should (equal "Discussion authority ended" (plist-get reply :error)))
    (niwa-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page" :discussionId "unknown")
     (lambda (value) (setq reply value)))
    (should (equal "No such discussion" (plist-get reply :error)))
    (plist-put guest :store-reads (cons (float-time) 100))
    (niwa-collaboration-discussions-perform
     room guest '(:action "history" :item "artifact:page")
     (lambda (value) (setq reply value)))
    (should (string-match-p "Too many requests" (plist-get reply :error)))))

(niwa-deftest niwa-collaboration-artifact-history--source ()
  ,test
  (test)
  :doc "room never reads another answering session; lobby follows only the selected session"
  (niwa-artifact-history-test--with-room
    (let* ((item (list :store "page" :workspace workspace))
           (discussion (list :session "history-session")))
      (should (eq session (car (niwa-collaboration-artifact-history--source
                               room item '(:session "another-session")))))
      (should (eq buffer (cdr (niwa-collaboration-artifact-history--source
                              nil item discussion))))
      (should-not (niwa-collaboration-artifact-history--source nil item nil))
      (should-error (niwa-collaboration-artifact-history--source
                     nil item '(:session "gone")))))

  :doc "cold answering sessions deserialize read-only; missing sessions fall back to existing dedicated history"
  (niwa-artifact-history-test--with-room
    (let* ((cold (niwa-session-create "Cold answering" workspace))
           (id (niwa-session-session-id cold))
           (path (file-name-as-directory
                  (file-name-concat (niwa-session-artifacts-sessions-dir workspace) id)))
           (item (list :store "page" :workspace workspace)))
      ;; A real leased publication, released so the session is cold.
      (let ((data (generate-new-buffer " *cold answering*")))
        (with-current-buffer data
          (delay-mode-hooks (org-mode))
          (insert "Cold\n")
          (niwa-session-artifacts-ensure-files cold data)
          (test-niwa-session-persistence--publish-generation
           cold path (niwa-session-artifacts-segment-path path 1) "Cold\n" 1))
        (should (equal path (niwa-session-save-path cold)))
        (test-niwa-session-persistence--release-and-kill data cold))
      (let* ((source (niwa-collaboration-artifact-history--source
                      nil item (list :session id)))
             (restored (car source)))
        (should (equal id (niwa-session-session-id restored)))
        (should (equal path (niwa-session-save-path restored)))
        (should-not (cdr source))
        (should-not (niwa-session-root-buffer restored))
        (should-not (file-exists-p (file-name-concat path ".lock"))))
      (niwa-artifact-store-update-meta workspace "page" :dedicated-session "history-session")
      (should (eq buffer (cdr (niwa-collaboration-artifact-history--source
                              nil item '(:session "gone"))))))))

(niwa-deftest niwa-collaboration-artifact-history--identity ()
  ,test
  (test)
  :doc "source identity changes on live edits, archive recovery and atomic archive replacement"
  (niwa-artifact-history-test--with-room
    (let ((before (niwa-collaboration-artifact-history--identity session buffer)))
      (with-current-buffer buffer (insert "Changed"))
      (should-not (equal before (niwa-collaboration-artifact-history--identity session buffer)))
      (setq before (niwa-collaboration-artifact-history--identity session buffer))
      (write-region "Archive restored" nil
                    (niwa-session-artifacts-segment-path directory 1) nil 'silent)
      (should-not (equal before (niwa-collaboration-artifact-history--identity session buffer)))
      (setq before (niwa-collaboration-artifact-history--identity session buffer))
      (let ((replacement (make-temp-file (file-name-concat directory "replace-"))))
        (write-region "Replacement archive" nil replacement nil 'silent)
        (rename-file replacement (niwa-session-artifacts-segment-path directory 1) t))
      (should-not (equal before (niwa-collaboration-artifact-history--identity session buffer)))))

  :doc "poll source identity probes only the current segment, regardless of archive count"
  (niwa-artifact-history-test--with-room
    (setf (niwa-session-current-segment session) 400)
    (let* ((probes 0) (probe (lambda (&rest _) (cl-incf probes))))
      (unwind-protect
          (progn
            (advice-add 'niwa-session-artifacts-transcript-segment-identity :before probe)
            (niwa-collaboration-artifact-history--identity session buffer)
            (should (= probes 1)))
        (advice-remove 'niwa-session-artifacts-transcript-segment-identity probe)))))

(niwa-deftest niwa-collaboration-artifact-history--project
  (:doc "projects attributed message identities from disposable cold current and archived segments")
  (niwa-artifact-history-test--with-room
    (setf (niwa-session-current-segment session) 1)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (niwa-artifact-history-test--turn "Cold review" "Cold answer" "artifact:page")
      (let ((gptel--bounds nil)) (gptel--save-state))
      (write-region (point-min) (point-max)
                    (niwa-session-artifacts-segment-path directory 1) nil 'silent))
    (let ((result (niwa-collaboration-artifact-history--project session nil "artifact:page")))
      (should (= 2 (length (plist-get result :conversation))))
      (should (string-match-p "Cold answer" (prin1-to-string result)))
      (should (equal "Cold review"
                     (plist-get (plist-get (aref (plist-get result :conversation) 0) :shared)
                                :messageId))))))

(provide 'test-niwa-collaboration-artifact-history)
;;; test-niwa-collaboration-artifact-history.el ends here
