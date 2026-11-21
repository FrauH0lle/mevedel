;;; test-niwa-session-control-transfer.el -- Transfer coordinator tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests session-owned transfer decisions without constructing a view.

;;; Code:

(require 'niwa-notes)
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))
(require 'niwa-agent-control)
(require 'niwa-execution)
(require 'niwa-overlays)
(require 'niwa-persistence)
(require 'niwa-schedule)
(require 'niwa-session-control-transfer)
;; Loaded up front because the coordinator requires them lazily inside the
;; functions under test, which would otherwise redefine stubbed symbols
;; mid-test.
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-transcript-restore)
(require 'niwa-structs)
(require 'niwa-turn)
(require 'niwa-workspace)

(require 'niwa-transport)

(niwa-deftest niwa-session-control-transfer--install-staged-segment ()
  ,test
  (test)
  :doc "restores buffer text, properties, and metadata after hook failure"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-transfer-install-" t)))
         (old-file (file-name-concat root "old.chat.org"))
         (new-file (file-name-concat root "new.chat.org"))
         (buffer nil)
         (staging (generate-new-buffer " *niwa-transfer-install*")))
    (write-region "0123456789" nil old-file nil 'silent)
    (write-region "new transcript" nil new-file nil 'silent)
    (setq buffer (find-file-noselect old-file))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (put-text-property 4 7 'niwa-test-property t)
            (goto-char 6)
            (narrow-to-region 2 9)
            (set-buffer-modified-p t))
          (with-current-buffer staging
            (setq buffer-file-name new-file
                  default-directory root
                  buffer-file-coding-system 'utf-8-unix)
            (insert "new transcript"))
          (let ((file-name (buffer-local-value 'buffer-file-name buffer))
                (file-truename
                 (buffer-local-value 'buffer-file-truename buffer))
                (directory (buffer-local-value 'default-directory buffer))
                (coding
                 (buffer-local-value 'buffer-file-coding-system buffer))
                (modtime (with-current-buffer buffer
                           (visited-file-modtime))))
            (with-current-buffer buffer
              (add-hook 'before-change-functions
                        (lambda (&rest _) (error "Injected hook failure"))
                        nil t))
            (should-error
             (niwa-session-control-transfer--install-staged-segment
              buffer staging))
            (with-current-buffer buffer
              (should (equal file-name buffer-file-name))
              (should (equal file-truename buffer-file-truename))
              (should (equal directory default-directory))
              (should (eq coding buffer-file-coding-system))
              (should (equal modtime (visited-file-modtime)))
              (should (buffer-modified-p))
              (should (= 6 (point)))
              (should (= 2 (point-min)))
              (should (= 9 (point-max)))
              (save-restriction
                (widen)
                (should (equal "0123456789" (buffer-string)))
                (should (get-text-property 5 'niwa-test-property))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local before-change-functions nil)
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (buffer-live-p staging)
        (with-current-buffer staging (set-buffer-modified-p nil))
        (kill-buffer staging))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-control-transfer-drain-registry ()
  ,test
  (test)
  :doc "combines session drains and safely unregisters transient owners"
  (let ((session (niwa-session--create :name "transfer"))
        (pending t))
    (should (niwa-session-control-transfer-drained-p session))
    (let ((predicate (lambda () pending)))
      (should (eq predicate
                  (niwa-session-control-transfer-register-drain
                   session predicate)))
      (should-not (niwa-session-control-transfer-drained-p session))
      (setq pending nil)
      (should (niwa-session-control-transfer-drained-p session))
      (niwa-session-control-transfer-unregister-drain session predicate)
      (should-not (niwa-session-control-transfer-drains session)))))

(niwa-deftest niwa-session-control-transfer-drained-p ()
  ,test
  (test)
  :doc "keeps ownership while a lost turn settles"
  (let ((session (niwa-session--create :name "transfer")))
    (with-temp-buffer
      (cl-letf (((symbol-function
                  'niwa-session-control-transfer-root-buffer)
                 (lambda (_session) (current-buffer))))
        (setq-local niwa--turn-settlements-pending '(settlement))
        (should-not (niwa-session-control-transfer-drained-p session))
        (should (equal "a settling turn"
                       (niwa-session-control-transfer-drain-blocker
                        session))))))

  :doc "admission and explanation agree for queued and active session work"
  (with-temp-buffer
    (dolist (case '(("a publication" :pending-publication (:reason "pending"))
                    ("a publication" :publication-active-p t)
                    ("queued input" :pending-steering ("steering"))
                    ("queued input" :pending-follow-ups ("follow-up"))
                    ("a plan approval" :pending-plan-approval t)
                    ("a permission prompt" :permission-queue (prompt))
                    ("an agent turn" :agent-reservations (reservation))))
      (let ((session (apply #'niwa-session--create
                            :root-buffer (current-buffer) (cdr case))))
        (should-not (niwa-session-control-transfer-drained-p session))
        (should (equal (car case)
                       (niwa-session-control-transfer-drain-blocker
                        session)))))
    (let ((session (niwa-session--create :root-buffer (current-buffer))))
      (setq-local niwa--current-request 'request)
      (should-not (niwa-session-control-transfer-drained-p session))
      (should (equal "a running request"
                     (niwa-session-control-transfer-drain-blocker session)))))

  :doc "a failing transient drain blocks both handoff and its explanation"
  (let* ((session (niwa-session--create))
         (predicate (lambda () (error "Injected drain failure"))))
    (niwa-session-control-transfer-register-drain session predicate)
    (should-not (niwa-session-control-transfer-drained-p session))
    (should (equal "the view"
                   (niwa-session-control-transfer-drain-blocker session)))
    (niwa-session-control-transfer-unregister-drain session predicate)
    (should (niwa-session-control-transfer-drained-p session))
    (should-not (niwa-session-control-transfer-drain-blocker session))))

(niwa-deftest niwa-session-control-transfer-descriptor ()
  ,test
  (test)
  :doc "gives the owner the decision and the requester its own status"
  (let ((session (niwa-session--create :name "transfer")))
    (setf (niwa-session-control-transfer session)
          '(:state requested
            :request (:requester-label "Laptop")))
    (let ((owner (niwa-session-control-transfer-descriptor session nil)))
      (should (eq 'grant (plist-get owner :action)))
      (should (plist-get owner :attention))
      (should (string-match-p "Laptop" (plist-get owner :title)))
      (should (equal '("g" "k") (mapcar #'car (plist-get owner :keys)))))
    ;; The requester shares the durable state but can decide nothing; the
    ;; owner's prompt on its screen offers a choice it does not have.
    (let ((requester (niwa-session-control-transfer-descriptor session t)))
      (should (eq 'status (plist-get requester :action)))
      (should (equal '(("b" . "take over")) (plist-get requester :keys)))
      (should-not (plist-get requester :attention)))
    (setf (niwa-session-control-transfer session)
          '(:state quiescing :request (:requester-label "Laptop")))
    (should
     (string-match-p
      "Publishing\\|Finishing"
      (plist-get (niwa-session-control-transfer-descriptor session nil)
                 :detail)))
    (should
     (string-match-p
      "finish"
      (plist-get (niwa-session-control-transfer-descriptor session t)
                 :detail)))
    (setf (niwa-session-control-transfer session) '(:state rejected))
    (let ((declined (niwa-session-control-transfer-descriptor session t)))
      (should (eq 'request (plist-get declined :action)))
      (should (plist-get declined :attention)))
    (setf (niwa-session-control-transfer session) '(:state released))
    (let ((idle (niwa-session-control-transfer-descriptor session t)))
      (should (eq 'request (plist-get idle :action)))
      ;; A permanent banner in the same colour as a live decision teaches
      ;; the user to stop seeing both.
      (should-not (plist-get idle :attention))
      (should (equal '("r" "b") (mapcar #'car (plist-get idle :keys)))))
    ;; A writable session with no request has nothing to say.
    (should-not
     (niwa-session-control-transfer-descriptor session nil))))

(niwa-deftest niwa-session-control-transfer-drain-blocker ()
  ,test
  (test)
  :doc "names the first thing holding a handoff open, and nothing when clear"
  (let ((session (niwa-session--create :name "transfer")))
    (should-not (niwa-session-control-transfer-drain-blocker session))
    (let ((predicate (lambda () t)))
      (niwa-session-control-transfer-register-drain session predicate)
      (should (equal "the view"
                     (niwa-session-control-transfer-drain-blocker
                      session)))
      (niwa-session-control-transfer-unregister-drain
       session predicate))
    (setf (niwa-session-pending-publication session) t)
    (should (equal "a publication"
                   (niwa-session-control-transfer-drain-blocker session)))
    (cl-letf (((symbol-function 'niwa-execution-session-live-p)
               (lambda (_session) t)))
      ;; A live execution outranks a queued publication: it is the wait the
      ;; user can actually end.
      (should (equal "a live execution"
                     (niwa-session-control-transfer-drain-blocker
                      session))))))

(niwa-deftest niwa-session-control-transfer--insert-committed-segment ()
  ,test
  (test)
  :doc "prepares a bare staging buffer as a transcript before restoring state"
  ;; The staging buffer is created with `generate-new-buffer\', so it starts
  ;; in `fundamental-mode\': the segment\'s persisted gptel state is only
  ;; restorable in the transcript\'s own major mode, and `gptel-mode\'
  ;; refuses every other one outright.
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-transfer-stage-" t)))
         (save-path (file-name-as-directory
                     (file-name-concat root "session")))
         (session (test-niwa-session-persistence--leased-session save-path
                   :name "stage" :working-directory root))
         (buffer (generate-new-buffer " *niwa-transfer-stage*")))
    (unwind-protect
        (progn
          (make-directory save-path t)
          (write-region "committed transcript\n" nil
                        (niwa-session-artifacts-segment-path save-path 1)
                        nil 'silent)
          (setf (niwa-session-save-path session) save-path
                (niwa-session-current-segment session) 1
                (niwa-session-working-directory session) root)
          (niwa-session-publication-publish
           session
           (list (list :path (niwa-session-artifacts-segment-path save-path 1)
                       :content "committed transcript\n")
                 (list :path (file-name-concat save-path "session.meta.el")
                       :content (niwa-session-artifacts-printed-value
                                 (niwa-session-codec-serialize session))
                       :commit-marker t)))
          (should (eq 'fundamental-mode
                      (buffer-local-value 'major-mode buffer)))
          (niwa-session-control-transfer--insert-committed-segment
           session buffer)
          (with-current-buffer buffer
            (should (derived-mode-p 'org-mode))
            (should (bound-and-true-p gptel-mode))
            (should (string-match-p "committed transcript" (buffer-string)))
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (setq-local kill-buffer-hook nil))
        (kill-buffer buffer))
      (niwa-session-persistence-lock-release save-path session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))


(niwa-deftest niwa-session-control-transfer--follow-published ()
  ,test
  (test)
  :doc "advances a non-owner buffer only when the owner published something new"
  (let* ((root (make-temp-file "niwa-follow-" t))
         (follow-file (file-name-concat root "segment.chat.org"))
         (session (niwa-session--create :name "follow"))
         (buffer (generate-new-buffer " *niwa-follow*"))
         (head "generation-old")
         (inserts 0)
         (drain (lambda () t))
         fail-insert)
    (write-region "" nil follow-file nil 'silent)
    (setf (niwa-session-save-path session) "/session/"
          (niwa-session-publication session) '(:head "generation-old")
          (niwa-session-control-transfer-drains session) (list drain))
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-durability-publication-head)
              (lambda (&rest _) head))
             ((symbol-function 'niwa-session-publication-read)
              (lambda (&rest _) (list :head head :sidecar "/sidecar")))
             ((symbol-function 'niwa-session-persistence-load-sidecar)
              (lambda (&rest _) '(:version 1)))
             ((symbol-function 'niwa-session-codec-deserialize)
              (lambda (&rest _)
                (list :session (niwa-session--create :name "follow"))))
             ((symbol-function
               'niwa-session-control-transfer--insert-committed-segment)
              (lambda (_session target)
                (with-current-buffer target
                  (setq buffer-file-name follow-file)
                  (erase-buffer)
                  (insert head))
                (when fail-insert
                  (error "Injected transcript restore failure"))
                (cl-incf inserts))))
          ;; The head names the owner's committed generation, so an owner
          ;; that has published nothing new costs one observation and no
          ;; artifact reads at all.
          (should-not
           (niwa-session-control-transfer--follow-published
            session buffer))
          (should (= 0 inserts))
          (setq head "generation-new")
          (should
           (niwa-session-control-transfer--follow-published
            session buffer))
          (should (= 1 inserts))
          (should (equal "generation-new"
                         (plist-get (niwa-session-publication session)
                                    :head)))
          (should (equal "/session/" (niwa-session-save-path session)))
          (should (equal (list drain)
                         (niwa-session-control-transfer-drains session)))
          (should-not (niwa-session-control-transfer-drained-p session))
          (should (equal "generation-new"
                         (with-current-buffer buffer (buffer-string))))
          (setq head "generation-failed"
                fail-insert t)
          (should-error
           (niwa-session-control-transfer--follow-published
            session buffer))
          (should (equal "generation-new"
                         (plist-get (niwa-session-publication session)
                                    :head)))
          (should (equal "generation-new"
                         (with-current-buffer buffer (buffer-string))))
          (setq fail-insert nil)
          (should
           (niwa-session-control-transfer--follow-published
            session buffer))
          (should (equal "generation-failed"
                         (with-current-buffer buffer (buffer-string))))
          ;; Advancing again needs a further publication, not another tick.
          (should-not
           (niwa-session-control-transfer--follow-published
            session buffer))
          (should (= 2 inserts))
          ;; Local edits are what the transfer path refuses to discard; a
          ;; timer must not resolve that conflict on the user's behalf.
          (setq head "generation-newer")
          (with-current-buffer buffer (insert "local edit"))
          (should (buffer-modified-p buffer))
          (should-not
           (niwa-session-control-transfer--follow-published
            session buffer))
          (should (= 2 inserts))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          ;; Following off holds the buffer where it is, but an explicit
          ;; refresh still reads.
          (with-current-buffer buffer
            (setq-local niwa-session-follow-published nil))
          (should-not
           (niwa-session-control-transfer--follow-published
            session buffer))
          (should (= 2 inserts))
          (should
           (niwa-session-control-transfer--follow-published
            session buffer t))
          (should (= 3 inserts)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(niwa-deftest niwa-session-control-transfer--adopt-control ()
  ,test
  (test)
  :doc "stages every fallible restore before changing the acquired live state"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-transfer-adopt-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat root "session")))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "transfer"))
         (session (niwa-session-create "requester" workspace))
         (target (niwa-session-execution-target session))
         (niwa-user-dir (file-name-as-directory (file-name-concat root "user")))
         (due (+ (truncate (float-time)) 3600))
         (wake-up (list :id "w-000001" :note "Check CI" :timing (list :kind 'once :at due)
                        :created-at (- due 3600) :due due :expires-at nil :status 'pending))
         (buffer (generate-new-buffer " *niwa-transfer-live*"))
         (source-buffer (generate-new-buffer " *niwa-transfer-source*"))
         reference reference-id
         (old-file (file-name-concat session-dir "segment-0001.chat.org"))
         (new-file (file-name-concat session-dir "segment-0002.chat.org"))
         (request '(:request-id "request"))
         (drain (lambda () t))
         native-history
         failure
         (releases 0))
    (make-directory session-dir t)
    (write-region "old transcript" nil old-file nil 'silent)
    (write-region "new transcript" nil new-file nil 'silent)
    (with-current-buffer source-buffer
      (setq-local niwa--workspace workspace
                  buffer-file-name (file-name-concat root "source.txt"))
      (insert "reference")
      (write-region (point-min) (point-max) buffer-file-name nil 'silent)
      (setq reference (niwa-instruction-test--create-note-in source-buffer 1 10)
            reference-id (overlay-get reference 'niwa-uuid)))
    (setf (niwa-session-save-path session) session-dir
          (niwa-session-session-id session) "transfer-session"
          (niwa-session-working-directory session) root
          (niwa-session-turn-count session) 1
          (niwa-session-current-segment session) 1
          (niwa-session-publication session) '(:head "old")
          (niwa-session-control-transfer session)
          (list :state 'requested :request request)
          (niwa-session-control-transfer-drains session) (list drain)
          (niwa-session-lease session) '(:state owned))
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-file-name old-file
                default-directory root
                niwa--workspace workspace)
          (setq-local niwa--session session)
          (insert "old transcript")
          (set-buffer-modified-p nil)
          (set-visited-file-modtime)
          (setq buffer-read-only t
                niwa-session--read-only-mode t)
          (niwa-session-control-transfer-register-root-buffer
           session buffer)
          (cl-letf
              (((symbol-function 'niwa-session-publication-read)
                (lambda (&rest _) '(:head "new" :sidecar "/sidecar")))
               ((symbol-function 'niwa-session-persistence-load-sidecar)
                (lambda (&rest _) '(:version 1)))
               ((symbol-function 'niwa-session-codec-deserialize)
                (lambda (&rest _)
                  (list
                   :session
                   (niwa-session--create
                    :name "requester" :working-directory root
                    :external-conversations native-history
                    :schedules (list wake-up)
                    :turn-count 9 :current-segment 2))))
               ((symbol-function 'niwa-session-artifacts-read-artifact)
                (lambda (&rest _)
                  (if (eq failure 'artifact)
                      (error "Injected artifact failure")
                    "new transcript")))
               ((symbol-function
                 'niwa-session-artifacts-check-target-incarnation)
                (lambda (candidate &rest _)
                  (when (eq failure 'target)
                    (setf
                     (niwa-execution-target-observed-incarnation
                      (niwa-session-execution-target candidate))
                     "changed")
                    (error "Injected incarnation failure"))))
               ((symbol-function 'niwa-transcript-restore-gptel-state)
                (lambda ()
                  (when (eq failure 'transcript)
                    (error "Injected transcript failure"))))
               ((symbol-function
                 'niwa-session-artifacts-load-instructions)
                (lambda (&rest _)
                  (if (eq failure 'instructions)
                      (progn
                        (niwa--clear-instruction-state workspace)
                        nil)
                    t)))
               ((symbol-function 'niwa-session-durability-lease-release)
                (lambda (&rest _) (cl-incf releases))))
            (dolist (stage '(artifact transcript target instructions))
              (setq failure stage)
              (should-error
               (niwa-session-control-transfer--adopt-control
                session buffer))
              (should-not (memq buffer niwa-schedule--buffers))
              (should (= 1 (niwa-session-turn-count session)))
              (should (= 1 (niwa-session-current-segment session)))
              (should (equal '(:head "old")
                             (niwa-session-publication session)))
              (should (equal (list drain)
                             (niwa-session-control-transfer-drains session)))
              (should (eq target (niwa-session-execution-target session)))
              (should-not
               (niwa-execution-target-observed-incarnation target))
              (should (equal old-file buffer-file-name))
              (should (equal "old transcript" (buffer-string)))
              (should buffer-read-only)
              (should niwa-session--read-only-mode)
              (let ((restored
                     (niwa--instruction-with-uuid reference-id workspace)))
                (should (eq reference restored))
                (should (overlayp restored))
                (with-current-buffer (overlay-buffer restored)
                  (should (equal "reference"
                                 (buffer-substring-no-properties
                                  (overlay-start restored)
                                  (overlay-end restored)))))))
            (setq failure nil)
            (setq native-history '(("root" :id "native-history")))
            (should-error
             (niwa-session-control-transfer--adopt-control session buffer)
             :type 'user-error)
            (should niwa-session--read-only-mode)
            (niwa-test--with-captured-messages nil
              (should
               (niwa-session-control-transfer--adopt-control
                session buffer t)))
            (should (equal native-history
                           (niwa-session-external-conversations session)))
            (should (= 9 (niwa-session-turn-count session)))
            (should (= 2 (niwa-session-current-segment session)))
            (should (equal "new transcript" (buffer-string)))
            (should-not buffer-read-only)
            (should-not niwa-session--read-only-mode)
            ;; Regaining the lease arms the wake-ups the owner left pending.
            (should (memq buffer niwa-schedule--buffers))
            (should niwa-schedule--timer)
            (should (equal (list drain)
                           (niwa-session-control-transfer-drains session)))
            (should-not
             (niwa-session-control-transfer-drained-p session))
            (niwa-session-control-transfer-unregister-drain session drain)
            (should (niwa-session-control-transfer-drained-p session))
            (should (= 5 releases))))
      (niwa-session-control-transfer-unregister-root-buffer session buffer)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      ;; Closing keeps a leased session's wake-up scheduled; forget it.
      (setq niwa-schedule--closed nil)
      (niwa-schedule--rearm)
      (should-not niwa-schedule--timer)
      (niwa--clear-instruction-state workspace)
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-session-control-transfer-poll ()
  ,test
  (test)
  :doc "defers target I/O while the transport or a publication is busy"
  (let ((session (niwa-session--create :name "transfer"))
        (owner 0)
        (requester 0))
    (cl-letf (((symbol-function
                'niwa-session-control-transfer--poll-owner)
               (lambda (_session) (cl-incf owner) 'owner))
              ((symbol-function
                'niwa-session-control-transfer--poll-requester)
               (lambda (_session _buffer) (cl-incf requester) 'requester)))
      (should (eq 'owner
                  (niwa-session-control-transfer-poll session nil nil)))
      (should (eq 'requester
                  (niwa-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      ;; Emacs runs filters and timers wherever the main loop waits,
      ;; including inside a TRAMP operation; polling from there wedges the
      ;; operation already running.
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _path) t)))
        (should-not
         (niwa-session-control-transfer-poll session nil nil))
        (should-not
         (niwa-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (niwa-session-publication-active-p session) t)
      (should-not (niwa-session-control-transfer-poll session nil nil))
      (should-not (niwa-session-control-transfer-poll session nil t))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (niwa-session-publication-active-p session) nil)
      (should (eq 'owner
                  (niwa-session-control-transfer-poll session nil nil)))
      (should (= 2 owner)))
    ;; A non-owner that did not acquire control still follows: the same tick
    ;; is what advances a joined client through the owner's published turns.
    (cl-letf (((symbol-function
                'niwa-session-control-transfer--poll-requester)
               (lambda (&rest _) nil))
              ((symbol-function
                'niwa-session-control-transfer--follow-published)
               (lambda (&rest _) 'followed)))
      (should (eq 'followed
                  (niwa-session-control-transfer-poll session nil t))))))

(provide 'test-niwa-session-control-transfer)

;;; test-niwa-session-control-transfer.el ends here
