;;; test-mevedel-session-control-transfer.el -- Transfer coordinator tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests session-owned transfer decisions without constructing a view.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-execution)
(require 'mevedel-overlays)
(require 'mevedel-persistence)
(require 'mevedel-session-control-transfer)
;; Loaded up front because the coordinator requires them lazily inside the
;; functions under test, which would otherwise redefine stubbed symbols
;; mid-test.
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-transcript-restore)
(require 'mevedel-structs)
(require 'mevedel-turn)
(require 'mevedel-workspace)

(require 'mevedel-transport)

(mevedel-deftest mevedel-session-control-transfer--install-staged-segment ()
  ,test
  (test)
  :doc "restores buffer text, properties, and metadata after hook failure"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-transfer-install-" t)))
         (old-file (file-name-concat root "old.chat.org"))
         (new-file (file-name-concat root "new.chat.org"))
         (buffer nil)
         (staging (generate-new-buffer " *mevedel-transfer-install*")))
    (write-region "0123456789" nil old-file nil 'silent)
    (write-region "new transcript" nil new-file nil 'silent)
    (setq buffer (find-file-noselect old-file))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (put-text-property 4 7 'mevedel-test-property t)
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
             (mevedel-session-control-transfer--install-staged-segment
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
                (should (get-text-property 5 'mevedel-test-property))))))
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

(mevedel-deftest mevedel-session-control-transfer-drain-registry ()
  ,test
  (test)
  :doc "combines session drains and safely unregisters transient owners"
  (let ((session (mevedel-session--create :name "transfer"))
        (pending t))
    (should (mevedel-session-control-transfer-drained-p session))
    (let ((predicate (lambda () pending)))
      (should (eq predicate
                  (mevedel-session-control-transfer-register-drain
                   session predicate)))
      (should-not (mevedel-session-control-transfer-drained-p session))
      (setq pending nil)
      (should (mevedel-session-control-transfer-drained-p session))
      (mevedel-session-control-transfer-unregister-drain session predicate)
      (should-not (mevedel-session-control-transfer-drains session)))))

(mevedel-deftest mevedel-session-control-transfer-drained-p ()
  ,test
  (test)
  :doc "keeps ownership while a lost turn settles"
  (let ((session (mevedel-session--create :name "transfer")))
    (with-temp-buffer
      (cl-letf (((symbol-function
                  'mevedel-session-control-transfer-root-buffer)
                 (lambda (_session) (current-buffer))))
        (setq-local mevedel--turn-settlements-pending '(settlement))
        (should-not (mevedel-session-control-transfer-drained-p session))
        (should (equal "a settling turn"
                       (mevedel-session-control-transfer-drain-blocker
                        session)))))))

(mevedel-deftest mevedel-session-control-transfer-descriptor ()
  ,test
  (test)
  :doc "gives the owner the decision and the requester its own status"
  (let ((session (mevedel-session--create :name "transfer")))
    (setf (mevedel-session-control-transfer session)
          '(:state requested
            :request (:requester-label "Laptop")))
    (let ((owner (mevedel-session-control-transfer-descriptor session nil)))
      (should (eq 'grant (plist-get owner :action)))
      (should (plist-get owner :attention))
      (should (string-match-p "Laptop" (plist-get owner :title)))
      (should (equal '("g" "k") (mapcar #'car (plist-get owner :keys)))))
    ;; The requester shares the durable state but can decide nothing; the
    ;; owner's prompt on its screen offers a choice it does not have.
    (let ((requester (mevedel-session-control-transfer-descriptor session t)))
      (should (eq 'status (plist-get requester :action)))
      (should-not (plist-get requester :keys))
      (should-not (plist-get requester :attention)))
    (setf (mevedel-session-control-transfer session)
          '(:state quiescing :request (:requester-label "Laptop")))
    (should
     (string-match-p
      "Publishing\\|Finishing"
      (plist-get (mevedel-session-control-transfer-descriptor session nil)
                 :detail)))
    (should
     (string-match-p
      "finish"
      (plist-get (mevedel-session-control-transfer-descriptor session t)
                 :detail)))
    (setf (mevedel-session-control-transfer session) '(:state rejected))
    (let ((declined (mevedel-session-control-transfer-descriptor session t)))
      (should (eq 'request (plist-get declined :action)))
      (should (plist-get declined :attention)))
    (setf (mevedel-session-control-transfer session) '(:state released))
    (let ((idle (mevedel-session-control-transfer-descriptor session t)))
      (should (eq 'request (plist-get idle :action)))
      ;; A permanent banner in the same colour as a live decision teaches
      ;; the user to stop seeing both.
      (should-not (plist-get idle :attention))
      (should (equal '("r") (mapcar #'car (plist-get idle :keys)))))
    ;; A writable session with no request has nothing to say.
    (should-not
     (mevedel-session-control-transfer-descriptor session nil))))

(mevedel-deftest mevedel-session-control-transfer-drain-blocker ()
  ,test
  (test)
  :doc "names the first thing holding a handoff open, and nothing when clear"
  (let ((session (mevedel-session--create :name "transfer")))
    (should-not (mevedel-session-control-transfer-drain-blocker session))
    (let ((predicate (lambda () t)))
      (mevedel-session-control-transfer-register-drain session predicate)
      (should (equal "the view"
                     (mevedel-session-control-transfer-drain-blocker
                      session)))
      (mevedel-session-control-transfer-unregister-drain
       session predicate))
    (setf (mevedel-session-pending-publication session) t)
    (should (equal "a publication"
                   (mevedel-session-control-transfer-drain-blocker session)))
    (cl-letf (((symbol-function 'mevedel-execution-session-live-p)
               (lambda (_session) t)))
      ;; A live execution outranks a queued publication: it is the wait the
      ;; user can actually end.
      (should (equal "a live execution"
                     (mevedel-session-control-transfer-drain-blocker
                      session))))))

(mevedel-deftest mevedel-session-control-transfer--insert-committed-segment ()
  ,test
  (test)
  :doc "prepares a bare staging buffer as a transcript before restoring state"
  ;; The staging buffer is created with `generate-new-buffer\', so it starts
  ;; in `fundamental-mode\': the segment\'s persisted gptel state is only
  ;; restorable in the transcript\'s own major mode, and `gptel-mode\'
  ;; refuses every other one outright.
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-transfer-stage-" t)))
         (save-path (file-name-as-directory
                     (file-name-concat root "session")))
         (session (mevedel-session--create
                   :name "stage" :authority-mode 'pid-lock))
         (buffer (generate-new-buffer " *mevedel-transfer-stage*")))
    (unwind-protect
        (progn
          (make-directory save-path t)
          (write-region "committed transcript\n" nil
                        (mevedel-session-artifacts-segment-path save-path 1)
                        nil 'silent)
          (setf (mevedel-session-save-path session) save-path
                (mevedel-session-current-segment session) 1
                (mevedel-session-working-directory session) root)
          (should (eq 'fundamental-mode
                      (buffer-local-value 'major-mode buffer)))
          (mevedel-session-control-transfer--insert-committed-segment
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
      (delete-directory root t))))


(mevedel-deftest mevedel-session-control-transfer--follow-published ()
  ,test
  (test)
  :doc "advances a non-owner buffer only when the owner published something new"
  (let* ((root (make-temp-file "mevedel-follow-" t))
         (follow-file (file-name-concat root "segment.chat.org"))
         (session (mevedel-session--create :name "follow"))
         (buffer (generate-new-buffer " *mevedel-follow*"))
         (head "generation-old")
         (inserts 0)
         (drain (lambda () t))
         fail-insert)
    (write-region "" nil follow-file nil 'silent)
    (setf (mevedel-session-save-path session) "/session/"
          (mevedel-session-publication session) '(:head "generation-old")
          (mevedel-session-control-transfer-drains session) (list drain))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-durability-publication-head)
              (lambda (&rest _) head))
             ((symbol-function 'mevedel-session-publication-read)
              (lambda (&rest _) (list :head head :sidecar "/sidecar")))
             ((symbol-function 'mevedel-session-persistence-load-sidecar)
              (lambda (&rest _) '(:version 1)))
             ((symbol-function 'mevedel-session-codec-deserialize)
              (lambda (&rest _)
                (list :session (mevedel-session--create :name "follow"))))
             ((symbol-function
               'mevedel-session-control-transfer--insert-committed-segment)
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
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 0 inserts))
          (setq head "generation-new")
          (should
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 1 inserts))
          (should (equal "generation-new"
                         (plist-get (mevedel-session-publication session)
                                    :head)))
          (should (equal "/session/" (mevedel-session-save-path session)))
          (should (equal (list drain)
                         (mevedel-session-control-transfer-drains session)))
          (should-not (mevedel-session-control-transfer-drained-p session))
          (should (equal "generation-new"
                         (with-current-buffer buffer (buffer-string))))
          (setq head "generation-failed"
                fail-insert t)
          (should-error
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (equal "generation-new"
                         (plist-get (mevedel-session-publication session)
                                    :head)))
          (should (equal "generation-new"
                         (with-current-buffer buffer (buffer-string))))
          (setq fail-insert nil)
          (should
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (equal "generation-failed"
                         (with-current-buffer buffer (buffer-string))))
          ;; Advancing again needs a further publication, not another tick.
          (should-not
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 2 inserts))
          ;; Local edits are what the transfer path refuses to discard; a
          ;; timer must not resolve that conflict on the user's behalf.
          (setq head "generation-newer")
          (with-current-buffer buffer (insert "local edit"))
          (should (buffer-modified-p buffer))
          (should-not
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 2 inserts))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          ;; Following off holds the buffer where it is, but an explicit
          ;; refresh still reads.
          (with-current-buffer buffer
            (setq-local mevedel-session-follow-published nil))
          (should-not
           (mevedel-session-control-transfer--follow-published
            session buffer))
          (should (= 2 inserts))
          (should
           (mevedel-session-control-transfer--follow-published
            session buffer t))
          (should (= 3 inserts)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-control-transfer--adopt-control ()
  ,test
  (test)
  :doc "stages every fallible restore before changing the acquired live state"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-transfer-adopt-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat root "session")))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "transfer"))
         (session (mevedel-session-create "requester" workspace))
         (target (mevedel-session-execution-target session))
         (buffer (generate-new-buffer " *mevedel-transfer-live*"))
         (source-buffer (generate-new-buffer " *mevedel-transfer-source*"))
         reference reference-id
         (old-file (file-name-concat session-dir "segment-0001.chat.org"))
         (new-file (file-name-concat session-dir "segment-0002.chat.org"))
         (request '(:request-id "request"))
         (drain (lambda () t))
         failure
         (releases 0))
    (make-directory session-dir t)
    (write-region "old transcript" nil old-file nil 'silent)
    (write-region "new transcript" nil new-file nil 'silent)
    (with-current-buffer source-buffer
      (setq-local mevedel--workspace workspace)
      (insert "reference")
      (setq reference (mevedel--create-reference-in source-buffer 1 10)
            reference-id (overlay-get reference 'mevedel-uuid)))
    (setf (mevedel-session-save-path session) session-dir
          (mevedel-session-session-id session) "transfer-session"
          (mevedel-session-working-directory session) root
          (mevedel-session-turn-count session) 1
          (mevedel-session-current-segment session) 1
          (mevedel-session-publication session) '(:head "old")
          (mevedel-session-control-transfer session)
          (list :state 'requested :request request)
          (mevedel-session-control-transfer-drains session) (list drain)
          (mevedel-session-lease session) '(:state owned))
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-file-name old-file
                default-directory root
                mevedel--workspace workspace)
          (insert "old transcript")
          (set-buffer-modified-p nil)
          (set-visited-file-modtime)
          (setq buffer-read-only t
                mevedel-session--read-only-mode t)
          (mevedel-session-control-transfer-register-root-buffer
           session buffer)
          (cl-letf
              (((symbol-function 'mevedel-session-publication-read)
                (lambda (&rest _) '(:head "new" :sidecar "/sidecar")))
               ((symbol-function 'mevedel-session-persistence-load-sidecar)
                (lambda (&rest _) '(:version 1)))
               ((symbol-function 'mevedel-session-codec-deserialize)
                (lambda (&rest _)
                  (list
                   :session
                   (mevedel-session--create
                    :name "requester" :working-directory root
                    :turn-count 9 :current-segment 2))))
               ((symbol-function 'mevedel-session-artifacts-read-artifact)
                (lambda (&rest _)
                  (if (eq failure 'artifact)
                      (error "Injected artifact failure")
                    "new transcript")))
               ((symbol-function
                 'mevedel-session-artifacts-check-target-incarnation)
                (lambda (candidate &rest _)
                  (when (eq failure 'target)
                    (setf
                     (mevedel-execution-target-observed-incarnation
                      (mevedel-session-execution-target candidate))
                     "changed")
                    (error "Injected incarnation failure"))))
               ((symbol-function 'mevedel-transcript-restore-gptel-state)
                (lambda ()
                  (when (eq failure 'transcript)
                    (error "Injected transcript failure"))))
               ((symbol-function
                 'mevedel-session-artifacts-load-instructions)
                (lambda (&rest _)
                  (if (eq failure 'instructions)
                      (progn
                        (mevedel--clear-instruction-state workspace)
                        nil)
                    t)))
               ((symbol-function 'mevedel-session-durability-lease-release)
                (lambda (&rest _) (cl-incf releases))))
            (dolist (stage '(artifact transcript target instructions))
              (setq failure stage)
              (should-error
               (mevedel-session-control-transfer--adopt-control
                session buffer))
              (should (= 1 (mevedel-session-turn-count session)))
              (should (= 1 (mevedel-session-current-segment session)))
              (should (equal '(:head "old")
                             (mevedel-session-publication session)))
              (should (equal (list drain)
                             (mevedel-session-control-transfer-drains session)))
              (should (eq target (mevedel-session-execution-target session)))
              (should-not
               (mevedel-execution-target-observed-incarnation target))
              (should (equal old-file buffer-file-name))
              (should (equal "old transcript" (buffer-string)))
              (should buffer-read-only)
              (should mevedel-session--read-only-mode)
              (let ((restored
                     (mevedel--instruction-with-uuid reference-id workspace)))
                (should (eq reference restored))
                (should (overlayp restored))
                (with-current-buffer (overlay-buffer restored)
                  (should (equal "reference"
                                 (buffer-substring-no-properties
                                  (overlay-start restored)
                                  (overlay-end restored)))))))
            (setq failure nil)
            (mevedel-test--with-captured-messages nil
              (should
               (mevedel-session-control-transfer--adopt-control
                session buffer)))
            (should (= 9 (mevedel-session-turn-count session)))
            (should (= 2 (mevedel-session-current-segment session)))
            (should (equal "new transcript" (buffer-string)))
            (should-not buffer-read-only)
            (should-not mevedel-session--read-only-mode)
            (should (equal (list drain)
                           (mevedel-session-control-transfer-drains session)))
            (should-not
             (mevedel-session-control-transfer-drained-p session))
            (mevedel-session-control-transfer-unregister-drain session drain)
            (should (mevedel-session-control-transfer-drained-p session))
            (should (= 4 releases))))
      (mevedel-session-control-transfer-unregister-root-buffer session buffer)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (mevedel--clear-instruction-state workspace)
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-control-transfer-poll ()
  ,test
  (test)
  :doc "defers target I/O while the transport or a publication is busy"
  (let ((session (mevedel-session--create :name "transfer"))
        (owner 0)
        (requester 0))
    (cl-letf (((symbol-function
                'mevedel-session-control-transfer--poll-owner)
               (lambda (_session) (cl-incf owner) 'owner))
              ((symbol-function
                'mevedel-session-control-transfer--poll-requester)
               (lambda (_session _buffer) (cl-incf requester) 'requester)))
      (should (eq 'owner
                  (mevedel-session-control-transfer-poll session nil nil)))
      (should (eq 'requester
                  (mevedel-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      ;; Emacs runs filters and timers wherever the main loop waits,
      ;; including inside a TRAMP operation; polling from there wedges the
      ;; operation already running.
      (cl-letf (((symbol-function 'mevedel-transport-busy-p)
                 (lambda (&optional _path) t)))
        (should-not
         (mevedel-session-control-transfer-poll session nil nil))
        (should-not
         (mevedel-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (mevedel-session-publication-active-p session) t)
      (should-not (mevedel-session-control-transfer-poll session nil nil))
      (should-not (mevedel-session-control-transfer-poll session nil t))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (mevedel-session-publication-active-p session) nil)
      (should (eq 'owner
                  (mevedel-session-control-transfer-poll session nil nil)))
      (should (= 2 owner)))
    ;; A non-owner that did not acquire control still follows: the same tick
    ;; is what advances a joined client through the owner's published turns.
    (cl-letf (((symbol-function
                'mevedel-session-control-transfer--poll-requester)
               (lambda (&rest _) nil))
              ((symbol-function
                'mevedel-session-control-transfer--follow-published)
               (lambda (&rest _) 'followed)))
      (should (eq 'followed
                  (mevedel-session-control-transfer-poll session nil t))))))

(provide 'test-mevedel-session-control-transfer)

;;; test-mevedel-session-control-transfer.el ends here
