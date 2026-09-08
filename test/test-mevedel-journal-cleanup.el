;;; test-mevedel-journal-cleanup.el -- Journal expiry tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises real target storage, immutable deletion election, and recovery.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-cleanup)
(require 'mevedel-journal-index)
(require 'mevedel-journal-pins)
(require 'mevedel-session-persistence)

(defun mevedel-test-journal-cleanup--entry (root name &optional fresh)
  "Publish one real digest named NAME under ROOT, old unless FRESH."
  (mevedel-journal-store-publish-digest
   root
   (list :capture-id (secure-hash 'sha256 name) :session name :session-name name
         :workspace (make-string 64 ?b) :trigger 'session-end :segment 1
         :source-revision (make-string 64 ?c) :turns '(1)
         :turn-ids (list (secure-hash 'sha256 (concat name "-turn")))
         :created (if fresh (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                    "2000-01-01T00:00:00Z")
         :model "provider:model")
   "## Done\n- Observed: completed (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))

(mevedel-deftest mevedel-journal-cleanup-expired ()
  ,test
  (test)
  :doc "old review coverage survives expiry while its digest remains"
  (let* ((root (make-temp-file "mevedel-review-retention-" t))
         (workspace (mevedel-workspace--create :type 'project :id root :root root))
         (mevedel-journal-max-age-days 365))
    (unwind-protect
        (let* ((digest (mevedel-test-journal-cleanup--entry root "surviving" t))
               (review (mevedel-journal-store-publish-review
                        root (list :pass-id (make-string 64 ?e) :workspace (make-string 64 ?b)
                                   :created "2000-01-01T00:00:00Z" :model "provider:model"
                                   :focus "" :digests (list (plist-get digest :id))
                                   :proposals nil :references nil))))
          (mevedel-test-journal-cleanup--entry root "expired")
          (mevedel-journal-cleanup-expired workspace)
          (should (equal review (mevedel-journal-store-read root (plist-get review :file))))
          (let ((entries (mevedel-journal-store-entries root)))
            (should (= 2 (length entries)))
            (should-not (mevedel-journal-index-unreviewed entries))))
      (delete-directory root t)))

  :doc "project journal expiry runs independently of disabled session cleanup and is throttled"
  (let* ((root (make-temp-file "mevedel-journal-cleanup-" t))
         (workspace (mevedel-workspace--create :type 'project :id root :root root))
         (mevedel-journal-max-age-days 365)
         (mevedel-session-max-age-days nil))
    (unwind-protect
        (let ((old (mevedel-test-journal-cleanup--entry root "old"))
              (fresh (mevedel-test-journal-cleanup--entry root "fresh" t)))
          (mevedel-session-persistence-cleanup-expired workspace)
          (should (equal (list fresh) (mevedel-journal-store-entries root)))
          (should-not (file-exists-p (file-name-concat (mevedel-journal-store-directory root) (plist-get old :file))))
          (should (member (car (plist-get old :turn-ids)) (mevedel-journal-store-covered-turns root)))
          (should-error (mevedel-journal-store-read root (plist-get old :file)))
          (should-error (mevedel-test-journal-cleanup--entry root "old"))
          (mevedel-test-journal-cleanup--entry root "another-old")
          (should-not (mevedel-journal-cleanup-expired workspace))
          (should (= 2 (length (mevedel-journal-store-entries root))))
          (should (= 1 (mevedel-journal-cleanup-expired workspace t))))
      (delete-directory root t)))

  :doc "one opportunity expires at most fifty entries without draining the backlog"
  (let* ((root (make-temp-file "mevedel-journal-cleanup-batch-" t))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-journal-max-age-days 365))
    (unwind-protect
        (progn
          (dotimes (n 51) (mevedel-test-journal-cleanup--entry root (format "old-%d" n)))
          (should (= 50 (mevedel-journal-cleanup-expired workspace)))
          (should (= 1 (length (mevedel-journal-store-entries root))))
          (should-not (mevedel-journal-cleanup-expired workspace))
          (should (= 51 (length (mevedel-journal-store-covered-turns root))))
          (should (= 1 (mevedel-journal-cleanup-expired workspace t))))
      (delete-directory root t)))

  :doc "preserves pending capture pins and review evidence while collecting retired payloads"
  (let* ((root (make-temp-file "mevedel-journal-cleanup-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-journal-store-directory root))
         (mevedel-journal-max-age-days 365))
    (unwind-protect
        (let* ((pending (mevedel-test-journal-cleanup--entry root "pending"))
               (pinned (mevedel-test-journal-cleanup--entry root "pinned"))
               (retired (mevedel-test-journal-cleanup--entry root "retired"))
               (source (file-name-concat root "source-session"))
               (capture (file-name-concat directory "state" "captures" (plist-get retired :capture-id)))
               (pin-directory (file-name-concat directory "state" "evidence-pins" (plist-get pinned :id))))
          (make-directory (file-name-concat directory "state" "captures" (plist-get pending :capture-id)) t)
          (mevedel-journal-pins-retain source (plist-get pending :capture-id) nil)
          (make-directory pin-directory t)
          (write-region "pending proposal evidence" nil (file-name-concat pin-directory "proposal.json") nil 'silent)
          (make-directory capture t)
          (write-region "published" nil (file-name-concat capture "retired") nil 'silent)
          (write-region "retired accepted payload" nil (file-name-concat capture "capture.json") nil 'silent)
          (should (= 1 (mevedel-journal-cleanup-expired workspace)))
          (should (= 2 (length (mevedel-journal-store-entries root))))
          (should (mevedel-journal-pins-present-p source))
          (should (file-exists-p pin-directory))
          (should-not (file-exists-p capture)))
      (delete-directory root t)))

  :doc "remote project expiry shares the same target-native rules without session expiry"
  (let ((root (make-temp-file "mevedel-journal-cleanup-remote-" t))
        (mevedel-journal-max-age-days 365)
        (mevedel-session-max-age-days nil))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("journal-expiry")
          (let* ((remote (format "/mevedelmock:journal-expiry:%s/" root))
                 (workspace (mevedel-workspace--create :type 'project :id remote :root remote))
                 (entry (mevedel-test-journal-cleanup--entry remote "remote-old")))
            (mevedel-session-persistence-cleanup-expired workspace)
            (should-not (mevedel-journal-store-entries remote))
            (should (equal (plist-get entry :turn-ids) (mevedel-journal-store-covered-turns remote)))))
      (delete-directory root t)))

  :doc "disabled expiry creates no state and live mutation or digest owners postpone deletion"
  (let* ((root (make-temp-file "mevedel-journal-cleanup-" t))
         (workspace (mevedel-workspace--create :root root)))
    (unwind-protect
        (progn
          (let ((mevedel-journal-max-age-days nil))
            (should-not (mevedel-journal-cleanup-expired workspace)))
          (should-not (file-exists-p (file-name-concat root ".mevedel")))
          (dolist (scope '("mutation" "digest-run"))
            (let* ((entry (mevedel-test-journal-cleanup--entry root scope))
                   (claim (mevedel-journal-claim-acquire
                           (file-name-concat (mevedel-journal-store-directory root) "state" scope) 120))
                   (mevedel-journal-max-age-days 365))
              (should-not (mevedel-journal-cleanup-expired workspace t))
              (should (mevedel-journal-store-read root (plist-get entry :file)))
              (mevedel-journal-claim-settle claim 'completed "")
              (should (= 1 (mevedel-journal-cleanup-expired workspace t))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-recover ()
  ,test
  (test)
  :doc "replays an accepted expiry after interrupted deletion, retaining first publication coverage"
  (let* ((root (make-temp-file "mevedel-journal-expiry-recover-" t))
         (workspace (mevedel-workspace--create :root root))
         (entry (mevedel-test-journal-cleanup--entry root "recover"))
         (directory (mevedel-journal-store-directory root))
         (path (file-name-concat directory (plist-get entry :file)))
         (delete-file-fn (symbol-function 'mevedel-session-control-fs-delete-file))
         (mevedel-journal-max-age-days 365))
    (unwind-protect
        (progn
          (mevedel-test--with-captured-messages nil
            (cl-letf (((symbol-function 'mevedel-session-control-fs-delete-file)
                       (lambda (target)
                         (if (equal target path) (error "Injected interruption")
                           (funcall delete-file-fn target)))))
              (should-not (mevedel-journal-cleanup-expired workspace))))
          (should (file-exists-p path))
          (should-not (mevedel-journal-store-entries root))
          (let* ((claim (mevedel-journal-claim-current (file-name-concat directory "state" "mutation")))
                 (manifest (mevedel-journal-cleanup--read
                            directory (file-name-concat directory "state" "expiry"
                                                        (concat (plist-get claim :owner) ".json")))))
            (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time)
                       (lambda (_path) (1+ (plist-get claim :expires-at)))))
              (should (= 1 (mevedel-journal-cleanup-expired workspace t))))
            (should (= 0 (mevedel-journal-cleanup--apply directory manifest))))
          (should-not (file-exists-p path))
          (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
          (should (equal (plist-get entry :turn-ids) (mevedel-journal-store-covered-turns root))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup--apply ()
  ,test
  (test)
  :doc "an expired unaccepted manifest cannot delete after a successor wins"
  (let* ((root (make-temp-file "mevedel-journal-expiry-fence-" t))
         (workspace (mevedel-workspace--create :root root))
         (entry (mevedel-test-journal-cleanup--entry root "fenced"))
         (directory (mevedel-journal-store-directory root))
         (scope (file-name-concat directory "state" "mutation"))
         (claim (mevedel-journal-claim-acquire scope 120))
         (mevedel-journal-max-age-days 365)
         successor)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time)
                     (lambda (_path) (1+ (plist-get claim :expires-at)))))
            (should (= 0 (mevedel-journal-cleanup--owned workspace claim)))
            (setq successor (mevedel-journal-claim-acquire scope 120)))
          (should successor)
          (let ((manifest (mevedel-journal-cleanup--read
                           directory (file-name-concat directory "state" "expiry"
                                                       (concat (plist-get claim :owner) ".json")))))
            (should-not (mevedel-journal-claim-settle claim 'completed (plist-get manifest :hash)))
            (should (= 0 (mevedel-journal-cleanup--apply directory manifest)))
            (should (mevedel-journal-store-read root (plist-get entry :file)))))
      (delete-directory root t)))

  :doc "changed public bytes remain on disk after accepted expiry recovery"
  (let* ((root (make-temp-file "mevedel-journal-expiry-changed-" t))
         (workspace (mevedel-workspace--create :root root))
         (entry (mevedel-test-journal-cleanup--entry root "changed"))
         (directory (mevedel-journal-store-directory root))
         (path (file-name-concat directory (plist-get entry :file)))
         (delete-file-fn (symbol-function 'mevedel-session-control-fs-delete-file))
         (mevedel-journal-max-age-days 365))
    (unwind-protect
        (progn
          (mevedel-test--with-captured-messages nil
            (cl-letf (((symbol-function 'mevedel-session-control-fs-delete-file)
                       (lambda (target)
                         (if (equal target path) (error "Injected interruption")
                           (funcall delete-file-fn target)))))
              (mevedel-journal-cleanup-expired workspace)))
          (write-region "intervening edit" nil path nil 'silent)
          (let (messages)
            (mevedel-test--with-captured-messages messages
              (should-not (mevedel-journal-cleanup-expired workspace t)))
            (should (string-match-p "changed; retained for inspection" messages)))
          (should (equal "intervening edit" (mevedel-session-control-fs-read-file path))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup--read ()
  ,test
  (test)
  :doc "changed or malformed accepted manifests cannot authorize recovery deletion"
  (let* ((root (make-temp-file "mevedel-journal-expiry-manifest-" t))
         (workspace (mevedel-workspace--create :root root))
         (entry (mevedel-test-journal-cleanup--entry root "manifest"))
         (directory (mevedel-journal-store-directory root))
         (path (file-name-concat directory (plist-get entry :file)))
         (apply-fn (symbol-function 'mevedel-journal-cleanup--apply))
         (mevedel-journal-max-age-days 365))
    (unwind-protect
        (progn
          (mevedel-test--with-captured-messages nil
            (cl-letf (((symbol-function 'mevedel-journal-cleanup--apply)
                       (lambda (&rest _) (error "Injected interruption before application"))))
              (should-not (mevedel-journal-cleanup-expired workspace))))
          (let* ((job (car (mevedel-session-control-fs-list-directory
                           (file-name-concat directory "state" "expiry") "\\.json\\'")))
                 (object (json-parse-string (mevedel-session-control-fs-read-file job))))
            (puthash "sha256" (make-string 64 ?0)
                     (aref (gethash "entries" object) 0))
            (write-region (json-serialize object) nil job nil 'silent)
            (should-error (funcall apply-fn directory (mevedel-journal-cleanup--read directory job)))
            (should (file-exists-p path))
            (write-region "{}" nil job nil 'silent)
            (should-error (mevedel-journal-cleanup--read directory job))
            (should (file-exists-p path))))
      (delete-directory root t))))

(provide 'test-mevedel-journal-cleanup)
;;; test-mevedel-journal-cleanup.el ends here
