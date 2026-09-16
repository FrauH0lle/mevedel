;;; test-mevedel-memory-cleanup.el -- Consolidation retention -*- lexical-binding: t -*-

;;; Commentary:

;; Expire real accepted reviews without losing surviving coverage or recovery.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-pass)
(require 'mevedel-memory-cleanup)
(require 'mevedel-memory-list)
(require 'mevedel-journal-jobs)
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-cleanup-entry
    (:vars* ((root (make-temp-file "mevedel-memory-retention-" t))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-max-age-days 365)
             (none "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")
             claim prepared review)
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled "")) (delete-directory root t)))
  (cl-labels ((publish (&optional digest reply focus)
                (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
                  (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)
                        prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace)
                                                               (and digest (list digest)) (or focus "")))
                  (mevedel-memory-store-accept workspace prepared (or reply none) (and digest (list digest)) "test:model" nil)
                  (setq review (mevedel-memory-store-publish workspace (plist-get prepared :id)))))
              (topic ()
                (let ((memory (file-name-concat root "memory")))
                  (make-directory memory)
                  (setq mevedel-memory-dirs (list memory))
                  (publish nil
                           (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"topic.md\"\ntype: \"project\"\n"
                                           "title: \"Topic\"\nhook: \"Current\"\nreason: \"Evidence\"\nevidence: []\n---\nNew topic.\n```\n"
                                           "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none")
                                   (caar (plist-get (mevedel-memory-scope-capture workspace) :roots))))
                  (car (plist-get (mevedel-memory-store-accepted workspace (plist-get prepared :id)) :proposals)))))
    ,test)
  (test)
  :doc "expires a rejected proposal's complete history without republishing its decision"
  (let ((proposal (topic)))
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (mevedel-memory-decision-reject workspace (plist-get prepared :id) (plist-get proposal :id)))
    (should (= 2 (mevedel-journal-cleanup-expired workspace)))
    (should-not (mevedel-journal-store-entries root))
    (should-not (mevedel-session-control-fs-list-directory (mevedel-memory-decision--directory workspace) "\\.el\\'"))
    (mevedel-memory-decision-recover workspace)
    (mevedel-memory-store-recover workspace)
    (should-not (mevedel-memory-list--collect (list :workspace workspace))))
  :doc "expires applied and reversed evidence together while preserving current memory files"
  (let ((proposal (topic)))
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (mevedel-memory-decision-apply workspace (plist-get prepared :id) (plist-get proposal :id))
      (mevedel-memory-decision-reverse workspace (plist-get prepared :id) (plist-get proposal :id)))
    (should (= 3 (mevedel-journal-cleanup-expired workspace)))
    (should-not (mevedel-journal-store-entries root))
    (should-not (mevedel-memory-write-list workspace))
    (should-not (mevedel-session-control-fs-list-directory (mevedel-memory-write--directory workspace) "\\.el\\'"))
    (should-not (file-exists-p (file-name-concat root "memory" "topic.md")))
    (mevedel-memory-decision-recover-pending workspace))
  :doc "a recent terminal decision retains an older review and its evidence"
  (let ((proposal (topic)))
    (mevedel-memory-decision-reject workspace (plist-get prepared :id) (plist-get proposal :id))
    (should (= 0 (mevedel-journal-cleanup-expired workspace)))
    (should (= 2 (length (mevedel-journal-store-entries root))))
    (should (mevedel-memory-store-accepted workspace (plist-get prepared :id))))
  :doc "an application on another client's memory root retains recovery evidence"
  (let ((proposal (topic)))
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (mevedel-memory-decision-apply workspace (plist-get prepared :id) (plist-get proposal :id)))
    (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () (make-string 64 ?f))))
      (should (= 0 (mevedel-journal-cleanup-expired workspace)))
      (should (= 2 (length (mevedel-journal-store-entries root))))
      (should-not (mevedel-journal-cleanup-pass-retired-p root (plist-get prepared :id))))
    (should (= 2 (mevedel-journal-cleanup-expired workspace t))))
  :doc "an unresolved reverse write retains a terminal proposal's entire history"
  (let ((proposal (topic)) intent)
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (let* ((applied (mevedel-memory-decision-apply workspace (plist-get prepared :id) (plist-get proposal :id)))
             (original (plist-get (mevedel-memory-decision--published workspace applied) :intent))
             (scope (plist-get prepared :scope)))
        (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
        (mevedel-memory-write-call
         scope (plist-get proposal :root)
         (lambda (target) (setq intent (mevedel-memory-write-prepare-reverse workspace claim target original))))
        (mevedel-journal-claim-settle claim 'cancelled "")))
    (should (= 0 (mevedel-journal-cleanup-expired workspace)))
    (should (file-exists-p (mevedel-memory-write--pin intent)))
    (should (= 2 (length (mevedel-journal-store-entries root))))
    (should-not (mevedel-journal-index-entries workspace))
    (should (seq-some
             (lambda (row) (member "Write recovery required" (plist-get row :reasons)))
             (mevedel-journal-jobs--retained workspace)))
    (should-not (mevedel-journal-cleanup-pass-retired-p root (plist-get prepared :id))))
  :doc "interrupted history deletion can resume after its pass and decision bodies are gone"
  (let ((proposal (topic)) intent)
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (setq intent (plist-get (mevedel-memory-decision--published
                              workspace (mevedel-memory-decision-apply workspace (plist-get prepared :id) (plist-get proposal :id))) :intent)))
    (let* ((path (file-name-concat (mevedel-memory-write--directory workspace) (concat (plist-get intent :id) ".el")))
           (topic-file (file-name-concat root "memory" "topic.md"))
           (current (mevedel-session-control-fs-read-file topic-file))
           (original (symbol-function 'mevedel-session-control-fs-delete-file)))
      (mevedel-test--with-captured-messages nil
        (cl-letf (((symbol-function 'mevedel-session-control-fs-delete-file)
                   (lambda (file) (if (equal file path) (error "Interrupted history deletion") (funcall original file)))))
          (should-not (mevedel-journal-cleanup-expired workspace))))
      (should (file-exists-p path))
      (should-not (mevedel-memory-write-list workspace))
      (mevedel-memory-decision-recover-pending workspace)
      (should-not (mevedel-memory-list--collect (list :workspace workspace)))
      (mevedel-journal-cleanup-expired workspace t)
      (should-not (file-exists-p path))
      (should (equal current (mevedel-session-control-fs-read-file topic-file)))))
  :doc "a history larger than fifty public decisions expires as one dependency group"
  (let* ((proposal (topic))
         (accepted (mevedel-memory-store-accepted workspace (plist-get prepared :id))))
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (dotimes (_ 51)
        (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
        (mevedel-journal-store-publish-decision
         root (plist-get (mevedel-memory-decision--accept workspace claim accepted proposal 'stale "Stale snapshot") :metadata)))
      (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
      (mevedel-journal-store-publish-decision
       root (plist-get (mevedel-memory-decision--accept workspace claim accepted proposal 'rejected "No longer needed") :metadata)))
    (should (= 53 (length (mevedel-journal-store-entries root))))
    (let ((original (symbol-function 'mevedel-memory-decision--publish))
          (read-pass (symbol-function 'mevedel-memory-store-accepted))
          (replays 0) (pass-reads 0))
      (cl-letf (((symbol-function 'mevedel-memory-decision--publish)
                 (lambda (&rest args) (cl-incf replays) (apply original args)))
                ((symbol-function 'mevedel-memory-store-accepted)
                 (lambda (&rest args) (cl-incf pass-reads) (apply read-pass args))))
        (mevedel-memory-decision-recover workspace))
      (should (= 0 replays))
      (should (= 1 pass-reads)))
    (let ((read-pass (symbol-function 'mevedel-memory-store-accepted))
          (read-decision (symbol-function 'mevedel-memory-decision--record))
          (pass-reads 0) (decision-reads 0))
      (cl-letf (((symbol-function 'mevedel-memory-store-accepted)
                 (lambda (&rest args) (cl-incf pass-reads) (apply read-pass args)))
                ((symbol-function 'mevedel-memory-decision--record)
                 (lambda (&rest args) (cl-incf decision-reads) (apply read-decision args))))
        (should (= 53 (mevedel-journal-cleanup-expired workspace))))
      (should (= 1 pass-reads))
      (should (= 52 decision-reads)))
    (should-not (mevedel-journal-store-entries root))
    (should-not (mevedel-session-control-fs-list-directory (mevedel-memory-decision--directory workspace) "\\.el\\'")))
  :doc "expires an old no-action review and its private body without later republication"
  (progn
    (publish)
    (should (= 1 (mevedel-journal-cleanup-expired workspace)))
    (should-not (mevedel-journal-store-entries root))
    (dolist (file '("prepared.el" "accepted.el"))
      (should-not (file-exists-p (file-name-concat
                                (mevedel-memory-store--directory workspace (plist-get prepared :id)) file))))
    (mevedel-memory-store-recover workspace)
    (should-not (mevedel-journal-store-entries root))
    (should-error (mevedel-memory-store-read workspace (plist-get prepared :id)))
    (let ((record (json-parse-string
                   (mevedel-session-control-fs-read-file
                    (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)) "retired-passes" (plist-get prepared :id))))))
      (should (equal "general" (gethash "scope" record)))
      (should (equal (plist-get review :created) (gethash "created" record))))
    (should-not (mevedel-memory-list--collect (list :workspace workspace))))
  :doc "retired focused reviews retain scheduling scope without retaining the private focus"
  (progn
    (publish nil nil "SECRET focus")
    (should (= 1 (mevedel-journal-cleanup-expired workspace)))
    (let* ((text (mevedel-session-control-fs-read-file
                  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)) "retired-passes" (plist-get prepared :id))))
           (record (json-parse-string text)))
      (should (equal "focused" (gethash "scope" record)))
      (should (equal (plist-get review :created) (gethash "created" record)))
      (should-not (string-search "SECRET" text))))
  :doc "expires focused history while preserving its unprocessed public source"
  (let ((digest (mevedel-journal-store-publish-digest
                 root (list :capture-id (make-string 64 ?a) :session "closed" :session-name "Closed"
                            :workspace identity :trigger 'session-end :segment 1 :source-revision (make-string 64 ?b)
                            :turns '(1) :turn-ids (list (make-string 64 ?c))
                            :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t) :model "test:model")
                 "## Done\n- Observed: Tests passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none")))
    (publish digest nil "Focused question")
    (should (= 1 (mevedel-journal-cleanup-expired workspace)))
    (should (equal (list (plist-get digest :id))
                   (mapcar (lambda (entry) (plist-get entry :id))
                           (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root)))))
    (should (mevedel-journal-cleanup-pass-retired-p root (plist-get prepared :id))))
  :doc "retains an old review while its covered digest or unreleased evidence pin survives"
  (let ((digest (mevedel-journal-store-publish-digest
                 root (list :capture-id (make-string 64 ?a) :session "closed" :session-name "Closed"
                            :workspace identity :trigger 'session-end :segment 1 :source-revision (make-string 64 ?b)
                            :turns '(1) :turn-ids (list (make-string 64 ?c))
                            :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t) :model "test:model")
                 "## Done\n- Observed: Tests passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none")))
    (publish digest)
    (should (= 1 (mevedel-journal-cleanup-expired workspace)))
    (should (= 1 (length (mevedel-journal-store-entries root))))
    (should-not (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root)))
    (let ((mevedel-journal-max-age-days 0))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) (+ 2 (floor (float-time))))))
        (should (mevedel-memory-store-accepted workspace (plist-get prepared :id)))
        (let ((pin (mevedel-memory-store--pin workspace (plist-get prepared :id) (plist-get digest :id))))
          (make-directory (file-name-directory pin) t)
          (write-region (plist-get prepared :hash) nil pin nil 'silent)
          (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
          (should (mevedel-memory-store-accepted workspace (plist-get prepared :id)))
          (delete-file pin))
        (should (= 1 (mevedel-journal-cleanup-expired workspace t)))
        (should-not (mevedel-journal-store-entries root)))))
  :doc "fully reviewed recent notes retire even when recall expiry is disabled"
  (let* ((mevedel-journal-max-age-days nil)
         (entry (mevedel-journal-store-publish-digest
                 root (list :capture-id (make-string 64 ?a) :session "closed" :session-name "Closed"
                            :workspace identity :trigger 'session-end :segment 1
                            :source-revision (make-string 64 ?b) :turns '(1)
                            :turn-ids (list (make-string 64 ?c))
                            :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t) :model "test:model")
                 "## Done\n- Observed: Completed work.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none")))
    (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
    (publish entry)
    (should (= 1 (mevedel-journal-cleanup-expired workspace t)))
    (should-error (mevedel-journal-store-read root (plist-get entry :file)))
    (should (equal (plist-get entry :body)
                   (plist-get (car (plist-get (plist-get (mevedel-memory-store-accepted
                                                        workspace (plist-get prepared :id)) :prepared) :entries)) :body))))
  :doc "a live consolidation owner postpones expiry without losing its claim"
  (progn
    (publish)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (should-not (mevedel-journal-cleanup-expired workspace))
    (should (equal claim (mevedel-journal-claim-current (plist-get claim :directory))))
    (should-not (mevedel-journal-claim-outcome claim))
    (should (mevedel-memory-store-accepted workspace (plist-get prepared :id))))
  :doc "accepted expiry resumes after private deletion is interrupted without exposing a pending proposal"
  (progn
    (publish)
    (let* ((path (file-name-concat (mevedel-memory-store--directory workspace (plist-get prepared :id)) "accepted.el"))
           (original (symbol-function 'mevedel-session-control-fs-delete-file)))
      (mevedel-test--with-captured-messages nil
        (cl-letf (((symbol-function 'mevedel-session-control-fs-delete-file)
                   (lambda (file) (if (equal file path) (error "Interrupted private retirement") (funcall original file)))))
          (should-not (mevedel-journal-cleanup-expired workspace))))
      (should (file-exists-p path))
      (mevedel-memory-store-recover workspace)
      (should-not (mevedel-memory-list--collect (list :workspace workspace)))
      (mevedel-journal-cleanup-expired workspace t)
      (should-not (file-exists-p path))
      (should-not (mevedel-journal-store-entries root))))
  :doc "changed private bytes remain after accepted retirement and cannot be silently deleted"
  (progn
    (publish)
    (let* ((path (file-name-concat (mevedel-memory-store--directory workspace (plist-get prepared :id)) "accepted.el"))
           (original (symbol-function 'mevedel-session-control-fs-delete-file)))
      (mevedel-test--with-captured-messages nil
        (cl-letf (((symbol-function 'mevedel-session-control-fs-delete-file)
                   (lambda (file) (if (equal file path) (error "Interrupted private retirement") (funcall original file)))))
          (mevedel-journal-cleanup-expired workspace)))
      (write-region "Foreign private edit" nil path nil 'silent)
      (let (messages)
        (mevedel-test--with-captured-messages messages
          (should-not (mevedel-journal-cleanup-expired workspace t)))
        (should (string-search "changed; retained for inspection" messages)))
      (should (equal "Foreign private edit" (mevedel-session-control-fs-read-file path)))))
  :doc "pending proposals retain their review and private before-state after the age threshold"
  (let* ((memory (file-name-concat root "memory"))
         (mevedel-memory-dirs (list memory)))
    (make-directory memory)
    (publish nil
             (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"topic.md\"\ntype: \"project\"\n"
                             "title: \"Topic\"\nhook: \"Current\"\nreason: \"Evidence\"\nevidence: []\n---\nNew topic.\n```\n"
                             "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none")
                     (caar (plist-get (mevedel-memory-scope-capture workspace) :roots))))
    (should (= 0 (mevedel-journal-cleanup-expired workspace)))
    (should-not (mevedel-journal-index-entries workspace))
    (let ((row (car (mevedel-journal-jobs--retained workspace))))
      (should (member "Awaiting proposal decisions" (plist-get row :reasons)))
      (should (> (plist-get row :age) 365)))
    (should (plist-get (mevedel-memory-store-accepted workspace (plist-get prepared :id)) :proposals)))
  :doc "manifest private paths cannot escape the exact retired pass"
  (progn
    (publish)
    (mevedel-journal-cleanup-expired workspace)
    (let* ((journal (mevedel-journal-store-directory root))
           (path (car (mevedel-session-control-fs-list-directory (file-name-concat (mevedel-journal-store-state-directory journal) "expiry") "\\.json\\'")))
           (manifest (json-parse-string (mevedel-session-control-fs-read-file path)))
           (private (gethash "private" (aref (gethash "entries" manifest) 0))))
      (puthash "file" "../outside.el" (aref private 0))
      (write-region (json-serialize manifest) nil path nil 'silent)
      (should-error (mevedel-journal-cleanup--read journal path))))
  :doc "history observations never hide accepted-pass edits from a later recovery"
  (let* ((proposal (topic))
         (pass (plist-get prepared :id))
         (path (file-name-concat (mevedel-memory-store--directory workspace pass) "accepted.el")))
    (mevedel-memory-decision-reject workspace pass (plist-get proposal :id))
    (mevedel-memory-decision-recover workspace)
    (let ((text (mevedel-session-control-fs-read-file path)))
      (write-region (concat text "\n") nil path nil 'silent))
    (should-error (mevedel-memory-decision-recover workspace))
    (should (= 0 (mevedel-journal-cleanup-expired workspace)))
    (should (= 2 (length (mevedel-journal-store-entries root))))
    (should-not (mevedel-journal-cleanup-pass-retired-p root pass)))
  :doc "a decision changed after history selection survives checked deletion"
  (let* ((proposal (topic)) decision
         (original (symbol-function 'mevedel-memory-cleanup--history)))
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (setq decision (mevedel-memory-decision-reject workspace (plist-get prepared :id) (plist-get proposal :id))))
    (let ((path (file-name-concat (mevedel-memory-decision--directory workspace)
                                 (concat (plist-get decision :decision-id) ".el")))
          messages)
      (mevedel-test--with-captured-messages messages
        (cl-letf (((symbol-function 'mevedel-memory-cleanup--history)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (write-region "Changed decision bytes" nil path nil 'silent)))))
          (should-not (mevedel-journal-cleanup-expired workspace))))
      (should (string-search "changed; retained for inspection" messages))
      (should (equal "Changed decision bytes" (mevedel-session-control-fs-read-file path)))))
  :doc "an additional accepted but unpublished decision retains the public history"
  (let* ((proposal (topic))
         (accepted (mevedel-memory-store-accepted workspace (plist-get prepared :id))))
    (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
      (mevedel-memory-decision-reject workspace (plist-get prepared :id) (plist-get proposal :id))
      (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
      (mevedel-memory-decision--accept workspace claim accepted proposal 'stale "Unpublished snapshot"))
    (should (= 0 (mevedel-journal-cleanup-expired workspace)))
    (should (= 2 (length (mevedel-journal-store-entries root))))
    (should (= 2 (length (mevedel-session-control-fs-list-directory
                         (mevedel-memory-decision--directory workspace) "\\.el\\'"))))
    (should-not (mevedel-journal-cleanup-pass-retired-p root (plist-get prepared :id))))
  :doc "unreviewed overdue evidence survives until a successful no-action review"
  (let ((entry (mevedel-journal-store-publish-digest
                root (list :capture-id (make-string 64 ?a) :session "closed"
                           :session-name "Closed" :workspace identity
                           :trigger 'session-end :segment 1
                           :source-revision (make-string 64 ?b) :turns '(1)
                           :turn-ids (list (make-string 64 ?c))
                           :created "2000-01-01T00:00:00Z" :model "test:model")
                "## Done\n- Observed: Completed work.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
    (should (= 0 (mevedel-journal-cleanup-expired workspace)))
    (should (equal entry (mevedel-journal-store-read root (plist-get entry :file))))
    (should-not (mevedel-journal-index-entries workspace))
    (publish entry)
    (should-not (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root)))
    (should (= 1 (mevedel-journal-cleanup-expired workspace t)))
    (should-error (mevedel-journal-store-read root (plist-get entry :file)))
    (should (= 1 (mevedel-journal-cleanup-expired workspace t)))
    (should-not (mevedel-journal-store-entries root))))


(mevedel-deftest mevedel-journal-cleanup-pass-retired-p ()
  ,test
  (test)
  :doc "retirement discovery refuses non-identities and creates no state"
  (let ((root (make-temp-file "mevedel-retired-pass-" t)))
    (unwind-protect
        (progn
          (should-not (mevedel-journal-cleanup-pass-retired-p root (make-string 64 ?a)))
          (should-error (mevedel-journal-cleanup-pass-retired-p root "../outside"))
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (delete-directory root t))))

(provide 'test-mevedel-memory-cleanup)
;;; test-mevedel-memory-cleanup.el ends here
