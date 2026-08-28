;;; test-mevedel-session-publication.el -- Publication tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-publication' generation summaries and collection.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))

(defun test-mevedel-session-publication--with-published
    (host prefix client-id body)
  "Call BODY with a leased portable session fixture.
HOST names the mock target, PREFIX the temporary root, and CLIENT-ID the
durability client character.  BODY receives the session, its directory,
and its segment path."
  (let ((mevedel-session-publication--manifest-cache
         (make-hash-table :test #'equal))
        (mevedel-session-publication--facts-cache
         (make-hash-table :test #'equal))
        (local-root
         (file-name-as-directory (make-temp-file prefix t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((mevedel-session-durability--client-id
                   (make-string 64 client-id))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*publication-test*" session))
              (unwind-protect
                  (progn
                    (setf (mevedel-session-publication session)
                          (mevedel-session-publication-read session-dir))
                    (funcall body session session-dir segment))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-publication-generation-summaries ()
  ,test
  (test)
  :doc "bounds picker facts while reading each manifest once"
  (let ((mevedel-session-publication-summary-scan-max 1))
    (test-mevedel-session-publication--with-published
     "publication-summaries" "mevedel-publication-summaries-" ?c
     (lambda (session session-dir segment)
       (test-mevedel-session-persistence--publish-generation
        session session-dir segment "Turn one\n" 1)
       (test-mevedel-session-persistence--publish-generation
        session session-dir segment "Turn two\n" 2)
       (let ((summaries
              (mevedel-session-publication-generation-summaries session-dir)))
         (should (> (length summaries) 1))
         (should (= 1 (cl-count-if
                       (lambda (summary)
                         (plist-member summary :turn-count))
                       summaries)))
         (should (cl-every
                  (lambda (summary)
                    (and (plist-get summary :manifest-readable-p)
                         (plist-get summary :references)))
                  summaries)))))))

(mevedel-deftest mevedel-session-publication-head-facts ()
  ,test
  (test)
  :doc "reads one published head through the validated manifest boundary"
  (test-mevedel-session-publication--with-published
   "publication-head-facts" "mevedel-publication-head-facts-" ?d
   (lambda (session session-dir segment)
     (let* ((head (test-mevedel-session-persistence--publish-generation
                   session session-dir segment "Turn one\n" 1))
            (facts (mevedel-session-publication-head-facts session-dir head)))
       (should (= 1 (plist-get facts :turn-count)))))))

(mevedel-deftest mevedel-session-publication-settled-summary-p ()
  ,test
  (test)
  :doc "distinguishes settled states from mid-turn and unreadable heads"
  (should (mevedel-session-publication-settled-summary-p
           '(:turn-count 2 :prompt (:cum-turn 2))))
  (should (mevedel-session-publication-settled-summary-p
           '(:turn-count 0 :prompt nil)))
  (should-not (mevedel-session-publication-settled-summary-p
               '(:turn-count 2 :prompt (:cum-turn 3))))
  (should-not (mevedel-session-publication-settled-summary-p
               '(:turn-count nil :prompt nil))))

(mevedel-deftest mevedel-session-publication-collect-generations ()
  ,test
  (test)
  :doc "keeps the current head, settled turn states, and a recent grace window"
  (let ((mevedel-session-publication-keep-recent-generations 1)
        ;; Collection must inspect beyond the picker/listing scan window.
        (mevedel-session-publication-summary-scan-max 1))
    (test-mevedel-session-publication--with-published
     "publication-collect" "mevedel-publication-collect-" ?a
     (lambda (session session-dir segment)
      (cl-flet ((publish (transcript turns)
                  (test-mevedel-session-persistence--publish-generation
                   session session-dir segment transcript turns)))
        ;; One settled turn, three further saves of that same state, then
        ;; a second settled turn.
        (let* ((turn-one (list (publish "Turn one\n" 1)
                               (publish "Turn one, more\n" 1)
                               (publish "Turn one, more still\n" 1)
                               (publish "Turn one, complete\n" 1)))
               (current (publish "Turn two\n" 2))
               (before (length (mevedel-session-publication--generation-names
                                session-dir)))
               (deleted (mevedel-session-publication-collect-generations
                         session))
               (kept (mapcar (lambda (entry) (plist-get entry :head))
                             (mevedel-session-publication--generation-names
                              session-dir))))
          (should (> before 5))
          (should (> deleted 0))
          (should (member current kept))
          (should (= (length kept)
                     (hash-table-count
                      mevedel-session-publication--manifest-cache)))
          (should (= (length kept)
                     (hash-table-count
                      mevedel-session-publication--facts-cache)))
          ;; One generation stands for the earlier settled turn; the three
          ;; saves of that same state are gone.  Which one survives is not
          ;; the contract -- they restore the same conversation.
          (let ((survivors (seq-filter (lambda (head) (member head kept))
                                       turn-one)))
            (should (= 1 (length survivors)))
            ;; What survived still resolves through whatever its manifest
            ;; carries forward.
            (should (mevedel-session-publication-read
                     session-dir (car survivors))))
          (should (equal "Turn two\n"
                         (mevedel-session-artifacts-read-artifact
                          session "segment-0001.chat.org" t)))
          ;; Collecting again finds nothing left to reclaim.
          (should (= 0 (mevedel-session-publication-collect-generations
                        session))))))))

  :doc "reads an immutable generation's manifest and facts once"
  (let ((reads 0))
    (test-mevedel-session-publication--with-published
     "publication-cache" "mevedel-publication-cache-" ?c
     (lambda (session session-dir segment)
       (test-mevedel-session-persistence--publish-generation
        session session-dir segment "Turn one\n" 1)
       (let ((raw (symbol-function
                   'mevedel-session-publication--read-publication-raw)))
         (cl-letf (((symbol-function
                     'mevedel-session-publication--read-publication-raw)
                    (lambda (&rest args)
                      (setq reads (1+ reads))
                      (apply raw args))))
           (mevedel-session-publication-generation-summaries
            session-dir most-positive-fixnum)
           (let ((first-pass reads))
             (should (> first-pass 0))
             ;; A committed generation cannot change, so a second listing
             ;; does not reread its manifest or sidecar.
             (mevedel-session-publication-generation-summaries
              session-dir most-positive-fixnum)
             (should (= first-pass reads))))))))

  :doc "collects every collectible generation in one batched pass"
  (let ((mevedel-session-publication-keep-recent-generations 1))
    (test-mevedel-session-publication--with-published
     "publication-bound" "mevedel-publication-bound-" ?b
     (lambda (session session-dir segment)
       (dolist (turns '(1 1 1 2))
         (test-mevedel-session-persistence--publish-generation
          session session-dir segment (format "Turn %d\n" turns) turns))
       (should (= 2 (mevedel-session-publication-collect-generations session)))
       ;; The pass was complete; nothing is left for later.
       (should (= 0 (mevedel-session-publication-collect-generations
                     session))))))

  :doc "warns instead of failing when collection cannot inspect generations"
  (test-mevedel-session-publication--with-published
   "publication-collect-failure" "mevedel-publication-collect-failure-" ?f
   (lambda (session _session-dir _segment)
     (let (captured)
       (mevedel-test--with-captured-diagnostics captured
         (cl-letf (((symbol-function
                     'mevedel-session-publication-generation-summaries)
                    (lambda (&rest _) (error "Injected scan failure"))))
           (should-not
            (mevedel-session-publication-collect-generations session))))
       (should (string-match-p
                "Could not collect published generations.*Injected scan failure"
                captured)))))

  :doc "refuses without the lease and for a PID-lock session"
  (should-not
   (mevedel-session-publication-collect-generations
    (mevedel-session--create :authority-mode 'pid-lock)))
  (should-not
   (mevedel-session-publication-collect-generations
    (mevedel-session--create :authority-mode 'portable
                             :save-path "/tmp/mevedel-absent/"))))

(mevedel-deftest mevedel-session-publication--cached-manifest
  (:doc "Caches only successfully read immutable manifests")
  (let ((mevedel-session-publication--manifest-cache
         (make-hash-table :test #'equal))
        (head ".publications/generation-deadbeef/manifest.el")
        (reads 0))
    (cl-letf (((symbol-function
                'mevedel-session-publication--read-publication-raw)
               (lambda (&rest _)
                 (setq reads (1+ reads))
                 (if (= reads 1)
                     (error "Transient read failure")
                   '(:head "generation")))))
      (should-not
       (mevedel-session-publication--cached-manifest "/tmp/session/" head))
      (should
       (equal '(:head "generation")
              (mevedel-session-publication--cached-manifest
               "/tmp/session/" head)))
      (should (= 2 reads)))))

(mevedel-deftest mevedel-session-publication--cached-sidecar-facts
  (:doc "Caches only successfully read immutable sidecar facts")
  (let ((mevedel-session-publication--facts-cache
         (make-hash-table :test #'equal))
        (head ".publications/generation-deadbeef/manifest.el")
        (reads 0))
    (cl-letf (((symbol-function 'mevedel-session-publication--sidecar-facts)
               (lambda (&rest _)
                 (setq reads (1+ reads))
                 (and (> reads 1) '(:turn-count 1)))))
      (should-not
       (mevedel-session-publication--cached-sidecar-facts
        "/tmp/session/" head '(:head "generation")))
      (should
       (equal '(:turn-count 1)
              (mevedel-session-publication--cached-sidecar-facts
               "/tmp/session/" head '(:head "generation"))))
      (should (= 2 reads)))))

(mevedel-deftest mevedel-session-publication-call-with-diagnostic-batch ()
  ,test
  (test)

  :doc "appends inside a batch share one reservation"
  ;; Each append otherwise opened its own transaction: recovery refresh,
  ;; lease renewal, ownership reading, then a reservation renewing on
  ;; entry and committing on exit -- four times over per flush point.
  (let ((session (mevedel-session--create :authority-mode 'portable))
        (reservations 0)
        (appends nil))
    (cl-letf (((symbol-function 'mevedel-session-recovery-refresh) #'ignore)
              ((symbol-function 'mevedel-session-durability-lease-renew)
               (lambda (_s) t))
              ((symbol-function 'mevedel-session-durability-lease-owned-p)
               (lambda (_s) t))
              ((symbol-function 'mevedel-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-session-control-fs-append-file)
               (lambda (path content) (push (cons path content) appends) t))
              ((symbol-function 'mevedel-session-durability-call-with-reserved-lease)
               (lambda (_s fn) (setq reservations (1+ reservations)) (funcall fn))))
      (mevedel-session-publication-with-diagnostic-batch session
        (should (mevedel-session-publication-append-diagnostic
                 session "/x/a.log" "a"))
        (should (mevedel-session-publication-append-diagnostic
                 session "/x/b.log" "b")))
      (should (= 1 reservations))
      (should (equal '(("/x/a.log" . "a") ("/x/b.log" . "b"))
                     (nreverse appends)))))

  :doc "an unavailable lease declines every append without re-testing"
  (let ((session (mevedel-session--create :authority-mode 'portable))
        (renewals 0))
    (cl-letf (((symbol-function 'mevedel-session-recovery-refresh) #'ignore)
              ((symbol-function 'mevedel-session-durability-lease-renew)
               (lambda (_s) (setq renewals (1+ renewals)) nil)))
      (mevedel-session-publication-with-diagnostic-batch session
        (should-not (mevedel-session-publication-append-diagnostic
                     session "/x/a.log" "a"))
        (should-not (mevedel-session-publication-append-diagnostic
                     session "/x/b.log" "b")))
      (should (= 1 renewals))))

  :doc "one failing append declines without aborting the others"
  ;; Its caller retains the content, which is what nil already means to it.
  (let ((session (mevedel-session--create :authority-mode 'portable))
        (written nil))
    (cl-letf (((symbol-function 'mevedel-session-recovery-refresh) #'ignore)
              ((symbol-function 'mevedel-session-durability-lease-renew)
               (lambda (_s) t))
              ((symbol-function 'mevedel-session-durability-lease-owned-p)
               (lambda (_s) t))
              ((symbol-function 'mevedel-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-session-control-fs-append-file)
               (lambda (path content)
                 (if (equal path "/x/bad.log")
                     (error "target refused")
                   (push content written) t)))
              ((symbol-function 'mevedel-session-durability-call-with-reserved-lease)
               (lambda (_s fn) (funcall fn))))
      (mevedel-test--with-captured-diagnostics nil
        (mevedel-session-publication-with-diagnostic-batch session
          (should-not (mevedel-session-publication-append-diagnostic
                       session "/x/bad.log" "bad"))
          (should (mevedel-session-publication-append-diagnostic
                   session "/x/good.log" "good"))))
      (should (equal '("good") written))))

  :doc "a session that does not publish through the lease reserves nothing"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock))
        (reservations 0) (ran nil))
    (cl-letf (((symbol-function 'mevedel-session-durability-call-with-reserved-lease)
               (lambda (_s fn) (setq reservations (1+ reservations)) (funcall fn))))
      (mevedel-session-publication-with-diagnostic-batch session
        (setq ran t))
      (should ran)
      (should (= 0 reservations)))))

(provide 'test-mevedel-session-publication)
;;; test-mevedel-session-publication.el ends here
