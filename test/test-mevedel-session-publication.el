;;; test-mevedel-session-publication.el -- Publication tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-publication' generation summaries and collection.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-journal-pins)

(mevedel-deftest mevedel-session-publication--immutable-entry ()
  (let* ((root (make-temp-file "mevedel-entry-" t))
         (source (file-name-concat root "source"))
         (directory (file-name-concat root ".publications/generation-test/"))
         (bytes (unibyte-string 0 127 128 255 10))
         (reader (symbol-function 'insert-file-contents-literally))
         (reads 0)
         entry)
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil source nil 'silent))
          (cl-letf (((symbol-function 'insert-file-contents-literally)
                     (lambda (&rest args)
                       (cl-incf reads)
                       (apply reader args))))
            (setq entry
                  (mevedel-session-publication--immutable-entry
                   directory (list :source source :logical "tool-results/result")
                   1 root)))
          (should (= reads 1))
          (should (equal bytes (plist-get entry :content)))
          (should (equal (secure-hash 'sha256 bytes)
                         (plist-get (cdr (plist-get entry :entry)) :sha256)))
          (should (equal ".publications/generation-test/000001.data"
                         (plist-get (cdr (plist-get entry :entry)) :published))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-publication--deduplicate-artifacts ()
  ,test
  (test)
  :doc "keeps the last occurrence in source order without rescanning old entries"
  (let* ((artifacts '((:logical "a" :content "old")
                      (:path "/outside")
                      (:logical "b" :delete t)
                      (:logical "a" :delete t)
                      (:logical "c" :content "new")
                      (:logical "b" :content "restored")))
         (original (copy-tree artifacts)))
    (should (equal (mevedel-session-publication--deduplicate-artifacts artifacts)
                   '((:logical "a" :delete t)
                     (:logical "c" :content "new")
                     (:logical "b" :content "restored"))))
    (should (equal artifacts original)))
  (let* ((artifacts (cl-loop for n below 1000 collect
                             (list :logical (number-to-string n) :delete t)))
         (get (symbol-function 'plist-get))
         (calls 0))
    (cl-letf (((symbol-function 'plist-get)
               (lambda (plist property &optional predicate)
                 (cl-incf calls)
                 (funcall get plist property predicate))))
      (should (equal artifacts
                     (mevedel-session-publication--deduplicate-artifacts artifacts))))
    (should (<= calls 2000))))

(mevedel-deftest mevedel-session-publication--capture-publication ()
  ,test
  (test)
  :doc "qualifies many immutable artifacts without repeating shared root work"
  (let* ((root (make-temp-file "mevedel-publication-paths-" t))
         (prefix ".publications/generation-0123456789abcdef0123/")
         (artifacts (cl-loop for n below 100
                             collect (list (if (zerop n) "session.meta.el"
                                             (format "file-history/%d" n))
                                           :published (concat prefix (format "%d.data" n))
                                           :sha256 (make-string 64 ?a))))
         (raw (list :head (concat prefix "manifest.el")
                    :sidecar "session.meta.el" :artifacts artifacts))
         (original (copy-tree raw))
         (physical (symbol-function 'mevedel-session-control-fs-physical-path))
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-control-fs-physical-path)
                   (lambda (path) (cl-incf calls) (funcall physical path))))
          (let ((captured (mevedel-session-publication--capture-publication
                           (concat root "//") raw)))
            (should (equal raw original))
            (should (equal (plist-get captured :head) (plist-get raw :head)))
            (should (equal (plist-get captured :sidecar)
                           (file-name-concat root prefix "0.data")))
            (should (= 100 (length (plist-get captured :artifacts))))
            (cl-mapc (lambda (before after)
                       (should (equal (car before) (car after)))
                       (should (equal (plist-get (cdr after) :published)
                                      (file-name-concat root (plist-get (cdr before) :published))))
                       (should (equal (plist-get (cdr before) :sha256)
                                      (plist-get (cdr after) :sha256))))
                     artifacts (plist-get captured :artifacts))
            (should (<= calls 102))))
      (delete-directory root t)))

  :doc "keeps remote prefixes and rejects paths outside immutable generations"
  (let ((root "/ssh:example.invalid:/workspace/session/"))
    (dolist (path '("/tmp/outside" "../outside" ".publications/../outside"
                    ".publications/generation-0123456789abcdef0123/../outside"
                    ".publications/generation-0123456789abcdef0123/nested/file"))
      (should-error
       (mevedel-session-publication--capture-publication
        root (list :artifacts (list (list "session.meta.el" :published path)))))))
  (let* ((path ".publications/generation-0123456789abcdef0123/1.data")
         (root "/ssh:example.invalid:/workspace//session/")
         (captured
          (mevedel-session-publication--capture-publication
           root (list :artifacts (list (list "session.meta.el" :published path))))))
    (should (equal (plist-get captured :sidecar)
                   (concat "/ssh:example.invalid:/workspace/session/" path))))
  (should-not (mevedel-session-publication--capture-publication "/tmp/unused" nil)))

(mevedel-deftest mevedel-session-publication--delete-batch ()
  ,test
  (test)
  :doc "removes contained recovery while preserving outside and escaping paths"
  (dolist (kind '(inside outside sibling escape alias))
    (let* ((root (make-temp-file "mevedel-batch-containment-" t))
           (temp (file-name-concat root "staging"))
           (outside (file-name-concat root "staging-other"))
           (inside (file-name-concat temp "batch"))
           (escape (file-name-concat temp "escape"))
           (alias (file-name-concat root "alias"))
           (temporary-file-directory (file-name-as-directory temp)))
      (unwind-protect
          (progn
            (make-directory inside t)
            (make-directory outside)
            (make-symbolic-link outside escape)
            (make-symbolic-link temp alias)
            (when (eq kind 'alias)
              (setq temporary-file-directory (file-name-as-directory alias)))
            (let ((path (pcase kind
                          ((or 'inside 'alias) inside)
                          ('outside root)
                          ('sibling outside)
                          ('escape escape))))
              (mevedel-session-publication--delete-batch (list :directory path))
              (if (memq kind '(inside alias))
                  (should-not (file-exists-p path))
                (should (file-directory-p path)))
              (should (file-directory-p outside))))
        (delete-directory root t)))))

(defun test-mevedel-session-publication--with-published
    (host prefix client-id body)
  "Call BODY with a leased portable session fixture.
HOST names the mock target, PREFIX the temporary root, and CLIENT-ID the
durability client character.  BODY receives the session, its directory,
and its segment path."
  (let ((mevedel-session-publication--generation-cache
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

(mevedel-deftest mevedel-session-publication-publish/tombstone-cost (:quiet t)
  (test-mevedel-session-publication--with-published
   "publication-tombstones" "mevedel-tombstones-" ?c
   (lambda (session session-dir _segment)
     (let* ((paths (cl-loop for n below 40
                            collect (file-name-concat session-dir "file-history" (format "unused-%d" n))))
            (marker (list :path (file-name-concat session-dir "session.meta.el")
                          :content (mevedel-session-artifacts-printed-value
                                    (mevedel-session-artifacts-build-sidecar session (current-buffer)))
                          :commit-marker t))
            (program (symbol-function 'mevedel-session-control-fs-run-program))
            (programs 0) old-head)
       (mevedel-session-publication-publish
        session (append (mapcar (lambda (path) (list :path path :content "retained bytes")) paths)
                        (list marker)))
       (should-not (seq-some #'file-exists-p paths))
       (setq old-head (plist-get (mevedel-session-publication session) :head))
       (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                  (lambda (&rest args) (cl-incf programs) (apply program args))))
         (mevedel-session-publication-publish
          session (append (mapcar (lambda (path) (list :path path :delete t)) paths)
                          (list marker))))
       (should (<= programs 20))
       (should-not (assoc "file-history/unused-0" (plist-get (mevedel-session-publication session) :artifacts)))
       (let* ((old (mevedel-session-publication-read session-dir old-head))
              (entry (cdr (assoc "file-history/unused-0" (plist-get old :artifacts)))))
         (should (equal "retained bytes" (mevedel-session-control-fs-read-file (plist-get entry :published)))))))))

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

(mevedel-deftest mevedel-session-publication-generation-summary ()
  ,test
  (test)
  :doc "one generation exposes references and optional turn facts"
  (test-mevedel-session-publication--with-published
   "one-summary" "mevedel-one-summary-" ?c
   (lambda (session directory segment)
     (let* ((head (test-mevedel-session-persistence--publish-generation
                   session directory segment "Turn one\n" 1))
            (generation (seq-find
                         (lambda (item) (equal head (plist-get item :head)))
                         (mevedel-session-publication--generation-names directory)))
            (summary (mevedel-session-publication-generation-summary directory generation)))
       (should (equal head (plist-get summary :head)))
       (should (plist-get summary :manifest-readable-p))
       (should (plist-get summary :references))
       (should-not (plist-member summary :turn-count))
       (should (= 1 (plist-get (mevedel-session-publication-generation-summary
                               directory generation t) :turn-count)))))))

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

(defun test-mevedel-publication--collect (session)
  "Drain native bounded collection for SESSION, returning deleted directories."
  (condition-case err
      (when (and (mevedel-session-codec-portable-authority-p session)
                 (mevedel-session-durability-lease-owned-p session))
        (let* ((summaries (mevedel-session-publication-generation-summaries
                           (mevedel-session-save-path session) most-positive-fixnum))
               (plan (mevedel-session-publication-collection-plan session summaries))
               (steps 0))
          (while (mevedel-session-publication-collect-step session plan)
            (cl-incf steps)
            (should (< steps 1000)))
          (plist-get plan :deleted-directories)))
    (error
     (display-warning 'mevedel (format "Could not collect published generations: %s"
                                      (error-message-string err)) :warning)
     nil)))

(mevedel-deftest mevedel-session-publication-collect-step/files ()
  (let ((mevedel-session-publication-keep-recent-generations 1))
    (test-mevedel-session-publication--with-published
     "publication-file-collection" "mevedel-file-collection-" ?a
     (lambda (session directory _segment)
       (setf (mevedel-session-turn-count session) 1)
       (let ((marker (list :path (file-name-concat directory "session.meta.el")
                           :content (mevedel-session-artifacts-printed-value
                                     (mevedel-session-artifacts-build-sidecar session (current-buffer)))
                           :commit-marker t)))
         (mevedel-session-publication-publish
          session (list (list :path (file-name-concat directory "stable") :content "keep")
                        (list :path (file-name-concat directory "obsolete") :content "discard") marker))
         (let* ((old (plist-get (mevedel-session-publication session) :head))
                (manifest (mevedel-session-publication-read directory old))
                (stable (plist-get (cdr (assoc "stable" (plist-get manifest :artifacts))) :published))
                (obsolete (plist-get (cdr (assoc "obsolete" (plist-get manifest :artifacts))) :published)))
           (mevedel-session-publication-publish
            session (list (list :path (file-name-concat directory "obsolete") :delete t) marker))
           (let ((current (plist-get (mevedel-session-publication session) :head)))
             (set-file-times (file-name-concat directory old) '(1 0 0 0))
             (let* ((summaries (mevedel-session-publication-generation-summaries
                                directory most-positive-fixnum))
                    (plan (mevedel-session-publication-collection-plan session summaries))
                    (program (symbol-function 'mevedel-session-control-fs-run-program)))
               (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                          (lambda (operations)
                            (when (seq-some
                                   (lambda (op)
                                     (and (memq (plist-get op :op) '(delete-file delete-directory))
                                          (not (equal (file-name-nondirectory (plist-get op :path))
                                                      "manifest.el")))) operations)
                              (error "Injected interrupted payload sweep"))
                            (funcall program operations))))
                 (should-error
                  (while (mevedel-session-publication-collect-step session plan))))
               ;; Every remaining discoverable head is still readable.  The
               ;; interrupted pass retired obsolete heads before any payload.
               (should-not (file-exists-p (file-name-concat directory old)))
               (should (file-exists-p obsolete))
               (dolist (generation (mevedel-session-publication--generation-names directory))
                 (should (mevedel-session-publication-read directory (plist-get generation :head)))))
             (test-mevedel-publication--collect session)
             (should-not (file-exists-p (file-name-concat directory old)))
             (should-not (file-exists-p obsolete))
             (should (equal "keep" (mevedel-session-control-fs-read-file stable)))
             (should (mevedel-session-publication-read directory current))
             ;; Once its last retained reference disappears, an artifact-only
             ;; generation is reclaimed too, despite having no manifest.
             (mevedel-session-publication-publish
              session (list (list :path (file-name-concat directory "stable") :delete t) marker))
             (set-file-times (file-name-concat directory current) '(1 0 0 0))
             (test-mevedel-publication--collect session)
             (should-not (file-exists-p (file-name-directory stable))))))))))

(mevedel-deftest mevedel-session-publication--retained-heads ()
  (let ((mevedel-session-publication-keep-recent-generations 1))
    (should (equal (mevedel-session-publication--retained-heads
                    '((:head "recent" :turn-count 2 :prompt (:cum-turn 3))
                      (:head "settled" :turn-count 2)
                      (:head "duplicate" :turn-count 2)
                      (:head "fork" :turn-count 2 :fork-point-id "fork")
                      (:head "unknown")))
                   '("recent" "settled" "fork" "unknown")))))

(mevedel-deftest mevedel-session-publication-collection-plan ()
  (test-mevedel-session-publication--with-published
   "publication-plan" "mevedel-publication-plan-" ?d
   (lambda (session directory _segment)
     (let ((summaries (mevedel-session-publication-generation-summaries directory most-positive-fixnum)))
       (cl-letf (((symbol-function 'mevedel-session-publication--read-publication-raw)
                  (lambda (&rest _) (error "Planning decoded a manifest"))))
         (let ((plan (mevedel-session-publication-collection-plan session summaries)))
           (should (member (plist-get (mevedel-session-publication session) :head)
                           (plist-get plan :heads)))
           (should (= 0 (hash-table-count (plist-get plan :keep))))))))))

(mevedel-deftest mevedel-session-publication-collect-step/freshness ()
  (test-mevedel-session-publication--with-published
   "publication-plan-freshness" "mevedel-plan-freshness-" ?e
   (lambda (session directory segment)
     (let* ((old (test-mevedel-session-persistence--publish-generation session directory segment "old" 1))
            (current (test-mevedel-session-persistence--publish-generation session directory segment "new" 1))
            (summaries (mevedel-session-publication-generation-summaries directory most-positive-fixnum))
            (plan (mevedel-session-publication-collection-plan session summaries))
            (capture (make-string 64 ?f)))
       (while (not (plist-get plan :marked))
         (mevedel-session-publication-collect-step session plan))
       (mevedel-journal-pins-retain directory capture (list old))
       (should-error (mevedel-session-publication-collect-step session plan))
       (should (mevedel-session-publication-read directory old))
       (should (mevedel-session-publication-read directory current))
       (mevedel-journal-pins-release directory capture)
       (test-mevedel-session-persistence--publish-generation session directory segment "later" 2)
       (should-error (mevedel-session-publication-collect-step session plan))))))

(mevedel-deftest mevedel-session-publication-collect-step/unreadable ()
  (test-mevedel-session-publication--with-published
   "publication-unreadable" "mevedel-publication-unreadable-" ?b
   (lambda (session directory _segment)
     (let* ((summaries (mevedel-session-publication-generation-summaries directory most-positive-fixnum))
            (plan (mevedel-session-publication-collection-plan session summaries))
            (path (file-name-concat directory (plist-get plan :head)))
            (bytes (mevedel-session-control-fs-read-file path)))
       (unwind-protect
           (progn
             (mevedel-session-control-fs-write-file path "(:broken")
             (should-error (mevedel-session-publication-collect-step session plan))
             (should-not (plist-get plan :marked))
             (should-not (plist-get plan :operations))
             (should (= (length summaries)
                        (length (mevedel-session-publication--generation-names directory)))))
         (mevedel-session-control-fs-write-file path bytes))))))

(mevedel-deftest mevedel-session-publication-collect-step ()
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
               (deleted (test-mevedel-publication--collect
                         session))
               (kept (mapcar (lambda (entry) (plist-get entry :head))
                             (mevedel-session-publication--generation-names
                              session-dir))))
          (should (> before 5))
          (should (> deleted 0))
          (should (member current kept))
          (should (= (length kept)
                     (hash-table-count
                      mevedel-session-publication--generation-cache)))
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
          (should (= 0 (test-mevedel-publication--collect
                        session))))))))

  :doc "retains captured evidence until its journal pin is released"
  (let ((mevedel-session-publication-keep-recent-generations 1))
    (test-mevedel-session-publication--with-published
     "publication-journal-pin" "mevedel-publication-journal-pin-" ?a
     (lambda (session session-dir segment)
       (let ((capture (make-string 64 ?a))
             heads)
         (dotimes (index 4)
           (push (test-mevedel-session-persistence--publish-generation
                  session session-dir segment (format "Capture source %d\n" index) 1)
                 heads))
         (mevedel-journal-pins-retain session-dir capture heads)
         (test-mevedel-session-persistence--publish-generation
          session session-dir segment "Later turn\n" 2)
         (test-mevedel-publication--collect session)
         (dolist (head heads)
           (let* ((publication (mevedel-session-publication-read session-dir head))
                  (artifact (cdr (assoc "segment-0001.chat.org"
                                        (plist-get publication :artifacts)))))
             (should (string-prefix-p
                      "Capture source"
                      (mevedel-session-control-fs-read-file
                       (plist-get artifact :published))))))
         (mevedel-journal-pins-release session-dir capture)
         (should (= 3 (test-mevedel-publication--collect session)))
         (should (= 1 (length
                       (seq-filter
                        (lambda (head) (file-exists-p (file-name-concat session-dir head)))
                        heads))))))))

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

  :doc "collects every collectible generation through bounded steps"
  (let ((mevedel-session-publication-keep-recent-generations 1))
    (test-mevedel-session-publication--with-published
     "publication-bound" "mevedel-publication-bound-" ?b
     (lambda (session session-dir segment)
       (dolist (turns '(1 1 1 2))
         (test-mevedel-session-persistence--publish-generation
          session session-dir segment (format "Turn %d\n" turns) turns))
       (should (= 2 (test-mevedel-publication--collect session)))
       ;; The pass was complete; nothing is left for later.
       (should (= 0 (test-mevedel-publication--collect
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
            (test-mevedel-publication--collect session))))
       (should (string-match-p
                "Could not collect published generations.*Injected scan failure"
                captured)))))

  :doc "refuses without the lease and for a PID-lock session"
  (should-not
   (test-mevedel-publication--collect
    (mevedel-session--create :authority-mode 'pid-lock)))
  (should-not
   (test-mevedel-publication--collect
    (mevedel-session--create :authority-mode 'portable
                             :save-path "/tmp/mevedel-absent/"))))

(mevedel-deftest mevedel-session-publication--cached-generation
  (:doc "Caches compact observations only after a successful immutable read")
  (let ((mevedel-session-publication--generation-cache
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
       (mevedel-session-publication--cached-generation "/tmp/session/" head))
      (should
       (equal '(:references nil :sidecar nil :transcript-bytes nil)
              (mevedel-session-publication--cached-generation
               "/tmp/session/" head)))
      (should (mevedel-session-publication--cached-generation "/tmp/session/" head))
      (should (= 2 reads)))))

(mevedel-deftest mevedel-session-publication-generation-summary/space ()
  (let* ((root (make-temp-file "mevedel-generation-cache-" t))
         (name "generation-0123456789abcdef0123")
         (head (file-name-concat ".publications" name "manifest.el"))
         (manifest-path (file-name-concat root head))
         (sidecar (file-name-concat ".publications" name "sidecar.data"))
         (generation (list :name name :head head :time (current-time)))
         (mevedel-session-publication--generation-cache (make-hash-table :test #'equal))
         (artifacts
          (cons (list "session.meta.el" :published sidecar :sha256 (make-string 64 ?a))
                (cl-loop for index below 1200
                         collect (list (format "file-history/%d" index)
                                       :published (file-name-concat ".publications" name (format "%d.data" index))
                                       :sha256 (make-string 64 ?b))))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory manifest-path) t)
          (with-temp-file manifest-path
            (prin1 (list :sidecar "session.meta.el" :artifacts artifacts) (current-buffer)))
          (let ((summary (mevedel-session-publication-generation-summary root generation)))
            (should (plist-get summary :manifest-readable-p))
            (should (equal (plist-get summary :references) (list name)))
            (should (equal summary (mevedel-session-publication-generation-summary root generation)))
            (setcar (plist-get summary :references) "changed-by-caller")
            (should (equal (plist-get (mevedel-session-publication-generation-summary root generation) :references)
                           (list name)))
            (should (= 1 (hash-table-count mevedel-session-publication--generation-cache)))
            ;; Retention needs the referenced generations, not every file entry.
            (maphash (lambda (_ value) (should (< (length (prin1-to-string value)) 2048)))
                     mevedel-session-publication--generation-cache)))
      (delete-directory root t))))

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

  :doc "nested target batches keep lease deadlines on their own target clocks"
  (let* ((root (make-temp-file "mevedel-nested-clocks-" t))
         (hosts '("clock-outer" "clock-inner"))
         (clocks '(("clock-outer" . 1000) ("clock-inner" . 100000)))
         (program (symbol-function 'mevedel-session-control-fs-run-program))
         (mevedel-session-durability--client-id (make-string 64 ?c))
         (mevedel-session-durability--disclosed-targets (make-hash-table :test #'equal))
         sessions records appends)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp hosts
          ;; All filesystem operations and lease comparisons remain native.
          ;; Only the returned filesystem clocks differ between these targets.
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations &optional lock-directory)
                       (let ((results (funcall program operations lock-directory)))
                         (cl-mapc
                          (lambda (operation result)
                            (let* ((path (plist-get operation :path))
                                   (host (file-remote-p path 'host))
                                   (now (cdr (assoc host clocks))))
                              (when (and now (eq 'ok (plist-get result :status)))
                                (pcase (plist-get operation :op)
                                  ('target-time (plist-put result :value now))
                                  ((or 'write 'create)
                                   (when (string-match-p "/\\.lease/[0-9]+\\.el\\'" path)
                                     (push (cons now (car (read-from-string
                                                          (plist-get operation :content))))
                                           records)))))))
                          operations results)
                         results))))
            (dolist (host hosts)
              (make-directory (file-name-concat root host))
              (cl-destructuring-bind (_workspace session directory _segment)
                  (test-mevedel-session-persistence--make-remote-restore-fixture
                   host (file-name-concat root host) "Transcript\n")
                (push session sessions)
                (puthash (mevedel-execution-target-identity
                          (mevedel-session-execution-target session))
                         t mevedel-session-durability--disclosed-targets)
                (should (mevedel-session-durability-lease-acquire
                         directory "*nested-clock*" session))
                ;; Exercise the normal steady state after a heartbeat has
                ;; retained the bytes its next renewal may compare and swap.
                (should (mevedel-session-durability-lease-renew session))))
            (setq sessions (nreverse sessions)
                  records nil)
            (unwind-protect
                (cl-labels ((append-log (session text)
                              (push (mevedel-session-publication-append-diagnostic
                                     session
                                     (file-name-concat (mevedel-session-save-path session) "clock.log")
                                     text)
                                    appends)))
                  (mevedel-session-publication-call-with-diagnostic-batch
                   (car sessions)
                   (lambda ()
                     (append-log (car sessions) "outer before\n")
                     (mevedel-session-publication-call-with-diagnostic-batch
                      (cadr sessions)
                      (lambda () (append-log (cadr sessions) "inner\n")))
                     (append-log (car sessions) "outer after\n")))
                  (should records)
                  (dolist (entry records)
                    (let ((now (car entry)) (record (cdr entry)))
                      (should (> (plist-get record :expires-at) now))
                      (should (<= (plist-get record :expires-at)
                                  (+ now mevedel-session-publication-lease-seconds)))))
                  (should (cl-every #'identity appends))
                  (cl-mapc
                   (lambda (session expected)
                     (should (mevedel-session-durability-lease-owned-p session))
                     (should (equal expected
                                    (mevedel-session-control-fs-read-file
                                     (file-name-concat (mevedel-session-save-path session) "clock.log")))))
                   sessions '("outer before\nouter after\n" "inner\n")))
              (dolist (session sessions)
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path session) session)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))

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
      (mevedel-session-publication-call-with-diagnostic-batch
       session
       (lambda ()
         (should (mevedel-session-publication-append-diagnostic
                  session "/x/a.log" "a"))
         (should (mevedel-session-publication-append-diagnostic
                  session "/x/b.log" "b"))))
      (should (= 1 reservations))
      (should (equal '(("/x/a.log" . "a") ("/x/b.log" . "b"))
                     (nreverse appends)))))

  :doc "a nested batch for another session reserves its own lease"
  (let ((outer (mevedel-session--create :authority-mode 'portable))
        (inner (mevedel-session--create :authority-mode 'portable))
        reservations)
    (cl-letf (((symbol-function 'mevedel-session-recovery-refresh) #'ignore)
              ((symbol-function 'mevedel-session-durability-lease-renew)
               (lambda (_) t))
              ((symbol-function 'mevedel-session-durability-lease-owned-p)
               (lambda (_) t))
              ((symbol-function 'mevedel-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-session-control-fs-append-file)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-session-durability-call-with-reserved-lease)
               (lambda (session function)
                 (push session reservations)
                 (funcall function))))
      (mevedel-session-publication-call-with-diagnostic-batch
       outer
       (lambda ()
         (should (mevedel-session-publication-append-diagnostic
                  outer "/outer.log" "outer"))
         (mevedel-session-publication-call-with-diagnostic-batch
          inner
          (lambda ()
            (should (mevedel-session-publication-append-diagnostic
                     inner "/inner.log" "inner"))))))
      (should (equal (list outer inner) (nreverse reservations)))))

  :doc "an unavailable lease declines every append without re-testing"
  (let ((session (mevedel-session--create :authority-mode 'portable))
        (renewals 0))
    (cl-letf (((symbol-function 'mevedel-session-recovery-refresh) #'ignore)
              ((symbol-function 'mevedel-session-durability-lease-renew)
               (lambda (_s) (setq renewals (1+ renewals)) nil)))
      (mevedel-session-publication-call-with-diagnostic-batch
       session
       (lambda ()
         (should-not (mevedel-session-publication-append-diagnostic
                      session "/x/a.log" "a"))
         (should-not (mevedel-session-publication-append-diagnostic
                      session "/x/b.log" "b"))))
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
                     (error "Target refused")
                   (push content written) t)))
              ((symbol-function 'mevedel-session-durability-call-with-reserved-lease)
               (lambda (_s fn) (funcall fn))))
      (mevedel-test--with-captured-diagnostics nil
        (mevedel-session-publication-call-with-diagnostic-batch
         session
         (lambda ()
           (should-not (mevedel-session-publication-append-diagnostic
                        session "/x/bad.log" "bad"))
           (should (mevedel-session-publication-append-diagnostic
                    session "/x/good.log" "good")))))
      (should (equal '("good") written))))

  :doc "a session that does not publish through the lease reserves nothing"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock))
        (reservations 0) (ran nil))
    (cl-letf (((symbol-function 'mevedel-session-durability-call-with-reserved-lease)
               (lambda (_s fn) (setq reservations (1+ reservations)) (funcall fn))))
      (mevedel-session-publication-call-with-diagnostic-batch
       session (lambda () (setq ran t)))
      (should ran)
      (should (= 0 reservations)))))

(mevedel-deftest mevedel-session-publication-abandon (:quiet t)
  ,test
  (test)
  :doc "retries interrupted specialized abandonment without accepting lost recovery"
  (dolist (scenario '(marker-delete payload-delete intent-write missing-without-approval))
    (let* ((root (make-temp-file "mevedel-abandon-retry-" t))
           (repair (make-temp-file "mevedel-abandon-source-" t))
           (workspace (mevedel-workspace--create
                       :type 'project :id root :root root :name "abandon"))
           (session (mevedel-session-create "main" workspace))
           (delete-marker (symbol-function 'mevedel-session-control-fs-delete-file))
           (delete-payload (symbol-function 'mevedel-session-control-fs-delete-directory))
           (write-marker (symbol-function 'mevedel-session-durability--write-plist))
           (confirmations 0)
           marker payload)
      (setf (mevedel-session-save-path session) root
            (mevedel-session-session-id session) "abandon-retry")
      (unwind-protect
          (progn
            (should (mevedel-session-durability-lease-acquire root "abandon" session))
            (write-region "repair bytes" nil (file-name-concat repair "before.el")
                          nil 'silent)
            (mevedel-session-recovery-record-failure session "incomplete rollback" repair)
            (ert-info ((format "Recovery setup: %S"
                               (mevedel-session-pending-publication session)))
              (should (plist-get (mevedel-session-pending-publication session)
                                 :recovery-portable)))
            (setq marker (plist-get (mevedel-session-pending-publication session)
                                    :manual-recovery-marker)
                  payload (plist-get (mevedel-session-pending-publication session)
                                     :manual-recovery))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) (cl-incf confirmations) t)))
              (ert-info ((format "Abandonment failure %s" scenario))
                (if (eq scenario 'missing-without-approval)
                    (progn
                      (delete-directory payload t)
                      (should (string-match-p
                               "Invalid specialized recovery marker"
                               (error-message-string
                                (should-error (mevedel-session-publication-abandon session)))))
                      (should (= confirmations 0)))
                  (cl-letf (((symbol-function 'mevedel-session-control-fs-delete-file)
                             (lambda (path)
                               (if (and (eq scenario 'marker-delete) (equal path marker))
                                   (error "Injected abandonment failure")
                                 (funcall delete-marker path))))
                            ((symbol-function 'mevedel-session-control-fs-delete-directory)
                             (lambda (path)
                               (if (and (eq scenario 'payload-delete) (equal path payload))
                                   (error "Injected abandonment failure")
                                 (funcall delete-payload path))))
                            ((symbol-function 'mevedel-session-durability--write-plist)
                             (lambda (path data)
                               (if (and (eq scenario 'intent-write) (equal path marker))
                                   (error "Injected abandonment failure")
                                 (funcall write-marker path data)))))
                    (should (equal "Injected abandonment failure"
                                   (error-message-string
                                    (should-error
                                     (mevedel-session-publication-abandon session))))))
                  (should (file-exists-p marker))
                  (if (eq scenario 'marker-delete)
                      (should-not (file-exists-p payload))
                    (should (equal "repair bytes"
                                   (mevedel-session-artifacts-read-file-raw
                                    (file-name-concat payload "before.el")))))
                  ;; Reconstruct the pending state from disk, as after client loss.
                  (setf (mevedel-session-pending-publication session) nil)
                  (mevedel-session-recovery-refresh session)
                  (should (mevedel-session-pending-publication session))
                  (with-temp-buffer
                    (setq-local mevedel--session session)
                    (should-error
                     (mevedel-session-artifacts-assert-mutation-authority
                      session (current-buffer))
                     :type 'user-error))
                  (should (= confirmations 1))
                  (should (mevedel-session-publication-abandon session))
                  (should (= confirmations 2))
                  (should-not (file-exists-p marker))
                  (should-not (file-exists-p payload))
                  (should-not (mevedel-session-recovery-read root))
                  (should-not (mevedel-session-pending-publication session))))))
        (mevedel-session-durability--cancel-renewal session)
        (when (file-directory-p repair) (delete-directory repair t))
        (delete-directory root t)
        (mevedel-session-durability-forget-removed-session session)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-session-publication--valid-published-path-p ()
  (let ((prefix ".publications/generation-0123456789abcdef0123/")
        (remote (symbol-function 'file-remote-p))
        (calls 0))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (&rest args) (cl-incf calls) (apply remote args))))
      (dolist (leaf '("manifest.el" "artifact-1" "a b" ".hidden" "a..b" "~name"))
        (should (mevedel-session-publication--valid-published-path-p
                 (concat prefix leaf))))
      (dolist (leaf '("" "." ".." "a/b" "/a" "a/" "../a"))
        (should-not (mevedel-session-publication--valid-published-path-p
                     (concat prefix leaf))))
      (dolist (path '(nil 1 "/ssh:host:/file" "../manifest.el" "~/.publications/x"
                          ".publications/generation-short/manifest.el"))
        (should-not (mevedel-session-publication--valid-published-path-p path))))
    ;; The fixed relative grammar itself proves this is not a remote spelling.
    (should (= 0 calls))))

(provide 'test-mevedel-session-publication)
;;; test-mevedel-session-publication.el ends here
