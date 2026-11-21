;;; test-niwa-session-publication.el -- Publication tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `niwa-session-publication' generation summaries and collection.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-journal-pins)
(require 'niwa-session-checkpoint)
(require 'niwa-session-artifacts)

(niwa-deftest niwa-session-publication--files-sha256 ()
  ,test
  (test)
  :doc "hashes local files in one process exactly as Lisp does, in order"
  (let* ((directory (make-temp-file "niwa-sha-" t))
         (files (mapcar (lambda (spec)
                          (let ((file (file-name-concat directory (car spec)))
                                (coding-system-for-write 'no-conversion))
                            (write-region (cdr spec) nil file nil 'silent)
                            file))
                        (list (cons "plain.txt" "bytes \377\000\n")
                              (cons "spaced name.txt" (make-string 4096 ?x))
                              (cons "caf\u00e9.txt" "")))))
    (unwind-protect
        (let ((expected (mapcar (lambda (file)
                                  (with-temp-buffer
                                    (set-buffer-multibyte nil)
                                    (insert-file-contents-literally file)
                                    (secure-hash 'sha256 (current-buffer))))
                                files))
              (processes 0))
          (skip-unless (executable-find "sha256sum"))
          (let ((call (symbol-function 'call-process)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest args) (cl-incf processes) (apply call args)))
                      ((symbol-function 'secure-hash)
                       (lambda (&rest _) (error "Hashed in Lisp"))))
              (should (equal expected
                             (niwa-session-publication--files-sha256 files)))
              (should (= 1 processes))))
          ;; Without the tool every file still hashes, in Lisp.
          (cl-letf (((symbol-function 'executable-find) #'ignore))
            (should (equal expected
                           (niwa-session-publication--files-sha256 files)))))
      (delete-directory directory t)))

  :doc "hashes from a deleted working directory and preserves relative path resolution"
  (skip-unless (executable-find "sha256sum"))
  (let* ((directory (make-temp-file "niwa-sha-cwd-" t))
         (working (file-name-concat directory "worktree"))
         (source (file-name-concat directory "source"))
         (bytes "Retained publication bytes\n")
         (expected (list (secure-hash 'sha256 bytes)))
         (call (symbol-function 'call-process))
         (processes 0))
    (unwind-protect
        (progn
          (make-directory working)
          (write-region bytes nil source nil 'silent)
          (cl-letf (((symbol-function 'call-process)
                     (lambda (&rest args)
                       (should (file-directory-p default-directory))
                       (should-not (file-remote-p default-directory))
                       (cl-incf processes)
                       (apply call args)))
                    ((symbol-function 'secure-hash)
                     (lambda (&rest _) (error "Hashed in Lisp"))))
            (let ((default-directory (file-name-as-directory working)))
              (should (equal expected
                             (niwa-session-publication--files-sha256
                              '("../source"))))
              (delete-directory working)
              (should (equal expected
                             (niwa-session-publication--files-sha256
                              (list source))))
              (should (equal default-directory
                             (file-name-as-directory working)))))
          (should (= 2 processes)))
      (delete-directory directory t))))

(niwa-deftest niwa-session-publication--fixed-artifact-p ()
  ,test
  (test)
  :doc "keeps fixed copies only for artifacts read from their fixed path"
  (dolist (logical '("session.meta.el" "tool-results/ToolCall-a.txt"
                     "agents/registry.el" "agents/a.compact-1.chat.org"))
    (should (niwa-session-publication--fixed-artifact-p
             (list :logical logical))))
  (dolist (logical '("segment-0001.chat.org" "agents/a.chat.org"
                     "file-history/1"))
    (should-not (niwa-session-publication--fixed-artifact-p
                 (list :logical logical))))
  (should-not (niwa-session-publication--fixed-artifact-p
               (list :logical "tool-results/x" :delete t))))

(niwa-deftest niwa-session-publication--immutable-entry ()
  (let* ((root (make-temp-file "niwa-entry-" t))
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
                  (niwa-session-publication--immutable-entry
                   directory (list :source source :logical "tool-results/result")
                   1 root)))
          ;; At most one read, for the digest, and none where `sha256sum'
          ;; hashes the file; the bytes stay in the source file.
          (should (= reads (if (executable-find "sha256sum") 0 1)))
          (should-not (plist-member entry :content))
          (should (equal source (plist-get entry :source-file)))
          (should (equal (secure-hash 'sha256 bytes)
                         (plist-get (cdr (plist-get entry :entry)) :sha256)))
          (should (equal ".publications/generation-test/000001.data"
                         (plist-get (cdr (plist-get entry :entry)) :published))))
      (delete-directory root t))))

(niwa-deftest niwa-session-publication--prepare-generation ()
  ,test
  (test)
  :doc "shares equal payloads, writes changes, and honors replacement snapshots"
  (let* ((directory (make-temp-file "niwa-generation-" t))
         (root (file-name-concat directory ".publications"))
         (source (file-name-concat directory "source"))
         (artifacts (list (list :logical "session.meta.el" :source source)
                          (list :logical "agents/a.chat.org" :source source)))
         (write
          (lambda (entries)
            ;; The commit runs the planned program as its head commit's prelude.
            (pcase-let ((`(,generation ,overlaid ,operations)
                         (niwa-session-publication--prepare-generation
                          root artifacts directory entries
                          (niwa-session-publication--generation-digests artifacts))))
              (let ((results (niwa-session-control-fs-run-program operations)))
                (dolist (result (cdr results))
                  (should (eq 'ok (plist-get result :status)))))
              (cons generation overlaid)))))
    (unwind-protect
        (progn
          (write-region "first" nil source nil 'silent)
          (let* ((first (funcall write nil))
                 (same (funcall write (cdr first))))
            (should (equal (cdr first) (cdr same)))
            (should (equal '("manifest.el")
                           (directory-files (car same) nil "^[^.].*")))
            (write-region "changed" nil source nil 'silent)
            (let ((changed (funcall write (cdr same))))
              (should-not (equal (cdr same) (cdr changed)))
              (should (= 3 (length (directory-files (car changed) nil "^[^.].*")))))
            ;; Replacement has no prior entries: even equal bytes must be
            ;; supplied by this snapshot, not resurrected from a dropped one.
            (let ((replacement (funcall write nil)))
              (should (= 3 (length (directory-files (car replacement) nil "^[^.].*")))))))
      (delete-directory directory t))))

(niwa-deftest niwa-session-publication--deduplicate-artifacts ()
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
    (should (equal (niwa-session-publication--deduplicate-artifacts artifacts)
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
                     (niwa-session-publication--deduplicate-artifacts artifacts))))
    (should (<= calls 2000))))

(niwa-deftest niwa-session-publication--capture-publication ()
  ,test
  (test)
  :doc "qualifies many immutable artifacts without repeating shared root work"
  (let* ((root (make-temp-file "niwa-publication-paths-" t))
         (prefix ".publications/generation-0123456789abcdef0123/")
         (artifacts (cl-loop for n below 100
                             collect (list (if (zerop n) "session.meta.el"
                                             (format "file-history/%d" n))
                                           :published (concat prefix (format "%d.data" n))
                                           :sha256 (make-string 64 ?a))))
         (raw (list :head (concat prefix "manifest.el")
                    :sidecar "session.meta.el" :artifacts artifacts))
         (original (copy-tree raw))
         (physical (symbol-function 'niwa-session-control-fs-physical-path))
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-control-fs-physical-path)
                   (lambda (path) (cl-incf calls) (funcall physical path))))
          (let ((captured (niwa-session-publication--capture-publication
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
       (niwa-session-publication--capture-publication
        root (list :artifacts (list (list "session.meta.el" :published path)))))))
  (let* ((path ".publications/generation-0123456789abcdef0123/1.data")
         (root "/ssh:example.invalid:/workspace//session/")
         (captured
          (niwa-session-publication--capture-publication
           root (list :artifacts (list (list "session.meta.el" :published path))))))
    (should (equal (plist-get captured :sidecar)
                   (concat "/ssh:example.invalid:/workspace/session/" path))))
  (should-not (niwa-session-publication--capture-publication "/tmp/unused" nil)))

(niwa-deftest niwa-session-publication--delete-batch ()
  ,test
  (test)
  :doc "removes contained recovery while preserving outside and escaping paths"
  (dolist (kind '(inside outside sibling escape alias))
    (let* ((root (make-temp-file "niwa-batch-containment-" t))
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
              (niwa-session-publication--delete-batch (list :directory path))
              (if (memq kind '(inside alias))
                  (should-not (file-exists-p path))
                (should (file-directory-p path)))
              (should (file-directory-p outside))))
        (delete-directory root t)))))

(defun test-niwa-session-publication--with-published
    (host prefix client-id body)
  "Call BODY with a leased portable session fixture.
HOST names the mock target, PREFIX the temporary root, and CLIENT-ID the
durability client character.  BODY receives the session, its directory,
and its segment path."
  (let ((niwa-session-publication--generation-cache
         (make-hash-table :test #'equal))
        (niwa-session-publication--facts-cache
         (make-hash-table :test #'equal))
        (local-root
         (file-name-as-directory (make-temp-file prefix t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((niwa-session-durability--client-id
                   (make-string 64 client-id))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*publication-test*" session))
              (unwind-protect
                  (progn
                    (setf (niwa-session-publication session)
                          (niwa-session-publication-read session-dir))
                    (funcall body session session-dir segment))
                (niwa-session-durability-lease-release
                 session-dir session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-session-publication-publish/deleted-working-directory ()
  ,test
  (test)
  :doc "commits retained transcript bytes after the caller's worktree is removed"
  (skip-unless (executable-find "sha256sum"))
  (test-niwa-session-publication--with-published
   "publication-deleted-cwd" "niwa-publication-deleted-cwd-" ?d
   (lambda (session session-dir segment)
     (let* ((working (make-temp-file "niwa-removed-worktree-" t))
            (old-head (plist-get (niwa-session-publication session) :head))
            (bytes "Transcript saved after worktree cleanup\n")
            (raw (niwa-session-artifacts-build-sidecar session (current-buffer))))
       (delete-directory working)
       (let ((default-directory (file-name-as-directory working)))
         (should
          (niwa-session-publication-publish
           session
           (list (list :path segment :content bytes)
                 (list :path (file-name-concat session-dir "session.meta.el")
                       :content (niwa-session-artifacts-printed-value raw)
                       :commit-marker t)))))
       (should-not (niwa-session-pending-publication session))
       (should-not (equal old-head
                          (plist-get (niwa-session-publication session) :head)))
       (should (equal bytes (niwa-session-artifacts-read-artifact
                             session "segment-0001.chat.org" t)))))))

(niwa-deftest niwa-session-publication-publish/checkpoint-retry (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-checkpoint-retry" "niwa-checkpoint-retry-" ?c
   (lambda (session session-dir _segment)
     (let* ((raw (niwa-session-artifacts-build-sidecar session (current-buffer)))
            (old-head (plist-get (niwa-session-publication session) :head))
            (commit (symbol-function 'niwa-session-durability-commit-publication-head))
            (attempts 0)
            collected)
       (niwa-session-checkpoint-seed session (plist-put (copy-tree raw) :total-turn-count 1))
       (setq raw (plist-put raw :total-turn-count 7))
       (let* ((candidate (niwa-session-checkpoint-capture session "/root" raw))
              (artifact (list :path (file-name-concat session-dir "session.meta.el")
                              :content (niwa-session-artifacts-printed-value raw)
                              :commit-marker t :checkpoint candidate)))
         (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
                    (lambda (&rest _) (cl-incf attempts) nil)))
           (should-error (niwa-session-publication-publish session (list artifact))))
         (should (= attempts 1))
         (should (equal old-head (plist-get (niwa-session-publication session) :head)))
         (should (= 1 (plist-get (niwa-session-checkpoint-project session raw) :total-turn-count)))
         (should (eq candidate
                     (plist-get
                      (niwa-session-publication--batch-marker
                       (car (plist-get (niwa-session-pending-publication session) :batches)))
                      :checkpoint)))
         ;; Retry must install the staged capture, not the now-newer live root.
         (setf (niwa-session-turn-count session) 99)
         (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
                    (lambda (&rest args) (cl-incf attempts) (apply commit args)))
                   ((symbol-function 'niwa-session-collection-schedule)
                    (lambda (owner)
                      (push (plist-get (niwa-session-checkpoint-project owner raw)
                                       :total-turn-count) collected))))
           (niwa-session-publication-retry session))
         (should (= attempts 2))
         (should (equal collected '(7)))
         (should-not (niwa-session-pending-publication session))
         (should (= 7 (plist-get (niwa-session-checkpoint-project session raw) :total-turn-count)))
         (should (= 7 (plist-get (niwa-session-codec-read
                                 (plist-get (niwa-session-publication session) :sidecar))
                                :total-turn-count))))))))

(niwa-deftest niwa-session-publication-publish/checkpoint-queue (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-checkpoint-queue" "niwa-checkpoint-queue-" ?c
   (lambda (session session-dir _segment)
     (let* ((raw (niwa-session-artifacts-build-sidecar session (current-buffer)))
            (first (plist-put (copy-tree raw) :total-turn-count 7))
            (second (plist-put (copy-tree raw) :total-turn-count 8))
            (first-candidate (niwa-session-checkpoint-capture session "/root" first))
            (second-candidate (niwa-session-checkpoint-capture session "/root" second))
            observations)
       (cl-letf (((symbol-function 'niwa-session-collection-schedule)
                  (lambda (owner)
                    (push (plist-get (niwa-session-checkpoint-project owner raw)
                                     :total-turn-count) observations)
                    (when (= (length observations) 1)
                      (should
                       (eq 'queued
                           (niwa-session-publication-publish
                            session
                            (list (list :path (file-name-concat session-dir "session.meta.el")
                                        :content (niwa-session-artifacts-printed-value second)
                                        :commit-marker t :checkpoint second-candidate)))))
                      (should (= 7 (plist-get (niwa-session-checkpoint-project owner raw)
                                             :total-turn-count)))))))
         (niwa-session-publication-publish
          session (list (list :path (file-name-concat session-dir "session.meta.el")
                              :content (niwa-session-artifacts-printed-value first)
                              :commit-marker t :checkpoint first-candidate))))
       (should (equal '(8 7) observations))
       (should-not (niwa-session-publication-queue session))
       (should (= 8 (plist-get (niwa-session-checkpoint-project session raw) :total-turn-count)))))))

(niwa-deftest niwa-session-publication--prewrite-operations (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-prewrite" "niwa-prewrite-" ?c
   (lambda (session session-dir _segment)
     (let* ((path (file-name-concat session-dir "session.meta.el"))
            (raw (niwa-session-artifacts-build-sidecar session (current-buffer)))
            (run (symbol-function 'niwa-session-control-fs-run-program))
            (publish
             (lambda (title)
               (let ((programs 0)
                     (head (plist-get (niwa-session-publication session) :head)))
                 (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                            (lambda (&rest args) (cl-incf programs) (apply run args))))
                   (niwa-session-publication-publish
                    session
                    (list (list :path path :commit-marker t
                                :content (niwa-session-artifacts-printed-value
                                          (plist-put (copy-tree raw) :title title))))))
                 (should-not (equal head (plist-get (niwa-session-publication session)
                                                    :head)))
                 ;; The fixed copy holds what the new head committed.
                 (should (equal (niwa-session-control-fs-read-file path)
                                (niwa-session-control-fs-read-file
                                 (plist-get (niwa-session-publication session)
                                            :sidecar))))
                 (should (equal title (plist-get (niwa-session-codec-read path)
                                                 :title)))
                 programs)))
            (merged (funcall publish "merged")))
       ;; A newer `.el' entry beside the lease stops the reservation-side write;
       ;; the fixed sidecar is then written by its own program, as before.
       (niwa-session-control-fs-write-file
        (file-name-concat (niwa-session-durability--lease-path session-dir) "zzz.el")
        "not a generation")
       (should (= (1+ merged) (funcall publish "fallback")))))))

(niwa-deftest niwa-session-publication--read-staged-sidecar ()
  ,test
  (test)
  :doc "reads a staged local sidecar and refuses a target path"
  (let ((file (make-temp-file "niwa-staged-sidecar-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(:title \"Grüße\" :n 1)"))
          (should (equal '(:title "Grüße" :n 1)
                         (niwa-session-publication--read-staged-sidecar file)))
          (should-error (niwa-session-publication--read-staged-sidecar
                         "/ssh:host:/tmp/staged")))
      (delete-file file))))

(niwa-deftest niwa-session-publication--commit-marker-publication (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-deadline" "niwa-deadline-" ?c
   (lambda (session session-dir _segment)
     (let* ((path (file-name-concat session-dir "session.meta.el"))
            (raw (niwa-session-artifacts-build-sidecar session (current-buffer)))
            (commit (symbol-function 'niwa-session-durability--commit-lease))
            (head (plist-get (niwa-session-publication session) :head))
            (lapsed 0))
       ;; The generation's upload outlasts the deadline its commit was decided
       ;; with: the prelude's clock proof refuses, and a fresh commit follows.
       (cl-letf (((symbol-function 'niwa-session-durability--commit-lease)
                  (lambda (directory generation expected record &optional deadline)
                    (when (and deadline
                               (plist-get niwa-session-durability--commit-riders :before)
                               (not (plist-get niwa-session-durability--commit-riders
                                               :consumed)))
                      (cl-incf lapsed)
                      (setq deadline 1))
                    (funcall commit directory generation expected record deadline))))
         (niwa-session-publication-publish
          session (list (list :path path :commit-marker t
                              :content (niwa-session-artifacts-printed-value
                                        (plist-put (copy-tree raw) :title "late"))))))
       (should (= 1 lapsed))
       (should-not (equal head (plist-get (niwa-session-publication session) :head)))
       (should (equal "late" (plist-get (niwa-session-codec-read
                                         (plist-get (niwa-session-publication session)
                                                    :sidecar))
                                        :title)))
       (should-not (niwa-session-pending-publication session))))))

(niwa-deftest niwa-session-publication-publish/checkpoint-queued-projection (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-queued-projection" "niwa-queued-projection-" ?c
   (lambda (session session-dir _segment)
     (let* ((raw (niwa-session-artifacts--live-sidecar session (current-buffer)))
            (old (plist-put (copy-tree raw) :total-turn-count 1))
            (_ (niwa-session-checkpoint-seed session old))
            (root (plist-put (copy-tree raw) :total-turn-count 7))
            (candidate (niwa-session-checkpoint-capture session "/root" root))
            (agent (niwa-session-checkpoint-capture session "/root/worker"))
            (path (file-name-concat session-dir "session.meta.el"))
            (write-generation (symbol-function 'niwa-session-publication--prepare-generation))
            (queued nil)
            observed)
       (cl-letf (((symbol-function 'niwa-session-publication--prepare-generation)
                  (lambda (&rest args)
                    (unless queued
                      (setq queued t)
                      ;; Both sidecars freeze while the old root is committed.
                      (dolist (checkpoint (list nil agent))
                        (should
                         (eq 'queued
                             (niwa-session-publication-publish
                              session
                              (list (list :path path :commit-marker t :checkpoint checkpoint
                                          :content
                                          (encode-coding-string
                                           (niwa-session-artifacts-printed-value old)
                                           'utf-8-unix)))))))
                      (should (= 1 (plist-get (niwa-session-checkpoint-project session nil)
                                             :total-turn-count))))
                    (apply write-generation args)))
                 ((symbol-function 'niwa-session-collection-schedule)
                  (lambda (owner)
                    (push (plist-get (niwa-session-codec-read
                                      (plist-get (niwa-session-publication owner) :sidecar))
                                     :total-turn-count) observed))))
         (niwa-session-publication-publish
          session (list (list :path path :commit-marker t :checkpoint candidate
                              :content (niwa-session-artifacts-printed-value root)))))
       ;; Neither ordinary metadata nor the agent commit can regress the root.
       (should (equal '(7 7 7) observed))
       (should (= 7 (plist-get (niwa-session-checkpoint-project session nil) :total-turn-count)))))))

(niwa-deftest niwa-session-publication-publish/checkpoint-queue-retry-proofs (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-queue-proofs" "niwa-queue-proofs-" ?c
   (lambda (session session-dir _segment)
     (let* ((raw (niwa-session-artifacts--live-sidecar session (current-buffer)))
            (mail (list :id "consumed" :type 'MAIL :sender "/root/worker"
                        :recipient "/root" :payload "consumed" :timestamp (current-time)))
            (stale (plist-put (copy-tree raw) :messages (list mail)))
            (path (file-name-concat session-dir "session.meta.el"))
            (write-generation (symbol-function 'niwa-session-publication--prepare-generation))
            (calls 0))
       (niwa-session-checkpoint-consume session "/root" (list mail))
       (let ((candidate (niwa-session-checkpoint-capture session "/root" raw)))
         (cl-letf (((symbol-function 'niwa-session-publication--prepare-generation)
                    (lambda (&rest args)
                      (cl-incf calls)
                      (pcase calls
                        (1
                         (should (eq 'queued
                                     (niwa-session-publication-publish
                                      session
                                      (list (list :path path :commit-marker t
                                                  :content (niwa-session-artifacts-printed-value stale)))))))
                        (2 (error "Injected queued publication failure")))
                      (apply write-generation args))))
           (should (equal '(error "Injected queued publication failure")
                          (should-error
                           (niwa-session-publication-publish
                            session (list (list :path path :commit-marker t :checkpoint candidate
                                                :content (niwa-session-artifacts-printed-value raw))))))))
         (should (= calls 2))
         (should (niwa-session-pending-publication session))
         ;; The first commit retired this id, but the failed successor still
         ;; owns frozen bytes.  Its suppression proof must survive until retry.
         (should-not (niwa-session-checkpoint-release-proofs session))
         (should-not (plist-get (niwa-session-checkpoint-project session stale) :messages))
         (niwa-session-publication-retry session)
         (should-not (niwa-session-pending-publication session))
         (should-not (plist-get (niwa-session-codec-read
                                 (plist-get (niwa-session-publication session) :sidecar))
                                :messages))
         ;; No queued snapshot remains: proofs can be released, not accumulated
         ;; across the session's lifetime.
         (should (niwa-session-checkpoint-release-proofs session)))))))

(niwa-deftest niwa-session-publication-publish/checkpoint-rotation-receipt (:quiet t)
  (let* ((directory (make-temp-file "niwa-rotation-receipt-" t))
         (workspace (test-niwa-session-persistence--make-workspace directory))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *rotation-receipt*"))
         (commit (symbol-function 'niwa-session-durability-commit-publication-head))
         queued restored save-path)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace)
            (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
            (niwa-session-artifacts-save session buffer)
            (setq save-path (niwa-session-save-path session))
            (niwa-agent-control-send-message session "/root" "Already consumed mail")
            (niwa-tools--handle-message-inject
             (gptel-make-fsm :info (list :buffer buffer :backend nil
                                        :data (list :messages (vector)))))
            (should-not (niwa-session-messages session))
            (should (niwa-transcript-audit-buffer-spans 'mailbox-receipt))
            (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
                       (lambda (&rest args)
                         (unless queued
                           (setq queued t)
                           (should (niwa-session-publication-active-p session))
                           (should (niwa-session-persistence-write-sidecar-now session buffer)))
                         (apply commit args))))
              (niwa-session-artifacts-rotate-segment session buffer "Summary without receipt"))
            (should queued))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (setq buffer nil restored (niwa-session-persistence-restore save-path))
          (with-current-buffer restored
            (should (= 2 (niwa-session-current-segment niwa--session)))
            ;; The receipt now lives only in the predecessor archive, so a
            ;; resurrected sidecar message would be delivered twice here.
            (should-not (niwa-transcript-audit-buffer-spans 'mailbox-receipt))
            (should-not (niwa-tools--mailbox-new-messages niwa--session restored))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (test-niwa-session-persistence--release-and-kill
       restored (and restored (buffer-local-value 'niwa--session restored)))
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-session-publication-publish/checkpoint-rotation-ptc (:quiet t)
  (let* ((directory (make-temp-file "niwa-rotation-ptc-" t))
         (workspace (test-niwa-session-persistence--make-workspace directory))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *rotation-ptc*"))
         (commit (symbol-function 'niwa-session-durability-commit-publication-head))
         queued restored save-path)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace)
            (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
            (niwa-session-artifacts-save session buffer)
            (setq save-path (niwa-session-save-path session))
            (should (niwa-ptc-checkpoint-start session buffer "retired-ptc" "(+ 1 1)"))
            (should (niwa-ptc-checkpoint-update
                     session buffer "retired-ptc"
                     '(:state settled :result "2" :render-data (:kind ptc :outcome success :calls nil))))
            (niwa-ptc-checkpoint--insert (car (niwa-session-ptc-checkpoints session)))
            (niwa-ptc-checkpoint-clear-settled session "/root" buffer)
            (should-not (niwa-session-ptc-checkpoints session))
            (should (niwa-tool-render-data-segment-bounds "retired-ptc"))
            (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
                       (lambda (&rest args)
                         (unless queued
                           (setq queued t)
                           (should (niwa-session-publication-active-p session))
                           (should (niwa-session-persistence-write-sidecar-now session buffer)))
                         (apply commit args))))
              (niwa-session-artifacts-rotate-segment session buffer "Summary without ToolCall"))
            (should queued))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (setq buffer nil restored (niwa-session-persistence-restore save-path))
          (with-current-buffer restored
            (should (= 2 (niwa-session-current-segment niwa--session)))
            (should-not (niwa-tool-render-data-segment-bounds "retired-ptc"))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (test-niwa-session-persistence--release-and-kill
       restored (and restored (buffer-local-value 'niwa--session restored)))
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-session-publication-publish/transition-late-mail (:quiet t)
  (dolist (storage '(project file))
    (dolist (transition '(rotate fresh))
      (let* ((directory (make-temp-file "niwa-transition-late-" t))
             (workspace (if (eq storage 'file)
                            (test-niwa-session-persistence--make-file-workspace directory)
                          (test-niwa-session-persistence--make-workspace directory)))
             (session (niwa-session-create "main" workspace))
             (buffer (generate-new-buffer " *transition-late*"))
             (commit (symbol-function 'niwa-session-durability-commit-publication-head))
             (write (symbol-function 'niwa-session-codec-write))
             entered restored save-path)
        (unwind-protect
            (ert-info ((format "%s %s" storage transition))
              (with-current-buffer buffer
                (niwa-chat-prepare-transcript-buffer)
                (setq-local niwa--session session niwa--workspace workspace)
                (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
                (niwa-session-artifacts-save session buffer)
                (setq save-path (niwa-session-save-path session))
                (niwa-agent-control-send-message session "/root" "Late transition mail")
                (cl-labels
                    ((inject ()
                       (unless entered
                         (setq entered t)
                         (with-current-buffer buffer
                           (niwa-tools--handle-message-inject
                            (gptel-make-fsm :info
                                            (list :buffer buffer :backend nil
                                                  :data (list :messages (vector)))))))))
                  (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
                             (lambda (&rest args) (inject) (apply commit args)))
                            ((symbol-function 'niwa-session-codec-write)
                             (lambda (&rest args)
                               (when (and (eq storage 'file)
                                          (equal (car args) (niwa-session-artifacts-sidecar-path save-path)))
                                 (inject))
                               (apply write args))))
                    (if (eq transition 'rotate)
                        (niwa-session-artifacts-rotate-segment session buffer "Summary")
                      (niwa-session-artifacts-start-fresh-segment session buffer))))
                (should entered)
                (should (buffer-modified-p))
                (should (string-search "Late transition mail" (buffer-string)))
                (should (niwa-transcript-audit-buffer-spans 'mailbox-receipt))
                ;; The ordinary save must see the dirty late receipt and commit
                ;; it before the pending consumption proof can retire.
                (niwa-session-artifacts-save session buffer))
              (test-niwa-session-persistence--release-and-kill buffer session)
              (setq buffer nil restored
                    (niwa-session-persistence-restore save-path nil nil workspace))
              (with-current-buffer restored
                (should (= 2 (niwa-session-current-segment niwa--session)))
                (should (string-search "Late transition mail" (buffer-string)))
                (should-not (niwa-tools--mailbox-new-messages niwa--session restored))))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (test-niwa-session-persistence--release-and-kill
           restored (and restored (buffer-local-value 'niwa--session restored)))
          (niwa-workspace-clear-registry)
          (delete-directory directory t))))))

(niwa-deftest niwa-session-publication-publish/admission-recovery-unread (:quiet t)
  (dolist (writer '(sidecar transcript))
    (let* ((directory (make-temp-file "niwa-admission-unread-" t))
           (workspace (test-niwa-session-persistence--make-workspace directory))
           (session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *admission-unread*"))
           (read-recovery (symbol-function 'niwa-session-recovery-read))
           (sidecar-artifact (symbol-function 'niwa-session-artifacts--sidecar-publication-artifact))
           (late-reads 0) (depth 0) frozen early late injecting restored save-path)
      (unwind-protect
          (ert-info ((format "Recovery admission for %s writer" writer))
            (with-current-buffer buffer
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session niwa--workspace workspace)
              (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
              (niwa-session-artifacts-save session buffer)
              (setq save-path (niwa-session-save-path session))
              (cl-letf (((symbol-function 'niwa-session-artifacts--sidecar-publication-artifact)
                         (lambda (&rest args)
                           (cl-incf depth)
                           (unwind-protect
                               (prog1 (apply sidecar-artifact args)
                                 (when (and (= depth 1) (not injecting))
                                   (setq frozen t)))
                             (cl-decf depth))))
                        ((symbol-function 'niwa-session-recovery-read)
                         (lambda (&rest args)
                           (unless injecting
                             (when (and frozen (not (niwa-session-publication-active-p session)))
                               (cl-incf late-reads))
                             (when (or (and (not frozen) (not early))
                                       (and frozen (not late)
                                            (not (niwa-session-publication-active-p session))))
                               (setq injecting t)
                               (unwind-protect
                                   (progn
                                     (should-not (niwa-session-publication-active-p session))
                                     (if frozen (setq late t) (setq early t))
                                     (niwa-agent-control-send-message
                                      session "/root" (if frozen "Acknowledged after capture"
                                                        "Acknowledged before capture")))
                                 (setq injecting nil))))
                           (apply read-recovery args))))
                (if (eq writer 'sidecar)
                    (niwa-session-artifacts-publish-sidecar-state session buffer)
                  (niwa-session-artifacts-publish-transcript-state
                   session buffer buffer-file-name (buffer-string) nil)))
              (when late
                (should (seq-some
                         (lambda (mail) (equal "Acknowledged after capture" (plist-get mail :payload)))
                         (plist-get
                          (niwa-session-codec-read
                           (plist-get (niwa-session-publication session) :sidecar))
                          :messages)))))
            (test-niwa-session-persistence--release-and-kill buffer session)
            (setq buffer nil restored (niwa-session-persistence-restore save-path))
            (with-current-buffer restored
              ;; Every acknowledged enqueue must survive, including one from
              ;; a redundant publisher admission read after sidecar capture.
              (should early)
              (dolist (payload (append '("Acknowledged before capture")
                                       (and late '("Acknowledged after capture"))))
                (should (seq-some
                         (lambda (mail)
                           (equal payload (plist-get mail :payload)))
                         (niwa-tools--mailbox-new-messages niwa--session restored)))))
            ;; Authority, capture and publication share one recovery observation.
            (should (= late-reads 0)))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (test-niwa-session-persistence--release-and-kill
         restored (and restored (buffer-local-value 'niwa--session restored)))
        (niwa-workspace-clear-registry)
        (delete-directory directory t)))))

(niwa-deftest niwa-session-publication-publish/transition-unread-enqueue (:quiet t)
  (dolist (transition '(rotate fresh))
    (let* ((directory (make-temp-file "niwa-transition-unread-" t))
           (workspace (test-niwa-session-persistence--make-workspace directory))
           (session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *transition-unread*"))
           (instructions (symbol-function 'niwa-session-artifacts--sidecar-artifact))
           entered restored save-path)
      (unwind-protect
          (ert-info ((format "Unread enqueue during %s" transition))
            (with-current-buffer buffer
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session niwa--workspace workspace)
              (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
              (niwa-session-artifacts-save session buffer)
              (setq save-path (niwa-session-save-path session))
              (cl-letf (((symbol-function 'niwa-session-artifacts--sidecar-artifact)
                         (lambda (&rest args)
                           (unless entered
                             (setq entered t)
                             (should-not (niwa-session-publication-active-p session))
                             (niwa-agent-control-send-message
                              session "/root" "Acknowledged late unread")
                             ;; The producer actually commits before the outer
                             ;; transition enters publication, not merely queues.
                             (should (seq-some
                                      (lambda (mail)
                                        (equal "Acknowledged late unread" (plist-get mail :payload)))
                                      (plist-get
                                       (niwa-session-codec-read
                                        (plist-get (niwa-session-publication session) :sidecar))
                                       :messages))))
                           (apply instructions args))))
                (if (eq transition 'rotate)
                    (niwa-session-artifacts-rotate-segment session buffer "Summary")
                  (niwa-session-artifacts-start-fresh-segment session buffer))))
            (should entered)
            (test-niwa-session-persistence--release-and-kill buffer session)
            (setq buffer nil restored (niwa-session-persistence-restore save-path))
            (with-current-buffer restored
              (should (= 2 (niwa-session-current-segment niwa--session)))
              (should (seq-some
                       (lambda (mail) (equal "Acknowledged late unread" (plist-get mail :payload)))
                       (niwa-tools--mailbox-new-messages niwa--session restored)))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (test-niwa-session-persistence--release-and-kill
         restored (and restored (buffer-local-value 'niwa--session restored)))
        (niwa-workspace-clear-registry)
        (delete-directory directory t)))))

(niwa-deftest niwa-session-publication-publish/transition-nested-root-save (:quiet t)
  (dolist (storage '(project file))
    (dolist (transition '(rotate fresh))
      (let* ((directory (make-temp-file "niwa-transition-nested-" t))
             (workspace (if (eq storage 'file)
                            (test-niwa-session-persistence--make-file-workspace directory)
                          (test-niwa-session-persistence--make-workspace directory)))
             (session (niwa-session-create "main" workspace))
             (buffer (generate-new-buffer " *transition-nested*"))
             (instructions (symbol-function 'niwa-session-artifacts--sidecar-artifact))
             (save-instructions (symbol-function 'niwa-session-artifacts-save-instructions))
             entered restored save-path)
        (unwind-protect
            (ert-info ((format "Nested root save during %s %s" storage transition))
              (with-current-buffer buffer
                (niwa-chat-prepare-transcript-buffer)
                (setq-local niwa--session session niwa--workspace workspace)
                (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
                (niwa-session-artifacts-save session buffer)
                (setq save-path (niwa-session-save-path session))
                (cl-labels
                    ((attempt ()
                       (unless entered
                         (setq entered t)
                         (goto-char (point-max))
                         (insert "Late nested root edit\n")
                         (let ((head (niwa-session-publication session)))
                           (should (equal '(user-error "Session checkpoint is being prepared")
                                          (should-error
                                           (niwa-session-artifacts-save session buffer t))))
                           (should (eq head (niwa-session-publication session)))))))
                  (cl-letf (((symbol-function 'niwa-session-artifacts--sidecar-artifact)
                             (lambda (&rest args) (attempt) (apply instructions args)))
                            ((symbol-function 'niwa-session-artifacts-save-instructions)
                             (lambda (&rest args)
                               (when (eq storage 'file) (attempt))
                               (apply save-instructions args))))
                    (if (eq transition 'rotate)
                        (niwa-session-artifacts-rotate-segment session buffer "Summary")
                      (niwa-session-artifacts-start-fresh-segment session buffer))))
                (should entered)
                (should (buffer-modified-p))
                (should (string-search "Late nested root edit" (buffer-string)))
                ;; Only the next non-overlapping checkpoint may commit the edit.
                (niwa-session-artifacts-save session buffer))
              (test-niwa-session-persistence--release-and-kill buffer session)
              (setq buffer nil restored
                    (niwa-session-persistence-restore save-path nil nil workspace))
              (with-current-buffer restored
                (should (= 2 (niwa-session-current-segment niwa--session)))
                (should (string-search "Late nested root edit" (buffer-string)))))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (test-niwa-session-persistence--release-and-kill
           restored (and restored (buffer-local-value 'niwa--session restored)))
          (niwa-workspace-clear-registry)
          (delete-directory directory t))))))

(niwa-deftest niwa-session-publication-publish/transition-ancillary-commit-before-failure (:quiet t)
  (dolist (transition '(rotate fresh clear))
    (let* ((directory (make-temp-file "niwa-transition-abort-" t))
           (workspace (test-niwa-session-persistence--make-workspace directory))
           (session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *transition-abort*"))
           entered captured sealed restored save-path)
      (unwind-protect
          (ert-info ((format "Ancillary commit then %s preparation failure" transition))
            (with-current-buffer buffer
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session niwa--workspace workspace)
              (insert "Original question\n\n" (propertize "Answer\n" 'gptel 'response))
              (niwa-session-artifacts-save session buffer)
              (setq save-path (niwa-session-save-path session))
              (let ((head (plist-get (niwa-session-publication session) :head))
                    (sidecar (symbol-function 'niwa-session-artifacts--sidecar-artifact)))
                (cl-letf (((symbol-function 'niwa-session-artifacts--sidecar-artifact)
                           (lambda (&rest args)
                             (if entered
                                 (apply sidecar args)
                               (setq entered t)
                               (niwa-agent-control-send-message session "/root" "Surviving unread")
                               (should-not (equal head (plist-get (niwa-session-publication session) :head)))
                               (error "Injected transition preparation failure"))))
                          ((symbol-function 'niwa-journal-capture-checkpoint)
                           (lambda (&rest _) (setq captured t) '(:id "unsealed-capture")))
                          ((symbol-function 'niwa-journal-capture-seal-and-schedule)
                           (lambda (&rest _) (setq sealed t))))
                  (should (equal '(error "Injected transition preparation failure")
                                 (should-error
                                  (if (eq transition 'rotate)
                                      (niwa-session-artifacts-rotate-segment session buffer "Summary")
                                    (niwa-session-artifacts-start-fresh-segment
                                     session buffer :clear (eq transition 'clear))))))))
              (should entered)
              (should (eq captured (eq transition 'clear)))
              (should-not sealed)
              (should (= 1 (niwa-session-current-segment session)))
              (should-not (niwa-session-chapters session))
              (should-not (niwa-session-pending-publication session))
              (should (string-search "Original question" (buffer-string))))
            (test-niwa-session-persistence--release-and-kill buffer session)
            (setq buffer nil restored (niwa-session-persistence-restore save-path))
            (with-current-buffer restored
              (should (= 1 (niwa-session-current-segment niwa--session)))
              (should (string-search "Original question" (buffer-string)))
              (should (seq-some
                       (lambda (mail) (equal "Surviving unread" (plist-get mail :payload)))
                       (niwa-tools--mailbox-new-messages niwa--session restored)))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (test-niwa-session-persistence--release-and-kill
         restored (and restored (buffer-local-value 'niwa--session restored)))
        (niwa-workspace-clear-registry)
        (delete-directory directory t)))))

(niwa-deftest niwa-session-publication-publish/transition-late-predecessor (:quiet t)
  (let* ((directory (make-temp-file "niwa-predecessor-late-" t))
         (workspace (test-niwa-session-persistence--make-workspace directory))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *predecessor-late*"))
         (finalize (symbol-function 'niwa-session-artifacts-finalized-segment-text))
         entered)
    (unwind-protect
        (with-current-buffer buffer
          (niwa-chat-prepare-transcript-buffer)
          (setq-local niwa--session session niwa--workspace workspace)
          (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
          (niwa-session-artifacts-save session buffer)
          (cl-letf (((symbol-function 'niwa-session-artifacts-finalized-segment-text)
                     (lambda (&rest args)
                       (prog1 (apply finalize args)
                         (unless entered
                           (setq entered t)
                           (with-current-buffer buffer
                             (goto-char (point-max))
                             (insert "Late predecessor edit\n")))))))
            (should (equal '(error "Transcript changed during segment transition; retry")
                           (should-error
                            (niwa-session-artifacts-rotate-segment session buffer "Summary")))))
          (should entered)
          (should (= 1 (niwa-session-current-segment session)))
          (should (string-search "Late predecessor edit" (buffer-string)))
          (should (buffer-modified-p))
          (should-not (niwa-session-pending-publication session)))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-session-publication-publish/checkpoint-cleanup-error (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-checkpoint-cleanup" "niwa-checkpoint-cleanup-" ?c
   (lambda (session session-dir _segment)
     (dolist (required '(nil t))
       (let* ((raw (plist-put (niwa-session-artifacts-build-sidecar session (current-buffer))
                              :total-turn-count (if required 8 7)))
              (candidate (niwa-session-checkpoint-capture session "/root" raw))
              (artifact (list :path (file-name-concat session-dir "session.meta.el")
                              :content (niwa-session-artifacts-printed-value raw)
                              :commit-marker t :checkpoint candidate))
              (calls 0))
         (cl-letf (((symbol-function 'niwa-session-collection-schedule)
                    (lambda (owner)
                      (cl-incf calls)
                      (should (= (plist-get raw :total-turn-count)
                                 (plist-get (niwa-session-checkpoint-project owner raw)
                                            :total-turn-count)))
                      (error "Injected postcommit collection failure"))))
           (if required
               (should (niwa-session-publication-publish session (list artifact) t))
             (should (equal '(error "Injected postcommit collection failure")
                            (should-error
                             (niwa-session-publication-publish session (list artifact))
                             :type 'error)))))
         (should (= calls 1))
         (should-not (niwa-session-pending-publication session))
         (should (= (plist-get raw :total-turn-count)
                    (plist-get (niwa-session-checkpoint-project session nil) :total-turn-count)))
         (should (= (plist-get raw :total-turn-count)
                    (plist-get (niwa-session-codec-read
                                (plist-get (niwa-session-publication session) :sidecar))
                               :total-turn-count))))))))

(niwa-deftest niwa-session-publication-publish/checkpoint-actual-artifact (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-checkpoint-actual" "niwa-checkpoint-actual-" ?c
   (lambda (session session-dir _segment)
     (let* ((raw (niwa-session-artifacts-build-sidecar session (current-buffer)))
            (first (plist-put (copy-tree raw) :total-turn-count 7))
            (last (plist-put (copy-tree raw) :total-turn-count 8))
            (path (file-name-concat session-dir "session.meta.el")))
       ;; Last-write-wins chooses the actual sidecar, not the marker object.
       (niwa-session-publication-publish
        session
        (list (list :path path :commit-marker t
                    :checkpoint (niwa-session-checkpoint-capture session "/root" first)
                    :content (niwa-session-artifacts-printed-value first))
              (list :path path
                    :checkpoint (niwa-session-checkpoint-capture session "/root" last)
                    :content (niwa-session-artifacts-printed-value last))))
       (should (= 8 (plist-get (niwa-session-checkpoint-project session raw) :total-turn-count)))
       (should (= 8 (plist-get (niwa-session-codec-read
                               (plist-get (niwa-session-publication session) :sidecar))
                              :total-turn-count)))))))

(niwa-deftest niwa-session-publication-publish/checkpoint-stale (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-checkpoint-stale" "niwa-checkpoint-stale-" ?c
   (lambda (session session-dir _segment)
     (let* ((raw (niwa-session-artifacts-build-sidecar session (current-buffer)))
            (candidate (niwa-session-checkpoint-capture session "/root" raw))
            (head (plist-get (niwa-session-publication session) :head))
            (commits 0))
       (niwa-session-checkpoint-seed session raw)
       (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
                  (lambda (&rest _) (cl-incf commits) t)))
         (should
          (equal
           '(error "Checkpoint candidate belongs to a different owner epoch")
           (should-error
            (niwa-session-publication-publish
             session
             (list (list :path (file-name-concat session-dir "session.meta.el")
                         :commit-marker t :checkpoint candidate
                         :content (niwa-session-artifacts-printed-value raw))))))))
       (should (zerop commits))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (niwa-session-publication-clear-transient session)))))

(niwa-deftest niwa-session-publication-publish/checkpoint-replacement (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-checkpoint-replace" "niwa-checkpoint-replace-" ?c
   (lambda (session session-dir _segment)
     (let* ((raw (niwa-session-artifacts-build-sidecar session (current-buffer)))
            (path (file-name-concat session-dir "session.meta.el")))
       (niwa-session-checkpoint-seed session (plist-put (copy-tree raw) :total-turn-count 1))
       (setq raw (plist-put raw :total-turn-count 7))
       ;; A metadata-only commit cannot adopt live, uncheckpointed root state.
       (niwa-session-publication-publish
        session (list (list :path path :commit-marker t
                            :content (niwa-session-artifacts-printed-value raw))))
       (should (= 1 (plist-get (niwa-session-checkpoint-project session raw) :total-turn-count)))
       (niwa-session-publication-publish
        session (list (list :path path :commit-marker t :replace t
                            :content (niwa-session-artifacts-printed-value raw))))
       (should (= 7 (plist-get (niwa-session-checkpoint-project session raw) :total-turn-count)))))))

(niwa-deftest niwa-session-publication-publish/restore-repair-freeze (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-restore-freeze" "niwa-restore-freeze-" ?c
   (lambda (session _session-dir segment)
     (let ((buffer (generate-new-buffer " *restore-repair-freeze*"))
           (row '(:id "repaired" :conversation "/root" :state settled
                  :args (:expression "1") :result "done"))
           (late (list :id "late-mail" :type 'MAIL :sender "/root/worker"
                       :recipient "/root" :payload "after freeze" :timestamp (current-time)))
           checkpoint artifacts)
       (unwind-protect
           (progn
             (niwa-session-checkpoint-seed session '(:total-turn-count 7))
             (setf (niwa-session-turn-count session) 7)
             (with-current-buffer buffer
               (org-mode)
               (insert "Already repaired transcript\n")
               (setq buffer-file-name segment))
             (cl-letf (((symbol-function 'niwa-session-artifacts-check-target-incarnation) #'ignore)
                       ((symbol-function 'niwa-ptc-checkpoint-reconcile)
                        (lambda (owner)
                          (niwa-session-checkpoint-defer-ptc owner "/root" (list row))
                          '(0 . 1)))
                       ((symbol-function 'niwa--chat-buffer-init-common)
                        (lambda (&rest _)
                          ;; Initialization can run arbitrary hooks after freeze.
                          (setf (niwa-session-turn-count session) 99)
                          (niwa-session-checkpoint-consume session "/root" (list late))))
                       ((symbol-function 'niwa-agent-persistence-restore-tree)
                        (lambda (&rest _) 0))
                       ((symbol-function 'niwa-session-artifacts-load-instructions) #'ignore)
                       ((symbol-function 'niwa-session-persistence-notify-session-event) #'ignore))
               (should (= 1 (niwa-session-persistence--hydrate-restored-buffer
                             buffer session (niwa-session-workspace session) segment t nil "resume"
                             (lambda (artifact) (push artifact artifacts))
                             (lambda (candidate) (setq checkpoint candidate)))))
               (should checkpoint)
               (should (= 1 (length artifacts)))
               (niwa-session-persistence--finish-restored-buffer
                buffer session nil t artifacts checkpoint))
             (let ((saved (niwa-session-codec-read
                           (plist-get (niwa-session-publication session) :sidecar))))
               (should (= 7 (plist-get saved :total-turn-count)))
               (should-not (plist-get saved :ptc-checkpoints))
               (should (equal '("late-mail")
                              (mapcar (lambda (mail) (plist-get mail :id))
                                      (plist-get saved :messages)))))
             (let ((projected (niwa-session-checkpoint-project session nil)))
               (should (= 7 (plist-get projected :total-turn-count)))
               (should-not (plist-get projected :ptc-checkpoints))
               (should (equal '("late-mail")
                              (mapcar (lambda (mail) (plist-get mail :id))
                                      (plist-get projected :messages))))))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (set-buffer-modified-p nil)
             (setq-local kill-buffer-hook nil))
           (kill-buffer buffer)))))))

(niwa-deftest niwa-session-publication-publish/transaction-scope (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-clock" "niwa-publication-clock-" ?c
   (lambda (session session-dir _segment)
     (let* ((path (file-name-concat session-dir "session.meta.el"))
            (content (niwa-session-artifacts-printed-value
                      (niwa-session-artifacts-build-sidecar session (current-buffer))))
            (program (symbol-function 'niwa-session-control-fs-run-program))
            (now (float-time))
            (clocks 0))
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now))
                 ((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (ops &rest args)
                    (cl-incf clocks (cl-count 'target-time ops :key (lambda (op) (plist-get op :op))))
                    (apply program ops args))))
         (dotimes (index 2)
           (niwa-session-publication-publish
            session (list (list :path path :content content :commit-marker t)) t)
           ;; One fresh target clock per publication; subsequent ownership
           ;; checks in that publication reuse only the bounded observation.
           (should (= clocks (1+ index)))))
       (should (equal content
                      (niwa-session-control-fs-read-file
                       (plist-get (niwa-session-publication session) :sidecar))))))))

(niwa-deftest niwa-session-publication-publish/telemetry (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-telemetry" "niwa-publication-telemetry-" ?c
   (lambda (session session-dir _segment)
     (require 'niwa-telemetry)
     (let* ((content (niwa-session-artifacts-printed-value
                      (niwa-session-artifacts-build-sidecar session (current-buffer))))
            (niwa-telemetry--lag-windows
             (list (list session :request-id "request-1" :until nil :counts nil)))
            (niwa-session-control-fs--measure-min-ms 0)
            (events (lambda (event)
                      (seq-filter (lambda (entry) (eq event (plist-get entry :event)))
                                  (niwa-session-telemetry-pending session)))))
       (setf (niwa-session-telemetry-pending session) nil)
       (niwa-session-publication-publish
        session (list (list :path (file-name-concat session-dir "session.meta.el")
                            :content content :commit-marker t))
        t)
       ;; One publication event carrying its input size, and the target
       ;; programs it ran attributed to the watched session.
       (let ((publication (funcall events 'session-publication)))
         (should (= 1 (length publication)))
         (should (= 1 (plist-get (car publication) :artifact-count)))
         (should (= (string-bytes content) (plist-get (car publication) :input-bytes)))
         (should (plist-get (car publication) :duration-ms))
         (should-not (plist-get (car publication) :dropped-keys)))
       (let ((programs (funcall events 'control-program)))
         (should programs)
         (should (cl-every (lambda (entry)
                             (and (symbolp (plist-get entry :kind))
                                  (natnump (plist-get entry :operation-count))))
                           programs)))))))

(niwa-deftest niwa-session-publication-publish/final-commit (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-final-commit" "niwa-final-commit-" ?c
   (lambda (session session-dir _segment)
     (let* ((content (niwa-session-artifacts-printed-value
                      (niwa-session-artifacts-build-sidecar session (current-buffer))))
            (updates nil)
            (count (lambda (original session accepted status &rest args)
                     (push status updates)
                     (apply original session accepted status args))))
       (advice-add 'niwa-session-durability--update-owned-lease :around count)
       (unwind-protect
           (niwa-session-publication-publish
            session (list (list :path (file-name-concat session-dir "session.meta.el")
                                :content content :commit-marker t))
            t)
         (advice-remove 'niwa-session-durability--update-owned-lease count))
       ;; The reservation, then a commit that also ends the window.
       (should (equal '(publishing active) (nreverse updates)))
       (should (eq 'active (plist-get (niwa-session-lease session) :status)))
       (should (eq 'owned (plist-get (niwa-session-lease session) :state)))))))

(niwa-deftest niwa-session-publication-publish/transcript-copies (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-transcripts" "niwa-transcript-copies-" ?c
   (lambda (session session-dir _segment)
     (let* ((paths '("segment-0002.chat.org" "agents/worker.chat.org"))
            (marker (list :path (file-name-concat session-dir "session.meta.el")
                          :content (niwa-session-artifacts-printed-value
                                    (niwa-session-artifacts-build-sidecar
                                     session (current-buffer)))
                          :commit-marker t)))
       (niwa-session-publication-publish
        session (append (mapcar (lambda (logical)
                                  (list :path (file-name-concat session-dir logical)
                                        :content "published transcript")) paths)
                        (list marker)) t)
       (dolist (logical paths)
         (should-not (file-exists-p (file-name-concat session-dir logical)))
         (should (equal "published transcript"
                        (niwa-session-artifacts-read-artifact session logical t))))
       ;; Discovery still has its small sidecar; mutable authored files keep
       ;; their ordinary file path as well as their immutable publication.
       (should (file-exists-p (plist-get marker :path)))
       (let ((path (file-name-concat session-dir "artifacts/example.chat.org")))
         (niwa-session-publication-publish
          session (list (list :path path :content "authored") marker) t)
         (should (equal "authored" (niwa-session-control-fs-read-file path))))))))

(niwa-deftest niwa-session-publication--stage-artifacts ()
  ,test
  (test)
  :doc "stages string and file-backed artifacts as local byte files"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-stage-" t)))
         (session (niwa-session--create :name "stage" :save-path root))
         (source (file-name-concat root "outside-source"))
         (bytes (concat (make-string 5000 ?b) (unibyte-string 0 255)))
         batch)
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil source nil 'silent))
          (setq batch (niwa-session-publication--stage-artifacts
                       session
                       (list (list :path (file-name-concat root "segment-0001.chat.org")
                                   :content "Unicode \u03bb")
                             (list :path (file-name-concat root "agents/a.chat.org")
                                   :source-file source))))
          (let ((artifacts (plist-get batch :artifacts)))
            (should (equal "Unicode \u03bb"
                           (decode-coding-string
                            (niwa-session-artifacts-read-file-raw
                             (plist-get (nth 0 artifacts) :source))
                            'utf-8-unix)))
            (should (equal bytes (niwa-session-artifacts-read-file-raw
                                  (plist-get (nth 1 artifacts) :source))))
            ;; Neither the string nor the original file path is retained.
            (should-not (plist-member (nth 0 artifacts) :content))
            (should-not (plist-member (nth 1 artifacts) :source-file))
            (should (file-exists-p source))))
      (when batch (delete-directory (plist-get batch :directory) t))
      (delete-directory root t))))

(niwa-deftest niwa-session-publication-publish/staging-only (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-staging" "niwa-staging-only-" ?c
   (lambda (session session-dir _segment)
     (let* ((logical "agents/staged.chat.org")
            (program (symbol-function 'niwa-session-control-fs-run-program))
            (writes 0))
       ;; A transcript without a marker only stages bytes for the next commit.
       (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (operations &rest args)
                    (when (seq-some (lambda (op)
                                      (memq (plist-get op :op) '(write create verify)))
                                    operations)
                      (cl-incf writes))
                    (apply program operations args))))
         (should (eq 'published
                     (niwa-session-publication-publish
                      session (list (list :path (file-name-concat session-dir logical)
                                          :content "staged transcript"))))))
       (should (= 0 writes))
       (should (niwa-session-publication-uncommitted-artifact session logical))
       ;; The next marker commits it through the immutable publication.
       (niwa-session-publication-publish
        session (list (list :path (file-name-concat session-dir "session.meta.el")
                            :content (niwa-session-artifacts-printed-value
                                      (niwa-session-artifacts-build-sidecar
                                       session (current-buffer)))
                            :commit-marker t))
        t)
       (should-not (niwa-session-publication-uncommitted-artifact session logical))
       (should (equal "staged transcript"
                      (niwa-session-artifacts-read-artifact session logical t)))))))

(niwa-deftest niwa-session-publication-publish/tombstone-cost (:quiet t)
  (test-niwa-session-publication--with-published
   "publication-tombstones" "niwa-tombstones-" ?c
   (lambda (session session-dir _segment)
     (let* ((paths (cl-loop for n below 40
                            collect (file-name-concat session-dir "file-history" (format "unused-%d" n))))
            (marker (list :path (file-name-concat session-dir "session.meta.el")
                          :content (niwa-session-artifacts-printed-value
                                    (niwa-session-artifacts-build-sidecar session (current-buffer)))
                          :commit-marker t))
            (program (symbol-function 'niwa-session-control-fs-run-program))
            (programs 0) old-head)
       (niwa-session-publication-publish
        session (append (mapcar (lambda (path) (list :path path :content "retained bytes")) paths)
                        (list marker)))
       (should-not (seq-some #'file-exists-p paths))
       (setq old-head (plist-get (niwa-session-publication session) :head))
       (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (&rest args) (cl-incf programs) (apply program args))))
         (niwa-session-publication-publish
          session (append (mapcar (lambda (path) (list :path path :delete t)) paths)
                          (list marker))))
       (should (<= programs 20))
       (should-not (assoc "file-history/unused-0" (plist-get (niwa-session-publication session) :artifacts)))
       (let* ((old (niwa-session-publication-read session-dir old-head))
              (entry (cdr (assoc "file-history/unused-0" (plist-get old :artifacts)))))
         (should (equal "retained bytes" (niwa-session-control-fs-read-file (plist-get entry :published)))))))))

(niwa-deftest niwa-session-publication-generation-summaries ()
  ,test
  (test)
  :doc "bounds picker facts while reading each manifest once"
  (let ((niwa-session-publication-summary-scan-max 1))
    (test-niwa-session-publication--with-published
     "publication-summaries" "niwa-publication-summaries-" ?c
     (lambda (session session-dir segment)
       (test-niwa-session-persistence--publish-generation
        session session-dir segment "Turn one\n" 1)
       (test-niwa-session-persistence--publish-generation
        session session-dir segment "Turn two\n" 2)
       (let ((summaries
              (niwa-session-publication-generation-summaries session-dir)))
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

(niwa-deftest niwa-session-publication-generation-summary ()
  ,test
  (test)
  :doc "one generation exposes references and optional turn facts"
  (test-niwa-session-publication--with-published
   "one-summary" "niwa-one-summary-" ?c
   (lambda (session directory segment)
     (let* ((head (test-niwa-session-persistence--publish-generation
                   session directory segment "Turn one\n" 1))
            (generation (seq-find
                         (lambda (item) (equal head (plist-get item :head)))
                         (niwa-session-publication--generation-names directory)))
            (summary (niwa-session-publication-generation-summary directory generation)))
       (should (equal head (plist-get summary :head)))
       (should (plist-get summary :manifest-readable-p))
       (should (plist-get summary :references))
       (should-not (plist-member summary :turn-count))
       (should (= 1 (plist-get (niwa-session-publication-generation-summary
                               directory generation t) :turn-count)))))))

(niwa-deftest niwa-session-publication-head-facts ()
  ,test
  (test)
  :doc "reads one published head through the validated manifest boundary"
  (test-niwa-session-publication--with-published
   "publication-head-facts" "niwa-publication-head-facts-" ?d
   (lambda (session session-dir segment)
     (let* ((head (test-niwa-session-persistence--publish-generation
                   session session-dir segment "Turn one\n" 1))
            (facts (niwa-session-publication-head-facts session-dir head)))
       (should (= 1 (plist-get facts :turn-count)))))))

(niwa-deftest niwa-session-publication-settled-summary-p ()
  ,test
  (test)
  :doc "distinguishes settled states from mid-turn and unreadable heads"
  (should (niwa-session-publication-settled-summary-p
           '(:turn-count 2 :prompt (:cum-turn 2))))
  (should (niwa-session-publication-settled-summary-p
           '(:turn-count 0 :prompt nil)))
  (should-not (niwa-session-publication-settled-summary-p
               '(:turn-count 2 :prompt (:cum-turn 3))))
  (should-not (niwa-session-publication-settled-summary-p
               '(:turn-count nil :prompt nil))))

(defun test-niwa-publication--collect (session)
  "Drain native bounded collection for SESSION, returning deleted directories."
  (condition-case err
      (when (and (niwa-session-codec-portable-authority-p session)
                 (niwa-session-durability-lease-owned-p session))
        (let* ((summaries (niwa-session-publication-generation-summaries
                           (niwa-session-save-path session) most-positive-fixnum))
               (plan (niwa-session-publication-collection-plan session summaries))
               (steps 0))
          (while (niwa-session-publication-collect-step session plan)
            (cl-incf steps)
            (should (< steps 1000)))
          (plist-get plan :deleted-directories)))
    (error
     (display-warning 'niwa (format "Could not collect published generations: %s"
                                      (error-message-string err)) :warning)
     nil)))

(niwa-deftest niwa-session-publication-collect-step/files ()
  (let ((niwa-session-publication-keep-recent-generations 1))
    (test-niwa-session-publication--with-published
     "publication-file-collection" "niwa-file-collection-" ?a
     (lambda (session directory _segment)
       (setf (niwa-session-turn-count session) 1)
       (let ((marker (list :path (file-name-concat directory "session.meta.el")
                           :content (niwa-session-artifacts-printed-value
                                     (niwa-session-artifacts-build-sidecar session (current-buffer)))
                           :commit-marker t)))
         (niwa-session-publication-publish
          session (list (list :path (file-name-concat directory "stable") :content "keep")
                        (list :path (file-name-concat directory "obsolete") :content "discard") marker))
         (let* ((old (plist-get (niwa-session-publication session) :head))
                (manifest (niwa-session-publication-read directory old))
                (stable (plist-get (cdr (assoc "stable" (plist-get manifest :artifacts))) :published))
                (obsolete (plist-get (cdr (assoc "obsolete" (plist-get manifest :artifacts))) :published)))
           (niwa-session-publication-publish
            session (list (list :path (file-name-concat directory "obsolete") :delete t) marker))
           (let ((current (plist-get (niwa-session-publication session) :head)))
             (set-file-times (file-name-concat directory old) '(1 0 0 0))
             (let* ((summaries (niwa-session-publication-generation-summaries
                                directory most-positive-fixnum))
                    (plan (niwa-session-publication-collection-plan session summaries))
                    (program (symbol-function 'niwa-session-control-fs-run-program)))
               (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                          (lambda (operations &optional lock-directory)
                            (when (seq-some
                                   (lambda (op)
                                     (and (memq (plist-get op :op) '(delete-file delete-directory))
                                          (not (equal (file-name-nondirectory (plist-get op :path))
                                                      "manifest.el")))) operations)
                              (error "Injected interrupted payload sweep"))
                            (funcall program operations lock-directory))))
                 (should
                  (equal '(error "Injected interrupted payload sweep")
                         (should-error
                          (while (niwa-session-publication-collect-step session plan))))))
               ;; Every remaining discoverable head is still readable.  The
               ;; interrupted pass retired obsolete heads before any payload.
               (should-not (file-exists-p (file-name-concat directory old)))
               (should (file-exists-p obsolete))
               (dolist (generation (niwa-session-publication--generation-names directory))
                 (should (niwa-session-publication-read directory (plist-get generation :head)))))
             (test-niwa-publication--collect session)
             (should-not (file-exists-p (file-name-concat directory old)))
             (should-not (file-exists-p obsolete))
             (should (equal "keep" (niwa-session-control-fs-read-file stable)))
             (should (niwa-session-publication-read directory current))
             ;; Once its last retained reference disappears, an artifact-only
             ;; generation is reclaimed too, despite having no manifest.
             ;; The equal sidecar was shared too; replace those bytes so this
             ;; publication really removes the generation's last reference.
             (plist-put marker :content (concat (plist-get marker :content) "\n"))
             (niwa-session-publication-publish
              session (list (list :path (file-name-concat directory "stable") :delete t) marker))
             (set-file-times (file-name-concat directory current) '(1 0 0 0))
             (test-niwa-publication--collect session)
             (should-not (file-exists-p (file-name-directory stable))))))))))

(niwa-deftest niwa-session-publication-collect-step/batched-listings ()
  (let ((niwa-session-publication-keep-recent-generations 1))
    (test-niwa-session-publication--with-published
     "publication-list-batch" "niwa-list-batch-" ?a
     (lambda (session directory _segment)
       (let ((marker (list :path (file-name-concat directory "session.meta.el")
                           :content (niwa-session-artifacts-printed-value
                                     (niwa-session-artifacts-build-sidecar session (current-buffer)))
                           :commit-marker t))
             (program (symbol-function 'niwa-session-control-fs-run-program))
             (largest 0))
         (dotimes (index 10)
           (niwa-session-publication-publish
            session (list (list :path (file-name-concat directory (format "stable-%d" index))
                                :content "retained") marker)))
         (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                    (lambda (operations &optional lock-directory)
                      (setq largest (max largest (cl-count 'list-directory operations
                                                          :key (lambda (op) (plist-get op :op)))))
                      (funcall program operations lock-directory))))
           (test-niwa-publication--collect session))
         (should (= 8 largest))
         (dotimes (index 10)
           (should (equal "retained" (niwa-session-artifacts-read-artifact
                                      session (format "stable-%d" index) t)))))))))

(niwa-deftest niwa-session-publication--retained-heads ()
  (let ((niwa-session-publication-keep-recent-generations 1))
    (should (equal (niwa-session-publication--retained-heads
                    '((:head "recent" :turn-count 2 :prompt (:cum-turn 3))
                      (:head "settled" :turn-count 2)
                      (:head "duplicate" :turn-count 2)
                      (:head "fork" :turn-count 2 :fork-point-id "fork")
                      (:head "unknown")))
                   '("recent" "settled" "fork" "unknown")))))

(niwa-deftest niwa-session-publication-collection-plan ()
  (test-niwa-session-publication--with-published
   "publication-plan" "niwa-publication-plan-" ?d
   (lambda (session directory _segment)
     (let ((summaries (niwa-session-publication-generation-summaries directory most-positive-fixnum)))
       (cl-letf (((symbol-function 'niwa-session-publication--read-publication-raw)
                  (lambda (&rest _) (error "Planning decoded a manifest"))))
         (let ((plan (niwa-session-publication-collection-plan session summaries)))
           (should (member (plist-get (niwa-session-publication session) :head)
                           (plist-get plan :heads)))
           (should (= 0 (hash-table-count (plist-get plan :keep))))))))))

(niwa-deftest niwa-session-publication-collect-step/freshness ()
  ;; Three same-turn generations beside the current head: whatever order their
  ;; names sort in, retention keeps two and collection reaches a deletion.
  (let ((niwa-session-publication-keep-recent-generations 1))
    (test-niwa-session-publication--with-published
     "publication-plan-freshness" "niwa-plan-freshness-" ?e
     (lambda (session directory segment)
       (let* ((old (test-niwa-session-persistence--publish-generation session directory segment "old" 1))
              (_ (test-niwa-session-persistence--publish-generation session directory segment "mid" 1))
              (current (test-niwa-session-persistence--publish-generation session directory segment "new" 1))
              (summaries (niwa-session-publication-generation-summaries directory most-positive-fixnum))
              (plan (niwa-session-publication-collection-plan session summaries))
              (capture (make-string 64 ?f))
              (original (symbol-function 'niwa-session-control-fs-run-program))
              (programs 0))
         ;; Reading a retained manifest is one program: no clock or pin proof.
         (should (plist-get plan :heads))
         (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                    (lambda (&rest args) (cl-incf programs) (apply original args))))
           (niwa-session-publication-collect-step session plan))
         (should (= 1 programs))
         (while (not (plist-get plan :marked))
           (niwa-session-publication-collect-step session plan))
         (niwa-journal-pins-retain directory capture (list old))
         ;; Steps that only read may continue; the program that would delete
         ;; proves the pin set on the target first and ends the plan.
         (while (niwa-session-publication-collect-step session plan))
         (should-not (plist-get plan :operations))
         (should (= 0 (plist-get plan :deleted-files)))
         (should (= 0 (plist-get plan :deleted-directories)))
         (should (niwa-session-publication-read directory old))
         (should (niwa-session-publication-read directory current))
         (niwa-journal-pins-release directory capture)
         ;; A later head joins the retained set instead of ending the plan.
         (let ((later (test-niwa-session-persistence--publish-generation
                       session directory segment "later" 2)))
           (should (niwa-session-publication-collect-step session plan))
           (should (equal later (plist-get plan :head)))
           (should (gethash (file-name-concat (plist-get plan :root) later)
                            (plist-get plan :keep)))))))))

(niwa-deftest niwa-session-publication-collect-step/batched-directories ()
  ;; Wholly unreferenced generations need no listing, so their deletion shares
  ;; one step -- one program carrying its own proofs -- instead of one each.
  (let ((niwa-session-publication-keep-recent-generations 1))
    (test-niwa-session-publication--with-published
     "publication-batched-directories" "niwa-batched-directories-" ?d
     (lambda (session directory segment)
       (dotimes (n 5)
         (test-niwa-session-persistence--publish-generation
          session directory segment (format "draft %d" n) 1))
       (let ((original (symbol-function 'niwa-session-control-fs-run-program))
             (batches nil))
         (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                    (lambda (operations &rest args)
                      (let* ((verbs (mapcar (lambda (op) (plist-get op :op)) operations))
                             (deleted (cl-count 'delete-directory verbs)))
                        (when (> deleted 0)
                          (should (equal '(verify list-directory before-time verify-list)
                                         (seq-take verbs 4)))
                          (push deleted batches)))
                      (apply original operations args))))
           (should (>= (test-niwa-publication--collect session) 3)))
         (should (= 1 (length batches)))
         (should (>= (car batches) 3)))))))

(niwa-deftest niwa-session-publication-collect-step/lapsed ()
  ,test
  (test)
  :doc "a lease lapsing on the target clock ends the plan without deleting"
  (let ((niwa-session-publication-keep-recent-generations 1))
    (test-niwa-session-publication--with-published
     "publication-lapsed" "niwa-lapsed-" ?d
     (lambda (session directory segment)
       (dotimes (n 3)
         (test-niwa-session-persistence--publish-generation
          session directory segment (format "draft %d" n) 1))
       (let* ((before (length (niwa-session-publication--generation-names directory)))
              (summaries (niwa-session-publication-generation-summaries
                          directory most-positive-fixnum))
              (plan (niwa-session-publication-collection-plan session summaries))
              (lease (niwa-session-lease session)))
         (unwind-protect
             (progn
               (setf (niwa-session-lease session) (plist-put (copy-sequence lease) :expires-at 1))
               (while (niwa-session-publication-collect-step session plan)))
           (setf (niwa-session-lease session) lease))
         (should (= 0 (plist-get plan :deleted-directories)))
         (should (= before (length (niwa-session-publication--generation-names directory))))
         (should (> (test-niwa-publication--collect session) 0)))))))

(niwa-deftest niwa-session-publication-collect-step/unreadable ()
  (test-niwa-session-publication--with-published
   "publication-unreadable" "niwa-publication-unreadable-" ?b
   (lambda (session directory _segment)
     (let* ((summaries (niwa-session-publication-generation-summaries directory most-positive-fixnum))
            (plan (niwa-session-publication-collection-plan session summaries))
            (path (file-name-concat directory (plist-get plan :head)))
            (bytes (niwa-session-control-fs-read-file path)))
       (unwind-protect
           (progn
             (niwa-session-control-fs-write-file path "(:broken")
             (should-error (niwa-session-publication-collect-step session plan))
             (should-not (plist-get plan :marked))
             (should-not (plist-get plan :operations))
             (should (= (length summaries)
                        (length (niwa-session-publication--generation-names directory)))))
         (niwa-session-control-fs-write-file path bytes))))))

(niwa-deftest niwa-session-publication-collect-step ()
  ,test
  (test)
  :doc "keeps the current head, settled turn states, and a recent grace window"
  (let ((niwa-session-publication-keep-recent-generations 1)
        ;; Collection must inspect beyond the picker/listing scan window.
        (niwa-session-publication-summary-scan-max 1))
    (test-niwa-session-publication--with-published
     "publication-collect" "niwa-publication-collect-" ?a
     (lambda (session session-dir segment)
      (cl-flet ((publish (transcript turns)
                  (test-niwa-session-persistence--publish-generation
                   session session-dir segment transcript turns)))
        ;; One settled turn, three further saves of that same state, then
        ;; a second settled turn.
        (let* ((turn-one (list (publish "Turn one\n" 1)
                               (publish "Turn one, more\n" 1)
                               (publish "Turn one, more still\n" 1)
                               (publish "Turn one, complete\n" 1)))
               (current (publish "Turn two\n" 2))
               (before (length (niwa-session-publication--generation-names
                                session-dir)))
               (deleted (test-niwa-publication--collect
                         session))
               (kept (mapcar (lambda (entry) (plist-get entry :head))
                             (niwa-session-publication--generation-names
                              session-dir))))
          (should (> before 5))
          (should (> deleted 0))
          (should (member current kept))
          (should (= (length kept)
                     (hash-table-count
                      niwa-session-publication--generation-cache)))
          (should (= (length kept)
                     (hash-table-count
                      niwa-session-publication--facts-cache)))
          ;; One generation stands for the earlier settled turn; the three
          ;; saves of that same state are gone.  Which one survives is not
          ;; the contract -- they restore the same conversation.
          (let ((survivors (seq-filter (lambda (head) (member head kept))
                                       turn-one)))
            (should (= 1 (length survivors)))
            ;; What survived still resolves through whatever its manifest
            ;; carries forward.
            (should (niwa-session-publication-read
                     session-dir (car survivors))))
          (should (equal "Turn two\n"
                         (niwa-session-artifacts-read-artifact
                          session "segment-0001.chat.org" t)))
          ;; Collecting again finds nothing left to reclaim.
          (should (= 0 (test-niwa-publication--collect
                        session))))))))

  :doc "retains captured evidence until its journal pin is released"
  (let ((niwa-session-publication-keep-recent-generations 1))
    (test-niwa-session-publication--with-published
     "publication-journal-pin" "niwa-publication-journal-pin-" ?a
     (lambda (session session-dir segment)
       (let ((capture (make-string 64 ?a))
             heads)
         (dotimes (index 4)
           (push (test-niwa-session-persistence--publish-generation
                  session session-dir segment (format "Capture source %d\n" index) 1)
                 heads))
         (niwa-journal-pins-retain session-dir capture heads)
         (test-niwa-session-persistence--publish-generation
          session session-dir segment "Later turn\n" 2)
         (test-niwa-publication--collect session)
         (dolist (head heads)
           (let* ((publication (niwa-session-publication-read session-dir head))
                  (artifact (cdr (assoc "segment-0001.chat.org"
                                        (plist-get publication :artifacts)))))
             (should (string-prefix-p
                      "Capture source"
                      (niwa-session-control-fs-read-file
                       (plist-get artifact :published))))))
         (niwa-journal-pins-release session-dir capture)
         (should (= 3 (test-niwa-publication--collect session)))
         (should (= 1 (length
                       (seq-filter
                        (lambda (head) (file-exists-p (file-name-concat session-dir head)))
                        heads))))))))

  :doc "reads an immutable generation's manifest and facts once"
  (let ((reads 0))
    (test-niwa-session-publication--with-published
     "publication-cache" "niwa-publication-cache-" ?c
     (lambda (session session-dir segment)
       (test-niwa-session-persistence--publish-generation
        session session-dir segment "Turn one\n" 1)
       (let ((raw (symbol-function
                   'niwa-session-publication--read-publication-raw)))
         (cl-letf (((symbol-function
                     'niwa-session-publication--read-publication-raw)
                    (lambda (&rest args)
                      (setq reads (1+ reads))
                      (apply raw args))))
           (niwa-session-publication-generation-summaries
            session-dir most-positive-fixnum)
           (let ((first-pass reads))
             (should (> first-pass 0))
             ;; A committed generation cannot change, so a second listing
             ;; does not reread its manifest or sidecar.
             (niwa-session-publication-generation-summaries
              session-dir most-positive-fixnum)
             (should (= first-pass reads))))))))

  :doc "collects every collectible generation through bounded steps"
  (let ((niwa-session-publication-keep-recent-generations 1))
    (test-niwa-session-publication--with-published
     "publication-bound" "niwa-publication-bound-" ?b
     (lambda (session session-dir segment)
       (dolist (turns '(1 1 1 2))
         (test-niwa-session-persistence--publish-generation
          session session-dir segment (format "Turn %d\n" turns) turns))
       (should (= 2 (test-niwa-publication--collect session)))
       ;; The pass was complete; nothing is left for later.
       (should (= 0 (test-niwa-publication--collect
                     session))))))

  :doc "warns instead of failing when collection cannot inspect generations"
  (test-niwa-session-publication--with-published
   "publication-collect-failure" "niwa-publication-collect-failure-" ?f
   (lambda (session _session-dir _segment)
     (let (captured)
       (niwa-test--with-captured-diagnostics captured
         (cl-letf (((symbol-function
                     'niwa-session-publication-generation-summaries)
                    (lambda (&rest _) (error "Injected scan failure"))))
           (should-not
            (test-niwa-publication--collect session))))
       (should (string-match-p
                "Could not collect published generations.*Injected scan failure"
                captured)))))

  :doc "refuses without a workspace or an owned lease"
  (should-not
   (test-niwa-publication--collect
    (niwa-session--create)))
  (let* ((root (make-temp-file "niwa-collection-no-lease-" t))
         (session (niwa-session-create
                   "unowned" (niwa-workspace--create
                              :type 'file :id root :root root))))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) root)
          (should (niwa-session-codec-portable-authority-p session))
          (should-not (niwa-session-durability-lease-owned-p session))
          (should-not (test-niwa-publication--collect session)))
      (delete-directory root t))))

(niwa-deftest niwa-session-publication--cached-generation
  (:doc "Caches compact observations only after a successful immutable read")
  (let ((niwa-session-publication--generation-cache
         (make-hash-table :test #'equal))
        (head ".publications/generation-deadbeef/manifest.el")
        (reads 0))
    (cl-letf (((symbol-function
                'niwa-session-publication--read-publication-raw)
               (lambda (&rest _)
                 (setq reads (1+ reads))
                 (if (= reads 1)
                     (error "Transient read failure")
                   '(:head "generation")))))
      (should-not
       (niwa-session-publication--cached-generation "/tmp/session/" head))
      (should
       (equal '(:references nil :sidecar nil :transcript-bytes nil)
              (niwa-session-publication--cached-generation
               "/tmp/session/" head)))
      (should (niwa-session-publication--cached-generation "/tmp/session/" head))
      (should (= 2 reads)))))

(niwa-deftest niwa-session-publication-generation-summary/space ()
  (let* ((root (make-temp-file "niwa-generation-cache-" t))
         (name "generation-0123456789abcdef0123")
         (head (file-name-concat ".publications" name "manifest.el"))
         (manifest-path (file-name-concat root head))
         (sidecar (file-name-concat ".publications" name "sidecar.data"))
         (generation (list :name name :head head :time (current-time)))
         (niwa-session-publication--generation-cache (make-hash-table :test #'equal))
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
          (let ((summary (niwa-session-publication-generation-summary root generation)))
            (should (plist-get summary :manifest-readable-p))
            (should (equal (plist-get summary :references) (list name)))
            (should (equal summary (niwa-session-publication-generation-summary root generation)))
            (setcar (plist-get summary :references) "changed-by-caller")
            (should (equal (plist-get (niwa-session-publication-generation-summary root generation) :references)
                           (list name)))
            (should (= 1 (hash-table-count niwa-session-publication--generation-cache)))
            ;; Retention needs the referenced generations, not every file entry.
            (maphash (lambda (_ value) (should (< (length (prin1-to-string value)) 2048)))
                     niwa-session-publication--generation-cache)))
      (delete-directory root t))))

(niwa-deftest niwa-session-publication--cached-sidecar-facts
  (:doc "Caches only successfully read immutable sidecar facts")
  (let ((niwa-session-publication--facts-cache
         (make-hash-table :test #'equal))
        (head ".publications/generation-deadbeef/manifest.el")
        (reads 0))
    (cl-letf (((symbol-function 'niwa-session-publication--sidecar-facts)
               (lambda (&rest _)
                 (setq reads (1+ reads))
                 (and (> reads 1) '(:turn-count 1)))))
      (should-not
       (niwa-session-publication--cached-sidecar-facts
        "/tmp/session/" head '(:head "generation")))
      (should
       (equal '(:turn-count 1)
              (niwa-session-publication--cached-sidecar-facts
               "/tmp/session/" head '(:head "generation"))))
      (should (= 2 reads)))))

(niwa-deftest niwa-session-publication-call-with-diagnostic-batch ()
  ,test
  (test)

  :doc "nested target batches append behind their own proofs, writing no lease records"
  (let* ((root (make-temp-file "niwa-nested-clocks-" t))
         (hosts '("clock-outer" "clock-inner"))
         (clocks '(("clock-outer" . 1000) ("clock-inner" . 100000)))
         (program (symbol-function 'niwa-session-control-fs-run-program))
         (niwa-session-durability--client-id (make-string 64 ?c))
         (niwa-session-durability--disclosed-targets (make-hash-table :test #'equal))
         sessions records appends)
    (unwind-protect
        (niwa-test--with-local-shell-tramp hosts
          ;; All filesystem operations and lease comparisons remain native.
          ;; Only the returned filesystem clocks differ between these targets.
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
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
                  (test-niwa-session-persistence--make-remote-restore-fixture
                   host (file-name-concat root host) "Transcript\n")
                (push session sessions)
                (puthash (niwa-execution-target-identity
                          (niwa-session-execution-target session))
                         t niwa-session-durability--disclosed-targets)
                (should (niwa-session-durability-lease-acquire
                         directory "*nested-clock*" session))
                ;; Exercise the normal steady state after a heartbeat has
                ;; retained the bytes its next renewal may compare and swap.
                (should (niwa-session-durability-lease-renew session))))
            (setq sessions (nreverse sessions)
                  records nil)
            (unwind-protect
                (cl-labels ((append-log (session text)
                              (push (niwa-session-publication-append-diagnostic
                                     session
                                     (file-name-concat (niwa-session-save-path session) "clock.log")
                                     text)
                                    appends)))
                  (niwa-session-publication-call-with-diagnostic-batch
                   (car sessions)
                   (lambda ()
                     (append-log (car sessions) "outer before\n")
                     (niwa-session-publication-call-with-diagnostic-batch
                      (cadr sessions)
                      (lambda () (append-log (cadr sessions) "inner\n")))
                     (append-log (car sessions) "outer after\n")))
                  ;; Each append proves its own session's generation in its
                  ;; own program; no reservation rewrites either lease.
                  (should-not records)
                  (should (cl-every #'identity appends))
                  (cl-mapc
                   (lambda (session expected)
                     (should (niwa-session-durability-lease-owned-p session))
                     (should (equal expected
                                    (niwa-session-control-fs-read-file
                                     (file-name-concat (niwa-session-save-path session) "clock.log")))))
                   sessions '("outer before\nouter after\n" "inner\n")))
              (dolist (session sessions)
                (niwa-session-durability-lease-release
                 (niwa-session-save-path session) session)))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))

  :doc "appends inside a batch share one admission and one held lease"
  (let ((session (niwa-session--create :authority-mode 'portable))
        (admissions 0)
        (holds 0)
        (appends nil))
    (cl-letf (((symbol-function 'niwa-session-recovery-refresh)
               (lambda (_s) (cl-incf admissions)))
              ((symbol-function 'niwa-session-durability-lease-renew)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-durability-lease-owned-p)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-session-control-fs-physical-path)
               #'identity)
              ((symbol-function 'niwa-session-durability-run-owned-program-async)
               (lambda (_s operations callback)
                 (push (mapcar (lambda (op)
                                 (cons (plist-get op :path) (plist-get op :content)))
                               operations)
                       appends)
                 (funcall callback
                          (mapcar (lambda (_) '(:status ok :value nil)) operations)
                          nil)))
              ((symbol-function 'niwa-session-durability-call-with-held-lease)
               (lambda (_s fn) (cl-incf holds) (funcall fn))))
      (niwa-session-publication-call-with-diagnostic-batch
       session
       (lambda ()
         (should (niwa-session-publication-append-diagnostic
                  session "/x/a.log" "a"))
         (should (niwa-session-publication-append-diagnostic
                  session "/x/b.log" "b"))))
      (should (= 1 admissions))
      (should (= 1 holds))
      ;; Both appends travel in one program, in order.
      (should (equal '((("/x/a.log" . "a") ("/x/b.log" . "b"))) appends))
      (should-not (niwa-session-publication-diagnostic-backlog-p session))))

  :doc "a nested batch for another session holds its own lease"
  (let ((outer (niwa-session--create :authority-mode 'portable))
        (inner (niwa-session--create :authority-mode 'portable))
        holds)
    (cl-letf (((symbol-function 'niwa-session-recovery-refresh) #'ignore)
              ((symbol-function 'niwa-session-durability-lease-renew)
               (lambda (_) t))
              ((symbol-function 'niwa-session-durability-lease-owned-p)
               (lambda (_) t))
              ((symbol-function 'niwa-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-session-control-fs-physical-path)
               #'identity)
              ((symbol-function 'niwa-session-durability-run-owned-program-async)
               (lambda (_s operations callback)
                 (funcall callback
                          (mapcar (lambda (_) '(:status ok :value nil)) operations)
                          nil)))
              ((symbol-function 'niwa-session-durability-call-with-held-lease)
               (lambda (session function)
                 (push session holds)
                 (funcall function))))
      (niwa-session-publication-call-with-diagnostic-batch
       outer
       (lambda ()
         (should (niwa-session-publication-append-diagnostic
                  outer "/outer.log" "outer"))
         (niwa-session-publication-call-with-diagnostic-batch
          inner
          (lambda ()
            (should (niwa-session-publication-append-diagnostic
                     inner "/inner.log" "inner"))))))
      (should (equal (list outer inner) (nreverse holds)))))

  :doc "a failed ownership proof keeps the batch for the next one, in order"
  (let ((session (niwa-session--create :authority-mode 'portable))
        (programs nil)
        (prove nil))
    (cl-letf (((symbol-function 'niwa-session-recovery-refresh) #'ignore)
              ((symbol-function 'niwa-session-durability-lease-renew)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-durability-lease-owned-p)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-session-control-fs-physical-path)
               #'identity)
              ((symbol-function 'niwa-session-durability-run-owned-program-async)
               (lambda (_s operations callback)
                 (push (mapcar (lambda (op) (plist-get op :content)) operations)
                       programs)
                 (funcall callback
                          (and prove
                               (mapcar (lambda (_) '(:status ok :value nil))
                                       operations))
                          nil)))
              ((symbol-function 'niwa-session-durability-call-with-held-lease)
               (lambda (_s fn) (funcall fn))))
      (niwa-session-publication-call-with-diagnostic-batch
       session
       (lambda ()
         (should (niwa-session-publication-append-diagnostic
                  session "/x/a.log" "a"))
         (should (niwa-session-publication-append-diagnostic
                  session "/x/b.log" "b"))))
      (should (niwa-session-publication-diagnostic-backlog-p session))
      (setq prove t)
      (niwa-session-publication-call-with-diagnostic-batch
       session
       (lambda ()
         (should (niwa-session-publication-append-diagnostic
                  session "/x/a.log" "c"))))
      (should (equal '(("a" "b") ("a" "b" "c")) (nreverse programs)))
      (should-not (niwa-session-publication-diagnostic-backlog-p session))))

  :doc "appends collected while a program runs wait for the next batch"
  (let ((session (niwa-session--create :authority-mode 'portable))
        (pending nil)
        (programs nil))
    (cl-letf (((symbol-function 'niwa-session-recovery-refresh) #'ignore)
              ((symbol-function 'niwa-session-durability-lease-renew)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-durability-lease-owned-p)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-session-control-fs-physical-path)
               #'identity)
              ((symbol-function 'niwa-session-durability-run-owned-program-async)
               (lambda (_s operations callback)
                 (push (mapcar (lambda (op) (plist-get op :content)) operations)
                       programs)
                 (setq pending (lambda ()
                                 (funcall callback
                                          (mapcar (lambda (_) '(:status ok))
                                                  operations)
                                          nil)))))
              ((symbol-function 'niwa-session-durability-call-with-held-lease)
               (lambda (_s fn) (funcall fn))))
      (dolist (content '("first" "second"))
        (niwa-session-publication-call-with-diagnostic-batch
         session
         (lambda ()
           (niwa-session-publication-append-diagnostic
            session "/x/log" content))))
      ;; Only one program is ever in flight; the second append waits.
      (should (equal '(("first")) programs))
      (funcall pending)
      (should (niwa-session-publication-diagnostic-backlog-p session))
      (niwa-session-publication-call-with-diagnostic-batch session #'ignore)
      (should (equal '(("first") ("second")) (nreverse programs)))))

  :doc "an unavailable lease declines every append without re-testing"
  (let ((session (niwa-session--create :authority-mode 'portable))
        (renewals 0))
    (cl-letf (((symbol-function 'niwa-session-recovery-refresh) #'ignore)
              ((symbol-function 'niwa-session-durability-lease-renew)
               (lambda (_s) (setq renewals (1+ renewals)) nil)))
      (niwa-session-publication-call-with-diagnostic-batch
       session
       (lambda ()
         (should-not (niwa-session-publication-append-diagnostic
                      session "/x/a.log" "a"))
         (should-not (niwa-session-publication-append-diagnostic
                      session "/x/b.log" "b"))))
      (should (= 1 renewals))))

  :doc "one failing append is retained without holding back the others"
  (let ((session (niwa-session--create :authority-mode 'portable))
        (written nil))
    (cl-letf (((symbol-function 'niwa-session-recovery-refresh) #'ignore)
              ((symbol-function 'niwa-session-durability-lease-renew)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-durability-lease-owned-p)
               (lambda (_s) t))
              ((symbol-function 'niwa-session-publication--artifact-for-session)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-session-control-fs-physical-path)
               #'identity)
              ((symbol-function 'niwa-session-durability-run-owned-program-async)
               (lambda (_s operations callback)
                 (funcall callback
                          (mapcar (lambda (op)
                                    (if (equal (plist-get op :path) "/x/bad.log")
                                        '(:status failed :diagnostic "refused")
                                      (push (plist-get op :content) written)
                                      '(:status ok)))
                                  operations)
                          nil)))
              ((symbol-function 'niwa-session-durability-call-with-held-lease)
               (lambda (_s fn) (funcall fn))))
      (let (warnings)
        (niwa-test--with-captured-diagnostics warnings
          (niwa-session-publication-call-with-diagnostic-batch
           session
           (lambda ()
             (should (niwa-session-publication-append-diagnostic
                      session "/x/bad.log" "bad"))
             (should (niwa-session-publication-append-diagnostic
                      session "/x/good.log" "good")))))
        (should (string-match-p "bad\\.log" warnings)))
      (should (equal '("good") written))
      (should (equal '(("/x/bad.log" . "bad"))
                     (cdr (gethash session
                                   niwa-session-publication--diagnostic-backlog))))))

  :doc "a session that does not publish through the lease holds nothing"
  (let ((session (niwa-session--create))
        (holds 0) (ran nil))
    (cl-letf (((symbol-function 'niwa-session-durability-call-with-held-lease)
               (lambda (_s fn) (setq holds (1+ holds)) (funcall fn))))
      (niwa-session-publication-call-with-diagnostic-batch
       session (lambda () (setq ran t)))
      (should ran)
      (should (= 0 holds)))))

(niwa-deftest niwa-session-publication-abandon (:quiet t)
  ,test
  (test)
  :doc "retries interrupted specialized abandonment without accepting lost recovery"
  (dolist (scenario '(marker-delete payload-delete intent-write missing-without-approval))
    (let* ((root (make-temp-file "niwa-abandon-retry-" t))
           (repair (make-temp-file "niwa-abandon-source-" t))
           (workspace (niwa-workspace--create
                       :type 'project :id root :root root :name "abandon"))
           (session (niwa-session-create "main" workspace))
           (delete-marker (symbol-function 'niwa-session-control-fs-delete-file))
           (delete-payload (symbol-function 'niwa-session-control-fs-delete-directory))
           (write-marker (symbol-function 'niwa-session-durability--write-plist))
           (confirmations 0)
           marker payload)
      (setf (niwa-session-save-path session) root
            (niwa-session-session-id session) "abandon-retry")
      (unwind-protect
          (progn
            (should (niwa-session-durability-lease-acquire root "abandon" session))
            (write-region "repair bytes" nil (file-name-concat repair "before.el")
                          nil 'silent)
            (niwa-session-recovery-record-failure session "incomplete rollback" repair)
            (ert-info ((format "Recovery setup: %S"
                               (niwa-session-pending-publication session)))
              (should (plist-get (niwa-session-pending-publication session)
                                 :recovery-portable)))
            (setq marker (plist-get (niwa-session-pending-publication session)
                                    :manual-recovery-marker)
                  payload (plist-get (niwa-session-pending-publication session)
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
                                (should-error (niwa-session-publication-abandon session)))))
                      (should (= confirmations 0)))
                  (cl-letf (((symbol-function 'niwa-session-control-fs-delete-file)
                             (lambda (path)
                               (if (and (eq scenario 'marker-delete) (equal path marker))
                                   (error "Injected abandonment failure")
                                 (funcall delete-marker path))))
                            ((symbol-function 'niwa-session-control-fs-delete-directory)
                             (lambda (path)
                               (if (and (eq scenario 'payload-delete) (equal path payload))
                                   (error "Injected abandonment failure")
                                 (funcall delete-payload path))))
                            ((symbol-function 'niwa-session-durability--write-plist)
                             (lambda (path data)
                               (if (and (eq scenario 'intent-write) (equal path marker))
                                   (error "Injected abandonment failure")
                                 (funcall write-marker path data)))))
                    (should (equal (format "niwa [%s]: Injected abandonment failure" root)
                                   (error-message-string
                                    (should-error
                                     (niwa-session-publication-abandon session))))))
                  (should (file-exists-p marker))
                  (if (eq scenario 'marker-delete)
                      (should-not (file-exists-p payload))
                    (should (equal "repair bytes"
                                   (niwa-session-artifacts-read-file-raw
                                    (file-name-concat payload "before.el")))))
                  ;; Reconstruct the pending state from disk, as after client loss.
                  (setf (niwa-session-pending-publication session) nil)
                  (niwa-session-recovery-refresh session)
                  (should (niwa-session-pending-publication session))
                  (with-temp-buffer
                    (setq-local niwa--session session)
                    (should-error
                     (niwa-session-artifacts-assert-mutation-authority
                      session (current-buffer))
                     :type 'user-error))
                  (should (= confirmations 1))
                  (should (niwa-session-publication-abandon session))
                  (should (= confirmations 2))
                  (should-not (file-exists-p marker))
                  (should-not (file-exists-p payload))
                  (should-not (niwa-session-recovery-read root))
                  (should-not (niwa-session-pending-publication session))))))
        (niwa-session-durability--cancel-renewal session)
        (when (file-directory-p repair) (delete-directory repair t))
        (delete-directory root t)
        (niwa-session-durability-forget-removed-session session)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-session-publication--valid-published-path-p ()
  (let ((prefix ".publications/generation-0123456789abcdef0123/")
        (remote (symbol-function 'file-remote-p))
        (calls 0))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (&rest args) (cl-incf calls) (apply remote args))))
      (dolist (leaf '("manifest.el" "artifact-1" "a b" ".hidden" "a..b" "~name"))
        (should (niwa-session-publication--valid-published-path-p
                 (concat prefix leaf))))
      (dolist (leaf '("" "." ".." "a/b" "/a" "a/" "../a"))
        (should-not (niwa-session-publication--valid-published-path-p
                     (concat prefix leaf))))
      (dolist (path '(nil 1 "/ssh:host:/file" "../manifest.el" "~/.publications/x"
                          ".publications/generation-short/manifest.el"))
        (should-not (niwa-session-publication--valid-published-path-p path))))
    ;; The fixed relative grammar itself proves this is not a remote spelling.
    (should (= 0 calls))))

(provide 'test-niwa-session-publication)
;;; test-niwa-session-publication.el ends here
