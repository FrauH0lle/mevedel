;;; test-mevedel-migrate-artifacts.el --- Artifact store migration tests -*- lexical-binding: t -*-

;;; Commentary:
;; Run unified in-place migration on PID-lock and portable legacy sessions,
;; then read the results through the store and the session codec.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-artifact-store)
(require 'mevedel-shared-editing)
;; Name the source: stale bytecode beside the script would otherwise shadow the
;; script under test.
(require 'mevedel-migrate-session
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           ".." "scripts" "migrate-sessions.el"))

(defun mevedel-migrate-artifacts-test--legacy-sidecar (id version authority)
  "Return a legacy VERSION sidecar for session ID with AUTHORITY."
  (let ((data (test-mevedel-session-persistence--complete-sidecar
               (list :version version :session-id id :session-name (concat "Chat " id)
                     :authority-mode authority
                     :workspace (list :type (if (eq authority 'portable) 'project 'file)
                                      :workspace-id (make-string 64 ?a)
                                      :target-native-root "/tmp/" :name "w")))))
    (cl-remf data :attached-artifacts)
    (cl-remf data :dedicated-artifact)
    (cl-remf data :naming-input)
    (cl-remf data :chapters)
    data))

(defconst mevedel-migrate-artifacts-test--board
  (list :format 1 :id "board-1" :revision 2 :kind "whiteboard" :title "Plan"
        :crdt "AAA=" :comments [] :receipts '(:one 1) :transactions [])
  "A legacy whiteboard state.")

(defun mevedel-migrate-artifacts-test--comments (name)
  "Return a legacy comment store for artifact NAME."
  (mevedel-shared-editing--json
   (list :artifact name
         :comments (vector (list :id "0123456789abcdef0123" :actor "Alice" :text "Bigger"
                                 :anchor '(:kind "word" :selector "p" :label "p")
                                 :resolved :json-false :replies [])))))

(defun mevedel-migrate-artifacts-test--pid-session (root id)
  "Create closed legacy PID-lock session ID with artifacts below ROOT."
  (let ((directory (file-name-concat root ".mevedel" "sessions" id)))
    (make-directory (file-name-concat directory "artifacts" "shared-editing" "artifact-comments") t)
    (mevedel-migrate-session--write
     (file-name-concat directory "session.meta.el")
     (mevedel-migrate-artifacts-test--legacy-sidecar id "v0.5.10" 'pid-lock))
    (write-region "<p>mockup</p>" nil (file-name-concat directory "artifacts" "mockup.html")
                  nil 'silent)
    (write-region (mevedel-migrate-artifacts-test--comments "mockup.html") nil
                  (file-name-concat directory "artifacts" "shared-editing" "artifact-comments"
                                    "abc.json")
                  nil 'silent)
    (write-region (mevedel-shared-editing--json mevedel-migrate-artifacts-test--board) nil
                  (file-name-concat directory "artifacts" "shared-editing" "board-1.json")
                  nil 'silent)
    directory))

(defun mevedel-migrate-artifacts-test--portable-session (root id)
  "Create closed legacy portable session ID publishing one artifact below ROOT."
  (let* ((directory (file-name-concat root ".mevedel" "sessions" id))
         (generation ".publications/generation-bbbbbbbbbbbbbbbbbbbb")
         (sidecar (concat generation "/000001.data"))
         (note (concat generation "/000002.data"))
         (comments (concat generation "/000003.data"))
         (head (concat generation "/manifest.el")))
    (make-directory (file-name-concat directory generation) t)
    (make-directory (file-name-concat directory ".lease") t)
    (mevedel-migrate-session--write
     (file-name-concat directory sidecar)
     (mevedel-migrate-artifacts-test--legacy-sidecar id "v0.5.10" 'portable))
    (write-region "# Notes" nil (file-name-concat directory note) nil 'silent)
    (write-region (mevedel-migrate-artifacts-test--comments "notes.md") nil
                  (file-name-concat directory comments) nil 'silent)
    ;; Even a malformed fixed sidecar is no authority for portable comments.
    (write-region "stale, unreadable cache" nil
                  (file-name-concat directory "session.meta.el") nil 'silent)
    ;; A stale fixed cache is no authority.
    (make-directory (file-name-concat directory "artifacts") t)
    (write-region "stale" nil (file-name-concat directory "artifacts" "notes.md") nil 'silent)
    (mevedel-migrate-session--write
     (file-name-concat directory head)
     (list :sidecar "session.meta.el"
           :artifacts (list (list "session.meta.el" :published sidecar
                                  :sha256 (mevedel-migrate-session--hash
                                           (file-name-concat directory sidecar)))
                            (list "artifacts/notes.md" :published note
                                  :sha256 (mevedel-migrate-session--hash
                                           (file-name-concat directory note)))
                            (list "artifacts/shared-editing/artifact-comments/notes.json"
                                  :published comments
                                  :sha256 (mevedel-migrate-session--hash
                                           (file-name-concat directory comments))))))
    (mevedel-migrate-session--write
     (file-name-concat directory ".lease" "00000000000000000001.el")
     (list :generation 1 :transfer-generation 1 :status 'released
           :publication-head head :unsettled-mutation nil
           :client-id (make-string 64 ?a) :renewed-at 1 :expires-at 2))
    directory))

(defun mevedel-migrate-artifacts-test--snapshot (directory)
  "Return DIRECTORY's paths, exact bytes and write-relevant file attributes.
Ignore access time, which reads by migration or this snapshot can change."
  (mapcar (lambda (file)
            (if (file-directory-p file)
                (list (file-relative-name file directory) 'directory)
              (list (file-relative-name file directory)
                    (mevedel-migrate-session--hash file)
                    (let ((attributes (file-attributes file)))
                      (setf (nth 4 attributes) nil)
                      attributes))))
          (directory-files-recursively directory "." t)))

(defmacro mevedel-migrate-artifacts-test--with-interrupted-install (setup &rest body)
  "Import a real session, interrupt installation, then run BODY.
Run SETUP before importing, with `root', `workspace', `store' and
`directory' bound to a legacy session containing a file and whiteboard."
  (declare (indent 1) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-install-retry-" t)))
          (workspace (mevedel-workspace--create :type 'project :id "w" :root root))
          (store (mevedel-artifact-store-directory workspace))
          (directory (mevedel-migrate-artifacts-test--pid-session root "s1"))
          install-interrupted)
     (unwind-protect
         (progn
           ,setup
           (let ((before (mevedel-migrate-artifacts-test--snapshot directory)))
             (cl-letf (((symbol-function 'mevedel-migrate-session--install)
                        (lambda (&rest _)
                          (setq install-interrupted t)
                          (error "Interrupted install after successful artifact imports"))))
               (let ((row (cdar (mevedel-migrate-sessions root))))
                 (should install-interrupted)
                 (should (eq 'failed (plist-get row :status)))
                 (should (string-search "Interrupted install after successful artifact imports"
                                        (plist-get row :reason)))))
             (should (equal before (mevedel-migrate-artifacts-test--snapshot directory))))
           ;; Prove the interruption came after both real imports completed.
           (dolist (id '("board-1" "mockup"))
             (should (member id (mevedel-artifact-store-ids workspace)))
             (should (= 1 (length (mevedel-artifact-store-versions workspace id)))))
           ,@body)
       (delete-directory root t))))

(mevedel-deftest mevedel-migrate-sessions/artifacts ()
  ,test
  (test)
  :doc "moves every session's artifacts into the store and attaches the sessions"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-artifacts-" t)))
         (sessions (file-name-concat root ".mevedel" "sessions"))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root))
         (store (mevedel-artifact-store-directory workspace)))
    (unwind-protect
        (progn
          (let ((folder (file-name-concat
                         (mevedel-migrate-artifacts-test--pid-session root "s1") "artifacts")))
            ;; Hidden files and directories and backups carry no artifact.
            (write-region "old" nil (file-name-concat folder "mockup.html~") nil 'silent)
            (write-region "x" nil (file-name-concat folder ".hidden.html") nil 'silent)
            (make-directory (file-name-concat folder ".cache") t)
            (write-region "x" nil (file-name-concat folder ".cache" "page.html") nil 'silent))
          (mevedel-migrate-artifacts-test--portable-session root "s2")
          (let ((report (mevedel-migrate-sessions root)))
            (should (cl-every (lambda (row) (eq 'migrated (plist-get (cdr row) :status))) report))
            (should (equal '(("s1" "board-1" "mockup") ("s2" "notes"))
                           (mapcar (lambda (row)
                                     (cons (car row)
                                           (sort (copy-sequence (plist-get (cdr row) :artifacts))
                                                 #'string<)))
                                   report)))
            ;; The file artifact; legacy comments are not carried over.
            (should (equal "<p>mockup</p>"
                           (with-temp-buffer
                             (insert-file-contents (file-name-concat store "mockup" "mockup.html"))
                             (buffer-string))))
            (dolist (id '("mockup" "notes"))
              (dolist (name '("comments.json" "discussions.json"))
                (should-not (file-exists-p
                             (file-name-concat (mevedel-artifact-store-bookkeeping-directory
                                                workspace id)
                                               name)))))
            (should (= 1 (length (mevedel-artifact-store-versions workspace "mockup"))))
            ;; The whiteboard keeps its identity and gets a version.
            (should (equal '(:kind whiteboard :title "Plan" :file "state.json")
                           (cl-subseq (mevedel-artifact-store-meta workspace "board-1") 0 6)))
            (should (equal "Plan" (plist-get (mevedel-shared-editing--read workspace "board-1")
                                             :title)))
            (should (= 1 (length (mevedel-artifact-store-versions workspace "board-1"))))
            ;; The portable artifact comes from its publication, not the cache.
            (should (equal "# Notes" (with-temp-buffer
                                       (insert-file-contents
                                        (file-name-concat store "notes" "notes.md"))
                                       (buffer-string))))
            ;; The installed sessions are current and attached.
            (let ((converted (mevedel-migrate-session--read
                              (file-name-concat sessions "s1" "session.meta.el"))))
              (should (equal mevedel-session-codec-format-version (plist-get converted :version)))
              (should (equal '("board-1" "mockup")
                             (sort (copy-sequence (plist-get converted :attached-artifacts))
                                   #'string<)))
              (mevedel-session-codec-validate-current-sidecar converted))
            ;; Installed sessions drop moved entries, while backups keep them.
            (should-not (file-exists-p (file-name-concat sessions "s1" "artifacts")))
            (should-not (file-exists-p (file-name-concat sessions "s2" "artifacts")))
            (let ((manifest (mevedel-migrate-session--read
                             (file-name-concat sessions "s2" ".publications"
                                               "generation-bbbbbbbbbbbbbbbbbbbb" "manifest.el"))))
              (should (equal '("session.meta.el")
                             (mapcar #'car (plist-get manifest :artifacts)))))
            (let* ((publication (mevedel-session-publication-read
                                 (file-name-concat sessions "s2")))
                   (data (mevedel-session-codec-read (plist-get publication :sidecar))))
              (should (equal '("notes") (plist-get data :attached-artifacts)))
              (mevedel-session-codec-validate-current-sidecar data))
            (let ((backup (plist-get (cdr (assoc "s1" report)) :backup)))
              (should (file-directory-p backup))
              (should (equal "v0.5.10" (plist-get (mevedel-migrate-session--read
                                                   (file-name-concat backup "session.meta.el"))
                                                  :version)))
              (should (file-exists-p (file-name-concat backup "artifacts" "mockup.html"))))
            (let ((backup (plist-get (cdr (assoc "s2" report)) :backup)))
              (should (file-exists-p (file-name-concat backup "artifacts" "notes.md"))))
            ;; Current sessions do not rewrite payloads, metadata or backups.
            (let ((before (mevedel-migrate-artifacts-test--snapshot root)))
              (should (equal '(current current)
                             (mapcar (lambda (row) (plist-get (cdr row) :status))
                                     (mevedel-migrate-sessions root))))
              (should (equal before (mevedel-migrate-artifacts-test--snapshot root))))
            (should (equal '("board-1" "mockup" "notes") (mevedel-artifact-store-ids workspace)))))
      (delete-directory root t)))

  :doc "shares a forked item that did not diverge and separates one that did"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-fork-" t)))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root)))
    (unwind-protect
        (progn
          (mevedel-migrate-artifacts-test--pid-session root "s1")
          (mevedel-migrate-artifacts-test--pid-session root "s2")
          (let ((diverged (file-name-concat root ".mevedel" "sessions" "s3")))
            (mevedel-migrate-artifacts-test--pid-session root "s3")
            (write-region (mevedel-shared-editing--json
                           (plist-put (copy-sequence mevedel-migrate-artifacts-test--board)
                                      :title "Forked"))
                          nil (file-name-concat diverged "artifacts" "shared-editing"
                                                "board-1.json")
                          nil 'silent))
          (let ((report (mevedel-migrate-sessions root)))
            (should (cl-every (lambda (row) (eq 'migrated (plist-get (cdr row) :status))) report))
            (should (member "board-1" (plist-get (cdr (assoc "s1" report)) :artifacts)))
            (should (member "board-1" (plist-get (cdr (assoc "s2" report)) :artifacts)))
            ;; A forked file with the same bytes is the same artifact too.
            (should (equal '("mockup") (cl-remove-if-not (lambda (id) (string-prefix-p "mockup" id))
                                                         (mevedel-artifact-store-ids workspace))))
            (should (member "mockup" (plist-get (cdr (assoc "s3" report)) :artifacts)))
            (should (member "board-1-2" (plist-get (cdr (assoc "s3" report)) :artifacts)))
            (should (equal "Forked" (plist-get (mevedel-shared-editing--read workspace "board-1-2")
                                               :title)))
            (should (equal "board-1-2" (plist-get (mevedel-shared-editing--read workspace "board-1-2")
                                                  :id)))
            ;; Restore each original to simulate a retry after a partial run.
            (dolist (row report)
              (let ((directory (file-name-concat root ".mevedel" "sessions" (car row))))
                (delete-directory directory t)
                (copy-directory (plist-get (cdr row) :backup) directory t t t)))
            (let ((retried (mevedel-migrate-sessions root)))
              (should (cl-every (lambda (row) (eq 'migrated (plist-get (cdr row) :status))) retried))
              (dolist (row report)
                (should (equal (plist-get (cdr row) :artifacts)
                               (plist-get (cdr (assoc (car row) retried)) :artifacts)))))
            ;; Unchanged retry must reuse the divergent fork's remapped id too.
            (should (= 1 (length (mevedel-artifact-store-versions workspace "board-1-2"))))
            ;; A later store edit must not be overwritten or mistaken for the
            ;; original legacy state on another retry.
            (let ((state (mevedel-shared-editing--read workspace "board-1")))
              (mevedel-migrate-artifacts--write
               (file-name-concat (mevedel-artifact-store-bookkeeping-directory workspace "board-1")
                                 "state.json")
               (mevedel-shared-editing--json (plist-put state :title "Edited after migration"))))
            (dolist (row report)
              (let ((directory (file-name-concat root ".mevedel" "sessions" (car row))))
                (delete-directory directory t)
                (copy-directory (plist-get (cdr row) :backup) directory t t t)))
            (let* ((s1 (file-name-concat root ".mevedel" "sessions" "s1"))
                   (s2 (file-name-concat root ".mevedel" "sessions" "s2"))
                   (s1-before (mevedel-migrate-artifacts-test--snapshot s1))
                   (s2-before (mevedel-migrate-artifacts-test--snapshot s2))
                   (store (mevedel-artifact-store-directory workspace))
                   (store-before (mevedel-migrate-artifacts-test--snapshot store))
                   (retried (mevedel-migrate-sessions root)))
              (dolist (id '("s1" "s2"))
                (let ((row (cdr (assoc id retried))))
                  (should (eq 'failed (plist-get row :status)))
                  (should (string-search "differs" (plist-get row :reason)))))
              (should (eq 'migrated (plist-get (cdr (assoc "s3" retried)) :status)))
              (should (equal s1-before (mevedel-migrate-artifacts-test--snapshot s1)))
              (should (equal s2-before (mevedel-migrate-artifacts-test--snapshot s2)))
              (should (equal store-before (mevedel-migrate-artifacts-test--snapshot store))))
            (should (equal "Edited after migration"
                           (plist-get (mevedel-shared-editing--read workspace "board-1") :title)))
            (should (equal '("board-1" "board-1-2" "mockup")
                           (mevedel-artifact-store-ids workspace)))))
      (delete-directory root t)))

  :doc "leaves an unpublished closed session untouched, and converts the rest"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-empty-" t)))
         (empty (file-name-concat root ".mevedel" "sessions" "s0")))
    (unwind-protect
        (progn
          (mevedel-migrate-artifacts-test--pid-session root "s1")
          (make-directory (file-name-concat empty ".lease") t)
          (mevedel-migrate-session--write
           (file-name-concat empty ".lease" "00000000000000000001.el")
           (list :generation 1 :transfer-generation 1 :status 'released
                 :publication-head nil :unsettled-mutation nil
                 :client-id (make-string 64 ?a) :renewed-at 1 :expires-at 2))
          (let* ((before (mevedel-migrate-artifacts-test--snapshot empty))
                 (report (mevedel-migrate-sessions root)))
            (should (eq 'empty (plist-get (cdr (assoc "s0" report)) :status)))
            (should-not (plist-get (cdr (assoc "s0" report)) :backup))
            (should (equal before (mevedel-migrate-artifacts-test--snapshot empty)))
            (should (member "mockup" (plist-get (cdr (assoc "s1" report)) :artifacts)))))
      (delete-directory root t)))

  :doc "preserves files whose names are reserved by store bookkeeping"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-names-" t)))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root)))
    (unwind-protect
        (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s1")))
          (dolist (name '("meta.el" "versions" "comments.json" "state.json"))
            (write-region (concat "Payload for " name) nil
                          (file-name-concat directory "artifacts" name) nil 'silent))
          (let ((report (mevedel-migrate-sessions root)))
            (should (eq 'migrated (plist-get (cdr (assoc "s1" report)) :status)))
            (dolist (name '("meta.el" "versions" "comments.json" "state.json"))
              (let* ((id (mevedel-migrate-artifacts--existing
                          workspace (cons "s1" (concat "artifacts/" name))))
                     (file (plist-get (mevedel-artifact-store-meta workspace id) :file)))
                (should (equal name file))
                (should (equal (concat "Payload for " name)
                               (with-temp-buffer
                                 (insert-file-contents
                                  (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                                                    file))
                                 (buffer-string))))))))
      (delete-directory root t)))

  :doc "leaves a failing session unchanged, reports it, and converts later sessions"
  (let ((root (file-name-as-directory (make-temp-file "mevedel-migrate-corrupt-" t))))
    (unwind-protect
        (progn
          (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s0")))
            (write-region "broken JSON" nil
                          (file-name-concat directory "artifacts" "shared-editing" "board-1.json")
                          nil 'silent))
          (mevedel-migrate-artifacts-test--pid-session root "s1")
          (let* ((directory (file-name-concat root ".mevedel" "sessions" "s0"))
                 (before (mevedel-migrate-artifacts-test--snapshot directory))
                 (report (mevedel-migrate-sessions root)))
            (should (eq 'failed (plist-get (cdr (assoc "s0" report)) :status)))
            (should (stringp (plist-get (cdr (assoc "s0" report)) :reason)))
            (should (equal before (mevedel-migrate-artifacts-test--snapshot directory)))
            (should (member "mockup" (plist-get (cdr (assoc "s1" report)) :artifacts)))))
      (delete-directory root t)))

  :doc "refuses legacy artifacts in a store-aware session instead of dropping its attachments"
  (let ((root (file-name-as-directory (make-temp-file "mevedel-migrate-aware-" t))))
    (unwind-protect
        (let* ((directory (mevedel-migrate-artifacts-test--pid-session root "s1"))
               (sidecar (file-name-concat directory "session.meta.el"))
               (data (test-mevedel-session-persistence--complete-sidecar
                      (list :version "v0.5.11" :session-id "s1" :session-name "Chat s1"
                            :authority-mode 'pid-lock :attached-artifacts '("kept")
                            :workspace (list :type 'file :workspace-id (make-string 64 ?a)
                                             :target-native-root "/tmp/" :name "w")))))
          (cl-remf data :naming-input)
          (cl-remf data :chapters)
          (mevedel-migrate-session--write sidecar data)
          (let* ((before (mevedel-migrate-artifacts-test--snapshot directory))
                 (report (mevedel-migrate-sessions root)))
            (should (eq 'failed (plist-get (cdr (assoc "s1" report)) :status)))
            (should (equal before (mevedel-migrate-artifacts-test--snapshot directory)))
            (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts")))))
      (delete-directory root t)))

  :doc "fails a session holding a link, without copying what it points to"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-link-" t)))
         (outside (file-name-concat root "secret.html")))
    (unwind-protect
        (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s1")))
          (write-region "secret" nil outside nil 'silent)
          (make-symbolic-link outside (file-name-concat directory "artifacts" "host.html"))
          (let ((report (mevedel-migrate-sessions root)))
            (should (eq 'failed (plist-get (cdr (assoc "s1" report)) :status)))
            (should (string-match-p "link" (plist-get (cdr (assoc "s1" report)) :reason))))
          (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts" "host"))))
      (delete-directory root t)))

  :doc "refuses before writing anything while a session is open"
  (let ((root (file-name-as-directory (make-temp-file "mevedel-migrate-open-" t))))
    (unwind-protect
        (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s1")))
          (write-region "(:pid 1)" nil (file-name-concat directory ".lock") nil 'silent)
          (let ((before (mevedel-migrate-artifacts-test--snapshot root)))
            (should-error (mevedel-migrate-sessions root))
            (should (equal before (mevedel-migrate-artifacts-test--snapshot root))))
          (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-migrate-sessions/artifact-boundaries ()
  ,test
  (test)
  :doc "a session-directory argument migrates only that session into its project's store"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-one-" t)))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root)))
    (unwind-protect
        (let* ((source (mevedel-migrate-artifacts-test--pid-session root "s1"))
               (untouched (mevedel-migrate-artifacts-test--portable-session root "s2"))
               (before (mevedel-migrate-artifacts-test--snapshot untouched))
               (report (mevedel-migrate-sessions source))
               (row (cdr (assoc "s1" report))))
          (should (= 1 (length report)))
          (should (eq 'migrated (plist-get row :status)))
          (should (equal '("board-1" "mockup")
                         (sort (copy-sequence (plist-get row :artifacts)) #'string<)))
          (should (equal '("board-1" "mockup") (mevedel-artifact-store-ids workspace)))
          (should (equal before (mevedel-migrate-artifacts-test--snapshot untouched)))
          (should (equal "v0.5.10"
                         (plist-get (mevedel-migrate-session--read
                                     (file-name-concat (plist-get row :backup) "session.meta.el"))
                                    :version))))
      (delete-directory root t)))

  :doc "a single artifact session refuses a live unselected sibling before store writes"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-live-sibling-" t)))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root))
         (store (mevedel-artifact-store-directory workspace)))
    (unwind-protect
        (let* ((source (mevedel-migrate-artifacts-test--pid-session root "selected"))
               (sibling (mevedel-migrate-artifacts-test--pid-session root "unselected"))
               (import (symbol-function 'mevedel-migrate-artifacts--import))
               import-attempted)
          ;; A premature import could mutate an existing item's origins even
          ;; without leaving a new artifact behind.
          (mevedel-migrate-artifacts--move-file
           workspace "seed" "artifacts/mockup.html" "<p>mockup</p>")
          (write-region "(:pid 1)" nil (file-name-concat sibling ".lock") nil 'silent)
          (let ((source-before (mevedel-migrate-artifacts-test--snapshot source))
                (sibling-before (mevedel-migrate-artifacts-test--snapshot sibling))
                (store-before (mevedel-migrate-artifacts-test--snapshot store))
                result)
            (cl-letf (((symbol-function 'mevedel-migrate-artifacts--import)
                       (lambda (&rest args)
                         (setq import-attempted t)
                         (apply import args))))
              (setq result
                    (condition-case err
                        (list :report (mevedel-migrate-sessions source))
                      (error (list :error (error-message-string err))))))
            (let ((reason (if (plist-member result :error)
                              (plist-get result :error)
                            (let ((row (cdr (assoc "selected" (plist-get result :report)))))
                              (should (eq 'failed (plist-get row :status)))
                              (plist-get row :reason)))))
              (should (stringp reason))
              (should (string-search "lock" (downcase reason))))
            (should-not import-attempted)
            (should (equal source-before (mevedel-migrate-artifacts-test--snapshot source)))
            (should (equal sibling-before (mevedel-migrate-artifacts-test--snapshot sibling)))
            (should (equal store-before (mevedel-migrate-artifacts-test--snapshot store)))))
      (delete-directory root t)))

  :doc "unsupported metadata is refused before any artifact is imported"
  (dolist (version '("v0.5.5" "v0.5.7" "v0.5.8" "v99.0.0"))
    (let ((root (file-name-as-directory (make-temp-file "mevedel-migrate-unsupported-" t))))
      (unwind-protect
          (let* ((directory (mevedel-migrate-artifacts-test--pid-session root "s1"))
                 (sidecar (file-name-concat directory "session.meta.el")))
            (mevedel-migrate-session--write
             sidecar (plist-put (mevedel-migrate-session--read sidecar) :version version))
            (let* ((before (mevedel-migrate-artifacts-test--snapshot directory))
                   (report (mevedel-migrate-sessions root)))
              (should (eq 'failed (plist-get (cdr (assoc "s1" report)) :status)))
              (should (equal before (mevedel-migrate-artifacts-test--snapshot directory)))
              (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts")))))
        (delete-directory root t))))

  :doc "detached sessions with legacy artifacts fail without guessing a workspace"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-detached-" t)))
         (detached (file-name-concat root "detached")))
    (unwind-protect
        (progn
          (rename-file (mevedel-migrate-artifacts-test--pid-session root "s1") detached)
          (let* ((before (mevedel-migrate-artifacts-test--snapshot detached))
                 (report (mevedel-migrate-sessions detached))
                 (row (cdar report)))
            (should (eq 'failed (plist-get row :status)))
            (should (stringp (plist-get row :reason)))
            (should (equal before (mevedel-migrate-artifacts-test--snapshot detached)))
            (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts")))))
      (delete-directory root t)))

  :doc "unrecognized reserved shared-editing entries fail instead of being silently discarded"
  (dolist (relative '("artifacts/shared-editing/notes.txt"
                      "artifacts/shared-editing/nested/board.json"))
    (let ((root (file-name-as-directory (make-temp-file "mevedel-migrate-unknown-item-" t))))
      (unwind-protect
          (let* ((directory (mevedel-migrate-artifacts-test--pid-session root "s1"))
                 (unknown (file-name-concat directory relative)))
            (make-directory (file-name-directory unknown) t)
            (write-region (if (string-suffix-p ".json" relative)
                              (mevedel-shared-editing--json mevedel-migrate-artifacts-test--board)
                            "Legacy bytes that must not be discarded")
                          nil unknown nil 'silent)
            (let* ((before (mevedel-migrate-artifacts-test--snapshot directory))
                   (siblings (directory-files (file-name-directory directory) t))
                   (row (cdar (mevedel-migrate-sessions root))))
              (should (eq 'failed (plist-get row :status)))
              (should (string-match-p "unrecognized\\|unknown\\|unsupported"
                                      (downcase (plist-get row :reason))))
              (should-not (plist-get row :backup))
              (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts")))
              (should (equal before (mevedel-migrate-artifacts-test--snapshot directory)))
              (should (equal siblings (directory-files (file-name-directory directory) t)))))
        (delete-directory root t))))

  :doc "failed artifact imports preserve the session and a retry starts with one version"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-import-retry-" t)))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root)))
    (unwind-protect
        (let* ((directory (mevedel-migrate-artifacts-test--pid-session root "s1"))
               (before (mevedel-migrate-artifacts-test--snapshot directory))
               injected)
          (cl-letf (((symbol-function 'mevedel-artifact-store-record-version)
                     (lambda (&rest _)
                       (setq injected t)
                       (error "Interrupted artifact version write"))))
            (let ((row (cdar (mevedel-migrate-sessions root))))
              (should injected)
              (should (eq 'failed (plist-get row :status)))
              (should (string-search "Interrupted artifact version write" (plist-get row :reason)))))
          (should (equal before (mevedel-migrate-artifacts-test--snapshot directory)))
          (should-not (mevedel-artifact-store-ids workspace))
          (let ((row (cdar (mevedel-migrate-sessions root))))
            (should (eq 'migrated (plist-get row :status)))
            (should (equal '("board-1" "mockup")
                           (sort (copy-sequence (plist-get row :artifacts)) #'string<))))
          (dolist (id '("board-1" "mockup"))
            (should (= 1 (length (mevedel-artifact-store-versions workspace id))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-migrate-sessions/artifact-diverged-retry ()
  ,test
  (test)
  :doc "retry after successful import refuses newer legacy file bytes without dropping them"
  (mevedel-migrate-artifacts-test--with-interrupted-install nil
    (write-region "<p>Edited legacy file after interrupted installation</p>" nil
                  (file-name-concat directory "artifacts" "mockup.html") nil 'silent)
    (let* ((source-before (mevedel-migrate-artifacts-test--snapshot directory))
           (store-before (mevedel-migrate-artifacts-test--snapshot store))
           (row (cdar (mevedel-migrate-sessions root))))
      (should (eq 'failed (plist-get row :status)))
      (should (string-match-p "chang\\|diverg\\|differ\\|mismatch"
                              (downcase (plist-get row :reason))))
      (should (equal source-before (mevedel-migrate-artifacts-test--snapshot directory)))
      (should (equal store-before (mevedel-migrate-artifacts-test--snapshot store)))
      (should (equal "v0.5.10" (plist-get (mevedel-migrate-session--read
                                          (file-name-concat directory "session.meta.el")) :version)))))

  :doc "retry refuses newer legacy board and document titles or state without dropping them"
  (dolist (kind '("whiteboard" "document"))
    (dolist (field '(:title :crdt))
      (mevedel-migrate-artifacts-test--with-interrupted-install
          (let ((state (plist-put (copy-tree mevedel-migrate-artifacts-test--board) :kind kind)))
            (write-region (mevedel-shared-editing--json state) nil
                          (file-name-concat directory "artifacts" "shared-editing" "board-1.json")
                          nil 'silent))
        (let* ((legacy (file-name-concat directory "artifacts" "shared-editing" "board-1.json"))
               (state (mevedel-migrate-artifacts--json (mevedel-migrate-artifacts--bytes legacy))))
          (setq state (plist-put state field (if (eq field :title) "Edited legacy title" "QkJC")))
          (write-region (mevedel-shared-editing--json state) nil legacy nil 'silent))
        (let* ((source-before (mevedel-migrate-artifacts-test--snapshot directory))
               (store-before (mevedel-migrate-artifacts-test--snapshot store))
               (row (cdar (mevedel-migrate-sessions root))))
          (should (eq 'failed (plist-get row :status)))
          (should (string-match-p "chang\\|diverg\\|differ\\|mismatch"
                                  (downcase (plist-get row :reason))))
          (should (equal source-before (mevedel-migrate-artifacts-test--snapshot directory)))
          (should (equal store-before (mevedel-migrate-artifacts-test--snapshot store)))
          (should (equal "v0.5.10" (plist-get (mevedel-migrate-session--read
                                              (file-name-concat directory "session.meta.el"))
                                             :version)))))))

  :doc "retry refuses a missing prior file or item payload while retaining its legacy source"
  (dolist (kind '(file whiteboard document))
    (mevedel-migrate-artifacts-test--with-interrupted-install
        (unless (eq kind 'file)
          (let ((state (plist-put (copy-tree mevedel-migrate-artifacts-test--board)
                                  :kind (symbol-name kind))))
            (write-region (mevedel-shared-editing--json state) nil
                          (file-name-concat directory "artifacts" "shared-editing" "board-1.json")
                          nil 'silent)))
      (delete-file
       (if (eq kind 'file)
           (mevedel-artifact-store-primary-path workspace "mockup")
         (file-name-concat (mevedel-artifact-store-bookkeeping-directory workspace "board-1")
                           "state.json")))
      (let* ((source-before (mevedel-migrate-artifacts-test--snapshot directory))
             (store-before (mevedel-migrate-artifacts-test--snapshot store))
             (row (cdar (mevedel-migrate-sessions root))))
        (should (eq 'failed (plist-get row :status)))
        (should (string-match-p "missing\\|no such file\\|incomplete\\|payload"
                                (downcase (plist-get row :reason))))
        (should (equal source-before (mevedel-migrate-artifacts-test--snapshot directory)))
        (should (equal store-before (mevedel-migrate-artifacts-test--snapshot store)))
        (should (equal "v0.5.10" (plist-get (mevedel-migrate-session--read
                                            (file-name-concat directory "session.meta.el")) :version)))))))

(mevedel-deftest mevedel-migrate-artifacts--import ()
  ,test
  (test)
  :doc "removes interrupted imports so retries keep a first version"
  (dolist (kind '(file item))
    (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-retry-" t)))
           (workspace (mevedel-workspace--create :type 'project :id "w" :root root))
           (import (lambda ()
                     (if (eq kind 'item)
                         (mevedel-migrate-artifacts--move-item
                          workspace "s1" "artifacts/shared-editing/board-1.json"
                          mevedel-migrate-artifacts-test--board)
                       (mevedel-migrate-artifacts--move-file
                        workspace "s1" "artifacts/a.html" "<p>Kept</p>")))))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mevedel-artifact-store-record-version)
                       (lambda (&rest _) (error "Interrupted version write"))))
              (should-error (funcall import)))
            (should-not (mevedel-artifact-store-ids workspace))
            (let ((id (funcall import)))
              (should (equal id (funcall import)))
              (should (= 1 (length (mevedel-artifact-store-versions workspace id))))))
        (delete-directory root t)))))

(mevedel-deftest mevedel-migrate-artifacts--fresh-id
  (:doc "preserves bookkeeping-only artifacts restored through Git")
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-hidden-" t)))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root)))
    (unwind-protect
        (progn
          (mevedel-migrate-artifacts--move-item
           workspace "s0" "artifacts/shared-editing/board-1.json"
           mevedel-migrate-artifacts-test--board)
          (delete-directory (mevedel-artifact-store-artifact-directory workspace "board-1"))
          (should (equal "board-1-2" (mevedel-migrate-artifacts--fresh-id workspace "board-1.json")))
          (let ((state (mevedel-shared-editing--read workspace "board-1")))
            (should-error
             (mevedel-migrate-artifacts--import workspace "board-1" '("s1" . "x")
                                                (lambda () (ert-fail "Overwrote existing item"))))
            (should (equal state (mevedel-shared-editing--read workspace "board-1"))))
          (should
           (equal "board-1-2"
                  (mevedel-migrate-artifacts--move-item
                   workspace "s1" "artifacts/shared-editing/board-1.json"
                   (plist-put (copy-sequence mevedel-migrate-artifacts-test--board)
                              :title "Diverged"))))
          (should (equal "Plan" (plist-get (mevedel-shared-editing--read workspace "board-1") :title))))
      (delete-directory root t))))

(provide 'test-mevedel-migrate-artifacts)
;;; test-mevedel-migrate-artifacts.el ends here
