;;; test-mevedel-migrate-artifacts.el --- Artifact store migration tests -*- lexical-binding: t -*-

;;; Commentary:
;; Run the standalone artifact migration on a PID-lock and a portable legacy
;; session, then read the result through the store and the session codec.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
;; Name the source: stale bytecode beside the script would otherwise shadow the
;; script under test.
(require 'mevedel-collaboration-artifact-comments)
(require 'mevedel-migrate-artifacts
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           ".." "scripts" "migrate-artifacts-to-store.el"))

(defun mevedel-migrate-artifacts-test--legacy-sidecar (id version authority)
  "Return a legacy VERSION sidecar for session ID with AUTHORITY."
  (let ((data (test-mevedel-session-persistence--complete-sidecar
               (list :version version :session-id id :session-name (concat "Chat " id)
                     :authority-mode authority
                     :workspace (list :type (if (eq authority 'portable) 'project 'file)
                                      :workspace-id (make-string 64 ?a)
                                      :target-native-root "/tmp/" :name "w")))))
    (cl-remf data :attached-artifacts)
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

(mevedel-deftest mevedel-migrate-artifacts ()
  ,test
  (test)
  :doc "moves every session's artifacts into the store and attaches the sessions"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-artifacts-" t)))
         (destination (file-name-concat root "converted"))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root))
         (store (mevedel-artifact-store-directory workspace)))
    (unwind-protect
        (progn
          (mevedel-migrate-artifacts-test--pid-session root "s1")
          (mevedel-migrate-artifacts-test--portable-session root "s2")
          (let ((report (mevedel-migrate-artifacts root destination)))
            (should (equal '(("s1" "board-1" "mockup") ("s2" "notes"))
                           (mapcar (lambda (row) (cons (car row) (sort (copy-sequence (cdr row))
                                                                       #'string<)))
                                   report)))
            ;; The file artifact, with its comments answering in their session.
            (should (equal "<p>mockup</p>"
                           (with-temp-buffer
                             (insert-file-contents (file-name-concat store "mockup" "mockup.html"))
                             (buffer-string))))
            (let ((thread (car (mevedel-collaboration--artifact-comments-read workspace "mockup"))))
              (should (equal "Bigger" (plist-get thread :text)))
              (should (equal "s1" (plist-get thread :session)))
              (should (equal "Chat s1" (plist-get thread :session-name))))
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
            (let ((thread (car (mevedel-collaboration--artifact-comments-read workspace "notes"))))
              (should (equal "s2" (plist-get thread :session)))
              (should (equal "Chat s2" (plist-get thread :session-name))))
            ;; Converted sessions are current and attached; originals unchanged.
            (let ((converted (mevedel-migrate-session--read
                              (file-name-concat destination "s1" "session.meta.el"))))
              (should (equal mevedel-session-codec-format-version (plist-get converted :version)))
              (should (equal '("board-1" "mockup")
                             (sort (copy-sequence (plist-get converted :attached-artifacts))
                                   #'string<)))
              (mevedel-session-codec-validate-current-sidecar converted))
            (should (equal "v0.5.10" (plist-get (mevedel-migrate-session--read
                                                 (file-name-concat root ".mevedel" "sessions"
                                                                   "s1" "session.meta.el"))
                                                :version)))
            ;; A rerun reuses what the first moved.
            (delete-directory destination t)
            (should (equal report (mevedel-migrate-artifacts root destination)))
            (should (equal '("board-1" "mockup" "notes") (mevedel-artifact-store-ids workspace)))))
      (delete-directory root t)))

  :doc "shares a forked item that did not diverge and separates one that did"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-fork-" t)))
         (destination (file-name-concat root "converted"))
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
          (let ((report (mevedel-migrate-artifacts root destination)))
            (should (member "board-1" (cdr (assoc "s1" report))))
            (should (member "board-1" (cdr (assoc "s2" report))))
            (should (member "board-1-2" (cdr (assoc "s3" report))))
            (should (equal "Forked" (plist-get (mevedel-shared-editing--read workspace "board-1-2")
                                               :title)))
            (should (equal "board-1-2" (plist-get (mevedel-shared-editing--read workspace "board-1-2")
                                                  :id)))
            ;; Retrying after editing a shared import must retain both origins.
            (let ((state (mevedel-shared-editing--read workspace "board-1")))
              (mevedel-migrate-artifacts--write
               (file-name-concat (mevedel-artifact-store-bookkeeping-directory workspace "board-1")
                                 "state.json")
               (mevedel-shared-editing--json (plist-put state :title "Edited after migration"))))
            (should (equal report (mevedel-migrate-artifacts root
                                                             (file-name-concat root "retry"))))))
      (delete-directory root t)))

  :doc "copies a session closed before it ever published, and converts the rest"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-empty-" t)))
         (destination (file-name-concat root "converted"))
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
          (let ((report (mevedel-migrate-artifacts root destination)))
            (should (eq :unconverted (cadr (assoc "s0" report))))
            (should (file-exists-p (file-name-concat destination "s0" ".lease"
                                                     "00000000000000000001.el")))
            (should (member "mockup" (cdr (assoc "s1" report))))))
      (delete-directory root t)))

  :doc "preserves files whose names are reserved by store bookkeeping"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-names-" t)))
         (workspace (mevedel-workspace--create :type 'project :id "w" :root root)))
    (unwind-protect
        (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s1")))
          (dolist (name '("meta.el" "versions" "comments.json" "state.json"))
            (write-region (concat "Payload for " name) nil
                          (file-name-concat directory "artifacts" name) nil 'silent))
          (let ((report (mevedel-migrate-artifacts root (file-name-concat root "converted"))))
            (should-not (eq :unconverted (cadr (assoc "s1" report))))
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

  :doc "copies an unreadable artifact session unchanged and converts later sessions"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-corrupt-" t)))
         (destination (file-name-concat root "converted")))
    (unwind-protect
        (progn
          (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s0")))
            (write-region "broken JSON" nil
                          (file-name-concat directory "artifacts" "shared-editing" "board-1.json")
                          nil 'silent))
          (mevedel-migrate-artifacts-test--pid-session root "s1")
          (let ((report (mevedel-migrate-artifacts root destination)))
            (should (eq :unconverted (cadr (assoc "s0" report))))
            (should (equal "broken JSON"
                           (with-temp-buffer
                             (insert-file-contents
                              (file-name-concat destination "s0" "artifacts" "shared-editing" "board-1.json"))
                             (buffer-string))))
            (should (member "mockup" (cdr (assoc "s1" report))))))
      (delete-directory root t)))

  :doc "refuses before writing anything while a session is open"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-open-" t)))
         (destination (file-name-concat root "converted")))
    (unwind-protect
        (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s1")))
          (write-region "(:pid 1)" nil (file-name-concat directory ".lock") nil 'silent)
          (should-error (mevedel-migrate-artifacts root destination))
          (should-not (file-exists-p destination))
          (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts")))
          (delete-file (file-name-concat directory ".lock"))
          (should-error (mevedel-migrate-artifacts root (file-name-concat directory "converted")))
          (should-not (file-exists-p (file-name-concat directory "converted"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-migrate-artifacts--import ()
  ,test
  (test)
  :doc "removes interrupted imports so retries retain comments and a first version"
  (dolist (kind '(file item))
    (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-retry-" t)))
           (workspace (mevedel-workspace--create :type 'project :id "w" :root root))
           (import (lambda ()
                     (if (eq kind 'item)
                         (mevedel-migrate-artifacts--move-item
                          workspace "s1" "artifacts/shared-editing/board-1.json"
                          mevedel-migrate-artifacts-test--board)
                       (mevedel-migrate-artifacts--move-file
                        workspace "s1" "artifacts/a.html" "<p>Kept</p>"
                        (list (list :id "thread" :text "Kept comment")) "Chat s1")))))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mevedel-artifact-store-record-version)
                       (lambda (&rest _) (error "Interrupted version write"))))
              (should-error (funcall import)))
            (should-not (mevedel-artifact-store-ids workspace))
            (let ((id (funcall import)))
              (should (equal id (funcall import)))
              (should (= 1 (length (mevedel-artifact-store-versions workspace id))))
              (when (eq kind 'file)
                (should (equal "Kept comment"
                               (plist-get
                                (car (mevedel-collaboration--artifact-comments-read workspace id))
                                :text))))))
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
