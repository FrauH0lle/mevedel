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
    (cl-remf data :dedicated-artifact)
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
         (head (concat generation "/manifest.el")))
    (make-directory (file-name-concat directory generation) t)
    (make-directory (file-name-concat directory ".lease") t)
    (mevedel-migrate-session--write
     (file-name-concat directory sidecar)
     (mevedel-migrate-artifacts-test--legacy-sidecar id "v0.5.10" 'portable))
    (write-region "# Notes" nil (file-name-concat directory note) nil 'silent)
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
                                           (file-name-concat directory note))))))
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
                                                  :id)))))
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

  :doc "refuses before writing anything while a session is open"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-migrate-open-" t)))
         (destination (file-name-concat root "converted")))
    (unwind-protect
        (let ((directory (mevedel-migrate-artifacts-test--pid-session root "s1")))
          (write-region "(:pid 1)" nil (file-name-concat directory ".lock") nil 'silent)
          (should-error (mevedel-migrate-artifacts root destination))
          (should-not (file-exists-p destination))
          (should-not (file-exists-p (file-name-concat root ".mevedel" "artifacts"))))
      (delete-directory root t))))

(provide 'test-mevedel-migrate-artifacts)
;;; test-mevedel-migrate-artifacts.el ends here
