;;; test-mevedel-memory-notes.el -- Shared working-note lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Unit coverage for shared-note classification, references, eligibility,
;; observation, collection and checked changes, using real files.  The
;; consolidation flow is covered in `test-mevedel-memory-pass-notes.el'.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-notes)
(require 'mevedel-memory-store)
(require 'mevedel-session-control-fs)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-working-note-files-text ()
  ,test
  (test)
  :doc "decodes UTF-8 text and rejects NUL bytes and invalid sequences"
  (progn
    (should (equal "café\n" (mevedel-working-note-files-text (encode-coding-string "café\n" 'utf-8))))
    (should-not (mevedel-working-note-files-text (unibyte-string ?a 0 ?b)))
    (should-not (mevedel-working-note-files-text (unibyte-string ?a 255 ?b))))
  :doc "a truncated prefix drops only its incomplete final character"
  (let ((bytes (encode-coding-string "ab界" 'utf-8)))
    (should (equal "ab" (mevedel-working-note-files-text (substring bytes 0 4) t)))
    (should-not (mevedel-working-note-files-text (substring bytes 0 4)))))

(mevedel-deftest mevedel-memory-notes--referenced ()
  ,test
  (test)
  :doc "links, backticked and prose paths, encodings and addresses all protect attachments"
  (let ((attachments '("lab/results/split.png" "lab/a b.png" "top.json" "orphan.png")))
    (should (equal '("lab/results/split.png" "lab/a b.png" "top.json")
                   (mevedel-memory-notes--referenced
                    attachments
                    '(("lab/report.md" . "See ![split](results/split.png) and [a](a%20b.png).")
                      ("notes.md" . "Raw data: `top.json`.")))))
    (should (equal '("orphan.png")
                   (mevedel-memory-notes--referenced
                    attachments '(("other.md" . "Read work://shared/orphan.png later.")))))
    (should-not (mevedel-memory-notes--referenced attachments nil))))

(mevedel-deftest mevedel-memory-notes--external-p ()
  ,test
  (test)
  :doc "memory citations of a file or a containing folder count by address or path"
  (progn
    (should (mevedel-memory-notes--external-p "lab/run/result.png" '("Data in `work://shared/lab/`.")))
    (should (mevedel-memory-notes--external-p "lab/report.md" '("See .mevedel/shared/lab/report.md")))
    (should-not (mevedel-memory-notes--external-p "other/report.md" '("Data in `work://shared/lab/`.")))
    (should-not (mevedel-memory-notes--external-p "lab/report.md" '("Working notes belong in work://shared/ (.mevedel/shared/).")))
    (should-not (mevedel-memory-notes--external-p "lab/report.md" nil))))

(mevedel-deftest mevedel-memory-notes--suppressed ()
  ,test
  (test)
  :doc "only exact pending and recovery versions wait; completed outcomes never add an age clock"
  (let ((now 1000000) (day 86400))
    (cl-flet ((seen (action status &rest keys)
                (list (append (list :action action :hash "v1" :status status :at (- now day)) keys))))
      (should (mevedel-memory-notes--suppressed (seen 'note-delete 'pending) "v1"))
      (should (mevedel-memory-notes--suppressed (seen 'note-delete 'recovery-required) "v1"))
      (dolist (status '(applied rejected reversed stale unavailable))
        (should-not (mevedel-memory-notes--suppressed (seen 'note-delete status) "v1")))
      (should-not (mevedel-memory-notes--suppressed (seen 'note-delete 'pending) "v9")))))

(mevedel-deftest mevedel-memory-notes--ancestors ()
  ,test
  (test)
  :doc "lists parents strictly below the shared root, innermost first"
  (progn
    (should (equal '("/w/.mevedel/shared/a/b" "/w/.mevedel/shared/a")
                   (mevedel-memory-notes--ancestors "/w/.mevedel/shared/a/b/c.md" "/w/.mevedel/shared/")))
    (should-not (mevedel-memory-notes--ancestors "/w/.mevedel/shared/c.md" "/w/.mevedel/shared"))))

(mevedel-deftest mevedel-memory-notes--admission-rank ()
  ,test
  (test)
  :doc "only the latest generation for the exact source version determines priority"
  (let* ((miss '(:capture captured-without-proposal :root "r1" :path "/s/n.md" :hash "v1" :generation 2))
         (proposal '(:capture proposal :root "r1" :path "/s/n.md" :hash "v1" :generation 3)))
    (should (= 0 (mevedel-memory-notes--admission-rank nil "r1" "/s/n.md" "v1")))
    (should (= 1 (mevedel-memory-notes--admission-rank (list miss) "r1" "/s/n.md" "v1")))
    (should (= 0 (mevedel-memory-notes--admission-rank (list miss) "r1" "/s/n.md" "v2")))
    (should (= 0 (mevedel-memory-notes--admission-rank (list miss) "r1" "/other/n.md" "v1")))
    (should (= 0 (mevedel-memory-notes--admission-rank (list miss) "r2" "/s/n.md" "v1")))
    (should (= 0 (mevedel-memory-notes--admission-rank (list miss proposal) "r1" "/s/n.md" "v1")))
    (should (= 0 (mevedel-memory-notes--admission-rank (list proposal miss) "r1" "/s/n.md" "v1")))))

(mevedel-deftest mevedel-memory-notes--history
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-notes-history-" t))))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (mevedel-memory-dirs nil)
             (now 1800000000)
             (no-action "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")
             claim)
     :before-each ((mevedel-workspace-identity-ensure root) (make-directory shared t))
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) now))
            ((symbol-function 'mevedel-session-control-fs-observation-time) (lambda (_) now)))
    (cl-labels
        ((note (name index &optional text)
           (let ((path (file-name-concat shared name)))
             (write-region (or text "Completed observation.\n") nil path nil 'silent)
             (set-file-times path (seconds-to-time (+ (- now (* 500 3600)) index)))
             path))
         (capture () (mevedel-memory-notes-capture workspace nil))
         (files (capture) (plist-get (cdr (plist-get capture :root)) :files))
         (accept (capture &optional reply admitted)
           (let* ((scope (mevedel-memory-scope-capture workspace))
                  (_ (setf (plist-get scope :roots) (list (plist-get capture :root))))
                  (notes (mapcar (lambda (file) (mevedel-memory-notes-address file)) (files capture))))
             (setq claim (mevedel-journal-claim-acquire
                          (file-name-concat (mevedel-journal-store-state-directory
                                             (mevedel-journal-store-directory root)) "consolidation") 180))
             (mevedel-memory-store-accept
              workspace (mevedel-memory-store-prepare workspace claim scope nil "")
              (or reply no-action) nil "test:model" nil (or admitted notes)))))
      ,test))
  (test)
  :doc "sixteen repeated no-proposal versions cannot exclude a seventeenth new note"
  (progn
    (dotimes (index 16) (note (format "n%02d.md" index) index))
    (let ((initial (capture)))
      (should (= 16 (plist-get initial :notes)))
      (accept initial)
      ;; Repeated accepted omissions do not suppress, delete or mutate notes.
      (accept (capture))
      (dotimes (index 16)
        (should (equal "Completed observation.\n"
                       (with-temp-buffer
                         (insert-file-contents (file-name-concat shared (format "n%02d.md" index)))
                         (buffer-string)))))
      (note "valid.md" 100)
      (let ((next (capture)))
        (should (= 16 (plist-get next :notes)))
        (should (= 1 (plist-get next :remaining)))
        (should (equal (cons "valid.md" (cl-loop for index below 15 collect (format "n%02d.md" index)))
                       (files next))))))
  :doc "editing a previously omitted version resets its priority without bypassing readiness"
  (progn
    (note "old.md" 0)
    (accept (capture))
    (note "new.md" 1)
    (should (equal '("new.md" "old.md") (files (capture))))
    (let ((path (note "old.md" 0 "Edited observation.\n")))
      (should (equal '("old.md" "new.md") (files (capture))))
      (set-file-times path (seconds-to-time now))
      (should (equal '("new.md") (files (capture))))))
  :doc "a later legitimate proposal supersedes an omission even when timestamps tie"
  (progn
    (note "old.md" 0)
    (accept (capture))
    (let* ((accepted (accept (capture)
                             (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n"
                                     "## Notes\n````proposal\nnote: \"work://shared/old.md\"\ndisposition: \"delete\"\n"
                                     "reason: \"No qualifying memory\"\n---\n````\n"
                                     "## No action\n- none")))
           (proposal (car (plist-get accepted :proposals))))
      (should (eq 'note-delete (plist-get proposal :action)))
      ;; Pending decisions still suppress normally, independently of rank.
      (should-not (files (capture)))
      (mevedel-memory-decision-reject workspace (plist-get (plist-get accepted :review) :pass-id)
                                      (plist-get proposal :id) "Reconsider now")
      (note "new.md" 1)
      (should (equal '("old.md" "new.md") (files (capture))))))
  :doc "captured but nonadmitted notes are labeled without claiming inference service"
  (progn
    (note "admitted.md" 0)
    (note "not-admitted.md" 1)
    (accept (capture) nil '("work://shared/admitted.md"))
    (let* ((paths (make-hash-table :test #'equal))
           (_ (puthash "not-admitted.md" t paths))
           (history (mevedel-memory-notes--history workspace nil paths))
           (observation (car (gethash "not-admitted.md" history))))
      (should (= 1 (hash-table-count history)))
      (should (eq 'captured-without-proposal (plist-get observation :capture)))
      (should-not (plist-get observation :status))
      (should-not (mevedel-memory-notes--suppressed (list observation) (plist-get observation :hash)))))
  :doc "another client's accepted omission cannot rank the same absolute path and hash"
  (progn
    (note "old.md" 0)
    (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () "another-client")))
      (accept (capture)))
    (note "new.md" 1)
    (should (equal '("old.md" "new.md") (files (capture)))))
  :doc "a different configured shared root cannot inherit another source's omission rank"
  (let ((first (file-name-concat root "first" ".mevedel" "shared"))
        (second (file-name-concat root "second" ".mevedel" "shared")))
    (make-directory first t)
    (make-directory second t)
    (cl-letf (((symbol-function 'mevedel-memory-notes-directory) (lambda (_) shared)))
      (setq shared first)
      (note "old.md" 0)
      (accept (capture))
      (setq shared second)
      (note "old.md" 0)
      (note "new.md" 1)
      (should (equal '("old.md" "new.md") (files (capture))))))
  :doc "changed accepted bytes fail authentication rather than influencing admission"
  (progn
    (note "old.md" 0)
    (let* ((accepted (accept (capture)))
           (id (plist-get (plist-get accepted :prepared) :id))
           (path (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root))
                                   "passes" id "accepted.el")))
      (write-region " " nil path t 'silent)
      (should-error (capture)))))

(mevedel-deftest mevedel-memory-notes-in-use
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-in-use-" t))))
             (other-root (file-name-as-directory (file-truename (make-temp-file "mevedel-in-use-other-" t))))
             (workspace (mevedel-workspace--create :root root))

             (buffers nil))
     :before-each ((dolist (directory (list root other-root))
                     (make-directory (file-name-concat directory ".mevedel" "shared" "audit") t)))
     :after-each ((mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) buffers)
                  (delete-directory root t)
                  (delete-directory other-root t)))
  ,test
  (test)
  :doc "a named shared file a live session's tool touched is in use until that session closes"
  (cl-flet ((session-buffer (directory &optional request)
              (let ((buffer (generate-new-buffer " *in-use-probe*")))
                (push buffer buffers)
                (with-current-buffer buffer
                  (setq-local mevedel--session
                              (mevedel-session--create
                               :workspace (if (equal directory root) workspace
                                            (mevedel-workspace--create :root directory))))
                  (setq-local mevedel--current-request request))
                buffer))
            (use (buffer operation address)
              (mevedel-resource-prepare operation address
                                        (list :session (buffer-local-value 'mevedel--session buffer)))))
    (let ((reader (session-buffer root))
          (elsewhere (session-buffer other-root)))
      ;; Another session's running turn alone protects nothing.
      (session-buffer root (list :request 'live))
      (should-not (mevedel-memory-notes-in-use workspace))
      ;; Listings and searches over folders are not use of their notes.
      (use reader 'glob "work://shared/")
      (use reader 'grep "work://shared/audit")
      (use elsewhere 'read "work://shared/audit/report.md")
      (should-not (mevedel-memory-notes-in-use workspace))
      (use reader 'read "work://shared/audit/report.md")
      (should (equal '("audit/report.md") (mevedel-memory-notes-in-use workspace)))
      (should (eq t (mevedel-memory-notes-in-use-p workspace "audit/report.md")))
      (should-not (mevedel-memory-notes-in-use-p workspace "audit/other.md"))
      ;; Use ends with the quiet period.
      (cl-letf (((symbol-function 'float-time)
                 (let ((future (+ (float-time) 86401))) (lambda (&optional _) future))))
        (should-not (mevedel-memory-notes-in-use workspace)))
      (kill-buffer reader)
      (should-not (mevedel-memory-notes-in-use workspace)))))

(mevedel-deftest mevedel-memory-notes-same-p ()
  ,test
  (test)
  :doc "compares absence, folder emptiness, and file hash and mode"
  (let ((file (list :path "/p" :exists t :hash "h" :mode #o644)))
    (should (mevedel-memory-notes-same-p file (append (list :bytes "x") file)))
    (should-not (mevedel-memory-notes-same-p file (plist-put (copy-sequence file) :hash "g")))
    (should-not (mevedel-memory-notes-same-p file (plist-put (copy-sequence file) :mode #o600)))
    (should (mevedel-memory-notes-same-p '(:path "/p" :exists nil) '(:path "/p" :exists nil)))
    (should-not (mevedel-memory-notes-same-p file '(:path "/p" :exists nil)))
    (should (mevedel-memory-notes-same-p '(:path "/d" :exists t :directory t :empty t)
                                         '(:path "/d" :exists t :directory t :empty t)))
    (should-not (mevedel-memory-notes-same-p '(:path "/d" :exists t :directory t :empty t)
                                             '(:path "/d" :exists t :directory t :empty nil)))))

(mevedel-deftest mevedel-memory-notes-observe
    (:vars* ((root (make-temp-file "mevedel-notes-observe-" t)))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "hashes files on the target, records folder emptiness and absence, and refuses links"
  (let ((file (file-name-concat root "a.bin")) (folder (file-name-concat root "d")))
    (write-region "abc" nil file nil 'silent)
    (set-file-modes file #o640)
    (make-directory folder)
    (should (equal (list :path file :exists t :hash (secure-hash 'sha256 "abc") :size 3 :mode #o640)
                   (mevedel-memory-notes-observe (list :path file))))
    (should (equal (list :path folder :exists t :directory t :empty t)
                   (mevedel-memory-notes-observe (list :path folder))))
    (write-region "" nil (file-name-concat folder "x") nil 'silent)
    (should-not (plist-get (mevedel-memory-notes-observe (list :path folder)) :empty))
    (should (equal (list :path (file-name-concat root "none") :exists nil)
                   (mevedel-memory-notes-observe (list :path (file-name-concat root "none")))))
    (make-symbolic-link file (file-name-concat root "link"))
    (should-error (mevedel-memory-notes-observe (list :path (file-name-concat root "link"))))))

(mevedel-deftest mevedel-memory-notes-changes ()
  ,test
  (test)
  :doc "disposals map to exact checked changes and refuse mismatched entries"
  (let* ((note (list :path "/s/n.md" :exists t :bytes "Old\n" :hash (secure-hash 'sha256 "Old\n") :mode #o644))
         (folder (list :path "/s/d" :exists t :directory t :empty t))
         (proposal (lambda (action file before &optional body)
                     (list :action action :file file :before (list (cons file before)) :body (or body "")))))
    (should (equal (list (list :path "/s/n.md" :before note :action 'delete :after '(:path "/s/n.md" :exists nil)))
                   (mevedel-memory-notes-changes (funcall proposal 'note-delete "n.md" note))))
    (should (equal '(:path "/s/d" :exists nil)
                   (plist-get (car (mevedel-memory-notes-changes (funcall proposal 'note-prune "d" folder))) :after)))
    (should-error (mevedel-memory-notes-changes (funcall proposal 'note-prune "n.md" note)))
    (should-error (mevedel-memory-notes-changes (funcall proposal 'note-delete "d" folder)))
    (should-error (mevedel-memory-notes-changes (funcall proposal 'unknown "n.md" note)))
    (should-error (mevedel-memory-notes-changes (funcall proposal 'note-delete "missing.md" nil)))))

(mevedel-deftest mevedel-memory-notes-housekeeping ()
  ,test
  (test)
  :doc "captured attachments and empty folders become structured decisions"
  (let ((scope (list :roots (list (cons "notes" (list :kind 'notes :attachments '("a.png")
                                                      :directories '("old")))))))
    (should (equal '((note-delete "a.png" "attachment") (note-prune "old" "directory"))
                   (mapcar (lambda (proposal) (list (plist-get proposal :action) (plist-get proposal :file)
                                                    (plist-get proposal :type)))
                           (mevedel-memory-notes-housekeeping scope))))
    (should-not (mevedel-memory-notes-housekeeping (list :roots nil)))))

(mevedel-deftest mevedel-memory-notes-capture
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-notes-capture-" t))))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared")))
     :before-each ((mevedel-workspace-identity-ensure root) (make-directory shared t))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "a missing folder captures nothing and young files wait"
  (progn
    (delete-directory shared)
    (should-not (mevedel-memory-notes-capture workspace nil))
    (make-directory shared)
    (write-region "Fresh.\n" nil (file-name-concat shared "fresh.md") nil 'silent)
    ;; The target clock has whole-second resolution.
    (set-file-times (file-name-concat shared "fresh.md") (seconds-to-time (- (float-time) 2)))
    (let ((capture (mevedel-memory-notes-capture workspace nil)))
      (should-not (plist-get capture :root))
      (should (= 0 (plist-get capture :notes))))
    (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :remaining)))
    (set-file-times (file-name-concat shared "fresh.md") (seconds-to-time (- (float-time) (* 15 86400))))
    (should (= 1 (plist-get (mevedel-memory-notes-capture workspace nil) :notes))))
  :doc "every blocked item is counted while the listed reasons stay bounded"
  (let ((mevedel-memory-notes-max-review-bytes mevedel-memory-notes--max-note-bytes))
    (dotimes (index 70)
      (write-region (make-string (* 33 1024) ?x) nil (file-name-concat shared (format "big-%02d.md" index)) nil 'silent))
    (let ((capture (mevedel-memory-notes-capture workspace nil)))
      (should (= 70 (plist-get capture :blocked-count)))
      (should (= mevedel-memory-notes--max-blocked (length (plist-get capture :blocked))))
      (should (= 70 (plist-get (mevedel-memory-notes-observation workspace) :blocked-count)))))
  :doc "the review batch is bounded and the rest waits, oldest first"
  (progn
    (dotimes (index 18)
      (let ((path (file-name-concat shared (format "n%02d.md" index))))
        (write-region "Short note.\n" nil path nil 'silent)
        (set-file-times path (seconds-to-time (- (float-time) (* 3600 (- 500 index)))))))
    (let* ((capture (mevedel-memory-notes-capture workspace nil))
           (root-row (cdr (plist-get capture :root))))
      (should (= mevedel-memory-notes--max-notes (plist-get capture :notes)))
      (should (= 2 (plist-get capture :remaining)))
      (should (equal "n00.md" (car (plist-get root-row :files)))))))

(mevedel-deftest mevedel-memory-notes-recheck
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-notes-recheck-" t)))))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "only text written since capture can newly protect an attachment"
  (let ((since (float-time)))
    (write-region "Old note about a.png.\n" nil (file-name-concat root "old.md") nil 'silent)
    (set-file-times (file-name-concat root "old.md") (seconds-to-time (- since 3600)))
    (should-not (mevedel-memory-notes-recheck (list :dir root :captured since) "a.png"))
    (write-region "New note about a.png.\n" nil (file-name-concat root "new.md") nil 'silent)
    (set-file-times (file-name-concat root "new.md") (seconds-to-time (+ since 60)))
    (should (mevedel-memory-notes-recheck (list :dir root :captured since) "a.png"))
    (should-not (mevedel-memory-notes-recheck (list :dir root :captured since) "b.png"))
    (make-symbolic-link "new.md" (file-name-concat root "link.md"))
    (should (mevedel-memory-notes-recheck (list :dir root :captured since) "b.png"))))

(mevedel-deftest mevedel-memory-notes-collect-blobs
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-notes-blobs-" t))))
             (workspace (mevedel-workspace--create :root root))
             (blobs (file-name-concat root ".mevedel" "state" "shared-notes" "blobs")))
     :before-each ((mevedel-workspace-identity-ensure root) (make-directory blobs t))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "removes blobs that no retained write references and fails closed on unreadable writes"
  (progn
    (write-region "x" nil (file-name-concat blobs (make-string 64 ?a)) nil 'silent)
    (write-region "other" nil (file-name-concat blobs "README") nil 'silent)
    (should (= 1 (mevedel-memory-notes-collect-blobs workspace blobs)))
    (should-not (file-exists-p (file-name-concat blobs (make-string 64 ?a))))
    (should (file-exists-p (file-name-concat blobs "README")))
    (write-region "x" nil (file-name-concat blobs (make-string 64 ?b)) nil 'silent)
    (cl-letf (((symbol-function 'mevedel-memory-write-list)
               (lambda (_) (list (list :id (make-string 64 ?c) :error "Unreadable")))))
      (should-error (mevedel-memory-notes-collect-blobs workspace blobs)))
    (should (file-exists-p (file-name-concat blobs (make-string 64 ?b))))))

(provide 'test-mevedel-memory-notes)
;;; test-mevedel-memory-notes.el ends here
