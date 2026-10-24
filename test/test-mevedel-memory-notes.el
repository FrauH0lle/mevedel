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

(mevedel-deftest mevedel-memory-notes--text ()
  ,test
  (test)
  :doc "decodes UTF-8 text and rejects NUL bytes and invalid sequences"
  (progn
    (should (equal "café\n" (mevedel-memory-notes--text (encode-coding-string "café\n" 'utf-8))))
    (should-not (mevedel-memory-notes--text (unibyte-string ?a 0 ?b)))
    (should-not (mevedel-memory-notes--text (unibyte-string ?a 255 ?b))))
  :doc "a truncated prefix drops only its incomplete final character"
  (let ((bytes (encode-coding-string "ab界" 'utf-8)))
    (should (equal "ab" (mevedel-memory-notes--text (substring bytes 0 4) t)))
    (should-not (mevedel-memory-notes--text (substring bytes 0 4)))))

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
    (should-not (mevedel-memory-notes--external-p "lab/report.md" nil))))

(mevedel-deftest mevedel-memory-notes--suppressed ()
  ,test
  (test)
  :doc "pending and recently retained versions wait; other outcomes do not"
  (let ((now 1000000) (day 86400))
    (cl-flet ((seen (action status &rest keys)
                (list (append (list :action action :hash "v1" :status status :at (- now day)) keys))))
      (should (mevedel-memory-notes--suppressed (seen 'note-delete 'pending) "v1" now 7))
      (should (mevedel-memory-notes--suppressed (seen 'note-keep 'applied) "v1" now 7))
      (should-not (mevedel-memory-notes--suppressed (seen 'note-keep 'applied) "v1" (+ now (* 7 day)) 7))
      (should (mevedel-memory-notes--suppressed (seen 'note-condense 'applied :result "v2") "v2" now 7))
      (should (mevedel-memory-notes--suppressed (seen 'note-delete 'rejected) "v1" now 7))
      (should-not (mevedel-memory-notes--suppressed (seen 'note-delete 'rejected :superseded t) "v1" now 7))
      (should (mevedel-memory-notes--suppressed (seen 'note-delete 'reversed) "v1" now 7))
      (should-not (mevedel-memory-notes--suppressed (seen 'note-delete 'stale) "v1" now 7))
      (should-not (mevedel-memory-notes--suppressed (seen 'note-delete 'applied) "v1" now 7))
      (should-not (mevedel-memory-notes--suppressed (seen 'note-keep 'applied) "v9" now 7)))))

(mevedel-deftest mevedel-memory-notes--ancestors ()
  ,test
  (test)
  :doc "lists parents strictly below the shared root, innermost first"
  (progn
    (should (equal '("/w/.mevedel/shared/a/b" "/w/.mevedel/shared/a")
                   (mevedel-memory-notes--ancestors "/w/.mevedel/shared/a/b/c.md" "/w/.mevedel/shared/")))
    (should-not (mevedel-memory-notes--ancestors "/w/.mevedel/shared/c.md" "/w/.mevedel/shared"))))

(mevedel-deftest mevedel-memory-notes-workspace-busy-p
    (:vars* ((workspace (mevedel-workspace--create :root "/tmp/mevedel-busy-ws/"))
             (buffers nil))
     :after-each ((mapc #'kill-buffer buffers)))
  ,test
  (test)
  :doc "returns t, never the live request, and skips sessions without a root"
  (cl-flet ((session-buffer (root request)
              (let ((buffer (generate-new-buffer " *busy-probe*")))
                (push buffer buffers)
                (with-current-buffer buffer
                  (setq-local mevedel--session
                              (mevedel-session--create :workspace (mevedel-workspace--create :root root)))
                  (setq-local mevedel--current-request request))
                buffer)))
    (session-buffer nil (list :request 'other-workspace))
    (session-buffer "/tmp/elsewhere/" (list :request 'elsewhere))
    (should-not (mevedel-memory-notes-workspace-busy-p workspace))
    (session-buffer "/tmp/mevedel-busy-ws" (list :request 'live))
    (should (eq t (mevedel-memory-notes-workspace-busy-p workspace)))))

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
    (should-not (mevedel-memory-notes-changes (funcall proposal 'note-keep "n.md" note)))
    (should (equal (list (list :path "/s/n.md" :before note :action 'delete :after '(:path "/s/n.md" :exists nil)))
                   (mevedel-memory-notes-changes (funcall proposal 'note-delete "n.md" note))))
    (should (equal '(:path "/s/d" :exists nil)
                   (plist-get (car (mevedel-memory-notes-changes (funcall proposal 'note-prune "d" folder))) :after)))
    (should-error (mevedel-memory-notes-changes (funcall proposal 'note-prune "n.md" note)))
    (should-error (mevedel-memory-notes-changes (funcall proposal 'note-delete "d" folder)))
    (let ((change (car (mevedel-memory-notes-changes (funcall proposal 'note-condense "n.md" note "Open.\n\n")))))
      (should (equal "Open.\n" (plist-get change :bytes)))
      (should (equal (secure-hash 'sha256 "Open.\n") (plist-get (plist-get change :after) :hash)))
      (should (= #o644 (plist-get (plist-get change :after) :mode))))
    (should-error (mevedel-memory-notes-changes
                   (funcall proposal 'note-condense "n.md" note (make-string (* 33 1024) ?x))))))

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
    (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil :quiet-hours 0) :remaining)))
    (should (= 1 (plist-get (mevedel-memory-notes-capture workspace nil :quiet-hours 0) :notes))))
  :doc "every blocked item is counted while the listed reasons stay bounded"
  (let ((mevedel-memory-notes-max-review-bytes mevedel-memory-notes--max-note-bytes))
    (dotimes (index 70)
      (write-region (make-string (* 33 1024) ?x) nil (file-name-concat shared (format "big-%02d.md" index)) nil 'silent))
    (let ((capture (mevedel-memory-notes-capture workspace nil :quiet-hours 0)))
      (should (= 70 (plist-get capture :blocked-count)))
      (should (= mevedel-memory-notes--max-blocked (length (plist-get capture :blocked))))
      (should (= 70 (plist-get (mevedel-memory-notes-observation workspace) :blocked-count)))))
  :doc "the review batch is bounded and the rest waits, oldest first"
  (progn
    (dotimes (index 18)
      (let ((path (file-name-concat shared (format "n%02d.md" index))))
        (write-region "Short note.\n" nil path nil 'silent)
        (set-file-times path (seconds-to-time (- (float-time) (* 3600 (- 100 index)))))))
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
