;;; test-mevedel-working-note-files.el --- Working-note observation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real files exercise bounded observations and nonmutating discovery.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-working-note-files)
(require 'mevedel-resource)
(require 'mevedel-tool-fs-read)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-session-control-fs)

(defconst test-mevedel-working-note-files--source-directory
  (file-name-directory (directory-file-name (file-name-directory
                                            (or load-file-name buffer-file-name))))
  "Source tree used by the isolated discovery subprocess.")

(defun test-mevedel-working-note-files--cached-handler (path attributes listing &optional on-modes)
  "Model attribute and directory caches for PATH, not a remote transport.
Reading modes repopulates ATTRIBUTES; ON-MODES observes that injection.
LISTING is the cached directory observation.
Like TRAMP, return cached data unless `remote-file-name-inhibit-cache' is t."
  (let (handler)
    (setq handler
          (lambda (operation &rest args)
            (let ((inhibit-file-name-handlers (cons handler inhibit-file-name-handlers))
                  (inhibit-file-name-operation operation))
              (cond
               ((and (eq operation 'file-attributes) (equal path (car args))
                     (not (eq remote-file-name-inhibit-cache t))) attributes)
               ((and (eq operation 'directory-files-and-attributes)
                     (not (eq remote-file-name-inhibit-cache t))) listing)
               ((and (eq operation 'file-modes) (equal path (car args)))
                (setq attributes (file-attributes path))
                (when on-modes (funcall on-modes))
                (apply operation args))
               (t (apply operation args))))))
    handler))

(mevedel-deftest mevedel-working-note-files-inventory
    (:vars* ((root (make-temp-file "mevedel-note-inventory-" t)))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "links and entry limits are explicit incomplete observations"
  (progn
    (write-region "body" nil (file-name-concat root "a.md") nil 'silent)
    (make-symbolic-link "a.md" (file-name-concat root "linked.md"))
    (let ((inventory (mevedel-working-note-files-inventory root)))
      (should-not (plist-get inventory :complete))
      (should (assoc "linked.md" (plist-get inventory :problems)))
      (should (= 1 (length (plist-get inventory :files)))))
    (let ((mevedel-working-note-files-max-entries 0))
      (should-not (plist-get (mevedel-working-note-files-inventory root) :complete)))
    (dotimes (index 10)
      (write-region "body" nil (file-name-concat root (format "wide-%s.md" index)) nil 'silent))
    (let* ((mevedel-working-note-files-max-entries 3)
           (inventory (mevedel-working-note-files-inventory root)))
      (should-not (plist-get inventory :complete))
      (should (<= (length (plist-get inventory :files)) 3))))
  :doc "inventory bypasses stale directory attribute caches"
  (let ((path (file-name-concat root "a.md")) (remote-file-name-inhibit-cache nil))
    (write-region "body" nil path nil 'silent)
    (set-file-times path (seconds-to-time 100))
    (let* ((handler (test-mevedel-working-note-files--cached-handler
                     path (file-attributes path) (directory-files-and-attributes root nil directory-files-no-dot-files-regexp)))
           (file-name-handler-alist (cons (cons (concat "\\`" (regexp-quote root)) handler) file-name-handler-alist)))
      ;; Simulate an external change without invalidating the handler's cache.
      (let ((file-name-handler-alist nil)) (set-file-times path (seconds-to-time 200)))
      (should (= 100 (float-time (file-attribute-modification-time (file-attributes path)))))
      (let ((row (assoc "a.md" (plist-get (mevedel-working-note-files-inventory root) :files))))
        (should (= 200 (float-time (plist-get (cdr row) :modified))))))))

(mevedel-deftest mevedel-working-note-files-read
    (:vars* ((root (make-temp-file "mevedel-note-read-" t)))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "Markdown is reviewed but readable attachments remain available to reference scans"
  (progn
    (write-region "# External\n" nil (file-name-concat root "a.MD") nil 'silent)
    (write-region "See a.wav" nil (file-name-concat root "a.json") nil 'silent)
    (write-region (unibyte-string 0 255) nil (file-name-concat root "a.wav") nil 'silent)
    (let ((rows (mevedel-working-note-files-read root
                 (plist-get (mevedel-working-note-files-inventory root) :files))))
      (should (eq 'text (plist-get (cdr (assoc "a.MD" rows)) :kind)))
      (should (eq 'attachment (plist-get (cdr (assoc "a.json" rows)) :kind)))
      (should (equal "See a.wav" (plist-get (cdr (assoc "a.json" rows)) :text)))
      (should-not (plist-get (cdr (assoc "a.wav" rows)) :text))
      (should (equal '("a.wav") (mevedel-memory-notes--referenced '("a.wav") (mevedel-memory-notes--texts rows))))))
  :doc "deleted or concurrently modified sources remain visible as incomplete"
  (let ((path (file-name-concat root "a.md")))
    (write-region "body" nil path nil 'silent)
    (let ((files (plist-get (mevedel-working-note-files-inventory root) :files)))
      (set-file-times path (seconds-to-time 100))
      (should (plist-get (cdr (car (mevedel-working-note-files-read root files))) :error))
      (delete-file path)
      (should (plist-get (cdr (car (mevedel-working-note-files-read root files))) :error)))))

(mevedel-deftest mevedel-working-note-files-current-p ()
  (let* ((root (make-temp-file "mevedel-note-current-" t))
         (path (file-name-concat root "a.md")))
    (unwind-protect
        (progn
          (write-region "body" nil path nil 'silent)
          (let ((row (car (plist-get (mevedel-working-note-files-inventory root) :files))))
            (should (mevedel-working-note-files-current-p root row))
            ;; Same bytes, different mtime must still invalidate eligibility.
            (set-file-times path (seconds-to-time 100))
            (should-not (mevedel-working-note-files-current-p root row))))
      (delete-directory root t))))

(mevedel-deftest mevedel-working-note-files-listing
    (:vars* ((root (make-temp-file "mevedel-note-listing-" t))
             (workspace (mevedel-workspace--create :root root))
             (session (mevedel-session--create :workspace workspace))
             (shared (mevedel-resource-work-shared-directory workspace))
             (directory (file-name-concat shared "nested"))
             (path (file-name-concat directory "a b.MD"))
             (now 1800000000))
     :after-each ((when (file-directory-p directory) (set-file-modes directory #o755))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-session-control-fs-observation-time) (lambda (_) now)))
    ,test)
  (test)
  :doc "production root and descendant Read share generated rows without touching files or use evidence"
  (progn
    (make-directory directory t)
    (write-region (mevedel-working-note-normalize "# Actual summary\nBody" "author" "2000-01-01T00:00:00Z") nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 7 86400))))
    (write-region "{\"private\":\"JSON-NOT-A-NOTE\"}" nil (file-name-concat directory "data.json") nil 'silent)
    (set-file-times directory (seconds-to-time 100))
    (set-file-modes directory #o555)
    (let* ((mtime (file-attribute-modification-time (file-attributes directory)))
           (file-mtime (file-attribute-modification-time (file-attributes path)))
           (accesses (copy-tree (mevedel-resource-shared-accesses workspace)))
           (root-result (plist-get (mevedel-resource-execute
                                   (mevedel-resource-prepare 'read "work://shared" (list :session session))) :result))
           descendant)
      (mevedel-tool-fs-read--resource-directory directory "work://shared/nested" (lambda (text) (setq descendant text)))
      (should (string-search "work://shared/nested/a%20b.MD\tkind=scratch status=open age=7.00d owner=author" root-result))
      (should (string-search "age-ready; capture/application safety checks pending summary=Actual summary" descendant))
      (should (string-search "data.json\tkind=attachment" descendant))
      (should-not (string-search "kind: scratch" descendant))
      (should-not (string-search "JSON-NOT-A-NOTE" descendant))
      (should (equal mtime (file-attribute-modification-time (file-attributes directory))))
      (should (equal file-mtime (file-attribute-modification-time (file-attributes path))))
      (should (= #o555 (file-modes directory)))
      (should (equal accesses (mevedel-resource-shared-accesses workspace)))
      (should-not (file-exists-p (file-name-concat root ".mevedel" "state")))
      ;; Negative control: direct Read still returns exact YAML-bearing bytes.
      (should (string-prefix-p "---\nkind: scratch\n"
                               (mevedel-resource-execute
                                (mevedel-resource-prepare 'read "work://shared/nested/a%20b.MD" (list :session session))
                                (lambda (file _) (mevedel-session-control-fs-read-file file)))))
      (dolist (op '(glob grep))
        (should (plist-get (mevedel-resource-execute
                            (mevedel-resource-prepare op "work://shared" (list :session session))) :resource-search-roots)))))
  :doc "large text and JSON attachments with frontmatter-like prefixes stay typed in listing and capture"
  (progn
    (mevedel-workspace-identity-ensure root)
    (make-directory directory t)
    (dolist (name '("payload.txt" "payload.json"))
      (let ((file (file-name-concat directory name)))
        (write-region (concat "---\nkind: scratch\n" (make-string (* 40 1024) ?x)) nil file nil 'silent)
        (set-file-times file (seconds-to-time (- now (* 14 86400))))))
    (let ((root-result (plist-get (mevedel-resource-execute
                                  (mevedel-resource-prepare 'read "work://shared" (list :session session))) :result))
          descendant)
      (mevedel-tool-fs-read--resource-directory directory "work://shared/nested" (lambda (text) (setq descendant text)))
      (dolist (listing (list root-result descendant))
        (dolist (name '("payload.txt" "payload.json"))
          (should (string-search (concat "work://shared/nested/" name "\tkind=attachment status=- age=14.00d") listing)))
        (should (string-search "readiness=age-ready; capture/application safety checks pending" listing))
        (should-not (string-search "observation-incomplete" listing))
        (should-not (string-search "Metadata prefix is incomplete" listing))))
    (let* ((capture (mevedel-memory-notes-capture workspace nil))
           (captured-root (cdr (plist-get capture :root))))
      (should (= 0 (plist-get capture :notes)))
      (should (= 2 (plist-get capture :attachments)))
      (should (= 0 (plist-get capture :blocked-count)))
      (should (equal '("nested/payload.json" "nested/payload.txt")
                     (sort (copy-sequence (plist-get captured-root :attachments)) #'string<)))))
  :doc "empty discovery creates no shared directory or state"
  (progn
    (should (string-search "No shared working files" (plist-get
                                                    (mevedel-resource-execute
                                                     (mevedel-resource-prepare 'read "work://shared" (list :session session))) :result)))
    (should-not (file-exists-p (file-name-concat root ".mevedel")))))

(mevedel-deftest mevedel-memory-notes-capture/age
    (:vars* ((root (make-temp-file "mevedel-note-age-" t))
             (workspace (mevedel-workspace--create :root root))
             (shared (mevedel-resource-work-shared-directory workspace))
             (path (file-name-concat shared "note.md"))
             (now 1800000000))
     :before-each ((mevedel-workspace-identity-ensure root) (make-directory shared t))
     :after-each ((when-let* ((buffer (find-buffer-visiting path)))
                    (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-session-control-fs-observation-time) (lambda (_) now)))
    ,test)
  (test)
  :doc "capture and generated discovery use the same exact managed and external age boundaries"
  (dolist (fixture '((scratch open 7) (handoff done 7) (report open 14) (unmanaged open 14)))
    (let* ((kind (nth 0 fixture)) (status (nth 1 fixture)) (days (nth 2 fixture))
           (body (if (eq kind 'unmanaged) "---\nkind: invalid\n---\n# External\n"
                   (mevedel-working-note-normalize
                    (format "---\nkind: %s\nstatus: %s\n---\n# Body\n" kind status)
                    "owner" "2000-01-01T00:00:00Z"))))
      (write-region body nil path nil 'silent)
      (set-file-times path (seconds-to-time (+ (- now (* days 86400)) 3600)))
      (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :notes)))
      (should (string-search "readiness=age-wait" (mevedel-working-note-files-listing shared "work://shared")))
      (set-file-times path (seconds-to-time (- now (* days 86400))))
      (should (= 1 (plist-get (mevedel-memory-notes-capture workspace nil) :notes)))
      (should (string-search "readiness=age-ready; capture/application safety checks pending"
                             (mevedel-working-note-files-listing shared "work://shared")))
      ;; Reopening/editing is an actual modification, even with an old created timestamp.
      (set-file-times path (seconds-to-time now))
      (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :notes)))
      (set-file-times path (seconds-to-time (1+ now)))
      (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :notes)))
      (should (string-search "readiness=future-mtime" (mevedel-working-note-files-listing shared "work://shared")))))
  :doc "a long valid header is explicitly incomplete in discovery but fully classified before capture"
  (progn
    (write-region (mevedel-working-note-normalize "# Body\n" (make-string (* 34 1024) ?x)
                                                   "2000-01-01T00:00:00Z") nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 7 86400))))
    (let ((listing (mevedel-working-note-files-listing shared "work://shared")))
      (should (string-search "readiness=observation-incomplete" listing))
      (should (string-search "Observation incomplete" listing))
      (should-not (string-search "kind=unmanaged" listing)))
    (should (= 1 (plist-get (mevedel-memory-notes-capture workspace nil) :notes))))
  :doc "a same-byte mtime refresh during snapshot preparation prevents stale admission"
  (progn
    (write-region (mevedel-working-note-normalize "# Body\n" "owner" "2000-01-01T00:00:00Z") nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 7 86400))))
    (let ((original (symbol-function 'mevedel-memory-scope--snapshot)) (snapshots 0))
      (cl-letf (((symbol-function 'mevedel-memory-scope--snapshot)
                 (lambda (&rest args)
                   (cl-incf snapshots)
                   (prog1 (apply original args) (set-file-times path (seconds-to-time now))))))
        (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :notes))))
      (should (= 1 snapshots))))
  :doc "final capture bypasses attributes repopulated by snapshot mode reads"
  (progn
    (write-region (mevedel-working-note-normalize "# Body\n" "owner" "2000-01-01T00:00:00Z") nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 7 86400))))
    (let* ((original (symbol-function 'mevedel-memory-scope--snapshot))
           (snapshots 0) (mode-reads 0) (remote-file-name-inhibit-cache nil)
           (handler (test-mevedel-working-note-files--cached-handler
                     path (file-attributes path) (directory-files-and-attributes shared nil directory-files-no-dot-files-regexp)
                     (lambda () (cl-incf mode-reads))))
           (file-name-handler-alist (cons (cons (concat "\\`" (regexp-quote shared)) handler) file-name-handler-alist)))
      (cl-letf (((symbol-function 'mevedel-memory-scope--snapshot)
                 (lambda (&rest args)
                   (cl-incf snapshots)
                   (prog1 (apply original args)
                     ;; External same-byte touch after mode reads leave an old cache entry.
                     (let ((file-name-handler-alist nil)) (set-file-times path (seconds-to-time now)))))))
        (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :notes))))
      (should (= 1 snapshots))
      (should (> mode-reads 0))
      (should (= (- now (* 7 86400)) (float-time (file-attribute-modification-time (file-attributes path)))))
      (let ((remote-file-name-inhibit-cache t))
        (should (= now (float-time (file-attribute-modification-time (file-attributes path))))))))
  :doc "unsaved visiting buffers prevent capture even when disk age is ready"
  (progn
    (write-region "# External\n" nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 14 86400))))
    (with-current-buffer (find-file-noselect path) (goto-char (point-max)) (insert "Unsaved"))
    (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :notes))))
  :doc "cohort admission excludes edited versions until a fresh cohort is frozen"
  (progn
    (write-region "# External\n" nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 14 86400))))
    (let ((cohort (mevedel-memory-notes-cohort workspace nil)))
      (should (= 1 (plist-get (mevedel-memory-notes-capture workspace nil :cohort cohort) :notes)))
      (write-region "# Changed external\n" nil path nil 'silent)
      (set-file-times path (seconds-to-time (- now (* 14 86400))))
      (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil :cohort cohort) :notes)))
      (should (= 1 (plist-get (mevedel-memory-notes-capture
                               workspace nil :cohort (mevedel-memory-notes-cohort workspace nil)) :notes)))))
  :doc "a live session beginning exact-file use during snapshot preparation prevents admission"
  (let ((reader (generate-new-buffer " *note-capture-reader*"))
        (original (symbol-function 'mevedel-memory-scope--snapshot))
        (snapshots 0))
    (unwind-protect
        (progn
          (write-region "# External\n" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- now (* 14 86400))))
          (with-current-buffer reader (setq-local mevedel--session (mevedel-session--create :workspace workspace)))
          (cl-letf (((symbol-function 'mevedel-memory-scope--snapshot)
                     (lambda (&rest args)
                       (cl-incf snapshots)
                       (prog1 (apply original args)
                         (mevedel-resource-execute
                          (mevedel-resource-prepare 'read "work://shared/note.md"
                                                    (list :session (buffer-local-value 'mevedel--session reader)))
                          (lambda (_path _address) nil))))))
            (should (= 0 (plist-get (mevedel-memory-notes-capture workspace nil) :notes))))
          (should (= 1 snapshots)))
      (kill-buffer reader))))

(mevedel-deftest mevedel-working-note-files-cold-load ()
  (let* ((root (make-temp-file "mevedel-note-discovery-cold-" t))
         (shared (file-name-concat root "shared"))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (make-directory shared)
          (write-region (mevedel-working-note-normalize "# Cold summary\n" "author" "2000-01-01T00:00:00Z")
                        nil (file-name-concat shared "note.md") nil 'silent)
          (setenv "HOME" root)
          (dolist (name '("XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_STATE_HOME" "XDG_RUNTIME_DIR"))
            (let ((directory (file-name-concat root name)))
              (make-directory directory)
              (setenv name directory)))
          (with-temp-buffer
            (let* ((expression
                    `(progn
                       (setq native-comp-jit-compilation nil
                             native-comp-eln-load-path (list ,(file-name-concat root "eln")))
                       (require 'mevedel-working-note-files)
                       (let ((listing (mevedel-working-note-files-listing ,shared "work://shared")))
                         (unless (and (string-search "work://shared/note.md\tkind=scratch" listing)
                                      (string-search "summary=Cold summary" listing))
                           (error "Cold discovery did not read the managed note")))
                       (dolist (feature '(mevedel-memory-notes mevedel-memory-decision
                                          mevedel-memory-apply mevedel-memory-store mevedel-memory-pass))
                         (when (featurep feature) (error "Discovery loaded memory runtime: %s" feature)))))
                   (status (call-process invocation-name nil t nil "-Q" "--batch"
                                         "-L" test-mevedel-working-note-files--source-directory
                                         "--eval" (prin1-to-string expression))))
              (ert-info ((buffer-string)) (should (zerop status)))
              (should (string-empty-p (buffer-string))))))
      (delete-directory root t))))

(provide 'test-mevedel-working-note-files)
;;; test-mevedel-working-note-files.el ends here
