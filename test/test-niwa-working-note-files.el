;;; test-niwa-working-note-files.el --- Working-note observation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real files exercise bounded observations and nonmutating discovery.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-working-note-files)
(require 'niwa-resource)
(require 'niwa-tool-fs-read)
(require 'niwa-memory-notes)
(require 'niwa-memory-scope)
(require 'niwa-session-control-fs)

(defconst test-niwa-working-note-files--source-directory
  (file-name-directory (directory-file-name (file-name-directory
                                            (or load-file-name buffer-file-name))))
  "Source tree used by the isolated discovery subprocess.")

(defun test-niwa-working-note-files--cached-handler (path attributes listing &optional on-modes)
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

(niwa-deftest niwa-working-note-files-markdown-p ()
  (progn
    (should (niwa-working-note-files-markdown-p "note.MD"))
    (should-not (niwa-working-note-files-markdown-p "note.txt"))
    (should-not (niwa-working-note-files-markdown-p nil))))

(niwa-deftest niwa-working-note-files-heading ()
  (progn
    (should (equal "First heading" (niwa-working-note-files-heading "Prose\n# First heading\n## Later\n")))
    (should (equal "" (niwa-working-note-files-heading "Prose only\n")))
    (should (<= (length (niwa-working-note-files-heading (concat "# " (make-string 200 ?x)))) 160))))

(niwa-deftest niwa-working-note-files-eligibility ()
  (let ((now 1800000000) (niwa-working-note-max-age-days 14))
    (should (plist-get (niwa-working-note-files-eligibility (- now (* 14 86400)) now) :eligible))
    (should-not (plist-get (niwa-working-note-files-eligibility (1+ (- now (* 14 86400))) now) :eligible))
    (should (eq 'future-mtime (plist-get (niwa-working-note-files-eligibility (1+ now) now) :reason)))
    (should (eq 'unknown-mtime (plist-get (niwa-working-note-files-eligibility nil now) :reason)))
    (should (eq 'unknown-mtime (plist-get (niwa-working-note-files-eligibility now nil) :reason)))
    (let ((niwa-working-note-max-age-days nil))
      (should (eq 'disabled (plist-get (niwa-working-note-files-eligibility 0 now) :reason))))))

(niwa-deftest niwa-working-note-files-inventory
    (:vars* ((root (make-temp-file "niwa-note-inventory-" t)))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "links and entry limits are explicit incomplete observations"
  (progn
    (write-region "body" nil (file-name-concat root "a.md") nil 'silent)
    (make-symbolic-link "a.md" (file-name-concat root "linked.md"))
    (let ((inventory (niwa-working-note-files-inventory root)))
      (should-not (plist-get inventory :complete))
      (should (assoc "linked.md" (plist-get inventory :problems)))
      (should (= 1 (length (plist-get inventory :files)))))
    (let ((niwa-working-note-files-max-entries 0))
      (should-not (plist-get (niwa-working-note-files-inventory root) :complete)))
    (dotimes (index 10)
      (write-region "body" nil (file-name-concat root (format "wide-%s.md" index)) nil 'silent))
    (let* ((niwa-working-note-files-max-entries 3)
           (inventory (niwa-working-note-files-inventory root)))
      (should-not (plist-get inventory :complete))
      (should (<= (length (plist-get inventory :files)) 3))))
  :doc "inventory bypasses stale directory attribute caches"
  (let ((path (file-name-concat root "a.md")) (remote-file-name-inhibit-cache nil))
    (write-region "body" nil path nil 'silent)
    (set-file-times path (seconds-to-time 100))
    (let* ((handler (test-niwa-working-note-files--cached-handler
                     path (file-attributes path) (directory-files-and-attributes root nil directory-files-no-dot-files-regexp)))
           (file-name-handler-alist (cons (cons (concat "\\`" (regexp-quote root)) handler) file-name-handler-alist)))
      ;; Simulate an external change without invalidating the handler's cache.
      (let ((file-name-handler-alist nil)) (set-file-times path (seconds-to-time 200)))
      (should (= 100 (float-time (file-attribute-modification-time (file-attributes path)))))
      (let ((row (assoc "a.md" (plist-get (niwa-working-note-files-inventory root) :files))))
        (should (= 200 (float-time (plist-get (cdr row) :modified))))))))

(niwa-deftest niwa-working-note-files-read
    (:vars* ((root (make-temp-file "niwa-note-read-" t)))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "Markdown is reviewed but readable attachments remain available to reference scans"
  (progn
    (write-region "# External\n" nil (file-name-concat root "a.MD") nil 'silent)
    (write-region "See a.wav" nil (file-name-concat root "a.json") nil 'silent)
    (write-region (unibyte-string 0 255) nil (file-name-concat root "a.wav") nil 'silent)
    (let ((rows (niwa-working-note-files-read root
                 (plist-get (niwa-working-note-files-inventory root) :files))))
      (should (eq 'text (plist-get (cdr (assoc "a.MD" rows)) :kind)))
      (should (eq 'attachment (plist-get (cdr (assoc "a.json" rows)) :kind)))
      (should (equal "See a.wav" (plist-get (cdr (assoc "a.json" rows)) :text)))
      (should-not (plist-get (cdr (assoc "a.wav" rows)) :text))
      (should (equal '("a.wav") (niwa-memory-notes--referenced '("a.wav") (niwa-memory-notes--texts rows))))))
  :doc "deleted or concurrently modified sources remain visible as incomplete"
  (let ((path (file-name-concat root "a.md")))
    (write-region "body" nil path nil 'silent)
    (let ((files (plist-get (niwa-working-note-files-inventory root) :files)))
      (set-file-times path (seconds-to-time 100))
      (should (plist-get (cdr (car (niwa-working-note-files-read root files))) :error))
      (delete-file path)
      (should (plist-get (cdr (car (niwa-working-note-files-read root files))) :error)))))

(niwa-deftest niwa-working-note-files-current-p ()
  (let* ((root (make-temp-file "niwa-note-current-" t))
         (path (file-name-concat root "a.md")))
    (unwind-protect
        (progn
          (write-region "body" nil path nil 'silent)
          (let ((row (car (plist-get (niwa-working-note-files-inventory root) :files))))
            (should (niwa-working-note-files-current-p root row))
            ;; Same bytes, different mtime must still invalidate eligibility.
            (set-file-times path (seconds-to-time 100))
            (should-not (niwa-working-note-files-current-p root row))))
      (delete-directory root t))))

(niwa-deftest niwa-working-note-files-listing
    (:vars* ((root (make-temp-file "niwa-note-listing-" t))
             (workspace (niwa-workspace--create :root root))
             (session (niwa-session--create :workspace workspace))
             (shared (niwa-resource-work-shared-directory workspace))
             (directory (file-name-concat shared "nested"))
             (path (file-name-concat directory "a b.MD"))
             (now 1800000000))
     :after-each ((when (file-directory-p directory) (set-file-modes directory #o755))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'niwa-session-control-fs-observation-time) (lambda (_) now)))
    ,test)
  (test)
  :doc "production root and descendant Read share generated rows without touching files or use evidence"
  (progn
    (make-directory directory t)
    (write-region "# Actual heading\nBody" nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 14 86400))))
    (write-region "{\"private\":\"JSON-NOT-A-NOTE\"}" nil (file-name-concat directory "data.json") nil 'silent)
    (set-file-times directory (seconds-to-time 100))
    (set-file-modes directory #o555)
    (let* ((mtime (file-attribute-modification-time (file-attributes directory)))
           (file-mtime (file-attribute-modification-time (file-attributes path)))
           (accesses (copy-tree (niwa-resource-shared-accesses workspace)))
           (root-result (plist-get (niwa-resource-execute
                                   (niwa-resource-prepare 'read "work://shared" (list :session session))) :result))
           descendant)
      (niwa-tool-fs-read--resource-directory directory "work://shared/nested" (lambda (text) (setq descendant text)))
      (should (string-search "work://shared/nested/a%20b.MD\tage=14.00d" root-result))
      (should (string-search "age-ready; capture/application safety checks pending heading=Actual heading" descendant))
      (should (string-search "data.json\tage=" descendant))
      (dolist (obsolete '("kind=" "status=" "owner="))
        (should-not (string-search obsolete descendant)))
      (should-not (string-search "JSON-NOT-A-NOTE" descendant))
      (should (equal mtime (file-attribute-modification-time (file-attributes directory))))
      (should (equal file-mtime (file-attribute-modification-time (file-attributes path))))
      (should (= #o555 (file-modes directory)))
      (should (equal accesses (niwa-resource-shared-accesses workspace)))
      (should-not (file-exists-p (file-name-concat root ".niwa" "state")))
      ;; Negative control: direct Read still returns exact authored bytes.
      (should (equal "# Actual heading\nBody"
                               (niwa-resource-execute
                                (niwa-resource-prepare 'read "work://shared/nested/a%20b.MD" (list :session session))
                                (lambda (file _) (niwa-session-control-fs-read-file file)))))
      (dolist (op '(glob grep))
        (should (plist-get (niwa-resource-execute
                            (niwa-resource-prepare op "work://shared" (list :session session))) :resource-search-roots)))))
  :doc "large text and JSON attachments with frontmatter-like prefixes stay typed in listing and capture"
  (progn
    (niwa-workspace-identity-ensure root)
    (make-directory directory t)
    (dolist (name '("payload.txt" "payload.json"))
      (let ((file (file-name-concat directory name)))
        (write-region (concat "---\nkind: scratch\n" (make-string (* 40 1024) ?x)) nil file nil 'silent)
        (set-file-times file (seconds-to-time (- now (* 14 86400))))))
    (let ((root-result (plist-get (niwa-resource-execute
                                  (niwa-resource-prepare 'read "work://shared" (list :session session))) :result))
          descendant)
      (niwa-tool-fs-read--resource-directory directory "work://shared/nested" (lambda (text) (setq descendant text)))
      (dolist (listing (list root-result descendant))
        (dolist (name '("payload.txt" "payload.json"))
          (should (string-search (concat "work://shared/nested/" name "\tage=14.00d") listing)))
        (should (string-search "readiness=age-ready; capture/application safety checks pending" listing))
        (should-not (string-search "observation-incomplete" listing))
        (should-not (string-search "Metadata prefix is incomplete" listing))))
    (let* ((capture (niwa-memory-notes-capture workspace nil))
           (captured-root (cdr (plist-get capture :root))))
      (should (= 0 (plist-get capture :notes)))
      (should (= 2 (plist-get capture :attachments)))
      (should (= 0 (plist-get capture :blocked-count)))
      (should (equal '("nested/payload.json" "nested/payload.txt")
                     (sort (copy-sequence (plist-get captured-root :attachments)) #'string<)))))
  :doc "empty discovery creates no shared directory or state"
  (progn
    (should (string-search "No shared working files" (plist-get
                                                    (niwa-resource-execute
                                                     (niwa-resource-prepare 'read "work://shared" (list :session session))) :result)))
    (should-not (file-exists-p (file-name-concat root ".niwa")))))

(niwa-deftest niwa-memory-notes-capture/age
    (:vars* ((root (make-temp-file "niwa-note-age-" t))
             (workspace (niwa-workspace--create :root root))
             (shared (niwa-resource-work-shared-directory workspace))
             (path (file-name-concat shared "note.md"))
             (now 1800000000))
     :before-each ((niwa-workspace-identity-ensure root) (make-directory shared t))
     :after-each ((when-let* ((buffer (find-buffer-visiting path)))
                    (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'niwa-session-control-fs-observation-time) (lambda (_) now)))
    ,test)
  (test)
  :doc "capture and discovery use mtime alone, ignoring all header-like prose"
  (dolist (body '("# Body\n" "---\nkind: scratch\nstatus: done\n---\n# Body\n" "---\nkind: invalid\n# Body\n"))
    (let ((days 14))
      (write-region body nil path nil 'silent)
      (set-file-times path (seconds-to-time (1+ (- now (* days 86400)))))
      (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil) :notes)))
      (should (string-search "readiness=age-wait" (niwa-working-note-files-listing shared "work://shared")))
      (set-file-times path (seconds-to-time (- now (* days 86400))))
      (should (= 1 (plist-get (niwa-memory-notes-capture workspace nil) :notes)))
      (should (string-search "readiness=age-ready; capture/application safety checks pending"
                             (niwa-working-note-files-listing shared "work://shared")))
      ;; An actual edit resets eligibility; reading the listing did not.
      (set-file-times path (seconds-to-time now))
      (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil) :notes)))
      (set-file-times path (seconds-to-time (1+ now)))
      (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil) :notes)))
      (should (string-search "readiness=future-mtime" (niwa-working-note-files-listing shared "work://shared")))))
  :doc "one setting changes note and attachment admission together and nil disables it"
  (let ((attachment (file-name-concat shared "a.bin")))
    (write-region "# Body\n" nil path nil 'silent)
    (write-region (unibyte-string 0 1 2) nil attachment nil 'silent)
    (dolist (file (list path attachment))
      (set-file-times file (seconds-to-time (- now (* 10 86400)))))
    (let ((niwa-working-note-max-age-days 9)
          capture)
      (setq capture (niwa-memory-notes-capture workspace nil))
      (should (= 1 (plist-get capture :notes)))
      (should (= 1 (plist-get capture :attachments))))
    (dolist (niwa-working-note-max-age-days '(14 nil))
      (let ((capture (niwa-memory-notes-capture workspace nil)))
        (should (= 0 (plist-get capture :notes)))
        (should (= 0 (plist-get capture :attachments))))))
  :doc "a same-byte mtime refresh during snapshot preparation prevents stale admission"
  (progn
    (write-region "# Body\n" nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 14 86400))))
    (let ((original (symbol-function 'niwa-memory-scope--snapshot)) (snapshots 0) other-paths)
      (cl-letf (((symbol-function 'niwa-memory-scope--snapshot)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (if (equal (car args) path)
                         (progn (cl-incf snapshots) (set-file-times path (seconds-to-time now)))
                       (push (car args) other-paths))))))
        (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil) :notes))))
      (should (= 1 snapshots))
      (should other-paths)
      (should (cl-every (lambda (observed) (not (file-in-directory-p observed shared))) other-paths))))
  :doc "final capture bypasses attributes repopulated by snapshot mode reads"
  (progn
    (write-region "# Body\n" nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 14 86400))))
    (let* ((original (symbol-function 'niwa-memory-scope--snapshot))
           (snapshots 0) (mode-reads 0) other-paths (remote-file-name-inhibit-cache nil)
           (handler (test-niwa-working-note-files--cached-handler
                     path (file-attributes path) (directory-files-and-attributes shared nil directory-files-no-dot-files-regexp)
                     (lambda () (cl-incf mode-reads))))
           (file-name-handler-alist (cons (cons (concat "\\`" (regexp-quote shared)) handler) file-name-handler-alist)))
      (cl-letf (((symbol-function 'niwa-memory-scope--snapshot)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (if (equal (car args) path)
                         (progn
                           (cl-incf snapshots)
                           ;; The note's mode read repopulated the old cache entry.
                           (let ((file-name-handler-alist nil)) (set-file-times path (seconds-to-time now))))
                       (push (car args) other-paths))))))
        (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil) :notes))))
      (should (= 1 snapshots))
      (should other-paths)
      (should (cl-every (lambda (observed) (not (file-in-directory-p observed shared))) other-paths))
      (should (> mode-reads 0))
      (should (= (- now (* 14 86400)) (float-time (file-attribute-modification-time (file-attributes path)))))
      (let ((remote-file-name-inhibit-cache t))
        (should (= now (float-time (file-attribute-modification-time (file-attributes path))))))))
  :doc "unsaved visiting buffers prevent capture even when disk age is ready"
  (progn
    (write-region "# External\n" nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 14 86400))))
    (with-current-buffer (find-file-noselect path) (goto-char (point-max)) (insert "Unsaved"))
    (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil) :notes))))
  :doc "cohort admission excludes edited versions until a fresh cohort is frozen"
  (progn
    (write-region "# External\n" nil path nil 'silent)
    (set-file-times path (seconds-to-time (- now (* 14 86400))))
    (let ((cohort (niwa-memory-notes-cohort workspace nil)))
      (should (= 1 (plist-get (niwa-memory-notes-capture workspace nil :cohort cohort) :notes)))
      (write-region "# Changed external\n" nil path nil 'silent)
      (set-file-times path (seconds-to-time (- now (* 14 86400))))
      (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil :cohort cohort) :notes)))
      (should (= 1 (plist-get (niwa-memory-notes-capture
                               workspace nil :cohort (niwa-memory-notes-cohort workspace nil)) :notes)))))
  :doc "a live session beginning exact-file use during snapshot preparation prevents admission"
  (let ((reader (generate-new-buffer " *note-capture-reader*"))
        (original (symbol-function 'niwa-memory-scope--snapshot))
        (snapshots 0) other-paths)
    (unwind-protect
        (progn
          (write-region "# External\n" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- now (* 14 86400))))
          (with-current-buffer reader (setq-local niwa--session (niwa-session--create :workspace workspace)))
          (cl-letf (((symbol-function 'niwa-memory-scope--snapshot)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (if (equal (car args) path)
                             (progn
                               (cl-incf snapshots)
                               (niwa-resource-execute
                                (niwa-resource-prepare 'read "work://shared/note.md"
                                                          (list :session (buffer-local-value 'niwa--session reader)))
                                (lambda (_path _address) nil)))
                           (push (car args) other-paths))))))
            (should (= 0 (plist-get (niwa-memory-notes-capture workspace nil) :notes))))
          (should (= 1 snapshots))
          (should other-paths)
          (should (cl-every (lambda (observed) (not (file-in-directory-p observed shared))) other-paths)))
      (kill-buffer reader))))

(niwa-deftest niwa-working-note-files-cold-load ()
  (let* ((root (make-temp-file "niwa-note-discovery-cold-" t))
         (shared (file-name-concat root "shared"))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (make-directory shared)
          (write-region "# Cold heading\n"
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
                       (require 'niwa-working-note-files)
                       (let ((listing (niwa-working-note-files-listing ,shared "work://shared")))
                         (unless (and (string-search "work://shared/note.md\tage=" listing)
                                      (string-search "heading=Cold heading" listing))
                           (error "Cold discovery did not read the note")))
                       (dolist (feature '(niwa-memory-notes niwa-memory-decision
                                          niwa-memory-apply niwa-memory-store niwa-memory-pass))
                         (when (featurep feature) (error "Discovery loaded memory runtime: %s" feature)))))
                   (status (call-process invocation-name nil t nil "-Q" "--batch"
                                         "-L" test-niwa-working-note-files--source-directory
                                         "--eval" (prin1-to-string expression))))
              (ert-info ((buffer-string)) (should (zerop status)))
              (should (string-empty-p (buffer-string))))))
      (delete-directory root t))))

(provide 'test-niwa-working-note-files)
;;; test-niwa-working-note-files.el ends here
