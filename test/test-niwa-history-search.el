;;; test-niwa-history-search.el --- Saved conversation search -*- lexical-binding: t -*-

;;; Commentary:
;; Native saved sources through the authorized resource and tool interfaces.

;;; Code:

(require 'niwa-history-test-helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-history-test-helpers"))

(defun test-niwa-history-search--program-paths (operations lock-directory)
  "Return every target path named by control OPERATIONS or LOCK-DIRECTORY."
  (let ((paths (and lock-directory (list lock-directory))))
    (dolist (operation operations paths)
      (dolist (key '(:path :source :source-file))
        (when (stringp (plist-get operation key))
          (push (plist-get operation key) paths))))))

(defun test-niwa-history-search--path-under-p (path root)
  "Return non-nil when PATH is ROOT or lies beneath it."
  (string-prefix-p (file-name-as-directory (expand-file-name root))
                   (file-name-as-directory (expand-file-name path))))

(defun test-niwa-history-search--count-programs (sessions-directory thunk)
  "Call THUNK, classifying each control program by the paths it names.
Source programs name only SESSIONS-DIRECTORY paths.  Helper programs name
only paths in the scratch root the external helper most recently allocated.
Return a plist with `:source' and `:helper' counts, allocated `:scratches',
`:other' operations for any program outside both or mixing them, and THUNK's
`:value'.  The classification is by location, so publication writes in the
source sessions count against the source budget whatever their verb."
  (let ((native-program (symbol-function 'niwa-session-control-fs-run-program))
        (native-temp (symbol-function 'make-temp-file))
        (source 0)
        (helper 0)
        scratches other value)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (prefix &optional dir-flag suffix text)
                 (let ((file (funcall native-temp prefix dir-flag suffix text)))
                   (when (and dir-flag (equal prefix "niwa-helper-"))
                     (push file scratches))
                   file)))
              ((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &optional lock-directory)
                 (let ((paths (test-niwa-history-search--program-paths
                               operations lock-directory)))
                   (cond
                    ((and paths
                          (cl-every (lambda (path)
                                      (test-niwa-history-search--path-under-p
                                       path sessions-directory))
                                    paths))
                     (cl-incf source))
                    ((and paths scratches
                          (cl-every (lambda (path)
                                      (test-niwa-history-search--path-under-p
                                       path (car scratches)))
                                    paths))
                     (cl-incf helper))
                    (t (push operations other))))
                 (funcall native-program operations lock-directory))))
      (setq value (funcall thunk)))
    (list :source source :helper helper :scratches (nreverse scratches)
          :other (nreverse other) :value value)))

(defun test-niwa-history-search--check-batched-query
    (sessions-directory query source-budget seen-scratches)
  "Run QUERY once and check its programs against SOURCE-BUDGET.
QUERY returns the search result text.  Each query must find every canary,
stay within the source budget and pay for one fresh helper scratch root not
in SEEN-SCRATCHES: one creation program and one cleanup program.  Return
SEEN-SCRATCHES extended with this query's root."
  (let* ((counts (test-niwa-history-search--count-programs
                  sessions-directory query))
         (text (plist-get counts :value))
         (scratches (plist-get counts :scratches)))
    (dotimes (index 6)
      (should (string-search (format "canary-%d" index) text)))
    (should-not (plist-get counts :other))
    (should (<= (plist-get counts :source) source-budget))
    (should (= 1 (length scratches)))
    (should-not (member (car scratches) seen-scratches))
    (should-not (file-exists-p (car scratches)))
    (should (= 2 (plist-get counts :helper)))
    (append seen-scratches scratches)))

(niwa-deftest niwa-history-search-start (:quiet t)
  ,test
  (test)
  :doc "searches saved workspace conversations asynchronously with canonical filtering"
  (let* ((root (make-temp-file "niwa-history-search-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace root))
         (session (niwa-session-create "source" workspace))
         (buffer (generate-new-buffer " *history-source*"))
         (address "history://saved")
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         done result cancel)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (niwa--transcript-org-mode)
            (setq-local niwa--session session)
            (niwa-transcript-enable-gptel-mode)
            (insert "User needle and literal <system-reminder>example</system-reminder>.\n")
            (insert (propertize "Assistant needle.\n" 'gptel 'response))
            (insert (propertize "\n<system-reminder>\nPrivate needle.\n</system-reminder>\n"
                                'gptel 'ignore))
            (niwa-session-artifacts-save session buffer))
          (let* ((attempt (niwa-resource-prepare
                           'grep address (list :workspace workspace)))
                 (niwa-resource-current-attempts (list (cons address attempt))))
            (setq cancel
                  (niwa-tool-fs-search-grep
                   (lambda (value) (setq result value done t))
                   (list :path address :pattern "needle" :output_mode "content"))))
          (should (functionp cancel))
          (should-not done)
          (with-timeout (10 (ert-fail "History callback timed out"))
            (while (not done) (accept-process-output nil 0.01)))
          (let ((text (plist-get result :result)))
            (should (string-search "User needle" text))
            (should (string-search "Assistant needle" text))
            (should (string-search "<system-reminder>example</system-reminder>" text))
            (should-not (string-search "Private needle" text))
            (should (string-match-p "history://saved/[^/\n]+/segment-0001.chat.org:[0-9]+:" text))
            (should-not (string-search root text)))
          (let* ((source (concat "history://saved/"
                                 (niwa-resource-encode-component
                                  (file-name-nondirectory
                                   (directory-file-name (niwa-session-save-path session))))
                                 "/segment-0001.chat.org"))
                 (attempt (niwa-resource-prepare 'read source (list :workspace workspace)))
                 (niwa-resource-current-attempts (list (cons source attempt))))
            (setq done nil
                  cancel (niwa-tool-fs-read
                          (lambda (value) (setq result value done t))
                          (list :file_path source :offset 1 :limit 30)))
            (should-not done)
            (with-timeout (10 (ert-fail "History Read timed out"))
              (while (not done) (accept-process-output nil 0.01)))
            (should (string-search "provenance: assistant" (plist-get result :result)))
            (should-not (string-search "Private needle" (plist-get result :result)))))
      (when cancel (funcall cancel))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/tool-round-trip (:quiet t)
  ,test
  (test)
  :doc "round-trips saved Bash and quoted ToolCall metadata through history Read"
  ;; The published 2026-10-19T16-39-e2fc4dd69926 segment contains these
  ;; shapes: response, blank separator, Org header, Lisp call, result.  Its
  ;; canonical tool spans include the separator before the header.  Keep
  ;; escaped quotes and a quoted ToolCall expression, but use small results.
  (let* ((root (make-temp-file "niwa-history-tools-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         (skill-body (concat "## Instructions\n" (make-string 200 ?x)
                             "\nPreserve existing user edits."))
         (source
          (test-niwa-history-search--source
           workspace "tools"
           (concat
            "Inspect the workspace.\n\n"
            (propertize "Inspecting the workspace." 'gptel 'response)
            "\n\n#+begin_tool (Bash :command \"printf ... )\n"
            (propertize
             (concat "(:name \"Bash\" :args (:command \"printf \\\"ok\\\"\""
                     " :description \"Inspect workspace\"))\n\n"
                     "workspace-result\n")
             'gptel '(tool . "toolu-bash"))
            "#+end_tool\n\n"
            (propertize "Loading the skill." 'gptel 'response)
            "\n\n#+begin_tool (ToolCall :expression \"(Skill ... )\n"
            (propertize
             (concat "(:name \"ToolCall\" :args (:expression "
                     "\"(Skill :name \\\"careful-edit\\\")\"))\n\n"
                     skill-body "\n")
             'gptel '(tool . "toolu-skill"))
            "#+end_tool\n")))
         (session (car source))
         (buffer (cdr source)))
    (unwind-protect
        (progn
          ;; Kill the producer: Read must use published bytes, restoration,
          ;; canonical classification and evidence projection, not live state.
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (let* ((address (concat "history://saved/"
                                  (niwa-session-session-id session)
                                  "/segment-0001.chat.org"))
                 (result (test-niwa-history-search--query
                          workspace 'read
                          (list :file_path address :offset 1 :limit 200)))
                 (text (plist-get result :result)))
            (should-not (plist-get result :error))
            (should-not (string-search "unparseable tool-call metadata" text))
            (should-not (string-search "#+begin_tool" text))
            (should-not (string-search "#+end_tool" text))
            (should (string-search
                     "(:name \"Bash\" :args (:command \"printf \\\"ok\\\"\"" text))
            (should (string-search "workspace-result" text))
            (should (string-search
                     "(:name \"ToolCall\" :args (:expression \"(Skill :name \\\"careful-edit\\\")\"))"
                     text))
            (dolist (line (split-string skill-body "\n"))
              (should (string-search line text)))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/handler-context (:quiet t)
  ,test
  (test)
  :doc "timer continuations preserve the requesting buffer and handler context"
  (let* ((root (make-temp-file "niwa-history-context-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace root))
         (source (test-niwa-history-search--source workspace "context" "Context needle.\n"))
         (session (car source))
         (owner (cdr source))
         (other (generate-new-buffer " *history-unrelated*"))
         (request (niwa-request--create :origin "/root" :session session))
         (native-helper (symbol-function 'niwa-execution-start-helper))
         calls helper-result result cancel)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-execution-start-helper)
                   (lambda (&rest args)
                     (push (list (current-buffer) niwa-pipeline--active-tool-use-id)
                           calls)
                     (let ((callback (car args)))
                       (setcar args (lambda (value)
                                      (setq helper-result value)
                                      (funcall callback value))))
                     (apply native-helper args))))
          (with-current-buffer owner
            (setq-local niwa--current-request request)
            (let* ((niwa-pipeline--active-tool-use-id "history-context")
                   (attempt (niwa-resource-prepare
                             'glob "history://saved" (list :workspace workspace)))
                   (niwa-resource-current-attempts
                    (list (cons "history://saved" attempt))))
              (setq cancel
                    (niwa-tool-fs-search-glob
                     (lambda (value) (setq result value))
                     '(:path "history://saved" :pattern "*")))))
          ;; Emacs timers run in whichever buffer is current when they fire.
          ;; Do not keep the request owner current while awaiting preparation.
          (with-current-buffer other
            (with-timeout (10 (ert-fail "History context callback timed out"))
              (while (not result) (accept-process-output nil 0.01))))
          (ert-info ((format "Helper contexts: %S; helper error: %S; result: %S"
                             (mapcar (lambda (call)
                                       (cons (buffer-name (car call)) (cdr call)))
                                     calls)
                             (plist-get helper-result :error)
                             result))
            (should (equal calls (list (list owner "history-context")))))
          (should-not (plist-get helper-result :error))
          (should (string-search "segment-0001.chat.org" (plist-get result :result)))
          (should-not (string-search "Error:" (plist-get result :result))))
      (when cancel (funcall cancel))
      (niwa-request-drain-cancellers request)
      (when (buffer-live-p other) (kill-buffer other))
      (test-niwa-session-persistence--release-and-kill owner session)
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "local projection helpers honor a remote requesting session's sandbox preference"
  (let* ((root (make-temp-file "niwa-history-sandbox-context-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace root))
         (source (test-niwa-history-search--source workspace "policy" "Policy needle.\n"))
         (owner (generate-new-buffer " *history-policy-owner*"))
         (session (niwa-session--create
                   :workspace workspace :permission-mode 'ask :sandbox-mode 'off
                   :execution-target (niwa-execution-target-create
                                      "/ssh:history.invalid:/workspace/")))
         (request (niwa-request--create :origin "/root" :session session))
         (niwa-sandbox-mode 'required)
         (native-prepare (symbol-function 'niwa-sandbox-prepare-start))
         (native-process (symbol-function 'niwa-execution--start-process))
         modes launches result cancel)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-sandbox-prepare-start)
                   (lambda (&rest args)
                     (push (nth 7 args) modes)
                     (apply native-prepare args)))
                  ((symbol-function 'niwa-execution--start-process)
                   (lambda (callback name command workdir timeout helper-session origin teardown)
                     (push (list (file-remote-p workdir) helper-session) launches)
                     (funcall native-process callback name command workdir timeout
                              helper-session origin teardown))))
          (with-current-buffer owner
            (setq-local niwa--session session niwa--current-request request)
            (let* ((attempt (niwa-resource-prepare
                             'glob "history://saved" (list :workspace workspace)))
                   (niwa-resource-current-attempts
                    (list (cons "history://saved" attempt))))
              (setq cancel
                    (niwa-tool-fs-search-glob
                     (lambda (value) (setq result value))
                     '(:path "history://saved" :pattern "*")))))
          (with-timeout (10 (ert-fail "History policy callback timed out"))
            (while (not result) (accept-process-output nil 0.01)))
          (should (equal modes '(off)))
          (should (equal launches '((nil nil))))
          (should (string-search "segment-0001.chat.org" (plist-get result :result)))
          (should-not (string-search "Error:" (plist-get result :result))))
      (when cancel (funcall cancel))
      (niwa-request-drain-cancellers request)
      (when (buffer-live-p owner) (kill-buffer owner))
      (test-niwa-session-persistence--release-and-kill (cdr source) (car source))
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search--prepare (:quiet t)
  ,test
  (test)
  :doc "reads current and archived native PID and portable sources without live views"
  (dolist (portable '(nil t))
    (let* ((root (make-temp-file "niwa-history-profiles-" t))
           (workspace (if portable
                          (test-niwa-session-persistence--make-workspace root)
                        (test-niwa-session-persistence--make-file-workspace root)))
           (niwa-history-search--cache (make-hash-table :test #'equal))
           (niwa-history-search--cache-bytes 0)
           (source (test-niwa-history-search--source
                    workspace "first"
                    (concat "Original user fact.\n"
                            (propertize "Original assistant fact.\n" 'gptel 'response)
                            (propertize "\n<system-reminder>\nPRIVATE\n</system-reminder>\n" 'gptel 'ignore))))
           (other (test-niwa-history-search--source workspace "second" "Other session fact.\n"))
           (session (car source))
           (buffer (cdr source)))
      (unwind-protect
          (progn
            (niwa-session-artifacts-rotate-segment session buffer "Inherited summary")
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "Current user fact.\n" (propertize "Current assistant fact.\n" 'gptel 'response))
              (niwa-session-artifacts-save session buffer)
              (set-buffer-modified-p nil))
            (kill-buffer buffer)
            (let* ((listing (plist-get (test-niwa-history-search--query
                                        workspace 'glob '(:path "history://saved" :pattern "**/*.org")) :result))
                   (addresses (split-string listing "\n" t))
                   (selected (seq-find (lambda (address)
                                         (string-search (concat "/" (niwa-session-session-id session) "/") address)) addresses)))
              (should (= 3 (length addresses)))
              (dolist (address addresses)
                (let ((text (plist-get (test-niwa-history-search--query
                                        workspace 'read (list :file_path address :limit 40)) :result)))
                  (should-not (string-search "PRIVATE" text))
                  (when (and (string-search (concat "/" (niwa-session-session-id session) "/") address)
                             (string-suffix-p "segment-0001.chat.org" address))
                    (should (string-search "Original assistant fact." text))
                    (should (string-search "provenance: assistant" text)))))
              (let ((text (plist-get (test-niwa-history-search--query
                                      workspace 'grep (list :path selected :pattern "fact" :output_mode "content")) :result)))
                (should (string-search selected text))
                (should-not (string-search "Other session" text)))
              (let ((scope (substring selected 0 (string-match "/segment-" selected))))
                (should (= 2 (length (split-string
                                      (plist-get (test-niwa-history-search--query
                                                  workspace 'glob (list :path scope :pattern "**/*.org")) :result)
                                      "\n" t)))))))
        (test-niwa-session-persistence--release-and-kill (cdr source) (car source))
        (test-niwa-session-persistence--release-and-kill (cdr other) (car other))
        (niwa-execution-teardown-all)
        (delete-directory root t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-history-search--remember ()
  ,test
  (test)
  :doc "bounds disposable payload, replaces keys and skips oversized entries"
  (let ((niwa-history-search--cache (make-hash-table :test #'equal))
        (niwa-history-search--cache-bytes 0)
        (niwa-history-search--cache-limit 5))
    (niwa-history-search--remember "one" "a" "123")
    (niwa-history-search--remember "one" "b" "12")
    (should (= 2 niwa-history-search--cache-bytes))
    (should (equal '("b" . "12") (gethash "one" niwa-history-search--cache)))
    (niwa-history-search--remember "two" "c" "4567")
    (should (= 4 niwa-history-search--cache-bytes))
    (should-not (gethash "one" niwa-history-search--cache))
    (niwa-history-search--remember "large" "d" "123456")
    (should (= 0 niwa-history-search--cache-bytes))
    (should (= 0 (hash-table-count niwa-history-search--cache)))))

(niwa-deftest niwa-history-search-start/freshness (:quiet t)
  ,test
  (test)
  :doc "warm projections cannot hide same-size edits, redirected sources or changed authority"
  (let* ((root (make-temp-file "niwa-history-fresh-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace root))
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         (source (test-niwa-history-search--source workspace "fresh" "Observed version ONE.\n"))
         (session (car source))
         (directory (niwa-session-save-path session))
         (file (file-name-concat directory "segment-0001.chat.org"))
         (args '(:path "history://saved" :pattern "version" :output_mode "content")))
    (unwind-protect
        (progn
          (should (string-search "version ONE" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          (should (= 1 (hash-table-count niwa-history-search--cache)))
          (with-current-buffer (cdr source)
            (goto-char (point-min))
            (search-forward "ONE")
            (replace-match "TWO" t t)
            (niwa-session-artifacts-save session (current-buffer)))
          (should (string-search "version TWO" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          ;; Fixed caches cannot replace the committed transcript.
          (write-region "Observed version BAD.\n" nil file nil 'silent)
          (should (string-search "version TWO" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          (let* ((publication (niwa-session-publication-read directory))
                 (published (plist-get (cdr (assoc "segment-0001.chat.org"
                                                   (plist-get publication :artifacts)))
                                       :published))
                 (stamp (file-attribute-modification-time (file-attributes published)))
                 (bytes (with-temp-buffer
                          (insert-file-contents-literally published)
                          (buffer-string))))
            (unwind-protect
                (progn
                  (with-temp-buffer
                    (insert bytes)
                    (goto-char (point-min))
                    (search-forward "TWO")
                    (replace-match "BAD" t t)
                    (write-region (point-min) (point-max) published nil 'silent))
                  (set-file-times published stamp)
                  (should (= (string-bytes bytes)
                             (file-attribute-size (file-attributes published))))
                  (let ((result (test-niwa-history-search--query workspace 'grep args)))
                    (should (eq 'error (plist-get result :status)))
                    (should-not (string-search "version TWO" (plist-get result :result)))))
              (let ((coding-system-for-write 'no-conversion))
                (write-region bytes nil published nil 'silent)))
            (let ((moved (concat published ".moved")))
              (rename-file published moved)
              (make-symbolic-link moved published)
              (unwind-protect
                  (should-not (string-search "version TWO" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
                (delete-file published)
                (rename-file moved published))))
          ;; A mixed authority directory must never be used as saved history.
          (write-region "legacy lock" nil (file-name-concat directory ".lock") nil 'silent)
          (should-not (string-search "version" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          (delete-file (file-name-concat directory ".lock"))
          (let ((moved (file-name-concat root "moved-session")))
            (rename-file (directory-file-name directory) moved)
            (make-symbolic-link moved (directory-file-name directory))
            (unwind-protect
                (should-not (string-search "version TWO" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
              (delete-file (directory-file-name directory))
              (rename-file moved (directory-file-name directory)))))
      (test-niwa-session-persistence--release-and-kill (cdr source) session)
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/portable-freshness (:quiet t)
  ,test
  (test)
  :doc "revalidates new portable publications and rejects corrupt committed bytes"
  (let* ((root (make-temp-file "niwa-history-publication-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         (source (test-niwa-history-search--source workspace "portable" "Published fact ONE.\n"))
         (session (car source))
         (args '(:path "history://saved" :pattern "fact" :output_mode "content")))
    (unwind-protect
        (progn
          (should (string-search "fact ONE" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          (with-current-buffer (cdr source)
            (goto-char (point-min))
            (search-forward "ONE")
            (replace-match "TWO" t t)
            (niwa-session-artifacts-save session (current-buffer)))
          (should (string-search "fact TWO" (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          (let* ((publication (niwa-session-publication-read (niwa-session-save-path session)))
                 (path (plist-get (cdr (assoc "segment-0001.chat.org" (plist-get publication :artifacts))) :published)))
            (write-region "Corrupt fact THREE" nil path nil 'silent)
            (let ((result (test-niwa-history-search--query workspace 'grep args)))
              (should (eq 'error (plist-get result :status)))
              (should (string-search "verification" (plist-get result :result)))
              (should-not (string-search "fact TWO" (plist-get result :result)))
              (should-not (string-search root (plist-get result :result))))))
      (test-niwa-session-persistence--release-and-kill (cdr source) session)
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/cancellation (:quiet t)
  ,test
  (test)
  :doc "cancels every preparation phase and helper launch without late results or files"
  (let* ((root (make-temp-file "niwa-history-cancel-" t))
         (temporary-file-directory (file-name-as-directory root))
         (workspace (test-niwa-session-persistence--make-file-workspace root))
         (source (test-niwa-history-search--source workspace "cancel" "Cancellation needle.\n"))
         (native-next (symbol-function 'iter-next))
         (native-helper (symbol-function 'niwa-execution-start-helper))
         (native-process (symbol-function 'make-process)))
    (unwind-protect
        (dolist (phase '(discovery reads restoration projection temporary-preparation search))
          (let* ((niwa-history-search--cache (make-hash-table :test #'equal))
                 (niwa-history-search--cache-bytes 0)
                 (before (directory-files temporary-file-directory nil "^niwa-history-search-"))
                 (attempt (niwa-resource-prepare 'grep "history://saved" (list :workspace workspace)))
                 (niwa-resource-current-attempts (list (cons "history://saved" attempt)))
                 (deadline (+ (float-time) 5))
                 cancel reached results child)
            (unwind-protect
                (cl-letf (((symbol-function 'iter-next)
                           (lambda (&rest args)
                             (let ((value (apply native-next args)))
                               (when (and (eq value phase) (not reached))
                                 (setq reached (float-time))
                                 (funcall cancel))
                               value)))
                          ((symbol-function 'make-process)
                           (lambda (&rest args)
                             (let ((process (apply native-process args)))
                               (when (string-search "niwa-grep" (or (plist-get args :name) ""))
                                 (setq child process))
                               process)))
                          ((symbol-function 'niwa-execution-start-helper)
                           (lambda (callback name command paths roots &rest keys)
                             (let ((handle (apply native-helper callback name
                                                  (if (eq phase 'search)
                                                      (append '("sh" "-c" "sleep 2; exec \"$@\"" "history-test") command)
                                                    command)
                                                  paths roots keys)))
                               (when (eq phase 'search)
                                 (setq reached (float-time))
                                 (funcall cancel))
                               handle))))
                         (setq cancel (niwa-tool-fs-search-grep
                                       (lambda (value) (push (cons (float-time) value) results))
                                       '(:path "history://saved" :pattern "needle" :output_mode "content")))
                         (while (and (not results) (< (float-time) deadline))
                           (accept-process-output nil 0.01))
                         (should reached)
                         (should (= 1 (length results)))
                         (should (eq 'cancelled (plist-get (cdar results) :status)))
                         (should (< (- (caar results) reached) 1))
                         (accept-process-output nil 0.15)
                         (should (= 1 (length results)))
                         (should-not (and child (process-live-p child)))
                         (should (equal before (directory-files temporary-file-directory nil "^niwa-history-search-"))))
              (when cancel (funcall cancel))
              (niwa-execution-teardown-all))))
      (test-niwa-session-persistence--release-and-kill (cdr source) (car source))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/owner-close (:quiet t)
  ,test
  (test)
  :doc "owner buffer closure, request teardown and terminal agents suppress queued delivery"
  (dolist (kind '(buffer request agent))
    (let* ((root (make-temp-file "niwa-history-owner-" t))
           (temporary-file-directory (file-name-as-directory root))
           (workspace (test-niwa-session-persistence--make-file-workspace root))
           (owner (generate-new-buffer " *history-owner*"))
           (request (niwa-request--create :origin "/root"))
           (invocation (niwa-agent-invocation--create))
           (before (directory-files temporary-file-directory nil "^niwa-history-search-"))
           result cancel)
      (unwind-protect
          (progn
            (with-current-buffer owner
              (setq-local niwa--current-request request)
              (setq-local niwa--agent-invocation invocation)
              (let* ((attempt (niwa-resource-prepare 'grep "history://saved" (list :workspace workspace)))
                     (niwa-resource-current-attempts (list (cons "history://saved" attempt))))
                (setq cancel (niwa-tool-fs-search-grep
                              (lambda (value) (setq result value))
                              '(:path "history://saved" :pattern "needle")))))
            (pcase kind
              ('buffer (kill-buffer owner))
              ('request (niwa-request-drain-cancellers request))
              ('agent (setf (niwa-agent-invocation-runtime-settled-p invocation) t)))
            (accept-process-output nil 0.05)
            (should-not result)
            (should (equal before (directory-files temporary-file-directory nil "^niwa-history-search-"))))
        (when cancel (funcall cancel))
        (niwa-request-drain-cancellers request)
        (when (buffer-live-p owner) (kill-buffer owner))
        (niwa-execution-teardown-all)
        (delete-directory root t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-resource-prepare/saved-history ()
  ,test
  (test)
  :doc "validates scope before access and leaves discovery to the authorized handler"
  (let* ((root (make-temp-file "niwa-history-prepare-" t))
         (workspace (niwa-workspace--create :type 'project :root root))
         (native (symbol-function 'niwa-session-control-fs-run-program))
         touched)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                   (lambda (&rest args) (setq touched t) (apply native args))))
                 (should (plist-get (niwa-resource-parse-address "history://saved") :dynamic-p))
                 (should (eq 'workspace-relative
                             (plist-get (niwa-resource-parse-address "history://saved/session") :locator-class)))
                 (dolist (address '("history://saved" "history://saved/session"
                                    "history://saved/session/segment-0001.chat.org"))
                   (dolist (operation '(read glob grep))
                     (let ((attempt (niwa-resource-prepare operation address (list :workspace workspace))))
                       (should attempt)
                       (niwa-resource-discard-attempts (list attempt))))
                   (should-error (niwa-resource-prepare 'apply-patch address (list :workspace workspace))))
                 (dolist (address '("history://saved/a/../b" "history://saved/a/.lock"
                                    "history://saved/a/segment-1.chat.org" "history://saved/a/b/c"
                                    "history://saved/a%2Fb" "history://saved/a#fragment"))
                   (should-error (niwa-resource-prepare 'read address (list :workspace workspace))))
                 (should-not touched))
      (delete-directory root t))))

(niwa-deftest niwa-session-publication-read-batch (:quiet t)
  ,test
  (test)
  :doc "batches portable publication observations without losing source evidence"
  (let* ((root (make-temp-file "niwa-history-batch-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         pairs scratches)
    (unwind-protect
        (progn
          (dotimes (index 6)
            (push (test-niwa-history-search--source
                   workspace (format "source-%d" index)
                   (format "Batch evidence canary-%d\n" index)) pairs))
          ;; Two discovery programs, three publication stages, one pinned
          ;; transcript batch: every query, including the cold first one.
          (dotimes (_ 3)
            (setq scratches
                  (test-niwa-history-search--check-batched-query
                   (file-name-directory
                    (directory-file-name
                     (niwa-session-save-path (caar pairs))))
                   (lambda ()
                     (plist-get
                      (test-niwa-history-search--query
                       workspace 'grep
                       '(:path "history://saved" :pattern "Batch evidence"
                               :output_mode "content"))
                      :result))
                   6 scratches))))
      (dolist (pair pairs)
        (when (buffer-live-p (cdr pair))
          (with-current-buffer (cdr pair) (set-buffer-modified-p nil))
          (kill-buffer (cdr pair)))
        (niwa-session-persistence-lock-release
         (niwa-session-save-path (car pair)) (car pair)))
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/publication-boundaries (:quiet t)
  ,test
  (test)
  :doc "keeps a captured publication coherent and rejects unpublished or invalid evidence"
  (let* ((root (make-temp-file "niwa-history-boundaries-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         (source (test-niwa-history-search--source workspace "changing" "Evidence OLD\n"))
         (other (test-niwa-history-search--source workspace "stable" "Evidence STABLE\n"))
         (session (car source))
         (directory (niwa-session-save-path session))
         (native-next (symbol-function 'iter-next))
         (args '(:path "history://saved" :pattern "Evidence" :output_mode "content"))
         published)
    (unwind-protect
        (progn
          (let ((result
                 (cl-letf (((symbol-function 'iter-next)
                            (lambda (&rest arguments)
                              (let ((value (apply native-next arguments)))
                                (when (and (eq value 'reads) (not published))
                                  (setq published t)
                                  (with-current-buffer (cdr source)
                                    (goto-char (point-min))
                                    (search-forward "OLD")
                                    (replace-match "NEW" t t)
                                    (niwa-session-artifacts-save session (current-buffer))))
                                value))))
                          (test-niwa-history-search--query workspace 'grep args))))
            (should published)
            (should (string-search "Evidence OLD" (plist-get result :result)))
            (should-not (string-search "Evidence NEW" (plist-get result :result))))
          (should (string-search "Evidence NEW"
                                 (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          ;; A loose transcript has never been committed into the publication.
          (write-region "Evidence UNPUBLISHED" nil
                        (file-name-concat directory "segment-9999.chat.org") nil 'silent)
          (should-not (string-search "UNPUBLISHED"
                                     (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
          ;; Corrupt control data cannot recover the warm projection. A broken
          ;; candidate also must not prevent other valid sessions being searched.
          (let* ((head (plist-get (niwa-session-publication session) :head))
                 (manifest (file-name-concat directory head)))
            (write-region "(:broken t)" nil manifest nil 'silent)
            (let ((text (plist-get (test-niwa-history-search--query workspace 'grep args) :result)))
              (should (string-search "Evidence STABLE" text))
              (should (string-search "Skipped 1 unavailable or incompatible" text))
              (should-not (string-search "Evidence NEW" text))
              (should-not (string-search root text)))))
      (dolist (pair (list source other))
        (test-niwa-session-persistence--release-and-kill (cdr pair) (car pair)))
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/helper-owner-close (:quiet t)
  ,test
  (test)
  :doc "owner close at helper launch releases the real child without normal delivery"
  (dolist (kind '(buffer request))
    (let* ((root (make-temp-file "niwa-history-helper-owner-" t))
           (temporary-file-directory (file-name-as-directory root))
           (workspace (test-niwa-session-persistence--make-file-workspace root))
           (owner (generate-new-buffer " *history-helper-owner*"))
           (request (niwa-request--create :origin "/root"))
           (native-helper (symbol-function 'niwa-execution-start-helper))
           (native-process (symbol-function 'make-process))
           (before (directory-files temporary-file-directory nil "^niwa-history-search-"))
           child reached result cancel)
      (unwind-protect
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (let ((process (apply native-process args)))
                         (when (string-search "niwa-grep" (or (plist-get args :name) ""))
                           (setq child process))
                         process)))
                    ((symbol-function 'niwa-execution-start-helper)
                     (lambda (callback name command paths roots &rest keys)
                       (let ((handle (apply native-helper callback name
                                            (append '("sh" "-c" "sleep 2; exec \"$@\"" "history-test") command)
                                            paths roots keys)))
                         ;; Preparation can yield before the actual helper
                         ;; exists.  This case closes the owner at launch;
                         ;; discovery cancellation has separate coverage.
                         (with-timeout (5 (ert-fail "History helper did not start"))
                           (while (not child) (accept-process-output nil 0.01)))
                         (setq reached t)
                         (if (eq kind 'buffer) (kill-buffer owner)
                           (niwa-request-drain-cancellers request))
                         handle))))
                   (with-current-buffer owner
                     (setq-local niwa--current-request request)
                     (let* ((attempt (niwa-resource-prepare 'grep "history://saved" (list :workspace workspace)))
                            (niwa-resource-current-attempts (list (cons "history://saved" attempt))))
                       (setq cancel (niwa-tool-fs-search-grep
                                     (lambda (value) (setq result value))
                                     '(:path "history://saved" :pattern "needle")))))
                   (with-timeout (5 (ert-fail "History helper did not launch"))
                     (while (not reached) (accept-process-output nil 0.01)))
                   (accept-process-output nil 0.15)
                   (should child)
                   (should-not (process-live-p child))
                   (should-not result)
                   (should (equal before (directory-files temporary-file-directory nil "^niwa-history-search-"))))
        (when cancel (funcall cancel))
        (niwa-request-drain-cancellers request)
        (when (buffer-live-p owner) (kill-buffer owner))
        (niwa-execution-teardown-all)
        (delete-directory root t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-history-search-start/selected-pagination (:quiet t)
  ,test
  (test)
  :doc "selected source pagination shares Read line references and workspace caches stay isolated"
  (let* ((root (make-temp-file "niwa-history-pages-" t))
         (one-root (file-name-concat root "one"))
         (two-root (file-name-concat root "two"))
         (_ (make-directory one-root))
         (_ (make-directory two-root))
         (one (test-niwa-session-persistence--make-file-workspace one-root))
         (two (test-niwa-session-persistence--make-file-workspace two-root))
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         (first (test-niwa-history-search--source
                 one "source" (mapconcat (lambda (index) (format "Pagination A-%d\n" index))
                                         (number-sequence 0 9) "")))
         (second (test-niwa-history-search--source two "source" "Pagination B-private\n"))
         (address (concat "history://saved/"
                          (niwa-resource-encode-component
                           (file-name-nondirectory (directory-file-name (niwa-session-save-path (car first)))))
                          "/segment-0001.chat.org"))
         (sessions-root (directory-file-name (niwa-session-artifacts-sessions-dir one)))
         (native-list (symbol-function 'directory-files)))
    (unwind-protect
        (progn
          (dolist (glob '("segment-*.chat.org" "segment-0001.chat.org"))
            (dolist (scope (list "history://saved" address))
              (should
               (string-search
                "Pagination A-0"
                (plist-get
                 (test-niwa-history-search--query
                  one 'grep (list :path scope :pattern "Pagination" :glob glob
                                  :output_mode "content" :head_limit 1))
                 :result)))))
          (cl-letf (((symbol-function 'directory-files)
                     (lambda (directory &rest args)
                       (when (equal (directory-file-name directory) sessions-root)
                         (ert-fail "Selected source enumerated every session"))
                       (apply native-list directory args))))
                   (let* ((text (plist-get
                                 (test-niwa-history-search--query
                                  one 'grep (list :path address :pattern "Pagination" :glob "*.org"
                                                  :output_mode "content" :head_limit 2 :offset 3)) :result))
                          (line (and (string-match (concat (regexp-quote address) ":\\([0-9]+\\):Pagination A-3") text)
                                     (string-to-number (match-string 1 text)))))
                     (should line)
                     (should (string-search "Pagination A-4" text))
                     (should-not (string-search "Pagination A-2" text))
                     (should-not (string-search "Pagination A-5" text))
                     (should (string-search address text))
                     (should (string-search
                              "Pagination A-3"
                              (plist-get (test-niwa-history-search--query
                                          one 'read (list :file_path address :offset line :limit 1)) :result)))))
          (let ((text (plist-get (test-niwa-history-search--query
                                  two 'grep '(:path "history://saved" :pattern "Pagination" :output_mode "content")) :result)))
            (should (string-search "Pagination B-private" text))
            (should-not (string-search "Pagination A-" text)))
          (let ((text (plist-get (test-niwa-history-search--query
                                  one 'grep '(:path "history://saved" :pattern "Pagination" :output_mode "content")) :result)))
            (should (string-search "Pagination A-0" text))
            (should-not (string-search "Pagination B-private" text))))
      (dolist (pair (list first second))
        (test-niwa-session-persistence--release-and-kill (cdr pair) (car pair)))
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-history-search-start/global-format-bound (:quiet t)
  ,test
  (test)
  :doc "formats only the requested global content page across dense sources"
  (let* ((root (make-temp-file "niwa-history-global-page-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace root))
         (niwa-history-search--cache (make-hash-table :test #'equal))
         (niwa-history-search--cache-bytes 0)
         (native-rewrite (symbol-function 'niwa-tool-fs-resource-child-address))
         (rewrites 0)
         sources)
    (unwind-protect
        (progn
          (dotimes (index 6)
            (push (test-niwa-history-search--source
                   workspace (format "dense-%d" index)
                   (mapconcat (lambda (line) (format "Dense evidence %d/%d\n" index line))
                              (number-sequence 0 19) "")) sources))
          (cl-letf (((symbol-function 'niwa-tool-fs-resource-child-address)
                     (lambda (&rest args)
                       (cl-incf rewrites)
                       (apply native-rewrite args))))
            (let ((text (plist-get
                         (test-niwa-history-search--query
                          workspace 'grep
                          '(:path "history://saved" :pattern "Dense evidence"
                            :output_mode "content" :offset 1 :head_limit 2))
                         :result)))
              ;; Native search may visit any source first.  Identify it from
              ;; the returned page rather than imposing a directory order.
              (should (string-match
                       "^\\(history://saved/[^\n]+\\):[0-9]+:Dense evidence \\([0-5]\\)/1$"
                       text))
              (let* ((address (match-string 1 text))
                     (index (match-string 2 text))
                     (source (seq-find
                              (lambda (entry)
                                (equal (niwa-session-name (car entry))
                                       (concat "dense-" index)))
                              sources)))
                (should (equal address
                               (concat "history://saved/"
                                       (niwa-resource-encode-component
                                        (file-name-nondirectory
                                         (directory-file-name
                                          (niwa-session-save-path (car source)))))
                                       "/segment-0001.chat.org")))
                (should (string-match-p
                         (concat "^" (regexp-quote address)
                                 ":[0-9]+:Dense evidence " index "/2$")
                         text))
                (should-not (string-search (format "Dense evidence %s/0" index) text))
                (should-not (string-search (format "Dense evidence %s/3" index) text)))
              (should (= 2 (length (seq-filter
                                    (lambda (line) (string-prefix-p "history://saved/" line))
                                    (split-string text "\n" t)))))
              (should (string-search "Results truncated (limit: 2, offset: 1)" text))
              ;; Two page lines, one skipped line, one truncation sentinel.
              (should (<= rewrites 4)))))
      (dolist (source sources)
        (test-niwa-session-persistence--release-and-kill (cdr source) (car source)))
      (niwa-execution-teardown-all)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-history-search)
;;; test-niwa-history-search.el ends here
