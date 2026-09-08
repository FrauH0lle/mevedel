;;; run.el -- Disposable real-model scope experiment -*- lexical-binding: t -*-

;;; Commentary:
;; Opt-in experiment only. See README.md for the frozen protocol.

;;; Code:

(require 'helpers (file-name-concat (locate-dominating-file load-file-name "Eask") "test" "helpers"))
(require 'gptel-openai-extras)
(require 'gptel-openai-oauth)
(require 'mevedel-tool-patch)

(defvar scope--nested-p nil
  "Whether this case exposes project storage at local://shared/.")

(defvar scope--freeform-p nil
  "Whether this case uses work:// with agent-chosen shared organization.")

(defun scope--local-address (suffix)
  "Return the current schema's session address for SUFFIX."
  (concat (if scope--freeform-p "work://" "local://") suffix))

(defun scope--shared-address (suffix)
  "Return the current schema's project address for SUFFIX."
  (concat (cond (scope--freeform-p "work://shared/")
                (scope--nested-p "local://shared/") (t "shared://")) suffix))

(defun scope--put (path text)
  "Write synthetic TEXT to PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text)))

(defun scope--read (path)
  "Return PATH contents."
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defun scope--resolve (root session address &optional write)
  "Resolve ADDRESS in ROOT and SESSION, optionally for WRITE."
  (when scope--freeform-p
    (cond
     ((or (string-prefix-p "local://" address) (string-prefix-p "shared://" address))
      (error "Use work://shared/ for project files and work:// for session files"))
     ((string-prefix-p "work://" address)
      (setq address (concat "local://" (substring address (length "work://")))))))
  (when scope--nested-p
    (cond
     ((string-prefix-p "shared://" address) (error "Use local://shared/ for project files"))
     ((equal address "local://shared") (setq address "shared://"))
     ((string-prefix-p "local://shared/" address)
      (setq address (concat "shared://" (substring address (length "local://shared/")))))))
  (unless (string-match "\\`\\(local\\|shared\\|memory\\|journal\\)://\\(.*\\)\\'" address)
    (error (cond (scope--freeform-p "Use a work://, memory:// or journal:// address")
                 (scope--nested-p "Use a local://, memory:// or journal:// address")
                 (t "Use a local://, shared://, memory:// or journal:// address"))))
  (let* ((scheme (match-string 1 address)) (tail (match-string 2 address))
         (base (if (equal scheme "local") (file-name-concat root "local" session)
                 (file-name-concat root scheme)))
         (path (expand-file-name tail (file-name-as-directory base))))
    (when (or (file-name-absolute-p tail) (member ".." (split-string tail "/"))
              (string-match-p "[\n\r]" tail)) (error "Invalid address path"))
    (when (and write (member scheme '("memory" "journal"))) (error "Address is read-only"))
    path))

(defun scope--files (root session)
  "Return visible addresses and contents in ROOT for SESSION."
  (let (rows)
    (dolist (prefix (list (scope--local-address "") (scope--shared-address "") "memory://" "journal://"))
      (let ((base (scope--resolve root session prefix)))
        (when (file-directory-p base)
          (dolist (file (directory-files-recursively base "."))
            (push (cons (concat prefix (file-relative-name file base)) (scope--read file)) rows)))))
    (sort rows (lambda (a b) (string< (car a) (car b))))))

(defun scope--call (root session operation args)
  "Execute prototype OPERATION with ARGS in ROOT for SESSION."
  (pcase operation
    ('read (scope--read (scope--resolve root session (car args))))
    ((or 'glob 'grep)
     (let* ((address (car args)) (pattern (cadr args))
            (_ (scope--resolve root session address))
            (rows (cl-remove-if-not (lambda (row) (string-prefix-p address (car row)))
                                    (scope--files root session))))
       (mapconcat (lambda (row) (if (eq operation 'glob) (car row) (format "%s\n%s" (car row) (cdr row))))
                  (cl-remove-if-not
                   (lambda (row) (string-match-p (if (eq operation 'glob) (wildcard-to-regexp pattern) pattern)
                                                 (if (eq operation 'glob) (file-name-nondirectory (car row)) (cdr row)))) rows)
                  "\n")))
    ('patch
     (let* ((patch (mapconcat
                    (lambda (line)
                      (if (string-match "\\`\\(\\*\\*\\* \\(?:Add File\\|Update File\\|Delete File\\|Move to\\): \\)\\(.+\\)\\'" line)
                          (concat (match-string 1 line)
                                  (scope--resolve root session (match-string 2 line) t)) line))
                    (split-string (car args) "\n") "\n"))
            (proposal (mevedel-tool-patch-parse patch root)))
       ;; Check every parsed destination too; malformed headers cannot bypass mapping.
       (dolist (op (plist-get proposal :operations))
         (dolist (path (delq nil (list (plist-get op :path) (plist-get op :move-path))))
           (unless (or (file-in-directory-p path (file-name-concat root "local" session))
                       (file-in-directory-p path (file-name-concat root "shared")))
             (error "Patch path outside writable scope"))))
       (mevedel-tool-patch-commit (mevedel-tool-patch-planned-changes proposal))
       "Patch applied"))))

(defun scope--tools (root session record)
  "Build request-local tools for ROOT and SESSION, logging to RECORD."
  (let ((gptel--known-tools nil))
    (mapcar
     (lambda (spec)
       (let ((op (car spec)))
         (gptel-make-tool
          :name (nth 1 spec) :category "scope-experiment" :confirm nil :include nil
          :description (nth 2 spec)
          :args (if scope--nested-p
                    (mapcar (lambda (arg)
                              (let ((copy (copy-sequence arg)))
                                (plist-put copy :description
                                           (replace-regexp-in-string "shared://" (scope--shared-address "")
                                                                     (plist-get copy :description) t t))))
                            (nth 3 spec))
                  (nth 3 spec))
          :function
          (lambda (&rest args)
            (let ((result (condition-case err (scope--call root session op args)
                            (error (concat "Error: " (error-message-string err))))))
              (funcall record (list :tool (nth 1 spec) :args (vconcat args) :result result))
              result)))))
     '((read "Read" "Read a UTF-8 text file by its full resource address."
             ((:name "path" :type string :description "Full resource address")))
       (glob "Glob" "List visible files below an address prefix. Pattern matches basenames; use * for all files."
             ((:name "path" :type string :description "Resource address prefix, e.g. shared://")
              (:name "pattern" :type string :description "Filename wildcard")))
       (grep "Grep" "Search file contents below an address prefix with an Emacs regular expression."
             ((:name "path" :type string :description "Resource address prefix")
              (:name "pattern" :type string :description "Regular expression")))
       (patch "ApplyPatch" "Persist edits. Format: *** Begin Patch, *** Add File: ADDRESS followed by + lines; or *** Update File: ADDRESS, @@ and context/removed-/added+ lines; or *** Delete File: ADDRESS; then *** End Patch. Read existing files before updating."
              ((:name "patch" :type string :description "Complete patch text using resource addresses")))))))

(defun scope--system (arm session)
  "Return equal-tool scope instructions for ARM and SESSION."
  (replace-regexp-in-string
   "local://" (scope--local-address "")
   (concat
   "You are an agent working on project Kite. Complete the requested task using tools. Persist notes with ApplyPatch, not just a promise in your reply. Be concise.\n"
   (if scope--nested-p
       (concat "Resource scopes:\nlocal:// belongs to the current session except for its shared/ subtree. Agents in this session share session-local files; a new session cannot see those files.\n"
               "local://shared/ is this project's working area, visible across sessions. It can contain tentative work; it is not standing guidance.\n")
     (concat "Resource scopes:\nlocal:// belongs to the current session. Agents in this session share it; a new session cannot see it.\n"
             "shared:// is this project's working area, visible across sessions. It can contain tentative work; it is not standing guidance.\n"))
   "memory:// is curated durable guidance, read-only here. journal:// is automatically recorded historical evidence, read-only; this session is not yet in it.\n"
   "Keep managed plans under local://plans/. Preserve unrelated files and other sessions' work. Do not treat hypotheses as facts. You have no other project or messaging tools.\n"
   (cond
    (scope--freeform-p
     "Note policy: Put working notes in work://shared/. Search for relevant existing material before creating a file; update it when appropriate. Choose filenames and organize the shared directory as needed. Avoid unnecessary duplicate copies.\n")
    ((equal arm "split")
     (format "Note policy: Keep session working notes at local://notes.md. When information should be available to other sessions, publish the useful part at shared://sessions/%s/notes.md. Decide what merits publishing; avoid unnecessary duplicate copies.\n" session))
    (t (format "Note policy: Keep session working notes directly at %s. This is already available to other sessions; no separate publishing step is needed. Keep unrelated sessions' notes separate; avoid unnecessary duplicate copies.\n"
               (scope--shared-address (format "sessions/%s/notes.md" session)))))
   (format "Current session: %s. Search the resource roots when you need to recover prior work.\n" session))))

(defun scope--request (backend model root session arm prompt)
  "Run a bounded real request and return credential-free evidence."
  (let* ((buffer (generate-new-buffer " *scope-evaluation*"))
         (start (float-time)) (deadline (+ start 180))
         (system (scope--system arm session))
         (round 0) (recorded-round 0) (input 0) (cached 0) (output 0)
         chunks transcript calls done status failure)
    (cl-labels
        ((usage (info)
           (when (and (/= recorded-round round) (plist-get info :tokens))
             (setq recorded-round round)
             (let ((tokens (plist-get info :tokens)))
               (cl-incf input (or (plist-get tokens :input) 0))
               (cl-incf cached (or (plist-get tokens :cached) 0))
               (cl-incf output (or (plist-get tokens :output) 0)))))
         (finish (outcome &optional error-text)
           (unless done (setq done t status outcome failure error-text)))
         (wait-handler (fsm)
           (if (or (>= round 12) (>= (float-time) deadline))
               (finish "limit" "Request limit reached")
             (setq chunks nil)
             (cl-incf round)
             (gptel--handle-wait fsm)))
         (tool-handler (fsm)
           (usage (gptel-fsm-info fsm))
           (gptel--handle-tool-use fsm))
         (provider (response info)
           (cond ((stringp response) (push response chunks) (push response transcript))
                 ((eq response t) (usage info))))
         (done-handler (fsm) (usage (gptel-fsm-info fsm)) (finish "success")))
      (unwind-protect
          (condition-case err
              (with-current-buffer buffer
                (setq-local gptel-backend backend gptel-model model
                            gptel-reasoning-effort (unless (eq model 'deepseek-v4-flash) 'none)
                            gptel-max-tokens (when (eq model 'deepseek-v4-flash) 4096)
                            gptel-use-context nil gptel-track-response nil
                            gptel-use-tools t gptel-confirm-tool-calls nil
                            gptel-tools (scope--tools root session (lambda (row) (push row calls)))
                            gptel-system-prompt system gptel-stream t)
                (gptel-request
                 prompt :buffer buffer :system system :stream t :transforms nil :callback #'provider
                 :fsm (gptel-make-fsm
                       :handlers
                       (append (list (list 'WAIT #'wait-handler) (list 'TOOL #'tool-handler)
                                     (list 'DONE #'done-handler)
                                     (list 'ERRS (lambda (fsm) (finish "error" (format "%s" (plist-get (gptel-fsm-info fsm) :error)))))
                                     (list 'ABRT (lambda (_) (finish "aborted"))))
                               (cl-remove-if (lambda (row) (memq (car row) '(WAIT TOOL DONE ERRS ABRT))) gptel-request--handlers))))
                (while (and (not done) (< (float-time) deadline)) (accept-process-output nil 0.1))
                (unless done (finish "timeout" "Request timed out")))
            (error (finish "error" (error-message-string err))))
        (unless (equal status "success") (gptel-abort buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))
    (list :status status :error failure :system system :prompt prompt :rounds round
          :input_tokens input :cached_tokens cached :output_tokens output
          :seconds (- (float-time) start) :calls (vconcat (nreverse calls))
          :reply (apply #'concat (nreverse chunks)) :transcript (apply #'concat (nreverse transcript)))))

(defun scope--case (backend model arm trial kind)
  "Execute writer and isolated reader for KIND under ARM in TRIAL."
  (let* ((scope--freeform-p (equal arm "freeform-work"))
         (scope--nested-p (or scope--freeform-p (equal arm "nested-shared")))
         (root (make-temp-file "mevedel-scope-case-" t))
         (writer (format "work-%d" trial)) (reader (if (equal kind "scratch") writer "next-session"))
         (notes (cond (scope--freeform-p (scope--shared-address (if (= trial 1) "export-investigation.md" "backend-findings.md")))
                      ((equal arm "split") "local://notes.md")
                      (t (scope--shared-address (format "sessions/%s/notes.md" writer)))))
         (peer-address (scope--shared-address (if scope--freeform-p "ui-investigation.md" "sessions/peer/notes.md")))
         (plan "# Managed plan\n1. Diagnose export retries.\n2. Validate before deployment.\n")
         (peer "# Other session\nOwner: peer. CSS issue still under investigation. Do not edit this note.\n")
         (memory "# Project guidance\nDo not store credentials in notes.\n")
         (journal "# Prior evidence\n2026-09-01: export investigation opened; backend not established.\n")
         prompt question result)
    (unwind-protect
        (progn
          (dolist (session (list writer reader)) (make-directory (scope--resolve root session (scope--local-address "")) t))
          (scope--put (scope--resolve root writer (scope--local-address "plans/current.md")) plan)
          (scope--put (scope--resolve root writer peer-address) peer)
          (scope--put (scope--resolve root writer "memory://MEMORY.md") memory)
          (scope--put (scope--resolve root writer "journal://prior.md") journal)
          (pcase kind
            ("scratch"
             (setq prompt "Keep a scratch note so another agent in this same session can pick up my investigation: I suspect retry jitter causes error KITE-217. This is an untested hypothesis; no reproducer or tests have run. Next try a deterministic random seed. Do not change the managed plan."
                   question "Recover the notes for error KITE-217. What is suspected, how certain is it, what tests ran, and what should I try next? Use the files, and do not guess."))
            ("handoff"
             (setq prompt "I am ending this session. Leave a useful handoff for a colleague starting another session on KITE-318. Observed: pytest tests/test_cache.py passed 8/8 on SQLite 3.46 after explicit None handling; empty-string behavior is unchanged. PostgreSQL integration was not run. The colleague should test PostgreSQL next. Do not deploy. Preserve the existing managed plan."
                   question "I just started a new session to continue KITE-318. Find the previous agent's handoff. Which tests passed on what backend, what is untested, what should I do next, and may I deploy? Do not guess."))
            ("correction"
             (scope--put (scope--resolve root writer notes)
                         "# KITE-419 investigation\nWorking assumption: production uses MariaDB 11, so logical replication is unavailable. This has not been verified.\n")
             (setq prompt "Update our KITE-419 investigation notes with this correction: config/production.env was inspected and shows DB_ENGINE=postgresql and DB_MAJOR=16. MariaDB 11 is only a disposable staging fixture. The old production-MariaDB assumption is wrong. Production export should use PostgreSQL logical replication. No migration or deployment was performed. Keep the managed plan unchanged."
                   question "I am in a new session continuing KITE-419. Recover the latest backend finding: what does production use, where is MariaDB used, what export approach was selected, and was any migration or deployment performed? Use evidence from files and report if it is unavailable.")))
          (setq result (list :model (symbol-name model) :arm arm :trial trial :case kind
                             :before (vconcat (mapcar (lambda (row) (list :path (car row) :content (cdr row))) (scope--files root writer))))
                result (append result (list :writer (scope--request backend model root writer arm prompt))))
          (setq result (append result
                               (list :after (vconcat (mapcar (lambda (row) (list :path (car row) :content (cdr row))) (scope--files root writer))))
                               (list :reader (scope--request backend model root reader arm question))))
          (append result
                  (when scope--freeform-p
                    (list :final (vconcat (mapcar (lambda (row) (list :path (car row) :content (cdr row))) (scope--files root writer)))))
                  (list :preserved
                        (if (and (equal (scope--read (scope--resolve root writer (scope--local-address "plans/current.md"))) plan)
                                 (equal (scope--read (scope--resolve root writer peer-address)) peer)
                                 (equal (scope--read (scope--resolve root writer "memory://MEMORY.md")) memory)
                                 (equal (scope--read (scope--resolve root writer "journal://prior.md")) journal)) t :false))))
      (delete-directory root t))))

(ert-deftest scope-experiment ()
  (if (getenv "MEVEDEL_SCOPE_SMOKE")
      (let ((root (make-temp-file "scope-smoke-" t)))
        (unwind-protect
            (progn
              (make-directory (file-name-concat root "shared") t)
              (make-directory (file-name-concat root "local" "a") t)
              (scope--call root "a" 'patch '("*** Begin Patch\n*** Add File: shared://sessions/a/notes.md\n+original\n*** End Patch"))
              (scope--call root "a" 'patch '("*** Begin Patch\n*** Update File: shared://sessions/a/notes.md\n@@\n-original\n+corrected\n*** End Patch"))
              (should (equal (scope--call root "b" 'read '("shared://sessions/a/notes.md")) "corrected\n"))
              (scope--call root "a" 'patch '("*** Begin Patch\n*** Add File: local://notes.md\n+private\n*** End Patch"))
              (should-error (scope--call root "b" 'read '("local://notes.md")))
              (should-error (scope--call root "a" 'patch '("*** Begin Patch\n*** Add File: memory://x.md\n+bad\n*** End Patch")))
              (should-error (scope--resolve root "a" "shared://../outside" t))
              (let ((scope--nested-p t))
                (should (equal (scope--call root "b" 'read '("local://shared/sessions/a/notes.md")) "corrected\n"))
                (should (equal (scope--call root "b" 'glob '("local://" "*")) "local://shared/sessions/a/notes.md"))
                (should-error (scope--call root "b" 'read '("local://notes.md")))
                (should-error (scope--resolve root "b" "shared://sessions/a/notes.md"))
                (should-error (scope--resolve root "b" "local://shared/../outside" t))
                (scope--call root "b" 'patch '("*** Begin Patch\n*** Update File: local://shared/sessions/a/notes.md\n@@\n-corrected\n+nested update\n*** End Patch"))
                (should (equal (scope--call root "a" 'read '("local://shared/sessions/a/notes.md")) "nested update\n")))
              (let ((scope--nested-p t) (scope--freeform-p t))
                (should (equal (scope--call root "b" 'read '("work://shared/sessions/a/notes.md")) "nested update\n"))
                (should-error (scope--resolve root "b" "local://shared/sessions/a/notes.md"))
                (should-error (scope--resolve root "b" "shared://sessions/a/notes.md"))
                (should-error (scope--call root "b" 'read '("work://notes.md")))
                (scope--call root "b" 'patch '("*** Begin Patch\n*** Add File: work://shared/chosen-name.md\n+New finding\n*** End Patch"))
                (should (string-match-p "chosen-name.md" (scope--call root "a" 'glob '("work://shared/" "*"))))
                (let ((system (scope--system "freeform-work" "a")))
                  (should-not (string-match-p "sessions/\\|topics/\\|local://" system))
                  (should (string-match-p "Search for relevant existing material" system)))))
          (delete-directory root t)))
    (let* ((config-path (getenv "MEVEDEL_SCOPE_PROVIDER"))
           (output (getenv "MEVEDEL_SCOPE_OUTPUT"))
           (model (intern (getenv "MEVEDEL_SCOPE_MODEL")))
           config backend rows)
      (unwind-protect
          (progn
            (setq config (read (scope--read config-path))
                  backend (apply (pcase (plist-get config :type)
                                   ('gptel-deepseek #'gptel-make-deepseek)
                                   ('gptel-openai-oauth #'gptel-make-openai-oauth)
                                   (_ (error "Unsupported backend")))
                                 (plist-get config :name) :models (plist-get config :models)
                                 (plist-get config :backend-options)))
            (setq config nil)
            (delete-file config-path)
            (dotimes (index 2)
              (dolist (kind '("scratch" "handoff" "correction"))
                (dolist (arm (cond ((getenv "MEVEDEL_SCOPE_FREEFORM") '("freeform-work"))
                                  ((getenv "MEVEDEL_SCOPE_NESTED") '("nested-shared"))
                                  ((= index 0) '("split" "shared-default"))
                                  (t '("shared-default" "split"))))
                  (let (row)
                    (mevedel-test--with-captured-diagnostics nil
                      (setq row (scope--case backend model arm (1+ index) kind)))
                    (push row rows)
                    (scope--put output (decode-coding-string (json-serialize (vconcat (reverse rows)) :false-object :false :null-object nil) 'utf-8-unix)))))))
        (setq config nil backend nil)
        (when (and config-path (file-exists-p config-path)) (delete-file config-path))))))

;;; run.el ends here
