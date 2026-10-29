;;; mevedel-memory-investigation.el -- Read-only consolidation tools -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the short-lived read tools for one consolidation request. Captured
;; evidence cannot be rebound through current memory aliases. Source access
;; stays within the captured workspace boundary. No session is manufactured.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'generator)
(require 'mevedel-memory-proposal)
(require 'mevedel-memory-scope)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)
(require 'mevedel-utilities)

;; `gptel-request'
(declare-function gptel-make-tool "gptel-request" (&rest slots))
(defvar gptel--known-tools)

;; `mevedel-structs'
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)

(defconst mevedel-memory-investigation--max-calls 64
  "Maximum admitted investigation calls per review request.")

(defconst mevedel-memory-investigation--max-bytes 65536
  "Maximum aggregate UTF-8 tool result bytes per review request.")

(defconst mevedel-memory-investigation--summary-bytes 32768
  "Maximum serialized UTF-8 bytes in one investigation summary.")

(defconst mevedel-memory-investigation-git-evidence "GIT-HISTORY"
  "Evidence key recording Git history a review read.
A note delete cites it in `resolved-by' as \"GIT-HISTORY: quote\".")

(cl-defstruct (mevedel-memory-investigation
               (:constructor mevedel-memory-investigation-create
                             (scope entries currentp exhausted)))
  "One bounded investigation; CURRENTP and EXHAUSTED belong to its owner.
SCOPE contains captured roots. ENTRIES are the owner's admitted public
journal snapshots. CURRENTP must reject expired generations. EXHAUSTED is
called with a reason when a tool budget prevents valid completion.
INSPECTED maps each normalized workspace path to the Read text delivered for it.
SUMMARIES retains bounded target fields and delivery metadata, never bodies."
  scope entries currentp exhausted (calls 0) (bytes 0) stopped active inspected summaries)

(defun mevedel-memory-investigation-summary (state)
  "Return STATE's bounded metadata-only call summary as an independent snapshot.
Calls remain in admission order.  Omitted rows are counted, never silently
treated as unmade calls.  Neither results nor error messages are retained."
  (let ((remaining (- mevedel-memory-investigation--summary-bytes 256))
        (print-length nil) (print-level nil) (print-circle nil)
        (print-escape-nonascii t) (print-escape-multibyte t) rows)
    (dolist (row (seq-take (reverse (mevedel-memory-investigation-summaries state)) 32))
      (let ((size (string-bytes (prin1-to-string row))))
        (when (>= remaining size)
          (cl-decf remaining size)
          (push (mapcar (lambda (value) (if (stringp value) (copy-sequence value) value)) row) rows))))
    (setq rows (nreverse rows))
    (list :calls rows
          :omitted-count (- (mevedel-memory-investigation-calls state) (length rows)))))

(defun mevedel-memory-investigation--summary-call (state operation args)
  "Admit a metadata row for OPERATION and ARGS in STATE.
Each target field is limited to 256 UTF-8 bytes, independently of execution."
  (let ((row (list :call-id (mevedel-memory-investigation-calls state)
                   :tool-name (pcase operation ('read "Read") ('glob "Glob") ('grep "Grep") ('git "Git") (_ "unknown"))
                   :status 'pending :result-bytes 0 :partial nil :truncated nil)))
    (dolist (pair '((:root . :investigation-root) (:path . :investigation-path)
                    (:pattern . :investigation-pattern) (:command . :investigation-command)))
     (let ((key (car pair)))
      (when-let* ((value (plist-get args key)) ((stringp value)))
        (when (eq key :path)
          (setq value (string-trim value))
          (setq value (cond ((string-empty-p value) ".")
                            ((mevedel-memory-proposal-relative-path-p value)
                             (mevedel-memory-proposal-normalize-path value))
                            (t "<invalid>"))))
        (when (and (eq key :command) (not (member value '("log" "show"))))
          (setq value "<invalid>"))
        (when (and (eq key :root)
                   (not (member value (append '("workspace" "journal")
                                               (mapcar #'car (plist-get (mevedel-memory-investigation-scope state) :roots))))))
          (setq value "<invalid>"))
        (when (> (string-bytes value) 256) (plist-put row :target-truncated t))
        (setq row (plist-put row (cdr pair) (mevedel--truncate-bytes value 256 ""))))))
    (unless (plist-member row :investigation-path) (setq row (plist-put row :investigation-path ".")))
    (when (eq operation 'git) (setq row (plist-put row :investigation-root "workspace")))
    (dolist (key '(:offset :limit))
      (when (and (integerp (plist-get args key))
                 (<= 0 (plist-get args key) most-positive-fixnum))
        (setq row (plist-put row key (plist-get args key)))))
    (push row (mevedel-memory-investigation-summaries state))
    row))

(defun mevedel-memory-investigation-stop (state)
  "Retire STATE and cancel its active helpers without delivering late results."
  (unless (mevedel-memory-investigation-stopped state)
    (setf (mevedel-memory-investigation-stopped state) t)
    (dolist (row (mevedel-memory-investigation-summaries state))
      (when (eq (plist-get row :status) 'pending) (plist-put row :status 'cancelled)))
    (dolist (cancel (mevedel-memory-investigation-active state)) (funcall cancel))
    (setf (mevedel-memory-investigation-active state) nil)))

(defun mevedel-memory-investigation--live-p (state)
  "Return non-nil while STATE's request still owns its generation."
  (and (not (mevedel-memory-investigation-stopped state))
       (funcall (mevedel-memory-investigation-currentp state))))

(defun mevedel-memory-investigation--exhaust (state reason)
  "Stop STATE and report the exhausted budget REASON exactly once."
  (unless (mevedel-memory-investigation-stopped state)
    (mevedel-memory-investigation-stop state)
    (funcall (mevedel-memory-investigation-exhausted state) reason)))

(defun mevedel-memory-investigation--bounded (text &optional read)
  "Return TEXT bounded to 8 KiB of UTF-8 with a visible omission marker.
For a numbered READ result the marker names the line to continue from, since
a whole large document otherwise shows only its beginning."
  (let ((text (mevedel--normalize-message-text text))
        (continue "\n[Result truncated at 8 KiB; Read again with offset %s to continue, or Grep this file for specific terms.]"))
    (if (or (not read) (<= (string-bytes text) 8192))
        (mevedel--truncate-bytes text 8192 "\n[Result truncated at 8 KiB; narrow the query.]")
      ;; Cut for the longest marker so the named line was actually delivered.
      (let* ((longest (format continue (make-string 10 ?9)))
             (prefix (string-remove-suffix longest (mevedel--truncate-bytes text 8192 longest))))
        (if (string-match "\\(?:\\`\\|\n\\) *\\([0-9]+\\)\t[^\n]*\\'" prefix)
            (concat prefix (format continue (match-string 1 prefix)))
          (mevedel--truncate-bytes text 8192 "\n[Result truncated at 8 KiB; narrow the query.]"))))))

(defun mevedel-memory-investigation--deliver (state callback text &optional read summary)
  "Deliver bounded TEXT to CALLBACK if STATE remains live and within budget.
READ marks a numbered Read result; SUMMARY is its metadata row.
Return the delivered text, or nil."
  (when (mevedel-memory-investigation--live-p state)
    (let* ((original (mevedel--normalize-message-text text))
           (text (mevedel-memory-investigation--bounded original read)))
      (when summary
        (plist-put summary :partial
                   (and (string-match-p "\\`\\[Partial source snapshot:\\|^Warning: .*results are partial[.]" text) t))
        (plist-put summary :truncated
                   (or (not (equal original text))
                       (and (string-match-p "^[.]\\{3\\} \\(Read output\\|Results\\|Output\\) truncated" original) t)
                       (and read
                            (string-match-p
                             "^ *[0-9]+\t[^\n]\\{2000\\} \\[\\.\\.\\. Read(file_path=.*, offset=[0-9]+, limit=1, char_offset=[0-9]+) to continue this line]$"
                             original)
                            t))))
      (if (> (+ (mevedel-memory-investigation-bytes state) (string-bytes text))
             mevedel-memory-investigation--max-bytes)
          (progn
            (when summary (plist-put summary :status 'output-limit))
            (mevedel-memory-investigation--exhaust state "Tool output budget exhausted") nil)
        (cl-incf (mevedel-memory-investigation-bytes state) (string-bytes text))
        (when summary
          (plist-put summary :status (if (or (eq (plist-get summary :status) 'error)
                                            (string-prefix-p "Error:" text)) 'error 'delivered))
          (plist-put summary :result-bytes (string-bytes text))
          (plist-put summary :result-hash (secure-hash 'sha256 (encode-coding-string text 'utf-8-unix))))
        (funcall callback text)
        text))))

(defun mevedel-memory-investigation--relative (path)
  "Validate one tool-relative PATH, accepting dot for a search root."
  (unless (mevedel-memory-proposal-relative-path-p path)
    (error "Invalid investigation path"))
  path)

(defun mevedel-memory-investigation--evidence (state root)
  "Return captured document pairs for ROOT in STATE, checking original authority."
  (if (equal root "journal")
      (mapcar (lambda (entry)
                (cons (plist-get entry :file)
                      (format "Digest: %s\nSession: %s\nCreated: %s\n\n%s"
                              (plist-get entry :id) (plist-get entry :session)
                              (plist-get entry :created) (plist-get entry :body))))
              (mevedel-memory-investigation-entries state))
    (let* ((scope (mevedel-memory-investigation-scope state))
           (descriptor (mevedel-memory-scope--root scope root)))
      (cl-loop for (file . snapshot) in (plist-get descriptor :before)
               ;; Attachments and directories carry hashes, not readable text.
               when (and (plist-get snapshot :exists) (plist-get snapshot :bytes))
               collect (cons file (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))))))

(iter-defun mevedel-memory-investigation--documents (state root path contents)
  "Yield progress then (:ready (DOCUMENTS . PARTIAL)) for STATE's ROOT and PATH.
When CONTENTS is nil, snapshot file names only. Source searches inspect at
most 256 entries and copy at most 2 MiB, with a 512 KiB per-file limit.
The caller labels omitted source data; model tools can narrow the subtree.
Each source admission keeps its fresh authority and pinned read checks."
  (if (not (equal root "workspace"))
      (let ((documents
             (cl-remove-if-not
              (lambda (document)
                (or (equal path ".") (equal path (car document))
                    (string-prefix-p (file-name-as-directory path) (car document))))
              (mevedel-memory-investigation--evidence state root))))
        (iter-yield (list :ready (cons documents nil))))
    (let* ((scope (mevedel-memory-investigation-scope state))
           (pending (list (list (mevedel-memory-scope-source-path scope path))))
           (count 0) (remaining (* 2 1024 1024)) documents partial)
      (while (and pending (< count 256))
        (unless (mevedel-memory-investigation--live-p state) (error "Investigation retired"))
        (let* ((siblings (pop pending))
               (native (car siblings))
               (relative (file-relative-name native (plist-get scope :workspace-root))))
          ;; Rotate directory batches one entry at a time.  Pending siblings
          ;; must not reserve every visit: a wide root would starve descendants.
          (when (cdr siblings)
            (setq pending (nconc pending (list (cdr siblings)))))
          (cl-incf count)
          (condition-case nil
              (progn
                (mevedel-memory-scope-source-path scope relative)
                (if (file-directory-p native)
                    (let* ((available (- 256 count))
                           (children (and (mevedel-memory-investigation--live-p state)
                                          (> available 0)
                                          (directory-files native t directory-files-no-dot-files-regexp nil (1+ available)))))
                      (when (or (= available 0) (> (length children) available)) (setq partial t))
                      (when children
                        (setq pending (nconc pending (list (seq-take children available))))))
                  (unless (file-regular-p native) (setq partial t))
                  (when (and (file-regular-p native)
                             (mevedel-memory-investigation--live-p state))
                    (let* ((snapshot (and contents (mevedel-memory-scope--snapshot native (min remaining (* 512 1024)))))
                           (bytes (plist-get snapshot :bytes)))
                      (when (and contents (not (plist-get snapshot :exists)))
                        (error "Source file is unavailable"))
                      (cl-decf remaining (length bytes))
                      (push (cons relative (if contents (decode-coding-string bytes 'utf-8-unix) "")) documents)))))
            (error (setq partial t))))
        (iter-yield nil))
      (iter-yield (list :ready (cons (nreverse documents) (or partial pending)))))))

(defun mevedel-memory-investigation--search (state operation args root path callback &optional summary)
  "Run bounded search OPERATION using ARGS over STATE's ROOT and PATH.
Only admitted copies enter the ordinary helper's read scope. CALLBACK gets
relative filenames; private copy paths are removed from results and errors.
SUMMARY is the call's metadata row."
  (let ((directory (make-temp-file "mevedel-memory-search-" t))
        (caller-directory default-directory)
        (caller-exec-path (copy-sequence exec-path))
        (caller-environment (copy-sequence process-environment))
        (caller-request (bound-and-true-p mevedel--current-request))
        (caller-invocation (bound-and-true-p mevedel--agent-invocation))
        (origin (current-buffer))
        (iterator (mevedel-memory-investigation--documents state root path (eq operation 'grep)))
        timer stepping writing documents partial delivery done cancel launched stopper)
    (cl-labels
     ((cleanup ()
        ;; Pinned reads and helper launch can dispatch cancellation.  Their
        ;; current step must unwind before closing its iterator or directory.
        (unless stepping
          (when timer (cancel-timer timer) (setq timer nil))
          (when iterator (iter-close iterator) (setq iterator nil))
          (setf (mevedel-memory-investigation-active state)
                (delq stopper (mevedel-memory-investigation-active state)))
          (when (buffer-live-p origin)
            (with-current-buffer origin (remove-hook 'kill-buffer-hook #'abort-search t)))
          (when (file-exists-p directory) (delete-directory directory t))
          (when delivery
            (let ((text delivery))
              (setq delivery nil)
              (when (buffer-live-p origin)
                (with-current-buffer origin
                  (let ((default-directory caller-directory)
                        (exec-path caller-exec-path)
                        (process-environment caller-environment)
                        (mevedel--session nil)
                        (mevedel--current-request caller-request)
                        (mevedel--agent-invocation caller-invocation))
                    (mevedel-memory-investigation--deliver
                     state callback
                     (string-replace directory "."
                                     (string-replace (file-name-as-directory directory) "" text)) nil summary))))))))
      (finish (text)
        (unless done
          (setq done t delivery text)
          (cleanup)))
      (abort-search ()
        (unless done
          (setq done t)
          (when (and summary (eq (plist-get summary :status) 'pending))
            (plist-put summary :status 'cancelled))
          (when (functionp cancel) (funcall cancel))
          (cleanup)))
      (launch ()
        (setq launched t)
        (let ((default-directory (file-name-as-directory directory))
              (mevedel-tool-fs-search--teardown #'abort-search)
              (mevedel-tool-fs-search-timeout 20))
          (setq cancel
                (funcall (if (eq operation 'glob) #'mevedel-tool-fs-search-glob #'mevedel-tool-fs-search-grep)
                         (lambda (result)
                           (when (and summary (mevedel-memory-investigation--live-p state)
                                      (string-prefix-p "Error:" (plist-get result :result)))
                             (plist-put summary :status 'error))
                           (finish (concat (when partial "[Partial source snapshot: excluded, unavailable, or bounded paths were not searched. Missing matches remain unknown.]\n")
                                           (plist-get result :result))))
                         (list :path directory :pattern (plist-get args :pattern)
                               :output_mode "content" :head_limit 100 :glob (plist-get args :glob))))
          (when (and done (functionp cancel)) (funcall cancel))))
      (step ()
        (unless stepping
          (setq timer nil stepping t)
          (unwind-protect
              (unless done
                (if (or (not (buffer-live-p origin)) (not (mevedel-memory-investigation--live-p state)))
                    (abort-search)
                  (with-current-buffer origin
                    (let ((default-directory caller-directory)
                          (exec-path caller-exec-path)
                          (process-environment caller-environment)
                          (mevedel--session nil)
                          (mevedel--current-request caller-request)
                          (mevedel--agent-invocation caller-invocation))
                      (condition-case err
                          (let ((deadline (+ (float-time) 0.02)) (count 0))
                            ;; Bound both traversal/read work and temporary-file
                            ;; writes.  A single native operation still owns its
                            ;; pinned proof; authority is never cached over a yield.
                            (while (and (not done) (< count 8) (< (float-time) deadline))
                              (cl-incf count)
                              (unless (mevedel-memory-investigation--live-p state) (abort-search))
                              (unless done
                                (if (not writing)
                                    (let ((value (iter-next iterator)))
                                      (when (and (not done) (plist-member value :ready))
                                        (setq writing t documents (car (plist-get value :ready))
                                              partial (cdr (plist-get value :ready)))))
                                  (if (null documents)
                                      (progn (launch) (setq count 8))
                                    (let* ((document (pop documents))
                                           (file (file-name-concat directory
                                                                   (mevedel-memory-investigation--relative (car document)))))
                                      (make-directory (file-name-directory file) t)
                                      (unless (or done (not (mevedel-memory-investigation--live-p state)))
                                        (let ((coding-system-for-write 'utf-8-unix))
                                          (write-region (cdr document) nil file nil 'silent))))))))
                            (unless (or done launched)
                              (setq timer (run-at-time 0.001 nil #'step))))
                        (error (finish (concat "Error: " (error-message-string err)))))))))
            (setq stepping nil)
            (when done (cleanup))))))
     (setq stopper #'abort-search)
     (push stopper (mevedel-memory-investigation-active state))
     (with-current-buffer origin (add-hook 'kill-buffer-hook #'abort-search nil t))
     (setq timer (run-at-time 0 nil #'step)))))

(defun mevedel-memory-investigation--record (state key delivered)
  "Record DELIVERED text as read evidence under KEY in STATE."
  (when delivered
    (let ((row (assoc key (mevedel-memory-investigation-inspected state))))
      (if row (setcdr row (concat (cdr row) "\n" delivered))
        (push (cons key delivered) (mevedel-memory-investigation-inspected state))))))

(defun mevedel-memory-investigation--git (state args path callback &optional summary)
  "Run read-only Git history query ARGS for STATE, limited to PATH.
`log' searches commit messages for :pattern; `show' prints one :pattern
commit's message, changed-file summary and whether the current HEAD
contains it.  A commit hash given to `log' is shown instead.  No shell,
pager, hook or optional lock is involved; output is bounded and recorded
as evidence.  SUMMARY is the call's metadata row."
  (let* ((scope (mevedel-memory-investigation-scope state))
         (root (plist-get scope :workspace-root))
         (pattern (plist-get args :pattern))
         (hashp (string-match-p "\\`[0-9a-f]\\{7,40\\}\\'" pattern))
         (command (if (and hashp (equal (plist-get args :command) "log")) "show" (plist-get args :command)))
         (format "--format=commit %h %ad%n%s%n%n%b")
         ;; Each step is (ARGUMENTS . FORMATTER); FORMATTER receives the
         ;; step's output and exit status and returns its text.
         (steps (pcase command
                  ("log" (list (cons (append (list "log" "--no-color" "-i" "-E" (concat "--grep=" pattern)
                                                   "--date=short" format "-n" "30")
                                             (unless (equal path ".") (list "--" path)))
                                     (lambda (text status)
                                       (cond ((/= status 0) "Error: Git history query failed")
                                             ((string-blank-p text) "No matching commits.")
                                             (t text))))))
                  ("show" (unless hashp (error "Git show needs an abbreviated or full commit hash"))
                          (list (cons (list "show" "--no-color" "--no-ext-diff" "--stat" "--date=short" format pattern "--")
                                      (lambda (text status)
                                        (if (= status 0) text "Error: Unknown commit")))
                                (cons (list "merge-base" "--is-ancestor" pattern "HEAD")
                                      (lambda (_text status)
                                        ;; Name the commit, so a quote of this
                                        ;; line cannot vouch for another one.
                                        (format "\nCommit %s contained in current HEAD: %s\n" pattern
                                                (pcase status (0 "yes") (1 "no") (_ "unknown")))))))
                  (_ (error "Git command must be log or show"))))
         (output (generate-new-buffer " *mevedel-memory-git*"))
         (process-environment (append '("GIT_OPTIONAL_LOCKS=0" "GIT_TERMINAL_PROMPT=0" "GIT_PAGER=cat")
                                      process-environment))
         (default-directory (file-name-as-directory root))
         (collected "")
         done process timer stopper)
    (when (file-remote-p root) (error "Git history is unavailable for remote workspaces"))
    (cl-labels
        ((finish (text)
           (unless done
             (setq done t)
             (when timer (cancel-timer timer))
             (setf (mevedel-memory-investigation-active state)
                   (delq stopper (mevedel-memory-investigation-active state)))
             (when (buffer-live-p output) (kill-buffer output))
             (when text
               (mevedel-memory-investigation--record
                state mevedel-memory-investigation-git-evidence
                (mevedel-memory-investigation--deliver state callback text nil summary)))))
         (run (remaining)
           (with-current-buffer output (erase-buffer))
           (setq process
                 (make-process
                  :name "mevedel-memory-git" :buffer output :noquery t :connection-type 'pipe
                  :stderr (get-buffer-create " *mevedel-memory-git-stderr*")
                  :command (append (list "git" "-c" "core.fsmonitor=false" "-C" root) (car (car remaining)))
                  :sentinel
                  (lambda (proc _event)
                    (unless (or done (process-live-p proc))
                      (let ((text (funcall (cdr (car remaining))
                                           (with-current-buffer output
                                             (decode-coding-string (buffer-string) 'utf-8-unix))
                                           (process-exit-status proc))))
                        (setq collected (concat collected text))
                        (if (and (cdr remaining) (not (string-prefix-p "Error:" text)))
                            (run (cdr remaining))
                          (finish collected)))))))
           (set-process-coding-system process 'binary 'binary)))
      (setq stopper (lambda ()
                      (when (process-live-p process) (delete-process process))
                      (finish nil)))
      (push stopper (mevedel-memory-investigation-active state))
      (run steps)
      (setq timer (run-at-time 20 nil (lambda ()
                                        (when (process-live-p process) (delete-process process))
                                        (finish "Error: Git history query timed out")))))))

(defun mevedel-memory-investigation-call (state operation args callback)
  "Run read-only OPERATION with ARGS and deliver a string to CALLBACK.
ARGS uses :root (workspace, journal, or a captured root ID), :path relative
to that root, and operation-specific fields.
`mevedel-memory-investigation--max-calls' bounds admitted calls;
`mevedel-memory-investigation--max-bytes' bounds aggregate returned text.
Exhaustion stops the owner instead of enabling
another model follow-up. Calls after retirement have no effects or callback."
  (when (mevedel-memory-investigation--live-p state)
    (if (>= (mevedel-memory-investigation-calls state) mevedel-memory-investigation--max-calls)
        (mevedel-memory-investigation--exhaust state "Tool call budget exhausted")
      (cl-incf (mevedel-memory-investigation-calls state))
      (let ((summary (mevedel-memory-investigation--summary-call state operation args)))
       (condition-case err
          (let* ((root (plist-get args :root))
                 (path (mevedel-memory-investigation--relative
                        (let ((given (string-trim (or (plist-get args :path) ""))))
                          (if (string-empty-p given) "." given))))
                 (scope (mevedel-memory-investigation-scope state))
                 (mevedel--session nil))
            (unless (memq operation '(read glob grep git)) (error "Unsupported investigation operation"))
            (when (memq operation '(glob grep git))
              (unless (and (stringp (plist-get args :pattern))
                           (<= 1 (length (plist-get args :pattern)) 1024))
                (error "Invalid investigation search pattern")))
            (cond
             ((eq operation 'git)
              (mevedel-memory-investigation--git state args path callback summary))
             ((memq operation '(glob grep))
              (mevedel-memory-investigation--search state operation args root path callback summary))
             (t
              (dolist (key '(:offset :limit))
		(when-let* ((value (plist-get args key)))
                  (unless (and (integerp value) (> value 0)) (error "Invalid Read range"))))
              (let (temporary)
                (unwind-protect
                    (let* ((document
                            (if (equal root "workspace")
                                (let ((snapshot
                                       (mevedel-memory-scope--snapshot
                                        (mevedel-memory-scope-source-path scope path)
                                        (* 512 1024))))
                                  (unless (plist-get snapshot :exists)
                                    (error "Source file is unavailable"))
                                  (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))
                              (or (cdr (assoc path (mevedel-memory-investigation--evidence state root)))
                                  (error "File is not in the captured evidence")))))
                      (setq temporary (make-temp-file "mevedel-memory-read-" nil
                                                      (file-name-extension path t)))
                      (let ((coding-system-for-write 'utf-8-unix))
                        (write-region document nil temporary nil 'silent))
                      (let ((delivered (mevedel-memory-investigation--deliver
                                        state callback
                                        (mevedel-tool-fs-read-slurp-file-contents
                                         temporary (plist-get args :offset) (plist-get args :limit) path)
                                        t summary)))
                        (when (and delivered (equal root "workspace"))
                          ;; A delete's resolution quotes must come from text the
                          ;; review actually read, not from a search match.
                          (mevedel-memory-investigation--record
                           state (mevedel-memory-proposal-normalize-path path) delivered))))
                  (when temporary (delete-file temporary)))))))
        (error (mevedel-memory-investigation--deliver
                state callback (concat "Error: " (error-message-string err)) nil summary)))))))

(defun mevedel-memory-investigation-tools (state)
  "Return STATE's four scoped asynchronous gptel tools.
Tool definitions stay request-local; they do not enter gptel's global registry.
Paths are relative to the explicit root argument, not a new resource family."
  (let ((gptel--known-tools nil)
        (roots (vconcat '("workspace" "journal")
                        (mapcar #'car (plist-get (mevedel-memory-investigation-scope state) :roots)))))
    (append
     (mapcar
     (lambda (operation)
       (let* ((readp (eq operation 'read))
              (keys (if readp '(:root :path :offset :limit) '(:root :path :pattern)))
              (args (append
                     (list (list :name "root" :type 'string :enum roots
                                 :description "workspace source, admitted journal, or captured memory/instruction root ID")
                           '(:name "path" :type string :description "Relative path within root; use . for a search root"))
                     (if readp
                         '((:name "offset" :type integer :optional t :description "First line, starting at 1")
                           (:name "limit" :type integer :optional t :description "Maximum lines to return"))
                       '((:name "pattern" :type string :description "Glob wildcard or Grep regular expression"))))))
         (gptel-make-tool
          :name (capitalize (symbol-name operation)) :category "memory-review"
          :async t :include nil :confirm nil :args args
          :description (concat
                        (pcase operation
                          ('read "Read one relative file, returning numbered text lines. ")
                          ('glob "Find relative paths matching a glob pattern. ")
                          ('grep "Search text with a regular expression, returning matching lines. "))
                        "Use the explicit root and relative path; resource URLs are not accepted. "
                               "Results are bounded to 8 KiB. Narrow partial searches; missing matches are not proof of absence. "
                               "Memory and journal are captured evidence, not write authority.")
          :function (lambda (callback &rest values)
                      (mevedel-memory-investigation-call
                       state operation (cl-mapcan #'list keys values) callback)))))
     '(read glob grep))
     (list
     (gptel-make-tool
      :name "Git" :category "memory-review" :async t :include nil :confirm nil
      :args '((:name "command" :type string :enum ["log" "show"]
               :description "log searches commit messages; show prints one commit's message and changed files")
              (:name "pattern" :type string
               :description "log: case-insensitive extended regular expression over commit messages; show: commit hash")
              (:name "path" :type string :optional t
               :description "log only: limit to commits touching this workspace-relative path; default ."))
      :description (concat "Read-only Git history of the workspace, bounded to 30 commits and 8 KiB. "
                           "Use it to establish that work a note leaves open was later committed; "
                           "quote its output in resolved-by as \"" mevedel-memory-investigation-git-evidence ": exact words\".")
      :function (lambda (callback command pattern &optional path)
                  (mevedel-memory-investigation-call
                   state 'git (list :command command :pattern pattern :path path) callback)))))))

(provide 'mevedel-memory-investigation)
;;; mevedel-memory-investigation.el ends here
