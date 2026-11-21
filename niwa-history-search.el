;;; niwa-history-search.el --- Search filtered saved conversations -*- lexical-binding: t -*-

;;; Commentary:
;; Workspace history access without resuming sessions. Native persistence owns
;; authority and transcript classification owns filtering. This module prepares
;; disposable projections in bounded steps and reuses ordinary resource tools.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'niwa-agent-conversation)
(require 'niwa-session-persistence)
(require 'niwa-transport)
(require 'niwa-turn)

;; `niwa-execution'
(declare-function niwa-execution--owner-admissible-p
                  "niwa-execution" (owner-context))
(autoload 'niwa-execution--owner-admissible-p "niwa-execution")

;; `niwa-pipeline'
(declare-function niwa-pipeline-handler-resumable "niwa-pipeline" (function))
(autoload 'niwa-pipeline-handler-resumable "niwa-pipeline")

;; `niwa-resource'
(declare-function niwa-resource-encode-component "niwa-resource" (value))
(declare-function niwa-resource-error-message
                  "niwa-resource" (failure &optional address private-paths))

;; `niwa-session-publication'
(declare-function niwa-session-publication-read-batch
                  "niwa-session-publication" (directories listings))
(autoload 'niwa-session-publication-read-batch "niwa-session-publication")

;; `niwa-structs'
(declare-function niwa-session-authority-mode-for-workspace
                  "niwa-structs" (workspace))

;; `niwa-tool-fs-read'
(declare-function niwa-tool-fs-read--virtual-text
                  "niwa-tool-fs-read" (text args address))

;; `niwa-tool-fs-search'
(declare-function niwa-tool-fs-search-glob
                  "niwa-tool-fs-search" (callback args))
(declare-function niwa-tool-fs-search-grep
                  "niwa-tool-fs-search" (callback args))
(defvar niwa-tool-fs-search--resource-address)
(defvar niwa-tool-fs-search--resource-dispatching)
(defvar niwa-tool-fs-search--teardown)

(defconst niwa-history-search--cache-limit (* 64 1024 1024)
  "Maximum retained projection payload bytes; not a process memory limit.")

(defvar niwa-history-search--cache (make-hash-table :test #'equal)
  "Disposable projections keyed by logical source path, as (SHA256 . TEXT).")

(defvar niwa-history-search--cache-bytes 0
  "Payload bytes retained in `niwa-history-search--cache'.")

(defun niwa-history-search--remember (key digest text)
  "Remember TEXT for KEY and DIGEST within the disposable payload bound."
  (when-let* ((old (gethash key niwa-history-search--cache)))
    (cl-decf niwa-history-search--cache-bytes (string-bytes (cdr old))))
  (remhash key niwa-history-search--cache)
  (when (> (+ niwa-history-search--cache-bytes (string-bytes text))
           niwa-history-search--cache-limit)
    (clrhash niwa-history-search--cache)
    (setq niwa-history-search--cache-bytes 0))
  (when (<= (string-bytes text) niwa-history-search--cache-limit)
    (puthash key (cons digest text) niwa-history-search--cache)
    (cl-incf niwa-history-search--cache-bytes (string-bytes text)))
  text)

(iter-defun niwa-history-search--prepare (workspace components args directory)
  "Yield preparation steps for WORKSPACE and selected COMPONENTS.
ARGS carries ordinary search options. DIRECTORY receives disposable files.
The final yielded value is (:ready SOURCES), using relative public names."
  (let* ((root (niwa-session-artifacts-sessions-dir workspace))
         (mode (niwa-session-authority-mode-for-workspace workspace))
         (selected-session (cadr components))
         (selected-source (caddr components))
         (contents (or (eq (plist-get args :operation) 'grep)
                       (and selected-source (eq (plist-get args :operation) 'read))))
         (glob (and (eq (plist-get args :operation) 'grep) (plist-get args :glob)))
         (skipped 0)
         entries sources)
    (iter-yield 'discovery)
    (when (file-directory-p root)
      (setq entries
            (if selected-session
                (list (file-name-concat root selected-session))
              (directory-files root t "\\`[^.]" t))))
    (while entries
      (let ((batch (seq-take entries 32)) entries-valid)
        (setq entries (nthcdr (length batch) entries))
        (dolist (path batch)
          (iter-yield 'discovery)
          (when (and (file-directory-p path)
                     (not (file-symlink-p (directory-file-name path)))
                     (file-in-directory-p path root))
            (push path entries-valid)))
        (setq entries-valid (nreverse entries-valid))
        (niwa-session-durability-with-transaction
          (iter-yield 'discovery)
          (let ((controls (niwa-session-persistence--control-artifacts entries-valid))
                listings observations)
            (iter-yield 'discovery)
            (setq listings (niwa-session-persistence--lease-listings entries-valid))
            (iter-yield 'discovery)
            (setq observations
                  (niwa-session-publication-read-batch entries-valid listings))
            (dolist (path entries-valid)
              (iter-yield 'discovery)
              (let* ((entry
                      (condition-case nil
                          (niwa-session-persistence--discover-entry
                           path mode (cdr (assoc path controls)) (cdr (assoc path listings))
                           (cdr (assoc path observations)))
                        (error nil)))
                     (publication (plist-get entry :publication)))
                (when (or (null entry) (eq (plist-get entry :kind) 'incompatible))
                  (cl-incf skipped))
                (when (and entry (not (eq (plist-get entry :kind) 'incompatible)))
                  (dolist (logical (niwa-session-artifacts--cold-segments publication))
                    (let* ((name (concat (file-name-nondirectory path) "/" logical))
                           (artifact (cdr (assoc logical (plist-get publication :artifacts)))))
                      (when (or (null selected-source) (equal logical selected-source))
                        (push (list :name name :key (file-name-concat path logical)
                                    :path (plist-get artifact :published)
                                    :digest (plist-get artifact :sha256)) sources)))))))))))
    (setq sources (sort sources :key (lambda (source) (plist-get source :name))
                        :lessp #'string-lessp))
    (when (and selected-session (null sources) (not glob))
      (error "Saved conversation is unavailable; Read history://saved to discover sources"))
    (let ((remaining sources))
      (while remaining
        (let* ((batch (seq-take remaining 32))
               (results
                (when contents
                  (iter-yield 'reads)
                  (niwa-session-control-fs-run-program
                   (mapcar (lambda (source)
                             (list :op 'read :path (plist-get source :path)
                                   :coding 'no-conversion)) batch)))))
          (setq remaining (nthcdr (length batch) remaining))
          (dolist (source batch)
            (let* ((bytes (and contents (niwa-session-control-fs-program-value (pop results))))
                   (digest (and bytes (secure-hash 'sha256 bytes)))
                   (key (plist-get source :key))
                   (cached (and contents (gethash key niwa-history-search--cache)))
                   (name (plist-get source :name))
                   (file (file-name-concat directory name))
                   (text ""))
              (when (and (plist-get source :digest) contents
                         (not (equal digest (plist-get source :digest))))
                (error "Published conversation failed verification: %s" name))
              (when contents
                (if (and cached (equal digest (car cached)))
                    (setq text (cdr cached))
                  (let ((projection-buffer (generate-new-buffer " *history-projection*")))
                    (unwind-protect
                        (progn
                          (iter-yield 'restoration)
                          (with-current-buffer projection-buffer
                            (insert (decode-coding-string bytes 'utf-8-unix))
                            (niwa--transcript-org-mode)
                            (niwa-transcript-restore-properties))
                          (iter-yield 'projection)
                          (with-current-buffer projection-buffer
                            (setq text
                                  (concat "Source: history://saved/"
                                          (mapconcat #'niwa-resource-encode-component
                                                     (split-string name "/") "/")
                                          "\nHistorical conversation; later corrections supersede earlier statements.\n\n"
                                          (niwa-agent-conversation-project-history (current-buffer))))))
                      (kill-buffer projection-buffer)))
                  (niwa-history-search--remember key digest text)))
              (iter-yield 'temporary-preparation)
              (make-directory (file-name-directory file) t)
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region text nil file nil 'silent)))))))
    (iter-yield (list :ready sources :skipped skipped))))

;;;###autoload
(defun niwa-history-search-start (callback args descriptor)
  "Run an authorized saved-history operation from DESCRIPTOR with ARGS.
Return an idempotent canceller immediately. CALLBACK receives one ordinary
handler result; owner teardown drops delivery and releases owned resources."
  (let* ((workspace (plist-get descriptor :history-workspace))
         (components (plist-get descriptor :history-components))
         (operation (plist-get descriptor :operation))
         (address (plist-get descriptor :address))
         (request (bound-and-true-p niwa--current-request))
         (origin-buffer (current-buffer))
         (invocation niwa--agent-invocation)
         (args (plist-put (copy-sequence args) :operation operation))
         directory iterator timer helper finished stepping delivery skipped resume-step)
    (cl-labels
     ((cleanup ()
        ;; A producer may settle or be cancelled while its launch is still
        ;; returning. Close the iterator only once that step has unwound.
        (unless stepping
          (when timer (cancel-timer timer) (setq timer nil))
          (when iterator (iter-close iterator) (setq iterator nil))
          (when (buffer-live-p origin-buffer)
            (with-current-buffer origin-buffer
              (remove-hook 'kill-buffer-hook #'teardown t)))
          (when directory (ignore-errors (delete-directory directory t)))
          (when delivery
            (let ((result delivery))
              (setq delivery nil)
              (when (and (buffer-live-p origin-buffer)
                         (niwa-execution--owner-admissible-p invocation))
                (funcall callback result))))))
      (finish (result)
        (unless finished
          (when (and skipped (> skipped 0) (not (eq (plist-get result :status) 'error)))
            (setq result (copy-sequence result))
            (plist-put result :result
                       (concat (plist-get result :result)
                               (format "\n\nSkipped %d unavailable or incompatible saved sessions." skipped))))
          (setq finished t delivery result)
          (cleanup)))
      (cancel (&optional quiet)
        (unless finished
          (setq finished t
                delivery (unless quiet '(:result "History operation cancelled." :status cancelled)))
          (when helper (funcall helper))
          (cleanup)))
      (teardown () (cancel t))
      (ready (sources skipped-count)
        (setq skipped skipped-count)
        (if (eq operation 'read)
            (let ((text
                   (if (caddr components)
                       (with-temp-buffer
                         (insert-file-contents (file-name-concat directory (plist-get (car sources) :name)))
                         (buffer-string))
                     (if sources
                         (mapconcat
                          (lambda (source)
                            (concat "history://saved/"
                                    (mapconcat #'niwa-resource-encode-component
                                               (split-string (plist-get source :name) "/") "/")))
                          sources "\n")
                       "No saved workspace history"))))
              (finish (list :result (niwa-tool-fs-read--virtual-text text args address))))
          (let ((native (copy-sequence args))
                (default-directory (file-name-as-directory directory))
                (niwa--session nil)
                (niwa--current-request request)
                (niwa--agent-invocation invocation)
                (niwa-tool-fs-search--resource-dispatching t)
                (niwa-tool-fs-search--resource-address address)
                (niwa-tool-fs-search--teardown #'teardown))
            (plist-put native :path nil)
            (plist-put native :resource-address address)
            (plist-put native :resource-roots
                       (list (list :path directory :address "history://saved")))
            (setq helper
                  (funcall (if (eq operation 'glob) #'niwa-tool-fs-search-glob
                             #'niwa-tool-fs-search-grep)
                           #'finish native))
            (when (and finished helper) (funcall helper)))))
      (step ()
        (setq timer nil stepping t)
        (unwind-protect
            (unless finished
              (cond
               ((or (not (buffer-live-p origin-buffer))
                    (not (niwa-execution--owner-admissible-p invocation)))
                (teardown))
               ((niwa-transport-busy-p (niwa-workspace-root workspace))
                (setq timer (run-at-time 0.02 nil resume-step)))
               (t
                (condition-case err
                    (progn
                      (unless directory
                        (setq directory (make-temp-file "niwa-history-search-" t)
                              iterator (niwa-history-search--prepare workspace components args directory)))
                      ;; Cheap yields can share a short event-loop turn.
                      ;; Expensive native operations still end that turn.
                      (let ((deadline (+ (float-time) 0.005)) ready-p)
                        (while (and (not finished) (not ready-p)
                                    (< (float-time) deadline))
                          (let ((value (iter-next iterator)))
                            (when (and (not finished) (listp value)
                                       (plist-member value :ready))
                              (setq ready-p t)
                              (ready (plist-get value :ready) (plist-get value :skipped)))))
                        (unless (or finished ready-p)
                          (setq timer (run-at-time 0.001 nil resume-step)))))
                  (error
                   (finish (list :result
                                 (concat "Error: "
                                         (niwa-resource-error-message
                                          err address (list directory (niwa-session-artifacts-sessions-dir workspace))))
                                 :status 'error)))))))
          (setq stepping nil)
          (when finished (cleanup)))))
     (with-current-buffer origin-buffer (add-hook 'kill-buffer-hook #'teardown nil t))
     (niwa-request-push-canceller request #'teardown)
     (setq resume-step (niwa-pipeline-handler-resumable #'step)
           timer (run-at-time 0 nil resume-step))
     #'cancel)))

(provide 'niwa-history-search)
;;; niwa-history-search.el ends here
