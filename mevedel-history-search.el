;;; mevedel-history-search.el --- Search filtered saved conversations -*- lexical-binding: t -*-

;;; Commentary:
;; Workspace history access without resuming sessions. Native persistence owns
;; authority and transcript classification owns filtering. This module prepares
;; disposable projections in bounded steps and reuses ordinary resource tools.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'mevedel-agent-conversation)
(require 'mevedel-session-persistence)
(require 'mevedel-transport)
(require 'mevedel-turn)

;; `mevedel-execution'
(declare-function mevedel-execution--owner-admissible-p
                  "mevedel-execution" (owner-context))
(autoload 'mevedel-execution--owner-admissible-p "mevedel-execution")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-read-batch
                  "mevedel-session-publication" (directories listings))
(autoload 'mevedel-session-publication-read-batch "mevedel-session-publication")

;; `mevedel-structs'
(declare-function mevedel-session-authority-mode-for-workspace
                  "mevedel-structs" (workspace))

;; `mevedel-tool-fs-read'
(declare-function mevedel-tool-fs-read--virtual-text
                  "mevedel-tool-fs-read" (text args address))

;; `mevedel-tool-fs-search'
(declare-function mevedel-tool-fs-search-glob
                  "mevedel-tool-fs-search" (callback args))
(declare-function mevedel-tool-fs-search-grep
                  "mevedel-tool-fs-search" (callback args))
(defvar mevedel-tool-fs-search--resource-address)
(defvar mevedel-tool-fs-search--resource-dispatching)
(defvar mevedel-tool-fs-search--teardown)

(defconst mevedel-history-search--cache-limit (* 64 1024 1024)
  "Maximum retained projection payload bytes; not a process memory limit.")

(defvar mevedel-history-search--cache (make-hash-table :test #'equal)
  "Disposable projections keyed by logical source path, as (SHA256 . TEXT).")

(defvar mevedel-history-search--cache-bytes 0
  "Payload bytes retained in `mevedel-history-search--cache'.")

(defun mevedel-history-search--remember (key digest text)
  "Remember TEXT for KEY and DIGEST within the disposable payload bound."
  (when-let* ((old (gethash key mevedel-history-search--cache)))
    (cl-decf mevedel-history-search--cache-bytes (string-bytes (cdr old))))
  (remhash key mevedel-history-search--cache)
  (when (> (+ mevedel-history-search--cache-bytes (string-bytes text))
           mevedel-history-search--cache-limit)
    (clrhash mevedel-history-search--cache)
    (setq mevedel-history-search--cache-bytes 0))
  (when (<= (string-bytes text) mevedel-history-search--cache-limit)
    (puthash key (cons digest text) mevedel-history-search--cache)
    (cl-incf mevedel-history-search--cache-bytes (string-bytes text)))
  text)

(iter-defun mevedel-history-search--prepare (workspace components args directory)
  "Yield preparation steps for WORKSPACE and selected COMPONENTS.
ARGS carries ordinary search options. DIRECTORY receives disposable files.
The final yielded value is (:ready SOURCES), using relative public names."
  (let* ((root (mevedel-session-artifacts-sessions-dir workspace))
         (mode (mevedel-session-authority-mode-for-workspace workspace))
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
        (mevedel-session-durability-with-transaction
          (iter-yield 'discovery)
          (let ((controls (mevedel-session-persistence--control-artifacts entries-valid))
                listings observations)
            (if (eq mode 'portable)
                (progn
                  (iter-yield 'discovery)
                  (setq listings (mevedel-session-persistence--lease-listings entries-valid))
                  (iter-yield 'discovery)
                  (setq observations
                        (mevedel-session-publication-read-batch entries-valid listings)))
              (iter-yield 'discovery)
              (setq observations
                    (mevedel-session-persistence-read-sidecar-batch entries-valid)))
            (dolist (path entries-valid)
              (iter-yield 'discovery)
              (let* ((entry
                      (condition-case nil
                          (mevedel-session-persistence--discover-entry
                           path mode (cdr (assoc path controls)) (cdr (assoc path listings))
                           (cdr (assoc path observations)))
                        (error nil)))
                     (publication (plist-get entry :publication)))
                (when (or (null entry) (eq (plist-get entry :kind) 'incompatible))
                  (cl-incf skipped))
                (when (and entry (not (eq (plist-get entry :kind) 'incompatible)))
                  (dolist (logical (mevedel-session-artifacts--cold-segments path mode publication))
                    (let* ((name (concat (file-name-nondirectory path) "/" logical))
                           (artifact (cdr (assoc logical (plist-get publication :artifacts)))))
                      (when (or (null selected-source) (equal logical selected-source))
                        (push (list :name name :key (file-name-concat path logical)
                                    :path (if publication (plist-get artifact :published)
                                            (file-name-concat path logical))
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
                  (mevedel-session-control-fs-run-program
                   (mapcar (lambda (source)
                             (list :op 'read :path (plist-get source :path)
                                   :coding 'no-conversion)) batch)))))
          (setq remaining (nthcdr (length batch) remaining))
          (dolist (source batch)
            (let* ((bytes (and contents (mevedel-session-control-fs-program-value (pop results))))
                   (digest (and bytes (secure-hash 'sha256 bytes)))
                   (key (plist-get source :key))
                   (cached (and contents (gethash key mevedel-history-search--cache)))
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
                            (mevedel--transcript-org-mode)
                            (mevedel-transcript-restore-properties))
                          (iter-yield 'projection)
                          (with-current-buffer projection-buffer
                            (setq text
                                  (concat "Source: history://saved/"
                                          (mapconcat #'mevedel-resource-encode-component
                                                     (split-string name "/") "/")
                                          "\nHistorical conversation; later corrections supersede earlier statements.\n\n"
                                          (mevedel-agent-conversation-project-history (current-buffer))))))
                      (kill-buffer projection-buffer)))
                  (mevedel-history-search--remember key digest text)))
              (iter-yield 'temporary-preparation)
              (make-directory (file-name-directory file) t)
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region text nil file nil 'silent)))))))
    (iter-yield (list :ready sources :skipped skipped))))

;;;###autoload
(defun mevedel-history-search-start (callback args descriptor)
  "Run an authorized saved-history operation from DESCRIPTOR with ARGS.
Return an idempotent canceller immediately. CALLBACK receives one ordinary
handler result; owner teardown drops delivery and releases owned resources."
  (let* ((workspace (plist-get descriptor :history-workspace))
         (components (plist-get descriptor :history-components))
         (operation (plist-get descriptor :operation))
         (address (plist-get descriptor :address))
         (request (bound-and-true-p mevedel--current-request))
         (origin-buffer (current-buffer))
         (invocation (bound-and-true-p mevedel--agent-invocation))
         (args (plist-put (copy-sequence args) :operation operation))
         directory iterator timer helper finished stepping delivery skipped)
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
                         (mevedel-execution--owner-admissible-p invocation))
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
                                    (mapconcat #'mevedel-resource-encode-component
                                               (split-string (plist-get source :name) "/") "/")))
                          sources "\n")
                       "No saved workspace history"))))
              (finish (list :result (mevedel-tool-fs-read--virtual-text text args address))))
          (let ((native (copy-sequence args))
                (default-directory (file-name-as-directory directory))
                (mevedel--session nil)
                (mevedel--current-request request)
                (mevedel--agent-invocation invocation)
                (mevedel-tool-fs-search--resource-dispatching t)
                (mevedel-tool-fs-search--resource-address address)
                (mevedel-tool-fs-search--teardown #'teardown))
            (plist-put native :path nil)
            (plist-put native :resource-address address)
            (plist-put native :resource-roots
                       (list (list :path directory :address "history://saved")))
            (setq helper
                  (funcall (if (eq operation 'glob) #'mevedel-tool-fs-search-glob
                             #'mevedel-tool-fs-search-grep)
                           #'finish native))
            (when (and finished helper) (funcall helper)))))
      (step ()
        (setq timer nil stepping t)
        (unwind-protect
            (unless finished
              (cond
               ((or (not (buffer-live-p origin-buffer))
                    (not (mevedel-execution--owner-admissible-p invocation)))
                (teardown))
               ((mevedel-transport-busy-p (mevedel-workspace-root workspace))
                (setq timer (run-at-time 0.02 nil #'step)))
               (t
                (condition-case err
                    (progn
                      (unless directory
                        (setq directory (make-temp-file "mevedel-history-search-" t)
                              iterator (mevedel-history-search--prepare workspace components args directory)))
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
                          (setq timer (run-at-time 0.001 nil #'step)))))
                  (error
                   (finish (list :result
                                 (concat "Error: "
                                         (mevedel-resource-error-message
                                          err address (list directory (mevedel-session-artifacts-sessions-dir workspace))))
                                 :status 'error)))))))
          (setq stepping nil)
          (when finished (cleanup)))))
     (with-current-buffer origin-buffer (add-hook 'kill-buffer-hook #'teardown nil t))
     (mevedel-request-push-canceller request #'teardown)
     (setq timer (run-at-time 0 nil #'step))
     #'cancel)))

(provide 'mevedel-history-search)
;;; mevedel-history-search.el ends here
