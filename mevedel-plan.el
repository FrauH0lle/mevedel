;;; mevedel-plan.el -- Plan data and artifacts -*- lexical-binding: t -*-

;;; Commentary:

;; Lifecycle-neutral plan parsing, artifact persistence, acceptance metadata,
;; and implementation inputs.  Goal workflows choose storage paths without
;; duplicating these operations.

;;; Code:

(eval-when-compile
  (require 'mevedel-structs))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-ensure-files
                  "mevedel-session-persistence" (session buffer))

;; `mevedel-skills-ui'
(declare-function mevedel-skills--refresh-view-input-prompt
                  "mevedel-skills-ui" ())

;; `mevedel-structs'
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text
                  "mevedel-utilities" (text))

(defconst mevedel-plan--open-tag "<proposed_plan>"
  "Opening tag for proposed plans.")

(defconst mevedel-plan--close-tag "</proposed_plan>"
  "Closing tag for proposed plans.")

(defconst mevedel-plan--relative-current-path
  (file-name-concat "plans" "current.md")
  "Relative path of the mutable current plan under a session directory.")


;;
;;; Plan conversation mode

(defun mevedel-plan--current-session (&optional session)
  "Return SESSION or the session reachable from the current buffer."
  (or session
      (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-plan-mode-active-p (&optional session)
  "Return non-nil when SESSION is in a Plan conversation."
  (when-let* ((session (mevedel-plan--current-session session)))
    (mevedel-session-plan-mode session)))

(defun mevedel-plan-mode-enter (&optional session)
  "Enter a sticky Plan conversation for SESSION."
  (interactive)
  (let ((session (mevedel-plan--current-session session)))
    (unless session
      (user-error "No mevedel session for Plan mode"))
    (when-let* ((goal (mevedel-session-goal session)))
      (unless (eq (mevedel-goal-status goal) 'complete)
        (user-error "Finish or clear the current Goal before entering Plan")))
    (setf (mevedel-session-plan-mode session) t)
    (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
      (mevedel-skills--refresh-view-input-prompt))
    (force-mode-line-update t)
    t))

(defun mevedel-plan-mode-exit (&optional session)
  "Leave the Plan conversation for SESSION."
  (interactive)
  (let ((session (mevedel-plan--current-session session)))
    (when session
      (setf (mevedel-session-plan-mode session) nil)
      (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
        (mevedel-skills--refresh-view-input-prompt))
      (force-mode-line-update t))
    nil))

(defun mevedel-plan-validate (plan-markdown)
  "Return normalized PLAN-MARKDOWN or signal when it is invalid."
  (unless (stringp plan-markdown)
    (error "Plan must be a string"))
  (require 'mevedel-utilities)
  (let ((plan-markdown (mevedel--normalize-message-text plan-markdown)))
    (when (string-blank-p plan-markdown)
      (error "Plan must not be blank"))
    plan-markdown))

(defun mevedel-plan-extract-proposed (text)
  "Return the last proposed-plan body found in TEXT, or nil.
Only exact line-oriented `<proposed_plan>' blocks are recognized."
  (let ((case-fold-search nil)
        found)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-quote mevedel-plan--open-tag)
                      "[ \t]*\n")
              nil t)
        (let ((body-start (point)))
          (when (re-search-forward
                 (concat "^" (regexp-quote mevedel-plan--close-tag)
                         "[ \t]*$")
                 nil t)
            (setq found
                  (string-trim-right
                   (buffer-substring-no-properties
                    body-start (match-beginning 0)))))))
      found)))

(defun mevedel-plan-strip-proposed (text)
  "Return TEXT with proposed-plan blocks removed."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-quote mevedel-plan--open-tag)
                      "[ \t]*\n")
              nil t)
        (let ((start (match-beginning 0)))
          (if (re-search-forward
               (concat "^" (regexp-quote mevedel-plan--close-tag)
                       "[ \t]*\n?")
               nil t)
              (delete-region start (match-end 0))
            (delete-region start (point-max)))
          (goto-char start)))
      (string-trim (buffer-string)))))

(defun mevedel-plan--metadata-put (session key value)
  "Set KEY to VALUE in SESSION's plan metadata."
  (require 'mevedel-structs)
  (let ((metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                     nil))))
    (setq metadata (plist-put metadata key value))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))

(defun mevedel-plan-hash (plan-markdown)
  "Return a stable hash for PLAN-MARKDOWN."
  (secure-hash 'sha256 (string-trim-right (or plan-markdown ""))))

(defun mevedel-plan-known-p (plan-markdown &optional session)
  "Return non-nil if PLAN-MARKDOWN was presented in SESSION."
  (require 'mevedel-structs)
  (when-let* ((session (or session mevedel--session))
              (hashes (plist-get (mevedel-session-plan-metadata session)
                                 :presented-plan-hashes)))
    (member (mevedel-plan-hash plan-markdown) hashes)))

(defun mevedel-plan-current-path (&optional session buffer relative-path)
  "Return the session-local current plan path for SESSION.
Materialize the session directory when needed.  BUFFER defaults to the
current data buffer."
  (require 'mevedel-structs)
  (let* ((session (or session mevedel--session))
         (buffer (or buffer (current-buffer)))
         (save-path (or (mevedel-session-save-path session)
                        (progn
                          (require 'mevedel-session-persistence)
                          (mevedel-session-persistence-ensure-files
                           session buffer)))))
    (unless save-path
      (error "Could not materialize session for plan"))
    (file-name-concat save-path
                      (or relative-path mevedel-plan--relative-current-path))))

(defun mevedel-plan--metadata-path (session)
  "Return SESSION's recorded current plan path, when available."
  (require 'mevedel-structs)
  (let ((metadata (mevedel-session-plan-metadata session)))
    (or (when-let* ((save-path (mevedel-session-save-path session))
                    (path (or (plist-get metadata :path)
                              mevedel-plan--relative-current-path)))
          (file-name-concat save-path path))
        (plist-get metadata :absolute-path))))

(defun mevedel-plan-write-current
    (plan-markdown session buffer &optional relative-path)
  "Write PLAN-MARKDOWN to SESSION's current plan artifact for BUFFER.
RELATIVE-PATH overrides the default path below SESSION's save directory.
Return an explicit artifact plist containing `:path', `:absolute-path', and
`:hash'."
  (require 'mevedel-structs)
  (let* ((relative-path (or relative-path
                            mevedel-plan--relative-current-path))
         (path (mevedel-plan-current-path session buffer relative-path))
         (plan-markdown (mevedel-plan-validate plan-markdown))
         (hash (mevedel-plan-hash plan-markdown)))
    (make-directory (file-name-directory path) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region plan-markdown nil path nil 'silent))
    (let ((turn (or (mevedel-session-turn-count session) 0))
          (metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                       nil))))
      (setq metadata (plist-put metadata :path relative-path))
      (setq metadata (plist-put metadata :absolute-path path))
      (setq metadata (plist-put metadata :hash hash))
      (setq metadata (plist-put metadata :status 'presented))
      (setq metadata (plist-put metadata :updated-turn turn))
      (setq metadata (plist-put metadata :updated-at
                                (format-time-string "%FT%H-%M-%S")))
      (setq metadata
            (plist-put metadata :presented-plan-hashes
                       (delete-dups
                        (cons hash
                              (plist-get metadata :presented-plan-hashes)))))
      (setq metadata (plist-put metadata :verification-pending nil))
      (setq metadata (plist-put metadata :approved-turn nil))
      (setq metadata (plist-put metadata :approved-at nil))
      (setf (mevedel-session-plan-metadata session) metadata))
    (list :path relative-path
          :absolute-path path
          :hash hash)))

(defun mevedel-plan-archive-accepted
    (current-artifact session &optional relative-path)
  "Archive CURRENT-ARTIFACT as an accepted plan for SESSION.
CURRENT-ARTIFACT is the plist returned by `mevedel-plan-write-current'.
RELATIVE-PATH names a deterministic immutable destination when non-nil.
Return a plist containing `:path' and `:absolute-path'."
  (require 'mevedel-structs)
  (let ((plan-path (plist-get current-artifact :absolute-path))
        (plan-hash (plist-get current-artifact :hash)))
    (unless (and plan-path plan-hash (file-exists-p plan-path))
      (error "Accepted plan artifact does not exist"))
    (let* ((save-path (mevedel-session-save-path session))
           (dir (file-name-directory plan-path))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (archive-path
            (if relative-path
                (file-name-concat save-path relative-path)
              (file-name-concat dir (format "accepted-%s.md" timestamp))))
           (index 1))
      (unless relative-path
        (while (file-exists-p archive-path)
          (setq archive-path
                (file-name-concat dir (format "accepted-%s-%d.md"
                                              timestamp index)))
          (setq index (1+ index))))
      (make-directory (file-name-directory archive-path) t)
      (if (file-exists-p archive-path)
          (with-temp-buffer
            (insert-file-contents archive-path)
            (unless (and relative-path
                         (equal plan-hash
                                (mevedel-plan-hash (buffer-string))))
              (error "Accepted plan artifact already exists with different content")))
        (copy-file plan-path archive-path))
      (list :path (and save-path (file-relative-name archive-path save-path))
            :absolute-path archive-path))))

(defun mevedel-plan-current-body (&optional session)
  "Return SESSION's current plan artifact contents, or nil."
  (require 'mevedel-utilities)
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan--metadata-path session)))
      (when (and path (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (mevedel--normalize-message-text (buffer-string)))))))

(defun mevedel-plan-current-exists-p (&optional session)
  "Return non-nil when SESSION has a current plan artifact on disk."
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan--metadata-path session)))
      (and path (file-exists-p path)))))

(defun mevedel-plan-mark-approved
    (session current-artifact accepted-artifact &optional skip-verification)
  "Mark SESSION's CURRENT-ARTIFACT as approved.
ACCEPTED-ARTIFACT identifies the immutable archived plan.  When
SKIP-VERIFICATION is non-nil, do not leave verification pending."
  (require 'mevedel-structs)
  (let ((metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                     nil))))
    (setq metadata (plist-put metadata :path
                              (plist-get current-artifact :path)))
    (setq metadata (plist-put metadata :status 'approved))
    (setq metadata (plist-put metadata :approved-turn
                              (or (mevedel-session-turn-count session) 0)))
    (setq metadata (plist-put metadata :approved-at
                              (format-time-string "%FT%H-%M-%S")))
    (setq metadata (plist-put metadata :verification-pending
                              (not skip-verification)))
    (setq metadata (plist-put metadata :absolute-path
                              (plist-get current-artifact :absolute-path)))
    (setq metadata (plist-put metadata :accepted-path
                              (plist-get accepted-artifact :path)))
    (setq metadata (plist-put metadata :accepted-absolute-path
                              (plist-get accepted-artifact :absolute-path)))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))

(defun mevedel-plan-accept
    (plan-markdown session buffer &optional skip-verification
                   current-relative-path accepted-relative-path)
  "Persist and approve PLAN-MARKDOWN for SESSION and BUFFER.
CURRENT-RELATIVE-PATH and ACCEPTED-RELATIVE-PATH override artifact locations.
Return `(:current ARTIFACT :accepted ARTIFACT)' for later dispatch."
  (let* ((current (mevedel-plan-write-current
                   plan-markdown session buffer current-relative-path))
         (accepted (mevedel-plan-archive-accepted
                    current session accepted-relative-path)))
    (mevedel-plan-mark-approved
     session current accepted skip-verification)
    (list :current current :accepted accepted)))

(defun mevedel-plan-implementation-input
    (context current-artifact permission-mode goal-context)
  "Return a validated implementation input plist.
CONTEXT is `full' or `focused', CURRENT-ARTIFACT is returned by
`mevedel-plan-write-current', PERMISSION-MODE is the implementation permission
mode.  GOAL-CONTEXT is the authoritative persisted lifecycle fragment."
  (unless (memq context '(full focused))
    (error "Unknown implementation context: %s" context))
  (unless (memq permission-mode '(ask auto full-auto))
    (error "Unknown implementation permission mode: %s" permission-mode))
  (unless (and (stringp goal-context) (not (string-blank-p goal-context)))
    (error "Implementation requires Goal context"))
  (let ((path (plist-get current-artifact :absolute-path)))
    (unless (and path (file-exists-p path))
      (error "Current plan artifact does not exist"))
    (list :context context
          :plan-file path
          :permission-mode permission-mode
          :goal-context goal-context)))

(defun mevedel-plan-clear-verification-pending (&optional session)
  "Clear SESSION's approved-plan verification pending flag."
  (when-let* ((session (or session (and (boundp 'mevedel--session)
                                        mevedel--session))))
    (mevedel-plan--metadata-put session :verification-pending nil)))

(provide 'mevedel-plan)
;;; mevedel-plan.el ends here
