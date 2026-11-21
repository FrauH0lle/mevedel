;;; niwa-plan.el -- Plan data and artifacts -*- lexical-binding: t -*-

;;; Commentary:

;; Lifecycle-neutral plan parsing, artifact persistence, acceptance metadata,
;; and implementation inputs.  Goal workflows choose storage paths without
;; duplicating these operations.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-structs)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-ensure-files
                  "niwa-session-artifacts" (session buffer))
(declare-function niwa-session-artifacts-publish-text
                  "niwa-session-artifacts"
                  (session path content &optional coding))
(declare-function niwa-session-artifacts-read-artifact
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-ensure-files
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-text
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-artifact
  "niwa-session-artifacts")

;; `niwa-structs'
(declare-function niwa-session-plan-metadata "niwa-structs"
		  (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x)
		  t)
(defvar niwa--session)

;; `niwa-utilities'
(declare-function niwa--normalize-message-text "niwa-utilities"
		  (text))
(autoload 'niwa--normalize-message-text "niwa-utilities")

(defconst niwa-plan--open-regexp "[ \t]*<proposed_plan>[ \t]*\n"
  "Opening plan delimiter, including its required following newline.
Assistant message boundaries may be flattened without a preceding newline.")

(defconst niwa-plan--close-regexp "^[ \t]*</proposed_plan>[ \t]*$"
  "Closing plan delimiter on its own, optionally indented line.")

(defconst niwa-plan--relative-current-path
  (file-name-concat "local" "plans" "current.md")
  "Relative path of the mutable current plan under a session directory.")

;;
;;; Plan data
(defun niwa-plan-validate (plan-markdown)
  "Return normalized PLAN-MARKDOWN or signal when it is invalid."
  (unless (stringp plan-markdown)
    (error "Plan must be a string"))
  (let ((plan-markdown (niwa--normalize-message-text plan-markdown)))
    (when (string-blank-p plan-markdown)
      (error "Plan must not be blank"))
    plan-markdown))

(defun niwa-plan-extract-proposed (text)
  "Return the last proposed-plan body found in TEXT, or nil.
The opening tag must end its line; the closing tag must occupy its own
line.  Tags may be indented, and prose may precede the opening tag."
  (let ((case-fold-search nil)
        found)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward niwa-plan--open-regexp nil t)
        (let ((body-start (point)))
          (when (re-search-forward niwa-plan--close-regexp nil t)
            (setq found
                  (string-trim-right
                   (buffer-substring-no-properties
                    body-start (match-beginning 0)))))))
      found)))

(defun niwa-plan-strip-proposed (text)
  "Return TEXT with proposed-plan blocks removed."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward niwa-plan--open-regexp nil t)
        (let ((start (match-beginning 0)))
          (if (re-search-forward niwa-plan--close-regexp nil t)
              (progn
                (when (looking-at "\n") (forward-char))
                (delete-region start (point)))
            (delete-region start (point-max)))
          (goto-char start)))
      (string-trim (buffer-string)))))

(defun niwa-plan--metadata-put (session key value)
  "Set KEY to VALUE in SESSION's plan metadata."
  (let ((metadata (copy-sequence (or (niwa-session-plan-metadata session)
                                     nil))))
    (setq metadata (plist-put metadata key value))
    (setf (niwa-session-plan-metadata session) metadata)
    metadata))


(defun niwa-plan-hash (plan-markdown)
  "Return a stable hash for PLAN-MARKDOWN."
  (secure-hash 'sha256 (string-trim-right (or plan-markdown ""))))

(defun niwa-plan-resource-address (relative-path)
  "Return the canonical local resource address for RELATIVE-PATH.
RELATIVE-PATH must name the current plan or an immutable accepted archive
below `local/plans/'."
  (unless (and (stringp relative-path)
               (not (file-name-absolute-p relative-path)))
    (error "Plan artifact path is not relative"))
  (let ((tail
         (cond
          ((equal relative-path niwa-plan--relative-current-path)
           "current.md")
          ((string-match
            "\\`local/plans/\\(accepted-[^/]+\\.md\\)\\'"
            relative-path)
           (match-string 1 relative-path))
          (t (error "Plan artifact path is outside managed plan storage")))))
    (concat "work://plans/" tail)))

(defun niwa-plan-artifact-path-p (path)
  "Return non-nil when PATH is a normalized session-relative artifact path."
  (and (stringp path)
       (not (string-empty-p path))
       (not (file-name-absolute-p path))
       (not (file-remote-p path))
       (not (string-prefix-p "~" path))
       (not (string-suffix-p "/" path))
       (not (member path '(".lease" ".publications")))
       (not (string-prefix-p ".lease/" path))
       (not (string-prefix-p ".publications/" path))
       (let ((components (split-string path "/" nil)))
         (and (not (member "" components))
              (not (member "." components))
              (not (member ".." components))))))

(defun niwa-plan-current-path (&optional session buffer relative-path)
  "Return the session-local current plan path for SESSION.
Materialize the session directory when needed.  BUFFER defaults to the
current data buffer."
  (let* ((session (or session niwa--session))
         (buffer (or buffer (current-buffer)))
         (save-path (or (niwa-session-save-path session)
                        (progn
                          (niwa-session-artifacts-ensure-files
                           session buffer)))))
    (unless save-path
      (error "Could not materialize session for plan"))
    (let ((relative (or relative-path niwa-plan--relative-current-path)))
      (unless (niwa-plan-artifact-path-p relative)
        (error "Invalid plan artifact path: %S" relative))
      (file-name-concat save-path relative))))

(defun niwa-plan-artifact-path (session artifact)
  "Return ARTIFACT's qualified logical path in SESSION.

ARTIFACT must carry one normalized session-relative `:path'.  The returned
path names the logical artifact, never an immutable publication file."
  (let* ((save-path (niwa-session-save-path session))
         (relative (plist-get artifact :path)))
    (unless (and save-path (niwa-plan-artifact-path-p relative))
      (error "Invalid plan artifact path: %S" relative))
    (file-name-concat save-path relative)))

(defun niwa-plan--read-path (session relative)
  "Return SESSION artifact RELATIVE decoded as UTF-8 text."
  (decode-coding-string
   (niwa-session-artifacts-read-artifact session relative)
   'utf-8-unix))

(defun niwa-plan-read-artifact (session artifact)
  "Return SESSION's verified accepted-plan ARTIFACT body."
  (let ((relative (plist-get artifact :path))
        (hash (plist-get artifact :hash)))
    (unless (and (stringp relative) (stringp hash))
      (error "Accepted plan artifact is unavailable"))
    ;; Resolve the logical display path as a structural validation before the
    ;; publication resolver accepts the artifact name.
    (niwa-plan-artifact-path session artifact)
    (let ((body
           (niwa--normalize-message-text
            (niwa-plan--read-path session relative))))
      (unless (equal hash (niwa-plan-hash body))
        (error "Accepted plan artifact hash does not match"))
      body)))

(defun niwa-plan--publish-current
    (plan-markdown session buffer &optional relative-path)
  "Publish PLAN-MARKDOWN as SESSION's current plan artifact for BUFFER.
RELATIVE-PATH overrides the default path below SESSION's save directory.
Return an explicit artifact plist containing `:path' and `:hash'.

Publication only: the caller decides when the session's plan metadata
follows, which matters when a second publication can still fail."
  (let* ((relative-path (or relative-path
                            niwa-plan--relative-current-path))
         (path (niwa-plan-current-path session buffer relative-path))
         (plan-markdown (niwa-plan-validate plan-markdown))
         (hash (niwa-plan-hash plan-markdown)))
    (niwa-session-artifacts-publish-text
     session path plan-markdown 'utf-8-unix)
    (list :path relative-path :hash hash)))

(defun niwa-plan-write-current
    (plan-markdown session buffer &optional relative-path)
  "Write PLAN-MARKDOWN to SESSION's current plan artifact for BUFFER.
RELATIVE-PATH overrides the default path below SESSION's save directory.
Return an explicit artifact plist containing `:path' and `:hash'."
  (let ((artifact (niwa-plan--publish-current
                   plan-markdown session buffer relative-path)))
    (niwa-plan--reset-current-metadata
     session (plist-get artifact :path) (plist-get artifact :hash))
    artifact))

(defun niwa-plan--reset-current-metadata (session relative-path hash)
  "Replace SESSION's plan metadata for a freshly presented RELATIVE-PATH.
HASH identifies the published artifact.  Only the selection survives: a
presented plan is not an accepted or proposed one."
  (let* ((turn (or (niwa-session-turn-count session) 0))
           (previous (niwa-session-plan-metadata session))
           (metadata
            (list :path relative-path
                  :hash hash
                  :status 'presented
                  :updated-turn turn
                  :updated-at (format-time-string "%FT%H-%M-%S")
                  :verification-pending nil
                  :accepted-turn nil
                  :accepted-at nil)))
      (when (plist-member previous :selection)
        (setq metadata
              (plist-put metadata :selection
                         (copy-tree (plist-get previous :selection)))))
      (setf (niwa-session-plan-metadata session) metadata)))

(defun niwa-plan-archive-accepted
    (current-artifact session &optional relative-path source-session)
  "Archive CURRENT-ARTIFACT as an accepted plan for SESSION.
CURRENT-ARTIFACT is the plist returned by `niwa-plan-write-current'.
RELATIVE-PATH names a deterministic immutable destination when non-nil.
SOURCE-SESSION owns CURRENT-ARTIFACT and defaults to SESSION.
Return a plist containing `:path' and `:hash'."
  (let* ((source-session (or source-session session))
         (plan-relative (plist-get current-artifact :path))
         (plan-hash (plist-get current-artifact :hash))
         (body (niwa-plan-read-artifact source-session current-artifact))
         (dir (or (file-name-directory plan-relative) ""))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (archive-relative
          (if relative-path
              relative-path
            (file-name-concat dir (format "accepted-%s.md" timestamp))))
         (index 1))
      (unless relative-path
        (while (niwa-session-artifacts-artifact-present-p
                session archive-relative)
          (setq archive-relative
                (file-name-concat dir (format "accepted-%s-%d.md"
                                              timestamp index)))
          (setq index (1+ index))))
      (if (niwa-session-artifacts-artifact-present-p
           session archive-relative)
          (unless (and relative-path
                       (equal body
                              (niwa-plan-read-artifact
                               session
                               (list :path archive-relative
                                     :hash plan-hash))))
            (error "Accepted plan artifact already exists with different content"))
        (niwa-session-artifacts-publish-text
         session
         (niwa-plan-artifact-path
          session (list :path archive-relative))
         body 'utf-8-unix))
      (list :path archive-relative :hash plan-hash)))

(defun niwa-plan-current-body (&optional session)
  "Return SESSION's current plan artifact contents, or nil."
  (when-let* ((session (or session niwa--session)))
    (let* ((metadata (niwa-session-plan-metadata session))
           (relative (or (plist-get metadata :path)
                         niwa-plan--relative-current-path)))
      (when (niwa-session-artifacts-artifact-present-p session relative)
        (niwa--normalize-message-text
         (niwa-plan--read-path session relative))))))

(defun niwa-plan-mark-accepted
    (session current-artifact accepted-artifact &optional skip-verification)
  "Mark SESSION's CURRENT-ARTIFACT as accepted.
ACCEPTED-ARTIFACT identifies the immutable archived plan.  When
SKIP-VERIFICATION is non-nil, do not leave verification pending."
  (let* ((previous (niwa-session-plan-metadata session))
         (metadata
          (list :path (plist-get current-artifact :path)
                :hash (plist-get current-artifact :hash)
                :status 'accepted
                :updated-turn (plist-get previous :updated-turn)
                :updated-at (plist-get previous :updated-at)
                :accepted-turn (or (niwa-session-turn-count session) 0)
                :accepted-at (format-time-string "%FT%H-%M-%S")
                :accepted-path (plist-get accepted-artifact :path)
                :accepted-hash (plist-get accepted-artifact :hash))))
    (dolist (key '(:selection :proposal-id))
      (when (plist-member previous key)
        (setq metadata
              (plist-put metadata key (copy-tree (plist-get previous key))))))
    (unless skip-verification
      (setq metadata (plist-put metadata :verification-pending t)))
    (setf (niwa-session-plan-metadata session) metadata)
    metadata))

(defun niwa-plan-accept
    (plan-markdown session buffer &optional skip-verification
                   current-relative-path accepted-relative-path)
  "Persist and accept PLAN-MARKDOWN for SESSION and BUFFER.
CURRENT-RELATIVE-PATH and ACCEPTED-RELATIVE-PATH override artifact locations.
Return `(:current ARTIFACT :accepted ARTIFACT)' for later dispatch."
  ;; Publish and archive before any metadata moves.  The reset drops the
  ;; proposal id, and `niwa-plan-mark-accepted' carries it forward from
  ;; the metadata it finds, so resetting first and then failing to archive
  ;; left a proposal that resume cannot restore -- for an acceptance that
  ;; never committed.
  (let* ((current (niwa-plan--publish-current
                   plan-markdown session buffer current-relative-path))
         (accepted (niwa-plan-archive-accepted
                    current session accepted-relative-path)))
    (niwa-plan--reset-current-metadata
     session (plist-get current :path) (plist-get current :hash))
    (niwa-plan-mark-accepted
     session current accepted skip-verification)
    (list :current current :accepted accepted)))

(provide 'niwa-plan)
;;; niwa-plan.el ends here
