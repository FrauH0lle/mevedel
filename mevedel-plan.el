;;; mevedel-plan.el -- Plan data and artifacts -*- lexical-binding: t -*-

;;; Commentary:

;; Lifecycle-neutral plan parsing, artifact persistence, acceptance metadata,
;; and implementation inputs.  Goal workflows choose storage paths without
;; duplicating these operations.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-artifact-present-p
                  "mevedel-session-persistence" (session logical))
(declare-function mevedel-session-persistence-ensure-files
		  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-publish-text
                  "mevedel-session-persistence"
                  (session path content &optional coding))
(declare-function mevedel-session-persistence-read-artifact
                  "mevedel-session-persistence"
                  (session logical &optional committed-only))

;; `mevedel-structs'
(declare-function mevedel-session-plan-metadata "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x)
		  t)
(defvar mevedel--session)

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text "mevedel-utilities"
		  (text))

(defconst mevedel-plan--open-tag "<proposed_plan>"
  "Opening tag for proposed plans.")

(defconst mevedel-plan--close-tag "</proposed_plan>"
  "Closing tag for proposed plans.")

(defconst mevedel-plan--relative-current-path
  (file-name-concat "local" "plans" "current.md")
  "Relative path of the mutable current plan under a session directory.")

;;
;;; Plan data
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

(defun mevedel-plan-resource-address (relative-path)
  "Return the canonical local resource address for RELATIVE-PATH.
RELATIVE-PATH must name the current plan or an immutable accepted archive
below `local/plans/'."
  (unless (and (stringp relative-path)
               (not (file-name-absolute-p relative-path)))
    (error "Plan artifact path is not relative"))
  (let ((tail
         (cond
          ((equal relative-path mevedel-plan--relative-current-path)
           "current.md")
          ((string-match
            "\\`local/plans/\\(accepted-[^/]+\\.md\\)\\'"
            relative-path)
           (match-string 1 relative-path))
          (t (error "Plan artifact path is outside managed plan storage")))))
    (concat "local://plans/" tail)))

(defun mevedel-plan-artifact-path-p (path)
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
    (let ((relative (or relative-path mevedel-plan--relative-current-path)))
      (unless (mevedel-plan-artifact-path-p relative)
        (error "Invalid plan artifact path: %S" relative))
      (file-name-concat save-path relative))))

(defun mevedel-plan-artifact-path (session artifact)
  "Return ARTIFACT's qualified logical path in SESSION.

ARTIFACT must carry one normalized session-relative `:path'.  The returned
path names the logical artifact, never an immutable publication file."
  (require 'mevedel-structs)
  (let* ((save-path (mevedel-session-save-path session))
         (relative (plist-get artifact :path)))
    (unless (and save-path (mevedel-plan-artifact-path-p relative))
      (error "Invalid plan artifact path: %S" relative))
    (file-name-concat save-path relative)))

(defun mevedel-plan--read-path (session relative)
  "Return SESSION artifact RELATIVE decoded as UTF-8 text."
  (require 'mevedel-session-persistence)
  (decode-coding-string
   (mevedel-session-persistence-read-artifact session relative)
   'utf-8-unix))

(defun mevedel-plan-read-artifact (session artifact)
  "Return SESSION's verified accepted-plan ARTIFACT body."
  (require 'mevedel-utilities)
  (let ((relative (plist-get artifact :path))
        (hash (plist-get artifact :hash)))
    (unless (and (stringp relative) (stringp hash))
      (error "Accepted plan artifact is unavailable"))
    ;; Resolve the logical display path as a structural validation before the
    ;; publication resolver accepts the artifact name.
    (mevedel-plan-artifact-path session artifact)
    (let ((body
           (mevedel--normalize-message-text
            (mevedel-plan--read-path session relative))))
      (unless (equal hash (mevedel-plan-hash body))
        (error "Accepted plan artifact hash does not match"))
      body)))

(defun mevedel-plan--metadata-path (session)
  "Return SESSION's canonical current plan path, when available."
  (require 'mevedel-structs)
  (let ((metadata (mevedel-session-plan-metadata session)))
    (when-let* ((path (or (plist-get metadata :path)
                          mevedel-plan--relative-current-path)))
      (mevedel-plan-artifact-path session (list :path path)))))

(defun mevedel-plan-write-current
    (plan-markdown session buffer &optional relative-path)
  "Write PLAN-MARKDOWN to SESSION's current plan artifact for BUFFER.
RELATIVE-PATH overrides the default path below SESSION's save directory.
Return an explicit artifact plist containing `:path' and `:hash'."
  (require 'mevedel-structs)
  (let* ((relative-path (or relative-path
                            mevedel-plan--relative-current-path))
         (path (mevedel-plan-current-path session buffer relative-path))
         (plan-markdown (mevedel-plan-validate plan-markdown))
         (hash (mevedel-plan-hash plan-markdown)))
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence-publish-text
     session path plan-markdown 'utf-8-unix)
    (let* ((turn (or (mevedel-session-turn-count session) 0))
           (previous (mevedel-session-plan-metadata session))
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
      (setf (mevedel-session-plan-metadata session) metadata))
    (list :path relative-path :hash hash)))

(defun mevedel-plan-archive-accepted
    (current-artifact session &optional relative-path source-session)
  "Archive CURRENT-ARTIFACT as an accepted plan for SESSION.
CURRENT-ARTIFACT is the plist returned by `mevedel-plan-write-current'.
RELATIVE-PATH names a deterministic immutable destination when non-nil.
SOURCE-SESSION owns CURRENT-ARTIFACT and defaults to SESSION.
Return a plist containing `:path' and `:hash'."
  (require 'mevedel-structs)
  (let* ((source-session (or source-session session))
         (plan-relative (plist-get current-artifact :path))
         (plan-hash (plist-get current-artifact :hash))
         (body (mevedel-plan-read-artifact source-session current-artifact))
         (dir (or (file-name-directory plan-relative) ""))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (archive-relative
          (if relative-path
              relative-path
            (file-name-concat dir (format "accepted-%s.md" timestamp))))
         (index 1))
      (unless relative-path
        (while (mevedel-session-persistence-artifact-present-p
                session archive-relative)
          (setq archive-relative
                (file-name-concat dir (format "accepted-%s-%d.md"
                                              timestamp index)))
          (setq index (1+ index))))
      (if (mevedel-session-persistence-artifact-present-p
           session archive-relative)
          (unless (and relative-path
                       (equal body
                              (mevedel-plan-read-artifact
                               session
                               (list :path archive-relative
                                     :hash plan-hash))))
            (error "Accepted plan artifact already exists with different content"))
        (require 'mevedel-session-persistence)
        (mevedel-session-persistence-publish-text
         session
         (mevedel-plan-artifact-path
          session (list :path archive-relative))
         body 'utf-8-unix))
      (list :path archive-relative :hash plan-hash)))

(defun mevedel-plan-current-body (&optional session)
  "Return SESSION's current plan artifact contents, or nil."
  (require 'mevedel-utilities)
  (when-let* ((session (or session mevedel--session)))
    (let* ((metadata (mevedel-session-plan-metadata session))
           (relative (or (plist-get metadata :path)
                         mevedel-plan--relative-current-path)))
      (when (mevedel-session-persistence-artifact-present-p session relative)
        (mevedel--normalize-message-text
         (mevedel-plan--read-path session relative))))))

(defun mevedel-plan-current-exists-p (&optional session)
  "Return non-nil when SESSION has a current plan artifact on disk."
  (when-let* ((session (or session mevedel--session)))
    (let* ((metadata (mevedel-session-plan-metadata session))
           (relative (or (plist-get metadata :path)
                         mevedel-plan--relative-current-path)))
      (require 'mevedel-session-persistence)
      (mevedel-session-persistence-artifact-present-p session relative))))

(defun mevedel-plan-mark-accepted
    (session current-artifact accepted-artifact &optional skip-verification)
  "Mark SESSION's CURRENT-ARTIFACT as accepted.
ACCEPTED-ARTIFACT identifies the immutable archived plan.  When
SKIP-VERIFICATION is non-nil, do not leave verification pending."
  (require 'mevedel-structs)
  (let* ((previous (mevedel-session-plan-metadata session))
         (metadata
          (list :path (plist-get current-artifact :path)
                :hash (plist-get current-artifact :hash)
                :status 'accepted
                :updated-turn (plist-get previous :updated-turn)
                :updated-at (plist-get previous :updated-at)
                :accepted-turn (or (mevedel-session-turn-count session) 0)
                :accepted-at (format-time-string "%FT%H-%M-%S")
                :accepted-path (plist-get accepted-artifact :path)
                :accepted-hash (plist-get accepted-artifact :hash))))
    (dolist (key '(:selection :proposal-id))
      (when (plist-member previous key)
        (setq metadata
              (plist-put metadata key (copy-tree (plist-get previous key))))))
    (unless skip-verification
      (setq metadata (plist-put metadata :verification-pending t)))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))

(defun mevedel-plan-accept
    (plan-markdown session buffer &optional skip-verification)
  "Persist and accept PLAN-MARKDOWN for SESSION and BUFFER.
Return `(:current ARTIFACT :accepted ARTIFACT)' for later dispatch."
  (let* ((current (mevedel-plan-write-current
                   plan-markdown session buffer))
         (accepted (mevedel-plan-archive-accepted current session)))
    (mevedel-plan-mark-accepted
     session current accepted skip-verification)
    (list :current current :accepted accepted)))

(provide 'mevedel-plan)
;;; mevedel-plan.el ends here
