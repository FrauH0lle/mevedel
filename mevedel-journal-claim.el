;;; mevedel-journal-claim.el -- Fenced journal work claims -*- lexical-binding: t -*-

;;; Commentary:

;; Each work scope has exclusive, numbered claim records and immutable
;; outcomes.  Completion and expired-claim takeover compete to create the
;; same outcome file.  The winner fences the other before a later generation
;; may start.  An accepted outcome retains the exact result for recovery;
;; publishing that result is a separate, idempotent operation.
;;
;; This uses the session authority's target clock and exclusive-generation
;; pattern without manufacturing a session.  Claims have fixed deadlines:
;; bounded journal requests do not renew their authority while running.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'generator)
(require 'mevedel-journal-store)
(require 'mevedel-session-control-fs)

(defconst mevedel-journal-claim--name-regexp "\\`[0-9]\\{20\\}\\.claim\\'"
  "Exact immutable claim filename spelling.")

(defun mevedel-journal-claim--path (token outcome-p)
  "Return TOKEN's claim path, or its outcome path when OUTCOME-P."
  (file-name-concat (plist-get token :directory)
                    (format "%020d.%s" (plist-get token :generation)
                            (if outcome-p "outcome" "claim"))))

(defun mevedel-journal-claim--record (token)
  "Return the persisted identity fields from TOKEN."
  (list :generation (plist-get token :generation)
        :owner (plist-get token :owner)
        :expires-at (plist-get token :expires-at)))

(defun mevedel-journal-claim--decode (text outcome-p)
  "Decode claim TEXT, or an outcome when OUTCOME-P, with the exact schema."
  (let* ((object (json-parse-string text))
         (generation (and (hash-table-p object) (gethash "generation" object)))
         (owner (and (hash-table-p object) (gethash "owner" object)))
         (expires (and (hash-table-p object) (gethash "expires-at" object)))
         (status (and outcome-p (hash-table-p object)
                      (car (memq (intern-soft (gethash "status" object))
                                 '(completed failed cancelled expired)))))
         (payload (and outcome-p (hash-table-p object) (gethash "payload" object))))
    (unless (and (hash-table-p object)
                 (= (hash-table-count object) (if outcome-p 5 3))
                 (integerp generation) (> generation 0)
                 (mevedel-journal-store-id-p owner)
                 (integerp expires) (> expires 0)
                 (or (not outcome-p) (and status (stringp payload))))
      (error "Invalid journal claim record"))
    (append (list :generation generation :owner owner :expires-at expires)
            (when outcome-p (list :status status :payload payload)))))

(defun mevedel-journal-claim--read (path outcome-p &optional observation)
  "Read the claim at PATH, or an outcome when OUTCOME-P.
Return nil for an absent record. Malformed records fail closed; they never
permit takeover or silently reopen a settled generation.
OBSERVATION may be PATH's native read result from this operation's batch;
it must never be retained between operations."
  (when (and observation (not (equal path (plist-get observation :path))))
    (error "Journal claim observation addresses another record"))
  (condition-case nil
      (mevedel-journal-claim--decode
       (if observation (mevedel-session-control-fs-program-value observation)
         (mevedel-session-control-fs-read-file path)) outcome-p)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-journal-claim-owned-p (token &rest tokens)
  "Observe whether TOKEN and additional TOKENS are live, unsettled owners.
Every call takes a fresh pinned target observation. This is a precondition,
not atomic admission; settlement still uses its exclusive outcome election.
All tokens must address the same execution target."
  (let* ((tokens (cons token tokens))
         (results
          (mevedel-session-control-fs-run-program
           (cl-mapcan
            (lambda (owner)
              (let ((directory (mevedel-session-control-fs-physical-path
                                (plist-get owner :directory))))
                (list (list :op 'list-directory :path directory)
                      (list :op 'read :path (mevedel-journal-claim--path owner nil))
                      (list :op 'absent :path (mevedel-journal-claim--path owner t))
                      (list :op 'target-time :path directory))))
            tokens))))
    (and (cl-every (lambda (result) (eq (plist-get result :status) 'ok)) results)
         (cl-loop for owner in tokens
                  for observed on results by #'cddddr
                  always
                  (and
                   (equal (file-name-nondirectory (mevedel-journal-claim--path owner nil))
                          (car (sort (seq-filter
                                      (lambda (name) (string-match-p mevedel-journal-claim--name-regexp name))
                                      (plist-get (car observed) :value)) #'string>)))
                   (equal owner
                          (append (list :directory (mevedel-session-control-fs-physical-path
                                                    (plist-get owner :directory)))
                                  (mevedel-journal-claim--decode
                                   (plist-get (nth 1 observed) :value) nil)))
                   (< (plist-get (nth 3 observed) :value) (plist-get owner :expires-at)))))))

(defun mevedel-journal-claim-current (directory)
  "Return DIRECTORY's newest claim token, or nil when no claim exists.
The token names an attempt, not proof that it is still allowed to settle."
  (setq directory (mevedel-session-control-fs-physical-path directory))
  (when-let* ((path (car (sort (mevedel-session-control-fs-list-directory
                              directory mevedel-journal-claim--name-regexp)
                             #'string>)))
              (record (mevedel-journal-claim--read path nil)))
    (unless (= (plist-get record :generation)
               (string-to-number (file-name-nondirectory path)))
      (error "Journal claim generation does not match its filename"))
    (append (list :directory directory) record)))

(defun mevedel-journal-claim-outcome (token &optional observation)
  "Return TOKEN's durable outcome or nil when it has not settled.
The returned payload belongs to the winning settlement, including after
process death or a later generation's admission.
OBSERVATION is a native read result from this operation's batch, as for
`mevedel-journal-claim--read'."
  (when-let* ((outcome (mevedel-journal-claim--read
                       (mevedel-journal-claim--path token t) t observation)))
    (unless (equal (mevedel-journal-claim--record token)
                   (mevedel-journal-claim--record outcome))
      (error "Journal outcome does not belong to this claim"))
    outcome))

(defun mevedel-journal-claim--finish (token status payload)
  "Exclusively settle TOKEN with STATUS and PAYLOAD.
The exclusive create is the election between the owner and a takeover.
Return the accepted outcome, or nil when another outcome already won."
  (let ((record (append (mevedel-journal-claim--record token)
                        (list :status (symbol-name status) :payload payload))))
    (let ((results
           (mevedel-session-control-fs-run-program
            (append
             (list (list :op 'verify :path (mevedel-journal-claim--path token nil)
                         :content (json-serialize (mevedel-journal-claim--record token))))
             (unless (eq status 'expired)
               (list (list :op 'before-time :path (plist-get token :directory)
                           :content (number-to-string (plist-get token :expires-at)))))
             (list (list :op 'create :path (mevedel-journal-claim--path token t)
                         :content (json-serialize record))))
            (plist-get token :directory))))
      (unless (seq-some (lambda (result) (memq (plist-get result :status) '(conflict mismatch absent))) results)
        (mapc #'mevedel-session-control-fs-program-value results)
        (plist-put record :status status)))))

(defun mevedel-journal-claim-acquire (directory seconds &optional deadline)
  "Claim one bounded journal operation in DIRECTORY for SECONDS.
Return an opaque token or nil when another claimant wins or remains live.
Use a private directory per work scope.  All deadlines use the target clock.
Expired claims are fenced durably before a successor can be admitted.
Optional DEADLINE is an exact target-clock expiry shared with an enclosing
admission claim; it must be future and no later than SECONDS from now."
  (unless (and (integerp seconds) (> seconds 0))
    (error "Journal claim duration must be a positive integer"))
  (setq directory (mevedel-session-control-fs-physical-path directory))
  (mevedel-session-control-fs-make-directory directory t)
  (let* ((previous (mevedel-journal-claim-current directory))
         (now (mevedel-session-control-fs-target-time directory))
         (outcome (and previous (mevedel-journal-claim-outcome previous))))
    (when (and deadline
               (not (and (integerp deadline) (< now deadline) (<= deadline (+ now seconds)))))
      (error "Invalid journal claim deadline"))
    (when (and previous (not outcome)
               (>= now (plist-get previous :expires-at)))
      (mevedel-journal-claim--finish previous 'expired "")
      ;; Completion may have won instead.  Either outcome fences that
      ;; attempt, but only the recorded payload may ever be published.
      (setq outcome (mevedel-journal-claim-outcome previous)))
    (when (or (not previous) outcome)
      (let ((token (list :directory directory
                         :generation (1+ (or (plist-get previous :generation) 0))
                         :owner (secure-hash
                                 'sha256
                                 (format "%S" (list (current-time) (emacs-pid)
                                                    (system-name) (random))))
                         :expires-at (or deadline (+ now seconds)))))
        (let ((results
               (mevedel-session-control-fs-run-program
                (append
                 (list (list :op 'verify-latest
                             :path (file-name-concat directory
                                                    (format "%020d.claim" (or (plist-get previous :generation) 0)))
                             :content ".claim"))
                 (when previous
                   (list (list :op 'verify :path (mevedel-journal-claim--path previous nil)
                               :content (json-serialize (mevedel-journal-claim--record previous)))
                         (list :op 'verify :path (mevedel-journal-claim--path previous t)
                               :content (json-serialize
                                         (plist-put (copy-sequence outcome) :status
                                                    (symbol-name (plist-get outcome :status)))))))
                 (list (list :op 'create :path (mevedel-journal-claim--path token nil)
                             :content (json-serialize (mevedel-journal-claim--record token)))))
                directory)))
          (unless (seq-some (lambda (result) (memq (plist-get result :status) '(conflict mismatch absent))) results)
            (mapc #'mevedel-session-control-fs-program-value results)
            token))))))

(defun mevedel-journal-claim-settle (token status payload)
  "Settle TOKEN with STATUS and a frozen string PAYLOAD.
STATUS is `completed', `failed', or `cancelled'.  Return the accepted
outcome, or nil for an expired, foreign, or already-settled token.  Only a
successful completed outcome authorizes publication of its exact payload."
  (unless (and (memq status '(completed failed cancelled)) (stringp payload))
    (error "Invalid journal claim settlement"))
  (when (equal (mevedel-journal-claim--record token)
               (mevedel-journal-claim--read
                (mevedel-journal-claim--path token nil) nil))
    (mevedel-journal-claim--finish token status payload)))

(iter-defun mevedel-journal-claim-prune (directory protected limit)
  "Yield while deleting at most LIMIT settled claim pairs in DIRECTORY.
PROTECTED lists generations still referenced by durable work.  Keep the newest
claim as the numbering anchor and all attempts whose deadlines have not passed.
Checks and deletion share the claim admission/settlement target lock.
Return the deletion count when the iterator completes."
  (let* ((current (mevedel-journal-claim-current directory))
         (head (or (plist-get current :generation) 0))
         (now (mevedel-session-control-fs-target-time directory))
         (names (mevedel-session-control-fs-list-directory
                 directory "\\`[0-9]\\{20\\}\\.\\(?:claim\\|outcome\\)\\'"))
         (generations (sort (delete-dups (mapcar (lambda (path) (string-to-number (file-name-base path))) names)) #'<))
         (deleted 0))
    (dolist (generation generations)
      (iter-yield nil)
      (when (and (< deleted limit) (< generation head) (not (memq generation protected)))
        (let* ((token (list :directory directory :generation generation))
               (claim-path (mevedel-journal-claim--path token nil))
               (outcome-path (mevedel-journal-claim--path token t))
               (claim (mevedel-journal-claim--read claim-path nil))
               (outcome (mevedel-journal-claim--read outcome-path t)))
          (when (and outcome (= generation (plist-get outcome :generation))
                     (<= (plist-get outcome :expires-at) now)
                     (or (null claim) (equal claim (mevedel-journal-claim--record outcome))))
            (let ((results
                   (mevedel-session-control-fs-run-program
                    (append
                     (list (list :op 'verify :path outcome-path
                                 :content (json-serialize
                                           (plist-put (copy-sequence outcome) :status
                                                      (symbol-name (plist-get outcome :status))))))
                     (if claim
                         (list (list :op 'verify :path claim-path :content (json-serialize claim))
                               (list :op 'delete-file :path claim-path))
                       (list (list :op 'absent :path claim-path)))
                     (list (list :op 'delete-file :path outcome-path)))
                    directory)))
              (unless (seq-some (lambda (result) (memq (plist-get result :status) '(conflict mismatch absent))) results)
                (mapc #'mevedel-session-control-fs-program-value results)
                (cl-incf deleted)))))))
    deleted))

(provide 'mevedel-journal-claim)
;;; mevedel-journal-claim.el ends here
