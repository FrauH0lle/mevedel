;;; niwa-journal-claim.el -- Fenced journal work claims -*- lexical-binding: t -*-

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
(require 'niwa-journal-store)
(require 'niwa-session-control-fs)

(defconst niwa-journal-claim--name-regexp "\\`[0-9]\\{20\\}\\.claim\\'"
  "Exact immutable claim filename spelling.")

(defun niwa-journal-claim--path (token outcome-p)
  "Return TOKEN's claim path, or its outcome path when OUTCOME-P."
  (file-name-concat (plist-get token :directory)
                    (format "%020d.%s" (plist-get token :generation)
                            (if outcome-p "outcome" "claim"))))

(defun niwa-journal-claim--record (token)
  "Return the persisted identity fields from TOKEN."
  (list :generation (plist-get token :generation)
        :owner (plist-get token :owner)
        :expires-at (plist-get token :expires-at)))

(defun niwa-journal-claim--decode (text outcome-p)
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
                 (niwa-journal-store-id-p owner)
                 (integerp expires) (> expires 0)
                 (or (not outcome-p) (and status (stringp payload))))
      (error "Invalid journal claim record"))
    (append (list :generation generation :owner owner :expires-at expires)
            (when outcome-p (list :status status :payload payload)))))

(defun niwa-journal-claim--read (path outcome-p &optional observation)
  "Read the claim at PATH, or an outcome when OUTCOME-P.
Return nil for an absent record. Malformed records fail closed; they never
permit takeover or silently reopen a settled generation.
OBSERVATION may be PATH's native read result from this operation's batch;
it must never be retained between operations."
  (when (and observation (not (equal path (plist-get observation :path))))
    (error "Journal claim observation addresses another record"))
  (condition-case nil
      (niwa-journal-claim--decode
       (if observation (niwa-session-control-fs-program-value observation)
         (niwa-session-control-fs-read-file path)) outcome-p)
    (niwa-session-control-fs-absent nil)))

(defun niwa-journal-claim-owned-p (token &rest tokens)
  "Observe whether TOKEN and additional TOKENS are live, unsettled owners.
Every call takes a fresh pinned target observation. This is a precondition,
not atomic admission; settlement still uses its exclusive outcome election.
All tokens must address the same execution target.  Newest-generation proof
stays on the target instead of transferring and sorting its full history."
  (let* ((tokens (cons token tokens))
         (results
          (niwa-session-control-fs-run-program
           (cl-mapcan
            (lambda (owner)
              (let ((directory (niwa-session-control-fs-physical-path
                                (plist-get owner :directory))))
                (list (list :op 'verify-latest :path (niwa-journal-claim--path owner nil)
                            :content ".claim")
                      (list :op 'read :path (niwa-journal-claim--path owner nil))
                      (list :op 'absent :path (niwa-journal-claim--path owner t))
                      (list :op 'target-time :path directory))))
            tokens))))
    (and (cl-every (lambda (result) (eq (plist-get result :status) 'ok)) results)
         (cl-loop for owner in tokens
                  for observed on results by #'cddddr
                  always
                  (and
                   (equal owner
                          (append (list :directory (niwa-session-control-fs-physical-path
                                                    (plist-get owner :directory)))
                                  (niwa-journal-claim--decode
                                   (plist-get (nth 1 observed) :value) nil)))
                   (< (plist-get (nth 3 observed) :value) (plist-get owner :expires-at)))))))

(defun niwa-journal-claim--newest (directory names &optional outcome-p)
  "Return `(TOKEN . OUTCOME)' for the newest claim among DIRECTORY's NAMES.
TOKEN is nil when NAMES hold no claim.  With OUTCOME-P, TOKEN's outcome is
read in the same target program; OUTCOME is nil when it has not settled."
  (when-let* ((name (car (sort (seq-filter
                                (lambda (name)
                                  (string-match-p niwa-journal-claim--name-regexp
                                                  name))
                                names)
                               #'string>))))
    (let* ((path (file-name-concat directory name))
           (results
            (niwa-session-control-fs-run-program
             (append
              (list (list :op 'read :path path))
              (when outcome-p
                (list (list :op 'read :optional t
                            :path (concat (file-name-sans-extension path)
                                          ".outcome")))))))
           (record (niwa-journal-claim--read path nil (car results)))
           (token (and record (append (list :directory directory) record))))
      (when (and record
                 (/= (plist-get record :generation) (string-to-number name)))
        (error "Journal claim generation does not match its filename"))
      (cons token
            (and token outcome-p
                 (niwa-journal-claim-outcome token (nth 1 results)))))))

(defun niwa-journal-claim-current-settlement (directory)
  "Return `(TOKEN . OUTCOME)' for DIRECTORY's newest claim, or nil.
TOKEN is as for `niwa-journal-claim-current'; OUTCOME is its durable
outcome, as for `niwa-journal-claim-outcome', read in the same target
program as the claim."
  (setq directory (niwa-session-control-fs-physical-path directory))
  (niwa-journal-claim--newest
   directory
   (mapcar #'file-name-nondirectory
           (niwa-session-control-fs-list-directory
            directory niwa-journal-claim--name-regexp))
   t))

(defun niwa-journal-claim-current (directory)
  "Return DIRECTORY's newest claim token, or nil when no claim exists.
The token names an attempt, not proof that it is still allowed to settle."
  (setq directory (niwa-session-control-fs-physical-path directory))
  (car (niwa-journal-claim--newest
        directory
        (mapcar #'file-name-nondirectory
                (niwa-session-control-fs-list-directory
                 directory niwa-journal-claim--name-regexp)))))

(defun niwa-journal-claim-outcome (token &optional observation)
  "Return TOKEN's durable outcome or nil when it has not settled.
The returned payload belongs to the winning settlement, including after
process death or a later generation's admission.
OBSERVATION is a native read result from this operation's batch, as for
`niwa-journal-claim--read'."
  (when-let* ((outcome (niwa-journal-claim--read
                       (niwa-journal-claim--path token t) t observation)))
    (unless (equal (niwa-journal-claim--record token)
                   (niwa-journal-claim--record outcome))
      (error "Journal outcome does not belong to this claim"))
    outcome))

(defun niwa-journal-claim--finish (token status payload &optional owner)
  "Exclusively settle TOKEN with STATUS and PAYLOAD.
OWNER, when supplied, must still hold the exact claim-directory flock.
The exclusive create is the election between the owner and a takeover.
Return the accepted outcome, or nil when another outcome already won."
  (when (and owner (not (and (process-live-p owner)
                             (equal (process-get owner 'lock) (plist-get token :directory)))))
    (error "Journal settlement lost its original lock owner"))
  (let ((record (append (niwa-journal-claim--record token)
                        (list :status (symbol-name status) :payload payload))))
    (let ((results
           (niwa-session-control-fs-run-program
            (append
             (list (list :op 'verify :path (niwa-journal-claim--path token nil)
                         :content (json-serialize (niwa-journal-claim--record token))))
             (unless (eq status 'expired)
               (list (list :op 'before-time :path (plist-get token :directory)
                           :content (number-to-string (plist-get token :expires-at)))))
             (list (list :op 'create :path (niwa-journal-claim--path token t)
                         :content (json-serialize record))))
            (or owner (plist-get token :directory)))))
      (unless (seq-some (lambda (result) (memq (plist-get result :status) '(conflict mismatch absent))) results)
        (mapc #'niwa-session-control-fs-program-value results)
        (plist-put record :status status)))))

(defun niwa-journal-claim-acquire (directory seconds &optional deadline)
  "Claim one bounded journal operation in DIRECTORY for SECONDS.
Return an opaque token or nil when another claimant wins or remains live.
Use a private directory per work scope.  All deadlines use the target clock.
Expired claims are fenced durably before a successor can be admitted.
Optional DEADLINE is an exact target-clock expiry shared with an enclosing
admission claim; it must be future and no later than SECONDS from now."
  (unless (and (integerp seconds) (> seconds 0))
    (error "Journal claim duration must be a positive integer"))
  (setq directory (niwa-session-control-fs-physical-path directory))
  (let* ((observe
          (lambda ()
            ;; Ensuring the directory, listing it and reading the clock are
            ;; one target program; only a missing parent costs another.
            (niwa-session-control-fs-run-program
             (list (list :op 'make-directory :path directory :optional t)
                   (list :op 'list-directory :path directory)
                   (list :op 'target-time :path directory)))))
         (observed (funcall observe))
         (observed (if (eq 'absent (plist-get (car observed) :status))
                       (progn (niwa-session-control-fs-make-directory directory t)
                              (funcall observe))
                     observed))
         (names (niwa-session-control-fs-program-value (nth 1 observed)))
         (now (niwa-session-control-fs-program-value (nth 2 observed)))
         (newest (niwa-journal-claim--newest directory names t))
         (previous (car newest))
         (outcome (cdr newest)))
    (when (and deadline
               (not (and (integerp deadline) (< now deadline) (<= deadline (+ now seconds)))))
      (error "Invalid journal claim deadline"))
    (when (and previous (not outcome)
               (>= now (plist-get previous :expires-at)))
      (niwa-journal-claim--finish previous 'expired "")
      ;; Completion may have won instead.  Either outcome fences that
      ;; attempt, but only the recorded payload may ever be published.
      (setq outcome (niwa-journal-claim-outcome previous)))
    (when (or (not previous) outcome)
      (let ((token (list :directory directory
                         :generation (1+ (or (plist-get previous :generation) 0))
                         :owner (secure-hash
                                 'sha256
                                 (format "%S" (list (current-time) (emacs-pid)
                                                    (system-name) (random))))
                         :expires-at (or deadline (+ now seconds)))))
        (let ((results
               (niwa-session-control-fs-run-program
                (append
                 (list (list :op 'verify-latest
                             :path (file-name-concat directory
                                                    (format "%020d.claim" (or (plist-get previous :generation) 0)))
                             :content ".claim"))
                 (when previous
                   (list (list :op 'verify :path (niwa-journal-claim--path previous nil)
                               :content (json-serialize (niwa-journal-claim--record previous)))
                         (list :op 'verify :path (niwa-journal-claim--path previous t)
                               :content (json-serialize
                                         (plist-put (copy-sequence outcome) :status
                                                    (symbol-name (plist-get outcome :status)))))))
                 (list (list :op 'create :path (niwa-journal-claim--path token nil)
                             :content (json-serialize (niwa-journal-claim--record token)))))
                directory)))
          (unless (seq-some (lambda (result) (memq (plist-get result :status) '(conflict mismatch absent))) results)
            (mapc #'niwa-session-control-fs-program-value results)
            token))))))

(defun niwa-journal-claim-settle (token status payload &optional owner)
  "Settle TOKEN with STATUS and a frozen string PAYLOAD.
If supplied, OWNER elects the outcome under its continuously held flock.
STATUS is `completed', `failed', or `cancelled'.  Return the accepted
outcome, or nil for an expired, foreign, or already-settled token.  Only a
successful completed outcome authorizes publication of its exact payload."
  (unless (and (memq status '(completed failed cancelled)) (stringp payload))
    (error "Invalid journal claim settlement"))
  ;; The election's own `verify' of the claim bytes is the ownership proof.
  (niwa-journal-claim--finish token status payload owner))

(iter-defun niwa-journal-claim-prune (directory protected limit)
  "Yield while deleting at most LIMIT settled claim pairs in DIRECTORY.
PROTECTED lists generations still referenced by durable work.  Keep the newest
claim as the numbering anchor and all attempts whose deadlines have not passed.
Checks and deletion share the claim admission/settlement target lock.
Return the deletion count when the iterator completes."
  (let* ((names (niwa-session-control-fs-list-directory
                 directory "\\`[0-9]\\{20\\}\\.\\(?:claim\\|outcome\\)\\'"))
         (anchor (car (sort (seq-filter
                             (lambda (path) (string-suffix-p ".claim" path))
                            names)
                            #'string>)))
         (head (if anchor (string-to-number (file-name-base anchor)) 0))
         ;; A listing can rule out work, never authorize deletion.  Avoid
         ;; reads, clock probes and ownership-checked yields for retained-only
         ;; directories; candidates still need the ordinary fresh proof.
         (generations
          (sort (cl-remove-if
                 (lambda (generation)
                   (or (>= generation head) (memq generation protected)))
                 (delete-dups
                  (mapcar (lambda (path) (string-to-number (file-name-base path)))
                          names)))
                #'<))
         (current (and generations (niwa-journal-claim-current directory)))
         (head (or (plist-get current :generation) 0))
         (now (and current (niwa-session-control-fs-target-time directory)))
         (deleted 0))
    (while (and generations (< deleted limit))
      (let ((generation (pop generations)))
        (iter-yield nil)
        (when (and (< generation head) (not (memq generation protected)))
          (let* ((token (list :directory directory :generation generation))
                 (claim-path (niwa-journal-claim--path token nil))
                 (outcome-path (niwa-journal-claim--path token t))
                 ;; One program reads both; it is no atomic snapshot, and the
                 ;; verify-and-delete program below re-proves both records.
                 (observed (niwa-session-control-fs-run-program
                            (list (list :op 'read :optional t :path claim-path)
                                  (list :op 'read :optional t :path outcome-path))))
                 (claim (niwa-journal-claim--read claim-path nil (car observed)))
                 (outcome (niwa-journal-claim--read outcome-path t (cadr observed))))
            (when (and outcome (= generation (plist-get outcome :generation))
                       (<= (plist-get outcome :expires-at) now)
                       (or (null claim) (equal claim (niwa-journal-claim--record outcome))))
              (let ((results
                     (niwa-session-control-fs-run-program
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
                  (mapc #'niwa-session-control-fs-program-value results)
                  (cl-incf deleted))))))))
    deleted))

(provide 'niwa-journal-claim)
;;; niwa-journal-claim.el ends here
