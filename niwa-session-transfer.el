;;; niwa-session-transfer.el -- Durable cooperative control transfer -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the immutable request, decision, and release-fence protocol for cooperative portable-session control transfer.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst niwa-session-transfer--maximum-timeout 300
  "Maximum accepted cooperative transfer timeout in seconds.")

;; Every control operation in this file runs through the session control
;; filesystem, so its feature is a hard load-time dependency rather than a
;; lazily reachable one.
(require 'niwa-session-control-fs)
;; The durable-transaction macro must expand for interpreted loads too,
;; so this is a load-time dependency rather than a lazy one.
(require 'niwa-session-durability)

;; `niwa-engine'
(declare-function niwa-engine-assert-local-history "niwa-engine"
                  (session operation &optional backend root-only))
(declare-function niwa-engine-external-history-p "niwa-engine"
                  (session &optional root-only))
(autoload 'niwa-engine-assert-local-history "niwa-engine")
(autoload 'niwa-engine-external-history-p "niwa-engine")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-directory-p
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-list-directory
                  "niwa-session-control-fs" (directory regexp))
(declare-function niwa-session-control-fs-make-directory
                  "niwa-session-control-fs" (path &optional parents))
(declare-function niwa-session-control-fs-path-exists-p
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-physical-path
                  "niwa-session-control-fs" (path))

;; `niwa-session-durability'
(declare-function niwa-session-durability--assert-no-pid-lock
                  "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability--finite-nonnegative-number-p
                  "niwa-session-durability" (value))
(declare-function niwa-session-durability--bind-lease
                  "niwa-session-durability" (session lease state &optional bytes predecessor))
(declare-function niwa-session-durability--bind-latest
                  "niwa-session-durability" (session latest directory now))
(declare-function niwa-session-durability--claim-next
                  "niwa-session-durability"
                  (directory expected buffer-name
                             &optional status unsettled-mutation-p
                             unsettled-mutation transfer-generation))
(declare-function niwa-session-durability--create-plist
                  "niwa-session-durability" (path plist))
(declare-function niwa-session-durability--ensure-lease-directory
                  "niwa-session-durability" (directory))
(declare-function niwa-session-durability--lease-head
                  "niwa-session-durability" (directory &optional names))
(declare-function niwa-session-durability--lease-path
                  "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability--newest-generation
                  "niwa-session-durability" (names))
(declare-function niwa-session-durability--observe-lease
                  "niwa-session-durability" (directory generation &optional extra))
(declare-function niwa-session-durability--observed-names
                  "niwa-session-durability" (observed))
(declare-function niwa-session-durability--owned-lease-record-p
                  "niwa-session-durability" (lease directory &optional now))
(declare-function niwa-session-durability--pid-lock-operation
                  "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability--portable-session-p
                  "niwa-session-durability" (session))
(declare-function niwa-session-durability--read-plist
                  "niwa-session-durability" (path))
(declare-function niwa-session-durability--settle-pid-lock
                  "niwa-session-durability" (session-dir result))
(declare-function niwa-session-durability--target-time
                  "niwa-session-durability" (directory))
(declare-function niwa-session-durability--valid-lease-p
                  "niwa-session-durability" (lease))
(declare-function niwa-session-durability-lease-release
                  "niwa-session-durability"
                  (session-dir &optional session))
(defvar niwa-session-durability--client-id)
(defvar niwa-session-durability--observed-time)

;; `niwa-session-recovery'
(declare-function niwa-session-recovery--root
                  "niwa-session-recovery" (session-dir))
(declare-function niwa-session-recovery-refresh
                  "niwa-session-recovery" (session &optional root-observation))
(autoload 'niwa-session-recovery--root "niwa-session-recovery")
(autoload 'niwa-session-recovery-refresh "niwa-session-recovery")

;; `niwa-structs'
(declare-function niwa-session-control-transfer
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-external-conversations "niwa-structs" (cl-x))
(declare-function niwa-session-lease "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-set-control-transfer
                  "niwa-structs" (session transfer))


(defcustom niwa-session-transfer-prompt-timeout 30
  "Seconds before an unanswered control-transfer request is granted.

The same short window is used for the release fence left for the named
requester after the owner has released its lease."
  :type 'integer
  :group 'niwa)

(defcustom niwa-session-transfer-client-label "Emacs"
  "User-visible label attached to cooperative lease-transfer requests."
  :type 'string
  :group 'niwa)

(defun niwa-session-transfer--path (directory name)
  "Return transfer subdirectory NAME below lease DIRECTORY without creating it.

Reads use this: an absent mailbox holds no requests, and a poll that created
target directories would mutate the session on behalf of an observer."
  (niwa-session-control-fs-physical-path directory)
  (niwa-session-control-fs-physical-path
   (file-name-concat directory name)))

(defun niwa-session-transfer--directory (directory name)
  "Ensure and return transfer subdirectory NAME below lease DIRECTORY."
  (let ((path (niwa-session-transfer--path directory name)))
    (cond
     ((niwa-session-control-fs-directory-p path) path)
     ((niwa-session-control-fs-path-exists-p path)
      (error "Invalid portable control-transfer directory: %s" path))
     (t
      (condition-case nil
          (niwa-session-control-fs-make-directory path)
        (file-already-exists nil))
      (setq path (niwa-session-control-fs-physical-path path))
      (unless (niwa-session-control-fs-directory-p path)
        (error "Could not create portable control-transfer directory: %s"
               path))
      path))))

(defun niwa-session-transfer--id-p (value length)
  "Return non-nil when VALUE is a lowercase hexadecimal ID of LENGTH."
  (and (stringp value)
       (= (length value) length)
       (string-match-p (format "\\`[0-9a-f]\\{%d\\}\\'" length) value)))

(defun niwa-session-transfer--record-keys-p (record keys)
  "Return non-nil when RECORD is a proper plist with exactly KEYS."
  (and (proper-list-p record)
       (= 0 (% (length record) 2))
       (equal (sort (copy-sequence
                     (let (result)
                       (while record
                         (push (pop record) result)
                         (pop record))
                       result))
                    (lambda (left right)
                      (string< (symbol-name left) (symbol-name right))))
              (sort (copy-sequence keys)
                    (lambda (left right)
                      (string< (symbol-name left) (symbol-name right)))))))

(defun niwa-session-transfer--fence-path (directory generation)
  "Return the immutable release-fence path for GENERATION in DIRECTORY."
  (let ((path
         (file-name-concat
          (niwa-session-transfer--path directory "fences")
          (format "fence-%020d.el" generation))))
    (niwa-session-control-fs-physical-path path)))

(defun niwa-session-transfer--valid-fence-p (record &optional now)
  "Return non-nil when RECORD is a trusted immutable release fence."
  (and (niwa-session-transfer--record-keys-p
        record '(:protocol-version :generation :request-id :session-id
                 :owner-client-id :requester-client-id :expires-at))
       (= 1 (plist-get record :protocol-version))
       (natnump (plist-get record :generation))
       (> (plist-get record :generation) 0)
       (niwa-session-transfer--id-p
        (plist-get record :request-id) 32)
       (stringp (plist-get record :session-id))
       (> (length (plist-get record :session-id)) 0)
       (niwa-session-transfer--id-p
        (plist-get record :owner-client-id) 64)
       (niwa-session-transfer--id-p
        (plist-get record :requester-client-id) 64)
       (niwa-session-durability--finite-nonnegative-number-p
        (plist-get record :expires-at))
       (or (null now)
           (<= (plist-get record :expires-at)
               (+ now niwa-session-transfer--maximum-timeout)))))

(defun niwa-session-transfer-release-fence (directory generation &optional observation)
  "Return the validated live release fence for GENERATION, or nil.
Use OBSERVATION, a pinned read result, when supplied by session discovery."
  (let* ((path (niwa-session-transfer--fence-path directory generation))
         (now (niwa-session-durability--target-time directory)))
    (when (if observation (not (eq 'absent (plist-get observation :status)))
            (niwa-session-control-fs-path-exists-p path))
      (let ((fence (if observation
                       (car (read-from-string
                             (niwa-session-control-fs-program-value observation)))
                     (niwa-session-durability--read-plist path))))
        (unless (niwa-session-transfer--valid-fence-p fence now)
          (error "Invalid portable control-transfer fence: %s" path))
        (when (> (plist-get fence :expires-at) now)
          fence)))))

(defun niwa-session-transfer--valid-timeout-p (value)
  "Return non-nil when VALUE is an accepted transfer timeout."
  (and (niwa-session-durability--finite-nonnegative-number-p value)
       (> value 0)
       (<= value niwa-session-transfer--maximum-timeout)))

(defun niwa-session-transfer--valid-client-label-p (value)
  "Return non-nil when VALUE is safe bounded transfer display text."
  (and (stringp value)
       (> (length value) 0)
       (<= (length value) 128)
       (not (string-match-p "[[:cntrl:]]" value))))

(defun niwa-session-transfer--request-path
    (directory generation)
  "Return the immutable request path for lease GENERATION in DIRECTORY."
  (let ((path
         (file-name-concat
          (niwa-session-transfer--path directory "requests")
          (format "request-%020d.el" generation))))
    (niwa-session-control-fs-physical-path path)))

(defun niwa-session-transfer--decision-path
    (directory generation)
  "Return the immutable decision path for lease GENERATION in DIRECTORY."
  (let ((path
         (file-name-concat
          (niwa-session-transfer--path directory "requests")
          (format "decision-%020d.el" generation))))
    (niwa-session-control-fs-physical-path path)))

(defun niwa-session-transfer--valid-request-p (record &optional now)
  "Return non-nil when RECORD is a trusted current request representation."
  (and (niwa-session-transfer--record-keys-p
        record '(:protocol-version :request-id :session-id :generation
                 :owner-client-id :requester-client-id :requester-label
                 :created-at :deadline))
       (= 1 (plist-get record :protocol-version))
       (niwa-session-transfer--id-p
        (plist-get record :request-id) 32)
       (stringp (plist-get record :session-id))
       (> (length (plist-get record :session-id)) 0)
       (natnump (plist-get record :generation))
       (> (plist-get record :generation) 0)
       (niwa-session-transfer--id-p
        (plist-get record :owner-client-id) 64)
       (niwa-session-transfer--id-p
        (plist-get record :requester-client-id) 64)
       (niwa-session-transfer--valid-client-label-p
        (plist-get record :requester-label))
       (niwa-session-durability--finite-nonnegative-number-p
        (plist-get record :created-at))
       (niwa-session-durability--finite-nonnegative-number-p
        (plist-get record :deadline))
       (>= (plist-get record :deadline) (plist-get record :created-at))
       (<= (- (plist-get record :deadline)
              (plist-get record :created-at))
           niwa-session-transfer--maximum-timeout)
       (or (null now)
           (<= (plist-get record :created-at)
               (+ now niwa-session-transfer--maximum-timeout)))))

(defun niwa-session-transfer--valid-decision-p (record)
  "Return non-nil when RECORD is a trusted immutable decision."
  (and (niwa-session-transfer--record-keys-p
        record '(:protocol-version :request-id :session-id :generation
                 :owner-client-id :requester-client-id :decision :decided-at))
       (= 1 (plist-get record :protocol-version))
       (niwa-session-transfer--id-p
        (plist-get record :request-id) 32)
       (stringp (plist-get record :session-id))
       (> (length (plist-get record :session-id)) 0)
       (natnump (plist-get record :generation))
       (> (plist-get record :generation) 0)
       (niwa-session-transfer--id-p
        (plist-get record :owner-client-id) 64)
       (niwa-session-transfer--id-p
        (plist-get record :requester-client-id) 64)
       (memq (plist-get record :decision) '(grant reject))
       (niwa-session-durability--finite-nonnegative-number-p
        (plist-get record :decided-at))))

(defun niwa-session-transfer--record-identity-matches-p
    (request record)
  "Return non-nil when transfer RECORD carries REQUEST's identity."
  (and (equal (plist-get request :request-id)
              (plist-get record :request-id))
       (equal (plist-get request :session-id)
              (plist-get record :session-id))
       (equal (plist-get request :owner-client-id)
              (plist-get record :owner-client-id))
       (equal (plist-get request :requester-client-id)
              (plist-get record :requester-client-id))))

(defun niwa-session-transfer--record-matches-p
    (request record)
  "Return non-nil when immutable transfer RECORD matches REQUEST."
  (and (niwa-session-transfer--record-identity-matches-p request record)
       (equal (plist-get request :generation)
              (plist-get record :generation))))

(defun niwa-session-transfer--request-id ()
  "Return a fresh opaque control-transfer request ID."
  (substring
   (secure-hash 'sha256
                (format "%S"
                        (list (current-time) (random most-positive-fixnum)
                              (emacs-pid) niwa-session-durability--client-id)))
   0 32))

(defun niwa-session-transfer--requests-operation (directory)
  "Return the optional program operation listing DIRECTORY's requests."
  (list :op 'list-directory
        :path (niwa-session-transfer--path directory "requests")
        :optional t))

(defun niwa-session-transfer--current-request
    (directory lease session-id &optional now listing)
  "Read DIRECTORY's newest live request for SESSION-ID, or nil.

LISTING, when non-nil, is the result of
`niwa-session-transfer--requests-operation' run by the caller's own
program, and replaces the listing round trip.

The owner's lease generation legitimately advances while a request is
pending: settling the durable mutation latch and publication both rotate
the continuously owned lease.  The newest request at or below LEASE's
generation is therefore the live one; it must still name LEASE's client
as its owner, so a request left over from an earlier ownership cannot
cross a release."
  (let* ((generation (plist-get lease :generation))
         (now (or now (niwa-session-durability--target-time directory)))
         (regexp "\\`request-[0-9]\\{20\\}\\.el\\'")
         (path
          (car (sort
                (if listing
                    (let ((requests (niwa-session-transfer--path
                                     directory "requests")))
                      (unless (eq 'absent (plist-get listing :status))
                        (delq nil
                              (mapcar
                               (lambda (name)
                                 (and (string-match-p regexp name)
                                      (expand-file-name name requests)))
                               (niwa-session-control-fs-program-value
                                listing)))))
                  (niwa-session-control-fs-list-directory
                   (niwa-session-transfer--path directory "requests")
                   regexp))
                #'string>))))
    (when path
      (let ((request (niwa-session-durability--read-plist path)))
        (unless (niwa-session-transfer--valid-request-p request now)
          (error "Invalid portable control-transfer request: %s" path))
        (and (<= (plist-get request :generation) generation)
             (equal session-id (plist-get request :session-id))
             (equal (plist-get lease :client-id)
                    (plist-get request :owner-client-id))
             request)))))

(defun niwa-session-transfer-observe-decision (session request)
  "Return REQUEST's validated immutable decision for requester SESSION."
  (unless (and (niwa-session-durability--portable-session-p session)
               (equal (niwa-session-session-id session)
                      (plist-get request :session-id))
               (equal niwa-session-durability--client-id
                      (plist-get request :requester-client-id)))
    (error "Control-transfer request does not belong to this session client"))
  (let* ((directory
          (niwa-session-durability--lease-path
           (niwa-session-save-path session)))
         (path
          (niwa-session-transfer--decision-path
           directory (plist-get request :generation)))
         (decision (niwa-session-durability--read-plist path)))
    (when decision
      (unless (and (niwa-session-transfer--valid-decision-p decision)
                   (niwa-session-transfer--record-matches-p
                    request decision))
        (error "Invalid portable control-transfer decision: %s" path))
      decision)))

(defun niwa-session-transfer--set-state
    (session state request &optional decision observed-at)
  "Set transient SESSION control-transfer STATE for REQUEST and DECISION.

OBSERVED-AT is the target time at which this owner first saw REQUEST.  It is
transient client state, not part of the durable protocol: a restarted owner
simply starts its notice window again."
  (niwa-session-set-control-transfer
   session (list :state state :request request :decision decision
                 :observed-at observed-at)))

(defun niwa-session-transfer--observed-at (session request now)
  "Return when this owner first saw REQUEST for SESSION, defaulting to NOW."
  (let ((previous (niwa-session-control-transfer session)))
    (or (and (equal (plist-get (plist-get previous :request) :request-id)
                    (plist-get request :request-id))
             (plist-get previous :observed-at))
        now)))

(defun niwa-session-transfer-request
    (session &optional label)
  "Atomically request control of portable SESSION.

Return the immutable request record, or nil when another requester won the
current-generation race.  The owner remains the sole mutator until it
explicitly decides and releases its lease."
  (niwa-engine-assert-local-history session "Session control transfer")
  (unless (niwa-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (let ((session-dir (or (niwa-session-save-path session)
                         (error "Session has not been materialized yet")))
        (session-id (or (niwa-session-session-id session)
                        (error "Session has no durable identity"))))
    (niwa-session-durability--assert-no-pid-lock session-dir)
    (unless (and (stringp session-id) (> (length session-id) 0))
      (error "Control-transfer session ID is required"))
    (unless (niwa-session-transfer--valid-timeout-p
             niwa-session-transfer-prompt-timeout)
      (error "Invalid control-transfer timeout: %S"
             niwa-session-transfer-prompt-timeout))
    (setq label (or label niwa-session-transfer-client-label))
    (unless (niwa-session-transfer--valid-client-label-p label)
      (error "Invalid control-transfer client label"))
    (let* ((directory (niwa-session-durability--lease-path session-dir))
           (_ (niwa-session-durability--ensure-lease-directory directory))
           (now (niwa-session-durability--target-time directory))
           (lease (niwa-session-durability--lease-head directory)))
      (unless (and (niwa-session-durability--valid-lease-p lease)
                   (memq (plist-get lease :status) '(active publishing))
                   (> (plist-get lease :expires-at) now))
        (error "Control transfer requires a live portable lease"))
      (when (equal niwa-session-durability--client-id
                   (plist-get lease :client-id))
        (error "The current lease owner cannot request control transfer"))
      (let* ((live-request
              (niwa-session-transfer--current-request
               directory lease session-id now))
             (live-decision
              (and live-request
                   (niwa-session-durability--read-plist
                    (niwa-session-transfer--decision-path
                     directory (plist-get live-request :generation)))))
             (_
              (when (and live-decision
                         (not
                          (and
                           (niwa-session-transfer--valid-decision-p
                            live-decision)
                           (niwa-session-transfer--record-matches-p
                            live-request live-decision))))
                (error "Invalid portable control-transfer decision")))
             (existing
              (and live-request
                   (not (and live-decision
                             (eq 'reject
                                 (plist-get live-decision :decision))))
                   live-request))
             (rejected
              (and live-request live-decision
                   (eq 'reject (plist-get live-decision :decision))))
             (request-generation
              (plist-get lease :transfer-generation))
             (request-path
              (niwa-session-transfer--request-path
               directory request-generation))
             (decision-path
              (niwa-session-transfer--decision-path
               directory request-generation)))
        (if existing
            (and (equal niwa-session-durability--client-id
                        (plist-get existing :requester-client-id))
                 (equal session-id (plist-get existing :session-id))
                 existing)
          (unless (and rejected
                       (/= request-generation
                           (1+ (plist-get live-request :generation))))
            (when (niwa-session-control-fs-path-exists-p decision-path)
              (error "Portable control-transfer decision has no request: %s"
                     decision-path))
            (let* ((request
                    (list :protocol-version 1
                          :request-id
                          (niwa-session-transfer--request-id)
                          :session-id session-id
                          :generation request-generation
                          :owner-client-id (plist-get lease :client-id)
                          :requester-client-id
                          niwa-session-durability--client-id
                          :requester-label label
                          :created-at now
                          :deadline
                          (+ now niwa-session-transfer-prompt-timeout))))
              (unless (niwa-session-transfer--valid-request-p request now)
                (error "Invalid control-transfer request"))
              (niwa-session-transfer--directory directory "requests")
              (if (niwa-session-durability--create-plist
                   request-path request)
                  request
                (let ((winner (niwa-session-durability--read-plist
                               request-path)))
                  (unless
                      (niwa-session-transfer--valid-request-p winner now)
                    (error "Invalid portable control-transfer request: %s"
                           request-path))
                  (and (equal niwa-session-durability--client-id
                              (plist-get winner :requester-client-id))
                       (equal session-id (plist-get winner :session-id))
                       winner))))))))))

(defvar niwa-session-transfer--quiet-mailboxes
  (make-hash-table :test #'eq :weakness 'key)
  "Session to the request mailbox stamp its last empty owner poll saw.")

(defun niwa-session-transfer--mailbox-stamp (session)
  "Return a native stamp of local SESSION's request mailbox, or nil.
A requester adds a file to the flat mailbox, which changes its modification
time, so an unchanged stamp proves no request arrived.  Remote sessions
return nil: a remote stat costs the round trip it would save."
  (when-let* ((session-dir (niwa-session-save-path session))
              ((not (file-remote-p session-dir))))
    (let ((attributes (file-attributes
                       (file-name-concat
                        (niwa-session-durability--lease-path session-dir)
                        "requests"))))
      (if attributes
          (list (file-attribute-modification-time attributes)
                (file-attribute-inode-number attributes)
                (file-attribute-size attributes))
        'absent))))

(defun niwa-session-transfer-poll (session &optional admission)
  "Poll SESSION's immutable control-transfer request and decision records.

The owner may call this from a UI timer or command loop.  An unanswered
request is granted once `niwa-session-transfer-prompt-timeout' has passed
since this owner could first see it, but grant does not release the lease or
transfer authority.

A local owner with no transfer in flight skips the target program while its
request mailbox is unchanged since a poll that found no request: that program
spawned a shell every few seconds per view, and lease renewal, not this
poll, detects a lost lease.  With ADMISSION, carry the recovery-root and
PID-lock observations in the same program, within the caller's durable
transaction; the ordinary mutation gate consumes those proofs afterward."
  (unless (niwa-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (let ((stamp (niwa-session-transfer--mailbox-stamp session))
        (state (plist-get (niwa-session-control-transfer session) :state)))
    (if (and stamp
             (not (memq state '(requested quiescing)))
             (equal stamp (gethash session niwa-session-transfer--quiet-mailboxes)))
        (niwa-session-control-transfer session)
      (prog1 (niwa-session-transfer--poll-target session admission)
        (if (and stamp
                 (not (memq (plist-get (niwa-session-control-transfer session) :state)
                            '(requested quiescing))))
            (puthash session stamp niwa-session-transfer--quiet-mailboxes)
          (remhash session niwa-session-transfer--quiet-mailboxes))))))

(defun niwa-session-transfer--poll-target (session &optional admission)
  "Poll SESSION's transfer records, combining recovery probes for ADMISSION."
  (niwa-session-durability-with-transaction
    (let* ((session-dir (niwa-session-save-path session))
           (session-id (niwa-session-session-id session))
           (bound (niwa-session-lease session))
           (transfer (niwa-session-control-transfer session))
           (directory (and session-dir
                           (niwa-session-durability--lease-path
                            session-dir)))
           (pid-lock (and admission directory
                          (niwa-session-durability--pid-lock-operation
                           session-dir)))
           ;; One observation answers the clock, the listing, and the
           ;; bound generation's record; the poll used to pay each as its
           ;; own target process on every tick.
           (observed (and directory
                          (niwa-session-durability--observe-lease
                           directory (plist-get bound :generation)
                           (append
                            (list (niwa-session-transfer--requests-operation
                                   directory))
                            (when admission
                              (append
                               (and pid-lock (list pid-lock))
                               (list (list :op 'directory-p
                                           :path (niwa-session-recovery--root
                                                  session-dir)
                                           :optional t))))))))
           (current
            (and observed
                 (let ((record (plist-get observed :record)))
                   (if (and record
                            (not (eq 'aborted (plist-get record :status)))
                            (equal (plist-get record :generation)
                                   (niwa-session-durability--newest-generation
                                    (plist-get observed :names))))
                       record
                     (niwa-session-durability--lease-head
                      directory
                      (niwa-session-durability--observed-names
                       observed))))))
           (now (and current
                     (let ((niwa-session-durability--observed-time
                            (plist-get observed :now)))
                       (niwa-session-durability--target-time
                        directory)))))
    (when (and admission observed)
      (let ((results (cdr (plist-get observed :extra))))
        ;; Before first lease acquisition, the absent lease directory can
        ;; stop the program before these probes run.  Leave the transaction
        ;; caches unanswered: the ordinary authority gate must perform its
        ;; recovery/PID-lock checks before acquiring that first lease.
        (unless (cl-some (lambda (result)
                          (eq 'skipped (plist-get result :status)))
                        results)
          (when pid-lock
            (niwa-session-durability--settle-pid-lock session-dir (pop results)))
          (niwa-session-recovery-refresh session (car results)))))
    (when (and current bound
               (or (equal niwa-session-durability--client-id
                          (plist-get bound :client-id))
                   (eq 'claiming (plist-get bound :status)))
               (not (equal niwa-session-durability--client-id
                           (plist-get current :client-id))))
      (niwa-session-durability--bind-latest session current directory now))
    (when (and session-dir session-id
               (niwa-session-durability--valid-lease-p current)
               (equal niwa-session-durability--client-id
                      (plist-get current :client-id))
               (equal (plist-get bound :generation)
                      (plist-get current :generation))
               (memq (plist-get current :status) '(active publishing))
               (> (plist-get current :expires-at) now))
      (let* ((request
              (niwa-session-transfer--current-request
               directory current session-id now
               (car (plist-get observed :extra))))
             (observed-at
              (and request
                   (niwa-session-transfer--observed-at
                    session request now)))
             (decision-path
              (and request
                   (niwa-session-transfer--decision-path
                    directory (plist-get request :generation))))
             (decision
              ;; An absent decision reads as nil, so the existence probe
              ;; was a second target process answering the same question.
              (when-let* ((decision-path)
                          (value (niwa-session-durability--read-plist
                                  decision-path)))
                (unless
                    (and (niwa-session-transfer--valid-decision-p
                          value)
                         (niwa-session-transfer--record-matches-p
                          request value))
                  (error "Invalid portable control-transfer decision: %s"
                         decision-path))
                value)))
        ;; The timeout exists to give the owner a chance to refuse, so it
        ;; runs from when this owner could first see the request, not from
        ;; when the requester wrote it.  A poll interval can be longer than
        ;; the timeout, and measuring from `:created-at' then granted
        ;; requests the owner had never displayed -- silently, from the
        ;; user's point of view.
        (when (and request (not decision)
                   (or (niwa-engine-external-history-p session)
                       (progn
                     (unless
                         (niwa-session-transfer--valid-timeout-p
                          niwa-session-transfer-prompt-timeout)
                       (error "Invalid control-transfer timeout: %S"
                              niwa-session-transfer-prompt-timeout))
                     (<= (+ (max (plist-get request :created-at) observed-at)
                            niwa-session-transfer-prompt-timeout)
                         now))))
          (let ((candidate
                 (list :protocol-version 1
                       :request-id (plist-get request :request-id)
                       :session-id (plist-get request :session-id)
                       :generation (plist-get request :generation)
                       :owner-client-id (plist-get request :owner-client-id)
                       :requester-client-id
                       (plist-get request :requester-client-id)
                       :decision (if (niwa-engine-external-history-p session)
                                     'reject 'grant)
                       :decided-at now)))
            (niwa-session-transfer--directory directory "requests")
            (unless (niwa-session-durability--create-plist
                     decision-path candidate)
              (setq candidate
                    (niwa-session-durability--read-plist decision-path))
              (unless (and
                       (niwa-session-transfer--valid-decision-p
                        candidate)
                       (niwa-session-transfer--record-matches-p
                        request candidate))
                (error "Invalid portable control-transfer decision: %s"
                       decision-path)))
            (setq decision candidate)))
        (let ((live (niwa-session-control-transfer session)))
          (cond
           ;; Preserve a nested callback's newer state, except that this
           ;; poll's durable decision supersedes a refresh of the same
           ;; undecided request.  Otherwise an outer timeout grant could
           ;; leave admission open after a nested poll set `requested'.
           ((and (not (eq transfer live))
                 (not (and decision
                           (eq 'requested (plist-get live :state))
                           (niwa-session-transfer--record-matches-p
                            request (plist-get live :request)))))
            live)
           ((not request) live)
           ((not decision)
            (niwa-session-transfer--set-state
             session 'requested request nil observed-at))
           ((eq 'grant (plist-get decision :decision))
            (niwa-session-transfer--set-state
             session 'quiescing request decision observed-at))
           (t
            (niwa-session-transfer--set-state
             session 'rejected request decision observed-at)))))))))

(defun niwa-session-transfer-decide
    (session decision)
  "Create the immutable owner DECISION for SESSION's current request.

DECISION is `grant' or `reject'.  Repeating the same decision is idempotent;
attempting to rewrite an existing decision fails."
  (niwa-session-durability--gate session nil)
  (let ((niwa-session-durability--heartbeat-inhibit t))
    (niwa-session-transfer--decide session decision)))

(defun niwa-session-transfer--decide (session decision)
  "Apply DECISION for SESSION with heartbeat admission excluded by the caller."
  (when (eq decision 'grant)
    (niwa-engine-assert-local-history session "Session control transfer"))
  (unless (niwa-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (unless (memq decision '(grant reject))
    (error "Control-transfer decision must be grant or reject"))
  (let* ((transfer (niwa-session-control-transfer session))
         (prior (and (eq (plist-get transfer :state) 'rejected)
                     (plist-get transfer :decision))))
    (if prior
        (if (eq decision (plist-get prior :decision))
            prior
          (error "Control-transfer decision is already rejected"))
      (let* ((session-dir (niwa-session-save-path session))
             (session-id (niwa-session-session-id session))
             (directory
              (and session-dir
                   (niwa-session-durability--lease-path session-dir)))
             (current
              (and directory
                   (niwa-session-durability--lease-head directory)))
             (now
              (and current
                   (niwa-session-durability--target-time directory)))
             (request
              (and directory
                   (niwa-session-durability--valid-lease-p current)
                   (niwa-session-transfer--current-request
                    directory current session-id now))))
        (unless (and session-dir session-id
                     (niwa-session-durability--valid-lease-p current)
                     (equal niwa-session-durability--client-id
                            (plist-get current :client-id))
                     (memq (plist-get current :status) '(active publishing))
                     (> (plist-get current :expires-at) now)
                     request)
          (error "No current control-transfer request"))
        (niwa-session-transfer--directory directory "requests")
        (let* ((path (niwa-session-transfer--decision-path
                      directory (plist-get request :generation)))
               (candidate
                (list :protocol-version 1
                      :request-id (plist-get request :request-id)
                      :session-id (plist-get request :session-id)
                      :generation (plist-get request :generation)
                      :owner-client-id (plist-get request :owner-client-id)
                      :requester-client-id
                      (plist-get request :requester-client-id)
                      :decision decision
                      :decided-at now))
               (existing
                (if (niwa-session-durability--create-plist path candidate)
                    candidate
                  (let ((value (niwa-session-durability--read-plist path)))
                    (unless
                        (and
                         (niwa-session-transfer--valid-decision-p
                          value)
                         (niwa-session-transfer--record-matches-p
                          request value))
                      (error "Invalid portable control-transfer decision: %s"
                             path))
                    value))))
          (when (and (eq decision 'grant)
                     (not (eq (plist-get existing :decision) 'grant)))
            (error "Control-transfer decision is already rejected"))
          (when (and (eq decision 'reject)
                     (not (eq (plist-get existing :decision) 'reject)))
            (error "Control-transfer decision is already granted"))
          (if (eq 'reject (plist-get existing :decision))
              (let ((successor
                     (niwa-session-durability--claim-next
                      directory current (plist-get current :buffer)
                      nil nil nil
                      (1+ (plist-get request :generation)))))
                (unless successor
                  (error
                   "Could not rotate rejected control-transfer generation"))
                (niwa-session-durability--bind-lease
                 session successor 'owned)
                (niwa-session-transfer--set-state
                 session 'rejected request existing))
            (niwa-session-transfer--set-state
             session 'quiescing request existing))
          existing)))))

(defun niwa-session-transfer-release (session)
  "Release SESSION's lease for its granted control-transfer request.

The requester's named release fence is created before the old lease is marked
released.  Creating the fence or releasing the lease is idempotent, while
acceptance alone never changes the lease owner."
  (niwa-session-durability--gate session nil)
  (let ((niwa-session-durability--heartbeat-inhibit t))
    (niwa-session-transfer--release session)))

(defun niwa-session-transfer--release (session)
  "Release SESSION's transfer lease with heartbeat admission excluded."
  (niwa-engine-assert-local-history session "Session control transfer")
  (unless (niwa-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (let* ((transfer (niwa-session-control-transfer session))
         (request (plist-get transfer :request))
         (decision (plist-get transfer :decision))
         (session-dir (niwa-session-save-path session))
         (directory (and session-dir
                         (niwa-session-durability--lease-path session-dir)))
         (current (and directory
                       (niwa-session-durability--lease-head directory)))
         (now (and directory
                   (niwa-session-durability--target-time directory)))
         (release-generation
          (or (plist-get transfer :release-generation)
              (and (niwa-session-durability--valid-lease-p current)
                   (eq 'released (plist-get current :status))
                   (equal (plist-get request :owner-client-id)
                          (plist-get current :client-id))
                   (plist-get current :generation))))
         (released-fence
          (and release-generation
               (niwa-session-transfer-release-fence
                directory release-generation))))
    (when released-fence
      (unless (niwa-session-transfer--record-identity-matches-p
               request released-fence)
        (error "Invalid portable control-transfer fence: %s"
               (niwa-session-transfer--fence-path
                directory release-generation))))
    (cond
     ((eq (plist-get transfer :state) 'released)
      (or released-fence
          (error "Released control-transfer fence has expired")))
     ((and (eq (plist-get current :status) 'released) released-fence)
      (niwa-session-set-control-transfer
       session
       (list :state 'released :request request :decision decision
             :release-generation release-generation))
      released-fence)
     (t
      (unless (niwa-session-transfer--valid-timeout-p
               niwa-session-transfer-prompt-timeout)
        (error "Invalid control-transfer timeout: %S"
               niwa-session-transfer-prompt-timeout))
      (let ((bound (niwa-session-lease session)))
        (unless (and session-dir
                     (eq (plist-get transfer :state) 'quiescing)
                     request
                     (eq (plist-get decision :decision) 'grant)
                     (niwa-session-durability--valid-lease-p current)
                     (niwa-session-durability--owned-lease-record-p
                      current directory now)
                     (equal (plist-get bound :generation)
                            (plist-get current :generation))
                     (natnump (plist-get request :generation))
                     (<= (plist-get request :generation)
                         (plist-get current :generation))
                     (equal (plist-get request :owner-client-id)
                            (plist-get current :client-id)))
          (error "Control transfer is not ready to release")))
      (setq release-generation (plist-get current :generation))
      (niwa-session-transfer--directory directory "fences")
      (let* ((path (niwa-session-transfer--fence-path
                    directory release-generation))
             (candidate
              (list :protocol-version 1
                    :generation release-generation
                    :request-id (plist-get request :request-id)
                    :session-id (plist-get request :session-id)
                    :owner-client-id (plist-get current :client-id)
                    :requester-client-id
                    (plist-get request :requester-client-id)
                    :expires-at
                    (+ now niwa-session-transfer-prompt-timeout)))
             (fence
              (if (niwa-session-durability--create-plist path candidate)
                  candidate
                (let ((value (niwa-session-durability--read-plist path)))
                  (unless
                      (and (niwa-session-transfer--valid-fence-p value now)
                           (niwa-session-transfer--record-identity-matches-p
                            request value)
                           (equal (plist-get value :owner-client-id)
                                  (plist-get current :client-id)))
                    (error "Invalid portable control-transfer fence: %s" path))
                  value))))
        (niwa-session-set-control-transfer
         session
         (list :state 'quiescing :request request :decision decision
               :release-generation release-generation))
        (niwa-session-durability-lease-release session-dir session)
        (niwa-session-set-control-transfer
         session
         (list :state 'released :request request :decision decision
               :release-generation release-generation))
        fence)))))

(provide 'niwa-session-transfer)

;;; niwa-session-transfer.el ends here
