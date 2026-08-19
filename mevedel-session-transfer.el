;;; mevedel-session-transfer.el -- Durable cooperative control transfer -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the immutable request, decision, and release-fence protocol for cooperative portable-session control transfer.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst mevedel-session-transfer--maximum-timeout 300
  "Maximum accepted cooperative transfer timeout in seconds.")

;; Every control operation in this file runs through the session control
;; filesystem, so its feature is a hard load-time dependency rather than a
;; lazily reachable one.
(require 'mevedel-session-control-fs)
;; The durable-transaction macro must expand for interpreted loads too,
;; so this is a load-time dependency rather than a lazy one.
(require 'mevedel-session-durability)

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-directory-p
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-list-directory
                  "mevedel-session-control-fs" (directory regexp))
(declare-function mevedel-session-control-fs-make-directory
                  "mevedel-session-control-fs" (path &optional parents))
(declare-function mevedel-session-control-fs-path-exists-p
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-control-fs" (path))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability--assert-no-pid-lock
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability--bind-lease
                  "mevedel-session-durability" (session lease state))
(declare-function mevedel-session-durability--claim-next
                  "mevedel-session-durability"
                  (directory expected buffer-name
                             &optional status unsettled-mutation-p
                             unsettled-mutation transfer-generation))
(declare-function mevedel-session-durability--create-plist
                  "mevedel-session-durability" (path plist))
(declare-function mevedel-session-durability--ensure-lease-directory
                  "mevedel-session-durability" (directory))
(declare-function mevedel-session-durability--lease-head
                  "mevedel-session-durability" (directory &optional names))
(declare-function mevedel-session-durability--lease-path
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability--newest-generation
                  "mevedel-session-durability" (names))
(declare-function mevedel-session-durability--observe-lease
                  "mevedel-session-durability" (directory generation))
(declare-function mevedel-session-durability--observed-names
                  "mevedel-session-durability" (observed))
(declare-function mevedel-session-durability--owned-lease-record-p
                  "mevedel-session-durability" (lease directory &optional now))
(declare-function mevedel-session-durability--portable-session-p
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability--read-plist
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-durability--target-time
                  "mevedel-session-durability" (directory))
(declare-function mevedel-session-durability--valid-lease-p
                  "mevedel-session-durability" (lease))
(declare-function mevedel-session-durability-lease-release
                  "mevedel-session-durability"
                  (session-dir &optional session))
(defvar mevedel-session-durability--client-id)
(defvar mevedel-session-durability--observed-time)

;; `mevedel-structs'
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-control-transfer
                  "mevedel-structs" (session transfer))


(defcustom mevedel-session-transfer-prompt-timeout 30
  "Seconds before an unanswered control-transfer request is granted.

The same short window is used for the release fence left for the named
requester after the owner has released its lease."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-transfer-client-label "Emacs"
  "User-visible label attached to cooperative lease-transfer requests."
  :type 'string
  :group 'mevedel)

(defun mevedel-session-transfer--path (directory name)
  "Return transfer subdirectory NAME below lease DIRECTORY without creating it.

Reads use this: an absent mailbox holds no requests, and a poll that created
target directories would mutate the session on behalf of an observer."
  (mevedel-session-control-fs-physical-path directory)
  (mevedel-session-control-fs-physical-path
   (file-name-concat directory name)))

(defun mevedel-session-transfer--directory (directory name)
  "Ensure and return transfer subdirectory NAME below lease DIRECTORY."
  (let ((path (mevedel-session-transfer--path directory name)))
    (cond
     ((mevedel-session-control-fs-directory-p path) path)
     ((mevedel-session-control-fs-path-exists-p path)
      (error "Invalid portable control-transfer directory: %s" path))
     (t
      (condition-case nil
          (mevedel-session-control-fs-make-directory path)
        (file-already-exists nil))
      (setq path (mevedel-session-control-fs-physical-path path))
      (unless (mevedel-session-control-fs-directory-p path)
        (error "Could not create portable control-transfer directory: %s"
               path))
      path))))

(defun mevedel-session-transfer--finite-nonnegative-number-p (value)
  "Return non-nil when VALUE is a finite nonnegative number."
  (and (numberp value)
       (>= value 0)
       (or (integerp value)
           (and (floatp value)
                (not (isnan value))
                (<= value most-positive-fixnum)))))

(defun mevedel-session-transfer--id-p (value length)
  "Return non-nil when VALUE is a lowercase hexadecimal ID of LENGTH."
  (and (stringp value)
       (= (length value) length)
       (string-match-p (format "\\`[0-9a-f]\\{%d\\}\\'" length) value)))

(defun mevedel-session-transfer--record-keys-p (record keys)
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

(defun mevedel-session-transfer--fence-path (directory generation)
  "Return the immutable release-fence path for GENERATION in DIRECTORY."
  (let ((path
         (file-name-concat
          (mevedel-session-transfer--directory directory "fences")
          (format "fence-%020d.el" generation))))
    (mevedel-session-control-fs-physical-path path)))

(defun mevedel-session-transfer--valid-fence-p (record &optional now)
  "Return non-nil when RECORD is a trusted immutable release fence."
  (and (mevedel-session-transfer--record-keys-p
        record '(:protocol-version :generation :request-id :session-id
                 :owner-client-id :requester-client-id :expires-at))
       (= 1 (plist-get record :protocol-version))
       (natnump (plist-get record :generation))
       (> (plist-get record :generation) 0)
       (mevedel-session-transfer--id-p
        (plist-get record :request-id) 32)
       (stringp (plist-get record :session-id))
       (> (length (plist-get record :session-id)) 0)
       (mevedel-session-transfer--id-p
        (plist-get record :owner-client-id) 64)
       (mevedel-session-transfer--id-p
        (plist-get record :requester-client-id) 64)
       (mevedel-session-transfer--finite-nonnegative-number-p
        (plist-get record :expires-at))
       (or (null now)
           (<= (plist-get record :expires-at)
               (+ now mevedel-session-transfer--maximum-timeout)))))

(defun mevedel-session-transfer-release-fence (directory generation)
  "Return the validated live release fence for GENERATION, or nil."
  (let* ((path (mevedel-session-transfer--fence-path directory generation))
         (now (mevedel-session-durability--target-time directory)))
    (when (mevedel-session-control-fs-path-exists-p path)
      (let ((fence (mevedel-session-durability--read-plist path)))
        (unless (mevedel-session-transfer--valid-fence-p fence now)
          (error "Invalid portable control-transfer fence: %s" path))
        (when (> (plist-get fence :expires-at) now)
          fence)))))

(defun mevedel-session-transfer--valid-timeout-p (value)
  "Return non-nil when VALUE is an accepted transfer timeout."
  (and (mevedel-session-transfer--finite-nonnegative-number-p value)
       (> value 0)
       (<= value mevedel-session-transfer--maximum-timeout)))

(defun mevedel-session-transfer--valid-client-label-p (value)
  "Return non-nil when VALUE is safe bounded transfer display text."
  (and (stringp value)
       (> (length value) 0)
       (<= (length value) 128)
       (not (string-match-p "[[:cntrl:]]" value))))

(defun mevedel-session-transfer--request-path
    (directory generation)
  "Return the immutable request path for lease GENERATION in DIRECTORY."
  (let ((path
         (file-name-concat
          (mevedel-session-transfer--directory directory "requests")
          (format "request-%020d.el" generation))))
    (mevedel-session-control-fs-physical-path path)))

(defun mevedel-session-transfer--decision-path
    (directory generation)
  "Return the immutable decision path for lease GENERATION in DIRECTORY."
  (let ((path
         (file-name-concat
          (mevedel-session-transfer--directory directory "requests")
          (format "decision-%020d.el" generation))))
    (mevedel-session-control-fs-physical-path path)))

(defun mevedel-session-transfer--valid-request-p (record &optional now)
  "Return non-nil when RECORD is a trusted current request representation."
  (and (mevedel-session-transfer--record-keys-p
        record '(:protocol-version :request-id :session-id :generation
                 :owner-client-id :requester-client-id :requester-label
                 :created-at :deadline))
       (= 1 (plist-get record :protocol-version))
       (mevedel-session-transfer--id-p
        (plist-get record :request-id) 32)
       (stringp (plist-get record :session-id))
       (> (length (plist-get record :session-id)) 0)
       (natnump (plist-get record :generation))
       (> (plist-get record :generation) 0)
       (mevedel-session-transfer--id-p
        (plist-get record :owner-client-id) 64)
       (mevedel-session-transfer--id-p
        (plist-get record :requester-client-id) 64)
       (mevedel-session-transfer--valid-client-label-p
        (plist-get record :requester-label))
       (mevedel-session-transfer--finite-nonnegative-number-p
        (plist-get record :created-at))
       (mevedel-session-transfer--finite-nonnegative-number-p
        (plist-get record :deadline))
       (>= (plist-get record :deadline) (plist-get record :created-at))
       (<= (- (plist-get record :deadline)
              (plist-get record :created-at))
           mevedel-session-transfer--maximum-timeout)
       (or (null now)
           (<= (plist-get record :created-at)
               (+ now mevedel-session-transfer--maximum-timeout)))))

(defun mevedel-session-transfer--valid-decision-p (record)
  "Return non-nil when RECORD is a trusted immutable decision."
  (and (mevedel-session-transfer--record-keys-p
        record '(:protocol-version :request-id :session-id :generation
                 :owner-client-id :requester-client-id :decision :decided-at))
       (= 1 (plist-get record :protocol-version))
       (mevedel-session-transfer--id-p
        (plist-get record :request-id) 32)
       (stringp (plist-get record :session-id))
       (> (length (plist-get record :session-id)) 0)
       (natnump (plist-get record :generation))
       (> (plist-get record :generation) 0)
       (mevedel-session-transfer--id-p
        (plist-get record :owner-client-id) 64)
       (mevedel-session-transfer--id-p
        (plist-get record :requester-client-id) 64)
       (memq (plist-get record :decision) '(grant reject))
       (mevedel-session-transfer--finite-nonnegative-number-p
        (plist-get record :decided-at))))

(defun mevedel-session-transfer--record-identity-matches-p
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

(defun mevedel-session-transfer--record-matches-p
    (request record)
  "Return non-nil when immutable transfer RECORD matches REQUEST."
  (and (mevedel-session-transfer--record-identity-matches-p request record)
       (equal (plist-get request :generation)
              (plist-get record :generation))))

(defun mevedel-session-transfer--request-id ()
  "Return a fresh opaque control-transfer request ID."
  (substring
   (secure-hash 'sha256
                (format "%S"
                        (list (current-time) (random most-positive-fixnum)
                              (emacs-pid) mevedel-session-durability--client-id)))
   0 32))

(defun mevedel-session-transfer--current-request
    (directory lease session-id &optional now)
  "Read DIRECTORY's newest live request for SESSION-ID, or nil.

The owner's lease generation legitimately advances while a request is
pending: settling the durable mutation latch and publication both rotate
the continuously owned lease.  The newest request at or below LEASE's
generation is therefore the live one; it must still name LEASE's client
as its owner, so a request left over from an earlier ownership cannot
cross a release."
  (let* ((generation (plist-get lease :generation))
         (now (or now (mevedel-session-durability--target-time directory)))
         (path
          (car (sort (mevedel-session-control-fs-list-directory
                      (mevedel-session-transfer--path directory "requests")
                      "\\`request-[0-9]\\{20\\}\\.el\\'")
                     #'string>))))
    (when path
      (let ((request (mevedel-session-durability--read-plist path)))
        (unless (mevedel-session-transfer--valid-request-p request now)
          (error "Invalid portable control-transfer request: %s" path))
        (and (<= (plist-get request :generation) generation)
             (equal session-id (plist-get request :session-id))
             (equal (plist-get lease :client-id)
                    (plist-get request :owner-client-id))
             request)))))

(defun mevedel-session-transfer-observe-decision (session request)
  "Return REQUEST's validated immutable decision for requester SESSION."
  (unless (and (mevedel-session-durability--portable-session-p session)
               (equal (mevedel-session-session-id session)
                      (plist-get request :session-id))
               (equal mevedel-session-durability--client-id
                      (plist-get request :requester-client-id)))
    (error "Control-transfer request does not belong to this session client"))
  (let* ((directory
          (mevedel-session-durability--lease-path
           (mevedel-session-save-path session)))
         (path
          (mevedel-session-transfer--decision-path
           directory (plist-get request :generation)))
         (decision (mevedel-session-durability--read-plist path)))
    (when decision
      (unless (and (mevedel-session-transfer--valid-decision-p decision)
                   (mevedel-session-transfer--record-matches-p
                    request decision))
        (error "Invalid portable control-transfer decision: %s" path))
      decision)))

(defun mevedel-session-transfer--set-state
    (session state request &optional decision observed-at)
  "Set transient SESSION control-transfer STATE for REQUEST and DECISION.

OBSERVED-AT is the target time at which this owner first saw REQUEST.  It is
transient client state, not part of the durable protocol: a restarted owner
simply starts its notice window again."
  (mevedel-session-set-control-transfer
   session (list :state state :request request :decision decision
                 :observed-at observed-at)))

(defun mevedel-session-transfer--observed-at (session request now)
  "Return when this owner first saw REQUEST for SESSION, defaulting to NOW."
  (let ((previous (mevedel-session-control-transfer session)))
    (or (and (equal (plist-get (plist-get previous :request) :request-id)
                    (plist-get request :request-id))
             (plist-get previous :observed-at))
        now)))

(defun mevedel-session-transfer-request
    (session &optional label)
  "Atomically request control of portable SESSION.

Return the immutable request record, or nil when another requester won the
current-generation race.  The owner remains the sole mutator until it
explicitly decides and releases its lease."
  (unless (mevedel-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (let ((session-dir (or (mevedel-session-save-path session)
                         (error "Session has not been materialized yet")))
        (session-id (or (mevedel-session-session-id session)
                        (error "Session has no durable identity"))))
    (mevedel-session-durability--assert-no-pid-lock session-dir)
    (unless (and (stringp session-id) (> (length session-id) 0))
      (error "Control-transfer session ID is required"))
    (unless (mevedel-session-transfer--valid-timeout-p
             mevedel-session-transfer-prompt-timeout)
      (error "Invalid control-transfer timeout: %S"
             mevedel-session-transfer-prompt-timeout))
    (setq label (or label mevedel-session-transfer-client-label))
    (unless (mevedel-session-transfer--valid-client-label-p label)
      (error "Invalid control-transfer client label"))
    (let* ((directory (mevedel-session-durability--lease-path session-dir))
           (_ (mevedel-session-durability--ensure-lease-directory directory))
           (now (mevedel-session-durability--target-time directory))
           (lease (mevedel-session-durability--lease-head directory)))
      (unless (and (mevedel-session-durability--valid-lease-p lease)
                   (memq (plist-get lease :status) '(active publishing))
                   (> (plist-get lease :expires-at) now))
        (error "Control transfer requires a live portable lease"))
      (when (equal mevedel-session-durability--client-id
                   (plist-get lease :client-id))
        (error "The current lease owner cannot request control transfer"))
      (let* ((live-request
              (mevedel-session-transfer--current-request
               directory lease session-id now))
             (live-decision
              (and live-request
                   (mevedel-session-durability--read-plist
                    (mevedel-session-transfer--decision-path
                     directory (plist-get live-request :generation)))))
             (_
              (when (and live-decision
                         (not
                          (and
                           (mevedel-session-transfer--valid-decision-p
                            live-decision)
                           (mevedel-session-transfer--record-matches-p
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
              (mevedel-session-transfer--request-path
               directory request-generation))
             (decision-path
              (mevedel-session-transfer--decision-path
               directory request-generation)))
        (if existing
            (and (equal mevedel-session-durability--client-id
                        (plist-get existing :requester-client-id))
                 (equal session-id (plist-get existing :session-id))
                 existing)
          (unless (and rejected
                       (/= request-generation
                           (1+ (plist-get live-request :generation))))
            (when (mevedel-session-control-fs-path-exists-p decision-path)
              (error "Portable control-transfer decision has no request: %s"
                     decision-path))
            (let* ((request
                    (list :protocol-version 1
                          :request-id
                          (mevedel-session-transfer--request-id)
                          :session-id session-id
                          :generation request-generation
                          :owner-client-id (plist-get lease :client-id)
                          :requester-client-id
                          mevedel-session-durability--client-id
                          :requester-label label
                          :created-at now
                          :deadline
                          (+ now mevedel-session-transfer-prompt-timeout))))
              (unless (mevedel-session-transfer--valid-request-p request now)
                (error "Invalid control-transfer request"))
              (if (mevedel-session-durability--create-plist
                   request-path request)
                  request
                (let ((winner (mevedel-session-durability--read-plist
                               request-path)))
                  (unless
                      (mevedel-session-transfer--valid-request-p winner now)
                    (error "Invalid portable control-transfer request: %s"
                           request-path))
                  (and (equal mevedel-session-durability--client-id
                              (plist-get winner :requester-client-id))
                       (equal session-id (plist-get winner :session-id))
                       winner))))))))))

(defun mevedel-session-transfer-poll (session)
  "Poll SESSION's immutable control-transfer request and decision records.

The owner may call this from a UI timer or command loop.  An unanswered
request is granted once `mevedel-session-transfer-prompt-timeout' has passed
since this owner could first see it, but grant does not release the lease or
transfer authority."
  (unless (mevedel-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (mevedel-session-durability-with-transaction
    (let* ((session-dir (mevedel-session-save-path session))
           (session-id (mevedel-session-session-id session))
           (bound (mevedel-session-lease session))
           (directory (and session-dir
                           (mevedel-session-durability--lease-path
                            session-dir)))
           ;; One observation answers the clock, the listing, and the
           ;; bound generation's record; the poll used to pay each as its
           ;; own target process on every tick.
           (observed (and directory
                          (mevedel-session-durability--observe-lease
                           directory (plist-get bound :generation))))
           (current
            (and observed
                 (let ((record (plist-get observed :record)))
                   (if (and record
                            (not (eq 'aborted (plist-get record :status)))
                            (equal (plist-get record :generation)
                                   (mevedel-session-durability--newest-generation
                                    (plist-get observed :names))))
                       record
                     (mevedel-session-durability--lease-head
                      directory
                      (mevedel-session-durability--observed-names
                       observed))))))
           (now (and current
                     (let ((mevedel-session-durability--observed-time
                            (plist-get observed :now)))
                       (mevedel-session-durability--target-time
                        directory)))))
    (when (and session-dir session-id
               (mevedel-session-durability--valid-lease-p current)
               (equal mevedel-session-durability--client-id
                      (plist-get current :client-id))
               (equal (plist-get bound :generation)
                      (plist-get current :generation))
               (memq (plist-get current :status) '(active publishing))
               (> (plist-get current :expires-at) now))
      (let* ((request
              (mevedel-session-transfer--current-request
               directory current session-id now))
             (observed-at
              (and request
                   (mevedel-session-transfer--observed-at
                    session request now)))
             (decision-path
              (and request
                   (mevedel-session-transfer--decision-path
                    directory (plist-get request :generation))))
             (decision
              ;; An absent decision reads as nil, so the existence probe
              ;; was a second target process answering the same question.
              (when-let* ((decision-path)
                          (value (mevedel-session-durability--read-plist
                                  decision-path)))
                (unless
                    (and (mevedel-session-transfer--valid-decision-p
                          value)
                         (mevedel-session-transfer--record-matches-p
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
                   (progn
                     (unless
                         (mevedel-session-transfer--valid-timeout-p
                          mevedel-session-transfer-prompt-timeout)
                       (error "Invalid control-transfer timeout: %S"
                              mevedel-session-transfer-prompt-timeout))
                     (<= (+ (max (plist-get request :created-at) observed-at)
                            mevedel-session-transfer-prompt-timeout)
                         now)))
          (let ((candidate
                 (list :protocol-version 1
                       :request-id (plist-get request :request-id)
                       :session-id (plist-get request :session-id)
                       :generation (plist-get request :generation)
                       :owner-client-id (plist-get request :owner-client-id)
                       :requester-client-id
                       (plist-get request :requester-client-id)
                       :decision 'grant
                       :decided-at now)))
            (unless (mevedel-session-durability--create-plist
                     decision-path candidate)
              (setq candidate
                    (mevedel-session-durability--read-plist decision-path))
              (unless (and
                       (mevedel-session-transfer--valid-decision-p
                        candidate)
                       (mevedel-session-transfer--record-matches-p
                        request candidate))
                (error "Invalid portable control-transfer decision: %s"
                       decision-path)))
            (setq decision candidate)))
        (cond
         ((not request)
          (mevedel-session-control-transfer session))
         ((not decision)
          (mevedel-session-transfer--set-state
           session 'requested request nil observed-at))
         ((eq 'grant (plist-get decision :decision))
          (mevedel-session-transfer--set-state
           session 'quiescing request decision observed-at))
         (t
          (mevedel-session-transfer--set-state
           session 'rejected request decision observed-at))))))))

(defun mevedel-session-transfer-decide
    (session decision)
  "Create the immutable owner DECISION for SESSION's current request.

DECISION is `grant' or `reject'.  Repeating the same decision is idempotent;
attempting to rewrite an existing decision fails."
  (unless (mevedel-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (unless (memq decision '(grant reject))
    (error "Control-transfer decision must be grant or reject"))
  (let* ((transfer (mevedel-session-control-transfer session))
         (prior (and (eq (plist-get transfer :state) 'rejected)
                     (plist-get transfer :decision))))
    (if prior
        (if (eq decision (plist-get prior :decision))
            prior
          (error "Control-transfer decision is already rejected"))
      (let* ((session-dir (mevedel-session-save-path session))
             (session-id (mevedel-session-session-id session))
             (directory
              (and session-dir
                   (mevedel-session-durability--lease-path session-dir)))
             (current
              (and directory
                   (mevedel-session-durability--lease-head directory)))
             (now
              (and current
                   (mevedel-session-durability--target-time directory)))
             (request
              (and directory
                   (mevedel-session-durability--valid-lease-p current)
                   (mevedel-session-transfer--current-request
                    directory current session-id now))))
        (unless (and session-dir session-id
                     (mevedel-session-durability--valid-lease-p current)
                     (equal mevedel-session-durability--client-id
                            (plist-get current :client-id))
                     (memq (plist-get current :status) '(active publishing))
                     (> (plist-get current :expires-at) now)
                     request)
          (error "No current control-transfer request"))
        (let* ((path (mevedel-session-transfer--decision-path
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
                (if (mevedel-session-durability--create-plist path candidate)
                    candidate
                  (let ((value (mevedel-session-durability--read-plist path)))
                    (unless
                        (and
                         (mevedel-session-transfer--valid-decision-p
                          value)
                         (mevedel-session-transfer--record-matches-p
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
                     (mevedel-session-durability--claim-next
                      directory current (plist-get current :buffer)
                      nil nil nil
                      (1+ (plist-get request :generation)))))
                (unless successor
                  (error
                   "Could not rotate rejected control-transfer generation"))
                (mevedel-session-durability--bind-lease
                 session successor 'owned)
                (mevedel-session-transfer--set-state
                 session 'rejected request existing))
            (mevedel-session-transfer--set-state
             session 'quiescing request existing))
          existing)))))

(defun mevedel-session-transfer-release (session)
  "Release SESSION's lease for its granted control-transfer request.

The requester's named release fence is created before the old lease is marked
released.  Creating the fence or releasing the lease is idempotent, while
acceptance alone never changes the lease owner."
  (unless (mevedel-session-durability--portable-session-p session)
    (error "Control transfer requires a portable project session"))
  (let* ((transfer (mevedel-session-control-transfer session))
         (request (plist-get transfer :request))
         (decision (plist-get transfer :decision))
         (session-dir (mevedel-session-save-path session))
         (directory (and session-dir
                         (mevedel-session-durability--lease-path session-dir)))
         (current (and directory
                       (mevedel-session-durability--lease-head directory)))
         (now (and directory
                   (mevedel-session-durability--target-time directory)))
         (release-generation
          (or (plist-get transfer :release-generation)
              (and (mevedel-session-durability--valid-lease-p current)
                   (eq 'released (plist-get current :status))
                   (equal (plist-get request :owner-client-id)
                          (plist-get current :client-id))
                   (plist-get current :generation))))
         (released-fence
          (and release-generation
               (mevedel-session-transfer-release-fence
                directory release-generation))))
    (when released-fence
      (unless (mevedel-session-transfer--record-identity-matches-p
               request released-fence)
        (error "Invalid portable control-transfer fence: %s"
               (mevedel-session-transfer--fence-path
                directory release-generation))))
    (cond
     ((eq (plist-get transfer :state) 'released)
      (or released-fence
          (error "Released control-transfer fence has expired")))
     ((and (eq (plist-get current :status) 'released) released-fence)
      (mevedel-session-set-control-transfer
       session
       (list :state 'released :request request :decision decision
             :release-generation release-generation))
      released-fence)
     (t
      (unless (mevedel-session-transfer--valid-timeout-p
               mevedel-session-transfer-prompt-timeout)
        (error "Invalid control-transfer timeout: %S"
               mevedel-session-transfer-prompt-timeout))
      (let ((bound (mevedel-session-lease session)))
        (unless (and session-dir
                     (eq (plist-get transfer :state) 'quiescing)
                     request
                     (eq (plist-get decision :decision) 'grant)
                     (mevedel-session-durability--valid-lease-p current)
                     (mevedel-session-durability--owned-lease-record-p
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
      (let* ((path (mevedel-session-transfer--fence-path
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
                    (+ now mevedel-session-transfer-prompt-timeout)))
             (fence
              (if (mevedel-session-durability--create-plist path candidate)
                  candidate
                (let ((value (mevedel-session-durability--read-plist path)))
                  (unless
                      (and (mevedel-session-transfer--valid-fence-p value now)
                           (mevedel-session-transfer--record-identity-matches-p
                            request value)
                           (equal (plist-get value :owner-client-id)
                                  (plist-get current :client-id)))
                    (error "Invalid portable control-transfer fence: %s" path))
                  value))))
        (mevedel-session-set-control-transfer
         session
         (list :state 'quiescing :request request :decision decision
               :release-generation release-generation))
        (mevedel-session-durability-lease-release session-dir session)
        (mevedel-session-set-control-transfer
         session
         (list :state 'released :request request :decision decision
               :release-generation release-generation))
        fence)))))

(provide 'mevedel-session-transfer)

;;; mevedel-session-transfer.el ends here
