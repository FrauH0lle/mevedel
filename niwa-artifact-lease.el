;;; niwa-artifact-lease.el --- Cross-Emacs leases on store items -*- lexical-binding: t -*-

;;; Commentary:

;; A whiteboard or document in the workspace artifact store is one live
;; object, so only one Emacs may commit it at a time.  This module gives each
;; item a lease with the session lease's lifecycle: generations elected by
;; exclusive creation on the target, target-clock deadlines, heartbeat
;; renewal, release after a quiet period, cooperative hand-over and a takeover
;; after expiry.  Generation changes and item writes share a target lock,
;; using the session durability record codec.  Writes replace the whole item
;; and are fenced by the lease, so taking over from a holder that stopped
;; renewing loses nothing: a late write from it fails.  No session state is
;; involved.
;;
;; Leases are machine state, so they live outside the store, under
;; `.niwa/leases/artifacts/ID/': the store itself stays safe to commit.
;; An Emacs that does not hold an item's lease sees it read-only; trying to
;; edit asks the holder to hand it over when it is idle.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-transport)
(require 'niwa-utilities)
(require 'niwa-workspace)

;; `niwa-shared-editing'
(declare-function niwa-shared-editing-item-busy-p "niwa-shared-editing" (workspace id))

(defcustom niwa-artifact-lease-idle-seconds 300
  "Seconds without edits after which this Emacs releases an item's lease.
Another Emacs can then edit the item without a hand-over."
  :type 'natnum
  :group 'niwa)

(defconst niwa-artifact-lease--hand-over-quiet-seconds 10
  "Seconds without an edit before a held item is handed to a requester.
An editor sends each edit as soon as its previous save is answered, so
a person drawing or typing touches the item well within this; one who
paused this long has most likely stopped.  Without it, an empty queue at
the renewal tick counted as idle, and two Emacs instances editing at once
passed the lease back and forth every renewal.")

(defvar niwa-artifact-lease--held (make-hash-table :test #'equal)
  "Leases this Emacs holds, by lease directory.
Each value is a plist with `:workspace', `:id', `:record', `:bytes' (the
record's bytes on the target), `:holding' (a token new with each held
generation), `:timer', `:touched' (local time of the last edit),
`:renewed' (local time the record was last written) and `:writing'
while a fenced target program is in progress.")

(defun niwa-artifact-lease-directory (workspace id)
  "Return the lease directory of WORKSPACE's store item ID."
  (file-name-concat (niwa-workspace-state-dir workspace)
                    "leases" "artifacts" id))

(defun niwa-artifact-lease--request-path (directory)
  "Return the hand-over request file in lease DIRECTORY."
  (file-name-concat directory "request"))

(defun niwa-artifact-lease--label (id)
  "Return the holder label written into ID's lease records."
  (format "Artifact %s" id))

(defun niwa-artifact-lease--now (directory)
  "Return the target clock for lease DIRECTORY."
  (niwa-session-durability--target-time directory))

(defun niwa-artifact-lease--live-p (record now)
  "Return non-nil when RECORD is an unexpired active lease at NOW."
  (and (eq 'active (plist-get record :status))
       (> (plist-get record :expires-at) now)))

(defun niwa-artifact-lease--own-p (record)
  "Return non-nil when RECORD was written by this Emacs."
  (equal niwa-session-durability--client-id (plist-get record :client-id)))

(defun niwa-artifact-lease--holder (record)
  "Return a readable name for RECORD's holder."
  (or (plist-get record :host) "another Emacs"))

(defun niwa-artifact-lease--request (directory now)
  "Ask the holder of lease DIRECTORY to hand it to this Emacs when idle.
NOW, the target clock, dates the request: one nobody repeated within a
lease period is ignored, so a requester who walked away does not take
the item from someone still editing it."
  (ignore-errors
    (niwa-session-control-fs-write-file
     (niwa-artifact-lease--request-path directory)
     (niwa-session-durability--record-bytes
      (list :client-id niwa-session-durability--client-id
            :host (niwa-session-durability--client-host)
            :at now)))))

(defun niwa-artifact-lease--hold (workspace id directory record)
  "Remember RECORD as this Emacs's lease on WORKSPACE's ID in DIRECTORY."
  (let ((held (gethash directory niwa-artifact-lease--held)))
    (unless held
      (setq held (list :workspace workspace :id id :diagnostic-scope (niwa-diagnostic-scope workspace)
                       :timer (run-at-time niwa-session-lease-renewal-seconds
                                           niwa-session-lease-renewal-seconds
                                           #'niwa-artifact-lease--renew directory)))
      (puthash directory held niwa-artifact-lease--held))
    (unless (eql (plist-get (plist-get held :record) :generation)
                 (plist-get record :generation))
      (plist-put held :holding (list (plist-get record :generation))))
    (plist-put held :record record)
    (plist-put held :bytes (niwa-session-durability--record-bytes record))
    (plist-put held :renewed (float-time))
    (plist-put held :touched (float-time))
    record))

(defun niwa-artifact-lease--forget (directory)
  "Stop renewing the lease in DIRECTORY and forget it."
  (when-let* ((held (gethash directory niwa-artifact-lease--held)))
    (when (timerp (plist-get held :timer))
      (cancel-timer (plist-get held :timer)))
    (remhash directory niwa-artifact-lease--held)))

(defun niwa-artifact-lease--observe (directory)
  "Observe lease DIRECTORY's clock and listing in one target program.
A missing directory is created first."
  (let ((observed (niwa-session-durability--observe-lease directory nil)))
    (if (plist-get observed :listed)
        observed
      (niwa-session-control-fs-make-directory directory t)
      (niwa-session-durability--observe-lease directory nil))))

(defun niwa-artifact-lease--claim (directory head id &optional now)
  "Claim lease DIRECTORY after HEAD for item ID, or return nil.
NOW, the target clock when already observed, dates the new record."
  (let* ((generation (1+ (or (plist-get head :generation) 0)))
         (record (niwa-session-durability--lease-record
                  (niwa-artifact-lease--label id) generation 'active nil nil
                  (or now (niwa-artifact-lease--now directory))))
         (previous (and head (niwa-session-durability--generation-path
                              directory (plist-get head :generation))))
         (results
          (niwa-session-control-fs-run-program
           (append
            (when head
              (list (list :op 'verify :path previous
                          :content (niwa-session-durability--record-bytes head))
                    (list :op 'verify-latest :path previous :content ".el")))
            (list (list :op 'create
                        :path (niwa-session-durability--generation-path directory generation)
                        :content (niwa-session-durability--record-bytes record)))
            (when previous (list (list :op 'delete-file :path previous :optional t))))
           ;; This parent survives deleting an item and its lease directory.
           (file-name-directory directory))))
    (if-let* ((failed (cl-find-if-not
                       (lambda (result) (eq 'ok (plist-get result :status)))
                       (if head (cl-subseq results 0 3) results))))
        (unless (memq (plist-get failed :status) '(conflict mismatch absent))
          (niwa-session-control-fs-program-value failed))
      record)))

(defun niwa-artifact-lease-acquire (workspace id)
  "Make this Emacs the holder of WORKSPACE's item ID, or signal why not.
A live foreign holder is asked to hand the item over when idle; one that
stopped renewing, such as a suspended laptop, is taken over."
  (let ((directory (niwa-artifact-lease-directory workspace id)))
    (niwa-session-durability-with-transaction
      (let* ((observed (niwa-artifact-lease--observe directory))
             (now (let ((niwa-session-durability--observed-time
                         (plist-get observed :now)))
                    (niwa-artifact-lease--now directory)))
             (head (niwa-session-durability--lease-head
                    directory (niwa-session-durability--observed-names observed)))
             (claim (lambda () (niwa-artifact-lease--claim directory head id now)))
             (record
              (cond
               ((null head) (funcall claim))
               ((not (niwa-session-durability--valid-lease-p head))
                (error "Invalid artifact lease: %s" directory))
               ((niwa-artifact-lease--live-p head now)
                (unless (niwa-artifact-lease--own-p head)
                  (niwa-artifact-lease--request directory now)
                  (error "%s is being edited in Emacs on %s; it was asked to hand it over, try again shortly"
                         id (niwa-artifact-lease--holder head)))
                ;; Our own record from before a restart of this lease's
                ;; bookkeeping: claim a fresh generation.
                (funcall claim))
               ((eq 'released (plist-get head :status))
                (let ((to (plist-get head :released-to)))
                  ;; Reserved for the requester for one lease period.
                  (when (and to (not (equal to niwa-session-durability--client-id))
                             (> (+ (plist-get head :expires-at) niwa-session-lease-seconds)
                                now))
                    (error "%s is being handed to another Emacs" id)))
                (funcall claim))
               ((niwa-artifact-lease--own-p head) (funcall claim))
               (t
                (message "niwa: took over editing %s from %s, whose lease expired"
                         id (niwa-artifact-lease--holder head))
                (funcall claim)))))
        (unless record
          (error "Another Emacs claimed %s at the same time; try again" id))
        ;; A request left by an earlier holder's requester is answered now.
        (when (member "request" (plist-get observed :names))
          (ignore-errors (niwa-session-control-fs-delete-file
                          (niwa-artifact-lease--request-path directory))))
        (niwa-artifact-lease--hold workspace id directory record)))))

(defun niwa-artifact-lease-ensure (workspace id)
  "Return non-nil once this Emacs holds WORKSPACE's item ID for an edit.
A held lease is reused without reading the target clock until its renewal
margin has passed locally since it was last written; then the clock
decides.  Otherwise it is acquired as `niwa-artifact-lease-acquire'
does."
  (let* ((directory (niwa-artifact-lease-directory workspace id))
         (held (gethash directory niwa-artifact-lease--held)))
    (if (and held
             (or (< (- (float-time) (plist-get held :renewed))
                    (- niwa-session-lease-seconds
                       niwa-session-lease-renewal-seconds))
                 (> (plist-get (plist-get held :record) :expires-at)
                    (niwa-artifact-lease--now directory))))
        (plist-put held :touched (float-time))
      (niwa-artifact-lease-acquire workspace id))))

(defun niwa-artifact-lease-held (workspace id)
  "Return a token for this Emacs's lease on WORKSPACE's item ID, or nil.
The token is `eq' only to itself: a lease lost, released or deleted with
its item and then acquired again yields a new one, even where the
generation number repeats.  A lease not renewed for a whole lease period,
as across a suspend, may have expired and been taken over unnoticed, so
it yields nil until `niwa-artifact-lease-ensure' or renewal settles it."
  (when-let* ((held (gethash (niwa-artifact-lease-directory workspace id)
                             niwa-artifact-lease--held))
              ((< (- (float-time) (plist-get held :renewed))
                  niwa-session-lease-seconds)))
    (plist-get held :holding)))

(defun niwa-artifact-lease-run (workspace id operations)
  "Run target OPERATIONS while this Emacs owns WORKSPACE's item ID.
The ownership and expiry proofs share a target lock with every lease change,
so a takeover cannot interleave with the operations.  Return their results."
  (let* ((directory (niwa-artifact-lease-directory workspace id))
         (held (or (gethash directory niwa-artifact-lease--held)
                   (error "This Emacs does not hold %s" id)))
         (record (plist-get held :record))
         (path (niwa-session-durability--generation-path
                directory (plist-get record :generation)))
         (results
          (let ((writing (plist-get held :writing)))
            ;; Waiting for the target can run local heartbeat timers.
            (plist-put held :writing t)
            (unwind-protect
                (niwa-session-control-fs-run-program
                 (append
                  (list (list :op 'verify :path path :content (plist-get held :bytes))
                        (list :op 'verify-latest :path path :content ".el")
                        (list :op 'before-time :path directory
                              :content (number-to-string (plist-get record :expires-at))))
                  operations)
                 (file-name-directory directory))
              (plist-put held :writing writing)))))
    (when-let* ((failed (cl-find-if-not
                         (lambda (result) (eq 'ok (plist-get result :status)))
                         (cl-subseq results 0 3))))
      (niwa-artifact-lease--forget directory)
      (if (memq (plist-get failed :status) '(mismatch absent))
          (error "The editing lease on %s ended; this edit was not saved" id)
        (niwa-session-control-fs-program-value failed)))
    (nthcdr 3 results)))

(defun niwa-artifact-lease-write (workspace id path content)
  "Write CONTENT to PATH while this Emacs still holds item ID's lease."
  (niwa-session-control-fs-program-value
   (car (niwa-artifact-lease-run
         workspace id (list (list :op 'write :path path :content content)))))
  t)

(defun niwa-artifact-lease-release (workspace id &optional to)
  "Release this Emacs's lease on WORKSPACE's item ID.
TO, a client id, reserves the item for that client for one lease period."
  (let* ((directory (niwa-artifact-lease-directory workspace id))
         (held (gethash directory niwa-artifact-lease--held)))
    (niwa-artifact-lease--forget directory)
    (when held
      (let* ((now (niwa-artifact-lease--now directory))
             (record (copy-sequence (plist-get held :record))))
        (setq record (plist-put record :status 'released))
        (setq record (plist-put record :renewed-at (min now (plist-get record :expires-at))))
        (setq record (plist-put record :expires-at (plist-get record :renewed-at)))
        (when to (setq record (plist-put record :released-to to)))
        (let* ((path (niwa-session-durability--generation-path
                      directory (plist-get record :generation)))
               (results (niwa-session-control-fs-run-program
                         (list (list :op 'verify :path path :content (plist-get held :bytes))
                               (list :op 'verify-latest :path path :content ".el")
                               (list :op 'write :path path
                                     :content (niwa-session-durability--record-bytes record)))
                         (file-name-directory directory))))
          (if-let* ((failed (cl-find-if-not
                            (lambda (result) (eq 'ok (plist-get result :status))) results)))
              (unless (memq (plist-get failed :status) '(mismatch absent))
                (niwa-session-control-fs-program-value failed))
            t))))))

(defun niwa-artifact-lease--renew (directory)
  "Renew, hand over or release this Emacs's lease in DIRECTORY."
  (when-let* ((held (gethash directory niwa-artifact-lease--held)))
    (unless (or (plist-get held :writing) (niwa-transport-busy-p directory))
      (condition-case err
          (let* ((workspace (plist-get held :workspace))
                 (id (plist-get held :id))
                 (busy (niwa-shared-editing-item-busy-p workspace id))
                 (request (and (not busy)
                               (> (- (float-time) (plist-get held :touched))
                                  niwa-artifact-lease--hand-over-quiet-seconds)
                               (niwa-session-durability--read-plist
                                (niwa-artifact-lease--request-path directory))))
                 (request
                  ;; Compared with the target time of the last renewal.
                  (if (and request (numberp (plist-get request :at))
                           (> (+ (plist-get request :at) niwa-session-lease-seconds)
                              (plist-get (plist-get held :record) :renewed-at)))
                      request
                    (when request
                      (ignore-errors (niwa-session-control-fs-delete-file
                                      (niwa-artifact-lease--request-path directory))))
                    nil)))
            (cond
             ((and request (stringp (plist-get request :client-id))
                   (not (equal (plist-get request :client-id)
                               niwa-session-durability--client-id)))
              (ignore-errors (niwa-session-control-fs-delete-file
                              (niwa-artifact-lease--request-path directory)))
              (when (niwa-artifact-lease-release workspace id (plist-get request :client-id))
                (message "niwa: handed %s over to %s" id
                         (or (plist-get request :host) "another Emacs"))))
             ((and (not busy)
                   (> (- (float-time) (plist-get held :touched))
                      niwa-artifact-lease-idle-seconds))
              (niwa-artifact-lease-release workspace id))
             (t
              (niwa-session-durability-with-transaction
                (let* ((now (niwa-artifact-lease--now directory))
                       (record (copy-sequence (plist-get held :record))))
                  (setq record (plist-put record :renewed-at now))
                  (setq record (plist-put record :expires-at
                                          (+ now niwa-session-lease-seconds)))
                  (if (let* ((path (niwa-session-durability--generation-path
                                   directory (plist-get record :generation)))
                             (results
                              (niwa-session-control-fs-run-program
                               (list (list :op 'verify :path path :content (plist-get held :bytes))
                                     (list :op 'verify-latest :path path :content ".el")
                                     (list :op 'write :path path
                                           :content (niwa-session-durability--record-bytes record)))
                               (file-name-directory directory))))
                        (cl-every (lambda (result) (eq 'ok (plist-get result :status))) results))
                      (progn
                        (plist-put held :record record)
                        (plist-put held :bytes
                                   (niwa-session-durability--record-bytes record))
                        (plist-put held :renewed (float-time)))
                    (niwa-artifact-lease--forget directory)
                    (niwa-diagnostic-message (or (plist-get held :diagnostic-scope) (niwa-diagnostic-scope workspace)) "Lost the editing lease on %s" id)))))))
        (error
         (niwa-artifact-lease--forget directory)
         (niwa-diagnostic-message (or (plist-get held :diagnostic-scope) (niwa-diagnostic-scope (plist-get held :workspace))) "Lost the editing lease on %s: %s"
                  (plist-get held :id) (error-message-string err)))))))

(defun niwa-artifact-lease-forget-item (workspace id)
  "Drop WORKSPACE's item ID's lease directory after the item is deleted."
  (let ((directory (niwa-artifact-lease-directory workspace id)))
    (unwind-protect
        (when (gethash directory niwa-artifact-lease--held)
          (niwa-session-control-fs-program-value
           (car (niwa-artifact-lease-run
                 workspace id (list (list :op 'delete-directory :path directory))))))
      (niwa-artifact-lease--forget directory))))

(defun niwa-artifact-lease-release-all ()
  "Release every item lease this Emacs holds, best effort."
  (let (held)
    (maphash (lambda (_directory value) (push value held))
             niwa-artifact-lease--held)
    (dolist (value held)
      (ignore-errors
        (niwa-artifact-lease-release
         (plist-get value :workspace) (plist-get value :id))))))

(add-hook 'kill-emacs-hook #'niwa-artifact-lease-release-all)

(provide 'niwa-artifact-lease)
;;; niwa-artifact-lease.el ends here
