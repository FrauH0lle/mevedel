;;; mevedel-artifact-lease.el --- Cross-Emacs leases on store items -*- lexical-binding: t -*-

;;; Commentary:

;; A whiteboard or document in the workspace artifact store is one live
;; object, so only one Emacs may commit it at a time.  This module gives each
;; item a lease with the session lease's lifecycle: generations elected by
;; exclusive creation on the target, target-clock deadlines, heartbeat
;; renewal, release after a quiet period, cooperative hand-over and a takeover
;; after expiry that only a person in Emacs confirms.  It reuses the session durability generation
;; primitives on its own directory; no session state is involved.
;;
;; Leases are machine state, so they live outside the store, under
;; `.mevedel/leases/artifacts/ID/': the store itself stays safe to commit.
;; An Emacs that does not hold an item's lease sees it read-only; trying to
;; edit asks the holder to hand it over when it is idle.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-durability)
(require 'mevedel-transport)
(require 'mevedel-workspace)

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-item-busy-p "mevedel-shared-editing" (workspace id))

(defcustom mevedel-artifact-lease-idle-seconds 300
  "Seconds without edits after which this Emacs releases an item's lease.
Another Emacs can then edit the item without a hand-over."
  :type 'natnum
  :group 'mevedel)

(defconst mevedel-artifact-lease--hand-over-quiet-seconds 10
  "Seconds without an edit before a held item is handed to a requester.
An editor saves pending changes every 300 ms, so a person drawing or typing
touches the item well within this; one who paused this long has most
likely stopped.  Without it, an empty queue at the renewal tick counted as
idle, and two Emacs instances editing at once passed the lease back and
forth every renewal.")

(defvar mevedel-artifact-lease--held (make-hash-table :test #'equal)
  "Leases this Emacs holds, by lease directory.
Each value is a plist with `:workspace', `:id', `:record', `:bytes' (the
record's bytes on the target), `:timer', `:touched' (local time of the
last edit) and `:renewed' (local time the record was last written).")

(defun mevedel-artifact-lease-directory (workspace id)
  "Return the lease directory of WORKSPACE's store item ID."
  (file-name-concat (mevedel-workspace-state-dir workspace)
                    "leases" "artifacts" id))

(defun mevedel-artifact-lease--request-path (directory)
  "Return the hand-over request file in lease DIRECTORY."
  (file-name-concat directory "request.el"))

(defun mevedel-artifact-lease--label (id)
  "Return the holder label written into ID's lease records."
  (format "Artifact %s" id))

(defun mevedel-artifact-lease--now (directory)
  "Return the target clock for lease DIRECTORY."
  (mevedel-session-durability--target-time directory))

(defun mevedel-artifact-lease--live-p (record now)
  "Return non-nil when RECORD is an unexpired active lease at NOW."
  (and (eq 'active (plist-get record :status))
       (> (plist-get record :expires-at) now)))

(defun mevedel-artifact-lease--own-p (record)
  "Return non-nil when RECORD was written by this Emacs."
  (equal mevedel-session-durability--client-id (plist-get record :client-id)))

(defun mevedel-artifact-lease--holder (record)
  "Return a readable name for RECORD's holder."
  (or (plist-get record :host) "another Emacs"))

(defun mevedel-artifact-lease--request (directory)
  "Ask the holder of lease DIRECTORY to hand it to this Emacs when idle."
  (ignore-errors
    (mevedel-session-control-fs-write-file
     (mevedel-artifact-lease--request-path directory)
     (mevedel-session-durability--record-bytes
      (list :client-id mevedel-session-durability--client-id
            :host (mevedel-session-durability--client-host))))))

(defun mevedel-artifact-lease--hold (workspace id directory record)
  "Remember RECORD as this Emacs's lease on WORKSPACE's ID in DIRECTORY."
  (let ((held (gethash directory mevedel-artifact-lease--held)))
    (unless held
      (setq held (list :workspace workspace :id id
                       :timer (run-at-time mevedel-session-lease-renewal-seconds
                                           mevedel-session-lease-renewal-seconds
                                           #'mevedel-artifact-lease--renew directory)))
      (puthash directory held mevedel-artifact-lease--held))
    (plist-put held :record record)
    (plist-put held :bytes (mevedel-session-durability--record-bytes record))
    (plist-put held :renewed (float-time))
    (plist-put held :touched (float-time))
    record))

(defun mevedel-artifact-lease--forget (directory)
  "Stop renewing the lease in DIRECTORY and forget it."
  (when-let* ((held (gethash directory mevedel-artifact-lease--held)))
    (when (timerp (plist-get held :timer))
      (cancel-timer (plist-get held :timer)))
    (remhash directory mevedel-artifact-lease--held)))

(defun mevedel-artifact-lease--observe (directory)
  "Observe lease DIRECTORY's clock and listing in one target program.
A missing directory is created first."
  (let ((observed (mevedel-session-durability--observe-lease directory nil)))
    (if (plist-get observed :listed)
        observed
      (mevedel-session-control-fs-make-directory directory t)
      (mevedel-session-durability--observe-lease directory nil))))

(defun mevedel-artifact-lease-acquire (workspace id &optional ask)
  "Make this Emacs the holder of WORKSPACE's item ID, or signal why not.
A live foreign holder is asked to hand the item over when idle.  One that
stopped renewing is taken over only when ASK is non-nil and the user
confirms; otherwise this refuses, naming the cockpit command that asks."
  (let ((directory (mevedel-artifact-lease-directory workspace id)))
    (mevedel-session-durability-with-transaction
      (let* ((observed (mevedel-artifact-lease--observe directory))
             (now (let ((mevedel-session-durability--observed-time
                         (plist-get observed :now)))
                    (mevedel-artifact-lease--now directory)))
             (head (mevedel-session-durability--lease-head
                    directory (mevedel-session-durability--observed-names observed)))
             (claim (lambda ()
                      (mevedel-session-durability--claim-next
                       directory head (mevedel-artifact-lease--label id)
                       nil nil nil nil observed)))
             (record
              (cond
               ((null head) (funcall claim))
               ((not (mevedel-session-durability--valid-lease-p head))
                (error "Invalid artifact lease: %s" directory))
               ((mevedel-artifact-lease--live-p head now)
                (unless (mevedel-artifact-lease--own-p head)
                  (mevedel-artifact-lease--request directory)
                  (error "%s is being edited in Emacs on %s; it was asked to hand it over, try again shortly"
                         id (mevedel-artifact-lease--holder head)))
                ;; Our own record from before a restart of this lease's
                ;; bookkeeping: claim a fresh generation.
                (funcall claim))
               ((eq 'released (plist-get head :status))
                (let ((to (plist-get head :released-to)))
                  ;; Reserved for the requester for one lease period.
                  (when (and to (not (equal to mevedel-session-durability--client-id))
                             (> (+ (plist-get head :expires-at) mevedel-session-lease-seconds)
                                now))
                    (error "%s is being handed to another Emacs" id)))
                (funcall claim))
               ((or (mevedel-artifact-lease--own-p head)
                    (and ask
                         (y-or-n-p (format "Take over editing %s from %s, whose lease expired? "
                                           id (mevedel-artifact-lease--holder head)))))
                (funcall claim))
               (ask (user-error "%s stays with %s" id (mevedel-artifact-lease--holder head)))
               (t (user-error "This needs a decision in Emacs on the host first: %s's editor on %s stopped renewing; take it over with T in the artifacts cockpit"
                              id (mevedel-artifact-lease--holder head))))))
        (unless record
          (error "Another Emacs claimed %s at the same time; try again" id))
        ;; A request left by an earlier holder's requester is answered now.
        (when (member "request.el" (plist-get observed :names))
          (ignore-errors (mevedel-session-control-fs-delete-file
                          (mevedel-artifact-lease--request-path directory))))
        (mevedel-artifact-lease--hold workspace id directory record)))))

(defun mevedel-artifact-lease-ensure (workspace id &optional ask)
  "Return non-nil once this Emacs holds WORKSPACE's item ID for an edit.
A held lease is reused without reading the target clock until its renewal
margin has passed locally since it was last written; then the clock
decides.  Otherwise it is acquired as `mevedel-artifact-lease-acquire'
does with ASK."
  (let* ((directory (mevedel-artifact-lease-directory workspace id))
         (held (gethash directory mevedel-artifact-lease--held)))
    (if (and held
             (or (< (- (float-time) (plist-get held :renewed))
                    (- mevedel-session-lease-seconds
                       mevedel-session-lease-renewal-seconds))
                 (> (plist-get (plist-get held :record) :expires-at)
                    (mevedel-artifact-lease--now directory))))
        (plist-put held :touched (float-time))
      (mevedel-artifact-lease-acquire workspace id ask))))

(defun mevedel-artifact-lease-held-p (workspace id)
  "Return non-nil while this Emacs holds WORKSPACE's item ID."
  (and (gethash (mevedel-artifact-lease-directory workspace id)
                mevedel-artifact-lease--held)
       t))

(defun mevedel-artifact-lease-write (workspace id path content)
  "Write CONTENT to PATH while this Emacs still holds item ID's lease.
The ownership proof and the write are one target program, so a client that
lost the lease cannot overwrite its successor's state."
  (let* ((directory (mevedel-artifact-lease-directory workspace id))
         (held (or (gethash directory mevedel-artifact-lease--held)
                   (error "This Emacs does not hold %s" id)))
         (results
          (mevedel-session-control-fs-run-program
           (list (list :op 'verify
                       :path (mevedel-session-durability--generation-path
                              directory (plist-get (plist-get held :record) :generation))
                       :content (plist-get held :bytes))
                 (list :op 'write :path path :content content)))))
    (unless (eq 'ok (plist-get (car results) :status))
      (mevedel-artifact-lease--forget directory)
      (error "Another Emacs took over %s; this edit was not saved" id))
    (mevedel-session-control-fs-program-value (cadr results))
    t))

(defun mevedel-artifact-lease-release (workspace id &optional to)
  "Release this Emacs's lease on WORKSPACE's item ID.
TO, a client id, reserves the item for that client for one lease period."
  (let* ((directory (mevedel-artifact-lease-directory workspace id))
         (held (gethash directory mevedel-artifact-lease--held)))
    (mevedel-artifact-lease--forget directory)
    (when held
      (let* ((now (mevedel-artifact-lease--now directory))
             (record (copy-sequence (plist-get held :record))))
        (setq record (plist-put record :status 'released))
        (setq record (plist-put record :renewed-at (min now (plist-get record :expires-at))))
        (setq record (plist-put record :expires-at (plist-get record :renewed-at)))
        (when to (setq record (plist-put record :released-to to)))
        (mevedel-session-durability--commit-lease
         directory (plist-get record :generation) (plist-get held :bytes) record)))))

(defun mevedel-artifact-lease--renew (directory)
  "Renew, hand over or release this Emacs's lease in DIRECTORY."
  (when-let* ((held (gethash directory mevedel-artifact-lease--held)))
    (unless (mevedel-transport-busy-p directory)
      (condition-case err
          (let* ((workspace (plist-get held :workspace))
                 (id (plist-get held :id))
                 (busy (mevedel-shared-editing-item-busy-p workspace id))
                 (request (and (not busy)
                               (> (- (float-time) (plist-get held :touched))
                                  mevedel-artifact-lease--hand-over-quiet-seconds)
                               (mevedel-session-durability--read-plist
                                (mevedel-artifact-lease--request-path directory)))))
            (cond
             ((and request (stringp (plist-get request :client-id))
                   (not (equal (plist-get request :client-id)
                               mevedel-session-durability--client-id)))
              (ignore-errors (mevedel-session-control-fs-delete-file
                              (mevedel-artifact-lease--request-path directory)))
              (mevedel-artifact-lease-release workspace id (plist-get request :client-id))
              (message "mevedel: handed %s over to %s" id
                       (or (plist-get request :host) "another Emacs")))
             ((and (not busy)
                   (> (- (float-time) (plist-get held :touched))
                      mevedel-artifact-lease-idle-seconds))
              (mevedel-artifact-lease-release workspace id))
             (t
              (mevedel-session-durability-with-transaction
                (let* ((now (mevedel-artifact-lease--now directory))
                       (record (copy-sequence (plist-get held :record))))
                  (setq record (plist-put record :renewed-at now))
                  (setq record (plist-put record :expires-at
                                          (+ now mevedel-session-lease-seconds)))
                  (if (mevedel-session-durability--commit-lease
                       directory (plist-get record :generation)
                       (plist-get held :bytes) record)
                      (progn
                        (plist-put held :record record)
                        (plist-put held :bytes
                                   (mevedel-session-durability--record-bytes record))
                        (plist-put held :renewed (float-time)))
                    (mevedel-artifact-lease--forget directory)
                    (message "mevedel: lost the editing lease on %s" id)))))))
        (error
         (mevedel-artifact-lease--forget directory)
         (message "mevedel: lost the editing lease on %s: %s"
                  (plist-get held :id) (error-message-string err)))))))

(defun mevedel-artifact-lease-forget-item (workspace id)
  "Drop WORKSPACE's item ID's lease directory after the item is deleted."
  (let ((directory (mevedel-artifact-lease-directory workspace id)))
    (mevedel-artifact-lease--forget directory)
    (ignore-errors (mevedel-session-control-fs-delete-directory directory))))

(defun mevedel-artifact-lease-release-all ()
  "Release every item lease this Emacs holds, best effort."
  (let (held)
    (maphash (lambda (_directory value) (push value held))
             mevedel-artifact-lease--held)
    (dolist (value held)
      (ignore-errors
        (mevedel-artifact-lease-release
         (plist-get value :workspace) (plist-get value :id))))))

(add-hook 'kill-emacs-hook #'mevedel-artifact-lease-release-all)

(provide 'mevedel-artifact-lease)
;;; mevedel-artifact-lease.el ends here
