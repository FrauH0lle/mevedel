;;; mevedel-journal-recovery.el -- Recover abandoned frozen captures -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace activation recovers frozen completed work without resuming its
;; conversation.  Journal admission serializes job changes.  Source authority
;; is acquired only while repairing a pin and sealing an abandoned checkpoint;
;; inference belongs to the existing digest processor after recovery returns.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-process)
(require 'mevedel-session-persistence)
(require 'mevedel-session-durability)

(defun mevedel-journal-recovery--seal (workspace capture)
  "Repair and seal abandoned CAPTURE in WORKSPACE under source authority."
  (let* ((metadata (plist-get capture :metadata))
         (id (plist-get capture :id))
         (source (mevedel-journal-capture--source-directory workspace capture))
         (mode (mevedel-session-codec-authority-mode-for-path source))
         (session-id (plist-get metadata :session))
         (operation
          (lambda ()
            (unless (eq (not (null (plist-get capture :head))) (eq mode 'portable))
              (error "Capture publication does not match its source authority"))
            (let* ((publication (and (eq mode 'portable)
                                     (mevedel-session-publication-read source (plist-get capture :head))))
                   (sidecar (mevedel-session-codec-read
                             (if (eq mode 'portable)
                                 (or (plist-get publication :sidecar)
                                     (error "Captured session publication is unavailable"))
                               (mevedel-session-artifacts-sidecar-path source)))))
              (unless (and (eq mode (plist-get sidecar :authority-mode))
                           (equal session-id (plist-get sidecar :session-id)))
                (error "Capture does not belong to its source session"))
              (mevedel-journal-capture-evidence workspace capture)
              (mevedel-journal-capture--ready workspace capture source)
              (mevedel-journal-capture--write-seal workspace id 'session-end)
              (mevedel-journal-capture-trigger workspace capture)))))
    (unless (mevedel-session-persistence-find-live-buffer
             session-id (mevedel-session-buffer-name (plist-get metadata :session-name) workspace))
      (if (eq mode 'portable)
          ;; This owner has no conversation, request, tools, or registry entry.
          ;; It exists only for the existing bounded lease reservation API.
          (mevedel-session-durability-call-with-abandoned-lease
           (mevedel-session--create :workspace workspace :authority-mode mode
                                    :session-id session-id :save-path source)
           operation)
        (mevedel-session-persistence-call-with-abandoned-lock source operation)))))

(defun mevedel-journal-recovery--retire-superseded (workspace capture entries)
  "Finish CAPTURE's accepted supersession in WORKSPACE using ENTRIES as proof.
Verify the successor's retained coverage before releasing an interrupted pin.
Delete the old raw bundle only after pin release; retain the retirement marker."
  (let* ((id (plist-get capture :id))
         (retired (mevedel-session-control-fs-read-file
                   (mevedel-journal-capture--file workspace id "retired") 'utf-8-unix 257)))
    (when (and (not (plist-get capture :unreadable))
               (string-match (concat "\\`superseded by \\(" mevedel-journal-store-hash-regexp "\\)\n\\'")
                             retired))
      (let* ((successor-id (match-string 1 retired))
             (successor (mevedel-journal-capture--read workspace successor-id))
             (metadata (or (plist-get successor :metadata)
                           (mevedel-journal-store-entry-for-capture entries successor-id)))
             (old (plist-get capture :metadata)))
        (unless (and metadata
                     (equal (plist-get metadata :session) (plist-get old :session))
                     (cl-every (lambda (turn) (member turn (plist-get metadata :turn-ids)))
                               (plist-get old :turn-ids))
                     (or (null successor)
                         (and (equal (mevedel-journal-capture--source-directory workspace capture)
                                     (mevedel-journal-capture--source-directory workspace successor))
                              (mevedel-journal-capture--marked-p workspace successor-id "ready"))))
          (error "Superseding journal coverage is unavailable"))
        (mevedel-journal-capture--retire workspace capture retired)
        t))))

(defun mevedel-journal-recovery-run (workspace)
  "Recover WORKSPACE's frozen captures without inference or interactive prompts.
Return per-capture recovery results.  Unavailable sources and damaged records
remain pending; one failure does not prevent unrelated captures from recovering."
  (when mevedel-journal-enabled
    (mevedel-journal-process--with-admission
     workspace
     (lambda (_claim)
       (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
             results)
              (dolist (capture (mevedel-journal-capture-list workspace t))
                (let ((id (plist-get capture :id)))
                  (push
                   (condition-case err
                       (list :id id :status
                             (cond
                              ((mevedel-journal-process--recover workspace capture entries) 'completed)
                              ((mevedel-journal-capture--marked-p workspace id "retired")
                               (mevedel-journal-recovery--retire-superseded workspace capture entries)
                               'retired)
                              ((plist-get capture :unreadable) (error "%s" (plist-get capture :error)))
                              ((mevedel-journal-capture-trigger workspace capture) 'sealed)
                              ((mevedel-journal-recovery--seal workspace capture) 'sealed)
                              (t 'held)))
                     (error (list :id id :status 'unavailable :error (error-message-string err))))
                   results)))
         (nreverse results))))))

(provide 'mevedel-journal-recovery)
;;; mevedel-journal-recovery.el ends here
