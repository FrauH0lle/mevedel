;;; niwa-journal-recovery.el -- Recover abandoned frozen captures -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace activation recovers frozen completed work without resuming its
;; conversation.  Journal admission serializes job changes.  Source authority
;; is acquired only while repairing a pin and sealing an abandoned checkpoint;
;; inference belongs to the existing digest processor after recovery returns.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-journal-process)
(require 'niwa-session-persistence)
(require 'niwa-session-durability)

(defun niwa-journal-recovery--seal (workspace capture)
  "Repair and seal abandoned CAPTURE in WORKSPACE under source authority."
  (let* ((metadata (plist-get capture :metadata))
         (id (plist-get capture :id))
         (source (niwa-journal-capture--source-directory workspace capture))
         (mode (niwa-session-codec-authority-mode-for-path source))
         (session-id (plist-get metadata :session))
         (operation
          (lambda ()
            (unless (plist-get capture :head)
              (error "Capture publication does not match its source authority"))
            (let* ((publication (niwa-session-publication-read
                                 source (plist-get capture :head)))
                   (sidecar (niwa-session-codec-read
                             (or (plist-get publication :sidecar)
                                 (error "Captured session publication is unavailable")))))
              (unless (and (eq mode (plist-get sidecar :authority-mode))
                           (equal session-id (plist-get sidecar :session-id)))
                (error "Capture does not belong to its source session"))
              (niwa-journal-capture-evidence workspace capture)
              (niwa-journal-capture--ready workspace capture source)
              (niwa-journal-capture--write-seal workspace id 'session-end)
              (niwa-journal-capture-trigger workspace capture)))))
    (unless (niwa-session-persistence-find-live-buffer
             session-id (niwa-session-buffer-name (plist-get metadata :session-name) workspace))
      ;; This owner has no conversation, request, tools, or registry entry.
      ;; It exists only for the bounded lease reservation API.
      (niwa-session-durability-call-with-abandoned-lease
       (niwa-session--create :workspace workspace :authority-mode mode
                                :session-id session-id :save-path source)
       operation))))

(defun niwa-journal-recovery--retire-superseded (workspace capture entries)
  "Finish CAPTURE's accepted supersession in WORKSPACE using ENTRIES as proof.
Verify the successor's retained coverage before releasing an interrupted pin.
Delete the old raw bundle only after pin release; retain the retirement marker."
  (let* ((id (plist-get capture :id))
         (retired (niwa-session-control-fs-read-file
                   (niwa-journal-capture--file workspace id "retired") 'utf-8-unix 257)))
    (when (and (not (plist-get capture :unreadable))
               (string-match (concat "\\`superseded by \\(" niwa-journal-store-hash-regexp "\\)\n\\'")
                             retired))
      (let* ((successor-id (match-string 1 retired))
             (successor (niwa-journal-capture--read workspace successor-id))
             (metadata (or (plist-get successor :metadata)
                           (niwa-journal-store-entry-for-capture entries successor-id)))
             (old (plist-get capture :metadata)))
        (unless (and metadata
                     (equal (plist-get metadata :session) (plist-get old :session))
                     (cl-every (lambda (turn) (member turn (plist-get metadata :turn-ids)))
                               (plist-get old :turn-ids))
                     (or (null successor)
                         (and (equal (niwa-journal-capture--source-directory workspace capture)
                                     (niwa-journal-capture--source-directory workspace successor))
                              (niwa-journal-capture--marked-p workspace successor-id "ready"))))
          (error "Superseding journal coverage is unavailable"))
        (niwa-journal-capture--retire workspace capture retired)
        t))))

(defun niwa-journal-recovery-run (workspace)
  "Recover WORKSPACE's frozen captures without inference or interactive prompts.
Return per-capture recovery results.  Unavailable sources and damaged records
remain pending; one failure does not prevent unrelated captures from recovering."
  (when niwa-journal-enabled
    (niwa-journal-process--with-admission
     workspace
     (lambda (_claim)
       (let ((entries (niwa-journal-store-entries (niwa-workspace-root workspace)))
             results)
              (dolist (capture (niwa-journal-capture-list workspace t))
                (let ((id (plist-get capture :id)))
                  (push
                   (condition-case err
                       (list :id id :status
                             (cond
                              ((niwa-journal-process--recover workspace capture entries) 'completed)
                              ((niwa-journal-capture--marked-p workspace id "retired")
                               (niwa-journal-recovery--retire-superseded workspace capture entries)
                               'retired)
                              ((plist-get capture :unreadable) (error "%s" (plist-get capture :error)))
                              ((niwa-journal-capture-trigger workspace capture) 'sealed)
                              ((niwa-journal-recovery--seal workspace capture) 'sealed)
                              (t 'held)))
                     (error (list :id id :status 'unavailable :error (error-message-string err))))
                   results)))
         (nreverse results))))))

(defun niwa-journal-recovery-child (workspace _payload)
  "Recover abandoned WORKSPACE captures without editor-side scheduling."
  (list :ok t :count (length (niwa-journal-recovery-run workspace))
        :more nil :cleanup-at (niwa-workspace-journal-cleanup-at workspace)))

(provide 'niwa-journal-recovery)
;;; niwa-journal-recovery.el ends here
