;;; mevedel-collaboration-agent.el --- browser agent sharing -*- lexical-binding: t; -*-

;;; Commentary:

;; Publishes the retained-agent roster and serves projected live agent
;; transcripts to collaboration guests.  Agent lookup is registry-only: a
;; browser poll never hydrates cold state or starts target I/O.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-agent-control'
(declare-function mevedel-agent-record-conversation-buffer
                  "mevedel-agent-control" (record))
(declare-function mevedel-agent-record-role "mevedel-agent-control" (record))
(declare-function mevedel-agent-record-settled-outcome
                  "mevedel-agent-control" (record))

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast
                  "mevedel-collaboration" (room frame))
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-session
                  "mevedel-collaboration" (session))
(declare-function mevedel-collaboration--schedule-publish
                  "mevedel-collaboration" (room))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))
(declare-function mevedel-collaboration--snapshot-chunks
                  "mevedel-collaboration-guest" (records &optional overhead))

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection" (data-buffer))
(declare-function mevedel-collaboration--json-string
                  "mevedel-collaboration-projection" (object))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-structs'
(declare-function mevedel-session-agent-registry "mevedel-structs" (session))

;; `mevedel-view-agent'
(declare-function mevedel-view--agent-record-status
                  "mevedel-view-agent" (record))

(defconst mevedel-collaboration--agent-fetch-window 1.0
  "Seconds within which repeated agent fetches from one guest are dropped.")

(defun mevedel-collaboration--agent-rows (room)
  "Return ROOM's guest-visible retained agent rows, sorted by path.
An active agent carries its live status; a settled one carries its
terminal outcome (done, errored, or interrupted), so its retained
transcript stays reachable from the viewer's finished-agents list."
  ;; ponytail: every retained agent travels in one frame; rows are a few
  ;; dozen fixed bytes each, so bound or paginate only if registries grow
  ;; to thousands.
  (when-let* ((session (plist-get room :session)))
    (let (rows)
      (dolist (pair (mevedel-session-agent-registry session))
        (let* ((record (cdr pair))
               (outcome (mevedel-agent-record-settled-outcome record))
               (status
                (or (mevedel-view--agent-record-status record)
                    (pcase outcome
                      ('completed 'done)
                      ((or 'errored 'interrupted) outcome)))))
          (when status
            (push (append
                   (list (cons "path" (car pair))
                         (cons "status" (symbol-name status)))
                   (when-let* ((role (mevedel-agent-record-role record)))
                     (list (cons "role" (format "%s" role)))))
                  rows))))
      (sort rows (lambda (left right)
                   (string-lessp (cdr (assoc "path" left))
                                 (cdr (assoc "path" right))))))))

(defun mevedel-collaboration--agents-frame (room)
  "Return ROOM's retained agent roster frame."
  (list :t "agents"
        :agents (vconcat (mevedel-collaboration--agent-rows room))))

(defun mevedel-collaboration--publish-agents (room)
  "Broadcast ROOM's retained agent roster when it has changed."
  (let ((frame (mevedel-collaboration--agents-frame room)))
    (unless (equal frame (plist-get room :agents))
      (plist-put room :agents frame)
      (mevedel-collaboration--broadcast room frame))))

(defun mevedel-collaboration-notify-agents-changed (session)
  "Schedule SESSION's publication after retained agent state changed."
  (when-let* ((room (mevedel-collaboration--room-for-session session)))
    (condition-case nil
        (mevedel-collaboration--schedule-publish room)
      (error (mevedel-collaboration--observer-failure room)))))

(defun mevedel-collaboration--agent-frame-overhead (req-id path)
  "Return encoded agent-frame overhead for REQ-ID and PATH."
  (string-bytes
   (mevedel-collaboration--json-string
    (list :t "agent" :reqId req-id :path path
          :digest (make-string 64 ?0) :records (vconcat nil)
          :final :json-false))))

(defun mevedel-collaboration--agent-conversation (room path)
  "Return the live conversation buffer for canonical PATH in ROOM, or nil."
  (when-let* ((session (plist-get room :session))
              ((stringp path))
              (entry (assoc path (mevedel-session-agent-registry session)))
              (buffer (mevedel-agent-record-conversation-buffer (cdr entry)))
              ((buffer-live-p buffer)))
    buffer))

(defun mevedel-collaboration--handle-fetch-agent (room peer frame)
  "Answer guest PEER's agent-transcript fetch FRAME for ROOM."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId))
        (path (plist-get frame :path))
        (transport (plist-get room :transport))
        (now (float-time)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      ;; ponytail: one budget per guest; split by path only if the viewer
      ;; gains concurrent agent panels.
      (let ((last (plist-get guest :last-agent-fetch)))
        (unless (and last (< (- now last)
                             mevedel-collaboration--agent-fetch-window))
          (plist-put guest :last-agent-fetch now)
          (if-let* ((buffer (mevedel-collaboration--agent-conversation
                             room path)))
              (let* ((records
                      (mapcar
                       (lambda (record)
                         (if (plist-get record :artifact)
                             (let ((copy (copy-sequence record)))
                               (plist-put
                                copy :id
                                (concat
                                 "agent-artifact-"
                                 (secure-hash
                                  'sha256
                                  (concat path "\0"
                                          (plist-get record :id))))))
                           record))
                       (mevedel-collaboration--canonical-records buffer)))
                     (chunks (or (mevedel-collaboration--snapshot-chunks
                                  records
                                  (mevedel-collaboration--agent-frame-overhead
                                   req-id path))
                                 (list nil)))
                     (digest (secure-hash
                              'sha256
                              (mapconcat #'mevedel-collaboration--json-string
                                         (apply #'append chunks) "\n"))))
                (plist-put guest :agent-artifacts
                           (cl-remove-if-not
                            (lambda (record) (plist-get record :artifact))
                            records))
                (if (equal digest (plist-get frame :known))
                    (mevedel-collaboration--transport-send
                     transport peer
                     (list :t "agent" :reqId req-id :path path
                           :digest digest :unchanged t))
                  (cl-loop for rest on chunks do
                           (mevedel-collaboration--transport-send
                            transport peer
                            (list :t "agent" :reqId req-id :path path
                                  :digest digest
                                  :records (vconcat (car rest))
                                  :final (if (cdr rest) :json-false t))))))
            (plist-put guest :agent-artifacts nil)
            (mevedel-collaboration--transport-send
             transport peer
             (list :t "agent" :reqId req-id
                   :error "This agent's transcript is not available"))))))))

(provide 'mevedel-collaboration-agent)
;;; mevedel-collaboration-agent.el ends here
