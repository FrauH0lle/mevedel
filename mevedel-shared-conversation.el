;;; mevedel-shared-conversation.el --- Shared-item conversation context -*- lexical-binding: t -*-

;;; Commentary:

;; Selects shared-item turns from the canonical session transcript.  The
;; transcript and its archived segments remain the only conversation store.
;; Request copies isolate item history while room history stays retrievable.

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-transcript)

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-segment
                  "mevedel-session-artifacts" (session number))
(autoload 'mevedel-session-artifacts-read-segment "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

(defconst mevedel-shared-conversation--history-limit 128000
  "Maximum characters of previous item turns included in a request.")

(defun mevedel-shared-conversation-ranges ()
  "Return trusted shared-item turn ranges in the current transcript.
Each range carries :start, :end and the host-authored :shared attribution.
:last-user identifies attribution to the final canonical user prompt.
A turn ends before the next canonical user prompt or directive boundary."
  ;; Most requests have no item attribution. Avoid classifying their transcript.
  (when-let* ((attributions
               (cl-remove-if-not
                (lambda (entry)
                  (let ((shared (plist-get (cdr entry) :shared)))
                    (and (stringp (plist-get shared :itemId))
                         (stringp (plist-get shared :questionId)))))
                (mevedel-transcript-audit-guest-prompts))))
    (let ((users (cl-remove-if-not
                  (lambda (segment) (eq (car segment) 'user))
                  (mevedel-transcript-segments (point-min) (point-max))))
          (directives (mevedel-transcript-buffer-directive-ranges t))
          owner ranges)
      ;; All three lists are ordered. Advance each cursor once instead of
      ;; searching the complete transcript again for every attribution.
      (dolist (attribution attributions)
        (while (and users (<= (cadr (car users)) (car attribution)))
          (setq owner (cadr (pop users))))
        (when owner
          (while (and directives (<= (plist-get (car directives) :start) owner))
            (pop directives))
          (push (list :start owner
                      :end (min (or (cadr (car users)) (point-max))
                                (or (plist-get (car directives) :start) (point-max)))
                      :last-user (null users)
                      :shared (plist-get (cdr attribution) :shared))
                ranges)))
      (nreverse ranges))))

(cl-defun mevedel-shared-conversation-history
    (session item-id &key live-buffer limit exclude-question)
  "Return prior canonical turns for ITEM-ID in SESSION, newest first.
LIVE-BUFFER supplies live content; nil selects archived segments only.
Repeated question identities in preserved compaction tails appear once.
LIMIT bounds complete-turn characters; nil requests exhaustive history.
EXCLUDE-QUESTION omits one identity, including its archived duplicates.
Return :turns carrying :text and :shared, and :truncated when LIMIT omitted
an older turn. Stop reading archives once that omission is established."
  (let ((seen (make-hash-table :test #'equal))
        (remaining limit) entries truncated)
    (when exclude-question (puthash exclude-question t seen))
    (catch 'full
      (cl-labels
          ((collect ()
             (dolist (range (reverse (mevedel-shared-conversation-ranges)))
               (let* ((shared (plist-get range :shared))
                      (id (plist-get shared :questionId)))
                 (when (and (equal item-id (plist-get shared :itemId))
                            (not (gethash id seen)))
                   (puthash id t seen)
                   (let ((size (- (plist-get range :end) (plist-get range :start))))
                     (when (and limit (> size remaining))
                       (setq truncated t)
                       (throw 'full nil))
                     (when limit (cl-decf remaining size))
                     (push (list :shared shared
                                 :text (buffer-substring (plist-get range :start)
                                                         (plist-get range :end)))
                           entries)))))))
        (when (buffer-live-p live-buffer)
          (with-current-buffer live-buffer (collect)))
        (cl-loop for number downfrom (1- (or (mevedel-session-current-segment session) 1))
                 to 1 do
                 (let ((buffer (mevedel-session-artifacts-read-segment session number)))
                   (unwind-protect
                       (with-current-buffer buffer (collect))
                     (kill-buffer buffer))))))
    (list :turns (nreverse entries) :truncated truncated)))

(defun mevedel-shared-conversation-transform (fsm)
  "Isolate shared-item context in FSM's temporary request buffer.
Ordinary room requests exclude item turns.  Item requests retain the current
question and recent same-item history, including archived segments.  This
changes neither the stored transcript nor the request's editing permissions."
  (when-let* ((source (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p source))
              (session (buffer-local-value 'mevedel--session source))
              ((eq source (mevedel-session-root-buffer session))))
    (let* ((ranges (mevedel-shared-conversation-ranges))
           (current (cl-find-if (lambda (range) (plist-get range :last-user)) ranges)))
      (plist-put (gptel-fsm-info fsm) :mevedel-shared-item nil)
      (if (not current)
          (dolist (range ranges)
            (add-text-properties (plist-get range :start) (plist-get range :end)
                                 '(gptel ignore)))
        (let* ((shared (plist-get current :shared))
               (id (plist-get shared :itemId))
               (question-id (plist-get shared :questionId))
               (question (buffer-substring (plist-get current :start) (point-max)))
               (history (mevedel-shared-conversation-history
                         session id :live-buffer source
                         :limit mevedel-shared-conversation--history-limit
                         :exclude-question question-id))
               (selected (mapcar (lambda (turn) (plist-get turn :text))
                                 (reverse (plist-get history :turns)))))
          (erase-buffer)
          (insert (format "Conversation about shared item %s (%s).\n"
                          (plist-get shared :title) id)
                  "Earlier snapshots below are historical; the current question carries its reviewed content.\n"
                  "Read/Grep history://root for relevant room decisions; history://saved includes archived conversations.\n"
                  (if (plist-get history :truncated)
                      "Older item turns were omitted from this request; retrieve them from session history when needed.\n"
                    "")
                  "\n")
          ;; Archived turns are canonical Org, unlike this already prepared
          ;; request. Use gptel's normal projection (including retained provider
          ;; fragments) before inserting them into the request copy.
          (when selected
            (insert
             (gptel--with-buffer-copy (current-buffer) nil nil
               (unwind-protect
                   (progn
                     (setq-local mevedel--session session)
                     (dolist (text selected) (insert text "\n"))
                     (let ((prepared (gptel--create-prompt-buffer (point-max))))
                       (unwind-protect
                           (with-current-buffer prepared (buffer-string))
                         (kill-buffer prepared))))
                 (kill-buffer (current-buffer))))))
          (insert question)
          (plist-put (gptel-fsm-info fsm) :mevedel-shared-item id))))))

(provide 'mevedel-shared-conversation)
;;; mevedel-shared-conversation.el ends here
