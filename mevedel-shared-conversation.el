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
A turn ends before the next canonical user prompt or directive boundary."
  (let* ((users (cl-remove-if-not
                 (lambda (segment) (eq (car segment) 'user))
                 (mevedel-transcript-segments (point-min) (point-max))))
         (directives (mevedel-transcript-buffer-directive-ranges t))
         ranges)
    (dolist (attribution (mevedel-transcript-audit-guest-prompts))
      (when-let* ((shared (plist-get (cdr attribution) :shared))
                  ((stringp (plist-get shared :itemId)))
                  ((stringp (plist-get shared :questionId))))
        (let (owner (end (point-max)))
          (dolist (user users)
            (if (<= (cadr user) (car attribution))
                (setq owner (cadr user))
              (setq end (min end (cadr user)))))
          (when owner
            (dolist (range directives)
              (when (> (plist-get range :start) owner)
                (setq end (min end (plist-get range :start)))))
            (push (list :start owner :end end :shared shared) ranges)))))
    (nreverse ranges)))

(defun mevedel-shared-conversation-history (session item-id &optional current-buffer)
  "Return prior canonical turns for ITEM-ID in SESSION, newest first.
CURRENT-BUFFER supplies live content; nil selects archived segments only.
Repeated question identities in preserved compaction tails appear once.
Returned entries carry :text and :shared; callers choose a context budget."
  (let ((seen (make-hash-table :test #'equal)) entries)
    (cl-labels
        ((collect ()
           (dolist (range (reverse (mevedel-shared-conversation-ranges)))
             (let* ((shared (plist-get range :shared))
                    (id (plist-get shared :questionId)))
               (when (and (equal item-id (plist-get shared :itemId))
                          (not (gethash id seen)))
                 (puthash id t seen)
                 (push (list :shared shared
                             :text (buffer-substring (plist-get range :start)
                                                     (plist-get range :end)))
                       entries))))))
      (when (buffer-live-p current-buffer)
        (with-current-buffer current-buffer (collect)))
      (cl-loop for number downfrom (1- (or (mevedel-session-current-segment session) 1))
               to 1 do
               (let ((buffer (mevedel-session-artifacts-read-segment session number)))
                 (unwind-protect
                     (with-current-buffer buffer (collect))
                   (kill-buffer buffer)))))
    (nreverse entries)))

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
           (last-user (car (last (cl-remove-if-not
                                 (lambda (segment) (eq (car segment) 'user))
                                 (mevedel-transcript-segments (point-min) (point-max))))))
           (current (cl-find (cadr last-user) ranges
                             :key (lambda (range) (plist-get range :start)))))
      (plist-put (gptel-fsm-info fsm) :mevedel-shared-item nil)
      (if (not current)
          (dolist (range ranges)
            (add-text-properties (plist-get range :start) (plist-get range :end)
                                 '(gptel ignore)))
        (let* ((shared (plist-get current :shared))
               (id (plist-get shared :itemId))
               (question-id (plist-get shared :questionId))
               (question (buffer-substring (plist-get current :start) (point-max)))
               (history (mevedel-shared-conversation-history session id source))
               (remaining mevedel-shared-conversation--history-limit)
               selected omitted)
          (dolist (turn history)
            (unless (equal question-id
                           (plist-get (plist-get turn :shared) :questionId))
              (let ((text (plist-get turn :text)))
                (if (and (not omitted) (<= (length text) remaining))
                    (progn (push text selected)
                           (cl-decf remaining (length text)))
                  (setq omitted t)))))
          (erase-buffer)
          (insert (format "Conversation about shared item %s (%s).\n"
                          (plist-get shared :title) id)
                  "Earlier snapshots below are historical; the current question carries its reviewed content.\n"
                  "Read/Grep history://root for relevant room decisions; history://saved includes archived conversations.\n"
                  (if omitted
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
