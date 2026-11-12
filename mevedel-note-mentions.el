;;; mevedel-note-mentions.el -- Explicit note context attachments -*- lexical-binding: t -*-

;;; Commentary:

;; Called only for explicit @note mentions.  Use the ordinary mention reminder
;; and deferred deduplication channel, without adding automatic note context.

;;; Code:

(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-note-resource)

;; `mevedel-mentions'
(declare-function mevedel-mentions-read-permission-context "mevedel-mentions" (info path))
(autoload 'mevedel-mentions-read-permission-context "mevedel-mentions")

;; `mevedel-permissions'
(declare-function mevedel-check-permission "mevedel-permissions" (tool-name &rest args))
(autoload 'mevedel-check-permission "mevedel-permissions")

(defun mevedel-note-mentions-resolve (identity workspace)
  "Look up numeric ID or UUID IDENTITY in WORKSPACE's loaded note snapshots.
Do not open source files merely to bind a token's identity."
  (when workspace
    (cl-find-if (lambda (entry)
                  (equal identity (if (integerp identity)
                                      (plist-get (plist-get entry :properties) 'mevedel-id)
                                    (plist-get entry :uuid))))
                (mevedel-notes--entries workspace))))

(defun mevedel-note-mentions--format (entry workspace)
  "Format ENTRY's current code and authored text as context in WORKSPACE."
  (when-let* ((code (mevedel-note-resource--code entry)))
    (let ((properties (plist-get entry :properties)))
      (format "#### Note #%d\n\nFile `%s`, line %d. Author: %s.\n\n%s\n\n%s"
              (plist-get properties 'mevedel-id)
              (file-relative-name (plist-get entry :file) (mevedel-workspace-root workspace))
              (plist-get code :line)
              (plist-get properties 'mevedel-note-author)
              (mevedel--markdown-enquote (or (plist-get properties 'mevedel-note-text) ""))
              (plist-get code :code)))))

(defun mevedel-note-mentions--matches-p (query tags)
  "Return whether parsed QUERY matches TAGS, without evaluating Lisp."
  (pcase query
    (`(and . ,operands)
     (cl-every (lambda (operand) (mevedel-note-mentions--matches-p operand tags)) operands))
    (`(or . ,operands)
     (cl-some (lambda (operand) (mevedel-note-mentions--matches-p operand tags)) operands))
    (`(not ,operand) (not (mevedel-note-mentions--matches-p operand tags)))
    ((pred symbolp) (and query (memq query tags)))
    (_ nil)))

(defun mevedel-note-mentions--query (text workspace)
  "Return open notes in WORKSPACE matching tag query TEXT.
Empty and malformed queries never select all notes."
  (when workspace
    (when-let* ((query (condition-case nil
                          (let* ((input (concat "(" text ")"))
                                 (read-result (read-from-string input)))
                            (and (= (cdr read-result) (length input))
                                 (mevedel--tag-query-prefix-from-infix (car read-result))))
                        (error nil))))
      (cl-remove-if-not
       (lambda (entry)
         (mevedel-note-mentions--matches-p
          query (plist-get (plist-get entry :properties) 'mevedel-note-tags)))
       (mevedel-notes--entries workspace)))))

(defun mevedel-note-mentions--attach (entry info workspace)
  "Return context for ENTRY in WORKSPACE, or an unavailable reason, using INFO.
Check Read authority before probing or restoring the source file."
  (let ((file (plist-get entry :file)))
    (condition-case err
        (cond
         ((not (eq 'allow (mevedel-check-permission
                           "Read" :normalized-context
                           (mevedel-mentions-read-permission-context info file))))
          (list :error "permission denied"))
         ((not (file-readable-p file)) (list :error "unreadable or missing source"))
         (t
          (if-let* ((body (mevedel-note-mentions--format entry workspace)))
              (list :body body)
            (list :error "unavailable"))))
      (file-error (list :error (format "unreadable source: %s" (error-message-string err)))))))

(defun mevedel-note-mentions-handle (info)
  "Expand an explicit note mention from the ordinary mention INFO plist."
  (let* ((id (nth 1 (plist-get info :captures)))
         (query (nth 2 (plist-get info :captures)))
         (binding (plist-get info :binding))
         (session (plist-get info :session))
         (workspace (and session (mevedel-session-workspace session)))
         (identity (or (plist-get binding :note-uuid)
                       (and id (string-to-number id))))
         (notes (if query
                    (mevedel-note-mentions--query query workspace)
                  (when-let* ((note (and identity (mevedel-note-mentions-resolve identity workspace))))
                    (list note))))
         (label (if query (format "{%s}" query) id))
         (key (if query (cons 'note-tag query)
                (cons 'note (if notes (plist-get (car notes) :uuid) identity)))))
    (if notes
        (let ((mevedel-note-resource--source-buffers (make-hash-table :test #'equal)))
          (unwind-protect
              (let (bodies failures)
                (dolist (entry notes)
                  (let ((result (mevedel-note-mentions--attach entry info workspace)))
                    (if-let* ((body (plist-get result :body)))
                        (push body bodies)
                      (push (format "Note #%d: %s"
                                    (plist-get (plist-get entry :properties) 'mevedel-id)
                                    (plist-get result :error)) failures))))
                (let* ((failure-text (string-join (nreverse failures) "; "))
                       (body (string-join (append (nreverse bodies)
                                                 (when failures
                                                   (list (concat failure-text
                                                                 ". The unavailable placeholders are system annotations, not user text.")))) "\n\n"))
                       (status (cond ((and bodies failures) "some contents attached; some unavailable")
                                     (bodies "contents attached above")
                                     (t failure-text))))
                  (list :placeholder (format "[note:%s -- %s]" label status)
                        :reminder body :key key
                        :hash (and (not failures) (secure-hash 'sha256 body))
                        :warning (and failures failure-text))))
            (maphash (lambda (_file buffer) (kill-buffer buffer))
                     mevedel-note-resource--source-buffers)))
      (let ((reason (if query "no matches" "unavailable")))
        (list :placeholder (format "[note:%s -- %s]" label reason)
              :reminder (format "The @note:%s mention has %s. The placeholder is a system annotation, not user text. Do not infer another note from its number." label reason)
              :key key
              :warning (format "note @note:%s: %s" label reason))))))

(provide 'mevedel-note-mentions)
;;; mevedel-note-mentions.el ends here
