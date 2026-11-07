;;; mevedel-permission-queue-test-support.el --- Real permission queue fixture -*- lexical-binding: t -*-

;;; Commentary:

;; Test-only fixture that runs the real permission queue in a session with a
;; live view and answers through the real rendered card.  Callers and the
;; decision table share it, so no test replaces the queue's entry point.
;;
;; A person answers a card by pressing its key at point on the card's own text.
;; A delayed or repeated answer uses the closure the card captured when it was
;; rendered; this file never looks up the head again, fabricates a closure or
;; settles an entry itself.  A card snapshot holds detached data only.
;;
;; Loading this file defines no test cases.

;;; Code:

(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-permission-queue)
(require 'mevedel-permission-prompt)
(require 'mevedel-view)
(require 'mevedel-view-interaction)

(defvar mevedel--session)
(defvar mevedel--prompt-overlays)
(defvar mevedel-view--interaction-overlays)

(defun mevedel-pq-test--request (entry &optional session)
  "Run the real permission queue entry point for ENTRY in SESSION."
  (mevedel-permission-request entry session))

(defun mevedel-pq-test--make-session (&optional rules mode)
  "Return a fresh fixture session with permission RULES and MODE."
  (mevedel-session--create
   :name "test" :workspace nil :permission-rules rules
   :permission-mode (or mode 'ask) :permission-queue nil
   :pending-plan-approval nil))

(defun mevedel-pq-test--requested-p (session)
  "Return non-nil when SESSION's permission log records a queue admission.
Reads the log the real queue wrote, so it needs `mevedel-permission-log-enabled'.
It tells a caller that must not prompt apart from one that reached the queue."
  (cl-some (lambda (event) (eq 'permission-enqueued (plist-get event :event)))
           (mevedel-session-permission-log-pending session)))

(defun mevedel-pq-test--forget-requests (session)
  "Clear SESSION's pending permission log so the next probe starts fresh."
  (setf (mevedel-session-permission-log-pending session) nil))

(defmacro mevedel-pq-test--with-queue (vars &rest body)
  "Run BODY with the real queue, a session and a live view.
VARS is (SESSION DATA VIEW), bound to the session and its buffers.  Optional
keywords before BODY are `:rules' and `:mode', evaluated for the session."
  (declare (indent 1) (debug t))
  (let (rules mode)
    (while (keywordp (car body))
      (pcase (pop body)
        (:rules (setq rules (pop body)))
        (:mode (setq mode (pop body)))))
    `(mevedel-view-test--with-buffers
       (let ((,(nth 0 vars) (mevedel-pq-test--make-session ,rules ,mode))
             (,(nth 1 vars) data-buf)
             (,(nth 2 vars) view-buf))
         (with-current-buffer data-buf
           (setq-local mevedel--session ,(nth 0 vars)))
         (setf (mevedel-session-root-buffer ,(nth 0 vars)) data-buf)
         (with-current-buffer data-buf
           ,@body)))))

(defmacro mevedel-pq-test--with-view-here (view &rest body)
  "Run BODY with a live view, bound to VIEW, for the current buffer.
For a test that owns its data buffer and session and needs the real card the
queue renders there.  The view is killed afterwards."
  (declare (indent 1) (debug t))
  `(let ((,view (generate-new-buffer " *pq-test-view*")))
     (unwind-protect
         (progn
           (mevedel-view--setup ,view (current-buffer))
           (cl-letf (((symbol-function 'mevedel-recovery-save) #'ignore)
                     ((symbol-function 'mevedel-view--position-screen-y)
                      (if noninteractive
                          #'mevedel-view-test--position-screen-y
                        (symbol-function 'mevedel-view--position-screen-y))))
             ,@body))
       (when (buffer-live-p ,view) (kill-buffer ,view)))))

(defmacro mevedel-pq-test--with-session-view (session view &rest body)
  "Run BODY in a data buffer for SESSION with a live view bound to VIEW.
The permission log is on, so `mevedel-pq-test--requested-p' works, and
whatever BODY leaves queued is aborted afterwards."
  (declare (indent 2) (debug t))
  `(let ((mevedel-permission-log-enabled t))
     (with-temp-buffer
       (setq-local mevedel--session ,session)
       (mevedel-pq-test--with-view-here ,view
         (unwind-protect (progn ,@body)
           (mevedel-permission-queue-abort-all ,session))))))

(defun mevedel-pq-test--card-text (overlay)
  "Return the displayed text of the card owning OVERLAY."
  (with-current-buffer (overlay-buffer overlay)
    (let ((pos (point-min)) text)
      (while (setq pos (text-property-any
                        pos (point-max)
                        'mevedel-view-interaction-overlay overlay))
        (let ((end (or (text-property-not-all
                        pos (point-max)
                        'mevedel-view-interaction-overlay overlay)
                       (point-max))))
          (push (buffer-substring-no-properties pos end) text)
          (setq pos end)))
      (apply #'concat (nreverse text)))))

(defun mevedel-pq-test--cards (view)
  "Return detached snapshots of the permission cards rendered in VIEW.
Each snapshot is a plist of :id, :overlay, :callback (the closure the card
captured), :entry (a copy of the card's entry), :count and :text."
  (with-current-buffer view
    (let (cards)
      (when (hash-table-p mevedel-view--interaction-overlays)
        (maphash
         (lambda (id overlay)
           (when (and (overlayp overlay) (overlay-buffer overlay)
                      (overlay-get overlay 'mevedel-permission-prompt))
             (push (list :id id :overlay overlay
                         :callback (overlay-get overlay 'mevedel--callback)
                         :entry (copy-tree
                                 (overlay-get
                                  overlay 'mevedel-view-interaction-entry))
                         :count (plist-get
                                 (gethash id mevedel-view--interaction-descriptors)
                                 :count)
                         :text (mevedel-pq-test--card-text overlay))
                   cards)))
         mevedel-view--interaction-overlays))
      cards)))

(defun mevedel-pq-test--card (view)
  "Return the single rendered card in VIEW, failing when there is not one."
  (let ((cards (mevedel-pq-test--cards view)))
    (unless (= 1 (length cards))
      (ert-fail (format "expected exactly one rendered card, found %d"
                        (length cards))))
    (car cards)))

(defun mevedel-pq-test--press (card key)
  "Answer CARD by pressing KEY (a `kbd' string) on its rendered text."
  (let ((overlay (plist-get card :overlay)))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (ert-fail "the card is no longer rendered; use a captured answer"))
    (with-current-buffer (overlay-buffer overlay)
      (let ((pos (text-property-any (point-min) (point-max)
                                    'mevedel-view-interaction-overlay overlay)))
        (unless pos (ert-fail "the card has no rendered text"))
        (goto-char pos)
        (let* ((keys (kbd key))
               (command (key-binding keys)))
          (unless (commandp command)
            (ert-fail (format "the card binds no command to %s" key)))
          (let ((last-command-event (aref keys 0)))
            (call-interactively command)))))))

(defun mevedel-pq-test--answer-captured (card outcome)
  "Answer CARD with OUTCOME through the closure it captured when rendered."
  (funcall (plist-get card :callback) outcome))

(provide 'mevedel-permission-queue-test-support)
;;; mevedel-permission-queue-test-support.el ends here
