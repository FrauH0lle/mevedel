;;; niwa-permission-queue-test-support.el --- Real permission queue fixture -*- lexical-binding: t -*-

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
(require 'niwa-structs)
(require 'niwa-permission-queue)
(require 'niwa-permission-prompt)
(require 'niwa-view)
(require 'niwa-view-interaction)

(defvar niwa--session)
(defvar niwa-permission-mode)
(defvar niwa--prompt-overlays)
(defvar niwa-view--interaction-overlays)

(defun niwa-pq-test--request (entry &optional session)
  "Run the real permission queue entry point for ENTRY in SESSION."
  (niwa-permission-request entry session))

(defun niwa-pq-test--make-session (&optional rules mode)
  "Return a fresh fixture session with permission RULES and MODE."
  (niwa-session--create
   :name "test" :workspace nil :permission-rules rules
   :permission-mode (or mode 'ask) :permission-queue nil
   :pending-plan-approval nil))

(defun niwa-pq-test--requested-p (session)
  "Return non-nil when SESSION's permission log records a queue admission.
Reads the log the real queue wrote, so it needs `niwa-permission-log-enabled'.
It tells a caller that must not prompt apart from one that reached the queue."
  (cl-some (lambda (event) (eq 'permission-enqueued (plist-get event :event)))
           (niwa-session-permission-log-pending session)))

(defun niwa-pq-test--forget-requests (session)
  "Clear SESSION's pending permission log so the next probe starts fresh."
  (setf (niwa-session-permission-log-pending session) nil))

(defmacro niwa-pq-test--with-queue (vars &rest body)
  "Run BODY with the real queue, a session and a live view.
VARS is (SESSION DATA VIEW), bound to the session and its buffers.  Optional
keywords before BODY are `:rules' and `:mode', evaluated for the session."
  (declare (indent 1) (debug t))
  (let (rules mode)
    (while (keywordp (car body))
      (pcase (pop body)
        (:rules (setq rules (pop body)))
        (:mode (setq mode (pop body)))))
    `(niwa-view-test--with-buffers
       (let ((,(nth 0 vars) (niwa-pq-test--make-session ,rules ,mode))
             (,(nth 1 vars) data-buf)
             (,(nth 2 vars) view-buf))
         (with-current-buffer data-buf
           (setq-local niwa--session ,(nth 0 vars)))
         (setf (niwa-session-root-buffer ,(nth 0 vars)) data-buf)
         (with-current-buffer data-buf
           ,@body)))))

(defmacro niwa-pq-test--with-view-here (view &rest body)
  "Run BODY with a live view, bound to VIEW, for the current buffer.
For a test that owns its data buffer and session and needs the real card the
queue renders there.  The view is killed afterwards."
  (declare (indent 1) (debug t))
  `(let ((,view (generate-new-buffer " *pq-test-view*")))
     (unwind-protect
         (progn
           (niwa-view--setup ,view (current-buffer))
           (cl-letf (((symbol-function 'niwa-recovery-save) #'ignore)
                     ((symbol-function 'niwa-view--position-screen-y)
                      (if noninteractive
                          #'niwa-view-test--position-screen-y
                        (symbol-function 'niwa-view--position-screen-y))))
             ,@body))
       (when (buffer-live-p ,view) (kill-buffer ,view)))))

(defmacro niwa-pq-test--with-session-view (session view &rest body)
  "Run BODY in a data buffer for SESSION with a live view bound to VIEW.
The permission log is on, so `niwa-pq-test--requested-p' works, and
whatever BODY leaves queued is aborted afterwards."
  (declare (indent 2) (debug t))
  `(let ((niwa-permission-log-enabled t))
     (with-temp-buffer
       (setq-local niwa--session ,session)
       (niwa-pq-test--with-view-here ,view
         (unwind-protect (progn ,@body)
           (niwa-permission-queue-abort-all ,session))))))

(defun niwa-pq-test--card-text (overlay)
  "Return the displayed text of the card owning OVERLAY."
  (with-current-buffer (overlay-buffer overlay)
    (let ((pos (point-min)) text)
      (while (setq pos (text-property-any
                        pos (point-max)
                        'niwa-view-interaction-overlay overlay))
        (let ((end (or (text-property-not-all
                        pos (point-max)
                        'niwa-view-interaction-overlay overlay)
                       (point-max))))
          (push (buffer-substring-no-properties pos end) text)
          (setq pos end)))
      (apply #'concat (nreverse text)))))

(defun niwa-pq-test--cards (view)
  "Return detached snapshots of the permission cards rendered in VIEW.
Each snapshot is a plist of :id, :overlay, :callback (the closure the card
captured), :entry (a copy of the card's entry), :count and :text."
  (with-current-buffer view
    (let (cards)
      (when (hash-table-p niwa-view--interaction-overlays)
        (maphash
         (lambda (id overlay)
           (when (and (overlayp overlay) (overlay-buffer overlay)
                      (overlay-get overlay 'niwa-permission-prompt))
             (push (list :id id :overlay overlay
                         :callback (overlay-get overlay 'niwa--callback)
                         :entry (copy-tree
                                 (overlay-get
                                  overlay 'niwa-view-interaction-entry))
                         :count (plist-get
                                 (gethash id niwa-view--interaction-descriptors)
                                 :count)
                         :text (niwa-pq-test--card-text overlay))
                   cards)))
         niwa-view--interaction-overlays))
      cards)))

(defun niwa-pq-test--card (view)
  "Return the single rendered card in VIEW, failing when there is not one."
  (let ((cards (niwa-pq-test--cards view)))
    (unless (= 1 (length cards))
      (ert-fail (format "expected exactly one rendered card, found %d"
                        (length cards))))
    (car cards)))

(defun niwa-pq-test--press (card key)
  "Answer CARD by pressing KEY (a `kbd' string) on its rendered text."
  (let ((overlay (plist-get card :overlay)))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (ert-fail "the card is no longer rendered; use a captured answer"))
    (with-current-buffer (overlay-buffer overlay)
      (let ((pos (text-property-any (point-min) (point-max)
                                    'niwa-view-interaction-overlay overlay)))
        (unless pos (ert-fail "the card has no rendered text"))
        (goto-char pos)
        (let* ((keys (kbd key))
               (command (key-binding keys)))
          (unless (commandp command)
            (ert-fail (format "the card binds no command to %s" key)))
          (let ((last-command-event (aref keys 0)))
            (call-interactively command)))))))

(defun niwa-pq-test--answer-captured (card outcome)
  "Answer CARD with OUTCOME through the closure it captured when rendered."
  (funcall (plist-get card :callback) outcome))

;;
;;; Scripted person

;; A caller test that wants a particular answer cannot wait for a person.  The
;; scripted person below stands where one would: it watches the real card
;; renderer and, once a card is on screen, presses keys on that card.  The queue,
;; the card and the key bindings stay real; only the choice of key is scripted.

(defvar niwa-pq-test--driven nil
  "Snapshots of the cards the scripted person has answered, newest first.")

(defvar niwa-pq-test--advice nil
  "The advice the scripted person installed.")

(defvar niwa-pq-test--last-session nil
  "The session of the latest `niwa-pq-test--answering' form.")

(defvar niwa-pq-test--failure nil
  "First error a scripted step raised, re-signalled by the answering form.")

(defvar niwa-pq-test--driving nil
  "Non-nil while the scripted person is pressing keys.")

(defconst niwa-pq-test--outcome-keys
  '((allow-once . "a") (allow-session . "s") (always-allow . "A")
    (deny-once . "d") (deny-session . "D"))
  "Key that a card binds for each outcome it can offer.")

(defun niwa-pq-test--admitted-p ()
  "Return non-nil when the latest answering form's session saw a request."
  (and niwa-pq-test--last-session
       (niwa-pq-test--requested-p niwa-pq-test--last-session)))

(defun niwa-pq-test--prompts ()
  "Return the entries of the cards shown so far, oldest first."
  (mapcar (lambda (card) (plist-get card :entry))
          (reverse niwa-pq-test--driven)))

(defun niwa-pq-test--key-offered-p (card key)
  "Return non-nil when CARD's own text binds KEY to a permission command."
  (let ((overlay (plist-get card :overlay)))
    (with-current-buffer (overlay-buffer overlay)
      (save-excursion
        (goto-char (text-property-any (point-min) (point-max)
                                      'niwa-view-interaction-overlay overlay))
        (let ((command (key-binding (kbd key))))
          (and (symbolp command)
               (string-prefix-p "niwa-permission--prompt-"
                                (symbol-name command))))))))

(defun niwa-pq-test--step (card step)
  "Apply STEP to CARD.
A string is a key pressed on the card.  A function receives the card.  An
outcome is pressed with its key and fails when the card does not offer it;
\(inject OUTCOME) deliberately calls the closure the card captured, for
stale or repeated answers; a feedback answer is typed."
  (cond
   ((functionp step) (funcall step card))
   ((eq 'inject (car-safe step))
    (niwa-pq-test--answer-captured card (cadr step)))
   ((stringp step) (niwa-pq-test--press card step))
   ((eq 'feedback (car-safe step))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (cdr step))))
      (niwa-pq-test--press card "f")))
   (t (let ((key (cdr (assq step niwa-pq-test--outcome-keys))))
        (cond
         ((null key)
          (ert-fail (format "unknown scripted outcome %S; use (inject OUTCOME)" step)))
         ((not (niwa-pq-test--key-offered-p card key))
          (ert-fail (format "the card offers no key for %S (%s)" step key)))
         (t (niwa-pq-test--press card key)))))))

(defun niwa-pq-test--drive (script view)
  "Answer every card in VIEW that SCRIPT has not yet answered.
SCRIPT is a list of steps, or a function of the card's 1-based number and its
snapshot returning that list."
  (let ((niwa-pq-test--driving t) card)
    (while (setq card
                 (cl-find-if
                  (lambda (candidate)
                    (not (cl-find (plist-get candidate :overlay)
                                  niwa-pq-test--driven
                                  :key (lambda (done) (plist-get done :overlay)))))
                  (niwa-pq-test--cards view)))
      (push card niwa-pq-test--driven)
      (let ((steps (if (functionp script)
                       (funcall script (length niwa-pq-test--driven) card)
                     script)))
        (dolist (step steps)
          ;; A step can settle the card; later steps need a live one.
          (when (or (functionp step)
                    (overlay-buffer (plist-get card :overlay)))
            (setq card (or (cl-find (plist-get card :id)
                                    (niwa-pq-test--cards view)
                                    :key (lambda (c) (plist-get c :id))
                                    :test #'equal)
                           card))
            ;; The renderer's caller swallows errors, so keep the first one
            ;; and re-signal it when the answering form ends.
            (condition-case err
                (niwa-pq-test--step card step)
              (error (unless niwa-pq-test--failure
                       (setq niwa-pq-test--failure err))))))))))

(defun niwa-pq-test--call-answering (script view thunk)
  "Call THUNK with the scripted person answering SCRIPT in VIEW's cards."
  (let ((niwa-pq-test--driven nil) (niwa-pq-test--failure nil))
    (advice-add 'niwa-permission-prompt-render :after
                (let ((advice (lambda (&rest _)
                                (unless niwa-pq-test--driving
                                  (niwa-pq-test--drive script view)))))
                  (setq niwa-pq-test--advice advice) advice))
    (unwind-protect (prog1 (funcall thunk)
                      (when niwa-pq-test--failure
                        (signal (car niwa-pq-test--failure)
                                (cdr niwa-pq-test--failure))))
      (advice-remove 'niwa-permission-prompt-render niwa-pq-test--advice))))

(defmacro niwa-pq-test--answering (spec &rest body)
  "Run BODY with the real queue and a scripted person answering its cards.
SPEC is (STEPS [:session SESSION] [:here BOOL] [:mode MODE]) with STEPS a list
of steps for every card, or a function of the card number and snapshot.  The
queue is real and so is its view; the session is SESSION, else a fresh one whose
mode is `niwa-permission-mode'.  With :here the current buffer is the data
buffer; otherwise a temporary one.  BODY reads the cards answered through
`niwa-pq-test--prompts'.  What it leaves queued is dropped unanswered afterwards."
  (declare (indent 1) (debug t))
  (let ((steps (car spec))
        (session (plist-get (cdr spec) :session))
        (mode (plist-get (cdr spec) :mode))
        (here (plist-get (cdr spec) :here)))
    `(let* ((pq--session (or ,session
                             (and (bound-and-true-p niwa--session)
                                  (niwa-session-p niwa--session)
                                  niwa--session)
                             (niwa-pq-test--make-session
                              nil (or ,mode niwa-permission-mode 'ask))))
            (niwa-permission-log-enabled t))
       (setq niwa-pq-test--last-session pq--session)
       (,(if here 'progn 'with-temp-buffer)
        (setq-local niwa--session pq--session)
        (niwa-pq-test--with-view-here pq--view
          (unwind-protect
              (niwa-pq-test--call-answering ,steps pq--view (lambda () ,@body))
            ;; A card nobody answered is dropped without settling it, so
            ;; assertions after the form still see it unanswered.
            (setf (niwa-session-permission-queue pq--session) nil)))))))

(provide 'niwa-permission-queue-test-support)
;;; niwa-permission-queue-test-support.el ends here
