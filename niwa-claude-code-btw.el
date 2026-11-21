;;; niwa-claude-code-btw.el --- Ephemeral native side conversations -*- lexical-binding: t -*-

;;; Commentary:
;; Fork settled native history for /btw.  The fork is never a durable niwa
;; session reference.  Each turn uses the ordinary ACP/MCP transport with the
;; side's frozen tools and its own settlement; normal root dispatch is bypassed.

;;; Code:

(require 'niwa-acp-turn)
(require 'niwa-claude-code)
(require 'niwa-claude-code-context)
(require 'niwa-claude-code-history)
(require 'niwa-tools)
(require 'niwa-turn)
(require 'niwa-utilities)

;; `niwa-side-conversation'
(defvar niwa-side-conversation--frozen-context)
(defvar niwa-side-conversation--frozen-request-denies)

;; `niwa-view-render'
(declare-function niwa-view--append-request-summary
                  "niwa-view-render" (data-buf search-start &optional extra))

(cl-defstruct (niwa-claude-code-btw--fork
               (:constructor niwa-claude-code-btw--make-fork))
  "One owned native fork, including late cleanup after its buffer dies."
  buffer launch id closed deleting pending error diagnostic-scope)

(defvar niwa-claude-code-btw--forks nil
  "Native side forks not yet deleted, including pending fork replies.")

(defvar-local niwa-claude-code-btw--fork nil
  "Native fork owned by this side data buffer.")
(put 'niwa-claude-code-btw--fork 'permanent-local t)

(defun niwa-claude-code-btw-parent-history ()
  "Return a snapshot of this buffer's settled native history, or refuse."
  (let ((history (alist-get "root" (niwa-session-external-conversations
                                    niwa--session) nil nil #'equal)))
    (niwa-claude-code-history-assert-current history)
    (unless (and (niwa-claude-code-backend-p gptel-backend)
                 (not (niwa-turn-busy-p))
                 (eq 'ready (plist-get history :state))
                 (plist-get history :id)
                 (not (plist-get history :start-failed)))
      (user-error "/btw needs settled, current Claude history; finish a parent turn first"))
    (copy-tree history)))

(defun niwa-claude-code-btw--delete (fork)
  "Delete FORK once its last writer has stopped, retaining failed cleanup."
  (let ((buffer (niwa-claude-code-btw--fork-buffer fork)))
    (when (and (niwa-claude-code-btw--fork-closed fork)
               (not (niwa-claude-code-btw--fork-pending fork))
               (not (niwa-claude-code-btw--fork-deleting fork))
               (not (and (buffer-live-p buffer)
                         (buffer-local-value 'niwa--current-request buffer))))
      (if (not (niwa-claude-code-btw--fork-id fork))
          (setq niwa-claude-code-btw--forks
                (delq fork niwa-claude-code-btw--forks))
        (setf (niwa-claude-code-btw--fork-deleting fork) t)
        (condition-case err
            (niwa-acp-session-operation
             (niwa-claude-code-btw--fork-launch fork) 'delete
             (niwa-claude-code-btw--fork-id fork)
             (lambda (outcome)
               (setf (niwa-claude-code-btw--fork-deleting fork) nil)
               (if (eq 'success (plist-get outcome :status))
                   (progn
                     (setf (niwa-claude-code-btw--fork-id fork) nil
                           (niwa-claude-code-btw--fork-error fork) nil)
                     (setq niwa-claude-code-btw--forks
                           (delq fork niwa-claude-code-btw--forks)))
                 (setf (niwa-claude-code-btw--fork-error fork)
                       (plist-get outcome :message))
                 (niwa-diagnostic-warning
                  (or (niwa-claude-code-btw--fork-diagnostic-scope fork) 'unavailable)
                  'niwa
                                  (format "Could not delete /btw native fork %s: %s"
                                          (niwa-claude-code-btw--fork-id fork)
                                          (plist-get outcome :message))))))
          (error
           (setf (niwa-claude-code-btw--fork-deleting fork) nil
                 (niwa-claude-code-btw--fork-error fork) (error-message-string err))
           (niwa-diagnostic-warning
            (or (niwa-claude-code-btw--fork-diagnostic-scope fork) 'unavailable)
            'niwa (error-message-string err))))))))

(defun niwa-claude-code-btw-close ()
  "Retire this buffer's native fork and delete it after cancellation settles."
  (when-let* ((fork niwa-claude-code-btw--fork))
    (setf (niwa-claude-code-btw--fork-closed fork) t)
    (when niwa--current-request
      (niwa-request-cancel niwa--current-request))
    (niwa-claude-code-btw--delete fork)))

(put 'niwa-claude-code-btw-close 'permanent-local-hook t)

(defun niwa-claude-code-btw-open (parent side history ready)
  "Fork PARENT's frozen HISTORY for SIDE, calling READY after admission.
Closing SIDE before the reply still deletes the returned fork.  If PARENT
changes during startup, refuse rather than silently capture newer history."
  (let* ((launch (with-current-buffer parent
                   (niwa-claude-code-launch
                    "Prepare ephemeral side conversation" []
                    (gptel--model-name gptel-model) nil)))
         (tick (buffer-chars-modified-tick parent))
         (fork (niwa-claude-code-btw--make-fork
                :buffer side :launch launch :pending t
                :diagnostic-scope (niwa-diagnostic-scope parent))))
    (push fork niwa-claude-code-btw--forks)
    (with-current-buffer side
      (setq-local niwa-claude-code-btw--fork fork)
      (add-hook 'kill-buffer-hook #'niwa-claude-code-btw-close nil t))
    (condition-case err
        (niwa-acp-session-operation
         launch 'fork (plist-get history :id)
         (lambda (outcome)
           (setf (niwa-claude-code-btw--fork-pending fork) nil
                 (niwa-claude-code-btw--fork-id fork) (plist-get outcome :session-id))
           (condition-case failure
               (progn
                 (unless (eq 'success (plist-get outcome :status))
                   (error "%s" (plist-get outcome :message)))
                 (unless (and (buffer-live-p parent) (buffer-live-p side)
                              (not (niwa-claude-code-btw--fork-closed fork)))
                   (error "The /btw owner closed while forking"))
                 (with-current-buffer parent
                   (unless (and (= tick (buffer-chars-modified-tick))
                                (equal history (niwa-claude-code-btw-parent-history)))
                     (error "Parent history changed while opening /btw; try again")))
                 (funcall ready))
             ((error quit)
              (setf (niwa-claude-code-btw--fork-closed fork) t)
              (when (buffer-live-p side) (kill-buffer side))
              (niwa-claude-code-btw--delete fork)
              (niwa-diagnostic-warning
               (niwa-claude-code-btw--fork-diagnostic-scope fork)
               'niwa (error-message-string failure))))))
      ((error quit)
       (setf (niwa-claude-code-btw--fork-pending fork) nil
             (niwa-claude-code-btw--fork-closed fork) t)
       (niwa-claude-code-btw--delete fork)
       (signal (car err) (cdr err))))))

(defun niwa-claude-code-btw--content (input model)
  "Expand side INPUT for MODEL, including frozen context and fresh mentions."
  (let* ((context niwa-side-conversation--frozen-context)
         (expansion (niwa-mentions-expand-user-input input niwa--session t model)))
    (vconcat
     (list `((type . "text")
             (text . ,(string-join
                       (delq nil (append (list (plist-get context :text)
                                               (plist-get expansion :text))
                                         (mapcar (lambda (item)
                                                   (niwa-reminders-format-block
                                                    (plist-get item :body)))
                                                 (plist-get expansion :reminder-items))))
                       "\n\n"))))
     (niwa-claude-code-context--images
      (append (mapcar (lambda (media)
                        (list (car media) (plist-get (cdr media) :mime)))
                      (plist-get context :media))
              (plist-get expansion :media-contexts)) model))))

(defun niwa-claude-code-btw-assert-open ()
  "Refuse a side whose native fork is not available for another turn."
  (let ((fork niwa-claude-code-btw--fork))
    (unless (and fork (niwa-claude-code-btw--fork-id fork)
                 (not (niwa-claude-code-btw--fork-closed fork)))
      (user-error "This native /btw fork has ended; close it and open a new side conversation"))))

(defun niwa-claude-code-btw-send (input policy)
  "Send side INPUT under frozen model POLICY without publishing native history."
  (let* ((fork niwa-claude-code-btw--fork)
         (buffer (current-buffer))
         (model (plist-get policy :model))
         (effort (plist-get policy :effort))
         (system gptel-system-prompt))
    (niwa-claude-code-btw-assert-open)
    (let ((request (niwa-request-begin niwa--session)))
      (condition-case err
          (progn
	    (setf (niwa-request-one-shot-mutations-p request) t
		  (niwa-request-ephemeral-p request) t
		  (niwa-request-skill-permission-rules request)
		  (copy-tree niwa-side-conversation--frozen-request-denies)
		  (niwa-engine-info request)
		  (append (niwa-engine-info request)
			  (list :backend (plist-get policy :backend) :model model
				:reasoning-effort effort :external-history t)))
	    (niwa-acp-turn-start
	     request
	     (lambda (mcp _hook-command)
               (let ((launch (niwa-claude-code-launch
                              system mcp (gptel--model-name model) effort
                              (niwa-claude-code-btw--fork-id fork))))
		 ;; Root context receipts and continuation hooks do not own a side.
		 (dolist (key '(:check-context :complete-prompt :control :observe))
		   (setq launch (plist-put launch key nil)))
		 launch))
	     (lambda () (niwa-claude-code-btw--content (or input "") model))
	     (niwa-tools-native-roster request)
	     nil
	     (lambda (outcome)
               (let* ((info (niwa-engine-info request))
                      (status (plist-get outcome :status))
                      (error (plist-get info :error)))
		 (when (eq status 'aborted)
		   (goto-char (point-max))
		   (insert (propertize
			    "\nSide conversation aborted. Open a new /btw conversation to continue.\n"
			    'gptel 'response)))
		 (niwa-view--append-request-summary
		  buffer (plist-get info :position)
		  (list :outcome status :backend "Claude Code"
			:message (if (stringp error) error (plist-get error :message))))))
	     (lambda (status)
               (with-current-buffer buffer
		 (unless (eq status 'success)
		   (setf (niwa-claude-code-btw--fork-closed fork) t))
		 (setf (niwa-session-turn-count niwa--session)
                       (niwa-request-turn request))
		 (when (eq request niwa--current-request) (niwa-request-end))
		 (niwa-claude-code-btw--delete fork))))
	    request)
        ((error quit)
         (setf (niwa-claude-code-btw--fork-closed fork) t)
         (when (eq request niwa--current-request)
           (niwa-request-cancel request)
           (niwa-request-end))
         (niwa-claude-code-btw--delete fork)
         (signal (car err) (cdr err)))))))

(defun niwa-claude-code-btw-shutdown ()
  "Retire native side writers and wait boundedly for ACP deletion on exit."
  (dolist (fork (copy-sequence niwa-claude-code-btw--forks))
    (if (buffer-live-p (niwa-claude-code-btw--fork-buffer fork))
        (with-current-buffer (niwa-claude-code-btw--fork-buffer fork)
          (niwa-claude-code-btw-close))
      (setf (niwa-claude-code-btw--fork-closed fork) t)
      (niwa-claude-code-btw--delete fork)))
  (let ((deadline (+ (float-time) (* 2 niwa-acp--control-timeout))))
    (while (and niwa-claude-code-btw--forks (< (float-time) deadline)
                (cl-some (lambda (fork)
                           (or (niwa-claude-code-btw--fork-pending fork)
                               (niwa-claude-code-btw--fork-deleting fork)
                               (and (buffer-live-p (niwa-claude-code-btw--fork-buffer fork))
                                    (buffer-local-value 'niwa--current-request
                                                        (niwa-claude-code-btw--fork-buffer fork)))))
                         niwa-claude-code-btw--forks))
      (accept-process-output nil 0.05)))
  (dolist (scope (delete-dups
                 (mapcar (lambda (fork)
                           (or (niwa-claude-code-btw--fork-diagnostic-scope fork)
                               'unavailable))
                         niwa-claude-code-btw--forks)))
    (niwa-diagnostic-warning
     scope 'niwa "Some /btw native forks could not be deleted before exit")))

(add-hook 'kill-emacs-hook #'niwa-claude-code-btw-shutdown)

(provide 'niwa-claude-code-btw)
;;; niwa-claude-code-btw.el ends here
