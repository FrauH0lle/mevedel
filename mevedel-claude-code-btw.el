;;; mevedel-claude-code-btw.el --- Ephemeral native side conversations -*- lexical-binding: t -*-

;;; Commentary:
;; Fork settled native history for /btw.  The fork is never a durable mevedel
;; session reference.  Each turn uses the ordinary ACP/MCP transport with the
;; side's frozen tools and its own settlement; normal root dispatch is bypassed.

;;; Code:

(require 'mevedel-acp-turn)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-context)
(require 'mevedel-claude-code-history)
(require 'mevedel-tools)
(require 'mevedel-turn)

;; `mevedel-side-conversation'
(defvar mevedel-side-conversation--frozen-context)
(defvar mevedel-side-conversation--frozen-request-denies)

;; `mevedel-view-render'
(declare-function mevedel-view--append-request-summary
                  "mevedel-view-render" (data-buf search-start &optional extra))

(cl-defstruct (mevedel-claude-code-btw--fork
               (:constructor mevedel-claude-code-btw--make-fork))
  "One owned native fork, including late cleanup after its buffer dies."
  buffer launch id closed deleting pending error)

(defvar mevedel-claude-code-btw--forks nil
  "Native side forks not yet deleted, including pending fork replies.")

(defvar-local mevedel-claude-code-btw--fork nil
  "Native fork owned by this side data buffer.")
(put 'mevedel-claude-code-btw--fork 'permanent-local t)

(defun mevedel-claude-code-btw-parent-history ()
  "Return a snapshot of this buffer's settled native history, or refuse."
  (let ((history (alist-get "root" (mevedel-session-external-conversations
                                    mevedel--session) nil nil #'equal)))
    (mevedel-claude-code-history-assert-current history)
    (unless (and (mevedel-claude-code-backend-p gptel-backend)
                 (not (mevedel-turn-busy-p))
                 (eq 'ready (plist-get history :state))
                 (plist-get history :id)
                 (not (plist-get history :start-failed)))
      (user-error "/btw needs settled, current Claude history; finish a parent turn first"))
    (copy-tree history)))

(defun mevedel-claude-code-btw--delete (fork)
  "Delete FORK once its last writer has stopped, retaining failed cleanup."
  (let ((buffer (mevedel-claude-code-btw--fork-buffer fork)))
    (when (and (mevedel-claude-code-btw--fork-closed fork)
               (not (mevedel-claude-code-btw--fork-pending fork))
               (not (mevedel-claude-code-btw--fork-deleting fork))
               (not (and (buffer-live-p buffer)
                         (buffer-local-value 'mevedel--current-request buffer))))
      (if (not (mevedel-claude-code-btw--fork-id fork))
          (setq mevedel-claude-code-btw--forks
                (delq fork mevedel-claude-code-btw--forks))
        (setf (mevedel-claude-code-btw--fork-deleting fork) t)
        (condition-case err
            (mevedel-acp-session-operation
             (mevedel-claude-code-btw--fork-launch fork) 'delete
             (mevedel-claude-code-btw--fork-id fork)
             (lambda (outcome)
               (setf (mevedel-claude-code-btw--fork-deleting fork) nil)
               (if (eq 'success (plist-get outcome :status))
                   (progn
                     (setf (mevedel-claude-code-btw--fork-id fork) nil
                           (mevedel-claude-code-btw--fork-error fork) nil)
                     (setq mevedel-claude-code-btw--forks
                           (delq fork mevedel-claude-code-btw--forks)))
                 (setf (mevedel-claude-code-btw--fork-error fork)
                       (plist-get outcome :message))
                 (display-warning 'mevedel
                                  (format "Could not delete /btw native fork %s: %s"
                                          (mevedel-claude-code-btw--fork-id fork)
                                          (plist-get outcome :message))))))
          (error
           (setf (mevedel-claude-code-btw--fork-deleting fork) nil
                 (mevedel-claude-code-btw--fork-error fork) (error-message-string err))
           (display-warning 'mevedel (error-message-string err))))))))

(defun mevedel-claude-code-btw-close ()
  "Retire this buffer's native fork and delete it after cancellation settles."
  (when-let* ((fork mevedel-claude-code-btw--fork))
    (setf (mevedel-claude-code-btw--fork-closed fork) t)
    (when mevedel--current-request
      (mevedel-request-cancel mevedel--current-request))
    (mevedel-claude-code-btw--delete fork)))

(put 'mevedel-claude-code-btw-close 'permanent-local-hook t)

(defun mevedel-claude-code-btw-open (parent side history ready)
  "Fork PARENT's frozen HISTORY for SIDE, calling READY after admission.
Closing SIDE before the reply still deletes the returned fork.  If PARENT
changes during startup, refuse rather than silently capture newer history."
  (let* ((launch (with-current-buffer parent
                   (mevedel-claude-code-launch
                    "Prepare ephemeral side conversation" []
                    (gptel--model-name gptel-model) nil)))
         (tick (buffer-chars-modified-tick parent))
         (fork (mevedel-claude-code-btw--make-fork
                :buffer side :launch launch :pending t)))
    (push fork mevedel-claude-code-btw--forks)
    (with-current-buffer side
      (setq-local mevedel-claude-code-btw--fork fork)
      (add-hook 'kill-buffer-hook #'mevedel-claude-code-btw-close nil t))
    (condition-case err
        (mevedel-acp-session-operation
         launch 'fork (plist-get history :id)
         (lambda (outcome)
           (setf (mevedel-claude-code-btw--fork-pending fork) nil
                 (mevedel-claude-code-btw--fork-id fork) (plist-get outcome :session-id))
           (condition-case failure
               (progn
                 (unless (eq 'success (plist-get outcome :status))
                   (error "%s" (plist-get outcome :message)))
                 (unless (and (buffer-live-p parent) (buffer-live-p side)
                              (not (mevedel-claude-code-btw--fork-closed fork)))
                   (error "The /btw owner closed while forking"))
                 (with-current-buffer parent
                   (unless (and (= tick (buffer-chars-modified-tick))
                                (equal history (mevedel-claude-code-btw-parent-history)))
                     (error "Parent history changed while opening /btw; try again")))
                 (funcall ready))
             ((error quit)
              (setf (mevedel-claude-code-btw--fork-closed fork) t)
              (when (buffer-live-p side) (kill-buffer side))
              (mevedel-claude-code-btw--delete fork)
              (display-warning 'mevedel (error-message-string failure))))))
      ((error quit)
       (setf (mevedel-claude-code-btw--fork-pending fork) nil
             (mevedel-claude-code-btw--fork-closed fork) t)
       (mevedel-claude-code-btw--delete fork)
       (signal (car err) (cdr err))))))

(defun mevedel-claude-code-btw--content (input model)
  "Expand side INPUT for MODEL, including frozen context and fresh mentions."
  (let* ((context mevedel-side-conversation--frozen-context)
         (expansion (mevedel-mentions-expand-user-input input mevedel--session t model)))
    (vconcat
     (list `((type . "text")
             (text . ,(string-join
                       (delq nil (append (list (plist-get context :text)
                                               (plist-get expansion :text))
                                         (mapcar (lambda (item)
                                                   (mevedel-reminders-format-block
                                                    (plist-get item :body)))
                                                 (plist-get expansion :reminder-items))))
                       "\n\n"))))
     (mevedel-claude-code-context--images
      (append (mapcar (lambda (media)
                        (list (car media) (plist-get (cdr media) :mime)))
                      (plist-get context :media))
              (plist-get expansion :media-contexts)) model))))

(defun mevedel-claude-code-btw-assert-open ()
  "Refuse a side whose native fork is not available for another turn."
  (let ((fork mevedel-claude-code-btw--fork))
    (unless (and fork (mevedel-claude-code-btw--fork-id fork)
                 (not (mevedel-claude-code-btw--fork-closed fork)))
      (user-error "This native /btw fork has ended; close it and open a new side conversation"))))

(defun mevedel-claude-code-btw-send (input policy)
  "Send side INPUT under frozen model POLICY without publishing native history."
  (let* ((fork mevedel-claude-code-btw--fork)
         (buffer (current-buffer))
         (model (plist-get policy :model))
         (effort (plist-get policy :effort))
         (system gptel-system-prompt))
    (mevedel-claude-code-btw-assert-open)
    (let ((request (mevedel-request-begin mevedel--session)))
      (condition-case err
          (progn
	    (setf (mevedel-request-one-shot-mutations-p request) t
		  (mevedel-request-ephemeral-p request) t
		  (mevedel-request-skill-permission-rules request)
		  (copy-tree mevedel-side-conversation--frozen-request-denies)
		  (mevedel-engine-info request)
		  (append (mevedel-engine-info request)
			  (list :backend (plist-get policy :backend) :model model
				:reasoning-effort effort :external-history t)))
	    (mevedel-acp-turn-start
	     request
	     (lambda (mcp _hook-command)
               (let ((launch (mevedel-claude-code-launch
                              system mcp (gptel--model-name model) effort
                              (mevedel-claude-code-btw--fork-id fork))))
		 ;; Root context receipts and continuation hooks do not own a side.
		 (dolist (key '(:check-context :complete-prompt :control :observe))
		   (setq launch (plist-put launch key nil)))
		 launch))
	     (lambda () (mevedel-claude-code-btw--content (or input "") model))
	     (mevedel-tools-native-roster request)
	     nil
	     (lambda (outcome)
               (let* ((info (mevedel-engine-info request))
                      (status (plist-get outcome :status))
                      (error (plist-get info :error)))
		 (when (eq status 'aborted)
		   (goto-char (point-max))
		   (insert (propertize
			    "\nSide conversation aborted. Open a new /btw conversation to continue.\n"
			    'gptel 'response)))
		 (mevedel-view--append-request-summary
		  buffer (plist-get info :position)
		  (list :outcome status :backend "Claude Code"
			:message (if (stringp error) error (plist-get error :message))))))
	     (lambda (status)
               (with-current-buffer buffer
		 (unless (eq status 'success)
		   (setf (mevedel-claude-code-btw--fork-closed fork) t))
		 (setf (mevedel-session-turn-count mevedel--session)
                       (mevedel-request-turn request))
		 (when (eq request mevedel--current-request) (mevedel-request-end))
		 (mevedel-claude-code-btw--delete fork))))
	    request)
        ((error quit)
         (setf (mevedel-claude-code-btw--fork-closed fork) t)
         (when (eq request mevedel--current-request)
           (mevedel-request-cancel request)
           (mevedel-request-end))
         (mevedel-claude-code-btw--delete fork)
         (signal (car err) (cdr err)))))))

(defun mevedel-claude-code-btw-shutdown ()
  "Retire native side writers and wait boundedly for ACP deletion on exit."
  (dolist (fork (copy-sequence mevedel-claude-code-btw--forks))
    (if (buffer-live-p (mevedel-claude-code-btw--fork-buffer fork))
        (with-current-buffer (mevedel-claude-code-btw--fork-buffer fork)
          (mevedel-claude-code-btw-close))
      (setf (mevedel-claude-code-btw--fork-closed fork) t)
      (mevedel-claude-code-btw--delete fork)))
  (let ((deadline (+ (float-time) (* 2 mevedel-acp--control-timeout))))
    (while (and mevedel-claude-code-btw--forks (< (float-time) deadline)
                (cl-some (lambda (fork)
                           (or (mevedel-claude-code-btw--fork-pending fork)
                               (mevedel-claude-code-btw--fork-deleting fork)
                               (and (buffer-live-p (mevedel-claude-code-btw--fork-buffer fork))
                                    (buffer-local-value 'mevedel--current-request
                                                        (mevedel-claude-code-btw--fork-buffer fork)))))
                         mevedel-claude-code-btw--forks))
      (accept-process-output nil 0.05)))
  (when mevedel-claude-code-btw--forks
    (display-warning 'mevedel "Some /btw native forks could not be deleted before exit")))

(add-hook 'kill-emacs-hook #'mevedel-claude-code-btw-shutdown)

(provide 'mevedel-claude-code-btw)
;;; mevedel-claude-code-btw.el ends here
