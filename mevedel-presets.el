;;; mevedel-presets.el -- Presets used -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))


;;
;;; Presets

(defcustom mevedel-action-preset-alist
  '((implement . mevedel-implement)
    (discuss . mevedel-discuss)
    (revise . mevedel-revise)
    (teach . mevedel-teach))
  "Alist mapping actions to presets."
  :group 'mevedel
  :type '(alist :key-type symbol))

;;;###autoload
(defun mevedel--define-presets ()
  "Define gptel presets for mevedel actions."
  (require 'gptel)

  ;; Read-only preset for discussion/analysis
  (gptel-make-preset 'mevedel-discuss
    :description "Read-only tools for code analysis and discussion"
    :tools '(:function (lambda (_tools)
                         (cl-delete-duplicates
                          (cl-loop for tool in (append mevedel-tools--read-tools
                                                       mevedel-tools--util-tools
                                                       mevedel-tools--eval-tools)
                                           append (ensure-list (gptel-get-tool tool)))))
             ;; Add agents
             :function (lambda (tools)
                         (when-let* ((chat-buffer (mevedel--chat-buffer nil (mevedel-workspace))))
                           (with-current-buffer chat-buffer
                             (setq-local gptel-agent--agents
                                         (append mevedel-agents--agents
                                                 `(,(with-temp-buffer
                                                      (make-local-variable 'gptel-agent--agents)
                                                      (gptel-agent-update)
                                                      (let* ((spec (assoc-string "introspector" gptel-agent--agents))
                                                             (plist (cdr spec)))
                                                        (setq plist (plist-put plist :tools '(:function (lambda (_tools)
                                                                                                          (append
                                                                                                           (gptel-get-tool "introspection")
                                                                                                           (list
                                                                                                            (gptel-get-tool '("gptel-agent" "Eval"))
                                                                                                            (gptel-get-tool '("mevedel" "Ask"))
                                                                                                            (gptel-get-tool '("mevedel" "RequestAccess"))))))))
                                                        (setq plist (plist-put plist :system (concat (plist-get plist :system) "\nIn case you need clarification, use your 'Ask' tool to interact with the user." )))
                                                        (cons "introspector" plist))))))
                             (setf (plist-get (car (gptel-tool-args (gptel-get-tool "Agent"))) :enum)
                                   (vconcat (mapcar #'car gptel-agent--agents)))))
                         tools))
    :send--handlers '(;; Generate final patch
                      :function (lambda (handlers)
                                  (mevedel--add-termination-handler
                                   (lambda (fsm)
                                     (when-let* ((info (gptel-fsm-info fsm))
                                                 (chat-buffer (plist-get info :buffer)))
                                       (let* ((workspace (with-current-buffer chat-buffer (mevedel-workspace)))
                                              (buffer (mevedel--chat-buffer nil workspace))
                                              (final-patch (with-current-buffer buffer
                                                             (mevedel--generate-final-patch))))
                                         (when (and final-patch (> (length final-patch) 0))
                                           (mevedel--replace-patch-buffer final-patch)))))
                                   handlers))
                      ;; Run callback from instruction
                      :function (lambda (handlers)
                                  (mevedel--add-termination-handler
                                   (lambda (fsm)
                                     (when-let* ((info (gptel-fsm-info fsm))
                                                 (request-callback (plist-get info :mevedel-request-callback)))
                                       (when (functionp request-callback)
                                         (funcall request-callback nil fsm))))
                                   handlers))
                      ;; Cleanup local vars
                      :function (lambda (handlers)
                                  (mevedel--add-termination-handler
                                   (lambda (fsm)
                                     (when-let* ((info (gptel-fsm-info fsm))
                                                 (chat-buffer (plist-get info :buffer)))
                                       (with-current-buffer chat-buffer
                                         ;; Clear file snapshots
                                         (setq mevedel--request-file-snapshots nil)
                                         ;; Clear pending access requests
                                         (mevedel--clear-pending-access-requests))))
                                   handlers))
                      ;; Ensure chat buffer contains the `gptel-prompt-prefix-string'
                      :function (lambda (handlers)
                                  (mevedel--add-termination-handler #'mevedel--cleanup-chat-buffer handlers)))
    :system '(:function (lambda (_system)
                          (mevedel-system-build-prompt mevedel-system--base-prompt mevedel-tools--read-tools))))

  ;; Full editing preset for implementation
  (gptel-make-preset 'mevedel-implement
    :parents '(mevedel-discuss)
    :description "Full editing capabilities with patch review workflow"
    :tools '(:function (lambda (tools)
                         (cl-delete-duplicates
                          (append tools
                                  (cl-loop for tool in mevedel-tools--edit-tools
                                           append (ensure-list (gptel-get-tool tool)))))))
    :system '(:function (lambda (_system)
                          (mevedel-system-build-prompt mevedel-system--base-prompt
                           (append mevedel-tools--read-tools mevedel-tools--edit-tools)))))

  ;; Revision preset with previous patch context
  (gptel-make-preset 'mevedel-revise
    :parents '(mevedel-implement)
    :description "Revise previous implementation with full context"
    :system "You are revising a previous implementation. The previous patch and its context are included in the conversation. Analyze what needs to be changed and create an improved implementation.")

  ;; Teaching preset - guides through hints, never provides solutions
  (gptel-make-preset 'mevedel-teach
    :parents '(mevedel-discuss)  ; Inherit read-only tools + handlers
    :description "Teaching preset - guides through hints, never provides solutions"
    :tools '(:function (lambda (tools)
                         ;; Add teaching tools to inherited tools
                         (append tools
                                 (list (gptel-get-tool '("mevedel" "GetHints"))
                                       (gptel-get-tool '("mevedel" "RecordHint"))))))
    :system '(:function (lambda (_system)
                          (mevedel-system-build-prompt
                           mevedel-system--teaching-base-prompt
                           (append mevedel-tools--read-tools
                                   mevedel-tools--util-tools
                                   mevedel-tools--eval-tools
                                   '(("mevedel" "GetHints")
                                     ("mevedel" "RecordHint"))))))))

(defun mevedel--add-termination-handler (handler handlers &optional transitions)
  "Update FSM's state HANDLERS to call HANDLER when the request terminates.

Optional argument TRANSITIONS is an alist like
`gptel-request--transitions'.

The HANDLER will receive one argument when the request terminates:
- FSM: the `gptel-fsm' struct for the request

The request is considered to have terminated when the FSM reaches a
state with no possible transitions to another state."
  (let* (;; An alist of states mapped to potential next states. See
         ;; 'gptel-request--transitions'.
         (transitions (or transitions gptel-request--transitions))
         ;; Find all potential next states in one of the rules
         (all-states
          (cl-remove-duplicates
           (append
            (mapcar #'car transitions)
            (cl-mapcan (lambda (entry) (mapcar #'cdr (cdr entry))) transitions))))
         ;; Collect states that either don't appear as keys, or appear as keys
         ;; but have no possible next states. These are states which can't
         ;; transition to any other states.
         (terminal-states
          (cl-remove-if-not
           (lambda (state)
             (let ((entry (assq state transitions)))
               ;; If no entry exists or entry exists but has no transitions
               (or (null entry) (null (cdr entry)))))
           all-states))
         ;; Alist whose keys are the terminal states, and values are their new
         ;; lists of handlers
         (terminal-state-handlers
          (cl-loop
           for state in terminal-states
           for existing-entry = (assq state handlers)
           collect (if existing-entry
                       (if (member handler (cdr existing-entry))
                           ;; Handler already present, return entry unchanged
                           existing-entry
                         ;; Handler not present, add it
                         (cons state (append (cdr existing-entry) (list handler))))
                     ;; (cons state (append (cdr existing-entry) (list handler)))
                     (cons state (list handler)))))
         ;; Create a new handlers list for this FSM
         (augmented-handlers
          (append
           ;; Copy existing non-terminal handlers
           (cl-remove-if (lambda (entry) (member (car entry) terminal-states)) handlers)
           ;; Add our terminal state handlers
           terminal-state-handlers)))
    ;; Update the handlers list
    augmented-handlers))

(defun mevedel--cleanup-chat-buffer (fsm)
  "Clean up chat buffer after request completion.

FSM is the `gptel' finite state machine.

Ensures the gptel prompt prefix is present at the end of the response,
and in `org-mode' buffers, performs additional fixup: indents the
subtree, widens if narrowed, and adds visual separator before next
heading."
  (let* ((info (gptel-fsm-info fsm))
         (chat-buffer (plist-get info :buffer))
         ;; This should always be present.
         (start-marker (plist-get info :position))
         ;; This might be nil if the request ended with an error/abort before
         ;; any response was received
         (tracking-marker (plist-get info :tracking-marker))
         (current-marker (or tracking-marker start-marker)))
    (with-current-buffer chat-buffer
      ;; The current marker should always be non-nil, but don't error out if
      ;; it's missing/both markers were nil for some reason.
      (when current-marker
        ;; Ensure prompt prefix is present
        (save-excursion
          (goto-char current-marker)
          (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
            ;; Check if we're at the end of the gptel prompt prefix, i.e. the one
            ;; that was just inserted due to the completion of the request. If not
            ;; (this is the case if the request ended with an error or abort),
            ;; insert it so we're prepared for the next prompt.
            (unless (and (>= (point) (length prefix))
                         (string=
                          prefix
                          (buffer-substring-no-properties (- (point) (length prefix)) (point))))
              (insert "\n" prefix))))

        ;; In org-mode, perform fixup: indent, widen, add separator
        (when (derived-mode-p 'org-mode)
          (unless (org-before-first-heading-p)
            (save-excursion
              ;; Indent the current subtree
              (org-mark-subtree)
              (indent-region (region-beginning) (region-end))
              (deactivate-mark)))
          ;; Remove narrowing if present
          (when (buffer-narrowed-p)
            (widen))
          ;; Add newline separator before next heading if not already present
          (save-excursion
            (goto-char current-marker)
            (when (= (org-next-visible-heading 1) 0)
              (beginning-of-line)
              ;; Only add newline if previous line is not empty
              (unless (and (> (point) (point-min))
                           (save-excursion
                             (forward-line -1)
                             (looking-at-p "^[[:space:]]*$")))
                (newline)))))))))

(provide 'mevedel-presets)
;;; mevedel-presets.el ends here
