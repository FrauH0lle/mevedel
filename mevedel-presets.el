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
    (revise . mevedel-revise))
  "Alist mapping actions to presets."
  :group 'mevedel
  :type '(alist :key-type symbol))

(defun mevedel--define-presets ()
  "Define gptel presets for mevedel actions."
  (require 'gptel)

  ;; Read-only preset for discussion/analysis
  (gptel-make-preset 'mevedel-discuss
    :description "Read-only tools for code analysis and discussion"
    :tools `(,@mevedel-tools--ro-tools)
    :send--handlers '(;; Clear any pending folder access requests
                      :function (lambda (handlers)
                                  (mevedel--add-termination-handler
                                   (lambda (fsm)
                                     (when-let* ((info (gptel-fsm-info fsm))
                                                 (chat-buffer (plist-get info :buffer)))
                                       (with-current-buffer chat-buffer
                                         (mevedel--clear-pending-access-requests))))
                                   handlers))
                      ;; Generate final patch
                      :function (lambda (handlers)
                                  (mevedel--add-termination-handler
                                   (lambda (fsm)
                                     (when-let* ((info (gptel-fsm-info fsm))
                                                 (chat-buffer (plist-get info :buffer)))
                                       (let* ((workspace (with-current-buffer chat-buffer (mevedel-workspace)))
                                              (buffer (mevedel--chat-buffer nil workspace))
                                              (final-patch (with-current-buffer buffer
                                                             (mevedel--generate-final-patch))))
                                         ;; Clear file snapshots
                                         (with-current-buffer buffer
                                           (setq mevedel--request-file-snapshots nil))
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
                                   handlers)))
    :system "You are helping analyze and discuss code. Use read_file_lines and grep_files to understand the codebase. Do not make any file modifications.")

  ;; Full editing preset for implementation
  (gptel-make-preset 'mevedel-implement
    :parents '(mevedel-discuss)
    :description "Full editing capabilities with patch review workflow"
    :tools `(:append ,mevedel-tools--rw-tools)
    :system "You are implementing code changes. Use edit_files to make changes - provide old_str and new_str for simple replacements. All changes will be shown to the user for approval before applying.")

  ;; Revision preset with previous patch context
  (gptel-make-preset 'mevedel-revise
    :parents '(mevedel-implement)
    :description "Revise previous implementation with full context"
    :system "You are revising a previous implementation. The previous patch and its context are included in the conversation. Analyze what needs to be changed and create an improved implementation."))

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

(provide 'mevedel-presets)
;;; mevedel-presets.el ends here
