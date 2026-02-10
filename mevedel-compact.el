;;; mevedel-compact.el --- Chat compaction -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; `gptel'
(defvar gptel-mode)
(defvar gptel-use-header-line)
(defvar gptel--header-line-info)
(defvar gptel--markdown-block-map)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-markdown-cycle-block "ext:gptel" ())

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request")
(declare-function gptel-request "ext:gptel-request")
(defvar gptel--request-alist)
(defvar gptel-tools)
(defvar gptel-use-tools)

;; `mevedel'
(declare-function mevedel--chat-buffer "mevedel" (&optional create workspace))

(defcustom mevedel-compact-context-limit 200000
  "Current models maximum context window in tokens.

Warning appears in header-line when tokens the of value
`mevedel-compact-token-threshold'.."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-compact-token-threshold 0.75
  "Estimated token threshold for compaction warning.
Can be either the number of tokens as an integer or a float between 0
and 1, used as a ratio."
  :type '(choice (integer :tag "Absolute token count")
                 (float :tag "Ratio (0.0-1.0)"))
  :group 'mevedel)

(defun mevedel--file-local-variables-start ()
  "Return position where file-local variables block starts, or nil.
Searches forward from beginning of buffer for the first Local Variables
block."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Local Variables:" nil t)
      (line-beginning-position))))

(defun mevedel--estimate-tokens ()
  "Estimate the number of tokens in the current buffer.
Only counts text not marked with the `gptel' property `ignore' and
excludes file-local variables block."
  (let ((pos (point-min))
        (total 0)
        (flv-start (mevedel--file-local-variables-start)))
    (while (< pos (point-max))
      (let* ((next (next-single-property-change pos 'gptel nil (point-max)))
             (prop (get-text-property pos 'gptel)))
        (unless (eq prop 'ignore)
          ;; Only count if not in file-local-variables region
          (when (or (null flv-start) (< pos flv-start))
            (setq total (+ total (- (if (and flv-start (> next flv-start))
                                        flv-start
                                      next)
                                   pos)))))
        (setq pos next)))
    (/ total 4)))

(defun mevedel--compact-prompt ()
  "Return the compaction system prompt."
  "Your task is to create a detailed summary of the conversation so far,
paying close attention to the user's explicit requests and your previous
actions. This summary should be thorough in capturing technical details,
code patterns, and architectural decisions that would be essential for
continuing development work without losing context.

Before providing your final summary, wrap your analysis in <analysis>
tags to organize your thoughts and ensure you've covered all necessary
points. In your analysis process:

1. Chronologically analyze each message and section of the conversation.
   For each section thoroughly identify:
   - The user's explicit requests and intents
   - Your approach to addressing the user's requests
   - Key decisions, technical concepts and code patterns
   - Specific details like:
     - file names
     - full code snippets
     - function signatures
     - file edits
  - Errors that you ran into and how you fixed them
  - Pay special attention to specific user feedback that you received,
    especially if the user told you to do something differently.

2. Double-check for technical accuracy and completeness, addressing each
   required element thoroughly.

Your summary should include the following sections:

1. Primary Request and Intent: Capture all of the user's explicit requests and intents in detail
2. Key Technical Concepts: List all important technical concepts, technologies, and frameworks discussed.
3. Files and Code Sections: Enumerate specific files and code sections examined, modified, or created. Pay special attention to the most recent messages and include full code snippets where applicable and include a summary of why this file read or edit is important.
4. Errors and fixes: List all errors that you ran into, and how you fixed them. Pay special attention to specific user feedback that you received, especially if the user told you to do something differently.
5. Problem Solving: Document problems solved and any ongoing troubleshooting efforts.
6. All user messages: List ALL user messages that are not tool results. These are critical for understanding the users' feedback and changing intent.
6. Pending Tasks: Outline any pending tasks that you have explicitly been asked to work on.
7. Current Work: Describe in detail precisely what was being worked on immediately before this summary request, paying special attention to the most recent messages from both user and assistant. Include file names and code snippets where applicable.
8. Optional Next Step: List the next step that you will take that is related to the most recent work you were doing. IMPORTANT: ensure that this step is DIRECTLY in line with the user's most recent explicit requests, and the task you were working on immediately before this summary request. If your last task was concluded, then only list next steps if they are explicitly in line with the users request. Do not start on tangential requests or really old requests that were already completed without confirming with the user first.
                       If there is a next step, include direct quotes from the most recent conversation showing exactly what task you were working on and where you left off. This should be verbatim to ensure there's no drift in task interpretation.

Here's an example of how your output should be structured:

<example>
<analysis>
[Your thought process, ensuring all points are covered thoroughly and accurately]
</analysis>

<summary>
1. Primary Request and Intent:
   [Detailed description]

2. Key Technical Concepts:
   - [Concept 1]
   - [Concept 2]
   - [...]

3. Files and Code Sections:
   - [File Name 1]
      - [Summary of why this file is important]
      - [Summary of the changes made to this file, if any]
      - [Important Code Snippet]
   - [File Name 2]
      - [Important Code Snippet]
   - [...]

4. Errors and fixes:
    - [Detailed description of error 1]:
      - [How you fixed the error]
      - [User feedback on the error if any]
    - [...]

5. Problem Solving:
   [Description of solved problems and ongoing troubleshooting]

6. All user messages:
    - [Detailed non tool use user message]
    - [...]

7. Pending Tasks:
   - [Task 1]
   - [Task 2]
   - [...]

8. Current Work:
   [Precise description of current work]

9. Optional Next Step:
   [Optional Next step to take]

</summary>
</example>

Please provide your summary based on the conversation so far, following this structure and ensuring precision and thoroughness in your response.

There may be additional summarization instructions provided in the included context. If so, remember to follow these instructions when creating the above summary. Examples of instructions include:
<example>
## Compact Instructions
When summarizing the conversation focus on typescript code changes and also remember the mistakes you made and how you fixed them.
</example>

<example>
# Summary instructions
When you are using compact - please focus on test output and code changes. Include file reads verbatim.
</example>")

(defun mevedel--token-header-segment ()
  "Return a header-line segment showing estimated token count.
Returns a propertized string when tokens exceed 80% of
`mevedel-compact-token-threshold', or an empty string otherwise."
  (let* ((tokens (mevedel--estimate-tokens))
         (context-width mevedel-compact-context-limit)
         (threshold mevedel-compact-token-threshold)
         ratio)
    (if (cond ((integerp threshold)
               (setq ratio (/ (float tokens) threshold))
               (> tokens (* 0.8 threshold)))
              ((floatp threshold)
               (setq ratio (/ (float tokens) context-width))
               (> (/ (float tokens) context-width) threshold)))
        (let ((face (if (>= ratio 1.0) 'error 'warning)))
          (propertize (format " [Context: %dk/%dk] "
                              (/ tokens 1000)
                              (/ context-width 1000))
                      'face face))
      "")))

(defun mevedel--compact-buffer-active-p (buf)
  "Return non-nil if BUF has an active gptel request."
  (cl-find-if
   (lambda (entry)
     (eq (thread-first (cadr entry)
                       (gptel-fsm-info)
                       (plist-get :buffer))
         buf))
   gptel--request-alist))

(defun mevedel--compact-find-boundary ()
  "Find the compaction boundary in the current buffer.
Walks backward from the end to find the end of the last response.
Everything up to that point will be compacted. Returns the position just
after the last response, or nil if no response exists."
  (let ((pos (point-max)))
    (while (and pos (not (eq (get-text-property pos 'gptel) 'response)))
      (setq pos (previous-single-property-change pos 'gptel)))
    ;; pos is now inside the last response region (or nil).
    ;; Find where this response region ends.
    (when pos
      (next-single-property-change pos 'gptel nil (point-max)))))

(defun mevedel--compact-apply (boundary summary)
  "Apply compaction to the current buffer at BOUNDARY with SUMMARY.
Marks content before BOUNDARY as ignored and dimmed, inserts a separator
and the SUMMARY text at BOUNDARY."
  (let ((inhibit-read-only t))
    ;; Mark old content as ignored by parsers
    (put-text-property (point-min) boundary 'gptel 'ignore)
    ;; Dim old content visually
    (put-text-property (point-min) boundary 'face 'shadow)
    ;; Insert separator and summary at boundary
    (save-excursion
      (goto-char boundary)
      (let ((sep (format "\n\n--- Conversation compacted at %s ---\n\n"
                         (format-time-string "%Y-%m-%d %H:%M"))))
        ;; Remove any inherited gptel properties from the inserted text
        (remove-text-properties 0 (length summary) '(gptel nil face nil) summary)

        ;; (add-text-properties
        ;;  0 (length sep) '(gptel ignore) sep)
        (insert
         (propertize sep 'gptel 'ignore)
         (if (derived-mode-p 'org-mode)
             (propertize "#+begin_summary\n" 'gptel 'ignore)
           (propertize "``` summary\n" 'gptel 'ignore
                       'keymap gptel--markdown-block-map))
         summary
         (if (derived-mode-p 'org-mode)
             (concat "\n" (propertize "#+end_summary\n" 'gptel 'ignore))
           (concat "\n" (propertize "```\n" 'gptel 'ignore
                                    'keymap gptel--markdown-block-map))))
        ;; Fold the summary immediately.
        (ignore-errors
          (if (derived-mode-p 'org-mode)
              (save-excursion
                (search-backward "#+begin_summary" boundary t)
                (when (looking-at "^#+begin_summary")
                  (org-cycle)))
            (save-excursion
              (when (re-search-backward "^```" boundary t)
                (gptel-markdown-cycle-block)))))))))

;;;###autoload
(defun mevedel-compact ()
  "Compact the conversation in the current mevedel chat buffer.
Summarizes old exchanges via an LLM call and marks them as ignored, so
only the summary and the last exchange are sent in future requests."
  (interactive)
  (let* ((chat-buffer
          (cond
           ((and (bound-and-true-p gptel-mode) (bound-and-true-p mevedel--workspace))
            (current-buffer))
           (t (mevedel--chat-buffer)))))
    (unless (and chat-buffer (buffer-live-p chat-buffer))
      (user-error "No mevedel chat buffer found"))
    (with-current-buffer chat-buffer
      ;; Guard: refuse if active request
      (when (mevedel--compact-buffer-active-p chat-buffer)
        (user-error "Cannot compact while a request is active"))
      ;; Guard: refuse if not enough content
      (let ((boundary (mevedel--compact-find-boundary)))
        (unless boundary
          (user-error "Not enough conversation content to compact"))
        (let* ((boundary-marker (copy-marker boundary))
               (old-content (buffer-substring-no-properties (point-min) boundary))
               (tokens-before (mevedel--estimate-tokens)))
          (when (and gptel-mode gptel-use-header-line header-line-format)
            (setf (nth 2 header-line-format)
                  (propertize "  Compacting conversation...  " 'face 'warning)))
          ;; Send compaction request without tools or transforms
          (let ((gptel-tools nil)
                (gptel-use-tools nil))
            (gptel-request old-content
              :system (mevedel--compact-prompt)
              :buffer chat-buffer
              :stream nil
              :transforms nil
              :callback
              (lambda (response info)
                (pcase response
                  ('nil (user-error "Compaction failed: %s" (plist-get info :error)))
                  ((pred stringp)
                   (with-current-buffer chat-buffer
                     (mevedel--compact-apply boundary-marker response)
                     (set-marker boundary-marker nil)
                     (when (and gptel-mode gptel-use-header-line header-line-format)
                       (setf (nth 2 header-line-format) gptel--header-line-info))
                     (message "Compaction complete: %dk → %dk tokens"
                              (/ tokens-before 1000)
                              (/ (mevedel--estimate-tokens) 1000))))
                  ('abort
                   (user-error "Compaction aborted by user")))))))))))

(provide 'mevedel-compact)
;;; mevedel-compact.el ends here
