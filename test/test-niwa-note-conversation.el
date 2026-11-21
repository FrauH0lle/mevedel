;;; test-niwa-note-conversation.el -- Note send flows -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise note commands in real files with the normal session/view pipeline.
;; gptel's network transport is replaced with canned answers; frame attendance
;; is fixed independently of the desktop's input focus.

;;; Code:

(require 'niwa)
(require 'niwa-chat)
(require 'niwa-claude-code)
(require 'niwa-note-frame)
(require 'niwa-note-conversation)
(require 'niwa-notes)
(require 'niwa-notes-list)
(require 'niwa-models)
(require 'niwa-permission-mode)
(require 'niwa-session-persistence)
(require 'niwa-session-codec)
(require 'niwa-session-control-fs)
(require 'niwa-session-fork)
(require 'niwa-session-rewind)
(require 'niwa-telemetry)
(require 'niwa-transport)
(require 'niwa-journal-process)
(require 'gptel-openai)
(require 'gptel-transient)
(require 'niwa-file-state)
(require 'niwa-tool-patch)
(require 'niwa-pipeline)
(require 'niwa-tool-note)
(require 'niwa-note-write)
(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(defmacro niwa-note-conversation-test--with-source (&rest body)
  "Run BODY in a real source file with canned transport and attended frames."
  (declare (indent 0))
  `(let* ((root (make-temp-file "niwa-note-send-" t))
          (file (file-name-concat root "source.el"))
          (niwa-sessions-directory (file-name-concat root "sessions"))
          (niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)
          (niwa-session-persistence--cleanup-throttle (make-hash-table :test #'equal))
          (niwa-instruction-store--dirty nil)
          (niwa-instruction-store--failed nil)
          (niwa-notes--unsaved-noticed nil)
          (niwa-journal-enabled nil)
          ;; `niwa-install' re-registers tools; restoring only gptel's list
          ;; would leave later tests' tools unknown to this registry.
          (niwa-tool--registry (copy-hash-table niwa-tool--registry))
          (gptel--known-tools (copy-tree gptel--known-tools))
          (gptel--known-presets (copy-tree gptel--known-presets))
          (niwa-preset--registry (copy-tree niwa-preset--registry))
          (gptel-prompt-transform-functions (copy-tree gptel-prompt-transform-functions))
          (transport-enabled niwa-transport--enabled-p)
          (gptel--known-backends nil)
          (gptel-backend (gptel-make-openai "Notes" :key "test" :models '(test-model)))
          (gptel-model 'test-model)
          (gptel-use-curl t)
          (gptel-stream nil)
          (before (buffer-list))
          (timers (append timer-list timer-idle-list nil))
          (events (copy-sequence unread-command-events))
          source workspace sent sent-payloads)
     (unwind-protect
         (save-window-excursion
           (make-directory (file-name-concat root ".niwa"))
           (write-region "outside-before\nselected code\noutside-after\n" nil file nil 'silent)
           (write-region "Project-only instruction" nil
                         (file-name-concat root "AGENTS.md") nil 'silent)
           (setq workspace (niwa-workspace-get-or-create 'file root root "notes")
                 source (find-file-noselect file))
           (niwa-install)
           ;; A compositor may deny focus; these cases test rendering, not suspension.
           (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                     ((symbol-function 'gptel-curl-get-response)
                      (lambda (fsm)
                        (let ((config (gptel-curl--get-config (gptel-fsm-info fsm) "test")))
                          (push (encode-coding-string
                                 (substring config (+ 2 (string-search "\n\n" config)))
                                 'utf-8-unix)
                                sent-payloads))
                        (push fsm sent))))
             (switch-to-buffer source)
             (setq-local niwa--workspace workspace)
             (goto-char (point-min))
             (forward-line 1)
             ,@body))
       (dolist (buf (cl-set-difference (buffer-list) before))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (set-buffer-modified-p nil)
             (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
       (dolist (timer (append timer-list timer-idle-list nil))
         (unless (memq timer timers) (cancel-timer timer)))
       (niwa-workspace-clear-registry)
       (setq niwa-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
       (niwa-uninstall)
       (when transport-enabled (niwa-transport-install))
       (delete-directory root t)
       ;; No command loop runs to consume our frame-focus events.
       (setq unread-command-events
             (cl-remove-if (lambda (event)
                             (and (eq (car-safe (if (eq (car-safe event) t)
                                                     (cdr event) event))
                                      'switch-frame)
                                  (not (memq event events))))
                           unread-command-events)))))

(niwa-deftest niwa-note-conversation-test--with-source
  (:quiet t)
  ,test
  (test)
  :doc "cleanup discards only new frame-switch events, preserving other input"
  (let* ((existing (list 'switch-frame (selected-frame)))
         (unread-command-events (list existing ?x)))
    (niwa-note-conversation-test--with-source
      (push ?y unread-command-events)
      (push (list t 'switch-frame (selected-frame)) unread-command-events)
      (push (list 'switch-frame (selected-frame)) unread-command-events))
    (should (equal (list ?y existing ?x) unread-command-events)))
  :doc "rendering stays attended without compositor focus and restores the focus query"
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) nil)))
    (niwa-note-conversation-test--with-source
      (should (frame-focus-state)))
    (should-not (frame-focus-state))))

(defun niwa-note-conversation-test--answer (fsm text)
  "Deliver canned TEXT through FSM's normal insertion and settlement."
  (let ((info (gptel-fsm-info fsm)))
    (plist-put info :http-status "200")
    (plist-put info :status "HTTP/1.1 200 OK")
    (gptel--fsm-transition fsm)
    (gptel--insert-response text info)
    (gptel--fsm-transition fsm)
    (niwa-test--await 5 "Note answer did not settle"
      (not (niwa-turn-busy-p (plist-get info :buffer))))))

(niwa-deftest niwa-note-send/streaming-frame
  (:quiet t)
  (cl-flet ((wait (seconds)
              (let ((timer (run-at-time seconds nil #'exit-recursive-edit)))
                (unwind-protect (recursive-edit) (cancel-timer timer)))))
    (when (display-graphic-p)
      (niwa-note-conversation-test--with-source
        (let* ((gptel-stream t)
               (gptel-backend (gptel-make-openai
                               "Streaming notes" :key "test" :models '(test-model)
                               :stream t))
               (niwa-view-spinner-style 'braille)
               (niwa-view-spinner-animate t)
               (niwa-note-frame-min-height 20)
               (niwa-note-frame-height 20)
               (draft "> active draft\nsecond line")
               (source-window (selected-window))
               (view (niwa-note-send (niwa-note-create "Explain this code")))
               (frame niwa-note-frame--frame)
               (window (get-buffer-window view frame))
               (editor (niwa-view-composer-window window))
               (info (gptel-fsm-info (car sent))))
          (unwind-protect
              (progn
                (should (frame-parent frame))
                (should (eq (window-buffer editor) (niwa-view-composer-buffer view)))
                (should (plist-get info :stream))
                (wait 0.2)
                (let ((target (if ,source-selected source-window window)))
                  (select-frame-set-input-focus (window-frame target))
                  (select-window target))
                (wait 0.2)
                (let ((selected (selected-window))
                      (source-start (window-start source-window))
                      draft-point draft-start draft-row)
                  (should (eq selected (if ,source-selected source-window window)))
                  (with-current-buffer view
                    (niwa-view-test--insert-composer-draft draft 4))
                  (wait 0.1)
                  (setq draft-point (window-point editor)
                        draft-start (window-start editor)
                        draft-row (cdr (posn-col-row (posn-at-point draft-point editor))))
                  (should draft-row)
                  (plist-put info :http-status "200")
                  (plist-put info :status "HTTP/1.1 200 OK")
                  (gptel--fsm-transition (car sent))
                  (dolist (chunk '("First streamed sentence.\n" "Second streamed sentence.\n"
                                   "Third streamed sentence.\n"))
                    (gptel-curl--stream-insert-response (copy-sequence chunk) info)
                    ;; Run real timers and redisplay without selecting the child.
                    (wait 0.6)
                    (with-current-buffer view
                      (should (string-search chunk (buffer-string)))
                      (should (equal draft (niwa-view-composer-draft-text))))
                    (with-current-buffer (window-buffer editor)
                      (let ((cell (text-property-any
                                   (point-min) (+ (point-min) 3)
                                   'niwa-view-spinner-frame t)))
                        (should cell)
                        (should (pos-visible-in-window-p cell editor))
                        (let ((sample (get-text-property cell 'display)))
                          (should (stringp sample))
                          (should
                           (cl-loop repeat 12
                                    do (wait 0.04)
                                    thereis (not (equal sample
                                                        (get-text-property cell 'display))))))))
                    (should (= draft-point (window-point editor)))
                    (should (= draft-start (window-start editor)))
                    (should (= draft-row (cdr (posn-col-row (posn-at-point draft-point editor)))))
                    (should (eq selected (selected-window)))
                    (should (= source-start (window-start source-window))))
                  (gptel--fsm-transition (car sent))
                  (niwa-test--await 5 "Streaming note did not settle"
                    (not (niwa-turn-busy-p (plist-get info :buffer))))))
            (when (frame-live-p frame) (niwa-note-frame-close)))))))
  (source-selected)
  :doc "child-selected note frame renders chunks and animates between chunks"
  nil
  :doc "source-selected note frame renders chunks and animates between chunks"
  t)

(niwa-deftest niwa-note-conversation-attention-state
  (:quiet t)
  ,test
  (test)
  :doc "redisplay projects overlay and plist conversation attention without copying overlays"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Explain this code"))
           (view (niwa-note-send note))
           (properties (overlay-properties note)))
      (with-current-buffer source
        (cl-letf (((symbol-function 'overlay-properties)
                   (lambda (_) (error "Redisplay copied an overlay plist"))))
          (should (equal " Notes: 1 working" (niwa-notes-mode-line)))
          (should (eq 'working (niwa-notes-attention-state note)))
          (should (eq 'working (niwa-notes-attention-state properties)))
          (overlay-put note 'niwa-note-attention-state 'ready)
          (should (equal " Notes: 1 ready" (niwa-notes-mode-line)))
          (overlay-put note 'niwa-note-attention-state nil)))
      (niwa-note-conversation-test--answer (car sent) "The answer.")
      (with-current-buffer source
        (should-not (niwa-notes-mode-line))
        (should (eq 'sent (niwa-notes-attention-state note)))
        (should (eq 'sent (niwa-notes-attention-state properties))))
      (should (buffer-live-p view)))))

(niwa-deftest niwa-note-send
  (:quiet t)
  ,test
  (test)
  :doc "send focuses a conversation with a read-only answer and editable prompt"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Why is this selected?"))
           (view (niwa-note-send note))
           (transcript (selected-window))
           (source-window (get-buffer-window source t))
           (composer (get-buffer-window (niwa-view-composer-buffer view) (selected-frame))))
      (should (eq view (window-buffer transcript)))
      (if (display-graphic-p)
          (progn
            (should (eq (selected-frame) niwa-note-frame--frame))
            (should (eq (frame-parent (selected-frame))
                        (window-frame source-window))))
        (should (> (window-left-column transcript) (window-left-column source-window))))
      (should (window-live-p composer))
      (should (eq composer (window-in-direction 'below transcript)))
      (should (= 1 (length sent)))
      (niwa-note-conversation-test--answer (car sent) "It is the selected code.")
      (with-current-buffer view
        (goto-char (point-min))
        (should (search-forward "It is the selected code." nil t))
        (should-error (insert "not editable") :type 'text-read-only)
        (niwa-view-composer-insert "A reply without the minibuffer"))
      (with-current-buffer source
        (should (equal "outside-before\nselected code\noutside-after\n" (buffer-string))))))
  :doc "initial input includes capabilities and memory but excludes unrelated conversation context"
  (niwa-note-conversation-test--with-source
    (make-directory (file-name-concat root ".niwa" "memory") t)
    (write-region "UNRELATED-MEMORY-SECRET" nil
                  (file-name-concat root ".niwa" "memory" "MEMORY.md") nil 'silent)
    (niwa-note-create "UNRELATED-NOTE-SECRET")
    (goto-char (point-min))
    (niwa-instruction-test--create-note-in source (point) (line-end-position))
    (let ((main (niwa--chat-buffer "main" t workspace)))
      (with-current-buffer main (insert "UNRELATED-CHAT-SECRET")))
    (forward-line 1)
    (let* ((note (niwa-note-create "Explain this code?"))
           (view (niwa-note-send note))
           (payload (decode-coding-string (car sent-payloads) 'utf-8-unix)))
      (dolist (expected '("Explain this code?" "selected code" "Project-only instruction"
                          "UNRELATED-MEMORY-SECRET" "## Skills" "## Resources"))
        (should (string-search expected payload)))
      (dolist (excluded '("UNRELATED-NOTE-SECRET"
                          "UNRELATED-CHAT-SECRET" "outside-before" "outside-after"))
        (should-not (string-search excluded payload)))
      (with-current-buffer view
        (let ((report (call-interactively (key-binding (kbd "C-c C-v")))))
          (with-current-buffer report
            (should (equal payload (buffer-substring-no-properties (point-min) (point-max)))))))
      (niwa-note-conversation-test--answer (car sent) "An explanation, not an edit.")))
  :doc "close preserves the conversation and send reopens without another request"
  (niwa-note-conversation-test--with-source
    (let* ((origin (point))
           (note (niwa-note-create "Explain, please"))
           (view (niwa-note-send note)))
      (niwa-note-conversation-test--answer (car sent) "A retained answer.")
      (with-current-buffer view
        (goto-char (point-max))
        (insert "Draft still here")
        (call-interactively (key-binding (kbd "C-c C-z"))))
      (should (buffer-live-p view))
      (should (eq source (window-buffer (selected-window))))
      (should (= origin (window-point (selected-window))))
      (with-current-buffer source
        (should (eq view (niwa-note-send note))))
      (should (= 1 (length sent)))
      (with-current-buffer view
        (should (string-search "A retained answer." (buffer-string)))
        (should (string-suffix-p "Draft still here" (buffer-string))))))
  :doc "source menu sends a note without installing plain-letter source bindings"
  (niwa-note-conversation-test--with-source
    (niwa-note-create "Menu question?")
    (should (eq #'self-insert-command (key-binding "s")))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?s)))
      (call-interactively #'niwa-note-menu))
    (should (= 1 (length sent)))
    (niwa-note-conversation-test--answer (car sent) "Sent through the menu."))
  :doc "a change prepared during the first real note request can be applied and reverted"
  (niwa-note-conversation-test--with-source
    (let* ((source-key (key-binding (kbd "C-c C-b")))
           (note (niwa-note-create "Improve this code"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view)))
      (should (= 1 (length sent)))
      (with-current-buffer data
        (should niwa--current-request)
        (niwa-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+improved code\n*** End Patch")))
      (should (equal "outside-before\nselected code\noutside-after\n"
                     (niwa-test--read-file file)))
      (niwa-note-conversation-test--answer (car sent) "A prepared improvement.")
      (with-current-buffer view
        (call-interactively (key-binding (kbd "C-c C-a"))))
      (should (equal "outside-before\nimproved code\noutside-after\n"
                     (niwa-test--read-file file)))
      (with-current-buffer source
        (goto-char (point-min))
        (should (eq source-key (key-binding (kbd "C-c C-b"))))
        (should-not (overlay-buffer note))
        (should (= 1 (length (niwa-notes-closed workspace)))))
      (with-current-buffer view
        (should (eq #'niwa-note-revert-change (key-binding (kbd "C-c C-b"))))
        (call-interactively (key-binding (kbd "C-c C-b"))))
      (ert-info ("source after reverting")
        (should (equal "outside-before\nselected code\noutside-after\n"
                       (niwa-test--read-file file))))
      (with-current-buffer view
        (should (string-search "A prepared improvement." (buffer-string)))))))

(niwa-deftest niwa-note-send/payload-encoding
  (:quiet t :doc "non-ASCII note input retains the sent UTF-8 bytes and displays decoded text")
  (niwa-note-conversation-test--with-source
    (niwa-define-preset note-test-unicode
      :description "No tool schemas" :system "Explain the note." :tools nil)
    (let ((note (niwa-note-create "Explain caf\u00e9 \u2013 please")))
      (overlay-put note 'niwa-note-settings '(:preset note-test-unicode))
      (let* ((view (niwa-note-send note))
             (data (buffer-local-value 'niwa--data-buffer view))
             (session (buffer-local-value 'niwa--session data))
             (directory (file-name-concat (niwa-session-save-path session) "note-input"))
             (file (car (directory-files directory t "\\.json\\'")))
             (payload (car sent-payloads)))
        (should-not (plist-get (plist-get (gptel-fsm-info (car sent)) :data) :tools))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file)
          (should (equal payload (buffer-string))))
        (with-current-buffer view
          (with-current-buffer (niwa-note-show-sent)
            (should (equal (decode-coding-string payload 'utf-8-unix) (buffer-string)))
            (should (string-search "caf\u00e9 \u2013" (buffer-string)))))
        (niwa-note-conversation-test--answer (car sent) "An answer.")))))

(niwa-deftest niwa-note-send/base-preset
  (:quiet t :doc "notes use the implementation base and keep isolation after a preset change")
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Explain this code"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data)))
      (should (eq 'niwa-implement (niwa-session-preset-name session)))
      (niwa-note-conversation-test--answer (car sent) "An answer.")
      (with-current-buffer data
        (niwa-define-preset note-test-base
          :description "Custom base" :system "CUSTOM-BASE-SYSTEM"
          :tools ((:tool "Read")))
        (niwa-preset-apply 'note-test-base)
        (should (equal "CUSTOM-BASE-SYSTEM" gptel-system-prompt))
        (should (equal '("Read") (mapcar #'gptel-tool-name gptel-tools)))
        (should-not gptel-use-context)
        (should (equal '(workspace-config memory environment tools skills resources)
                       (niwa-system-context-components session))))
      (with-current-buffer view
        (should niwa-note-conversation-mode)
        (niwa-view-composer-insert "Follow up")
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-view-send)))
      (should (= 2 (length sent)))
      (should (string-search "CUSTOM-BASE-SYSTEM"
                             (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
      (niwa-note-conversation-test--answer (car sent) "Custom answer.")
      (should (= 2 (length (directory-files
                            (file-name-concat (niwa-session-save-path session) "note-input")
                            nil "\\.json\\'")))))))

(defun niwa-note-conversation-test--displayed-prompt ()
  "Return the note composer's hint and prompt from their display cell."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (concat (get-text-property (point) 'display)
            (buffer-substring (1+ (point))
                              (niwa-view-composer-draft-start)))))

(niwa-deftest niwa-note-send/composer-hints
  (:quiet t :doc "state changes update dimmed hints without moving or editing the composer")
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Improve this code"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (composer (niwa-view-composer-buffer view))
           (draft "> active draft\nsecond line"))
      (niwa-note-conversation-test--answer (car sent) "An answer.")
      (with-current-buffer composer
        (should (equal "C-c C-z close\n> "
                       (niwa-note-conversation-test--displayed-prompt)))
        (niwa-view-test--insert-composer-draft draft))
      (when (display-graphic-p)
        (redisplay t)
        (sit-for 0.02)
        (redisplay t))
      (when-let* ((window (and (display-graphic-p) (get-buffer-window composer t))))
        (with-current-buffer composer
          (let ((rail (posn-at-point (point-min) window))
                (draft (posn-at-point (niwa-view-composer-draft-start) window)))
            (should rail)
            (should draft)
            (should (>= (- (cdr (posn-x-y draft)) (cdr (posn-x-y rail)))
                        (* 2 (frame-char-height (window-frame window))))))))
      (let ((selected (selected-window))
            (position (niwa-view-test--composer-point-offset view))
            (start (window-start (selected-window))))
        (with-current-buffer data
          (niwa-tool-patch-handler
           #'ignore
           '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+improved code\n*** End Patch")))
        (with-current-buffer composer
          (should (equal "C-c C-z close · C-c C-a apply · C-c C-g merge\n> "
                         (niwa-note-conversation-test--displayed-prompt)))
          (should (equal draft (niwa-view-composer-draft-text)))
          (should (= position (niwa-view-test--composer-point-offset view))))
        (should (eq selected (selected-window)))
        (should (= start (window-start selected))))
      (niwa-note-apply-change note)
      (with-current-buffer composer
        (should (equal "C-c C-z close\n> "
                       (niwa-note-conversation-test--displayed-prompt)))
        (should (equal draft (niwa-view-composer-draft-text)))))))

(niwa-deftest niwa-note-send/saved-settings
  (:quiet t :doc "the first real send uses the note's persisted choices, not the chat defaults")
  (niwa-note-conversation-test--with-source
    (cl-letf (((symbol-plist 'note-reasoner)
               '(:reasoning-effort (member low high))))
      (gptel-make-openai "Other" :key "test" :models '(note-reasoner))
      (niwa-define-preset note-test-base
        :description "Custom note base" :system "CUSTOM-FIRST-SYSTEM"
        :tools ((:tool "Read")))
      (let ((note (niwa-note-create "Explain this code")))
        (overlay-put note 'niwa-note-settings
                     '(:preset note-test-base :provider "Other:note-reasoner"
                       :effort high :permission-mode edits))
        (niwa-instruction-store-touch workspace)
        (niwa-instruction-store-flush t)
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (with-current-buffer source
          (goto-char (point-min)) (forward-line 1)
          (setq note (niwa-notes--at-point)))
        (let* ((view (niwa-note-send note))
               (data (buffer-local-value 'niwa--data-buffer view))
               (session (buffer-local-value 'niwa--session data)))
          (should (eq 'note-test-base (niwa-session-preset-name session)))
          (should (eq 'high (niwa-session-reasoning-effort session)))
          (should (eq 'edits (niwa-session-permission-mode session)))
          (with-current-buffer data
            (should (eq 'note-reasoner gptel-model))
            (should (equal "Other" (gptel-backend-name gptel-backend))))
          (should (string-search "CUSTOM-FIRST-SYSTEM"
                                 (gptel--json-encode
                                  (plist-get (gptel-fsm-info (car sent)) :data))))
          (niwa-note-conversation-test--answer (car sent) "Custom first answer."))))))

(niwa-deftest niwa-note-show-sent
  (:quiet t :doc "inputs are separate immutable files shown in dispatch order after reopening")
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Explain this code"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data))
           (directory (file-name-concat (niwa-session-save-path session) "note-input"))
           (payload (decode-coding-string (car sent-payloads) 'utf-8-unix)))
      (niwa-note-conversation-test--answer (car sent) "An answer.")
      (should (file-directory-p directory))
      (let* ((first (car (directory-files directory t "\\.json\\'")))
             (bytes (niwa-test--read-file first))
             (mtime (file-attribute-modification-time (file-attributes first))))
        (dotimes (index 10)
          (niwa-note-conversation-record-input session (list :round (1+ index))))
        (should (equal bytes (niwa-test--read-file first)))
        (should (equal mtime (file-attribute-modification-time (file-attributes first))))
        (should (= 11 (length (directory-files directory t "\\.json\\'"))))
        (should (= #o600 (logand #o777 (file-modes first)))))
      (with-current-buffer view
        (with-current-buffer (niwa-note-show-sent)
          (should (equal (concat payload "\n\n"
                                 (mapconcat (lambda (index)
                                              (gptel--json-encode (list :round (1+ index))))
                                            (number-sequence 0 9) "\n\n"))
                         (buffer-string))))))))

(niwa-deftest niwa-note-send/failed-first-send
  (:quiet t :doc "a failed or interrupted preset discards unlinked data and view buffers")
  (niwa-note-conversation-test--with-source
    (dolist (condition '(error quit))
      (let ((note (niwa-note-create "Explain this code"))
            (existing (buffer-list)))
        (setf (plist-get (alist-get 'niwa-implement gptel--known-presets) :pre)
              (lambda () (signal condition '("Preset refused first send"))))
        (condition-case err
            (progn (niwa-note-send note) (ert-fail "First send did not fail"))
          ((error quit) (should (eq condition (car err)))))
        (should-not (overlay-get note 'niwa-note-session-id))
        (should-not sent)
        (dolist (buffer (cl-set-difference (buffer-list) existing))
          (should-not (buffer-local-value 'niwa--session buffer))
          (should-not (buffer-local-value 'niwa--data-buffer buffer)))))))

(niwa-deftest niwa-rewind/note-session
  (:quiet t)
  (cl-letf (((symbol-function 'read-multiple-choice)
             (lambda (&rest _) '(?f "conversation and files"))))
    ,test)
  (test)
  :doc "Rewind reverses only its applied first turn, keeping another session and user edits"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Improve this code"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view)))
      (with-current-buffer data
        (niwa-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+first proposal\n*** End Patch")))
      (niwa-note-conversation-test--answer (car sent) "Ready to apply.")
      (niwa-note-apply-change note)
      (should (equal "outside-before\nfirst proposal\noutside-after\n"
                     (niwa-test--read-file file)))
      (with-current-buffer source
        (goto-char (point-max)) (forward-line -1)
        (let* ((other (niwa-note-create "Another conversation"))
               (other-view (niwa-note-send other))
               (other-data (buffer-local-value 'niwa--data-buffer other-view)))
          (with-current-buffer other-data
            (niwa-tool-patch-handler
             #'ignore
             '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-outside-after\n+other session\n*** End Patch")))
          (niwa-note-conversation-test--answer (car sent) "Another change.")
          (niwa-note-apply-change other)
          (with-current-buffer other-view (niwa-note-conversation-close))))
      (with-current-buffer source
        (goto-char (point-min)) (insert "user edit\n") (save-buffer))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (niwa-rewind)))
      (should (equal "user edit\noutside-before\nselected code\nother session\n"
                     (niwa-test--read-file file)))))
  :doc "Rewind before first turn preserves the current note and makes it sendable again"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Original request"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (id (overlay-get note 'niwa-note-session-id)))
      (with-current-buffer data
        (niwa-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+waiting proposal\n*** End Patch")))
      (niwa-note-conversation-test--answer (car sent) "A waiting change.")
      (niwa-note-edit "Current request" note)
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (niwa-rewind)))
      (with-current-buffer (niwa-view-composer-buffer view)
        (should (equal "C-c C-z close\n> "
                       (niwa-note-conversation-test--displayed-prompt))))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (with-current-buffer source
        (goto-char (point-min)) (forward-line 1)
        (setq note (niwa-notes--at-point))
        (should (equal "Current request" (overlay-get note 'niwa-note-text)))
        (should (= (point) (overlay-start note)))
        (should (eq 'unsent (overlay-get note 'niwa-note-state)))
        (should (equal id (overlay-get note 'niwa-note-session-id)))
        (should-not (overlay-get note 'niwa-note-prepared-change))
        (should-not (memq (niwa-notes-attention-state note) '(ready stale)))
        (niwa-note-send note))
      (should (= 2 (length sent)))
      (should (equal id (overlay-get note 'niwa-note-session-id)))
      (niwa-note-conversation-test--answer (car sent) "Sent again.")))
  :doc "sending a note without initial workspace storage still supports Rewind"
  (niwa-note-conversation-test--with-source
    (delete-directory (file-name-concat root ".niwa") t)
    (let* ((note (niwa-note-create "Ephemeral request"))
           (view (niwa-note-send note)))
      (niwa-note-conversation-test--answer (car sent) "An ephemeral answer.")
      (niwa-note-edit "Current ephemeral request" note)
      ;; Session startup establishes storage even outside a project.
      (should (file-exists-p (niwa-instruction-store-path workspace)))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (niwa-rewind)))
      (with-current-buffer source
        (goto-char (point-min)) (forward-line 1)
        (let ((restored (niwa-notes--at-point)))
          (should (eq 'unsent (overlay-get restored 'niwa-note-state)))
          (should (equal "Current ephemeral request" (overlay-get restored 'niwa-note-text)))))))
  :doc "batch Rewind unsends closed and unvisited notes without losing their anchors"
  (niwa-note-conversation-test--with-source
    (let* ((first (niwa-note-create "First"))
           (second (progn (forward-line 1) (niwa-note-create "Second")))
           (second-id (overlay-get second 'niwa-uuid))
           (other-file (file-name-concat root "other.txt"))
           (other-buffer (progn (write-region "unvisited code\n" nil other-file nil 'silent)
                                (find-file-noselect other-file)))
           (third (with-current-buffer other-buffer
                    (setq-local niwa--workspace workspace)
                    (niwa-note-create "Third")))
           (view (niwa-note-send-batch (list first second third))))
      (niwa-note-conversation-test--answer (car sent) "One batch.")
      (niwa-note-done second)
      (kill-buffer other-buffer)
      (should-not (find-buffer-visiting other-file))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (niwa-rewind)))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (let ((restored (niwa-note-restore second-id workspace)))
        (should (eq 'unsent (overlay-get restored 'niwa-note-state)))
        (should (equal "Second" (overlay-get restored 'niwa-note-text))))
      (with-current-buffer (find-file-noselect other-file)
        (setq-local niwa--workspace workspace)
        (goto-char (point-min))
        (let ((restored (niwa-notes--at-point)))
          (should (eq 'unsent (overlay-get restored 'niwa-note-state)))
          (should (equal "Third" (overlay-get restored 'niwa-note-text))))))))

(niwa-deftest niwa-note-send/taken
  (:quiet t)
  ,test
  (test)
  :doc "sending a taken note ends watching, including with an existing conversation"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Implement this myself"))
           (view (niwa-note-send (niwa-note-take note))))
      (should (eq 'sent (overlay-get note 'niwa-note-state)))
      (niwa-note-conversation-test--answer (car sent) "A retained answer.")
      (with-current-buffer view (niwa-note-conversation-close))
      (with-current-buffer source
        (niwa-note-take note)
        (should (eq view (niwa-note-send note)))
        (should (eq 'sent (overlay-get note 'niwa-note-state)))
        (should (= 1 (length sent)))
        (with-current-buffer view (niwa-note-conversation-close))
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (with-current-buffer source
          (goto-char (point-min)) (forward-line 1)
          (should (eq 'sent (overlay-get (niwa-notes--at-point)
                                        'niwa-note-state))))))))

(niwa-deftest niwa-note-send/buddy-ownership
  (:quiet t)
  ,test
  (test)
  :doc "send transfers a Buddy remark before creating its durable conversation"
  (niwa-note-conversation-test--with-source
    (require 'niwa-buddy-note)
    (let ((note (niwa-note-create "Fix what Buddy found")))
      (overlay-put note 'niwa-note-author 'buddy)
      (let ((view (niwa-note-send note)))
        (should (eq 'user (overlay-get note 'niwa-note-author)))
        (should (overlay-get note 'niwa-note-kept-from-buddy))
        (niwa-note-conversation-test--answer (car sent) "Fixed in this conversation.")
        (with-current-buffer view
          (should (string-search "Fixed in this conversation." (buffer-string)))))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (let ((restored (with-current-buffer source (niwa-notes--at-point))))
        (should (eq 'user (overlay-get restored 'niwa-note-author)))
        (should (overlay-get restored 'niwa-note-kept-from-buddy))
        (should (overlay-get restored 'niwa-note-session-id))))))

(niwa-deftest niwa-inspect-effective-prompt/note
  (:quiet t :doc "note conversations expose their profile and ordered prompt components")
  (niwa-note-conversation-test--with-source
    (let ((view (niwa-note-send (niwa-note-create "Explain this code"))))
      (niwa-note-conversation-test--answer (car sent) "An answer.")
      (with-current-buffer view
        (with-current-buffer (niwa-inspect-effective-prompt)
          (should (string-search "Profile: main" (buffer-string)))
          (should (string-search "** workspace-config" (buffer-string)))
          (should-not (string-search "component breakdown unavailable" (buffer-string))))))))

(niwa-deftest niwa/default-preset
  (:quiet t :doc "a removed saved default falls back to the implement preset for a new chat")
  (niwa-note-conversation-test--with-source
    (let ((niwa-default-chat-preset 'discuss))
      (call-interactively #'niwa)
      (let* ((view (window-buffer (selected-window)))
             (data (buffer-local-value 'niwa--data-buffer view)))
        (should (eq 'niwa-implement
                    (niwa-session-preset-name (buffer-local-value 'niwa--session data))))))))

(niwa-deftest niwa-note-send/chat-selection-isolation
  (:quiet t)
  ,test
  (test)
  :doc "ordinary chat entry never adopts a sent note's dedicated conversation"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "A dedicated question"))
           (note-view (niwa-note-send note)))
      (niwa-note-conversation-test--answer (car sent) "Only this note's answer")
      (with-current-buffer note-view (niwa-note-conversation-close))
      (with-current-buffer source
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Note session offered in ordinary chat chooser"))))
          (call-interactively #'niwa)))
      (should-not (eq note-view (window-buffer (selected-window))))
      (with-current-buffer source
        (should (eq note-view (niwa-note-send note))))))
  :doc "ordinary saved selection excludes several saved note conversations"
  (niwa-note-conversation-test--with-source
    (dotimes (_ 3)
      (let ((view (with-current-buffer source
                    (goto-char (point-min))
                    (forward-line 1)
                    (niwa-note-send (niwa-note-create "Dedicated question")))))
        (niwa-note-conversation-test--answer (car sent) "Retained answer")
        (let ((data (buffer-local-value 'niwa--data-buffer view)))
          (with-current-buffer data (niwa-session-persistence-release-on-kill))
          (niwa-chat-discard-buffers data t))))
    (with-current-buffer source
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Saved note session offered in ordinary chooser"))))
        (call-interactively #'niwa))))
  :doc "an explicit fork remains an ordinary discoverable chat with a changeable preset"
  (niwa-note-conversation-test--with-source
    (let* ((view (niwa-note-send (niwa-note-create "Forkable question")))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data)))
      (niwa-note-conversation-test--answer (car sent) "Forkable answer")
      (let* ((target (cadr (assq 1 (niwa-session-prompt-index session))))
             (child (niwa-session-fork-create data target 'conversation)))
        (with-current-buffer view (niwa-note-conversation-close))
        (with-current-buffer source
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt table &rest _)
                       (let ((choices (delete "Start new session" (all-completions "" table))))
                         (should (= 1 (length choices)))
                         (should (string-search
                                  (niwa-session-session-id
                                   (buffer-local-value 'niwa--session child))
                                  (car choices)))
                         (car choices)))))
            (call-interactively #'niwa)))
        (should (eq child (buffer-local-value 'niwa--data-buffer
                                             (window-buffer (selected-window)))))
        (with-current-buffer child
          (niwa-preset-apply 'niwa-implement)
          (should (eq 'niwa-implement (niwa-session-preset-name niwa--session))))))))

(niwa-deftest niwa-session-persistence-cleanup-expired/note
  (:quiet t)
  ,test
  (test)
  :doc "closed note sessions survive even an unreadable store and do not displace the ordinary retention floor"
  (niwa-note-conversation-test--with-source
    (let* ((ordinary (niwa--chat-buffer "ordinary" t workspace))
           (ordinary-session (buffer-local-value 'niwa--session ordinary))
           (note (niwa-note-create "Retain my conversation"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data)))
      (niwa-note-conversation-test--answer (car sent) "Durable answer")
      (with-current-buffer ordinary
        (niwa--insert-local-user-turn "An ordinary retained chat")
        (niwa-session-artifacts-save ordinary-session ordinary nil t))
      (with-current-buffer source (niwa-note-done note))
      (niwa-instruction-store-flush t)
      (dolist (buffer (list ordinary data))
        (with-current-buffer buffer (niwa-session-persistence-release-on-kill))
        (niwa-chat-discard-buffers buffer t))
      (let* ((path (niwa-session-artifacts-sidecar-path
                    (niwa-session-save-path ordinary-session)))
             (sidecar (niwa-session-codec-read path)))
        (niwa-session-codec-write path (plist-put sidecar :updated-at "2001-01-01T00-00-00")))
      (write-region "unreadable instruction store" nil
                    (file-name-concat root ".niwa" "instructions.el") nil 'silent)
      (let ((niwa-session-max-age-days 0)
            (niwa-session-keep-recent-count 1))
        (should (zerop (niwa-session-persistence-cleanup-expired workspace t))))
      (should (file-directory-p (niwa-session-save-path session)))
      (should (file-directory-p (niwa-session-save-path ordinary-session))))))

(niwa-deftest niwa-session-persistence-cleanup-expired/note-expiry
  (:quiet t)
  ,test
  (test)
  :doc "an expired closed note releases its conversation after reload without using the ordinary retention floor"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Collect after expiry"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data))
           (path (niwa-session-save-path session)))
      (niwa-note-conversation-test--answer (car sent) "An expiring answer")
      (with-current-buffer source (niwa-note-done note))
      (let ((niwa-notes-closed-max-count 0))
        (should-not (niwa-notes-closed workspace)))
      (niwa-instruction-store-flush t)
      (with-current-buffer data (niwa-session-persistence-release-on-kill))
      (niwa-chat-discard-buffers data t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (let ((niwa-session-max-age-days 0)
            (niwa-session-keep-recent-count 10)
            (read-file (symbol-function 'niwa-session-control-fs-read-file))
            (store (file-name-concat root ".niwa" "instructions.el"))
            (reads 0))
        (cl-letf (((symbol-function 'niwa-session-control-fs-read-file)
                   (lambda (file &optional coding max-bytes)
                     (when (equal file store)
                       (cl-incf reads)
                       (should (<= max-bytes (1+ (* 16 1024 1024)))))
                     (funcall read-file file coding max-bytes))))
          (should (= 1 (niwa-session-persistence-cleanup-expired workspace t))))
        (should (= 1 reads)))
      (should-not (file-directory-p path)))))

(niwa-deftest niwa-session-persistence-delete/note-live-and-stashed
  (:quiet t)
  ,test
  (test)
  :doc "deletion and cleanup retain dirty live and killed-buffer references absent from the disk store"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Still in use"))
           (store (file-name-concat root ".niwa" "instructions.el"))
           (unsent (with-temp-buffer (insert-file-contents store) (buffer-string)))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data))
           (path (niwa-session-save-path session))
           (niwa-session-max-age-days 0)
           (niwa-session-keep-recent-count 0))
      (niwa-note-conversation-test--answer (car sent) "A retained answer")
      (with-current-buffer data (niwa-session-persistence-release-on-kill))
      (niwa-chat-discard-buffers data t)
      ;; Another store version lacks the local unsaved session reference.
      (should (eq source (overlay-buffer note)))
      (write-region unsent nil store nil 'silent)
      (niwa-instruction-store-touch workspace)
      (let ((dirty (copy-sequence niwa-instruction-store--dirty)))
        (should-not (niwa-session-persistence-delete workspace path))
        (should (= 0 (niwa-session-persistence-cleanup-expired workspace t)))
        (should (equal dirty niwa-instruction-store--dirty)))
      (kill-buffer source)
      (should-not (overlay-buffer note))
      (write-region unsent nil store nil 'silent)
      (should-not (niwa-session-persistence-delete workspace path))
      (should (= 0 (niwa-session-persistence-cleanup-expired workspace t)))
      (should (file-directory-p path)))))

(niwa-deftest niwa-note-restore/collected-session
  (:quiet t)
  ,test
  (test)
  :doc "a closed note survives cleanup after reload and reopens its saved conversation"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Restore with conversation"))
           (uuid (overlay-get note 'niwa-uuid))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data))
           (path (niwa-session-save-path session)))
      (niwa-note-conversation-test--answer (car sent) "An answer after restoration")
      (with-current-buffer source (niwa-note-done note))
      (niwa-instruction-store-flush t)
      (with-current-buffer data (niwa-session-persistence-release-on-kill))
      (niwa-chat-discard-buffers data t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (let ((niwa-session-max-age-days 0)
            (niwa-session-keep-recent-count 0))
        (should (= 0 (niwa-session-persistence-cleanup-expired workspace t))))
      (should-not (niwa-session-persistence-delete workspace path))
      (with-current-buffer source
        (setq view (niwa-note-send (niwa-note-restore uuid workspace))))
      (should (= 1 (length sent)))
      (niwa-test--await 5 "Restored note answer did not render"
        (progn
          ;; Restored views defer rendering until redisplay observes their window.
          (redisplay t)
          (with-current-buffer view
            (string-search "An answer after restoration" (buffer-string))))))))

(niwa-deftest niwa-session-persistence-cleanup-expired/note-uncertainty
  (:quiet t)
  ,test
  (test)
  :doc "an otherwise collectable conversation survives corrupt, unsupported, absent and unloaded stores"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Fail safe"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data))
           (path (niwa-session-save-path session))
           (store (file-name-concat root ".niwa" "instructions.el"))
           (niwa-session-max-age-days 0)
           (niwa-session-keep-recent-count 0))
      (niwa-note-conversation-test--answer (car sent) "Keep when uncertain")
      (with-current-buffer source (niwa-note-done note))
      (let ((niwa-notes-closed-max-count 0))
        (should-not (niwa-notes-closed workspace)))
      (niwa-instruction-store-flush t)
      (with-current-buffer data (niwa-session-persistence-release-on-kill))
      (niwa-chat-discard-buffers data t)
      (let ((valid (with-temp-buffer (insert-file-contents store) (buffer-string))))
        (dolist (contents '("broken store" "(:format 999)" "(:format 1)" nil))
          (if contents
              (write-region contents nil store nil 'silent)
            (delete-file store))
          (should (= 0 (niwa-session-persistence-cleanup-expired workspace t)))
          (should-not (niwa-session-persistence-delete workspace path))
          (should (file-directory-p path)))
        (write-region valid nil store nil 'silent)
        (niwa--clear-instruction-state workspace)
        (should (= 0 (niwa-session-persistence-cleanup-expired workspace t)))
        (should-not (niwa-session-persistence-delete workspace path))
        (niwa-instruction-store-observe workspace)
        (should (niwa-session-persistence-delete workspace path))
        (should-not (file-directory-p path))))))

(niwa-deftest niwa-session-persistence-cleanup-expired/note-remote
  (:quiet t)
  ,test
  (test)
  :doc "a remote reference check retains the conversation without probing its store"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Remote reference safety"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data))
           (path (niwa-session-save-path session))
           (remote (copy-niwa-workspace workspace))
           (probes 0))
      (niwa-note-conversation-test--answer (car sent) "No remote store I/O")
      (with-current-buffer data (niwa-session-persistence-release-on-kill))
      (niwa-chat-discard-buffers data t)
      (setf (niwa-workspace-root remote) "/notes-remote:/project/")
      (let* ((handler (lambda (operation &rest _)
                        (if (eq operation 'file-remote-p)
                            "/notes-remote:"
                          (cl-incf probes)
                          (error "Unexpected remote probe"))))
             (file-name-handler-alist
              (cons (cons "\\`/notes-remote:" handler) file-name-handler-alist))
             (niwa-session-max-age-days 0)
             (niwa-session-keep-recent-count 0))
        (should (= 0 (niwa-session-persistence-cleanup-expired remote t)))
        (should-not (niwa-session-persistence-delete remote path)))
      (should (= 0 probes))
      (should (file-directory-p path)))))

(niwa-deftest niwa-session-persistence-cleanup-expired/note-fork
  (:quiet t)
  ,test
  (test)
  :doc "an explicit note-session fork follows ordinary cleanup while its original stays referenced"
  (niwa-note-conversation-test--with-source
    (let* ((view (niwa-note-send (niwa-note-create "Fork and collect")))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data)))
      (niwa-note-conversation-test--answer (car sent) "Forked answer")
      (let* ((target (cadr (assq 1 (niwa-session-prompt-index session))))
             (child (niwa-session-fork-create data target 'conversation))
             (child-session (buffer-local-value 'niwa--session child))
             (child-path (niwa-session-save-path child-session)))
        (dolist (buffer (list data child))
          (with-current-buffer buffer (niwa-session-persistence-release-on-kill))
          (niwa-chat-discard-buffers buffer t))
        (let ((niwa-session-max-age-days 0)
              (niwa-session-keep-recent-count 0))
          (should (= 1 (niwa-session-persistence-cleanup-expired workspace t))))
        (should-not (file-directory-p child-path))
        (should (file-directory-p (niwa-session-save-path session)))))))

(niwa-deftest niwa-session-persistence-cleanup-expired/note-missing-metadata
  (:quiet t)
  ,test
  (test)
  :doc "corrupt or missing session metadata cannot erase a referenced conversation"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Keep despite metadata loss"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (session (buffer-local-value 'niwa--session data))
           (path (niwa-session-save-path session))
           (sidecar (niwa-session-artifacts-sidecar-path path))
           (niwa-session-max-age-days 0)
           (niwa-session-keep-recent-count 0))
      (niwa-note-conversation-test--answer (car sent) "A recoverable answer")
      (with-current-buffer source (niwa-note-done note))
      (niwa-instruction-store-flush t)
      (with-current-buffer data (niwa-session-persistence-release-on-kill))
      (niwa-chat-discard-buffers data t)
      (dolist (contents '("(" "(:version \"unknown\")"
                         "(:note-conversation t)" nil))
        (if contents
            (write-region contents nil sidecar nil 'silent)
          (delete-file sidecar))
        (should-not (niwa-session-persistence-delete workspace path))
        (should (= 0 (niwa-session-persistence-cleanup-expired workspace t)))
        (should (file-directory-p path)))
      (let ((niwa-notes-closed-max-count 0))
        (should-not (niwa-notes-closed workspace)))
      (niwa-instruction-store-flush t)
      ;; A recently settled session can still receive control-program telemetry.
      (let ((niwa-telemetry-enabled t)
            (niwa-telemetry--lag-windows (list (list session)))
            (niwa-session-control-fs--measure-min-ms 0))
        (should (= 1 (niwa-session-persistence-cleanup-expired workspace t)))
        (should-not (file-directory-p path))))))

(niwa-deftest niwa-session-persistence-cleanup-expired/ordinary-no-note-read
  (:quiet t)
  ,test
  (test)
  :doc "ordinary cleanup and deletion add no instruction-store reads"
  (niwa-note-conversation-test--with-source
    (let ((store (file-name-concat root ".niwa" "instructions.el"))
          (read-file (symbol-function 'niwa-session-control-fs-read-file))
          (reads 0)
          paths)
      (dotimes (n 2)
        (let* ((data (niwa--chat-buffer (format "ordinary-%d" n) t workspace))
               (session (buffer-local-value 'niwa--session data)))
          (with-current-buffer data
            (niwa--insert-local-user-turn "Ordinary conversation")
            (niwa-session-artifacts-save session data nil t)
            (niwa-session-persistence-release-on-kill))
          (push (niwa-session-save-path session) paths)
          (niwa-chat-discard-buffers data t)))
      (cl-letf (((symbol-function 'niwa-session-control-fs-read-file)
                 (lambda (file &optional coding max-bytes)
                   (when (equal file store) (cl-incf reads))
                   (funcall read-file file coding max-bytes))))
        (should (niwa-session-persistence-delete workspace (pop paths)))
        (let ((niwa-session-max-age-days 0)
              (niwa-session-keep-recent-count 0))
          (should (= 1 (niwa-session-persistence-cleanup-expired workspace t)))))
      (should (= 0 reads)))))

(niwa-deftest niwa-chat-discard-buffers/note-source
  (:quiet t)
  ,test
  (test)
  :doc "discarding a note conversation leaves its visited source note live and unstashed"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Keep the source label"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view)))
      (niwa-note-conversation-test--answer (car sent) "Only the conversation closes")
      (should (eq source (overlay-buffer note)))
      (niwa-chat-discard-buffers data t)
      (should-not (buffer-live-p data))
      (should-not (buffer-live-p view))
      (should (buffer-live-p source))
      (should (eq source (overlay-buffer note)))
      (let ((entries (plist-get (gethash (niwa--instruction-workspace-key workspace)
                                        niwa--instruction-states)
                                :instructions)))
        (should (memq note (cdr (assq source entries))))
        (should-not (assoc file entries)))
      (with-current-buffer source
        (should (equal "outside-before\nselected code\noutside-after\n" (buffer-string)))))))

(niwa-deftest niwa-note-send/frame-reuse
  (:quiet t)
  ,test
  (test)
  :doc "a graphical frame reused from another conversation shows the note answer"
  (progn
   (skip-unless (display-graphic-p))
  (niwa-note-conversation-test--with-source
    (let* ((prior (save-excursion
                    (forward-line -1)
                    (niwa-note-create "Prior note")))
           (old-data (niwa--chat-buffer "prior" t workspace))
           (old-view (buffer-local-value 'niwa--view-buffer old-data)))
      (unwind-protect
          (progn
            (niwa-view-test--insert-data old-data "Prior question\n\n" nil)
            (niwa-view-test--insert-data old-data "Prior answer\n" 'response)
            (with-current-buffer old-view (niwa-view--full-rerender))
            (niwa-note-frame-display prior old-view)
            (let* ((frame niwa-note-frame--frame)
                   (old-draft (niwa-view-composer-buffer old-view))
                   (old-dock (get-buffer-window old-draft frame)))
              (set-frame-selected-window frame old-dock)
              (let ((view (niwa-note-send (niwa-note-create "New note question"))))
                (should (frame-live-p frame))
                (should (eq frame niwa-note-frame--frame))
                (should (eq view (window-buffer (selected-window))))
                (should-not (get-buffer-window old-draft frame))
                (should (get-buffer-window (niwa-view-composer-buffer view) frame))
                (niwa-note-conversation-test--answer (car sent) "Visible note answer")
                (with-current-buffer view
                  (goto-char (point-min))
                  (should (search-forward "Visible note answer" nil t))
                  (should-not (invisible-p (1- (point))))))))
        (when (frame-live-p niwa-note-frame--frame)
          (niwa-note-frame-close)))))))

(niwa-deftest niwa-note-send/edited-file-isolation
  (:quiet t)
  ,test
  (test)
  :doc "send does not inject unrelated changed files remembered by another chat"
  (niwa-note-conversation-test--with-source
    (let ((other (file-name-concat root "other.txt")))
      (write-region "old unrelated content" nil other nil 'silent)
      (niwa-file-cache-put (niwa-workspace-file-cache workspace)
                              (niwa-file-state-from-file other))
      (write-region "UNRELATED-EDIT-SECRET" nil other nil 'silent)
      (set-file-times other (time-add (current-time) 2))
      (niwa-note-send (niwa-note-create "Only this selected code"))
      (niwa-test--await 5 "Note request did not reach transport" sent)
      (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should-not (string-search "UNRELATED-EDIT-SECRET" payload))
        (should-not (string-search "other.txt" payload)))
      (niwa-note-conversation-test--answer (car sent) "Only this note."))))

(niwa-deftest niwa-note-send/mentioned-notes
  (:quiet t)
  ,test
  (test)
  :doc "authored mentions freeze once; captured code and attached text cannot activate more notes"
  (niwa-note-conversation-test--with-source
    (forward-line 1)
    (let ((unmentioned (niwa-note-create "UNMENTIONED-TEXT"))
          (secret (file-name-concat root "not-requested.txt")))
      (write-region "UNREQUESTED-FILE-SECRET" nil secret nil 'silent)
      (goto-char (point-min))
      (let ((context (niwa-note-create
                      (format "MENTIONED-TEXT @note:%s @file:%s"
                              (overlay-get unmentioned 'niwa-id) secret))))
        (forward-line 1)
        (end-of-line)
        (insert (format " @note:%s @file:%s" (overlay-get unmentioned 'niwa-id) secret))
        (beginning-of-line)
        (let* ((note (niwa-note-create
                      (format "Explain @note:%s" (overlay-get context 'niwa-id))))
               (view (niwa-note-send note))
               (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
          (should (string-search "MENTIONED-TEXT" payload))
          (should (string-search "outside-before" payload))
          (should-not (string-search "UNMENTIONED-TEXT" payload))
          (should-not (string-search "UNREQUESTED-FILE-SECRET" payload))
          (niwa-note-conversation-test--answer (car sent) "Frozen answer")
          (with-current-buffer source
            (niwa-note-edit "CHANGED-MENTION-CONTENT" context)
            (save-buffer)
            (niwa-instruction-store-flush t))
          (let ((data (buffer-local-value 'niwa--data-buffer view)))
            (with-current-buffer data (niwa-session-persistence-release-on-kill))
            (niwa-chat-discard-buffers data t))
          (kill-buffer source)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local niwa--workspace workspace)
            (niwa-instruction-store-observe workspace)
            (goto-char (point-min))
            (forward-line 1)
            (setq view (niwa-note-send)))
          (with-current-buffer (niwa-view-composer-buffer view)
            (niwa-view-composer-insert "Follow up without a new mention")
            (call-interactively (key-binding (kbd "C-c C-c"))))
          (let ((next (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
            (should (string-search "MENTIONED-TEXT" next))
            (should-not (string-search "CHANGED-MENTION-CONTENT" next))
            (should-not (string-search "UNREQUESTED-FILE-SECRET" next))
            (should-not (string-search "UNMENTIONED-TEXT" next)))
          (niwa-note-conversation-test--answer (car sent) "Still frozen"))))))

(niwa-deftest niwa-note-send/literal-code
  (:quiet t)
  ,test
  (test)
  :doc "freezing does not alter ordinary at-sign decorators in captured code"
  (niwa-note-conversation-test--with-source
    (insert "@property @Override @param @media ")
    (beginning-of-line)
    (niwa-note-send (niwa-note-create "Explain these annotations"))
    (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
      (should (string-search "@property @Override @param @media selected code" payload)))
    (niwa-note-conversation-test--answer (car sent) "Annotations retained")))

(niwa-deftest niwa-note-send/request-mentions
  (:quiet t)
  ,test
  (test)
  :doc "authored file media uses request-time expansion without persistent gptel context"
  (niwa-note-conversation-test--with-source
    (let ((image (file-name-concat root "image.png")))
      (write-region "note image bytes" nil image nil 'silent)
      (cl-letf (((symbol-plist 'test-model)
                 '(:capabilities (media) :mime-types ("image/png"))))
        (let* ((note (niwa-note-create (concat "Explain @file:" image)))
               (view (niwa-note-send note))
               (data (buffer-local-value 'niwa--data-buffer view))
               (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
          (should (string-search "data:image/png;base64," payload))
          (with-current-buffer data
            (should-not gptel-context)
            (should-not gptel-use-context))
          (niwa-note-conversation-test--answer (car sent) "Image explained"))))))

(niwa-deftest niwa-notes-list-send
  (:quiet t)
  ,test
  (test)
  :doc "list send focuses the note conversation and projects working then sent without a second request"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Send from the list"))
           (uuid (overlay-get note 'niwa-uuid))
           (list-buffer (niwa-notes-list workspace))
           view)
      (with-current-buffer list-buffer
        (goto-char (point-min))
        (while (and (not (eobp)) (not (equal uuid (tabulated-list-get-id)))) (forward-line 1))
        (should (equal uuid (tabulated-list-get-id)))
        (should (commandp (key-binding "s")))
        (setq view (call-interactively (key-binding "s"))))
      (should (eq view (window-buffer (selected-window))))
      (with-current-buffer list-buffer
        (niwa-notes-list-refresh)
        (should (string-search "working" (buffer-string))))
      (niwa-note-conversation-test--answer (car sent) "List answer")
      (with-current-buffer list-buffer
        (niwa-notes-list-refresh)
        (should-not (string-search "working" (buffer-string)))
        (goto-char (point-min))
        (while (and (not (eobp)) (not (equal uuid (tabulated-list-get-id)))) (forward-line 1))
        (should (equal uuid (tabulated-list-get-id)))
        (should (eq view (call-interactively (key-binding "s")))))
      (should (= 1 (length sent))))))

(niwa-deftest niwa-note-send-batch
  (:quiet t)
  ,test
  (test)
  :doc "batch sends two notes into one durable conversation and either note reopens it"
  (niwa-note-conversation-test--with-source
    (let ((first (niwa-note-create "First question")))
      (forward-line 1)
      (let* ((second (niwa-note-create "Related question"))
             (view (niwa-note-send-batch (list first second)))
             (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should (= 1 (length sent)))
        (dolist (text '("First question" "Related question" "selected code" "outside-after"))
          (should (string-search text payload)))
        (niwa-note-conversation-test--answer (car sent) "One answer for both.")
        (with-current-buffer source
          (should (eq view (niwa-note-send first)))
          (should (eq view (niwa-note-send second)))
          (should (eq view (niwa-note-send-batch (list first second)))))
        (should (= 1 (length sent))))))
  :doc "interactive batch choices keep comma prose in annotations, not candidate IDs"
  (niwa-note-conversation-test--with-source
    (let ((note (niwa-note-create "Rename foo, then update callers")))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_prompt collection &rest _)
                   (let ((id (number-to-string (overlay-get note 'niwa-id))))
                     (should (equal (list id) (all-completions "" collection)))
                     (should (string-search
                              "Rename foo, then update callers"
                              (funcall (plist-get completion-extra-properties :annotation-function) id)))
                     (list id)))))
        (let ((view (call-interactively #'niwa-note-send-batch)))
          (should (buffer-live-p view))
          (should (= 1 (length sent)))
          (niwa-note-conversation-test--answer (car sent) "Renaming advice.")))))
  :doc "batch refuses mixing existing conversations without losing either thread"
  (niwa-note-conversation-test--with-source
    (let* ((first (niwa-note-create "First"))
           (first-view (niwa-note-send first))
           second second-view)
      (niwa-note-conversation-test--answer (car sent) "First answer")
      (with-current-buffer source
        (forward-line 1)
        (setq second (niwa-note-create "Second")
              second-view (niwa-note-send second)))
      (niwa-note-conversation-test--answer (car sent) "Second answer")
      (with-current-buffer source
        (should-error (niwa-note-send-batch (list first second)) :type 'user-error)
        (should (eq first-view (niwa-note-send first)))
        (should (eq second-view (niwa-note-send second)))))))

(niwa-deftest niwa-note-conversation-reply
  (:quiet t)
  ,test
  (test)
  :doc "one-key reply uses its own history and background answer preserves the next draft"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Explain this?"))
           (view (niwa-note-send note)))
      (niwa-note-conversation-test--answer (car sent) "First answer.")
      (with-current-buffer (niwa-view-composer-buffer view)
        (niwa-view-composer-insert "And why?")
        (call-interactively (key-binding (kbd "C-c C-c"))))
      (should (= 2 (length sent)))
      (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should (string-search "First answer." payload))
        (should (string-search "And why?" payload)))
      (with-current-buffer view
        (niwa-view-composer-insert "> multiline draft\nsecond line")
        (let ((position (niwa-view-test--composer-point-offset))
              (start (window-start))
              (window (selected-window)))
          (niwa-note-conversation-test--answer (car sent) "Second answer.")
          (should (eq window (selected-window)))
          (should (= position (niwa-view-test--composer-point-offset)))
          (should (= start (window-start)))
          (should (equal "> multiline draft\nsecond line"
                         (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa-note-conversation-restart
  (:quiet t)
  ,test
  (test)
  :doc "a fresh Emacs reopens the saved note's session and original sent input"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Question before restart?"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (script (file-name-concat root "restart.el")))
      (niwa-note-conversation-test--answer (car sent) "Answer survives restart.")
      (with-current-buffer data (niwa-session-persistence-release-on-kill))
      (niwa-chat-discard-buffers data t)
      (write-region
       (concat ";;; restart.el -*- lexical-binding: t -*-\n" (prin1-to-string
        `(progn
           (setq load-path ',load-path)
           (require 'niwa)
           (require 'niwa-notes-activation)
           (niwa-notes-global-mode 1)
           (require 'gptel-openai)
           (require 'niwa-persistence)
           (setq gptel-backend (gptel-make-openai "Notes" :key "test" :models '(test-model))
                 gptel-model 'test-model
                 niwa-sessions-directory ,niwa-sessions-directory)
           (niwa-install)
           (let* ((workspace (niwa-workspace-get-or-create 'file ,root ,root "notes"))
                  (source (find-file-noselect ,file)))
             (switch-to-buffer source)
             (setq-local niwa--workspace workspace)
             (niwa-instruction-store-observe workspace)
             (goto-char (point-min))
             (forward-line 1)
             (let ((view (niwa-note-send)))
               (with-current-buffer view
                 (let ((deadline (+ (float-time) 5)))
                   (while (and (not (string-search "Answer survives restart." (buffer-string)))
                               (< (float-time) deadline))
                     (accept-process-output nil 0.01)))
                 (unless (string-search "Answer survives restart." (buffer-string))
                   (error "Lost note answer"))
                 (let ((report (call-interactively (key-binding (kbd "C-c C-v")))))
                   (with-current-buffer report
                     (unless (string-search "Question before restart?" (buffer-string))
                       (error "Lost sent input")))))
               (princ "NOTE-RESTART-PASSED")))
           (kill-emacs 0))))
       nil script nil 'silent)
      (with-temp-buffer
        (let ((default-directory root))
          (let ((status (call-process (expand-file-name invocation-name invocation-directory)
                                      nil (current-buffer) nil "-Q" "--batch" "-l" script)))
            (ert-info ((buffer-string)) (should (equal 0 status)))))
        (should (string-search "NOTE-RESTART-PASSED" (buffer-string)))))))

(niwa-deftest niwa-note-send/native
  (:quiet t)
  ,test
  (test)
  :doc "native note session retains its exact initial context including ordinary capabilities"
  (niwa-note-conversation-test--with-source
    (let ((niwa-claude-code-directory (file-name-concat root "claude"))
          (acp--shutdown-grace-period 0)
          system prompts view data)
      (make-directory (file-name-concat root ".niwa" "memory") t)
      (write-region "NATIVE-MEMORY-SECRET" nil
                    (file-name-concat root ".niwa" "memory" "MEMORY.md") nil 'silent)
      (cl-letf (((symbol-function 'niwa-claude-code-unavailable-reason) #'ignore)
                ((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (prompt &rest _)
                    (setq system prompt)
                    (list :meta '((echoAllText . t)))))))
        (niwa-claude-code-register)
        (let ((gptel-backend (gptel-get-backend "Claude Code"))
              (gptel-model 'sonnet))
          (advice-add 'niwa-acp-prompt :before
                      (lambda (_client input &rest _) (push input prompts))
                      '((name . niwa-note-native-test)))
          (unwind-protect
              (progn
                (setq view (niwa-note-send (niwa-note-create "Native question?"))
                      data (buffer-local-value 'niwa--data-buffer view))
                (niwa-test--await 10 "Native note did not settle"
                  (not (niwa-turn-busy-p data)))
                (should prompts)
                (should (string-search "Native question?" (format "%S" prompts)))
                (dolist (text '("NATIVE-MEMORY-SECRET" "## Skills" "## Resources"))
                  (should (string-search text (format "%S" prompts))))
                (with-current-buffer view
                  (let* ((report (call-interactively (key-binding (kbd "C-c C-v"))))
                         (payload (with-current-buffer report
                                    (json-parse-string (buffer-string) :object-type 'alist))))
                    (should (equal system (alist-get 'system payload)))
                    (should (equal (car prompts) (alist-get 'input payload))))))
            (advice-remove 'niwa-acp-prompt 'niwa-note-native-test)))))))

(niwa-deftest niwa-note-preparation-during-turn
  (:quiet t)
  ,test
  (test)
  :doc "a disjoint saved edit during the AI turn survives preparation and later apply"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Improve this code"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           result)
      (should (niwa-turn-busy-p data))
      (with-current-buffer source
        (goto-char (point-min))
        (insert "my ")
        (save-buffer))
      (with-current-buffer data
        (niwa-tool-patch-handler
         (lambda (value) (setq result value))
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+prepared proposal\n*** End Patch")))
      (should (string-search "Prepared" (plist-get result :result)))
      (niwa-note-conversation-test--answer (car sent) "The change is ready when you are.")
      (should-not (niwa-turn-busy-p data))
      (should (eq 'ready (niwa-notes-attention-state note)))
      (with-current-buffer view
        (call-interactively (key-binding (kbd "C-c C-a"))))
      (should (equal "my outside-before\nprepared proposal\noutside-after\n"
                     (niwa-test--read-file file))))))

(niwa-deftest niwa-tool-note-write/rewind (:quiet t)
  ,test
  (test)
  :doc "a suggestion created during the active note request belongs to its discarded turn"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Review this code and leave a suggestion"))
           (note-uuid (overlay-get note 'niwa-uuid))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           outcome suggestion suggestion-uuid)
      (should (niwa-turn-busy-p data))
      (with-current-buffer data
        (niwa-pipeline-run-tool-outcome
         (niwa-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
         (list :operations
               (vector (list :action "create" :file_path file :start_line 2
                             :text "A suggested fix" :prepared_change
                             "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch"))))
        (niwa-test--await 5 "NoteWrite suggestion did not settle" outcome))
      (ert-info ((format "%S" outcome))
        (should (eq 'success (plist-get outcome :status))))
      (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get outcome :result)))
      (setq suggestion-uuid (match-string 1 (plist-get outcome :result))
            suggestion (niwa-note-resolve suggestion-uuid workspace))
      (niwa-note-conversation-test--answer (car sent) "The suggestion is ready.")
      (niwa-note-apply-change suggestion)
      (should (equal "outside-before\nsuggested code\noutside-after\n"
                     (niwa-test--read-file file)))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _) (car (all-completions "" collection))))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?f "conversation and files")))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (niwa-rewind)))
      (should (equal "outside-before\nselected code\noutside-after\n"
                     (niwa-test--read-file file)))
      (setq note (niwa-note-resolve note-uuid workspace))
      (with-current-buffer source
        (should (= 1 (length (niwa--instructions-in (point-min) (point-max))))))
      (should-not (niwa-note-resolve suggestion-uuid workspace))
      (niwa-note-send note)
      (with-current-buffer data
        (niwa-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+new turn code\n*** End Patch")))
      (niwa-note-conversation-test--answer (car sent) "A different change in the reused turn.")
      (niwa-note-apply-change (niwa-note-resolve note-uuid workspace))
      (should (equal "outside-before\nnew turn code\noutside-after\n"
                     (niwa-test--read-file file)))
      (should-error (niwa-note-revert-change suggestion-uuid nil workspace)
                    :type 'user-error)
      (should (equal "outside-before\nnew turn code\noutside-after\n"
                     (niwa-test--read-file file))))))

(niwa-deftest niwa-tool-note-write/rewind-waiting (:quiet t)
  ,test
  (test)
  :doc "Rewind clears a main chat's waiting suggestion without a note conversation link"
  (dolist (closed '(nil t))
    (niwa-note-conversation-test--with-source
      (let* ((main (niwa--chat-buffer "main" t workspace))
             (view (buffer-local-value 'niwa--view-buffer main))
             outcome)
        (with-current-buffer view
          (niwa-view-composer-insert "Suggest a fix")
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
        (with-current-buffer main
          (niwa-pipeline-run-tool-outcome
           (niwa-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
           (list :operations
                 (vector (list :action "create" :file_path file :start_line 2
                               :text "Main chat suggestion" :prepared_change
                               "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch"))))
          (niwa-test--await 5 "NoteWrite did not settle" outcome))
        (should (eq 'success (plist-get outcome :status)))
        (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get outcome :result)))
        (let* ((uuid (match-string 1 (plist-get outcome :result)))
               (suggestion (niwa-note-resolve uuid workspace)))
          (should-not (overlay-get suggestion 'niwa-note-session-id))
          (should (eq 'ready (niwa-notes-attention-state suggestion)))
          (niwa-note-conversation-test--answer (car sent) "The suggestion is ready.")
          (when closed (niwa-note-done suggestion))
          (with-current-buffer view
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _) (car (all-completions "" collection))))
                      ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (niwa-rewind)))
          (when closed (niwa-note-restore uuid workspace))
          (setq suggestion (niwa-note-resolve uuid workspace))
          (should-not (eq 'ready (niwa-notes-attention-state suggestion)))
          (should-error (niwa-note-apply-change suggestion) :type 'user-error)
          (should (equal "outside-before\nselected code\noutside-after\n"
                         (niwa-test--read-file file))))))))

(niwa-deftest niwa-tool-note-write/send (:quiet t)
  ,test
  (test)
  :doc "user-requested batch send leaves the current window, point and composer alone"
  (niwa-note-conversation-test--with-source
    (let* ((first (niwa-note-create "First requested note"))
           (second (niwa-note-create "Second requested note"))
           (main (niwa--chat-buffer "main" t workspace))
           (view (buffer-local-value 'niwa--view-buffer main))
           (syncs 0)
           (counter (lambda (&rest _) (cl-incf syncs)))
           outcome)
      (switch-to-buffer view)
      (goto-char (point-max))
      (insert "> main draft\ncontinued")
      (let ((text (buffer-string)) (position (point))
            (window (selected-window)) (start (window-start)))
        (advice-add 'niwa-instruction-store-sync :before counter)
        (unwind-protect
            (with-current-buffer main
              (niwa-pipeline-run-tool-outcome
               (niwa-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
               (list :operations
                     (vector (list :action "send" :id (overlay-get first 'niwa-uuid))
                             (list :action "send" :id (overlay-get second 'niwa-uuid)))))
              (niwa-test--await 5 "NoteWrite send did not settle" outcome)
              (ert-info ((format "%S" outcome))
                (should (eq 'success (plist-get outcome :status)))))
          (advice-remove 'niwa-instruction-store-sync counter))
        (should (eq window (selected-window)))
        (should (eq view (window-buffer window)))
        (should (= position (window-point window)))
        (should (= start (window-start window)))
        (with-current-buffer view (should (equal text (buffer-string)))))
      (should (= 1 syncs))
      (should (= 1 (length sent)))
      (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should (string-search "First requested note" payload))
        (should (string-search "Second requested note" payload))
        (should-not (string-search "main draft" payload)))
      (niwa-note-conversation-test--answer (car sent) "Batch answer."))))

(niwa-deftest niwa-tool-note-write/review (:quiet t)
  ,test
  (test)
  :doc "reviewing a created suggestion opens review without sending the unsent note"
  (niwa-note-conversation-test--with-source
    (let ((main (niwa--chat-buffer "main" t workspace)) outcome)
      (with-current-buffer main
        (niwa-pipeline-run-tool-outcome
         (niwa-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
         (list :operations
               (vector (list :action "create" :file_path file :start_line 2 :text "Suggestion"
                             :prepared_change
                             "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch")))))
      (niwa-test--await 5 "NoteWrite did not settle" outcome)
      (should (eq 'success (plist-get outcome :status)))
      (let ((note (car (niwa--all-instructions))))
        (niwa-note-apply-change note t)
        (should-not sent)
        (should (eq 'unsent (overlay-get note 'niwa-note-state)))
        (should (equal "outside-before\nselected code\noutside-after\n" (niwa-test--read-file file)))))))

(niwa-deftest niwa-tool-note-write/nested-match
  (:quiet t :doc "a matched child is previewed, sent and closed with its whole-line parent")
  (niwa-note-conversation-test--with-source
    (let ((parent (niwa-note-create "Parent brief"))
          (main (niwa--chat-buffer "main" t workspace)) outcome)
      (with-current-buffer main
        (niwa-pipeline-run-tool-outcome
         (niwa-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
         (list :operations (vector (list :action "create" :file_path file :start_line 2
                                         :match "selected" :text "Matched child brief")))))
      (niwa-test--await 5 "NoteWrite did not settle" outcome)
      (should (eq 'success (plist-get outcome :status)))
      (let* ((child (car (niwa--child-instructions parent)))
             (child-id (overlay-get child 'niwa-uuid))
             (preview (with-current-buffer (niwa-note-preview-prompt parent) (buffer-string)))
             (view (niwa-note-send parent :no-display t))
             (data (buffer-local-value 'niwa--data-buffer view)))
        (should (string-search "Matched child brief" preview))
        (should (equal (overlay-get parent 'niwa-note-session-id)
                       (overlay-get child 'niwa-note-session-id)))
        (should (string-search "Matched child brief"
                               (decode-coding-string (car sent-payloads) 'utf-8-unix)))
        (niwa-note-conversation-test--answer (car sent) "An answer.")
        (with-current-buffer data
          (niwa-tool-patch-handler
           #'ignore '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+selected text\n*** End Patch")))
        (niwa-note-apply-change parent)
        (should-not (overlay-buffer parent))
        (should-not (overlay-buffer child))
        (should (cl-find child-id (niwa-notes-closed workspace)
                         :key (lambda (entry)
                                (plist-get (plist-get (plist-get entry :instruction) :properties)
                                           'niwa-uuid))
                         :test #'equal))))))

(niwa-deftest niwa-note-send/store-write (:quiet t)
  ,test
  (test)
  :doc "ordinary send persists its sent state in one store write without a caller flush"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Send normally"))
           (writes 0)
           (counter (lambda (outcome)
                      (when (memq outcome '(written merged replaced)) (cl-incf writes))
                      outcome)))
      (advice-add 'niwa-instruction-store-sync :filter-return counter)
      (unwind-protect (niwa-note-send note)
        (advice-remove 'niwa-instruction-store-sync counter))
      (should (= 1 writes))
      (niwa-note-conversation-test--answer (car sent) "Normal answer.")
      (kill-buffer source)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (let ((restored (niwa-note-resolve (overlay-get note 'niwa-uuid) workspace)))
        (should (eq 'sent (overlay-get restored 'niwa-note-state)))))))

(niwa-deftest niwa-note-send/flush-point (:quiet t)
  ,test
  (test)
  :doc "a send flushes once, after every note is already sent; a deferred send never flushes"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Flush order"))
           (other (progn (goto-char (point-min)) (niwa-note-create "Deferred")))
           states
           (spy (lambda (&rest _)
                  (push (list (overlay-get note 'niwa-note-state)
                              (overlay-get other 'niwa-note-state))
                        states))))
      (advice-add 'niwa-instruction-store-flush :before spy)
      (unwind-protect
          (progn (niwa-note-send note :no-display t)
                 (should (equal '((sent unsent)) states))
                 (setq states nil)
                 (niwa-note-send other :no-display t :defer-write t)
                 (should (eq 'sent (overlay-get other 'niwa-note-state)))
                 (should-not states))
        (advice-remove 'niwa-instruction-store-flush spy)))))

(niwa-deftest niwa-note-conversation/hint-after-transitions (:quiet t)
  ,test
  (test)
  :doc "the composer hint follows hold-change and record-applied, and ignores an unlinked suggestion"
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Review this code and leave a suggestion"))
           (view (niwa-note-send note))
           (data (buffer-local-value 'niwa--data-buffer view))
           (sid (overlay-get note 'niwa-note-session-id))
           (record (list :id "r" :session-id sid :turn 1))
           outcome)
      (cl-flet ((hint () (substring-no-properties
                          (buffer-local-value 'niwa-view--input-hint view)))
                (label () (overlay-get (overlay-get note 'niwa-label-overlay)
                                       'after-string)))
        (should (equal "C-c C-z close" (hint)))
        ;; A suggestion made from inside the conversation has no session link.
        (with-current-buffer data
          (niwa-pipeline-run-tool-outcome
           (niwa-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
           (list :operations
                 (vector (list :action "create" :file_path file :start_line 2
                               :text "A suggested fix" :prepared_change
                               "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch")))))
        (should (equal "C-c C-z close" (hint)))
        (overlay-put note 'niwa-note-labels 'expanded)
        (niwa-note-hold-change (list note) record)
        (should (string-search "C-c C-a apply" (hint)))
        (should (string-search "change ready" (label)))
        (niwa-note-record-applied record nil workspace)
        (should (equal "C-c C-z close" (hint)))
        (should-not (string-search "change ready" (or (label) "")))))))

(ert-deftest niwa-note-conversation-hint-preserves-draft-undo ()
  (niwa-note-conversation-test--with-source
    (let* ((note (niwa-note-create "Suggest a change"))
           (view (niwa-note-send note))
           (record (list :id "r" :session-id
                         (overlay-get note 'niwa-note-session-id) :turn 1)))
      (with-current-buffer (niwa-view-composer-buffer view)
        (goto-char (point-max))
        (insert "draft")
        (undo-boundary))
      (niwa-note-hold-change (list note) record)
      (with-current-buffer (niwa-view-composer-buffer view)
        (undo-only))
      (should (equal "" (niwa-view-composer-draft-text view t))))))

(defmacro niwa-note-conversation-test--with-tree (&rest body)
  "Run BODY with three nested notes in the ordinary send fixture."
  (declare (indent 0))
  `(niwa-note-conversation-test--with-source
     (erase-buffer)
     (insert "one\ntwo\nthree\nfour\nfive\nsix\nseven\n")
     (save-buffer)
     (let ((parent (niwa-note-make source 1 (point-max) :text "Parent brief"))
           (child (niwa-note-make source 5 29 :text "Child brief"))
           (grandchild (niwa-note-make source 9 24 :text "Grandchild brief")))
       ,@body)))

(niwa-deftest niwa-note-conversation--tree
  (:quiet t)
  ,test
  (test)
  :doc "all depths are included with immediate parents, never ancestors of the root"
  (niwa-note-conversation-test--with-tree
    (should (equal (list parent child grandchild)
                   (mapcar (lambda (entry) (plist-get entry :note))
                           (niwa-note-conversation--tree parent))))
    (let ((tree (niwa-note-conversation--tree child)))
      (should (equal (list child grandchild)
                     (mapcar (lambda (entry) (plist-get entry :note)) tree)))
      (should (eq child (plist-get (cadr tree) :parent)))))
  :doc "an independent conversation excludes its entire subtree"
  (niwa-note-conversation-test--with-tree
    (overlay-put child 'niwa-note-session-id "independent")
    (let ((tree (niwa-note-conversation--tree parent)))
      (should (= 2 (length tree)))
      (should (plist-get (cadr tree) :excluded)))))

(niwa-deftest niwa-note-conversation--batch-tree
  (:quiet t :doc "batch roots dedupe descendants regardless of selection order")
  (niwa-note-conversation-test--with-tree
    (should (equal (niwa-note-conversation--tree parent)
                   (niwa-note-conversation--batch-tree
                    (list grandchild child parent child))))
    (overlay-put child 'niwa-note-session-id "independent")
    (should (equal (append (niwa-note-conversation--tree parent)
                           (niwa-note-conversation--tree child))
                   (niwa-note-conversation--batch-tree
                    (list parent child grandchild))))))

(niwa-deftest niwa-note-conversation--prompt
  (:quiet t)
  ,test
  (test)
  :doc "only the root has code; every descendant names its immediate parent"
  (niwa-note-conversation-test--with-tree
    (let* ((session (niwa-session-create "Preview" workspace root))
           (prompt (niwa-note-conversation--prompt parent session)))
      (should (= 6 (cl-count ?` prompt)))
      (should (string-search (format "Note %s at line 2, inside Note %s\n\nChild brief"
                                     (overlay-get child 'niwa-id)
                                     (overlay-get parent 'niwa-id)) prompt))
      (should (string-search (format "Note %s at line 3, inside Note %s\n\nGrandchild brief"
                                     (overlay-get grandchild 'niwa-id)
                                     (overlay-get child 'niwa-id)) prompt))))
  :doc "excluded child is one exact line without its text or descendants"
  (niwa-note-conversation-test--with-tree
    (overlay-put child 'niwa-note-session-id "independent")
    (let ((prompt (niwa-note-conversation--prompt
                   parent (niwa-session-create "Preview" workspace root))))
      (should (string-search
               (format "Note %s at line 2, inside Note %s, has its own conversation and is not part of this request."
                       (overlay-get child 'niwa-id) (overlay-get parent 'niwa-id)) prompt))
      (should-not (string-search "Child brief" prompt))
      (should-not (string-search "Grandchild brief" prompt)))))

(niwa-deftest niwa-note-preview-prompt
  (:quiet t :doc "preview and send use identical generated text without preview side effects")
  (niwa-note-conversation-test--with-tree
    (let* ((buffers (buffer-list))
           (preview (niwa-note-preview-prompt parent))
           (text (with-current-buffer preview (buffer-string))))
      (should-not sent)
      (should-not (overlay-get parent 'niwa-note-session-id))
      (should (= 1 (length (cl-set-difference (buffer-list) buffers))))
      (niwa-note-send-batch (list grandchild parent child) :no-display t)
      (let* ((data (plist-get (gptel-fsm-info (car sent)) :buffer))
             (session (buffer-local-value 'niwa--session data)))
        (should (equal text (niwa-note-conversation--prompt parent session)))
        (with-current-buffer data
          (should (string-search (string-trim-right text) (buffer-string))))
        (dolist (note (list parent child grandchild))
          (should (equal (overlay-get note 'niwa-note-session-id)
                         (niwa-session-session-id session)))))
      (should (= 1 (length sent))))))

(niwa-deftest niwa-note-conversation--send-notes
  (:quiet t)
  ,test
  (test)
  :doc "sending a child includes its descendants, but both the parent and new descendants stay unsent on reopen"
  (niwa-note-conversation-test--with-tree
    (let* ((view (niwa-note-send child :no-display t))
           (payload (decode-coding-string (car sent-payloads) 'utf-8-unix))
           (id (overlay-get child 'niwa-note-session-id)))
      (should (string-search "Grandchild brief" payload))
      (should-not (string-search "Parent brief" payload))
      (should (equal id (overlay-get grandchild 'niwa-note-session-id)))
      (should-not (overlay-get parent 'niwa-note-session-id))
      (let ((new (niwa-note-make source 10 12 :text "Not sent yet")))
        (should (eq view (niwa-note-send child :no-display t)))
        (should-not (overlay-get new 'niwa-note-session-id))
        (should (= 1 (length sent))))))
  :doc "selecting an independent child together with its parent refuses rather than losing either thread"
  (niwa-note-conversation-test--with-tree
    (niwa-note-send child :no-display t)
    (should-error (niwa-note-send-batch (list parent child) :no-display t) :type 'user-error)
    (should-not (overlay-get parent 'niwa-note-session-id))
    (should (= 1 (length sent))))
  :doc "invalid selections fail validation before any new conversation"
  (niwa-note-conversation-test--with-tree
    (should-error (niwa-note-send-batch (list parent 'invalid) :no-display t) :type 'user-error)
    (should-error (niwa-note-send-batch (list 'invalid parent) :no-display t) :type 'user-error)
    (should-not sent))
  :doc "batch send builds each selected note's tree only once"
  (niwa-note-conversation-test--with-source
    (erase-buffer)
    (dotimes (_ 50) (insert "code\n"))
    (save-buffer)
    (let* ((notes (cl-loop for start from 1 below (point-max) by 5
                           collect (niwa-note-make source start (+ start 4)
                                                     :text "Review this line")))
           (build-tree (symbol-function 'niwa-note-conversation--tree))
           (builds 0))
      (cl-letf (((symbol-function 'niwa-note-conversation--tree)
                 (lambda (item)
                   (cl-incf builds)
                   (funcall build-tree item))))
        (niwa-note-send-batch notes :no-display t))
      (should (= (length notes) builds))
      (should (= 1 (length sent))))))

(provide 'test-niwa-note-conversation)
;;; test-niwa-note-conversation.el ends here
