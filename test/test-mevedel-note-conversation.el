;;; test-mevedel-note-conversation.el -- Note send flows -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise note commands in real files with the normal session/view pipeline.
;; gptel's network transport is replaced with canned answers; frame attendance
;; is fixed independently of the desktop's input focus.

;;; Code:

(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-claude-code)
(require 'mevedel-note-frame)
(require 'mevedel-note-conversation)
(require 'mevedel-notes)
(require 'mevedel-notes-list)
(require 'mevedel-models)
(require 'mevedel-permission-mode)
(require 'mevedel-session-persistence)
(require 'mevedel-session-codec)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-fork)
(require 'mevedel-session-rewind)
(require 'mevedel-telemetry)
(require 'mevedel-transport)
(require 'mevedel-journal-process)
(require 'gptel-openai)
(require 'gptel-transient)
(require 'mevedel-file-state)
(require 'mevedel-tool-patch)
(require 'mevedel-pipeline)
(require 'mevedel-tool-note)
(require 'mevedel-note-write)
(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-instruction-test-support"))

(defmacro mevedel-note-conversation-test--with-source (&rest body)
  "Run BODY in a real source file with canned transport and attended frames."
  (declare (indent 0))
  `(let* ((root (make-temp-file "mevedel-note-send-" t))
          (file (file-name-concat root "source.el"))
          (mevedel-sessions-directory (file-name-concat root "sessions"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)
          (mevedel-session-persistence--cleanup-throttle (make-hash-table :test #'equal))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          (mevedel-notes--unsaved-noticed nil)
          (mevedel-journal-enabled nil)
          (gptel--known-tools (copy-tree gptel--known-tools))
          (gptel--known-presets (copy-tree gptel--known-presets))
          (mevedel-preset--registry (copy-tree mevedel-preset--registry))
          (gptel-prompt-transform-functions (copy-tree gptel-prompt-transform-functions))
          (transport-enabled mevedel-transport--enabled-p)
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
           (make-directory (file-name-concat root ".mevedel"))
           (write-region "outside-before\nselected code\noutside-after\n" nil file nil 'silent)
           (write-region "Project-only instruction" nil
                         (file-name-concat root "AGENTS.md") nil 'silent)
           (setq workspace (mevedel-workspace-get-or-create 'file root root "notes")
                 source (find-file-noselect file))
           (mevedel-install)
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
             (setq-local mevedel--workspace workspace)
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
       (mevedel-workspace-clear-registry)
       (setq mevedel-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
       (mevedel-uninstall)
       (when transport-enabled (mevedel-transport-install))
       (delete-directory root t)
       ;; No command loop runs to consume our frame-focus events.
       (setq unread-command-events
             (cl-remove-if (lambda (event)
                             (and (eq (car-safe (if (eq (car-safe event) t)
                                                     (cdr event) event))
                                      'switch-frame)
                                  (not (memq event events))))
                           unread-command-events)))))

(mevedel-deftest mevedel-note-conversation-test--with-source
  (:quiet t)
  ,test
  (test)
  :doc "cleanup discards only new frame-switch events, preserving other input"
  (let* ((existing (list 'switch-frame (selected-frame)))
         (unread-command-events (list existing ?x)))
    (mevedel-note-conversation-test--with-source
      (push ?y unread-command-events)
      (push (list t 'switch-frame (selected-frame)) unread-command-events)
      (push (list 'switch-frame (selected-frame)) unread-command-events))
    (should (equal (list ?y existing ?x) unread-command-events)))
  :doc "rendering stays attended without compositor focus and restores the focus query"
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) nil)))
    (mevedel-note-conversation-test--with-source
      (should (frame-focus-state)))
    (should-not (frame-focus-state))))

(defun mevedel-note-conversation-test--answer (fsm text)
  "Deliver canned TEXT through FSM's normal insertion and settlement."
  (let ((info (gptel-fsm-info fsm)))
    (plist-put info :http-status "200")
    (plist-put info :status "HTTP/1.1 200 OK")
    (gptel--fsm-transition fsm)
    (gptel--insert-response text info)
    (gptel--fsm-transition fsm)
    (mevedel-test--await 5 "Note answer did not settle"
      (not (mevedel-turn-busy-p (plist-get info :buffer))))))

(mevedel-deftest mevedel-note-send/streaming-frame
  (:quiet t)
  (cl-flet ((wait (seconds)
              (let ((timer (run-at-time seconds nil #'exit-recursive-edit)))
                (unwind-protect (recursive-edit) (cancel-timer timer)))))
    (when (display-graphic-p)
      (mevedel-note-conversation-test--with-source
        (let* ((gptel-stream t)
               (gptel-backend (gptel-make-openai
                               "Streaming notes" :key "test" :models '(test-model)
                               :stream t))
               (mevedel-view-spinner-style 'braille)
               (mevedel-view-spinner-animate t)
               (mevedel-note-frame-min-height 20)
               (mevedel-note-frame-height 20)
               (draft "> active draft\nsecond line")
               (source-window (selected-window))
               (view (mevedel-note-send (mevedel-note-create "Explain this code")))
               (frame mevedel-note-frame--frame)
               (window (get-buffer-window view frame))
               (editor (mevedel-view-composer-window window))
               (info (gptel-fsm-info (car sent))))
          (unwind-protect
              (progn
                (should (frame-parent frame))
                (should (eq (window-buffer editor) (mevedel-view-composer-buffer view)))
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
                    (mevedel-view-test--insert-composer-draft draft 4))
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
                      (let ((sample (get-char-property
                                     (car mevedel-view--spinner-label-target) 'display window)))
                        (should sample)
                        (should
                         (cl-loop repeat 12
                                  do (wait 0.04)
                                  thereis (not (equal sample
                                                      (get-char-property
                                                       (car mevedel-view--spinner-label-target)
                                                       'display window))))))
                      (should (equal draft (mevedel-view-composer-draft-text))))
                    (should (= draft-point (window-point editor)))
                    (should (= draft-start (window-start editor)))
                    (should (= draft-row (cdr (posn-col-row (posn-at-point draft-point editor)))))
                    (should (eq selected (selected-window)))
                    (should (= source-start (window-start source-window))))
                  (gptel--fsm-transition (car sent))
                  (mevedel-test--await 5 "Streaming note did not settle"
                    (not (mevedel-turn-busy-p (plist-get info :buffer))))))
            (when (frame-live-p frame) (mevedel-note-frame-close)))))))
  (source-selected)
  :doc "child-selected note frame renders chunks and animates between chunks"
  nil
  :doc "source-selected note frame renders chunks and animates between chunks"
  t)

(mevedel-deftest mevedel-note-conversation-attention-state
  (:quiet t)
  ,test
  (test)
  :doc "redisplay projects overlay and plist conversation attention without copying overlays"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Explain this code"))
           (view (mevedel-note-send note))
           (properties (overlay-properties note)))
      (with-current-buffer source
        (cl-letf (((symbol-function 'overlay-properties)
                   (lambda (_) (error "Redisplay copied an overlay plist"))))
          (should (equal " Notes: 1 working" (mevedel-notes-mode-line)))
          (should (eq 'working (mevedel-notes-attention-state note)))
          (should (eq 'working (mevedel-notes-attention-state properties)))
          (overlay-put note 'mevedel-note-attention-state 'ready)
          (should (equal " Notes: 1 ready" (mevedel-notes-mode-line)))
          (overlay-put note 'mevedel-note-attention-state nil)))
      (mevedel-note-conversation-test--answer (car sent) "The answer.")
      (with-current-buffer source
        (should-not (mevedel-notes-mode-line))
        (should (eq 'sent (mevedel-notes-attention-state note)))
        (should (eq 'sent (mevedel-notes-attention-state properties))))
      (should (buffer-live-p view)))))

(mevedel-deftest mevedel-note-send
  (:quiet t)
  ,test
  (test)
  :doc "send focuses a conversation with a read-only answer and editable prompt"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Why is this selected?"))
           (view (mevedel-note-send note)))
      (should (eq view (window-buffer (selected-window))))
      (if (display-graphic-p)
          (progn
            (should (eq (selected-frame) mevedel-note-frame--frame))
            (should (eq (frame-parent (selected-frame))
                        (window-frame (get-buffer-window source t)))))
        (should (eq 'right (window-parameter (selected-window) 'window-side))))
      (should (= 1 (length sent)))
      (mevedel-note-conversation-test--answer (car sent) "It is the selected code.")
      (with-current-buffer view
        (goto-char (point-min))
        (should (search-forward "It is the selected code." nil t))
        (should-error (insert "not editable") :type 'text-read-only)
        (mevedel-view-composer-insert "A reply without the minibuffer"))
      (with-current-buffer source
        (should (equal "outside-before\nselected code\noutside-after\n" (buffer-string))))))
  :doc "initial input includes capabilities and memory but excludes unrelated conversation context"
  (mevedel-note-conversation-test--with-source
    (make-directory (file-name-concat root ".mevedel" "memory") t)
    (write-region "UNRELATED-MEMORY-SECRET" nil
                  (file-name-concat root ".mevedel" "memory" "MEMORY.md") nil 'silent)
    (mevedel-note-create "UNRELATED-NOTE-SECRET")
    (goto-char (point-min))
    (mevedel-instruction-test--create-note-in source (point) (line-end-position))
    (let ((main (mevedel--chat-buffer "main" t workspace)))
      (with-current-buffer main (insert "UNRELATED-CHAT-SECRET")))
    (forward-line 1)
    (let* ((note (mevedel-note-create "Explain this code?"))
           (view (mevedel-note-send note))
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
      (mevedel-note-conversation-test--answer (car sent) "An explanation, not an edit.")))
  :doc "close preserves the conversation and send reopens without another request"
  (mevedel-note-conversation-test--with-source
    (let* ((origin (point))
           (note (mevedel-note-create "Explain, please"))
           (view (mevedel-note-send note)))
      (mevedel-note-conversation-test--answer (car sent) "A retained answer.")
      (with-current-buffer view
        (goto-char (point-max))
        (insert "Draft still here")
        (call-interactively (key-binding (kbd "C-c C-z"))))
      (should (buffer-live-p view))
      (should (eq source (window-buffer (selected-window))))
      (should (= origin (window-point (selected-window))))
      (with-current-buffer source
        (should (eq view (mevedel-note-send note))))
      (should (= 1 (length sent)))
      (with-current-buffer view
        (should (string-search "A retained answer." (buffer-string)))
        (should (string-suffix-p "Draft still here" (buffer-string))))))
  :doc "source menu sends a note without installing plain-letter source bindings"
  (mevedel-note-conversation-test--with-source
    (mevedel-note-create "Menu question?")
    (should (eq #'self-insert-command (key-binding "s")))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?s)))
      (call-interactively #'mevedel-note-menu))
    (should (= 1 (length sent)))
    (mevedel-note-conversation-test--answer (car sent) "Sent through the menu."))
  :doc "a change prepared during the first real note request can be applied and reverted"
  (mevedel-note-conversation-test--with-source
    (let* ((source-key (key-binding (kbd "C-c C-b")))
           (note (mevedel-note-create "Improve this code"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view)))
      (should (= 1 (length sent)))
      (with-current-buffer data
        (should mevedel--current-request)
        (mevedel-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+improved code\n*** End Patch")))
      (should (equal "outside-before\nselected code\noutside-after\n"
                     (mevedel-test--read-file file)))
      (mevedel-note-conversation-test--answer (car sent) "A prepared improvement.")
      (with-current-buffer view
        (call-interactively (key-binding (kbd "C-c C-a"))))
      (should (equal "outside-before\nimproved code\noutside-after\n"
                     (mevedel-test--read-file file)))
      (with-current-buffer source
        (goto-char (point-min))
        (should (eq source-key (key-binding (kbd "C-c C-b"))))
        (should-not (overlay-buffer note))
        (should (= 1 (length (mevedel-notes-closed workspace)))))
      (with-current-buffer view
        (should (eq #'mevedel-note-revert-change (key-binding (kbd "C-c C-b"))))
        (call-interactively (key-binding (kbd "C-c C-b"))))
      (ert-info ("source after reverting")
        (should (equal "outside-before\nselected code\noutside-after\n"
                       (mevedel-test--read-file file))))
      (with-current-buffer view
        (should (string-search "A prepared improvement." (buffer-string)))))))

(mevedel-deftest mevedel-note-send/payload-encoding
  (:quiet t :doc "non-ASCII note input retains the sent UTF-8 bytes and displays decoded text")
  (mevedel-note-conversation-test--with-source
    (mevedel-define-preset note-test-unicode
      :description "No tool schemas" :system "Explain the note." :tools nil)
    (let ((note (mevedel-note-create "Explain caf\u00e9 \u2013 please")))
      (overlay-put note 'mevedel-note-settings '(:preset note-test-unicode))
      (let* ((view (mevedel-note-send note))
             (data (buffer-local-value 'mevedel--data-buffer view))
             (session (buffer-local-value 'mevedel--session data))
             (directory (file-name-concat (mevedel-session-save-path session) "note-input"))
             (file (car (directory-files directory t "\\.json\\'")))
             (payload (car sent-payloads)))
        (should-not (plist-get (plist-get (gptel-fsm-info (car sent)) :data) :tools))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file)
          (should (equal payload (buffer-string))))
        (with-current-buffer view
          (with-current-buffer (mevedel-note-show-sent)
            (should (equal (decode-coding-string payload 'utf-8-unix) (buffer-string)))
            (should (string-search "caf\u00e9 \u2013" (buffer-string)))))
        (mevedel-note-conversation-test--answer (car sent) "An answer.")))))

(mevedel-deftest mevedel-note-send/base-preset
  (:quiet t :doc "notes use the implementation base and keep isolation after a preset change")
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Explain this code"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data)))
      (should (eq 'mevedel-implement (mevedel-session-preset-name session)))
      (mevedel-note-conversation-test--answer (car sent) "An answer.")
      (with-current-buffer data
        (mevedel-define-preset note-test-base
          :description "Custom base" :system "CUSTOM-BASE-SYSTEM"
          :tools ((:tool "Read")))
        (mevedel-preset-apply 'note-test-base)
        (should (equal "CUSTOM-BASE-SYSTEM" gptel-system-prompt))
        (should (equal '("Read") (mapcar #'gptel-tool-name gptel-tools)))
        (should-not gptel-use-context)
        (should (equal '(workspace-config memory environment tools skills resources)
                       (mevedel-system-context-components session))))
      (with-current-buffer view
        (should mevedel-note-conversation-mode)
        (mevedel-view-composer-insert "Follow up")
        (with-current-buffer (mevedel-view-composer-buffer)
          (mevedel-view-send)))
      (should (= 2 (length sent)))
      (should (string-search "CUSTOM-BASE-SYSTEM"
                             (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
      (mevedel-note-conversation-test--answer (car sent) "Custom answer.")
      (should (= 2 (length (directory-files
                            (file-name-concat (mevedel-session-save-path session) "note-input")
                            nil "\\.json\\'")))))))

(mevedel-deftest mevedel-note-send/composer-hints
  (:quiet t :doc "state changes update dimmed hints without moving or editing the composer")
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Improve this code"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (composer (mevedel-view-composer-buffer view))
           (draft "> active draft\nsecond line"))
      (mevedel-note-conversation-test--answer (car sent) "An answer.")
      (with-current-buffer composer
        (should (string-search "C-c C-z close\n> " (buffer-string)))
        (mevedel-view-test--insert-composer-draft draft))
      (let ((selected (selected-window))
            (position (mevedel-view-test--composer-point-offset view))
            (start (window-start (selected-window))))
        (with-current-buffer data
          (mevedel-tool-patch-handler
           #'ignore
           '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+improved code\n*** End Patch")))
        (with-current-buffer composer
          (should (string-search "C-c C-z close · C-c C-a apply · C-c C-g merge\n> "
                                 (buffer-string)))
          (should (equal draft (mevedel-view-composer-draft-text)))
          (should (= position (mevedel-view-test--composer-point-offset view))))
        (should (eq selected (selected-window)))
        (should (= start (window-start selected))))
      (mevedel-note-apply-change note)
      (with-current-buffer composer
        (should (string-search "C-c C-z close\n> " (buffer-string)))
        (should-not (string-search "C-c C-a apply" (buffer-string)))
        (should (equal draft (mevedel-view-composer-draft-text)))))))

(mevedel-deftest mevedel-note-send/saved-settings
  (:quiet t :doc "the first real send uses the note's persisted choices, not the chat defaults")
  (mevedel-note-conversation-test--with-source
    (cl-letf (((symbol-plist 'note-reasoner)
               '(:reasoning-effort (member low high))))
      (gptel-make-openai "Other" :key "test" :models '(note-reasoner))
      (mevedel-define-preset note-test-base
        :description "Custom note base" :system "CUSTOM-FIRST-SYSTEM"
        :tools ((:tool "Read")))
      (let ((note (mevedel-note-create "Explain this code")))
        (overlay-put note 'mevedel-note-settings
                     '(:preset note-test-base :provider "Other:note-reasoner"
                       :effort high :permission-mode edits))
        (mevedel-instruction-store-touch workspace)
        (mevedel-instruction-store-flush t)
        (mevedel--clear-instruction-state workspace)
        (mevedel-instruction-store-observe workspace)
        (with-current-buffer source
          (goto-char (point-min)) (forward-line 1)
          (setq note (mevedel-notes--at-point)))
        (let* ((view (mevedel-note-send note))
               (data (buffer-local-value 'mevedel--data-buffer view))
               (session (buffer-local-value 'mevedel--session data)))
          (should (eq 'note-test-base (mevedel-session-preset-name session)))
          (should (eq 'high (mevedel-session-reasoning-effort session)))
          (should (eq 'edits (mevedel-session-permission-mode session)))
          (with-current-buffer data
            (should (eq 'note-reasoner gptel-model))
            (should (equal "Other" (gptel-backend-name gptel-backend))))
          (should (string-search "CUSTOM-FIRST-SYSTEM"
                                 (gptel--json-encode
                                  (plist-get (gptel-fsm-info (car sent)) :data))))
          (mevedel-note-conversation-test--answer (car sent) "Custom first answer."))))))

(mevedel-deftest mevedel-note-show-sent
  (:quiet t :doc "inputs are separate immutable files shown in dispatch order after reopening")
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Explain this code"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data))
           (directory (file-name-concat (mevedel-session-save-path session) "note-input"))
           (payload (decode-coding-string (car sent-payloads) 'utf-8-unix)))
      (mevedel-note-conversation-test--answer (car sent) "An answer.")
      (should (file-directory-p directory))
      (let* ((first (car (directory-files directory t "\\.json\\'")))
             (bytes (mevedel-test--read-file first))
             (mtime (file-attribute-modification-time (file-attributes first))))
        (dotimes (index 10)
          (mevedel-note-conversation-record-input session (list :round (1+ index))))
        (should (equal bytes (mevedel-test--read-file first)))
        (should (equal mtime (file-attribute-modification-time (file-attributes first))))
        (should (= 11 (length (directory-files directory t "\\.json\\'"))))
        (should (= #o600 (logand #o777 (file-modes first)))))
      (with-current-buffer view
        (with-current-buffer (mevedel-note-show-sent)
          (should (equal (concat payload "\n\n"
                                 (mapconcat (lambda (index)
                                              (gptel--json-encode (list :round (1+ index))))
                                            (number-sequence 0 9) "\n\n"))
                         (buffer-string))))))))

(mevedel-deftest mevedel-note-send/failed-first-send
  (:quiet t :doc "a failed or interrupted preset discards unlinked data and view buffers")
  (mevedel-note-conversation-test--with-source
    (dolist (condition '(error quit))
      (let ((note (mevedel-note-create "Explain this code"))
            (existing (buffer-list)))
        (setf (plist-get (alist-get 'mevedel-implement gptel--known-presets) :pre)
              (lambda () (signal condition '("Preset refused first send"))))
        (condition-case err
            (progn (mevedel-note-send note) (ert-fail "First send did not fail"))
          ((error quit) (should (eq condition (car err)))))
        (should-not (overlay-get note 'mevedel-note-session-id))
        (should-not sent)
        (dolist (buffer (cl-set-difference (buffer-list) existing))
          (should-not (buffer-local-value 'mevedel--session buffer))
          (should-not (buffer-local-value 'mevedel--data-buffer buffer)))))))

(mevedel-deftest mevedel-rewind/note-session
  (:quiet t)
  (cl-letf (((symbol-function 'read-multiple-choice)
             (lambda (&rest _) '(?f "conversation and files"))))
    ,test)
  (test)
  :doc "Rewind reverses only its applied first turn, keeping another session and user edits"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Improve this code"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view)))
      (with-current-buffer data
        (mevedel-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+first proposal\n*** End Patch")))
      (mevedel-note-conversation-test--answer (car sent) "Ready to apply.")
      (mevedel-note-apply-change note)
      (should (equal "outside-before\nfirst proposal\noutside-after\n"
                     (mevedel-test--read-file file)))
      (with-current-buffer source
        (goto-char (point-max)) (forward-line -1)
        (let* ((other (mevedel-note-create "Another conversation"))
               (other-view (mevedel-note-send other))
               (other-data (buffer-local-value 'mevedel--data-buffer other-view)))
          (with-current-buffer other-data
            (mevedel-tool-patch-handler
             #'ignore
             '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-outside-after\n+other session\n*** End Patch")))
          (mevedel-note-conversation-test--answer (car sent) "Another change.")
          (mevedel-note-apply-change other)
          (with-current-buffer other-view (mevedel-note-conversation-close))))
      (with-current-buffer source
        (goto-char (point-min)) (insert "user edit\n") (save-buffer))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (mevedel-rewind)))
      (should (equal "user edit\noutside-before\nselected code\nother session\n"
                     (mevedel-test--read-file file)))))
  :doc "Rewind before first turn preserves the current note and makes it sendable again"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Original request"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (id (overlay-get note 'mevedel-note-session-id)))
      (with-current-buffer data
        (mevedel-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+waiting proposal\n*** End Patch")))
      (mevedel-note-conversation-test--answer (car sent) "A waiting change.")
      (mevedel-note-edit "Current request" note)
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (mevedel-rewind)))
      (with-current-buffer (mevedel-view-composer-buffer view)
        (should (string-search "C-c C-z close\n> " (buffer-string)))
        (should-not (string-search "C-c C-a apply" (buffer-string))))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (with-current-buffer source
        (goto-char (point-min)) (forward-line 1)
        (setq note (mevedel-notes--at-point))
        (should (equal "Current request" (overlay-get note 'mevedel-note-text)))
        (should (= (point) (overlay-start note)))
        (should (eq 'unsent (overlay-get note 'mevedel-note-state)))
        (should (equal id (overlay-get note 'mevedel-note-session-id)))
        (should-not (overlay-get note 'mevedel-note-prepared-change))
        (should-not (memq (mevedel-notes-attention-state note) '(ready stale)))
        (mevedel-note-send note))
      (should (= 2 (length sent)))
      (should (equal id (overlay-get note 'mevedel-note-session-id)))
      (mevedel-note-conversation-test--answer (car sent) "Sent again.")))
  :doc "sending a note without initial workspace storage still supports Rewind"
  (mevedel-note-conversation-test--with-source
    (delete-directory (file-name-concat root ".mevedel") t)
    (let* ((note (mevedel-note-create "Ephemeral request"))
           (view (mevedel-note-send note)))
      (mevedel-note-conversation-test--answer (car sent) "An ephemeral answer.")
      (mevedel-note-edit "Current ephemeral request" note)
      ;; Session startup establishes storage even outside a project.
      (should (file-exists-p (mevedel-instruction-store-path workspace)))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (mevedel-rewind)))
      (with-current-buffer source
        (goto-char (point-min)) (forward-line 1)
        (let ((restored (mevedel-notes--at-point)))
          (should (eq 'unsent (overlay-get restored 'mevedel-note-state)))
          (should (equal "Current ephemeral request" (overlay-get restored 'mevedel-note-text)))))))
  :doc "batch Rewind unsends closed and unvisited notes without losing their anchors"
  (mevedel-note-conversation-test--with-source
    (let* ((first (mevedel-note-create "First"))
           (second (progn (forward-line 1) (mevedel-note-create "Second")))
           (second-id (overlay-get second 'mevedel-uuid))
           (other-file (file-name-concat root "other.txt"))
           (other-buffer (progn (write-region "unvisited code\n" nil other-file nil 'silent)
                                (find-file-noselect other-file)))
           (third (with-current-buffer other-buffer
                    (setq-local mevedel--workspace workspace)
                    (mevedel-note-create "Third")))
           (view (mevedel-note-send-batch (list first second third))))
      (mevedel-note-conversation-test--answer (car sent) "One batch.")
      (mevedel-note-done second)
      (kill-buffer other-buffer)
      (should-not (find-buffer-visiting other-file))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (car (all-completions "" collection))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (mevedel-rewind)))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (let ((restored (mevedel-note-restore second-id workspace)))
        (should (eq 'unsent (overlay-get restored 'mevedel-note-state)))
        (should (equal "Second" (overlay-get restored 'mevedel-note-text))))
      (with-current-buffer (find-file-noselect other-file)
        (setq-local mevedel--workspace workspace)
        (goto-char (point-min))
        (let ((restored (mevedel-notes--at-point)))
          (should (eq 'unsent (overlay-get restored 'mevedel-note-state)))
          (should (equal "Third" (overlay-get restored 'mevedel-note-text))))))))

(mevedel-deftest mevedel-note-send/taken
  (:quiet t)
  ,test
  (test)
  :doc "sending a taken note ends watching, including with an existing conversation"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Implement this myself"))
           (view (mevedel-note-send (mevedel-note-take note))))
      (should (eq 'sent (overlay-get note 'mevedel-note-state)))
      (mevedel-note-conversation-test--answer (car sent) "A retained answer.")
      (with-current-buffer view (mevedel-note-conversation-close))
      (with-current-buffer source
        (mevedel-note-take note)
        (should (eq view (mevedel-note-send note)))
        (should (eq 'sent (overlay-get note 'mevedel-note-state)))
        (should (= 1 (length sent)))
        (with-current-buffer view (mevedel-note-conversation-close))
        (mevedel--clear-instruction-state workspace)
        (mevedel-instruction-store-observe workspace)
        (with-current-buffer source
          (goto-char (point-min)) (forward-line 1)
          (should (eq 'sent (overlay-get (mevedel-notes--at-point)
                                        'mevedel-note-state))))))))

(mevedel-deftest mevedel-note-send/buddy-ownership
  (:quiet t)
  ,test
  (test)
  :doc "send transfers a Buddy remark before creating its durable conversation"
  (mevedel-note-conversation-test--with-source
    (require 'mevedel-buddy-note)
    (let ((note (mevedel-note-create "Fix what Buddy found")))
      (overlay-put note 'mevedel-note-author 'buddy)
      (let ((view (mevedel-note-send note)))
        (should (eq 'user (overlay-get note 'mevedel-note-author)))
        (should (overlay-get note 'mevedel-note-kept-from-buddy))
        (mevedel-note-conversation-test--answer (car sent) "Fixed in this conversation.")
        (with-current-buffer view
          (should (string-search "Fixed in this conversation." (buffer-string)))))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (let ((restored (with-current-buffer source (mevedel-notes--at-point))))
        (should (eq 'user (overlay-get restored 'mevedel-note-author)))
        (should (overlay-get restored 'mevedel-note-kept-from-buddy))
        (should (overlay-get restored 'mevedel-note-session-id))))))

(mevedel-deftest mevedel-inspect-effective-prompt/note
  (:quiet t :doc "note conversations expose their profile and ordered prompt components")
  (mevedel-note-conversation-test--with-source
    (let ((view (mevedel-note-send (mevedel-note-create "Explain this code"))))
      (mevedel-note-conversation-test--answer (car sent) "An answer.")
      (with-current-buffer view
        (with-current-buffer (mevedel-inspect-effective-prompt)
          (should (string-search "Profile: main" (buffer-string)))
          (should (string-search "** workspace-config" (buffer-string)))
          (should-not (string-search "component breakdown unavailable" (buffer-string))))))))

(mevedel-deftest mevedel/default-preset
  (:quiet t :doc "a removed saved default falls back to the implement preset for a new chat")
  (mevedel-note-conversation-test--with-source
    (let ((mevedel-default-chat-preset 'discuss))
      (call-interactively #'mevedel)
      (let* ((view (window-buffer (selected-window)))
             (data (buffer-local-value 'mevedel--data-buffer view)))
        (should (eq 'mevedel-implement
                    (mevedel-session-preset-name (buffer-local-value 'mevedel--session data))))))))

(mevedel-deftest mevedel-note-send/chat-selection-isolation
  (:quiet t)
  ,test
  (test)
  :doc "ordinary chat entry never adopts a sent note's dedicated conversation"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "A dedicated question"))
           (note-view (mevedel-note-send note)))
      (mevedel-note-conversation-test--answer (car sent) "Only this note's answer")
      (with-current-buffer note-view (mevedel-note-conversation-close))
      (with-current-buffer source
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Note session offered in ordinary chat chooser"))))
          (call-interactively #'mevedel)))
      (should-not (eq note-view (window-buffer (selected-window))))
      (with-current-buffer source
        (should (eq note-view (mevedel-note-send note))))))
  :doc "ordinary saved selection excludes several saved note conversations"
  (mevedel-note-conversation-test--with-source
    (dotimes (_ 3)
      (let ((view (with-current-buffer source
                    (goto-char (point-min))
                    (forward-line 1)
                    (mevedel-note-send (mevedel-note-create "Dedicated question")))))
        (mevedel-note-conversation-test--answer (car sent) "Retained answer")
        (let ((data (buffer-local-value 'mevedel--data-buffer view)))
          (with-current-buffer data (mevedel-session-persistence-release-on-kill))
          (mevedel-chat-discard-buffers data t))))
    (with-current-buffer source
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Saved note session offered in ordinary chooser"))))
        (call-interactively #'mevedel))))
  :doc "an explicit fork remains an ordinary discoverable chat with a changeable preset"
  (mevedel-note-conversation-test--with-source
    (let* ((view (mevedel-note-send (mevedel-note-create "Forkable question")))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data)))
      (mevedel-note-conversation-test--answer (car sent) "Forkable answer")
      (let* ((target (cadr (assq 1 (mevedel-session-prompt-index session))))
             (child (mevedel-session-fork-create data target 'conversation)))
        (with-current-buffer view (mevedel-note-conversation-close))
        (with-current-buffer source
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt table &rest _)
                       (let ((choices (delete "Start new session" (all-completions "" table))))
                         (should (= 1 (length choices)))
                         (should (string-search
                                  (mevedel-session-session-id
                                   (buffer-local-value 'mevedel--session child))
                                  (car choices)))
                         (car choices)))))
            (call-interactively #'mevedel)))
        (should (eq child (buffer-local-value 'mevedel--data-buffer
                                             (window-buffer (selected-window)))))
        (with-current-buffer child
          (mevedel-preset-apply 'mevedel-implement)
          (should (eq 'mevedel-implement (mevedel-session-preset-name mevedel--session))))))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired/note
  (:quiet t)
  ,test
  (test)
  :doc "closed note sessions survive even an unreadable store and do not displace the ordinary retention floor"
  (mevedel-note-conversation-test--with-source
    (let* ((ordinary (mevedel--chat-buffer "ordinary" t workspace))
           (ordinary-session (buffer-local-value 'mevedel--session ordinary))
           (note (mevedel-note-create "Retain my conversation"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data)))
      (mevedel-note-conversation-test--answer (car sent) "Durable answer")
      (with-current-buffer ordinary
        (mevedel--insert-local-user-turn "An ordinary retained chat")
        (mevedel-session-artifacts-save ordinary-session ordinary nil t))
      (with-current-buffer source (mevedel-note-done note))
      (mevedel-instruction-store-flush t)
      (dolist (buffer (list ordinary data))
        (with-current-buffer buffer (mevedel-session-persistence-release-on-kill))
        (mevedel-chat-discard-buffers buffer t))
      (let* ((path (mevedel-session-artifacts-sidecar-path
                    (mevedel-session-save-path ordinary-session)))
             (sidecar (mevedel-session-codec-read path)))
        (mevedel-session-codec-write path (plist-put sidecar :updated-at "2001-01-01T00-00-00")))
      (write-region "unreadable instruction store" nil
                    (file-name-concat root ".mevedel" "instructions.el") nil 'silent)
      (let ((mevedel-session-max-age-days 0)
            (mevedel-session-keep-recent-count 1))
        (should (zerop (mevedel-session-persistence-cleanup-expired workspace t))))
      (should (file-directory-p (mevedel-session-save-path session)))
      (should (file-directory-p (mevedel-session-save-path ordinary-session))))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired/note-expiry
  (:quiet t)
  ,test
  (test)
  :doc "an expired closed note releases its conversation after reload without using the ordinary retention floor"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Collect after expiry"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data))
           (path (mevedel-session-save-path session)))
      (mevedel-note-conversation-test--answer (car sent) "An expiring answer")
      (with-current-buffer source (mevedel-note-done note))
      (let ((mevedel-notes-closed-max-count 0))
        (should-not (mevedel-notes-closed workspace)))
      (mevedel-instruction-store-flush t)
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (let ((mevedel-session-max-age-days 0)
            (mevedel-session-keep-recent-count 10)
            (read-file (symbol-function 'mevedel-session-control-fs-read-file))
            (store (file-name-concat root ".mevedel" "instructions.el"))
            (reads 0))
        (cl-letf (((symbol-function 'mevedel-session-control-fs-read-file)
                   (lambda (file &optional coding max-bytes)
                     (when (equal file store)
                       (cl-incf reads)
                       (should (<= max-bytes (1+ (* 16 1024 1024)))))
                     (funcall read-file file coding max-bytes))))
          (should (= 1 (mevedel-session-persistence-cleanup-expired workspace t))))
        (should (= 1 reads)))
      (should-not (file-directory-p path)))))

(mevedel-deftest mevedel-session-persistence-delete/note-live-and-stashed
  (:quiet t)
  ,test
  (test)
  :doc "deletion and cleanup retain dirty live and killed-buffer references absent from the disk store"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Still in use"))
           (store (file-name-concat root ".mevedel" "instructions.el"))
           (unsent (with-temp-buffer (insert-file-contents store) (buffer-string)))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data))
           (path (mevedel-session-save-path session))
           (mevedel-session-max-age-days 0)
           (mevedel-session-keep-recent-count 0))
      (mevedel-note-conversation-test--answer (car sent) "A retained answer")
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      ;; Another store version lacks the local unsaved session reference.
      (should (eq source (overlay-buffer note)))
      (write-region unsent nil store nil 'silent)
      (mevedel-instruction-store-touch workspace)
      (let ((dirty (copy-sequence mevedel-instruction-store--dirty)))
        (should-not (mevedel-session-persistence-delete workspace path))
        (should (= 0 (mevedel-session-persistence-cleanup-expired workspace t)))
        (should (equal dirty mevedel-instruction-store--dirty)))
      (kill-buffer source)
      (should-not (overlay-buffer note))
      (write-region unsent nil store nil 'silent)
      (should-not (mevedel-session-persistence-delete workspace path))
      (should (= 0 (mevedel-session-persistence-cleanup-expired workspace t)))
      (should (file-directory-p path)))))

(mevedel-deftest mevedel-note-restore/collected-session
  (:quiet t)
  ,test
  (test)
  :doc "a closed note survives cleanup after reload and reopens its saved conversation"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Restore with conversation"))
           (uuid (overlay-get note 'mevedel-uuid))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data))
           (path (mevedel-session-save-path session)))
      (mevedel-note-conversation-test--answer (car sent) "An answer after restoration")
      (with-current-buffer source (mevedel-note-done note))
      (mevedel-instruction-store-flush t)
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (let ((mevedel-session-max-age-days 0)
            (mevedel-session-keep-recent-count 0))
        (should (= 0 (mevedel-session-persistence-cleanup-expired workspace t))))
      (should-not (mevedel-session-persistence-delete workspace path))
      (with-current-buffer source
        (setq view (mevedel-note-send (mevedel-note-restore uuid workspace))))
      (should (= 1 (length sent)))
      (mevedel-test--await 5 "Restored note answer did not render"
        (progn
          ;; Restored views defer rendering until redisplay observes their window.
          (redisplay t)
          (with-current-buffer view
            (string-search "An answer after restoration" (buffer-string))))))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired/note-uncertainty
  (:quiet t)
  ,test
  (test)
  :doc "an otherwise collectable conversation survives corrupt, unsupported, absent and unloaded stores"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Fail safe"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data))
           (path (mevedel-session-save-path session))
           (store (file-name-concat root ".mevedel" "instructions.el"))
           (mevedel-session-max-age-days 0)
           (mevedel-session-keep-recent-count 0))
      (mevedel-note-conversation-test--answer (car sent) "Keep when uncertain")
      (with-current-buffer source (mevedel-note-done note))
      (let ((mevedel-notes-closed-max-count 0))
        (should-not (mevedel-notes-closed workspace)))
      (mevedel-instruction-store-flush t)
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      (let ((valid (with-temp-buffer (insert-file-contents store) (buffer-string))))
        (dolist (contents '("broken store" "(:format 999)" "(:format 1)" nil))
          (if contents
              (write-region contents nil store nil 'silent)
            (delete-file store))
          (should (= 0 (mevedel-session-persistence-cleanup-expired workspace t)))
          (should-not (mevedel-session-persistence-delete workspace path))
          (should (file-directory-p path)))
        (write-region valid nil store nil 'silent)
        (mevedel--clear-instruction-state workspace)
        (should (= 0 (mevedel-session-persistence-cleanup-expired workspace t)))
        (should-not (mevedel-session-persistence-delete workspace path))
        (mevedel-instruction-store-observe workspace)
        (should (mevedel-session-persistence-delete workspace path))
        (should-not (file-directory-p path))))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired/note-remote
  (:quiet t)
  ,test
  (test)
  :doc "a remote reference check retains the conversation without probing its store"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Remote reference safety"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data))
           (path (mevedel-session-save-path session))
           (remote (copy-mevedel-workspace workspace))
           (probes 0))
      (mevedel-note-conversation-test--answer (car sent) "No remote store I/O")
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      (setf (mevedel-workspace-root remote) "/notes-remote:/project/")
      (let* ((handler (lambda (operation &rest _)
                        (if (eq operation 'file-remote-p)
                            "/notes-remote:"
                          (cl-incf probes)
                          (error "Unexpected remote probe"))))
             (file-name-handler-alist
              (cons (cons "\\`/notes-remote:" handler) file-name-handler-alist))
             (mevedel-session-max-age-days 0)
             (mevedel-session-keep-recent-count 0))
        (should (= 0 (mevedel-session-persistence-cleanup-expired remote t)))
        (should-not (mevedel-session-persistence-delete remote path)))
      (should (= 0 probes))
      (should (file-directory-p path)))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired/note-fork
  (:quiet t)
  ,test
  (test)
  :doc "an explicit note-session fork follows ordinary cleanup while its original stays referenced"
  (mevedel-note-conversation-test--with-source
    (let* ((view (mevedel-note-send (mevedel-note-create "Fork and collect")))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data)))
      (mevedel-note-conversation-test--answer (car sent) "Forked answer")
      (let* ((target (cadr (assq 1 (mevedel-session-prompt-index session))))
             (child (mevedel-session-fork-create data target 'conversation))
             (child-session (buffer-local-value 'mevedel--session child))
             (child-path (mevedel-session-save-path child-session)))
        (dolist (buffer (list data child))
          (with-current-buffer buffer (mevedel-session-persistence-release-on-kill))
          (mevedel-chat-discard-buffers buffer t))
        (let ((mevedel-session-max-age-days 0)
              (mevedel-session-keep-recent-count 0))
          (should (= 1 (mevedel-session-persistence-cleanup-expired workspace t))))
        (should-not (file-directory-p child-path))
        (should (file-directory-p (mevedel-session-save-path session)))))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired/note-missing-metadata
  (:quiet t)
  ,test
  (test)
  :doc "corrupt or missing session metadata cannot erase a referenced conversation"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Keep despite metadata loss"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (session (buffer-local-value 'mevedel--session data))
           (path (mevedel-session-save-path session))
           (sidecar (mevedel-session-artifacts-sidecar-path path))
           (mevedel-session-max-age-days 0)
           (mevedel-session-keep-recent-count 0))
      (mevedel-note-conversation-test--answer (car sent) "A recoverable answer")
      (with-current-buffer source (mevedel-note-done note))
      (mevedel-instruction-store-flush t)
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      (dolist (contents '("(" "(:version \"unknown\")"
                         "(:note-conversation t)" nil))
        (if contents
            (write-region contents nil sidecar nil 'silent)
          (delete-file sidecar))
        (should-not (mevedel-session-persistence-delete workspace path))
        (should (= 0 (mevedel-session-persistence-cleanup-expired workspace t)))
        (should (file-directory-p path)))
      (let ((mevedel-notes-closed-max-count 0))
        (should-not (mevedel-notes-closed workspace)))
      (mevedel-instruction-store-flush t)
      ;; A recently settled session can still receive control-program telemetry.
      (let ((mevedel-telemetry-enabled t)
            (mevedel-telemetry--lag-windows (list (list session)))
            (mevedel-session-control-fs--measure-min-ms 0))
        (should (= 1 (mevedel-session-persistence-cleanup-expired workspace t)))
        (should-not (file-directory-p path))))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired/ordinary-no-note-read
  (:quiet t)
  ,test
  (test)
  :doc "ordinary cleanup and deletion add no instruction-store reads"
  (mevedel-note-conversation-test--with-source
    (let ((store (file-name-concat root ".mevedel" "instructions.el"))
          (read-file (symbol-function 'mevedel-session-control-fs-read-file))
          (reads 0)
          paths)
      (dotimes (n 2)
        (let* ((data (mevedel--chat-buffer (format "ordinary-%d" n) t workspace))
               (session (buffer-local-value 'mevedel--session data)))
          (with-current-buffer data
            (mevedel--insert-local-user-turn "Ordinary conversation")
            (mevedel-session-artifacts-save session data nil t)
            (mevedel-session-persistence-release-on-kill))
          (push (mevedel-session-save-path session) paths)
          (mevedel-chat-discard-buffers data t)))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-read-file)
                 (lambda (file &optional coding max-bytes)
                   (when (equal file store) (cl-incf reads))
                   (funcall read-file file coding max-bytes))))
        (should (mevedel-session-persistence-delete workspace (pop paths)))
        (let ((mevedel-session-max-age-days 0)
              (mevedel-session-keep-recent-count 0))
          (should (= 1 (mevedel-session-persistence-cleanup-expired workspace t)))))
      (should (= 0 reads)))))

(mevedel-deftest mevedel-chat-discard-buffers/note-source
  (:quiet t)
  ,test
  (test)
  :doc "discarding a note conversation leaves its visited source note live and unstashed"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Keep the source label"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view)))
      (mevedel-note-conversation-test--answer (car sent) "Only the conversation closes")
      (should (eq source (overlay-buffer note)))
      (mevedel-chat-discard-buffers data t)
      (should-not (buffer-live-p data))
      (should-not (buffer-live-p view))
      (should (buffer-live-p source))
      (should (eq source (overlay-buffer note)))
      (let ((entries (plist-get (gethash (mevedel--instruction-workspace-key workspace)
                                        mevedel--instruction-states)
                                :instructions)))
        (should (memq note (cdr (assq source entries))))
        (should-not (assoc file entries)))
      (with-current-buffer source
        (should (equal "outside-before\nselected code\noutside-after\n" (buffer-string)))))))

(mevedel-deftest mevedel-note-send/frame-reuse
  (:quiet t)
  ,test
  (test)
  :doc "a graphical frame reused from another conversation shows the note answer"
  (progn
   (skip-unless (display-graphic-p))
  (mevedel-note-conversation-test--with-source
    (let* ((prior (save-excursion
                    (forward-line -1)
                    (mevedel-note-create "Prior note")))
           (old-data (mevedel--chat-buffer "prior" t workspace))
           (old-view (buffer-local-value 'mevedel--view-buffer old-data)))
      (unwind-protect
          (progn
            (mevedel-view-test--insert-data old-data "Prior question\n\n" nil)
            (mevedel-view-test--insert-data old-data "Prior answer\n" 'response)
            (with-current-buffer old-view (mevedel-view--full-rerender))
            (mevedel-note-frame-display prior old-view)
            (let* ((frame mevedel-note-frame--frame)
                   (view (mevedel-note-send (mevedel-note-create "New note question"))))
              (should (frame-live-p frame))
              (should (eq frame mevedel-note-frame--frame))
              (mevedel-note-conversation-test--answer (car sent) "Visible note answer")
              (with-current-buffer view
                (goto-char (point-min))
                (should (search-forward "Visible note answer" nil t))
                (should-not (invisible-p (1- (point)))))))
        (when (frame-live-p mevedel-note-frame--frame)
          (mevedel-note-frame-close)))))))

(mevedel-deftest mevedel-note-send/edited-file-isolation
  (:quiet t)
  ,test
  (test)
  :doc "send does not inject unrelated changed files remembered by another chat"
  (mevedel-note-conversation-test--with-source
    (let ((other (file-name-concat root "other.txt")))
      (write-region "old unrelated content" nil other nil 'silent)
      (mevedel-file-cache-put (mevedel-workspace-file-cache workspace)
                              (mevedel-file-state-from-file other))
      (write-region "UNRELATED-EDIT-SECRET" nil other nil 'silent)
      (set-file-times other (time-add (current-time) 2))
      (mevedel-note-send (mevedel-note-create "Only this selected code"))
      (mevedel-test--await 5 "Note request did not reach transport" sent)
      (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should-not (string-search "UNRELATED-EDIT-SECRET" payload))
        (should-not (string-search "other.txt" payload)))
      (mevedel-note-conversation-test--answer (car sent) "Only this note."))))

(mevedel-deftest mevedel-note-send/mentioned-notes
  (:quiet t)
  ,test
  (test)
  :doc "authored mentions freeze once; captured code and attached text cannot activate more notes"
  (mevedel-note-conversation-test--with-source
    (forward-line 1)
    (let ((unmentioned (mevedel-note-create "UNMENTIONED-TEXT"))
          (secret (file-name-concat root "not-requested.txt")))
      (write-region "UNREQUESTED-FILE-SECRET" nil secret nil 'silent)
      (goto-char (point-min))
      (let ((context (mevedel-note-create
                      (format "MENTIONED-TEXT @note:%s @file:%s"
                              (overlay-get unmentioned 'mevedel-id) secret))))
        (forward-line 1)
        (end-of-line)
        (insert (format " @note:%s @file:%s" (overlay-get unmentioned 'mevedel-id) secret))
        (beginning-of-line)
        (let* ((note (mevedel-note-create
                      (format "Explain @note:%s" (overlay-get context 'mevedel-id))))
               (view (mevedel-note-send note))
               (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
          (should (string-search "MENTIONED-TEXT" payload))
          (should (string-search "outside-before" payload))
          (should-not (string-search "UNMENTIONED-TEXT" payload))
          (should-not (string-search "UNREQUESTED-FILE-SECRET" payload))
          (mevedel-note-conversation-test--answer (car sent) "Frozen answer")
          (with-current-buffer source
            (mevedel-note-edit "CHANGED-MENTION-CONTENT" context)
            (save-buffer)
            (mevedel-instruction-store-flush t))
          (let ((data (buffer-local-value 'mevedel--data-buffer view)))
            (with-current-buffer data (mevedel-session-persistence-release-on-kill))
            (mevedel-chat-discard-buffers data t))
          (kill-buffer source)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local mevedel--workspace workspace)
            (mevedel-instruction-store-observe workspace)
            (goto-char (point-min))
            (forward-line 1)
            (setq view (mevedel-note-send)))
          (with-current-buffer (mevedel-view-composer-buffer view)
            (mevedel-view-composer-insert "Follow up without a new mention")
            (call-interactively (key-binding (kbd "C-c C-c"))))
          (let ((next (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
            (should (string-search "MENTIONED-TEXT" next))
            (should-not (string-search "CHANGED-MENTION-CONTENT" next))
            (should-not (string-search "UNREQUESTED-FILE-SECRET" next))
            (should-not (string-search "UNMENTIONED-TEXT" next)))
          (mevedel-note-conversation-test--answer (car sent) "Still frozen"))))))

(mevedel-deftest mevedel-note-send/literal-code
  (:quiet t)
  ,test
  (test)
  :doc "freezing does not alter ordinary at-sign decorators in captured code"
  (mevedel-note-conversation-test--with-source
    (insert "@property @Override @param @media ")
    (beginning-of-line)
    (mevedel-note-send (mevedel-note-create "Explain these annotations"))
    (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
      (should (string-search "@property @Override @param @media selected code" payload)))
    (mevedel-note-conversation-test--answer (car sent) "Annotations retained")))

(mevedel-deftest mevedel-note-send/request-mentions
  (:quiet t)
  ,test
  (test)
  :doc "authored file media uses request-time expansion without persistent gptel context"
  (mevedel-note-conversation-test--with-source
    (let ((image (file-name-concat root "image.png")))
      (write-region "note image bytes" nil image nil 'silent)
      (cl-letf (((symbol-plist 'test-model)
                 '(:capabilities (media) :mime-types ("image/png"))))
        (let* ((note (mevedel-note-create (concat "Explain @file:" image)))
               (view (mevedel-note-send note))
               (data (buffer-local-value 'mevedel--data-buffer view))
               (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
          (should (string-search "data:image/png;base64," payload))
          (with-current-buffer data
            (should-not gptel-context)
            (should-not gptel-use-context))
          (mevedel-note-conversation-test--answer (car sent) "Image explained"))))))

(mevedel-deftest mevedel-notes-list-send
  (:quiet t)
  ,test
  (test)
  :doc "list send focuses the note conversation and projects working then sent without a second request"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Send from the list"))
           (uuid (overlay-get note 'mevedel-uuid))
           (list-buffer (mevedel-notes-list workspace))
           view)
      (with-current-buffer list-buffer
        (goto-char (point-min))
        (while (and (not (eobp)) (not (equal uuid (tabulated-list-get-id)))) (forward-line 1))
        (should (equal uuid (tabulated-list-get-id)))
        (should (commandp (key-binding "s")))
        (setq view (call-interactively (key-binding "s"))))
      (should (eq view (window-buffer (selected-window))))
      (with-current-buffer list-buffer
        (mevedel-notes-list-refresh)
        (should (string-search "working" (buffer-string))))
      (mevedel-note-conversation-test--answer (car sent) "List answer")
      (with-current-buffer list-buffer
        (mevedel-notes-list-refresh)
        (should-not (string-search "working" (buffer-string)))
        (goto-char (point-min))
        (while (and (not (eobp)) (not (equal uuid (tabulated-list-get-id)))) (forward-line 1))
        (should (equal uuid (tabulated-list-get-id)))
        (should (eq view (call-interactively (key-binding "s")))))
      (should (= 1 (length sent))))))

(mevedel-deftest mevedel-note-send-batch
  (:quiet t)
  ,test
  (test)
  :doc "batch sends two notes into one durable conversation and either note reopens it"
  (mevedel-note-conversation-test--with-source
    (let ((first (mevedel-note-create "First question")))
      (forward-line 1)
      (let* ((second (mevedel-note-create "Related question"))
             (view (mevedel-note-send-batch (list first second)))
             (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should (= 1 (length sent)))
        (dolist (text '("First question" "Related question" "selected code" "outside-after"))
          (should (string-search text payload)))
        (mevedel-note-conversation-test--answer (car sent) "One answer for both.")
        (with-current-buffer source
          (should (eq view (mevedel-note-send first)))
          (should (eq view (mevedel-note-send second)))
          (should (eq view (mevedel-note-send-batch (list first second)))))
        (should (= 1 (length sent))))))
  :doc "interactive batch choices keep comma prose in annotations, not candidate IDs"
  (mevedel-note-conversation-test--with-source
    (let ((note (mevedel-note-create "Rename foo, then update callers")))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_prompt collection &rest _)
                   (let ((id (number-to-string (overlay-get note 'mevedel-id))))
                     (should (equal (list id) (all-completions "" collection)))
                     (should (string-search
                              "Rename foo, then update callers"
                              (funcall (plist-get completion-extra-properties :annotation-function) id)))
                     (list id)))))
        (let ((view (call-interactively #'mevedel-note-send-batch)))
          (should (buffer-live-p view))
          (should (= 1 (length sent)))
          (mevedel-note-conversation-test--answer (car sent) "Renaming advice.")))))
  :doc "batch refuses mixing existing conversations without losing either thread"
  (mevedel-note-conversation-test--with-source
    (let* ((first (mevedel-note-create "First"))
           (first-view (mevedel-note-send first))
           second second-view)
      (mevedel-note-conversation-test--answer (car sent) "First answer")
      (with-current-buffer source
        (forward-line 1)
        (setq second (mevedel-note-create "Second")
              second-view (mevedel-note-send second)))
      (mevedel-note-conversation-test--answer (car sent) "Second answer")
      (with-current-buffer source
        (should-error (mevedel-note-send-batch (list first second)) :type 'user-error)
        (should (eq first-view (mevedel-note-send first)))
        (should (eq second-view (mevedel-note-send second)))))))

(mevedel-deftest mevedel-note-conversation-reply
  (:quiet t)
  ,test
  (test)
  :doc "one-key reply uses its own history and background answer preserves the next draft"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Explain this?"))
           (view (mevedel-note-send note)))
      (mevedel-note-conversation-test--answer (car sent) "First answer.")
      (with-current-buffer (mevedel-view-composer-buffer view)
        (mevedel-view-composer-insert "And why?")
        (call-interactively (key-binding (kbd "C-c C-c"))))
      (should (= 2 (length sent)))
      (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should (string-search "First answer." payload))
        (should (string-search "And why?" payload)))
      (with-current-buffer view
        (mevedel-view-composer-insert "> multiline draft\nsecond line")
        (let ((position (mevedel-view-test--composer-point-offset))
              (start (window-start))
              (window (selected-window)))
          (mevedel-note-conversation-test--answer (car sent) "Second answer.")
          (should (eq window (selected-window)))
          (should (= position (mevedel-view-test--composer-point-offset)))
          (should (= start (window-start)))
          (should (equal "> multiline draft\nsecond line"
                         (mevedel-view-composer-draft-text))))))))

(mevedel-deftest mevedel-note-conversation-restart
  (:quiet t)
  ,test
  (test)
  :doc "a fresh Emacs reopens the saved note's session and original sent input"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Question before restart?"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (script (file-name-concat root "restart.el")))
      (mevedel-note-conversation-test--answer (car sent) "Answer survives restart.")
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      (write-region
       (concat ";;; restart.el -*- lexical-binding: t -*-\n" (prin1-to-string
        `(progn
           (setq load-path ',load-path)
           (require 'mevedel)
           (require 'mevedel-notes-activation)
           (mevedel-notes-global-mode 1)
           (require 'gptel-openai)
           (require 'mevedel-persistence)
           (setq gptel-backend (gptel-make-openai "Notes" :key "test" :models '(test-model))
                 gptel-model 'test-model
                 mevedel-sessions-directory ,mevedel-sessions-directory)
           (mevedel-install)
           (let* ((workspace (mevedel-workspace-get-or-create 'file ,root ,root "notes"))
                  (source (find-file-noselect ,file)))
             (switch-to-buffer source)
             (setq-local mevedel--workspace workspace)
             (mevedel-instruction-store-observe workspace)
             (goto-char (point-min))
             (forward-line 1)
             (let ((view (mevedel-note-send)))
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

(mevedel-deftest mevedel-note-send/native
  (:quiet t)
  ,test
  (test)
  :doc "native note session retains its exact initial context including ordinary capabilities"
  (mevedel-note-conversation-test--with-source
    (let ((mevedel-claude-code-directory (file-name-concat root "claude"))
          (acp--shutdown-grace-period 0)
          system prompts view data)
      (make-directory (file-name-concat root ".mevedel" "memory") t)
      (write-region "NATIVE-MEMORY-SECRET" nil
                    (file-name-concat root ".mevedel" "memory" "MEMORY.md") nil 'silent)
      (cl-letf (((symbol-function 'mevedel-claude-code-unavailable-reason) #'ignore)
                ((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (prompt &rest _)
                    (setq system prompt)
                    (list :meta '((echoAllText . t)))))))
        (mevedel-claude-code-register)
        (let ((gptel-backend (gptel-get-backend "Claude Code"))
              (gptel-model 'sonnet))
          (advice-add 'mevedel-acp-prompt :before
                      (lambda (_client input &rest _) (push input prompts))
                      '((name . mevedel-note-native-test)))
          (unwind-protect
              (progn
                (setq view (mevedel-note-send (mevedel-note-create "Native question?"))
                      data (buffer-local-value 'mevedel--data-buffer view))
                (mevedel-test--await 10 "Native note did not settle"
                  (not (mevedel-turn-busy-p data)))
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
            (advice-remove 'mevedel-acp-prompt 'mevedel-note-native-test)))))))

(mevedel-deftest mevedel-note-preparation-during-turn
  (:quiet t)
  ,test
  (test)
  :doc "a disjoint saved edit during the AI turn survives preparation and later apply"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Improve this code"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           result)
      (should (mevedel-turn-busy-p data))
      (with-current-buffer source
        (goto-char (point-min))
        (insert "my ")
        (save-buffer))
      (with-current-buffer data
        (mevedel-tool-patch-handler
         (lambda (value) (setq result value))
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+prepared proposal\n*** End Patch")))
      (should (string-search "Prepared" (plist-get result :result)))
      (mevedel-note-conversation-test--answer (car sent) "The change is ready when you are.")
      (should-not (mevedel-turn-busy-p data))
      (should (eq 'ready (mevedel-notes-attention-state note)))
      (with-current-buffer view
        (call-interactively (key-binding (kbd "C-c C-a"))))
      (should (equal "my outside-before\nprepared proposal\noutside-after\n"
                     (mevedel-test--read-file file))))))

(mevedel-deftest mevedel-tool-note-write/rewind (:quiet t)
  ,test
  (test)
  :doc "a suggestion created during the active note request belongs to its discarded turn"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Review this code and leave a suggestion"))
           (note-uuid (overlay-get note 'mevedel-uuid))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           outcome suggestion suggestion-uuid)
      (should (mevedel-turn-busy-p data))
      (with-current-buffer data
        (mevedel-pipeline-run-tool-outcome
         (mevedel-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
         (list :operations
               (vector (list :action "create" :file_path file :start_line 2
                             :text "A suggested fix" :prepared_change
                             "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch"))))
        (mevedel-test--await 5 "NoteWrite suggestion did not settle" outcome))
      (ert-info ((format "%S" outcome))
        (should (eq 'success (plist-get outcome :status))))
      (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get outcome :result)))
      (setq suggestion-uuid (match-string 1 (plist-get outcome :result))
            suggestion (mevedel-note-resolve suggestion-uuid workspace))
      (mevedel-note-conversation-test--answer (car sent) "The suggestion is ready.")
      (mevedel-note-apply-change suggestion)
      (should (equal "outside-before\nsuggested code\noutside-after\n"
                     (mevedel-test--read-file file)))
      (with-current-buffer view
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _) (car (all-completions "" collection))))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?f "conversation and files")))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (mevedel-rewind)))
      (should (equal "outside-before\nselected code\noutside-after\n"
                     (mevedel-test--read-file file)))
      (setq note (mevedel-note-resolve note-uuid workspace))
      (with-current-buffer source
        (should (= 1 (length (mevedel--instructions-in (point-min) (point-max))))))
      (should-not (mevedel-note-resolve suggestion-uuid workspace))
      (mevedel-note-send note)
      (with-current-buffer data
        (mevedel-tool-patch-handler
         #'ignore
         '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+new turn code\n*** End Patch")))
      (mevedel-note-conversation-test--answer (car sent) "A different change in the reused turn.")
      (mevedel-note-apply-change (mevedel-note-resolve note-uuid workspace))
      (should (equal "outside-before\nnew turn code\noutside-after\n"
                     (mevedel-test--read-file file)))
      (should-error (mevedel-note-revert-change suggestion-uuid nil workspace)
                    :type 'user-error)
      (should (equal "outside-before\nnew turn code\noutside-after\n"
                     (mevedel-test--read-file file))))))

(mevedel-deftest mevedel-tool-note-write/rewind-waiting (:quiet t)
  ,test
  (test)
  :doc "Rewind clears a main chat's waiting suggestion without a note conversation link"
  (dolist (closed '(nil t))
    (mevedel-note-conversation-test--with-source
      (let* ((main (mevedel--chat-buffer "main" t workspace))
             (view (buffer-local-value 'mevedel--view-buffer main))
             outcome)
        (with-current-buffer view
          (mevedel-view-composer-insert "Suggest a fix")
          (with-current-buffer (mevedel-view-composer-buffer)
            (mevedel-view-send)))
        (with-current-buffer main
          (mevedel-pipeline-run-tool-outcome
           (mevedel-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
           (list :operations
                 (vector (list :action "create" :file_path file :start_line 2
                               :text "Main chat suggestion" :prepared_change
                               "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch"))))
          (mevedel-test--await 5 "NoteWrite did not settle" outcome))
        (should (eq 'success (plist-get outcome :status)))
        (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get outcome :result)))
        (let* ((uuid (match-string 1 (plist-get outcome :result)))
               (suggestion (mevedel-note-resolve uuid workspace)))
          (should-not (overlay-get suggestion 'mevedel-note-session-id))
          (should (eq 'ready (mevedel-notes-attention-state suggestion)))
          (mevedel-note-conversation-test--answer (car sent) "The suggestion is ready.")
          (when closed (mevedel-note-done suggestion))
          (with-current-buffer view
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _) (car (all-completions "" collection))))
                      ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (mevedel-rewind)))
          (when closed (mevedel-note-restore uuid workspace))
          (setq suggestion (mevedel-note-resolve uuid workspace))
          (should-not (eq 'ready (mevedel-notes-attention-state suggestion)))
          (should-error (mevedel-note-apply-change suggestion) :type 'user-error)
          (should (equal "outside-before\nselected code\noutside-after\n"
                         (mevedel-test--read-file file))))))))

(mevedel-deftest mevedel-tool-note-write/send (:quiet t)
  ,test
  (test)
  :doc "user-requested batch send leaves the current window, point and composer alone"
  (mevedel-note-conversation-test--with-source
    (let* ((first (mevedel-note-create "First requested note"))
           (second (mevedel-note-create "Second requested note"))
           (main (mevedel--chat-buffer "main" t workspace))
           (view (buffer-local-value 'mevedel--view-buffer main))
           (syncs 0)
           (counter (lambda (&rest _) (cl-incf syncs)))
           outcome)
      (switch-to-buffer view)
      (goto-char (point-max))
      (insert "> main draft\ncontinued")
      (let ((text (buffer-string)) (position (point))
            (window (selected-window)) (start (window-start)))
        (advice-add 'mevedel-instruction-store-sync :before counter)
        (unwind-protect
            (with-current-buffer main
              (mevedel-pipeline-run-tool-outcome
               (mevedel-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
               (list :operations
                     (vector (list :action "send" :id (overlay-get first 'mevedel-uuid))
                             (list :action "send" :id (overlay-get second 'mevedel-uuid)))))
              (mevedel-test--await 5 "NoteWrite send did not settle" outcome)
              (ert-info ((format "%S" outcome))
                (should (eq 'success (plist-get outcome :status)))))
          (advice-remove 'mevedel-instruction-store-sync counter))
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
      (mevedel-note-conversation-test--answer (car sent) "Batch answer."))))

(mevedel-deftest mevedel-tool-note-write/review (:quiet t)
  ,test
  (test)
  :doc "reviewing a created suggestion opens review without sending the unsent note"
  (mevedel-note-conversation-test--with-source
    (let ((main (mevedel--chat-buffer "main" t workspace)) outcome)
      (with-current-buffer main
        (mevedel-pipeline-run-tool-outcome
         (mevedel-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
         (list :operations
               (vector (list :action "create" :file_path file :start_line 2 :text "Suggestion"
                             :prepared_change
                             "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch")))))
      (mevedel-test--await 5 "NoteWrite did not settle" outcome)
      (should (eq 'success (plist-get outcome :status)))
      (let ((note (car (mevedel--all-instructions))))
        (mevedel-note-apply-change note t)
        (should-not sent)
        (should (eq 'unsent (overlay-get note 'mevedel-note-state)))
        (should (equal "outside-before\nselected code\noutside-after\n" (mevedel-test--read-file file)))))))

(mevedel-deftest mevedel-tool-note-write/nested-match
  (:quiet t :doc "a matched child is previewed, sent and closed with its whole-line parent")
  (mevedel-note-conversation-test--with-source
    (let ((parent (mevedel-note-create "Parent brief"))
          (main (mevedel--chat-buffer "main" t workspace)) outcome)
      (with-current-buffer main
        (mevedel-pipeline-run-tool-outcome
         (mevedel-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
         (list :operations (vector (list :action "create" :file_path file :start_line 2
                                         :match "selected" :text "Matched child brief")))))
      (mevedel-test--await 5 "NoteWrite did not settle" outcome)
      (should (eq 'success (plist-get outcome :status)))
      (let* ((child (car (mevedel--child-instructions parent)))
             (child-id (overlay-get child 'mevedel-uuid))
             (preview (with-current-buffer (mevedel-note-preview-prompt parent) (buffer-string)))
             (view (mevedel-note-send parent :no-display t))
             (data (buffer-local-value 'mevedel--data-buffer view)))
        (should (string-search "Matched child brief" preview))
        (should (equal (overlay-get parent 'mevedel-note-session-id)
                       (overlay-get child 'mevedel-note-session-id)))
        (should (string-search "Matched child brief"
                               (decode-coding-string (car sent-payloads) 'utf-8-unix)))
        (mevedel-note-conversation-test--answer (car sent) "An answer.")
        (with-current-buffer data
          (mevedel-tool-patch-handler
           #'ignore '(:patch "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+selected text\n*** End Patch")))
        (mevedel-note-apply-change parent)
        (should-not (overlay-buffer parent))
        (should-not (overlay-buffer child))
        (should (cl-find child-id (mevedel-notes-closed workspace)
                         :key (lambda (entry)
                                (plist-get (plist-get (plist-get entry :instruction) :properties)
                                           'mevedel-uuid))
                         :test #'equal))))))

(mevedel-deftest mevedel-note-send/store-write (:quiet t)
  ,test
  (test)
  :doc "ordinary send persists its sent state in one store write without a caller flush"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Send normally"))
           (writes 0)
           (counter (lambda (outcome)
                      (when (memq outcome '(written merged replaced)) (cl-incf writes))
                      outcome)))
      (advice-add 'mevedel-instruction-store-sync :filter-return counter)
      (unwind-protect (mevedel-note-send note)
        (advice-remove 'mevedel-instruction-store-sync counter))
      (should (= 1 writes))
      (mevedel-note-conversation-test--answer (car sent) "Normal answer.")
      (kill-buffer source)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (let ((restored (mevedel-note-resolve (overlay-get note 'mevedel-uuid) workspace)))
        (should (eq 'sent (overlay-get restored 'mevedel-note-state)))))))

(mevedel-deftest mevedel-note-send/flush-point (:quiet t)
  ,test
  (test)
  :doc "a send flushes once, after every note is already sent; a deferred send never flushes"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Flush order"))
           (other (progn (goto-char (point-min)) (mevedel-note-create "Deferred")))
           states
           (spy (lambda (&rest _)
                  (push (list (overlay-get note 'mevedel-note-state)
                              (overlay-get other 'mevedel-note-state))
                        states))))
      (advice-add 'mevedel-instruction-store-flush :before spy)
      (unwind-protect
          (progn (mevedel-note-send note :no-display t)
                 (should (equal '((sent unsent)) states))
                 (setq states nil)
                 (mevedel-note-send other :no-display t :defer-write t)
                 (should (eq 'sent (overlay-get other 'mevedel-note-state)))
                 (should-not states))
        (advice-remove 'mevedel-instruction-store-flush spy)))))

(mevedel-deftest mevedel-note-conversation/hint-after-transitions (:quiet t)
  ,test
  (test)
  :doc "the composer hint follows hold-change and record-applied, and ignores an unlinked suggestion"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Review this code and leave a suggestion"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (sid (overlay-get note 'mevedel-note-session-id))
           (record (list :id "r" :session-id sid :turn 1))
           outcome)
      (cl-flet ((hint () (substring-no-properties
                          (buffer-local-value 'mevedel-view--input-hint view)))
                (label () (overlay-get (overlay-get note 'mevedel-label-overlay)
                                       'after-string)))
        (should (equal "C-c C-z close" (hint)))
        ;; A suggestion made from inside the conversation has no session link.
        (with-current-buffer data
          (mevedel-pipeline-run-tool-outcome
           (mevedel-tool-ensure "NoteWrite") (lambda (value) (setq outcome value))
           (list :operations
                 (vector (list :action "create" :file_path file :start_line 2
                               :text "A suggested fix" :prepared_change
                               "*** Begin Patch\n*** Update File: source.el\n@@\n-selected code\n+suggested code\n*** End Patch")))))
        (should (equal "C-c C-z close" (hint)))
        (overlay-put note 'mevedel-note-labels 'expanded)
        (mevedel-note-hold-change (list note) record)
        (should (string-search "C-c C-a apply" (hint)))
        (should (string-search "change ready" (label)))
        (mevedel-note-record-applied record nil workspace)
        (should (equal "C-c C-z close" (hint)))
        (should-not (string-search "change ready" (or (label) "")))))))

(defmacro mevedel-note-conversation-test--with-tree (&rest body)
  "Run BODY with three nested notes in the ordinary send fixture."
  (declare (indent 0))
  `(mevedel-note-conversation-test--with-source
     (erase-buffer)
     (insert "one\ntwo\nthree\nfour\nfive\nsix\nseven\n")
     (save-buffer)
     (let ((parent (mevedel-note-make source 1 (point-max) :text "Parent brief"))
           (child (mevedel-note-make source 5 29 :text "Child brief"))
           (grandchild (mevedel-note-make source 9 24 :text "Grandchild brief")))
       ,@body)))

(mevedel-deftest mevedel-note-conversation--tree
  (:quiet t)
  ,test
  (test)
  :doc "all depths are included with immediate parents, never ancestors of the root"
  (mevedel-note-conversation-test--with-tree
    (should (equal (list parent child grandchild)
                   (mapcar (lambda (entry) (plist-get entry :note))
                           (mevedel-note-conversation--tree parent))))
    (let ((tree (mevedel-note-conversation--tree child)))
      (should (equal (list child grandchild)
                     (mapcar (lambda (entry) (plist-get entry :note)) tree)))
      (should (eq child (plist-get (cadr tree) :parent)))))
  :doc "an independent conversation excludes its entire subtree"
  (mevedel-note-conversation-test--with-tree
    (overlay-put child 'mevedel-note-session-id "independent")
    (let ((tree (mevedel-note-conversation--tree parent)))
      (should (= 2 (length tree)))
      (should (plist-get (cadr tree) :excluded)))))

(mevedel-deftest mevedel-note-conversation--batch-tree
  (:quiet t :doc "batch roots dedupe descendants regardless of selection order")
  (mevedel-note-conversation-test--with-tree
    (should (equal (mevedel-note-conversation--tree parent)
                   (mevedel-note-conversation--batch-tree
                    (list grandchild child parent child))))
    (overlay-put child 'mevedel-note-session-id "independent")
    (should (equal (append (mevedel-note-conversation--tree parent)
                           (mevedel-note-conversation--tree child))
                   (mevedel-note-conversation--batch-tree
                    (list parent child grandchild))))))

(mevedel-deftest mevedel-note-conversation--prompt
  (:quiet t)
  ,test
  (test)
  :doc "only the root has code; every descendant names its immediate parent"
  (mevedel-note-conversation-test--with-tree
    (let* ((session (mevedel-session-create "Preview" workspace root))
           (prompt (mevedel-note-conversation--prompt parent session)))
      (should (= 6 (cl-count ?` prompt)))
      (should (string-search (format "Note %s at line 2, inside Note %s\n\nChild brief"
                                     (overlay-get child 'mevedel-id)
                                     (overlay-get parent 'mevedel-id)) prompt))
      (should (string-search (format "Note %s at line 3, inside Note %s\n\nGrandchild brief"
                                     (overlay-get grandchild 'mevedel-id)
                                     (overlay-get child 'mevedel-id)) prompt))))
  :doc "excluded child is one exact line without its text or descendants"
  (mevedel-note-conversation-test--with-tree
    (overlay-put child 'mevedel-note-session-id "independent")
    (let ((prompt (mevedel-note-conversation--prompt
                   parent (mevedel-session-create "Preview" workspace root))))
      (should (string-search
               (format "Note %s at line 2, inside Note %s, has its own conversation and is not part of this request."
                       (overlay-get child 'mevedel-id) (overlay-get parent 'mevedel-id)) prompt))
      (should-not (string-search "Child brief" prompt))
      (should-not (string-search "Grandchild brief" prompt)))))

(mevedel-deftest mevedel-note-preview-prompt
  (:quiet t :doc "preview and send use identical generated text without preview side effects")
  (mevedel-note-conversation-test--with-tree
    (let* ((buffers (buffer-list))
           (preview (mevedel-note-preview-prompt parent))
           (text (with-current-buffer preview (buffer-string))))
      (should-not sent)
      (should-not (overlay-get parent 'mevedel-note-session-id))
      (should (= 1 (length (cl-set-difference (buffer-list) buffers))))
      (mevedel-note-send-batch (list grandchild parent child) :no-display t)
      (let* ((data (plist-get (gptel-fsm-info (car sent)) :buffer))
             (session (buffer-local-value 'mevedel--session data)))
        (should (equal text (mevedel-note-conversation--prompt parent session)))
        (with-current-buffer data
          (should (string-search (string-trim-right text) (buffer-string))))
        (dolist (note (list parent child grandchild))
          (should (equal (overlay-get note 'mevedel-note-session-id)
                         (mevedel-session-session-id session)))))
      (should (= 1 (length sent))))))

(mevedel-deftest mevedel-note-conversation--send-notes
  (:quiet t)
  ,test
  (test)
  :doc "sending a child includes its descendants, but both the parent and new descendants stay unsent on reopen"
  (mevedel-note-conversation-test--with-tree
    (let* ((view (mevedel-note-send child :no-display t))
           (payload (decode-coding-string (car sent-payloads) 'utf-8-unix))
           (id (overlay-get child 'mevedel-note-session-id)))
      (should (string-search "Grandchild brief" payload))
      (should-not (string-search "Parent brief" payload))
      (should (equal id (overlay-get grandchild 'mevedel-note-session-id)))
      (should-not (overlay-get parent 'mevedel-note-session-id))
      (let ((new (mevedel-note-make source 10 12 :text "Not sent yet")))
        (should (eq view (mevedel-note-send child :no-display t)))
        (should-not (overlay-get new 'mevedel-note-session-id))
        (should (= 1 (length sent))))))
  :doc "selecting an independent child together with its parent refuses rather than losing either thread"
  (mevedel-note-conversation-test--with-tree
    (mevedel-note-send child :no-display t)
    (should-error (mevedel-note-send-batch (list parent child) :no-display t) :type 'user-error)
    (should-not (overlay-get parent 'mevedel-note-session-id))
    (should (= 1 (length sent))))
  :doc "invalid selections fail validation before any new conversation"
  (mevedel-note-conversation-test--with-tree
    (should-error (mevedel-note-send-batch (list parent 'invalid) :no-display t) :type 'user-error)
    (should-error (mevedel-note-send-batch (list 'invalid parent) :no-display t) :type 'user-error)
    (should-not sent))
  :doc "batch send builds each selected note's tree only once"
  (mevedel-note-conversation-test--with-source
    (erase-buffer)
    (dotimes (_ 50) (insert "code\n"))
    (save-buffer)
    (let* ((notes (cl-loop for start from 1 below (point-max) by 5
                           collect (mevedel-note-make source start (+ start 4)
                                                     :text "Review this line")))
           (build-tree (symbol-function 'mevedel-note-conversation--tree))
           (builds 0))
      (cl-letf (((symbol-function 'mevedel-note-conversation--tree)
                 (lambda (item)
                   (cl-incf builds)
                   (funcall build-tree item))))
        (mevedel-note-send-batch notes :no-display t))
      (should (= (length notes) builds))
      (should (= 1 (length sent))))))

(provide 'test-mevedel-note-conversation)
;;; test-mevedel-note-conversation.el ends here
