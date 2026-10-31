;;; test-mevedel-claude-code-btw.el --- Native side lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:
;; Real side views and the stdio peer exercise forked stored history.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-btw)
(require 'mevedel-side-conversation)

(defmacro mevedel-test-btw--with-parent (&rest body)
  "Run BODY with a settled native parent and fake stored history.
Expose root, buffer, session, parent-file, fork-file, view and launches."
  (declare (indent 0) (debug t))
  `(mevedel-engine-test--with-claude-session
     (mevedel-request-end)
     (let* ((process-environment (copy-sequence process-environment))
            (parent-file (file-name-concat root "fixture-session.json"))
            (fork-file (file-name-concat root "fixture-fork.json"))
            view launches overrides)
       (setenv "MEVEDEL_ACP_TEST_HISTORY" root)
       (with-temp-file parent-file (insert "[\"parent context\"]"))
       (setq-local gptel-system-prompt "Frozen system")
       (insert "*** Accepted parent prompt\n"
               (propertize "Visible answer\n" 'gptel 'response))
       (setf (mevedel-session-external-conversations session)
             (list (list "root" :engine 'claude-code :id "fixture-session"
                         :host (system-name) :directory mevedel-claude-code-directory
                         :state 'ready)))
       (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                  (mevedel-engine-test--claude-launch
                   (lambda (&rest args) (push args launches) overrides)))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (value &rest _) (setq view value))))
         (unwind-protect (progn ,@body)
           (dolist (name '("MEVEDEL_ACP_TEST_FORK_GATE" "MEVEDEL_ACP_TEST_CANCEL_GATE"))
             (when-let* ((file (getenv name))) (with-temp-file file)))
           (when (buffer-live-p view) (kill-buffer view))
           (when (file-exists-p (file-name-concat root "delete-fails"))
             (delete-file (file-name-concat root "delete-fails")))
           (mevedel-claude-code-btw-shutdown))))))

(mevedel-deftest mevedel-claude-code-btw/integration (:quiet t)
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (let* ((process-environment (copy-sequence process-environment))
           (parent-file (file-name-concat root "fixture-session.json"))
           (fork-file (file-name-concat root "fixture-fork.json"))
           side view parent-text)
      (setenv "MEVEDEL_ACP_TEST_HISTORY" root)
      (with-temp-file parent-file (insert "[\"native-only context\"]"))
      (setq-local gptel-system-prompt "Frozen system")
      (insert "*** Accepted parent prompt\n"
              (propertize "Visible answer\n" 'gptel 'response))
      (setq parent-text (buffer-string))
      (setf (mevedel-session-external-conversations session)
            (list (list "root" :engine 'claude-code :id "fixture-session"
                        :host (system-name) :directory mevedel-claude-code-directory
                        :state 'ready)))
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (&rest _) nil)))
                ((symbol-function 'pop-to-buffer)
                 (lambda (value &rest _) (setq view value))))
        (unwind-protect
            (progn
              (mevedel-side-conversation-open)
              (mevedel-test--await 10 "Native side opening" view)
              (setq side (buffer-local-value 'mevedel--data-buffer view))
              (with-current-buffer view
                (should-not (string-search "Visible answer" (buffer-string)))
                (goto-char (mevedel-view--input-start))
                (insert "first")
                (mevedel-side-conversation-send))
              (mevedel-test--await 10 "First native side answer"
                (not (mevedel-turn-busy-p side)))
              (with-current-buffer view
                (should (string-search "native-only context" (buffer-string)))
                (goto-char (mevedel-view--input-start))
                (insert "second")
                (mevedel-side-conversation-send)
                (goto-char (mevedel-view--input-start))
                (insert "> unfinished draft\nsecond line"))
              (mevedel-test--await 10 "Second native side answer"
                (not (mevedel-turn-busy-p side)))
              (with-current-buffer view
                (should (string-search "first" (buffer-string)))
                (should (string-search "second" (buffer-string)))
                (should (equal "> unfinished draft\nsecond line"
                               (mevedel-view--input-text))))
              (should (equal parent-text (buffer-string)))
              (should (equal "[\"native-only context\"]"
                             (with-temp-buffer
                               (insert-file-contents parent-file) (buffer-string))))
              (with-current-buffer side
                (should-not (mevedel-session-external-conversations mevedel--session)))
              (should (file-exists-p fork-file))
              (with-current-buffer view (mevedel-side-conversation-close))
              (mevedel-test--await 10 "Native fork deletion"
                (not mevedel-claude-code-btw--forks))
              (should-not (file-exists-p fork-file))
              (should (file-exists-p parent-file)))
          (when (buffer-live-p view) (kill-buffer view))
          (mevedel-claude-code-btw-shutdown))))))

(mevedel-deftest mevedel-claude-code-btw-close (:quiet t)
  ,test
  (test)
  :doc "abort waits for acknowledgement before deleting the fork and preserves a refused draft"
  (mevedel-test-btw--with-parent
    (let ((gate (file-name-concat root "cancel-release")))
      (setenv "MEVEDEL_ACP_TEST_CANCEL_GATE" gate)
      (mevedel-side-conversation-open)
      (mevedel-test--await 10 "Side opening" view)
      (let ((side (buffer-local-value 'mevedel--data-buffer view)))
        (with-current-buffer view
          (goto-char (mevedel-view--input-start))
          (insert "wait")
          (mevedel-side-conversation-send))
        (mevedel-test--await 10 "Prompt accepted"
          (string-search "wait" (with-temp-buffer
                                   (insert-file-contents fork-file) (buffer-string))))
        (with-current-buffer view (mevedel-view-abort))
        (mevedel-test--await 10 "Cancel received" (file-exists-p (concat gate ".ready")))
        (should (mevedel-turn-busy-p side))
        (should (file-exists-p fork-file))
        (with-temp-file gate)
        (mevedel-test--await 10 "Abort deletion" (not mevedel-claude-code-btw--forks))
        (should-not (mevedel-turn-busy-p side))
        (should-not (file-exists-p fork-file))
        (with-current-buffer view
          (should (string-match-p "[Aa]bort" (buffer-string)))
          (goto-char (mevedel-view--input-start))
          (insert "> keep refused draft")
          (let ((before (point)))
            (should-error (mevedel-side-conversation-send) :type 'user-error)
            (should (= before (point)))
            (should (equal "> keep refused draft" (mevedel-view--input-text)))))
        (should (file-exists-p parent-file)))))

  :doc "prompt error and process loss delete only the fork and render terminal evidence"
  (dolist (prompt '("fail" "crash"))
    (mevedel-test-btw--with-parent
      (mevedel-side-conversation-open)
      (mevedel-test--await 10 "Side opening" view)
      (with-current-buffer view
        (goto-char (mevedel-view--input-start))
        (insert prompt)
        (mevedel-side-conversation-send))
      (mevedel-test--await 10 "Failed fork deletion" (not mevedel-claude-code-btw--forks))
      (should-not (file-exists-p fork-file))
      (should (file-exists-p parent-file))
      (with-current-buffer view
        (should (string-match-p "[Ee]rror\\|[Ff]ailed" (buffer-string))))))

  :doc "closing during a prompt stops its writer before deleting stored history"
  (mevedel-test-btw--with-parent
    (mevedel-side-conversation-open)
    (mevedel-test--await 10 "Side opening" view)
    (with-current-buffer view
      (goto-char (mevedel-view--input-start))
      (insert "wait")
      (mevedel-side-conversation-send))
    (mevedel-test--await 10 "Prompt accepted"
      (string-search "wait" (with-temp-buffer
                               (insert-file-contents fork-file) (buffer-string))))
    (with-current-buffer view (mevedel-side-conversation-close))
    (mevedel-test--await 10 "Closed fork deletion" (not mevedel-claude-code-btw--forks))
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file)))

  :doc "deletion failure warns and retains the identity for exit cleanup retry"
  (mevedel-test-btw--with-parent
    (mevedel-side-conversation-open)
    (mevedel-test--await 10 "Side opening" view)
    (with-temp-file (file-name-concat root "delete-fails"))
    (let (diagnostics)
      (mevedel-test--with-captured-diagnostics diagnostics
        (with-current-buffer view (mevedel-side-conversation-close))
        (mevedel-test--await 10 "Deletion failure"
          (mevedel-claude-code-btw--fork-error (car mevedel-claude-code-btw--forks))))
      (should (string-search "Could not delete /btw" (format "%s" diagnostics))))
    (should (file-exists-p fork-file))
    (delete-file (file-name-concat root "delete-fails"))
    (mevedel-claude-code-btw-shutdown)
    (should-not mevedel-claude-code-btw--forks)
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file)))

  :doc "exit cleanup deletes an idle multi-turn fork"
  (mevedel-test-btw--with-parent
    (mevedel-side-conversation-open)
    (mevedel-test--await 10 "Side opening" view)
    (should (memq #'mevedel-claude-code-btw-shutdown kill-emacs-hook))
    (mevedel-claude-code-btw-shutdown)
    (should-not mevedel-claude-code-btw--forks)
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file))))

(mevedel-deftest mevedel-claude-code-btw-open (:quiet t)
  ,test
  (test)
  :doc "a late fork reply after side closure deletes the fork without showing a view"
  (mevedel-test-btw--with-parent
    (let ((gate (file-name-concat root "fork-release")))
      (setenv "MEVEDEL_ACP_TEST_FORK_GATE" gate)
      (mevedel-side-conversation-open)
      (mevedel-test--await 10 "Fork held" (file-exists-p (concat gate ".ready")))
      (kill-buffer mevedel-side-conversation--side-buffer)
      (should-not view)
      (with-temp-file gate)
      (mevedel-test--await 10 "Late fork deletion" (not mevedel-claude-code-btw--forks))
      (should-not view)
      (should-not (file-exists-p fork-file))
      (should (file-exists-p parent-file))))

  :doc "a parent changed while forking refuses the stale snapshot and deletes it"
  (mevedel-test-btw--with-parent
    (let ((gate (file-name-concat root "fork-release")))
      (setenv "MEVEDEL_ACP_TEST_FORK_GATE" gate)
      (mevedel-side-conversation-open)
      (mevedel-test--await 10 "Fork held" (file-exists-p (concat gate ".ready")))
      (goto-char (point-max))
      (insert "New parent material")
      (with-temp-file gate)
      (mevedel-test--await 10 "Stale fork deletion" (not mevedel-claude-code-btw--forks))
      (should-not view)
      (should-not (file-exists-p fork-file))))

  :doc "unsupported adapters refuse without changing parent history"
  (mevedel-test-btw--with-parent
    (setenv "MEVEDEL_ACP_TEST_MISSING_CAP" "fork")
    (let (diagnostics)
      (mevedel-test--with-captured-diagnostics diagnostics
        (mevedel-side-conversation-open)
        (mevedel-test--await 10 "Unsupported fork" (not mevedel-claude-code-btw--forks)))
      (should (string-search "lacks session fork capability" (format "%s" diagnostics))))
    (should-not view)
    (should-not mevedel-side-conversation--side-buffer)
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file))))

(mevedel-deftest mevedel-claude-code-btw-parent-history (:quiet t)
  (mevedel-test-btw--with-parent
    (let ((record (cdar (mevedel-session-external-conversations session))))
      (dolist (state '(unstarted in-flight uncertain diverged))
        (plist-put record :state state)
        (should-error (mevedel-side-conversation-open) :type 'user-error))
      (plist-put record :state 'ready)
      (plist-put record :host "foreign.invalid")
      (should-error (mevedel-side-conversation-open) :type 'user-error)
      (plist-put record :host (system-name))
      (plist-put record :start-failed t)
      (should-error (mevedel-side-conversation-open) :type 'user-error)
      (plist-put record :start-failed nil)
      (mevedel-request-begin session)
      (should-error (mevedel-side-conversation-open) :type 'user-error)
      (mevedel-request-end)
      (should-not launches)
      (should-not mevedel-side-conversation--side-buffer))))

(mevedel-deftest mevedel-claude-code-btw-send (:quiet t)
  (mevedel-test-btw--with-parent
    (let ((file (file-name-concat root "evidence.txt")))
      (with-temp-file file (insert "Read through the side pipeline"))
      (setq-local gptel-reasoning-effort 'low)
      (mevedel-side-conversation-open)
      (mevedel-test--await 10 "Side opening" view)
      ;; Parent configuration changes cannot widen the already-frozen side.
      (setq-local gptel-model 'opus gptel-reasoning-effort 'high)
      (setq overrides
            (list :meta `((responseText . "Read completed")
                          (toolBatches . [[((name . "Read") (id . "side-read")
                                           (args . ((file_path . ,file))))]]))))
      (let ((side (buffer-local-value 'mevedel--data-buffer view)) owner)
        (with-current-buffer view
          (goto-char (mevedel-view--input-start))
          (insert "Read the evidence")
          (mevedel-side-conversation-send))
        (with-current-buffer side
          (setq owner mevedel--current-request)
          (should (mevedel-request-ephemeral-p owner))
          (should (mevedel-request-one-shot-mutations-p owner))
          (should-not (mevedel-request-fsm owner))
          (should (equal (sort (mapcar #'gptel-tool-name
                                      (plist-get (mevedel-engine-info owner) :tools))
                               #'string<)
                         (sort (copy-sequence mevedel-side-conversation--tool-names) #'string<))))
        (mevedel-test--await 10 "Side Read settlement" (not (mevedel-turn-busy-p side)))
        (should (eq 'success (plist-get (mevedel-engine-info owner) :mevedel-acp-outcome)))
        (should (equal "sonnet" (nth 2 (car launches))))
        (should (eq 'low (nth 3 (car launches))))
        (should (equal "fixture-fork" (nth 4 (car launches))))
        (with-current-buffer side
          (should (string-search "Read through the side pipeline" (buffer-string)))
          (should-not (mevedel-session-external-conversations mevedel--session)))
        (with-current-buffer view
          (should (string-search "Read completed" (buffer-string)))
          (should (string-search "evidence.txt" (buffer-string))))))))

(mevedel-deftest mevedel-claude-code-btw--content (:quiet t)
  (mevedel-test-btw--with-parent
    (let* ((image (file-name-concat root "image.png"))
           (file (file-name-concat root "mentioned.txt")))
      (with-temp-file image (insert "frozen-image-bytes"))
      (with-temp-file file (insert "Fresh mention body"))
      (setq-local mevedel-side-conversation--frozen-context
                  (list :text "Frozen selected context"
                        :media (list (list image :mime "image/png"))))
      (let* ((parts (mevedel-claude-code-btw--content
                     (concat "Read @file:" file) gptel-model))
             (text (alist-get 'text (aref parts 0))))
        (should (string-search "Frozen selected context" text))
        (should (string-search "Fresh mention body" text))
        (should (equal "image/png" (alist-get 'mimeType (aref parts 1))))
        (should (equal "frozen-image-bytes"
                       (base64-decode-string (alist-get 'data (aref parts 1)))))))))

(mevedel-deftest mevedel-view-send/native-btw (:quiet t)
  (mevedel-test-btw--with-parent
    (let ((parent-view (generate-new-buffer " *native-parent-view*"))
          (gate (file-name-concat root "fork-release"))
          (context-file (file-name-concat root "selected.txt")) point-before)
      (unwind-protect
          (progn
            (with-temp-file context-file (insert "Invocation-time selected context"))
            (setq-local gptel-context (list context-file) gptel-use-context 'user)
            (mevedel-view--setup parent-view buffer)
            (setenv "MEVEDEL_ACP_TEST_FORK_GATE" gate)
            (with-current-buffer parent-view
              (mevedel-view--full-rerender)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (mevedel-view-send))
            (mevedel-test--await 10 "Command fork held" (file-exists-p (concat gate ".ready")))
            (with-temp-file context-file (insert "Later selected context must not leak"))
            (with-current-buffer parent-view
              (mevedel-view--clear-input)
              (goto-char (mevedel-view--input-start))
              (insert "> Parent draft edited during startup\nsecond line")
              (setq point-before (point)))
            (with-temp-file gate)
            (mevedel-test--await 10 "Command side ready" view)
            (with-current-buffer view
              (goto-char (mevedel-view--input-start))
              (insert "Question from the command-created side")
              (mevedel-view-send))
            (mevedel-test--await 10 "Command side settled"
              (not (mevedel-turn-busy-p (buffer-local-value 'mevedel--data-buffer view))))
            (with-current-buffer parent-view
              (should (= point-before (point)))
              (should (equal "> Parent draft edited during startup\nsecond line"
                             (mevedel-view--input-text))))
            (with-current-buffer view
              (should (string-search "parent context" (buffer-string)))
              (should (string-search "Invocation-time selected context" (buffer-string)))
              (should-not (string-search "Later selected context must not leak" (buffer-string)))))
        (when (buffer-live-p parent-view) (kill-buffer parent-view))))))

(mevedel-deftest mevedel-claude-code-btw/provider-compaction (:quiet t)
  (mevedel-test-btw--with-parent
    (mevedel-side-conversation-open)
    (mevedel-test--await 10 "Side opening" view)
    (setq overrides
          '(:meta ((responseText . "Answer after native compaction")
                   (compactionEvents .
                    [((sessionUpdate . "compaction_update")
                      (compactionId . "side-compact") (status . "in_progress"))
                     ((sessionUpdate . "compaction_update")
                      (compactionId . "side-compact") (status . "completed")
                      (summary . [((type . "text") (text . "Native retained summary"))]))]))))
    (let* ((side (buffer-local-value 'mevedel--data-buffer view))
           (side-session (buffer-local-value 'mevedel--session side))
           segment owner)
      (with-current-buffer view
        (goto-char (mevedel-view--input-start))
        (insert "Keep this side prompt visible")
        (mevedel-view-send))
      (setq owner (buffer-local-value 'mevedel--current-request side))
      (setq segment (mevedel-session-current-segment side-session))
      (mevedel-test--await 10 "Compacted side answer" (not (mevedel-turn-busy-p side)))
      (should (eq 'success (plist-get (mevedel-engine-info owner) :mevedel-acp-outcome)))
      (should-not (plist-get (mevedel-engine-info owner) :mevedel-acp-compactions))
      (should (equal segment (mevedel-session-current-segment side-session)))
      (should-not (mevedel-session-external-conversations side-session))
      (with-current-buffer view
        (should (string-search "Keep this side prompt visible" (buffer-string)))
        (should (string-search "Answer after native compaction" (buffer-string)))))))

(provide 'test-mevedel-claude-code-btw)
;;; test-mevedel-claude-code-btw.el ends here
