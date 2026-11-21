;;; test-niwa-claude-code-btw.el --- Native side lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:
;; Real side views and the stdio peer exercise forked stored history.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'niwa-claude-code)
(require 'niwa-claude-code-btw)
(require 'niwa-side-conversation)

(defmacro niwa-test-btw--with-parent (&rest body)
  "Run BODY with a settled native parent and fake stored history.
Expose root, buffer, session, parent-file, fork-file, view and launches."
  (declare (indent 0) (debug t))
  `(niwa-engine-test--with-claude-session
     (niwa-request-end)
     (let* ((process-environment (copy-sequence process-environment))
            (parent-file (file-name-concat root "fixture-session.json"))
            (fork-file (file-name-concat root "fixture-fork.json"))
            view launches overrides)
       (setenv "NIWA_ACP_TEST_HISTORY" root)
       (with-temp-file parent-file (insert "[\"parent context\"]"))
       (setq-local gptel-system-prompt "Frozen system")
       (insert "*** Accepted parent prompt\n"
               (propertize "Visible answer\n" 'gptel 'response))
       (setf (niwa-session-external-conversations session)
             (list (list "root" :engine 'claude-code :id "fixture-session"
                         :host (system-name) :directory niwa-claude-code-directory
                         :state 'ready)))
       (cl-letf (((symbol-function 'niwa-claude-code-launch)
                  (niwa-engine-test--claude-launch
                   (lambda (&rest args) (push args launches) overrides)))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (value &rest _) (setq view value))))
         (unwind-protect (progn ,@body)
           (dolist (name '("NIWA_ACP_TEST_FORK_GATE" "NIWA_ACP_TEST_CANCEL_GATE"))
             (when-let* ((file (getenv name))) (with-temp-file file)))
           (when (buffer-live-p view) (kill-buffer view))
           (when (file-exists-p (file-name-concat root "delete-fails"))
             (delete-file (file-name-concat root "delete-fails")))
           (niwa-claude-code-btw-shutdown))))))

(niwa-deftest niwa-claude-code-btw/integration (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let* ((process-environment (copy-sequence process-environment))
           (parent-file (file-name-concat root "fixture-session.json"))
           (fork-file (file-name-concat root "fixture-fork.json"))
           side view parent-text)
      (setenv "NIWA_ACP_TEST_HISTORY" root)
      (with-temp-file parent-file (insert "[\"native-only context\"]"))
      (setq-local gptel-system-prompt "Frozen system")
      (insert "*** Accepted parent prompt\n"
              (propertize "Visible answer\n" 'gptel 'response))
      (setq parent-text (buffer-string))
      (setf (niwa-session-external-conversations session)
            (list (list "root" :engine 'claude-code :id "fixture-session"
                        :host (system-name) :directory niwa-claude-code-directory
                        :state 'ready)))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (&rest _) nil)))
                ((symbol-function 'pop-to-buffer)
                 (lambda (value &rest _) (setq view value))))
        (unwind-protect
            (progn
              (niwa-side-conversation-open)
              (niwa-test--await 10 "Native side opening" view)
              (setq side (buffer-local-value 'niwa--data-buffer view))
              (with-current-buffer view
                (should-not (string-search "Visible answer" (buffer-string)))
                (niwa-view-test--insert-composer-draft "first")
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-side-conversation-send)))
              (niwa-test--await 10 "First native side answer"
                (not (niwa-turn-busy-p side)))
              (with-current-buffer view
                (should (string-search "native-only context" (buffer-string)))
                (niwa-view-test--insert-composer-draft "second")
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-side-conversation-send))
                (niwa-view-test--insert-composer-draft "> unfinished draft\nsecond line"))
              (niwa-test--await 10 "Second native side answer"
                (not (niwa-turn-busy-p side)))
              (with-current-buffer view
                (should (string-search "first" (buffer-string)))
                (should (string-search "second" (buffer-string)))
                (should (equal "> unfinished draft\nsecond line"
                               (niwa-view-composer-draft-text nil t))))
              (should (equal parent-text (buffer-string)))
              (should (equal "[\"native-only context\"]"
                             (with-temp-buffer
                               (insert-file-contents parent-file) (buffer-string))))
              (with-current-buffer side
                (should-not (niwa-session-external-conversations niwa--session)))
              (should (file-exists-p fork-file))
              (with-current-buffer view (niwa-side-conversation-close))
              (niwa-test--await 10 "Native fork deletion"
                (not niwa-claude-code-btw--forks))
              (should-not (file-exists-p fork-file))
              (should (file-exists-p parent-file)))
          (when (buffer-live-p view) (kill-buffer view))
          (niwa-claude-code-btw-shutdown))))))

(niwa-deftest niwa-claude-code-btw-close (:quiet t)
  ,test
  (test)
  :doc "abort waits for acknowledgement before deleting the fork and preserves a refused draft"
  (niwa-test-btw--with-parent
    (let ((gate (file-name-concat root "cancel-release")))
      (setenv "NIWA_ACP_TEST_CANCEL_GATE" gate)
      (niwa-side-conversation-open)
      (niwa-test--await 10 "Side opening" view)
      (let ((side (buffer-local-value 'niwa--data-buffer view)))
        (with-current-buffer view
          (niwa-view-test--insert-composer-draft "wait")
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-side-conversation-send)))
        (niwa-test--await 10 "Prompt accepted"
          (string-search "wait" (with-temp-buffer
                                   (insert-file-contents fork-file) (buffer-string))))
        (with-current-buffer view (niwa-view-abort))
        (niwa-test--await 10 "Cancel received" (file-exists-p (concat gate ".ready")))
        (should (niwa-turn-busy-p side))
        (should (file-exists-p fork-file))
        (with-temp-file gate)
        (niwa-test--await 10 "Abort deletion" (not niwa-claude-code-btw--forks))
        (should-not (niwa-turn-busy-p side))
        (should-not (file-exists-p fork-file))
        (with-current-buffer view
          (should (string-match-p "[Aa]bort" (buffer-string)))
          (niwa-view-test--insert-composer-draft "> keep refused draft")
          (let ((before (niwa-view-test--composer-point-offset)))
            (should-error (with-current-buffer (niwa-view-composer-buffer)
                            (niwa-side-conversation-send)) :type 'user-error)
            (should (= before (niwa-view-test--composer-point-offset)))
            (should (equal "> keep refused draft" (niwa-view-composer-draft-text nil t)))))
        (should (file-exists-p parent-file)))))

  :doc "prompt error and process loss delete only the fork and render terminal evidence"
  (dolist (prompt '("fail" "crash"))
    (niwa-test-btw--with-parent
      (niwa-side-conversation-open)
      (niwa-test--await 10 "Side opening" view)
      (with-current-buffer view
        (niwa-view-test--insert-composer-draft prompt)
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-side-conversation-send)))
      (niwa-test--await 10 "Failed fork deletion" (not niwa-claude-code-btw--forks))
      (should-not (file-exists-p fork-file))
      (should (file-exists-p parent-file))
      (with-current-buffer view
        (should (string-match-p "[Ee]rror\\|[Ff]ailed" (buffer-string))))))

  :doc "closing during a prompt stops its writer before deleting stored history"
  (niwa-test-btw--with-parent
    (niwa-side-conversation-open)
    (niwa-test--await 10 "Side opening" view)
    (with-current-buffer view
      (niwa-view-test--insert-composer-draft "wait")
      (with-current-buffer (niwa-view-composer-buffer)
        (niwa-side-conversation-send)))
    (niwa-test--await 10 "Prompt accepted"
      (string-search "wait" (with-temp-buffer
                               (insert-file-contents fork-file) (buffer-string))))
    (with-current-buffer view (niwa-side-conversation-close))
    (niwa-test--await 10 "Closed fork deletion" (not niwa-claude-code-btw--forks))
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file)))

  :doc "deletion failure warns and retains the identity for exit cleanup retry"
  (niwa-test-btw--with-parent
    (niwa-side-conversation-open)
    (niwa-test--await 10 "Side opening" view)
    (with-temp-file (file-name-concat root "delete-fails"))
    (let (diagnostics)
      (niwa-test--with-captured-diagnostics diagnostics
        (with-current-buffer view (niwa-side-conversation-close))
        (niwa-test--await 10 "Deletion failure"
          (niwa-claude-code-btw--fork-error (car niwa-claude-code-btw--forks))))
      (should (string-search "Could not delete /btw" (format "%s" diagnostics))))
    (should (file-exists-p fork-file))
    (delete-file (file-name-concat root "delete-fails"))
    (niwa-claude-code-btw-shutdown)
    (should-not niwa-claude-code-btw--forks)
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file)))

  :doc "exit cleanup deletes an idle multi-turn fork"
  (niwa-test-btw--with-parent
    (niwa-side-conversation-open)
    (niwa-test--await 10 "Side opening" view)
    (should (memq #'niwa-claude-code-btw-shutdown kill-emacs-hook))
    (niwa-claude-code-btw-shutdown)
    (should-not niwa-claude-code-btw--forks)
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file))))

(niwa-deftest niwa-claude-code-btw-open (:quiet t)
  ,test
  (test)
  :doc "a late fork reply after side closure deletes the fork without showing a view"
  (niwa-test-btw--with-parent
    (let ((gate (file-name-concat root "fork-release")))
      (setenv "NIWA_ACP_TEST_FORK_GATE" gate)
      (niwa-side-conversation-open)
      (niwa-test--await 10 "Fork held" (file-exists-p (concat gate ".ready")))
      (kill-buffer niwa-side-conversation--side-buffer)
      (should-not view)
      (with-temp-file gate)
      (niwa-test--await 10 "Late fork deletion" (not niwa-claude-code-btw--forks))
      (should-not view)
      (should-not (file-exists-p fork-file))
      (should (file-exists-p parent-file))))

  :doc "a parent changed while forking refuses the stale snapshot and deletes it"
  (niwa-test-btw--with-parent
    (let ((gate (file-name-concat root "fork-release")))
      (setenv "NIWA_ACP_TEST_FORK_GATE" gate)
      (niwa-side-conversation-open)
      (niwa-test--await 10 "Fork held" (file-exists-p (concat gate ".ready")))
      (goto-char (point-max))
      (insert "New parent material")
      (with-temp-file gate)
      (niwa-test--await 10 "Stale fork deletion" (not niwa-claude-code-btw--forks))
      (should-not view)
      (should-not (file-exists-p fork-file))))

  :doc "unsupported adapters refuse without changing parent history"
  (niwa-test-btw--with-parent
    (setenv "NIWA_ACP_TEST_MISSING_CAP" "fork")
    (let (diagnostics)
      (niwa-test--with-captured-diagnostics diagnostics
        (niwa-side-conversation-open)
        (niwa-test--await 10 "Unsupported fork" (not niwa-claude-code-btw--forks)))
      (should (string-search "lacks session fork capability" (format "%s" diagnostics))))
    (should-not view)
    (should-not niwa-side-conversation--side-buffer)
    (should-not (file-exists-p fork-file))
    (should (file-exists-p parent-file))))

(niwa-deftest niwa-claude-code-btw-parent-history (:quiet t)
  (niwa-test-btw--with-parent
    (let ((record (cdar (niwa-session-external-conversations session))))
      (dolist (state '(unstarted in-flight uncertain diverged))
        (plist-put record :state state)
        (should-error (niwa-side-conversation-open) :type 'user-error))
      (plist-put record :state 'ready)
      (plist-put record :host "foreign.invalid")
      (should-error (niwa-side-conversation-open) :type 'user-error)
      (plist-put record :host (system-name))
      (plist-put record :start-failed t)
      (should-error (niwa-side-conversation-open) :type 'user-error)
      (plist-put record :start-failed nil)
      (niwa-request-begin session)
      (should-error (niwa-side-conversation-open) :type 'user-error)
      (niwa-request-end)
      (should-not launches)
      (should-not niwa-side-conversation--side-buffer))))

(niwa-deftest niwa-claude-code-btw-send (:quiet t)
  (niwa-test-btw--with-parent
    (let ((file (file-name-concat root "evidence.txt")))
      (with-temp-file file (insert "Read through the side pipeline"))
      (setq-local gptel-reasoning-effort 'low)
      (niwa-side-conversation-open)
      (niwa-test--await 10 "Side opening" view)
      ;; Parent configuration changes cannot widen the already-frozen side.
      (setq-local gptel-model 'opus gptel-reasoning-effort 'high)
      (setq overrides
            (list :meta `((responseText . "Read completed")
                          (toolBatches . [[((name . "Read") (id . "side-read")
                                           (args . ((file_path . ,file))))]]))))
      (let ((side (buffer-local-value 'niwa--data-buffer view)) owner)
        (with-current-buffer view
          (niwa-view-test--insert-composer-draft "Read the evidence")
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-side-conversation-send)))
        (with-current-buffer side
          (setq owner niwa--current-request)
          (should (niwa-request-ephemeral-p owner))
          (should (niwa-request-one-shot-mutations-p owner))
          (should-not (niwa-request-fsm owner))
          (should (equal (sort (mapcar #'gptel-tool-name
                                      (plist-get (niwa-engine-info owner) :tools))
                               #'string<)
                         (sort (copy-sequence niwa-side-conversation--tool-names) #'string<))))
        (niwa-test--await 10 "Side Read settlement" (not (niwa-turn-busy-p side)))
        (should (eq 'success (plist-get (niwa-engine-info owner) :niwa-acp-outcome)))
        (should (equal "sonnet" (nth 2 (car launches))))
        (should (eq 'low (nth 3 (car launches))))
        (should (equal "fixture-fork" (nth 4 (car launches))))
        (with-current-buffer side
          (should (string-search "Read through the side pipeline" (buffer-string)))
          (should-not (niwa-session-external-conversations niwa--session)))
        (with-current-buffer view
          (should (string-search "Read completed" (buffer-string)))
          (should (string-search "evidence.txt" (buffer-string))))))))

(niwa-deftest niwa-claude-code-btw--content (:quiet t)
  (niwa-test-btw--with-parent
    (let* ((image (file-name-concat root "image.png"))
           (file (file-name-concat root "mentioned.txt")))
      (with-temp-file image (insert "frozen-image-bytes"))
      (with-temp-file file (insert "Fresh mention body"))
      (setq-local niwa-side-conversation--frozen-context
                  (list :text "Frozen selected context"
                        :media (list (list image :mime "image/png"))))
      (let* ((parts (niwa-claude-code-btw--content
                     (concat "Read @file:" file) gptel-model))
             (text (alist-get 'text (aref parts 0))))
        (should (string-search "Frozen selected context" text))
        (should (string-search "Fresh mention body" text))
        (should (equal "image/png" (alist-get 'mimeType (aref parts 1))))
        (should (equal "frozen-image-bytes"
                       (base64-decode-string (alist-get 'data (aref parts 1)))))))))

(niwa-deftest niwa-view-send/native-btw (:quiet t)
  (niwa-test-btw--with-parent
    (let ((parent-view (generate-new-buffer " *native-parent-view*"))
          (gate (file-name-concat root "fork-release"))
          (context-file (file-name-concat root "selected.txt")) point-before)
      (unwind-protect
          (progn
            (with-temp-file context-file (insert "Invocation-time selected context"))
            (setq-local gptel-context (list context-file) gptel-use-context 'user)
            (niwa-view--setup parent-view buffer)
            (setenv "NIWA_ACP_TEST_FORK_GATE" gate)
            (with-current-buffer parent-view
              (niwa-view--full-rerender)
              (niwa-view-test--insert-composer-draft "/btw")
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send)))
            (niwa-test--await 10 "Command fork held" (file-exists-p (concat gate ".ready")))
            (with-temp-file context-file (insert "Later selected context must not leak"))
            (with-current-buffer parent-view
              (niwa-view--clear-input)
              (niwa-view-test--insert-composer-draft "> Parent draft edited during startup\nsecond line")
              (setq point-before (niwa-view-test--composer-point-offset)))
            (with-temp-file gate)
            (niwa-test--await 10 "Command side ready" view)
            (with-current-buffer view
              (niwa-view-test--insert-composer-draft "Question from the command-created side")
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send)))
            (niwa-test--await 10 "Command side settled"
              (not (niwa-turn-busy-p (buffer-local-value 'niwa--data-buffer view))))
            (with-current-buffer parent-view
              (should (= point-before (niwa-view-test--composer-point-offset)))
              (should (equal "> Parent draft edited during startup\nsecond line"
                             (niwa-view-composer-draft-text nil t))))
            (with-current-buffer view
              (should (string-search "parent context" (buffer-string)))
              (should (string-search "Invocation-time selected context" (buffer-string)))
              (should-not (string-search "Later selected context must not leak" (buffer-string)))))
        (when (buffer-live-p parent-view) (kill-buffer parent-view))))))

(niwa-deftest niwa-claude-code-btw/provider-compaction (:quiet t)
  (niwa-test-btw--with-parent
    (niwa-side-conversation-open)
    (niwa-test--await 10 "Side opening" view)
    (setq overrides
          '(:meta ((responseText . "Answer after native compaction")
                   (compactionEvents .
                    [((sessionUpdate . "compaction_update")
                      (compactionId . "side-compact") (status . "in_progress"))
                     ((sessionUpdate . "compaction_update")
                      (compactionId . "side-compact") (status . "completed")
                      (summary . [((type . "text") (text . "Native retained summary"))]))]))))
    (let* ((side (buffer-local-value 'niwa--data-buffer view))
           (side-session (buffer-local-value 'niwa--session side))
           segment owner)
      (with-current-buffer view
        (niwa-view-test--insert-composer-draft "Keep this side prompt visible")
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-view-send)))
      (setq owner (buffer-local-value 'niwa--current-request side))
      (setq segment (niwa-session-current-segment side-session))
      (niwa-test--await 10 "Compacted side answer" (not (niwa-turn-busy-p side)))
      (should (eq 'success (plist-get (niwa-engine-info owner) :niwa-acp-outcome)))
      (should-not (plist-get (niwa-engine-info owner) :niwa-acp-compactions))
      (should (equal segment (niwa-session-current-segment side-session)))
      (should-not (niwa-session-external-conversations side-session))
      (with-current-buffer view
        (should (string-search "Keep this side prompt visible" (buffer-string)))
        (should (string-search "Answer after native compaction" (buffer-string)))))))

(provide 'test-niwa-claude-code-btw)
;;; test-niwa-claude-code-btw.el ends here
