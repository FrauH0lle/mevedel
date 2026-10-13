;;; test-mevedel-implementation-native.el --- Accepted native policy -*- lexical-binding: t -*-

;;; Commentary:
;; Accepted implementation settings reach the real subscription dispatcher and
;; ACP peer without replacing the session's ordinary settings.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-implementation-policy)
(require 'mevedel-prompt-submission)

(defun mevedel-implementation-native-test--assert-baseline (session)
  "Assert SESSION's saved and buffer-local selections remain the baseline."
  (should (equal "Claude Code:haiku" (mevedel-session-model-provider session)))
  (should-not (mevedel-session-reasoning-effort session))
  (should (eq 'ask (mevedel-session-permission-mode session)))
  (should (equal "Claude Code" (gptel-backend-name gptel-backend)))
  (should (equal "haiku" (gptel--model-name gptel-model)))
  (should-not gptel-reasoning-effort)
  (should (eq 'ask mevedel-permission-mode))
  (mevedel-session-artifacts-save session (current-buffer))
  (let ((saved (mevedel-session-codec-read
                (mevedel-session-artifacts-sidecar-path
                 (mevedel-session-save-path session)))))
    (should (equal "Claude Code:haiku" (plist-get saved :model-provider)))
    (should-not (plist-get saved :reasoning-effort))
    (should (eq 'ask (plist-get saved :permission-mode)))
    saved))

(defun mevedel-implementation-native-test--assert-request
    (owner policy model effort mode)
  "Assert admitted OWNER captured POLICY and dispatched MODEL, EFFORT and MODE."
  (should (mevedel-request-p owner))
  (should (equal policy (mevedel-request-implementation-policy owner)))
  (should (eq (and policy mode)
              (mevedel-request-permission-mode-override owner)))
  (should (eq mode (mevedel-permission-mode-effective
                   (mevedel-request-session owner) (current-buffer) nil owner)))
  (let ((info (mevedel-engine-info owner)))
    (should (equal "Claude Code" (gptel-backend-name (plist-get info :backend))))
    (should (equal model (gptel--model-name (plist-get info :model))))
    (should (eq effort (plist-get info :reasoning-effort)))
    (should (eq 'success (plist-get info :mevedel-acp-outcome)))))

(mevedel-deftest mevedel--implement-plan/native-accepted-policy
  (:quiet t :doc "Direct implementation uses accepted native settings for one request only.")
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (mevedel-model-set-session-provider
     session (mevedel-model-resolve-provider "Claude Code:haiku") buffer)
    (mevedel-permission-mode-set-raw 'ask)
    (setq-local gptel-system-prompt "Accepted implementation fixture" gptel-tools nil)
    (mevedel-chat-install-request-hooks)
    (let* ((policy '(:model-provider "Claude Code:sonnet"
                    :reasoning-effort high :permission-mode edits))
           (submission (mevedel-prompt-submission-create :session session))
           owners launches)
      (mevedel-prompt-submission-accept
       submission "Implement the accepted change" nil nil nil)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (ert-fail "Native policy fell back to an API request")))
                ((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (_system _mcp model effort &optional _id _hook)
                    (push mevedel--current-request owners)
                    (push (list model effort) launches)
                    (list :meta '((responseText . "Implementation fixture complete")))))))
        (mevedel--implement-plan
         (list :implementation-policy policy :prompt-submission submission))
        (mevedel-test--await 5 "Accepted implementation did not settle"
          (not (mevedel-turn-busy-p buffer)))
        (should (equal '(("sonnet" high)) launches))
        (should (eq 'committed (mevedel-prompt-submission-state submission)))
        (should-not mevedel--pending-implementation-policy)
        (should-not mevedel--current-request)
        (mevedel-implementation-native-test--assert-request
         (car owners) policy "sonnet" 'high 'edits)
        (should-not (mevedel-session-goal session))
        (mevedel-implementation-native-test--assert-baseline session)
        ;; The canonical input transaction appends after received evidence;
        ;; raw insertion at the response cursor is a genuine history edit.
        (mevedel--submit-generated-turn "Continue ordinary chat")
        (mevedel-test--await 5 "Ordinary baseline turn did not settle"
          (not (mevedel-turn-busy-p buffer)))
        (should (equal '(("haiku" nil) ("sonnet" high)) launches))
        (should (= 2 (mevedel-session-turn-count session)))
        (mevedel-implementation-native-test--assert-request
         (car owners) nil "haiku" nil 'ask)
        (mevedel-implementation-native-test--assert-baseline session)))))

(mevedel-deftest mevedel-goal-resume/native-accepted-policy
  (:quiet t :doc "Active user and automatic turns inherit accepted policy; paused chat does not.")
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (mevedel-model-set-session-provider
     session (mevedel-model-resolve-provider "Claude Code:haiku") buffer)
    (mevedel-permission-mode-set-raw 'ask)
    (let* ((policy '(:model-provider "Claude Code:sonnet"
                    :reasoning-effort high :permission-mode full-auto))
           (mevedel-goal-token-budget nil)
           (goal (mevedel-goal-create "Carry out the accepted change"
                                      :session session :implementation-policy policy))
           (file (file-name-concat root "evidence.txt"))
           (launch-count 0) owners launches
           (mevedel-post-tool-use-functions
            (list (lambda (_event)
                    ;; Pause at the automatic turn's real tool boundary.  The
                    ;; admitted request keeps its policy despite the pause.
                    (when (memq launch-count '(2 4))
                      (with-current-buffer buffer
                        (let ((owner mevedel--current-request))
                          (mevedel-goal-pause)
                          (should (equal policy
                                         (mevedel-request-implementation-policy owner)))
                          (should (eq 'full-auto
                                      (mevedel-permission-mode-effective
                                       session buffer nil owner))))))
                    nil))))
      (write-region "Implementation evidence" nil file nil 'silent)
      (setq-local gptel-system-prompt "Accepted Goal fixture"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
      (mevedel-chat-install-request-hooks)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (ert-fail "Native Goal fell back to an API request")))
                ((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (_system _mcp model effort &optional _id hook)
                    (cl-incf launch-count)
                    (when (> launch-count 4)
                      (ert-fail "Goal started an unexpected extra native turn"))
                    (push mevedel--current-request owners)
                    (push (list model effort) launches)
                    (list :meta
                          `((hookCommand . ,hook)
                            (toolBatches . [[((name . "Read")
                                              (id . ,(format "evidence-%d" launch-count))
                                              (args . ((file_path . ,file))))]])
                            (promptResponse . ((stopReason . "end_turn")
                                               (usage . ((inputTokens . 10)
                                                         (outputTokens . 5)
                                                         (cachedWriteTokens . 0)))))))))))
        (mevedel--submit-generated-turn "Start with the evidence")
        (mevedel-test--await 8 "Active Goal user and automatic turns did not pause"
          (and (eq 'paused (mevedel-goal-status goal))
               (not (mevedel-turn-busy-p buffer))))
        (should (= 2 launch-count))
        (should (= 2 (mevedel-goal-turns-run goal)))
        (should (equal '(("sonnet" high) ("sonnet" high)) launches))
        (dolist (owner owners)
          (mevedel-implementation-native-test--assert-request
           owner policy "sonnet" 'high 'full-auto))
        (let ((saved (mevedel-implementation-native-test--assert-baseline session)))
          (should (equal policy (plist-get (plist-get saved :goal) :implementation-policy))))
        (mevedel--submit-generated-turn "Discuss ordinary work while paused")
        (mevedel-test--await 5 "Paused Goal ordinary turn did not settle"
          (not (mevedel-turn-busy-p buffer)))
        (should (= 3 launch-count))
        (should (eq 'paused (mevedel-goal-status goal)))
        (should (= 2 (mevedel-goal-turns-run goal)))
        (mevedel-implementation-native-test--assert-request
         (car owners) nil "haiku" nil 'ask)
        (mevedel-implementation-native-test--assert-baseline session)
        (mevedel-goal-resume)
        (mevedel-test--await 8 "Resumed Goal did not dispatch and pause"
          (and (= launch-count 4) (eq 'paused (mevedel-goal-status goal))
               (not (mevedel-turn-busy-p buffer))))
        (should (equal '(("sonnet" high) ("haiku" nil)
                         ("sonnet" high) ("sonnet" high)) launches))
        (should (= 3 (mevedel-goal-turns-run goal)))
        (should (= 4 (mevedel-session-turn-count session)))
        (mevedel-implementation-native-test--assert-request
         (car owners) policy "sonnet" 'high 'full-auto)
        (should (equal policy (mevedel-goal-implementation-policy goal)))
        (should-not mevedel--pending-implementation-policy)
        (should-not mevedel--current-request)
        (mevedel-implementation-native-test--assert-baseline session)))))

(provide 'test-mevedel-implementation-native)
;;; test-mevedel-implementation-native.el ends here
