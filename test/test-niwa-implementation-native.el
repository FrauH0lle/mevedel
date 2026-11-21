;;; test-niwa-implementation-native.el --- Accepted native policy -*- lexical-binding: t -*-

;;; Commentary:
;; Accepted implementation settings reach the real subscription dispatcher and
;; ACP peer without replacing the session's ordinary settings.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)
(require 'niwa-implementation-policy)
(require 'niwa-prompt-submission)
(require 'niwa-permission-review)

(defun niwa-implementation-native-test--assert-baseline (session)
  "Assert SESSION's saved and buffer-local selections remain the baseline."
  (should (equal "Claude Code:haiku" (niwa-session-model-provider session)))
  (should-not (niwa-session-reasoning-effort session))
  (should (eq 'ask (niwa-session-permission-mode session)))
  (should (equal "Claude Code" (gptel-backend-name gptel-backend)))
  (should (equal "haiku" (gptel--model-name gptel-model)))
  (should-not gptel-reasoning-effort)
  (should (eq 'ask niwa-permission-mode))
  (niwa-session-artifacts-save session (current-buffer))
  (let ((saved (niwa-session-codec-read
                (niwa-session-artifacts-sidecar-path
                 (niwa-session-save-path session)))))
    (should (equal "Claude Code:haiku" (plist-get saved :model-provider)))
    (should-not (plist-get saved :reasoning-effort))
    (should (eq 'ask (plist-get saved :permission-mode)))
    saved))

(defun niwa-implementation-native-test--assert-request
    (owner policy model effort mode)
  "Assert admitted OWNER captured POLICY and dispatched MODEL, EFFORT and MODE."
  (should (niwa-request-p owner))
  (should (equal policy (niwa-request-implementation-policy owner)))
  (should (eq (and policy mode)
              (niwa-request-permission-mode-override owner)))
  (should (eq mode (niwa-permission-mode-effective
                   (niwa-request-session owner) (current-buffer) nil owner)))
  (let ((info (niwa-engine-info owner)))
    (should (equal "Claude Code" (gptel-backend-name (plist-get info :backend))))
    (should (equal model (gptel--model-name (plist-get info :model))))
    (should (eq effort (plist-get info :reasoning-effort)))
    (should (eq 'success (plist-get info :niwa-acp-outcome)))))

(niwa-deftest niwa--implement-plan/native-accepted-policy
  (:quiet t :doc "Direct implementation uses accepted native settings for one request only.")
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (niwa-model-set-session-provider
     session (niwa-model-resolve-provider "Claude Code:haiku") buffer)
    (niwa-permission-mode-set-raw 'ask)
    (setq-local gptel-system-prompt "Accepted implementation fixture" gptel-tools nil)
    (niwa-chat-install-request-hooks)
    (let* ((policy '(:model-provider "Claude Code:sonnet"
                    :reasoning-effort high :permission-mode edits))
           (submission (niwa-prompt-submission-create :session session))
           owners launches)
      (niwa-prompt-submission-accept
       submission "Implement the accepted change" nil nil nil)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (ert-fail "Native policy fell back to an API request")))
                ((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system _mcp model effort &optional _id _hook)
                    (push niwa--current-request owners)
                    (push (list model effort) launches)
                    (list :meta '((responseText . "Implementation fixture complete")))))))
        (niwa--implement-plan
         (list :implementation-policy policy :prompt-submission submission))
        (niwa-test--await 5 "Accepted implementation did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (equal '(("sonnet" high)) launches))
        (should (eq 'committed (niwa-prompt-submission-state submission)))
        (should-not niwa--pending-implementation-policy)
        (should-not niwa--current-request)
        (niwa-implementation-native-test--assert-request
         (car owners) policy "sonnet" 'high 'edits)
        (should-not (niwa-session-goal session))
        (niwa-implementation-native-test--assert-baseline session)
        ;; The canonical input transaction appends after received evidence;
        ;; raw insertion at the response cursor is a genuine history edit.
        (niwa--submit-generated-turn "Continue ordinary chat")
        (niwa-test--await 5 "Ordinary baseline turn did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (equal '(("haiku" nil) ("sonnet" high)) launches))
        (should (= 2 (niwa-session-turn-count session)))
        (niwa-implementation-native-test--assert-request
         (car owners) nil "haiku" nil 'ask)
        (niwa-implementation-native-test--assert-baseline session)))))

(niwa-deftest niwa-goal-resume/native-accepted-policy
  (:quiet t :doc "Active user and automatic turns inherit accepted policy; paused chat does not.")
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (niwa-model-set-session-provider
     session (niwa-model-resolve-provider "Claude Code:haiku") buffer)
    (niwa-permission-mode-set-raw 'ask)
    (let* ((policy '(:model-provider "Claude Code:sonnet"
                    :reasoning-effort high :permission-mode full-auto))
           (niwa-goal-token-budget nil)
           (goal (niwa-goal-create "Carry out the accepted change"
                                      :session session :implementation-policy policy))
           (file (file-name-concat root "evidence.txt"))
           (launch-count 0) owners launches review-evidence
           (niwa-post-tool-use-functions
            (list (lambda (_event)
                    ;; Pause at the automatic turn's real tool boundary.  The
                    ;; admitted request keeps its policy despite the pause.
                    (when (memq launch-count '(2 4))
                      (with-current-buffer buffer
                        (let ((owner niwa--current-request))
                          (niwa-goal-pause)
                          (should (equal policy
                                         (niwa-request-implementation-policy owner)))
                          (should (eq 'full-auto
                                      (niwa-permission-mode-effective
                                       session buffer nil owner))))))
                    nil))))
      (write-region "Implementation evidence" nil file nil 'silent)
      (setq-local gptel-system-prompt "Accepted Goal fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
      (niwa-chat-install-request-hooks)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (ert-fail "Native Goal fell back to an API request")))
                ((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system _mcp model effort &optional _id hook)
                    (cl-incf launch-count)
                    (when (> launch-count 4)
                      (ert-fail "Goal started an unexpected extra native turn"))
                    (push niwa--current-request owners)
                    (push (list model effort) launches)
                    ;; Consume the real generated transcript and admitted owner
                    ;; through the permission review's evidence builder.  The
                    ;; objective is separate from actual user-role trigger text.
                    (push (niwa-permission-review--evidence
                           (list :kind 'generic :tool-name "Read"
                                 :args (list :file_path file)
                                 :session session :data-buffer buffer
                                 :request niwa--current-request))
                          review-evidence)
                    (list :meta
                          `((hookCommand . ,hook)
                            (toolBatches . [[((name . "Read")
                                              (id . ,(format "evidence-%d" launch-count))
                                              (args . ((file_path . ,file))))]])
                            (promptResponse . ((stopReason . "end_turn")
                                               (usage . ((inputTokens . 10)
                                                         (outputTokens . 5)
                                                         (cachedWriteTokens . 0)))))))))))
        (niwa--submit-generated-turn "Start with the evidence")
        (niwa-test--await 8 "Active Goal user and automatic turns did not pause"
          (and (eq 'paused (niwa-goal-status goal))
               (not (niwa-turn-busy-p buffer))))
        (should (= 2 launch-count))
        (should (= 2 (length review-evidence)))
        (let ((initial (cadr review-evidence))
              (automatic (car review-evidence)))
          (should (equal '("Start with the evidence")
                         (plist-get (plist-get initial :user-intent) :turns)))
          (should (equal (list "Start with the evidence" niwa-goal--continuation-trigger)
                         (plist-get (plist-get automatic :user-intent) :turns)))
          (should (equal "Carry out the accepted change"
                         (plist-get automatic :goal-objective)))
          ;; Neither the objective nor the model's answer is a new human
          ;; authorization; the only additional user-role text is the fixed
          ;; continuation trigger produced by the Goal scheduler.
          (should-not (plist-get (plist-get automatic :user-intent) :older-turns-omitted))
          (should-not (plist-get (plist-get automatic :user-intent) :latest-turn-omitted)))
        (should (= 2 (niwa-goal-turns-run goal)))
        (should (equal '(("sonnet" high) ("sonnet" high)) launches))
        (dolist (owner owners)
          (niwa-implementation-native-test--assert-request
           owner policy "sonnet" 'high 'full-auto))
        (let ((saved (niwa-implementation-native-test--assert-baseline session)))
          (should (equal policy (plist-get (plist-get saved :goal) :implementation-policy))))
        (niwa--submit-generated-turn "Discuss ordinary work while paused")
        (niwa-test--await 5 "Paused Goal ordinary turn did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (= 3 launch-count))
        (should (eq 'paused (niwa-goal-status goal)))
        (should (= 2 (niwa-goal-turns-run goal)))
        (niwa-implementation-native-test--assert-request
         (car owners) nil "haiku" nil 'ask)
        (niwa-implementation-native-test--assert-baseline session)
        (niwa-goal-resume)
        (niwa-test--await 8 "Resumed Goal did not dispatch and pause"
          (and (= launch-count 4) (eq 'paused (niwa-goal-status goal))
               (not (niwa-turn-busy-p buffer))))
        (should (equal '(("sonnet" high) ("haiku" nil)
                         ("sonnet" high) ("sonnet" high)) launches))
        (should (= 3 (niwa-goal-turns-run goal)))
        (should (= 4 (niwa-session-turn-count session)))
        (niwa-implementation-native-test--assert-request
         (car owners) policy "sonnet" 'high 'full-auto)
        (should (equal policy (niwa-goal-implementation-policy goal)))
        (should-not niwa--pending-implementation-policy)
        (should-not niwa--current-request)
        (niwa-implementation-native-test--assert-baseline session)))))

(provide 'test-niwa-implementation-native)
;;; test-niwa-implementation-native.el ends here
