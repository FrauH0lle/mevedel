;;; test-niwa-engine.el --- Tests for niwa-engine.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests turn-engine context shared by native and external engines.

;;; Code:

(require 'niwa-engine)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(require 'gptel-openai-oauth)
(require 'niwa-agents)

(ert-deftest niwa-engine-codex-sends-session-cache-key ()
  "The realized payload carries the root id, or the agent's own stable key."
  (let* ((gptel--known-backends nil)
         (codex (gptel-make-openai-oauth "Cache Codex"))
         (plain (gptel-make-openai "Cache Plain" :key "x" :models '(gpt-4o)))
         (session (niwa-session--create :session-id "root-id"))
         (bufs nil))
    (cl-flet* ((buffer (agent-id)
                 ;; Nested agents share the top-level session, as in
                 ;; `niwa-agent-conversation-open'.
                 (with-current-buffer (generate-new-buffer " *cache-key*")
                   (push (current-buffer) bufs)
                   (setq-local niwa--session session)
                   (when agent-id
                     (setq-local niwa--agent-invocation
                                 (niwa-agent-invocation--create :agent-id agent-id)))
                   (current-buffer)))
               (key (buf backend model)
                 (with-current-buffer buf
                   (setq-local gptel-backend backend gptel-model model
                               gptel-use-tools nil)
                   (let ((fsm (gptel-request "hi" :buffer buf :dry-run t :stream nil)))
                     (plist-get (plist-get (gptel-fsm-info fsm) :data)
                                :prompt_cache_key)))))
      (unwind-protect
          (let ((root (buffer nil)) (agent (buffer "a--11111111"))
                (nested (buffer "a--22222222")))
            (should (equal "root-id" (key root codex 'gpt-5.5)))
            (should (equal "root-id:a--11111111" (key agent codex 'gpt-5.5)))
            (should (equal (key agent codex 'gpt-5.5) (key agent codex 'gpt-5.5)))
            (should (equal "root-id:a--22222222" (key nested codex 'gpt-5.5)))
            (should-not (key root plain 'gpt-4o))
            (should-not (key agent plain 'gpt-4o)))
        (mapc #'kill-buffer bufs)))))

(niwa-deftest niwa-engine-steerable-p ()
  ,test
  (test)
  :doc "a gptel turn takes steering until its machine terminates"
  (dolist (state '(WAIT TOOL TYPE))
    (should (niwa-engine-steerable-p (gptel-make-fsm :state state))))
  (dolist (state '(DONE ERRS ABRT))
    (should-not (niwa-engine-steerable-p (gptel-make-fsm :state state))))
  :doc "an external turn takes steering until it settles or stops at a boundary"
  (let ((request (niwa-request--create :id "native" :context '(:external-history t))))
    (should (niwa-engine-steerable-p request))
    (dolist (key '(:niwa-acp-outcome :niwa-end-turn))
      (setf (niwa-request-context request) (list :external-history t key 'stop))
      (should-not (niwa-engine-steerable-p request))))
  (should-not (niwa-engine-steerable-p (niwa-request--create :id "plain"))))

(niwa-deftest niwa-engine-request-text ()
  ,test
  (test)
  :doc "a gptel request notifies readiness once, after the request is submitted"
  (with-temp-buffer
    (let ((gptel-backend (gptel--make-backend :name "engine-test"))
          (gptel-tools nil)
          events)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (push 'submitted events))))
        (should (functionp (niwa-engine-request-text
                            gptel-backend "prompt" "system" #'ignore nil nil
                            (lambda () (push 'ready events))))))
      (should (equal '(submitted ready) (reverse events)))))
  :doc "a refused submission never reports readiness"
  (with-temp-buffer
    (let ((gptel-backend (gptel--make-backend :name "engine-test"))
          (gptel-tools nil))
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (user-error "Refused"))))
        (should-error (niwa-engine-request-text
                       gptel-backend "prompt" "system" #'ignore nil nil
                       (lambda () (ert-fail "Readiness after refusal")))
                      :type 'user-error)))))

(niwa-deftest niwa-engine-text-unavailable ()
  ,test
  (test)
  :doc "a ready API-key backend reports no unavailability"
  (with-temp-buffer
    (should-not (niwa-engine-text-unavailable
                 (gptel--make-backend :name "engine-test")))))

(provide 'test-niwa-engine)
;;; test-niwa-engine.el ends here
