;;; test-niwa-checkpoint-ordering.el --- Checkpoint coherence -*- lexical-binding: t -*-
;;; Commentary:
;; Real disk restore at the synchronous root commit/deferred save boundary.
;;; Code:
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "niwa-session-test-support"))
(require 'niwa-turn)
(require 'niwa-agent-persistence)
(require 'niwa-session-checkpoint)

(defun test-niwa-checkpoint-ordering--restore (route phase)
  "Assert root checkpoint consistency after ROUTE in PHASE."
  (let* ((tempdir (file-name-as-directory (make-temp-file "niwa-checkpoint-order-" t)))
         (workspace (test-niwa-session-persistence--make-workspace tempdir))
         (session (niwa-session-create "main" workspace))
         (root (generate-new-buffer " *checkpoint-root*"))
         (fsm (gptel-make-fsm :info (list :buffer root :niwa-request-id "root")))
         restored child directory old-index deferred entered)
    (unwind-protect
        (progn
          (with-current-buffer root
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace)
            (setq-local niwa--current-request (niwa-request--create :id "root" :turn 1))
            (insert "Question\n\n" (propertize "Partial answer\n" 'gptel 'response))
            (niwa-session-artifacts-save session root))
          (setq directory (niwa-session-save-path session)
                old-index (copy-tree (niwa-session-prompt-index session)))
          (should (= 0 (niwa-session-turn-count session)))
          (should (= 1 (plist-get (car (cdar old-index)) :cum-turn)))
          (with-current-buffer root
            (goto-char (point-max))
            (insert (propertize "Final answer tail\n" 'gptel 'response)))
          (cl-letf (((symbol-function 'niwa--defer-turn-steps)
                     (lambda (_machine steps &rest _) (setq deferred steps))))
            (niwa--complete-turn fsm))
          (should (memq #'niwa--turn-save deferred))
          (should (= 1 (niwa-session-turn-count session)))
          (pcase phase
            ('failed
             (cl-letf (((symbol-function 'niwa-session-checkpoint-capture)
                        (lambda (&rest _) (setq entered t) (error "Injected staging failure"))))
               (niwa-test--with-captured-diagnostics nil (niwa--turn-save fsm)))
             (should entered)
             (should-not (plist-get (gptel-fsm-info fsm) :niwa-turn-saved)))
            ('saved
             (niwa--turn-save fsm)
             (should (plist-get (gptel-fsm-info fsm) :niwa-turn-saved))))
          (pcase route
            ('sidecar (niwa-session-artifacts-publish-sidecar-state session root))
            ('observational (niwa-session-persistence-write-sidecar-now session root))
            ('registry (niwa-session-artifacts-save-agent-registry session root))
            ('acknowledged (niwa-agent-control-send-message session "/root" "Durable mail"))
            ('agent-terminal
             (setq child (generate-new-buffer " *checkpoint-child*"))
             (let* ((relative "agents/test.chat.org")
                    (invocation (niwa-agent-invocation--create
                                 :agent-id "test" :path "/root/test" :parent-session session
                                 :parent-data-buffer root :buffer child
                                 :transcript-relative-path relative)))
               (with-current-buffer child
                 (setq-local niwa--session session create-lockfiles nil)
                 (setq buffer-file-name (expand-file-name relative directory))
                 (insert "Agent terminal result\n"))
               (should (niwa-session-artifacts-publish-agent-terminal-state invocation))
               (should (string-search "Agent terminal result"
                                      (niwa-session-artifacts-read-artifact session relative t))))))
          ;; The history picker must not offer a settled root with a missing tail.
          (let ((facts (niwa-session-publication-head-facts
                        directory (plist-get (niwa-session-publication session) :head)))
                (text (niwa-session-artifacts-read-artifact session "segment-0001.chat.org" t)))
            (should-not (and (niwa-session-publication-settled-summary-p facts)
                             (not (string-search "Final answer tail" text)))))
          (test-niwa-session-persistence--release-and-kill root session)
          (setq root nil restored (niwa-session-persistence-restore directory))
          (with-current-buffer restored
            (should (string-search "Partial answer" (buffer-string)))
            (if (eq phase 'saved)
                (progn
                  (should (string-search "Final answer tail" (buffer-string)))
                  (should (= 1 (niwa-session-turn-count niwa--session))))
              (should-not (string-search "Final answer tail" (buffer-string)))
              (should (= 0 (niwa-session-turn-count niwa--session)))
              (should (equal old-index (niwa-session-prompt-index niwa--session))))))
      (when (buffer-live-p child)
        (with-current-buffer child (set-buffer-modified-p nil))
        (kill-buffer child))
      (test-niwa-session-persistence--release-and-kill root session)
      (test-niwa-session-persistence--release-and-kill
       restored (and restored (buffer-local-value 'niwa--session restored)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-checkpoint-ordering-root ()
  ,test
  (test)
  :doc "strict sidecar publication before deferred root save"
  (test-niwa-checkpoint-ordering--restore 'sidecar 'deferred)
  :doc "strict sidecar publication after failed root save"
  (test-niwa-checkpoint-ordering--restore 'sidecar 'failed)
  :doc "observational sidecar writer before deferred root save"
  (test-niwa-checkpoint-ordering--restore 'observational 'deferred)
  :doc "agent terminal publication before deferred root save"
  (test-niwa-checkpoint-ordering--restore 'agent-terminal 'deferred)
  :doc "agent terminal publication after failed root save"
  (test-niwa-checkpoint-ordering--restore 'agent-terminal 'failed)
  :doc "no sidecar publication preserves old root count"
  (test-niwa-checkpoint-ordering--restore 'none 'deferred)
  :doc "completed root save then strict sidecar is consistent"
  (test-niwa-checkpoint-ordering--restore 'sidecar 'saved)
  :doc "completed root save then agent terminal is consistent"
  (test-niwa-checkpoint-ordering--restore 'agent-terminal 'saved)
  :doc "debounced registry writer before deferred root save"
  (test-niwa-checkpoint-ordering--restore 'registry 'deferred)
  :doc "debounced registry writer after failed root save"
  (test-niwa-checkpoint-ordering--restore 'registry 'failed)
  :doc "observational sidecar writer after failed root save"
  (test-niwa-checkpoint-ordering--restore 'observational 'failed)
  :doc "acknowledgement leaves uncommitted root text and count unchanged"
  (test-niwa-checkpoint-ordering--restore 'acknowledged 'deferred))

(provide 'test-niwa-checkpoint-ordering)
;;; test-niwa-checkpoint-ordering.el ends here
