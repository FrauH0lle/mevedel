;;; test-mevedel-checkpoint-ordering.el --- Checkpoint coherence -*- lexical-binding: t -*-
;;; Commentary:
;; Real disk restore at the synchronous root commit/deferred save boundary.
;;; Code:
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-turn)
(require 'mevedel-agent-persistence)

(defun test-mevedel-checkpoint-ordering--restore (route phase)
  "Assert root checkpoint consistency after ROUTE in PHASE."
  (let* ((tempdir (file-name-as-directory (make-temp-file "mevedel-checkpoint-order-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace tempdir))
         (session (mevedel-session-create "main" workspace))
         (root (generate-new-buffer " *checkpoint-root*"))
         (fsm (gptel-make-fsm :info (list :buffer root :mevedel-request-id "root")))
         restored child directory old-index deferred entered)
    (unwind-protect
        (progn
          (with-current-buffer root
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session mevedel--workspace workspace)
            (setq-local mevedel--current-request (mevedel-request--create :id "root" :turn 1))
            (insert "Question\n\n" (propertize "Partial answer\n" 'gptel 'response))
            (mevedel-session-artifacts-save session root))
          (setq directory (mevedel-session-save-path session)
                old-index (copy-tree (mevedel-session-prompt-index session)))
          (should (= 0 (mevedel-session-turn-count session)))
          (should (= 1 (plist-get (car (cdar old-index)) :cum-turn)))
          (with-current-buffer root
            (goto-char (point-max))
            (insert (propertize "Final answer tail\n" 'gptel 'response)))
          (cl-letf (((symbol-function 'mevedel--defer-turn-steps)
                     (lambda (_machine steps &rest _) (setq deferred steps))))
            (mevedel--complete-turn fsm))
          (should (memq #'mevedel--turn-save deferred))
          (should (= 1 (mevedel-session-turn-count session)))
          (pcase phase
            ('failed
             (cl-letf (((symbol-function 'mevedel-session-artifacts--instruction-artifacts)
                        (lambda (&rest _) (setq entered t) (error "Injected staging failure"))))
               (mevedel-test--with-captured-diagnostics nil (mevedel--turn-save fsm)))
             (should entered)
             (should-not (plist-get (gptel-fsm-info fsm) :mevedel-turn-saved)))
            ('saved
             (mevedel--turn-save fsm)
             (should (plist-get (gptel-fsm-info fsm) :mevedel-turn-saved))))
          (pcase route
            ('sidecar (mevedel-session-artifacts-publish-sidecar-state session root))
            ('observational (mevedel-session-persistence-write-sidecar-now session root))
            ('registry (mevedel-session-artifacts-save-agent-registry session root))
            ('acknowledged (mevedel-agent-control-send-message session "/root" "Durable mail"))
            ('agent-terminal
             (setq child (generate-new-buffer " *checkpoint-child*"))
             (let* ((relative "agents/test.chat.org")
                    (invocation (mevedel-agent-invocation--create
                                 :agent-id "test" :path "/root/test" :parent-session session
                                 :parent-data-buffer root :buffer child
                                 :transcript-relative-path relative)))
               (with-current-buffer child
                 (setq-local mevedel--session session create-lockfiles nil)
                 (setq buffer-file-name (expand-file-name relative directory))
                 (insert "Agent terminal result\n"))
               (should (mevedel-session-artifacts-publish-agent-terminal-state invocation))
               (should (string-search "Agent terminal result"
                                      (mevedel-session-artifacts-read-artifact session relative t))))))
          ;; The history picker must not offer a settled root with a missing tail.
          (let ((facts (mevedel-session-publication-head-facts
                        directory (plist-get (mevedel-session-publication session) :head)))
                (text (mevedel-session-artifacts-read-artifact session "segment-0001.chat.org" t)))
            (should-not (and (mevedel-session-publication-settled-summary-p facts)
                             (not (string-search "Final answer tail" text)))))
          (test-mevedel-session-persistence--release-and-kill root session)
          (setq root nil restored (mevedel-session-persistence-restore directory))
          (with-current-buffer restored
            (should (string-search "Partial answer" (buffer-string)))
            (if (eq phase 'saved)
                (progn
                  (should (string-search "Final answer tail" (buffer-string)))
                  (should (= 1 (mevedel-session-turn-count mevedel--session))))
              (should-not (string-search "Final answer tail" (buffer-string)))
              (should (= 0 (mevedel-session-turn-count mevedel--session)))
              (should (equal old-index (mevedel-session-prompt-index mevedel--session))))))
      (when (buffer-live-p child)
        (with-current-buffer child (set-buffer-modified-p nil))
        (kill-buffer child))
      (test-mevedel-session-persistence--release-and-kill root session)
      (test-mevedel-session-persistence--release-and-kill
       restored (and restored (buffer-local-value 'mevedel--session restored)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-checkpoint-ordering-root ()
  ,test
  (test)
  :doc "strict sidecar publication before deferred root save"
  (test-mevedel-checkpoint-ordering--restore 'sidecar 'deferred)
  :doc "strict sidecar publication after failed root save"
  (test-mevedel-checkpoint-ordering--restore 'sidecar 'failed)
  :doc "observational sidecar writer before deferred root save"
  (test-mevedel-checkpoint-ordering--restore 'observational 'deferred)
  :doc "agent terminal publication before deferred root save"
  (test-mevedel-checkpoint-ordering--restore 'agent-terminal 'deferred)
  :doc "agent terminal publication after failed root save"
  (test-mevedel-checkpoint-ordering--restore 'agent-terminal 'failed)
  :doc "no sidecar publication preserves old root count"
  (test-mevedel-checkpoint-ordering--restore 'none 'deferred)
  :doc "completed root save then strict sidecar is consistent"
  (test-mevedel-checkpoint-ordering--restore 'sidecar 'saved)
  :doc "completed root save then agent terminal is consistent"
  (test-mevedel-checkpoint-ordering--restore 'agent-terminal 'saved)
  :doc "debounced registry writer before deferred root save"
  (test-mevedel-checkpoint-ordering--restore 'registry 'deferred)
  :doc "debounced registry writer after failed root save"
  (test-mevedel-checkpoint-ordering--restore 'registry 'failed)
  :doc "observational sidecar writer after failed root save"
  (test-mevedel-checkpoint-ordering--restore 'observational 'failed)
  :doc "acknowledgement leaves uncommitted root text and count unchanged"
  (test-mevedel-checkpoint-ordering--restore 'acknowledged 'deferred))

(provide 'test-mevedel-checkpoint-ordering)
;;; test-mevedel-checkpoint-ordering.el ends here
