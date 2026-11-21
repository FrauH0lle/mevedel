;;; test-niwa-view-composer-native-admission.el --- Native admission drafts -*- lexical-binding: t -*-

;;; Commentary:
;; Refused composer sends and held browser prompts must not consume input.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-chat)
(require 'niwa-claude-code)
(require 'niwa-claude-code-fork)
(require 'niwa-claude-code-history)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-transport)
(require 'niwa-pending-inputs)
(require 'niwa-presets)
(require 'niwa-skills-ui)
(require 'niwa-turn)
(require 'niwa-view-composer)

(niwa-deftest niwa-view-send/native-preparation (:quiet t)
  (dolist (late '(nil t))
    (niwa-engine-test--with-claude-session
      (niwa-request-end)
      (let* ((view (generate-new-buffer " *native-admission-view*"))
             (niwa-claude-code-fork--sessions (unless late (list session)))
             (check (symbol-function 'niwa-request-assert-target-ready))
             (before (buffer-string))
             (turn-count (niwa-session-turn-count session))
             (draft "> Keep this draft\n  exactly as typed  "))
        (unwind-protect
            (progn
              (niwa-view--setup view buffer)
              (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                         (lambda (owner)
                           (funcall check owner)
                           (when late
                             (push session niwa-claude-code-fork--sessions)))))
                (with-current-buffer view
                  (niwa-view-composer-insert draft)
                  (backward-char 5)
                  (let ((offset (niwa-view-test--composer-point-offset)))
                    (condition-case err
                        (niwa-view-send)
                      (user-error
                       (should (string-match-p "native history preparation"
                                               (error-message-string err)))))
                    (should (equal draft (niwa-view--visible-draft)))
                    (should (= offset (niwa-view-test--composer-point-offset))))))
              (should (equal before (buffer-string)))
              (should (equal turn-count (niwa-session-turn-count session)))
              (should-not niwa--current-request)
              (should-not (niwa-session-pending-follow-ups session)))
          (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa-collaboration--handle-prompt/native-preparation (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let* ((view (generate-new-buffer " *native-guest-view*"))
           (niwa-claude-code-fork--sessions nil)
           (check (symbol-function 'niwa-request-assert-target-ready))
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer buffer :guests guests
                       :transport 'transport))
           (before (buffer-string))
           (turn-count (niwa-session-turn-count session))
           (text "Keep this browser message\nfor the next turn")
           sent)
      (unwind-protect
          (progn
            (niwa-view--setup view buffer)
            (puthash 1 (list :name "Phone" :guest-id "phone" :writable t :ready t)
                     guests)
            (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                       (lambda (_transport _peer frame) (push frame sent) t)))
              (niwa-collaboration--handle-prompt
               room 1 (list :t "prompt" :text text)))
            (let ((entry (car (niwa-session-pending-follow-ups session))))
              (should entry)
              (should (equal "queued" (plist-get (car sent) :t)))
              (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                         (lambda (owner)
                           (funcall check owner)
                           (push session niwa-claude-code-fork--sessions))))
                (dotimes (_ 2) (niwa-view--drain-follow-up buffer)))
              (should (eq entry (car (niwa-session-pending-follow-ups session))))
              (should (equal text (plist-get entry :input)))
              (should-not (plist-get entry :delivering))
              (should-not (plist-get entry :state)))
            (should (equal before (buffer-string)))
            (should (equal turn-count (niwa-session-turn-count session)))
            (should-not niwa--current-request)
            (setq niwa-claude-code-fork--sessions nil)
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (&rest _)
                          '(:meta ((responseText . "Queued message received")))))))
              (niwa-claude-code-fork-histories
               nil
               (lambda (_histories diagnosis _cleanup)
                 (should-not diagnosis)
                 (should (memq session niwa-claude-code-fork--sessions))
                 (should (niwa-session-pending-follow-ups session)))
               session)
              (niwa-test--await 5 "Released preparation did not deliver queued input"
                (and (not (niwa-session-pending-follow-ups session))
                     (not (niwa-turn-busy-p buffer)))))
            (should (= 1 (niwa-engine-test--count-evidence
                          (regexp-quote text))))
            (should (= 1 (niwa-engine-test--count-evidence
                          "Queued message received")))
            (should (= (1+ turn-count) (niwa-session-turn-count session))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa--dispatch-request/native-preparation (:quiet t)
  (niwa-engine-test--with-session
    (niwa-request-end)
    (niwa-skills-test--with-model-backends
      (niwa-model-set-session-provider
       session (niwa-model-resolve-provider "Fast:fast-model") buffer)
      (goto-char (point-max))
      (niwa--insert-user-turn "Keep native history attached")
      (setf (niwa-session-external-conversations session)
            (list (cons "root" (niwa-claude-code-history-open request "retained"))))
      (niwa-session-artifacts-save session buffer)
      (let ((history (copy-tree (niwa-session-external-conversations session)))
            (publication (niwa-session-publication session))
            (before (buffer-string))
            (niwa-claude-code-fork--sessions (list session))
            (gptel-send--handlers
             (niwa-preset--build-handlers (copy-tree gptel-send--handlers)))
            (advised (advice-member-p #'niwa-skills--gptel-send-advice 'gptel-send)))
        (unwind-protect
            (progn
              (niwa-skills-install-slash-commands)
              (let ((err (should-error (gptel-send) :type 'user-error)))
                (should (string-match-p "native history preparation"
                                        (error-message-string err))))
              (should (equal history (niwa-session-external-conversations session)))
              (should (equal publication (niwa-session-publication session)))
              (should (equal before (buffer-string)))
              (should-not niwa--current-request))
          (unless advised (niwa-skills-uninstall-slash-commands)))))))

(provide 'test-niwa-view-composer-native-admission)
;;; test-niwa-view-composer-native-admission.el ends here
