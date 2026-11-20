;;; test-mevedel-view-composer-native-admission.el --- Native admission drafts -*- lexical-binding: t -*-

;;; Commentary:
;; Refused composer sends and held browser prompts must not consume input.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-chat)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-fork)
(require 'mevedel-claude-code-history)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-transport)
(require 'mevedel-pending-inputs)
(require 'mevedel-presets)
(require 'mevedel-skills-ui)
(require 'mevedel-turn)
(require 'mevedel-view-composer)

(mevedel-deftest mevedel-view-send/native-preparation (:quiet t)
  (dolist (late '(nil t))
    (mevedel-engine-test--with-claude-session
      (mevedel-request-end)
      (let* ((view (generate-new-buffer " *native-admission-view*"))
             (mevedel-claude-code-fork--sessions (unless late (list session)))
             (check (symbol-function 'mevedel-request-assert-target-ready))
             (before (buffer-string))
             (turn-count (mevedel-session-turn-count session))
             (draft "> Keep this draft\n  exactly as typed  "))
        (unwind-protect
            (progn
              (mevedel-view--setup view buffer)
              (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                         (lambda (owner)
                           (funcall check owner)
                           (when late
                             (push session mevedel-claude-code-fork--sessions)))))
                (with-current-buffer view
                  (mevedel-view-composer-insert draft)
                  (backward-char 5)
                  (let ((offset (mevedel-view-test--composer-point-offset)))
                    (condition-case err
                        (mevedel-view-send)
                      (user-error
                       (should (string-match-p "native history preparation"
                                               (error-message-string err)))))
                    (should (equal draft (mevedel-view--visible-draft)))
                    (should (= offset (mevedel-view-test--composer-point-offset))))))
              (should (equal before (buffer-string)))
              (should (equal turn-count (mevedel-session-turn-count session)))
              (should-not mevedel--current-request)
              (should-not (mevedel-session-pending-follow-ups session)))
          (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel-collaboration--handle-prompt/native-preparation (:quiet t)
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (let* ((view (generate-new-buffer " *native-guest-view*"))
           (mevedel-claude-code-fork--sessions nil)
           (check (symbol-function 'mevedel-request-assert-target-ready))
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer buffer :guests guests
                       :transport 'transport))
           (before (buffer-string))
           (turn-count (mevedel-session-turn-count session))
           (text "Keep this browser message\nfor the next turn")
           sent)
      (unwind-protect
          (progn
            (mevedel-view--setup view buffer)
            (puthash 1 (list :name "Phone" :guest-id "phone" :writable t :ready t)
                     guests)
            (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                       (lambda (_transport _peer frame) (push frame sent) t)))
              (mevedel-collaboration--handle-prompt
               room 1 (list :t "prompt" :text text)))
            (let ((entry (car (mevedel-session-pending-follow-ups session))))
              (should entry)
              (should (equal "queued" (plist-get (car sent) :t)))
              (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                         (lambda (owner)
                           (funcall check owner)
                           (push session mevedel-claude-code-fork--sessions))))
                (dotimes (_ 2) (mevedel-view--drain-follow-up buffer)))
              (should (eq entry (car (mevedel-session-pending-follow-ups session))))
              (should (equal text (plist-get entry :input)))
              (should-not (plist-get entry :delivering))
              (should-not (plist-get entry :state)))
            (should (equal before (buffer-string)))
            (should (equal turn-count (mevedel-session-turn-count session)))
            (should-not mevedel--current-request)
            (setq mevedel-claude-code-fork--sessions nil)
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (&rest _)
                          '(:meta ((responseText . "Queued message received")))))))
              (mevedel-claude-code-fork-histories
               nil
               (lambda (_histories diagnosis _cleanup)
                 (should-not diagnosis)
                 (should (memq session mevedel-claude-code-fork--sessions))
                 (should (mevedel-session-pending-follow-ups session)))
               session)
              (mevedel-test--await 5 "Released preparation did not deliver queued input"
                (and (not (mevedel-session-pending-follow-ups session))
                     (not (mevedel-turn-busy-p buffer)))))
            (should (= 1 (mevedel-engine-test--count-evidence
                          (regexp-quote text))))
            (should (= 1 (mevedel-engine-test--count-evidence
                          "Queued message received")))
            (should (= (1+ turn-count) (mevedel-session-turn-count session))))
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest mevedel--dispatch-request/native-preparation (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (mevedel-skills-test--with-model-backends
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
      (goto-char (point-max))
      (mevedel--insert-user-turn "Keep native history attached")
      (setf (mevedel-session-external-conversations session)
            (list (cons "root" (mevedel-claude-code-history-open request "retained"))))
      (mevedel-session-artifacts-save session buffer)
      (let ((history (copy-tree (mevedel-session-external-conversations session)))
            (publication (mevedel-session-publication session))
            (before (buffer-string))
            (mevedel-claude-code-fork--sessions (list session))
            (gptel-send--handlers
             (mevedel-preset--build-handlers (copy-tree gptel-send--handlers)))
            (advised (advice-member-p #'mevedel-skills--gptel-send-advice 'gptel-send)))
        (unwind-protect
            (progn
              (mevedel-skills-install-slash-commands)
              (let ((err (should-error (gptel-send) :type 'user-error)))
                (should (string-match-p "native history preparation"
                                        (error-message-string err))))
              (should (equal history (mevedel-session-external-conversations session)))
              (should (equal publication (mevedel-session-publication session)))
              (should (equal before (buffer-string)))
              (should-not mevedel--current-request))
          (unless advised (mevedel-skills-uninstall-slash-commands)))))))

(provide 'test-mevedel-view-composer-native-admission)
;;; test-mevedel-view-composer-native-admission.el ends here
