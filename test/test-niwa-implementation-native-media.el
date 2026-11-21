;;; test-niwa-implementation-native-media.el --- Captured native media -*- lexical-binding: t -*-

;;; Commentary:
;; Media capabilities follow the admitted native model, not the root buffer's
;; saved baseline.  The real ACP peer checks the exact image bytes and MIME.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)
(require 'niwa-claude-code-context)
(require 'niwa-prompt-submission)

(niwa-deftest niwa--implement-plan/native-captured-media (:quiet t)
  (dolist (route '(mention context))
    (niwa-engine-test--with-claude-session
      (niwa-request-end)
      (let* ((baseline-label "Media baseline:media-text-only")
             (baseline-backend
              (gptel-make-openai "Media baseline" :host "127.0.0.1:9"
                                 :key "fixture" :models '(media-text-only)))
             (baseline (niwa-model-resolve-provider baseline-label))
             (policy '(:model-provider "Claude Code:sonnet"
                       :reasoning-effort high :permission-mode edits))
             (file (file-name-concat root "accepted.png"))
             (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
             (prompt (if (eq route 'mention) (format "Describe @file:{%s}" file)
                       "Describe the selected image"))
             (submission (niwa-prompt-submission-create :session session))
             owner launch)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert (base64-decode-string data))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point-min) (point-max) file nil 'silent)))
        (niwa-model-set-session-provider session baseline buffer)
        (niwa-permission-mode-set-raw 'ask)
        (setq-local gptel-system-prompt "Captured media fixture" gptel-tools nil)
        (when (eq route 'context)
          (setq-local gptel-use-context t
                      gptel-context (list (list file :mime "image/png"))))
        (should-not (gptel--model-mime-capable-p "image/png" gptel-model))
        (should (gptel--model-mime-capable-p
                 "image/png" (plist-get (niwa-model-resolve-provider "Claude Code:sonnet") :model)))
        (niwa-chat-install-request-hooks)
        (niwa-prompt-submission-accept submission prompt nil nil nil)
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (&rest _) (ert-fail "Accepted media fell back to an API request")))
                  ((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system _mcp model effort &optional _id _hook)
                      (setq owner niwa--current-request launch (list model effort))
                      (list :meta `((expectedImage . ,data)))))))
          (niwa--implement-plan
           (list :implementation-policy policy :prompt-submission submission))
          (niwa-test--await 5 (format "Captured %s image did not settle" route)
            (not (niwa-turn-busy-p buffer))))
        (should (equal '("sonnet" high) launch))
        (should (equal policy (niwa-request-implementation-policy owner)))
        (ert-info ((format "Native %s media: %S" route
                           (plist-get (niwa-engine-info owner) :error)))
          (should (eq 'success (plist-get (niwa-engine-info owner) :niwa-acp-outcome))))
        (should (string-search "Image accepted" (buffer-string)))
        (should (eq 'ready
                    (plist-get (alist-get "root" (niwa-session-external-conversations session)
                                         nil nil #'equal) :state)))
        (should (= 1 (hash-table-count (niwa-session-mentions-shown session))))
        (should (= 1 (niwa-session-turn-count session)))
        (should-not (string-search data (buffer-string)))
        (should (equal baseline-label (niwa-session-model-provider session)))
        (should (eq baseline-backend gptel-backend))
        (should (eq (plist-get baseline :model) gptel-model))
        (should-not (niwa-session-reasoning-effort session))
        (should-not gptel-reasoning-effort)
        (should (eq 'ask (niwa-session-permission-mode session)))
        (should (eq 'ask niwa-permission-mode))
        (should-not niwa--current-request)
        (should-not niwa--pending-implementation-policy)))))

(niwa-deftest niwa-mentions-expand-user-input/explicit-media-model (:quiet t)
  (niwa-engine-test--with-claude-session
    (let* ((file (file-name-concat root "capabilities.png"))
           (input (format "Describe @file:{%s}" file))
           (image-model gptel-model)
           (text-model (make-symbol "media-text-only")))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert (base64-decode-string
                 "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII="))
        (let ((coding-system-for-write 'no-conversion))
          (write-region (point-min) (point-max) file nil 'silent)))
      ;; An explicit non-media model cannot inherit the image-capable baseline.
      (should-not (plist-get (niwa-mentions-expand-user-input
                              input session nil text-model) :media-contexts))
      (should-error (niwa-claude-code-context--images
                     (list (list file "image/png")) text-model) :type 'user-error)
      (should (eq image-model gptel-model))
      ;; Callers without a captured model, including independent requests,
      ;; retain the original ambient-model behavior.
      (should (equal (list (list file "image/png"))
                     (plist-get (niwa-mentions-expand-user-input input session)
                                :media-contexts)))
      (setq-local gptel-model text-model)
      (should-not (plist-get (niwa-mentions-expand-user-input input session) :media-contexts))
      (should (equal (list (list file "image/png"))
                     (plist-get (niwa-mentions-expand-user-input input session nil image-model)
                                :media-contexts)))
      (should (eq text-model gptel-model)))))

(niwa-deftest niwa-tools-steering-expand/captured-native-model (:quiet t)
  (niwa-engine-test--with-claude-session
    (let* ((file (file-name-concat root "steering.png"))
           (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
           (image-model gptel-model)
           (text-model (make-symbol "steering-text-only"))
           (input (format "Describe @file:{%s}" file)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert (base64-decode-string data))
        (let ((coding-system-for-write 'no-conversion))
          (write-region (point-min) (point-max) file nil 'silent)))
      (setq-local gptel-model text-model)
      (setf (niwa-engine-info request)
            (list :buffer buffer :model image-model :position (point-max-marker)))
      (niwa-session-enqueue-pending-input
       session 'steering (list :input input :request-id (niwa-request-id request)))
      ;; Expand through the native consumer and the common grant/mention path,
      ;; with no capability helper mocked and no root-buffer model rebinding.
      (let ((prepared (niwa-claude-code-context--steering request)))
        (should (equal (list `((type . "image") (mimeType . "image/png") (data . ,data)))
                       (plist-get prepared :images)))
        (should (string-search "[file:" (plist-get prepared :text)))
        (should (= 1 (length (niwa-session-pending-steering session))))
        (should (= 0 (hash-table-count (niwa-session-mentions-shown session)))))
      ;; Ordinary gptel steering still inherits its ambient prompt model.
      (should-not
       (plist-get (niwa-tools-steering-expand
                   session (car (niwa-session-pending-steering session))) :media-contexts))
      (should (eq text-model gptel-model)))))

(niwa-deftest niwa-tools--handle-steering-inject/captured-api-model (:quiet t)
  (niwa-engine-test--with-session
    (niwa-request-end)
    (let* ((gptel--known-backends nil)
           (backend (gptel-make-openai
                     "API steering fixture" :host "127.0.0.1:9" :key "fixture"
                     :models '((api-steering-text :capabilities nil :mime-types nil)
                               (api-steering-vision :capabilities (media)
                                                    :mime-types ("image/png")))))
           (baseline-label "API steering fixture:api-steering-text")
           (baseline (niwa-model-resolve-provider baseline-label))
           (policy '(:model-provider "API steering fixture:api-steering-vision"
                     :reasoning-effort nil :permission-mode edits))
           (effective (niwa-implementation-policy-model policy))
           (file (file-name-concat root "api-steering.png"))
           (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
           (input (format "Describe @file:{%s}" file))
           (context-before gptel-context)
           (payload (list :model "api-steering-vision" :messages [])))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert (base64-decode-string data))
        (let ((coding-system-for-write 'no-conversion))
          (write-region (point-min) (point-max) file nil 'silent)))
      (niwa-model-set-session-provider session baseline buffer)
      (niwa-permission-mode-set-raw 'ask)
      (should-not (gptel--model-mime-capable-p "image/png" gptel-model))
      (should (gptel--model-mime-capable-p "image/png" (plist-get effective :model)))
      (setq-local niwa--pending-implementation-policy policy)
      (let* ((owner (niwa-request-begin session))
             (fsm (gptel-make-fsm
                   :state 'TRET
                   :info (list :buffer buffer :backend backend
                               :model (plist-get effective :model) :data payload
                               :position (point-max-marker)
                               :niwa-request owner
                               :niwa-request-id (niwa-request-id owner))
                   :handlers '((WAIT niwa-tools--handle-steering-inject)))))
        (setf (niwa-request-fsm owner) fsm)
        (niwa-session-enqueue-pending-input
         session 'steering
         (list :input input :model-input input :transcript-payload input
               :request-id (niwa-request-id owner) :dropped-file-grants (list file)))
        ;; Enter the real gptel WAIT dispatch in an unrelated buffer.  The
        ;; consumer must use captured info, not an assumed dynamic model binding.
        (with-temp-buffer (gptel--fsm-transition fsm 'WAIT))
        (should (eq 'WAIT (gptel-fsm-state fsm)))
        (should (equal policy (niwa-request-implementation-policy owner)))
        (should (equal "api-steering-vision" (plist-get payload :model)))
        (should (= 1 (length (plist-get payload :messages))))
        (let* ((message (aref (plist-get payload :messages) 0))
               (content (plist-get message :content))
               (image (aref content 0)))
          (should (equal "user" (plist-get message :role)))
          (should (equal "image_url" (plist-get image :type)))
          (should (equal (concat "data:image/png;base64," data)
                         (plist-get (plist-get image :image_url) :url)))
          (should (string-search "media attached" (plist-get (aref content 1) :text))))
        (should-not (niwa-session-pending-steering session))
        (should (equal (list file) (niwa-session-active-dropped-file-grants session)))
        (should (= 1 (hash-table-count (niwa-session-mentions-shown session))))
        (should (string-search input (buffer-string)))
        (should-not (string-search data (buffer-string)))
        (should (eq context-before gptel-context))
        (should (equal baseline-label (niwa-session-model-provider session)))
        (should (eq backend gptel-backend))
        (should (eq (plist-get baseline :model) gptel-model))
        (should-not (niwa-session-reasoning-effort session))
        (should-not gptel-reasoning-effort)
        (should (eq 'ask (niwa-session-permission-mode session)))
        (should (eq 'ask niwa-permission-mode))))))

(provide 'test-niwa-implementation-native-media)
;;; test-niwa-implementation-native-media.el ends here
