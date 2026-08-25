;;; test-mevedel-compact.el -- Tests for mevedel-compact.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel)
(require 'mevedel-agent-control)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-compact)
(require 'mevedel-compact-estimation)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-run)
(require 'mevedel-compact-target)
(require 'mevedel-execution-transcript)
(require 'mevedel-models)
(require 'mevedel-hooks)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-system)
(require 'mevedel-utilities)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-compact-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-compact-test-support"))

(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)


(mevedel-deftest mevedel-compact ()
  ,test
  (test)
  :doc "refuses direct invocation from historical segment inspection"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'mevedel-view-historical-segment-p)
                 (lambda () t)))
        (should-error (mevedel-compact) :type 'user-error))))

  :doc "loads facade-owned compaction gates without the umbrella"
  (let ((root (file-name-directory (locate-library "mevedel-compact")))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (should
       (= 0
          (call-process
           emacs nil t nil "--batch" "-Q" "-L" root
           "--eval"
           (prin1-to-string
            '(progn
               (require 'mevedel-compact)
               (let ((mevedel-compact-auto nil))
                 (mevedel--compact-auto-eligible-p))
               (unless (and (featurep 'mevedel-compact-run)
                            (featurep 'mevedel-compact-target))
                 (error "Facade did not load compaction owners")))))))
      (should (string-empty-p (string-trim (buffer-string)))))))


(mevedel-deftest mevedel--compact-provider-wait
  (:doc "records provider dispatch before forwarding the FSM wait")
  (let* ((session (mevedel-session--create :name "provider-dispatch"))
         (buffer (generate-new-buffer " *compact-provider-dispatch*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :mevedel-request-id "request-1"
                           :model 'model :reasoning-effort 'high)))
         captured forwarded)
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-telemetry-record)
                     (lambda (_session event &rest props)
                       (setq captured (cons event props))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (value) (setq forwarded value))))
            (mevedel--compact-provider-wait fsm))
          (should (eq fsm forwarded))
          (should (eq 'provider-dispatch (car captured)))
          (should (equal "request-1"
                         (plist-get (cdr captured) :request-id))))
      (kill-buffer buffer))))


(mevedel-deftest mevedel--compact-transform-auto ()
  ,test
  (test)
  :doc "successful auto-compaction preserves later prompt-buffer transforms"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (continued nil)
        fsm)
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel-compact-run-in-flight nil)
            (setq-local mevedel--view-buffer nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Pending prompt\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (setq fsm (gptel-make-fsm
                     :info (list :buffer source-buf)))
          (cl-letf (((symbol-function 'mevedel--compact-should-compact-p)
                       (lambda (&optional _token-estimate)
                         '(:summary-policy nil :target-pressure nil)))
                      ((symbol-function 'mevedel-compact-run-start)
                       (lambda (&rest args)
                         (with-current-buffer prompt-buf
                           (goto-char (point-min))
                           (insert "Late prefix\n")
                           (when-let* ((start (mevedel-compact-evidence-find-boundary)))
                             (goto-char start)
                             (insert "Late context\n\n"))
                           (goto-char (point-max))
                           (insert "Concurrent gptel context\n")
                           (setq gptel-system-prompt
                                 (concat "Concurrent gptel context\n\n"
                                         gptel-system-prompt)))
                         (with-current-buffer source-buf
                           (setq-local mevedel-compact-target-current-request-reminder
                                       "Re-read /tmp/old.el")
                           (setq-local
                            mevedel-compact-target-current-request-hook-context
                            "<hook-context>\n<hook-event name=\"SessionStart\">\ncompact context\n</hook-event>\n</hook-context>")
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "#+begin_summary\nSummary\n#+end_summary\n")
                             (insert "Tail prompt\n")
                             (insert (propertize "Tail response\n"
                                                 'gptel 'response))
                             (insert "Pending prompt\n")))
                         (funcall (plist-get args :callback) nil))))
              (with-current-buffer prompt-buf
                (let ((gptel-backend nil)
                      (gptel-model nil)
                      (gptel-reasoning-effort 'high)
                      (gptel-system-prompt "Effective system")
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm))))
          (with-current-buffer prompt-buf
            (let* ((text (buffer-string))
                   (snapshot
                    (plist-get (gptel-fsm-info fsm)
                               :mevedel-model-context))
                   (context-start
                    (string-match "Concurrent gptel context\n" text)))
              (should continued)
              (should (string-match-p "Summary" text))
              (should (string-match-p "Late prefix" text))
              (should (string-match-p "Late context" text))
              (let ((first (string-match "compact context" text)))
                (should first)
                (should-not (string-match "compact context" text (1+ first))))
              (should (string-match-p "<system-reminder>\nRe-read /tmp/old.el"
                                      text))
              (should-not (string-match-p "Old prompt" text))
              (should (string-match-p "Pending prompt" text))
              (should context-start)
              (should-not (string-match-p "Concurrent gptel context"
                                          snapshot))
              (should (string-match-p "Late context" snapshot))
              (should (string-match-p "compact context" snapshot))
              (let ((locals
                     (plist-get (gptel-fsm-info fsm)
                                :mevedel-request-locals)))
                (should (equal "Effective system"
                               (alist-get 'gptel-system-prompt locals)))
                (should (eq 'high
                            (alist-get 'gptel-reasoning-effort locals))))))
          (with-current-buffer source-buf
            (should-not mevedel-compact-target-current-request-reminder)
            (should-not mevedel-compact-target-current-request-hook-context)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "blocks target-pressure prompt without a transcript boundary"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (continued 0) failure)
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (insert "No response boundary\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-should-compact-p)
                       (lambda (&optional _tokens)
                         '(:summary-policy nil :target-pressure t)))
                      ((symbol-function 'mevedel--compact-auto-failure)
                       (lambda (_buffer err) (setq failure err))))
              (with-current-buffer prompt-buf
                (mevedel--compact-transform-auto
                 (lambda () (cl-incf continued)) fsm))))
          (should (= continued 0))
          (should (string-match-p "No compactable history" failure)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "auto threshold uses source-buffer API baseline"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (ran nil)
        (continued nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel-compact-run-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Small pending\n")
            (setq-local mevedel-compact-estimation--known-token-baseline
                        (list :tokens 100
                              :position (copy-marker (point-max)))))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert "tiny\n"))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'mevedel-compact-run-start)
                       (lambda (&rest args)
                         (setq ran t)
                         (funcall (plist-get args :callback) :skip))))
              (with-current-buffer prompt-buf
                (let ((mevedel-compact-estimation-token-threshold 0.5)
                      (mevedel-model-context-limit 200)
                      (gptel-backend nil)
                      (gptel-model nil)
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm)))))
          (should ran)
          (should continued))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "auto threshold uses transformed prompt buffer size"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (ran nil)
        (continued nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel-compact-run-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Small pending\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf)
            (insert (make-string 400 ?x)))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'mevedel-compact-run-start)
                       (lambda (&rest args)
                         (setq ran t)
                         (funcall (plist-get args :callback) :skip))))
              (with-current-buffer prompt-buf
                (let ((mevedel-compact-estimation-token-threshold 0.5)
                      (mevedel-model-context-limit 200)
                      (gptel-backend nil)
                      (gptel-model nil)
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm)))))
          (should ran)
          (should continued))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "summarizer pressure triggers before target-model pressure"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        captured-args captured-policy)
    (unwind-protect
        (progn
          (put 'mevedel-target-model :context-window 2)
          (put 'mevedel-summary-model :context-window 0.2)
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel-compact-run-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Pending\n")
            (setq-local mevedel-compact-estimation--known-token-baseline
                        (list :tokens 200
                              :position (copy-marker (point-max)))))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'mevedel-model-resolve-workload)
                       (lambda (&rest _)
                         '(:backend nil :model mevedel-summary-model)))
                      ((symbol-function 'mevedel-compact-run-start)
                       (lambda (&rest args)
                         (setq captured-args args
                               captured-policy
                               (plist-get
                                (gptel-fsm-info fsm)
                                :mevedel-compaction-target-policy)))))
              (with-current-buffer prompt-buf
                (let ((mevedel-compact-estimation-token-threshold 0.8)
                      (mevedel-model-reserve-tokens 0)
                      (gptel-backend nil)
                      (gptel-model 'mevedel-target-model)
                      (gptel-max-tokens 150)
                      (gptel--request-params '(:temperature 0.5)))
                  (mevedel--compact-transform-auto #'ignore fsm)))))
          (should captured-args)
          (should
           (equal captured-policy
                  '(:backend nil :model mevedel-target-model
                    :effort nil
                    :max-tokens 150
                    :request-params (:temperature 0.5))))
          (should-not (plist-get (plist-get captured-args :admission)
                                 :target-pressure))
          (should (eq (plist-get
                       (plist-get (plist-get captured-args :admission)
                                  :summary-policy)
                       :model)
                      'mevedel-summary-model)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf)))))

(mevedel-deftest mevedel--compact-rebuild-info-data-from-buffer ()
  ,test
  (test)
  :doc "injects current-request reminder before realizing continuation data"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-rebuild*"))
        captured)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (org-mode)
            (setq-local mevedel-compact-target-current-request-reminder
                        "Re-read /tmp/old.el")
            (insert "Prompt\n")
            (let ((response-start (point)))
              (insert "Response\n")
              (put-text-property response-start (point) 'gptel 'response))
            (insert "Tool result\n"))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer chat-buf
                                  :data 'old-data))))
            (cl-letf (((symbol-function 'gptel--create-prompt-buffer)
                       (lambda (&optional _prompt-end)
                         (let ((buf (generate-new-buffer
                                     " *mevedel-compact-realize*")))
                           (with-current-buffer buf
                             (org-mode)
                             (insert-buffer-substring chat-buf))
                           buf)))
                      ((symbol-function 'gptel--realize-query)
                       (lambda (realize-fsm)
                         (let ((prompt-buffer
                                (plist-get (gptel-fsm-info realize-fsm)
                                           :data)))
                           (with-current-buffer prompt-buffer
                             (setq captured (buffer-string)))
                           (plist-put (gptel-fsm-info realize-fsm)
                                      :data 'realized)))))
              (mevedel--compact-rebuild-info-data-from-buffer fsm chat-buf)
              (should (eq (plist-get (gptel-fsm-info fsm) :data) 'realized))
              (should (string-match-p
                       "<system-reminder>\nRe-read /tmp/old.el"
                       captured))
              (with-current-buffer chat-buf
                (should-not mevedel-compact-target-current-request-reminder)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(mevedel-deftest mevedel--compact-main-wait-decision ()
  ,test
  (test)
  :doc "evaluates continuation admission with the realized request policy"
  (let* ((chat-buf (generate-new-buffer " *mevedel-compact-decision*"))
         (policy '(:backend chosen-backend :model chosen-model
                   :effort high :max-tokens 321
                   :request-params (:temperature 0)))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :history '(TRET)
                           :data 'realized
                           :mevedel-compaction-target-policy policy)))
         captured)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-compact-estimation-estimate-data-tokens)
                   (lambda (data)
                     (should (eq data 'realized))
                     42))
                  ((symbol-function 'mevedel--compact-should-compact-p)
                   (lambda (estimate)
                     (setq captured
                           (list estimate gptel-backend gptel-model
                                 gptel-reasoning-effort gptel-max-tokens
                                 gptel--request-params))
                     'admitted)))
          (should
           (equal (list :admission 'admitted :target-policy policy)
                  (mevedel--compact-main-wait-decision fsm)))
          (should
           (equal '(42 chosen-backend chosen-model high 321
                       (:temperature 0))
                  captured)))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--compact-defer-steering-p ()
  ,test
  (test)
  :doc "caches one admission decision for the final WAIT gate"
  (let* ((decision '(:admission (:target-pressure nil)))
         (fsm (gptel-make-fsm :info nil)))
    (cl-letf (((symbol-function 'mevedel--compact-main-wait-decision)
               (lambda (_fsm) decision)))
      (should (equal '(:target-pressure nil)
                     (mevedel--compact-defer-steering-p fsm)))
      (should (eq decision
                  (plist-get (gptel-fsm-info fsm)
                             :mevedel-compaction-wait-decision))))))

(mevedel-deftest mevedel--compact-target-provider-wait ()
  ,test
  (test)
  :doc "root compaction reinjects steering before provider dispatch"
  (let ((fsm (gptel-make-fsm :info nil))
        events)
    (cl-letf (((symbol-function 'mevedel-tools--handle-steering-inject)
               (lambda (_fsm skip-compaction-gate)
                 (push (list 'steering skip-compaction-gate) events)))
              ((symbol-function 'mevedel--compact-provider-wait)
               (lambda (_fsm) (push 'provider events))))
      (mevedel--compact-target-provider-wait '(:invocation nil) fsm))
    (should (equal '((steering t) provider) (nreverse events))))
  :doc "a new interaction hold prevents post-compact provider dispatch"
  (let ((fsm (gptel-make-fsm :info nil))
        sent)
    (cl-letf (((symbol-function 'mevedel-tools--handle-steering-inject)
              (lambda (actual-fsm _skip-compaction-gate)
                 (setf (gptel-fsm-info actual-fsm)
                       (plist-put (gptel-fsm-info actual-fsm)
                                  :mevedel-pending-input-hold t))))
              ((symbol-function 'mevedel--compact-provider-wait)
               (lambda (_fsm) (setq sent t))))
      (mevedel--compact-target-provider-wait '(:invocation nil) fsm))
    (should-not sent))
  :doc "agent compaction dispatches without root steering"
  (let ((fsm (gptel-make-fsm :info nil))
        sent)
    (cl-letf (((symbol-function 'mevedel-tools--handle-steering-inject)
               (lambda (&rest _) (error "Root steering called")))
              ((symbol-function 'mevedel--compact-provider-wait)
               (lambda (_fsm) (setq sent t))))
      (mevedel--compact-target-provider-wait '(:invocation agent) fsm))
    (should sent)))

(mevedel-deftest mevedel--compact-handle-wait ()
  ,test
  (test)
  :doc "does not run continuation gate on initial INIT to WAIT"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (checked nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(INIT)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       (setq checked t)
                       t)))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 1))
          (should-not checked))
      (kill-buffer chat-buf)))

  :doc "held pending input keeps WAIT open without provider dispatch"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-held-wait*"))
        (sent 0))
    (unwind-protect
        (let ((fsm
               (gptel-make-fsm
                :info (list :buffer chat-buf
                            :history '(TRET)
                            :mevedel-pending-input-hold t))))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent))))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 0)))
      (kill-buffer chat-buf)))

  :doc "consumes the admission decision cached before steering injection"
  (with-temp-buffer
    (let* ((decision '(:admission admitted :target-policy nil))
           (fsm
            (gptel-make-fsm
             :info (list :buffer (current-buffer)
                         :mevedel-compaction-wait-decision decision)))
           captured)
      (cl-letf (((symbol-function 'mevedel--compact-main-wait-decision)
                 (lambda (_fsm) (error "Recomputed admission")))
                ((symbol-function 'mevedel-compact-target-main-target)
                 (lambda () 'target))
                ((symbol-function 'mevedel--compact-handle-target-wait)
                 (lambda (actual-fsm target admission
                          &optional _pending-start)
                   (setq captured (list actual-fsm target admission)))))
        (mevedel--compact-handle-wait fsm))
      (should (equal (list fsm 'target 'admitted) captured))
      (should-not (plist-member (gptel-fsm-info fsm)
                                :mevedel-compaction-wait-decision))))

  :doc "sends continuation directly when realized data is below threshold"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content "small")))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest _args) (setq ran t))))
            (let ((mevedel-compact-estimation-token-threshold 0.5)
                  (gptel-model nil)
                  (mevedel-model-context-limit 200))
              (mevedel--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "preserves the realized reasoning effort during continuation admission"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        captured-effort
        sent-effort)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list
                           :buffer chat-buf
                           :history '(TRET)
                           :mevedel-compaction-target-policy
                           '(:backend target-backend :model target-model
                             :effort max :max-tokens nil
                             :request-params nil)
                           :data '(:messages [])))))
          (with-current-buffer chat-buf
            (setq-local gptel-reasoning-effort 'medium))
          (cl-letf (((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       (setq captured-effort gptel-reasoning-effort)
                       nil))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm)
                       (setq sent-effort gptel-reasoning-effort))))
            (mevedel--compact-handle-wait fsm))
          (should (eq captured-effort 'max))
          (should (eq sent-effort 'max)))
      (kill-buffer chat-buf)))

  :doc "does not compact continuation when image payload is below media-aware threshold"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let* ((prefix "data:image/png;base64,")
               (url (concat prefix (make-string 400 ?A)))
               (fsm (gptel-make-fsm
                     :info (list :buffer chat-buf
                                 :history '(TRET)
                                 :data (list :messages
                                             (vector
                                              (list :role "user"
                                                    :content
                                                    (vector
                                                     (list :type "input_image"
                                                           :image_url
                                                           url)))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest _args) (setq ran t))))
            (let ((mevedel-compact-estimation-token-threshold 0.5)
                  (mevedel-model-context-limit 200)
                  (mevedel-compact-estimation-image-token-estimate 10))
              (mevedel--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "uses the realized request reserve instead of chat-buffer settings"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info
                    (list
                     :buffer chat-buf
                     :history '(TRET)
                     :mevedel-compaction-target-policy
                     '(:backend nil :model nil :max-tokens 100
                       :request-params nil)
                     :data
                     (list :messages
                           (vector
                            (list :role "user"
                                  :content (make-string 240 ?x))))))))
          (with-current-buffer chat-buf
            (setq-local gptel-max-tokens nil)
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args)
                       (setq ran t)
                       (funcall (plist-get args :callback) :skip))))
            (let ((mevedel-compact-estimation-token-threshold 0.5)
                  (mevedel-model-context-limit 200)
                  (mevedel-model-reserve-tokens 0))
              (mevedel--compact-handle-wait fsm)))
          (should ran)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "runs continuation gate on TRET to WAIT over threshold"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args)
                       (setq ran t)
                       (funcall (plist-get args :callback) :skip))))
            (let ((mevedel-compact-estimation-token-threshold 0.5)
                  (gptel-model nil)
                  (mevedel-model-context-limit 200))
              (mevedel--compact-handle-wait fsm)))
          (should ran)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "suppresses original wait handler until compaction succeeds"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (rebuilt nil)
        callback)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil)))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args)
                       (setq callback (plist-get args :callback))))
                    ((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (_fsm _chat-buffer)
                       (setq rebuilt t))))
            (mevedel--compact-handle-wait fsm)
            (should (= sent 0))
            (should callback)
            (funcall callback nil))
          (should rebuilt)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "does not send continuation when compaction fails"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        warning)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil)))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args)
                       (funcall (plist-get args :callback) "boom")))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &optional _level _buffer)
                       (setq warning message)))
                    ((symbol-function 'gptel--update-status)
                     #'ignore))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 0))
          (should (string-match-p
                   "Auto-compaction failed; request not sent: boom"
                   warning)))
      (kill-buffer chat-buf)))

  :doc "blocks target-pressure continuation without a transcript boundary"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0) failure)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data '(:messages [])))))
          (with-current-buffer chat-buf
            (insert "No response boundary\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure t)))
                    ((symbol-function 'mevedel--compact-auto-failure)
                     (lambda (_buffer err) (setq failure err))))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 0))
          (should (string-match-p "No compactable history" failure)))
      (kill-buffer chat-buf)))

  :doc "allows summarizer-only continuation without a transcript boundary"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data '(:messages [])))))
          (with-current-buffer chat-buf
            (insert "No response boundary\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil))))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 1)))
      (kill-buffer chat-buf))))


(mevedel-deftest mevedel--compact-main-resume-status ()
  ,test
  (test)
  :doc "restores main request progress after continuation compaction"
  (let ((chat-buffer (generate-new-buffer " *mevedel-compact-chat*"))
        (view-buffer (generate-new-buffer " *mevedel-compact-view*"))
        spinner)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--update-spinner)
                     (lambda (text) (setq spinner text))))
            (mevedel--compact-main-resume-status
             (list :buffer chat-buffer)))
          (should (equal "Thinking..." spinner)))
      (kill-buffer chat-buffer)
      (kill-buffer view-buffer))))

(mevedel-deftest mevedel--compact-target-resume ()
  ,test
  (test)
  :doc "rebuilds a compacted target and resumes its FSM once"
  (let* ((buffer (generate-new-buffer " *mevedel-compact-resume*"))
         (marker (with-current-buffer buffer (copy-marker (point-min))))
         (fsm (gptel-make-fsm :info (list :position marker)))
         rebuilt status (waits 0))
    (unwind-protect
        (progn
          (with-current-buffer buffer (insert "compacted"))
          (cl-letf (((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (actual-fsm actual-buffer)
                       (setq rebuilt (list actual-fsm actual-buffer))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf waits))))
            (mevedel--compact-target-resume
             (list :buffer buffer
                   :resume-status
                   (lambda (_target) (setq status t)))
             fsm))
          (should (equal (list fsm buffer) rebuilt))
          (should status)
          (should (= 1 waits))
          (should (= (with-current-buffer buffer (point-max))
                     (marker-position marker))))
      (set-marker marker nil)
      (kill-buffer buffer))))

(mevedel-deftest mevedel--compact-agent-terminal-failure ()
  ,test
  (test)
  :doc "clears activity, records a structured error, and enters ERRS"
  (let* ((invocation
          (mevedel-agent-invocation-create
           (mevedel-agent--create :name "explorer")))
         (fsm
          (gptel-make-fsm
           :info (list :buffer nil
                       :mevedel-agent-invocation invocation)))
         activity status transition)
    (cl-letf (((symbol-function 'gptel--fsm-transition)
               (lambda (_fsm state) (setq transition state)))
              ((symbol-function 'mevedel-agent-conversation-record-activity)
               (lambda (_invocation item) (setq activity item)))
              ((symbol-function 'gptel--update-status)
               (lambda (text &optional _face) (setq status text))))
      (mevedel--compact-agent-terminal-failure nil fsm "boom"))
    (should (equal '(:type status :summary "error") activity))
    (should (equal " Agent failed" status))
    (should (eq 'ERRS transition))
    (should (equal "Compaction failed: boom"
                   (plist-get (gptel-fsm-info fsm) :status)))
    (should (equal '(:type "compaction_error" :message "boom")
                   (plist-get (gptel-fsm-info fsm) :error)))))

(mevedel-deftest mevedel--compact-main-failure ()
  ,test
  (test)
  :doc "routes target failure through main automatic failure reporting"
  (let ((buffer (generate-new-buffer " *mevedel-compact-failure*"))
        captured paused)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--compact-auto-failure)
                   (lambda (actual-buffer err)
                     (setq captured (list actual-buffer err))))
                  ((symbol-function 'mevedel-goal-pause-runtime-failure)
                   (lambda (actual-buffer reason)
                     (setq paused (list actual-buffer reason)))))
          (mevedel--compact-main-failure
           (list :buffer buffer) nil "boom")
          (should (equal (list buffer "boom") captured))
          (should (equal (list buffer "Compaction failed: boom") paused)))
      (kill-buffer buffer))))

(mevedel-deftest mevedel--compact-handle-target-wait ()
  ,test
  (test)
  :doc "passes through when admission does not request compaction"
  (let ((fsm (gptel-make-fsm :info nil)) (waits 0))
    (cl-letf (((symbol-function 'gptel--handle-wait)
               (lambda (_fsm) (cl-incf waits))))
      (mevedel--compact-handle-target-wait fsm nil nil))
    (should (= 1 waits)))

  :doc "settles no-boundary continuations according to target pressure"
  (dolist (pressure '(nil t))
    (let ((fsm (gptel-make-fsm :info nil)) (waits 0) failure)
      (cl-letf (((symbol-function 'mevedel-compact-evidence-find-boundary)
                 (lambda () nil))
                ((symbol-function 'gptel--handle-wait)
                 (lambda (_fsm) (cl-incf waits))))
        (mevedel--compact-handle-target-wait
         fsm
         (list :fail (lambda (_target _fsm err) (setq failure err)))
         (list :target-pressure pressure)))
      (if pressure
          (progn
            (should (= 0 waits))
            (should (string-match-p "No compactable history" failure)))
        (should (= 1 waits)))))

  :doc "resumes success and routes run or rebuild failures once"
  (dolist (outcome '(success run-error rebuild-error))
    (let ((fsm (gptel-make-fsm :info nil)) resumed failure)
      (cl-letf (((symbol-function 'mevedel-compact-evidence-find-boundary)
                 (lambda () 10))
                ((symbol-function 'mevedel-compact-run-start)
                 (lambda (&rest args)
                   (funcall (plist-get args :callback)
                            (and (eq outcome 'run-error) "run failed")))))
        (mevedel--compact-handle-target-wait
         fsm
         (list :resume
               (lambda (_target _fsm)
                 (if (eq outcome 'rebuild-error)
                     (error "rebuild failed")
                   (setq resumed t)))
               :fail
               (lambda (_target _fsm err) (setq failure err)))
         '(:target-pressure t)))
      (pcase outcome
        ('success (should resumed) (should-not failure))
        ('run-error (should (equal "run failed" failure)))
        ('rebuild-error (should (equal "rebuild failed" failure)))))))

(mevedel-deftest mevedel--compact-handle-agent-wait ()
  ,test
  (test)
  :doc "archives, compacts, rebuilds, and resumes one persisted continuation"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((task (test-mevedel-compact--insert-agent-task
                 invocation "inspect" "Keep this exact task."))
          pre-event post-event captured-system captured-body
          resumed-data statuses messages)
      (setf (mevedel-session-goal session) "PARENT-GOAL-ONLY")
      (setf (mevedel-session-invoked-skills session)
            (list
             (mevedel-skill-invocation-record--create
              :name "parent-only-skill"
              :role 'command
              :origin 'user
              :turn 1)))
      (setf (mevedel-session-touched-files session)
            '(("/tmp/PARENT-FILE-ONLY" . 1)))
      (dolist (turn '(("Old response one.\n" . "Old user two.\n")
                      ("Old response two.\n" . "Old user three.\n")
                      ("Old response three.\n" . "Recent user four.\n")))
        (let ((start (point)))
          (insert (car turn))
          (put-text-property start (point) 'gptel 'response))
        (insert (cdr turn)))
      (let ((start (point)))
        (insert "Recent response four.\n")
        (put-text-property start (point) 'gptel 'response))
      (insert "Pending tool result.\n")
      (basic-save-buffer)
      (let* ((original (buffer-string))
             (data (list :messages
                         (vector
                          (list :role "user"
                                :content (make-string 1000 ?x)))))
             (fsm
              (gptel-make-fsm
               :info
               (list :buffer agent-buffer
                     :history '(TRET)
                     :data data
                     :backend nil
                     :model 'mevedel-agent-target-model
                     :mevedel-agent-invocation invocation
                     :mevedel-compaction-target-policy
                     '(:backend nil :model mevedel-agent-target-model
                       :max-tokens 0 :request-params nil)))))
        (put 'mevedel-agent-target-model :context-window 0.4)
        (put 'mevedel-agent-summary-model :context-window 2)
        (let ((mevedel-compact-estimation-token-threshold 0.5)
              (mevedel-model-reserve-tokens 0)
              (mevedel-compact-evidence-tail-turns 1)
              (mevedel-compact-evidence-tail-budget 1.0)
              (mevedel-compact-run-warn-on-completion t)
              (gptel-backend nil)
              (gptel-model 'mevedel-agent-target-model)
              (gptel-max-tokens 0)
              (gptel--request-params nil)
              (gptel-stream nil)
              (mevedel-pre-compact-functions
               (list
                (lambda (event)
                  (setq pre-event event)
                  '(:additional-context ("agent hook context")))))
              (mevedel-post-compact-functions
               (list (lambda (event) (setq post-event event)))))
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model mevedel-agent-summary-model)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :max-tokens nil
                         :request-params nil)))
                    ((symbol-function 'gptel-request)
                     (lambda (body &rest args)
                       (setq captured-body body
                             captured-system (plist-get args :system))
                       (funcall (plist-get args :callback)
                                (replace-regexp-in-string
                                 "- test"
                                 "- Continue the agent task."
                                 test-mevedel-compact--valid-summary
                                 nil t)
                                nil)))
                    ((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (rebuild-fsm buffer)
                       (plist-put
                        (gptel-fsm-info rebuild-fsm) :data
                        (with-current-buffer buffer (buffer-string)))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (wait-fsm)
                       (push (plist-get (gptel-fsm-info wait-fsm) :data)
                             resumed-data)))
                    ((symbol-function 'gptel--update-status)
                     (lambda (status &optional _face) (push status statuses)))
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (when format-string
                         (let ((rendered
                                (apply #'format format-string args)))
                           (push rendered messages)
                           rendered)))))
            (mevedel--compact-handle-agent-wait fsm)))
        (let* ((canonical (buffer-string))
               (archives
                (directory-files
                 (file-name-directory canonical-path) t
                 "\\.compact-[0-9]+\\.chat\\.org\\'")))
          (should (= 1 (length archives)))
          (should (equal original
                         (with-temp-buffer
                           (insert-file-contents (car archives))
                           (buffer-string))))
          (should (mevedel--same-file-p canonical-path buffer-file-name))
          (should (string-prefix-p task canonical))
          (should (string-match-p "#\\+begin_summary" canonical))
          (should (string-match-p "Continue the agent task" canonical))
          (should (string-match-p "Recent user four" canonical))
          (should (string-match-p "Pending tool result" canonical))
          (should-not (string-match-p "Old user two" canonical))
          (should (string-match-p "mevedel-hook-audit" canonical))
          (should-not (plist-get (gptel-fsm-info fsm) :error))
          (should (= 1 (length resumed-data)))
          (should (string-match-p "Continue the agent task"
                                  (car resumed-data)))
          (should (equal (plist-get pre-event :origin) "/root/explorer"))
          (should (equal (plist-get pre-event :transcript-path)
                         canonical-path))
          (should (equal (plist-get post-event :origin) "/root/explorer"))
          (should (string-match-p "agent hook context" captured-body))
          (should (string-match-p "Old user two" captured-body))
          (should-not (string-match-p "Recent user four" captured-body))
          (dolist (parent-only
                   '("PARENT-GOAL-ONLY" "parent-only-skill"
                     "PARENT-FILE-ONLY"))
            (should-not (string-match-p parent-only captured-system))
            (should-not (string-match-p parent-only captured-body))
            (should-not (string-match-p parent-only canonical)))
          (should (member " Compacting..." statuses))
          (should-not
           (cl-some (lambda (entry) (string-match-p "long threads" entry))
                    messages))
          (should-not mevedel-compact-target-current-request-reminder)))))

  :doc "preserves reasoning and prose before the current persisted tool batch"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this exact task.")
    (let ((old-start (point)))
      (insert "old tool result\n")
      (put-text-property old-start (point) 'gptel '(tool . "call-old")))
    (let ((batch-start (point)))
      (insert "#+begin_reasoning\nthinking\n#+end_reasoning\n")
      (insert (propertize "assistant preface\n" 'gptel 'response))
      (let ((current-start (point)))
        (insert "current tool result\n")
        (put-text-property current-start (point)
                           'gptel '(tool . "call-current")))
      (basic-save-buffer)
      (let* ((fsm
              (gptel-make-fsm
               :info
               (list :buffer agent-buffer
                     :history '(TRET)
                     :data '(:messages [])
                     :tool-use '((:id "call-current"))
                     :mevedel-agent-invocation invocation
                     :mevedel-compaction-target-policy '(:model test))))
             pending-start waits)
        (let ((mevedel-compact-auto t)
              (mevedel--compact-auto-disabled nil)
              (mevedel-compact-run-in-flight nil))
          (cl-letf (((symbol-function 'mevedel-compact-estimation-admission)
                     (lambda (&rest _) '(:target-pressure t)))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args)
                       (setq pending-start
                             (plist-get args :pending-start))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf waits))))
            (mevedel--compact-handle-agent-wait fsm)))
        (should (= batch-start pending-start))
        (should-not waits))))

  :doc "updates the anchored summary and archives each persisted continuation"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((task (test-mevedel-compact--insert-agent-task
                 invocation "inspect" "Keep this exact task."))
          (request-data
           (list :messages
                 (vector
                  (list :role "user" :content (make-string 1000 ?x)))))
          systems bodies (resumed 0)
          first-canonical before-second)
      (dolist (turn '(("Old response one.\n" . "Old user two.\n")
                      ("Old response two.\n" . "Old user three.\n")
                      ("Old response three.\n" . "Recent user four.\n")))
        (let ((start (point)))
          (insert (car turn))
          (put-text-property start (point) 'gptel 'response))
        (insert (cdr turn)))
      (let ((start (point)))
        (insert "Recent response four.\n")
        (put-text-property start (point) 'gptel 'response))
      (insert "Pending result four.\n")
      (basic-save-buffer)
      (let ((original (buffer-string))
            (fsm
             (gptel-make-fsm
              :info
              (list :buffer agent-buffer
                    :history '(TRET)
                    :data request-data
                    :backend nil
                    :model 'mevedel-agent-target-model
                    :mevedel-agent-invocation invocation
                    :mevedel-compaction-target-policy
                    '(:backend nil :model mevedel-agent-target-model
                      :max-tokens 0 :request-params nil)))))
        (put 'mevedel-agent-target-model :context-window 0.4)
        (put 'mevedel-agent-summary-model :context-window 2)
        (let ((mevedel-compact-estimation-token-threshold 0.5)
              (mevedel-model-reserve-tokens 0)
              (mevedel-compact-evidence-tail-turns 1)
              (mevedel-compact-evidence-tail-budget 1.0)
              (gptel-backend nil)
              (gptel-model 'mevedel-agent-target-model)
              (gptel-max-tokens 0)
              (gptel--request-params nil)
              (gptel-stream nil))
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model mevedel-agent-summary-model)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :max-tokens nil
                         :request-params nil)))
                    ((symbol-function 'gptel-request)
                     (lambda (body &rest args)
                       (push body bodies)
                       (push (plist-get args :system) systems)
                       (funcall
                        (plist-get args :callback)
                        (replace-regexp-in-string
                         "- test"
                         (if (= (length bodies) 1)
                             "- First retained fact."
                           "- Updated retained fact.")
                         test-mevedel-compact--valid-summary nil t)
                        nil)))
                    ((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (rebuild-fsm _buffer)
                       (plist-put (gptel-fsm-info rebuild-fsm)
                                  :data request-data)))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf resumed)))
                    ((symbol-function 'gptel--update-status) #'ignore)
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (mevedel--compact-handle-agent-wait fsm)
            (setq first-canonical (buffer-string))
            (goto-char (point-max))
            (insert "New user five.\n")
            (let ((start (point)))
              (insert "New response five.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "New user six.\n")
            (let ((start (point)))
              (insert "New response six.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "Latest user seven.\n")
            (let ((start (point)))
              (insert "Latest response seven.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "Pending result seven.\n")
            (basic-save-buffer)
            (setq before-second (buffer-string))
            (plist-put (gptel-fsm-info fsm) :data request-data)
            (mevedel--compact-handle-agent-wait fsm)))
        (let* ((canonical (buffer-string))
               (archive-directory (file-name-directory canonical-path))
               (first-archive
                (expand-file-name
                 "explorer-test.compact-0001.chat.org"
                 archive-directory))
               (second-archive
                (expand-file-name
                 "explorer-test.compact-0002.chat.org"
                 archive-directory)))
          (should (= 2 resumed))
          (should (= 2 (length bodies)))
          (should (file-exists-p first-archive))
          (should (file-exists-p second-archive))
          (should
           (equal original
                  (with-temp-buffer
                    (insert-file-contents first-archive)
                    (buffer-string))))
          (should
           (equal before-second
                  (with-temp-buffer
                    (insert-file-contents second-archive)
                    (buffer-string))))
          (should (string-prefix-p task first-canonical))
          (should (string-prefix-p task canonical))
          (should (= 1 (how-many "^#\\+begin_summary" (point-min)
                                 (point-max))))
          (should (string-match-p "Updated retained fact" canonical))
          (should (string-match-p "Latest user seven" canonical))
          (should (string-match-p "Pending result seven" canonical))
          (should-not (string-match-p "New user five" canonical))
          (should (string-match-p "previous continuation summary"
                                  (car bodies)))
          (should (string-match-p "First retained fact" (car bodies)))
          (should (string-match-p "New user five" (car bodies)))
          (should-not (string-match-p "Latest user seven" (car bodies)))
          (should (mevedel--same-file-p canonical-path buffer-file-name))))))

  :doc "does not inspect or compact an agent's initial WAIT"
  (let* ((agent-buffer (generate-new-buffer " *mevedel-agent-initial-wait*"))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent--create :name "explorer")))
         (fsm (gptel-make-fsm
               :info (list :buffer agent-buffer
                           :history '(INIT)
                           :mevedel-agent-invocation invocation)))
         (waits 0)
         compacted)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--handle-wait)
                   (lambda (_fsm) (cl-incf waits)))
                  ((symbol-function 'mevedel-compact-run-start)
                   (lambda (&rest _) (setq compacted t))))
          (mevedel--compact-handle-agent-wait fsm)
          (should (= 1 waits))
          (should-not compacted))
      (kill-buffer agent-buffer)))

  :doc "lets ephemeral agents ignore summarizer-only pressure but not target pressure"
  (let* ((agent-buffer (generate-new-buffer " *mevedel-ephemeral-agent*"))
         (invocation
          (mevedel-agent-invocation-create
           (mevedel-agent--create :name "explorer")))
         (fsm
          (gptel-make-fsm
           :table gptel-send--transitions
           :handlers (copy-tree mevedel-agent-exec--handlers)
           :state 'WAIT
           :info
           (list :buffer agent-buffer
                 :history '(TRET)
                 :data
                 (list :messages
                       (vector
                        (list :role "user" :content (make-string 1000 ?x))))
                 :mevedel-agent-invocation invocation
                 :mevedel-compaction-target-policy
                 '(:backend nil :model mevedel-large-agent-model
                   :max-tokens 0 :request-params nil))))
         (waits 0) (terminal-events 0))
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-buffer invocation) agent-buffer)
          (setf (mevedel-agent-invocation-agent-id invocation)
                "explorer--ephemeral")
          (setf (mevedel-agent-invocation-transcript-status invocation)
                'running)
          (plist-put
           (gptel-fsm-info fsm) :mevedel-agent-terminal-callback
           (lambda (_response _info)
             (cl-incf terminal-events)
             (setf (mevedel-agent-invocation-transcript-status invocation)
                   'error)))
          (put 'mevedel-large-agent-model :context-window 2)
          (put 'mevedel-small-agent-model :context-window 0.4)
          (put 'mevedel-agent-summary-model :context-window 0.4)
          (let ((mevedel-compact-estimation-token-threshold 0.5)
                (mevedel-model-reserve-tokens 0))
            (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                       (lambda (&rest _)
                         (error "Summarizer policy must not be resolved")))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _)
                         '(:description "test" :max-tokens nil
                           :request-params nil)))
                      ((symbol-function 'gptel--handle-wait)
                       (lambda (_fsm) (cl-incf waits)))
                      ((symbol-function 'gptel--handle-error) #'ignore)
                      ((symbol-function 'gptel--update-status) #'ignore))
              (with-current-buffer agent-buffer
                (insert "Ephemeral transcript stays intact.\n"))
              (mevedel--compact-handle-agent-wait fsm)
              (should (= 1 waits))
              (should (= 0 terminal-events))
              (plist-put
               (gptel-fsm-info fsm) :mevedel-compaction-target-policy
               '(:backend nil :model mevedel-small-agent-model
                 :max-tokens 0 :request-params nil))
              (mevedel--compact-handle-agent-wait fsm)))
          (should (= 1 waits))
          (should (= 1 terminal-events))
          (should (eq 'ERRS (gptel-fsm-state fsm)))
          (should (eq 'error
                      (mevedel-agent-invocation-transcript-status invocation)))
          (with-current-buffer agent-buffer
            (should-not buffer-file-name)
            (should (equal "Ephemeral transcript stays intact.\n"
                           (buffer-string)))))
      (when (buffer-live-p agent-buffer)
        (kill-buffer agent-buffer))))

  :doc "lets a prefixless persisted agent proceed only below target pressure"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "First recent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (insert "Only recent user turn.\n")
    (let ((start (point)))
      (insert "Second recent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (insert "Pending tool result.\n")
    (basic-save-buffer)
    (let* ((original (buffer-string))
           (fsm
            (gptel-make-fsm
             :table gptel-send--transitions
             :handlers (copy-tree mevedel-agent-exec--handlers)
             :state 'WAIT
             :info
             (list :buffer agent-buffer
                   :history '(TRET)
                   :data
                   (list :messages
                         (vector
                          (list :role "user"
                                :content (make-string 1000 ?x))))
                   :mevedel-agent-invocation invocation
                   :mevedel-compaction-target-policy
                   '(:backend nil :model mevedel-large-agent-model
                     :max-tokens 0 :request-params nil))))
           (waits 0) (terminal-events 0) (requests 0))
      (plist-put
       (gptel-fsm-info fsm) :mevedel-agent-terminal-callback
       (lambda (_response _info)
         (cl-incf terminal-events)
         (setf (mevedel-agent-invocation-transcript-status invocation)
               'error)))
      (put 'mevedel-large-agent-model :context-window 20)
      (put 'mevedel-small-agent-model :context-window 0.4)
      (put 'mevedel-agent-summary-model :context-window 0.4)
      (let ((mevedel-compact-estimation-token-threshold 0.5)
            (mevedel-model-reserve-tokens 0)
            (mevedel-compact-evidence-tail-turns 10)
            (mevedel-compact-evidence-tail-budget 1.0))
        (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                   (lambda (&rest _)
                     '(:backend nil :model mevedel-agent-summary-model)))
                  ((symbol-function 'gptel-get-preset)
                   (lambda (&rest _)
                     '(:description "test" :max-tokens nil
                       :request-params nil)))
                  ((symbol-function 'gptel-request)
                   (lambda (&rest _) (cl-incf requests)))
                  ((symbol-function 'gptel--handle-wait)
                   (lambda (_fsm) (cl-incf waits)))
                  ((symbol-function 'gptel--handle-error) #'ignore)
                  ((symbol-function 'gptel--update-status) #'ignore)
                  ((symbol-function 'display-warning) #'ignore))
          (mevedel--compact-handle-agent-wait fsm)
          (should (= 1 waits))
          (should (= 0 terminal-events))
          (plist-put
           (gptel-fsm-info fsm) :mevedel-compaction-target-policy
           '(:backend nil :model mevedel-small-agent-model
             :max-tokens 0 :request-params nil))
          (mevedel--compact-handle-agent-wait fsm)))
      (should (= 0 requests))
      (should (= 1 waits))
      (should (= 1 terminal-events))
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should (equal original (buffer-string)))
      (should
       (equal original
              (with-temp-buffer
                (insert-file-contents canonical-path)
                (buffer-string)))))))


(provide 'test-mevedel-compact)
;;; test-mevedel-compact.el ends here
