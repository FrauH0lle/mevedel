;;; test-mevedel-memory-pass-provider.el --- Note deletion through native provider parsing -*- lexical-binding: t -*-

;;; Commentary:
;; Run real pass -> native gptel URL response -> parser -> acceptance -> checked
;; deletion -> undo.  Only URL transport is replaced; no provider is contacted.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'gptel-request)
(require 'gptel-openai)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-store)
(require 'mevedel-models)
(require 'mevedel-telemetry)
(require 'mevedel-telemetry-usage)
(require 'mevedel-working-note)

(mevedel-deftest mevedel-memory-pass-start/provider
    (:quiet t
     :vars* ((root (make-temp-file "mevedel-note-provider-" t))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (path (file-name-concat shared "open-handoff.md"))
             (original (mevedel-working-note-normalize
                        "---\nkind: handoff\nstatus: open\n---\n# Open handoff\nTODO: port the remote fix.\n512\treads on target A.\nMeasured update: 12.25 ms median, 6 paired samples.\n"
                        "owner" "2000-01-01T00:00:00Z"))
             (mevedel-memory-dirs nil)
             (mevedel-memory-consolidation-mode 'auto)
             (mevedel-journal-worker--child-p t)
             (mevedel-telemetry-enabled t)
             (gptel--known-backends nil)
             (gptel--request-alist nil)
             (gptel-use-curl nil) (gptel-stream nil) (gptel-log-level nil)
             (model (make-symbol "note-extraction-model"))
             (backend (gptel-make-openai "note-extraction" :key "synthetic" :host "example.invalid"
                                         :stream nil :models (list model)))
             (mevedel-model-workloads (list (list 'memory :provider (cons "note-extraction" model))))
             (observer-advice
              '((gptel--handle-wait . mevedel-telemetry-usage--dispatch)
                (gptel-curl--parse-response . mevedel-telemetry-usage--parse-response)
                (gptel--url-parse-response . mevedel-telemetry-usage--parse-response)
                (gptel-curl--stream-cleanup . mevedel-telemetry-usage--stream-cleanup)
                (gptel-curl--parse-stream . mevedel-telemetry-usage--parse-stream)
                (gptel--fsm-transition . mevedel-telemetry-usage--abort)))
             (new-advice (seq-remove (lambda (pair) (advice-member-p (cdr pair) (car pair))) observer-advice))
             (calls 0) pending transport state result)
     :before-each ((put model :context-window 128) (put model :capabilities '(tool-use))
                   (make-directory shared t)
                   (write-region original nil path nil 'silent)
                   (set-file-modes path #o640)
                   (set-file-times path (seconds-to-time (- (floor (float-time)) (* 14 86400)))))
     :after-each ((mevedel-memory-pass-stop-all)
                  (when (buffer-live-p transport) (kill-buffer transport))
                  (dolist (pair new-advice) (advice-remove (car pair) (cdr pair)))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (url callback &rest _)
               (should (string-prefix-p "https://example.invalid/" url))
               (cl-incf calls)
               (should (string-search "work://shared/open-handoff.md" url-request-data))
               (should (string-search "TODO: port the remote fix" url-request-data))
               (should (string-search "512" url-request-data))
               (should (string-search "12.25 ms median" url-request-data))
               (setq pending callback transport (generate-new-buffer " *note-provider-url*")))))
    (setq state (mevedel-memory-pass-start workspace (lambda (value) (setq result value))))
    (should state)
    (should (= 1 calls))
    (should pending)
    (let* ((reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n"
                          "## Instructions\n- none\n## Notes\n```proposal\nnote: \"work://shared/open-handoff.md\"\n"
                          "disposition: \"delete\"\nreason: \"No qualifying durable lesson\"\n---\n```\n## No action\n- none"))
           (response (gptel--json-encode
                      (list :choices (vector (list :index 0 :finish_reason "stop"
                                                   :message (list :role "assistant" :content reply)))
                            :usage '(:prompt_tokens 100 :completion_tokens 10 :total_tokens 110)))))
      (with-current-buffer transport
        (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
        (setq-local url-http-end-of-headers (copy-marker (point)) url-http-response-status 200)
        (insert response)
        (goto-char (point-min))
        (funcall pending nil)))
    (should (eq 'success (plist-get result :outcome)))
    (should-not (plist-get result :error))
    (should (= 1 (plist-get result :notes-reviewed)))
    (should (= 0 (plist-get result :note-format-failure-count)))
    (should (= 1 (plist-get result :updated-files)))
    (should (= 1 (plist-get result :provider-call-count)))
    (dolist (key '(:guard-converted-keep-count :guard-preserved-condense-count :model-keep-count))
      (should-not (plist-member result key)))
    (let* ((pass (plist-get (plist-get state :claim) :owner))
           (accepted (mevedel-memory-store-accepted workspace pass))
           (proposals (plist-get accepted :proposals))
           (proposal (car proposals)))
      ;; No extraction or invented semantic keep can account for this deletion.
      (should (= 1 (length proposals)))
      (should (eq 'note-delete (plist-get proposal :action)))
      (should-not (plist-get proposal :requires))
      (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (file-exists-p path))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get proposal :id)) :status)))
      (should (equal original (with-temp-buffer (insert-file-contents path) (buffer-string))))
      (should (= #o640 (file-modes path))))))

(provide 'test-mevedel-memory-pass-provider)
;;; test-mevedel-memory-pass-provider.el ends here
