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
(require 'mevedel-memory-cycle)
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
  ,test
  (test)
  :doc "an unresolved numeric handoff is deleted and exactly restored without a semantic audit"
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
                          "---\n```\n## No action\n- none"))
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
      (should (= #o640 (file-modes path)))))
  :doc "a native fifty-note drain attempts invalid inputs once and excludes later or changed versions"
  (let ((old-mtime (file-attribute-modification-time (file-attributes path)))
        (arrival (file-name-concat shared "later-arrival.md"))
        (deadline (+ (float-time) 120))
        pending-response requests responses buffers offered)
    (dotimes (number 49)
      (let ((file (file-name-concat shared (format "finding-%03d.md" (1+ number)))))
        (write-region (format "# Finding %d\nDisposable result only.\n" (1+ number)) nil file nil 'silent)
        (set-file-times file (seconds-to-time (- (float-time) (* 15 86400))))))
    (unwind-protect
        (cl-letf
            (((symbol-function 'url-retrieve)
              (lambda (url callback &rest _)
                (should (string-prefix-p "https://example.invalid/" url))
                (should-not pending-response)
                (let* ((request (json-parse-string url-request-data))
                       (message (seq-find (lambda (message) (equal "user" (gethash "role" message)))
                                          (reverse (append (gethash "messages" request) nil))))
                       (input (json-parse-string (gethash "content" message)))
                       (notes (append (gethash "notes" input) nil))
                       (ids (mapcar (lambda (note) (gethash "id" note)) notes))
                       (running (mevedel-memory-pass-running workspace))
                       (buffer (generate-new-buffer " *note-cohort-provider-url*")))
                  (cl-incf calls)
                  ;; Derive acknowledgement targets from actual serialized input,
                  ;; not the captured roster or a fabricated callback :notes list.
                  (should (<= 1 (length ids) 16))
                  (dolist (id ids)
                    (should-not (member id offered))
                    (should-not (equal id "work://shared/later-arrival.md"))
                    (push id offered))
                  (push (list :pass (plist-get (plist-get running :claim) :owner) :ids ids) requests)
                  (push buffer buffers)
                  (setq pending-response (list :callback callback :buffer buffer :ids ids))
                  buffer))))
          (setq state (mevedel-memory-pass-drain workspace (lambda (value) (push value responses))))
          (while (and (mevedel-memory-pass-drain-status workspace) (< (float-time) deadline))
            (if (not pending-response)
                (accept-process-output nil 0.01)
              (let* ((pending pending-response)
                     (reply
                      (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                              "## Remove\n- none\n## Instructions\n- none\n## Notes\n"
                              (mapconcat
                               (lambda (id)
                                 (format "```proposal\nnote: %S\n%s---\n```\n"
                                         id (if (= calls 1) "unknown: true\n" "")))
                               (plist-get pending :ids) "")
                              "## No action\n- none"))
                     (response
                      (gptel--json-encode
                       (list :choices (vector (list :index 0 :finish_reason "stop"
                                                    :message (list :role "assistant" :content reply)))
                             :usage '(:prompt_tokens 100 :completion_tokens 10 :total_tokens 110)))))
                (setq pending-response nil)
                (when (= calls 1)
                  ;; These are old enough by mtime but not the frozen versions.
                  (write-region "A later arrival.\n" nil arrival nil 'silent)
                  (set-file-times arrival (seconds-to-time (- (float-time) (* 15 86400))))
                  (write-region "An edited handoff after cohort admission.\n" nil path nil 'silent)
                  (set-file-times path old-mtime))
                (with-current-buffer (plist-get pending :buffer)
                  (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
                  (setq-local url-http-end-of-headers (copy-marker (point)) url-http-response-status 200)
                  (insert response)
                  (goto-char (point-min))
                  (funcall (plist-get pending :callback) nil)))))
          (should-not (mevedel-memory-pass-drain-status workspace))
          (should (= 4 calls (length requests)))
          (should (= 49 (length offered) (length (delete-dups (copy-sequence offered)))))
          (let* ((success (seq-filter (lambda (response) (eq 'success (plist-get response :outcome))) responses))
                 (cycle (car (mevedel-memory-cycle--read workspace))))
            (should (= 4 (length success)))
            (should (= 33 (apply #'+ (mapcar (lambda (response) (plist-get response :updated-files)) success))))
            (should (= 33 (apply #'+ (mapcar (lambda (response) (plist-get response :notes-reviewed)) success))))
            (should (= 16 (apply #'+ (mapcar (lambda (response) (plist-get response :note-format-failure-count)) success))))
            (should (eq 'complete (plist-get cycle :status)))
            (should (= 50 (length (plist-get cycle :members))))
            (should (= 49 (length (plist-get cycle :attempted))))
            (should (= 1 (length (plist-get cycle :deferred))))
            (should-not (plist-get cycle :inflight)))
          (dolist (request requests)
            (let ((accepted (mevedel-memory-store-accepted workspace (plist-get request :pass))))
              (should (equal (plist-get request :ids) (plist-get accepted :admitted-notes)))))
          (should-not (plist-get (mevedel-memory-store-accepted workspace (plist-get (car (last requests)) :pass)) :proposals))
          (dotimes (number 49)
            (should (eq (< number 16)
                        (file-exists-p (file-name-concat shared (format "finding-%03d.md" (1+ number)))))))
          (should (file-exists-p arrival))
          (should (equal "An edited handoff after cohort admission.\n"
                         (mevedel-session-control-fs-read-file path))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'test-mevedel-memory-pass-provider)
;;; test-mevedel-memory-pass-provider.el ends here
