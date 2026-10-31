;;; test-mevedel-memory-pass-provider.el --- Note deletion through native provider parsing -*- lexical-binding: t -*-

;;; Commentary:
;; Run real pass -> native gptel URL response -> parser -> acceptance -> checked
;; permanent deletion.  Only URL transport is replaced; no provider is contacted.

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

(mevedel-deftest mevedel-memory-pass-start/provider
    (:quiet t
     :vars* ((root (make-temp-file "mevedel-note-provider-" t))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (path (file-name-concat shared "open-handoff.md"))
             (original "# Open handoff\nTODO: port the remote fix.\n512\treads on target A.\nMeasured update: 12.25 ms median, 6 paired samples.\n")
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
  :doc "an unresolved numeric handoff is permanently deleted without a semantic audit"
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
                          "## Instructions\n- none\n## No action\n- none"))
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
    (should-not (plist-member result :note-format-failure-count))
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
      (should-error (mevedel-memory-decision-reverse workspace pass (plist-get proposal :id)) :type 'user-error)
      (should-not (file-exists-p path))))
  :doc "a rejected native memory reply creates no disposal and leaves the admitted source unchanged"
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (_url callback &rest _)
               (cl-incf calls)
               (should (string-search "work://shared/open-handoff.md" url-request-data))
               (setq pending callback transport (generate-new-buffer " *note-rejected-provider-url*")))))
    (setq state (mevedel-memory-pass-start workspace (lambda (value) (setq result value))))
    (should state)
    (should (= 1 calls))
    (let* ((reply (concat "## Promote\n````proposal\nnote: \"work://shared/open-handoff.md\"\n---\n````\n"
                          "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n"
                          "## Instructions\n- none\n## No action\n- none"))
           (response (gptel--json-encode
                      (list :choices (vector (list :index 0 :finish_reason "stop"
                                                   :message (list :role "assistant" :content reply)))))))
      (with-current-buffer transport
        (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
        (setq-local url-http-end-of-headers (copy-marker (point)) url-http-response-status 200)
        (insert response)
        (goto-char (point-min))
        (funcall pending nil)))
    (should (eq 'error (plist-get result :outcome)))
    (should (equal "Unknown or repeated proposal field" (plist-get result :error)))
    (should (= 0 (or (plist-get result :updated-files) 0)))
    (should-not (mevedel-memory-store-accepted workspace (plist-get (plist-get state :claim) :owner)))
    (should (equal original (mevedel-session-control-fs-read-file path)))
    (should (= #o640 (file-modes path))))
  :doc "a native multi-batch drain deletes admitted notes without acknowledgements and excludes changed or late versions"
  ;; A two-note request bound keeps four serialized requests (2+2+2+1) over an
  ;; eight-member cohort with real deletions per batch.  The production
  ;; sixteen-note request size stays covered by the drain and cycle tests.
  (let ((old-mtime (file-attribute-modification-time (file-attributes path)))
        (arrival (file-name-concat shared "later-arrival.md"))
        (recent (file-name-concat shared "recent-handoff.md"))
        (mevedel-memory-notes--max-notes 2)
        (findings 7)
        (deadline (+ (float-time) 120))
        pending-response requests responses buffers offered)
    ;; A headerless handoff below fourteen days is not in the eligible cohort.
    (write-region "# Recent handoff\nTODO: still working.\n" nil recent nil 'silent)
    (set-file-times recent (seconds-to-time (- (float-time) (* 13 86400))))
    (dotimes (number findings)
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
                  ;; Observe admitted notes from actual serialized input,
                  ;; not the captured roster or a fabricated callback :notes list.
                  (should (<= 1 (length ids) 2))
                  (dolist (id ids)
                    (should-not (member id offered))
                    (should-not (equal id "work://shared/later-arrival.md"))
                    (should-not (equal id "work://shared/recent-handoff.md"))
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
                              "## Remove\n- none\n## Instructions\n- none\n"
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
          (should (= findings (length offered) (length (delete-dups (copy-sequence offered)))))
          (let* ((success (seq-filter (lambda (response) (eq 'success (plist-get response :outcome))) responses))
                 (cycle (car (mevedel-memory-cycle--read workspace))))
            (should (= 4 (length success)))
            (should (= findings (apply #'+ (mapcar (lambda (response) (plist-get response :updated-files)) success))))
            (should (= findings (apply #'+ (mapcar (lambda (response) (plist-get response :notes-reviewed)) success))))
            (should (eq 'complete (plist-get cycle :status)))
            (should (= (1+ findings) (length (plist-get cycle :members))))
            (should (= findings (length (plist-get cycle :attempted))))
            (should (= 1 (length (plist-get cycle :deferred))))
            (should-not (plist-get cycle :inflight)))
          (dolist (request requests)
            (let ((accepted (mevedel-memory-store-accepted workspace (plist-get request :pass))))
              (should (equal (plist-get request :ids) (plist-get accepted :admitted-notes)))
              (should (equal (plist-get request :ids)
                             (mapcar (lambda (item) (plist-get item :note)) (plist-get accepted :proposals))))))
          (dotimes (number findings)
            (should-not (file-exists-p (file-name-concat shared (format "finding-%03d.md" (1+ number))))))
          (should (file-exists-p arrival))
          (should (equal "# Recent handoff\nTODO: still working.\n"
                         (mevedel-session-control-fs-read-file recent)))
          (should (equal "An edited handoff after cohort admission.\n"
                         (mevedel-session-control-fs-read-file path))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'test-mevedel-memory-pass-provider)
;;; test-mevedel-memory-pass-provider.el ends here
