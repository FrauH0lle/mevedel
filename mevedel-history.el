;;; mevedel-history.el --- Preserve delivered conversation fragments -*- lexical-binding: t -*-

;;; Commentary:

;; Reconstruct delivered reminders and complete tool-response groups from trusted
;; transcript records.  Ordinary text still uses gptel's backend serializers.
;; Provider fragments cover one rendered response, never the complete history.

;;; Code:

(require 'cl-lib)
(require 'mevedel-transcript-audit)
(require 'mevedel-utilities)

;; `gptel-request'
(declare-function gptel--parse-list "ext:gptel-request" (backend prompt-list))
(declare-function gptel-backend-endpoint "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-host "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-protocol "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-include "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel-backend)
(defvar gptel-cache)
(defvar gptel-include-tool-results)
(defvar gptel-model)

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-transcript'
(autoload 'mevedel-transcript-normalize-properties "mevedel-transcript")

(defvar-local mevedel-history--origin nil
  "Non-nil in a prompt buffer prepared from a mevedel conversation.")

(defun mevedel-history--prepared-hash (text)
  "Fingerprint TEXT and its nonblank provider-visible role boundaries.
Audit insertion can reclassify adjacent blank separators without changing text."
  (let ((position 0) properties)
    (while (< position (length text))
      (let* ((next (next-single-property-change position 'gptel text (length text)))
             (run (substring-no-properties text position next)))
        (when (string-match "[^ \t\n\r]" run)
          (push (list (+ position (match-beginning 0))
                      (get-text-property position 'gptel text)) properties))
        (setq position next)))
    (secure-hash 'sha256
                 (concat (secure-hash 'sha256 (substring-no-properties text))
                         (prin1-to-string (nreverse properties))))))

(defun mevedel-history--provider (backend)
  "Return a transport-credential-free identity for BACKEND."
  (secure-hash 'sha256
               (prin1-to-string
                (list (type-of backend) (gptel-backend-name backend)
                      (gptel-backend-protocol backend) (gptel-backend-host backend)
                      (gptel-backend-endpoint backend)))))

(defun mevedel-history--full-results-p (info)
  "Return non-nil when INFO's complete results are retained in the buffer."
  (or (eq gptel-include-tool-results t)
      (and (eq gptel-include-tool-results 'auto)
           (cl-every
            (lambda (call)
              (when-let* ((tool (cl-find (plist-get call :name) (plist-get info :tools)
                                         :test #'equal :key #'gptel-tool-name)))
                (eq (gptel-tool-include tool) t)))
            (plist-get info :tool-use)))))

(defun mevedel-history--message-key (data)
  "Return the conversation-array key in provider DATA, or nil."
  (cl-find-if (lambda (key) (plist-member data key))
              '(:messages :input :contents)))

(defun mevedel-history-note-dispatch (info)
  "Remember INFO's rendered boundary and message count before dispatch."
  (when-let* ((buffer (plist-get info :buffer))
              ((buffer-live-p buffer))
              ((buffer-local-value 'mevedel--session buffer))
              (key (mevedel-history--message-key (plist-get info :data)))
              (marker (mevedel--active-response-marker info buffer)))
    (when-let* ((old (plist-get info :mevedel-history-start)))
      (set-marker old nil))
    (plist-put info :mevedel-history-start (copy-marker marker nil))
    (plist-put info :mevedel-history-count
               (length (plist-get (plist-get info :data) key)))))

(defun mevedel-history-record-tool-batch (info)
  "Retain INFO's completed provider response beside its rendered text.
The source digest prevents replay after edits or partial context selection."
  (when-let* ((start (plist-get info :mevedel-history-start))
              (buffer (marker-buffer start))
              ((buffer-live-p buffer))
              ((with-current-buffer buffer (mevedel-history--full-results-p info)))
              (end (mevedel--active-response-marker info buffer))
              ((< start end))
              (key (mevedel-history--message-key (plist-get info :data)))
              (messages (plist-get (plist-get info :data) key))
              (count (plist-get info :mevedel-history-count))
              ((< count (length messages))))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (mode major-mode)
             (text (buffer-substring start end))
             (id (plist-get (car (plist-get info :tool-use)) :id))
             (opening (mevedel--format-hook-audit-record
                       (list :type 'provider-tool-batch-start :id id)))
             (record (list :type 'provider-tool-batch :id id
                           :backend (mevedel-history--provider (plist-get info :backend))
                           :model (plist-get info :model)
                           :cache gptel-cache
                           :source-length (length text)
                           :opening-length (length opening)
                           :source-hash
                           (with-temp-buffer
                             (delay-mode-hooks (funcall mode))
                             (insert text)
                             (mevedel-transcript-normalize-properties)
                             (mevedel-history--prepared-hash (buffer-string)))
                           :messages (cl-subseq messages count))))
        (save-excursion
          (goto-char end)
          (insert (mevedel--format-hook-audit-record record))
          (set-marker end (point))
          (goto-char start)
          (insert opening))))
    (set-marker start nil)
    (plist-put info :mevedel-history-start nil)))

(defun mevedel-history--replay-span (span text backend)
  "Return (START END MESSAGES) for a replayable SPAN in TEXT for BACKEND.
Offsets are relative to TEXT.  Reject modified or incomplete source spans."
  (let* ((record (plist-get span :record))
         (begin (mevedel-transcript--audit-block-start
                 text (plist-get span :start)))
         (end (mevedel-transcript--audit-block-end text (plist-get span :end))))
    (unless (eq (get-text-property (plist-get span :start) 'gptel text) 'ignore)
      (pcase (plist-get record :type)
	('injected-reminders
	 (let ((gptel-cache nil))
           (list begin end
		 (gptel--parse-list
                  backend
                  (list (cons 'prompt
                              (mapconcat
                               (lambda (entry)
				 (format "<system-reminder>\n%s\n</system-reminder>"
					 (plist-get entry :body)))
                               (plist-get record :items) "\n")))))))
	('provider-tool-batch
	 (let* ((length (plist-get record :source-length))
		(start (and (natnump length) (- begin length))))
           (when (and start (>= start 0)
                      (equal (plist-get record :source-hash)
                             (mevedel-history--prepared-hash (substring text start begin)))
                      (equal (plist-get record :backend) (mevedel-history--provider backend))
                      (equal (plist-get record :model) gptel-model)
                      (equal (plist-get record :cache) gptel-cache))
             (let* ((opening-length (plist-get record :opening-length))
                    (opening-start (and (natnump opening-length)
					(- start opening-length))))
               (when (and opening-start (>= opening-start 0)
                          (equal
                           (substring-no-properties text opening-start start)
                           (substring-no-properties
                            (mevedel--format-hook-audit-record
                             (list :type 'provider-tool-batch-start
                                   :id (plist-get record :id))))))
		 (list opening-start end
                       (append (copy-tree (plist-get record :messages) t) nil)))))))))))

(defun mevedel-history--parse-buffer-advice (original backend &optional max-entries)
  "Reconstruct retained fragments while ORIGINAL parses ordinary text.
Respect BACKEND and MAX-ENTRIES, and never read beyond the selected context."
  (if (not (or mevedel-history--origin (bound-and-true-p mevedel--session)))
      (funcall original backend max-entries)
    (let* ((end (point))
           (ordinary (funcall original backend max-entries))
           (begin (point))
           (text (buffer-substring begin end))
           (spans (mevedel-transcript-audit-spans text))
           (cursor 0)
           replays
           fragments)
      (let ((position 0))
	(while (< position (length text))
          (let* ((next (next-single-property-change
			position 'mevedel-history-fragment text (length text)))
		 (record (get-text-property position 'mevedel-history-fragment text)))
            (when (and record
                       (equal (plist-get record :backend) (mevedel-history--provider backend))
                       (equal (plist-get record :model) gptel-model)
                       (equal (plist-get record :cache) gptel-cache)
                       (equal (plist-get record :prepared-hash)
                              (mevedel-history--prepared-hash (substring text position next)))
                       (let ((records (mevedel-transcript-audit-records
                                       (substring text position next))))
			 (and (cl-find (plist-get record :id) records :test #'equal
                                       :key (lambda (entry)
                                              (and (eq (plist-get entry :type)
                                                       'provider-tool-batch-start)
                                                   (plist-get entry :id))))
                              (cl-find (plist-get record :id) records :test #'equal
                                       :key (lambda (entry)
                                              (and (eq (plist-get entry :type)
                                                       'provider-tool-batch)
                                                   (plist-get entry :id)))))))
              (push (list position next
                          (append (copy-tree (plist-get record :messages) t) nil))
                    replays))
            (setq position next))))
      (dolist (span spans)
	(unless (and mevedel-history--origin
                     (eq (plist-get (plist-get span :record) :type) 'provider-tool-batch))
          (when-let* ((replay (mevedel-history--replay-span span text backend)))
            (push replay replays))))
      (dolist (replay (sort replays (lambda (a b) (< (car a) (car b)))))
	(when (>= (car replay) cursor)
          (when (< cursor (car replay))
            (let ((gptel-cache nil))
              (save-restriction
		(narrow-to-region (+ begin cursor) (+ begin (car replay)))
		(goto-char (point-max))
		(push (funcall original backend nil) fragments))))
          (push (nth 2 replay) fragments)
          (setq cursor (nth 1 replay))))
      (if (null fragments)
          ordinary
	(when (< cursor (length text))
          (save-restriction
            (narrow-to-region (+ begin cursor) end)
            (goto-char (point-max))
            (unless (string-blank-p (buffer-substring-no-properties (point-min) (point-max)))
              (push (funcall original backend nil) fragments))))
	(goto-char begin)
	(mevedel-history--cache-messages backend (apply #'append (nreverse fragments)))))))

(defun mevedel-history--cache-messages (backend messages)
  "Apply BACKEND's current message-cache annotation to assembled MESSAGES."
  (when-let* ((last (car (last messages))))
    (let* ((content (plist-get last :content))
           (template (car (gptel--parse-list backend (list (cons 'prompt "")))))
           (parts (plist-get template :content))
           (control (and (vectorp parts) (> (length parts) 0)
                         (plist-get (aref parts 0) :cache_control))))
      (when control
        (if (stringp content)
            (plist-put last :content
                       (vector (list :type "text" :text content :cache_control control)))
          (when (and (vectorp content) (> (length content) 0))
            (plist-put (aref content 0) :cache_control control))))))
  messages)

(defun mevedel-history--prepare-buffer-advice (original &rest arguments)
  "Carry validated fragments through ORIGINAL's prompt-buffer projection.
Org removes block headers and unescapes results while copying the transcript.
Temporary properties map the already-validated spans through those edits.
Remove hidden metadata between runs of one tool result in the prompt copy,
so backend parsers read its call metadata once and retain the entire result."
  (if (not (bound-and-true-p mevedel--session))
      (apply original arguments)
    (let ((buffer (current-buffer))
          (modified (buffer-modified-p))
          (text (buffer-substring (point-min) (point-max)))
          ranges)
      (unwind-protect
          (progn
            (with-silent-modifications
              (dolist (span (mevedel-transcript-audit-spans text 'provider-tool-batch))
		(when-let* ((replay (mevedel-history--replay-span span text gptel-backend)))
                  (let ((start (+ (point-min) (car replay)))
			(end (+ (point-min) (nth 1 replay))))
                    (put-text-property start end 'mevedel-history-fragment
                                       (plist-get span :record))
                    (push (cons (copy-marker start) (copy-marker end)) ranges)))))
            (let ((prompt (apply original arguments)))
              (with-current-buffer prompt
		(setq-local mevedel-history--origin t)
		(let ((position (point-min)))
                  (while (< position (point-max))
                    (let ((next (next-single-property-change
                                 position 'gptel nil (point-max)))
                          (previous (and (> position (point-min))
                                         (get-text-property (1- position) 'gptel))))
                      (when (and (eq (car-safe previous) 'tool)
                                 (memq (get-text-property position 'gptel)
                                       '(mevedel-hook-audit mevedel-render-data)))
                        (while (and (< next (point-max))
                                    (memq (get-text-property next 'gptel)
                                          '(mevedel-hook-audit mevedel-render-data)))
                          (setq next (next-single-property-change
                                      next 'gptel nil (point-max))))
                        (when (equal previous (get-text-property next 'gptel))
                          (delete-region position next)
                          (setq next position)))
                      (setq position next))))
		(let ((position (point-min)))
                  (while (< position (point-max))
                    (let* ((next (next-single-property-change
                                  position 'mevedel-history-fragment nil (point-max)))
                           (record (get-text-property position 'mevedel-history-fragment)))
                      (when record
			(put-text-property
			 position next 'mevedel-history-fragment
			 (plist-put (copy-sequence record) :prepared-hash
                                    (mevedel-history--prepared-hash
                                     (buffer-substring position next)))))
                      (setq position next)))))
              prompt))
	(when (buffer-live-p buffer)
          (with-current-buffer buffer
            (with-silent-modifications
              (dolist (range ranges)
		(remove-text-properties (car range) (cdr range)
					'(mevedel-history-fragment nil))
		(set-marker (car range) nil)
		(set-marker (cdr range) nil)))
            (set-buffer-modified-p modified)))))))

(defun mevedel-history-install ()
  "Install retained-history reconstruction at gptel's parsing boundary."
  (advice-add 'gptel--create-prompt-buffer :around #'mevedel-history--prepare-buffer-advice)
  (advice-add 'gptel--parse-buffer :around #'mevedel-history--parse-buffer-advice))

(defun mevedel-history-uninstall ()
  "Remove retained-history reconstruction."
  (advice-remove 'gptel--create-prompt-buffer #'mevedel-history--prepare-buffer-advice)
  (advice-remove 'gptel--parse-buffer #'mevedel-history--parse-buffer-advice))

(provide 'mevedel-history)
;;; mevedel-history.el ends here
