;;; test-mevedel-session-artifacts-staged.el --- Staged save selection -*- lexical-binding: t -*-

;;; Commentary:

;; The transcript proof and the staged-save selector decide, from frozen state
;; alone, whether a save is an exact no-op.  Each case stages real files with
;; the production helpers and compares with synchronously computed hashes.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))

(defun test-mevedel-staged--batch (session buffer payloads &optional deletes marker-text)
  "Stage PAYLOADS, DELETES and a sidecar marker of SESSION from BUFFER.
PAYLOADS is a list of (LOGICAL . CONTENT).  Every written source receives its
synchronously computed digest, as the asynchronous digest step would set it."
  (let* ((root (file-name-as-directory (mevedel-session-save-path session)))
         (batch (mevedel-session-publication--stage-artifacts
                 session
                 (append
                  (mapcar (lambda (payload)
                            (list :path (file-name-concat root (car payload))
                                  :content (cdr payload)))
                          payloads)
                  (mapcar (lambda (logical)
                            (list :path (file-name-concat root logical) :delete t))
                          deletes)
                  (list (list :path (file-name-concat root "session.meta.el")
                              ;; A real sidecar: the selector reads the staged
                              ;; marker as the frozen, projected snapshot.
                              :content (or marker-text
                                           (mevedel-session-artifacts-printed-value
                                            (mevedel-session-artifacts--live-sidecar
                                             session buffer)))
                              :commit-marker t)))
                 t)))
    (dolist (artifact (plist-get batch :artifacts))
      (when (plist-get artifact :source)
        (setf (plist-get artifact :source-sha256)
              (mevedel-session-publication--file-sha256
               (plist-get artifact :source)))))
    batch))

(defun test-mevedel-staged--artifact (batch logical)
  "Return BATCH's artifact for LOGICAL."
  (cl-find logical (plist-get batch :artifacts)
           :key (lambda (artifact) (plist-get artifact :logical)) :test #'equal))

(defun test-mevedel-staged--entries (batch logicals)
  "Return manifest entries naming the digests BATCH staged for LOGICALS."
  (mapcar (lambda (logical)
            (list logical :published (concat ".publications/x/" logical)
                  :sha256 (plist-get (test-mevedel-staged--artifact batch logical)
                                     :source-sha256)))
          logicals))

(defun test-mevedel-staged--frozen (session buffer batch &rest overrides)
  "Return frozen state matching BATCH, with OVERRIDES, for SESSION and BUFFER."
  (let ((frozen (list :sidecar (copy-tree
                                (mevedel-session-artifacts--live-sidecar session buffer))
                      :stamp "2099-01-01T00-00-00" :force nil :comparable t
                      :entries (test-mevedel-staged--entries
                                batch '("notes/a.txt" "session.meta.el")))))
    (while overrides
      (setf (plist-get frozen (pop overrides)) (pop overrides)))
    frozen))

(defun test-mevedel-staged--read (path)
  "Return the raw bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(mevedel-deftest mevedel-session-artifacts--transcript-proof (:quiet t)
  ,test
  (test)
  :doc "captures identity, coding, restriction, path, epoch, tick and digest but no text"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (let ((proof (mevedel-session-artifacts--transcript-proof session))
             (text (buffer-substring-no-properties (point-min) (point-max))))
         (should (eq buffer (nth 0 proof)))
         (should (eq (mevedel-session-checkpoint-state session) (nth 1 proof)))
         (should (equal (expand-file-name buffer-file-name) (nth 2 proof)))
         (should (eql (buffer-chars-modified-tick) (nth 3 proof)))
         (should (eq (or buffer-file-coding-system 'utf-8-unix) (nth 4 proof)))
         (should (eq enable-multibyte-characters (nth 5 proof)))
         (should (equal (list (point-min) (point-max)) (list (nth 6 proof) (nth 7 proof))))
         (should (equal (plist-get
                         (cdr (assoc (file-relative-name
                                      buffer-file-name (mevedel-session-save-path session))
                                     (plist-get (mevedel-session-publication session)
                                                :artifacts)))
                         :sha256)
                        (nth 8 proof)))
         ;; A small identity, never the transcript.
         (should-not (cl-some (lambda (item)
                                (and (stringp item) (equal item text)))
                              proof))
         (should (< (length (prin1-to-string proof))
                    (+ 600 (length buffer-file-name))))))))
  :doc "an edit, a narrowing or a changed coding changes the proof"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (let ((proof (mevedel-session-artifacts--transcript-proof session)))
         (save-restriction
           (narrow-to-region (point-min) (1- (point-max)))
           (should-not (equal proof (mevedel-session-artifacts--transcript-proof session))))
         (let ((inhibit-read-only t))
           (save-excursion (goto-char (point-max)) (insert "x")))
         (should-not (equal proof (mevedel-session-artifacts--transcript-proof session)))
         (let ((buffer-file-coding-system 'iso-8859-1))
           (should-not (equal (nth 4 proof)
                              (nth 4 (mevedel-session-artifacts--transcript-proof
                                      session)))))))))
  :doc "a buffer without a file or a session without a path has no proof"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (let ((buffer-file-name nil))
         (should-not (mevedel-session-artifacts--transcript-proof session)))
       (let ((path (mevedel-session-save-path session)))
         (unwind-protect
             (progn (setf (mevedel-session-save-path session) nil)
                    (should-not (mevedel-session-artifacts--transcript-proof session)))
           (setf (mevedel-session-save-path session) path)))))))

(mevedel-deftest mevedel-session-artifacts--select-staged-save (:quiet t)
  ,test
  (test)
  :doc "digests and sidecar equal to the manifest are an exact no-op"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha"))))
            (frozen (test-mevedel-staged--frozen session buffer batch))
            (before (copy-tree frozen))
            (stamp (mevedel-session-updated-at session))
            (marker-bytes (test-mevedel-staged--read
                           (plist-get (test-mevedel-staged--artifact
                                       batch "session.meta.el")
                                      :source))))
       (should-not (mevedel-session-artifacts--select-staged-save session batch frozen))
       (should (equal before frozen))
       (should (equal stamp (mevedel-session-updated-at session)))
       (should (equal marker-bytes
                      (test-mevedel-staged--read
                       (plist-get (test-mevedel-staged--artifact batch "session.meta.el")
                                  :source)))))))
  :doc "a changed payload keeps only it and stamps a marker with the correct bytes and hash"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha") ("notes/b.txt" . "beta"))))
            (frozen (test-mevedel-staged--frozen
                     session buffer batch
                     :entries (append (test-mevedel-staged--entries
                                       batch '("notes/b.txt" "session.meta.el"))
                                      '(("notes/a.txt" :published "p" :sha256 "old")))))
            (expected (encode-coding-string
                       (mevedel-session-artifacts-printed-value
                        (plist-put (copy-sequence (plist-get frozen :sidecar))
                                   :updated-at (plist-get frozen :stamp)))
                       'utf-8-unix t))
            (result (cl-letf (((symbol-function 'mevedel-session-artifacts--live-sidecar)
                               (lambda (&rest _) (ert-fail "Live sidecar read"))))
                      (mevedel-session-artifacts--select-staged-save
                       session batch frozen)))
            (marker (test-mevedel-staged--artifact result "session.meta.el")))
       (should (eq batch result))
       (should (equal '("notes/a.txt" "session.meta.el")
                      (mapcar (lambda (artifact) (plist-get artifact :logical))
                              (plist-get result :artifacts))))
       (should (eq marker (car (last (plist-get result :artifacts)))))
       (should (equal expected (test-mevedel-staged--read (plist-get marker :source))))
       (should (equal (secure-hash 'sha256 expected) (plist-get marker :source-sha256)))
       (should (equal (plist-get marker :source-sha256)
                      (mevedel-session-publication--file-sha256 (plist-get marker :source))))
       (should (equal (plist-get frozen :stamp) (mevedel-session-updated-at session))))))
  :doc "the written sidecar is the frozen snapshot whatever the live session becomes"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha"))))
            (frozen (test-mevedel-staged--frozen
                     session buffer batch :entries nil))
            (frozen-name (mevedel-session-name session))
            (before (copy-tree frozen)))
       ;; Live mutation after the full snapshot: a string and a nested field.
       (setf (mevedel-session-name session) "mutated-live-name"
             (mevedel-session-prompt-index session)
             (list (list 99 (list :text "mutated-live-prompt"))))
       (let* ((result (mevedel-session-artifacts--select-staged-save
                       session batch frozen))
              (disk (test-mevedel-staged--read
                     (plist-get (test-mevedel-staged--artifact result "session.meta.el")
                                :source))))
         (should-not (string-match-p "mutated-live" disk))
         (should (string-match-p (regexp-quote frozen-name) disk))
         ;; The frozen snapshot itself is neither read through nor modified.
         (should (equal before frozen))))))
  :doc "a session that is not comparable never reports an exact no-op"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha"))))
            (frozen (test-mevedel-staged--frozen session buffer batch :comparable nil))
            (result (mevedel-session-artifacts--select-staged-save session batch frozen)))
       (should result)
       (should (equal '("notes/a.txt" "session.meta.el")
                      (mapcar (lambda (artifact) (plist-get artifact :logical))
                              (plist-get result :artifacts)))))))
  :doc "a forced save is never an exact no-op and keeps only changed payloads"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha"))))
            (frozen (test-mevedel-staged--frozen session buffer batch :force t))
            (result (mevedel-session-artifacts--select-staged-save session batch frozen)))
       (should result)
       (should (equal '("session.meta.el")
                      (mapcar (lambda (artifact) (plist-get artifact :logical))
                              (plist-get result :artifacts)))))))
  :doc "a sidecar that differs from the manifest is written even when payloads match"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha"))))
            (frozen (test-mevedel-staged--frozen
                     session buffer batch
                     :entries (append (test-mevedel-staged--entries
                                       batch '("notes/a.txt"))
                                      '(("session.meta.el" :published "p"
                                         :sha256 "different")))))
            (result (mevedel-session-artifacts--select-staged-save session batch frozen)))
       (should result)
       (should (equal '("session.meta.el")
                      (mapcar (lambda (artifact) (plist-get artifact :logical))
                              (plist-get result :artifacts)))))))
  :doc "a replacing artifact is always kept"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha"))))
            (frozen (test-mevedel-staged--frozen session buffer batch)))
       (nconc (test-mevedel-staged--artifact batch "notes/a.txt") (list :replace t))
       (should (mevedel-session-artifacts--select-staged-save session batch frozen)))))
  :doc "a tombstone is dropped without a manifest entry and kept with one"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha")) '("notes/gone.txt")))
            (kept (mevedel-session-artifacts--select-staged-save
                   session batch
                   (test-mevedel-staged--frozen
                    session buffer batch
                    :entries (append (test-mevedel-staged--entries
                                      batch '("notes/a.txt" "session.meta.el"))
                                     '(("notes/gone.txt" :published "p" :sha256 "x"))))))
            (batch2 (test-mevedel-staged--batch
                     session buffer '(("notes/a.txt" . "alpha")) '("notes/gone.txt"))))
       (should kept)
       (should (member "notes/gone.txt"
                       (mapcar (lambda (artifact) (plist-get artifact :logical))
                               (plist-get kept :artifacts))))
       ;; Without an entry the delete changes nothing: an exact no-op.
       (should-not (mevedel-session-artifacts--select-staged-save
                    session batch2
                    (test-mevedel-staged--frozen session buffer batch2))))))
  :doc "a tombstone is kept when the session is not comparable"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer nil '("notes/gone.txt")))
            (result (mevedel-session-artifacts--select-staged-save
                     session batch
                     (test-mevedel-staged--frozen
                      session buffer batch :comparable nil :entries nil))))
       (should (member "notes/gone.txt"
                       (mapcar (lambda (artifact) (plist-get artifact :logical))
                               (plist-get result :artifacts)))))))
  :doc "a batch without a sidecar marker is refused"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (ignore buffer)
     (let ((batch (test-mevedel-staged--batch session buffer nil)))
       (setf (plist-get batch :artifacts) nil)
       (should-error (mevedel-session-artifacts--select-staged-save
                      session batch (test-mevedel-staged--frozen session buffer batch))
                     :type 'error))))
  :doc "recovery rows deferred between capture and stage survive in the saved marker"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (test-mevedel-staged--batch
                    session buffer '(("notes/a.txt" . "alpha"))))
            (frozen (test-mevedel-staged--frozen session buffer batch :entries nil))
            (marker (test-mevedel-staged--artifact batch "session.meta.el")))
       ;; Deferred after the sidecar was captured, before stage-time projection.
       (mevedel-session-checkpoint-defer-ptc
        session "/root" (list (list :id "deferred-ptc-row")))
       (mevedel-session-publication--project-staged-sidecar session marker nil)
       (let* ((result (mevedel-session-artifacts--select-staged-save
                       session batch frozen))
              (disk (mevedel-session-publication--read-staged-sidecar
                     (plist-get (test-mevedel-staged--artifact result "session.meta.el")
                                :source))))
         (should (member "deferred-ptc-row"
                         (mapcar (lambda (row) (plist-get row :id))
                                 (plist-get disk :ptc-checkpoints))))
         (should (equal (plist-get frozen :stamp) (plist-get disk :updated-at))))))))

(provide 'test-mevedel-session-artifacts-staged)

;;; test-mevedel-session-artifacts-staged.el ends here
