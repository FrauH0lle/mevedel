;;; test-mevedel-session-control-fs.el --- Pinned session control filesystem -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers target-side descriptor pinning and no-follow control operations.

;;; Code:

(require 'mevedel-session-control-fs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-session-control-fs-operations
  (:doc "round trips UTF-8 content and distinguishes creation conflicts")
  (let* ((root (make-temp-file "mevedel-control-fs-" t))
         (path (file-name-concat root "lease")))
    (unwind-protect
        (progn
          (should (numberp
                   (mevedel-session-control-fs-target-time root)))
          (should
           (mevedel-session-control-fs-create-file path "ä/界"))
          (should-not
           (mevedel-session-control-fs-create-file path "replacement"))
          (should (equal "ä/界"
                         (mevedel-session-control-fs-read-file path)))
          (mevedel-session-control-fs-write-file path "replacement")
          (should (equal "replacement"
                         (mevedel-session-control-fs-read-file path)))
          (should (equal (list path)
                         (mevedel-session-control-fs-list-directory
                          root "\\`lease\\'")))
          (mevedel-session-control-fs-delete-file path)
          (should-not (file-exists-p path))
          (let ((binary (unibyte-string 0 127 128 255))
                (binary-path (file-name-concat root "binary")))
            (should
             (mevedel-session-control-fs-create-file
              binary-path binary 'no-conversion))
            (should
             (equal binary
                    (mevedel-session-control-fs-read-file
                     binary-path 'no-conversion))))
          ;; A newline in a name must not present itself as two entries,
          ;; and neither a write nor an exclusive create may land inside a
          ;; directory that occupies the name.
          (let* ((tricky (file-name-concat root "odd\n00000000000000000001.el"))
                 (occupied (file-name-concat root "occupied")))
            (write-region "x" nil tricky nil 'silent)
            (should (equal (list tricky)
                           (mevedel-session-control-fs-list-directory
                            root "\\`odd")))
            (should-not (mevedel-session-control-fs-list-directory
                         root "\\`0+1\\.el\\'"))
            (delete-file tricky)
            (make-directory occupied)
            (should-error
             (mevedel-session-control-fs-write-file occupied "replacement"))
            (should-not
             (mevedel-session-control-fs-create-file occupied "created"))
            (should (file-directory-p occupied))
            (should-not (directory-files occupied nil "\\`[^.]" t))
            (delete-directory occupied t))
          ;; A missing parent is the absent condition for every operation,
          ;; not a working-directory failure.
          (let ((orphan (file-name-concat root "gone" "record.el")))
            (should-not (mevedel-session-control-fs-path-exists-p orphan))
            (should-not (mevedel-session-control-fs-directory-p orphan))
            (should-not (mevedel-session-control-fs-list-directory
                         (file-name-concat root "gone") ".*"))
            (should-error (mevedel-session-control-fs-read-file orphan)
                          :type 'mevedel-session-control-fs-absent))
          ;; Missing parents are created one pinned component at a time.
          (let ((nested (file-name-concat root "a" "b" "c")))
            (should-error
             (mevedel-session-control-fs-make-directory nested)
             :type 'mevedel-session-control-fs-absent)
            (should (mevedel-session-control-fs-make-directory nested t))
            (should (mevedel-session-control-fs-directory-p nested))
            (should-not (mevedel-session-control-fs-make-directory nested t)))
          ;; Multi-kilobyte content must round trip byte for byte through the
          ;; staged payload rather than through a command line.
          (let* ((large-path (file-name-concat root "large"))
                 (large (apply #'unibyte-string
                               (mapcar (lambda (i) (% i 256))
                                       (number-sequence
                                        1 (* 64 1024))))))
            (should
             (mevedel-session-control-fs-create-file
              large-path large 'no-conversion))
            (should
             (equal large
                    (mevedel-session-control-fs-read-file
                     large-path 'no-conversion))))
          ;; No control path may resolve through a link: neither a linked
          ;; parent component nor a linked final name.
          (let* ((target (file-name-concat root "target"))
                 (linked-parent (file-name-concat root "linked-dir"))
                 (linked-leaf (file-name-concat root "linked-leaf")))
            (make-directory target)
            (mevedel-session-control-fs-create-file
             (file-name-concat target "record") "inside")
            (make-symbolic-link "target" linked-parent)
            (make-symbolic-link "target/record" linked-leaf)
            (should-error
             (mevedel-session-control-fs-read-file
              (file-name-concat linked-parent "record")))
            (should-error
             (mevedel-session-control-fs-read-file linked-leaf))
            (should-error
             (mevedel-session-control-fs-write-file linked-leaf "replaced"))
            (should (equal "inside"
                           (mevedel-session-control-fs-read-file
                            (file-name-concat target "record"))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-control-fs--programs
  (:doc "resolves the target interpreters once and retries after a failure")
  (let ((root (make-temp-file "mevedel-control-fs-programs-" t))
        (real (symbol-function 'executable-find))
        (lookups 0))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &optional remote)
                     (cl-incf lookups)
                     (funcall real name remote))))
          (clrhash mevedel-session-control-fs--programs)
          (mevedel-session-control-fs-target-time root)
          (should (= 2 lookups))
          ;; Locating them costs one target round trip per `exec-path'
          ;; entry, so every later operation reuses the resolved pair.
          (mevedel-session-control-fs-target-time root)
          (mevedel-session-control-fs-path-exists-p
           (file-name-concat root "absent"))
          (should (= 2 lookups))
          ;; An absent name is a normal answer and keeps the pair.
          (should-error
           (mevedel-session-control-fs-read-file
            (file-name-concat root "absent")))
          (should (= 2 lookups))
          ;; A refused operation may mean the interpreter moved, so the next
          ;; operation resolves again.
          (let ((link (file-name-concat root "link")))
            (make-symbolic-link "absent" link)
            (should-error (mevedel-session-control-fs-read-file link)))
          (mevedel-session-control-fs-target-time root)
          (should (= 4 lookups)))
      (clrhash mevedel-session-control-fs--programs)
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-control-fs-run-program
  ()
  ,test
  (test)

  :doc "runs every operation of a program in one target process"
  (let* ((root (make-temp-file "mevedel-control-fs-program-" t))
         (alpha (file-name-concat root "alpha"))
         (beta (file-name-concat root "beta"))
         (sub (file-name-concat root "sub"))
         (calls 0)
         results)
    (unwind-protect
        (progn
          (setq results
                (cl-letf* ((original (symbol-function 'process-file))
                           ((symbol-function 'process-file)
                            (lambda (&rest args)
                              (setq calls (1+ calls))
                              (apply original args))))
                  (mevedel-session-control-fs-run-program
                   (list (list :op 'make-directory :path sub)
                         (list :op 'create :path alpha :content "ä/界")
                         (list :op 'read :path alpha)
                         (list :op 'list-directory :path root)
                         (list :op 'target-time :path root)))))
          ;; The whole program is one target process; that is the point.
          (should (= 1 calls))
          (should (equal '(ok ok ok ok ok)
                         (mapcar (lambda (r) (plist-get r :status)) results)))
          (should (equal "ä/界" (plist-get (nth 2 results) :value)))
          (should (equal '("alpha" "sub")
                         (sort (plist-get (nth 3 results) :value) #'string<)))
          (should (integerp (plist-get (nth 4 results) :value)))
          (should-not (file-exists-p beta)))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "carries arbitrary bytes and names a shell cannot pass literally"
  (let* ((root (make-temp-file "mevedel-control-fs-program-" t))
         (bytes (apply #'unibyte-string (number-sequence 0 255)))
         (binary (file-name-concat root "binary"))
         (odd (file-name-concat root "odd\nname"))
         results)
    (unwind-protect
        (progn
          (setq results
                (mevedel-session-control-fs-run-program
                 (list (list :op 'write :path binary
                             :content bytes :coding 'no-conversion)
                       (list :op 'create :path odd :content "x")
                       (list :op 'read :path binary :coding 'no-conversion)
                       (list :op 'list-directory :path root))))
          (should (equal '(ok ok ok ok)
                         (mapcar (lambda (r) (plist-get r :status)) results)))
          ;; A NUL byte survives the framing in both directions.
          (should (equal bytes (plist-get (nth 2 results) :value)))
          ;; A newline in a name must arrive as one entry, not two.
          (should (equal '("binary" "odd\nname")
                         (sort (plist-get (nth 3 results) :value) #'string<))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "delivers a request as arguments, falling back to a file when oversized"
  ;; Arguments cost nothing; a stdin file costs TRAMP a remote temporary and a
  ;; copy into it on every program.  Either way it stays one target process.
  (let* ((root (make-temp-file "mevedel-control-fs-program-" t))
         (small (file-name-concat root "small"))
         (large (file-name-concat root "large"))
         (bulk (make-string (* 4 1024) ?x))
         calls)
    (unwind-protect
        (cl-letf* ((original (symbol-function 'process-file))
                   ((symbol-function 'process-file)
                    (lambda (&rest args)
                      (push args calls)
                      (apply original args))))
          (should (equal '(ok)
                         (mapcar (lambda (r) (plist-get r :status))
                                 (mevedel-session-control-fs-run-program
                                  (list (list :op 'write :path small
                                              :content "tiny"))))))
          (should (= 1 (length calls)))
          ;; No stdin file exists at all: the fields rode the command line,
          ;; and they arrive there in the order the script reads them.
          (should-not (nth 1 (car calls)))
          (let ((fields (last (car calls) 6)))
            (should (equal "write" (nth 0 fields)))
            (should (equal (file-name-as-directory (nth 2 fields))
                           (nth 1 fields)))
            (should (equal "small" (nth 3 fields)))
            (should (equal (base64-encode-string "tiny" t) (nth 4 fields)))
            (should (equal "0" (nth 5 fields))))

          (setq calls nil)
          (should (equal '(ok ok)
                         (mapcar (lambda (r) (plist-get r :status))
                                 (mevedel-session-control-fs-run-program
                                  (list (list :op 'write :path large
                                              :content bulk)
                                        (list :op 'read :path large))))))
          ;; An oversized request moves to the file; it does not become a
          ;; second call.
          (should (= 1 (length calls)))
          (should (stringp (nth 1 (car calls))))
          (should (equal bulk
                         (mevedel-session-control-fs-read-file large))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "stops at the first operation that does not succeed"
  (let* ((root (make-temp-file "mevedel-control-fs-program-" t))
         (alpha (file-name-concat root "alpha"))
         (beta (file-name-concat root "beta"))
         results)
    (unwind-protect
        (progn
          (should (mevedel-session-control-fs-create-file alpha "first"))
          (setq results
                (mevedel-session-control-fs-run-program
                 (list (list :op 'create :path alpha :content "second")
                       (list :op 'write :path beta :content "unreached"))))
          (should (equal 'conflict (plist-get (nth 0 results) :status)))
          (should (equal 'skipped (plist-get (nth 1 results) :status)))
          ;; A stopped program performs none of its remaining writes.
          (should-not (file-exists-p beta))
          (should (equal "first"
                         (mevedel-session-control-fs-read-file alpha)))
          ;; Target diagnostics reach the caller instead of the framing.
          (should (stringp (plist-get (nth 0 results) :diagnostic))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "expresses a compare-and-set as a verify its writes depend on"
  (let* ((root (make-temp-file "mevedel-control-fs-program-" t))
         (record (file-name-concat root "record"))
         (next (file-name-concat root "next")))
    (unwind-protect
        (progn
          (should (mevedel-session-control-fs-write-file record "generation-1"))
          ;; The expected bytes are present, so the dependent writes run.
          (let ((results
                 (mevedel-session-control-fs-run-program
                  (list (list :op 'verify :path record :content "generation-1")
                        (list :op 'write :path record :content "generation-2")
                        (list :op 'create :path next :content "claimed")))))
            (should (equal '(ok ok ok)
                           (mapcar (lambda (r) (plist-get r :status))
                                   results))))
          (should (equal "generation-2"
                         (mevedel-session-control-fs-read-file record)))
          ;; A foreign writer moved the record, so nothing after the proof runs.
          (let ((results
                 (mevedel-session-control-fs-run-program
                  (list (list :op 'verify :path record :content "generation-1")
                        (list :op 'write :path record :content "generation-3")))))
            (should (equal 'mismatch (plist-get (nth 0 results) :status)))
            (should (equal 'skipped (plist-get (nth 1 results) :status))))
          (should (equal "generation-2"
                         (mevedel-session-control-fs-read-file record)))
          ;; An absent record is its own answer, not a silent mismatch.
          (let ((results
                 (mevedel-session-control-fs-run-program
                  (list (list :op 'verify
                              :path (file-name-concat root "missing")
                              :content "anything")))))
            (should (equal 'absent (plist-get (nth 0 results) :status)))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "refuses a linked parent component and a linked final name"
  (let* ((root (make-temp-file "mevedel-control-fs-program-" t))
         (target (file-name-concat root "target"))
         (linked-dir (file-name-concat root "linked-dir"))
         (linked-leaf (file-name-concat root "linked-leaf")))
    (unwind-protect
        (progn
          (make-directory target)
          (should (mevedel-session-control-fs-create-file
                   (file-name-concat target "record") "inside"))
          (make-symbolic-link target linked-dir)
          (make-symbolic-link (file-name-concat target "record") linked-leaf)
          ;; A linked parent component fails the descriptor proof, and a
          ;; linked final name is refused.  Both are reported per operation;
          ;; `mevedel-session-control-fs-program-value' is what raises them.
          (let ((results
                 (mevedel-session-control-fs-run-program
                  (list (list :op 'read
                              :path (file-name-concat linked-dir "record"))))))
            (should (equal 'failed (plist-get (nth 0 results) :status)))
            (should-error
             (mevedel-session-control-fs-program-value (nth 0 results))
             :type 'file-error))
          (let ((results
                 (mevedel-session-control-fs-run-program
                  (list (list :op 'read :path linked-leaf)))))
            (should (equal 'failed (plist-get (nth 0 results) :status)))
            (should-error
             (mevedel-session-control-fs-program-value (nth 0 results))
             :type 'file-error))
          ;; The legitimate path still reads.
          (should (equal "inside"
                         (plist-get
                          (nth 0 (mevedel-session-control-fs-run-program
                                  (list (list :op 'read
                                              :path (file-name-concat
                                                     target "record")))))
                          :value))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-control-fs-program-parent-swap
  (:doc "keeps a program's write in the opened directory when its pathname is swapped")
  (let* ((root (make-temp-file "mevedel-control-fs-root-" t))
         (outside (make-temp-file "mevedel-control-fs-outside-" t))
         (moved (concat root ".moved"))
         (path (file-name-concat root "lease"))
         (pause ".mevedel-test-pause")
         (worker-buffer (generate-new-buffer " *mevedel-control-fs-worker*"))
         worker)
    (unwind-protect
        (progn
          (setq worker
                (start-process
                 "mevedel-control-fs-worker" worker-buffer
                 (or invocation-name "emacs")
                 "-Q" "--batch"
                 "--eval"
                 (format
                  "(progn (load %S nil t) (let ((mevedel-session-control-fs--test-pause-file %S)) (mevedel-session-control-fs-run-program (list (list :op 'write :path %S :content \"pinned\")))))"
                  (expand-file-name "mevedel-session-control-fs.el"
                                    default-directory)
                  pause path)))
          (while (and (process-live-p worker)
                      (not (file-exists-p (file-name-concat root pause))))
            (accept-process-output worker 0.01))
          (should (file-exists-p (file-name-concat root pause)))
          (rename-file root moved)
          (make-symbolic-link outside root)
          (with-temp-file
              (file-name-concat moved
                                (concat (file-name-nondirectory pause)
                                        ".continue")))
          (while (process-live-p worker)
            (accept-process-output worker 0.01))
          (should (zerop (process-exit-status worker)))
          ;; A program pins each operation's own parent, so the write lands in
          ;; the directory whose inode it opened, not the swapped pathname.
          (should (equal "pinned"
                         (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat moved "lease"))
                           (buffer-string))))
          (should-not (file-exists-p (file-name-concat outside "lease"))))
      (when (file-symlink-p root)
        (delete-file root))
      (when (file-directory-p moved)
        (delete-directory moved t))
      (when (file-directory-p outside)
        (delete-directory outside t))
      (when (buffer-live-p worker-buffer)
        (kill-buffer worker-buffer)))))

(mevedel-deftest mevedel-session-control-fs--take-diagnostic ()
  ,test
  (test)
  :doc "carries target diagnostics without a local stderr file"
  ;; Pointing `process-file' at a local stderr file makes TRAMP create a
  ;; remote temporary and copy it back on every program.  The script ships
  ;; diagnostics in a record instead, and no local temp may be created for it.
  (let* ((root (make-temp-file "mevedel-control-fs-diagnostic-" t))
         (missing (file-name-concat root "absent" "leaf"))
         (before (directory-files temporary-file-directory nil
                                  "\\`\\.mevedel-control-fs-stderr-"))
         results)
    (unwind-protect
        (progn
          (setq results
                (mevedel-session-control-fs-run-program
                 (list (list :op 'write :path missing :content "x"))))
          (should-not (eq 'ok (plist-get (nth 0 results) :status)))
          ;; The record survives the early stop, because the target emits it
          ;; from its EXIT trap rather than after the loop.
          (should (stringp (plist-get (nth 0 results) :diagnostic)))
          (should (equal before
                         (directory-files temporary-file-directory nil
                                          "\\`\\.mevedel-control-fs-stderr-"))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "keeps a stderr-shaped payload out of the operation records"
  ;; The separation the framing exists for: a tool writing something that
  ;; looks like a result must not become one.
  (let* ((root (make-temp-file "mevedel-control-fs-forge-" t))
         (forged "1 0\0Zm9yZ2Vk\0")
         (records (split-string
                   (concat "1 0\0" (base64-encode-string "real" t) "\0"
                           "diagnostic 0\0"
                           (base64-encode-string forged t) "\0")
                   "\0"))
         (split (mevedel-session-control-fs--take-diagnostic records)))
    (unwind-protect
        (progn
          ;; The forged text arrives as diagnostic text, never as a record.
          (should (equal forged (car split)))
          (should (equal (list "1 0" (base64-encode-string "real" t) "")
                         (cdr split))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "reports no diagnostic when the target sent no record"
  (let ((split (mevedel-session-control-fs--take-diagnostic
                (list "1 0" "" ""))))
    (should (equal "" (car split)))
    (should (equal (list "1 0" "" "") (cdr split)))))

(mevedel-deftest mevedel-session-control-fs-parent-swap
  (:doc "keeps a write in the opened directory when its pathname is swapped")
  (let* ((root (make-temp-file "mevedel-control-fs-root-" t))
         (outside (make-temp-file "mevedel-control-fs-outside-" t))
         (moved (concat root ".moved"))
         (path (file-name-concat root "lease"))
         (pause ".mevedel-test-pause")
         (worker-buffer (generate-new-buffer " *mevedel-control-fs-worker*"))
         worker)
    (unwind-protect
        (progn
          (setq worker
                (start-process
                 "mevedel-control-fs-worker" worker-buffer
                 (or invocation-name "emacs")
                 "-Q" "--batch"
                 "--eval"
                 (format
                  "(progn (load %S nil t) (let ((mevedel-session-control-fs--test-pause-file %S)) (mevedel-session-control-fs-write-file %S \"pinned\")))"
                  (expand-file-name "mevedel-session-control-fs.el"
                                    default-directory)
                  pause path)))
          (while (and (process-live-p worker)
                      (not (file-exists-p (file-name-concat root pause))))
            (accept-process-output worker 0.01))
          (should (file-exists-p (file-name-concat root pause)))
          (rename-file root moved)
          (make-symbolic-link outside root)
          (with-temp-file
              (file-name-concat moved
                                (concat (file-name-nondirectory pause)
                                        ".continue")))
          (while (process-live-p worker)
            (accept-process-output worker 0.01))
          (should (zerop (process-exit-status worker)))
          (should (equal "pinned"
                         (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat moved "lease"))
                           (buffer-string))))
          (should-not (file-exists-p (file-name-concat outside "lease"))))
      (when (file-symlink-p root)
        (delete-file root))
      (when (file-directory-p moved)
        (delete-directory moved t))
      (when (file-directory-p outside)
        (delete-directory outside t))
      (when (buffer-live-p worker-buffer)
        (kill-buffer worker-buffer)))))

(provide 'test-mevedel-session-control-fs)
;;; test-mevedel-session-control-fs.el ends here
