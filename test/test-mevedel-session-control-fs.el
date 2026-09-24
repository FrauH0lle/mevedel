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

(mevedel-deftest mevedel-session-control-fs--write-program-request ()
  ,test
  (test)
  :doc "writes each field NUL-terminated, copying no payload"
  (let* ((file (make-temp-file "mevedel-control-request-"))
         (payload (make-string (* 1024 1024) ?x))
         (fields (list (list "write" "/tmp" "first" payload "0")
                       (list "read" "/tmp" "second" "" "1"))))
    (unwind-protect
        (let* ((before (nth 4 (memory-use-counts)))
               (_ (mevedel-session-control-fs--write-program-request fields file))
               (allocated (- (nth 4 (memory-use-counts)) before)))
          (should (< allocated 4096))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            (should (equal (mapconcat #'identity
                                      (list "write" "/tmp" "first" "0"
                                            "1048576" payload
                                            "read" "/tmp" "second" "1" "0" "" "")
                                      "\0")
                           (buffer-string))))
          (should (equal payload (nth 3 (car fields)))))
      (delete-file file)))

  :doc "writes non-ASCII and raw-byte fields as their UTF-8 bytes"
  (let* ((file (make-temp-file "mevedel-control-request-"))
         (parent (concat "/tmp/\u00fc \u03bb \U0001F600"
                         (string (unibyte-char-to-multibyte 200))))
         (fields (list (list "read" parent "leaf" "" "0"))))
    (unwind-protect
        (progn
          (mevedel-session-control-fs--write-program-request fields file)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            (should (equal (encode-coding-string
                            (concat "read\0" parent "\0leaf\0" "0\0" "0\0" "\0")
                            'utf-8-unix)
                           (buffer-string)))))
      (delete-file file)))

  :doc "writes an empty request for no fields"
  (let ((file (make-temp-file "mevedel-control-request-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "stale"))
          (mevedel-session-control-fs--write-program-request nil file)
          (should (= 0 (file-attribute-size (file-attributes file)))))
      (delete-file file))))

(mevedel-deftest mevedel-session-control-fs-paths-exist ()
  (let* ((root (make-temp-file "mevedel-control-exist-" t))
         (present (file-name-concat root "present"))
         (linked (file-name-concat root "linked"))
         (original (symbol-function 'mevedel-session-control-fs-run-program))
         (calls 0))
    (unwind-protect
        (progn
          (write-region "" nil present nil 'silent)
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (&rest args)
                       (cl-incf calls)
                       (apply original args))))
            (should-not (mevedel-session-control-fs-paths-exist nil))
            (should (= calls 0))
            (should (equal '(t nil nil)
                           (mevedel-session-control-fs-paths-exist
                            (list present
                                  (file-name-concat root "absent")
                                  (file-name-concat root "gone" "leaf")))))
            (should (= calls 1)))
          ;; A symbolic link is refused, as for a single existence test.
          (make-symbolic-link "present" linked)
          (should-error (mevedel-session-control-fs-paths-exist (list linked))
                        :type 'file-error))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-control-fs-run-program-async ()
  ,test
  (test)
  :doc "runs a program without waiting and delivers its results"
  (let* ((root (make-temp-file "mevedel-control-async-" t))
         (path (file-name-concat root "log"))
         (mevedel-session-control-fs--pipe-local t)
         settled)
    (unwind-protect
        (progn
          (should (processp
                   (mevedel-session-control-fs-run-program-async
                    (list (list :op 'append :path path :content "one\n")
                          (list :op 'read :path path))
                    (lambda (results error) (setq settled (list results error))))))
          (should-not settled)
          (with-timeout (10 (ert-fail "Asynchronous program never settled"))
            (while (not settled) (accept-process-output nil 0.02)))
          (should-not (nth 1 settled))
          (should (equal '(ok ok) (mapcar (lambda (r) (plist-get r :status))
                                          (car settled))))
          (should (equal "one\n" (plist-get (nth 1 (car settled)) :value))))
      (delete-directory root t)))

  :doc "runs synchronously where the editor is about to exit"
  (let* ((root (make-temp-file "mevedel-control-async-" t))
         (mevedel-session-control-fs--pipe-local t)
         (mevedel-session-control-fs--async-wait t)
         settled)
    (unwind-protect
        (progn
          (should-not (mevedel-session-control-fs-run-program-async
                       (list (list :op 'path-exists-p :path root))
                       (lambda (results error) (setq settled (list results error)))))
          (should (equal 'ok (plist-get (car (car settled)) :status))))
      (delete-directory root t)))

  :doc "reports a program that failed as a whole"
  (let ((mevedel-session-control-fs--pipe-local t)
        settled)
    (cl-letf (((symbol-function 'mevedel-session-control-fs--programs)
               (lambda (_) (cons "false" "stat"))))
      (mevedel-session-control-fs-run-program-async
       (list (list :op 'path-exists-p :path "/tmp"))
       (lambda (results error) (setq settled (list results error)))))
    (with-timeout (10 (ert-fail "Failed program never settled"))
      (while (not settled) (accept-process-output nil 0.02)))
    (should-not (car settled))
    (should (eq 'file-error (car (nth 1 settled)))))

  :doc "counts its dispatch as a remote operation, so work a filter starts defers"
  (let ((mevedel-session-control-fs--pipe-local t)
        busy settled)
    (cl-letf* ((original (symbol-function 'process-send-region))
               ((symbol-function 'process-send-region)
                (lambda (&rest args)
                  (setq busy (mevedel-transport-busy-p))
                  (apply original args))))
      (mevedel-session-control-fs-run-program-async
       (list (list :op 'path-exists-p :path "/tmp"))
       (lambda (results error) (setq settled (list results error)))))
    (should busy)
    (should-not (mevedel-transport-busy-p))
    (with-timeout (10 (ert-fail "Program never settled"))
      (while (not settled) (accept-process-output nil 0.02)))))

(mevedel-deftest mevedel-session-control-fs--program-arguments/large-field ()
  (let* ((field (make-string (1+ mevedel-session-control-fs--argument-field-budget) ?x))
         (quote (symbol-function 'shell-quote-argument))
         (quoted 0))
    (cl-letf (((symbol-function 'shell-quote-argument)
               (lambda (&rest args) (cl-incf quoted) (apply quote args))))
      (should-not (mevedel-session-control-fs--program-arguments (list (list field)))))
    (should (zerop quoted))))

(mevedel-deftest mevedel-session-control-fs-append-rotating ()
  (let* ((root (make-temp-file "mevedel-control-fs-" t))
         (path (file-name-concat root "diagnostic.el"))
         (archive (concat path ".1")))
    (unwind-protect
        (progn
          (mevedel-session-control-fs-append-rotating path "aa\n" 6)
          (mevedel-session-control-fs-append-rotating path "bb\n" 6)
          (should-not (file-exists-p archive))
          (mevedel-session-control-fs-append-rotating path "cc\n" 6)
          (should (equal "aa\nbb\n" (mevedel-session-control-fs-read-file archive)))
          (should (equal "cc\n" (mevedel-session-control-fs-read-file path)))
          (mevedel-session-control-fs-append-rotating path "ddd\n" 6)
          (should (equal "cc\n" (mevedel-session-control-fs-read-file archive)))
          (should-error (mevedel-session-control-fs-append-rotating path "too large\n" 6))
          (should (equal "ddd\n" (mevedel-session-control-fs-read-file path)))
          (mevedel-session-control-fs-write-file path "old\nold\nnew\n")
          (mevedel-session-control-fs-append-rotating path "ok\n" 6)
          (should (equal "new\n" (mevedel-session-control-fs-read-file archive)))
          (should (equal "ok\n" (mevedel-session-control-fs-read-file path)))
          (delete-file archive)
          (make-symbolic-link path archive)
          (should-error (mevedel-session-control-fs-append-rotating path "x\n" 6))
          (should (equal "ok\n" (mevedel-session-control-fs-read-file path)))
          (should-not (directory-files root nil "\\`.mevedel-control-fs-")))
      (delete-directory root t))))

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
          ;; Missing parents are created one pinned component at a time,
          ;; in one program after the attempt that found them missing.
          (let ((nested (file-name-concat root "a" "b" "c"))
                (programs 0))
            (should-error
             (mevedel-session-control-fs-make-directory nested)
             :type 'mevedel-session-control-fs-absent)
            (cl-letf* ((original (symbol-function
                                  'mevedel-session-control-fs-run-program))
                       ((symbol-function 'mevedel-session-control-fs-run-program)
                        (lambda (&rest args)
                          (cl-incf programs)
                          (apply original args))))
              (should (mevedel-session-control-fs-make-directory nested t)))
            (should (= 2 programs))
            (should (mevedel-session-control-fs-directory-p nested))
            (should-not (mevedel-session-control-fs-make-directory nested t))
            ;; A linked ancestor is refused, not created through.
            (make-symbolic-link "a" (file-name-concat root "linked-a"))
            (should-error
             (mevedel-session-control-fs-make-directory
              (file-name-concat root "linked-a" "x" "y") t)
             :type 'file-error)
            (should-not (file-exists-p (file-name-concat root "a" "x"))))
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

(mevedel-deftest mevedel-session-control-fs-read-file
  (:doc "bounds native reads by bytes before transferring data to Emacs")
  (let* ((root (make-temp-file "mevedel-control-prefix-" t))
         (path (file-name-concat root "notes"))
         (bytes (encode-coding-string "a\u754cb" 'utf-8-unix)))
    (unwind-protect
        (progn
          (mevedel-session-control-fs-create-file path bytes 'no-conversion)
          (should (equal (substring bytes 0 3)
                         (mevedel-session-control-fs-read-file path 'no-conversion 3)))
          (should (equal "" (mevedel-session-control-fs-read-file path nil 0)))
          (should (equal bytes (mevedel-session-control-fs-read-file path 'no-conversion 100)))
          (should-error (mevedel-session-control-fs-read-file path nil -1))
          (should-error (mevedel-session-control-fs-read-file path nil "4")))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-control-fs-append-file
  (:doc "appends deltas in order and refuses symlink leaves")
  (let* ((root (make-temp-file "mevedel-control-fs-append-" t))
         (path (file-name-concat root "stream.el")))
    (unwind-protect
        (progn
          ;; First append creates the file; later appends keep order and
          ;; multibyte content intact.
          (should (mevedel-session-control-fs-append-file path "one\n"))
          (should (mevedel-session-control-fs-append-file
                   path "zwei \u00e4\u754c\n"))
          (should (equal "one\nzwei \u00e4\u754c\n"
                         (mevedel-session-control-fs-read-file path)))
          ;; A symlink leaf is refused before any write.
          (let ((link (file-name-concat root "link")))
            (make-symbolic-link path link)
            (should-error
             (mevedel-session-control-fs-append-file link "x\n"))
            (should (equal "one\nzwei \u00e4\u754c\n"
                           (mevedel-session-control-fs-read-file path)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-control-fs--programs ()
  ,test
  (test)
  :doc "resolves the target interpreters once and retries after a failure"
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
          ;; A refused operation keeps the pair too: the program process
          ;; itself ran, so the interpreters demonstrably work.  Only a
          ;; program that fails as a whole retries the lookup.
          (let ((link (file-name-concat root "link")))
            (make-symbolic-link "absent" link)
            (should-error (mevedel-session-control-fs-read-file link)))
          (mevedel-session-control-fs-target-time root)
          (should (= 2 lookups)))
      (clrhash mevedel-session-control-fs--programs)
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "refuses a non-Linux local host with a named constraint"
  (let ((root (make-temp-file "mevedel-control-fs-darwin-" t)))
    (unwind-protect
        (let ((system-type 'darwin))
          (clrhash mevedel-session-control-fs--programs)
          (let ((err (should-error
                      (mevedel-session-control-fs-target-time root)
                      :type 'user-error)))
            (should (string-match-p "Linux host" (cadr err)))
            (should (string-match-p "darwin" (cadr err)))))
      (clrhash mevedel-session-control-fs--programs)
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-control-fs--program-results ()
  ,test
  (test)
  :doc "accepts streamed bytes only with their successful trailing status"
  (let* ((ops (list (list :op 'read :path "/tmp/read" :coding 'no-conversion)
                    (list :op 'write :path "/tmp/write")))
         (bytes (unibyte-string 0 128 255))
         (encoded (base64-encode-string bytes t))
         (results (mevedel-session-control-fs--program-results
                   ops (concat encoded "\0" "1 0\0\0" "2 0\0"))))
    (should (equal bytes (plist-get (car results) :value)))
    (should (eq 'ok (plist-get (cadr results) :status))))
  :doc "a partial failed read discards bytes and leaves later writes skipped"
  (let* ((ops (list (list :op 'read :path "/tmp/read")
                    (list :op 'write :path "/tmp/write")))
         (results (mevedel-session-control-fs--program-results
                   ops (concat "partial-base64\0" "1 67\0"))))
    (should (eq 'failed (plist-get (car results) :status)))
    (should-not (plist-get (car results) :value))
    (should (eq 'skipped (plist-get (cadr results) :status))))
  :doc "rejects absent completion status and misordered operation identities"
  (let ((ops (list (list :op 'read :path "/tmp/read"))))
    (dolist (output (list "eA==\0" (concat "eA==\0" "1 0")
                          (concat "eA==\0" "2 0\0")
                          (concat "eA==\0" "1 0\0extra\0" "2 0\0")))
      (should-error (mevedel-session-control-fs--program-results ops output)))))

(mevedel-deftest mevedel-session-control-fs--archive-results
  (:doc "decodes native binary members and long UTF-8 names while rejecting mismatched or corrupt archives")
  (let* ((root (make-temp-file "mevedel-control-fs-archive-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root (concat (make-string 120 ?a) "\u754c\nlast")))
         (payload (unibyte-string 0 1 127 128 255))
         (operations (list (list :op 'read :path first :coding 'no-conversion)
                           (list :op 'read :path second :coding 'no-conversion)))
         (original (symbol-function 'mevedel-session-control-fs--archive-results))
         archive)
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region payload nil first nil 'silent)
            (write-region "" nil second nil 'silent))
          (cl-letf (((symbol-function 'mevedel-session-control-fs--archive-results)
                     (lambda (ops bytes)
                       (setq archive bytes)
                       (funcall original ops bytes))))
                   (should (equal (list payload "")
                                  (mapcar (lambda (r) (plist-get r :value))
                                          (mevedel-session-control-fs-run-program operations)))))
          (should (stringp archive))
          (should (equal (list payload "")
                         (mapcar (lambda (r) (plist-get r :value))
                                 (mevedel-session-control-fs--archive-results operations archive))))
          (should-error (mevedel-session-control-fs--archive-results
                         (reverse operations) archive))
          (should-error (mevedel-session-control-fs--archive-results
                         (list (car operations)) archive))
          (should-error (mevedel-session-control-fs--archive-results
                         (append operations (list (car operations))) archive))
          (should-error (mevedel-session-control-fs--archive-results
                         operations (substring archive 0 513)))
          (let ((corrupt (copy-sequence archive)))
            (aset corrupt 100 (if (= (aref corrupt 100) ?0) ?1 ?0))
            (should-error (mevedel-session-control-fs--archive-results operations corrupt))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-control-fs-run-program
  ()
  ,test
  (test)

  :doc "batches independent binary reads in one archive without per-file encoding processes"
  (let* ((root (make-temp-file "mevedel-control-fs-bulk-" t))
         (bin (file-name-concat root "bin"))
         (marker (file-name-concat root "tar-used"))
         (tar (executable-find "tar"))
         (payload (encode-coding-string "Binary\0evidence\n" 'utf-8-unix))
         (calls 0) paths)
    (skip-unless tar)
    (unwind-protect
        (progn
          (make-directory bin)
          (with-temp-file (file-name-concat bin "tar")
            (insert "#!/bin/bash\n: >" (shell-quote-argument marker)
                    "\nexec " (shell-quote-argument tar) " \"$@\"\n"))
          (set-file-modes (file-name-concat bin "tar") #o700)
          (dotimes (i 3)
            (let ((path (file-name-concat root (number-to-string i) "session.org")))
              (make-directory (file-name-directory path) t)
              (let ((coding-system-for-write 'no-conversion))
                (write-region payload nil path nil 'silent))
              (push path paths)))
          (let ((process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
                (original (symbol-function 'process-file)))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (&rest args) (cl-incf calls) (apply original args))))
                     (let ((results (mevedel-session-control-fs-run-program
                                     (mapcar (lambda (path) (list :op 'read :path path :coding 'no-conversion)) paths))))
                       (should (equal '(ok ok ok) (mapcar (lambda (r) (plist-get r :status)) results)))
                       (dolist (result results) (should (equal payload (plist-get result :value)))))))
          (should (= 1 calls))
          (should (file-exists-p marker)))
      (delete-directory root t)))

  :doc "bulk reads ignore archive preferences and reject a leaf replaced after proof"
  (let* ((root (make-temp-file "mevedel-control-fs-bulk-race-" t))
         (bin (file-name-concat root "bin"))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (outside (file-name-concat root "outside"))
         (tar (executable-find "tar"))
         (original (symbol-function 'process-file))
         (calls 0))
    (skip-unless tar)
    (unwind-protect
        (progn
          (make-directory bin)
          (write-region "initial" nil first nil 'silent)
          (write-region "second" nil second nil 'silent)
          (write-region "OUTSIDE-CANARY" nil outside nil 'silent)
          ;; The wrapper runs after the target has proved both parents and
          ;; checked both leaves, immediately before the actual archive read.
          (with-temp-file (file-name-concat bin "tar")
            (insert "#!/bin/bash\nrm -- " (shell-quote-argument first)
                    "\nln -s -- " (shell-quote-argument outside) " "
                    (shell-quote-argument first) "\nexec "
                    (shell-quote-argument tar) " \"$@\"\n"))
          (set-file-modes (file-name-concat bin "tar") #o700)
          (let ((process-environment
                 (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                               "TAR_OPTIONS=--dereference --remove-files")
                         process-environment)))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (&rest args)
                         (cl-incf calls)
                         (apply original args))))
                     (let ((results
                            (mevedel-session-control-fs-run-program
                             (list (list :op 'read :path first :optional t)
                                   (list :op 'read :path second)))))
                       (should (equal '(failed ok)
                                      (mapcar (lambda (r) (plist-get r :status)) results)))
                       (should-not (plist-get (car results) :value))
                       (should (equal "second" (plist-get (cadr results) :value))))))
          (should (= calls 2))
          (should (file-symlink-p first))
          (should (file-exists-p second))
          (should (equal "OUTSIDE-CANARY"
                         (mevedel-session-control-fs-read-file outside))))
      (delete-directory root t)))

  :doc "unavailable bulk carrier retries ordinary reads with ordered failure semantics"
  (let* ((root (make-temp-file "mevedel-control-fs-bulk-fallback-" t))
         (bin (file-name-concat root "bin"))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (original (symbol-function 'process-file))
         (calls 0))
    (unwind-protect
        (progn
          (make-directory bin)
          (write-region "first" nil first nil 'silent)
          (write-region "second" nil second nil 'silent)
          (with-temp-file (file-name-concat bin "tar")
            (insert "#!/bin/bash\nexit 127\n"))
          (set-file-modes (file-name-concat bin "tar") #o700)
          (let ((process-environment
                 (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment)))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (&rest args)
                         (cl-incf calls)
                         (apply original args))))
                     (should (equal '("first" "second")
                                    (mapcar (lambda (r) (plist-get r :value))
                                            (mevedel-session-control-fs-run-program
                                             (list (list :op 'read :path first)
                                                   (list :op 'read :path second))))))
                     (should (= calls 2))
                     (delete-file first)
                     (should (equal '(absent skipped)
                                    (mapcar (lambda (r) (plist-get r :status))
                                            (mevedel-session-control-fs-run-program
                                             (list (list :op 'read :path first)
                                                   (list :op 'read :path second))))))
                     (should (equal '(absent ok)
                                    (mapcar (lambda (r) (plist-get r :status))
                                            (mevedel-session-control-fs-run-program
                                             (list (list :op 'read :path first :optional t)
                                                   (list :op 'read :path second)))))))))
      (delete-directory root t)))

  :doc "rejected bulk output is discarded and retried against fresh native bytes"
  (let* ((root (make-temp-file "mevedel-control-fs-bulk-fresh-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (original (symbol-function 'process-file))
         (calls 0))
    (unwind-protect
        (progn
          (write-region "initial" nil first nil 'silent)
          (write-region "second" nil second nil 'silent)
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest args)
                       (cl-incf calls)
                       (prog1 (apply original args)
                         (when (= calls 1)
                           (with-current-buffer (car (nth 2 args))
                             (should (string-prefix-p "archive 0\0" (buffer-string)))
                             (erase-buffer)
                             (insert "archive 0\0invalid-base64!\0"))
                           (write-region "updated" nil first nil 'silent))))))
                   (should (equal '("updated" "second")
                                  (mapcar (lambda (r) (plist-get r :value))
                                          (mevedel-session-control-fs-run-program
                                           (list (list :op 'read :path first)
                                                 (list :op 'read :path second)))))))
          (should (= calls 2)))
      (delete-directory root t)))

  :doc "a complete-looking stream from a failed carrier cannot supply stale success"
  (let* ((root (make-temp-file "mevedel-control-fs-stream-failure-" t))
         (bin (file-name-concat root "bin"))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (tar (executable-find "tar")))
    (skip-unless tar)
    (unwind-protect
        (progn
          (make-directory bin)
          (write-region "initial" nil first nil 'silent)
          (write-region "second" nil second nil 'silent)
          (with-temp-file (file-name-concat bin "tar")
            (insert "#!/bin/bash\n" (shell-quote-argument tar) " \"$@\"\n"
                    "printf updated >" (shell-quote-argument first) "\nexit 1\n"))
          (set-file-modes (file-name-concat bin "tar") #o700)
          (let ((process-environment
                 (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment)))
            (should (equal '("updated" "second")
                           (mapcar (lambda (r) (plist-get r :value))
                                   (mevedel-session-control-fs-run-program
                                    (list (list :op 'read :path first)
                                          (list :op 'read :path second))))))))
      (delete-directory root t)))

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
          ;; and they arrive there in the order the script reads them.  The
          ;; parent travels once, as the physical no-trailing-slash spelling
          ;; the script both opens and proves against `pwd -P'.
          (should-not (nth 1 (car calls)))
          (let ((fields (last (car calls) 5)))
            (should (equal "write" (nth 0 fields)))
            (should (equal (directory-file-name
                            (file-name-directory
                             (file-truename small)))
                           (nth 1 fields)))
            (should (equal "small" (nth 2 fields)))
            (should (equal (base64-encode-string "tiny" t) (nth 3 fields)))
            (should (equal "0" (nth 4 fields))))
          ;; The root parent keeps its only possible spelling.
          (should (equal "/" (nth 1 (mevedel-session-control-fs--program-fields
                                     (list :op 'path-exists-p
                                           :path "/mevedel-missing")))))

          (setq calls nil)
          (should (equal '(ok ok)
                         (mapcar (lambda (r) (plist-get r :status))
                                 (mevedel-session-control-fs-run-program
                                  (list (list :op 'write :path large
                                              :content bulk)
                                        (list :op 'read :path large))))))
          ;; A large payload still rides the command line: its base64 is
          ;; newline-wrapped, so no physical line outgrows the pty budget.
          (should (= 1 (length calls)))
          (should-not (nth 1 (car calls)))
          (should (equal bulk
                         (mevedel-session-control-fs-read-file large)))
          ;; A wrapped verify payload compares equal to the unwrapped
          ;; observation, and still proves a mismatch.
          (should (equal '(ok)
                         (mapcar (lambda (r) (plist-get r :status))
                                 (mevedel-session-control-fs-run-program
                                  (list (list :op 'verify :path large
                                              :content bulk))))))
          (should (eq 'mismatch
                      (plist-get
                       (car (mevedel-session-control-fs-run-program
                             (list (list :op 'verify :path large
                                         :content (concat bulk "y")))))
                       :status)))
          ;; Only a field past the kernel's one-argument ceiling moves the
          ;; request to the stdin file; it does not become a second call.
          (setq calls nil)
          (let ((huge (make-string (* 128 1024) ?z)))
            (should (equal '(ok)
                           (mapcar (lambda (r) (plist-get r :status))
                                   (mevedel-session-control-fs-run-program
                                    (list (list :op 'write :path large
                                                :content huge))))))
            (should (= 1 (length calls)))
            (should (stringp (nth 1 (car calls))))
            (should (equal huge
                           (mevedel-session-control-fs-read-file large)))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "keeps operation fields independent across large mixed stdin requests"
  (let* ((root (make-temp-file "mevedel-control-mixed-fields-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (binary (concat (make-string (* 128 1024) ?x) (unibyte-string 0 128 255)))
         (text "Unicode: λ\nsecond line\0"))
    (unwind-protect
        (let ((results (mevedel-session-control-fs-run-program
                        (list (list :op 'read :path first :optional t)
                              (list :op 'write :path first :content binary)
                              (list :op 'write :path second :content text)
                              (list :op 'verify :path first :content binary)
                              (list :op 'read :path second)
                              (list :op 'verify :path second :content "mismatch")
                              (list :op 'delete-file :path first)))))
          (should (equal '(absent ok ok ok ok mismatch skipped)
                         (mapcar (lambda (result) (plist-get result :status)) results)))
          (should (equal text (decode-coding-string (plist-get (nth 4 results) :value) 'utf-8-unix)))
          (should (equal binary (mevedel-session-control-fs-read-file first 'no-conversion))))
      (delete-directory root t)))

  :doc "streams an oversized request over a pipe where the target allows one"
  ;; The pipe carrier replaces the stdin file TRAMP would copy; the script
  ;; must also read a payload that arrives in pipe-sized pieces.
  (let* ((root (make-temp-file "mevedel-control-pipe-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (binary (concat (make-string (* 512 1024) ?x) (unibyte-string 0 128 255)))
         (text "Unicode: λ\nsecond line\0")
         (mevedel-session-control-fs--pipe-local t)
         (spawns 0))
    (unwind-protect
        (cl-letf* (((symbol-function 'process-file)
                    (lambda (&rest _) (error "Request file carrier used")))
                   (original (symbol-function 'make-process))
                   ((symbol-function 'make-process)
                    (lambda (&rest args)
                      (cl-incf spawns)
                      (apply original args))))
          (let ((results (mevedel-session-control-fs-run-program
                          (list (list :op 'write :path first :content binary)
                                (list :op 'write :path second :content text)
                                (list :op 'read :path second)
                                (list :op 'verify :path second :content "mismatch")
                                (list :op 'delete-file :path first)))))
            (should (equal '(ok ok ok mismatch skipped)
                           (mapcar (lambda (result) (plist-get result :status))
                                   results)))
            (should (equal text (decode-coding-string
                                 (plist-get (nth 2 results) :value) 'utf-8-unix)))
            (should (= 1 spawns)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally first)
            (should (equal binary (buffer-string)))))
      (delete-directory root t)))

  :doc "a truncated streamed field cannot replace or create a destination"
  (let* ((root (make-temp-file "mevedel-control-truncated-stream-" t))
         (path (file-name-concat root "target"))
         (process (symbol-function 'process-file)))
    (unwind-protect
        (dolist (operation '(write create))
          (when (eq operation 'write) (with-temp-file path (insert "original")))
          (cl-letf (((symbol-function 'process-file)
                     (lambda (program input &rest arguments)
                       (should input)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally input)
                         ;; Keep every encoded payload byte but remove its
                         ;; final framing byte, simulating a torn transfer.
                         (delete-region (1- (point-max)) (point-max))
                         (let ((coding-system-for-write 'no-conversion))
                           (write-region (point-min) (point-max) input nil 'silent)))
                       (apply process program input arguments))))
            (should (eq 'failed
                        (plist-get
                         (car (mevedel-session-control-fs-run-program
                               (list (list :op operation :path path
                                           :content (make-string (* 128 1024) ?x))))) :status))))
          (if (eq operation 'write)
              (progn
                (should (equal "original" (mevedel-session-control-fs-read-file path)))
                (delete-file path))
            (should-not (file-exists-p path)))
          (should-not (directory-files root nil "\\`\\.mevedel-control")))
      (delete-directory root t)))

  :doc "stdin framing preserves the batched read archive path"
  (let* ((root (make-temp-file "mevedel-control-stdin-archive-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (process (symbol-function 'process-file))
         (calls 0))
    (unwind-protect
        (progn
          (with-temp-file first (insert "one"))
          (with-temp-file second (insert "two"))
          (cl-letf (((symbol-function 'mevedel-session-control-fs--program-arguments) #'ignore)
                    ((symbol-function 'process-file)
                     (lambda (&rest args) (cl-incf calls) (apply process args))))
            (should (equal '("one" "two")
                           (mapcar (lambda (result) (plist-get result :value))
                                   (mevedel-session-control-fs-run-program
                                    (list (list :op 'read :path first)
                                          (list :op 'read :path second)))))))
          (should (= calls 1)))
      (delete-directory root t)))

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

  :doc "expresses a precondition as a verify its writes depend on"
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

  :doc "validates numeric mode and deadline guards before dependent writes"
  (let* ((root (make-temp-file "mevedel-control-fs-numeric-" t))
         (record (file-name-concat root "record"))
         (next (file-name-concat root "next")))
    (unwind-protect
        (progn
          (write-region "original" nil record nil 'silent)
          (set-file-modes record #o600)
          (dolist (case
                   (list (list 'verify-mode record "600" 'ok)
                         (list 'verify-mode record "644" 'mismatch)
                         (list 'verify-mode record "888" 'failed)
                         (list 'verify-mode record "600\n" 'failed)
                         (list 'before-time root
                               (number-to-string
                                (+ 3600 (mevedel-session-control-fs-target-time root)))
                               'ok)
                         (list 'before-time root "0" 'mismatch)
                         (list 'before-time root "-1" 'failed)
                         (list 'before-time root "1; true" 'failed)))
            (let ((results
                   (mevedel-session-control-fs-run-program
                    (list (list :op (nth 0 case) :path (nth 1 case)
                                :content (nth 2 case))
                          (list :op 'create :path next :content "guarded")))))
              (should (eq (nth 3 case) (plist-get (car results) :status)))
              (if (eq (nth 3 case) 'ok)
                  (progn
                    (should (equal "guarded" (mevedel-session-control-fs-read-file next)))
                    (delete-file next))
                (should (eq 'skipped (plist-get (cadr results) :status)))
                (should-not (file-exists-p next))))))
      (delete-directory root t)))

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

(mevedel-deftest mevedel-session-control-fs-delete-directories
  ()
  ,test
  (test)

  :doc "deletes every directory in one program, independently"
  (let* ((root (make-temp-file "mevedel-control-fs-batch-" t))
         (one (file-name-concat root "one"))
         (two (file-name-concat root "two"))
         (absent (file-name-concat root "absent"))
         (calls 0)
         results)
    (unwind-protect
        (progn
          (make-directory one)
          (with-temp-file (file-name-concat one "data") (insert "x"))
          (make-directory two)
          (setq results
                (cl-letf* ((original (symbol-function 'process-file))
                           ((symbol-function 'process-file)
                            (lambda (&rest args)
                              (setq calls (1+ calls))
                              (apply original args))))
                  (mevedel-session-control-fs-delete-directories
                   (list one absent two))))
          (should (= 1 calls))
          ;; Deletion is idempotent: an already-absent directory
          ;; reports ok and never stops the deletions after it.
          (should (equal '(ok ok ok)
                         (mapcar (lambda (r) (plist-get r :status)) results)))
          (should-not (file-exists-p one))
          (should-not (file-exists-p two)))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "an empty path list runs no program"
  (should-not (mevedel-session-control-fs-delete-directories nil)))

(mevedel-deftest mevedel-session-control-fs-listing-proof ()
  ,test
  (test)
  :doc "proves exactly the listed entries, dotfiles and an absent directory included"
  (let* ((root (make-temp-file "mevedel-control-fs-listing-" t))
         (directory (file-name-concat root "listed"))
         (status (lambda (entries)
                   (plist-get (car (mevedel-session-control-fs-run-program
                                    (list (mevedel-session-control-fs-listing-proof
                                           directory entries))))
                              :status))))
    (unwind-protect
        (progn
          (should (eq 'ok (funcall status nil)))
          (make-directory directory)
          (should (eq 'ok (funcall status nil)))
          (dolist (name '("b" ".hidden" "a"))
            (write-region "" nil (file-name-concat directory name) nil 'silent))
          (let ((entries (mevedel-session-control-fs-list-directory directory ".")))
            (should (eq 'ok (funcall status entries)))
            (should (eq 'mismatch (funcall status (cdr entries))))
            (delete-file (file-name-concat directory "a"))
            (should (eq 'mismatch (funcall status entries)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-control-fs-tree-sizes
  ()
  ,test
  (test)

  :doc "counts nested and hidden regular files in one target program"
  (let* ((root (make-temp-file "mevedel-control-fs-size-" t))
         (one (file-name-concat root "one"))
         (nested (file-name-concat one ".nested"))
         (two (file-name-concat root "two"))
         (calls 0)
         results)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory two)
          (with-temp-file (file-name-concat one "visible") (insert "abc"))
          (with-temp-file (file-name-concat one ".hidden") (insert "12345"))
          (with-temp-file (file-name-concat nested "data") (insert "1234567"))
          (with-temp-file (file-name-concat two "data") (insert "four"))
          (setq results
                (cl-letf* ((original (symbol-function 'process-file))
                           ((symbol-function 'process-file)
                            (lambda (&rest args)
                              (setq calls (1+ calls))
                              (apply original args))))
                  (mevedel-session-control-fs-tree-sizes (list one two))))
          (should (= 1 calls))
          (should (equal (list one two)
                         (mapcar (lambda (result)
                                   (plist-get result :path))
                                 results)))
          (should (equal '(ok ok)
                         (mapcar (lambda (result)
                                   (plist-get result :status))
                                 results)))
          (should (equal '(15 4)
                         (mapcar (lambda (result)
                                   (plist-get result :value))
                                 results))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "refuses symlink entries and roots without suppressing later sizes"
  (let* ((root (make-temp-file "mevedel-control-fs-size-links-" t))
         (linked-entry (file-name-concat root "linked-entry"))
         (linked-root (file-name-concat root "linked-root"))
         (good (file-name-concat root "good")))
    (unwind-protect
        (progn
          (make-directory linked-entry)
          (make-directory good)
          (with-temp-file (file-name-concat good "data") (insert "valid"))
          (make-symbolic-link (file-name-concat good "data")
                              (file-name-concat linked-entry "link"))
          (make-symbolic-link good linked-root)
          (let ((results
                 (mevedel-session-control-fs-tree-sizes
                  (list linked-entry linked-root good))))
            (should (equal '(failed failed ok)
                           (mapcar (lambda (result)
                                     (plist-get result :status))
                                   results)))
            (should (= 5 (plist-get (nth 2 results) :value)))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "treats a leading-dash directory name as data"
  (let* ((root (make-temp-file "mevedel-control-fs-size-dash-" t))
         (tree (file-name-concat root "-delete"))
         (sibling (file-name-concat root "sibling")))
    (unwind-protect
        (progn
          (make-directory tree)
          (with-temp-file (file-name-concat tree "data") (insert "safe"))
          (with-temp-file sibling (insert "untouched"))
          (let ((result (car (mevedel-session-control-fs-tree-sizes
                              (list tree)))))
            (should (eq 'ok (plist-get result :status)))
            (should (= 4 (plist-get result :value)))
            (should (file-exists-p sibling))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "does not follow a descendant replaced by a symlink"
  (let* ((root (make-temp-file "mevedel-control-fs-size-link-race-" t))
         (tree (file-name-concat root "tree"))
         (outside (file-name-concat root "outside"))
         (entry (file-name-concat tree "entry"))
         (pause ".mevedel-test-pause")
         (worker-buffer (generate-new-buffer " *mevedel-control-fs-size-worker*"))
         worker)
    (unwind-protect
        (progn
          (make-directory entry t)
          (make-directory outside)
          (with-temp-file (file-name-concat entry "small") (insert "x"))
          (with-temp-file (file-name-concat outside "large")
            (insert (make-string 1000 ?x)))
          (setq worker
                (start-process
                 "mevedel-control-fs-size-worker" worker-buffer
                 (or invocation-name "emacs")
                 "-Q" "--batch" "--eval"
                 (format
                  "(progn (load %S nil t) (let ((mevedel-session-control-fs--test-pause-file %S)) (prin1 (mevedel-session-control-fs-tree-sizes (list %S)))))"
                  (expand-file-name "mevedel-session-control-fs.el"
                                    default-directory)
                  pause tree)))
          (while (and (process-live-p worker)
                      (not (file-exists-p (file-name-concat root pause))))
            (accept-process-output worker 0.01))
          (should (file-exists-p (file-name-concat root pause)))
          (delete-directory entry t)
          (make-symbolic-link outside entry)
          (with-temp-file (file-name-concat root (concat pause ".continue")))
          (while (process-live-p worker)
            (accept-process-output worker 0.01))
          (should (zerop (process-exit-status worker)))
          (with-current-buffer worker-buffer
            (goto-char (point-min))
            (should (search-forward ":status failed" nil t))))
      (when (process-live-p worker)
        (delete-process worker))
      (when (buffer-live-p worker-buffer)
        (kill-buffer worker-buffer))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "an empty path list runs no program"
  (should-not (mevedel-session-control-fs-tree-sizes nil)))

(mevedel-deftest mevedel-session-control-fs--program-value
  (:doc "decodes only nonnegative integer tree byte counts")
  (let ((op (list :op 'tree-size :path "/tmp/tree")))
    (should (= 42 (mevedel-session-control-fs--program-value op "42")))
    (dolist (payload '("" "-1" "+1" "1.0" " 1" "1\n" "unknown"))
      (should-error (mevedel-session-control-fs--program-value op payload)
                    :type 'file-error))))

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

(mevedel-deftest mevedel-session-control-fs-read-leaf-swap
  (:doc "refuses a file replaced by a symlink after its parent is pinned")
  (let* ((root (make-temp-file "mevedel-control-fs-read-swap-" t))
         (outside (make-temp-file "mevedel-control-fs-read-outside-" nil))
         (path (file-name-concat root "segment.chat.org"))
         (pause ".mevedel-test-pause")
         (worker-buffer (generate-new-buffer " *mevedel-control-fs-read-worker*"))
         worker)
    (unwind-protect
        (progn
          (with-temp-file path (insert "inside"))
          (with-temp-file outside (insert "outside"))
          (setq worker
                (start-process
                 "mevedel-control-fs-read-worker" worker-buffer
                 (or invocation-name "emacs")
                 "-Q" "--batch" "--eval"
                 (format
                  "(progn (load %S nil t) (let ((mevedel-session-control-fs--test-pause-file %S)) (mevedel-session-control-fs-read-file %S 'no-conversion)))"
                  (expand-file-name "mevedel-session-control-fs.el"
                                    default-directory)
                  pause path)))
          (while (and (process-live-p worker)
                      (not (file-exists-p (file-name-concat root pause))))
            (accept-process-output worker 0.01))
          (should (file-exists-p (file-name-concat root pause)))
          (delete-file path)
          (make-symbolic-link outside path)
          (with-temp-file (file-name-concat root (concat pause ".continue")))
          (while (process-live-p worker)
            (accept-process-output worker 0.01))
          (should-not (zerop (process-exit-status worker))))
      (when (process-live-p worker)
        (delete-process worker))
      (when (buffer-live-p worker-buffer)
        (kill-buffer worker-buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-exists-p outside)
        (delete-file outside)))))

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
          ;; The enclosing diagnostic pipe survives the program's early stop.
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
                   (concat (base64-encode-string "real" t) "\0" "1 0\0"
                           "diagnostic 0\0"
                           (base64-encode-string forged t) "\0")
                   "\0"))
         (split (mevedel-session-control-fs--take-diagnostic records)))
    (unwind-protect
        (progn
          ;; The forged text arrives as diagnostic text, never as a record.
          (should (equal forged (car split)))
          (should (equal (list (base64-encode-string "real" t) "1 0" "")
                         (cdr split))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "transports binary target errors without executing a skipped write"
  (let* ((root (make-temp-file "mevedel-control-fs-stderr-" t))
         (process-environment (copy-sequence process-environment))
         (command (file-name-concat root "mv"))
         (path (file-name-concat root "record"))
         (later (file-name-concat root "later"))
         (forged "1 0\0Zm9yZ2Vk\0"))
    (unwind-protect
        (progn
          ;; A failing target utility can emit arbitrary bytes on stderr.
          ;; Exercise the real transport, including its encoding and pipe.
          (write-region "#!/bin/sh\nprintf '1 0\\000Zm9yZ2Vk\\000\\n' >&2\nexit 1\n"
                        nil command nil 'silent)
          (set-file-modes command #o700)
          (setenv "PATH" (concat root ":" (getenv "PATH")))
          (write-region "original" nil path nil 'silent)
          (let ((results
                 (mevedel-session-control-fs-run-program
                  (list (list :op 'write :path path :content "replacement")
                        (list :op 'create :path later :content "unreachable")))))
            (should (eq 'failed (plist-get (car results) :status)))
            (should (equal forged (plist-get (car results) :diagnostic)))
            (should (eq 'skipped (plist-get (cadr results) :status)))
            (should-not (file-exists-p later))
            (should (equal "original" (mevedel-session-control-fs-read-file path))))
          (should (equal '("mv" "record")
                         (directory-files root nil directory-files-no-dot-files-regexp))))
      (delete-directory root t)))

  :doc "reports no diagnostic when the target sent no record"
  (let ((split (mevedel-session-control-fs--take-diagnostic
                (list "" "1 0" ""))))
    (should (equal "" (car split)))
    (should (equal (list "" "1 0" "") (cdr split)))))

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

(mevedel-deftest mevedel-session-control-fs-create-or-verify
  (:doc "creates once, accepts an identical record, and rejects a different one")
  (let* ((root (make-temp-file "mevedel-control-fs-verify-" t))
         (path (file-name-concat root "marker")))
    (unwind-protect
        (progn
          (should (mevedel-session-control-fs-create-or-verify path "ä/界"))
          (should (mevedel-session-control-fs-create-or-verify path "ä/界"))
          (should-not (mevedel-session-control-fs-create-or-verify path "ä/界 changed"))
          (should-not (mevedel-session-control-fs-create-or-verify path "ä"))
          (should (equal "ä/界" (mevedel-session-control-fs-read-file path))))
      (delete-directory root t))))

(provide 'test-mevedel-session-control-fs)
;;; test-mevedel-session-control-fs.el ends here
