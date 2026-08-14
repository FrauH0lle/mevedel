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
