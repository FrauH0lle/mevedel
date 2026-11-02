;;; test-mevedel-memory-investigation-slices.el --- Cooperative searches -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise yielding, fresh source authority, and cancellation with real files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-investigation)
(require 'mevedel-memory-scope)
(require 'mevedel-execution)
(require 'mevedel-sandbox)

(mevedel-deftest mevedel-memory-investigation--search/slices ()
  ,test
  (test)
  :doc "later slices observe changed, deleted, and newly excluded source paths"
  (let* ((directory (make-temp-file "mevedel-search-slices-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (mevedel-sandbox-mode 'off)
         (original (symbol-function 'mevedel-memory-scope-source-path))
         state mutation result scheduled changed)
    (unwind-protect
        (progn
          (dotimes (n 24)
            (with-temp-file (file-name-concat directory (format "a%02d.txt" n))
              (insert "Ordinary needle.\n")))
          (make-directory (file-name-concat directory "memory"))
          (with-temp-file (file-name-concat directory "memory/secret.txt")
            (insert "PRIVATE needle.\n"))
          (dolist (file '("zchanged.txt" "zdeleted.txt" "zexcluded.txt"))
            (with-temp-file (file-name-concat directory file) (insert "Old needle.\n")))
          (setq state (mevedel-memory-investigation-create
                       (mevedel-memory-scope-capture workspace) nil (lambda () t) #'ignore))
          (cl-letf (((symbol-function 'mevedel-memory-scope-source-path)
                     (lambda (scope relative)
                       (when (and (equal relative "a00.txt") (not scheduled))
                         (setq scheduled t
                               mutation
                               (run-at-time
                                0 nil
                                (lambda ()
                                  (with-temp-file (file-name-concat directory "zchanged.txt")
                                    (insert "Updated needle.\n"))
                                  (delete-file (file-name-concat directory "zdeleted.txt"))
                                  (delete-file (file-name-concat directory "zexcluded.txt"))
                                  (make-symbolic-link "memory/secret.txt"
                                                      (file-name-concat directory "zexcluded.txt"))
                                  (setq changed t)))))
                       (when (string-prefix-p "z" relative) (should changed))
                       (funcall original scope relative))))
            (mevedel-memory-investigation-call
             state 'grep '(:root "workspace" :path "." :pattern "needle")
             (lambda (text) (setq result text)))
            ;; Preparation is asynchronous, not a whole-tree admission block.
            (should-not scheduled)
            (with-timeout (10 (ert-fail "Sliced search did not settle"))
              (while (not result) (accept-process-output nil 0.01))))
          (should changed)
          (should (string-prefix-p "[Partial source snapshot" result))
          (should (string-match-p "a00.txt\n1:Ordinary needle" result))
          (should (string-match-p "zchanged.txt\n1:Updated needle" result))
          (dolist (absent '("Old needle" "PRIVATE" "zdeleted" "zexcluded" "mevedel-memory-search-"))
            (should-not (string-search absent result)))
          (should-not (mevedel-memory-investigation-active state)))
      (when mutation (cancel-timer mutation))
      (when state (mevedel-memory-investigation-stop state))
      (mevedel-execution-teardown-all)
      (delete-directory directory t)))
  :doc "cancellation before, during, and between slices removes all copies without delivery"
  (dolist (mode '(before authority read between expired kill))
    (let* ((directory (make-temp-file "mevedel-search-cancel-slice-" t))
           (workspace (mevedel-workspace--create :root directory))
           (origin (generate-new-buffer " *investigation-slice-origin*"))
           (mevedel-memory-dirs nil)
           (mevedel-sandbox-mode 'off)
           (original (symbol-function 'mevedel-memory-scope--snapshot))
           (source-path (symbol-function 'mevedel-memory-scope-source-path))
           (before (directory-files temporary-file-directory t "\\`mevedel-memory-search-"))
           (live t) state cancellation intercepted admitted delivered)
      (unwind-protect
          (progn
            (dotimes (n 24)
              (with-temp-file (file-name-concat directory (format "a%02d.txt" n))
                (insert "Needle.\n")))
            (setq state (mevedel-memory-investigation-create
                         (mevedel-memory-scope-capture workspace) nil (lambda () live) #'ignore))
            (cl-letf (((symbol-function 'mevedel-memory-scope-source-path)
                       (lambda (scope relative)
                         (prog1 (funcall source-path scope relative)
                           (when (and (eq mode 'authority) (equal relative "a00.txt"))
                             (setq admitted t)
                             (mevedel-memory-investigation-stop state)))))
                      ((symbol-function 'mevedel-memory-scope--snapshot)
                       (lambda (&rest args)
                         (prog1 (apply original args)
                           (unless intercepted
                             (setq intercepted t)
                             (pcase mode
                               ('read (mevedel-memory-investigation-stop state))
                               ('kill (kill-buffer origin))
                               ('between
                                (setq cancellation
                                      (run-at-time 0 nil #'mevedel-memory-investigation-stop state)))
                               ('expired
                                (setq cancellation (run-at-time 0 nil (lambda () (setq live nil)))))))))))
              (with-current-buffer origin
                (mevedel-memory-investigation-call
                 state 'grep '(:root "workspace" :path "." :pattern "Needle")
                 (lambda (_) (setq delivered t))))
              (when (eq mode 'before) (mevedel-memory-investigation-stop state))
              (with-timeout (10 (ert-fail "Cancellation did not clean up"))
                (while (mevedel-memory-investigation-active state)
                  (accept-process-output nil 0.01))))
            (if (memq mode '(before authority))
                (should-not intercepted)
              (should intercepted))
            (when (eq mode 'authority) (should admitted))
            (accept-process-output nil 0.02)
            (should-not delivered)
            (should (eq 'cancelled (plist-get (car (plist-get (mevedel-memory-investigation-summary state) :calls)) :status)))
            (should (equal before (directory-files temporary-file-directory t "\\`mevedel-memory-search-"))))
        (when cancellation (cancel-timer cancellation))
        (when state (mevedel-memory-investigation-stop state))
        (when (buffer-live-p origin) (kill-buffer origin))
        (mevedel-execution-teardown-all)
        (delete-directory directory t)))))

(mevedel-deftest mevedel-memory-investigation--search/context ()
  ,test
  (test)
  :doc "deferred helpers stay sessionless and restore the caller buffer and request"
  (let* ((directory (make-temp-file "mevedel-search-context-" t))
         (workspace (mevedel-workspace--create :root directory))
         (origin (generate-new-buffer " *investigation-context*"))
         (request (mevedel-request--create :origin "/root"))
         (environment (cons "MEVEDEL_SEARCH_CONTEXT=caller" process-environment))
         (paths (cons "/private-caller-bin" exec-path))
         (original (symbol-function 'mevedel-execution-start-helper))
         (mevedel-memory-dirs nil)
         (mevedel-sandbox-mode 'off)
         state launched)
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "source.txt") (insert "Needle.\n"))
          (setq state (mevedel-memory-investigation-create
                       (mevedel-memory-scope-capture workspace) nil (lambda () t) #'ignore))
          (with-current-buffer origin
            (setq-local mevedel--session (mevedel-session--create)))
          (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                     (lambda (&rest args)
                       (should-not mevedel--session)
                       (should (eq request mevedel--current-request))
                       (should (equal paths exec-path))
                       (should (equal environment process-environment))
                       (setq launched t)
                       (apply original args))))
            (dolist (operation '(glob grep))
              (let (result)
                (with-current-buffer origin
                  (let ((default-directory (file-name-as-directory directory))
                        (exec-path paths)
                        (process-environment environment)
                        (mevedel--current-request request))
                    (mevedel-memory-investigation-call
                     state operation '(:root "workspace" :path "." :pattern "source")
                     (lambda (text)
                       (should (eq (current-buffer) origin))
                       (should (equal default-directory (file-name-as-directory directory)))
                       (should-not mevedel--session)
                       (should (eq request mevedel--current-request))
                       (should (equal paths exec-path))
                       (should (equal environment process-environment))
                       (setq result text)))))
                (with-timeout (10 (ert-fail "Context search did not settle"))
                  (while (not result) (accept-process-output nil 0.01)))
                (should launched)))))
      (when state (mevedel-memory-investigation-stop state))
      (when (buffer-live-p origin) (kill-buffer origin))
      (mevedel-execution-teardown-all)
      (delete-directory directory t)))
  :doc "cancellation during helper launch cancels the handle returned afterward"
  (let* ((directory (make-temp-file "mevedel-search-launch-cancel-" t))
         (workspace (mevedel-workspace--create :root directory))
         (original (symbol-function 'mevedel-execution-start-helper))
         (mevedel-memory-dirs nil)
         (mevedel-sandbox-mode 'off)
         state launched cancelled delivered)
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "source.txt") (insert "Needle.\n"))
          (setq state (mevedel-memory-investigation-create
                       (mevedel-memory-scope-capture workspace) nil (lambda () t) #'ignore))
          (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                     (lambda (&rest args)
                       (let ((cancel (apply original args)))
                         (setq launched t)
                         (mevedel-memory-investigation-stop state)
                         (lambda () (setq cancelled t) (funcall cancel))))))
            (mevedel-memory-investigation-call
             state 'glob '(:root "workspace" :path "." :pattern "*.txt")
             (lambda (_) (setq delivered t)))
            (with-timeout (10 (ert-fail "Search did not cancel at launch"))
              (while (mevedel-memory-investigation-active state)
                (accept-process-output nil 0.01))))
          (should launched)
          (should cancelled)
          (accept-process-output nil 0.02)
          (should-not delivered))
      (when state (mevedel-memory-investigation-stop state))
      (mevedel-execution-teardown-all)
      (delete-directory directory t))))

(provide 'test-mevedel-memory-investigation-slices)
;;; test-mevedel-memory-investigation-slices.el ends here
