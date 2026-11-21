;;; test-niwa-memory-investigation-slices.el --- Cooperative searches -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise yielding, fresh source authority, and cancellation with real files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-memory-investigation)
(require 'niwa-memory-scope)
(require 'niwa-execution)
(require 'niwa-sandbox)

(niwa-deftest niwa-memory-investigation--search/slices ()
  ,test
  (test)
  :doc "later slices observe changed, deleted, and newly excluded source paths"
  (let* ((directory (make-temp-file "niwa-search-slices-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs '("memory"))
         (niwa-sandbox-mode 'off)
         (original (symbol-function 'niwa-memory-scope-source-path))
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
          (setq state (niwa-memory-investigation-create
                       (niwa-memory-scope-capture workspace) nil (lambda () t) #'ignore))
          (cl-letf (((symbol-function 'niwa-memory-scope-source-path)
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
            (niwa-memory-investigation-call
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
          (dolist (absent '("Old needle" "PRIVATE" "zdeleted" "zexcluded" "niwa-memory-search-"))
            (should-not (string-search absent result)))
          (should-not (niwa-memory-investigation-active state)))
      (when mutation (cancel-timer mutation))
      (when state (niwa-memory-investigation-stop state))
      (niwa-execution-teardown-all)
      (delete-directory directory t)))
  :doc "cancellation before, during, and between slices removes all copies without delivery"
  (dolist (mode '(before authority read between expired kill))
    (let* ((directory (make-temp-file "niwa-search-cancel-slice-" t))
           (workspace (niwa-workspace--create :root directory))
           (origin (generate-new-buffer " *investigation-slice-origin*"))
           (niwa-memory-dirs nil)
           (niwa-sandbox-mode 'off)
           (original (symbol-function 'niwa-memory-scope--snapshot))
           (source-path (symbol-function 'niwa-memory-scope-source-path))
           (before (directory-files temporary-file-directory t "\\`niwa-memory-search-"))
           (live t) state cancellation intercepted admitted delivered)
      (unwind-protect
          (progn
            (dotimes (n 24)
              (with-temp-file (file-name-concat directory (format "a%02d.txt" n))
                (insert "Needle.\n")))
            (setq state (niwa-memory-investigation-create
                         (niwa-memory-scope-capture workspace) nil (lambda () live) #'ignore))
            (cl-letf (((symbol-function 'niwa-memory-scope-source-path)
                       (lambda (scope relative)
                         (prog1 (funcall source-path scope relative)
                           (when (and (eq mode 'authority) (equal relative "a00.txt"))
                             (setq admitted t)
                             (niwa-memory-investigation-stop state)))))
                      ((symbol-function 'niwa-memory-scope--snapshot)
                       (lambda (&rest args)
                         (prog1 (apply original args)
                           (unless intercepted
                             (setq intercepted t)
                             (pcase mode
                               ('read (niwa-memory-investigation-stop state))
                               ('kill (kill-buffer origin))
                               ('between
                                (setq cancellation
                                      (run-at-time 0 nil #'niwa-memory-investigation-stop state)))
                               ('expired
                                (setq cancellation (run-at-time 0 nil (lambda () (setq live nil)))))))))))
              (with-current-buffer origin
                (niwa-memory-investigation-call
                 state 'grep '(:root "workspace" :path "." :pattern "Needle")
                 (lambda (_) (setq delivered t))))
              (when (eq mode 'before) (niwa-memory-investigation-stop state))
              (with-timeout (10 (ert-fail "Cancellation did not clean up"))
                (while (niwa-memory-investigation-active state)
                  (accept-process-output nil 0.01))))
            (if (memq mode '(before authority))
                (should-not intercepted)
              (should intercepted))
            (when (eq mode 'authority) (should admitted))
            (accept-process-output nil 0.02)
            (should-not delivered)
            (should (eq 'cancelled (plist-get (car (plist-get (niwa-memory-investigation-summary state) :calls)) :status)))
            (should (equal before (directory-files temporary-file-directory t "\\`niwa-memory-search-"))))
        (when cancellation (cancel-timer cancellation))
        (when state (niwa-memory-investigation-stop state))
        (when (buffer-live-p origin) (kill-buffer origin))
        (niwa-execution-teardown-all)
        (delete-directory directory t)))))

(niwa-deftest niwa-memory-investigation--search/context ()
  ,test
  (test)
  :doc "deferred helpers stay sessionless and restore the caller buffer and request"
  (let* ((directory (make-temp-file "niwa-search-context-" t))
         (workspace (niwa-workspace--create :root directory))
         (origin (generate-new-buffer " *investigation-context*"))
         (request (niwa-request--create :origin "/root"))
         (environment (cons "NIWA_SEARCH_CONTEXT=caller" process-environment))
         (paths (cons "/private-caller-bin" exec-path))
         (original (symbol-function 'niwa-execution-start-helper))
         (niwa-memory-dirs nil)
         (niwa-sandbox-mode 'off)
         state launched)
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "source.txt") (insert "Needle.\n"))
          (setq state (niwa-memory-investigation-create
                       (niwa-memory-scope-capture workspace) nil (lambda () t) #'ignore))
          (with-current-buffer origin
            (setq-local niwa--session (niwa-session--create)))
          (cl-letf (((symbol-function 'niwa-execution-start-helper)
                     (lambda (&rest args)
                       (should-not niwa--session)
                       (should (eq request niwa--current-request))
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
                        (niwa--current-request request))
                    (niwa-memory-investigation-call
                     state operation '(:root "workspace" :path "." :pattern "source")
                     (lambda (text)
                       (should (eq (current-buffer) origin))
                       (should (equal default-directory (file-name-as-directory directory)))
                       (should-not niwa--session)
                       (should (eq request niwa--current-request))
                       (should (equal paths exec-path))
                       (should (equal environment process-environment))
                       (setq result text)))))
                (with-timeout (10 (ert-fail "Context search did not settle"))
                  (while (not result) (accept-process-output nil 0.01)))
                (should launched)))))
      (when state (niwa-memory-investigation-stop state))
      (when (buffer-live-p origin) (kill-buffer origin))
      (niwa-execution-teardown-all)
      (delete-directory directory t)))
  :doc "cancellation during helper launch cancels the handle returned afterward"
  (let* ((directory (make-temp-file "niwa-search-launch-cancel-" t))
         (workspace (niwa-workspace--create :root directory))
         (original (symbol-function 'niwa-execution-start-helper))
         (niwa-memory-dirs nil)
         (niwa-sandbox-mode 'off)
         state launched cancelled delivered)
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "source.txt") (insert "Needle.\n"))
          (setq state (niwa-memory-investigation-create
                       (niwa-memory-scope-capture workspace) nil (lambda () t) #'ignore))
          (cl-letf (((symbol-function 'niwa-execution-start-helper)
                     (lambda (&rest args)
                       (let ((cancel (apply original args)))
                         (setq launched t)
                         (niwa-memory-investigation-stop state)
                         (lambda () (setq cancelled t) (funcall cancel))))))
            (niwa-memory-investigation-call
             state 'glob '(:root "workspace" :path "." :pattern "*.txt")
             (lambda (_) (setq delivered t)))
            (with-timeout (10 (ert-fail "Search did not cancel at launch"))
              (while (niwa-memory-investigation-active state)
                (accept-process-output nil 0.01))))
          (should launched)
          (should cancelled)
          (accept-process-output nil 0.02)
          (should-not delivered))
      (when state (niwa-memory-investigation-stop state))
      (niwa-execution-teardown-all)
      (delete-directory directory t))))

(provide 'test-niwa-memory-investigation-slices)
;;; test-niwa-memory-investigation-slices.el ends here
