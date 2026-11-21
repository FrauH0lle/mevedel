;;; test-niwa-profiler-optimization.el --- Projection benchmark tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise the benchmark's real projection fixtures without running its CLI
;; or collecting profiler artifacts during ERT.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'org-persist)

(defvar niwa-benchmark--no-main)
(defvar org-persist-directory)

(let ((niwa-benchmark--no-main t)
      (org-persist-directory org-persist-directory))
  (require 'niwa-profiler-optimization
           (file-name-concat
            (file-name-directory
             (or load-file-name byte-compile-current-file buffer-file-name))
            ".." "benchmark" "niwa-profiler-optimization.el")))


(niwa-deftest niwa-benchmark--compiled-harness ()
  ,test
  (test)
  :doc "plain batch byte compilation and cold compiled loading find sibling fixtures"
  (let* ((root niwa-benchmark--repository-directory)
         (temporary (make-temp-file "niwa-benchmark-compile-test-" t))
         (benchmark (file-name-concat temporary "benchmark"))
         (source (file-name-concat benchmark "niwa-profiler-optimization.el"))
         (compiled (byte-compile-dest-file source))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (make-directory benchmark)
          (dolist (name '("niwa-profiler-optimization.el" "niwa-view-fixtures.el"))
            (copy-file (file-name-concat root "benchmark" name)
                       (file-name-concat benchmark name)))
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil "--batch" "-Q"
                    "--eval" (prin1-to-string
                              `(progn
                                 (setq load-path ',load-path
                                       byte-compile-error-on-warn t
                                       niwa-benchmark--no-main t)
                                 (when (native-comp-available-p)
                                   (startup-redirect-eln-cache
                                    ,(file-name-concat temporary "eln")))))
                    "-f" "batch-byte-compile" source)))
              (ert-info ((buffer-string))
                (should (eql status 0))
                (should (equal "" (buffer-string))))))
          (should (file-exists-p compiled))
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil "--batch" "-Q"
                    "--eval" (prin1-to-string
                              `(progn
                                 (setq load-path ',load-path
                                       niwa-benchmark--no-main t)
                                 (when (native-comp-available-p)
                                   (startup-redirect-eln-cache
                                    ,(file-name-concat temporary "eln")))))
                    "-l" compiled
                    "--eval" (prin1-to-string
                              `(progn
                                 (cl-assert (featurep 'niwa-view-fixtures))
                                 (cl-assert (byte-code-function-p
                                             (symbol-function 'niwa-benchmark-run)))
                                 (cl-assert (equal niwa-benchmark--harness-file
                                                   ,compiled))
                                 (cl-assert (equal niwa-benchmark--repository-directory
                                                   ,(file-name-as-directory temporary))))))))
              (ert-info ((buffer-string))
                (should (eql status 0))
                (should (equal "" (buffer-string)))))))
      (delete-directory temporary t))))


(niwa-deftest niwa-benchmark--draft-intact-p ()
  ,test
  (test)
  :doc "checks the dock's caret, not point in the transcript"
  (niwa-benchmark--with-view
   (lambda (_data _view)
     (let ((dock (niwa-benchmark--place-draft)))
       (goto-char (point-min))
       (should (niwa-benchmark--draft-intact-p dock))
       (set-window-point dock (niwa-view-composer-draft-end))
       (should-not (niwa-benchmark--draft-intact-p dock))))))


(niwa-deftest niwa-benchmark--projection-history ()
  ,test
  (test)
  :doc "builds forty real mixed exchanges that the transcript scanner consumes"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (niwa-benchmark--projection-history)
    (let* ((segments (niwa-transcript-segments (point-min) (point-max)))
           (types (mapcar #'car segments)))
      (should (= 40 (cl-count 'reasoning types)))
      (should (= 120 (cl-count 'tool types)))
      (should (= 40 (cl-count 'response types)))
      (should (= 40 (cl-count 'assistant
                              (niwa-view--group-into-turns segments (current-buffer))
                              :key (lambda (turn) (plist-get turn :role))))))
    (goto-char (point-min))
    (should (search-forward "* Prompt 0\n" nil t))
    (should (search-forward "## Result 39\n" nil t))))


(niwa-deftest niwa-benchmark--projection-drain ()
  ,test
  (test)
  :doc "drains real scheduled projection without dispatch and preserves text and composer"
  (niwa-benchmark--with-view
   (lambda (data view)
     (with-current-buffer data (niwa-benchmark--projection-history))
     (with-current-buffer view
       (should (niwa-view-composer-display (selected-window)))
       (niwa-view-test--insert-composer-draft "> benchmark draft\nsecond line" 4)
       (let ((composer (niwa-view-composer-window)))
         (set-window-point composer
                           (+ 4 (niwa-view-composer-draft-start))))
       (niwa-view--full-rerender data)
       (let ((reference (buffer-substring-no-properties
                         (point-min) (niwa-view-transcript-tail)))
             (source (with-current-buffer data (buffer-string))))
         (should (string-match-p "Result 39" reference))
         (should (string-match-p "Read" reference))
         ;; Drive callbacks twice to cover repeated planning/publication,
         ;; not merely a no-op after the first completed job.
         (dotimes (_ 2)
           (should (> (niwa-benchmark--projection-drain) 1))
           (should-not niwa-view-render--batch)
           (should (equal reference (buffer-substring-no-properties
                                     (point-min) (niwa-view-transcript-tail))))
           (should (equal "> benchmark draft\nsecond line"
                          (niwa-view-composer-draft-text)))
           (should (= 4 (niwa-view-test--composer-point-offset)))
           (should (= (window-point (niwa-view-composer-window))
                      (with-current-buffer (niwa-view-composer-buffer) (point))))
           (should (equal-including-properties
                    source (with-current-buffer data (buffer-string))))))))))


(niwa-deftest niwa-benchmark--projection-attribution ()
  ,test
  (test)
  :doc "charges nested stages exclusively and partitions cached turn work"
  (let ((scan (make-symbol "scan"))
        (scan-child (make-symbol "scan-child"))
        (turn (make-symbol "turn"))
        (cache (make-symbol "cache"))
        (markdown (make-symbol "markdown"))
        (insertion (make-symbol "insertion"))
        (clock 0.0))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
              ((symbol-function insertion) (lambda (cost) (cl-incf clock cost)))
              ((symbol-function scan-child) (lambda () (cl-incf clock 4)))
              ((symbol-function scan)
               (lambda ()
                 (cl-incf clock 3)
                 (funcall scan-child)
                 (funcall insertion 17)))
              ((symbol-function markdown)
               (lambda () (cl-incf clock 7) (funcall insertion 11)))
              ((symbol-function cache)
               (lambda () (cl-incf clock 2) (funcall markdown)))
              ((symbol-function turn)
               (lambda ()
                 (cl-incf clock 5)
                 (funcall cache)
                 (funcall insertion 13))))
      (let* ((stages `((,scan . scan) (,scan-child . scan) (,turn . turn)
                       (,cache . response-cache) (,markdown . markdown)
                       (,insertion . insertion)))
             (originals (mapcar (lambda (entry) (symbol-function (car entry)))
                               stages))
             (result (niwa-benchmark--projection-attribution
                      (lambda ()
                        (cl-incf clock 2)
                        (funcall scan)
                        (funcall turn)
                        (cl-incf clock 19))
                      stages)))
        (dolist (entry '((publication . 21) (scan . 24) (turn . 5)
                         (response-cache . 2) (markdown . 7) (insertion . 24)))
          (should (= (cdr entry)
                     (alist-get (car entry) (plist-get result :seconds)))))
        (should (= 83 (cl-reduce #'+ (plist-get result :seconds) :key #'cdr)))
        (dolist (entry '((scan . 2) (turn . 1) (response-cache . 1)
                         (markdown . 1) (insertion . 2)))
          (should (= (cdr entry)
                     (alist-get (car entry) (plist-get result :calls)))))
        (should-not (alist-get 'scan (plist-get result :inside-turn-seconds)))
        (should (= 38 (cl-reduce #'+ (plist-get result :inside-turn-seconds)
                                 :key #'cdr)))
        (should (= 18 (plist-get result :turn-uncached-seconds)))
        (should (= 20 (plist-get result :turn-cached-seconds)))
        (should (equal originals
                       (mapcar (lambda (entry) (symbol-function (car entry)))
                               stages))))))
  :doc "restores every wrapped function when a nested workload signals"
  (let ((outer (make-symbol "outer"))
        (inner (make-symbol "inner"))
        (entered 0))
    (cl-letf (((symbol-function outer) (lambda () (funcall inner)))
              ((symbol-function inner)
               (lambda () (cl-incf entered) (error "Injected attribution failure"))))
      (let ((outer-original (symbol-function outer))
            (inner-original (symbol-function inner)))
        (should (equal '(error "Injected attribution failure")
                       (should-error
                        (niwa-benchmark--projection-attribution
                         (lambda () (funcall outer))
                         `((,outer . turn) (,inner . markdown))))))
        (should (= entered 1))
        (should (eq outer-original (symbol-function outer)))
        (should (eq inner-original (symbol-function inner)))))))


(niwa-deftest niwa-benchmark--projection-restored (:quiet t)
  ,test
  (test)
  :doc "restores a private read-only production session copy and leaves the source intact"
  (let* ((root (make-temp-file "niwa-benchmark-restore-test-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "projection-source" workspace))
         (buffer (generate-new-buffer " *projection-source*"))
         copied-root restored-data restored-view called)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (niwa--transcript-org-mode)
            (gptel-mode 1)
            (insert "* Saved prompt\nExplain the restored fixture.\n\n")
            (insert (propertize "Saved **response**.\n" 'gptel 'response))
            (niwa-session-artifacts-save session buffer))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (let* ((source (niwa-session-save-path session))
                 (files (sort (directory-files-recursively source ".") #'string<))
                 (digests (mapcar #'niwa-benchmark--file-sha256 files)))
            ;; Keep the production copy, restore, setup and cleanup path; replace
            ;; only expensive repeated measurement/profiler artifact collection.
            (cl-letf (((symbol-function 'niwa-benchmark--projection-measure)
                       (lambda (data view directory parameters)
                         (setq called t restored-data data restored-view view
                               copied-root
                               (niwa-workspace-root
                                (niwa-session-workspace
                                 (buffer-local-value 'niwa--session data))))
                         (should (equal directory (file-name-concat root "output")))
                         (should (eq 'restored (plist-get parameters :fixture)))
                         (should (equal (niwa-session-session-id session)
                                        (plist-get parameters :session-id)))
                         (should (buffer-local-value 'niwa-session--read-only-mode data))
                         (should-not (file-in-directory-p
                                      (niwa-session-save-path
                                       (buffer-local-value 'niwa--session data))
                                      root))
                         (with-current-buffer data
                           (should (memq 'response
                                         (mapcar #'car (niwa-transcript-segments
                                                       (point-min) (point-max))))))
                         (with-current-buffer view
                           (niwa-view-test--insert-composer-draft
                            "> restored draft\nsecond line" 3)
                           (let ((composer (niwa-view-composer-window)))
                             (set-window-point
                              composer (+ 3 (niwa-view-composer-draft-start))))
                           (niwa-view--full-rerender data)
                           (let ((text (buffer-substring-no-properties
                                        (point-min) (niwa-view-transcript-tail))))
                             (should (string-match-p "Saved" text))
                             (should (> (niwa-benchmark--projection-drain) 0))
                             (should (equal text (buffer-substring-no-properties
                                                 (point-min) (niwa-view-transcript-tail))))
                             (should (equal "> restored draft\nsecond line"
                                            (niwa-view-composer-draft-text)))
                             (should (= 3 (niwa-view-test--composer-point-offset)))
                             (should (= (window-point (niwa-view-composer-window))
                                        (with-current-buffer (niwa-view-composer-buffer) (point)))))))))
              (niwa-benchmark--projection-restored
               (file-name-concat root "output") source))
            (should called)
            (should-not (buffer-live-p restored-data))
            (should-not (buffer-live-p restored-view))
            (should-not (file-exists-p copied-root))
            (should (equal files (sort (directory-files-recursively source ".") #'string<)))
            (should (equal digests (mapcar #'niwa-benchmark--file-sha256 files)))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-profiler-optimization)
;;; test-niwa-profiler-optimization.el ends here
