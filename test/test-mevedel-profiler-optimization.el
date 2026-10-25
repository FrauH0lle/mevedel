;;; test-mevedel-profiler-optimization.el --- Projection benchmark tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise the benchmark's real projection fixtures without running its CLI
;; or collecting profiler artifacts during ERT.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'org-persist)

(defvar mevedel-benchmark--no-main)
(defvar org-persist-directory)

(let ((mevedel-benchmark--no-main t)
      (org-persist-directory org-persist-directory))
  (require 'mevedel-profiler-optimization
           (file-name-concat
            (file-name-directory
             (or load-file-name byte-compile-current-file buffer-file-name))
            ".." "benchmark" "mevedel-profiler-optimization.el")))


(mevedel-deftest mevedel-benchmark--compiled-harness ()
  ,test
  (test)
  :doc "plain batch byte compilation and cold compiled loading find sibling fixtures"
  (let* ((root mevedel-benchmark--repository-directory)
         (temporary (make-temp-file "mevedel-benchmark-compile-test-" t))
         (benchmark (file-name-concat temporary "benchmark"))
         (source (file-name-concat benchmark "mevedel-profiler-optimization.el"))
         (compiled (byte-compile-dest-file source))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (make-directory benchmark)
          (dolist (name '("mevedel-profiler-optimization.el" "mevedel-view-fixtures.el"))
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
                                       mevedel-benchmark--no-main t)
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
                                       mevedel-benchmark--no-main t)
                                 (when (native-comp-available-p)
                                   (startup-redirect-eln-cache
                                    ,(file-name-concat temporary "eln")))))
                    "-l" compiled
                    "--eval" (prin1-to-string
                              `(progn
                                 (cl-assert (featurep 'mevedel-view-fixtures))
                                 (cl-assert (byte-code-function-p
                                             (symbol-function 'mevedel-benchmark-run)))
                                 (cl-assert (equal mevedel-benchmark--harness-file
                                                   ,compiled))
                                 (cl-assert (equal mevedel-benchmark--repository-directory
                                                   ,(file-name-as-directory temporary))))))))
              (ert-info ((buffer-string))
                (should (eql status 0))
                (should (equal "" (buffer-string)))))))
      (delete-directory temporary t))))


(mevedel-deftest mevedel-benchmark--projection-history ()
  ,test
  (test)
  :doc "builds forty real mixed exchanges that the transcript scanner consumes"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (mevedel-benchmark--projection-history)
    (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
           (types (mapcar #'car segments)))
      (should (= 40 (cl-count 'reasoning types)))
      (should (= 120 (cl-count 'tool types)))
      (should (= 40 (cl-count 'response types)))
      (should (= 40 (cl-count 'assistant
                              (mevedel-view--group-transcript-turns
                               segments (current-buffer))
                              :key (lambda (turn) (plist-get turn :role))))))
    (goto-char (point-min))
    (should (search-forward "* Prompt 0\n" nil t))
    (should (search-forward "## Result 39\n" nil t))))


(mevedel-deftest mevedel-benchmark--projection-drain ()
  ,test
  (test)
  :doc "drains real scheduled projection without dispatch and preserves text and composer"
  (mevedel-benchmark--with-view
   (lambda (data view)
     (with-current-buffer data (mevedel-benchmark--projection-history))
     (with-current-buffer view
       (goto-char (mevedel-view--input-start))
       (insert "> benchmark draft\nsecond line")
       (goto-char (+ (mevedel-view--input-start) 4))
       (set-window-point (selected-window) (point))
       (mevedel-view--full-rerender data)
       (let ((reference (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker))
             (source (with-current-buffer data (buffer-string))))
         (should (string-match-p "Result 39" reference))
         (should (string-match-p "Read" reference))
         ;; Drive callbacks twice to cover repeated planning/publication,
         ;; not merely a no-op after the first completed job.
         (dotimes (_ 2)
           (should (> (mevedel-benchmark--projection-drain) 1))
           (should-not mevedel-view-render--batch)
           (should (equal reference (buffer-substring-no-properties
                                     (point-min) mevedel-view--input-marker)))
           (should (equal "> benchmark draft\nsecond line"
                          (mevedel-view--input-text)))
           (should (= (point) (+ (mevedel-view--input-start) 4)))
           (should (= (window-point (selected-window)) (point)))
           (should (equal-including-properties
                    source (with-current-buffer data (buffer-string))))))))))


(mevedel-deftest mevedel-benchmark--projection-attribution ()
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
             (result (mevedel-benchmark--projection-attribution
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
                        (mevedel-benchmark--projection-attribution
                         (lambda () (funcall outer))
                         `((,outer . turn) (,inner . markdown))))))
        (should (= entered 1))
        (should (eq outer-original (symbol-function outer)))
        (should (eq inner-original (symbol-function inner)))))))


(mevedel-deftest mevedel-benchmark--projection-restored (:quiet t)
  ,test
  (test)
  :doc "restores a private read-only production session copy and leaves the source intact"
  (let* ((root (make-temp-file "mevedel-benchmark-restore-test-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "projection-source" workspace))
         (buffer (generate-new-buffer " *projection-source*"))
         copied-root restored-data restored-view called)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel--transcript-org-mode)
            (gptel-mode 1)
            (insert "* Saved prompt\nExplain the restored fixture.\n\n")
            (insert (propertize "Saved **response**.\n" 'gptel 'response))
            (mevedel-session-artifacts-save session buffer))
          (test-mevedel-session-persistence--release-and-kill buffer session)
          (let* ((source (mevedel-session-save-path session))
                 (files (sort (directory-files-recursively source ".") #'string<))
                 (digests (mapcar #'mevedel-benchmark--file-sha256 files)))
            ;; Keep the production copy, restore, setup and cleanup path; replace
            ;; only expensive repeated measurement/profiler artifact collection.
            (cl-letf (((symbol-function 'mevedel-benchmark--projection-measure)
                       (lambda (data view directory parameters)
                         (setq called t restored-data data restored-view view
                               copied-root
                               (mevedel-workspace-root
                                (mevedel-session-workspace
                                 (buffer-local-value 'mevedel--session data))))
                         (should (equal directory (file-name-concat root "output")))
                         (should (eq 'restored (plist-get parameters :fixture)))
                         (should (equal (mevedel-session-session-id session)
                                        (plist-get parameters :session-id)))
                         (should (buffer-local-value 'mevedel-session--read-only-mode data))
                         (should-not (file-in-directory-p
                                      (mevedel-session-save-path
                                       (buffer-local-value 'mevedel--session data))
                                      root))
                         (with-current-buffer data
                           (should (memq 'response
                                         (mapcar #'car (mevedel-transcript-segments
                                                       (point-min) (point-max))))))
                         (with-current-buffer view
                           (goto-char (mevedel-view--input-start))
                           (insert "> restored draft\nsecond line")
                           (goto-char (+ (mevedel-view--input-start) 3))
                           (set-window-point (selected-window) (point))
                           (mevedel-view--full-rerender data)
                           (let ((text (buffer-substring-no-properties
                                        (point-min) mevedel-view--input-marker)))
                             (should (string-match-p "Saved" text))
                             (should (> (mevedel-benchmark--projection-drain) 0))
                             (should (equal text (buffer-substring-no-properties
                                                 (point-min) mevedel-view--input-marker)))
                             (should (equal "> restored draft\nsecond line"
                                            (mevedel-view--input-text)))
                             (should (= (point) (+ (mevedel-view--input-start) 3)))
                             (should (= (window-point (selected-window)) (point))))))))
              (mevedel-benchmark--projection-restored
               (file-name-concat root "output") source))
            (should called)
            (should-not (buffer-live-p restored-data))
            (should-not (buffer-live-p restored-view))
            (should-not (file-exists-p copied-root))
            (should (equal files (sort (directory-files-recursively source ".") #'string<)))
            (should (equal digests (mapcar #'mevedel-benchmark--file-sha256 files)))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-profiler-optimization)
;;; test-mevedel-profiler-optimization.el ends here
