;;; test-mevedel-minimal-edit-twin.el --- Snapshot twin tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic text/property cases, independent of any view implementation.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-minimal-edit-twin
         (file-name-concat mevedel-test--worktree-root "benchmark"
                           "mevedel-minimal-edit-twin"))

(mevedel-deftest mevedel-twin-derive
  ()
  (let* ((before ,before)
         (after ,after)
         (before-copy (copy-sequence before))
         (after-copy (copy-sequence after))
         (edit (mevedel-twin-derive before after)))
    (should (= ,deleted (plist-get edit :deleted)))
    (should (= ,inserted (plist-get edit :inserted)))
    (should (= ,runs (length (plist-get edit :property-runs))))
    (should (equal-including-properties
             (substring after (plist-get edit :start)
                        (+ (plist-get edit :start) ,inserted))
             (plist-get edit :text)))
    (should (equal-including-properties before before-copy))
    (should (equal-including-properties after after-copy))
    (with-temp-buffer
      (insert before)
      (mevedel-twin-replay edit)
      (should (equal-including-properties after (buffer-string)))))
  (before after deleted inserted runs)
  :doc "insert in an unpropertized string"
  "ac" "abc" 0 1 0
  :doc "delete in an unpropertized string"
  "abc" "ac" 1 0 0
  :doc "replace only the differing middle"
  "abcde" "aXYde" 2 2 0
  :doc "repeated characters cannot overlap prefix and suffix"
  "aa" "aaa" 0 1 0
  :doc "repeated characters cannot overlap on deletion"
  "aaa" "aa" 1 0 0
  :doc "empty strings are a no-op"
  "" "" 0 0 0
  :doc "nonempty identical strings are a no-op"
  (propertize "same" 'face 'bold) (propertize "same" 'face 'bold) 0 0 0
  :doc "insert from empty carries desired properties"
  "" (propertize "ab" 'face 'bold) 0 2 0
  :doc "propertized replacement needs no explicit property writes"
  "abc" (concat "a" (propertize "XY" 'face 'bold) "c") 1 2 0
  :doc "inserted properties are free but retained character changes need a run"
  "abc" (concat (propertize "a" 'face 'italic)
                (propertize "XY" 'face 'bold) "c") 1 2 1
  :doc "delete to empty needs no property correction"
  (propertize "ab" 'face 'bold) "" 2 0 0
  :doc "property-only change preserves characters"
  "abc" (concat "a" (propertize "b" 'face 'bold) "c") 0 0 1
  :doc "property-only removal clears stale properties"
  (propertize "abc" 'face 'bold) "abc" 0 0 1
  :doc "property runs stop at already-correct characters"
  "abc" (concat (propertize "a" 'face 'bold) "b"
                (propertize "c" 'face 'bold)) 0 0 2
  :doc "adjacent differing old runs merge when desired properties match"
  (concat (propertize "a" 'face 'bold) (propertize "b" 'face 'italic)
          (propertize "c" 'face 'underline))
  (propertize "abc" 'face 'fixed-pitch) 0 0 1
  :doc "insertion carries properties matching the preceding text"
  (propertize "aa" 'face 'bold) (propertize "aaa" 'face 'bold) 0 1 0
  :doc "plain insertion does not inherit undesired properties"
  (propertize "aa" 'face 'bold)
  (concat (propertize "aa" 'face 'bold) "a") 0 1 0
  :doc "plain insertion ignores explicit stickiness"
  (propertize "aa" 'face 'bold 'rear-nonsticky t)
  (concat (propertize "aa" 'face 'bold 'rear-nonsticky t) "a") 0 1 0
  :doc "plain insertion does not inherit read-only properties"
  (propertize "aa" 'read-only t)
  (concat (propertize "aa" 'read-only t) "a") 0 1 0
  :doc "equivalent property order does not need correction"
  (propertize "aa" 'face 'bold 'help-echo "tip")
  (propertize "aa" 'help-echo "tip" 'face 'bold) 0 0 0
  :doc "replacement keeps surrounding properties and clears stale middle"
  (propertize "abc" 'face 'bold)
  (concat (propertize "a" 'face 'bold) "X" (propertize "c" 'face 'bold))
  1 1 0
  :doc "characters are counted rather than UTF-8 bytes"
  "a" (string ?a #x1f600) 0 1 0)

(mevedel-deftest mevedel-twin--same-properties-p
  (:doc "property comparison ignores order but preserves absent versus nil")
  (progn
    (should (mevedel-twin--same-properties-p '(face bold foo nil)
                                            '(foo nil face bold)))
    (should-not (mevedel-twin--same-properties-p '(foo nil) nil))
    (should-not (mevedel-twin--same-properties-p '(face bold) '(face italic)))))

(mevedel-deftest mevedel-twin--apply
  (:doc "plain replay insertion ignores caller stickiness and inhibits hooks")
  (let* ((before (propertize "aa" 'face 'bold))
         (after (concat before "a"))
         (edit (mevedel-twin-derive before after)))
    (with-temp-buffer
      (insert before)
      (setq-local text-property-default-nonsticky '((face . t)))
      (add-hook 'before-change-functions
                (lambda (&rest _) (error "Replay ran a change hook")) nil t)
      (setq buffer-read-only t)
      (mevedel-twin--apply edit)
      (should (equal-including-properties after (buffer-string))))))

(mevedel-deftest mevedel-twin--assert
  (:doc "snapshot assertion detects property differences as well as text")
  (with-temp-buffer
    (insert (propertize "a" 'face 'bold))
    (mevedel-twin--assert (propertize "a" 'face 'bold))
    (should-error (mevedel-twin--assert "a"))
    (should-error (mevedel-twin--assert "b"))))

(mevedel-deftest mevedel-twin-replay
  ()
  ,test
  (test)
  :doc "replay widens and restores narrowing"
  (with-temp-buffer
    (insert "abc")
    (narrow-to-region 1 2)
    (mevedel-twin-replay (mevedel-twin-derive "abc" "abX"))
    (should (= 2 (point-max)))
    (save-restriction
      (widen)
      (should (equal "abX" (buffer-string)))))
  :doc "replay rejects the wrong snapshot before mutation"
  (with-temp-buffer
    (insert "wrong")
    (should-error (mevedel-twin-replay (mevedel-twin-derive "old" "new")))
    (should (equal "wrong" (buffer-string)))))

(mevedel-deftest mevedel-twin-observe
  ()
  ,test
  (test)
  :doc "observation sees inhibited hooks and excludes other buffers/strings"
  (with-temp-buffer
    (insert "abc")
    (let* ((target (current-buffer))
           (result
            (mevedel-twin-observe
             (lambda ()
               (let ((inhibit-modification-hooks t))
                 (delete-region 2 3)
                 (goto-char 2)
                 (insert "XY" ?Z)
                 (set-text-properties 2 3 '(face bold))
                 (add-text-properties 3 4 '(face italic))
                 (put-text-property 4 5 'face 'underline)
                 ;; A redundant primitive write is still real work.
                 (put-text-property 4 5 'face 'underline)
                 (set-text-properties 2 2 nil)
                 (set-text-properties 0 1 '(face bold) (copy-sequence "s"))
                 (with-temp-buffer
                   (insert "unobserved")
                   (set-text-properties 1 2 '(face bold))
                   ;; An explicit target buffer still counts from elsewhere.
                   (put-text-property 1 2 'help-echo "tip" target)))))))
      (should (equal "aXYZc" (buffer-substring-no-properties 1 (point-max))))
      (should (= 1 (plist-get result :real-deleted)))
      (should (= 3 (plist-get result :real-inserted)))
      (should (= 5 (plist-get result :real-property-runs)))))
  :doc "no-op update has zero mutation counts"
  (with-temp-buffer
    (insert "abc")
    (let ((result (mevedel-twin-observe #'ignore)))
      (dolist (key '(:real-deleted :real-inserted :real-property-runs))
        (should (= 0 (plist-get result key))))))
  :doc "propertized insertion carries properties without explicit writes"
  (with-temp-buffer
    (let* ((text (propertize "ab" 'face 'bold))
           (result (mevedel-twin-observe (lambda () (insert text)))))
      (should (equal-including-properties text (buffer-string)))
      (should (= 2 (plist-get result :real-inserted)))
      (should (= 0 (plist-get result :real-property-runs)))))
  :doc "advice is removed on success and failure, preserving existing advice"
  (let* ((primitives '(delete-region insert set-text-properties
                      add-text-properties put-text-property))
         (definitions (mapcar #'symbol-function primitives)))
    (with-temp-buffer
      (mevedel-twin-observe (lambda () (insert "ok")))
      (should (equal definitions (mapcar #'symbol-function primitives)))
      (should-error
       (mevedel-twin-observe
        (lambda () (insert "partial") (error "Expected update failure"))))
      (should (equal definitions (mapcar #'symbol-function primitives)))))
  :doc "ordinary bytecode opcodes bypass advice as a negative control"
  (with-temp-buffer
    (insert "abc")
    (let* ((update (byte-compile
                    (lambda ()
                      (delete-region 2 3)
                      (goto-char 2)
                      (insert "XY"))))
           (result (mevedel-twin-observe update)))
      (should (equal "aXYc" (buffer-string)))
      (should (= 0 (plist-get result :real-deleted)))
      (should (= 0 (plist-get result :real-inserted)))))
  :doc "generic-call compiled observation build sees insert and deletion"
  (let* ((symbols '(insert delete-region))
         (handlers (mapcar (lambda (symbol) (get symbol 'byte-compile)) symbols)))
    (unwind-protect
        (progn
          (dolist (symbol symbols) (put symbol 'byte-compile nil))
          (with-temp-buffer
            (insert "abc")
            (let* ((update (byte-compile
                            (lambda ()
                              (delete-region 2 3)
                              (goto-char 2)
                              (insert "XY"))))
                   (result (mevedel-twin-observe update)))
              (should (equal "aXYc" (buffer-string)))
              (should (= 1 (plist-get result :real-deleted)))
              (should (= 2 (plist-get result :real-inserted))))))
      (cl-mapc (lambda (symbol handler) (put symbol 'byte-compile handler))
               symbols handlers))))

(mevedel-deftest mevedel-twin-measure
  ()
  ,test
  (test)
  :doc "measurement returns floor counts and nonnegative times"
  (with-temp-buffer
    (insert "abc")
    (let ((result (mevedel-twin-measure
                   (lambda ()
                     (delete-region 2 3)
                     (goto-char 2)
                     (insert (propertize "XY" 'face 'bold))))))
      (should (equal "aXYc" (buffer-substring-no-properties 1 (point-max))))
      (should (= 1 (plist-get result :floor-deleted)))
      (should (= 2 (plist-get result :floor-inserted)))
      (should (= 0 (plist-get result :floor-property-runs)))
      (dolist (key '(:real-seconds :floor-seconds))
        (should (floatp (plist-get result key)))
        (should (>= (plist-get result key) 0)))))
  :doc "no-op measurement returns zero floor counts"
  (with-temp-buffer
    (insert "abc")
    (let ((result (mevedel-twin-measure #'ignore)))
      (dolist (key '(:floor-deleted :floor-inserted :floor-property-runs))
        (should (= 0 (plist-get result key))))))
  :doc "whole update timing excludes derivation and floor assertion"
  (with-temp-buffer
    (let ((real-apply (symbol-function 'mevedel-twin--apply))
          (real-derive (symbol-function 'mevedel-twin-derive))
          (real-assert (symbol-function 'mevedel-twin--assert))
          (clock 0))
      ;; A deterministic clock proves boundaries without wall-time thresholds.
      (cl-letf (((symbol-function 'current-time) (lambda () clock))
                ((symbol-function 'mevedel-twin-derive)
                 (lambda (&rest args)
                   (cl-incf clock 100)
                   (apply real-derive args)))
                ((symbol-function 'mevedel-twin--apply)
                 (lambda (&rest args)
                   (cl-incf clock 2)
                   (apply real-apply args)))
                ((symbol-function 'mevedel-twin--assert)
                 (lambda (&rest args)
                   (cl-incf clock 100)
                   (apply real-assert args))))
        (let ((result (mevedel-twin-measure
                       (lambda () (cl-incf clock 3) (insert "x")))))
          (should (= 3.0 (plist-get result :real-seconds)))
          (should (= 2.0 (plist-get result :floor-seconds))))))))

(provide 'test-mevedel-minimal-edit-twin)
;;; test-mevedel-minimal-edit-twin.el ends here
