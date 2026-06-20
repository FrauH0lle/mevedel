;;; test-mevedel-file-state.el --- Tests for mevedel-file-state.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-file-state)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; File state structs

(mevedel-deftest mevedel-file-state--create
  (:doc "`mevedel-file-state--create' creates with all slots")
  (let ((state (mevedel-file-state--create
                :path "/tmp/foo"
                :content "hello"
                :mtime '(0 0 0 0)
                :size 5)))
    (should (equal "/tmp/foo" (mevedel-file-state-path state)))
    (should (equal "hello" (mevedel-file-state-content state)))
    (should (equal '(0 0 0 0) (mevedel-file-state-mtime state)))
    (should (= 5 (mevedel-file-state-size state)))))

(mevedel-deftest mevedel-file-interaction--create
  (:doc "`mevedel-file-interaction--create' creates with defaults")
  (let ((entry (mevedel-file-interaction--create :path "/tmp/foo")))
    (should (equal "/tmp/foo" (mevedel-file-interaction-path entry)))
    (should (null (mevedel-file-interaction-read-turn entry)))
    (should (null (mevedel-file-interaction-modified-turn entry)))))


;;
;;; File state factory

(mevedel-deftest mevedel-file-state-from-file
  ()
  ,test
  (test)
  :doc "reads existing file into state"
  (let ((tmp (make-temp-file "mevedel-fs-" nil ".txt" "hello world")))
    (unwind-protect
        (let ((state (mevedel-file-state-from-file tmp)))
          (should (mevedel-file-state-p state))
          (should (equal (expand-file-name tmp)
                         (mevedel-file-state-path state)))
          (should (equal "hello world" (mevedel-file-state-content state)))
          (should (= 11 (mevedel-file-state-size state)))
          (should (mevedel-file-state-mtime state)))
      (delete-file tmp)))

  :doc "returns nil for missing file"
  (should (null (mevedel-file-state-from-file "/tmp/mevedel-nonexistent-xyz")))

  :doc "captures empty file"
  (let ((tmp (make-temp-file "mevedel-fs-empty-" nil ".txt" "")))
    (unwind-protect
        (let ((state (mevedel-file-state-from-file tmp)))
          (should (mevedel-file-state-p state))
          (should (equal "" (mevedel-file-state-content state)))
          (should (= 0 (mevedel-file-state-size state))))
      (delete-file tmp))))


;;
;;; LRU cache primitives

(mevedel-deftest mevedel-file-cache-create
  (:doc "`mevedel-file-cache-create' returns empty cache")
  (let ((cache (mevedel-file-cache-create)))
    (should (mevedel-file-cache-p cache))
    (should (hash-table-p (mevedel-file-cache-table cache)))
    (should (null (mevedel-file-cache-order cache)))
    (should (= 0 (mevedel-file-cache-total-bytes cache)))
    (should (= 0 (hash-table-count (mevedel-file-cache-table cache))))))

(mevedel-deftest mevedel-file-cache-put
  ()
  ,test
  (test)
  :doc "inserts new state and promotes to MRU"
  (let* ((cache (mevedel-file-cache-create))
         (a (mevedel-file-state--create :path "/tmp/a" :content "aa" :size 2))
         (b (mevedel-file-state--create :path "/tmp/b" :content "bbb" :size 3)))
    (mevedel-file-cache-put cache a)
    (mevedel-file-cache-put cache b)
    (should (= 2 (hash-table-count (mevedel-file-cache-table cache))))
    (should (= 5 (mevedel-file-cache-total-bytes cache)))
    (should (equal "/tmp/b" (car (mevedel-file-cache-order cache)))))

  :doc "update in place replaces prior size"
  (let* ((cache (mevedel-file-cache-create))
         (v1 (mevedel-file-state--create :path "/tmp/a" :content "aa" :size 2))
         (v2 (mevedel-file-state--create :path "/tmp/a" :content "aaaaa" :size 5)))
    (mevedel-file-cache-put cache v1)
    (mevedel-file-cache-put cache v2)
    (should (= 1 (hash-table-count (mevedel-file-cache-table cache))))
    (should (= 5 (mevedel-file-cache-total-bytes cache)))
    (should (equal "aaaaa"
                   (mevedel-file-state-content
                    (mevedel-file-cache-get cache "/tmp/a")))))

  :doc "returns the state argument"
  (let* ((cache (mevedel-file-cache-create))
         (s (mevedel-file-state--create :path "/tmp/a" :content "x" :size 1)))
    (should (eq s (mevedel-file-cache-put cache s)))))

(mevedel-deftest mevedel-file-cache-get
  ()
  ,test
  (test)
  :doc "returns nil for missing key"
  (let ((cache (mevedel-file-cache-create)))
    (should (null (mevedel-file-cache-get cache "/tmp/missing"))))

  :doc "returns cached state and promotes to MRU"
  (let* ((cache (mevedel-file-cache-create))
         (a (mevedel-file-state--create :path "/tmp/a" :content "aa" :size 2))
         (b (mevedel-file-state--create :path "/tmp/b" :content "bb" :size 2))
         (c (mevedel-file-state--create :path "/tmp/c" :content "cc" :size 2)))
    (mevedel-file-cache-put cache a)
    (mevedel-file-cache-put cache b)
    (mevedel-file-cache-put cache c)
    ;; Order is now (c b a); touch a to promote.
    (should (eq a (mevedel-file-cache-get cache "/tmp/a")))
    (should (equal "/tmp/a" (car (mevedel-file-cache-order cache))))))

(mevedel-deftest mevedel-file-cache-remove
  ()
  ,test
  (test)
  :doc "removes existing entry and decrements total bytes"
  (let* ((cache (mevedel-file-cache-create))
         (a (mevedel-file-state--create :path "/tmp/a" :content "aa" :size 2))
         (b (mevedel-file-state--create :path "/tmp/b" :content "bbb" :size 3)))
    (mevedel-file-cache-put cache a)
    (mevedel-file-cache-put cache b)
    (should (mevedel-file-cache-remove cache "/tmp/a"))
    (should (= 1 (hash-table-count (mevedel-file-cache-table cache))))
    (should (= 3 (mevedel-file-cache-total-bytes cache)))
    (should (null (mevedel-file-cache-get cache "/tmp/a"))))

  :doc "returns nil when removing missing entry"
  (let ((cache (mevedel-file-cache-create)))
    (should (null (mevedel-file-cache-remove cache "/tmp/missing")))))

(mevedel-deftest mevedel-file-cache--evict
  ()
  ,test
  (test)
  :doc "drops least recently used entries by count"
  (let* ((mevedel-file-cache-max-entries 2)
         (mevedel-file-cache-max-bytes (* 1024 1024))
         (cache (mevedel-file-cache-create))
         (a (mevedel-file-state--create :path "/tmp/a" :content "a" :size 1))
         (b (mevedel-file-state--create :path "/tmp/b" :content "b" :size 1))
         (c (mevedel-file-state--create :path "/tmp/c" :content "c" :size 1)))
    (mevedel-file-cache-put cache a)
    (mevedel-file-cache-put cache b)
    (mevedel-file-cache-put cache c)
    (should (= 2 (hash-table-count (mevedel-file-cache-table cache))))
    (should (null (mevedel-file-cache-get cache "/tmp/a")))
    (should (mevedel-file-cache-get cache "/tmp/b"))
    (should (mevedel-file-cache-get cache "/tmp/c")))

  :doc "drops least recently used entries by byte limit"
  (let* ((mevedel-file-cache-max-entries 100)
         (mevedel-file-cache-max-bytes 5)
         (cache (mevedel-file-cache-create))
         (a (mevedel-file-state--create :path "/tmp/a" :content "aaa" :size 3))
         (b (mevedel-file-state--create :path "/tmp/b" :content "bbb" :size 3)))
    (mevedel-file-cache-put cache a)
    (mevedel-file-cache-put cache b)
    (should (= 1 (hash-table-count (mevedel-file-cache-table cache))))
    (should (= 3 (mevedel-file-cache-total-bytes cache)))
    (should (null (mevedel-file-cache-get cache "/tmp/a"))))

  :doc "MRU promotion protects recently used from eviction"
  (let* ((mevedel-file-cache-max-entries 2)
         (mevedel-file-cache-max-bytes (* 1024 1024))
         (cache (mevedel-file-cache-create))
         (a (mevedel-file-state--create :path "/tmp/a" :content "a" :size 1))
         (b (mevedel-file-state--create :path "/tmp/b" :content "b" :size 1))
         (c (mevedel-file-state--create :path "/tmp/c" :content "c" :size 1)))
    (mevedel-file-cache-put cache a)
    (mevedel-file-cache-put cache b)
    (mevedel-file-cache-get cache "/tmp/a") ; promote a
    (mevedel-file-cache-put cache c)        ; should evict b, not a
    (should (mevedel-file-cache-get cache "/tmp/a"))
    (should (null (mevedel-file-cache-get cache "/tmp/b")))
    (should (mevedel-file-cache-get cache "/tmp/c"))))

(mevedel-deftest mevedel-file-cache-clear
  (:doc "`mevedel-file-cache-clear' empties cache")
  (let* ((cache (mevedel-file-cache-create))
         (a (mevedel-file-state--create :path "/tmp/a" :content "aa" :size 2)))
    (mevedel-file-cache-put cache a)
    (mevedel-file-cache-clear cache)
    (should (= 0 (hash-table-count (mevedel-file-cache-table cache))))
    (should (= 0 (mevedel-file-cache-total-bytes cache)))
    (should (null (mevedel-file-cache-order cache)))
    (should (null (mevedel-file-cache-get cache "/tmp/a")))))


;;
;;; Session interaction helpers

(mevedel-deftest mevedel-session-record-interaction
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "records read interaction with turn count"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
         (session (mevedel-session-create "main" ws))
         (entry (mevedel-session-record-interaction session "/tmp/a" 'read 3)))
    (should (mevedel-file-interaction-p entry))
    (should (equal "/tmp/a" (mevedel-file-interaction-path entry)))
    (should (= 3 (mevedel-file-interaction-read-turn entry)))
    (should (null (mevedel-file-interaction-modified-turn entry))))

  :doc "records modify interaction"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
         (session (mevedel-session-create "main" ws))
         (entry (mevedel-session-record-interaction session "/tmp/a" 'modify 7)))
    (should (= 7 (mevedel-file-interaction-modified-turn entry)))
    (should (null (mevedel-file-interaction-read-turn entry))))

  :doc "updates existing entry in place"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
         (session (mevedel-session-create "main" ws)))
    (mevedel-session-record-interaction session "/tmp/a" 'read 1)
    (mevedel-session-record-interaction session "/tmp/a" 'modify 2)
    (let ((entry (gethash (expand-file-name "/tmp/a")
                          (mevedel-session-touched-files session))))
      (should (= 1 (mevedel-file-interaction-read-turn entry)))
      (should (= 2 (mevedel-file-interaction-modified-turn entry)))))

  :doc "errors on unknown kind"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
         (session (mevedel-session-create "main" ws)))
    (should-error
     (mevedel-session-record-interaction session "/tmp/a" 'bogus 1))))


;;
;;; External change detection

(mevedel-deftest mevedel-file-cache-detect-external-changes
  ()
  ,test
  (test)
  :doc "returns empty list when no files changed"
  (let* ((cache (mevedel-file-cache-create))
         (tmp (make-temp-file "mevedel-ec-" nil ".txt" "hello")))
    (unwind-protect
        (progn
          (mevedel-file-cache-put cache (mevedel-file-state-from-file tmp))
          (should (null (mevedel-file-cache-detect-external-changes cache))))
      (delete-file tmp)))

  :doc "detects modified file with advanced mtime and different content"
  (let* ((cache (mevedel-file-cache-create))
         (tmp (make-temp-file "mevedel-ec-" nil ".txt" "hello")))
    (unwind-protect
        (progn
          (mevedel-file-cache-put cache (mevedel-file-state-from-file tmp))
          ;; Advance mtime by one second and rewrite content.
          (let ((future (time-add (current-time) 2)))
            (with-temp-file tmp (insert "world"))
            (set-file-times tmp future))
          (let ((changes (mevedel-file-cache-detect-external-changes cache)))
            (should (= 1 (length changes)))
            (let ((change (car changes)))
              (should (eq 'modified (plist-get change :status)))
              (should (equal (expand-file-name tmp) (plist-get change :path)))
              (should (equal "hello" (plist-get change :old)))
              (should (equal "world" (plist-get change :new))))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "ignores mtime bumps when content is unchanged"
  (let* ((cache (mevedel-file-cache-create))
         (tmp (make-temp-file "mevedel-ec-" nil ".txt" "hello")))
    (unwind-protect
        (progn
          (mevedel-file-cache-put cache (mevedel-file-state-from-file tmp))
          (let ((future (time-add (current-time) 2)))
            (set-file-times tmp future))
          (should (null (mevedel-file-cache-detect-external-changes cache))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "detects deleted file"
  (let* ((cache (mevedel-file-cache-create))
         (tmp (make-temp-file "mevedel-ec-" nil ".txt" "hello")))
    (unwind-protect
        (progn
          (mevedel-file-cache-put cache (mevedel-file-state-from-file tmp))
          (delete-file tmp)
          (let ((changes (mevedel-file-cache-detect-external-changes cache)))
            (should (= 1 (length changes)))
            (let ((change (car changes)))
              (should (eq 'deleted (plist-get change :status)))
              (should (equal "hello" (plist-get change :old)))
              (should (null (plist-get change :new))))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "does not mutate cache on detection"
  (let* ((cache (mevedel-file-cache-create))
         (tmp (make-temp-file "mevedel-ec-" nil ".txt" "hello")))
    (unwind-protect
        (progn
          (mevedel-file-cache-put cache (mevedel-file-state-from-file tmp))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file tmp (insert "world"))
            (set-file-times tmp future))
          (mevedel-file-cache-detect-external-changes cache)
          (let ((state (mevedel-file-cache-get
                        cache (expand-file-name tmp))))
            (should (equal "hello" (mevedel-file-state-content state)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(mevedel-deftest mevedel-file-cache-consume-external-changes
  ()
  ,test
  (test)
  :doc "updates modified entry to current on-disk state"
  (let* ((cache (mevedel-file-cache-create))
         (tmp (make-temp-file "mevedel-ec-" nil ".txt" "hello")))
    (unwind-protect
        (progn
          (mevedel-file-cache-put cache (mevedel-file-state-from-file tmp))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file tmp (insert "world"))
            (set-file-times tmp future))
          (let ((changes (mevedel-file-cache-detect-external-changes cache)))
            (mevedel-file-cache-consume-external-changes cache changes))
          (let ((state (mevedel-file-cache-get
                        cache (expand-file-name tmp))))
            (should (equal "world" (mevedel-file-state-content state))))
          (should (null (mevedel-file-cache-detect-external-changes cache))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "removes deleted entry from cache"
  (let* ((cache (mevedel-file-cache-create))
         (tmp (make-temp-file "mevedel-ec-" nil ".txt" "hello")))
    (unwind-protect
        (progn
          (mevedel-file-cache-put cache (mevedel-file-state-from-file tmp))
          (delete-file tmp)
          (let ((changes (mevedel-file-cache-detect-external-changes cache)))
            (mevedel-file-cache-consume-external-changes cache changes))
          (should (= 0 (hash-table-count (mevedel-file-cache-table cache)))))
      (when (file-exists-p tmp) (delete-file tmp)))))


;;
;;; Session recording

(mevedel-deftest mevedel-session-record-file-access
  ()
  ,test
  (test)
  :doc "records read interaction and refreshes workspace cache"
  (let* ((tmp (make-temp-file "mevedel-ia-" nil ".txt" "hello"))
         (ws (mevedel-workspace--create
              :type 'test :id "record-access"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 3)))
    (unwind-protect
        (progn
          (mevedel-session-record-file-access session tmp 'read)
          (let ((entry (gethash (expand-file-name tmp)
                                (mevedel-session-touched-files session))))
            (should entry)
            (should (= 3 (mevedel-file-interaction-read-turn entry)))
            (should (null (mevedel-file-interaction-modified-turn entry))))
          (should (mevedel-file-cache-get
                   (mevedel-workspace-file-cache ws) tmp)))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "records modify interaction and preserves existing read-turn"
  (let* ((tmp (make-temp-file "mevedel-ia-" nil ".txt" "hello"))
         (ws (mevedel-workspace--create
              :type 'test :id "record-access-2"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (mevedel-session-record-file-access session tmp 'read)
          (setf (mevedel-session-turn-count session) 4)
          (mevedel-session-record-file-access session tmp 'modify)
          (let ((entry (gethash (expand-file-name tmp)
                                (mevedel-session-touched-files session))))
            (should (= 1 (mevedel-file-interaction-read-turn entry)))
            (should (= 4 (mevedel-file-interaction-modified-turn entry)))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "records interaction even when session has no workspace"
  (let* ((session (mevedel-session--create
                   :name "main" :workspace nil
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 7)))
    (mevedel-session-record-file-access session "/tmp/no-ws" 'read)
    (let ((entry (gethash (expand-file-name "/tmp/no-ws")
                          (mevedel-session-touched-files session))))
      (should entry)
      (should (= 7 (mevedel-file-interaction-read-turn entry))))))


;;
;;; Read deduplication

(mevedel-deftest mevedel-session-read-is-duplicate-p
  ()
  ,test
  (test)
  :doc "returns nil for never-read file"
  (let* ((tmp (make-temp-file "mevedel-dedup-" nil ".txt" "hello"))
         (ws (mevedel-workspace--create
              :type 'test :id "dedup-never"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (should-not (mevedel-session-read-is-duplicate-p
                     session tmp nil nil))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "returns non-nil when file and range match and mtime unchanged"
  (let* ((tmp (make-temp-file "mevedel-dedup-" nil ".txt" "hello"))
         (ws (mevedel-workspace--create
              :type 'test :id "dedup-hit"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (mevedel-session-record-file-access session tmp 'read nil nil)
          (should (mevedel-session-read-is-duplicate-p
                   session tmp nil nil)))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "returns nil when range differs"
  (let* ((tmp (make-temp-file "mevedel-dedup-" nil ".txt" "hello"))
         (ws (mevedel-workspace--create
              :type 'test :id "dedup-range"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (mevedel-session-record-file-access session tmp 'read 1 10)
          (should-not (mevedel-session-read-is-duplicate-p
                       session tmp 20 10))
          (should-not (mevedel-session-read-is-duplicate-p
                       session tmp nil nil))
          (should (mevedel-session-read-is-duplicate-p
                   session tmp 1 10)))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "returns nil when mtime advances after record"
  (let* ((tmp (make-temp-file "mevedel-dedup-" nil ".txt" "hello"))
         (ws (mevedel-workspace--create
              :type 'test :id "dedup-mtime"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (mevedel-session-record-file-access session tmp 'read nil nil)
          (let ((future (time-add (current-time) 2)))
            (with-temp-file tmp (insert "world"))
            (set-file-times tmp future))
          (should-not (mevedel-session-read-is-duplicate-p
                       session tmp nil nil)))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "returns nil when session has no workspace"
  (let* ((session (mevedel-session--create
                   :name "main" :workspace nil
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (should-not (mevedel-session-read-is-duplicate-p
                 session "/tmp/x" nil nil)))

  :doc "returns nil when tool-driven modify happened after read"
  (let* ((tmp (make-temp-file "mevedel-dedup-" nil ".txt" "hello"))
         (ws (mevedel-workspace--create
              :type 'test :id "dedup-post-modify"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (mevedel-session-record-file-access session tmp 'read nil nil)
          (should (mevedel-session-read-is-duplicate-p
                   session tmp nil nil))
          (setf (mevedel-session-turn-count session) 2)
          (mevedel-session-record-file-access session tmp 'modify)
          (should-not (mevedel-session-read-is-duplicate-p
                       session tmp nil nil)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'test-mevedel-file-state)
;;; test-mevedel-file-state.el ends here
