;;; test-mevedel-collaboration-artifact.el --- focused collaboration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for the extracted collaboration feature module.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-agent-control)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-pending-inputs)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-view-agent)
(require 'mevedel-view-render)
(require 'mevedel-workspace)

(require 'mevedel-collaboration-artifact-projection)
(require 'mevedel-collaboration-artifact)

(mevedel-deftest mevedel-collaboration--artifact-fields
  (:doc "projects selected ApplyPatch render data inside the artifacts directory")
  (let* ((save-path (make-temp-file "mevedel-collab-artifacts-" t))
         (dir (mevedel-session-artifacts-artifacts-dir save-path))
         (session (mevedel-session--create :name "s" :save-path save-path))
         (path (file-name-concat dir "mockup.html")))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (make-directory dir t)
          (write-region "<h1>hi</h1>" nil path nil 'silent)
          (let ((fields (car (mevedel-collaboration--artifact-fields
                              `(:kind patch
                                :files ((:kind add :path ,path)))))))
            (should (equal "mockup.html" (plist-get fields :artifact)))
            (should (= 11 (plist-get fields :size)))
            (should (equal (expand-file-name path)
                           (plist-get fields :artifact-path)))
            (should-not (plist-member fields :missing)))
          (should-not
           (mevedel-collaboration--artifact-fields
            `(:kind patch :files
              ((:kind add :path ,(file-name-concat save-path "notes.html"))
               (:kind delete :path ,path)))))
          (should-not (mevedel-collaboration--artifact-fields
                       `(:kind media :files ((:kind add :path ,path)))))
          ;; The stat is memoized against per-tick target round trips,
          ;; and a settling ApplyPatch drops the entry so the card follows.
          (write-region "<h1>hello again</h1>" nil path nil 'silent)
          (should (= 11 (plist-get
                         (car (mevedel-collaboration--artifact-fields
                               `(:kind patch
                                 :files ((:kind update :path ,path)))))
                         :size)))
          (mevedel-collaboration--artifact-stat-invalidate)
          (should (= 20 (plist-get
                         (car (mevedel-collaboration--artifact-fields
                               `(:kind patch
                                 :files ((:kind update :path ,path)))))
                         :size)))
          ;; A deleted artifact still projects, marked missing, so it
          ;; reads as deleted rather than as a gap in the log.
          (delete-file path)
          (mevedel-collaboration--artifact-stat-invalidate)
          (let ((fields (car (mevedel-collaboration--artifact-fields
                              `(:kind patch
                                :files ((:kind update :path ,path)))))))
            (should (eq t (plist-get fields :missing)))
            (should-not (plist-member fields :size)))
          ;; Without a session there is no artifacts directory at all.
          (with-temp-buffer
            (should-not (mevedel-collaboration--artifact-fields
                         `(:kind patch
                           :files ((:kind add :path ,path))))))
          ;; A remote session's TRAMP directory accepts the model's
          ;; target-native path and maps host I/O back to the TRAMP form.
          (with-temp-buffer
            (setq-local mevedel--session
                        (mevedel-session--create
                         :name "r" :save-path "/ssh:example:/base"))
            (cl-letf (((symbol-function
                        'mevedel-collaboration--artifact-stat)
                       (lambda (seen)
                         (should (equal "/ssh:example:/base/artifacts/m.html"
                                        seen))
                         (cons 5 nil))))
              (let ((fields (car (mevedel-collaboration--artifact-fields
                                  '(:kind patch
                                    :files
                                    ((:kind add
                                      :path "/base/artifacts/m.html")))))))
                (should (equal "m.html" (plist-get fields :artifact)))
                (should (equal "/ssh:example:/base/artifacts/m.html"
                               (plist-get fields :artifact-path)))))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory save-path t))))


(mevedel-deftest mevedel-collaboration--tool-segment-records
  (:doc "expands selected ApplyPatch files into stable artifact cards")
  (let* ((save-path (make-temp-file "mevedel-collab-patch-artifacts-" t))
         (dir (mevedel-session-artifacts-artifacts-dir save-path))
         (one (file-name-concat dir "one.html"))
         (two (file-name-concat dir "two.md"))
         (code (file-name-concat save-path "code.el"))
         parsed)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session
                      (mevedel-session--create :name "s" :save-path save-path))
          (insert "tool")
          (make-directory dir t)
          (write-region "one" nil one nil 'silent)
          (write-region "two" nil two nil 'silent)
          (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                     (lambda (_buffer _start _end) parsed)))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Applied patch: 2 changes"
                    :render-data
                    (:kind patch :files
                     ((:kind add :path ,one) (:kind add :path ,two)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (equal '("one.html" "two.md")
                             (mapcar (lambda (record)
                                       (plist-get record :artifact))
                                     records)))
              (should (= 2 (length (delete-dups
                                    (mapcar (lambda (record)
                                              (plist-get record :id))
                                            records)))))
              (should (= 1 (length
                            (mevedel-collaboration--tool-records records)))))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Applied patch: 2 changes"
                    :render-data
                    (:kind patch :files
                     ((:kind update :path ,code)
                      (:kind move :path ,code :move-path ,one)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (= 2 (length records)))
              (should-not (plist-get (car records) :artifact))
              (should (equal "one.html"
                             (plist-get (cadr records) :artifact))))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Error: patch failed"
                    :render-data
                    (:kind patch :files ((:kind add :path ,one)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (= 1 (length records)))
              (should-not (plist-get (car records) :artifact)))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory save-path t))))


(mevedel-deftest mevedel-collaboration-notify-artifacts-changed
  (:doc "drops cached artifact stats and re-publishes the shared room")
  (let* ((data-buffer (generate-new-buffer " *collab-artifacts-data*"))
         (session (mevedel-session--create :name "artifacts"))
         (room (list :session session :data-buffer data-buffer
                     :guests (make-hash-table :test #'eql)
                     :transport 'transport))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         (path (make-temp-file "mevedel-collab-stat-"))
         published)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                   (lambda (target) (push target published))))
          ;; Prime the memo, then delete behind it: the stale size
          ;; survives until this seam drops the cache.
          (should (= 0 (car (mevedel-collaboration--artifact-stat
                             (expand-file-name path)))))
          (delete-file path)
          (mevedel-collaboration-notify-artifacts-changed session)
          (should (equal (list room) published))
          (should (cdr (mevedel-collaboration--artifact-stat
                        (expand-file-name path))))
          ;; An unshared session still drops the cache, publishes nothing,
          ;; and does not error.
          (mevedel-collaboration-notify-artifacts-changed
           (mevedel-session--create :name "other"))
          (should (= 1 (length published))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (when (file-exists-p path) (delete-file path))
      (kill-buffer data-buffer))))


(mevedel-deftest mevedel-collaboration--artifact-mime
  (:doc "maps artifact extensions case-insensitively and defaults to octet-stream")
  (progn
    (should (equal "text/html"
                   (mevedel-collaboration--artifact-mime "Mockup.HTML")))
    (should (equal "text/markdown"
                   (mevedel-collaboration--artifact-mime "notes.md")))
    (should (equal "image/png"
                   (mevedel-collaboration--artifact-mime "shot.png")))
    (should (equal "application/octet-stream"
                   (mevedel-collaboration--artifact-mime "data.bin")))
    (should (equal "application/octet-stream"
                   (mevedel-collaboration--artifact-mime "noext")))))

(mevedel-deftest mevedel-collaboration--handle-artifact-get
  (:doc "answers published artifacts in bounded chunks and refuses everything else")
  (let* ((save-path (make-temp-file "mevedel-guest-artifact-" t))
         (dir (file-name-as-directory
               (expand-file-name
                (mevedel-session-artifacts-artifacts-dir save-path))))
         (path (file-name-concat dir "mockup.html"))
         (outside (file-name-concat save-path "outside.txt"))
         (escape (file-name-concat dir "escape.txt"))
         (session (mevedel-session--create :name "s" :save-path save-path))
         (guests (make-hash-table :test #'eql))
         (content (make-string 1000 ?x))
         (room (list :session session :guests guests :transport 'transport
                     :records
                     (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                                 :artifact "mockup.html"
                                 :artifact-path path)
                           (list :id "tool-2" :kind "tool" :name "ApplyPatch"
                                 :artifact "escape"
                                 :artifact-path "/etc/passwd")
                           (list :id "tool-3" :kind "tool" :name "Bash"))))
         (now 1000.0)
         sent)
    ;; A read-only guest may fetch: the card is read state.
    (puthash 1 (list :name "viewer" :writable nil :ready t) guests)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'float-time)
                   (lambda (&optional _) now)))
          (make-directory dir t)
          (let ((coding-system-for-write 'binary))
            (write-region content nil path nil 'silent)
            (write-region "private" nil outside nil 'silent))
          (make-symbolic-link outside escape)
          (setf (plist-get room :records)
                (append (plist-get room :records)
                        (list (list :id "tool-4" :kind "tool" :name "ApplyPatch"
                                    :artifact "escape.txt"
                                    :artifact-path escape))))
          ;; An id outside the published set, a tool record without an
          ;; artifact, and a record whose path escaped the directory all
          ;; earn the same bounded refusal, touching no file.
          (dolist (id '("nope" "tool-3" "tool-2" "tool-4"))
            (setq now (+ now 2.0) sent nil)
            (mevedel-collaboration--handle-artifact-get
             room 1 (list :reqId 1 :id id))
            (should (= 1 (length sent)))
            (should (equal "artifact" (plist-get (cdr (car sent)) :t)))
            (should (stringp (plist-get (cdr (car sent)) :error))))
          ;; A published artifact answers final-flagged base64 chunks
          ;; carrying name, type, and decoded size, each under the wire
          ;; bound.
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-frame-json-bytes 600))
            (mevedel-collaboration--handle-artifact-get
             room 1 (list :reqId 7 :id "tool-1")))
          (setq sent (nreverse sent))
          (should (> (length sent) 1))
          (let ((first-frame (cdr (car sent))))
            (should (equal "artifact" (plist-get first-frame :t)))
            (should (= 7 (plist-get first-frame :reqId)))
            (should (equal "mockup.html" (plist-get first-frame :name)))
            (should (equal "text/html" (plist-get first-frame :mime)))
            (should (= 1000 (plist-get first-frame :size))))
          (should (eq :json-false
                      (plist-get (cdr (car sent)) :final)))
          (should (eq t (plist-get (cdr (car (last sent))) :final)))
          (dolist (entry sent)
            (should (<= (string-bytes (mevedel-collaboration--json-string
                                       (cdr entry)))
                        600)))
          (should (equal content
                         (base64-decode-string
                          (mapconcat (lambda (entry)
                                       (plist-get (cdr entry) :data))
                                     sent))))
          ;; A repeat inside the throttle window is dropped silently.
          (setq now (+ now 0.5) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 8 :id "tool-1"))
          (should-not sent)
          ;; The read itself is capped at max+1, which is the authoritative
          ;; overflow check even if the file changes after containment.
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-artifact-bytes 10)
                (read-function
                 (symbol-function 'insert-file-contents-literally))
                read-end)
            (cl-letf
                (((symbol-function 'insert-file-contents-literally)
                  (lambda (filename &optional visit beg end replace)
                    (setq read-end end)
                    (funcall read-function filename visit beg end replace))))
              (mevedel-collaboration--handle-artifact-get
               room 1 (list :reqId 9 :id "tool-1")))
            (should (= 11 read-end)))
          (should (string-match-p "too large"
                                  (plist-get (cdr (car sent)) :error)))
          ;; A deleted file is a targeted refusal, not a broken stream.
          (delete-file path)
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 10 :id "tool-1"))
          (should (string-match-p "deleted"
                                  (plist-get (cdr (car sent)) :error)))
          ;; An unregistered peer and a malformed request id get nothing.
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 9 (list :reqId 11 :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId "11" :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId -1 :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId #x20000000000000 :id "tool-1"))
          (should-not sent))
      (delete-directory save-path t))))


(provide 'test-mevedel-collaboration-artifact)
;;; test-mevedel-collaboration-artifact.el ends here
