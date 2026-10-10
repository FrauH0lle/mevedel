;;; test-mevedel-collaboration-artifact-store.el --- Store projection tests -*- lexical-binding: t -*-

;;; Commentary:

;; Store fanout reads one workspace snapshot and decorates it per room.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-artifact-store)
(require 'mevedel-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-lobby)
(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))

(mevedel-deftest mevedel-collaboration--store-frame
  (:doc "one fanout reads each artifact once while preserving room attachment")
  (let* ((root (make-temp-file "mevedel-store-fanout-" t))
         (workspace (mevedel-workspace--create :type 'file :id root :root root))
         (mevedel-artifact-store-changed-functions nil)
         (mevedel-collaboration--artifact-notifications (make-hash-table :test #'eq))
         (mevedel-transport--enabled-p t)
         (mevedel-transport--background-resume-at 0)
         (mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (rooms (cl-loop for n below 3
                         collect (list :session
                                       (mevedel-session--create
                                        :workspace workspace
                                        :attached-artifacts
                                        (list (format "item-%d" n)))
                                       :guests (make-hash-table))))
         (mevedel-collaboration--rooms (apply #'mevedel-test-room-registry rooms))
         (read-program (symbol-function 'mevedel-session-control-fs-run-program))
         (reads 0)
         (programs 0)
         frames)
    (unwind-protect
        (progn
          (dotimes (n 50)
            (let* ((id (format "item-%d" n))
                   (dir (mevedel-artifact-store-artifact-directory workspace id)))
              (make-directory dir t)
              (write-region "hi" nil (file-name-concat dir "index.html") nil 'silent)
              (mevedel-artifact-store-create-meta workspace id "index.html")))
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations &rest args)
                       (cl-incf programs)
                       (cl-incf reads
                                (cl-count "meta.el" operations :test #'equal
                                          :key (lambda (op)
                                                 (file-name-nondirectory
                                                  (plist-get op :path)))))
                       (apply read-program operations args)))
                    ((symbol-function 'mevedel-collaboration--publish) #'ignore)
                    ((symbol-function 'mevedel-collaboration--broadcast)
                     (lambda (room frame) (push (cons room frame) frames))))
            (mevedel-collaboration-notify-artifacts-changed workspace)
            (mevedel-collaboration-notify-artifacts-changed workspace)
            (should (= 0 reads))
            (should (= 1 (hash-table-count mevedel-collaboration--artifact-notifications)))
            (let ((timer (gethash workspace mevedel-collaboration--artifact-notifications)))
              (cancel-timer timer)
              (apply (timer--function timer) (timer--args timer))))
          (should (= 50 reads))
          (should (<= programs 4))
          (should (= 3 (length frames)))
          (dolist (entry frames)
            (let* ((room (car entry))
                   (attached (car (mevedel-session-attached-artifacts
                                   (plist-get room :session))))
                   (rows (append (plist-get (cdr entry) :artifacts) nil)))
              (should (= 50 (length rows)))
              (should (= 1 (cl-count t rows :key (lambda (row) (plist-get row :attached)))))
              (should (eq t (plist-get (cl-find attached rows
                                               :key (lambda (row) (plist-get row :id))
                                               :test #'equal)
                                      :attached)))))
          ;; No rooms must not turn a settled file write into a store scan.
          (let ((mevedel-collaboration--rooms (mevedel-test-room-registry)))
            (cl-letf (((symbol-function 'mevedel-artifact-store-list)
                       (lambda (&rest _) (ert-fail "Unexpected store scan"))))
              (mevedel-collaboration-notify-artifacts-changed workspace))))
      (mevedel-transport-cancel-idle
       mevedel-collaboration--artifact-notifications 'artifact-notifications)
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--store-snapshot
  (:doc "lobby counts merge one saved discovery with live attachments")
  (let* ((root (make-temp-file "mevedel-store-counts-" t))
         (workspace (mevedel-workspace--create :type 'file :id root :root root))
         (mevedel-artifact-store-changed-functions nil)
         (mevedel-session-persistence--list-sessions-cache (make-hash-table :test #'equal))
         (mevedel-session-persistence--summary-cache (make-hash-table :test #'equal))
         (buffer (generate-new-buffer " *store-live-count*"))
         (session (mevedel-session--create :workspace workspace :session-id "saved-0"
                                          :attached-artifacts '("page" "draft")))
         (lobby (list :workspace workspace)))
    (unwind-protect
        (progn
          (mevedel-workspace-identity-ensure root)
          (dolist (id '("page" "draft"))
            (let ((dir (mevedel-artifact-store-artifact-directory workspace id)))
              (make-directory dir t)
              (write-region "hi" nil (file-name-concat dir "index.html") nil 'silent)
              (mevedel-artifact-store-create-meta workspace id "index.html")))
          ;; A realistic saved catalog makes per-artifact session scans costly.
          (dotimes (n 100)
            (let* ((id (format "saved-%d" n))
                   (directory (file-name-concat
                               (mevedel-session-artifacts-sessions-dir workspace) id))
                   (sidecar (test-mevedel-session-persistence--complete-sidecar
                             (list :session-id id
                                   :workspace (mevedel-session-codec--workspace-to-plist workspace)
                                   :working-directory root
                                   :created-at "2026-10-10T12:00:00+0000"
                                   :updated-at "2026-10-10T12:00:00+0000"
                                   :attached-artifacts '("page" "page")))))
              (make-directory directory t)
              (mevedel-session-codec-write
               (mevedel-session-artifacts-sidecar-path directory) sidecar)))
          (with-current-buffer buffer (setq-local mevedel--session session))
          (let* ((snapshot (mevedel-collaboration--store-snapshot workspace t))
                 (rows (mevedel-collaboration--store-rows lobby snapshot)))
            (should (= 100 (gethash "page" (plist-get snapshot :attached-counts))))
            (should (= 1 (gethash "draft" (plist-get snapshot :attached-counts))))
            (should (= 100 (plist-get
                            (cl-find "page" rows :key (lambda (row) (plist-get row :id))
                                     :test #'equal)
                            :attachedSessions))))
          ;; Mutation notifications reuse discovery without probing saved
          ;; sessions, while live attachments remain current.
          (setf (mevedel-session-attached-artifacts session) '("draft"))
          (cl-letf (((symbol-function 'mevedel-session-persistence--control-artifacts)
                     (lambda (&rest _) (ert-fail "Unexpected session discovery"))))
            (let ((counts (plist-get (mevedel-collaboration--store-snapshot workspace t t)
                                     :attached-counts)))
              (should (= 99 (gethash "page" counts)))
              (should (= 1 (gethash "draft" counts))))
            ;; Session room listings never need project attachment counts.
            (should (= 0 (hash-table-count
                          (plist-get (mevedel-collaboration--store-snapshot workspace)
                                     :attached-counts))))))
      (kill-buffer buffer)
      (delete-directory root t))))

(provide 'test-mevedel-collaboration-artifact-store)
;;; test-mevedel-collaboration-artifact-store.el ends here
