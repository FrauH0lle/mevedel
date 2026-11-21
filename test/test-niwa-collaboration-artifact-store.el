;;; test-niwa-collaboration-artifact-store.el --- Store projection tests -*- lexical-binding: t -*-

;;; Commentary:

;; Store fanout reads one workspace snapshot and decorates it per room.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-artifact-store)
(require 'niwa-artifact-backend)
(require 'niwa-session-control-fs)
(require 'niwa-session-persistence)
(require 'niwa-transport)
(require 'niwa-collaboration)
(require 'niwa-collaboration-artifact)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-lobby)
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))

(niwa-deftest niwa-collaboration--store-frame
  (:doc "one fanout reads each artifact once while preserving room attachment")
  (let* ((root (make-temp-file "niwa-store-fanout-" t))
         (workspace (niwa-workspace--create :type 'file :id root :root root))
         (niwa-artifact-store-changed-functions nil)
         (niwa-collaboration--artifact-notifications (make-hash-table :test #'eq))
         (niwa-transport--enabled-p t)
         (niwa-transport--background-resume-at 0)
         (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (rooms (cl-loop for n below 3
                         collect (list :session
                                       (niwa-session--create
                                        :workspace workspace
                                        :attached-artifacts
                                        (list (format "item-%d" n)))
                                       :guests (let ((guests (make-hash-table)))
                                                 (puthash 1 '(:name "viewer" :ready t) guests)
                                                 guests))))
         (niwa-collaboration--rooms (apply #'niwa-test-room-registry rooms))
         (read-program (symbol-function 'niwa-session-control-fs-run-program))
         (reads 0)
         (programs 0)
         frames)
    (unwind-protect
        (progn
          (dotimes (n 50)
            (let* ((id (format "item-%d" n))
                   (dir (niwa-artifact-store-artifact-directory workspace id)))
              (make-directory dir t)
              (write-region "hi" nil (file-name-concat dir "index.html") nil 'silent)
              (niwa-artifact-store-create-meta workspace id "index.html")))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations &rest args)
                       (cl-incf programs)
                       (should (<= (length operations) 32))
                       (when (cl-find 'read operations :key (lambda (op) (plist-get op :op)))
                         (should (cl-every (lambda (op) (eq 'read (plist-get op :op)))
                                           operations)))
                       (cl-incf reads
                                (cl-count "meta.el" operations :test #'equal
                                          :key (lambda (op)
                                                 (file-name-nondirectory
                                                  (plist-get op :path)))))
                       (apply read-program operations args)))
                    ((symbol-function 'niwa-artifact-store-shared-p)
                     (lambda (&rest _) (ert-fail "Unexpected scalar sharing probe")))
                    ((symbol-function 'niwa-collaboration--publish) #'ignore)
                    ((symbol-function 'niwa-collaboration--broadcast)
                     (lambda (room frame) (push (cons room frame) frames))))
            (niwa-collaboration-notify-artifacts-changed workspace)
            (niwa-collaboration-notify-artifacts-changed workspace)
            (should (= 0 reads))
            (should (= 1 (hash-table-count niwa-collaboration--artifact-notifications)))
            (let ((timer (gethash workspace niwa-collaboration--artifact-notifications)))
              (cancel-timer timer)
              (apply (timer--function timer) (timer--args timer))))
          (should (= 50 reads))
          (should (= programs 6))
          (should (= 3 (length frames)))
          (dolist (entry frames)
            (let* ((room (car entry))
                   (attached (car (niwa-session-attached-artifacts
                                   (plist-get room :session))))
                   (rows (append (plist-get (cdr entry) :artifacts) nil)))
              (should (= 50 (length rows)))
              (should (= 1 (cl-count t rows :key (lambda (row) (plist-get row :attached)))))
              (should (eq t (plist-get (cl-find attached rows
                                               :key (lambda (row) (plist-get row :id))
                                               :test #'equal)
                                      :attached)))))
          ;; No rooms must not turn a settled file write into a store scan.
          (let ((niwa-collaboration--rooms (niwa-test-room-registry)))
            (cl-letf (((symbol-function 'niwa-artifact-store-list)
                       (lambda (&rest _) (ert-fail "Unexpected store scan"))))
              (niwa-collaboration-notify-artifacts-changed workspace))))
      (niwa-transport-cancel-idle
       niwa-collaboration--artifact-notifications 'artifact-notifications)
      (niwa-collaboration--artifact-stat-invalidate)
      (delete-directory root t))))

(niwa-deftest niwa-collaboration--store-snapshot ()
  ,test
  (test)
  :doc "consumes batched sharing observations instead of guarded per-row walks"
  (let* ((root (make-temp-file "niwa-store-sharing-" t))
         (workspace (niwa-workspace--create :type 'file :id root :root root))
         (niwa-artifact-store-changed-functions nil)
         (niwa-artifact-backend--registry (make-hash-table :test #'equal))
         (niwa-artifact-backend--restored (make-hash-table :test #'equal))
         (link-path (symbol-function 'niwa-artifact-store-link-path))
         (walks 0))
    (unwind-protect
        (progn
          (dolist (id '("shared" "private"))
            (niwa-artifact-store-create-meta workspace id "index.html"))
          (write-region "private" nil (niwa-artifact-store-link-path workspace "shared")
                        nil 'silent)
          (niwa-artifact-backend-register
           workspace "remote"
           (list :catalog (lambda (_workspace _detail)
                            (list (list :id "remote" :remote t :modified 0)))))
          (cl-letf (((symbol-function 'niwa-artifact-store-link-path)
                     (lambda (&rest args)
                       (cl-incf walks)
                       (apply link-path args))))
            ;; Negative control: the old snapshot's loop repeats guarded walks.
            (dolist (row (niwa-artifact-backend-catalog workspace))
              (niwa-artifact-store-shared-p workspace (plist-get row :id)))
            (should (= 3 walks))
            (setq walks 0)
            (let ((snapshot (niwa-collaboration--store-snapshot workspace)))
              (should (equal '("shared") (plist-get snapshot :shared)))
              (should (= 3 (length (plist-get snapshot :artifacts))))
              (should (= 0 walks)))))
      (delete-directory root t)))

  :doc "lobby counts merge one saved discovery with live attachments"
  (let* ((root (make-temp-file "niwa-store-counts-" t))
         (workspace (niwa-workspace--create :type 'file :id root :root root))
         (niwa-artifact-store-changed-functions nil)
         (niwa-session-persistence--list-sessions-cache (make-hash-table :test #'equal))
         (niwa-session-persistence--summary-cache (make-hash-table :test #'equal))
         (buffer (generate-new-buffer " *store-live-count*"))
         (session (niwa-session--create :workspace workspace :session-id "saved-0"
                                          :attached-artifacts '("page" "draft")))
         (saved-count 8)
         (lobby (list :workspace workspace)))
    (unwind-protect
        (progn
          (niwa-workspace-identity-ensure root)
          (dolist (id '("page" "draft"))
            (let ((dir (niwa-artifact-store-artifact-directory workspace id)))
              (make-directory dir t)
              (write-region "hi" nil (file-name-concat dir "index.html") nil 'silent)
              (niwa-artifact-store-create-meta workspace id "index.html")))
          ;; Several saved sessions suffice to tell merged discovery from live
          ;; attachments; each costs real lease and publication programs.
          (dotimes (n saved-count)
            (let* ((id (format "saved-%d" n))
                   (directory (file-name-concat
                               (niwa-session-artifacts-sessions-dir workspace) id))
                   (sidecar (test-niwa-session-persistence--complete-sidecar
                             (list :session-id id
                                   :workspace (niwa-session-codec--workspace-to-plist workspace)
                                   :working-directory root
                                   :created-at "2026-10-10T12:00:00+0000"
                                   :updated-at "2026-10-10T12:00:00+0000"
                                   :attached-artifacts '("page" "page")))))
              (let ((saved-session (niwa-session-create id workspace)))
                (setf (niwa-session-save-path saved-session) directory
                      (niwa-session-session-id saved-session) id)
                (unwind-protect
                    (progn
                      (make-directory directory t)
                      (should (niwa-session-durability-lease-acquire
                               directory "*saved catalog*" saved-session))
                      (niwa-session-publication-publish
                       saved-session
                       (list (list :path (niwa-session-artifacts-sidecar-path directory)
                                   :content (niwa-session-artifacts-printed-value sidecar)
                                   :commit-marker t))))
                  (niwa-session-persistence-lock-release directory saved-session)))))
          (with-current-buffer buffer (setq-local niwa--session session))
          (let* ((snapshot (niwa-collaboration--store-snapshot workspace t))
                 (rows (niwa-collaboration--store-rows lobby snapshot)))
            (should (= saved-count (gethash "page" (plist-get snapshot :attached-counts))))
            (should (= 1 (gethash "draft" (plist-get snapshot :attached-counts))))
            (should (= saved-count (plist-get
                                    (cl-find "page" rows :key (lambda (row) (plist-get row :id))
                                             :test #'equal)
                                    :attachedSessions))))
          ;; Mutation notifications reuse discovery without probing saved
          ;; sessions, while live attachments remain current.
          (setf (niwa-session-attached-artifacts session) '("draft"))
          (cl-letf (((symbol-function 'niwa-session-persistence--control-artifacts)
                     (lambda (&rest _) (ert-fail "Unexpected session discovery"))))
            (let ((counts (plist-get (niwa-collaboration--store-snapshot workspace t t)
                                     :attached-counts)))
              (should (= (1- saved-count) (gethash "page" counts)))
              (should (= 1 (gethash "draft" counts))))
            ;; Session room listings never need project attachment counts.
            (should (= 0 (hash-table-count
                          (plist-get (niwa-collaboration--store-snapshot workspace)
                                     :attached-counts))))))
      (kill-buffer buffer)
      (delete-directory root t))))

(provide 'test-niwa-collaboration-artifact-store)
;;; test-niwa-collaboration-artifact-store.el ends here
