;;; test-mevedel-artifact-backend.el --- Artifact backend interface tests -*- lexical-binding: t; -*-

;;; Commentary:

;; A fake adapter registered for one artifact id stands in for an artifact
;; served from elsewhere.  The tests drive the model's tools, `shared://'
;; addresses and the listings, and check what reaches the fake and what
;; stays in the local store.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-artifact-backend)
(require 'mevedel-artifact-store)
(require 'mevedel-artifacts-list)
(require 'mevedel-pipeline)
(require 'mevedel-shared-editing)
(require 'mevedel-shared-library)
(require 'mevedel-tools)

(defun mevedel-artifact-backend-test--fake (log)
  "Return a fake adapter serving the board \"remote-board\".
Each call's arguments are pushed onto the car of the cons LOG; replies
arrive asynchronously, as the local adapter's do."
  (list
   :call (lambda (_workspace args callback &optional _authorize _commit)
           (push args (car log))
           (run-at-time
            0 nil callback
            (pcase (plist-get args :action)
              ((and "view" (guard (plist-get args :search)))
               (list :result (list :documents
                                   (vector (list :address "shared://remote-board"
                                                 :text "Remote board overview"
                                                 :lines ["Remote board overview"]
                                                 :spans [[0 21 1]])))))
              ("view" (list :result (list :text "Remote board overview")))
              (_ (list :result (list :model (list :id (plist-get args :id) :revision 2)
                                     :transaction "remote-1")))))
           #'ignore)
   :catalog (lambda (_workspace detail)
              (pcase detail
                ('ids (list (list :id "remote-board")))
                ('items (list (list :id "remote-board" :kind "whiteboard"
                                    :title "Remote board" :revision 2)))
                (_ (list (list :id "remote-board" :kind 'whiteboard
                               :title "Remote board" :file "state.json"
                               :versions 0 :size 0 :modified 0)))))))

(defmacro mevedel-artifact-backend-test--with-session (&rest body)
  "Run BODY in a live session buffer with the model's tools, then clean up.
BODY sees WORKSPACE, SESSION and the local functions `run', `text' and
`parse'."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "mevedel-artifact-backend-" t))
          (workspace (mevedel-workspace--create :root directory))
          (session (mevedel-session--create :save-path directory
                                            :workspace workspace :working-directory directory
                                            :permission-mode 'full-auto :authority-mode 'pid-lock))
          (buffer (generate-new-buffer " *artifact-backend-test*"))
          (mevedel-artifact-backend--registry (make-hash-table :test #'equal))
          (mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
          (mevedel-artifact-lease--held (make-hash-table :test #'equal))
          (mevedel-shared-library-directory (file-name-concat directory "libraries"))
          (mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
     (unwind-protect
         (with-current-buffer buffer
           (setq-local mevedel--session session)
           (setf (mevedel-session-root-buffer session) buffer)
           (mevedel-tools-register)
           (cl-labels ((run (name args &optional status)
                         (let (reply)
                           (mevedel-pipeline-run-tool-outcome
                            (mevedel-tool-get name) (lambda (value) (setq reply value)) args)
                           (let ((deadline (+ (float-time) 10)))
                             (while (and (not reply) (< (float-time) deadline))
                               (accept-process-output nil 0.05)))
                           (unless (eq (or status 'success) (plist-get reply :status))
                             (ert-fail (list name args (plist-get reply :status)
                                             (plist-get reply :result))))
                           reply))
                       (text (reply)
                         (let ((result (plist-get reply :result)))
                           (if (stringp result) result (format "%S" result))))
                       (parse (reply)
                         (mevedel-shared-editing--parse (plist-get reply :result))))
             ,@body))
       (mevedel-shared-editing-stop)
       (mevedel-artifact-lease-release-all)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (delete-directory directory t))))

(mevedel-deftest mevedel-artifact-backend-call
  (:doc "shared:// Read and SharedEdit reach a registered adapter; other ids stay local" :quiet t)
  (mevedel-artifact-backend-test--with-session
    (let ((log (list nil)))
      (mevedel-artifact-backend-register
       workspace "remote-board" (mevedel-artifact-backend-test--fake log))
      (should (string-search "Remote board overview"
                             (text (run "Read" '(:file_path "shared://remote-board")))))
      (run "SharedEdit" '(:id "remote-board" :action "rename" :title "Renamed"))
      (should (equal '("view" "rename")
                     (mapcar (lambda (args) (plist-get args :action)) (reverse (car log)))))
      (should (equal "Agent: /root" (plist-get (caar log) :actor)))
      ;; The edited remote board is attached like a local one.
      (should (member "remote-board" (mevedel-session-attached-artifacts session)))
      ;; Nothing about it is written into the local store.
      (should-not (mevedel-artifact-store-ids workspace))
      ;; A local item is served by the local store, unseen by the fake.
      (let ((id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Local notes")))
                           :id)))
        (should (string-search "Local notes"
                               (text (run "Read" (list :file_path (concat "shared://" id))))))
        (should (= 2 (length (car log)))))
      ;; A settled turn's version and a deletion go to the adapter too.
      (mevedel-artifact-store-save-version workspace "remote-board" "session-1")
      (let (failure (done nil))
        (mevedel-artifact-store-delete workspace "remote-board" "Host"
                                       (lambda (reason) (setq failure reason done t)))
        (let ((deadline (+ (float-time) 10)))
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (should done)
        (should-not failure))
      (should (equal '(("delete" "Host") ("version" "session-1"))
                     (mapcar (lambda (args)
                               (list (plist-get args :action)
                                     (or (plist-get args :session) (plist-get args :actor))))
                             (seq-take (car log) 2))))
      (should-not (member "remote-board" (mevedel-artifact-store-ids workspace))))))

(mevedel-deftest mevedel-artifact-backend-catalog
  (:doc "Listings, Grep and the cockpit come from the catalog, local and registered" :quiet t)
  (mevedel-artifact-backend-test--with-session
    (let* ((log (list nil))
           (local (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Local notes")))
                             :id))
           (page (file-name-concat directory ".mevedel/artifacts/page/index.html")))
      (make-directory (file-name-directory page) t)
      (write-region "<p>page</p>" nil page nil 'silent)
      (mevedel-artifact-store-note-writes session (list (list :action 'write :path page)))
      (mevedel-artifact-backend-register
       workspace "remote-board" (mevedel-artifact-backend-test--fake log))
      ;; Each detail level merges the local store with the registered board.
      (should (equal (sort (list local "page" "remote-board") #'string<)
                     (sort (mapcar (lambda (row) (plist-get row :id))
                                   (mevedel-artifact-backend-catalog workspace 'ids))
                           #'string<)))
      (should (equal (list (list local "Local notes") (list "remote-board" "Remote board"))
                     (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :title)))
                             (mevedel-artifact-backend-catalog workspace 'items))))
      (should (equal (sort (list "page" local "remote-board") #'string<)
                     (sort (mapcar (lambda (row) (plist-get row :id))
                                   (mevedel-artifact-backend-catalog workspace))
                           #'string<)))
      ;; The model's bare shared:// listing shows the registered board.
      (mevedel-artifact-store-attach session "remote-board")
      (let ((listing (text (run "Read" '(:file_path "shared://")))))
        (should (string-search "shared://remote-board\twhiteboard \"Remote board\" · attached"
                               listing))
        (should (string-search (format "shared://%s\tdocument \"Local notes\"" local) listing)))
      ;; Grep over every item reaches the registered board's view.
      (should (string-search "Remote board overview"
                             (text (run "Grep" '(:pattern "Remote" :path "shared://"
                                                          :output_mode "content")))))
      (should (member "view" (mapcar (lambda (args) (plist-get args :action)) (car log))))
      ;; The cockpit lists and counts it with the local artifacts.
      (should (member "remote-board"
                      (mapcar (lambda (row) (plist-get row :row))
                              (mevedel-artifacts-list--store-rows workspace session))))
      (should (equal '(3 . 3) (mevedel-artifacts-list-count session)))
      ;; Unregistered, it is gone from every listing and nothing stayed local.
      (mevedel-artifact-backend-unregister workspace "remote-board")
      (should-not (member "remote-board"
                          (mapcar (lambda (row) (plist-get row :id))
                                  (mevedel-artifact-backend-catalog workspace 'ids))))
      (should-not (string-search "remote-board" (text (run "Read" '(:file_path "shared://")))))
      (should (string-search "Shared item not found"
                             (text (run "Read" '(:file_path "shared://remote-board") 'error)))))))

(mevedel-deftest mevedel-artifact-backend-register
  (:doc "Registration keeps ids valid and distinct from local artifacts, and is announced")
  (let* ((directory (make-temp-file "mevedel-artifact-backend-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-artifact-backend--registry (make-hash-table :test #'equal))
         (announced 0)
         (mevedel-artifact-store-changed-functions
          (list (lambda (changed) (when (eq changed workspace) (cl-incf announced)))))
         (adapter (mevedel-artifact-backend-test--fake (list nil))))
    (unwind-protect
        (progn
          (make-directory (file-name-concat directory ".mevedel/artifacts/page") t)
          (should-error (mevedel-artifact-backend-register workspace "origin:board" adapter))
          (should-error (mevedel-artifact-backend-register workspace "page" adapter))
          (should (zerop announced))
          (mevedel-artifact-backend-register workspace "remote-board" adapter)
          (should (= 1 announced))
          (should (mevedel-artifact-backend-present-p workspace "remote-board"))
          (mevedel-artifact-backend-unregister workspace "remote-board")
          (should (= 2 announced))
          (should-not (mevedel-artifact-backend-present-p workspace "remote-board"))
          (should (zerop (hash-table-count mevedel-artifact-backend--registry))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-artifact-backend-notify
  (:doc "Every change observer hears an adapter's commit, even after one fails")
  (let* ((heard nil)
         (mevedel-artifact-backend-change-hook
          (list (lambda (&rest _) (error "Broken observer"))
                (lambda (workspace state result) (push (list workspace state result) heard)))))
    (mevedel-artifact-backend-notify 'workspace '(:id "remote-board" :revision 3) '(:model nil))
    (should (equal '((workspace (:id "remote-board" :revision 3) (:model nil))) heard))))

(provide 'test-mevedel-artifact-backend)
;;; test-mevedel-artifact-backend.el ends here
