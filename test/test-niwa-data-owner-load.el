;;; test-niwa-data-owner-load.el -- Cold data owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the foundational data owners without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-data-owners/cold-load ()
  ,test
  (test)
  :doc "loads each compiled owner directly and exercises its public behavior"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-data-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("niwa-workspace.el"
                   "niwa-turn.el"
                   "niwa-instruction-registry.el"
                   "niwa-permission-persistence.el"
                   "niwa-file-state.el"))
         (cases
          '((workspace
             (progn
               (require 'niwa-workspace)
               (let ((workspace
                      (niwa-workspace-get-or-create
                       'file "cold" default-directory "cold")))
                 (unless (string-suffix-p
                          "/.niwa/"
                          (niwa-workspace-state-dir workspace))
                   (error "Workspace owner did not construct state")))
               (unless
                   (string-suffix-p
                    "niwa-workspace.elc"
                    (or (symbol-file 'niwa-workspace-get-or-create
                                     'defun)
                        ""))
                 (error "Workspace behavior has the wrong owner"))))
            (turn
             (progn
               (require 'niwa-turn)
               (with-temp-buffer
                 (setq-local niwa--current-request
                             (niwa-request--create
                              :session (niwa-session--create)))
                 (unless (niwa-request-active-p)
                   (error "Turn owner did not expose request state")))
               (unless
                   (string-suffix-p
                    "niwa-turn.elc"
                    (or (symbol-file 'niwa-request-begin 'defun) ""))
                 (error "Request behavior has the wrong owner"))))
            (instruction-consumer
             (progn
               (require 'niwa-structs)
               (require 'niwa-instruction-registry)
               (niwa--clear-instruction-state
                (niwa-workspace--create :type 'file :id "cold"))))
            (permission-persistence-consumer
             (progn
               (require 'niwa-structs)
               (require 'niwa-permission-persistence)
               (niwa-permission-persistence-file
                (niwa-workspace--create
                 :type 'file :id "cold" :root default-directory) 'workspace)
               (unless (featurep 'niwa-workspace)
                 (error "Permission consumer did not load Workspace"))))
            (file-state-consumer
             (progn
               (require 'niwa-file-state)
               (let* ((session
                       (niwa-session--create
                        :touched-files (make-hash-table :test #'equal)))
                      (niwa--current-request
                       (niwa-request--create :session session :turn 1)))
                 (niwa-session-record-file-access
                  session "missing" 'read))
               (unless (featurep 'niwa-turn)
                 (error "File-state consumer did not load Turn")))))))
    (unwind-protect
        (progn
          (dolist (owner owners)
            (copy-file (file-name-concat root owner)
                       (file-name-concat compiled-root owner))
            (let ((byte-compile-verbose nil))
              (byte-compile-file (file-name-concat compiled-root owner))))
          (dolist (case cases)
            (with-temp-buffer
              (ert-info ((format "cold owner: %s" (car case)))
                (should
                 (= 0
                    (call-process
                     emacs nil t nil
                     "--batch" "-Q" "-L" compiled-root "-L" root
                     "--eval"
                     (prin1-to-string
                      `(progn
                         ,(cadr case)
                         (when (featurep 'niwa)
                           (error
                            "Data owner loaded the niwa umbrella")))))))
                (should
                 (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-data-owner-load)
;;; test-niwa-data-owner-load.el ends here
