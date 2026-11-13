;;; test-mevedel-data-owner-load.el -- Cold data owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the foundational data owners without loading the mevedel umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-data-owners/cold-load ()
  ,test
  (test)
  :doc "loads each compiled owner directly and exercises its public behavior"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-data-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("mevedel-workspace.el"
                   "mevedel-turn.el"
                   "mevedel-instruction-registry.el"
                   "mevedel-permission-persistence.el"
                   "mevedel-file-state.el"))
         (cases
          '((workspace
             (progn
               (require 'mevedel-workspace)
               (let ((workspace
                      (mevedel-workspace-get-or-create
                       'file "cold" default-directory "cold")))
                 (unless (string-suffix-p
                          "/.mevedel/"
                          (mevedel-workspace-state-dir workspace))
                   (error "Workspace owner did not construct state")))
               (unless
                   (string-suffix-p
                    "mevedel-workspace.elc"
                    (or (symbol-file 'mevedel-workspace-get-or-create
                                     'defun)
                        ""))
                 (error "Workspace behavior has the wrong owner"))))
            (turn
             (progn
               (require 'mevedel-turn)
               (with-temp-buffer
                 (setq-local mevedel--current-request
                             (mevedel-request--create
                              :session (mevedel-session--create)))
                 (unless (mevedel-request-active-p)
                   (error "Turn owner did not expose request state")))
               (unless
                   (string-suffix-p
                    "mevedel-turn.elc"
                    (or (symbol-file 'mevedel-request-begin 'defun) ""))
                 (error "Request behavior has the wrong owner"))))
            (instruction-consumer
             (progn
               (require 'mevedel-structs)
               (require 'mevedel-instruction-registry)
               (mevedel--clear-instruction-state
                (mevedel-workspace--create :type 'file :id "cold"))))
            (permission-persistence-consumer
             (progn
               (require 'mevedel-structs)
               (require 'mevedel-permission-persistence)
               (mevedel-permission-persistence-file
                (mevedel-workspace--create
                 :type 'file :id "cold" :root default-directory) 'workspace)
               (unless (featurep 'mevedel-workspace)
                 (error "Permission consumer did not load Workspace"))))
            (file-state-consumer
             (progn
               (require 'mevedel-file-state)
               (let* ((session
                       (mevedel-session--create
                        :touched-files (make-hash-table :test #'equal)))
                      (mevedel--current-request
                       (mevedel-request--create :session session :turn 1)))
                 (mevedel-session-record-file-access
                  session "missing" 'read))
               (unless (featurep 'mevedel-turn)
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
                         (when (featurep 'mevedel)
                           (error
                            "Data owner loaded the mevedel umbrella")))))))
                (should
                 (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-data-owner-load)
;;; test-mevedel-data-owner-load.el ends here
