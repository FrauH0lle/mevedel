;;; mevedel-sandbox.el --- Optional child-process confinement -*- lexical-binding: t -*-

;;; Commentary:

;; Builds and probes the Linux Bubblewrap boundary used by Bash and batch Eval.
;; The host is read-only, approved roots are rebound writable, and process and
;; network namespaces cover the requested process and all descendants.  This
;; module prepares execution facts; the tool executor owns asynchronous launch
;; and the narrowly permitted pre-exec fallback.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))


;;
;;; Configuration and state

(defcustom mevedel-sandbox-mode 'auto
  "Child-process confinement policy for Bash and batch Eval.

`auto' uses Bubblewrap when its cached probe succeeds and otherwise executes
directly with unrestricted-filesystem and unrestricted-network disclosure.
`required' refuses execution when Bubblewrap is unavailable.  `off' executes
directly and visibly disables confinement."
  :type '(choice
          (const :tag "Auto -- prefer Bubblewrap, disclose fallback" auto)
          (const :tag "Required -- refuse without Bubblewrap" required)
          (const :tag "Off -- execute directly with disclosure" off))
  :group 'mevedel)

(defvar mevedel-sandbox--probe-cache nil
  "Cached Bubblewrap availability plist, or nil before the first probe.")

(defvar mevedel-sandbox--last-facts nil
  "Most recently prepared child-confinement facts.")

(defconst mevedel-sandbox--marker-script
  "printf '%s\\n' \"$1\"; shift; exec \"$@\""
  "Shell wrapper that records entry into the requested process boundary.")

(define-error 'mevedel-sandbox-policy-error
  "Invalid child-confinement authority")


;;
;;; Bubblewrap profile

(defun mevedel-sandbox--probe-command (executable)
  "Return a harmless confinement probe command for EXECUTABLE."
  (list executable
        "--new-session"
        "--die-with-parent"
        "--ro-bind" "/" "/"
        "--dev" "/dev"
        "--unshare-user"
        "--unshare-pid"
        "--unshare-net"
        "--proc" "/proc"
        "--" (or (executable-find "true") "/bin/true")))

(defun mevedel-sandbox-probe ()
  "Return and cache the availability facts for Linux Bubblewrap.

The probe creates the same core namespaces and mounts as real execution but
runs only `true'.  A failed probe means the backend is unavailable even when a
`bwrap' executable is installed."
  (or mevedel-sandbox--probe-cache
      (setq
       mevedel-sandbox--probe-cache
       (if (not (eq system-type 'gnu/linux))
           (list :available nil
                 :reason "Bubblewrap confinement is supported only on Linux")
         (let ((executable (executable-find "bwrap")))
           (if (not executable)
               (list :available nil :reason "'bwrap' is not installed")
             (with-temp-buffer
               (condition-case err
                   (let ((status
                          (apply #'call-process executable nil t nil
                                 (cdr (mevedel-sandbox--probe-command
                                       executable)))))
                     (if (and (integerp status) (zerop status))
                         (list :available t :executable executable)
                       (list :available nil
                             :executable executable
                             :reason
                             (format "Bubblewrap probe failed%s"
                                     (let ((detail
                                            (string-trim (buffer-string))))
                                       (if (string-empty-p detail)
                                           (format " with exit code %s" status)
                                         (format ": %s" detail)))))))
                 (error
                  (list :available nil
                        :executable executable
                        :reason
                        (format "Bubblewrap probe failed: %s"
                                (error-message-string err))))))))))))

(defun mevedel-sandbox--canonical-directories (roots)
  "Return existing canonical directories from ROOTS without duplicates."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (root)
            (when (and (stringp root)
                       (file-directory-p (expand-file-name root)))
              (file-name-as-directory
               (file-truename (expand-file-name root)))))
          roots))))

(defun mevedel-sandbox--unrestricted-facts (sandbox reason)
  "Return direct-execution facts for SANDBOX and REASON."
  (list :sandbox sandbox
        :filesystem 'unrestricted
        :network 'unrestricted
        :reason reason))

(defun mevedel-sandbox--direct-preparation (command sandbox reason)
  "Return direct preparation for COMMAND with SANDBOX and REASON facts."
  (let ((facts (mevedel-sandbox--unrestricted-facts sandbox reason)))
    (setq mevedel-sandbox--last-facts facts)
    (list :state 'unrestricted :command command :facts facts)))

(defun mevedel-sandbox--confined-preparation
    (command workdir writable-roots executable)
  "Prepare COMMAND in WORKDIR with WRITABLE-ROOTS using EXECUTABLE."
  (let* ((canonical-workdir
          (file-name-as-directory (file-truename workdir)))
         (roots (mevedel-sandbox--canonical-directories writable-roots)))
    (unless roots
      (signal 'mevedel-sandbox-policy-error
              '("Sandbox confinement requires a writable root")))
    (unless (cl-some
             (lambda (root)
               (or (string-equal root canonical-workdir)
                   (file-in-directory-p canonical-workdir root)))
             roots)
      (signal 'mevedel-sandbox-policy-error
              (list
               (format "Sandbox working directory is outside writable roots: %s"
                       canonical-workdir))))
    (let* ((marker (make-temp-name "MEVEDEL_SANDBOX_STARTED_"))
           (facts (list :sandbox 'bubblewrap
                        :filesystem 'workspace-write
                        :network 'isolated
                        :writable-roots roots))
           (arguments
            (append
             (list "--new-session"
                   "--die-with-parent"
                   "--ro-bind" "/" "/"
                   "--dev" "/dev")
             (cl-mapcan
              (lambda (root) (list "--bind" root root))
              roots)
             (list "--unshare-user"
                   "--unshare-pid"
                   "--unshare-net"
                   "--proc" "/proc"
                   "--chdir" canonical-workdir
                   "--"
                   "sh" "-c" mevedel-sandbox--marker-script
                   "mevedel-sandbox" marker)
             command)))
      (setq mevedel-sandbox--last-facts facts)
      (list :state 'confined
            :command (cons executable arguments)
            :original-command command
            :marker marker
            :facts facts))))


;;
;;; Public execution interface

(defun mevedel-sandbox-prepare (command workdir writable-roots)
  "Prepare child COMMAND for WORKDIR and WRITABLE-ROOTS.

Return a plist with :state, :command, and :facts.  Confined preparations also
carry :marker and :original-command.  A required but unavailable backend
returns :state `refused' and :error without a command."
  (pcase mevedel-sandbox-mode
    ('off
     (mevedel-sandbox--direct-preparation
      command 'off "Confinement disabled by mevedel-sandbox-mode"))
    ((or 'auto 'required)
     (let ((availability (mevedel-sandbox-probe)))
       (if (plist-get availability :available)
           (condition-case err
               (let ((preparation
                      (mevedel-sandbox--confined-preparation
                       command workdir writable-roots
                       (plist-get availability :executable))))
                 (plist-put preparation :fallback-p
                            (eq mevedel-sandbox-mode 'auto)))
             (mevedel-sandbox-policy-error
              (let* ((reason (error-message-string err))
                     (facts
                      (mevedel-sandbox--unrestricted-facts
                       'refused reason)))
                (setq mevedel-sandbox--last-facts facts)
                (list :state 'refused :error reason :facts facts)))
             (error
              (let ((reason (error-message-string err)))
                (if (eq mevedel-sandbox-mode 'required)
                    (let ((facts
                           (mevedel-sandbox--unrestricted-facts
                            'unavailable reason)))
                      (setq mevedel-sandbox--last-facts facts)
                      (list :state 'refused :error reason :facts facts))
                  (mevedel-sandbox--direct-preparation
                   command 'unavailable reason)))))
         (let ((reason (plist-get availability :reason)))
           (if (eq mevedel-sandbox-mode 'required)
               (let ((facts
                      (mevedel-sandbox--unrestricted-facts
                       'unavailable reason)))
                 (setq mevedel-sandbox--last-facts facts)
                 (list :state 'refused :error reason :facts facts))
             (mevedel-sandbox--direct-preparation
              command 'unavailable reason))))))
    (_ (error "Unknown sandbox mode: %s" mevedel-sandbox-mode))))

(defun mevedel-sandbox-launch-failed-p (preparation child-result)
  "Return non-nil when PREPARATION failed before CHILD-RESULT ran the command."
  (and (eq (plist-get preparation :state) 'confined)
       (not (plist-get child-result :timed-out-p))
       (not (member (plist-get preparation :marker)
                    (split-string (or (plist-get child-result :output) "")
                                  "\n" nil)))
       (or (plist-get child-result :error)
           (not (zerop (or (plist-get child-result :exit-code) -1))))))

(defun mevedel-sandbox-strip-marker (preparation child-result)
  "Return CHILD-RESULT without PREPARATION's private start marker line."
  (let* ((marker (plist-get preparation :marker))
         (output (or (plist-get child-result :output) ""))
         (lines (split-string output "\n" nil))
         (cleaned (mapconcat #'identity (delete marker lines) "\n")))
    (plist-put (copy-sequence child-result) :output cleaned)))

(defun mevedel-sandbox-status-text (facts)
  "Return one persistent human-readable status line for FACTS."
  (concat
   (format "sandbox: %s; filesystem: %s; network: %s"
           (plist-get facts :sandbox)
           (plist-get facts :filesystem)
           (plist-get facts :network))
   (let ((reason (plist-get facts :reason)))
     (when reason
       (format "; reason: %s" reason)))))

(defun mevedel-sandbox--record-launch-failure (child-result)
  "Record CHILD-RESULT as an unavailable backend launch."
  (let* ((output (string-trim (or (plist-get child-result :output) "")))
         (reason
          (if (string-empty-p output)
              "Bubblewrap failed before the requested process started"
            (format "Bubblewrap failed before process start: %s" output))))
    (setq mevedel-sandbox--probe-cache
          (list :available nil :reason reason))
    (setq mevedel-sandbox--last-facts
          (mevedel-sandbox--unrestricted-facts 'unavailable reason))))

(provide 'mevedel-sandbox)

;;; mevedel-sandbox.el ends here
