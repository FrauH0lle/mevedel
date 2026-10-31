;;; mevedel-pin.el --- Open authorized paths without following symlinks -*- lexical-binding: t -*-

;;; Commentary:

;; A file tool is authorized on a canonical path and opens it afterwards.  A
;; concurrent confined process can replace a directory on that path with a
;; symlink in between, so a plain open would follow it into a protected tree.
;; `scripts/mevedel-pin.py' walks the authorized path with O_NOFOLLOW, refuses
;; any symlink component and holds a descriptor; Emacs opens or writes through
;; its /proc/PID/fd/N name, which keeps naming the pinned inode whatever
;; happens to the path.  One helper per execution target serves every caller.
;;
;; Callers pass canonical paths: the tool pipeline canonicalizes arguments
;; and resource preparation canonicalizes backing paths before authorization.
;; A symlink on such a path at open time is therefore a swap, and refused.

;;; Code:

(require 'cl-lib)
(require 'mevedel-utilities)

(define-error 'mevedel-pin-refused "Path cannot be opened safely")

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-localname "tramp" (vec))
(declare-function tramp-flush-file-properties "tramp-cache" (key file))
(declare-function tramp-flush-directory-properties "tramp-cache" (key directory))

(defconst mevedel-pin--script
  (file-name-concat
   (mevedel-library-source-directory (or load-file-name buffer-file-name))
   "scripts" "mevedel-pin.py")
  "Helper that pins paths without following symlinks.")

(defvar mevedel-pin-timeout 10
  "Seconds to wait for one pin helper answer.")

(defvar mevedel-pin--helpers nil
  "Alist of execution-target prefix (nil when local) to its helper process.")

(defvar mevedel-pin--next-id 0
  "Last request id sent to a pin helper.")

(defvar mevedel-pin--redirects nil
  "Alist of authorized path to the pinned name that opens it.")

(defun mevedel-pin--prefix (path)
  "Return PATH's authored remote prefix, or nil when PATH is local."
  (when (file-remote-p path)
    (substring path 0 (- (length path) (length (file-local-name path))))))

(defun mevedel-pin--flush (name &optional directory-p)
  "Drop TRAMP's cached properties for remote NAME.
Pinned names repeat as descriptors are reused, and a write through a pin
changes the real path behind TRAMP's back.  DIRECTORY-P flushes a subtree."
  (when (file-remote-p name)
    (ignore-errors
      (let ((vec (tramp-dissect-file-name name)))
        (if directory-p
            (tramp-flush-directory-properties vec (tramp-file-name-localname vec))
          (tramp-flush-file-properties vec (tramp-file-name-localname vec)))))))

(defun mevedel-pin--filter (process output)
  "Collect PROCESS OUTPUT and record each complete JSON answer by id."
  (let ((pending (concat (or (process-get process 'mevedel-pin-pending) "") output))
        (replies (process-get process 'mevedel-pin-replies)))
    (while (string-match "\n" pending)
      (let ((line (substring pending 0 (match-beginning 0))))
        (setq pending (substring pending (match-end 0)))
        (when-let* ((reply (ignore-errors
                             (json-parse-string line :object-type 'plist
                                                :false-object nil :null-object nil)))
                    (id (plist-get reply :id)))
          (puthash id reply replies))))
    (process-put process 'mevedel-pin-pending pending)))

(defun mevedel-pin--helper (prefix)
  "Return the live pin helper for PREFIX, starting it when needed."
  (let ((process (alist-get prefix mevedel-pin--helpers nil nil #'equal)))
    (if (process-live-p process)
        process
      (let* ((default-directory (if prefix (concat prefix "/")
                                  (file-name-as-directory temporary-file-directory)))
             (python (if prefix "python3" (executable-find "python3")))
             (command
              (cond
               ((null python)
                (signal 'mevedel-pin-refused
                        '("python3 is required to open tool paths safely")))
               ;; A remote command line passes a terminal line limit, so a
               ;; short bootstrap reads the script from stdin as wrapped
               ;; base64 ending at a blank line, then serves on that stdin.
               (prefix
                (list python "-I" "-c"
                      (concat "import base64,sys;exec(base64.b64decode(''.join("
                              "iter(sys.stdin.readline,'\\n'))))")))
               (t (list python "-I" mevedel-pin--script))))
             (process
              (make-process
               :name "mevedel-pin" :command command
               :connection-type 'pipe :coding 'utf-8-unix :noquery t
               :file-handler t :filter #'mevedel-pin--filter :sentinel #'ignore)))
        (when prefix
          (process-send-string
           process
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally mevedel-pin--script)
             (base64-encode-region (point-min) (point-max))
             (goto-char (point-max))
             (insert "\n\n")
             (buffer-string))))
        (process-put process 'mevedel-pin-replies (make-hash-table))
        (setf (alist-get prefix mevedel-pin--helpers nil nil #'equal) process)
        process))))

(defun mevedel-pin--request (prefix request)
  "Send REQUEST to PREFIX's helper and return its answer plist.
Signal `mevedel-pin-refused' when the helper refuses or does not answer."
  (let* ((process (mevedel-pin--helper prefix))
         (replies (process-get process 'mevedel-pin-replies))
         (id (cl-incf mevedel-pin--next-id))
         (deadline (+ (float-time) mevedel-pin-timeout))
         ;; Helper I/O must not change the coding a caller just read with.
         (last-coding-system-used last-coding-system-used)
         reply)
    (process-send-string
     process (concat (json-serialize (append (list :id id) request)) "\n"))
    (while (and (not (setq reply (gethash id replies)))
                (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.05))
    (remhash id replies)
    (cond
     ((null reply)
      (signal 'mevedel-pin-refused '("the path pin helper did not answer")))
     ((not (plist-get reply :ok))
      (signal 'mevedel-pin-refused (list (plist-get reply :error))))
     (t reply))))

(defun mevedel-pin--acquire (path op &optional create)
  "Pin PATH with helper OP; return (PINNED-NAME . RELEASE-FUNCTION).
CREATE asks a directory pin to make missing components first."
  (let* ((prefix (mevedel-pin--prefix path))
         (reply (mevedel-pin--request
                 prefix (list :op op
                              :path (directory-file-name
                                     (mevedel-collapse-leading-slashes
                                      (file-local-name path)))
                              :create (if create t :false))))
         (pin (plist-get reply :pin))
         (name (concat prefix (plist-get reply :path))))
    (mevedel-pin--flush name (equal op "dir"))
    (cons name
          (lambda ()
            (ignore-errors
              (mevedel-pin--request prefix (list :op "release" :pin pin)))))))

(defun mevedel-pin-call-with-file (path function)
  "Call FUNCTION while opens of PATH resolve to its pinned inode.
PATH must be the authorized canonical path of an existing file."
  (let* ((pin (mevedel-pin--acquire path "file"))
         (mevedel-pin--redirects (cons (cons path (car pin)) mevedel-pin--redirects)))
    (unwind-protect (funcall function)
      (funcall (cdr pin)))))

(defmacro mevedel-pin-with-file (path &rest body)
  "Run BODY while `mevedel-pin-open-path' maps PATH to its pinned inode."
  (declare (indent 1) (debug (form body)))
  `(mevedel-pin-call-with-file ,path (lambda () ,@body)))

(defun mevedel-pin-open-path (path)
  "Return the name that opens PATH: its pin when one is held, else PATH."
  (or (cdr (assoc path mevedel-pin--redirects)) path))

(defun mevedel-pin-insert-file-contents (path &optional _visit beg end)
  "Insert PATH's decoded contents at point through its pin.
Like `insert-file-contents' with BEG and END byte offsets.  The bytes are
read from the pinned name and decoded as if read from PATH, so coding
cookies and name-based coding rules still apply; `last-coding-system-used'
is set.  Return (PATH LENGTH)."
  (let ((start (point))
        (end-marker (copy-marker (point) t)))
    (unwind-protect
        (progn
          (let ((coding-system-for-read 'no-conversion))
            (insert-file-contents-literally (mevedel-pin-open-path path) nil beg end))
          (goto-char end-marker)
          (decode-coding-inserted-region start end-marker path nil beg end)
          (goto-char start)
          (list path (- end-marker start)))
      (set-marker end-marker nil))))

(defun mevedel-pin-call-with-directory (directory function &optional create)
  "Call FUNCTION with the pinned name of DIRECTORY.
CREATE makes missing components through the helper, never via a symlink."
  (let ((pin (mevedel-pin--acquire directory "dir" create)))
    (unwind-protect (funcall function (file-name-as-directory (car pin)))
      (funcall (cdr pin)))))

(defun mevedel-pin-write-file-atomically (path content &optional coding mode)
  "Replace PATH with CONTENT through its pinned parent directory.
Like `mevedel--write-file-atomically': CODING and MODE apply to the new
file, which is written beside PATH and renamed over it.  The rename replaces
a symlink planted at PATH instead of following it."
  (let ((name (file-name-nondirectory path)))
    (mevedel-pin-call-with-directory
     (file-name-directory path)
     (lambda (directory)
       (let ((temporary (make-temp-file (concat directory ".mevedel-write-"))))
         (unwind-protect
             (progn
               (with-temp-buffer
                 (when (eq coding 'no-conversion)
                   (set-buffer-multibyte nil))
                 (insert content)
                 (let ((coding-system-for-write (or coding 'utf-8-unix)))
                   (write-region (point-min) (point-max) temporary nil 'silent)))
               (set-file-modes temporary (or mode (default-file-modes)))
               (rename-file temporary (concat directory name) t))
           (when (file-exists-p temporary)
             (delete-file temporary)))))
     t)
    (mevedel-pin--flush path)
    (mevedel-pin--flush (file-name-directory path) t)))

(defun mevedel-pin-delete-file (path)
  "Delete PATH through its pinned parent directory, never through a symlink."
  (mevedel-pin-call-with-directory
   (file-name-directory path)
   (lambda (directory)
     (delete-file (concat directory (file-name-nondirectory path)))))
  (mevedel-pin--flush path)
  (mevedel-pin--flush (file-name-directory path) t))

(provide 'mevedel-pin)

;;; mevedel-pin.el ends here
